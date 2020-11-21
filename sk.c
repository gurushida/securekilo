
/************************************ includes ************************************/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <locale.h>
#include <ncurses.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#include <wchar.h>
#include <xlocale.h>



/************************************ defines ************************************/



#define SECURE_KILO_VERSION "0.0.1"
#define SECURE_KILO_TAB_STOP 8
#define SECURE_KILO_QUIT_TIMES 2

#define CTRL_KEY(k) ((k) & 0x1f)
#define KEY_ESC ('\x1b')
#define BACKSPACE (127)

enum editorHighlight {
    HL_NORMAL = 1,
    HL_COMMENT,
    HL_MLCOMMENT,
    HL_KEYWORD1,
    HL_KEYWORD2,
    HL_STRING,
    HL_NUMBER,
    HL_MATCH
};

#define HL_HIGHLIGHT_NUMBERS (1 << 0)
#define HL_HIGHLIGHT_STRINGS (1 << 1)
#define MAX_BYTES_UTF8 4



/************************************ data ************************************/



struct editorSyntax {
    char* filetype;
    char** filematch;
    wchar_t** keywords;
    wchar_t* singleline_comment_start;
    wchar_t* multiline_comment_start;
    wchar_t* multiline_comment_end;
    int flags;
};



struct erow {
    int idx;
    int size;
    int rsize;
    wchar_t* chars;
    wchar_t* render;
    unsigned char* hl;
    int hl_open_comment;
};

struct editorConfig {
    int cx, cy;
    int rx;
    int rowoff;
    int coloff;
    int screenrows;
    int screencols;
    int numrows;
    struct erow* rows;
    int dirty;
    char* filename;
    char* gpgtty;
    int gpgpid;
    char statusmsg[80];
    time_t statusmsg_time;
    struct editorSyntax* syntax;
    WINDOW* window;
    char buf[MAX_BYTES_UTF8];
    int buflen;
};
struct editorConfig E;



/************************************ file types ************************************/



char* C_HL_EXTENSIONS[] = { ".c", ".h", ".cpp", NULL };

wchar_t* C_HL_KEYWORDS[] = {
    L"switch", L"if", L"while", L"for", L"break", L"continue", L"return", L"else",
    L"struct", L"union", L"typedef", L"static", L"enum", L"class", L"case",

    L"int|", L"long|", L"double|", L"float|", L"char|", L"unsigned|", L"signed|",
    L"void|", NULL
};

struct editorSyntax HLDB[] = {
    {
        "c",
        C_HL_EXTENSIONS,
        C_HL_KEYWORDS,
        L"//", L"/*", L"*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    }
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))



/************************************ prototypes ************************************/



void editorSetStatusMessage(const char* fmt, ...);
void editorRefreshScreen();
wchar_t* editorPrompt(char* prompt, void (*callback)(wchar_t*, int));



/************************************ terminal ************************************/



void initColors() {
    use_default_colors();
    start_color();
    init_pair(HL_NORMAL, COLOR_BLACK, -1);
    init_pair(HL_COMMENT, COLOR_GREEN, -1);
    init_pair(HL_MLCOMMENT, COLOR_GREEN, -1);
    init_pair(HL_KEYWORD1, COLOR_RED, -1);
    init_pair(HL_KEYWORD2, COLOR_MAGENTA, -1);
    init_pair(HL_STRING, COLOR_BLUE, -1);
    init_pair(HL_NUMBER, COLOR_CYAN, -1);
    init_pair(HL_MATCH, COLOR_RED, -1);
}



void initWindow() {
    E.window = initscr();
    raw();
    noecho();
    keypad(E.window, true);
    initColors();
}



void getWindowSize() {
    E.screenrows = LINES - 2;
    E.screencols = COLS;
}



void cleanup() {
    delwin(E.window);
    endwin();
}



void colorOn(int color) {
    if (has_colors() == FALSE) {
        return;
    }

    attron(COLOR_PAIR(color));
}



void colorOff(int color) {
    if (has_colors() == FALSE) {
        return;
    }

    attroff(COLOR_PAIR(color));
}



/**
 * Leaves ncurses mode temporarily. This is needed
 * when we are about to run another process that
 * will use stdin and output things in the terminal.
 */
void leaveNcursesModeTemporarily() {
    def_prog_mode();
    endwin();
}



void restoreNcursesMode() {
    reset_prog_mode();
    refresh();
}



void die(const char* s) {
    cleanup();

    perror(s);
    exit(1);
}



void dieWithMsg(const char* fmt, ...) {
    cleanup();

    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);

    exit(1);
}



/************************************ wide characters ************************************/



/**
 * Taken from man page of mbrlen.
 *
 * Returns the number of wide characters represented by the given
 * multibyte character string.
 */
size_t nchars(const char *s) {
    size_t charlen, chars;
    mbstate_t mbs;

    chars = 0;
    memset(&mbs, 0, sizeof(mbs));
    while ((charlen = mbrlen(s, MB_CUR_MAX, &mbs)) != 0 &&
            charlen != (size_t)-1 && charlen != (size_t)-2) {
        s += charlen;
        chars++;
    }

    return (chars);
}


/**
 * Returns the number of bytes needed to convert the given
 * wide character string into a multibyte character string,
 * excluding the terminating NULL character.
 */
size_t nbytes(wchar_t* s) {
    char buf[32];
    int n = 0;
    while (*s != '\0') {
        n += wctomb(buf, *s);
        s++;
    }
    return n;
}



char* wc_to_mb(wchar_t* src) {
    if (src == NULL) {
        return NULL;
    }
    int n = nbytes(src);
    char* buf = (char*)malloc(n + 1);
    wcstombs(buf, src, wcslen(src));
    return buf;
}




/************************************ syntax highlighting ************************************/



int is_separator(int c) {
    return iswspace(c) || c == L'\0' || wcschr(L",.()+-/*=~%<>[];", c) != NULL;
}



void editorUpdateSyntax(struct erow* row) {
    row->hl = realloc(row->hl, row->rsize);
    memset(row->hl, HL_NORMAL, row->rsize);

    if (E.syntax == NULL) {
        return;
    }

    wchar_t** keywords = E.syntax->keywords;

    wchar_t* scs = E.syntax->singleline_comment_start;
    wchar_t* mcs = E.syntax->multiline_comment_start;
    wchar_t* mce = E.syntax->multiline_comment_end;

    int scs_len = scs ? wcslen(scs) : 0;
    int mcs_len = scs ? wcslen(mcs) : 0;
    int mce_len = scs ? wcslen(mce) : 0;

    int prev_sep = 1;
    int in_string = 0;
    int in_comment = (row->idx > 0 && E.rows[row->idx - 1].hl_open_comment);

    int i = 0;
    while (i < row -> rsize) {
        wchar_t c = row->render[i];
        unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

        if (scs_len && !in_string && !in_comment) {
            if (!wcsncmp(&row->render[i], scs, scs_len)) {
                memset(&row->hl[i], HL_COMMENT, row->rsize - i);
            }
        }

        if (mcs_len && mce_len && !in_string) {
            if (in_comment) {
                row->hl[i] = HL_MLCOMMENT;
                if (!wcsncmp(&row->render[i], mce, mce_len)) {
                    memset(&row->hl[i], HL_MLCOMMENT, mce_len);
                    i += mce_len;
                    in_comment = 0;
                    prev_sep = 1;
                    continue;
                } else {
                    i++;
                    continue;
                }
            } else if (!wcsncmp(&row->render[i], mcs, mcs_len)) {
                memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
                i += mcs_len;
                in_comment = 1;
                continue;
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
            if (in_string) {
                row->hl[i] = HL_STRING;
                if (c == L'\\' && i + 1 < row->rsize) {
                    row->hl[i + 1] = HL_STRING;
                    i += 2;
                    continue;
                }
                if (c == in_string) {
                    in_string = 0;
                }
                i++;
                prev_sep = 1;
                continue;
            } else {
                if (c == L'"' || c == L'\'') {
                    in_string = c;
                    row->hl[i] = HL_STRING;
                    i++;
                    continue;
                }
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
            if ((iswdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
                (c == '.' && prev_hl == HL_NUMBER)) {
                row->hl[i] = HL_NUMBER;
                i++;
                prev_sep = 0;
                continue;
            }
        }

        if (prev_sep) {
            int j;
            for (j = 0 ; keywords[j] ; j++) {
                int klen = wcslen(keywords[j]);
                int kw2 = keywords[j][klen - 1] == L'|';
                if (kw2) {
                    klen--;
                }

                if (!wcsncmp(&row->render[i], keywords[j], klen) &&
                    is_separator(row->render[i + klen])) {
                    memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
                    i += klen;
                    break;
                }
            }
            if (keywords[j] == NULL) {
                prev_sep = 0;
                continue;
            }
        }

        prev_sep = is_separator(c);
        i++;
    }

    int changed = (row->hl_open_comment != in_comment);
    row->hl_open_comment = in_comment;
    if (changed && row->idx + 1 < E.numrows) {
        editorUpdateSyntax(&E.rows[row->idx + 1]);
    }
}



void editorSelectSyntaxHighlight() {
    E.syntax = NULL;
    if (E.filename == NULL) {
        return;
    }

    char* ext = strrchr(E.filename, '.');

    for (unsigned int j = 0 ; j < HLDB_ENTRIES ; j++) {
        struct editorSyntax* s = &HLDB[j];
        unsigned int i = 0;

        editorSetStatusMessage("match %s", s->filematch[i]);
        while (s->filematch[i]) {
            int is_ext = (s->filematch[i][0] == '.');
            if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
                (!is_ext && strstr(E.filename, s->filematch[i]))) {
                E.syntax = s;

                int filerow;
                for (filerow = 0 ; filerow < E.numrows ; filerow++) {
                    editorUpdateSyntax(&E.rows[filerow]);
                }

                return;
            }
            i++;
        }
    }
}



/************************************ row operations ************************************/



int editorRowCxToRx(struct erow* row, int cx) {
    int rx = 0;
    int j;
    for (j = 0 ; j < cx ; j++) {
        if (row->chars[j] == L'\t') {
            rx += (SECURE_KILO_TAB_STOP - 1) - (rx % SECURE_KILO_TAB_STOP);
        }
        rx++;
    }
    return rx;
}



int editorRowRxToCx(struct erow* row, int rx) {
    int cur_rx = 0;
    int cx;
    for (cx = 0 ; cx < row->size ; cx++) {
        if (row->chars[cx] == L'\t') {
            cur_rx += (SECURE_KILO_TAB_STOP - 1) - (cur_rx % SECURE_KILO_TAB_STOP);
        }
        cur_rx++;

        if (cur_rx > rx) {
            return cx;
        }
    }
    return cx;
}



void editorUpdateRow(struct erow* row) {
    int tabs = 0;
    int j;
    for (j = 0 ; j < row->size ; j++) {
        if (row->chars[j] == L'\t') {
            tabs++;
        }
    }

    free(row->render);
    row->render = malloc(sizeof(wchar_t) * (row->size + tabs * (SECURE_KILO_TAB_STOP - 1) + 1));

    int idx = 0;
    for (j = 0 ; j < row->size ; j++) {
        if (row->chars[j] == L'\t') {
            row->render[idx++] = ' ';
            while (idx % SECURE_KILO_TAB_STOP != 0) {
                row->render[idx++] = L' ';
            }
        } else {
            row->render[idx++] = row->chars[j];
        }
    }
    row->render[idx] = L'\0';
    row->rsize = idx;

    editorUpdateSyntax(row);
}



void editorInsertRow(int at, wchar_t* s, size_t len) {
    if (at < 0 || at > E.numrows) {
        return;
    }

    E.rows = realloc(E.rows, sizeof(struct erow) * (E.numrows + 1));
    memmove(&E.rows[at + 1], &E.rows[at], sizeof(struct erow) * (E.numrows - at));
    for (int j = at + 1 ; j <= E.numrows ; j++) {
        E.rows[j].idx++;
    }

    E.rows[at].idx = at;

    E.rows[at].size = len;
    E.rows[at].chars = wcsdup(s);

    E.rows[at].rsize = 0;
    E.rows[at].render = NULL;
    E.rows[at].hl = NULL;
    E.rows[at].hl_open_comment = 0;
    editorUpdateRow(&E.rows[at]);

    E.numrows++;
    E.dirty++;
}



void editorFreeRow(struct erow* row) {
    free(row->render);
    free(row->chars);
    free(row->hl);
}



void editorDelRow(int at) {
    if (at < 0 || at >= E.numrows) {
        return;
    }
    editorFreeRow(&E.rows[at]);
    memmove(&E.rows[at], &E.rows[at + 1], sizeof(struct erow) * (E.numrows - at - 1));
    for (int j = at ; j < E.numrows - 1 ; j++) {
        E.rows[j].idx--;
    }
    E.numrows--;
    E.dirty++;
}



void editorRowInsertChar(struct erow* row, int at, wchar_t c) {
    if (at < 0 || at > row->size) {
        at = row->size;
    }
    row->chars = realloc(row->chars, sizeof(wchar_t) * (row->size + 2));
    wmemmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
    row->size++;
    row->chars[at] = c;
    editorUpdateRow(row);
    E.dirty++;
}



void editorRowAppendString(struct erow* row, wchar_t* s, size_t len) {
    row->chars = realloc(row->chars, sizeof(wchar_t) * (row->size + len + 1));
    wmemcpy(&row->chars[row->size], s, len);
    row->size += len;
    row->chars[row->size] = L'\0';
    editorUpdateRow(row);
    E.dirty++;
}



void editorRowDelChar(struct erow* row, int at) {
    if (at < 0 || at >= row->size) {
        return;
    }
    wmemmove(&row->chars[at], &row->chars[at + 1], row->size - at);
    row->size--;
    editorUpdateRow(row);
    E.dirty++;
}



/************************************ editor operations ************************************/



void editorInsertChar(wchar_t c) {
    if (E.cy == E.numrows) {
        editorInsertRow(E.numrows, L"", 0);
    }
    editorRowInsertChar(&E.rows[E.cy], E.cx, c);
    E.cx++;
}



void editorInsertNewLine() {
    if (E.cx == 0) {
        editorInsertRow(E.cy, L"", 0);
    } else {
        struct erow* row = &E.rows[E.cy];
        editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
        row = &E.rows[E.cy];
        row->size = E.cx;
        row->chars[row->size] = L'\0';
        editorUpdateRow(row);
    }
    E.cy++;
    E.cx = 0;
}



void editorDelChar() {
    if (E.cy == E.numrows) {
        return;
    }

    if (E.cx == 0 && E.cy == 0) {
        return;
    }

    struct erow* row = &E.rows[E.cy];
    if (E.cx > 0) {
        editorRowDelChar(row, E.cx - 1);
        E.cx--;
    } else {
        E.cx = E.rows[E.cy - 1].size;
        editorRowAppendString(&E.rows[E.cy - 1], row->chars, row->size);
        editorDelRow(E.cy);
        E.cy--;
    }
}



char* editorRowsToString(int *buflen) {
    int totlen = 0;
    int i, j;
    char tmp[MAX_BYTES_UTF8];
    for (j = 0 ; j < E.numrows ; j++) {
        for (i = 0 ; i < E.rows[j].size ; i++) {
            totlen += wctomb(tmp, E.rows[j].chars[i]);
        }
        totlen++;
    }
    *buflen = totlen;

    char* buf = malloc(totlen);
    char* p = buf;
    for (j = 0 ; j < E.numrows ; j++) {
        for (i = 0 ; i < E.rows[j].size ; i++) {
            int n = wctomb(p, E.rows[j].chars[i]);
            p += n;
        }

        *p = L'\n';
        p++;
    }
    return buf;
}



/************************************ file i/o ************************************/



/**
 * Returns a file descriptor corresponding to the content
 * of the given file after decryption by gpg, or dies if
 * any error occurs. It does so by feeding the given file to
 * gpg for decryption and redirects gpg's output to a pipe.
 * The returned descripor is the read descriptor of the pipe.
 */
int openForSecureRead(char* filename) {
    int p[2];
    if (-1 == pipe(p)) {
        die("pipe");
    }

    int pid = fork();
    switch(pid) {
        case -1: die("fork"); return -1;
        case 0: {
            // child
            close(p[0]);

            int in = open(filename, O_RDONLY);
            if (-1 == in) {
	            die("open");
	            return -1;
            }

            if (-1 == dup2(in, 0)) {
	            die("dup2");
	            return -1;
            }

            if (-1 == dup2(p[1], 1)) {
	            die("dup2");
	            return -1;
            }

            int devNull = open("/dev/null", O_WRONLY);
            if (devNull == -1) {
                die("open");
                return -1;
            }

            if (-1 == dup2(devNull, 2)) {
	            die("dup2");
	            return -1;
            }

            putenv(E.gpgtty);
            execlp("gpg", "gpg", "--no-symkey-cache", "-d", NULL);
            die("execlp");
            return -1;
        }
        default: {
            // father
            close(p[1]);
            E.gpgpid = pid;
            return p[0];
        }
    }
}



/**
 * Same trick as openForSecureRead but for write.
 */
int openForSecureWrite(char* filename) {
    int p[2];
    if (-1 == pipe(p)) {
        die("pipe");
    }

    int pid = fork();
    switch(pid) {
        case -1: die("fork"); return -1;
        case 0: {
            // child
            close(p[1]);

            if (-1 == dup2(p[0], 0)) {
	            die("dup2");
	            return -1;
            }

            int devNull = open("/dev/null", O_WRONLY);
            if (devNull == -1) {
                die("open");
                return -1;
            }

            if (-1 == dup2(devNull, 2)) {
	            die("dup2");
	            return -1;
            }

            putenv(E.gpgtty);
            execlp("gpg", "gpg", "--no-symkey-cache", "-c", "--output", filename, NULL);
            die("execlp");
            return -1;
        }
        default: {
            // father
            close(p[0]);
            E.gpgpid = pid;
            return p[1];
        }
    }
}




void editorOpenSecure(char* filename) {
    free(E.filename);
    E.filename = strdup(filename);

    editorSelectSyntaxHighlight();

    leaveNcursesModeTemporarily();

    int fd = openForSecureRead(filename);
    int N = 4096;
    char buf[N];
    int n;
    size_t linebuflen = N;
    char* linebuf = (char*)malloc(linebuflen);
    size_t wbuflen = N;
    wchar_t* wbuf = (wchar_t*)malloc(wbuflen);
    size_t curpos = 0;
    while ((n = read(fd, buf, N)) > 0) {
        for (int i = 0 ; i < n ; i++) {
            if (curpos == linebuflen - 1) {
                linebuflen *= 2;
                linebuf = (char*)realloc(linebuf, linebuflen);
            }

            if (buf[i] != '\n') {
                linebuf[curpos++] = buf[i];
                continue;
            }

            do {
                linebuf[curpos--] = '\0';
            } while (curpos >= 0 && linebuf[curpos] == '\r');


            size_t nwchars = nchars(linebuf);
            if (nwchars + 1 > wbuflen) {
                wbuflen = nwchars + 1;
                wbuf = (wchar_t*)realloc(wbuf, sizeof(wchar_t) * wbuflen);
            }
            mbstowcs(wbuf, linebuf, nwchars);
            wbuf[nwchars] = L'\0';

            editorInsertRow(E.numrows, wbuf, nwchars);
            curpos = 0;
        }
    }

    free(linebuf);
    close(fd);
    E.dirty = 0;

    // Let's verify that the gpg process did not die with an error
    int status;
    waitpid(E.gpgpid, &status, 0);
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
        dieWithMsg("Could not decrypt file '%s'\r\n", filename);
    }

    restoreNcursesMode();
}



void editorSaveSecure() {
    char* output = NULL;
    if (E.filename == NULL) {
        wchar_t* wname = editorPrompt("Save as: %s (ESC to cancel)", NULL);
        E.filename = wc_to_mb(wname);
        free(wname);

        if (E.filename == NULL) {
            editorSetStatusMessage("Save aborted");
            return;
        }
        output = strdup(E.filename);
        editorSelectSyntaxHighlight();
    }

    int N;
    char* buf = editorRowsToString(&N);


    if (output == NULL) {
        output = malloc(strlen(E.filename) + 8);
        sprintf(output, "%s.XXXXXX", E.filename);
        mktemp(output);
    }

    leaveNcursesModeTemporarily();

    int fd = openForSecureWrite(output);

    int len;
    int written = 0;
    while (written != N && -1 != (len = write(fd, buf + written, N - written))) {
        written += len;
    }

    close(fd);
    free(buf);

    int status;
    waitpid(E.gpgpid, &status, 0);

    int success = WIFEXITED(status) && WEXITSTATUS(status) == 0 && written == N;
    restoreNcursesMode();

    if (!success) {
        free(output);
        editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
        return;
    }

    struct stat ebuf;
    if (-1 == stat(output, &ebuf)) {
        free(output);
        editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
        return;
    }

    if (strcmp(output, E.filename)) {
        // We need to replace the original file with the one created
        struct stat buf;
        if (-1 == stat(E.filename, &buf)
            || -1 == chmod(output, buf.st_mode)
            || -1 == remove(E.filename)
            || -1 == rename(output, E.filename)) {

            free(output);
            editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
            return;
        }
    }

    free(output);
    E.dirty = 0;
    editorSetStatusMessage("%d data bytes encrypted into %d bytes written to disk", len, ebuf.st_size);
}



/**
 * For gpg to accept reading data from stdin, we need
 * to set the environment variable "GPG_TTY=<current tty>".
 * The purpose of this function is to retrieve the current terminal. 
 */
void getTTY() {
    int p[2];
    if (-1 == pipe(p)) {
        die("pipe");
    }

    int pid = fork();
    switch(pid) {
        case -1: die("fork"); return;
        case 0: {
            // child
            close(p[0]);

            if (-1 == dup2(p[1], 1)) {
	            die("dup2");
	            return;
            }

            execlp("tty", "tty", NULL);
            die("execlp");
            return;
        }
        default: {
            // father
            close(p[1]);
            int N = 256;
            char buf[N];
            int len = 0;
            int n;
            while ((n = read(p[0], buf + len, N - len)) > 0) {
                len += n;
            }
            close(p[0]);

            int status;
            waitpid(pid, &status, 0);
            if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
                dieWithMsg("Could not identify terminal");
            }
            while (len > 0 && (buf[len - 1] == '\r' || buf[len - 1] == '\n')) {
                buf[len - 1] = '\0';
                len--;
            }

            // Let's store GPG_TTY=<value>
            E.gpgtty = malloc(len + 9);
            sprintf(E.gpgtty, "GPG_TTY=%s", buf);
        }
    }
}



/************************************ find ************************************/



void editorFindCallback(wchar_t* query, int key) {
    static int last_match = -1;
    static int direction = 1;

    static int saved_hl_line;
    static char* saved_hl = NULL;

    if (saved_hl) {
        memcpy(E.rows[saved_hl_line].hl, saved_hl, E.rows[saved_hl_line].rsize);
    }

    if (key == '\n' || key == KEY_ESC) {
        last_match = -1;
        direction = 1;
        return;
    } else if (key == KEY_RIGHT || key == KEY_DOWN) {
        direction = 1;
    } else if (key == KEY_LEFT || key == KEY_UP) {
        direction = -1;
    } else {
        last_match = -1;
        direction = 1;
    }

    if (last_match == -1) {
        direction = 1;
    }
    int current = last_match;
    int i;
    for (i = 0 ; i < E.numrows ; i++) {
        current += direction;
        if (current == -1) {
            current = E.numrows - 1;
        } else if (current == E.numrows) {
            current = 0;
        }

        struct erow* row = &E.rows[current];
        wchar_t* match = wcsstr(row->render, query);
        if (match) {
            last_match = current;
            E.cy = current;
            E.cx = editorRowRxToCx(row, match - row->render);
            E.rowoff = E.numrows;

            saved_hl_line = current;
            saved_hl = malloc(row->size);
            memcpy(saved_hl, row->hl, row->rsize);
            memset(&row->hl[match - row->render], HL_MATCH, wcslen(query));
            break;
        }
    }
}



void editorFind() {
    int saved_cx = E.cx;
    int saved_cy = E.cy;
    int saved_coloff = E.coloff;
    int saved_rowoff = E.rowoff;

    wchar_t* query = editorPrompt("Search: %S (Use ESC/Arrows/Enter)", editorFindCallback);
    if (query) {
        free(query);
    } else {
        E.cx = saved_cx;
        E.cy = saved_cy;
        E.coloff = saved_coloff;
        E.rowoff = saved_rowoff;
    }
}



/************************************ output ************************************/



void editorScroll() {
    E.rx = 0;
    if (E.cy < E.numrows) {
        E.rx = editorRowCxToRx(&E.rows[E.cy], E.cx);
    }

    if (E.cy < E.rowoff) {
        E.rowoff = E.cy;
    }
    if (E.cy >= E.rowoff + E.screenrows) {
        E.rowoff = E.cy - E.screenrows + 1;
    }
    if (E.rx < E.coloff) {
        E.coloff = E.rx;
    }
    if (E.rx >= E.coloff + E.screencols) {
        E.coloff = E.rx - E.screencols + 1;
    }
}



void editorDrawRows() {
    int y;
    for (y = 0 ; y < E.screenrows; y++) {
        int filerow = y + E.rowoff;
        int x = 0;
        if (filerow >= E.numrows) {
            colorOn(HL_NORMAL);
            if (E.numrows == 0 && y == E.screenrows / 3) {
                char welcome[80];
                int welcomelen = snprintf(welcome, sizeof(welcome),
                   "Secure Kilo editor -- version %s", SECURE_KILO_VERSION);
                if (welcomelen > E.screencols) {
                    welcomelen = E.screencols;
                }
                int padding = (E.screencols - welcomelen) / 2;
                if (padding) {
                    mvprintw(y, x++, "~");
                    padding--;
                }
                while (padding--) {
                    mvprintw(y, x++, " ");
                }
                mvprintw(y, x, welcome);
                clrtoeol();
            } else {
                mvprintw(y, 0, "~");
                clrtoeol();
            }
            colorOff(HL_NORMAL);
        } else {
            int len = E.rows[filerow].rsize - E.coloff;
            if (len < 0) {
                len = 0;
            }
            if (len > E.screencols) {
                len = E.screencols;
            }
            wchar_t* c = &E.rows[filerow].render[E.coloff];
            unsigned char* hl = &E.rows[filerow].hl[E.coloff];
            int current_color = HL_NORMAL;
            colorOn(current_color);
            int j;
            move(y, x);
            for (j = 0 ; j < len ; j++) {
                if (hl[j] != current_color) {
                    colorOff(current_color);
                    current_color = hl[j];
                    colorOn(current_color);
                }
                wchar_t ch;
                if (iscntrl(c[j])) {
                    ch = (c[j] <= 26) ? '@' + c[j] : '?';
                } else {
                    ch = c[j];
                }
                mvprintw(y, x++, "%lc", ch);
            }
            clrtoeol();
        }
    }
}



void editorDrawStatusBar() {

    colorOn(HL_NORMAL);

    char status[80];
    char rstatus[80];
    int len = snprintf(status, sizeof(status), "%.20s - %d line%s %s",
        E.filename ? E.filename : "[No Name]", E.numrows, E.numrows > 1 ? "s" : "",
        E.dirty ? "(modified)" : "");
    int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d",
        E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.numrows);
    if (len > E.screencols) {
        len = E.screencols;
    }
    int y = E.screenrows;
    int x = 0;
    mvprintw(y, x, status);
    x += len;

    while (len < E.screencols) {
        if (E.screencols - len == rlen) {
            mvprintw(y, x, rstatus);
            x += rlen;
            break;
        }
        mvprintw(y, x++, " ");
        len++;
    }
    clrtoeol();
    colorOff(HL_NORMAL);
}



void editorDrawMessageBar() {
    int y = E.screenrows + 1;
    colorOn(HL_NORMAL);
    int msglen = strlen(E.statusmsg);
    if (msglen > E.screencols) {
        msglen = E.screencols;
    }
    move(y, 0);
    if (msglen && time(NULL) - E.statusmsg_time < 5) {
        char old = E.statusmsg[msglen];
        E.statusmsg[msglen] = '\0';
        mvprintw(y, 0, E.statusmsg);
        E.statusmsg[msglen] = old;
    }
    clrtoeol();
    colorOff(HL_NORMAL);
}



void editorRefreshScreen() {
    getWindowSize();
    editorScroll();

    editorDrawRows();
    editorDrawStatusBar();
    editorDrawMessageBar();

    int cursorX = (E.rx - E.coloff);
    int cursorY = (E.cy - E.rowoff);
    mvchgat(cursorY, cursorX, 1, A_REVERSE, 0, NULL);

    refresh();
}



void editorSetStatusMessage(const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
    va_end(ap);
    E.statusmsg_time = time(NULL);
}



/************************************ input ************************************/



wchar_t* editorPrompt(char* prompt, void (*callback)(wchar_t*, int)) {
    size_t bufsize = 128;
    wchar_t* buf = (wchar_t*)malloc(sizeof(wchar_t) * bufsize);

    size_t buflen = 0;
    buf[0] = L'\0';

    while (1) {
        editorSetStatusMessage(prompt, buf);
        editorRefreshScreen();

        int c = getch();

        if (c == KEY_RESIZE) {
            continue;
        } else if (c == KEY_DC || c == CTRL_KEY('h') || c == KEY_BACKSPACE || c == BACKSPACE) {
            if (buflen > 0) {
                buf[--buflen] = L'\0';
            }
        } else if (c == KEY_ESC) {
            editorSetStatusMessage("");
            if (callback) {
                callback(buf, c);
            }
            free(buf);
            return NULL;
        } else if (c == '\n') {
            if (buflen != 0) {
                editorSetStatusMessage("");
                if (callback) {
                    callback(buf, c);
                }
                return buf;
            }
        } else if (!iscntrl(c) && c < 128) {
            if (buflen == bufsize - 1) {
                bufsize *= 2;
                buf = realloc(buf, sizeof(wchar_t) * bufsize);
            }
            buf[buflen++] = c;
            buf[buflen] = L'\0';
        }

        if (callback) {
            callback(buf, c);
        }
    }
}



void editorMoveCursor(int key) {
    struct erow* row = (E.cy >= E.numrows) ? NULL : &E.rows[E.cy];
    switch(key) {
        case KEY_LEFT: {
            if (E.cx != 0) {
                E.cx--;
            } else if (E.cy > 0) {
                E.cy--;
                E.cx = E.rows[E.cy].size;
            }
            break;
        }
        case KEY_RIGHT: {
            if (row && E.cx < row -> size) {
                E.cx++;                
            } else if (row && E.cx == row->size) {
                E.cy++;
                E.cx = 0;
            }
            break;
        }
        case KEY_UP: {
            if (E.cy != 0) {
                E.cy--;
            }
            break;
        }
        case KEY_DOWN: {
            if (E.cy < E.numrows) {
                E.cy++;
            }
            break;
        }
    }

    row = (E.cy >= E.numrows) ? NULL : &E.rows[E.cy];
    int rowlen = row ? row->size : 0;
    if (E.cx > rowlen) {
        E.cx = rowlen;
    } 
}



void handleByte(int c) {
    E.buf[E.buflen++] = c;

    wchar_t wc = 0;
    mbtowc(&wc, NULL, 0);
    int res = mbtowc(&wc, E.buf, E.buflen);
    if (res > 0) {
        E.buflen = 0;
        editorInsertChar(wc);
    } else if (E.buflen == MAX_BYTES_UTF8 - 1) {
        E.buflen = 0;
    }
}


void editorProcessKeypress() {
    static int quit_times = SECURE_KILO_QUIT_TIMES;

    int c = getch();

    switch(c) {
        // Don't try to add characters when the value returned by getch()
        // is indicating that a resize operation has occurred on the terminal
        case -1:
        case KEY_RESIZE: {
            break;
        }

        case '\n': {
            editorInsertNewLine();
            break;
        }

        case CTRL_KEY('q'): {
            if (E.dirty && quit_times > 0) {
                editorSetStatusMessage("WARNING!!! File has unsaved changes. "
                    "Press Ctrl-Q %d more times to quit.", quit_times);
                quit_times--;
                return;
            }

            cleanup();
            exit(0);
            break;
        }

        case CTRL_KEY('s'): {
            editorSaveSecure();
            break;
        }

        case KEY_HOME: {
            E.cx = 0;
            break;
        }

        case KEY_END: {
            if (E.cy < E.numrows) {
                E.cx = E.rows[E.cy].size;
            }
            break;
        }

        case CTRL_KEY('f'): {
            editorFind();
            break;
        }

        case BACKSPACE:
        case KEY_BACKSPACE:
        case CTRL_KEY('h'):
        case KEY_DC: {
            if (c == KEY_DC) {
                editorMoveCursor(KEY_RIGHT);
            }
            editorDelChar();
            break;
        }

        case KEY_PPAGE:
        case KEY_NPAGE: {
            if (c == KEY_PPAGE) {
                E.cy = E.rowoff;
            } else {
                E.cy = E.rowoff + E.screenrows - 1;
                if (E.cy > E.numrows) {
                    E.cy = E.numrows;
                }
            }

            int times = E.screenrows;
            while (times--) {
                editorMoveCursor(c == KEY_PPAGE ? KEY_UP : KEY_DOWN);
            }
            break;
        }

        case KEY_UP:
        case KEY_DOWN:
        case KEY_LEFT:
        case KEY_RIGHT: {
            editorMoveCursor(c);
            break;
        }

        case CTRL_KEY('l'):
        case KEY_ESC: {
            break;
        }

        default: {
            handleByte(c);
            break;
        }
    }

    quit_times = SECURE_KILO_QUIT_TIMES;
}



/************************************ init ************************************/



void initEditor() {
    E.cx = 0;
    E.cy = 0;
    E.rx = 0;
    E.rowoff = 0;
    E.coloff = 0;
    E.numrows = 0;
    E.rows = NULL;
    E.dirty = 0;
    E.filename = NULL;
    E.statusmsg[0] = '\0';
    E.statusmsg_time = 0;
    E.syntax = NULL;

    getWindowSize();
}



int main(int argc, char* argv[]) {
    if (NULL == setlocale(LC_CTYPE, "UTF-8")) {
        dieWithMsg("'UTF-8' is not a supported locale !\n");
    }

    getTTY();
    initWindow();
    initEditor();

    if (argc >= 2) {
        editorOpenSecure(argv[1]);
    }

    editorSetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

    while (1) {
        editorRefreshScreen();
        editorProcessKeypress();
    }
    return 0;
}
