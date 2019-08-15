
/************************************ includes ************************************/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
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



/************************************ defines ************************************/



#define SECURE_KILO_VERSION "0.0.1"
#define SECURE_KILO_TAB_STOP 8
#define SECURE_KILO_QUIT_TIMES 2

#define CTRL_KEY(k) ((k) & 0x1f)

enum EDITOR_KEY {
    BACKSPACE = 127,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN
};

enum editorHighlight {
    HL_NORMAL = 0,
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



/************************************ data ************************************/



struct editorSyntax {
    char* filetype;
    char** filematch;
    char** keywords;
    char* singleline_comment_start;
    char* multiline_comment_start;
    char* multiline_comment_end;
    int flags;
};



struct erow {
    int idx;
    int size;
    int rsize;
    char* chars;
    char* render;
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
    struct termios orig_termios;
};
struct editorConfig E;



/************************************ file types ************************************/



char* C_HL_EXTENSIONS[] = { ".c", ".h", ".cpp", NULL };

char* C_HL_KEYWORDS[] = {
    "switch", "if", "while", "for", "break", "continue", "return", "else",
    "struct", "union", "typedef", "static", "enum", "class", "case",

    "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
    "void|", NULL
};

struct editorSyntax HLDB[] = {
    {
        "c",
        C_HL_EXTENSIONS,
        C_HL_KEYWORDS,
        "//", "/*", "*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    }
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))



/************************************ prototypes ************************************/



void editorSetStatusMessage(const char* fmt, ...);
void editorRefreshScreen();
char* editorPrompt(char* prompt, void (*callback)(char*, int));



/************************************ terminal ************************************/



void die(const char* s) {
    write(STDOUT_FILENO, "\x1b[2J", 4);
    write(STDOUT_FILENO, "\x1b[H", 3);

    perror(s);
    exit(1);
}



void dieWithMsg(const char* fmt, ...) {
    write(STDOUT_FILENO, "\x1b[2J", 4);
    write(STDOUT_FILENO, "\x1b[H", 3);

    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);

    exit(1);
}



void disableRawMode() {
    if (-1 == tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios)) {
        die("tcsetattr");
    }
}



void enableRawMode() {
    if (-1 == tcgetattr(STDIN_FILENO, &E.orig_termios)) {
        die("tcgetattr");
    }
    atexit(disableRawMode);

    struct termios raw = E.orig_termios;
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= (CS8);
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;

    if (-1 == tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw)) {
        die("tcsetattr");
    }
}



int editorReadKey() {
    int nread;
    char c;
    while (1 != (nread = read(STDIN_FILENO, &c, 1))) {
        if (-1 == nread && errno != EAGAIN) {
            die("read");
        }
    }

    if (c == '\x1b') {
        char seq[3];

        if (1 != read(STDIN_FILENO, &seq[0], 1)) {
            return '\x1b';
        }
        if (1 != read(STDIN_FILENO, &seq[1], 1)) {
            return '\x1b';
        }

        if (seq[0] == '[') {
            if (seq[1] >= '0' && seq[1] <= '9') {
                if (1 != read(STDIN_FILENO, &seq[2], 1)) {
                    return '\x1b';
                }
                if (seq[2] == '~') {
                    switch (seq[1]) {
                        case '1': return HOME_KEY;
                        case '3': return DEL_KEY;
                        case '4': return END_KEY;
                        case '5': return PAGE_UP;
                        case '6': return PAGE_DOWN;
                        case '7': return HOME_KEY;
                        case '8': return END_KEY;
                    }
                }

            } else {
                switch(seq[1]) {
                    case 'A': return ARROW_UP;
                    case 'B': return ARROW_DOWN;
                    case 'C': return ARROW_RIGHT;
                    case 'D': return ARROW_LEFT;
                    case 'E': return HOME_KEY;
                    case 'F': return END_KEY;
                }
            }
        } else if (seq[0] == 'O') {
            switch(seq[1]) {
                case 'H': return HOME_KEY;
                case 'F': return END_KEY;
            }
        }
        return '\x1b';
    }

    return c;
}



int getCursorPosition(int *row, int *col) {
    char buf[32];
    unsigned int i = 0;
    if (4 != write(STDOUT_FILENO, "\x1b[6n", 4)) {
        return -1;
    }

    printf("\r\n");
    while (i < sizeof(buf) - 1) {
        if (1 != read(STDIN_FILENO, &buf[i], 1)) {
            break;
        }
        if (buf[i] == 'R') {
            break;
        }
        i++;
    }
    buf[i] = '\0';
    if (buf[0] != '\x1b' || buf[1] != '[') {
        return -1;
    }
    if (2 != sscanf(&buf[2], "%d;%d", row, col)) {
        return -1;
    }
    return 0;
}



int getWindowSize(int *rows, int *cols) {
    WINDOW* w = initscr();
    *rows = LINES;
    *cols = COLS;
    delwin(w);
    endwin();
    refresh();
    return 0;
}



/************************************ syntax highlighting ************************************/



int is_separator(int c) {
    return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}



void editorUpdateSyntax(struct erow* row) {
    row->hl = realloc(row->hl, row->rsize);
    memset(row->hl, HL_NORMAL, row->rsize);

    if (E.syntax == NULL) {
        return;
    }

    char** keywords = E.syntax->keywords;

    char* scs = E.syntax->singleline_comment_start;
    char* mcs = E.syntax->multiline_comment_start;
    char* mce = E.syntax->multiline_comment_end;

    int scs_len = scs ? strlen(scs) : 0;
    int mcs_len = scs ? strlen(mcs) : 0;
    int mce_len = scs ? strlen(mce) : 0;

    int prev_sep = 1;
    int in_string = 0;
    int in_comment = (row->idx > 0 && E.rows[row->idx - 1].hl_open_comment);

    int i = 0;
    while (i < row -> rsize) {
        char c = row->render[i];
        unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

        if (scs_len && !in_string && !in_comment) {
            if (!strncmp(&row->render[i], scs, scs_len)) {
                memset(&row->hl[i], HL_COMMENT, row->rsize - i);
            }
        }

        if (mcs_len && mce_len && !in_string) {
            if (in_comment) {
                row->hl[i] = HL_MLCOMMENT;
                if (!strncmp(&row->render[i], mce, mce_len)) {
                    memset(&row->hl[i], HL_MLCOMMENT, mce_len);
                    i += mce_len;
                    in_comment = 0;
                    prev_sep = 1;
                    continue;
                } else {
                    i++;
                    continue;
                }
            } else if (!strncmp(&row->render[i], mcs, mcs_len)) {
                memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
                i += mcs_len;
                in_comment = 1;
                continue;
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
            if (in_string) {
                row->hl[i] = HL_STRING;
                if (c == '\\' && i + 1 < row->rsize) {
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
                if (c == '"' || c == '\'') {
                    in_string = c;
                    row->hl[i] = HL_STRING;
                    i++;
                    continue;
                }
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
            if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
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
                int klen = strlen(keywords[j]);
                int kw2 = keywords[j][klen - 1] == '|';
                if (kw2) {
                    klen--;
                }

                if (!strncmp(&row->render[i], keywords[j], klen) &&
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



int editorSyntaxToColor(int hl) {
    switch(hl) {
        case HL_COMMENT:
        case HL_MLCOMMENT: return 36;
        case HL_KEYWORD1: return 33;
        case HL_KEYWORD2: return 32;
        case HL_STRING: return 35;
        case HL_NUMBER: return 31;
        case HL_MATCH: return 34;
        default: return 37;
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

        editorSetStatusMessage("ZZZ match %s", s->filematch[i]);
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
        if (row->chars[j] == '\t') {
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
        if (row->chars[cx] == '\t') {
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
        if (row->chars[j] == '\t') {
            tabs++;
        }
    }

    free(row->render);
    row->render = malloc(row->size + tabs * (SECURE_KILO_TAB_STOP - 1) + 1);

    int idx = 0;
    for (j = 0 ; j < row->size ; j++) {
        if (row->chars[j] == '\t') {
            row->render[idx++] = ' ';
            while (idx % SECURE_KILO_TAB_STOP != 0) {
                row->render[idx++] = ' ';
            }
        } else {
            row->render[idx++] = row->chars[j];
        }
    }
    row->render[idx] = '\0';
    row->rsize = idx;

    editorUpdateSyntax(row);
}



void editorInsertRow(int at, char* s, size_t len) {
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
    E.rows[at].chars = malloc(len + 1);
    memcpy(E.rows[at].chars, s, len);
    E.rows[at].chars[len] = '\0';

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



void editorRowInsertChar(struct erow* row, int at, int c) {
    if (at < 0 || at > row->size) {
        at = row->size;
    }
    row->chars = realloc(row->chars, row->size + 2);
    memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
    row->size++;
    row->chars[at] = c;
    editorUpdateRow(row);
    E.dirty++;
}



void editorRowAppendString(struct erow* row, char* s, size_t len) {
    row->chars = realloc(row->chars, row->size + len + 1);
    memcpy(&row->chars[row->size], s ,len);
    row->size += len;
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
    E.dirty++;
}



void editorRowDelChar(struct erow* row, int at) {
    if (at < 0 || at >= row->size) {
        return;
    }
    memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
    row->size--;
    editorUpdateRow(row);
    E.dirty++;
}



/************************************ editor operations ************************************/



void editorInsertChar(int c) {
    if (E.cy == E.numrows) {
        editorInsertRow(E.numrows, "", 0);
    }
    editorRowInsertChar(&E.rows[E.cy], E.cx, c);
    E.cx++;
}



void editorInsertNewLine() {
    if (E.cx == 0) {
        editorInsertRow(E.cy, "", 0);
    } else {
        struct erow* row = &E.rows[E.cy];
        editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
        row = &E.rows[E.cy];
        row->size = E.cx;
        row->chars[row->size] = '\0';
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
    int j;
    for (j = 0 ; j < E.numrows ; j++) {
        totlen += E.rows[j].size + 1;
    }
    *buflen = totlen;

    char* buf = malloc(totlen);
    char* p = buf;
    for (j = 0 ; j < E.numrows ; j++) {
        memcpy(p, E.rows[j].chars, E.rows[j].size);
        p += E.rows[j].size;
        *p = '\n';
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

    int fd = openForSecureRead(filename);

    int N = 4096;
    char buf[N];
    int n;
    size_t linebuflen = N;
    char* linebuf = (char*)malloc(linebuflen);
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

            editorInsertRow(E.numrows, linebuf, curpos + 1);

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
}



void editorSaveSecure() {
    char* output = NULL;
    if (E.filename == NULL) {
        E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
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



void editorFindCallback(char* query, int key) {
    static int last_match = -1;
    static int direction = 1;

    static int saved_hl_line;
    static char* saved_hl = NULL;

    if (saved_hl) {
        memcpy(E.rows[saved_hl_line].hl, saved_hl, E.rows[saved_hl_line].rsize);
    }

    if (key == '\r' || key == '\x1b') {
        last_match = -1;
        direction = 1;
        return;
    } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
        direction = 1;
    } else if (key == ARROW_LEFT || key == ARROW_UP) {
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
    for (i = 0 ; E.numrows ; i++) {
        current += direction;
        if (current == -1) {
            current = E.numrows - 1;
        } else if (current == E.numrows) {
            current = 0;
        }

        struct erow* row = &E.rows[current];
        char* match = strstr(row->render, query);
        if (match) {
            last_match = current;
            E.cy = current;
            E.cx = editorRowRxToCx(row, match - row->render);
            E.rowoff = E.numrows;

            saved_hl_line = current;
            saved_hl = malloc(row->size);
            memcpy(saved_hl, row->hl, row->rsize);
            memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
            break;
        }
    }
}



void editorFind() {
    int saved_cx = E.cx;
    int saved_cy = E.cy;
    int saved_coloff = E.coloff;
    int saved_rowoff = E.rowoff;

    char* query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)", editorFindCallback);
    if (query) {
        free(query);
    } else {
        E.cx = saved_cx;
        E.cy = saved_cy;
        E.coloff = saved_coloff;
        E.rowoff = saved_rowoff;
    }
}



/************************************ append buffer ************************************/

struct abuf {
    char* b;
    int len;
};

#define ABUF_INIT { NULL, 0 }



void abAppend(struct abuf* ab, const char* s, int len) {
    char* new = realloc(ab->b, ab->len + len);

    if (new == NULL) {
        return;
    }
    memcpy(&new[ab->len], s, len);
    ab->b = new;
    ab->len += len;
}



void abFree(struct abuf* ab) {
    free(ab->b);
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

void editorDrawRows(struct abuf* ab) {
    int y;
    for (y = 0 ; y < E.screenrows; y++) {
        int filerow = y + E.rowoff;
        if (filerow >= E.numrows) {
            if (E.numrows == 0 && y == E.screenrows / 3) {
                char welcome[80];
                int welcomelen = snprintf(welcome, sizeof(welcome),
                   "Secure Kilo editor -- version %s", SECURE_KILO_VERSION);
                if (welcomelen > E.screencols) {
                    welcomelen = E.screencols;
                }
                int padding = (E.screencols - welcomelen) / 2;
                if (padding) {
                    abAppend(ab, "~", 1);
                    padding--;
                }
                while (padding--) {
                    abAppend(ab, " ", 1);
                }
                abAppend(ab, welcome, welcomelen);
            } else {
                abAppend(ab, "~", 1);
            }
        } else {
            int len = E.rows[filerow].rsize - E.coloff;
            if (len < 0) {
                len = 0;
            }
            if (len > E.screencols) {
                len = E.screencols;
            }
            char* c = &E.rows[filerow].render[E.coloff];
            unsigned char* hl = &E.rows[filerow].hl[E.coloff];
            int current_color = -1;
            int j;
            for (j = 0 ; j < len ; j++) {
                if (iscntrl(c[j])) {
                    char sym = (c[j] <= 26) ? '@' + c[j] : '?';
                    abAppend(ab, "\x1b[7m", 4);
                    abAppend(ab, &sym, 1);
                    abAppend(ab, "\x1b[m", 3);
                    if (current_color != -1) {
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
                        abAppend(ab, buf, clen);
                    }
                } else if (hl[j] == HL_NORMAL) {
                    if (current_color != -1) {
                        abAppend(ab, "\x1b[39m", 5);
                        current_color = -1;
                    }
                    abAppend(ab, &c[j], 1);
                } else {
                    int color = editorSyntaxToColor(hl[j]);
                    if (color != current_color) {
                        current_color = color;
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
                        abAppend(ab, buf, clen);
                    }
                    abAppend(ab, &c[j], 1);
                }
            }
            abAppend(ab, "\x1b[39m", 5);
        }

        abAppend(ab, "\x1b[K", 3);
        abAppend(ab, "\r\n", 2);
    }
}



void editorDrawStatusBar(struct abuf* ab) {
    abAppend(ab, "\x1b[7m", 4);

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
    abAppend(ab, status, len);

    while (len < E.screencols) {
        if (E.screencols - len == rlen) {
            abAppend(ab, rstatus, rlen);
            break;
        }
        abAppend(ab, " ", 1);
        len++;
    }
    abAppend(ab, "\x1b[m", 3);
    abAppend(ab, "\r\n", 2);
}



void editorDrawMessageBar(struct abuf* ab) {
    abAppend(ab, "\x1b[K", 3);
    int msglen = strlen(E.statusmsg);
    if (msglen > E.screencols) {
        msglen = E.screencols;
    }
    if (msglen && time(NULL) - E.statusmsg_time < 5) {
        abAppend(ab, E.statusmsg, msglen);
    }
}



void editorRefreshScreen() {
    editorScroll();
    struct abuf ab = ABUF_INIT;

    abAppend(&ab, "\x1b[?25l", 6);
    abAppend(&ab, "\x1b[H", 3);

    editorDrawRows(&ab);
    editorDrawStatusBar(&ab);
    editorDrawMessageBar(&ab);

    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1,
                                                (E.rx - E.coloff) + 1);
    abAppend(&ab, buf, strlen(buf));

    abAppend(&ab, "\x1b[?25h", 6);

    write(STDOUT_FILENO, ab.b, ab.len);
    abFree(&ab);

}



void editorSetStatusMessage(const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
    va_end(ap);
    E.statusmsg_time = time(NULL);
}



/************************************ input ************************************/



char* editorPrompt(char* prompt, void (*callback)(char*, int)) {
    size_t bufsize = 128;
    char* buf = malloc(bufsize);

    size_t buflen = 0;
    buf[0] = '\0';

    while (1) {
        editorSetStatusMessage(prompt, buf);
        editorRefreshScreen();

        int c = editorReadKey();
        if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
            if (buflen > 0) {
                buf[--buflen] = '\0';
            }
        } else if (c == '\x1b') {
            editorSetStatusMessage("");
            if (callback) {
                callback(buf, c);
            }
            free(buf);
            return NULL;
        } else if (c == '\r') {
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
                buf = realloc(buf, bufsize);
            }
            buf[buflen++] = c;
            buf[buflen] = '\0';
        }

        if (callback) {
            callback(buf, c);
        }
    }
}



void editorMoveCursor(int key) {
    struct erow* row = (E.cy >= E.numrows) ? NULL : &E.rows[E.cy];
    switch(key) {
        case ARROW_LEFT: {
            if (E.cx != 0) {
                E.cx--;
            } else if (E.cy > 0) {
                E.cy--;
                E.cx = E.rows[E.cy].size;
            }
            break;
        }
        case ARROW_RIGHT: {
            if (row && E.cx < row -> size) {
                E.cx++;                
            } else if (row && E.cx == row->size) {
                E.cy++;
                E.cx = 0;
            }
            break;
        }
        case ARROW_UP: {
            if (E.cy != 0) {
                E.cy--;
            }
            break;
        }
        case ARROW_DOWN: {
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



void editorProcessKeypress() {
    static int quit_times = SECURE_KILO_QUIT_TIMES;

    int c = editorReadKey();

    switch(c) {
        case '\r': {
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

            write(STDOUT_FILENO, "\x1b[2J", 4);
            write(STDOUT_FILENO, "\x1b[H", 3);
            exit(0);
            break;
        }

        case CTRL_KEY('s'): {
            editorSaveSecure();
            break;
        }

        case HOME_KEY: {
            E.cx = 0;
            break;
        }

        case END_KEY: {
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
        case CTRL_KEY('h'):
        case DEL_KEY: {
            if (c == DEL_KEY) {
                editorMoveCursor(ARROW_RIGHT);
            }
            editorDelChar();
            break;
        }

        case PAGE_UP:
        case PAGE_DOWN: {
            if (c == PAGE_UP) {
                E.cy = E.rowoff;
            } else {
                E.cy = E.rowoff + E.screenrows - 1;
                if (E.cy > E.numrows) {
                    E.cy = E.numrows;
                }
            }

            int times = E.screenrows;
            while (times--) {
                editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
            }
            break;
        }

        case ARROW_UP:
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT: {
            editorMoveCursor(c);
            break;
        }

        case CTRL_KEY('l'):
        case '\x1b': {
            break;
        }

        default: {
            editorInsertChar(c);
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

    if (-1 == getWindowSize(&E.screenrows, &E.screencols)) {
        die("getWindowSize");
    }
    E.screenrows -= 2;
}



int main(int argc, char* argv[]) {
    getTTY();
    enableRawMode();
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
