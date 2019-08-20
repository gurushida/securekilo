sk: sk.c
	$(CC) -D_XOPEN_SOURCE_EXTENDED -lncurses sk.c -o sk -Wall -Wextra -pedantic -std=c99
