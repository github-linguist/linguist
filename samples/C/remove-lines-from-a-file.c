#include <stdio.h>
#include <stdlib.h> /* for atoi() and malloc() */
#include <string.h> /* for memmove() */

/* Conveniently print to standard error and exit nonzero. */
#define ERROR(fmt, arg) return fprintf(stderr, fmt "\n", arg)

int main(int argc, char **argv)
{
    FILE *fp;
    char *buf;
    size_t sz;
    int start, count, lines = 1;
    int dest = 0, src = 0, pos = -1;

    /* Initialization and sanity checks */
    if (argc != 4)
        ERROR("Usage: %s <file> <start> <count>", argv[0]);

    if ((count = atoi(argv[3])) < 1) /* We're a no-op. */
        return 0;

    if ((start = atoi(argv[2])) < 1)
        ERROR("Error: <start> (%d) must be positive", start);

    if ((fp = fopen(argv[1], "r")) == NULL)
        ERROR("No such file: %s", argv[1]);

    /* Determine filesize and allocate a buffer accordingly. */
    fseek(fp, 0, SEEK_END);
    sz = ftell(fp);
    buf = malloc(sz + 1);
    rewind(fp);

    /* Fill the buffer, count lines, and remember a few important offsets. */
    while ((buf[++pos] = fgetc(fp)) != EOF) {
        if (buf[pos] == '\n') {
            ++lines;
            if (lines == start) dest = pos + 1;
            if (lines == start + count) src = pos + 1;
        }
    }

    /* We've been asked to do something weird; clean up and bail. */
    if (start + count > lines) {
        free(buf);
        fclose(fp);
        ERROR("Error: invalid parameters for file with %d lines", --lines);
    }

    /* Overwrite the lines to be deleted with the survivors. */
    memmove(buf + dest, buf + src, pos - src);

    /* Reopen the file and write back just enough of the buffer. */
    freopen(argv[1], "w", fp);
    fwrite(buf, pos - src + dest, 1, fp);

    free(buf);
    fclose(fp);
    return 0;
}
