#include <stdio.h>
#include <stdarg.h>

void varstrings(int count, ...)   /* the ellipsis indicates variable arguments */
{
    va_list args;
    va_start(args, count);
    while (count--)
        puts(va_arg(args, const char *));
    va_end(args);
}

varstrings(5, "Mary", "had", "a", "little", "lamb");
