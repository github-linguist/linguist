#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    // Get a string from stdin
    char str[BUFSIZ];
    puts("Enter a string: ");
    fgets(str, sizeof(str), stdin);

    // Get 75000 from stdin
    long num;
    char buf[BUFSIZ];
    do
    {
        puts("Enter 75000: ");
        fgets(buf, sizeof(buf), stdin);
        num = strtol(buf, NULL, 10);
    } while (num != 75000);

    return EXIT_SUCCESS;
}
