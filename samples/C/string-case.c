/* Demonstrate toupper and tolower for
   standard C strings.
   This does not work for multibyte character sets. */
#include <ctype.h>
#include <stdio.h>

/* upper-cases s in place */
void str_toupper(char *s)
{
    while(*s)
    {
        *s=toupper(*s);
        s++;
    }
}


/* lower-cases s in place */
void str_tolower(char *s)
{
    while(*s)
    {
        *s=tolower(*s);
        s++;
    }
}

int main(int argc, char *argv[])
{
    char t[255]="alphaBETA";
    str_toupper(t);
    printf("uppercase: %s\n", t);
    str_tolower(t);
    printf("lowercase: %s\n", t);
    return 0;
}
