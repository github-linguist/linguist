#include <stdio.h>

int digits[26] = { 0, 0, 100, 500, 0, 0, 0, 0, 1, 1, 0, 50, 1000, 0, 0, 0, 0, 0, 0, 0, 5, 5, 0, 10, 0, 0 };

/* assuming ASCII, do upper case and get index in alphabet. could also be
        inline int VALUE(char x) { return digits [ (~0x20 & x) - 'A' ]; }
   if you think macros are evil */
#define VALUE(x) digits[(~0x20 & (x)) - 'A']

int decode(const char * roman)
{
        const char *bigger;
        int current;
        int arabic = 0;
        while (*roman != '\0') {
                current = VALUE(*roman);
                /*      if (!current) return -1;
                        note: -1 can be used as error code; Romans didn't even have zero
                */
                bigger = roman;

                /* look for a larger digit, like IV or XM */
                while (VALUE(*bigger) <= current && *++bigger != '\0');

                if (*bigger == '\0')
                        arabic += current;
                else {
                        arabic += VALUE(*bigger);
                        while (roman < bigger)
                                arabic -= VALUE(* (roman++) );
                }

                roman ++;
        }
        return arabic;
}

int main()
{
        const char * romans[] = { "MCmxC", "MMVIII", "MDClXVI", "MCXLUJ" };
        int i;

        for (i = 0; i < 4; i++)
                printf("%s\t%d\n", romans[i], decode(romans[i]));

        return 0;
}
