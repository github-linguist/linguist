#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

typedef const char * String;
typedef struct sTable {
    String * *rows;
    int      n_rows,n_cols;
} *Table;

typedef int (*CompareFctn)(String a, String b);

struct {
   CompareFctn  compare;
   int   column;
   int   reversed;
} sortSpec;

int CmprRows( const void *aa, const void *bb)
{
   String *rA = *(String *const *)aa;
   String *rB = *(String *const *)bb;
   int sortCol = sortSpec.column;

   String left = sortSpec.reversed ? rB[sortCol] : rA[sortCol];
   String right = sortSpec.reversed ? rA[sortCol] : rB[sortCol];
   return sortSpec.compare( left, right );
}

/** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * tbl parameter is a table of rows of strings
 * argSpec is a string containing zero or more of the letters o,c,r
 * if o is present - the corresponding optional argument is a function which
 *      determines the ordering of the strings.
 * if c is present - the corresponding optional argument is an integer that
 *      specifies the column to sort on.
 * if r is present - the corresponding optional argument is either
 *      true(nonzero) or false(zero) and if true, the sort will b in reverse order
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int sortTable(Table tbl, const char* argSpec,... )
{
   va_list vl;
   const char *p;
   int c;
   sortSpec.compare = &strcmp;
   sortSpec.column = 0;
   sortSpec.reversed = 0;

   va_start(vl, argSpec);
   if (argSpec)
      for (p=argSpec; *p; p++) {
         switch (*p) {
         case 'o':
            sortSpec.compare = va_arg(vl,CompareFctn);
            break;
         case 'c':
            c = va_arg(vl,int);
            if ( 0<=c && c<tbl->n_cols)
               sortSpec.column  = c;
            break;
         case 'r':
            sortSpec.reversed = (0!=va_arg(vl,int));
            break;
         }
      }
   va_end(vl);
   qsort( tbl->rows, tbl->n_rows, sizeof(String *), CmprRows);
   return 0;
}

void printTable( Table tbl, FILE *fout, const char *colFmts[])
{
   int row, col;

   for (row=0; row<tbl->n_rows; row++) {
      fprintf(fout, "   ");
      for(col=0; col<tbl->n_cols; col++) {
         fprintf(fout, colFmts[col], tbl->rows[row][col]);
      }
      fprintf(fout, "\n");
   }
   fprintf(fout, "\n");
}

int ord(char v)
{
    return v-'0';
}

/* an alternative comparison function */
int cmprStrgs(String s1, String s2)
{
    const char *p1 = s1;
    const char *p2 = s2;
    const char *mrk1, *mrk2;
    while ((tolower(*p1) == tolower(*p2)) && *p1) {
       p1++; p2++;
    }
    if (isdigit(*p1) && isdigit(*p2)) {
        long v1, v2;
        if ((*p1 == '0') ||(*p2 == '0')) {
            while (p1 > s1) {
                p1--; p2--;
                if (*p1 != '0') break;
            }
            if (!isdigit(*p1)) {
                p1++; p2++;
            }
        }
        mrk1 = p1; mrk2 = p2;
        v1 = 0;
        while(isdigit(*p1)) {
            v1 = 10*v1+ord(*p1);
            p1++;
        }
        v2 = 0;
        while(isdigit(*p2)) {
            v2 = 10*v2+ord(*p2);
            p2++;
        }
        if (v1 == v2)
           return(p2-mrk2)-(p1-mrk1);
        return v1 - v2;
    }
    if (tolower(*p1) != tolower(*p2))
       return (tolower(*p1) - tolower(*p2));
    for(p1=s1, p2=s2; (*p1 == *p2) && *p1; p1++, p2++);
    return (*p1 -*p2);
}

int main()
{
   const char *colFmts[] = {" %-5.5s"," %-5.5s"," %-9.9s"};
   String r1[] = { "a101", "red",  "Java" };
   String r2[] = { "ab40", "gren", "Smalltalk" };
   String r3[] = { "ab9",  "blue", "Fortran" };
   String r4[] = { "ab09", "ylow", "Python" };
   String r5[] = { "ab1a", "blak", "Factor" };
   String r6[] = { "ab1b", "brwn", "C Sharp" };
   String r7[] = { "Ab1b", "pink", "Ruby" };
   String r8[] = { "ab1",  "orng", "Scheme" };

   String *rows[] = { r1, r2, r3, r4, r5, r6, r7, r8 };
   struct sTable table;
   table.rows = rows;
   table.n_rows = 8;
   table.n_cols = 3;

   sortTable(&table, "");
   printf("sort on col 0, ascending\n");
   printTable(&table, stdout, colFmts);

   sortTable(&table, "ro", 1, &cmprStrgs);
   printf("sort on col 0, reverse.special\n");
   printTable(&table, stdout, colFmts);

   sortTable(&table, "c", 1);
   printf("sort on col 1, ascending\n");
   printTable(&table, stdout, colFmts);

   sortTable(&table, "cr", 2, 1);
   printf("sort on col 2, reverse\n");
   printTable(&table, stdout, colFmts);
   return 0;
}
