#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

typedef struct twoStringsStruct {
    char * key, *value;
} sTwoStrings;

int ord( char v )
{
    static char *dgts = "012345679";
    char *cp;
    for (cp=dgts; v != *cp; cp++);
    return (cp-dgts);
}

int cmprStrgs(const sTwoStrings *s1,const sTwoStrings *s2)
{
    char *p1 = s1->key;
    char *p2 = s2->key;
    char *mrk1, *mrk2;
    while ((tolower(*p1) == tolower(*p2)) && *p1) { p1++; p2++;}
    if (isdigit(*p1) && isdigit(*p2)) {
        long v1, v2;
        if ((*p1 == '0') ||(*p2 == '0')) {
            while (p1 > s1->key) {
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
    for(p1=s1->key, p2=s2->key; (*p1 == *p2) && *p1; p1++, p2++);
    return (*p1 -*p2);
}

int maxstrlen( char *a, char *b)
{
	int la = strlen(a);
	int lb = strlen(b);
	return (la>lb)? la : lb;
}

int main()
{
    sTwoStrings toBsorted[] = {
        { "Beta11a", "many" },
        { "alpha1", "This" },
        { "Betamax", "sorted." },
        { "beta3", "order" },
        { "beta11a", "strings" },
        { "beta001", "is" },
        { "beta11", "which" },
        { "beta041", "be" },
        { "beta05", "in" },
        { "beta1", "the" },
        { "beta40", "should" },
    };
#define ASIZE (sizeof(toBsorted)/sizeof(sTwoStrings))
    int k, maxlens[ASIZE];
    char format[12];
    sTwoStrings *cp;

    qsort( (void*)toBsorted, ASIZE, sizeof(sTwoStrings),cmprStrgs);

    for (k=0,cp=toBsorted; k < ASIZE; k++,cp++) {
        maxlens[k] = maxstrlen(cp->key, cp->value);
        sprintf(format," %%-%ds", maxlens[k]);
        printf(format, toBsorted[k].value);
	}
    printf("\n");
    for (k=0; k < ASIZE; k++) {
        sprintf(format," %%-%ds", maxlens[k]);
        printf(format, toBsorted[k].key);
	}
    printf("\n");

  return 0;
}
