#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef const char * (*Responder)( int p1);

typedef struct sDelegate {
    Responder operation;
} *Delegate;

/* Delegate class constructor */
Delegate NewDelegate( Responder rspndr )
{
    Delegate dl = malloc(sizeof(struct sDelegate));
    dl->operation = rspndr;
    return dl;
}

/* Thing method of Delegate */
const char *DelegateThing(Delegate dl, int p1)
{
    return  (dl->operation)? (*dl->operation)(p1) : NULL;
}

/** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
typedef struct sDelegator {
    int     param;
    char    *phrase;
    Delegate delegate;
} *Delegator;

const char * defaultResponse( int p1)
{
    return "default implementation";
}

static struct sDelegate defaultDel = { &defaultResponse };

/* Delegator class constructor */
Delegator NewDelegator( int p, char *phrase)
{
    Delegator d  = malloc(sizeof(struct sDelegator));
    d->param = p;
    d->phrase = phrase;
    d->delegate = &defaultDel;	/* default delegate */
    return d;
}

/* Operation method of Delegator */
const char *Delegator_Operation( Delegator theDelegator, int p1, Delegate delroy)
{
    const char *rtn;
    if (delroy) {
        rtn = DelegateThing(delroy, p1);
        if (!rtn) {			/* delegate didn't handle 'thing' */
            rtn = DelegateThing(theDelegator->delegate, p1);
        }
    }
    else 		/* no delegate */
        rtn = DelegateThing(theDelegator->delegate, p1);

    printf("%s\n", theDelegator->phrase );
    return rtn;
}

/** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
const char *thing1( int p1)
{
    printf("We're in thing1 with value %d\n" , p1);
    return "delegate implementation";
}

int main()
{
    Delegate del1 = NewDelegate(&thing1);
    Delegate del2 = NewDelegate(NULL);
    Delegator theDelegator = NewDelegator( 14, "A stellar vista, Baby.");

    printf("Delegator returns %s\n\n",
            Delegator_Operation( theDelegator, 3, NULL));
    printf("Delegator returns %s\n\n",
            Delegator_Operation( theDelegator, 3, del1));
    printf("Delegator returns %s\n\n",
            Delegator_Operation( theDelegator, 3, del2));
    return 0;
}
