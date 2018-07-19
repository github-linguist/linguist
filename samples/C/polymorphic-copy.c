#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct object *BaseObj;
typedef struct sclass *Class;
typedef void (*CloneFctn)(BaseObj s, BaseObj clo);
typedef const char * (*SpeakFctn)(BaseObj s);
typedef void (*DestroyFctn)(BaseObj s);

typedef struct sclass {
    size_t csize;		/* size of the class instance */
    const char  *cname;		/* name of the class */
    Class  parent;		/* parent class */

    CloneFctn clone;		/* clone function */
    SpeakFctn speak;		/* speak function */
    DestroyFctn del;		/* delete the object */
} sClass;

typedef struct object {
    Class class;
} SObject;

static
BaseObj obj_copy( BaseObj s, Class c )
{
    BaseObj clo;
    if (c->parent)
        clo = obj_copy( s, c->parent);
    else
        clo = malloc( s->class->csize );

    if (clo)
        c->clone( s, clo );
    return clo;
}

static
void obj_del( BaseObj s, Class c )
{
    if (c->del)
        c->del(s);
    if (c->parent)
        obj_del( s, c->parent);
    else
        free(s);
}

BaseObj ObjClone( BaseObj s )
{ return obj_copy( s, s->class ); }

const char * ObjSpeak( BaseObj s )
{
    return s->class->speak(s);
}

void ObjDestroy( BaseObj s )
{ if (s) obj_del( s, s->class ); }

/* * * * * * */
static
void baseClone( BaseObj s, BaseObj clone)
{
    clone->class = s->class;
}

static
const char *baseSpeak(BaseObj s)
{
    return "Hello, I'm base object";
}

sClass boc = { sizeof(SObject), "BaseObj", NULL,
    &baseClone, &baseSpeak, NULL };
Class BaseObjClass = &boc;

/* * * * * * */
/* Dog - a derived class */

typedef struct sDogPart {
    double weight;
    char color[32];
    char name[24];
} DogPart;

typedef struct sDog *Dog;

struct sDog {
    Class   class;		// parent structure
    DogPart dog;
};

static
void dogClone( BaseObj s, BaseObj c)
{
    Dog src = (Dog)s;
    Dog clone = (Dog)c;
    clone->dog = src->dog;	/* no pointers so strncpys not needed */
}

static
const char *dogSpeak( BaseObj s)
{
    Dog d = (Dog)s;
    static char  response[90];
    sprintf(response, "woof! woof! My name is %s. I'm a %s %s",
            d->dog.name, d->dog.color, d->class->cname);
    return response;
}


sClass dogc = { sizeof(struct sDog), "Dog", &boc,
    &dogClone, &dogSpeak, NULL };
Class DogClass = &dogc;

BaseObj NewDog( const char *name, const char *color, double weight )
{
    Dog dog = malloc(DogClass->csize);
    if (dog) {
        DogPart *dogp = &dog->dog;
        dog->class = DogClass;
        dogp->weight = weight;
        strncpy(dogp->name, name, 23);
        strncpy(dogp->color, color, 31);
    }
    return (BaseObj)dog;
}

/* * * * * * * * * */
/* Ferret - a derived class */

typedef struct sFerretPart {
    char color[32];
    char name[24];
    int  age;
} FerretPart;

typedef struct sFerret *Ferret;

struct sFerret {
    Class   class;		// parent structure
    FerretPart ferret;
};

static
void ferretClone( BaseObj s, BaseObj c)
{
    Ferret src = (Ferret)s;
    Ferret clone = (Ferret)c;
    clone->ferret = src->ferret;  /* no pointers so strncpys not needed */
}

static
const char *ferretSpeak(BaseObj s)
{
    Ferret f = (Ferret)s;
    static char  response[90];
    sprintf(response, "My name is %s. I'm a %d mo. old %s wiley %s",
            f->ferret.name, f->ferret.age, f->ferret.color,
            f->class->cname);
    return response;
}

sClass ferretc = { sizeof(struct sFerret), "Ferret", &boc,
    &ferretClone, &ferretSpeak, NULL };
Class FerretClass = &ferretc;

BaseObj NewFerret( const char *name, const char *color, int age )
{
    Ferret ferret = malloc(FerretClass->csize);
    if (ferret) {
        FerretPart *ferretp = &(ferret->ferret);
        ferret->class = FerretClass;
        strncpy(ferretp->name, name, 23);
        strncpy(ferretp->color, color, 31);
        ferretp->age = age;
    }
    return (BaseObj)ferret;
}

/* * Now you really understand why Bjarne created C++ * */

int main()
{
    BaseObj  o1;
    BaseObj  kara = NewFerret( "Kara", "grey", 15 );
    BaseObj  bruce = NewDog("Bruce", "yellow", 85.0 );
    printf("Ok created things\n");

    o1 = ObjClone(kara );
    printf("Karol says %s\n", ObjSpeak(o1));
    printf("Kara says %s\n", ObjSpeak(kara));
    ObjDestroy(o1);

    o1 = ObjClone(bruce );
    strncpy(((Dog)o1)->dog.name, "Donald", 23);
    printf("Don says %s\n", ObjSpeak(o1));
    printf("Bruce says %s\n", ObjSpeak(bruce));
    ObjDestroy(o1);
    return 0;
}
