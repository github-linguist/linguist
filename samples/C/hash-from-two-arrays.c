#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define KeyType const char *
#define ValType int

#define HASH_SIZE 4096

// hash function useful when KeyType is char * (string)
unsigned strhashkey( const char * key, int max)
{
    unsigned h=0;
    unsigned hl, hr;

    while(*key) {
        h += *key;
        hl= 0x5C5 ^ (h&0xfff00000 )>>18;
        hr =(h&0x000fffff );
        h = hl ^ hr ^ *key++;
    }
    return h % max;
}

typedef struct sHme {
    KeyType    key;
    ValType    value;
    struct sHme  *link;
} *MapEntry;

typedef struct he {
    MapEntry  first, last;
} HashElement;

HashElement hash[HASH_SIZE];

typedef void (*KeyCopyF)(KeyType *kdest, KeyType ksrc);
typedef void (*ValCopyF)(ValType *vdest, ValType vsrc);
typedef unsigned (*KeyHashF)( KeyType key, int upperBound );
typedef int (*KeyCmprF)(KeyType key1, KeyType key2);

void HashAddH( KeyType key, ValType value,
        KeyCopyF copyKey, ValCopyF copyVal, KeyHashF hashKey, KeyCmprF keySame )
{
    unsigned hix = (*hashKey)(key, HASH_SIZE);
    MapEntry m_ent;

    for (m_ent= hash[hix].first;
            m_ent && !(*keySame)(m_ent->key,key); m_ent=m_ent->link);
    if (m_ent) {
        (*copyVal)(&m_ent->value, value);
    }
    else {
        MapEntry last;
        MapEntry hme = malloc(sizeof(struct sHme));
        (*copyKey)(&hme->key, key);
        (*copyVal)(&hme->value, value);
        hme->link = NULL;
        last = hash[hix].last;
        if (last) {
//	    printf("Dup. hash key\n");
            last->link = hme;
        }
        else
            hash[hix].first = hme;
        hash[hix].last = hme;
    }
}

int HashGetH(ValType *val, KeyType key, KeyHashF hashKey, KeyCmprF keySame )
{
    unsigned hix = (*hashKey)(key, HASH_SIZE);
    MapEntry m_ent;
    for (m_ent= hash[hix].first;
            m_ent && !(*keySame)(m_ent->key,key); m_ent=m_ent->link);
    if (m_ent) {
        *val = m_ent->value;
    }
    return (m_ent != NULL);
}

void copyStr(const char**dest, const char *src)
{
    *dest = strdup(src);
}
void copyInt( int *dest, int src)
{
    *dest = src;
}
int strCompare( const char *key1, const char *key2)
{
    return strcmp(key1, key2) == 0;
}

void HashAdd( KeyType key, ValType value )
{
    HashAddH( key, value, &copyStr, &copyInt, &strhashkey, &strCompare);
}

int HashGet(ValType *val, KeyType key)
{
    return HashGetH( val, key, &strhashkey, &strCompare);
}

int main()
{
    static const char * keyList[] = {"red","orange","yellow","green", "blue", "violet" };
    static int valuList[] = {1,43,640, 747, 42, 42};
    int ix;

    for (ix=0; ix<6; ix++) {
        HashAdd(keyList[ix], valuList[ix]);
    }
    return 0;
}
