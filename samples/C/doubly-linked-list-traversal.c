// A doubly linked list of strings;
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct sListEntry {
    const char *value;
    struct sListEntry *next;
    struct sListEntry *prev;
} *ListEntry, *LinkedList;

typedef struct sListIterator{
    ListEntry  link;
    LinkedList head;
} *LIterator;

LinkedList NewList() {
    ListEntry le = malloc(sizeof(struct sListEntry));
    if (le) {
        le->value = NULL;
        le->next = le->prev = NULL;
    }
    return le;
}

int LL_Append(LinkedList ll, const char *newVal)
{
    ListEntry le = malloc(sizeof(struct sListEntry));
    if (le) {
        le->value = strdup(newVal);
        le->prev = ll->prev;
        le->next = NULL;
        if (le->prev)
            le->prev->next = le;
        else
            ll->next = le;
        ll->prev = le;
    }
    return (le!= NULL);
}

int LI_Insert(LIterator iter, const char *newVal)
{
    ListEntry crnt = iter->link;
    ListEntry le = malloc(sizeof(struct sListEntry));
    if (le) {
        le->value = strdup(newVal);
        if ( crnt == iter->head) {
            le->prev = NULL;
            le->next = crnt->next;
            crnt->next = le;
            if (le->next)
                le->next->prev = le;
            else
                crnt->prev = le;
        }
        else {
            le->prev = ( crnt == NULL)? iter->head->prev : crnt->prev;
            le->next = crnt;
            if (le->prev)
                le->prev->next = le;
            else
                iter->head->next = le;
            if (crnt)
                crnt->prev = le;
            else
                iter->head->prev = le;
        }
    }
    return (le!= NULL);
}

LIterator LL_GetIterator(LinkedList ll )
{
    LIterator liter = malloc(sizeof(struct sListIterator));
    liter->head = ll;
    liter->link = ll;
    return liter;
}

#define LLI_Delete( iter ) \
    {free(iter); \
    iter = NULL;}

int LLI_AtEnd(LIterator iter)
{
    return iter->link == NULL;
}
const char *LLI_Value(LIterator iter)
{
    return (iter->link)? iter->link->value: NULL;
}
int LLI_Next(LIterator iter)
{
    if (iter->link) iter->link = iter->link->next;
    return(iter->link != NULL);
}
int LLI_Prev(LIterator iter)
{
    if (iter->link) iter->link = iter->link->prev;
    return(iter->link != NULL);
}

int main()
{
    static const char *contents[] = {"Read", "Orage", "Yeller",
                                     "Glean", "Blew", "Burple"};
    int ix;
    LinkedList ll = NewList();    //new linked list
    LIterator iter;

    for (ix=0; ix<6; ix++)        //insert contents
        LL_Append(ll, contents[ix]);

    iter = LL_GetIterator(ll);    //get an iterator
    printf("forward\n");
    while(LLI_Next(iter))         //iterate forward
        printf("value=%s\n", LLI_Value(iter));
    LLI_Delete(iter);             //delete iterator

    printf("\nreverse\n");
    iter = LL_GetIterator(ll);
    while(LLI_Prev(iter))         //iterate reverse
        printf("value=%s\n", LLI_Value(iter));
    LLI_Delete(iter);
                        //uhhh-- delete list??
    return 0;
}
