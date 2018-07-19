#define _POSIX_SOURCE
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stddef.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
struct functionInfo {
    char* name;
    int timesCalled;
    char marked;
};
void addToList(struct functionInfo** list, struct functionInfo toAdd, \
               size_t* numElements, size_t* allocatedSize)
{
    static const char* keywords[32] = {"auto", "break", "case", "char", "const", \
                                       "continue", "default", "do", "double", \
                                       "else", "enum", "extern", "float", "for", \
                                       "goto", "if", "int", "long", "register", \
                                       "return", "short", "signed", "sizeof", \
                                       "static", "struct", "switch", "typedef", \
                                       "union", "unsigned", "void", "volatile", \
                                       "while"
                                      };
    int i;
    /* If the "function" being called is actually a keyword, then ignore it */
    for (i = 0; i < 32; i++) {
        if (!strcmp(toAdd.name, keywords[i])) {
            return;
        }
    }
    if (!*list) {
        *allocatedSize = 10;
        *list = calloc(*allocatedSize, sizeof(struct functionInfo));
        if (!*list) {
            printf("Failed to allocate %lu elements of %lu bytes each.\n", \
                   *allocatedSize, sizeof(struct functionInfo));
            abort();
        }
        (*list)[0].name = malloc(strlen(toAdd.name)+1);
        if (!(*list)[0].name) {
            printf("Failed to allocate %lu bytes.\n", strlen(toAdd.name)+1);
            abort();
        }
        strcpy((*list)[0].name, toAdd.name);
        (*list)[0].timesCalled = 1;
        (*list)[0].marked = 0;
        *numElements = 1;
    } else {
        char found = 0;
        unsigned int i;
        for (i = 0; i < *numElements; i++) {
            if (!strcmp((*list)[i].name, toAdd.name)) {
                found = 1;
                (*list)[i].timesCalled++;
                break;
            }
        }
        if (!found) {
            struct functionInfo* newList = calloc((*allocatedSize)+10, \
                                                  sizeof(struct functionInfo));
            if (!newList) {
                printf("Failed to allocate %lu elements of %lu bytes each.\n", \
                       (*allocatedSize)+10, sizeof(struct functionInfo));
                abort();
            }
            memcpy(newList, *list, (*allocatedSize)*sizeof(struct functionInfo));
            free(*list);
            *allocatedSize += 10;
            *list = newList;
            (*list)[*numElements].name = malloc(strlen(toAdd.name)+1);
            if (!(*list)[*numElements].name) {
                printf("Failed to allocate %lu bytes.\n", strlen(toAdd.name)+1);
                abort();
            }
            strcpy((*list)[*numElements].name, toAdd.name);
            (*list)[*numElements].timesCalled = 1;
            (*list)[*numElements].marked = 0;
            (*numElements)++;
        }
    }
}
void printList(struct functionInfo** list, size_t numElements)
{
    char maxSet = 0;
    unsigned int i;
    size_t maxIndex = 0;
    for (i = 0; i<10; i++) {
        maxSet = 0;
        size_t j;
        for (j = 0; j<numElements; j++) {
            if (!maxSet || (*list)[j].timesCalled > (*list)[maxIndex].timesCalled) {
                if (!(*list)[j].marked) {
                    maxSet = 1;
                    maxIndex = j;
                }
            }
        }
        (*list)[maxIndex].marked = 1;
        printf("%s() called %d times.\n", (*list)[maxIndex].name, \
               (*list)[maxIndex].timesCalled);
    }
}
void freeList(struct functionInfo** list, size_t numElements)
{
    size_t i;
    for (i = 0; i<numElements; i++) {
        free((*list)[i].name);
    }
    free(*list);
}
char* extractFunctionName(char* readHead)
{
    char* identifier = readHead;
    if (isalpha(*identifier) || *identifier == '_') {
        while (isalnum(*identifier) || *identifier == '_') {
            identifier++;
        }
    }
    /* Search forward for spaces and then an open parenthesis
     * but do not include this in the function name.
     */
    char* toParen = identifier;
    if (toParen == readHead) return NULL;
    while (isspace(*toParen)) {
        toParen++;
    }
    if (*toParen != '(') return NULL;
    /* Copy the found function name to the output string */
    ptrdiff_t size = (ptrdiff_t)((ptrdiff_t)identifier) \
                     - ((ptrdiff_t)readHead)+1;
    char* const name = malloc(size);
    if (!name) {
        printf("Failed to allocate %lu bytes.\n", size);
        abort();
    }
    name[size-1] = '\0';
    memcpy(name, readHead, size-1);
    /* Function names can't be blank */
    if (strcmp(name, "")) {
        return name;
    }
    free(name);
    return NULL;
}
int main(int argc, char** argv)
{
    int i;
    for (i = 1; i<argc; i++) {
        errno = 0;
        FILE* file = fopen(argv[i], "r");
        if (errno || !file) {
            printf("fopen() failed with error code \"%s\"\n", \
                   strerror(errno));
            abort();
        }
        char comment = 0;
#define DOUBLEQUOTE 1
#define SINGLEQUOTE 2
        int string = 0;
        struct functionInfo* functions = NULL;
        struct functionInfo toAdd;
        size_t numElements = 0;
        size_t allocatedSize = 0;
        struct stat metaData;
        errno = 0;
        if (fstat(fileno(file), &metaData) < 0) {
            printf("fstat() returned error \"%s\"\n", strerror(errno));
            abort();
        }
        char* mmappedSource = (char*)mmap(NULL, metaData.st_size, PROT_READ, \
                                          MAP_PRIVATE, fileno(file), 0);
        if (errno) {
            printf("mmap() failed with error \"%s\"\n", strerror(errno));
            abort();
        }
        if (!mmappedSource) {
            printf("mmap() returned NULL.\n");
            abort();
        }
        char* readHead = mmappedSource;
        while (readHead < mmappedSource + metaData.st_size) {
            while (*readHead) {
                /* Ignore comments inside strings */
                if (!string) {
                    if (*readHead == '/' && !strncmp(readHead, "/*", 2)) {
                        comment = 1;
                    }
                    if (*readHead == '*' && !strncmp(readHead, "*/", 2)) {
                        comment = 0;
                    }
                }
                /* Ignore strings inside comments */
                if (!comment) {
                    if (*readHead == '"') {
                        if (!string) {
                            string = DOUBLEQUOTE;
                        } else if (string == DOUBLEQUOTE) {
                            /* Only toggle string mode if the quote character
                             * is not escaped
                             */
                            if (strncmp((readHead-1), "\\\"", 2)) {
                                string = 0;
                            }
                        }
                    }
                    if (*readHead == '\'') {
                        if (!string) {
                            string = SINGLEQUOTE;
                        } else if (string == SINGLEQUOTE) {
                            if (strncmp((readHead-1), "\\\'", 2)) {
                                string = 0;
                            }
                        }
                    }
                }
                /* Look for identifiers outside of any comment or string */
                if (!comment && !string) {
                    char* name = extractFunctionName(readHead);
                    /* Don't read part of an identifier on the next iteration */
                    if (name) {
                        toAdd.name = name;
                        addToList(&functions, toAdd, &numElements, &allocatedSize);
                        readHead += strlen(name);
                    }
                    free(name);
                }
                readHead++;
            }
        }
        errno = 0;
        munmap(mmappedSource, metaData.st_size);
        if (errno) {
            printf("munmap() returned error \"%s\"\n", strerror(errno));
            abort();
        }
        errno = 0;
        fclose(file);
        if (errno) {
            printf("fclose() returned error \"%s\"\n", strerror(errno));
            abort();
        }
        printList(&functions, numElements);
        freeList(&functions, numElements);
    }
    return 0;
}
