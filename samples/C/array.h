#ifndef ARRAY_H
#define ARRAY_H value

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define array(type, name, initial_length) \
    type *name = __array_alloc(sizeof(type), initial_length)

#define aforeach(it, array) \
    for (unsigned it = 0; \
            it < alength(array); \
            ++it)

#define __header(array) \
    ((struct __array_header *) array - 1)

#define alength(array) \
    (__header(array)->length)

#define afree(array) \
    free(__header(array))

#define apush(array, elem) \
    __array_resize((void **) &array, sizeof *array, 1); \
    array[alength(array)-1] = elem

#define apop(array) \
    aremove(array, (alength(array) - 1))

#define aremove(array, index) \
    assert(alength(array) > index); \
    memmove(array + index, array + index + 1, sizeof *array * (alength(array) - index - 1)); \
    __array_resize((void **) &array, sizeof *array, -1)

#define ainsert(array, index, elem) \
    __array_resize((void **) &array, sizeof *array, index >= alength(array) ? index - alength(array) + 1 : 1); \
    memmove(array + index + 1, array + index, sizeof *array * (alength(array) - index - 1)); \
    array[index] = elem

#define acontains(array, elem) \
    __array_search(array, &elem, sizeof elem)

#define __arrayallocated(array) \
    (__header(array)->allocated)

struct __array_header {
    unsigned length;
    unsigned allocated;
};

unsigned __bump_up(unsigned n);
void *__array_alloc(size_t size, unsigned length);
void __array_resize(void **array, size_t size, int difference);
int __array_search(void *array, void *elem, size_t size);

#endif /* ifndef ARRAY_H */
