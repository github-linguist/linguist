#include <array.h>

unsigned __bump_up(unsigned n) {
    unsigned base = 1;
    --n;
    while (base < sizeof n * 8) {
        n |= n >> base;
        base *= 2;
    }
    ++n;
    n += (n == 0);
    return n;
}

void *__array_alloc(size_t size, unsigned length) {
    unsigned allocated = __bump_up(length);
    struct __array_header *head = malloc(sizeof *head + allocated * size);
    assert(head);
    head->length = length;
    head->allocated = allocated;
    return (void *) (head + 1);
}

void __array_resize(void **array, size_t size, int difference) {
    if (difference == 0) {
        return;
    }
    struct __array_header *head = __header(*array);
    head->length += difference;
    if (head->length >= head->allocated) {
        head->allocated = __bump_up(head->length);
        head = realloc(head, sizeof *head + head->allocated * size);
        assert(head);
        *array = head + 1;
    }
}

int __array_search(void *array, void *elem, size_t size) {
    for (unsigned i = 0; i < alength(array) * size; i += size) {
        if (memcmp((char *)array + i, elem, size) == 0) {
            return 1;
        }
    }
    return 0;
}
