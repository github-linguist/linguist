/*
 * Copyright 2010, Lloyd Hilaiel.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 * 
 *  3. Neither the name of Lloyd Hilaiel nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */ 

/*
 * A header only implementation of a simple stack of bytes, used in YAJL
 * to maintain parse state.
 */

#ifndef __YAJL_BYTESTACK_H__
#define __YAJL_BYTESTACK_H__

#include <limits.h>
#include <assert.h>
#include "api/yajl_common.h"

#define YAJL_BS_INC 128
#define YAJL_BS_MAX_SIZE UINT_MAX

typedef struct yajl_bytestack_t
{
    unsigned char * stack;
    unsigned int size;
    unsigned int used;
    yajl_alloc_funcs * yaf;
} yajl_bytestack;

/* initialize a bytestack */
#define yajl_bs_init(obs, _yaf) {               \
        (obs).stack = NULL;                     \
        (obs).size = 0;                         \
        (obs).used = 0;                         \
        (obs).yaf = (_yaf);                     \
    }                                           \


/* initialize a bytestack */
#define yajl_bs_free(obs)                 \
    if ((obs).stack) (obs).yaf->free((obs).yaf->ctx, (obs).stack);   

#define yajl_bs_current(obs)               \
    (assert((obs).used > 0), (obs).stack[(obs).used - 1])

/* 0: success, 1: error */
static inline YAJL_WARN_UNUSED
int yajl_bs_push_inline(yajl_bytestack *obs, unsigned char byte) {
    if ((obs->size - obs->used) == 0) {
        if (obs->size > YAJL_BS_MAX_SIZE - YAJL_BS_INC)
            return 1;
        obs->size += YAJL_BS_INC;
        obs->stack = obs->yaf->realloc(obs->yaf->ctx, (void *)obs->stack, obs->size);
        if (!obs->stack)
            return 1;
    }
    obs->stack[obs->used++] = byte;
    return 0;
}

#define yajl_bs_push(obs, byte) yajl_bs_push_inline(&(obs), (byte))

/* removes the top item of the stack, returns nothing */
#define yajl_bs_pop(obs) { ((obs).used)--; }

static inline
void
yajl_bs_set_inline(yajl_bytestack *obs, unsigned char byte) {
    assert(obs->used > 0);
    assert(obs->size >= obs->used);
    obs->stack[obs->used - 1] = byte;
}

#define yajl_bs_set(obs, byte) yajl_bs_set_inline(&obs, byte)

#endif
