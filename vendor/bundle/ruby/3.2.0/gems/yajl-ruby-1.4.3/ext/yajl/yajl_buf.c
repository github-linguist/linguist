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

#include "yajl_buf.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define YAJL_BUF_INIT_SIZE 2048

struct yajl_buf_t {
    yajl_buf_state state;
    unsigned int len;
    unsigned int used;
    unsigned char * data;
    yajl_alloc_funcs * alloc;
};

static void *noop_realloc(void *ctx, void *ptr, unsigned int sz) {
    fprintf(stderr, "Attempt to allocate on invalid yajl_buf_t\n");
    abort();
}
static void *noop_malloc(void *ctx, unsigned int sz) { return noop_realloc(ctx, NULL, sz); }
static void noop_free(void *ctx, void *ptr) { }

static yajl_alloc_funcs noop_allocs = {
    .malloc = &noop_malloc,
    .realloc = &noop_realloc,
    .free = &noop_free,
};

// A buffer to be returned if the initial allocation fails
static struct yajl_buf_t buf_alloc_error = {
    .state = yajl_buf_alloc_failed,
    .alloc = &noop_allocs
};

#include <stdio.h>

yajl_buf_state yajl_buf_err(yajl_buf buf)
{
    assert(buf);
    return buf->state;
}

static
yajl_buf_state yajl_buf_set_error(yajl_buf buf, yajl_buf_state err)
{
    buf->state = err;

    // free and clear all data from the buffer
    YA_FREE(buf->alloc, buf->data);
    buf->len = 0;
    buf->data = 0;
    buf->used = 0;

    return err;
}

static
yajl_buf_state yajl_buf_ensure_available(yajl_buf buf, unsigned int want)
{
    unsigned int need;
    
    assert(buf != NULL);

    if (buf->state != yajl_buf_ok) {
        return buf->state;
    }

    /* first call */
    if (buf->data == NULL) {
        buf->len = YAJL_BUF_INIT_SIZE;
        buf->data = (unsigned char *) YA_MALLOC(buf->alloc, buf->len);
        if (buf->data == NULL)  {
            return yajl_buf_set_error(buf, yajl_buf_overflow);
        }

        buf->data[0] = 0;
    }

    if (want == 0) {
        return yajl_buf_ok;
    }

    need = buf->len;

    while (want >= (need - buf->used) && need > 0) need <<= 1;

    // Check for overflow
    if (need < buf->used || need == 0) {
        return yajl_buf_set_error(buf, yajl_buf_overflow);
    }

    if (need != buf->len) {
        buf->data = (unsigned char *) YA_REALLOC(buf->alloc, buf->data, need);

        if (buf->data == NULL)  {
            return yajl_buf_set_error(buf, yajl_buf_overflow);
        }

        buf->len = need;
    }

    return yajl_buf_ok;
}

yajl_buf yajl_buf_alloc(yajl_alloc_funcs * alloc)
{
    yajl_buf b = YA_MALLOC(alloc, sizeof(struct yajl_buf_t));
    if (b == NULL) {
        return &buf_alloc_error;
    }

    memset((void *) b, 0, sizeof(struct yajl_buf_t));
    b->alloc = alloc;
    return b;
}

void yajl_buf_free(yajl_buf buf)
{
    assert(buf != NULL);
    if (buf->data) YA_FREE(buf->alloc, buf->data);
    YA_FREE(buf->alloc, buf);
}

void yajl_buf_append(yajl_buf buf, const void * data, unsigned int len)
{
    if (yajl_buf_ensure_available(buf, len)) {
        return;
    }
    if (len > 0) {
        assert(data != NULL);
        memcpy(buf->data + buf->used, data, len);
        buf->used += len;
        buf->data[buf->used] = 0;
    }
}

void yajl_buf_clear(yajl_buf buf)
{
    assert(buf);
    assert(!yajl_buf_err(buf));
    buf->used = 0;
    if (buf->data) buf->data[buf->used] = 0;
}

const unsigned char * yajl_buf_data(yajl_buf buf)
{
    assert(buf);
    assert(!yajl_buf_err(buf));
    return buf->data;
}

unsigned int yajl_buf_len(yajl_buf buf)
{
    assert(buf);
    assert(!yajl_buf_err(buf));
    return buf->used;
}

void
yajl_buf_truncate(yajl_buf buf, unsigned int len)
{
    assert(buf);
    assert(!yajl_buf_err(buf));
    assert(len <= buf->used);
    buf->used = len;
}
