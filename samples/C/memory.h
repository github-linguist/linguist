/*
memory management definitions
Copyright (C) 2017  Peter Elliott

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef _MEMORY_H
#define _MEMORY_H

#include <stdint.h>
#include <stddef.h>

#define MALLOC_MAG 0x8a05e5ad623cc4e
#define MIN_SPLIT_SIZE 128 //must be larger than 40 or sizeof(struct block)+8

struct block {
    struct block *prev;
    struct block *next;
    uint64_t pos_mag; // position dependant magic number
    uint32_t len;     // length of the block
    uint32_t used;    // this could be one byte but we need to preserve 8 byte alignment
};

extern struct block *base; // the tail of the blocks linked list

struct block *push_block(size_t size);
int shrink_block(struct block *mblock, size_t size);
struct block *get_free_block(size_t size);
void block_merge_next(struct block *mblock);
int is_mem_block(struct block *mblock);

#endif
