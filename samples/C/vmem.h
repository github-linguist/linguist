#pragma once

/* Copyright © 2011 Fritz Grimpen
 * Copyright © 2013 Lukas Martini
 *
 * This file is part of Xelix.
 *
 * Xelix is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Xelix is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Xelix. If not, see <http://www.gnu.org/licenses/>.
 */

#include <lib/generic.h>

struct vmem_context;

struct vmem_page
{
	enum
	{
		VMEM_SECTION_STACK,   /* Initial stack */
		VMEM_SECTION_CODE,    /* Contains program code and is read-only */
		VMEM_SECTION_DATA,    /* Contains static data */
		VMEM_SECTION_HEAP,    /* Allocated by brk(2) at runtime */
		VMEM_SECTION_MMAP,    /* Allocated by mmap(2) at runtime */
		VMEM_SECTION_KERNEL,  /* Contains kernel-internal data */
		VMEM_SECTION_UNMAPPED /* Unmapped */
	} section;

	bool readonly:1;
	bool cow:1; /* Copy-on-Write mechanism */
	bool allocated:1;

	void *cow_src_addr;
	void *virt_addr;
	void *phys_addr;
};

typedef void (*vmem_iterator_t)(struct vmem_context *, struct vmem_page *, uint32_t);

/* Initialize vmem_kernelContext for paging_init() */
void vmem_init();
struct vmem_context *vmem_kernelContext;
struct vmem_context *vmem_currentContext;
struct vmem_context *vmem_processContext;
void *vmem_faultAddress;

/* Some callbacks for magic functions */
void (*vmem_applyPage)(struct vmem_context *, struct vmem_page *);

/* Generate new page context */
struct vmem_context *vmem_new();
struct vmem_page *vmem_new_page();

int vmem_add_page(struct vmem_context *ctx, struct vmem_page *pg);

struct vmem_page *vmem_get_page_phys(struct vmem_context *ctx, void *phys_addr);
struct vmem_page *vmem_get_page_virt(struct vmem_context *ctx, void *virt_addr);
struct vmem_page *vmem_get_page(struct vmem_context *ctx, uint32_t offset);

/* Remove pages in a specific context by physical or virtual address */
struct vmem_page *vmem_rm_page_phys(struct vmem_context *ctx, void *phys_addr);
struct vmem_page *vmem_rm_page_virt(struct vmem_context *ctx, void *virt_addr);

/* Iterator */
int vmem_iterate(struct vmem_context *ctx, vmem_iterator_t callback);

uint32_t vmem_count_pages(struct vmem_context *ctx);
void vmem_dump_page(struct vmem_page *pg);
void vmem_dump(struct vmem_context *ctx);
void vmem_handle_fault(uint32_t code, void *addr, void *instruction);

/* Get/Set cached paging context */
void vmem_set_cache(struct vmem_context *ctx, void *cache);
void *vmem_get_cache(struct vmem_context *ctx);

#ifdef __i386__
	#define PAGE_SIZE 4096
	#define VMEM_ALIGN(x) (typeof(x))(((intptr_t)(x) & 0xFFFFF000) + 0x1000)
	#define VMEM_ALIGN_DOWN(x) (typeof(x))( \
		((intptr_t)(x) - ((intptr_t)(x) % PAGE_SIZE)))
#else
	#define PAGE_SIZE 0
	#define VMEM_ALIGN(x) (x)
#endif
