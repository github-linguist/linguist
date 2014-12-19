#pragma once

/* Copyright © 2011 Lukas Martini
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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Xelix. If not, see <http://www.gnu.org/licenses/>.
 */

#include <lib/generic.h>
#include <tasks/scheduler.h>

#define ELF_TYPE_NONE 0
#define ELF_TYPE_REL 1
#define ELF_TYPE_EXEC 2
#define ELF_TYPE_DYN 3
#define ELF_TYPE_CORE 4

#define ELF_ARCH_NONE 0
#define ELF_ARCH_386 3

#define ELF_VERSION_CURRENT 1

typedef struct {
	unsigned char magic[4];
	/* Note: There _is_ other stuff in here, but we don't need it */
	unsigned char pad[12]; 
} __attribute__((packed)) elf_ident_t;

typedef struct {
	uint32_t    type;
	uint32_t    offset;
	void*		virtaddr;
	void*		physaddr;
	uint32_t    filesize;
	uint32_t    memsize;
	uint32_t    flags;
	uint32_t    alignment;
} __attribute__((packed)) elf_program_t;

typedef struct {
	elf_ident_t ident;
	uint16_t	type;		/* Object file type */
	uint16_t	machine;	/* Architecture */
	uint32_t	version;	/* Object file version */
	void*		entry;		/* Entry point virtual address */
	uint32_t	phoff;		/* Program header table file offset */
	uint32_t	shoff;		/* Section header table file offset */
	uint32_t	flags;		/* Processor-specific flags */
	uint16_t	ehsize;		/* ELF header size in bytes */
	uint16_t	phentsize;	/* Program header table entry size */
	uint16_t	phnum;		/* Program header table entry count */
	uint16_t	shentsize;	/* Section header table entry size */
	uint16_t	shnum;		/* Section header table entry count */
	uint16_t	shstrndx;	/* Section header string table index */
} __attribute__((packed)) elf_t;

task_t* elf_load(elf_t* bin, char* name, char** environ, char** argv, int argc);
task_t* elf_load_file(char* path, char** environ, char** argv, int argc);
