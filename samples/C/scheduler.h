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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Xelix. If not, see <http://www.gnu.org/licenses/>.
 */

#include <lib/generic.h>
#include <hw/cpu.h>
#include <memory/vmem.h>

#define SCHEDULER_MAXNAME 256
#define SCHEDULER_TASK_PATH_MAX 256

// Single linked list
typedef struct task {
	uint32_t pid;
	char name[SCHEDULER_MAXNAME];
	struct task *parent;
	cpu_state_t* state;
	struct task* next;
	struct task* previous;

	void* stack;
	void* entry;
	struct vmem_context *memory_context;

	// Current task state
	enum {
		TASK_STATE_KILLED,
		TASK_STATE_TERMINATED,
		TASK_STATE_BLOCKING,
		TASK_STATE_STOPPED,
		TASK_STATE_RUNNING
	} task_state;

	char** environ;
	char** argv;
	int argc;

	// TODO Is this actually the same as PATH_MAX in our toolchain?
	char cwd[SCHEDULER_TASK_PATH_MAX + 1];
} task_t;

int scheduler_state;

task_t* scheduler_new(void* entry, task_t* parent, char name[SCHEDULER_MAXNAME],
	char** environ, char** argv, int argc, struct vmem_context* memory_context, bool map_structs);
void scheduler_add(task_t *task);
void scheduler_terminate_current();
task_t* scheduler_get_current();
task_t* scheduler_select(cpu_state_t* lastRegs);
void scheduler_init();
void scheduler_yield();
void scheduler_remove(task_t *t);
task_t* scheduler_fork(task_t* to_fork, cpu_state_t* state);