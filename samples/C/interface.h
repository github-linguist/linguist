#pragma once

/* Copyright © 2011 Fritz Grimpen
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
#include <console/info.h>
#include <console/filter.h>
#include <console/driver.h>

typedef struct {
	console_info_t info;

	console_filter_t* input_filter;
	console_filter_t* output_filter;

	console_driver_t* input_driver;
	console_driver_t* output_driver;
} console_t;

console_t* default_console;

// Generate raw console, connected to the Display, Keyboard and the
// ECMA-48-Filter
void console_init();

size_t console_write(console_t* console, const char* buffer, int32_t length);
#define console_write2(console, buffer) console_write(console, buffer, strlen(buffer))
size_t console_read(console_t* console, char* buffer, size_t length);
size_t console_scroll(console_t* console, int32_t pages);

void console_clear(console_t* console);
