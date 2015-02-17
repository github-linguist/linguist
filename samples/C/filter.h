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
#include <console/driver.h>

struct console_filter {
	// General callback for all actions etc.
	// Preferred prototype:
	// char <name>(char c, console_info_t *info, console_driver_t *input, console_driver_t *output);
	char (*callback)(char, console_info_t*, console_driver_t*, console_driver_t*);

	// Specific callbacks for read and write
	// Preferred prototype:
	// char <name>(char c, console_info_t *info, console_driver_t *input);
	char (*read_callback)(char, console_info_t*, console_driver_t*);

	// Preferred prototype:
	// char <name>(char c, console_info_t *info, console_driver_t *output);
	char (*write_callback)(char, console_info_t*, console_driver_t*);

	// The next filter in the filter chain
	struct console_filter* next;
};

typedef struct console_filter console_filter_t;

