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

typedef struct {
	uint32_t background;
	uint32_t foreground;
} console_color_t;

#define CONSOLE_COLOR_BLACK    0x0
#define CONSOLE_COLOR_BLUE     0x1
#define CONSOLE_COLOR_GREEN    0x2
#define CONSOLE_COLOR_CYAN     0x3
#define CONSOLE_COLOR_RED      0x4
#define CONSOLE_COLOR_MAGENTA  0x5
#define CONSOLE_COLOR_BROWN    0x6
#define CONSOLE_COLOR_LGREY    0x7
#define CONSOLE_COLOR_DGREY    0x8
#define CONSOLE_COLOR_LBLUE    0x9
#define CONSOLE_COLOR_LGREEN   0xa
#define CONSOLE_COLOR_LCYAN    0xb
#define CONSOLE_COLOR_LRED     0xc
#define CONSOLE_COLOR_LMAGENTA 0xd
#define CONSOLE_COLOR_YELLOW   0xe
#define CONSOLE_COLOR_WHITE    0xf

