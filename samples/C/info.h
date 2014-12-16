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
#include <console/color.h>

typedef struct {
	uint32_t cursor_x;
	uint32_t cursor_y;

	uint32_t rows;
	uint32_t columns;

	uint32_t tabstop;

	console_color_t default_color;
	console_color_t current_color;

	uint8_t nonblocking;
	uint8_t reverse_video;
	uint8_t bold;
	uint8_t blink;
	uint8_t underline;
	uint8_t newline_mode;
	uint8_t auto_echo;
	uint8_t handle_backspace;
} console_info_t;
