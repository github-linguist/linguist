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
#include <lib/stdint.h>

// Legacy
#define outb(args...) portio_out8(args)
#define outw(args...) portio_out16(args)
#define outl(args...) portio_out32(args)
#define outq(args...) portio_out64(args)

#define inb(args...) portio_in8(args)
#define inw(args...) portio_in16(args)
#define inl(args...) portio_in32(args)
#define inq(args...) portio_in64(args)

void portio_out8(uint16_t port, uint8_t value);
void portio_out16(uint16_t port, uint16_t value);
void portio_out32(uint16_t port, uint32_t value);
void portio_out64(uint16_t port, uint64_t value);

uint8_t portio_in8(uint16_t port);
uint16_t portio_in16(uint16_t port);
uint32_t portio_in32(uint16_t port);
uint64_t portio_in64(uint16_t port);
