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
#include <net/net.h>

#define IP4_TOS_ICMP 0

typedef uint32_t ip4_addr_t;

typedef struct {
	unsigned int hl:4; /* both fields are 4 bits */
	unsigned int version:4;
	uint8_t	tos;
	uint16_t len;
	uint16_t id;
	uint16_t off;
	uint8_t ttl;
	uint8_t p;
	uint16_t checksum;
	ip4_addr_t src;
	ip4_addr_t dst;
} ip4_header_t;

typedef struct {
	uint8_t type;
	uint8_t code;
	uint16_t checksum;
	uint16_t id;
	uint16_t sequence;
} ip4_icmp_header_t;

void ip4_receive(net_device_t* origin, net_l2proto_t proto, size_t size, void* raw);
