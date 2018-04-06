/*
macros for calculating alignment
the _plibc_syscall function that makes syscalls available in c
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
#ifndef _ALIGN_H
#define _ALIGN_H
#include <stdint.h>

#define align2(i) ((2 - (((uint64_t) i) % 2)) % 2)
#define align4(i) ((4 - (((uint64_t) i) % 4)) % 4)
#define align8(i) ((8 - (((uint64_t) i) % 8)) % 8)

#define alignn(i, n) ((n - (((uint64_t) i) % n)) % n)

#endif
