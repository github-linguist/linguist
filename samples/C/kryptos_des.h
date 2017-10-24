/*
 *                          Copyright (C) 2006, 2017 by Rafael Santiago
 *
 * This is a free software. You can redistribute it and/or modify under
 * the terms of the GNU General Public License version 3.
 *
 */
#ifndef KRYPTOS_KRYPTOS_DES_H
#define KRYPTOS_KRYPTOD_DES_H 1

#include <kryptos_types.h>

#define KRYPTOS_DES_BLOCKSIZE 8

KRYPTOS_DECL_STANDARD_BLOCK_CIPHER_SETUP(des)

KRYPTOS_DECL_BLOCK_CIPHER_PROCESSOR(des)

#endif
