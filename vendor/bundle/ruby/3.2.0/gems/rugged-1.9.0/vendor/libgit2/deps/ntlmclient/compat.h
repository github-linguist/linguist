/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#ifndef PRIVATE_COMPAT_H__
#define PRIVATE_COMPAT_H__

#if defined (_MSC_VER)
 typedef unsigned char bool;
# ifndef true
#  define true 1
# endif
# ifndef false
#  define false 0
# endif
#else
# include <stdbool.h>
#endif

#ifndef MIN
# define MIN(x, y) (((x) < (y)) ? (x) : (y))
#endif

#endif /* PRIVATE_COMPAT_H__ */
