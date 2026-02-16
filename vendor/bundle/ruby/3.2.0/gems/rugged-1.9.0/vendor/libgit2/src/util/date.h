/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_date_h__
#define INCLUDE_date_h__

#include "util.h"
#include "str.h"

/*
 * Parse a string into a value as a git_time_t with a timezone offset.
 *
 * Sample valid input:
 * - "yesterday"
 * - "July 17, 2003"
 * - "2003-7-17 08:23i+03"
 */
extern int git_date_offset_parse(git_time_t *out, int *out_offset, const char *date);

/*
 * Parse a string into a value as a git_time_t.
 *
 * Timezone offset is ignored.
 *
 * Sample valid input:
 * - "yesterday"
 * - "July 17, 2003"
 * - "2003-7-17 08:23"
 */
extern int git_date_parse(git_time_t *out, const char *date);

/*
 * Format a git_time as a RFC2822 string
 *
 * @param out buffer to store formatted date
 * @param time the time to be formatted
 * @param offset the timezone offset
 * @return 0 if successful; -1 on error
 */
extern int git_date_rfc2822_fmt(git_str *out, git_time_t time, int offset);

#endif
