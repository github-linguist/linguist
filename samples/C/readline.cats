/*
** API in ATS for GNU-readline
*/

/* ****** ****** */

/*
** Permission to use, copy, modify, and distribute this software for any
** purpose with or without fee is hereby granted, provided that the above
** copyright notice and this permission notice appear in all copies.
** 
** THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
** WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
** MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
** ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
** WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
** ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
** OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

/* ****** ****** */

#ifndef READLINE_READLINE_CATS
#define READLINE_READLINE_CATS

/* ****** ****** */

#include <readline/readline.h>

/* ****** ****** */
//
#define \
atscntrb_readline_rl_library_version() ((char*)rl_library_version)
//
#define atscntrb_readline_rl_readline_version() (rl_readline_version)
//
/* ****** ****** */

#define atscntrb_readline_readline readline

/* ****** ****** */

#endif // ifndef READLINE_READLINE_CATS

/* ****** ****** */

/* end of [readline.cats] */
