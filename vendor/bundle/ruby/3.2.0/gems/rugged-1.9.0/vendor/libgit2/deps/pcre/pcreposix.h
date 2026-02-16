/*************************************************
*       Perl-Compatible Regular Expressions      *
*************************************************/

#ifndef _PCREPOSIX_H
#define _PCREPOSIX_H

/* This is the header for the POSIX wrapper interface to the PCRE Perl-
Compatible Regular Expression library. It defines the things POSIX says should
be there. I hope.

            Copyright (c) 1997-2012 University of Cambridge

-----------------------------------------------------------------------------
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

    * Neither the name of the University of Cambridge nor the names of its
      contributors may be used to endorse or promote products derived from
      this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
-----------------------------------------------------------------------------
*/

/* Have to include stdlib.h in order to ensure that size_t is defined. */

#include <stdlib.h>

#define PCREPOSIX_EXP_DEFN  extern
#define PCREPOSIX_EXP_DECL  extern

/* Options, mostly defined by POSIX, but with some extras. */

#define PCRE_REG_ICASE     0x0001   /* Maps to PCRE_CASELESS */
#define PCRE_REG_NEWLINE   0x0002   /* Maps to PCRE_MULTILINE */
#define PCRE_REG_NOTBOL    0x0004   /* Maps to PCRE_NOTBOL */
#define PCRE_REG_NOTEOL    0x0008   /* Maps to PCRE_NOTEOL */
#define PCRE_REG_DOTALL    0x0010   /* NOT defined by POSIX; maps to PCRE_DOTALL */
#define PCRE_REG_NOSUB     0x0020   /* Maps to PCRE_NO_AUTO_CAPTURE */
#define PCRE_REG_UTF8      0x0040   /* NOT defined by POSIX; maps to PCRE_UTF8 */
#define PCRE_REG_STARTEND  0x0080   /* BSD feature: pass subject string by so,eo */
#define PCRE_REG_NOTEMPTY  0x0100   /* NOT defined by POSIX; maps to PCRE_NOTEMPTY */
#define PCRE_REG_UNGREEDY  0x0200   /* NOT defined by POSIX; maps to PCRE_UNGREEDY */
#define PCRE_REG_UCP       0x0400   /* NOT defined by POSIX; maps to PCRE_UCP */

/* This is not used by PCRE, but by defining it we make it easier
to slot PCRE into existing programs that make POSIX calls. */

#define PCRE_REG_EXTENDED  0

/* Error values. Not all these are relevant or used by the wrapper. */

enum {
  PCRE_REG_ASSERT = 1,  /* internal error ? */
  PCRE_REG_BADBR,       /* invalid repeat counts in {} */
  PCRE_REG_BADPAT,      /* pattern error */
  PCRE_REG_BADRPT,      /* ? * + invalid */
  PCRE_REG_EBRACE,      /* unbalanced {} */
  PCRE_REG_EBRACK,      /* unbalanced [] */
  PCRE_REG_ECOLLATE,    /* collation error - not relevant */
  PCRE_REG_ECTYPE,      /* bad class */
  PCRE_REG_EESCAPE,     /* bad escape sequence */
  PCRE_REG_EMPTY,       /* empty expression */
  PCRE_REG_EPAREN,      /* unbalanced () */
  PCRE_REG_ERANGE,      /* bad range inside [] */
  PCRE_REG_ESIZE,       /* expression too big */
  PCRE_REG_ESPACE,      /* failed to get memory */
  PCRE_REG_ESUBREG,     /* bad back reference */
  PCRE_REG_INVARG,      /* bad argument */
  PCRE_REG_NOMATCH      /* match failed */
};


/* The structure representing a compiled regular expression. */

typedef struct {
  void *re_pcre;
  size_t re_nsub;
  size_t re_erroffset;
} pcre_regex_t;

/* The structure in which a captured offset is returned. */

typedef int pcre_regoff_t;

typedef struct {
  pcre_regoff_t rm_so;
  pcre_regoff_t rm_eo;
} pcre_regmatch_t;

/* The functions */

PCREPOSIX_EXP_DECL int pcre_regcomp(pcre_regex_t *, const char *, int);
PCREPOSIX_EXP_DECL int pcre_regexec(const pcre_regex_t *, const char *, size_t,
                     pcre_regmatch_t *, int);
PCREPOSIX_EXP_DECL size_t pcre_regerror(int, const pcre_regex_t *, char *, size_t);
PCREPOSIX_EXP_DECL void pcre_regfree(pcre_regex_t *);

#endif /* End of pcreposix.h */
