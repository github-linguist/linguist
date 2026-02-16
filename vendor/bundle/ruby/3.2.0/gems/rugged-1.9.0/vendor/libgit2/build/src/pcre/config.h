/* config.h for CMake builds */

#define HAVE_DIRENT_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_UNISTD_H 1
/* #undef HAVE_WINDOWS_H */
#define HAVE_STDINT_H 1                                                   
#define HAVE_INTTYPES_H 1    

/* #undef HAVE_TYPE_TRAITS_H */
/* #undef HAVE_BITS_TYPE_TRAITS_H */

#define HAVE_BCOPY 1
#define HAVE_MEMMOVE 1
#define HAVE_STRERROR 1
#define HAVE_STRTOLL 1
#define HAVE_STRTOQ 1
/* #undef HAVE__STRTOI64 */

/* #undef PCRE_STATIC */

#define SUPPORT_PCRE8 1
/* #undef SUPPORT_PCRE16 */
/* #undef SUPPORT_PCRE32 */
/* #undef SUPPORT_JIT */
/* #undef SUPPORT_PCREGREP_JIT */
/* #undef SUPPORT_UTF */
/* #undef SUPPORT_UCP */
/* #undef EBCDIC */
/* #undef EBCDIC_NL25 */
/* #undef BSR_ANYCRLF */
#define NO_RECURSE 1

#define HAVE_LONG_LONG 1
#define HAVE_UNSIGNED_LONG_LONG 1

/* #undef SUPPORT_LIBBZ2 */
/* #undef SUPPORT_LIBZ */
/* #undef SUPPORT_LIBEDIT */
/* #undef SUPPORT_LIBREADLINE */

/* #undef SUPPORT_VALGRIND */
/* #undef SUPPORT_GCOV */

#define NEWLINE			10
#define POSIX_MALLOC_THRESHOLD	10
#define LINK_SIZE		2
#define PARENS_NEST_LIMIT       250
#define MATCH_LIMIT		10000000
#define MATCH_LIMIT_RECURSION	MATCH_LIMIT
#define PCREGREP_BUFSIZE        

#define MAX_NAME_SIZE	32
#define MAX_NAME_COUNT	10000

/* end config.h for CMake builds */
