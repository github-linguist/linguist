include(CheckIncludeFile)
include(CheckFunctionExists)
include(CheckTypeSize)

check_include_file(dirent.h     HAVE_DIRENT_H)
check_include_file(stdint.h     HAVE_STDINT_H)
check_include_file(inttypes.h   HAVE_INTTYPES_H)
check_include_file(sys/stat.h   HAVE_SYS_STAT_H)
check_include_file(sys/types.h  HAVE_SYS_TYPES_H)
check_include_file(unistd.h     HAVE_UNISTD_H)
check_include_file(windows.h    HAVE_WINDOWS_H)

check_function_exists(bcopy     HAVE_BCOPY)
check_function_exists(memmove   HAVE_MEMMOVE)
check_function_exists(strerror  HAVE_STRERROR)
check_function_exists(strtoll   HAVE_STRTOLL)
check_function_exists(strtoq    HAVE_STRTOQ)
check_function_exists(_strtoi64 HAVE__STRTOI64)

check_type_size("long long"             LONG_LONG)
check_type_size("unsigned long long"    UNSIGNED_LONG_LONG)

disable_warnings(unused-function)
disable_warnings(implicit-fallthrough)
disable_warnings(unused-but-set-variable)

# User-configurable options

set(SUPPORT_PCRE8 1)
set(PCRE_LINK_SIZE "2")
set(PCRE_PARENS_NEST_LIMIT "250")
set(PCRE_MATCH_LIMIT "10000000")
set(PCRE_MATCH_LIMIT_RECURSION "MATCH_LIMIT")
set(PCRE_NEWLINE "LF")
set(NO_RECURSE 1)
set(PCRE_POSIX_MALLOC_THRESHOLD "10")
set(BSR_ANYCRLF 0)

if(MINGW)
	option(NON_STANDARD_LIB_PREFIX
		"ON=Shared libraries built in mingw will be named pcre.dll, etc., instead of libpcre.dll, etc."
		OFF)

	option(NON_STANDARD_LIB_SUFFIX
		"ON=Shared libraries built in mingw will be named libpcre-0.dll, etc., instead of libpcre.dll, etc."
		OFF)
endif(MINGW)

# Prepare build configuration

set(pcre_have_long_long 0)
set(pcre_have_ulong_long 0)

if(HAVE_LONG_LONG)
        set(pcre_have_long_long 1)
endif(HAVE_LONG_LONG)

if(HAVE_UNSIGNED_LONG_LONG)
        set(pcre_have_ulong_long 1)
endif(HAVE_UNSIGNED_LONG_LONG)

set(NEWLINE "")

if(PCRE_NEWLINE STREQUAL "LF")
        set(NEWLINE "10")
endif(PCRE_NEWLINE STREQUAL "LF")
if(PCRE_NEWLINE STREQUAL "CR")
        set(NEWLINE "13")
endif(PCRE_NEWLINE STREQUAL "CR")
if(PCRE_NEWLINE STREQUAL "CRLF")
        set(NEWLINE "3338")
endif(PCRE_NEWLINE STREQUAL "CRLF")
if(PCRE_NEWLINE STREQUAL "ANY")
        set(NEWLINE "-1")
endif(PCRE_NEWLINE STREQUAL "ANY")
if(PCRE_NEWLINE STREQUAL "ANYCRLF")
        set(NEWLINE "-2")
endif(PCRE_NEWLINE STREQUAL "ANYCRLF")

if(NEWLINE STREQUAL "")
        message(FATAL_ERROR "The PCRE_NEWLINE variable must be set to one of the following values: \"LF\", \"CR\", \"CRLF\", \"ANY\", \"ANYCRLF\".")
endif(NEWLINE STREQUAL "")

# Output files
configure_file(config.h.in
               ${PROJECT_BINARY_DIR}/src/pcre/config.h
               @ONLY)

# Source code

set(PCRE_HEADERS ${PROJECT_BINARY_DIR}/src/pcre/config.h)

set(PCRE_SOURCES
  pcre_byte_order.c
  pcre_chartables.c
  pcre_compile.c
  pcre_config.c
  pcre_dfa_exec.c
  pcre_exec.c
  pcre_fullinfo.c
  pcre_get.c
  pcre_globals.c
  pcre_jit_compile.c
  pcre_maketables.c
  pcre_newline.c
  pcre_ord2utf8.c
  pcre_refcount.c
  pcre_string_utils.c
  pcre_study.c
  pcre_tables.c
  pcre_ucd.c
  pcre_valid_utf8.c
  pcre_version.c
  pcre_xclass.c
)

set(PCREPOSIX_HEADERS pcreposix.h)

set(PCREPOSIX_SOURCES pcreposix.c)

# Fix static compilation with MSVC: https://bugs.exim.org/show_bug.cgi?id=1681
# This code was taken from the CMake wiki, not from WebM.

# Build setup

add_definitions(-DHAVE_CONFIG_H)

if(MSVC)
        add_definitions(-D_CRT_SECURE_NO_DEPRECATE -D_CRT_SECURE_NO_WARNINGS)
endif(MSVC)

set(CMAKE_INCLUDE_CURRENT_DIR 1)

set(targets)

# Libraries
# pcre
include_directories(${PROJECT_BINARY_DIR}/src/pcre)
add_library(pcre OBJECT ${PCRE_HEADERS} ${PCRE_SOURCES} ${PCREPOSIX_SOURCES})

# end CMakeLists.txt
