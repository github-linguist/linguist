# CMake build script for the bundled Chromium zlib implementation. So far, it
# is only supported for x86_64 processors with CLMUL, SSE3, SSE4.2.
#
# TODO: The Chromium build file (in deps/chromium-zlib/zlib/BUILD.gn) supports
# more platforms (like ARM with NEON), more can be enabled as needed.

cmake_minimum_required(VERSION 3.11)

include(FetchContent)
include(FindGit)

# Ensure that the git binary is present to download the sources.
find_package(Git)
if(NOT Git_FOUND)
	message(FATAL_ERROR "git is required to download the Chromium zlib sources")
endif()

fetchcontent_populate(chromium_zlib_src
	GIT_REPOSITORY https://chromium.googlesource.com/chromium/src/third_party/zlib.git
	GIT_TAG 2c183c9f93a328bfb3121284da13cf89a0f7e64a
	QUIET
)

# The Chromium build globally disables some warnings.
disable_warnings(implicit-fallthrough)
disable_warnings(unused-function)
disable_warnings(unused-parameter)
disable_warnings(sign-compare)
disable_warnings(declaration-after-statement)
disable_warnings(missing-declarations)

# -O3 is also set by the Chromium configuration and has been deemed safe enough
# for them.
set(CMAKE_C_FLAGS_RELWITHDEBINFO "-O3 -g -DNDEBUG")
set(CMAKE_C_FLAGS_RELEASE "-O3 -DNDEBUG")

# Common definitions.
add_definitions(
	-DSTDC
	-DNO_GZIP
	-DZLIB_IMPLEMENTATION
)
list(APPEND SRC_ZLIB
	"${chromium_zlib_src_SOURCE_DIR}/adler32.c"
	"${chromium_zlib_src_SOURCE_DIR}/chromeconf.h"
	"${chromium_zlib_src_SOURCE_DIR}/compress.c"
	"${chromium_zlib_src_SOURCE_DIR}/contrib/optimizations/insert_string.h"
	"${chromium_zlib_src_SOURCE_DIR}/cpu_features.c"
	"${chromium_zlib_src_SOURCE_DIR}/cpu_features.h"
	"${chromium_zlib_src_SOURCE_DIR}/crc32.c"
	"${chromium_zlib_src_SOURCE_DIR}/crc32.h"
	"${chromium_zlib_src_SOURCE_DIR}/deflate.c"
	"${chromium_zlib_src_SOURCE_DIR}/deflate.h"
	"${chromium_zlib_src_SOURCE_DIR}/gzclose.c"
	"${chromium_zlib_src_SOURCE_DIR}/gzguts.h"
	"${chromium_zlib_src_SOURCE_DIR}/gzlib.c"
	"${chromium_zlib_src_SOURCE_DIR}/gzread.c"
	"${chromium_zlib_src_SOURCE_DIR}/gzwrite.c"
	"${chromium_zlib_src_SOURCE_DIR}/infback.c"
	"${chromium_zlib_src_SOURCE_DIR}/inffast.c"
	"${chromium_zlib_src_SOURCE_DIR}/inffast.h"
	"${chromium_zlib_src_SOURCE_DIR}/inffixed.h"
	"${chromium_zlib_src_SOURCE_DIR}/inflate.h"
	"${chromium_zlib_src_SOURCE_DIR}/inftrees.c"
	"${chromium_zlib_src_SOURCE_DIR}/inftrees.h"
	"${chromium_zlib_src_SOURCE_DIR}/trees.c"
	"${chromium_zlib_src_SOURCE_DIR}/trees.h"
	"${chromium_zlib_src_SOURCE_DIR}/uncompr.c"
	"${chromium_zlib_src_SOURCE_DIR}/zconf.h"
	"${chromium_zlib_src_SOURCE_DIR}/zlib.h"
	"${chromium_zlib_src_SOURCE_DIR}/zutil.c"
	"${chromium_zlib_src_SOURCE_DIR}/zutil.h"
)

# x86_64-specific optimizations
string(APPEND CMAKE_C_FLAGS " -mssse3 -msse4.2 -mpclmul")
add_definitions(
  -DCHROMIUM_ZLIB_NO_CHROMECONF
	-DX86_NOT_WINDOWS
	-DADLER32_SIMD_SSSE3
	-DCRC32_SIMD_SSE42_PCLMUL
	-DDEFLATE_FILL_WINDOW_SSE2
	-DINFLATE_CHUNK_READ_64LE
	-DINFLATE_CHUNK_SIMD_SSE2
)
list(APPEND SRC_ZLIB
	"${chromium_zlib_src_SOURCE_DIR}/adler32_simd.c"
	"${chromium_zlib_src_SOURCE_DIR}/adler32_simd.h"
	"${chromium_zlib_src_SOURCE_DIR}/contrib/optimizations/chunkcopy.h"
	"${chromium_zlib_src_SOURCE_DIR}/contrib/optimizations/inffast_chunk.c"
	"${chromium_zlib_src_SOURCE_DIR}/contrib/optimizations/inffast_chunk.h"
	"${chromium_zlib_src_SOURCE_DIR}/contrib/optimizations/inflate.c"
	"${chromium_zlib_src_SOURCE_DIR}/crc32_simd.c"
	"${chromium_zlib_src_SOURCE_DIR}/crc32_simd.h"
	"${chromium_zlib_src_SOURCE_DIR}/crc_folding.c"
	"${chromium_zlib_src_SOURCE_DIR}/fill_window_sse.c"
)

list(SORT SRC_ZLIB)
include_directories("${chromium_zlib_src_SOURCE_DIR}")
add_library(chromium_zlib OBJECT ${SRC_ZLIB})
