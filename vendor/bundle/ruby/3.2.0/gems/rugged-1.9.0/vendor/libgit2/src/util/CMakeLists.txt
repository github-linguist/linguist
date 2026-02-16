# util: a shared library for common utility functions for libgit2 projects

add_library(util OBJECT)

configure_file(git2_features.h.in git2_features.h)

set(UTIL_INCLUDES
	"${PROJECT_BINARY_DIR}/src/util"
	"${PROJECT_BINARY_DIR}/include"
	"${PROJECT_SOURCE_DIR}/src/util"
	"${PROJECT_SOURCE_DIR}/include")

file(GLOB UTIL_SRC *.c *.h allocators/*.c allocators/*.h hash.h)
list(SORT UTIL_SRC)

#
# Platform specific sources
#

if(WIN32 AND NOT CYGWIN)
	file(GLOB UTIL_SRC_OS win32/*.c win32/*.h)
	list(SORT UTIL_SRC_OS)
elseif(NOT AMIGA)
	file(GLOB UTIL_SRC_OS unix/*.c unix/*.h)
	list(SORT UTIL_SRC_OS)
endif()

#
# Hash backend selection
#

if(USE_SHA1 STREQUAL "CollisionDetection")
	file(GLOB UTIL_SRC_SHA1 hash/collisiondetect.* hash/sha1dc/*)
	target_compile_definitions(util PRIVATE SHA1DC_NO_STANDARD_INCLUDES=1)
        target_compile_definitions(util PRIVATE SHA1DC_CUSTOM_INCLUDE_SHA1_C=\"git2_util.h\")
        target_compile_definitions(util PRIVATE SHA1DC_CUSTOM_INCLUDE_UBC_CHECK_C=\"git2_util.h\")
elseif(USE_SHA1 STREQUAL "OpenSSL" OR USE_SHA1 STREQUAL "OpenSSL-Dynamic" OR USE_SHA1 STREQUAL "OpenSSL-FIPS")
	add_definitions(-DOPENSSL_API_COMPAT=0x10100000L)
	file(GLOB UTIL_SRC_SHA1 hash/openssl.*)
elseif(USE_SHA1 STREQUAL "CommonCrypto")
	file(GLOB UTIL_SRC_SHA1 hash/common_crypto.*)
elseif(USE_SHA1 STREQUAL "mbedTLS")
	file(GLOB UTIL_SRC_SHA1 hash/mbedtls.*)
elseif(USE_SHA1 STREQUAL "Win32")
	file(GLOB UTIL_SRC_SHA1 hash/win32.*)
else()
	message(FATAL_ERROR "Asked for unknown SHA1 backend: ${USE_SHA1}")
endif()

list(SORT UTIL_SRC_SHA1)

if(USE_SHA256 STREQUAL "builtin")
	file(GLOB UTIL_SRC_SHA256 hash/builtin.* hash/rfc6234/*)
elseif(USE_SHA256 STREQUAL "OpenSSL" OR USE_SHA256 STREQUAL "OpenSSL-Dynamic" OR USE_SHA256 STREQUAL "OpenSSL-FIPS")
	add_definitions(-DOPENSSL_API_COMPAT=0x10100000L)
	file(GLOB UTIL_SRC_SHA256 hash/openssl.*)
elseif(USE_SHA256 STREQUAL "CommonCrypto")
	file(GLOB UTIL_SRC_SHA256 hash/common_crypto.*)
elseif(USE_SHA256 STREQUAL "mbedTLS")
	file(GLOB UTIL_SRC_SHA256 hash/mbedtls.*)
elseif(USE_SHA256 STREQUAL "Win32")
	file(GLOB UTIL_SRC_SHA256 hash/win32.*)
else()
	message(FATAL_ERROR "asked for unknown SHA256 backend: ${USE_SHA256}")
endif()

list(SORT UTIL_SRC_SHA256)

#
# Build the library
#

target_sources(util PRIVATE ${UTIL_SRC} ${UTIL_SRC_OS} ${UTIL_SRC_SHA1} ${UTIL_SRC_SHA256})
ide_split_sources(util)

target_include_directories(util PRIVATE ${UTIL_INCLUDES} ${LIBGIT2_DEPENDENCY_INCLUDES} PUBLIC ${libgit2_SOURCE_DIR}/include)
target_include_directories(util SYSTEM PRIVATE ${LIBGIT2_SYSTEM_INCLUDES})
