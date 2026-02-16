disable_warnings(implicit-fallthrough)
add_definitions(-DNO_VIZ -DSTDC -DNO_GZIP -DHAVE_SYS_TYPES_H -DHAVE_STDINT_H -DHAVE_STDDEF_H)

if(MINGW OR MSYS)
	add_definitions(-DZ_HAVE_UNISTD_H -D_LFS64_LARGEFILE -D_LARGEFILE64_SOURCE=1)
endif()

file(GLOB SRC_ZLIB "*.c" "*.h")
list(SORT SRC_ZLIB)
include_directories(".")
add_library(zlib OBJECT ${SRC_ZLIB})
