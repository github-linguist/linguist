# - Try to find Iconv
# Once done this will define
#
# ICONV_FOUND - system has Iconv
# ICONV_INCLUDE_DIR - the Iconv include directory
# ICONV_LIBRARIES - Link these to use Iconv
#

if(ICONV_INCLUDE_DIR AND ICONV_LIBRARIES)
	# Already in cache, be silent
	set(ICONV_FIND_QUIETLY TRUE)
endif()

find_path(ICONV_INCLUDE_DIR iconv.h)
check_function_exists(iconv_open libc_has_iconv)
find_library(iconv_lib NAMES iconv libiconv libiconv-2 c)

# workaround the iOS issue where iconv is provided by libc
# We set it to false to force it add -liconv to the linker flags
if(CMAKE_SYSTEM_NAME MATCHES "iOS")
    set(libc_has_iconv FALSE)
endif()

if(ICONV_INCLUDE_DIR AND libc_has_iconv)
	set(ICONV_FOUND TRUE)
	set(ICONV_LIBRARIES "")
	if(NOT ICONV_FIND_QUIETLY)
		message(STATUS "Found Iconv: provided by libc")
	endif(NOT ICONV_FIND_QUIETLY)
elseif(ICONV_INCLUDE_DIR AND iconv_lib)
	set(ICONV_FOUND TRUE)
	# split iconv into -L and -l linker options, so we can
	# set them for pkg-config
	get_filename_component(iconv_path ${iconv_lib} PATH)
	get_filename_component(iconv_name ${iconv_lib} NAME_WE)
	string(REGEX REPLACE "^lib" "" iconv_name ${iconv_name})
	set(ICONV_LIBRARIES "-L${iconv_path} -l${iconv_name}")

	if(NOT ICONV_FIND_QUIETLY)
		message(STATUS "Found Iconv: ${ICONV_LIBRARIES}")
	endif()
else()
	if(Iconv_FIND_REQUIRED)
		message(FATAL_ERROR "Could not find Iconv")
	endif(Iconv_FIND_REQUIRED)
endif()

mark_as_advanced(
	ICONV_INCLUDE_DIR
	ICONV_LIBRARIES
)
