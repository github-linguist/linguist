# - Try to find llhttp
#
# Defines the following variables:
#
# LLHTTP_FOUND - system has llhttp
# LLHTTP_INCLUDE_DIR - the llhttp include directory
# LLHTTP_LIBRARIES - Link these to use llhttp
# LLHTTP_VERSION_MAJOR - major version
# LLHTTP_VERSION_MINOR - minor version
# LLHTTP_VERSION_STRING - the version of llhttp found

# Find the header and library
find_path(LLHTTP_INCLUDE_DIR NAMES llhttp.h)
find_library(LLHTTP_LIBRARY NAMES llhttp libllhttp)

# Found the header, read version
if(LLHTTP_INCLUDE_DIR AND EXISTS "${LLHTTP_INCLUDE_DIR}/llhttp.h")
	file(READ "${LLHTTP_INCLUDE_DIR}/llhttp.h" LLHTTP_H)
	if(LLHTTP_H)
		string(REGEX REPLACE ".*#define[\t ]+LLHTTP_VERSION_MAJOR[\t ]+([0-9]+).*" "\\1" LLHTTP_VERSION_MAJOR "${LLHTTP_H}")
		string(REGEX REPLACE ".*#define[\t ]+LLHTTP_VERSION_MINOR[\t ]+([0-9]+).*" "\\1" LLHTTP_VERSION_MINOR "${LLHTTP_H}")
		set(LLHTTP_VERSION_STRING "${LLHTTP_VERSION_MAJOR}.${LLHTTP_VERSION_MINOR}")
	endif()
	unset(LLHTTP_H)
endif()

# Handle the QUIETLY and REQUIRED arguments and set LLHTTP_FOUND
# to TRUE if all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LLHTTP REQUIRED_VARS LLHTTP_INCLUDE_DIR LLHTTP_LIBRARY)

# Hide advanced variables
mark_as_advanced(LLHTTP_INCLUDE_DIR LLHTTP_LIBRARY)

# Set standard variables
if(LLHTTP_FOUND)
	set(LLHTTP_LIBRARIES ${LLHTTP_LIBRARY})
	set(LLHTTP_INCLUDE_DIRS ${LLHTTP_INCLUDE_DIR})
endif()
