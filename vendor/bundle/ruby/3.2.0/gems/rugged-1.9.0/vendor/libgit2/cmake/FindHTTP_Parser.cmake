# - Try to find http-parser
#
# Defines the following variables:
#
# HTTP_PARSER_FOUND - system has http-parser
# HTTP_PARSER_INCLUDE_DIR - the http-parser include directory
# HTTP_PARSER_LIBRARIES - Link these to use http-parser
# HTTP_PARSER_VERSION_MAJOR - major version
# HTTP_PARSER_VERSION_MINOR - minor version
# HTTP_PARSER_VERSION_STRING - the version of http-parser found

# Find the header and library
find_path(HTTP_PARSER_INCLUDE_DIR NAMES http_parser.h)
find_library(HTTP_PARSER_LIBRARY NAMES http_parser libhttp_parser)

# Found the header, read version
if(HTTP_PARSER_INCLUDE_DIR AND EXISTS "${HTTP_PARSER_INCLUDE_DIR}/http_parser.h")
	file(READ "${HTTP_PARSER_INCLUDE_DIR}/http_parser.h" HTTP_PARSER_H)
	if(HTTP_PARSER_H)
		string(REGEX REPLACE ".*#define[\t ]+HTTP_PARSER_VERSION_MAJOR[\t ]+([0-9]+).*" "\\1" HTTP_PARSER_VERSION_MAJOR "${HTTP_PARSER_H}")
		string(REGEX REPLACE ".*#define[\t ]+HTTP_PARSER_VERSION_MINOR[\t ]+([0-9]+).*" "\\1" HTTP_PARSER_VERSION_MINOR "${HTTP_PARSER_H}")
		set(HTTP_PARSER_VERSION_STRING "${HTTP_PARSER_VERSION_MAJOR}.${HTTP_PARSER_VERSION_MINOR}")
	endif()
	unset(HTTP_PARSER_H)
endif()

# Handle the QUIETLY and REQUIRED arguments and set HTTP_PARSER_FOUND
# to TRUE if all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(HTTP_Parser REQUIRED_VARS HTTP_PARSER_INCLUDE_DIR HTTP_PARSER_LIBRARY)

# Hide advanced variables
mark_as_advanced(HTTP_PARSER_INCLUDE_DIR HTTP_PARSER_LIBRARY)

# Set standard variables
if(HTTP_PARSER_FOUND)
	set(HTTP_PARSER_LIBRARIES ${HTTP_PARSER_LIBRARY})
	set(HTTP_PARSER_INCLUDE_DIRS ${HTTP_PARSER_INCLUDE_DIR})
endif()
