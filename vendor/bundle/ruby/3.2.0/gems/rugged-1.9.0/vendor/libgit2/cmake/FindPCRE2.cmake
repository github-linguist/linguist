# Copyright (C) 2007-2009 LuaDist.
# Created by Peter Kapec <kapecp@gmail.com>
# Redistribution and use of this file is allowed according to the terms of the MIT license.
# For details see the COPYRIGHT file distributed with LuaDist.
#	Note:
#		Searching headers and libraries is very simple and is NOT as powerful as scripts
#		distributed with CMake, because LuaDist defines directories to search for.
#		Everyone is encouraged to contact the author with improvements. Maybe this file
#		becomes part of CMake distribution sometimes.

# - Find pcre
# Find the native PCRE2 headers and libraries.
#
# PCRE2_INCLUDE_DIRS	- where to find pcre.h, etc.
# PCRE2_LIBRARIES	- List of libraries when using pcre.
# PCRE2_FOUND	- True if pcre found.

# Look for the header file.
find_path(PCRE2_INCLUDE_DIR NAMES pcre2.h)

# Look for the library.
find_library(PCRE2_LIBRARY NAMES pcre2-8)

# Handle the QUIETLY and REQUIRED arguments and set PCRE2_FOUND to TRUE if all listed variables are TRUE.
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(PCRE2 DEFAULT_MSG PCRE2_LIBRARY PCRE2_INCLUDE_DIR)

# Copy the results to the output variables.
if(PCRE2_FOUND)
	set(PCRE2_LIBRARIES ${PCRE2_LIBRARY})
	set(PCRE2_INCLUDE_DIRS ${PCRE2_INCLUDE_DIR})
else(PCRE2_FOUND)
	set(PCRE2_LIBRARIES)
	set(PCRE2_INCLUDE_DIRS)
endif()

mark_as_advanced(PCRE2_INCLUDE_DIRS PCRE2_LIBRARIES)
