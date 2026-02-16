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
# Find the native PCRE headers and libraries.
#
# PCRE_INCLUDE_DIRS	- where to find pcre.h, etc.
# PCRE_LIBRARIES	- List of libraries when using pcre.
# PCRE_FOUND	- True if pcre found.

# Look for the header file.
find_path(PCRE_INCLUDE_DIR NAMES pcre.h)

# Look for the library.
find_library(PCRE_LIBRARY NAMES pcre)

# Handle the QUIETLY and REQUIRED arguments and set PCRE_FOUND to TRUE if all listed variables are TRUE.
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(PCRE DEFAULT_MSG PCRE_LIBRARY PCRE_INCLUDE_DIR)

# Copy the results to the output variables.
if(PCRE_FOUND)
	set(PCRE_LIBRARIES ${PCRE_LIBRARY})
	set(PCRE_INCLUDE_DIRS ${PCRE_INCLUDE_DIR})
else(PCRE_FOUND)
	set(PCRE_LIBRARIES)
	set(PCRE_INCLUDE_DIRS)
endif()

mark_as_advanced(PCRE_INCLUDE_DIRS PCRE_LIBRARIES)
