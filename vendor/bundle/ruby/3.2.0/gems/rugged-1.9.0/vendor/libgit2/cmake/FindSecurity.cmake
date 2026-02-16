# Find Security.framework
# This will define :
#
# SECURITY_FOUND
# SECURITY_LIBRARIES
# SECURITY_LDFLAGS
# SECURITY_HAS_SSLCREATECONTEXT
#

find_path(SECURITY_INCLUDE_DIR NAMES Security/Security.h)
find_library(SECURITY_LIBRARIES NAMES Security)
if(SECURITY_INCLUDE_DIR AND SECURITY_LIBRARIES)
	if(NOT Security_FIND_QUIETLY)
		message(STATUS "Found Security ${SECURITY_LIBRARIES}")
	endif()
	set(SECURITY_FOUND TRUE)
	set(SECURITY_LDFLAGS "-framework Security")
	check_library_exists("${SECURITY_LIBRARIES}" SSLCreateContext "Security/SecureTransport.h" SECURITY_HAS_SSLCREATECONTEXT)
endif()

if(Security_FIND_REQUIRED AND NOT SECURITY_FOUND)
	message(FATAL_ERROR "Security not found")
endif()

mark_as_advanced(
	SECURITY_INCLUDE_DIR
	SECURITY_LIBRARIES
)
