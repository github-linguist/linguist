# LIBSSH2_FOUND - system has the libssh2 library
# LIBSSH2_INCLUDE_DIR - the libssh2 include directory
# LIBSSH2_LIBRARY - the libssh2 library name

find_path(LIBSSH2_INCLUDE_DIR libssh2.h)

find_library(LIBSSH2_LIBRARY NAMES ssh2 libssh2)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LibSSH2
	REQUIRED_VARS LIBSSH2_LIBRARY LIBSSH2_INCLUDE_DIR)

mark_as_advanced(LIBSSH2_INCLUDE_DIR LIBSSH2_LIBRARY)
