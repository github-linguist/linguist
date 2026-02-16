#----------------------------------------------------------------
# Generated CMake target import file for configuration "RelWithDebInfo".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "libgit2::libgit2package" for configuration "RelWithDebInfo"
set_property(TARGET libgit2::libgit2package APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(libgit2::libgit2package PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELWITHDEBINFO "C"
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/lib/libgit2.a"
  )

list(APPEND _cmake_import_check_targets libgit2::libgit2package )
list(APPEND _cmake_import_check_files_for_libgit2::libgit2package "${_IMPORT_PREFIX}/lib/libgit2.a" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
