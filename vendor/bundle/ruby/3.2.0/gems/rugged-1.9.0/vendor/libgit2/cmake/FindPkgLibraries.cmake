include(FindPkgConfig)

# This function will find and set up a pkg-config based module.
# If a pc-file was found, it will resolve library paths to
# absolute paths. Furthermore, the function will automatically
# fall back to use static libraries in case no dynamic libraries
# were found.
function(FIND_PKGLIBRARIES prefix package)
	pkg_check_modules(${prefix} ${package})
	if(NOT ${prefix}_FOUND)
		return()
	endif()

	foreach(LIBRARY ${${prefix}_LIBRARIES})
		find_library(${LIBRARY}_RESOLVED ${LIBRARY} PATHS ${${prefix}_LIBRARY_DIRS})
		if(${${LIBRARY}_RESOLVED} STREQUAL "${LIBRARY}_RESOLVED-NOTFOUND")
			message(FATAL_ERROR "could not resolve ${LIBRARY}")
		endif()
		list(APPEND RESOLVED_LIBRARIES ${${LIBRARY}_RESOLVED})
	endforeach()

	set(${prefix}_FOUND 1 PARENT_SCOPE)
	set(${prefix}_LIBRARIES ${RESOLVED_LIBRARIES} PARENT_SCOPE)
	set(${prefix}_INCLUDE_DIRS ${${prefix}_INCLUDE_DIRS} PARENT_SCOPE)
	set(${prefix}_LDFLAGS ${${prefix}_LDFLAGS} PARENT_SCOPE)

	message(STATUS "  Resolved libraries: ${RESOLVED_LIBRARIES}")
endfunction()
