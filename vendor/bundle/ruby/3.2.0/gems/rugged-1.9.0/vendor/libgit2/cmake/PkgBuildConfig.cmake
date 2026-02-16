# pkg-config file generation
#

function(pkg_build_config)
	set(options)
	set(oneValueArgs NAME DESCRIPTION VERSION FILENAME LIBS_SELF)
	set(multiValueArgs LIBS PRIVATE_LIBS REQUIRES CFLAGS)

	cmake_parse_arguments(PKGCONFIG "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

	if (NOT DEFINED PKGCONFIG_FILENAME AND DEFINED PKGCONFIG_NAME)
		set(PKGCONFIG_FILENAME ${PKGCONFIG_NAME})
	endif()
	if (NOT DEFINED PKGCONFIG_FILENAME)
		message(FATAL_ERROR "Missing FILENAME argument")
	endif()
	set(PKGCONFIG_FILE "${PROJECT_BINARY_DIR}/${PKGCONFIG_FILENAME}.pc")

	if (NOT DEFINED PKGCONFIG_DESCRIPTION)
		message(FATAL_ERROR "Missing DESCRIPTION argument")
	endif()

	if (NOT DEFINED PKGCONFIG_VERSION)
		message(FATAL_ERROR "Missing VERSION argument")
	endif()

	# Write .pc "header"
	file(WRITE "${PKGCONFIG_FILE}"
		"prefix=\"${CMAKE_INSTALL_PREFIX}\"\n"
		"libdir=\"${CMAKE_INSTALL_FULL_LIBDIR}\"\n"
		"includedir=\"${CMAKE_INSTALL_FULL_INCLUDEDIR}\"\n"
		"\n"
		"Name: ${PKGCONFIG_NAME}\n"
		"Description: ${PKGCONFIG_DESCRIPTION}\n"
		"Version: ${PKGCONFIG_VERSION}\n"
	)

	# Prepare Libs
	if(NOT DEFINED PKGCONFIG_LIBS_SELF)
		set(PKGCONFIG_LIBS_SELF "${PKGCONFIG_FILE}")
	endif()

	if(NOT DEFINED PKGCONFIG_LIBS)
		set(PKGCONFIG_LIBS "-l${PKGCONFIG_LIBS_SELF}")
	else()
		list(INSERT PKGCONFIG_LIBS 0 "-l${PKGCONFIG_LIBS_SELF}")
	endif()

	list(REMOVE_DUPLICATES PKGCONFIG_LIBS)
	string(REPLACE ";" " " PKGCONFIG_LIBS "${PKGCONFIG_LIBS}")
	file(APPEND "${PKGCONFIG_FILE}" "Libs: -L\${libdir} ${PKGCONFIG_LIBS}\n")

	# Prepare Libs.private
	if(DEFINED PKGCONFIG_PRIVATE_LIBS)
		list(REMOVE_DUPLICATES PKGCONFIG_PRIVATE_LIBS)
		string(REPLACE ";" " " PKGCONFIG_PRIVATE_LIBS "${PKGCONFIG_PRIVATE_LIBS}")
		file(APPEND "${PKGCONFIG_FILE}" "Libs.private: ${PKGCONFIG_PRIVATE_LIBS}\n")
	endif()

	# Prepare Requires.private
	if(DEFINED PKGCONFIG_REQUIRES)
		list(REMOVE_DUPLICATES PKGCONFIG_REQUIRES)
		string(REPLACE ";" " " PKGCONFIG_REQUIRES "${PKGCONFIG_REQUIRES}")
		file(APPEND "${PKGCONFIG_FILE}" "Requires.private: ${PKGCONFIG_REQUIRES}\n")
	endif()

	# Prepare Cflags
	if(DEFINED PKGCONFIG_CFLAGS)
		string(REPLACE ";" " " PKGCONFIG_CFLAGS "${PKGCONFIG_CFLAGS}")
	else()
		set(PKGCONFIG_CFLAGS "")
	endif()
	file(APPEND "${PKGCONFIG_FILE}" "Cflags: -I\${includedir} ${PKGCONFIG_CFLAGS}\n")

	# Install .pc file
	install(FILES "${PKGCONFIG_FILE}" DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig")
endfunction()
