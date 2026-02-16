find_program(DLLTOOL dlltool CMAKE_FIND_ROOT_PATH_BOTH)
if(NOT DLLTOOL)
	message(FATAL_ERROR "Could not find dlltool command")
endif()

set(LIBWINHTTP_PATH "${PROJECT_BINARY_DIR}/deps/winhttp")
set(LIBWINHTTP_PATH ${LIBWINHTTP_PATH} PARENT_SCOPE)
file(MAKE_DIRECTORY ${LIBWINHTTP_PATH})

if(CMAKE_SIZEOF_VOID_P EQUAL 8)
	set(WINHTTP_DEF "winhttp64.def")
else()
	set(WINHTTP_DEF "winhttp.def")
endif()

add_custom_command(
	OUTPUT ${LIBWINHTTP_PATH}/libwinhttp.a
	COMMAND ${DLLTOOL} -d ${WINHTTP_DEF} -k -D winhttp.dll -l libwinhttp.a
	DEPENDS ${WINHTTP_DEF}
	WORKING_DIRECTORY ${LIBWINHTTP_PATH})

set_source_files_properties(
	${CMAKE_CURRENT_SOURCE_DIR}/src/transports/winhttp.c
	PROPERTIES OBJECT_DEPENDS ${LIBWINHTTP_PATH}/libwinhttp.a)
