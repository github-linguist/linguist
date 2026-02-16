set(CLI_INCLUDES
	"${libgit2_BINARY_DIR}/src/util"
	"${libgit2_BINARY_DIR}/include"
	"${libgit2_SOURCE_DIR}/src/util"
	"${libgit2_SOURCE_DIR}/src/cli"
	"${libgit2_SOURCE_DIR}/include"
	"${LIBGIT2_DEPENDENCY_INCLUDES}"
	"${LIBGIT2_SYSTEM_INCLUDES}")

if(WIN32 AND NOT CYGWIN)
	file(GLOB CLI_SRC_OS win32/*.c)
	list(SORT CLI_SRC_OS)
else()
	file(GLOB CLI_SRC_OS unix/*.c)
	list(SORT CLI_SRC_OS)
endif()

file(GLOB CLI_SRC_C *.c *.h)
list(SORT CLI_SRC_C)

#
# The CLI currently needs to be statically linked against libgit2 because
# the utility library uses libgit2's thread-local error buffers.  TODO:
# remove this dependency and allow us to dynamically link against libgit2.
#

if(BUILD_CLI STREQUAL "dynamic")
	set(CLI_LIBGIT2_LIBRARY libgit2package)
else()
	set(CLI_LIBGIT2_OBJECTS $<TARGET_OBJECTS:libgit2>)
endif()

#
# Compile and link the CLI
#

add_executable(git2_cli ${CLI_SRC_C} ${CLI_SRC_OS} ${CLI_OBJECTS}
	$<TARGET_OBJECTS:util>
	${CLI_LIBGIT2_OBJECTS}
	${LIBGIT2_DEPENDENCY_OBJECTS})
target_link_libraries(git2_cli ${CLI_LIBGIT2_LIBRARY} ${LIBGIT2_SYSTEM_LIBS})

set_target_properties(git2_cli PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${libgit2_BINARY_DIR})
set_target_properties(git2_cli PROPERTIES OUTPUT_NAME ${LIBGIT2_FILENAME})

ide_split_sources(git2_cli)

target_include_directories(git2_cli PRIVATE ${CLI_INCLUDES})

if(MSVC_IDE)
	# Precompiled headers
	set_target_properties(git2_cli PROPERTIES COMPILE_FLAGS "/Yuprecompiled.h /FIprecompiled.h")
	set_source_files_properties(win32/precompiled.c COMPILE_FLAGS "/Ycprecompiled.h")
endif()

install(TARGETS git2_cli RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
