
file(GLOB SRC_XDIFF "*.c" "*.h")
list(SORT SRC_XDIFF)

add_library(xdiff OBJECT ${SRC_XDIFF})
target_include_directories(xdiff SYSTEM PRIVATE
	"${PROJECT_SOURCE_DIR}/include"
	"${PROJECT_SOURCE_DIR}/src/util"
	"${PROJECT_BINARY_DIR}/src/util"
	${LIBGIT2_SYSTEM_INCLUDES}
	${LIBGIT2_DEPENDENCY_INCLUDES})

# the xdiff dependency is not (yet) warning-free, disable warnings
# as errors for the xdiff sources until we've sorted them out
if(MSVC)
        set_source_files_properties(xdiffi.c PROPERTIES COMPILE_FLAGS -WX-)
        set_source_files_properties(xemit.c PROPERTIES COMPILE_FLAGS -WX-)
        set_source_files_properties(xhistogram.c PROPERTIES COMPILE_FLAGS -WX-)
        set_source_files_properties(xmerge.c PROPERTIES COMPILE_FLAGS -WX-)
        set_source_files_properties(xutils.c PROPERTIES COMPILE_FLAGS -WX-)
        set_source_files_properties(xpatience.c PROPERTIES COMPILE_FLAGS -WX-)
else()
        set_source_files_properties(xdiffi.c PROPERTIES COMPILE_FLAGS "-Wno-sign-compare -Wno-unused-parameter")
        set_source_files_properties(xemit.c PROPERTIES COMPILE_FLAGS "-Wno-sign-compare -Wno-unused-parameter")
        set_source_files_properties(xhistogram.c PROPERTIES COMPILE_FLAGS "-Wno-sign-compare")
        set_source_files_properties(xutils.c PROPERTIES COMPILE_FLAGS "-Wno-sign-compare")
        set_source_files_properties(xpatience.c PROPERTIES COMPILE_FLAGS "-Wno-sign-compare")
endif()
