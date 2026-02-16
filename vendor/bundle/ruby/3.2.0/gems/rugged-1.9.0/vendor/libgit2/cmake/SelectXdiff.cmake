# Optional external dependency: xdiff
if(USE_XDIFF STREQUAL "system")
	message(FATAL_ERROR "external/system xdiff is not yet supported")
else()
	add_subdirectory("${PROJECT_SOURCE_DIR}/deps/xdiff" "${PROJECT_BINARY_DIR}/deps/xdiff")
	list(APPEND LIBGIT2_DEPENDENCY_INCLUDES "${PROJECT_SOURCE_DIR}/deps/xdiff")
	list(APPEND LIBGIT2_DEPENDENCY_OBJECTS "$<TARGET_OBJECTS:xdiff>")
	add_feature_info(xdiff ON "xdiff support (bundled)")
endif()
