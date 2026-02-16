# Experimental feature support for libgit2 - developers can opt in to
# experimental functionality, like sha256 support. When experimental
# functionality is enabled, we set both a cmake flag *and* a compile
# definition. The cmake flag is used to generate `experimental.h`,
# which will be installed by a `make install`. But the compile definition
# is used by the libgit2 sources to detect the functionality at library
# build time. This allows us to have an in-tree `experimental.h` with
# *no* experiments enabled. This lets us support users who build without
# cmake and cannot generate the `experimental.h` file.

if(EXPERIMENTAL_SHA256)
	add_feature_info("SHA256 API" ON "experimental SHA256 APIs")

	set(EXPERIMENTAL 1)
	set(GIT_EXPERIMENTAL_SHA256 1)
	add_definitions(-DGIT_EXPERIMENTAL_SHA256=1)
else()
	add_feature_info("SHA256 API" OFF "experimental SHA256 APIs")
endif()

if(EXPERIMENTAL)
	set(LIBGIT2_FILENAME "${LIBGIT2_FILENAME}-experimental")
endif()
