cmake_minimum_required (VERSION 2.6)

set (CMAKE_RUNTIME_OUTPUT_DIRECTORY "bin")

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/vala)
find_package(Vala REQUIRED)
include(ValaPrecompile)
include(ValaVersion)
ensure_vala_version("0.11.0" MINIMUM)

project (template C)

find_package(PkgConfig)

pkg_check_modules(GOBJECT REQUIRED gobject-2.0)
add_definitions(${GOBJECT_CFLAGS} ${GOBJECT_CFLAGS_OTHER})
link_libraries(${GOBJECT_LIBRARIES})
link_directories(${GOBJECT_LIBRARY_DIRS})


vala_precompile(VALA_C
    src/template.vala
PACKAGES
OPTIONS
    --thread
CUSTOM_VAPIS
GENERATE_VAPI
GENERATE_HEADER
DIRECTORY
	gen
)

add_executable("template" ${VALA_C})
