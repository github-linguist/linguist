#
# Copyright OpenEmbedded Contributors
#
# SPDX-License-Identifier: MIT
#

# Path to the CMake file to process.
OECMAKE_SOURCEPATH ??= "${S}"

DEPENDS:prepend = "cmake-native "
B = "${WORKDIR}/build"

# What CMake generator to use.
# The supported options are "Unix Makefiles" or "Ninja".
OECMAKE_GENERATOR ?= "Ninja"

python() {
    generator = d.getVar("OECMAKE_GENERATOR")
    if "Unix Makefiles" in generator:
        args = "-G '" + generator +  "' -DCMAKE_MAKE_PROGRAM=" + d.getVar("MAKE")
        d.setVar("OECMAKE_GENERATOR_ARGS", args)
        d.setVarFlag("do_compile", "progress", "percent")
    elif "Ninja" in generator:
        args = "-G '" + generator + "' -DCMAKE_MAKE_PROGRAM=ninja"
        d.appendVar("DEPENDS", " ninja-native")
        d.setVar("OECMAKE_GENERATOR_ARGS", args)
        d.setVarFlag("do_compile", "progress", r"outof:^\[(\d+)/(\d+)\]\s+")
    else:
        bb.fatal("Unknown CMake Generator %s" % generator)
}
OECMAKE_AR ?= "${AR}"

# Compiler flags
OECMAKE_C_FLAGS ?= "${HOST_CC_ARCH} ${TOOLCHAIN_OPTIONS} ${CFLAGS}"
OECMAKE_CXX_FLAGS ?= "${HOST_CC_ARCH} ${TOOLCHAIN_OPTIONS} ${CXXFLAGS}"
OECMAKE_C_FLAGS_RELEASE ?= "-DNDEBUG"
OECMAKE_CXX_FLAGS_RELEASE ?= "-DNDEBUG"
OECMAKE_C_LINK_FLAGS ?= "${HOST_CC_ARCH} ${TOOLCHAIN_OPTIONS} ${CPPFLAGS} ${LDFLAGS}"
OECMAKE_CXX_LINK_FLAGS ?= "${HOST_CC_ARCH} ${TOOLCHAIN_OPTIONS} ${CXXFLAGS} ${LDFLAGS}"

def oecmake_map_compiler(compiler, d):
    args = d.getVar(compiler).split()
    if args[0] == "ccache":
        return args[1], args[0]
    return args[0], ""

# C/C++ Compiler (without cpu arch/tune arguments)
OECMAKE_C_COMPILER ?= "${@oecmake_map_compiler('CC', d)[0]}"
OECMAKE_C_COMPILER_LAUNCHER ?= "${@oecmake_map_compiler('CC', d)[1]}"
OECMAKE_CXX_COMPILER ?= "${@oecmake_map_compiler('CXX', d)[0]}"
OECMAKE_CXX_COMPILER_LAUNCHER ?= "${@oecmake_map_compiler('CXX', d)[1]}"

# clear compiler vars for allarch to avoid sig hash difference
OECMAKE_C_COMPILER:allarch = ""
OECMAKE_C_COMPILER_LAUNCHER:allarch = ""
OECMAKE_CXX_COMPILER:allarch = ""
OECMAKE_CXX_COMPILER_LAUNCHER:allarch = ""

OECMAKE_RPATH ?= ""
OECMAKE_PERLNATIVE_DIR ??= ""
OECMAKE_EXTRA_ROOT_PATH ?= ""

OECMAKE_FIND_ROOT_PATH_MODE_PROGRAM = "ONLY"

EXTRA_OECMAKE:append = " ${PACKAGECONFIG_CONFARGS}"

export CMAKE_BUILD_PARALLEL_LEVEL
CMAKE_BUILD_PARALLEL_LEVEL:task-compile = "${@oe.utils.parallel_make(d, False)}"
CMAKE_BUILD_PARALLEL_LEVEL:task-install = "${@oe.utils.parallel_make(d, True)}"

OECMAKE_TARGET_COMPILE ?= "all"
OECMAKE_TARGET_INSTALL ?= "install"

def map_host_os_to_system_name(host_os):
    if host_os.startswith('mingw'):
        return 'Windows'
    if host_os.startswith('linux'):
        return 'Linux'
    return host_os

# CMake expects target architectures in the format of uname(2),
# which do not always match TARGET_ARCH, so all the necessary
# conversions should happen here.
def map_host_arch_to_uname_arch(host_arch):
    if host_arch == "powerpc":
        return "ppc"
    if host_arch == "powerpc64le":
        return "ppc64le"
    if host_arch == "powerpc64":
        return "ppc64"
    return host_arch


cmake_do_generate_toolchain_file() {
	if [ "${BUILD_SYS}" = "${HOST_SYS}" ]; then
		cmake_crosscompiling="set( CMAKE_CROSSCOMPILING FALSE )"
	else
		cmake_sysroot="set( CMAKE_SYSROOT \"${RECIPE_SYSROOT}\" )"
	fi

	cat > ${WORKDIR}/toolchain.cmake <<EOF
# CMake system name must be something like "Linux".
# This is important for cross-compiling.
$cmake_crosscompiling
set( CMAKE_SYSTEM_NAME ${@map_host_os_to_system_name(d.getVar('HOST_OS'))} )
set( CMAKE_SYSTEM_PROCESSOR ${@map_host_arch_to_uname_arch(d.getVar('HOST_ARCH'))} )
set( CMAKE_C_COMPILER ${OECMAKE_C_COMPILER} )
set( CMAKE_CXX_COMPILER ${OECMAKE_CXX_COMPILER} )
set( CMAKE_C_COMPILER_LAUNCHER ${OECMAKE_C_COMPILER_LAUNCHER} )
set( CMAKE_CXX_COMPILER_LAUNCHER ${OECMAKE_CXX_COMPILER_LAUNCHER} )
set( CMAKE_ASM_COMPILER ${OECMAKE_C_COMPILER} )
find_program( CMAKE_AR ${OECMAKE_AR} DOC "Archiver" REQUIRED )

set( CMAKE_C_FLAGS "${OECMAKE_C_FLAGS}" CACHE STRING "CFLAGS" )
set( CMAKE_CXX_FLAGS "${OECMAKE_CXX_FLAGS}" CACHE STRING "CXXFLAGS" )
set( CMAKE_ASM_FLAGS "${OECMAKE_C_FLAGS}" CACHE STRING "ASM FLAGS" )
set( CMAKE_C_FLAGS_RELEASE "${OECMAKE_C_FLAGS_RELEASE}" CACHE STRING "Additional CFLAGS for release" )
set( CMAKE_CXX_FLAGS_RELEASE "${OECMAKE_CXX_FLAGS_RELEASE}" CACHE STRING "Additional CXXFLAGS for release" )
set( CMAKE_ASM_FLAGS_RELEASE "${OECMAKE_C_FLAGS_RELEASE}" CACHE STRING "Additional ASM FLAGS for release" )
set( CMAKE_C_LINK_FLAGS "${OECMAKE_C_LINK_FLAGS}" CACHE STRING "LDFLAGS" )
set( CMAKE_CXX_LINK_FLAGS "${OECMAKE_CXX_LINK_FLAGS}" CACHE STRING "LDFLAGS" )

# only search in the paths provided so cmake doesnt pick
# up libraries and tools from the native build machine
set( CMAKE_FIND_ROOT_PATH ${STAGING_DIR_HOST} ${STAGING_DIR_NATIVE} ${CROSS_DIR} ${OECMAKE_PERLNATIVE_DIR} ${OECMAKE_EXTRA_ROOT_PATH} ${EXTERNAL_TOOLCHAIN} ${HOSTTOOLS_DIR})
set( CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY )
set( CMAKE_FIND_ROOT_PATH_MODE_PROGRAM ${OECMAKE_FIND_ROOT_PATH_MODE_PROGRAM} )
set( CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY )
set( CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY )
set( CMAKE_PROGRAM_PATH "/" )

$cmake_sysroot

# Use qt.conf settings
set( ENV{QT_CONF_PATH} ${WORKDIR}/qt.conf )

# We need to set the rpath to the correct directory as cmake does not provide any
# directory as rpath by default
set( CMAKE_INSTALL_RPATH ${OECMAKE_RPATH} )

# Use RPATHs relative to build directory for reproducibility
set( CMAKE_BUILD_RPATH_USE_ORIGIN ON )

# Use our cmake modules
list(APPEND CMAKE_MODULE_PATH "${STAGING_DATADIR}/cmake/Modules/")

# add for non /usr/lib libdir, e.g. /usr/lib64
set( CMAKE_LIBRARY_PATH ${libdir} ${base_libdir})

# add include dir to implicit includes in case it differs from /usr/include
list(APPEND CMAKE_C_IMPLICIT_INCLUDE_DIRECTORIES ${includedir})
list(APPEND CMAKE_CXX_IMPLICIT_INCLUDE_DIRECTORIES ${includedir})

EOF
}

addtask generate_toolchain_file after do_patch before do_configure

CONFIGURE_FILES = "CMakeLists.txt *.cmake"

do_configure[cleandirs] = "${@d.getVar('B') if d.getVar('S') != d.getVar('B') else ''}"

OECMAKE_ARGS = "\
    -DCMAKE_INSTALL_PREFIX:PATH=${prefix} \
    -DCMAKE_INSTALL_BINDIR:PATH=${@os.path.relpath(d.getVar('bindir'), d.getVar('prefix') + '/')} \
    -DCMAKE_INSTALL_SBINDIR:PATH=${@os.path.relpath(d.getVar('sbindir'), d.getVar('prefix') + '/')} \
    -DCMAKE_INSTALL_LIBEXECDIR:PATH=${@os.path.relpath(d.getVar('libexecdir'), d.getVar('prefix') + '/')} \
    -DCMAKE_INSTALL_SYSCONFDIR:PATH=${sysconfdir} \
    -DCMAKE_INSTALL_SHAREDSTATEDIR:PATH=${@os.path.relpath(d.getVar('sharedstatedir'), d.  getVar('prefix') + '/')} \
    -DCMAKE_INSTALL_LOCALSTATEDIR:PATH=${localstatedir} \
    -DCMAKE_INSTALL_LIBDIR:PATH=${@os.path.relpath(d.getVar('libdir'), d.getVar('prefix') + '/')} \
    -DCMAKE_INSTALL_INCLUDEDIR:PATH=${@os.path.relpath(d.getVar('includedir'), d.getVar('prefix') + '/')} \
    -DCMAKE_INSTALL_DATAROOTDIR:PATH=${@os.path.relpath(d.getVar('datadir'), d.getVar('prefix') + '/')} \
    -DPYTHON_EXECUTABLE:PATH=${PYTHON} \
    -DPython_EXECUTABLE:PATH=${PYTHON} \
    -DPython3_EXECUTABLE:PATH=${PYTHON} \
    -DLIB_SUFFIX=${@d.getVar('baselib').replace('lib', '')} \
    -DCMAKE_INSTALL_SO_NO_EXE=0 \
    -DCMAKE_TOOLCHAIN_FILE:FILEPATH=${WORKDIR}/toolchain.cmake \
    -DCMAKE_NO_SYSTEM_FROM_IMPORTED=1 \
    -DCMAKE_EXPORT_NO_PACKAGE_REGISTRY=ON \
    -DFETCHCONTENT_FULLY_DISCONNECTED=ON \
    -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=ON \
"

cmake_do_configure() {
	if [ "${OECMAKE_BUILDPATH}" ]; then
		bbnote "cmake.bbclass no longer uses OECMAKE_BUILDPATH.  The default behaviour is now out-of-tree builds with B=WORKDIR/build."
	fi

	if [ "${S}" = "${B}" ]; then
		find ${B} -name CMakeFiles -or -name Makefile -or -name cmake_install.cmake -or -name CMakeCache.txt -delete
	fi

	# Just like autotools cmake can use a site file to cache result that need generated binaries to run
	if [ -e ${WORKDIR}/site-file.cmake ] ; then
		oecmake_sitefile="-C ${WORKDIR}/site-file.cmake"
	else
		oecmake_sitefile=
	fi

	cmake \
	  ${OECMAKE_GENERATOR_ARGS} \
	  $oecmake_sitefile \
	  ${OECMAKE_SOURCEPATH} \
	  ${OECMAKE_ARGS} \
	  ${EXTRA_OECMAKE} \
	  -Wno-dev
}

# To disable verbose cmake logs for a given recipe or globally config metadata e.g. local.conf
# add following
#
# CMAKE_VERBOSE = ""
#

CMAKE_VERBOSE ??= "VERBOSE=1"

# Then run do_compile again
cmake_runcmake_build() {
	bbnote ${DESTDIR:+DESTDIR=${DESTDIR} }${CMAKE_VERBOSE} cmake --build '${B}' "$@" -- ${EXTRA_OECMAKE_BUILD}
	eval ${DESTDIR:+DESTDIR=${DESTDIR} }${CMAKE_VERBOSE} cmake --build '${B}' "$@" -- ${EXTRA_OECMAKE_BUILD}
}

cmake_do_compile()  {
	cmake_runcmake_build --target ${OECMAKE_TARGET_COMPILE}
}

cmake_do_install() {
	DESTDIR='${D}' cmake_runcmake_build --target ${OECMAKE_TARGET_INSTALL}
}

EXPORT_FUNCTIONS do_configure do_compile do_install do_generate_toolchain_file
