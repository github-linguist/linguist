#
# Newlib configuration
#

LIBCEXTENSION = "-newlib"
LIBCOVERRIDE = ":libc-newlib"

PREFERRED_PROVIDER_virtual/libc ?= "newlib"
PREFERRED_PROVIDER_virtual/libiconv ?= "newlib"
PREFERRED_PROVIDER_virtual/libintl ?= "newlib"
PREFERRED_PROVIDER_virtual/nativesdk-libintl ?= "nativesdk-glibc"
PREFERRED_PROVIDER_virtual/nativesdk-libiconv ?= "nativesdk-glibc"

DISTRO_FEATURES_BACKFILL_CONSIDERED += "ldconfig"

#USE_NLS ?= "no"

IMAGE_LINGUAS = ""

LIBC_DEPENDENCIES = "\
    newlib-dbg \
    newlib-dev \
    libgloss \
    libgloss-dev \
    libgloss-dbg \
    libgcc-dev \
    libgcc-dbg \
    libstdc++-dev \
    libstdc++-staticdev \
    "

ASSUME_PROVIDED += "virtual/crypt"

# Its useful to be able to extend newlib, but we dont provide a native variant of libgloss
NEWLIB_EXTENDED ?=  "libgloss libgcc"
BASE_DEFAULT_DEPS:append:class-target = " ${NEWLIB_EXTENDED}"

TARGET_OS = "elf"
TARGET_OS:arm = "eabi"

TOOLCHAIN_HOST_TASK ?= "packagegroup-cross-canadian-${MACHINE} nativesdk-qemu nativesdk-sdk-provides-dummy"
TOOLCHAIN_TARGET_TASK ?= "${LIBC_DEPENDENCIES}"
TOOLCHAIN_NEED_CONFIGSITE_CACHE:remove = "zlib ncurses"

# disable pie security flags by default
SECURITY_CFLAGS:libc-newlib = "${SECURITY_NOPIE_CFLAGS}"
SECURITY_LDFLAGS:libc-newlib = ""
