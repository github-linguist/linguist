SUMMARY = "X driver"
HOMEPAGE = "http://www.x.org"
BUGTRACKER = "https://bugs.freedesktop.org"
SECTION = "x11/drivers"
LICENSE = "MIT"

PE = "2"

DEPENDS = "virtual/xserver xorgproto util-macros"

XORG_DRIVER_COMPRESSOR ?= ".tar.bz2"
SRC_URI = "${XORG_MIRROR}/individual/driver/${BPN}-${PV}${XORG_DRIVER_COMPRESSOR}"

FILES:${PN} += " ${libdir}/xorg/modules/drivers/*.so"

XORGBUILDCLASS ??= "autotools"
inherit ${XORGBUILDCLASS} pkgconfig features_check

# depends on virtual/xserver
REQUIRED_DISTRO_FEATURES = "x11"

# FIXME: We don't want to include the libtool archives (*.la) from modules
# directory, as they serve no useful purpose. Upstream should fix Makefile.am
do_install:append() {
	find ${D}${libdir}/xorg/modules -regex ".*\.la$" | xargs rm -f --
}

# Function to add the relevant ABI dependency to drivers, which should be called
# from a PACKAGEFUNC.
def _add_xorg_abi_depends(d, name):
    # Map of ABI names exposed in the dependencies to pkg-config variables
    abis = {
      "video": "abi_videodrv",
      "input": "abi_xinput"
    }

    output = os.popen("pkg-config xorg-server --variable=%s" % abis[name]).read()
    mlprefix = d.getVar('MLPREFIX') or ''
    abi = "%sxorg-abi-%s-%s" % (mlprefix, name, output.split(".")[0])

    pn = d.getVar("PN")
    d.appendVar('RDEPENDS:' + pn, ' ' + abi)

SECURITY_LDFLAGS = "${SECURITY_X_LDFLAGS}"
