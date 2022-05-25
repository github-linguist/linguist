Source: e2fsprogs
Section: admin
Priority: required
Maintainer: Ubuntu Developers <ubuntu-devel-discuss@lists.ubuntu.com>
XSBC-Original-Maintainer: Theodore Y. Ts'o <tytso@mit.edu>
Build-Depends: texi2html (>= 1.76), gettext:any, texinfo, pkg-config, gcc-multilib [mips mipsel], debhelper (>= 7.0), libblkid-dev (>= 2.16), uuid-dev (>= 2.16), m4
Standards-Version: 3.9.2
Homepage: http://e2fsprogs.sourceforge.net

Package: e2fsprogs
Essential: yes
Pre-Depends: ${shlibs:Depends}, ${misc:Depends}, util-linux (>= 2.15~rc1-1)
Suggests: gpart, parted, e2fsck-static
Conflicts: dump (<< 0.4b4-4), quota (<< 1.55-8.1), initscripts (<< 2.85-4), sysvinit (<< 2.85-4)
Replaces: hurd (<= 20040301-1), libblkid1 (<< 1.38+1.39-WIP-2005.12.10-2), libuuid1 (<< 1.38+1.39-WIP-2005.12.10-2)
Architecture: any
Description: ext2/ext3/ext4 file system utilities
 The ext2, ext3 and ext4 file systems are successors of the original ext
 ("extended") file system. They are the main file system types used for
 hard disks on Debian and other Linux systems.
 .
 This package contains programs for creating, checking, and maintaining
 ext2/3/4-based file systems.