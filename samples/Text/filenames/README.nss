zlib data compression library

URL: http://zlib.net/
Version: 1.2.5
License: zlib License
License File: http://zlib.net/zlib_license.html

Description:

NSS uses zlib in libSSL (for the DEFLATE compression method), modutil, and
signtool.

Local Modifications:

- patches/prune-zlib.sh: run this shell script to remove unneeded files
  from the zlib distribution.
- patches/msvc-vsnprintf.patch: define HAVE_VSNPRINTF for Visual C++ 2008
  (9.0) and later.
