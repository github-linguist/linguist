#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.68)
AC_INIT([GARDEN], 
	[1.0.9], 
	[bubla@users.sourceforge.net])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([-Wall])
AC_CONFIG_SRCDIR([src/input.h])
AC_CONFIG_HEADERS([src/configure.h])
AC_CONFIG_MACRO_DIR([m4])

AC_ARG_ENABLE([debug],
	      [AS_HELP_STRING([--enable-debug],
			   [Builds the debug version of the library [[default = no]]]) ],
	      [],
	      [enable_debug="no"])

AS_IF([test "x$enable_debug" = "xyes"],
      [CFLAGS="${CFLAGS} -g -O0"])

# Checks for programs.
AC_PROG_CC
AC_PROG_LIBTOOL
LT_PROG_RC

AC_CANONICAL_HOST

dnl
dnl Check whether it makes sense to install a garden.desktop file
dnl
AC_CHECK_PROG([have_freedesktop],
	      [update-desktop-database],
	      [yes])

AM_CONDITIONAL([HAVE_FREEDESKTOP],
	       [test "x$have_freedesktop" = "xyes"])

AM_CONDITIONAL([WANT_FREEDESKTOP],
	       [test "x$enable_desktop_install" = "xyes"])

AC_ARG_ENABLE([desktop-install],
	      [AS_HELP_STRING([--enable-desktop-install],
			      [Whether you want to install the garden.desktop file if applicable. !!! DO NOT USE if you are a PACKAGER!!!])]
	      )

AS_CASE([$host],
        [*mingw* | *cygwin*],
        [AC_DEFINE([WINDOWS_VERSION],
 		   [1],
		   [Define when building for Windows])
         windows_version="yes"])

AM_CONDITIONAL([WINDOWS_VERSION],
	       [test "x$windows_version" = "xyes" ])

dnl now the datadir specification, that is useful if one does want to play without installing
AC_ARG_ENABLE([datadir],
	      [AS_HELP_STRING([--enable-datadir=path-to-your-garden-datafiles],
			      [Normally you dont have to use this, but it is handy when you want to play the game without installing it or if you want to use already installed data. In the first case, use for instance --enable-datadir=`pwd`/data (if it makes sense)])], 
	      [DATADIR_NAME="$enableval"],
	      [DATADIR_NAME='$(datadir)/'garden])

# Sets the data subdirectory
AC_SUBST([DATADIR_NAME])

# Checks for libraries.
AC_CHECK_HEADER([allegro.h],
		[],
		[have_allegro="no"
		 AC_MSG_ERROR([You don't even have Allegro headers... Get Allegro first!]) ])

AC_CHECK_LIB([m], [sin])

test "x$host_os" != "x$build_os" && CROSS_COMPILING="yes"

try_link_allegro ()
{
	LIBS_SAVE=$LIBS
	LIBS="$LIBS $1"
	AC_MSG_CHECKING([for Allegro using $1])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT
					 [ #include <allegro.h> ]],
					[allegro_init();])END_OF_MAIN() ],
		       [have_allegro="yes"],
		       [have_allegro="no"])
	AC_MSG_RESULT([$have_allegro])
	LIBS=$LIBS_SAVE
	AS_IF([test "x$have_allegro" == "xyes"],
	      [return 0],
	      [return 1])
}
# first check for 'official allegro'
# The official allegro does not support cross-compiling, though...
AM_PATH_ALLEGRO([4.2.0]) 

try_link_allegro "$allegro_LIBS"
AS_IF([test "x$?" == "x0"],
      [[LIBS="$LIBS $allegro_LIBS"
       ALLEGRO_LIB=`echo $allegro_LIBS | sed -e 's/.*-l\([^[:blank:]]*\).*/\1/'`
       have_allegro="yes"]])

ALLEGRO_RELEASE_LIBS="alleg alleg42 alleg44"
ALLEGRO_DEBUG_LIBS="alld42 alleg44-debug"
AS_IF([test "x$enable_debug" = "xyes"],
      [ALLEGRO_LIBS="$ALLEGRO_DEBUG_LIBS $ALLEGRO_RELEASE_LIBS"],
      [ALLEGRO_LIBS="$ALLEGRO_RELEASE_LIBS $ALLEGRO_DEBUG_LIBS"])

for lib in $ALLEGRO_LIBS
do
	ldflag="-l$lib"
	AS_IF([test "x$have_allegro" == "xyes"],
	      [break])
	try_link_allegro $ldflag
	AS_IF([test "x$?" == "x0"],
	      [LIBS="$LIBS $ldflag"
	       ALLEGRO_LIB="$lib"
	       have_allegro="yes"])
done

AS_IF([test "x$have_allegro" != "xyes"],
      [AC_MSG_ERROR([Unable to find Allegro game programming library 4.2, check out www.allegro.cc (or your distro repositories if you use a unix-like system)]) ])

# Checks for header files.
AC_CHECK_HEADERS([string.h sys/stat.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE
AC_HEADER_STDBOOL

# Checks for library functions.

AC_SUBST([ALLEGRO_LIB])

AC_CONFIG_FILES([Makefile
                 src/Makefile
		 data/Makefile
		 resources/Makefile
	 	 docs/garden.doxyfile
		 pkgs/w32/winstaller.nsi])

AC_OUTPUT
