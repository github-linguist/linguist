# ===========================================================================
#       http://www.gnu.org/software/autoconf-archive/ax_ruby_devel.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_RUBY_DEVEL([version])
#
# DESCRIPTION
#
#   This macro checks for Ruby and tries to get the include path to
#   'ruby.h'. It provides the $(RUBY_CPPFLAGS) and $(RUBY_LDFLAGS) output
#   variables. It also exports $(RUBY_EXTRA_LIBS) for embedding Ruby in your
#   code.
#
#   You can search for some particular version of Ruby by passing a
#   parameter to this macro, for example "1.8.6".
#
# LICENSE
#
#   Copyright (c) 2008 Rafal Rzepecki <divided.mind@gmail.com>
#   Copyright (c) 2008 Sebastian Huber <sebastian-huber@web.de>
#   Copyright (c) 2008 Alan W. Irwin
#   Copyright (c) 2008 Rafael Laboissiere <rafael@laboissiere.net>
#   Copyright (c) 2008 Andrew Collier
#   Copyright (c) 2008 Matteo Settenvini <matteo@member.fsf.org>
#   Copyright (c) 2008 Horst Knorr <hk_classes@knoda.org>
#
#   This program is free software: you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the
#   Free Software Foundation, either version 3 of the License, or (at your
#   option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
#   Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program. If not, see <http://www.gnu.org/licenses/>.
#
#   As a special exception, the respective Autoconf Macro's copyright owner
#   gives unlimited permission to copy, distribute and modify the configure
#   scripts that are the output of Autoconf when processing the Macro. You
#   need not follow the terms of the GNU General Public License when using
#   or distributing such scripts, even though portions of the text of the
#   Macro appear in them. The GNU General Public License (GPL) does govern
#   all other use of the material that constitutes the Autoconf Macro.
#
#   This special exception to the GPL applies to versions of the Autoconf
#   Macro released by the Autoconf Archive. When you make and distribute a
#   modified version of the Autoconf Macro, you may extend this special
#   exception to the GPL to apply to your modified version as well.

#serial 11

AC_DEFUN([AX_RUBY_DEVEL],[
    AC_REQUIRE([AX_WITH_RUBY])
    AS_IF([test -n "$1"], [AX_PROG_RUBY_VERSION([$1])])

    #
    # Check if you have mkmf, else fail
    #
    AC_MSG_CHECKING([for the mkmf Ruby package])
    ac_mkmf_result=`$RUBY -rmkmf -e ";" 2>&1`
    if test -z "$ac_mkmf_result"; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
        AC_MSG_ERROR([cannot import Ruby module "mkmf".
Please check your Ruby installation. The error was:
$ac_mkmf_result])
    fi

    #
    # Check for Ruby include path
    #
    AC_MSG_CHECKING([for Ruby include path])
    if test -z "$RUBY_CPPFLAGS"; then
        ruby_path=`$RUBY -rmkmf -e 'print Config::CONFIG[["archdir"]]'`
        if test -n "${ruby_path}"; then
                ruby_path="-I$ruby_path"
        fi
        RUBY_CPPFLAGS=$ruby_path
    fi
    AC_MSG_RESULT([$RUBY_CPPFLAGS])
    AC_SUBST([RUBY_CPPFLAGS])

    #
    # Check for Ruby library path
    #
    AC_MSG_CHECKING([for Ruby library path])
    if test -z "$RUBY_LDFLAGS"; then
        RUBY_LDFLAGS=`$RUBY -rmkmf -e 'print Config::CONFIG[["LIBRUBYARG_SHARED"]]'`
    fi
    AC_MSG_RESULT([$RUBY_LDFLAGS])
    AC_SUBST([RUBY_LDFLAGS])

    #
    # Check for site packages
    #
    AC_MSG_CHECKING([for Ruby site-packages path])
    if test -z "$RUBY_SITE_PKG"; then
        RUBY_SITE_PKG=`$RUBY -rmkmf -e 'print Config::CONFIG[["sitearchdir"]]'`
    fi
    AC_MSG_RESULT([$RUBY_SITE_PKG])
    AC_SUBST([RUBY_SITE_PKG])

    #
    # libraries which must be linked in when embedding
    #
    AC_MSG_CHECKING(ruby extra libraries)
    if test -z "$RUBY_EXTRA_LIBS"; then
       RUBY_EXTRA_LIBS=`$RUBY -rmkmf -e 'print Config::CONFIG[["SOLIBS"]]'`
    fi
    AC_MSG_RESULT([$RUBY_EXTRA_LIBS])
    AC_SUBST(RUBY_EXTRA_LIBS)

    #
    # linking flags needed when embedding
    # (is it even needed for Ruby?)
    #
    # AC_MSG_CHECKING(ruby extra linking flags)
    # if test -z "$RUBY_EXTRA_LDFLAGS"; then
    # RUBY_EXTRA_LDFLAGS=`$RUBY -rmkmf -e 'print Config::CONFIG[["LINKFORSHARED"]]'`
    # fi
    # AC_MSG_RESULT([$RUBY_EXTRA_LDFLAGS])
    # AC_SUBST(RUBY_EXTRA_LDFLAGS)

    # this flags breaks ruby.h, and is sometimes defined by KDE m4 macros
    CFLAGS="`echo "$CFLAGS" | sed -e 's/-std=iso9899:1990//g;'`"
    #
    # final check to see if everything compiles alright
    #
    AC_MSG_CHECKING([consistency of all components of ruby development environment])
    AC_LANG_PUSH([C])
    # save current global flags
    ac_save_LIBS="$LIBS"
    LIBS="$ac_save_LIBS $RUBY_LDFLAGS"
    ac_save_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$ac_save_CPPFLAGS $RUBY_CPPFLAGS"
    AC_TRY_LINK([
        #include <ruby.h>
    ],[
        ruby_init();
    ],[rubyexists=yes],[rubyexists=no])

    AC_MSG_RESULT([$rubyexists])

    if test ! "$rubyexists" = "yes"; then
       AC_MSG_ERROR([
  Could not link test program to Ruby. Maybe the main Ruby library has been
  installed in some non-standard library path. If so, pass it to configure,
  via the LDFLAGS environment variable.
  Example: ./configure LDFLAGS="-L/usr/non-standard-path/ruby/lib"
  ============================================================================
   ERROR!
   You probably have to install the development version of the Ruby package
   for your distribution.  The exact name of this package varies among them.
  ============================================================================
       ])
      RUBY_VERSION=""
    fi
    AC_LANG_POP
    # turn back to default flags
    CPPFLAGS="$ac_save_CPPFLAGS"
    LIBS="$ac_save_LIBS"

    #
    # all done!
    #
])
