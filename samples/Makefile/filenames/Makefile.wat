#
#                     The FreeType Project LICENSE
#                     ----------------------------
# 
#                        Copyright 1996-1999 by
#           David Turner, Robert Wilhelm, and Werner Lemberg
# 
# 
# 
# Introduction
# ============
# 
#   The FreeType  Project is distributed in  several archive packages;
#   some of them may contain, in addition to the FreeType font engine,
#   various tools and  contributions which rely on, or  relate to, the
#   FreeType Project.
# 
#   This  license applies  to all  files found  in such  packages, and
#   which do not  fall under their own explicit  license.  The license
#   affects  thus  the  FreeType   font  engine,  the  test  programs,
#   documentation and makefiles, at the very least.
# 
#   This  license   was  inspired  by  the  BSD,   Artistic,  and  IJG
#   (Independent JPEG  Group) licenses, which  all encourage inclusion
#   and  use of  free  software in  commercial  and freeware  products
#   alike.  As a consequence, its main points are that:
# 
#     o We don't promise that this software works.  However, we are be
#       interested in any kind of bug reports. (`as is' distribution)
# 
#     o You can  use this software for whatever you  want, in parts or
#       full form, without having to pay us. (`royalty-free' usage)
# 
#     o You may not pretend that  you wrote this software.  If you use
#       it, or  only parts of it,  in a program,  you must acknowledge
#       somewhere in your documentation  that you've used the FreeType
#       code. (`credits')
# 
#   We  specifically  permit  and  encourage  the  inclusion  of  this
#   software,  with  or without modifications, in commercial products,
#   provided that all warranty or  liability claims are assumed by the
#   product vendor.
# 
# 
# Legal Terms
# ===========
# 
# 0. Definitions
# --------------
# 
#   Throughout this license,  the terms `package', `FreeType Project',
#   and  `FreeType  archive' refer  to  the  set  of files  originally
#   distributed  by the  authors  (David Turner,  Robert Wilhelm,  and
#   Werner Lemberg) as the `FreeType project', be they named as alpha,
#   beta or final release.
# 
#   `You' refers to  the licensee, or person using  the project, where
#   `using' is a generic term including compiling the project's source
#   code as  well as linking it  to form a  `program' or `executable'.
#   This  program is  referred to  as  `a program  using the  FreeType
#   engine'.
# 
#   This  license applies  to all  files distributed  in  the original
#   FreeType  archive,   including  all  source   code,  binaries  and
#   documentation,  unless  otherwise  stated   in  the  file  in  its
#   original, unmodified form as  distributed in the original archive.
#   If you are  unsure whether or not a particular  file is covered by
#   this license, you must contact us to verify this.
# 
#   The FreeType  project is copyright (C) 1996-1999  by David Turner,
#   Robert Wilhelm, and Werner Lemberg.  All rights reserved except as
#   specified below.
# 
# 1. No Warranty
# --------------
# 
#   THE FREETYPE ARCHIVE  IS PROVIDED `AS IS' WITHOUT  WARRANTY OF ANY
#   KIND, EITHER EXPRESSED OR  IMPLIED, INCLUDING, BUT NOT LIMITED TO,
#   WARRANTIES  OF  MERCHANTABILITY   AND  FITNESS  FOR  A  PARTICULAR
#   PURPOSE.  IN NO EVENT WILL ANY OF THE AUTHORS OR COPYRIGHT HOLDERS
#   BE LIABLE  FOR ANY DAMAGES CAUSED  BY THE USE OR  THE INABILITY TO
#   USE, OF THE FREETYPE PROJECT.
# 
#   As  you have  not signed  this license,  you are  not  required to
#   accept  it.   However,  as  the FreeType  project  is  copyrighted
#   material, only  this license, or  another one contracted  with the
#   authors, grants you  the right to use, distribute,  and modify it.
#   Therefore,  by  using,  distributing,  or modifying  the  FreeType
#   project, you indicate that you understand and accept all the terms
#   of this license.
# 
# 2. Redistribution
# -----------------
# 
#   Redistribution and use in source and binary forms, with or without
#   modification, are permitted provided that the following conditions
#   are met:
# 
#     o Redistribution  of source code  must retain this  license file
#       (`licence.txt') unaltered; any additions, deletions or changes
#       to   the  original   files  must   be  clearly   indicated  in
#       accompanying  documentation.   The  copyright notices  of  the
#       unaltered, original  files must be preserved in  all copies of
#       source files.
# 
#     o Redistribution in binary form must provide a  disclaimer  that
#       states  that  the software is based in part of the work of the
#       FreeType Team,  in  the  distribution  documentation.  We also
#       encourage you to put an URL to the FreeType web page  in  your
#       documentation, though this isn't mandatory.
# 
#   These conditions  apply to any  software derived from or  based on
#   the FreeType code, not just  the unmodified files.  If you use our
#   work, you  must acknowledge us.  However,  no fee need  be paid to
#   us.
# 
# 3. Advertising
# --------------
# 
#   The names of  FreeType's authors and contributors may  not be used
#   to endorse or promote  products derived from this software without
#   specific prior written permission.
#   
#   We suggest,  but do not require, that  you use one or  more of the
#   following phrases to refer  to this software in your documentation
#   or advertising  materials: `FreeType Project',  `FreeType Engine',
#   `FreeType library', or `FreeType Distribution'.
# 
# 4. Contacts
# -----------
# 
#   There are two mailing lists related to FreeType:
# 
#     o freetype@freetype.org
# 
#       Discusses general use and applications of FreeType, as well as
#       future and  wanted additions to the  library and distribution.
#       If  you are looking  for support,  start in  this list  if you
#       haven't found anything to help you in the documentation.
# 
#     o devel@freetype.org
# 
#       Discusses bugs,  as well  as engine internals,  design issues,
#       specific licenses, porting, etc.
# 
#     o http://www.freetype.org
# 
#       Holds the current  FreeType web page, which will  allow you to
#       download  our  latest  development  version  and  read  online
#       documentation.
# 
#   You can also contact us individually at:
# 
#     David Turner      <david.turner@freetype.org>
#     Robert Wilhelm    <robert.wilhelm@freetype.org>
#     Werner Lemberg    <werner.lemberg@freetype.org>
# 
# 
# --- end of license ---
#
# This file is part of the FreeType project.
#
# This builds the Watcom library with Watcom's wcc386 under OS/2.
#
# You'll need Watcom's wmake.
#
#
# Invoke by "wmake -f arch\os2\Makefile.wat" when in the "lib" directory
#
# This will build "freetype\lib\libttf.lib"

ARCH = arch\os2
FT_MAKEFILE = $(ARCH)\Makefile.wat
FT_MAKE = wmake -h


.EXTENSIONS:
.EXTENSIONS: .lib .obj .c .h
.obj:.;.\extend;.\$(ARCH)
.c:.;.\extend;.\$(ARCH)
.h:.;.\extend;.\$(ARCH)

CC = wcc386

CCFLAGS = /otexanl+ /s /w5 /zq -Iarch\os2 -I. -Iextend

TTFILE   = .\ttfile.c
TTMEMORY = .\ttmemory.c
TTMUTEX  = .\ttmutex.c

TTFILE_OBJ = ttfile.obj
TTMEMORY_OBJ = ttmemory.obj
TTMUTEX_OBJ = ttmutex.obj

PORT = $(TTFILE) $(TTMEMORY) $(TTMUTEX)
PORT_OBJS = $(TTFILE_OBJ) $(TTMEMORY_OBJ) $(TTMUTEX_OBJ)

SRC_X = extend\ftxgasp.c extend\ftxkern.c  extend\ftxpost.c &
        extend\ftxcmap.c extend\ftxwidth.c extend\ftxsbit.c &
        extend\ftxgsub.c extend\ftxgpos.c  extend\ftxopen.c &
        extend\ftxgdef.c

OBJS_X = extend\ftxgasp.obj extend\ftxkern.obj  extend\ftxpost.obj &
         extend\ftxcmap.obj extend\ftxwidth.obj extend\ftxsbit.obj &
         extend\ftxgsub.obj extend\ftxgpos.obj  extend\ftxopen.obj &
         extend\ftxgdef.obj

SRC_M = ttapi.c     ttcache.c   ttcalc.c   ttcmap.c  &
        ttgload.c   ttinterp.c  ttload.c   ttobjs.c  &
        ttraster.c  ttextend.c  $(PORT)

OBJS_M = ttapi.obj     ttcache.obj   ttcalc.obj   ttcmap.obj  &
         ttgload.obj   ttinterp.obj  ttload.obj   ttobjs.obj  &
         ttraster.obj  ttextend.obj  $(PORT_OBJS) $(OBJS_X)

SRC_S = freetype.c
OBJ_S = freetype.obj
OBJS_S = $(OBJ_S) $(OBJS_X)


.c.obj:
  $(CC) $(CCFLAGS) $[* /fo=$[*.obj

all: .symbolic
  $(FT_MAKE) -f $(FT_MAKEFILE) libttf.lib

debug: .symbolic
  $(FT_MAKE) -f $(FT_MAKEFILE) LIB_FILES="$(OBJS_M)" libttf.lib


libttf.lib: $(OBJS_M)
  wlib -q -n libttf.lib $(OBJS_M)

# is this correct? Know nothing about wmake and the Watcom compiler...
$(OBJ_S): $(SRC_S) $(SRC_M)
  $(CC) $(CCFLAGS) $(SRC_S) /fo=$(OBJ_S)

clean: .symbolic
  @-erase $(OBJS_M)
  @-erase *.err

distclean: .symbolic clean
  @-erase libttf.lib

new: .symbolic
  @-wtouch *.c

# end of Makefile.wat
