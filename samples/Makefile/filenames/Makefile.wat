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
