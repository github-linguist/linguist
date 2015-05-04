#	MMS Description file for xv
#	Written by Rick Dyson (dyson@iowasp.physics.uiowa.edu)
#	Last Modified:	30-APR-1992 for v2.21
#			 5-OCT-1992 for v2.21 (export.lcs.mit.edu version
#				    of xv-2.21 seemed to change about
#				    25-Sep-1992 without version number
#				    changing.
#			 8-FEB-1993 for v2.21b
#				    ALPHA support is in ALPHA.MMS
#			 2-MAR-1993 for v3.00
#			15-APR-1993 for v3.00 (DEC C changes)
#			25-MAY-1993 merged ALPHA.MMS and MAKEFILE.MMS
#                       27-APR-1994 for v3.01
#			23-DEC-1994 for v3.10
#
# 	Modeled after the original Unix Makefile for xv
#	Most of the Unix comments have been left intact to help debug any
#	problems.


# BE SURE TO SET THIS TO YOUR SITE'S DESTINATION DIRECTORY...!!!
BINDIR = Sys$Disk:[]

# your C compiler (and options) of choice
# Remember:  if you change the C compiler (to gcc, or whatever), be sure to
# do the same thing to the Makefile in the 'jpeg' and 'tiff' subdirectories
#
# For ALPHA/DEC C users, you must add a MACRO qualifier to the
# command line, i.e.,
#       MMS /Description = Makefile.mms /Macro = ("ALPHA=1")
#
# For VAX/DEC C users, you must add a MACRO qualifier to the
# command line, i.e.,
#       MMS /Description = Makefile.mms /Macro = ("DECC=1")
#
# Users still using the non-MOTIF DECWindows (i.e., XUI) should also
# add another macro of "DEC_XUI=1"

CC = cc


################ CONFIGURATION OPTIONS #################

# if, for whatever reason, you're unable to get the JPEG library to compile
# on your machine, *COMMENT OUT* the following lines.
#
# Also, comment out the JPEGLIB dependancy below.
#
#  VMS MMS USERS!!!
#
# 	if you don't use the JPEG package as supplied with XV, you
# 	will need fill in the complete directory specifications for JPEGDIR.
#
JPEG = ,HAVE_JPEG
JPEGDIR = [.JPEG]
JPEGLIB = $(JPEGDIR)LIBJPEG.OLB
JPEGINCLUDE = ,$(JPEGDIR)


# if, for whatever reason, you're unable to get the TIFF library to compile
# on your machine, *COMMENT OUT* the following lines
#
# Also, comment out the LIBTIFF dependancy at the end of this Makefile
#
# 	if you don't use the TIFF package as supplied with XV, you
# 	will need to fill in the complete directory specifications for TIFFDIR.
#
TIFF = ,HAVE_TIFF
TIFFDIR = [.TIFF]
TIFFLIB = $(TIFFDIR)LIBTIFF.OLB
TIFFINCLUDE = ,$(TIFFDIR)


# if, for whatever reason, you're unable to get the PDS/VICAR support
# to compile (xvpds.c, and vdcomp.c), *COMMENT OUT* the following line,
# and also remove 'vdcomp' from the 'all:' dependancy
PDS = ,HAVE_PDS


# If you are still using the DECWindows XUI interface, uncomment the
# next line.  This will help stop the "window creep" problem with this
# window mananager
.ifdef DEC_XUI
XUI = ,HAVE_XUI
.endif

DEFS = /Define = (VMS$(JPEG)$(PDS)$(TIFF)$(TIMERS)$(XUI))
INCS = /Include = ([]$(JPEGINCLUDE)$(TIFFINCLUDE))

.ifdef ALPHA
OPTIMIZE = /Optimize /Standard = VAXC
OPTS = Sys$Disk:[]DECC_OPTIONS.OPT
.else
.ifdef DECC
OPTIMIZE = /Optimize /Standard = VAXC /Warnings = NoInformationals
OPTS = Sys$Disk:[]DECC_OPTIONS.OPT
.else
OPTIMIZE = /Optimize
OPTS = Sys$Disk:[]VAXC_OPTIONS.OPT
.endif
.endif
DEBUG = /NoDebug

CFLAGS = $(CFLAGS) $(DEFS) $(INCS) $(DEBUG) $(OPTIMIZE)
LINKFLAGS = $(LINKFLAGS) $(DEBUG)

XVLIB = LIBXV.OLB

OBJS = 	xv.obj,xvevent.obj,xvroot.obj,xvmisc.obj,xvimage.obj,xvcolor.obj, \
        xvsmooth.obj,xv24to8.obj,xvgif.obj,xvpm.obj,xvinfo.obj,xvctrl.obj, \
        xvscrl.obj,xvalg.obj,xvgifwr.obj,xvdir.obj,xvbutt.obj,xvpbm.obj, \
        xvxbm.obj,xvgam.obj,xvbmp.obj,xvdial.obj,xvgraf.obj,xvsunras.obj, \
        xvjpeg.obj,xvps.obj,xvpopup.obj,xvdflt.obj,xvtiff.obj,xvtiffwr.obj, \
        xvpds.obj,xvrle.obj,xviris.obj,xvgrab.obj,xvbrowse.obj,xviff.obj, \
        xvtext.obj,xvpcx.obj,xvtarga.obj,xvxpm.obj,xvcut.obj,xvxwd.obj,   \
        xvfits.obj,vms.obj


BITS = [.Bits]annot.h

MISC = readme. changelog. ideas.

.first
.ifdef ALPHA
	@- Define /NoLog Sys DECC$Library_Include
.else
.ifdef DECC
	@- Define /NoLog Sys DECC$Library_Include
.else
	@- Define /NoLog Sys Sys$Library
.endif
	@- Define /NoLog X11 DECW$Include
	@- XVDIR = F$Environment ("Default")
.endif

all : 		$(BITS) $(OPTS) lib xv bggen decompress xcmap xvpictoppm help
	! All Finished with the VMS build of XV (v3.10)

lib :   	$(JPEGLIB) $(TIFFLIB) $(XVLIB)
	@ Continue

xv :		xv.exe
	@ Continue

bggen :		bggen.exe
	@ Continue

xcmap :         xcmap.exe
	@ Continue

xvpictoppm :	xvpictoppm.exe
	@ Continue

help :		xv.hlb
	@ Continue

decompress :	decompress.exe vdcomp.exe
	@ Continue

bggen.exe : 	bggen.obj $(XVLIB) $(OPTS)
	$(LINK) $(LINKFLAGS) bggen.obj,$(XVLIB)/Library,$(OPTS)/Option

xcmap.exe :     xcmap.obj
	$(LINK) $(LINKFLAGS) xcmap.obj,$(XVLIB)/Library,$(OPTS)/Option

xvpictoppm.exe :	xvpictoppm.obj
	$(LINK) $(LINKFLAGS) xvpictoppm.obj,$(XVLIB)/Library,$(OPTS)/Option

xv.exe : 	xv.obj $(XVLIB) $(JPEGLIB) $(TIFFLIB) $(OPTS) 
	$(LINK) $(LINKFLAGS) xv.obj,$(XVLIB)/Library,$(JPEGLIB)/Library,$(TIFFLIB)/Library,$(OPTS)/Option

$(JPEGLIB) :
	Set Default $(JPEGDIR)
.ifdef ALPHA
	$(MMS) $(MMSDEFAULTS) /Description = MAKEFILE.MMS /Macro = "ALPHA = 1" LIBJPEG.OLB
.else
	$(MMS) $(MMSDEFAULTS) /Description = MAKEFILE.MMS LIBJPEG.OLB
.endif
	Set Default 'XVDIR'

$(TIFFLIB) :
	Set Default $(TIFFDIR)
.ifdef ALPHA
	$(MMS) $(MMSDEFAULTS) /Description = MAKEFILE.MMS /Macro = "ALPHA = 1" LIBTIFF.OLB
.else
.ifdef DECC
	$(MMS) $(MMSDEFAULTS) /Description = MAKEFILE.MMS /Macro = "ALPHA = 1" LIBTIFF.OLB
.else
	$(MMS) $(MMSDEFAULTS) /Description = MAKEFILE.MMS LIBTIFF.OLB
.endif
.endif
	Set Default 'XVDIR'

$(XVLIB) :	$(OBJS)
        If "''F$Search ("$(XVLIB)")'" .eqs. "" Then Library /Create $(XVLIB)
	Library /Replace $(XVLIB) $(OBJS)

decompress.exe :	decompress.obj
	$(LINK) $(LINKFLAGS) decompress.obj,$(OPTS)/Option

vdcomp.exe :	vdcomp.obj
	$(LINK) $(LINKFLAGS) vdcomp.obj,$(OPTS)/Option

[.Bits]annot.h :	
	Set Default [.Bits]
	Set Protection = Owner:RWED *.
	Rename *. *.H
	Set Protection = Owner:RWE *.H
	Set Default [-]

#	various dependencies
$(OBJS) :   		xv.h config.h
xv.hlb :		xv.hlp
vms.obj :		includes.h dirent.h

#
#  Build the linker options file for OpenVMS VAX and VAX C.
#
Sys$Disk:[]VAXC_Options.opt :
        @ Open /Write TMP VAXC_Options.opt
        @ Write TMP "! XV (v3.10) Linker Options list for VMS VAX C"
        @ Write TMP "!"
        @ Write TMP "Sys$Disk:[]LibXV.OLB /Library"
        @ Write TMP "Sys$Disk:[.JPEG]LibJPEG.OLB /Library"
        @ Write TMP "Sys$Disk:[.TIFF]LibTIFF.OLB /Library"
.ifdef DEC_XUI
        @ Write TMP "!"
        @ Write TMP "! These were appropriate for VAX C and XUI in the past."
        @ Write TMP "Sys$Library:DECW$DWTLibShr.exe /Share
        @ Write TMP "Sys$Library:DECW$XLibShr.exe /Share
        @ Write TMP "!You may need the next line for VAX C (v3.2-044)"
        @ Write TMP "!But you don't for DEC C (v4.0-000)"
        @ Write TMP "Sys$Library:VAXCRTL.EXE /Share"
.else
        @ Write TMP "!"
        @ Write TMP "! Some of the following libraries may not be available on"
        @ Write TMP "! older systems, namely XUI or Motif v1.0/1.1"
        @ Write TMP "! I know they are available for OpenVMS v6.1/Motif v1.2."
        @ Write TMP "! If you get a report that they can't be found,"
        @ Write TMP "! comment them out and try again..."
        @ Write TMP "Sys$Library:DECW$DXMLibShr12.exe /Share
        @ Write TMP "Sys$Library:DECW$XMLibShr12.exe /Share
        @ Write TMP "Sys$Library:DECW$XTLibShrR5.exe /Share"
        @ Write TMP "Sys$Library:DECW$XLibShr.exe /Share
.endif
        @ Close TMP

Sys$Disk:[]DECC_Options.opt :
        @ Open /Write TMP DECC_Options.opt
        @ Write TMP "! XV (v3.10) Linker Options list for VMS DEC C"
        @ Write TMP "!"
        @ Write TMP "Sys$Disk:[]LibXV.olb /Library"
        @ Write TMP "Sys$Disk:[.JPEG]LibJPEG.olb /Library"
        @ Write TMP "Sys$Disk:[.TIFF]LibTIFF.olb /Library"
        @ Write TMP "Sys$Library:DECW$XTShr.exe /Share"
        @ Write TMP "Sys$Library:DECW$XLibShr.exe /Share"
        @ Close TMP

install :	xv.exe vdcomp.exe bggen.exe decompress.exe
	Copy *.exe $(BINDIR)

clean :
	@- Set Protection = Owner:RWED *.obj,*.*;-1
	- Delete /NoConfirm /NoLog *.obj;*,*.exe;*,*.log;*,*.olb;*,*.hlb;*
	- Purge /NoConfirm /NoLog
	Set Default [.JPEG]
	$(MMS) /Description = MAKEFILE.MMS clean
	- Delete /NoConfirm /NoLog *.olb;*
	Set Default [-.TIFF]
	$(MMS) /Description = MAKEFILE.MMS clean
	Set Default [-]
