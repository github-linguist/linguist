$! Compiling with VAXC is said to work, but it requires the usual cruft
$! (vaxcrtl and all), and to avoid hair we don't supply said cruft here.
$ CC/DECC/PREFIX=all VMSBACKUP.C/DEFINE=(HAVE_MT_IOCTLS=0,HAVE_UNIXIO_H=1)
$ CC/DECC/PREFIX=all DCLMAIN.C
$! Probably we don't want match as it probably doesn't implement VMS-style
$! matching, but I haven't looking into the issues yet.
$ CC/DECC/PREFIX=all match
$ LINK/exe=VMSBACKUP.EXE -
vmsbackup.obj,dclmain.obj,match.obj,sys$input/opt
identification="VMSBACKUP4.1.1"
