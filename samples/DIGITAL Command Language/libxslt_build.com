$! BUILD_XSLT.COM
$!
$! Build the XSLT library
$!
$! Arguments:
$!
$!	p1	- "DEBUG" is you want to build with debug
$!
$! This package requires libxml to have already been installed.  You need
$! to ensure that the logical name LIBXML is defined and points to the 
$! directory containing libxml's .h files
$!
$! This procedure creates the object libraries
$!
$!	XML_LIBDIR:LIBXSLT.OLB
$!	XML_LIBDIR:LIBEXSLT.OLB
$!
$! and the program
$!
$!	XSLTPROC
$!
$! After the library is built, you can link these routines into
$! your code with the command  
$!
$! 	LINK your_modules,XML_LIBDIR:LIBEXSLT/LIB,LIBXSLT/LIBRARY,LIBXML/LIB
$!
$! Change History
$! --------------
$! Command file author : John A Fotheringham (jaf@jafsoft.com)
$! Last update         : 2 Nov 2001
$! 
$!- configuration -------------------------------------------------------------
$!
$!- compile command.
$!
$   cc_opts = "/INCLUDE=([],XML_SRCDIR:,[-.libxslt])/NAMES=(SHORTENED)/FLOAT=IEEE/IEEE_MODE=DENORM_RESULTS"
$!
$   if p1.eqs."DEBUG"
$   then
$     debug = "Y"
$     cc_command = "CC''cc_opts'/DEBUG/NOOPTIMIZE/LIST/SHOW=ALL"
$   else
$     debug = "N"
$     cc_command = "CC''cc_opts'"
$   endif
$!
$!- configure multiple build passes for each library. -------------------------
$!
$!  For each pass:
$!
$!  "libname" is the name of the library or module being created
$!
$!  "progname" is the name of the program being created
$!
$!  "src" is the list of sources to be built into the library  or program
$!	- This should be compared to the definition of 
$!	  "<NAME>_la_SOURCES" in the MAKEFILE.IN file in 
$!	  corresponding directory.
$!
$   num_passes = 3	! two libraries and a program
$!
$!- pass 1 - library LIBXSLT
$!
$   libname_1  = "LIBXSLT"
$   h_file_1   = "xslt.h"
$   progname_1 = ""
$!
$   ! see "libxslt_la_SOURCES" in [.libxslt]makefile.in
$   src_1 = "xslt.c xsltutils.c pattern.c templates.c variables.c keys.c"
$   src_1 = src_1 + " numbers.c extensions.c extra.c functions.c"
$   src_1 = src_1 + " namespaces.c imports.c attributes.c documents.c"
$   src_1 = src_1 + " preproc.c transform.c security.c"
$!
$!- pass 2 - library LIBEXSLT
$!
$   libname_2  = "LIBEXSLT"
$   h_file_2   = "exslt.h"
$   progname_2 = ""
$!
$   ! see "libexslt_la_SOURCES" in [.libexslt]makefile.in
$   src_2   = "exslt.c common.c math.c sets.c functions.c strings.c date.c saxon.c dynamic.c"
$!
$!- pass 3 - program XSLTPROC
$!
$   libname_3  = ""
$   h_file_3   = ""
$   progname_3 = "XSLTPROC"
$!
$   ! see "xsltproc_SOURCES" in [.xsltproc]makefile.in
$   src_3   = "xsltproc.c"
$!
$!- set up and check logicals  -----------------------------------------------
$!
$!  XML_LIBDIR - object library directory
$!  XML_SRCDIR - top-level build directory of libxml package -- needed for config.h and trio.h
$!  LIBXML - source directory containing .h files for libxml package
$!
$   if f$trnlnm("XML_LIBDIR").eqs.""
$   then
$     on error then continue
$     globfile = f$search("[--...]libxml.olb")
$     if globfile.eqs.""
$     then
$       write sys$output ""
$       write sys$output "	You need to define the XML_LIBDIR logical name to"
$       write sys$output "	point to the directory containing your object"
$       write sys$output "	libraries.  This should already contain LIBXML.OLB"
$       write sys$output "	from the libxml package, and will be the directory"
$       write sys$output "	the new LIBXSLT.OLB library will be placed in"
$       write sys$output ""
$       exit
$     else
$	srcdir = f$parse(globfile,,,"DEVICE") + f$parse(globfile,,,"DIRECTORY")
$	define/process XML_LIBDIR "''srcdir'"
$       write sys$output "Defining XML_LIBDIR as ""''srcdir'"""
$     endif
$   endif
$!
$   if f$trnlnm("libxml").eqs.""
$   then
$     ! look for globals.h in a directory installed paralle to this one
$     on error then continue
$     globfile = f$search("[--...]globals.h")
$     if globfile.eqs.""
$     then
$       write sys$output ""
$       write sys$output "	You need to define a LIBXML logical directory to"
$       write sys$output "	point to the directory containing the .h files"
$       write sys$output "	for the libxml package"
$       write sys$output ""
$       exit
$     else
$	srcdir = f$element(0,"]",globfile)+ "]"
$	define/process LIBXML "''srcdir'"
$       write sys$output "Defining LIBXML as ""''srcdir'"""
$     endif
$   endif
$!
$   if f$trnlnm("XML_SRCDIR").eqs.""
$   then
$     globfile = f$search("[--...]globals.c")
$     if globfile.eqs.""
$     then
$	write sys$output "Can't locate globals.c.  You need to manually define a XML_SRCDIR logical"
$	exit
$     else
$	srcdir = f$parse(globfile,,,"DEVICE") + f$parse(globfile,,,"DIRECTORY")
$	define/process XML_SRCDIR "''srcdir'"
$       write sys$output "Defining XML_SRCDIR as ""''srcdir'"""
$     endif
$   endif
$!
$!- set up some working logicals -------------------
$!
$ pass_no = 1
$ set_pass_logical:
$!
$   if pass_no.le.num_passes
$   then
$!
$     Libname  = libname_'pass_no'
$     progname = progname_'pass_no'
$     if libname.nes.""
$     then
$       logname  = "''libname'_SRCDIR"
$     else
$       logname  = "''progname'_SRCDIR"
$     endif
$     findfile = f$element(0," ",src_'pass_no')
$!
$!--- set up a source directory logical
$!
$     if f$trnlnm("''logname'").eqs.""
$     then
$       ! look for the target file in a parallel subdirectory
$       globfile = f$search("[-...]''findfile'")
$       if globfile.eqs.""
$       then
$  	  write sys$output "Can't locate ''findfile'.  You need to manually define a ''logname' logical"
$	  exit
$       else
$  	  srcdir = f$element(0,"]",globfile)+ "]"
$	  define/process 'logname' "''srcdir'"
$         write sys$output "Defining ''logname' as ""''srcdir'"""
$       endif
$     endif
$!
$!--- if it's a library, set up a logical pointing to the .h files
$!
$     if libname.nes."" 
$     then
$	if f$trnlnm("''libname'").eqs."" 
$       then 
$         ! look for the target .h file in a parallel subdirectory
$  	  h_file = h_file_'pass_no'
$         globfile = f$search("[-...]''h_file'")
$         if globfile.eqs.""
$         then
$	    write sys$output "Can't locate ''h_file'.  You need to manually define a ''libname' logical"
$	    exit
$         else
$	    includedir = f$element(0,"]",globfile)+ "]"
$	    define/process 'libname' "''includedir'"
$           write sys$output "Defining ''libname' as ""''includedir'"""
$	  endif
$       endif
$     endif
$!
$     pass_no = pass_no +1
$     goto set_pass_logical
$!
$   endif	! for each pass
$!
$!- set up error handling (such as it is) -------------------------------------
$!
$ exit_status = 1
$ saved_default = f$environment("default")
$ on error then goto ERROR_OUT 
$ on control_y then goto ERROR_OUT 
$!
$ goto start_here
$ start_here:	  ! move this line to debug/rerun parts of this command file
$!
$!- compile modules into the library ------------------------------------------
$!
$!
$ pass_no = 1	! make three passes, one for each library, one for XSLTPROC
$ pass_loop:
$!
$ if pass_no.le.num_passes
$ then
$   Libname  = libname_'pass_no'
$   progname = progname_'pass_no'
$   if libname.nes.""
$   then
$     logname  = "''libname'_SRCDIR"
$     pass_description = "the XML_LIBDIR:''libname'.OLB object library"
$   else
$     logname  = "''progname'_SRCDIR"
$     pass_description = "the programs in ''progname'"
$   endif
$   src  = src_'pass_no'
$!
$!- create the library if need
$!
$   if libname.nes."" 
$   then
$     if f$search("XML_LIBDIR:''libname'.OLB").eqs."" 
$     then
$       write sys$output "Creating new object library XML_LIBDIR:''libname'.OLB..."
$       library/create XML_LIBDIR:'libname'.OLB
$     endif
$   endif
$!
$!- move to the source directory 
$!
$   set def 'logname'
$!
$!- define the library and link commands (link command not used as is)
$!
$   if libname.nes.""
$   then
$     lib_command  = "LIBRARY/REPLACE XML_LIBDIR:''libname'.OLB"
$     link_command = ""
$   else
$     lib_command  = ""
$     link_command = "LINK"
$   endif
$!
$   write sys$output ""
$   write sys$output "Building ''pass_description'
$   write sys$output ""
$!
$   s_no = 0
$   src = f$edit(src,"COMPRESS")
$!
$ source_loop:
$!
$     next_source = f$element (S_no," ",src)
$     if next_source.nes."" .and. next_source.nes." "
$     then
$       call build 'next_source'
$       s_no = s_no + 1
$       goto source_loop
$     endif
$!
$     pass_no = pass_no + 1
$     goto pass_loop
$!
$   endif	! for each pass
$!
$!- Th-th-th-th-th-that's all folks! ------------------------------------------
$!
$EXIT_OUT:
$!
$ set def 'saved_default
$ exit 'exit_status
$!
$
$ERROR_OUT:
$ exit_status = $status
$ write sys$output "''f$message(exit_status)'"
$ goto EXIT_OUT
$!
$!- the BUILD subroutine.  Compile then insert into library or link as required
$!
$BUILD: subroutine
$   on warning then goto EXIT_BUILD
$   source_file = p1
$   name = f$element(0,".",source_file)
$   object_file = f$fao("XML_LIBDIR:!AS.OBJ",name)
$!
$!- compile
$   write sys$output "Compiling ",p1,p2,"..."
$   cc_command /object='object_file 'source_file' 'p2'
$!
$!- insert into library if command defined
$!
$   if lib_command.nes.""  
$   then 
$     lib_command 'object_file'
$     delete/nolog 'object_file';*
$   endif
$!
$!- link module if command defined
$!
$   if link_command.nes."" 
$   then
$	text = f$element(0,".",p1)	! lose the ".c"
$	write sys$output "Linking ",text,"..."
$	dbgopts = ""
$	if debug then dbgopts = "/DEBUG"
$	link_command'dbgopts' 'object_file',-
      		XML_LIBDIR:libexslt/lib,-
      		XML_LIBDIR:libxslt/lib,-
      		XML_LIBDIR:libxml/library
$   endif
$!
$EXIT_BUILD:
$   exit $status
$!
$endsubroutine
