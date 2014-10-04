$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!								!
$!	Copyright 2001, 2011 Fidelity Information Services, Inc	!
$!								!
$!	This source code contains the intellectual property	!
$!	of its copyright holder(s), and is made available	!
$!	under a license.  If you do not know the terms of	!
$!	the license, please stop and do not read further.	!
$!								!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$!
$!	KITINSTAL.COM PROCEDURE FOR THE GT.M PRODUCT
$!
$ ON CONTROL_Y THEN VMI$CALLBACK CONTROL_Y
$! ON WARNING THEN EXIT $STATUS !! allow warning errors for INSTALL REPLACE
$ IF P1 .EQS. "VMI$_INSTALL" THEN GOTO INSTALL
$ IF P1 .EQS. "VMI$_POSTINSTALL" THEN GOTO POSTINSTALL
$ IF P1 .EQS. "VMI$_IVP" THEN GOTO IVP
$ EXIT VMI$_UNSUPPORTED
$!
$INSTALL:
$ TYPE SYS$INPUT

  GT.M  (c)  COPYRIGHT 1985 - 2000  by  Sanchez Computer Associates
                            ALL RIGHTS RESERVED

$!  the following 2 lines must be maintained
$ GTM$VMS_VERSION :== 072	! Minimum VMS version required
$ ALPHA = (f$getsyi("arch_name") .eqs. "Alpha")
$ IF ALPHA
$  THEN
$   GTM$DISK_SPACE == 28000	! Minumum disk space on system disk required
$  ELSE
$   GTM$DISK_SPACE == 16000	! Minumum disk space on system disk required
$ ENDIF
$ IF F$ELEMENT(0,",",VMI$VMS_VERSION) .EQS. "RELEASED"
$  THEN
$   GTM$VMS_IS == F$ELEMENT(1,",",VMI$VMS_VERSION)
$   IF GTM$VMS_IS .LTS. GTM$VMS_VERSION
$    THEN
$     VMI$CALLBACK MESSAGE E VMSMISMATCH "This GT.M kit requires an existing VMS''GTM$VMS_VERSION' system."
$     EXIT VMI$_FAILURE
$   ENDIF
$  ELSE
$   GTM$VMS_IS :==
$   WRITE SYS$OUTPUT "  No VMS version checking performed for field test versions."
$ ENDIF
$ IF (GTM$VMS_IS .GES. "052") THEN T1 = F$VERIFY(VMI$KIT_DEBUG)
$ VMI$CALLBACK CHECK_NET_UTILIZATION GTM$ROOM 'GTM$DISK_SPACE'
$ IF .NOT. GTM$ROOM
$  THEN
$   VMI$CALLBACK MESSAGE E NOSPACE "There is not enough disk space -- GT.M needs ''GTM$DISK_SPACE' blocks."
$   EXIT VMI$_FAILURE
$ ENDIF
$!  setup default answers
$ GTM$DOPURGE :== YES
$ GTM$RUN_IVP :== YES
$ GTM$STD_CNF :== YES
$ GTM$DST_OWN :== SYSTEM
$ IF F$IDENTIFIER(GTM$DST_OWN,"NAME_TO_NUMBER") .EQ. 0 THEN GTM$DST_OWN :== 1,4
$ GTM$SYS_DST :== YES
$ GTM$DST_DIR :== GTM_DIST
$ GTM$DST_CRE == GTM$DST_DIR
$ GTM$DST_DEV :==
$ GTM$STARTDB :== YES
$ GTM$MGR_COM :== YES
$ GTM$HLP_DIR :== NO
$ GTM$DEF_DCL :== YES
$ GTM$DEF_SYS :== YES
$ GTM$LNK_LOG :== YES
$ GTM$INSTALL :== YES
$ GTM$DEF_GLD :== YES
$ GTM$GBL_DIR :== MUMPS.GLD
$ GTM$DEF_RTN :== YES
$ GTM$RTN_DIR :== [],GTM$DIST:
$ GTM$PCT_RTN :== YES
$ VMI$CALLBACK ASK GTM$DOPURGE "Do you want to purge files replaced by this installation" 'GTM$DOPURGE' B -
  "@VMI$KWD:GTMKITHLP HELP_PURGE"
$ IF .NOT. GTM$DOPURGE THEN VMI$CALLBACK SET PURGE NO
$ VMI$CALLBACK ASK GTM$STD_CNF "Do you want the standard GT.M configuration (performs INSTALL)" 'GTM$STD_CNF' B -
  "@VMI$KWD:GTMKITHLP HELP_STD_CNF"
$ IF GTM$STD_CNF
$  THEN
$   GTM$SYS_DST == 1
$   GTM$STARTDB == 1
$   GTM$MGR_COM == 1
$   GTM$HLP_DIR == 0
$   GTM$DEF_DCL == 1
$   GTM$DEF_SYS == 1
$   GTM$INSTALL == 1
$   GTM$LNK_LOG == 1
$   GTM$INSTALL == 1
$   GTM$DEF_GLD == 1
$   GTM$DEF_RTN == 1
$   GTM$PCT_RTN == 1
$   GTM$DST_LOG :== SYS$COMMON:['GTM$DST_DIR']
$   GTM$DIR_TYPE :== COMMON
$  ELSE ! Not standard configuration
$   VMI$CALLBACK ASK GTM$DST_OWN "What UIC should own the GT.M distribution" 'GTM$DST_OWN' S "@VMI$KWD:GTMKITHLP HELP_DST_OWN"
$   GTM$DST_OWN == GTM$DST_OWN - "[" - "]"
$   VMI$CALLBACK ASK GTM$SYS_DST "Do you want the GT.M distribution to go into a System Directory" 'GTM$SYS_DST' B -
    "@VMI$KWD:GTMKITHLP HELP_SYS_DST"
$   IF GTM$SYS_DST
$    THEN
$     VMI$CALLBACK ASK GTM$DST_DIR "In what System Directory do you want to place GT.M" 'GTM$DST_DIR' S -
      "@VMI$KWD:GTMKITHLP HELP_SYS_DIR"
$     GTM$DST_DIR == GTM$DST_DIR - "[" - "]"
$     GTM$DST_CRE == GTM$DST_DIR
$     GTM$DST_LOG :== SYS$COMMON:['GTM$DST_DIR']
$     GTM$DIR_TYPE :== COMMON
$    ELSE ! Not system disk
$     VMI$CALLBACK ASK GTM$DST_DEV "On which device do you want to place GT.M" "''GTM$DST_DEV'" S "@VMI$KWD:GTMKITHLP HELP_DST_DEV"
$     VMI$CALLBACK ASK GTM$DST_DIR "In what directory on that device do you want to place GT.M" 'GTM$DST_DIR' S -
      "@VMI$KWD:GTMKITHLP HELP_DST_DIR"
$     GTM$DST_DEV == GTM$DST_DEV - ":"
$     GTM$DST_DIR == GTM$DST_DIR - "[" - "]"
$     GTM$DST_LOG :== 'GTM$DST_DEV':['GTM$DST_DIR']
$     GTM$DST_CRE == GTM$DST_LOG
$     GTM$DIR_TYPE :== USER
$   ENDIF ! system disk
$   VMI$CALLBACK ASK GTM$STARTDB "Do you want GTMSTART.COM in the startup database" 'GTM$STARTDB' B -
    "@VMI$KWD:GTMKITHLP HELP_STARTDB"
$   IF .NOT. GTM$STARTDB
$    THEN
$     VMI$CALLBACK ASK GTM$MGR_COM "Do you want the GT.M .COM files in SYS$MANAGER" 'GTM$MGR_COM' B -
      "@VMI$KWD:GTMKITHLP HELP_MGR_COM"
$   ENDIF
$   VMI$CALLBACK ASK GTM$HLP_DIR "Do you want the GT.M help files in SYS$HELP" 'GTM$HLP_DIR' B "@VMI$KWD:GTMKITHLP HELP_HLP_DIR"
$   VMI$CALLBACK ASK GTM$DEF_DCL "Do you want to define GT.M commands to the system" 'GTM$DEF_DCL' B -
    "@VMI$KWD:GTMKITHLP HELP_DEF_DCL"
$   VMI$CALLBACK ASK GTM$DEF_SYS "Do you want to define GT.M logical names in the System Table" 'GTM$DEF_SYS' B -
    "@VMI$KWD:GTMKITHLP HELP_DEF_SYS"
$   VMI$CALLBACK ASK GTM$LNK_LOG "Do you want to define the LNK$LIBRARY logical names" 'GTM$LNK_LOG' B -
    "@VMI$KWD:GTMKITHLP HELP_LNK_LOG"
$   VMI$CALLBACK ASK GTM$RUN_IVP "Do you want to run the IVP (performs INSTALL)" 'GTM$RUN_IVP' B -
    "@VMI$KWD:GTMKITHLP HELP_RUN_IVP"
$   IF GTM$RUN_IVP
$    THEN
$     GTM$PCT_RTN == 1
$    ELSE
$     VMI$CALLBACK ASK GTM$PCT_RTN "Do you want to compile the GT.M percent routines (performs INSTALL)" 'GTM$PCT_RTN' B -
      "@VMI$KWD:GTMKITHLP HELP_PCT_RTN"
$   ENDIF
$   IF (GTM$RUN_IVP .OR. GTM$PCT_RTN)
$    THEN
$     GTM$INSTALL == 1
$    ELSE
$     VMI$CALLBACK ASK GTM$INSTALL "Do you want to INSTALL the GT.M shareable images now" 'GTM$INSTALL' B -
      "@VMI$KWD:GTMKITHLP HELP_INSTALL"
$   ENDIF
$   VMI$CALLBACK ASK GTM$DEF_RTN "Do you want to have a default definition for GTM$ROUTINES" 'GTM$DEF_RTN' B -
    "@VMI$KWD:GTMKITHLP HELP_DEF_RTN"
$   IF GTM$DEF_RTN
$    THEN
$     VMI$CALLBACK ASK GTM$RTN_DIR "What is the search specification for GTM$ROUTINES" 'GTM$RTN_DIR' S -
      "@VMI$KWD:GTMKITHLP HELP_RTN_DIR"
$   ENDIF
$   VMI$CALLBACK ASK GTM$DEF_GLD "Do you want to have a default definition for GTM$GBLDIR" 'GTM$DEF_GLD' B -
    "@VMI$KWD:GTMKITHLP HELP_DEF_GLD"
$   IF GTM$DEF_GLD
$    THEN
$     VMI$CALLBACK ASK GTM$GBL_DIR "What is the file specification for GTM$GBLDIR" 'GTM$GBL_DIR' S -
      "@VMI$KWD:GTMKITHLP HELP_GBL_DIR"
$   ENDIF
$ ENDIF ! standard configuration
$!  tell them what's happening
$ IF GTM$MGR_COM
$  THEN
$   WRITE SYS$OUTPUT "  The following command files are created and copied to SYS$MANAGER:"
$  ELSE
$   WRITE SYS$OUTPUT "  The following command files are created:"
$ ENDIF
$ TYPE SYS$INPUT

        GTMINSTALL.COM
        GTMLOGICALS.COM
        GTMLOGIN.COM
        GTMSTART.COM
        GTMSTOP.COM

  Each file contains its own user documentation.

  All the questions have been asked.   Installation now proceeds  without your
  manual intervention for about 10-15 minutes.
$ IF GTM$RUN_IVP THEN WRITE SYS$OUTPUT "  Finally the Installation Verification Procedure tests the installation."
$ WRITE SYS$OUTPUT ""
$ VMI$CALLBACK CREATE_DIRECTORY 'GTM$DIR_TYPE' 'GTM$DST_CRE' "/OWNER_UIC=[''GTM$DST_OWN'] /PROTECTION=(WO:RE)"
$ VMI$CALLBACK RESTORE_SAVESET B
$ VMI$CALLBACK RESTORE_SAVESET C
$ WRITE SYS$OUTPUT ""
$ VMI$CALLBACK MESSAGE I CRECOM "Creating command files."
$!  Create GTMINSTALL.COM
$ OPEN /WRITE OUFILE VMI$KWD:GTMINSTALL.COM
$ WRITE OUFILE "$!"
$ WRITE OUFILE "$!	GTMINSTALL.COM installs GTMSECSHR and other GT.M images."
$ WRITE OUFILE "$!      GTMSECSHR is a small protected image and must be installed."
$ WRITE OUFILE "$!      GTMSHR is the run-time library and is installed for performance."
$ WRITE OUFILE "$!      GTM$DMOD and MCOMPILE are small images frequently used in development."
$ WRITE OUFILE "$!"
$ WRITE OUFILE "$ INSTALL"
$ WRITE OUFILE "REPLACE  /OPEN/SHARED/HEADER/PROTECTED	GTMSECSHR"
$ WRITE OUFILE "REPLACE  /OPEN/SHARED/HEADER		GTMSHR"
$ WRITE OUFILE "REPLACE  /OPEN/SHARED/HEADER		GTM$DMOD"
$ WRITE OUFILE "REPLACE  /OPEN/SHARED/HEADER		MCOMPILE"
$ WRITE OUFILE "$ EXIT"
$ CLOSE OUFILE
$!  Create GTMLOGICALS.COM
$ GTM$HLP_LOG :== GTM$DIST
$ IF GTM$HLP_DIR THEN GTM$HLP_LOG :== SYS$HELP
$ OPEN /WRITE OUFILE VMI$KWD:GTMLOGICALS.COM
$ WRITE OUFILE "$!"
$ WRITE OUFILE "$!	GTMLOGICALS.COM defines the logical names required to use GT.M."
$ WRITE OUFILE "$!	By default the definitions are placed in the PROCESS table."
$ WRITE OUFILE "$!	Parameter 1, if supplied should be the name of a logical name table"
$ WRITE OUFILE "$!	and/or the mode of definition."
$ WRITE OUFILE "$!      Assignments in a ""permanent"" table reduce GT.M activation time."
$ WRITE OUFILE "$!"
$ IF GTM$LNK_LOG THEN WRITE OUFILE "$!	The LNK$LIBRARY names many require adjustment to your environment."
$ IF GTM$DEF_GLD THEN WRITE OUFILE "$!	GTM$GBLDIR is defined to provide default access to a global directory."
$ IF GTM$DEF_RTN THEN WRITE OUFILE "$!	GTM$ROUTINES is defined to provide access to the GT.M utilities."
$ IF GTM$DEF_RTN THEN WRITE OUFILE "$!	You may wish to define a different structure for $ZROUTINES."
$ WRITE OUFILE "$!"
$ WRITE OUFILE "$ IF (P1 .NES. """") .AND. (F$EXTRACT(0,1,P1) .NES. ""/"") THEN P1 := /'P1"
$ WRITE OUFILE "$ DEFINE 'P1' GTM$DIST		''GTM$DST_LOG'"
$ IF GTM$DEF_GLD THEN WRITE OUFILE "$ DEFINE 'P1' GTM$GBLDIR	''GTM$GBL_DIR'"
$ IF GTM$DEF_RTN THEN WRITE OUFILE "$ DEFINE 'P1' GTM$ROUTINES	""''GTM$RTN_DIR'"""
$ WRITE OUFILE "$ DEFINE 'P1' GTM$HELP		''GTM$HLP_LOG'"
$ WRITE OUFILE "$ DEFINE 'P1' GTMSHR		GTM$DIST:GTMSHR.EXE"
$ WRITE OUFILE "$ DEFINE 'P1' GTMSECSHR		GTM$DIST:GTMSECSHR.EXE"
$ WRITE OUFILE "$ DEFINE 'P1' GTM$DMOD		GTM$DIST:GTM$DMOD.EXE"
$ WRITE OUFILE "$ DEFINE 'P1' MCOMPILE		GTM$DIST:MCOMPILE.EXE"
$ IF GTM$LNK_LOG
$  THEN
$   N1 = 0
$   DN = 0
$   T1 = F$TRNLNM("LNK$LIBRARY")
$   IF  (T1 .EQS. "") .OR. (F$LOCATE("GTMLIB",T1) .NE. F$LENGTH(T1)) .OR. (F$LOCATE("GTMSHR",T1) .NE. F$LENGTH(T1))
$    THEN
$     WRITE OUFILE "$ DEFINE 'P1' LNK$LIBRARY	GTM$DIST:GTMLIB.OLB"
$     DN = 1
$    ELSE ! lnk$library is in use
$LNK_LOOP:
$     N1 = N1 + 1
$     T1 = F$TRNLNM("LNK$LIBRARY_''N1'")
$     IF  (T1 .EQS. "") .OR. (F$LOCATE("GTMLIB",T1) .NE. F$LENGTH(T1)) .OR. (F$LOCATE("GTMSHR",T1) .NE. F$LENGTH(T1))
$      THEN
$       WRITE OUFILE "$ DEFINE 'P1' LNK$LIBRARY_''N1'	GTM$DIST:GTMLIB.OLB"
$       DN = 1
$     ENDIF
$     IF  (.NOT. DN) .AND. (N1 .LT. 998)  THEN  GOTO LNK_LOOP
$   ENDIF ! gtmlib handling
$   IF  DN ! placed gtmlib
$    THEN
$     N1 = N1 + 1
$     WRITE OUFILE "$ DEFINE 'P1' LNK$LIBRARY_''N1'	GTM$DIST:GTMSHR.OLB"
$    ELSE
$     VMI$CALLBACK MESSAGE I NOLNKLOG "No LNK$LIBRARY logical names available"
$   ENDIF
$ ENDIF ! setting up LNK$LIBRARYs
$ WRITE OUFILE "$ EXIT"
$ CLOSE OUFILE
$!  Create GTMLOGIN.COM
$ OPEN /WRITE OUFILE VMI$KWD:GTMLOGIN.COM
$ WRITE OUFILE "$!"
$ WRITE OUFILE "$!	GTMLOGIN.COM performs process specific setup for GT.M."
$ WRITE OUFILE "$!	It calls GTMLOGICALS.COM if the logical names are not"
$ WRITE OUFILE "$!      in the SYSTEM table."
$ WRITE OUFILE "$!	It defines symbols to access GT.M images."
$ WRITE OUFILE "$!	It defines GT.M commands locally if they are not defined to the system."
$ WRITE OUFILE "$!      When the command and logical names are not defined on a process level,"
$ WRITE OUFILE "$!      a production user may save start-up time by not using GTMLOGIN."
$ WRITE OUFILE "$!      CCE is infrequently used, but may be defined as a foreign command."
$ WRITE OUFILE "$!"
$ IF .NOT. GTM$DEF_SYS
$  THEN
$   WRITE OUFILE "$ dir = F$ENVIRONMENT(""PROCEDURE"")"
$   WRITE OUFILE "$ dir = F$PARSE(dir,,,""DEVICE"") + F$PARSE(dir,,,""DIRECTORY"")"
$   WRITE OUFILE "$ @'dir'GTMLOGICALS.COM"
$ ENDIF
$ IF .NOT. GTM$DEF_DCL THEN WRITE OUFILE "$ SET COMMAND GTM$DIST:GTMCOMMANDS.CLD"
$ WRITE OUFILE "$ DSE :== $GTM$DIST:DSE.EXE	! Database System Editor"
$ WRITE OUFILE "$ GDE :== $GTM$DIST:GDE.EXE	! Global Directory Editor"
$ WRITE OUFILE "$ GTM :== MUMPS/DIRECT		! Direct Mode MUMPS"
$ WRITE OUFILE "$ LKE :== $GTM$DIST:LKE.EXE	! Lock Editor"
$ WRITE OUFILE "$ MUPI*P :== $GTM$DIST:MUPIP.EXE	! MUMPS Peripheral Interchange Program"
$ WRITE OUFILE "$ EXIT"
$ WRITE OUFILE "$ CCE :== $GTM$DIST:CCE.EXE	! GT.CX Operator Interface Program"
$ WRITE OUFILE "$ EXIT"
$ CLOSE OUFILE
$!  Create GTMSTART.COM
$ OPEN /WRITE OUFILE VMI$KWD:GTMSTART.COM
$ WRITE OUFILE "$!"
$ WRITE OUFILE "$!	GTMSTART.COM should be placed in the VMS startup database."
$ WRITE OUFILE "$!	It invokes GTMLOGICALS.COM and GTMINSTALL.COM."
$ WRITE OUFILE "$!"
$ WRITE OUFILE "$ dir = F$ENVIRONMENT(""PROCEDURE"")"
$ WRITE OUFILE "$ dir = F$PARSE(dir,,,""DEVICE"") + F$PARSE(dir,,,""DIRECTORY"")"
$ IF GTM$DEF_SYS THEN WRITE OUFILE "$ IF P1 .EQS. """" .OR. (P1 .EQS. ""FULL"") THEN P1 := SYSTEM/EXEC"
$ WRITE OUFILE "$ @'dir'GTMLOGICALS 'P1'"
$ WRITE OUFILE "$ @'dir'GTMINSTALL"
$ WRITE OUFILE "$ EXIT"
$ CLOSE OUFILE
$!  Create GTMSTOP.COM
$ OPEN /WRITE OUFILE VMI$KWD:GTMSTOP.COM
$ WRITE OUFILE "$!"
$ WRITE OUFILE "$!	GTMSTOP.COM stops all the active GT.M processes and does a RUNDOWN."
$ WRITE OUFILE "$!	Place an invocation or copy of this procedure in the site specific"
$ WRITE OUFILE "$!	 shutdown: SYS$MANAGER:SYSHUTDWN to ensure all GT.M databases are"
$ WRITE OUFILE "$!	 properly closed before VMS terminates.  GTMSTOP should follow"
$ WRITE OUFILE "$!	 GTCMSTOP and precede GTCXSTOP, if they are used."
$ WRITE OUFILE "$!	If GTMSTOP is not intended to disable subsequent use of GT.M,"
$ WRITE OUFILE "$!	 add a comment (!) before the INSTALL REMOVE GTMSECSHR."
$ WRITE OUFILE "$!"
$ IF .NOT. GTM$DEF_SYS
$  THEN
$   WRITE OUFILE "$ dir = F$ENVIRONMENT(""PROCEDURE"")"
$   WRITE OUFILE "$ dir = F$PARSE(dir,,,""DEVICE"") + F$PARSE(dir,,,""DIRECTORY"")"
$   WRITE OUFILE "$ @'dir'GTMLOGICALS.COM"
$ ENDIF
$ WRITE OUFILE "$ MUPIP := $GTM$DIST:MUPIP.EXE"
$ WRITE OUFILE "$ STOP := $GTM$DIST:GTM$STOP"
$ WRITE OUFILE "$ STOP 'P1'"
$ WRITE OUFILE "$ MUPIP RUNDOWN"
$ WRITE OUFILE "$ INSTALL REMOVE GTMSECSHR"
$ WRITE OUFILE "$ EXIT"
$ IF GTM$DEF_SYS THEN WRITE OUFILE "$ IF P2 .EQS. """" THEN P2 := /SYSTEM/EXEC"
$ WRITE OUFILE "$ DEASSIGN 'P2' GTMSECSHR"
$ CLOSE OUFILE
$ VMI$CALLBACK MESSAGE I PREINS "Preparing files for installation."
$!  GTMFILES.KIT must be maintained as kit contents change
$ GTM$HLP_LOG == GTM$DST_LOG
$ IF GTM$HLP_DIR THEN GTM$HLP_LOG :== VMI$ROOT:[SYSHLP]
$ OPEN /WRITE OUFILE VMI$KWD:GTMFILES.KIT
$ IF GTM$MGR_COM
$  THEN
$   WRITE OUFILE "GTM$ GTMINSTALL.COM VMI$ROOT:[SYSMGR] C"
$   WRITE OUFILE "GTM$ GTMLOGICALS.COM VMI$ROOT:[SYSMGR] C"
$   WRITE OUFILE "GTM$ GTMLOGIN.COM VMI$ROOT:[SYSMGR] C"
$   WRITE OUFILE "GTM$ GTMSTART.COM VMI$ROOT:[SYSMGR] C"
$   WRITE OUFILE "GTM$ GTMSTOP.COM VMI$ROOT:[SYSMGR] C"
$ ENDIF
$ WRITE OUFILE "GTM$ GTMINSTALL.COM ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ GTMLOGICALS.COM ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ GTMLOGIN.COM ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ GTMSTART.COM ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ GTMSTOP.COM ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ DSE.HLB ''GTM$HLP_LOG'"
$ WRITE OUFILE "GTM$ GDE.HLB ''GTM$HLP_LOG'"
$ WRITE OUFILE "GTM$ LKE.HLB ''GTM$HLP_LOG'"
$ WRITE OUFILE "GTM$ MUMPS.HLB ''GTM$HLP_LOG'"
$ WRITE OUFILE "GTM$ MUPIP.HLB ''GTM$HLP_LOG'"
$ WRITE OUFILE "GTM$ GTMLIB.OLB ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ GTMSHR.OLB ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ GTMZCALL.MLB ''GTM$DST_LOG'"
$ IF ALPHA
$  THEN
$   WRITE OUFILE "GTM$ GTM$DEFAULTS.M64 ''GTM$DST_LOG'"
$  ELSE
$   WRITE OUFILE "GTM$ GTM$DEFAULTS.MAR ''GTM$DST_LOG'"
$ ENDIF
$ WRITE OUFILE "GTM$ GTM$CE.H ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ GTMCOLLECT.OPT ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ GTMCOMMANDS.CLD ''GTM$DST_LOG' C"
$ WRITE OUFILE "GTM$ *.M ''GTM$DST_LOG'"
$ CLOSE OUFILE
$!  GTMIMAGES.KIT must be maintained as kit contents change
$ OPEN /WRITE OUFILE VMI$KWD:GTMIMAGES.KIT
$ WRITE OUFILE "GTM$ GTMSECSHR.EXE ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ GTMSHR.EXE ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ DSE.EXE ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ GDE.EXE ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ GTM$DMOD.EXE ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ LKE.EXE ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ MCOMPILE.EXE ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ MUPIP.EXE ''GTM$DST_LOG'"
$ WRITE OUFILE "GTM$ GTM$STOP.EXE ''GTM$DST_LOG'"
$ CLOSE OUFILE
$!  Provide with file.KITs
$ VMI$CALLBACK PROVIDE_FILE "" VMI$KWD:GTMFILES.KIT "" T
$ VMI$CALLBACK PROVIDE_IMAGE "" VMI$KWD:GTMIMAGES.KIT "" T
$ VMI$CALLBACK MESSAGE I FININS "Finalizing the installation."
$ IF GTM$DEF_DCL THEN VMI$CALLBACK PROVIDE_DCL_COMMAND GTMCOMMANDS.CLD
$ IF GTM$STARTDB THEN VMI$CALLBACK MODIFY_STARTUP_DB ADD GTMSTART.COM LPMAIN
$!  GTM$INSTALL is TRUE if GTM$RUN_IVP or GTM$PCT_RTN
$ IF GTM$INSTALL THEN VMI$CALLBACK SET POSTINSTALL YES
$ IF GTM$RUN_IVP THEN VMI$CALLBACK SET IVP YES
$ EXIT VMI$_SUCCESS
$!
$POSTINSTALL:
$ ON CONTROL_Y THEN EXIT VMI$_FAILURE
$! remove MUPIP from command tables for change from V2.4 to V2.5
$ SET NOON
$ DEFINE /USER_MODE SYS$OUTPUT NL:
$ DEFINE /USER_MODE SYS$ERROR NL:
$ SET COMMAND /TABLE=SYS$COMMON:[SYSLIB]DCLTABLES /OUTPUT=SYS$COMMON:[SYSLIB]DCLTABLES /DELETE=MUPIP
$ DEFINE /USER_MODE SYS$OUTPUT NL:
$ DEFINE /USER_MODE SYS$ERROR NL:
$ SET COMMAND /DELETE=MUPIP
$ SET ON
$ IF GTM$MGR_COM
$  THEN
$   T1 := SYS$MANAGER:
$  ELSE
$   T1 = GTM$DST_LOG
$ ENDIF
$ @'T1'GTMSTART
$ @'T1'GTMLOGIN
$ ON CONTROL_Y THEN EXIT VMI$_FAILURE
$ SET DEFAULT GTM$DIST
$ T2 = F$ENVIRONMENT("PROTECTION")
$ SET PROTECTION=(S=REWD,O=REWD,G=REWD,W=RE)/DEFAULT
$ MUMPS GTM$DMOD.M
$ IF GTM$LNK_LOG
$  THEN
$   T1 :=
$  ELSE
$   T1 :=,GTMLIB.OLB/LIB,GTMSHR.OLB/LIB
$ ENDIF
$ LINK GTM$DMOD.OBJ/NOTRACE'T1
$ IF GTM$PCT_RTN
$  THEN
$   TYPE SYS$INPUT

  Compiling the GT.M percent (%) routines.
$   MUMPS *
$   IF GTM$DOPURGE THEN PURGE *.*
$   SET DEFAULT VMI$KWD
$ ENDIF ! percent routines
$ SET PROTECTION=('T2')/DEFAULT
$ EXIT VMI$_SUCCESS
$!
$IVP:
$!	The real Installation Verification Procedure.
$ TYPE SYS$INPUT

  GT.M  Installation Verification Procedure

$!	Extract the IVP .COM file from the text library.
$ LIBRARIAN /EXTRACT=GTM$IVP /OUTPUT=GTM$IVP.COM	GTM$IVP.TLB
$ @GTM$IVP
$ EXIT $STATUS
$!
