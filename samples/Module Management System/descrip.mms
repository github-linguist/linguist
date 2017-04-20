# DESCRIP.MMS
# Written By:	Robert Alan Byer / byer@mail.ourservers.net
# Modified By:	Mark Pizzolato / mark@infocomm.com
#		Norman Lastovica / norman.lastovica@oracle.com
#
# This MMS/MMK build script is used to compile the various simulators in
# the SIMH package for OpenVMS using DEC C v6.0-001(AXP), v6.5-001(AXP),
# HP C V7.2-001 (IA64) and v6.4-005(VAX).
#
# Notes:  On VAX, the PDP-10 and Eclipse simulators will not be built 
#         due to the fact that INT64 is required for that simulator.
#
# This build script will accept the following build options.
#
#            ALL             Just Build "Everything".
#            ALTAIR          Just Build The MITS Altair.
#            ALTAIRZ80       Just Build The MITS Altair Z80.
#            ECLIPSE         Just Build The Data General Eclipse.
#            GRI             Just Build The GRI Corporation GRI-909.
#            LGP             Just Build The Royal-McBee LGP-30.
#            H316            Just Build The Honewell 316/516.
#            HP2100          Just Build The Hewlett-Packard HP-2100. 
#            I1401           Just Build The IBM 1401.
#            I1620           Just Build The IBM 1620.
#            IBM1130         Just Build The IBM 1130.
#            ID16            Just Build The Interdata 16-bit CPU.
#            ID32            Just Build The Interdata 32-bit CPU.
#            NOVA            Just Build The Data General Nova.
#            PDP1            Just Build The DEC PDP-1.
#            PDP4            Just Build The DEC PDP-4.
#            PDP7            Just Build The DEC PDP-7.
#            PDP8            Just Build The DEC PDP-8.
#            PDP9            Just Build The DEC PDP-9.
#            PDP10           Just Build The DEC PDP-10.
#            PDP11           Just Build The DEC PDP-11.
#            PDP15           Just Build The DEC PDP-15.
#            S3              Just Build The IBM System 3.
#            SDS             Just Build The SDS 940.
#            VAX             Just Build The DEC VAX.
#            VAX780          Just Build The DEC VAX780.
#            CLEAN           Will Clean Files Back To Base Kit.
#
# To build with debugging enabled (which will also enable traceback 
# information) use..
#
#        MMK/MACRO=(DEBUG=1)
#
# This will produce an executable named {Simulator}-{I64|VAX|AXP}-DBG.EXE
#

# Let's See If We Are Going To Build With DEBUG Enabled.  Always compile
# /DEBUG so that the traceback and debug information is always available
# in the object files.

CC_DEBUG = /DEBUG

.IFDEF DEBUG
LINK_DEBUG = /DEBUG/TRACEBACK
CC_OPTIMIZE = /NOOPTIMIZE

.IFDEF MMSALPHA
ALPHA_OR_IA64 = 1
CC_FLAGS = /PREF=ALL
ARCH = AXP-DBG
CC_DEFS = "_LARGEFILE"
.ENDIF

.IFDEF MMSIA64
ALPHA_OR_IA64 = 1
CC_FLAGS = /PREF=ALL
ARCH = I64-DBG
CC_DEFS = "_LARGEFILE"
.ENDIF

.IFDEF MMSVAX
ALPHA_OR_IA64 = 0
CC_FLAGS = $(CC_FLAGS)
ARCH = VAX-DBG
CC_DEFS = "__VAX"
.ENDIF

.ELSE
LINK_DEBUG = /NODEBUG/NOTRACEBACK

.IFDEF MMSALPHA
ALPHA_OR_IA64 = 1
CC_OPTIMIZE = /OPT=(LEV=5)/ARCH=HOST
CC_FLAGS = /PREF=ALL
ARCH = AXP
CC_DEFS = "_LARGEFILE"
LINK_SECTION_BINDING = /SECTION_BINDING
.ENDIF

.IFDEF MMSIA64
ALPHA_OR_IA64 = 1
CC_OPTIMIZE = /OPT=(LEV=5)
CC_FLAGS = /PREF=ALL
ARCH = I64
CC_DEFS = "_LARGEFILE"
.ENDIF

.IFDEF MMSVAX
ALPHA_OR_IA64 = 0
CC_OPTIMIZE = /OPTIMIZE
CC_FLAGS = $(CC_FLAGS)
ARCH = VAX
CC_DEFS = "__VAX"
.ENDIF

.ENDIF

# Define Our Compiler Flags & Define The Compile Command
OUR_CC_FLAGS = $(CC_FLAGS)$(CC_DEBUG)$(CC_OPTIMIZE) \
	/NEST=PRIMARY/NAME=(AS_IS,SHORT)
CC = CC/DECC$(OUR_CC_FLAGS)

# Define The BIN Directory Where The Executables Will Go.
# Define Our Library Directory.
# Define The platform specific Build Directory Where The Objects Will Go.
#
BIN_DIR = SYS$DISK:[.BIN]
LIB_DIR = SYS$DISK:[.LIB]
BLD_DIR = SYS$DISK:[.LIB.BLD-$(ARCH)]

# Check To Make Sure We Have SYS$DISK:[.BIN] & SYS$DISK:[.LIB] Directory.
#
.FIRST
  @ IF (F$SEARCH("SYS$DISK:[]BIN.DIR").EQS."") THEN CREATE/DIRECTORY $(BIN_DIR)
  @ IF (F$SEARCH("SYS$DISK:[]LIB.DIR").EQS."") THEN CREATE/DIRECTORY $(LIB_DIR)
  @ IF (F$SEARCH("SYS$DISK:[.LIB]BLD-$(ARCH).DIR").EQS."") THEN CREATE/DIRECTORY $(BLD_DIR)
  @ IF (F$SEARCH("$(BLD_DIR)*.*").NES."") THEN DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.*;*
  @ IF "".NES."''CC'" THEN DELETE/SYMBOL/GLOBAL CC

# Core SIMH File Definitions.
#
SIMH_DIR = SYS$DISK:[]
SIMH_LIB = $(LIB_DIR)SIMH-$(ARCH).OLB
SIMH_SOURCE = $(SIMH_DIR)SIM_CONSOLE.C,$(SIMH_DIR)SIM_SOCK.C,\
              $(SIMH_DIR)SIM_TMXR.C,$(SIMH_DIR)SIM_ETHER.C,\
              $(SIMH_DIR)SIM_TAPE.C,$(SIMH_DIR)SIM_FIO.C,\
              $(SIMH_DIR)SIM_TIMER.C

# VMS PCAP File Definitions.
#
PCAP_DIR = SYS$DISK:[.PCAP-VMS.PCAP-VCI]
PCAP_LIB = $(LIB_DIR)PCAP-$(ARCH).OLB
PCAP_SOURCE = \
	$(PCAP_DIR)PCAPVCI.C,$(PCAP_DIR)VCMUTIL.C,\
	$(PCAP_DIR)BPF_DUMP.C,$(PCAP_DIR)BPF_FILTER.C,\
	$(PCAP_DIR)BPF_IMAGE.C,$(PCAP_DIR)ETHERENT.C,\
	$(PCAP_DIR)FAD-GIFC.C,$(PCAP_DIR)GENCODE.C,\
	$(PCAP_DIR)GRAMMAR.C,$(PCAP_DIR)INET.C,\
	$(PCAP_DIR)NAMETOADDR.C,$(PCAP_DIR)OPTIMIZE.C,\
	$(PCAP_DIR)PCAP.C,$(PCAP_DIR)SAVEFILE.C,\
	$(PCAP_DIR)SCANNER.C,$(PCAP_DIR)SNPRINTF.C,\
	$(PCAP_DIR)PCAP-VMS.C
PCAP_VCMDIR = SYS$DISK:[.PCAP-VMS.PCAPVCM]
PCAP_VCM_SOURCES = $(PCAP_VCMDIR)PCAPVCM.C,$(PCAP_VCMDIR)PCAPVCM_INIT.MAR,\
		   $(PCAP_VCMDIR)VCI_JACKET.MAR,$(PCAP_VCMDIR)VCMUTIL.C
PCAP_VCI = SYS$COMMON:[SYS$LDR]PCAPVCM.EXE

# PCAP is not available on OpenVMS VAX or IA64 right now
#
.IFDEF MMSALPHA
PCAP_EXECLET = $(PCAP_VCI)
PCAP_INC = ,$(PCAP_DIR)
PCAP_LIBD = $(PCAP_LIB)
PCAP_LIBR = ,$(PCAP_LIB)/LIB/SYSEXE
PCAP_DEFS = ,"USE_NETWORK=1"
PCAP_SIMH_INC = /INCL=($(PCAP_DIR))
.ENDIF

# MITS Altair Simulator Definitions.
#
ALTAIR_DIR = SYS$DISK:[.ALTAIR]
ALTAIR_LIB = $(LIB_DIR)ALTAIR-$(ARCH).OLB
ALTAIR_SOURCE = $(ALTAIR_DIR)ALTAIR_SIO.C,$(ALTAIR_DIR)ALTAIR_CPU.C,\
                $(ALTAIR_DIR)ALTAIR_DSK.C,$(ALTAIR_DIR)ALTAIR_SYS.C
ALTAIR_OPTIONS = /INCL=($(SIMH_DIR),$(ALTAIR_DIR))/DEF=($(CC_DEFS))

#
# MITS Altair Z80 Simulator Definitions.
#
ALTAIRZ80_DIR = SYS$DISK:[.ALTAIRZ80]
ALTAIRZ80_LIB = $(LIB_DIR)ALTAIRZ80-$(ARCH).OLB
ALTAIRZ80_SOURCE = $(ALTAIRZ80_DIR)/ALTAIRZ80_CPU.C,$(ALTAIRZ80_DIR)/ALTAIRZ80_CPU_NOMMU.C,\
                   $(ALTAIRZ80_DIR)/ALTAIRZ80_DSK.C,$(ALTAIRZ80_DIR)/DISASM.C,\
                   $(ALTAIRZ80_DIR)/ALTAIRZ80_SIO.C,$(ALTAIRZ80_DIR)/ALTAIRZ80_SYS.C,\
                   $(ALTAIRZ80_DIR)/ALTAIRZ80_HDSK.C,$(ALTAIRZ80_DIR)/ALTAIRZ80_NET.C,\
                   $(ALTAIRZ80_DIR)/FLASHWRITER2.C,$(ALTAIRZ80_DIR)/I86_DECODE.C,\
                   $(ALTAIRZ80_DIR)/I86_OPS.C,$(ALTAIRZ80_DIR)/I86_PRIM_OPS.C,\
                   $(ALTAIRZ80_DIR)/I8272.C,$(ALTAIRZ80_DIR)/INSNSA.C,$(ALTAIRZ80_DIR)/INSNSD.C,\
                   $(ALTAIRZ80_DIR)/MFDC.C,$(ALTAIRZ80_DIR)/N8VEM.C,$(ALTAIRZ80_DIR)/VFDHD.C,\
                   $(ALTAIRZ80_DIR)/S100_DISK1A.C,$(ALTAIRZ80_DIR)/S100_DISK2.C,\
                   $(ALTAIRZ80_DIR)/S100_FIF.C,$(ALTAIRZ80_DIR)/S100_MDRIVEH.C,\
                   $(ALTAIRZ80_DIR)/S100_MDSAD.C,$(ALTAIRZ80_DIR)/S100_SELCHAN.C,\
                   $(ALTAIRZ80_DIR)/S100_SS1.C,$(ALTAIRZ80_DIR)/S100_64FDC.C,\
                   $(ALTAIRZ80_DIR)/S100_SCP300F.C,$(ALTAIRZ80_DIR)/SIM_IMD.C,\
                   $(ALTAIRZ80_DIR)/WD179X.C
ALTAIRZ80_OPTIONS = /INCL=($(SIMH_DIR),$(ALTAIRZ80_DIR))/DEF=($(CC_DEFS))

#
# Data General Nova Simulator Definitions.
#
NOVA_DIR = SYS$DISK:[.NOVA]
NOVA_LIB = $(LIB_DIR)NOVA-$(ARCH).OLB
NOVA_SOURCE = $(NOVA_DIR)NOVA_SYS.C,$(NOVA_DIR)NOVA_CPU.C,\
              $(NOVA_DIR)NOVA_DKP.C,$(NOVA_DIR)NOVA_DSK.C,\
              $(NOVA_DIR)NOVA_LP.C,$(NOVA_DIR)NOVA_MTA.C,\
              $(NOVA_DIR)NOVA_PLT.C,$(NOVA_DIR)NOVA_PT.C,\
              $(NOVA_DIR)NOVA_CLK.C,$(NOVA_DIR)NOVA_TT.C,\
              $(NOVA_DIR)NOVA_TT1.C,$(NOVA_DIR)NOVA_QTY.C
NOVA_OPTIONS = /INCL=($(SIMH_DIR),$(NOVA_DIR))/DEF=($(CC_DEFS))

#
# Data General Eclipse Simulator Definitions.
#
ECLIPSE_LIB = $(LIB_DIR)ECLIPSE-$(ARCH).OLB
ECLIPSE_SOURCE = $(NOVA_DIR)ECLIPSE_CPU.C,$(NOVA_DIR)ECLIPSE_TT.C,\
                 $(NOVA_DIR)NOVA_SYS.C,$(NOVA_DIR)NOVA_DKP.C,\
                 $(NOVA_DIR)NOVA_DSK.C,$(NOVA_DIR)NOVA_LP.C,\
                 $(NOVA_DIR)NOVA_MTA.C,$(NOVA_DIR)NOVA_PLT.C,\
                 $(NOVA_DIR)NOVA_PT.C,$(NOVA_DIR)NOVA_CLK.C,\
                 $(NOVA_DIR)NOVA_TT1.C,$(NOVA_DIR)NOVA_QTY.C
ECLIPSE_OPTIONS = /INCL=($(SIMH_DIR),$(NOVA_DIR))\
    		/DEF=($(CC_DEFS),"USE_INT64=1","ECLIPSE=1")

#
# GRI Corporation GRI-909 Simulator Definitions.
#
GRI_DIR = SYS$DISK:[.GRI]
GRI_LIB = $(LIB_DIR)GRI-$(ARCH).OLB
GRI_SOURCE = $(GRI_DIR)GRI_CPU.C,$(GRI_DIR)GRI_STDDEV.C,$(GRI_DIR)GRI_SYS.C
GRI_OPTIONS = /INCL=($(SIMH_DIR),$(GRI_DIR))/DEF=($(CC_DEFS))

#
# Royal-McBee LGP-30 Simulator Definitions.
#
LGP_DIR = SYS$DISK:[.LGP]
LGP_LIB = $(LIB_DIR)LGP-$(ARCH).OLB
LGP_SOURCE = $(LGP_DIR)LGP_CPU.C,$(LGP_DIR)LGP_STDDEV.C,$(LGP_DIR)LGP_SYS.C
LGP_OPTIONS = /INCL=($(SIMH_DIR),$(LGP_DIR))/DEF=($(CC_DEFS))

#
# Honeywell 316/516 Simulator Definitions.
#
H316_DIR = SYS$DISK:[.H316]
H316_LIB = $(LIB_DIR)H316-$(ARCH).OLB
H316_SOURCE = $(H316_DIR)H316_STDDEV.C,$(H316_DIR)H316_LP.C,\
              $(H316_DIR)H316_CPU.C,$(H316_DIR)H316_SYS.C,\
              $(H316_DIR)H316_FHD.C,$(H316_DIR)H316_MT.C,\
              $(H316_DIR)H316_DP.C
H316_OPTIONS = /INCL=($(SIMH_DIR),$(H316_DIR))/DEF=($(CC_DEFS))

#
# Hewlett-Packard HP-2100 Simulator Definitions.
#
HP2100_DIR = SYS$DISK:[.HP2100]
HP2100_LIB = $(LIB_DIR)HP2100-$(ARCH).OLB
HP2100_SOURCE = $(HP2100_DIR)HP2100_STDDEV.C,$(HP2100_DIR)HP2100_DP.C,\
                $(HP2100_DIR)HP2100_DQ.C,$(HP2100_DIR)HP2100_DR.C,\
                $(HP2100_DIR)HP2100_LPS.C,$(HP2100_DIR)HP2100_MS.C,\
                $(HP2100_DIR)HP2100_MT.C,$(HP2100_DIR)HP2100_MUX.C,\
                $(HP2100_DIR)HP2100_CPU.C,$(HP2100_DIR)HP2100_FP.C,\
                $(HP2100_DIR)HP2100_SYS.C,$(HP2100_DIR)HP2100_LPT.C,\
                $(HP2100_DIR)HP2100_IPL.C,$(HP2100_DIR)HP2100_DS.C,\
                $(HP2100_DIR)HP2100_CPU0.C,$(HP2100_DIR)HP2100_CPU1.C,\
                $(HP2100_DIR)HP2100_CPU2.C,$(HP2100_DIR)HP2100_CPU3.C,\
                $(HP2100_DIR)HP2100_CPU4.C,$(HP2100_DIR)HP2100_CPU5.C,\
                $(HP2100_DIR)HP2100_CPU6.C,$(HP2100_DIR)HP2100_CPU7.C,\
                $(HP2100_DIR)HP2100_FP1.C,$(HP2100_DIR)HP2100_BACI.C,\
                $(HP2100_DIR)HP2100_MPX.C,$(HP2100_DIR)HP2100_PIF.C
.IF ALPHA_OR_IA64
HP2100_OPTIONS = /INCL=($(SIMH_DIR),$(HP2100_DIR))\
    		/DEF=($(CC_DEFS),"HAVE_INT64=1")
.ELSE
HP2100_OPTIONS = /INCL=($(SIMH_DIR),$(HP2100_DIR))/DEF=($(CC_DEFS))
.ENDIF

#
# Interdata 16-bit CPU.
#
ID16_DIR = SYS$DISK:[.INTERDATA]
ID16_LIB = $(LIB_DIR)ID16-$(ARCH).OLB
ID16_SOURCE = $(ID16_DIR)ID16_CPU.C,$(ID16_DIR)ID16_SYS.C,$(ID16_DIR)ID_DP.C,\
              $(ID16_DIR)ID_FD.C,$(ID16_DIR)ID_FP.C,$(ID16_DIR)ID_IDC.C,\
              $(ID16_DIR)ID_IO.C,$(ID16_DIR)ID_LP.C,$(ID16_DIR)ID_MT.C,\
              $(ID16_DIR)ID_PAS.C,$(ID16_DIR)ID_PT.C,$(ID16_DIR)ID_TT.C,\
              $(ID16_DIR)ID_UVC.C,$(ID16_DIR)ID16_DBOOT.C,$(ID16_DIR)ID_TTP.C
ID16_OPTIONS = /INCL=($(SIMH_DIR),$(ID16_DIR))/DEF=($(CC_DEFS))

#
# Interdata 32-bit CPU.
#
ID32_DIR = SYS$DISK:[.INTERDATA]
ID32_LIB = $(LIB_DIR)ID32-$(ARCH).OLB
ID32_SOURCE = $(ID32_DIR)ID32_CPU.C,$(ID32_DIR)ID32_SYS.C,$(ID32_DIR)ID_DP.C,\
              $(ID32_DIR)ID_FD.C,$(ID32_DIR)ID_FP.C,$(ID32_DIR)ID_IDC.C,\
              $(ID32_DIR)ID_IO.C,$(ID32_DIR)ID_LP.C,$(ID32_DIR)ID_MT.C,\
              $(ID32_DIR)ID_PAS.C,$(ID32_DIR)ID_PT.C,$(ID32_DIR)ID_TT.C,\
              $(ID32_DIR)ID_UVC.C,$(ID32_DIR)ID32_DBOOT.C,$(ID32_DIR)ID_TTP.C
ID32_OPTIONS = /INCL=($(SIMH_DIR),$(ID32_DIR))/DEF=($(CC_DEFS))

#
# IBM 1130 Simulator Definitions.
#
IBM1130_DIR = SYS$DISK:[.IBM1130]
IBM1130_LIB = $(LIB_DIR)IBM1130-$(ARCH).OLB
IBM1130_SOURCE = $(IBM1130_DIR)IBM1130_CPU.C,$(IBM1130_DIR)IBM1130_CR.C,\
                 $(IBM1130_DIR)IBM1130_DISK.C,$(IBM1130_DIR)IBM1130_STDDEV.C,\
                 $(IBM1130_DIR)IBM1130_SYS.C,$(IBM1130_DIR)IBM1130_GDU.C,\
                 $(IBM1130_DIR)IBM1130_GUI.C,$(IBM1130_DIR)IBM1130_PRT.C,\
                 $(IBM1130_DIR)IBM1130_FMT.C,$(IBM1130_DIR)IBM1130_PTRP.C,\
                 $(IBM1130_DIR)IBM1130_PLOT.C,$(IBM1130_DIR)IBM1130_SCA.C,\
                 $(IBM1130_DIR)IBM1130_T2741.C
IBM1130_OPTIONS = /INCL=($(SIMH_DIR),$(IBM1130_DIR))/DEF=($(CC_DEFS))

#
# IBM 1401 Simulator Definitions.
#
I1401_DIR = SYS$DISK:[.I1401]
I1401_LIB = $(LIB_DIR)I1401-$(ARCH).OLB
I1401_SOURCE = $(I1401_DIR)I1401_LP.C,$(I1401_DIR)I1401_CPU.C,\
               $(I1401_DIR)I1401_IQ.C,$(I1401_DIR)I1401_CD.C,\
               $(I1401_DIR)I1401_MT.C,$(I1401_DIR)I1401_DP.C,\
               $(I1401_DIR)I1401_SYS.C
I1401_OPTIONS = /INCL=($(SIMH_DIR),$(I1401_DIR))/DEF=($(CC_DEFS))


#
# IBM 1620 Simulators Definitions.
#
I1620_DIR = SYS$DISK:[.I1620]
I1620_LIB = $(LIB_DIR)I1620-$(ARCH).OLB
I1620_SOURCE = $(I1620_DIR)I1620_CD.C,$(I1620_DIR)I1620_DP.C,\
               $(I1620_DIR)I1620_PT.C,$(I1620_DIR)I1620_TTY.C,\
               $(I1620_DIR)I1620_CPU.C,$(I1620_DIR)I1620_LP.C,\
               $(I1620_DIR)I1620_FP.C,$(I1620_DIR)I1620_SYS.C
I1620_OPTIONS = /INCL=($(SIMH_DIR),$(I1620_DIR))/DEF=($(CC_DEFS))

#
# PDP-1 Simulator Definitions.
#
PDP1_DIR = SYS$DISK:[.PDP1]
PDP1_LIB = $(LIB_DIR)PDP1-$(ARCH).OLB
PDP1_SOURCE = $(PDP1_DIR)PDP1_LP.C,$(PDP1_DIR)PDP1_CPU.C,\
              $(PDP1_DIR)PDP1_STDDEV.C,$(PDP1_DIR)PDP1_SYS.C,\
              $(PDP1_DIR)PDP1_DT.C,$(PDP1_DIR)PDP1_DRM.C,\
              $(PDP1_DIR)PDP1_CLK.C,$(PDP1_DIR)PDP1_DCS.C
PDP1_OPTIONS = /INCL=($(SIMH_DIR),$(PDP1_DIR))/DEF=($(CC_DEFS))

#
# Digital Equipment PDP-8 Simulator Definitions.
#
PDP8_DIR = SYS$DISK:[.PDP8]
PDP8_LIB = $(LIB_DIR)PDP8-$(ARCH).OLB
PDP8_SOURCE = $(PDP8_DIR)PDP8_CPU.C,$(PDP8_DIR)PDP8_CLK.C,\
              $(PDP8_DIR)PDP8_DF.C,$(PDP8_DIR)PDP8_DT.C,\
              $(PDP8_DIR)PDP8_LP.C,$(PDP8_DIR)PDP8_MT.C,\
	      $(PDP8_DIR)PDP8_PT.C,$(PDP8_DIR)PDP8_RF.C,\
              $(PDP8_DIR)PDP8_RK.C,$(PDP8_DIR)PDP8_RX.C,\
              $(PDP8_DIR)PDP8_SYS.C,$(PDP8_DIR)PDP8_TT.C,\
	      $(PDP8_DIR)PDP8_TTX.C,$(PDP8_DIR)PDP8_RL.C,\
	      $(PDP8_DIR)PDP8_TSC.C,$(PDP8_DIR)PDP8_TD.C,\
	      $(PDP8_DIR)PDP8_CT.C
PDP8_OPTIONS = /INCL=($(SIMH_DIR),$(PDP8_DIR))/DEF=($(CC_DEFS))

#
# Digital Equipment PDP-4, PDP-7, PDP-9 And PDP-15 Simulator Definitions.
#
PDP18B_DIR = SYS$DISK:[.PDP18B]
PDP4_LIB = $(LIB_DIR)PDP4-$(ARCH).OLB
PDP7_LIB = $(LIB_DIR)PDP7-$(ARCH).OLB
PDP9_LIB = $(LIB_DIR)PDP9-$(ARCH).OLB
PDP15_LIB = $(LIB_DIR)PDP15-$(ARCH).OLB
PDP18B_SOURCE = $(PDP18B_DIR)PDP18B_DT.C,$(PDP18B_DIR)PDP18B_DRM.C,\
                $(PDP18B_DIR)PDP18B_CPU.C,$(PDP18B_DIR)PDP18B_LP.C,\
                $(PDP18B_DIR)PDP18B_MT.C,$(PDP18B_DIR)PDP18B_RF.C,\
                $(PDP18B_DIR)PDP18B_RP.C,$(PDP18B_DIR)PDP18B_STDDEV.C,\
                $(PDP18B_DIR)PDP18B_SYS.C,$(PDP18B_DIR)PDP18B_TT1.C,\
                $(PDP18B_DIR)PDP18B_RB.C,$(PDP18B_DIR)PDP18B_FPP.C
PDP4_OPTIONS = /INCL=($(SIMH_DIR),$(PDP18B_DIR))/DEF=($(CC_DEFS),"PDP4=1")
PDP7_OPTIONS = /INCL=($(SIMH_DIR),$(PDP18B_DIR))/DEF=($(CC_DEFS),"PDP7=1")
PDP9_OPTIONS = /INCL=($(SIMH_DIR),$(PDP18B_DIR))/DEF=($(CC_DEFS),"PDP9=1")
PDP15_OPTIONS = /INCL=($(SIMH_DIR),$(PDP18B_DIR))/DEF=($(CC_DEFS),"PDP15=1")

#
# Digital Equipment PDP-11 Simulator Definitions.
#
PDP11_DIR = SYS$DISK:[.PDP11]
PDP11_LIB1 = $(LIB_DIR)PDP11L1-$(ARCH).OLB
PDP11_SOURCE1 = $(PDP11_DIR)PDP11_FP.C,$(PDP11_DIR)PDP11_CPU.C,\
               $(PDP11_DIR)PDP11_DZ.C,$(PDP11_DIR)PDP11_CIS.C,\
               $(PDP11_DIR)PDP11_LP.C,$(PDP11_DIR)PDP11_RK.C,\
               $(PDP11_DIR)PDP11_RL.C,$(PDP11_DIR)PDP11_RP.C,\
               $(PDP11_DIR)PDP11_RX.C,$(PDP11_DIR)PDP11_STDDEV.C,\
               $(PDP11_DIR)PDP11_SYS.C,$(PDP11_DIR)PDP11_TC.C, \
               $(PDP11_DIR)PDP11_CPUMOD.C,$(PDP11_DIR)PDP11_CR.C,\
               $(PDP11_DIR)PDP11_TA.C,$(PDP11_DIR)PDP11_IO_LIB.C
PDP11_LIB2 = $(LIB_DIR)PDP11L2-$(ARCH).OLB
PDP11_SOURCE2 = $(PDP11_DIR)PDP11_TM.C,$(PDP11_DIR)PDP11_TS.C,\
               $(PDP11_DIR)PDP11_IO.C,$(PDP11_DIR)PDP11_RQ.C,\
               $(PDP11_DIR)PDP11_TQ.C,$(PDP11_DIR)PDP11_PCLK.C,\
               $(PDP11_DIR)PDP11_RY.C,$(PDP11_DIR)PDP11_PT.C,\
               $(PDP11_DIR)PDP11_HK.C,$(PDP11_DIR)PDP11_XQ.C,\
               $(PDP11_DIR)PDP11_VH.C,$(PDP11_DIR)PDP11_RH.C,\
               $(PDP11_DIR)PDP11_XU.C,$(PDP11_DIR)PDP11_TU.C,\
               $(PDP11_DIR)PDP11_DL.C,$(PDP11_DIR)PDP11_RF.C, \
               $(PDP11_DIR)PDP11_RC.C,$(PDP11_DIR)PDP11_KG.C,\
               $(PDP11_DIR)PDP11_KE.C,$(PDP11_DIR)PDP11_DC.C
PDP11_OPTIONS = /INCL=($(SIMH_DIR),$(PDP11_DIR)$(PCAP_INC))\
		/DEF=($(CC_DEFS),"VM_PDP11=1"$(PCAP_DEFS))

#
# Digital Equipment PDP-10 Simulator Definitions.
#
PDP10_DIR = SYS$DISK:[.PDP10]
PDP10_LIB = $(LIB_DIR)PDP10-$(ARCH).OLB
PDP10_SOURCE = $(PDP10_DIR)PDP10_FE.C,\
               $(PDP10_DIR)PDP10_CPU.C,$(PDP10_DIR)PDP10_KSIO.C,\
               $(PDP10_DIR)PDP10_LP20.C,$(PDP10_DIR)PDP10_MDFP.C,\
	       $(PDP10_DIR)PDP10_PAG.C,$(PDP10_DIR)PDP10_XTND.C,\
               $(PDP10_DIR)PDP10_RP.C,$(PDP10_DIR)PDP10_SYS.C,\
               $(PDP10_DIR)PDP10_TIM.C,$(PDP10_DIR)PDP10_TU.C,\
	       $(PDP11_DIR)PDP11_PT.C,$(PDP11_DIR)PDP11_DZ.C,\
               $(PDP11_DIR)PDP11_RY.C,$(PDP11_DIR)PDP11_XU.C,\
               $(PDP11_DIR)PDP11_CR.C
PDP10_OPTIONS = /INCL=($(SIMH_DIR),$(PDP10_DIR),$(PDP11_DIR))\
		/DEF=($(CC_DEFS),"USE_INT64=1","VM_PDP10=1"$(PCAP_DEFS))

#
# IBM System 3 Simulator Definitions.
#
S3_DIR = SYS$DISK:[.S3]
S3_LIB = $(LIB_DIR)S3-$(ARCH).OLB
S3_SOURCE = $(S3_DIR)S3_CD.C,$(S3_DIR)S3_CPU.C,$(S3_DIR)S3_DISK.C,\
            $(S3_DIR)S3_LP.C,$(S3_DIR)S3_PKB.C,$(S3_DIR)S3_SYS.C
S3_OPTIONS = /INCL=($(SIMH_DIR),$(S3_DIR))/DEF=($(CC_DEFS))

#
# SDS 940
#
SDS_DIR = SYS$DISK:[.SDS]
SDS_LIB = $(LIB_DIR)SDS-$(ARCH).OLB
SDS_SOURCE = $(SDS_DIR)SDS_CPU.C,$(SDS_DIR)SDS_DRM.C,$(SDS_DIR)SDS_DSK.C,\ 
             $(SDS_DIR)SDS_IO.C,$(SDS_DIR)SDS_LP.C,$(SDS_DIR)SDS_MT.C,\
             $(SDS_DIR)SDS_MUX.C,$(SDS_DIR)SDS_RAD.C,$(SDS_DIR)SDS_STDDEV.C,\
             $(SDS_DIR)SDS_SYS.C
SDS_OPTIONS = /INCL=($(SIMH_DIR),$(SDS_DIR))/DEF=($(CC_DEFS))

#
# Digital Equipment VAX Simulator Definitions.
#
VAX_DIR = SYS$DISK:[.VAX]
VAX_LIB = $(LIB_DIR)VAX-$(ARCH).OLB
VAX_SOURCE = $(VAX_DIR)VAX_CIS.C,$(VAX_DIR)VAX_CMODE.C,\
             $(VAX_DIR)VAX_CPU.C,$(VAX_DIR)VAX_CPU1.C,\
             $(VAX_DIR)VAX_FPA.C,$(VAX_DIR)VAX_MMU.C,\
             $(VAX_DIR)VAX_OCTA.C,$(VAX_DIR)VAX_SYS.C,\
             $(VAX_DIR)VAX_SYSCM.C,$(VAX_DIR)VAX_SYSDEV.C,\
	     $(VAX_DIR)VAX_SYSLIST.C,$(VAX_DIR)VAX_IO.C,\
             $(VAX_DIR)VAX_STDDEV.C,$(PDP11_DIR)PDP11_IO_LIB.C,\
             $(PDP11_DIR)PDP11_RL.C,$(PDP11_DIR)PDP11_RQ.C,\
             $(PDP11_DIR)PDP11_TS.C,$(PDP11_DIR)PDP11_DZ.C,\
             $(PDP11_DIR)PDP11_LP.C,$(PDP11_DIR)PDP11_TQ.C,\
             $(PDP11_DIR)PDP11_XQ.C,$(PDP11_DIR)PDP11_CR.C,\
             $(PDP11_DIR)PDP11_RY.C,$(PDP11_DIR)PDP11_VH.C
VAX_OPTIONS = /INCL=($(SIMH_DIR),$(VAX_DIR),$(PDP11_DIR)$(PCAP_INC))\
		/DEF=($(CC_DEFS),"VM_VAX=1"$(PCAP_DEFS))

# Digital Equipment VAX780 Simulator Definitions.
#
VAX780_DIR = SYS$DISK:[.VAX]
VAX780_LIB1 = $(LIB_DIR)VAX780L1-$(ARCH).OLB
VAX780_SOURCE1 = $(VAX780_DIR)VAX_CPU.C,$(VAX780_DIR)VAX_CPU1.C,\
	$(VAX780_DIR)VAX_FPA.C,$(VAX780_DIR)VAX_CIS.C,\
	$(VAX780_DIR)VAX_OCTA.C,$(VAX780_DIR)VAX_CMODE.C,\
	$(VAX780_DIR)VAX_MMU.C,$(VAX780_DIR)VAX_SYS.C,\
	$(VAX780_DIR)VAX_SYSCM.C,$(VAX780_DIR)VAX780_STDDEV.C,\
	$(VAX780_DIR)VAX780_SBI.C,$(VAX780_DIR)VAX780_MEM.C,\
	$(VAX780_DIR)VAX780_UBA.C,$(VAX780_DIR)VAX780_MBA.C,\
	$(VAX780_DIR)VAX780_FLOAD.C,$(VAX780_DIR)VAX780_SYSLIST.C
VAX780_LIB2 = $(LIB_DIR)VAX780L2-$(ARCH).OLB
VAX780_SOURCE2 = $(PDP11_DIR)PDP11_RL.C,$(PDP11_DIR)PDP11_RQ.C,\
	$(PDP11_DIR)PDP11_TS.C,$(PDP11_DIR)PDP11_DZ.C,\
	$(PDP11_DIR)PDP11_LP.C,$(PDP11_DIR)PDP11_TQ.C,\
	$(PDP11_DIR)PDP11_XU.C,$(PDP11_DIR)PDP11_RY.C,\
	$(PDP11_DIR)PDP11_CR.C,$(PDP11_DIR)PDP11_RP.C,\
	$(PDP11_DIR)PDP11_TU.C,$(PDP11_DIR)PDP11_HK.C,\
        $(PDP11_DIR)PDP11_IO_LIB.C
VAX780_OPTIONS = /INCL=($(SIMH_DIR),$(VAX780_DIR),$(PDP11_DIR)$(PCAP_INC))\
	/DEF=($(CC_DEFS),"VM_VAX=1"$(PCAP_DEFS),"VAX_780=1")

# IBM 7094 Simulator Definitions.
#
I7094_DIR = SYS$DISK:[.I7094]
I7094_LIB = $(LIB_DIR)I7094-$(ARCH).OLB
I7094_SOURCE = $(I7094_DIR)I7094_CPU.C,$(I7094_DIR)I7094_CPU1.C,\
	$(I7094_DIR)I7094_IO.C,$(I7094_DIR)I7094_CD.C,\
	$(I7094_DIR)I7094_CLK.C,$(I7094_DIR)I7094_COM.C,\
	$(I7094_DIR)I7094_DRM.C,$(I7094_DIR)I7094_DSK.C,\
	$(I7094_DIR)I7094_SYS.C,$(I7094_DIR)I7094_LP.C,\
	$(I7094_DIR)I7094_MT.C,$(I7094_DIR)I7094_BINLOADER.C
I7094_OPTIONS = /INCL=($(SIMH_DIR),$(I7094_DIR))/DEF=($(CC_DEFS))

# If we're not a VAX, Build Everything
#
.IF ALPHA_OR_IA64
ALL :	ALTAIR ALTAIRZ80 ECLIPSE GRI LGP H316 HP2100 I1401 I1620 IBM1130 ID16 \
	ID32 NOVA PDP1 PDP4 PDP7 PDP8 PDP9 PDP10 PDP11 PDP15 S3 VAX VAX780 SDS \
	I7094
	@CONTINUE
.ELSE
#
# Else We Are On VAX And Build Everything EXCEPT the 64b simulators
#
ALL :	ALTAIR ALTAIRZ80 GRI H316 HP2100 I1401 I1620 IBM1130 ID16 ID32 \
	NOVA PDP1 PDP4 PDP7 PDP8 PDP9 PDP11 PDP15 S3 VAX VAX780 SDS
	@CONTINUE
.ENDIF

CLEAN : 
	$!
	$! Clean out all targets and building Remnants
	$!
	$ IF (F$SEARCH("$(BIN_DIR)*.EXE;*").NES."") THEN -
	     DELETE/NOLOG/NOCONFIRM $(BIN_DIR)*.EXE;*
	$ IF (F$SEARCH("$(LIB_DIR)*.OLB;*").NES."") THEN -
	     DELETE/NOLOG/NOCONFIRM $(LIB_DIR)*.OLB;*
	$ IF (F$SEARCH("SYS$DISK:[...]*.OBJ;*").NES."") THEN -
	     DELETE/NOLOG/NOCONFIRM SYS$DISK:[...]*.OBJ;*
	$ IF (F$SEARCH("SYS$DISK:[...]*.LIS;*").NES."") THEN -
	     DELETE/NOLOG/NOCONFIRM SYS$DISK:[...]*.LIS;*
	$ IF (F$SEARCH("SYS$DISK:[...]*.MAP;*").NES."") THEN -
	     DELETE/NOLOG/NOCONFIRM SYS$DISK:[...]*.MAP;*

#
# Build The Libraries.
#
$(SIMH_LIB) : $(SIMH_SOURCE)
                $!
		$! Building The $(SIMH_LIB) Library.
                $!
                $ $(CC)/DEF=($(CC_DEFS)$(PCAP_DEFS))$(PCAP_SIMH_INC) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(ALTAIR_LIB) : $(ALTAIR_SOURCE)
		$!
		$! Building The $(ALTAIR_LIB) Library.
                $!
                $ $(CC)$(ALTAIR_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(ALTAIRZ80_LIB) : $(ALTAIRZ80_SOURCE)
                $!
		$! Building The $(ALTAIRZ80_LIB) Library.
                $!
                $ $(CC)$(ALTAIRZ80_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

#
# If Not On VAX, Build The Eclipse Library.
#
.IF ALPHA_OR_IA64
$(ECLIPSE_LIB) : $(ECLIPSE_SOURCE)
                $!
		$! Building The $(ECLIPSE_LIB) Library.
                $!
                $ $(CC)$(ECLIPSE_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*
.ELSE
#
# We Are On VAX And Due To The Use of INT64 We Can't Build It.
#
$(ECLIPSE_LIB) : 
                $!
		$! Due To The Use Of INT64 We Can't Build The
                $! $(LIB_DIR)ECLIPSE-$(ARCH).OLB Library On VAX.
                $!
.ENDIF

$(GRI_LIB) : $(GRI_SOURCE)
		$!
		$! Building The $(GRI_LIB) Library.
		$!
		$ $(CC)$(GRI_OPTIONS) -
			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
		$ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
			LIBRARY/CREATE $(MMS$TARGET)
		$ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
		$ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(LGP_LIB) : $(LGP_SOURCE)
		$!
		$! Building The $(LGP_LIB) Library.
		$!
		$ $(CC)$(LGP_OPTIONS) -
			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
		$ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
			LIBRARY/CREATE $(MMS$TARGET)
		$ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
		$ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(H316_LIB) : $(H316_SOURCE)
                $!
		$! Building The $(H316_LIB) Library.
                $!
                $ $(CC)$(H316_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(HP2100_LIB) : $(HP2100_SOURCE)
                $!
		$! Building The $(HP2100_LIB) Library.
                $!
                $ $(CC)$(HP2100_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(I1401_LIB) : $(I1401_SOURCE)
                $!
		$! Building The $(I1401_LIB) Library.
                $!
	        $ $(CC)$(I1401_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(I1620_LIB) : $(I1620_SOURCE)
                $!
		$! Building The $(I1620_LIB) Library.
                $!
	        $ $(CC)$(I1620_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(IBM1130_LIB) : $(IBM1130_SOURCE)
                $!
		$! Building The $(IBM1130_LIB) Library.
                $!
	        $ $(CC)$(IBM1130_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(ID16_LIB) : $(ID16_SOURCE)
                $!
		$! Building The $(ID16_LIB) Library.
                $!
	        $ $(CC)$(ID16_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(ID32_LIB) : $(ID32_SOURCE)
                $!
		$! Building The $(ID32_LIB) Library.
                $!
	        $ $(CC)$(ID32_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(NOVA_LIB) : $(NOVA_SOURCE)
                $!
		$! Building The $(NOVA_LIB) Library.
                $!
                $ $(CC)$(NOVA_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(PDP1_LIB) : $(PDP1_SOURCE)
                $!
		$! Building The $(PDP1_LIB) Library.
                $!
                $ $(CC)$(PDP1_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(PDP4_LIB) : $(PDP18B_SOURCE)
                $!
		$! Building The $(PDP4_LIB) Library.
                $!
                $ $(CC)$(PDP4_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(PDP7_LIB) : $(PDP18B_SOURCE)
                $!
		$! Building The $(PDP7_LIB) Library.
                $!
                $ $(CC)$(PDP7_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(PDP8_LIB) : $(PDP8_SOURCE)
                $!
		$! Building The $(PDP8_LIB) Library.
                $!
                $ $(CC)$(PDP8_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(PDP9_LIB) : $(PDP18B_SOURCE)
                $!
		$! Building The $(PDP9_LIB) Library.
                $!
                $ $(CC)$(PDP9_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

#
# If Not On VAX, Build The PDP-10 Library.
#
.IF ALPHA_OR_IA64
$(PDP10_LIB) : $(PDP10_SOURCE)
                $!
		$! Building The $(PDP10_LIB) Library.
                $!
                $ $(CC)$(PDP10_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*
.ELSE
#
# We Are On VAX And Due To The Use of INT64 We Can't Build It.
#
$(PDP10_LIB) : 
		$! Due To The Use Of INT64 We Can't Build The
                $! $(LIB_DIR)PDP10-$(ARCH).OLB Library On VAX.
.ENDIF

$(PDP11_LIB1) : $(PDP11_SOURCE1)
                $!
		$! Building The $(PDP11_LIB1) Library.
                $!
                $(CC)$(PDP11_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(PDP11_LIB2) : $(PDP11_SOURCE2)
                $!
		$! Building The $(PDP11_LIB2) Library.
                $!
                $(CC)$(PDP11_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(PDP15_LIB) : $(PDP18B_SOURCE)
                $!
		$! Building The $(PDP15_LIB) Library.
                $!
                $ $(CC)$(PDP15_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(S3_LIB) : $(S3_SOURCE)
                $!
		$! Building The $(S3_LIB) Library.
                $!
                $ $(CC)$(S3_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(SDS_LIB) : $(SDS_SOURCE)
                $!
		$! Building The $(SDS_LIB) Library.
                $!
                $ $(CC)$(SDS_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(VAX_LIB) : $(VAX_SOURCE)
                $!
		$! Building The $(VAX_LIB) Library.
                $!
                $ $(CC)$(VAX_OPTIONS)/OBJ=$(VAX_DIR) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(VAX780_LIB1) : $(VAX780_SOURCE1)
                $!
		$! Building The $(VAX780_LIB1) Library.
                $!
                $ $(CC)$(VAX780_OPTIONS)/OBJ=$(VAX780_DIR) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(VAX780_LIB2) : $(VAX780_SOURCE2)
                $!
		$! Building The $(VAX780_LIB2) Library.
                $!
                $ $(CC)$(VAX780_OPTIONS)/OBJ=$(VAX780_DIR) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

$(PCAP_LIB) : $(PCAP_SOURCE)
                $!
		$! Building The $(PCAP_LIB) Library.
                $!
		$ SET DEFAULT $(PCAP_DIR)
                $ @VMS_PCAP $(DEBUG)
		$ SET DEFAULT [--]
                $ IF (F$SEARCH("$(PCAP_LIB)").NES."") THEN -
                        DELETE $(PCAP_LIB);
                $ COPY $(PCAP_DIR)PCAP.OLB $(PCAP_LIB)
                $ DELETE/NOLOG/NOCONFIRM $(PCAP_DIR)*.OBJ;*,$(PCAP_DIR)*.OLB;*
			     
#
# If Not On VAX, Build The IBM 7094 Library.
#
.IF ALPHA_OR_IA64
$(I7094_LIB) : $(I7094_SOURCE)
                $!
		$! Building The $(I7094_LIB) Library.
                $!
                $ $(CC)$(I7094_OPTIONS) -
    			/OBJ=$(BLD_DIR) $(MMS$CHANGED_LIST)
                $ IF (F$SEARCH("$(MMS$TARGET)").EQS."") THEN -
                        LIBRARY/CREATE $(MMS$TARGET)
                $ LIBRARY/REPLACE $(MMS$TARGET) $(BLD_DIR)*.OBJ
                $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*
.ELSE
#
# We Are On VAX And Due To The Use of INT64 We Can't Build It.
#
$(I7094_LIB) : 
		$! Due To The Use Of INT64 We Can't Build The
                $! $(LIB_DIR)I7094-$(ARCH).OLB Library On VAX.
.ENDIF

#
# Individual Simulator Builds.
#
ALTAIR : $(SIMH_LIB) $(ALTAIR_LIB)
         $!
         $! Building The $(BIN_DIR)ALTAIR-$(ARCH).EXE Simulator.
         $!
         $ $(CC)$(ALTAIR_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
         $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)ALTAIR-$(ARCH).EXE -
                $(BLD_DIR)SCP.OBJ,$(ALTAIR_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
         $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

ALTAIRZ80 : $(SIMH_LIB) $(ALTAIRZ80_LIB)
            $!
            $! Building The $(BIN_DIR)ALTAIRZ80-$(ARCH).EXE Simulator.
            $!
            $ $(CC)$(ALTAIRZ80_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
            $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)ALTAIRZ80-$(ARCH).EXE -
                   $(BLD_DIR)SCP.OBJ,$(ALTAIRZ80_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
            $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

#
# If Not On VAX, Build The PDP-10 Simulator.
#
.IF ALPHA_OR_IA64
ECLIPSE : $(SIMH_LIB) $(ECLIPSE_LIB)
          $!
          $! Building The $(BIN_DIR)ECLIPSE-$(ARCH).EXE Simulator.
          $!
          $ $(CC)$(ECLIPSE_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
          $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)ECLIPSE-$(ARCH).EXE -
                 $(BLD_DIR)SCP.OBJ,$(ECLIPSE_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
          $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*
.ELSE
#
# Else We Are On VAX And Tell The User We Can't Build On VAX
# Due To The Use Of INT64.
#
ECLIPSE : 
        $! Sorry, Can't Build $(BIN_DIR)ECLIPSE-$(ARCH).EXE Simulator
        $! Because It Requires The Use Of INT64.
.ENDIF

GRI : $(SIMH_LIB) $(GRI_LIB)
      $!
      $! Building The $(BIN_DIR)GRI-$(ARCH).EXE Simulator.
      $!
      $ $(CC)$(GRI_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
      $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)GRI-$(ARCH).EXE -
             $(BLD_DIR)SCP.OBJ,$(GRI_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
      $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

LGP : $(SIMH_LIB) $(LGP_LIB)
      $!
      $! Building The $(BIN_DIR)LGP-$(ARCH).EXE Simulator.
      $!
      $ $(CC)$(LGP_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
      $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)LGP-$(ARCH).EXE -
             $(BLD_DIR)SCP.OBJ,$(LGP_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
      $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

H316 : $(SIMH_LIB) $(H316_LIB)
       $!
       $! Building The $(BIN_DIR)H316-$(ARCH).EXE Simulator.
       $!
       $ $(CC)$(H316_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
       $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)H316-$(ARCH).EXE -
              $(BLD_DIR)SCP.OBJ,$(H316_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
       $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

HP2100 : $(SIMH_LIB) $(HP2100_LIB)
         $!
         $! Building The $(BIN_DIR)HP2100-$(ARCH).EXE Simulator.
         $!
         $ $(CC)$(HP2100_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
         $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)HP2100-$(ARCH).EXE -
                $(BLD_DIR)SCP.OBJ,$(HP2100_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
         $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

I1401 : $(SIMH_LIB) $(I1401_LIB)
        $!
        $! Building The $(BIN_DIR)I1401-$(ARCH).EXE Simulator.
        $!
        $ $(CC)$(I1401_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
        $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)I1401-$(ARCH).EXE -
               $(BLD_DIR)SCP.OBJ,$(I1401_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
        $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

I1620 : $(SIMH_LIB) $(I1620_LIB)
        $!
        $! Building The $(BIN_DIR)I1620-$(ARCH).EXE Simulator.
        $!
        $ $(CC)$(I1620_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
        $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)I1620-$(ARCH).EXE -
               $(BLD_DIR)SCP.OBJ,$(I1620_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
        $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

IBM1130 : $(SIMH_LIB) $(IBM1130_LIB)
          $!
          $! Building The $(BIN_DIR)IBM1130-$(ARCH).EXE Simulator.
          $!
          $ $(CC)$(IBM1130_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
          $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)IBM1130-$(ARCH).EXE -
                 $(BLD_DIR)SCP.OBJ,$(IBM1130_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
          $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

ID16 : $(SIMH_LIB) $(ID16_LIB)
       $!
       $! Building The $(BIN_DIR)ID16-$(ARCH).EXE Simulator.
       $!
       $ $(CC)$(ID16_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
       $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)ID16-$(ARCH).EXE -
              $(BLD_DIR)SCP.OBJ,$(ID16_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
       $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

ID32 : $(SIMH_LIB) $(ID32_LIB)
       $!
       $! Building The $(BIN_DIR)ID32-$(ARCH).EXE Simulator.
       $!
       $ $(CC)$(ID32_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
       $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)ID32-$(ARCH).EXE -
              $(BLD_DIR)SCP.OBJ,$(ID32_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
       $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

NOVA : $(SIMH_LIB) $(NOVA_LIB)
       $!
       $! Building The $(BIN_DIR)NOVA-$(ARCH).EXE Simulator.
       $!
       $ $(CC)$(NOVA_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
       $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)NOVA-$(ARCH).EXE -
              $(BLD_DIR)SCP.OBJ,$(NOVA_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
       $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

PDP1 : $(SIMH_LIB) $(PDP1_LIB)
       $!
       $! Building The $(BIN_DIR)PDP1-$(ARCH).EXE Simulator.
       $!
       $ $(CC)$(PDP1_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
       $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)PDP1-$(ARCH).EXE -
              $(BLD_DIR)SCP.OBJ,$(PDP1_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
       $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

PDP4 : $(SIMH_LIB) $(PDP4_LIB)
       $!
       $! Building The $(BIN_DIR)PDP4-$(ARCH).EXE Simulator.
       $!
       $ $(CC)$(PDP4_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
       $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)PDP4-$(ARCH).EXE -
              $(BLD_DIR)SCP.OBJ,$(PDP4_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
       $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

PDP7 : $(SIMH_LIB) $(PDP7_LIB)
       $!
       $! Building The $(BIN_DIR)PDP7-$(ARCH).EXE Simulator.
       $!
       $ $(CC)$(PDP7_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
       $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)PDP7-$(ARCH).EXE -
              $(BLD_DIR)SCP.OBJ,$(PDP7_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
       $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

PDP8 : $(SIMH_LIB) $(PDP8_LIB)
       $!
       $! Building The $(BIN_DIR)PDP8-$(ARCH).EXE Simulator.
       $!
       $ $(CC)$(PDP8_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
       $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)PDP8-$(ARCH).EXE -
              $(BLD_DIR)SCP.OBJ,$(PDP8_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
       $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

PDP9 : $(SIMH_LIB) $(PDP9_LIB)
       $!
       $! Building The $(BIN_DIR)PDP9-$(ARCH).EXE Simulator.
       $!
       $ $(CC)$(PDP9_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
       $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)PDP9-$(ARCH).EXE -
              $(BLD_DIR)SCP.OBJ,$(PDP9_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
       $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

#
# If Not On VAX, Build The PDP-10 Simulator.
#
.IF ALPHA_OR_IA64
PDP10 : $(SIMH_LIB) $(PCAP_LIBD) $(PDP10_LIB) $(PCAP_EXECLET)
        $!
        $! Building The $(BIN_DIR)PDP10-$(ARCH).EXE Simulator.
        $!
        $ $(CC)$(PDP10_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
        $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)PDP10-$(ARCH).EXE -
               $(BLD_DIR)SCP.OBJ,$(PDP10_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY$(PCAP_LIBR)
        $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*
.ELSE
#
# Else We Are On VAX And Tell The User We Can't Build On VAX
# Due To The Use Of INT64.
#
PDP10 : 
        $! Sorry, Can't Build $(BIN_DIR)PDP10-$(ARCH).EXE Simulator
        $! Because It Requires The Use Of INT64.
.ENDIF

PDP11 : $(SIMH_LIB) $(PCAP_LIBD) $(PDP11_LIB1) $(PDP11_LIB2) $(PCAP_EXECLET)
        $!
        $! Building The $(BIN_DIR)PDP11-$(ARCH).EXE Simulator.
        $!
        $ $(CC)$(PDP11_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
        $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)PDP11-$(ARCH).EXE -
               $(BLD_DIR)SCP.OBJ,$(PDP11_LIB1)/LIBRARY,$(PDP11_LIB2)/LIBRARY,$(SIMH_LIB)/LIBRARY$(PCAP_LIBR)
        $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

PDP15 : $(SIMH_LIB) $(PDP15_LIB)
        $!
        $! Building The $(BIN_DIR)PDP15-$(ARCH).EXE Simulator.
        $!
        $ $(CC)$(PDP15_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
        $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)PDP15-$(ARCH).EXE -
               $(BLD_DIR)SCP.OBJ,$(PDP15_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
        $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

S3 : $(SIMH_LIB) $(S3_LIB)
     $!
     $! Building The $(BIN_DIR)S3-$(ARCH).EXE Simulator.
     $!
     $ $(CC)$(S3_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
     $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)S3-$(ARCH).EXE -
            $(BLD_DIR)SCP.OBJ,$(S3_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
     $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

SDS : $(SIMH_LIB) $(SDS_LIB)
      $!
      $! Building The $(BIN_DIR)SDS-$(ARCH).EXE Simulator.
      $!
      $ $(CC)$(SDS_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
      $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)SDS-$(ARCH).EXE -
             $(BLD_DIR)SCP.OBJ,$(SDS_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY
      $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

VAX : $(SIMH_LIB) $(PCAP_LIBD) $(VAX_LIB) $(PCAP_EXECLET)
      $!
      $! Building The $(BIN_DIR)VAX-$(ARCH).EXE Simulator.
      $!
      $ $(CC)$(VAX_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
      $ LINK $(LINK_DEBUG)$(LINK_SECTION_BINDING)-
	     /EXE=$(BIN_DIR)VAX-$(ARCH).EXE -
             $(BLD_DIR)SCP.OBJ,$(VAX_LIB)/LIBRARY,-
	     $(SIMH_LIB)/LIBRARY$(PCAP_LIBR)
      $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

VAX780 : $(SIMH_LIB) $(PCAP_LIBD) $(VAX780_LIB1) $(VAX780_LIB2) $(PCAP_EXECLET)
      $!
      $! Building The $(BIN_DIR)VAX780-$(ARCH).EXE Simulator.
      $!
      $ $(CC)$(VAX780_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
      $ LINK $(LINK_DEBUG)$(LINK_SECTION_BINDING)-
             /EXE=$(BIN_DIR)VAX780-$(ARCH).EXE -
             $(BLD_DIR)SCP.OBJ,-
             $(VAX780_LIB1)/LIBRARY,$(VAX780_LIB2)/LIBRARY,-
 	     $(SIMH_LIB)/LIBRARY$(PCAP_LIBR)
       $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*

#
# If Not On VAX, Build The IBM 7094 Simulator.
#
.IF ALPHA_OR_IA64
I7094 : $(SIMH_LIB) $(I7094_LIB)
        $!
        $! Building The $(BIN_DIR)I7094-$(ARCH).EXE Simulator.
        $!
        $ $(CC)$(I7094_OPTIONS)/OBJ=$(BLD_DIR) SCP.C
        $ LINK $(LINK_DEBUG)/EXE=$(BIN_DIR)I7094-$(ARCH).EXE -
               $(BLD_DIR)SCP.OBJ,$(I7094_LIB)/LIBRARY,$(SIMH_LIB)/LIBRARY$(PCAP_LIBR)
        $ DELETE/NOLOG/NOCONFIRM $(BLD_DIR)*.OBJ;*
.ELSE
#
# Else We Are On VAX And Tell The User We Can't Build On VAX
# Due To The Use Of INT64.
#
I7094 : 
        $! Sorry, Can't Build $(BIN_DIR)I7094-$(ARCH).EXE Simulator
        $! Because It Requires The Use Of INT64.
.ENDIF

#
# PCAP VCI Components
#
$(PCAP_VCI) : $(PCAP_VCMDIR)PCAPVCM.EXE
              $!
              $! Installing the PCAP VCI Execlet in SYS$LOADABLE_IMAGES
              $!
              $ COPY $(PCAP_VCMDIR)PCAPVCM.EXE SYS$COMMON:[SYS$LDR]PCAPVCM.EXE 

$(PCAP_VCMDIR)PCAPVCM.EXE : $(PCAP_VCM_SOURCES) 
                            $!
                            $! Building The PCAP VCI Execlet
                            $!
                            $ @SYS$DISK:[.PCAP-VMS.PCAPVCM]BUILD_PCAPVCM
                            $ DELETE/NOLOG/NOCONFIRM $(PCAP_VCMDIR)*.OBJ;*,$(PCAP_VCMDIR)*.MAP;*
