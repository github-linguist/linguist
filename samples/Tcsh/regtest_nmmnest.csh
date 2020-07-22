#!/bin/csh

# #BSUB -x                                # exlusive use of node (not_shared)
# #BSUB -a mpich_gm                       # at NCAR: lightning
# #BSUB -R "span[ptile=2]"                # how many tasks per node (1 or 2)
#BSUB -a poe                            # at NCAR: bluevista
#BSUB -R "span[ptile=4]"                # how many tasks per node (up to 8)
#BSUB -n 4                              # number of total tasks
#BSUB -o reg.out                        # output filename (%J to add job id)
#BSUB -e reg.err                        # error filename
#BSUB -J regtest                        # job name
#BSUB -q share                          # queue
#BSUB -W 12:00                          # wallclock time
##BSUB -P 48500053
#BSUB -P 64000400

# QSUB -q ded_4             # submit to 4 proc
# QSUB -l mpp_p=4           # request 4 processors
# QSUB -lT  21600           # max. job time limit is 6 h
# QSUB -lF 250Mw            # max. job file size limit is 250 Megawords
# QSUB -eo                  # merge error and output into one file
# QSUB -o  reg.out          # output file name
# QSUB                      # there are no further QSUB commands

#	This is a script to test the bit-for-bit reproducibility of
#	the WRF model, when comparing single processor serial runs to
#	OpenMP and MPI parallel runs.  There are several regression tests
#	that are performed.  Failed comparisons get reported, but don't
#	stop the script.  Failed builds or forecasts force an exit from 
#	the script.

#	Approximate time for completion of full test suite
#		Compaq 733 MHz   ev67 :  2.5 hours (empty)
#		Intel  1.2 GHz (4-pe) :  3.0 hours (empty)
#		IBM            P4     :  2.0 hours (empty)

setenv HWRF 0
#	Do we keep running even when there are BAD failures?

set KEEP_ON_RUNNING = FALSE
set KEEP_ON_RUNNING = TRUE

#	These need to be changed for your particular set of runs.  This is
#	where email gets sent.

if ( ( `uname` == AIX ) && ( ( `hostname | cut -c 1-2` != bs ) && \
                             ( `hostname | cut -c 1-2` != bv ) && ( `hostname | cut -c 1-2` != be ) ) ) then
	set FAIL_MAIL = ( ${user}@noaa.gov )
	set GOOD_MAIL = ( ${user}@noaa.gov )

	setenv MP_EAGER_LIMIT 65536
	setenv MP_SHARED_MEMORY yes
	setenv MP_SINGLE_THREAD yes
	setenv MP_LABELIO yes
	setenv MP_STDOUTMODE ordered

	setenv OMP_NUM_THREADS 4
	setenv XLSMPOPTS "parthds=4:spins=0:yields=0:stack=128000000:schedule=static"
	setenv AIXTHREAD_SCOPE S
	setenv AIXTHREAD_MNRATIO 1:1
	setenv SPINLOOPTIME 1000
	setenv YIELDLOOPTIME 1000
else
	set FAIL_MAIL = ( ${user}@ucar.edu )
	set GOOD_MAIL = ( ${user}@ucar.edu )
endif

unalias cd cp rm ls pushd popd mv
if ( ( `uname` == Linux ) || ( `uname` == Darwin ) ) alias banner echo

#	Get the command line input

set thedate = -999
set thefile = "null"
set thedata = "null"
set clrm = 0          # compile local run mmmtmp, for using clsroom cluster and local disk

#	If this is a batch job (NCAR's IBMs or FSL's Intel and Alpha), we need to muck with the "input"
#	parameters a bit.

if      ( ( `uname` == AIX ) || ( `hostname` == tempest ) || ( `hostname | cut -c 1-2` == ln ) ) then
	set argv = ( -here )
	set argv = ( -ftp )
        set argv = ( -D today )
	set argv = ( -env )
	set WRFREGFILE = /mmm/users/gill/wrf.tar
	if ( ( `uname` == AIX ) && ( ( `hostname | cut -c 1-2` != bs ) && \
	                             ( `hostname | cut -c 1-2` != bv ) && ( `hostname | cut -c 1-2` != be ) ) ) then
		set argv = ( -f /nbns/meso/wx22tb/regression_tests/wrf.tar )
	else
		set argv = ( -f wrf.tar )
	endif
endif

#	Where is the input data located - for a few known NCAR/MMM machines.

if      ( ( `hostname` == master ) || (`hostname | cut -c 1-4` == node ) ) then
	set WRFREGDATAEM = /big/users/gill/WRF-data-EM
	set WRFREGDATANMM = /big/users/gill/WRF-data-NMM
else if   ( `hostname` == jacaranda ) then
	set WRFREGDATAEM = /jacaranda/users/gill/WRF-data-EM
	set WRFREGDATANMM = /jacaranda/users/gill/WRF-data-NMM
else if   ( `hostname` == stink ) then
	set WRFREGDATAEM = /stink/gill/Regression_Tests/WRF_regression_data/processed
	set WRFREGDATANMM = /stink/gill/Regression_Tests/WRF_regression_data/WRF-data-NMM
else if   ( `hostname` == cape ) then
	set WRFREGDATAEM = /cape/users/michalak/WRF-data-EM
	set WRFREGDATANMM = /cape/users/michalak/WRF-data-NMM
else if ( (`hostname | cut -c 1-6` == joshua ) || \
          ( `hostname` == maple ) || (`hostname | cut -c 1-7` == service ) ) then
	set WRFREGDATAEM = /users/gill/WRF-data-EM
	set WRFREGDATANMM = /users/gill/WRF-data-NMM
else if ( ( `hostname | cut -c 1-2` == bs ) || ( `hostname` == tempest ) || ( `hostname | cut -c 1-2` == ln ) || \
          ( `hostname | cut -c 1-2` == bv ) || ( `hostname | cut -c 1-2` == be ) ) then
	set WRFREGDATAEM = /mmm/users/gill/WRF-data-EM
	set WRFREGDATAEM = /mmm/users/gill/WRF_regression_data/processed
	set WRFREGDATANMM = /glade/proj2/ral/RJNTB/dtc//WRF-data-NMM
else if ( ( `uname` == AIX ) && ( ( `hostname | cut -c 1-2` != bs ) && \
                                  ( `hostname | cut -c 1-2` != bv ) && ( `hostname | cut -c 1-2` != be ) ) ) then
	set WRFREGDATAEM = /nbns/meso/wx22tb/regression_tests/WRF-data-EM
	set WRFREGDATANMM = /nbns/meso/wx22tb/regression_tests/WRF-data-NMM
else
	if      ( ( -d /users/gill/WRF-data-EM ) && ( -d /users/gill/WRF-data-NMM ) ) then
		set WRFREGDATAEM = /users/gill/WRF-data-EM
		set WRFREGDATANMM = /users/gill/WRF-data-NMM
	else if ( ( -d /mmm/users/gill/WRF-data-EM ) && ( -d /mmm/users/gill/WRF-data-NMM ) ) then
		set WRFREGDATAEM = /mmm/users/gill/WRF-data-EM
		set WRFREGDATANMM = /glade/proj2/ral/RJNTB/dtc//WRF-data-NMM
	else
		echo "stick the WRF em and nmm data somewhere, and then fill in the shell vars"
		echo "inside this script, you NEED WRFREGDATAEM and WRFREGDATANMM set"
		exit ( 1 ) 
	endif
endif
#DAVE###################################################
echo DAVE em data is located at $WRFREGDATAEM
ls -ls $WRFREGDATAEM
echo DAVE nmm data is located at $WRFREGDATANMM
ls -ls $WRFREGDATANMM
banner 1
#set ans = "$<"
#DAVE###################################################

if ( $#argv == 0 ) then
	echo "Please enter either a date for cvs checkout. ex regtest.csh -D date"
	echo " or a file name containing WRF. ex regtest.csh -f tarfile"
	echo " or the -ftp flag for the script to pick code off anon ftp"
	exit ( 2 ) 
endif

set theargs = 0
foreach a ( $argv )
	if ( "$a" == "-D" ) then

		rsh -n maple.mmm.ucar.edu w >& /dev/null
		if ( $status ) then
			echo "Cannot execute a remote shell on maple.mmm.ucar.edu, where the"
			echo "WRF code resides."
			echo "Please check that it is up and that you have permission to rsh"
			echo "to this host. (Create a .rhosts file)."
			ping -c 1 maple.mmm.ucar.edu
			exit 2
		endif
		setenv CVSROOT maple.mmm.ucar.edu:/data3/mp/wrfhelp/WRF
		
		set acquire_from = "cvs"
		set thedate = $argv[2]

	endif

	if ( "$a" == "-f" ) then

		set thefile = $argv[2]
		#	Check for absolute path, if not, make it absolute
		echo $thefile | grep '^/' > /dev/null
		if ( $status != 0 ) set thefile = `pwd`/$thefile
		set acquire_from = "filearg"

	endif

	if ( "$a" == "-ftp" ) then
		set acquire_from = "ftp"
echo "anon ftp temporarily disabled"
exit ( 3 )
	endif

	if ( "$a" == "-here" ) then
		set acquire_from = "here"
	endif

	if ( "$a" == "-env" ) then
		set acquire_from = "environment"
		set thefile = $WRFREGFILE
	endif
end

#	Start recording everything - for debug purposes.

set echo 
set date

#	And to tell us how long we've spent on this whole regression test,
#	we should remember when we started.

set start = ( `date` )

#####################################################################

#	Initial set up values

#	Is this a single domain regression test or is this nested.  Well, a nested one
#	is a bit special.  It can only run on machines that have the WRF RSL_LITE-but-no-MPI
#	option available.

set NESTED = TRUE
set NESTED = FALSE

if ( $NESTED == TRUE ) then
	echo DOING a NESTED TEST
endif

#	Use the adaptive time step option

set ADAPTIVE = TRUE
set ADAPTIVE = FALSE

if ( $ADAPTIVE == TRUE ) then
	set STEP_TO_OUTPUT_TIME    = .TRUE.
	set USE_ADAPTIVE_TIME_STEP = .TRUE.
else
	set STEP_TO_OUTPUT_TIME    = .FALSE.
	set USE_ADAPTIVE_TIME_STEP = .FALSE.
endif

#	We can choose to do grid and obs nudging tests.

set FDDA = TRUE
set FDDA = FALSE

set FDDA2 = TRUE
set FDDA2 = FALSE

if ( $FDDA2 == TRUE ) then
	set FDDA = TRUE
endif

#	The default floating point precision is either 4 bytes or 8 bytes.
#	We assume that it is 4 (or the default for the architecture) unless 
#	REAL8 is set to TRUE.

set REAL8 = TRUE
set REAL8 = FALSE

#	Are we shooting for a bit-for-bit run (serial vs OpenMP, serial vs MPI), or not?
#	If you want to do a performance-only run, the forecasts are still short, but you
#	get to insure that the optimized code builds and runs.

set REG_TYPE = OPTIMIZED
set REG_TYPE = BIT4BIT

#	For a Mac/Intel, we can run either g95 or PGI.

if ( `uname` == Darwin ) then
	set LINUX_COMP = G95
	set LINUX_COMP = PGI
endif

#	We can choose to do a global test

if ( $NESTED != TRUE ) then
	set GLOBAL = TRUE
	set GLOBAL = FALSE
else if ( $NESTED == TRUE ) then
	set GLOBAL = FALSE
endif

#	Is this a WRF chem test?  
#	if CHEM = TRUE, then chemistry run
#	if KPP = TRUE, then chemistry with KPP run (CHEM set to true)

if ( $NESTED != TRUE ) then
	set KPP = TRUE
	set KPP = FALSE
	set CHEM = TRUE
	set CHEM = FALSE
	if ( $KPP == TRUE ) then
		set CHEM = TRUE
	endif
else if ( $NESTED == TRUE ) then
	set CHEM = FALSE
	set KPP  = FALSE
endif
if ( $CHEM == TRUE ) then
	setenv WRF_CHEM 1
else if ( $CHEM == FALSE ) then
	setenv WRF_CHEM 0 
endif
if ( $KPP == TRUE ) then
	setenv WRF_KPP 1
	setenv FLEX_LIB_DIR /usr/local/lib
	set CHEM_OPT = 104
else if ( $KPP == FALSE ) then
	setenv WRF_KPP 0 
	setenv FLEX_LIB_DIR
	set CHEM_OPT =
endif

#	For the real data case, we can run either one of two data cases.  If this is
#	a chemistry run, we are forced to use that data.

set dataset = jun01
set dataset = jan00
if ( $CHEM == TRUE ) then
	set dataset = chem
endif
if ( $GLOBAL == TRUE ) then
	set dataset = global
endif

#	Yet another local variable to change the name of where the data is located.

set thedataem = ${WRFREGDATAEM}/${dataset}
set thedatanmm = $WRFREGDATANMM

#	A separately installed version of the latest ESMF library (NOT the 
#	ESMF library included in the WRF tarfile) can be tested by setting 
#	"ESMF_LIB" to "TRUE" below.  This test is not supported on all 
#	machines.  

set ESMF_LIB = TRUE
set ESMF_LIB = FALSE

# serial and OMP are not tested with ESMF so always start with env vars cleared
unsetenv ESMFLIB
unsetenv ESMFINC

if ( $ESMF_LIB == TRUE ) then
	if      ( ( `uname` == AIX ) && ( ( `hostname | cut -c 1-2` == bv ) || ( `hostname | cut -c 1-2` == be ) ) ) then
		echo "A separately installed version of the latest ESMF library"
		echo "(NOT the ESMF library included in the WRF tarfile) will"
		echo "be used for MPI tests"
		setenv OBJECT_MODE 64
#	set ESMFLIBSAVE = /home/bluevista/hender/esmf/esmf_2_2_2r/lib/libO/AIX.default.64.mpi.default
#	set ESMFINCSAVE = /home/bluevista/hender/esmf/esmf_2_2_2r/mod/modO/AIX.default.64.mpi.default
		setenv ESMF_DIR /mmm/users/michalak/esmf
		setenv ESMF_BOPT g
		setenv ESMF_ABI 64
		setenv ESMF_INSTALL_PREFIX $ESMF_DIR/../esmf_install
		setenv ESMFLIB $ESMF_INSTALL_PREFIX/lib/libg/AIX.default.64.mpi.default
		setenv ESMFINC $ESMF_INSTALL_PREFIX/mod/modg/AIX.default.64.mpi.default
		set ESMFLIBSAVE = $ESMFLIB
		set ESMFINCSAVE = $ESMFINC
		echo "Using ESMFLIB = ${ESMFLIBSAVE}"
		echo "Using ESMFINC = ${ESMFINCSAVE}"
	else
		echo "Only the ESMF library included in the WRF tarfile is"
		echo "tested on this machine"
		exit ( 3 ) 
	endif
	if ( $NESTED == TRUE ) then
		echo "The ESMF library does not work with nesting."
		exit ( 3 ) 
	endif
endif

#	A single WRF output "quilt" server can be tested by setting "QUILT"  to 
#       "TRUE" below.  At the moment, testing of I/O quilt servers is not supported 
#       on all machines.  

set QUILT = TRUE
set QUILT = FALSE

if ( $QUILT == TRUE ) then
	echo "One WRF output quilt server will be used for some tests"
endif

#	Baseline data sets can be generated and archived or compared against.  
#       - To generate and archive, set GENERATE_BASELINE to a pathname that can 
#         be created by this script via "mkdir -p $GENERATE_BASELINE".  This 
#         directory must not already exist.  
#         Set GENERATE_BASELINE = FALSE to avoid baseline generation.  
#       - To compare with a previously archived baseline, set COMPARE_BASELINE 
#         to an existing directory that contains an archived baseline.  
#         Set COMPARE_BASELINE = FALSE to avoid baseline comparison.  
set GENERATE_BASELINE = FALSE
set COMPARE_BASELINE = FALSE

#	Baseline generation and comparison are only done when BIT4BIT is set.  
if ( $GENERATE_BASELINE != FALSE ) then
	if ( $REG_TYPE != BIT4BIT ) then
		echo "ERROR:  Baseline generation can only be done during BIT4BIT tests."
		exit ( 3 ) 
	endif
	if ( -d $GENERATE_BASELINE ) then
		echo "ERROR:  Baseline directory ${GENERATE_BASELINE} already exists."
		exit ( 3 ) 
	else
		# Archive serial output file to baseline
		mkdir -p $GENERATE_BASELINE || ( echo "ERROR:  cannot mkdir ${GENERATE_BASELINE}"; exit 3 )
	endif
endif
if ( $COMPARE_BASELINE != FALSE ) then
	if ( $REG_TYPE != BIT4BIT ) then
		echo "Baseline comparison can only be done during BIT4BIT tests."
		exit ( 3 ) 
	endif
	if ( ! -d $COMPARE_BASELINE ) then
		echo "${0}: ERROR:  Baseline directory ${COMPARE_BASELINE} does not exist"
		exit ( 3 ) 
	endif
endif

#	Set the input/output format type (currently 1, 2 or 5 OK).
#	Binary		NetCDF				PHDF, IBM	GriB, history only
#	1		2		3		4		5

set IO_FORM = 2
set IO_FORM_NAME = ( io_bin io_netcdf io_dummy io_phdf5 io_grib1 )
set IO_FORM_WHICH =( IO     IO        IO       IO       O        )

#	There is a breakdown of cores to test depending on the various
#	options that the user is testing.
#	nested: cannot test NMM
#	rsl_lite: cannot test anything with y periodic bc
#	chem: em_real only
#	esmf_lib: cannot test NMM
#	grib output: cannot test NMM

if      ( $NESTED == TRUE ) then
	set CORES = ( em_real em_b_wave em_quarter_ss          )
else if ( $NESTED != TRUE ) then
	set CORES = ( em_real em_b_wave em_quarter_ss nmm_real )
	set CORES = (                                 nmm_real )
	if ( $CHEM == TRUE ) then
		set CORES = ( em_real em_real )
	endif
	if ( $GLOBAL == TRUE ) then
		set CORES = ( em_real )
	endif
	if ( $ADAPTIVE == TRUE ) then
		set CORES = ( em_real )
	endif
	if ( $ESMF_LIB == TRUE ) then
		set CORES = ( em_real em_b_wave em_quarter_ss )
	endif
	if ( $IO_FORM_NAME[$IO_FORM] == io_grib1 ) then
		set CORES = ( em_real em_b_wave em_quarter_ss )
	endif
	if ( $FDDA == TRUE ) then
		set CORES = ( em_real )
	endif
endif

#	The b_wave case has binary input (4-byte only), the nmm
#	core has raw MPI calls, skip them if we are doing real*8 floats.

if      ( $REAL8 == TRUE ) then
	set CORES = ( em_real em_quarter_ss )
endif

if      ( ( $CHEM != TRUE ) && ( $FDDA != TRUE ) &&   ( $NESTED != TRUE ) && ( $REAL8 != TRUE ) && ( $GLOBAL != TRUE )   ) then
	set PHYSOPTS =	( 1 2 3 4 5 6 7 )
else if ( ( $CHEM != TRUE ) && ( $FDDA != TRUE ) && ( ( $NESTED == TRUE ) || ( $REAL8 == TRUE ) || ( $GLOBAL == TRUE ) ) ) then
	set PHYSOPTS =	( 1 2 3 4 5 6 )
else if ( ( $CHEM != TRUE ) && ( $FDDA == TRUE ) ) then
	if ( $FDDA2 == TRUE ) then
		set PHYSOPTS_FDDA = BOTH
	else
		set PHYSOPTS_FDDA = GRID
	endif
	if ( $PHYSOPTS_FDDA == GRID ) then
		set PHYSOPTS =	( 1 )
	else
		set PHYSOPTS =	( 1 2 3 )
	endif
else if ( $CHEM == TRUE ) then
	set PHYSOPTS =	( 1 2 3 4 5 6 )
endif

##LPC
##LPC --- run just nmm test
##LPC
set CORES = (nmm_real)

#	This is selecting the ideal physics options - mostly selecting BC options.
#	With no nesting, run all three ideal physics options.

if      ( $NESTED == TRUE ) then
	set Max_Ideal_Physics_Options = 2
else if ( $NESTED != TRUE ) then
	set Max_Ideal_Physics_Options = 3
endif

set CUR_DIR = `pwd`

#	How many domains to run (nest tests).  Only em_real and ideals use this.
#	The max is 3 due to the number of columns in the namelist that are 
#	currently filled in.

if      ( $NESTED == TRUE ) then
if      ( $dataset == jan00 ) then
cat >! dom_real << EOF
 time_step                           = 180,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 2,
 s_we                                = 1,     1,     1,
 e_we                                = 74,    31,    31,
 s_sn                                = 1,     1,     1,
 e_sn                                = 61,    31,    31,
 s_vert                              = 1,     1,     1,
 e_vert                              = 28,    28,    28,
 dx                                  = 30000, 10000,  3333.333333,
 dy                                  = 30000, 10000,  3333.333333,
 grid_id                             = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 0,     31,    11,
 j_parent_start                      = 0,     17,    11,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = 0
 num_moves                           = 3
 move_id                             = 2 , 2 , 2
 move_interval                       = 3 , 6 , 9
 move_cd_x                           = 1 , 1 , 1
 move_cd_y                           = 1 , 1 , 1
 use_adaptive_time_step              = $USE_ADAPTIVE_TIME_STEP
 step_to_output_time                 = $STEP_TO_OUTPUT_TIME
EOF
else if ( $dataset == jun01 ) then
cat >! dom_real << EOF
 time_step                           = 60,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 2,
 s_we                                = 1,     1,     1,
 e_we                                = 91,    31,    31,
 s_sn                                = 1,     1,     1,
 e_sn                                = 82,    31,    31,
 s_vert                              = 1,     1,     1,
 e_vert                              = 28,    28,    28,
 dx                                  = 10000,  3333.333333,  1111.111111,
 dy                                  = 10000,  3333.333333,  1111.111111,
 grid_id                             = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 0,     30,    11,
 j_parent_start                      = 0,     20,    11,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = 0
 num_moves                           = 3
 move_id                             = 2 , 2 , 2
 move_interval                       = 1 , 2 , 3
 move_cd_x                           = 1 , 1 , 1
 move_cd_y                           = 1 , 1 , 1
 use_adaptive_time_step              = $USE_ADAPTIVE_TIME_STEP
 step_to_output_time                 = $STEP_TO_OUTPUT_TIME
EOF
endif
cat >! dom_ideal << EOF
 max_dom                             = 2,
EOF
else if ( $NESTED != TRUE ) then
if      ( $dataset == jan00 ) then
cat >! dom_real << EOF
 time_step                           = 180,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 s_we                                = 1,     1,     1,
 e_we                                = 74,    31,    31,
 s_sn                                = 1,     1,     1,
 e_sn                                = 61,    31,    31,
 s_vert                              = 1,     1,     1,
 e_vert                              = 28,    28,    28,
 dx                                  = 30000, 10000,  3333,
 dy                                  = 30000, 10000,  3333,
 grid_id                             = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 0,     31,    30,
 j_parent_start                      = 0,     17,    30,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = 0
 use_adaptive_time_step              = $USE_ADAPTIVE_TIME_STEP
 step_to_output_time                 = $STEP_TO_OUTPUT_TIME
EOF
else if ( $dataset == jun01 ) then
cat >! dom_real << EOF
 time_step                           = 60,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 s_we                                = 1,     1,     1,
 e_we                                = 91,    31,    31,
 s_sn                                = 1,     1,     1,
 e_sn                                = 82,    31,    31,
 s_vert                              = 1,     1,     1,
 e_vert                              = 28,    28,    28,
 dx                                  = 10000,  3333.333333,  1111.111111,
 dy                                  = 10000,  3333.333333,  1111.111111,
 grid_id                             = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 0,     30,    11,
 j_parent_start                      = 0,     20,    11,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = 0
 use_adaptive_time_step              = $USE_ADAPTIVE_TIME_STEP
 step_to_output_time                 = $STEP_TO_OUTPUT_TIME
EOF
else if ( $dataset == global ) then
cat >! dom_real << EOF
 time_step                           = 600
 time_step_fract_num                 = 00
 time_step_fract_den                 = 112
 max_dom                             = 1,
 s_we                                = 1,     1,     1,
 e_we                                = 65,     41,    41,
 s_sn                                = 1,     1,     1,
 e_sn                                = 33,     81,    81,
 s_vert                              = 1,     1,     1,
 e_vert                              = 41,    41,    41,
 num_metgrid_levels                  = 27
 dx                                  = 625373.288,20000, 4000,
 dy                                  = 625373.288,20000, 4000,
 p_top_requested                     = 5000
 grid_id                             = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 0,     17,    17,
 j_parent_start                      = 0,     33,    33,
 parent_grid_ratio                   = 1,     5,     5,
 parent_time_step_ratio              = 1,     5,     5,
 feedback                            = 1,
 smooth_option                       = 00
EOF
endif
cat >! dom_ideal << EOF
 max_dom                             = 1,
EOF
endif

#	The em_real entire physics namelist.  Change what you want.

cat >! phys_real_1  << EOF
 mp_physics                          = 3,     3,     3,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 1,     1,     1,
 radt                                = 30,    30,    30,
 sf_sfclay_physics                   = 1,     1,     1,
 sf_surface_physics                  = 1,     1,     1,
 bl_pbl_physics                      = 1,     1,     1,
 bldt                                = 0,     0,     0,
 cu_physics                          = 1,     1,     0,
 cudt                                = 5,     5,     5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 5,
 mp_zero_out                         = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
EOF

cat >! dyn_real_SAFE  << EOF
 moist_adv_opt                       = 0,      0,      0,     
 scalar_adv_opt                      = 0,      0,      0,     
 chem_adv_opt                        = 0,      0,      0,     
 tke_adv_opt                         = 0,      0,      0,     
EOF

cat >! dyn_real_1  << EOF
 moist_adv_opt                       = 1,      1,      1,      
 scalar_adv_opt                      = 0,      0,      0,     
 chem_adv_opt                        = 0,      0,      0,     
 tke_adv_opt                         = 0,      0,      0,     
EOF

cat >! time_real_1  << EOF
 auxinput1_inname                    = "met_em.d<domain>.<date>"
EOF

cat >! nest_real_1  << EOF
 input_from_file                     = .true.,.false.,.false.
EOF

cat >! damp_real_1  << EOF
 damp_opt                            = 0,
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.01,   0.01,   0.01
EOF

cat >! phys_real_2 << EOF
 mp_physics                          = 4,     4,     4,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 1,     1,     1,
 radt                                = 30,    30,    30,
 sf_sfclay_physics                   = 2,     2,     2,
 sf_surface_physics                  = 2,     2,     2,
 bl_pbl_physics                      = 2,     2,     2,
 bldt                                = 0,     0,     0,
 cu_physics                          = 2,     2,     0,
 cudt                                = 5,     5,     5,
 slope_rad                           = 1,     1,     1,
 topo_shading                        = 0,     0,     0,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 4,
 mp_zero_out                         = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
EOF

cat >! dyn_real_2  << EOF
 moist_adv_opt                       = 1,      1,      1,      
 scalar_adv_opt                      = 0,      0,      0,     
 chem_adv_opt                        = 0,      0,      0,     
 tke_adv_opt                         = 0,      0,      0,     
EOF

if ( $GLOBAL == TRUE ) then
	cp dyn_real_SAFE dyn_real_2
endif

cat >! time_real_2  << EOF
 auxinput1_inname                    = "met_em.d<domain>.<date>"
EOF

cat >! nest_real_2  << EOF
 input_from_file                     = .true.,.false.,.false.
EOF

cat >! damp_real_2  << EOF
 damp_opt                            = 0,
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.01,   0.01,   0.01
EOF

cat >! phys_real_3 << EOF
 mp_physics                          = 5,     5,     5,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 2,     2,     2,
 radt                                = 30,    30,    30,
 sf_sfclay_physics                   = 2,     2,     2,
 sf_surface_physics                  = 3,     3,     3,
 bl_pbl_physics                      = 2,     2,     2,
 bldt                                = 0,     0,     0,
 cu_physics                          = 3,     3,     0,
 cudt                                = 5,     5,     5,
 omlcall                             = 1,
 oml_hml0                            = 50,
 oml_gamma                           = 0.14
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 6,
 mp_zero_out                         = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
EOF

cat >! dyn_real_3  << EOF
 moist_adv_opt                       = 2,      2,      2,     
 scalar_adv_opt                      = 0,      0,      0,     
 chem_adv_opt                        = 0,      0,      0,     
 tke_adv_opt                         = 0,      0,      0,     
EOF

cat >! time_real_3  << EOF
 auxinput1_inname                    = "met_em.d<domain>.<date>"
EOF

cat >! nest_real_3  << EOF
 input_from_file                     = .true.,.false.,.false.
EOF

cat >! damp_real_3  << EOF
 damp_opt                            = 1,
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.01,   0.01,   0.01
EOF

cat >! phys_real_4 << EOF
 mp_physics                          = 6,     6,     6,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 2,     2,     2,
 radt                                = 30,    30,    30,
 sf_sfclay_physics                   = 2,     2,     2,
 sf_surface_physics                  = 2,     2,     2,
 bl_pbl_physics                      = 2,     2,     2,
 bldt                                = 0,     0,     0,
 cu_physics                          = 5,     5,     0,
 cudt                                = 5,     5,     5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 sf_urban_physics                    = 1,     1,     1,
 surface_input_source                = 1,
 num_soil_layers                     = 4,
 mp_zero_out                         = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
EOF

cat >! dyn_real_4  << EOF
 moist_adv_opt                       = 2,      2,      2,     
 scalar_adv_opt                      = 0,      0,      0,     
 chem_adv_opt                        = 0,      0,      0,     
 tke_adv_opt                         = 0,      0,      0,     
EOF

cat >! time_real_4  << EOF
 auxinput1_inname                    = "met_em.d<domain>.<date>"
EOF

cat >! nest_real_4  << EOF
 input_from_file                     = .true.,.false.,.false.
EOF

cat >! damp_real_4  << EOF
 damp_opt                            = 1,
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.01,   0.01,   0.01
EOF

cat >! phys_real_5 << EOF
 mp_physics                          = 10,    10,    10,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 1,     1,     1,
 radt                                = 30,    30,    30,
 sf_sfclay_physics                   = 7,     7,     7,
 sf_surface_physics                  = 1,     1,     1,
 bl_pbl_physics                      = 7,     7,     7,
 bldt                                = 0,     0,     0,
 cu_physics                          = 99,    99,    0,
 cudt                                = 0,     0,     0,
 slope_rad                           = 1,     1,     1, 
 topo_shading                        = 0,     0,     0, 
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 sf_urban_physics                    = 1,     1,     1,
 surface_input_source                = 1,
 num_soil_layers                     = 5,
 mp_zero_out                         = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 levsiz                              = 59
 paerlev                             = 29
 cam_abs_freq_s                      = 21600
 cam_abs_dim1                        = 4
 cam_abs_dim2                        = 28
EOF

cat >! dyn_real_5  << EOF
 moist_adv_opt                       = 2,      2,      2,     
 scalar_adv_opt                      = 0,      0,      0,     
 chem_adv_opt                        = 0,      0,      0,     
 tke_adv_opt                         = 0,      0,      0,     
EOF

cat >! time_real_5  << EOF
 auxinput1_inname                    = "met_em.d<domain>.<date>"
EOF

cat >! nest_real_5  << EOF
 input_from_file                     = .true.,.false.,.false.
EOF

cat >! damp_real_5  << EOF
 damp_opt                            = 3,
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.05,   0.05,   0.05
EOF

cat >! phys_real_6 << EOF
 mp_physics                          = 7,     7,     7,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 2,     2,     2,
 radt                                = 30,    30,    30,
 sf_sfclay_physics                   = 7,     7,     7,
 sf_surface_physics                  = 1,     1,     1,
 bl_pbl_physics                      = 7,     7,     7,
 bldt                                = 0,     0,     0,
 cu_physics                          = 1,     1,     0,
 cudt                                = 0,     0,     0,
 omlcall                             = 1,
 oml_hml0                            = 50,
 oml_gamma                           = 0.14
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 sf_urban_physics                    = 1,     1,     1,
 surface_input_source                = 1,
 num_soil_layers                     = 5,
 mp_zero_out                         = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 levsiz                              = 59
 paerlev                             = 29
 cam_abs_freq_s                      = 21600
 cam_abs_dim1                        = 4
 cam_abs_dim2                        = 28
EOF

cat >! dyn_real_6  << EOF
 moist_adv_opt                       = 0,      0,      0,     
 scalar_adv_opt                      = 0,      0,      0,     
 chem_adv_opt                        = 0,      0,      0,     
 tke_adv_opt                         = 0,      0,      0,     
EOF

cat >! time_real_6  << EOF
 auxinput1_inname                    = "met_em.d<domain>.<date>"
EOF

cat >! nest_real_6  << EOF
 input_from_file                     = .true.,.false.,.false.
EOF

cat >! damp_real_6  << EOF
 damp_opt                            = 3,
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.05,   0.05,   0.05
EOF

cat >! phys_real_7 << EOF
 mp_physics                          = 8,     8,     8,
 ra_lw_physics                       = 3,     3,     3,
 ra_sw_physics                       = 3,     3,     3,
 radt                                = 30,    30,    30,
 sf_sfclay_physics                   = 2,     2,     2,
 sf_surface_physics                  = 2,     2,     2,
 bl_pbl_physics                      = 2,     2,     2,
 bldt                                = 0,     0,     0,
 cu_physics                          = 99,    99,    0,
 cudt                                = 5,     5,     5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 sf_urban_physics                    = 1,     1,     1,
 surface_input_source                = 1,
 num_soil_layers                     = 4,
 mp_zero_out                         = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 levsiz                              = 59
 paerlev                             = 29
 cam_abs_freq_s                      = 21600
 cam_abs_dim1                        = 4
 cam_abs_dim2                        = 28
EOF

cat >! dyn_real_7  << EOF
 moist_adv_opt                       = 0,      0,      0,     
 scalar_adv_opt                      = 0,      0,      0,     
 chem_adv_opt                        = 0,      0,      0,     
 tke_adv_opt                         = 0,      0,      0,     
EOF

cat >! time_real_7  << EOF
 auxinput1_inname                    = "met_em.d<domain>.<date>"
EOF

cat >! nest_real_7  << EOF
 input_from_file                     = .true.,.false.,.false.
EOF

cat >! damp_real_7  << EOF
 damp_opt                            = 3,
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.05,   0.05,   0.05
EOF

if ( $GLOBAL == TRUE ) then
	sed -e 's/ cam_abs_dim2 *= [0-9][0-9]/ cam_abs_dim2 = 41/g' phys_real_5 >! phys_foo
	mv phys_foo phys_real_7
	cp dyn_real_SAFE dyn_real_1
	cp dyn_real_SAFE dyn_real_2
	cp dyn_real_SAFE dyn_real_3
	cp dyn_real_SAFE dyn_real_4
	cp dyn_real_SAFE dyn_real_5
	cp dyn_real_SAFE dyn_real_6
	cp dyn_real_SAFE dyn_real_7
endif

cat >! fdda_real_1 << EOF
 grid_fdda                           = 1,     1,     1,
 gfdda_inname                        = "wrffdda_d<domain>",
 gfdda_end_h                         = 24,    24,    24,
 gfdda_interval_m                    = 360,   360,   360,
 fgdt                                = 0,     0,     0,
 if_no_pbl_nudging_uv                = 0,     0,     1,
 if_no_pbl_nudging_t                 = 0,     0,     1,
 if_no_pbl_nudging_q                 = 0,     0,     1,
 if_zfac_uv                          = 0,     0,     1,
  k_zfac_uv                          = 10,   10,     1,
 if_zfac_t                           = 0,     0,     1,
  k_zfac_t                           = 10,   10,     1,
 if_zfac_q                           = 0,     0,     1,
  k_zfac_q                           = 10,   10,     1,
 guv                                 = 0.0003,     0.0003,     0.0003,
 gt                                  = 0.0003,     0.0003,     0.0003,
 gq                                  = 0.0003,     0.0003,     0.0003,
 if_ramping                          = 1,
 dtramp_min                          = 360.0,
 io_form_gfdda                       = 2,
EOF

cat >! fdda_real_time_1 << EOF
EOF

cat >! fdda_real_2 << EOF
 obs_nudge_opt                       = 1,1,1,1,1
 max_obs                             = 150000,
 obs_nudge_wind                      = 1,1,1,1,1
 obs_coef_wind                       = 6.E-4,6.E-4,6.E-4,6.E-4,6.E-4
 obs_nudge_temp                      = 1,1,1,1,1
 obs_coef_temp                       = 6.E-4,6.E-4,6.E-4,6.E-4,6.E-4
 obs_nudge_mois                      = 1,1,1,1,1
 obs_coef_mois                       = 6.E-4,6.E-4,6.E-4,6.E-4,6.E-4
 obs_rinxy                           = 240.,240.,180.,180,180
 obs_rinsig                          = 0.1,
 obs_twindo                          = 40.
 obs_npfi                            = 10,
 obs_ionf                            = 2,
 obs_idynin                          = 0,
 obs_dtramp                          = 40.,
 obs_ipf_errob                       = .true.
 obs_ipf_nudob                       = .true.
 obs_ipf_in4dob                      = .true.
EOF

cat >! fdda_real_time_2 << EOF
 auxinput11_interval_s               = 180
 auxinput11_end_h                    = 6
EOF

cat >! fdda_real_3 << EOF
 grid_fdda                           = 1,     1,     1,
 gfdda_inname                        = "wrffdda_d<domain>",
 gfdda_end_h                         = 24,    24,    24,
 gfdda_interval_m                    = 360,   360,   360,
 fgdt                                = 0,     0,     0,
 if_no_pbl_nudging_uv                = 0,     0,     1,
 if_no_pbl_nudging_t                 = 0,     0,     1,
 if_no_pbl_nudging_q                 = 0,     0,     1,
 if_zfac_uv                          = 0,     0,     1,
  k_zfac_uv                          = 10,   10,     1,
 if_zfac_t                           = 0,     0,     1,
  k_zfac_t                           = 10,   10,     1,
 if_zfac_q                           = 0,     0,     1,
  k_zfac_q                           = 10,   10,     1,
 guv                                 = 0.0003,     0.0003,     0.0003,
 gt                                  = 0.0003,     0.0003,     0.0003,
 gq                                  = 0.0003,     0.0003,     0.0003,
 if_ramping                          = 1,
 dtramp_min                          = 360.0,
 io_form_gfdda                       = 2,
 obs_nudge_opt                       = 1,1,1,1,1
 max_obs                             = 150000,
 obs_nudge_wind                      = 1,1,1,1,1
 obs_coef_wind                       = 6.E-4,6.E-4,6.E-4,6.E-4,6.E-4
 obs_nudge_temp                      = 1,1,1,1,1
 obs_coef_temp                       = 6.E-4,6.E-4,6.E-4,6.E-4,6.E-4
 obs_nudge_mois                      = 1,1,1,1,1
 obs_coef_mois                       = 6.E-4,6.E-4,6.E-4,6.E-4,6.E-4
 obs_rinxy                           = 240.,240.,180.,180,180
 obs_rinsig                          = 0.1,
 obs_twindo                          = 40.
 obs_npfi                            = 10,
 obs_ionf                            = 2,
 obs_idynin                          = 0,
 obs_dtramp                          = 40.,
 obs_ipf_errob                       = .true.
 obs_ipf_nudob                       = .true.
 obs_ipf_in4dob                      = .true.
EOF

cat >! fdda_real_time_3 << EOF
 auxinput11_interval_s               = 180
 auxinput11_end_h                    = 6
EOF

#	Tested options for ideal case em_b_wave.  Modifying these
#	parameters is acceptable.  Adding to these requires changes
#	to the ideal namelist build below.

cat >! phys_b_wave_1a << EOF
 diff_opt                            = 1,
 km_opt                              = 1,
 damp_opt                            = 0,
EOF
cat >! phys_b_wave_1b << EOF
 mp_physics                          = 1,     1,     1,
EOF
cat >! phys_b_wave_1c << EOF
 non_hydrostatic                     = .true., .true., .true.,
EOF
cat >! phys_b_wave_1d  << EOF
 input_from_file                     = .true.,.false.,.false.
EOF

cat >! phys_b_wave_2a << EOF
 diff_opt                            = 1,
 km_opt                              = 1,
 damp_opt                            = 0,
EOF
cat >! phys_b_wave_2b << EOF
 mp_physics                          = 1,     1,     1,
EOF
cat >! phys_b_wave_2c << EOF
 non_hydrostatic                     = .false., .false., .false.,
EOF
cat >! phys_b_wave_2d  << EOF
 input_from_file                     = .true.,.false.,.false.
EOF

cat >! phys_b_wave_3a << EOF
 diff_opt                            = 1,
 km_opt                              = 1,
 damp_opt                            = 0,
EOF
cat >! phys_b_wave_3b << EOF
 mp_physics                          = 2,     2,     2,
EOF
cat >! phys_b_wave_3c << EOF
 non_hydrostatic                     = .false., .false., .false.,
EOF
cat >! phys_b_wave_3d  << EOF
 input_from_file                     = .true.,.false.,.false.
EOF

#	Tested options for ideal case em_quarter_ss.  Modifying these
#	parameters is acceptable.  Adding to these requires changes
#	to the ideal namelist build below.

cat >! phys_quarter_ss_1a << EOF
 diff_opt                            = 1,
 km_opt                              = 1,
 damp_opt                            = 0,
EOF
cat >! phys_quarter_ss_1b << EOF
 mp_physics                          = 1,     1,     1,
EOF
cat >! phys_quarter_ss_1c << EOF
 moist_adv_opt                       = 1,      1,      1,
 scalar_adv_opt                      = 1,      1,      1,
 chem_adv_opt                        = 1,      1,      1,
 tke_adv_opt                         = 1,      1,      1,
 non_hydrostatic                     = .true., .true., .true.,
EOF
cat >! phys_quarter_ss_1d  << EOF
 input_from_file                     = .true.,.false.,.false.
EOF
cat >! phys_quarter_ss_1e << EOF
 periodic_x                          = .false.,.false.,.false.,
 open_xs                             = .true., .false.,.false.,
 open_xe                             = .true., .false.,.false.,
 periodic_y                          = .false.,.false.,.false.,
 open_ys                             = .true., .false.,.false.,
 open_ye                             = .true., .false.,.false.,
EOF
cat >! phys_quarter_ss_1f << EOF
 sf_sfclay_physics                   = 0,     0,     0,
EOF

cat >! phys_quarter_ss_2a << EOF
 diff_opt                            = 2,
 km_opt                              = 2,
 damp_opt                            = 1,
EOF
cat >! phys_quarter_ss_2b << EOF
 mp_physics                          = 1,     1,     1,
EOF
cat >! phys_quarter_ss_2c << EOF
 moist_adv_opt                       = 2,      2,      2,
 scalar_adv_opt                      = 2,      2,      2,
 chem_adv_opt                        = 2,      2,      2,
 tke_adv_opt                         = 2,      2,      2,
 non_hydrostatic                     = .true., .true., .true.,
EOF
cat >! phys_quarter_ss_2d  << EOF
 input_from_file                     = .true.,.false.,.false.
EOF
cat >! phys_quarter_ss_2e << EOF
 periodic_x                          = .false.,.false.,.false.,
 open_xs                             = .true., .false.,.false.,
 open_xe                             = .true., .false.,.false.,
 periodic_y                          = .false.,.false.,.false.,
 open_ys                             = .true., .false.,.false.,
 open_ye                             = .true., .false.,.false.,
EOF
cat >! phys_quarter_ss_2f << EOF
 sf_sfclay_physics                   = 1,     1,     1,
EOF

cat >! phys_quarter_ss_3a << EOF
 diff_opt                            = 2,
 km_opt                              = 3,
 damp_opt                            = 1,
EOF
cat >! phys_quarter_ss_3b << EOF
 mp_physics                          = 2,     2,     2,
EOF
cat >! phys_quarter_ss_3c << EOF
 moist_adv_opt                       = 1,      1,      1,
 scalar_adv_opt                      = 1,      1,      1,
 chem_adv_opt                        = 1,      1,      1,
 tke_adv_opt                         = 1,      1,      1,
 non_hydrostatic                     = .false., .false., .false.,
EOF
cat >! phys_quarter_ss_3d  << EOF
 input_from_file                     = .true.,.false.,.false.
EOF
cat >! phys_quarter_ss_3e << EOF
 periodic_x                          = .true., .false.,.false.,
 open_xs                             = .false.,.false.,.false.,
 open_xe                             = .false.,.false.,.false.,
 periodic_y                          = .true., .false.,.false.,
 open_ys                             = .false.,.false.,.false.,
 open_ye                             = .false.,.false.,.false.,
EOF
cat >! phys_quarter_ss_3f << EOF
 sf_sfclay_physics                   = 1,     1,     1,
EOF

if      ( $IO_FORM_WHICH[$IO_FORM] == IO ) then
cat >! io_format << EOF
 io_form_history                     = $IO_FORM
 io_form_restart                     = $IO_FORM
 io_form_input                       = $IO_FORM
 io_form_boundary                    = $IO_FORM
EOF
else if ( $IO_FORM_WHICH[$IO_FORM] == I  ) then
cat >! io_format << EOF
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = $IO_FORM
 io_form_boundary                    = $IO_FORM
EOF
else if ( $IO_FORM_WHICH[$IO_FORM] ==  O ) then
cat >! io_format << EOF
 io_form_history                     = $IO_FORM
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
EOF
endif


if ( $dataset == jun01 ) then
	set filetag_real=2001-06-11_12:00:00
else if ( $dataset == jan00 ) then
	set filetag_real=2000-01-24_12:00:00
else if ( $dataset == chem ) then
	set filetag_real = ( 2006-04-06_00:00:00  2006-04-06_12:00:00 )
else if ( $dataset == global ) then
	set filetag_real=2008-01-02_12:00:00
endif

set filetag_ideal=0001-01-01_00:00:00
#DAVE###################################################
echo did phys, set date to $filetag_real
banner 2
#set ans = "$<"
#DAVE###################################################

#####################################################################

#	Set up info for particular architectures

set ARCH = ( `uname` )

set ZAP_SERIAL          = FALSE
set ZAP_OPENMP          = FALSE
set SERIALRUNCOMMAND	= 
set OMPRUNCOMMAND	= 
set MPIRUNCOMMANDPOST   = 

touch version_info
if ( $ARCH[1] == AIX ) then
	set DEF_DIR             = $home
	set TMPDIR              = /ptmp/$user
	# keep stuff out of $HOME and /ptmp/$USER
	# this allows multiple regressions tests to run simultaneously
	# extend this to other machines later
	if      ( ( `hostname | cut -c 1-2` == bs ) && ( ! $?LOADL_JOB_NAME ) ) then
		echo "${0}: ERROR::  This batch script must be submitted via"
		echo "${0}:          LoadLeveler on an AIX machine\!"
		exit
	else if   ( `hostname | cut -c 1-2` == bs ) then
		set job_id              = `echo ${LOADL_JOB_NAME} | cut -f2 -d'.'`
		set DEF_DIR             = /ptmp/$user/wrf_regression.${job_id}
		set TMPDIR              = $DEF_DIR
		if ( -d $DEF_DIR ) then
			echo "${0}: ERROR::  Directory ${DEF_DIR} exists, please remove it"
			exit ( 1 ) 
		else
			mkdir -p $DEF_DIR
			echo "See ${DEF_DIR}/wrftest.output and other files in ${DEF_DIR} for test results"
		endif
		set CUR_DIR = ${LOADL_STEP_INITDIR}
	else if   ( ( `hostname | cut -c 1-2` == bv ) || ( `hostname | cut -c 1-2` == be ) ) then
		set job_id              = $LSB_JOBID
		set DEF_DIR             = /ptmp/$user/wrf_regression.${job_id}
		set TMPDIR              = $DEF_DIR
		if ( -d $DEF_DIR ) then
			echo "${0}: ERROR::  Directory ${DEF_DIR} exists, please remove it"
			exit ( 1 ) 
		else
			mkdir -p $DEF_DIR
			echo "See ${DEF_DIR}/wrftest.output and other files in ${DEF_DIR} for test results"
		endif
	else if ( ( ( `hostname | cut -c 1-2` != be ) && ( `hostname | cut -c 1-2` != bv ) ) && ( ! $?LOADL_JOB_NAME ) ) then
		echo "${0}: ERROR::  This batch script must be submitted via"
		echo "${0}:          LoadLeveler on an AIX machine\!"
		exit
	else if   ( ( `hostname | cut -c 1-2` != be ) && ( `hostname | cut -c 1-2` != bv ) ) then
		set job_id              = `echo ${LOADL_JOB_NAME} | cut -f2 -d'.'`
		set DEF_DIR             = /ptmp/$user/wrf_regression.${job_id}
		set TMPDIR              = $DEF_DIR
		if ( -d $DEF_DIR ) then
			echo "${0}: ERROR::  Directory ${DEF_DIR} exists, please remove it"
			exit ( 1 ) 
		else
			mkdir -p $DEF_DIR
			echo "See ${DEF_DIR}/wrftest.output and other files in ${DEF_DIR} for test results"
		endif
		set CUR_DIR = ${LOADL_STEP_INITDIR}
	endif
	if ( ! -d $TMPDIR ) mkdir $TMPDIR
	set MAIL                = /usr/bin/mailx
	set COMPOPTS    = ( 1 2 3 )
	set COMPOPTS_NO_NEST = 0
	set COMPOPTS_NEST_STATIC = 1
	set COMPOPTS_NEST_PRESCRIBED = 2
	set Num_Procs		= 4
	set OPENMP 		= $Num_Procs
        setenv MP_PROCS  $Num_Procs
        setenv MP_RMPOOL 1
	if      ( `hostname | cut -c 1-2` == bs ) then
		set MPIRUNCOMMAND       =  poe 
	else if ( `hostname | cut -c 1-2` == bv ) then
		set MPIRUNCOMMAND       =  mpirun.lsf
	else if ( `hostname | cut -c 1-2` == be ) then
		set MPIRUNCOMMAND       =  /contrib/mpiruns/be/mpirun.lsf
	else if ( ( `hostname | cut -c 1-2` != bs ) && \
	          ( `hostname | cut -c 1-2` != bv ) && ( `hostname | cut -c 1-2` != be ) ) then
		set MPIRUNCOMMAND       =  poe
	endif
	if ( $CHEM == TRUE ) then
		set ZAP_OPENMP		= TRUE
	else if ( $CHEM == FALSE ) then
		set ZAP_OPENMP		= FALSE
	endif
# check compiler version, JM
        lslpp -i | grep xlf | grep ' xlfcmp ' | head -1
        set xlfvers=`lslpp -i | grep xlf | grep ' xlfcmp ' | head -1 | awk '{print $2}' | sed 's/\...*$//'`
        if ( ( $xlfvers > 9 ) && ( $NESTED == TRUE ) ) then
#		set ZAP_OPENMP		= TRUE
        endif
# end of compiler check, JM
	echo "Compiler version info: " >! version_info
	echo "FORTRAN:        " `lslpp -l | grep xlfrte | head -1 | awk '{print $1 "   " $2}'` >>! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	echo "AIX:            " `lslpp -l | grep bos.mp | head -1 | awk '{print $1 "   " $2}'` >>! version_info
	echo " " >>! version_info
	setenv MP_SHARED_MEMORY yes
else if ( $ARCH[1] == Darwin ) then
	if      ( ( `hostname` == stink )               && ( -d /stink/gill/Regression_Tests ) ) then
		set DEF_DIR	= /stink/gill/Regression_Tests/wrf_regression
		mkdir $DEF_DIR
	else 
		echo "We at least need a directory from which to do stuff"
		exit ( 2 ) 
	endif
	set TMPDIR              = .
	set MAIL		= /usr/bin/mailx
	if      ( $LINUX_COMP == PGI ) then
		set COMPOPTS	= (  1 2  3 )
		set ZAP_OPENMP	= FALSE
	else if ( $LINUX_COMP == G95 ) then
		set COMPOPTS	= ( 13 0 14 )
		set ZAP_OPENMP	= TRUE
	endif
	set COMPOPTS_NO_NEST = 0
	set COMPOPTS_NEST_STATIC = 1
	set COMPOPTS_NEST_PRESCRIBED = 2
	set Num_Procs		= 4
	set OPENMP 		= 2
	cat >! `pwd`/machfile << EOF
`hostname`
`hostname`
`hostname`
`hostname`
EOF
        set Mach = `pwd`/machfile
	set SERIALRUNCOMMAND	= 
	set OMPRUNCOMMAND	= 
	echo "Compiler version info: " >! version_info
	if      ( $LINUX_COMP == PGI ) then
		set MPIRUNCOMMAND 	= ( /usr/local/mpich2-1.0.6p1-pgi/bin/mpirun -np $Num_Procs )
		pgf90 -V | head -2 | tail -1 >>&! version_info
	else if ( $LINUX_COMP == G95 ) then
		set MPIRUNCOMMAND 	= ( /stink/gill/local/bin/mpirun -np $Num_Procs )
		g95 -v |& grep gcc >>&! version_info
	endif
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
	ps -A | grep mpd | grep -v grep >& /dev/null
	set ok = $status
	if ( $ok != 0 ) then
		echo starting an mpd process
		mpd &
	endif
else if ( $ARCH[1] == OSF1 && $clrm == 1 ) then
	set DEF_DIR		= /`hostname | cut -d. -f1`/$user
	set TMPDIR		= /mmmtmp/$user
	set MAIL		= /usr/bin/mailx
	if      ( ( $NESTED == TRUE ) && ( $RSL_LITE != TRUE ) ) then
		set COMPOPTS	= ( 2 4 6 )
	else if ( ( $NESTED != TRUE ) && ( $RSL_LITE != TRUE ) ) then
		set COMPOPTS    = ( 1 3 6 )
	else if ( ( $NESTED == TRUE ) && ( $RSL_LITE == TRUE ) ) then
		set COMPOPTS	= ( 2 4 5 )
	else if ( ( $NESTED != TRUE ) && ( $RSL_LITE == TRUE ) ) then
		set COMPOPTS	= ( 1 3 5 )
	endif
	set Num_Procs		= 4
	set OPENMP 		= 0
	set ZAP_OPENMP		= TRUE
	cat >! $TMPDIR/machfile << EOF
service03
service04
service05
service06
EOF
        set Mach = $TMPDIR/machfile
	set MPIRUNCOMMAND 	= ( mpirun -np $Num_Procs -machinefile $Mach )
	echo "Compiler version info: " >! version_info
	f90 -version >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( ( $ARCH[1] == Linux ) && ( `hostname` == bay-mmm ) ) then
	set DEF_DIR	= /data3/mp/${user}/`hostname`
	if ( ! -d $DEF_DIR ) mkdir $DEF_DIR
	set TMPDIR		= .
	set MAIL		= /bin/mail
	if      ( $LINUX_COMP == PGI ) then
		if      ( ( $NESTED == TRUE ) && ( $RSL_LITE != TRUE ) ) then
			set COMPOPTS	= ( 2 4 5 )
		else if ( ( $NESTED != TRUE ) && ( $RSL_LITE != TRUE ) ) then
			set COMPOPTS    = ( 1 3 5 )
		else if ( ( $NESTED == TRUE ) && ( $RSL_LITE == TRUE ) ) then
			set COMPOPTS	= ( 2 4 6 )
		else if ( ( $NESTED != TRUE ) && ( $RSL_LITE == TRUE ) ) then
			set COMPOPTS	= ( 1 3 6 )
		endif
	else if ( $LINUX_COMP == INTEL ) then
		if        ( $NESTED == TRUE )                            then
			set COMPOPTS	= ( 8 10 11 )
		else if   ( $NESTED != TRUE )                            then
			set COMPOPTS	= ( 7  9 11 )
		endif
	endif
	set Num_Procs		= 2
	set OPENMP		= $Num_Procs
	cat >! machfile << EOF
`hostname`
`hostname`
`hostname`
`hostname`
EOF
	set Mach		= `pwd`/machfile
	if ( $CHEM == TRUE ) then
		set ZAP_OPENMP		= TRUE
	else if ( $CHEM == FALSE ) then
		set ZAP_OPENMP		= FALSE
	endif
	if ( $LINUX_COMP == INTEL ) then
		set ZAP_OPENMP		= TRUE
	endif
	set MPIRUNCOMMAND       = ( mpirun -np $Num_Procs -machinefile $Mach )
	echo "Compiler version info: " >! version_info
	if      ( $LINUX_COMP == PGI ) then
		pgf90 -V >>&! version_info
	else if ( $LINUX_COMP == INTEL ) then
		ifort -v >>&! version_info
	endif
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( ( $ARCH[1] == Linux ) && ( `hostname | cut -c 1-2` ==  ln ) ) then
	set DEF_DIR	= /ptmp/${user}/wrf_regtest
	if ( ! -d $DEF_DIR ) mkdir $DEF_DIR
	set TMPDIR		= .
	set MAIL		= /bin/mail
	if      ( $LINUX_COMP == PGI ) then
		if      ( ( $NESTED == TRUE ) && ( $RSL_LITE != TRUE ) ) then
			set COMPOPTS    = ( 4 2 3 )
		else if ( ( $NESTED != TRUE ) && ( $RSL_LITE != TRUE ) ) then
			set COMPOPTS    = ( 1 2 3 )
		endif
	endif
	set Num_Procs		= 4
	set OPENMP		= 2
	cat >! machfile << EOF
`hostname`
`hostname`
`hostname`
`hostname`
EOF
	set Mach		= `pwd`/machfile
	set ZAP_OPENMP		= TRUE
	set MPIRUNCOMMAND       =  mpirun.lsf
	echo "Compiler version info: " >! version_info
	if      ( $LINUX_COMP == PGI ) then
		pgf90 -V >>&! version_info
	else if ( $LINUX_COMP == INTEL ) then
		ifort -v >>&! version_info
	endif
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( ( $ARCH[1] == Linux ) && ( `hostname` == loquat ) ) then
	set job_id              = $$
	set DEF_DIR             = /loquat2/$user/wrf_regression.${job_id}
	set TMPDIR              = $DEF_DIR
	if ( -d $DEF_DIR ) then
		echo "${0}: ERROR::  Directory ${DEF_DIR} exists, please remove it"
		exit ( 1 ) 
	else
		mkdir -p $DEF_DIR
		echo "See directory ${DEF_DIR}/ for wrftest.output and other test results"
	endif
	set MAIL		= /bin/mail
	if      ( $LINUX_COMP == PGI ) then
		if      ( ( $NESTED == TRUE ) && ( $RSL_LITE != TRUE ) ) then
			set COMPOPTS	= ( 2 4 5 )
		else if ( ( $NESTED != TRUE ) && ( $RSL_LITE != TRUE ) ) then
			set COMPOPTS    = ( 1 3 5 )
		else if ( ( $NESTED == TRUE ) && ( $RSL_LITE == TRUE ) ) then
			set COMPOPTS	= ( 2 4 6 )
		else if ( ( $NESTED != TRUE ) && ( $RSL_LITE == TRUE ) ) then
			set COMPOPTS	= ( 1 3 6 )
		endif
	else if ( $LINUX_COMP == INTEL ) then
		if        ( $NESTED == TRUE )                            then
			set COMPOPTS	= ( 8 10 11 )
		else if   ( $NESTED != TRUE )                            then
			set COMPOPTS	= ( 7  9 11 )
		endif
	endif
	set Num_Procs		= 2
	set OPENMP		= $Num_Procs
	cat >! machfile << EOF
`hostname`
`hostname`
`hostname`
`hostname`
EOF
	set Mach		= `pwd`/machfile
	if ( $CHEM == TRUE ) then
		set ZAP_OPENMP		= TRUE
	else if ( $CHEM == FALSE ) then
		set ZAP_OPENMP		= FALSE
	endif
	if ( $LINUX_COMP == INTEL ) then
		set ZAP_OPENMP		= TRUE
	endif
	set MPIRUNCOMMAND       = ( mpirun -np $Num_Procs -machinefile $Mach )
	echo "Compiler version info: " >! version_info
	if      ( $LINUX_COMP == PGI ) then
		pgf90 -V >>&! version_info
	else if ( $LINUX_COMP == INTEL ) then
		ifort -v >>&! version_info
	endif
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( `hostname` == tempest ) then
	set DEF_DIR	= /ptmp/${user}/wrf_regtest.${QSUB_REQID}
	if ( ! -d $DEF_DIR ) mkdir $DEF_DIR
	set TMPDIR		= .
	set MAIL		= /usr/sbin/Mail
	set COMPOPTS		= ( 1 2 3 )
	set Num_Procs		= 2
	set OPENMP		= $Num_Procs
	set Mach		= `pwd`/machfile
	set ZAP_OPENMP		= TRUE
	set MPIRUNCOMMAND       = ( mpirun -np $Num_Procs )
	echo "Compiler version info: " >! version_info
	f90 -version >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( ( $ARCH[1] == Linux ) && ( `hostname` == master ) ) then
	set DEF_DIR		= /big6/gill/DO_NOT_REMOVE_DIR
	set TMPDIR		= .
	set MAIL		= /bin/mail
	if      ( $LINUX_COMP == PGI ) then
		if        ( $NESTED == TRUE )                            then
			set COMPOPTS	= ( 2 4 5 )
		else if ( ( $NESTED != TRUE ) && ( $RSL_LITE == TRUE ) ) then
			set COMPOPTS	= ( 1 3 6 )
		else if ( ( $NESTED != TRUE ) && ( $RSL_LITE != TRUE ) ) then
			set COMPOPTS	= ( 1 3 5 )
		endif
	else if ( $LINUX_COMP == INTEL ) then
		if        ( $NESTED == TRUE )                            then
			set COMPOPTS	= ( 8 10 11 )
		else if   ( $NESTED != TRUE )                            then
			set COMPOPTS	= ( 7  9 11 )
		endif
	endif
	set Num_Procs		= 4
	set OPENMP		= 2
	cat >! machfile << EOF
node3
node3
node4
node4
EOF
	set Mach		= `pwd`/machfile
	if ( $CHEM == TRUE ) then
		set ZAP_OPENMP		= TRUE
	else if ( $CHEM == FALSE ) then
		set ZAP_OPENMP		= FALSE
	endif
	if ( $LINUX_COMP == INTEL ) then
		set ZAP_OPENMP		= TRUE
	endif
	set MPIRUNCOMMAND       = ( mpirun -v -np $Num_Procs -machinefile $Mach -nolocal )
	set MPIRUNCOMMANDPOST   = "< /dev/null"
	echo "Compiler version info: " >! version_info
	if      ( $LINUX_COMP == PGI ) then
		pgf90 -V >>&! version_info
	else if ( $LINUX_COMP == INTEL ) then
		ifort -v >>&! version_info
	endif
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else
	echo "Unrecognized architecture for regression test"  >! error_message
	echo `uname`                                          >> error_message
	echo `hostname`                                       >> error_message
	$MAIL -s "Unknown architecture $ARCH[1] " $FAIL_MAIL   < error_message
	exit ( 1 )
endif

#####################################################################
#DAVE###################################################
echo did the arch specific stuff
banner 3
#set ans = "$<"
#DAVE###################################################

if ( $FDDA == TRUE ) then
	if      ( ( $PHYSOPTS_FDDA == GRID ) && ( $ZAP_OPENMP == FALSE ) ) then
		set ZAP_OPENMP = FALSE
	else if ( $PHYSOPTS_FDDA == BOTH ) then
		set ZAP_OPENMP = TRUE
	endif
endif

#	First of all, in which particular directory do we start.

cd $DEF_DIR

#	We want to keep the old regression stuff around

if ( -d regression_test ) then
	if ( -d regression_test.old ) then
		/bin/rm -fr regression_test.old
	endif
	/bin/mv regression_test regression_test.old
endif

#	Go to the regression test directory

mkdir regression_test
set ok = $status
if ( $ok != 0 ) then
	echo "Gee, I cannot make a directory in $DEF_DIR"  >! error_message
	echo `pwd`                                         >> error_message
	echo `\ls -ls`                                     >> error_message
	$MAIL -s "$DEF_DIR not writable $ARCH[1] " $FAIL_MAIL < error_message
	exit ( 1 )
else
	pushd regression_test
endif

if      ( $acquire_from == "cvs" ) then

	#	Checkout the most recent version of WRF from the NCAR cvs repository,
	#	and pick up the required input data from the anonymous ftp site.

	cvs checkout -D $thedate WRFV3
	find ./WRFV3 -exec touch \{\} \;
	ftp -n ftp.ucar.edu < ftp_script_data


else if ( $acquire_from == "filearg" ) then

	#	A tar file of the WRF source was provided, so that is used, along with 
	#	the required input data files from the ftp site.

	tar xvf $thefile
	cd WRFV3
	clean -a
	cd ..
	ftp -n ftp.ucar.edu < ftp_script_data

else if ( $acquire_from == "environment" ) then

	#	A tar file of WRF is assumed to be available.

	tar xvf $thefile

endif

#	And we can stick the input data where we want, the WRFV3 directory has been created.

( cd WRFV3/test/em_real  ; ln -sf $thedataem/* . ) 
#( cd WRFV3/test/nmm_real ; ln -s $thedatanmm/wrf_real* . ; cp $thedatanmm/namelist.input.regtest . )
 ( cd WRFV3/test/nmm_real ; ln -s $thedatanmm/wrf_real* . ;  ln -s $thedatanmm/wrf_nest* . ; ln -s $thedatanmm/geo_* .; \
   cp  $thedatanmm/namelist.nest.regtest . ;  ln -s $thedatanmm/met_nmm* . ; \
  sed '/dyn_opt/d' $thedatanmm/namelist.input.regtest >! ./namelist.input.regtest )

##LPC( cd WRFV3/test/nmm_real ; ln -s $thedatanmm/wrf_real* . ; \
##LPC  sed '/dyn_opt/d' $thedatanmm/namelist.input.regtest >! ./namelist.input.regtest )
#DAVE###################################################
( cd WRFV3/test/em_real ; ls -ls )
( cd WRFV3/test/nmm_real ; ls -ls )
banner 4
#set ans = "$<"
#DAVE###################################################

#	John-specific stuff for maple is the else; part of the "using service machines".

if ( ! $clrm ) then
	pushd WRFV3
else
	if ( ! -d $TMPDIR ) then
		echo something wrong 1
	endif
	if ( ! -d $TMPDIR/RUN ) then
		mkdir $TMPDIR/RUN
		/bin/rm -fr $TMPDIR/RUN/*
	endif
	if ( -d $TMPDIR/RUN ) then
		tar cf - ./WRFV3/test ./WRFV3/main | ( cd $TMPDIR/RUN ; tar xvf - )
		pushd WRFV3
	else
		echo something wrong 2
		exit
	endif
endif

#	Here we initialize our output message.

if ( -e ${DEF_DIR}/wrftest.output ) rm ${DEF_DIR}/wrftest.output
echo "Architecture $ARCH[1]      machine: `hostname`" >>! ${DEF_DIR}/wrftest.output
echo "WRFV3 source from: $acquire_from " >>! ${DEF_DIR}/wrftest.output
echo "Number of OpenMP processes to use: $OPENMP" >>! ${DEF_DIR}/wrftest.output
echo "Number of MPI    processes to use: $Num_Procs" >>! ${DEF_DIR}/wrftest.output
if ( $ARCH[1] == Darwin ) then
	set name = `finger $user | grep "Name:" | awk '{print $4 " " $5}'`
else
	set name = ( `grep ^${user}: /etc/passwd | cut -d: -f5` ) 
endif
echo "Test conducted by $name" >>! ${DEF_DIR}/wrftest.output
echo " " >>! ${DEF_DIR}/wrftest.output
echo "Test run from directory ${CUR_DIR}" >>! ${DEF_DIR}/wrftest.output
echo " " >>! ${DEF_DIR}/wrftest.output
if ( $?LOADL_JOB_NAME ) then
	echo "Loadlever job name = ${LOADL_JOB_NAME}" >>! ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
endif
echo "Real data case for EM is from $dataset " >>! ${DEF_DIR}/wrftest.output
echo " " >>! ${DEF_DIR}/wrftest.output
echo "The em real and ideal forecasts will be nested: $NESTED " >>! ${DEF_DIR}/wrftest.output
echo " " >>! ${DEF_DIR}/wrftest.output
if ( $REG_TYPE == BIT4BIT ) then
	echo "This is a bit-wise (traditional) regression test. " >>! ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
else if ( $REG_TYPE == OPTIMIZED ) then
	echo "This is a fully optimized regression test. " >>! ${DEF_DIR}/wrftest.output
	echo "No inter-comparisons are made. " >>! ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
endif
if ( $REAL8 == TRUE ) then
	echo "Floating point precision is 8-bytes" >>! ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
endif
if      ( ( $CHEM == TRUE ) && ( $KPP != TRUE ) ) then
	echo "WRF_CHEM tests run for em_real core only" >>! ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
else if ( ( $CHEM == TRUE ) && ( $KPP == TRUE ) ) then
	echo "WRF_CHEM KPP tests run for em_real core only" >>! ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
endif
if ( $ADAPTIVE == TRUE ) then
	echo "Adaptive time step for em_real core only" >>! ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
endif
if ( $GLOBAL == TRUE ) then
	echo "Global tests run for em_real core only" >>! ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
endif
if ( $ESMF_LIB == TRUE ) then
	echo "A separately installed version of the latest ESMF library" >>! ${DEF_DIR}/wrftest.output
	echo "(NOT the ESMF library included in the WRF tarfile) will" >>! ${DEF_DIR}/wrftest.output
	echo "be used for MPI tests" >>! ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
endif
if ( $QUILT == TRUE ) then
	echo "One WRF output quilt server will be used for some tests" >>! ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
endif
if ( $FDDA == TRUE ) then
	if      ( $PHYSOPTS_FDDA == GRID ) then
		echo "Running FDDA tests (grid nudging only)" >>! ${DEF_DIR}/wrftest.output
		echo " " >>! ${DEF_DIR}/wrftest.output
	else if ( $PHYSOPTS_FDDA == BOTH ) then
		echo "Running FDDA tests (grid=1, obs=2, grid+obs=3)" >>! ${DEF_DIR}/wrftest.output
		echo " " >>! ${DEF_DIR}/wrftest.output
	endif
endif
if ( $GENERATE_BASELINE != FALSE ) then
	echo "WRF output will be archived in baseline directory ${GENERATE_BASELINE} for some tests" >>! \
	     ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
endif
if ( $COMPARE_BASELINE != FALSE ) then
	echo "WRF output will be compared with files in baseline directory ${COMPARE_BASELINE} for some tests" >>! \
	     ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
endif
echo "The selected I/O option is $IO_FORM ($IO_FORM_NAME[$IO_FORM])" >>! ${DEF_DIR}/wrftest.output
if      ( $IO_FORM_WHICH[$IO_FORM] == IO ) then
	echo "This option is for both input and history files" >>! ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
else if ( $IO_FORM_WHICH[$IO_FORM] == I  ) then
	echo "This option is for input files only" >>! ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
else if ( $IO_FORM_WHICH[$IO_FORM] ==  O ) then
	echo "This option is for history files only" >>! ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
endif

cat ${CUR_DIR}/version_info >>! ${DEF_DIR}/wrftest.output

#	There are three WRF em executables to be considered that can run in threaded and
#	distributed memory.  The 2d hills and 2d squall lines cannot be parallelized with
#	MPI, and are therefore not considered in this shell.  The nmm is only run with 
#	distributed memory (1 vs 4 procs).

set first_time_in = TRUE
foreach core ( $CORES )
#DAVE###################################################
echo doing core $core
banner 5
#set ans = "$<"
#DAVE###################################################

	#	Some sleight of hand is required for the chemistry tests because we need to
	#	build it twice.  But normally, we only build with different real vs ideal, or em vs nmm.
	#	What to do, what to do?  Well, we ask for em_real TWICE.  The first time we build without chemistry
	#	activated, the second time with it activated.  The first time, we do a single
	#	test, the second time through, we do the other 5 tests.

	if      ( ( $CHEM == TRUE ) && ( ${#CORES} == 2 ) && ( $first_time_in == TRUE ) ) then
		setenv WRF_CHEM 0
		set PHYSOPTS =	( 1 )
		set first_time_in = FALSE
	else if ( ( $CHEM == TRUE ) && ( $KPP != TRUE ) && ( ${#CORES} == 2 ) && ( $first_time_in != TRUE ) ) then
		setenv WRF_CHEM 1
		set PHYSOPTS =	( 2 3 4 5 6 )
	else if ( ( $CHEM == TRUE ) && ( $KPP == TRUE ) && ( ${#CORES} == 2 ) && ( $first_time_in != TRUE ) ) then
		setenv WRF_CHEM 1
		set PHYSOPTS =	( 2 3 )
	endif

	#	Cores to test.

        set ZAP_SERIAL_FOR_THIS_CORE          = FALSE
        set ZAP_OPENMP_FOR_THIS_CORE          = FALSE
	if      ( `echo $core | cut -c 1-2` == em ) then
		setenv WRF_EM_CORE	1
		setenv WRF_NMM_CORE	0
		setenv WRF_COAMPS_CORE	0
		setenv WRF_EXP_CORE	0
                set ZAP_SERIAL_FOR_THIS_CORE          = FALSE
                set ZAP_OPENMP_FOR_THIS_CORE          = FALSE
        else if ( `echo $core | cut -c 1-3` == nmm ) then
		setenv WRF_EM_CORE	0
		setenv WRF_NMM_CORE	1
		setenv WRF_NMM_NEST	1
		setenv WRF_COAMPS_CORE	0
		setenv WRF_EXP_CORE	0
                set ZAP_SERIAL_FOR_THIS_CORE          = TRUE
                set ZAP_OPENMP_FOR_THIS_CORE          = TRUE
	endif

	#	Here we are looping over all of the various compilation configurations,
	#	such as serial only, OpenMP only, MPI only, etc.  Each architecture
	#	has its own list of these options.  We build each of the executables for
	#	this particular ${core}.
	
	foreach compopt ( $COMPOPTS )
#DAVE###################################################
echo doing compile option $compopt
banner 6
#set ans = "$<"
#DAVE###################################################

		#	We sometimes are interested in bypassing the OpenMP option.

		if ( $compopt == $COMPOPTS[2] ) then
			if ( $ZAP_OPENMP == TRUE || $ZAP_OPENMP_FOR_THIS_CORE == TRUE ) then
				goto GOT_THIS_EXEC
			endif
		endif

		#	NMM only runs parallel
		if ( $compopt == $COMPOPTS[1] ) then
			if ( $ZAP_SERIAL == TRUE || $ZAP_SERIAL_FOR_THIS_CORE == TRUE ) then
				goto GOT_THIS_EXEC
			endif
		endif

		if ( `uname` == AIX ) goto BUILD_REGARDLESS
	
		#	Did we already build this one?

		if  ( $core == em_real )  then
			if      ( ( $compopt == $COMPOPTS[1] ) && \
                                  ( -e main/wrf_${core}.exe.$compopt ) && \
                                  ( -e main/real_${core}.exe.1 ) && \
                                  ( -e ${DEF_DIR}/regression_test/WRFV3/external/$IO_FORM_NAME[$IO_FORM]/diffwrf ) ) then
				goto GOT_THIS_EXEC
			else if ( ( $compopt != $COMPOPTS[1] ) && \
                                  ( -e main/wrf_${core}.exe.$compopt ) && \
                                  ( -e ${DEF_DIR}/regression_test/WRFV3/external/$IO_FORM_NAME[$IO_FORM]/diffwrf ) ) then
				goto GOT_THIS_EXEC
			endif
		else
			if      ( ( $compopt == $COMPOPTS[1] ) && \
                                  ( -e main/wrf_${core}.exe.$compopt ) && \
                                  ( -e main/ideal_${core}.exe.1 ) && \
                                  ( -e ${DEF_DIR}/regression_test/WRFV3/external/$IO_FORM_NAME[$IO_FORM]/diffwrf ) ) then
				goto GOT_THIS_EXEC
			else if ( ( $compopt != $COMPOPTS[1] ) && \
                                  ( -e main/wrf_${core}.exe.$compopt ) && \
                                  ( -e ${DEF_DIR}/regression_test/WRFV3/external/$IO_FORM_NAME[$IO_FORM]/diffwrf ) ) then
				goto GOT_THIS_EXEC
			endif
		endif

		BUILD_REGARDLESS:
	
		#	The WRF configuration file works with a single integer
		#	input, which is the compiler option.  By convention, option $COMPOPTS[1] is
		#	serial, $COMPOPTS[2] is OMP, and $COMPOPTS[3] is MPI.

		#	Print info about use of separately installed ESMF library.  
		set esmf_lib_str = " - - - - - - - - - - - - - "
		if ( $ESMF_LIB == TRUE ) then
			# only test ESMF with MPI
			if ( $compopt == $COMPOPTS[3] ) then
				echo "A separately installed version of the latest ESMF library" >>! ${DEF_DIR}/wrftest.output
				echo "(NOT the ESMF library included in the WRF tarfile) is" >>! ${DEF_DIR}/wrftest.output
				echo "being used for this test of $core parallel $compopt..." >>! ${DEF_DIR}/wrftest.output
				set esmf_lib_str = "using separate ESMF library"
				echo "Setting ESMFLIB = ${ESMFLIBSAVE}" >>! ${DEF_DIR}/wrftest.output
				echo "Setting ESMFINC = ${ESMFINCSAVE}" >>! ${DEF_DIR}/wrftest.output
				setenv ESMFLIB $ESMFLIBSAVE
				setenv ESMFINC $ESMFINCSAVE
			else
				unsetenv ESMFLIB
				unsetenv ESMFINC
			endif
		endif
	
#DAVE###################################################
echo start build mechanism
banner 7
#set ans = "$<"
#DAVE###################################################
		./clean -a

		#	Edit build command for either bit-wise comparison or full optimization.

		if ( $REG_TYPE == BIT4BIT ) then
			set DEBUG_FLAG = -d
		else
			set DEBUG_FLAG =
		endif

		#	Edit build command.  If this is a nested run, then either static nest 
		#	(idealized) or prescribed move (em_real).  If not nested, then shut off
		#	the nesting build option.

		if ( $NESTED == TRUE ) then
			if ( $core ==  em_real ) then
				set compopts_nest = $COMPOPTS_NEST_PRESCRIBED
			else
				set compopts_nest = $COMPOPTS_NEST_STATIC
			endif
		else
			set compopts_nest = $COMPOPTS_NO_NEST
		endif
			
		./configure $DEBUG_FLAG << EOF
$compopt
$compopts_nest
EOF
cp configure.wrf configure.wrf.core=${core}_build=${compopt}
	
		#	The configure.wrf file needs to be adjusted as to whether we are requesting real*4 or real*8
		#	as the default floating precision.

		if ( $REAL8 == TRUE ) then
			sed -e '/^RWORDSIZE/s/\$(NATIVE_RWORDSIZE)/8/'  configure.wrf > ! foo ; /bin/mv foo configure.wrf
		endif
	
		#	Fix the OpenMP default for IBM regression testing - noopt required for bit-wise comparison.

		if ( ( $compopt == $COMPOPTS[2] ) && ( `uname` == AIX ) ) then
			sed -e '/^OMP/s/-qsmp=noauto/-qsmp=noauto:noopt/'  configure.wrf > ! foo ; /bin/mv foo configure.wrf
		endif

		#	Save the configure file.

		cp configure.wrf configure.wrf.core=${core}_build=${compopt}

#DAVE###################################################
echo configure built with optim mods removed, ready to compile
banner 8
#set ans = "$<"
#DAVE###################################################
	
		# The WRF_SRC_ROOT_DIR hack is only used by the OSF1 build.  
		# It works around the annoying fact that in OSF1 $(PWD) does 
		# not change during execution of regtest.csh, despite the "cd" 
		# and "pushd" commands.  
		setenv WRF_SRC_ROOT_DIR "${DEF_DIR}/regression_test/WRFV3"
		#	Build this executable
		
		./compile $core >&! compile_${core}_build=${compopt}.log
#DAVE###################################################
echo compile done
banner 9
#set ans = "$<"
#DAVE###################################################
	
		#	Did the compile work?  Check the expected executable names and locations.

		set ok = $status
                if ( ! -x main/wrf.exe ) set ok = 1

		if      ( ( $core ==  em_real ) && ( $compopt == $COMPOPTS[1] ) ) then
			if ( ! -e main/real.exe ) set ok = 1
		else if ( ( $core == nmm_real ) && ( $compopt == $COMPOPTS[3] ) ) then
			if ( ! -e main/real_nmm.exe ) set ok = 1
		else if ( $compopt == $COMPOPTS[1] ) then
			if ( ! -e main/ideal.exe ) set ok = 1
		endif

                if ( ! -x external/io_netcdf/diffwrf ) set ok = 1
#	if ( ! -x external/io_int/diffwrf ) set ok = 1

		if ( $ok != 0 ) then
			echo "SUMMARY compilation    for $core           parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrftest.output
			$MAIL -s "REGRESSION FAILURE $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
			exit ( 3 )
		else
			echo "SUMMARY compilation    for $core           parallel $compopt $esmf_lib_str PASS" >>! ${DEF_DIR}/wrftest.output
			echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
			mv main/wrf.exe main/wrf_${core}.exe.$compopt
			if      ( ( $core ==  em_real ) && ( $compopt == $COMPOPTS[1] ) ) then
				mv main/real.exe main/real_${core}.exe.1
			else if ( ( $core == nmm_real ) && ( $compopt == $COMPOPTS[3] ) ) then
				mv main/real_nmm.exe main/real_${core}.exe.$COMPOPTS[3]
			else if ( $compopt == $COMPOPTS[1] ) then
				mv main/ideal.exe main/ideal_${core}.exe.1
			endif
#DAVE###################################################
echo exec exists
ls -ls main/*.exe*
banner 10
#set ans = "$<"
#DAVE###################################################
		endif

		GOT_THIS_EXEC:

		if ( $clrm ) then
			cp main/*exe* $TMPDIR/RUN/WRFV3/main
		endif
	
	end

	if ( $clrm ) then
		pushd $TMPDIR/RUN/WRFV3
	endif
	
	#	We have all of the executables built, now we run'em.  This is a loop
	#	over all of the various physics options for this particular
	#	${core}.  Inside the physics loop, we loop over the parallel options.
	#	This allows us to use the same WRF input files for each of the parallel
	#	choices for a single physics loop index.

	foreach phys_option ( $PHYSOPTS )
#DAVE###################################################
echo which phys option $phys_option
banner 11
#set ans = "$<"
#DAVE###################################################
	
		#	For each of the executables, we need to run several physics
		#	options.  

		if (  $core == em_real )  then

			if ( $CHEM != TRUE ) then
				set filetag=$filetag_real
			else if ( $CHEM == TRUE ) then
				if ( $phys_option <= 3 ) then
					set filetag=$filetag_real[1]
				else
					set filetag=$filetag_real[2]
				endif
			endif

			foreach compopt ( $COMPOPTS )
#DAVE###################################################
echo real if filetag is $filetag
echo compopt = $compopt
banner 12
#set ans = "$<"
#DAVE###################################################

				#	We sometimes are interested in bypassing the OpenMP option.

				if ( $compopt == $COMPOPTS[2] ) then
					if ( $ZAP_OPENMP == TRUE || $ZAP_OPENMP_FOR_THIS_CORE == TRUE ) then
						goto BYPASS_COMP_LOOP_REAL
					endif
				endif

				if ( $compopt == $COMPOPTS[1] ) then
					if ( $ZAP_SERIAL == TRUE || $ZAP_SERIAL_FOR_THIS_CORE == TRUE ) then
						goto BYPASS_COMP_LOOP_REAL
					endif
				endif

				pushd test/$core
				
				#
				#	Create the correct namelist.input file for real data cases.
				#

				if ( $CHEM != TRUE ) then
					cp ${CUR_DIR}/phys_real_${phys_option} phys_opt
					if ( $NESTED != TRUE ) then
						cp ${CUR_DIR}/dyn_real_${phys_option} dyn_opt
					else
						cp ${CUR_DIR}/dyn_real_SAFE dyn_opt
					endif
					cp ${CUR_DIR}/time_real_${phys_option} time_opt
					cp ${CUR_DIR}/dom_real dom_real
					cp ${CUR_DIR}/nest_real_${phys_option} nest_input_opt
					cp ${CUR_DIR}/damp_real_${phys_option} damp_real
					if ( -e fdda_opt ) rm fdda_opt
					cat " grid_fdda=0" > fdda_opt
					if ( -e fdda_time ) rm fdda_time

					if ( $FDDA == TRUE ) then
						cp ${CUR_DIR}/fdda_real_${phys_option} fdda_opt
						cp ${CUR_DIR}/fdda_real_time_${phys_option} fdda_time
					endif
	
					set time_step = `awk ' /^ time_step /{ print $3 } ' namelist.input.$dataset | cut -d, -f1`
	
					#	Wanna do more/less time steps on the real cases?  Easy. Those last two numbers
					#	in the eqns are all you need.  Their product must be 60.  So, instead of 3 and 20,
					#	(3 coarse grid timesteps), you could use 20 and 3 (20 coarse grid time steps).
	
					if      ( $NESTED == TRUE ) then
						@ run_seconds = $time_step * 3
						@ history_interval = $time_step / 20
					else if ( $NESTED != TRUE ) then
						@ run_seconds = $time_step * 10
						@ history_interval = $time_step / 6
					endif
					rm ed_in namelist.input.temp
					cat >! ed_in << EOF
g/run_seconds/s/[0-9]/$run_seconds
g/history_interval/s/[0-9][0-9][0-9]/$history_interval
w namelist.input.temp
q
EOF
					ed namelist.input.$dataset < ed_in
	
					cp ${CUR_DIR}/io_format io_format
					sed -e '/^ mp_physics/,/ensdim/d' -e '/^ &physics/r ./phys_opt' \
					    -e '/^ moist_adv_opt/,/scalar_adv_opt/d' -e '/^ non_hydrostatic/r ./dyn_opt' \
					    -e '/^ auxinput1_inname/d' -e '/^ debug_level/r ./time_opt' \
					    -e '/^ input_from_file/d' -e '/^ interval_seconds/r ./nest_input_opt' \
					    -e '/^ time_step /,/^ smooth_option/d' -e '/^ &domains/r ./dom_real' \
					    -e '/^ damp_opt /,/^ dampcoef/d' -e '/^ base_temp/r ./damp_real' \
					    -e '/^ io_form_history /,/^ io_form_boundary/d' -e '/^ restart_interval/r ./io_format' \
					    -e 's/ frames_per_outfile *= [0-9][0-9]*/ frames_per_outfile = 200/g' \
					    -e 's/ run_days *= [0-9][0-9]*/ run_days = 0/g' \
					    -e 's/ run_hours *= [0-9][0-9]*/ run_hours = 0/g' \
					    -e 's/ run_minutes *= [0-9][0-9]*/ run_minutes = 0/g' \
					    -e '/^ &fdda/r fdda_opt' \
					    -e '/^ debug_level/r fdda_time' \
					    -e '/dyn_opt/d' \
					namelist.input.temp >! namelist.input
				
				#	The chem run has its own namelist, due to special input files (io_form not tested for chem)

				else if ( $CHEM == TRUE ) then
					if ( ( $KPP == TRUE ) && ( $phys_option >= 3 ) ) then
						sed -e '/dyn_opt/d' \
						    -e 's/^ chem_opt *= [0-9]/ chem_opt = '${CHEM_OPT}'/' \
						    namelist.input.chem_test_${phys_option} >! namelist.input
					else
						sed -e '/dyn_opt/d' \
						    namelist.input.chem_test_${phys_option} >! namelist.input
					endif

					if ( -e met_em.d01.${filetag} ) then
						\rm met_em.d01.*
					endif
					if ( ${phys_option} <= 3 ) then
						ln -s 00z/met_em* .
					else
						ln -s 12z/met_em* .
					endif

				endif

				# WRF output quilt servers are only tested for MPI configuration.  
				# Currently, only one WRF output quilt server is used.  

				if ( $QUILT == TRUE ) then
					if ( $compopt == $COMPOPTS[3] ) then
						#	For now, test only one group of one output quilt servers.  
						sed -e 's/ nio_tasks_per_group *= *[0-9][0-9]*/ nio_tasks_per_group = 1/g' \
						    -e 's/ nio_groups *= *[0-9][0-9]*/ nio_groups = 1/g' \
						namelist.input >! namelist.input.temp
						mv -f namelist.input.temp namelist.input
						echo "Building namelist.input.$core.${phys_option}.$compopt with one I/O quilt server enabled."
						echo "NOTE  one I/O quilt server enabled for $core physics $phys_option parallel $compopt..." >>! ${DEF_DIR}/wrftest.output
					endif
				endif

				/bin/cp namelist.input $TMPDIR/namelist.input.$core.${phys_option}.$compopt
#DAVE###################################################
echo built namelist $TMPDIR/namelist.input.$core.${phys_option}.$compopt
cat  $TMPDIR/namelist.input.$core.${phys_option}.$compopt
echo need history interval to be 30
banner 13
#set ans = "$<"
#DAVE###################################################
#DAVE###################################################
echo skipped link of data files, we push them elsewhere
ls -ls met_em*
banner 14
#set ans = "$<"
#DAVE###################################################

				#	If this is the serial code, generate the IC and BC.  The real.exe program is not
				#	parallelized, so the data is generated and saved for the rest of the parallel tests.
				#	This data is necessarily updated for each of the physics tests.

				if ( $compopt == $COMPOPTS[1] ) then

					#	Zap any old input data laying around.

					rm wrfinput_d01 >& /dev/null
					rm wrfbdy_d01   >& /dev/null

					if ( $NESTED == TRUE ) then
						setenv OMP_NUM_THREADS 1
						if ( `uname` == AIX ) then
							setenv XLSMPOPTS "parthds=1"
						endif
						$SERIALRUNCOMMAND ../../main/real_${core}.exe.1 >! print.out.real_${core}_Phys=${phys_option}_Parallel=${compopt}
					else if ( $NESTED != TRUE ) then
						../../main/real_${core}.exe.1 >! print.out.real_${core}_Phys=${phys_option}_Parallel=${compopt}
					endif
#DAVE###################################################
echo finished real
banner 15
#set ans = "$<"
#DAVE###################################################

					grep "SUCCESS COMPLETE" print.out.real_${core}_Phys=${phys_option}_Parallel=${compopt} >& /dev/null
					set success = $status

					#	Did making the IC BC files work?

					if ( $GLOBAL == FALSE ) then
						if ( ( -e wrfinput_d01 ) && ( -e wrfbdy_d01 ) && ( $success == 0 ) ) then
							echo "SUMMARY generate IC/BC for $core physics $phys_option parallel $compopt $esmf_lib_str PASS" >>! ${DEF_DIR}/wrftest.output
							echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
						else
							echo "SUMMARY generate IC/BC for $core physics $phys_option parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrftest.output
							$MAIL -s "WRF FAIL making IC/BC $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
							if ( $KEEP_ON_RUNNING == FALSE ) exit ( 4 )
							echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
						endif
					else if ( $GLOBAL == TRUE ) then
						if ( ( -e wrfinput_d01 ) && ( $success == 0 ) ) then
							echo "SUMMARY generate IC for $core physics $phys_option parallel $compopt $esmf_lib_str PASS" >>! ${DEF_DIR}/wrftest.output
							echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
						else
							echo "SUMMARY generate IC for $core physics $phys_option parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrftest.output
							echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
							$MAIL -s "WRF FAIL making IC $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
							if ( $KEEP_ON_RUNNING == FALSE ) exit ( 41 )
						endif
					endif
#DAVE###################################################
echo IC BC must be OK
ls -ls wrfi* wrfb*
if ( $IO_FORM_NAME[$IO_FORM] == io_netcdf ) then
	ncdump -v Times wrfb* | tail -20
endif
banner 16
#set ans = "$<"
#DAVE###################################################
				endif
		
				#	Run the forecast for this core, physics package and parallel option

				rm $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$compopt >& /dev/null
				
				#	The chem run has its own set of namelists, due to special input files.

				if ( $CHEM == TRUE ) then

					# WRF output quilt servers are only tested for MPI configuration.  
					# Currently, only one WRF output quilt server is used.  
	
					if ( $QUILT == TRUE ) then
						if ( $compopt == $COMPOPTS[3] ) then
							#	For now, test only one group of one output quilt servers.  
							sed -e 's/ nio_tasks_per_group *= *[0-9][0-9]*/ nio_tasks_per_group = 1/g' \
							    -e 's/ nio_groups *= *[0-9][0-9]*/ nio_groups = 1/g' \
							namelist.input >! namelist.input.temp
							mv -f namelist.input.temp namelist.input
							echo "Building namelist.input.$core.${phys_option}.$compopt with one I/O quilt server enabled."
							echo "NOTE  one I/O quilt server enabled for $core physics $phys_option parallel $compopt..." >>! ${DEF_DIR}/wrftest.output
						endif
					endif
				endif

				if      ( $compopt == $COMPOPTS[1] ) then
					setenv OMP_NUM_THREADS 1
					if ( `uname` == AIX ) then
						setenv XLSMPOPTS "parthds=1"
					endif
					if ( $NESTED == TRUE ) then
						$SERIALRUNCOMMAND ../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}
					else if ( $NESTED != TRUE ) then
						../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}
					endif
				else if ( $compopt == $COMPOPTS[2] ) then
					setenv OMP_NUM_THREADS $OPENMP
					if ( `uname` == AIX ) then
						setenv XLSMPOPTS "parthds=${OPENMP}"
					endif
					if ( $NESTED == TRUE ) then
						$OMPRUNCOMMAND ../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}
					else if ( $NESTED != TRUE ) then
						../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}
					endif
				else if ( $compopt == $COMPOPTS[3] ) then
					setenv OMP_NUM_THREADS 1
					if ( `uname` == AIX ) then
						setenv XLSMPOPTS "parthds=1"
					endif
					$MPIRUNCOMMAND ../../main/wrf_${core}.exe.$compopt $MPIRUNCOMMANDPOST
					mv rsl.error.0000 print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}
				endif
#DAVE###################################################
echo ran wrf fcst compopt = $compopt
banner 17
#set ans = "$<"
#DAVE###################################################

				grep "SUCCESS COMPLETE" print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}
				set success = $status

				#	Did making the forecast work, by that, we mean "is there an output file created", and "are there 2 times periods".

				if ( ( -e wrfout_d01_${filetag} ) && ( $success == 0 ) ) then
					if      ( $IO_FORM_NAME[$IO_FORM] == io_netcdf ) then
						ncdump -h wrfout_d01_${filetag} | grep Time | grep UNLIMITED | grep currently | grep -q 2
						set ok = $status
						set found_nans = 1
						if      ( `uname` == AIX   ) then
							ncdump wrfout_d01_${filetag} | grep NaN >& /dev/null
							set found_nans = $status
						else if ( `uname` == OSF1  ) then
							ncdump wrfout_d01_${filetag} | grep nan >& /dev/null
					#		set found_nans = $status
						else if ( `uname` == Linux ) then
							ncdump wrfout_d01_${filetag} | grep nan >& /dev/null
							set found_nans = $status
						endif
						if ( $found_nans == 0 ) then
							echo found nans
							set ok = 1
						endif

					else if ( $IO_FORM_NAME[$IO_FORM] == io_grib1  ) then
#						set joe_times = `../../external/io_grib1/wgrib -s -4yr wrfout_d01_${filetag} |  grep -v ":anl:" | wc -l`
#						if ( $joe_times >= 100 ) then
						set joe_times = `../../external/io_grib1/wgrib -s -4yr wrfout_d01_${filetag} |  grep "UGRD:10 m" | wc -l`
						if ( $joe_times == 2 ) then
							set ok = 0
						else
							set ok = 1
						endif
					endif
					if ( $ok == 0 ) then
						echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt $esmf_lib_str PASS" >>! ${DEF_DIR}/wrftest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
					else
						echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrftest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
						$MAIL -s "WRF FAIL FCST $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
						if ( $KEEP_ON_RUNNING == FALSE ) exit ( 5 )
					endif
				else
					echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrftest.output
					echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
					$MAIL -s "WRF FAIL FCST $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
					if ( $KEEP_ON_RUNNING == FALSE ) exit ( 6 )
				endif
#DAVE###################################################
echo success or failure of fcst
if ( $IO_FORM_NAME[$IO_FORM] == io_netcdf ) then
	ncdump -v Times wrfout_d01_${filetag} | tail -20
endif
banner 18
#set ans = "$<"
#DAVE###################################################

				#	We have to save this output file for our biggy comparison after all of the
				#	parallel options have been considered.

				mv wrfout_d01_${filetag} $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$compopt

				#       To save space, we move the executables after we are finished with them.

				if ( $phys_option == $PHYSOPTS[${#PHYSOPTS}] ) then
					mv ../../main/wrf_${core}.exe.$compopt $TMPDIR/wrf_${core}.exe.$compopt
				endif

				popd

				BYPASS_COMP_LOOP_REAL:

			end

		else if ( $core == nmm_real )  then
#DAVE###################################################
echo doing nmm pre no-nest
banner 19
#set ans = "$<"
#DAVE###################################################

                        set compopt = $COMPOPTS[3]   # ! parallel only
                        set filetag = 2005-01-23_00:00:00
                        set phys_option=1
                        pushd test/$core

#DAVE###################################################
echo did rms
echo $filetag $phys_option
banner 19a
#set ans = "$<"
#DAVE###################################################

			#	Build NMM namelist

			cp ${CUR_DIR}/io_format io_format
			sed -e '/^ io_form_history /,/^ io_form_boundary/d' -e '/^ restart_interval/r ./io_format' \
			    namelist.input.regtest >! namelist.input.temp
	
			#	A fairly short forecast, 10 time steps

			sed -e 's/^ run_days *= *[0-9]*/ run_days = 0 /' \
			    -e 's/^ run_seconds *= *[0-9]*/ run_seconds = 900 /' \
			    -e 's/^ history_interval *= *[0-9][0-9]*/ history_interval = 15,15 /' \
			    -e 's/^ frames_per_outfile *= [0-9]*/ frames_per_outfile = 200/g' \
			    namelist.input.temp >! namelist.input

#DAVE###################################################
echo did cp of namelist no-nest
ls -ls namelist.input
cat namelist.input
banner 19b
#set ans = "$<"
#DAVE###################################################

			#	Generate IC/BC for NMM run

			set RUNCOMMAND = `echo $MPIRUNCOMMAND | sed "s/$Num_Procs/1/"`

			#	Zap any old input data laying around.

			rm wrfinput_d01 >& /dev/null
			rm wrfbdy_d01   >& /dev/null

			$RUNCOMMAND ../../main/real_${core}.exe.$COMPOPTS[3] >! print.out.real_${core}_Phys=${phys_option}_Parallel=$COMPOPTS[3]
#DAVE###################################################
echo finished real no-nest
banner 19c
#set ans = "$<"
#DAVE###################################################

			mv rsl.out.0000 print.out.real_${core}_Phys=${phys_option}_Parallel=${compopt}
			grep "SUCCESS COMPLETE" print.out.real_${core}_Phys=${phys_option}_Parallel=${compopt} >& /dev/null
			set success = $status

			#	Did making the IC BC files work?

			if ( ( -e wrfinput_d01 ) && ( -e wrfbdy_d01 ) && ( $success == 0 ) ) then
				echo "SUMMARY generate IC/BC for $core physics $phys_option parallel $compopt $esmf_lib_str PASS" >>! ${DEF_DIR}/wrftest.output
				echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
			else
				echo "SUMMARY generate IC/BC for $core physics $phys_option parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrftest.output
				echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
				$MAIL -s "WRF FAIL making IC/BC $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
				if ( $KEEP_ON_RUNNING == FALSE ) exit ( 4 )
			endif
#DAVE###################################################
echo IC BC must be OK no-nest
ls -lsL wrfinput* wrfb*
if ( $IO_FORM_NAME[$IO_FORM] == io_netcdf ) then
	ncdump -v Times wrfb* | tail -20
endif
banner 20
#set ans = "$<"
#DAVE###################################################

			#	Run on 1 and then on Num_Procs processors

			foreach n ( 1 $Num_Procs )
#DAVE###################################################
echo running nmm on $n procs no-nest
banner 21
#set ans = "$<"
#DAVE###################################################

				if      ( ( $n != 1 ) && ( $QUILT != TRUE ) ) then
					@ nmm_proc = $Num_Procs
					cat >! nproc_xy << EOF
 nproc_x = $nmm_proc
 nproc_y = 1
EOF
					sed -e '/^ numtiles/r nproc_xy' namelist.input >! file.foo
					mv file.foo namelist.input
				else if ( ( $n != 1 ) && ( $QUILT == TRUE ) ) then
					@ nmm_proc = $Num_Procs - 1
					cat >! nproc_xy << EOF
 nproc_x = $nmm_proc
 nproc_y = 1
EOF
					sed -e '/^ numtiles/r nproc_xy' namelist.input >! file.foo
					mv file.foo namelist.input
				endif

				if ( `uname` == AIX ) then
					set RUNCOMMAND = $MPIRUNCOMMAND
				else
					set RUNCOMMAND = `echo $MPIRUNCOMMAND | sed "s/$Num_Procs/$n/"`
				endif

				#	WRF output quilt servers are only tested for MPI configuration.  
				#	Currently, only one WRF output quilt server is used.  
			
				if ( ( $QUILT == TRUE ) && ( $n == $Num_Procs ) ) then
					if ( $compopt == $COMPOPTS[3] ) then
						#	For now, test only one group of one output quilt servers.  
						sed -e 's/ nio_tasks_per_group *= *[0-9]*/ nio_tasks_per_group = 1/g' \
						    -e 's/ nio_groups *= *[0-9]*/ nio_groups = 1/g' \
						namelist.input >! namelist.input.temp
						mv -f namelist.input.temp namelist.input
						echo "Building namelist.input.$core.${phys_option}.$compopt with one I/O quilt server enabled."
						echo "NOTE  one I/O quilt server enabled for $core physics $phys_option parallel $compopt..." >>! ${DEF_DIR}/wrftest.output
					endif
				endif

				#	NMM can fail on spurious fp exceptions that don't affect soln. Retry if necessary.

				set tries=0
				while ( $tries < 2 )
#DAVE###################################################
echo try attempt $tries allowed to be less than 2
banner 22
#set ans = "$<"
#DAVE###################################################
					@ tries = $tries + 1
					$RUNCOMMAND ../../main/wrf_${core}.exe.$compopt $MPIRUNCOMMANDPOST
					mv rsl.error.0000 print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}_${n}p
					grep "SUCCESS COMPLETE" print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}_${n}p
					set success = $status
					set ok = $status
					if ( ( -e wrfout_d01_${filetag} ) && ( $success == 0 ) ) then
						if      ( $IO_FORM_NAME[$IO_FORM] == io_netcdf ) then
							ncdump -h wrfout_d01_${filetag} | grep Time | grep UNLIMITED | grep currently | grep -q 2
							set ok = $status
							set found_nans = 1
							if      ( `uname` == AIX   ) then
								ncdump wrfout_d01_${filetag} | grep NaN >& /dev/null
								set found_nans = $status
							else if ( `uname` == OSF1  ) then
								ncdump wrfout_d01_${filetag} | grep nan >& /dev/null
						#		set found_nans = $status
							else if ( `uname` == Linux ) then
								ncdump wrfout_d01_${filetag} | grep nan >& /dev/null
								set found_nans = $status
							endif
							if ( $found_nans == 0 ) then
								echo found nans
								set ok = 1
							endif

						else if ( $IO_FORM_NAME[$IO_FORM] == io_grib1  ) then
							../../external/io_grib1/wgrib -s -4yr wrfout_d01_${filetag} |  grep "UGRD:10 m above gnd:3600 sec fcst"
							set ok = $status
						endif
						if ( $ok == 0 ) then
							echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt $esmf_lib_str PASS" >>! ${DEF_DIR}/wrftest.output
							echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
							set tries=2  # success, bail from loop
						else
							echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrftest.output
							echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
							$MAIL -s "WRF FAIL FCST $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
							if ( if ( $KEEP_ON_RUNNING == FALSE ) && ( $tries == 2 ) )exit ( 5 )
						endif
					else
						echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrftest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
						$MAIL -s "WRF FAIL FCST $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
						if ( ( $KEEP_ON_RUNNING == FALSE ) && ( $tries == 2 ) ) exit ( 6 )
					endif
					mv wrfout_d01_${filetag} $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.${compopt}_${n}p
#DAVE###################################################
echo did nmm fcst no-nest
ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.${compopt}_${n}p
banner 23
#set ans = "$<"
#DAVE###################################################
				end
			end

                        popd

#DAVE###################################################
echo doing nmm pre nesting
banner 19
#set ans = "$<"
#DAVE###################################################

                        set compopt = $COMPOPTS[3]   # ! parallel only
                        set filetag = 2008-03-06_00:00:00
                        set phys_option=1
                        pushd test/$core

#DAVE###################################################
echo did rms
echo $filetag $phys_option
banner 19a
#set ans = "$<"
#DAVE###################################################

			#	Build NMM namelist

			cp ${CUR_DIR}/io_format io_format
			sed -e '/^ io_form_history /,/^ io_form_boundary/d' -e '/^ restart_interval/r ./io_format' \
			    namelist.nest.regtest >! namelist.input.temp
	
			#	A fairly short forecast, 10 time steps

			sed -e 's/^ run_hours *= *[0-9]*/ run_hours = 0 /' \
			    -e 's/^ run_seconds *= *[0-9]*/ run_seconds = 900 /' \
			    -e 's/^ history_interval *= *[0-9][0-9]*/ history_interval = 15,15 /' \
			    -e 's/^ frames_per_outfile *= [0-9]*/ frames_per_outfile = 200/g' \
			    namelist.input.temp >! namelist.input

#DAVE###################################################
echo did cp of namelist nesting
ls -ls namelist.input
cat namelist.input
banner 19b
#set ans = "$<"
#DAVE###################################################

			#	Generate IC/BC for NMM run

			set RUNCOMMAND = `echo $MPIRUNCOMMAND | sed "s/$Num_Procs/1/"`

			#	Zap any old input data laying around.

			rm wrfinput_d01 >& /dev/null
			rm wrfbdy_d01   >& /dev/null

			$RUNCOMMAND ../../main/real_${core}.exe.$COMPOPTS[3] >! print.out.real_${core}_Phys=${phys_option}_Parallel_Nest=$COMPOPTS[3]
#DAVE###################################################
echo finished real nesting
banner 19c
#set ans = "$<"
#DAVE###################################################

			mv rsl.out.0000 print.out.real_${core}_Phys=${phys_option}_Parallel=${compopt}
			grep "SUCCESS COMPLETE" print.out.real_${core}_Phys=${phys_option}_Parallel=${compopt} >& /dev/null
			set success = $status

			#	Did making the IC BC files work?

			if ( ( -e wrfinput_d01 ) && ( -e wrfbdy_d01 ) && ( $success == 0 ) ) then
				echo "SUMMARY generate IC/BC for $core physics $phys_option parallel nest $compopt $esmf_lib_str PASS" >>! ${DEF_DIR}/wrftest.output
				echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
			else
				echo "SUMMARY generate IC/BC for $core physics $phys_option parallel nest $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrftest.output
				echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
				$MAIL -s "WRF FAIL making IC/BC $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
				if ( $KEEP_ON_RUNNING == FALSE ) exit ( 4 )
			endif
#DAVE###################################################
echo IC BC must be OK nesting
ls -lsL wrfinput* wrfb*
if ( $IO_FORM_NAME[$IO_FORM] == io_netcdf ) then
	ncdump -v Times wrfb* | tail -20
endif
banner 20
#set ans = "$<"
#DAVE###################################################

			#	Run on 1 and then on Num_Procs processors

#LPC
			foreach n ( 1 $Num_Procs )
#DAVE###################################################
echo running nmm on $n procs nesting
banner 21
#set ans = "$<"
#DAVE###################################################

				if      ( ( $n != 1 ) && ( $QUILT != TRUE ) ) then
					@ nmm_proc = $Num_Procs
#LPC
					cat >! nproc_xy << EOF
 nproc_x = $nmm_proc
 nproc_y = 1
EOF
					sed -e '/^ numtiles/r nproc_xy' namelist.input >! file.foo
					mv file.foo namelist.input
				else if ( ( $n != 1 ) && ( $QUILT == TRUE ) ) then
					@ nmm_proc = $Num_Procs - 1
#LPC
					cat >! nproc_xy << EOF
 nproc_x = $nmm_proc
 nproc_y = 1
EOF
					sed -e '/^ numtiles/r nproc_xy' namelist.input >! file.foo
					mv file.foo namelist.input
				endif

				if ( `uname` == AIX ) then
					set RUNCOMMAND = $MPIRUNCOMMAND
				else
					set RUNCOMMAND = `echo $MPIRUNCOMMAND | sed "s/$Num_Procs/$n/"`
				endif

				#	WRF output quilt servers are only tested for MPI configuration.  
				#	Currently, only one WRF output quilt server is used.  
			
				if ( ( $QUILT == TRUE ) && ( $n == $Num_Procs ) ) then
					if ( $compopt == $COMPOPTS[3] ) then
						#	For now, test only one group of one output quilt servers.  
						sed -e 's/ nio_tasks_per_group *= *[0-9]*/ nio_tasks_per_group = 1/g' \
						    -e 's/ nio_groups *= *[0-9]*/ nio_groups = 1/g' \
						namelist.input >! namelist.input.temp
						mv -f namelist.input.temp namelist.input
						echo "Building namelist.input.$core.${phys_option}.$compopt with one I/O quilt server enabled."
						echo "NOTE  one I/O quilt server enabled for $core physics $phys_option parallel $compopt..." >>! ${DEF_DIR}/wrftest.output
					endif
				endif

				#	NMM can fail on spurious fp exceptions that don't affect soln. Retry if necessary.

				set tries=0
				while ( $tries < 2 )
#DAVE###################################################
echo try attempt $tries allowed to be less than 2
banner 22
#set ans = "$<"
#DAVE###################################################
					@ tries = $tries + 1
					$RUNCOMMAND ../../main/wrf_${core}.exe.$compopt $MPIRUNCOMMANDPOST
ls -lsL rsl*
					mv rsl.error.0000 print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}_${n}p
					grep "SUCCESS COMPLETE" print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}_${n}p
					set success = $status
					set ok = $status
					if ( ( -e wrfout_d01_${filetag} ) && ( $success == 0 ) ) then
						if      ( $IO_FORM_NAME[$IO_FORM] == io_netcdf ) then
							ncdump -h wrfout_d01_${filetag} | grep Time | grep UNLIMITED | grep currently | grep -q 2
							set ok = $status
							set found_nans = 1
							if      ( `uname` == AIX   ) then
								ncdump wrfout_d01_${filetag} | grep NaN >& /dev/null
								set found_nans = $status
							else if ( `uname` == OSF1  ) then
								ncdump wrfout_d01_${filetag} | grep nan >& /dev/null
						#		set found_nans = $status
							else if ( `uname` == Linux ) then
								ncdump wrfout_d01_${filetag} | grep nan >& /dev/null
								set found_nans = $status
							endif
							if ( $found_nans == 0 ) then
								echo found nans
								set ok = 1
							endif

						else if ( $IO_FORM_NAME[$IO_FORM] == io_grib1  ) then
							../../external/io_grib1/wgrib -s -4yr wrfout_d01_${filetag} |  grep "UGRD:10 m above gnd:3600 sec fcst"
							set ok = $status
						endif
						if ( $ok == 0 ) then
							echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt $esmf_lib_str PASS" >>! ${DEF_DIR}/wrftest.output
							echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
							set tries=2  # success, bail from loop
						else
							echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrftest.output
							echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
							$MAIL -s "WRF FAIL FCST $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
							if ( if ( $KEEP_ON_RUNNING == FALSE ) && ( $tries == 2 ) )exit ( 5 )
						endif
					else
						echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrftest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
						$MAIL -s "WRF FAIL FCST $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
						if ( if ( $KEEP_ON_RUNNING == FALSE ) && ( $tries == 2 ) ) exit ( 6 )
					endif
					mv wrfout_d01_${filetag} $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.${compopt}_${n}p
				/bin/cp namelist.input $TMPDIR/namelist.input.$core.${phys_option}.$compopt
#DAVE###################################################
echo did nmm fcst with nesting
ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.${compopt}_${n}p
banner 23
#set ans = "$<"
#DAVE###################################################
				end
			end

                        popd

		else

#DAVE###################################################
echo doing ideal runs
banner 24
#set ans = "$<"
#DAVE###################################################
			#	The ideal cases have different physics tests than the real cases.  If this is 
			#	more that the total number of ideal physics experiments that we were led to 
			#	believe would exist, jump to the end of the physics loop.

			if ( $phys_option > $Max_Ideal_Physics_Options ) then
				goto BOTTOM_OF_PHYSICS_LOOP
			endif

                        set filetag=$filetag_ideal

			foreach compopt ( $COMPOPTS )
#DAVE###################################################
echo doing compopt = $compopt
echo filetag = $filetag
banner 25
#set ans = "$<"
#DAVE###################################################


				#	We sometimes are interested in bypassing the OpenMP option.

				if ( $compopt == $COMPOPTS[2] ) then
					if ( $ZAP_OPENMP == TRUE ) then
						goto BYPASS_COMP_LOOP_IDEAL
					endif
				endif

				pushd test/$core

				if ( ! -e namelist.input.template ) cp namelist.input namelist.input.template

				#	Create the correct namelist.input file.

				cp ${CUR_DIR}/dom_ideal dom_ideal
				if      ( $core == em_quarter_ss )  then
					cp ${CUR_DIR}/phys_quarter_ss_${phys_option}a phys_tke
					cp ${CUR_DIR}/phys_quarter_ss_${phys_option}b phys_mp
					cp ${CUR_DIR}/phys_quarter_ss_${phys_option}c phys_nh
					cp ${CUR_DIR}/phys_quarter_ss_${phys_option}d phys_nest
					cp ${CUR_DIR}/phys_quarter_ss_${phys_option}e phys_bc
					cp ${CUR_DIR}/phys_quarter_ss_${phys_option}f phys_sfclay
				else if ( $core == em_b_wave     ) then
					cp ${CUR_DIR}/phys_b_wave_${phys_option}a     phys_tke
					cp ${CUR_DIR}/phys_b_wave_${phys_option}b     phys_mp
					cp ${CUR_DIR}/phys_b_wave_${phys_option}c     phys_nh
					cp ${CUR_DIR}/phys_b_wave_${phys_option}d     phys_nest
				endif

				cp ${CUR_DIR}/io_format io_format
				if      ( $NESTED == TRUE ) then
					if      ( $core == em_quarter_ss )  then
						sed -e 's/ run_days *= *[0-9][0-9]*/ run_days = 00/g'                              		\
						    -e 's/ run_minutes *= *[0-9][0-9]*/ run_minutes = 1/g'                         		\
						    -e 's/ run_seconds *= *[0-9][0-9]*/ run_seconds = 0/g'                         		\
		                                    -e 's/ history_interval *= [0-9][0-9]*/ history_interval = 1/g'                     	\
						    -e 's/ frames_per_outfile *= [0-9][0-9]*/ frames_per_outfile = 100/g'               	\
						    -e '/^ diff_opt/d' -e '/^ km_opt/d' -e '/^ damp_opt/d' -e '/^ rk_ord/r ./phys_tke' 		\
 		                                    -e '/^ io_form_history /,/^ io_form_boundary/d' -e '/^ restart_interval/r ./io_format'	\
						    -e '/^ mp_physics/d' -e '/^ &physics/r ./phys_mp' 						\
						    -e '/^ sf_sfclay_physics/d' -e '/^ radt/r ./phys_sfclay' 					\
						    -e '/^ moist_adv_opt/,/^ non_hydrostatic/d' -e '/^ v_sca_adv_order/r ./phys_nh' 					\
						    -e '/^ periodic_x /,/^ open_ye/d'								\
						    -e '/^ &bdy_control/r ./phys_bc' 								\
						    -e '/^ max_dom/d' -e '/^ time_step_fract_den/r ./dom_ideal'					\
						    ./namelist.input.template >! namelist.input
					else if ( $core == em_b_wave     ) then
						sed -e 's/ run_days *= *[0-9][0-9]*/ run_days = 00/g'                              		\
						    -e 's/ run_minutes *= *[0-9][0-9]*/ run_minutes = 20/g'                       		\
						    -e 's/ history_interval *= [0-9][0-9]*/ history_interval = 20/g'                   		\
						    -e 's/ frames_per_outfile *= [0-9][0-9]*/ frames_per_outfile = 100/g'               	\
						    -e '/^ diff_opt/d' -e '/^ km_opt/d' -e '/^ damp_opt/d' -e '/^ rk_ord/r ./phys_tke' 		\
 		                                    -e '/^ io_form_history /,/^ io_form_boundary/d' -e '/^ restart_interval/r ./io_format'	\
						    -e '/^ mp_physics/d' -e '/^ &physics/r ./phys_mp' 						\
						    -e '/^ non_hydrostatic/d' -e '/^ v_sca_adv_order/r ./phys_nh' 				\
						    -e '/^ max_dom/d' -e '/^ time_step_fract_den/r ./dom_ideal'					\
						./namelist.input.template >! namelist.input
					endif
				else if ( $NESTED != TRUE ) then
					if      ( $core == em_quarter_ss )  then
						sed -e 's/ run_days *= *[0-9][0-9]*/ run_days = 00/g'                              		\
						    -e 's/ run_minutes *= *[0-9][0-9]*/ run_minutes = 2/g'                         		\
		                                    -e 's/ history_interval *= [0-9][0-9]*/ history_interval = 2/g'                     	\
						    -e 's/ frames_per_outfile *= [0-9][0-9]*/ frames_per_outfile = 100/g'               	\
						    -e '/^ diff_opt/d' -e '/^ km_opt/d' -e '/^ damp_opt/d' -e '/^ rk_ord/r ./phys_tke' 		\
 		                                    -e '/^ io_form_history /,/^ io_form_boundary/d' -e '/^ restart_interval/r ./io_format'	\
						    -e '/^ mp_physics/d' -e '/^ &physics/r ./phys_mp' 						\
						    -e '/^ sf_sfclay_physics/d' -e '/^ radt/r ./phys_sfclay' 					\
						    -e '/^ moist_adv_opt/,/^ non_hydrostatic/d' -e '/^ v_sca_adv_order/r ./phys_nh' 					\
						    -e '/^ periodic_x/d' -e '/^ open_xs/d' -e '/^ open_xe/d' 					\
						    -e '/^ periodic_y/d' -e '/^ open_ys/d' -e '/^ open_ye/d' 					\
						    -e '/^ &bdy_control/r ./phys_bc' 								\
						    -e '/^ max_dom/d' -e '/^ time_step_fract_den/r ./dom_ideal'					\
						    ./namelist.input.template >! namelist.input
					else if ( $core == em_b_wave     ) then
						sed -e 's/ run_days *= *[0-9][0-9]*/ run_days = 00/g'                              		\
						    -e 's/ run_minutes *= *[0-9][0-9]*/ run_minutes = 100/g'                       		\
						    -e 's/ history_interval *= [0-9][0-9]*/ history_interval = 100/g'                   	\
						    -e 's/ frames_per_outfile *= [0-9][0-9]*/ frames_per_outfile = 100/g'               	\
						    -e '/^ diff_opt/d' -e '/^ km_opt/d' -e '/^ damp_opt/d' -e '/^ rk_ord/r ./phys_tke' 		\
 		                                    -e '/^ io_form_history /,/^ io_form_boundary/d' -e '/^ restart_interval/r ./io_format'	\
						    -e '/^ mp_physics/d' -e '/^ &physics/r ./phys_mp' 						\
						    -e '/^ non_hydrostatic/d' -e '/^ v_sca_adv_order/r ./phys_nh' 					\
						    -e '/^ max_dom/d' -e '/^ time_step_fract_den/r ./dom_ideal'					\
						./namelist.input.template >! namelist.input
					endif
				endif

				# WRF output quilt servers are only tested for MPI configuration.  
				# Currently, only one WRF output quilt server is used.  

				if ( $QUILT == TRUE ) then
					if ( $compopt == $COMPOPTS[3] ) then
						#	For now, test only one group of one output quilt servers.  
						sed -e 's/ nio_tasks_per_group *= *[0-9][0-9]*/ nio_tasks_per_group = 1/g' \
						    -e 's/ nio_groups *= *[0-9][0-9]*/ nio_groups = 1/g' \
						namelist.input >! namelist.input.temp
						mv -f namelist.input.temp namelist.input
						echo "Building namelist.input.$core.${phys_option}.$compopt with one I/O quilt server enabled."
						echo "NOTE  one I/O quilt server enabled for $core physics $phys_option parallel $compopt..." >>! ${DEF_DIR}/wrftest.output
					endif
				endif

				/bin/cp namelist.input $TMPDIR/namelist.input.$core.${phys_option}.$compopt
#DAVE###################################################
echo built namelist 
ls -ls namelist.input
banner 26
#set ans = "$<"
#DAVE###################################################

				#	If this is the serial code, generate the IC and BC.  The ideal.exe program is not
				#	parallelized, so the data is generated and saved for the rest of the parallel tests.

				if ( $compopt == $COMPOPTS[1] ) then

					#	Zap any old input data laying around.

					rm wrfinput_d01 >& /dev/null
					rm wrfbdy_d01   >& /dev/null

					../../main/ideal_${core}.exe.1 >! print.out.ideal_${core}_Parallel=${compopt}
#DAVE###################################################
echo ran ideal
ls -ls wrfinput*
banner 27
#set ans = "$<"
#DAVE###################################################

					grep "SUCCESS COMPLETE" print.out.ideal_${core}_Parallel=${compopt} >& /dev/null
					set success = $status

					#	Did making the IC BC files work?

					if ( ( -e wrfinput_d01 ) && ( $success == 0 ) ) then
						echo "SUMMARY generate IC/BC for $core physics $phys_option parallel $compopt $esmf_lib_str PASS" >>! ${DEF_DIR}/wrftest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
					else
						echo "SUMMARY generate IC/BC for $core physics $phys_option parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrftest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
						$MAIL -s "WRF FAIL making IC/BC $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
						if ( $KEEP_ON_RUNNING == FALSE ) exit ( 7 )
					endif
				endif
		
				#	Run the forecast for this core and parallel option

				rm $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$compopt >& /dev/null

				if      ( $compopt == $COMPOPTS[1] ) then
					setenv OMP_NUM_THREADS 1
					if ( `uname` == AIX ) then
						setenv XLSMPOPTS "parthds=1"
					endif
					if ( $NESTED == TRUE ) then
						$SERIALRUNCOMMAND ../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Parallel=${compopt}
					else if ( $NESTED != TRUE ) then
						../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Parallel=${compopt}
					endif
				else if ( $compopt == $COMPOPTS[2] ) then
					setenv OMP_NUM_THREADS $OPENMP
					if ( `uname` == AIX ) then
						setenv XLSMPOPTS "parthds=${OPENMP}"
					endif
					if ( $NESTED == TRUE ) then
						$OMPRUNCOMMAND ../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Parallel=${compopt}
					else if ( $NESTED != TRUE ) then
						../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Parallel=${compopt}
					endif
				else if ( $compopt == $COMPOPTS[3] ) then
					setenv OMP_NUM_THREADS 1
					if ( `uname` == AIX ) then
						setenv XLSMPOPTS "parthds=1"
					endif
					$MPIRUNCOMMAND ../../main/wrf_${core}.exe.$compopt $MPIRUNCOMMANDPOST
					mv rsl.error.0000 print.out.wrf_${core}_Parallel=${compopt}
				endif
#DAVE###################################################
echo ran ideal fcst
banner 28
#set ans = "$<"
#DAVE###################################################

				grep "SUCCESS COMPLETE" print.out.wrf_${core}_Parallel=${compopt}
				set success = $status

				#	Did making the forecast work, by that, we mean "is there an output file created?"

				if ( ( -e wrfout_d01_${filetag} ) && ( $success == 0 ) ) then
					if      ( $IO_FORM_NAME[$IO_FORM] == io_netcdf ) then
						ncdump -h wrfout_d01_${filetag} | grep Time | grep UNLIMITED | grep currently | grep -q 2
						set ok = $status
						set found_nans = 1
						if      ( `uname` == AIX   ) then
							ncdump wrfout_d01_${filetag} | grep NaN >& /dev/null
							set found_nans = $status
						else if ( `uname` == OSF1  ) then
							ncdump wrfout_d01_${filetag} | grep nan >& /dev/null
					#		set found_nans = $status
						else if ( `uname` == Linux ) then
							ncdump wrfout_d01_${filetag} | grep nan >& /dev/null
							set found_nans = $status
						endif
						if ( $found_nans == 0 ) then
							echo found nans
							set ok = 1
						endif

					else if ( $IO_FORM_NAME[$IO_FORM] == io_grib1  ) then
#						set joe_times = `../../external/io_grib1/wgrib -s -4yr wrfout_d01_${filetag} |  grep -v ":anl:" | wc -l`
#						if ( $joe_times >= 100 ) then
						set joe_times = `../../external/io_grib1/wgrib -s -4yr wrfout_d01_${filetag} |  grep "UGRD:10 m" | wc -l`
						if ( $joe_times == 2 ) then
							set ok = 0
						else
							set ok = 1
						endif
					endif
					if ( $ok == 0 ) then
						echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt $esmf_lib_str PASS" >>! ${DEF_DIR}/wrftest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
					else
						echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrftest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
						$MAIL -s "WRF FAIL FCST $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
						if ( $KEEP_ON_RUNNING == FALSE ) exit ( 8 )
					endif
				else
					echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt $esmf_lib_str FAIL" >>! ${DEF_DIR}/wrftest.output
					echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
					$MAIL -s "WRF FAIL FCST $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
					if ( $KEEP_ON_RUNNING == FALSE ) exit ( 9 )
				endif

				#	We have to save this output file for our biggy comparison after all of the
				#	parallel options have been considered.

				mv wrfout_d01_${filetag} $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$compopt

				#       To save space, we move the executables after we are finished with them.

				if ( $phys_option == $Max_Ideal_Physics_Options ) then
					mv ../../main/wrf_${core}.exe.$compopt $TMPDIR/wrf_${core}.exe.$compopt
				endif
#DAVE###################################################
echo fcst was a success
ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$compopt
if ( $IO_FORM_NAME[$IO_FORM] == io_netcdf ) then
	ncdump -v Times $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$compopt | tail -20
endif
banner 29
#set ans = "$<"
#DAVE###################################################

				popd

				BYPASS_COMP_LOOP_IDEAL:

			end

		endif

		#	OK, once more, we gotta check if this is a BIT4BIT run.  If so then there
		#	are a number of comparisons to do.  If this is a an OPTIMIZED run, then the
		#	comparisons will fail the bit-wise comparisons.

		if ( $REG_TYPE == BIT4BIT) then

	                if ( $core == nmm_real ) then
	
				pushd ${DEF_DIR}/regression_test/WRFV3/test/$core
				set DIFFWRF = ${DEF_DIR}/regression_test/WRFV3/external/$IO_FORM_NAME[$IO_FORM]/diffwrf
	
	                        if ( ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_1p ) && \
	                             ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_${Num_Procs}p ) ) then
	                                set foo1 = ( ` \ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_1p `)
	                                set foo2 = ( ` \ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_${Num_Procs}p `)
	                                set size1 = $foo1[6]
	                                set size2 = $foo2[6]
	                                if ( $size1 == $size2 ) then
	                                        set RIGHT_SIZE = TRUE
	                                else
	                                        set RIGHT_SIZE = FALSE
	                                endif
	                        else
	                                set RIGHT_SIZE = FALSE
	                        endif
	
	                        #       1p vs Num_Procs MPI
	
	                        rm fort.88 fort.98 >& /dev/null
	                        if ( ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_1p ) && \
	                             ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_${Num_Procs}p ) && \
	                             ( $RIGHT_SIZE == TRUE ) ) then
	                                $DIFFWRF $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_1p \
	                                         $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_${Num_Procs}p >& /dev/null
	                        else
	                                touch fort.88 fort.98
	                        endif
	                        if ( ! -e fort.88 ) then
	                                echo "SUMMARY 1 vs $Num_Procs MPI  for $core physics $phys_option $esmf_lib_str            PASS" >>! ${DEF_DIR}/wrftest.output
	                                echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
	                        else
	                                echo "SUMMARY 1 vs $Num_Procs MPI  for $core physics $phys_option $esmf_lib_str            FAIL" >>! ${DEF_DIR}/wrftest.output
	                                echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
	                        endif
	
	                        popd
	
			else if ( ${#COMPOPTS} != 1 ) then
	
			#	If there is only a single parallel option, then we are just trying to
			#	build the serial code.  That implies no comparisons are needed.
	
				#	All of the forecasts for this set of physics and core have been
				#	generated.  We now compare the WRF model output files to see
				#	if they are S^2D^2.
		
				pushd ${DEF_DIR}/regression_test/WRFV3/test/$core
				set DIFFWRF = ${DEF_DIR}/regression_test/WRFV3/external/$IO_FORM_NAME[$IO_FORM]/diffwrf
	
				#	Are we skipping the OpenMP runs?
	
				if ( $ZAP_OPENMP == TRUE ) then
					goto BYPASS_OPENMP_SUMMARY1
				endif
	
				#	Are the files the same size?  If not, then only the initial times
				#	will be compared.  That means, on a failure to run a forecast, the
				#	diffwrf will give a pass.  We need to root out this evil.
	
				if ( ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] ) && \
				     ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[2] ) ) then
					set foo1 = ( ` \ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] `)
					set foo2 = ( ` \ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[2] `)
					set size1 = $foo1[6]
					set size2 = $foo2[6]
					if ( $size1 == $size2 ) then
						set RIGHT_SIZE_OMP = TRUE
					else
						set RIGHT_SIZE_OMP = FALSE
					endif
				else
					set RIGHT_SIZE_OMP = FALSE
				endif
	
				BYPASS_OPENMP_SUMMARY1:
	
				if ( ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] ) && \
				     ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3] ) ) then
					set foo1 = ( ` \ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] `)
					set foo3 = ( ` \ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3] `)
					set size1 = $foo1[6]
					set size3 = $foo3[6]
					if ( $size1 == $size3 ) then
						set RIGHT_SIZE_MPI = TRUE
					else
						set RIGHT_SIZE_MPI = FALSE
					endif
				else
					set RIGHT_SIZE_MPI = FALSE
				endif
	
				#	Are we skipping the OpenMP runs?
	
				if ( $ZAP_OPENMP == TRUE ) then
					goto BYPASS_OPENMP_SUMMARY2
				endif
		
				#	Serial vs OpenMP
		
				rm fort.88 fort.98 >& /dev/null
				if ( ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] ) && \
				     ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[2] ) && \
				     ( $RIGHT_SIZE_OMP == TRUE ) ) then
					$DIFFWRF $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] \
					         $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[2] >& /dev/null
				else
					touch fort.88 fort.98
				endif
				if ( ! -e fort.88 ) then
					echo "SUMMARY serial vs OMP  for $core physics $phys_option $esmf_lib_str            PASS" >>! ${DEF_DIR}/wrftest.output
					echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
				else
					echo "SUMMARY serial vs OMP  for $core physics $phys_option $esmf_lib_str            FAIL" >>! ${DEF_DIR}/wrftest.output
					echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
				endif
	
				BYPASS_OPENMP_SUMMARY2:
		
				#	Serial vs MPI
		
				rm fort.88 fort.98 >& /dev/null
				if ( ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] ) && \
				     ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3] ) && \
				     ( $RIGHT_SIZE_MPI == TRUE ) ) then
					$DIFFWRF $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] \
					         $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3] >& /dev/null
				else
					touch fort.88 fort.98
				endif
				if ( ! -e fort.88 ) then
					echo "SUMMARY serial vs MPI  for $core physics $phys_option $esmf_lib_str            PASS" >>! ${DEF_DIR}/wrftest.output
					echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
				else
					echo "SUMMARY serial vs MPI  for $core physics $phys_option $esmf_lib_str            FAIL" >>! ${DEF_DIR}/wrftest.output
					echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
				endif
		
				popd
	
			endif

			# Generate and archive baseline or compare against baseline

			if ( $core != nmm_real ) then
				if ( $GENERATE_BASELINE != FALSE ) then
					if ( ! -d $GENERATE_BASELINE ) then
						echo "ERROR:  Baseline directory ${GENERATE_BASELINE} does not exist" >>! \
						     ${DEF_DIR}/wrftest.output
						exit ( 10 ) 
					else
						# Archive serial output file to baseline
						pushd ${DEF_DIR}/regression_test/WRFV3/test/$core
						set basefilenm = wrfout_d01_${filetag}.${core}.${phys_option}
						set basefile = ${GENERATE_BASELINE}/${basefilenm}
						set outfile = $TMPDIR/${basefilenm}.$COMPOPTS[1]
						if ( -e $outfile ) then
							cp $outfile $basefile || \
							  ( echo "ERROR:  cannot copy ${outfile} to ${basefile}" >>! \
							   ${DEF_DIR}/wrftest.output; exit 10 )
						else
							echo "ERROR:  Cannot archive baseline, file $outfile does not exist" >>! \
							     ${DEF_DIR}/wrftest.output
					echo "SUMMARY baseline archived in ${GENERATE_BASELINE} for $core physics $phys_option  FAIL" >>! \
							     ${DEF_DIR}/wrftest.output
							echo "-------------------------------------------------------------" >> \
							     ${DEF_DIR}/wrftest.output
							exit ( 10 ) 
						endif
					echo "SUMMARY baseline archived in ${GENERATE_BASELINE} for $core physics $phys_option  PASS" >>! \
						     ${DEF_DIR}/wrftest.output
						echo "-------------------------------------------------------------" >> \
						     ${DEF_DIR}/wrftest.output
						popd
					endif
				endif
				if ( $COMPARE_BASELINE != FALSE ) then
					if ( ! -d $COMPARE_BASELINE ) then
						echo "${0}: ERROR::  Baseline directory ${COMPARE_BASELINE} does not exist" >>! \
						     ${DEF_DIR}/wrftest.output
						exit ( 10 ) 
					else
						# Compare against baseline output file
						set basefilenm = wrfout_d01_${filetag}.${core}.${phys_option}
						set basefile = ${COMPARE_BASELINE}/${basefilenm}
						set DIFFWRF = ${DEF_DIR}/regression_test/WRFV3/external/$IO_FORM_NAME[$IO_FORM]/diffwrf
						set testdir = ${DEF_DIR}/regression_test/WRFV3/test/$core
						pushd ${testdir}
						foreach compopt ( $COMPOPTS )
							set cmpfile = $TMPDIR/${basefilenm}.$compopt
							rm fort.88 fort.98 >& /dev/null
							if ( ( -e ${basefile} ) && ( -e ${cmpfile} ) ) then
								#	Are the files the same size?  If not, then only the initial times
								#	will be compared.  That means, on a failure to run a forecast, the
								#	diffwrf will give a pass.  We need to root out this evil.
								set foob = ( ` \ls -ls ${basefile} `)
								set fooc = ( ` \ls -ls ${cmpfile} `)
								set sizeb = $foob[6]
								set sizec = $fooc[6]
								if ( $sizeb == $sizec ) then
									$DIFFWRF ${basefile} ${cmpfile} >& /dev/null
									if ( -e fort.88 ) then
			echo "FAIL:  Baseline comparison, file ${cmpfile} did not match ${basefile} according to diffwrf" >>! \
										${DEF_DIR}/wrftest.output
									endif
								else
									touch fort.88 fort.98
			echo "FAIL:  Baseline comparison, files ${cmpfile} and ${basefile} have different sizes" >>! \
									${DEF_DIR}/wrftest.output
								endif
							else
			echo "FAIL:  Baseline comparison, either ${cmpfile} or ${basefile} does not exist" >>! \
									${DEF_DIR}/wrftest.output
								touch fort.88 fort.98
							endif
							if ( ! -e fort.88 ) then
			echo "SUMMARY compare vs baseline ${COMPARE_BASELINE} for $core physics $phys_option compopt $compopt $esmf_lib_str  PASS" >>! \
								     ${DEF_DIR}/wrftest.output
								echo "-------------------------------------------------------------" >> \
								     ${DEF_DIR}/wrftest.output
							else
			echo "SUMMARY compare vs baseline ${COMPARE_BASELINE} for $core physics $phys_option compopt $compopt $esmf_lib_str  FAIL" >>! \
								     ${DEF_DIR}/wrftest.output
								echo "-------------------------------------------------------------" >> \
								     ${DEF_DIR}/wrftest.output
							endif
						end
						popd
					endif
				endif
			else if ( $core == nmm_real ) then
				if ( $GENERATE_BASELINE != FALSE ) then
					if ( ! -d $GENERATE_BASELINE ) then
						echo "ERROR:  Baseline directory ${GENERATE_BASELINE} does not exist" >>! \
						     ${DEF_DIR}/wrftest.output
						exit ( 10 ) 
					else
						# Archive serial output file to baseline
						pushd ${DEF_DIR}/regression_test/WRFV3/test/$core
						set basefilenm = wrfout_d01_${filetag}.${core}.${phys_option}
						set basefile = ${GENERATE_BASELINE}/${basefilenm}
						set outfile = $TMPDIR/${basefilenm}.$COMPOPTS[3]_1p
						if ( -e $outfile ) then
							cp $outfile $basefile || \
							  ( echo "ERROR:  cannot copy ${outfile} to ${basefile}" >>! \
							   ${DEF_DIR}/wrftest.output; exit 10 )
						else
							echo "ERROR:  Cannot archive baseline, file $outfile does not exist" >>! \
							     ${DEF_DIR}/wrftest.output
					echo "SUMMARY baseline archived in ${GENERATE_BASELINE} for $core physics $phys_option  FAIL" >>! \
							     ${DEF_DIR}/wrftest.output
							echo "-------------------------------------------------------------" >> \
							     ${DEF_DIR}/wrftest.output
							exit ( 10 ) 
						endif
					echo "SUMMARY baseline archived in ${GENERATE_BASELINE} for $core physics $phys_option  PASS" >>! \
						     ${DEF_DIR}/wrftest.output
						echo "-------------------------------------------------------------" >> \
						     ${DEF_DIR}/wrftest.output
						popd
					endif
				endif
				if ( $COMPARE_BASELINE != FALSE ) then
					if ( ! -d $COMPARE_BASELINE ) then
						echo "${0}: ERROR::  Baseline directory ${COMPARE_BASELINE} does not exist" >>! \
						     ${DEF_DIR}/wrftest.output
						exit ( 10 ) 
					else
						# Compare against baseline output file
						set basefilenm = wrfout_d01_${filetag}.${core}.${phys_option}
						set basefile = ${COMPARE_BASELINE}/${basefilenm}
						set DIFFWRF = ${DEF_DIR}/regression_test/WRFV3/external/$IO_FORM_NAME[$IO_FORM]/diffwrf
						set testdir = ${DEF_DIR}/regression_test/WRFV3/test/$core
						pushd ${testdir}
						set compopt = $COMPOPTS[3]
						foreach proc ( 1p 4p )
							set cmpfile = $TMPDIR/${basefilenm}.${compopt}_${proc}
							rm fort.88 fort.98 >& /dev/null
							if ( ( -e ${basefile} ) && ( -e ${cmpfile} ) ) then
								#	Are the files the same size?  If not, then only the initial times
								#	will be compared.  That means, on a failure to run a forecast, the
								#	diffwrf will give a pass.  We need to root out this evil.
								set foob = ( ` \ls -ls ${basefile} `)
								set fooc = ( ` \ls -ls ${cmpfile} `)
								set sizeb = $foob[6]
								set sizec = $fooc[6]
								if ( $sizeb == $sizec ) then
									$DIFFWRF ${basefile} ${cmpfile} >& /dev/null
									if ( -e fort.88 ) then
			echo "FAIL:  Baseline comparison, file ${cmpfile} did not match ${basefile} according to diffwrf" >>! \
										${DEF_DIR}/wrftest.output
									endif
								else
									touch fort.88 fort.98
			echo "FAIL:  Baseline comparison, files ${cmpfile} and ${basefile} have different sizes" >>! \
									${DEF_DIR}/wrftest.output
								endif
							else
			echo "FAIL:  Baseline comparison, either ${cmpfile} or ${basefile} does not exist" >>! \
									${DEF_DIR}/wrftest.output
								touch fort.88 fort.98
							endif
							if ( ! -e fort.88 ) then
			echo "SUMMARY compare vs baseline ${COMPARE_BASELINE} for $core physics $phys_option compopt $compopt $esmf_lib_str  PASS" >>! \
								     ${DEF_DIR}/wrftest.output
								echo "-------------------------------------------------------------" >> \
								     ${DEF_DIR}/wrftest.output
							else
			echo "SUMMARY compare vs baseline ${COMPARE_BASELINE} for $core physics $phys_option compopt $compopt $esmf_lib_str  FAIL" >>! \
								     ${DEF_DIR}/wrftest.output
								echo "-------------------------------------------------------------" >> \
								     ${DEF_DIR}/wrftest.output
							endif
						end
						popd
					endif
				endif
				goto ALL_SHE_WROTE_FOR_NMM
			endif
			# End of generate and archive baseline or compare against baseline

		endif

		BOTTOM_OF_PHYSICS_LOOP:
		
	end

ALL_SHE_WROTE_FOR_NMM:

	echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output

        if ( $clrm ) then
          popd
        endif
		
end


#	How long did this take.

set end = ( `date` )
echo "Start WRF Regression: $start " >> ${DEF_DIR}/wrftest.output
echo "End   WRF Regression: $end   " >> ${DEF_DIR}/wrftest.output

#	We have done all of the tests, and placed the PASS FAIL labels in the
#	output file.  If there are any FAIL messages, we are in trouble.

grep FAIL ${DEF_DIR}/wrftest.output
set ok = $status

#	Send email of the status.

if ( $ok == 0 ) then
	$MAIL -s "REGRESSION FAILURE $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
else
	$MAIL -s "REGRESSION SUCCESS $ARCH[1] " $GOOD_MAIL < ${DEF_DIR}/wrftest.output
endif

#	Clean left up detritus

cd $CUR_DIR

rm -rf damp_*eal >& /dev/null
rm -rf dom_*eal >& /dev/null
rm -rf phys_real_* >& /dev/null
rm -rf nest_real_* >& /dev/null
rm -rf phys_quarter_* >& /dev/null
rm -rf phys_b_wave_* >& /dev/null
rm -rf version_info >& /dev/null
rm -rf machfile >& /dev/null
rm -rf fdda_real* >& /dev/null
rm -rf time_real_* >& /dev/null
rm -rf dyn_real_* >& /dev/null
rm -rf damp_* >& /dev/null
rm -rf io_format >& /dev/null
