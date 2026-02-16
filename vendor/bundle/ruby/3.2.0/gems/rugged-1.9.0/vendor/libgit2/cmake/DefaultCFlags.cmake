# Platform specific compilation flags
if(MSVC)
	add_definitions(-D_SCL_SECURE_NO_WARNINGS)
	add_definitions(-D_CRT_SECURE_NO_DEPRECATE)
	add_definitions(-D_CRT_NONSTDC_NO_DEPRECATE)

	string(REPLACE "/Zm1000" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")

	# /GF - String pooling
	# /MP - Parallel build
	set(CMAKE_C_FLAGS "/GF /MP /nologo ${CMAKE_C_FLAGS}")

	# /Gd - explicitly set cdecl calling convention
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /Gd")

	if(NOT (MSVC_VERSION LESS 1900))
		# /guard:cf - Enable Control Flow Guard
		set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /guard:cf")
	endif()

	if(STATIC_CRT)
		set(CRT_FLAG_DEBUG "/MTd")
		set(CRT_FLAG_RELEASE "/MT")
	else()
		set(CRT_FLAG_DEBUG "/MDd")
		set(CRT_FLAG_RELEASE "/MD")
	endif()

	if(WIN32_LEAKCHECK)
		set(GIT_WIN32_LEAKCHECK 1)
		set(CRT_FLAG_DEBUG "${CRT_FLAG_DEBUG}")
		set(CMAKE_C_STANDARD_LIBRARIES "${CMAKE_C_STANDARD_LIBRARIES} Dbghelp.lib")
	endif()

	# /Zi - Create debugging information
	# /Od - Disable optimization
	# /D_DEBUG - #define _DEBUG
	# /MTd - Statically link the multithreaded debug version of the CRT
	# /MDd - Dynamically link the multithreaded debug version of the CRT
	# /RTC1 - Run time checks
	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} /Zi /Od /D_DEBUG /RTC1 ${CRT_FLAG_DEBUG}")

	# /DNDEBUG - Disables asserts
	# /MT - Statically link the multithreaded release version of the CRT
	# /MD - Dynamically link the multithreaded release version of the CRT
	# /O2 - Optimize for speed
	# /Oy - Enable frame pointer omission (FPO) (otherwise CMake will automatically turn it off)
	# /GL - Link time code generation (whole program optimization)
	# /Gy - Function-level linking
	set(CMAKE_C_FLAGS_RELEASE "/DNDEBUG /O2 /Oy /GL /Gy ${CRT_FLAG_RELEASE}")

	# /Oy- - Disable frame pointer omission (FPO)
	set(CMAKE_C_FLAGS_RELWITHDEBINFO "/DNDEBUG /Zi /O2 /Oy- /GL /Gy ${CRT_FLAG_RELEASE}")

	# /O1 - Optimize for size
	set(CMAKE_C_FLAGS_MINSIZEREL "/DNDEBUG /O1 /Oy /GL /Gy ${CRT_FLAG_RELEASE}")

	# /IGNORE:4221 - Ignore empty compilation units
	set(CMAKE_STATIC_LINKER_FLAGS "/IGNORE:4221")

	# /DYNAMICBASE - Address space load randomization (ASLR)
	# /NXCOMPAT - Data execution prevention (DEP)
	# /LARGEADDRESSAWARE - >2GB user address space on x86
	# /VERSION - Embed version information in PE header
	set(CMAKE_EXE_LINKER_FLAGS "/DYNAMICBASE /NXCOMPAT /LARGEADDRESSAWARE /VERSION:${libgit2_VERSION_MAJOR}.${libgit2_VERSION_MINOR}")

	if(NOT (MSVC_VERSION LESS 1900))
		# /GUARD:CF - Enable Control Flow Guard
		set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /GUARD:CF")
	endif()

	# /DEBUG - Create a PDB
	# /LTCG - Link time code generation (whole program optimization)
	# /OPT:REF /OPT:ICF - Fold out duplicate code at link step
	# /INCREMENTAL:NO - Required to use /LTCG
	# /DEBUGTYPE:cv,fixup - Additional data embedded in the PDB (requires /INCREMENTAL:NO, so not on for Debug)
	set(CMAKE_EXE_LINKER_FLAGS_DEBUG "/DEBUG")
	set(CMAKE_EXE_LINKER_FLAGS_RELEASE "/RELEASE /LTCG /OPT:REF /OPT:ICF /INCREMENTAL:NO")
	set(CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO "/DEBUG /RELEASE /LTCG /OPT:REF /OPT:ICF /INCREMENTAL:NO /DEBUGTYPE:cv,fixup")
	set(CMAKE_EXE_LINKER_FLAGS_MINSIZEREL "/RELEASE /LTCG /OPT:REF /OPT:ICF /INCREMENTAL:NO")

	# Same linker settings for DLL as EXE
	set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}")
	set(CMAKE_SHARED_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS_DEBUG}")
	set(CMAKE_SHARED_LINKER_FLAGS_RELEASE "${CMAKE_EXE_LINKER_FLAGS_RELEASE}")
	set(CMAKE_SHARED_LINKER_FLAGS_RELWITHDEBINFO "${CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO}")
	set(CMAKE_SHARED_LINKER_FLAGS_MINSIZEREL "${CMAKE_EXE_LINKER_FLAGS_MINSIZEREL}")
else()
	if(ENABLE_REPRODUCIBLE_BUILDS)
		set(CMAKE_C_ARCHIVE_CREATE "<CMAKE_AR> Dqc <TARGET> <LINK_FLAGS> <OBJECTS>")
		set(CMAKE_C_ARCHIVE_APPEND "<CMAKE_AR> Dq  <TARGET> <LINK_FLAGS> <OBJECTS>")
		set(CMAKE_C_ARCHIVE_FINISH "<CMAKE_RANLIB> -D <TARGET>")
	endif()

	if(NOT BUILD_SHARED_LIBS AND LINK_WITH_STATIC_LIBRARIES)
		set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")
	endif()

	set(CMAKE_C_FLAGS "-D_GNU_SOURCE ${CMAKE_C_FLAGS}")

	enable_warnings(all)
	enable_warnings(extra)

	if(CMAKE_SYSTEM_NAME MATCHES "(Solaris|SunOS)")
		set(CMAKE_C_FLAGS "-D_POSIX_C_SOURCE=200112L -D__EXTENSIONS__ -D_POSIX_PTHREAD_SEMANTICS ${CMAKE_C_FLAGS}")
	endif()

	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -D_DEBUG -O0")

	if(MINGW OR MSYS) # MinGW and MSYS always do PIC and complain if we tell them to
		string(REGEX REPLACE "-fPIC" "" CMAKE_SHARED_LIBRARY_C_FLAGS "${CMAKE_SHARED_LIBRARY_C_FLAGS}")
	elseif(BUILD_SHARED_LIBS)
		add_c_flag_IF_SUPPORTED(-fvisibility=hidden)

		set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")
	endif()

	if(MINGW)
		# MinGW >= 3.14 uses the C99-style stdio functions
		# automatically, but forks like mingw-w64 still want
		# us to define this in order to use them
		add_definitions(-D__USE_MINGW_ANSI_STDIO=1)
	endif()

	enable_warnings(documentation)
	disable_warnings(documentation-deprecated-sync)
	disable_warnings(missing-field-initializers)
	enable_warnings(missing-declarations)
	enable_warnings(strict-aliasing)
	enable_warnings(strict-prototypes)
	enable_warnings(declaration-after-statement)
	enable_warnings(shift-count-overflow)
	enable_warnings(unused-const-variable)
	enable_warnings(unused-function)
	enable_warnings(int-conversion)
	enable_warnings(c11-extensions)
	enable_warnings(c99-c11-compat)

	# MinGW uses gcc, which expects POSIX formatting for printf, but
	# uses the Windows C library, which uses its own format specifiers.
	# Disable format specifier warnings.
	if(MINGW)
		disable_warnings(format)
		disable_warnings(format-security)
	else()
		enable_warnings(format)
		enable_warnings(format-security)
	endif()
endif()

# Ensure that MinGW provides the correct header files.
if(WIN32 AND NOT CYGWIN)
	add_definitions(-DWIN32 -D_WIN32_WINNT=0x0600)
endif()
