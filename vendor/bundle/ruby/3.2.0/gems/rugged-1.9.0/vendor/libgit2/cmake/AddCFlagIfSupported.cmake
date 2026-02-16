# - Append compiler flag to CMAKE_C_FLAGS if compiler supports it
# ADD_C_FLAG_IF_SUPPORTED(<flag>)
#  <flag> - the compiler flag to test
# This internally calls the CHECK_C_COMPILER_FLAG macro.

include(CheckCCompilerFlag)

macro(ADD_C_FLAG _FLAG)
	string(TOUPPER ${_FLAG} UPCASE)
	string(REGEX REPLACE "[-=]" "_" UPCASE_PRETTY ${UPCASE})
	string(REGEX REPLACE "^_+" "" UPCASE_PRETTY ${UPCASE_PRETTY})
	check_c_compiler_flag(${_FLAG} IS_${UPCASE_PRETTY}_SUPPORTED)

	if(IS_${UPCASE_PRETTY}_SUPPORTED)
		set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${_FLAG}")
	else()
		message(FATAL_ERROR "Required flag ${_FLAG} is not supported")
	endif()
endmacro()

macro(ADD_C_FLAG_IF_SUPPORTED _FLAG)
	string(TOUPPER ${_FLAG} UPCASE)
	string(REGEX REPLACE "[-=]" "_" UPCASE_PRETTY ${UPCASE})
	string(REGEX REPLACE "^_+" "" UPCASE_PRETTY ${UPCASE_PRETTY})
	check_c_compiler_flag(${_FLAG} IS_${UPCASE_PRETTY}_SUPPORTED)

	if(IS_${UPCASE_PRETTY}_SUPPORTED)
		set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${_FLAG}")
	endif()
endmacro()
