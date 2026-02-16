include(CheckPrototypeDefinition)

function(check_prototype_definition_safe function prototype return header variable)
	# temporarily save CMAKE_C_FLAGS and disable warnings about unused
	# unused functions and parameters, otherwise they will always fail
	# if ENABLE_WERROR is on
	set(SAVED_CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")

	disable_warnings(unused-function)
	disable_warnings(unused-parameter)

	check_prototype_definition("${function}" "${prototype}" "${return}" "${header}" "${variable}")

	# restore CMAKE_C_FLAGS
	set(CMAKE_C_FLAGS "${SAVED_CMAKE_C_FLAGS}")
endfunction()
