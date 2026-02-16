include(EnableWarnings)

if(APPLE)
	# We cannot simply CHECK_FUNCTION_EXISTS on macOS because
	# MACOSX_DEPLOYMENT_TARGET may be set to a version in the past
	# that doesn't have futimens.  Instead we need to enable warnings
	# as errors, then check for the symbol existing in `sys/stat.h`,
	# then reset warnings as errors.
	enable_warnings(error)
	check_symbol_exists(futimens sys/stat.h HAVE_FUTIMENS)
	disable_warnings(error)
else()
	check_function_exists(futimens HAVE_FUTIMENS)
endif()
