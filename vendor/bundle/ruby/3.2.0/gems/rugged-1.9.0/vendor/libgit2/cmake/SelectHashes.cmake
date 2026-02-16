# Select a hash backend

include(SanitizeBool)

sanitizebool(USE_SHA1)
sanitizebool(USE_SHA256)

# sha1

if(USE_SHA1 STREQUAL "" OR USE_SHA1 STREQUAL ON)
	SET(USE_SHA1 "CollisionDetection")
elseif(USE_SHA1 STREQUAL "HTTPS")
	if(USE_HTTPS STREQUAL "SecureTransport")
		set(USE_SHA1 "CommonCrypto")
	elseif(USE_HTTPS STREQUAL "Schannel")
		set(USE_SHA1 "Win32")
	elseif(USE_HTTPS STREQUAL "WinHTTP")
		set(USE_SHA1 "Win32")
	elseif(USE_HTTPS)
		set(USE_SHA1 ${USE_HTTPS})
	else()
		message(FATAL_ERROR "asked for HTTPS SHA1 backend but HTTPS is not enabled")
	endif()
endif()

if(USE_SHA1 STREQUAL "CollisionDetection")
	set(GIT_SHA1_COLLISIONDETECT 1)
elseif(USE_SHA1 STREQUAL "OpenSSL")
	set(GIT_SHA1_OPENSSL 1)
elseif(USE_SHA1 STREQUAL "OpenSSL-FIPS")
	set(GIT_SHA1_OPENSSL_FIPS 1)
elseif(USE_SHA1 STREQUAL "OpenSSL-Dynamic")
	set(GIT_SHA1_OPENSSL 1)
	set(GIT_SHA1_OPENSSL_DYNAMIC 1)
	list(APPEND LIBGIT2_SYSTEM_LIBS dl)
elseif(USE_SHA1 STREQUAL "CommonCrypto")
	set(GIT_SHA1_COMMON_CRYPTO 1)
elseif(USE_SHA1 STREQUAL "mbedTLS")
	set(GIT_SHA1_MBEDTLS 1)
elseif(USE_SHA1 STREQUAL "Win32")
	set(GIT_SHA1_WIN32 1)
else()
	message(FATAL_ERROR "asked for unknown SHA1 backend: ${USE_SHA1}")
endif()

# sha256

if(USE_SHA256 STREQUAL "" OR USE_SHA256 STREQUAL ON)
	if(USE_HTTPS)
		SET(USE_SHA256 "HTTPS")
	else()
		SET(USE_SHA256 "builtin")
	endif()
endif()

if(USE_SHA256 STREQUAL "Builtin")
	set(USE_SHA256 "builtin")
endif()

if(USE_SHA256 STREQUAL "HTTPS")
	if(USE_HTTPS STREQUAL "SecureTransport")
		set(USE_SHA256 "CommonCrypto")
	elseif(USE_HTTPS STREQUAL "Schannel")
		set(USE_SHA256 "Win32")
	elseif(USE_HTTPS STREQUAL "WinHTTP")
		set(USE_SHA256 "Win32")
	elseif(USE_HTTPS)
		set(USE_SHA256 ${USE_HTTPS})
	endif()
endif()

if(USE_SHA256 STREQUAL "builtin")
	set(GIT_SHA256_BUILTIN 1)
elseif(USE_SHA256 STREQUAL "OpenSSL")
	set(GIT_SHA256_OPENSSL 1)
elseif(USE_SHA256 STREQUAL "OpenSSL-FIPS")
	set(GIT_SHA256_OPENSSL_FIPS 1)
elseif(USE_SHA256 STREQUAL "OpenSSL-Dynamic")
	set(GIT_SHA256_OPENSSL 1)
	set(GIT_SHA256_OPENSSL_DYNAMIC 1)
	list(APPEND LIBGIT2_SYSTEM_LIBS dl)
elseif(USE_SHA256 STREQUAL "CommonCrypto")
	set(GIT_SHA256_COMMON_CRYPTO 1)
elseif(USE_SHA256 STREQUAL "mbedTLS")
	set(GIT_SHA256_MBEDTLS 1)
elseif(USE_SHA256 STREQUAL "Win32")
	set(GIT_SHA256_WIN32 1)
else()
	message(FATAL_ERROR "asked for unknown SHA256 backend: ${USE_SHA256}")
endif()

# add library requirements
if(USE_SHA1 STREQUAL "OpenSSL" OR USE_SHA256 STREQUAL "OpenSSL" OR
   USE_SHA1 STREQUAL "OpenSSL-FIPS" OR USE_SHA256 STREQUAL "OpenSSL-FIPS")
	if(CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
		list(APPEND LIBGIT2_PC_LIBS "-lssl")
	else()
		list(APPEND LIBGIT2_PC_REQUIRES "openssl")
	endif()
endif()

if(USE_SHA1 STREQUAL "mbedTLS" OR USE_SHA256 STREQUAL "mbedTLS")
	list(APPEND LIBGIT2_SYSTEM_INCLUDES ${MBEDTLS_INCLUDE_DIR})
	list(APPEND LIBGIT2_SYSTEM_LIBS ${MBEDTLS_LIBRARIES})
	# mbedTLS has no pkgconfig file, hence we can't require it
	# https://github.com/ARMmbed/mbedtls/issues/228
	# For now, pass its link flags as our own
	list(APPEND LIBGIT2_PC_LIBS ${MBEDTLS_LIBRARIES})
endif()

# notify feature enablement

add_feature_info(SHA1 ON "using ${USE_SHA1}")
add_feature_info(SHA256 ON "using ${USE_SHA256}")

# warn for users who do not use sha1dc

if(NOT "${USE_SHA1}" STREQUAL "CollisionDetection")
	list(APPEND WARNINGS "SHA1 support is set to ${USE_SHA1} which is not recommended - git's hash algorithm is sha1dc, it is *not* SHA1. Using SHA1 may leave you and your users susceptible to SHAttered-style attacks.")
	set(WARNINGS ${WARNINGS} PARENT_SCOPE)
endif()
