file(GLOB SRC_LLHTTP "*.c" "*.h")
list(SORT SRC_LLHTTP)

add_library(llhttp OBJECT ${SRC_LLHTTP})

if(NOT MSVC)
	set_source_files_properties(api.c http.c llhttp.c PROPERTIES COMPILE_FLAGS "-Wno-unused-parameter -Wno-missing-declarations")
endif()
