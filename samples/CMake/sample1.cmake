cmake_minimum_required(VERSION 2.6)

enable_testing()

set(CMAKE_BUILD_TYPE debug)

include_directories("/usr/local/include")

find_library(ssl_LIBRARY NAMES ssl PATHS "/usr/local/lib")

add_custom_command(OUTPUT "ver.c" "ver.h" COMMAND ./ver.sh)

add_executable(foo foo.c bar.c baz.c ver.c)

target_link_libraries(foo ${ssl_LIBRARY})
