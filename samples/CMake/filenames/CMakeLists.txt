cmake_minimum_required(VERSION 2.8)

project(Foo)

set(CMAKE_SKIP_RPATH TRUE)
set(CMAKE_INSTALL_PREFIX "/usr/local")

add_subdirectory(bar)

add_executable(foo foo.c)
target_link_libraries(foo pthread)
install(TARGETS foo DESTINATION bin)
