cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
 
project(PCLVisualizer)
target_link_libraries (PCLVisualizer ${PCL_LIBRARIES})

#it seems it's needed only on OS X 10.9
find_package(GLEW REQUIRED)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -I/usr/include -v")

find_package(PCL 1.7 REQUIRED)
 
include_directories(${PCL_INCLUDE_DIRS})
link_directories(${PCL_LIBRARY_DIRS})
add_definitions(${PCL_DEFINITIONS})
 
set(PCL_BUILD_TYPE Release)
 
file(GLOB PCL_openni_viewer_SRC
    "src/*.h"
    "src/*.cpp"
)
add_executable(PCLVisualizer ${PCL_openni_viewer_SRC})

#add this line to solve probem in mac os x 10.9
target_link_libraries(PCLVisualizer ${PCL_COMMON_LIBRARIES} ${PCL_IO_LIBRARIES} ${PCL_VISUALIZATION_LIBRARIES} ${PCL_FEATURES_LIBRARIES}) 