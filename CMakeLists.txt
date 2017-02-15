cmake_minimum_required(VERSION 2.8)

project (neutran)
set(CMAKE_BUILD_TYPE Release)
enable_language (Fortran)

set(PRERELEASE ${CMAKE_BINARY_DIR})

include_directories(${PRERELEASE})
add_library(neutran SHARED src/types.f95 src/functions.f95 src/neuron.f95
    src/initMethods.f95)

add_subdirectory(test)

install(TARGETS neutran
       LIBRARY DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/lib    
       INCLUDES DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/include
)

