cmake_minimum_required(VERSION 2.8)

project (neutran)
set(CMAKE_BUILD_TYPE Release)
enable_language (Fortran)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)
add_library(neutran SHARED src/types.f95 src/functions.f95 src/neuron.f95
    src/layer.f95 src/nnet.f95)


add_custom_command(
    TARGET neutran POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy
    ${CMAKE_SOURCE_DIR}/release/*.mod
    ${CMAKE_CURRENT_SOURCE_DIR}/include
)
add_custom_command(
    TARGET neutran POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy
    ${CMAKE_SOURCE_DIR}/release/*.so
    ${CMAKE_CURRENT_SOURCE_DIR}/lib
)
