cmake_minimum_required(VERSION 2.8)

project (tests)
set(CMAKE_BUILD_TYPE Release)
enable_language (Fortran)

LINK_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}../lib)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}../lib/include)

add_executable(neuronTest.test neuronTest.f95)
TARGET_LINK_LIBRARIES(neuronTest.test sft)
TARGET_LINK_LIBRARIES(neuronTest.test neutran)


add_custom_command(
    TARGET neutran POST_BUILD
    COMMAND ./neuronTest.test
)
