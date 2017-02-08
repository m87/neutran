cmake_minimum_required(VERSION 2.8)

project (tests)
set(CMAKE_BUILD_TYPE Release)
enable_language (Fortran)

link_directories(${CMAKE_CURRENT_SOURCE_DIR}/lib ${PRERELEASE}/)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include ${PRERELEASE}/)

add_executable(neuronTest.test src/neuronTest.f95)
TARGET_LINK_LIBRARIES(neuronTest.test sft neutran)

add_executable(netTest.test src/netTest.f95)
TARGET_LINK_LIBRARIES(netTest.test sft neutran)

set(UNIT_TEST neuronTest.test netTest.test)
add_custom_command(
TARGET ${UNIT_TEST}
COMMENT "Run tests"
POST_BUILD 
COMMAND ${UNIT_TEST}
)


