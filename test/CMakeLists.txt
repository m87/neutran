cmake_minimum_required(VERSION 2.8)

project (tests)
set(CMAKE_BUILD_TYPE Release)
enable_language (Fortran)

link_directories(${CMAKE_CURRENT_SOURCE_DIR}/lib ${PRERELEASE}/../include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include ${PRERELEASE}/../lib)

add_executable(neuronTest.test src/neuronTest.f95)
TARGET_LINK_LIBRARIES(neuronTest.test sft neutran)

#add_executable(netTest.test src/netTest.f95)
#TARGET_LINK_LIBRARIES(netTest.test sft neutran)

add_executable(layerTest.test src/layerTest.f95)
TARGET_LINK_LIBRARIES(layerTest.test sft neutran)

#add_executable(functionsTest.test src/functionsTest.f95)
#TARGET_LINK_LIBRARIES(functionsTest.test sft neutran)



#add_custom_command(
#    TARGET functionsTest.test
#    COMMENT "Run functionsTest"
#    POST_BUILD 
#    COMMAND functionsTest.test
#)

add_custom_command(
    TARGET neuronTest.test
    COMMENT "Run neuronTest"
    POST_BUILD 
    COMMAND neuronTest.test
)

add_custom_command(
    TARGET layerTest.test
    COMMENT "Run layerTest"
    POST_BUILD 
    COMMAND layerTest.test
)

#add_custom_command(
#    TARGET netTest.test
#    COMMENT "Run netTest"
#    POST_BUILD 
#    COMMAND netTest.test
#)

