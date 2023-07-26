cmake_minimum_required(VERSION 3.22)
project(Calculator)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(BINARY_NAME  ${PROJECT_NAME})
set(PROJECT_DIR  ${PROJECT_SOURCE_DIR})
set(EXTERNAL_DIR ${PROJECT_DIR}/external)
set(BINARY_DIR   ${PROJECT_DIR}/bin)

message(STATUS "Calculator")
message(STATUS "    BinaryName:  ${BINARY_NAME}")
message(STATUS "    ProjectDir:  ${PROJECT_DIR}")
message(STATUS "    BinaryDir:   ${BINARY_DIR}")

if (${MSVC})
    add_compile_options(/utf-8)
endif()

set(EXTERNAL_INCLUDE_DIRS )
set(EXTERNAL_LIBS         )

# AsmJit

find_package(asmjit REQUIRED)

set(EXTERNAL_INCLUDE_DIRS ${EXTERNAL_INCLUDE_DIRS} ${asmjit_INCLUDE_DIRS})
set(EXTERNAL_LIBS         ${EXTERNAL_LIBS}         ${asmjit_LIBRARIES})

# GSCrossPlatform

find_package(GSCrossPlatform 1.0 REQUIRED)

set(EXTERNAL_INCLUDE_DIRS ${EXTERNAL_INCLUDE_DIRS} ${GSCrossPlatform_INCLUDE_DIRS})
set(EXTERNAL_LIBS         ${EXTERNAL_LIBS}         ${GSCrossPlatform_LIBRARIES})

# args (taywee-args)

find_package(args REQUIRED)

set(EXTERNAL_INCLUDE_DIRS ${EXTERNAL_INCLUDE_DIRS} ${args_INCLUDE_DIRS})

add_executable(${PROJECT_NAME}
        main.cpp)

target_include_directories(${PROJECT_NAME} PRIVATE ${EXTERNAL_INCLUDE_DIRS})

target_link_libraries(${PROJECT_NAME} PRIVATE ${EXTERNAL_LIBS})

install(TARGETS ${PROJECT_NAME} DESTINATION ${BINARY_DIR})
