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

include(${EXTERNAL_DIR}/conanbuildinfo.cmake)

add_executable(${PROJECT_NAME}
        main.cpp)

target_include_directories(${PROJECT_NAME} PRIVATE ${CONAN_INCLUDE_DIRS})

target_link_directories(${PROJECT_NAME} PRIVATE ${CONAN_LIB_DIRS})

target_link_libraries(${PROJECT_NAME} PRIVATE ${CONAN_LIBS})

install(TARGETS ${PROJECT_NAME} DESTINATION ${BINARY_DIR})
