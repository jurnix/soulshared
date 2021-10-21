cmake_minimum_required(VERSION 3.9)

project(soulshared)
enable_language(Fortran)

find_program(FYPP fypp)
if(NOT FYPP)
  message(FATAL_ERROR "Preprocessor fypp could not be found")
endif()

# custom compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(dialect "-ffree-form -std=f2008 -fimplicit-none")
    set(debugMode "-fbounds-check -pedantic -ffpe-trap=zero,invalid,overflow,underflow -O0 -Wall -fcheck=all -fbacktrace -Wextra")
    set(optimizedMode "-ftree-vectorize" ) 
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(dialect "-stand f08 -free -implicitnone")
    set(debugMode "-check bounds")
    set(optimizedMode "-O3 -xHost")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
    set(dialect "-Mfreeform -Mdclchk -Mstandard -Mallocatable=03")
    set(debugMode "-C")
    set(optimizedMode "")
endif()

set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${optimizedMode}")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${debugMode}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")

# include cmake modules
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")
include(fypp)

# Place lib and binary files
# dynamic libraries
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)

# static library
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib) 

# target files
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

# Have the .mod files placed in the lib folder
SET(LIB ${CMAKE_SOURCE_DIR}/lib)
SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})

message(STATUS "Run: ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} EXECUTABLE ${MPIEXEC_POSTFLAGS} ARGS")

if (ENABLE_MPI)
  find_package (MPI REQUIRED)
  add_definitions(-DENABLE_MPI)
endif()

add_subdirectory(src)

if(BUILD_TESTING)
  enable_testing()
  SET( CMAKE_BUILD_TYPE Debug )

  add_subdirectory(tests)
endif()
