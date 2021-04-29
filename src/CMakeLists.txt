#
# Compile.
#
set(BINARY ${CMAKE_PROJECT_NAME})

# scan source code files
file(GLOB_RECURSE sources  *.f90 *.F90 *.h)

add_library(${BINARY}_lib STATIC ${sources})

# create as lib
if (ENABLE_MPI)
  target_include_directories(${BINARY}_lib PUBLIC ${MPI_Fortran_INCLUDE_PATH})
  target_link_libraries(${BINARY}_lib PUBLIC ${MPI_Fortran_LIBRARIES})
else()
  target_include_directories(${BINARY}_lib PUBLIC )
  target_link_libraries(${BINARY}_lib PUBLIC  )
endif()
