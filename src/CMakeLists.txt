#
# Compile.
#
set(LIBNAME "soulshared")

# fypp to F90 (preprocess)
file(GLOB_RECURSE sources-fpp *.fpp)
fypp_preprocess("${sources-fpp}" sources-postpp)

# scan source code files
file(GLOB_RECURSE sources  *.f90 *.F90 *.h)

add_library(${LIBNAME}_lib STATIC ${sources} ${sources-postpp})

# create as lib
if (ENABLE_MPI)
  target_include_directories(${LIBNAME}_lib PUBLIC ${MPI_Fortran_INCLUDE_PATH})
  target_link_libraries(${LIBNAME}_lib PUBLIC ${MPI_Fortran_LIBRARIES})
else()
  target_include_directories(${LIBNAME}_lib PUBLIC )
  target_link_libraries(${LIBNAME}_lib PUBLIC  )
endif()

add_subdirectory(grid)
add_subdirectory(config)
add_subdirectory(arrays)
add_subdirectory(testing)
add_subdirectory(structures)
