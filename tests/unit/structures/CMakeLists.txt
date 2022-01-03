# scan source code files
file(GLOB_RECURSE sources  *.f90 *.F90 *.h)

target_sources(soulshared_test PRIVATE ${sources})