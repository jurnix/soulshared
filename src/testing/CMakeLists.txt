
# fypp to F90 (preprocess)
file(GLOB_RECURSE sources-fpp *.fpp)
fypp_preprocess("${sources-fpp}" sources-postpp)

# scan source code files
file(GLOB_RECURSE sources  *.f90 *.F90 *.h)

target_sources(soulshared_lib PRIVATE ${sources} ${sources-postpp})
