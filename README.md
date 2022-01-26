# Soulshared

Soulshared is a fundamental Fortran library meant to be share across soulsm packages.

## Dependencies

* gnu gfortran compiler
  * Mpi libraries
  * OpenMp libraries 
* CMake
* Git
* Makefile (ubuntu build-essentials package)
* pip install fypp -> fortran preprocessor
  * Enable it by adding into PATH environment variable


## Download
How to download the source code:

* git clone http://sofie.pku.edu.cn:8000/lsm/soulshared.git

## Compile
How to compile (place into the main folder):
1. (make sure netcdf and netcdf fortran libraries are installed/loaded)
1. mkdir build
1. cd build
1. Chose to compile in debug (fix problems) or release mode (fast)
   1. cmake -DCMAKE_BUILD_TYPE=Debug/Release ..
   2. cmake -DENABLE_MPI=yes/no ..
   3. cmake -DBUILD_TESTING=yes/no ..
1. make


## Unit Test
How to run its unit tests:
1. Go to the main project folder
1. Run ./bin/soulshared_test

