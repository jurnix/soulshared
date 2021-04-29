# Soulshared

Fundamental Fortran functions to share across soulsm dependencies.

## Download
How to download the source code:
git clone http://sofie.pku.edu.cn:8000/lsm/soulshared.git

## Compile
How to compile (place into the main folder):
1. (make sure netcdf and netcdf fortran libraries are installed/loaded)
1. mkdir build
1. cd build
1. Chose to compile in debug (fix problems) or release mode (fast)
   - cmake -DCMAKE_BUILD_TYPE=Debug/Release ..
   - cmake -DENABLE_MPI=yes/no ..
   - cmake -DBUILD_TESTING=yes/no ..
1. make


## Unit Test
How to run its unit tests:
1. -go to the main project folder
1. -./bin/soulshared_test

