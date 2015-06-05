#!/bin/sh
SOURCE_PATH=/home/users/p01923/berry/pFUnit
EXTRA_ARGS=$@

rm -f CMakeCache.txt
cmake \
  -D CMAKE_Fortran_COMPILER:FILEPATH=ftn \
  -D CMAKE_VERBOSE_MAKEFILE:BOOL=TRUE \
  -D MPI=NO \
  -D OPENMP=NO \
  -D INSTALL_PATH=/home/users/p01923/bin/pFUnit \
$EXTRA_ARGS \
$SOURCE_PATH
