#!/bin/sh
SOURCE_PATH=/home/users/p01923/berry/pFUnit-berry-branch
EXTRA_ARGS=$@

rm -f CMakeCache.txt
cmake \
  -D CMAKE_Fortran_COMPILER:FILEPATH=ftn \
  -D CMAKE_VERBOSE_MAKEFILE:BOOL=TRUE \
  -DMPI=YES \
  -DOPENMP=NO \
  -DINSTALL_PATH=/home/users/p01923/bin/pFUnit \
$EXTRA_ARGS \
$SOURCE_PATH
