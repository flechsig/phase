# File      : CMakeLists.txt
# Date      : <2025-08-13 15:17:26 flechsig> 
# Time-stamp: <2025-08-26 16:45:35 flechsig> 
# Author    : Flechsig Uwe, uwe.flechsig&#64;psi.&#99;&#104;

#
cmake_minimum_required(VERSION 3.16)
project(phase LANGUAGES C)

#option(GRIDSIZE "define gridsize" 255)
#add_compile_definitions(GRIDSIZE=${GRIDSIZE})

INCLUDE_DIRECTORIES (../phase )

find_package(HDF5)

add_executable(wave2phase_hdf5
  wave2phase_hdf5.c
  ../phase/error.c 
  ../phase/xmalloc.c ../phase/xstrdup.c ../phase/myhdf5.c
)

configure_file(cmake_config.h.in config.h)

TARGET_LINK_LIBRARIES(wave2phase_hdf5 ${HDF5_LIBRARIES})
