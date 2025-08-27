# File      : CMakeLists.txt
# Date      : <2025-08-13 15:17:26 flechsig> 
# Time-stamp: <2025-08-27 16:42:08 flechsig> 
# Author    : Flechsig Uwe, uwe.flechsig&#64;psi.&#99;&#104;

#
cmake_minimum_required(VERSION 3.16)
project( wave2phase_h5 LANGUAGES C)

#option(GRIDSIZE "define gridsize" 255)
#add_compile_definitions(GRIDSIZE=${GRIDSIZE})



INCLUDE_DIRECTORIES ( ${CMAKE_BINARY_DIR} ../phase )  # config.h is in ${CMAKE_BINARY_DIR}

find_package(HDF5) # sets HDF5_FOUND
#if ( ${HDF5_FOUND} )
#  message("my_message: hdf5 found")
#  add_compile_definitions(HAVE_HDF5)
#else ( ${HDF5_FOUND} )  
#  message("my_message: hdf5 not found")
#endif ( ${HDF5_FOUND} )

#message("1" ${Feature_B_FOUND})
#set(Feature_B_FOUND "ON")
#message("2" ${Feature_B_FOUND})

configure_file(cmake_config.h.in config.h @ONLY) # put it after all the search

set(AUX_FILES
  ../phase/myhdf5.c  ../phase/error.c 
  ../phase/xmalloc.c ../phase/xstrdup.c ../phase/myhdf5.c)

add_executable(wave2phase_hdf5
  ../phase/error.c 
  ../phase/xmalloc.c ../phase/xstrdup.c ../phase/myhdf5.c 
  wave2phase_hdf5.c
)

LINK_DIRECTORIES( ../phase )
TARGET_LINK_LIBRARIES(wave2phase_hdf5 PRIVATE HDF5::HDF5)

# end file
