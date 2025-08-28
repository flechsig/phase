# File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/CMakeLists.txt
# Date      : <2025-08-13 15:17:26 flechsig> 
# Time-stamp: <2025-08-28 17:36:20 flechsig> 
# Author    : Flechsig Uwe, uwe.flechsig&#64;psi.&#99;&#104;

#
cmake_minimum_required(VERSION 3.16)
project(phasesrv LANGUAGES C Fortran )

option(GRIDSIZE "define gridsize" 255)
add_compile_definitions(GRIDSIZE=${GRIDSIZE})

INCLUDE_DIRECTORIES ( ${CMAKE_BINARY_DIR} ../phase )

configure_file(cmake_config.h.in config.h @ONLY) # put it after all the search
add_compile_definitions(HAVE_CONFIG_H)           # use config should be optional

set(AUX_FILES  ../phase/error.c  ../phase/xmalloc.c ../phase/xstrdup.c ../phase/myhdf5.c )
add_executable(phasesrv ${AUX_FILES}
  phasesrv.c 
  #../phase/bline.c      ../phase/cutils.c
        ../phase/pst.c 
  ../phase/phasec.c     ../phase/rtrace.c
  
  ../phase/drift_8.for  ../phase/elli_8.F
)
    
TARGET_LINK_LIBRARIES(phasesrv m)
