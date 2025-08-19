# File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/CMakeLists.txt
# Date      : <2025-08-13 15:17:26 flechsig> 
# Time-stamp: <2025-08-19 16:22:14 flechsig> 
# Author    : Flechsig Uwe, uwe.flechsig&#64;psi.&#99;&#104;

#
cmake_minimum_required(VERSION 3.16)
project(phasesrv LANGUAGES C)

option(GRIDSIZE "define gridsize" 255)
add_compile_definitions(GRIDSIZE=${GRIDSIZE})

INCLUDE_DIRECTORIES (../phase )

add_executable(phasesrv
  phasesrv.c phasesrv.h
  #../phase/bline.c      ../phase/cutils.c
  ../phase/error.c      ../phase/pst.c 
  ../phase/phasec.c     ../phase/rtrace.c
  ../phase/xmalloc.c    ../phase/xstrdup.c
  ../phase/drift_8.for  ../phase/elli_8.F
)
    
TARGET_LINK_LIBRARIES(phasesrv m)
