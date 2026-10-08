# Standalone build of this module (as described in src/README.md, e.g.
#   cmake -B . -S ../phasesrv):
# delegate to the top-level project with only this module enabled.
if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR AND NOT PHASE_STANDALONE_DELEGATE)
  cmake_minimum_required(VERSION 3.20)
  project(phasesrv_standalone LANGUAGES NONE)
  foreach(_m PHASEQT PHASESRV HDF5TOOLS FKOE OPTI BASELIB PHASELIB)
    set(BUILD_${_m} OFF CACHE BOOL "" FORCE)
  endforeach()
  set(BUILD_PHASESRV ON CACHE BOOL "" FORCE)
  set(PHASE_STANDALONE_DELEGATE ON)
  add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/../.." "${CMAKE_BINARY_DIR}/phase")
  return()
endif()

add_executable(phasesrv
  phasesrv.c
  phasesrv.h
  ../phase/bline.c
  ../phase/cutils.c
  ../phase/error.c
  ../phase/pst.c
  ../phase/phasec.c
  ../phase/rtrace.c
  ../phase/xmalloc.c
  ../phase/xstrdup.c
  ../phase/drift_8.for
  ../phase/elli_8.F
  ../phase/fdet_8.for
  ../phase/phase_integration.F
  ../phase/fgmapidp_8.F
  ../phase/make_matrix_8.for
  ../phase/rekursiv_2d_8.F
  ../phase/phasefor.F
  ../phase/get_partial_etc.for
  ../phase/replace_6v4v.F
  ../phase/replace_wl_in_u.F
  ../phase/replace_wl_in_ypzp.F
  ../phase/taylor-ops.for
  ../phase/psdi.F
  ../phase/heighterror.c
  ../phase/heighterror.h
  ../phase/myhdf5.c
  ../phase/myhdf5.h
  ../phase/myfftw3.c
  ../phase/myfftw3.h
  ../phase/posrc.c
  ../phase/posrc.h
  ../phase/pst_thread.c
  ../phase/pst_thread.h
  ../phase/reflectivity.c
  ../phase/reflectivity.h
  ../phase/spa_3rd_order.c
  ../phase/spa_3rd_order.h
  ../phase/src4.F
)

target_compile_definitions(phasesrv PRIVATE NOGUI)
target_link_libraries(phasesrv PRIVATE phase_common_flags phase_hdf5 phase_fftw3 phase_runtime phase_threads)

install(TARGETS phasesrv RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
