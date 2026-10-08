add_executable(phaseopti
  phaseopti.cpp
  phaseopti.h
  mycost.cpp
  mycost.h
  optisubc.c
  optisubc.h
  ../phase/xmalloc.c
  ../phase/fgmapidp_8.F
  ../phase/make_matrix_8.for
  ../phase/fdet_8.for
  ../phase/taylor-ops.for
  ../phase/rekursiv_2d_8.F
  ../phase/get_partial_etc.for
  ../phase/replace_6v4v.F
  ../phase/replace_wl_in_u.F
  ../phase/replace_wl_in_ypzp.F
  ../phase/drift_8.for
  ../phase/elli_8.F
  ../phase/phasefor.F
  ../phase/error.c
  ../phase/xstrdup.c
  ../phase/cutils.c
  ../phase/rtrace.c
  ../phase/bline.c
  ../phase/reflectivity.c
  ../phase/reflectivity.h
  ../phase/heighterror.c
  ../phase/heighterror.h
  ../phase/myhdf5.c
  ../phase/myhdf5.h
)

target_include_directories(phaseopti PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ../phase
)

target_compile_definitions(phaseopti PRIVATE OPTI NOGUI)
target_link_libraries(phaseopti PRIVATE
  phase_common_flags
  phase_hdf5
  phase_runtime
  phase_root
  m
)

add_executable(opti_root_example
  opti_root_example.cpp
  opti_root_example.h
)

target_include_directories(opti_root_example PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
)

target_link_libraries(opti_root_example PRIVATE
  phase_root
)

install(TARGETS phaseopti opti_root_example RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
