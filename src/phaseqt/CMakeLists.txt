# phaseqt cannot be configured on its own (it needs the shared targets and options of the
# top-level project). Use:  cmake -S <phase-root> -B build -DBUILD_PHASEQT=ON
if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
  cmake_minimum_required(VERSION 3.20)
  project(phaseqt_standalone LANGUAGES NONE)
  message(FATAL_ERROR
    "phaseqt must be built from the top-level project:\n"
    "  cmake -S ${CMAKE_CURRENT_SOURCE_DIR}/../.. -B build -DBUILD_PHASEQT=ON")
endif()

set(PHASE_QT_COMPONENTS Core Gui Widgets Svg PrintSupport Concurrent OpenGL)

if(NOT DEFINED PHASE_QT_PREFIX)
  set(PHASE_QT_PREFIX "" CACHE PATH "Optional Qt installation prefix (contains lib/cmake/Qt6 or lib/cmake/Qt5)")
endif()

set(_phase_qt_prefix_hints)
set(_phase_qt_exclude_re "android|ios|wasm|winrt|mingw|msvc|ucrt|qnx")
set(_phase_qt_prefer_re "gcc|clang|linux|desktop")

function(_phase_collect_qt_prefixes _out_var _pkg _base_prefix)
  if(NOT _base_prefix)
    return()
  endif()

  file(TO_CMAKE_PATH "${_base_prefix}" _phase_qt_root)
  if(NOT EXISTS "${_phase_qt_root}")
    return()
  endif()

  set(_preferred)
  set(_fallback)

  # Direct kit prefix (e.g. .../Qt/6.10.2/gcc_arm64)
  if(EXISTS "${_phase_qt_root}/lib/cmake/${_pkg}/${_pkg}Config.cmake")
    get_filename_component(_name "${_phase_qt_root}" NAME)
    string(TOLOWER "${_name}" _name_lc)
    if(NOT _name_lc MATCHES "${_phase_qt_exclude_re}")
      if(_name_lc MATCHES "${_phase_qt_prefer_re}")
        list(APPEND _preferred "${_phase_qt_root}")
      else()
        list(APPEND _fallback "${_phase_qt_root}")
      endif()
    endif()
  endif()

  # Version prefix (e.g. .../Qt/6.10.2) -> scan one level below for kits.
  file(GLOB _phase_qt_children LIST_DIRECTORIES true "${_phase_qt_root}/*")
  foreach(_child IN LISTS _phase_qt_children)
    if(EXISTS "${_child}/lib/cmake/${_pkg}/${_pkg}Config.cmake")
      get_filename_component(_name "${_child}" NAME)
      string(TOLOWER "${_name}" _name_lc)
      if(_name_lc MATCHES "${_phase_qt_exclude_re}")
        continue()
      endif()
      if(_name_lc MATCHES "${_phase_qt_prefer_re}")
        list(APPEND _preferred "${_child}")
      else()
        list(APPEND _fallback "${_child}")
      endif()
    endif()
  endforeach()

  set(_collected "${${_out_var}}")
  list(APPEND _collected ${_preferred} ${_fallback})
  set(${_out_var} "${_collected}" PARENT_SCOPE)
endfunction()

if(USE_QT6)
  set(_phase_qt_package Qt6)
  set(_phase_qt_targets Qt6::Core Qt6::Gui Qt6::Widgets Qt6::Svg Qt6::PrintSupport Qt6::Concurrent Qt6::OpenGL)

  if(DEFINED ENV{Qt6_DIR} AND NOT "$ENV{Qt6_DIR}" STREQUAL "" AND NOT Qt6_DIR)
    set(Qt6_DIR "$ENV{Qt6_DIR}" CACHE PATH "Path to Qt6Config.cmake")
  endif()
  if(DEFINED ENV{Qt6_ROOT} AND NOT "$ENV{Qt6_ROOT}" STREQUAL "")
    _phase_collect_qt_prefixes(_phase_qt_prefix_hints Qt6 "$ENV{Qt6_ROOT}")
  endif()
else()
  set(_phase_qt_package Qt5)
  set(_phase_qt_targets Qt5::Core Qt5::Gui Qt5::Widgets Qt5::Svg Qt5::PrintSupport Qt5::Concurrent Qt5::OpenGL)

  if(DEFINED ENV{Qt5_DIR} AND NOT "$ENV{Qt5_DIR}" STREQUAL "" AND NOT Qt5_DIR)
    set(Qt5_DIR "$ENV{Qt5_DIR}" CACHE PATH "Path to Qt5Config.cmake")
  endif()
  if(DEFINED ENV{Qt5_ROOT} AND NOT "$ENV{Qt5_ROOT}" STREQUAL "")
    _phase_collect_qt_prefixes(_phase_qt_prefix_hints Qt5 "$ENV{Qt5_ROOT}")
  endif()
endif()

if(PHASE_QT_PREFIX)
  _phase_collect_qt_prefixes(_phase_qt_prefix_hints ${_phase_qt_package} "${PHASE_QT_PREFIX}")
endif()

if(DEFINED ENV{QTDIR} AND NOT "$ENV{QTDIR}" STREQUAL "")
  _phase_collect_qt_prefixes(_phase_qt_prefix_hints ${_phase_qt_package} "$ENV{QTDIR}")
endif()

if(USE_QT6 AND (NOT Qt6_DIR))
  if(DEFINED ENV{HOME} AND NOT "$ENV{HOME}" STREQUAL "")
    foreach(_root IN ITEMS "$ENV{HOME}/Qt" "$ENV{HOME}/qt" "$ENV{HOME}/opt/Qt" "$ENV{HOME}/.local/Qt")
      if(EXISTS "${_root}")
        _phase_collect_qt_prefixes(_phase_qt_prefix_hints Qt6 "${_root}")
        file(GLOB _phase_qt_versions LIST_DIRECTORIES true "${_root}/*")
        foreach(_ver IN LISTS _phase_qt_versions)
          _phase_collect_qt_prefixes(_phase_qt_prefix_hints Qt6 "${_ver}")
        endforeach()
      endif()
    endforeach()
  endif()
endif()

list(REMOVE_DUPLICATES _phase_qt_prefix_hints)
if(_phase_qt_prefix_hints)
  find_package(${_phase_qt_package} COMPONENTS ${PHASE_QT_COMPONENTS}
    QUIET
    HINTS ${_phase_qt_prefix_hints}
  )
endif()

if(NOT ${_phase_qt_package}_FOUND)
  find_package(${_phase_qt_package} COMPONENTS ${PHASE_QT_COMPONENTS} REQUIRED)
endif()

set(QT_LIBS ${_phase_qt_targets})

if(USE_QWT)
  find_package(Qwt REQUIRED)
endif()

set(PHASEQT_SOURCES
  e2wavelength.cpp
  configwindow.cpp
  main.cpp
  mainwindow_slots.cpp
  mainwindow.cpp
  myzoomer.cpp
  optiinput.cpp
  phaseqt.cpp
  plot.cpp
  plot1d.cpp
  singleray.cpp
  treemodel.cpp
  treeitem.cpp
  plotmatrix.cpp
  plot2x2.cpp
  unwrap_phase.cpp
  phaseqt.qrc
  ../phase/bline.c
  ../phase/cutils.c
  ../phase/error.c
  ../phase/phasec.c
  ../phase/OptiIntParam.c
  ../phase/rtrace.c
  ../phase/myhdf5.c
  ../phase/myfftw3.c
  ../phase/xmalloc.c
  ../phase/pst_thread.c
  ../phase/drift_8.for
  ../phase/elli_8.F
  ../phase/fdet_8.for
  ../phase/fgmapidp_8.F
  ../phase/make_matrix_8.for
  ../phase/rekursiv_2d_8.F
  ../phase/phasefor.F
  ../phase/get_partial_etc.for
  ../phase/replace_6v4v.F
  ../phase/replace_wl_in_u.F
  ../phase/replace_wl_in_ypzp.F
  ../phase/taylor-ops.for
  ../phase/pst.c
  ../phase/phase_integration.F
  ../phase/posrc.c
  ../phase/spa_3rd_order.c
  ../phase/reflectivity.c
  ../phase/heighterror.c
  ../phase/psdi.F
  ../phase/src4.F
)

set(PHASEQT_HEADERS
  e2wavelength.h
  configwindow.h
  mainwindow.h
  myzoomer.h
  optiinput.h
  phaseqt.h
  plot.h
  plot1d.h
  singleray.h
  treemodel.h
  treeitem.h
  plotmatrix.h
  plot2x2.h
  unwrap_phase.h
  ../phase/myhdf5.h
  ../phase/myfftw3.h
  ../phase/pst.h
  ../phase/posrc.h
  ../phase/spa_3rd_order.h
  ../phase/reflectivity.h
  ../phase/heighterror.h
)

add_executable(phaseqt ${PHASEQT_SOURCES} ${PHASEQT_HEADERS})

set_target_properties(phaseqt PROPERTIES
  AUTOMOC ON
  AUTORCC ON
)

target_include_directories(phaseqt PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ../phase
)

target_compile_definitions(phaseqt PRIVATE LINUX QTGUI QT_GUI_LIB QT_CORE_LIB QT_SHARED)
if(USE_QT6)
  target_compile_definitions(phaseqt PRIVATE QT6 QT56)
else()
  target_compile_definitions(phaseqt PRIVATE QT5 QT56)
endif()
if(USE_QWT)
  target_compile_definitions(phaseqt PRIVATE HAVE_QWT)
endif()

target_compile_options(phaseqt PRIVATE -fPIC)

target_link_libraries(phaseqt PRIVATE
  phase_common_flags
  phase_hdf5
  phase_fftw3
  phase_runtime
  phase_threads
  ${QT_LIBS}
)

if(USE_QWT)
  target_include_directories(phaseqt PRIVATE ${QWT_INCLUDE_DIRS})
  target_link_libraries(phaseqt PRIVATE ${QWT_LIBRARIES})
endif()

if(UNIX AND NOT APPLE)
  # Ensure installed binaries can find Qt shared libraries from a non-system Qt prefix.
  set_target_properties(phaseqt PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)
  if(PHASE_QT_PREFIX)
    set_target_properties(phaseqt PROPERTIES INSTALL_RPATH "${PHASE_QT_PREFIX}/lib")
  endif()
endif()

install(TARGETS phaseqt RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
set(PHASEQT_RUNTIME_DATA_FILES
  ../data/coefficient-example.coeff
  ../data/map70_lh.omx
  ../data/map70_rh.omx
  ../data/spa3table.tab
  ../data/spa3table_20001.tab
  ../data/material/rhoatom.dat
  ../data/henke/Ac.f12 ../data/henke/Ag.f12 ../data/henke/Al.f12
  ../data/henke/Ar.f12 ../data/henke/As.f12 ../data/henke/At.f12
  ../data/henke/Au.f12 ../data/henke/Ba.f12 ../data/henke/Be.f12
  ../data/henke/B.f12 ../data/henke/Bi.f12 ../data/henke/Br.f12
  ../data/henke/Ca.f12 ../data/henke/Cd.f12 ../data/henke/Ce.f12
  ../data/henke/C.f12 ../data/henke/Cl.f12 ../data/henke/Co.f12
  ../data/henke/Cr.f12 ../data/henke/Cs.f12 ../data/henke/Cu.f12
  ../data/henke/Dy.f12 ../data/henke/Er.f12 ../data/henke/Eu.f12
  ../data/henke/Fe.f12 ../data/henke/F.f12 ../data/henke/Fr.f12
  ../data/henke/Ga.f12 ../data/henke/Gd.f12 ../data/henke/Ge.f12
  ../data/henke/He.f12 ../data/henke/H.f12 ../data/henke/Hf.f12
  ../data/henke/Hg.f12 ../data/henke/Ho.f12 ../data/henke/I.f12
  ../data/henke/In.f12 ../data/henke/Ir.f12 ../data/henke/K.f12
  ../data/henke/Kr.f12 ../data/henke/La.f12 ../data/henke/Li.f12
  ../data/henke/Lu.f12 ../data/henke/Mg.f12 ../data/henke/Mn.f12
  ../data/henke/Mo.f12 ../data/henke/Na.f12 ../data/henke/Nb.f12
  ../data/henke/Nd.f12 ../data/henke/Ne.f12 ../data/henke/N.f12
  ../data/henke/Ni.f12 ../data/henke/O.f12 ../data/henke/Os.f12
  ../data/henke/Pa.f12 ../data/henke/Pb.f12 ../data/henke/Pd.f12
  ../data/henke/P.f12 ../data/henke/Pm.f12 ../data/henke/Po.f12
  ../data/henke/Pr.f12 ../data/henke/Pt.f12 ../data/henke/Ra.f12
  ../data/henke/Rb.f12 ../data/henke/Re.f12 ../data/henke/Rh.f12
  ../data/henke/Rn.f12 ../data/henke/Ru.f12 ../data/henke/Sb.f12
  ../data/henke/Sc.f12 ../data/henke/Se.f12 ../data/henke/S.f12
  ../data/henke/Si.f12 ../data/henke/Sm.f12 ../data/henke/Sn.f12
  ../data/henke/Sr.f12 ../data/henke/Ta.f12 ../data/henke/Tb.f12
  ../data/henke/Tc.f12 ../data/henke/Te.f12 ../data/henke/Th.f12
  ../data/henke/Ti.f12 ../data/henke/Tl.f12 ../data/henke/Tm.f12
  ../data/henke/U.f12 ../data/henke/V.f12 ../data/henke/W.f12
  ../data/henke/Xe.f12 ../data/henke/Yb.f12 ../data/henke/Y.f12
  ../data/henke/Zn.f12 ../data/henke/Zr.f12 ../data/henke/B4C.f12
)

install(FILES
  ${PHASEQT_RUNTIME_DATA_FILES}
  DESTINATION ${CMAKE_INSTALL_DATADIR}/phaseqt/data
)

# The data tables are also installed to share/phase/ by the top-level CMakeLists.txt
# (needed by phasesrv without phaseqt).

install(FILES
  phaseqt.qrc parameter.default
  images/Blue-darrow-32.png images/Blue-tarrow-32.png
  images/Blue-arrow-left-32.png images/Blue-arrow-right-32.png
  images/Blue-arrows-right-32.png images/down-32.png images/footprint.png
  images/Left-arrow-48.png images/new.png images/print.png
  images/quickrtrace.png images/Right-arrow-48.png images/rtrace.png
  images/save.png images/undo.png images/up-32.png
  README
  DESTINATION ${CMAKE_INSTALL_DATADIR}/phaseqt
)

set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "moc_*.cpp;qrc_*.cpp;*.f90;*.mod")
