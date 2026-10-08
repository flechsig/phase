cmake_minimum_required(VERSION 3.20)

# PHASE uses legacy Fortran sources that require gfortran in practice.
# If no compiler was specified explicitly, prefer gfortran automatically.
if(NOT DEFINED CMAKE_Fortran_COMPILER AND NOT DEFINED ENV{FC})
  find_program(PHASE_DEFAULT_GFORTRAN NAMES gfortran)
  if(PHASE_DEFAULT_GFORTRAN)
    set(CMAKE_Fortran_COMPILER "${PHASE_DEFAULT_GFORTRAN}" CACHE FILEPATH
      "Fortran compiler used by PHASE")
  endif()
endif()

project(phase
  VERSION 1.33
  DESCRIPTION "PHASE Simulation Software"
  LANGUAGES C CXX Fortran
)

include(GNUInstallDirs)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules")

# Build options
option(USE_MPI "Enable MPI support" OFF)
option(USE_DEBUG "Enable debug build" OFF)
option(USE_OPENMP "Enable OpenMP" OFF)
option(USE_QT6 "Prefer Qt6 over Qt5" ON)
option(USE_QWT "Enable QWT support" ON)
set(PHASE_QT_PREFIX "" CACHE PATH "Optional Qt installation prefix (contains lib/cmake/Qt6 or lib/cmake/Qt5)")

option(BUILD_PHASEQT "Build phaseqt GUI" ON)
option(BUILD_PHASESRV "Build phasesrv" ON)
option(BUILD_HDF5TOOLS "Build hdf5tools" ON)
option(BUILD_FKOE "Build fkoe" ON)
option(BUILD_OPTI "Build opti (experimental)" OFF)
option(BUILD_BASELIB "Build baselib (experimental)" OFF)
option(BUILD_PHASELIB "Build phaselib (experimental)" OFF)

set(GRIDSIZE 1 CACHE STRING "Max grid size for PO source")

if(PHASE_QT_PREFIX)
  list(PREPEND CMAKE_PREFIX_PATH "${PHASE_QT_PREFIX}")
endif()

if(USE_DEBUG)
  set(default_build_type Debug)
else()
  set(default_build_type Release)
endif()

if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE STRING "Build type" FORCE)
endif()

# Legacy C sources rely on permissive C behavior and configure-style feature defines.
set(CMAKE_C_STANDARD 90)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

add_library(phase_common_flags INTERFACE)
target_compile_definitions(phase_common_flags INTERFACE
  LINUX
  STDC_HEADERS=1
  HAVE_UNISTD_H=1
  HAVE_ERRNO_H=1
  SEVEN_ORDER=1
  VERSION="${PROJECT_VERSION}"
  CONFIGURED="cmake"
  GRIDSIZE=${GRIDSIZE}
  PHASE_SOURCE_TREE_DIR="${CMAKE_CURRENT_SOURCE_DIR}"
)
target_include_directories(phase_common_flags INTERFACE
  "${CMAKE_CURRENT_SOURCE_DIR}/src/phase"
)

if(CMAKE_C_COMPILER_ID MATCHES "Clang")
  target_compile_options(phase_common_flags INTERFACE
    "$<$<COMPILE_LANGUAGE:C>:-Wno-implicit-function-declaration>"
  )
endif()

# Fortran compiler compatibility markers used in the GUI and naming conventions.
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU|LLVMFlang|Flang")
  add_compile_definitions(HAVE_GFORTRAN_F77)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  add_compile_definitions(HAVE_INTEL_F77)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC")
  add_compile_definitions(HAVE_PORTLAND_F77)
endif()

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:--std=legacy>")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "LLVMFlang|Flang")
  message(FATAL_ERROR
    "LLVM Flang is not compatible with PHASE legacy Fortran sources.\n"
    "Use gfortran instead. Example:\n"
    "  rm -rf build\n"
    "  cmake -S . -B build -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=Release\n"
    "  cmake --build build -j"
  )
endif()

if(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64")
  add_compile_definitions(HAVE_X86_64)
endif()

if(USE_MPI)
  find_package(MPI REQUIRED)
  add_compile_definitions(USE_MPI)
endif()

if(USE_OPENMP)
  find_package(OpenMP REQUIRED)
endif()

set(PHASE_NEEDS_HDF5 OFF)
if(BUILD_PHASEQT OR BUILD_PHASESRV OR BUILD_HDF5TOOLS OR BUILD_FKOE OR BUILD_OPTI)
  set(PHASE_NEEDS_HDF5 ON)
endif()

if(PHASE_NEEDS_HDF5)
  find_package(HDF5 REQUIRED COMPONENTS C HL)
  add_library(phase_hdf5 INTERFACE)
  target_compile_definitions(phase_hdf5 INTERFACE HAVE_HDF5)
  target_include_directories(phase_hdf5 INTERFACE ${HDF5_INCLUDE_DIRS})
  target_link_libraries(phase_hdf5 INTERFACE ${HDF5_LIBRARIES})
endif()

set(PHASE_NEEDS_FFTW3 OFF)
if(BUILD_PHASEQT OR BUILD_PHASESRV OR BUILD_OPTI)
  set(PHASE_NEEDS_FFTW3 ON)
endif()

if(PHASE_NEEDS_FFTW3)
  find_package(FFTW3 REQUIRED)
  add_library(phase_fftw3 INTERFACE)
  target_compile_definitions(phase_fftw3 INTERFACE HAVE_FFTW3)
  target_include_directories(phase_fftw3 INTERFACE ${FFTW3_INCLUDE_DIRS})
  target_link_libraries(phase_fftw3 INTERFACE ${FFTW3_LIBRARIES})
endif()

if(BUILD_PHASEQT OR BUILD_PHASESRV)
  find_package(Threads REQUIRED)
  add_library(phase_threads INTERFACE)
  target_link_libraries(phase_threads INTERFACE Threads::Threads)
endif()

set(PHASE_NEEDS_RUNTIME_LIB OFF)
if(BUILD_PHASEQT OR BUILD_PHASESRV OR BUILD_OPTI)
  set(PHASE_NEEDS_RUNTIME_LIB ON)
endif()

if(PHASE_NEEDS_RUNTIME_LIB)
  add_library(phase_runtime STATIC
    src/phaselib/misali_8.f
    src/phaselib/misali4_8.f
    src/phaselib/misali3_8.f
    src/phaselib/misali2_8.f
    src/phaselib/misali1_8.f
    src/phaselib/wdfgmapm17.for
    src/phaselib/wdfgmapm27.for
    src/phaselib/wdfgmapl7.for
    src/phaselib/q0_q1_q2_8.for
  )
  target_link_libraries(phase_runtime PUBLIC phase_common_flags)
endif()

if(BUILD_OPTI)
  find_package(ROOT REQUIRED COMPONENTS Minuit)
  add_library(phase_root INTERFACE)
  target_include_directories(phase_root INTERFACE ${ROOT_INCLUDE_DIRS})
  target_link_libraries(phase_root INTERFACE ${ROOT_LIBRARIES})
endif()

if(BUILD_BASELIB)
  add_subdirectory(src/baselib)
endif()

if(BUILD_PHASELIB)
  add_subdirectory(src/phaselib)
endif()

if(BUILD_PHASEQT)
  add_subdirectory(src/phaseqt)
endif()

if(BUILD_PHASESRV)
  add_subdirectory(src/phasesrv)
endif()

if(BUILD_HDF5TOOLS)
  add_subdirectory(src/hdf5tools)
endif()

if(BUILD_FKOE)
  add_subdirectory(src/fkoe)
endif()

if(BUILD_OPTI)
  add_subdirectory(src/opti)
endif()

# Runtime data tables, flat layout <prefix>/share/phase/<file> as probed by the code
# (phase_resolve_data_file). Needed by every program that traces or propagates, also
# for a headless install without phaseqt.
if(BUILD_PHASEQT OR BUILD_PHASESRV OR BUILD_OPTI)
  file(GLOB PHASE_HENKE_FILES "${CMAKE_CURRENT_SOURCE_DIR}/src/data/henke/*.f12")
  install(FILES
    src/data/coefficient-example.coeff
    src/data/map35_lh.omx src/data/map35_rh.omx
    src/data/map70_lh.omx src/data/map70_rh.omx
    src/data/spa3table.tab src/data/spa3table_20001.tab
    src/data/material/rhoatom.dat
    ${PHASE_HENKE_FILES}
    DESTINATION ${CMAKE_INSTALL_DATADIR}/phase
  )
endif()

message(STATUS "Build configuration summary:")
message(STATUS "  Install prefix:      ${CMAKE_INSTALL_PREFIX}")
message(STATUS "  Build type:          ${CMAKE_BUILD_TYPE}")
message(STATUS "  GRIDSIZE:            ${GRIDSIZE}")
message(STATUS "  MPI support:         ${USE_MPI}")
message(STATUS "  OpenMP support:      ${USE_OPENMP}")
message(STATUS "  Build phaseqt:       ${BUILD_PHASEQT}")
message(STATUS "  Build phasesrv:      ${BUILD_PHASESRV}")
message(STATUS "  Build hdf5tools:     ${BUILD_HDF5TOOLS}")
message(STATUS "  Build fkoe:          ${BUILD_FKOE}")
message(STATUS "  Build opti:          ${BUILD_OPTI}")
message(STATUS "  Build baselib:       ${BUILD_BASELIB}")
message(STATUS "  Build phaselib:      ${BUILD_PHASELIB}")
if(PHASE_QT_PREFIX)
  message(STATUS "  Qt prefix hint:      ${PHASE_QT_PREFIX}")
endif()
if(HDF5_FOUND)
  message(STATUS "  HDF5 version:        ${HDF5_VERSION}")
endif()
if(FFTW3_FOUND)
  message(STATUS "  FFTW3 support:       ${FFTW3_FOUND}")
endif()
