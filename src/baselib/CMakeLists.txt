# CMakeLists.txt für phaselib

# Falls noch nicht gesetzt:
# cmake_minimum_required(VERSION 3.20)
# project(phase Fortran C)

# Alle Quellcodes (C, Fortran, Header)
set(PHASELIB_COMMON_SOURCES
    error.c xmalloc.c xstrdup.c cutils.c
    phaBLBuild.c
    phaBLReadFromFile.c phaBLInit.c
    DefGeometryC.c DefMirrorC.c MakeMapandMatrix.c SetFilePos.c
    SetDeltaLambda.c rrtrace.c
    misali.F misali1.f misali2.f misali3.f misali4.f
    phasefor.F phase_integration.F phase_source.F
    wdfgmapa4.for  wdfgmapb4.for
    wdfgmapc4.for  wdfgmapd14.for
    wdfgmapd34.for wdfgmapd44.for
    wdfgmapd54.for wdfgmape14.for
    wdfgmape24.for wdfgmapf4.for
    wdfgmapg4.for  wdfgmaph4.for
    wdfgmapi4.for  wdfgmapk14.for
    wdfgmapk24.for wdfgmapl4.for
    wdfgmapm14.for wdfgmapm24.for
    lengthab4.for  lengtha4.for
    lengthb4.for   lengthc4.for
    lengthd400.for lengthd401.for
    lengthd402.for lengthd410.for
    lengthd411.for lengthd412.for
    lengthd42.for  lengthd43.for
    p_m_4.for
    zernike_nl_60.for zernike_n_60.for
    dfpl1_uf.F  dfpl2_uf.F
    # Header werden nicht kompiliert, aber ggf. für Install/Export
    # phabasestructs.h phabasedefs.h idl_export.h
)

# Optionen für verschiedene Fortran-Compiler
option(WANT_ABSOFT "Build phaselib with Absoft Fortran" OFF)
option(WANT_IFC "Build phaselib with Intel Fortran" OFF)
option(WANT_PORTLAND "Build phaselib with Portland Group Fortran" OFF)

# Bedingte Targets für die verschiedenen Compiler
if(WANT_ABSOFT)
    add_library(phabaseabsoft STATIC ${PHASELIB_COMMON_SOURCES})
    set_target_properties(phabaseabsoft PROPERTIES OUTPUT_NAME "phabaseabsoft")
    # Hier ggf. Compiler-spezifische Flags setzen:
    # target_compile_options(phabaseabsoft PRIVATE <Absoft-Flags>)
endif()

if(WANT_IFC)
    add_library(phabaseifc STATIC ${PHASELIB_COMMON_SOURCES})
    set_target_properties(phabaseifc PROPERTIES OUTPUT_NAME "phabaseifc")
    # target_compile_options(phabaseifc PRIVATE <Intel-Flags>)
endif()

if(WANT_PORTLAND)
    add_library(phabaseportland STATIC ${PHASELIB_COMMON_SOURCES})
    set_target_properties(phabaseportland PROPERTIES OUTPUT_NAME "phabaseportland")
    # target_compile_options(phabaseportland PRIVATE <Portland-Flags>)
endif()

# Optional: Header installieren
# install(FILES phabasestructs.h phabasedefs.h idl_export.h DESTINATION include/phaselib)
