cmake_minimum_required(VERSION 3.10)
project(phaselib LANGUAGES Fortran C)

# ---- Optionen für verschiedene Compiler ----
option(WANT_ABSOFT    "Build with Absoft Fortran"    OFF)
option(WANT_IFC       "Build with Intel Fortran"     OFF)
option(WANT_PORTLAND  "Build with Portland Fortran"  OFF)
option(WANT_GNU       "Build with GNU Fortran"       ON)   # Default: GNU

# ---- Quellcode-Dateien ----
set(COMMONSOURCES
    misali_8.f
    misali4_8.f
    misali3_8.f
    misali2_8.f
    misali1_8.f
    wdfgmapm17.for
    wdfgmapm27.for
    wdfgmapl7.for
    q0_q1_q2_8.for
)

# ---- Bibliotheken je nach Compiler ----
if(WANT_ABSOFT)
    add_library(phaseabsoft SHARED ${COMMONSOURCES})
    set_target_properties(phaseabsoft PROPERTIES OUTPUT_NAME "phaseabsoft")
    # Hier ggf. spezielle Compilerflags für Absoft setzen
endif()

if(WANT_IFC)
    add_library(phaseifc SHARED ${COMMONSOURCES})
    set_target_properties(phaseifc PROPERTIES OUTPUT_NAME "phaseifc")
    # Kein Optimieren für Intel Fortran
    target_compile_options(phaseifc PRIVATE -O0)
endif()

if(WANT_PORTLAND)
    add_library(phaseportland SHARED ${COMMONSOURCES})
    set_target_properties(phaseportland PROPERTIES OUTPUT_NAME "phaseportland")
    # Hier ggf. spezielle Compilerflags für Portland setzen
endif()

if(WANT_GNU)
    add_library(phasegnu SHARED ${COMMONSOURCES})
    set_target_properties(phasegnu PROPERTIES OUTPUT_NAME "phasegnu")
    # Hier ggf. spezielle Compilerflags für GNU setzen
endif()

# ---- C als Linker für Fortran-Bibliotheken ----
# CMake verwendet automatisch die richtigen Linker, wenn C und Fortran im Projekt aktiviert sind.
# Falls du spezielle Linkerflags brauchst, kannst du sie so setzen:
# target_link_options(phasegnu PRIVATE <flags>)

# ---- Hinweise ----
# Die Pfade zu den Quellen müssen ggf. angepasst werden.
# Für komplexe Abhängigkeiten oder mehrere Compiler empfiehlt sich ein separates Build-Verzeichnis pro Compiler.
# Wenn du mehrere Compiler gleichzeitig bauen willst, musst du mehrere Build-Verzeichnisse anlegen und jeweils mit -DWANT_X=ON konfigurieren.

# ---- Installationsregeln ----