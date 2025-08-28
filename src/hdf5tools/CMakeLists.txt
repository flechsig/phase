# File      : CMakeLists.txt
# Date      : <2025-08-13 15:17:26 flechsig> 
# Time-stamp: <2025-08-28 16:17:30 flechsig> 
# Author    : Flechsig Uwe, uwe.flechsig&#64;psi.&#99;&#104;

#******************************************************************************
#
#   Copyright (C) 2025 Helmholtz-Zentrum Berlin, Germany and 
#                      Paul Scherrer Institut Villigen, Switzerland
#   
#   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
#          Uwe Flechsig,    uwe.flechsig@psi.ch
#
# ------------------------------------------------------------------------------
#
#   This file is part of PHASE.
#
#   PHASE is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, version 3 of the License, or
#   (at your option) any later version.
#
#   PHASE is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
#
# ******************************************************************************

cmake_minimum_required(VERSION 3.16)

project( hdf5tools LANGUAGES C CXX)

INCLUDE_DIRECTORIES ( ${CMAKE_BINARY_DIR} ../phase ../phaseqt )  # config.h is in ${CMAKE_BINARY_DIR}

find_package(HDF5) # sets HDF5_FOUND

configure_file(cmake_config.h.in config.h @ONLY) # put it after all the search
add_compile_definitions(HAVE_CONFIG_H)           # use config should be optional

set(AUX_FILES  ../phase/error.c  ../phase/xmalloc.c ../phase/xstrdup.c )

add_executable(add_phaseu_2h5     ${AUX_FILES} ../phase/myhdf5.c ../phaseqt/unwrap_phase.cpp add_phaseu_2h5.c )
add_executable(genesis2phase_hdf5 ${AUX_FILES} genesis2phase_hdf5.c )
add_executable(genesis_hdf5_2wave ${AUX_FILES} genesis_hdf5_2wave.c )
add_executable(phase_hdf5_2wave   ${AUX_FILES} ../phase/myhdf5.c phase_hdf5_2wave.c )
add_executable(txtlog2phase_hdf5  ${AUX_FILES} ../phase/myhdf5.c txtlog2phase_hdf5.c )
add_executable(wave2genesis_hdf5  ${AUX_FILES} wave2genesis_hdf5.c )
add_executable(wave2phase_hdf5    ${AUX_FILES} ../phase/myhdf5.c wave2phase_hdf5.c )

TARGET_LINK_LIBRARIES(add_phaseu_2h5     PRIVATE HDF5::HDF5 m)
TARGET_LINK_LIBRARIES(genesis2phase_hdf5 PRIVATE HDF5::HDF5)
TARGET_LINK_LIBRARIES(genesis_hdf5_2wave PRIVATE HDF5::HDF5 m)
TARGET_LINK_LIBRARIES(phase_hdf5_2wave   PRIVATE HDF5::HDF5)
TARGET_LINK_LIBRARIES(txtlog2phase_hdf5  PRIVATE HDF5::HDF5)
TARGET_LINK_LIBRARIES(wave2genesis_hdf5  PRIVATE HDF5::HDF5)
TARGET_LINK_LIBRARIES(wave2phase_hdf5    PRIVATE HDF5::HDF5)

# end file
