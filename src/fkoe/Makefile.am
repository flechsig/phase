# File      : /afs/psi.ch/user/f/flechsig/phase/src/fkoe/Makefile.am
# Date      : <01 Apr 14 09:25:08 flechsig> 
# Time-stamp: <29 Aug 14 15:23:58 flechsig> 
# Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

# $Source$ 
# $Date$
# $Revision$ 
# $Author$ 

# ******************************************************************************
#
#   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
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

# this is the makefile template to built phase with automake/ autoconfig

bin_PROGRAMS = fkoe
fkoe_SOURCES = spa_check.F spa-check.cmn write_phase_hdf5.c \
	../phase/myhdf5.c ../phase/myhdf5.h \
	../phase/xmalloc.c ../phase/xstrdup.c ../phase/error.c 
#fkoe_mpi_LDADD = 

# files installed under data
pkgdata_DATA = 

# This is to ensure the myapp.h file is distributed in your myapp-0.1.tar.gz
# I also have a resources.qrc I need to include. The png is used in myapp.
EXTRA_DIST = fkoe.par gauss_beam.par mk_surf.par 

## flags
AM_CPPFLAGS = -DLINUX -DNOGUI
AM_CFLAGS   =  @HDF5_CFLAGS@ -I../phase -W 

AM_LDFLAGS =  @HDF5_LDFLAGS@ @FLIBS@   ##-L@HDF5@/lib -lhdf5

##we always use the c linker
##F77LINK = $(LINK)

## CLEANFILES is a variable called from target clean: rm -f $CLEANFILES
CLEANFILES = moc_*.cpp qrc_*.cpp *.f90 *.mod
# end
