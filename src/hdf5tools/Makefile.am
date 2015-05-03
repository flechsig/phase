#  File      : /afs/psi.ch/user/f/flechsig/phase/src/hdf5tools/Makefile.am
#  Date      : <15 Mar 13 12:33:54 flechsig> 
#  Time-stamp: <2015-05-03 09:52:43 flechsig> 
#  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

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

bin_PROGRAMS  = wave2phase_hdf5 genesis2phase_hdf5 wave2genesis_hdf5 genesis_hdf5_2wave phase_hdf5_2wave add_phaseu_2h5

add_phaseu_2h5_SOURCES = add_phaseu_2h5.c ../phase/error.c \
	../phase/xmalloc.c ../phase/xstrdup.c  ../phase/myhdf5.c ../phaseqt/unwrap_phase.cpp

wave2phase_hdf5_SOURCES = wave2phase_hdf5.c ../phase/error.c \
	../phase/xmalloc.c ../phase/xstrdup.c ../phase/myhdf5.c

genesis2phase_hdf5_SOURCES = genesis2phase_hdf5.c ../phase/error.c \
	../phase/xmalloc.c ../phase/xstrdup.c 

wave2genesis_hdf5_SOURCES = wave2genesis_hdf5.c ../phase/error.c \
	../phase/xmalloc.c ../phase/xstrdup.c  

genesis_hdf5_2wave_SOURCES = genesis_hdf5_2wave.c ../phase/error.c \
	../phase/xmalloc.c ../phase/xstrdup.c

phase_hdf5_2wave_SOURCES = phase_hdf5_2wave.c ../phase/error.c \
	../phase/xmalloc.c ../phase/xstrdup.c  ../phase/myhdf5.c

##dummy_DATA = \
##	configwindow.cpp configwindow.h \
##	main.cpp mainwindow.cpp mainwindow_slots.cpp mainwindow.h \
##        myzoomer.cpp myzoomer.h \
##        optiinput.cpp optiinput.h  \
##	phaseqt.cpp phaseqt.h \
##	plot.cpp plot.h \
##	plot1d.cpp plot1d.h \
##	singleray.cpp singleray.h \
##        treemodel.cpp treemodel.h \
##        treeitem.cpp treeitem.h \
##	plotmatrix.cpp plotmatrix.h \
##        plot2x2.cpp plot2x2.h \
##        ../phase/bline.c ../phase/cutils.c ../phase/error.c \
##	../phase/phasec.c  ../phase/rtrace.c \
##        ../phase/xmalloc.c   \
##	../phase/drift_8.for    ../phase/elli_8.F ../phase/fdet_8.for \
##	../phase/fgmapidp_8.F   ../phase/make_matrix_8.for \
##	../phase/q0_q1_q2_8.for ../phase/rekursiv_2d_8.for \
##	../phase/phasefor.F  \
##	../phase/get_partial_etc.for ../phase/replace_6v4v.for \
##	../phase/replace_wl_in_u.for ../phase/replace_wl_in_ypzp.for \
##	../phase/taylor-ops.for \
##	../phase/phase_source.F ../phase/pst.c ../phase/phase_integration.F \
##        ../phase/wdfgmapm17.for ../phase/wdfgmapm27.for ../phase/wdfgmapl7.for \
##        ../phase/pstf.F ../phase/posrc.c ../phase/posrc.h \
##	../phaselib/misali_8.f ../phaselib/misali4_8.f \
##        ../phaselib/misali3_8.f ../phaselib/misali2_8.f ../phaselib/misali1_8.f



## UF 28.8.2012 take this out for phasesrv
#phasesrv_LDADD = $(libdir)/@LIBPHASE@

# files installed under data
pkgdata_DATA = 

# This is to ensure the myapp.h file is distributed in your myapp-0.1.tar.gz
# I also have a resources.qrc I need to include. The png is used in myapp.
EXTRA_DIST = ../phase/phase_struct.F  $(pkgdata_DATA) \
	README

## flags
AM_CPPFLAGS = -DLINUX -DNOGUI @HDF5_CPPFLAGS@
AM_CFLAGS   = -I../phase -W 
AM_LDFLAGS  = @MY_HDF5_LIBS@ -lm -lz    

##we always use the c linker
F77LINK = $(LINK)

# This line ensures that generated moc_*.cpp and resources.cpp files are
# deleted when we make clean.
##CLEANFILES = $(filter moc_%.cpp,$(phaseqt_SOURCES)) 
## CLEANFILES is a variable called from target clean: rm -f $CLEANFILES
CLEANFILES = moc_*.cpp qrc_*.cpp *.f90 *.mod
# end
