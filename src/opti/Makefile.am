#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <2021-05-28 11:38:07 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

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

# makefile template to built phase with automake/ autoconfig

bin_PROGRAMS       = phaseopti opti_root_example

phaseopti_SOURCES = phaseopti.cpp phaseopti.h \
	mycost.cpp mycost.h \
	optisubc.c optisubc.h \
	../phase/xmalloc.c \
	../phase/fgmapidp_8.F   ../phase/make_matrix_8.for \
	../phase/fdet_8.for     ../phase/taylor-ops.for\
	../phase/rekursiv_2d_8.F \
	../phase/get_partial_etc.for ../phase/replace_6v4v.F \
	../phase/replace_wl_in_u.F ../phase/replace_wl_in_ypzp.F \
	../phase/drift_8.for    ../phase/elli_8.F ../phase/hyperbola_8.F \
	../phase/phasefor.F \
	../phase/error.c        ../phase/xstrdup.c \
	../phase/cutils.c       ../phase/rtrace.c \
	../phase/bline.c  \
	../phase/reflectivity.c ../phase/reflectivity.h  \
	../phase/heighterror.c ../phase/heighterror.h  \
	../phase/myhdf5.c        ../phase/myhdf5.h   

opti_root_example_SOURCES = opti_root_example.cpp opti_root_example.h

phaseopti_LDADD = $(libdir)/@LIBPHASE@

## flags
AM_CPPFLAGS = -DLINUX -DOPTI -DNOGUI
AM_CXXFLAGS=  $(ROOT_CFLAGS) -I. -I../phase -W
AM_LDFLAGS =  $(ROOT_LIBS) -lMinuit @MY_HDF5_LIBS@ @FLIBS@ @CFORLIB@

## we use the c linker
F77LINK = $(LINK)
## end




