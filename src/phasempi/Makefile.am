#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phasempi/Makefile.am
#  Date      : <17 May 13 17:36:23 flechsig> 
#  Time-stamp: <2023-08-08 11:29:29 flechsig> 
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
#   along with PHASE (src/LICENSE).  If not, see <http:#www.gnu.org/licenses/>. 
#
# ******************************************************************************

# this is the makefile template to built phase with automake/ autoconfig

bin_PROGRAMS  = phasempi 
phasempi_SOURCES = \
        phasempi.c       phasempi.h \
        ../phase/bline.c ../phase/cutils.c \
	../phase/error.c ../phase/pst.c \
        ../phase/phasec.c     ../phase/rtrace.c \
        ../phase/xmalloc.c    ../phase/xstrdup.c  \
	../phase/drift_8.for  ../phase/elli_8.F  \
	../phase/fdet_8.for   ../phase/phase_integration.F \
	../phase/fgmapidp_8.F        ../phase/make_matrix_8.for \
	../phase/rekursiv_2d_8.F     ../phase/phasefor.F  \
	../phase/get_partial_etc.for ../phase/replace_6v4v.F \
	../phase/replace_wl_in_u.F   ../phase/replace_wl_in_ypzp.F \
	../phase/taylor-ops.for      ../phase/psdi.F \
	../phase/heighterror.c   ../phase/heighterror.h \
	../phase/myhdf5.c        ../phase/myhdf5.h \
	../phase/myfftw3.c       ../phase/myfftw3.h \
        ../phase/posrc.c         ../phase/posrc.h \
	../phase/pst_thread.c    ../phase/pst_thread.h \
	../phase/reflectivity.c  ../phase/reflectivity.h \
	../phase/spa_3rd_order.c ../phase/spa_3rd_order.h \
	../phase/src4.F

phasempi_LDADD = $(libdir)/@LIBPHASE@

# files installed under data
pkgdata_DATA = ../data/coefficient-example.dat \
../data/map70_lh.omx\
../data/map70_rh.omx\
../data/spa3table.tab \
../data/spa3table_20001.tab \
../data/material/rhoatom.dat \
../data/henke/Ac.f12 ../data/henke/Ag.f12 ../data/henke/Al.f12 \
../data/henke/Ar.f12 ../data/henke/As.f12 ../data/henke/At.f12 \
../data/henke/Au.f12 \
../data/henke/Ba.f12 ../data/henke/Be.f12 ../data/henke/B.f12 \
../data/henke/Bi.f12 ../data/henke/Br.f12 \
../data/henke/Ca.f12 ../data/henke/Cd.f12 ../data/henke/Ce.f12 \
../data/henke/C.f12  ../data/henke/Cl.f12 ../data/henke/Co.f12 \
../data/henke/Cr.f12 ../data/henke/Cs.f12 ../data/henke/Cu.f12 \
../data/henke/Dy.f12 ../data/henke/Er.f12 ../data/henke/Eu.f12 \
../data/henke/Fe.f12 ../data/henke/F.f12  ../data/henke/Fr.f12 \
../data/henke/Ga.f12 ../data/henke/Gd.f12 ../data/henke/Ge.f12 \
../data/henke/He.f12 ../data/henke/H.f12  ../data/henke/Hf.f12 \
../data/henke/Hg.f12 ../data/henke/Ho.f12 ../data/henke/I.f12  \
../data/henke/In.f12 ../data/henke/Ir.f12 ../data/henke/K.f12  \
../data/henke/Kr.f12 ../data/henke/La.f12 ../data/henke/Li.f12 \
../data/henke/Lu.f12 ../data/henke/Mg.f12 ../data/henke/Mn.f12 \
../data/henke/Mo.f12 ../data/henke/Na.f12 ../data/henke/Nb.f12 \
../data/henke/Nd.f12 ../data/henke/Ne.f12 ../data/henke/N.f12  \
../data/henke/Ni.f12 ../data/henke/O.f12  ../data/henke/Os.f12 \
../data/henke/Pa.f12 ../data/henke/Pb.f12 ../data/henke/Pd.f12 \
../data/henke/P.f12  ../data/henke/Pm.f12 ../data/henke/Po.f12 \
../data/henke/Pr.f12 ../data/henke/Pt.f12 ../data/henke/Ra.f12 \
../data/henke/Rb.f12 ../data/henke/Re.f12 ../data/henke/Rh.f12 \
../data/henke/Rn.f12 ../data/henke/Ru.f12 ../data/henke/Sb.f12 \
../data/henke/Sc.f12 ../data/henke/Se.f12 ../data/henke/S.f12  \
../data/henke/Si.f12 ../data/henke/Sm.f12 ../data/henke/Sn.f12 \
../data/henke/Sr.f12 ../data/henke/Ta.f12 ../data/henke/Tb.f12 \
../data/henke/Tc.f12 ../data/henke/Te.f12 ../data/henke/Th.f12 \
../data/henke/Ti.f12 ../data/henke/Tl.f12 ../data/henke/Tm.f12 \
../data/henke/U.f12  ../data/henke/V.f12  ../data/henke/W.f12  \
../data/henke/Xe.f12 ../data/henke/Yb.f12 ../data/henke/Y.f12  \
../data/henke/Zn.f12 ../data/henke/Zr.f12 ../data/henke/B4C.f12

# This is to ensure the myapp.h file is distributed in your myapp-0.1.tar.gz
# I also have a resources.qrc I need to include. The png is used in myapp.
EXTRA_DIST = ../phase/phase_struct.F ../phase/beamline_struct.F $(pkgdata_DATA) \
	phaseqt.qrc parameter.default \
	images/Blue-arrow-left-32.png images/Blue-arrow-right-32.png \
	images/down-32.png images/footprint.png \
	images/Left-arrow-48.png images/new.png \
	images/print.png  images/quickrtrace.png \
	images/Right-arrow-48.png images/rtrace.png \
	images/save.png images/undo.png images/up-32.png \
	README

## flags
AM_CPPFLAGS = -DLINUX -DNOGUI @HDF5_CPPFLAGS@
AM_CFLAGS   =  @HDF5_CFLAGS@ -I../phase -W 
AM_LDFLAGS  =  @MY_HDF5_LIBS@ @FLIBS@ @FFTW3_LIBS@ 

##we always use the c linker
F77LINK = $(LINK)

# This line ensures that generated moc_*.cpp and resources.cpp files are
# deleted when we make clean.
##CLEANFILES = $(filter moc_%.cpp,$(phaseqt_SOURCES)) 
## CLEANFILES is a variable called from target clean: rm -f $CLEANFILES
CLEANFILES = moc_*.cpp qrc_*.cpp *.f90 *.mod
# end
