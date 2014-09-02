#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <02 Sep 14 12:01:37 flechsig> 
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


# makefile template to built phase with automake/ autoconfig and libtool
# we built conditional libraries for the different fortran compilers i.e  
# installed libraries can coexists but- we keep only one set of object files!
# i.e. to built a library for a new compiler the objects have to be cleaned

OLDCOMMONSOURCES= \
misali.F       misali_8.f \
misali4.f      misali4_8.f \
misali3.f      misali3_8.f \
misali2.f      misali2_8.f \
misali1.f      misali1_8.f \
wdfgmapa4.for  wdfgmapb4.for   \
wdfgmapc4.for  wdfgmapd14.for  \
wdfgmapd34.for wdfgmapd44.for  \
wdfgmapd54.for wdfgmape14.for  \
wdfgmape24.for wdfgmapf4.for   \
wdfgmapg4.for  wdfgmaph4.for   \
wdfgmapi4.for  wdfgmapk14.for  \
wdfgmapk24.for wdfgmapl4.for   \
wdfgmapm14.for wdfgmapm24.for  \
lengthab4.for  lengtha4.for    \
lengthb4.for   lengthc4.for    \
lengthd400.for lengthd401.for  \
lengthd402.for lengthd410.for  \
lengthd411.for lengthd412.for  \
lengthd42.for  lengthd43.for   \
p_m_4.for  \
zernike_nl_60.for zernike_n_60.for \
dfpl1_uf.F  dfpl2_uf.F dfpl1a.F \
../phase4idl/ATnT_port3_driftroutines/drift_routines_02.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_03.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_04.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_05.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_06.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_07.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_08.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_09.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_10.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_12.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_13.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_14.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_15.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_16.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_17.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_18_no_machine_consts.f \
../phase4idl/ATnT_port3_driftroutines/port/s1mach.f \
../phase4idl/ATnT_port3_driftroutines/port/s2mach.f \
../phase4idl/ATnT_port3_driftroutines/port/s3mach.f \
../phase4idl/ATnT_port3_driftroutines/port/r1mach.f \
../phase4idl/ATnT_port3_driftroutines/port/d1mach.f \
../phase4idl/ATnT_port3_driftroutines/port/i1mach.f \
../phase4idl/ATnT_port3_driftroutines/port/s88fmt.f 

COMMONSOURCES= \
misali_8.f  \
misali4_8.f \
misali3_8.f \
misali2_8.f \
misali1_8.f \
wdfgmapm17.for \
wdfgmapm27.for \
wdfgmapl7.for \
q0_q1_q2_8.for 

## funktioniert so nicht UF -- routinen muss man einzeln dazulinken 
## oder im include den relativen pfad angeben startend von phaselib
## ../phase4idl/ATnT_port3_driftroutines/ATnT_port3_driftroutines.for
## end common sources

## we use an AM_CONDITIONAL to switch library names for different compilers
lib_LTLIBRARIES = 
if WANT_ABSOFT
   lib_LTLIBRARIES += libphaseabsoft.la
endif
if WANT_IFC
   lib_LTLIBRARIES += libphaseifc.la
   AM_FFLAGS        = -O0                ## no optimization
endif
if WANT_PORTLAND
   lib_LTLIBRARIES += libphaseportland.la
endif

libphaseabsoft_la_SOURCES   = $(COMMONSOURCES)
libphaseportland_la_SOURCES = $(COMMONSOURCES)
libphaseifc_la_SOURCES      = $(COMMONSOURCES)

## libtool versions (optional)
## libphaseabsoft_la_LDFLAGS =  -version-info 3:12:1 
## libphaseifc_la_LDFLAGS =  -version-info 3:12:1

AM_LDFLAGS = @FLIBS@ 

## we want the c- Linker and not the fortran linker
libphaseabsoft_la_LINK = $(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) $(CFLAGS) \
	$(AM_LDFLAGS) $(LDFLAGS) -o $@

libphaseifc_la_LINK = $(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) $(CFLAGS) \
	$(AM_LDFLAGS) $(LDFLAGS) -o $@

libphaseportland_la_LINK = $(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) \
	$(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

# end