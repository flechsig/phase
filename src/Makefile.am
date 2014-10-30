##  File      : /afs/psi.ch/user/f/flechsig/c/gnubuild-test/Makefile.am
##  Date      : <02 Dec 03 15:46:24 flechsig> 
##  Time-stamp: <30 Oct 14 14:51:33 flechsig> 
##  Author    : Uwe Flechsig, flechsig@psi.ch

##  $Source$ 
##  $Date$
##  $Revision$ 
##  $Author$ 

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

## Makefile.am the inputfile for automake
## is a comment in automake
## += is a automake assignment transfered to = in Makefile.in

## top level Makefile
 
## UF 13.11.07 took phaselib out of subdirs - it must be 
## now built/installed separately

EXTRA_DIST = BUGS ChangeLog.O bin/apps_home.sh configure COPYING LICENSE.txt

bin_SCRIPTS = bin/create-coefficients.pl bin/phaseinit.csh  \
	      bin/phaseinit.sh           bin/scale-wave-output.pl

## UF Mar 2014 SUBDIRS = phaseqt phasesrv extr opti hdf5tools phase4idl idlphase

SUBDIRS = phaseqt phasesrv hdf5tools fkoe

ACLOCAL_AMFLAGS = -I config

## gui phase4idl idlphase phaselib phaseidl

## UF baselib erst einfuegen wenn es wirklich geht
## baselib 

## end /afs/psi.ch/user/f/flechsig/phase/src/Makefile.am
