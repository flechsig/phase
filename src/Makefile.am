##  File      : /afs/psi.ch/user/f/flechsig/c/gnubuild-test/Makefile.am
##  Date      : <02 Dec 03 15:46:24 flechsig> 
##  Time-stamp: <11 Mar 14 09:54:33 flechsig> 
##  Author    : Uwe Flechsig, flechsig@psi.ch

##  $Source$ 
##  $Date$
##  $Revision$ 
##  $Author$ 

## Makefile.am the inputfile for automake
## is a comment in automake
## += is a automake assignment transfered to = in Makefile.in

## top level Makefile
 
## UF 13.11.07 took phaselib out of subdirs - it must be 
## now built/installed separately

EXTRA_DIST = BUGS ChangeLog.O bin/apps_home.sh configure

bin_SCRIPTS = bin/create-coefficients.pl bin/phaseinit.csh  \
	      bin/phaseinit.sh           bin/scale-wave-output.pl

## UF Mar 2014 SUBDIRS = phaseqt phasesrv extr opti hdf5tools phase4idl idlphase

SUBDIRS = phaseqt phasesrv extr opti hdf5tools idlphase

ACLOCAL_AMFLAGS = -I config

## gui phase4idl idlphase phaselib phaseidl

## UF baselib erst einfuegen wenn es wirklich geht
## baselib 

## end /afs/psi.ch/user/f/flechsig/phase/src/Makefile.am
