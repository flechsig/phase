#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <2012-11-04 16:53:17 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# makefile template to built phase with automake/ autoconfig

bin_PROGRAMS       = phaseopti opti_root_example

phaseopti_SOURCES = phaseopti.cpp phaseopti.h \
	mycost.cpp mycost.h \
	optisubc.c optisubc.h

opti_root_example_SOURCES = opti_root_example.cpp opti_root_example.h

## flags
AM_CPPFLAGS = -DLINUX -DOPTI -DNOGUI
AM_CXXFLAGS=  $(ROOT_CFLAGS) -I. -I../phase -W
AM_LDFLAGS =  $(ROOT_LIBS) -lMinuit @FLIBS@ @CFORLIB@

## we use the c linker
F77LINK = $(LINK)
## end




