#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phasesrv/Makefile.am
#  Date      : <17 May 13 17:36:23 flechsig> 
#  Time-stamp: <13 Aug 14 14:08:34 flechsig> 
#  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 


# this is the makefile template to built phase with automake/ autoconfig

bin_PROGRAMS     = fkoempi 
fkoempi_SOURCES  = fkoempi.c fkoempi.h

#fkoe_mpi_LDADD = 



# files installed under data
pkgdata_DATA = fkoempi.par

# This is to ensure the myapp.h file is distributed in your myapp-0.1.tar.gz
# I also have a resources.qrc I need to include. The png is used in myapp.
EXTRA_DIST = 

## flags
AM_CPPFLAGS = -DLINUX -DNOGUI
AM_CFLAGS   = -W 
AM_LDFLAGS  =  

##we always use the c linker
F77LINK = $(LINK)

## CLEANFILES is a variable called from target clean: rm -f $CLEANFILES
CLEANFILES = moc_*.cpp qrc_*.cpp *.f90 *.mod
# end
