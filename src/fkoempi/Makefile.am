#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phasesrv/Makefile.am
#  Date      : <17 May 13 17:36:23 flechsig> 
#  Time-stamp: <01 Apr 14 09:00:22 flechsig> 
#  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 


# this is the makefile template to built phase with automake/ autoconfig

bin_PROGRAMS      = fkoe_mpi spa-check
fkoe_mpi_SOURCES  = fkoe_mpi.c
spa_check_SOURCES = spa-check.F
#fkoe_mpi_LDADD = 



# files installed under data
pkgdata_DATA = 

# This is to ensure the myapp.h file is distributed in your myapp-0.1.tar.gz
# I also have a resources.qrc I need to include. The png is used in myapp.
EXTRA_DIST = 

## flags
AM_CPPFLAGS = -DLINUX -DNOGUI
AM_CFLAGS   =  -W 

##-I@HDF5@/include \
##		-I@HDF5I@ \
###-pipe -g -D_REENTRANT -Wall -W 
#####-I@QT@/mkspecs/linux-g++       
###AM_LDFLAGS = $(X_LIBS) -lXm -lMrm -lXt -lXext -lXp -lX11 -lm -lc -lnsl\
#####	     @FLIBS@    @CFORLIB@

AM_LDFLAGS =  @HDF5_LDFLAGS@ @FLIBS@   ##-L@HDF5@/lib -lhdf5

##we always use the c linker
F77LINK = $(LINK)



# This line ensures that generated moc_*.cpp and resources.cpp files are
# deleted when we make clean.
##CLEANFILES = $(filter moc_%.cpp,$(phaseqt_SOURCES)) 
## CLEANFILES is a variable called from target clean: rm -f $CLEANFILES
CLEANFILES = moc_*.cpp qrc_*.cpp *.f90 *.mod
# end
