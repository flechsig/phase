 # File      : /afs/psi.ch/user/f/flechsig/phase/src/fkoe/Makefile.am
 # Date      : <01 Apr 14 09:25:08 flechsig> 
 # Time-stamp: <01 Apr 14 09:34:50 flechsig> 
 # Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 # $Source$ 
 # $Date$
 # $Revision$ 
 # $Author$ 

# this is the makefile template to built phase with automake/ autoconfig

bin_PROGRAMS      = spa_check
spa_check_SOURCES = spa_check.F spa-check.cmn
#fkoe_mpi_LDADD = 

# files installed under data
pkgdata_DATA = 

# This is to ensure the myapp.h file is distributed in your myapp-0.1.tar.gz
# I also have a resources.qrc I need to include. The png is used in myapp.
EXTRA_DIST = fkoe.par gauss_beam.par mk_surf.par 

## CLEANFILES is a variable called from target clean: rm -f $CLEANFILES
CLEANFILES = moc_*.cpp qrc_*.cpp *.f90 *.mod
# end
