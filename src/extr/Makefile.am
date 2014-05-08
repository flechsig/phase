#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <08 May 14 10:03:17 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# makefile template to built phase with automake/ autoconfig

bin_PROGRAMS            = phaseextract

phaseextract_SOURCES    = phaseextract.c cost.c cost.h \
	../opti/optisubc.c ../opti/optisubc.h \
	../phase/xmalloc.c \
	../phase/fgmapidp_8.F   ../phase/make_matrix_8.for \
	../phase/fdet_8.for     ../phase/taylor-ops.for\
	../phase/rekursiv_2d_8.F \
	../phase/get_partial_etc.for ../phase/replace_6v4v.F \
	../phase/replace_wl_in_u.F ../phase/replace_wl_in_ypzp.F \
	../phase/drift_8.for    ../phase/elli_8.F \
	../phase/phasefor.F \
	../phase/error.c        ../phase/xstrdup.c \
	../phase/cutils.c       ../phase/rtrace.c \
	../phase/bline.c ../phase/myhdf5.c ../phase/myhdf5.h \
	../phase/reflectivity.c ../phase/reflectivity.h \
	../phase/heighterror.c ../phase/heighterror.h

phaseextract_LDADD = $(libdir)/@LIBPHASE@

AM_CPPFLAGS = -DLINUX -DEXTR -DNOGUI
AM_CFLAGS   =  @HDF5_CFLAGS@ -I../phase -W 
##AM_FFLAGS   = @FDEBUG@
AM_LDFLAGS =  @HDF5_LDFLAGS@ @FLIBS@ @CFORLIB@

##we use the c linker
F77LINK = $(LINK)
