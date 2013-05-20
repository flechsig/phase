#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <2013-05-20 20:25:55 flechsig> 
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
	../phase/q0_q1_q2_8.for ../phase/rekursiv_2d_8.F \
	../phase/get_partial_etc.for ../phase/replace_6v4v.for \
	../phase/replace_wl_in_u.for ../phase/replace_wl_in_ypzp.for\
	../phase/drift_8.for    ../phase/elli_8.F \
	../phaselib/misali_8.f  ../phaselib/misali4_8.f \
	../phaselib/misali3_8.f ../phaselib/misali2_8.f \
	../phaselib/misali1_8.f ../phase/phasefor.F \
	../phase/error.c        ../phase/xstrdup.c \
	../phase/cutils.c       ../phase/rtrace.c \
	../phase/bline.c


AM_CPPFLAGS = -DLINUX -DEXTR -DNOGUI
##AM_FFLAGS   = @FDEBUG@
AM_LDFLAGS =  @FLIBS@ @CFORLIB@

##we use the c linker
F77LINK = $(LINK)
