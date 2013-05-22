#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <22 May 13 10:41:34 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# makefile template to built phase with automake/ autoconfig

bin_PROGRAMS       = phaseopti opti_root_example

phaseopti_SOURCES = phaseopti.cpp phaseopti.h \
	mycost.cpp mycost.h \
	optisubc.c optisubc.h \
	../phase/xmalloc.c \
	../phase/fgmapidp_8.F   ../phase/make_matrix_8.for \
	../phase/fdet_8.for     ../phase/taylor-ops.for\
	../phase/q0_q1_q2_8.for ../phase/rekursiv_2d_8.F \
	../phase/get_partial_etc.for ../phase/replace_6v4v.F \
	../phase/replace_wl_in_u.F ../phase/replace_wl_in_ypzp.F \
	../phase/drift_8.for    ../phase/elli_8.F \
	../phaselib/misali_8.f  ../phaselib/misali4_8.f \
	../phaselib/misali3_8.f ../phaselib/misali2_8.f \
	../phaselib/misali1_8.f ../phase/phasefor.F \
	../phase/error.c        ../phase/xstrdup.c \
	../phase/cutils.c       ../phase/rtrace.c \
	../phase/bline.c        

opti_root_example_SOURCES = opti_root_example.cpp opti_root_example.h
## flags
AM_CPPFLAGS = -DLINUX -DOPTI -DNOGUI
AM_CXXFLAGS=  $(ROOT_CFLAGS) -I. -I../phase -W
AM_LDFLAGS =  $(ROOT_LIBS) -lMinuit @FLIBS@ @CFORLIB@

## we use the c linker
F77LINK = $(LINK)
## end




