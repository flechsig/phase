#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <02 Feb 04 17:19:14 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# makefile template to built phase with automake/ autoconfig

bin_PROGRAMS = phaseopti
phase_SOURCES = phase.c ../phase/phasec.c ../phase/activate_proc.c \
          ../phase/phasegraf.c ../phase/initdatset.c \
	  ../phase/geometrypck.c ../phase/bline.c phase0.c \
	  ../phase/mirrorpck.c ../phase/pst.c \
	  ../phase/cutils.c ../phase/rtrace.c \
	  ../phase/phasegraffor.F ../phase/phasefor.for \
	  ../phase/phase_source.for \
	  ../phase/phase_integration.for \
	treiber.c optisubc.c 


phase_LDADD = $(libdir)/@LIBPHASE@


AM_CPPFLAGS = -DLINUX @CDEBUG@
AM_FFLAGS   = @FDEBUG@
AM_LDFLAGS = $(X_LIBS) -lXm -lMrm -lXt -lXext -lXp -lX11 -lm -lc -lnsl\
          -L@F77LIB@ -lf77math -lfio -lU77 -lV77 -lf77math -lg2c\
          -L@CERNLIB@ -lpawlib -lpacklib \
          -lgraflib -lgrafX11 -lpacklib -lmathlib -lkernlib -lg2c
##we use the c linker
F77LINK = $(LINK)
