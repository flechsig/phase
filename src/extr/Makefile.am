#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <10 Feb 04 16:50:04 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# makefile template to built phase with automake/ autoconfig

bin_PROGRAMS            = phaseextract

phaseextract_SOURCES    = phaseextract.c cost.F \
          ../phase/phasec.c      ../phase/activate_proc.c \
          ../phase/phasegraf.c   ../phase/initdatset.c \
	  ../phase/geometrypck.c ../phase/bline.c ../phase/phase0.c \
	  ../phase/mirrorpck.c   ../phase/pst.c \
	  ../phase/cutils.c      ../phase/rtrace.c \
	  ../phase/phasegraffor.F ../phase/phasefor.F \
	  ../phase/phase_source.F ../phase/xmalloc.c ../phase/error.c ../phase/xstrdup.c \
	  ../phase/phase_integration.F \
	 ../opti/treiber.F ../opti/optisubc.c



phaseextract_LDADD = $(libdir)/@LIBPHASE@
 

AM_CPPFLAGS = -DLINUX @CDEBUG@
AM_FFLAGS   = @FDEBUG@
AM_LDFLAGS = $(X_LIBS) -lXm -lMrm -lXt -lXext -lXp -lX11 -lm -lc -lnsl\
          @FLIBS@ \
          -L@CERNLIB@ -lpawlib -lpacklib \
          -lgraflib -lgrafX11 -lpacklib -lmathlib -lkernlib -lg2c
##we use the c linker
F77LINK = $(LINK)
