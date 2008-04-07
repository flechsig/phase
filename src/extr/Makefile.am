#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <07 Apr 08 15:45:00 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# makefile template to built phase with automake/ autoconfig

bin_PROGRAMS            = phaseextract

phaseextract_SOURCES    = phaseextract.c cost.F \
                ../phase/initdatset.c \
	  	../phase/geometrypck.c ../phase/bline.c  \
	  	../phase/mirrorpck.c      \
	  	../phase/cutils.c ../phase/rtrace.c \
	   	../phase/phasefor.F \
	   	../phase/xmalloc.c \
	  	../phase/error.c ../phase/xstrdup.c \
		../phase/ctype.c \
	  	../opti/treiber.F ../opti/optisubc.c \
	  	../phase/misali.F ../phase/misali1.f ../phase/misali2.f \
		../phase/misali3.f ../phase/misali4.f

phaseextract_LDADD = $(libdir)/@LIBPHASE@

AM_CPPFLAGS = -DLINUX ##@CDEBUG@
##AM_FFLAGS   = @FDEBUG@
AM_LDFLAGS = @MOTIFLIBS@ $(X_LIBS) -lXm -lMrm -lXt -lXext -lXp -lX11 -lm -lc -lnsl\
          @FLIBS@ \
         -L@CERNLIB@ -lpawlib -lpacklib \
         -lgraflib -lgrafX11 -lpacklib -lmathlib -lkernlib @CFORLIB@

##we use the c linker
F77LINK = $(LINK)
