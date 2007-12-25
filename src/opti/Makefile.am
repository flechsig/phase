#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <25 Dec 07 10:54:23 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# makefile template to built phase with automake/ autoconfig

bin_PROGRAMS            = phaseopti
##lib_LTLIBRARIES         = libphaseopti.la
phaseopti_SOURCES       = phaseopti.c phaseopti.h \
          ../phase/phasec.c       ../phase/activate_proc.c \
          ../phase/phasegraf.c    ../phase/initdatset.c \
	  ../phase/geometrypck.c  ../phase/bline.c ../phase/phase0.c \
	  ../phase/mirrorpck.c    ../phase/pst.c \
	  ../phase/cutils.c       ../phase/rtrace.c \
	  ../phase/phasegraffor.F ../phase/phasefor.F \
	  ../phase/phase_source.F ../phase/xmalloc.c ../phase/error.c \
	  ../phase/xstrdup.c      ../phase/ctype.c \
	  ../phase/phase_integration.F \
	  treiber.F optisubc.c cost.F \
	  ../phase/misali.F  ../phase/misali1.f ../phase/misali2.f \
          ../phase/misali3.f ../phase/misali4.f
##libphaseopti_la_SOURCES = 

phaseopti_LDADD = $(libdir)/@LIBPHASE@
##libphaseopti_la_LDFLAGS = -rpath '$(libdir)' -version-info 3:13:1 

AM_CPPFLAGS = -DLINUX 
##AM_FFLAGS   = @FDEBUG@
AM_LDFLAGS = $(X_LIBS) -lXm -lMrm -lXt -lXext -lXp -lX11 -lm -lc -lnsl\
          @FLIBS@\
          -L@CERNLIB@ -lpawlib -lpacklib \
          -lgraflib -lgrafX11 -lpacklib -lmathlib -lkernlib @CFORLIB@
##we use the c linker
F77LINK = $(LINK)
##we use the c linker
##libphaseopti_la_LINK = $(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) $(CFLAGS) \
##	$(AM_LDFLAGS) $(LDFLAGS) -o $@




