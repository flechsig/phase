#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <29 Jul 11 09:37:48 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# makefile template to built phase with automake/ autoconfig

bin_PROGRAMS       = phaseopti

phaseopti_SOURCES = phaseopti.c  ../phase/phasefor.F \
           ../phase/initdatset.c  \
	  ../phase/geometrypck.c  ../phase/bline.c   \
	  ../phase/mirrorpck.c     \
	  ../phase/cutils.c       ../phase/rtrace.c \
	  ../phase/xmalloc.c      ../phase/error.c \
	  ../phase/xstrdup.c      ../phase/ctype.c \
	  treiber.F optisubc.c cost.F \
	  ../phase/misali.F  ../phase/misali1.f ../phase/misali2.f \
          ../phase/misali3.f ../phase/misali4.f \
	  ../phase/misali_8.f ../phase/misali1_8.f ../phase/misali2_8.f ../phase/misali3_8.f ../phase/misali4_8.f \
          ../phase/drift_8.for ../phase/fgmapidp_8.F ../phase/get_partial_etc.for ../phase/rekursiv_2d_8.for \
          ../phase/replace_6v4v.for ../phase/replace_wl_in_ypzp.for ../phase/replace_wl_in_u.for \
          ../phase/taylor-ops.for ../phase/q0_q1_q2_8.for ../phase/make_matrix_8.for ../phase/fdet_8.for

##libphaseopti_la_SOURCES = 

phaseopti_LDADD = $(libdir)/@LIBPHASE@
##libphaseopti_la_LDFLAGS = -rpath '$(libdir)' -version-info 3:13:1 

AM_CPPFLAGS = -DLINUX 
##AM_FFLAGS   = @FDEBUG@
AM_LDFLAGS = @MOTIFLIBS@ $(X_LIBS) -lXm -lMrm -lXt -lXext -lXp -lX11 -lm -lc -lnsl\
          @FLIBS@\
          -L@CERNLIB@ -lpawlib -lpacklib \
          -lgraflib -lgrafX11 -lpacklib -lmathlib -lkernlib @CFORLIB@
##we use the c linker
F77LINK = $(LINK)
##we use the c linker
##libphaseopti_la_LINK = $(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) $(CFLAGS) \
##	$(AM_LDFLAGS) $(LDFLAGS) -o $@




