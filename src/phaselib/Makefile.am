#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <05 Feb 04 15:05:28 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# makefile template to built phase with automake/ autoconfig

lib_LTLIBRARIES = libphase.la
libphase_la_SOURCES = \
wdfgmapa4.for  wdfgmapb4.for   \
wdfgmapc4.for  wdfgmapd14.for  \
wdfgmapd34.for wdfgmapd44.for  \
wdfgmapd54.for wdfgmape14.for  \
wdfgmape24.for wdfgmapf4.for   \
wdfgmapg4.for  wdfgmaph4.for   \
wdfgmapi4.for  wdfgmapk14.for  \
wdfgmapk24.for wdfgmapl4.for   \
wdfgmapm14.for wdfgmapm24.for  \
lengthab4.for  lengtha4.for    \
lengthb4.for   lengthc4.for    \
lengthd400.for lengthd401.for  \
lengthd402.for lengthd410.for  \
lengthd411.for lengthd412.for  \
lengthd42.for  lengthd43.for   \
p_m_4.for  \
zernike_nl_60.for zernike_n_60.for \
dfpl1_uf.for  dfpl2_uf.for

libphase_la_LDFLAGS = -rpath '$(libdir)' -version-info 3:12:1 

##AM_LDFLAGS = -L@F77LIB@ -lf77math -lfio -lU77 -lV77 -lf77math -lg2c
AM_FFLAGS  = @FDEBUG@
AM_LDFLAGS = @FLIBS@ 

##we use the c linker
libphase_la_LINK = $(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) $(CFLAGS) \
	$(AM_LDFLAGS) $(LDFLAGS) -o $@