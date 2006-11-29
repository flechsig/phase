#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <14 Mar 06 12:22:25 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# makefile template to built phase with automake/ autoconfig and libtool
# we built conditional libraries for the different fortran compilers i.e  
# installed libraries can coexists but- we keep only one set of object files!
# i.e. to built a library for a new compiler the objects have to be cleaned

## put all sources which are independent of the fortran compiler
COMMONSOURCES= \
	error.c xmalloc.c xstrdup.c cutils.c \
        phaBLBuild.c \
	phaBLReadFromFile.c phaBLInit.c \
        DefGeometryC.c DefMirrorC.c MakeMapandMatrix.c SetFilePos.c \
        SetDeltaLambda.c rrtrace.c \
        misali.F misali1.f misali2.f misali3.f misali4.f \
	phasefor.F phase_integration.F phase_source.F \
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
dfpl1_uf.F  dfpl2_uf.F \
phabasestructs.h phabasedefs.h idl_export.h

## end common sources

## we use an AM_CONDITIONAL to switch library names for different compilers
lib_LTLIBRARIES = 
if WANT_ABSOFT
   lib_LTLIBRARIES += libphabaseabsoft.la
endif
if WANT_IFC
   lib_LTLIBRARIES += libphabaseifc.la
endif
if WANT_PORTLAND
   lib_LTLIBRARIES += libphabaseportland.la
endif

libphabaseabsoft_la_SOURCES   = $(COMMONSOURCES)
libphabaseportland_la_SOURCES = $(COMMONSOURCES)
libphabaseifc_la_SOURCES      = $(COMMONSOURCES)

## libtool versions (optional)
## libphaseabsoft_la_LDFLAGS =  -version-info 3:12:1 
## libphaseifc_la_LDFLAGS =  -version-info 3:12:1

AM_LDFLAGS = @FLIBS@ 

## we want the c- Linker and not the fortran linker
libphabaseabsoft_la_LINK = $(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) \
	$(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

libphabaseifc_la_LINK = $(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) \
	$(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

libphabaseportland_la_LINK = $(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) \
	$(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

# end