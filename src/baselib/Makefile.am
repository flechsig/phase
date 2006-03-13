#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/Makefile.am
#  Date      : <23 Jan 04 09:33:24 flechsig> 
#  Time-stamp: <13 Mar 06 08:23:12 flechsig> 
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
	phaBLReadFromFile.c phaBLInit.c SetFilePos.c
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