#   -*-sh-*-
#   File      : /afs/psi.ch/user/f/flechsig/phase/src/configure.ac
#   Date      : <11 Dec 03 13:59:53 flechsig> 
#   Time-stamp: <05 Feb 04 14:14:25 flechsig> 
#   Author    : Uwe Flechsig, flechsig@psi.ch

#   $Source$ 
#   $Date$
#   $Revision$ 
#   $Author$ 

## it is tested with autoconf 2.57
AC_PREREQ(2.57)
AC_INIT(phase,0.5,flechsig@psi.ch)
AC_CONFIG_SRCDIR(phase/phase.c)
AC_REVISION($Revision$)
AC_CONFIG_AUX_DIR(config)
##AC_PREFIX_PROGRAM([/afs/psi.ch/user/f/flechsig/tmp])


AM_INIT_AUTOMAKE
AM_CONFIG_HEADER(phase/config.h)
## adds the maintainer option to configure
##AM_MAINTAINER_MODE

dnl Checks for programs.
AC_PROG_AWK
AC_PROG_YACC
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_CHECK_PROG([UIL],[uil],[uil])
##do LIBTOOL test after fortran stuff

## the preference of fortran compilers
AC_PROG_F77(ifc f77 g77)
AC_F77_LIBRARY_LDFLAGS
AC_SUBST(F77LIBS,${FLIBS})
##AC_MSG_NOTICE(***FLIBS*** ${FLIBS})
AC_PATH_PROG([f77path],[f77])
AC_MSG_CHECKING(for a supported compiler type)
##if test "$F77" = "g77"; then
if test "$G77" = "yes"; then
   AC_MSG_RESULT(no)
   AC_MSG_WARN([[GNU g77 will probably not work]])
else 
    if test "$F77" = "ifc"; then
     AC_MSG_RESULT(found Intel ifc)
     AC_DEFINE(HAVE_INTEL_F77,"yes","Intel f77 compiler")
     AC_SUBST([FFLAGS],"-c -vms -w95 -132")
##     f77lib=`echo $f77path | sed -e 's/bin\/f77/lib/'`
##     AC_MSG_NOTICE(we expect f77 libs at ${f77lib}) 
##     AC_SUBST(F77LIB,${f77lib})
  else
     AC_MSG_RESULT(found Absoft f77)
     AC_DEFINE(HAVE_ABSOFT_F77,"yes","Absoft f77 compiler")
     AC_SUBST([FFLAGS],"-c -f -B108 -W -s -N3 -N51 -N22 -N33")
##FFLAGS for absoft f77 compiler
##	-g generate debugging information
##	-c generate object files
##    	-f fold to lower case
##	-B108 append a single underscore to funtion names
##	-W allow > 72 columns
##	-s force all program storage to be treated as static and initialized
##	   to zero (weglassen???)
##	-N3 include record length information for sequential, unformated files
##	-N51 interpret RECL specifier as a number of 32bit words in a record
##	-C Check array boundaries (for debugging only)
##	-B111 Validate FPU stack after procedure calls (for debugging only)
##	-m0 give more error messages
##	-N22 append underscore to common block names (wichtig wegen pawc)
##      -N33 pack structures (kein alignment) 
##    f77lib=`echo $f77path | sed -e 's/bin\/f77/lib/'`

##    AC_MSG_NOTICE(we expect f77 libs at ${f77lib}) 
##    f77lib= ${f77lib} -lf77math -lfio -lU77 -lV77 -lf77math
##    AC_MSG_NOTICE(we expect f77 libs at ${f77lib})
##    AC_SUBST(F77LIB,${f77lib})
  fi
fi
## libtool init after selecting the fortran compiler
AC_PROG_LIBTOOL

## check for cernlib 
## we search for paw and expect the cernlib there
AC_MSG_NOTICE(search for CERN lib by searching for paw)
cernlib=`bin/apps_home.sh paw | sed -e 's/bin/lib/'`
AC_MSG_NOTICE((1) we search the CERN lib at ${cernlib})
## check if we find the packlib there libpacklib.a
AC_CHECK_FILE([${cernlib}/libpacklib.a], 
  AC_SUBST(CERNLIB,${cernlib}),
  [ cernlib=`echo ${cernlib} | sed -e 's|lib|apps/cern/2000/lib|'`
    AC_MSG_NOTICE((2) we search the CERN lib at ${cernlib})
    AC_CHECK_FILE([${cernlib}/libpacklib.a], 
      AC_SUBST(CERNLIB,${cernlib}), AC_MSG_ERROR(cannot find the CERN lib))
])

##AC_SUBST(CERNLIB,${cernlib})

dnl Checks for libraries.
## we skip all library tests

## deal with debugger
AC_ARG_ENABLE([debug],
AC_HELP_STRING([--enable-debug],[debugging mode, static linking]),
  if test "$enable_debug" = "yes"; then
      AC_MSG_NOTICE(*** enable debugging mode (using static libs) ***)
##      AC_DEFINE(FDEBUG, [-g -C -B111], "Debugger options f77")
##      AC_DEFINE(DEBUG, ["yes"], 
##    "debug mode with static libraries")
      AC_SUBST([FDEBUG], "-g -C -B111")
      AC_SUBST([CDEBUG], "-DDEBUG -gdwarf")
      AC_SUBST([LIBPHASE], libphase.a)
  fi,
  AC_MSG_NOTICE(*** enable production mode (using shared libs) ***)
  AC_SUBST([LIBPHASE], libphase.so)
)

# get configure date put it into header
configured=`date`
AC_DEFINE_UNQUOTED(CONFIGURED, "$configured", "configuration time")
AC_DEFINE_UNQUOTED(PREFIX, "$prefix", "the installation prefix")

dnl Checks for header files.
AC_PATH_X
AC_PATH_XTRA
AC_HEADER_STDC
AC_CHECK_HEADERS(strings.h unistd.h)
AC_SUBST([CPPFLAGS],[-I$x_includes])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_STRUCT_TM
AC_C_CONST
AC_TYPE_SIZE_T

## call own m4
##SIC_VAR_SYS_ERRLIST

dnl Checks for library functions.



AC_OUTPUT(Makefile phaselib/Makefile phase/Makefile \
          opti/Makefile extr/Makefile gui/Makefile)

## end /afs/psi.ch/user/f/flechsig/phase/src/configure.ac