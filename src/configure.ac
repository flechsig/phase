dnl   File      : /afs/psi.ch/user/f/flechsig/phase/src/configure.ac
dnl   Date      : <11 Dec 03 13:59:53 flechsig> 
dnl   Time-stamp: <21 Feb 06 14:47:38 flechsig> 
dnl   Author    : Uwe Flechsig, flechsig@psi.ch

dnl   $Source$ 
dnl   $Date$
dnl   $Revision$ 
dnl   $Author$ 

## it is tested with autoconf 2.57
AC_PREREQ(2.57)
AC_INIT(phase,1.16,uwe.flechsig@psi.ch)
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

dnl do LIBTOOL test after fortran stuff
dnl Fortran specials
dnl select the compiler
dnl !! if $F77 is set: test skipped 

dnl define  the preference of fortran compilers
AC_PROG_F77(ifc f77 g77)
AC_F77_LIBRARY_LDFLAGS
AC_SUBST(F77LIBS,${FLIBS})
##AC_MSG_NOTICE(***FLIBS*** ${FLIBS})


## UF 0602 AC_PATH_PROG([f77path],[f77])

dnl set fortran compiler specials
AC_MSG_CHECKING(for a supported compiler type)
# init all conditionals as false
AM_CONDITIONAL(WANT_IFC, [test xtrue=xfalse])  # generate a false
AM_CONDITIONAL(WANT_F77, [test xtrue=xfalse])  # generate a false
##if test "$F77" = "g77"; then
if test "$G77" = "yes"; then
   AC_MSG_RESULT(no)
   AC_MSG_WARN([[GNU g77 will probably not work (lack of VAX extensions)]])
else 
    if test "$F77" = "ifc"; then
       AC_MSG_RESULT(found Intel ifc)
       AC_DEFINE(HAVE_INTEL_F77,"yes","Intel f77 compiler")
       AC_SUBST([FFLAGS],"-vms -w95 -132 -fPIC")
       AM_CONDITIONAL(WANT_IFC, [test xtrue=xtrue])
       AC_SUBST([LIBPHASE], libphaseifc)
##     f77lib=`echo $f77path | sed -e 's/bin\/f77/lib/'`
##     AC_MSG_NOTICE(we expect f77 libs at ${f77lib}) 
##     AC_SUBST(F77LIB,${f77lib})
    else   # assume absoft compiler
	AC_MSG_RESULT(assume Absoft f77)
	AC_DEFINE(HAVE_ABSOFT_F77,"yes","Absoft f77 compiler")
	AC_SUBST([FFLAGS],"-c -f -B108 -W -s -N3 -N51 -N22 -N33")
	AM_CONDITIONAL(WANT_F77, [test xtrue=xtrue])
	AC_SUBST([LIBPHASE], libphasef77)
#	AC_DISABLE_SHARED   # warum
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


dnl check for cernlib 
## we search for paw and expect the cernlib there
AC_MSG_NOTICE(search for CERN lib by searching for paw)
cernlib=`bin/apps_home.sh paw | sed -e 's/bin/lib/'`
AC_MSG_NOTICE((1) we search the CERN lib at ${cernlib})
## check if we find the packlib there libpacklib.a
AC_CHECK_FILE([${cernlib}/libpacklib.a], 
  AC_SUBST(CERNLIB,${cernlib}),
  [ cernlib=`echo ${cernlib} | sed -e 's|lib|apps/cern/2003/lib|'`
    AC_MSG_NOTICE((2) we search the CERN lib at ${cernlib})
    AC_CHECK_FILE([${cernlib}/libpacklib.a], 
      AC_SUBST(CERNLIB,${cernlib}), 
      AC_MSG_ERROR(cannot find the CERN lib))
])

##AC_SUBST(CERNLIB,${cernlib})

dnl Checks for libraries.


## deal with debugger
AC_ARG_ENABLE([debug],
AC_HELP_STRING([--enable-debug],[debugging code, static linking]),
  if test "$enable_debug" = "yes"; then
      AC_MSG_NOTICE(*** produce debugging code ***)
##      AC_DEFINE(FDEBUG, [-g -C -B111], "Debugger options f77")
##      AC_DEFINE(DEBUG, ["yes"], 
##    "debug mode with static libraries")
#      AC_ENABLE_STATIC
#      AC_DISABLE_SHARED
      AC_SUBST([FDEBUG], "-g -C -B111")
      AC_SUBST([CDEBUG], "-DDEBUG -gdwarf-2 -g3")
      AC_SUBST([CFLAGS], "")
      libextension="a"
#      AC_SUBST([LIBPHASE], "libphasef77.a")
  fi,
## AC_MSG_NOTICE(*** produce production code (using shared libs) ***)
## AC_SUBST([LIBPHASE], libphase.so)
## shared libs are not possible with our absoft compiler
[
  AC_MSG_NOTICE(***** produce production code *****)
  libextension="so"
]
#  AC_SUBST([LIBPHASE], [${LIBPHASE}.so])
)

dnl libtool init after selecting the fortran compiler
AC_PROG_LIBTOOL


## Optimization with full ray trace
AC_ARG_ENABLE([full_rt],
    AC_HELP_STRING([--disable-full-rt],
	[disable optimization in full ray trace mode]),
    [AC_MSG_NOTICE(***** optimization without full ray trace ***)],
    [AC_MSG_NOTICE(***** optimization in full ray trace mode *****)
    AC_DEFINE_UNQUOTED(WITH_FULL_RT, "yes", 
    "optimization in full ray trace mode")]
)

## expire option
AC_ARG_ENABLE([EXPIRE],
    AC_HELP_STRING([--enable-EXPIRE=YYYYMMDD],[expiring date of the code]),
    AC_DEFINE_UNQUOTED([EXPIRE], $enableval, "expiring date"))

# get configure date put it into header
configured=`date`
AC_DEFINE_UNQUOTED(CONFIGURED, "$configured", "configuration time")
if test "$prefix" = "NONE"; then
  AC_MSG_NOTICE(***** prefix: /usr/local ****************** )
  prefixlib="/usr/local/lib"
  AC_DEFINE_UNQUOTED(PREFIX, "/usr/local", "the installation prefix")
else
  AC_MSG_NOTICE(***** prefix: $prefix ****************** )
  prefixlib=${prefix}/lib
  AC_DEFINE_UNQUOTED(PREFIX, "$prefix", "the installation prefix")
fi

dnl Checks for header files.
AC_PATH_X
AC_PATH_XTRA
AC_HEADER_STDC
AC_CHECK_HEADERS(strings.h unistd.h)
AC_CHECK_HEADERS([Xm/Text.h X11/Xlib.h],
    [
# AC_MSG_NOTICE(Motif headers found )
],
    [AC_MSG_WARN([[headers missing - Motif installation is probably not OK]])])
AC_SUBST([CPPFLAGS],[-I$x_includes])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_STRUCT_TM
AC_C_CONST
AC_TYPE_SIZE_T

## call own m4
##SIC_VAR_SYS_ERRLIST

dnl Checks for library functions.
dnl check if a phaselib is installed
AC_MSG_NOTICE(search if library ${LIBPHASE} is installed)
AC_CHECK_FILE(["${prefixlib}/${LIBPHASE}.${libextension}"], 
   [have_libphase="yes"], [have_libphase="no"])
#   AC_DEFINE_UNQUOTED([HAVE_LIBPHASE],"yes","installed phaselib"),
#   AC_DEFINE_UNQUOTED([HAVE_LIBPHASE],"no","installed phaselib"))

AC_OUTPUT(Makefile phaselib/Makefile phase/Makefile \
          opti/Makefile extr/Makefile gui/Makefile)

echo ""
echo "===================================================================="
### result of phaselib 
if test "x$have_libphase" = "xyes"; then
   echo "we found an installed PhaseLibrary: ${LIBPHASE} ==> good"
   echo ""
   echo "if make starts an unwanted rebuild of the library you can interrupt"
   echo "and the individual subdirectories and call make and make install"
   echo "separately"
else
   echo "The required PhaseLibrary ${LIBPHASE} is not installed"
   echo "we recommend (not required) to built the library in a separate step"
   echo "(!! compilation of phaselib may take several hours !!)"
   echo "cd ./phaselib"
   echo "make"
   echo "su -c \"make install\""
   echo "and after the library is installed call make in the main directory"
   echo "cd .."
   echo "make"
fi
echo "===================================================================="
#[ AC_MSG_NOTICE([=====> ${LIBPHASE} not installed !!!])
#  AC_MSG_NOTICE([=====> goto ./phaselib and do "make install"])
#  AC_MSG_ERROR([=====> end. ])
#]
## end /afs/psi.ch/user/f/flechsig/phase/src/configure.ac
