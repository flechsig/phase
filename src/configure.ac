dnl   File      : /afs/psi.ch/user/f/flechsig/phase/src/configure.ac
dnl   Date      : <11 Dec 03 13:59:53 flechsig> 
dnl   Time-stamp: <27 Feb 06 15:31:20 flechsig> 
dnl   Author    : Uwe Flechsig, flechsig@psi.ch

dnl   $Source$ 
dnl   $Date$
dnl   $Revision$ 
dnl   $Author$ 

dnl we need autoconf 2.57 for bootstrapping
AC_PREREQ(2.57)
AC_INIT(phase,1.16,uwe.flechsig@psi.ch)
AC_CONFIG_SRCDIR(phase/phase.c)
AC_REVISION($Revision$)
AC_CONFIG_AUX_DIR(config)

AM_INIT_AUTOMAKE
AM_CONFIG_HEADER(phase/config.h)

dnl maintainer option i.e. make does not bootstrap
##AM_MAINTAINER_MODE

dnl Checks for programs.
AC_PROG_AWK
AC_PROG_YACC
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_CHECK_PROG([UIL],[uil],[uil])

dnl do LIBTOOL test after fortran stuff

dnl define the search preference of fortran compilers
dnl ************** FORTRAN specials *********************
dnl !!the environment variable F77 overwrites this test!!
AC_PROG_F77(ifc f77 g77)
AC_F77_LIBRARY_LDFLAGS
AC_SUBST(F77LIBS,${FLIBS})

dnl set fortran compiler specials
AC_MSG_CHECKING(for a supported compiler type)
# init conditionals as false
#AM_CONDITIONAL(WANT_IFC,    test xtrue=xfalse)  # generate a false
#AM_CONDITIONAL(WANT_ABSOFT, test xtrue=xfalse)  # generate a false

if test "$G77" = "yes"; then
   AC_MSG_RESULT(no)
   AC_MSG_ERROR([[GNU Fortran g77 will not work (lack of VAX extensions)]])
else 
   if test "$F77" = "ifc"; then   # intel compiler
      AC_MSG_RESULT(found Intel ifc)
      AC_DEFINE(HAVE_INTEL_F77,"yes","Intel f77 compiler")
      AC_SUBST([FFLAGS],"-vms -w95 -132")
#      AM_CONDITIONAL(WANT_IFC, test xtrue=xtrue)
      libphasename=libphaseifc
      wantifc=true
      wantabsoft=false
   else   # assume absoft compiler as default
      AC_MSG_RESULT(assume Absoft f77)
      AC_DEFINE(HAVE_ABSOFT_F77,"yes","Absoft f77 compiler")
#      AM_CONDITIONAL(WANT_ABSOFT, test xtrue=xtrue)
      libphasename=libphaseabsoft
      wantifc=false
      wantabsoft=true
      AC_SUBST([FFLAGS],"-f -B108 -W -s -N3 -N51 -N22 -N33")

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

  fi
fi

AM_CONDITIONAL([WANTIFC],    [test x"$wantifc" = xtrue])  
AM_CONDITIONAL([WANTABSOFT], [test x"$wantabsoft" = xtrue])  
dnl end fortran specials

dnl ****************** CERN **************************
dnl check for cernlib 
AC_ARG_WITH([cernlib],
  AC_HELP_STRING([--with-cernlib=DIR],[path to CERN libraries]),
  [ echo "cernlib given as option: ${with_cernlib}"
    cernlib=${with_cernlib}
    dnl check if we find the libpacklib.a there
    AC_CHECK_FILE([${cernlib}/libpacklib.a], 
      AC_SUBST(CERNLIB,${cernlib}), 
      AC_MSG_ERROR(cannot find a CERN lib))],
  [ dnl cernlib is not given- we do a search
    dnl we search for paw and expect the cernlib there
    AC_MSG_NOTICE(search for CERN lib by searching for paw)
    dnl call a script which determines the absolute path of a 
    dnl program without symbolic links
    cernlib=`bin/apps_home.sh paw | sed -e 's/bin/lib/'`
    AC_MSG_NOTICE((1) we search the CERN lib at ${cernlib})
    dnl check if we find the libpacklib.a there 
    AC_CHECK_FILE([${cernlib}/libpacklib.a], 
    AC_SUBST(CERNLIB,${cernlib}),
    [ cernlib=`echo ${cernlib} | sed -e 's|lib|apps/cern/2003/lib|'`
      AC_MSG_NOTICE((2) we search the CERN lib at ${cernlib})
      AC_CHECK_FILE([${cernlib}/libpacklib.a], 
      AC_SUBST(CERNLIB,${cernlib}), 
      AC_MSG_ERROR(cannot find a CERN lib))])])
dnl end cern lib


dnl deal with the debugger 
AC_ARG_ENABLE([debug],
  AC_HELP_STRING([--enable-debug],[debugging code, static linking]),
    if test "$enable_debug" = "yes"; then
       AC_MSG_NOTICE(*** produce debugging code ***)
       if test "$F77" = "ifc"; then
	  FFLAGS="-DDEBUG -g ${FFLAGS}"
       else
          FFLAGS="-DDEBUG -g -C -B111 ${FFLAGS}"
       fi
       CFLAGS="-DDEBUG -gdwarf-2 -g3 ${CFLAGS}"
       libextension="a"
    fi,
    [AC_MSG_NOTICE(***** produce production code *****)
    enable_debug="no"
    libextension="so"])
dnl end debugger

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

dnl get configure date put it into header
configured=`date`
AC_DEFINE_UNQUOTED(CONFIGURED, "$configured", "configuration time")

dnl deal with the prefix
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
   [AC_MSG_NOTICE(Motif headers found)],
   [AC_MSG_WARN([[headers missing - Motif installation is probably not OK]])])

dnl add include path to motif headers 
AC_SUBST([CPPFLAGS],[-I$x_includes])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_STRUCT_TM
AC_C_CONST
AC_TYPE_SIZE_T

dnl ************ PHASELIB *******************************
dnl check if a phaselib is installed
AC_SUBST([LIBPHASE], "${libphasename}.${libextension}")
AC_MSG_NOTICE(search if library ${LIBPHASE} is installed)
AC_CHECK_FILE(["${prefixlib}/${LIBPHASE}"], 
   [have_libphase="yes"], [have_libphase="no"])

dnl output files 
AC_OUTPUT(Makefile phaselib/Makefile phase/Makefile \
          opti/Makefile extr/Makefile gui/Makefile)

dnl print some messages
echo ""
echo "===================================================================="
### result of phaselib 
if test "x$have_libphase" = "xyes"; then
   echo "we found an installed PhaseLibrary: ${LIBPHASE} ==> good"
   echo ""
   echo "if make starts an unwanted rebuild of the library you can interrupt"
   echo "and goto the individual subdirectories and call make and make install"
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
echo "summary: "
echo "fortran compiler: ${F77}"
echo "phaselib        : ${LIBPHASE}"
echo "debug code      : ${enable_debug}"
echo "cernlib:        : ${cernlib}"
echo "prefix:         : ${prefix}"
## end /afs/psi.ch/user/f/flechsig/phase/src/configure.ac
