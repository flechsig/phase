#   -*-sh-*-
#   File      : /afs/psi.ch/user/f/flechsig/phase/src/configure.ac
#   Date      : <11 Dec 03 13:59:53 flechsig> 
#   Time-stamp: <11 Dec 03 14:02:06 flechsig> 
#   Author    : Uwe Flechsig, flechsig@psi.ch

#   $Source$ 
#   $Date$
#   $Revision$ 
#   $Author$ 

## it is tested with autoconf 2.57
AC_PREREQ(2.57)
AC_INIT(phase,0.5,flechsig@psi.ch)
AC_CONFIG_SRCDIR(src/ccut.c)
AC_REVISION($Revision$)
AC_CONFIG_AUX_DIR(config)
#AC_PREFIX_PROGRAM([/afs/psi.ch/user/f/flechsig/tmp])

AM_INIT_AUTOMAKE
AM_CONFIG_HEADER(phase/config.h)
## adds the maintainer option to configure
AM_MAINTAINER_MODE

dnl Checks for programs.
AC_PROG_AWK
AC_PROG_YACC
AC_PROG_CC
AC_PROG_RANLIB
AC_PROG_INSTALL
AC_PROG_LN_S

## the preference of fortran compilers
AC_PROG_F77(f90com f77 g77)

AC_MSG_CHECKING(for a supported compiler type)
if test "$G77" = "yes"; then
   AC_MSG_RESULT(no)
   AC_MSG_WARN([[GNU g77 will probably not work]])
else 
  if test "$F77" = "f90com"; then
     AC_MSG_RESULT(found Intel f90com)
     AC_DEFINE(HAVE_INTEL_F77,"yes","Intel f77 compiler)
     AC_SUBST([FFLAGS],"-c -vms -w95 -132")
  else
     AC_MSG_RESULT(found Absoft f77)
     AC_DEFINE(HAVE_ABSOFT_F77,"yes","Absoft f77 cpmpiler)
     AC_SUBST([FFLAGS],"-g -C -B111 -c -f -B108 -W -s -N3 -N51 -N22 -N33")
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

dnl Checks for libraries.
dnl Replace `main' with a function in -lc:
AC_CHECK_LIB(c, main)
dnl Replace `main' with a function in -lg2c:
AC_CHECK_LIB(g2c, main)
dnl Replace `main' with a function in -lm:
AC_CHECK_LIB(m, main)
dnl Replace `main' with a function in -lnsl:
AC_CHECK_LIB(nsl, main)

dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(strings.h unistd.h)

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_SIZE_T

## call own m4
##SIC_VAR_SYS_ERRLIST



dnl Checks for library functions.

AC_OUTPUT(Makefile phase/Makefile)

## end /afs/psi.ch/user/f/flechsig/phase/src/configure.ac