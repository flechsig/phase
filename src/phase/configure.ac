-*-sh-*-
#   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/configure.ac
#   Date      : <26 Nov 03 15:50:42 flechsig> 
#   Time-stamp: <26 Nov 03 15:51:10 flechsig> 
#   Author    : Uwe Flechsig, flechsig@psi.ch

#   $Source$ 
#   $Date$
#   $Revision$ 
#   $Author$ 

dnl Process this file with autoconf to produce a configure script.
AC_INIT(activate_proc.c)

dnl Checks for programs.
AC_PROG_AWK
AC_PROG_YACC
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

dnl Checks for libraries.
dnl Replace `main' with a function in -lMrm:
AC_CHECK_LIB(Mrm, main)
dnl Replace `main' with a function in -lU77:
AC_CHECK_LIB(U77, main)
dnl Replace `main' with a function in -lV77:
AC_CHECK_LIB(V77, main)
dnl Replace `main' with a function in -lX11:
AC_CHECK_LIB(X11, main)
dnl Replace `main' with a function in -lXext:
AC_CHECK_LIB(Xext, main)
dnl Replace `main' with a function in -lXm:
AC_CHECK_LIB(Xm, main)
dnl Replace `main' with a function in -lXp:
AC_CHECK_LIB(Xp, main)
dnl Replace `main' with a function in -lXt:
AC_CHECK_LIB(Xt, main)
dnl Replace `main' with a function in -lc:
AC_CHECK_LIB(c, main)
dnl Replace `main' with a function in -lf77math:
AC_CHECK_LIB(f77math, main)
dnl Replace `main' with a function in -lfio:
AC_CHECK_LIB(fio, main)
dnl Replace `main' with a function in -lg2c:
AC_CHECK_LIB(g2c, main)
dnl Replace `main' with a function in -lgrafX11:
AC_CHECK_LIB(grafX11, main)
dnl Replace `main' with a function in -lgraflib:
AC_CHECK_LIB(graflib, main)
dnl Replace `main' with a function in -lkernlib:
AC_CHECK_LIB(kernlib, main)
dnl Replace `main' with a function in -lm:
AC_CHECK_LIB(m, main)
dnl Replace `main' with a function in -lmathlib:
AC_CHECK_LIB(mathlib, main)
dnl Replace `main' with a function in -lnsl:
AC_CHECK_LIB(nsl, main)
dnl Replace `main' with a function in -lpacklib:
AC_CHECK_LIB(packlib, main)
dnl Replace `main' with a function in -lpawlib:
AC_CHECK_LIB(pawlib, main)

dnl Checks for header files.
AC_PATH_X
AC_HEADER_STDC

dnl Checks for typedefs, structures, and compiler characteristics.
AC_STRUCT_TM

dnl Checks for library functions.

AC_OUTPUT(Makefile)
