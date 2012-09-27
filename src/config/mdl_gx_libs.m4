dnl ####################### -*- Mode: M4 -*- ###########################
#  File      : /afs/psi.ch/user/f/flechsig/phase/src/config/mdl_gx_libs.m4
#  Date      : <27 Sep 12 17:52:45 flechsig> 
#  Time-stamp: <27 Sep 12 17:53:12 flechsig> 


#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

dnl mdl_gx_libs.m4 -- 
dnl 
dnl Copyright (C) 1999 Matthew D. Langston <langston@SLAC.Stanford.EDU>
dnl
dnl This file is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2 of the License, or
dnl (at your option) any later version.
dnl
dnl This file is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this file; if not, write to:
dnl
dnl   Free Software Foundation, Inc.
dnl   Suite 330
dnl   59 Temple Place
dnl   Boston, MA 02111-1307, USA.
dnl ####################################################################

dnl MDL_GX_LIBS
dnl -----------
dnl 
dnl Determine the graphics libraries necessary to link ROOT programs.
dnl
AC_DEFUN([MDL_GX_LIBS],
[
dnl Checks for X11.
  AC_PATH_X
  AC_PATH_XTRA

dnl Add everything we need to compile and link X programs to CPPFLAGS
dnl and GX_LIBS.
  CPPFLAGS="$CPPFLAGS $X_CFLAGS"
  GX_LIBS="$X_PRE_LIBS $X_LIBS -lX11 -lXext $X_EXTRA_LIBS"

dnl Check for the Xpm library.
  AC_CHECK_LIB(Xpm, XpmAttributesSize, mdl_cv_have_libxpm=yes, mdl_cv_have_libxpm=no, $GX_LIBS)
  if test x"$mdl_cv_have_libxpm" = xno; then
    AC_MSG_ERROR([The Xpm library is required to build ${PACKAGE}.])
  fi

dnl If we have gotten this far, then we must have found the Xpm library.
dnl Therefore, add it to GX_LIBS.
  GX_LIBS="$GX_LIBS -lXpm"
  AC_SUBST(GX_LIBS)
])
