dnl ####################### -*- Mode: M4 -*- ###########################
#  File      : /afs/psi.ch/user/f/flechsig/phase/src/config/mdl_dl.m4
#  Date      : <27 Sep 12 17:53:36 flechsig> 
#  Time-stamp: <27 Sep 12 17:54:19 flechsig> 


#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

dnl mdl_dl.m4 -- 
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

dnl MDL_DL
dnl ------
dnl 
dnl Determine the appropriate libraries for dlopen, et. al., and set the
dnl output variable LIBADD_DL to these libraries.
dnl
AC_DEFUN([MDL_DL],
[
  test_dlerror=no
  LIBADD_DL=
  AC_CHECK_FUNCS(dlopen,
                 [AC_DEFINE(HAVE_LIBDL,
                            1,
                            [Define if you have the libdl library or equivalent.])
                   test_dlerror=yes],
                 [AC_CHECK_LIB(dl,
                               dlopen,
                               [AC_DEFINE(HAVE_LIBDL,
                                          1,
			                  [Define if you have the libdl library.])
    	                         LIBADD_DL="-ldl"
                                 test_dlerror=yes],
                               [AC_CHECK_LIB(dld,
                                             dld_link,
                                             [AC_DEFINE(HAVE_DLD,
                                                        1,
			                                [Define if you have the GNU dld library.])
			                       LIBADD_DL="-ldld"],
                                             [AC_CHECK_FUNCS(shl_load,
                                                             [AC_DEFINE(HAVE_SHL_LOAD,
                                                                        1,
                    				                        [Define if you have the shl_load function.])])])])])

  AC_SUBST(LIBADD_DL)

  if test x"$test_dlerror" = xyes; then
    LIBS_SAVE="$LIBS"
    LIBS="$LIBS $LIBADD_DL"
    AC_CHECK_FUNCS(dlerror)
    LIBS="$LIBS_SAVE"
  fi
])
