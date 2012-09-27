dnl ####################### -*- Mode: M4 -*- ###########################
#  File      : /afs/psi.ch/user/f/flechsig/phase/src/config/mdl_have_root.m4
#  Date      : <27 Sep 12 17:51:19 flechsig> 
#  Time-stamp: <27 Sep 12 17:52:06 flechsig> 

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 


dnl mdl_have_root.m4 -- 
dnl 
dnl Copyright (C) 1999, 2000 Matthew D. Langston <langston@SLAC.Stanford.EDU>
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

dnl MDL_HAVE_ROOT
dnl -------------
dnl 

dnl Search for ROOT.  If ROOT is found, then the output variables
dnl ROOTSYS, ROOTCINT, ROOT_CFLAGS and ROOT_LIBS are defined, and the
dnl shell variable "have_root" is set to "yes".  Otherwise, if ROOT is
dnl not found, a diagnostic is emitted and the shell variable
dnl "have_root" is set to "no".
dnl
dnl The ROOTCINT variable is quite convenient in Makefiles.
dnl
AC_DEFUN([MDL_HAVE_ROOT],
[
  AC_REQUIRE([AC_PROG_CXX])
  AC_REQUIRE([MDL_DL])
  AC_REQUIRE([MDL_GX_LIBS])
  AC_CACHE_CHECK([for ROOT], mdl_cv_have_root,
  [
    # Be pessimistic at first.
    mdl_cv_have_root=no

    # Save the "AC_MSG_RESULT file descriptor" to FD 8.
    exec 8>&AC_FD_MSG

    # Temporarily turn off AC_MSG_RESULT so that the user gets pretty
    # messages.
    exec AC_FD_MSG>/dev/null

dnl Allow the user to specify the top of the ROOT installation
dnl directory.
    AC_HELP_STRING([--with-rootsys], [top of the ROOT installation directory], rootsys_help_string)
    AC_ARG_WITH(rootsys, $rootsys_help_string, user_rootsys=$withval, user_rootsys="none")

    if test ! x"$user_rootsys" = xnone; then
      rootsys="$user_rootsys"
    else
      rootsys=$ROOTSYS
    fi

dnl Perhaps these will be useful in the future.
dnl
dnl mdl_root_version_parser="sed -ne 's/.*Version[ \t][ \t]*\([^ \t][^ \t]*\).*/\1/p' | sed -ne 's/\//\./p'"
dnl eval "mdl_root_version=\`echo .q | root -b -n -l -q | $mdl_root_version_parser\`"

    # We treat mdl_rootsys_search_list as a LIFO, on the assumption that
    # more recent (and therefore "later") versions of ROOT, and more
    # recent (and therefore "later") versions of the C++ compiler, are
    # preferable.  This only works for the simplistic (but usually
    # correct) assumption that the sort order for directories as listed
    # by the `ls' command is "version ascending order".
    #
    mdl_cxx_version=`$CXX --version | sed -ne 's/[^0-9.]*//g' -ne 's/.*/gcc-c++-&/p'`
    mdl_rootsys_search_list="/usr/local/root"
    for mdl_path in /usr/local/root/*; do
        eval "mdl_path=\`echo $mdl_path | grep '[0-9]\+\(\.[0-9]\+\)\{2\}'\`/$mdl_cxx_version"
        if test -d "$mdl_path"; then
            mdl_rootsys_search_list="$mdl_path $mdl_rootsys_search_list"
        fi
    done

    # If it turns out that there are other canonical places that ROOT is
    # installed, these directories should be specified as elements of
    # the following `for' loop.
    #
    for MDL_ROOTSYS in $rootsys $mdl_rootsys_search_list; do

dnl The existence of the `rootcint' program is a requirement.
      AC_PATH_PROG(ROOTCINT, rootcint, no, $MDL_ROOTSYS/bin)
      test ! x"$ROOTCINT" = xno && break;

    done

    if test ! x"$ROOTCINT" = xno; then

dnl The `root-config' script is handy if it is available.  This script
dnl is included only in later versions of ROOT, which I think means ROOT
dnl 2.22.10 and later.
        AC_PATH_PROG(root_config,
                     root-config,
                     [AC_MSG_ERROR([Your ROOT installation does not have root-config installed])],
                     $MDL_ROOTSYS/bin)

        ac_save_ROOTSYS=$ROOTSYS
        ROOTSYS=$MDL_ROOTSYS

        eval "ROOT_CFLAGS=\`$root_config --cflags\`"
        eval "ROOT_LIBS=\`$root_config --libs\`"
        eval "ROOT_GLIBS=\`$root_config --glibs\`"

        ROOTSYS=$ac_save_ROOTSYS

        # Try to compile and link a simple ROOT program.
        ac_save_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$ROOT_CFLAGS $CPPFLAGS"

        ac_save_LIBS="$LIBS"
        LIBS="$LIBS $ROOT_LIBS"

        AC_LANG_SAVE
        AC_LANG_CPLUSPLUS

        changequote(, )dnl
        cat > conftest.$ac_ext <<EOF
#include "TROOT.h"
#include "TRint.h"


int Error;                      // left undefined by Motif

extern void  InitGui();         // initializer for GUI needed for interactive interface
VoidFuncPtr_t initfuncs[] = { InitGui, 0 };

// Initialize the ROOT system
TROOT root( "Rint","The ROOT Interactive Interface", initfuncs );

int
main( int argc, char* argv[] )
{
   // Create interactive interface
   TRint *theApp = new TRint( "ROOT example", &argc, argv, NULL, 0 );

   // Run interactive interface
   theApp->Run();

   return( 0 );
}
EOF
        changequote([, ])dnl

        if AC_TRY_EVAL(ac_link) && test -s conftest${ac_exeext}; then
          mdl_cv_have_root=yes
        else
          mdl_cv_have_root=no
        fi

        AC_LANG_RESTORE

        LIBS="$ac_save_LIBS"
        CPPFLAGS="$ac_save_CPPFLAGS"

        if test x"$mdl_cv_have_root" = xyes; then

            ROOTSYS=$MDL_ROOTSYS
            AC_SUBST(ROOTSYS)

            # Inform all of the Makefiles where to find rootcint.
            ROOTCINT='export ROOTSYS=$(ROOTSYS); $(ROOTSYS)/bin/rootcint'
            AC_SUBST(ROOTCINT)

            AC_SUBST(ROOT_CFLAGS)
            AC_SUBST(ROOT_LIBS)
            AC_SUBST(ROOT_GLIBS)
        fi
        rm -f conftest*
    fi

    if test ! x"$mdl_cv_have_root" = xyes; then
      AC_MSG_WARN([ROOT was not found on your system.  Try using --with-rootsys])
    fi

    # Restore pretty messages.
    exec AC_FD_MSG>&8

    have_root=$mdl_cv_have_root
  ])
])
