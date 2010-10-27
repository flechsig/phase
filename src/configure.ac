dnl   File      : /afs/psi.ch/user/f/flechsig/phase/src/configure.ac
dnl   Date      : <11 Dec 03 13:59:53 flechsig> 
dnl   Time-stamp: <27 Oct 10 09:16:45 flechsig> 
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

dnl check 64bit linux
AC_MSG_CHECKING(for 64 bit linux)
machine=`uname -m`
dnl echo machine: $machine
if test "$machine" = "x86_64"; then
   AC_MSG_RESULT(yes)
   AC_DEFINE(HAVE_X86_64,"yes","64 bit linux")
else
   AC_MSG_RESULT(no) 
fi 

dnl end 64 bit

dnl do LIBTOOL test after fortran stuff

dnl define the search preference of fortran compilers
dnl ************** FORTRAN specials *********************
dnl !!the environment variable F77 overwrites this test!!
AC_PROG_F77(ifort ifc pgf77 f77 gfortran g77)
AC_F77_LIBRARY_LDFLAGS
AC_SUBST(F77LIBS,${FLIBS})
echo "debug: found FLIBS: ${FLIBS}"

dnl set fortran compiler specials
AC_MSG_CHECKING(for a supported compiler type)

if test "$G77" = "yes"; then
   AC_MSG_RESULT(no)
   AC_MSG_ERROR([[GNU Fortran g77 will not work (lack of VAX extensions)]])
fi

dnl check for fortran compatibility libs
AC_CHECK_LIB(gfortran,_gfortran_st_read)
AC_CHECK_LIB(g2c,fseek_)

case "$F77" in
  "ifc")        # intel compiler
      AC_MSG_RESULT(found Intel ifc)
      AC_DEFINE(HAVE_INTEL_F77,"yes","Intel f77 compiler")
      AC_SUBST([FFLAGS],"-vms -w95 -132")
      libphasename=libphaseifc
	libphaselinkname=lphaseifc
      wantifc=true
      wantabsoft=false
      wantportland=false;;
  "ifort")        # intel compiler
      AC_MSG_RESULT(found Intel ifc)
      AC_DEFINE(HAVE_INTEL_F77,"yes","Intel f77 compiler")
      if test "$machine" = "x86_64"; then   
	 AC_SUBST([FFLAGS],"-vms -w95 -132 -O0")  # opti. fails on 64 bit
dnl UF cforlib="-lintlc"
dnl UF to be modified and tested 
         AC_CHECK_LIB(intlc,put_any_routine_from_intlc)
      else
         AC_SUBST([FFLAGS],"-vms -w95 -132")
      fi	 
      libphasename=libphaseifc
	libphaselinkname=lphaseifc
      wantifc=true
      wantabsoft=false
      wantportland=false;;
  "pgf77")        # portland compiler
      AC_MSG_RESULT(found Portland pgf77)
      AC_DEFINE(HAVE_PORTLAND_F77,"yes","Portland f77 compiler")
      AC_SUBST([FFLAGS],"-w")

## FFLAGS -silent: no warnings ??
##        -w     : no warnings
      libphasename=libphaseportland
	libphaselinkname=lphaseportland
      wantifc=false
      wantabsoft=false
      wantportland=true;;
  "f77")
      # assume absoft compiler as default
      AC_MSG_RESULT(assume Absoft f77)
      AC_DEFINE(HAVE_ABSOFT_F77,"yes","Absoft f77 compiler")
      libphasename=libphaseabsoft
	libphaselinkname=lphaseabsoft
      wantifc=false
      wantportland=false
      wantabsoft=true
      AC_SUBST([FFLAGS],"-DABSOFT -f -B108 -W -s -N3 -N51 -N22 -N33");;

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


esac
dnl cforlib is obsolete- can be taken out  (of makefiles)
cforlib=""
AC_SUBST([CFORLIB],${cforlib})
AM_CONDITIONAL([WANT_IFC],      [test x"$wantifc"      = xtrue])  
AM_CONDITIONAL([WANT_ABSOFT],   [test x"$wantabsoft"   = xtrue])
AM_CONDITIONAL([WANT_PORTLAND], [test x"$wantportland" = xtrue])  
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
       if test "$F77" = "f77"; then
	  FFLAGS="-DDEBUG -g -C -B111 ${FFLAGS}"
       else
          FFLAGS="-DDEBUG -g -O0 ${FFLAGS}"
       fi
dnl       CFLAGS="-DDEBUG -gdwarf-2 -g3 ${CFLAGS}"
       CFLAGS="-DDEBUG -ggdb"
       libextension="a"
    fi,
    [AC_MSG_NOTICE(***** produce production code *****)
    enable_debug="no"
    libextension="so"])
dnl end debugger

dnl libtool init after selecting the fortran compiler
AC_PROG_LIBTOOL

## expire option
AC_ARG_ENABLE([EXPIRE],
    AC_HELP_STRING([--enable-EXPIRE=YYYYMMDD],[expiring date of the code]),
    AC_DEFINE_UNQUOTED([EXPIRE], $enableval, "expiring date"))

## phase with power series up to eighth order (iord= 1 ...7) Nov. 2010
## default (iord= 1..4)
AC_ARG_ENABLE([SEVEN],
    AC_HELP_STRING([--enable-SEVEN],[max. order of power series 7, default: 4]),
    AC_DEFINE([SEVEN_ORDER], 1, "expansion up to 7th order"))
if test "$enable_SEVEN" != "yes" ; then
   enable_SEVEN="no"
fi

dnl get configure date put it into header
configured=`date`
AC_DEFINE_UNQUOTED(CONFIGURED, "$configured", "configuration time")

dnl deal with the prefix
if test "$prefix" = "NONE" ; then
  atpsi=`/sbin/ifconfig | grep 129.129.`
  if test "$atpsi" = "" ; then
     prefix="/usr/local"
  else
     prefix="/afs/psi.ch/project/phase"
  fi
fi

AC_MSG_NOTICE(***** prefix: $prefix ****************** )
prefixlib=${prefix}/lib
AC_DEFINE_UNQUOTED(PREFIX, "$prefix", "the installation prefix")


dnl Checks for header files.
AC_PATH_X
AC_PATH_XTRA
AC_HEADER_STDC
AC_CHECK_HEADERS(strings.h unistd.h)

dnl **************** MOTIF ***************************

AC_ARG_WITH([motif],
  AC_HELP_STRING([--with-motif=DIR],[path to Motif libraries, includes, uil]),
  [ echo "motif path given as option: ${with_motif}"
    motifpath=${with_motif}
    AC_CHECK_PROG([UIL],[uil],[${motifpath}/bin/uil],[],[${motifpath}/bin])
    AC_SUBST([CPPFLAGS],[-I${motifpath}/include])
    AC_SUBST([MOTIFLIBS],["-L${motifpath}/lib -lXm -lMrm"])
  ],
  [
    AC_CHECK_PROG([UIL],[uil],[uil])
dnl autodetect Motif - but only add to libs_x later (if necessary)
dnl Use a different function to the earlier test to avoid problems with the
dnl internal cache.
AC_CHECK_HEADER(Xm/Xm.h,have_motif=yes,have_motif=no)
if test "$have_motif" = "yes"; then
dnl autodetect lesstif
    AC_MSG_CHECKING(for Lesstif)
    AC_EGREP_CPP(yes,
[#include <Xm/Xm.h>
#ifdef LESSTIF_VERSION
yes
#endif
], have_lesstif=yes, have_lesstif=no)
  AC_MSG_RESULT($have_lesstif)
fi

if test "$have_motif" = "no" -o "$have_lesstif" = "yes" ; then
   AC_MSG_WARN([[Motif installation is not OK!]])
   if test "$have_lesstif" = "yes" ; then
      AC_MSG_WARN([[with LESSTIF expect problems- use openMotif instead]])
   fi
else
dnl add include path to motif headers 
   AC_SUBST([CPPFLAGS],[-I$x_includes])
fi
])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_STRUCT_TM
AC_C_CONST
dnl UF 2.12.07 take this test out makes problems on 64bit Suse 10.3
dnl UF makes also problem on Scientific linux 5- add test i686
if test "$machine" != "x86_64" -a "$machine" != "i686"; then 
   AC_TYPE_SIZE_T
fi

dnl ************ PHASELIB *******************************
dnl check if a phaselib is installed
AC_SUBST([LIBPHASE], "${libphasename}.${libextension}")
AC_MSG_NOTICE(search if library ${LIBPHASE} is installed)
AC_CHECK_FILE(["${prefixlib}/${LIBPHASE}"], 
   [have_libphase="yes"], [have_libphase="no"])
dnl Linkeroption to build "lphase4idl"
AC_SUBST([LINK_LIBPHASE], "-L${prefixlib} -${libphaselinkname}")

   
   
   
dnl IDL
dnl idldir=${prefix}/idl"
dnl AC_DEFINE_UNQUOTED(IDLDIR, "${prefix}/idl", "xxx")

dnl output files 
AC_OUTPUT(Makefile phase/Makefile \
          opti/Makefile extr/Makefile gui/Makefile \
	  baselib/Makefile phaselib/Makefile phaseidl/Makefile \
	  misalignment/Makefile phase4idl/Makefile \
	  idlphase/Makefile tmplib/Makefile)

dnl print some messages
echo ""
echo "===================================================================="
### result of phaselib 
if test "x$have_libphase" = "xyes"; then
   echo "we found an installed PhaseLibrary: ${LIBPHASE} ==> good"
   echo ""
else
   echo "The required PhaseLibrary ${LIBPHASE} is not installed"
   echo "the library has to be built and installed in a separate step"
   echo "(!! compilation of ${LIBPHASE} may take several hours !!)"
   echo "cd ./phaselib"
   echo "make"
   echo "su -c \"make install\""
   echo "after the library has been installed- call make in the main directory"
   echo "cd .."
   echo "make"
   echo "su -c \"make install\""
fi
echo "===================================================================="
echo "summary: "
echo "fortran compiler         : ${F77}"
echo "phaselib                 : ${LIBPHASE}"
dnl echo "LINK_LIBPHASE   : ${LINK_LIBPHASE} "
echo "debug code               : ${enable_debug}"
echo "cernlib:                 : ${cernlib}"
echo "prefix:                  : ${prefix}"
echo "seventh order:           : ${enable_SEVEN}"

dnl., TORSTEN, einige Dinge um configure zu verstehen ...
dnl., echo " "
dnl., echo " ttttttttttssssssssssssssssssttttttttttttttttttttt"
dnl., echo " "
dnl., echo " LIBPHASE      : ${LIBPHASE} "
dnl., echo " LINK_LIBPHASE : ${LINK_LIBPHASE} "
dnl., echo " FLIBS         : ${FLIBS} "
dnl., echo " "
dnl., echo " ttttttttttssssssssssssssssssttttttttttttttttttttt"

echo " "
## end /afs/psi.ch/user/f/flechsig/phase/src/configure.ac
