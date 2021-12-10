dnl   File      : /afs/psi.ch/user/f/flechsig/phase/src/configure.ac
dnl   Date      : <11 Dec 03 13:59:53 flechsig> 
dnl   Time-stamp: <2021-12-10 15:18:44 flechsig> 
dnl   Author    : Uwe Flechsig, flechsig@psi.ch

dnl   $Source$ 
dnl   $Date$
dnl   $Revision$ 
dnl   $Author$ 

dnl ******************************************************************************
dnl
dnl   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
dnl                      Paul Scherrer Institut Villigen, Switzerland 
dnl   
dnl   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
dnl          Uwe Flechsig,    uwe.flechsig@psi.ch
dnl
dnl ------------------------------------------------------------------------------
dnl
dnl   This file is part of PHASE.
dnl
dnl   PHASE is free software: you can redistribute it and/or modify
dnl   it under the terms of the GNU General Public License as published by
dnl   the Free Software Foundation, version 3 of the License, or
dnl   (at your option) any later version.
dnl
dnl   PHASE is distributed in the hope that it will be useful,
dnl   but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl   GNU General Public License for more details.
dnl
dnl   You should have received a copy of the GNU General Public License
dnl   along with PHASE (LICENSE.txt).  If not, see <http://www.gnu.org/licenses/>. 
dnl
dnl ******************************************************************************

dnl hardcoded version for -std=c++11 required for qt-5.9 - can cause problems with old versions


AC_PREREQ([2.59])  # autoscan: AC_PREREQ([2.69])
AC_INIT([phase],[1.30],[uwe.flechsig@psi.ch])
AC_CONFIG_SRCDIR([phase/bline.c])
AC_REVISION([$Revision$]) # no autoscan
AC_CONFIG_AUX_DIR([config]) # no autoscan
AC_CONFIG_MACRO_DIR([config])

AM_INIT_AUTOMAKE([subdir-objects]) # no autoscan

dnl AC_CONFIG_HEADERS([phase/config.h]) # autoscan AC_CONFIG_HEADERS([config.h])
AC_CONFIG_HEADERS([config.h])

dnl uf Aug 2011
dnl AC_CONFIG_MACRO_DIR([m4])

dnl maintainer option i.e. make does not bootstrap
##AM_MAINTAINER_MODE

dnl Checks for programs.
AC_PROG_CXX
AC_PROG_AWK
AC_PROG_YACC

# Probleme mit mpicc, daher mpi auskommentiert am 5.9.2013 Johannes
#AC_PROG_CC
dnl includes mpi (replacement of AC_PROG_CC)
#If --with-mpi=auto is used, try to find MPI, but use standard C compiler if it is not found.
# If --with-mpi=yes is used, try to find MPI and fail if it isn't found.
# If --with-mpi=no is used, use a standard C compiler instead.
AC_ARG_WITH(mpi, 
  [AS_HELP_STRING([--with-mpi],
     [compile with MPI (parallelization) support. If none is found,
      MPI is not used. Default: auto])
  ],,[with_mpi=auto])

AX_PROG_CC_MPI([test x"$with_mpi" != xno],[use_mpi=yes],[
  use_mpi=no
  if test x"$with_mpi" = xyes; then
     AC_MSG_FAILURE([MPI compiler requested, but couldn't use MPI.])
  else
     AC_MSG_WARN([No MPI compiler found, won't use MPI.])
  fi
])
# Ende der Aenderungen vom 5.9.2013

AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
dnl UF Aug 15 AC_PROG_RANLIB
AC_LIBTOOL_DLOPEN


dnl check for root http://root.cern.ch/root/roottalk/roottalk01/0009.html
MDL_HAVE_ROOT

# Checks for libraries.
if test x"${have_root}" = "xyes"; then
   AC_CHECK_LIB([Minuit], [TMinuit::mnparm])
fi

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
AC_PROG_F77(gfortran ifort ifc pgf77 f77 g77)
AC_F77_LIBRARY_LDFLAGS
AC_SUBST(F77LIBS,${FLIBS})
echo "debug: found FLIBS: ${FLIBS}"

dnl set fortran compiler specials
AC_MSG_CHECKING(for a supported compiler type)

if test "$G77" = "yes"; then
   AC_MSG_RESULT(yes)
   AC_MSG_WARN([[support of GNU gfortran is new (not much tested)]])
fi

dnl check for fortran compatibility libs
AC_CHECK_LIB(gfortran,_gfortran_st_read)
AC_CHECK_LIB(g2c,fseek_)

dnl if $F77 is already defined we should strip a possible path
F77bn=`basename $F77`
wantgnu=false
wantabsoft=false
wantifc=false
wantportland=false
case "$F77bn" in
  "ifc")        # intel compiler
      AC_MSG_RESULT(found Intel ifc)
      AC_DEFINE(HAVE_INTEL_F77,"yes","Intel f77 compiler")
      AC_SUBST([FFLAGS],"-DINTEL -132")   # -vms
      libphasename=libphaseifc
      libphaselinkname=lphaseifc
      wantifc=true;;
  "ifort")        # intel compiler
      AC_MSG_RESULT(found Intel ifort)
      AC_DEFINE(HAVE_INTEL_F77,"yes","Intel f77 compiler")
      if test "$machine" = "x86_64"; then   
      AC_SUBST([FFLAGS],"-DINTEL -nogen-interfaces -threads -auto -noalign -132")  # opti. fails on 64 bit -noalign ??
dnl UF cforlib="-lintlc"
dnl UF to be modified and tested 
         AC_CHECK_LIB(intlc,put_any_routine_from_intlc)
      else
         AC_SUBST([FFLAGS],"-DINTEL -threads -auto -noalign -132") #  -noalign ?? -auto required for threads
      fi	 
      libphasename=libphaseifc
      libphaselinkname=lphaseifc
      wantifc=true;;
  "pgf77")        # portland compiler
      AC_MSG_RESULT(found Portland pgf77)
      AC_DEFINE(HAVE_PORTLAND_F77,"yes","Portland f77 compiler")
      AC_SUBST([FFLAGS],"-DPORTLAND -w")

## FFLAGS -silent: no warnings ??
##        -w     : no warnings
      libphasename=libphaseportland
      libphaselinkname=lphaseportland
      wantportland=true;;
  "f77")
      # assume absoft compiler as default
      AC_MSG_RESULT(assume Absoft f77)
      AC_DEFINE(HAVE_ABSOFT_F77,"yes","Absoft f77 compiler")
      libphasename=libphaseabsoft
      libphaselinkname=lphaseabsoft
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
   "gfortran")	
      AC_MSG_RESULT(found gfortran)
      AC_DEFINE(HAVE_GFORTRAN_F77,"yes","gfortran f77 compiler")
##      AC_SUBST([FFLAGS],"-DGNU --std=gnu")
      AC_SUBST([FFLAGS],"-DGNU --std=legacy")
##    --std=gnu or --std=legacy allows real numbers as index 

      libphasename=libphasegnu
      libphaselinkname=lphasegnu
      wantgnu=true;;
   *)
      AC_MSG_ERROR([Your Fortran compiler $F77bn can not be used to built PHASE]);;
esac
dnl cforlib is obsolete- can be taken out  (of makefiles)
cforlib=""
AC_SUBST([CFORLIB],${cforlib})
AM_CONDITIONAL([WANT_IFC],      [test x"$wantifc"      = xtrue])  
AM_CONDITIONAL([WANT_ABSOFT],   [test x"$wantabsoft"   = xtrue])
AM_CONDITIONAL([WANT_PORTLAND], [test x"$wantportland" = xtrue]) 
AM_CONDITIONAL([WANT_GNU],      [test x"$wantgnu"      = xtrue])  
dnl end fortran specials

dnl ***************** PO SOURCE MAX GRIDSIZE ***********
AC_ARG_WITH([gridsize],
  AS_HELP_STRING([--with-gridsize=NN],[max gridsize for obsolete OLD_PO_SOURCE (default: 1)]),
  dnl given action
  [
     echo "found  gridsize option: ${with_gridsize}"
     gridsize=${with_gridsize}
  ],
dnl option is not given- we use default 1
  [
    gridsize=1
  ])
AC_SUBST(GRIDSIZE, [${gridsize}])
AC_DEFINE_UNQUOTED(GRIDSIZE, [${gridsize}],[max size of PO source grid])
FFLAGS="-DGRIDSIZE=${gridsize} ${FFLAGS}"
dnl end gridsize
	
dnl ****************** QT **************************
dnl new qt test in config/ax_have_qt
AX_HAVE_QT

dnl test qt5
echo "have qt:" ${have_qt}
if test "x${have_qt}" = "xyes"; then
   qt5=`echo ${QT_LIBS} | grep "Qt5"`	
dnl echo "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx>>"${qt5}"<<"
dnl echo "xxxxxxxxxxxxx QT_LIBS xxxxxxxxxx>>"${QT_LIBS}"<<"
   if test -n "${qt5}"; then
       have_qt5="yes"
       AC_DEFINE([QT5], 1, [qt5])
   else  
       have_qt5="no" 
       AC_DEFINE([QT5], 0, [qt5])
   fi
   echo "have_qt5" ${have_qt5}
else
   echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
   echo "hint: some distributions call the Qt tools name-qt5 like qmake-qt5 instead of the default qmake"
   echo "      create symbolic links in your path for qmake, moc, uic, rcc, lrelease, lupdate"
   echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
fi
AM_CONDITIONAL([QT5], [test x$have_qt5 = xyes])
dnl end qt5

dnl ****************** QWT **************************
AC_ARG_WITH([qwt],
  AS_HELP_STRING([--with-qwt=DIR],[path to QWT libraries or --without-qwt]),
  [ echo "found qwt option: ${with_qwt}"
    qwt=${with_qwt}
    if test "${qwt}" = "no"; then
       echo "do not use qwt"
       have_qwt="no"
    else
      dnl   echo "do use qwt"
      AC_CHECK_FILE([${qwt}/lib/libqwt.so], 
      [
        AC_SUBST(QWT, [${qwt}])
	have_qwt="yes"
	AC_DEFINE([HAVE_QWT], 1, [qwt])
        qwti=${qwt}
	AC_SUBST(QWTI,[${qwti}])
      ],
      AC_MSG_ERROR(cannot find libqwt.so))
    fi
  ],
  [ dnl qwt is not given- we do a search
    AC_MSG_NOTICE((1) search for QWT lib libqwt.so in /usr/local)
    dnl to avoid permission denied measages
    qwt=`find /usr/local -name libqwt.so -print 2>/dev/null | sort | tail -1 | sed -e 's|/lib/libqwt.so||'`
    if test -n "${qwt}"; then
        AC_MSG_NOTICE((1) we found libqwt.so under ${qwt} - OK)
	dnl check if we find the libqwt.so there 
	AC_SUBST(QWT,  [${qwt}])
	have_qwt="yes"
        AC_DEFINE([HAVE_QWT], 1, [qwt])
	qwti=${qwt}
        AC_SUBST(QWTI, [${qwti}])
    else
      AC_MSG_NOTICE((1) did not found libqwt.so in /usr/local)
      AC_MSG_NOTICE((2) search for libqwt.so in /usr/lib64)
      qwt=`find /usr/lib64/ -name libqwt.so -print 2>/dev/null | sort | tail -1 | sed -e 's|/lib64/libqwt.so||'`
      if test -n "${qwt}"; then
         AC_MSG_NOTICE([(2) we found libqwt.so under ${qwt} - OK])
         AC_SUBST(QWT, [${qwt}])
	 have_qwt="yes"
         AC_DEFINE([HAVE_QWT], 1, [qwt])
	 qwti=${qwt}
         AC_SUBST(QWTI, [${qwti}])
      else
         AC_MSG_ERROR(cannot find any libqwt.so)
      fi
      AC_MSG_NOTICE((2) search for qwt_plot.h on /usr/include)	 
      qwti=`find /usr/include -name qwt_plot.h -print 2>/dev/null | sort | tail -1 | sed -e 's|/qwt_plot.h||'`
      if test -n "${qwti}"; then
         AC_MSG_NOTICE([(2) we found qwt_plot.h under ${qwti} - OK])
	 have_qwt="yes"
	 AC_DEFINE([HAVE_QWT], 1, [qwt])
         AC_SUBST(QWTI, [${qwti}])
      else
         AC_MSG_ERROR(cannot find any qwt_plot.h)
      fi
    fi
  ])
  AM_CONDITIONAL([QWT], [test x$have_qwt = xyes])
dnl end QWT

dnl Check for HDF5 support
AX_LIB_HDF5
dnl UF Oct 2014 I observed problems in the substitution of HDF5_LIBS 
dnl UF Oct 2014 I merge HDF5_LDFLAGS and HDF5_LIBS into MY_HDF5_LIBS
AC_SUBST([MY_HDF5_LIBS], ["${HDF5_LDFLAGS} ${HDF5_LIBS}"])

dnl Check for FFTW3 (FFT)
dnl AX_CHECK_FFTW3
ACX_FFTW3

dnl deal with the debugger 
AC_ARG_ENABLE([debug],
  AS_HELP_STRING([--enable-debug],[debugging code, static linking]),
    if test "$enable_debug" = "yes"; then
       AC_DEFINE([DEBUG], 1, "debugging on")
       AC_MSG_NOTICE(*** produce debugging code ***)
       F77bn=`basename $F77`
       if test "$F77" = "f77"; then
	  FFLAGS="-DDEBUG -g -C -B111 ${FFLAGS}"
       else
          if test "$F77bn" = "ifort" -o "$F77bn" = "ifc"; then
dnl	    FFLAGS="-DDEBUG -g -O0 -check bounds ${FFLAGS}"
dnl            FFLAGS="-DDEBUG -g -O0 -check all -debug -warn all -warn nodeclarations -warn nounused ${FFLAGS}"
dnl	       FFLAGS="-DDEBUG -g -O0 -check all -debug -warn all -warn nodeclarations  ${FFLAGS}"
	       FFLAGS="-DDEBUG -g -O0 -check all -debug -warn all ${FFLAGS}"
	  else
            FFLAGS="-DDEBUG -g -O0 ${FFLAGS}"
          fi
       fi
dnl       CFLAGS="-DDEBUG -gdwarf-2 -g3 ${CFLAGS}"
       CFLAGS="-DDEBUG -ggdb"             # -static
dnl UF 20180612 add -std=c++11 for qt-5.9
       CXXFLAGS="-DDEBUG -ggdb 
###-std=c++11"
       ####LDFLAGS="-static"
       libextension="a"
    fi,
    [AC_MSG_NOTICE(***** produce production code *****)
    enable_debug="no"
    CXXFLAGS="-std=c++11"
    libextension="so"])
dnl end debugger

dnl deal with experimental code
AC_ARG_ENABLE([experimental],
  AS_HELP_STRING([--enable-experimental],[include experimental code/features]),
    if test "$enable_experimental" = "yes"; then
       AC_DEFINE([EXPERIMENTAL], 1, "experimental on")
       AC_MSG_NOTICE(*** include experimental code ***)
       FFLAGS="${FFLAGS} -DEXPERIMENTAL"
    fi,
    [
    enable_experimental="no"
    ])
dnl end experimental

dnl libtool init after selecting the fortran compiler
AC_PROG_LIBTOOL

## expire option
AC_ARG_ENABLE([EXPIRE],
    AS_HELP_STRING(--enable-EXPIRE=YYYYMMDD,expiring date of the code),
    AC_DEFINE_UNQUOTED([EXPIRE], $enableval, "expiring date"))

dnl enable f2c replacement to save stack
AC_ARG_ENABLE([F2C],
   AS_HELP_STRING([--enable-F2C],[activate f2c code to lower stack consumption]),
   AC_DEFINE([F2C], 1, "enabled F2C"))
if test "$enable_F2C" != "yes" ; then
  enable_F2C="no"
else  
  FFLAGS="-DF2C ${FFLAGS}"
fi

## phase with REDUCE code in phaselib - replaces --enable-SEVEN
## default (iord= 1..4)
AC_ARG_ENABLE([PHASELIB],
    AS_HELP_STRING([--enable-PHASELIB],[activate old reduce code]),
    AC_DEFINE([PHASELIB], 1, "phaselib reduce code"))
AC_DEFINE([SEVEN_ORDER], 1, "expansion up to 7th order")
enable_SEVEN="yes"
if test "$enable_PHASELIB" != "yes" ; then
   enable_PHASELIB="no"
   FFLAGS="-DSEVEN_ORDER ${FFLAGS}"	
else 
### AC_DEFINE adds the defines to configure.h - configure.h can not be used for fortran
   FFLAGS="-DPHASELIB -DSEVEN_ORDER ${FFLAGS}"
fi

## whether to use OpenMP multithreading 
echo "new openmp test"
##dnl AX_OPENMP
##echo "AX_OPENMP result: " ${have_openmp}
##echo "end new openmp test"

AC_ARG_ENABLE([OPENMP],
    AS_HELP_STRING([--enable-OPENMP],[enables multicore parallelization of calculation-intense parts]),  
    AC_DEFINE([OPENMP], 1, "enabled OpenMP parallelization"))
if test "$enable_OPENMP" != "yes" ; then
  enable_OPENMP="no"
else   
   FFLAGS="${FFLAGS} -openmp -mp"
   FLIBS="${FLIBS} -liomp5 -lpthread"
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

dnl ************ PHASELIB *******************************
dnl check if a phaselib is installed
AC_SUBST([LIBPHASE], "${libphasename}.${libextension}")
AC_MSG_NOTICE(search if library ${LIBPHASE} is installed)
AC_CHECK_FILE(["${prefixlib}/${LIBPHASE}"], 
   [have_libphase="yes"], [have_libphase="no"])
dnl Linkeroption to build "lphase4idl"
if test "$enable_PHASELIB" == "yes" ; then
   AC_SUBST([LINK_LIBPHASE], "-L${prefixlib} -${libphaselinkname}")
fi
dnl IDL
dnl idldir=${prefix}/idl"
dnl AC_DEFINE_UNQUOTED(IDLDIR, "${prefix}/idl", "xxx")

# Checks for libraries.
# FIXME: Replace `main' with a function in `-lMinuit':
dnl AC_CHECK_LIB([Minuit], [main])
# FIXME: Replace `main' with a function in `-lQtCore':
dnl AC_CHECK_LIB([QtCore], [main])
# FIXME: Replace `main' with a function in `-lQtGui':
dnl AC_CHECK_LIB([QtGui], [main])
# FIXME: Replace `main' with a function in `-lQtSvg':
dnl AC_CHECK_LIB([QtSvg], [main])
# FIXME: Replace `main' with a function in `-lX11':
dnl AC_CHECK_LIB([X11], [main])
# FIXME: Replace `main' with a function in `-lXext':
dnl AC_CHECK_LIB([Xext], [main])
# FIXME: Replace `main' with a function in `-lXm':
dnl AC_CHECK_LIB([Xm], [main])
# FIXME: Replace `main' with a function in `-lXp':
dnl AC_CHECK_LIB([Xp], [main])
# FIXME: Replace `main' with a function in `-lXt':
dnl AC_CHECK_LIB([Xt], [main])
# FIXME: Replace `main' with a function in `-lc':
dnl AC_CHECK_LIB([c], [main])
# FIXME: Replace `main' with a function in `-lhdf5':
dnl AC_CHECK_LIB([hdf5], [main])
# FIXME: Replace `main' with a function in `-lm':
dnl AC_CHECK_LIB([m], [main])
# FIXME: Replace `main' with a function in `-lqwt':
dnl AC_CHECK_LIB([qwt], [main])
# FIXME: Replace `main' with a function in `-lz':
dnl AC_CHECK_LIB([z], [main])

dnl Checks for header files.
AC_PATH_X        # autoscan
AC_PATH_XTRA     # UF 13.5.13 ??
AC_HEADER_STDC   # autoscan
AC_CHECK_HEADERS([stdint.h stdlib.h string.h strings.h unistd.h]) # autoscan

dnl Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL # autoscan
AC_C_CONST        # autoscan
AC_C_INLINE       # autoscan
AC_TYPE_SIZE_T    # autoscan
dnl AC_TYPE_INT32_T   # autoscan64
dnl AC_TYPE_SSIZE_T   # autoscan64
dnl AC_TYPE_UINT16_T  # autoscan64
dnl AC_TYPE_UINT32_T  # autoscan64
AC_STRUCT_TM      # autoscan
AC_C_VOLATILE     # autoscan

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE # autoscan
# removed these chks because they produce sophisticated errors, JB, 5.9.2013
# AC_FUNC_MALLOC        # autoscan
# AC_FUNC_REALLOC       # autoscan
AC_FUNC_STAT          # autoscan
AC_CHECK_FUNCS([bzero memset pow sqrt strchr strrchr strstr]) # autoscan

AC_CONFIG_FILES([Makefile
                 baselib/Makefile
                 extr/Makefile
		 fkoe/Makefile
		 fkoempi/Makefile
                 hdf5tools/Makefile
                 idlphase/Makefile
                 phasepython/Makefile
                 misalignment/Makefile
                 opti/Makefile
                 phase4idl/Makefile
		 phaselib/Makefile
                 phaseqt/Makefile
		 phasempi/Makefile
                 phasesrv/Makefile])

AC_OUTPUT

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
echo "enable_phaselib          : ${enable_PHASELIB}"
dnl echo "LINK_LIBPHASE   : ${LINK_LIBPHASE} "
echo "debug code               : ${enable_debug}"
echo "prefix:                  : ${prefix}"
echo "seventh order:           : ${enable_SEVEN}"
echo "OpenMP:                  : ${enable_OPENMP}"
echo "Qt:                      : ${have_qt}"
echo "Qt5:                     : ${have_qt5}"
echo "QT_DIR:                  : ${QT_DIR}"
echo "qwt:                     : ${qwt}"
echo "qwti:                    : ${qwti}"
echo "CPPFLAGS:                : ${CPPFLAGS}"
echo "GRIDSIZE:                : ${gridsize}"
echo "hdf5:                    : ${with_hdf5}"
echo "HDF5_VERSION             : ${HDF5_VERSION}"
echo "root:                    : ${have_root}"
echo "f2c:                     : ${enable_F2C}"
echo "MPI:                     : ${use_mpi}"
echo "FFTW3:                   : ${acx_fftw3_ok}"
echo "incl. experimental code  : ${enable_experimental}"
echo " "
echo "******* UF debug ax_lib_hdf5.m4 HDF5_CFLAGS  =" $HDF5_CFLAGS
echo "******* UF debug ax_lib_hdf5.m4 HDF5_CPPFLAGS=" $HDF5_CPPFLAGS
echo "******* UF debug ax_lib_hdf5.m4 HDF5_LDFLAGS =" $HDF5_LDFLAGS
echo "******* UF debug ax_lib_hdf5.m4 HDF5_LIBS    =" $HDF5_LIBS
echo "******* UF debug             MY_HDF5_LIBS    =" $MY_HDF5_LIBS
echo "******* UF debug ax_lib_hdf5.m4 HDF5_VERSION =" $HDF5_VERSION
## end /afs/psi.ch/user/f/flechsig/phase/src/configure.ac
