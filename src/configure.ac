dnl   File      : /afs/psi.ch/user/f/flechsig/phase/src/configure.ac
dnl   Date      : <11 Dec 03 13:59:53 flechsig> 
dnl   Time-stamp: <17 May 13 17:19:55 flechsig> 
dnl   Author    : Uwe Flechsig, flechsig@psi.ch

dnl   $Source$ 
dnl   $Date$
dnl   $Revision$ 
dnl   $Author$ 
 
AC_PREREQ([2.59])  # autoscan: AC_PREREQ([2.69])
AC_INIT([phase],[1.21],[uwe.flechsig@psi.ch])
AC_CONFIG_SRCDIR([phase/bline.c])
AC_REVISION([$Revision$]) # no autoscan
AC_CONFIG_AUX_DIR([config]) # no autoscan
AC_CONFIG_MACRO_DIR([config])

AM_INIT_AUTOMAKE # no autoscan

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
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB

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

dnl if $F77 is already defined we should strip a possible path
F77bn=`basename $F77`

case "$F77bn" in
  "ifc")        # intel compiler
      AC_MSG_RESULT(found Intel ifc)
      AC_DEFINE(HAVE_INTEL_F77,"yes","Intel f77 compiler")
      AC_SUBST([FFLAGS],"-132")   # -vms
      libphasename=libphaseifc
      libphaselinkname=lphaseifc
      wantifc=true
      wantabsoft=false
      wantportland=false;;
  "ifort")        # intel compiler
      AC_MSG_RESULT(found Intel ifort)
      AC_DEFINE(HAVE_INTEL_F77,"yes","Intel f77 compiler")
      if test "$machine" = "x86_64"; then   
      AC_SUBST([FFLAGS],"-132 -O0 -nogen-interfaces -threads -auto")  # opti. fails on 64 bit -noalign ??
dnl UF cforlib="-lintlc"
dnl UF to be modified and tested 
         AC_CHECK_LIB(intlc,put_any_routine_from_intlc)
      else
         AC_SUBST([FFLAGS],"-132 -threads -auto") #  -noalign ?? -auto required for threads
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

   *)
      AC_MSG_ERROR([Your Fortran compiler $F77bn can not be used to built PHASE]);;
esac
dnl cforlib is obsolete- can be taken out  (of makefiles)
cforlib=""
AC_SUBST([CFORLIB],${cforlib})
AM_CONDITIONAL([WANT_IFC],      [test x"$wantifc"      = xtrue])  
AM_CONDITIONAL([WANT_ABSOFT],   [test x"$wantabsoft"   = xtrue])
AM_CONDITIONAL([WANT_PORTLAND], [test x"$wantportland" = xtrue])  
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
AC_ARG_WITH([qt],
  AS_HELP_STRING([--with-qt=DIR],[path to QT libraries or --without-qt]),
  dnl given action
  [ echo "found qt option: ${with_qt}"
    qt=${with_qt}
    if test "${qt}" = "no"; then
       echo "do not use qt"
    else
    dnl   echo "do use qt"
    dnl check if we find the libQtGui.so there
      AC_CHECK_FILE([${qt}/lib/libQtGui.so], 
      [     
	AC_SUBST(QT,  [${qt}])
	qti=${qt}
	AC_SUBST(QTI, [${qti}])
      ], 
      AC_MSG_ERROR(cannot find libQtGui.so))
    fi
  ],
  [ dnl qt is not given- we do a search
    AC_MSG_NOTICE((1) search for QT lib libQtGui.so on /usr/local)
    dnl to avoid permission denied measages
    qt=`find /usr/local -name libQtGui.so -print 2>/dev/null | head -1 | sed -e 's|/lib/libQtGui.so||'`
    if test -n "${qt}"; then
        AC_MSG_NOTICE((1) we found libQtGui.so under ${qt} - OK)
	dnl check if we find the libQtGui.so there 
	AC_SUBST(QT, [${qt}])
	qti=${qt}
	AC_SUBST(QTI, [${qti}])
    else
      AC_MSG_NOTICE((1) did not found libQtGui.so /usr/local)
      AC_MSG_NOTICE((2) search for libQtGui.so on /usr/lib)
      qt=`find /usr/lib -name libQtGui.so -print 2>/dev/null | head -1 | sed -e 's|/lib/libQtGui.so||'`
      if test -n "${qt}"; then
         AC_MSG_NOTICE([(2) we found libQtGui.so under ${qt} - OK])
         AC_SUBST(QT, [${qt}])
	 qti=${qt}
	AC_SUBST(QTI, [${qti}])
      else
         AC_MSG_ERROR(cannot find any libQtGui.so)
      fi
      AC_MSG_NOTICE((2) search for QtGui in /usr/include)
      qti=`find /usr/include -name QtGui -type f -print 2>/dev/null | head -1 | sed -e 's|/QtGui||'`
      if test -n "${qti}"; then
         AC_MSG_NOTICE([(2) we found QtGui under ${qti} - OK])
	 AC_SUBST(QTI, [${qti}])
      else
         AC_MSG_ERROR(cannot find any libQtGui.so)
      fi
    fi
  ])
dnl end QT

dnl ****************** QWT **************************
AC_ARG_WITH([qwt],
  AS_HELP_STRING([--with-qwt=DIR],[path to QWT libraries or --without-qwt]),
  [ echo "found qwt option: ${with_qwt}"
    qwt=${with_qwt}
    if test "${qwt}" = "no"; then
       echo "do not use qwt"
    else
      dnl   echo "do use qwt"
      AC_CHECK_FILE([${qwt}/lib/libqwt.so], 
      [
        AC_SUBST(QWT, [${qwt}])
        qwti=${qwt}
	AC_SUBST(QWTI,[${qwti}])
      ],
      AC_MSG_ERROR(cannot find libqwt.so))
    fi
  ],
  [ dnl qwt is not given- we do a search
    AC_MSG_NOTICE((1) search for QWT lib libqwt.so in /usr/local)
    dnl to avoid permission denied measages
    qwt=`find /usr/local -name libqwt.so -print 2>/dev/null | head -1 | sed -e 's|/lib/libqwt.so||'`
    if test -n "${qwt}"; then
        AC_MSG_NOTICE((1) we found libqwt.so under ${qwt} - OK)
	dnl check if we find the libqwt.so there 
	AC_SUBST(QWT,  [${qwt}])
	qwti=${qwt}
        AC_SUBST(QWTI, [${qwti}])
    else
      AC_MSG_NOTICE((1) did not found libqwt.so in /usr/local)
      AC_MSG_NOTICE((2) search for libqwt.so in /usr/lib)
      qwt=`find /usr/lib -name libqwt.so -print 2>/dev/null | head -1 | sed -e 's|/lib/libqwt.so||'`
      if test -n "${qwt}"; then
         AC_MSG_NOTICE([(2) we found libqwt.so under ${qwt} - OK])
         AC_SUBST(QWT,  [${qwt}])
	 qwti=${qwt}
         AC_SUBST(QWTI, [${qwti}])
      else
         AC_MSG_ERROR(cannot find any libqwt.so)
      fi
      AC_MSG_NOTICE((2) search for qwt_plot.h on /usr/include)	 
      qwti=`find /usr/include -name qwt_plot.h -print 2>/dev/null | head -1 | sed -e 's|/qwt_plot.h||'`
      if test -n "${qwti}"; then
         AC_MSG_NOTICE([(2) we found qwt_plot.h under ${qwti} - OK])
         AC_SUBST(QWTI, [${qwti}])
      else
         AC_MSG_ERROR(cannot find any qwt_plot.h)
      fi
    fi
  ])
dnl end QWT

dnl Check for HDF5 support (does not work with the current autoconf version)
echo "new hdf5 test"
           AX_LIB_HDF5()
echo "HDF5 support:  $with_hdf5"
echo "end new hdf5 test"

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
            FFLAGS="-DDEBUG -g -O0 -check all -debug -warn all -warn nodeclarations -warn nounused ${FFLAGS}"
	  else
            FFLAGS="-DDEBUG -g -O0 ${FFLAGS}"
          fi
       fi
dnl       CFLAGS="-DDEBUG -gdwarf-2 -g3 ${CFLAGS}"
       CFLAGS="-DDEBUG -ggdb"             # -static
       CXXFLAGS="-DDEBUG -ggdb"
       ####LDFLAGS="-static"
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
AC_FUNC_MALLOC        # autoscan
AC_FUNC_REALLOC       # autoscan
AC_FUNC_STAT          # autoscan
AC_CHECK_FUNCS([bzero memset pow sqrt strchr strrchr strstr]) # autoscan

AC_CONFIG_FILES([Makefile
                 baselib/Makefile
                 extr/Makefile
                 hdf5tools/Makefile
                 idlphase/Makefile
                 misalignment/Makefile
                 opti/Makefile
                 phase4idl/Makefile
                 phaseidl/Makefile
                 phaselib/Makefile
                 phaseqt/Makefile
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
echo "qt:                      : ${qt}"
echo "qti:                     : ${qti}"
echo "qwt:                     : ${qwt}"
echo "qwti:                    : ${qwti}"
echo "CPPFLAGS:                : ${CPPFLAGS}"
echo "GRIDSIZE:                : ${gridsize}"
echo "hdf5:                    : ${with_hdf5}"
echo "root:                    : ${have_root}"
echo "f2c:                     : ${enable_F2C}"
echo " "
## end /afs/psi.ch/user/f/flechsig/phase/src/configure.ac
