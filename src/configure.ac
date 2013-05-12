dnl   File      : /afs/psi.ch/user/f/flechsig/phase/src/configure.ac
dnl   Date      : <11 Dec 03 13:59:53 flechsig> 
dnl   Time-stamp: <2013-05-12 23:30:53 flechsig> 
dnl   Author    : Uwe Flechsig, uwe.flechsig@psi.ch

dnl   $Source$ 
dnl   $Date$
dnl   $Revision$ 
dnl   $Author$ 

dnl AC_PREREQ([2.69]) # autoscan
AC_PREREQ([2.59]) # manual
AC_INIT([phase],[1.21],[uwe.flechsig@psi.ch])
AC_CONFIG_SRCDIR([phase/bline.c])
AC_REVISION([$Revision$])
AC_CONFIG_AUX_DIR([config])      # not in scan

AM_INIT_AUTOMAKE

dnl AC_CONFIG_HEADERS([phase/config.h]) # manual 
AC_CONFIG_HEADERS([config.h])           # autoscan

dnl AC_CONFIG_MACRO_DIR([m4])
dnl maintainer option i.e. make does not bootstrap
##AM_MAINTAINER_MODE

dnl Checks for programs.
AC_PROG_CXX
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB




dnl check for root http://root.cern.ch/root/roottalk/roottalk01/0009.html
MDL_HAVE_ROOT



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
dnl           AX_LIB_HDF5()
dnl echo "Uwe HDF5 support:  $with_hdf5"



dnl ****************** HDF5 **************************
AC_ARG_WITH([hdf5],
  AS_HELP_STRING([--with-hdf5=DIR],[path to HDF5 libraries or --without-hdf5]),
  [ echo "found hdf5 option: ${with_hdf5}"
    hdf5=${with_hdf5}
    if test "${hdf5}" = "no"; then
       echo "do not use hdf5"
    else
      dnl   echo "do use hdf5"
      AC_CHECK_FILE([${hdf5}/lib/libhdf5.so],
      [
        AC_SUBST(HDF5, [${hdf5}])
        hdf5i=${hdf5}
        AC_SUBST(HDF5I,[${hdf5i}])
        AC_DEFINE(HAVE_HDF5,"yes","hdf5 sopport enabled")
      ],
      AC_MSG_ERROR(cannot find libhdf5.so))
    fi
  ],
  [ dnl hdf5 is not given- we do a search
    AC_MSG_NOTICE((1) search for HDF5 lib libhdf5.so in /usr/local)
    dnl to avoid permission denied measages
    hdf5=`find /usr/local -name libhdf5.so -print 2>/dev/null | head -1 | sed -e 's|/lib/libhdf5.so||'`
    if test -n "${hdf5}"; then
        dnl deal with lib64
        echo xxxxx $hdf5
        hdf5=`echo $hdf5 | sed -e 's|/lib64/libhdf5.so||'`
        AC_MSG_NOTICE((1) we found libhdf5.so under ${hdf5} - OK)
        dnl check if we find the libhdf5.so there
        AC_SUBST(HDF5,  [${hdf5}])
        hdf5i=${hdf5}
        AC_SUBST(HDF5I, [${hdf5i}])
        AC_DEFINE(HAVE_HDF5,"yes","hdf5 sopport enabled")
    else
      AC_MSG_NOTICE((1) did not found libhdf5.so in /usr/local)
      AC_MSG_NOTICE((2) search for libhdf5.so in /usr/lib)
      hdf5=`find /usr/lib -name libhdf5.so -print 2>/dev/null | head -1 | sed -e 's|/lib/libhdf5.so||'`
      if test -n "${hdf5}"; then
         AC_MSG_NOTICE([(2) we found libhdf5.so under ${hdf5} - OK])
         AC_SUBST(HDF5,  [${hdf5}])
         hdf5i=${hdf5}
         AC_SUBST(HDF5I, [${hdf5i}])
         AC_DEFINE(HAVE_HDF5,"yes","hdf5 sopport enabled")
      else
         AC_MSG_ERROR(cannot find any libhdf5.so)
      fi
      AC_MSG_NOTICE((2) search for hdf5.h on /usr/include)
      hdf5i=`find /usr/include -name hdf5.h -print 2>/dev/null | head -1 | sed -e 's|/hdf5.h||'`
      if test -n "${hdf5i}"; then
         AC_MSG_NOTICE([(2) we found hdf5.h under ${hdf5i} - OK])
         AC_SUBST(HDF5I, [${hdf5i}])
         AC_DEFINE(HAVE_HDF5,"yes","hdf5 sopport enabled")
      else
         AC_MSG_ERROR(cannot find any hdf5.h)
      fi
    fi
  ])
dnl end HDF5

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


dnl Checks for header files.
AC_PATH_X
AC_PATH_XTRA
AC_HEADER_STDC
AC_CHECK_HEADERS(strings.h unistd.h)

dnl Checks for typedefs, structures, and compiler characteristics.
AC_STRUCT_TM
dnl UF Aug 2011 AC_C_CONST test makes problems with c++
dnl AC_C_CONST it is obsolete
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
if test "$enable_PHASELIB" == "yes" ; then
   AC_SUBST([LINK_LIBPHASE], "-L${prefixlib} -${libphaselinkname}")
fi
dnl IDL
dnl idldir=${prefix}/idl"
dnl AC_DEFINE_UNQUOTED(IDLDIR, "${prefix}/idl", "xxx")

dnl output files 
AC_CONFIG_FILES([Makefile                 
 		 extr/Makefile
		 hdf5tools/Makefile
		 idlphase/Makefile
		 misalignment/Makefile
		 opti/Makefile
dnl		 phase/Makefile
		 phase4idl/Makefile
		 phaseidl/Makefile
 		 phaselib/Makefile 
                 phaseqt/Makefile 
                 phasesrv/Makefile])
AC_OUTPUT

dnl UF 31.8.11 take out something baselib/Makefile tmplib/Makefile
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
echo "hdf5:                    : ${hdf5}"
echo "root:                    : ${have_root}"
echo "f2c:                     : ${enable_F2C}"

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
