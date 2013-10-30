 # File      : /afs/psi.ch/user/f/flechsig/phase/src/config/uf_have_qt.m4
 # Date      : <30 Oct 13 10:24:05 flechsig> 
 # Time-stamp: <30 Oct 13 10:26:19 flechsig> 
 # Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 # $Source$ 
 # $Date$
 # $Revision$ 
 # $Author$ 

### uf's old qt test

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
