# -*-makefile-*-
#  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/qtgui.pro
#  Date      : <31 May 11 16:59:10 flechsig> 
#  Time-stamp: <10 Jun 11 14:58:13 flechsig> 
#  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# 1st version generated with qmake -project

TEMPLATE = app
TARGET   = 
DEPENDPATH  += .
INCLUDEPATH += ../phase/.

# Input
HEADERS   += mainwindow.h qtphase.h 
SOURCES   += main.cpp mainwindow.cpp qtphase.cpp phasec.c cutils.c bline.c rtrace.c xmalloc.c error.c
RESOURCES += qtgui.qrc
DEFINES   += QTGUI SEVEN_ORDER DEBUG
CONFIG    += qt debug

