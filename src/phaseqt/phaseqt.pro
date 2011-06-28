# -*-makefile-*-
#  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/qtgui.pro
#  Date      : <31 May 11 16:59:10 flechsig> 
#  Time-stamp: <22 Jun 11 17:50:10 flechsig> 
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
SOURCES   += main.cpp mainwindow.cpp qtphase.cpp phasec.c cutils.c bline.c rtrace.c xmalloc.c error.c geometrypck.c mirrorpck.c
RESOURCES += qtgui.qrc
DEFINES   += QTGUI  DEBUG HAVE_CONFIG_H
#DEFINES   += QTGUI  SEVEN_ORDER DEBUG HAVE_CONFIG_H
CONFIG    += qt debug

