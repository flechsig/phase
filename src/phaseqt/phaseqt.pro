# -*-makefile-*-
#  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/qtgui.pro
#  Date      : <31 May 11 16:59:10 flechsig> 
#  Time-stamp: <13 Jul 11 12:01:45 flechsig> 
#  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# 1st version generated with qmake -project

include(qwt.pri)   # qwt widgets


TEMPLATE = app
TARGET   = 
DEPENDPATH  += .
INCLUDEPATH += ../phase/.

# Input
HEADERS   += mainwindow.h qtphase.h plot.h
SOURCES   += main.cpp mainwindow.cpp qtphase.cpp plot.cpp ../phase/phasec.c ../phase/cutils.c ../phase/bline.c \
	../phase/rtrace.c ../phase/xmalloc.c ../phase/error.c ../phase/geometrypck.c ../phase/mirrorpck.c
RESOURCES += qtgui.qrc
DEFINES   += QTGUI DEBUG HAVE_CONFIG_H LINUX
#DEFINES   += QTGUI  SEVEN_ORDER DEBUG HAVE_CONFIG_H
CONFIG    += qt debug
LIBS      += ../phase/drift_8.o ../phase/phasefor.o ../phase/make_matrix_8.o ../phase/taylor-ops.o \
	../phase/q0_q1_q2_8.o ../phase/replace_wl_in_ypzp.o ../phase/replace_wl_in_u.o \
	../phase/misali_8.o ../phase/misali1_8.o ../phase/misali2_8.o ../phase/misali3_8.o ../phase/misali4_8.o\
	../phase/replace_6v4v.o ../phase/rekursiv_2d_8.o ../phase/fdet_8.o ../phase/fgmapidp_8.o \
	../phase/get_partial_etc.o -L/opt/intel/intel-10.0/fc-10.0/lib \
	-L/usr/lib/gcc/i386-redhat-linux/4.1.2/ \
	-L/usr/lib/gcc/i386-redhat-linux/4.1.2/../../../ \
	-lifport -lifcore -limf -lm -lipgo -lirc -lgcc_s -lirc_s -ldl /afs/psi.ch/project/phase/lib/libphaseifc.a -lgfortran 
