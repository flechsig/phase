#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/Makefile.am
#  Date      : <26 Nov 03 15:37:47 flechsig> 
#  Time-stamp: <29 Aug 11 18:43:59 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# this is the makefile template to built phase with automake/ autoconfig

bin_PROGRAMS  = phaseqt 
phaseqt_SOURCES = main.cpp mainwindow.cpp phaseqt.cpp ../phase/phasec.c ../phase/cutils.c ../phase/bline.c ../phase/rtrace.c \
        ../phase/xmalloc.c ../phase/error.c  \
	../phase/phasefor.F \
	../phase/fgmapidp_8.F ../phase/make_matrix_8.for ../phase/taylor-ops.for \
	../phase/q0_q1_q2_8.for ../phase/get_partial_etc.for ../phase/rekursiv_2d_8.for \
	../phase/replace_6v4v.for ../phase/replace_wl_in_ypzp.for ../phase/replace_wl_in_u.for \
	../phase/drift_8.for ../phase/fdet_8.for

# You have one .h file, it's called myapp.h. Therefore, here I list
# its mocced name, moc_myapp.cpp.
nodist_phaseqt_SOURCES = \
	moc_phaseqt.cpp moc_mainwindow.cpp qrc_phaseqt.cpp moc_singleray.cpp moc_configwindow.cpp moc_optiinput.cpp



#phase.c phasec.c activate_proc.c phasegraf.c initdatset.c \
#	  geometrypck.c bline.c phase0.c error.c xmalloc.c \
#	  xstrdup.c phaseX.c \
#	  mirrorpck.c pst.c pstf.F \
#	  cutils.c rtrace.c ctype.c \
#	  phasegraffor.F phasefor.F \
#	  phase_source.F \
#	  phase_integration.F \
#	  misali.F misali1.f misali2.f misali3.f misali4.f \
#	  cutils.h phase_struct.h fg3pck.h mirrorpck.h geometrypck.h \
#	  phase.h rtrace.h error.h common.h version.h phaseX.h \
#          misali_8.f misali1_8.f misali2_8.f misali3_8.f misali4_8.f \
#          drift_8.for fgmapidp_8.F get_partial_etc.for rekursiv_2d_8.for \
#          replace_6v4v.for replace_wl_in_ypzp.for replace_wl_in_u.for \
#          taylor-ops.for q0_q1_q2_8.for make_matrix_8.for fdet_8.for

phaseqt_LDADD = $(libdir)/@LIBPHASE@

pkgdata_DATA = ../data/coefficient-example.dat \
 ../data/higz_windows.dat\
 ../data/map35_lh.omx\
 ../data/map35_rh.omx\
 ../data/map70_lh.omx\
 ../data/map70_rh.omx\
 ../data/news\
 ../data/phase.dat\
 ../data/phase.hlb\
 ../data/phaseinit.com\
 ../data/phaseuser.com

# This is to ensure the myapp.h file is distributed in your myapp-0.1.tar.gz
# I also have a resources.qrc I need to include. The png is used in myapp.
EXTRA_DIST = phase_struct.for $(pkgdata_DATA) \
phaseqt.qrc images/Blue-arrow-left-32.png images/Blue-arrow-right-32.png images/down-32.png	\
images/Left-arrow-48.png images/new.png images/print.png images/Right-arrow-48.png \
images/save.png images/undo.png images/up-32.png

## flags
AM_CPPFLAGS = -DLINUX -DQTGUI -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED
AM_CXXFLAGS   =  -I. \
	-I@QT@/mkspecs/linux-g++ \
	-I@QT@/include/QtCore \
	-I@QT@/include/QtGui -I@QT@/include \
	-I@QWT@/include \
	-I../phase 

###-pipe -g -D_REENTRANT -Wall -W 
#####-I@QT@/mkspecs/linux-g++       
###AM_LDFLAGS = $(X_LIBS) -lXm -lMrm -lXt -lXext -lXp -lX11 -lm -lc -lnsl\
#####	     @FLIBS@    @CFORLIB@
AM_LDFLAGS =  -Wl,-rpath,@QT@/lib \
	-L@QT@/lib -lQtGui  \
	-L/usr/X11R6/lib -lQtCore -lpthread  @FLIBS@  @CFORLIB@

##we always use the c linker
F77LINK = $(LINK)


# This rule lets GNU make create any moc_*.cpp from the equivalent *.h
moc_%.cpp: %.h
	moc $< -o $@

# Adjust this line according to your resources.qrc
qrc_%.cpp : %.qrc
	rcc $< -o $@

# This line ensures that generated moc_*.cpp and resources.cpp files are
# deleted when we make clean.
CLEANFILES = $(filter moc_%.cpp,$(phaseqt_SOURCES)) 
# end