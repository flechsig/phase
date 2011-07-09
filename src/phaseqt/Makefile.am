#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/Makefile.am
#  Date      : <26 Nov 03 15:37:47 flechsig> 
#  Time-stamp: <07 Jul 11 15:37:35 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# this is the makefile template to built phase with automake/ autoconfig

bin_PROGRAMS  = phaseqt 
phaseqt_SOURCES = main.cpp mainwindow.cpp qtphase.cpp ../phase/phasec.c ../phase/cutils.c ../phase/bline.c ../phase/rtrace.c \
                  ../phase/xmalloc.c ../phase/error.c ../phase/geometrypck.c ../phase/mirrorpck.c ../phase/phasefor.F \
	../phase/fgmapidp_8.F ../phase/make_matrix_8.for ../phase/taylor-ops.for ../phase/q0_q1_q2_8.for ../phase/get_partial_etc.for ../phase/rekursiv_2d_8.for \
	../phase/replace_6v4v.for ../phase/replace_wl_in_ypzp.for ../phase/replace_wl_in_u.for ../phase/drift_8.for ../phase/fdet_8.for


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

EXTRA_DIST = phase_struct.for $(pkgdata_DATA)

## flags
AM_CPPFLAGS = -DLINUX -DQTGUI -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED
AM_CXXFLAGS   = -I/usr/local/Trolltech/Qt-4.7.3/mkspecs/linux-g++ -I. -I/usr/local/Trolltech/Qt-4.7.3/include/QtCore -I/usr/local/Trolltech/Qt-4.7.3/include/QtGui -I/usr/local/Trolltech/Qt-4.7.3/include -I../phase -I. -pipe -g -D_REENTRANT -Wall -W 
###AM_LDFLAGS = $(X_LIBS) -lXm -lMrm -lXt -lXext -lXp -lX11 -lm -lc -lnsl\
#####	     @FLIBS@    @CFORLIB@
AM_LDFLAGS = moc_mainwindow.o qrc_qtgui.o -Wl,-rpath,/usr/local/Trolltech/Qt-4.7.3/lib -L/usr/local/Trolltech/Qt-4.7.3/lib -lQtGui -L/usr/local/Trolltech/Qt-4.7.3/lib -L/usr/X11R6/lib -lQtCore -lpthread  @FLIBS@    @CFORLIB@

##we always use the c linker
F77LINK = $(LINK)
# end