#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/Makefile.am
#  Date      : <26 Nov 03 15:37:47 flechsig> 
#  Time-stamp: <2014-05-21 07:26:25 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# this is the makefile template to built phase with automake/ autoconfig

bin_PROGRAMS  = phaseqt 
phaseqt_SOURCES = \
	configwindow.cpp configwindow.h \
	main.cpp mainwindow.cpp mainwindow_slots.cpp mainwindow.h \
        myzoomer.cpp myzoomer.h \
        optiinput.cpp optiinput.h  \
	phaseqt.cpp phaseqt.h \
	plot.cpp plot.h \
	plot1d.cpp plot1d.h \
	singleray.cpp singleray.h \
        treemodel.cpp treemodel.h \
        treeitem.cpp treeitem.h \
	plotmatrix.cpp plotmatrix.h \
        plot2x2.cpp plot2x2.h \
	unwrap_phase.cpp unwrap_phase.h \
        ../phase/bline.c ../phase/cutils.c ../phase/error.c \
	../phase/phasec.c  ../phase/OptiIntParam.c \
        ../phase/rtrace.c ../phase/myhdf5.c ../phase/myhdf5.h \
	../phase/myfftw3.c ../phase/myfftw3.h \
        ../phase/xmalloc.c ../phase/pst_thread.c ../phase/pst_thread.h   \
	../phase/drift_8.for    ../phase/elli_8.F ../phase/fdet_8.for \
	../phase/fgmapidp_8.F   ../phase/make_matrix_8.for \
	../phase/rekursiv_2d_8.F \
	../phase/phasefor.F  \
	../phase/get_partial_etc.for ../phase/replace_6v4v.F \
	../phase/replace_wl_in_u.F ../phase/replace_wl_in_ypzp.F \
	../phase/taylor-ops.for \
	../phase/phase_source.F ../phase/pst.c ../phase/pst.h \
        ../phase/phase_integration.F \
	../phase/posrc.c ../phase/posrc.h \
	../phase/spa_3rd_order.c ../phase/spa_3rd_order.h \
	../phase/reflectivity.c ../phase/reflectivity.h \
	../phase/heighterror.c ../phase/heighterror.h
##../phase/wdfgmapm17.for ../phase/wdfgmapm27.for ../phase/wdfgmapl7.for 
##	../phaselib/misali_8.f ../phaselib/misali4_8.f 
##        ../phaselib/misali3_8.f ../phaselib/misali2_8.f ../phaselib/misali1_8.f

# for all *.h and *.qrc add one *.cpp
nodist_phaseqt_SOURCES = \
	moc_configwindow.cpp moc_mainwindow.cpp \
	moc_optiinput.cpp moc_plot.cpp moc_singleray.cpp  \
	qrc_phaseqt.cpp moc_treemodel.cpp moc_treeitem.cpp \
	moc_plotmatrix.cpp moc_plot2x2.cpp

## UF 28.8.2012 take this out for phaseqt
## UF 22.6.2013 take new version in again
phaseqt_LDADD = $(libdir)/@LIBPHASE@

# files installed under data
pkgdata_DATA = ../data/coefficient-example.dat \
 ../data/map70_lh.omx\
 ../data/map70_rh.omx \
../data/spa3table.tab \
../data/spa3table_20001.tab \
../data/material/rhoatom.dat \
../data/henke/Ac.f12 ../data/henke/Ag.f12 ../data/henke/Al.f12 \
../data/henke/Ar.f12 ../data/henke/As.f12 ../data/henke/At.f12 \
../data/henke/Au.f12 \
../data/henke/Ba.f12 ../data/henke/Be.f12 ../data/henke/B.f12 \
../data/henke/Bi.f12 ../data/henke/Br.f12 \
../data/henke/Ca.f12 ../data/henke/Cd.f12 ../data/henke/Ce.f12 \
../data/henke/C.f12  ../data/henke/Cl.f12 ../data/henke/Co.f12 \
../data/henke/Cr.f12 ../data/henke/Cs.f12 ../data/henke/Cu.f12 \
../data/henke/Dy.f12 ../data/henke/Er.f12 ../data/henke/Eu.f12 \
../data/henke/Fe.f12 ../data/henke/F.f12  ../data/henke/Fr.f12 \
../data/henke/Ga.f12 ../data/henke/Gd.f12 ../data/henke/Ge.f12 \
../data/henke/He.f12 ../data/henke/H.f12  ../data/henke/Hf.f12 \
../data/henke/Hg.f12 ../data/henke/Ho.f12 ../data/henke/I.f12  \
../data/henke/In.f12 ../data/henke/Ir.f12 ../data/henke/K.f12  \
../data/henke/Kr.f12 ../data/henke/La.f12 ../data/henke/Li.f12 \
../data/henke/Lu.f12 ../data/henke/Mg.f12 ../data/henke/Mn.f12 \
../data/henke/Mo.f12 ../data/henke/Na.f12 ../data/henke/Nb.f12 \
../data/henke/Nd.f12 ../data/henke/Ne.f12 ../data/henke/N.f12  \
../data/henke/Ni.f12 ../data/henke/O.f12  ../data/henke/Os.f12 \
../data/henke/Pa.f12 ../data/henke/Pb.f12 ../data/henke/Pd.f12 \
../data/henke/P.f12  ../data/henke/Pm.f12 ../data/henke/Po.f12 \
../data/henke/Pr.f12 ../data/henke/Pt.f12 ../data/henke/Ra.f12 \
../data/henke/Rb.f12 ../data/henke/Re.f12 ../data/henke/Rh.f12 \
../data/henke/Rn.f12 ../data/henke/Ru.f12 ../data/henke/Sb.f12 \
../data/henke/Sc.f12 ../data/henke/Se.f12 ../data/henke/S.f12  \
../data/henke/Si.f12 ../data/henke/Sm.f12 ../data/henke/Sn.f12 \
../data/henke/Sr.f12 ../data/henke/Ta.f12 ../data/henke/Tb.f12 \
../data/henke/Tc.f12 ../data/henke/Te.f12 ../data/henke/Th.f12 \
../data/henke/Ti.f12 ../data/henke/Tl.f12 ../data/henke/Tm.f12 \
../data/henke/U.f12  ../data/henke/V.f12  ../data/henke/W.f12  \
../data/henke/Xe.f12 ../data/henke/Yb.f12 ../data/henke/Y.f12  \
../data/henke/Zn.f12 ../data/henke/Zr.f12 ../data/henke/B4C.f12

# This is to ensure the myapp.h file is distributed in your myapp-0.1.tar.gz
# I also have a resources.qrc I need to include. The png is used in myapp.
EXTRA_DIST = ../phase/phase_struct.F  $(pkgdata_DATA) \
	phaseqt.qrc parameter.default \
	images/Blue-darrow-32.png \
	images/Blue-arrow-left-32.png images/Blue-arrow-right-32.png images/Blue-arrows-right-32.png\
	images/down-32.png images/footprint.png \
	images/Left-arrow-48.png images/new.png \
	images/print.png  images/quickrtrace.png \
	images/Right-arrow-48.png images/rtrace.png \
	images/save.png images/undo.png images/up-32.png \
	README

## flags
AM_CPPFLAGS = -DLINUX -DQTGUI -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED
###-I@QT_DIR@/mkspecs/linux-g++ 
### -fPIC  
AM_CXXFLAGS   =  -fPIC \
	@QT_CXXFLAGS@ \
	@QT_CXXFLAGS@/QtCore \
	@QT_CXXFLAGS@/QtGui \
	@QT_CXXFLAGS@/QtWidgets \
	@QT_CXXFLAGS@/QtPrintSupport \
	@QT_CXXFLAGS@/QtConcurrent \
	-I@QWT@/include \
	-I@QWTI@/ \
	-I../phase 

AM_CFLAGS = @HDF5_CFLAGS@ 

###-pipe -g -D_REENTRANT -Wall -W 
#####-I@QT_DIR@/mkspecs/linux-g++       
###AM_LDFLAGS = $(X_LIBS) -lXm -lMrm -lXt -lXext -lXp -lX11 -lm -lc -lnsl\
#####	     @FLIBS@    @CFORLIB@
#### -L@QT_DIR@/lib -lQtGui -lQtCore -lQtSvg 
## -lQt5Concurrent
## UF explicit linking to -lQtGui -lQtCore -lQtSvg is required for ubuntu
AM_LDFLAGS =  -Wl,-rpath,@QT_DIR@/lib \
	@QT_LIBS@ -lQtGui -lQtCore -lQtSvg \
	-L@QWT@/lib -lqwt @HDF5_LDFLAGS@  \
	@FLIBS@  @FFTW3_LIBS@ @CFORLIB@

##we always use the c linker
F77LINK = $(LINK)

# This rule lets GNU make create any moc_*.cpp from the equivalent *.h
moc_%.cpp: %.h
	@QT_MOC@ $< -o $@
####	@QT_MOC@ $(AM_CPPFLAGS) $(AM_CXXFLAGS) $< -o $@

# special
# does not work with --with-Qt-bin-dir=DIR
qrc_phaseqt.cpp : parameter.default
	@QT_RCC@ -name $(bin_PROGRAMS) phaseqt.qrc -o $@
#	@QT_DIR@/bin/rcc -name $(bin_PROGRAMS) phaseqt.qrc -o $@
#	@QT_RCC@ -name $(bin_PROGRAMS) phaseqt.qrc -o $@

# Adjust this line according to your resources.qrc
qrc_%.cpp : %.qrc
	@QT_RCC@ -name $(bin_PROGRAMS) $< -o $@
#	@QT_DIR@/bin/rcc -name $(bin_PROGRAMS) $< -o $@
#	@QT_RCC@ -name $(bin_PROGRAMS) $< -o $@


# This line ensures that generated moc_*.cpp and resources.cpp files are
# deleted when we make clean.
## CLEANFILES = $(filter moc_%.cpp,$(phaseqt_SOURCES)) 
## CLEANFILES is a variable called from target clean: rm -f $CLEANFILES
CLEANFILES = moc_*.cpp qrc_*.cpp *.f90 *.mod
# end
