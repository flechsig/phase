#  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/Makefile.am
#  Date      : <26 Nov 03 15:37:47 flechsig> 
#  Time-stamp: <14 Sep 12 17:20:09 flechsig> 
#  Author    : Uwe Flechsig, flechsig@psi.ch

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# this is the makefile template to built phase with automake/ autoconfig

bin_PROGRAMS  = phasesrv 
phasesrv_SOURCES = \
        phasesrv.c       phasesrv.h \
        ../phase/bline.c ../phase/cutils.c ../phase/error.c \
	../phase/phasec.c  ../phase/rtrace.c \
        ../phase/xmalloc.c   \
	../phase/drift_8.for    ../phase/elli_8.F ../phase/fdet_8.for \
	../phase/fgmapidp_8.F   ../phase/make_matrix_8.for \
	../phase/q0_q1_q2_8.for ../phase/rekursiv_2d_8.for \
	../phase/phasefor.F  \
	../phase/get_partial_etc.for ../phase/replace_6v4v.for \
	../phase/replace_wl_in_u.for ../phase/replace_wl_in_ypzp.for \
	../phase/taylor-ops.for \
	../phase/phase_source.F ../phase/pst.c ../phase/phase_integration.F \
        ../phase/wdfgmapm17.for ../phase/wdfgmapm27.for ../phase/wdfgmapl7.for \
        ../phase/pstf.F ../phase/posrc.c ../phase/posrc.h \
	../phaselib/misali_8.f ../phaselib/misali4_8.f \
        ../phaselib/misali3_8.f ../phaselib/misali2_8.f ../phaselib/misali1_8.f


##dummy_DATA = \
##	configwindow.cpp configwindow.h \
##	main.cpp mainwindow.cpp mainwindow_slots.cpp mainwindow.h \
##        myzoomer.cpp myzoomer.h \
##        optiinput.cpp optiinput.h  \
##	phaseqt.cpp phaseqt.h \
##	plot.cpp plot.h \
##	plot1d.cpp plot1d.h \
##	singleray.cpp singleray.h \
##        treemodel.cpp treemodel.h \
##        treeitem.cpp treeitem.h \
##	plotmatrix.cpp plotmatrix.h \
##        plot2x2.cpp plot2x2.h \
##        ../phase/bline.c ../phase/cutils.c ../phase/error.c \
##	../phase/phasec.c  ../phase/rtrace.c \
##        ../phase/xmalloc.c   \
##	../phase/drift_8.for    ../phase/elli_8.F ../phase/fdet_8.for \
##	../phase/fgmapidp_8.F   ../phase/make_matrix_8.for \
##	../phase/q0_q1_q2_8.for ../phase/rekursiv_2d_8.for \
##	../phase/phasefor.F  \
##	../phase/get_partial_etc.for ../phase/replace_6v4v.for \
##	../phase/replace_wl_in_u.for ../phase/replace_wl_in_ypzp.for \
##	../phase/taylor-ops.for \
##	../phase/phase_source.F ../phase/pst.c ../phase/phase_integration.F \
##        ../phase/wdfgmapm17.for ../phase/wdfgmapm27.for ../phase/wdfgmapl7.for \
##        ../phase/pstf.F ../phase/posrc.c ../phase/posrc.h \
##	../phaselib/misali_8.f ../phaselib/misali4_8.f \
##        ../phaselib/misali3_8.f ../phaselib/misali2_8.f ../phaselib/misali1_8.f



## UF 28.8.2012 take this out for phasesrv
#phasesrv_LDADD = $(libdir)/@LIBPHASE@

# files installed under data
pkgdata_DATA = ../data/coefficient-example.dat \
 ../data/map70_lh.omx\
 ../data/map70_rh.omx

# This is to ensure the myapp.h file is distributed in your myapp-0.1.tar.gz
# I also have a resources.qrc I need to include. The png is used in myapp.
EXTRA_DIST = ../phase/phase_struct.F ../phase/beamline_struct.F $(pkgdata_DATA) \
	phaseqt.qrc parameter.default \
	images/Blue-arrow-left-32.png images/Blue-arrow-right-32.png \
	images/down-32.png images/footprint.png \
	images/Left-arrow-48.png images/new.png \
	images/print.png  images/quickrtrace.png \
	images/Right-arrow-48.png images/rtrace.png \
	images/save.png images/undo.png images/up-32.png \
	README

## flags
AM_CPPFLAGS = -DLINUX 
AM_CXXFLAGS   =  -I. \
		-I../phase -W

###-pipe -g -D_REENTRANT -Wall -W 
#####-I@QT@/mkspecs/linux-g++       
###AM_LDFLAGS = $(X_LIBS) -lXm -lMrm -lXt -lXext -lXp -lX11 -lm -lc -lnsl\
#####	     @FLIBS@    @CFORLIB@
AM_LDFLAGS =  -L@HDF5@/lib -lhdf5 \
	@FLIBS@  @CFORLIB@

##we always use the c linker
F77LINK = $(LINK)



# This line ensures that generated moc_*.cpp and resources.cpp files are
# deleted when we make clean.
##CLEANFILES = $(filter moc_%.cpp,$(phaseqt_SOURCES)) 
## CLEANFILES is a variable called from target clean: rm -f $CLEANFILES
CLEANFILES = moc_*.cpp qrc_*.cpp
# end