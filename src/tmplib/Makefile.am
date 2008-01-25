## Makefile template to build 
## libphase4idl.tmp.so
##
## Author: Torsten.Leitner@email.de
## 12/2007
##
##

lib_LTLIBRARIES = libphase4idl.tmp.la


libphase4idl_tmp_la_SOURCES   = \
        ../phase/phasec.c \
	  ../phase/activate_proc.c \
	  ../phase/phasegraf.c \
	  ../phase/initdatset.c \
	  ../phase/geometrypck.c \
	  ../phase/bline.c \
	  ../phase/phase0.c \
	  ../phase/error.c \
	  ../phase/xmalloc.c \
	  ../phase/xstrdup.c \
	  ../phase/mirrorpck.c \
	  ../phase/pst.c \
	  ../phase/cutils.c \
	  ../phase/rtrace.c \
	  ../phase/ctype.c \
	  ../phase/phasegraffor.F \
	  ../phase/phasefor.F \
	  ../phase/phase_source.F \
	  ../phase/phase_integration.F \
	  ../phase/misali.F \
	  ../phase/misali1.f \
	  ../phase/misali2.f \
	  ../phase/misali3.f \
	  ../phase/misali4.f \
	  ../phase/cutils.h \
	  ../phase/phase_struct.h \
	  ../phase/fg3pck.h \
	  ../phase/mirrorpck.h \
	  ../phase/geometrypck.h \
	  ../phase/phase.h \
	  ../phase/rtrace.h \
	  ../phase/error.h \
	  ../phase/common.h \
	  ../phase/version.h 
## phase.c 

##libphase4idl_la_SOURCES   = \
##               idl_phase_c.c \
##               pha_Drift.f \
##               pha_helper_functions.f \
##               pha_SrcWFGauss.f 
## benchmark.f \
## pha_Drift_1024.f \
## pha_Drift_256.f \
## phase4idl_fortran_master.f \
## phase4idl_fortran_master_standalone.f \
## test_phase.f \
## test_phase_gaussetc.f \

## ATnT_port3_driftroutines/ATnT_port3_driftroutines.for
## ---> these routines are included in libphase*.so


## flags
AM_CPPFLAGS = -DLINUX 

AM_LDFLAGS = @MOTIFLIBS@ $(X_LIBS) -lXm -lMrm -lXt -lXext -lXp -lX11 -lm -lc -lnsl\
	     @FLIBS@  -L@CERNLIB@ -lpawlib -lpacklib  \
             -lgraflib -lgrafX11 -lpacklib -lmathlib -lkernlib @CFORLIB@ @LINK_LIBPHASE@

## original fuer libphase4idl
## AM_LDFLAGS = @FLIBS@ @LINK_LIBPHASE@

## we want the c- Linker and not the fortran linker
libphase4idl_tmp_la_LINK = $(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) $(AM_CPPFLAGS) $(CFLAGS) \
	$(AM_LDFLAGS) $(LDFLAGS) -o $@




##we always use the c linker
F77LINK = $(LINK)


#pkgdata_DATA = ../data/coefficient-example.dat \
# ../data/higz_windows.dat\
# ../data/map35_lh.omx\
# ../data/map35_rh.omx\
# ../data/map70_lh.omx\
# ../data/map70_rh.omx\
# ../data/news\
# ../data/phase.dat\
# ../data/phase.hlb\
# ../data/phaseinit.com\
# ../data/phaseuser.com

# EXTRA_DIST = phase_struct.for $(pkgdata_DATA)

# end

