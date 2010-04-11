##  $Source$ 
##  $Date$
##  $Revision$ 
##  $Author$
 
## Makefile template to build 
## libphase4idl.so
##
## Author: Torsten.Leitner@email.de
## 12/2007
##
##


#######################################################################
ORIGINALPHASExSOURCES   = \
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


## original phase flags
ORIGINALPHASExAMxCPPFLAGS = -DLINUX 

ORIGINALPHASExAMxLDFLAGS = @MOTIFLIBS@ $(X_LIBS) -lXm -lMrm -lXt -lXext -lXp -lX11 -lm -lc -lnsl\
	     @FLIBS@  -L@CERNLIB@ -lpawlib -lpacklib  \
             -lgraflib -lgrafX11 -lpacklib -lmathlib -lkernlib @CFORLIB@ @LINK_LIBPHASE@

#######################################################################

lib_LTLIBRARIES = libphase4idl.la

libphase4idl_la_SOURCES   = \
               idl_phase_batchmode_access.c \
               idl_phase_c.c \
               idl_bline.c \
               phase4idl.c \
               phaSrc4Drift.F \
               phaSrc4WFGauss.F \
               phaSrc4Tools.F \
               phase4idl.h \
               phaSrc4Drift_oe.F
##                \
##               $(ORIGINALPHASExSOURCES)

##
##               pha_Drift.F \
##                pha_Src4Drift.c \
##                pha_Src4WFGauss.c \
##               pha_helper_functions.F \
##               pha_Src4Tools.c \
## benchmark.f \
## phase4idl_fortran_master.f \
## phase4idl_fortran_master_standalone.f \
## test_phase.f \
## test_phase_gaussetc.f \

## ATnT_port3_driftroutines/ATnT_port3_driftroutines.for
## ---> these routines are included in libphase*.so

AM_CPPFLAGS= $(ORIGINALPHASExAMxCPPFLAGS)

AM_LDFLAGS = $(ORIGINALPHASExAMxLDFLAGS)

# only phase4idl ... 
#   AM_CPPFLAGS ... not defined here
#   AM_LDFLAGS = @FLIBS@ @LINK_LIBPHASE@ 

# -lphase4idl.tmp
# libphase4idl.tmp.so liegt im selben pfad wie LIBPHASE ...

## we want the c- Linker and not the fortran linker
libphase4idl_la_LINK = $(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) $(CFLAGS) \
	$(AM_LDFLAGS) $(LDFLAGS) -o $@
#	$(AM_LDFLAGS) $(LDFLAGS) -L/home/leitner/phase/lib -lphase4idl.tmp -o $@
##	$(AM_LDFLAGS) $(LDFLAGS) -L$(libdir) @LIBPHASE@ -o $@
##	$(AM_LDFLAGS) $(LDFLAGS) -o $@
## 	$(AM_LDFLAGS) $(LDFLAGS) -L/home/leitner/cvs/lib -lphaseifc -o $@

# end

##we always use the c linker
F77LINK = $(LINK)
# end
