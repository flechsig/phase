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
PORTLIBxSOURCES = ../phase4idl/ATnT_port3_driftroutines/drift_routines_02.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_03.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_04.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_05.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_06.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_07.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_08.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_09.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_10.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_12.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_13.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_14.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_15.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_16.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_17.f \
../phase4idl/ATnT_port3_driftroutines/drift_routines_18_no_machine_consts.f \
../phase4idl/ATnT_port3_driftroutines/port/s1mach.f \
../phase4idl/ATnT_port3_driftroutines/port/s2mach.f \
../phase4idl/ATnT_port3_driftroutines/port/s3mach.f \
../phase4idl/ATnT_port3_driftroutines/port/r1mach.f \
../phase4idl/ATnT_port3_driftroutines/port/d1mach.f \
../phase4idl/ATnT_port3_driftroutines/port/i1mach.f \
../phase4idl/ATnT_port3_driftroutines/port/s88fmt.f

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

ORIGINALPHASExAMxLDFLAGS = -shared  $(X_LIBS) -lm -lc -lnsl\
	     @FLIBS@ @CFORLIB@ @LINK_LIBPHASE@

#######################################################################

lib_LTLIBRARIES = libphase4idl.la

libphase4idl_la_SOURCES   = \
		phase4idl.c \
		phaSrc4Drift.F \
		phaSrc4WFGauss.F \
		phaSrc4Tools.F \
		phase4idl.h \
		phaSrc4Drift_oe.F $(PORTLIBxSOURCES)

##     idl_phase_batchmode_access.c \
##               idl_phase_c.c \
 ##              idl_bline.c \
##                \
##               $(ORIGINALPHASExSOURCES)

##
##               pha_Drift.F \
##                pha_Src4Drift.c \
##                pha_Src4WFGauss.c \
##               pha_helper_functions.F \
##               pha_Src4Tools.c \
## phase4idl_fortran_master.f \
## phase4idl_fortran_master_standalone.f \

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
