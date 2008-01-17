## Makefile template to build 
## libphase4idl.so
##
## Author: Torsten.Leitner@email.de
## 12/2007
##
##


## we use an AM_CONDITIONAL to switch library names for different compilers
PHASELIB = 
if WANT_ABSOFT
   PHASELIB += lphaseabsoft
endif
if WANT_IFC
   PHASELIB += lphaseifc
##   AM_FFLAGS        = -O0                ## no optimization
endif
if WANT_PORTLAND
   PHASELIB += lphaseportland
endif




COMMONSOURCES= idl_phase_c.c \
               pha_Drift.f \
               pha_helper_functions.f \
               pha_SrcWFGauss.f \
               ATnT_port3_driftroutines/ATnT_port3_driftroutines.for


## benchmark.f \
## pha_Drift_1024.f \
## pha_Drift_256.f \
## phase4idl_fortran_master.f \
## phase4idl_fortran_master_standalone.f \
## test_phase.f \
## test_phase_gaussetc.f \

## end common sources

## we use an AM_CONDITIONAL to switch library names for different compilers
##lib_LTLIBRARIES = 
lib_LTLIBRARIES = libphase4idl.la

libphase4idl_la_SOURCES   = $(COMMONSOURCES)

# libphase4idl_la_LIBADD = $(libdir)/@LIBPHASE@
# libphase4idl_LDADD = $(libdir)/@LIBPHASE@


## libtool versions (optional)
## libphaseabsoft_la_LDFLAGS =  -version-info 3:12:1 
## libphaseifc_la_LDFLAGS =  -version-info 3:12:1

AM_LDFLAGS = @FLIBS@ 
#@LIBPHASE@

## L$(libdir) -lphaseifc

## -$(PHASELIB)

## we want the c- Linker and not the fortran linker
libphase4idl_la_LINK = $(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) $(CFLAGS) \
	$(AM_LDFLAGS) $(LDFLAGS) -o $@

# end

##we always use the c linker
F77LINK = $(LINK)
# end
