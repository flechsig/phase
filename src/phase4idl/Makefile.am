## Makefile template to build 
## libphase4idl.so
##
## Author: Torsten.Leitner@email.de
## 12/2007
##
##

lib_LTLIBRARIES = libphase4idl.la

libphase4idl_la_SOURCES   = \
               idl_phase_c.c \
               idl_phase_batchmode_access.c \
               pha_Drift.f \
               pha_helper_functions.f \
               pha_SrcWFGauss.f 

## benchmark.f \
## pha_Drift_1024.f \
## pha_Drift_256.f \
## phase4idl_fortran_master.f \
## phase4idl_fortran_master_standalone.f \
## test_phase.f \
## test_phase_gaussetc.f \

## ATnT_port3_driftroutines/ATnT_port3_driftroutines.for
## ---> these routines are included in libphase*.so



AM_LDFLAGS = @FLIBS@ @LINK_LIBPHASE@ -lphase4idl.tmp
# libphase4idl.tmp.so liegt im selben pfad wie LIBPHASE ...

## we want the c- Linker and not the fortran linker
libphase4idl_la_LINK = $(LIBTOOL) --mode=link $(CC) $(AM_CFLAGS) $(CFLAGS) \
	$(AM_LDFLAGS) $(LDFLAGS) -o $@
##	$(AM_LDFLAGS) $(LDFLAGS) -L$(libdir) @LIBPHASE@ -o $@
##	$(AM_LDFLAGS) $(LDFLAGS) -o $@
## 	$(AM_LDFLAGS) $(LDFLAGS) -L/home/leitner/cvs/lib -lphaseifc -o $@

# end

##we always use the c linker
F77LINK = $(LINK)
# end
