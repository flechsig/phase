## Makefile template to copy idl functions to 
## $bindir/phase4idl/.
## 
##
## Author: Torsten.Leitner@email.de
## 12/2007
##
##

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

## define the directory where phase related idl functions should be installed 
## this directory should be in your idl search path

idldir   = $(prefix)/idl

idl_SCRIPTS = \
       phase4idl.sh \
       testphase4idl.sh

idl_DATA  = \
            read_extr_out.pro \
            read_int_file.pro \
            read_opti_out.pro \
            benchmark.pro \
            phabeamload.pro \
            pha_src4_functions.pro \
            c_test.pro \
            phainit.pro \
            phaSrcWFGauss.pro \
            phaplotting.pro \
            rtrace.pro \
            examples.pro \
            phaProp.pro \
            test.pro \
            ffttest.pro \
            object-tests.pro \
            phabeamcopy.pro \
            phabeamdiff.pro \
            phase_structures.pro \
            uwe_phase_beamline__define.pro \
            uwe_initbeamline.pro \
            uwe_README \
            phase4idl.compile_all.idl \
            phase4idl.compile-test-exit.idl

## help.html
## phase_structures_256.pro \
## phase_structures_1024.pro \

## target to automatically update the documentation 
## before installation (needs idl)
install-data-local:
	./updateidlphasehelp.csh

## idl_HTML = help.html


##

# end
