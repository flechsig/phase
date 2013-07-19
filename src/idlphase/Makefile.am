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

## all idl scripts with a doc header
idl_with_doc_header = \
	crl.pro \
	crl_test.idl \
	drift.pro \
	drift_crl_test.idl \
        drift_test.idl \
        emf_statistics.pro \
	h5_check_type.pro \
        h5_plot.pro \
	h5_read.pro \
        h5_read_dataset.pro \
	h5_read_genesis.pro \
	h5_read_phase.pro \
        h5_test.pro \
        h5_write_genesis.pro \
        h5_write_phase.pro \
	plot2d.pro \
	plotdivergence.pro  \
	plotfootprint.pro  \
	plotfootprint_simple.pro  \
	plothdf5_genesis_source.pro  \
	plothdf5_phase_source.pro  \
	plothdf5.pro  \
	plothdf5_pst.pro  \
	plotphase.pro  \
	plotsi.pro  \
	read_extr_out.pro \
        read_int_file.pro \
        read_opti_out.pro \
        tag_exist.pro

## everything 2b installed in idldir
idl_DATA  = help.html \
	$(idl_with_doc_header) \
	MeinSpieglein.coeff \
	pha4idl.compile_all.idl \
	phaBatchmode.pro \
	phaBeamline.pro \
	phaExamples.pro \
	phaOpticalElements.pro \
	phaPropagation.pro \
	phaSetBeamlineOptions.pro \
	phaSetSources.pro \
	phaSrcWFGauss.pro \
	phaUsefullTools.pro \
	phaModGrid.pro \
	phainit.pro \
	phaplotting.pro \
	phainit_defines.pro \
	phainit_structures.pro \
        phaLoadEMField.pro \
	phaSaveEMField.pro \
	read_extr_out.pro \
	read_int_file.pro \
	read_opti_out.pro


###
help.html: $(idl_with_doc_header) 
	./updateidlphasehelp.csh	

## phase_structures_256.pro \
## phase_structures_1024.pro \

## target to automatically update the documentation 
## before installation (needs idl)
##install-data-local:
##	./updateidlphasehelp.csh

## idl_HTML = help.html


##

# end
