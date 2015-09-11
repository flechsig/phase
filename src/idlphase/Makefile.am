# File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/Makefile.am
# Date      : <28 Aug 14 16:08:16 flechsig> 
# Time-stamp: <11 Sep 15 13:19:29 flechsig> 
# Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

# $Source$ 
# $Date$
# $Revision$ 
# $Author$ 

# ******************************************************************************
#
#   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
#                      Paul Scherrer Institut Villigen, Switzerland
#   
#   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
#          Uwe Flechsig,    uwe.flechsig@psi.ch
#          Torsten Leitner, Torsten.Leitner@email.de
#
# ------------------------------------------------------------------------------
#
#   This file is part of PHASE.
#
#   PHASE is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, version 3 of the License, or
#   (at your option) any later version.
#
#   PHASE is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
#
# ******************************************************************************


## define the directory where phase related idl functions should be installed 
## this directory should be in your idl search path

idldir   = $(prefix)/idl

idl_SCRIPTS = \
            phase4idl.sh \
            testphase4idl.sh

## all idl scripts with a doc header
idl_with_doc_header = \
	aperture.pro \
	crl.pro \
        crl_focus_1A.idl \
	test_crl.idl \
	double_slit.idl \
	drift.pro \
	driftfrauenhofer.pro \
	drift_crl_test.idl \
        drift_test.idl \
        emfield.pro \
	emfield2vars.pro \
        gaussbeam.pro \
	h5_check_type.pro \
        h5_plot.pro \
	h5_read.pro \
        h5_read_dataset.pro \
	h5_read_genesis.pro \
	h5_read_phase.pro \
	h5_read_pha4idl.pro \
	h5_read_surf_error.pro \
        h5_test.pro \
        h5_write_genesis.pro \
        h5_write_phase.pro \
        h5_write_surf_error.pro \
        initphase.pro \
	mywindow.pro \
        phaPutEMField.pro \
        phase__define.pro \
	phunwrap.pro myphunwrap.pro \
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
	poisson_spot_1A.idl \
	poisson_spot_vis.idl \
	propfourier.pro \
	propfresnel.pro \
	prophuygens.pro \
	readhenke.pro \
        readmaterial.pro \
	read_extr_out.pro \
        read_int_file.pro \
        read_opti_out.pro \
	reflec.pro \
        resize.pro \
        tag_exist.pro \
	unwrap_phase.pro myunwrap_phase.pro unwrap_herra.pro

## everything 2b installed in idldir
idl_DATA  = help.html \
	$(idl_with_doc_header) \
        ar1_offset_mirrors.idl \
        crl_focus_1A.idl \
	MeinSpieglein.coeff \
        mirror_test.idl \
        mirror_test_kb.idl \
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
	propagation_test.idl \
	examples.pro \
	ezre_gb_12.dat \
	ezim_gb_12.dat \
	test_5000.out-ezrec \
	test_5000.out-ezimc \
	test_5000.out-eyrec \
	test_5000.out-eyimc \
	test_fzp.idl \
	test_mirror.idl \
	test_mirrorg.idl \
	test_mirrorp.idl \
	test_mirrort.idl \
	test_phaseplate.idl \
        test_phaseplate_1.idl \
	pha4idlstart.idl 	

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

## the shared library for phase unwrapping
lib_LTLIBRARIES         = unwrap_herra.la c_c340_main.la
unwrap_herra_la_SOURCES = ../phaseqt/unwrap_phase.cpp ../phaseqt/unwrap_phase.h
unwrap_herra_la_LDFLAGS = -module -avoid-version -shared
c_c340_main_la_sources  = c_c340_main.c
c_c340_main_la_LDFLAGS = -module -avoid-version -shared
# end
