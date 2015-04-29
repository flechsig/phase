#!/bin/bash
# File      : /afs/psi.ch/user/f/flechsig/phase/src/phasempi/phasempiexample.sh
# Date      : <30 Apr 14 10:26:54 flechsig> 
# Time-stamp: <29 Apr 15 14:54:19 flechsig> 
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
#   along with PHASE (src/LICENSE).  If not, see <http:#www.gnu.org/licenses/>. 
#
# ******************************************************************************


## phasempi example script for merlin cluster at PSI

# xhpl_pe_openmpi_using_openmpi-1.5.4-gcc-4.6.1-mkl-10.sge
#
# This script uses the parallel environment (PE) "openmpi" with an explicit machinefile.
# This script must be used with qsub command - do NOT run it as a stand-alone
# script unless NSLOTS and TMPDIR/machines are properly set for the MPI command MPICMD
# and PE_HOSTFILE is set to an empty file.

# Define your job name, parallel environment with the number of slots, and run time:
# UF this lines are not comments
#$ -cwd
#$ -N phasempi 
#$ -pe openmpi 45
#$ -l s_rt=07:30:30,h_rt=7:30:30

###################################################
# Fix the SGE environment-handling bug (bash):
source /usr/share/Modules/init/sh
export -n -f module

# Load the environment modules for this job (the order may be important):
module add mpi

# No modules are loaded explicitly - set the environment below.
###################################################
# Set the environment variables:
MPIEXEC=$OPENMPI/bin/mpiexec
# OPENMPI is set by the mpi/openmpi-1.4.3-intel-12.1 module.
export OMP_NUM_THREADS=1

# Not using "-x OMPI_MCA_btl" in mpiexec because it is configured in ~/.openmpi/mca-params.conf.
# export OMPI_MCA_btl='openib,sm,self'
# export OMPI_MCA_orte_process_binding=core

##############
# BEGIN DEBUG
# Print the SGE environment on master host:
echo "================================================================"
echo "=== SGE job  JOB_NAME=$JOB_NAME  JOB_ID=$JOB_ID"
echo "================================================================"
echo DATE=`date`
echo HOSTNAME=`hostname`
echo PWD=`pwd`
echo "NSLOTS=$NSLOTS"
echo "PE_HOSTFILE=$PE_HOSTFILE"
cat $PE_HOSTFILE
echo "Machinefile created by openmpi PE $TMPDIR/machines:"
cat $TMPDIR/machines
echo "================================================================"
echo "Running environment:"
env
echo "================================================================"
echo "Loaded environment modules:"
module list 2>&1
echo
# END DEBUG
##############

###################################################
# The command to run with mpiexec:
# CMD=/bin/hostname
# CMD=$HOME/bin/xhpl-OpenMPI_Intel-12.1
# CMD=/gpfs/home/sge/examples/merlin4/bin/xhpl-OpenMPI_GCC_MKL10
# UF CMD=$HOME/bin/xhpl-OpenMPI15_GCC46_MKL10
CMD=/gpfs/home/flechsig/phase/bin/phasempi
ARGS='-oaramis12_0.1nm_po_13e.h5 -O2 aramis12_0.1nm_po_13e.phase'

#echo "=== HPL BENCHMARK ========================================================="
#if [ -f HPL.dat ]; then
#   echo "=== HPL.dat ============================================================"
#   cat HPL.dat
#   echo "========================================================================"
#else
#   echo "ERROR: cannot find HPL.dat - EXIT"
#   exit 2
#fi

##############
# BEGIN DEBUG
# Check that the libraries are available (on the master host):
echo "ldd $CMD"
ldd $CMD
echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
# Check the number of threads used by OpenMP:
echo "OMP_NUM_THREADS=$OMP_NUM_THREADS"
# END DEBUG
##############

# The MPI command to run:
MPICMD="$MPIEXEC --prefix $OPENMPI -x LD_LIBRARY_PATH -x OMP_NUM_THREADS -np $NSLOTS -machinefile $TMPDIR/machines $CMD $ARGS"
echo "Command to run:"
echo "$MPICMD"
echo
$MPICMD

################################################################################