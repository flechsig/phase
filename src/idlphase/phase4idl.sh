#!/bin/sh
# Startup Script for Phase4IDL
PATH=/bin:/usr/bin:/usr/local/bin
export PATH

declare -x LANG="C"
declare -x PHASE_HOME=$PHASE_HOME/phase
declare -x LD_LIBRARY_PATH=$PHASE_HOME"/lib":$LD_LIBRARY_PATH


cd $PHASE_HOME/idl

idl "pha4idl.compile_all.idl"

# -path "$PHASE_HOME/idl"

