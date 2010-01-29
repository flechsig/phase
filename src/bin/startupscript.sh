#!/bin/sh

# (c) 2009 : torsten.leitner@helmholtz-berlin.de

PATH=/bin:/usr/bin:/usr/local/bin
export PATH

export LANG="C"
export PHASE_HOME=$HOME/myphase

### give preference to user's custom lib paths (does not work on 64-bit)
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PHASE_HOME"/lib"
### (on 64Bit): export LD_LIBRARY_PATH=$PHASE_HOME"/lib":$LD_LIBRARY_PATH
echo "LD_LIBRARY_PATH = " $LD_LIBRARY_PATH

#
# ort von phase binary in idl angeben... wegen korrektem batchmode access
# . $PHASE_HOME/phase params
#
# oder: alle parameter and phase direkt weiterleiten, falls kein keyword
#   ..............   $@   ...............
#
#

############## funcs ##########################

 usage()
{
   echo "*** USAGE ***"
   echo ""
   echo " phase         : start phase in Motif mode"
   echo " phase idl     : start phase4idl (idl must be installed)"
   echo " phase -b      : start phase in batchmode (see below,-b)"
   echo ""
   echo " phase idlhelp : show phase4idl's help.html in firefox"
   echo " phase help"
   echo " phase usage"
   echo " phase -h      : shows this usage"
   echo ""
   return
}

 runphase4idl()
{
   export OLD_CALLDIR=$PWD
   cd $PHASE_HOME/idl
   exec idl "pha4idl.compile_all.idl"
   ### may need full absolute path to idl executable
   ### (for 64Bit) exec /nidl -32 "pha4idl.compile_all.idl"
  
   return
}

 runidlhelp()
{
   # start firefox as subprocess with $PHASE_HOME/idl/help.html
   firefox $PHASE_HOME/idl/help.html &
   return
}

runphase()
{
   exec $PHASE_HOME/bin/phase $@
   return
}

############## script #########################


if   [ $# -lt 1 ]; then
  runphase
elif [ $1 = "-h" ]; then
 usage
  echo "Motif Phase's help:"
  runphase $@
elif [ $1 = "-b" ]; then
  runphase $@

elif [ $1 = "idl" ]; then
  runphase4idl

elif [ $1 = "idlhelp" ]; then
  runidlhelp

elif [ $1 = "help" ]; then
  usage
  echo "Motif Phase's help:"
  runphase -h
elif [ $1 = "usage" ]; then
  usage
  echo "Motif Phase's help:"
  runphase -h

else
  echo "Oops! unknown parameter: $@"
  echo ""
  usage
fi
