#  File      : /afs/psi.ch/user/f/flechsig/phase/src/data/phaseinit.csh
#  Date      : <14 Feb 06 08:32:48 flechsig> 
#  Time-stamp: <17 Jan 08 12:51:24 flechsig> 
#  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# source this file to set the environment for phase (version for bash)
# only required if phase has been compiled with intel compiler @ PSI (using shared libs)

export PHASE_HOME /afs/psi.ch/project/phase
export PATH $PHASE_HOME/bin:$PATH
export LD_LIBRARY_PATH $PHASE_HOME/lib:/afs/psi.ch/sys/@sys/apps/intel-fc-6.0.1/compiler60/ia32/lib:$LD_LIBRARY_PATH
#export LD_ASSUME_KERNEL 2.4.1
