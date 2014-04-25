#  File      : /afs/psi.ch/user/f/flechsig/phase/src/data/phaseinit.csh
#  Date      : <14 Feb 06 08:32:48 flechsig> 
#  Time-stamp: <25 Apr 14 09:06:01 flechsig> 
#  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# example file
# source this file to set the environment for phase (version for bash)

export PHASE_HOME ${HOME}/phase
export PATH ${PHASE_HOME}/bin:${PATH}
export LD_LIBRARY_PATH ${PHASE_HOME}/lib::${LD_LIBRARY_PATH}
#export LD_ASSUME_KERNEL 2.4.1
