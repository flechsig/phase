#  File      : /afs/psi.ch/user/f/flechsig/phase/src/data/phaseinit.csh
#  Date      : <14 Feb 06 08:32:48 flechsig> 
#  Time-stamp: <17 Jan 08 12:49:01 flechsig> 
#  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

# source this file to set the environment for phase (version for tcsh)
# eventually required if phase has been compiled with intel compiler @ PSI (using shared libs)

# test where_I_am
set at_psi=`/sbin/ifconfig | grep 129.129. | wc -m`
if ( $at_psi != '0' ) then
    set phase_home='/afs/psi.ch/project/phase'
    set path= ( ${phase_home}/bin $path ) 
    setenv LD_LIBRARY_PATH ${phase_home}/lib:/afs/psi.ch/sys/@sys/apps/intel-fc-6.0.1/compiler60/ia32/lib:$LD_LIBRARY_PATH
#    setenv LD_ASSUME_KERNEL 2.4.1
endif
