#  File      : /afs/psi.ch/user/f/flechsig/phase/src/data/phaseinit.csh
#  Date      : <14 Feb 06 08:32:48 flechsig> 
#  Time-stamp: <25 Apr 14 09:07:32 flechsig> 
#  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

#example
# source this file to set the environment for phase (version for tcsh)

# test where_I_am
set at_psi=`/sbin/ifconfig | grep 129.129. | wc -m`
if ( $at_psi != '0' ) then
    set phase_home='$home/phase'
    set path= ( ${phase_home}/bin $path ) 
    setenv LD_LIBRARY_PATH ${phase_home}/lib:$LD_LIBRARY_PATH
#    setenv LD_ASSUME_KERNEL 2.4.1
endif
