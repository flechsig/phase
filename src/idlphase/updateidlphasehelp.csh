#! /bin/tcsh
#  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/updateidlphasehelp.sh
#  Date      : <25 Jan 08 13:44:04 flechsig> 
#  Time-stamp: <25 Jan 08 13:55:27 flechsig> 
#  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

unalias idl
unsetenv IDL_STARTUP
idl updatehelp.idl
echo 'help.html updated!'
