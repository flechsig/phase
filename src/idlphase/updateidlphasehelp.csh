#! /bin/tcsh
#  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/updateidlphasehelp.sh
#  Date      : <25 Jan 08 13:44:04 flechsig> 
#  Time-stamp: <25 Jan 08 18:17:19 flechsig> 
#  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

echo "you may have an IDL problem if you read this" > help.html
unalias idl
unsetenv IDL_STARTUP
idl updatehelp.idl
echo 'help.html updated!'


