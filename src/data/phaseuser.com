$! File      : /home/vms/flechsig/vms/phas/phaseprg.dir/phaseuser.com
$! Date      : <15 Oct 98 08:45:48 flechsig> 
$! Time-stamp: <15 Oct 98 08:46:11 flechsig> 
$! Author    : Uwe Flechsig, flechsig@exp.bessy.de
$!
$! Datei: USERDISK_3:[FLECHSIG.PHASE]PHASEUSER.COM
$! Datum: 19.JUL.1994
$! Stand:  7-MAR-1996
$! Autor: FLECHSIG, BESSY Berlin
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$! example for an user routine, called by the  
$! 'Commands - User' button
$! arguments -> p1:	filename 	text file with parameter set
$! additional options are delivered by request
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
$! create a postscript file 
$ prog= p1
$ atops -p -optmp.ps 'prog'
$ write sys$output "ptmp.ps created!"   
$!!!!!!!!!!!!!!!!!!!!!!!   end of userfile      !!!!!!!!!!!!!!!!!!!!!!!!!!
