$! Datei: USERDISK_3:[FLECHSIG.PHAS.PHASEPRG]PHASEINIT.COM
$! Datum: 16.FEB.1995
$! Stand: 27-MAR-1997
$! Autor: FLECHSIG, BESSY Berlin
$!
$! setup programm for PHASE
$!*****************************************************************************
$! default paths... 
$! 
$ myphasedir = f$trnlnm("SYS$LOGIN") - "]"             	  ! HOME directory
$! 
$ define PHASE$DISK  "''myphasedir'.phase]"            	  ! working directory 
$ define PHASE$LIB   "''myphasedir'.phase.lib]"           ! lib directory
$ define OPTI$PRG    "''myphasedir'.phase.opti]"          ! Optimization progr. 
$ define EXTR$PRG    "''myphasedir'.phase.extr]"          ! Extraction   progr. 
$!*****************************************************************************
$ define PHASE$PRG   "PRG_ROOT:[PHASE]"                   ! Programs    
$!****************************************************************************  
$! if you do not build your own optimization/extraction programs
$! remove the comments at the next lines 
$!
$! define/nolog OPTI$PRG   "PRG_ROOT:[PHASE]"
$! define/nolog EXTR$PRG   "PRG_ROOT:[PHASE]"
$!****************************************************************************
$!
$ PHASEusercom  :== "@PHASE$DISK:PHASEuser.com"          ! User program
$!
$!*****************************************************************************
$! we use different extensions for VAX and AXP systems
$! so we can keep both versions in the same path
$!
$ arch_name = f$getsyi("ARCH_NAME")
$ if arch_name .eqs. "Alpha"
$ then                  
$ 	PHASE	        :== "$PHASE$prg:PHASE.exe_axp"
$ 	phaseopti	:== "$OPTI$prg:phaseopti.exe_axp"  
$ 	phaseextract	:== "$EXTR$prg:phaseextract.exe_axp"     
$    	write sys$output "phaseinit for Alpha"
$ else
$ 	PHASE         	:== "$PHASE$prg:PHASE.exe"
$ 	phaseopti	:== "$OPTI$prg:phaseopti.exe"  
$ 	phaseextract	:== "$EXTR$prg:phaseextract.exe"     
$    	write sys$output "phaseinit for VAX"
$ endif
$ write sys$output "PHASE- setup completed"
$!*****************************************************************************
