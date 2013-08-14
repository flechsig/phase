;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/phainit_defines.pro
;  Date      : <14 Aug 13 11:23:04 flechsig> 
;  Time-stamp: <14 Aug 13 11:23:57 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 


pro phainit_defines, verbose=verbose

if n_elements(verbose) ne 0 then print, 'phainit_defines called'

; defsysv, '!phalib', 'libphase4idl.so'		
; numbers are first defined in <phase.h>, see also <phasec.c>
defsysv,'!phaOptElGeneral'		,350
defsysv,'!phaOptElPlaneMirror'		,353
defsysv,'!phaOptElPlaneGrating'		,354
defsysv,'!phaOptElPlaneVLSGrating'	,355
defsysv,'!phaOptElSlit'			,99
defsysv,'!phaOptElDrift'		,999
defsysv,'!phaOptElToroidalMirror'	,66
defsysv,'!phaOptElToroidalGrating'	,67
defsysv,'!phaOptElToroidalVLSGrating'	,68
defsysv,'!phaOptElCone'			,351
defsysv,'!phaOptElEllipticalMirror'	,78
defsysv,'!phaOptElPlaneEllipticalMirror',79
defsysv,'!phaOptElPlaneEllipticalGrating',352


END
