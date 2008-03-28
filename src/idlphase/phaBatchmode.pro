;
; NAME: phaBatchmode.pro
;

;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro phaBatchMode, BLfile, ResultFile, cmode
;+
; NAME:
;	phaBatchMode
;
; PURPOSE:
;       Start phase run in batchmode
;
; CATEGORY:
;	pro : pha4idl - run phase
;
; CALLING SEQUENCE:
;	phaBatchMode, BLfile, ResultFile, cmode
;
; INPUTS:
;     	BLfile:		name of the beamlinefile
;	ResultFile	prefix for the phase results
;			phase adds the following postfixes:
;			-ezre, -ezim, -eyre, -eyim  : real and imaginary 
;				of the EM-fields with z & y polarizaiton
;			-psd  : phase space density
;
;	cmode		calculation mode
;			1 : simple raytracing
;			2 : full raytracing
;			3 : phase space calculations
;			4 : footprint (not yet tested)
;			5 : multi freq. mode (not yet tested)
;
; OUTPUTS:
;     	ResultFile	results are stored to hdd
;
; KEYWORDS:
;	None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;      March 28, 2008, TL, added help
;
;-

CASE cmode OF
	-1	: Modus='-1'
	1	: Modus='1'
	2	: Modus='2'
	3	: Modus='3'
	4	: Modus='4'
ELSE	 : Modus='9'
ENDCASE



;; shellcommand='phase -b -fSGM.PHASE -oSGM.RESULT -m1'
shellcommand='phase -b'$
		+' -f'+BLfile $
		+' -o'+ResultFile  $
		+' -m'+Modus

print,'starting phasebatchmode with the following shellcommand:'
print,'$> '+shellcommand
;WaitForEnter

spawn,shellcommand ;,shelloutput

;shelloutput='phase : '+shelloutput
;print,shelloutput


END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

