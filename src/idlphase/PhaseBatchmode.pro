;+
; NAME: PhaseBatchmode.pro
;

;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro bb, cmode

;; test fuer die eigentlichen Batchmode-Routinen

np=n_params()
IF np LT 1  THEN BEGIN 
   print,' Using standard calculation mode: 1 - ray trace'
   cmode=1
ENDIF

PhaseBatchMode, 'SGM.PHASE', 'SGM.RESULT', cmode

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro PhaseBatchMode, inPHASEfile, ResultFile, cmode


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
		+' -f'+inPHASEfile $
		+' -o'+ResultFile  $
		+' -m'+Modus

;print,shellcommand
;WaitForEnter

spawn,shellcommand,shelloutput

shelloutput='phase : '+shelloutput
print,shelloutput


END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

