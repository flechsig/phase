;+
; NAME: PhaseBatchmode.pro
;

;
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

spawn,shellcommand ;,shelloutput

;shelloutput='phase : '+shelloutput
;print,shelloutput


END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro PhaseCopyFieldFiles, src, dest

; infiles nach tmpfiles kopieren
spawn,'cp -f --reply=yes '+src+'-eyrec '+dest+'-eyrec'
spawn,'cp -f --reply=yes '+src+'-eyimc '+dest+'-eyimc'
spawn,'cp -f --reply=yes '+src+'-ezrec '+dest+'-ezrec'
spawn,'cp -f --reply=yes '+src+'-ezimc '+dest+'-ezimc'

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro PhaseMoveFieldFiles, src, dest

; tmpfiles nach outfiles kopieren
spawn,'mv -f --reply=yes '+src+'-eyrec '+dest+'-eyrec'
spawn,'mv -f --reply=yes '+src+'-eyimc '+dest+'-eyimc'
spawn,'mv -f --reply=yes '+src+'-ezrec '+dest+'-ezrec'
spawn,'mv -f --reply=yes '+src+'-ezimc '+dest+'-ezimc'
                         
                         
END                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



