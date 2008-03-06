;+
; NAME: SharedLibAcessTests.pro
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro BatchModeNachbildung, cmode

np=n_params()
IF np LT 1  THEN BEGIN 
   print,' Using standard calculation mode: 1 - ray trace'
   cmode=1
ENDIF

cmode=long(cmode)

result = 1
result = call_external(!phalib,'test_batchmode_nachbildung',$
                       cmode, $
                       /I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

;print, 'fehler? ',result
			     
;return, result
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

