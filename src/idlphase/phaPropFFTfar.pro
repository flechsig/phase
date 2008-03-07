
;+
; NAME:
;	phaPropFFTfar
;
; PURPOSE:
;       Propagate the fields in a pha4idl beamline structure with a farfield fourier propagator.
;
; CATEGORY:
;	pha4idl - Free space propagator
;
; CALLING SEQUENCE:
;
;
; INPUTS:
;
;
; KEYWORDS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;      March 7, 2008, TL, added help
;
;-


;--------------------------------------------------------------------------------------------------;
pro phaPropFFTfar, beam, distance

lib= !phalib
func='phaPropFFTfar'

; beam is of type src4 

np=n_params()
IF np NE 2 THEN BEGIN 
   print,''
   print,'Wrong Number of Arguments in procedure:  phaPropFFTfar '
   print,'ArgCount is ',np,',    but should be     2.'
   print,'Returning to IDL-Level 0 (main) ...'
   print,''
   retall
ENDIF

distance=double(distance)


result=1
result = call_external( lib,func,$
			beam, $
			distance,  $
			/I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

;print, '***',result,'  ***'
END
;--------------------------------------------------------------------------------------------------;


