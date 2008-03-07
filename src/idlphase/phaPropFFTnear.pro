
;+
; NAME:
;	phaPropFFTnear
;
; PURPOSE:
;       Propagate the fields in a pha4idl beamline structure with a nearfield fourier propagator.
;
; CATEGORY:
;	pha4idl - Free space propagator
;
; CALLING SEQUENCE:
;	phaPropFFTnear, beam, distance	 
;
; INPUTS:
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

;- -------------------------------------------------------------------------------------------------;
pro phaPropFFTnear, beam, distance	

lib= !phalib
func='phaPropFFTnear'

; beam is of type src4 

np=n_params()
IF np NE 2 THEN BEGIN 
   print,''
   print,'Wrong Number of Arguments in procedure:  phaPropFFTnear '
   print,'ArgCount is ',np,',    but should be      2.'
   print,'Returning to IDL-Level 0 (main) ...'
   print,''
   retall
ENDIF

distance=double(distance)


result=1
result = call_external(lib,func,$
			beam, $
			distance,  $
			/I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

;print, '***',result,'  ***'
END
;--------------------------------------------------------------------------------------------------;


