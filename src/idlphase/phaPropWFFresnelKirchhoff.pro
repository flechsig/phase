
;+
; NAME:
;	phaPropWFFresnelKirchhoff
;
; PURPOSE:
;       Propagate the fields in a pha4idl beamline structure with the Fresnel-Kirchhoff integration method.
;
; CATEGORY:
;	pha4idl - Free space propagator
;
; CALLING SEQUENCE:
;	phaPropWFFresnelKirchhoff, beam, distance, nz2, zmin2, zmax2, ny2, ymin2, ymax2 
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
pro phaPropWFFresnelKirchhoff, beam, distance, nz2, zmin2, zmax2, ny2, ymin2, ymax2 
; beam is of type src4 

lib= !phalib
func='phaPropWFFresnelKirchhoff'

np=n_params()
IF np NE 8 THEN BEGIN 
   print,''
   print,'Wrong Number of Arguments in procedure:  phaPropWFFresnelKirchhoff '
   print,'ArgCount is ',np,',    but should be     8.'
   print,'Returning to IDL-Level 0 (main) ...'
   print,''
   retall
ENDIF

nz2=long(nz2)
zmin2=double(zmin2)
zmax2=double(zmax2)
ny2=long(ny2)
ymin2=double(ymin2)
ymax2=double(ymax2)
distance=double(distance)

result=1
result = call_external( lib,func,$
			beam, distance, $
			nz2, zmin2, zmax2, $
			ny2, ymin2, ymax2, $
			/I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

;print, '***',result,'  ***'
END
;--------------------------------------------------------------------------------------------------;

