
;+
; NAME:
;	phaPropWFFresnelKirchhoff
;
; PURPOSE:
;       Propagate the fields in a pha4idl beamline structure 
;	with the Fresnel-Kirchhoff integration method.
;
; CATEGORY:
;	pha4idl - Free space propagator
;
; CALLING SEQUENCE:
;	phaPropWFFresnelKirchhoff, beam, distance, nz2, zmin2, zmax2, ny2, ymin2, ymax2 
;
; INPUTS:
;     	beam:		struct Source4 containing EM fields to propagate
;     	distance:	propagation distance [mm]
;
;	nz2:		imageplane: number of gridpoints in z-direction
;	zmin2:		imageplane: lower border in z-direction [mm]
;	zmax2:		imageplane: upper border in z-direction [mm]
;
;	ny2:		imageplane: number of gridpoints in y-direction
;	ymin2:		imageplane: lower border in y-direction [mm]
;	ymax2:		imageplane: upper border in y-direction [mm]
;
; OUTPUTS:
;     	beam:		initial fields are overwritten with the new values
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

