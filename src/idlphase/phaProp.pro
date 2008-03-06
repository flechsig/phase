;+
; NAME: phaProp.pro
;
; Included pro.s: 
;	phaPropFFTnear	
;	phaPropFFTfar	
;	phaPropWFFresnelKirchhoff
;
;
;--------------------------------------------------------------------------------------------------;
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
