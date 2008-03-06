;+
; NAME:
;	beispiel

function phaSrcWFGauss, ianzz, zmin, zmax, $
			ianzy, ymin, ymax, $
                        w0  , deltax, xlam 

;e.g.... "IDL> beam0=phaSrcWFGauss(128, -1, 1,128, -1, 1, 0.2 , 0 , 20) "

np=n_params()
IF np NE 9 THEN BEGIN 
   print,''
   print,'Wrong Number of Arguments in function:  phaSrcWFGauss '
   print,'ArgCount is ',np,',    but should be    9.'
   print,'Returning to IDL-Level 0 (main) ...'
   print,''
   retall
ENDIF

  ;long(var): convert var to 32bit integer: 
  ;IDL_LONG - C int - ifort integer (*4) -- Standard

ianzy=long(ianzy) 
ianzz=long(ianzz)
ymin=double(ymin)
ymax=double(ymax)
;dy=(ymax-ymin)/double(ianzy)
zmin=double(zmin)
zmax=double(zmax)
;dz=(zmax-zmin)/double(ianzz)
xlam=double(xlam)
w0=double(w0)
deltax=double(deltax)


source= {source4}

result = 1
result = call_external(!phalib,'phaSrcWFGauss',$
			 source,  $
                         ianzz, zmin, zmax, $
			 ianzy, ymin, ymax, $
		          w0,   deltax, xlam, $
;
;			VALUE=[0,1,1,1,1,1,1,1,1,1], $  
; 
                       /I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

return, source
END
