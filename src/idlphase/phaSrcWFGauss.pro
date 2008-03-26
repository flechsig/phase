;+
; NAME:
;	beispiel

function phaSrcWFGauss, ianzz, zmin, zmax, $
			ianzy, ymin, ymax, $
                        w0  , deltax, xlam, $
                        ez0, ey0, dphi_zy



;e.g.... "IDL> beam0=phaSrcWFGauss(128, -1, 1,128, -1, 1, 0.2 , 0 , 20, 1,0,0) "

np=n_params()
IF np NE 12 THEN BEGIN 
   print,''
   print,'Wrong Number of Arguments in function:  phaSrcWFGauss '
   print,'ArgCount is ',np,',    but should be    12.'
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
ez0=double(ez0)
ey0=double(ey0)
dphi_zy=double(dphi_zy)

source= {source4}

result = 1
result = call_external(!phalib,'phaSrcWFGauss',$
			 source,  $
                         ianzz, zmin, zmax, $
			 ianzy, ymin, ymax, $
		         w0,  deltax, xlam, $
                         ez0, ey0, dphi_zy, $
                       /I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

return, source
END
