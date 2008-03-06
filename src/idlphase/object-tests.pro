;;; Object Tests

;;;; Standard Definition of Methods and Funktions for Objects :

; PRO ClassName::Method
;	statements
; END
;
; or
;
; FUNCTION ClassName::Method, Arguments
;	statements
; RETURN,value
; END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Test Methoden fuer Source4 ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO SOURCE4::ianzprint
  print,'ianzz:  ',self.iezrex,'    ianzy:  ',self.iezrey
END

pro SOURCE4::IntensityPlot

miny=dblarr(self.iezrey)
miny(*)=self.yezremin
y = miny + ((self.yezremax-self.yezremin)/(self.iezrey-1)) * dindgen(self.iezrey)
minz=dblarr(self.iezrex)
minz(*)=self.xezremin
z = minz + ((self.xezremax-self.xezremin)/(self.iezrex-1)) * dindgen(self.iezrex)


surface,((self.zezre^2+self.zezim^2+self.zeyre^2+self.zeyim^2)(0:self.iezrex-1,0:self.iezrey-1)),z,y,title=name
;shade_surf,((self.zezre^2+self.zezim^2+self.zeyre^2+self.zeyim^2)(0:self.iezrex-1,0:self.iezrey-1)),z,y,title=name

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


function SOURCE4::phaWFGauss, ianzz, zmin, zmax, $
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


;source= {source4}

result = 1
result = call_external(!phalib,'phaSrcWFGauss',$
			 self,  $
                         ianzz, zmin, zmax, $
			 ianzy, ymin, ymax, $
		          w0,   deltax, xlam, $
;
;			VALUE=[0,1,1,1,1,1,1,1,1,1], $  
; 
                       /I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

return, result
END
