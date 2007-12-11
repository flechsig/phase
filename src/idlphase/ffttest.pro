;+
; NAME: testnear.pro
;	
;   Tests the phase4idl-fftnear-Routine 

pro testnear,n,d

np=n_params()

waist=0.2
d0=5000
zmin1=-1
zmax1=2
ymin1=-2
ymax1=2
lambda=20

IF np gt 0 THEN nz1=n ELSE nz1=128
ny1=nz1

IF np gt 1 THEN distance=d ELSE distance=5000  

phainit

!P.Multi=[0,1,1]
!P.Multi=[0,2,2]

print,'---------------------------------------------------------------------------------------'
print,' testing phaSrcWFGauss + phaPropFFTnear '
print,'---------------------------------------------------------------------------------------'
print,'Quelle: w0 [mm] =',double(waist),' ::::: Wellenlaenge l [nm] =',double(lambda),'' 
print,'        nx = ny =',double(nz1)  ,' ::::: Kantenlaenge s [mm] =',double(2*zmax1),''
print,'         d [mm] =',double(distance) 
print,'---------------------------------------------------------------------------------------'



;---------------------------------------------------------------------------------------------
zeit=systime(1) ; "1" entspricht "/seconds"  
beam0=phaSrcWFGauss( nz1 , zmin1 , zmax1 , ny1, ymin1 , ymax1 , waist, d0 , lambda)
zeit=systime(1)-zeit
print,'       Gauss at 0 :: Max:', $
	   max(beam0.zezre^2+beam0.zezim^2+beam0.zeyre^2+beam0.zeyim^2,maxi), $
      ':: Tot:',beam0.dxezre*beam0.dyezre*total(beam0.zezre^2+beam0.zezim^2+beam0.zeyre^2+beam0.zeyim^2),$
	':: t[s]:', zeit
print,maxi mod 256,maxi/256
;phaIntensitySurface,beam0,'gauss at d0'
phaIntensityShade_Surf,beam0,'gauss at d0'
;iphaIntensitySurface,beam0,'gauss at d0'
;Surface,(beam0.zezre^2+beam0.zezim^2)
;---------------------------------------------------------------------------------------------



;---------------------------------------------------------------------------------------------
beam=phaSrcWFGauss( nz1 , zmin1 , zmax1 , ny1, ymin1 , ymax1 , waist, d0 , lambda)
zeit=systime(1) ; "1" entspricht "/seconds" 
phaPropFFTnear,beam,  distance
zeit=systime(1)-zeit

phasrc4addzeros,beam,256,256

print,' PropFFTnear at d :: Max:', $
	   max((beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2),maxi), $
      ':: Tot:',beam.dxezre*beam.dyezre*total(beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2),$
	':: t[s]:', zeit	
print,maxi mod 256,maxi/256

print,'Grid Borders:'
print,'x:',zmin1,' ,',zmax1 ,'  ->',beam.xezremin,' ,',beam.xezremax
print,'y:',ymin1,' ,',ymax1 ,'  ->',beam.yezremin,' ,',beam.yezremax

;phaIntensitySurface,beam,'propFFTnear at dist'

phaIntensityShade_Surf,beam,'propFFTnear at dist'

;iphaIntensitySurface,beam,'propFFTnear at dist'

;phaRealShade_Surf_Ez,beam,'REAL of propFFTnear'
;phaImagShade_Surf_Ez,beam,'IMAG of propFFTnear'
;---------------------------------------------------------------------------------------------


;phaIntensityShade_Surf,(phasrc4diff(beam0, beam)),'Difference ...'
;iphaIntensitySurface,(phasrc4diff(beam0, beam)),'Difference ...'


print,'---------------------------------------------------------------------------------------'
print,'-----------------------------------------------------------------------end-test-fftnear'
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;+
; NAME: testfar.pro
;	
;   Tests the phase4idl-fftfar-Routine 

pro testfar,n,d

np=n_params()

waist=0.2
d0=5000
zmin1=-1
zmax1=2
ymin1=-2
ymax1=2
lambda=20

IF np gt 0 THEN nz1=n ELSE nz1=128
ny1=nz1
;ny1=nz1-1

IF np gt 1 THEN distance=d ELSE distance=5000  

phainit

!P.Multi=[0,1,1]
!P.Multi=[0,2,2]

print,'---------------------------------------------------------------------------------------'
print,' testing phaSrcWFGauss + phaPropFFTfar '
print,'---------------------------------------------------------------------------------------'
print,'Quelle: w0 [mm] =',double(waist),' ::::: Wellenlaenge l [nm] =',double(lambda),'' 
print,'        nx = ny =',double(nz1)  ,' ::::: Kantenlaenge s [mm] =',double(2*zmax1),''
print,'         d [mm] =',double(distance) 
print,'---------------------------------------------------------------------------------------'



;---------------------------------------------------------------------------------------------
zeit=systime(1) ; "1" entspricht "/seconds"  
beam0=phaSrcWFGauss( nz1 , zmin1 , zmax1 , ny1, ymin1 , ymax1 , waist, d0 , lambda)
zeit=systime(1)-zeit
print,'       Gauss at 0 :: Max:', $
	   max(beam0.zezre^2+beam0.zezim^2+beam0.zeyre^2+beam0.zeyim^2,maxi), $
      ':: Tot:',beam0.dxezre*beam0.dyezre*total(beam0.zezre^2+beam0.zezim^2+beam0.zeyre^2+beam0.zeyim^2),$
	':: t[s]:', zeit
;print,maxi mod 256,maxi/256
;phaIntensitySurface,beam0,'gauss at d0'
phaIntensityShade_Surf,beam0,'gauss at d0'
;iphaIntensitySurface,beam0,'gauss at d0'
;Surface,(beam0.zezre^2+beam0.zezim^2)
;---------------------------------------------------------------------------------------------

;---------------------------------------------------------------------------------------------
beam=phaSrcWFGauss( nz1 , zmin1 , zmax1 , ny1, ymin1 , ymax1 , waist, d0 , lambda)

;beam.zezre(0:nz1-1,0:ny1-1)=cos(0.005*dindgen(nz1,ny1))
;beam.zezim(0:nz1-1,0:ny1-1)=cos(dblarr(nz1,ny1))
;print,'   cos(.005*dindgen(*)) ::               :: Tot:',beam.dxezre*beam.dyezre*total(beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2)
;phaIntensityShade_Surf,beam,'cos(.005*dindgen(*))'

zeit=systime(1) ; "1" entspricht "/seconds" 
phaPropFFTfar,beam,  distance
zeit=systime(1)-zeit

print,'  PropFFTfar at d :: Max:', $
	   max(beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2,maxi), $
      ':: Tot:',beam.dxezre*beam.dyezre*total(beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2),$
	':: t[s]:', zeit	
;print,maxi mod 256,maxi/256

print,'Grid Borders:'
print,'x:',zmin1,' ,',zmax1 ,'  ->',beam.xezremin,' ,',beam.xezremax
print,'y:',ymin1,' ,',ymax1 ,'  ->',beam.yezremin,' ,',beam.yezremax

;phaIntensitySurface,beam,'propFFTfar at dist'

phaIntensityShade_Surf,beam,'propFFTfar at dist'

;iphaIntensitySurface,beam,'propFFTfar at dist'

;phaRealShade_Surf_Ez,beam,'REAL of propFFTfar'
;phaImagShade_Surf_Ez,beam,'Imag of propFFTfar'
;---------------------------------------------------------------------------------------------

print,'---------------------------------------------------------------------------------------'
print,'------------------------------------------------------------------------end-test-fftfar'
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
