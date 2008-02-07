;+
; NAME: test.pro
;	
;   Tests the phase4idl-Routines


PRO tst

;Test SavePhaseResults.pro


nz = 128
ny = nz

zmin = 0.1
zmax = zmin

ymin = zmin 
ymax = zmin 

waist  = 0.2
focus  = 0.
lambda = 60.



filename='tstSaveResults'

beam = {source4}

beam=phaSrcWFGauss( nz , zmin , zmax , ny, ymin , ymax , waist, focus , lambda)


SavePhaseResults,beam,filename

END ;tst








; teste write beamlinefile
PRO WBLF

bl = { BeamlineType }

;bl.ElementList:cptr ,$;Hier wird auf Struktur gezeigt...  *ElementList:{ElementType} 
;dummy_ElementType = { ElementType , $
;  ElementOK:intzero , $
;  matrix:MAP70TYPE, MtoSource:MAP70TYPE, $
;  ypc1:MAP7TYPE, zpc1:MAP7TYPE, dypc:MAP7TYPE, dzpc:MAP7TYPE, wc:MAP7TYPE, xlc:MAP7TYPE, $ 
;  xlm:{xlenmaptype}, $ 
;  mir:{mirrortype}, $ 
;  MDat:{mdatset},  $                          
;  geo:{geometrytype},  $ 
;  GDat:{gdatset}, $ 
;  elementname:bytarr(MaxPathLength) $
;};ElementType


fnamein = bytarr(1)
fnamein = byte('test.bl.in')


fnameout = bytarr(1)
fnameout = byte('test.bl.out')

print,string(fnamein)
print,string(fnameout)


result = 1
result = call_external(!phalib,'testReadBeamLineFile',$
                        fnamein, bl ,$
                       /I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

print, 'fehler? ',result


result = 1
result = call_external(!phalib,'testWriteBeamLineFile',$
                        fnameout, bl ,$
                       /I_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

print, 'fehler? ',result

			     
			     
;return, result

END
























pro testprop,n,d

np=n_params()

IF np gt 0 THEN nz1=n else nz1=128
IF np gt 1 THEN distance=d else distance=5000  



phainit

!P.Multi=[0,1,2]

nz2=nz1
ny1=nz1
ny2=nz2
waist=0.2
d0=0
zmin1=-1
ymin1=zmin1
zmax1=-zmin1
ymax1=-ymin1
zmin2=zmin1
ymin2=zmin2
zmax2=-zmin2
ymax2=-ymin2
lambda=20

print,'---------------------------------------------------------------------------------------'
print,' testing phaSrcWFGauss + phaPropWFFresnelKirchhoff '
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
print,maxi,maxi mod 256,maxi/256
;phaIntensitySurface,beam0,'gauss at d=0'
phaIntensityShade_Surf,beam0,'gauss at d=0'
;iphaIntensitySurface,beam0,'gauss at d=0'
;Surface,(beam0.zezre^2+beam0.zezim^2)
;---------------------------------------------------------------------------------------------


;---------------------------------------------------------------------------------------------
;zeit=systime(1) ; "1" entspricht "/seconds" 
;gauss=phaSrcWFGauss( nz2 , zmin2 , zmax2 , ny2, ymin2 , ymax2 , waist, distance , lambda)
;zeit=systime(1)-zeit
;print,'       Gauss at d :: Max:' , $
;                 max(gauss.zezre^2+gauss.zezim^2+gauss.zeyre^2+gauss.zeyim^2), $
;      ':: Tot:',gauss.dxezre*gauss.dyezre*total(gauss.zezre^2+gauss.zezim^2+gauss.zeyre^2+gauss.zeyim^2),$
;	':: t[s]:', zeit	
;phaIntensitySurface,gauss,'gauss at dist'
;phaIntensityShade_Surf,gauss,'gauss at dist'
;iphaIntensitySurface,gauss,'gauss at dist'
;---------------------------------------------------------------------------------------------


;---------------------------------------------------------------------------------------------
;beam=phaSrcWFGauss( nz1 , zmin1 , zmax1 , ny1, ymin1 , ymax1 , waist, d0 , lambda)
;zeit=systime(1) ; "1" entspricht "/seconds" 
;phaPropWFFresnelKirchhoff,beam, distance,nz2, zmin2, zmax2, ny2, ymin2, ymax2
;zeit=systime(1)-zeit
;print,'     Prop FK at d :: Max:', $
;        	max(beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2), $
;      ':: Tot:',beam.dxezre*beam.dyezre*total(beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2),$
;	':: t[s]:', zeit	
;phaIntensitySurface,beam,'prop FK at dist'
;phaIntensityShade_Surf,beam,'prop FK at dist'
;iphaIntensitySurface,beam,'prop FK at dist'
;---------------------------------------------------------------------------------------------

;---------------------------------------------------------------------------------------------
beam=phaSrcWFGauss( nz1 , zmin1 , zmax1 , ny1, ymin1 , ymax1 , waist, d0 , lambda)
zeit=systime(1) ; "1" entspricht "/seconds" 
phaPropFFTnear,beam,  distance
zeit=systime(1)-zeit
print,' PropFFTnear at d :: Max:', $
	   max(beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2), $
      ':: Tot:',beam.dxezre*beam.dyezre*total(beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2),$
	':: t[s]:', zeit	
;phaIntensitySurface,beam,'propFFTnear at dist'
;phaIntensityShade_Surf,beam,'propFFTnear at dist'
;iphaIntensitySurface,beam,'propFFTnear at dist'
phaRealShade_surf_ez,beam,'REAL of propFFTnear'
;---------------------------------------------------------------------------------------------

;---------------------------------------------------------------------------------------------
;beam=phaSrcWFGauss( nz1 , zmin1 , zmax1 , ny1, ymin1 , ymax1 , waist, d0 , lambda)
;zeit=systime(1) ; "1" entspricht "/seconds" 
;phaPropFFTfar, beam,  distance
;zeit=systime(1)-zeit
;print,' PropFFT far at d :: Max:', $
;	   max(beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2), $
;      ':: Tot:',beam.dxezre*beam.dyezre*total(beam.zezre^2+beam.zezim^2+beam.zeyre^2+beam.zeyim^2),$
;	':: t[s]:', zeit	
;phaIntensityShade_Surf,beam,'propFFTfar at dist'
;iphaIntensitySurface,beam,'propFFTfar at dist'
;---------------------------------------------------------------------------------------------


;phaIntensityShade_Surf,(phasrc4diff(beam, beam0)),'Difference ...'
;iphaIntensitySurface,(phasrc4diff(beam0, beam)),'Difference ...'


;print,'---------------------------------------------------------------------------------------'
print,'-------------------------------------------------------------------------------end-test'
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


pro test

;np=n_params()

;IF np gt 0 THEN nz1=n else nz1=128
; IF np gt 1 THEN distance=d else distance=5000  

phainit

!P.Multi=[0,2,2]

nz1=128
ny1=nz1
waist=0.2d0
lambda=20
d0=0.d0
distance=5000.d0
zmin1=-1.d0
ymin1=zmin1
zmax1=-zmin1
ymax1=-ymin1

beam0=phaSrcWFGauss( nz1 , zmin1 , zmax1 , ny1, ymin1 , ymax1 , waist, d0 , lambda)

phaIntensitySurface,beam0,'gauss original'

print,'       Gauss at 0 :: Max:', $
	   max(beam0.zezre^2+beam0.zezim^2+beam0.zeyre^2+beam0.zeyim^2,maxi), $
      ':: Tot:',beam0.dxezre*beam0.dyezre*total(beam0.zezre^2+beam0.zezim^2+beam0.zeyre^2+beam0.zeyim^2)


phaModSizeCut,beam0,0,150,-3,160

phaIntensitySurface,beam0,'gauss cutted'

print,'       Gauss Cut  :: Max:', $
	   max(beam0.zezre^2+beam0.zezim^2+beam0.zeyre^2+beam0.zeyim^2,maxi), $
      ':: Tot:',beam0.dxezre*beam0.dyezre*total(beam0.zezre^2+beam0.zezim^2+beam0.zeyre^2+beam0.zeyim^2)


phaModSizeAddZeros,beam0,10,18
phaIntensitySurface,beam0,'gauss cutted + addzero '

print,'       Gauss Add0 :: Max:', $
	   max(beam0.zezre^2+beam0.zezim^2+beam0.zeyre^2+beam0.zeyim^2,maxi), $
      ':: Tot:',beam0.dxezre*beam0.dyezre*total(beam0.zezre^2+beam0.zezim^2+beam0.zeyre^2+beam0.zeyim^2)



END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro testgauss,ngrid,dxy,waist

;np=n_params()

;IF np gt 0 THEN nz1=n else nz1=128
; IF np gt 1 THEN distance=d else distance=5000  

phainit

!P.Multi=[0,1,1]

dxy=1000*dxy ; m to mm
waist= 1000*waist ; m to mm
nz1=ngrid
ny1=ngrid
;;;;waist=0.2d0
lambda=20.51
d0=0.d0
distance=5000.d0
zmin1=-dxy*(ngrid-1)/2
ymin1=zmin1
zmax1=-zmin1
ymax1=-ymin1

beam0=phaSrcWFGauss( nz1 , zmin1 , zmax1 , ny1, ymin1 , ymax1 , waist, d0 , lambda)

phaIntensitySurface,beam0,'gauss original'
print,''
print,'Gauss at 0 :: Max:', $
	   max(beam0.zezre^2+beam0.zezim^2+beam0.zeyre^2+beam0.zeyim^2,maxi), $
      ':: Tot:',beam0.dxezre*beam0.dyezre*total(beam0.zezre^2+beam0.zezim^2+beam0.zeyre^2+beam0.zeyim^2)


print,' xymin [mm] : ',zmin1
print,' dxy   [mm] : ',dxy
print,' waist [mm] : ',waist
print,'Intensity at (',0,'/',(ngrid/2),') : ',(beam0.zezre^2+beam0.zezim^2)(0,(ngrid/2))
print,' SeitenRandInt/MaxInt :',(beam0.zezre^2+beam0.zezim^2)(0,(ngrid/2))/max(beam0.zezre^2+beam0.zezim^2+beam0.zeyre^2+beam0.zeyim^2,maxi)
print,'  EckenRandInt/MaxInt :',(beam0.zezre^2+beam0.zezim^2)(0,0)/max(beam0.zezre^2+beam0.zezim^2+beam0.zeyre^2+beam0.zeyim^2,maxi)

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;testgauss

pro testmodgrid,nz2,ny2

phainit

!P.Multi=[0,1,1]

b=phaSrcWFGauss(128, -1, 1,128, -1, 1, 0.2 , 0 , 20)

phaModGrid,b,nz2,ny2

phaintensitysurface,b

END
