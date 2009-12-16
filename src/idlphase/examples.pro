
pro testbeamload

beam={source4}

LoadEzReal,beam,'ezre_gb_8000.dat'

LoadEzImag,beam,'ezim_gb_8000.dat'

phaIntensitySurface,beam,'TestBeamLoad'

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro testreadresults

beam={source4}
name='SGM.RESULT'

LoadEzReal,beam,name+'-ezrec'
LoadEzImag,beam,name+'-ezimc'

LoadEyReal,beam,name+'-eyrec'
LoadEyImag,beam,name+'-eyimc'

phaIntensitySurface,beam,'TestBeamLoad'

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro ttt

print,''
print,'--------------------------------------------------------------'
print,'     test - ttt'
print,'--------------------------------------------------------------'

phainit

!P.Multi=[0,2,2]


;  New Gaussian-Beam with 193x67 Points
;   ( nz , zmin , zmax , ny , ymin,ymax, waist, dist , lambda, ez0, ey0, dphi_zy)
 beam=phaSrcWFGauss( 193, -1, 1, 67 ,0,3, 0.2 , 0 , 20,1,0,0)

print,'phaSrcWFGauss ...'
phaIntensitySurface,beam,'Gauss-Source 193x67'


;  Change number of gridpoints (don't change borders) , because FFT works fastest 
;  with a number of gridpoints, which is a product of small prime-factors, eg.: 128=2^7
;                 nz, ny 
 phaModGrid,beam,128,128

print,'phaModGrid ...'
phaIntensitySurface,beam,'Gauss-Source 128x128'


;  Propagate the field with the FFT-near-field-propagator
;                     distance/[mm]
 phaPropFFTnear, beam,  2000

print,'phaPropFFTnear ...'
phaIntensitySurface,beam,'FFTnear - 2000mm'


;  Now let the beam propagate through a rectangular-slit (64x4) in the center of the beamline 
;                    nz1 ,nz2 ,ny1 ,ny2
 phaModSizeCut, beam, 32 , 96 , 62 , 66

print,'phaModSizeCut ...'
phaIntensitySurface,beam,'Slit (64x4)'




END ; ttt


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro example1

; srcgauss - modgrid - propfftnear - modsizecut - modsizeaddzero - propfftfar

print,''
print,'--------------------------------------------------------------'
print,'     EXAMPLE1'
print,'--------------------------------------------------------------'

phainit

!P.Multi=[0,2,3]


;  New Gaussian-Beam with 193x67 Points
;   ( nz , zmin , zmax , ny , ymin,ymax, waist, dist , lambda, ez0, ey0, dphi_zy)
 beam=phaSrcWFGauss( 193, -1, 1, 67 ,0,3, 0.2 , 0 , 20,1,0,0)

print,'phaSrcWFGauss ...'
phaIntensitySurface,beam,'Gauss-Source 193x67'


;  Change number of gridpoints (don't change borders) , because FFT works fastest 
;  with a number of gridpoints, which is a product of small prime-factors, eg.: 128=2^7
;                 nz, ny 
 phaModGrid,beam,128,128

print,'phaModGrid ...'
phaIntensitySurface,beam,'Gauss-Source 128x128'


;  Propagate the field with the FFT-near-field-propagator
;                     distance/[mm]
 phaPropFFTnear, beam,  2000

print,'phaPropFFTnear ...'
phaIntensitySurface,beam,'FFTnear - 2000mm'


;  Now let the beam propagate through a rectangular-slit (64x4) in the center of the beamline 
;                    nz1 ,nz2 ,ny1 ,ny2
 phaModSizeCut, beam, 32 , 96 , 62 , 66

print,'phaModSizeCut ...'
phaIntensitySurface,beam,'Slit (64x4)'


;  For further calculations, we add a rim of zeros, so that we obtain a 256x256-field (after the slit)
;                  new:  nz ,ny             
 phaModSizeAddZeros,beam,256,256

print,'phaModSizeAddZeros ...'
phaIntensitySurface,beam,'Slit with ZEROS'


;  Propagate the field with the FFT-far-field-propagator
;                     distance/[mm]
 phaPropFFTfar, beam,  15000

print,'phaPropFFTfar ...'
;phaIntensityShade_Surf,beam,'FFTfar - 15000mm'
phaIntensitySurface,beam,'FFTfar - 15000mm'



print,''
print,'----------------------------------------------end-of-EXAMPLE1-'
END ;example1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro example2   ; small rectangular slit

; srcgauss - cut - addzero - propfftnear

print,''
print,'--------------------------------------------------------------'
print,'     EXAMPLE2'
print,'--------------------------------------------------------------'

phainit

!P.Multi=[0,2,2]


;  New Gaussian-Beam with 256x256 Points
;   ( nz , zmin , zmax , ny , ymin,ymax, waist, dist , lambda, ez0, ey0, dphi_zy)
 beam=phaSrcWFGauss( 256, -1, 1, 256 ,0,3, 0.2 , 0 , 20,1,0,0)

print,'phaSrcWFGauss ...'
;phaIntensityshade_surf,beam,'Gauss-Source '
phaIntensitySurface,beam,'Gauss-Source '


;  Now let the beam propagate through a rectangular-slit (2x4) in the center of the beamline 
;                     nz1 , nz2 , ny1 , ny2
 phaModSizeCut, beam, 127 , 129 , 126 , 130

print,'phaModSizeCut ...'
phaIntensitySurface,beam,'rectangular Slit '

;  For further calculations, we add a rim of zeros, so that we obtain a 256x256-field (after the slit)
;                  new:  nz ,ny             
 phaModSizeAddZeros,beam,256,256

print,'phaModSizeAddZeros ...'
;phaIntensityshade_Surf,beam,'Slit with ZEROS'
phaIntensitySurface,beam,'Slit with ZEROS'


;  Propagate the field with the FFT-near-field-propagator
;                     distance/[mm]
 phaPropFFTnear, beam,  1500

print,'phaPropFFTnear ...'
;phaIntensityshade_surf,beam,'FFTnear - 2000mm'
phaIntensitySurface,beam,'FFTfar - 2000mm'




print,''
print,'----------------------------------------------end-of-EXAMPLE1-'
END ;example2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
