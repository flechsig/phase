
function example1

; srcgauss - modgrid - propfftnear - modsizecut - modsizeaddzero - propfftfar

print,''
print,'--------------------------------------------------------------'
print,'     EXAMPLE1'
print,'--------------------------------------------------------------'

phainit

!P.Multi=[0,2,3]


;  New Gaussian-Beam with 193x67 Points
;                  ( nz , zmin , zmax , ny , ymin , ymax , waist, dist , lambda, ez0, ey0, dphi_zy)
 beam=phaSrcWFGauss( 193,  -1  , 1    , 67 ,  0  ,   3  ,  0.2 ,  0   , 20     ,  1 ,  2 ,  0     )

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

print,beam.iezrex,' ... iezrex'
print,beam.iezrey,' ... iezrey'

;  Now let the beam propagate through a rectangular-slit (64x4) in the center of the beamline 
;                    nz1 ,nz2 ,ny1 ,ny2
 phaModSizeCut, beam, 32 , 96 , 61 , 66

print,'phaModSizeCut ...'
phaIntensitySurface,beam,'Slit (64x4)'

print,beam.iezrex,' ... iezrex'
print,beam.iezrey,' ... iezrey'

;  For further calculations, we add a rim of zeros, so that we obtain a 256x256-field (after the slit)
;                  new:  nz ,ny             
 phaModSizeAddZeros,beam,256,256

print,'phaModSizeAddZeros ...'
phaIntensitySurface,beam,'Slit with ZEROS'


print,beam.iezrex,' ... iezrex'
print,beam.iezrey,' ... iezrey'

;  Propagate the field with the FFT-far-field-propagator
;                     distance/[mm]
 phaPropFFTfar, beam,  15000

print,'phaPropFFTfar ...'
;phaIntensityShade_Surf,beam,'FFTfar - 15000mm'
phaIntensitySurface,beam,'FFTfar - 15000mm'


print,beam.iezrex,' ... iezrex'
print,beam.iezrey,' ... iezrey'

print,''
print,'----------------------------------------------end-of-EXAMPLE1-'

return,beam

END ;example1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function example2   ; small rectangular slit

; srcgauss - cut - addzero - propfftnear

print,''
print,'--------------------------------------------------------------'
print,'     EXAMPLE2'
print,'--------------------------------------------------------------'

phainit

!P.Multi=[0,2,2]


;  New Gaussian-Beam 
;                  ( nz , zmin , zmax , ny , ymin , ymax , waist, dist , lambda, ez0, ey0, dphi_zy)
 beam=phaSrcWFGauss( 256,-1.8  , 1.8  ,256 , -1.8 , 1.8  ,  0.2 , 5000 ,  20   ,  1 ,  1 , 0     )

;help,/str,beam

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
phaIntensitySurface,beam,'Slit with ZEROS'


;  Propagate the field with the FFT-near-field-propagator
;                     distance/[mm]
 phaPropFFTnear, beam,  1500

print,'phaPropFFTnear ...'
phaIntensitySurface,beam,'FFTnear - 2000mm'


;help,/str,beam

print,''
print,'----------------------------------------------end-of-EXAMPLE2-'

return,beam

END ;example2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
