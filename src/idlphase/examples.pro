;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/examples.pro
;  Date      : <19 Aug 13 10:30:00 flechsig> 
;  Time-stamp: <19 Aug 13 17:04:06 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro testbeamload  ;; UF tested Aug 19 2013

beam={source4}

phaseidl= getenv('PHASE_HOME')+'/idl'

phaLoadEzReal,beam, phaseidl+'/ezre_gb_12.dat'

phaLoadEzImag,beam, phaseidl+'/ezim_gb_12.dat'

phaIntensitySurface,beam,'TestBeamLoad'

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro testreadresults ;; UF tested Aug 19 2013

beam={source4}

phaseidl= getenv('PHASE_HOME')+'/idl/'
;;name='SGM.RESULT'
name='test_5000.out'

phaLoadEzReal,beam,phaseidl+name+'-ezrec'
phaLoadEzImag,beam,phaseidl+name+'-ezimc'

phaLoadEyReal,beam,phaseidl+name+'-eyrec'
phaLoadEyImag,beam,phaseidl+name+'-eyimc'

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
 beam=phaSrcWFGauss( 193, -1, 1, 67, 0, 3, 0.2, 0, 20, 1, 0, 0)

print,'phaSrcWFGauss ...'
phaIntensitySurface,beam,'Gauss-Source 193x67'


;  Change number of gridpoints (don't change borders) , because FFT works fastest 
;  with a number of gridpoints, which is a product of small prime-factors, eg.: 128=2^7
;                 nz, ny 
;; UF 20.8.13 function not found phaModGrid,beam,128,128
phaModGridPoints,beam,128,128

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

;; UF get the following error
;; Changing number of grid-points ...
;; debug: after ISTKIN
;; debug: call DCSPIN
;; ERROR    4 IN
;;      D   C   S   P   I   N       -
;;1STACK DUMP
;;0STORAGE PARAMETERS
;; LOGICAL                1 STORAGE UNITS
;; INTEGER                1 STORAGE UNITS
;; REAL                   1 STORAGE UNITS
;; DOUBLE PRECISION       2 STORAGE UNITS
;; COMPLEX                2 STORAGE UNITS
;;0STACK STATISTICS
;; STACK SIZE              24576
;; MAXIMUM STACK USED         10
;; CURRENT STACK USED         10
;; NUMBER OF ALLOCATIONS       0
;;0END OF STACK DUMP



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
;; UF  phaModGrid,beam,128,128
phaModGridPoints,beam,128,128

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
;;UF  phaModSizeCut, beam, 127 , 129 , 126 , 130
phaModGridSizeCut, beam, 127 , 129 , 126 , 130

print,'phaModGridSizeCut ...'
phaIntensitySurface,beam,'rectangular Slit '

;  For further calculations, we add a rim of zeros, so that we obtain a 256x256-field (after the slit)
;                  new:  nz ,ny             
;;UF phaModSizeAddZeros,beam,256,256
phaModGridSizeAddZeros,beam,256,256

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
