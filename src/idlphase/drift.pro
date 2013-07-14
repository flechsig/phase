;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <2013-07-14 17:29:06 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro drift, acomp=acomp, areal=areal, aimag=aimag, apfac=apfac, bcomp=bcomp, breal=breal, bimag=bimag, $
         bamp=bamp, bphase=bphase, driftamp=driftamp, driftphase=driftphase, $
         radius=radius, thickness=thickness, wavelength=wavelength, y_vec=y_vec, z_vec=z_vec
;+
; NAME:
;   drift
;
;
; PURPOSE:
;   calculate the electric field after some distance in vacuum
;
;
; CATEGORY:
;   phase_calc
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;   drift distance dx in m
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;   acomp:      input field, idl complex array, if given parameters
;               areal, aimag are ignored
;   areal:      input field, real part (required)
;   aimag:      input field, imag. part (required)
;   apfac:      aperture factor, max. ap.= apfac* radius, default= 1.0
;   bcomp:      output field, idl complex array
;   bamp:       output field, amplitude
;   bphase:     output field, phase
;   breal:      output field, real part
;   bimag:      output field, imag part
;   driftamp:     drift amplitude factor
;   driftphase:   drift phase factor
;   radius:     the lens radius in m
;   thickness:  the thickness of the lens on axis in m
;   wavelength: the wavelength in m
;   y_vec:      vertical input vector (required) in m
;   z_vec:      horizontal input vector (required) in m
;
; OUTPUTS:
;   see keyword parameters
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;   no
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
; PROCEDURE:
;   
;
;
; EXAMPLE:
;   drift, areal=zreal, aimag=zimag, breal=z1real, bimag=z1imag, y_vec=y_vec, z_vec=z_vec
;
;
; MODIFICATION HISTORY:
;    11.7.13 UF
;-

;;; UF I do not use the complex numbers functionality in idl
;;; UF I follow the the reference follath:2013f

u1= 'usage: drift, areal=areal, aimag=aimag, [apfac=apfac,] [breal=breal,] [bimag=bimag,] [bamp=bamp,] [bphase=bphase,] [driftamp=driftamp,]'
u2= ' [driftphase=driftphase,] [radius=radius,] [thickness=thickness,] [wavelength=wavelength,] y_vec=y_vec, z_vec=z_vec'
usage= u1+u2

print, 'drift called'

if n_elements(apfac)      eq 0 then apfac     = 1.0    ;; aperture factor
if n_elements(radius)     eq 0 then radius    = 5e-4   ;; default radius    0.5 mm
if n_elements(thickness)  eq 0 then thickness = 2e-5   ;; default thickness 20 mum
if n_elements(wavelength) eq 0 then wavelength= 1e-10  ;; default 12.4 keV

if n_elements(acomp) ne 0 then begin
  areal= real_part(acomp)
  aimag= imaginary(acomp)
endif else begin
  if n_elements(areal) eq 0 then print, usage 
  if n_elements(aimag) eq 0 then print, usage
endelse 
if n_elements(z_vec) eq 0 then print, usage 
if n_elements(y_vec) eq 0 then print, usage 

print, 'drift start calculation'

;; optical constants of Be - can be extended to other materials
rene        = 1.39e15   ;; 1/m^2
mu3kev      = 39.1e2    ;; 1/m
mu12p4kev   = 0.4e2     ;; 1/m
delta3kev   = 3.8e-5    ;;
delta12p4kev= 2.21e-6   ;;

;; interpolate mu and delta - should be improved
kev  = 1e-3* 1240e-9/wavelength     ;; photon energy in keV
mu   = mu3kev+    ((mu12p4kev- mu3kev)/(12.4- 3.0))       * (kev- 3.0)
delta= delta3kev+ ((delta12p4kev- delta3kev)/(12.4- 3.0)) * (kev- 3.0)
if mu    lt 0.0 then mu= 0.0        ;; avoid overflow
if delta lt 0.0 then delta= 0.0     ;; avoid overflow

maxr= apfac*radius       ;; define a maximum radius

print,'photon energy=',kev,', mu=', mu, ', delta=',delta,', aperture=', 2.0*maxr 

nz= n_elements(z_vec)
ny= n_elements(y_vec)

;; determine factors for amplitude and phase for the drift
driftamp  = dblarr(nz, ny) ;; amplitude
driftphase= dblarr(nz, ny) ;; complex phase
for i=0, nz-1 do begin
    for j=0, ny-1 do begin
        rr= sqrt(z_vec[i]^2 + y_vec[j]^2)          ;; the radial distance 
        if rr lt maxr then begin                   ;; inside the aperture
            f4= exp(-mu*rr^2/(2.0*radius))         ;; factor 4
            f3= (-1.0)*rene*wavelength*rr^2/radius ;; the phase of the complex number
            f2= exp(-mu*thickness/2.0)             ;; neglectable for normalized flux
            ;; print,'f4=',f4,' f2=', f2, ' f24', f4*f2
        endif else begin
            f4= 0.0   
            f3= 0.0
            f2= 0.0
        endelse
        driftamp[i,j]  = f4*f2
        driftphase[i,j]= f3
    endfor
endfor

;; calculate amplitude and phase of the input field and output field
aamp  = sqrt(areal^2+aimag^2)
aphase= atan(aimag, areal)
bamp  = aamp* driftamp
bphase= aphase+ driftphase

;; calculate real and imag description
breal= bamp* cos(bphase)
bimag= bamp* sin(bphase)
bcomp= complex(breal, bimag, /double)

print,'drift end'
return
end
;; end
