;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/crl.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <11 Jul 13 12:36:12 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro crl, areal=areal, aimag=aimag, breal=breal, bimag=bimag, bamp=bamp, bphase=bphase, crlamp=crlamp, crlphase=crlphase, $
         radius=radius, thickness=thickness, wavelength=wavelength, y_vec=y_vec, z_vec=z_vec
;+
; NAME:
;   crl
;
;
; PURPOSE:
;   calculate the electric field after a parabolic compound refractive
;   (Be) lense, (thin lense approximation), units (m) and (rad), !!
;   output arrays must be allocated before 
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
;   
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;   radius:     the lense radius in m
;   thickness:  the thickness of the lense on axis
;   wavelength: the wavelength in m
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;   material properties hard coded - only Be so far
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;   crl, areal=zreal, aimag=zimag, breal=z1real, bimag=z1imag, y_vec=y_vec, z_vec=z_vec
;
;
; MODIFICATION HISTORY:
;    11.7.13 UF
;-

;;; UF I do not use the complex numbers functionality in idl
;;; UF I follow the the reference follath:2013f

u1= 'usage: crl, areal=areal, aimag=aimag, [breal=breal,] [bimag=bimag,] [bamp=bamp,] [bphase=bphase,] [crlamp=crlamp,]'
u2= ' [crlphase=crlphase,] [radius=radius,] [thickness=thickness,] [wavelength=wavelength,] y_vec=y_vec, z_vec=z_vec'
usage= u1+u2

print, 'crl called'


if n_elements(radius)     eq 0 then radius    = 5e-4   ;; default radius    0.5 mm
if n_elements(thickness)  eq 0 then thickness = 2e-5   ;; default thickness 20 mum
if n_elements(wavelength) eq 0 then wavelength= 1e-10  ;; default 12.4 keV
if n_elements(areal) eq 0 then print, usage 
if n_elements(aimag) eq 0 then print, usage 
if n_elements(z_vec) eq 0 then print, usage 
if n_elements(y_vec) eq 0 then print, usage 

print, 'crl start calculation'

;; optical constants of Be - can be extended to other materials
rene        = 1.39e15   ;; 1/m^2
mu3kev      = 39.1e2    ;; 1/m
mu12p4kev   = 0.4e2     ;; 1/m
delta3kev   = 3.8e-5    ;;
delta12p4kev= 2.21e-6   ;;

;; interpolate mu and delta 
kev  = 1e3* 1240e-9/wavelength   ;; photon energy in keV
mu   = mu3kev+    ((mu12p4kev- mu3kev)/(12.4- 3.0))       * (kev- 3.0)
delta= delta3kev+ ((delta12p4kev- delta3kev)/(12.4- 3.0)) * (kev- 3.0)

nz= n_elements(z_vec)
ny= n_elements(y_vec)

;; determine factors for amplitude and phase for the crl
crlamp  = dblarr(nz, ny) ;; amplitude
crlphase= dblarr(nz, ny) ;; complex phase
for i=0, nz-1 do begin
    for j=0, ny-1 do begin
        rr= sqrt(z_vec[i]^2 + z_vec[j]^2)          ;; the radial distance 
        if rr lt radius then begin                 ;; inside the lens
            f4= exp(-mu*rr^2/(2.0*radius))         ;; factor 4
            f3= (-1.0)*rene*wavelength*rr^2/radius ;; the phase of the complex number
            f2= exp(-mu*thickness/2.0)             ;; neglectable for normalized flux
        endif else begin
            f4= 0.0   
            f3= 0.0
            f2= 0.0
        endelse
        crlamp[i,j]  = f4*f2
        crlphase[i,j]= f3
    endfor
endfor

;; calculate amplitude and phase of the input field and output field
aamp  = sqrt(areal^2+aimag^2)
aphase= atan(aimag, areal)
bamp  = aamp* crlamp
bphase= aphase+ crlphase

;; calculate real and imag description
breal= bamp* cos(bphase)
bimag= bamp* sin(bphase)

return
end
;; end
