;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/crl.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <11 Jul 13 10:28:12 flechsig> 
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
;   (Be) lense, (thin lense approximation), units (m) and (rad) 
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
;
;
;
; MODIFICATION HISTORY:
;    11.7.13 UF
;-

;;; UF I do not use the complex numbers functionality in idl
;;; UF I follow the the reference follath:2013f

if n_elements(radius)     eq 0 then radius    = 1e-3   ;; default radius 1 mm
if n_elements(thickness)  eq 0 then thickness = 1e-4   ;; default radius 100 mum
if n_elements(wavelength) eq 0 then wavelength= 1e-10  ;; default 12.4 keV
if n_elements(areal) eq 0 then print, usage & return
if n_elements(aimag) eq 0 then print, usage & return
if n_elements(z_vec) eq 0 then print, usage & return
if n_elements(y_vec) eq 0 then print, usage & return

usage= 'usage: crl, areal=areal, aimag=aimag, [breal=breal, bimag=bimag, bamp=bamp, bphase=bphase, crlamp=crlamp, crlphase=crlphase, $
         radius=radius, thickness=thickness, wavelength=wavelength,] y_vec=y_vec, z_vec=z_vec'

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

crlamp  = dblarr(nz, ny) ;; amplitude
crlphase= dblarr(nz, ny) ;; complex phase
for i=0, nz-1 do begin
    for j=0, ny-1 do begin
        rr= sqrt(z_vec[i]^2 + z_vec[j]^2) ;; the radial distance 
        f4= exp(-mu*rr^2/(2.0*radius))    ;; factor 4 
        f3= (-1.0)*rene*wavelength*rr^2/radius ;; the phase of the complex number
        f2= exp(-mu*d/2.0)                ;; neglectable for normalized flux
        crlamp[i,j]= f4*f2
        crlphase[i,j]= f3
    endfor
endfor

;; we have now the factors for amplitude and phase for the crl
;; and calculater amplitude and phase of the input and output
aamp  = sqrt(areal^2+aimag^2)
aphase= atan(aimag,areal)
bamp  = aamp* crlamp
bphase= aphase+ crlphase

;; go back to real and imag description
breal= bamp* cos(bphase)
bimag= bamp* sin(bphase)

return
end
;; end
