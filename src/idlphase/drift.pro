;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <2013-07-14 18:25:59 flechsig> 
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
  acomp= complex(areal, aimag, /double)
endelse 
if n_elements(z_vec) eq 0 then print, usage 
if n_elements(y_vec) eq 0 then print, usage 

print, 'drift start calculation'

nz= n_elements(z_vec)
ny= n_elements(y_vec)

;; determine factors for amplitude and phase for the drift
driftarr  = dcomplexarr(nz, ny) ;; amplitude
k= 2* !dpi/wavelength

for i=0, nz-1 do begin
    for j=0, ny-1 do begin
        rr= sqrt(z_vec[i]^2 + y_vec[j]^2)          ;; the radial distance 
        if rr lt maxr then begin                   ;; inside the aperture
            phase= k/(2.0*drift) * rr^2            ;; 
            
            ;; print,'f4=',f4,' f2=', f2, ' f24', f4*f2
        endif else begin
            f4= 0.0   
            f3= 0.0
            f2= 0.0
        endelse
        driftarr[i,j]  = complex(cos(phase), sin(phase), /double)  ;;
     endfor
endfor

field0= acomp* driftarr
field1= fft(driftarr, -1, /center, dimension=2, /double)
scale = complex(,, /double)

bcomp= scale* driftarr* field1
;; calculate amplitude and phase of the input field and output field
bamp  = aamp* driftamp
bphase= aphase+ driftphase

;; calculate real and imag description
breal= real_part(bcomp)
bimag= imaginary(bcomp)
bamp  = sqrt(breal^2+bimag^2)
bphase= atan2(bimag,breal)


print,'drift end'
return
end
;; end
