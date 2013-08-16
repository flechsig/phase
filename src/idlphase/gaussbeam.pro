;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <16 Aug 13 08:33:00 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 
;
;
;
pro gaussbeam, dist=dist, w0=w0, Nz=Nz, Ny=Ny, sizez=sizez, sizey=sizey, bcomp=bcomp, $
               z_vec=z_vec, y_vec=y_vec, wavelength=wavelength, plot=plot
;+
; NAME:
;  gaussbeam  
;
;
; PURPOSE:
; Creates gaussian beam  
;  currently only a 2dim gaussian distribution (in the waist)
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
;   bcomp:        field, idl complex array,
;   sigmaz:       rms horizontal              in m
;   sigmay        rms vert.                   in m
;   w0            waist                       in m
;   z             distance to waist           in m
;   wavelength    the wavelength              in m
;   y_vec:        vertical   position vector  in m
;   z_vec:        horizontal position vector  in m
;   Nz            points hor.
;   Ny            points vert
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
;
;
; MODIFICATION HISTORY:
;    23.7.13 RF
;
; lambda= 1 um, in  0 m   sigma(Efield) = W0 / sqrt(2) => FWHM = 16.6 um         sigma(intensity)  = w0/2 = 5e-6,
;
;  gaussbeam, dist=0, Nz=100,sizez=0.0002, z_vec=z_vec, y_vec=y_vec, bcomp=bcomp , w0=10e-6 , wavelength=1e-6
;
;
;  lambda= 1 um, in 20 m  w  = 0.636 m 
;            sigma(Efield)= W / sqrt(2) => FWHM =  1.056 m
; 
;  gaussbeam, dist=20, Nz=100,sizez=5, z_vec=z_vec, y_vec=y_vec, bcomp=bcomp , w0=10e-6 , wavelength=1e-6
;
; lambda = 1.24 A     w=27.7 um
;
;  gaussbeam, dist=0, Nz=200,sizez=0.0002, z_vec=z_vec, y_vec=y_vec, bcomp=bcomp , w0=27.7e-6 , wavelength=1.24e-10
;
;;; 

u1= 'usage: gaussbeam, [bcomp=bcomp,][sigmaz=sigmaz,][sigmaz=sigmaz,][Nz=Nz,][Ny=Ny,]'
u2= '[wavelength=wavelength,] [y_vec=y_vec], [z_vec=z_vec], [plot=plot]'
usage= u1+u2

print, 'gaussbeam called'

if n_elements(Nz        ) eq 0 then Nz        = 100  
if n_elements(Ny        ) eq 0 then Ny        = Nz  
if n_elements(wavelength) eq 0 then wavelength= 1e-10  
if n_elements(w0        ) eq 0 then w0        = 1e-5  
if n_elements(sizez     ) eq 0 then sizez     = 1e-3
if n_elements(sizey     ) eq 0 then sizey     = sizez
if n_elements(dist      ) eq 0 then dist      = 0

bcomp  = dcomplexarr(Nz, Ny) 
z_vec  = (dindgen(Nz)/(Nz-1) - 0.5) * sizez
y_vec  = (dindgen(Ny)/(Ny-1) - 0.5) * sizey

 
print, 'wavelength = ',wavelength
print, 'Nz     = ', Nz      , ' Ny     = ', Ny
print, 'sizez  = ', sizez   , ' sizey  = ', sizey
print, 'w0     = ', w0      , ' dist   = ', dist

k   = !dpi * 2    / wavelength   
z0  = !dpi * w0^2 / wavelength
w   = w0 * sqrt(1+ (dist/z0)^2)
w2  = w^2
eta = atan(dist/z0)
Ri  = dist / (dist^2 + z0^2)                                         ;; Ri  = 1/R;


print, 'z0     = ',!dpi * w0^2/wavelength
print, 'w      = ',w   ,'    w2 = ', w2
print, 'eta    = ',eta ,'    Ri = ', Ri 
 
for i=0, Nz-1 do begin
  for j=0, Ny-1 do begin
    rho2  =  z_vec[i]^2 + y_vec[j]^2 
    arg1  = -1 *  rho2 / w2    
    if (arg1 le -40) then arg1 = -40                             ;;  -40 bisher -80 immer noch ok
    arg2  = 0.5 *k * rho2 * Ri  + k*dist - eta                    ;; For notation of Siegman multiply by -1                    
    phas2 = complex(cos(arg2), sin(arg2),/double)     

    bcomp[i,j]= phas2 * exp(arg1) * w0 / w
;    print, i, ' ', j,' ',bcomp[i,j] , ' ' ,'arg1=', arg1, ' phas2= ',phas2
  endfor
endfor


if n_elements(plot) ne 0 then begin
  bamp = abs(bcomp)
  window,20,  RETAIN=2
  contour, bamp,z_vec,y_vec, xtitle='z (mm)', ytitle='y (mm)', title='gaussbeam'

  bamp = atan(bcomp,/phase)
  window,21, RETAIN=2
  contour, bamp,z_vec,y_vec, xtitle='z (mm)', ytitle='y (mm)', title='gaussbeam'
endif
 
print,'gaussbeam end'
return
end
