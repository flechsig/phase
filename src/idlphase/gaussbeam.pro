;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <18 Jul 13 17:18:21 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 
;
;
;
pro gaussbeam, dist=dist, bcomp=bcomp, w0=w0, Nz=Nz, Ny=Ny, sizez=sizez, sizey=sizey, wavelength=wavelength, y_vec=y_vec, z_vec=z_vec, plot=plot
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
;   sigmaz:       rms horizontal    in m
;   sigmay        rms vert.         in m
;   w0            waist             in m
;   dist          distance to waist in m
;   Nz            points hor.
;   Ny            points vert
;   wavelength    the wavelength in m
;   y_vec:        vertical   position vector  in m
;   z_vec:        horizontal position vector  in m
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
;-

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
 

print, 'wavelength = ',wavelength
print, 'Nz     = ', Nz      , ' Ny     = ', Ny
print, 'sizez  = ', sizez   , ' sizey  = ', sizey
print, 'w0     = ', w0      , ' dist   = ', dist
print, 'z0     = ',!dpi * w0^2/wavelength
z_vec  = (dindgen(Nz)/(Nz-1) - 0.5) * sizez
y_vec  = (dindgen(Ny)/(Ny-1) - 0.5) * sizey
bcomp  = dcomplexarr(Nz, Ny) 

q0    = complex(0, -1* !dpi * w0^2 / wavelength)            ;; 1/q0 = i lambda / (pi w0^2)
q     = q0 + dist
qi    = 1/q

print, ' q0 = ', q0
print, ' q  = ',q
print, ' qi = ',qi

qireal = REAL_PART(qi)
qiimag = Imaginary(qi)

w2 = wavelength / ( !dpi * qiimag)

print,' w2 = ',w2, '     w = ',sqrt(w2)

for i=0, Nz-1 do begin
  for j=0, Ny-1 do begin
    rho  =  z_vec[i]^2 + y_vec[j]^2 
    arg1 = -1*  rho / w2
    if (arg1 le -40) then arg1 = -40                             ;;  -80 immer noch ok
    
    arg2 = !dpi * rho * qireal / wavelength                    ;; R  = 1/qireal;
    phas2 = complex(cos(arg2), sin(arg2),/double)    
 
    bcomp[i,j]= exp(arg1) * phas2 / q
  endfor
endfor


if n_elements(plot) ne 0 then begin
  bamp = abs(bcomp)
  window, RETAIN=2
 contour, bamp,z_vec,y_vec, xtitle='z (mm)', ytitle='y (mm)', title='gaussbeam'
endif
 
print,'gaussbeam end'
return
end
