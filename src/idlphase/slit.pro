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
pro slit, dz=dz, dy=dy, Nz=Nz, Ny=Ny, sizez=sizez, sizey=sizey,  bcomp=bcomp, z_vec=z_vec, y_vec=y_vec, wavelength=wavelength, plot=plot
;+
; NAME:
;  slit  
;
;
; PURPOSE:
; Creates a uniform illuminated slit  
;
;
; CATEGORY:
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;   bcomp:        field, idl complex array,
;   dz,dy:        width                       in m
;   sizez, sizey  Field size                  in m 
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
; COMMON BLOCKS:
;   no
;
;
; SIDE EFFECTS:
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
;    8.8.13 RF
;

u1= 'usage:slit, [bcomp=bcomp,][sizez=sizez,][sizey=sizey,][Nz=Nz,][Ny=Ny,]'
u2= '[wavelength=wavelength,] [y_vec=y_vec], [z_vec=z_vec], [plot=plot]'
usage= u1+u2

print, 'slit called'

if n_elements(Nz        ) eq 0 then Nz        = 101 
if n_elements(Ny        ) eq 0 then Ny        = Nz  
if n_elements(wavelength) eq 0 then wavelength= 1e-10  
if n_elements(dz        ) eq 0 then dz        = 1e-5  
if n_elements(dy        ) eq 0 then dy        = dz  
if n_elements(sizez     ) eq 0 then sizez     = 1e-3
if n_elements(sizey     ) eq 0 then sizey     = sizez

bcomp  = dcomplexarr(Nz, Ny) 
z_vec  = (dindgen(Nz)/(Nz-1) - 0.5) * sizez
y_vec  = (dindgen(Ny)/(Ny-1) - 0.5) * sizey

 
print, 'wavelength = ',wavelength
print, 'Nz     = ', Nz      , ' Ny     = ', Ny
print, 'sizez  = ', sizez   , ' sizey  = ', sizey
print, 'dz     = ', dz      , ' dy     = ', dy

; dz,dy are width of slit, 
; make code faster and divide it by 2 for faster evaluation of if statement
dzh=dz/2
dyh=dy/2
 
for i=0, Nz-1 do begin
  for j=0, Ny-1 do begin
     if ( (abs(z_vec[i]) le dzh) AND (abs(y_vec[j]) le dyh)) then amp = 1 else amp = 0 

     bcomp[i,j]=  complex(amp, 0,/double)     

;    print, i, ' ', j,' ',bcomp[i,j] 
  endfor
endfor

 
print,'slit end'
return
end
