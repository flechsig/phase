;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <29 Aug 13 11:08:19 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 
;
;
;
pro gaussbeam, dist=dist, w0=w0, Nz=Nz, Ny=Ny, sizez=sizez, sizey=sizey, bcomp=bcomp, $
               z_vec=z_vec, y_vec=y_vec, wavelength=wavelength, plot=plot, example=example
;+
; NAME:
;  gaussbeam  
;
;
; PURPOSE:
;  Creates gaussian beam  
;  currently only a 2dim gaussian distribution (in the waist), UF
;  (2b_confirmed) for the intensity distribution we have w=2*sigma
;  with sigma= sigma_x = sigma_y the variance of a 2d Gaussian and
;  sigma_r= sqrt(2) * sigma ???
;
;
; CATEGORY:
;   phase_calc
;
;
; CALLING SEQUENCE:
;   gaussbeam, ...
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
;   example:      example calculation plus plot
;   sigmaz:       rms horizontal              in m
;   sigmay        rms vert.                   in m
;   w0            waist                       in m
;   z             distance to waist           in m
;   wavelength    the wavelength              in m
;   y_vec:        vertical   position vector  in m
;   z_vec:        horizontal position vector  in m
;   Nz            points hor.
;   Ny            points vert
;   sizey:        height (m), default = sizez
;   sizez:        width (m)
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
;    27.8.13 UF minor extension
;-

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

u1= 'usage: gaussbeam,[dist=dist,][bcomp=bcomp,][w0=w0,][sizez=sizez,][sizey=sizey,][Nz=Nz,][Ny=Ny,]'
u2= '[wavelength=wavelength,] [y_vec=y_vec], [z_vec=z_vec], [plot=plot]'
usage= u1+u2

print, 'gaussbeam called'

IF KEYWORD_SET(EXAMPLE) THEN BEGIN
    print, '**********************************************************'
    print, 'example: HeNe Laser '
    print, 'wavelength=633e-9, w0= 1e-3, dist= 10., sizez=1e-2'
    print, '**********************************************************'
    gaussbeam, dist=10.,wavelength=633.e-9, w0=1e-3,sizez=1e-2,/plot
    return
endif  ;; end example



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

print, 'wavelength (m) = ',wavelength
print, 'Nz     = ', Nz      , ', Ny     = ', Ny
print, 'sizez (m) = ', sizez   , ', sizey (m) = ', sizey
print, 'w0    (m) = ', w0      , ', dist  (m) = ', dist

k   = !dpi * 2    / wavelength   
z0  = !dpi * w0^2 / wavelength
w   = w0 * sqrt(1d0+ (dist/z0)^2)
w2  = w^2
eta = atan(dist/z0)
Ri  = dist / (dist^2 + z0^2)                                         ;; Ri  = 1/R;

print, 'z0    (m) = ', z0
print, 'w     (m) = ', w   ,', w2 (m^2) = ', w2
print, 'eta (rad) = ', eta ,', Ri (1/m) = ', Ri 

truncation= 0 
for i=0, Nz-1 do begin
  for j=0, Ny-1 do begin
    rho2  =  z_vec[i]^2 + y_vec[j]^2 
    arg1  = -1 *  rho2 / w2    
    if (arg1 le -40) then begin 
        arg1 = -40                        ;;  -40, but -80 is still ok
        truncation= 1
    endif
    arg2  = 0.5 * k * rho2 * Ri + k*dist - eta                    ;; For notation of Siegman multiply by -1                    
    phas2 = complex(cos(arg2), sin(arg2), /double)     
    bcomp[i,j]= phas2 * exp(arg1) * w0 / w
  endfor
endfor

if truncation gt 0 then print, '!! warning -- some points are truncated !!'  

;; plot using mycontour
if n_elements(plot) ne 0 then begin
  bamp = abs(bcomp)
  window, 20, RETAIN=2
  stat = dblarr(7)
  fit   = gauss2dfit(bamp,    stat, z_vec, y_vec) 
  fit2  = gauss2dfit(bamp^2, stat2, z_vec, y_vec) 
  print, 'gaussfit amplitude: rms_z, rms_y (m)= ', stat(2),  stat(3)
  print, 'gaussfit intensity: rms_z, rms_y (m)= ', stat2(2), stat2(3)
  title= 'gaussbeam intensity '+  'size='+  string(stat2(2)*1e6,FORMAT="(f6.1)")+ ' x ' +string(stat2(3)*1e6, FORMAT="(f6.1)") + textoidl(' \mum^2 rms')
  mycontour, bamp, z_vec*1e3, y_vec*1e3, xtitle='z (mm)', ytitle='y (mm)', title=title

  pha = atan(bcomp, /phase)
  if max(pha)- min(pha) gt 1e-10 then begin
      window,21, RETAIN=2
      mycontour, pha, z_vec*1e3, y_vec*1e3, xtitle='z (mm)', ytitle='y (mm)', title='gaussbeam phase'
  endif else begin
      print, 'phase(z,y) is zero- no phase plot'
      device,window_state=window_list
      if window_list[21] gt 0 then wdelete, 21
  endelse
endif ;; plot
 


print,'gaussbeam end'
return
end
