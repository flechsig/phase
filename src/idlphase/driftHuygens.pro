;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <18 Jul 13 17:18:21 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro driftHuygens, drift=drift, y_vec=y_vec, z_vec=z_vec, acomp=acomp $
                 ,Nu=Nu, Nv=Nv, sizeu=sizeu, sizev=sizev, u=u, v=v, bcomp=bcomp $
                 , plot=plot, wavelength=wavelength
;+
; NAME:
;
;
; PURPOSE:
;   calculate the electric field after distance 'drift' in vacuum
;
;   eqn 4.2 of Goodman
;
; CATEGORY:
;
;
; CALLING SEQUENCE:
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
;   acomp:      input field, idl complex array, if given parameters
;   bcomp:      output field, idl complex array
;   drift:      drift distance in m
;   plot:       make contour plot of amplitude
;   wavelength: the wavelength in m
;   u:          the horizontal output vector
;   v:          the vertical output vector
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
;
;
; MODIFICATION HISTORY:
;    23.7.13 RF
;-

;;; 

u1= 'usage: driftHuygens, [acomp=acomp,][areal=areal,][aimag=aimag,][breal=breal,][bimag=bimag,][bamp=bamp,][bphase=bphase,]'
u2= '[wavelength=wavelength,] y_vec=y_vec, z_vec=z_vec'
usage= u1+u2

print, '------------------ driftHuygens called ----------------------------'

if n_elements(drift)      eq 0 then begin
  print, usage & return  
endif

if n_elements(wavelength) eq 0 then wavelength= 1e-10  
if n_elements(z_vec) eq 0 then begin 
    print, usage & return 
endif
if n_elements(y_vec) eq 0 then begin 
    print, usage & return 
endif
if n_elements(acomp) eq 0 and (n_elements(areal) eq 0 or n_elements(aimag) eq 0) then begin 
    print, usage & return 
endif
if n_elements(acomp) eq 0 then acomp= complex(areal, aimag, /double)

if n_elements(filter) eq 0 then filter=0



print, 'driftHuygens start calculation  ---  drift=',drift

Nz= n_elements(z_vec)
Ny= n_elements(y_vec)

zz= z_vec[Nz-1]- z_vec[0]                                      ;; total width
yy= y_vec[Ny-1]- y_vec[0]                                      ;; total width

dz=z_vec[1]- z_vec[0]                                          ;; step width
dy=y_vec[1]- y_vec[0]

print, 'z_vec[0] = ',z_vec[0]*1e3, ' z_vec[Nz-1] ', z_vec[nz-1]*1e3, ' mm^2 '
print, 'width    = ', zz*1e3 , ' x ', yy*1e3, ' mm^2 '
print, 'dz       = ',   dz   , ' dy    = ', dy
print, 'Nz       = ',   Nz   , ' Ny    = ', Ny
; print, 'sizeu    = ',   sizeu, ' sizev = ', sizev
print, 'Nu       = ',   Nu   , ' Nv    = ', Nv

u=(dindgen(Nu) - Nu/2) * dz                            ;; same step width as input field
v=(dindgen(Nv) - Nv/2) * dy                   

r     = double(1.0)
amp   = double (1.0)
ppi   = 2 * !pi


bcomp  = dcomplexarr(Nu, Nv) 
kernel = dcomplexarr(Nu, Nv) 

phase  = dindgen(Nu,Nv)

for i=0, Nu-1 do begin
    for j=0, Nv-1 do begin
          r           = sqrt ( u[i]^2 + v[j]^2 + drift^2 )
          amp         = -1.0 * drift / r^2 / wavelength            ;; cos(n r)  = dist /r        
          phase [i,j] = r * ppi / wavelength 
          kernel[i,j] = complex( amp * sin(phase[i,j]), -1.0 * amp *cos(phase[i,j]), /double)
    endfor
endfor

help, u,v, bcomp, acomp, kernel

bcomp = CONVOL( Kernel, acomp)
;bcomp = CONVOL( acomp, Kernel)

help, u,v, bcomp, acomp, kernel

; plot only relativ phase, otherwise mycontour crashes.

 phase = phase - ppi * drift / wavelength
 window,21, RETAIN=2, XSIZE=400, YSIZE=300, XPOS=0, YPOS=1000 
 mycontour, phase, u*1e3, v*1e3, xtitle='  u (mm)', ytitle='v (mm)', title='Relative Phase of Kernel'

print,'---------------- driftHuygens end ----------------------------------'
return
end
;; end
