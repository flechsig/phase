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
                 ,Nu=Nu, Nv=Nv, u=u, v=v, bcomp=bcomp $
                 , plot=plot, wavelength=wavelength, Npi= Npi 
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
;   Npi  :      Limits the field size of the Propagator 
;               Condition: Phase advance between adjacent field 
;               elements must not be larger than Npi * pi
;               If not set: Npi = 1                             
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
;-   13.8.13 RF   Problem : aliasing occurs when sampling the quadratic phase term.
;                 Solution: Limit the size of the u,v-fields  to values where 
;                           the phase difference of adjacent elements is smaller than pi.
;   15.8. 13 renamed to drifthuygens.pro                            

;;; 

u1= 'usage: driftHuygens, [drift=drift,], [acomp=acomp,][bcomp=bcomp,]'
u2= '[wavelength=wavelength,] y_vec=y_vec, z_vec=z_vec, u=u, v=v,[plot=plot,]'
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

if n_elements(plot) eq 0 then plot=0

if n_elements(Npi) eq 0 then  Npi=1


print, 'driftHuygens start calculation  ---  drift=',drift
r     = double(1.0)
amp   = double (1.0)
ppi   = double(2 * !pi)

help, z_vec, y_vec

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
print, 'Nu       = ',   Nu   , ' Nv    = ', Nv
print, 'Npi      = ',   Npi


Nuu =  long(wavelength * drift / dz / dz)              ;; limit the range of u,v, otherwise
Nvv =  long(wavelength * drift / dy / dy)              ;; aliasing occurs by sampling the quadratic phase term
print, 'Nuu      = ',  Nuu,    ' Nvv   = ',Nvv

if (Npi ne 0) then begin
 if (Nu ge Nuu ) then Nu = Nuu*Npi
 if (Nv ge Nvv ) then Nv = Nvv*Npi
endif

print, 'Nu       = ',   Nu   , ' Nv    = ', Nv


print,'Output window  Z = ', Nu*dz*1e3, ' mm  Y = ', Nv * dy*1e3, ' mm'

u=(dindgen(Nu) - Nu/2) * dz                            ;; same step width as input field
v=(dindgen(Nv) - Nv/2) * dy                   
 
phase0=drift * ppi / wavelength
print, 'Phas0 = ', string(phase0, FORMAT="(f30.10)") 


bcomp  = dcomplexarr(Nu, Nv) * 0.0
kernel = dcomplexarr(Nu, Nv) * 0.0
phase  = dindgen    (Nu, Nv) * 0.0 ;; + phase0

;; print,phase[Nu/2,*] 


for i=0, (Nu-1) do begin
    for j=0, (Nv - 1)do begin
          r           = sqrt ( u[i]^2 + v[j]^2 + drift^2 )
          amp         = -1.0 * drift / r^2 / wavelength            ;; cos(n r)  = dist /r        
          phase [i,j] = r * ppi / wavelength 
          kernel[i,j] = complex( amp * sin(phase[i,j]), -1.0 * amp *cos(phase[i,j]), /double)
    endfor
endfor


help, acomp, kernel, phase


dum  = size(Kernel)
sizek= dum[2]
dum  = size(acomp)
sizea= dum[2]
print, ' size of amp : ',sizea, ' size of Kernel :',sizek



if (sizea le sizek) then begin
  print, 'CONVOL( Kernel, acomp, /EDGE_ZERO )'   
  bcomp = CONVOL( Kernel, acomp, /EDGE_ZERO )        ;; dimension Kernel > dimension (acomb)    
endif else begin
  print, 'CONVOL( acomp, Kernel, /EDGE_ZERO )'   
  bcomp = CONVOL( acomp, Kernel, /EDGE_ZERO )         ;; dimension acomp > dimension (Kernel)  
  u=z_vec
  v=y_vec
endelse
 help, u,v, bcomp




if (plot ne 0) then begin
 help, u,v, bcomp, acomp, kernel
 help, ppi, wavelength 
 
 print, 'Phase drift = ', string(drift * ppi / wavelength, FORMAT="(f30.10)") 
 print, 'Phase min   = ', string(min(phase), FORMAT="(f30.10)") 
 print, 'Phase max   = ', string(max(phase), FORMAT="(f30.10)") 

 window,24, RETAIN=2, XSIZE=500, YSIZE=200, XPOS=0, YPOS=500 
 plot,phase[Nu/2,*] , title='phase'
 
 ;; contour only relative phase, otherwise mycontour crashes.
 
 phase = phase - drift * ppi / wavelength
 window,21, RETAIN=2, XSIZE=400, YSIZE=300, XPOS=0, YPOS=1000 
 mycontour, phase, u*1e3, v*1e3, xtitle='  u (mm)', ytitle='v (mm)', title='Relative Phase of Kernel'

 kphas = atan(Kernel,/PHASE)
 window,22, RETAIN=2, XSIZE=400, YSIZE=300, XPOS=0, YPOS=700 
 mycontour, kphas, u*1e3, v*1e3, xtitle='  u (mm)', ytitle='v (mm)', title='Kernel Phase'
 
endif

print,'---------------- driftHuygens end ----------------------------------'
return
end
;; end
