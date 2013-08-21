;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <18 Jul 13 17:18:21 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro  propfresnel, drift=drift, y_vec=y_vec, z_vec=z_vec, field=field $
                 ,Nu=Nu, Nv=Nv $
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
;   field:      input field, idl complex array,
;   y_vec:      input vector vertical, (required) in m
;   z_vec:      input vector horizontal (required) in m
;         :      above three fields will be overwritten
;   drift:      drift distance in m
;   plot:       make contour plot of amplitude
;   wavelength: the wavelength in m
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
;   15.8. 13 renamed to propfresnel.pro                            
;            operate on input field, output overwrites input field                               

;;; 

u1= 'usage: driftfresnel, [drift=drift,], [field=field,]'
u2= '[wavelength=wavelength,] y_vec=y_vec, z_vec=z_vec,[plot=plot,]'
usage= u1+u2

print, '------------------ driftfresnel called ----------------------------'

if n_elements(drift)      eq 0 then begin print, usage & return & endif
if n_elements(z_vec)      eq 0 then begin print, usage & return & endif
if n_elements(y_vec)      eq 0 then begin print, usage & return & endif
if n_elements(field)      eq 0 then begin print, usage & return & endif
if n_elements(wavelength) eq 0 then wavelength= 1e-10  
if n_elements(filter)     eq 0 then filter=0
if n_elements(plot  )     eq 0 then plot=0
if n_elements(Npi   )     eq 0 then Npi=1

;----------------------------------------------------------------------------------------------


print, 'propfresnel start calculation  ---  drift=',drift
r   = double(1.0)                 
amp = double(1.0)                 
ppi = double(2 * !pi)             
Nz  = n_elements(z_vec)           
Ny  = n_elements(y_vec)           
dz  = z_vec[1]   - z_vec[0]                                    ;; step width
dy  = y_vec[1]   - y_vec[0]           
zz  = z_vec[Nz-1]- z_vec[0]                                    ;; total width
yy  = y_vec[Ny-1]- y_vec[0]                                    ;; total width

help, z_vec, y_vec
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
 if (Nu ge Nuu ) then Nu = Nuu*Npi                      ;; Npi see explanation above
 if (Nv ge Nvv ) then Nv = Nvv*Npi
endif

print,'Nu       = ',   Nu   , ' Nv    = ', Nv
print,'Output window  Z = ', Nu*dz*1e3, ' mm  Y = ', Nv * dy*1e3, ' mm'

u=(dindgen(Nu) - Nu/2) * dz                            ;; same step width as input field
v=(dindgen(Nv) - Nv/2) * dy                   

;----------------------------------------------------------------------------------------------
;; amp = 1  / (i l d)  =  -i  / (l d) 
;; -i*amp * ( cos(phi) + i sin(phi) = amp * ( sin(phi - i cos(phi)

kernel = dcomplexarr(Nu, Nv) * 0.0
phase  = dindgen    (Nu, Nv) * 0.0 
amp    =      1/  drift  / wavelength            ;; 16.8.2013       

for i=0, (Nu-1) do begin
    for j=0, (Nv - 1)do begin
          phase [i,j] =  double(!pi / drift / wavelength   * ( u[i]^2 + v[j]^2 ))  
          kernel[i,j] = complex( amp * sin(phase[i,j]), -1.0 * amp *cos(phase[i,j]), /double)
    endfor
endfor


dum  = size(Kernel) & sizek= dum[2]
dum  = size(field)  & sizea= dum[2]

print, ' size of field : ',sizea, ' size of Kernel :',sizek

help, field, kernel, phase,u, v


if (sizea le sizek) then begin
  bcomp  = dcomplexarr(Nu, Nv) * 0.0                ;; bcomp hat die Dimension von Kernel
  print, 'CONVOL( Kernel, field, /EDGE_ZERO )'   
  bcomp = CONVOL( Kernel, field, /EDGE_ZERO )        ;; dimension Kernel > dimension (field)   
  z_vec = u
  y_vec = v                                          
endif else begin
  bcomp  = dcomplexarr(Nz, Ny) * 0.0                ;; bcomp hat die Dimension von field
  print, '---In diesem Zweig geht noch was schief.---'
  print, 'CONVOL( field, Kernel, /EDGE_ZERO )'   
  bcomp = CONVOL( field, Kernel, /EDGE_ZERO )         ;; dimension field > dimension (Kernel) , das gibt nachfolgend probleme mit dimension von u 
endelse                         

help, bcomp


field=bcomp

if (plot ne 0) then begin
 
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

print,'---------------- propfresnel end ----------------------------------'
return
end
;; end
