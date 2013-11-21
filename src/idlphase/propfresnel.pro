;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <21 Nov 13 12:23:40 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro propfresnel, drift=drift, y_vec=y_vec, z_vec=z_vec, field=field $
                 , plot=plot, wavelength=wavelength           
;
;+
; NAME:
;   propfresnel
;
; PURPOSE:
;   calculate the electric field after some distance in vacuum
;   Fresnel approximation with one Fouriertransform, the grid changes: 
;   \Delta z_1= \frac{\lambda  x}{\Delta z_0 M and 
;   \Delta y_1= \frac{\lambda  x}{\Delta y_0 N
;   assuming drift= x and number of gridpoints M, N
;
; CATEGORY:
;   phase_calc
;
; CALLING SEQUENCE:
;  
; INPUTS:
;   
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;   field:      input field, idl complex array,
;   y_vec:      input vector vertical, (required) in m
;   z_vec:      input vector horizontal (required) in m
;         :      above three fields will be overwritten
;   drift:      drift distance in m
;   plot:       make contour plot of amplitude
;   wavelength: the wavelength in m
;
; OUTPUTS:
;   see keyword parameters
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;   no
;
; EXAMPLE:
;   idl> 
;
;
; MODIFICATION HISTORY:
;    11.7.2013 UF
;    27.8.2013 RF renamed to propfourier 
;                 opererates on input field
;    12.9.2013 RF sign change when calculateing the  origin
;              RF width zz, yy corrected for N/(N-1)
;
;;; UF I follow the the reference follath:2013f

u1    = 'usage: propfresnel, y_vec=y_vec, z_vec=z_vec, field=field,drift=drift'
u2    = '[, plot=plot][, wavelength=wavelength]'    
usage = u1+u2

print, '------------------ propfresnel called ----------------------------'

if n_elements(drift)      eq 0 then begin print,'drift missing: '+ usage & return & endif
if n_elements(z_vec)      eq 0 then begin print,'z_vec missing: '+ usage & return & endif
if n_elements(y_vec)      eq 0 then begin print,'y_vec missing: '+ usage & return & endif
if n_elements(field)      eq 0 then begin print,'field missing: '+ usage & return & endif
if n_elements(wavelength) eq 0 then wavelength= 1d-10  
if n_elements(plot  )     eq 0 then plot=0

;------------------------- I want wavelength to be a scalar ---------------
dum=size(wavelength)
if (dum[0] gt 0) then begin
   if (dum[1] eq 0) then begin
      print, 'wavelength not defined ', dum
      return
   endif
   print, 'wavelength of type array, dim =',dum[0],' convert to scalar.'
   wavelength = wavelength[0]
endif

k  = 2* !dpi/wavelength

nz = n_elements(z_vec)
ny = n_elements(y_vec)
zz = (z_vec[nz-1]- z_vec[0] ) * nz / (nz-1)                      ;; total width, 12.9.2013: corrected for nz/(nz-1)
yy = (y_vec[ny-1]- y_vec[0] ) * ny / (ny-1)                      ;; total height,     -"-

print, 'width = ', zz*1e3, ' x ', yy*1e3, ' mm^2 '
print, 'drift = ', drift
;;------------------------ Multipy input field with phase factor -------

driftarr= dcomplexarr(nz, ny)                                    ;; make a complex array
for i=0, nz-1 do begin
    for j=0, ny-1 do begin
        phase= k*(z_vec[i]^2 + y_vec[j]^2)/(2.0*drift)         
        driftarr[i,j]= complex(cos(phase), sin(phase), /double)
    endfor
endfor

modfield = field * driftarr                                       

print, '--------------- FT of Source field ------------------ exp(-i ...)'

newfield = fft(modfield, -1, /center, /double)                      ;; forward 2d fft, centered output

u0       = dindgen(nz)/(nz-1) - 0.5                                     ;; define the vectors in the image plane
v0       = dindgen(ny)/(ny-1) - 0.5   
uscale   = (drift*wavelength)/zz * nz                                    ;; why is uscale,vscale of type array[1] ?
vscale   = (drift*wavelength)/yy * ny                                    ;;-> wavelength comes as array[1], solved
u        = u0*uscale
v        = v0*vscale
z0       = z_vec[0]
y0       = y_vec[0]


print, ' z0 = ',z0, ' y0 = ',y0
print, ' nz = ',nz, ' ny = ',ny

;;-------------------- Multiply new field with phase factor ----------------

scale   = dcomplexarr(nz, ny)                                    ;; make a complex array
for i=0, nz-1 do begin
    for j=0, ny-1 do begin
        phase = (u[i]^2 + v[j]^2) * k/(2.0*drift)             
        phase = phase + (u[i]*z0 + v[j] * y0) * k / drift        ;; set origin  12.9.2013: changed sign from - to +
        scale[i,j]= complex(cos(phase), sin(phase), /double)   
    endfor
endfor

scale1 = complex(cos(k*drift), sin(k*drift)      , /double)             ;; why is this of type array[1] ?
scale2 = complex(0.0         , (wavelength*drift), /double)             ;; -> wavelength comes as array[1] -> k is array[1]
field  = zz * yy * newfield * scale  * scale1/ scale2
z_vec  = u
y_vec  = v

print, '------------------ propfresnel end ----------------------------'

return
end
;; end
