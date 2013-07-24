;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <18 Jul 13 17:18:21 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro driftfreespace, acomp=acomp, areal=areal, aimag=aimag, bcomp=bcomp, breal=breal, bimag=bimag, $
           bamp=bamp, bphase=bphase, drift=drift, plot=plot, $
           wavelength=wavelength, y_vec=y_vec, z_vec=z_vec, u=u, v=v
;+
; NAME:
;   driftfresnel
;
;
; PURPOSE:
;   calculate the electric field after distance 'drift' in vacuum
;   near field
;
;
; CATEGORY:
;   phase_calc
;
;
; CALLING SEQUENCE:
;   driftfreespace, acomp=acomp, z_vec=z_vec, y_vec=y_vec, drift = drift
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
;               areal, aimag are ignored
;   areal:      input field, real part (required)
;   aimag:      input field, imag. part (required)
;   bcomp:      output field, idl complex array
;   bamp:       output field, amplitude
;   bphase:     output field, phase
;   breal:      output field, real part
;   bimag:      output field, imag part
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
;   driftfreespace, areal=zreal, aimag=zimag, breal=z1real, bimag=z1imag, y_vec=y_vec, z_vec=z_vec
;
;
; MODIFICATION HISTORY:
;    23.7.13 RF
;-

;;; 

u1= 'usage: driftfreespace, [acomp=acomp,][areal=areal,][aimag=aimag,][breal=breal,][bimag=bimag,][bamp=bamp,][bphase=bphase,]'
u2= '[wavelength=wavelength,] y_vec=y_vec, z_vec=z_vec'
usage= u1+u2

print, 'driftfreespace called'

if n_elements(drift)      eq 0 then drift     = 1.   
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



print, 'driftfreespace start calculation'

Nz= n_elements(z_vec)
Ny= n_elements(y_vec)
zz= z_vec[nz-1]- z_vec[0]                                      ;; total width
yy= y_vec[ny-1]- y_vec[0]                                      ;; total width
k = 2* !dpi/wavelength                                         ;; wavevector

print, 'z_vec[0] = ',z_vec[0]*1e3, ' z_vec[Nz-1] ', z_vec[nz-1]*1e3, ' mm^2 '
print, 'width = ', zz*1e3, ' x ', yy*1e3, ' mm^2 '
print, 'Nz    = ',   Nz  , ' Ny = ', Ny
;;------------------------- FT of Source field -- exp(-i ...) ----------
  

E0ft = dcomplexarr(Nz, Ny) 

E0ft= fft(acomp, -1, /center, /double)        ;; Fourier transform of source Field E0, forward 2d fft, centered output
                                              ;; at positions -(Nz/2-1)/(zz),... -1/(zz) , 0 ,  1/(zz), 2/(zz),... (Nz/2-1)/(zz),
  
    
u = (dindgen(Nz)/(Nz-1) - 0.5)                ;; runs from -0.5..0.. 0.5 
v = (dindgen(Ny)/(Ny-1) - 0.5)                ;; for even and odd Ny, Nz 

u = u * (Nz-1)/zz                             ; ok with odd number elements
v = v * (Ny-1)/yy
;print, u

if n_elements(plot) ne 0 then begin
  amp = dcomplexarr(Nz, Ny) 

  amp = abs(E0ft)
  window,10, RETAIN=2, XSIZE=500, YSIZE=400 ,XPOS=0, YPOS=770
  mycontour, amp ,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Fourier transform of input field, amplitude'

  amp= atan(E0ft,/phase)
  window,11, RETAIN=2, XSIZE=500, YSIZE=400 ,XPOS=500, YPOS=770
  mycontour, amp ,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Fourier transform of input field, phase'
endif

;;--------------- Propagator for driftspace --------------------------

propagator = dcomplexarr(nz, ny)  
for i=0, Nz-1 do begin
    for j=0, Ny-1 do begin
         arg = 1 -  (u[i]*wavelength)^2 -  (v[j]*wavelength)^2
         IF (arg>0) THEN BEGIN
           arg            = sqrt(arg)
           phase          = k * drift* arg     
           propagator[i,j]= complex(cos(phase), sin(phase), /double)
         ENDIF ELSE BEGIN
           print,'driftfreespace.pro: sqrt of neg. argument, evanescent waves ',arg, ' i = ',i, 'j = ',j
           arg            = sqrt(-1.0*arg)
           phase          = -1.0 * k * drift* arg  
           if phase le -40 then phase = -40   
           propagator[i,j]= complex(exp(phase), 0 , /double)
         ENDELSE
    endfor
endfor


if n_elements(plot) ne 0 then begin
 amp= atan(propagator,/phase)
 window,12, RETAIN=2, XSIZE=500, YSIZE=400,XPOS=0, YPOS=330 
 mycontour, amp,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Phase of Propagator'
endif

;Eft = dcomplexarr(nz, ny) 
Eft = E0ft * propagator

;;---------------  Inverse FT to get output field ----- exp(+i ...) -----

bcomp= fft(Eft, 1, /center, /double)                          

;;------------- calculate real and imag description -------------------- 

breal = real_part(bcomp)
bimag = imaginary(bcomp)
bamp  = abs      (bcomp)
bphase= atan     (bcomp,/phase)

u=z_vec
v=y_vec

;if n_elements(plot) ne 0 then mycontour, bamp,u*1e3,v*1e3, xtitle='z (mm)', ytitle='y (mm)', title='drift'

print,'driftfreespace end'
return
end
;; end
