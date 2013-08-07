;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <18 Jul 13 17:18:21 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro driftnear, drift=drift, acomp=acomp, areal=areal, aimag=aimag, bcomp=bcomp, breal=breal, bimag=bimag $
           ,bamp=bamp, bphase=bphase, plot=plot $
           ,wavelength=wavelength, y_vec=y_vec, z_vec=z_vec, u=u, v=v
;+
; NAME:
;   driftnear 
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
;   driftnear, acomp=acomp, z_vec=z_vec, y_vec=y_vec, drift = drift
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
;
;
; MODIFICATION HISTORY:
;    23.7.13 RF
;-

;;; 

u1= 'usage: driftnear, [acomp=acomp,][areal=areal,][aimag=aimag,][breal=breal,][bimag=bimag,][bamp=bamp,][bphase=bphase,]'
u2= '[wavelength=wavelength,] y_vec=y_vec, z_vec=z_vec'
usage= u1+u2

print, '------------------ driftnear called ----------------------------'

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



print, 'driftnear start calculation  ---  drift=',drift
ppi = 2.0 * !dpi 
Nz= n_elements(z_vec)
Ny= n_elements(y_vec)
zz= z_vec[Nz-1]- z_vec[0]                                      ;; total width
yy= y_vec[Ny-1]- y_vec[0]                                      ;; total width
k = 2* !dpi/wavelength                                         ;; wavevector

print, 'z_vec[0] = ',z_vec[0]*1e3, ' z_vec[Nz-1] ', z_vec[nz-1]*1e3, ' mm^2 '
print, 'width    = ', zz*1e3, ' x ', yy*1e3, ' mm^2 '
print, 'Nz       = ',   Nz  , ' Ny = ', Ny
;;------------------------- FT of Source field -- exp(-i ...) ----------
  

E0ft= fft(acomp, -1, /center, /double)        ;; Fourier transform of source Field E0, forward 2d fft, centered output
                                              ;; at positions -(Nz/2-1)/(zz),... -1/(zz) , 0 ,  1/(zz), 2/(zz),... (Nz/2-1)/(zz),
print, '---------------- M0 --'
aamp  = dcomplexarr(Nz, Ny) 
aphas = dcomplexarr(Nz, Ny) 
aamp  = abs (E0ft)
aphas = atan(E0ft,/phase)
  
    
u = (dindgen(Nz)/(Nz-1) - 0.5)                ;; runs from -0.5..0.. 0.5 
v = (dindgen(Ny)/(Ny-1) - 0.5)                ;; for even and odd values of Ny, Nz 

u = u * (Nz-1)/zz                             ;; ok with odd number of elements
v = v * (Ny-1)/yy

if n_elements(plot) ne 0 then begin

  window,10, RETAIN=2, XSIZE=400, YSIZE=300 ,XPOS=400, YPOS=850
  mycontour, aamp ,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Fourier transform of input field, amplitude'   

  window,11, RETAIN=2, XSIZE=400, YSIZE=300 ,XPOS=0, YPOS=850
  mycontour, aphas ,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Fourier transform of input field, phase' 
path       = dindgen(Nz,Ny)

endif

print, '--------------- Propagator for driftspace --------------------------'

phase      = dindgen(Nz,Ny)
propagator = dcomplexarr(nz, ny) 
p0         = drift  MOD wavelength

for i=0, Nz-1 do begin
    for j=0, Ny-1 do begin
         arg = 1.0 - (u[i]*wavelength)^2 -  (v[j]*wavelength)^2
         IF (arg>0) THEN BEGIN
           arg            = sqrt(arg)
           phase[i,j]     = ((drift *(arg - 1.0) ) MOD wavelength ) * k + P0  * k 
;;         phase[i,j]     = k * drift* arg   
           propagator[i,j]= complex(cos(phase[i,j]), sin(phase[i,j]), /double)
         ENDIF ELSE BEGIN
           print,'driftnear.pro: sqrt of neg. argument, evanescent waves ',arg, ' i = ',i, 'j = ',j
           arg            = sqrt(-1.0*arg)
           phase          = -1.0 * k * drift* arg  
           if phase le -40 then phase = -40   
           propagator[i,j]= complex(exp(phase), 0 , /double)
         ENDELSE
    endfor
endfor

if n_elements(plot) ne 0 then begin

   M=Nz/2

   print,'++++ M=', M, ' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

   hilf = path[M,M] 

   print, FORMAT = '("u     =   ",E20.14)',u[M]
   print, FORMAT = '("v     =   ",E20.14)',v[M]
   print, FORMAT = '("drift =   ",E20.14)',drift
   print, FORMAT = '("lambda=   ",E20.14)',wavelength
   print, FORMAT = '("p0    =   ",E20.14)',p0
   ;print, FORMAT = '("cos   =   ",f)',cos(p0)
   ;print, FORMAT = '("sin   =   ",f)',sin(p0)
   print, FORMAT = '("PATH  =   ",E20.14)',path[M,M]
   print, FORMAT = '("phase =   ",E20.14)',phase[M,M]
   print, FORMAT = '("2pi   =   ",E20.14)',ppi
   print, FORMAT = '("cos   =   ",f)',cos(phase[M,M])
   print, FORMAT = '("sin   =   ",f)',sin(phase[M,M])
   print, FORMAT = '("hilf  =   ",E20.14)',hilf
   print, FORMAT = '("cos   =   ",f)', cos(hilf*ppi)
   print, FORMAT = '("sin   =   ",f)', sin(hilf*ppi)

   print, FORMAT = '("Propag=   ",2F)',propagator[M,M]
   print, FORMAT = '("propph=   ",F)',atan(propagator[M,M],/phase)
endif


print, '---------------- Modify Field -------------------------------------'

Eft = dcomplexarr(nz, ny) 

Eft = E0ft * propagator



if n_elements(plot) ne 0 then begin

 amp= atan(E0ft,/phase)
 window,12, RETAIN=2, XSIZE=400, YSIZE=300, XPOS=0, YPOS=550 
 mycontour, amp,u, v, xtitle='v_z (1/m)', ytitle='v_y (1/m)', title='Phase of input field'

 propphas= atan(propagator,/phase)
 window,13, RETAIN=2, XSIZE=400, YSIZE=300, XPOS=0, YPOS=250 
 mycontour, propphas,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Phase of Propagator'

 amp= atan(Eft,/phase)
 window,14, RETAIN=2, XSIZE=400, YSIZE=300,XPOS=0, YPOS=0
 mycontour, amp,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Phase before FFT-1'

 dummy = (aphas + propphas ) +ppi
; dummy = dummy MOD ppi
 window,15, RETAIN=2, XSIZE=400, YSIZE=300,XPOS=400, YPOS=0
 mycontour, dummy,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Add Phase before FFT-1'

  
 window,16, RETAIN=2, XSIZE=400, YSIZE=300,XPOS=400, YPOS=250
;; plot , v, phase[*,0] mod ppi  ;; , yrange=[-4,8]
 plot, v, path [*,50], color = 2
;; oplot, v, phase [*,*], color = 3
  
 print, 'Path : Min =', min(path), 'Max = ', max(path), 'Dif= ', max(path)-min(path)
 window,17, RETAIN=2, XSIZE=400, YSIZE=300,XPOS=400, YPOS=550
 mycontour, path,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Path '
  
endif

print, '---------------  Inverse FT to get output field ----- exp(+i ...) -----'

bcomp= fft(Eft, 1, /center, /double)                          


breal = real_part(bcomp)
bimag = imaginary(bcomp)
bamp  = abs      (bcomp)
bphase= atan     (bcomp,/phase)

u=z_vec
v=y_vec

;if n_elements(plot) ne 0 then mycontour, bamp,u*1e3,v*1e3, xtitle='z (mm)', ytitle='y (mm)', title='drift'

print,'---------------- driftnear end ----------------------------------'
return
end
;; end
