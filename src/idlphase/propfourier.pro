;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <18 Jul 13 17:18:21 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro propfourier, drift=drift, y_vec=y_vec, z_vec=z_vec, field=field $
                 , plot=plot, wavelength=wavelength , filter=filter 

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
;    12.8.13 RF, renamed from driftnear.pro to driftFourier.pro
;-

;;; 

u1= 'usage: propfourier, field=field, wavelength=wavelength, z_vec=z_vec, y_vec=y_vec, drift = drift'
u2= ' [filter=filter,] [plot=plot]'
usage= u1+u2

print, '------------------ propfourier called ----------------------------'

if n_elements(drift)      eq 0 then begin print, usage & return & endif
if n_elements(z_vec)      eq 0 then begin print, usage & return & endif
if n_elements(y_vec)      eq 0 then begin print, usage & return & endif
if n_elements(field)      eq 0 then begin print, usage & return & endif
if n_elements(wavelength) eq 0 then wavelength= 1e-10  
if n_elements(filter)     eq 0 then filter=0
if n_elements(plot  )     eq 0 then plot=0


print, '------------------ propfourier start calculation  ---  drift=',drift

ppi = 2.0 * !dpi 
Nz  = n_elements(z_vec)
Ny  = n_elements(y_vec)
zz  = z_vec[Nz-1]- z_vec[0]                                    ;; total width
yy  = y_vec[Ny-1]- y_vec[0]                                    ;; total width
k   = 2* !dpi/wavelength                                       ;; wavevector

print, 'z_vec[0] = ',z_vec[0]*1e3, ' z_vec[Nz-1] ', z_vec[nz-1]*1e3, ' mm^2 '
print, 'width    = ', zz*1e3, ' x ', yy*1e3, ' mm^2 '
print, 'Nz       = ',   Nz  , ' Ny = ', Ny
print, 'filter   = ', filter, ' plot = ',plot
;;------------------------ FT of Source field -- exp(-i ...) ----------

E0ft = dcomplexarr(Nz, Ny) 
 
E0ft= fft(field, -1, /center, /double)        ;; Fourier transform of source Field E0, forward 2d fft, centered output
                                              ;; at positions -(Nz/2-1)/(zz),... -1/(zz) , 0 ,  1/(zz), 2/(zz),... (Nz/2-1)/(zz),
    
u = (dindgen(Nz)/(Nz-1) - 0.5)                ;; runs from -0.5..0.. 0.5 
v = (dindgen(Ny)/(Ny-1) - 0.5)                ;; for even and odd values of Ny, Nz 
u = u * (Nz-1)/zz                             ;; ok with odd number of elements
v = v * (Ny-1)/yy
print, ' u: ', min(u),' ... ', max(u)

if (plot ne 0) then begin
   aamp  = dindgen(Nz, Ny) 
   aphas = dindgen(Nz, Ny) 
   aamp  = abs (E0ft)
   aphas = atan(E0ft,/phase)
   M=Nz/2
   N=Ny/2

  window,10, RETAIN=2, XSIZE=400, YSIZE=300 ,XPOS=0, YPOS=850
;  mycontour, aamp ,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Fourier transform of input field, amplitude'   
  plot , u, aamp[*,N], xtitle='  v_z (1/m)',  title='Fourier transform of input field, amplitude' , xrange=[0,8e5],psym=4  
  oplot, u, aamp[*,N]  

  window,11, RETAIN=2, XSIZE=400, YSIZE=300 ,XPOS=400, YPOS=850
;  mycontour, aphas ,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Fourier transform of input field, phase' 
  plot , u, aphas[*,N], xtitle='  v_z (1/m)',  title='Fourier transform of input field, phas', xrange=[0,8e5],psym=4     
  oplot, u, aphas[*,N]

  path       = dindgen(Nz,Ny)

endif

print, '--------------- Propagator for free space --------------------------'

phase      = dindgen(Nz,Ny)
propagator = dcomplexarr(nz, ny) 
p0         = drift  MOD wavelength

for i=0, Nz-1 do begin
    for j=0, Ny-1 do begin
         arg = 1.0 - (u[i]*wavelength)^2 -  (v[j]*wavelength)^2
         IF (arg>0) THEN BEGIN
           arg            = sqrt(arg)
          phase[i,j]    = ((drift *(arg - 1.0) ) MOD wavelength ) * k + P0  * k 
;;              phase[i,j]    = k * drift* arg   
          propagator[i,j]= complex( cos(phase[i,j]), sin(phase[i,j]), /double)
         ENDIF ELSE BEGIN  
           print,'driftnear.pro: sqrt of neg. argument, evanescent waves ',arg, ' i = ',i, 'j = ',j
           arg            = sqrt(-1.0*arg)
           phase          = -1.0 * k * drift* arg  
           if phase le -40 then phase = -40   
           propagator[i,j]= complex(exp(phase), 0 , /double)
         ENDELSE
    endfor
endfor


print, '---------------- Modify Field -------------------------------------'

Eft = dcomplexarr(nz, ny) 
Eft = E0ft * propagator



if (plot ne 0) then begin

 amp= abs(propagator)
 window,13, RETAIN=2, XSIZE=400, YSIZE=300,XPOS=0, YPOS=550
 plot, u,  amp[*,N]     ,xtitle='  u_z (1/m)', title='Amplitude of Propagator' ;;, xrange=[0,8e5],psym=4  

 propphas= atan(propagator,/phase)
 window,14, RETAIN=2, XSIZE=400, YSIZE=300, XPOS=400, YPOS=550 
;mycontour, propphas,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Phase of Propagator'
 plot, u,  propphas[*,N],xtitle='  u_z (1/m)', title='Phase of Propagator';; , xrange=[0,8e5],psym=4  


 if (filter  ne 0) then begin
   amp= abs(Eft)
   window,15, RETAIN=2, XSIZE=400, YSIZE=300,XPOS=0, YPOS=250
   plot, u,  amp[*,N],xtitle='  u_z (1/m)', title='Amplitude before filtering', xrange=[0,8e5],psym=4  
 endif
 
 amp= atan(Eft,/phase)
 window,16, RETAIN=2, XSIZE=400, YSIZE=300,XPOS=400, YPOS=250
 plot, u,  amp[*,N],xtitle='  u_z (1/m)', title='Phase before FFT-1' ;;, xrange=[0,8e4],psym=4  

; window,16, RETAIN=2, XSIZE=400, YSIZE=300,XPOS=400, YPOS=250
; plot , v, phase[*,(Nz-1)/2 ] mod ppi  ;; , yrange=[-4,8]
;; plot, v, path [*,50], color = 2
;; oplot, v, phase [*,*], color = 3
;  
; print, 'Path : Min =', min(path), 'Max = ', max(path), 'Dif= ', max(path)-min(path)
; window,17, RETAIN=2, XSIZE=400, YSIZE=300,XPOS=400, YPOS=550
; plot,u, path[100,*], xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Path '
  
endif




if filter ne 0 then begin
 f =  HANNING(Nz,Ny,alpha=0.5,/double)

 print, ' Apply Hanning  filter'
 Eft = Eft * f

 if (filter eq 2) then begin
  print, ' Apply Hanning  filter'
  Eft = Eft *f  
 endif

endif


if (plot ne 0) then begin
 amp= abs(Eft)
 window,17, RETAIN=2, XSIZE=400, YSIZE=300,XPOS=0, YPOS=250
 plot, u,  amp[*,N],xtitle='  u_z (1/m)', title='Amplitude before FFT-1', xrange=[0,8e5],psym=4  
endif



print, '---------------  Inverse FT to get output field ----- exp(+i ...) -----'

field= fft(Eft, 1, /center, /double)                          



print,'---------------- propfourier end ----------------------------------'
return
end
