;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <04 Mar 14 14:04:04 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro propfourier, emf, drift=drift, example=example, field=field, y_vec=y_vec, z_vec=z_vec, $
                 plot=plot, wavelength=wavelength , filter=filter 

;+,
; NAME:
;   propfourier 
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
;   propfourier, [emf,][field=field, y_vec=y_vec, z_vec=z_vec,] drift=drift  $
;                [,/plot][,wavelength=wavelength][,filter=filter][,/example]
;
;
; INPUTS:
;   
;
;
; OPTIONAL INPUTS:
;   emf: emfield structure
;
;
; KEYWORD PARAMETERS:
;   example:    plot example
;   field     : input field (required),
;                idl complex array, 
;               will be overwritten to give results.
;   y_vec     : vertical input vector   (required) in m
;   z_vec     : horizontal input vector (required) in m
;   drift     : drift distance in m
;   wavelength: the wavelength in m
;   plot      : make contour plot of amplitude
;   filter    : filter in fourierspace 
;         1   : Hanning filter
;         2   : Hanning filter twice applied                
;
; OUTPUTS:
;    field    : field distribution in distance "drift"
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
;    idl 7.1 does not work correctly- gives unsymmetric results, works
;    with idl 8.1 (reason: keyword /center in fft)
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
;   idl> propfourier, field=field, y_vec=y_vec, z_vec=z_vec, $
;   drift=10., wavelength=wavelength 
;
; MODIFICATION HISTORY:
;    23.7.13 RF
;    12.8.13 RF, renamed from driftnear.pro to driftFourier.pro
;            RF, renamed to driftfourier.pro
; 
;
;- 

use_struct= (n_params() gt 0) ?  1 : 0

u1= 'usage: propfourier, [emf,][field=field, wavelength=wavelength, z_vec=z_vec, y_vec=y_vec,] drift = drift'
u2= ' [filter=filter,] [plot=plot]'
usage= u1+u2

print, '------------------ propfourier called ----------------------------'

IF KEYWORD_SET(EXAMPLE) THEN BEGIN
    print, '**********************************************************'
    print, 'example: '
    print, '**********************************************************'
    gaussbeam, dist=0, Nz=11, sizez= 6e-4, z_vec=z_vec, y_vec=y_vec, field=field , w0=27e-6,  wavelength=1e-10
    propfourier , drift=100, z_vec=z_vec, y_vec=y_vec, field=field, wavelength=wavelength, /plot
    print, '**********************************************************'
    print, 'in case the image is unsymmetric your idl version has a problem'
    print, 'end example'
    print, '**********************************************************'
    return
endif  ;; end example

if n_elements(drift)      eq 0 then begin print,'drift missing: '+ usage & return & endif
if use_struct eq 0 then begin
    if n_elements(z_vec)      eq 0 then begin print,'z_vec missing: '+ usage & return & endif
    if n_elements(y_vec)      eq 0 then begin print,'y_vec missing: '+ usage & return & endif
    if n_elements(field)      eq 0 then begin print,'field missing: '+ usage & return & endif
    if n_elements(wavelength) eq 0 then begin print,'wavelength missing: '+ usage & return & endif 
endif else begin
    z_vec=      emf.z_vec
    y_vec=      emf.y_vec
    field=      emf.field
    wavelength= emf.wavelength
endelse

if n_elements(filter)     eq 0 then filter=0
if n_elements(plot  )     eq 0 then plot=0

drift      = double(drift)
wavelength = double(wavelength)

print, '--------------- propfourier start calculation  --- drift=',drift

ppi = 2.0 * !dpi 
Nz  = n_elements(z_vec)
Ny  = n_elements(y_vec)
zz  = z_vec[Nz-1]- z_vec[0]                                    ;; total width
yy  = y_vec[Ny-1]- y_vec[0]                                    ;; total width
k   = 2* !dpi/wavelength                                       ;; wavevector

print, 'filter   = ', filter,      '  plot        = ',plot
print, 'Nz, Ny   = ',   Nz  ,      ' , ', Ny
print, 'z_vec[0] = ',z_vec[0]*1e3, '   z_vec[Nz-1] = ', z_vec[nz-1]*1e3, ' mm '
print, 'width    = ', zz*1e3,      '   height      = ', yy*1e3,  ' mm '

u = (dindgen(Nz)/(Nz-1) - 0.5)                ;; runs from -0.5..0.. 0.5 
v = (dindgen(Ny)/(Ny-1) - 0.5)                ;; for even and odd values of Ny, Nz 
u = u * (Nz-1)/zz                             ;; ok with odd number of elements
v = v * (Ny-1)/yy
print, 'u:   min = ', min(u)      ,'   max         = ', max(u)

print, '--------------- FT of Source field ------------------ exp(-i ...)'

;E0ft = dcomplexarr(Nz, Ny) 
 
E0ft = fft(field, -1, /center, /double)       ;; Fourier transform of source Field E0, forward 2d fft, centered output
                                              ;; at positions -(Nz/2-1)/(zz),... -1/(zz) , 0 ,  1/(zz), 2/(zz),... (Nz/2-1)/(zz),    

if (plot ne 0) then begin
  aamp  = dindgen(Nz, Ny) 
  aphas = dindgen(Nz, Ny) 
  aamp  = abs (E0ft)
  aphas = atan(E0ft,/phase)
  M=Nz/2
  N=Ny/2

  window,10, RETAIN=2, XSIZE=400, YSIZE=300 ,XPOS=0, YPOS=850
   mycontour, aamp ,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Fourier transform of input field, amplitude'   
;    plot , u, aamp[*,N], xtitle='  v_z (1/m)',  title='Fourier transform of input field, amplitude' , xrange=[0,8e5],psym=4  
;    oplot, u, aamp[*,N]  

  window,11, RETAIN=2, XSIZE=400, YSIZE=300 ,XPOS=400, YPOS=850
   mycontour, aphas ,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Fourier transform of input field, phase' 
 ;   plot , u, aphas[*,N], xtitle='  v_z (1/m)',  title='Fourier transform of input field, phas', xrange=[0,8e5],psym=4     
;    oplot, u, aphas[*,N]

  path       = dindgen(Nz,Ny)

endif

print, '--------------- Propagator for free space ------------------------'

phase      = dindgen(Nz,Ny)
propagator = dcomplexarr(nz, ny) 
p0         = drift MOD wavelength

for i=0, Nz-1 do begin
    for j=0, Ny-1 do begin
         arg = 1.0 - (u[i]*wavelength)^2 - (v[j]*wavelength)^2
         IF (arg>0) THEN BEGIN
           arg           = sqrt(arg)
           phase[i,j]    = ((drift *(arg - 1.0) ) MOD wavelength ) * k + P0  * k   ;; numerically more accurate than next line
;;         phase[i,j]    = k * drift* arg                                          ;; Phase according text book
           propagator[i,j]= complex( cos(phase[i,j]), sin(phase[i,j]), /double)
         ENDIF ELSE BEGIN  
           print,'driftnear.pro: sqrt of neg. argument, evanescent waves ', arg, ' i = ',i, 'j = ',j
           arg            = sqrt(-1.0*arg)
           phase          = -1.0 * k * drift* arg  
           if (phase le -40) then phase = -40   
           propagator[i,j]= complex(exp(phase), 0 , /double)
         ENDELSE
    endfor
endfor


print, '--------------- Propagate in Fourier space -----------------------'

;Eft = dcomplexarr(nz, ny) 
Eft = E0ft * propagator

if (plot ne 0) then begin

; amp= abs(propagator)
; amp=amp/max(amp)
; window,15, RETAIN=2, XSIZE=400, YSIZE=300,XPOS=0, YPOS=550
;  mycontour, amp,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Amplitude of Propagator'

 propphas= atan(propagator,/phase)
 window,14, RETAIN=2, XSIZE=400, YSIZE=300, XPOS=400, YPOS=550 
  mycontour, propphas,u, v, xtitle='  v_z (1/m)', ytitle='v_y (1/m)', title='Phase of Propagator'
  
endif


if (filter ne 0) then begin

 f =  HANNING(Nz,Ny,alpha=0.5,/double)

 if ((filter eq 1) or (filter eq 2 )) then begin
  print, ' Apply Hanning  filter'
  Eft = Eft * f
 endif
 
 if (filter eq 2) then begin
  print, ' Apply Hanning  filter twice'
  Eft = Eft *f  
 endif

endif


if (plot ne 0) then begin
 amp= abs(Eft)
 window,17, RETAIN=2, XSIZE=400, YSIZE=300,XPOS=0, YPOS=250
 plot, u,  amp[*,N], xtitle='  u_z (1/m)', title='Amplitude before FFT-1', xrange=[0,8e5],psym=4  
endif


print, '--------------- Inverse FT to get output field ------ exp(+i ...)'

E0ft      =0  ;; free memory
propagator=0  ;; free memory
phase     =0  ;; free memory

field= fft(Eft, 1, /center, /double)     

if use_struct eq 1 then begin
    emf.field= field
    emf.y_vec= y_vec
    emf.z_vec= z_vec
    emf.wavelength= wavelength
    print, 'fill emfield structure'
endif                     

print, '--------------- propfourier end ----------------------------------'
return
end
