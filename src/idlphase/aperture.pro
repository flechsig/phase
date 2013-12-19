;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/crl.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <17 Dec 13 13:39:56 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro aperture, emf, example=example, field=field, y_vec=y_vec, z_vec=z_vec, type=type, $
              P1=P1, P2=P2, P3=P3, P4=P4, plot=plot, N=N, size=size, verbose=verbose
;+
; NAME:
;   aperture
;
; PURPOSE:
;   acts as an aperture, or generates wavefield with amplitude according to 'type'
;
;   type 1  : rectangular              P1 = hsize, P2 = vsize, P3= hpos, P4= vpos
;   type 2  : vertical slit            P1 = hsize, P2 = hpos (default= 0)
;   type 3  : horizontal slit          P1 = vsize, P2 = vpos (default= 0)
;   type 4  : vertical slit with LCLS like cylinders   P1 = hsize, P2 = limits,  P3=Radius of cylinders.  RF 18.12.2013
;   type 12 : double slit vertical,    P1 = hsize, P2= hsep
;   type 13 : double slit horizontal,  P1 = vsize, P2= vsep
;   type 20 : circular     P1 = Radius
;                          Radius > 0: central circular part is transparent
;                          Radius < 0: central circular part is oblique
;   type 21 : annular      P1 = Outer radius, P2 = inner radius
;   type 32 : vertical mirror          P1 = length, P2 = grazing angle (rad)
;   type 33 : horizontalal mirror      P1 = length, P2 = grazing angle (rad)
;   type 40 : diamond                  P1= width, P2= hpos, P3= vpos
;   type 50 : triangle                 P1= width, P2= hpos, P3= vpos
;   type 62 : transmission grating ruled vertical,   P1= pitch, P2= duty cycle (opening/pitch), center transparent
;   type 63 : transmission grating ruled horizontal, P1= pitch, P2= duty cycle (opening/pitch), center transparent
;   type 72 : phase grating ruled vertical,   P1= pitch, P2= duty cycle (opening/pitch), P3= phase_shift (rad), center transparent
;   type 73 : phase grating ruled horizontal, P1= pitch, P2= duty cycle (opening/pitch), P3= phase_shift (rad), center transparent
;   type 82 : vertical   slit with round blades, P1 = vsize, P2 = hpos (default= 0), P3= edge radius
;   type 83 : horizontal slit with round blades, P1 = vsize, P2 = hpos (default= 0), P3= edge radius
;
; CATEGORY:
;   phase_calc
;
; CALLING SEQUENCE:
;
;
; INPUTS:
;   see keyword parameters
;
; OPTIONAL INPUTS:
;   emf: emfield structure 
;
; KEYWORD PARAMETERS:
;   field:      input field, idl complex array, 
;   example:    vertical double slit
;   P1:         generic parameter (depends on type)
;   P2:         generic parameter (depends on type)
;   P3:         generic parameter (depends on type)
;   P4:         generic parameter (depends on type)
;   y_vec:      vertical input vector (required) in m
;   z_vec:      horizontal input vector (required) in m
;   type :      type of aperture
;
;   N, size :   if N and size are defined, a wavefield with amplitude
;                according to the aperture type is generated.
; OUTPUTS:
;   see keyword parameters
;
; PROCEDURE:
;   
; EXAMPLE:
;
;  generates circular source with diam. 10 um.   The field size is 80 um, with 80 points
;
;   aperture, field=field,  z_vec=z_vec, y_vec=y_vec, type = 20, P1 = 1e-5, P2=0, plot = 1, N=80, size=8e-5
;
; MODIFICATION HISTORY:
;    14.8.13 RF
;    Aug 13 UF add some types
;    Dec 18 RF add LCLS-like slits
;-

use_struct= (n_params() gt 0) ?  1 : 0

u1= 'usage: aperture, [emf,][field=field, y_vec=y_vec, z_vec=z_vec,] type=type, [P1=P1,] [P2=P2,] '
u2= ' '
usage= u1+u2

print, 'aperture called'

IF KEYWORD_SET(EXAMPLE) THEN BEGIN
    print, '**********************************************************'
    print, 'example: double slit '
    print, '**********************************************************'
    aperture , field=field, y_vec=y_vec, z_vec=z_vec, type=12, p1=2e-3, p2=5e-3, N=51, size=2e-2, /plot
    print, '**********************************************************'
    print, 'end example'
    print, '**********************************************************'
    return
endif  ;; end example

if (n_elements(N) ne 0) and (n_elements(size) ne 0) then begin ;; source
  print, 'create field, N = ', N, 'pts   size = ', size
  create= 1
  z_vec= (dindgen(N) - N/2) * size / N                          
  y_vec= (dindgen(N) - N/2) * size / N                          
  field= dcomplexarr(N, N) + dcomplex(1.0, 0.0)
  help, z_vec, v_vec, field
endif

if use_struct eq 0 then begin
    if n_elements(z_vec) eq 0 then print, usage 
    if n_elements(y_vec) eq 0 then print, usage 
    if n_elements(field) eq 0 then print, usage
endif else begin
    z_vec= emf.z_vec
    y_vec= emf.y_vec
    field= emf.field
endelse

if n_elements(type ) eq 0 then print, usage
if n_elements(plot ) eq 0 then plot=0

print, 'type = ', type
help, field, z_vec, y_vec

nz= n_elements(z_vec)
ny= n_elements(y_vec)

;;print,' P1=' , P1, ' P2= ' ,P2
T  = dblarr(nz, ny)* 0.0 

help, T

;; call case twice for speed
case type of
    1 : begin                                  ;; rectangular 
        if n_elements(P2) eq 0 then P2= P1
        if n_elements(P3) eq 0 then h0= 0.0 else h0= P3
        if n_elements(P4) eq 0 then v0= 0.0 else v0= P4
        p1half= 0.5 * p1
        p2half= 0.5 * p2
        if n_elements(verbose) ne 0 then print, 'rectangular aperture (h x v): ', P1, P2
    end
            
    2 : begin                                 ;; vertical slit
        if n_elements(P2) eq 0 then P2= 0.0
        p1half= 0.5 * p1
        if n_elements(verbose) ne 0 then print, 'vertical slit (hwidth, hpos): ', P1, P2
    end
            
    3 : begin                               ;; horizontal slit
        if n_elements(P2) eq 0 then P2= 0.0  
        p1half= 0.5 * p1
        if n_elements(verbose) ne 0 then print, 'horizontal slit (vwidth, vpos): ', P1, P2
    end

    4 : begin                         ;; cylindrical LCLS slit

        mu3kev      = 39.1e2    ;; 1/m         optical constants of Be - can be extended to other materials
        mu12p4kev   = 0.4e2     ;; 1/m
        rene        = 1.39e15   ;; 1/m^2
        mu          = mu12p4kev         ;; hard for 1 A       
        
        p1half      = 0.5 * p1

        if n_elements(P3) eq 0 then begin 
          print, 'usage aperture  ... P3=Radius' 
          return
        endif 
        
        if n_elements(P2) eq 0 then begin 
          p2half = p1half + p3
        endif else begin
          p2half= 0.5 * p2        
        endelse
    end
   
    12 : begin                         ;; double slit vertical
        if n_elements(P2) eq 0 then P2= P1
        p1half= 0.5 * p1
        p2half= 0.5 * p2
    end
            
    13: begin                      ;; double slit horizontal 
        if n_elements(P2) eq 0 then P2= P1 
    end
    
    20: begin                                    ;; circular 
        
    end
    
    21: begin                                      ;; annular
      if n_elements(P2) eq 0 then P2= P1  
    end
    
    32: begin                ;; vertical mirror (assuming l= infinite)
        ap    = P1  * sin(P2)
        aphalf= 0.5 * ap
        print, 'mirror vertical (w, theta_g): ', P1, P2, ' rad, ap= ', ap
    end
    
    33: begin              ;; horizontal mirror (assuming l= infinite)
        ap    = P1  * sin(P2)
        aphalf= 0.5 * ap
        print, 'mirror horizontal (w, theta_g): ', P1, P2, ' rad, ap= ', ap
    end

    40: begin
        const= P1/sqrt(2.0)
        if n_elements(P2) eq 0 then h0= 0.0 else h0= P2
        if n_elements(P3) eq 0 then v0= 0.0 else v0= P3
        print, 'diamond width= ', P1 
    end

    50: begin
        m= sqrt(3.0)
        const= P1/4.0* sqrt(3.0)
        if n_elements(P2) eq 0 then h0= 0.0 else h0= P2
        if n_elements(P3) eq 0 then v0= 0.0 else v0= P3
        print, 'triangle width= ', P1 
    end

    62: begin
        if n_elements(P2) eq 0 then P2= 0.5
        print, 'vertcally ruled grating, pitch= ', P1, ' duty cycle= ', P2 
       
    end

    63: begin
        if n_elements(P2) eq 0 then P2= 0.5
        print, 'horizontally ruled grating, pitch= ', P1, ' duty cycle= ', P2 
        
    end

    72: begin
        if n_elements(P2) eq 0 then P2= 0.5
        if n_elements(P3) eq 0 then P3= !dpi
        T*= dcomplex(1.0, 0.0)
        help, T
        print, 'vertcally ruled grating, pitch= ', P1, ' duty cycle= ', P2, ' phase_shift= ', P3 
        
    end

    73: begin
        if n_elements(P2) eq 0 then P2= 0.5
        if n_elements(P3) eq 0 then P3= !dpi
        T*= dcomplex(1.0, 0.0)
        help, T
        print, 'horizontally ruled grating, pitch= ', P1, ' duty cycle= ', P2, ' phase_shift= ', P3 
      
    end

    82 : begin                               ;; vertical slit
        if n_elements(P2) eq 0 then P2= 0.0 
        if n_elements(P3) eq 0 then P3= 1e-2 
        p1half= 0.5 * p1
        k= 2.0*!dpi/emf.wavelength
        T*= dcomplex(1.0, 0.0)
        help, T
        if n_elements(verbose) ne 0 then print, 'vertical slit with round edges (vwidth, vpos, edge_radius): ', P1, P2, P3
    end

    83 : begin                               ;; horizontal slit
        if n_elements(P2) eq 0 then P2= 0.0 
        if n_elements(P3) eq 0 then P3= 1e-2 
        p1half= 0.5 * p1
        k= 2.0*!dpi/emf.wavelength
        T*= dcomplex(1.0, 0.0)
        help, T
        if n_elements(verbose) ne 0 then print, 'horizontal slit with round edges (vwidth, vpos, edge_radius): ', P1, P2, P3
    end

    else : begin
        print, ' type ', type, ' not defined'
        return         
    end   
endcase
    

for i=0, nz-1 do begin
    for j=0, ny-1 do begin
  
        case type of

            1 : begin                                  ;; rectangular 
               if ((abs(z_vec[i]- h0) le P1half) and (abs(y_vec[j]- v0) le P2half)) then T[i,j]= double(10.0)
            end
            
            2 : begin                                 ;; vertical slit
               if (abs(z_vec[i]- P2) le P1half) then T[i,j]= double(1.0)
            end
            
            3 : begin                               ;; horizontal slit
               if  (abs(y_vec[j]- P2) le P1half) then T[i,j]= double(1.0)
            end
            
            4 : begin
                pos = abs(z_vec[i]) 
                if   ( pos le p1half) then begin T[i,j]= double(1.0)   ; inside  P1: T=1      
                endif else begin
                  if ( pos ge p2half) then begin T[i,j]= double(0.0)   ; outside P2: T=0      
                endif else begin
                  d     = 2*sqrt(P3^2 - ( P3- (pos - p1half) )^2 )                  
                  f0    = exp(-mu*d/2.0)                                  ;; absorption 
                  f2    = (-1.0) * rene * emf.wavelength * d                  ;; phase shift
                  T[i,j]= f0*complex(cos(f2), sin(f2), /double)
                  endelse
                endelse
               
               end   

            12 : begin                         ;; double slit vertical
               if ((abs(z_vec[i]) le (p2half+p1half)) and (abs(z_vec[i]) ge (p2half-p1half))) $
               then T[i,j]= double(1.0)
            end
            
            13: begin                      ;; double slit horizontal 
               if ( (abs(y_vec[j]) le (p2half+p1half)) and (abs(y_vec[j]) ge (p2half-p1half))) $
               then T[i,j]= double(1.0)
            end
            
            20: begin                                    ;; circular 
               rr= (z_vec[i]^2 + y_vec[j]^2)           
               if ((P1 ge 0) and (rr le P1^2)) then T[i,j]= double(1.0)
               if ((P1 le 0) and (rr ge P1^2)) then T[i,j]= double(1.0)
            end
            
            21: begin                                      ;; annular
               rr= (z_vec[i]^2 + y_vec[j]^2)           
               if ((rr le P1^2) and (rr ge P2^2)) then T[i,j]= double(1.0)                 
;                  if (rr le P1^2) then T[i,j]=double(1.0)
            end
            
            32: begin                             ;; vertical mirror (assuming l= infinite)
               if  (abs(y_vec[j]) le aphalf) then T[i,j]= double(1.0)
            end

            33: begin                             ;; horizontal mirror (assuming l= infinite)
               if  (abs(z_vec[i]) le aphalf) then T[i,j]= double(1.0)
           end

           40: begin                             ;; diamond (Karo)
               if (y_vec[j]-v0 le z_vec[i]-h0 + const) and $
                 (y_vec[j]-v0 le (-1.0)*(z_vec[i]-h0) + const)  and $
                 (y_vec[j]-v0 ge z_vec[i]-h0 - const)  and $
                 (y_vec[j]-v0 ge (-1.0)*(z_vec[i]-h0) - const) $
                 then T[i,j]= double(1.0)
           end

           50: begin                             ;; triangle
               if (y_vec[j]-v0 le m*(z_vec[i]-h0) + const) and $
                 (y_vec[j]-v0 le (-1.0)*m*(z_vec[i]-h0) + const)  and $
                 (y_vec[j]-v0 ge ((-1.0)* const) )  $
                 then T[i,j]= double(1.0)
           end

           62 : begin                                 ;; vertical grating
               if ((abs(z_vec[i])+ 0.5* P2)/P1- floor((abs(z_vec[i])+ 0.5* P2)/P1)) le P2/2.0 then T[i,j]= double(1.0)
           end
           
           63 : begin                               ;; horizontal slit
               if ((abs(y_vec[j])+ 0.5* P2)/P1- floor((abs(y_vec[j])+ 0.5* P2)/P1)) le P2/2.0 then T[i,j]= double(1.0)
           end
           
           
           72 : begin                                 ;; vertical slit
               if ((abs(z_vec[i])+ 0.5* P2)/P1- floor((abs(z_vec[i])+ 0.5* P2)/P1)) le P2/2.0 then T[i,j]= dcomplex(cos(P3), sin(P3))
           end
           
           73 : begin                               ;; horizontal slit
               if ((abs(y_vec[j])+ 0.5* P2)/P1- floor((abs(y_vec[j])+ 0.5* P2)/P1)) le P2/2.0 then T[i,j]= dcomplex(cos(P3), sin(P3))
           end
           
           83 : begin                               ;; horizontal slit round edge
               amp= 0.0
               pha= 0.0
               if  (abs(y_vec[j]- P2) le P1half) then begin   ;; inside transparent
                   amp= 1.0
                   pha= 0.0
                   if i eq 0 then print, 'yinn=', y_vec[j]
               endif else begin
                   if  (abs(y_vec[j]- P2)- P3 le P1half) then begin
                       ;;             erst mal ausfuehrlich und ohne p2 (zentrierter schlitz)
                       amp= 1.0
                       ;; dy/dx (y)= +/- x/sqrt(r^2-x^2) 
                       y0_betrag= p1half+p3            ;; halbe Breite + radius 
                       yy= abs(y_vec[j])- p1half       ;; immer > 0
                       if yy gt 1e-12 then begin
                           xx= -1.0* sqrt(p3^2- (p3-yy)^2) ;; x immer negativ
                           dydx= -1.0* xx/sqrt(p3^2-xx^2)        ;; positiv for y < 0
                           thetag= atan(dydx)       ;; positiv fuer y < 0
                           if thetag gt (20e-3 * !dpi) then amp= 1e-2 ;; reflectivity - to be improved
                           if y_vec[j] gt 0.0 then thetag*= -1.0 
                       endif else thetag= 0.0
                       
                       pha= 2.0* k * thetag
                       if i eq 0 then print, 'yout=', y_vec[j], ' amp, theta, yy', amp, thetag, yy
                   endif 
                   ;; if i eq 0 then print, 'yganzout=', y_vec[j]
               endelse
               T[i,j]= dcomplex(amp*cos(pha), amp*sin(pha))
           end

           else : begin
               print, ' type ', type, ' not defined'
               return         
            end   
        endcase
        
    endfor
endfor

if n_elements(ap) then print, 'aperture size (mm)= ', ap*1e3

if (plot ne 0) then begin
     window, 20, RETAIN=2, XSIZE=400, YSIZE=300 ,XPOS=0, YPOS=0
     mycontour, T, z_vec*1e3, y_vec*1e3, xtitle='z (mm) ', ytitle='y (mm)', $
                title='Transmission Aperture' + string(type) ;;,xrange=[-0.3,0.3] ,yrange=[-0.3,0.3]
endif

field = field * T 

if use_struct eq 1 then begin
    emf.field= field
    emf.y_vec= y_vec
    emf.z_vec= z_vec
    print, 'fill emfield structure'
endif

print, 'aperture end'
return
end
;; end
