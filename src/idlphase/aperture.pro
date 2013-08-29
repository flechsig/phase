;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/crl.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <29 Aug 13 13:52:53 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro aperture, field=field, y_vec=y_vec, z_vec=z_vec, type=type, P1=P1, P2=P2,  plot=plot, N=N, size=size
;+
; NAME:
;   aperture
;
;
; PURPOSE:
;
;   acts as an aperture, or generates wavefield with amplitude according to 'type'
;
;
;   type 1  : rectangular   P1 = hsize, P2 = vsize
;   type 10 : double slit vertical,    P1 = hsize, P2= hsep
;   type 11 : double slit horizontal,  P1 = vsize, P2= vsep
;   type 20 : circular     P1 = Radius
;                          Radius > 0: central circular part is transparent
;                          Radius < 0: central circular part is oblique
;   type 21 : annular      P1 = Outer radius, P2 = inner radius
;
; CATEGORY:
;   phase_calc
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;   see keyword parameters
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;   field:      input field, idl complex array, 
;   y_vec:      vertical input vector (required) in m
;   z_vec:      horizontal input vector (required) in m
;   type :      type of aperture
;
;   N, size :   if N and size are defined, a wavefield with amplitude
;                according to the aperture type is generated.
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
;  generates circular source with diam. 10 um.   The field size is 80 um, with 80 points
;
;   aperture, field=field,  z_vec=z_vec, y_vec=y_vec, type = 20, P1 = 1e-5, P2=0, plot = 1, N=80, size=8e-5
;
; MODIFICATION HISTORY:
;    14.8.13 RF
;-


u1= 'usage: aperture field=field,y_vec=y_vec, z_vec=z_vec,type = type, [P1=P1,] [P2=P2,] '
u2= ' '
usage= u1+u2

print, 'aperture called'



if (n_elements(N) ne 0) and (n_elements(size) ne 0)   then begin
  print, 'create field, N = ', N, 'pts   size = ', size
  create = 1
  z_vec=(dindgen(N) - N/2) * size / N                          
  y_vec=(dindgen(N) - N/2) * size / N                          
  field = dcomplexarr(N,N) + dcomplex(1.0,0.0)
  help, z_vec, v_vec, field
endif

if n_elements(z_vec) eq 0 then print, usage 
if n_elements(y_vec) eq 0 then print, usage 
if n_elements(field) eq 0 then print, usage
if n_elements(type ) eq 0 then print, usage
if n_elements(plot ) eq 0 then plot=0
if n_elements(P2 )   eq 0 then P2=1


print, 'type = ', type

help, field, z_vec, y_vec

nz= n_elements(z_vec)
ny= n_elements(y_vec)

print,' P1=' , P1, ' P2= ' ,P2
T  = dblarr(nz, ny)* 0.0 

help, T

for i=0, nz-1 do begin
    for j=0, ny-1 do begin
  
        case type of

            1 : begin                                  ;; rectangular 
                if (  (abs(z_vec[i]) le P1) and (abs(y_vec[j]) le P2) ) then T[i,j]= double(1.0)
            end

            10 : begin                                 ;; double slit vertical
                halfsep= 0.5* P2
                halfsiz= 0.5* P1
                if ( (abs(z_vec[i]) le (halfsep+halfsiz)) and (abs(z_vec[i]) ge (halfsep-halfsiz)) ) then T[i,j]= double(1.0)
            end

            11 : begin                                 ;; double slit horizontal 
                halfsep= 0.5* P2
                halfsiz= 0.5* P1
                if ( (abs(y_vec[j]) le (halfsep+halfsiz)) and (abs(y_vec[j]) ge (halfsep-halfsiz))) then T[i,j]= double(1.0)
            end


           20 : begin                                     ;; circular 
                  rr= (z_vec[i]^2 + y_vec[j]^2)           
                  if ((P1 ge 0) and (rr le P1^2)) then T[i,j]=double(1.0)
                  if ((P1 le 0) and (rr ge P1^2)) then T[i,j]=double(1.0)
                 end

           21 : begin                                      ;; annular
                  rr= (z_vec[i]^2 + y_vec[j]^2)           
                  if ((rr le P1^2) and (rr ge P2^2)) then T[i,j]=double(1.0)                 
;                  if (rr le P1^2) then T[i,j]=double(1.0)
                end

         else : begin
                 print, ' type ', type, ' not defined'
                 exit         
                end   
         endcase
        
    endfor
endfor

if (plot ne 0) then begin
     window,20, RETAIN=2, XSIZE=400, YSIZE=300 ,XPOS=0,YPOS=0
     mycontour, T,z_vec*1e3,y_vec*1e3, xtitle='z (mm) ', ytitle='y (mm)', title='Transmission Aperture' + string(type) ;;,xrange=[-0.3,0.3] ,yrange=[-0.3,0.3]
endif

field = field * T 
 
print, 'aperture end'
return
end
;; end
