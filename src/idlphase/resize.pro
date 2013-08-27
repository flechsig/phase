;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <18 Jul 13 17:18:21 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro resize,  y_vec=y_vec, z_vec=z_vec, field=field, Ninter=Ninter,Nzero=Nzero     
;          
;+
; NAME:
;
;
; PURPOSE:
;   Interpolate between residuals
;   Zero pad  to size Nzero
;   keep pattern centered
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
;   Ninter:      Interpolate Ninter - fold  ,  e.g. Ninter = 2 : two times
;   Nzero:       enlarge field to size Nzero and fill with 0
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
;
; MODIFICATION HISTORY:
;    27.8.2013 RF 
;                 

;;;

u1    = 'usage: resize, y_vec=y_vec, z_vec=z_vec, field=field,Ninter=Ninter,Nzero=Nzero '    
usage = u1

print, '------------------ resize called ----------------------------'

if n_elements(z_vec)      eq 0 then begin print, usage & return & endif
if n_elements(y_vec)      eq 0 then begin print, usage & return & endif
if n_elements(field)      eq 0 then begin print, usage & return & endif
if n_elements(Ninter)     eq 0 then begin Ninter = 0 & endif
if n_elements(Nzero)      eq 0 then begin Nzero  = 0 & endif

print, '------------------resize start caculation -----------------'


size = size(field)

if (size[1] ge size[2]) then sizeField = size[1] else sizeField = size[2]

if (Nzero ne 0) then begin
  help, field
  print, ' Nzero = ',Nzero, ' sizeField = ',sizeField
 
 if (Nzero ge sizeField) then begin
  print, '----------------- Zero padding of field ------', Nzero
 
  dz= z_vec[1]-z_vec[0]
  dy= y_vec[1]-y_vec[0]
  s          = size(field,/DIM)                       ;; s gives the dimensions of field
  Null       = dcomplexarr(NZero,NZero)               ;; make larger, zero padded array
  z0         = (NZero-s[0])/2
  y0         = (Nzero-s[1])/2
  ze         = (NZero+s[0])/2
  ye         = (NZero+s[1])/2
  Null[z0,y0]= field                                 ;; put field in the middle of the larger array
  field      = Null
  

  Null    = dindgen(NZero)*0.0                                    ;; extend z_vec to new values, assume constant stepwidth
  Null[z0]= z_vec
  z_vec   = Null
  for i=0,  (z0-1)    do z_vec[i]= z_vec[z0]   - ( z0 -  i  )*dz
  for i=ze, (Nzero-1) do z_vec[i]= z_vec[ze-1] + ( i  - ze+1)*dz

  Null    = dindgen(NZero)*0.0                                  ;; extend y_vec to new values, assume constant stepwid
  Null[y0]= y_vec
  y_vec   = Null
  for i=0,  (y0-1)    do y_vec[i]= y_vec[y0]   - ( y0 -  i  )*dy
  for i=ye, (Nzero-1) do y_vec[i]= y_vec[ye-1] + ( i  - ye+1)*dy
 endif
endif

;-----------------------------------------------  interpolate

if (Ninter ne 0 ) then begin 
  s     = size(field,/DIM)                       ;; s gives the dimensions of field
  print, 'Dim of zero padded array = ',s, ' Interpolate by ',Ninter

 x=dindgen(Ninter *(S[0]) +1) / Ninter 
 y=dindgen(Ninter *(S[1]) +1) / Ninter 
 field=interpolate(field,x,y,/GRID)

 
 z_vec=interpolate(z_vec,x,/GRID)
 y_vec=interpolate(y_vec,y,/GRID)
 
END

print, '------------------ resize end ----------------------------'

return
end
;; end
