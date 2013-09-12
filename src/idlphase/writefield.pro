;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <29 Aug 13 16:25:27 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro writefield , filename=filename, field=field, y_vec=y_vec, z_vec=z_vec, zmin=zmin,zmax=zmax,ymin=ymin,ymax=ymax

;+,
; NAME:
;  writefield 
;
;
; PURPOSE:
;   writes a field to file, format:   z  y field
;   
;
;
; CATEGORY:
;   phase_calc 
;
;
; CALLING SEQUENCE:
;  
;  pro writefield , filename=filename, field=field, y_vec=y_vec, z_vec=z_vec
;
;
; INPUTS:
;   
;
;
; OPTIONAL INPUTS:
;   no
;
;
; KEYWORD PARAMETERS:
;
;   filename  : Name of file
;   field     : input field (required),
;                idl complex array, 
;               will be overwritten to give results.
;   y_vec     : vertical input vector   (required) in m
;   z_vec     : horizontal input vector (required) in m
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
;;
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
; MODIFICATION HISTORY:
;    30.8.13 RF
; 
;
;- 

u1= 'usage: writefield, field=field, z_vec=z_vec, y_vec=y_vec '
usage= u1

ZminLimit = 0;
ZmaxLimit = 0;
YminLimit = 0;
YmaxLimit = 0;

if n_elements(filename)   eq 0 then begin print,'filename missing: '+ usage & return & endif
if n_elements(z_vec)      eq 0 then begin print,'z_vec    missing: '+ usage & return & endif
if n_elements(y_vec)      eq 0 then begin print,'y_vec    missing: '+ usage & return & endif
if n_elements(field)      eq 0 then begin print,'field    missing: '+ usage & return & endif

if n_elements(zmin)       ne 0 then begin  ZminLimit=1 & endif
if n_elements(zmax)       ne 0 then begin  ZmaxLimit=1 & endif
if n_elements(ymin)       ne 0 then begin  YminLimit=1 & endif
if n_elements(ymax)       ne 0 then begin  YmaxLimit=1 & endif

print, 'ZminLimit = ',ZminLimit
print, 'ZmaxLimit = ',ZmaxLimit
print, 'YminLimit = ',YminLimit
print, 'YmaxLimit = ',YmaxLimit

Nz  = n_elements(z_vec)
Ny  = n_elements(y_vec)


GET_LUN, f
OPENW, f, filename


for i=0, Nz-1 do begin
   write=1
   if ((ZminLimit ne 0) && (z_vec[i] le zmin)) then write =0
   if ((ZmaxLimit ne 0) && (z_vec[i] ge zmax)) then write =0
 
   if (write eq 1) then begin
       for j=0, Ny-1 do begin
         write=1
         if ((YminLimit ne 0) && (y_vec[j] le ymin)) then write =0
         if ((YmaxLimit ne 0) && (y_vec[j] ge ymax)) then write =0

         if (write eq 1) then   PRINTF, f, z_vec[i], ' ', y_vec[j], ' ', field[i,j]
      endfor
      PRINTF ,f
;      print, ' write LF for i = ',i, ' ',z_vec[i]
   endif
endfor

CLOSE, f
FREE_LUN,f
return
end
