;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/drift.pro
;  Date      : <11 Jul 13 08:23:00 flechsig> 
;  Time-stamp: <29 Aug 13 16:25:27 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro writearray , filename=filename,  x=x, y=y

;+,
; NAME:
;  writefield 
;
;
; PURPOSE:
;   writes an array  to file, format:   z  y 
;   
;
;
; CATEGORY:
;   phase_calc 
;
;
; CALLING SEQUENCE:
;  
;  pro writearray , filename=filename,  x=x, y=y
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
;   x         : Abszisse
;   y         : Ordinate
;
; OUTPUTS:
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
;   4.9.13 RF
; 
;
;- 

u1= 'usage: writearray , filename=filename,  x=x, y=y '
usage= u1


if n_elements(filename)   eq 0 then begin print,'filename missing: '+ usage & return & endif
if n_elements(x)          eq 0 then begin print,'z        missing: '+ usage & return & endif
if n_elements(y)          eq 0 then begin print,'y        missing: '+ usage & return & endif
Nx  = n_elements(x)
Ny  = n_elements(y)


GET_LUN, f
OPENW, f, filename


for i=0, Nx-1 do begin
      PRINTF, f, x[i], ' ', y[i]
endfor

CLOSE, f
FREE_LUN,f
return
end
