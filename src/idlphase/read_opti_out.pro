;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/read_opti_out.pro
;  Date      : <04 Jan 08 08:22:27 flechsig> 
;  Time-stamp: <04 Jan 08 08:22:34 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro read_opti_out, fname
;+
; NAME:
;
;
;
; PURPOSE:
;
;
;
; CATEGORY:
;
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
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
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
;
;-

a=read_ascii(fname, DATA_START=1)
x=reform(a.field1[0,*])
y=reform(a.field1[1,*])
dy=reform(a.field1[2,*])
dz=reform(a.field1[3,*])
ry=reform(a.field1[4,*])
rz=reform(a.field1[5,*])
tr=reform(a.field1[6,*])

zone,2,2
plot, y, dy, /nodata
oplot, y, dy, color=1
plot, y, dz,/nodata
oplot, y, dz, color=2
plot, y, dz+dy,/nodata
oplot, y, dz+dy, color=3
plot, y, tr,/nodata
oplot, y, tr, color=4
return
end
