;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plot2d.pro
;  Date      : <21 Sep 09 17:42:58 flechsig> 
;  Time-stamp: <21 Sep 09 17:47:50 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro plot2d, filename
;+
; NAME:
;   plot2d
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

if n_elements(filename) eq 0 then filename=file_search()

;;filename='test2d.out'
datalines=file_lines(filename)-1
rows=0
cols=0
arr=dblarr(3,datalines)
x=dblarr(datalines)
y=dblarr(datalines)
z=dblarr(datalines)

openr, lun, filename, /get_lun
readf, lun, rows, cols, format='(%"%d %d")'
readf, lun, arr, format='(%"%f %f %f")'
close, lun


x0=reform(arr[0,*], rows, cols)
y0=reform(arr[1,*], rows, cols)
z=reform(arr[2,*], rows, cols)
x=reform(x0[*,0])
y=reform(y0[0,*])

mycontour,z, x, y, xtitle='z (mm)', ytitle='y (mm)', ztitle='intensity'

return
end
