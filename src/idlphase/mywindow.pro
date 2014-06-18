 ; File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/mywindow.pro
 ; Date      : <18 Jun 14 08:35:17 flechsig> 
 ; Time-stamp: <18 Jun 14 09:01:28 flechsig> 
 ; Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 ; $Source$ 
 ; $Date$
 ; $Revision$ 
 ; $Author$ 

pro mywindow, row, col, rows=rows, cols=cols
;+
; NAME:
;   mywindow
;
; PURPOSE:
;   position a window in row and col on the screen
;
; CATEGORY:
;   generic
;
; CALLING SEQUENCE:
;   mywindow, row, col [,rows=rows] [,cols=cols]
;
; INPUTS:
;   row: the row starting from 1
;   col: the column starting from 1
;
; OPTIONAL INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   rows: number of rows-    default= 4
;   cols: number of columns- default= 4
;
; OUTPUTS:
;   no
;
; EXAMPLE:
;   idl> mywindow, 1, 3
;
; MODIFICATION HISTORY:
;   UF Jun 2014
;-

if n_elements(rows) eq 0 then rows= 4
if n_elements(cols) eq 0 then cols= 4

;--------- Get Screen size in pixel -----------
device, Get_Screen_size=size
screenX= size[0]
ScreenY= size[1]
borderx= 5       ;; experimentally determinded
bordery= 30      ;; experimentally determinded
Wx= (ScreenX/ rows)- borderx
Wy= (ScreenY/ cols)- bordery

wnumber= col+ row* cols
window, wnumber, XSIZE=wx, YSIZE=wy, XPOS= wx*(col-1), YPOS= screeny- wy* row
return
end
