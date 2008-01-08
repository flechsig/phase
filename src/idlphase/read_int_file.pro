;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/read_opti_out.pro
;  Date      : <04 Jan 08 08:22:27 flechsig> 
;  Time-stamp: <08 Jan 08 13:24:43 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro read_int_file, fname, arr, column=column, all=all, grid=grid, noplot=noplot, ny=ny 
;+
; NAME:
;   read_int_file
;
;
; PURPOSE:
;   read out an output file from phaseopti 
;
;
; CATEGORY:
;   phase
;
;
; CALLING SEQUENCE:
;   read_extr_out, fname
;
;
; INPUTS:
;   filename
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;   /all      : plot all columns
;   column: the column to plot, default is 1
;   /grid     : automatic zone
;   /noplot
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;   output array
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
;   UF Jan 08
;-

linarr=dblarr(ny*8)
OpenR, lun, fname, /get_lun

readf, lun, linarr

Free_Lun, lun

help,linarr
arr=reform(linarr,8,ny)
;;if n_elements(noplot) eq 0 then begin  
zone,2,2
    plot,arr[0,*],arr[1,*],title='at min(z)',xtitle='y (mm)'
    plot,arr[2,*],arr[3,*],title='at center(z)',xtitle='y (mm)'
    plot,arr[4,*],arr[5,*],title='at max(z)',xtitle='y (mm)'
    plot,arr[6,*],arr[7,*],title='y integral',xtitle='z (mm)'
;;endif ;; noplot
return
end
