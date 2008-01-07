;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/read_opti_out.pro
;  Date      : <04 Jan 08 08:22:27 flechsig> 
;  Time-stamp: <04 Jan 08 16:14:05 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro read_extr_out, fname, array, column=column, all=all, grid=grid, noplot=noplot 
;+
; NAME:
;   read_extr_out
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
OpenR, lun, fname, /get_lun

data_start=6

header= strarr(data_start)
readf, lun, header
print, 'header: '
print, header

nx  = 0
ny  = 0
cols= 0
readf, lun, nx, ny, cols
if n_elements(cols) eq 0 then cols=3
rows= nx * ny
data= fltarr(cols, nx, ny)
readf, lun, data
Free_Lun, lun

x=reform(data[0,*,0])
y=reform(data[1,0,*])
parr= data[2:*,*,*]

if n_elements(column) eq 0 then begin 
    column=1
    array=parr
endif else array=reform(parr[column-1,*,*])

if n_elements(noplot) eq 0 then begin  
    print, 'plot'
    arr= reform(parr[column-1,*,*])
    if n_elements(grid) ne 0 then begin
        iy= fix(sqrt(cols-2)) 
        ix=(cols-2)/iy
        if (iy * ix lt (cols-2)) then ix= ix+ 1
        zone,ix,iy
    endif
    
    if n_elements(all) ne 0 then begin
        for i=0, cols-3 do begin
            arr= reform(parr[i,*,*])
            ztitle='column '+ string(i+1)
            mycontour,arr,x,y,xtitle='x',ytitle='y',ztitle=ztitle,title=ztitle,/nocolorbar
        endfor
    endif else begin
        ztitle='column '+ string(column)
        mycontour,arr,x,y,xtitle='x',ytitle='y',ztitle=ztitle,title=ztitle
    endelse
    
endif ;; noplot
return
end
