;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plotsimp.pro
;  Date      : <10 May 12 07:49:52 flechsig> 
;  Time-stamp: <10 May 12 07:53:51 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro plotsi, filename
;+
; NAME:
;   plotsi
;
;
; PURPOSE:
;   plot density cuts, the filename can be a parameter or
;   interactively selected. 
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;   plotsi, filename
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
;   U. Flechsig May 12
;-

beauty=0.1
nbins=100

if n_elements(filename) eq 0 then begin
    a=read_ascii(COMMENT_SYMBOL='#') 
    filename=''
end else a=read_ascii(filename, COMMENT_SYMBOL='#')
if n_elements(title) eq 0 then title=filename

;;a=read_ascii(filename, data_start=1)
zone,2,2
a1  = reform(a.field1[0,*])
a2  = reform(a.field1[1,*])
a3  = reform(a.field1[2,*])
a4  = reform(a.field1[3,*])
a5  = reform(a.field1[4,*])
a6  = reform(a.field1[5,*])

plot,a1,a2,title=filename,xtitle='dy (mrad)',ytitle= 'dens. @ dzmin'
plot,a1,a3,title=filename,xtitle='dy (mrad)',ytitle= 'dens. @ dzcenter'
plot,a1,a4,title=filename,xtitle='dy (mrad)',ytitle= 'dens. @ dzmax'
plot,a5,a6,title=filename,xtitle='dz (mrad)',ytitle= 'dens. @ dycenter'

return
end
