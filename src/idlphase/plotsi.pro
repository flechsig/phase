;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plotsimp.pro
;  Date      : <10 May 12 07:49:52 flechsig> 
;  Time-stamp: <10 May 12 08:18:52 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 



pro plotsi, filename, title=title
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
;  title: a title, default is filename
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
;   idl> plotsi, 'test.out-simpre'
;
;
; MODIFICATION HISTORY:
;   U. Flechsig May 12
;-

if n_elements(filename) eq 0 then begin
    a=read_ascii(COMMENT_SYMBOL='#') 
    filename=''
end else a=read_ascii(filename, COMMENT_SYMBOL='#')

if n_elements(title) eq 0 then title=filename

zone,2,2
a1  = reform(a.field1[0,*])
a2  = reform(a.field1[1,*])
a3  = reform(a.field1[2,*])
a4  = reform(a.field1[3,*])
a5  = reform(a.field1[4,*])
a6  = reform(a.field1[5,*])

plot,a1,a2,title=title,xtitle='dy (mrad)',ytitle= 'dens. @ dzmin'
plot,a1,a3,title=title,xtitle='dy (mrad)',ytitle= 'dens. @ dzcenter'
plot,a1,a4,title=title,xtitle='dy (mrad)',ytitle= 'dens. @ dzmax'
plot,a5,a6,title=title,xtitle='dz (mrad)',ytitle= 'dens. @ dycenter'

return
end
