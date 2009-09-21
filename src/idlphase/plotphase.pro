;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plotresult.pro
;  Date      : <21 Sep 09 12:13:39 flechsig> 
;  Time-stamp: <21 Sep 09 15:40:42 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro plotphase, filename, nofit=nofit, title=title, $
                ymin=ymin, ymax=ymax, zmin=zmin, zmax=zmax
                
;+
; NAME:
;   plotphase
;
;
; PURPOSE:
;   plot the phase as function of y,z
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;   plotphase, filename, nofit=nofit, title=title, $
;                ymin=ymin, ymax=ymax, zmin=zmin, zmax=zmaxplotresult, filename,;
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
;   nofit: don't plot fit
;   title: title (default is filename)
;   ymin, ymax, zmin, zmax: manual scaling (default is autoscaling)
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
;   U. Flechsig Sep 09
;-

beauty=0.1
nbins=50
if n_elements(filename) eq 0 then a=read_ascii(data_start=1)
if n_elements(title)    eq 0 then title=filename

a=read_ascii(filename, data_start=1)
y  = reform(a.field1[0,*])
z  = reform(a.field1[1,*])
phi= reform(a.field1[4,*])

if n_elements(ymin)   eq 0 then ymin= (1.0-beauty)*min(y)
if n_elements(ymax)   eq 0 then ymax= (1.0+beauty)*max(y)
if n_elements(zmin)   eq 0 then zmin= (1.0-beauty)*min(z)
if n_elements(zmax)   eq 0 then zmax= (1.0+beauty)*max(z)

rays= n_elements(y)*1.0

ybin=dindgen(nbins)/(nbins-1)*(ymax-ymin)+ymin
zbin=dindgen(nbins)/(nbins-1)*(zmax-zmin)+zmin

phase=dblarr(nbins,nbins)
count=make_array(nbins, nbins, /double, value=1.0)

for i=0,n_elements(y)-1 do begin
;;for i=0,1 do begin
    posy= where(ybin ge y[i], county)            ;; index in posy[0]
    posz= where(zbin ge z[i], countz)
    if ((county ne 0) and (countz ne 0)) then begin
        phase[posy[0],posz[0]]= phi[i]
        count[posy[0],posz[0]]+= 1.0
    endif
endfor
phase/= count

zone,1,1
mycontour,phase,ybin,zbin, xtitle='y (mm)', ytitle='z (mm)', ztitle='phase'

return
end
