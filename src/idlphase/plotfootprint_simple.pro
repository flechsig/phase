;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plotfootprint.pro
;  Date      : <21 Sep 09 14:22:52 flechsig> 
;  Time-stamp: <12 Aug 11 15:04:54 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 


pro plotfootprint_simple, filename, title=title, $
                   ymin=ymin, ymax=ymax, zmin=zmin, zmax=zmax
                
;+
; NAME:
;   plotfootprint
;
;
; PURPOSE:
;   plot a file of rays as footprint data
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;   plotresult, filename, nofit=nofit, title=title, ymin=ymin,
;   ymax=ymax, zmin=zmin, zmax=zmax
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
nbins=100

if n_elements(filename) eq 0 then begin
    a=read_ascii(data_start=1) 
    filename=''
end else a=read_ascii(filename, data_start=1)
if n_elements(title)    eq 0 then title=filename

;;a=read_ascii(filename, data_start=1)
y  = reform(a.field1[0,*])*1e3
z  = reform(a.field1[1,*])

;; stimmt nur bei min < 0
if n_elements(ymin)   eq 0 then ymin= (1.0+beauty)*min(y)
if n_elements(ymax)   eq 0 then ymax= (1.0+beauty)*max(y)
if n_elements(zmin)   eq 0 then zmin= (1.0+beauty)*min(z)
if n_elements(zmax)   eq 0 then zmax= (1.0+beauty)*max(z)

!p.font=-1
!p.charsize=2.3
!p.charthick=2.3


plot, z, y, xrange=[zmin,zmax], yrange=[ymin,ymax], /nodata, $
  xtitle='z (mm)', ytitle=textoidl('y (\mum)'), title=''
oplot,z, y, psym=8, color=1, SYMSIZE=0.5







return
end
