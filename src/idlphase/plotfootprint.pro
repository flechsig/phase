;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plotfootprint.pro
;  Date      : <21 Sep 09 14:22:52 flechsig> 
;  Time-stamp: <22 Dec 09 14:19:50 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 


pro plotfootprint, filename, nofit=nofit, title=title, $
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
y  = reform(a.field1[0,*])
z  = reform(a.field1[1,*])

if n_elements(ymin)   eq 0 then ymin= (1.0-beauty)*min(y)
if n_elements(ymax)   eq 0 then ymax= (1.0+beauty)*max(y)
if n_elements(zmin)   eq 0 then zmin= (1.0-beauty)*min(z)
if n_elements(zmax)   eq 0 then zmax= (1.0+beauty)*max(z)

rays= n_elements(y)* 1.0

yhis=histogram(y,min=ymin,max=ymax,nbins=nbins)
zhis=histogram(z,min=zmin,max=zmax,nbins=nbins)
ybin=dindgen(nbins)/(nbins-1)*(ymax-ymin)+ymin
zbin=dindgen(nbins)/(nbins-1)*(zmax-zmin)+zmin
yfit=gaussfit(ybin,yhis,ystat)
zfit=gaussfit(zbin,zhis,zstat)

!P.MULTI = [0, 2, 2]
!P.region=[0.0,0.3,0.7,1.0]
;;!P.position=[0.0,0.3,0.7,1.0]
plot, z, y, xrange=[zmin,zmax], yrange=[ymin,ymax], /nodata, $
  xtitle='z (mm)', ytitle='y (mm)', title=title
oplot,z, y, psym=8, color=1, SYMSIZE=0.5

!P.region=[0.7,0.3,1.0,1.0]
plot, yhis, ybin,/nodata, ytitle='y (mm)', xtitle='y density'
oplot,yhis, ybin,color=1
if n_elements(nofit) eq 0 then oplot, yfit, ybin,color=3

!P.region=[0.0,0.0,0.7,0.3]
plot, zbin, zhis, /nodata, ytitle='z density', xtitle='z (mm)'
oplot, zbin, zhis, color=1
if n_elements(nofit) eq 0 then oplot, zbin, zfit, color=3

s1= string(n_elements(y),  format='(%"rays:     %7d")')
s2= string(ystat[1],       format='(%"y center: %7.4f mm")')
s3= string(ystat[2]* 2.35, format='(%"y FWHM:   %7.4f mm")')
s4= string(zstat[1],       format='(%"z center: %7.4f mm")')
s5= string(zstat[2]* 2.35, format='(%"z FWHM:   %7.4f mm")')

;; plot results
xyouts, 0.8, 0.25, s1, /normal
xyouts, 0.8, 0.22, s2, /normal
xyouts, 0.8, 0.19, s3, /normal
xyouts, 0.8, 0.16, s4, /normal
xyouts, 0.8, 0.13, s5, /normal

return
end
