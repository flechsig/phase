;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plotdivergence.pro
;  Date      : <21 Sep 09 14:27:26 flechsig> 
;  Time-stamp: <21 Sep 09 14:57:44 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 


pro plotdivergence, filename, nofit=nofit, title=title, $
                    dymin=dymin, dymax=dymax, dzmin=dzmin, dzmax=dzmax
;+
; NAME:
;   plotdivergence
;
;
; PURPOSE:
;   plot the divergence from a file of rays 
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;   plotdivergence, filename, nofit=nofit, title=title, ymin=ymin,
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

if n_elements(filename) eq 0 then a=read_ascii(data_start=1)
if n_elements(title)    eq 0 then title=filename

a=read_ascii(filename, data_start=1)

dy = reform(a.field1[2,*])*1e3
dz = reform(a.field1[3,*])*1e3

if n_elements(dymin)  eq 0 then dymin= (1.0-beauty)*min(dy)
if n_elements(dymax)  eq 0 then dymax= (1.0+beauty)*max(dy)
if n_elements(dzmin)  eq 0 then dzmin= (1.0-beauty)*min(dz)
if n_elements(dzmax)  eq 0 then dzmax= (1.0+beauty)*max(dz)

rays= n_elements(y)*1.0

dyhis=histogram(dy,min=dymin,max=dymax,nbins=nbins)
dzhis=histogram(dz,min=dzmin,max=dzmax,nbins=nbins)
dybin=dindgen(nbins)/(nbins-1)*(dymax-dymin)+dymin
dzbin=dindgen(nbins)/(nbins-1)*(dzmax-dzmin)+dzmin
dyfit=gaussfit(dybin,dyhis,dystat)
dzfit=gaussfit(dzbin,dzhis,dzstat)

;;zone,1,2
!P.MULTI = [0, 2, 2]
!P.region=[0.0,0.3,0.7,1.0]
;;!P.position=[0.0,0.3,0.7,1.0]
plot, dy, dz, xrange=[dymin,dymax], yrange=[dzmin,dzmax], /nodata, $
  xtitle='dy (mrad)', ytitle='dz (mrad)', title=title
oplot,dy,dz, psym=8, color=1, SYMSIZE=0.5

!P.region=[0.7,0.3,1.0,1.0]
plot,dzhis,dzbin,/nodata,xtitle='dz density',ytitle='dz (mrad)'
oplot,dzhis,dzbin,color=1
if n_elements(nofit) eq 0 then oplot,dzfit,dzbin,color=3

!P.region=[0.0,0.0,0.7,0.3]
plot,dybin,dyhis,/nodata,xtitle='dy (mrad)',ytitle='dy density'
oplot,dybin,dyhis,color=1
if n_elements(nofit) eq 0 then oplot,dybin,dyfit,color=3

s1= string(n_elements(dy),  format='(%"rays:      %7d")')
s2= string(dystat[1],       format='(%"dy center: %7.4f mrad")')
s3= string(dystat[2]* 2.35, format='(%"dy FWHM:   %7.4f mrad")')
s4= string(dzstat[1],       format='(%"dz center: %7.4f mrad")')
s5= string(dzstat[2]* 2.35, format='(%"dz FWHM:   %7.4f mrad")')

;; plot results
xyouts, 0.8, 0.25, s1, /normal
xyouts, 0.8, 0.22, s2, /normal
xyouts, 0.8, 0.19, s3, /normal
xyouts, 0.8, 0.16, s4, /normal
xyouts, 0.8, 0.13, s5, /normal

return
end
