
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1) komplettes phase4idl compilieren mit:  
; idl aufrufen
; "@phase4idl.compile_all.idl"

 device,decomposed=0
 loadct,39
 !QUIET =1

 @phase4idl.compile_all.idl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print,''
print,'--------------------------------------------------------------'
print,'     LTPtest
print,'--------------------------------------------------------------'

;; notwendiger Initialisierungsaufruf
; phainit


;;; allgemeine Settings
tmpfile = 'mysource'
beamlinefile = 'myltp.phase' 
cmode =3


winx = 500   ; window size x
winy = 350   ; window size y 

nz = 128     ; array size
ny = 128
waist = 0.5      ; gaussian beam waist [mm]
size = waist*10.  ; size of sourcefield [mm]
lambda = 632.8   ; wavelength [nm]

dist1 = 10       ; distance [mm] to "slit plane"
dist2 = 4500     ; distance [mm] from "slit plane" to "image plane"
nearfield1 = 1
nearfield2 = 0

sourcecheck = 1

bild_invers= 1
xr = [-10,10]  ; size [mm] contourplot
yr = [-10,10]  ; size [mm] contourplot


;bild = read_bmp('b1_face.bmp')
;bild = read_bmp('b2_slitrect.bmp')
;bild = read_bmp('b3_freestyle.bmp')
;bild = read_bmp('b4_circle.bmp')
;bild = read_bmp('b4a_circle_small.bmp')
;bild = read_bmp('b5_cross.bmp')
;bild = read_bmp('b6_dots.bmp')
;bild = read_bmp('b7_quarter.bmp')
;bild = read_bmp('b8_slits.bmp')
bild = read_bmp('b9_half.bmp')
;bild = read_bmp('b10_beam.bmp')

bild = bild /255.
IF (bild_invers EQ 1) THEN bild = bild *(-1.) +1.
bild = congrid(bild,nz,ny)

;  New Gaussian-Beam 
;                  ( nz ,  zmin ,   zmax ,  ny ,  ymin ,  ymax , waist, dist[mm] , lambda[nm])
 beam=phaSrcWFGauss( nz,-size/2, size/2  ,  ny ,-size/2, size/2, waist,        0 , lambda ,1,0,0)
 beamsource=beam

; calculation of Raleigh range
 raleigh = !DPI* waist^2/lambda * 1e6 
 print,"Raleigh range [mm] : ", raleigh
 print,"Distances [mm]     : ",dist1,dist2
 print,"Nearfield [boolean]: ",nearfield1,nearfield2

; Anzeige Gauss-Source
;IF (sourcecheck EQ 1) THEN  window,/free
;IF (sourcecheck EQ 1) THEN  phaIntensitySurface,beam,'Gauss-Source '

;  Propagate the field with the FFT-near/far-field-propagator
;                     distance/[mm]
 IF (nearfield1 EQ 1) THEN phaPropFFTnear, beam,  dist1 ELSE phaPropFFTfar, beam, dist1
 beamslit=beam
 xgrenze=beam.iezrex
 ygrenze=beam.iezrey
 

IF (sourcecheck EQ 1) THEN window,xsize=winx,ysize=winy,/free
 ;IF (sourcecheck EQ 1) THEN phaIntensitySurface,beam,'Imageplane'
 intens2 =(beamslit.zezre^2 + beamslit.zezim^2)(0:xgrenze-1,0:ygrenze-1)
 intens1 =(beamsource.zezre^2 + beamsource.zezim^2)(0:xgrenze-1,0:ygrenze-1)
 IF (sourcecheck EQ 1) THEN contour,intens1,indgen(nz),indgen(ny), $
                             /iso,nlevels=1,xst=1,yst=1,thick=5
 IF (sourcecheck EQ 1) THEN contour,intens2,indgen(nz),indgen(ny), $
                             /iso,nlevels=1,/noerase, $
                             color=253,xst=5,yst=5



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Phase in 'Spaltebene' bearbeiten

 phi=180D

; Variante 1: Haelfte des Strahles 180 Grad Phase schieben
  ; ere = beam.zezre(0:xgrenze/2-1,*)
  ; eim = beam.zezim(0:xgrenze/2-1,*)
  ; beam.zezre(0:xgrenze/2-1,*)=ere * cos(phi *!DPI/180D) - eim * sin(phi *!DPI/180D)
  ; beam.zezim(0:xgrenze/2-1,*)=ere * sin(phi *!DPI/180D) + eim * cos(phi *!DPI/180D)


; Variante 2: ein inverser Spalt 
  ; pix=5
  ; ere = beam.zezre(xgrenze/2-1-pix:xgrenze/2-1+pix,*)
  ; eim = beam.zezim(xgrenze/2-1-pix:xgrenze/2-1+pix,*)
  ; beam.zezre(xgrenze/2-1-pix:xgrenze/2-1+pix,*)= ere * cos(phi *!DPI/180D) - eim * sin(phi *!DPI/180D)
  ; beam.zezim(xgrenze/2-1-pix:xgrenze/2-1+pix,*)= ere * sin(phi *!DPI/180D) + eim * cos(phi *!DPI/180D)

; Variante 3: Bilder aufmultiplizieren 
  ; beam.zezre = beam.zezre * bild
  ; beam.zezim = beam.zezim * bild

; Variante 4: Bilderbereiche Phase schieben
  bildall = beam.zezre * 0D
  bildall(0:xgrenze-1,0:ygrenze-1) = bild
  ere = beam.zezre
  eim = beam.zezim
  ere_sh = ere * cos(phi *!DPI/180D) - eim * sin(phi *!DPI/180D)
  eim_sh = ere * sin(phi *!DPI/180D) + eim * cos(phi *!DPI/180D)
  idx = where(bildall EQ 1)
  beam.zezre(idx) = ere_sh(idx)
  beam.zezim(idx) = eim_sh(idx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;phaIntensitySurface,beam,'beschnitten'
 z=get_pha_src4_axis_z(beam)
 y=get_pha_src4_axis_y(beam)


; weiter propagieren
IF (nearfield2 EQ 1) THEN phaPropFFTnear, beam,  dist2 ELSE phaPropFFTfar, beam, dist2


;phaIntensitySurface,beam,'Imageplane '
z=get_pha_src4_axis_z(beam)
y=get_pha_src4_axis_y(beam)
intens =(beam.zezre^2 + beam.zezim^2)(0:xgrenze-1,0:ygrenze-1)


window,xsize=winx,ysize=winy,/free
data = (intens)
contour,data,z,y,/fill,nlevels=40,pos=[0.1,0.4,0.7,0.9],xst=1,yst=1,$
   xrange=xr,yrange=yr
plot,y,total(data,2),/noerase,pos =[0.1,0.1,0.7,0.3],xst=1,xrange=xr
plot,total(data,1),z,/noerase,pos =[0.8,0.4,0.95,0.9],xticks=1,yst=1,yrange=yr
print
print,"limits z,y [mm] : ",min(z),max(z),min(y),max(y)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


SavePhaseResults,beam, tmpfile

PhaseBatchMode, beamlinefile , tmpfile, cmode

LoadPhaseResults,beam, tmpfile

z=get_pha_src4_axis_z(beam)
y=get_pha_src4_axis_y(beam)
xgrenze = beam.iezrex
ygrenze = beam.iezrey

intens =(beam.zezre^2 + beam.zezim^2)(0:xgrenze-1,0:ygrenze-1)

window,xsize=winx,ysize=winy,/free
data = (intens)
contour,data,z,y,/fill,nlevels=40,pos=[0.1,0.4,0.7,0.9],xst=1,yst=1,$
   xrange=xr,yrange=yr
plot,y,total(data,2),/noerase,pos =[0.1,0.1,0.7,0.3],xst=1,xrange=xr
plot,total(data,1),z,/noerase,pos =[0.8,0.4,0.95,0.9],xticks=1,yst=1,yrange=yr
print
