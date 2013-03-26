;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <26 Mar 13 08:44:30 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro plothdf5_genesis_source, fname, png=png, limit=limit
;+
; NAME:
;   plothdf5_genesis_source
;
;
; PURPOSE:
;   plot a hdf5 file of type genesis_hdf5, plot the source 
;
;
; CATEGORY:
;   phase_plot
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;   fname: filename
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;   limit: limit the number of plots to limit
;   png:   save png files
;
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
;    25.3.13 UF
;-

if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.h5'
if n_elements(limit) eq 0 then limit= 1

file_id     = H5F_OPEN(fname)
dataset_id1 = H5D_OPEN(file_id, 'slice000001/field')
dataset_id2 = H5D_OPEN(file_id, 'gridsize')
field0      = H5D_READ(dataset_id1)
gridsize    = H5D_READ(dataset_id2)

h5d_close, dataset_id1
h5d_close, dataset_id2
h5f_close, file_id

len   = n_elements(field0)/2
size  = fix(sqrt(len))
field2= reform(field0, 2, size, size)

print, 'size= ', size, ' gridsize= ', gridsize

real= reform(field2[0,*,*], size, size)
imag= reform(field2[1,*,*], size, size)

amp  = sqrt(real^2+imag^2)
phase= atan(imag,real)

x0= dindgen(size)- size/2
x = x0* gridsize[0]* 1e3
y = x * 1.0

window,0
mycontour,real, x, y, title='real', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'genesis-real.png'
if limit lt 2 then return

window,1
mycontour,imag,x,y,title='imag', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'genesis-imag.png'
if limit lt 3 then return

window,2
mycontour,amp, x, y, title='amplitude', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'genesis-ampl.png'
if limit lt 4 then return

window,3
mycontour,phase, x, y, title='phase', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'genesis-phas.png'

return
end
;; end
