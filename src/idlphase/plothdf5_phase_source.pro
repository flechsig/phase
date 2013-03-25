;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <25 Mar 13 13:47:26 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro plothdf5_phase_source, fname, png=png
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
;   png: save png files
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

if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/test_5000_out.h5'                   

file_id     = H5F_OPEN(fname)
dataset_id1 = H5D_OPEN(file_id, '/z_vec')
dataset_id2 = H5D_OPEN(file_id, '/y_vec')
dataset_id3 = H5D_OPEN(file_id, '/t_vec')
dataset_id4 = H5D_OPEN(file_id, '/e_field')
z_vec       = H5D_READ(dataset_id1)
y_vec       = H5D_READ(dataset_id2)
t_vec       = H5D_READ(dataset_id3)
field       = H5D_READ(dataset_id4)

h5d_close, dataset_id1
h5d_close, dataset_id2
h5d_close, dataset_id3
h5d_close, dataset_id4
h5f_close, file_id

nz   = n_elements(z_vec)
ny   = n_elements(y_vec)
nt   = n_elements(t_vec)
field1= reform(field,nt,4,nz,ny)

yreal= reform(field1[0,0,*,*], nz, ny)
yimag= reform(field1[0,1,*,*], nz, ny) 
zreal= reform(field1[0,2,*,*], nz, ny)
zimag= reform(field1[0,3,*,*], nz, ny)

yamp  = sqrt(yreal^2+yimag^2)
yphase= atan(yimag,yreal)
zamp  = sqrt(zreal^2+zimag^2)
zphase= atan(zimag,zreal)

window,0
mycontour,yreal, z_vec, y_vec, title='yreal', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-yreal.png'

window,1
mycontour,yimag,z_vec,y_vec,title='yimag', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-yimag.png'

window,2
mycontour,yamp, z_vec, y_vec, title='yamplitude', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-yampl.png'

window,3
mycontour,yphase, z_vec, y_vec, title='yphase', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-yphas.png'

window,4
mycontour,zreal, z_vec, y_vec, title='zreal', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-zreal.png'

window,5
mycontour,zimag,z_vec,y_vec,title='zimag', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-zimag.png'

window,6
mycontour,zamp, z_vec, y_vec, title='zamplitude', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-zampl.png'

window,7
mycontour,zphase, z_vec, y_vec, title='zphase', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-zphas.png'

return
end
;; end
