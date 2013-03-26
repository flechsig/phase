;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <26 Mar 13 09:03:04 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro plothdf5_phase_source, fname, png=png, limit=limit
;+
; NAME:
;   plothdf5_genesis_source
;
;
; PURPOSE:
;   plot a hdf5 file of type phase_hdf5, plot the source 
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

if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/EZRE_GB_5000.h5'                   
if n_elements(limit) eq 0 then limit= 100

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

field1= reform(field,nz,ny,4,nt)
field2= reform(field1[*,*,*,0],nz,ny,4)

yreal= reform(field2[*,*,0], nz, ny)
yimag= reform(field2[*,*,1], nz, ny) 
zreal= reform(field2[*,*,2], nz, ny)
zimag= reform(field2[*,*,3], nz, ny)
help,field,field1,field2,yreal

yamp  = sqrt(yreal^2+yimag^2)
yphase= atan(yimag,yreal)
zamp  = sqrt(zreal^2+zimag^2)
zphase= atan(zimag,zreal)

window,0
mycontour,yreal, z_vec, y_vec, title='y_real', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-yreal.png'
if limit eq 1 then return

;return
window,1
mycontour,yimag,z_vec,y_vec,title='y_imag', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-yimag.png'
if limit eq 2 then return

window,2
mycontour,yamp, z_vec, y_vec, title='y_amplitude', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-yampl.png'
if limit eq 3 then return

window,3
mycontour,yphase, z_vec, y_vec, title='y_phase', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-yphas.png'
if limit eq 4 then return

window,4
mycontour,zreal, z_vec, y_vec, title='z_real', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-zreal.png'
if limit eq 5 then return

window,5
mycontour,zimag,z_vec,y_vec,title='z_imag', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-zimag.png'
if limit eq 6 then return

window,6
mycontour,zamp, z_vec, y_vec, title='z_amplitude', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-zampl.png'
if limit eq 7 then return

window,7
mycontour,zphase, z_vec, y_vec, title='z_phase', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'phase-zphas.png'

return
end
;; end
