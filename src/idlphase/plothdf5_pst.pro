;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <25 Mar 13 12:59:00 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro plothdf5_pst, fname, png=png
;+
; NAME:
;   plothdf5_pst
;
;
; PURPOSE:
;   plot the psd in a hdf5 
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

if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.h5'

file_id     = H5F_OPEN(fname)
dataset_id1 = H5D_OPEN(file_id, '/phase_psd/z')
dataset_id2 = H5D_OPEN(file_id, '/phase_psd/y')
dataset_id3 = H5D_OPEN(file_id, '/phase_psd/psd')
z_vec       = H5D_READ(dataset_id1)
y_vec       = H5D_READ(dataset_id2)
field0      = H5D_READ(dataset_id3)

h5d_close, dataset_id1
h5d_close, dataset_id2
h5d_close, dataset_id3
h5f_close, file_id

field1= reform(field0, n_elements(z_vec), n_elements(y_vec))

mycontour,field1, z_vec, y_vec, title='intensity', xtitle='z (mm)', ytitle='y (mm)'
if keyword_set(png) then spng,'pst.png'

return
end
;; end
