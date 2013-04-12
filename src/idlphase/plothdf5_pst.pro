;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <11 Apr 13 17:14:31 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro plothdf5_pst, fname, png=png, surface=surface, shade_surf=shade_surf, cut=cut
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
;   shade_surf: plot style (default contour)
;   surface   : plot style (default contour)
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

if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/aramis12_0.1nm_po_out.h5'
;;if n_elements(surface) eq 0 then style=0 else style=1 

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

help, field0, y_vec, z_vec
field1= reform(field0, n_elements(z_vec), n_elements(y_vec))
help, field0, field1, y_vec, z_vec
;print,field1

if keyword_set(surface) then begin 
    surface,field1, z_vec, y_vec, title='intensity', xtitle='z (mm)', ytitle='y (mm)' 
endif else begin
    if keyword_set(shade_surf) then begin
        shade_surf, field1, z_vec, y_vec, title='intensity', xtitle='z (mm)', ytitle='y (mm)'
    endif else begin
        mycontour,field1, z_vec, y_vec, title='intensity', xtitle='z (mm)', ytitle='y (mm)'
    endelse
endelse

if keyword_set(cut) then begin
    size0= size(field1)
    size1= size0[1]/2
    zpro0= field1[*,size1]
    zprof= zpro0/max(zpro0)
    plot, z_vec, zprof, xtitle='z (mm)', ytitle='intensity', title='normalized horizontal profile at the center'
    zfit= gaussfit(z_vec, zprof, fit, nterms=4)
    help, zrof, fit
    sigma= fit[2]
    print, 'FWHM= ',2.35*sigma
    print, 'fit: ', fit
 oplot, z_vec, zfit, color=2 
endif


if keyword_set(png) then spng,'pst.png'

return
end
;; end
