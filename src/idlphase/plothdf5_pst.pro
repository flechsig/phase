;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <2013-07-16 21:21:56 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro plothdf5_pst, fname, png=png, surface=surface, shade_surf=shade_surf, cut=cut, $
                  norm=norm, arr=arr, xvec=xvec, yvec=yvec, _extra=extra
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
arr=field1
xvec=z_vec
yvec=y_vec

help, field0, field1, y_vec, z_vec, arr
;print,field1

if keyword_set(norm) then field1= field1/max(field1)

if keyword_set(surface) then begin 
    surface, field1, z_vec, y_vec, title='intensity', xtitle='z (mm)', ytitle='y (mm)', $
             charsize=3, _extra=extra
endif else begin
    if keyword_set(shade_surf) then begin
        shade_surf, field1, z_vec, y_vec, title='intensity', xtitle='z (mm)', $
                    ytitle='y (mm)', _extra=extra
    endif else begin
        mycontour,field1, z_vec, y_vec, title='intensity', xtitle='z (mm)', ytitle='y (mm)', _extra=extra
    endelse
endelse

if keyword_set(cut) then begin
    size0= size(field1)
    ycenteridx = size0[1]/2
    zcenteridx = size0[2]/2
    zpro0= field1[*, ycenteridx]
    ypro0= field1[zcenteridx, *]
    zprof= zpro0/max(zpro0)
    yprof= ypro0/max(ypro0)
    help, y_vec, ycenteridx
    min=min([min(z_vec),min(y_vec)])
    max=max([max(z_vec),max(y_vec)])
;help, y_vec
;print,'start plot'
    plot, [min, max], [0,1.1], xtitle='pos (mm)', ytitle='intensity', $
          title='normalized profiles at the center',/nodata, _extra=extra
    oplot, z_vec, zprof, color=1, thick=2
    oplot, y_vec, yprof, color=2, thick=2
;help, y_vec
;print,'start fit'
    zfit= gaussfit(z_vec, zprof, fitz, nterms=4)
    yfit= gaussfit(y_vec, yprof, fity, nterms=4)
;    help, zrof, fitz
    sigmaz= fitz[2]
    sigmay= fity[2]
    print, 'z FWHM= ', 2.35*sigmaz*1e3,' mum'
    print, 'z fit: ', fitz
    print, 'y FWHM= ', 2.35*sigmay*1e3,' mum'
    print, 'y fit: ', fity

    oplot, z_vec, zfit, color=3, thick=2 
    oplot, y_vec, yfit, color=4, thick=2 
    legend, ['hor cut','vert cut','hor fit','vert fit'], color=[1,2,3,4], $
            linestyle=[0,0,0,0], thick=[2,2,2,2]
endif

if keyword_set(png) then spng, 'pst.png'

return
end
;; end
