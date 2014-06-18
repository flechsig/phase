; -*-idlwave-*-
; File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/h5_read_surf_error.pro
; Date      : <17 Jun 14 11:38:15 flechsig> 
; Time-stamp: <17 Jun 14 12:02:39 flechsig> 
; Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;
;
; $Source$ 
; $Date$
; $Revision$ 
; $Author$ 

pro h5_read_surf_error, fname, ename, u, w, l, verbose=verbose
;+
; NAME:
;   h5_read_surf_error
;
; PURPOSE:
;   read phase surface error file, units m, u can be 1d or 2d data
;
; CATEGORY:
;   phase_h5
;
; CALLING SEQUENCE:
;   h5_read_surf_error, fname, ename, u, w, l [,/verbose]
;
; INPUTS:
;   fname: filename
;   ename: elementname
;   u:     height u(w,l)
;   w:     w
;   l:     l
;
; KEYWORD PARAMETERS:
;   verbose
;
; OUTPUTS:
;  no
;
; EXAMPLE:
; idl> h5_read_surf_error, file, ename, u, w, l
;
; MODIFICATION HISTORY:
;    25.6.14 UF
;-

print, 'h5_read_surf_error called'

usage= 'usage: h5_read_surf_error, fname, ename, u, w, l'

if n_params() ne 5 then begin
    print, usage
    return
endif

file_id   = H5F_OPEN(fname)
height2D  = h5_read_dataset(file_id, ename+'/height2D')
height_vec= h5_read_dataset(file_id, ename+'/height_vec')
wvec      = h5_read_dataset(file_id, ename+'/wvec')
lvec      = h5_read_dataset(file_id, ename+'/lvec')
h5f_close, file_id

help, height2D, height_vec, wvec, lvec 
u= height2D
w= wvec
l=lvec
print, 'read surface error file: ', fname

return
end
;; end
