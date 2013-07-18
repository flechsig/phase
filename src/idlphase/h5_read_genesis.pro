;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <18 Jul 13 09:57:58 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro h5_read_genesis, fname, comp=comp, real=real, imag=imag, $
                     phase=phase, amp=amp, z=z, y=y, wavelength=wavelength, verbose=verbose
;+
; NAME:
;   h5_read_genesis
;
;
; PURPOSE:
;   read genesis source
;
;
; CATEGORY:
;   hdf5
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

file_id = H5F_OPEN(fname)
field0  = h5_read_dataset(file_id, 'slice000001/field')
gridsize= h5_read_dataset(file_id, 'gridsize')
wavelength= h5_read_dataset(file_id, 'wavelength')
h5f_close, file_id

len   = n_elements(field0)/2
size  = fix(sqrt(len))
size2 = size*size
print, 'size      = ', size, ' gridsize= ', gridsize
print, 'len       = ',  len, ' size^2  = ', size2
print, 'wavelenth = ',  wavelength

if (size2 ne len) then begin
    print, 'genesis works assumes a quadratic grid- return'
    return
endif

field2= reform(field0, 2, size, size)

real= reform(field2[0,*,*], size, size)
imag= reform(field2[1,*,*], size, size)

amp  = sqrt(real^2 + imag^2)
phase= atan(imag, real)

comp= dcomplexarr(size, size)
comp= complex(real, imag, /double)

x0= dindgen(size)- size/2
z = x0* gridsize[0]
y = z * 1.0

return
end
;; end
