;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <08 Jan 15 12:23:50 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro h5_read_genesis, fname, comp=comp, real=real, imag=imag, $
                     phase=phase, amp=amp, z_vec=z_vec, y_vec=y_vec, $
                     wavelength=wavelength, verbose=verbose
;+
; NAME:
;   h5_read_genesis
;
;
; PURPOSE:
;   read genesis source, Genesis calculates an EM field on a centered,
;   equidistant, quadratic grid, the output is one field - no polarization 
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
;   amp:        amplitude (2d)
;   beam:       beam structure (source4)
;   comp:       complex field (2d)
;   imag:       imaginary part (2d)
;   phase:      phase: (2d)
;   real:       real part (2d)
;   verbose:    verbose
;   wavelength: wavelength
;   y:          vertical vector
;   z:          horizontal vector 
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
;   idl> h5_read_genesis,'abc.h5', amp=a, z=z, y=y
;   idl> mycontour, a ,z, y
;
; MODIFICATION HISTORY:
;    25.3.13 UF
;     8.1.15 UF correct normalization, remove beam
;-

if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.h5'

file_id   = H5F_OPEN(fname)
field0    = h5_read_dataset(file_id, 'slice000001/field')
gridsize  = h5_read_dataset(file_id, 'gridsize')
lambda    = h5_read_dataset(file_id, 'wavelength')
h5f_close, file_id

wavelength= lambda[0] ;; wavelength should be a scalar
;; help, wavelength, lambda

len   = n_elements(field0)/2
size  = long(fix(sqrt(len)))
size2 = long(size*size)
print, 'read GENESIS file -- units: (m)'
print, 'size       = ', size, ' gridsize= ', gridsize
print, 'len        = ',  len, ' size^2  = ', size2
print, 'wavelength = ',  wavelength

if (size2 ne len) then begin
    print, 'error: GENESIS  assumes a quadratic grid- return'
    return
endif

;; normalization
eev= 511000   ;; electronen ruhemasse in eV
k  = 2.0*!dpi/ wavelength

field2= reform(field0, 2, size, size)* eev/k  ;; scaled field

real= reform(field2[0,*,*], size, size)
imag= reform(field2[1,*,*], size, size)

amp  = sqrt(real^2 + imag^2)
phase= atan(imag, real)

comp= dcomplexarr(size, size)
comp= complex(real, imag, /double)

x0= dindgen(size)- size/2
z_vec = x0* gridsize[0]
y_vec = z_vec * 1.0

return
end
;; end
