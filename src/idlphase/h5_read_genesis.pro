;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <05 Feb 14 14:51:41 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro h5_read_genesis, fname, comp=comp, real=real, imag=imag, $
                     phase=phase, amp=amp, z_vec=z_vec, y_vec=y_vec, wavelength=wavelength, beam=beam, verbose=verbose
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
size  = fix(sqrt(len))
size2 = size*size
print, 'read GENESIS file -- units: (m)'
print, 'size       = ', size, ' gridsize= ', gridsize
print, 'len        = ',  len, ' size^2  = ', size2
print, 'wavelength = ',  wavelength

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
z_vec = x0* gridsize[0]
y_vec = z_vec * 1.0

if KEYWORD_SET(beam) ne 0 then begin
    print, 'fill beam structure'
    beam={source4}  
    beam.iezrex   = long(NZ)
    beam.iezrey   = long(NY)
    beam.ieyrex   = long(NZ)
    beam.ieyrey   = long(NY)
    beam.iezimx   = long(NZ)
    beam.iezimy   = long(NY)
    beam.ieyimx   = long(NZ)
    beam.ieyimy   = long(NY)

    beam.zezre(0:NY-1, 0:NZ-1)= zreal
    beam.zeyre(0:NY-1, 0:NZ-1)= yreal
    beam.zezim(0:NY-1, 0:NZ-1)= zimag
    beam.zeyim(0:NY-1, 0:NZ-1)= yimag

    beam.xezremin= min(z_vec)
    beam.xezremax= max(z_vec)
    beam.yezremin= min(y_vec)
    beam.yezremax= max(y_vec)

    beam.xlam= wavelength
endif

return
end
;; end
