;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <18 Jul 13 16:09:36 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro h5_plot, fname,  
;+
; NAME:
;   h5_plot
;
;
; PURPOSE:
;   plot the intensity in a genesis or phase hdf5 file plus some statistics
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
;   idl> h5_read,'abc.h5', amp=a, z=z, y=y
;   idl> mycontour, a ,z, y
;
; MODIFICATION HISTORY:
;    25.3.13 UF
;-

if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.h5'

if h5_test(fname) eq 0 then begin
    print, 'file >>',fname,'<< is not a hdf5- exit'
    return
endif

h5type= h5_check_type(fname, verbose=verbose)

if (n_elements(verbose) ne 0) then print, 'h5_read: h5type=', h5type

case h5type of
    7: h5_read_phase, fname, zcomp=zcomp, zreal=zreal, zimag=zimag, zphase=zphase, zamp=zamp, $
      ycomp=ycomp, yreal=yreal, yimag=yimag, yphase=yphase, yamp=yamp, $
      z_vec=z_vec, y_vec=y_vec, wavelength=wavelength, verbose=verbose 
    8: h5_read_genesis, fname, comp=zcomp,  real=zreal, imag=zimag, phase=zphase, amp=zamp, $
      z_vec=z_vec, y_vec=y_vec, wavelength=wavelength, verbose=verbose
    else: print, 'no valid hdf5 file'
endcase

print, 'h5_read done'

return
end
;; end
