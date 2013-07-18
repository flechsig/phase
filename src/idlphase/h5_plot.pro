;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <18 Jul 13 16:17:17 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro h5_plot, fname  
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

h5_read, fname, zamp=zamp, z_vec=z_vec, y_vec=y_vec
mycontour, zamp, z_vec, y_vec, xtitle='z (mm)', ytitle='y (mm)', title='Ez intensity'

stat= gauss2dfit(zamp, z_vec, y_vec)
print, 'result of gauss2dfit:', stat
print, 'z fwhm=',stat[2]
print, 'y fwhm=',stat[3]
print, 'z0    =',stat[4]
print, 'y0    =',stat[5]
return
end
;; end
