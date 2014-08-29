;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <29 Aug 14 10:17:09 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

; ******************************************************************************
;
;   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
;                      Paul Scherrer Institut Villigen, Switzerland
;   
;   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
;          Uwe Flechsig,    uwe.flechsig@psi.ch
;
; ------------------------------------------------------------------------------
;
;   This file is part of PHASE.
;
;   PHASE is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, version 3 of the License, or
;   (at your option) any later version.
;
;   PHASE is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with PHASE (src/LICENSE).  If not, see <http:;www.gnu.org/licenses/>. 
;
; ******************************************************************************


pro h5_read, fname, zcomp=zcomp, zreal=zreal, zimag=zimag, zphase=zphase, zamp=zamp, $
             ycomp=ycomp, yreal=yreal, yimag=yimag, yphase=yphase, yamp=yamp, $
             z_vec=z_vec, y_vec=y_vec, wavelength=wavelength, beam=beam, verbose=verbose 
;+
; NAME:
;   h5_read
;
;
; PURPOSE:
;   read genesis-, phase- and pha4idl- hdf5 files with automatic detection of the type
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
;   idl> h5_read,'abc.h5', amp=a, z=z, y=y
;   idl> mycontour, a ,z, y
;
; MODIFICATION HISTORY:
;    25.3.13 UF
;-

if n_elements(fname) eq 0 then fname= DIALOG_PICKFILE(filter='*.h5')

if h5_test(fname) eq 0 then begin
    print, 'file >>',fname,'<< is not a hdf5- exit'
    return
endif

h5type= h5_check_type(fname, verbose=verbose)

if (n_elements(verbose) ne 0) then print, 'h5_read: h5type=', h5type

case h5type of
    7: h5_read_phase, fname, zcomp=zcomp, zreal=zreal, zimag=zimag, zphase=zphase, zamp=zamp, $
      ycomp=ycomp, yreal=yreal, yimag=yimag, yphase=yphase, yamp=yamp, $
      z_vec=z_vec, y_vec=y_vec, wavelength=wavelength, beam=beam, verbose=verbose 
    8: h5_read_genesis, fname, comp=zcomp,  real=zreal, imag=zimag, phase=zphase, amp=zamp, $
      z_vec=z_vec, y_vec=y_vec, wavelength=wavelength, beam=beam, verbose=verbose
    9: h5_read_pha4idl, fname, zcomp=zcomp,  zreal=zreal, zimag=zimag, zphase=zphase, zamp=zamp, $
      ycomp=ycomp, yreal=yreal, yimag=yimag, yphase=yphase, yamp=yamp, $
      z_vec=z_vec, y_vec=y_vec, wavelength=wavelength, beam=beam, verbose=verbose
    else: print, 'no valid hdf5 file'
endcase

print, 'h5_read done'

return
end
;; end
