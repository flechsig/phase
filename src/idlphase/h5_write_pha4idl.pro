;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <20 Aug 13 16:17:21 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro h5_write_pha4idl, fname, ycomp=ycomp, zcomp=zcomp, yreal=yreal, yimag=yimag, zreal=zreal, $
                    zimag=zimag, y_vec=y_vec, z_vec=z_vec, beam=beam, $
                    verbose=verbose
;+
; NAME:
;   h5_write_phase
;
;
; PURPOSE:
;   write pha4idl hdf5  format
;
;
; CATEGORY:
;   phase_h5
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

if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/myphase.h5'

if n_elements(beam) eq 0 then begin
    print, 'fill beam structure'
    beam={source4}

    if n_elements(ycomp) ne 0 then begin
        yreal= real_part(ycomp)
        yimag= imaginary(ycomp)
    endif
    
    if n_elements(zcomp) ne 0 then begin
        zreal= real_part(zcomp)
        zimag= imaginary(zcomp)
    endif

    nz=n_elements(z_vec)
    ny=n_elements(y_vec)

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

    beam.xlam= 1234.5

endif ;; no beam given


phaSaveEMFieldHDF5, beam, fname

print, 'wrote pha4idl file: ', fname
return
end
;; end
