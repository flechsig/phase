;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/phaPutEMField.pro
;  Date      : <20 Aug 13 16:22:23 flechsig> 
;  Time-stamp: <20 Aug 13 16:31:46 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 


function phaPutEMField, ycomp=ycomp, zcomp=zcomp, yreal=yreal, yimag=yimag, zreal=zreal, $
                        zimag=zimag, y_vec=y_vec, z_vec=z_vec, wavelength=wavelength, verbose=verbose
;+
; NAME:
;	phaPutEMField
;
; PURPOSE:
;       Loads phasestyle EMField structure from fields
;
; CATEGORY:
;	pro : pha4idl - create src4
;
; CALLING SEQUENCE: 
;	beam= phaPutEMField()
;
; INPUTS:
;       none
; OUTPUTS:
;     	beam		filled source4 struct
;
; KEYWORDS:
;   [yz]amp:    amplitude (2d)
;   [yz]comp:   complex field (2d)
;   [yz]imag:   imaginary part (2d)
;   [yz]phase:  phase: (2d)
;   [yz]real:   real part (2d)
;   verbose:    verbose
;   wavelength: wavelength
;   y_vec:      vertical vector
;   z_vec:      horizontal vector 	
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;      Aug 2013 UF, new
;
;-

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

    if n_element(wavelength) eq 0 then wavelength= 1234.5
    beam.xlam= wavelength

return, beam
END


