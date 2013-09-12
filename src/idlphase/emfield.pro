;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/emfield.idl
;  Date      : <12 Sep 13 14:40:40 flechsig> 
;  Time-stamp: <12 Sep 13 15:09:50 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

function emfield, field=field, y_vec=y_vec, z_vec=z_vec, wavelength=wavelength
;+
; NAME:
;   emfield
;
;
; PURPOSE:
;   create a named structure emfield from input values
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;   field: field
;   y_vec: y_vec
;   z_vec: z_vec, 
;   wavelength: wavelength
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
;     UF 9/2013
;-

emf= create_struct('field', field, 'y_vec', y_vec, 'z_vec', z_vec, 'wavelength', wavelength, NAME='emfield')

return, emf
end
