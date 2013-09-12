;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/emfield.idl
;  Date      : <12 Sep 13 14:40:40 flechsig> 
;  Time-stamp: <12 Sep 13 15:32:01 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro emfield2vars, emf, field=field, y_vec=y_vec, z_vec=z_vec, wavelength=wavelength
;+
; NAME:
;   emfield2vars
;
;
; PURPOSE:
;   export variables from emfield structure
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
;   emfield structure
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
;   IDL> emfield2vars, emf, z_vec=z_vec, y_vec=y_vec, field=field, wavelength=wavelength
;   IDL> help, emf, /struct
;
; MODIFICATION HISTORY:
;     UF 9/2013
;-

if n_elements(field)      ne 0 then field     = emf.field
if n_elements(y_vec)      ne 0 then y_vec     = emf.y_vec
if n_elements(z_vec)      ne 0 then z_vec     = emf.z_vec
if n_elements(wavelength) ne 0 then wavelength= emf.wavelength

return
end
