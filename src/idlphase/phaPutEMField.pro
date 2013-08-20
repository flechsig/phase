;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/phaPutEMField.pro
;  Date      : <20 Aug 13 16:22:23 flechsig> 
;  Time-stamp: <20 Aug 13 16:25:13 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 


function phaPutEMField,  _EXTRA = e
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
;	phaPutEMField()
;
; INPUTS:
;	      MainFileName	prefix for the phase EMField files
;			    phase adds the following postfixes:
;			    -ezre, -ezim, -eyre, -eyim  : real and imaginary 
;				  of the EM-fields with z & y polarizaiton
;
;       lambda  wavelength in nm
;
;       [optional]: SCALE=[double] scaling factor for field dimensions
;                   default is 1.0, i.e. units in file are interpreted as [mm]
;                   (often file format is in [m], therefore SCALE=10^3 is needed)
; OUTPUTS:
;     	beam		filled source4 struct
;
; KEYWORDS:
;	None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;      Aug 2013 UF, new
;
;-

np=n_params()

if np lt 2 then begin
	print, 'Too few number of arguments ...'
	print, 'Usage:phaSrcLoadEMField(MainFileName[string], lambda[double], {SCALE=[double]}) '
	print, ''
	retall
endif

beam={source4}

return, beam
END


