;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO phaPhaseshift, beam, shift_function
;+
; NAME:
;	phaPhaseshift
;
; PURPOSE:
;       Multiplies each complex gridpoint c(y,z) by
;       phasefactor exp(i * d), 
;	where d = d(y-y0, z-z0) is a function provided
;	by the user.
;
; CATEGORY:
;	      pro : pha4idl - modifiy grid field
;
; CALLING SEQUENCE:
;	      phaPhaseshift, beam, shift_function
;
; INPUTS:
;     	beam:	pha4idl src4beam
;	shift_function: name of a custom function defined like 'FUNCTION funcname, dist_y, dist_z'
;			which returns the phaseshift as function of the distances
;			to the grid's center
;	
;	
; OUTPUTS:
;     	None.
;
; KEYWORDS:
;	      None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS: 
;
; MODIFICATION HISTORY:
;    November 4, 2010, SG, initial version
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
y=get_pha_src4_axis_y(beam)
z=get_pha_src4_axis_z(beam)

i0 = beam.iezrey/2-1;
j0 = beam.iezrex/2-1;

y0 = y(i0);
z0 = z(j0);

FOR i = 0, beam.iezrey-1 DO BEGIN
FOR j = 0, beam.iezrex-1 DO BEGIN
  y1 = y(i)
  z1 = z(j)
  
  ;call user defined function with distances-to-center
  delta = Call_Function(shift_function, y1-y0, absz1-z0);
  expo = complex(0, 1) * delta;
  
  ; multiply E-field by (complex) factor
  zez = complex(beam.zezre(i, j), beam.zezim(i, j))
  zey = complex(beam.zeyre(i, j), beam.zeyim(i, j))
  
  zez *= exp(expo)
  zey *= exp(expo)
  
  beam.zezre(i, j) = float(zez);
  beam.zezim(i, j) = imaginary(zez);
  beam.zeyre(i, j) = float(zey);
  beam.zeyim(i, j) = imaginary(zey);
ENDFOR
ENDFOR

END
