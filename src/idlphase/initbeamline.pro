function initbeamline
;+
; NAME:
;   initbeamline
;
;
; PURPOSE:
;   generate a beamline object, define phaseidllib if undefined
;
;
; CATEGORY:
;   PHASE
;
;
; CALLING SEQUENCE:
;   beamline=initbeamline()
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
;
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
;   U. Flechsig 13.3.06
;-

if (!phaseidllib eq '') then $
   defsysv, '!phaseidllib', '/usr/local/lib/libphabaseabsoft.so'

return, obj_new('scan')
end
