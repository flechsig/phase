function initbeamline
;+
; NAME:
;   initbeamline
;
;
; PURPOSE:
;   generate a new beamline object, define phaseidllib if undefined
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
;   IDL> clrcm=initbeamline()
;
;
; MODIFICATION HISTORY:
;   U. Flechsig 13.3.06
;-

if (!phaseidllib eq '') then $
   defsysv, '!phaseidllib', '/usr/local/lib/libphabaseabsoft.so'

return, obj_new('PHASE_BEAMLINE')
end
