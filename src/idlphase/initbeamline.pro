;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/initbeamline.pro
;  Date      : <13 Mar 06 08:54:48 flechsig> 
;  Time-stamp: <13 Mar 06 09:31:00 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$
 
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

defsysv, '!phaseidllib', exist=exist
if (exist eq 0) then $
   defsysv, '!phaseidllib', '/usr/local/lib/libphabaseabsoft.so'

print, 'initbeamline: use shared lib: ', !phaseidllib 

return, obj_new('PHASE_BEAMLINE')
end
