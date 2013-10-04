;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/initphase.pro
;  Date      : <04 Oct 13 16:34:58 flechsig> 
;  Time-stamp: <04 Oct 13 16:35:08 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

 
function initphase
;+
; NAME:
;   initphase
;
;
; PURPOSE:
;   generate a new phase object
;
;
; CATEGORY:
;   PHASE
;
;
; CALLING SEQUENCE:
;   emf=initphase()
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
;   IDL> emf=initphase()
;
;
; MODIFICATION HISTORY:
;   U. Flechsig 4. 10. 13
;-


return, obj_new('PHASE')
end
