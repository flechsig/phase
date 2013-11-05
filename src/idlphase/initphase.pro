;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/initphase.pro
;  Date      : <04 Oct 13 16:34:58 flechsig> 
;  Time-stamp: <05 Nov 13 16:44:14 flechsig> 
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
; PURPOSE:
;   generate a new phase object
;
; CATEGORY:
;   PHASE
;
; CALLING SEQUENCE:
;   emf=initphase()
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   no
;
; OUTPUTS:
;   object reference
;
; EXAMPLE:
;   IDL> emf=initphase()
;
;; MODIFICATION HISTORY:
;   U. Flechsig 4. 10. 13
;-

return, obj_new('PHASE')
end
;; end
