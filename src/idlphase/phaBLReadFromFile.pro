;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/phaBLReadFromFile.pro
;  Date      : <10 Mar 06 15:03:31 flechsig> 
;  Time-stamp: <15 May 07 09:49:15 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro PhaBLReadFromFile, filename, blptr
;+
; NAME:
;
;
;
; PURPOSE:
;
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
;
;-



func= 'phaBLReadFromFileIDL'
lib = '/usr/local/lib/libphasecommonabsoft.so'

if ((n_params() eq 0)) then begin
  print, 'usage example:' 
  print, 'IDL> s->read, "clrcm.phase"'
  return
endif

IF (CALL_EXTERNAL(lib, func, $
                  filename, blptr, /AUTO_GLUE)) THEN BEGIN
        PRINT, func, ' done'
    ENDIF ELSE MESSAGE,'Call to ', func, ' failed'

end
