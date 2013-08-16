;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/phaBLIinit.pro
;  Date      : <06 Mar 06 16:23:25 flechsig> 
;  Time-stamp: <16 Aug 13 08:44:16 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

function phaBLInitstatic,verbose=verbose
;+
; NAME:
;   phaBLIinitstatic
;
;
; PURPOSE:
;   return a pointer to an empty beamline struct
;
;
; CATEGORY:
;   PHASE
;
;
; CALLING SEQUENCE:
;   mybeamline=phaBLInit()
;
;
; INPUTS:
;   no
;
;
; OPTIONAL INPUTS:
;   no
;
;
; KEYWORD PARAMETERS:
;   verbose: verbose
;
;
; OUTPUTS:
;   pointer to beamline structure
;
;
; OPTIONAL OUTPUTS:
;   no
;
;
; COMMON BLOCKS:
;   no
;
;
; SIDE EFFECTS:
;   no
;
;
; RESTRICTIONS:
;   no
;
;
; PROCEDURE:
;   create a "static struct BeamlineType" returns its address,
;   all fields are empty
;
;
; EXAMPLE:
;   idl>  pgmbeamline=phaBLInit()
;
;
; MODIFICATION HISTORY:
;   UF 6.3.06
;-

if NOT(KEYWORD_SET(debug)) THEN ON_ERROR,2
beamlinestructptr= ptr_new()

IF (CALL_EXTERNAL('/usr/local/lib/libphasecommonabsoft.so', $
                  'phaBLInit_static', beamlinestructptr, /AUTO_GLUE)) THEN BEGIN
        PRINT, 'phaBLIinit_static done'
    ENDIF ELSE MESSAGE,'Call to phaBLIinit_static failed'

;;    PRINT, 'idl: phaBLIinit done'
    return, beamlinestructptr
end

function phaBLInit,verbose=verbose
;+
; NAME:
;   phaBLIinit
;
;
; PURPOSE:
;   return a pointer to an empty beamline struct
;
;
; CATEGORY:
;   PHASE
;
;
; CALLING SEQUENCE:
;   mybeamline=phaBLInit()
;
;
; INPUTS:
;   no
;
;
; OPTIONAL INPUTS:
;   no
;
;
; KEYWORD PARAMETERS:
;   no
;
;
; OUTPUTS:
;   pointer to beamline structure
;
;
; OPTIONAL OUTPUTS:
;   no
;
;
; COMMON BLOCKS:
;   no
;
;
; SIDE EFFECTS:
;   no
;
;
; RESTRICTIONS:
;   no
;
;
; PROCEDURE:
;   create a "static struct BeamlineType" returns its address,
;   all fields are empty
;
;
; EXAMPLE:
;   idl>  pgmbeamline=phaBLInit()
;
;
; MODIFICATION HISTORY:
;   UF 6.3.06
;-

if NOT(KEYWORD_SET(debug)) THEN ON_ERROR,2
lib = '/usr/local/lib/libphasecommonabsoft.so'

;; returns idl_long
beamlinestructptr= CALL_EXTERNAL(lib, 'phaBLInitIDL', /AUTO_GLUE)
        

    PRINT, 'idl: phaBLIinit done'
    return, beamlinestructptr
end



;; end/afs/psi.ch/user/f/flechsig/phase/src/phaseidl/phaBLInit.pro
