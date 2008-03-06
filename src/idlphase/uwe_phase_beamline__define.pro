;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/phase_beamline__define.pro
;  Date      : <09 Mar 06 10:01:36 flechsig> 
;  Time-stamp: <14 Mar 06 13:09:02 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro phase_beamline::WriteRayFile
lib = !phaseidllib
func= 'WriteRayFileIDL'
ret= CALL_EXTERNAL(lib, func, self.beamline, /AUTO_GLUE)
end

pro phase_beamline::RayTracec
lib = !phaseidllib
func= 'RayTracecIDL'
ret= CALL_EXTERNAL(lib, func, self.beamline, /AUTO_GLUE)
end

pro phase_beamline::RayTraceFull
lib = !phaseidllib
func= 'RayTraceFullIDL'
ret= CALL_EXTERNAL(lib, func, self.beamline, /AUTO_GLUE)
end

pro phase_beamline::makertsource
lib = !phaseidllib
func= 'MakeRTSourceIDL'
ret= CALL_EXTERNAL(lib, func, self.beamline, /AUTO_GLUE)
end 

pro phase_beamline::build
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
;   PHASE
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
;   IDL> bl->build
;
;
; MODIFICATION HISTORY:
;   Written by:  U. Flechsig, 10. 3. 06
;-
lib = !phaseidllib
func= 'phaBLBuildIDL'

ret= CALL_EXTERNAL(lib, func, self.beamline, /AUTO_GLUE)
end

pro phase_beamline::read, filename
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
;   IDL> bl->read, 'clrcm.phase'
;
;
; MODIFICATION HISTORY:
;   Written by:  U. Flechsig, 10. 3. 06
;-

lib = !phaseidllib
func= 'phaBLReadFromFileIDL'

if ((n_params() eq 0)) then begin
  print, 'usage example:' 
  print, 'IDL> s->read, "clrcm.phase"'
  return
endif

ret= CALL_EXTERNAL(lib, func, filename, self.beamline, /AUTO_GLUE)

end ;; read

;; internal function automatically called with object creation
function phase_beamline::init
   ; This routine would do any initialization.
   ; we use it to allocate memory for a struct beamlinetype
   ; the address of the memory is stored as long integer 

   lib   = !phaseidllib
   func  = 'phaBLInitIDL'

   print, 'phase_beamline::init ===> call c function to allocate memory'
   self.beamline= CALL_EXTERNAL(lib, func, /AUTO_GLUE)
   return, 1
end

pro phase_beamline::cleanup
   self->cleanup_ptrs
end


pro phase_beamline__define
;+
; NAME:
;   phase_beamline__define
;
; PURPOSE:
;   This procedure defines the PHASE_BEAMLINE class.
;   The  PHASE_BEAMLINE class is designed to do the following:
;   - Provide an object-oriented interface to standard BEAMLINE in
;     PHASE.
;
; CATEGORY:
;   PHASE
;
;
; CALLING SEQUENCE:
;   This routine cannot be called directly. It is called indirectly as follows:
;   beamline = OBJ_NEW('PHASE_BEAMLINE')
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
;   IDL> bl=obj_new('phase_beamline')
;   IDL> bl->read, 'clrcm.phase' 
;
; MODIFICATION HISTORY:
;   Written by:  U. Flechsig, 9. 3. 06
;-

;xlm={xlenmaptype}
;RESULT={RESULTType}
;BLOptions={OptionsType}
;src={sources}

phase_beamline =  $
   {phase_beamline, $
    beamline: 0L  $   ;; address of struct BeamlineType as long integer

;    xlm:               xlm, $
;    raysout:           ptr_new(), $   ;; pointer to struct RayType
;    RESULT:            RESULT, $
;    beamlineOK:        0U, $
;    elementzahl:       0U, $  
;    position:          0U, $ 
;    hormapsloaded:     0U, $
;    BLOptions:         BLOptions, $
;    deltalambdafactor: 0.0D, $
;    xlen0:             0.0D, $
;    src:               src $
   }
end
;; end phase_beamline__define.pro
