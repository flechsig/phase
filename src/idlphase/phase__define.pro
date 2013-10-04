;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/phase__define.pro
;  Date      : <04 Oct 13 16:26:36 flechsig> 
;  Time-stamp: <04 Oct 13 17:56:19 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 


pro phase::gaussbeam, _EXTRA=extra
;+
; NAME:
;   phase::gaussbeam
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
;   phase->gaussbeam
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
self.name = 'Gaussbeam'
gaussbeam, emf, _EXTRA=extra
self.wavelength= emf.wavelength
self.field= ptr_new(emf.field)
self.z_vec= ptr_new(emf.z_vec)
self.y_vec= ptr_new(emf.y_vec)
return 
end

pro phase::plotintensity, _EXTRA=extra
;+
; NAME:
;   phase::plotintensity
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
title= self.name+ ' intensity'
mycontour, abs(*self.field)^2, *self.z_vec*1e3, *self.y_vec*1e3, title=title, xtitle='z (mm)', ytitle='y (mm)', ztitle='intensity'
return 
end

pro phase::plotphase, _EXTRA=extra
;+
; NAME:
;   phase::plotphase
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

title= self.name+ ' phase'
mycontour, atan(*self.field, phase), *self.z_vec*1e3, *self.y_vec*1e3, title=title, xtitle='z (mm)', ytitle='y (mm)', ztitle='phase'
return 
end

pro phase::setTitle, title
;+
; NAME:
;   phase::setTitle
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
self.title=title
return
end


;; the phase object
pro phase__define
;+
; NAME:
;   phase__define
;
;
; PURPOSE:
;   This is the definition code which is invoked when a new object of
;   type PHASE is created. It cannot be called directly, but only
;   indirectly by the IDL OBJ_NEW() function.  It defines the data
;   structures used for the PHASE class.
;
;
; CATEGORY:
;   PHASE
;
;
; CALLING SEQUENCE:
;   Result = OBJ_NEW('PHASE')
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
;  UF 4. 10. 13
;-


phase = $
  {phase, $
   name:  '', $
   field: ptr_new(), $
   z_vec: ptr_new(), $
   y_vec: ptr_new(), $
   wavelength: 0.0D $
  }
end
;; end

