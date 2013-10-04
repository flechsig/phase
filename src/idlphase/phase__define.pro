;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/phase__define.pro
;  Date      : <04 Oct 13 16:26:36 flechsig> 
;  Time-stamp: <04 Oct 13 16:32:10 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

;; the phase object

emfield = $
  {emfield, $
   field: ptr_new(), $
   z_vec: ptr_new(), $
   y_vec: ptr_new(), $
   wavelength: 0.0D $
  }
end
;; end

