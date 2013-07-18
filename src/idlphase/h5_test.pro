;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/h5_test.pro
;  Date      : <18 Jul 13 09:04:13 flechsig> 
;  Time-stamp: <18 Jul 13 09:06:20 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

function h5_test, fname
;+
; NAME:
;   h5_test
;
;
; PURPOSE:
;   test if hdf5 file
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

if n_elements(fname) eq 0 then $
  fname='/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.phase_hdf5.h5'

return
end

