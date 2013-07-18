;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/h5_test.pro
;  Date      : <18 Jul 13 09:04:13 flechsig> 
;  Time-stamp: <18 Jul 13 09:27:22 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

function h5_test, fname, verbose=verbose
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
;   filename as string
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
;   1 if hdf5 found else 0
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
;   UF Jul 2013
;-

if n_elements(fname) eq 0 then $
  fname='/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.phase_hdf5.h5'
;;if n_elements(verbose) ne 0 then verbose=verbose

cmd= 'file -b '+ fname
spawn, cmd, result
if result eq 'Hierarchical Data Format (version 5) data' then begin
    if verbose then print, '>>', fname, '<< is a hdf5 file' 
    ret= 1
endif else begin
    if verbose then print, '>>',fname, '<< is not a hdf5 file' 
    ret= 0
endelse

return, ret
end

