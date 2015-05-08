 ; File      : /afs/psi.ch/project/phase/GIT/phase/src/idlphase/unwrap_herra.pro
 ; Date      : <05 May 15 10:22:30 flechsig> 
 ; Time-stamp: <08 May 15 14:31:26 flechsig> 
 ; Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 ; $Source$ 
 ; $Date$
 ; $Revision$ 
 ; $Author$ 
 

FUNCTION unwrap_herra, data, verbose=verbose
;+
; NAME:
;      UNWRAP_HERRA
;
; PURPOSE:
;      unwrap phase jumps in two-dimensional double arrays
;
; EXPLANATION:
;      2d phase unwrapping using algorithm by  Miguel Arevallilo Herra´ez, 
;      David R. Burton, Michael J. Lalor, and Munther A. Gdeisat
;      published in the Applied Optics, Vol. 41, No. 35, pp. 7437, 2002.
;      The function is implemented in c++ in phaseqt/unwrap_phase.cpp and called
;      using the idl call_external function.
;       
; CALLING SEQUENCE:
;      result=unwrap_herra(data)
;
; INPUTS:
;      data:   a 2D double array (NO COMPLEX ARRAYS!!)
;
; KEYWORD PARAMETERS:
;      verbose: print some debug messages
; 
; OUTPUTS:
;      an hopefully phase unwrapped array 
;
; REVISION HISTORY:
;       written by UF May 2015
;-

print, 'unwrap_herra called (phase unwrapping using Herras code)'
help, data

decision = size(data)
IF decision[0] NE 2 THEN BEGIN
    print, 'Array dimensions other than 2 not supported.'
    return, -1
ENDIF 

nx= decision[1]
ny= decision[2]

;; the shared lib
unwrap_herra_lib= getenv('PHASE_HOME')+'/lib/unwrap_herra.so'

if n_elements(verbose) ne 0 then print,'shared lib: ', unwrap_herra_lib

result = call_external(unwrap_herra_lib,'unwrap_phase',$
                       data,  $
                       nx, ny, $
                       value=[0,1,1], $
                       /AUTO_GLUE, /IGNORE_EXISTING_GLUE, /CDECL, verbose=verbose)

;;print, 'result:', result
;;//unwrap_phase(double *inout, int cols, int rows)

return, data
END
;; end
