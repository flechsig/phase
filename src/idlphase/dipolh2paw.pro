 ; File      : /afs/psi.ch/project/phase/GIT/phase/src/idlphase/unwrap_herra.pro
 ; Date      : <05 May 15 10:22:30 flechsig> 
 ; Time-stamp: <11 Sep 15 13:38:02 flechsig> 
 ; Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 ; $Source$ 
 ; $Date$
 ; $Revision$ 
 ; $Author$ 
 

FUNCTION dipolh2paw, x, verbose=verbose
;+
; NAME:
;      DIPOLH2
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
;      result=dipolh2(data)
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

print, 'dipolh2 called (phase unwrapping using Herras code)'
help, data

 

nx= n_elements(x)
y = x* 1.0

;; the shared lib
c_c340_main_lib= getenv('PHASE_HOME')+'/lib/c_c340_main.so'

if n_elements(verbose) ne 0 then print,'shared lib: ', c_c340_main_lib

result = call_external(c_c340_main_lib,'c_c340_main',$
                       nx,  $
                       1.0, x, y, 2, $
                       value=[0,1,1,1,1], $
                       /AUTO_GLUE, /IGNORE_EXISTING_GLUE, /CDECL, verbose=verbose)

;;print, 'result:', result
;;//unwrap_phase(double *inout, int cols, int rows)

return, data
END
;; end
