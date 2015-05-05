 ; File      : /afs/psi.ch/project/phase/GIT/phase/src/idlphase/unwrap_herra.pro
 ; Date      : <05 May 15 10:22:30 flechsig> 
 ; Time-stamp: <05 May 15 10:27:33 flechsig> 
 ; Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 ; $Source$ 
 ; $Date$
 ; $Revision$ 
 ; $Author$ 
 

FUNCTION unwrap_phase, data
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
;       
; CALLING SEQUENCE:
;      result=unwrap_herra(data)
;
; INPUTS:
;      data:   a 2D double array (NO COMPLEX ARRAYS!!)
; 
; OUTPUTS:
;      an hopefully phase unwrapped array 
;
; REVISION HISTORY:
;       written by UF May 2015
;-

print, 'unwrap_herra called'
help, data

decision = size(data)
IF decision[0] NE 2 THEN BEGIN
    print, 'Array dimensions other than 2 not supported.'
    return, -1
ENDIF 

return, result
END
