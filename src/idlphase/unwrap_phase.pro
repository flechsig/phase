 ; File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/unwrap_phase.pro
 ; Date      : <04 Nov 13 17:14:36 flechsig> 
 ; Time-stamp: <06 Nov 13 09:34:15 flechsig> 
 ; Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 ; $Source$ 
 ; $Date$
 ; $Revision$ 
 ; $Author$ 

FUNCTION unwrap_phase_2d, data
;; first correct a reference column
result = data
refcol = transpose(result[0,*])
result[0,*]=unwrap_phase(refcol)
;; now correct all rows
FOR count = 0,(size(result))[2]-1 DO BEGIN
    refrow = result[*,count]
    result[*,count]=unwrap_phase(refrow)
ENDFOR
;; then correct all columns again to be sure
FOR count = 0,(size(result))[1]-1 DO BEGIN
    refrow = transpose(result[count,*])
    result[count,*]=unwrap_phase(refrow)
ENDFOR
return, result

END


FUNCTION unwrap_phase, data
;+
; NAME:
;      UNWRAP_PHASE
;
; PURPOSE:
;      unwrap phase jumps in one and two-dimensional arrays
;
; EXPLANATION:
;      when transforming from a (real,imaginary)-representation of
;      complex numbers to a (argument, phase)-representation,
;      undesired phase jumps may occur. This routines searches for
;      these jumps and corrects them by adding/subtrating multiples of
;      2*PI. This routine may be called with one and two-dimensional
;      arrays. If the data is 2D, the routine UNWRAP_PHASE_2D will be
;      automatically called.
;       
; CALLING SEQUENCE:
;      result=unwrap_phase(data)
;
; INPUTS:
;      data:   a 1D or 2D real array (NO COMPLEX ARRAYS!!)
; 
; OUTPUTS:
;      an hopefully phase unwrapped array 
;
; PROCEDURES USED:
;      UNWRAP_PHASE_2D
;
; REVISION HISTORY:
;       written by A. R. Weiss (ARW), MPI for Astronomy, Heidelberg,
;       Germany, Sep. 1999
;-


decision = size(data)
IF decision[0] GT 2 THEN BEGIN
    print, 'Array dimensions higher than 2 not supported.'
    return, -1
ENDIF 
IF decision[0] EQ 2 THEN BEGIN
    result = unwrap_phase_2d(data)
    return, result
ENDIF ELSE BEGIN
    ;; find the phase jump locations
    jumps = data - shift(data,1)
    jumps[0]=0.0
    jumps = long(jumps GE !DPI) - long(jumps LE -!DPI)
    jumpindex = where(jumps NE 0)
    result = data
    IF jumpindex[0] NE -1 THEN BEGIN
        FOR count=0,n_elements(jumpindex)-1 DO BEGIN
            result[jumpindex[count]:*] = $
              result[jumpindex[count]:*] $
              -2*!DPI*jumps[jumpindex[count]]
        ENDFOR
    ENDIF
    return, result
ENDELSE
END
