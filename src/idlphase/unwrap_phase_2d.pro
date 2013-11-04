 ; File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/unwrap_phase_2d.pro
 ; Date      : <04 Nov 13 17:16:50 flechsig> 
 ; Time-stamp: <04 Nov 13 17:16:54 flechsig> 
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
