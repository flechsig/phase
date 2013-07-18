;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/emf_stat.pro
;  Date      : <18 Jul 13 17:34:57 flechsig> 
;  Time-stamp: <18 Jul 13 17:41:06 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro emf_stat, field, z_vec=z_vec, y_vec=y_vec
;+
; NAME:
;   emf_stat
;
;
; PURPOSE:
;   print statistics of a field (does a 2d gaussfit to determine fwhm
;
;
; CATEGORY:
;   emf
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
stat= dblarr(7)
fit= gauss2dfit(field, stat, z_vec, y_vec)
print, 'result of gauss2dfit in (m):', stat
print, 'z fwhm=',stat[2], ' m'
print, 'y fwhm=',stat[3], ' m'
print, 'z0    =',stat[4], ' m'
print, 'y0    =',stat[5], ' m'
return
end
