;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/emf_stat.pro
;  Date      : <18 Jul 13 17:34:57 flechsig> 
;  Time-stamp: <19 Jul 13 08:32:39 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro emf_statistics, field, z_vec=z_vec, y_vec=y_vec, yfwhm=yfwhm, zfwhm=zfwhm
;+
; NAME:
;   emf_statistics
;
;
; PURPOSE:
;   print statistics of a field (does a 2d gaussfit to determine fwhm)
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
;   the field
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;   yfwhm: vertical fwhm (output)
;   y_vec: vertical vector
;   zfwhm: horizontal fwhm (output)
;   z_vec: horizontal vector
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
;  idl> emf_stat, amp, y_vec=y, z_vec=z
;
;
; MODIFICATION HISTORY:
;   UF Jul 2013
;-

stat= dblarr(7)
fit= gauss2dfit(field, stat, z_vec, y_vec)
print, '=============='
print, 'emf_statistics'
print, '=============='
help, field, y_vec, z_vec
print, 'result of gauss2dfit in (m):', stat
print, 'z fwhm=',stat[2], ' m'
print, 'y fwhm=',stat[3], ' m'
print, 'z0    =',stat[4], ' m'
print, 'y0    =',stat[5], ' m'
print, '=============='
return
end
