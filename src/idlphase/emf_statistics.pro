;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/emf_stat.pro
;  Date      : <18 Jul 13 17:34:57 flechsig> 
;  Time-stamp: <06 Dec 13 14:19:46 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro emf_statistics, fields, yfwhm=yfwhm, zfwhm=zfwhm, ysig=ysig, zsig= zsig
;+
; NAME:
;   emf_statistics
;
;
; PURPOSE:
;   print statistics of a field (does a 2d gaussfit to determine
;   fwhm), export fwhm if requested
;
;
; CATEGORY:
;   emf
;
; CALLING SEQUENCE:
;    emf_statistics, field
;
; INPUTS:
;   the field strct
;
; KEYWORD PARAMETERS:
;   yfwhm: vertical fwhm (output)
;   ysig : vertical rms (output)
;   zfwhm: horizontal fwhm (output)
;   zsig : horizontal rms (output)
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; PROCEDURE:
;
; EXAMPLE:
;  idl> emf_stat, amp, y_vec=y, z_vec=z
;
; MODIFICATION HISTORY:
;   UF Jul 2013
;-

ms= size(fields.field)

;field_n =dindgen(ms[1],ms[2])
field_n= fields.field/max(fields.field)

stat= dblarr(7)
fit= gauss2dfit(field_n, stat, fields.z_vec, fields.y_vec)
zmin= min(fields.z_vec)
zmax= max(fields.z_vec)
ymin= min(fields.y_vec)
ymax= max(fields.y_vec)
print, '====================='
print, 'emf_statistics'
print, '====================='
print, 'z fwhm=',stat[2]*2.35, ' m, rms = ',stat[2], ' m'
print, 'y fwhm=',stat[3]*2.35, ' m, rms = ',stat[3], ' m'
print, 'z0    =',stat[4], ' m'
print, 'y0    =',stat[5], ' m'
print, 'zmin, zmax (m) =', zmin, zmax, 'nz=', n_elements(fields.z_vec)
print, 'ymin, ymax (m) =', ymin, ymax, 'ny=', n_elements(fields.y_vec)
print, '====================='
print, 'result of gauss2dfit in (m):', stat
print, '====================='
return
end
