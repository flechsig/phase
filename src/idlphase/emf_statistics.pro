;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/emf_stat.pro
;  Date      : <18 Jul 13 17:34:57 flechsig> 
;  Time-stamp: <19 Jul 13 09:52:06 flechsig> 
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

ms= size(field)

if n_elements(z_vec) eq 0 the z_vec=dindgen(ms[1])
if n_elements(y_vec) eq 0 the y_vec=dindgen(ms[2])

stat= dblarr(7)
fit= gauss2dfit(field, stat, z_vec, y_vec)
zmin= min(z_vec)
zmax= max(z_vec)
ymin= min(y_vec)
ymax= max(y_vec)
print, '====================='
print, 'emf_statistics'
print, '====================='
print, 'z fwhm=',stat[2], ' m'
print, 'y fwhm=',stat[3], ' m'
print, 'z0    =',stat[4], ' m'
print, 'y0    =',stat[5], ' m'
print, 'zmin, zmax=', zmin, zmax
print, 'ymin, ymax=', ymin, ymax
print, 'result of gauss2dfit in (m):', stat
help, field, y_vec, z_vec
print, '====================='
return
end
