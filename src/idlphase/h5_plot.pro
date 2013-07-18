;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <18 Jul 13 16:24:56 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro h5_plot, fname  
;+
; NAME:
;   h5_plot
;
;
; PURPOSE:
;   plot the intensity in a genesis or phase hdf5 file plus some statistics
;
; CATEGORY:
;   hdf5
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;   fname: filename
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
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
;   idl> h5_plot,'abc.h5'
;
; MODIFICATION HISTORY:
;    25.3.13 UF
;-

if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.h5'

if h5_test(fname) eq 0 then begin
    print, 'file >>',fname,'<< is not a hdf5- exit'
    return
endif

h5_read, fname, zamp=zamp, z_vec=z_vec, y_vec=y_vec
mycontour, zamp, z_vec*1e3, y_vec*1e3, xtitle='z (mm)', ytitle='y (mm)', title='Ez intensity'

stat= dblarr(7)
fit= gauss2dfit(zamp, stat, z_vec, y_vec)
print, 'result of gauss2dfit in (m):', stat
print, 'z fwhm=',stat[2], ' m'
print, 'y fwhm=',stat[3], ' m'
print, 'z0    =',stat[4], ' m'
print, 'y0    =',stat[5], ' m'
return
end
;; end
