 ; File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/readhenke.pro
 ; Date      : <20 Dec 13 09:57:10 flechsig> 
 ; Time-stamp: <07 Jan 14 09:03:12 flechsig> 
 ; Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 ; $Source$ 
 ; $Date$
 ; $Revision$ 
 ; $Author$ 

pro reflec, element, en, r, theta= theta, plot=plot, verbose=verbose
;+
; NAME:
;   reflec
;
; PURPOSE:
;   calculate reflectivity as function of photon energy 
;
; CATEGORY:
;   generic
;
; CALLING SEQUENCE:
;   reflec, element, en, r, theta= theta, plot=plot, verbose=verbose
;
; INPUTS:
;   element: element as chemical formula (String, case sensitive)
;
; OPTIONAL INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   plot: do a plot
;   theta: angle to normal in rad
;   verbose: print filename
;
; OPTIONAL OUTPUTS:
;   energy: photon energy in eV
;   f1: f1
;   f2: f2;
;
; PROCEDURE:
;   calls readhenke
;
; EXAMPLE:
;   idl> reflec, 'Au', en, theta=!pi/4.0, /verbose, /plot
;
; MODIFICATION HISTORY:
;  UF 19.12.13
;-

usage= 'usage: reflec, element'

if n_elements(element) eq 0 then begin
    print, usage 
    return
endif

readhenke, element, en0, f10, f20

if n_elements(en) eq 0 then en=en0
f1= interpol(f10, en0, en)
f2= interpol(f20, en0, en)



lambda= 1240e-9/ en
r0    = 2.81794e-15 
k     = 2.0 * !dpi/ lambda
beta  = r0/(2.0*!dpi) * lambda^2 * f2
rho   = 1.0
delta = (2.0*!dpi) * r0 * rho /k^2
ref   = en^2  ;; UF not yet ready

if n_elements(plot) ne 0 then begin
    plot, [20, 40000], [0, max(ref)*1.1], /nodata, xtitle='E (eV)', $
      ytitle='reflectivity', title=element, /xlog 
    oplot, en, ref, color=1
endif

return
end
