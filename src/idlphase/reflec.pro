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
;   calls readmaterial
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

readhenke   , element, en0, f10, f20
readmaterial, element, Z  , A  , rho

if n_elements(en) eq 0 then en=en0

f1    = interpol(f10, en0, en)
f2    = interpol(f20, en0, en)

help, f1,f2

NA    = 6.0221e23                                              ; Avogadronumber
re    = 2.81794e-15                                            ; Classical electron radius (m)
Nt    = 1e6* rho * NA / A                                      ; Teilchendichte  (1/m^3), rho is in (g/cm^3)
lambda= 1240e-9/ en                                            ; Wavelength      (m)

delta = re * lambda^2 * Nt * f1 / (2.0 * !dpi)
beta  = re * lambda^2 * Nt * f2 / (2.0 * !dpi)
n     = complex(1-delta, beta, /double)                        ; complex index of refraction

help, delta, beta, n
;print, 'd= ' , delta, 'beta = ', beta, 'n = ',n

wu    = sqrt( n^2 - (cos(theta))^2 )                           ; Fresnel - formulas
rs    = (        sin(theta) - wu ) / (      sin(theta) + wu)   ; reflection coeff. s-pol
rp    = (    n^2*sin(theta) - wu ) / ( n^2 *sin(theta) + wu)   ; reflection coeff. p-pol

Rs    =  abs(rs)^2                                             ; reflectance s-pol   
Rp    =  abs(rp)^2                                             ; reflectance p-pol.  

if n_elements(plot) ne 0 then begin
    plot, [20, 40000], [0, max(Rs)*1.1], /nodata, xtitle='E (eV)', $
      ytitle='reflectivity', title=element, /xlog 
    oplot, en, Rs, color=1
endif

r = Rs

return
end
