 ; File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/readhenke.pro
 ; Date      : <20 Dec 13 09:57:10 flechsig> 
 ; Time-stamp: <05 Feb 14 14:31:53 flechsig> 
 ; Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 ; $Source$ 
 ; $Date$
 ; $Revision$ 
 ; $Author$ 

pro reflec, element, en, theta, crp=crp, crs=crs, plot=plot, verbose=verbose, rp=rp, $
            rs=rs, n=n, delta=delta, beta=beta, ac=ac, ts=ts, tp=tp, _extra=extra
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
;   reflec, element, en, theta, plot=plot, verbose=verbose
;
; INPUTS:
;   element: element as chemical formula (String, case sensitive)
;   energy:  energy vector
;   theta:   Grazing incidence angle (rad)
;
; OPTIONAL INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   ac:      critical grazing angle in rad (output)
;   crp:     complex reflectivity rp (output
;   crs:     complex reflectivity rs (output
;   beta:    Im(n)= beta, imaginary part of refractive index (output)
;   delta:   Re(n)= 1-delta, real part of refractive index (output)
;   plot:    do a plot
;   n:       complex refractive index n= 1-delta + j*beta (output)
;   rp:      reflectivity rp
;   rs:      reflectivity rs
;   tp:      transmission tp
;   ts:      transmission ts
;   verbose: print filename
;
; OPTIONAL OUTPUTS:
;
; PROCEDURE:
;   calls readhenke
;   calls readmaterial
;
; EXAMPLE:
;   idl> en=dindgen(101)/100*970+30 
;   idl> reflec, 'Au', en, 4e-3, /plot
;
; MODIFICATION HISTORY:
;  UF 19.12.13
;-

usage= 'usage: reflec, ''Au'', en, theta=4e-3, r, /plot'

if n_elements(element) eq 0 then begin
    print, usage 
    return
endif

readhenke   , element, en0, f10, f20
readmaterial, element, Z  , A  , rho

if n_elements(en) eq 0 then en=en0

f1    = interpol(f10, en0, en)
f2    = interpol(f20, en0, en)

;help, f1,f2

NA    = 6.0221e23                                              ; Avogadronumber
re    = 2.81794e-15                                            ; Classical electron radius (m)
Nt    = 1e6* rho * NA / A                                      ; Teilchendichte  (1/m^3), rho is in (g/cm^3)
lambda= 1240e-9/ en                                            ; Wavelength      (m)

delta = re * lambda^2 * Nt * f1 / (2.0 * !dpi)
beta  = re * lambda^2 * Nt * f2 / (2.0 * !dpi)
n     = complex(1-delta, beta, /double)                        ; complex index of refraction
ac    = acos(1.0 - delta)                                      ; critical (grazing) angle in rad
 
;help, delta, beta, n
;print, 'd= ' , delta, 'beta = ', beta, 'n = ',n

wu    = sqrt( n^2 - (cos(theta))^2 )                         ; Fresnel - formulas
crs   = (      sin(theta) - wu ) / (      sin(theta) + wu)   ; reflection coeff. s-pol
cts   = (  2 * sin(theta)      ) / (      sin(theta) + wu)   ; transmiss. coeff. s-pol

crp   = (n^2 * sin(theta) - wu ) / ( n^2 *sin(theta) + wu)   ; reflection coeff. p-pol
ctp   = (2*n * sin(theta)      ) / ( n^2 *sin(theta) + wu)   ; transmiss. coeff. s-pol

Rs    =  abs(crs)^2                                          ; reflectance s-pol   
Rp    =  abs(crp)^2                                          ; reflectance p-pol.  
Ts    =  abs( 2*  wu / (      sin(theta) + wu))^2            ; transmitance s-pol        
Tp    =  abs( 2*n*wu / (n^2 * sin(theta) + wu))^2            ; transmitance p-pol  

if n_elements(plot) ne 0 then begin
   thetag= theta*180./!pi 
   title = element + '   GI-angle = '+ string(theta, FORMAT="(f8.6)") + ' rad ('+ string(thetag, FORMAT="(f6.2)")+' deg.)'
 
   plot , [20, 40000], [1e-3, max(Rs)*1.1], /nodata, xtitle='E (eV)', $
      ytitle='reflectivity', title=title, /xlog, _extra=extra 

   oplot, en, Rs, color=1
   oplot, en, Rp, color=2

   legend, ['Rs', 'Rp'], color=[1,2], linestyle=[0,0], /right
endif

return
end
