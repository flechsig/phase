;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/phase__define.pro
;  Date      : <04 Oct 13 16:26:36 flechsig> 
;  Time-stamp: <25 Nov 14 15:25:19 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

; ******************************************************************************
;
;   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
;                      Paul Scherrer Institut Villigen, Switzerland
;   
;   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
;          Uwe Flechsig,    uwe.flechsig@psi.ch
;
; ------------------------------------------------------------------------------
;
;   This file is part of PHASE.
;
;   PHASE is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, version 3 of the License, or
;   (at your option) any later version.
;
;   PHASE is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with PHASE (src/LICENSE).  If not, see <http:;www.gnu.org/licenses/>. 
;
; ******************************************************************************

pro phase::aperture, _EXTRA=extra
;+
; NAME:
;   phase::aperture
;
; PURPOSE:
;   acts as an aperture, or generates wavefield with amplitude
;   according to 'type', 
;   calls internally the routine aperture.pro- see docs there for updated features
;
;   type 1  : rectangular              P1 = hsize, P2 = vsize
;   type 2  : vertical slit            P1 = hsize, P2 = hpos (default= 0)
;   type 3  : horizontal slit          P1 = vsize, P2 = vpos (default= 0)
;   type 12 : double slit vertical,    P1 = hsize, P2= hsep
;   type 13 : double slit horizontal,  P1 = vsize, P2= vsep
;   type 20 : circular     P1 = Radius
;                          Radius > 0: central circular part is transparent
;                          Radius < 0: central circular part is oblique
;   type 21 : annular      P1 = Outer radius, P2 = inner radius
;   type 32 : vertical mirror          P1 = length, P2 = grazing angle (rad)
;   type 33 : horizontalal mirror      P1 = length, P2 = grazing angle (rad)
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   field:      input field, idl complex array, 
;   example:    vertical double slit
;   P1:         generic parameter (depends on type)
;   P2:         generic parameter (depends on type)
;   y_vec:      vertical input vector (required) in m
;   z_vec:      horizontal input vector (required) in m
;   type :      type of aperture
;
;   N, size :   if N and size are defined, a wavefield with amplitude
;                according to the aperture type is generated.
;
; OUTPUTS:
;   no
;
; EXAMPLE:
;   idl> emf->aperture, type=1, p1=1e-4, p2=5e-5, N=243, size=1e-3
;
; MODIFICATION HISTORY:
;   UF NOV 2013
;-
emf= emfield(field=*self.field, y_vec=*self.y_vec, z_vec=*self.z_vec, wavelength=self.wavelength)
aperture, emf, _EXTRA=extra
*self.field= emf.field
*self.z_vec= emf.z_vec
*self.y_vec= emf.y_vec
return
end ;; aperture

pro phase::check_sampling, drift=drift, ratio=ratio, verbose=verbose
;+
; NAME:
;   phase::check_sampling
;
; PURPOSE:
;   checks critical sampling
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;    
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   drift     : drift distance in m
;   ratio     : sampling ratio
;   /verbose  : default verbose on
;
; OUTPUTS:
;   no
;
; EXAMPLE:
;  idl> emf->check_sampling, drift=drift
;
; MODIFICATION HISTORY:
;   UF 26.5.14
;-
if n_elements(verbose) eq 0 then verbose=1

myz_vec = *self.z_vec
myy_vec = *self.y_vec
mylambda= self.wavelength
cols    = n_elements(myz_vec)
rows    = n_elements(myy_vec)
zwidth  = myz_vec[cols-1]- myz_vec[0] ;
ywidth  = myy_vec[rows-1]- myy_vec[0] ;

lambda_x_x= mylambda* drift

yratio= 1.0/(lambda_x_x * rows/ywidth^2)
zratio= 1.0/(lambda_x_x * cols/zwidth^2) 

ratio= 0.5 * (yratio + zratio)  

myydrift= ywidth^2/ rows/ mylambda
myzdrift= zwidth^2/ cols/ mylambda
mydrift = 0.5* (myydrift+ myzdrift)

if verbose then begin
    print, 'check_sampling, ratio= ', ratio
    print, 'critical_sampling= ', lambda_x_x, ' (m^2)'
    print, 'act. hor_sampling= ', zwidth^2/ cols, ' (m^2)'
    print, 'act.vert_sampling= ', ywidth^2/ rows, ' (m^2)'
    
    if (ratio gt 1.0) then begin
        print, 'drift= ', drift, ' yields to oversampling'
        print, 'recommend transfer function (TR) based propagator (fourier)'
    endif else begin
        print, 'drift= ', drift, ' yields to undersampling'
        print, 'recommend impulse response (IR) based propagator (fresnel, fraunhofer)'
    endelse
    
    print, 'critical drift= ', mydrift
endif ;; verbose
return
end ;; check_sampling

pro phase::crl, _EXTRA=extra
;+
; NAME:
;   phase::crl
;
; PURPOSE:
;   calculate the electric field after a parabolic compound refractive
;   (Be) lens, (thin lens approximation), units (m) and (rad)
;
; CATEGORY:
;   Phase
;
; CALLING SEQUENCE:
;   phase->crl
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   field     : input field, idl complex array, 
;               idl complex array, 
;               will be overwritten to give results.
;   y_vec     : vertical input vector   (required) in m
;   z_vec     : horizontal input vector (required) in m
;   wavelength: the wavelength                     in m
;
;   radius    : the lens radius                    in m
;   thickness : the thickness of the lens on axis  in m
;   size      : Aperture (diameter) of lens        in m
;   crlamp:     OUTPUT crl amplitude factor
;   crlphase:   OUTPUT crl phase factor
;
; OUTPUTS:
;   no
;
; PROCEDURE:
;
; EXAMPLE:
;   idl> emf->crl
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-
crl, field=*self.field, y_vec=*self.y_vec, z_vec=*self.z_vec, wavelength=self.wavelength, _EXTRA=extra
return 
end
;; end crl

pro phase::fermidiracbeam, example=example, fwhm=fwhm, plot=plot, slope=slope, $
         wavelength=wavelength, z_off=z_off, y_off=y_off, sizez=sizez, sizey=sizey, Nz=Nz, Ny=Ny
;+
; NAME:
;   phase::fermidiracbeam
;
; PURPOSE:
;   generate fermidirac like beam
;
; CATEGORY:
;   Phase
;
; CALLING SEQUENCE:
;   phase->fermidiracbeam
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   example:      example calculation plus plot (HeNe laser)
;   fwhm:         fwhm of field               in m fwhminensity=fwhmfield/sqrt(0.5)
;   dist:         distance to waist           in m
;   wavelength    the wavelength              in m
;   Nz            points hor.
;   Ny            points vert, default = Nz
;   sizey:        height (m),  default = sizez
;   sizez:        width (m);
;   slope:        dieout factor, default: 0.05*fwhm
;
; OUTPUTS:
;   no
;
; PROCEDURE:
;   fills the object 
;
; EXAMPLE:
;   idl> emf->fermidiracbeam, Nz=243, sizez=0.0002, fwhm=27.7e-6 , wavelength=1.24e-10
;
; MODIFICATION HISTORY:
;   UF Jun 2014
;-
self.name = 'Fermi-Dirac-Beam'
print, 'fermidiracbeam called'

IF KEYWORD_SET(EXAMPLE) THEN BEGIN
    print, '**********************************************************'
    print, 'example: HeNe Laser '
    print, 'wavelength=633e-9, fwhm= 1e-3, sizez=1e-2'
    print, '**********************************************************'
    self->fermidiracbeam, wavelength=633e-9, fwhm=1e-3, sizez=1e-2, /plot
    print, '**********************************************************'
    print, 'end example'
    print, '**********************************************************'
    return
endif  ;; end example

if n_elements(Nz        ) eq 0 then Nz        = 243  ;; 3^5
if n_elements(Ny        ) eq 0 then Ny        = Nz  
if n_elements(wavelength) eq 0 then wavelength= 1e-10  
if n_elements(fwhm      ) eq 0 then fwhm      = 5e-4  
if n_elements(sizez     ) eq 0 then sizez     = 1e-3
if n_elements(sizey     ) eq 0 then sizey     = sizez
if n_elements(slope     ) eq 0 then slope     = 0.05* fwhm
if n_elements(z_off     ) eq 0 then z_off     = 0.0
if n_elements(y_off     ) eq 0 then y_off     = 0.0

wavelength = double(wavelength)
sizey      = double(sizey)
sizez      = double(sizez)
fwhm       = double(fwhm)

field  = dcomplexarr(Nz, Ny) 
z_vec  = (dindgen(Nz)/(Nz-1) - 0.5) * sizez 
y_vec  = (dindgen(Ny)/(Ny-1) - 0.5) * sizey

print, 'wavelength (m) = ', wavelength
print, 'Nz     = ', Nz      , ', Ny     = ', Ny
print, 'sizez (m) = ', sizez   , ', sizey (m) = ', sizey
print, 'z_off (m) = ', z_off   , ', y_off (m) = ', y_off
print, 'fwhm    (m) = ', fwhm      , ', slope  (m) = ', slope

for i=0, Nz-1 do begin
  for j=0, Ny-1 do begin
    r   =  sqrt((z_vec[i]-z_off)^2 + (y_vec[j]-y_off)^2)
    arg0= (r- 0.5 * fwhm)/ slope 
    arg1= 1.0/(1.0 + exp(arg0))
         
    field[i,j]= complex(arg1, 0.0, /double)
  endfor
endfor

;; plot using mycontour
if n_elements(plot) ne 0 then begin
  bamp = abs(field)
  window, 20
  stat = dblarr(7)
  fit   = gauss2dfit(bamp,    stat, z_vec, y_vec) 
  fit2  = gauss2dfit(bamp^2, stat2, z_vec, y_vec) 
  print, 'gaussfit amplitude: rms_z, rms_y (m)= ', stat(2),  stat(3)
  print, 'gaussfit intensity: rms_z, rms_y (m)= ', stat2(2), stat2(3)
  title= 'fermi-dirac beam intensity '+  'size='+  string(stat2(2)*1e6,FORMAT="(f6.1)")+ ' x ' + string(stat2(3)*1e6, FORMAT="(f6.1)") + textoidl(' \mum^2 rms')
  mycontour, bamp, z_vec*1e3, y_vec*1e3, xtitle='z (mm)', ytitle='y (mm)', title=title

  
endif ;; plot


self.wavelength= wavelength
self.field= ptr_new(field)
self.z_vec= ptr_new(z_vec)
self.y_vec= ptr_new(y_vec)

return 
end 
;; end fermidiracbeam

pro phase::fzp, ampmap=ampmap, f=f, d=d, phasemap=phasemap, y_off=y_off, z_off=z_off, _EXTRA=extra
;+
; NAME:
;   phase::fzp
;
; PURPOSE:
;   calculate the electric field after a Fresnel zone plate, includes a zero order stop, 
;   no material properties so far, the current version has just an amplitude map
;
; CATEGORY:
;   Phase
;
; CALLING SEQUENCE:
;   phase->fzp
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   ampmap: amplitude map
;   f: first order focal length in m
;   d: diameter in m
;   phasemap: export the phasemap as double array
;   y_off: y offset
;   z_off: z offset
;
; OUTPUTS:
;   no
;
; PROCEDURE:
;
; EXAMPLE:
;   idl> emf->fzp
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-
field= *self.field
y_vec= *self.y_vec
z_vec= *self.z_vec
wavelength= self.wavelength
if n_elements(z_off) eq 0 then z_off     = 0.0
if n_elements(y_off) eq 0 then y_off     = 0.0

d      = double(d) 
f      = double(f)
drn    = f* wavelength/d          ;; outermost zone width
n      = d/(4* drn)
res    = 1.22* drn
na     = 0.610* wavelength/ res   ;; spatial resolution
dof    = wavelength/(2.0*na*na)   ;; depth of field +/-
dlambda= wavelength/n             ;; otherwise chromatic blurring
r1     = sqrt(wavelength*(f+ wavelength/4.0)) ;; first edge


print, '** Fresnel zone plate **'
print, '========================'
print, 'focal length     f (m) =', f
print, 'diameter         D (m) =', d
print, 'wavelength         (m) =', wavelength
print, 'outerm. zone width (m) =', drn
print, 'inner zone rad. r1 (m) =', r1
print, 'number of zones  N     =', n
print, 'spatial resolution (m) =', res
print, 'numerical aperture     =', na
print, 'DOF +/-            (m) =', dof
print, 'dlambda must be    (m) <', dlambda
print, 'our grid dz        (m) =', z_vec[1]- z_vec[0]
print, 'our grid dy        (m) =', y_vec[1]- y_vec[0]
print, '========================'
nz= n_elements(z_vec)
ny= n_elements(y_vec)
fzpcomp= dcomplexarr(nz, ny) ;; make a complex array 
maxr= 0.5*d
for i=0, nz-1 do begin
    for j=0, ny-1 do begin
        fzpcomp[i,j]= complex(0.0, 0.0, /double)           ;; initialize with 0
        rr= sqrt((z_vec[i]- z_off)^2 + (y_vec[j]- y_off)^2);; the radial distance 
        if (rr gt r1) and (rr lt maxr) then begin          ;; zero order stop and apertur
            nr= fix(2.0/wavelength*(sqrt(f*f+ rr*rr)- f))  ;; calc n(r) (zone edge number)
            if (nr MOD 2) ne 0 then fzpcomp[i,j]= complex(1.0, 0.0, /double) ;; amplitude
            ;; the pathlength == n * wavelength/2 + f
            ;; the phase shift n/2 * 2pi
            ;; pshift= fix(nr) * !dpi
            ;; fzpcomp[i,j] = complex(cos(pshift), sin(pshift), /double) ;; computing time
            ;; if (N MOD 2) eq 0 then fzpcomp[i,j] = complex(-1.0, 0.0, /double) else fzpcomp[i,j] = complex(1.0, 0.0, /double)
        endif 
    endfor
endfor
*self.field= fzpcomp* field
if n_elements(phasemap) ne 0 then phasemap= atan(fzpcomp,/phase)
if n_elements(ampmap)   ne 0 then ampmap  = abs(fzpcomp)
return 
end
;; end fzp


pro phase::h5_read, fname, vertical=vertical, _EXTRA=extra
;+
; NAME:
;   phase::h5_read
;
; PURPOSE:
;   read hdf5 input 
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->h5_read, fname
;
; INPUTS:
;   filename
;
; OPTIONAL INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   vertical: use vertical polarization, default is horizontal
;
; EXAMPLE:
;   idl> emf->h5_read, 'input.h5'
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-

if n_elements(vertical) ne 0 then $
  h5_read, fname, ycomp=field, wavelength=wavelength, z_vec=z_vec, y_vec=y_vec, _EXTRA=extra $
else $
  h5_read, fname, zcomp=field, wavelength=wavelength, z_vec=z_vec, y_vec=y_vec, _EXTRA=extra 

self.wavelength= wavelength
self.field= ptr_new(field)
self.z_vec= ptr_new(z_vec)
self.y_vec= ptr_new(y_vec)

return
end ;; h5_read

pro phase::h5_write, fname, genesis=genesis, phase=phase, pha4idl=pha4idl, delta=delta, yscale=yscale, _EXTRA=extra
;+
; NAME:
;   phase::h5_write
;
; PURPOSE:
;   write hdf5 output - default is GENESIS format
;                       with multiple format switches 
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->h5_write, fname
;
; INPUTS:
;   filename
;
; OPTIONAL INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;    delta:    phase shift between Ez and Ey (phase format only), default= pi/6
;    /genesis: genesis format
;    /phase:   phase format
;    /pha4idl: pha4idl format
;    yscale:   scale of y amplitude (default= 0.9)
;
; EXAMPLE:
;   idl> emf->h5_write, 'output.h5'
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-
sgenesis= 0
sphase  = 0
spha4idl= 0
if (n_elements(phase) eq 0 and n_elements(pha4idl) eq 0) or n_elements(genesis) ne 0 then sgenesis=1
if (n_elements(phase) ne 0 ) then sphase= 1
if (n_elements(pha4idl) ne 0) then spha4idl= 1
if (n_elements(delta) eq 0)   then delta= 0.0
if (n_elements(yscale) eq 0)  then yscale= 1.0

if sgenesis gt 0 then $
  h5_write_genesis, fname, comp=*self.field, wavelength=self.wavelength, $
  z_vec=*self.z_vec, y_vec=*self.y_vec, _EXTRA=extra 

if sphase gt 0 then begin
    amp= abs(*self.field)
    pha= atan(*self.field, /phase)
    pha+= delta
    amp*= yscale
    ycomp= complex(amp*cos(pha), amp*sin(pha)) 
    
    print, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    print, '!! we save the field to zcomp AND ycomp !!'
    print, '!! use parameters delta and yscale      !!'
    print, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    print, 'current: delta=', delta, ', yscale=', yscale
    h5_write_phase, fname, zcomp=*self.field, ycomp=ycomp, wavelength=self.wavelength, $
      z_vec=*self.z_vec, y_vec=*self.y_vec, _EXTRA=extra 
endif

if spha4idl gt 0 then begin
    print, '!! save field to zcomp !!'
    h5_write_pha4idl, fname, zcomp=*self.field, wavelength=self.wavelength, $
      z_vec=*self.z_vec, y_vec=*self.y_vec, _EXTRA=extra 
endif
return
end ;; h5_write

pro phase::gaussbeam, dist=dist, drift=drift, w0=w0, Nz=Nz, Ny=Ny, sizez=sizez, sizey=sizey,  $
               wavelength=wavelength, plot=plot, example=example, $
               z_off=z_off, y_off=y_off
;+
; NAME:
;   phase::gaussbeam
;
; PURPOSE:
;   generate gaussian beam
;   currently only a 2dim gaussian distribution (in the waist), UF
;   (2b_confirmed) for the intensity distribution we have w=2*sigma
;   with sigma= sigma_x = sigma_y the variance of a 2d Gaussian and
;   sigma_r= sqrt(2) * sigma ???, the intensity is normalized to 1W
;
; CATEGORY:
;   Phase
;
; CALLING SEQUENCE:
;   phase->gaussbeam
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   example:      example calculation plus plot (HeNe laser in 10 m)
;   w0            waist                       in m
;   dist:         distance to waist           in m
;   wavelength    the wavelength              in m
;   Nz            points hor.
;   Ny            points vert, default = Nz
;   sizey:        height (m),  default = sizez
;   sizez:        width (m);
;
; OUTPUTS:
;   no
;
; PROCEDURE:
;   fills the object 
;
; EXAMPLE:
;   idl> emf->gaussbeam, dist=0, Nz=243, sizez=0.0002, w0=27.7e-6 , wavelength=1.24e-10
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;   based on gaussbeam.pro from RF
;-
self.name = 'Gaussbeam'

u1= 'usage: emf->gaussbeam, [dist=dist,][field=field,][w0=w0,][sizez=sizez,][sizey=sizey,][Nz=Nz,][Ny=Ny,]'
u2= '[wavelength=wavelength,] [y_vec=y_vec], [z_vec=z_vec], [plot=plot], [z_off=z_off], [y_off=y_off]'
usage= u1+u2

print, 'gaussbeam called'

IF KEYWORD_SET(EXAMPLE) THEN BEGIN
    print, '**********************************************************'
    print, 'example: HeNe Laser '
    print, 'wavelength=633e-9, w0= 1e-3, dist= 10., sizez=1e-2'
    print, '**********************************************************'
    self->gaussbeam, dist=10., wavelength=633e-9, w0=1e-3, sizez=1e-2, /plot
    print, '**********************************************************'
    print, 'end example'
    print, '**********************************************************'
    return
endif  ;; end example

if n_elements(drift     ) ne 0 then dist      = drift
if n_elements(Nz        ) eq 0 then Nz        = 243  ;; 3^5
if n_elements(Ny        ) eq 0 then Ny        = Nz  
if n_elements(wavelength) eq 0 then wavelength= 1e-10  
if n_elements(w0        ) eq 0 then w0        = 1e-5  
if n_elements(sizez     ) eq 0 then sizez     = 1e-3
if n_elements(sizey     ) eq 0 then sizey     = sizez
if n_elements(dist      ) eq 0 then dist      = 0.0
if n_elements(bcomp     ) ne 0 then begin & print, 'obsolete keyword: bcomp- use keyword: field intead!' & return & endif
if n_elements(z_off     ) eq 0 then z_off     = 0.0
if n_elements(y_off     ) eq 0 then y_off     = 0.0

dist       = double(dist)
wavelength = double(wavelength)
sizey      = double(sizey)
sizez      = double(sizez)
w0         = double(w0)

field  = dcomplexarr(Nz, Ny) 
z_vec  = (dindgen(Nz)/(Nz-1) - 0.5) * sizez 
y_vec  = (dindgen(Ny)/(Ny-1) - 0.5) * sizey

print, 'wavelength (m) = ', wavelength
print, 'Nz     = ', Nz      , ', Ny     = ', Ny
print, 'sizez (m) = ', sizez   , ', sizey (m) = ', sizey
print, 'z_off (m) = ', z_off   , ', y_off (m) = ', y_off
print, 'w0    (m) = ', w0      , ', dist  (m) = ', dist

k   = !dpi * 2    / wavelength     ;; wave number
z0  = !dpi * w0^2 / wavelength     ;; Rayleigh Range
w   = w0 * sqrt(1d0+ (dist/z0)^2)  ;; w(dist)
w2  = w^2
eta = atan(dist/z0)
Ri  = dist / (dist^2 + z0^2)       ;; curvature Ri  = 1/R;

print, 'z0    (m) = ', z0, ' (Rayleigh Range= +/- z0)'
print, 'w     (m) = ', w   ,', w2 (m^2) = ', w2
print, 'eta (rad) = ', eta ,', Ri (1/m) = ', Ri

truncation= 0 
for i=0, Nz-1 do begin
  for j=0, Ny-1 do begin
    rho2  =  (z_vec[i]-z_off)^2 + (y_vec[j]-y_off)^2 
    arg1  = -1 *  rho2 / w2               ;; the intensity factor as function of aperture
    if (arg1 le -40) then begin 
        arg1 = -40                        ;;  -40, but -80 is still ok
        truncation= 1
    endif
    arg2  = 0.5 * k * rho2 * Ri + k*dist - eta                    ;; For notation of Siegman multiply by -1                    
    phas2 = complex(cos(arg2), sin(arg2), /double)     
    field[i,j]= phas2 * exp(arg1) * w0 / w
  endfor
endfor

;; norm to 0.5 W  !! we assume only one polarization 
intensity = abs(field)^2/377.0
binsize= (z_vec[1]-z_vec[0])*(y_vec[1]-y_vec[0])
itot= total(intensity)*binsize*2.0
scale= 1.0/sqrt(itot)
field*= scale

if truncation gt 0 then print, '!! warning -- some outside points are truncated !!'

;; plot using mycontour
if n_elements(plot) ne 0 then begin
  bamp = abs(field)
  window, 20
  stat = dblarr(7)
  fit   = gauss2dfit(bamp,    stat, z_vec, y_vec) 
  fit2  = gauss2dfit(bamp^2, stat2, z_vec, y_vec) 
  print, 'gaussfit amplitude: rms_z, rms_y (m)= ', stat(2),  stat(3)
  print, 'gaussfit intensity: rms_z, rms_y (m)= ', stat2(2), stat2(3)
  title= 'gaussbeam intensity '+  'size='+  string(stat2(2)*1e6,FORMAT="(f6.1)")+ ' x ' + string(stat2(3)*1e6, FORMAT="(f6.1)") + textoidl(' \mum^2 rms')
  mycontour, bamp, z_vec*1e3, y_vec*1e3, xtitle='z (mm)', ytitle='y (mm)', title=title

  pha = atan(field, /phase)
  if max(pha)- min(pha) gt 1e-10 then begin
      window,21
      mycontour, pha, z_vec*1e3, y_vec*1e3, xtitle='z (mm)', ytitle='y (mm)', title='gaussbeam phase'
  endif else begin
      print, 'phase(z,y) is zero- no phase plot'
      device,window_state=window_list
      if window_list[21] gt 0 then wdelete, 21
  endelse
endif ;; plot


;;gaussbeam, emf, _EXTRA=extra
self.wavelength= wavelength
self.field= ptr_new(field)
self.z_vec= ptr_new(z_vec)
self.y_vec= ptr_new(y_vec)
return 
end
;; end gaussbeam

function phase::getamplitude
;+
; NAME:
;   phase::getamplitude
;
; PURPOSE:
;   export amplitude
;
;; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   y= getamplitude()  
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   no
;
; OUTPUTS:
;   the amplitude
;
; EXAMPLE:
;  idl> amplitude= emf->getamplitude()
;
; MODIFICATION HISTORY:
;   UF 4.11.13
;-
amp= abs(*self.field)

return, amp
end ;; amplitude

function phase::getemf
;+
; NAME:
;   phase::getemf
;
; PURPOSE:
;   export emf struct
;
;; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   y= getemf()  
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   no
;
; OUTPUTS:
;   the emf struct
;
; EXAMPLE:
;  idl> field= emf->getemf()
;
; MODIFICATION HISTORY:
;   UF 4.11.13
;-
emf= emfield(field=*self.field, y_vec=*self.y_vec, z_vec=*self.z_vec, wavelength=self.wavelength)
return, emf
end ;; emf

function phase::getfield
;+
; NAME:
;   phase::getfield
;
; PURPOSE:
;   export field
;
;; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   y= getfield()  
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   no
;
; OUTPUTS:
;   the complex field
;
; EXAMPLE:
;  idl> field= emf->getfield()
;
; MODIFICATION HISTORY:
;   UF 4.11.13
;-
field= *self.field

return, field
end ;; field

function phase::getintensity
;+
; NAME:
;   phase::getintensity
;
; PURPOSE:
;   export intensity, the squared field divided by the vacuum impedance
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   y= getintensity()  
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   no
;
; OUTPUTS:
;   the intensity
;
; EXAMPLE:
;  idl> int= getyintensity()
;
; MODIFICATION HISTORY:
;   UF 4.11.13
;-
intensity= abs(*self.field)^2/377.0

return, intensity
end ;; intensity

function phase::getname
;+
; NAME:
;   phase::getname
;
; PURPOSE:
;   export name
;
;; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   y= getname()  
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   no
;
; OUTPUTS:
;   the name
;
; EXAMPLE:
;  idl> name= emf->getname()
;
; MODIFICATION HISTORY:
;   UF 4.11.13
;-
name= self.name

return, name
end ;; name

function phase::getphase, phunwrap=phunwrap, unwrap_phase=unwrap_phase, raw=raw, _EXTRA=extra
;+
; NAME:
;   phase::getphase
;
; PURPOSE:
;   export phase, optional with unwrapping
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   phase= phase::getphase([/phunwrap][/unwrap_phase])
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   /phunwrap    : unwrap using phunwrap.pro
;   /raw         : get raw phase (default)
;   /unwrap_phase: unwrap using unwrap_phase.pro
;
; OUTPUTS:
;   the phase
;
; EXAMPLE:
;   idl> phase= emf->phunwrap()
;
;; MODIFICATION HISTORY:
;   UF 4.11.13
;-

if keyword_set(raw) then begin
    phi= atan(*self.field, /phase)
    print, 'get raw phase'
    help, phi
    return, phi
endif

if keyword_set(phunwrap) then begin
    print, 'call myphunwrap'
    phi0= self->getphase(/raw)
    phi= phi0*1d0
    phi= myphunwrap(phi0)
    help, phi
    return, phi
endif

if keyword_set(unwrap_phase) then begin
    print, 'call myunwrap_phase'
    phi0= self->getphase(/raw)
    phi= myunwrap_phase(phi0)
    return, phi
endif

print, 'getphase default called' 
phi= atan(*self.field, /phase) 
help, phi
return, phi
end ;; getphase

function phase::getphotons
;+
; NAME:
;   phase::getphotons
;
; PURPOSE:
;   export intensity as photons
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   y= getphotons()  
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   no
;
; OUTPUTS:
;   the photons
;
; EXAMPLE:
;  idl> int= getphotons()
;
; MODIFICATION HISTORY:
;   UF 19.09.14
;-
photons= abs(*self.field)^2/377.0/1.6e-19

return, photons
end ;; photons


function phase::getprofile, amplitude=amplitude, min=min, phase=phase, z=z
;+
; NAME:
;   phase::getprofile
;
; PURPOSE:
;   get the profile (cut) at the maximum or minimum for intensity, amplitude or phase
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->getprofile()
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;     /amplitude: get amplitude profile  (default: intensity)
;     /min      : profile at minimum     (default: maximum)
;     /phase    : get phase profile      (default: intensity)
;     /z        : the horizontal profile (default: vertical)
;
; OUTPUTS:
;   the profile
;
; EXAMPLE:
;   idl> zprof= emf->getprofile(/z)
;
;; MODIFICATION HISTORY:
;   UF 4.12.13
;-
if n_elements(amplitude) ne 0 then field = self->getamplitude()
if n_elements(phase)     ne 0 then field = self->getphase(/unwrap)
if (n_elements(amplitude) eq 0) and  (n_elements(phase) eq 0) then field= self->getintensity() 

help,field
s= size(field)
nz= s[1]
ny= s[2]
if n_elements(min) ne 0 then m= min(field, mindex) else m= max(field, mindex)
mz= mindex mod ny 
my= mindex / ny

if (mz ge nz) then begin
    mz= nz/2
    print, 'mz out of range: force profile at center'
endif

if (my ge ny) then begin
    my= ny/2
    print, 'my out of range: force profile at center'
endif

print, 'getprofile: nz, ny, mz, my, m, mindex:', nz, ny, mz, my, m, mindex

if n_elements(z) ne 0 then prof= reform(field[*,my]) else prof= reform(field[mz,*])

return, prof
end ;; getprofile

function phase::getwavelength
;+
; NAME:
;   phase::getwavelength
;
; PURPOSE:
;   export wavelength
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   y= getwavelength()  
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   no
;
; OUTPUTS:
;   the wavelength
;
; EXAMPLE:
;  idl> wavelength= emf->getwavelength()
;
; MODIFICATION HISTORY:
;   UF 4.11.13
;-
wl= self.wavelength

return, wl
end ;; wavelength

function phase::gety_vec
;+
; NAME:
;   phase::gety_vec
;
; PURPOSE:
;   export vertical vector
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   y= phase::gety_vec() 
;
; INPUTS:
;   no
;
; OUTPUTS:
;   the vertical vector
;
; EXAMPLE:
;  idl> y= emf->gety_vec()
;
; MODIFICATION HISTORY:
;   UF 4.11.13
;-
y= *self.y_vec
return, y
end ;; y_vec

function phase::getz_vec
;+
; NAME:
;   phase::getz_vec
;
; PURPOSE:
;   export horizontal vector
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   z= getz_vec() 
;
; INPUTS:
;   no
;
; OUTPUTS:
;   the hor. vector
;
; EXAMPLE:
;  idl> z= emf->getz_vec()
;
; MODIFICATION HISTORY:
;   UF 4.11.13
;-
z= *self.z_vec
return, z
end ;; z

pro phase::lens, fz=fz, fy=fy
;+
; NAME:
;   phase::lens
;
; PURPOSE:
;   calculate the electric field after a thin lens
;
; CATEGORY:
;   Phase
;
; CALLING SEQUENCE:
;   phase->lens
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   fy: vertical focal length
;   fz: horizontal focal length
; 
; OUTPUTS:
;   no
;
; PROCEDURE:
;
; EXAMPLE:
;   idl> emf->lens
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-
if n_elements(fy) eq 0 then fy= 1d200
if n_elements(fz) eq 0 then fz= 1d200
print, 'thin lens with focal length (fy, fz): ', fy, fz

myz_vec= *self.z_vec
myy_vec= *self.y_vec
nz= n_elements(myz_vec)
ny= n_elements(myy_vec)

;; do not knwo why we need local vars
help, myz_vec, myy_vec, *self.z_vec, *self.y_vec

;nz= n_elements(*self.z_vec)
;ny= n_elements(*self.y_vec)

print, 'ny, nz, lambda ', ny, nz, self.wavelength

lcomp = dcomplexarr(nz, ny) ;; make a complex array

for i=0, nz-1 do begin
    for j=0, ny-1 do begin
        f1= myz_vec[i]^2/(2.0*fz) + myy_vec[j]^2/(2.0*fy) 
        f1 *= (-2.0)* !dpi/ self.wavelength
        lcomp[i,j] = complex(cos(f1), sin(f1), /double)
    endfor
endfor
*self.field*= lcomp
return 
end
;; end lens

pro phase::mirror, u=u, rl=rl, rw=rw, thetag=thetag, azimut=azimut, w=w, l=l
;+
; NAME:
;   phase::mirror
;
; PURPOSE:
;   calculate the electric field after a "thin" toroidal mirror with/without 1d or 2d height errors
;
; CATEGORY:
;   Phase
;
; CALLING SEQUENCE:
;   phase->mirror
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   azimut: azimut angle or Rx in rad, math. positive, 0 means vertical deflecting 
;   l     : the mirror coordinate
;   rl    : short radius
;   rw    : long radius
;   thetag: grazing angle in rad  
;   u     : the height error of the mirror as a vector of the mirror coordinate w or matrix u(w,l) 
;   w     : the mirror coordinate
; 
; OUTPUTS:
;   no
;
; PROCEDURE:
;
; EXAMPLE:
;   idl> emf->mirror, thetag=thetag, rw=rw, rl=rl, u=u2, w=w, l=l
;
; MODIFICATION HISTORY:
;   UF Jun 2014
;-
if n_elements(rw)     eq 0 then rw= 0.0
if n_elements(rl)     eq 0 then rl= 0.0
if n_elements(azimut) eq 0 then azimut= 0.0 else print, 'azimut not yet implemented!'
if n_elements(thetag) eq 0 then thetag= !dpi/2.0

if (abs(rw)- 1d-200)   lt 0.0 then rw= 1d200
if (abs(rl)- 1d-200)   lt 0.0 then rl= 1d200
if (abs(thetag)- 1e-9) lt 0.0 then thetag= 1e-9

fl= rl/(2.0* sin(thetag))  ;; brennweiten
fw= rw* sin(thetag)/ 2.0

print, 'mirror with radius (rw,rl): ', rw, rl
print, 'mirror with focus  (fw,fl): ', fw, fl

myz_vec= *self.z_vec
myy_vec= *self.y_vec
nz= n_elements(myz_vec)
ny= n_elements(myy_vec)

lcomp = dcomplexarr(nz, ny) ;; make a complex array

case azimut of
  0 : begin
      print, 'vertical up deflecting mirror'
  end
  90 : begin
      print, 'horizontal left deflecting mirror'
      ff= fw
      fw= fl
      fl= ff
  end
  180 : begin
      print, 'vertical down deflecting mirror'
  end
  270 : begin
      print, 'horzontal right deflecting mirror'
      ff= fw
      fw= fl
      fl= ff
  end
  else : begin
    print, 'no valid azimut= ', azimut,', allowed are: 0 90 180 270'
    return
  end
endcase

;; the phase shift of a lens
for i=0, nz-1 do begin
  for j=0, ny-1 do begin
    f1 = myz_vec[i]^2/(2.0*fl) + myy_vec[j]^2/(2.0*fw) ;; phase   
    f1*= (-2)* !dpi/ self.wavelength                   ;; k
    lcomp[i,j]= complex(cos(f1), sin(f1), /double)
  endfor
endfor

*self.field*= lcomp   ;; factor

;; deal with error
if n_elements(u) ne 0 then begin
    print, 'mirror with height error'
    if n_elements(w) eq 0 then message, 'we need w as the mirror coordinate' && return
    if (size(u, /n_dimensions) eq 2) and (n_elements(l) eq 0) then message, 'we need l as the mirror coordinate' && return
 
    if (size(u, /n_dimensions) eq 1) then begin  ;; 1d
        print, 'deal with 1d height error'
        hw1= u* sin(thetag) ;; the phase shift relevant height as function of w
        w1 = w* sin(thetag) ;; the projection of the mirror to normal incidence
        hw2= interpol(hw1, w1, myy_vec) ;; hw2 as function of myy_vec
        for i=0, nz-1 do begin
            for j=0, ny-1 do begin
                f1= hw2[j]    
                f1*= (-4)* !dpi/ self.wavelength
                lcomp[i,j] = complex(cos(f1), sin(f1), /double)
            endfor
        endfor
    endif else begin ;; 2d
        print, 'deal with 2d height error- not yet debugged'
        hw1= u* sin(thetag) ;; the phase shift relevant height as function of w
        w1 = w* sin(thetag) ;; the projection of the mirror to normal incidence
        hw2= interp2d(hw1, w1, l, myy_vec, myz_vec, /grid)  
        for i=0, nz-1 do begin
            for j=0, ny-1 do begin
                f1= hw2[j]    
                f1*= (-4)* !dpi/ self.wavelength
                lcomp[i,j] = complex(cos(f1), sin(f1), /double)
            endfor
        endfor
    endelse
    
    *self.field*= lcomp

endif ;; end error

return 
end
;; end mirror

pro phase::mirrorg, u=u, w=w, l=l, thetag=thetag, azimut=azimut 
;+
; NAME:
;   phase::mirrorg
;
; PURPOSE:
;   calculate the electric field after a generic mirror defined by a height profile u(w,l)
;
; CATEGORY:
;   Phase
;
; CALLING SEQUENCE:
;   phase->mirrorg
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   azimut: azimut angle or Rx in rad, math. positive, 0 means vertical deflecting 
;   hw:     the height error of the mirror as a vector of the mirror coordinate w
;   rl:     short radius
;   rw:     long radius
;   thetag: grazing angle in rad   
;   w     : the mirror coordinate
; 
; OUTPUTS:
;   no
;
; PROCEDURE:
;
; EXAMPLE:
;   idl> emf->mirrorg
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-

if n_elements(azimut) eq 0 then azimut= 0.0 else print, 'azimut not yet implemented!'
if n_elements(thetag) eq 0 then thetag= !dpi/2.0
if (abs(thetag)- 1e-9) lt 0.0 then thetag= 1e-9

myz_vec= *self.z_vec
myy_vec= *self.y_vec
nz= n_elements(myz_vec)
ny= n_elements(myy_vec)

lcomp = dcomplexarr(nz, ny) ;; make a complex array

case azimut of
  0 : begin
      print, 'vertical up deflecting mirror'
      wm= myy_vec/ sin(thetag)
      lm= myz_vec
  end
  90 : begin
      print, 'horizontal left deflecting mirror'
      wm= (-1.0) * myz_vec /sin(thetag)
      lm= myy_vec
  end
  180 : begin
      print, 'vertical down deflecting mirror'
      wm= (-1.0) * myy_vec/ sin(thetag)
      lm= (-1.0) * myz_vec
  end
  270 : begin
      print, 'horzontal right deflecting mirror'
      wm= myz_vec /sin(thetag)
      lm= (-1.0) * myy_vec
  end
  else : begin
    print, 'no valid azimut= ', azimut,', allowed are: 0 90 180 270'
    return
end
endcase

; we calculated the coordinate wm, lm on the mirror as function of y_vec, z_vec, azimut and thetag 
; now we calculate the 2d height matrix um
print , 'call interp2d'
um= interp2d(u, w, l, wm, lm, /grid)  
print , 'mirrorg start loop'
for i=0, nz-1 do begin
  for j=0, ny-1 do begin
    f1 = um[i,j] * sin(thetag)    ;; 2d interpolator
    f1*= 2.0* (-2)* !dpi/ self.wavelength                   ;; k
    lcomp[i,j]= complex(cos(f1), sin(f1), /double)
  endfor
endfor

*self.field*= lcomp   ;; factor

return 
end
;; end mirrorg

pro phase::mirrorp, thetag=thetag, azimut=azimut 
;+
; NAME:
;   phase::mirrorp
;
; PURPOSE:
;   calculate the electric field after a "thin" flat mirror defined as phase shifter, 
;   this simple approach does only work in limitd cases where the phase shift is only 
;   a few k (long wavelength and/or small angle), it must fail when the relative phase
;   shift between points approaches 2 pi
;
; CATEGORY:
;   Phase
;
; CALLING SEQUENCE:
;   phase->mirrorp
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   azimut: azimut angle or Rx in rad, math. positive, 0 means vertical deflecting 
;   thetag: grazing angle in rad   
;   w     : the mirror coordinate
; 
; OUTPUTS:
;   no
;
; PROCEDURE:
;
; EXAMPLE:
;   idl> emf->mirrorp, thetag=1e-6
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-

print, 'mirrorp caleed with thetag= ', thetag
if n_elements(azimut) eq 0 then azimut= 0.0 else print, 'azimut not yet implemented!'
if n_elements(thetag) eq 0 then thetag= !dpi/2.0
if (abs(thetag)- 1e-9) lt 0.0 then thetag= 1e-9

myz_vec= *self.z_vec
myy_vec= *self.y_vec
nz= n_elements(myz_vec)
ny= n_elements(myy_vec)
k = 2.0*!dpi/ self.wavelength

lcomp = dcomplexarr(nz, ny) ;; make a complex array

for i=0, nz-1 do begin
  for j=0, ny-1 do begin
      f1= -2.0* k * thetag * myy_vec[j]
      lcomp[i,j]= complex(cos(f1), sin(f1), /double)
  endfor
endfor

*self.field*= lcomp   ;; factor

return 
end
;; end mirrorp

pro phase::mirrort, thetag=thetag, azimut=azimut 
;+
; NAME:
;   phase::mirrort
;
; PURPOSE:
;   calculate the electric field after a "thin" flat mirror defined as phase shifter, 
;   the method uses the transfer function approach first fft to angular spectrum, 
;   second apply angular shift, third fft back to spatial distribution. The granularity 
;   of the shiftare frequency steps   
;
; CATEGORY:
;   Phase
;
; CALLING SEQUENCE:
;   phase->mirrorp
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   azimut: azimut angle or Rx in rad, math. positive, 0 means vertical deflecting 
;   thetag: grazing angle in rad   
;   w     : the mirror coordinate
; 
; OUTPUTS:
;   no
;
; PROCEDURE:
;
; EXAMPLE:
;   idl> emf->mirrort
;
; MODIFICATION HISTORY:
;   UF Feb 2014
;-

if n_elements(azimut) eq 0 then azimut= 0.0 else print, 'azimut not yet implemented!'
if n_elements(thetag) eq 0 then thetag= !dpi/2.0
if (abs(thetag)- 1e-9) lt 0.0 then thetag= 1e-9

myz_vec= *self.z_vec
myy_vec= *self.y_vec
myfield= *self.field
nz= n_elements(myz_vec)
ny= n_elements(myy_vec)
k = 2.0*!dpi/ self.wavelength
twopi= 2.0 * !dpi
zz  = myz_vec[Nz-1]- myz_vec[0]                                    ;; total width
yy  = myy_vec[Ny-1]- myy_vec[0]                                    ;; total width
u = (dindgen(Nz)/(Nz-1) - 0.5)                ;; runs from -0.5..0.. 0.5 
v = (dindgen(Ny)/(Ny-1) - 0.5)                ;; for even and odd values of Ny, Nz 
u = u * (Nz-1)/zz                             ;; ok with odd number of elements
v = v * (Ny-1)/yy

E0ft = fft(myfield, -1, /center, /double)     ;; remember: the frequencies are the 
                                              ;; direction cosines divided by lambda

fy= sin(2.*thetag)/ self.wavelength
szi= 0 ;;;30
idx= max(where(u lt fy))
center_idx= Ny/2
;; syi has to be determined from thetag- how???

syi= idx-center_idx ;;8;;fix(sin(thetag)/ self.wavelength)

print, '******************* syi= ', syi, ny/2.0
;;print, u

sarr= shift(e0ft,szi,syi)

myf1= fft(sarr, 1, /center, /double)     

*self.field= myf1   ;; factor
print, 'mirrort not yet debugged!'
return 
end
;; end mirrort

pro phase::phaseplate, arr
;+
; NAME:
;   phase::phaseplate
;
; PURPOSE:
;   multiplys the field by a matrix, the dimensions of the matrix must be the same as the field  
;
; CATEGORY:
;   Phase
;
; CALLING SEQUENCE:
;   phase->phaseplate
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;   no
;
; PROCEDURE:
;
; EXAMPLE:
;   idl> emf->phaseplate, arr
;
; MODIFICATION HISTORY:
;   UF Jun 2014
;-

print, 'phaseplate called'

f= self->getfield()
sizef= size(f,   /dimensions)
sizea= size(arr, /dimensions)

if (sizef[0] ne sizea[0]) or (sizef[1] ne sizea[1]) then begin
    print, 'phaseplate error: size mismatch '
    print, 'phaseplate return'
    return
endif

f*= arr
self->setfield, f

return 
end
;; end phaseplate

pro phase::plotamplitude, window=window, _EXTRA=extra
;+
; NAME:
;   phase::plotamplitude
;
; PURPOSE:
;   plot amplitude
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->plotamplitude
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   
; EXAMPLE:
;   idl> emf->plotamplitude
;
; MODIFICATION HISTORY:
;   UF Dec 2013
;-

if n_elements(window) ne 0 then window, window

title= self.name+ ' amplitude'
mycontour, abs(*self.field), *self.z_vec*1e3, *self.y_vec*1e3, title=title, $
  xtitle='z (mm)', ytitle='y (mm)', ztitle='amplitude (V/m)', _EXTRA=extra
return 
end
; end plotamplitude

pro phase::plotintensity, window=window, _EXTRA=extra
;+
; NAME:
;   phase::plotintensity
;
; PURPOSE:
;   plot intensity
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->plotintensity
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   
; EXAMPLE:
;   idl> emf->plotintensity
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-
if n_elements(window) ne 0 then window, window
title= self.name+ ' intensity'
a= self->getintensity()
mycontour, a, *self.z_vec*1e3, *self.y_vec*1e3, title=title, $
  xtitle='z (mm)', ytitle='y (mm)', ztitle='intensity (W/m^2)', _EXTRA=extra 
return 
end
; end plotintensity

pro phase::plotphase, window=window, _EXTRA=extra
;+
; NAME:
;   phase::plotphase
;
; PURPOSE:
;   plot the phase
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->plotphase
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;
; EXAMPLE:
;   idl> emf->plotphase
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-
if n_elements(window) ne 0 then window, window
title= self.name+ ' phase'
myphase= self->getphase(_EXTRA=extra)
mycontour, myphase, *self.z_vec*1e3, *self.y_vec*1e3, title=title, $
  xtitle='z (mm)', ytitle='y (mm)', ztitle='phase', _EXTRA=extra
return 
end ;; plotphase

pro phase::plotphotons, window=window, _EXTRA=extra
;+
; NAME:
;   phase::plotphotons
;
; PURPOSE:
;   plot photons
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->plotphotons
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   
; EXAMPLE:
;   idl> emf->plotphotons
;
; MODIFICATION HISTORY:
;   UF Sep 2014
;-
if n_elements(window) ne 0 then window, window
title= self.name+ ' photon intensity'
a= self->getphotons()
mycontour, a, *self.z_vec*1e3, *self.y_vec*1e3, title=title, $
  xtitle='z (mm)', ytitle='y (mm)', ztitle='intensity (photons/m^2)', _EXTRA=extra 
return 
end
; end plotphotons

pro phase::plotprofile, window=window, ylog=ylog, _EXTRA=extra
;+
; NAME:
;   phase::plotprofile
;
; PURPOSE:
;   plot the profiles in z and y
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->plotprofile
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   _EXTRA: passed to getprofile
;   ylog  : ylog
;
; EXAMPLE:
;   idl> emf->plotprofile
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-

if n_elements(window) ne 0 then window, window

title= self.name+ ' profile'
zp= self->getprofile(/z, _EXTRA=extra)
yp= self->getprofile(_EXTRA=extra)
y = self->gety_vec()
z = self->getz_vec()

miy= min(y)
miz= min(z)
miyp= min(yp)
mizp= min(zp)
may= max(y)
maz= max(z)
mayp= max(yp)
mazp= max(zp)

miyz = min(miy, miz)* 1e3
mayz = max(may, maz)* 1e3
miyzp= min(miyp,mizp)
mayzp= max(mayp,mazp)

if n_elements(ylog) ne 0 then begin
    mayzp*= 1.5
    miyzp= mayzp* 1e-3
endif 

plot, [miyz, mayz], [miyzp, mayzp], title=title, $
  xtitle='[z,y] (mm)', ytitle='intensity etc.', ylog=ylog, /nodata, _EXTRA=extra
oplot, z*1e3, zp, color=1
oplot, y*1e3, yp, color=2
legend, ['z','y'], color=[1,2], linestyle=[0,0], /right
return 
end ;; plotprofile

pro phase::propagate, drift=drift
;+
; NAME:
;   phase::propagate
;
; PURPOSE:
;   propagate with fresnel or fourier propagator depending on sampling
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->propagate
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   drift     : drift distance in m
;
; OUTPUTS:
;   no
;
; PROCEDURE:
;
; EXAMPLE:
;  idl> emf->propagate, drift=drift
;
; MODIFICATION HISTORY:
;   UF Jun 2014
;-
print,'propagate called- automatic selection of propagator '
self->check_sampling, drift=drift, ratio=ratio, verbose=0

if (ratio gt 1.0) then self->propfourier, drift=drift else self->propfresnel, drift=drift 

return 
end ;;propagate  

pro phase::propfraunhofer, _EXTRA=extra
;+
; NAME:
;   phase::propfraunhofer
;
; PURPOSE:
;   propagate field using the fraunhofer approximation (1 FFT)
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->propfraunhofer
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;   no
;
; PROCEDURE:
;
; EXAMPLE:
;  idl> emf->propfraunhofer
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-

propfraunhofer, field=*self.field, y_vec=*self.y_vec, z_vec=*self.z_vec, $
  wavelength=self.wavelength, _EXTRA=extra



return 
end ;;propfraunhofer  

pro phase::propfresnel, _EXTRA=extra
;+
; NAME:
;   phase::propfresnel
;
; PURPOSE:
;   propagate field using the fresnel propagator (it does one FFTs)
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->propfresnel
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;   no
;
; PROCEDURE:
;
; EXAMPLE:
;  idl> emf->propfresnel
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-

propfresnel, field=*self.field, y_vec=*self.y_vec, z_vec=*self.z_vec, $
  wavelength=self.wavelength, _EXTRA=extra

return
end ;; propfresnel

pro phase::propfourier, _EXTRA=extra
;+
; NAME:
;   phase::propfourier
;
; PURPOSE:
;   propagate field using the fourier propagator (it does two FFTs)
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->propfourier
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   drift: propagation distance in m   
;
; OUTPUTS:
;   no
;
; PROCEDURE:
;
; EXAMPLE:
;  idl> emf->propfourier
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-

emf=emfield(field=*self.field, y_vec=*self.y_vec, z_vec=*self.z_vec, wavelength=self.wavelength)

propfourier, emf, _EXTRA=extra

*self.field= emf.field
*self.z_vec= emf.z_vec
*self.y_vec= emf.y_vec
return
end ;; propfourier

pro phase::resize, newsize=newsize, nocenter=nocenter, newy_vec=newy_vec, newz_vec=newz_vec, interpolate=interpolate
;+
; NAME:
;   phase::resize
;
; PURPOSE:
;   resize the field and grid we assume an equidistant quadratic grid. The default is zero padding 
;   to newsize and keep the spatial resolution. Interpolate keeps the area and interpolates to newsize. 
;   Providing vectors forces the field to be interpolated and zero padded. The routine modifies the 
;   vectors and fields.
;   The method functions and calling parameters are different from resize.pro!
;
;   hint for fast idl fft: N should be built from prime factors 2,3,5,
;   to avoid special treatment of the Nyquist frequency odd N
;   are recommended good examples:
;   3,    9,  27,  81,  243, 729, 2187 
;   5,   25, 125, 625, 3125
;   15,  75, 375, 1875
;   45, 135, 405, 1215
;   225
;   if N= 3^k3 + 5^k5 then time = a*(3*k3+5*k5) i.e. 243=>15, 225=>16
;   i.e. 243 is faster than 225
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->resize
;
; INPUTS:
;   no
;
; OPTIONAL INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   interpolate: inerpolate, default is zero padding
;   newsize:     new quadratic gridzize 
;   nocenter:    zeropadding without centering 
;   newy_vec:       new y_vector
;   newz_vec:       new z_vector
;
; EXAMPLE:
;   idl> emf->resize
;
; MODIFICATION HISTORY:
;   27.8.2013 RF 
;   UF Feb 2014 
;-

;;if n_elements(ne)  eq 0 then begin

z_vec= self->getz_vec()
y_vec= self->gety_vec()
field= self->getfield()
size = size(field)
if (size[1] ge size[2]) then sizeField = size[1] else sizeField = size[2]

if n_elements(newsize) ne 0 then begin
    if newsize le sizeField  then begin
        print, 'error: newsize < oldsize'
        return
    endif
endif

if n_elements(newy_vec) ne 0 then begin
    print, 'error: vector input not yet supported'
    return
endif

if n_elements(newz_vec) ne 0 then begin
    print, 'error: vector input not yet supported'
    return
endif

if n_elements(interpolate) eq 0 then begin  ;; zero padding
    print, '----------------- Zero padding of field ------'
    print, 'old= ',sizeField,' new=', newsize
    Null       = dcomplexarr(newsize, newsize)  ;; make a quadratic array filled with 0
    s          = size(field, /DIM)  ;; s gives the dimensions of field
    zshift     = (newsize-s[0])/2     ;; the index to shift 
    yshift     = (newsize-s[1])/2     ;; the index to shift
    Null[0,0]  = field              ;; copy original field  
    if n_elements(nocenter) eq 0 then field= shift(Null, zshift, yshift) else field= Null ;shift the field to the center
;; now the vectors
    z0 = z_vec[0]                           ;; the start value
    y0 = y_vec[0]                           ;; the start value
    dz = z_vec[1] - z0                      ;; stepsize
    dy = y_vec[1] - y0                      ;; stepsize
    if n_elements(nocenter) eq 0 then begin ;; center
            z_vec= (dindgen(newsize)- zshift) * dz + z0
            y_vec= (dindgen(newsize)- yshift) * dy + y0 
        endif else begin
            z_vec= dindgen(newsize) * dz + z0
            y_vec= dindgen(newsize) * dy + y0
        endelse

endif else begin   ;; end zero padding - begin interpolate
    print, '-----------------interpolate-----------'
    print, 'old= ',sizeField,' new=', newsize
    field= interpolate(field, z_vec, y_vec, /GRID)
    z0 = z_vec[0]                           ;; the start value
    y0 = y_vec[0]                           ;; the start value
    dz = z_vec[1] - z0                      ;; stepsize
    dy = y_vec[1] - y0                      ;; stepsize
    z_vec= dindgen(newsize) * dz + z0
    y_vec= dindgen(newsize) * dy + y0
endelse ;; end interpolate

ptr_free, self.field, self.z_vec, self.y_vec   ;; clean up pointers
self.field= ptr_new(field)
self.z_vec= ptr_new(z_vec)
self.y_vec= ptr_new(y_vec)

return
end ;; resize

pro phase::setField, field
;+
; NAME:
;   phase::setField
;
; PURPOSE:
;   set the Field in the datastructure
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->setfield, field 
;
; INPUTS:
;   the field as as DCOMPLEX array
;
; OPTIONAL INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   no
;
; EXAMPLE:
;   idl> 
;   idl> emf->setfield, field
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-
self.field= ptr_new(field)
return
end ;; setfield

pro phase::setName, title
;+
; NAME:
;   phase::setName
;
; PURPOSE:
;   set the name in the datastructure
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->setname, name
;
; INPUTS:
;   the name as string
;
; OPTIONAL INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   no
;
; EXAMPLE:
;   idl> emf->setname, 'new title'
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-
self.name= title
return
end ;; setname

pro phase::setWavelength, lambda
;+
; NAME:
;   phase::setWavelength
;
; PURPOSE:
;   set the wavelength in the datastructure
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->setwavelength, name
;
; INPUTS:
;   the wavelength as double
;
; OPTIONAL INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   no
;
; EXAMPLE:
;   idl> emf->setwavelength, 1e-10
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-
self.wavelength= lambda
return
end ;; setwavelength

pro phase::sety_vec, y_vec
;+
; NAME:
;   phase::sety_vec
;
; PURPOSE:
;   set the y_vec in the datastructure
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->sety_vec, y_vec
;
; INPUTS:
;   the field as as DCOMPLEX array
;
; OPTIONAL INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   no
;
; EXAMPLE:
;   idl> 
;   idl> emf->sety_vec, y_vec
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-
self.y_vec= ptr_new(y_vec)
return
end ;; sety_vec

pro phase::setz_vec, z_vec
;+
; NAME:
;   phase::setz_vec
;
; PURPOSE:
;   set the z_vec in the datastructure
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->setz_vec, z_vec
;
; INPUTS:
;   the field as as DCOMPLEX array
;
; OPTIONAL INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   no
;
; EXAMPLE:
;   idl> 
;   idl> emf->setz_vec, z_vec
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-
self.z_vec= ptr_new(z_vec)
return
end ;; setz_vec

pro phase::statistics, comment, amplitude=amplitude, yfwhm=yfwhm, zfwhm=zfwhm, ysig=ysig, zsig=zsig, $
         max=max, total=total, nofit=nofit
;+
; NAME:
;   phase::statistics
;
; PURPOSE:
;   print statistics of a field (does a 2d gaussfit to determine
;   fwhm and sigma), export fwhm or sigma if requested   
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   emf->statistics
;
; INPUTS:
;   no
;
; OPTIONAL INPUTS:
;   comment
;
; KEYWORD PARAMETERS:
;   amplitude: statistics of field - default is intensity field^2/377
;   max:       maximum output
;   nofit:     no fit
;   total:     integral output
;   yfwhm:     vertical fwhm (output)
;   ysig :     vertical rms (output)
;   zfwhm:     horizontal fwhm (output)
;   zsig :     horizontal rms (output)
;
; EXAMPLE:
;   idl> emf->statistics
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-

if n_elements(amplitude) eq 0 then begin
    title= 'intensity statistics'
    myfield= self->getintensity()
    mymaxstr= 'max intensity (W/m^2) = ' 
    mytotstr= 'total intensity (W)   = '
endif else begin 
    title= 'amplitude statistics'
    myfield= self->getamplitude() 
    mymaxstr= 'max field       (V/m) = ' 
    mytotstr= 'total field     (V m) = '
endelse

if n_elements(comment) ne 0 then title+= ' => '+comment

z_vec= self->getz_vec()
y_vec= self->gety_vec()
lambda= self->getwavelength()
zmin= min(z_vec)
zmax= max(z_vec)
ymin= min(y_vec)
ymax= max(y_vec)
binsize= (z_vec[1]- z_vec[0])*(y_vec[1]- y_vec[0])

mymax= max(myfield, mymaxidx)                        ;; photons/m^2
mysum= total(myfield, /double)
mytot= total(myfield, /double) * binsize   ;; sum of all bins*binsize

if mymax gt 0 then field_n= myfield/mymax else field_n= myfield     ;; normalized field for fit

stat   = dblarr(7)
if n_elements(nofit) eq 0 then fit= gauss2dfit(field_n, stat, z_vec, y_vec) else begin
    print, 'we do not fit- we search fwhm'
    z0i= mymaxidx mod  n_elements(z_vec)
    y0i= mymaxidx / n_elements(z_vec)
    mymax05= 0.5* mymax
;    print,'z0i,y0i,mymax05',z0i,y0i,mymax05
    zcut= reform(myfield[*,y0i])
    ycut= reform(myfield[z0i,*])
;    plot, ycut
;    print, 'zcut::::', zcut
    zidx= where(zcut gt mymax05)
;    print, 'zidx::::', zidx
 ;   help, zidx
    yidx= where(ycut gt mymax05)
    
    if n_elements(zidx) gt 1 then zfwhm= z_vec[zidx[n_elements(zidx)-1]]- z_vec[zidx[0]] else zfwhm= 0
    if n_elements(yidx) gt 1 then yfwhm= y_vec[yidx[n_elements(yidx)-1]]- y_vec[yidx[0]] else yfwhm= 0
endelse

print, '=============================================================================='
print, title
print, '=============================================================================='
if n_elements(nofit) eq 0 then begin
print, 'z fwhm=',stat[2]*2.35, ' m, rms = ',stat[2], ' m'
print, 'y fwhm=',stat[3]*2.35, ' m, rms = ',stat[3], ' m'
print, 'z0    =',stat[4], ' m'
print, 'y0    =',stat[5], ' m'
endif else begin
print, 'z fwhm=', zfwhm, ' m'
print, 'y fwhm=', yfwhm, ' m'
print, 'z0    =', z_vec[z0i], ' m'
print, 'y0    =', y_vec[y0i], ' m'
endelse
print, 'zmin, zmax (m) =', zmin, zmax, ', nz=', n_elements(z_vec)
print, 'ymin, ymax (m) =', ymin, ymax, ', ny=', n_elements(y_vec)
print, 'wavelength (nm)=', lambda*1e9
print, mymaxstr, mymax
print, mytotstr, mytot
if n_elements(amplitude) eq 0 then begin
print, 'max intensity (photons/m^2) = ', mymax/1.6e-19
print, 'total intensity (photons)   = ', mytot/1.6e-19
endif
;;print, 'debug: mysum, binsize=', mysum, binsize
print, '=============================================================================='
if n_elements(nofit) eq 0 then begin
print, 'result of gauss2dfit in (m):', stat
print, '=============================================================================='
endif

total=mytot
max= mymax
return
end ;; statistics

pro phase::torus, degree=degree, rl=rl, rw=rw, s1=s1, s2=s2, thetan=thetan, thetag=thetag, verbose=verbose
;+
; NAME:
;   phase::torus
;
; PURPOSE:
;   calculate radii of a toroidal mirror- angle in rad or degree, grazing or normal
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;    
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   /degree: theta in degree, default is rad
;   rl     : short radius
;   rw     : long radius
;   s1     : source distance
;   s2     : image distance
;   thetan : angle to normal
;   thetag : grazing angle
;   /verbose
; 
; OUTPUTS:
;   no
;
; EXAMPLE:
;  idl> emf->torus, /degree, s1=10, s2=1, thetag=2, rl=rl, rw=rw, /verbose
;
; MODIFICATION HISTORY:
;   UF 23.6.14
;-

if (n_elements(thetan) ne 0) and (n_elements(degree) ne 0) then thetan= thetan*!dpi/180.
if (n_elements(thetag) ne 0) and (n_elements(degree) ne 0) then thetan= !dpi/2.0- thetag*!dpi/180.
if (n_elements(thetag) ne 0) and (n_elements(degree) eq 0) then thetan= !dpi/2.0- thetag
if (abs(thetan) ge !dpi/2.0) or (abs(s1) lt 1e-10) or (abs(s2) lt 1e-10) then begin
    message, 'unphysical input- return'
    return
endif

rw= 1.0/((1.0/s1+ 1.0/s2) * cos(thetan)/2.0)
rl= 1.0/((1.0/s1+ 1.0/s2) / (2.0 * cos(thetan)))
thetag= !dpi/2.0- thetan

if (n_elements(verbose) ne 0) then begin
    print, '=========== torus =============='
    print, 's1    = ', s1, ' m'
    print, 's2    = ', s2, ' m'
    print, 'thetan= ', thetan, ' = ', thetan* 180/ !dpi, ' deg.'
    print, 'thetag= ', thetag, ' = ', thetag* 180/ !dpi, ' deg.'
    print, 'rw    = ', rw, ' m'
    print, 'rl    = ', rl, ' m'
    print, '========== torus end ==========='
endif

return
end ;; torus

;; the phase object
pro phase__define
;+
; NAME:
;   phase__define
;
;
; PURPOSE:
;   This is the definition code which is invoked when a new object of
;   type PHASE is created. It cannot be called directly, but only
;   indirectly by the IDL OBJ_NEW() function.  It defines the data
;   structures used for the PHASE class.
;
; CATEGORY:
;   PHASE
;
; CALLING SEQUENCE:
;   Result = OBJ_NEW('PHASE')
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   no
;
; OUTPUTS:
;   no
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;  UF 4. 10. 13
;-

phase = $
  {phase, $
   name:  '', $
   field: ptr_new(), $
   z_vec: ptr_new(), $
   y_vec: ptr_new(), $
   wavelength: 0.0D $
  }
end
;; end

;; end file
