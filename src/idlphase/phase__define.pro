;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/phase__define.pro
;  Date      : <04 Oct 13 16:26:36 flechsig> 
;  Time-stamp: <16 Dec 13 09:53:28 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro phase::aperture,  _EXTRA=extra
;+
; NAME:
;   phase::aperture
;
; PURPOSE:
;   acts as an aperture, or generates wavefield with amplitude according to 'type', calls internally the routine aperture.pro- see docs there for updated features
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

pro phase::h5_write, fname,  _EXTRA=extra
;+
; NAME:
;   phase::h5_write
;
; PURPOSE:
;   write hdf5 output - in the moment we use GENESIS format only
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
;
; EXAMPLE:
;   idl> emf->h5_write, 'output.h5'
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-

h5_write_genesis, fname, comp=*self.field, wavelength=self.wavelength, $
  z_vec=*self.z_vec, y_vec=*self.y_vec, _EXTRA=extra 

return
end ;; h5_read

pro phase::gaussbeam, _EXTRA=extra
;+
; NAME:
;   phase::gaussbeam
;
; PURPOSE:
;   generate gaussian beam
;   currently only a 2dim gaussian distribution (in the waist), UF
;   (2b_confirmed) for the intensity distribution we have w=2*sigma
;   with sigma= sigma_x = sigma_y the variance of a 2d Gaussian and
;   sigma_r= sqrt(2) * sigma ???
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
;   field:        field, idl complex array,
;   example:      example calculation plus plot (HeNe laser in 10 m)
;   w0            waist                       in m
;   dist:         distance to waist           in m
;   wavelength    the wavelength              in m
;   y_vec:        vertical   position vector  in m
;   z_vec:        horizontal position vector  in m
;   Nz            points hor.
;   Ny            points vert, default = Nz
;   sizey:        height (m),  default = sizez
;   sizez:        width (m);
;
; OUTPUTS:
;   no
;
; PROCEDURE:
;   fills the object using gaussbeam.pro
;
; EXAMPLE:
;   idl> emf->gaussbeam, dist=0, Nz=243, sizez=0.0002, w0=27.7e-6 , wavelength=1.24e-10
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-
self.name = 'Gaussbeam'
gaussbeam, emf, _EXTRA=extra
self.wavelength= emf.wavelength
self.field= ptr_new(emf.field)
self.z_vec= ptr_new(emf.z_vec)
self.y_vec= ptr_new(emf.y_vec)
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
;   export intensity
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
intensity= abs(*self.field)^2

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

function phase::getphase, phunwrap=phunwrap, unwrap_phase=unwrap_phase, _EXTRA=extra
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
phase0= atan(*self.field, /phase)
;;phase= phase0
help,phase0
if n_elements(phunwrap)     ne 0 then phase=phunwrap(phase0) else $
if n_elements(unwrap_phase) ne 0 then phase=unwrap_phase(phase0) else $
phase= phase0

help, phase
return, phase
end ;; getphase

function phase::getprofile, amplitude=amplitude, y=y, z=z
;+
; NAME:
;   phase::getprofile
;
; PURPOSE:
;   export phase, optional with unwrapping
;
; CATEGORY:
;   phase
;
; CALLING SEQUENCE:
;   phase= phase::getprofile([/phunwrap][/unwrap_phase])
;
; INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;     z: the horizontal profile
;
; OUTPUTS:
;   the phase
;
; EXAMPLE:
;   idl> zprof= emf->getprofile(/z)
;
;; MODIFICATION HISTORY:
;   UF 4.12.13
;-
if n_elements(amplitude) ne 0 then field = self->getamplitude() else field= self->getintensity()
help,field
s= size(field)
nz= s[1]
ny= s[2]
m= max(field, mindex)
mz= mindex mod ny 
my= mindex / ny

print, nz, ny, mz, my, m, mindex

if n_elements(z) ne 0 then prof= reform(field[*,my])
if n_elements(y) ne 0 then prof= reform(field[mz,*])

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

pro phase::mirror, hw=hw, rl=rl, rw=rw, thetag=thetag, azimut=azimut, w=w
;+
; NAME:
;   phase::mirror
;
; PURPOSE:
;   calculate the electric field after a "thin" mirror
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
;   idl> emf->mirror
;
; MODIFICATION HISTORY:
;   UF Nov 2013
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

for i=0, nz-1 do begin
  for j=0, ny-1 do begin
    f1 = myz_vec[i]^2/(2.0*fl) + myy_vec[j]^2/(2.0*fw) ;; phase   
    f1*= (-2)* !dpi/ self.wavelength                   ;; k
    lcomp[i,j]= complex(cos(f1), sin(f1), /double)
  endfor
endfor

*self.field*= lcomp   ;; factor

;; deal with error
if n_elements(hw) ne 0 then begin
    print, 'mirror with height error'
    if n_elements(w) eq 0 then message, 'we need als the mirror coordinate'
    print, 'deal with height error'
    hw1= hw* sin(thetag) ;; the projection of the mirror UF: nicht sicher ob das stimmt
    w1 = w * sin(thetag) ;; the projection of the mirror
    hw2= interpol(hw1, w1, myy_vec)
    for i=0, nz-1 do begin
        for j=0, ny-1 do begin

            if (myy_vec[j] > min(w1)) and (myy_vec[j] < max(w1)) then begin
                f1= hw2[j]    
                f1*= (-4)* !dpi/ self.wavelength
                lcomp[i,j] = complex(cos(f1), sin(f1), /double)
            endif else begin
                lcomp[i,j]= complex(0.0, 0.0, /double)
            endelse
        endfor
    endfor

    *self.field*= lcomp

endif

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

um= interp2d(u, w, l, wm, lm, /grid)  

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
;   calculate the electric field after a "thin" flat mirror defined as phase shifter
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
;   idl> emf->mirrorp
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
  xtitle='z (mm)', ytitle='y (mm)', ztitle='amplitude'
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
mycontour, abs(*self.field)^2, *self.z_vec*1e3, *self.y_vec*1e3, title=title, $
  xtitle='z (mm)', ytitle='y (mm)', ztitle='intensity'
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
myphase= self->getprofile(_EXTRA=extra)
mycontour, myphase, *self.z_vec*1e3, *self.y_vec*1e3, title=title, $
  xtitle='z (mm)', ytitle='y (mm)', ztitle='phase'
return 
end ;; plotphase

pro phase::plotprofile, window=window, _EXTRA=extra
;+
; NAME:
;   phase::plotprofile
;
; PURPOSE:
;   plot the profiles
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
;
; EXAMPLE:
;   idl> emf->plotprofile
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-

if n_elements(window) ne 0 then window, window

title= self.name+ ' phase'
zp= self->getprofile(/z, _EXTRA=extra)
yp= self->getprofile(/y, _EXTRA=extra)
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

plot, [miyz, mayz], [miyzp, mayzp], title=title, $
  xtitle='[z,y] (mm)', ytitle='intensity', /nodata
oplot, z*1e3, zp, color=1
oplot, y*1e3, yp, color=2
legend, ['z','y'], color=[1,2], linestyle=[0,0]
return 
end ;; plotrofile

pro phase::propfresnel, _EXTRA=extra
;+
; NAME:
;   phase::propfresnel
;
; PURPOSE:
;   propagate field using the fourier propagator (it does two FFTs)
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

pro phase::statistics, amplitude=amplitude, yfwhm=yfwhm, zfwhm=zfwhm, ysig=ysig, zsig=zsig
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
;   no
;
; KEYWORD PARAMETERS:
;   amplitude: statistics of field - default is intensity field^2
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
endif else begin 
    title= 'amplitude statistics'
    myfield= self->getamplitude() 
endelse
z_vec= self->getz_vec()
y_vec= self->gety_vec()
lambda= self->getwavelength()

field_n= myfield/max(myfield)

stat= dblarr(7)
fit = gauss2dfit(field_n, stat, z_vec, y_vec)
zmin= min(z_vec)
zmax= max(z_vec)
ymin= min(y_vec)
ymax= max(y_vec)

print, '====================='
print, title
print, '====================='
print, 'z fwhm=',stat[2]*2.35, ' m, rms = ',stat[2], ' m'
print, 'y fwhm=',stat[3]*2.35, ' m, rms = ',stat[3], ' m'
print, 'z0    =',stat[4], ' m'
print, 'y0    =',stat[5], ' m'
print, 'zmin, zmax (m) =', zmin, zmax, ', nz=', n_elements(z_vec)
print, 'ymin, ymax (m) =', ymin, ymax, ', ny=', n_elements(y_vec)
print, 'wavelength (nm)=', lambda*1e9
print, '====================='
print, 'result of gauss2dfit in (m):', stat
print, '====================='

return
end ;; statistics

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
