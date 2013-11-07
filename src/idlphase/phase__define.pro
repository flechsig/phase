;  File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/phase__define.pro
;  Date      : <04 Oct 13 16:26:36 flechsig> 
;  Time-stamp: <07 Nov 13 18:27:51 flechsig> 
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
;   acts as an aperture, or generates wavefield with amplitude according to 'type'
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
;   (Be) lense, (thin lens approximation), units (m) and (rad)
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
;   size      : Aperture (diameter) of lense       in m
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


function phase::getintensity, _EXTRA=extra
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
phase= phase0
if n_elements(phunwrap)     ne 0 then phase=phunwrap(phase0)
if n_elements(unwrap_phase) ne 0 then phase=unwrap_phase(phase0)

return, phase
end ;; getphase

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
wl= *self.wavelength

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
if n_elements(fy) eq 0 then fy= 1e200
if n_elements(fz) eq 0 then fz= 1e200
print, 'thin lens with focal length (fy, fz): ', fy, fz

nz= n_elements(*self.z_vec)
ny= n_elements(*self.y_vec)

lcomp = dcomplexarr(nz, ny) ;; make a complex array

for i=0, nz-1 do begin
    for j=0, ny-1 do begin
        f1= *self.z_vec[i]^2/(2.0*fz) + *self.y_vec[j]^2/(2.0*fy)           
        lcomp[i,j] = complex(cos(f1), sin(f1), /double)
    endfor
endfor
*self.field*= lcomp
return 
end
;; end lens

pro phase::mirror, rl=rl, rw=rw, thetag=thetag, azimut=azimut
;+
; NAME:
;   phase::mirror
;
; PURPOSE:
;   calculate the electric field after a thin mirror
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
;   rl:     short radius
;   rw:     long radius
;   thetag: grazing angle in rad   
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
if n_elements(azimut) eq 0 then azimut= 0.0
if n_elements(thetag) eq 0 then thetag= !dpi/2.0
print, 'mirror with radius (rw,rl): ', rw, rl

nz= n_elements(*self.z_vec)
ny= n_elements(*self.y_vec)

lcomp = dcomplexarr(nz, ny) ;; make a complex array

for i=0, nz-1 do begin
    for j=0, ny-1 do begin
        f1= *self.z_vec[i]^2/(2.0*fz) + *self.y_vec[j]^2/(2.0*fy)           
        lcomp[i,j] = complex(cos(f1), sin(f1), /double)
    endfor
endfor
*self.field*= lcomp
return 
end
;; end mirror

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

title= self.name+ ' phase'
myphase= self->getphase(_EXTRA=extra)
mycontour, myphase, *self.z_vec*1e3, *self.y_vec*1e3, title=title, $
  xtitle='z (mm)', ytitle='y (mm)', ztitle='phase'
return 
end ;; plotphase

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

pro phase::statistics, _EXTRA=extra
;+
; NAME:
;   phase::statistics
;
; PURPOSE:
;   print statistics of a field (does a 2d gaussfit to determine
;   fwhm), export fwhm if requested   
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
;   no
;
; EXAMPLE:
;   idl> emf->statistics
;
; MODIFICATION HISTORY:
;   UF Nov 2013
;-
emf= emfield(field=*self.field, y_vec=*self.y_vec, z_vec=*self.z_vec, wavelength=self.wavelength)
emf_statistics, emf, _EXTRA=extra
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
