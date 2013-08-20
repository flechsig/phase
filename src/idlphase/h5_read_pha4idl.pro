;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <20 Aug 13 10:59:05 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ \

pro h5_read_pha4idl, fname, zcomp=zcomp, zreal=zreal, zimag=zimag, ycomp=ycomp, yreal=yreal, yimag=yimag,$
                   zphase=zphase, zamp=zamp, yphase=yphase, yamp=yamp, $
                   z_vec=z_vec, y_vec=y_vec, wavelength=wavelength, verbose=verbose
;+
; NAME:
;   h5_read_pha4idl
;
;
; PURPOSE:
;   read phase4idl source 
;
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
;   [yz]amp:    amplitude (2d)
;   [yz]comp:   complex field (2d)
;   [yz]imag:   imaginary part (2d)
;   [yz]phase:  phase: (2d)
;   [yz]real:   real part (2d)
;   verbose:    verbose
;   wavelength: wavelength
;   y_vec:      vertical vector
;   z_vec:      horizontal vector 
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
;   idl> h5_read_pha4idl,'abc.h5', amp=a, z=z, y=y
;   idl> mycontour, a ,z, y
;
; MODIFICATION HISTORY:
;    25.8.13 UF
;-



if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/uf-gauss.h5' 

beam= phaLoadEMFieldHDF5(fname)

y_vec=get_pha_src4_axis_y(beam)
z_vec=get_pha_src4_axis_z(beam)

yreal= beam.zeyre
yimag= beam.zeyim
zreal= beam.zezre
zimag= beam.zezim

yamp  = sqrt(yreal^2+yimag^2)
yphase= atan(yimag,yreal)
zamp  = sqrt(zreal^2+zimag^2)
zphase= atan(zimag,zreal)

nz   = n_elements(z_vec)
ny   = n_elements(y_vec)

ycomp= dcomplexarr(nz, ny)
ycomp= complex(yreal, yimag, /double)

zcomp= dcomplexarr(nz, ny)
zcomp= complex(zreal, zimag, /double)

print, 'h5_read_pha4idl done'

return
end
;; end
