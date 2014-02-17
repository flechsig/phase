;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <2014-02-17 21:45:07 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ \

pro h5_read_phase, fname, zcomp=zcomp, zreal=zreal, zimag=zimag, ycomp=ycomp, yreal=yreal, yimag=yimag,$
                   zphase=zphase, zamp=zamp, yphase=yphase, yamp=yamp, $
                   z_vec=z_vec, y_vec=y_vec, wavelength=wavelength, beam=beam, verbose=verbose
;+
; NAME:
;   h5_read_phase
;
;
; PURPOSE:
;   read phase source 
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
;   beam:       beam structure (source4)
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
;   idl> h5_read_phase,'abc.h5', amp=a, z=z, y=y
;   idl> mycontour, a ,z, y
;
; MODIFICATION HISTORY:
;    25.3.13 UF
;-

if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/EZRE_GB_5000.h5' 

file_id= H5F_OPEN(fname)
z_vec = h5_read_dataset(file_id, '/z_vec')
y_vec = h5_read_dataset(file_id, '/y_vec')
t_vec = h5_read_dataset(file_id, '/t_vec')
field = h5_read_dataset(file_id, '/e_field')
h5f_close, file_id

nz   = n_elements(z_vec)
ny   = n_elements(y_vec)
nt   = n_elements(t_vec)

field1= reform(field,nz,ny,4,nt)
field2= reform(field1[*,*,*,0],nz,ny,4)

yreal= reform(field2[*,*,0], nz, ny)
yimag= reform(field2[*,*,1], nz, ny) 
zreal= reform(field2[*,*,2], nz, ny)
zimag= reform(field2[*,*,3], nz, ny)

yamp  = sqrt(yreal^2+yimag^2)
yphase= atan(yimag,yreal)
zamp  = sqrt(zreal^2+zimag^2)
zphase= atan(zimag,zreal)

ycomp= dcomplexarr(nz, ny)
ycomp= complex(yreal, yimag, /double)

zcomp= dcomplexarr(nz, ny)
zcomp= complex(zreal, zimag, /double)

if n_elements(beam) ne 0 then begin
    print, 'fill beam structure'
    beam={source4}  
    beam.iezrex   = long(NZ)
    beam.iezrey   = long(NY)
    beam.ieyrex   = long(NZ)
    beam.ieyrey   = long(NY)
    beam.iezimx   = long(NZ)
    beam.iezimy   = long(NY)
    beam.ieyimx   = long(NZ)
    beam.ieyimy   = long(NY)

    beam.zezre(0:NY-1, 0:NZ-1)= zreal
    beam.zeyre(0:NY-1, 0:NZ-1)= yreal
    beam.zezim(0:NY-1, 0:NZ-1)= zimag
    beam.zeyim(0:NY-1, 0:NZ-1)= yimag
    
    beam.xezremin= min(z_vec)
    beam.xezremax= max(z_vec)
    beam.yezremin= min(y_vec)
    beam.yezremax= max(y_vec)

    beam.xlam= 1234.5
endif 

print, 'wavelength not found in file- set it to default 1e-10'
wavelength=1e-10

print, 'h5_read_phase done'

return
end
;; end
