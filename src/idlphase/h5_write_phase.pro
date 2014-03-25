;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <25 Mar 14 15:54:51 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro h5_write_phase, fname, ycomp=ycomp, zcomp=zcomp, yreal=yreal, yimag=yimag, zreal=zreal, $
                    zimag=zimag, y_vec=y_vec, z_vec=z_vec, wavelength=wavelength, $
                    verbose=verbose
;+
; NAME:
;   h5_write_phase
;
;
; PURPOSE:
;   write phase hdf5 phase format, input units m
;
;
; CATEGORY:
;   phase_h5
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
;
;
;
; MODIFICATION HISTORY:
;    25.3.13 UF
;-

print, 'h5_write_phase called'

if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/myphase.h5'
if n_elements(wavelength) eq 0 then wavelength=1e-10
lambda  = double(wavelength)

if n_elements(ycomp) ne 0 then begin
    yreal= real_part(ycomp)
    yimag= imaginary(ycomp)
endif

if n_elements(zcomp) ne 0 then begin
    zreal= real_part(zcomp)
    zimag= imaginary(zcomp)
endif

file_id = H5F_CREATE(fname)

lambda= double(1.0)
t_vec= lambda

y_vec_mm= y_vec* 1e3  ;; phase grid in mm
z_vec_mm= z_vec* 1e3  ;; phase grid in mm

nz= n_elements(z_vec)
ny= n_elements(y_vec)
nt= n_elements(t_vec)
a = dblarr(nz,ny,4,nt)

a[*,*,0,0]= yreal*1e-3
a[*,*,1,0]= yimag*1e-3
a[*,*,2,0]= zreal*1e-3
a[*,*,3,0]= zimag*1e-3

esize=[ny, nz, 4, nt]
;help, esize

datatype_double_id = H5T_IDL_CREATE(lambda);

e_dataspace_id = H5S_create_simple(esize)
y_dataspace_id = H5S_create_simple(ny)
z_dataspace_id = H5S_create_simple(nz)
t_dataspace_id = H5S_create_simple(nt)
w_dataspace_id = H5S_create_simple(1)

e_dataset_id = H5D_CREATE(file_id, '/e_field',    datatype_double_id, e_dataspace_id);
y_dataset_id = H5D_CREATE(file_id, '/y_vec',      datatype_double_id, y_dataspace_id);
z_dataset_id = H5D_CREATE(file_id, '/z_vec',      datatype_double_id, z_dataspace_id);
t_dataset_id = H5D_CREATE(file_id, '/t_vec',      datatype_double_id, t_dataspace_id);
w_dataset_id = H5D_CREATE(file_id, '/wavelength', datatype_double_id, w_dataspace_id);

H5D_WRITE, e_dataset_id, a
H5D_WRITE, y_dataset_id, y_vec_mm
H5D_WRITE, z_dataset_id, z_vec_mm
;H5D_WRITE, t_dataset_id, t_vec
H5D_WRITE, w_dataset_id, lambda

H5D_CLOSE, e_dataset_id
H5D_CLOSE, y_dataset_id
H5D_CLOSE, z_dataset_id
H5D_CLOSE, t_dataset_id 
H5D_CLOSE, w_dataset_id

H5S_CLOSE, e_dataspace_id
H5S_CLOSE, y_dataspace_id
H5S_CLOSE, z_dataspace_id
H5S_CLOSE, t_dataspace_id
H5S_CLOSE, w_dataspace_id

h5f_close, file_id

print, 'wrote phase file: ', fname
return
end
;; end
