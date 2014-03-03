;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <03 Mar 14 16:00:06 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro h5_write_genesis, fname, comp=comp, real=real, imag=imag, $
                      y_vec=y_vec, z_vec=z_vec, wavelength=wavelength, verbose=verbose
;+
; NAME:
;  h5_write_genesis
;
;
; PURPOSE:
;   write genesis hdf5 file
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

if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/mygenesis.h5'
if n_elements(wavelength) eq 0 then wavelength=1e-10

if n_elements(comp) ne 0 then begin
    real= real_part(comp)
    imag= imaginary(comp)
endif

file_id = H5F_CREATE(fname)

lambda  = double(wavelength)
gridsizey= double(y_vec[1]- y_vec[0])
gridsizez= double(z_vec[1]- z_vec[0])
gridsize= gridsizey 
slicecount= double(1.0)

nz= n_elements(z_vec)
ny= n_elements(y_vec)

if (ny ne nz) or (abs(gridsizey- gridsizez) gt 0.0) then begin
    print, 'error: GENESIS hdf5 files needs a quadratic grid- return'
    return
endif

fsize= nz*ny*2
field = dblarr(fsize)
k= 0L

help, nz, ny, k, field, real, imag, gridsize, lambda
for j=0, ny-1 do begin
    for i=0, nz-1 do begin
       field[k]  = real[i,j]
       field[k+1]= imag[i,j]
       k+= 2
    endfor
endfor 

datatype_double_id = H5T_IDL_CREATE(lambda)

u_dataspace_id = H5S_create_simple(1)
w_dataspace_id = H5S_create_simple(1)
g_dataspace_id = H5S_create_simple(1)
f_dataspace_id = H5S_create_simple(fsize)

group_id = H5G_CREATE(file_id, 'slice000001');

u_dataset_id = H5D_CREATE(file_id,  'slicecount',  datatype_double_id, u_dataspace_id);
w_dataset_id = H5D_CREATE(file_id,  'wavelength',  datatype_double_id, w_dataspace_id);
g_dataset_id = H5D_CREATE(file_id,  'gridsize',    datatype_double_id, g_dataspace_id);
f_dataset_id = H5D_CREATE(group_id, 'field',       datatype_double_id, f_dataspace_id);

H5D_WRITE, u_dataset_id, slicecount   ;; not sure if it is double
H5D_WRITE, w_dataset_id, lambda
H5D_WRITE, g_dataset_id, gridsize
H5D_WRITE, f_dataset_id, field

H5D_CLOSE, u_dataset_id
H5D_CLOSE, w_dataset_id 
H5D_CLOSE, g_dataset_id
H5D_CLOSE, f_dataset_id 

H5S_CLOSE, u_dataspace_id
H5S_CLOSE, w_dataspace_id
H5S_CLOSE, g_dataspace_id
H5S_CLOSE, f_dataspace_id

H5G_CLOSE, group_id

h5f_close, file_id

print, 'wrote genesis file: ', fname

return
end
;; end
