;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <18 Jul 13 11:24:26 flechsig> 
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
;   write genesis
;
;
; CATEGORY:
;   phase_plot
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
;   limit: limit the number of plots to limit
;   png:   save png files
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

if n_elements(comp) ne 0 then begin
    real= real_part(comp)
    imag= imaginary(comp)
endif

file_id = H5F_CREATE(fname)

lambda  = double(wavelength)
gridzize= double(y_vec[1]- y_vec[0])

nz= n_elements(z_vec)
ny= n_elements(y_vec)

field = dblarr(nz*ny*2)

k= 0
for i=0, nz-1 do begin
    for j=0, ny-1 do begin
       field[k]  = real[i,j]
       field[k+1]= imag[i,j]
       k+= 2
    endfor
endfor 


datatype_double_id = H5T_IDL_CREATE(lambda)


w_dataspace_id = H5S_create_simple(1)
g_dataspace_id = H5S_create_simple(1)
f_dataspace_id = H5S_create_simple(1)

group_id = H5G_CREATE(file_id, 'slice000001');

w_dataset_id = H5D_CREATE(file_id,  'wavelength',  datatype_double_id, w_dataspace_id);
g_dataset_id = H5D_CREATE(file_id,  'gridsize',    datatype_double_id, g_dataspace_id);
f_dataset_id = H5D_CREATE(group_id, 'field',       datatype_double_id, f_dataspace_id);

H5D_WRITE, w_dataset_id, lambda
H5D_WRITE, w_dataset_id, gridsize
H5D_WRITE, f_dataset_id, field

H5D_CLOSE, w_dataset_id 
H5D_CLOSE, g_dataset_id
H5D_CLOSE, f_dataset_id 

H5S_CLOSE, w_dataspace_id
H5S_CLOSE, g_dataspace_id
H5S_CLOSE, f_dataspace_id

H5G_CLOSE, group_id

h5f_close, file_id


return
end
;; end
