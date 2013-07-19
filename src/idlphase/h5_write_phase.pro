;; -*-idlwave-*-
;  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseidl/plothdf5.pro
;  Date      : <25 Mar 13 10:51:13 flechsig> 
;  Time-stamp: <19 Jul 13 11:31:37 flechsig> 
;  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

pro h5_write_phase, fname, ycomp=ycomp, zcomp=zcomp, yreal=yreal, yimag=yimag, zreal=zreal, $
                    zimag=zimag, y_vec=y_vec, z_vec=z_vec, $
                    verbose=verbose
;+
; NAME:
;   h5_write_phase
;
;
; PURPOSE:
;   write phase hdf5 phase format
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

if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/myphase.h5'

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

nz= n_elements(z_vec)
ny= n_elements(y_vec)
nt= n_elements(t_vec)
a = dblarr(nz,ny,4,nt)

a[*,*,0,0]= yreal
a[*,*,1,0]= yimag
a[*,*,2,0]= zreal
a[*,*,3,0]= zimag

esize=[ny, nz, 4, nt]
;help, esize

datatype_double_id = H5T_IDL_CREATE(lambda);

e_dataspace_id = H5S_create_simple(esize)
y_dataspace_id = H5S_create_simple(ny)
z_dataspace_id = H5S_create_simple(nz)
t_dataspace_id = H5S_create_simple(nt)

e_dataset_id = H5D_CREATE(file_id, '/e_field', datatype_double_id, e_dataspace_id);
y_dataset_id = H5D_CREATE(file_id, '/y_vec',   datatype_double_id, y_dataspace_id);
z_dataset_id = H5D_CREATE(file_id, '/z_vec',   datatype_double_id, z_dataspace_id);
t_dataset_id = H5D_CREATE(file_id, '/t_vec',   datatype_double_id, t_dataspace_id);

H5D_WRITE, e_dataset_id, a
H5D_WRITE, y_dataset_id, y_vec
H5D_WRITE, z_dataset_id, z_vec
;H5D_WRITE, t_dataset_id, t_vec

H5D_CLOSE,e_dataset_id
H5D_CLOSE,y_dataset_id
H5D_CLOSE,z_dataset_id
H5D_CLOSE,t_dataset_id 

H5S_CLOSE, e_dataspace_id
H5S_CLOSE, y_dataspace_id
H5S_CLOSE, z_dataspace_id
H5S_CLOSE, t_dataspace_id

h5f_close, file_id

return
end
;; end
