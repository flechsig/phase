 ;; File      : /home/flechsig/phase/src/phaseidl/read_hdf5_dataset.pro
 ;; Date      : <2013-07-16 22:18:34 flechsig> 
 ;; Time-stamp: <2013-07-16 22:41:18 flechsig> 
 ;; Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 ;; $Source$ 
 ;; $Date$
 ;; $Revision$ 
 ;; $Author$ 

pro write_hdf5_dataset, file_id, name, data, size
;+
; NAME:
;   read_hdf5_dataset
;
;
; PURPOSE:
;   read a named dataset from an open hdf5 file
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;   file_id: the id of the open hdf5 file
;   name:    the name of the dataset
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
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
;
;-
  dataspace_id = H5S_CREATE_simple(1, size)
  dataset_id = H5D_CREATE(file_id, name)
  H5D_WRITE(dataset_id, data)
  h5d_close, dataset_id
  h5s_close, dataspace_id
  return
end
