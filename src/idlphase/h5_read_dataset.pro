 ;; File      : /home/flechsig/phase/src/phaseidl/read_hdf5_dataset.pro
 ;; Date      : <2013-07-16 22:18:34 flechsig> 
 ;; Time-stamp: <2013-07-16 22:24:39 flechsig> 
 ;; Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 ;; $Source$ 
 ;; $Date$
 ;; $Revision$ 
 ;; $Author$ 

function read_hdf5_dataset(file_id, name)
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

  dataset_id = H5D_OPEN(file_id, name)
  data       = H5D_READ(dataset_id)
  h5d_close, dataset_id
  
  return, data
end
