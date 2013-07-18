 ;; File      : /home/flechsig/phase/src/phaseidl/read_hdf5_dataset.pro
 ;; Date      : <2013-07-16 22:18:34 flechsig> 
 ;; Time-stamp: <18 Jul 13 11:37:25 flechsig> 
 ;; Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 ;; $Source$ 
 ;; $Date$
 ;; $Revision$ 
 ;; $Author$ 

function h5_read_dataset, file_id, name
;+
; NAME:
;   h5_read_dataset
;
;
; PURPOSE:
;   low level routine to read a named dataset from an open hdf5 file
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
;   UF 2013
;-

  dataset_id = H5D_OPEN(file_id, name)
  data       = H5D_READ(dataset_id)
  h5d_close, dataset_id
  
  return, data
end
