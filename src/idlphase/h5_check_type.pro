 ;; File      : /home/flechsig/phase/src/phaseidl/check_hdf5_type.pro
 ;; Date      : <2013-07-16 21:39:39 flechsig> 
 ;; Time-stamp: <2013-07-16 22:04:07 flechsig> 
 ;; Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 ;; $Source$ 
 ;; $Date$
 ;; $Revision$ 
 ;; $Author$ 

function check_hdf5_type(fname, type, verbose=verbose)
;+
; NAME:
;   check_hdf5_type
;
;
; PURPOSE:
;   detect the type of a hdf file type=7: phase_hdf5, type=8: GENESIS, returns true if type has been detected
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
;
;
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

  if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.h5'

  myreturn= 0

  file_id     = H5F_OPEN(fname)

  if type eq 7 then begin
     myreturn= 1
     if ( H5Lexists(file_id, '/e_field', H5P_DEFAULT) lt 1) myreturn *= 0
     if ( H5Lexists(file_id, '/y_vec',   H5P_DEFAULT) lt 1) myreturn *= 0
     if ( H5Lexists(file_id, '/z_vec',   H5P_DEFAULT) lt 1) myreturn *= 0
     if ( H5Lexists(file_id, '/t_vec',   H5P_DEFAULT) lt 1) myreturn *= 0
     if n_elements(verbose) ne 0 then print, 'file ', fname, ' => hdf5 file from phase (source7)'
  endif 

  if type eq 8 then begin
     myreturn= 1
     if ( H5Lexists(file_id, '/slice000001',       H5P_DEFAULT) lt 1) myreturn *= 0
     if ( H5Lexists(file_id, '/slice000001/field', H5P_DEFAULT) lt 1) myreturn *= 0
     if ( H5Lexists(file_id, '/wavelength',        H5P_DEFAULT) lt 1) myreturn *= 0
     if ( H5Lexists(file_id, '/gridsize',          H5P_DEFAULT) lt 1) myreturn *= 0
     if ( H5Lexists(file_id, '/slicecount',        H5P_DEFAULT) lt 1) myreturn *= 0
     if n_elements(verbose) ne 0 then print, 'file ', fname, ' => hdf5 file from GENESIS (source7)'
  endif 

  if (type ne 8) and (type ne 7) then begin
     print, 'unknown type: ', type
     myreturn = -1
  endif

  h5f_close, file_id

  return,  myreturn
end
