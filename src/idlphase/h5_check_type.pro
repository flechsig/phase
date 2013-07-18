 ;; File      : /home/flechsig/phase/src/phaseidl/check_hdf5_type.pro
 ;; Date      : <2013-07-16 21:39:39 flechsig> 
 ;; Time-stamp: <18 Jul 13 12:15:10 flechsig> 
 ;; Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 ;; $Source$ 
 ;; $Date$
 ;; $Revision$ 
 ;; $Author$ 



function h5_check_type, fname, verbose=verbose
;+
; NAME:
;   h5_check_type
;
;
; PURPOSE:
;   detect the type of a hdf file type=7: phase_hdf5, type=8: GENESIS, returns true if type has been detected
;
;
; CATEGORY:
;   hdf5
;
; CALLING SEQUENCE:
;   a=check_hdf5_type(fname, type, /verbose))
;
; INPUTS:
;   fname: filename
;   type: hdf5 type - 7 or 8
;
; OPTIONAL INPUTS:
;  
; KEYWORD PARAMETERS:
;    verbose: verbose
;
; OUTPUTS:
;   return the type
;
; OPTIONAL OUTPUTS:
;
; PROCEDURE:
;
; EXAMPLE:
;   a=h5_check_type(fname, type, /verbose))
;
;
; MODIFICATION HISTORY:
;   UF 13.7.13
;-

  if n_elements(fname) eq 0 then fname='/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.h5'
  if n_elements(type)  eq 0 then type= 7

  myreturn= 0

  fstructure= H5_PARSE(fname)

;; test phase type
  myreturn= 7
  myreturn*= tag_exist(fstructure, 'e_field')
  myreturn*= tag_exist(fstructure, 'y_vec')
  myreturn*= tag_exist(fstructure, 'z_vec')
  myreturn*= tag_exist(fstructure, 't_vec')
  if myreturn ne 0 then begin
      if (n_elements(verbose) ne 0) then print, 'h5_check_type: file ', fname, ' => hdf5 file from phase (source7)' 
      return, myreturn
  endif
  
;; test genesis type
  myreturn= 8
  myreturn*= tag_exist(fstructure, 'slice000001')       
                                ;myreturn*= tag_exist(fstructure, 'slice000001/field') 
  myreturn*= tag_exist(fstructure, 'wavelength')        
  myreturn*= tag_exist(fstructure, 'gridsize')          
  myreturn*= tag_exist(fstructure, 'slicecount') 
  if myreturn ne 0 then begin    
      if (n_elements(verbose) ne 0) then print, 'h5_check_type: file ', fname, ' => hdf5 file from GENESIS (source7)'
      return, myreturn
  endif 

  if (n_elements(verbose) ne 0) then print, 'h5_check_type: unknown type: '
  myreturn= 0
  
  return,  myreturn
end
