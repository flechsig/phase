 ; File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/readhenke.pro
 ; Date      : <20 Dec 13 09:57:10 flechsig> 
 ; Time-stamp: <20 Dec 13 10:44:26 flechsig> 
 ; Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 ; $Source$ 
 ; $Date$
 ; $Revision$ 
 ; $Author$ 

pro readhenke, element, en, f1, f2, plot=plot, verbose=verbose
;+
; NAME:
;   readhenke
;
; PURPOSE:
;   read henke tables  
;
; CATEGORY:
;   generic
;
; CALLING SEQUENCE:
;   readhenke, element, en, f1, f2, plot=plot, verbose=verbose
;
; INPUTS:
;   element: element as chemical formula (String, case sensitive)
;
; OPTIONAL INPUTS:
;   no
;
; KEYWORD PARAMETERS:
;   plot: do a plot
;   verbose: print filename
;
; OPTIONAL OUTPUTS:
;   energy: photon energy in eV
;   f1: f1
;   f2: f2;
;
; PROCEDURE:
;   read table from file, filepath defined in system variable !henketablepath 
;   otherwise default is used '/afs/psi.ch/project/soft_x/OpticsTools/ray_reflec/henke/'
;
; EXAMPLE:
;   idl> readhenke, /verbose, /plot, 'Au', en, f1, f2
;
; MODIFICATION HISTORY:
;  UF 19.12.13
;-

defsysv, '!henketablepath', exists=exists
ext= '.f12'
usage= 'usage: readhenke, element, en, f1, f2'

if exists eq 1 then path= !henketablepath else $
  path='/afs/psi.ch/project/soft_x/OpticsTools/ray_reflec/henke/'
if n_elements(element) eq 0 then begin
    print, usage 
    return
endif

fname=path+element+ext

if n_elements(verbose) ne 0 then print, 'read: ', fname
s= read_ascii(fname, data_start=1) ;; skip 1 line
en= reform(s.field1[0,*])
f1= reform(s.field1[1,*])
f2= reform(s.field1[2,*])

if n_elements(plot) ne 0 then begin
    zone,1,2
    plot, [20, 40000], [0, max(f1)*1.1], /nodata, xtitle='E (eV)', $
      ytitle='f1', title=element, /xlog 
    oplot, en, f1, color=1
    plot,  [20, 40000], [0, max(f2)*1.1], /nodata, xtitle='E (eV)', $
      ytitle='f2', title=element, /xlog 
    oplot, en, f2, color=1
endif
return
end
