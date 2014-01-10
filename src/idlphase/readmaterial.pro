 ; File      : /afs/psi.ch/user/f/flechsig/phase/src/idlphase/readhenke.pro
 ; Date      : <20 Dec 13 09:57:10 flechsig> 
 ; Time-stamp: <10 Jan 14 16:21:42 flechsig> 
 ; Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 ; $Source$ 
 ; $Date$
 ; $Revision$ 
 ; $Author$ 

pro readmaterial, element, Z, A , rho, plot=plot, verbose=verbose
;+
; NAME:
;   readmaterial
;
; PURPOSE:
;   read material parameter  
;
; CATEGORY:
;   generic
;
; CALLING SEQUENCE:
;   readmaterial, element, Z, A, rho, plot=plot, verbose=verbose
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
;   Z   : Atomic number
;   A   : Molar weight  in g   (== Atomic weight in u)
;   rho : density  in g / cm^3
;
; PROCEDURE:
;   read table from file, defined in system variable !materialfile 
;   otherwise default is used '/afs/psi.ch/project/soft_x/OpticsTools/ray_reflec/material/RHOATOM.DAT'
;
; EXAMPLE:
;   idl>  readmaterial,'Au', Z, A, rho
;
; MODIFICATION HISTORY:
;  RF 8.1.14
;-

defsysv, '!materialfile', exists=exists

usage= 'usage: readmaterial, element, Z, A, rho'

if exists eq 1 then file= !materialtablefile else $
  fname='/afs/psi.ch/project/soft_x/OpticsTools/ray_reflec/material/RHOATOM.DAT'
if n_elements(element) eq 0 then begin
    print, usage 
    return
endif

if n_elements(verbose) ne 0 then print, 'read: ', fname
element = STRTRIM(element, 2) 

El  = STRING(0)
Z   = FIX(1)
A   = DOUBLE(1.0)
rho = DOUBLE(1.0)

openr, u ,fname, /GET_LUN
READF, u, El                    ; comment line

if n_elements(verbose) then begin
  help,El, Z,A,rho
  print, 'Search for <',element,'>'
  print,'Commentline: ',  El
endif

WHILE NOT EOF(u) DO BEGIN & $
  READF,u,  El    
  READF,u,  Z, A, rho  
  El=STRTRIM(El, 2) 
  if n_elements(verbose) ne 0 then   print, '<', El,'> ',Z, ' ' ,A, ' ', rho
  if (element EQ El) then  return
ENDWHILE

return
end

