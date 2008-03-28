;  $Source$ 
;  $Date$
;  $Revision$ 
;  $Author$ 

PRO phainit
;+
; NAME:
;   phainit
;
; PURPOSE:
;   Initialize the IDL Phase environment.
;
; CATEGORY:
;   	pro : phase init
;
; CALLING SEQUENCE:
;		phainit
;
; MODIFICATION HISTORY:
;      March 28, 2008, TL, added help
;-


defsysv, '!phalib', exist=exist  ;; check if  defined

if (exist eq 0) then begin            ; defaults if not defined
   phasehome=getenv('PHASE_HOME')     ; get phasepath
   if (phasehome eq '' ) then phasehome= getenv('HOME')+'/phase'
   phaseidl= expand_path('+'+phasehome+'/idl')  ; path erweitern
   !path=phaseidl+':'+!path                     ; path erweitern
   defsysv, '!phalib', phasehome+'/lib/libphase4idl.so'
endif

; !phalib aliase
defsysv, '!phaseidllib',!phalib
defsysv, '!phase4idllib',!phalib

; Initialize Phase-Structures and defines
phainit_defines
phainit_structures

END


