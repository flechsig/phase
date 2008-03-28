; phaseinit ...

PRO phainit

; Set System-Variables

;; defsysv,'!phalib','/home/leitner/phase/lib/libphase4idl.so'


;; Unter der Annahme, dass phase4idl in phase/idl installiert, 
;; und die lib eben in phase/lib instlliert ist ...
;;  --> Sollte spaeter mit richtigem Pfad, der durch make eingetragen wird ersetzt werden !!!
;; defsysv,'!phalib','../lib/libphase4idl.so'



defsysv, '!phalib', exist=exist  ;; check if  defined
;; defaults if not defined
if (exist eq 0) then begin
        phasehome=getenv('PHASE_HOME')	; get phasepath
        if (phasehome eq '' ) then phasehome= '/usr/local/phase'
;        phaselibs= expand_path('+'+phasehome+'/lib')  ; path erweitern
;        !path=phaselibs+':'+!path				; path erweitern
	   !path=phasehome+'/lib:'+phasehome+'/idl:'+!path
	        ; path erweitern

	  defsysv, '!phalib', 'libphase4idl.so'		; zeiger auf libphase4idl.so definieren
endif

; !phalib aliase
defsysv, '!phaseidllib',!phalib
defsysv, '!phase4idllib',!phalib


; Initialize Phase-Structures and defines

phainit_defines

phainit_structures

END


