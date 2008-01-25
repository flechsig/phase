; phaseinit ...

PRO phainit

; Set System-Variables

;; defsysv,'!phalib','/home/leitner/phase/lib/libphase4idl.so'


;; Unter der Annahme, dass phase4idl in phase/idl installiert, 
;; und die lib eben in phase/lib instlliert ist ...
;;  --> Sollte spaeter mit richtigem Pfad, der durch make eingetragen wird ersetzt werden !!!
;; defsysv,'!phalib','../lib/libphase4idl.so'



;; Uwe's Vorschlag: Nach PHASE_HOME pfad suchen
defsysv, '!phalib','libphase4idl.so'
defsysv, '!phalib', exist=exist  ;; check if  defined
;; defaults if not defined
if (exist eq 0) then begin
        phasehome=getenv('PHASE_HOME')
        if (phasehome eq '' ) then phasehome= '/usr/local/phase'
        defsysv, '!phalib', phasehome+'/lib'
        phaselibs= expand_path('+'+!phalib)
        !path=phaselibs+':'+!path
endif


defsysv,'!phaseidllib',!phalib
defsysv,'!phase4idllib',!phalib


; Initialize Phase-Structures

init_phase_structures

END
