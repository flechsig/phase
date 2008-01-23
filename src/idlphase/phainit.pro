; phaseinit ...

PRO phainit

; Set System-Variables

;; defsysv,'!phalib','/home/leitner/phase/lib/libphase4idl.so'


;; Unter der Annahme, dass phase4idl in phase/idl installiert, 
;; und die lib eben in phase/lib instlliert ist ...
;;  --> Sollte spaeter mit richtigem Pfad, der durch make eingetragen wird ersetzt werden !!!
defsysv,'!phalib','../lib/libphase4idl.so'


defsysv,'!phaseidllib',!phalib
defsysv,'!phase4idllib',!phalib


; Initialize Phase-Structures

init_phase_structures

END
