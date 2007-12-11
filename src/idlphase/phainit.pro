; phaseinit ...

PRO phainit

; Set System-Variables

defsysv,'!phalib','/home/leitner/cvs/lib/libphase4idl.so'

defsysv,'!phaseidllib',!phalib
defsysv,'!phase4idllib',!phalib


; Initialize Phase-Structures

init_phase_structures

END
