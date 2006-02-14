      SUBROUTINE already_expired()

C To determine date and time and write it to logical unit LUN

      IMPLICIT NONE
      INTEGER LUN

      CHARACTER*(8) DAY
      CHARACTER*(10) TIM
      CHARACTER*(50) SPACER

      c UF syntax error???     DATA SPACER/10*' '/

c UF      CALL DATE_AND_TIME(DAY,TIM)

      LUN=6

      WRITE(LUN,*)
      WRITE(LUN,*)SPACER,TIM(1:2),':',TIM(3:4),':',TIM(5:6),' '
     &,DAY(7:8),'.',DAY(5:6),'.',DAY(3:4)
      WRITE(LUN,*)

      IF(DAY(3:4).NE.'05'.AND.DAY(3:4).NE.'06') THEN
        WRITE(6,*)'Program PHASE expired..., terminating'
	WRITE(6,*)'Please, contact Johannes Bahrdt' 
cUF        STOP
      ENDIF

      RETURN
      END
