c$$$ File      : /afs/psi.ch/project/phase/GIT/phase/src/idlphase/c_c340_main.f
c$$$ Date      : <11 Sep 15 11:50:22 flechsig> 
c$$$ Time-stamp: <11 Sep 15 11:53:03 flechsig> 
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 
c
c ******************************************************************************
c
c   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
c                      Paul Scherrer Institut Villigen, Switzerland
c   
c   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
c          Uwe Flechsig,    uwe.flechsig@psi.ch
c
c ------------------------------------------------------------------------------
c
c   This file is part of PHASE.
c
c   PHASE is free software: you can redistribute it and/or modify
c   it under the terms of the GNU General Public License as published by
c   the Free Software Foundation, version 3 of the License, or
c   (at your option) any later version.
c
c   PHASE is distributed in the hope that it will be useful,
c   but WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU General Public License for more details.
c
c   You should have received a copy of the GNU General Public License
c   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
c
c ******************************************************************************

c file has been taken from the paw library of Michael Scheer
c subroutine to calculate h2 and g1 in idl

*CMZ :          22/04/94  14.58.01  by  Unknown
*-- Author :
      DOUBLE PRECISION FUNCTION DBSKR3(X,NU)
*KEEP,IMP64.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
*KEND.
      CHARACTER*80 ERRTXT
      CHARACTER NAMEI*(*),NAMEK*(*),NAMEIE*(*),NAMEKE*(*)
      PARAMETER (NAMEI = 'BSIR3/DBSIR3', NAMEIE = 'EBSIR3/DEBIR3')
      PARAMETER (NAMEK = 'BSKR3/DBSKR3', NAMEKE = 'EBSKR3/DEBKR3')
      LOGICAL LEX

      DIMENSION BC(0:23,2),CC(0:15,2),PP(-2:2),GG(-2:2)
      PARAMETER (EPS = 1D-15)
      PARAMETER (Z1 = 1, HF = Z1/2)
      PARAMETER (PI = 3.14159265358979324D0)
      PARAMETER (W3 = 1.73205080756887729D0)
      PARAMETER (G1 = 2.67893853470774763D0)
      PARAMETER (G2 = 1.35411793942640042D0)
      PARAMETER (PIH = PI/2, RPIH = 2/PI, RPI2 = 1/(2*PI))
      PARAMETER (C1 = 2*PI/(3*W3))
      PARAMETER (GM = 3*(1/G2-3/G1)/2, GP = (3/G1+1/G2)/2)

      DATA PP(-2) /-0.66666666666666667D0/
      DATA PP(-1) /-0.33333333333333333D0/
      DATA PP( 1) / 0.33333333333333333D0/
      DATA PP( 2) / 0.66666666666666667D0/

      DATA GG(-2) / 0.37328217390739523D0/
      DATA GG(-1) / 0.73848811162164831D0/
      DATA GG( 1) / 1.11984652172218568D0/
      DATA GG( 2) / 1.10773216743247247D0/

      DATA BC( 0,1) / 1.00458617109320735D0/
      DATA BC( 1,1) / 0.00467347919987360D0/
      DATA BC( 2,1) / 0.00009080340481504D0/
      DATA BC( 3,1) / 0.00000372621611059D0/
      DATA BC( 4,1) / 0.00000025207323790D0/
      DATA BC( 5,1) / 0.00000002278211077D0/
      DATA BC( 6,1) / 0.00000000129133228D0/
      DATA BC( 7,1) /-0.00000000061191516D0/
      DATA BC( 8,1) /-0.00000000037561685D0/
      DATA BC( 9,1) /-0.00000000011641546D0/
      DATA BC(10,1) /-0.00000000001444325D0/
      DATA BC(11,1) / 0.00000000000537369D0/
      DATA BC(12,1) / 0.00000000000307427D0/
      DATA BC(13,1) / 0.00000000000029766D0/
      DATA BC(14,1) /-0.00000000000026520D0/
      DATA BC(15,1) /-0.00000000000009137D0/
      DATA BC(16,1) / 0.00000000000001552D0/
      DATA BC(17,1) / 0.00000000000001412D0/
      DATA BC(18,1) /-0.00000000000000023D0/
      DATA BC(19,1) /-0.00000000000000198D0/
      DATA BC(20,1) /-0.00000000000000013D0/
      DATA BC(21,1) / 0.00000000000000029D0/
      DATA BC(22,1) / 0.00000000000000003D0/
      DATA BC(23,1) /-0.00000000000000005D0/

      DATA BC( 0,2) / 0.99363498671692514D0/
      DATA BC( 1,2) /-0.00646715260061603D0/
      DATA BC( 2,2) /-0.00010601882235155D0/
      DATA BC( 3,2) /-0.00000414065771624D0/
      DATA BC( 4,2) /-0.00000029169541821D0/
      DATA BC( 5,2) /-0.00000003657157433D0/
      DATA BC( 6,2) /-0.00000000758159037D0/
      DATA BC( 7,2) /-0.00000000192300852D0/
      DATA BC( 8,2) /-0.00000000042043880D0/
      DATA BC( 9,2) /-0.00000000003937204D0/
      DATA BC(10,2) / 0.00000000001900744D0/
      DATA BC(11,2) / 0.00000000001013764D0/
      DATA BC(12,2) / 0.00000000000133130D0/
      DATA BC(13,2) /-0.00000000000067692D0/
      DATA BC(14,2) /-0.00000000000031172D0/
      DATA BC(15,2) / 0.00000000000001187D0/
      DATA BC(16,2) / 0.00000000000004021D0/
      DATA BC(17,2) / 0.00000000000000478D0/
      DATA BC(18,2) /-0.00000000000000474D0/
      DATA BC(19,2) /-0.00000000000000116D0/
      DATA BC(20,2) / 0.00000000000000059D0/
      DATA BC(21,2) / 0.00000000000000021D0/
      DATA BC(22,2) /-0.00000000000000008D0/
      DATA BC(23,2) /-0.00000000000000003D0/

      DATA CC( 0,1) / 0.99353641227609339D0/
      DATA CC( 1,1) /-0.00631443926079863D0/
      DATA CC( 2,1) / 0.00014300958096113D0/
      DATA CC( 3,1) /-0.00000578706059203D0/
      DATA CC( 4,1) / 0.00000032655033320D0/
      DATA CC( 5,1) /-0.00000002312323195D0/
      DATA CC( 6,1) / 0.00000000193955514D0/
      DATA CC( 7,1) /-0.00000000018589789D0/
      DATA CC( 8,1) / 0.00000000001986842D0/
      DATA CC( 9,1) /-0.00000000000232679D0/
      DATA CC(10,1) / 0.00000000000029468D0/
      DATA CC(11,1) /-0.00000000000003995D0/
      DATA CC(12,1) / 0.00000000000000575D0/
      DATA CC(13,1) /-0.00000000000000087D0/
      DATA CC(14,1) / 0.00000000000000014D0/
      DATA CC(15,1) /-0.00000000000000002D0/

      DATA CC( 0,2) / 1.00914953807278940D0/
      DATA CC( 1,2) / 0.00897120684248360D0/
      DATA CC( 2,2) /-0.00017138959826154D0/
      DATA CC( 3,2) / 0.00000655479254982D0/
      DATA CC( 4,2) /-0.00000035951919049D0/
      DATA CC( 5,2) / 0.00000002502441219D0/
      DATA CC( 6,2) /-0.00000000207492413D0/
      DATA CC( 7,2) / 0.00000000019722367D0/
      DATA CC( 8,2) /-0.00000000002094647D0/
      DATA CC( 9,2) / 0.00000000000244093D0/
      DATA CC(10,2) /-0.00000000000030791D0/
      DATA CC(11,2) / 0.00000000000004161D0/
      DATA CC(12,2) /-0.00000000000000597D0/
      DATA CC(13,2) / 0.00000000000000091D0/
      DATA CC(14,2) /-0.00000000000000014D0/
      DATA CC(15,2) / 0.00000000000000002D0/

      LEX=.FALSE.
      GO TO 9

    9 MU=ABS(NU)
      IF(MU .NE. 1 .AND. MU .NE. 2 .OR. X .LE. 0) THEN
       S=0
C       WRITE(ERRTXT,101) X,NU
C       IF(.NOT.LEX) CALL MTLPRT(NAMEK ,'C340.1',ERRTXT)
C       IF(     LEX) CALL MTLPRT(NAMEKE,'C340.1',ERRTXT)
	WRITE(6,*)' ',X,NU
 	WRITE(6,*)' '
 	WRITE(6,*)' *** ERROR SR C_DBKSR3: WRONG ARGUMENT ***'
 	WRITE(6,*)' '
      ELSEIF(X .LE. 1) THEN
       A0=PP(-1)
       B=HF*X
       D=-LOG(B)
       F=A0*D
       E=EXP(F)
       G=(GM*A0+GP)*E
       BK=C1*(HF*GM*(E+1/E)+GP*D*SINH(F)/F)
       F=BK
       E=A0**2
       P=HF*C1*G
       Q=HF/G
       C=1
       D=B**2
       BK1=P
       DO 11 N = 1,15
       FN=N
       F=(FN*F+P+Q)/(FN**2-E)
       C=C*D/FN
       P=P/(FN-A0)
       Q=Q/(FN+A0)
       G=C*(P-FN*F)
       H=C*F
       BK=BK+H
       BK1=BK1+G
       IF(H*BK1+ABS(G)*BK .LE. EPS*BK*BK1) GO TO 12
   11  CONTINUE
   12  S=BK
       IF(MU .EQ. 2) S=BK1/B
       IF(LEX) S=EXP(X)*S
      ELSEIF(X .LE. 5) THEN
       XN=4*PP(MU)**2
       A=9-XN
       B=25-XN
       C=768*X**2
       C0=48*X
       A0=1
       A1=(16*X+7+XN)/A
       A2=(C+C0*(XN+23)+XN*(XN+62)+129)/(A*B)
       B0=1
       B1=(16*X+9-XN)/A
       B2=(C+C0*B)/(A*B)+1
       C=0
       DO 24 N = 3,30
       C0=C
       FN=N
       FN2=FN+FN
       FN1=FN2-1
       FN3=FN1/(FN2-3)
       FN4=12*FN**2-(1-XN)
       FN5=16*FN1*X
       RAN=1/((FN2+1)**2-XN)
       F1=FN3*(FN4-20*FN)+FN5
       F2=28*FN-FN4-8+FN5
       F3=FN3*((FN2-5)**2-XN)
       A=(F1*A2+F2*A1+F3*A0)*RAN
       B=(F1*B2+F2*B1+F3*B0)*RAN
       C=A/B
       IF(ABS(C0-C) .LT. EPS*ABS(C)) GO TO 25
       A0=A1
       A1=A2
       A2=A
       B0=B1
       B1=B2
       B2=B
   24  CONTINUE
   25  S=C/SQRT(RPIH*X)
       IF(.NOT.LEX) S=EXP(-X)*S
      ELSE
       R=1/X
       H=10*R-1
       ALFA=H+H
       B1=0
       B2=0
       DO 13 I = 15,0,-1
       B0=CC(I,MU)+ALFA*B1-B2
       B2=B1
   13  B1=B0
       S=SQRT(PIH*R)*(B0-H*B2)
       IF(.NOT.LEX) S=EXP(-X)*S
      END IF
   99 DBSKR3=S
      RETURN
C  101 FORMAT('INCORRECT ARGUMENT OR INDEX, X = ',1P,E15.6,' NU = ',I5)
      END
C************************************************************************
	SUBROUTINE C_C340_MAIN(N,EC,X,Y,MOD)

	DIMENSION X(N),Y(N)

	DOUBLE PRECISION XD,XD2,DBSKR3

	DO I=1,N
		XD=DBLE(X(I)/EC)
		XD2=XD/2.D0
		IF (MOD.GT.0) THEN
			Y(I)=(XD*DBSKR3(XD2,MOD))**2
		ELSE
			Y(I)=(DBSKR3(XD2,-MOD))**2
		ENDIF
	ENDDO
	
	RETURN
	END
