C Datei: USERDISK_3:[FLECHSIG.PHAS.PHASEFOR.LIB]DFPL2_UF.FOR
C Datum: 16.JAN.1997
C Stand: 16-JAN-1997
C Autor: FLECHSIG, BESSY Berlin

	subroutine subdfpl2(g,a,ra,pd)
	implicit real*8(a-h,o-z)
	real*8 l
	dimension a(0:5,0:5)

        include 'phase_struct.for'

        record /geometryst/ g
        record /rayst/ ra
        record /parder/ pd
 
      pd.pl1w2=(2.*sqrt(pd.arg1)*pd.arg1*pd.arg1w2-
     . sqrt(pd.arg1)*pd.arg1w1**2)/(4.*pd.arg1**2)
      pd.pl1l2=(2.*sqrt(pd.arg1)*pd.arg1*pd.arg1l2-
     . sqrt(pd.arg1)*pd.arg1l1**2)/(4.*pd.arg1**2)
      pd.pl2w2=(2.*sqrt(pd.arg2)*pd.arg2*pd.arg2w2-
     . sqrt(pd.arg2)*pd.arg2w1**2)/(4.*pd.arg2**2)
      pd.pl2l2=(2.*sqrt(pd.arg2)*pd.arg2*pd.arg2l2-
     . sqrt(pd.arg2)*pd.arg2l1**2)/(4.*pd.arg2**2)
      pd.pl1w1l1=(2.*sqrt(pd.arg1)*pd.arg1*pd.arg1w1l1-
     . sqrt(pd.arg1)*pd.arg1l1*pd.arg1w1)/(4.*pd.arg1**2)
      pd.pl2w1l1=(2.*sqrt(pd.arg2)*pd.arg2*pd.arg2w1l1-
     . sqrt(pd.arg2)*pd.arg2l1*pd.arg2w1)/(4.*pd.arg2**2)
      return
      end
