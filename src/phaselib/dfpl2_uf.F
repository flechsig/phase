C File      : /home/pss060/sls/flechsig/phase/src/phaselib/dfpl2_uf.for
C Date      : <20 Dec 99 08:14:12 flechsig> 
C Time-stamp: <20 Dec 99 08:14:43 flechsig> 
C Author    : Flechsig Uwe OVGA/203a 4535, flechsig@psi.ch

	subroutine subdfpl2(g,a,ra,pd)
	implicit real*8(a-h,o-z)
c	real*8 l   never used UF 21.12.99
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
