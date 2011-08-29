c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/dfpl2_uf.F
c$$$ Date      : <06 Feb 04 09:39:28 flechsig> 
c$$$ Time-stamp: <2011-08-29 21:37:10 flechsig> 
c$$$ Author    : Uwe Flechsig, flechsig@psi.ch
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 


	subroutine subdfpl2(g,a,ra,pd)
	implicit real*8(a-h,o-z)
c	real*8 l   never used UF 21.12.99
	dimension a(0:5,0:5)


#include '../phase/phase_struct.F'

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
