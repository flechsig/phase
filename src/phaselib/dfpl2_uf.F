c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/dfpl2_uf.F
c$$$ Date      : <06 Feb 04 09:39:28 flechsig> 
c$$$ Time-stamp: <02 Sep 14 12:05:46 flechsig> 
c$$$ Author    : Uwe Flechsig, flechsig@psi.ch
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

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


	subroutine subdfpl2(g,a,ra,pd)
	implicit real*8(a-h,o-z)
c	real*8 l   never used UF 21.12.99
	dimension a(0:5,0:5)


#include '../phase/phase_struct.F'

        record /geometryst/ g
        record /rayst/ ra
        record /parder4/ pd
 
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
