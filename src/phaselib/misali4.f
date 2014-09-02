c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/misali4.for
c$$$ Date      : <16 Feb 04 16:37:40 flechsig> 
c$$$ Time-stamp: <02 Sep 14 12:11:21 flechsig> 

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


      subroutine misali4(a,anew,dw,dl,dz)
      implicit real*8(a-h,o-z)
      dimension a(0:5,0:5),anew(0:5,0:5)
      anew(0.0,0.0)=-a(5.0,0.0)*dw**5-a(4.0,1.0)*dl*dw**4+a(4.0,0.0)*
     . dw**4-a(3.0,2.0)*dl**2*dw**3+a(3.0,1.0)*dl*dw**3-a(3.0,0.0)*dw
     . **3-a(2.0,3.0)*dl**3*dw**2+a(2.0,2.0)*dl**2*dw**2-a(2.0,1.0)*
     . dl*dw**2+a(2.0,0.0)*dw**2-a(1.0,4.0)*dl**4*dw+a(1.0,3.0)*dl**3
     . *dw-a(1.0,2.0)*dl**2*dw+a(1.0,1.0)*dl*dw-a(1.0,0.0)*dw-a(0.0,
     . 5.0)*dl**5+a(0.0,4.0)*dl**4-a(0.0,3.0)*dl**3+a(0.0,2.0)*dl**2-
     . a(0.0,1.0)*dl+a(0.0,0.0)+dz
      anew(0.0,1.0)=a(4.0,1.0)*dw**4+2.0*a(3.0,2.0)*dl*dw**3-a(3.0,
     . 1.0)*dw**3+3.0*a(2.0,3.0)*dl**2*dw**2-2.0*a(2.0,2.0)*dl*dw**2+
     . a(2.0,1.0)*dw**2+4.0*a(1.0,4.0)*dl**3*dw-3.0*a(1.0,3.0)*dl**2*
     . dw+2.0*a(1.0,2.0)*dl*dw-a(1.0,1.0)*dw+5.0*a(0.0,5.0)*dl**4-4.0
     . *a(0.0,4.0)*dl**3+3.0*a(0.0,3.0)*dl**2-2.0*a(0.0,2.0)*dl+a(0.0
     . ,1.0)
      anew(0.0,2.0)=-a(3.0,2.0)*dw**3-3.0*a(2.0,3.0)*dl*dw**2+a(2.0,
     . 2.0)*dw**2-6.0*a(1.0,4.0)*dl**2*dw+3.0*a(1.0,3.0)*dl*dw-a(1.0,
     . 2.0)*dw-10.0*a(0.0,5.0)*dl**3+6.0*a(0.0,4.0)*dl**2-3.0*a(0.0,
     . 3.0)*dl+a(0.0,2.0)
      anew(0.0,3.0)=a(2.0,3.0)*dw**2+4.0*a(1.0,4.0)*dl*dw-a(1.0,3.0)*
     . dw+10.0*a(0.0,5.0)*dl**2-4.0*a(0.0,4.0)*dl+a(0.0,3.0)
      anew(0.0,4.0)=-a(1.0,4.0)*dw-5.0*a(0.0,5.0)*dl+a(0.0,4.0)
      anew(0.0,5.0)=a(0.0,5.0)
      anew(1.0,0.0)=5.0*a(5.0,0.0)*dw**4+4.0*a(4.0,1.0)*dl*dw**3-4.0*
     . a(4.0,0.0)*dw**3+3.0*a(3.0,2.0)*dl**2*dw**2-3.0*a(3.0,1.0)*dl*
     . dw**2+3.0*a(3.0,0.0)*dw**2+2.0*a(2.0,3.0)*dl**3*dw-2.0*a(2.0,
     . 2.0)*dl**2*dw+2.0*a(2.0,1.0)*dl*dw-2.0*a(2.0,0.0)*dw+a(1.0,4.0
     . )*dl**4-a(1.0,3.0)*dl**3+a(1.0,2.0)*dl**2-a(1.0,1.0)*dl+a(1.0,
     . 0.0)
      anew(1.0,1.0)=-4.0*a(4.0,1.0)*dw**3-6.0*a(3.0,2.0)*dl*dw**2+3.0
     . *a(3.0,1.0)*dw**2-6.0*a(2.0,3.0)*dl**2*dw+4.0*a(2.0,2.0)*dl*dw
     . -2.0*a(2.0,1.0)*dw-4.0*a(1.0,4.0)*dl**3+3.0*a(1.0,3.0)*dl**2-
     . 2.0*a(1.0,2.0)*dl+a(1.0,1.0)
      anew(1.0,2.0)=3.0*a(3.0,2.0)*dw**2+6.0*a(2.0,3.0)*dl*dw-2.0*a(
     . 2.0,2.0)*dw+6.0*a(1.0,4.0)*dl**2-3.0*a(1.0,3.0)*dl+a(1.0,2.0)
      anew(1.0,3.0)=-2.0*a(2.0,3.0)*dw-4.0*a(1.0,4.0)*dl+a(1.0,3.0)
      anew(1.0,4.0)=a(1.0,4.0)
      anew(2.0,0.0)=-10.0*a(5.0,0.0)*dw**3-6.0*a(4.0,1.0)*dl*dw**2+
     . 6.0*a(4.0,0.0)*dw**2-3.0*a(3.0,2.0)*dl**2*dw+3.0*a(3.0,1.0)*dl
     . *dw-3.0*a(3.0,0.0)*dw-a(2.0,3.0)*dl**3+a(2.0,2.0)*dl**2-a(2.0,
     . 1.0)*dl+a(2.0,0.0)
      anew(2.0,1.0)=6.0*a(4.0,1.0)*dw**2+6.0*a(3.0,2.0)*dl*dw-3.0*a(
     . 3.0,1.0)*dw+3.0*a(2.0,3.0)*dl**2-2.0*a(2.0,2.0)*dl+a(2.0,1.0)
      anew(2.0,2.0)=-3.0*a(3.0,2.0)*dw-3.0*a(2.0,3.0)*dl+a(2.0,2.0)
      anew(2.0,3.0)=a(2.0,3.0)
      anew(3.0,0.0)=10.0*a(5.0,0.0)*dw**2+4.0*a(4.0,1.0)*dl*dw-4.0*a(
     . 4.0,0.0)*dw+a(3.0,2.0)*dl**2-a(3.0,1.0)*dl+a(3.0,0.0)
      anew(3.0,1.0)=-4.0*a(4.0,1.0)*dw-2.0*a(3.0,2.0)*dl+a(3.0,1.0)
      anew(3.0,2.0)=a(3.0,2.0)
      anew(4.0,0.0)=-5.0*a(5.0,0.0)*dw-a(4.0,1.0)*dl+a(4.0,0.0)
      anew(4.0,1.0)=a(4.0,1.0)
      anew(5.0,0.0)=a(5.0,0.0)
      return
      end
c /afs/psi.ch/user/f/flechsig/phase/src/phase/misali4.for
