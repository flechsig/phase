c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/misali3.f
c$$$ Date      : <16 Feb 04 16:39:51 flechsig> 
c$$$ Time-stamp: <02 Sep 14 12:11:39 flechsig> 

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


      subroutine misali3(a,anew,drw)
      implicit real*8(a-h,o-z)
      dimension a(0:5,0:5),anew(0:5,0:5)
      dsindrw=dsin(drw)
      dcosdrw=dcos(drw)
      anew(0,0)=0.
      anew(0.0,1.0)=(dcosdrw*(-5.0*a(0.0,5.0)*anew(0.0,0.0)**4*
     . dcosdrw*dsindrw**4-4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*
     . dsindrw**3-3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**2-
     . 2.0*a(0.0,2.0)*anew(0.0,0.0)*dcosdrw*dsindrw-a(0.0,1.0)*
     . dcosdrw-dsindrw))/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      anew(0.0,2.0)=(dcosdrw*(-10.0*a(0.0,5.0)*anew(0.0,1.0)**2*anew(
     . 0.0,0.0)**3*dsindrw**5-20.0*a(0.0,5.0)*anew(0.0,1.0)*anew(0.0,
     . 0.0)**3*dcosdrw*dsindrw**4-10.0*a(0.0,5.0)*anew(0.0,0.0)**3*
     . dcosdrw**2*dsindrw**3-6.0*a(0.0,4.0)*anew(0.0,1.0)**2*anew(0.0
     . ,0.0)**2*dsindrw**4-12.0*a(0.0,4.0)*anew(0.0,1.0)*anew(0.0,0.0
     . )**2*dcosdrw*dsindrw**3-6.0*a(0.0,4.0)*anew(0.0,0.0)**2*
     . dcosdrw**2*dsindrw**2-3.0*a(0.0,3.0)*anew(0.0,1.0)**2*anew(0.0
     . ,0.0)*dsindrw**3-6.0*a(0.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)*
     . dcosdrw*dsindrw**2-3.0*a(0.0,3.0)*anew(0.0,0.0)*dcosdrw**2*
     . dsindrw-a(0.0,2.0)*anew(0.0,1.0)**2*dsindrw**2-2.0*a(0.0,2.0)*
     . anew(0.0,1.0)*dcosdrw*dsindrw-a(0.0,2.0)*dcosdrw**2))/(5.0*a(
     . 0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*dsindrw**5+4.0*a(0.0,4.0)*
     . anew(0.0,0.0)**3*dcosdrw*dsindrw**4+3.0*a(0.0,3.0)*anew(0.0,
     . 0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,2.0)*anew(0.0,0.0)*
     . dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*dsindrw+dsindrw**2-1.0)
      ans1=dcosdrw*(-20.0*a(0.0,5.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew
     . (0.0,0.0)**3*dsindrw**5-20.0*a(0.0,5.0)*anew(0.0,2.0)*anew(0.0
     . ,0.0)**3*dcosdrw*dsindrw**4-10.0*a(0.0,5.0)*anew(0.0,1.0)**3*
     . anew(0.0,0.0)**2*dsindrw**5-30.0*a(0.0,5.0)*anew(0.0,1.0)**2*
     . anew(0.0,0.0)**2*dcosdrw*dsindrw**4-30.0*a(0.0,5.0)*anew(0.0,
     . 1.0)*anew(0.0,0.0)**2*dcosdrw**2*dsindrw**3-10.0*a(0.0,5.0)*
     . anew(0.0,0.0)**2*dcosdrw**3*dsindrw**2-12.0*a(0.0,4.0)*anew(
     . 0.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**4-12.0*a(0.0,
     . 4.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3-4.0*a(
     . 0.0,4.0)*anew(0.0,1.0)**3*anew(0.0,0.0)*dsindrw**4-12.0*a(0.0,
     . 4.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dcosdrw*dsindrw**3-12.0*a(
     . 0.0,4.0)*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrw**2*dsindrw**2-4.0
     . *a(0.0,4.0)*anew(0.0,0.0)*dcosdrw**3*dsindrw-6.0*a(0.0,3.0)*
     . anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(0.0
     . ,3.0)*anew(0.0,2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2-a(0.0,3.0
     . )*anew(0.0,1.0)**3*dsindrw**3-3.0*a(0.0,3.0)*anew(0.0,1.0)**2*
     . dcosdrw*dsindrw**2-3.0*a(0.0,3.0)*anew(0.0,1.0)*dcosdrw**2*
     . dsindrw-a(0.0,3.0)*dcosdrw**3-2.0*a(0.0,2.0)*anew(0.0,2.0)*
     . anew(0.0,1.0)*dsindrw**2-2.0*a(0.0,2.0)*anew(0.0,2.0)*dcosdrw*
     . dsindrw)
      anew(0.0,3.0)=ans1/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      ans3=-4.0*a(0.0,4.0)*anew(0.0,1.0)**3*dcosdrw*dsindrw**3-6.0*a(
     . 0.0,4.0)*anew(0.0,1.0)**2*dcosdrw**2*dsindrw**2-4.0*a(0.0,4.0)
     . *anew(0.0,1.0)*dcosdrw**3*dsindrw-a(0.0,4.0)*dcosdrw**4-6.0*a(
     . 0.0,3.0)*anew(0.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**3-
     . 6.0*a(0.0,3.0)*anew(0.0,3.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2-
     . 3.0*a(0.0,3.0)*anew(0.0,2.0)**2*anew(0.0,0.0)*dsindrw**3-3.0*a
     . (0.0,3.0)*anew(0.0,2.0)*anew(0.0,1.0)**2*dsindrw**3-6.0*a(0.0,
     . 3.0)*anew(0.0,2.0)*anew(0.0,1.0)*dcosdrw*dsindrw**2-3.0*a(0.0,
     . 3.0)*anew(0.0,2.0)*dcosdrw**2*dsindrw-2.0*a(0.0,2.0)*anew(0.0,
     . 3.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(0.0,2.0)*anew(0.0,3.0)*
     . dcosdrw*dsindrw-a(0.0,2.0)*anew(0.0,2.0)**2*dsindrw**2
      ans2=-20.0*a(0.0,5.0)*anew(0.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **3*dsindrw**5-20.0*a(0.0,5.0)*anew(0.0,3.0)*anew(0.0,0.0)**3*
     . dcosdrw*dsindrw**4-10.0*a(0.0,5.0)*anew(0.0,2.0)**2*anew(0.0,
     . 0.0)**3*dsindrw**5-30.0*a(0.0,5.0)*anew(0.0,2.0)*anew(0.0,1.0)
     . **2*anew(0.0,0.0)**2*dsindrw**5-60.0*a(0.0,5.0)*anew(0.0,2.0)*
     . anew(0.0,1.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**4-30.0*a(0.0,
     . 5.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*dcosdrw**2*dsindrw**3-5.0*
     . a(0.0,5.0)*anew(0.0,1.0)**4*anew(0.0,0.0)*dsindrw**5-20.0*a(
     . 0.0,5.0)*anew(0.0,1.0)**3*anew(0.0,0.0)*dcosdrw*dsindrw**4-
     . 30.0*a(0.0,5.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dcosdrw**2*
     . dsindrw**3-20.0*a(0.0,5.0)*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrw
     . **3*dsindrw**2-5.0*a(0.0,5.0)*anew(0.0,0.0)*dcosdrw**4*dsindrw
     . -12.0*a(0.0,4.0)*anew(0.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*
     . dsindrw**4-12.0*a(0.0,4.0)*anew(0.0,3.0)*anew(0.0,0.0)**2*
     . dcosdrw*dsindrw**3-6.0*a(0.0,4.0)*anew(0.0,2.0)**2*anew(0.0,
     . 0.0)**2*dsindrw**4-12.0*a(0.0,4.0)*anew(0.0,2.0)*anew(0.0,1.0)
     . **2*anew(0.0,0.0)*dsindrw**4-24.0*a(0.0,4.0)*anew(0.0,2.0)*
     . anew(0.0,1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**3-12.0*a(0.0,4.0)
     . *anew(0.0,2.0)*anew(0.0,0.0)*dcosdrw**2*dsindrw**2-a(0.0,4.0)*
     . anew(0.0,1.0)**4*dsindrw**4+ans3
      ans1=dcosdrw*ans2
      anew(0.0,4.0)=ans1/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      ans4=-3.0*a(0.0,3.0)*anew(0.0,2.0)**2*dcosdrw*dsindrw**2-2.0*a(
     . 0.0,2.0)*anew(0.0,4.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(0.0,2.0)
     . *anew(0.0,4.0)*dcosdrw*dsindrw-2.0*a(0.0,2.0)*anew(0.0,3.0)*
     . anew(0.0,2.0)*dsindrw**2
      ans3=-12.0*a(0.0,4.0)*anew(0.0,4.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **2*dsindrw**4-12.0*a(0.0,4.0)*anew(0.0,4.0)*anew(0.0,0.0)**2*
     . dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(0.0,3.0)*anew(0.0,2.0)
     . *anew(0.0,0.0)**2*dsindrw**4-12.0*a(0.0,4.0)*anew(0.0,3.0)*
     . anew(0.0,1.0)**2*anew(0.0,0.0)*dsindrw**4-24.0*a(0.0,4.0)*anew
     . (0.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**3-12.0*
     . a(0.0,4.0)*anew(0.0,3.0)*anew(0.0,0.0)*dcosdrw**2*dsindrw**2-
     . 12.0*a(0.0,4.0)*anew(0.0,2.0)**2*anew(0.0,1.0)*anew(0.0,0.0)*
     . dsindrw**4-12.0*a(0.0,4.0)*anew(0.0,2.0)**2*anew(0.0,0.0)*
     . dcosdrw*dsindrw**3-4.0*a(0.0,4.0)*anew(0.0,2.0)*anew(0.0,1.0)
     . **3*dsindrw**4-12.0*a(0.0,4.0)*anew(0.0,2.0)*anew(0.0,1.0)**2*
     . dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(0.0,2.0)*anew(0.0,1.0)
     . *dcosdrw**2*dsindrw**2-4.0*a(0.0,4.0)*anew(0.0,2.0)*dcosdrw**3
     . *dsindrw-6.0*a(0.0,3.0)*anew(0.0,4.0)*anew(0.0,1.0)*anew(0.0,
     . 0.0)*dsindrw**3-6.0*a(0.0,3.0)*anew(0.0,4.0)*anew(0.0,0.0)*
     . dcosdrw*dsindrw**2-6.0*a(0.0,3.0)*anew(0.0,3.0)*anew(0.0,2.0)*
     . anew(0.0,0.0)*dsindrw**3-3.0*a(0.0,3.0)*anew(0.0,3.0)*anew(0.0
     . ,1.0)**2*dsindrw**3-6.0*a(0.0,3.0)*anew(0.0,3.0)*anew(0.0,1.0)
     . *dcosdrw*dsindrw**2-3.0*a(0.0,3.0)*anew(0.0,3.0)*dcosdrw**2*
     . dsindrw-3.0*a(0.0,3.0)*anew(0.0,2.0)**2*anew(0.0,1.0)*dsindrw
     . **3+ans4
      ans2=-20.0*a(0.0,5.0)*anew(0.0,4.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **3*dsindrw**5-20.0*a(0.0,5.0)*anew(0.0,4.0)*anew(0.0,0.0)**3*
     . dcosdrw*dsindrw**4-20.0*a(0.0,5.0)*anew(0.0,3.0)*anew(0.0,2.0)
     . *anew(0.0,0.0)**3*dsindrw**5-30.0*a(0.0,5.0)*anew(0.0,3.0)*
     . anew(0.0,1.0)**2*anew(0.0,0.0)**2*dsindrw**5-60.0*a(0.0,5.0)*
     . anew(0.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**
     . 4-30.0*a(0.0,5.0)*anew(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw**2*
     . dsindrw**3-30.0*a(0.0,5.0)*anew(0.0,2.0)**2*anew(0.0,1.0)*anew
     . (0.0,0.0)**2*dsindrw**5-30.0*a(0.0,5.0)*anew(0.0,2.0)**2*anew(
     . 0.0,0.0)**2*dcosdrw*dsindrw**4-20.0*a(0.0,5.0)*anew(0.0,2.0)*
     . anew(0.0,1.0)**3*anew(0.0,0.0)*dsindrw**5-60.0*a(0.0,5.0)*anew
     . (0.0,2.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dcosdrw*dsindrw**4-
     . 60.0*a(0.0,5.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*
     . dcosdrw**2*dsindrw**3-20.0*a(0.0,5.0)*anew(0.0,2.0)*anew(0.0,
     . 0.0)*dcosdrw**3*dsindrw**2-a(0.0,5.0)*anew(0.0,1.0)**5*dsindrw
     . **5-5.0*a(0.0,5.0)*anew(0.0,1.0)**4*dcosdrw*dsindrw**4-10.0*a(
     . 0.0,5.0)*anew(0.0,1.0)**3*dcosdrw**2*dsindrw**3-10.0*a(0.0,5.0
     . )*anew(0.0,1.0)**2*dcosdrw**3*dsindrw**2-5.0*a(0.0,5.0)*anew(
     . 0.0,1.0)*dcosdrw**4*dsindrw-a(0.0,5.0)*dcosdrw**5+ans3
      ans1=dcosdrw*ans2
      anew(0.0,5.0)=ans1/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      anew(1.0,0.0)=(-dcosdrw*(a(1.0,4.0)*anew(0.0,0.0)**4*dsindrw**4
     . +a(1.0,3.0)*anew(0.0,0.0)**3*dsindrw**3+a(1.0,2.0)*anew(0.0,
     . 0.0)**2*dsindrw**2+a(1.0,1.0)*anew(0.0,0.0)*dsindrw+a(1.0,0.0)
     . ))/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*dsindrw**5+4.0*a(
     . 0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+3.0*a(0.0,3.0)*
     . anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,2.0)*anew(0.0,
     . 0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*dsindrw+dsindrw**2-
     . 1.0)
      anew(1.0,1.0)=(dcosdrw*(-4.0*a(1.0,4.0)*anew(0.0,1.0)*anew(0.0,
     . 0.0)**3*dsindrw**4-4.0*a(1.0,4.0)*anew(0.0,0.0)**3*dcosdrw*
     . dsindrw**3-3.0*a(1.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*
     . dsindrw**3-3.0*a(1.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**2-
     . 2.0*a(1.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**2-2.0*a(
     . 1.0,2.0)*anew(0.0,0.0)*dcosdrw*dsindrw-a(1.0,1.0)*anew(0.0,1.0
     . )*dsindrw-a(1.0,1.0)*dcosdrw-20.0*a(0.0,5.0)*anew(1.0,0.0)*
     . anew(0.0,1.0)*anew(0.0,0.0)**3*dsindrw**5-20.0*a(0.0,5.0)*anew
     . (1.0,0.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4-12.0*a(0.0,4.0)*
     . anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**4-12.0*a
     . (0.0,4.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3-
     . 6.0*a(0.0,3.0)*anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*
     . dsindrw**3-6.0*a(0.0,3.0)*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrw*
     . dsindrw**2-2.0*a(0.0,2.0)*anew(1.0,0.0)*anew(0.0,1.0)*dsindrw
     . **2-2.0*a(0.0,2.0)*anew(1.0,0.0)*dcosdrw*dsindrw))/(5.0*a(0.0,
     . 5.0)*anew(0.0,0.0)**4*dcosdrw*dsindrw**5+4.0*a(0.0,4.0)*anew(
     . 0.0,0.0)**3*dcosdrw*dsindrw**4+3.0*a(0.0,3.0)*anew(0.0,0.0)**2
     . *dcosdrw*dsindrw**3+2.0*a(0.0,2.0)*anew(0.0,0.0)*dcosdrw*
     . dsindrw**2+a(0.0,1.0)*dcosdrw*dsindrw+dsindrw**2-1.0)
      ans3=-12.0*a(0.0,4.0)*anew(1.0,1.0)*anew(0.0,0.0)**2*dcosdrw*
     . dsindrw**3-12.0*a(0.0,4.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(
     . 0.0,0.0)**2*dsindrw**4-12.0*a(0.0,4.0)*anew(1.0,0.0)*anew(0.0,
     . 1.0)**2*anew(0.0,0.0)*dsindrw**4-24.0*a(0.0,4.0)*anew(1.0,0.0)
     . *anew(0.0,1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**3-12.0*a(0.0,4.0
     . )*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrw**2*dsindrw**2-6.0*a(0.0,
     . 3.0)*anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0*
     . a(0.0,3.0)*anew(1.0,1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2-6.0*
     . a(0.0,3.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrw**
     . 3-3.0*a(0.0,3.0)*anew(1.0,0.0)*anew(0.0,1.0)**2*dsindrw**3-6.0
     . *a(0.0,3.0)*anew(1.0,0.0)*anew(0.0,1.0)*dcosdrw*dsindrw**2-3.0
     . *a(0.0,3.0)*anew(1.0,0.0)*dcosdrw**2*dsindrw-2.0*a(0.0,2.0)*
     . anew(1.0,1.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(0.0,2.0)*anew(1.0
     . ,1.0)*dcosdrw*dsindrw-2.0*a(0.0,2.0)*anew(1.0,0.0)*anew(0.0,
     . 2.0)*dsindrw**2
      ans2=-4.0*a(1.0,4.0)*anew(0.0,2.0)*anew(0.0,0.0)**3*dsindrw**4-
     . 6.0*a(1.0,4.0)*anew(0.0,1.0)**2*anew(0.0,0.0)**2*dsindrw**4-
     . 12.0*a(1.0,4.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw
     . **3-6.0*a(1.0,4.0)*anew(0.0,0.0)**2*dcosdrw**2*dsindrw**2-3.0*
     . a(1.0,3.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*dsindrw**3-3.0*a(1.0
     . ,3.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dsindrw**3-6.0*a(1.0,3.0)
     . *anew(0.0,1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2-3.0*a(1.0,3.0)
     . *anew(0.0,0.0)*dcosdrw**2*dsindrw-2.0*a(1.0,2.0)*anew(0.0,2.0)
     . *anew(0.0,0.0)*dsindrw**2-a(1.0,2.0)*anew(0.0,1.0)**2*dsindrw
     . **2-2.0*a(1.0,2.0)*anew(0.0,1.0)*dcosdrw*dsindrw-a(1.0,2.0)*
     . dcosdrw**2-a(1.0,1.0)*anew(0.0,2.0)*dsindrw-20.0*a(0.0,5.0)*
     . anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)**3*dsindrw**5-20.0*a
     . (0.0,5.0)*anew(1.0,1.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4-
     . 20.0*a(0.0,5.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)**3*
     . dsindrw**5-30.0*a(0.0,5.0)*anew(1.0,0.0)*anew(0.0,1.0)**2*anew
     . (0.0,0.0)**2*dsindrw**5-60.0*a(0.0,5.0)*anew(1.0,0.0)*anew(0.0
     . ,1.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**4-30.0*a(0.0,5.0)*anew
     . (1.0,0.0)*anew(0.0,0.0)**2*dcosdrw**2*dsindrw**3-12.0*a(0.0,
     . 4.0)*anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**4+
     . ans3
      ans1=dcosdrw*ans2
      anew(1.0,2.0)=ans1/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      ans4=-12.0*a(0.0,4.0)*anew(1.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)
     . **2*dsindrw**4-24.0*a(0.0,4.0)*anew(1.0,0.0)*anew(0.0,2.0)*
     . anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**4-24.0*a(0.0,4.0)*anew(
     . 1.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**3-4.0*a(
     . 0.0,4.0)*anew(1.0,0.0)*anew(0.0,1.0)**3*dsindrw**4-12.0*a(0.0,
     . 4.0)*anew(1.0,0.0)*anew(0.0,1.0)**2*dcosdrw*dsindrw**3-12.0*a(
     . 0.0,4.0)*anew(1.0,0.0)*anew(0.0,1.0)*dcosdrw**2*dsindrw**2-4.0
     . *a(0.0,4.0)*anew(1.0,0.0)*dcosdrw**3*dsindrw-6.0*a(0.0,3.0)*
     . anew(1.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(0.0
     . ,3.0)*anew(1.0,2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2-6.0*a(0.0
     . ,3.0)*anew(1.0,1.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrw**3-3.0
     . *a(0.0,3.0)*anew(1.0,1.0)*anew(0.0,1.0)**2*dsindrw**3-6.0*a(
     . 0.0,3.0)*anew(1.0,1.0)*anew(0.0,1.0)*dcosdrw*dsindrw**2-3.0*a(
     . 0.0,3.0)*anew(1.0,1.0)*dcosdrw**2*dsindrw-6.0*a(0.0,3.0)*anew(
     . 1.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(0.0,3.0)
     . *anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*dsindrw**3-6.0*a(
     . 0.0,3.0)*anew(1.0,0.0)*anew(0.0,2.0)*dcosdrw*dsindrw**2-2.0*a(
     . 0.0,2.0)*anew(1.0,2.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(0.0,2.0)
     . *anew(1.0,2.0)*dcosdrw*dsindrw-2.0*a(0.0,2.0)*anew(1.0,1.0)*
     . anew(0.0,2.0)*dsindrw**2-2.0*a(0.0,2.0)*anew(1.0,0.0)*anew(0.0
     . ,3.0)*dsindrw**2
      ans3=-30.0*a(0.0,5.0)*anew(1.0,1.0)*anew(0.0,1.0)**2*anew(0.0,
     . 0.0)**2*dsindrw**5-60.0*a(0.0,5.0)*anew(1.0,1.0)*anew(0.0,1.0)
     . *anew(0.0,0.0)**2*dcosdrw*dsindrw**4-30.0*a(0.0,5.0)*anew(1.0,
     . 1.0)*anew(0.0,0.0)**2*dcosdrw**2*dsindrw**3-20.0*a(0.0,5.0)*
     . anew(1.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)**3*dsindrw**5-60.0*a
     . (0.0,5.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0,
     . 0.0)**2*dsindrw**5-60.0*a(0.0,5.0)*anew(1.0,0.0)*anew(0.0,2.0)
     . *anew(0.0,0.0)**2*dcosdrw*dsindrw**4-20.0*a(0.0,5.0)*anew(1.0,
     . 0.0)*anew(0.0,1.0)**3*anew(0.0,0.0)*dsindrw**5-60.0*a(0.0,5.0)
     . *anew(1.0,0.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dcosdrw*dsindrw
     . **4-60.0*a(0.0,5.0)*anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*
     . dcosdrw**2*dsindrw**3-20.0*a(0.0,5.0)*anew(1.0,0.0)*anew(0.0,
     . 0.0)*dcosdrw**3*dsindrw**2-12.0*a(0.0,4.0)*anew(1.0,2.0)*anew(
     . 0.0,1.0)*anew(0.0,0.0)**2*dsindrw**4-12.0*a(0.0,4.0)*anew(1.0,
     . 2.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(
     . 1.0,1.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*dsindrw**4-12.0*a(0.0,
     . 4.0)*anew(1.0,1.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dsindrw**4-
     . 24.0*a(0.0,4.0)*anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)*
     . dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(1.0,1.0)*anew(0.0,0.0)
     . *dcosdrw**2*dsindrw**2+ans4
      ans2=-4.0*a(1.0,4.0)*anew(0.0,3.0)*anew(0.0,0.0)**3*dsindrw**4-
     . 12.0*a(1.0,4.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*
     . dsindrw**4-12.0*a(1.0,4.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*
     . dcosdrw*dsindrw**3-4.0*a(1.0,4.0)*anew(0.0,1.0)**3*anew(0.0,
     . 0.0)*dsindrw**4-12.0*a(1.0,4.0)*anew(0.0,1.0)**2*anew(0.0,0.0)
     . *dcosdrw*dsindrw**3-12.0*a(1.0,4.0)*anew(0.0,1.0)*anew(0.0,0.0
     . )*dcosdrw**2*dsindrw**2-4.0*a(1.0,4.0)*anew(0.0,0.0)*dcosdrw**
     . 3*dsindrw-3.0*a(1.0,3.0)*anew(0.0,3.0)*anew(0.0,0.0)**2*
     . dsindrw**3-6.0*a(1.0,3.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0
     . ,0.0)*dsindrw**3-6.0*a(1.0,3.0)*anew(0.0,2.0)*anew(0.0,0.0)*
     . dcosdrw*dsindrw**2-a(1.0,3.0)*anew(0.0,1.0)**3*dsindrw**3-3.0*
     . a(1.0,3.0)*anew(0.0,1.0)**2*dcosdrw*dsindrw**2-3.0*a(1.0,3.0)*
     . anew(0.0,1.0)*dcosdrw**2*dsindrw-a(1.0,3.0)*dcosdrw**3-2.0*a(
     . 1.0,2.0)*anew(0.0,3.0)*anew(0.0,0.0)*dsindrw**2-2.0*a(1.0,2.0)
     . *anew(0.0,2.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(1.0,2.0)*anew(
     . 0.0,2.0)*dcosdrw*dsindrw-a(1.0,1.0)*anew(0.0,3.0)*dsindrw-20.0
     . *a(0.0,5.0)*anew(1.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)**3*
     . dsindrw**5-20.0*a(0.0,5.0)*anew(1.0,2.0)*anew(0.0,0.0)**3*
     . dcosdrw*dsindrw**4-20.0*a(0.0,5.0)*anew(1.0,1.0)*anew(0.0,2.0)
     . *anew(0.0,0.0)**3*dsindrw**5+ans3
      ans1=dcosdrw*ans2
      anew(1.0,3.0)=ans1/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      ans6=-6.0*a(0.0,3.0)*anew(1.0,1.0)*anew(0.0,3.0)*anew(0.0,0.0)*
     . dsindrw**3-6.0*a(0.0,3.0)*anew(1.0,1.0)*anew(0.0,2.0)*anew(0.0
     . ,1.0)*dsindrw**3-6.0*a(0.0,3.0)*anew(1.0,1.0)*anew(0.0,2.0)*
     . dcosdrw*dsindrw**2-6.0*a(0.0,3.0)*anew(1.0,0.0)*anew(0.0,4.0)*
     . anew(0.0,0.0)*dsindrw**3-6.0*a(0.0,3.0)*anew(1.0,0.0)*anew(0.0
     . ,3.0)*anew(0.0,1.0)*dsindrw**3-6.0*a(0.0,3.0)*anew(1.0,0.0)*
     . anew(0.0,3.0)*dcosdrw*dsindrw**2-3.0*a(0.0,3.0)*anew(1.0,0.0)*
     . anew(0.0,2.0)**2*dsindrw**3-2.0*a(0.0,2.0)*anew(1.0,3.0)*anew(
     . 0.0,1.0)*dsindrw**2-2.0*a(0.0,2.0)*anew(1.0,3.0)*dcosdrw*
     . dsindrw-2.0*a(0.0,2.0)*anew(1.0,2.0)*anew(0.0,2.0)*dsindrw**2-
     . 2.0*a(0.0,2.0)*anew(1.0,1.0)*anew(0.0,3.0)*dsindrw**2-2.0*a(
     . 0.0,2.0)*anew(1.0,0.0)*anew(0.0,4.0)*dsindrw**2
      ans5=-24.0*a(0.0,4.0)*anew(1.0,1.0)*anew(0.0,2.0)*anew(0.0,1.0)
     . *anew(0.0,0.0)*dsindrw**4-24.0*a(0.0,4.0)*anew(1.0,1.0)*anew(
     . 0.0,2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**3-4.0*a(0.0,4.0)*anew(
     . 1.0,1.0)*anew(0.0,1.0)**3*dsindrw**4-12.0*a(0.0,4.0)*anew(1.0,
     . 1.0)*anew(0.0,1.0)**2*dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(
     . 1.0,1.0)*anew(0.0,1.0)*dcosdrw**2*dsindrw**2-4.0*a(0.0,4.0)*
     . anew(1.0,1.0)*dcosdrw**3*dsindrw-12.0*a(0.0,4.0)*anew(1.0,0.0)
     . *anew(0.0,4.0)*anew(0.0,0.0)**2*dsindrw**4-24.0*a(0.0,4.0)*
     . anew(1.0,0.0)*anew(0.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)*
     . dsindrw**4-24.0*a(0.0,4.0)*anew(1.0,0.0)*anew(0.0,3.0)*anew(
     . 0.0,0.0)*dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(1.0,0.0)*anew
     . (0.0,2.0)**2*anew(0.0,0.0)*dsindrw**4-12.0*a(0.0,4.0)*anew(1.0
     . ,0.0)*anew(0.0,2.0)*anew(0.0,1.0)**2*dsindrw**4-24.0*a(0.0,4.0
     . )*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*dcosdrw*dsindrw**3
     . -12.0*a(0.0,4.0)*anew(1.0,0.0)*anew(0.0,2.0)*dcosdrw**2*
     . dsindrw**2-6.0*a(0.0,3.0)*anew(1.0,3.0)*anew(0.0,1.0)*anew(0.0
     . ,0.0)*dsindrw**3-6.0*a(0.0,3.0)*anew(1.0,3.0)*anew(0.0,0.0)*
     . dcosdrw*dsindrw**2-6.0*a(0.0,3.0)*anew(1.0,2.0)*anew(0.0,2.0)*
     . anew(0.0,0.0)*dsindrw**3-3.0*a(0.0,3.0)*anew(1.0,2.0)*anew(0.0
     . ,1.0)**2*dsindrw**3-6.0*a(0.0,3.0)*anew(1.0,2.0)*anew(0.0,1.0)
     . *dcosdrw*dsindrw**2-3.0*a(0.0,3.0)*anew(1.0,2.0)*dcosdrw**2*
     . dsindrw+ans6
      ans4=-60.0*a(0.0,5.0)*anew(1.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)
     . **2*dcosdrw*dsindrw**4-30.0*a(0.0,5.0)*anew(1.0,0.0)*anew(0.0,
     . 2.0)**2*anew(0.0,0.0)**2*dsindrw**5-60.0*a(0.0,5.0)*anew(1.0,
     . 0.0)*anew(0.0,2.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dsindrw**5-
     . 120.0*a(0.0,5.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*
     . anew(0.0,0.0)*dcosdrw*dsindrw**4-60.0*a(0.0,5.0)*anew(1.0,0.0)
     . *anew(0.0,2.0)*anew(0.0,0.0)*dcosdrw**2*dsindrw**3-5.0*a(0.0,
     . 5.0)*anew(1.0,0.0)*anew(0.0,1.0)**4*dsindrw**5-20.0*a(0.0,5.0)
     . *anew(1.0,0.0)*anew(0.0,1.0)**3*dcosdrw*dsindrw**4-30.0*a(0.0,
     . 5.0)*anew(1.0,0.0)*anew(0.0,1.0)**2*dcosdrw**2*dsindrw**3-20.0
     . *a(0.0,5.0)*anew(1.0,0.0)*anew(0.0,1.0)*dcosdrw**3*dsindrw**2-
     . 5.0*a(0.0,5.0)*anew(1.0,0.0)*dcosdrw**4*dsindrw-12.0*a(0.0,4.0
     . )*anew(1.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**4-12.0
     . *a(0.0,4.0)*anew(1.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3-
     . 12.0*a(0.0,4.0)*anew(1.0,2.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*
     . dsindrw**4-12.0*a(0.0,4.0)*anew(1.0,2.0)*anew(0.0,1.0)**2*anew
     . (0.0,0.0)*dsindrw**4-24.0*a(0.0,4.0)*anew(1.0,2.0)*anew(0.0,
     . 1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(1.0
     . ,2.0)*anew(0.0,0.0)*dcosdrw**2*dsindrw**2-12.0*a(0.0,4.0)*anew
     . (1.0,1.0)*anew(0.0,3.0)*anew(0.0,0.0)**2*dsindrw**4+ans5
      ans3=-2.0*a(1.0,2.0)*anew(0.0,3.0)*dcosdrw*dsindrw-a(1.0,2.0)*
     . anew(0.0,2.0)**2*dsindrw**2-a(1.0,1.0)*anew(0.0,4.0)*dsindrw-
     . 20.0*a(0.0,5.0)*anew(1.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)**3*
     . dsindrw**5-20.0*a(0.0,5.0)*anew(1.0,3.0)*anew(0.0,0.0)**3*
     . dcosdrw*dsindrw**4-20.0*a(0.0,5.0)*anew(1.0,2.0)*anew(0.0,2.0)
     . *anew(0.0,0.0)**3*dsindrw**5-30.0*a(0.0,5.0)*anew(1.0,2.0)*
     . anew(0.0,1.0)**2*anew(0.0,0.0)**2*dsindrw**5-60.0*a(0.0,5.0)*
     . anew(1.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**
     . 4-30.0*a(0.0,5.0)*anew(1.0,2.0)*anew(0.0,0.0)**2*dcosdrw**2*
     . dsindrw**3-20.0*a(0.0,5.0)*anew(1.0,1.0)*anew(0.0,3.0)*anew(
     . 0.0,0.0)**3*dsindrw**5-60.0*a(0.0,5.0)*anew(1.0,1.0)*anew(0.0,
     . 2.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**5-60.0*a(0.0,5.0)
     . *anew(1.0,1.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw
     . **4-20.0*a(0.0,5.0)*anew(1.0,1.0)*anew(0.0,1.0)**3*anew(0.0,
     . 0.0)*dsindrw**5-60.0*a(0.0,5.0)*anew(1.0,1.0)*anew(0.0,1.0)**2
     . *anew(0.0,0.0)*dcosdrw*dsindrw**4-60.0*a(0.0,5.0)*anew(1.0,1.0
     . )*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrw**2*dsindrw**3-20.0*a(0.0
     . ,5.0)*anew(1.0,1.0)*anew(0.0,0.0)*dcosdrw**3*dsindrw**2-20.0*a
     . (0.0,5.0)*anew(1.0,0.0)*anew(0.0,4.0)*anew(0.0,0.0)**3*dsindrw
     . **5-60.0*a(0.0,5.0)*anew(1.0,0.0)*anew(0.0,3.0)*anew(0.0,1.0)*
     . anew(0.0,0.0)**2*dsindrw**5+ans4
      ans2=-4.0*a(1.0,4.0)*anew(0.0,4.0)*anew(0.0,0.0)**3*dsindrw**4-
     . 12.0*a(1.0,4.0)*anew(0.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*
     . dsindrw**4-12.0*a(1.0,4.0)*anew(0.0,3.0)*anew(0.0,0.0)**2*
     . dcosdrw*dsindrw**3-6.0*a(1.0,4.0)*anew(0.0,2.0)**2*anew(0.0,
     . 0.0)**2*dsindrw**4-12.0*a(1.0,4.0)*anew(0.0,2.0)*anew(0.0,1.0)
     . **2*anew(0.0,0.0)*dsindrw**4-24.0*a(1.0,4.0)*anew(0.0,2.0)*
     . anew(0.0,1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**3-12.0*a(1.0,4.0)
     . *anew(0.0,2.0)*anew(0.0,0.0)*dcosdrw**2*dsindrw**2-a(1.0,4.0)*
     . anew(0.0,1.0)**4*dsindrw**4-4.0*a(1.0,4.0)*anew(0.0,1.0)**3*
     . dcosdrw*dsindrw**3-6.0*a(1.0,4.0)*anew(0.0,1.0)**2*dcosdrw**2*
     . dsindrw**2-4.0*a(1.0,4.0)*anew(0.0,1.0)*dcosdrw**3*dsindrw-a(
     . 1.0,4.0)*dcosdrw**4-3.0*a(1.0,3.0)*anew(0.0,4.0)*anew(0.0,0.0)
     . **2*dsindrw**3-6.0*a(1.0,3.0)*anew(0.0,3.0)*anew(0.0,1.0)*anew
     . (0.0,0.0)*dsindrw**3-6.0*a(1.0,3.0)*anew(0.0,3.0)*anew(0.0,0.0
     . )*dcosdrw*dsindrw**2-3.0*a(1.0,3.0)*anew(0.0,2.0)**2*anew(0.0,
     . 0.0)*dsindrw**3-3.0*a(1.0,3.0)*anew(0.0,2.0)*anew(0.0,1.0)**2*
     . dsindrw**3-6.0*a(1.0,3.0)*anew(0.0,2.0)*anew(0.0,1.0)*dcosdrw*
     . dsindrw**2-3.0*a(1.0,3.0)*anew(0.0,2.0)*dcosdrw**2*dsindrw-2.0
     . *a(1.0,2.0)*anew(0.0,4.0)*anew(0.0,0.0)*dsindrw**2-2.0*a(1.0,
     . 2.0)*anew(0.0,3.0)*anew(0.0,1.0)*dsindrw**2+ans3
      ans1=dcosdrw*ans2
      anew(1.0,4.0)=ans1/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      anew(2.0,0.0)=(dcosdrw*(-a(2.0,3.0)*anew(0.0,0.0)**3*dsindrw**3
     . -a(2.0,2.0)*anew(0.0,0.0)**2*dsindrw**2-a(2.0,1.0)*anew(0.0,
     . 0.0)*dsindrw-a(2.0,0.0)-4.0*a(1.0,4.0)*anew(1.0,0.0)*anew(0.0,
     . 0.0)**3*dsindrw**4-3.0*a(1.0,3.0)*anew(1.0,0.0)*anew(0.0,0.0)
     . **2*dsindrw**3-2.0*a(1.0,2.0)*anew(1.0,0.0)*anew(0.0,0.0)*
     . dsindrw**2-a(1.0,1.0)*anew(1.0,0.0)*dsindrw-10.0*a(0.0,5.0)*
     . anew(1.0,0.0)**2*anew(0.0,0.0)**3*dsindrw**5-6.0*a(0.0,4.0)*
     . anew(1.0,0.0)**2*anew(0.0,0.0)**2*dsindrw**4-3.0*a(0.0,3.0)*
     . anew(1.0,0.0)**2*anew(0.0,0.0)*dsindrw**3-a(0.0,2.0)*anew(1.0,
     . 0.0)**2*dsindrw**2))/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      ans3=-12.0*a(0.0,4.0)*anew(2.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **2*dsindrw**4-12.0*a(0.0,4.0)*anew(2.0,0.0)*anew(0.0,0.0)**2*
     . dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(1.0,1.0)*anew(1.0,0.0)
     . *anew(0.0,0.0)**2*dsindrw**4-12.0*a(0.0,4.0)*anew(1.0,0.0)**2*
     . anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**4-12.0*a(0.0,4.0)*anew(
     . 1.0,0.0)**2*anew(0.0,0.0)*dcosdrw*dsindrw**3-6.0*a(0.0,3.0)*
     . anew(2.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(0.0
     . ,3.0)*anew(2.0,0.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2-6.0*a(0.0
     . ,3.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrw**3-3.0
     . *a(0.0,3.0)*anew(1.0,0.0)**2*anew(0.0,1.0)*dsindrw**3-3.0*a(
     . 0.0,3.0)*anew(1.0,0.0)**2*dcosdrw*dsindrw**2-2.0*a(0.0,2.0)*
     . anew(2.0,0.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(0.0,2.0)*anew(2.0
     . ,0.0)*dcosdrw*dsindrw-2.0*a(0.0,2.0)*anew(1.0,1.0)*anew(1.0,
     . 0.0)*dsindrw**2
      ans2=-3.0*a(2.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**3-
     . 3.0*a(2.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**2-2.0*a(2.0,
     . 2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**2-2.0*a(2.0,2.0)*
     . anew(0.0,0.0)*dcosdrw*dsindrw-a(2.0,1.0)*anew(0.0,1.0)*dsindrw
     . -a(2.0,1.0)*dcosdrw-4.0*a(1.0,4.0)*anew(1.0,1.0)*anew(0.0,0.0)
     . **3*dsindrw**4-12.0*a(1.0,4.0)*anew(1.0,0.0)*anew(0.0,1.0)*
     . anew(0.0,0.0)**2*dsindrw**4-12.0*a(1.0,4.0)*anew(1.0,0.0)*anew
     . (0.0,0.0)**2*dcosdrw*dsindrw**3-3.0*a(1.0,3.0)*anew(1.0,1.0)*
     . anew(0.0,0.0)**2*dsindrw**3-6.0*a(1.0,3.0)*anew(1.0,0.0)*anew(
     . 0.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(1.0,3.0)*anew(1.0,0.0)
     . *anew(0.0,0.0)*dcosdrw*dsindrw**2-2.0*a(1.0,2.0)*anew(1.0,1.0)
     . *anew(0.0,0.0)*dsindrw**2-2.0*a(1.0,2.0)*anew(1.0,0.0)*anew(
     . 0.0,1.0)*dsindrw**2-2.0*a(1.0,2.0)*anew(1.0,0.0)*dcosdrw*
     . dsindrw-a(1.0,1.0)*anew(1.0,1.0)*dsindrw-20.0*a(0.0,5.0)*anew(
     . 2.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**3*dsindrw**5-20.0*a(0.0,
     . 5.0)*anew(2.0,0.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4-20.0*a(
     . 0.0,5.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)**3*dsindrw
     . **5-30.0*a(0.0,5.0)*anew(1.0,0.0)**2*anew(0.0,1.0)*anew(0.0,
     . 0.0)**2*dsindrw**5-30.0*a(0.0,5.0)*anew(1.0,0.0)**2*anew(0.0,
     . 0.0)**2*dcosdrw*dsindrw**4+ans3
      ans1=dcosdrw*ans2
      anew(2.0,1.0)=ans1/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      ans5=-6.0*a(0.0,3.0)*anew(2.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)*
     . dsindrw**3-3.0*a(0.0,3.0)*anew(2.0,0.0)*anew(0.0,1.0)**2*
     . dsindrw**3-6.0*a(0.0,3.0)*anew(2.0,0.0)*anew(0.0,1.0)*dcosdrw*
     . dsindrw**2-3.0*a(0.0,3.0)*anew(2.0,0.0)*dcosdrw**2*dsindrw-6.0
     . *a(0.0,3.0)*anew(1.0,2.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrw
     . **3-3.0*a(0.0,3.0)*anew(1.0,1.0)**2*anew(0.0,0.0)*dsindrw**3-
     . 6.0*a(0.0,3.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)*
     . dsindrw**3-6.0*a(0.0,3.0)*anew(1.0,1.0)*anew(1.0,0.0)*dcosdrw*
     . dsindrw**2-3.0*a(0.0,3.0)*anew(1.0,0.0)**2*anew(0.0,2.0)*
     . dsindrw**3-2.0*a(0.0,2.0)*anew(2.0,1.0)*anew(0.0,1.0)*dsindrw
     . **2-2.0*a(0.0,2.0)*anew(2.0,1.0)*dcosdrw*dsindrw-2.0*a(0.0,2.0
     . )*anew(2.0,0.0)*anew(0.0,2.0)*dsindrw**2-2.0*a(0.0,2.0)*anew(
     . 1.0,2.0)*anew(1.0,0.0)*dsindrw**2-a(0.0,2.0)*anew(1.0,1.0)**2*
     . dsindrw**2
      ans4=-60.0*a(0.0,5.0)*anew(1.0,0.0)**2*anew(0.0,1.0)*anew(0.0,
     . 0.0)*dcosdrw*dsindrw**4-30.0*a(0.0,5.0)*anew(1.0,0.0)**2*anew(
     . 0.0,0.0)*dcosdrw**2*dsindrw**3-12.0*a(0.0,4.0)*anew(2.0,1.0)*
     . anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**4-12.0*a(0.0,4.0)*anew
     . (2.0,1.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*
     . anew(2.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*dsindrw**4-12.0*a
     . (0.0,4.0)*anew(2.0,0.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dsindrw
     . **4-24.0*a(0.0,4.0)*anew(2.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*
     . dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(2.0,0.0)*anew(0.0,0.0)
     . *dcosdrw**2*dsindrw**2-12.0*a(0.0,4.0)*anew(1.0,2.0)*anew(1.0,
     . 0.0)*anew(0.0,0.0)**2*dsindrw**4-6.0*a(0.0,4.0)*anew(1.0,1.0)
     . **2*anew(0.0,0.0)**2*dsindrw**4-24.0*a(0.0,4.0)*anew(1.0,1.0)*
     . anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**4-24.0*a(
     . 0.0,4.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrw*
     . dsindrw**3-12.0*a(0.0,4.0)*anew(1.0,0.0)**2*anew(0.0,2.0)*anew
     . (0.0,0.0)*dsindrw**4-6.0*a(0.0,4.0)*anew(1.0,0.0)**2*anew(0.0,
     . 1.0)**2*dsindrw**4-12.0*a(0.0,4.0)*anew(1.0,0.0)**2*anew(0.0,
     . 1.0)*dcosdrw*dsindrw**3-6.0*a(0.0,4.0)*anew(1.0,0.0)**2*
     . dcosdrw**2*dsindrw**2-6.0*a(0.0,3.0)*anew(2.0,1.0)*anew(0.0,
     . 1.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(0.0,3.0)*anew(2.0,1.0)*
     . anew(0.0,0.0)*dcosdrw*dsindrw**2+ans5
      ans3=-6.0*a(1.0,3.0)*anew(1.0,0.0)*anew(0.0,1.0)*dcosdrw*
     . dsindrw**2-3.0*a(1.0,3.0)*anew(1.0,0.0)*dcosdrw**2*dsindrw-2.0
     . *a(1.0,2.0)*anew(1.0,2.0)*anew(0.0,0.0)*dsindrw**2-2.0*a(1.0,
     . 2.0)*anew(1.0,1.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(1.0,2.0)*
     . anew(1.0,1.0)*dcosdrw*dsindrw-2.0*a(1.0,2.0)*anew(1.0,0.0)*
     . anew(0.0,2.0)*dsindrw**2-a(1.0,1.0)*anew(1.0,2.0)*dsindrw-20.0
     . *a(0.0,5.0)*anew(2.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)**3*
     . dsindrw**5-20.0*a(0.0,5.0)*anew(2.0,1.0)*anew(0.0,0.0)**3*
     . dcosdrw*dsindrw**4-20.0*a(0.0,5.0)*anew(2.0,0.0)*anew(0.0,2.0)
     . *anew(0.0,0.0)**3*dsindrw**5-30.0*a(0.0,5.0)*anew(2.0,0.0)*
     . anew(0.0,1.0)**2*anew(0.0,0.0)**2*dsindrw**5-60.0*a(0.0,5.0)*
     . anew(2.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**
     . 4-30.0*a(0.0,5.0)*anew(2.0,0.0)*anew(0.0,0.0)**2*dcosdrw**2*
     . dsindrw**3-20.0*a(0.0,5.0)*anew(1.0,2.0)*anew(1.0,0.0)*anew(
     . 0.0,0.0)**3*dsindrw**5-10.0*a(0.0,5.0)*anew(1.0,1.0)**2*anew(
     . 0.0,0.0)**3*dsindrw**5-60.0*a(0.0,5.0)*anew(1.0,1.0)*anew(1.0,
     . 0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**5-60.0*a(0.0,5.0)
     . *anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw
     . **4-30.0*a(0.0,5.0)*anew(1.0,0.0)**2*anew(0.0,2.0)*anew(0.0,
     . 0.0)**2*dsindrw**5-30.0*a(0.0,5.0)*anew(1.0,0.0)**2*anew(0.0,
     . 1.0)**2*anew(0.0,0.0)*dsindrw**5+ans4
      ans2=-3.0*a(2.0,3.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*dsindrw**3-
     . 3.0*a(2.0,3.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dsindrw**3-6.0*a
     . (2.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2-3.0*a
     . (2.0,3.0)*anew(0.0,0.0)*dcosdrw**2*dsindrw-2.0*a(2.0,2.0)*anew
     . (0.0,2.0)*anew(0.0,0.0)*dsindrw**2-a(2.0,2.0)*anew(0.0,1.0)**2
     . *dsindrw**2-2.0*a(2.0,2.0)*anew(0.0,1.0)*dcosdrw*dsindrw-a(2.0
     . ,2.0)*dcosdrw**2-a(2.0,1.0)*anew(0.0,2.0)*dsindrw-4.0*a(1.0,
     . 4.0)*anew(1.0,2.0)*anew(0.0,0.0)**3*dsindrw**4-12.0*a(1.0,4.0)
     . *anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**4-12.0*
     . a(1.0,4.0)*anew(1.0,1.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3-
     . 12.0*a(1.0,4.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*
     . dsindrw**4-12.0*a(1.0,4.0)*anew(1.0,0.0)*anew(0.0,1.0)**2*anew
     . (0.0,0.0)*dsindrw**4-24.0*a(1.0,4.0)*anew(1.0,0.0)*anew(0.0,
     . 1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**3-12.0*a(1.0,4.0)*anew(1.0
     . ,0.0)*anew(0.0,0.0)*dcosdrw**2*dsindrw**2-3.0*a(1.0,3.0)*anew(
     . 1.0,2.0)*anew(0.0,0.0)**2*dsindrw**3-6.0*a(1.0,3.0)*anew(1.0,
     . 1.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(1.0,3.0)*
     . anew(1.0,1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2-6.0*a(1.0,3.0)*
     . anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrw**3-3.0*a(1.0
     . ,3.0)*anew(1.0,0.0)*anew(0.0,1.0)**2*dsindrw**3+ans3
      ans1=dcosdrw*ans2
      anew(2.0,2.0)=ans1/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      ans8=-3.0*a(0.0,3.0)*anew(1.0,1.0)**2*dcosdrw*dsindrw**2-6.0*a(
     . 0.0,3.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,2.0)*dsindrw**3-
     . 3.0*a(0.0,3.0)*anew(1.0,0.0)**2*anew(0.0,3.0)*dsindrw**3-2.0*a
     . (0.0,2.0)*anew(2.0,2.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(0.0,2.0
     . )*anew(2.0,2.0)*dcosdrw*dsindrw-2.0*a(0.0,2.0)*anew(2.0,1.0)*
     . anew(0.0,2.0)*dsindrw**2-2.0*a(0.0,2.0)*anew(2.0,0.0)*anew(0.0
     . ,3.0)*dsindrw**2-2.0*a(0.0,2.0)*anew(1.0,3.0)*anew(1.0,0.0)*
     . dsindrw**2-2.0*a(0.0,2.0)*anew(1.0,2.0)*anew(1.0,1.0)*dsindrw
     . **2
      ans7=-24.0*a(0.0,4.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)
     . *dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(1.0,1.0)*anew(1.0,0.0
     . )*dcosdrw**2*dsindrw**2-12.0*a(0.0,4.0)*anew(1.0,0.0)**2*anew(
     . 0.0,3.0)*anew(0.0,0.0)*dsindrw**4-12.0*a(0.0,4.0)*anew(1.0,0.0
     . )**2*anew(0.0,2.0)*anew(0.0,1.0)*dsindrw**4-12.0*a(0.0,4.0)*
     . anew(1.0,0.0)**2*anew(0.0,2.0)*dcosdrw*dsindrw**3-6.0*a(0.0,
     . 3.0)*anew(2.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0*
     . a(0.0,3.0)*anew(2.0,2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2-6.0*
     . a(0.0,3.0)*anew(2.0,1.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrw**
     . 3-3.0*a(0.0,3.0)*anew(2.0,1.0)*anew(0.0,1.0)**2*dsindrw**3-6.0
     . *a(0.0,3.0)*anew(2.0,1.0)*anew(0.0,1.0)*dcosdrw*dsindrw**2-3.0
     . *a(0.0,3.0)*anew(2.0,1.0)*dcosdrw**2*dsindrw-6.0*a(0.0,3.0)*
     . anew(2.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(0.0
     . ,3.0)*anew(2.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*dsindrw**3-6.0
     . *a(0.0,3.0)*anew(2.0,0.0)*anew(0.0,2.0)*dcosdrw*dsindrw**2-6.0
     . *a(0.0,3.0)*anew(1.0,3.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrw
     . **3-6.0*a(0.0,3.0)*anew(1.0,2.0)*anew(1.0,1.0)*anew(0.0,0.0)*
     . dsindrw**3-6.0*a(0.0,3.0)*anew(1.0,2.0)*anew(1.0,0.0)*anew(0.0
     . ,1.0)*dsindrw**3-6.0*a(0.0,3.0)*anew(1.0,2.0)*anew(1.0,0.0)*
     . dcosdrw*dsindrw**2-3.0*a(0.0,3.0)*anew(1.0,1.0)**2*anew(0.0,
     . 1.0)*dsindrw**3+ans8
      ans6=-12.0*a(0.0,4.0)*anew(2.0,1.0)*anew(0.0,1.0)**2*anew(0.0,
     . 0.0)*dsindrw**4-24.0*a(0.0,4.0)*anew(2.0,1.0)*anew(0.0,1.0)*
     . anew(0.0,0.0)*dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(2.0,1.0)
     . *anew(0.0,0.0)*dcosdrw**2*dsindrw**2-12.0*a(0.0,4.0)*anew(2.0,
     . 0.0)*anew(0.0,3.0)*anew(0.0,0.0)**2*dsindrw**4-24.0*a(0.0,4.0)
     . *anew(2.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*
     . dsindrw**4-24.0*a(0.0,4.0)*anew(2.0,0.0)*anew(0.0,2.0)*anew(
     . 0.0,0.0)*dcosdrw*dsindrw**3-4.0*a(0.0,4.0)*anew(2.0,0.0)*anew(
     . 0.0,1.0)**3*dsindrw**4-12.0*a(0.0,4.0)*anew(2.0,0.0)*anew(0.0,
     . 1.0)**2*dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(2.0,0.0)*anew(
     . 0.0,1.0)*dcosdrw**2*dsindrw**2-4.0*a(0.0,4.0)*anew(2.0,0.0)*
     . dcosdrw**3*dsindrw-12.0*a(0.0,4.0)*anew(1.0,3.0)*anew(1.0,0.0)
     . *anew(0.0,0.0)**2*dsindrw**4-12.0*a(0.0,4.0)*anew(1.0,2.0)*
     . anew(1.0,1.0)*anew(0.0,0.0)**2*dsindrw**4-24.0*a(0.0,4.0)*anew
     . (1.0,2.0)*anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**4
     . -24.0*a(0.0,4.0)*anew(1.0,2.0)*anew(1.0,0.0)*anew(0.0,0.0)*
     . dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(1.0,1.0)**2*anew(0.0,
     . 1.0)*anew(0.0,0.0)*dsindrw**4-12.0*a(0.0,4.0)*anew(1.0,1.0)**2
     . *anew(0.0,0.0)*dcosdrw*dsindrw**3-24.0*a(0.0,4.0)*anew(1.0,1.0
     . )*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrw**4-12.0*a(
     . 0.0,4.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)**2*dsindrw
     . **4+ans7
      ans5=-30.0*a(0.0,5.0)*anew(1.0,1.0)**2*anew(0.0,1.0)*anew(0.0,
     . 0.0)**2*dsindrw**5-30.0*a(0.0,5.0)*anew(1.0,1.0)**2*anew(0.0,
     . 0.0)**2*dcosdrw*dsindrw**4-60.0*a(0.0,5.0)*anew(1.0,1.0)*anew(
     . 1.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*dsindrw**5-60.0*a(0.0,
     . 5.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)**2*anew(0.0,0.0
     . )*dsindrw**5-120.0*a(0.0,5.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew
     . (0.0,1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**4-60.0*a(0.0,5.0)*
     . anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrw**2*dsindrw**
     . 3-30.0*a(0.0,5.0)*anew(1.0,0.0)**2*anew(0.0,3.0)*anew(0.0,0.0)
     . **2*dsindrw**5-60.0*a(0.0,5.0)*anew(1.0,0.0)**2*anew(0.0,2.0)*
     . anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**5-60.0*a(0.0,5.0)*anew(
     . 1.0,0.0)**2*anew(0.0,2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**4-
     . 10.0*a(0.0,5.0)*anew(1.0,0.0)**2*anew(0.0,1.0)**3*dsindrw**5-
     . 30.0*a(0.0,5.0)*anew(1.0,0.0)**2*anew(0.0,1.0)**2*dcosdrw*
     . dsindrw**4-30.0*a(0.0,5.0)*anew(1.0,0.0)**2*anew(0.0,1.0)*
     . dcosdrw**2*dsindrw**3-10.0*a(0.0,5.0)*anew(1.0,0.0)**2*dcosdrw
     . **3*dsindrw**2-12.0*a(0.0,4.0)*anew(2.0,2.0)*anew(0.0,1.0)*
     . anew(0.0,0.0)**2*dsindrw**4-12.0*a(0.0,4.0)*anew(2.0,2.0)*anew
     . (0.0,0.0)**2*dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(2.0,1.0)*
     . anew(0.0,2.0)*anew(0.0,0.0)**2*dsindrw**4+ans6
      ans4=-20.0*a(0.0,5.0)*anew(2.0,2.0)*anew(0.0,0.0)**3*dcosdrw*
     . dsindrw**4-20.0*a(0.0,5.0)*anew(2.0,1.0)*anew(0.0,2.0)*anew(
     . 0.0,0.0)**3*dsindrw**5-30.0*a(0.0,5.0)*anew(2.0,1.0)*anew(0.0,
     . 1.0)**2*anew(0.0,0.0)**2*dsindrw**5-60.0*a(0.0,5.0)*anew(2.0,
     . 1.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**4-30.0*a(
     . 0.0,5.0)*anew(2.0,1.0)*anew(0.0,0.0)**2*dcosdrw**2*dsindrw**3-
     . 20.0*a(0.0,5.0)*anew(2.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)**3*
     . dsindrw**5-60.0*a(0.0,5.0)*anew(2.0,0.0)*anew(0.0,2.0)*anew(
     . 0.0,1.0)*anew(0.0,0.0)**2*dsindrw**5-60.0*a(0.0,5.0)*anew(2.0,
     . 0.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**4-20.0*a(
     . 0.0,5.0)*anew(2.0,0.0)*anew(0.0,1.0)**3*anew(0.0,0.0)*dsindrw
     . **5-60.0*a(0.0,5.0)*anew(2.0,0.0)*anew(0.0,1.0)**2*anew(0.0,
     . 0.0)*dcosdrw*dsindrw**4-60.0*a(0.0,5.0)*anew(2.0,0.0)*anew(0.0
     . ,1.0)*anew(0.0,0.0)*dcosdrw**2*dsindrw**3-20.0*a(0.0,5.0)*anew
     . (2.0,0.0)*anew(0.0,0.0)*dcosdrw**3*dsindrw**2-20.0*a(0.0,5.0)*
     . anew(1.0,3.0)*anew(1.0,0.0)*anew(0.0,0.0)**3*dsindrw**5-20.0*a
     . (0.0,5.0)*anew(1.0,2.0)*anew(1.0,1.0)*anew(0.0,0.0)**3*dsindrw
     . **5-60.0*a(0.0,5.0)*anew(1.0,2.0)*anew(1.0,0.0)*anew(0.0,1.0)*
     . anew(0.0,0.0)**2*dsindrw**5-60.0*a(0.0,5.0)*anew(1.0,2.0)*anew
     . (1.0,0.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**4+ans5
      ans3=-4.0*a(1.0,4.0)*anew(1.0,0.0)*anew(0.0,1.0)**3*dsindrw**4-
     . 12.0*a(1.0,4.0)*anew(1.0,0.0)*anew(0.0,1.0)**2*dcosdrw*dsindrw
     . **3-12.0*a(1.0,4.0)*anew(1.0,0.0)*anew(0.0,1.0)*dcosdrw**2*
     . dsindrw**2-4.0*a(1.0,4.0)*anew(1.0,0.0)*dcosdrw**3*dsindrw-3.0
     . *a(1.0,3.0)*anew(1.0,3.0)*anew(0.0,0.0)**2*dsindrw**3-6.0*a(
     . 1.0,3.0)*anew(1.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**3-
     . 6.0*a(1.0,3.0)*anew(1.0,2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2-
     . 6.0*a(1.0,3.0)*anew(1.0,1.0)*anew(0.0,2.0)*anew(0.0,0.0)*
     . dsindrw**3-3.0*a(1.0,3.0)*anew(1.0,1.0)*anew(0.0,1.0)**2*
     . dsindrw**3-6.0*a(1.0,3.0)*anew(1.0,1.0)*anew(0.0,1.0)*dcosdrw*
     . dsindrw**2-3.0*a(1.0,3.0)*anew(1.0,1.0)*dcosdrw**2*dsindrw-6.0
     . *a(1.0,3.0)*anew(1.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)*dsindrw
     . **3-6.0*a(1.0,3.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*
     . dsindrw**3-6.0*a(1.0,3.0)*anew(1.0,0.0)*anew(0.0,2.0)*dcosdrw*
     . dsindrw**2-2.0*a(1.0,2.0)*anew(1.0,3.0)*anew(0.0,0.0)*dsindrw
     . **2-2.0*a(1.0,2.0)*anew(1.0,2.0)*anew(0.0,1.0)*dsindrw**2-2.0*
     . a(1.0,2.0)*anew(1.0,2.0)*dcosdrw*dsindrw-2.0*a(1.0,2.0)*anew(
     . 1.0,1.0)*anew(0.0,2.0)*dsindrw**2-2.0*a(1.0,2.0)*anew(1.0,0.0)
     . *anew(0.0,3.0)*dsindrw**2-a(1.0,1.0)*anew(1.0,3.0)*dsindrw-
     . 20.0*a(0.0,5.0)*anew(2.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)**3*
     . dsindrw**5+ans4
      ans2=-3.0*a(2.0,3.0)*anew(0.0,3.0)*anew(0.0,0.0)**2*dsindrw**3-
     . 6.0*a(2.0,3.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*
     . dsindrw**3-6.0*a(2.0,3.0)*anew(0.0,2.0)*anew(0.0,0.0)*dcosdrw*
     . dsindrw**2-a(2.0,3.0)*anew(0.0,1.0)**3*dsindrw**3-3.0*a(2.0,
     . 3.0)*anew(0.0,1.0)**2*dcosdrw*dsindrw**2-3.0*a(2.0,3.0)*anew(
     . 0.0,1.0)*dcosdrw**2*dsindrw-a(2.0,3.0)*dcosdrw**3-2.0*a(2.0,
     . 2.0)*anew(0.0,3.0)*anew(0.0,0.0)*dsindrw**2-2.0*a(2.0,2.0)*
     . anew(0.0,2.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(2.0,2.0)*anew(0.0
     . ,2.0)*dcosdrw*dsindrw-a(2.0,1.0)*anew(0.0,3.0)*dsindrw-4.0*a(
     . 1.0,4.0)*anew(1.0,3.0)*anew(0.0,0.0)**3*dsindrw**4-12.0*a(1.0,
     . 4.0)*anew(1.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**4-
     . 12.0*a(1.0,4.0)*anew(1.0,2.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw
     . **3-12.0*a(1.0,4.0)*anew(1.0,1.0)*anew(0.0,2.0)*anew(0.0,0.0)
     . **2*dsindrw**4-12.0*a(1.0,4.0)*anew(1.0,1.0)*anew(0.0,1.0)**2*
     . anew(0.0,0.0)*dsindrw**4-24.0*a(1.0,4.0)*anew(1.0,1.0)*anew(
     . 0.0,1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**3-12.0*a(1.0,4.0)*anew
     . (1.0,1.0)*anew(0.0,0.0)*dcosdrw**2*dsindrw**2-12.0*a(1.0,4.0)*
     . anew(1.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)**2*dsindrw**4-24.0*a
     . (1.0,4.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0,
     . 0.0)*dsindrw**4-24.0*a(1.0,4.0)*anew(1.0,0.0)*anew(0.0,2.0)*
     . anew(0.0,0.0)*dcosdrw*dsindrw**3+ans3
      ans1=dcosdrw*ans2
      anew(2.0,3.0)=ans1/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      ans1=dcosdrw*(-a(3.0,2.0)*anew(0.0,0.0)**2*dsindrw**2-a(3.0,1.0
     . )*anew(0.0,0.0)*dsindrw-a(3.0,0.0)-3.0*a(2.0,3.0)*anew(1.0,0.0
     . )*anew(0.0,0.0)**2*dsindrw**3-2.0*a(2.0,2.0)*anew(1.0,0.0)*
     . anew(0.0,0.0)*dsindrw**2-a(2.0,1.0)*anew(1.0,0.0)*dsindrw-4.0*
     . a(1.0,4.0)*anew(2.0,0.0)*anew(0.0,0.0)**3*dsindrw**4-6.0*a(1.0
     . ,4.0)*anew(1.0,0.0)**2*anew(0.0,0.0)**2*dsindrw**4-3.0*a(1.0,
     . 3.0)*anew(2.0,0.0)*anew(0.0,0.0)**2*dsindrw**3-3.0*a(1.0,3.0)*
     . anew(1.0,0.0)**2*anew(0.0,0.0)*dsindrw**3-2.0*a(1.0,2.0)*anew(
     . 2.0,0.0)*anew(0.0,0.0)*dsindrw**2-a(1.0,2.0)*anew(1.0,0.0)**2*
     . dsindrw**2-a(1.0,1.0)*anew(2.0,0.0)*dsindrw-20.0*a(0.0,5.0)*
     . anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)**3*dsindrw**5-10.0*a
     . (0.0,5.0)*anew(1.0,0.0)**3*anew(0.0,0.0)**2*dsindrw**5-12.0*a(
     . 0.0,4.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dsindrw
     . **4-4.0*a(0.0,4.0)*anew(1.0,0.0)**3*anew(0.0,0.0)*dsindrw**4-
     . 6.0*a(0.0,3.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)*
     . dsindrw**3-a(0.0,3.0)*anew(1.0,0.0)**3*dsindrw**3-2.0*a(0.0,
     . 2.0)*anew(2.0,0.0)*anew(1.0,0.0)*dsindrw**2)
      anew(3.0,0.0)=ans1/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      ans4=-24.0*a(0.0,4.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0)
     . *anew(0.0,0.0)*dsindrw**4-24.0*a(0.0,4.0)*anew(2.0,0.0)*anew(
     . 1.0,0.0)*anew(0.0,0.0)*dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew
     . (1.0,1.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*dsindrw**4-4.0*a(0.0,
     . 4.0)*anew(1.0,0.0)**3*anew(0.0,1.0)*dsindrw**4-4.0*a(0.0,4.0)*
     . anew(1.0,0.0)**3*dcosdrw*dsindrw**3-6.0*a(0.0,3.0)*anew(3.0,
     . 0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(0.0,3.0)*
     . anew(3.0,0.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2-6.0*a(0.0,3.0)*
     . anew(2.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(0.0
     . ,3.0)*anew(2.0,0.0)*anew(1.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0
     . *a(0.0,3.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0)*dsindrw
     . **3-6.0*a(0.0,3.0)*anew(2.0,0.0)*anew(1.0,0.0)*dcosdrw*dsindrw
     . **2-3.0*a(0.0,3.0)*anew(1.0,1.0)*anew(1.0,0.0)**2*dsindrw**3-
     . 2.0*a(0.0,2.0)*anew(3.0,0.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(
     . 0.0,2.0)*anew(3.0,0.0)*dcosdrw*dsindrw-2.0*a(0.0,2.0)*anew(2.0
     . ,1.0)*anew(1.0,0.0)*dsindrw**2-2.0*a(0.0,2.0)*anew(2.0,0.0)*
     . anew(1.0,1.0)*dsindrw**2
      ans3=-3.0*a(1.0,3.0)*anew(1.0,0.0)**2*dcosdrw*dsindrw**2-2.0*a(
     . 1.0,2.0)*anew(2.0,1.0)*anew(0.0,0.0)*dsindrw**2-2.0*a(1.0,2.0)
     . *anew(2.0,0.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(1.0,2.0)*anew(
     . 2.0,0.0)*dcosdrw*dsindrw-2.0*a(1.0,2.0)*anew(1.0,1.0)*anew(1.0
     . ,0.0)*dsindrw**2-a(1.0,1.0)*anew(2.0,1.0)*dsindrw-20.0*a(0.0,
     . 5.0)*anew(3.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**3*dsindrw**5-
     . 20.0*a(0.0,5.0)*anew(3.0,0.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw
     . **4-20.0*a(0.0,5.0)*anew(2.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)
     . **3*dsindrw**5-20.0*a(0.0,5.0)*anew(2.0,0.0)*anew(1.0,1.0)*
     . anew(0.0,0.0)**3*dsindrw**5-60.0*a(0.0,5.0)*anew(2.0,0.0)*anew
     . (1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**5-60.0*a(0.0
     . ,5.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dcosdrw*
     . dsindrw**4-30.0*a(0.0,5.0)*anew(1.0,1.0)*anew(1.0,0.0)**2*anew
     . (0.0,0.0)**2*dsindrw**5-20.0*a(0.0,5.0)*anew(1.0,0.0)**3*anew(
     . 0.0,1.0)*anew(0.0,0.0)*dsindrw**5-20.0*a(0.0,5.0)*anew(1.0,0.0
     . )**3*anew(0.0,0.0)*dcosdrw*dsindrw**4-12.0*a(0.0,4.0)*anew(3.0
     . ,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**4-12.0*a(0.0,4.0
     . )*anew(3.0,0.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3-12.0*a(0.0
     . ,4.0)*anew(2.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dsindrw**4-
     . 12.0*a(0.0,4.0)*anew(2.0,0.0)*anew(1.0,1.0)*anew(0.0,0.0)**2*
     . dsindrw**4+ans4
      ans2=-2.0*a(3.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**2-2.0
     . *a(3.0,2.0)*anew(0.0,0.0)*dcosdrw*dsindrw-a(3.0,1.0)*anew(0.0,
     . 1.0)*dsindrw-a(3.0,1.0)*dcosdrw-3.0*a(2.0,3.0)*anew(1.0,1.0)*
     . anew(0.0,0.0)**2*dsindrw**3-6.0*a(2.0,3.0)*anew(1.0,0.0)*anew(
     . 0.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(2.0,3.0)*anew(1.0,0.0)
     . *anew(0.0,0.0)*dcosdrw*dsindrw**2-2.0*a(2.0,2.0)*anew(1.0,1.0)
     . *anew(0.0,0.0)*dsindrw**2-2.0*a(2.0,2.0)*anew(1.0,0.0)*anew(
     . 0.0,1.0)*dsindrw**2-2.0*a(2.0,2.0)*anew(1.0,0.0)*dcosdrw*
     . dsindrw-a(2.0,1.0)*anew(1.0,1.0)*dsindrw-4.0*a(1.0,4.0)*anew(
     . 2.0,1.0)*anew(0.0,0.0)**3*dsindrw**4-12.0*a(1.0,4.0)*anew(2.0,
     . 0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**4-12.0*a(1.0,4.0)
     . *anew(2.0,0.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3-12.0*a(1.0,
     . 4.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dsindrw**4-
     . 12.0*a(1.0,4.0)*anew(1.0,0.0)**2*anew(0.0,1.0)*anew(0.0,0.0)*
     . dsindrw**4-12.0*a(1.0,4.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*
     . dcosdrw*dsindrw**3-3.0*a(1.0,3.0)*anew(2.0,1.0)*anew(0.0,0.0)
     . **2*dsindrw**3-6.0*a(1.0,3.0)*anew(2.0,0.0)*anew(0.0,1.0)*anew
     . (0.0,0.0)*dsindrw**3-6.0*a(1.0,3.0)*anew(2.0,0.0)*anew(0.0,0.0
     . )*dcosdrw*dsindrw**2-6.0*a(1.0,3.0)*anew(1.0,1.0)*anew(1.0,0.0
     . )*anew(0.0,0.0)*dsindrw**3-3.0*a(1.0,3.0)*anew(1.0,0.0)**2*
     . anew(0.0,1.0)*dsindrw**3+ans3
      ans1=dcosdrw*ans2
      anew(3.0,1.0)=ans1/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      ans8=-3.0*a(0.0,3.0)*anew(1.0,2.0)*anew(1.0,0.0)**2*dsindrw**3-
     . 3.0*a(0.0,3.0)*anew(1.0,1.0)**2*anew(1.0,0.0)*dsindrw**3-2.0*a
     . (0.0,2.0)*anew(3.0,1.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(0.0,2.0
     . )*anew(3.0,1.0)*dcosdrw*dsindrw-2.0*a(0.0,2.0)*anew(3.0,0.0)*
     . anew(0.0,2.0)*dsindrw**2-2.0*a(0.0,2.0)*anew(2.0,2.0)*anew(1.0
     . ,0.0)*dsindrw**2-2.0*a(0.0,2.0)*anew(2.0,1.0)*anew(1.0,1.0)*
     . dsindrw**2-2.0*a(0.0,2.0)*anew(2.0,0.0)*anew(1.0,2.0)*dsindrw
     . **2
      ans7=-12.0*a(0.0,4.0)*anew(1.0,2.0)*anew(1.0,0.0)**2*anew(0.0,
     . 0.0)*dsindrw**4-12.0*a(0.0,4.0)*anew(1.0,1.0)**2*anew(1.0,0.0)
     . *anew(0.0,0.0)*dsindrw**4-12.0*a(0.0,4.0)*anew(1.0,1.0)*anew(
     . 1.0,0.0)**2*anew(0.0,1.0)*dsindrw**4-12.0*a(0.0,4.0)*anew(1.0,
     . 1.0)*anew(1.0,0.0)**2*dcosdrw*dsindrw**3-4.0*a(0.0,4.0)*anew(
     . 1.0,0.0)**3*anew(0.0,2.0)*dsindrw**4-6.0*a(0.0,3.0)*anew(3.0,
     . 1.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(0.0,3.0)*
     . anew(3.0,1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2-6.0*a(0.0,3.0)*
     . anew(3.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrw**3-3.0*a(0.0
     . ,3.0)*anew(3.0,0.0)*anew(0.0,1.0)**2*dsindrw**3-6.0*a(0.0,3.0)
     . *anew(3.0,0.0)*anew(0.0,1.0)*dcosdrw*dsindrw**2-3.0*a(0.0,3.0)
     . *anew(3.0,0.0)*dcosdrw**2*dsindrw-6.0*a(0.0,3.0)*anew(2.0,2.0)
     . *anew(1.0,0.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(0.0,3.0)*anew(
     . 2.0,1.0)*anew(1.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(0.0,3.0)
     . *anew(2.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)*dsindrw**3-6.0*a(
     . 0.0,3.0)*anew(2.0,1.0)*anew(1.0,0.0)*dcosdrw*dsindrw**2-6.0*a(
     . 0.0,3.0)*anew(2.0,0.0)*anew(1.0,2.0)*anew(0.0,0.0)*dsindrw**3-
     . 6.0*a(0.0,3.0)*anew(2.0,0.0)*anew(1.0,1.0)*anew(0.0,1.0)*
     . dsindrw**3-6.0*a(0.0,3.0)*anew(2.0,0.0)*anew(1.0,1.0)*dcosdrw*
     . dsindrw**2-6.0*a(0.0,3.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0
     . ,2.0)*dsindrw**3+ans8
      ans6=-12.0*a(0.0,4.0)*anew(3.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **2*dsindrw**4-12.0*a(0.0,4.0)*anew(3.0,1.0)*anew(0.0,0.0)**2*
     . dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(3.0,0.0)*anew(0.0,2.0)
     . *anew(0.0,0.0)**2*dsindrw**4-12.0*a(0.0,4.0)*anew(3.0,0.0)*
     . anew(0.0,1.0)**2*anew(0.0,0.0)*dsindrw**4-24.0*a(0.0,4.0)*anew
     . (3.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**3-12.0*
     . a(0.0,4.0)*anew(3.0,0.0)*anew(0.0,0.0)*dcosdrw**2*dsindrw**2-
     . 12.0*a(0.0,4.0)*anew(2.0,2.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*
     . dsindrw**4-12.0*a(0.0,4.0)*anew(2.0,1.0)*anew(1.0,1.0)*anew(
     . 0.0,0.0)**2*dsindrw**4-24.0*a(0.0,4.0)*anew(2.0,1.0)*anew(1.0,
     . 0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**4-24.0*a(0.0,4.0)*
     . anew(2.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrw*dsindrw**3-
     . 12.0*a(0.0,4.0)*anew(2.0,0.0)*anew(1.0,2.0)*anew(0.0,0.0)**2*
     . dsindrw**4-24.0*a(0.0,4.0)*anew(2.0,0.0)*anew(1.0,1.0)*anew(
     . 0.0,1.0)*anew(0.0,0.0)*dsindrw**4-24.0*a(0.0,4.0)*anew(2.0,0.0
     . )*anew(1.0,1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**3-24.0*a(0.0,
     . 4.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)*
     . dsindrw**4-12.0*a(0.0,4.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(
     . 0.0,1.0)**2*dsindrw**4-24.0*a(0.0,4.0)*anew(2.0,0.0)*anew(1.0,
     . 0.0)*anew(0.0,1.0)*dcosdrw*dsindrw**3-12.0*a(0.0,4.0)*anew(2.0
     . ,0.0)*anew(1.0,0.0)*dcosdrw**2*dsindrw**2+ans7
      ans5=-60.0*a(0.0,5.0)*anew(2.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)
     . **2*dcosdrw*dsindrw**4-20.0*a(0.0,5.0)*anew(2.0,0.0)*anew(1.0,
     . 2.0)*anew(0.0,0.0)**3*dsindrw**5-60.0*a(0.0,5.0)*anew(2.0,0.0)
     . *anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**5-60.0*
     . a(0.0,5.0)*anew(2.0,0.0)*anew(1.0,1.0)*anew(0.0,0.0)**2*
     . dcosdrw*dsindrw**4-60.0*a(0.0,5.0)*anew(2.0,0.0)*anew(1.0,0.0)
     . *anew(0.0,2.0)*anew(0.0,0.0)**2*dsindrw**5-60.0*a(0.0,5.0)*
     . anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*
     . dsindrw**5-120.0*a(0.0,5.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(
     . 0.0,1.0)*anew(0.0,0.0)*dcosdrw*dsindrw**4-60.0*a(0.0,5.0)*anew
     . (2.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrw**2*dsindrw**3-
     . 30.0*a(0.0,5.0)*anew(1.0,2.0)*anew(1.0,0.0)**2*anew(0.0,0.0)**
     . 2*dsindrw**5-30.0*a(0.0,5.0)*anew(1.0,1.0)**2*anew(1.0,0.0)*
     . anew(0.0,0.0)**2*dsindrw**5-60.0*a(0.0,5.0)*anew(1.0,1.0)*anew
     . (1.0,0.0)**2*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**5-60.0*a(0.0
     . ,5.0)*anew(1.0,1.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*dcosdrw*
     . dsindrw**4-20.0*a(0.0,5.0)*anew(1.0,0.0)**3*anew(0.0,2.0)*anew
     . (0.0,0.0)*dsindrw**5-10.0*a(0.0,5.0)*anew(1.0,0.0)**3*anew(0.0
     . ,1.0)**2*dsindrw**5-20.0*a(0.0,5.0)*anew(1.0,0.0)**3*anew(0.0,
     . 1.0)*dcosdrw*dsindrw**4-10.0*a(0.0,5.0)*anew(1.0,0.0)**3*
     . dcosdrw**2*dsindrw**3+ans6
      ans4=-6.0*a(1.0,3.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)*
     . dsindrw**3-6.0*a(1.0,3.0)*anew(1.0,1.0)*anew(1.0,0.0)*dcosdrw*
     . dsindrw**2-3.0*a(1.0,3.0)*anew(1.0,0.0)**2*anew(0.0,2.0)*
     . dsindrw**3-2.0*a(1.0,2.0)*anew(2.0,2.0)*anew(0.0,0.0)*dsindrw
     . **2-2.0*a(1.0,2.0)*anew(2.0,1.0)*anew(0.0,1.0)*dsindrw**2-2.0*
     . a(1.0,2.0)*anew(2.0,1.0)*dcosdrw*dsindrw-2.0*a(1.0,2.0)*anew(
     . 2.0,0.0)*anew(0.0,2.0)*dsindrw**2-2.0*a(1.0,2.0)*anew(1.0,2.0)
     . *anew(1.0,0.0)*dsindrw**2-a(1.0,2.0)*anew(1.0,1.0)**2*dsindrw
     . **2-a(1.0,1.0)*anew(2.0,2.0)*dsindrw-20.0*a(0.0,5.0)*anew(3.0,
     . 1.0)*anew(0.0,1.0)*anew(0.0,0.0)**3*dsindrw**5-20.0*a(0.0,5.0)
     . *anew(3.0,1.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4-20.0*a(0.0,
     . 5.0)*anew(3.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)**3*dsindrw**5-
     . 30.0*a(0.0,5.0)*anew(3.0,0.0)*anew(0.0,1.0)**2*anew(0.0,0.0)**
     . 2*dsindrw**5-60.0*a(0.0,5.0)*anew(3.0,0.0)*anew(0.0,1.0)*anew(
     . 0.0,0.0)**2*dcosdrw*dsindrw**4-30.0*a(0.0,5.0)*anew(3.0,0.0)*
     . anew(0.0,0.0)**2*dcosdrw**2*dsindrw**3-20.0*a(0.0,5.0)*anew(
     . 2.0,2.0)*anew(1.0,0.0)*anew(0.0,0.0)**3*dsindrw**5-20.0*a(0.0,
     . 5.0)*anew(2.0,1.0)*anew(1.0,1.0)*anew(0.0,0.0)**3*dsindrw**5-
     . 60.0*a(0.0,5.0)*anew(2.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)*anew
     . (0.0,0.0)**2*dsindrw**5+ans5
      ans3=-24.0*a(1.0,4.0)*anew(2.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . *dcosdrw*dsindrw**3-12.0*a(1.0,4.0)*anew(2.0,0.0)*anew(0.0,0.0
     . )*dcosdrw**2*dsindrw**2-12.0*a(1.0,4.0)*anew(1.0,2.0)*anew(1.0
     . ,0.0)*anew(0.0,0.0)**2*dsindrw**4-6.0*a(1.0,4.0)*anew(1.0,1.0)
     . **2*anew(0.0,0.0)**2*dsindrw**4-24.0*a(1.0,4.0)*anew(1.0,1.0)*
     . anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**4-24.0*a(
     . 1.0,4.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrw*
     . dsindrw**3-12.0*a(1.0,4.0)*anew(1.0,0.0)**2*anew(0.0,2.0)*anew
     . (0.0,0.0)*dsindrw**4-6.0*a(1.0,4.0)*anew(1.0,0.0)**2*anew(0.0,
     . 1.0)**2*dsindrw**4-12.0*a(1.0,4.0)*anew(1.0,0.0)**2*anew(0.0,
     . 1.0)*dcosdrw*dsindrw**3-6.0*a(1.0,4.0)*anew(1.0,0.0)**2*
     . dcosdrw**2*dsindrw**2-3.0*a(1.0,3.0)*anew(2.0,2.0)*anew(0.0,
     . 0.0)**2*dsindrw**3-6.0*a(1.0,3.0)*anew(2.0,1.0)*anew(0.0,1.0)*
     . anew(0.0,0.0)*dsindrw**3-6.0*a(1.0,3.0)*anew(2.0,1.0)*anew(0.0
     . ,0.0)*dcosdrw*dsindrw**2-6.0*a(1.0,3.0)*anew(2.0,0.0)*anew(0.0
     . ,2.0)*anew(0.0,0.0)*dsindrw**3-3.0*a(1.0,3.0)*anew(2.0,0.0)*
     . anew(0.0,1.0)**2*dsindrw**3-6.0*a(1.0,3.0)*anew(2.0,0.0)*anew(
     . 0.0,1.0)*dcosdrw*dsindrw**2-3.0*a(1.0,3.0)*anew(2.0,0.0)*
     . dcosdrw**2*dsindrw-6.0*a(1.0,3.0)*anew(1.0,2.0)*anew(1.0,0.0)*
     . anew(0.0,0.0)*dsindrw**3-3.0*a(1.0,3.0)*anew(1.0,1.0)**2*anew(
     . 0.0,0.0)*dsindrw**3+ans4
      ans2=-2.0*a(3.0,2.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrw**2-a(
     . 3.0,2.0)*anew(0.0,1.0)**2*dsindrw**2-2.0*a(3.0,2.0)*anew(0.0,
     . 1.0)*dcosdrw*dsindrw-a(3.0,2.0)*dcosdrw**2-a(3.0,1.0)*anew(0.0
     . ,2.0)*dsindrw-3.0*a(2.0,3.0)*anew(1.0,2.0)*anew(0.0,0.0)**2*
     . dsindrw**3-6.0*a(2.0,3.0)*anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0
     . ,0.0)*dsindrw**3-6.0*a(2.0,3.0)*anew(1.0,1.0)*anew(0.0,0.0)*
     . dcosdrw*dsindrw**2-6.0*a(2.0,3.0)*anew(1.0,0.0)*anew(0.0,2.0)*
     . anew(0.0,0.0)*dsindrw**3-3.0*a(2.0,3.0)*anew(1.0,0.0)*anew(0.0
     . ,1.0)**2*dsindrw**3-6.0*a(2.0,3.0)*anew(1.0,0.0)*anew(0.0,1.0)
     . *dcosdrw*dsindrw**2-3.0*a(2.0,3.0)*anew(1.0,0.0)*dcosdrw**2*
     . dsindrw-2.0*a(2.0,2.0)*anew(1.0,2.0)*anew(0.0,0.0)*dsindrw**2-
     . 2.0*a(2.0,2.0)*anew(1.0,1.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(
     . 2.0,2.0)*anew(1.0,1.0)*dcosdrw*dsindrw-2.0*a(2.0,2.0)*anew(1.0
     . ,0.0)*anew(0.0,2.0)*dsindrw**2-a(2.0,1.0)*anew(1.0,2.0)*
     . dsindrw-4.0*a(1.0,4.0)*anew(2.0,2.0)*anew(0.0,0.0)**3*dsindrw
     . **4-12.0*a(1.0,4.0)*anew(2.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **2*dsindrw**4-12.0*a(1.0,4.0)*anew(2.0,1.0)*anew(0.0,0.0)**2*
     . dcosdrw*dsindrw**3-12.0*a(1.0,4.0)*anew(2.0,0.0)*anew(0.0,2.0)
     . *anew(0.0,0.0)**2*dsindrw**4-12.0*a(1.0,4.0)*anew(2.0,0.0)*
     . anew(0.0,1.0)**2*anew(0.0,0.0)*dsindrw**4+ans3
      ans1=dcosdrw*ans2
      anew(3.0,2.0)=ans1/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      ans3=-6.0*a(0.0,4.0)*anew(2.0,0.0)**2*anew(0.0,0.0)**2*dsindrw
     . **4-12.0*a(0.0,4.0)*anew(2.0,0.0)*anew(1.0,0.0)**2*anew(0.0,
     . 0.0)*dsindrw**4-a(0.0,4.0)*anew(1.0,0.0)**4*dsindrw**4-6.0*a(
     . 0.0,3.0)*anew(3.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrw**3-
     . 3.0*a(0.0,3.0)*anew(2.0,0.0)**2*anew(0.0,0.0)*dsindrw**3-3.0*a
     . (0.0,3.0)*anew(2.0,0.0)*anew(1.0,0.0)**2*dsindrw**3-2.0*a(0.0,
     . 2.0)*anew(3.0,0.0)*anew(1.0,0.0)*dsindrw**2-a(0.0,2.0)*anew(
     . 2.0,0.0)**2*dsindrw**2
      ans2=-a(4.0,1.0)*anew(0.0,0.0)*dsindrw-a(4.0,0.0)-2.0*a(3.0,2.0
     . )*anew(1.0,0.0)*anew(0.0,0.0)*dsindrw**2-a(3.0,1.0)*anew(1.0,
     . 0.0)*dsindrw-3.0*a(2.0,3.0)*anew(2.0,0.0)*anew(0.0,0.0)**2*
     . dsindrw**3-3.0*a(2.0,3.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*
     . dsindrw**3-2.0*a(2.0,2.0)*anew(2.0,0.0)*anew(0.0,0.0)*dsindrw
     . **2-a(2.0,2.0)*anew(1.0,0.0)**2*dsindrw**2-a(2.0,1.0)*anew(2.0
     . ,0.0)*dsindrw-4.0*a(1.0,4.0)*anew(3.0,0.0)*anew(0.0,0.0)**3*
     . dsindrw**4-12.0*a(1.0,4.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(
     . 0.0,0.0)**2*dsindrw**4-4.0*a(1.0,4.0)*anew(1.0,0.0)**3*anew(
     . 0.0,0.0)*dsindrw**4-3.0*a(1.0,3.0)*anew(3.0,0.0)*anew(0.0,0.0)
     . **2*dsindrw**3-6.0*a(1.0,3.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew
     . (0.0,0.0)*dsindrw**3-a(1.0,3.0)*anew(1.0,0.0)**3*dsindrw**3-
     . 2.0*a(1.0,2.0)*anew(3.0,0.0)*anew(0.0,0.0)*dsindrw**2-2.0*a(
     . 1.0,2.0)*anew(2.0,0.0)*anew(1.0,0.0)*dsindrw**2-a(1.0,1.0)*
     . anew(3.0,0.0)*dsindrw-20.0*a(0.0,5.0)*anew(3.0,0.0)*anew(1.0,
     . 0.0)*anew(0.0,0.0)**3*dsindrw**5-10.0*a(0.0,5.0)*anew(2.0,0.0)
     . **2*anew(0.0,0.0)**3*dsindrw**5-30.0*a(0.0,5.0)*anew(2.0,0.0)*
     . anew(1.0,0.0)**2*anew(0.0,0.0)**2*dsindrw**5-5.0*a(0.0,5.0)*
     . anew(1.0,0.0)**4*anew(0.0,0.0)*dsindrw**5-12.0*a(0.0,4.0)*anew
     . (3.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dsindrw**4+ans3
      ans1=dcosdrw*ans2
      anew(4.0,0.0)=ans1/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      ans6=-6.0*a(0.0,3.0)*anew(2.0,1.0)*anew(2.0,0.0)*anew(0.0,0.0)*
     . dsindrw**3-3.0*a(0.0,3.0)*anew(2.0,1.0)*anew(1.0,0.0)**2*
     . dsindrw**3-3.0*a(0.0,3.0)*anew(2.0,0.0)**2*anew(0.0,1.0)*
     . dsindrw**3-3.0*a(0.0,3.0)*anew(2.0,0.0)**2*dcosdrw*dsindrw**2-
     . 6.0*a(0.0,3.0)*anew(2.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)*
     . dsindrw**3-2.0*a(0.0,2.0)*anew(4.0,0.0)*anew(0.0,1.0)*dsindrw
     . **2-2.0*a(0.0,2.0)*anew(4.0,0.0)*dcosdrw*dsindrw-2.0*a(0.0,2.0
     . )*anew(3.0,1.0)*anew(1.0,0.0)*dsindrw**2-2.0*a(0.0,2.0)*anew(
     . 3.0,0.0)*anew(1.0,1.0)*dsindrw**2-2.0*a(0.0,2.0)*anew(2.0,1.0)
     . *anew(2.0,0.0)*dsindrw**2
      ans5=-12.0*a(0.0,4.0)*anew(3.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)
     . **2*dsindrw**4-12.0*a(0.0,4.0)*anew(3.0,0.0)*anew(1.0,1.0)*
     . anew(0.0,0.0)**2*dsindrw**4-24.0*a(0.0,4.0)*anew(3.0,0.0)*anew
     . (1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**4-24.0*a(0.0,
     . 4.0)*anew(3.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrw*dsindrw
     . **3-12.0*a(0.0,4.0)*anew(2.0,1.0)*anew(2.0,0.0)*anew(0.0,0.0)
     . **2*dsindrw**4-12.0*a(0.0,4.0)*anew(2.0,1.0)*anew(1.0,0.0)**2*
     . anew(0.0,0.0)*dsindrw**4-12.0*a(0.0,4.0)*anew(2.0,0.0)**2*anew
     . (0.0,1.0)*anew(0.0,0.0)*dsindrw**4-12.0*a(0.0,4.0)*anew(2.0,
     . 0.0)**2*anew(0.0,0.0)*dcosdrw*dsindrw**3-24.0*a(0.0,4.0)*anew(
     . 2.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrw**4-
     . 12.0*a(0.0,4.0)*anew(2.0,0.0)*anew(1.0,0.0)**2*anew(0.0,1.0)*
     . dsindrw**4-12.0*a(0.0,4.0)*anew(2.0,0.0)*anew(1.0,0.0)**2*
     . dcosdrw*dsindrw**3-4.0*a(0.0,4.0)*anew(1.0,1.0)*anew(1.0,0.0)
     . **3*dsindrw**4-6.0*a(0.0,3.0)*anew(4.0,0.0)*anew(0.0,1.0)*anew
     . (0.0,0.0)*dsindrw**3-6.0*a(0.0,3.0)*anew(4.0,0.0)*anew(0.0,0.0
     . )*dcosdrw*dsindrw**2-6.0*a(0.0,3.0)*anew(3.0,1.0)*anew(1.0,0.0
     . )*anew(0.0,0.0)*dsindrw**3-6.0*a(0.0,3.0)*anew(3.0,0.0)*anew(
     . 1.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(0.0,3.0)*anew(3.0,0.0)
     . *anew(1.0,0.0)*anew(0.0,1.0)*dsindrw**3-6.0*a(0.0,3.0)*anew(
     . 3.0,0.0)*anew(1.0,0.0)*dcosdrw*dsindrw**2+ans6
      ans4=-20.0*a(0.0,5.0)*anew(4.0,0.0)*anew(0.0,0.0)**3*dcosdrw*
     . dsindrw**4-20.0*a(0.0,5.0)*anew(3.0,1.0)*anew(1.0,0.0)*anew(
     . 0.0,0.0)**3*dsindrw**5-20.0*a(0.0,5.0)*anew(3.0,0.0)*anew(1.0,
     . 1.0)*anew(0.0,0.0)**3*dsindrw**5-60.0*a(0.0,5.0)*anew(3.0,0.0)
     . *anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**5-60.0*
     . a(0.0,5.0)*anew(3.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*
     . dcosdrw*dsindrw**4-20.0*a(0.0,5.0)*anew(2.0,1.0)*anew(2.0,0.0)
     . *anew(0.0,0.0)**3*dsindrw**5-30.0*a(0.0,5.0)*anew(2.0,1.0)*
     . anew(1.0,0.0)**2*anew(0.0,0.0)**2*dsindrw**5-30.0*a(0.0,5.0)*
     . anew(2.0,0.0)**2*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**5-
     . 30.0*a(0.0,5.0)*anew(2.0,0.0)**2*anew(0.0,0.0)**2*dcosdrw*
     . dsindrw**4-60.0*a(0.0,5.0)*anew(2.0,0.0)*anew(1.0,1.0)*anew(
     . 1.0,0.0)*anew(0.0,0.0)**2*dsindrw**5-60.0*a(0.0,5.0)*anew(2.0,
     . 0.0)*anew(1.0,0.0)**2*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**5-
     . 60.0*a(0.0,5.0)*anew(2.0,0.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*
     . dcosdrw*dsindrw**4-20.0*a(0.0,5.0)*anew(1.0,1.0)*anew(1.0,0.0)
     . **3*anew(0.0,0.0)*dsindrw**5-5.0*a(0.0,5.0)*anew(1.0,0.0)**4*
     . anew(0.0,1.0)*dsindrw**5-5.0*a(0.0,5.0)*anew(1.0,0.0)**4*
     . dcosdrw*dsindrw**4-12.0*a(0.0,4.0)*anew(4.0,0.0)*anew(0.0,1.0)
     . *anew(0.0,0.0)**2*dsindrw**4-12.0*a(0.0,4.0)*anew(4.0,0.0)*
     . anew(0.0,0.0)**2*dcosdrw*dsindrw**3+ans5
      ans3=-24.0*a(1.0,4.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0)
     . *anew(0.0,0.0)*dsindrw**4-24.0*a(1.0,4.0)*anew(2.0,0.0)*anew(
     . 1.0,0.0)*anew(0.0,0.0)*dcosdrw*dsindrw**3-12.0*a(1.0,4.0)*anew
     . (1.0,1.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*dsindrw**4-4.0*a(1.0,
     . 4.0)*anew(1.0,0.0)**3*anew(0.0,1.0)*dsindrw**4-4.0*a(1.0,4.0)*
     . anew(1.0,0.0)**3*dcosdrw*dsindrw**3-3.0*a(1.0,3.0)*anew(3.0,
     . 1.0)*anew(0.0,0.0)**2*dsindrw**3-6.0*a(1.0,3.0)*anew(3.0,0.0)*
     . anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(1.0,3.0)*anew(3.0
     . ,0.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2-6.0*a(1.0,3.0)*anew(2.0
     . ,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(1.0,3.0)*
     . anew(2.0,0.0)*anew(1.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(1.0
     . ,3.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0)*dsindrw**3-6.0
     . *a(1.0,3.0)*anew(2.0,0.0)*anew(1.0,0.0)*dcosdrw*dsindrw**2-3.0
     . *a(1.0,3.0)*anew(1.0,1.0)*anew(1.0,0.0)**2*dsindrw**3-2.0*a(
     . 1.0,2.0)*anew(3.0,1.0)*anew(0.0,0.0)*dsindrw**2-2.0*a(1.0,2.0)
     . *anew(3.0,0.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(1.0,2.0)*anew(
     . 3.0,0.0)*dcosdrw*dsindrw-2.0*a(1.0,2.0)*anew(2.0,1.0)*anew(1.0
     . ,0.0)*dsindrw**2-2.0*a(1.0,2.0)*anew(2.0,0.0)*anew(1.0,1.0)*
     . dsindrw**2-a(1.0,1.0)*anew(3.0,1.0)*dsindrw-20.0*a(0.0,5.0)*
     . anew(4.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**3*dsindrw**5+ans4
      ans2=-a(4.0,1.0)*anew(0.0,1.0)*dsindrw-a(4.0,1.0)*dcosdrw-2.0*a
     . (3.0,2.0)*anew(1.0,1.0)*anew(0.0,0.0)*dsindrw**2-2.0*a(3.0,2.0
     . )*anew(1.0,0.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(3.0,2.0)*anew(
     . 1.0,0.0)*dcosdrw*dsindrw-a(3.0,1.0)*anew(1.0,1.0)*dsindrw-3.0*
     . a(2.0,3.0)*anew(2.0,1.0)*anew(0.0,0.0)**2*dsindrw**3-6.0*a(2.0
     . ,3.0)*anew(2.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrw**3-6.0
     . *a(2.0,3.0)*anew(2.0,0.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2-6.0
     . *a(2.0,3.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrw
     . **3-3.0*a(2.0,3.0)*anew(1.0,0.0)**2*anew(0.0,1.0)*dsindrw**3-
     . 3.0*a(2.0,3.0)*anew(1.0,0.0)**2*dcosdrw*dsindrw**2-2.0*a(2.0,
     . 2.0)*anew(2.0,1.0)*anew(0.0,0.0)*dsindrw**2-2.0*a(2.0,2.0)*
     . anew(2.0,0.0)*anew(0.0,1.0)*dsindrw**2-2.0*a(2.0,2.0)*anew(2.0
     . ,0.0)*dcosdrw*dsindrw-2.0*a(2.0,2.0)*anew(1.0,1.0)*anew(1.0,
     . 0.0)*dsindrw**2-a(2.0,1.0)*anew(2.0,1.0)*dsindrw-4.0*a(1.0,4.0
     . )*anew(3.0,1.0)*anew(0.0,0.0)**3*dsindrw**4-12.0*a(1.0,4.0)*
     . anew(3.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrw**4-12.0*a
     . (1.0,4.0)*anew(3.0,0.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3-
     . 12.0*a(1.0,4.0)*anew(2.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*
     . dsindrw**4-12.0*a(1.0,4.0)*anew(2.0,0.0)*anew(1.0,1.0)*anew(
     . 0.0,0.0)**2*dsindrw**4+ans3
      ans1=dcosdrw*ans2
      anew(4.0,1.0)=ans1/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      ans3=-20.0*a(0.0,5.0)*anew(4.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)
     . **3*dsindrw**5-20.0*a(0.0,5.0)*anew(3.0,0.0)*anew(2.0,0.0)*
     . anew(0.0,0.0)**3*dsindrw**5-30.0*a(0.0,5.0)*anew(3.0,0.0)*anew
     . (1.0,0.0)**2*anew(0.0,0.0)**2*dsindrw**5-30.0*a(0.0,5.0)*anew(
     . 2.0,0.0)**2*anew(1.0,0.0)*anew(0.0,0.0)**2*dsindrw**5-20.0*a(
     . 0.0,5.0)*anew(2.0,0.0)*anew(1.0,0.0)**3*anew(0.0,0.0)*dsindrw
     . **5-a(0.0,5.0)*anew(1.0,0.0)**5*dsindrw**5-12.0*a(0.0,4.0)*
     . anew(4.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dsindrw**4-12.0*a
     . (0.0,4.0)*anew(3.0,0.0)*anew(2.0,0.0)*anew(0.0,0.0)**2*dsindrw
     . **4-12.0*a(0.0,4.0)*anew(3.0,0.0)*anew(1.0,0.0)**2*anew(0.0,
     . 0.0)*dsindrw**4-12.0*a(0.0,4.0)*anew(2.0,0.0)**2*anew(1.0,0.0)
     . *anew(0.0,0.0)*dsindrw**4-4.0*a(0.0,4.0)*anew(2.0,0.0)*anew(
     . 1.0,0.0)**3*dsindrw**4-6.0*a(0.0,3.0)*anew(4.0,0.0)*anew(1.0,
     . 0.0)*anew(0.0,0.0)*dsindrw**3-6.0*a(0.0,3.0)*anew(3.0,0.0)*
     . anew(2.0,0.0)*anew(0.0,0.0)*dsindrw**3-3.0*a(0.0,3.0)*anew(3.0
     . ,0.0)*anew(1.0,0.0)**2*dsindrw**3-3.0*a(0.0,3.0)*anew(2.0,0.0)
     . **2*anew(1.0,0.0)*dsindrw**3-2.0*a(0.0,2.0)*anew(4.0,0.0)*anew
     . (1.0,0.0)*dsindrw**2-2.0*a(0.0,2.0)*anew(3.0,0.0)*anew(2.0,0.0
     . )*dsindrw**2
      ans2=-a(5.0,0.0)-a(4.0,1.0)*anew(1.0,0.0)*dsindrw-2.0*a(3.0,2.0
     . )*anew(2.0,0.0)*anew(0.0,0.0)*dsindrw**2-a(3.0,2.0)*anew(1.0,
     . 0.0)**2*dsindrw**2-a(3.0,1.0)*anew(2.0,0.0)*dsindrw-3.0*a(2.0,
     . 3.0)*anew(3.0,0.0)*anew(0.0,0.0)**2*dsindrw**3-6.0*a(2.0,3.0)*
     . anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrw**3-a(2.0,3.0
     . )*anew(1.0,0.0)**3*dsindrw**3-2.0*a(2.0,2.0)*anew(3.0,0.0)*
     . anew(0.0,0.0)*dsindrw**2-2.0*a(2.0,2.0)*anew(2.0,0.0)*anew(1.0
     . ,0.0)*dsindrw**2-a(2.0,1.0)*anew(3.0,0.0)*dsindrw-4.0*a(1.0,
     . 4.0)*anew(4.0,0.0)*anew(0.0,0.0)**3*dsindrw**4-12.0*a(1.0,4.0)
     . *anew(3.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dsindrw**4-6.0*a
     . (1.0,4.0)*anew(2.0,0.0)**2*anew(0.0,0.0)**2*dsindrw**4-12.0*a(
     . 1.0,4.0)*anew(2.0,0.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*dsindrw
     . **4-a(1.0,4.0)*anew(1.0,0.0)**4*dsindrw**4-3.0*a(1.0,3.0)*anew
     . (4.0,0.0)*anew(0.0,0.0)**2*dsindrw**3-6.0*a(1.0,3.0)*anew(3.0,
     . 0.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrw**3-3.0*a(1.0,3.0)*
     . anew(2.0,0.0)**2*anew(0.0,0.0)*dsindrw**3-3.0*a(1.0,3.0)*anew(
     . 2.0,0.0)*anew(1.0,0.0)**2*dsindrw**3-2.0*a(1.0,2.0)*anew(4.0,
     . 0.0)*anew(0.0,0.0)*dsindrw**2-2.0*a(1.0,2.0)*anew(3.0,0.0)*
     . anew(1.0,0.0)*dsindrw**2-a(1.0,2.0)*anew(2.0,0.0)**2*dsindrw**
     . 2-a(1.0,1.0)*anew(4.0,0.0)*dsindrw+ans3
      ans1=dcosdrw*ans2
      anew(5.0,0.0)=ans1/(5.0*a(0.0,5.0)*anew(0.0,0.0)**4*dcosdrw*
     . dsindrw**5+4.0*a(0.0,4.0)*anew(0.0,0.0)**3*dcosdrw*dsindrw**4+
     . 3.0*a(0.0,3.0)*anew(0.0,0.0)**2*dcosdrw*dsindrw**3+2.0*a(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrw*dsindrw**2+a(0.0,1.0)*dcosdrw*
     . dsindrw+dsindrw**2-1.0)
      return
      end
c /afs/psi.ch/user/f/flechsig/phase/src/phase/misali3.for

