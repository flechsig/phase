c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/misali2.for
c$$$ Date      : <16 Feb 04 16:38:37 flechsig> 
c$$$ Time-stamp: <02 Sep 14 12:11:58 flechsig> 

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

      subroutine misali2(a,anew,drl)
      implicit real*8(a-h,o-z)
      dimension a(0:5,0:5),anew(0:5,0:5)

c UF correction of the direction
      drl=- drl

      dsindrl=dsin(drl)
      dcosdrl=dcos(drl)
      anew(0,0)=0.
      anew(0.0,1.0)=(-dcosdrl*(a(4.0,1.0)*anew(0.0,0.0)**4*dsindrl**4
     . +a(3.0,1.0)*anew(0.0,0.0)**3*dsindrl**3+a(2.0,1.0)*anew(0.0,
     . 0.0)**2*dsindrl**2+a(1.0,1.0)*anew(0.0,0.0)*dsindrl+a(0.0,1.0)
     . ))/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*dsindrl**5+4.0*a(
     . 4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+3.0*a(3.0,0.0)*
     . anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,0.0)*anew(0.0,
     . 0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*dsindrl+dsindrl**2-
     . 1.0)
      anew(0.0,2.0)=(dcosdrl*(-10.0*a(5.0,0.0)*anew(0.0,1.0)**2*anew(
     . 0.0,0.0)**3*dsindrl**5-4.0*a(4.0,1.0)*anew(0.0,1.0)*anew(0.0,
     . 0.0)**3*dsindrl**4-6.0*a(4.0,0.0)*anew(0.0,1.0)**2*anew(0.0,
     . 0.0)**2*dsindrl**4-a(3.0,2.0)*anew(0.0,0.0)**3*dsindrl**3-3.0*
     . a(3.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl**3-3.0*a(3.0
     . ,0.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dsindrl**3-a(2.0,2.0)*
     . anew(0.0,0.0)**2*dsindrl**2-2.0*a(2.0,1.0)*anew(0.0,1.0)*anew(
     . 0.0,0.0)*dsindrl**2-a(2.0,0.0)*anew(0.0,1.0)**2*dsindrl**2-a(
     . 1.0,2.0)*anew(0.0,0.0)*dsindrl-a(1.0,1.0)*anew(0.0,1.0)*
     . dsindrl-a(0.0,2.0)))/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      ans1=dcosdrl*(-20.0*a(5.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew
     . (0.0,0.0)**3*dsindrl**5-10.0*a(5.0,0.0)*anew(0.0,1.0)**3*anew(
     . 0.0,0.0)**2*dsindrl**5-4.0*a(4.0,1.0)*anew(0.0,2.0)*anew(0.0,
     . 0.0)**3*dsindrl**4-6.0*a(4.0,1.0)*anew(0.0,1.0)**2*anew(0.0,
     . 0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)
     . *anew(0.0,0.0)**2*dsindrl**4-4.0*a(4.0,0.0)*anew(0.0,1.0)**3*
     . anew(0.0,0.0)*dsindrl**4-3.0*a(3.0,2.0)*anew(0.0,1.0)*anew(0.0
     . ,0.0)**2*dsindrl**3-3.0*a(3.0,1.0)*anew(0.0,2.0)*anew(0.0,0.0)
     . **2*dsindrl**3-3.0*a(3.0,1.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*
     . dsindrl**3-6.0*a(3.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0
     . ,0.0)*dsindrl**3-a(3.0,0.0)*anew(0.0,1.0)**3*dsindrl**3-a(2.0,
     . 3.0)*anew(0.0,0.0)**2*dsindrl**2-2.0*a(2.0,2.0)*anew(0.0,1.0)*
     . anew(0.0,0.0)*dsindrl**2-2.0*a(2.0,1.0)*anew(0.0,2.0)*anew(0.0
     . ,0.0)*dsindrl**2-a(2.0,1.0)*anew(0.0,1.0)**2*dsindrl**2-2.0*a(
     . 2.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*dsindrl**2-a(1.0,3.0)*
     . anew(0.0,0.0)*dsindrl-a(1.0,2.0)*anew(0.0,1.0)*dsindrl-a(1.0,
     . 1.0)*anew(0.0,2.0)*dsindrl-a(0.0,3.0))
      anew(0.0,3.0)=ans1/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      ans3=-2.0*a(2.0,2.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrl**2-a(
     . 2.0,2.0)*anew(0.0,1.0)**2*dsindrl**2-2.0*a(2.0,1.0)*anew(0.0,
     . 3.0)*anew(0.0,0.0)*dsindrl**2-2.0*a(2.0,1.0)*anew(0.0,2.0)*
     . anew(0.0,1.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(0.0,3.0)*anew(0.0
     . ,1.0)*dsindrl**2-a(2.0,0.0)*anew(0.0,2.0)**2*dsindrl**2-a(1.0,
     . 4.0)*anew(0.0,0.0)*dsindrl-a(1.0,3.0)*anew(0.0,1.0)*dsindrl-a(
     . 1.0,2.0)*anew(0.0,2.0)*dsindrl-a(1.0,1.0)*anew(0.0,3.0)*
     . dsindrl-a(0.0,4.0)
      ans2=-20.0*a(5.0,0.0)*anew(0.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **3*dsindrl**5-10.0*a(5.0,0.0)*anew(0.0,2.0)**2*anew(0.0,0.0)
     . **3*dsindrl**5-30.0*a(5.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)**2*
     . anew(0.0,0.0)**2*dsindrl**5-5.0*a(5.0,0.0)*anew(0.0,1.0)**4*
     . anew(0.0,0.0)*dsindrl**5-4.0*a(4.0,1.0)*anew(0.0,3.0)*anew(0.0
     . ,0.0)**3*dsindrl**4-12.0*a(4.0,1.0)*anew(0.0,2.0)*anew(0.0,1.0
     . )*anew(0.0,0.0)**2*dsindrl**4-4.0*a(4.0,1.0)*anew(0.0,1.0)**3*
     . anew(0.0,0.0)*dsindrl**4-12.0*a(4.0,0.0)*anew(0.0,3.0)*anew(
     . 0.0,1.0)*anew(0.0,0.0)**2*dsindrl**4-6.0*a(4.0,0.0)*anew(0.0,
     . 2.0)**2*anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(0.0,
     . 2.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dsindrl**4-a(4.0,0.0)*anew
     . (0.0,1.0)**4*dsindrl**4-3.0*a(3.0,2.0)*anew(0.0,2.0)*anew(0.0,
     . 0.0)**2*dsindrl**3-3.0*a(3.0,2.0)*anew(0.0,1.0)**2*anew(0.0,
     . 0.0)*dsindrl**3-3.0*a(3.0,1.0)*anew(0.0,3.0)*anew(0.0,0.0)**2*
     . dsindrl**3-6.0*a(3.0,1.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0
     . ,0.0)*dsindrl**3-a(3.0,1.0)*anew(0.0,1.0)**3*dsindrl**3-6.0*a(
     . 3.0,0.0)*anew(0.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**3-
     . 3.0*a(3.0,0.0)*anew(0.0,2.0)**2*anew(0.0,0.0)*dsindrl**3-3.0*a
     . (3.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)**2*dsindrl**3-2.0*a(2.0,
     . 3.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**2+ans3
      ans1=dcosdrl*ans2
      anew(0.0,4.0)=ans1/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      ans3=-3.0*a(3.0,1.0)*anew(0.0,4.0)*anew(0.0,0.0)**2*dsindrl**3-
     . 6.0*a(3.0,1.0)*anew(0.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)*
     . dsindrl**3-3.0*a(3.0,1.0)*anew(0.0,2.0)**2*anew(0.0,0.0)*
     . dsindrl**3-3.0*a(3.0,1.0)*anew(0.0,2.0)*anew(0.0,1.0)**2*
     . dsindrl**3-6.0*a(3.0,0.0)*anew(0.0,4.0)*anew(0.0,1.0)*anew(0.0
     . ,0.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(0.0,3.0)*anew(0.0,2.0)*
     . anew(0.0,0.0)*dsindrl**3-3.0*a(3.0,0.0)*anew(0.0,3.0)*anew(0.0
     . ,1.0)**2*dsindrl**3-3.0*a(3.0,0.0)*anew(0.0,2.0)**2*anew(0.0,
     . 1.0)*dsindrl**3-2.0*a(2.0,3.0)*anew(0.0,2.0)*anew(0.0,0.0)*
     . dsindrl**2-a(2.0,3.0)*anew(0.0,1.0)**2*dsindrl**2-2.0*a(2.0,
     . 2.0)*anew(0.0,3.0)*anew(0.0,0.0)*dsindrl**2-2.0*a(2.0,2.0)*
     . anew(0.0,2.0)*anew(0.0,1.0)*dsindrl**2-2.0*a(2.0,1.0)*anew(0.0
     . ,4.0)*anew(0.0,0.0)*dsindrl**2-2.0*a(2.0,1.0)*anew(0.0,3.0)*
     . anew(0.0,1.0)*dsindrl**2-a(2.0,1.0)*anew(0.0,2.0)**2*dsindrl**
     . 2-2.0*a(2.0,0.0)*anew(0.0,4.0)*anew(0.0,1.0)*dsindrl**2-2.0*a(
     . 2.0,0.0)*anew(0.0,3.0)*anew(0.0,2.0)*dsindrl**2-a(1.0,4.0)*
     . anew(0.0,1.0)*dsindrl-a(1.0,3.0)*anew(0.0,2.0)*dsindrl-a(1.0,
     . 2.0)*anew(0.0,3.0)*dsindrl-a(1.0,1.0)*anew(0.0,4.0)*dsindrl-a(
     . 0.0,5.0)
      ans2=-20.0*a(5.0,0.0)*anew(0.0,4.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **3*dsindrl**5-20.0*a(5.0,0.0)*anew(0.0,3.0)*anew(0.0,2.0)*
     . anew(0.0,0.0)**3*dsindrl**5-30.0*a(5.0,0.0)*anew(0.0,3.0)*anew
     . (0.0,1.0)**2*anew(0.0,0.0)**2*dsindrl**5-30.0*a(5.0,0.0)*anew(
     . 0.0,2.0)**2*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl**5-20.0*a(
     . 5.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)**3*anew(0.0,0.0)*dsindrl
     . **5-a(5.0,0.0)*anew(0.0,1.0)**5*dsindrl**5-4.0*a(4.0,1.0)*anew
     . (0.0,4.0)*anew(0.0,0.0)**3*dsindrl**4-12.0*a(4.0,1.0)*anew(0.0
     . ,3.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl**4-6.0*a(4.0,1.0)
     . *anew(0.0,2.0)**2*anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,1.0)*
     . anew(0.0,2.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dsindrl**4-a(4.0,
     . 1.0)*anew(0.0,1.0)**4*dsindrl**4-12.0*a(4.0,0.0)*anew(0.0,4.0)
     . *anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*
     . anew(0.0,3.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*dsindrl**4-12.0*a
     . (4.0,0.0)*anew(0.0,3.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dsindrl
     . **4-12.0*a(4.0,0.0)*anew(0.0,2.0)**2*anew(0.0,1.0)*anew(0.0,
     . 0.0)*dsindrl**4-4.0*a(4.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)**3*
     . dsindrl**4-3.0*a(3.0,2.0)*anew(0.0,3.0)*anew(0.0,0.0)**2*
     . dsindrl**3-6.0*a(3.0,2.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0
     . ,0.0)*dsindrl**3-a(3.0,2.0)*anew(0.0,1.0)**3*dsindrl**3+ans3
      ans1=dcosdrl*ans2
      anew(0.0,5.0)=ans1/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      anew(1.0,0.0)=(dcosdrl*(-5.0*a(5.0,0.0)*anew(0.0,0.0)**4*
     . dcosdrl*dsindrl**4-4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*
     . dsindrl**3-3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**2-
     . 2.0*a(2.0,0.0)*anew(0.0,0.0)*dcosdrl*dsindrl-a(1.0,0.0)*
     . dcosdrl-dsindrl))/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      anew(1.0,1.0)=(dcosdrl*(-20.0*a(5.0,0.0)*anew(1.0,0.0)*anew(0.0
     . ,1.0)*anew(0.0,0.0)**3*dsindrl**5-20.0*a(5.0,0.0)*anew(0.0,1.0
     . )*anew(0.0,0.0)**3*dcosdrl*dsindrl**4-4.0*a(4.0,1.0)*anew(1.0,
     . 0.0)*anew(0.0,0.0)**3*dsindrl**4-4.0*a(4.0,1.0)*anew(0.0,0.0)
     . **3*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(1.0,0.0)*anew(0.0,
     . 1.0)*anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(0.0,1.0)
     . *anew(0.0,0.0)**2*dcosdrl*dsindrl**3-3.0*a(3.0,1.0)*anew(1.0,
     . 0.0)*anew(0.0,0.0)**2*dsindrl**3-3.0*a(3.0,1.0)*anew(0.0,0.0)
     . **2*dcosdrl*dsindrl**2-6.0*a(3.0,0.0)*anew(1.0,0.0)*anew(0.0,
     . 1.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(0.0,1.0)*
     . anew(0.0,0.0)*dcosdrl*dsindrl**2-2.0*a(2.0,1.0)*anew(1.0,0.0)*
     . anew(0.0,0.0)*dsindrl**2-2.0*a(2.0,1.0)*anew(0.0,0.0)*dcosdrl*
     . dsindrl-2.0*a(2.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0)*dsindrl**2-
     . 2.0*a(2.0,0.0)*anew(0.0,1.0)*dcosdrl*dsindrl-a(1.0,1.0)*anew(
     . 1.0,0.0)*dsindrl-a(1.0,1.0)*dcosdrl))/(5.0*a(5.0,0.0)*anew(0.0
     . ,0.0)**4*dcosdrl*dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*
     . dcosdrl*dsindrl**4+3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*
     . dsindrl**3+2.0*a(2.0,0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(
     . 1.0,0.0)*dcosdrl*dsindrl+dsindrl**2-1.0)
      ans3=-6.0*a(3.0,0.0)*anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)*
     . dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0
     . ,0.0)*dsindrl**3-3.0*a(3.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0)**2
     . *dsindrl**3-6.0*a(3.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)*dcosdrl
     . *dsindrl**2-3.0*a(3.0,0.0)*anew(0.0,1.0)**2*dcosdrl*dsindrl**2
     . -2.0*a(2.0,2.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrl**2-2.0*a(
     . 2.0,2.0)*anew(0.0,0.0)*dcosdrl*dsindrl-2.0*a(2.0,1.0)*anew(1.0
     . ,1.0)*anew(0.0,0.0)*dsindrl**2-2.0*a(2.0,1.0)*anew(1.0,0.0)*
     . anew(0.0,1.0)*dsindrl**2-2.0*a(2.0,1.0)*anew(0.0,1.0)*dcosdrl*
     . dsindrl-2.0*a(2.0,0.0)*anew(1.0,1.0)*anew(0.0,1.0)*dsindrl**2-
     . 2.0*a(2.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*dsindrl**2-2.0*a(
     . 2.0,0.0)*anew(0.0,2.0)*dcosdrl*dsindrl-a(1.0,2.0)*anew(1.0,0.0
     . )*dsindrl-a(1.0,2.0)*dcosdrl-a(1.0,1.0)*anew(1.0,1.0)*dsindrl
      ans2=-20.0*a(5.0,0.0)*anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **3*dsindrl**5-20.0*a(5.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*
     . anew(0.0,0.0)**3*dsindrl**5-30.0*a(5.0,0.0)*anew(1.0,0.0)*anew
     . (0.0,1.0)**2*anew(0.0,0.0)**2*dsindrl**5-20.0*a(5.0,0.0)*anew(
     . 0.0,2.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4-30.0*a(5.0,0.0)*
     . anew(0.0,1.0)**2*anew(0.0,0.0)**2*dcosdrl*dsindrl**4-4.0*a(4.0
     . ,1.0)*anew(1.0,1.0)*anew(0.0,0.0)**3*dsindrl**4-12.0*a(4.0,1.0
     . )*anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl**4-12.0
     . *a(4.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3-
     . 12.0*a(4.0,0.0)*anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*
     . dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(
     . 0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,0.0)*anew(0.0,
     . 1.0)**2*anew(0.0,0.0)*dsindrl**4-12.0*a(4.0,0.0)*anew(0.0,2.0)
     . *anew(0.0,0.0)**2*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(0.0,
     . 1.0)**2*anew(0.0,0.0)*dcosdrl*dsindrl**3-3.0*a(3.0,2.0)*anew(
     . 1.0,0.0)*anew(0.0,0.0)**2*dsindrl**3-3.0*a(3.0,2.0)*anew(0.0,
     . 0.0)**2*dcosdrl*dsindrl**2-3.0*a(3.0,1.0)*anew(1.0,1.0)*anew(
     . 0.0,0.0)**2*dsindrl**3-6.0*a(3.0,1.0)*anew(1.0,0.0)*anew(0.0,
     . 1.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0,1.0)*anew(0.0,1.0)*
     . anew(0.0,0.0)*dcosdrl*dsindrl**2+ans3
      ans1=dcosdrl*ans2
      anew(1.0,2.0)=ans1/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      ans4=-6.0*a(3.0,0.0)*anew(1.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)*
     . dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0
     . ,1.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)*
     . dcosdrl*dsindrl**2-6.0*a(3.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*
     . dcosdrl*dsindrl**2-2.0*a(2.0,3.0)*anew(1.0,0.0)*anew(0.0,0.0)*
     . dsindrl**2-2.0*a(2.0,3.0)*anew(0.0,0.0)*dcosdrl*dsindrl-2.0*a(
     . 2.0,2.0)*anew(1.0,1.0)*anew(0.0,0.0)*dsindrl**2-2.0*a(2.0,2.0)
     . *anew(1.0,0.0)*anew(0.0,1.0)*dsindrl**2-2.0*a(2.0,2.0)*anew(
     . 0.0,1.0)*dcosdrl*dsindrl-2.0*a(2.0,1.0)*anew(1.0,2.0)*anew(0.0
     . ,0.0)*dsindrl**2-2.0*a(2.0,1.0)*anew(1.0,1.0)*anew(0.0,1.0)*
     . dsindrl**2-2.0*a(2.0,1.0)*anew(1.0,0.0)*anew(0.0,2.0)*dsindrl
     . **2-2.0*a(2.0,1.0)*anew(0.0,2.0)*dcosdrl*dsindrl-2.0*a(2.0,0.0
     . )*anew(1.0,2.0)*anew(0.0,1.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(
     . 1.0,1.0)*anew(0.0,2.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(1.0,0.0)
     . *anew(0.0,3.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(0.0,3.0)*dcosdrl
     . *dsindrl-a(1.0,3.0)*anew(1.0,0.0)*dsindrl-a(1.0,3.0)*dcosdrl-a
     . (1.0,2.0)*anew(1.0,1.0)*dsindrl-a(1.0,1.0)*anew(1.0,2.0)*
     . dsindrl
      ans3=-12.0*a(4.0,0.0)*anew(1.0,1.0)*anew(0.0,1.0)**2*anew(0.0,
     . 0.0)*dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,0.0)*anew(0.0,3.0)*
     . anew(0.0,0.0)**2*dsindrl**4-24.0*a(4.0,0.0)*anew(1.0,0.0)*anew
     . (0.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**4-4.0*a(4.0,0.0
     . )*anew(1.0,0.0)*anew(0.0,1.0)**3*dsindrl**4-12.0*a(4.0,0.0)*
     . anew(0.0,3.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3-24.0*a(4.0,
     . 0.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrl*dsindrl
     . **3-4.0*a(4.0,0.0)*anew(0.0,1.0)**3*dcosdrl*dsindrl**3-3.0*a(
     . 3.0,2.0)*anew(1.0,1.0)*anew(0.0,0.0)**2*dsindrl**3-6.0*a(3.0,
     . 2.0)*anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**3-6.0*
     . a(3.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2-3.0*
     . a(3.0,1.0)*anew(1.0,2.0)*anew(0.0,0.0)**2*dsindrl**3-6.0*a(3.0
     . ,1.0)*anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**3-6.0
     . *a(3.0,1.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrl
     . **3-3.0*a(3.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)**2*dsindrl**3-
     . 6.0*a(3.0,1.0)*anew(0.0,2.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2-
     . 3.0*a(3.0,1.0)*anew(0.0,1.0)**2*dcosdrl*dsindrl**2-6.0*a(3.0,
     . 0.0)*anew(1.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**3-6.0*
     . a(3.0,0.0)*anew(1.0,1.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrl**
     . 3-3.0*a(3.0,0.0)*anew(1.0,1.0)*anew(0.0,1.0)**2*dsindrl**3+
     . ans4
      ans2=-20.0*a(5.0,0.0)*anew(1.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **3*dsindrl**5-20.0*a(5.0,0.0)*anew(1.0,1.0)*anew(0.0,2.0)*
     . anew(0.0,0.0)**3*dsindrl**5-30.0*a(5.0,0.0)*anew(1.0,1.0)*anew
     . (0.0,1.0)**2*anew(0.0,0.0)**2*dsindrl**5-20.0*a(5.0,0.0)*anew(
     . 1.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)**3*dsindrl**5-60.0*a(5.0,
     . 0.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)**
     . 2*dsindrl**5-20.0*a(5.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0)**3*
     . anew(0.0,0.0)*dsindrl**5-20.0*a(5.0,0.0)*anew(0.0,3.0)*anew(
     . 0.0,0.0)**3*dcosdrl*dsindrl**4-60.0*a(5.0,0.0)*anew(0.0,2.0)*
     . anew(0.0,1.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**4-20.0*a(5.0,
     . 0.0)*anew(0.0,1.0)**3*anew(0.0,0.0)*dcosdrl*dsindrl**4-4.0*a(
     . 4.0,1.0)*anew(1.0,2.0)*anew(0.0,0.0)**3*dsindrl**4-12.0*a(4.0,
     . 1.0)*anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl**4-
     . 12.0*a(4.0,1.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*
     . dsindrl**4-12.0*a(4.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)**2*anew
     . (0.0,0.0)*dsindrl**4-12.0*a(4.0,1.0)*anew(0.0,2.0)*anew(0.0,
     . 0.0)**2*dcosdrl*dsindrl**3-12.0*a(4.0,1.0)*anew(0.0,1.0)**2*
     . anew(0.0,0.0)*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(1.0,2.0)
     . *anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*
     . anew(1.0,1.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*dsindrl**4+ans3
      ans1=dcosdrl*ans2
      anew(1.0,3.0)=ans1/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      ans6=-2.0*a(2.0,1.0)*anew(1.0,0.0)*anew(0.0,3.0)*dsindrl**2-2.0
     . *a(2.0,1.0)*anew(0.0,3.0)*dcosdrl*dsindrl-2.0*a(2.0,0.0)*anew(
     . 1.0,3.0)*anew(0.0,1.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(1.0,2.0)
     . *anew(0.0,2.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(1.0,1.0)*anew(
     . 0.0,3.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(1.0,0.0)*anew(0.0,4.0)
     . *dsindrl**2-2.0*a(2.0,0.0)*anew(0.0,4.0)*dcosdrl*dsindrl-a(1.0
     . ,4.0)*anew(1.0,0.0)*dsindrl-a(1.0,4.0)*dcosdrl-a(1.0,3.0)*anew
     . (1.0,1.0)*dsindrl-a(1.0,2.0)*anew(1.0,2.0)*dsindrl-a(1.0,1.0)*
     . anew(1.0,3.0)*dsindrl
      ans5=-6.0*a(3.0,1.0)*anew(0.0,2.0)*anew(0.0,1.0)*dcosdrl*
     . dsindrl**2-6.0*a(3.0,0.0)*anew(1.0,3.0)*anew(0.0,1.0)*anew(0.0
     . ,0.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,2.0)*anew(0.0,2.0)*
     . anew(0.0,0.0)*dsindrl**3-3.0*a(3.0,0.0)*anew(1.0,2.0)*anew(0.0
     . ,1.0)**2*dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,1.0)*anew(0.0,3.0)
     . *anew(0.0,0.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,1.0)*anew(
     . 0.0,2.0)*anew(0.0,1.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,0.0)
     . *anew(0.0,4.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(
     . 1.0,0.0)*anew(0.0,3.0)*anew(0.0,1.0)*dsindrl**3-3.0*a(3.0,0.0)
     . *anew(1.0,0.0)*anew(0.0,2.0)**2*dsindrl**3-6.0*a(3.0,0.0)*anew
     . (0.0,4.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2-6.0*a(3.0,0.0)*anew
     . (0.0,3.0)*anew(0.0,1.0)*dcosdrl*dsindrl**2-3.0*a(3.0,0.0)*anew
     . (0.0,2.0)**2*dcosdrl*dsindrl**2-2.0*a(2.0,3.0)*anew(1.0,1.0)*
     . anew(0.0,0.0)*dsindrl**2-2.0*a(2.0,3.0)*anew(1.0,0.0)*anew(0.0
     . ,1.0)*dsindrl**2-2.0*a(2.0,3.0)*anew(0.0,1.0)*dcosdrl*dsindrl-
     . 2.0*a(2.0,2.0)*anew(1.0,2.0)*anew(0.0,0.0)*dsindrl**2-2.0*a(
     . 2.0,2.0)*anew(1.0,1.0)*anew(0.0,1.0)*dsindrl**2-2.0*a(2.0,2.0)
     . *anew(1.0,0.0)*anew(0.0,2.0)*dsindrl**2-2.0*a(2.0,2.0)*anew(
     . 0.0,2.0)*dcosdrl*dsindrl-2.0*a(2.0,1.0)*anew(1.0,3.0)*anew(0.0
     . ,0.0)*dsindrl**2-2.0*a(2.0,1.0)*anew(1.0,2.0)*anew(0.0,1.0)*
     . dsindrl**2-2.0*a(2.0,1.0)*anew(1.0,1.0)*anew(0.0,2.0)*dsindrl
     . **2+ans6
      ans4=-12.0*a(4.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)**2*anew(0.0,
     . 0.0)*dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*
     . anew(0.0,1.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(0.0,4.0)*anew
     . (0.0,0.0)**2*dcosdrl*dsindrl**3-24.0*a(4.0,0.0)*anew(0.0,3.0)*
     . anew(0.0,1.0)*anew(0.0,0.0)*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)
     . *anew(0.0,2.0)**2*anew(0.0,0.0)*dcosdrl*dsindrl**3-12.0*a(4.0,
     . 0.0)*anew(0.0,2.0)*anew(0.0,1.0)**2*dcosdrl*dsindrl**3-3.0*a(
     . 3.0,2.0)*anew(1.0,2.0)*anew(0.0,0.0)**2*dsindrl**3-6.0*a(3.0,
     . 2.0)*anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**3-6.0*
     . a(3.0,2.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrl**
     . 3-3.0*a(3.0,2.0)*anew(1.0,0.0)*anew(0.0,1.0)**2*dsindrl**3-6.0
     . *a(3.0,2.0)*anew(0.0,2.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2-3.0
     . *a(3.0,2.0)*anew(0.0,1.0)**2*dcosdrl*dsindrl**2-3.0*a(3.0,1.0)
     . *anew(1.0,3.0)*anew(0.0,0.0)**2*dsindrl**3-6.0*a(3.0,1.0)*anew
     . (1.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0,1.0
     . )*anew(1.0,1.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrl**3-3.0*a(
     . 3.0,1.0)*anew(1.0,1.0)*anew(0.0,1.0)**2*dsindrl**3-6.0*a(3.0,
     . 1.0)*anew(1.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)*dsindrl**3-6.0*
     . a(3.0,1.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*dsindrl**
     . 3-6.0*a(3.0,1.0)*anew(0.0,3.0)*anew(0.0,0.0)*dcosdrl*dsindrl**
     . 2+ans5
      ans3=-12.0*a(4.0,1.0)*anew(1.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **2*dsindrl**4-12.0*a(4.0,1.0)*anew(1.0,1.0)*anew(0.0,2.0)*
     . anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,1.0)*anew(1.0,1.0)*anew
     . (0.0,1.0)**2*anew(0.0,0.0)*dsindrl**4-12.0*a(4.0,1.0)*anew(1.0
     . ,0.0)*anew(0.0,3.0)*anew(0.0,0.0)**2*dsindrl**4-24.0*a(4.0,1.0
     . )*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*
     . dsindrl**4-4.0*a(4.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)**3*
     . dsindrl**4-12.0*a(4.0,1.0)*anew(0.0,3.0)*anew(0.0,0.0)**2*
     . dcosdrl*dsindrl**3-24.0*a(4.0,1.0)*anew(0.0,2.0)*anew(0.0,1.0)
     . *anew(0.0,0.0)*dcosdrl*dsindrl**3-4.0*a(4.0,1.0)*anew(0.0,1.0)
     . **3*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(1.0,3.0)*anew(0.0,
     . 1.0)*anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,2.0)
     . *anew(0.0,2.0)*anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*
     . anew(1.0,2.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dsindrl**4-12.0*a
     . (4.0,0.0)*anew(1.0,1.0)*anew(0.0,3.0)*anew(0.0,0.0)**2*dsindrl
     . **4-24.0*a(4.0,0.0)*anew(1.0,1.0)*anew(0.0,2.0)*anew(0.0,1.0)*
     . anew(0.0,0.0)*dsindrl**4-4.0*a(4.0,0.0)*anew(1.0,1.0)*anew(0.0
     . ,1.0)**3*dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,0.0)*anew(0.0,4.0
     . )*anew(0.0,0.0)**2*dsindrl**4-24.0*a(4.0,0.0)*anew(1.0,0.0)*
     . anew(0.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**4+ans4
      ans2=-20.0*a(5.0,0.0)*anew(1.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **3*dsindrl**5-20.0*a(5.0,0.0)*anew(1.0,2.0)*anew(0.0,2.0)*
     . anew(0.0,0.0)**3*dsindrl**5-30.0*a(5.0,0.0)*anew(1.0,2.0)*anew
     . (0.0,1.0)**2*anew(0.0,0.0)**2*dsindrl**5-20.0*a(5.0,0.0)*anew(
     . 1.0,1.0)*anew(0.0,3.0)*anew(0.0,0.0)**3*dsindrl**5-60.0*a(5.0,
     . 0.0)*anew(1.0,1.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)**
     . 2*dsindrl**5-20.0*a(5.0,0.0)*anew(1.0,1.0)*anew(0.0,1.0)**3*
     . anew(0.0,0.0)*dsindrl**5-20.0*a(5.0,0.0)*anew(1.0,0.0)*anew(
     . 0.0,4.0)*anew(0.0,0.0)**3*dsindrl**5-60.0*a(5.0,0.0)*anew(1.0,
     . 0.0)*anew(0.0,3.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl**5-
     . 30.0*a(5.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)**2*anew(0.0,0.0)**
     . 2*dsindrl**5-60.0*a(5.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(
     . 0.0,1.0)**2*anew(0.0,0.0)*dsindrl**5-5.0*a(5.0,0.0)*anew(1.0,
     . 0.0)*anew(0.0,1.0)**4*dsindrl**5-20.0*a(5.0,0.0)*anew(0.0,4.0)
     . *anew(0.0,0.0)**3*dcosdrl*dsindrl**4-60.0*a(5.0,0.0)*anew(0.0,
     . 3.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**4-30.0*a(
     . 5.0,0.0)*anew(0.0,2.0)**2*anew(0.0,0.0)**2*dcosdrl*dsindrl**4-
     . 60.0*a(5.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*
     . dcosdrl*dsindrl**4-5.0*a(5.0,0.0)*anew(0.0,1.0)**4*dcosdrl*
     . dsindrl**4-4.0*a(4.0,1.0)*anew(1.0,3.0)*anew(0.0,0.0)**3*
     . dsindrl**4+ans3
      ans1=dcosdrl*ans2
      anew(1.0,4.0)=ans1/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      anew(2.0,0.0)=(dcosdrl*(-10.0*a(5.0,0.0)*anew(1.0,0.0)**2*anew(
     . 0.0,0.0)**3*dsindrl**5-20.0*a(5.0,0.0)*anew(1.0,0.0)*anew(0.0,
     . 0.0)**3*dcosdrl*dsindrl**4-10.0*a(5.0,0.0)*anew(0.0,0.0)**3*
     . dcosdrl**2*dsindrl**3-6.0*a(4.0,0.0)*anew(1.0,0.0)**2*anew(0.0
     . ,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0
     . )**2*dcosdrl*dsindrl**3-6.0*a(4.0,0.0)*anew(0.0,0.0)**2*
     . dcosdrl**2*dsindrl**2-3.0*a(3.0,0.0)*anew(1.0,0.0)**2*anew(0.0
     . ,0.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)*
     . dcosdrl*dsindrl**2-3.0*a(3.0,0.0)*anew(0.0,0.0)*dcosdrl**2*
     . dsindrl-a(2.0,0.0)*anew(1.0,0.0)**2*dsindrl**2-2.0*a(2.0,0.0)*
     . anew(1.0,0.0)*dcosdrl*dsindrl-a(2.0,0.0)*dcosdrl**2))/(5.0*a(
     . 5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*dsindrl**5+4.0*a(4.0,0.0)*
     . anew(0.0,0.0)**3*dcosdrl*dsindrl**4+3.0*a(3.0,0.0)*anew(0.0,
     . 0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,0.0)*anew(0.0,0.0)*
     . dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*dsindrl+dsindrl**2-1.0)
      ans3=-6.0*a(3.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrl*
     . dsindrl**2-3.0*a(3.0,1.0)*anew(0.0,0.0)*dcosdrl**2*dsindrl-6.0
     . *a(3.0,0.0)*anew(2.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl
     . **3-6.0*a(3.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*
     . dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,1.0)*anew(0.0,0.0)*dcosdrl*
     . dsindrl**2-3.0*a(3.0,0.0)*anew(1.0,0.0)**2*anew(0.0,1.0)*
     . dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0)*dcosdrl*
     . dsindrl**2-3.0*a(3.0,0.0)*anew(0.0,1.0)*dcosdrl**2*dsindrl-2.0
     . *a(2.0,1.0)*anew(2.0,0.0)*anew(0.0,0.0)*dsindrl**2-a(2.0,1.0)*
     . anew(1.0,0.0)**2*dsindrl**2-2.0*a(2.0,1.0)*anew(1.0,0.0)*
     . dcosdrl*dsindrl-a(2.0,1.0)*dcosdrl**2-2.0*a(2.0,0.0)*anew(2.0,
     . 0.0)*anew(0.0,1.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(1.0,1.0)*
     . anew(1.0,0.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(1.0,1.0)*dcosdrl*
     . dsindrl-a(1.0,1.0)*anew(2.0,0.0)*dsindrl
      ans2=-20.0*a(5.0,0.0)*anew(2.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **3*dsindrl**5-20.0*a(5.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)*
     . anew(0.0,0.0)**3*dsindrl**5-20.0*a(5.0,0.0)*anew(1.0,1.0)*anew
     . (0.0,0.0)**3*dcosdrl*dsindrl**4-30.0*a(5.0,0.0)*anew(1.0,0.0)
     . **2*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl**5-60.0*a(5.0,0.0)*
     . anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**
     . 4-30.0*a(5.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dcosdrl**2*
     . dsindrl**3-4.0*a(4.0,1.0)*anew(2.0,0.0)*anew(0.0,0.0)**3*
     . dsindrl**4-6.0*a(4.0,1.0)*anew(1.0,0.0)**2*anew(0.0,0.0)**2*
     . dsindrl**4-12.0*a(4.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*
     . dcosdrl*dsindrl**3-6.0*a(4.0,1.0)*anew(0.0,0.0)**2*dcosdrl**2*
     . dsindrl**2-12.0*a(4.0,0.0)*anew(2.0,0.0)*anew(0.0,1.0)*anew(
     . 0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,1.0)*anew(1.0,
     . 0.0)*anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,1.0)
     . *anew(0.0,0.0)**2*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(1.0,
     . 0.0)**2*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**4-24.0*a(4.0,0.0)
     . *anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrl*dsindrl**3-
     . 12.0*a(4.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrl**2*dsindrl
     . **2-3.0*a(3.0,1.0)*anew(2.0,0.0)*anew(0.0,0.0)**2*dsindrl**3-
     . 3.0*a(3.0,1.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*dsindrl**3+ans3
      ans1=dcosdrl*ans2
      anew(2.0,1.0)=ans1/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      ans5=-3.0*a(3.0,0.0)*anew(1.0,0.0)**2*anew(0.0,2.0)*dsindrl**3-
     . 6.0*a(3.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*dcosdrl*dsindrl**2-
     . 3.0*a(3.0,0.0)*anew(0.0,2.0)*dcosdrl**2*dsindrl-2.0*a(2.0,2.0)
     . *anew(2.0,0.0)*anew(0.0,0.0)*dsindrl**2-a(2.0,2.0)*anew(1.0,
     . 0.0)**2*dsindrl**2-2.0*a(2.0,2.0)*anew(1.0,0.0)*dcosdrl*
     . dsindrl-a(2.0,2.0)*dcosdrl**2-2.0*a(2.0,1.0)*anew(2.0,1.0)*
     . anew(0.0,0.0)*dsindrl**2-2.0*a(2.0,1.0)*anew(2.0,0.0)*anew(0.0
     . ,1.0)*dsindrl**2-2.0*a(2.0,1.0)*anew(1.0,1.0)*anew(1.0,0.0)*
     . dsindrl**2-2.0*a(2.0,1.0)*anew(1.0,1.0)*dcosdrl*dsindrl-2.0*a(
     . 2.0,0.0)*anew(2.0,1.0)*anew(0.0,1.0)*dsindrl**2-2.0*a(2.0,0.0)
     . *anew(2.0,0.0)*anew(0.0,2.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(
     . 1.0,2.0)*anew(1.0,0.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(1.0,2.0)
     . *dcosdrl*dsindrl-a(2.0,0.0)*anew(1.0,1.0)**2*dsindrl**2-a(1.0,
     . 2.0)*anew(2.0,0.0)*dsindrl-a(1.0,1.0)*anew(2.0,1.0)*dsindrl
      ans4=-6.0*a(4.0,0.0)*anew(0.0,1.0)**2*dcosdrl**2*dsindrl**2-3.0
     . *a(3.0,2.0)*anew(2.0,0.0)*anew(0.0,0.0)**2*dsindrl**3-3.0*a(
     . 3.0,2.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0,
     . 2.0)*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2-3.0*a(3.0,
     . 2.0)*anew(0.0,0.0)*dcosdrl**2*dsindrl-3.0*a(3.0,1.0)*anew(2.0,
     . 1.0)*anew(0.0,0.0)**2*dsindrl**3-6.0*a(3.0,1.0)*anew(2.0,0.0)*
     . anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0,1.0)*anew(1.0
     . ,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0,1.0)*
     . anew(1.0,1.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2-3.0*a(3.0,1.0)*
     . anew(1.0,0.0)**2*anew(0.0,1.0)*dsindrl**3-6.0*a(3.0,1.0)*anew(
     . 1.0,0.0)*anew(0.0,1.0)*dcosdrl*dsindrl**2-3.0*a(3.0,1.0)*anew(
     . 0.0,1.0)*dcosdrl**2*dsindrl-6.0*a(3.0,0.0)*anew(2.0,1.0)*anew(
     . 0.0,1.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(2.0,0.0)
     . *anew(0.0,2.0)*anew(0.0,0.0)*dsindrl**3-3.0*a(3.0,0.0)*anew(
     . 2.0,0.0)*anew(0.0,1.0)**2*dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,
     . 2.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0,0.0)*
     . anew(1.0,2.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2-3.0*a(3.0,0.0)*
     . anew(1.0,1.0)**2*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(
     . 1.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)*dsindrl**3-6.0*a(3.0,0.0)
     . *anew(1.0,1.0)*anew(0.0,1.0)*dcosdrl*dsindrl**2+ans5
      ans3=-12.0*a(4.0,1.0)*anew(1.0,1.0)*anew(0.0,0.0)**2*dcosdrl*
     . dsindrl**3-12.0*a(4.0,1.0)*anew(1.0,0.0)**2*anew(0.0,1.0)*anew
     . (0.0,0.0)*dsindrl**4-24.0*a(4.0,1.0)*anew(1.0,0.0)*anew(0.0,
     . 1.0)*anew(0.0,0.0)*dcosdrl*dsindrl**3-12.0*a(4.0,1.0)*anew(0.0
     . ,1.0)*anew(0.0,0.0)*dcosdrl**2*dsindrl**2-12.0*a(4.0,0.0)*anew
     . (2.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0
     . ,0.0)*anew(2.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*dsindrl**4-
     . 12.0*a(4.0,0.0)*anew(2.0,0.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*
     . dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,2.0)*anew(1.0,0.0)*anew(
     . 0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,2.0)*anew(0.0,
     . 0.0)**2*dcosdrl*dsindrl**3-6.0*a(4.0,0.0)*anew(1.0,1.0)**2*
     . anew(0.0,0.0)**2*dsindrl**4-24.0*a(4.0,0.0)*anew(1.0,1.0)*anew
     . (1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**4-24.0*a(4.0,
     . 0.0)*anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrl*dsindrl
     . **3-12.0*a(4.0,0.0)*anew(1.0,0.0)**2*anew(0.0,2.0)*anew(0.0,
     . 0.0)*dsindrl**4-6.0*a(4.0,0.0)*anew(1.0,0.0)**2*anew(0.0,1.0)
     . **2*dsindrl**4-24.0*a(4.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*
     . anew(0.0,0.0)*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(1.0,0.0)
     . *anew(0.0,1.0)**2*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(0.0,
     . 2.0)*anew(0.0,0.0)*dcosdrl**2*dsindrl**2+ans4
      ans2=-20.0*a(5.0,0.0)*anew(2.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **3*dsindrl**5-20.0*a(5.0,0.0)*anew(2.0,0.0)*anew(0.0,2.0)*
     . anew(0.0,0.0)**3*dsindrl**5-30.0*a(5.0,0.0)*anew(2.0,0.0)*anew
     . (0.0,1.0)**2*anew(0.0,0.0)**2*dsindrl**5-20.0*a(5.0,0.0)*anew(
     . 1.0,2.0)*anew(1.0,0.0)*anew(0.0,0.0)**3*dsindrl**5-20.0*a(5.0,
     . 0.0)*anew(1.0,2.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4-10.0*a(
     . 5.0,0.0)*anew(1.0,1.0)**2*anew(0.0,0.0)**3*dsindrl**5-60.0*a(
     . 5.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,
     . 0.0)**2*dsindrl**5-60.0*a(5.0,0.0)*anew(1.0,1.0)*anew(0.0,1.0)
     . *anew(0.0,0.0)**2*dcosdrl*dsindrl**4-30.0*a(5.0,0.0)*anew(1.0,
     . 0.0)**2*anew(0.0,2.0)*anew(0.0,0.0)**2*dsindrl**5-30.0*a(5.0,
     . 0.0)*anew(1.0,0.0)**2*anew(0.0,1.0)**2*anew(0.0,0.0)*dsindrl**
     . 5-60.0*a(5.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)**2
     . *dcosdrl*dsindrl**4-60.0*a(5.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0
     . )**2*anew(0.0,0.0)*dcosdrl*dsindrl**4-30.0*a(5.0,0.0)*anew(0.0
     . ,2.0)*anew(0.0,0.0)**2*dcosdrl**2*dsindrl**3-30.0*a(5.0,0.0)*
     . anew(0.0,1.0)**2*anew(0.0,0.0)*dcosdrl**2*dsindrl**3-4.0*a(4.0
     . ,1.0)*anew(2.0,1.0)*anew(0.0,0.0)**3*dsindrl**4-12.0*a(4.0,1.0
     . )*anew(2.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl**4-12.0
     . *a(4.0,1.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*
     . dsindrl**4+ans3
      ans1=dcosdrl*ans2
      anew(2.0,2.0)=ans1/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      ans8=-2.0*a(2.0,0.0)*anew(2.0,2.0)*anew(0.0,1.0)*dsindrl**2-2.0
     . *a(2.0,0.0)*anew(2.0,1.0)*anew(0.0,2.0)*dsindrl**2-2.0*a(2.0,
     . 0.0)*anew(2.0,0.0)*anew(0.0,3.0)*dsindrl**2-2.0*a(2.0,0.0)*
     . anew(1.0,3.0)*anew(1.0,0.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(1.0
     . ,3.0)*dcosdrl*dsindrl-2.0*a(2.0,0.0)*anew(1.0,2.0)*anew(1.0,
     . 1.0)*dsindrl**2-a(1.0,3.0)*anew(2.0,0.0)*dsindrl-a(1.0,2.0)*
     . anew(2.0,1.0)*dsindrl-a(1.0,1.0)*anew(2.0,2.0)*dsindrl
      ans7=-6.0*a(3.0,0.0)*anew(1.0,2.0)*anew(1.0,1.0)*anew(0.0,0.0)*
     . dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,2.0)*anew(1.0,0.0)*anew(0.0
     . ,1.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,2.0)*anew(0.0,1.0)*
     . dcosdrl*dsindrl**2-3.0*a(3.0,0.0)*anew(1.0,1.0)**2*anew(0.0,
     . 1.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)*
     . anew(0.0,2.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,1.0)*anew(0.0
     . ,2.0)*dcosdrl*dsindrl**2-3.0*a(3.0,0.0)*anew(1.0,0.0)**2*anew(
     . 0.0,3.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,0.0)*anew(0.0,3.0)
     . *dcosdrl*dsindrl**2-3.0*a(3.0,0.0)*anew(0.0,3.0)*dcosdrl**2*
     . dsindrl-2.0*a(2.0,3.0)*anew(2.0,0.0)*anew(0.0,0.0)*dsindrl**2-
     . a(2.0,3.0)*anew(1.0,0.0)**2*dsindrl**2-2.0*a(2.0,3.0)*anew(1.0
     . ,0.0)*dcosdrl*dsindrl-a(2.0,3.0)*dcosdrl**2-2.0*a(2.0,2.0)*
     . anew(2.0,1.0)*anew(0.0,0.0)*dsindrl**2-2.0*a(2.0,2.0)*anew(2.0
     . ,0.0)*anew(0.0,1.0)*dsindrl**2-2.0*a(2.0,2.0)*anew(1.0,1.0)*
     . anew(1.0,0.0)*dsindrl**2-2.0*a(2.0,2.0)*anew(1.0,1.0)*dcosdrl*
     . dsindrl-2.0*a(2.0,1.0)*anew(2.0,2.0)*anew(0.0,0.0)*dsindrl**2-
     . 2.0*a(2.0,1.0)*anew(2.0,1.0)*anew(0.0,1.0)*dsindrl**2-2.0*a(
     . 2.0,1.0)*anew(2.0,0.0)*anew(0.0,2.0)*dsindrl**2-2.0*a(2.0,1.0)
     . *anew(1.0,2.0)*anew(1.0,0.0)*dsindrl**2-2.0*a(2.0,1.0)*anew(
     . 1.0,2.0)*dcosdrl*dsindrl-a(2.0,1.0)*anew(1.0,1.0)**2*dsindrl**
     . 2+ans8
      ans6=-3.0*a(3.0,2.0)*anew(0.0,1.0)*dcosdrl**2*dsindrl-3.0*a(3.0
     . ,1.0)*anew(2.0,2.0)*anew(0.0,0.0)**2*dsindrl**3-6.0*a(3.0,1.0)
     . *anew(2.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(
     . 3.0,1.0)*anew(2.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrl**3-
     . 3.0*a(3.0,1.0)*anew(2.0,0.0)*anew(0.0,1.0)**2*dsindrl**3-6.0*a
     . (3.0,1.0)*anew(1.0,2.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrl**3
     . -6.0*a(3.0,1.0)*anew(1.0,2.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2
     . -3.0*a(3.0,1.0)*anew(1.0,1.0)**2*anew(0.0,0.0)*dsindrl**3-6.0*
     . a(3.0,1.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)*dsindrl**
     . 3-6.0*a(3.0,1.0)*anew(1.0,1.0)*anew(0.0,1.0)*dcosdrl*dsindrl**
     . 2-3.0*a(3.0,1.0)*anew(1.0,0.0)**2*anew(0.0,2.0)*dsindrl**3-6.0
     . *a(3.0,1.0)*anew(1.0,0.0)*anew(0.0,2.0)*dcosdrl*dsindrl**2-3.0
     . *a(3.0,1.0)*anew(0.0,2.0)*dcosdrl**2*dsindrl-6.0*a(3.0,0.0)*
     . anew(2.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0
     . ,0.0)*anew(2.0,1.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrl**3-3.0
     . *a(3.0,0.0)*anew(2.0,1.0)*anew(0.0,1.0)**2*dsindrl**3-6.0*a(
     . 3.0,0.0)*anew(2.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)*dsindrl**3-
     . 6.0*a(3.0,0.0)*anew(2.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*
     . dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,3.0)*anew(1.0,0.0)*anew(0.0
     . ,0.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,3.0)*anew(0.0,0.0)*
     . dcosdrl*dsindrl**2+ans7
      ans5=-24.0*a(4.0,0.0)*anew(1.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . *dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(1.0,1.0)**2*anew(0.0,
     . 1.0)*anew(0.0,0.0)*dsindrl**4-24.0*a(4.0,0.0)*anew(1.0,1.0)*
     . anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrl**4-12.0*a(
     . 4.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)**2*dsindrl
     . **4-24.0*a(4.0,0.0)*anew(1.0,1.0)*anew(0.0,2.0)*anew(0.0,0.0)*
     . dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(1.0,1.0)*anew(0.0,1.0)
     . **2*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(1.0,0.0)**2*anew(
     . 0.0,3.0)*anew(0.0,0.0)*dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,0.0
     . )**2*anew(0.0,2.0)*anew(0.0,1.0)*dsindrl**4-24.0*a(4.0,0.0)*
     . anew(1.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)*dcosdrl*dsindrl**3-
     . 24.0*a(4.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*
     . dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)
     . *dcosdrl**2*dsindrl**2-12.0*a(4.0,0.0)*anew(0.0,2.0)*anew(0.0,
     . 1.0)*dcosdrl**2*dsindrl**2-3.0*a(3.0,2.0)*anew(2.0,1.0)*anew(
     . 0.0,0.0)**2*dsindrl**3-6.0*a(3.0,2.0)*anew(2.0,0.0)*anew(0.0,
     . 1.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0,2.0)*anew(1.0,1.0)*
     . anew(1.0,0.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0,2.0)*anew(1.0
     . ,1.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2-3.0*a(3.0,2.0)*anew(1.0
     . ,0.0)**2*anew(0.0,1.0)*dsindrl**3-6.0*a(3.0,2.0)*anew(1.0,0.0)
     . *anew(0.0,1.0)*dcosdrl*dsindrl**2+ans6
      ans4=-24.0*a(4.0,1.0)*anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . *dcosdrl*dsindrl**3-12.0*a(4.0,1.0)*anew(1.0,0.0)**2*anew(0.0,
     . 2.0)*anew(0.0,0.0)*dsindrl**4-6.0*a(4.0,1.0)*anew(1.0,0.0)**2*
     . anew(0.0,1.0)**2*dsindrl**4-24.0*a(4.0,1.0)*anew(1.0,0.0)*anew
     . (0.0,2.0)*anew(0.0,0.0)*dcosdrl*dsindrl**3-12.0*a(4.0,1.0)*
     . anew(1.0,0.0)*anew(0.0,1.0)**2*dcosdrl*dsindrl**3-12.0*a(4.0,
     . 1.0)*anew(0.0,2.0)*anew(0.0,0.0)*dcosdrl**2*dsindrl**2-6.0*a(
     . 4.0,1.0)*anew(0.0,1.0)**2*dcosdrl**2*dsindrl**2-12.0*a(4.0,0.0
     . )*anew(2.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl**4-12.0
     . *a(4.0,0.0)*anew(2.0,1.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*
     . dsindrl**4-12.0*a(4.0,0.0)*anew(2.0,1.0)*anew(0.0,1.0)**2*anew
     . (0.0,0.0)*dsindrl**4-12.0*a(4.0,0.0)*anew(2.0,0.0)*anew(0.0,
     . 3.0)*anew(0.0,0.0)**2*dsindrl**4-24.0*a(4.0,0.0)*anew(2.0,0.0)
     . *anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**4-4.0*a(
     . 4.0,0.0)*anew(2.0,0.0)*anew(0.0,1.0)**3*dsindrl**4-12.0*a(4.0,
     . 0.0)*anew(1.0,3.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dsindrl**4-
     . 12.0*a(4.0,0.0)*anew(1.0,3.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl
     . **3-12.0*a(4.0,0.0)*anew(1.0,2.0)*anew(1.0,1.0)*anew(0.0,0.0)
     . **2*dsindrl**4-24.0*a(4.0,0.0)*anew(1.0,2.0)*anew(1.0,0.0)*
     . anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**4+ans5
      ans3=-30.0*a(5.0,0.0)*anew(1.0,0.0)**2*anew(0.0,3.0)*anew(0.0,
     . 0.0)**2*dsindrl**5-60.0*a(5.0,0.0)*anew(1.0,0.0)**2*anew(0.0,
     . 2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**5-10.0*a(5.0,0.0)*
     . anew(1.0,0.0)**2*anew(0.0,1.0)**3*dsindrl**5-60.0*a(5.0,0.0)*
     . anew(1.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**
     . 4-120.0*a(5.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*
     . anew(0.0,0.0)*dcosdrl*dsindrl**4-20.0*a(5.0,0.0)*anew(1.0,0.0)
     . *anew(0.0,1.0)**3*dcosdrl*dsindrl**4-30.0*a(5.0,0.0)*anew(0.0,
     . 3.0)*anew(0.0,0.0)**2*dcosdrl**2*dsindrl**3-60.0*a(5.0,0.0)*
     . anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrl**2*dsindrl**
     . 3-10.0*a(5.0,0.0)*anew(0.0,1.0)**3*dcosdrl**2*dsindrl**3-4.0*a
     . (4.0,1.0)*anew(2.0,2.0)*anew(0.0,0.0)**3*dsindrl**4-12.0*a(4.0
     . ,1.0)*anew(2.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl**4-
     . 12.0*a(4.0,1.0)*anew(2.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*
     . dsindrl**4-12.0*a(4.0,1.0)*anew(2.0,0.0)*anew(0.0,1.0)**2*anew
     . (0.0,0.0)*dsindrl**4-12.0*a(4.0,1.0)*anew(1.0,2.0)*anew(1.0,
     . 0.0)*anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,1.0)*anew(1.0,2.0)
     . *anew(0.0,0.0)**2*dcosdrl*dsindrl**3-6.0*a(4.0,1.0)*anew(1.0,
     . 1.0)**2*anew(0.0,0.0)**2*dsindrl**4-24.0*a(4.0,1.0)*anew(1.0,
     . 1.0)*anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**4+ans4
      ans2=-20.0*a(5.0,0.0)*anew(2.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **3*dsindrl**5-20.0*a(5.0,0.0)*anew(2.0,1.0)*anew(0.0,2.0)*
     . anew(0.0,0.0)**3*dsindrl**5-30.0*a(5.0,0.0)*anew(2.0,1.0)*anew
     . (0.0,1.0)**2*anew(0.0,0.0)**2*dsindrl**5-20.0*a(5.0,0.0)*anew(
     . 2.0,0.0)*anew(0.0,3.0)*anew(0.0,0.0)**3*dsindrl**5-60.0*a(5.0,
     . 0.0)*anew(2.0,0.0)*anew(0.0,2.0)*anew(0.0,1.0)*anew(0.0,0.0)**
     . 2*dsindrl**5-20.0*a(5.0,0.0)*anew(2.0,0.0)*anew(0.0,1.0)**3*
     . anew(0.0,0.0)*dsindrl**5-20.0*a(5.0,0.0)*anew(1.0,3.0)*anew(
     . 1.0,0.0)*anew(0.0,0.0)**3*dsindrl**5-20.0*a(5.0,0.0)*anew(1.0,
     . 3.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4-20.0*a(5.0,0.0)*anew(
     . 1.0,2.0)*anew(1.0,1.0)*anew(0.0,0.0)**3*dsindrl**5-60.0*a(5.0,
     . 0.0)*anew(1.0,2.0)*anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**
     . 2*dsindrl**5-60.0*a(5.0,0.0)*anew(1.0,2.0)*anew(0.0,1.0)*anew(
     . 0.0,0.0)**2*dcosdrl*dsindrl**4-30.0*a(5.0,0.0)*anew(1.0,1.0)**
     . 2*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl**5-60.0*a(5.0,0.0)*
     . anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*
     . dsindrl**5-60.0*a(5.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(
     . 0.0,1.0)**2*anew(0.0,0.0)*dsindrl**5-60.0*a(5.0,0.0)*anew(1.0,
     . 1.0)*anew(0.0,2.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**4-60.0*a(
     . 5.0,0.0)*anew(1.0,1.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dcosdrl*
     . dsindrl**4+ans3
      ans1=dcosdrl*ans2
      anew(2.0,3.0)=ans1/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      ans1=dcosdrl*(-20.0*a(5.0,0.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew
     . (0.0,0.0)**3*dsindrl**5-20.0*a(5.0,0.0)*anew(2.0,0.0)*anew(0.0
     . ,0.0)**3*dcosdrl*dsindrl**4-10.0*a(5.0,0.0)*anew(1.0,0.0)**3*
     . anew(0.0,0.0)**2*dsindrl**5-30.0*a(5.0,0.0)*anew(1.0,0.0)**2*
     . anew(0.0,0.0)**2*dcosdrl*dsindrl**4-30.0*a(5.0,0.0)*anew(1.0,
     . 0.0)*anew(0.0,0.0)**2*dcosdrl**2*dsindrl**3-10.0*a(5.0,0.0)*
     . anew(0.0,0.0)**2*dcosdrl**3*dsindrl**2-12.0*a(4.0,0.0)*anew(
     . 2.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,
     . 0.0)*anew(2.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3-4.0*a(
     . 4.0,0.0)*anew(1.0,0.0)**3*anew(0.0,0.0)*dsindrl**4-12.0*a(4.0,
     . 0.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*dcosdrl*dsindrl**3-12.0*a(
     . 4.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrl**2*dsindrl**2-4.0
     . *a(4.0,0.0)*anew(0.0,0.0)*dcosdrl**3*dsindrl-6.0*a(3.0,0.0)*
     . anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0
     . ,0.0)*anew(2.0,0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2-a(3.0,0.0
     . )*anew(1.0,0.0)**3*dsindrl**3-3.0*a(3.0,0.0)*anew(1.0,0.0)**2*
     . dcosdrl*dsindrl**2-3.0*a(3.0,0.0)*anew(1.0,0.0)*dcosdrl**2*
     . dsindrl-a(3.0,0.0)*dcosdrl**3-2.0*a(2.0,0.0)*anew(2.0,0.0)*
     . anew(1.0,0.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(2.0,0.0)*dcosdrl*
     . dsindrl)
      anew(3.0,0.0)=ans1/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      ans4=-6.0*a(3.0,1.0)*anew(2.0,0.0)*anew(0.0,0.0)*dcosdrl*
     . dsindrl**2-a(3.0,1.0)*anew(1.0,0.0)**3*dsindrl**3-3.0*a(3.0,
     . 1.0)*anew(1.0,0.0)**2*dcosdrl*dsindrl**2-3.0*a(3.0,1.0)*anew(
     . 1.0,0.0)*dcosdrl**2*dsindrl-a(3.0,1.0)*dcosdrl**3-6.0*a(3.0,
     . 0.0)*anew(3.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**3-6.0*
     . a(3.0,0.0)*anew(2.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrl**
     . 3-6.0*a(3.0,0.0)*anew(2.0,1.0)*anew(0.0,0.0)*dcosdrl*dsindrl**
     . 2-6.0*a(3.0,0.0)*anew(2.0,0.0)*anew(1.0,1.0)*anew(0.0,0.0)*
     . dsindrl**3-6.0*a(3.0,0.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0
     . ,1.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(2.0,0.0)*anew(0.0,1.0)*
     . dcosdrl*dsindrl**2-3.0*a(3.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)
     . **2*dsindrl**3-6.0*a(3.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)*
     . dcosdrl*dsindrl**2-3.0*a(3.0,0.0)*anew(1.0,1.0)*dcosdrl**2*
     . dsindrl-2.0*a(2.0,1.0)*anew(3.0,0.0)*anew(0.0,0.0)*dsindrl**2-
     . 2.0*a(2.0,1.0)*anew(2.0,0.0)*anew(1.0,0.0)*dsindrl**2-2.0*a(
     . 2.0,1.0)*anew(2.0,0.0)*dcosdrl*dsindrl-2.0*a(2.0,0.0)*anew(3.0
     . ,0.0)*anew(0.0,1.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(2.0,1.0)*
     . anew(1.0,0.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(2.0,1.0)*dcosdrl*
     . dsindrl-2.0*a(2.0,0.0)*anew(2.0,0.0)*anew(1.0,1.0)*dsindrl**2-
     . a(1.0,1.0)*anew(3.0,0.0)*dsindrl
      ans3=-12.0*a(4.0,1.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*dcosdrl*
     . dsindrl**3-12.0*a(4.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrl
     . **2*dsindrl**2-4.0*a(4.0,1.0)*anew(0.0,0.0)*dcosdrl**3*dsindrl
     . -12.0*a(4.0,0.0)*anew(3.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*
     . dsindrl**4-12.0*a(4.0,0.0)*anew(2.0,1.0)*anew(1.0,0.0)*anew(
     . 0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(2.0,1.0)*anew(0.0,
     . 0.0)**2*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(2.0,0.0)*anew(
     . 1.0,1.0)*anew(0.0,0.0)**2*dsindrl**4-24.0*a(4.0,0.0)*anew(2.0,
     . 0.0)*anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**4-24.0
     . *a(4.0,0.0)*anew(2.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrl*
     . dsindrl**3-12.0*a(4.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)**2*anew
     . (0.0,0.0)*dsindrl**4-24.0*a(4.0,0.0)*anew(1.0,1.0)*anew(1.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(1.0
     . ,1.0)*anew(0.0,0.0)*dcosdrl**2*dsindrl**2-4.0*a(4.0,0.0)*anew(
     . 1.0,0.0)**3*anew(0.0,1.0)*dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,
     . 0.0)**2*anew(0.0,1.0)*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(
     . 1.0,0.0)*anew(0.0,1.0)*dcosdrl**2*dsindrl**2-4.0*a(4.0,0.0)*
     . anew(0.0,1.0)*dcosdrl**3*dsindrl-3.0*a(3.0,1.0)*anew(3.0,0.0)*
     . anew(0.0,0.0)**2*dsindrl**3-6.0*a(3.0,1.0)*anew(2.0,0.0)*anew(
     . 1.0,0.0)*anew(0.0,0.0)*dsindrl**3+ans4
      ans2=-20.0*a(5.0,0.0)*anew(3.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **3*dsindrl**5-20.0*a(5.0,0.0)*anew(2.0,1.0)*anew(1.0,0.0)*
     . anew(0.0,0.0)**3*dsindrl**5-20.0*a(5.0,0.0)*anew(2.0,1.0)*anew
     . (0.0,0.0)**3*dcosdrl*dsindrl**4-20.0*a(5.0,0.0)*anew(2.0,0.0)*
     . anew(1.0,1.0)*anew(0.0,0.0)**3*dsindrl**5-60.0*a(5.0,0.0)*anew
     . (2.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl
     . **5-60.0*a(5.0,0.0)*anew(2.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **2*dcosdrl*dsindrl**4-30.0*a(5.0,0.0)*anew(1.0,1.0)*anew(1.0,
     . 0.0)**2*anew(0.0,0.0)**2*dsindrl**5-60.0*a(5.0,0.0)*anew(1.0,
     . 1.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**4-30.0*a(
     . 5.0,0.0)*anew(1.0,1.0)*anew(0.0,0.0)**2*dcosdrl**2*dsindrl**3-
     . 20.0*a(5.0,0.0)*anew(1.0,0.0)**3*anew(0.0,1.0)*anew(0.0,0.0)*
     . dsindrl**5-60.0*a(5.0,0.0)*anew(1.0,0.0)**2*anew(0.0,1.0)*anew
     . (0.0,0.0)*dcosdrl*dsindrl**4-60.0*a(5.0,0.0)*anew(1.0,0.0)*
     . anew(0.0,1.0)*anew(0.0,0.0)*dcosdrl**2*dsindrl**3-20.0*a(5.0,
     . 0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrl**3*dsindrl**2-4.0*a(
     . 4.0,1.0)*anew(3.0,0.0)*anew(0.0,0.0)**3*dsindrl**4-12.0*a(4.0,
     . 1.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dsindrl**4-
     . 12.0*a(4.0,1.0)*anew(2.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl
     . **3-4.0*a(4.0,1.0)*anew(1.0,0.0)**3*anew(0.0,0.0)*dsindrl**4+
     . ans3
      ans1=dcosdrl*ans2
      anew(3.0,1.0)=ans1/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      ans8=-2.0*a(2.0,1.0)*anew(2.0,1.0)*dcosdrl*dsindrl-2.0*a(2.0,
     . 1.0)*anew(2.0,0.0)*anew(1.0,1.0)*dsindrl**2-2.0*a(2.0,0.0)*
     . anew(3.0,1.0)*anew(0.0,1.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(3.0
     . ,0.0)*anew(0.0,2.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(2.0,2.0)*
     . anew(1.0,0.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(2.0,2.0)*dcosdrl*
     . dsindrl-2.0*a(2.0,0.0)*anew(2.0,1.0)*anew(1.0,1.0)*dsindrl**2-
     . 2.0*a(2.0,0.0)*anew(2.0,0.0)*anew(1.0,2.0)*dsindrl**2-a(1.0,
     . 2.0)*anew(3.0,0.0)*dsindrl-a(1.0,1.0)*anew(3.0,1.0)*dsindrl
      ans7=-3.0*a(3.0,0.0)*anew(3.0,0.0)*anew(0.0,1.0)**2*dsindrl**3-
     . 6.0*a(3.0,0.0)*anew(2.0,2.0)*anew(1.0,0.0)*anew(0.0,0.0)*
     . dsindrl**3-6.0*a(3.0,0.0)*anew(2.0,2.0)*anew(0.0,0.0)*dcosdrl*
     . dsindrl**2-6.0*a(3.0,0.0)*anew(2.0,1.0)*anew(1.0,1.0)*anew(0.0
     . ,0.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(2.0,1.0)*anew(1.0,0.0)*
     . anew(0.0,1.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(2.0,1.0)*anew(0.0
     . ,1.0)*dcosdrl*dsindrl**2-6.0*a(3.0,0.0)*anew(2.0,0.0)*anew(1.0
     . ,2.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(2.0,0.0)*
     . anew(1.0,1.0)*anew(0.0,1.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(2.0
     . ,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*dsindrl**3-6.0*a(3.0,0.0)*
     . anew(2.0,0.0)*anew(0.0,2.0)*dcosdrl*dsindrl**2-3.0*a(3.0,0.0)*
     . anew(1.0,2.0)*anew(1.0,0.0)**2*dsindrl**3-6.0*a(3.0,0.0)*anew(
     . 1.0,2.0)*anew(1.0,0.0)*dcosdrl*dsindrl**2-3.0*a(3.0,0.0)*anew(
     . 1.0,2.0)*dcosdrl**2*dsindrl-3.0*a(3.0,0.0)*anew(1.0,1.0)**2*
     . anew(1.0,0.0)*dsindrl**3-3.0*a(3.0,0.0)*anew(1.0,1.0)**2*
     . dcosdrl*dsindrl**2-2.0*a(2.0,2.0)*anew(3.0,0.0)*anew(0.0,0.0)*
     . dsindrl**2-2.0*a(2.0,2.0)*anew(2.0,0.0)*anew(1.0,0.0)*dsindrl
     . **2-2.0*a(2.0,2.0)*anew(2.0,0.0)*dcosdrl*dsindrl-2.0*a(2.0,1.0
     . )*anew(3.0,1.0)*anew(0.0,0.0)*dsindrl**2-2.0*a(2.0,1.0)*anew(
     . 3.0,0.0)*anew(0.0,1.0)*dsindrl**2-2.0*a(2.0,1.0)*anew(2.0,1.0)
     . *anew(1.0,0.0)*dsindrl**2+ans8
      ans6=-12.0*a(4.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*dcosdrl**2*
     . dsindrl**2-4.0*a(4.0,0.0)*anew(0.0,2.0)*dcosdrl**3*dsindrl-3.0
     . *a(3.0,2.0)*anew(3.0,0.0)*anew(0.0,0.0)**2*dsindrl**3-6.0*a(
     . 3.0,2.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrl**3-
     . 6.0*a(3.0,2.0)*anew(2.0,0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2-
     . a(3.0,2.0)*anew(1.0,0.0)**3*dsindrl**3-3.0*a(3.0,2.0)*anew(1.0
     . ,0.0)**2*dcosdrl*dsindrl**2-3.0*a(3.0,2.0)*anew(1.0,0.0)*
     . dcosdrl**2*dsindrl-a(3.0,2.0)*dcosdrl**3-3.0*a(3.0,1.0)*anew(
     . 3.0,1.0)*anew(0.0,0.0)**2*dsindrl**3-6.0*a(3.0,1.0)*anew(3.0,
     . 0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0,1.0)*
     . anew(2.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(3.0
     . ,1.0)*anew(2.0,1.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2-6.0*a(3.0
     . ,1.0)*anew(2.0,0.0)*anew(1.0,1.0)*anew(0.0,0.0)*dsindrl**3-6.0
     . *a(3.0,1.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0)*dsindrl
     . **3-6.0*a(3.0,1.0)*anew(2.0,0.0)*anew(0.0,1.0)*dcosdrl*dsindrl
     . **2-3.0*a(3.0,1.0)*anew(1.0,1.0)*anew(1.0,0.0)**2*dsindrl**3-
     . 6.0*a(3.0,1.0)*anew(1.0,1.0)*anew(1.0,0.0)*dcosdrl*dsindrl**2-
     . 3.0*a(3.0,1.0)*anew(1.0,1.0)*dcosdrl**2*dsindrl-6.0*a(3.0,0.0)
     . *anew(3.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**3-6.0*a(
     . 3.0,0.0)*anew(3.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)*dsindrl**3+
     . ans7
      ans5=-24.0*a(4.0,0.0)*anew(2.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . *dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(2.0,0.0)*anew(1.0,2.0
     . )*anew(0.0,0.0)**2*dsindrl**4-24.0*a(4.0,0.0)*anew(2.0,0.0)*
     . anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**4-24.0*a(
     . 4.0,0.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,2.0)*anew(0.0,
     . 0.0)*dsindrl**4-12.0*a(4.0,0.0)*anew(2.0,0.0)*anew(1.0,0.0)*
     . anew(0.0,1.0)**2*dsindrl**4-24.0*a(4.0,0.0)*anew(2.0,0.0)*anew
     . (0.0,2.0)*anew(0.0,0.0)*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*
     . anew(2.0,0.0)*anew(0.0,1.0)**2*dcosdrl*dsindrl**3-12.0*a(4.0,
     . 0.0)*anew(1.0,2.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*dsindrl**4-
     . 24.0*a(4.0,0.0)*anew(1.0,2.0)*anew(1.0,0.0)*anew(0.0,0.0)*
     . dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(1.0,2.0)*anew(0.0,0.0)
     . *dcosdrl**2*dsindrl**2-12.0*a(4.0,0.0)*anew(1.0,1.0)**2*anew(
     . 1.0,0.0)*anew(0.0,0.0)*dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,1.0
     . )**2*anew(0.0,0.0)*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(1.0
     . ,1.0)*anew(1.0,0.0)**2*anew(0.0,1.0)*dsindrl**4-24.0*a(4.0,0.0
     . )*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)*dcosdrl*dsindrl**3
     . -12.0*a(4.0,0.0)*anew(1.0,1.0)*anew(0.0,1.0)*dcosdrl**2*
     . dsindrl**2-4.0*a(4.0,0.0)*anew(1.0,0.0)**3*anew(0.0,2.0)*
     . dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,0.0)**2*anew(0.0,2.0)*
     . dcosdrl*dsindrl**3+ans6
      ans4=-12.0*a(4.0,1.0)*anew(2.0,1.0)*anew(0.0,0.0)**2*dcosdrl*
     . dsindrl**3-12.0*a(4.0,1.0)*anew(2.0,0.0)*anew(1.0,1.0)*anew(
     . 0.0,0.0)**2*dsindrl**4-24.0*a(4.0,1.0)*anew(2.0,0.0)*anew(1.0,
     . 0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**4-24.0*a(4.0,1.0)*
     . anew(2.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrl*dsindrl**3-
     . 12.0*a(4.0,1.0)*anew(1.0,1.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*
     . dsindrl**4-24.0*a(4.0,1.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(
     . 0.0,0.0)*dcosdrl*dsindrl**3-12.0*a(4.0,1.0)*anew(1.0,1.0)*anew
     . (0.0,0.0)*dcosdrl**2*dsindrl**2-4.0*a(4.0,1.0)*anew(1.0,0.0)**
     . 3*anew(0.0,1.0)*dsindrl**4-12.0*a(4.0,1.0)*anew(1.0,0.0)**2*
     . anew(0.0,1.0)*dcosdrl*dsindrl**3-12.0*a(4.0,1.0)*anew(1.0,0.0)
     . *anew(0.0,1.0)*dcosdrl**2*dsindrl**2-4.0*a(4.0,1.0)*anew(0.0,
     . 1.0)*dcosdrl**3*dsindrl-12.0*a(4.0,0.0)*anew(3.0,1.0)*anew(0.0
     . ,1.0)*anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(3.0,0.0
     . )*anew(0.0,2.0)*anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*
     . anew(3.0,0.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*dsindrl**4-12.0*a
     . (4.0,0.0)*anew(2.0,2.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dsindrl
     . **4-12.0*a(4.0,0.0)*anew(2.0,2.0)*anew(0.0,0.0)**2*dcosdrl*
     . dsindrl**3-12.0*a(4.0,0.0)*anew(2.0,1.0)*anew(1.0,1.0)*anew(
     . 0.0,0.0)**2*dsindrl**4-24.0*a(4.0,0.0)*anew(2.0,1.0)*anew(1.0,
     . 0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**4+ans5
      ans3=-30.0*a(5.0,0.0)*anew(1.0,2.0)*anew(0.0,0.0)**2*dcosdrl**2
     . *dsindrl**3-30.0*a(5.0,0.0)*anew(1.0,1.0)**2*anew(1.0,0.0)*
     . anew(0.0,0.0)**2*dsindrl**5-30.0*a(5.0,0.0)*anew(1.0,1.0)**2*
     . anew(0.0,0.0)**2*dcosdrl*dsindrl**4-60.0*a(5.0,0.0)*anew(1.0,
     . 1.0)*anew(1.0,0.0)**2*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl**5-
     . 120.0*a(5.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)*
     . anew(0.0,0.0)*dcosdrl*dsindrl**4-60.0*a(5.0,0.0)*anew(1.0,1.0)
     . *anew(0.0,1.0)*anew(0.0,0.0)*dcosdrl**2*dsindrl**3-20.0*a(5.0,
     . 0.0)*anew(1.0,0.0)**3*anew(0.0,2.0)*anew(0.0,0.0)*dsindrl**5-
     . 10.0*a(5.0,0.0)*anew(1.0,0.0)**3*anew(0.0,1.0)**2*dsindrl**5-
     . 60.0*a(5.0,0.0)*anew(1.0,0.0)**2*anew(0.0,2.0)*anew(0.0,0.0)*
     . dcosdrl*dsindrl**4-30.0*a(5.0,0.0)*anew(1.0,0.0)**2*anew(0.0,
     . 1.0)**2*dcosdrl*dsindrl**4-60.0*a(5.0,0.0)*anew(1.0,0.0)*anew(
     . 0.0,2.0)*anew(0.0,0.0)*dcosdrl**2*dsindrl**3-30.0*a(5.0,0.0)*
     . anew(1.0,0.0)*anew(0.0,1.0)**2*dcosdrl**2*dsindrl**3-20.0*a(
     . 5.0,0.0)*anew(0.0,2.0)*anew(0.0,0.0)*dcosdrl**3*dsindrl**2-
     . 10.0*a(5.0,0.0)*anew(0.0,1.0)**2*dcosdrl**3*dsindrl**2-4.0*a(
     . 4.0,1.0)*anew(3.0,1.0)*anew(0.0,0.0)**3*dsindrl**4-12.0*a(4.0,
     . 1.0)*anew(3.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl**4-
     . 12.0*a(4.0,1.0)*anew(2.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*
     . dsindrl**4+ans4
      ans2=-20.0*a(5.0,0.0)*anew(3.0,1.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **3*dsindrl**5-20.0*a(5.0,0.0)*anew(3.0,0.0)*anew(0.0,2.0)*
     . anew(0.0,0.0)**3*dsindrl**5-30.0*a(5.0,0.0)*anew(3.0,0.0)*anew
     . (0.0,1.0)**2*anew(0.0,0.0)**2*dsindrl**5-20.0*a(5.0,0.0)*anew(
     . 2.0,2.0)*anew(1.0,0.0)*anew(0.0,0.0)**3*dsindrl**5-20.0*a(5.0,
     . 0.0)*anew(2.0,2.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4-20.0*a(
     . 5.0,0.0)*anew(2.0,1.0)*anew(1.0,1.0)*anew(0.0,0.0)**3*dsindrl
     . **5-60.0*a(5.0,0.0)*anew(2.0,1.0)*anew(1.0,0.0)*anew(0.0,1.0)*
     . anew(0.0,0.0)**2*dsindrl**5-60.0*a(5.0,0.0)*anew(2.0,1.0)*anew
     . (0.0,1.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**4-20.0*a(5.0,0.0)*
     . anew(2.0,0.0)*anew(1.0,2.0)*anew(0.0,0.0)**3*dsindrl**5-60.0*a
     . (5.0,0.0)*anew(2.0,0.0)*anew(1.0,1.0)*anew(0.0,1.0)*anew(0.0,
     . 0.0)**2*dsindrl**5-60.0*a(5.0,0.0)*anew(2.0,0.0)*anew(1.0,0.0)
     . *anew(0.0,2.0)*anew(0.0,0.0)**2*dsindrl**5-60.0*a(5.0,0.0)*
     . anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0)**2*anew(0.0,0.0)*
     . dsindrl**5-60.0*a(5.0,0.0)*anew(2.0,0.0)*anew(0.0,2.0)*anew(
     . 0.0,0.0)**2*dcosdrl*dsindrl**4-60.0*a(5.0,0.0)*anew(2.0,0.0)*
     . anew(0.0,1.0)**2*anew(0.0,0.0)*dcosdrl*dsindrl**4-30.0*a(5.0,
     . 0.0)*anew(1.0,2.0)*anew(1.0,0.0)**2*anew(0.0,0.0)**2*dsindrl**
     . 5-60.0*a(5.0,0.0)*anew(1.0,2.0)*anew(1.0,0.0)*anew(0.0,0.0)**2
     . *dcosdrl*dsindrl**4+ans3
      ans1=dcosdrl*ans2
      anew(3.0,2.0)=ans1/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      ans3=-4.0*a(4.0,0.0)*anew(1.0,0.0)**3*dcosdrl*dsindrl**3-6.0*a(
     . 4.0,0.0)*anew(1.0,0.0)**2*dcosdrl**2*dsindrl**2-4.0*a(4.0,0.0)
     . *anew(1.0,0.0)*dcosdrl**3*dsindrl-a(4.0,0.0)*dcosdrl**4-6.0*a(
     . 3.0,0.0)*anew(3.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrl**3-
     . 6.0*a(3.0,0.0)*anew(3.0,0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2-
     . 3.0*a(3.0,0.0)*anew(2.0,0.0)**2*anew(0.0,0.0)*dsindrl**3-3.0*a
     . (3.0,0.0)*anew(2.0,0.0)*anew(1.0,0.0)**2*dsindrl**3-6.0*a(3.0,
     . 0.0)*anew(2.0,0.0)*anew(1.0,0.0)*dcosdrl*dsindrl**2-3.0*a(3.0,
     . 0.0)*anew(2.0,0.0)*dcosdrl**2*dsindrl-2.0*a(2.0,0.0)*anew(3.0,
     . 0.0)*anew(1.0,0.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(3.0,0.0)*
     . dcosdrl*dsindrl-a(2.0,0.0)*anew(2.0,0.0)**2*dsindrl**2
      ans2=-20.0*a(5.0,0.0)*anew(3.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)
     . **3*dsindrl**5-20.0*a(5.0,0.0)*anew(3.0,0.0)*anew(0.0,0.0)**3*
     . dcosdrl*dsindrl**4-10.0*a(5.0,0.0)*anew(2.0,0.0)**2*anew(0.0,
     . 0.0)**3*dsindrl**5-30.0*a(5.0,0.0)*anew(2.0,0.0)*anew(1.0,0.0)
     . **2*anew(0.0,0.0)**2*dsindrl**5-60.0*a(5.0,0.0)*anew(2.0,0.0)*
     . anew(1.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**4-30.0*a(5.0,
     . 0.0)*anew(2.0,0.0)*anew(0.0,0.0)**2*dcosdrl**2*dsindrl**3-5.0*
     . a(5.0,0.0)*anew(1.0,0.0)**4*anew(0.0,0.0)*dsindrl**5-20.0*a(
     . 5.0,0.0)*anew(1.0,0.0)**3*anew(0.0,0.0)*dcosdrl*dsindrl**4-
     . 30.0*a(5.0,0.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*dcosdrl**2*
     . dsindrl**3-20.0*a(5.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrl
     . **3*dsindrl**2-5.0*a(5.0,0.0)*anew(0.0,0.0)*dcosdrl**4*dsindrl
     . -12.0*a(4.0,0.0)*anew(3.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*
     . dsindrl**4-12.0*a(4.0,0.0)*anew(3.0,0.0)*anew(0.0,0.0)**2*
     . dcosdrl*dsindrl**3-6.0*a(4.0,0.0)*anew(2.0,0.0)**2*anew(0.0,
     . 0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(2.0,0.0)*anew(1.0,0.0)
     . **2*anew(0.0,0.0)*dsindrl**4-24.0*a(4.0,0.0)*anew(2.0,0.0)*
     . anew(1.0,0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)
     . *anew(2.0,0.0)*anew(0.0,0.0)*dcosdrl**2*dsindrl**2-a(4.0,0.0)*
     . anew(1.0,0.0)**4*dsindrl**4+ans3
      ans1=dcosdrl*ans2
      anew(4.0,0.0)=ans1/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      ans6=-6.0*a(3.0,0.0)*anew(2.0,1.0)*anew(1.0,0.0)*dcosdrl*
     . dsindrl**2-3.0*a(3.0,0.0)*anew(2.0,1.0)*dcosdrl**2*dsindrl-3.0
     . *a(3.0,0.0)*anew(2.0,0.0)**2*anew(0.0,1.0)*dsindrl**3-6.0*a(
     . 3.0,0.0)*anew(2.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)*dsindrl**3-
     . 6.0*a(3.0,0.0)*anew(2.0,0.0)*anew(1.0,1.0)*dcosdrl*dsindrl**2-
     . 2.0*a(2.0,1.0)*anew(4.0,0.0)*anew(0.0,0.0)*dsindrl**2-2.0*a(
     . 2.0,1.0)*anew(3.0,0.0)*anew(1.0,0.0)*dsindrl**2-2.0*a(2.0,1.0)
     . *anew(3.0,0.0)*dcosdrl*dsindrl-a(2.0,1.0)*anew(2.0,0.0)**2*
     . dsindrl**2-2.0*a(2.0,0.0)*anew(4.0,0.0)*anew(0.0,1.0)*dsindrl
     . **2-2.0*a(2.0,0.0)*anew(3.0,1.0)*anew(1.0,0.0)*dsindrl**2-2.0*
     . a(2.0,0.0)*anew(3.0,1.0)*dcosdrl*dsindrl-2.0*a(2.0,0.0)*anew(
     . 3.0,0.0)*anew(1.0,1.0)*dsindrl**2-2.0*a(2.0,0.0)*anew(2.0,1.0)
     . *anew(2.0,0.0)*dsindrl**2-a(1.0,1.0)*anew(4.0,0.0)*dsindrl
      ans5=-12.0*a(4.0,0.0)*anew(2.0,0.0)*anew(0.0,1.0)*dcosdrl**2*
     . dsindrl**2-4.0*a(4.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)**3*
     . dsindrl**4-12.0*a(4.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)**2*
     . dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)
     . *dcosdrl**2*dsindrl**2-4.0*a(4.0,0.0)*anew(1.0,1.0)*dcosdrl**3
     . *dsindrl-3.0*a(3.0,1.0)*anew(4.0,0.0)*anew(0.0,0.0)**2*dsindrl
     . **3-6.0*a(3.0,1.0)*anew(3.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)*
     . dsindrl**3-6.0*a(3.0,1.0)*anew(3.0,0.0)*anew(0.0,0.0)*dcosdrl*
     . dsindrl**2-3.0*a(3.0,1.0)*anew(2.0,0.0)**2*anew(0.0,0.0)*
     . dsindrl**3-3.0*a(3.0,1.0)*anew(2.0,0.0)*anew(1.0,0.0)**2*
     . dsindrl**3-6.0*a(3.0,1.0)*anew(2.0,0.0)*anew(1.0,0.0)*dcosdrl*
     . dsindrl**2-3.0*a(3.0,1.0)*anew(2.0,0.0)*dcosdrl**2*dsindrl-6.0
     . *a(3.0,0.0)*anew(4.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dsindrl
     . **3-6.0*a(3.0,0.0)*anew(3.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*
     . dsindrl**3-6.0*a(3.0,0.0)*anew(3.0,1.0)*anew(0.0,0.0)*dcosdrl*
     . dsindrl**2-6.0*a(3.0,0.0)*anew(3.0,0.0)*anew(1.0,1.0)*anew(0.0
     . ,0.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(3.0,0.0)*anew(1.0,0.0)*
     . anew(0.0,1.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(3.0,0.0)*anew(0.0
     . ,1.0)*dcosdrl*dsindrl**2-6.0*a(3.0,0.0)*anew(2.0,1.0)*anew(2.0
     . ,0.0)*anew(0.0,0.0)*dsindrl**3-3.0*a(3.0,0.0)*anew(2.0,1.0)*
     . anew(1.0,0.0)**2*dsindrl**3+ans6
      ans4=-6.0*a(4.0,1.0)*anew(1.0,0.0)**2*dcosdrl**2*dsindrl**2-4.0
     . *a(4.0,1.0)*anew(1.0,0.0)*dcosdrl**3*dsindrl-a(4.0,1.0)*
     . dcosdrl**4-12.0*a(4.0,0.0)*anew(4.0,0.0)*anew(0.0,1.0)*anew(
     . 0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(3.0,1.0)*anew(1.0,
     . 0.0)*anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(3.0,1.0)
     . *anew(0.0,0.0)**2*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(3.0,
     . 0.0)*anew(1.0,1.0)*anew(0.0,0.0)**2*dsindrl**4-24.0*a(4.0,0.0)
     . *anew(3.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*
     . dsindrl**4-24.0*a(4.0,0.0)*anew(3.0,0.0)*anew(0.0,1.0)*anew(
     . 0.0,0.0)*dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(2.0,1.0)*anew
     . (2.0,0.0)*anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(2.0
     . ,1.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*dsindrl**4-24.0*a(4.0,0.0
     . )*anew(2.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**3
     . -12.0*a(4.0,0.0)*anew(2.0,1.0)*anew(0.0,0.0)*dcosdrl**2*
     . dsindrl**2-12.0*a(4.0,0.0)*anew(2.0,0.0)**2*anew(0.0,1.0)*anew
     . (0.0,0.0)*dsindrl**4-24.0*a(4.0,0.0)*anew(2.0,0.0)*anew(1.0,
     . 1.0)*anew(1.0,0.0)*anew(0.0,0.0)*dsindrl**4-24.0*a(4.0,0.0)*
     . anew(2.0,0.0)*anew(1.0,1.0)*anew(0.0,0.0)*dcosdrl*dsindrl**3-
     . 12.0*a(4.0,0.0)*anew(2.0,0.0)*anew(1.0,0.0)**2*anew(0.0,1.0)*
     . dsindrl**4-24.0*a(4.0,0.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(
     . 0.0,1.0)*dcosdrl*dsindrl**3+ans5
      ans3=-20.0*a(5.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)**3*anew(0.0,
     . 0.0)*dsindrl**5-60.0*a(5.0,0.0)*anew(1.0,1.0)*anew(1.0,0.0)**2
     . *anew(0.0,0.0)*dcosdrl*dsindrl**4-60.0*a(5.0,0.0)*anew(1.0,1.0
     . )*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrl**2*dsindrl**3-20.0*a(5.0
     . ,0.0)*anew(1.0,1.0)*anew(0.0,0.0)*dcosdrl**3*dsindrl**2-5.0*a(
     . 5.0,0.0)*anew(1.0,0.0)**4*anew(0.0,1.0)*dsindrl**5-20.0*a(5.0,
     . 0.0)*anew(1.0,0.0)**3*anew(0.0,1.0)*dcosdrl*dsindrl**4-30.0*a(
     . 5.0,0.0)*anew(1.0,0.0)**2*anew(0.0,1.0)*dcosdrl**2*dsindrl**3-
     . 20.0*a(5.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0)*dcosdrl**3*dsindrl
     . **2-5.0*a(5.0,0.0)*anew(0.0,1.0)*dcosdrl**4*dsindrl-4.0*a(4.0,
     . 1.0)*anew(4.0,0.0)*anew(0.0,0.0)**3*dsindrl**4-12.0*a(4.0,1.0)
     . *anew(3.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dsindrl**4-12.0*
     . a(4.0,1.0)*anew(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3-
     . 6.0*a(4.0,1.0)*anew(2.0,0.0)**2*anew(0.0,0.0)**2*dsindrl**4-
     . 12.0*a(4.0,1.0)*anew(2.0,0.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*
     . dsindrl**4-24.0*a(4.0,1.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(
     . 0.0,0.0)*dcosdrl*dsindrl**3-12.0*a(4.0,1.0)*anew(2.0,0.0)*anew
     . (0.0,0.0)*dcosdrl**2*dsindrl**2-a(4.0,1.0)*anew(1.0,0.0)**4*
     . dsindrl**4-4.0*a(4.0,1.0)*anew(1.0,0.0)**3*dcosdrl*dsindrl**3+
     . ans4
      ans2=-20.0*a(5.0,0.0)*anew(4.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **3*dsindrl**5-20.0*a(5.0,0.0)*anew(3.0,1.0)*anew(1.0,0.0)*
     . anew(0.0,0.0)**3*dsindrl**5-20.0*a(5.0,0.0)*anew(3.0,1.0)*anew
     . (0.0,0.0)**3*dcosdrl*dsindrl**4-20.0*a(5.0,0.0)*anew(3.0,0.0)*
     . anew(1.0,1.0)*anew(0.0,0.0)**3*dsindrl**5-60.0*a(5.0,0.0)*anew
     . (3.0,0.0)*anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)**2*dsindrl
     . **5-60.0*a(5.0,0.0)*anew(3.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)
     . **2*dcosdrl*dsindrl**4-20.0*a(5.0,0.0)*anew(2.0,1.0)*anew(2.0,
     . 0.0)*anew(0.0,0.0)**3*dsindrl**5-30.0*a(5.0,0.0)*anew(2.0,1.0)
     . *anew(1.0,0.0)**2*anew(0.0,0.0)**2*dsindrl**5-60.0*a(5.0,0.0)*
     . anew(2.0,1.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**
     . 4-30.0*a(5.0,0.0)*anew(2.0,1.0)*anew(0.0,0.0)**2*dcosdrl**2*
     . dsindrl**3-30.0*a(5.0,0.0)*anew(2.0,0.0)**2*anew(0.0,1.0)*anew
     . (0.0,0.0)**2*dsindrl**5-60.0*a(5.0,0.0)*anew(2.0,0.0)*anew(1.0
     . ,1.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dsindrl**5-60.0*a(5.0,0.0
     . )*anew(2.0,0.0)*anew(1.0,1.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl
     . **4-60.0*a(5.0,0.0)*anew(2.0,0.0)*anew(1.0,0.0)**2*anew(0.0,
     . 1.0)*anew(0.0,0.0)*dsindrl**5-120.0*a(5.0,0.0)*anew(2.0,0.0)*
     . anew(1.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*dcosdrl*dsindrl**4-
     . 60.0*a(5.0,0.0)*anew(2.0,0.0)*anew(0.0,1.0)*anew(0.0,0.0)*
     . dcosdrl**2*dsindrl**3+ans3
      ans1=dcosdrl*ans2
      anew(4.0,1.0)=ans1/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)
      ans4=-3.0*a(3.0,0.0)*anew(2.0,0.0)**2*dcosdrl*dsindrl**2-2.0*a(
     . 2.0,0.0)*anew(4.0,0.0)*anew(1.0,0.0)*dsindrl**2-2.0*a(2.0,0.0)
     . *anew(4.0,0.0)*dcosdrl*dsindrl-2.0*a(2.0,0.0)*anew(3.0,0.0)*
     . anew(2.0,0.0)*dsindrl**2
      ans3=-12.0*a(4.0,0.0)*anew(4.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)
     . **2*dsindrl**4-12.0*a(4.0,0.0)*anew(4.0,0.0)*anew(0.0,0.0)**2*
     . dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(3.0,0.0)*anew(2.0,0.0)
     . *anew(0.0,0.0)**2*dsindrl**4-12.0*a(4.0,0.0)*anew(3.0,0.0)*
     . anew(1.0,0.0)**2*anew(0.0,0.0)*dsindrl**4-24.0*a(4.0,0.0)*anew
     . (3.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**3-12.0*
     . a(4.0,0.0)*anew(3.0,0.0)*anew(0.0,0.0)*dcosdrl**2*dsindrl**2-
     . 12.0*a(4.0,0.0)*anew(2.0,0.0)**2*anew(1.0,0.0)*anew(0.0,0.0)*
     . dsindrl**4-12.0*a(4.0,0.0)*anew(2.0,0.0)**2*anew(0.0,0.0)*
     . dcosdrl*dsindrl**3-4.0*a(4.0,0.0)*anew(2.0,0.0)*anew(1.0,0.0)
     . **3*dsindrl**4-12.0*a(4.0,0.0)*anew(2.0,0.0)*anew(1.0,0.0)**2*
     . dcosdrl*dsindrl**3-12.0*a(4.0,0.0)*anew(2.0,0.0)*anew(1.0,0.0)
     . *dcosdrl**2*dsindrl**2-4.0*a(4.0,0.0)*anew(2.0,0.0)*dcosdrl**3
     . *dsindrl-6.0*a(3.0,0.0)*anew(4.0,0.0)*anew(1.0,0.0)*anew(0.0,
     . 0.0)*dsindrl**3-6.0*a(3.0,0.0)*anew(4.0,0.0)*anew(0.0,0.0)*
     . dcosdrl*dsindrl**2-6.0*a(3.0,0.0)*anew(3.0,0.0)*anew(2.0,0.0)*
     . anew(0.0,0.0)*dsindrl**3-3.0*a(3.0,0.0)*anew(3.0,0.0)*anew(1.0
     . ,0.0)**2*dsindrl**3-6.0*a(3.0,0.0)*anew(3.0,0.0)*anew(1.0,0.0)
     . *dcosdrl*dsindrl**2-3.0*a(3.0,0.0)*anew(3.0,0.0)*dcosdrl**2*
     . dsindrl-3.0*a(3.0,0.0)*anew(2.0,0.0)**2*anew(1.0,0.0)*dsindrl
     . **3+ans4
      ans2=-20.0*a(5.0,0.0)*anew(4.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)
     . **3*dsindrl**5-20.0*a(5.0,0.0)*anew(4.0,0.0)*anew(0.0,0.0)**3*
     . dcosdrl*dsindrl**4-20.0*a(5.0,0.0)*anew(3.0,0.0)*anew(2.0,0.0)
     . *anew(0.0,0.0)**3*dsindrl**5-30.0*a(5.0,0.0)*anew(3.0,0.0)*
     . anew(1.0,0.0)**2*anew(0.0,0.0)**2*dsindrl**5-60.0*a(5.0,0.0)*
     . anew(3.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**
     . 4-30.0*a(5.0,0.0)*anew(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl**2*
     . dsindrl**3-30.0*a(5.0,0.0)*anew(2.0,0.0)**2*anew(1.0,0.0)*anew
     . (0.0,0.0)**2*dsindrl**5-30.0*a(5.0,0.0)*anew(2.0,0.0)**2*anew(
     . 0.0,0.0)**2*dcosdrl*dsindrl**4-20.0*a(5.0,0.0)*anew(2.0,0.0)*
     . anew(1.0,0.0)**3*anew(0.0,0.0)*dsindrl**5-60.0*a(5.0,0.0)*anew
     . (2.0,0.0)*anew(1.0,0.0)**2*anew(0.0,0.0)*dcosdrl*dsindrl**4-
     . 60.0*a(5.0,0.0)*anew(2.0,0.0)*anew(1.0,0.0)*anew(0.0,0.0)*
     . dcosdrl**2*dsindrl**3-20.0*a(5.0,0.0)*anew(2.0,0.0)*anew(0.0,
     . 0.0)*dcosdrl**3*dsindrl**2-a(5.0,0.0)*anew(1.0,0.0)**5*dsindrl
     . **5-5.0*a(5.0,0.0)*anew(1.0,0.0)**4*dcosdrl*dsindrl**4-10.0*a(
     . 5.0,0.0)*anew(1.0,0.0)**3*dcosdrl**2*dsindrl**3-10.0*a(5.0,0.0
     . )*anew(1.0,0.0)**2*dcosdrl**3*dsindrl**2-5.0*a(5.0,0.0)*anew(
     . 1.0,0.0)*dcosdrl**4*dsindrl-a(5.0,0.0)*dcosdrl**5+ans3
      ans1=dcosdrl*ans2
      anew(5.0,0.0)=ans1/(5.0*a(5.0,0.0)*anew(0.0,0.0)**4*dcosdrl*
     . dsindrl**5+4.0*a(4.0,0.0)*anew(0.0,0.0)**3*dcosdrl*dsindrl**4+
     . 3.0*a(3.0,0.0)*anew(0.0,0.0)**2*dcosdrl*dsindrl**3+2.0*a(2.0,
     . 0.0)*anew(0.0,0.0)*dcosdrl*dsindrl**2+a(1.0,0.0)*dcosdrl*
     . dsindrl+dsindrl**2-1.0)

c UF back correction of the direction
      drl=- drl
      return
      end
c end /afs/psi.ch/user/f/flechsig/phase/src/phase/misali2.for
