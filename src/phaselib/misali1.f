c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/misali1.f
c$$$ Date      : <16 Feb 04 16:39:00 flechsig> 
c$$$ Time-stamp: <02 Sep 14 12:12:14 flechsig> 

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


      subroutine misali1(a,anew,drz)
      implicit real*8(a-h,o-z)
      dimension a(0:5,0:5),anew(0:5,0:5)

c UF correction of the direction
      drz=- drz

      anew(0.0,0.0)=a(0.0,0.0)
      anew(0.0,1.0)=a(1.0,0.0)*dsin(drz)+a(0.0,1.0)*dcos(drz)
      anew(0.0,2.0)=a(2.0,0.0)*dsin(drz)**2+a(1.0,1.0)*dcos(drz)*dsin
     . (drz)+a(0.0,2.0)*dcos(drz)**2
      anew(0.0,3.0)=a(3.0,0.0)*dsin(drz)**3+a(2.0,1.0)*dcos(drz)*dsin
     . (drz)**2+a(1.0,2.0)*dcos(drz)**2*dsin(drz)+a(0.0,3.0)*dcos(drz
     . )**3
      anew(0.0,4.0)=a(4.0,0.0)*dsin(drz)**4+a(3.0,1.0)*dcos(drz)*dsin
     . (drz)**3+a(2.0,2.0)*dcos(drz)**2*dsin(drz)**2+a(1.0,3.0)*dcos(
     . drz)**3*dsin(drz)+a(0.0,4.0)*dcos(drz)**4
      anew(0.0,5.0)=a(5.0,0.0)*dsin(drz)**5+a(4.0,1.0)*dcos(drz)*dsin
     . (drz)**4+a(3.0,2.0)*dcos(drz)**2*dsin(drz)**3+a(2.0,3.0)*dcos(
     . drz)**3*dsin(drz)**2+a(1.0,4.0)*dcos(drz)**4*dsin(drz)+a(0.0,
     . 5.0)*dcos(drz)**5
      anew(1.0,0.0)=a(1.0,0.0)*dcos(drz)-a(0.0,1.0)*dsin(drz)
      anew(1.0,1.0)=2.0*a(2.0,0.0)*dcos(drz)*dsin(drz)+a(1.0,1.0)*
     . dcos(drz)**2-a(1.0,1.0)*dsin(drz)**2-2.0*a(0.0,2.0)*dcos(drz)*
     . dsin(drz)
      anew(1.0,2.0)=3.0*a(3.0,0.0)*dcos(drz)*dsin(drz)**2+2.0*a(2.0,
     . 1.0)*dcos(drz)**2*dsin(drz)-a(2.0,1.0)*dsin(drz)**3+a(1.0,2.0)
     . *dcos(drz)**3-2.0*a(1.0,2.0)*dcos(drz)*dsin(drz)**2-3.0*a(0.0,
     . 3.0)*dcos(drz)**2*dsin(drz)
      anew(1.0,3.0)=4.0*a(4.0,0.0)*dcos(drz)*dsin(drz)**3+3.0*a(3.0,
     . 1.0)*dcos(drz)**2*dsin(drz)**2-a(3.0,1.0)*dsin(drz)**4+2.0*a(
     . 2.0,2.0)*dcos(drz)**3*dsin(drz)-2.0*a(2.0,2.0)*dcos(drz)*dsin(
     . drz)**3+a(1.0,3.0)*dcos(drz)**4-3.0*a(1.0,3.0)*dcos(drz)**2*
     . dsin(drz)**2-4.0*a(0.0,4.0)*dcos(drz)**3*dsin(drz)
      anew(1.0,4.0)=5.0*a(5.0,0.0)*dcos(drz)*dsin(drz)**4+4.0*a(4.0,
     . 1.0)*dcos(drz)**2*dsin(drz)**3-a(4.0,1.0)*dsin(drz)**5+3.0*a(
     . 3.0,2.0)*dcos(drz)**3*dsin(drz)**2-2.0*a(3.0,2.0)*dcos(drz)*
     . dsin(drz)**4+2.0*a(2.0,3.0)*dcos(drz)**4*dsin(drz)-3.0*a(2.0,
     . 3.0)*dcos(drz)**2*dsin(drz)**3+a(1.0,4.0)*dcos(drz)**5-4.0*a(
     . 1.0,4.0)*dcos(drz)**3*dsin(drz)**2-5.0*a(0.0,5.0)*dcos(drz)**4
     . *dsin(drz)
      anew(2.0,0.0)=a(2.0,0.0)*dcos(drz)**2-a(1.0,1.0)*dcos(drz)*dsin
     . (drz)+a(0.0,2.0)*dsin(drz)**2
      anew(2.0,1.0)=3.0*a(3.0,0.0)*dcos(drz)**2*dsin(drz)+a(2.0,1.0)*
     . dcos(drz)**3-2.0*a(2.0,1.0)*dcos(drz)*dsin(drz)**2-2.0*a(1.0,
     . 2.0)*dcos(drz)**2*dsin(drz)+a(1.0,2.0)*dsin(drz)**3+3.0*a(0.0,
     . 3.0)*dcos(drz)*dsin(drz)**2
      anew(2.0,2.0)=6.0*a(4.0,0.0)*dcos(drz)**2*dsin(drz)**2+3.0*a(
     . 3.0,1.0)*dcos(drz)**3*dsin(drz)-3.0*a(3.0,1.0)*dcos(drz)*dsin(
     . drz)**3+a(2.0,2.0)*dcos(drz)**4-4.0*a(2.0,2.0)*dcos(drz)**2*
     . dsin(drz)**2+a(2.0,2.0)*dsin(drz)**4-3.0*a(1.0,3.0)*dcos(drz)
     . **3*dsin(drz)+3.0*a(1.0,3.0)*dcos(drz)*dsin(drz)**3+6.0*a(0.0,
     . 4.0)*dcos(drz)**2*dsin(drz)**2
      anew(2.0,3.0)=10.0*a(5.0,0.0)*dcos(drz)**2*dsin(drz)**3+6.0*a(
     . 4.0,1.0)*dcos(drz)**3*dsin(drz)**2-4.0*a(4.0,1.0)*dcos(drz)*
     . dsin(drz)**4+3.0*a(3.0,2.0)*dcos(drz)**4*dsin(drz)-6.0*a(3.0,
     . 2.0)*dcos(drz)**2*dsin(drz)**3+a(3.0,2.0)*dsin(drz)**5+a(2.0,
     . 3.0)*dcos(drz)**5-6.0*a(2.0,3.0)*dcos(drz)**3*dsin(drz)**2+3.0
     . *a(2.0,3.0)*dcos(drz)*dsin(drz)**4-4.0*a(1.0,4.0)*dcos(drz)**4
     . *dsin(drz)+6.0*a(1.0,4.0)*dcos(drz)**2*dsin(drz)**3+10.0*a(0.0
     . ,5.0)*dcos(drz)**3*dsin(drz)**2
      anew(3.0,0.0)=a(3.0,0.0)*dcos(drz)**3-a(2.0,1.0)*dcos(drz)**2*
     . dsin(drz)+a(1.0,2.0)*dcos(drz)*dsin(drz)**2-a(0.0,3.0)*dsin(
     . drz)**3
      anew(3.0,1.0)=4.0*a(4.0,0.0)*dcos(drz)**3*dsin(drz)+a(3.0,1.0)*
     . dcos(drz)**4-3.0*a(3.0,1.0)*dcos(drz)**2*dsin(drz)**2-2.0*a(
     . 2.0,2.0)*dcos(drz)**3*dsin(drz)+2.0*a(2.0,2.0)*dcos(drz)*dsin(
     . drz)**3+3.0*a(1.0,3.0)*dcos(drz)**2*dsin(drz)**2-a(1.0,3.0)*
     . dsin(drz)**4-4.0*a(0.0,4.0)*dcos(drz)*dsin(drz)**3
      anew(3.0,2.0)=10.0*a(5.0,0.0)*dcos(drz)**3*dsin(drz)**2+4.0*a(
     . 4.0,1.0)*dcos(drz)**4*dsin(drz)-6.0*a(4.0,1.0)*dcos(drz)**2*
     . dsin(drz)**3+a(3.0,2.0)*dcos(drz)**5-6.0*a(3.0,2.0)*dcos(drz)
     . **3*dsin(drz)**2+3.0*a(3.0,2.0)*dcos(drz)*dsin(drz)**4-3.0*a(
     . 2.0,3.0)*dcos(drz)**4*dsin(drz)+6.0*a(2.0,3.0)*dcos(drz)**2*
     . dsin(drz)**3-a(2.0,3.0)*dsin(drz)**5+6.0*a(1.0,4.0)*dcos(drz)
     . **3*dsin(drz)**2-4.0*a(1.0,4.0)*dcos(drz)*dsin(drz)**4-10.0*a(
     . 0.0,5.0)*dcos(drz)**2*dsin(drz)**3
      anew(4.0,0.0)=a(4.0,0.0)*dcos(drz)**4-a(3.0,1.0)*dcos(drz)**3*
     . dsin(drz)+a(2.0,2.0)*dcos(drz)**2*dsin(drz)**2-a(1.0,3.0)*dcos
     . (drz)*dsin(drz)**3+a(0.0,4.0)*dsin(drz)**4
      anew(4.0,1.0)=5.0*a(5.0,0.0)*dcos(drz)**4*dsin(drz)+a(4.0,1.0)*
     . dcos(drz)**5-4.0*a(4.0,1.0)*dcos(drz)**3*dsin(drz)**2-2.0*a(
     . 3.0,2.0)*dcos(drz)**4*dsin(drz)+3.0*a(3.0,2.0)*dcos(drz)**2*
     . dsin(drz)**3+3.0*a(2.0,3.0)*dcos(drz)**3*dsin(drz)**2-2.0*a(
     . 2.0,3.0)*dcos(drz)*dsin(drz)**4-4.0*a(1.0,4.0)*dcos(drz)**2*
     . dsin(drz)**3+a(1.0,4.0)*dsin(drz)**5+5.0*a(0.0,5.0)*dcos(drz)*
     . dsin(drz)**4
      anew(5.0,0.0)=a(5.0,0.0)*dcos(drz)**5-a(4.0,1.0)*dcos(drz)**4*
     . dsin(drz)+a(3.0,2.0)*dcos(drz)**3*dsin(drz)**2-a(2.0,3.0)*dcos
     . (drz)**2*dsin(drz)**3+a(1.0,4.0)*dcos(drz)*dsin(drz)**4-a(0.0,
     . 5.0)*dsin(drz)**5

c UF back correction of the direction
      drz=- drz
      return
      end
c /afs/psi.ch/user/f/flechsig/phase/src/phase/misali1.for
