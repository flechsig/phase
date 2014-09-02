C Datei: USERDISK_3:[FLECHSIG.PHAS.PHASEFOR.LIB]WDFGMAPM14.FOR
C Datum: 18.NOV.1996
C Stand: 18-NOV-1996
C Autor: FLECHSIG, BESSY Berlin

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


      subroutine subm1(p1c,y,z,p1rc)
      implicit real*8(a-h,o-z)
      dimension   p1c(0:4,0:4,0:4,0:4)
      dimension   p1rc(0:4,0:4) 

      P1RC(0.,0.)=P1C(4.,0.,0.,0.)*Y**4+P1C(3.,1.,0.,0.)*Y
     . **3*Z+P1C(3.,0.,0.,0.)*Y**3+P1C(2.,2.,0.,0.)*Y**2*Z
     . **2+P1C(2.,1.,0.,0.)*Y**2*Z+P1C(2.,0.,0.,0.)*Y**2+
     . P1C(1.,3.,0.,0.)*Y*Z**3+P1C(1.,2.,0.,0.)*Y*Z**2+P1C
     . (1.,1.,0.,0.)*Y*Z+P1C(1.,0.,0.,0.)*Y+P1C(0.,4.,0.,
     . 0.)*Z**4+P1C(0.,3.,0.,0.)*Z**3+P1C(0.,2.,0.,0.)*Z**2
     . +P1C(0.,1.,0.,0.)*Z+P1C(0.,0.,0.,0.)
      P1RC(0.,1.)=P1C(3.,0.,0.,1.)*Y**3+P1C(2.,1.,0.,1.)*Y
     . **2*Z+P1C(2.,0.,0.,1.)*Y**2+P1C(1.,2.,0.,1.)*Y*Z**2
     . +P1C(1.,1.,0.,1.)*Y*Z+P1C(1.,0.,0.,1.)*Y+P1C(0.,3.,
     . 0.,1.)*Z**3+P1C(0.,2.,0.,1.)*Z**2+P1C(0.,1.,0.,1.)*Z
     . +P1C(0.,0.,0.,1.)
      P1RC(0.,2.)=P1C(2.,0.,0.,2.)*Y**2+P1C(1.,1.,0.,2.)*Y
     . *Z+P1C(1.,0.,0.,2.)*Y+P1C(0.,2.,0.,2.)*Z**2+P1C(0.,
     . 1.,0.,2.)*Z+P1C(0.,0.,0.,2.)
      P1RC(0.,3.)=P1C(1.,0.,0.,3.)*Y+P1C(0.,1.,0.,3.)*Z+
     . P1C(0.,0.,0.,3.)
      P1RC(0.,4.)=P1C(0.,0.,0.,4.)
      P1RC(1.,0.)=P1C(3.,0.,1.,0.)*Y**3+P1C(2.,1.,1.,0.)*Y
     . **2*Z+P1C(2.,0.,1.,0.)*Y**2+P1C(1.,2.,1.,0.)*Y*Z**2
     . +P1C(1.,1.,1.,0.)*Y*Z+P1C(1.,0.,1.,0.)*Y+P1C(0.,3.,
     . 1.,0.)*Z**3+P1C(0.,2.,1.,0.)*Z**2+P1C(0.,1.,1.,0.)*Z
     . +P1C(0.,0.,1.,0.)
      P1RC(1.,1.)=P1C(2.,0.,1.,1.)*Y**2+P1C(1.,1.,1.,1.)*Y
     . *Z+P1C(1.,0.,1.,1.)*Y+P1C(0.,2.,1.,1.)*Z**2+P1C(0.,
     . 1.,1.,1.)*Z+P1C(0.,0.,1.,1.)
      P1RC(1.,2.)=P1C(1.,0.,1.,2.)*Y+P1C(0.,1.,1.,2.)*Z+
     . P1C(0.,0.,1.,2.)
      P1RC(1.,3.)=P1C(0.,0.,1.,3.)
      P1RC(2.,0.)=P1C(2.,0.,2.,0.)*Y**2+P1C(1.,1.,2.,0.)*Y
     . *Z+P1C(1.,0.,2.,0.)*Y+P1C(0.,2.,2.,0.)*Z**2+P1C(0.,
     . 1.,2.,0.)*Z+P1C(0.,0.,2.,0.)
      P1RC(2.,1.)=P1C(1.,0.,2.,1.)*Y+P1C(0.,1.,2.,1.)*Z+
     . P1C(0.,0.,2.,1.)
      P1RC(2.,2.)=P1C(0.,0.,2.,2.)
      P1RC(3.,0.)=P1C(1.,0.,3.,0.)*Y+P1C(0.,1.,3.,0.)*Z+
     . P1C(0.,0.,3.,0.)
      P1RC(3.,1.)=P1C(0.,0.,3.,1.)
      P1RC(4.,0.)=P1C(0.,0.,4.,0.)
      return
      end
