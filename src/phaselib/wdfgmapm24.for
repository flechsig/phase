C Datei: USERDISK_3:[FLECHSIG.PHAS.PHASEFOR.LIB]WDFGMAPM24.FOR
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


      subroutine subm2(p2c,dz,p2rc)
      implicit real*8(a-h,o-z)

      dimension   p2c(0:4,0:4)
      dimension   p2rc(0:4) 

      P2RC(0.)=P2C(0.,4.)*DZ**4+P2C(0.,3.)*DZ**3+P2C(0.,2.
     . )*DZ**2+P2C(0.,1.)*DZ+P2C(0.,0.)
      P2RC(1.)=P2C(1.,3.)*DZ**3+P2C(1.,2.)*DZ**2+P2C(1.,1.
     . )*DZ+P2C(1.,0.)
      P2RC(2.)=P2C(2.,2.)*DZ**2+P2C(2.,1.)*DZ+P2C(2.,0.)
      P2RC(3.)=P2C(3.,1.)*DZ+P2C(3.,0.)
      P2RC(4.)=P2C(4.,0.)
      return
      end
