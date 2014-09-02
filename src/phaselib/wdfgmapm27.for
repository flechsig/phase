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


      subroutine subm27(p2c,dz,p2rc)
      implicit real*8(a-h,o-z)
      dimension   p2c(0:7,0:7)
      dimension   p2rc(0:7)
      p2rc(0.0)=p2c(0.0,7.0)*dz**7+p2c(0.0,6.0)*dz**6+p2c(0.0,5.0)*dz
     . **5+p2c(0.0,4.0)*dz**4+p2c(0.0,3.0)*dz**3+p2c(0.0,2.0)*dz**2+
     . p2c(0.0,1.0)*dz+p2c(0.0,0.0)
      p2rc(1.0)=p2c(1.0,6.0)*dz**6+p2c(1.0,5.0)*dz**5+p2c(1.0,4.0)*dz
     . **4+p2c(1.0,3.0)*dz**3+p2c(1.0,2.0)*dz**2+p2c(1.0,1.0)*dz+p2c(
     . 1.0,0.0)
      p2rc(2.0)=p2c(2.0,5.0)*dz**5+p2c(2.0,4.0)*dz**4+p2c(2.0,3.0)*dz
     . **3+p2c(2.0,2.0)*dz**2+p2c(2.0,1.0)*dz+p2c(2.0,0.0)
      p2rc(3.0)=p2c(3.0,4.0)*dz**4+p2c(3.0,3.0)*dz**3+p2c(3.0,2.0)*dz
     . **2+p2c(3.0,1.0)*dz+p2c(3.0,0.0)
      p2rc(4.0)=p2c(4.0,3.0)*dz**3+p2c(4.0,2.0)*dz**2+p2c(4.0,1.0)*dz
     . +p2c(4.0,0.0)
      p2rc(5.0)=p2c(5.0,2.0)*dz**2+p2c(5.0,1.0)*dz+p2c(5.0,0.0)
      p2rc(6.0)=p2c(6.0,1.0)*dz+p2c(6.0,0.0)
      p2rc(7.0)=p2c(7.0,0.0)
      return
      end
