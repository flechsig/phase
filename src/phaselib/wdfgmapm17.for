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


c UF 25.6.13 diese Routine ist nicht thread safe- sie veraendert die input parameter
c p1c und p1rc

      subroutine subm17(p1c,y,z,p1rc)
      implicit real*8(a-h,o-z)
      dimension   p1c(0:7,0:7,0:7,0:7)
      dimension   p1rc(0:7,0:7)
      p1rc(0.0,0.0)=p1c(7.0,0.0,0.0,0.0)*y**7+p1c(6.0,1.0,0.0,0.0)*y
     . **6*z+p1c(6.0,0.0,0.0,0.0)*y**6+p1c(5.0,2.0,0.0,0.0)*y**5*z**2
     . +p1c(5.0,1.0,0.0,0.0)*y**5*z+p1c(5.0,0.0,0.0,0.0)*y**5+p1c(4.0
     . ,3.0,0.0,0.0)*y**4*z**3+p1c(4.0,2.0,0.0,0.0)*y**4*z**2+p1c(4.0
     . ,1.0,0.0,0.0)*y**4*z+p1c(4.0,0.0,0.0,0.0)*y**4+p1c(3.0,4.0,0.0
     . ,0.0)*y**3*z**4+p1c(3.0,3.0,0.0,0.0)*y**3*z**3+p1c(3.0,2.0,0.0
     . ,0.0)*y**3*z**2+p1c(3.0,1.0,0.0,0.0)*y**3*z+p1c(3.0,0.0,0.0,
     . 0.0)*y**3+p1c(2.0,5.0,0.0,0.0)*y**2*z**5+p1c(2.0,4.0,0.0,0.0)*
     . y**2*z**4+p1c(2.0,3.0,0.0,0.0)*y**2*z**3+p1c(2.0,2.0,0.0,0.0)*
     . y**2*z**2+p1c(2.0,1.0,0.0,0.0)*y**2*z+p1c(2.0,0.0,0.0,0.0)*y**
     . 2+p1c(1.0,6.0,0.0,0.0)*y*z**6+p1c(1.0,5.0,0.0,0.0)*y*z**5+p1c(
     . 1.0,4.0,0.0,0.0)*y*z**4+p1c(1.0,3.0,0.0,0.0)*y*z**3+p1c(1.0,
     . 2.0,0.0,0.0)*y*z**2+p1c(1.0,1.0,0.0,0.0)*y*z+p1c(1.0,0.0,0.0,
     . 0.0)*y+p1c(0.0,7.0,0.0,0.0)*z**7+p1c(0.0,6.0,0.0,0.0)*z**6+p1c
     . (0.0,5.0,0.0,0.0)*z**5+p1c(0.0,4.0,0.0,0.0)*z**4+p1c(0.0,3.0,
     . 0.0,0.0)*z**3+p1c(0.0,2.0,0.0,0.0)*z**2+p1c(0.0,1.0,0.0,0.0)*z
     . +p1c(0.0,0.0,0.0,0.0)
      p1rc(0.0,1.0)=p1c(6.0,0.0,0.0,1.0)*y**6+p1c(5.0,1.0,0.0,1.0)*y
     . **5*z+p1c(5.0,0.0,0.0,1.0)*y**5+p1c(4.0,2.0,0.0,1.0)*y**4*z**2
     . +p1c(4.0,1.0,0.0,1.0)*y**4*z+p1c(4.0,0.0,0.0,1.0)*y**4+p1c(3.0
     . ,3.0,0.0,1.0)*y**3*z**3+p1c(3.0,2.0,0.0,1.0)*y**3*z**2+p1c(3.0
     . ,1.0,0.0,1.0)*y**3*z+p1c(3.0,0.0,0.0,1.0)*y**3+p1c(2.0,4.0,0.0
     . ,1.0)*y**2*z**4+p1c(2.0,3.0,0.0,1.0)*y**2*z**3+p1c(2.0,2.0,0.0
     . ,1.0)*y**2*z**2+p1c(2.0,1.0,0.0,1.0)*y**2*z+p1c(2.0,0.0,0.0,
     . 1.0)*y**2+p1c(1.0,5.0,0.0,1.0)*y*z**5+p1c(1.0,4.0,0.0,1.0)*y*z
     . **4+p1c(1.0,3.0,0.0,1.0)*y*z**3+p1c(1.0,2.0,0.0,1.0)*y*z**2+
     . p1c(1.0,1.0,0.0,1.0)*y*z+p1c(1.0,0.0,0.0,1.0)*y+p1c(0.0,6.0,
     . 0.0,1.0)*z**6+p1c(0.0,5.0,0.0,1.0)*z**5+p1c(0.0,4.0,0.0,1.0)*z
     . **4+p1c(0.0,3.0,0.0,1.0)*z**3+p1c(0.0,2.0,0.0,1.0)*z**2+p1c(
     . 0.0,1.0,0.0,1.0)*z+p1c(0.0,0.0,0.0,1.0)
      p1rc(0.0,2.0)=p1c(5.0,0.0,0.0,2.0)*y**5+p1c(4.0,1.0,0.0,2.0)*y
     . **4*z+p1c(4.0,0.0,0.0,2.0)*y**4+p1c(3.0,2.0,0.0,2.0)*y**3*z**2
     . +p1c(3.0,1.0,0.0,2.0)*y**3*z+p1c(3.0,0.0,0.0,2.0)*y**3+p1c(2.0
     . ,3.0,0.0,2.0)*y**2*z**3+p1c(2.0,2.0,0.0,2.0)*y**2*z**2+p1c(2.0
     . ,1.0,0.0,2.0)*y**2*z+p1c(2.0,0.0,0.0,2.0)*y**2+p1c(1.0,4.0,0.0
     . ,2.0)*y*z**4+p1c(1.0,3.0,0.0,2.0)*y*z**3+p1c(1.0,2.0,0.0,2.0)*
     . y*z**2+p1c(1.0,1.0,0.0,2.0)*y*z+p1c(1.0,0.0,0.0,2.0)*y+p1c(0.0
     . ,5.0,0.0,2.0)*z**5+p1c(0.0,4.0,0.0,2.0)*z**4+p1c(0.0,3.0,0.0,
     . 2.0)*z**3+p1c(0.0,2.0,0.0,2.0)*z**2+p1c(0.0,1.0,0.0,2.0)*z+p1c
     . (0.0,0.0,0.0,2.0)
      p1rc(0.0,3.0)=p1c(4.0,0.0,0.0,3.0)*y**4+p1c(3.0,1.0,0.0,3.0)*y
     . **3*z+p1c(3.0,0.0,0.0,3.0)*y**3+p1c(2.0,2.0,0.0,3.0)*y**2*z**2
     . +p1c(2.0,1.0,0.0,3.0)*y**2*z+p1c(2.0,0.0,0.0,3.0)*y**2+p1c(1.0
     . ,3.0,0.0,3.0)*y*z**3+p1c(1.0,2.0,0.0,3.0)*y*z**2+p1c(1.0,1.0,
     . 0.0,3.0)*y*z+p1c(1.0,0.0,0.0,3.0)*y+p1c(0.0,4.0,0.0,3.0)*z**4+
     . p1c(0.0,3.0,0.0,3.0)*z**3+p1c(0.0,2.0,0.0,3.0)*z**2+p1c(0.0,
     . 1.0,0.0,3.0)*z+p1c(0.0,0.0,0.0,3.0)
      p1rc(0.0,4.0)=p1c(3.0,0.0,0.0,4.0)*y**3+p1c(2.0,1.0,0.0,4.0)*y
     . **2*z+p1c(2.0,0.0,0.0,4.0)*y**2+p1c(1.0,2.0,0.0,4.0)*y*z**2+
     . p1c(1.0,1.0,0.0,4.0)*y*z+p1c(1.0,0.0,0.0,4.0)*y+p1c(0.0,3.0,
     . 0.0,4.0)*z**3+p1c(0.0,2.0,0.0,4.0)*z**2+p1c(0.0,1.0,0.0,4.0)*z
     . +p1c(0.0,0.0,0.0,4.0)
      p1rc(0.0,5.0)=p1c(2.0,0.0,0.0,5.0)*y**2+p1c(1.0,1.0,0.0,5.0)*y*
     . z+p1c(1.0,0.0,0.0,5.0)*y+p1c(0.0,2.0,0.0,5.0)*z**2+p1c(0.0,1.0
     . ,0.0,5.0)*z+p1c(0.0,0.0,0.0,5.0)
      p1rc(0.0,6.0)=p1c(1.0,0.0,0.0,6.0)*y+p1c(0.0,1.0,0.0,6.0)*z+p1c
     . (0.0,0.0,0.0,6.0)
      p1rc(0.0,7.0)=p1c(0.0,0.0,0.0,7.0)
      p1rc(1.0,0.0)=p1c(6.0,0.0,1.0,0.0)*y**6+p1c(5.0,1.0,1.0,0.0)*y
     . **5*z+p1c(5.0,0.0,1.0,0.0)*y**5+p1c(4.0,2.0,1.0,0.0)*y**4*z**2
     . +p1c(4.0,1.0,1.0,0.0)*y**4*z+p1c(4.0,0.0,1.0,0.0)*y**4+p1c(3.0
     . ,3.0,1.0,0.0)*y**3*z**3+p1c(3.0,2.0,1.0,0.0)*y**3*z**2+p1c(3.0
     . ,1.0,1.0,0.0)*y**3*z+p1c(3.0,0.0,1.0,0.0)*y**3+p1c(2.0,4.0,1.0
     . ,0.0)*y**2*z**4+p1c(2.0,3.0,1.0,0.0)*y**2*z**3+p1c(2.0,2.0,1.0
     . ,0.0)*y**2*z**2+p1c(2.0,1.0,1.0,0.0)*y**2*z+p1c(2.0,0.0,1.0,
     . 0.0)*y**2+p1c(1.0,5.0,1.0,0.0)*y*z**5+p1c(1.0,4.0,1.0,0.0)*y*z
     . **4+p1c(1.0,3.0,1.0,0.0)*y*z**3+p1c(1.0,2.0,1.0,0.0)*y*z**2+
     . p1c(1.0,1.0,1.0,0.0)*y*z+p1c(1.0,0.0,1.0,0.0)*y+p1c(0.0,6.0,
     . 1.0,0.0)*z**6+p1c(0.0,5.0,1.0,0.0)*z**5+p1c(0.0,4.0,1.0,0.0)*z
     . **4+p1c(0.0,3.0,1.0,0.0)*z**3+p1c(0.0,2.0,1.0,0.0)*z**2+p1c(
     . 0.0,1.0,1.0,0.0)*z+p1c(0.0,0.0,1.0,0.0)
      p1rc(1.0,1.0)=p1c(5.0,0.0,1.0,1.0)*y**5+p1c(4.0,1.0,1.0,1.0)*y
     . **4*z+p1c(4.0,0.0,1.0,1.0)*y**4+p1c(3.0,2.0,1.0,1.0)*y**3*z**2
     . +p1c(3.0,1.0,1.0,1.0)*y**3*z+p1c(3.0,0.0,1.0,1.0)*y**3+p1c(2.0
     . ,3.0,1.0,1.0)*y**2*z**3+p1c(2.0,2.0,1.0,1.0)*y**2*z**2+p1c(2.0
     . ,1.0,1.0,1.0)*y**2*z+p1c(2.0,0.0,1.0,1.0)*y**2+p1c(1.0,4.0,1.0
     . ,1.0)*y*z**4+p1c(1.0,3.0,1.0,1.0)*y*z**3+p1c(1.0,2.0,1.0,1.0)*
     . y*z**2+p1c(1.0,1.0,1.0,1.0)*y*z+p1c(1.0,0.0,1.0,1.0)*y+p1c(0.0
     . ,5.0,1.0,1.0)*z**5+p1c(0.0,4.0,1.0,1.0)*z**4+p1c(0.0,3.0,1.0,
     . 1.0)*z**3+p1c(0.0,2.0,1.0,1.0)*z**2+p1c(0.0,1.0,1.0,1.0)*z+p1c
     . (0.0,0.0,1.0,1.0)
      p1rc(1.0,2.0)=p1c(4.0,0.0,1.0,2.0)*y**4+p1c(3.0,1.0,1.0,2.0)*y
     . **3*z+p1c(3.0,0.0,1.0,2.0)*y**3+p1c(2.0,2.0,1.0,2.0)*y**2*z**2
     . +p1c(2.0,1.0,1.0,2.0)*y**2*z+p1c(2.0,0.0,1.0,2.0)*y**2+p1c(1.0
     . ,3.0,1.0,2.0)*y*z**3+p1c(1.0,2.0,1.0,2.0)*y*z**2+p1c(1.0,1.0,
     . 1.0,2.0)*y*z+p1c(1.0,0.0,1.0,2.0)*y+p1c(0.0,4.0,1.0,2.0)*z**4+
     . p1c(0.0,3.0,1.0,2.0)*z**3+p1c(0.0,2.0,1.0,2.0)*z**2+p1c(0.0,
     . 1.0,1.0,2.0)*z+p1c(0.0,0.0,1.0,2.0)
      p1rc(1.0,3.0)=p1c(3.0,0.0,1.0,3.0)*y**3+p1c(2.0,1.0,1.0,3.0)*y
     . **2*z+p1c(2.0,0.0,1.0,3.0)*y**2+p1c(1.0,2.0,1.0,3.0)*y*z**2+
     . p1c(1.0,1.0,1.0,3.0)*y*z+p1c(1.0,0.0,1.0,3.0)*y+p1c(0.0,3.0,
     . 1.0,3.0)*z**3+p1c(0.0,2.0,1.0,3.0)*z**2+p1c(0.0,1.0,1.0,3.0)*z
     . +p1c(0.0,0.0,1.0,3.0)
      p1rc(1.0,4.0)=p1c(2.0,0.0,1.0,4.0)*y**2+p1c(1.0,1.0,1.0,4.0)*y*
     . z+p1c(1.0,0.0,1.0,4.0)*y+p1c(0.0,2.0,1.0,4.0)*z**2+p1c(0.0,1.0
     . ,1.0,4.0)*z+p1c(0.0,0.0,1.0,4.0)
      p1rc(1.0,5.0)=p1c(1.0,0.0,1.0,5.0)*y+p1c(0.0,1.0,1.0,5.0)*z+p1c
     . (0.0,0.0,1.0,5.0)
      p1rc(1.0,6.0)=p1c(0.0,0.0,1.0,6.0)
      p1rc(2.0,0.0)=p1c(5.0,0.0,2.0,0.0)*y**5+p1c(4.0,1.0,2.0,0.0)*y
     . **4*z+p1c(4.0,0.0,2.0,0.0)*y**4+p1c(3.0,2.0,2.0,0.0)*y**3*z**2
     . +p1c(3.0,1.0,2.0,0.0)*y**3*z+p1c(3.0,0.0,2.0,0.0)*y**3+p1c(2.0
     . ,3.0,2.0,0.0)*y**2*z**3+p1c(2.0,2.0,2.0,0.0)*y**2*z**2+p1c(2.0
     . ,1.0,2.0,0.0)*y**2*z+p1c(2.0,0.0,2.0,0.0)*y**2+p1c(1.0,4.0,2.0
     . ,0.0)*y*z**4+p1c(1.0,3.0,2.0,0.0)*y*z**3+p1c(1.0,2.0,2.0,0.0)*
     . y*z**2+p1c(1.0,1.0,2.0,0.0)*y*z+p1c(1.0,0.0,2.0,0.0)*y+p1c(0.0
     . ,5.0,2.0,0.0)*z**5+p1c(0.0,4.0,2.0,0.0)*z**4+p1c(0.0,3.0,2.0,
     . 0.0)*z**3+p1c(0.0,2.0,2.0,0.0)*z**2+p1c(0.0,1.0,2.0,0.0)*z+p1c
     . (0.0,0.0,2.0,0.0)
      p1rc(2.0,1.0)=p1c(4.0,0.0,2.0,1.0)*y**4+p1c(3.0,1.0,2.0,1.0)*y
     . **3*z+p1c(3.0,0.0,2.0,1.0)*y**3+p1c(2.0,2.0,2.0,1.0)*y**2*z**2
     . +p1c(2.0,1.0,2.0,1.0)*y**2*z+p1c(2.0,0.0,2.0,1.0)*y**2+p1c(1.0
     . ,3.0,2.0,1.0)*y*z**3+p1c(1.0,2.0,2.0,1.0)*y*z**2+p1c(1.0,1.0,
     . 2.0,1.0)*y*z+p1c(1.0,0.0,2.0,1.0)*y+p1c(0.0,4.0,2.0,1.0)*z**4+
     . p1c(0.0,3.0,2.0,1.0)*z**3+p1c(0.0,2.0,2.0,1.0)*z**2+p1c(0.0,
     . 1.0,2.0,1.0)*z+p1c(0.0,0.0,2.0,1.0)
      p1rc(2.0,2.0)=p1c(3.0,0.0,2.0,2.0)*y**3+p1c(2.0,1.0,2.0,2.0)*y
     . **2*z+p1c(2.0,0.0,2.0,2.0)*y**2+p1c(1.0,2.0,2.0,2.0)*y*z**2+
     . p1c(1.0,1.0,2.0,2.0)*y*z+p1c(1.0,0.0,2.0,2.0)*y+p1c(0.0,3.0,
     . 2.0,2.0)*z**3+p1c(0.0,2.0,2.0,2.0)*z**2+p1c(0.0,1.0,2.0,2.0)*z
     . +p1c(0.0,0.0,2.0,2.0)
      p1rc(2.0,3.0)=p1c(2.0,0.0,2.0,3.0)*y**2+p1c(1.0,1.0,2.0,3.0)*y*
     . z+p1c(1.0,0.0,2.0,3.0)*y+p1c(0.0,2.0,2.0,3.0)*z**2+p1c(0.0,1.0
     . ,2.0,3.0)*z+p1c(0.0,0.0,2.0,3.0)
      p1rc(2.0,4.0)=p1c(1.0,0.0,2.0,4.0)*y+p1c(0.0,1.0,2.0,4.0)*z+p1c
     . (0.0,0.0,2.0,4.0)
      p1rc(2.0,5.0)=p1c(0.0,0.0,2.0,5.0)
      p1rc(3.0,0.0)=p1c(4.0,0.0,3.0,0.0)*y**4+p1c(3.0,1.0,3.0,0.0)*y
     . **3*z+p1c(3.0,0.0,3.0,0.0)*y**3+p1c(2.0,2.0,3.0,0.0)*y**2*z**2
     . +p1c(2.0,1.0,3.0,0.0)*y**2*z+p1c(2.0,0.0,3.0,0.0)*y**2+p1c(1.0
     . ,3.0,3.0,0.0)*y*z**3+p1c(1.0,2.0,3.0,0.0)*y*z**2+p1c(1.0,1.0,
     . 3.0,0.0)*y*z+p1c(1.0,0.0,3.0,0.0)*y+p1c(0.0,4.0,3.0,0.0)*z**4+
     . p1c(0.0,3.0,3.0,0.0)*z**3+p1c(0.0,2.0,3.0,0.0)*z**2+p1c(0.0,
     . 1.0,3.0,0.0)*z+p1c(0.0,0.0,3.0,0.0)
      p1rc(3.0,1.0)=p1c(3.0,0.0,3.0,1.0)*y**3+p1c(2.0,1.0,3.0,1.0)*y
     . **2*z+p1c(2.0,0.0,3.0,1.0)*y**2+p1c(1.0,2.0,3.0,1.0)*y*z**2+
     . p1c(1.0,1.0,3.0,1.0)*y*z+p1c(1.0,0.0,3.0,1.0)*y+p1c(0.0,3.0,
     . 3.0,1.0)*z**3+p1c(0.0,2.0,3.0,1.0)*z**2+p1c(0.0,1.0,3.0,1.0)*z
     . +p1c(0.0,0.0,3.0,1.0)
      p1rc(3.0,2.0)=p1c(2.0,0.0,3.0,2.0)*y**2+p1c(1.0,1.0,3.0,2.0)*y*
     . z+p1c(1.0,0.0,3.0,2.0)*y+p1c(0.0,2.0,3.0,2.0)*z**2+p1c(0.0,1.0
     . ,3.0,2.0)*z+p1c(0.0,0.0,3.0,2.0)
      p1rc(3.0,3.0)=p1c(1.0,0.0,3.0,3.0)*y+p1c(0.0,1.0,3.0,3.0)*z+p1c
     . (0.0,0.0,3.0,3.0)
      p1rc(3.0,4.0)=p1c(0.0,0.0,3.0,4.0)
      p1rc(4.0,0.0)=p1c(3.0,0.0,4.0,0.0)*y**3+p1c(2.0,1.0,4.0,0.0)*y
     . **2*z+p1c(2.0,0.0,4.0,0.0)*y**2+p1c(1.0,2.0,4.0,0.0)*y*z**2+
     . p1c(1.0,1.0,4.0,0.0)*y*z+p1c(1.0,0.0,4.0,0.0)*y+p1c(0.0,3.0,
     . 4.0,0.0)*z**3+p1c(0.0,2.0,4.0,0.0)*z**2+p1c(0.0,1.0,4.0,0.0)*z
     . +p1c(0.0,0.0,4.0,0.0)
      p1rc(4.0,1.0)=p1c(2.0,0.0,4.0,1.0)*y**2+p1c(1.0,1.0,4.0,1.0)*y*
     . z+p1c(1.0,0.0,4.0,1.0)*y+p1c(0.0,2.0,4.0,1.0)*z**2+p1c(0.0,1.0
     . ,4.0,1.0)*z+p1c(0.0,0.0,4.0,1.0)
      p1rc(4.0,2.0)=p1c(1.0,0.0,4.0,2.0)*y+p1c(0.0,1.0,4.0,2.0)*z+p1c
     . (0.0,0.0,4.0,2.0)
      p1rc(4.0,3.0)=p1c(0.0,0.0,4.0,3.0)
      p1rc(5.0,0.0)=p1c(2.0,0.0,5.0,0.0)*y**2+p1c(1.0,1.0,5.0,0.0)*y*
     . z+p1c(1.0,0.0,5.0,0.0)*y+p1c(0.0,2.0,5.0,0.0)*z**2+p1c(0.0,1.0
     . ,5.0,0.0)*z+p1c(0.0,0.0,5.0,0.0)
      p1rc(5.0,1.0)=p1c(1.0,0.0,5.0,1.0)*y+p1c(0.0,1.0,5.0,1.0)*z+p1c
     . (0.0,0.0,5.0,1.0)
      p1rc(5.0,2.0)=p1c(0.0,0.0,5.0,2.0)
      p1rc(6.0,0.0)=p1c(1.0,0.0,6.0,0.0)*y+p1c(0.0,1.0,6.0,0.0)*z+p1c
     . (0.0,0.0,6.0,0.0)
      p1rc(6.0,1.0)=p1c(0.0,0.0,6.0,1.0)
      p1rc(7.0,0.0)=p1c(0.0,0.0,7.0,0.0)
      return
      end
