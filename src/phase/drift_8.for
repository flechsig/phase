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
c-------------------------------------------------------------
	subroutine drift_8(g,acc,wc,
     &  xlc,ypc1,zpc1,dypc,dzpc,imodus,iord)
c-------------------------------------------------------------
	implicit real*8(a-h,o-z)
	structure/geometryst/
           real*8 sina,cosa,sinb,cosb,
     &            r,rp,xdens(0:4),xlam
           integer idefl
        end structure
        record /geometryst/g
        dimension   wc(0:7,0:7,0:7,0:7),
     &              xlc(0:7,0:7,0:7,0:7),
     &              ypc1(0:7,0:7,0:7,0:7),
     &              zpc1(0:7,0:7,0:7,0:7),
     &              dypc(0:7,0:7,0:7,0:7),
     &              dzpc(0:7,0:7,0:7,0:7)

        drift=g.r+g.rp
        do n1=0,iord
         do n2=0,iord-n1
          do n3=0,iord-n1-n2
           do n4=0,iord-n1-n2-n3
            ypc1(n1,n2,n3,n4)=0.d0
            zpc1(n1,n2,n3,n4)=0.d0
            dypc(n1,n2,n3,n4)=0.d0
            dzpc(n1,n2,n3,n4)=0.d0
            wc(n1,n2,n3,n4)=0.d0
            xlc(n1,n2,n3,n4)=0.d0
           enddo
          enddo
         enddo
        enddo

        ypc1(1,0,0,0)=1.d0
        ypc1(0,0,1,0)=drift
        zpc1(0,1,0,0)=1.d0
        zpc1(0,0,0,1)=drift

        dypc(0,0,1,0)=1.d0
        dzpc(0,0,0,1)=1.d0

	return
	end
