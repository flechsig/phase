c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/int_2d.F
c$$$ Date      : <24 Jun 13 14:59:41 flechsig> 
c$$$ Time-stamp: <02 Sep 14 11:57:05 flechsig> 
c$$$ Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;
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


c UF 22.6.13 not used

c**************************************************************
	subroutine int_2d(f,xint,dy,dz,ny,nz)
c**************************************************************

	implicit real*8(a-h,o-z)

        dimension f(4096,4096),fz(4096)

        do i=1,ny
	 fz(i)=0.5d0*(f(i,1)+f(i,nz))
         do j=2,nz-1
          fz(i)=fz(i)+f(i,j)
         enddo
	 fz(i)=fz(i)*dz
	enddo

	xint=0.5d0*(fz(1)+fz(ny))
	do i=2,ny-1
         xint=xint+fz(i)
	enddo

	xint=xint*dy

	return
	end
c------------------------- end int_2d ----------------------
