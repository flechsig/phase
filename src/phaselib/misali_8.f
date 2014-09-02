c$$$ File      : /afs/psi.ch/project/phase/src/phase/misali_8.f
c$$$ Date      : <07 Mar 12 11:13:47 flechsig> 
c$$$ Time-stamp: <02 Sep 14 12:11:13 flechsig> 

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


	subroutine misali_8(a,anew,drz,drl,drw,dw,dl,dz)
	implicit real*8(a-h,o-z)

	dimension a(0:8,0:8),anew(0:8,0:8)

c----------- subtract offset
	a00=a(0,0)
	a(0,0)=0.d0

c----------- rotation around normal

	call misali1_8(a,anew,drz)

c----------- rotation around l-axis
	call misali2_8(anew,anew,drl)

c----------- rotation around w-axis
	call misali3_8(anew,anew,drw)

c----------- translation

c	dz=dz+a00     ! UF 7.3.2011 - not good we change input variable
	dz1=dz+a00
	call misali4_8(anew,anew,dw,dl,dz1)

	return
	end

