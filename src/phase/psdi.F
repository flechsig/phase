c$$$ File      : phase_source.F
c$$$ Date      : <19 Nov 07 09:34:34 flechsig> 
c$$$ Time-stamp: <12 Aug 15 10:06:57 flechsig> 
c$$$ Author    : Johannes Bahrdt
c$$$ Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

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

c UF extract from phase_source.F

c**************************************************************
        subroutine psdi(g,isrctype,apr,cs,ifl,ra,sr,lostwl,blp)
c**************************************************************
c
c       Berechnung der Phasenraumdichte in der Gegenstandsebene
c
c**************************************************************
c       blp is the integer pointer to the beamline struct
c**************************************************************
c
c
c       Optical Path Length:
c       --------------------
c
c	iexpand = 1 : power series expansion of 
c		      optical path length
c	iexpand = 0 : explicit calculation of
c		      optical path length
c
c**************************************************************
c UF 25.11.14 add isrctype = 1 new gauss source
c             this routine is more or less obsolete could be removed/replaced
c**************************************************

        implicit none

#include <../phase/phase_struct.F>

        integer lostwl, blp, isrctype

	TYPE(constants)      cs
	TYPE(geometryst)     g
	TYPE(rayst)          ra 
	TYPE(source_results) sr 
	TYPE(control_flags)  ifl
	TYPE(apertures)      apr

#ifdef DEBUG1
	write(*,*),'DEBUG1: psdi called'
        call debug_beamline_type_f(blp)
#endif

c comment U.F. 20.4.2012	
c	write(*,*) 'UF psdi: source type:',src.isrctype

c---------------- set values to zero by default

	sr%dens=0.d0
	sr%densy=0.d0
	sr%densz=0.d0
	sr%eya=0.d0
	sr%eza=0.d0
	sr%eyp=0.d0
	sr%ezp=0.d0
	sr%densyre=0.d0
	sr%densyim=0.d0
	sr%denszre=0.d0
	sr%denszim=0.d0

c------------------------------------------------------
c	die folgenden drei Abfragen nach den Aperturen
c	koennen spaeter ausgelagert werden
c------------------------------------------------------

c---------------- Aperture in source plane ------------
c#ifdef XXX
	if((dsqrt(ra%rf%zp*ra%rf%zp+ra%rf%yp*ra%rf%yp).
     &       lt.apr%rpin).and.
     &       ((ra%rf%yp.gt.apr%srcymin).and.
     &       (ra%rf%yp.lt.apr%srcymax)).and.
     &       ((ra%rf%zp.gt.apr%srczmin).and.
     &       (ra%rf%zp.lt.apr%srczmax)))then
c#endif          
           if(isrctype.eq.1)call src4(cs,g,ra,sr,ifl,lostwl,blp)
           if(isrctype.eq.4)call src4(cs,g,ra,sr,ifl,lostwl,blp)
           if(isrctype.eq.7)call src4(cs,g,ra,sr,ifl,lostwl,blp)
c#ifdef XXX           
        else  
           write(*,*)'psdi.F: warning: aperture restriction active'
	endif                   ! source plane
c#endif        
	return
        end
c----------------------end psdi ------------------------------
      
c     end
 
