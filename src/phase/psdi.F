c$$$ File      : phase_source.F
c$$$ Date      : <19 Nov 07 09:34:34 flechsig> 
c$$$ Time-stamp: <15 Dec 14 14:59:48 flechsig> 
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
c	Source Type:
c       ------------
c
c	isrctype = 1 : hard edge or Gaussian distribution of 
c                      divergence
c
c		isrcy = 0 : Gaussian distribution
c		isrcz = 0 : Gaussian distribution
c		isrcy = 1 : hard edge
c		isrcz = 1 : hard edge
c
c			--->  OUTPUT : dens real*8 	 
c
c       isrctype = 2 : Source: Zernike Polynomials
c                      Parameters in FIT_EY_RE.PAR 
c                      and FIT_EY_IM.PAR
c
c			--->  OUTPUT : densy 	 
c
c	isrctype = 3 ; Source from files EYRE0_500.SPL and
c                      EYIM0_500.SPL, linear interpolation,
c	               the files contain the radial distribution
c		       of Ey_real and Ey_imaginary
c		       we assume circular symmetry
c
c			--->  OUTPUT : densy 	 
c
c	isrctype = 4 ; Data from Files
c		       EYRE.DAT, EYIM.DAT
c		       EZRE.DAT, EZIM.DAT
c		       --> complete description of the source
c		       linear Interpolation on a 2D-grid
c
c			--->  OUTPUT : densy, densz 	 
c
c	isrctype = 5 ; Dipole Source, Analytical Representation
c
c			--->  OUTPUT : densy, densz 	 
c
c	isrctype = 6 ; Brightness Representation
c
c			--->  OUTPUT : densy 	 
c
c       isrctype = 7 ; identical to isrctype = 4 but:
c                      data from hdf5 file       
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


	implicit real*8(a-h,o-z)
	integer lostwl, blp

cc	include 'phase_struct.for'
#include 'phase_struct.F'
c**************************************************

	record /constants/ cs
	record /geometryst/ g
	record /rayst/ ra 
	record /source_results/ sr 
	record /control_flags/ ifl
c	record /source2/ so2
c	record /source3/ so3
c	record /source4/ so4
c	record /source5/ so5
c	record /source6/ so6
c	record /sources/ src
	record /apertures/ apr

#ifdef DEBUG1
	write(*,*),'DEBUG1: psdi called'
        call debug_beamline_type_f(blp)
#endif

c comment U.F. 20.4.2012	
c	write(*,*) 'UF psdi: source type:',src.isrctype

c---------------- set values to zero by default

	sr.dens=0.d0
	sr.densy=0.d0
	sr.densz=0.d0
	sr.eya=0.d0
	sr.eza=0.d0
	sr.eyp=0.d0
	sr.ezp=0.d0
	sr.densyre=0.d0
	sr.densyim=0.d0
	sr.denszre=0.d0
	sr.denszim=0.d0

c------------------------------------------------------
c	die folgenden drei Abfragen nach den Aperturen
c	koennen spaeter ausgelagert werden
c------------------------------------------------------

c---------------- Aperture in source plane ------------

	if((dsqrt(ra.rf.zp*ra.rf.zp+ra.rf.yp*ra.rf.yp).
     &		lt.apr.rpin).and.
     &		((ra.rf.yp.gt.apr.srcymin).and.
     &		 (ra.rf.yp.lt.apr.srcymax)).and.
     &		((ra.rf.zp.gt.apr.srczmin).and.
     &		(ra.rf.zp.lt.apr.srczmax)))then

c---------------- Aperture plane ----------------------
c	if( (dsqrt(ra.ap.yp_ap*ra.ap.yp_ap+
c     &		ra.ap.zp_ap*ra.ap.zp_ap).lt.apr.rpin_ap).and.
c     &		(ra.ap.yp_ap.gt.apr.ymin_ap).and.
c     &		(ra.ap.yp_ap.lt.apr.ymax_ap).and.
c     &		(ra.ap.zp_ap.gt.apr.zmin_ap).and.
c     &		(ra.ap.zp_ap.lt.apr.zmax_ap) ) then

c---------------- Finite size of OE--------------------

c	iii=1
c	if(ifl.ilimits.eq.1)then
c	    if( (ra.oe.w.lt.apr.w_min).or.   
c    &        (ra.oe.w.gt.apr.w_max).or.  
c    &        (ra.oe.xl.lt.apr.xl_min).or.  
c    &        (ra.oe.xl.gt.apr.xl_max) ) iii=0
c	endif
c	if(iii.eq.1)then

c	if(src.isrctype.eq.1)call src1(cs,g,ra,src.so1,sr,ifl)
c	if(src.isrctype.eq.2)call src2(cs,g,ra,src.so2,sr,ifl)
c	if(src.isrctype.eq.3)call src3(cs,g,ra,src.so3,sr,ifl)
	if(isrctype.eq.1)call src4(cs,g,ra,sr,ifl,lostwl,blp)
	if(isrctype.eq.4)call src4(cs,g,ra,sr,ifl,lostwl,blp)
	if(isrctype.eq.7)call src4(cs,g,ra,sr,ifl,lostwl,blp)
c uf2.7.13	if(src.isrctype.eq.4)call src4(cs,g,ra,src.so4,sr,ifl,blp)
c uf2.7.13	if(src.isrctype.eq.7)call src4(cs,g,ra,src.so4,sr,ifl,blp)
c	if(src.isrctype.eq.5)call src5(cs,g,ra,src.so5,sr,ifl)
c	if(src.isrctype.eq.6)call src6(cs,g,ra,src.so6,sr,ifl)

c	endif	! size of OE
c	endif	! aperture plane
	endif	! source plane

	return
        end
c---------------------- end psdi ------------------------------

c end
 
