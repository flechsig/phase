c$$$ File      : phase_source.F
c$$$ Date      : <19 Nov 07 09:34:34 flechsig> 
c$$$ Time-stamp: <15 Dec 14 15:23:19 flechsig> 
c$$$ Author    : Johannes Bahrdt
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

c UF Dec 2014 routine is obsolete and no longer used

c
c--------------------------------------------------------------------------
c Quelltext phase_source_11.for von J. Bahrdt 20 Jan 2000
c differences to phase_source_10.for from 19 Jan 2000
c Aenderungen UF 20 Jan 2000
c   include 'phase_struct_11.for' in phase_struct.for
c   einige Kommentare eingefuegt
c   filenamen auf Strukturvariablen gesetzt
c   src3_ini structurvariablen eingefuegt
c
c	Input files are expected to be in mm
c	rather than in m 27.2.2004
c !!! preprocessor directives must start at line 0
c
c	!!!!!  ACHTUNG. es wird nur die Source 4 verwendet !!!!! JB 20.5.2011
c
c	spaeter kann man evetuell mal Source 6 testen und in Betrieb nehmen
c	sie beschreibt die vierdimensionale Brightness
c
c**************************************************************
c #define DEBUG1

	subroutine src_ini(src)	
c**************************************************************

	implicit real*8(a-h,o-z)

c a trick to have the struct only in the ./phase and find it from opti 

#include '../phase/phase_struct.F'


c**************************************************

c	record /source1/ so1
c	record /source2/ so2
c	record /source3/ so3
c	record /source4/ so4
c	record /source5/ so5
c	record /source6/ so6
	record /sources/ src

#ifdef OLD_PO_SOURCE
	write(*,*) 'src_ini: source type:',src.isrctype
	if(src.isrctype.lt.4.or.src.isrctype.gt.4)write(*,*)'unknown/obsolete source type-> calculation will fail'

c	if(src.isrctype.eq.2)call src2_ini(src.so2)
c	if(src.isrctype.eq.3)call src3_ini(src.so3)
	if(src.isrctype.eq.4)call src4_ini(src.so4)
c	if(src.isrctype.eq.5)write(*,*)'srctype=5 is currently not implemented'
c	if(src.isrctype.eq.6)call src6_ini(src.so6)
#else
	write(*,*) 'src_ini: obsolete routine called- send bug report'
#endif
	return
	end
c------------------end src_ini -------------------------

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


#ifdef OBSOLETE
c*****************************************************************
	subroutine src1(cs,g,ra,so1,sr,ifl)
c*****************************************************************
c	returns complex*16 densy
c	result is real part of complex variable
c----------------------------------------------
	implicit real*8(a-h,o-z)
cc	include 'phase_struct.for'
#include 'phase_struct.F'

	record /constants/ cs
	record /geometryst/ g
	record /rayst/ ra 
	record /source_results/ sr 
	record /control_flags/ ifl
	record /source1/ so1

	if(so1.isrcdy.eq.0)then
		sr.densy=exp(-(ra.rf.dyp*ra.rf.dyp)/
     &		(2.*so1.sigmayp*so1.sigmayp))
	      else
		if(abs(ra.rf.dyp).lt.so1.sigmayp)then
			sr.densy=1.d0
			else
			sr.densy=0.d0
		endif
	endif

	if(so1.isrcdz.eq.0)then
		sr.densy=sr.densy*
     &		exp(-(ra.rf.dzp*ra.rf.dzp)/
     &		(2.*so1.sigmazp*so1.sigmazp))
	      else
		if(abs(ra.rf.dzp).gt.so1.sigmazp)sr.densy=0.d0
	endif

	if(so1.isrcy.eq.0)then
		sr.densy=sr.densy*
     &		exp(-(ra.rf.yp*ra.rf.yp)/
     &		(2.*so1.sigmay*so1.sigmay))
	      else
		if(abs(ra.rf.yp).gt.so1.sigmay)sr.densy=0.d0
	endif

	if(so1.isrcz.eq.0)then
		sr.densy=sr.densy*
     &		exp(-(ra.rf.zp*ra.rf.zp)/
     &		(2.*so1.sigmaz*so1.sigmaz))
	      else
		if(abs(ra.rf.zp).gt.so1.sigmaz)sr.densy=0.d0
	endif

	return
	end
c-------------------- end src1 -------------------------------
#endif

#ifdef PHASELIB
c*****************************************************************
	subroutine src2(cs,g,ra,so2,sr,ifl)
c*****************************************************************
c	returns complex*16 densy
c-------------------------------
	implicit real*8(a-h,o-z)
cc	include 'phase_struct.for'
#include 'phase_struct.F'
	record /constants/ cs
	record /geometryst/ g
	record /rayst/ ra 
	record /source_results/ sr 
	record /control_flags/ ifl
	record /source2/ so2

c------	27.2.2004
c	if((dsqrt(ra.rf.zp*ra.rf.zp+ra.rf.yp*ra.rf.yp)/1000.).
c     &			ge.(0.9*so2.fscale))then
	if((dsqrt(ra.rf.zp*ra.rf.zp+ra.rf.yp*ra.rf.yp)).
     &			ge.(0.9*so2.fscale))then
     	sr.densy=0.d0
	else
c	    call source_zernike(so2,cs,ra.rf.zp/1000.,
c     &			ra.rf.yp/1000.,sr.densy)
	    call source_zernike(so2,cs,ra.rf.zp,
     &			ra.rf.yp,sr.densy)
	endif
c-----  27.2.2004

	if(ifl.ipinarr.eq.1)then
c  Achtung Übergabeparameter anpassen
c	  call pin_arr(src,ra.rf.yp,ra.rf.zp,f)
c	  sr.densy=sr.densy*f
	endif

	return
	end
c----------------------- end src2---------------------
#endif

c*****************************************************************
	subroutine src3(cs,g,ra,so3,sr,ifl)
c*****************************************************************
c	returns complex*16 densy
c-------------------------------
	implicit real*8(a-h,o-z)
cc	include 'phase_struct.for'
#include 'phase_struct.F'
	record /constants/ cs
	record /geometryst/ g
	record /rayst/ ra 
	record /source_results/ sr 
	record /control_flags/ ifl
	record /source3/ so3

c-------- 27.2.2004
c	call source_inter(so3,cs,ra.rf.zp/1000.,ra.rf.yp/1000.,
c     &			sr.densy)
	call source_inter(so3,cs,ra.rf.zp,ra.rf.yp,
     &			sr.densy)
c------- 27.2.2004

	if(ifl.ipinarr.eq.1)then
c        Achtung Übergabeparaneter anpassen
c	  call pin_arr(src,ra.rf.yp,ra.rf.zp,f)
c	  sr.densy=sr.densy*f
	endif

	return
	end
c----------------------- end src3---------------------

c*****************************************************************
c UF 2.7.13	subroutine src4(cs,g,ra,so4,sr,ifl,blp)
        subroutine src4(cs,g,ra,sr,ifl,lostwl,blp)
c*****************************************************************
c	returns complex*16 densy, densz
c--------------------------------------
	implicit real*8(a-h,o-z)
	complex*16 factor
	integer lostwl,blp               ! the beamline pointer 

cc	include 'phase_struct.for'
#include 'phase_struct.F'

	record /constants/ cs
	record /geometryst/ g
	record /rayst/ ra 
	record /source_results/ sr 
	record /control_flags/ ifl
#ifdef OLD_PO_SOURCE
c UF 2.7.13	record /source4/ so4
	record /source4/ so4
#endif
	phase= 0.0                  ! give a default

#ifdef DEBUG2
	write(*,*),'DEBUG1: src4 called'
        call debug_beamline_type_f(blp)
#endif
c	iexpand sollte ausser fuer debug-Zwecke immer auf 1 gesetzt sein
c--------- get path length
c	if(ifl.iexpand.eq.0)then
c
c	xxi=g.rp*g.cosb+dflotj(g.idefl)*ra.ri.yi*g.sinb
c	yyi=g.rp*g.sinb-dflotj(g.idefl)*ra.ri.yi*g.cosb
c	zzi=ra.ri.zi
c
c	xxp=g.r*g.cosa-dflotj(g.idefl)*ra.rf.yp*g.sina
c	yyp=g.r*g.sina+dflotj(g.idefl)*ra.rf.yp*g.cosa
c	zzp=ra.rf.zp
c
c	call intersection(am.a,wc,xlc,rin,uu,ww,xll,iord)
c
c	xlength=dsqrt(
c    &  (xxi-uu)*(xxi-uu)+(yyi-ww)*(yyi-ww)+(zzi-xll)*(zzi-xll))
c	ra.xlength1=xlength
c	xlength=xlength+
c     &  dsqrt((xxp-uu)*(xxp-uu)+(yyp-ww)*(yyp-ww)+(zzp-xll)*(zzp-xll))
c	ra.xlength2=xlength-ra.xlength1
c
c	else

	xlength=ra.xlength1+ra.xlength2

c	endif	! iexpand

c       Propagatioswellenlaenge steht in g.xlam

	if(ifl.iplmode.eq.0)then
c	phase = ((2.*cs.pi)*(xlength-g.r-g.rp))/ra.xlam_test 
        phase = ((2.*cs.pi)*(xlength-g.r-g.rp))/dabs(g.xlam) 
	endif
        
c-----------------------------------------------
c	ra.xlam_test wird variiert bei multiple propagation
c	so wie es sein soll
c-----------------------------------------------

	if(ifl.iplmode.eq.1)then
c	phase = (2.*cs.pi*xlength)/ra.xlam_test 
	phase = (2.*cs.pi*xlength)/dabs(g.xlam) 
c        write(6,*)'============= xlam_test ',ra.xlam_test
	endif

        if(ifl.ispline.lt.0)then
	sr.eyp=phase
	sr.ezp=phase
	endif

c--------------- end path length

c--------------- grating  

       delphi=0.d0

       if((ifl.igrating.eq.1).and.(g.xdens(0).gt.1.d-10))then

c********* aeussere Ordung      : xlam < 0
c********* innere Ordung        : xlam > 0
c********* Ablenkung nach oben  : idefl > 0
c********* Ablenkung nach unten : idefl < 0

	if (g.xlam.ge.0)isig=1
	if (g.xlam.lt.0)isig=-1

c------- new 19.1.2000 ----------------------------------
c	has been checked for non VLS grating 
c		(ifl.ispline = 0 and = -1)
c	has to be checked for VLS grating
c--------------------------------------------------------
c
c	22.3.2006

	w2=ra.oe.w*ra.oe.w
	w3=w2*ra.oe.w
	w4=w3*ra.oe.w
	w5=w4*ra.oe.w

c------- signs changed 17.6.2006
	xden=g.xdens(0)+		! local line density
     &		ra.oe.w*g.xdens(1)+
     &		w2*g.xdens(2)+
     &		w3*g.xdens(3)+
     &		w4*g.xdens(4)
	grdist=1.d0/xden		! local grid distance

	xnn=ra.oe.w*g.xdens(0)+	! line number
     &		(w2/2.d0)*g.xdens(1)+
     &		(w3/3.d0)*g.xdens(2)+
     &		(w4/4.d0)*g.xdens(3)+
     &		(w5/5.d0)*g.xdens(4)
	nn=xnn

	if(ifl.ispline.ge.0)then
 	  delphi=-dflotj(isig)*(xnn-dflotj(nn))*2.d0*cs.pi
	endif

	if(ifl.ispline.lt.0)then
	  delphi=-dflotj(isig)*xnn*2.d0*cs.pi
	  sr.eyp=sr.eyp+delphi
	  sr.ezp=sr.ezp+delphi
	endif
c------- end 19.1.2000 ----------------------------------

	endif			! grating

	factorre=dcos(delphi+phase)
        factorim=dsin(delphi+phase)
        if(ifl.ispline.ge.0)factor=exp(cs.sqrtm1*(delphi+phase))

        xwert=ra.rf.zp	        ! units are mm
	ywert=ra.rf.yp	        ! units are mm

c        if ((ifl.pst_mode.gt.2).or.(ifl.pst_mode.lt.0))then   ! for tests
#ifdef OLD_PO_SOURCE
c          original code von JB
	   if(  ((xwert.gt.so4.xeyremin)).and.
     &	     ((xwert.lt.so4.xeyremax)).and.
     &	     ((ywert.gt.so4.yeyremin)).and.
     &	     ((ywert.lt.so4.yeyremax)))then
	      call source_inter_2d(so4,cs,xwert,ywert,sr)
	   endif		! min, max
#else
c          new code with dynamic source does also the range test 
	   call source4c_inter_2d(sr,xwert,ywert,blp);


c code to deal with slope errors 
c contributed by Walan Grizolli modified and extended by UF
c procedure takes the beamline pointer, w and l and
c the electrical field picked up for the particular "ray" as input
c the field will be overwritten in place 
c routine does also the aperture check

        call apply_height_error(blp, lostwl, ra.oe.w, ra.oe.xl, sr.densyre, 
     &                          sr.densyim, sr.denszre, sr.denszim)

#endif
c	endif

	if(ifl.ipinarr.eq.1)then
c        Achtung Übergabeparaneter anpassen
c	  call pin_arr(src,ra.rf.yp,ra.rf.zp,f)
	  else
	  f=1.0d0
	endif

	srdensyre=sr.densyre
	srdensyim=sr.densyim
	srdenszre=sr.denszre
	srdenszim=sr.denszim
        
	if(ifl.ispline.ge.0)then
		sr.densyre=f*(srdensyre*factorre-
     &          	      srdensyim*factorim)
		sr.densyim=f*(srdensyre*factorim+
     &          	      srdensyim*factorre)
		sr.denszre=f*(srdenszre*factorre-
     &          	      srdenszim*factorim)
		sr.denszim=f*(srdenszre*factorim+
     &          	      srdenszim*factorre)
                sr.densy=(srdensyre+cs.sqrtm1*srdensyim)*factor
                sr.densz=(srdenszre+cs.sqrtm1*srdenszim)*factor
	endif

	if(ifl.ispline.lt.0)then
		sr.eya=sr.densyre*f
		sr.eyp=sr.eyp+sr.densyim
		sr.eza=sr.denszre*f
		sr.ezp=sr.ezp+sr.denszim
	endif

	return
	end
c----------------------- end src4---------------------

c*****************************************************************
	subroutine src5(cs,g,ra,so5,sr,ifl)

c*****************************************************************
c	returns complex*16 densy, densz
c--------------------------------------
	implicit real*8(a-h,o-z)
	complex*16 factor

cc	include 'phase_struct.for'
#include 'phase_struct.F'
	record /constants/ cs
	record /geometryst/ g
	record /rayst/ ra 
	record /source_results/ sr 
	record /control_flags/ ifl
	record /source5/ so5

c------------- get path length 
c
c	if(ifl.iexpand.eq.0)then
c
c	xxi=g.rp*g.cosb+dflotj(g.idefl)*ra.ri.yi*g.sinb
c	yyi=g.rp*g.sinb-dflotj(g.idefl)*ra.ri.yi*g.cosb
c	zzi=ra.ri.zi
c
c	xxp=g.r*g.cosa-dflotj(g.idefl)*ra.rf.yp*g.sina
c	yyp=g.r*g.sina+dflotj(g.idefl)*ra.rf.yp*g.cosa
c	zzp=ra.rf.zp
c
c	call intersection(uu,ww,xll)
c
c	xlength=dsqrt(
c     &  (xxi-uu)*(xxi-uu)+(yyi-ww)*(yyi-ww)+
c     &  (zzi-xll)*(zzi-xll))
c	ra.xlength1=xlength
c	xlength=xlength+
c     &  dsqrt((xxp-uu)*(xxp-uu)+(yyp-ww)*(yyp-ww)+
c     &  (zzp-xll)*(zzp-xll))
c	ra.xlength2=xlength-ra.xlength1
c
c	else

	xlength=ra.xlength1+ra.xlength2

c	endif	! iexpand

	if(ifl.iplmode.eq.0)then
c	phase = ((2.*cs.pi)*(xlength-g.r-g.rp))/ra.xlam_test 
        phase = ((2.*cs.pi)*(xlength-g.r-g.rp))/dabs(g.xlam) 
	endif

	if(ifl.iplmode.eq.1)then
c	phase = (2.*cs.pi*xlength)/ra.xlam_test 
        phase = (2.*cs.pi*xlength)/dabs(g.xlam)
	endif

	if(ifl.ispline.ge.0)factor=exp(cs.sqrtm1*phase)
	if(ifl.ispline.lt.0)then
	sr.eyp=phase
	sr.ezp=phase
	endif

c-------------- end path length

c-------------- grating 
	if((ifl.igrating.eq.1).and.(g.xdens(0).gt.1.d-10))then
	grdist=1.d0/g.xdens(0)
	xx=dmod(ra.oe.w,grdist)
	xx=xx+grdist
	xx=dmod(xx,grdist)

c********* aeussere Ordung      : xlam < 0
c********* innere Ordung        : xlam > 0
c********* Ablenkung nach oben  : idefl > 0
c********* Ablenkung nach unten : idefl < 0

	if (g.xlam.ge.0)isig=1
	if (g.xlam.lt.0)isig=-1

	delphi=dflotj(isig)*(xx/grdist)*2.d0*cs.pi

	if(ifl.ispline.ge.0)factor=factor*exp(cs.sqrtm1*delphi)
	if(ifl.ispline.lt.0)then

c------- neu 20.6.1997
	delphi=-dflotj(isig)*(ra.oe.w/grdist)*2.d0*cs.pi
c------- end neu 20.6.1997

	sr.eyp=sr.eyp+delphi
	sr.ezp=sr.ezp+delphi
	endif
c--------------------------------------------------------

c------- new 19.1.2000 ----------------------------------
c	has been checked for non VLS grating and source 4 
c		(ifl.ispline = 0 and = -1)
c	has to be checked for VLS grating source 4 and not 
c		at all 	for source 5
c--------------------------------------------------------
	xden=g.xdens(0)-		! local line density
     &		ra.oe.w*g.xdens(1)+
     &		ra.oe.w**2*g.xdens(2)-
     &		ra.oe.w**3*g.xdens(3)+
     &		ra.oe.w**4*g.xdens(4)
	grdist=1.d0/xden		! local grid distance

	xnn=ra.oe.w*g.xdens(0)-		! line number
     &		(ra.oe.w**2/2.d0)*g.xdens(1)+
     &		(ra.oe.w**3/3.d0)*g.xdens(2)-
     &		(ra.oe.w**4/4.d0)*g.xdens(3)+
     &		(ra.oe.w**5/5.d0)*g.xdens(4)
	nn=xnn

	if(ifl.ispline.ge.0)then
	  delphi=-dflotj(isig)*(xnn-dflotj(nn))*2.d0*cs.pi
	  factor=factor*exp(cs.sqrtm1*delphi)
	endif

	if(ifl.ispline.lt.0)then
	  delphi=-dflotj(isig)*xnn*2.d0*cs.pi
	  sr.eyp=sr.eyp+delphi
	  sr.ezp=sr.ezp+delphi
	endif
c------- end 19.1.2000 ----------------------------------

	endif		! grating

	xwert=ra.rf.zp	! units are mm
	ywert=ra.rf.yp          

c	PhiSrc=((2.d0*cs.pi)/ra.xlam_test)*
        PhiSrc=((2.d0*cs.pi)/g.xlam)*
     &     ( (xwert*xwert)/(2.d0*so5.dipdisz) +
     &       (ywert*ywert)/(2.d0*so5.dipdisy) )

	if(ifl.ipinarr.eq.1)then
c        Achtung Übergabeparaneter anpassen
c	  call pin_arr(src,ra.rf.yp,ra.rf.zp,f)
	  else
	  f=1.0d0
	endif

	if(ifl.ispline.ge.0)then
	sr.densy=f*factor*so5.dipcy*ywert*exp(cs.sqrtm1*(PhiSrc))
	sr.densz=f*factor*so5.dipcz*
     &			exp(cs.sqrtm1*(PhiSrc+0.5d0*cs.pi))
	endif

	if(ifl.ispline.lt.0)then
	sr.eyp=sr.eyp+phisrc
	sr.ezp=sr.ezp+phisrc
	sr.eya=f*so5.dipcy*ywert
	sr.eza=f*so5.dipcz
	endif

	return
	end
c----------------------- end src5---------------------

c*****************************************************************
	subroutine src6(cs,g,ra,so6,sr,ifl)
c*****************************************************************
c	returns complex*16 densy
c	result is real part of complex variable
c----------------------------------------------
	implicit real*8(a-h,o-z)

cc	include 'phase_struct.for'
#include 'phase_struct.F'

	record /constants/ cs
	record /geometryst/ g
	record /rayst/ ra 
	record /source_results/ sr 
	record /control_flags/ ifl
	record /source6/ so6

c-------- 27.2.2004
c	xwert=ra.rf.zp/1000.		! mm --> m
c	ywert=ra.rf.yp/1000.		! mm --> m
	xwert=ra.rf.zp	 ! units are mm
	ywert=ra.rf.yp	 ! units are mm
c-------- 27.2.2004
	pxwert=ra.rf.dzp		! rad
	pywert=ra.rf.dyp		! rad

	if(	(xwert.gt.so6.brxmin).and.
     &		(xwert.lt.so6.brxmax).and.
     &	        (ywert.gt.so6.brymin).and.
     &		(ywert.lt.so6.brymax).and.
     &	        (pxwert.gt.so6.brpxmin).and.
     &		(pxwert.lt.so6.brpxmax).and.
     &	        (pywert.gt.so6.brpymin).and.
     &		(pywert.lt.so6.brpymax)  ) then

c        Achtung Übergabeparaneter anpassen
c	call source_inter_4d(so6,xwert,ywert,pxwert,pywert,sr.densy)

	endif	! min, max

	return
	end
c----------------------- end src6 ---------------------

c***********************************************************
	subroutine src2_ini(so2)
c***********************************************************
	implicit real*8(a-h,o-z)

cc	include 'phase_struct.for'
#include 'phase_struct.F'
	record /source2/ so2

	open(unit=10,name=so2.fsource2a,type='old')
	read(10,*)extyr
	do n1=0,so2.iordzern
	  if(mod(n1,2).eq.0)then
	  do l1=0,n1,2
            read(10,*)nn,ll,so2.ceyre(n1,l1)
	  enddo
	  else
	  do l1=1,n1,2
            read(10,*)nn,ll,so2.ceyre(n1,l1)
          enddo
          endif
	enddo	
	close(10)

	open(unit=10,name=so2.fsource2b,type='old')
	read(10,*)extyi
	do n1=0,so2.iordzern
	  if(mod(n1,2).eq.0)then
	  do l1=0,n1,2
            read(10,*)nn,ll,so2.ceyim(n1,l1)
          enddo
	  else
	  do l1=1,n1,2
            read(10,*)nn,ll,so2.ceyim(n1,l1)
          enddo
          endif    
	enddo	
	close(10)

	so2.fscale=extyi

	return
	end

c***********************************************************
	subroutine src3_ini(so3)
c***********************************************************
	implicit real*8(a-h,o-z)

cc	include 'phase_struct.for'
#include 'phase_struct.F'

	record /source3/ so3

	write(*,*) 'src3_ini: read file: ',so3.fsource3a
	open(unit=10,name=so3.fsource3a,type='old')
	read(10,*)so3.isrcreal
	do i=1,so3.isrcreal
	read(10,*)so3.xsrcreal(i),so3.ysrcreal(i)
	enddo
	close(10)

	write(*,*) 'src3_ini: read file: ',so3.fsource3b
	open(unit=10,name=so3.fsource3b,type='old')
	read(10,*)so3.isrcimag
	do i=1,so3.isrcimag
	read(10,*)so3.xsrcimag(i),so3.ysrcimag(i)
	enddo
	close(10)

	return
	end

c***********************************************************
	subroutine src4_ini(so4)
c***********************************************************
c UF Jan 12 add low level tests if files exists
c UF Apr 12 add dynamic memory

c	implicit real*8(a-h,o-z)
	
#include 'phase_struct.F'

	record /source4/ so4
        logical ex 
	real*8 , allocatable :: xx(:), yy(:)    !, zzd(:,:) 
	integer nx, ny, alloc_err

c	maximale Dimension der Quelle 1024 x 1024
c UF	dimension xx(1048576),yy(1048576)

	write(*,*) 'src4_ini: filenames hardcoded test_5000.s4a,s4b,s4c,s4d'
	so4.fsource4a='test_5000.s4a'
	so4.fsource4b='test_5000.s4b'
	so4.fsource4c='test_5000.s4c'
	so4.fsource4d='test_5000.s4d'


	write(*,*) 'src4_ini: read file: ',so4.fsource4a
	INQUIRE(File=so4.fsource4a, Exist=ex)
	if (.NOT. ex) then
	   write(*,*) 'error: src4_ini: file not found: ', so4.fsource4a
	   write(*,*) 'exit'
	   stop
	endif

	write(*,*) 'src4_ini: open'
	open(unit=10, name=so4.fsource4a, type='old')
	write(*,*) 'src4_ini: read1'
	read(10,*) so4.ieyrex, so4.ieyrey
	write(*,*) 'src4_ini: after read1'
	nx=so4.ieyrex      ! remember the numbers and allocate the fields                               
	ny=so4.ieyrey
	write(*,*) 'src4_ini: allocate', nx, ny
	allocate(xx(nx*ny), yy(nx*ny), stat=alloc_err)
	if (alloc_err .gt. 0) then
	   write(*,*) 'error: memory exhausted- exit'
	   stop
	endif
	  
	do j=1,so4.ieyrey
	   do i=1,so4.ieyrex
	      k=(j-1)*so4.ieyrex+i
	      read(10,*)xx(k),yy(k),so4.zeyre(i,j)
	   enddo
	enddo
	close(10)
	write(*,*) '==> done'  ! so4.fsource4a
	   
	so4.xeyremin=xx(1)
	so4.xeyremax=xx(k)
	so4.dxeyre=(so4.xeyremax-so4.xeyremin)
     &		/dflotj(so4.ieyrex-1)
	so4.yeyremin=yy(1)
	so4.yeyremax=yy(k)
	so4.dyeyre=(so4.yeyremax-so4.yeyremin)
     &		/dflotj(so4.ieyrey-1)

	do i=1,so4.ieyrex
	  so4.gridx(i)=xx(i)
	enddo

	do j=1,so4.ieyrey
	  so4.gridy(j)=yy((j-1)*so4.ieyrex+1)
	enddo
	
c---------------------------------------------------------------

	write(*,*) 'src4_ini: read file: ',so4.fsource4b
	INQUIRE(File=so4.fsource4b, Exist=ex)
	if (.NOT. ex) then
	   write(*,*) 'error: src4_ini: file not found: ', so4.fsource4b
	   write(*,*) 'exit'
	   stop
	endif

	open(unit=10,name=so4.fsource4b,type='old')
	read(10,*)so4.ieyimx,so4.ieyimy
	if ((so4.ieyimx.ne.nx).or.(so4.ieyimy.ne.ny)) then
	   write(*,*) 'error: src4_ini: dimension error ', so4.fsource4b
	   write(*,*) 'exit'
	   stop
	endif

	do j=1,so4.ieyimy
	   do i=1,so4.ieyimx
	      k=(j-1)*so4.ieyimx+i
	      read(10,*)xx(k),yy(k),so4.zeyim(i,j)
c-------------10.8.2005
	      if((so4.iconj).eq.1)so4.zeyim(i,j)=-so4.zeyim(i,j)
c-------------10.8.2005
	   enddo
	enddo
	close(10)
	write(*,*) '==> done'
		
	so4.xeyimmin=xx(1)
	so4.xeyimmax=xx(k)
	so4.dxeyim=(so4.xeyimmax-so4.xeyimmin)
     &		/dflotj(so4.ieyimx-1)
	so4.yeyimmin=yy(1)
	so4.yeyimmax=yy(k)
	so4.dyeyim=(so4.yeyimmax-so4.yeyimmin)
     &		/dflotj(so4.ieyimy-1)


c******* deallocate, allow different grid sizes
	if (allocated(xx)) deallocate(xx)
	if (allocated(yy)) deallocate(yy)

c----------------------------------------------------------

	write(*,*) 'src4_ini: read file: ',so4.fsource4c
	INQUIRE(File=so4.fsource4c, Exist=ex)
	if (.NOT. ex) then
	   write(*,*) 'error: src4_ini: file not found: ', so4.fsource4c
	   write(*,*) 'exit'
	   stop
	endif

	open(unit=10,name=so4.fsource4c,type='old')
	read(10,*)so4.iezrex,so4.iezrey

	nx=so4.iezrex		! remember the numbers and allocate the fields                               
	ny=so4.iezrey

	allocate(xx(nx*ny), yy(nx*ny), stat=alloc_err)
	if (alloc_err.gt.0) then
	   write(*,*) 'error: memory exhausted- exit'
	   stop
	endif

	do j=1,so4.iezrey
	   do i=1,so4.iezrex
	      k=(j-1)*so4.iezrex+i
	      read(10,*)xx(k),yy(k),so4.zezre(i,j)
	   enddo
	enddo
	close(10)
	write(*,*) '==> done'
	
	so4.xezremin=xx(1)
	so4.xezremax=xx(k)
	so4.dxezre=(so4.xezremax-so4.xezremin)
     &		/dflotj(so4.iezrex-1)
	so4.yezremin=yy(1)
	so4.yezremax=yy(k)
	so4.dyezre=(so4.yezremax-so4.yezremin)
     &		/dflotj(so4.iezrey-1)

c-----------------------------------------------------------

	write(*,*) 'src4_ini: read file: ',so4.fsource4d
	INQUIRE(File=so4.fsource4d, Exist=ex)
	if (.NOT. ex) then
	   write(*,*) 'error: src4_ini: file not found: ', so4.fsource4d
	   write(*,*) 'exit'
	   stop
	endif

	open(unit=10,name=so4.fsource4d,type='old')
	read(10,*)so4.iezimx,so4.iezimy
	if ((so4.iezimx.ne.nx).or.(so4.iezimy.ne.ny)) then
	   write(*,*) 'error: src4_ini: dimension error ', so4.fsource4d
	   write(*,*) 'exit'
	   stop
	endif

	do j=1,so4.iezimy
	   do i=1,so4.iezimx
	      k=(j-1)*so4.iezimx+i
	      read(10,*)xx(k),yy(k),so4.zezim(i,j)
c-------------10.8.2005
	      if((so4.iconj).eq.1)so4.zezim(i,j)=-so4.zezim(i,j)
c-------------10.8.2005
	   enddo
	enddo
	close(10)
	write(*,*) '==> done'
	
	so4.xezimmin=xx(1)
	so4.xezimmax=xx(k)
	so4.dxezim=(so4.xezimmax-so4.xezimmin)
     &		/dflotj(so4.iezimx-1)
	so4.yezimmin=yy(1)
	so4.yezimmax=yy(k)
	so4.dyezim=(so4.yezimmax-so4.yezimmin)
     &		/dflotj(so4.iezimy-1)

c******* deallocate
	if (allocated(xx)) deallocate(xx)
	if (allocated(yy)) deallocate(yy)
	
	return
	end
c********** src4_ini ************

c*****************************************************************
	subroutine src6_ini(so6)
c*****************************************************************
	implicit real*8(a-h,o-z)

#include 'phase_struct.F'

	record /source6/ so6
	logical ex

	INQUIRE(File=so6.fsource6, Exist=ex)
	if (ex) then
	open(unit=10,name=so6.fsource6,
     &  type='old',form='unformatted',access='sequential')
	read(10)so6.brxmin,so6.brxmax,so6.brdx,so6.ibrx
	read(10)so6.brymin,so6.brymax,so6.brdy,so6.ibry
	read(10)so6.brpxmin,so6.brpxmax,so6.brdpx,so6.ibrpx
	read(10)so6.brpymin,so6.brpymax,so6.brdpy,so6.ibrpy

	do i=1,so6.ibrx
	do ii=1,so6.ibry
	do j=1,so6.ibrpx
	do jj=1,so6.ibrpy
	read(10)so6.br(i,ii,j,jj)
	enddo
	enddo
	enddo
	enddo
	close(10)
	else
	   write(*,*) 'error: src6_ini: file not found: ', so6.fsource6
	endif
	return
	end
#ifdef PHASELIB
c********************************************************
	subroutine source_zernike(so2,cs,xwert,ywert,carg)
c********************************************************

	implicit real*8(a-h,o-z)

cc	include 'phase_struct.for'
#include 'phase_struct.F'

	complex*16 carg

	record /source2/ so2
	record /constants/ cs

	dimension rn(0:60),rnl(0:60,-60:60)

c------------ change to polar coordinates -------------

        rho=dsqrt(xwert*xwert+ywert*ywert)
        rho=rho/so2.fscale

	if(dabs(xwert).lt.so2.small)then
          phi=cs.pi/2.
	 else
	  phi=datan(dabs(ywert)/abs(xwert))
        endif
	if((ywert.gt.0.).and.(xwert.lt.0.))
     &              phi=cs.pi-phi
	if((ywert.lt.0.).and.(xwert.lt.0.))
     &              phi=phi+cs.pi
	if((ywert.lt.0.).and.(xwert.gt.0.))
     &              phi=2.*cs.pi-phi

c------------------- get rn, rnl -----------------

      if(so2.iflagl.eq.0)call zernike_n(rho,rn,rnl)
      if(so2.iflagl.eq.1)call zernike_nl(rho,rn,rnl)

c------------------ calculation -------------------------

	sign=-1.d0

      carg=0.d0

      if(so2.iflagl.eq.1)then
      do n1=0,so2.iordzern
c-------------------
       if(mod(n1,2).eq.0)then
c-------------------
       do l1=0,n1,2
        valre=so2.ceyre(n1,l1)*rnl(n1,l1)
        valim=so2.ceyim(n1,l1)*rnl(n1,l1)
        carg=carg+(valre+cs.sqrtm1*valim)*exp(cs.sqrtm1*l1*phi)
       enddo
c------------------
       else
c------------------
       do l1=1,n1,2
        valre=so2.ceyre(n1,l1)*rnl(n1,l1)
        valim=so2.ceyim(n1,l1)*rnl(n1,l1)
        carg=carg+(valre+
     &		cs.sqrtm1*valim)*exp(cs.sqrtm1*dflotj(l1)*phi)
       enddo
c-----------------
       endif
c-----------------
      enddo	! n1
      endif     ! iflagl=1

c---------------------------------------------------------

      if(so2.iflagl.eq.0)then
       do n1=0,so2.iordzern,2
        valre=so2.ceyre(n1,0)*rn(n1)
        valim=so2.ceyim(n1,0)*rn(n1)
        carg=carg+(valre+sign*cs.sqrtm1*valim)
       enddo	! n1
      endif     ! iflagl=0

c---------------------------------------------------------

      return
      end
#endif


c********************************************************
	subroutine source_inter(so3,cs,xwert,ywert,carg)
c********************************************************

	implicit real*8(a-h,o-z)

cc	include 'phase_struct.for'
#include 'phase_struct.F'

	record /source3/ so3
	record /constants/ cs

	complex*16 carg

	rho=dsqrt(xwert*xwert+ywert*ywert)
	
	if(so3.xsrcreal(so3.iactreal).lt.rho)then

	do i=so3.iactreal,so3.isrcreal
	 if(so3.xsrcreal(i).ge.rho)then
         so3.iactreal=i
c    ********	    interpolate ********
	 valreal=so3.ysrcreal(i-1)+
     &            (so3.ysrcreal(i)-so3.ysrcreal(i-1))*
     &            (rho-so3.xsrcreal(i-1))/(so3.xsrcreal(i)
     &		  -so3.xsrcreal(i-1))
	 goto 100
	 endif
	enddo
        valreal=0.d0
100	continue

	endif

	if(so3.xsrcimag(so3.iactimag).lt.rho)then

	do i=so3.iactimag,so3.isrcimag
	 if(so3.xsrcimag(i).ge.rho)then
         so3.iactimag=i
c    ********	    interpolate ********
	 valimag=so3.ysrcimag(i-1)+
     &       (so3.ysrcimag(i)-so3.ysrcimag(i-1))*
     &       (rho-so3.xsrcimag(i-1))/(so3.xsrcimag(i)
     &	     -so3.xsrcimag(i-1))
	 goto 101
	 endif
	enddo
        valimag=0.d0
101	continue

	endif

c------------------------------------------------------------

	if(so3.xsrcreal(so3.iactreal).ge.rho)then

	do i=so3.iactreal,1,-1
	 if(so3.xsrcreal(i).lt.rho)then
         so3.iactreal=i
c    ********	    interpolate ********
	 valreal=so3.ysrcreal(i)+
     &            (so3.ysrcreal(i+1)-so3.ysrcreal(i))*
     &            (rho-so3.xsrcreal(i))/(so3.xsrcreal(i+1)
     &		  -so3.xsrcreal(i))
	 goto 102
	 endif
	enddo
        valreal=0.d0
102	continue

	endif

	if(so3.xsrcimag(so3.iactimag).ge.rho)then

	do i=so3.iactimag,1,-1
	 if(so3.xsrcimag(i).lt.rho)then
         so3.iactimag=i
c    ********	    interpolate ********
	 valimag=so3.ysrcimag(i)+
     &       (so3.ysrcimag(i+1)-so3.ysrcimag(i))*
     &       (rho-so3.xsrcimag(i))/(so3.xsrcimag(i+1)
     &	     -so3.xsrcimag(i))
	 goto 103
	 endif
	enddo
        valimag=0.d0
103	continue

	endif

c--------------------------------------------------------

	sign=-1.d0

        carg=(valreal+sign*cs.sqrtm1*valimag)

	return
	end


c********************************************************************
	subroutine source_inter_2d(so4,cs,xwert,ywert,sr)
c********************************************************************
c
c	returns complex values densy, densz
c	for ispline.lt.0 dreal(XXX)=amplitude
c			 dimag(XXX)=phase
c
c-----------------------------------------------------------
	implicit real*8(a-h,o-z)
cc	include 'phase_struct.for'
#include 'phase_struct.F'

	record /source4/ so4
	record /constants/ cs
	record /source_results/ sr 
	record /control_flags/ ifl

	complex*16 carg,cargy,cargz,amp1,amp2,amp3,amp4,fact1

c	sign=1.
c	fact1=sign*cs.sqrtm1
	fact1=cs.sqrtm1

c---------- es wird gleiches Raster fuer Real- und
c---------- Imaginaerteil sowie fuer Ey und Ez vorausgesetzt
c---------- Aenderungen 17.3.2006

c---------  Interpolation of Ey

	ix1=(xwert-so4.xeyremin)/so4.dxeyre + 1
	ix2=ix1+1
	iy1=(ywert-so4.yeyremin)/so4.dyeyre + 1
	iy2=iy1+1

	x1=so4.gridx(ix1)
	x2=so4.gridx(ix2)
	y1=so4.gridy(iy1)
	y2=so4.gridy(iy2)
c /* UF 18.2.14 */
        ddxy=so4.dxeyre*so4.dxeyre              
	fact3=((x2-xwert)*(y2-ywert))/ddxy
	fact4=((xwert-x1)*(y2-ywert))/ddxy
	fact5=((x2-xwert)*(ywert-y1))/ddxy
	fact6=((xwert-x1)*(ywert-y1))/ddxy

        sr.densyre=fact3*so4.zeyre(ix1,iy1)+
     &             fact4*so4.zeyre(ix2,iy1)+
     &	           fact5*so4.zeyre(ix1,iy2)+
     &             fact6*so4.zeyre(ix2,iy2)

        sr.densyim=fact3*so4.zeyim(ix1,iy1)+
     &             fact4*so4.zeyim(ix2,iy1)+
     &	           fact5*so4.zeyim(ix1,iy2)+
     &             fact6*so4.zeyim(ix2,iy2)

c---------  Interpolation of Ez, same grid as for Ey

        sr.denszre=fact3*so4.zezre(ix1,iy1)+
     &             fact4*so4.zezre(ix2,iy1)+
     &	           fact5*so4.zezre(ix1,iy2)+
     &             fact6*so4.zezre(ix2,iy2)

        sr.denszim=fact3*so4.zezim(ix1,iy1)+
     &             fact4*so4.zezim(ix2,iy1)+
     &	           fact5*so4.zezim(ix1,iy2)+
     &             fact6*so4.zezim(ix2,iy2)
  
     	return
	end

c**********************************************************
	subroutine source_inter_4d(so6,xwert,ywert,
     &		pxwert,pywert,sr)
c**********************************************************

	implicit real*8(a-h,o-z)

cc	include 'phase_struct.for'
#include 'phase_struct.F'

	record /source6/ so6
	record /geometryst/ g
	record /rayst/ ra 
	record /source_results/ sr 
	record /control_flags/ ifl

	ix1=(xwert-so6.brxmin)/so6.brdx + 1
	ix2=ix1+1
	iy1=(ywert-so6.brymin)/so6.brdy + 1
	iy2=iy1+1
	ipx1=(pxwert-so6.brpxmin)/so6.brdpx + 1
	ipx2=ipx1+1
	ipy1=(pywert-so6.brpymin)/so6.brdpy + 1
	ipy2=ipy1+1

	x1=so6.brxmin+dflotj(ix1-1)*so6.brdx
	x2=x1+so6.brdx
	y1=so6.brymin+dflotj(iy1-1)*so6.brdy
	y2=y1+so6.brdy
	px1=so6.brpxmin+dflotj(ipx1-1)*so6.brdpx
	px2=px1+so6.brdpx
	py1=so6.brpymin+dflotj(ipy1-1)*so6.brdpy
	py2=py1+so6.brdpy

	br1111=so6.br(ix1,iy1,ipx1,ipy1)
	br1112=so6.br(ix1,iy1,ipx1,ipy2)
	br1121=so6.br(ix1,iy1,ipx2,ipy1)
	br1122=so6.br(ix1,iy1,ipx2,ipy2)
	br1211=so6.br(ix1,iy2,ipx1,ipy1)
	br1212=so6.br(ix1,iy2,ipx1,ipy2)
	br1221=so6.br(ix1,iy2,ipx2,ipy1)
	br1222=so6.br(ix1,iy2,ipx2,ipy2)
	br2111=so6.br(ix2,iy1,ipx1,ipy1)
	br2112=so6.br(ix2,iy1,ipx1,ipy2)
	br2121=so6.br(ix2,iy1,ipx2,ipy1)
	br2122=so6.br(ix2,iy1,ipx2,ipy2)
	br2211=so6.br(ix2,iy2,ipx1,ipy1)
	br2212=so6.br(ix2,iy2,ipx1,ipy2)
	br2221=so6.br(ix2,iy2,ipx2,ipy1)
	br2222=so6.br(ix2,iy2,ipx2,ipy2)

	bright= (
     &       br1111*(x2-xwert)*(y2-ywert)*(px2-pxwert)*(py2-pywert) +	
     &       br1112*(x2-xwert)*(y2-ywert)*(px2-pxwert)*(pywert-py1) +	
     &       br1121*(x2-xwert)*(y2-ywert)*(pxwert-px1)*(py2-pywert) +	
     &       br1122*(x2-xwert)*(y2-ywert)*(pxwert-px1)*(pywert-py1) +	
     &       br1211*(x2-xwert)*(ywert-y1)*(px2-pxwert)*(py2-pywert) +	
     &       br1212*(x2-xwert)*(ywert-y1)*(px2-pxwert)*(pywert-py1) +	
     &       br1221*(x2-xwert)*(ywert-y1)*(pxwert-px1)*(py2-pywert) +	
     &       br1222*(x2-xwert)*(ywert-y1)*(pxwert-px1)*(pywert-py1) +	
     &       br2111*(xwert-x1)*(y2-ywert)*(px2-pxwert)*(py2-pywert) +	
     &       br2112*(xwert-x1)*(y2-ywert)*(px2-pxwert)*(pywert-py1) +	
     &       br2121*(xwert-x1)*(y2-ywert)*(pxwert-px1)*(py2-pywert) +	
     &       br2122*(xwert-x1)*(y2-ywert)*(pxwert-px1)*(pywert-py1) +	
     &       br2211*(xwert-x1)*(ywert-y1)*(px2-pxwert)*(py2-pywert) +	
     &       br2212*(xwert-x1)*(ywert-y1)*(px2-pxwert)*(pywert-py1) +	
     &       br2221*(xwert-x1)*(ywert-y1)*(pxwert-px1)*(py2-pywert) +	
     &       br2222*(xwert-x1)*(ywert-y1)*(pxwert-px1)*(pywert-py1) )

	bright=bright/((x2-x1)*(y2-y1)*(px2-px1)*(py2-py1))

	sr.densy=bright

	return
	end

c*************************************************************************
	subroutine pin_arr(src,y,z,f)
c*************************************************************************
c-----------------------------------------------------------------
c
c	mesh is defined as follows
c
c	periodicity in y (mm): pin_yl0
c	periodicity in z (mm): pin_zl0
c
c	width of non transparent areas in y (mm): pin_yl
c	width of non transparent areas in z (mm): pin_zl
c	
c	routine returns 1 if hit area is transparent, otherwise 0
c
c-----------------------------------------------------------------

	implicit real*8(a-h,o-z)

cc	include 'phase_struct.for'
#include 'phase_struct.F'

c**************************************************

c	record /source1/ so1
c	record /source2/ so2
c	record /source3/ so3
c	record /source4/ so4
c	record /source5/ so5
c	record /source6/ so6
	record /sources/ src

	f=1.d0
#ifdef XXX
	yy=dmod(dabs(y),src.pin_yl0)
	zz=dmod(dabs(z),src.pin_zl0)

	if( (yy.ge.(0.5d0*src.pin_yl0-0.5d0*src.pin_yl)) .and. 
     &      (yy.le.(0.5d0*src.pin_yl0+0.5d0*src.pin_yl)) ) f=0.d0	
	if( (zz.ge.(0.5d0*src.pin_zl0-0.5d0*src.pin_zl)) .and. 
     &      (zz.le.(0.5d0*src.pin_zl0+0.5d0*src.pin_zl)) ) f=0.d0	
#endif
	return
	end



c end
 
