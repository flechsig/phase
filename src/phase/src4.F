c$$$ File      : phase_source.F
c$$$ Date      : <19 Nov 07 09:34:34 flechsig> 
c$$$ Time-stamp: <02 Apr 15 16:18:08 flechsig> 
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
c UF called from psdi.F

c*****************************************************************
        subroutine src4(cs,g,ra,sr,ifl,lostwl,blp)
c*****************************************************************
c	returns complex*16 densy, densz
c--------------------------------------
c	implicit real*8(a-h,o-z)
	complex*16 factor
        real*8 phase,xlength,delphi,w2,w3,w4,w5,xden,f
        real*8 grdist,xnn,factorre,factorim,xwert,ywert
        real*8 srdensyre,srdensyim,srdenszre,srdenszim
	integer lostwl,blp,isig,nn               

#include 'phase_struct.F'

	record /constants/ cs
	record /geometryst/ g 
	record /rayst/ ra 
	record /source_results/ sr 
	record /control_flags/ ifl

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

c	factorre=dcos(delphi+phase)
c       factorim=dsin(delphi+phase)
	factorre=cos(delphi+phase)
        factorim=sin(delphi+phase)

        if(ifl.ispline.ge.0)factor=exp(cs.sqrtm1*(delphi+phase))

        xwert=ra.rf.zp	        ! units are mm
	ywert=ra.rf.yp	        ! units are mm

c     if ((ifl.pst_mode.gt.2).or.(ifl.pst_mode.lt.0))then   ! for tests

c     new code with dynamic source does also the range test 
c     fills sr.dens[y|z][re|im] 
        call source4c_inter_2d(sr,xwert,ywert,blp);
        

c code to deal with slope errors 
c contributed by Walan Grizolli modified and extended by UF
c procedure takes the beamline pointer, w and l and
c the electrical field picked up for the particular "ray" as input
c the field will be overwritten in place 
c routine does also the aperture check

        call apply_height_error(blp, lostwl, ra.oe.w, ra.oe.xl, sr.densyre, 
     &                          sr.densyim, sr.denszre, sr.denszim)
        

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
c end
 
