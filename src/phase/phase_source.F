From - Thu Jan 20 09:36:35 2000
Received: from psi12.psi.ch ([129.129.12.142]) by psi14.psi.ch with SMTP (Microsoft Exchange Internet Mail Service Version 5.5.2650.21)
	id CVLHLQ0C; Wed, 19 Jan 2000 11:56:49 +0100
Received: from 193.149.14.28 by psi12.psi.ch (InterScan E-Mail VirusWall NT); Wed, 19 Jan 2000 11:56:48 +0100 (W. Europe Standard Time)
Received: by exp.bessy.de (MX V5.1 AnBl) id 38; Wed, 19 Jan 2000 11:56:48 +0100
Date: Wed, 19 Jan 2000 11:56:48 +0100
From: bahrdt@exp.bessy.de
Reply-To: bahrdt@bessy.de
To: UWE.FLECHSIG@PSI.CH
Message-ID: <009E45FC.BAA8A96C.38@exp.bessy.de>
Subject: neue Source-Routine



	Hallo Uwe,

	Anbei die Routine PHASE_SOURCE_11.FOR
	Ich habe ein paar Aenderungen vorgenommen (gekennzeichnet
	mit Datum 19.1.2000) und hoffe, dass die komischen Effekte, die
	ich gestern gesehen habe, jetzt weg sind.

	Die Variable ra.oe.w inPHASE_SOURCE_11.FOR ist der Auftreffpunkt 
	auf dem Gitter. Falls mehrere optische Elemente vorhanden sind, 
	muss dass entsprechend beruecksichtigt werden. Aber ich denke,
	dass Du das bereits eingebaut hast.

	Beste Gruesse,

		Johannes

c--------------------------------------------------------------------------


**************************************************************
	subroutine src_ini(src)	
c**************************************************************

	implicit real*8(a-h,o-z)

	include 'phase_struct_11.for'

c**************************************************

	record /source1/ so1
	record /source2/ so2
	record /source3/ so3
	record /source4/ so4
	record /source5/ so5
	record /source6/ so6
	record /sources/ src

	if(src.isrctype.eq.2)call src2_ini(src.so2)

	if(src.isrctype.eq.3)call src3_ini(src.so3)

	if(src.isrctype.eq.4)call src4_ini(src.so4)

	if(src.isrctype.eq.6)call src6_ini(src.so6)

	return
	end

c**************************************************************
        subroutine psdi(g,src,apr,cs,ifl,ra,sr)
c**************************************************************
c
c       Berechnung der Phasenraumdichte in der Gegenstandsebene
c
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
c       Optical Path Length:
c       --------------------
c
c	iexpand = 1 : power series expansion of 
c		      optical path length
c	iexpand = 0 : explicit calculation of
c		      optical path length
c
c**************************************************************

	implicit real*8(a-h,o-z)

	include 'phase_struct_11.for'

c**************************************************

	record /constants/ cs
	record /geometryst/ g
	record /rayst/ ra 
	record /source_results/ sr 
	record /control_flags/ ifl
	record /source2/ so2
	record /source3/ so3
	record /source4/ so4
	record /source5/ so5
	record /source6/ so6
	record /sources/ src
	record /apertures/ apr

c---------------- set values to zero by default

	sr.dens=0.d0
	sr.densy=0.d0
	sr.densz=0.d0
	sr.eya=0.d0
	sr.eza=0.d0
	sr.eyp=0.d0
	sr.ezp=0.d0

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
	if( (dsqrt(ra.ap.yp_ap*ra.ap.yp_ap+
     &		ra.ap.zp_ap*ra.ap.zp_ap).lt.apr.rpin_ap).and.
     &		(ra.ap.yp_ap.gt.apr.ymin_ap).and.
     &		(ra.ap.yp_ap.lt.apr.ymax_ap).and.
     &		(ra.ap.zp_ap.gt.apr.zmin_ap).and.
     &		(ra.ap.zp_ap.lt.apr.zmax_ap) ) then

c---------------- Finite size of OE--------------------

	iii=1
	if(ifl.ilimits.eq.1)then
	    if( (ra.oe.w.lt.apr.w_min).or.   
     &        (ra.oe.w.gt.apr.w_max).or.  
     &        (ra.oe.xl.lt.apr.xl_min).or.  
     &        (ra.oe.xl.gt.apr.xl_max) ) iii=0
	endif
	if(iii.eq.1)then

	if(src.isrctype.eq.1)call src1(cs,g,ra,src.so1,sr,ifl)
	if(src.isrctype.eq.2)call src2(cs,g,ra,src.so2,sr,ifl)
	if(src.isrctype.eq.3)call src3(cs,g,ra,src.so3,sr,ifl)
	if(src.isrctype.eq.4)call src4(cs,g,ra,src.so4,sr,ifl)
	if(src.isrctype.eq.5)call src5(cs,g,ra,src.so5,sr,ifl)
	if(src.isrctype.eq.6)call src6(cs,g,ra,src.so6,sr,ifl)

	endif	! size of OE
	endif	! aperture plane
	endif	! source plane

        return
        end

c*****************************************************************
	subroutine src1(cs,g,ra,so1,sr,ifl)
c*****************************************************************
c	returns complex*16 densy
c	result is real part of complex variable
c----------------------------------------------
	implicit real*8(a-h,o-z)
	include 'phase_struct_11.for'

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

c*****************************************************************
	subroutine src2(cs,g,ra,so2,sr,ifl)
c*****************************************************************
c	returns complex*16 densy
c-------------------------------
	implicit real*8(a-h,o-z)
	include 'phase_struct_11.for'

	record /constants/ cs
	record /geometryst/ g
	record /rayst/ ra 
	record /source_results/ sr 
	record /control_flags/ ifl
	record /source2/ so2

	if((dsqrt(ra.rf.zp*ra.rf.zp+ra.rf.yp*ra.rf.yp)/1000.).
     &			ge.(0.9*so2.fscale))then
	    sr.densy=0.
          else
	    call source_zernike(so2,cs,ra.rf.zp/1000.,
     &			ra.rf.yp/1000.,sr.densy)
	endif

	if(ifl.ipinarr.eq.1)then
	  call pin_arr(src,ra.rf.yp,ra.rf.zp,f)
	  sr.densy=sr.densy*f
	endif

	return
	end

c*****************************************************************
	subroutine src3(cs,g,ra,so3,sr,ifl)
c*****************************************************************
c	returns complex*16 densy
c-------------------------------
	implicit real*8(a-h,o-z)
	include 'phase_struct_11.for'

	record /constants/ cs
	record /geometryst/ g
	record /rayst/ ra 
	record /source_results/ sr 
	record /control_flags/ ifl
	record /source3/ so3

	call source_inter(so3,cs,ra.rf.zp/1000.,ra.rf.yp/1000.,
     &			sr.densy)

	if(ifl.ipinarr.eq.1)then
	  call pin_arr(src,ra.rf.yp,ra.rf.zp,f)
	  sr.densy=sr.densy*f
	endif

	return
	end

c*****************************************************************
	subroutine src4(cs,g,ra,so4,sr,ifl)
c*****************************************************************
c	returns complex*16 densy, densz
c--------------------------------------
	implicit real*8(a-h,o-z)
	complex*16 factor

	include 'phase_struct_11.for'

	record /constants/ cs
	record /geometryst/ g
	record /rayst/ ra 
	record /source_results/ sr 
	record /control_flags/ ifl
	record /source4/ so4

c--------- get path length
	if(ifl.iexpand.eq.0)then

	xxi=g.rp*g.cosb+dflotj(g.idefl)*ra.ri.yi*g.sinb
	yyi=g.rp*g.sinb-dflotj(g.idefl)*ra.ri.yi*g.cosb
	zzi=ra.ri.zi

	xxp=g.r*g.cosa-dflotj(g.idefl)*ra.rf.yp*g.sina
	yyp=g.r*g.sina+dflotj(g.idefl)*ra.rf.yp*g.cosa
	zzp=ra.rf.zp

	call intersection(uu,ww,xll)

	xlength=dsqrt(
     &  (xxi-uu)*(xxi-uu)+(yyi-ww)*(yyi-ww)+(zzi-xll)*(zzi-xll))
	ra.xlength1=xlength
	xlength=xlength+
     &  dsqrt((xxp-uu)*(xxp-uu)+(yyp-ww)*(yyp-ww)+(zzp-xll)*(zzp-xll))
	ra.xlength2=xlength-ra.xlength1

	else

	xlength=ra.xlength1+ra.xlength2

	endif	! iexpand

	if(ifl.iplmode.eq.0)then
	phase = ((2.*cs.pi)*(xlength-g.r-g.rp))/ra.xlam_test 
	endif

	if(ifl.iplmode.eq.1)then
	phase = (2.*cs.pi*xlength)/ra.xlam_test 
	endif

	if(ifl.ispline.ge.0)factor=exp(cs.sqrtm1*phase)
	if(ifl.ispline.lt.0)then
	sr.eyp=phase
	sr.ezp=phase
	endif

c--------------- end path length

c--------------- grating  
	if((ifl.igrating.eq.1).and.(g.xdens(0).gt.1.d-10))then

c********* aeussere Ordung      : xlam < 0
c********* innere Ordung        : xlam > 0
c********* Ablenkung nach oben  : idefl > 0
c********* Ablenkung nach unten : idefl < 0

	if (g.xlam.ge.0)isig=1
	if (g.xlam.lt.0)isig=-1

c------- old (before 19.1.2000) -----------------------------
c	grdist=1.d0/g.xdens(0)
c	xx=dmod(ra.oe.w,grdist)
c	xx=xx+grdist
c	xx=dmod(xx,grdist)
c
c	if(ifl.ispline.ge.0)then
c------- changed sign to minus at 19.1.2000
c	  delphi=-dflotj(isig)*(xx/grdist)*2.d0*cs.pi
c	  factor=factor*exp(cs.sqrtm1*delphi)
c	endif
c
c	if(ifl.ispline.lt.0)then
c-------  neu 20.6.1997
c	  delphi=-dflotj(isig)*(ra.oe.w/grdist)*2.d0*cs.pi
c-------  end neu 20.6.1997
c	  sr.eyp=sr.eyp+delphi
c	  sr.ezp=sr.ezp+delphi
c	endif
c--------------------------------------------------------

c------- new 19.1.2000 ----------------------------------
c	has been checked for non VLS grating 
c		(ifl.ispline = 0 and = -1)
c	has to be checked for VLS grating
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

	endif			! grating

	xwert=ra.rf.zp/1000.	! mm --> m
	ywert=ra.rf.yp/1000.	! mm --> m

	if(  ((xwert.gt.so4.xeyremin).and.(xwert.gt.so4.xeyimmin)).and.
     &	     ((xwert.lt.so4.xeyremax).and.(xwert.lt.so4.xeyimmax)).and.
     &	     ((ywert.gt.so4.yeyremin).and.(ywert.gt.so4.yeyimmin)).and.
     &	     ((ywert.lt.so4.yeyremax).and.(ywert.lt.so4.yeyimmax)).and.
     &       ((xwert.gt.so4.xezremin).and.(xwert.gt.so4.xezimmin)).and.
     &	     ((xwert.lt.so4.xezremax).and.(xwert.lt.so4.xezimmax)).and.
     &	     ((ywert.gt.so4.yezremin).and.(ywert.gt.so4.yezimmin)).and.
     &	     ((ywert.lt.so4.yezremax).and.(ywert.lt.so4.yezimmax)))then

	call source_inter_2d(so4,cs,xwert,ywert,sr.densy,sr.densz)

	endif	! min, max

	if(ifl.ipinarr.eq.1)then
	  call pin_arr(src,ra.rf.yp,ra.rf.zp,f)
	  else
	  f=1.0d0
	endif

	if(ifl.ispline.ge.0)then
		sr.densy=sr.densy*factor*f
		sr.densz=sr.densz*factor*f
	endif
	if(ifl.ispline.lt.0)then
		sr.eya=dreal(sr.densy)*f
		sr.eyp=sr.eyp+dimag(sr.densy)
		sr.eza=dreal(sr.densz)*f
		sr.ezp=sr.ezp+dimag(sr.densz)
	endif

	return
	end

c*****************************************************************
	subroutine src5(cs,g,ra,so5,sr,ifl)
c*****************************************************************
c	returns complex*16 densy, densz
c--------------------------------------
	implicit real*8(a-h,o-z)
	complex*16 factor

	include 'phase_struct_11.for'

	record /constants/ cs
	record /geometryst/ g
	record /rayst/ ra 
	record /source_results/ sr 
	record /control_flags/ ifl
	record /source5/ so5

c------------- get path length 

	if(ifl.iexpand.eq.0)then

	xxi=g.rp*g.cosb+dflotj(g.idefl)*ra.ri.yi*g.sinb
	yyi=g.rp*g.sinb-dflotj(g.idefl)*ra.ri.yi*g.cosb
	zzi=ra.ri.zi

	xxp=g.r*g.cosa-dflotj(g.idefl)*ra.rf.yp*g.sina
	yyp=g.r*g.sina+dflotj(g.idefl)*ra.rf.yp*g.cosa
	zzp=ra.rf.zp

	call intersection(uu,ww,xll)

	xlength=dsqrt(
     &  (xxi-uu)*(xxi-uu)+(yyi-ww)*(yyi-ww)+
     &  (zzi-xll)*(zzi-xll))
	ra.xlength1=xlength
	xlength=xlength+
     &  dsqrt((xxp-uu)*(xxp-uu)+(yyp-ww)*(yyp-ww)+
     &  (zzp-xll)*(zzp-xll))
	ra.xlength2=xlength-ra.xlength1

	else

	xlength=ra.xlength1+ra.xlength2

	endif	! iexpand

	if(ifl.iplmode.eq.0)then
	phase = ((2.*cs.pi)*(xlength-g.r-g.rp))/ra.xlam_test 
	endif

	if(ifl.iplmode.eq.1)then
	phase = (2.*cs.pi*xlength)/ra.xlam_test 
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

	PhiSrc=((2.d0*cs.pi)/ra.xlam_test)*
     &     ( (xwert*xwert)/(2.d0*so5.dipdisz) +
     &       (ywert*ywert)/(2.d0*so5.dipdisy) )

	if(ifl.ipinarr.eq.1)then
	  call pin_arr(src,ra.rf.yp,ra.rf.zp,f)
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

c*****************************************************************
	subroutine src6(cs,g,ra,so6,sr,ifl)
c*****************************************************************
c	returns complex*16 densy
c	result is real part of complex variable
c----------------------------------------------
	implicit real*8(a-h,o-z)

	include 'phase_struct_11.for'

	record /constants/ cs
	record /geometryst/ g
	record /rayst/ ra 
	record /source_results/ sr 
	record /control_flags/ ifl
	record /source6/ so6

	xwert=ra.rf.zp/1000.		! mm --> m
	ywert=ra.rf.yp/1000.		! mm --> m
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
	call source_inter_4d(so6,xwert,ywert,pxwert,pywert,sr.densy)

	endif	! min, max

	return
	end

c***********************************************************
	subroutine src2_ini(so2)
c***********************************************************
	implicit real*8(a-h,o-z)

	include 'phase_struct_11.for'

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

	include 'phase_struct_11.for'

	record /source3/ so3

	open(unit=10,name=fsource3a,type='old')
	read(10,*)so3.isrcreal
	do i=1,so3.isrcreal
	read(10,*)so3.xsrcreal(i),so3.ysrcreal(i)
	enddo
	close(10)

	open(unit=10,name=fsource3b,type='old')
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
	implicit real*8(a-h,o-z)

	include 'phase_struct_11.for'

	record /source4/ so4

	dimension xx(400000),yy(400000)

	open(unit=10,name=so4.fsource4a,type='old')
	read(10,*)so4.ieyrex,so4.ieyrey
	do j=1,so4.ieyrey
	do i=1,so4.ieyrex
	k=(j-1)*so4.ieyrex+i
	read(10,*)xx(k),yy(k),so4.zeyre(i,j)
	enddo
	enddo
	close(10)

	so4.xeyremin=xx(1)
	so4.xeyremax=xx(k)
	so4.dxeyre=(so4.xeyremax-so4.xeyremin)
     &		/dflotj(so4.ieyrex-1)
	so4.yeyremin=yy(1)
	so4.yeyremax=yy(k)
	so4.dyeyre=(so4.yeyremax-so4.yeyremin)
     &		/dflotj(so4.ieyrey-1)

	open(unit=10,name=so4.fsource4b,type='old')
	read(10,*)so4.ieyimx,so4.ieyimy
	do j=1,so4.ieyimy
	do i=1,so4.ieyimx
	k=(j-1)*so4.ieyimx+i
	read(10,*)xx(k),yy(k),so4.zeyim(i,j)
	enddo
	enddo
	close(10)

	so4.xeyimmin=xx(1)
	so4.xeyimmax=xx(k)
	so4.dxeyim=(so4.xeyimmax-so4.xeyimmin)
     &		/dflotj(so4.ieyimx-1)
	so4.yeyimmin=yy(1)
	so4.yeyimmax=yy(k)
	so4.dyeyim=(so4.yeyimmax-so4.yeyimmin)
     &		/dflotj(so4.ieyimy-1)

c----------------------------------------------------------

	open(unit=10,name=so4.fsource4c,type='old')
	read(10,*)so4.iezrex,so4.iezrey
	do j=1,so4.iezrey
	do i=1,so4.iezrex
	k=(j-1)*so4.iezrex+i
	read(10,*)xx(k),yy(k),so4.zezre(i,j)
	enddo
	enddo
	close(10)

	so4.xezremin=xx(1)
	so4.xezremax=xx(k)
	so4.dxezre=(so4.xezremax-so4.xezremin)
     &		/dflotj(so4.iezrex-1)
	so4.yezremin=yy(1)
	so4.yezremax=yy(k)
	so4.dyezre=(so4.yezremax-so4.yezremin)
     &		/dflotj(so4.iezrey-1)

	open(unit=10,name=so4.fsource4d,type='old')
	read(10,*)so4.iezimx,so4.iezimy
	do j=1,so4.iezimy
	do i=1,so4.iezimx
	k=(j-1)*so4.iezimx+i
	read(10,*)xx(k),yy(k),so4.zezim(i,j)
	enddo
	enddo
	close(10)

	so4.xezimmin=xx(1)
	so4.xezimmax=xx(k)
	so4.dxezim=(so4.xezimmax-so4.xezimmin)
     &		/dflotj(so4.iezimx-1)
	so4.yezimmin=yy(1)
	so4.yezimmax=yy(k)
	so4.dyezim=(so4.yezimmax-so4.yezimmin)
     &		/dflotj(so4.iezimy-1)

	return
	end

c*****************************************************************
	subroutine src6_ini(so6)
c*****************************************************************
	implicit real*8(a-h,o-z)

	include 'phase_struct_11.for'

	record /source6/ so6

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

	return
	end

c********************************************************
	subroutine source_zernike(so2,cs,xwert,ywert,carg)
c********************************************************

	implicit real*8(a-h,o-z)

	include 'phase_struct_11.for'

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

	sign=-1.

      carg=0.

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


c********************************************************
	subroutine source_inter(so3,cs,xwert,ywert,carg)
c********************************************************

	implicit real*8(a-h,o-z)

	include 'phase_struct_11.for'

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
        valreal=0.
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
        valimag=0.
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
        valreal=0.
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
        valimag=0.
103	continue

	endif

c--------------------------------------------------------

	sign=-1.

        carg=(valreal+sign*cs.sqrtm1*valimag)

	return
	end


c********************************************************************
	subroutine source_inter_2d(so4,cs,xwert,ywert,cargy,cargz)
c********************************************************************
c
c	returns complex values densy, densz
c	for ispline.lt.0 dreal(XXX)=amplitude
c			 dimag(XXX)=phase
c
c-----------------------------------------------------------
	implicit real*8(a-h,o-z)
	include 'phase_struct_11.for'

	record /source4/ so4
	record /constants/ cs
	record /source_results/ sr 
	record /control_flags/ ifl

	complex*16 carg,cargy,cargz,amp1,amp2,amp3,amp4

	sign=1.

c---------- es wird gleiches Raster fuer Real- und
c---------- Imaginaerteil sowie fuer Ey und Ez vorausgesetzt

c---------  Interpolation of Ey
 
	ix1=(xwert-so4.xeyremin)/so4.dxeyre + 1
	ix2=ix1+1
	iy1=(ywert-so4.yeyremin)/so4.dyeyre + 1
	iy2=iy1+1

	x1=so4.xeyremin+dflotj(ix1-1)*so4.dxeyre
	x2=x1+so4.dxeyre
	y1=so4.yeyremin+dflotj(iy1-1)*so4.dyeyre
	y2=y1+so4.dyeyre

	amp1=so4.zeyre(ix1,iy1) + sign*cs.sqrtm1*so4.zeyim(ix1,iy1)
	amp2=so4.zeyre(ix2,iy1) + sign*cs.sqrtm1*so4.zeyim(ix2,iy1)
	amp3=so4.zeyre(ix1,iy2) + sign*cs.sqrtm1*so4.zeyim(ix1,iy2)
	amp4=so4.zeyre(ix2,iy2) + sign*cs.sqrtm1*so4.zeyim(ix2,iy2)

	cargy=(1./((x2-x1)*(y2-y1)))*     (
     &       amp1*(x2-xwert)*(y2-ywert) +	
     &       amp2*(xwert-x1)*(y2-ywert) +	
     &       amp3*(x2-xwert)*(ywert-y1) +	
     &       amp4*(xwert-x1)*(ywert-y1)  )	

c---------  Interpolation of Ez

	ix1=(xwert-so4.xezremin)/so4.dxezre + 1
	ix2=ix1+1
	iy1=(ywert-so4.yezremin)/so4.dyezre + 1
	iy2=iy1+1

	x1=so4.xezremin+dflotj(ix1-1)*so4.dxezre
	x2=x1+so4.dxezre
	y1=so4.yezremin+dflotj(iy1-1)*so4.dyezre
	y2=y1+so4.dyezre

	amp1=so4.zezre(ix1,iy1) + sign*cs.sqrtm1*so4.zezim(ix1,iy1)
	amp2=so4.zezre(ix2,iy1) + sign*cs.sqrtm1*so4.zezim(ix2,iy1)
	amp3=so4.zezre(ix1,iy2) + sign*cs.sqrtm1*so4.zezim(ix1,iy2)
	amp4=so4.zezre(ix2,iy2) + sign*cs.sqrtm1*so4.zezim(ix2,iy2)

	cargz=(1./((x2-x1)*(y2-y1)))*     (
     &       amp1*(x2-xwert)*(y2-ywert) +	
     &       amp2*(xwert-x1)*(y2-ywert) +	
     &       amp3*(x2-xwert)*(ywert-y1) +	
     &       amp4*(xwert-x1)*(ywert-y1)  )	

c----------------------------------------------------------

	return
	end

c**********************************************************
	subroutine source_inter_4d(so6,xwert,ywert,
     &		pxwert,pywert,sr)
c**********************************************************

	implicit real*8(a-h,o-z)

	include 'phase_struct_11.for'

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

	include 'phase_struct_11.for'

c**************************************************

	record /source1/ so1
	record /source2/ so2
	record /source3/ so3
	record /source4/ so4
	record /source5/ so5
	record /source6/ so6
	record /sources/ src

	f=1.d0

	yy=dmod(dabs(y),src.pin_yl0)
	zz=dmod(dabs(z),src.pin_zl0)

	if( (yy.ge.(0.5d0*src.pin_yl0-0.5d0*src.pin_yl)) .and. 
     &      (yy.le.(0.5d0*src.pin_yl0+0.5d0*src.pin_yl)) ) f=0.d0	
	if( (zz.ge.(0.5d0*src.pin_zl0-0.5d0*src.pin_zl)) .and. 
     &      (zz.le.(0.5d0*src.pin_zl0+0.5d0*src.pin_zl)) ) f=0.d0	

	return
	end
