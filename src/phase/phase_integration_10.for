c*********************************************************
      subroutine adaptive_int(m4,g,a,src,apr,cs,ra,ifl,xi,xir,st)
c*********************************************************

c************************************************
c
c	Programm zur adaptiven Integration
c       in zwei Dimensionen
c
c	Festlegung der Integrationsgrenzen:
c		Absolutbetrag auswerten
c	Festlegung der Rasterdichte:
c		auch Phasenvorschub zwischen 
c		Datenpunkten auswerten	
c
c
c************************************************

	implicit real*8(a-h,o-z)
	complex*16 fwert,fwerty,fwertz,
     &      s1,s1eyre,s1eyim,s1ezre,s1ezim,
     &      s2,s2eyre,s2eyim,s2ezre,s2ezim,
     &      yzint,yzintey,yzintez
	complex*16 fzey(4096),fzoldey(4096)
	complex*16 fzez(4096),fzoldez(4096)

	include 'phase_struct_10.for'
        record /constants/ cs
        record /geometryst/ g
        record /rayst/ ra
        record /source_results/ sr
        record /integration_results/ xir
        record /control_flags/ ifl
        record /sources/ src
	record /integration/ xi
	record /apertures/ apr
	record /statistics/ st
	record /map4/ m4

	dimension a(0:5,0:5)

  	dimension z(4096),dz(4096)
	dimension zold(4096),dzold(4096)
	dimension fya(4096),fyp(4096)
	dimension fza(4096),fzp(4096)

c*******************************************************
c	Erstellen eines reduzierten Konstantensatzes
c*******************************************************

	yi=ra.ri.yi
	zi=ra.ri.zi

        call subm1(m4.fdetc,yi,zi,m4.fdetrc)
        call subm1(m4.fdetphc,yi,zi,m4.fdetphrc)
        call subm1(m4.fdet1phc,yi,zi,m4.fdet1phrc)
        call subm1(m4.ypc1,yi,zi,m4.yprc1)
        call subm1(m4.zpc1,yi,zi,m4.zprc1)
        call subm1(m4.dypc,yi,zi,m4.dyprc)
        call subm1(m4.dzpc,yi,zi,m4.dzprc)
        call subm1(m4.ypc_ap,yi,zi,m4.ypc_ap_r)
        call subm1(m4.zpc_ap,yi,zi,m4.zpc_ap_r)
        call subm1(m4.xlen1c,yi,zi,m4.xlen1c_r)
        call subm1(m4.xlen2c,yi,zi,m4.xlen2c_r)

        call subm1(m4.wc,yi,zi,m4.wrc)
        call subm1(m4.xlc,yi,zi,m4.xlrc)

c------------------------------------------------------

	st.inumb(st.nn1,st.nn2)=0.
	fmax=0.

	ianzz=xi.ianzz0
	iterz=xi.iterz0

c*** Number of Iterations:                      iterz
c*** Integrationsgrenzen:                       zmin, zmax
c*** Anzahl der Stuetzstellen im ersten Raster: ianzz
c*** Maximale Anzahl von Stuetzstellen:         imaxz
c*** Verhaeltnis von fmin zu fmax:              fracz
c*** minimaler Quotient benachbarter Y-Werte:   frac1z

c---------------- START ---------------------------------------
c---------------- first grid is equdistant, the following are not

	if(ianzz.eq.1)then
	dzz=0.
	else
	dzz=(xi.zmax-xi.zmin)/dflotj(ianzz-1)
	endif

c----------------------------------------------------------
	if(src.isrctype.lt.4)then
c----------------------------------------------------------
c
c	noch nicht getestet, vergl. mit isrctype.eq.4
c
c----------------------------------------------------------

	do i=1,ianzz
 	   z(i)=xi.zmin+dflotj(i-1)*dzz
	   call yyint(m4,g,a,src,xi,xir,apr,cs,ra,ifl,st,
     &		z(i),fwerty,fwertz,yintya,yintyp,yintza,yintzp)

	   fzey(i)=cdabs(fwerty)
	   dz(i)=dzz
	   fmax=dmax1(fmax,cdabs(fwerty))
	enddo

	fmin=xi.fracz*fmax

c******* next iteration *********************************

1000    continue
	do i=1,ianzz
	zold(i)=z(i)
	fzoldey(i)=fzey(i)
	dzold(i)=dz(i)
	iold=ianzz
	enddo

	ianzz=1

	do i=1,iold-1          ! Summation ueber altes Raster
c                              ------------------------------
	z(ianzz)=zold(i)
	fzey(ianzz)=fzoldey(i)
	dz(ianzz)=dzold(i)
	fmax=dmax1(fmax,cdabs(fzey(ianzz)))
	fmin=xi.fracz*fmax

	if(ianzz.lt.(xi.imaxz-(iold-i)))then	! insert grid point

	s1=xi.frac1z*fzoldey(i)
	s2=xi.frac1z*fzoldey(i+1)

	if(  (  ((cdabs(fzoldey(i+1)).lt.cdabs(s1)).or.
     &          (cdabs(fzoldey(i)).lt.cdabs(s2))   )  )
     &  .and.  
     &     (
     &     ((cdabs(fzoldey(i)).gt.fmin).or.
     &      (cdabs(fzoldey(i+1)).gt.fmin))   
     &  .or.
     &     ((cdabs(fzoldey(i)).gt.fmin).and.
     &      (cdabs(fzoldey(i+1)).gt.fmin))   
     &      )
     &              )then
	
        dz(ianzz)=0.5*dzold(i)
        dz(ianzz+1)=0.5*dzold(i)
	z(ianzz+1)=z(ianzz)+dz(ianzz)

	call yyint(m4,g,a,src,xi,xir,apr,cs,ra,ifl,st,
     &		z(ianzz+1),fwerty,fwertz,
     &		yintya,yintyp,yintza,yintzp)

	fzey(ianzz+1)=cdabs(fwerty)
	fmax=dmax1(fmax,cdabs(fzey(ianzz+1)))
	fmin=xi.fracz*fmax
	ianzz=ianzz+2

	else

	ianzz=ianzz+1

	endif

	else 	           ! ianzz.lt.xi.imaxz
	  ianzz=ianzz+1
	endif

	enddo        ! naechster Wert vom alten Raster
c                    ---------------------------------

	z(ianzz)=zold(iold)
	fzey(ianzz)=fzoldey(iold)

c------------------------------------------------------

	if((ianzz.gt.iold).and.
     &     ((iterz.gt.0).and.(ianzz.lt.xi.imaxz)))then

	iterz=iterz-1

	iold=ianzz
        goto 1000		! next iteration
c                               ----------------
	endif

	if(iterz.eq.0)st.inumzit=st.inumzit+1
	if(ianzz.ge.xi.imaxz)st.inumzan=st.inumzan+1

	call simpson(cs,ifl,xi,xir,ianzz,dz,fzey,fya,fyp,
     &		xir.yzintey,xir.yzintya,xir.yzintyp)

	endif

c-----------------------------------------------------------
	if((src.isrctype.eq.4).or.(src.isrctype.eq.5).or.
     &                        (src.isrctype.eq.6))then
c-----------------------------------------------------------
	do i=1,ianzz
	   ra.n3=i
 	   z(i)=xi.zmin+dflotj(i-1)*dzz
	   call yyint(m4,g,a,src,xi,xir,apr,cs,ra,ifl,st,
     &		z(i),fwerty,fwertz,
     &          yintya,yintyp,yintza,yintzp)
	   if(ifl.ispline.ge.0)then
	   	fzey(i)=fwerty
	   	fzez(i)=fwertz
	   endif
	   if(ifl.ispline.lt.0)then
	 	fya(i)=yintya
	 	fza(i)=yintza
	 	fyp(i)=yintyp
	 	fzp(i)=yintzp
	   endif
	   fmax=dmax1(fmax,cdabs(fwerty),cdabs(fwertz))
	   dz(i)=dzz
	enddo

	fmin=xi.fracz*fmax

c******* next iteration *********************************

1001    continue
	do i=1,ianzz
	zold(i)=z(i)
	fzoldey(i)=fzey(i)
	fzoldez(i)=fzez(i)
	dzold(i)=dz(i)
	iold=ianzz
	enddo

	ianzz=1

	do i=1,iold-1          ! Summation ueber altes Raster
c                              ------------------------------
	z(ianzz)=zold(i)
	fzey(ianzz)=fzoldey(i)
	fzez(ianzz)=fzoldez(i)
	fmax=dmax1(fmax,cdabs(fzey(ianzz)),
     &               cdabs(fzez(ianzz)))
	dz(ianzz)=dzold(i)
	fmin=xi.fracz*fmax

	if(ianzz.lt.(xi.imaxz-(iold-i)))then	! insert grid point

	s1eyre=xi.frac1y*fzoldey(i)
	s1ezre=xi.frac1z*fzoldez(i)
	s2eyre=xi.frac1y*fzoldey(i+1)
	s2ezre=xi.frac1z*fzoldez(i+1)

	s1eyim=xi.frac1y*fzoldey(i)
	s1ezim=xi.frac1z*fzoldez(i)
	s2eyim=xi.frac1y*fzoldey(i+1)
	s2ezim=xi.frac1z*fzoldez(i+1)

	if( (  
     &      ( (dabs(dreal(fzoldey(i+1))).lt.dabs(dreal(s1eyre)) ).and.
     &        (dabs(dreal(fzoldey(i+1))).gt.dabs(fmin))  ) .or.
     &
     &      ( (dabs(dreal(fzoldey(i))).lt.dabs(dreal(s2eyre)) ).and.
     &        (dabs(dreal(fzoldey(i))).gt.dabs(fmin)) ) .or.
     &
     &      ( (dabs(dreal(fzoldez(i+1))).lt.dabs(dreal(s1ezre)) ).and.
     &        (dabs(dreal(fzoldez(i+1))).gt.dabs(fmin)) ) .or.
     &
     &      ( (dabs(dreal(fzoldez(i))).lt.dabs(dreal(s2ezre)) ).and.
     &        (dabs(dreal(fzoldez(i))).gt.dabs(fmin)) ) .or.
     &
     &      ( (dabs(dimag(fzoldey(i+1))).lt.dabs(dimag(s1eyre)) ).and.
     &        (dabs(dimag(fzoldey(i+1))).gt.dabs(fmin)) ) .or.
     &
     &      ( (dabs(dimag(fzoldey(i))).lt.dabs(dimag(s2eyre)) ).and.
     &        (dabs(dimag(fzoldey(i))).gt.dabs(fmin)) ) .or.
     &
     &      ( (dabs(dimag(fzoldez(i+1))).lt.dabs(dimag(s1ezre)) ).and.
     &        (dabs(dimag(fzoldez(i+1))).gt.dabs(fmin)) ) .or.
     &
     &      ( (dabs(dimag(fzoldez(i))).lt.dabs(dimag(s2ezre)) ).and.
     &        (dabs(dimag(fzoldez(i))).gt.dabs(fmin)) ) )
     &       )then
	
        dz(ianzz)=0.5*dzold(i)
        dz(ianzz+1)=0.5*dzold(i)
	z(ianzz+1)=z(ianzz)+dz(ianzz)

	call yyint(m4,g,a,src,xi,xir,apr,cs,ra,ifl,st,
     &		z(ianzz+1),fwerty,fwertz,
     &          yintya,yintyp,yintza,yintzp)

	if(ifl.ispline.ge.0)then
		fzey(ianzz+1)=fwerty
		fzez(ianzz+1)=fwertz
	endif
	if(ifl.ispline.lt.0)then
	 	fya(ianzz+1)=yintya
	 	fza(ianzz+1)=yintza
	 	fyp(ianzz+1)=yintyp
	 	fzp(ianzz+1)=yintzp
	endif
	fmax=dmax1(fmax,cdabs(fzey(ianzz+1)),
     &       cdabs(fzez(ianzz+1)))
	fmin=xi.fracz*fmax
	ianzz=ianzz+2

	else

	ianzz=ianzz+1

	endif

	else 	           ! ianzz.lt.xi.imaxz
	  ianzz=ianzz+1
	endif

	enddo        ! naechster Wert vom alten Raster
c                    ---------------------------------

	z(ianzz)=zold(iold)
	fzey(ianzz)=fzoldey(iold)
	fzez(ianzz)=fzoldez(iold)

c------------------------------------------------------

	if((ianzz.gt.iold).and.
     &     ((iterz.gt.0).and.(ianzz.lt.xi.imaxz)))then

	iterz=iterz-1

	iold=ianzz
        goto 1001		! next iteration
c                               ----------------
	endif

	if(iterz.eq.0)st.inumzit=st.inumzit+1
	if(ianzz.ge.xi.imaxz)st.inumzan=st.inumzan+1

	ispline_save=ifl.ispline

	if(ifl.ispline.eq.-2)then
	ifl.ispline=0
	do i=1,ianzz
		fzey(i)=fya(i)*(dcos(fyp(i))+cs.sqrtm1*dsin(fyp(i)))
		fzez(i)=fza(i)*(dcos(fzp(i))+cs.sqrtm1*dsin(fzp(i)))
	enddo		
	endif

	if(ifl.ispline.eq.-1)call phacor(cs,xi,xir,dz,fya,fyp,ianzz)

	call simpson(cs,ifl,xi,xir,ianzz,dz,fzey,fya,fyp,xir.yzintey,
     &			xir.yzintya,xir.yzintyp)

	if(ifl.ispline.eq.-1)call phacor(cs,xi,xir,dz,fza,fzp,ianzz)

	call simpson(cs,ifl,xi,xir,ianzz,dz,fzez,fza,fzp,xir.yzintez,
     &			xir.yzintza,xir.yzintzp)

	ifl.ispline=ispline_save

c------------------------------------------------------------
	endif
c------------------------------------------------------------

	return
	end

c*************************************************************
	subroutine yyint(m4,g,a,src,xi,xir,apr,cs,ra,ifl,st,
     &		dzi,yintey,yintez,
     &          yintya,yintyp,yintza,yintzp)
c*************************************************************
c
c	INPUT : dzi
c      OUTPUT : yint, yintey, yintez
c
c*************************************************************

	implicit real*8(a-h,o-z)
	complex*16 fwert,fwerty,fwertz,
     &      s1,s1eyre,s1eyim,s1ezre,s1ezim,
     &      s2,s2eyre,s2eyim,s2ezre,s2ezim,
     &      yint,yintey,yintez
	complex*16 fyey(4096),fyoldey(4096)
	complex*16 fyez(4096),fyoldez(4096)

	dimension a(0:5,0:5)

	include 'phase_struct_10.for'
        record /constants/ cs
        record /geometryst/ g
        record /rayst/ ra
        record /source_results/ sr
        record /control_flags/ ifl
        record /sources/ src
	record /integration/ xi
	record /integration_results/ xir
        record /source6/ so6
	record /apertures/ apr
	record /statistics/ st
	record /map4/ m4

	dimension y(4096),dy(4096)
	dimension yold(4096),dyold(4096)
	dimension fya(4096),fyp(4096)
	dimension fza(4096),fzp(4096)

c*******************************************************
c	Erstellen eines nochmals reduzierten 
c       Konstantensatzes
c*******************************************************

c------------ changed 16.8.1996 ----------
	  if(xi.distfoc.gt.1.e-10)then
	    dzii=dzi+ra.ri.zi/xi.distfoc
	    else
	    dzii=dzi
	  endif
c-----------------------------------------

	ra.ri.dzi=dzi
c	ra.ri.dzi=dzii

        call subm2(m4.fdetrc,dzii,m4.fdtrrc)
        call subm2(m4.fdetphrc,dzii,m4.fdtphrrc)
        call subm2(m4.fdet1phrc,dzii,m4.fdt1phrrc)
        call subm2(m4.yprc1,dzii,m4.yprrc1)
        call subm2(m4.zprc1,dzii,m4.zprrc1)
        call subm2(m4.dyprc,dzii,m4.dyprrc)
        call subm2(m4.dzprc,dzii,m4.dzprrc)
        call subm2(m4.ypc_ap_r,dzii,m4.ypc_ap_rr)
        call subm2(m4.zpc_ap_r,dzii,m4.zpc_ap_rr)
        call subm2(m4.xlen1c_r,dzii,m4.xlen1c_rr)
        call subm2(m4.xlen2c_r,dzii,m4.xlen2c_rr)

        call subm2(m4.wrc,dzii,m4.wrrc)
        call subm2(m4.xlrc,dzii,m4.xlrrc)

c        call subm2(m4.fdetrc,dzi,m4.fdtrrc)
c        call subm2(m4.fdetphrc,dzi,m4.fdtphrrc)
c        call subm2(m4.fdet1phrc,dzi,m4.fdt1phrrc)
c        call subm2(m4.yprc1,dzi,m4.yprrc1)
c        call subm2(m4.zprc1,dzi,m4.zprrc1)
c        call subm2(m4.dyprc,dzi,m4.dyprrc)
c        call subm2(m4.dzprc,dzi,m4.dzprrc)
c
c        call subm2(m4.wrc,dzi,m4.wrrc)
c        call subm2(m4.xlrc,dzi,m4.xlrrc)

c------------ end of changes 16.8.1996

	ianzy=xi.ianzy0
	itery=xi.itery0
	fmax=0.

c*** Number of Iterations:                      itery
c*** Integrationsgrenzen:                       ymin, ymax
c*** Anzahl der Stuetzstellen im ersten Raster: ianzy
c*** Maximale Anzahl von Stuetzstellen:         imaxy
c*** Verhaeltnis von fmin zu fmax:              fracy
c*** minimaler Quotient benachbarter Y-Werte:   frac1y

c---------------- START ---------------------------------------
c---------------- first grid is equdistant, the following are not

	if(ianzy.eq.1)then
	dyy=0.
	else
	dyy=(xi.ymax-xi.ymin)/dflotj(ianzy-1)
	endif

c--------------------------------------------------------------
	if(src.isrctype.lt.4)then
c--------------------------------------------------------------
c
c	noch nicht getestet, vergl. mit isrctype.eq.4
c
c----------------------------------------------------------

	do i=1,ianzy
 	  y(i)=xi.ymin+dflotj(i-1)*dyy
 	  call fywert(m4,g,a,src,xi,xir,apr,cs,ra,ifl,st,
     &		y(i),fwerty,fwertz,fwya,fwyp,fwza,fwzp)
	  fyey(i)=cdabs(fwerty)
	  dy(i)=dyy
	  fmax=dmax1(fmax,cdabs(fwerty))
	enddo

	fmin=xi.fracy*fmax

c******* next iteration *********************************

1000    continue
c-------- save old values ------------
	do i=1,ianzy
	yold(i)=y(i)
	fyoldey(i)=fyey(i)
	dyold(i)=dy(i)
	iold=ianzy
	enddo
c-------------------------------------

	ianzy=1

	do i=1,iold-1          ! Summation ueber altes Raster
	y(ianzy)=yold(i)
	fyey(ianzy)=fyoldey(i)
	dy(ianzy)=dyold(i)
	fmax=dmax1(fmax,cdabs(fyey(ianzy)))
	fmin=xi.fracy*fmax

	if(ianzy.lt.(xi.imaxy-(iold-i)))then	! insert grid point

	s1=xi.frac1y*fyoldey(i)
	s2=xi.frac1y*fyoldey(i+1)

	if(  (  ((cdabs(fyoldey(i+1)).lt.cdabs(s1)).or.
     &          (cdabs(fyoldey(i)).lt.cdabs(s2))   )  )
     &  .and.  
     &     (
     &     ((cdabs(fyoldey(i)).gt.fmin).or.
     &      (cdabs(fyoldey(i+1)).gt.fmin))   
     &  .or.
     &     ((cdabs(fyoldey(i)).gt.fmin).and.
     &      (cdabs(fyoldey(i+1)).gt.fmin))   
     &      )
     &              )then

        dy(ianzy)=0.5*dyold(i)
        dy(ianzy+1)=0.5*dyold(i)
	y(ianzy+1)=y(ianzy)+dy(ianzy)
 	call fywert(m4,g,a,src,xi,xir,apr,cs,ra,ifl,st,
     &		y(ianzy+1),fwerty,fwertz,fwya,fwyp,fwza,fwzp)
	fyey(ianzy+1)=cdabs(fwerty)
	fmax=dmax1(fmax,cdabs(fyey(ianzy+1)))
	fmin=xi.fracy*fmax
	ianzy=ianzy+2

	else

	ianzy=ianzy+1

	endif

	else 	     ! ianzy.lt.xi.imaxy
	ianzy=ianzy+1
	endif	

	enddo        ! naechster Wert vom alten Raster

	y(ianzy)=yold(iold)
	fyey(ianzy)=fyoldey(iold)

	if((ianzy.gt.iold).and.
     &     ((itery.gt.0).and.(ianzy.lt.xi.imaxy)))then

	itery=itery-1
	iold=ianzy
        goto 1000		! next iteration
	endif

	if(itery.eq.0)st.inumyit=st.inumyit+1
	if(ianzy.ge.xi.imaxy)st.inumyan=st.inumyan+1

	call simpson(cs,ifl,xi,xir,ianzy,dy,fyey,fya,fyp,
     &		yintey,yintya,yintyp)

	endif

c------------------------------------------------------------
	if((src.isrctype.eq.4).or.(src.isrctype.eq.5).or.
     &                        (src.isrctype.eq.6))then
c------------------------------------------------------------
	do i=1,ianzy
	  ra.n4=i
 	  y(i)=xi.ymin+dflotj(i-1)*dyy
 	  call fywert(m4,g,a,src,xi,xir,apr,cs,ra,ifl,st,
     &		y(i),fwerty,fwertz,fwya,fwyp,fwza,fwzp)
	  if(ifl.ibright.eq.1)then
	  src.so6.abr(ra.n1,ra.n2,ra.n3,ra.n4)=dreal(fwerty)
	  endif
	  fyey(i)=fwerty
	  fyez(i)=fwertz
	  if(ifl.ispline.lt.0)then
	   fya(i)=fwya
	   fza(i)=fwza
	   fyp(i)=fwyp
	   fzp(i)=fwzp
	  endif
	  fmax=dmax1(fmax,cdabs(fwerty),cdabs(fwertz))
	  dy(i)=dyy
	enddo

	fmin=xi.fracy*fmax

c******* next iteration *********************************

1001    continue
c-------- save old values ------------
	do i=1,ianzy
	yold(i)=y(i)
	fyoldey(i)=fyey(i)
	fyoldez(i)=fyez(i)
	dyold(i)=dy(i)
	iold=ianzy
	enddo
c-------------------------------------

	ianzy=1

	do i=1,iold-1          ! Summation ueber altes Raster

	y(ianzy)=yold(i)
	fyey(ianzy)=fyoldey(i)
	fyez(ianzy)=fyoldez(i)
	fmax=dmax1(fmax,cdabs(fyey(ianzy)),cdabs(fyez(ianzy)))
	dy(ianzy)=dyold(i)
	fmin=xi.fracy*fmax

	if(ianzy.lt.(xi.imaxy-(iold-i)))then	! insert grid point

	s1eyre=xi.frac1y*fyoldey(i)
	s1ezre=xi.frac1z*fyoldez(i)
	s2eyre=xi.frac1y*fyoldey(i+1)
	s2ezre=xi.frac1z*fyoldez(i+1)

	s1eyim=xi.frac1y*fyoldey(i)
	s1ezim=xi.frac1z*fyoldez(i)
	s2eyim=xi.frac1y*fyoldey(i+1)
	s2ezim=xi.frac1z*fyoldez(i+1)

	if( (  
     &      ( (dabs(dreal(fyoldey(i+1))).lt.dabs(dreal(s1eyre)) ).and.
     &        (dabs(dreal(fyoldey(i+1))).gt.dabs(fmin))  ) .or.
     &
     &      ( (dabs(dreal(fyoldey(i))).lt.dabs(dreal(s2eyre)) ).and.
     &        (dabs(dreal(fyoldey(i))).gt.dabs(fmin)) ) .or.
     &
     &      ( (dabs(dreal(fyoldez(i+1))).lt.dabs(dreal(s1ezre)) ).and.
     &        (dabs(dreal(fyoldez(i+1))).gt.dabs(fmin)) ) .or.
     &
     &      ( (dabs(dreal(fyoldez(i))).lt.dabs(dreal(s2ezre)) ).and.
     &        (dabs(dreal(fyoldez(i))).gt.dabs(fmin)) ) .or.
     &
     &      ( (dabs(dimag(fyoldey(i+1))).lt.dabs(dimag(s1eyre)) ).and.
     &        (dabs(dimag(fyoldey(i+1))).gt.dabs(fmin)) ) .or.
     &
     &      ( (dabs(dimag(fyoldey(i))).lt.dabs(dimag(s2eyre)) ).and.
     &        (dabs(dimag(fyoldey(i))).gt.dabs(fmin)) ) .or.
     &
     &      ( (dabs(dimag(fyoldez(i+1))).lt.dabs(dimag(s1ezre)) ).and.
     &        (dabs(dimag(fyoldez(i+1))).gt.dabs(fmin)) ) .or.
     &
     &      ( (dabs(dimag(fyoldez(i))).lt.dabs(dimag(s2ezre)) ).and.
     &        (dabs(dimag(fyoldez(i))).gt.dabs(fmin)) ) )
     &       )then
	
        dy(ianzy)=0.5*dyold(i)
        dy(ianzy+1)=0.5*dyold(i)
	y(ianzy+1)=y(ianzy)+dy(ianzy)

 	call fywert(m4,g,a,src,xi,xir,apr,cs,ra,ifl,st,
     &		y(ianzy+1),fwerty,fwertz,fwya,fwyp,fwza,fwzp)
	fyey(ianzy+1)=fwerty
	fyez(ianzy+1)=fwertz
	  if(ifl.ispline.lt.0)then
	   fya(ianzy+1)=fwya
	   fza(ianzy+1)=fwza
	   fyp(ianzy+1)=fwyp
	   fzp(ianzy+1)=fwzp
	  endif
	fmax=dmax1(fmax,cdabs(fyey(ianzy+1)),cdabs(fyez(ianzy+1)))
	fmin=xi.fracy*fmax
	ianzy=ianzy+2

	else

	ianzy=ianzy+1

	endif

	else 	     ! ianzy.lt.xi.imaxy
	ianzy=ianzy+1
	endif	

	enddo        ! naechster Wert vom alten Raster

	y(ianzy)=yold(iold)
	fyey(ianzy)=fyoldey(iold)
	fyez(ianzy)=fyoldez(iold)

	if((ianzy.gt.iold).and.
     &     ((itery.gt.0).and.(ianzy.lt.xi.imaxy)))then

	itery=itery-1
	iold=ianzy
        goto 1001		! next iteration
	endif

	if(itery.eq.0)st.inumyit=st.inumyit+1
	if(ianzy.ge.xi.imaxy)st.inumyan=st.inumyan+1

c**************************** checken *************************

	call simpson(cs,ifl,xi,xir,ianzy,dy,fyey,fya,fyp,
     &		yintey,yintya,yintyp)
	call simpson(cs,ifl,xi,xir,ianzy,dy,fyez,fza,fzp,
     &		yintez,yintza,yintzp)

c---------------------------------------------------------
	endif
c---------------------------------------------------------

	st.inumb(st.nn1,st.nn2)=st.inumb(st.nn1,st.nn2)+ianzy

	return
	end

c*************************************************************
 	subroutine fywert(m4,g,a,src,xi,xir,apr,cs,ra,ifl,st,
     &		dyi,fwerty,fwertz,fwya,fwyp,fwza,fwzp)
c*************************************************************
	implicit real*8(a-h,o-z)
	complex*16 densy,densz,fwert,fwerty,fwertz

	dimension a(0:5,0:5)

	include 'phase_struct_10.for'
        record /constants/ cs
        record /geometryst/ g
        record /rayst/ ra
        record /source_results/ sr
        record /control_flags/ ifl
        record /sources/ src
	record /integration/ xi
	record /integration_results/ xir
	record /apertures /apr
	record /statistics/ st
        record /parder/ pd
	record /map4/ m4

      dimension p1(0:4,0:4,0:4,0:4)
      dimension p2(0:4,0:4,0:4,0:4)
      dimension p12(0:4,0:4,0:4,0:4)
      dimension p3(0:4,0:4,0:4,0:4)
      dimension p4(0:4,0:4,0:4,0:4)
      dimension p34(0:4,0:4,0:4,0:4)
      dimension p1234(0:4,0:4,0:4,0:4)
      dimension p11(0:4,0:4,0:4,0:4)
      dimension p22(0:4,0:4,0:4,0:4)
      dimension p1122(0:4,0:4,0:4,0:4)
      dimension p1a(0:4,0:4,0:4,0:4)
      dimension p2a(0:4,0:4,0:4,0:4)
      dimension p3a(0:4,0:4,0:4,0:4)
      dimension p4a(0:4,0:4,0:4,0:4)
      dimension p11a(0:4,0:4,0:4,0:4)
      dimension p22a(0:4,0:4,0:4,0:4)
      dimension p1122a(0:4,0:4,0:4,0:4)

	  if(xi.distfoc.gt.1.e-10)then
	    dyii=dyi+ra.ri.yi/xi.distfoc
	    else
	    dyii=dyi
	  endif

	ra.ri.dyi=dyii	

	ra.rf.yp=0.
	ra.rf.zp=0.
	ra.rf.dyp=0.
	ra.rf.dzp=0.
        fd=0.
	fdph=0.
	fd1ph=0.
	ra.ap.yp_ap=0.
	ra.ap.zp_ap=0.
	ra.xlength1=0.
	ra.xlength2=0.

	ra.oe.w=0.
	ra.oe.xl=0.

        var0=1.
        do ii=0,ifl.iord
          ra.rf.yp=ra.rf.yp+m4.yprrc1(ii)*var0
          ra.rf.zp=ra.rf.zp+m4.zprrc1(ii)*var0
          ra.rf.dyp=ra.rf.dyp+m4.dyprrc(ii)*var0
          ra.rf.dzp=ra.rf.dzp+m4.dzprrc(ii)*var0
          ra.ap.yp_ap=ra.ap.yp_ap+m4.ypc_ap_rr(ii)*var0
          ra.ap.zp_ap=ra.ap.zp_ap+m4.ypc_ap_rr(ii)*var0
	  ra.xlength1=ra.xlength1+m4.xlen1c_rr(ii)*var0
	  ra.xlength2=ra.xlength2+m4.xlen2c_rr(ii)*var0

          fd=fd+m4.fdtrrc(ii)*var0
          fdph=fdph+m4.fdtphrrc(ii)*var0
          fd1ph=fd1ph+m4.fdt1phrrc(ii)*var0

	  ra.oe.w=ra.oe.w+m4.wrrc(ii)*var0
          ra.oe.xl=ra.oe.xl+m4.xlrrc(ii)*var0

          var0=var0*dyii
        enddo

c---------- new 3.6.1997
	fd1ph=0.d0

	var0=1.d0
        do ii=0,ifl.iordsc
	  var1=var0
          do jj=0,ifl.iordsc-ii
	    var2=var1
              do kk=0,ifl.iordsc-ii-jj
	        var3=var2
		do ll=0,ifl.iordsc-ii-jj-kk
          	  fd1ph=fd1ph+m4.fdet1phc(ii,jj,kk,ll)*var3
	      	  var3=var3*ra.rf.dzp
	      	enddo
	    var2=var2*ra.rf.dyp
	    enddo
	  var1=var1*ra.rf.zp
	  enddo
	var0=var0*ra.rf.yp
	enddo
c---------- end new 3.6.1997

c----------new 28.4.
	if(ifl.ipath.eq.0)fd1ph=1.d0/dsqrt(dabs(fd1ph))
	if((ifl.ipath.eq.1).or.(ifl.ipath.eq.2))then
	  fd1ph=1.d0
	  fd=1.d0
	  fdph=1.d0
	endif
c--------- end new

c----------------------------------------------
	if(ifl.matrel.eq.1)then
c----------------------------------------------
c--------- fd und fdph genauer: ---------------

	delta=1.e-20

	yi=ra.ri.yi
	zi=ra.ri.zi
	dyi=ra.ri.dyi
	dzi=ra.ri.dzi

	fd1=0.
	fd2=0.

        call subl(m4.ypc1,m4.zpc1,m4.dypc,m4.dzpc,m4.xmec)
			! Berechnung der Matrixelemente xmec
			! Indizes 1 und 2: Reihe, Spalte
			! Indizes 3 bis 6: Koeffizienten von
			! yp, zp, dyp, dzp

c----- check:

       do i=1,4

        do j=1,4
        if(j.ne.i)then

         do k=1,4
         if((k.ne.i).and.(k.ne.j))then
          do l=1,4
          if((l.ne.i).and.(l.ne.j).and.(l.ne.k))then

         call sig(i,j,k,l,isig)

         do n1=0,ifl.iord
         do n2=0,ifl.iord
         do n3=0,ifl.iord
         do n4=0,ifl.iord
           p1(n1,n2,n3,n4)=m4.xmec(1,i,n1,n2,n3,n4)
           p2(n1,n2,n3,n4)=m4.xmec(2,j,n1,n2,n3,n4)
           p3(n1,n2,n3,n4)=m4.xmec(3,k,n1,n2,n3,n4)
           p4(n1,n2,n3,n4)=m4.xmec(4,l,n1,n2,n3,n4)
         enddo
         enddo
         enddo
         enddo

       call p_m_4(p1,p2,p12)
       call p_m_4(p3,p4,p34)
       call p_m_4(p12,p34,p1234)

	pp1234=0.d0
	do iii=0,ifl.iord
	 do jjj=0,ifl.iord-iii
	  do kkk=0,ifl.iord-iii-jjj
	   do lll=0,ifl.iord-iii-jjj-kkk
	   pp1234=pp1234+p1234(iii,jjj,kkk,lll)*
     &             (yi+delta)**iii*(zi+delta)**jjj*
     &             (dyi+delta)**kkk*(dzi+delta)**lll
	   enddo
	  enddo
         enddo
        enddo

       	fd1=fd1+dflotj(isig)*pp1234

         endif
        enddo
        endif
       enddo
       endif
      enddo
      enddo

c----- jetzt genauer:

       do i=1,4

        do j=1,4
        if(j.ne.i)then

         do k=1,4
         if((k.ne.i).and.(k.ne.j))then
          do l=1,4
          if((l.ne.i).and.(l.ne.j).and.(l.ne.k))then

         call sig(i,j,k,l,isig)

         do n1=0,ifl.iord
         do n2=0,ifl.iord
         do n3=0,ifl.iord
         do n4=0,ifl.iord
           p1(n1,n2,n3,n4)=m4.xmec(1,i,n1,n2,n3,n4)
           p2(n1,n2,n3,n4)=m4.xmec(2,j,n1,n2,n3,n4)
           p3(n1,n2,n3,n4)=m4.xmec(3,k,n1,n2,n3,n4)
           p4(n1,n2,n3,n4)=m4.xmec(4,l,n1,n2,n3,n4)
         enddo
         enddo
         enddo
         enddo

	pp1=0.
	pp2=0.
	pp3=0.
	pp4=0.
	do iii=0,ifl.iord
	 do jjj=0,ifl.iord-iii
	  do kkk=0,ifl.iord-iii-jjj
	   do lll=0,ifl.iord-iii-jjj-kkk
	   pp1=pp1+p1(iii,jjj,kkk,lll)*
     &             (yi+delta)**iii*(zi+delta)**jjj*
     &             (dyi+delta)**kkk*(dzi+delta)**lll
	   pp2=pp2+p2(iii,jjj,kkk,lll)*
     &             (yi+delta)**iii*(zi+delta)**jjj*
     &             (dyi+delta)**kkk*(dzi+delta)**lll
	   pp3=pp3+p3(iii,jjj,kkk,lll)*
     &             (yi+delta)**iii*(zi+delta)**jjj*
     &             (dyi+delta)**kkk*(dzi+delta)**lll
	   pp4=pp4+p4(iii,jjj,kkk,lll)*
     &             (yi+delta)**iii*(zi+delta)**jjj*
     &             (dyi+delta)**kkk*(dzi+delta)**lll

	   enddo
	  enddo
         enddo
        enddo

       	fd2=fd2+dflotj(isig)*pp1*pp2*pp3*pp4

c----------------------------------------------

         endif
        enddo
        endif
       enddo
       endif
      enddo
      enddo

	ra.fd1(ra.n4)=fd1
	ra.fd2(ra.n4)=fd2

c------------------------------------------
c------- check
         do n1=0,ifl.iord
         do n2=0,ifl.iord
         do n3=0,ifl.iord
         do n4=0,ifl.iord
           p1(n1,n2,n3,n4)=m4.xmec(1,3,n1,n2,n3,n4)
           p2(n1,n2,n3,n4)=m4.xmec(2,4,n1,n2,n3,n4)
           p3(n1,n2,n3,n4)=m4.xmec(1,4,n1,n2,n3,n4)
           p4(n1,n2,n3,n4)=m4.xmec(2,3,n1,n2,n3,n4)
         enddo
         enddo
         enddo
         enddo

        call p_m_4(p1,p2,p12)
        call p_m_4(p3,p4,p34)

	pp12=0.d0
	pp34=0.d0

	do iii=0,ifl.iord
	 do jjj=0,ifl.iord-iii
	  do kkk=0,ifl.iord-iii-jjj
	   do lll=0,ifl.iord-iii-jjj-kkk
	   pp12=pp12+p12(iii,jjj,kkk,lll)*
     &             (yi+delta)**iii*(zi+delta)**jjj*
     &             (dyi+delta)**kkk*(dzi+delta)**lll
	   pp34=pp34+p34(iii,jjj,kkk,lll)*
     &             (yi+delta)**iii*(zi+delta)**jjj*
     &             (dyi+delta)**kkk*(dzi+delta)**lll
	   enddo
	  enddo
         enddo
        enddo

	fdph1=pp12-pp34

c------- jetzt genauer:

	pp1=0.
	pp2=0.
	pp3=0.
	pp4=0.
	do iii=0,ifl.iord
	 do jjj=0,ifl.iord-iii
	  do kkk=0,ifl.iord-iii-jjj
	   do lll=0,ifl.iord-iii-jjj-kkk
	   pp1=pp1+p1(iii,jjj,kkk,lll)*
     &             (yi+delta)**iii*(zi+delta)**jjj*
     &             (dyi+delta)**kkk*(dzi+delta)**lll
	   pp2=pp2+p2(iii,jjj,kkk,lll)*
     &             (yi+delta)**iii*(zi+delta)**jjj*
     &             (dyi+delta)**kkk*(dzi+delta)**lll
	   pp3=pp3+p3(iii,jjj,kkk,lll)*
     &             (yi+delta)**iii*(zi+delta)**jjj*
     &             (dyi+delta)**kkk*(dzi+delta)**lll
	   pp4=pp4+p4(iii,jjj,kkk,lll)*
     &             (yi+delta)**iii*(zi+delta)**jjj*
     &             (dyi+delta)**kkk*(dzi+delta)**lll
	   enddo
	  enddo
         enddo
        enddo

       	fdph2=pp1*pp2-pp3*pp4

	ra.fdph1(ra.n4)=fdph1
	ra.fdph2(ra.n4)=fdph2

c------------------------------------------
	endif
c------------------------------------------

	ra.fd(ra.n4)=fd
	ra.fdph(ra.n4)=fdph
	if((ra.n3.le.201).and.(ra.n4.le.201))then
	  ra.fd1ph(ra.n3,ra.n4)=fd1ph
	endif

c----------------------------------------

        call psdi(g,src,apr,cs,ifl,ra,sr)

c----------------------------------------

	if(src.isrctype.lt.4)then
	fwerty=cdabs(sr.densy*abs(fd))
	endif

	if((src.isrctype.eq.4).or.(src.isrctype.eq.5))then

	if(ifl.inorm1.eq.1)fdph=1.d0

	if(ifl.inorm1.eq.0)fdph=dabs(fdph)
	
	if(ifl.ispline.ge.0)then
		fwerty=sr.densy*fdph
		fwertz=sr.densz*fdph
	endif
	if(ifl.ispline.lt.0)then
		fwya=sr.eya*fdph
		fwza=sr.eza*fdph
		fwyp=sr.eyp
		fwzp=sr.ezp
	endif

	if(ifl.inorm2.eq.1)then
	  fd1ph=1.d0
	  if((ra.n3.le.201).and.(ra.n4.le.201))then
	  ra.fd1ph(ra.n3,ra.n4)=fd1ph
	  endif
	endif

	if((ifl.inorm2.eq.2).or.(ifl.inorm2.eq.3))then
	  call subdfpl1(g,a,ra,pd)
	  call subdfpl2(g,a,ra,pd)
	endif

	if(ifl.inorm2.eq.2)then
	  fd1ph=dsqrt(dabs(pd.pl1w2+pd.pl2w2))*
     &        dsqrt(dabs(pd.pl1l2+pd.pl2l2))
	  fd1ph=1.d0/(fd1ph*dsqrt(pd.arg1)*dsqrt(pd.arg2))
	  if((ra.n3.le.201).and.(ra.n4.le.201))then
	  ra.fd1ph(ra.n3,ra.n4)=fd1ph
	  endif
	endif

	if(ifl.inorm2.eq.3)then
	  fd1ph=dsqrt(dabs(  
     &      (pd.pl1w2+pd.pl2w2) * (pd.pl1l2+pd.pl2l2) -
     &      (pd.pl1w1l1+pd.pl2w1l1) * (pd.pl1w1l1+pd.pl2w1l1)
     &               )    )
	  fd1ph=1.d0/(fd1ph*dsqrt(pd.arg1)*dsqrt(pd.arg2))
	  if((ra.n3.le.201).and.(ra.n4.le.201))then
	  ra.fd1ph(ra.n3,ra.n4)=fd1ph
	  endif
	endif

	if(ifl.ispline.ge.0)then
		fwerty=fwerty*fd1ph
		fwertz=fwertz*fd1ph
	endif
	if(ifl.ispline.lt.0)then
		fwya=fwya*fd1ph
		fwza=fwza*fd1ph
	endif

	st.fd1phmax(ra.n1,ra.n2)=
     &        dmax1(fd1ph,st.fd1phmax(ra.n1,ra.n2))

	endif

	if(src.isrctype.eq.6)then
	fwerty=sr.densy*abs(fd)
	endif

	return
	end


c**************************************************************
	subroutine int_2d(f,xint,dy,dz,ny,nz)
c**************************************************************

	implicit real*8(a-h,o-z)

        dimension f(4096,4096),fz(4096)

        do i=1,ny
	 fz(i)=0.5*(f(i,1)+f(i,nz))
         do j=2,nz-1
          fz(i)=fz(i)+f(i,j)
         enddo
	 fz(i)=fz(i)*dz
	enddo

	xint=0.5*(fz(1)+fz(ny))
	do i=2,ny-1
         xint=xint+fz(i)
	enddo

	xint=xint*dy

	return
	end


c***********************************************************
	subroutine phacor(cs,xi,xir,dx,amp,pha,ianz)
c***********************************************************
c
c	PHACOR funktioniert folgendermassen:
c
c	1. Auffindung der Bereiche quasi stationaerer Phase:
c	----------------------------------------------------
c	
c	Zunaechst muss ein Bereich gefunden werden, wo die Phase sich
c	wenig aendert. Die Mitte dieses Bereiches ist IANZ0.
c	Von diesem Punkt ausgehend werden Amplitude und Phase rekonstru-
c	iert.
c	Steuerparameter: xi.d12_max
c			 xi.id12
c
c	Es kann mehrere Bereiche quasi stationaerer Phase
c	geben (z.B. im Coma eines Toroidspiegels). Daher
c	muss noch die Option fur mehere ianz0 implementiert werden.
c	Steuerparameter sind 
c
c	IANZ0_CAL = 1: Berechnung von IANZ0
c	IANZ0_CAL = 0: IANZ0 wird extern vorgegeben durch IANZ0_FIXED
c
c	2. Korrektur der Amplituden:
c	----------------------------
c	Ausgehend vom Zentrum des Bereiches quasi stationaerer Phase IANZ0
c	wird nach rechts und links hin korrigiert. Dies geschieht so: 
c	Durch 3 oder mehr Punkte wird entweder durch einen Spline
c	(xi.iord_amp.ge.0) oder ein Spline-Fit (xi.iord_amp.lt.0) ein Polynom 
c	gelegt. Daraus wird ein Schaetzwert fuer die naechste Amplitude 
c	extrapoliert. Haben geschaetzte und tatsaechliche Amplitude unter-
c	schiedliches Vorzeichen, so wird die tatsaechliche Amplitude mit 
c	dem Faktor -1 multipliziert und die zugehoerige Phase um pi erhoeht.
c	(xi.amp_change scheint nicht so wichtig zu sein).
c	Steuerparameter: xi.iamp_smooth
c			 xi.iord_amp
c			 xi.ifm_amp
c			 xi.amp_change
c
c	3. Korrektur der Phasen:
c	------------------------
c	Ausgehend vom Zentrum des Bereiches quasi stationaerer Phase IANZ0
c	wird nach rechts und links hin korrigiert. Dies geschieht so: 
c	Durch 3 oder mehr Punkte wird entweder durch einen Spline
c	(xi.iord_pha.ge.0) oder ein Spline-Fit (xi.iord_pha.lt.0) ein Polynom 
c	gelegt. Daraus wird ein Schaetzwert fuer die naechste Phase 
c	extrapoliert. Zur tatsaechlichen Phase werden so oft 2 pi bzw. 
c	- 2pi addiert, bis die Differenz zum Schaetzwert minimal wird.
c	Die Phase weist in einigen Bereichen Rippel auf, die leicht die
c	ganze Korrektur in den Wald laufen lassen. Es gibt zwei Methoden, 
c	dies zu vermeiden:
c	1. (xi.iord_pha.ge.0) Es wird bei der Berechnung des Schaetzwertes 
c	geprueft, ob die neue Steigung sehr viel anders ist als die alte. 
c	Wenn ja, wird der Schaetzwert mit der alten Steigung berechnet. 
c	2. (xi.iord_pha.lt.0) Zur Berechnung des Schaetzwertes werden deutlich
c	mehr Punkte als notwendig verwendet. Diese Mittelung verringert 
c	das Gewicht der Rippel.
c	Steuerparameter: xi.iord_pha
c			 xi.ifm_pha
c			 xi.phase_change_1
c			 xi.phase_change_2
c			 xi.iphase_curv
c			 xi.iphase_pi2
c
c	4. Korrektur auf Spruenge von 2 pi:
c	-----------------------------------
c	Fuer xi.iord_pha.ge.0 wird zum Schluss noch mal auf Spruenge von
c	2 pi korrigiert.
c
c----------------------------------------------------------
c
c	checken:
c	was passiert, wenn x3=-x1 und damit x3**2-x1**2=0 wird???
c	(kein Problem bei linearer Extrapolation)
c
c-----------------

	implicit real*8(a-h,o-z)

	include 'phase_struct_10.for'
        record /constants/ cs
	record /statistics/ st
	record /integration_results/ xir
	record /integration/ xi
        record /control_flags/ ifl

	dimension dx(4096),x(4096),
     &            amp(4096),pha(4096),pha_new(4096),
     &		  d1(4096),d2(4096),d12(4096),
     &		  istart(4096),iend(4096)


	dimension x_fit(1024),amp_fit(1024),pha_fit(1024)
	dimension coef(1024),coef1(1024),xx(100)

	small_loc=1.d-15
	small_loc1=1.d-10
	iii=0

	x(1)=0.d0
	do i=2,ianz
	x(i)=x(i-1)+dx(i-1)
	enddo

	if(xi.ianz0_cal.eq.0)then
	  ianz0=xi.ianz0_fixed
	else
c---------- Calculation of ianz0 -----------------------
c
c	ianz0 is the starting point for the 
c	phase correction procedure
c	for z=0 ianz0 has the value: ianz0=(ianz+1)/2
c		
c-------------------------------------------------------

	d1(1)=10.d0
	d1(2)=10.d0
	d1(3)=10.d0
	d1(ianz)=10.d0
	d1(ianz-1)=10.d0
	d1(ianz-2)=10.d0

	d2(1)=10.d0
	d2(2)=10.d0
	d2(3)=10.d0
	d2(ianz)=10.d0
	d2(ianz-1)=10.d0
	d2(ianz-2)=10.d0

c-------- Scan from left to right 

	do i=1,ianz-3
	x1=x(i)
	x2=x(i+1)
	x3=x(i+2)
	y1=pha(i)
	y2=pha(i+1)
	y3=pha(i+2)

	r=x2*x2-x1*x1
	s=x3*x3-x1*x1

	b=( (y3-y1)/s - (y2-y1)/r ) / ( (x3-x1)/s - (x2-x1)/r )
	a=(y3-y1-b*(x3-x1)) / s
	c=y1-a*x1*x1-b*x1

	pha_guess=a*x(i+3)*x(i+3)+b*x(i+3)+c

	d1(i+3)=dabs(pha_guess-pha(i+3))

	enddo

c-------- Scan from right to left 

	do i=ianz,4,-1
	x1=x(i)
	x2=x(i-1)
	x3=x(i-2)
	y1=pha(i)
	y2=pha(i-1)
	y3=pha(i-2)

	r=x2*x2-x1*x1
	s=x3*x3-x1*x1

	b=( (y3-y1)/s - (y2-y1)/r ) / ( (x3-x1)/s - (x2-x1)/r )
	a=(y3-y1-b*(x3-x1)) / s
	c=y1-a*x1*x1-b*x1

	pha_guess=a*x(i-3)*x(i-3)+b*x(i-3)+c

	d2(i-3)=dabs(pha_guess-pha(i-3))

	enddo

c------ determine smoothness of the phase

	d12(1)=10.d0
	d12(2)=10.d0
	d12(3)=10.d0
	d12(ianz)=10.d0
	d12(ianz-1)=10.d0
	d12(ianz-2)=10.d0

	do i=4,ianz-3
c	d12(i)=dsqrt(d1(i)*d1(i)*d2(i)*d2(i))
	d12(i)=d1(i)
	if(d12(i).eq.0.d0)d12(i)=xi.d12_max
	enddo

c------ now, search ianz0
c	xi.d12_max=0.1

	istart_flag=1	! search for istart
	iend_flag=0	! do not serach for iend

	iregion=0
	do i=1,ianz
	if(istart_flag.eq.1)then
		if(d12(i).lt.xi.d12_max)then
			iregion=iregion+1		
			istart(iregion)=i
			istart_flag=0	! do not search for istart
			iend_flag=1	! search for iend
		endif
	endif
	if(iend_flag.eq.1)then
		if(d12(i).lt.xi.d12_max)then
			iend(iregion)=i
		       else
			istart_flag=1
			iend_flag=0
		endif
	endif

	enddo

	iimax=0

	if(iregion.gt.0)then
	do i=1,iregion
	ii=iend(i)-istart(i)+1
	if(ii.gt.iimax)then
		iimax=ii
		iiregion=i
	endif
	enddo

	ianz0=(istart(iiregion)+iend(iiregion))/2

	else

	ianz0=ianz/2

	endif

c------------- neu: 10.11.1996 -----
	ianz0=ianz0-1
c-----------------------------------

c	do i=1,iregion
c	type*,istart(i),iend(i)
c	enddo
c	type*,istart(iiregion),iend(iiregion),ianz0
c	type*,' ianz0,x(ianz0) ',ianz0,x(ianz0)

c----------------------------------
	if(xi.id12.eq.1)then
c	Output
c----------------------------------

	do i=1,ianz
	xir.d12(1,1,i)=x(i)
	xir.d12(2,1,i)=d1(i)
	enddo
	xir.ianzd12(1)=ianz

	do i=1,ianz
	xir.d12(1,2,i)=x(i)
	xir.d12(2,2,i)=d2(i)
	enddo
	xir.ianzd12(2)=ianz

	do i=1,ianz
	xir.d12(1,3,i)=x(i)
	xir.d12(2,3,i)=d12(i)
	enddo
	xir.ianzd12(3)=ianz

	endif

c---------------------------------------------
	endif	! (ianz0_cal.eq.1)
c---------------------------------------------

	if(xi.iamp_smooth.eq.0)goto 9988
	
c------- check smoothness of amplitude ------------------
c	version 1	
c-------------------------------------
	if(xi.iord_amp.ge.0)then
c-------------------------------------

	do i=ianz0,ianz-2
	x1=x(i-1)
	x2=x(i)
	x3=x(i+1)
	y1=amp(i-1)
	y2=amp(i)
	y3=amp(i+1)

	if(xi.iord_amp.eq.0)then
		a=0.d0
		b=0.d0
		c=y3
	endif

	if(xi.iord_amp.eq.1)then
		a=0.d0
		a_old=0.d0
		b=(y2-y3)/(x2-x3)
		c=y2-b*x2
	endif

	if(xi.iord_amp.eq.2)then
	r=x2*x2-x1*x1
	s=x3*x3-x1*x1

	b=( (y3-y1)/s - (y2-y1)/r ) / ( (x3-x1)/s - (x2-x1)/r )
	a=(y3-y1-b*(x3-x1)) / s
	c=y1-a*x1*x1-b*x1
	endif

	if(i.gt.ianz0)then
	  if(dabs(b-b_old).gt.xi.amp_change)then
	  a=a_old
	  b=b_old
	  c=c_old
	  else
	  a_old=a
	  b_old=b
	  c_old=c
	  endif
	else
	  a_old=a
	  b_old=b
	  c_old=c
	endif  

	amp_guess=a*x(i+2)*x(i+2)+
     &      	b*x(i+2)+c
	if(dabs(amp_guess-amp(i+2)).gt.
     &	   dabs(amp_guess+amp(i+2)) )then
		amp(i+2)=-amp(i+2)
		pha(i+2)=pha(i+2)+cs.pi
	endif

	enddo

	do i=ianz0,3,-1
	x1=x(i-1)
	x2=x(i)
	x3=x(i+1)
	y1=amp(i-1)
	y2=amp(i)
	y3=amp(i+1)

	if(xi.iord_amp.eq.0)then
		a=0.d0
		b=0.d0
		c=y1
	endif

	if(xi.iord_amp.eq.1)then
		a=0.d0
		a_old=0.d0
		b=(y1-y2)/(x1-x2)
		c=y1-b*x1
	endif

	if(xi.iord_amp.eq.2)then
	r=x2*x2-x1*x1
	s=x3*x3-x1*x1

	b=( (y3-y1)/s - (y2-y1)/r ) / ( (x3-x1)/s - (x2-x1)/r )
	a=(y3-y1-b*(x3-x1)) / s
	c=y1-a*x1*x1-b*x1
	endif

	if(i.lt.ianz0)then
	  if(dabs(b-b_old).gt.xi.amp_change)then
	  a=a_old
	  b=b_old
	  c=c_old
	  else
	  a_old=a
	  b_old=b
	  c_old=c
	  endif
	else
	  a_old=a
	  b_old=b
	  c_old=c
	endif

	amp_guess=a*x(i-2)*x(i-2)+b*x(i-2)+c
	if(dabs(amp_guess-amp(i-2)).gt.
     &		dabs(amp_guess+amp(i-2)) )then
		amp(i-2)=-amp(i-2)
		pha(i-2)=pha(i-2)+cs.pi
	endif

c	type*,'***',b

	enddo

c----------------------------------
	endif	! (xi.iord_amp.ge.0)
c----------------------------------

	if(xi.iamp_smooth.eq.1)goto 9989
	if(xi.iamp_smooth.eq.2)goto 9990

9989	continue
c----------------------------------
	if(xi.iord_amp.lt.0)then
c----------------------------------
	iiord=-xi.iord_amp

c-------- go from ianz0 to the right
	if(ianz0.le.ianz-2)then
	iiend=ianz0
	do i=ianz0,ianz-2
	iiend=iiend+1
	iistart=jmax0(ianz0-1,iiend-xi.ifm_amp+1)
	ianz_fit=iiend-iistart+1
	do j=iistart,iiend
	  x_fit(j-iistart+1)=x(j)
	  amp_fit(j-iistart+1)=amp(j)
	enddo
	call fitpar(ianz_fit,x_fit,amp_fit,iiord,coef)

	xx(1)=1.d0
	do j=1,iiord
	xx(j+1)=xx(j)*x(i+2)
	enddo

	amp_guess=0.d0
	do j=1,iiord+1
	amp_guess=amp_guess+coef(j)*xx(j)
	enddo

c	type*,amp_guess,amp(i+2)

	if(dabs(amp_guess-amp(i+2)).gt.
     &		dabs(amp_guess+amp(i+2)) )then
		amp(i+2)=-amp(i+2)
		pha(i+2)=pha(i+2)+cs.pi
	endif

c	type*,amp(i+2)

	enddo
	endif

c-------- go from ianz0 to the left 
	if(ianz0.ge.2)then
	iiend=ianz0
	do i=ianz0,3,-1
	iiend=iiend-1
	iistart=jmin0(ianz0+1,iiend+xi.ifm_amp-1)
	ianz_fit=iistart-iiend+1
	do j=iiend,iistart
	  x_fit(j-iiend+1)=x(j)
	  amp_fit(j-iiend+1)=amp(j)
	enddo

c	type*,' iiord ',iiord
c	do j=1,ianz_fit
c	  type*,j,x_fit(j),amp_fit(j)
c	enddo  	

	call fitpar(ianz_fit,x_fit,amp_fit,iiord,coef)

	xx(1)=1.d0
	do j=1,iiord
	xx(j+1)=xx(j)*x(i-2)
	enddo

	amp_guess=0.d0
	do j=1,iiord+1
	amp_guess=amp_guess+coef(j)*xx(j)
	enddo

c	type*,amp_guess,amp(i-2)

	if(dabs(amp_guess-amp(i-2)).gt.
     &		dabs(amp_guess+amp(i-2)) )then
		amp(i-2)=-amp(i-2)
		pha(i-2)=pha(i-2)+cs.pi
	endif

c	type*,amp(i-2)

	enddo
	endif
c----------------------------------
	endif	! (xi.iord_amp.lt.0)
c----------------------------------
	goto 9988

9990	continue
c----------------------------------
	if(xi.iord_amp.lt.0)then
c----------------------------------
	iiord=-xi.iord_amp

c-------- go from ianz0 to the right
	if(ianz0.le.ianz-2)then
	iiend=ianz0
	do i=ianz0,ianz-2
	iiend=iiend+1
	iistart=jmax0(ianz0-1,iiend-xi.ifm_amp+1)
	ianz_fit=iiend-iistart+1

	do j=iistart,iiend
	  x_fit(j-iistart+1)=x(j)
	  amp_fit(j-iistart+1)=amp(j)
	enddo
	call fitpar(ianz_fit,x_fit,amp_fit,iiord,coef)

        do j=iistart,iiend
          x_fit(j-iistart+1)=x(j)
          amp_fit(j-iistart+1)=amp(j)
        enddo
        amp_fit(iiend-iistart+1)=-amp(iiend)
        call fitpar(ianz_fit,x_fit,amp_fit,iiord,coef1)
 
	xx(1)=1.d0
	do j=1,iiord
	xx(j+1)=xx(j)*x(i+2)
	enddo

	if(dabs(coef1(3)).lt.dabs(coef(3)))then
	  amp_guess=0.d0
	  do j=1,iiord+1
	  amp_guess=amp_guess+coef1(j)*xx(j)
	  enddo
	else
	  amp_guess=0.d0
	  do j=1,iiord+1
	  amp_guess=amp_guess+coef(j)*xx(j)
	  enddo
	endif

	if(dabs(amp_guess-amp(i+2)).gt.
     &		dabs(amp_guess+amp(i+2)) )then
		amp(i+2)=-amp(i+2)
		pha(i+2)=pha(i+2)+cs.pi
	endif

	enddo
	endif

c-------- go from ianz0 to the left 
	if(ianz0.ge.2)then
	iiend=ianz0
	do i=ianz0,3,-1
	iiend=iiend-1
	iistart=jmin0(ianz0+1,iiend+xi.ifm_amp-1)
	ianz_fit=iistart-iiend+1

	do j=iiend,iistart
	  x_fit(j-iiend+1)=x(j)
	  amp_fit(j-iiend+1)=amp(j)
	enddo
	call fitpar(ianz_fit,x_fit,amp_fit,iiord,coef)

        do j=iiend,iistart
          x_fit(j-iiend+1)=x(j)
          amp_fit(j-iiend+1)=amp(j)
        enddo
        amp_fit(1)=-amp(iiend)
        call fitpar(ianz_fit,x_fit,amp_fit,iiord,coef1)

	xx(1)=1.d0
	do j=1,iiord
	xx(j+1)=xx(j)*x(i-2)
	enddo

	if(dabs(coef1(3)).lt.dabs(coef(3)))then
	  amp_guess=0.d0
	  do j=1,iiord+1
	  amp_guess=amp_guess+coef1(j)*xx(j)
	  enddo
	else
	  amp_guess=0.d0
	  do j=1,iiord+1
	  amp_guess=amp_guess+coef(j)*xx(j)
	  enddo
	endif

	if(dabs(amp_guess-amp(i-2)).gt.
     &		dabs(amp_guess+amp(i-2)) )then
		amp(i-2)=-amp(i-2)
		pha(i-2)=pha(i-2)+cs.pi
	endif

	enddo
	endif

c----------------------------------
	endif	! (xi.iord_amp.lt.0)
c----------------------------------
	goto 9988

9988	continue
c--------------- end amplitude correction -------------------

c--------- start phase correction
c----------------------------------
	if(xi.iord_pha.ge.0)then
c----------------------------------
	pha_new(ianz0-1)=pha(ianz0-1)
	pha_new(ianz0)=pha(ianz0)
	pha_new(ianz0+1)=pha(ianz0+1)
	do i=ianz0,ianz-2
	x1=x(i-1)
	x2=x(i)
	x3=x(i+1)
	y1=pha_new(i-1)
	y2=pha_new(i)
	y3=pha_new(i+1)

	if(xi.iord_pha.eq.0)then
		a=0.d0
		b=0.d0
		c=y3
	endif

	if(xi.iord_pha.eq.1)then
		a=0.d0
		a_old=0.d0
		b=(y2-y3)/(x2-x3)
		c=y2-b*x2
	endif

	if(xi.iord_pha.eq.2)then
	r=x2*x2-x1*x1
	s=x3*x3-x1*x1

	b=( (y3-y1)/s - (y2-y1)/r ) / ( (x3-x1)/s - (x2-x1)/r )
	a=(y3-y1-b*(x3-x1)) / s
	c=y1-a*x1*x1-b*x1
	endif

	pha_guess=a*x(i+2)*x(i+2)+
     &			b*x(i+2)+c

c	type*,x1,x2,y1,y2,a,b,c,pha_guess

c	type*,'--------'
c	type*,x1,y1
c	type*,x2,y2
c	type*,x3,y3
c	type*,x(i+3),pha_guess

	if(i.gt.ianz0)then
	  if(dabs(b_old).gt.small_loc)then
		if( (b*b_old.lt.0.0).and.(xi.iphase_curv.eq.0) )then
		    pha_guess=a_old*x(i+2)*x(i+2)+
     &				b_old*x(i+2)+c_old
		    iii=1
		else
c		type*,i,b,b_old
		test=dabs(b/b_old)
c		type*,test
		if(test.gt.1.0)then
		  if(test.gt.xi.phase_change_1)then
		    pha_guess=a_old*x(i+2)*x(i+2)+
     &				b_old*x(i+2)+c_old
		    iii=1
		  endif
		endif
		if(test.lt.1.0)then
		  if(test.lt.xi.phase_change_2)then
		    pha_guess=a_old*x(i+2)*x(i+2)+
     &				b_old*x(i+2)+c_old
		    iii=1
		  endif
		endif
		endif
	  endif
	endif

c	type*,i,x(i+2),pha_guess

	if(iii.eq.1)then
		iii=0
		else
		a_old=a
		b_old=b
		c_old=c
	endif

	xipi=pha_guess/(2.d0*cs.pi)
	ipi=xipi
	ipi=ipi-5
	pha_new(i+2)=pha(i+2)+dflotj(ipi)*2.d0*cs.pi
	dd=dabs(pha_guess-pha_new(i+2))
10	continue
c	type*,pha_new(i+2)
	pha_new(i+2)=pha_new(i+2)+2.d0*cs.pi
	dd1=dabs(pha_new(i+2)-pha_guess) 
c	type*,i,pha_new(i+2),pha_guess
c	type*,dd,dd1
	if(dabs(dabs(dd1)-dabs(dd)).gt.small_loc1)then
	if(dabs(dd1).lt.dabs(dd))then
		dd=dd1
		goto 10
	endif	
	endif
	pha_new(i+2)=pha_new(i+2)-2.d0*cs.pi

c	type*,pha_new(i+2)
c	type*,' ---'

	if(xi.iphase_pi2.eq.1)then
	if(dabs(pha_guess-(pha_new(i+2)-cs.pi)).lt.dd)then
		pha_new(i+2)=pha_new(i+2)-cs.pi
		amp(i+2)=-amp(i+2)
	endif
	if(dabs(pha_guess-(pha_new(i+2)+cs.pi)).lt.dd)then
		pha_new(i+2)=pha_new(i+2)+cs.pi
		amp(i+2)=-amp(i+2)
	endif
	endif

c	type*,x(i+2),pha_guess,pha_new(i+2)

	enddo

c	goto 1236
c---------- zeroth order correction
c	removes 2*pi jumps at low k-values
c	introduces 2*pi jumps at high k-values, 
c	however this does not hurt that much
c---------------------------------------------------
	if((ianz0.gt.1).and.(ianz0.lt.ianz))then
        ipi=0
        do i=ianz0+1,ianz
        pha_guess=pha_new(i-1)
        pha0=pha_new(i)+dflotj(ipi)*2.d0*cs.pi
        if(dabs(pha_guess-pha0-2.d0*cs.pi).lt.
     &     dabs(pha_guess-pha0))ipi=ipi+1
        if(dabs(pha_guess-pha0+2.d0*cs.pi).lt.
     &     dabs(pha_guess-pha0))ipi=ipi-1
        pha_new(i)=pha_new(i)+dflotj(ipi)*2.d0*cs.pi
        enddo
	endif

1236	continue
c--------- first order is unstable
c	ipi=0
c	do i=ianz0+2,ianz
c	x1=x(i-2)
c	x2=x(i-1)
c	y1=pha_new(i-2)
c	y2=pha_new(i-1)
c	b=(y1-y2)/(x1-x2)
c	c=y1-b*x1
c	pha_guess=b*x(i)+c
c	pha0=pha_new(i)+dflotj(ipi)*2.d0*cs.pi
c	if(dabs(pha_guess-pha0-2.d0*cs.pi).lt.
c    &	   dabs(pha_guess-pha0))ipi=ipi+1
c	if(dabs(pha_guess-pha0+2.d0*cs.pi).lt.
c    &	   dabs(pha_guess-pha0))ipi=ipi-1
c	pha_new(i)=pha_new(i)+dflotj(ipi)*2.d0*cs.pi
c	enddo
	
c------------ opposite direction

	iii=0

	pha_new(ianz0-1)=pha(ianz0-1)
	pha_new(ianz0)=pha(ianz0)
	pha_new(ianz0+1)=pha(ianz0+1)
	do i=ianz0,3,-1
	x1=x(i-1)
	x2=x(i)
	x3=x(i+1)
	y1=pha_new(i-1)
	y2=pha_new(i)
	y3=pha_new(i+1)

	if(xi.iord_pha.eq.0)then
		a=0.d0
		b=0.d0
		c=y1
	endif

	if(xi.iord_pha.eq.1)then
		a=0.d0
		a_old=0.d0
		b=(y1-y2)/(x1-x2)
		c=y1-b*x1
	endif

	if(xi.iord_pha.eq.2)then
	r=x2*x2-x1*x1
	s=x3*x3-x1*x1

	b=( (y3-y1)/s - (y2-y1)/r ) / ( (x3-x1)/s - (x2-x1)/r )
	a=(y3-y1-b*(x3-x1)) / s
	c=y1-a*x1*x1-b*x1
	endif

	pha_guess=a*x(i-2)*x(i-2)+b*x(i-2)+c

	if(i.lt.ianz0)then
	  if(dabs(b_old).gt.small_loc)then
		if( (b*b_old.lt.0.0).and.(xi.iphase_curv.eq.0) )then
		    pha_guess=a_old*x(i-2)*x(i-2)+
     &				b_old*x(i-2)+c_old
		    iii=1
		else
c		type*,i,b,b_old
		test=dabs(b/b_old)
c		type*,test
		if(test.gt.1.0)then
		  if(test.gt.xi.phase_change_1)then
		    pha_guess=a_old*x(i-2)*x(i-2)+
     &				b_old*x(i-2)+c_old
		    iii=1
		  endif
		endif
		if(test.lt.1.0)then
		  if(test.lt.xi.phase_change_2)then
		    pha_guess=a_old*x(i-2)*x(i-2)+
     &				b_old*x(i-2)+c_old
		    iii=1
		  endif
		endif
		endif
	  endif
	endif
	if(iii.eq.1)then
		iii=0
		else
		a_old=a
		b_old=b
		c_old=c
	endif

	xipi=pha_guess/(2.d0*cs.pi)
	ipi=xipi
	ipi=ipi-5
	pha_new(i-2)=pha(i-2)+dflotj(ipi)*2.d0*cs.pi
	dd=dabs(pha_guess-pha_new(i-2))
11	continue
	pha_new(i-2)=pha_new(i-2)+2.d0*cs.pi

c	type*,i,x(i-2),pha_guess
c	type*,pha_new(i-2)

	dd1=dabs(pha_new(i-2)-pha_guess) 

	if(dabs(dabs(dd1)-dabs(dd)).gt.small_loc1)then
	if(dabs(dd1).lt.dabs(dd))then
		dd=dd1
		goto 11
	endif
	endif
	pha_new(i-2)=pha_new(i-2)-2.d0*cs.pi

c	type*,pha_new(i-2)

c	type*,' ----'

	if(xi.iphase_pi2.eq.1)then
	if(dabs(pha_guess-(pha_new(i-2)-cs.pi)).lt.dd)then
		pha_new(i-2)=pha_new(i-2)-cs.pi
		amp(i-2)=-amp(i-2)
	endif
	if(dabs(pha_guess-(pha_new(i-2)+cs.pi)).lt.dd)then
		pha_new(i-2)=pha_new(i-2)+cs.pi
		amp(i-2)=-amp(i-2)
	endif
	endif

	enddo

c	goto 1235
c---------- zeroth order correction
c	removes 2*pi jumps at low k-values
c	introduces 2*pi jumps at high k-values, 
c	however this does not hurt that much
c---------------------------------------------------
	if((ianz0.gt.1).and.(ianz0.lt.ianz))then
        ipi=0
        do i=ianz0-1,1,-1
        pha_guess=pha_new(i+1)
        pha0=pha_new(i)+dflotj(ipi)*2.d0*cs.pi
        if(dabs(pha_guess-pha0-2.d0*cs.pi).lt.
     &     dabs(pha_guess-pha0))ipi=ipi+1
        if(dabs(pha_guess-pha0+2.d0*cs.pi).lt.
     &     dabs(pha_guess-pha0))ipi=ipi-1
        pha_new(i)=pha_new(i)+dflotj(ipi)*2.d0*cs.pi
        enddo
	endif

1235	continue   
c--------- first order is unstable
c	ipi=0
c	do i=ianz0-2,1,-1
c	x1=x(i+1)
c	x2=x(i+2)
c	y1=pha_new(i+1)
c	y2=pha_new(i+2)
c	b=(y1-y2)/(x1-x2)
c	c=y1-b*x1
c	pha_guess=b*x(i)+c
c	pha0=pha_new(i)+dflotj(ipi)*2.d0*cs.pi
c	if(dabs(pha_guess-pha0-2.d0*cs.pi).lt.
c    &	   dabs(pha_guess-pha0))ipi=ipi+1
c	if(dabs(pha_guess-pha0+2.d0*cs.pi).lt.
c    &	   dabs(pha_guess-pha0))ipi=ipi-1
c	pha_new(i)=pha_new(i)+dflotj(ipi)*2.d0*cs.pi
c	enddo

	endif	! xi.iord_pha.ge.0

c----------------------------------
	if(xi.iord_pha.lt.0)then
c----------------------------------

	iiord=-xi.iord_pha

c-------- go from ianz0 to the right
	if(ianz0.le.ianz-2)then
	do i=ianz0-1,ianz0+1
	pha_new(i)=pha(i)
	enddo
	iiend=ianz0
	do i=ianz0,ianz-2
	iiend=iiend+1
	iistart=jmax0(ianz0-1,iiend-xi.ifm_pha+1)
	ianz_fit=iiend-iistart+1
	do j=iistart,iiend
	  x_fit(j-iistart+1)=x(j)
	  pha_fit(j-iistart+1)=pha_new(j)
	enddo
	call fitpar(ianz_fit,x_fit,pha_fit,iiord,coef)

	xx(1)=1.d0
	do j=1,iiord
	xx(j+1)=xx(j)*x(i+2)
	enddo

	pha_guess=0.d0
	do j=1,iiord+1
	pha_guess=pha_guess+coef(j)*xx(j)
	enddo

c	type*,pha_guess,pha(i+2)

c----------- now, correct phase
	xipi=pha_guess/(2.d0*cs.pi)
	ipi=xipi
	ipi=ipi-5
	pha_new(i+2)=pha(i+2)+dflotj(ipi)*2.d0*cs.pi
	dd=dabs(pha_guess-pha_new(i+2))
30	continue
c	type*,pha_new(i+2)
	pha_new(i+2)=pha_new(i+2)+2.d0*cs.pi
	dd1=dabs(pha_new(i+2)-pha_guess) 
c	type*,i,pha_new(i+2),pha_guess
c	type*,dd,dd1
	if(dabs(dabs(dd1)-dabs(dd)).gt.small_loc1)then
	if(dabs(dd1).lt.dabs(dd))then
		dd=dd1
		goto 30
	endif	
	endif
	pha_new(i+2)=pha_new(i+2)-2.d0*cs.pi

c	type*,pha_new(i+2)

	enddo
	endif

c-------- go from ianz0 to the left 

c	type*,' ianz0,x(ianz0) ',ianz0,x(ianz0)

	if(ianz0.ge.2)then
	iiend=ianz0
	do i=ianz0,3,-1
	iiend=iiend-1
	iistart=jmin0(ianz0+1,iiend+xi.ifm_pha-1)
	ianz_fit=iistart-iiend+1
	do j=iiend,iistart
	  x_fit(j-iiend+1)=x(j)
	  pha_fit(j-iiend+1)=pha_new(j)
	enddo

c	type*,' iiord ',iiord
c	do j=1,ianz_fit
c	  type*,j,x_fit(j),pha_fit(j)
c	enddo  	

	call fitpar(ianz_fit,x_fit,pha_fit,iiord,coef)

	xx(1)=1.d0
	do j=1,iiord
	xx(j+1)=xx(j)*x(i-2)
	enddo

	pha_guess=0.d0
	do j=1,iiord+1
	pha_guess=pha_guess+coef(j)*xx(j)
	enddo

c	type*,pha_guess,pha(i-2)

c----------- now,correct phase
	xipi=pha_guess/(2.d0*cs.pi)
	ipi=xipi
	ipi=ipi-5
	pha_new(i-2)=pha(i-2)+dflotj(ipi)*2.d0*cs.pi
	dd=dabs(pha_guess-pha_new(i-2))
31	continue
c	type*,pha_new(i-2)
	pha_new(i-2)=pha_new(i-2)+2.d0*cs.pi
	dd1=dabs(pha_new(i-2)-pha_guess) 
c	type*,i,pha_new(i-2),pha_guess
c	type*,dd,dd1
	if(dabs(dabs(dd1)-dabs(dd)).gt.small_loc1)then
	if(dabs(dd1).lt.dabs(dd))then
		dd=dd1
		goto 31
	endif	
	endif
	pha_new(i-2)=pha_new(i-2)-2.d0*cs.pi

c	type*,pha_new(i-2)

	enddo
	endif

	endif	! xi.iord_pha.lt.0

	do i=1,ianz
	pha(i)=pha_new(i)
	enddo

	return
	end

c******************************************************************
	subroutine fitpar(ianz,x,y,iiord,c)
c******************************************************************
c------------------------------------------------------
c
c	Routine fits a polynomial of order iiord through
c	a set of data points
c
c------------------------------------------------------

	implicit real*8 (a-h,o-z)

	parameter(number=210)

	dimension work(number)
	dimension x(1024),y(1024)
	dimension xx(number,1024)
	dimension b(number),a(number,number)
	dimension c(1024)

c	type*,'---------------'
c	type*,' ianz,iiord ',ianz,iiord
c	do i=1,ianz
c	type*,x(i),y(i)
c	enddo
c
c-------- check order and number of data points
	if((ianz-1).lt.iiord)then
		type*,'******** error in fitpar *************'
		type*,'******** not enough data points ******'	
		stop
	endif

c-------- products of x(i)
	do j=1,ianz
		xx(1,j)=1.d0
		do i=1,2*iiord
			xx(i+1,j)=xx(i,j)*x(j)
		enddo
	enddo			

c-------- derivation of vector B 
	do i=1,iiord+1
		b(i)=0.d0
		do j=1,ianz
			b(i)=b(i)+y(j)*xx(i,j)
		enddo
	enddo

c-------- derivation of matrix A 
	do i=iiord+1,1,-1
		do j=iiord+1,1,-1
			a(i,j)=0.d0
			do k=1,ianz
				a(i,j)=a(i,j)+xx(i+j-1,k)
			enddo
		enddo
	enddo

	lnum=iiord+1
	call deqn(lnum,a,number,work,ifail1,1,b)
 	if(ifail1.ne.0)then
		type*,'******* Fehler in FITPAR ******'
		type*,'******* Fehler beim Loesen' 
		type*,'******* des Gleichungssystems ******'
	endif

	do i=1,iiord+1
		c(i)=b(i)
	enddo

	return
	end


c******************************************************************
	subroutine simpson(cs,ifl,xi,xir,ianz,dyz,fyz,fyza,fyzp,xint,
     &                      xinta,xintp)
c******************************************************************
c
c	''Turbo Version'' 
c
c	Unter folgenden Annahmen kann das Integral analytisch 
c	geloest werden:
c		a) Zwischen zwei benachbarten Stuetzpunkten aendert
c		   sich die Amplitude nur wenig, sodass eine Beschreibung
c		   mit einem Polynom 2. Ordnung moeglich ist (Ent-
c		   wicklung nach dem Winkel in der Bildebene).		   
c		b) Zwischen zwei benachbarten Stuetzpunkten ist die
c		   Phase linear zum Winkel.
c	Problematisch sind Unstetigkeiten in der Phase (Spruenge von 2 pi
c	und pi). Allerdings kann dieses Problem folgendermassen geloest 
c	werden:
c
c	i) Integration in y-Richtung
c	   Berechnung der optischen Phase ohne Anwendung der 
c	   modulo-Funktion. Dies ist fuer die Quellberechnung und 
c	   die optische Weglaengenberechnung moeglich und fuer
c	   die Dipolquelle bereits implementiert.
c
c	ii) Integration in z-Richtung
c	    Die Phase weist Spruenge von 2 pi und pi auf, die zunaechst
c	    mit der Routine PHACOR entfernt werden muessen. Danach
c	    erfolgt ebenfalls eine analytische Integration 
c	    (modus ifl.ispline = -1). Alternativ kann fuer die z-Integration 
c	    auch auf ifl.ispline =0 umgeschaltet werden (modus ifl.ispline = -2).
c	    ifl.ispline = 0 ist eine simple SIMPSON-Integration.
c
c-----------------------------------------------------------------------

	implicit real*8(a-h,o-z)

	include 'phase_struct_10.for'
        record /constants/ cs
	record /statistics/ st
        record /integration_results/ xir
        record /integration/ xi
        record /control_flags/ ifl

	complex*16 xint,fyz(4096),f1,f2,xxx(4096)

	dimension dyz(4096)
	dimension x(4096),y(4096)
	dimension fyza(4096),fyzp(4096)

	xir.nsimp=xir.nsimp+1	! number of calls of simpson

c---------------------------------------------------------
	if(ifl.ispline.lt.0)then
c---------------------------------------------------------

	small_loc=1.0d-15
	xint=0.d0

	if(ianz.ge.2)then

	x(1)=0.d0
	do i=2,ianz
	x(i)=x(i-1)+dyz(i-1)
	enddo

	ph2=fyzp(1)
	dsinph2=dsin(ph2)
	dcosph2=dcos(ph2)
	x2=x(1)
	x3=x(2)
	y2=fyza(1)
	y3=fyza(2)

	do i=1,ianz-1
	ph1=ph2		! fyzp(i)
	ph2=fyzp(i+1)
	dsinph1=dsinph2
	dcosph1=dcosph2
	dsinph2=dsin(ph2)
	dcosph2=dcos(ph2)
	xk_loc=(ph2-ph1)/dyz(i)
	x1=x2		! x(i)
	x2=x3		! x(i+1)
	y1=y2		! fyza(i)
	y2=y3		! fyza(i+1)
	if(i.lt.ianz-1)then
		x3=x(i+2)
		y3=fyza(i+2)
	      else
		x3=x(i-1)
		y3=fyza(i-1)
	endif

c------------ order of expansion of amplitude ---------

	if(xi.iordap.eq.0)then
		a=0.d0
		b=0.d0
		c=0.5*(y1+y2)
	endif

	if(xi.iordap.eq.1)then
		a=0.d0
		b=(y1-y2)/(x1-x2)
		c=y1-b*x1
	endif

	if(xi.iordap.eq.2)then
	r=x2*x2-x1*x1
	s=x3*x3-x1*x1

	b=( (y3-y1)/s - (y2-y1)/r ) / ( (x3-x1)/s - (x2-x1)/r )
	a=(y3-y1-b*(x3-x1)) / s
	c=y1-a*x1*x1-b*x1

	endif

c-------------------------------------------------

	if(dabs(xk_loc).gt.xi.dphi_min)then
		a=a/xk_loc**3
		b=b/xk_loc**2
	      else
		a=0.d0
		b=0.d0
		c=0.5*(y1+y2)
	endif

	e1=x1*xk_loc
	e11=e1*e1
	e2=x2*xk_loc
	e22=e2*e2

	partre=c 
	if(dabs(xk_loc).ge.xi.dphi_min)then
 		partre=partre*((dsinph2 - dsinph1)/xk_loc)
		else
		partre=partre*dcosph1*dyz(i)
	endif
	if(xi.iordap.gt.0)partre = partre +
     &  	b * ( (dcosph2+dsinph2*e2) 
     &		- (dcosph1+dsinph1*e1) ) 

	if(xi.iordap.gt.1)partre = partre +
     &		a * ( (2.0*dcosph2*e2+dsinph2*e22-
     &          2.0*dsinph2)  
     &		- (2.0*dcosph1*e1+dsinph1*e11-
     &          2.0*dsinph1) ) 

	partim=c 
	if(dabs(xk_loc).ge.xi.dphi_min)then
		partim=partim*( (-dcosph2+dcosph1 )/xk_loc )
		else
c		partim=0.d0
		partim=partim*dsinph1*dyz(i)
	endif	
	if(xi.iordap.gt.0)partim = partim +
     &		b * ( (-dcosph2*e2+dsinph2) -
     &		(-dcosph1*e1+dsinph1) ) 

	if(xi.iordap.gt.1)partim = partim +
     &		a * ( (-dcosph2*e22+2.0*dcosph2+
     &		2.0*dsinph2*e2)  
     &		- (-dcosph1*e11+2.0*dcosph1+
     &		2.0*dsinph1*e1) )  

	xint=xint+cs.sqrtm1*(partre+cs.sqrtm1*partim)

	xxx(i)=xint

	enddo

	endif
c------------------------
	xinta=cdabs(xint)
	if(xinta.gt.small_loc)then
		xintp=dimag(cdlog(xint/xinta))
	  else
		xintp=0.d0
	endif

	endif

c----------- output

	do ii=1,xir.iisimp
	if(xir.isimp(ii).eq.xir.nsimp)then

	xx=0.
	do i=1,ianz-1
	xx=xx+dyz(i)
	xir.sintre(ii,1,i)=xx
	xir.sintre(ii,2,i)=dreal(xxx(i))
	enddo
	xir.isintre(ii)=ianz-1

	xx=0.
	do i=1,ianz-1
	xx=xx+dyz(i)
	xir.sintim(ii,1,i)=xx
	xir.sintim(ii,2,i)=dimag(xxx(i))
	enddo
	xir.isintim(ii)=ianz-1

	xx=0.
	do i=1,ianz
	xir.simpa(ii,1,i)=xx
	xir.simpa(ii,2,i)=fyza(i)
	xx=xx+dyz(i)
	enddo
	xir.isimpa(ii)=ianz

	xx=0.
	do i=1,ianz
	xir.simpp(ii,1,i)=xx
	xir.simpp(ii,2,i)=fyzp(i)
	xx=xx+dyz(i)
	enddo
	xir.isimpp(ii)=ianz

	endif
	enddo

c---------------------------------------------------------
	if(ifl.ispline.eq.0)then
c---------------------------------------------------------

	xint=0.
	if(ianz.ge.2)then
	do i=1,ianz-1
	xint=xint+0.5*dyz(i)*(fyz(i+1)+fyz(i))
	xxx(i)=xint
	enddo
	endif

c----------- output
	do ii=1,xir.iisimp
	if(xir.isimp(ii).eq.xir.nsimp)then

	xx=0.
	do i=1,ianz-1
	xx=xx+dyz(i)
	xir.sintre(ii,1,i)=xx
	xir.sintre(ii,2,i)=dreal(xxx(i))*1.0d10
	enddo
	xir.isintre(ii)=ianz-1

	xx=0.
	do i=1,ianz-1
	xx=xx+dyz(i)
	xir.sintim(ii,1,i)=xx
	xir.sintim(ii,2,i)=dimag(xxx(i))*1.0d10
	enddo
	xir.isintim(ii)=ianz-1

	xx=0.
	do i=1,ianz
	xir.simpre(ii,1,i)=xx
	xir.simpre(ii,2,i)=dreal(fyz(i))*1.0d10
	xx=xx+dyz(i)
	enddo
	xir.isimpre(ii)=ianz

	xx=0.
	do i=1,ianz
	xir.simpim(ii,1,i)=xx
	xir.simpim(ii,2,i)=dimag(fyz(i))*1.0d10
	xx=xx+dyz(i)
	enddo
	xir.isimpim(ii)=ianz

	endif
	enddo
c-------------------

	endif

c------------------------------------------------------------

	if(ifl.ispline.eq.1)then

	x(i)=0.
	do i=2,ianz
	x(i)=x(i-1)+dyz(i)
	y(i)=dreal(fyz(i))
	enddo
	call UTIL_SPLINE_INTEGRAL(X,Y,ianz,xintre)	

	do i=2,ianz
	y(i)=dimag(fyz(i))
	enddo
	call UTIL_SPLINE_INTEGRAL(X,Y,ianz,xintim)	

	xint=xintre+cs.sqrtm1*xintim

c----------- output
	do ii=1,xir.iisimp
	if(xir.isimp(ii).eq.xir.nsimp)then

	xx=0.
	do i=1,ianz
	xir.simpre(ii,1,i)=xx
	xir.simpre(ii,2,i)=dreal(fyz(i))
	xx=xx+dyz(i)
	enddo
	xir.isimpre(ii)=ianz

	xx=0.
	do i=1,ianz
	xir.simpim(ii,1,i)=xx
	xir.simpim(ii,2,i)=dimag(fyz(i))
	xx=xx+dyz(i)
	enddo
	xir.isimpim(ii)=ianz

	endif
	enddo
c----------------

	endif

	return
	end
	

c-------------------------------------------------------------

c*****************************************************************
      SUBROUTINE UTIL_SPLINE_INTEGRAL(X,Y,N,RESULT)
c*****************************************************************
c	Author: Michael Scheer
c-----------------------------------------------------------------
C---  CALCULATES INTERGRAL OF Y(X) VIA SPLINES
c-----------------------------------------------------------------

      IMPLICIT real*8(a-h,o-z)

      dimension X(4096),Y(4096),COEF(4096)

C---  SPLINE-COEFFICIENTS

      CALL UTIL_SPLINE_COEF(X,Y,N,0.,0.,COEF)

C--- INTEGRATION

      RESULT=0.0
      DO I=1,N-1
      RESULT=RESULT
     &          +(X(I+1)-X(I))*0.5D0
     &          *(Y(I)+Y(I+1))
      RESULT=RESULT
     &          -(X(I+1)-X(I))**3/24.D0
     &          *(COEF(I)+COEF(I+1))
      ENDDO

      RETURN
      END

c*******************************************************************
      SUBROUTINE UTIL_SPLINE_COEF(X,Y,N,YP1,YPN,Y2)
c*******************************************************************
c	Author: Michael Scheer
c------------------------------------------------------------------
C--- CALCULATES SPLINE COEFFICIENTS
C--   INPUT:
C-       N: NUMBER OF X,Y-VALUES
C-       X: ARRAY OF X-VALUES
C-       Y: ARRAY OF Y-VALUES
C-       YP1:  SECOND DERIVATIVE AT FIRST X-VALUE
C-       YPN:  SECOND DERIVATIVE AT LAST X-VALUE
C--   OUPUT:
C-       Y2:   SPLINE-COEFFICIENTS
C--   WORKINGSPACE: AA(N),BB(N),CC(N),C(N)
c------------------------------------------------------------------

      IMPLICIT real*8(a-h,o-z)

      dimension X(4096),Y(4096),Y2(4096),
     &        AA(4096),BB(4096),CC(4096),C(4096)

      Y2(1)=YP1
      Y2(N)=YPN

      IF (N.LT.3) RETURN

      C(1)=YP1
      C(N)=YPN

      BB(1)=1.D0
      CC(1)=0.D0
      CC(N)=1.D0

      DO J=2,N-1
          AA(J)=(X(J  )-X(J-1))/6.D0
          BB(J)=(X(J+1)-X(J-1))/3.D0
          CC(J)=(X(J+1)-X(J  ))/6.D0
          C(J)=(Y(J+1)-Y(J  ))/(X(J+1)-X(J  ))
     &          -(Y(J  )-Y(J-1))/(X(J  )-X(J-1))
      ENDDO !J

      DO J=2,N-1

          BB(J)=BB(J)-AA(J)*CC(J-1)
           C(J)= C(J)-AA(J)* C(J-1)
          AA(J)=AA(J)-AA(J)*BB(J-1)

          CC(J)=CC(J)/BB(J)
           C(J)= C(J)/BB(J)
          BB(J)=1.D0

      ENDDO !J

      DO J=N-1,2,-1
         Y2(J)=C(J)-CC(J)*Y2(J+1)
      ENDDO

      RETURN
      END



