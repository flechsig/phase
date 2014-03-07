c File      : /home/pss060/sls/flechsig/phase/src/phase/phase_integration_11.for
c Date      : <28 Oct 99 09:17:59 flechsig> 
c Time-stamp: <20 Dec 99 15:29:36 flechsig> 
c Author    : Flechsig Uwe OVGA/203a 4535, flechsig@psi.ch
c 
c 28.10.99 Die Versionsnummern im Fortran File werden nur noch
c in der urspruenglichen Source beibehalten, es werden mit namen ohne
c Versionsnummer angelegt. Im Makefile und bei includes sollten die
c namen ohen Versionsnummer benutzt werden. Im Source Release tauchen die 
c Filenamen ohen Versionsnummer auf!
c
c Aenderungen an der Version von JB:
c   Versionsnummern (_11) bei includes entfernt
c   adaptive_int geloescht
c                                                       UF
c*********************************************************
c	22.4.1999
c	ACHTUNG:
c	Datentransfer ueber Common-Block SIMPS1 zum Hauptprogramm.
c	Das muss spaeter in PHASE_STRUCT_11.FOR integriert werden
c	(in xir. etc).
c	Datenausgabe auf File im Hauptprogramm.
c
c       erledigt simps1 als record si1 in integration_results integriert
c       UF 21.12.99 und entsprechende structuren eingefuegt
c
c	gleichzeitig sollten distfocy und distfocz implementiert werden
c	(in xi. etc).
c
c	adaptive integration rausschmeissen
c	nur eine Iteration
c	falls spaeter adaptive Integration implementiert werden soll,
c	muss dies auf andere Weise als bisher geschehen.
c
c*********************************************************
 
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
	complex*16 fyey(4096),fyez(4096)

	dimension a(0:5,0:5)

	include 'phase_struct.for'
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
c 17.17.99 UF itery0 wurde gestrichen	itery=xi.itery0
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

	include 'phase_struct.for'
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
	subroutine phacor(cs,ra,xi,xir,dx,amp,pha,ianz)
c***********************************************************
c
c	PHACOR funktioniert folgendermassen:
c
c	1. Auffindung der Bereiche quasi stationaerer Phase:
c	----------------------------------------------------
c
c	Es gibt zwei Moeglichkeiten
c
c	a) explizite Vorgabe des Punktes der stationaeren Phase:
c	   ianz0_cal   = 0
c	   ianz0_fixed = Pinkt stationaerer Phase 
c
c	b) Berechnung des Punktes der stationaeren Phase:
c	   ianz0_cal   = 1
c	   ianz0_fixed: keine Bedeutung
c
c	   Zunaechst werden alle Bereiche quasistationaerer Phase gesucht.
c	   Die Bereiche zeichnen sich dadurch aus, dass sich die 
c	   extrapolierte (Polynomfit 2. Ordnung) und die tatsaechliche 
c	   Phase um weniger als 
c	   
c		D12_MAX 
c
c	   unterscheiden. Die Mitte des groessten dieser Bereiche ist
c
c		IANZ0
c
c	   Die Differenzen von extrapolierter und 
c	   tatsaechlicher Phase werden, falls
c
c		ID12=1
c
c	   ist, auf den File D12.DAT geschrieben. 
c	   Vom Punkt IANZ0 ausgehend werden Amplitude und Phase 
c	   rekonstruiert.
c
c	   Es kann mehrere Bereiche quasi stationaerer Phase
c	   geben (z.B. im Coma eines Toroidspiegels). Daher
c	   muss noch die Option fur mehere ianz0 implementiert werden.
c
c	2. Korrektur der Amplituden:
c	----------------------------
c	a) IAMP_SMOOTH = 0 keine Amplitudenkorrektur (default)
c	b) IAMP_SMOOTH = 1 Amplitudenkorrektur 
c
c	Ausgehend vom Zentrum des Bereiches quasi stationaerer Phase IANZ0
c	wird nach rechts und links hin korrigiert. Dies geschieht so: 
c	Durch 3 oder mehr Punkte wird ein Polynom der Ordnung 
c	
c	XI.IORD_AMP 
c
c	gelegt. 
c
c	XI.IFM_AMP 
c
c	gibt die Zahl der zu verwendenden Amplitudenwerte an.
c
c	Aus dem Polynom wird ein Schaetzwert fuer die naechste Amplitude 
c	extrapoliert. Haben geschaetzte und tatsaechliche Amplitude unter-
c	schiedliches Vorzeichen, so wird die tatsaechliche Amplitude mit 
c	dem Faktor -1 multipliziert und die zugehoerige Phase um pi erhoeht.
c
c
c	3. Korrektur der Phasen:
c	------------------------
c	Ausgehend vom Zentrum des Bereiches quasi stationaerer Phase IANZ0
c	wird nach rechts und links hin korrigiert. Dies geschieht so: 
c	Durch 3 oder mehr Punkte wird ein Polynom der Ordnung 
c	
c	XI.IORD_PHA
c
c	gelegt. XI.IFM_PHA gibt die Zahl der zu verwendenden
c	Phasen an.
c
c	Aus dem Polynom wird ein Schaetzwert fuer die naechste Phase
c	extrapoliert. Zur tatsaechlichen Phase werden so oft 2 pi bzw. 
c	- 2pi addiert, bis die Differenz zum Schaetzwert minimal wird.
c	Die Phase weist in einigen Bereichen Rippel auf, die leicht die
c	ganze Korrektur in den Wald laufen lassen. Dies kann durch 
c	Erhoehung der Zahl XI.IFM_PHA vermieden werden.
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

	include 'phase_struct.for'
        record /rayst/ ra
        record /constants/ cs
	record /statistics/ st
	record /integration_results/ xir
	record /integration/ xi
        record /control_flags/ ifl

c	common/simps1/fya1(501),fyp1(501),fza1(501),fzp1(501),
c     &                fya2(501),fyp2(501),fza2(501),fzp2(501),
c     &		z1,z2,
c     &		tya(301,301),tza(301,301),
c     &		typ(301,301),tzp(301,301),
c     &		ianz0_save(301,301),
c     &		iiheigh,iiwidth,
c     &		jmult

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

c111	format(3(3x,d12.5))
c	open(unit=10,name='t0_phacor.dat',type='new')
c	do i=1,ianz
c	write(10,*)x(i),amp(i),pha(i)
c	enddo
c	close(10)

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

	d12(1)=99.d0
	d12(2)=99.d0
	d12(3)=99.d0
	d12(ianz)=99.d0
	d12(ianz-1)=99.d0
	d12(ianz-2)=99.d0

	do i=4,ianz-3
	d12(i)=dsqrt(dabs(d1(i)*d2(i)))
	if(d12(i).eq.0.d0)d12(i)=99
	enddo

c------ now, search ianz0

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

	iiregion=0
	if(iregion.gt.0)then
	do i=1,iregion
	ii=iend(i)-istart(i)+1
	if(ii.gt.iimax)then
		iimax=ii
		iiregion=i
	endif
	enddo

	ianz0=(istart(iiregion)+iend(iiregion))/2

	endif

	if(iiregion.eq.0)then
	  type*,' can not find region of stationary phase'
	  type*,' increase number of grid points'
	  stop
	endif

	if((iend(iiregion)-istart(iiregion)+1).eq.2)then
	  type*,' region of stationary phase has only two points'
	  type*,' increase number of grid points'
	  stop
	endif

c------------- neu: 10.11.1996 -----
c	raauskommentiert am 26.4.1999
c	ianz0=ianz0-1
c-----------------------------------
c------------- neu 22.4.1999
        xir.si1.ianz0_save(ra.n2,ra.n1)=ianz0

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

	xx(1)=1.d0
	do j=1,iiord
	xx(j+1)=xx(j)*x(i-2)
	enddo

	amp_guess=0.d0
	do j=1,iiord+1
	amp_guess=amp_guess+coef(j)*xx(j)
	enddo

	if(dabs(amp_guess-amp(i-2)).gt.
     &		dabs(amp_guess+amp(i-2)) )then
		amp(i-2)=-amp(i-2)
		pha(i-2)=pha(i-2)+cs.pi
	endif

	enddo
	endif

9988	continue
c--------------- end amplitude correction -------------------

c----------------------------------
c--------- start phase correction
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

c----------- now, correct phase
	xipi=pha_guess/(2.d0*cs.pi)
	ipi=xipi
	ipi=ipi-5
	pha_new(i+2)=pha(i+2)+dflotj(ipi)*2.d0*cs.pi
	dd=dabs(pha_guess-pha_new(i+2))
30	continue
	pha_new(i+2)=pha_new(i+2)+2.d0*cs.pi
	dd1=dabs(pha_new(i+2)-pha_guess) 
	if(dabs(dabs(dd1)-dabs(dd)).gt.small_loc1)then
	if(dabs(dd1).lt.dabs(dd))then
		dd=dd1
		goto 30
	endif	
	endif
	pha_new(i+2)=pha_new(i+2)-2.d0*cs.pi

	enddo
	endif

c-------- go from ianz0 to the left 

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

	call fitpar(ianz_fit,x_fit,pha_fit,iiord,coef)

	xx(1)=1.d0
	do j=1,iiord
	xx(j+1)=xx(j)*x(i-2)
	enddo

	pha_guess=0.d0
	do j=1,iiord+1
	pha_guess=pha_guess+coef(j)*xx(j)
	enddo

c----------- now,correct phase
	xipi=pha_guess/(2.d0*cs.pi)
	ipi=xipi
	ipi=ipi-5
	pha_new(i-2)=pha(i-2)+dflotj(ipi)*2.d0*cs.pi
	dd=dabs(pha_guess-pha_new(i-2))
31	continue
	pha_new(i-2)=pha_new(i-2)+2.d0*cs.pi
	dd1=dabs(pha_new(i-2)-pha_guess) 
	if(dabs(dabs(dd1)-dabs(dd)).gt.small_loc1)then
	if(dabs(dd1).lt.dabs(dd))then
		dd=dd1
		goto 31
	endif	
	endif
	pha_new(i-2)=pha_new(i-2)-2.d0*cs.pi

	enddo
	endif

	do i=1,ianz
	pha(i)=pha_new(i)
	enddo

c	open(unit=10,name='t1_phacor.dat',type='new')
c	do i=1,ianz
c	write(10,*)x(i),amp(i),pha(i)
c	enddo
c	close(10)

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
c	Weitere Steuerparameter:
c
c	iordap_loc  = Ordnung der Polynomentwicklung der Amplitude (0,1,2)
c	xk_loc_min  = Falls sich die Phase zweier aufeinander folgender
c	              Punkte um weniger als xk_loc_min unterscheiden,
c		      wird die Integration mittels (y1+y2)/2 ausgefuehrt.
c-----------------------------------------------------------------------

	implicit real*8(a-h,o-z)

	include 'phase_struct.for'
        record /constants/ cs
	record /statistics/ st
        record /integration_results/ xir
        record /integration/ xi
        record /control_flags/ ifl

c	common/simps1/fya1(501),fyp1(501),fza1(501),fzp1(501),
c     &                fya2(501),fyp2(501),fza2(501),fzp2(501),
c     &		z1,z2,
c     &		tya(301,301),tza(301,301),
c     &		typ(301,301),tzp(301,301),
c     &		ianz0_save(301,301),
c     &		iiheigh,iiwidth,
c     &		jmult

	complex*16 xint,fyz(4096),f1,f2,xxx(4096)

	real*4 fyza_loc(4096),fyzp_loc(4096)
	real*4 x_loc(4096),dyz_loc(4096)
	real*4 xxr(4096),ffyza(4096),ffyzp(4096)

c	dimension fyza_loc(4096),fyzp_loc(4096)
c	dimension x_loc(4096),dyz_loc(4096)
c	dimension xxr(4096),ffyza(4096),ffyzp(4096)

	dimension partre_int(4096),partim_int(4096)

	dimension dyz(4096)
	dimension x(4096),y(4096)
	dimension fyza(4096),fyzp(4096)

	dphi_loc_min=1.e-2
	iordap_loc=2

	xir.nsimp=xir.nsimp+1	! number of calls of simpson

c---------------------------------------------------------
	if(ifl.ispline.lt.0)then
c---------------------------------------------------------

	if(ianz.ge.2)then
	small_loc=1.0d-15
	xint=0.d0

	x(1)=0.d0
	do i=2,ianz
	x(i)=x(i-1)+dyz(i)
	enddo

111	format(3(3x,d12.5))
c	open(unit=10,name='t0.dat',type='new')
c	do i=1,ianz
c	write(10,*)x(i),fyza(i),fyzp(i)
c	enddo
c	close(10)

c	fyza --> fyza_loc	
c	fyzp --> fyzp_loc	
c	ianz --> ianz_loc

	ianz_loc=ianz
	do i=1,ianz_loc
	dyz_loc(i)=dyz(i)
	x_loc(i)=x(i)
	fyza_loc(i)=fyza(i)
	fyzp_loc(i)=fyzp(i)
	enddo

	ph2=fyzp_loc(1)
	dsinph2=dsin(ph2)
	dcosph2=dcos(ph2)
	x2=x_loc(1)
	x3=x_loc(2)
	y2=fyza_loc(1)
	y3=fyza_loc(2)

	do i=1,ianz_loc-1
	ph1=ph2		
	ph2=fyzp_loc(i+1)
	dsinph1=dsinph2
	dcosph1=dcosph2
	dsinph2=dsin(ph2)
	dcosph2=dcos(ph2)
	xk_loc=(ph2-ph1)/dyz_loc(i)
	x1=x2		
	x2=x3		
	y1=y2		
	y2=y3		
	if(i.lt.ianz_loc-1)then
		x3=x_loc(i+2)
		y3=fyza_loc(i+2)
	      else
		x3=x_loc(i-1)
		y3=fyza_loc(i-1)
	endif

c------------ order of expansion of amplitude ---------

	if(iordap_loc.eq.0)then
		a=0.d0
		b=0.d0
		c=0.5*(y1+y2)
	endif

	if(iordap_loc.eq.1)then
		a=0.d0
		b=(y1-y2)/(x1-x2)
		c=y1-b*x1
	endif

	if(iordap_loc.eq.2)then
	r=x2*x2-x1*x1
	s=x3*x3-x1*x1

	b=( (y3-y1)/s - (y2-y1)/r ) / ( (x3-x1)/s - (x2-x1)/r )
	a=(y3-y1-b*(x3-x1)) / s
	c=y1-a*x1*x1-b*x1

	endif

c-------------------------------------------------

	if(dabs(ph2-ph1).gt.dphi_loc_min)then
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
	if(dabs(ph2-ph1).gt.dphi_loc_min)then
 		partre=partre*((dsinph2 - dsinph1)/xk_loc)
		else
		partre=partre*dcosph1*dyz_loc(i)
	endif
	if(iordap_loc.gt.0)partre = partre +
     &  	b * ( (dcosph2+dsinph2*e2) 
     &		- (dcosph1+dsinph1*e1) ) 

	if(iordap_loc.gt.1)partre = partre +
     &		a * ( (2.0*dcosph2*e2+dsinph2*e22-
     &          2.0*dsinph2)  
     &		- (2.0*dcosph1*e1+dsinph1*e11-
     &          2.0*dsinph1) ) 

	partim=c 
	if(dabs(ph2-ph1).gt.dphi_loc_min)then
		partim=partim*( (-dcosph2+dcosph1 )/xk_loc )
		else
		partim=partim*dsinph1*dyz_loc(i)
	endif	
	if(iordap_loc.gt.0)partim = partim +
     &		b * ( (-dcosph2*e2+dsinph2) -
     &		(-dcosph1*e1+dsinph1) ) 

	if(iordap_loc.gt.1)partim = partim +
     &		a * ( (-dcosph2*e22+2.0*dcosph2+
     &		2.0*dsinph2*e2)  
     &		- (-dcosph1*e11+2.0*dcosph1+
     &		2.0*dsinph1*e1) )  

	xint=xint+cs.sqrtm1*(partre+cs.sqrtm1*partim)

	partre_int(i)=partre
	partim_int(i)=partim

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
c	if(xir.nsimp.eq.2*xi.ianzy0+1)then

c	open(unit=10,name='t1.dat',type='new')
c	do i=1,ianz_loc
c	write(10,111)x_loc(i),fyza_loc(i),fyzp_loc(i)
c	enddo
c	close(10)
c
c	open(unit=10,name='t1_int.dat',type='new')
c	do i=1,ianz_loc
c	write(10,111)x_loc(i),partre_int(i),partim_int(i)
c	enddo
c	close(10)
c
c	endif

	do ii=1,xir.iisimp
	if(xir.isimp(ii).eq.xir.nsimp)then

	xx=0.
	do i=1,ianz_loc-1
	xx=xx+dyz_loc(i)
	xir.sintre(ii,1,i)=xx
	xir.sintre(ii,2,i)=dreal(xxx(i))
	enddo
	xir.isintre(ii)=ianz_loc-1

	xx=0.
	do i=1,ianz_loc-1
	xx=xx+dyz_loc(i)
	xir.sintim(ii,1,i)=xx
	xir.sintim(ii,2,i)=dimag(xxx(i))
	enddo
	xir.isintim(ii)=ianz_loc-1

	xx=0.
	do i=1,ianz_loc
	xir.simpa(ii,1,i)=xx
	xir.simpa(ii,2,i)=fyza_loc(i)
	xx=xx+dyz_loc(i)
	enddo
	xir.isimpa(ii)=ianz_loc

	xx=0.
	do i=1,ianz_loc
	xir.simpp(ii,1,i)=xx
	xir.simpp(ii,2,i)=fyzp_loc(i)
	xx=xx+dyz_loc(i)
	enddo
	xir.isimpp(ii)=ianz_loc

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



c------------------------------------------------------
	subroutine guess(m4,g,a,src,apr,cs,ra,ifl,xi,xir,st)
c------------------------------------------------------
c	variable grid generation for dyi and dzi:
c	- calculate tza, tzp for central ray: yi = zi = 0
c	  with ymin ymax zmin zmax ianz etc like in 
c	  parameter file
c	- befor first call of subroutine guess
c	  determine distfocy and distfocz:

c	  dyi_loc=dyi_loc+yi_loc/xi.distfocy
c	  dzi_loc=dzi_loc+zi_loc/xi.distfocz

c	- adapt grid in size and location accordingly
c	  and get new ymin, ymax, zmin, zmax
c	- grid for yi + dyi etc
c

        implicit real*8(a-h,o-z)
        implicit integer(i-n)
	include 'phase_struct.for'

	common/simps1/fya1(501),fyp1(501),fza1(501),fzp1(501),
     &                fya2(501),fyp2(501),fza2(501),fzp2(501),
     &		z1,z2,
     &		tya(301,301),tza(301,301),
     &		typ(301,301),tzp(301,301),
     &		ianz0_save(301,301),
     &		iiheigh,iiwidth,
     &		jmult

      common/xlengthc/xlen1cc(0:4,0:4,0:4,0:4),
     &                 xlen1c(0:4,0:4,0:4,0:4),
     &                 xlen1c_r(0:4,0:4),
     &                 xlen1c_rr(0:4),xlength1

      common/xlengthd/xlen2cc(0:4,0:4,0:4,0:4),
     &                 xlen2c(0:4,0:4,0:4,0:4),
     &                 xlen2c_r(0:4,0:4),
     &                 xlen2c_rr(0:4),xlength2

        common/map7/wc(0:4,0:4,0:4,0:4),
     &              xlc(0:4,0:4,0:4,0:4),
     &              ypc1(0:4,0:4,0:4,0:4),
     &              zpc1(0:4,0:4,0:4,0:4)

   	common/geometry/sina,cosa,sinb,cosb,
     &                  r,rp,xdens(0:4),xlam,idefl
        common/orders/iord,iexpand

        record /constants/ cs
        record /geometryst/ g
        record /rayst/ ra
        record /source_results/ sr
        record /integration_results/ xir
        record /control_flags/ ifl
        record /sources/ src
        record /integration/ xi
        record /source1/ so1
        record /source2/ so2
        record /source3/ so3
        record /source4/ so4
        record /source5/ so5
        record /source6/ so6
        record /apertures/ apr
        record /statistics/ st
        record /map4/ m4

	dimension dyi_loc(301),dzi_loc(301)

	small_loc=1.0d-15
c-----------------------------------------------------------------
	yi_loc=ra.ri.yi
	zi_loc=ra.ri.zi

	ianzy_loc=xi.ianzy0
	ianzz_loc=xi.ianzz0
        dyy=(xi.ymax-xi.ymin)/dflotj(ianzy_loc-1)
        dzz=(xi.zmax-xi.zmin)/dflotj(ianzz_loc-1)

	do ii=1,ianzy_loc
	do jj=1,ianzz_loc

	dyi_loc(ii)=xi.ymin+dflotj(ii-1)*dyy
	dzi_loc(jj)=xi.zmin+dflotj(jj-1)*dzz

	if(abs(xi.distfoc).gt.1.e-12)then
	dyi_loc(ii)=dyi_loc(ii)+yi_loc/xi.distfoc
	endif
	if(abs(xi.distfoc).gt.1.e-12)then
	dzi_loc(jj)=dzi_loc(jj)+zi_loc/xi.distfoc
	endif

        yp=0.d0
	zp=0.d0
        xxlength=0.d0

          var0=1.
        do i=0,iord
          var1=var0
        do j=0,iord-i
          var2=var1
        do k=0,iord-i-j
          var3=var2
        do l=0,iord-i-j-k

         yp=yp+ypc1(i,j,k,l)*var3
         zp=zp+zpc1(i,j,k,l)*var3
         xxlength=xxlength+
     &           (xlen1c(i,j,k,l)+xlen2c(i,j,k,l))*var3
          var3=var3*dzi_loc(jj)
        enddo
          var2=var2*dyi_loc(ii)
        enddo
          var1=var1*zi_loc
        enddo
          var0=var0*yi_loc
        enddo
    
	ra.rf.yp=yp
	ra.rf.zp=zp

c	type*,ii,jj,ra.rf.yp,ra.rf.zp

	call psdi(g,src,apr,cs,ifl,ra,sr)

	xir.si1.tya(ii,jj)=sr.eya
        xir.si1.tza(ii,jj)=sr.eza
        xir.si1.typ(ii,jj)=sr.eyp+xxlength/ra.xlam_test
        xir.si1.tzp(ii,jj)=sr.ezp+xxlength/ra.xlam_test

	if(dabs(xir.si1.tya(ii,jj)).lt.small_loc)xir.si1.typ(ii,jj)=0.d0
	if(dabs(xir.si1.tza(ii,jj)).lt.small_loc)xir.si1.tzp(ii,jj)=0.d0
 
	enddo
	enddo

	return
	end
c /home/pss060/sls/flechsig/phase/src/phase/phase_integration_11.for 


