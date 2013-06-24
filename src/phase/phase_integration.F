c$$$ File      : ~/phase/src/phase/phase_inegration.F
c$$$ Date      : <21 Nov 07 12:42:16 flechsig> 
c$$$ Time-stamp: <24 Jun 13 15:00:52 flechsig> 
c$$$ Author    : J.B. + modification Uwe Flechsig
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 
c
c phase_integration.F enthaelt folgende routinen:
c adaptive_int(): Hauptroutine- wird von pst.c oder pstf.F gerufen
c yyint()  : called from adaptive_int()
c fywert() : called from yyint()
c int_2d() : wird nicht genutzt??
c phacor() : called from adaptive_int()
c fitpar() : called from phacor()
c simpson(): called from adaptive_int()
c guess()  : called from adaptive_int()
c
c Aenderungen UF:
c (1) file umbenannt in phase_integration_12.for
c (2) versionsnummer bei den include files entfernt
c     in der development version werden links auf die files mit
c     versionsnummer im filenamen angelegt (nur unter UNIX)
c     im VMS production release gibt es nur noch eine Version!
c (3) SIMPS1 als record si1 in den record integration_results
c     eingefuegt, z.B. jmult ansprechen ueber xir.si1.jmult
c     common block geloescht, Aufrufe auf die Strukturvariaben gelenkt
c (4) Zeile 105 iterz0 auskommentiert (nicht mehr in der structur)  
c     Zeile 282 itery0 auskommentiert (nicht mehr in der structur)
c (5) stop durch return ersetzt 
c        if(iiregion.eq.0)then ...
c        if((iend(iiregion)-istart(iiregion)+1).eq.2)then ...
c
c (6) neuer Aufrufparameter s in adaptive_int und phacor 
c      wird zum Ruecksprung benutzt
c     siehe (5) und dort gesetzt siehe c UF 12.1.2000
c------------------------------------------------------------------
c von J.B. am 20.12.99
c Aenderungen von JB am 11.2.00
c UF habe xxsave eingefuehrt !!! pruefen
c*********************************************************
c UF 12.1.2000
c #define DEBUG1
      subroutine adaptive_int(m4,g,am,src,apr,cs,ra,
     &                       ifl,xi,xir,st,sp,blp)
c*********************************************************
c	22.4.1999
c	ACHTUNG:
c	Datentransfer ueber Common-Block SIMPS1 zum Hauptprogramm.
c	Das muss spaeter in PHASE_STRUCT_11.FOR integriert werden
c	(in xir. etc).
c	Datenausgabe auf File im Hauptprogramm.
c
c	adaptive integration rausschmeissen
c	nur eine Iteration
c	falls spaeter adaptive Integration implementiert we rden soll,
c	muss dies auf andere Weise als bisher geschehen.
cc
c	ACHTUNG. Umstellen auf 8 Ordnung Spiegelentwicklung
c	erfolgt ueber Link-Flag SEVEN_ORDER,
c	die Struktur m4 und die Variablen in der Struktur m4 
c	haben zwar noch alte Namen, haben aber gegebenenfalls 
c	hoehere Dimensionen
c	JB 20.5.2011
c
c	ifl.iord ist ueberall eingesetzt (keine Hartverdrahtung der Ordnung)
c	phacor nicht getestet
c	turbo simpson nicht getestet 
c	      JB 20.5.2011
c
c	Skalierungsfaktoren angepasst (neue Definition etc)
c	      JB 20.5.2011
c
c	dimension von a rausgeworfen, wird jetzt direkt in
c	phase_struct.h/F gesetzt
c	dimension pij..() in fywert ueberall hochgesetzt
c		  JB 20.5.2011
c
c	zu den vielen Parametern:
c	   ifl.ipath   = 0 ausser fuer Debug-Zwecke
c	   ifl.matrel  = 0 --------- " ------------
c	   ifl.iexpand = 0 --------- " ------------
c
c************************************************

	implicit real*8(a-h,o-z)
        integer blp
	complex*16 fwert,fwerty,fwertz,
     &      s1,s1eyre,s1eyim,s1ezre,s1ezim,
     &      s2,s2eyre,s2eyim,s2ezre,s2ezim,
     &      yzint,yzintey,yzintez
	complex*16 fzey(4096),fzez(4096)

#include '../phase/phase_struct.F'

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
	record /mirrortype/ am
	record /map4/ m4
        record /psimagest/ sp

	dimension densiyre(MAX_INTEGRATION_SIZE),densiyim(MAX_INTEGRATION_SIZE),
     &            densizre(MAX_INTEGRATION_SIZE),densizim(MAX_INTEGRATION_SIZE)

  	dimension z(4096),dz(4096)
	dimension fya(4096),fyp(4096)
	dimension fza(4096),fzp(4096)

#ifdef DEBUG1
        write(*,*),'adaptive_int called1'
        call debug_beamline_type_f(blp)
        write(*,*),'adaptive_int called2'
#endif

	if((xir.si1.iiheigh.eq.1).and.(xir.si1.iiwidth.eq.1).and.
     &		(ifl.ispline.lt.0))then
           write(*,*)'************************************'
           write(*,*)'call to guess removed U.F. 22.6.13  '
           write(*,*)'************************************'
c	call guess(m4,g,am,src,apr,cs,ra,ifl,xi,xir,st,blp)
	endif

1       format(d15.5)
        do i=0,ifl.iord
        do j=0,ifl.iord-i
        do k=0,ifl.iord-i-j
        do l=0,ifl.iord-i-j-k
c        write(6,*)i,j,k,l
c        write(6,1)m4.fdet1phc(i,j,k,l),m4.fdet1phca(i,j,k,l),m4.fdet1phcb(i,j,k,l)
c        write(6,*)i,j,k,l,m4.xlen1c(i,j,k,l),m4.xlen2c(i,j,k,l)
        enddo
        enddo
        enddo
        enddo

c*******************************************************
c	Erstellen eines reduzierten Konstantensatzes
c*******************************************************
 
	yi=ra.ri.yi
	zi=ra.ri.zi

#ifdef SEVEN_ORDER
        call subm17(m4.fdetc,yi,zi,m4.fdetrc)
        call subm17(m4.fdetphc,yi,zi,m4.fdetphrc)
        call subm17(m4.fdet1phc,yi,zi,m4.fdet1phrc)
        call subm17(m4.fdet1phca,yi,zi,m4.fdet1phrca)
        call subm17(m4.fdet1phcb,yi,zi,m4.fdet1phrcb)
        call subm17(m4.ypc1,yi,zi,m4.yprc1)
        call subm17(m4.zpc1,yi,zi,m4.zprc1)
        call subm17(m4.dypc,yi,zi,m4.dyprc)
        call subm17(m4.dzpc,yi,zi,m4.dzprc)
        call subm17(m4.ypc_ap,yi,zi,m4.ypc_ap_r)
        call subm17(m4.zpc_ap,yi,zi,m4.zpc_ap_r)
        call subm17(m4.xlen1c,yi,zi,m4.xlen1c_r)
        call subm17(m4.xlen2c,yi,zi,m4.xlen2c_r)
        call subm17(m4.wc,yi,zi,m4.wrc)
        call subm17(m4.xlc,yi,zi,m4.xlrc)
#else
        call subm1(m4.fdetc,yi,zi,m4.fdetrc)
        call subm1(m4.fdetphc,yi,zi,m4.fdetphrc)
        call subm1(m4.fdet1phc,yi,zi,m4.fdet1phrc)
        call subm1(m4.fdet1phca,yi,zi,m4.fdet1phrca)
        call subm1(m4.fdet1phcb,yi,zi,m4.fdet1phrcb)
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
#endif

c        write(6,*)'m4.fdet1phrcb(0,0),m4.fdet1phrcb(1,0)',m4.fdet1phrcb(0,0),m4.fdet1phrcb(1,0)
c------------------------------------------------------

	st.inumb(st.nn1,st.nn2)=0.d0
	fmax=0.d0

	ianzz=xi.ianzz0
c gestrichen UF 21.12.99 iterz=xi.iterz0

c*** Number of Iterations:                      iterz
c*** Integrationsgrenzen:                       zmin, zmax
c*** Anzahl der Stuetzstellen im ersten Raster: ianzz
c*** Maximale Anzahl von Stuetzstellen:         imaxz
c*** Verhaeltnis von fmin zu fmax:              fracz
c*** minimaler Quotient benachbarter Y-Werte:   frac1z

c---------------- START ---------------------------------------
c---------------- first grid is equdistant, the following are not

	if(ianzz.eq.1)then
	dzz=0.d0
	else
	dzz=(xi.zmax-xi.zmin)/dflotj(ianzz-1)
	endif

c-----------------------------------------------------------

       if((src.isrctype.eq.4).or.(src.isrctype.eq.5).or.
     &     (src.isrctype.eq.6).or.(src.isrctype.eq.7))then
c-----------------------------------------------------------
c !$OMP PARALLEL DO PRIVATE(i, ra)
	do i=1,ianzz    
	   ra.n3=i
 	   z(i)=xi.zmin+dflotj(i-1)*dzz
	   call yyint(m4,g,am,src,xi,xir,apr,cs,ra,ifl,st,
     &		z(i),fwerty,fwertz,
     &          yintya,yintyp,yintza,yintzp,sr,blp)
	   if(ifl.ispline.ge.0)then
c	   	fzey(i)=fwerty
c	   	fzez(i)=fwertz
	   densiyre(i)=sr.xintyre
	   densiyim(i)=sr.xintyim
	   densizre(i)=sr.xintzre
	   densizim(i)=sr.xintzim
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

	ispline_save=ifl.ispline

	if(ifl.ispline.eq.-2)then
	ifl.ispline=0
	do i=1,ianzz
		fzey(i)=fya(i)*(dcos(fyp(i))+cs.sqrtm1*dsin(fyp(i)))
		fzez(i)=fza(i)*(dcos(fzp(i))+cs.sqrtm1*dsin(fzp(i)))
	enddo		
	endif

        if(ifl.ispline.eq.-1)call phacor(cs,ra,xi,xir,dz,fya,fyp,ianzz,sp)

	iyz=2
	call simpson(cs,ifl,xi,xir,ianzz,dz,fzey,fya,fyp,xir.yzintey,
     &			xir.yzintya,xir.yzintyp,
     &          densiyre,densiyim,xintyzre,xintyzim,iyz)

        sr.xintyre=xintyzre
        sr.xintyim=xintyzim

        if(ifl.ispline.eq.-1)call phacor(cs,ra,xi,xir,dz,fza,fzp,ianzz,sp)

	iyz=2
	call simpson(cs,ifl,xi,xir,ianzz,dz,fzez,fza,fzp,xir.yzintez,
     &			xir.yzintza,xir.yzintzp,
     &          densizre,densizim,xintyzre,xintyzim,iyz)

        sr.xintzre=xintyzre
        sr.xintzim=xintyzim

	ifl.ispline=ispline_save

c------------------------------------------------------------
	endif
c------------------------------------------------------------

	xir.yzintey=sr.xintyre+cs.sqrtm1*sr.xintyim
	xir.yzintez=sr.xintzre+cs.sqrtm1*sr.xintzim

	return
	end
c-------------------- end adaptive_int ----------------------

c*************************************************************
	subroutine yyint(m4,g,am,src,xi,xir,apr,cs,ra,ifl,st,
     &		dzi,yintey,yintez,
     &          yintya,yintyp,yintza,yintzp,sr,blp)
c*************************************************************
c
c	INPUT : dzi
c      OUTPUT : yint, yintey, yintez
c
c*************************************************************

	implicit real*8(a-h,o-z)
        integer blp
	complex*16 fwert,fwerty,fwertz,
     &      s1,s1eyre,s1eyim,s1ezre,s1ezim,
     &      s2,s2eyre,s2eyim,s2ezre,s2ezim,
     &      yint,yintey,yintez
	complex*16 fyey(4096),fyez(4096)

	dimension densiyre(MAX_INTEGRATION_SIZE),densiyim(MAX_INTEGRATION_SIZE),
     &            densizre(MAX_INTEGRATION_SIZE),densizim(MAX_INTEGRATION_SIZE)

#include '../phase/phase_struct.F'

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
	record /mirrortype/ am
	record /map4/ m4

	dimension y(4096),dy(4096)
	dimension fya(4096),fyp(4096)
	dimension fza(4096),fzp(4096)

c*******************************************************
c	Erstellen eines nochmals reduzierten 
c       Konstantensatzes
c*******************************************************
#ifdef DEBUG1      
        write(*,*),'yyint called'
        call debug_beamline_type_f(blp)
#endif

        if(dabs(xi.distfocz).gt.1.e-10)then
           dzii=dzi+ra.ri.zi/xi.distfocz
        else
           dzii=dzi
        endif

	ra.ri.dzi=dzii

        
#ifdef SEVEN_ORDER
        call subm27(m4.fdetrc,dzii,m4.fdtrrc)
        call subm27(m4.fdetphrc,dzii,m4.fdtphrrc)
        call subm27(m4.fdet1phrc,dzii,m4.fdt1phrrc)
        call subm27(m4.fdet1phrca,dzii,m4.fdt1phrrca)
        call subm27(m4.fdet1phrcb,dzii,m4.fdt1phrrcb)
        call subm27(m4.yprc1,dzii,m4.yprrc1)
        call subm27(m4.zprc1,dzii,m4.zprrc1)
        call subm27(m4.dyprc,dzii,m4.dyprrc)
        call subm27(m4.dzprc,dzii,m4.dzprrc)
        call subm27(m4.ypc_ap_r,dzii,m4.ypc_ap_rr)
        call subm27(m4.zpc_ap_r,dzii,m4.zpc_ap_rr)
        call subm27(m4.xlen1c_r,dzii,m4.xlen1c_rr)
        call subm27(m4.xlen2c_r,dzii,m4.xlen2c_rr)
        call subm27(m4.wrc,dzii,m4.wrrc)
        call subm27(m4.xlrc,dzii,m4.xlrrc)
#else
        call subm2(m4.fdetrc,dzii,m4.fdtrrc)
        call subm2(m4.fdetphrc,dzii,m4.fdtphrrc)
        call subm2(m4.fdet1phrc,dzii,m4.fdt1phrrc)
        call subm2(m4.fdet1phrca,dzii,m4.fdt1phrrca)
        call subm2(m4.fdet1phrcb,dzii,m4.fdt1phrrcb)
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
#endif

c         do ii=0,4
c          write(6,*)'==',m4.fdtphrrc(ii)
c         enddo


	ianzy=xi.ianzy0
	fmax=0.d0

c*** Number of Iterations:                      itery
c*** Integrationsgrenzen:                       ymin, ymax
c*** Anzahl der Stuetzstellen im ersten Raster: ianzy
c*** Maximale Anzahl von Stuetzstellen:         imaxy
c*** Verhaeltnis von fmin zu fmax:              fracy
c*** minimaler Quotient benachbarter Y-Werte:   frac1y

c---------------- START ---------------------------------------
c---------------- first grid is equdistant, the following are not

	if(ianzy.eq.1)then
	dyy=0.d0
	else
	dyy=(xi.ymax-xi.ymin)/dflotj(ianzy-1)
	endif

c------------------------------------------------------------
	if((src.isrctype.eq.4).or.(src.isrctype.eq.5).or.
     &     (src.isrctype.eq.6).or.(src.isrctype.eq.7))then
c------------------------------------------------------------

c-------------------------------------------------------------------------
c----------- Diese Schleife parallelisieren
c-------------------------------------------------------------------------
#ifdef DEBUG1
        write(6,*)' start loop ueber y'
#endif

        do i=1,ianzy
	  ra.n4=i
 	  y(i)=xi.ymin+dflotj(i-1)*dyy
	 
#ifdef DEBUG1
        write(6,*)' call fywert, i=', i
#endif
 
 	  call fywert(m4,g,am,src,xi,xir,apr,cs,ra,ifl,st,
     &		y(i),fyey(i),fyez(i),fya(i),fyp(i),fza(i),fzp(i),sr,blp)

	 densiyre(i)=sr.densyre
	 densiyim(i)=sr.densyim
	 densizre(i)=sr.denszre
	 densizim(i)=sr.denszim

	  if(ifl.ibright.eq.1)then
	  src.so6.abr(ra.n1,ra.n2,ra.n3,ra.n4)=dreal(fwerty)
	  endif

	dy(i)=dyy
	enddo

	iyz=1
	call simpson(cs,ifl,xi,xir,ianzy,dy,fyey,fya,fyp,
     &		yintey,yintya,yintyp,densiyre,densiyim,
     &          xintyzre,xintyzim,iyz)
        sr.xintyre=xintyzre
        sr.xintyim=xintyzim

	iyz=1
	call simpson(cs,ifl,xi,xir,ianzy,dy,fyez,fza,fzp,
     &		yintez,yintza,yintzp,densizre,densizim,
     &          xintyzre,xintyzim,iyz)
        sr.xintzre=xintyzre
        sr.xintzim=xintyzim

c---------------------------------------------------------
	endif
c---------------------------------------------------------

	st.inumb(st.nn1,st.nn2)=st.inumb(st.nn1,st.nn2)+ianzy

	return
	end
c--------------------- end yyint -------------------------


c*************************************************************
 	subroutine fywert(m4,g,am,src,xi,xir,apr,cs,ra,ifl,st,
     &		dyi,fwerty,fwertz,fwya,fwyp,fwza,fwzp,sr,blp)
c*************************************************************
	implicit real*8(a-h,o-z)
	complex*16 densy,densz,fwert,fwerty,fwertz,fd1phcpl,ff,eycpl,ezcpl
        integer blp

#include '../phase/phase_struct.F'


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
        record /parder/ pd                          !!! UF 5.9.2011 
c kann ev. probleme machen in phase_struct ist parder abhaengig von Seven_order
c in der phaselib benutze ich parder4 
	record /mirrortype/ am
	record /map4/ m4

        dimension bb(0:165)

c      dimension p1(0:7,0:7,0:7,0:7)
c      dimension p2(0:7,0:7,0:7,0:7)
c      dimension p12(0:7,0:7,0:7,0:7)
c      dimension p3(0:7,0:7,0:7,0:7)
c      dimension p4(0:7,0:7,0:7,0:7)
c      dimension p34(0:7,0:7,0:7,0:7)
c      dimension p1234(0:7,0:7,0:7,0:7)
c      dimension p11(0:7,0:7,0:7,0:7)
c      dimension p22(0:7,0:7,0:7,0:7)
c      dimension p1122(0:7,0:7,0:7,0:7)
c      dimension p1a(0:7,0:7,0:7,0:7)
c      dimension p2a(0:7,0:7,0:7,0:7)
c      dimension p3a(0:7,0:7,0:7,0:7)
c      dimension p4a(0:7,0:7,0:7,0:7)
c      dimension p11a(0:7,0:7,0:7,0:7)
c      dimension p22a(0:7,0:7,0:7,0:7)
c      dimension p1122a(0:7,0:7,0:7,0:7)
#ifdef DEBUG1
        write(*,*),'fywert called'
        call debug_beamline_type_f(blp)
#endif

        if(dabs(xi.distfocy).gt.1.e-10)then
           dyii=dyi+ra.ri.yi/xi.distfocy
        else
           dyii=dyi
        endif

	ra.ri.dyi=dyii	

	ra.rf.yp=0.d0
	ra.rf.zp=0.d0
	ra.rf.dyp=0.d0
	ra.rf.dzp=0.d0
        fd=0.d0
	fdph=0.d0
	fd1ph=0.d0
        fd1pha=0.d0
        fd1phb=0.d0
	ra.ap.yp_ap=0.d0
	ra.ap.zp_ap=0.d0
	ra.xlength1=0.d0
	ra.xlength2=0.d0

	ra.oe.w=0.d0
	ra.oe.xl=0.d0

c-------------------------------------------------------------------------
c JB: diese Schleife parallelisieren
c SG: besser nicht, diese Schleife benoetigt kaum Rechenzeit
c     und Thread-Scheduling kostet viel mehr Zeit als potentieller Gewinn
c-------------------------------------------------------------------------
        
	var0=1.
        do ii=0,ifl.iord
          ra.rf.yp=ra.rf.yp+m4.yprrc1(ii)*var0
          ra.rf.zp=ra.rf.zp+m4.zprrc1(ii)*var0
          ra.rf.dyp=ra.rf.dyp+m4.dyprrc(ii)*var0
          ra.rf.dzp=ra.rf.dzp+m4.dzprrc(ii)*var0
	  ra.xlength1=ra.xlength1+m4.xlen1c_rr(ii)*var0
	  ra.xlength2=ra.xlength2+m4.xlen2c_rr(ii)*var0

          fd=fd+m4.fdtrrc(ii)*var0
          fdph=fdph+m4.fdtphrrc(ii)*var0
          fd1ph=fd1ph+m4.fdt1phrrc(ii)*var0    
          fd1pha=fd1pha+m4.fdt1phrrca(ii)*var0
          fd1phb=fd1phb+m4.fdt1phrrcb(ii)*var0
          ra.oe.w=ra.oe.w+m4.wrrc(ii)*var0
          ra.oe.xl=ra.oe.xl+m4.xlrrc(ii)*var0
          var0=var0*dyii
        enddo

        xw3sign=1.d0
        xw2sign=1.d0
        xl2sign=1.d0
        
	fd=dabs(fd)
        fdph=dabs(fdph)
        if(fd1ph.lt.0.d0)xl2sign=-1.d0
        fd1ph=dabs(fd1ph)
        if(fd1pha.lt.0.d0)xw3sign=-1.d0  
        fd1pha=dabs(fd1pha)
        if(fd1phb.lt.0.d0)xw2sign=-1.d0
        fd1phb=dabs(fd1phb)

        call psdi(g,src,apr,cs,ifl,ra,sr,blp)

c----------------------------------------

	if((src.isrctype.eq.4).or.(src.isrctype.eq.5).or.(src.isrctype.eq.7))then

c------- ACHTUNG. Vorfaktor sqrt(cos(a)cos(b))/(r*rp)
c	 wird nicht in phase_integration gesetzt, da hier nicht 
c	 unterschieden wird, ob ein OE oder die Summe von mehreren 
c	 OEs berechnet wird. Reihenentwicklungen fuer die Vorfaktoren 
c	 fuer ein OE muessen sinnvollerweise in der Routine fdet_8 
c	 berechnet werden und in bline.c zusammengebaut werden.
c        Zurzeit ist es nur für ein OE richtig implementiert in fdet_8.
c	 Es ist darauf zu achten, dass
c	 - alpha, beta functions of (z,y,dz,dy)
c	 - r, rp functions of (z,y,dz,dy)
c	 von den Startkoordinaten abhaengen
c	 Das wird spaeter mit der Implementierung der hoeheren
c	 Ordnungen der asymptotischen Entwicklung noch mal nachgeholt
c	 (fuer ein optisches Element)
c---------------------------------------------------------------------
c	 Berechnung der Laengen r und rp
c	 if only one OE

c	 Nutzung von  xlen1c, xlen2c
	 
c	 call subdfpl1(g,am.a,ra,pd)       ! mirror
c        call subdfpl1a(g,am.a,ra,pd)      ! grating
c	 fd1ph=dabs(fd1ph*g.cosa*g.cosb)/(dsqrt(pd.qrg1)*dsqrt(pd.arg2))

c        Varianten von inorm2 = 4
c        inorm2 = 40: alle Terme werden mit alter Methode berechnet (convlim=1.0d-50)
c        inorm2 = 41: Beschränkung auf neue Terme
c        inorm2 = 42: Beschränkung auf alte Terme 
	 
c---------------------------------------------------------------------
        if(ifl.inorm2.eq.0)ff=fdph*fd1ph
        if(ifl.inorm2.eq.1)ff=fdph
c        if(ifl.inorm2.eq.2)ff=(fdph*fd1ph)/ra.xlam_test
        if(ifl.inorm2.eq.2)ff=(fdph*fd1ph)/g.xlam
c        if(ifl.inorm2.eq.20)ff=(fdph*fd1pha*fd1phb)/ra.xlam_test
        if(ifl.inorm2.eq.20)ff=(fdph*fd1pha*fd1phb)/g.xlam
        if(ifl.inorm2.eq.21)then
c          ff=fdph/(ra.xlam_test*dsqrt(dabs(fd1pha*fd1phb)))
          ff=fdph/(g.xlam*dsqrt(dabs(fd1pha*fd1phb)))
        endif
c        if(ifl.inorm2.eq.3)ff=(fdph*fd1ph)/ra.xlam_test
        if(ifl.inorm2.eq.3)ff=(fdph*fd1ph)/g.xlam
        if(ifl.inorm2.eq.31)then
c          ff=fdph/(ra.xlam_test*dsqrt(dabs(fd1pha*fd1phb-fd1ph*fd1ph)))
          ff=fdph/(g.xlam*dsqrt(dabs(fd1pha*fd1phb-fd1ph*fd1ph)))
        endif

        if((ifl.inorm2.eq.4 ).or.(ifl.inorm2.eq.40).or.
     &     (ifl.inorm2.eq.41).or.(ifl.inorm2.eq.42))then

c       fd1pha is 3rd order, 
c       fd1phb is 2nd order term in w     
c       fd1ph  is 2nd order term in l

        if(dabs(fd1phb).lt.1.d-50)then
          write(6,*)' Attention: quadratic term < 1d-50: ',fd1phb 
          write(6,*)'terminating program'
          stop
        endif

c=================== A C H T U N G =================================
c       xlenmir: Spiegellänge in mm, später als Parameter übergeben
        xlenmir=500.d0 

c       Konvergenzkriterien, maximale Werte sind:
        nk=165
        convlim=5.1d0   
        if(ifl.inorm2.eq.40)convlim=1.0d-50           ! use only 2nd order
 
        if(fd1pha.lt.1.d0-50)then
          w0a=1.0d50                                   ! use only 2nd order
          else
          w0a=ra.oe.w-(2.d0*xw2sign*fd1phb)/(3.d0*xw3sign*fd1pha)
        endif

        if(dabs(w0a).gt.xlenmir/2.d0)convlim=1.0d-50  ! use only 2nd order

        fd1ph_sav=fd1ph
        fd1phb_sav=fd1phb

        fact=(dsqrt(dabs(g.cosa))*
     &        dsqrt(dabs(g.cosb)))/(g.r*g.rp)
        fact1=fact*2.0d0   
c        fact2=fact1*((2.d0*cs.pi)/ra.xlam_test)
        fact2=fact1*((2.d0*cs.pi)/g.xlam)
c        fd1ph=(fd1ph/fact1)*2.d0*ra.xlam_test**3
        fd1ph=(fd1ph/fact1)*2.d0*g.xlam**3
        fd1pha=fd1pha*fact2
        fd1phb=fd1phb*fact2

        abar=xw3sign/fd1pha**(1.d0/3.d0)
        bbar=xw2sign*fd1phb*abar**2

c------------- get convlim (can be done on a finer grid in the future)
        if(dabs(bbar).lt.2.d0)nk=15
        if((dabs(bbar).ge.2.d0).and.(dabs(bbar).lt.3.d0))nk=60
        if((dabs(bbar).ge.3.d0).and.(dabs(bbar).lt.4.d0))nk=100
        if(dabs(bbar).ge.4.d0)nk=165

        if(dabs(bbar).gt.convlim)then

c------------- only quadratic terms

          fd1ph=fd1ph_sav
          fd1phb=fd1phb_sav
          ff=exp(xw2sign*cs.sqrtm1*cs.pi*0.25d0)*exp(xl2sign*cs.sqrtm1*cs.pi*0.25d0)*
     &       (fdph/(g.xlam*dsqrt(dabs(fd1ph*fd1phb))))/2.d0
c     &       (fdph/(ra.xlam_test*dsqrt(dabs(fd1ph*fd1phb))))/2.d0
         if(ifl.inorm2.eq.41)ff=0.d0

         else

c------------- include 3rd order Term in w

        bb(0)=1.d0
        do n=1,nk
         bb(n)=bb(n-1)*bbar
        enddo

        sumcos=0.d0
        sumsin=0.d0     

        nmaxc=2*(nk/2)
        nmaxs=2*((nk+1)/2)-1

        do n=0,nmaxc,2
          sumcos=sumcos+bb(n)*cs.fc(n)     
        enddo
        do n=1,nmaxs,2
          sumsin=sumsin+bb(n)*cs.fs(n)     
        enddo
        sumcos=sumcos*abar
        sumsin=sumsin*abar

        ff=(fdph/dsqrt(fd1ph))*exp(xl2sign*cs.sqrtm1*cs.pi*0.25d0)
        ff=ff*(sumcos+sumsin*cs.sqrtm1)
        if(ifl.inorm2.eq.42)ff=0.d0

        endif  ! bbar.lt.convlim

        endif  ! inorm2.eq.4

c--------------------------------------------------------------------------

	if(ifl.ispline.ge.0)then
                sr.densy=sr.densy*ff
                sr.densz=sr.densz*ff
		sr.densyre=dreal(sr.densy)
                sr.densyim=dimag(sr.densy)
                sr.denszre=dreal(sr.densz)
                sr.denszim=dimag(sr.densz)
	endif

	if(ifl.ispline.lt.0)then
                eycpl=sr.eya*cdexp(cs.sqrtm1*sr.eyp)
                eycpl=eycpl*ff
                ezcpl=sr.eza*cdexp(cs.sqrtm1*sr.ezp)
                ezcpl=ezcpl*ff
               write(6,*)' hier gibt es noch was zu tun '
c               conversion of complex data in amplitude and phase
c		fwya=cdabs(eycpl)
c		fwza=cdabs(ezcpl)
c		fwyp=sr.eyp
c		fwzp=sr.ezp
	endif

c------------------------------------------------

	endif

c--------------- ist zu checken
	if(src.isrctype.eq.6)then
	fwerty=sr.densy*abs(fd)
	endif

	return
	end
c---------------------- end fywert -----------------------


c***********************************************************
c UF 12.1.2000
	subroutine phacor(cs,ra,xi,xir,dx,amp,pha,ianz,sp)
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

#include '../phase/phase_struct.F'

        record /rayst/ ra
        record /constants/ cs
	record /statistics/ st
	record /integration_results/ xir
	record /integration/ xi
        record /control_flags/ ifl
        record /psimagest/ sp

	dimension dx(4096),x(4096),
     &            amp(4096),pha(4096),pha_new(4096),
     &		  d1(4096),d2(4096),d12(4096),
     &		  istart(4096),iend(4096)


	dimension x_fit(1024),amp_fit(1024),pha_fit(1024)
	dimension coef(1024),coef1(1024),xx(1024)

	SMALL_PHA1=SMALL_PHA*1.0d5
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

c UF 27.5.11 hier steigt es aus
c The shapes of the array expressions do not conform

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
	  print*,' can not find region of stationary phase'
	  print*,' increase number of grid points'
c UF 11.11.00	  stop
c UF 12.1.2000
          sp.iwidth= 1
          sp.iheigh= 1
          return
	endif

	if((iend(iiregion)-istart(iiregion)+1).eq.2)then
	  print*,' region of stationary phase has only two points'
	  print*,' increase number of grid points'
c	UF 11.11.00  stop
c UF 12.1.2000
          sp.iwidth= 1
          sp.iheigh= 1
          return
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
	if(dabs(dabs(dd1)-dabs(dd)).gt.SMALL_PHA1)then
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
	if(dabs(dabs(dd1)-dabs(dd)).gt.SMALL_PHA1)then
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
c--------------------------- end phacor ---------------

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
		print*,'******** error in fitpar *************'
		print*,'******** not enough data points ******'	
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
c 14.3.06 kann deqn nicht finden
c UF!!!	call deqn(lnum,a,number,work,ifail1,1,b)
 	if(ifail1.ne.0)then
		print*,'******* Fehler in FITPAR ******'
		print*,'******* Fehler beim Loesen' 
		print*,'******* des Gleichungssystems ******'
	endif

	do i=1,iiord+1
		c(i)=b(i)
	enddo

	return
	end
c--------------------end fitpar ----------------

c******************************************************************
	subroutine simpson(cs,ifl,xi,xir,ianz,dyz,fyz,fyza,fyzp,xint,
     &                      xinta,xintp,densiyzre,densiyzim,
     &                      xintre,xintim,iyz)
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
c
c	iyz = 1 call from y-loop
c	iyz = 2 call from z-loop
c-----------------------------------------------------------------------

	implicit real*8(a-h,o-z)

	dimension densiyzre(MAX_INTEGRATION_SIZE),densiyzim(MAX_INTEGRATION_SIZE)

#include '../phase/phase_struct.F'

        record /constants/ cs
	record /statistics/ st
        record /integration_results/ xir
        record /integration/ xi
        record /control_flags/ ifl

	complex*16 xint,fyz(4096),f1,f2,xxx(4096)
	dimension xxxre(4096),xxxim(4096)

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
	xint=0.d0

	x(1)=0.d0
	do i=2,ianz
	x(i)=x(i-1)+dyz(i)
	enddo

c111	format(3(3x,d12.5))
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
		c=0.5d0*(y1+y2)
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
		c=0.5d0*(y1+y2)
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
	if(xinta.gt.SMALL_PHA)then
		xintp=dimag(cdlog(xint/xinta))
	  else
		xintp=0.d0
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
c UF 11.2.00 xx muss bestimmt festgehalten werden xxsave
	if(iyz.eq.1)xx=xi.ymin
	if(iyz.eq.2)xx=xi.zmin
        xxsave=xx
	do ii=1,xir.iisimp
           if(xir.isimp(ii).eq.xir.nsimp)then
c     UF
              xx=xxsave
              do i=1,ianz_loc-1
                 xx=xx+dyz_loc(i)
                 xir.sintre(ii,1,i)=xx
                 xir.sintre(ii,2,i)=dreal(xxx(i))
              enddo
              xir.isintre(ii)=ianz_loc-1
c     UF
              xx=xxsave
              do i=1,ianz_loc-1
                 xx=xx+dyz_loc(i)
                 xir.sintim(ii,1,i)=xx
                 xir.sintim(ii,2,i)=dimag(xxx(i))
              enddo
              xir.isintim(ii)=ianz_loc-1
c     UF
              xx=xxsave
              do i=1,ianz_loc
                 xir.simpa(ii,1,i)=xx
                 xir.simpa(ii,2,i)=fyza_loc(i)
                 xx=xx+dyz_loc(i)
              enddo
              xir.isimpa(ii)=ianz_loc
c     UF
              xx=xxsave
              do i=1,ianz_loc
                 xir.simpp(ii,1,i)=xx
                 xir.simpp(ii,2,i)=fyzp_loc(i)
                 xx=xx+dyz_loc(i)
	enddo
	xir.isimpp(ii)=ianz_loc
        
      endif
      enddo
      
c---------------------------------------------------------
      
      endif                     ! ispline.lt.0

c---------------------------------------------------------
	if(ifl.ispline.eq.0)then
c---------------------------------------------------------

c	xint=0.
c	if(ianz.ge.2)then
c	do i=1,ianz-1
c	xint=xint+0.5*dyz(i)*(fyz(i+1)+fyz(i))
c	xxx(i)=xint
c	enddo
c	endif

	xintre=0.d0
	xintim=0.d0
	if(ianz.ge.2)then
	do i=1,ianz
	xintre=xintre+densiyzre(i)
	xintim=xintim+densiyzim(i)
	xxxre(i)=xintre
	xxxim(i)=xintim
	enddo
	endif

	xintre=xintre*dyz(1)
	xintim=xintim*dyz(1)

c----------- output
c UF 11.2.00 xx muss bestimmt festgehalten werden xxsave
	if(iyz.eq.1)xx=xi.ymin
	if(iyz.eq.2)xx=xi.zmin
        xxsave=xx

	do ii=1,xir.iisimp
           if(xir.isimp(ii).eq.xir.nsimp)then
              xx=xxsave
              do i=1,ianz-1
                 xx=xx+dyz(i)
                 xir.sintre(ii,1,i)=xx
                 xir.sintre(ii,2,i)=xxxre(i)*dyz(1)
              enddo
              xir.isintre(ii)=ianz-1
c UF
              xx=xxsave               
              do i=1,ianz-1
                 xx=xx+dyz(i)
                 xir.sintim(ii,1,i)=xx
                 xir.sintim(ii,2,i)=xxxim(i)*dyz(1)
              enddo
              xir.isintim(ii)=ianz-1
c UF
              xx=xxsave               
              do i=1,ianz                 
                 xir.simpre(ii,1,i)=xx
                 xir.simpre(ii,2,i)=densiyzre(i)
                 xx=xx+dyz(i)
              enddo
              xir.isimpre(ii)=ianz
c UF
              xx=xxsave              
              do i=1,ianz
                 xir.simpim(ii,1,i)=xx
                 xir.simpim(ii,2,i)=densiyzim(i)
                 xx=xx+dyz(i)
              enddo
              xir.isimpim(ii)=ianz
              
           endif
	enddo
c-------------------

      endif                     ! ispline.eq.0

c------------------------------------------------------------

	if(ifl.ispline.eq.1)then

	x(i)=0.d0
	do i=2,ianz
	x(i)=x(i-1)+dyz(i)
c	y(i)=dreal(fyz(i))
	y(i)=densiyzre(i)
        enddo

	do i=2,ianz
c	y(i)=dimag(fyz(i))
	y(i)=densiyzim(i)
	enddo

	xint=xintre+cs.sqrtm1*xintim

c----------- output
c UF 11.2.00 xx muss bestimmt festgehalten werden xxsave
	if(iyz.eq.1)xx=xi.ymin
	if(iyz.eq.2)xx=xi.zmin
        xxsave=xx
	do ii=1,xir.iisimp
           if(xir.isimp(ii).eq.xir.nsimp)then
c UF
              xx=xxsave
              do i=1,ianz
                 xir.simpre(ii,1,i)=xx
                 xir.simpre(ii,2,i)=densiyzre(i)
                 xx=xx+dyz(i)
              enddo
              xir.isimpre(ii)=ianz
c UF
              xx=xxsave
              do i=1,ianz
                 xir.simpim(ii,1,i)=xx
                 xir.simpim(ii,2,i)=densiyzim(i)
                 xx=xx+dyz(i)
              enddo
              xir.isimpim(ii)=ianz
              
           endif
      enddo
c----------------

	endif

	return
	end
	
c------------------------------------------------------
c	subroutine guess(m4,g,am,src,apr,cs,ra,ifl,xi,xir,st,blp)
c------------------------------------------------------
c routine ausgelagert in separate file guess.F
