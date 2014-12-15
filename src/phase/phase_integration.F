c$$$ File      : ~/phase/src/phase/phase_integration.F
c$$$ Date      : <21 Nov 07 12:42:16 flechsig> 
c$$$ Time-stamp: <15 Dec 14 14:23:50 flechsig> 
c$$$ Author    : J.B. + modification Uwe Flechsig
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
c UF 20. Mar 2014 removed statistics 
c UF OCT 14 remove src and give isrctype directly
c UF isrctype wird wegen brightness gebraucht
c*********************************************************
      subroutine adaptive_int(m4,g,isrctype,apr,cs,ra,
     &     ifl,xi,xir,sp,lostwl,blp)
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
        integer blp, isrctype, lostwl
c UF 28.6.13	complex*16 fwert,fwerty,fwertz,
c     &      s1,s1eyre,s1eyim,s1ezre,s1ezim,
c     &      s2,s2eyre,s2eyim,s2ezre,s2ezim,
c     &      yzint,yzintey,yzintez
        complex*16 fwerty,fwertz
     
	complex*16 fzey(4096),fzez(4096)

#include '../phase/phase_struct.F'

        record /constants/ cs
        record /geometryst/ g
        record /rayst/ ra
        record /source_results/ sr
        record /integration_results/ xir
        record /control_flags/ ifl
c        record /sources/ src
	record /integration/ xi
	record /apertures/ apr
c uf	record /mirrortype/ am
	record /map4/ m4
        record /psimagest/ sp

	dimension densiyre(MAX_INTEGRATION_SIZE),
     &            densiyim(MAX_INTEGRATION_SIZE),
     &            densizre(MAX_INTEGRATION_SIZE),
     &            densizim(MAX_INTEGRATION_SIZE)

  	dimension z(4096),dz(4096)
	dimension fya(4096),fyp(4096)
	dimension fza(4096),fzp(4096)

c#ifdef DEBUG1
c       write(*,*),'adaptive_int called1'
c        call debug_beamline_type_f(blp)
c        write(*,*),'adaptive_int called2'
c#endif

  
                   
	if((xir.si1.iiheigh.eq.1).and.(xir.si1.iiwidth.eq.1).and.
     &		(ifl.ispline.lt.0))then
           write(*,*)'************************************'
           write(*,*)'call to guess removed U.F. 22.6.13  '
           write(*,*)'************************************'
c	call guess(m4,g,am,src,apr,cs,ra,ifl,xi,xir,st,blp)
	endif

#ifdef DEBUG1
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
#endif

c*******************************************************
c	Erstellen eines reduzierten Konstantensatzes
c*******************************************************
 
	yi=ra.ri.yi
	zi=ra.ri.zi

c#ifdef SEVEN_ORDER
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
c#else
c        call subm1(m4.fdetc,yi,zi,m4.fdetrc)
c        call subm1(m4.fdetphc,yi,zi,m4.fdetphrc)
c        call subm1(m4.fdet1phc,yi,zi,m4.fdet1phrc)
c        call subm1(m4.fdet1phca,yi,zi,m4.fdet1phrca)
c        call subm1(m4.fdet1phcb,yi,zi,m4.fdet1phrcb)
c        call subm1(m4.ypc1,yi,zi,m4.yprc1)
c        call subm1(m4.zpc1,yi,zi,m4.zprc1)
c        call subm1(m4.dypc,yi,zi,m4.dyprc)
c        call subm1(m4.dzpc,yi,zi,m4.dzprc)
c        call subm1(m4.ypc_ap,yi,zi,m4.ypc_ap_r)
c        call subm1(m4.zpc_ap,yi,zi,m4.zpc_ap_r)
c        call subm1(m4.xlen1c,yi,zi,m4.xlen1c_r)
c        call subm1(m4.xlen2c,yi,zi,m4.xlen2c_r)
c        call subm1(m4.wc,yi,zi,m4.wrc)
c        call subm1(m4.xlc,yi,zi,m4.xlrc)
c#endif

c        write(6,*)'m4.fdet1phrcb(0,0),m4.fdet1phrcb(1,0)',m4.fdet1phrcb(0,0),m4.fdet1phrcb(1,0)
c------------------------------------------------------

c	st.inumb(st.nn1,st.nn2)=0.d0
c	fmax=0.d0

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
C UF Nov 14 take out if
c uf       if((isrctype.eq.4).or.(isrctype.eq.5).or.
c uf     &     (isrctype.eq.6).or.(isrctype.eq.7))then
c-----------------------------------------------------------
c !$OMP PARALLEL DO PRIVATE(i, ra)
	do i=1,ianzz    
	   ra.n3=i
 	   z(i)=xi.zmin+dflotj(i-1)*dzz
c uf	   call yyint(m4,g,am,src,xi,xir,apr,cs,ra,ifl,st,
c UF	   call yyint(m4,g,src,xi,xir,apr,cs,ra,ifl,
           call yyint(m4,g,isrctype,xi,xir,apr,cs,ra,ifl,
     &		z(i),fwerty,fwertz,
     &          yintya,yintyp,yintza,yintzp,sr,lostwl,blp)
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
c	   fmax=dmax1(fmax,cdabs(fwerty),cdabs(fwertz))
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
        
        if(ifl.ispline.eq.-1)then
           call phacor(cs,ra,xi,xir,dz,fya,fyp,ianzz,sp)
        endif
	iyz=2
c     UF	call simpson(cs,ifl,xi,xir,ianzz,dz,fzey,fya,fyp,xir.yzintey,
        call simpson(cs,ifl,xi,xir,ianzz,dz,fya,fyp,xir.yzintey,
     &       xir.yzintya,xir.yzintyp,
     &       densiyre,densiyim,xintyzre,xintyzim,iyz)
        
        sr.xintyre=xintyzre
        sr.xintyim=xintyzim
        
        if(ifl.ispline.eq.-1)then
           call phacor(cs,ra,xi,xir,dz,fza,fzp,ianzz,sp)
        endif
	iyz=2
c     call simpson(cs,ifl,xi,xir,ianzz,dz,fzez,fza,fzp,xir.yzintez,
        call simpson(cs,ifl,xi,xir,ianzz,dz,fza,fzp,xir.yzintez,
     &       xir.yzintza,xir.yzintzp,
     &       densizre,densizim,xintyzre,xintyzim,iyz)
        
        sr.xintzre=xintyzre
        sr.xintzim=xintyzim
        
	ifl.ispline=ispline_save
        
c------------------------------------------------------------
c      endif
c------------------------------------------------------------
      
      xir.yzintey=sr.xintyre+cs.sqrtm1*sr.xintyim
      xir.yzintez=sr.xintzre+cs.sqrtm1*sr.xintzim
      
      return
      end
c--------------------end adaptive_int ----------------------

c*************************************************************
c       subroutine yyint(m4,g,src,xi,xir,apr,cs,ra,ifl,
       subroutine yyint(m4,g,isrctype,xi,xir,apr,cs,ra,ifl,
     &		dzi,yintey,yintez,
     &          yintya,yintyp,yintza,yintzp,sr,lostwl,blp)
c*************************************************************
c
c	INPUT : dzi
c      OUTPUT : yint, yintey, yintez
c
c*************************************************************

	implicit real*8(a-h,o-z)
        integer lostwl,blp
	complex*16 
c UF 28.6.13     &      fwert,fwerty,fwertz,
c     &      s1,s1eyre,s1eyim,s1ezre,s1ezim,
c     &      s2,s2eyre,s2eyim,s2ezre,s2ezim,
c     &      yint,yintey,yintez
     &       yintey,yintez
c UF	complex*16 fyey(4096),fyez(4096)
        complex*16 fyey(4096)

	dimension densiyre(MAX_INTEGRATION_SIZE),densiyim(MAX_INTEGRATION_SIZE),
     &            densizre(MAX_INTEGRATION_SIZE),densizim(MAX_INTEGRATION_SIZE)

#include '../phase/phase_struct.F'

        record /constants/ cs
        record /geometryst/ g
        record /rayst/ ra
        record /source_results/ sr
        record /control_flags/ ifl
c        record /sources/ src
	record /integration/ xi
	record /integration_results/ xir
c UF 28.6.13        record /source6/ so6
	record /apertures/ apr
c uf        record / statistics/ st
c uf	record /mirrortype/ am
	record /map4/ m4

	dimension y(4096),dy(4096)
	dimension fya(4096),fyp(4096)
	dimension fza(4096),fzp(4096)

c*******************************************************
c	Erstellen eines nochmals reduzierten 
c       Konstantensatzes
c*******************************************************
c#ifdef DEBUG      
c       write(*,*),'yyint called'
c      call debug_beamline_type_f(blp)
c#endif

        if(dabs(xi.distfocz).gt.1.e-10)then
           dzii=dzi+ra.ri.zi/xi.distfocz
        else
           dzii=dzi
        endif

	ra.ri.dzi=dzii

        
c#ifdef SEVEN_ORDER
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
c#else
c        call subm2(m4.fdetrc,dzii,m4.fdtrrc)
c       call subm2(m4.fdetphrc,dzii,m4.fdtphrrc)
c        call subm2(m4.fdet1phrc,dzii,m4.fdt1phrrc)
c        call subm2(m4.fdet1phrca,dzii,m4.fdt1phrrca)
c        call subm2(m4.fdet1phrcb,dzii,m4.fdt1phrrcb)
c        call subm2(m4.yprc1,dzii,m4.yprrc1)
c        call subm2(m4.zprc1,dzii,m4.zprrc1)
c        call subm2(m4.dyprc,dzii,m4.dyprrc)
c        call subm2(m4.dzprc,dzii,m4.dzprrc)
c        call subm2(m4.ypc_ap_r,dzii,m4.ypc_ap_rr)
c        call subm2(m4.zpc_ap_r,dzii,m4.zpc_ap_rr)
c        call subm2(m4.xlen1c_r,dzii,m4.xlen1c_rr)
c        call subm2(m4.xlen2c_r,dzii,m4.xlen2c_rr)
c        call subm2(m4.wrc,dzii,m4.wrrc)
c        call subm2(m4.xlrc,dzii,m4.xlrrc)
c#endif

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
C UF Nov 14 take out if
c uf 	if((isrctype.eq.4).or.(isrctype.eq.5).or.
c uf     &     (isrctype.eq.6).or.(isrctype.eq.7))then
c------------------------------------------------------------

c-------------------------------------------------------------------------
c----------- Diese Schleife parallelisieren
c-------------------------------------------------------------------------
c#ifdef DEBUG
c        write(6,*)' start loop ueber y'
c#endif

        do i=1,ianzy
	  ra.n4=i
 	  y(i)=xi.ymin+dflotj(i-1)*dyy
	 
c#ifdef DEBUG
c        write(6,*)' call fywert, i=', i
c#endif
 
 	  call fywert(m4,g,isrctype,xi,apr,cs,ra,ifl,
     &		y(i),fyey(i),sr,lostwl,blp)

          densiyre(i)=sr.densyre
          densiyim(i)=sr.densyim
          densizre(i)=sr.denszre
          densizim(i)=sr.denszim
          
c     UF	  if(ifl.ibright.eq.1)then
c     UF	  src.so6.abr(ra.n1,ra.n2,ra.n3,ra.n4)=dreal(fwerty)
c     UF	  endif

          dy(i)=dyy
       enddo
       
       iyz=1
c     UF	call simpson(cs,ifl,xi,xir,ianzy,dy,fyey,fya,fyp,
       call simpson(cs,ifl,xi,xir,ianzy,dy,fya,fyp,
     &      yintey,yintya,yintyp,densiyre,densiyim,
     &      xintyzre,xintyzim,iyz)
       sr.xintyre=xintyzre
       sr.xintyim=xintyzim
       
       iyz=1
c     UF	call simpson(cs,ifl,xi,xir,ianzy,dy,fyez,fza,fzp,
       call simpson(cs,ifl,xi,xir,ianzy,dy,fza,fzp,
     &      yintez,yintza,yintzp,densizre,densizim,
     &      xintyzre,xintyzim,iyz)
       sr.xintzre=xintyzre
       sr.xintzim=xintyzim
       
c---------------------------------------------------------
c uf      endif
c---------------------------------------------------------

c     st.inumb(st.nn1,st.nn2)=st.inumb(st.nn1,st.nn2)+ianzy
      
      return
      end
c--------------------- end yyint -------------------------


c*************************************************************
c 	subroutine fywert(m4,g,am,src,xi,xir,apr,cs,ra,ifl,st,
c     &		dyi,fwerty,fwertz,fwya,fwyp,fwza,fwzp,sr,blp)
c UF remove unused vars am , xir, st, fwertz, fwya, fwyp, fwza, fwzp
 	subroutine fywert(m4,g,isrctype,xi,apr,cs,ra,ifl,
     &		          dyi,fwerty,sr,lostwl,blp)
c*************************************************************
	implicit real*8(a-h,o-z)
c UF	complex*16 densy,densz,fwert,fwerty,fwertz,fd1phcpl,ff,eycpl,ezcpl
        complex*16 fwerty,ff,ff1,eycpl,ezcpl
        integer lostwl,blp

c switch off the height_error_code

#include '../phase/phase_struct.F'

        record /constants/ cs
        record /geometryst/ g
        record /rayst/ ra
        record /source_results/ sr
        record /control_flags/ ifl
c        record /sources/ src
	record /integration/ xi
c uf	record /integration_results/ xir
	record /apertures /apr
c uf        record /statistics/ st
c UF28.6.13        record /parder/ pd                          !!! UF 5.9.2011 
c kann ev. probleme machen in phase_struct ist parder abhaengig von Seven_order
c in der phaselib benutze ich parder4 
c uf	record /mirrortype/ am
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
c UF 28.6.13 uninitialized memory determins fd1ph- probably m4.fdtphrrc
          fd1ph=fd1ph+m4.fdt1phrrc(ii)*var0    
          fd1pha=fd1pha+m4.fdt1phrrca(ii)*var0
          fd1phb=fd1phb+m4.fdt1phrrcb(ii)*var0
          ra.oe.w=ra.oe.w+m4.wrrc(ii)*var0
          ra.oe.xl=ra.oe.xl+m4.xlrrc(ii)*var0
          var0=var0*dyii
        enddo

c--------------- Vorzeichen checken
c        write(6,*)' yp, zp ',ra.rf.yp,ra.rf.zp
c        write(6,*)' dyp, dzp ',ra.rf.dyp,ra.rf.dzp
c        write(6,*)' xlength1, xlength2 ',ra.xlength1,ra.xlength2
c        write(6,*)' w, l ',ra.oe.w,ra.oe.xl

        xw3sign=1.d0
        xw2sign=1.d0
        xl2sign=1.d0
        
	fd=dabs(fd)
        fdph=dabs(fdph)
c UF 29.6.13 valgrind finds error next line: Conditional jump or move depends on uninitialised value(s)

       if((ifl.inorm2.eq.4 ).or.(ifl.inorm2.eq.40).or.
     &     (ifl.inorm2.eq.41).or.(ifl.inorm2.eq.42).or.
     &     (ifl.inorm2.eq.43).or.(ifl.inorm2.eq.44).or.
     &     (ifl.inorm2.eq.45))then
        if(fd1ph.lt.0.d0)xl2sign=-1.d0
        fd1ph=dabs(fd1ph)
        if(fd1pha.lt.0.d0)xw3sign=-1.d0  
        fd1pha=dabs(fd1pha)
        if(fd1phb.lt.0.d0)xw2sign=-1.d0
        fd1phb=dabs(fd1phb)
        endif

c   here we call phase_source.F
        call psdi(g,isrctype,apr,cs,ifl,ra,sr,lostwl,blp)     

c----------------------------------------
C UF Nov 14 change
c	if((isrctype.eq.4).or.(isrctype.eq.5).or.
c     &     (isrctype.eq.7))then
        if(isrctype.ne.6)then
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
c        inorm2 = 4: Berechnung mit Loolup Table alle Terme
c        inorm2 = 41: Berechnung mit Loolup Table aber beschränkt auf quadratische Terme
c        inorm2 = 42: Berechnung mit Loolup Table aber beschränkt auf kubische Terme 
c        inorm2 = 40: alle Terme quadratisch berechnet (convlim=1.0d-50)
c        inorm2 = 43: Isabelles Formeln, komplett
c        inorm2 = 44: Isabelles Formeln, aber beschränkt auf quadratische Terme
c        inorm2 = 45: Isabelles Formeln, aber beschränkt auf kubische Terme

c---------------------------------------------------------------------
             
c UF neue routine           
c        call getgeostr(blp, sina, cosa, sinb, cosb, r, rp, xlam)

        if(ifl.inorm2.eq.0)ff=fdph*fd1ph
        if(ifl.inorm2.eq.1)ff=fdph*fd1ph
        if(ifl.inorm2.eq.2)ff=(fdph*fd1ph)/g.xlam
        if(ifl.inorm2.eq.20)ff=(fdph*fd1pha*fd1phb)/g.xlam
        if(ifl.inorm2.eq.21)then
          ff=fdph/(g.xlam*dsqrt(dabs(fd1pha*fd1phb)))
        endif

        if(ifl.inorm2.eq.3)ff=(fdph*fd1ph)/g.xlam

c------------------ only quadratic terms, no diagonalization 
c   Mandel & Wolf (different from Focke?)
        if(ifl.inorm2.eq.31)then
          arg=fd1pha*fd1phb-fd1ph*fd1ph
          spur=fd1pha+fd1phb
          ff=fdph/(g.xlam*dsqrt(dabs(arg)))
          if((arg.gt.0.d0).and.(spur.gt.0.d0))ff1=cs.sqrtm1
          if((arg.gt.0.d0).and.(spur.lt.0.d0))ff1=-cs.sqrtm1
          if(arg.lt.0.d0)ff1=1.d0
          ff=ff*ff1
         endif

c--------- Variante 2, with diagonalization, gleiches Ergebnis wie Variante 1
          if(ifl.inorm2.eq.32)then
          ax=fd1pha
          bx=fd1phb
          cx=fd1ph
          at=ax
          bt=bx-cx**2/ax
          asig=1.d0
          bsig=1.d0
          if(at.lt.0.d0)asig=-1.d0
          if(bt.lt.0.d0)bsig=-1.d0
          ff=cdexp(asig*cs.sqrtm1*cs.pi*0.25d0)*
     &       cdexp(bsig*cs.sqrtm1*cs.pi*0.25d0)*
     &     (fdph/(g.xlam*dsqrt(dabs(at))*dsqrt(dabs(bt))))      
          endif

c--------- Variante 3, liefert wie erwartet gleiche Ergebnisse wie Variante 2
          if(ifl.inorm2.eq.33)then
          ax=fd1pha
          bx=fd1phb
          cx=fd1ph
          bt=bx
          at=ax-cx**2/bx
          asig=1.d0
          bsig=1.d0
          if(at.lt.0.d0)asig=-1.d0
          if(bt.lt.0.d0)bsig=-1.d0
          ff=cdexp(asig*cs.sqrtm1*cs.pi*0.25d0)*
     &       cdexp(bsig*cs.sqrtm1*cs.pi*0.25d0)*
     &     (fdph/(g.xlam*dsqrt(dabs(at))*dsqrt(dabs(bt))))            
        endif

c------------------ only quadratic terms, diagonalization version 1 & 2
        if((ifl.inorm2.eq.41).or.(ifl.inorm2.eq.42))then

          ff=fdph/(g.xlam*dsqrt(dabs(fd1pha)*dabs(fd1phb)))
          ff=ff*cdexp(xw3sign*cs.sqrtm1*cs.pi*0.25d0)*
     &          cdexp(xw2sign*cs.sqrtm1*cs.pi*0.25d0)
  
c        write(6,*)' '
c        write(6,*)xw3sign*fd1pha,xw2sign*fd1phb
c        write(6,*)cdabs(ff)

        endif

c-------------- further values of inorm2

        if((ifl.inorm2.eq.4 ).or.
     &     (ifl.inorm2.eq.40).or.(ifl.inorm2.eq.41).or.
     &     (ifl.inorm2.eq.43).or.(ifl.inorm2.eq.44).or.
     &     (ifl.inorm2.eq.45))then

c       fd1pha is 3rd order, 
c       fd1phb is 2nd order term in w     
c       fd1ph  is 2nd order term in l

        if(dabs(fd1phb).lt.1.d-50)then
          write(6,*)' Attention: quadratic term < 1d-50: ',fd1phb 
          write(6,*)'terminating program'
          stop
        endif

c  folgenden Ausdruck nur einmal berechnen, auslagern
        divmax=dmax1(dabs(xi.ymin),dabs(xi.ymax),
     &               dabs(xi.zmin),dabs(xi.zmax))
        xlenmir=2.d0*g.rp*(dtan(divmax)/g.cosb)
c        write(6,*)' xlenmir === ',xlenmir
 
        small_fd1ph=1.d-50

        if(ifl.inorm2.eq.4)convlim=19.1d0
        if((ifl.inorm2.eq.40).or.
     &     (ifl.inorm2.eq.41))convlim=1.0d-50           ! use only 2nd order
        if((ifl.inorm2.eq.43).or.
     &     (ifl.inorm2.eq.44).or.
     &     (ifl.inorm2.eq.45))convlim=5.1d0

        if(fd1pha.lt.1.d0-50)convlim=1.0d-50           ! use only 2nd order
        w0a=dabs(fd1phb/fd1pha)
   
        if(w0a.lt.xlenmir/2.d0)then    
          redu=0.5d0                                   ! do not count contributions from
          else                                         ! the critical points twice
          redu=1.d0
        endif                                            

        fd1ph_sav=fd1ph
        fd1phb_sav=fd1phb

        fact1=(dsqrt(dabs(g.cosa))*
     &        dsqrt(dabs(g.cosb)))/(g.r*g.rp) 
        fact2=fact1*((2.d0*cs.pi)/g.xlam)
        fd1ph=(fd1ph/fact1)*2.d0*g.xlam**3
        tl=((ra.xlength1*ra.xlength2)/(g.r*g.rp))**2
c        fd1ph=fd1ph*tl ! thick lens correction
        fd1pha=fd1pha*fact2
        fd1phb=fd1phb*fact2

        abar=xw3sign/fd1pha**(1.d0/3.d0)
        bbar=xw2sign*fd1phb*abar**2

c----------------------------------------------------------------------------      
        if((ifl.inorm2.eq.4).or.
     &     (ifl.inorm2.ne.40).or.
     &     (ifl.inorm2.ne.41).or.
     &     (ifl.inorm2.ne.42))then
c-----------------------------------    
 
         if(dabs(bbar).gt.convlim)then

c------------- only quadratic terms
          fd1ph=fd1ph_sav
          fd1phb=fd1phb_sav
          ff=cdexp(xw2sign*cs.sqrtm1*cs.pi*0.25d0)*
     &     cdexp(xl2sign*cs.sqrtm1*cs.pi*0.25d0)*
     &     (fdph/(g.xlam*dsqrt(dabs(2.d0*fd1ph*2.d0*fd1phb))))
         if(fd1ph.lt.small_fd1ph)ff=0.d0
  
c        write(6,*)' '
c        write(6,*)fd1phb,fd1ph
c        write(6,*)cdabs(ff)

         else

        if(bbar.lt.0.d0)then
          bbar1=-bbar
          else
          bbar1=bbar
        endif
        call spa_3rd_order(bbar1,ssin,scos,blp)
        if(bbar.lt.0.d0)ssin=-ssin
        sumcos=scos*abar
        sumsin=ssin*abar
 
        ff=(fdph/dsqrt(fd1ph))*
     &     cdexp(xl2sign*cs.sqrtm1*cs.pi*0.25d0)
        ff=redu*ff*(sumcos+sumsin*cs.sqrtm1)
        if(fd1ph.lt.small_fd1ph)ff=0.d0

c         write(6,*)ra.oe.w,fd1pha,fd1phb
c         write(6,*)ra.rf.yp,ra.rf.zp
c         write(6,*)' '

c--------------------------------
        endif ! inorm2.eq. 4, 40, 41, 42
c---------------------------------------------------------------------------
        endif

c---------------------------------------------------------------------------
         if((ifl.inorm2.eq.43).or.
     &      (ifl.inorm2.eq.44).or.
     &      (ifl.inorm2.eq.45))then
c--------------------------------
c       Konvergenzkriterien in diesem Modus, maximale Werte sind:
c       nk=165, convlim=5.1d0   
c------------- get nk
        if(dabs(bbar).lt.2.d0)nk=15
        if((dabs(bbar).ge.2.d0).and.(dabs(bbar).lt.3.d0))nk=60
        if((dabs(bbar).ge.3.d0).and.(dabs(bbar).lt.4.d0))nk=100
        if(dabs(bbar).ge.4.d0)nk=165
        if(dabs(bbar).gt.convlim)then

c------------- only quadratic terms
          fd1ph=fd1ph_sav
          fd1phb=fd1phb_sav
          ff=cdexp(xw2sign*cs.sqrtm1*cs.pi*0.25d0)*
     &       cdexp(xl2sign*cs.sqrtm1*cs.pi*0.25d0)*
     &       (fdph/(g.xlam*dsqrt(dabs(fd1ph*fd1phb))))/2.d0
         if(ifl.inorm2.eq.45)ff=0.d0
         if(fd1ph.lt.small_fd1ph)ff=0.d0

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

c-- test
c  es kommt das gleiche raus!
c        if(bbar.lt.0.d0)then
c          bbar1=-bbar
c          else
c          bbar1=bbar
c        endif
c        call spa_3rd_order(bbar1,ssin,scos,blp)
c        if(bbar.lt.0.d0)ssin=-ssin
c        scos=scos*abar
c        ssin=ssin*abar        
c        write(6,*)' cos ',sumcos,scos
c        write(6,*)' sin ',sumsin,ssin
c--- test 
c        sumcos=scos*abar
c        sumsin=ssin*abar

        ff=(fdph/dsqrt(fd1ph))*
     &      cdexp(xl2sign*cs.sqrtm1*cs.pi*0.25d0)
        ff=ff*(sumcos+sumsin*cs.sqrtm1)
        if(ifl.inorm2.eq.44)ff=0.d0
        if(fd1ph.lt.small_fd1ph)ff=0.d0
c---------------------------------------------------------------------------
        endif   ! inorm2.eq.43,44, 45
c---------------------------------------------------------------------------
        endif

c---------------------------------------
        endif  ! inorm2.eq.4 or 44
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

	endif   ! isrctype.eq.4 or 5 or 7

c--------------- ist zu checken
	if(isrctype.eq.6)then
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
	record /integration_results/ xir
	record /integration/ xi
c UF        record /control_flags/ ifl
        record /psimagest/ sp

	dimension dx(4096),x(4096),
     &            amp(4096),pha(4096),pha_new(4096),
     &		  d1(4096),d2(4096),d12(4096),
     &		  istart(4096),iend(4096)


	dimension x_fit(1024),amp_fit(1024),pha_fit(1024)
c UF	dimension coef(1024),coef1(1024),xx(1024)
        dimension coef(1024),xx(1024)

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

c UF	dimension work(number)
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
c UF	subroutine simpson(cs,ifl,xi,xir,ianz,dyz,fyz,fyza,fyzp,xint,
c take out unused fyz
       subroutine simpson(cs,ifl,xi,xir,ianz,dyz,fyza,fyzp,xint,
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
        record /integration_results/ xir
        record /integration/ xi
        record /control_flags/ ifl

c UF	complex*16 xint,fyz(4096),f1,f2,xxx(4096)
        complex*16 xint,xxx(4096)
	dimension xxxre(4096),xxxim(4096)

	real*8 fyza_loc(4096),fyzp_loc(4096)

	real*8 x_loc(MAX_INTEGRATION_SIZE),dyz_loc(MAX_INTEGRATION_SIZE) 
        real*8 COEF(MAX_INTEGRATION_SIZE),WORK1(MAX_INTEGRATION_SIZE),
     &       WORK2(MAX_INTEGRATION_SIZE),WORK3(MAX_INTEGRATION_SIZE),
     &       WORK4(MAX_INTEGRATION_SIZE)
         
C UF	real*4 xxr(4096),ffyza(4096),ffyzp(4096)

c	dimension fyza_loc(4096),fyzp_loc(4096)
c	dimension x_loc(4096),dyz_loc(4096)
c	dimension xxr(4096),ffyza(4096),ffyzp(4096)

	dimension partre_int(4096),partim_int(4096)

	dimension dyz(4096)
c uf	dimension x(4096),y(4096)
        dimension x(4096)
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
	if((ifl.ispline.eq.1).or.(ifl.ispline.eq.2))then
c------- spline integtration von Michael

         x_loc(1)=0.d0
         do i=2,ianz
         x_loc(i)=x_loc(i-1)+dyz(1)
         enddo

         call UTIL_SPLINE_INTEGRAL(x_loc,densiyzre,xintre,
     &          COEF,WORK1,WORK2,WORK3,WORK4,ianz,ifl.ispline)

         call UTIL_SPLINE_INTEGRAL(x_loc,densiyzim,xintim,
     &          COEF,WORK1,WORK2,WORK3,WORK4,ianz,ifl.ispline)

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

c----------------
	endif    ! ispline.eq.1

	return
	end
	
*CMZ : 00.00/15 12/10/2013  12.22.24  by  Michael Scheer
*CMZ : 00.00/07 02/05/2008  13.10.35  by  Michael Scheer
*CMZ : 00.00/02 17/08/2004  09.47.26  by  Michael Scheer
*CMZ : 00.00/00 10/01/95  15.25.29  by  Michael Scheer
*-- Author :
      SUBROUTINE UTIL_SPLINE_INTEGRAL(X,Y,resul,
     &       COEF,WORK1,WORK2,WORK3,WORK4,N,ispline2)

C---  CALCULATES INTERGRAL OF Y(X) VIA SPLINES

      IMPLICIT NONE

      INTEGER I,N,ispline2
      REAL*8 X(*),Y(*),resul
      REAL*8 COEF(*),WORK1(*),WORK2(*),WORK3(*),WORK4(*)

C---  SPLINE-COEFFICIENTS

      CALL UTIL_SPLINE_COEF(X,Y,9999.0d0,9999.0d0,COEF,
     &     WORK1,WORK2,WORK3,WORK4,N,ispline2)

C--- INTEGRATION

      resul=0.0D0

      DO I=1,N-1

      resul=resul
     &          +(X(I+1)-X(I))*0.5D0
     &          *(Y(I)+Y(I+1))
     &          -(X(I+1)-X(I))**3/24.D0
     &          *(COEF(I)+COEF(I+1))

      ENDDO

      RETURN
      END


*CMZ : 00.00/15 03/09/2012  09.27.48  by  Michael scheer
*CMZ : 00.00/07 12/10/2009  12.17.45  by  Michael Scheer
*CMZ : 00.00/02 14/04/2003  12.46.09  by  Michael Scheer
*CMZ : 00.00/00 10/01/95  15.27.48  by  Michael Scheer
*-- Author :
      SUBROUTINE UTIL_SPLINE_COEF(X,Y,YP1,YPN,Y2,AA,BB,CC,C,N,ispline2)

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


      IMPLICIT NONE

      INTEGER N,J,ispline2
      REAL*8  X(*),Y(*),Y2(*),AA(*),BB(*),CC(*),C(*)

      REAL*8 YP1,YPN

      double precision xx(3),yy(3),a(3),yp(3),xopt,yopt
      INTEGER ifail

c     save

      IF (N.LT.3) then
        if (abs(yp1).eq.9999.0d0) then
          y2(1)=0.0d0
        else
          y2(1)=yp1
        endif
        if (abs(ypn).eq.9999.0d0) then
          y2(n)=0.0d0
        else
          y2(n)=ypn
        endif
        RETURN
      endif

      if (abs(yp1).eq.9999.0d0) then
        xx=x(1:3)
        yy=y(1:3)
        call UTIL_PARABEL(xx,yy,A,YP,XOPT,yopt,IFAIL,ispline2)
        if (ifail.eq.0) then
          y2(1)=2.0d0*a(3)
        else
          y2(1)=0.0d0
        endif
      else
        Y2(1)=YP1
      endif

      if (abs(ypn).eq.9999.0d0) then
        xx=x(n-2:n)
        yy=y(n-2:n)
        call UTIL_PARABEL(xx,yy,A,YP,XOPT,yopt,IFAIL,ispline2)
        if (ifail.eq.0) then
          y2(n)=2.0d0*a(3)
        else
          y2(N)=0.0d0
        endif
      else
        Y2(N)=YPN
      endif

      C(1)=Y2(1)
      C(N)=y2(n)

      BB(1)=1.D0
      CC(1)=0.D0
      CC(N)=1.D0

      DO J=2,N-1
        if(x(j+1).eq.x(j)) then
          write(6,*)
          write(6,*)'*** Error in util_spline_coef: 
     & Intervall of zero length'
          write(6,*)'j, x(j), x(j+1):',j,x(j),x(j+1)
          write(6,*)
          stop
        endif
          AA(J)=(X(J  )-X(J-1))/6.D0
          BB(J)=(X(J+1)-X(J-1))/3.D0
          CC(J)=(X(J+1)-X(J  ))/6.D0
          C(J)=(Y(J+1)-Y(J  ))/(X(J+1)-X(J  ))
     &          -(Y(J  )-Y(J-1))/(X(J  )-X(J-1))
      ENDDO !J

      DO J=2,N-1

          BB(J)=BB(J)-AA(J)*CC(J-1)
           C(J)= C(J)-AA(J)* C(J-1)
C          AA(J)=AA(J)-AA(J)*BB(J-1)

          CC(J)=CC(J)/BB(J)
           C(J)= C(J)/BB(J)
          BB(J)=1.D0

      ENDDO !J

      DO J=N-1,2,-1
         Y2(J)=C(J)-CC(J)*Y2(J+1)
      ENDDO

      RETURN
      END
*CMZ : 00.00/15 03/09/2012  09.27.13  by  Michael Scheer
*CMZ : 00.00/07 22/03/2010  15.28.00  by  Michael Scheer
*CMZ : 00.00/02 26/03/97  10.23.11  by  Michael Scheer
*CMZ : 00.00/00 10/01/95  15.27.40  by  Michael Scheer
*-- Author :
      SUBROUTINE UTIL_PARABEL(Xin,Yin,A,YP,XOPT,yopt,IFAIL,ispline2)

C--- CALCULATES A(1),A(2),A(3), THE DERIVATIVES YP(X(1)),YP(X(2)),YP(X(3)),
C    AND THE EXTREMUM (XOPT,A(XOPT)) OF PARABOLA Y=A1+A2*X+A3*X**2
C    FROM COORDINATES OF THE THREE POINTS (X(1),Y(1)),(X(2),Y(2)),(X(3),Y(3))
C

      IMPLICIT NONE

      INTEGER IFAIL,ispline2

      REAL*8 A(3),X(3),Y(3),DXM,DXP,x0,a1,a2,dxm2,dxp2,dxmax,dymax,
     &  xin(3),yin(3)
      REAL*8 DET,YP(3),XOPT,yopt,a22,fm,fp,f0

c      save

      IFAIL=0

c calculate f=a0+a1*(x-x0)+a2*(x-x0)**2
c  = a0 + a1*x - a0*x0 + a2*x**2 - 2*a2*x*x0 + a2*x0**2
c  = a0 + (a2*x0 -a0)*x0 + (a1 - 2*a2*x0 )*x+ a2*x**2

c change system: (x0,s0)->(0,0), i.e.
c calculate f=a1*dx+a2*dx**2
c  df/dx=a1+2*a2*dx_max =! 0, dx_max=-a1/2/a2

      x=xin
      y=yin

c nur rufen, falls x-Werte nicht monoton steigen
      if(ispline2.eq.2)call util_sort_func(3,x,y)

      x0=x(2)
      f0=y(2)

      fm=y(1)-f0
      fp=y(3)-f0

      dxm=x(1)-x0
      dxp=x(3)-x0

c fm=a1*dxm+a2*dxm**2
c fp=a1*dxp+a2*dxp**2

c (dxm dxm2) (a1) = (y(1))
c (dxp dxp2) (a2) = (y(3))

      dxm2=dxm*dxm
      dxp2=dxp*dxp

      det=dxm*dxp2-dxp*dxm2

      if (det.ne.0.0d0) then
        a1=(fm*dxp2-fp*dxm2)/det
        a2=(fp*dxm-fm*dxp)/det
      else
        ifail=1
        return
      endif

      if (a2.ne.0.0d0) then
        dxmax=-a1/(2.0d0*a2)
        dymax=(a1+a2*dxmax)*dxmax
        xopt=x0+dxmax
        yopt=f0+dymax
      endif

c calculate f=f0+a1*dx+a2*dx**2
c = a1*x - a1*x0 + a2*x**2 + a2*x0**2 - 2*a2*x*x0
c  f = f0 + (a2*x0 -a1)*x0 + (a1 - 2*a2*x0 )*x+ a2*x**2

      a22=2.0d0*a2

      a(1)=f0 + (a2*x0 -a1)*x0
      a(2)=a1 - a22*x0
      a(3)=a2

c calculate yp=a1+2*a2*dx

      yp(1)=a1+a22*dxm
      yp(2)=a1
      yp(3)=a1+a22*dxp

      RETURN
      END
*CMZ : 00.00/15 05/01/2012  13.52.39  by  Michael Scheer
*CMZ : 00.00/00 11/01/95  11.41.04  by  Michael Scheer
*-- Author :
      SUBROUTINE UTIL_SORT_FUNC(N,RA,YA)

C--- HEAPSORT ROUTINE; SEE NUMERICAL RECIPES 8.2 S 231
C--- ARRAY YA IS FUNCTION OF RA AND SORTED ACCORDINGLY

      IMPLICIT NONE

      INTEGER N,L,IR,I,J

      REAL*8 RA(N),RRA
      REAL*8 YA(N),YYA

      if (n.lt.2) return

      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
          YYA=YA(L)
        ELSE
          RRA=RA(IR)
          YYA=YA(IR)
          RA(IR)=RA(1)
          YA(IR)=YA(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            YA(1)=YYA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(RA(J).LT.RA(J+1))J=J+1
          ENDIF
          IF(RRA.LT.RA(J))THEN
            RA(I)=RA(J)
            YA(I)=YA(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
        YA(I)=YYA
      GO TO 10
      END




