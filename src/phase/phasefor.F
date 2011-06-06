c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/phasefor.for
c$$$ Date      : <06 Feb 04 08:50:29 flechsig> 
c$$$ Time-stamp: <06 Jun 11 16:57:11 flechsig> 
c$$$ Author    : Uwe Flechsig, flechsig@psi.ch
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

c File      : /home/pss060/sls/flechsig/phase/src/phase/phasefor.for
c Date      : <06 Jan 00 13:18:08 flechsig> 
c Time-stamp: <06 Feb 04 08:50:29 flechsig> 
c Author    : Flechsig Uwe OVGA/203a 4535, flechsig@psi.ch
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c einige routinen aus fgmap3dpp
c function   facult(nn) 
c subroutine fgmapidp(iord,imodus,acc,a,g,wc,xlc,ypc1,zpc1,dypc,dzpc)       
c subroutine intersection(a,wc,xlc,rin,iord,uu,ww,xll) 
c subroutine intersection_8(am,wc,xlc,rin,iord,uu,ww,xll) 
c subroutine pathlen0(am,g,iord,iplmode,wc,xlc,ypc1,zpc1,xlm)     
c subroutine pathlen1(xlm,rin,iord,
c     &               xlength1,xlength2,xlength)
c subroutine readmatrixfile35(fname,xmap35)  
c subroutine readintersection(fname,wc,xlc,iord) 
c subroutine extractmap35(xmap35,ypc1,zpc1,dypc,dzpc,iord)  
c subroutine reduce_1(imodus,iord,a,g,acc,wc,xlc,ypc1,zpc1,dypc,dzpc)    
c subroutine reduce_1_drift(iord,a,g,acc,wc,xlc,ypc1,zpc1,dypc,dzpc)
c subroutine transponiere (a,g)   
c subroutine transponiere_8 (am,g)
c subroutine ray_tracef(rin, rout, iord,ypc1,zpc1,dypc,dzpc)       
c subroutine writemap(mapname,iord,ypc1,zpc1,dypc,dzpc)  
c subroutine xxmap35(xmap35,ypc1,zpc1,dypc,dzpc)  
c subroutine geometrd(gname, g)  
c subroutine mirror4d(ename, a)
c subroutine adaptive_int_old(yzint,s0)
c subroutine int_2d(f,dy,ny,dz,nz,xint)	
c subroutine fdet(iord,fdetc,ypc1,zpc1,dypc,dzpc)      
c subroutine sig(i,j,k,l,isig)   
c subroutine simpson(ianz,dyz,fyz,xint)   
c subroutine psdi(dens,phase) 
c subroutine fywert()
c subroutine psdi_gh(yp,zp,dyp,dzp,dens,s0)  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c UF 0804 routine pstf ausgelagert in separate file
 
c*********************************************************
	function facult(nn) 
c*********************************************************
        integer nn,i
	real*8 facult

	facult=1.
	if(nn.gt.1)then
	  do i=1,nn
	     facult=facult*float(i)
	  enddo
	endif
	return
	end 
c*********** end facult()*********************************   

c**********************************************************
c	initialisieren der Konstanten
c	U. Flechsig 21.1.97
c----------------------------------------------------------
	subroutine initconstants(cs)
ccc        include 'phase_struct.for'
c a trick to have the struct only in the ./phase and find it from opti 
c if it works under vms is not yet tested
#ifdef VMS
        include '[-.phase]phase_struct.for'
c        include 'phase_struct.for'
#else
cc        include '../phase/phase_struct.for'
#include '../phase/phase_struct.F'
#endif

        complex*16 xm1
	record /constants/ cs
	
	xm1=-1.
	cs.pi=4.d0*datan(1.d0)
        cs.sqrtm1=cdsqrt(xm1)
	return
	end 
c*********** end initconstants ****************************

c*************************** neu **************************
 	subroutine fgmapidp(iord,imodus,acc,a,g,
     &             wc,xlc,ypc1,zpc1,dypc,dzpc)       
c*************************** neu **************************  
c       acc ist epsilon
c 10.10.96 umbenannt und iord eingefuegt

 	implicit real*8(a-h,o-z) 
        integer iord
       	structure/geometryst/
           real*8 sina,cosa,sinb,cosb,
     &            r,rp,xdens(0:4),xlam
	   integer idefl   
        end structure   
        record /geometryst/ g   
        dimension a(0:5,0:5)              ! mirror     

        dimension wc(0:4,0:4,0:4,0:4),
     &            xlc(0:4,0:4,0:4,0:4),  
     &            ypc1(0:4,0:4,0:4,0:4),
     &            zpc1(0:4,0:4,0:4,0:4),
     &            dypc(0:4,0:4,0:4,0:4),
     &            dzpc(0:4,0:4,0:4,0:4)    

c----------------------------------------------------------       
c----------------------------------------------------------
c
c	imodus = 1 : ray tracing von der Quelle zum Bild
c	imodus = 2 : ray tracing vom Bild zur Quelle
c       imodus = 5 : Beamline Optimierung
c
c----------------------------------------------------------
c        write(*,*)'fortran gerufen'
#ifdef DEBUG
	 write(6,*)' fgmapidp: imodus = ',imodus
#endif
	 idrift=0
         if(imodus.ge.1000)then
           imodus=imodus-1000
           idrift=1
           endif

c        write(*,*)'for: fgmapidp called'
	if(imodus.eq.2)then
#ifdef DEBUG
          write(*,*)'fgmapidp: make map for image to source iord=',iord 
#endif
          call transponiere(a,g)    ! bild zur quelle
        else 
#ifdef DEBUG
          write(*,*)'fgmapidp: make map for source to image iord=',iord 
#endif
	endif 
c------------------------------------------------------- 
        if(idrift.eq.0)then
	  call reduce_1(imodus,iord,a,g,acc,wc,xlc,ypc1,zpc1,dypc,dzpc)
	endif
	if(idrift.eq.1)then
	  call reduce_1_drift(imodus,iord,a,g,acc,wc,xlc,ypc1,zpc1,dypc,dzpc)
	endif
c------------------------------------------------------- 
        if(imodus.eq.2)then
          call transponiere(a,g)    ! bild zur quelle
	  do n1=0,iord
	   do n2=0,iord
	    do n3=0,iord
	     do n4=0,iord
	      ypc1(n1,n2,n3,n4)=((-1)**(n2+n3))*
     &                          ypc1(n1,n2,n3,n4)
	      zpc1(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                          zpc1(n1,n2,n3,n4)
	      dypc(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                          dypc(n1,n2,n3,n4)
	      dzpc(n1,n2,n3,n4)=((-1)**(n2+n3))*
     &                          dzpc(n1,n2,n3,n4)
	      wc(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                        wc(n1,n2,n3,n4)
	      xlc(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                         xlc(n1,n2,n3,n4)
            
	     enddo
	    enddo
	   enddo
	  enddo
	endif

c	write(6,*)g.cosa,g.cosb,g.sina,g.sinb

c        write(*,*)'fgmapidp (wc, xlc results):'
c       do i=0,4
c          do j=0,4-i
c             do k=0,4-i-j
c                do l=0,4-i-j-k
c                   write(*,*)'wc ',i,j,k,l,wc(i,j,k,l)
c                   write(*,*)'xlc',i,j,k,l,xlc(i,j,k,l)
c                enddo
c             enddo
c          enddo
c       enddo
c       write(*,*)'end fgmapidp'

	return
        end
c-------- end fgmapidp()-----------------------------------

c**************************************************************
	subroutine intersection(a,wc,xlc,rin,uu,ww,xll,iord)   
c
c 	intersection returns uu,ww,xll
c**************************************************************
	implicit real*8(a-h,o-z)

        dimension a(0:5,0:5)         ! mirror
	real*8 yi_global,zi_global,dyi_global,dzi_global,rin(4)
        dimension wc(0:4,0:4,0:4,0:4),
     &            xlc(0:4,0:4,0:4,0:4)

	yi_global=rin(1) 
        zi_global=rin(2) 
        dyi_global=rin(3)     
        dzi_global=rin(4)   
        
	uu=0.d0
	ww=0.d0
	xll=0.d0
c        write(*,*)'intersection called'   

        do i=0,iord
         do j=0,iord
          do k=0,iord
           do l=0,iord
            if((i+j+k+l).le.iord)then
	     deltaww=wc(i,j,k,l)
	     deltall=xlc(i,j,k,l)
             if(abs(yi_global).gt.1e-10)then
	      deltaww=deltaww*yi_global**i
	      deltall=deltall*yi_global**i
	     else
	      if(i.gt.0)then
	       deltaww=0.d0
	       deltall=0.d0
	      endif
	     endif
             if(abs(zi_global).gt.1e-10)then
	      deltaww=deltaww*zi_global**j
	      deltall=deltall*zi_global**j
	     else
	      if(j.gt.0)then
	       deltaww=0.d0
	       deltall=0.d0
              endif
	     endif
             if(abs(dyi_global).gt.1e-10)then
	      deltaww=deltaww*dyi_global**k
	      deltall=deltall*dyi_global**k
	     else
	      if(k.gt.0)then
	       deltaww=0.d0
	       deltall=0.d0
              endif
	     endif
             if(abs(dzi_global).gt.1e-10)then
	      deltaww=deltaww*dzi_global**l
	      deltall=deltall*dzi_global**l
	     else
	      if(l.gt.0)then
	       deltaww=0.d0
	       deltall=0.d0
              endif
	     endif
             ww=ww+deltaww
	     xll=xll+deltall
            endif
           enddo
          enddo
         enddo
        enddo
c********************************************************
	do i=0,iord+1
	 do j=0,iord+1
          deltauu=a(i,j)
          if(abs(ww).gt.1e-10)then
	   deltauu=deltauu*ww**i
          else
	   if(i.gt.0)then
            deltauu=0.d0
	   endif
          endif
          if(abs(xll).gt.1e-10)then
           deltauu=deltauu*xll**j
	  else
	   if(j.gt.0)then
            deltauu=0.d0
	   endif
          endif
	  uu=uu+deltauu
         enddo
	enddo
c        write(*,*)'intersection end'   

	return
	end
c*************** end intersection ****************************
                          
c**************************************************************
	subroutine intersection_8(a,wc,xlc,rin,uu,ww,xll,iord)   
c
c 	intersection returns uu,ww,xll
c**************************************************************
	implicit real*8(a-h,o-z)

        dimension a(0:8,0:8)         ! mirror
	real*8 yi_global,zi_global,dyi_global,dzi_global,rin(4)
        dimension wc(0:7,0:7,0:7,0:7),
     &            xlc(0:7,0:7,0:7,0:7)

	yi_global=rin(1) 
        zi_global=rin(2) 
        dyi_global=rin(3)     
        dzi_global=rin(4)   
        
	uu=0.d0
	ww=0.d0
	xll=0.d0
c        write(*,*)'intersection called'   

        do i=0,iord
         do j=0,iord
          do k=0,iord
           do l=0,iord
            if((i+j+k+l).le.iord)then
	     deltaww=wc(i,j,k,l)
	     deltall=xlc(i,j,k,l)
             if(abs(yi_global).gt.1e-10)then
	      deltaww=deltaww*yi_global**i
	      deltall=deltall*yi_global**i
	     else
	      if(i.gt.0)then
	       deltaww=0.d0
	       deltall=0.d0
	      endif
	     endif
             if(abs(zi_global).gt.1e-10)then
	      deltaww=deltaww*zi_global**j
	      deltall=deltall*zi_global**j
	     else
	      if(j.gt.0)then
	       deltaww=0.d0
	       deltall=0.d0
              endif
	     endif
             if(abs(dyi_global).gt.1e-10)then
	      deltaww=deltaww*dyi_global**k
	      deltall=deltall*dyi_global**k
	     else
	      if(k.gt.0)then
	       deltaww=0.d0
	       deltall=0.d0
              endif
	     endif
             if(abs(dzi_global).gt.1e-10)then
	      deltaww=deltaww*dzi_global**l
	      deltall=deltall*dzi_global**l
	     else
	      if(l.gt.0)then
	       deltaww=0.d0
	       deltall=0.d0
              endif
	     endif
             ww=ww+deltaww
	     xll=xll+deltall
            endif
           enddo
          enddo
         enddo
        enddo
c********************************************************
	do i=0,iord+1
	 do j=0,iord+1
          deltauu=a(i,j)
          if(abs(ww).gt.1e-10)then
	   deltauu=deltauu*ww**i
          else
	   if(i.gt.0)then
            deltauu=0.d0
	   endif
          endif
          if(abs(xll).gt.1e-10)then
           deltauu=deltauu*xll**j
	  else
	   if(j.gt.0)then
            deltauu=0.d0
	   endif
          endif
	  uu=uu+deltauu
         enddo
	enddo
c        write(*,*)'intersection end'   

	return
	end
c*************** end intersection_8 ****************************
                          
c*************************************************************
	subroutine pathlen0(am,g,iord,iplmode,isti,wc,xlc,ypc1,zpc1,xlm)
c*************************************************************
c	(ini = 0 : calculate the transformation coefficients)
c*************************************************************
c	Berechnung der Transformationskoeffizienten der 
c	Pfadlaenge fuer ein opt. Element
c-------------------------------------------------------------
c       input: am: mirrortype
c	       g : geometrytype
c	iplmode = 0 : do not subtract r and rp from path length
c		      in analytical code
c	iplmode = 1 : subtract r and rp from path length
c		      in analytical code   
c	isti    = 1  : Tarnsformation von Quelle zum Bild
c	isti    != 1 : Tarnsformation von Bild zur Quelle
c	       wc,xlc,ypc1,zpc1: Entwicklungskoeffizienten
c       return: xlm: xlen1c, xlen2c (Koeffizientenstruktur)   
c**************************************************************
c	J. Bahrdt  xx.xx.9x
c	Stand:     30.1.97	UF
c last modification: 26 Jun 97 08:29:17 flechsig
c**************************************************************

        implicit real*8(a-h,o-z)
#include '../phase/phase_struct.F'

c---------- Typen ---------------------------------------------
       	
c	structure/geometryst/
c           real*8 sina,cosa,sinb,cosb,
c     &            r,rp,xdens(0:4),xlam
c	   integer idefl   
c        end structure 
c	
c	structure/xlenmap/                 ! Entw. Koeffizienten
c	   real*8 xlen1c(0:4,0:4,0:4,0:4), ! der Pfadlaenge
c     &  	  xlen2c(0:4,0:4,0:4,0:4)
c	end structure      

        dimension wc(0:4,0:4,0:4,0:4),
     &            xlc(0:4,0:4,0:4,0:4),
     &            ypc1(0:4,0:4,0:4,0:4),
     &            zpc1(0:4,0:4,0:4,0:4)  
     
        dimension dpl0(0:4,0:4,0:4,0:4,0:4,0:4),
     &            dsqrl(0:4,0:4,0:4,0:4,0:4,0:4),
     &		  dsqrpl2(0:4,0:4,0:4,0:4,0:4,0:4),
     &            dsqrpl1(0:4,0:4,0:4,0:4,0:4,0:4)
        dimension dql1(0:4,0:4,0:4,0:4,0:4,0:4),
     &            dql2(0:4,0:4,0:4,0:4,0:4,0:4)
        dimension xlen1cc(0:4,0:4,0:4,0:4)
        dimension xlen2cc(0:4,0:4,0:4,0:4)

c--------- Variablen ------------------------------------------   

	record /xlenmap/ xlm     
        record /geometryst/ g 
	record /mirrortype/ am

	integer iord,n1,n2,n3,n4,n5,n6,iplmode,isti  ! ,i,j,k,l

c*****************************************************************
#ifdef DEBUG
       write(*,*) 'pathlen0: iplmode=',iplmode
#endif

      if (iplmode.eq.0)then
#ifdef DEBUG
        write(*,*) 'pathlen0: do not subtract r and rp from path length' 
#endif
	if(isti.eq.1)then
	  call lsubb(am.a,g.sinb,g.cosb,g.sina,g.cosa,g.rp,g.r,-g.idefl,dql1,dql2)
	endif
	if(isti.ne.1)then
	  call lsubb(am.a,g.sina,g.cosa,g.sinb,g.cosb,g.r,g.rp,g.idefl,dql1,dql2) 
	endif
	
        do n1=0,iord
	  do n2=0,iord
	   do n3=0,iord
	    do n4=0,iord
	     do n5=0,iord
	      do n6=0,iord
	       if((n1+n2+n3+n4+n5+n6).le.iord)then
                dpl0(n1,n2,n3,n4,n5,n6)=dql1(n1,n2,n3,n4,n5,n6)
c	write(*,*)' dql1 ',dql1(n1,n2,n3,n4,n5,n6)
               endif
	      enddo
	     enddo
	    enddo
	   enddo
	  enddo
	enddo
        call lsuba(dpl0,dsqrl)
        do n1=0,iord
	  do n2=0,iord
	   do n3=0,iord
	    do n4=0,iord
	     if((n1+n2+n3+n4).le.iord)then
              xlen1cc(n1,n2,n3,n4)=dsqrl(n1,n2,n3,n4,0,0)
c	write(*,*)n1,n2,n3,n4,xlen1cc(n1,n2,n3,n4)
             endif
	    enddo
	   enddo
	  enddo
	enddo
c----------------------------
	do n1=0,iord
	  do n2=0,iord
	   do n3=0,iord
	    do n4=0,iord
	     do n5=0,iord
	      do n6=0,iord
	       if((n1+n2+n3+n4+n5+n6).le.iord)then
                dpl0(n1,n2,n3,n4,n5,n6)=dql2(n1,n2,n3,n4,n5,n6)
c	write(*,*)' dql2 ',dql2(n1,n2,n3,n4,n5,n6)
               endif
	      enddo
	     enddo
	    enddo
	   enddo
	  enddo
	enddo
        call lsuba(dpl0,dsqrl)
        do n1=0,iord
	  do n2=0,iord
	   do n5=0,iord
	    do n6=0,iord
	     if((n1+n2+n5+n6).le.iord)then
              xlen2cc(n1,n2,n5,n6)=dsqrl(n1,n2,0,0,n5,n6)
c	write(*,*)n1,n2,n5,n6,xlen2cc(n1,n2,n5,n6)
             endif
	    enddo
	   enddo
	  enddo
	enddo
c----------------------------------
       endif 	! iplmode = 0
c----------------------------------
       if (iplmode.eq.1)then
#ifdef DEBUG
	write(*,*)'pathlen0: subtract r and rp from path length'
#endif
	if(isti.eq.1)call lsubab(am.a,g.sinb,g.cosb,g.sina,g.cosa,
     &              g.rp,g.r,-g.idefl,dsqrpl1,dsqrpl2)
	if(isti.ne.1)call lsubab(am.a,g.sina,g.cosa,g.sinb,g.cosb,
     &              g.r,g.rp,g.idefl,dsqrpl1,dsqrpl2)
	do n1=0,iord
         do n2=0,iord
	  do n3=0,iord
	   do n4=0,iord
	    if((n1+n2+n3+n4).le.iord)then
             xlen1cc(n1,n2,n3,n4)=dsqrpl1(n1,n2,n3,n4,0,0)
	    endif
	   enddo
	  enddo
	 enddo
	enddo
	do n1=0,iord
         do n2=0,iord
	  do n5=0,iord
	   do n6=0,iord
	    if((n1+n2+n5+n6).le.iord)then
             xlen2cc(n1,n2,n5,n6)=dsqrpl2(n1,n2,0,0,n5,n6)
	    endif
	   enddo
	  enddo
	 enddo
	enddo
       endif	! iplmode = 1
c----------------------------------
       call lsubc(wc,xlc,xlen1cc,xlm.xlen1c)
       call lsubd00(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)
       call lsubd01(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)
       call lsubd02(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)
       call lsubd10(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)
       call lsubd11(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)
       call lsubd12(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)
       call lsubd2(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)
       call lsubd3(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)
    
c       write(*,*)'Sum of pathlen coefficients'
	do n1=0,4
          do n2=0,4-n1
             do n3=0,4-n1-n2
                do n4=0,4-n3-n2-n1
                enddo
             enddo
          enddo
       enddo
c       write(*,*)'end pathlen0'
       
       return
       end
c************ end pathlen0 ***********************************

c*************************************************************
	subroutine pathlen1(xlm,rin,iord,
     &                      xlength1,xlength2,xlength)
c*************************************************************
c	Berechnung der Pfadlaenge aus gegebenen Koeffizienten
c       xlm: xlen1c, xlen2c (Koeffizientenstruktur)
c-------------------------------------------------------------
c       input: image Koordinaten als raytype : rin
c       return: xlength1, xlength2, xlength,
c**************************************************************
c	J. Bahrdt  xx.xx.9x
c	Stand:     18.10.96	UF
c**************************************************************

        implicit real*8(a-h,o-z)
        integer iord,i,j,k,l

#include '../phase/phase_struct.F'      
	
c---------- Typen ---------------------------------------------
c       	structure/raytype/                 ! ein Phasenraumpunkt
c	   real*8 y,z,dy,dz
c	end structure
c
c	structure/xlenmap/                 ! Entw. Koeffizienten
c	   real*8 xlen1c(0:4,0:4,0:4,0:4), ! der Pfadlaenge
c     &  	  xlen2c(0:4,0:4,0:4,0:4)
c	end structure
c--------- Variablen ------------------------------------------   

	record /ray_i/ rin
	record /xlenmap/ xlm     

       	dimension yi(0:7),zi(0:7),dyi(0:7),dzi(0:7)
       
c*****************************************************************

        yi(0)=1.d0
	zi(0)=1.d0
	dyi(0)=1.d0
	dzi(0)=1.d0
	do i=1,iord
	   yi(i)=yi(i-1)*rin.yi
	   zi(i)=zi(i-1)*rin.zi
	   dyi(i)=dyi(i-1)*rin.dyi
	   dzi(i)=dzi(i-1)*rin.dzi
	enddo
c--------------------------------------------------
        xlength1=0.d0
	xlength2=0.d0
	do i=0,iord
          do j=0,iord-i
           do k=0,iord-i-j
            do l=0,iord-i-j-k
	     xlength1=xlength1+xlm.xlen1c(i,j,k,l)*
     &            yi(i)*zi(j)*dyi(k)*dzi(l)
	     xlength2=xlength2+xlm.xlen2c(i,j,k,l)*
     &            yi(i)*zi(j)*dyi(k)*dzi(l)
            enddo
	   enddo
	  enddo
	enddo
        xlength=xlength1+xlength2
        
	return
	end
c************ end pathlen1 ********************************                          
                          
c******* read matrixfile ************************************ 
	subroutine readmatrixfile(fname,xmap70,iord)  
c UF 6.6.2011 xmap70 ist ein pointer auf MAP70TYPE d.h. hat unterschiedliche dimension bei seven order
c------------------------------------------------------------
c       located in fgmap3dpp.for,phasefor.for      
c************************************************************    
        implicit none

        character*255  	fname
        real*8 xmap70
        integer i,j,idim,ii,jj,iord
#ifdef SEVEN_ORDER
	dimension xmap70(330,330)
#else
        dimension xmap70(70,70)
#endif        
c UF 6.6.2011 andere ordnungen sollten auch noch beruecksichtigt werden
        if(iord.eq.4)then
          idim=70
        else 
	   if (iord.eq.3)then
	      idim=35
	   else
              idim=330
	   endif
	endif

        open(unit=10,file=fname,status='old',err=556) 
	do i=1,idim
	   do j=1,idim
	      read(10,*)ii,jj,xmap70(i,j)
	   enddo
	enddo
	close(10)     

        return
 556    write(*,*)'readmatrixfile: error open ', fname
        return
        end
c******* read matrixfile ************************************ 

c****************************************************************
 	subroutine readintersection(fname,wc,xlc,iord) 
c****************************************************************
        implicit real*8(a-h,o-z)  
        character*255 fname
        dimension wc(0:4,0:4,0:4,0:4),
     &            xlc(0:4,0:4,0:4,0:4)
        
	open(unit=10,file=fname,status='old',err=558)
        write(*,*)'****read intersection from: ',fname     
        do i=0,iord
	 do j=0,iord-i
	  do k=0,iord-i-j
           do l=0,iord-i-j-k
             read(10,*)wc(i,j,k,l)
             read(10,*)xlc(i,j,k,l)
           enddo
          enddo
	 enddo
	enddo
	close(10)  
      	return
 558    write(*,*)'readintersection: error open file: ', fname
        return
	end
c********* end readintersection() ***************************************

c*************************************************************** 
	subroutine extractmap(xmap70,ypc1,zpc1,dypc,dzpc,iord)  
c       Uwe 12.2.97 umgeschrieben auf universelle map
c---------------------------------------------------------------      
	implicit real*8(a-h,o-z)

        integer iord,iy,iz,idy,idz,i,j,k,l,nn
#ifdef SEVEN_ORDER
        dimension xmap70(330,330),
     &            ypc1(0:8,0:8,0:8,0:8),
     &            zpc1(0:8,0:8,0:8,0:8),
     &            dypc(0:8,0:8,0:8,0:8),
     &            dzpc(0:8,0:8,0:8,0:8)
#else
        dimension xmap70(70,70),
     &            ypc1(0:4,0:4,0:4,0:4),
     &            zpc1(0:4,0:4,0:4,0:4),
     &            dypc(0:4,0:4,0:4,0:4),
     &            dzpc(0:4,0:4,0:4,0:4)
#endif

        if(iord.eq.4)then
         iy=36
         iz=16
         idy=6
         idz=2
        else 
	   if(iord.eq.3)then
          ! iord=3
	      iy=21
	      iz=11
	      idy=5
	      idz=2
	   else
cccc UF 6.6.2011 seven order correction not yet done
	     iy=36
             iz=16
             idy=6
            idz=2 
	   endif
        endif   

        nn=0
        do i=0,iord
          do j=0,iord-i
            do k=0,iord-i-j
              do l=0,iord-i-j-k
		 nn=nn+1
         	 ypc1(i,j,k,l)=xmap70(iy,nn)
         	 zpc1(i,j,k,l)=xmap70(iz,nn)
         	 dypc(i,j,k,l)=xmap70(idy,nn)
         	 dzpc(i,j,k,l)=xmap70(idz,nn)    
              enddo
            enddo        
          enddo
        enddo        

        return
        end
c******* extractmap ************************************     


c********************************************************** 
	subroutine reduce_1_drift(imodus,iord,a,g,acc,wc,
     &	xlc,ypc1,zpc1,dypc,dzpc)    
c********************************************************** 
        implicit real*8(a-h,o-z) 
c       mirror a und geometry g gehen herein
c       map7 und map8 gehen heraus
c 	wird von fgmapidp genutzt
c----------------------------------------------------------
        structure/geometryst/
           real*8 sina,cosa,sinb,cosb,
     &            r,rp,xdens(0:4),xlam
	   integer idefl   
        end structure
        record /geometryst/ g  
        dimension a(0:5,0:5)

      	dimension   dq1(0:4,0:4,0:4,0:4,0:4,0:4),
     &              dq2(0:4,0:4,0:4,0:4,0:4,0:4),
     &              dq3(0:4,0:4,0:4,0:4,0:4,0:4),
     &              dq4(0:4,0:4,0:4,0:4,0:4,0:4),
     &              dq5(0:4,0:4,0:4,0:4,0:4,0:4),
     &              dq6(0:4,0:4,0:4,0:4,0:4,0:4)    
     	
        dimension   f2c1(0:4,0:4,0:4,0:4,0:4,0:4),
     &              f2c2(0:4,0:4,0:4,0:4,0:4,0:4),
     &              f2c3(0:4,0:4,0:4,0:4,0:4,0:4),
     &              f2c4(0:4,0:4,0:4,0:4,0:4,0:4),
     &              g2c(0:4,0:4,0:4,0:4,0:4,0:4)

	dimension dsqrq1(0:4,0:4,0:4,0:4,0:4,0:4),    
     &            dsqrq2(0:4,0:4,0:4,0:4,0:4,0:4),
     &  	  g2cc(0:4,0:4,0:4,0:4,0:4,0:4),
     &		  density(0:4,0:4,0:4,0:4,0:4,0:4),   
     &		  fak(0:4)
        dimension   eq1c(0:4,0:4,0:4,0:4,0:4,0:4),
     &              eq2c(0:4,0:4,0:4,0:4,0:4,0:4)
        dimension   ypc(0:4,0:4,0:4,0:4),
     &              zpc(0:4,0:4,0:4,0:4)
        dimension   dq7(0:4,0:4,0:4,0:4),
     &              dq8(0:4,0:4,0:4,0:4),
     &              dq9(0:4,0:4,0:4,0:4),
     &              dqa(0:4,0:4,0:4,0:4),
     &              dqb(0:4,0:4,0:4,0:4),
     &              dqc(0:4,0:4,0:4,0:4),
     &              dqd(0:4,0:4,0:4,0:4),
     &              dqe(0:4,0:4,0:4,0:4)
        dimension   dyic(0:4,0:4,0:4,0:4),
     &              dzic(0:4,0:4,0:4,0:4)
        dimension   wc(0:4,0:4,0:4,0:4),
     &              xlc(0:4,0:4,0:4,0:4),
     &              ypc1(0:4,0:4,0:4,0:4),
     &              zpc1(0:4,0:4,0:4,0:4)
        dimension   dyp1c(0:4,0:4,0:4,0:4),
     &              dzp1c(0:4,0:4,0:4,0:4),
     &              dypc(0:4,0:4,0:4,0:4),
     &              dzpc(0:4,0:4,0:4,0:4)
        dimension dqk(0:4,0:4,0:4,0:4)
        dimension dyp2c(0:4,0:4,0:4,0:4,0:4,0:4),
     &            dzp2c(0:4,0:4,0:4,0:4,0:4,0:4)

        drift=g.r+g.rp
	do n1=0,4
	 do n2=0,4
	  do n3=0,4
	   do n4=0,4
	    ypc1(n1,n2,n3,n4)=0.d0
	    zpc1(n1,n2,n3,n4)=0.d0
            dypc(n1,n2,n3,n4)=0.d0
            dzpc(n1,n2,n3,n4)=0.d0
	    wc(n1,n2,n3,n4)=0.d0
	    xlc(n1,n2,n3,n4)=0.d0
	   enddo
	  enddo
	 enddo
        enddo

        ypc1(0,1,0,0)=1.d0
        ypc1(0,0,0,1)=drift
		
	wc(0,1,0,0)=1.d0
	wc(0,0,0,1)=g.r
				
	zpc1(1,0,0,0)=1.d0
	zpc1(0,0,1,0)=drift
						
	xlc(1,0,0,0)=1.d0
	xlc(0,0,1,0)=g.r
								
	dypc(0,0,0,1)=1.d0
									
        dzpc(0,0,1,0)=1.d0
 
        return
	end
		    
c********************************************************** 
	subroutine reduce_1(imodus,iord,a,g,acc,wc,xlc,ypc1,zpc1,dypc,dzpc)    
c********************************************************** 
        implicit real*8(a-h,o-z) 
c       mirror a und geometry g gehen herein
c       map7 und map8 gehen heraus
c 	wird von fgmapidp genutzt
c----------------------------------------------------------
        structure/geometryst/
           real*8 sina,cosa,sinb,cosb,
     &            r,rp,xdens(0:4),xlam
	   integer idefl   
        end structure
        record /geometryst/ g  
        dimension a(0:5,0:5)

      	dimension   dq1(0:4,0:4,0:4,0:4,0:4,0:4),
     &              dq2(0:4,0:4,0:4,0:4,0:4,0:4),
     &              dq3(0:4,0:4,0:4,0:4,0:4,0:4),
     &              dq4(0:4,0:4,0:4,0:4,0:4,0:4),
     &              dq5(0:4,0:4,0:4,0:4,0:4,0:4),
     &              dq6(0:4,0:4,0:4,0:4,0:4,0:4)    
     	
        dimension   f2c1(0:4,0:4,0:4,0:4,0:4,0:4),
     &              f2c2(0:4,0:4,0:4,0:4,0:4,0:4),
     &              f2c3(0:4,0:4,0:4,0:4,0:4,0:4),
     &              f2c4(0:4,0:4,0:4,0:4,0:4,0:4),
     &              g2c(0:4,0:4,0:4,0:4,0:4,0:4)

	dimension dsqrq1(0:4,0:4,0:4,0:4,0:4,0:4),    
     &            dsqrq2(0:4,0:4,0:4,0:4,0:4,0:4),
     &  	  g2cc(0:4,0:4,0:4,0:4,0:4,0:4),
     &		  density(0:4,0:4,0:4,0:4,0:4,0:4),   
     &		  fak(0:4)
        dimension   eq1c(0:4,0:4,0:4,0:4,0:4,0:4),
     &              eq2c(0:4,0:4,0:4,0:4,0:4,0:4)
        dimension   ypc(0:4,0:4,0:4,0:4),
     &              zpc(0:4,0:4,0:4,0:4)
        dimension   dq7(0:4,0:4,0:4,0:4),
     &              dq8(0:4,0:4,0:4,0:4),
     &              dq9(0:4,0:4,0:4,0:4),
     &              dqa(0:4,0:4,0:4,0:4),
     &              dqb(0:4,0:4,0:4,0:4),
     &              dqc(0:4,0:4,0:4,0:4),
     &              dqd(0:4,0:4,0:4,0:4),
     &              dqe(0:4,0:4,0:4,0:4)
        dimension   dyic(0:4,0:4,0:4,0:4),
     &              dzic(0:4,0:4,0:4,0:4)
        dimension   wc(0:4,0:4,0:4,0:4),
     &              xlc(0:4,0:4,0:4,0:4),
     &              ypc1(0:4,0:4,0:4,0:4),
     &              zpc1(0:4,0:4,0:4,0:4)
        dimension   dyp1c(0:4,0:4,0:4,0:4),
     &              dzp1c(0:4,0:4,0:4,0:4),
     &              dypc(0:4,0:4,0:4,0:4),
     &              dzpc(0:4,0:4,0:4,0:4)
        dimension dqk(0:4,0:4,0:4,0:4)
        dimension dyp2c(0:4,0:4,0:4,0:4,0:4,0:4),
     &            dzp2c(0:4,0:4,0:4,0:4,0:4,0:4)
	
c----------------- initialisieren -----------------
#ifdef DEBUG
        write(*,*)'reduce_1: epsilon:',acc
#endif
        if(acc.lt.1e-8)then
           write(*,*)'reduce_1: epsilon < 1e-8'
           write(*,*)'reduce_1: put epsilon to default'
           acc=0.01
           write(*,*)'reduce_1: (new) epsilon:',acc
        endif

        fak(0)=1.d0
	fak(1)=1.d0
	fak(2)=2.d0
	fak(3)=6.d0
	fak(4)=24.d0 

	do n1=0,4
	 do n2=0,4
	  do n3=0,4
	   do n4=0,4
	    do n5=0,4
	     do n6=0,4
		density(n1,n2,n3,n4,n5,n6)=0.d0
	     enddo
	    enddo
	   enddo
	  enddo
	 enddo
	enddo

c-------------------------------------------------------------------
c Reduce Rechnungen

        call subc(a,
     &            g.sina,g.cosa,g.sinb,g.cosb,g.r,g.rp,g.idefl,
     &            dq1,dq2,dq3,dq4,dq5,dq6)  	!wdfgmapc3   
        call subb(dq2,dsqrq1)      		!wdfgmapb3  
        call suba(dsqrq1,dq3,f2c1) 		!wdfgmapa3 
        call suba(dsqrq1,dq5,f2c3) 		!wdfgmapa3 
        call subb(dq1,dsqrq1)
        call suba(dsqrq1,dq4,f2c2)
        call suba(dsqrq1,dq6,f2c4)
        call subb(dq1,dsqrq1)
        call subb(dq2,dsqrq2)         		!wdfgmapb3  
        call suba(dsqrq1,dsqrq2,g2cc) 		!wdfgmapa3 

c--------------- line density ------------------------------------
c-------- produziere density aus xdens(5) ------------------------      
 
        do n1=0,4
	   density(n1,0,0,0,0,0)=g.xdens(n1)
	enddo     

c----------- 18.1.2000
	if(imodus.eq.2)then
	  density(1,0,0,0,0,0)=-density(1,0,0,0,0,0)
	  density(3,0,0,0,0,0)=-density(3,0,0,0,0,0)
	endif
c----------- end 18.1.2000

        do n1=0,iord
	 do n2=0,iord
	  do n3=0,iord
	   do n4=0,iord
	    do n5=0,iord
	     do n6=0,iord
	   g2cc(n1,n2,n3,n4,n5,n6)=g2cc(n1,n2,n3,n4,n5,n6)*
     &	         fak(n1)*fak(n2)*fak(n3)*fak(n4)*fak(n5)*fak(n6)
	   density(n1,n2,n3,n4,n5,n6)=density(n1,n2,n3,n4,n5,n6)*
     &	         fak(n1)*fak(n2)*fak(n3)*fak(n4)*fak(n5)*fak(n6)
	     enddo
	    enddo
	   enddo
	  enddo
	 enddo
	enddo

        call suba(density,g2cc,g2c) !wdfgmapa3 
c-------------------------------------------------------

        call subd1(g.xlam,
     &             f2c1,f2c2,f2c3,f2c4,g2c,
     &		   eq1c,eq2c)  			!wdfgmap14
        call subd3(acc,eq1c,eq2c,ypc,zpc)       !wdfgmapd34
        call subd4(acc,eq1c,eq2c,ypc,zpc)
        call subd5(acc,eq1c,eq2c,ypc,zpc)
        call subf(a,
     &            g.sina,g.cosa,g.sinb,g.cosb,g.r,g.rp,g.idefl,
     &            dq7,dq8,dq9,dqa,dqb,dqc,dqd,dqe)
        call sube2(dq8,dqk)
        call sube1(dq7,dqk,dyic)
        call sube2(dqa,dqk)
        call sube1(dq9,dqk,dzic)
        call subg(acc,dyic,dzic,wc,xlc)
        call subh(ypc,zpc,wc,xlc,ypc1,zpc1)
        call sube2(dqc,dqk)
        call sube1(dqb,dqk,dyp1c)
        call sube2(dqe,dqk)
        call sube1(dqd,dqk,dzp1c)
        call subi(dyp1c,dyp2c,wc,xlc)
        call subk1(dyp2c,dypc,ypc1,zpc1)
        call subk2(dyp2c,dypc,ypc1,zpc1)
        call subi(dzp1c,dzp2c,wc,xlc)
        call subk1(dzp2c,dzpc,ypc1,zpc1)
        call subk2(dzp2c,dzpc,ypc1,zpc1)
c        write(*,*)'end reduce_1'
	return
	end
c ***************** end reduce_1 **************************

c********************************************************** 
	subroutine transponiere (a,g)   
c----------------------------------------------------------
c       "transponiert" die Geometriedaten und Spiegel- 
c	koeffizienten fuer Rechnung      Bild-> Quelle
c
c	ACHTUNG: Vorzeichen von xdens(1) und xdens(3)
c	werden lokal in reduce_1 umgedreht
c
c********************************************************** 
	implicit real*8(a-h,o-z)    
	structure/geometryst/
           real*8 sina,cosa,sinb,cosb,
     &            r,rp,xdens(0:4),xlam  
	   integer idefl   
        end structure
        record /geometryst/ g  
        dimension a(0:5,0:5)    

        do i=0,5
          do j=0,5
              a(i,j)=((-1)**(i+j)) * a(i,j)
          enddo
        enddo  

	xxx=g.rp
	g.rp=g.r
	g.r=xxx
	g.xlam=-g.xlam
	xxx=g.cosa
	g.cosa=g.cosb
	g.cosb=xxx
	xxx=g.sina
	g.sina=-g.sinb
	g.sinb=-xxx
	return
        end
c******* end transponiere  ********************************** 

c********************************************************** 
	subroutine transponiere_8(am,g)   
c
c	wird gerufen von fgmapidp_8
c
c----------------------------------------------------------
c       "transponiert" die Geometriedaten und Spiegel- 
c	koeffizienten fuer Rechnung      Bild-> Quelle
c
c	ACHTUNG: Vorzeichen von xdens(1) und xdens(3)
c	werden lokal in reduce_1 umgedreht
c
c********************************************************** 
	implicit real*8(a-h,o-z)
#include '../phase/phase_struct.F'
c	structure/geometryst/
c           real*8 sina,cosa,sinb,cosb,
c     &            r,rp,xdens(0:4),xlam  
c	   integer idefl   
c        end structure
        record /geometryst/ g  
        record /mirrortype/ am  
c        dimension a(0:5,0:5)    

        do i=0,8
          do j=0,8
              am.a(i,j)=((-1)**(i+j)) * am.a(i,j)
          enddo
        enddo  

	xxx=g.rp
	g.rp=g.r
	g.r=xxx
	g.xlam=-g.xlam
	xxx=g.cosa
	g.cosa=g.cosb
	g.cosb=xxx
	xxx=g.sina
	g.sina=-g.sinb
	g.sinb=-xxx
	return
        end
c******* end transponiere_8  ********************************** 

c*********************************************************
	subroutine ray_tracef(rin, rout, iord,
     &                        ypc1,zpc1,dypc,dzpc)       
c*********************************************************
        IMPLICIT REAL*8(A-H,O-Z)

        dimension ypc1(0:4,0:4,0:4,0:4),
     &            zpc1(0:4,0:4,0:4,0:4),
     &            dypc(0:4,0:4,0:4,0:4),
     &            dzpc(0:4,0:4,0:4,0:4)
                  
        real*8 rin(4), rout(4)

         yp=0.
         zp=0.
         dyp=0.
         dzp=0.
c         type*,'rtfor'
         yi=rin(1) 
         zi=rin(2) 
         dyi=rin(3)     
         dzi=rin(4)     

c         type*,'for:',yi,zi,dyi,dzi
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
                 dyp=dyp+dypc(i,j,k,l)*var3
                 dzp=dzp+dzpc(i,j,k,l)*var3
		 var3=var3*dzi
               enddo
               var2=var2*dyi
             enddo
             var1=var1*zi
           enddo
           var0=var0*yi
         enddo

        rout(1)=yp 
        rout(2)=zp 
        rout(3)=dyp 
        rout(4)=dzp 

	return
	end
c****** end rtracef ********************************************

c****************************************************************
 	subroutine writemap(mapname,iord,ypc1,zpc1,dypc,dzpc)  
c UF 6.6.2011 hier werden MAP7TYPE arrays uebergeben
c****************************************************************
        implicit real*8(a-h,o-z)  
        character*255 mapname
#ifdef SEVEN_ORDER
	dimension   ypc1(0:8,0:8,0:8,0:8),
     &              zpc1(0:8,0:8,0:8,0:8),
     &              dypc(0:8,0:8,0:8,0:8),
     &              dzpc(0:8,0:8,0:8,0:8) 
#else
        dimension   ypc1(0:4,0:4,0:4,0:4),
     &              zpc1(0:4,0:4,0:4,0:4),
     &              dypc(0:4,0:4,0:4,0:4),
     &              dzpc(0:4,0:4,0:4,0:4)   
#endif
	open(unit=10,file=mapname,status='new',err=559)
        write(*,*)'****write transformation map to: ',mapname  
	write(10,*)'writemap: ypc, zpc, dypc, dzpc'
        do i=0,iord
         do j=0,iord
          do k=0,iord
           do l=0,iord
            if((i+j+k+l).le.iord)then
             write(10,*)i,j,k,l
             write(10,*)ypc1(i,j,k,l), zpc1(i,j,k,l)
             write(10,*)dypc(i,j,k,l), dzpc(i,j,k,l)
            endif
           enddo
          enddo        
         enddo
        enddo        
        close(10)
	return
 559    write(*,*)'writemap: error open file', mapname
        return
	end
c********* end writemap() ***************************************

C Datei: USERDISK_3:[FLECHSIG.PHASE.PHASEFOR]MIRROR4DP.FOR
C Datum: 19.JUL.1994
C Stand: 12-APR-1996
C Autor: FLECHSIG, BESSY Berlin

c***************************************************** 
	subroutine mirror4d(ename, a)
c***************************************************** 
        implicit real*8(a-h,o-z)
        dimension a(0:5,0:5)   
	character*255 ename
c----------------------------------------------------------
       	write(*,*) '***read opt. element data from: ',ename 
        open(unit=10,file=ename,status='old',err=666)  
        do i=0,5
          do j=0,5
            if((i+j).le.5)then
              read(10,*)a(i,j)
            endif
          enddo
        enddo
        close(10)
        return
 666    write(*,*)'mirror4dp: error open file',ename 
        end
c*********** end mirror4dp.for ***************************

C Datei: USERDISK_3:[FLECHSIG.PHASE.PHASEFOR]GEOMETRDP.FOR
C Datum: 19.JUL.1994
C Stand: 12-APR-1996
C Autor: FLECHSIG, BESSY Berlin

c***************************************************** 
 	subroutine geometrd(gname, g)  
c*****************************************************
        implicit real*8(a-h,o-z)
       	structure/geometryst/
           real*8 sina,cosa,sinb,cosb,
     &            r,rp,xdens(0:4),xlam
	   integer idefl   
        end structure
        record /geometryst/ g   
    	character*255  	gname
c----------------------------------------------------------
        pi= 4.d0*datan(1.d0)

        open(unit=10,file=gname,status='old',err=667)  
        write(*,*) '***read Geometry data from: ',gname
        read(10,*)alpha
        read(10,*)beta   
	read(10,*)g.r
        read(10,*)g.rp    
	do i=0,4
             read(10,*)g.xdens(i)
	enddo
        read(10,*)g.xlam
	read(10,*)g.idefl
        close(10) 
        goto 668
 667    write(*,*)'geometrd: error open file ', gname
 668    continue
        alpha=alpha*(pi/180.d0)
        beta=beta*(pi/180.d0)
        g.xlam=g.xlam*1.d-6
        
        g.sina=dsin(alpha)
        g.cosa=dcos(alpha)
        g.sinb=dsin(beta)
        g.cosb=dcos(beta)

        return
        end
c*********** end geometrdp.for ***************************     

c**********************************************************
      subroutine xxmap35(xmap35,ypc1,zpc1,dypc,dzpc)  
c**********************************************************
c     Berechnung der vollstaendigen 35x35 Matrix
c     zur Transformation 3. Ordnung
c**********************************************************
	implicit real*8(a-h,o-z)

      	dimension   ypc1(0:4,0:4,0:4,0:4),
     &              zpc1(0:4,0:4,0:4,0:4),
     &              dypc(0:4,0:4,0:4,0:4),
     &              dzpc(0:4,0:4,0:4,0:4),   
     & 		    xmap35(35,35)
ccccccccccccccccccc alt cccccccccccccccccccccccccccccc
c        common/map7/wc(0:4,0:4,0:4,0:4),
c     &              xlc(0:4,0:4,0:4,0:4),
c        common/map8/dyp1c(0:4,0:4,0:4,0:4),
c     &              dzp1c(0:4,0:4,0:4,0:4), 
c        common/map35/   
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension cc(0:4,0:4,0:4,0:4,35)
      dimension p1(0:4,0:4,0:4,0:4)
      dimension p2(0:4,0:4,0:4,0:4)
      dimension p11(0:4,0:4,0:4,0:4)
      dimension ispalte(0:4,0:4,0:4,0:4)
      dimension facul(0:4,0:4,0:4,0:4)	

      write(*,*)'iord=3fest'
      iord=3
      inum=35
      isube1=1
     
      ianz=0
      do n1=0,iord
       do n2=0,iord-n1
        do n3=0,iord-n1-n2
         do n4=0,iord-n1-n2-n3
          facul(n1,n2,n3,n4)=
     &           facult(n1)*facult(n2)*facult(n3)*facult(n4)	
          ianz=ianz+1
          ispalte(n1,n2,n3,n4)=ianz
         enddo
        enddo
       enddo
      enddo

c***************************************************************
c             Potenzen von dzp
c***************************************************************

      do n1=0,iord
       do n2=0,iord-n1
        do n3=0,iord-n1-n2
         do n4=0,iord-n1-n2-n3
          cc(0,0,0,1,ispalte(n1,n2,n3,n4))=dzpc(n1,n2,n3,n4)
         enddo
        enddo
       enddo
      enddo

      do l4=2,iord
       do n1=0,iord
        do n2=0,iord-n1
         do n3=0,iord-n1-n2
          do n4=0,iord-n1-n2-n3
           p1(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &          cc(0,0,0,1,ispalte(n1,n2,n3,n4))
           p2(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &          cc(0,0,0,l4-1,ispalte(n1,n2,n3,n4))
          enddo
         enddo
        enddo
       enddo
       call sube1(p1,p2,p11)
       do n1=0,iord
        do n2=0,iord-n1
         do n3=0,iord-n1-n2
          do n4=0,iord-n1-n2-n3
           cc(0,0,0,l4,ispalte(n1,n2,n3,n4))=p11(n1,n2,n3,n4)
          enddo
         enddo
        enddo
       enddo
      enddo

c***********************************************************
c        Potenzen von dyp und dzp
c***********************************************************
      do n1=0,iord
       do n2=0,iord-n1
        do n3=0,iord-n1-n2
         do n4=0,iord-n1-n2-n3
          cc(0,0,1,0,ispalte(n1,n2,n3,n4))=dypc(n1,n2,n3,n4)
         enddo
        enddo
       enddo
      enddo

      do l3=1,iord
       do l4=0,iord-l3
        if((l3+l4).gt.1)then
         do n1=0,iord
          do n2=0,iord-n1
           do n3=0,iord-n1-n2
            do n4=0,iord-n1-n2-n3
             p1(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &           cc(0,0,1,0,ispalte(n1,n2,n3,n4))
             p2(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &           cc(0,0,l3-1,l4,ispalte(n1,n2,n3,n4))
            enddo
           enddo
          enddo
         enddo
         call sube1(p1,p2,p11)
         do n1=0,iord
          do n2=0,iord-n1
           do n3=0,iord-n1-n2
            do n4=0,iord-n1-n2-n3
             cc(0,0,l3,l4,ispalte(n1,n2,n3,n4))=p11(n1,n2,n3,n4)
            enddo
           enddo
          enddo
         enddo
        endif
       enddo
      enddo

c***********************************************************
c        Potenzen von zp, dyp und dzp
c***********************************************************

      do n1=0,iord
       do n2=0,iord-n1
        do n3=0,iord-n1-n2
         do n4=0,iord-n1-n2-n3
          cc(0,1,0,0,ispalte(n1,n2,n3,n4))=zpc1(n1,n2,n3,n4)
         enddo
        enddo
       enddo
      enddo

      do l2=1,iord
       do l3=0,iord-l2
        do l4=0,iord-l2-l3
         if((l2+l3+l4).gt.1)then
          do n1=0,iord
           do n2=0,iord-n1
            do n3=0,iord-n1-n2
             do n4=0,iord-n1-n2-n3
              p1(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &          cc(0,1,0,0,ispalte(n1,n2,n3,n4))
              p2(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &          cc(0,l2-1,l3,l4,ispalte(n1,n2,n3,n4))
             enddo
            enddo
           enddo
          enddo
          call sube1(p1,p2,p11)
          do n1=0,iord
           do n2=0,iord-n1
            do n3=0,iord-n1-n2
             do n4=0,iord-n1-n2-n3
              cc(0,l2,l3,l4,ispalte(n1,n2,n3,n4))=p11(n1,n2,n3,n4)
             enddo
            enddo
           enddo
          enddo
         endif
        enddo
       enddo
      enddo

c***********************************************************
c        Potenzen von yp, zp, dyp und dzp
c***********************************************************
      do n1=0,iord
       do n2=0,iord-n1
        do n3=0,iord-n1-n2
         do n4=0,iord-n1-n2-n3
          cc(1,0,0,0,ispalte(n1,n2,n3,n4))=ypc1(n1,n2,n3,n4)
         enddo
        enddo
       enddo
      enddo

      do l1=1,iord
       do l2=0,iord-l1
        do l3=0,iord-l1-l2
         do l4=0,iord-l1-l2-l3
          if((l1+l2+l3+l4).gt.1)then
           do n1=0,iord
            do n2=0,iord-n1
             do n3=0,iord-n1-n2
              do n4=0,iord-n1-n2-n3
               p1(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &                 cc(1,0,0,0,ispalte(n1,n2,n3,n4))
               p2(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &                 cc(l1-1,l2,l3,l4,ispalte(n1,n2,n3,n4))
              enddo
             enddo
            enddo
           enddo
           call sube1(p1,p2,p11)
           do n1=0,iord
            do n2=0,iord-n1
             do n3=0,iord-n1-n2
              do n4=0,iord-n1-n2-n3
               cc(l1,l2,l3,l4,ispalte(n1,n2,n3,n4))=p11(n1,n2,n3,n4)
              enddo
             enddo
            enddo
           enddo
          endif
         enddo
        enddo
       enddo
      enddo

c*************************************************************

      izeile=1
      xmap35(izeile,1)=1.
      do i=2,inum
        xmap35(izeile,i)=0.
      enddo

      do l1=0,iord                ! Zeilennummerierung
       do l2=0,iord-l1
        do l3=0,iord-l1-l2
         do l4=0,iord-l1-l2-l3
          if((l1+l2+l3+l4).gt.0)then
           izeile=izeile+1
           do nn=1,35                 ! Spaltennummerierung
            xmap35(izeile,nn)=cc(l1,l2,l3,l4,nn)
           enddo
          endif
         enddo
        enddo
       enddo
      enddo
      return
      end
c**** end xxmap35() ***************************************     


c**********************************************************
      subroutine xxmap70(xmap70,ypc1,zpc1,dypc,dzpc,iord)
c**********************************************************
c     Berechnung der vollstaendigen 70x70 Matrix
c     zur Transformation 4. Ordnung
c**********************************************************
c	implicit real*8(a-h,o-z)
c     Uwe 12.2.97

        implicit none

        real*8 xmap70,ypc1,zpc1,dypc,dzpc,cc,p1,p2,p11,facul,
     .         facult
        integer ispalte,iord,inum,isube1,ianz,n1,n2,n3,n4,
     .          nn,i,izeile,l1,l2,l3,l4

      	dimension   ypc1(0:4,0:4,0:4,0:4),
     &              zpc1(0:4,0:4,0:4,0:4),
     &              dypc(0:4,0:4,0:4,0:4),
     &              dzpc(0:4,0:4,0:4,0:4),   
     & 		    xmap70(70,70)

      dimension cc(0:4,0:4,0:4,0:4,70)
      dimension p1(0:4,0:4,0:4,0:4)
      dimension p2(0:4,0:4,0:4,0:4)
      dimension p11(0:4,0:4,0:4,0:4)
      dimension ispalte(0:4,0:4,0:4,0:4)
      dimension facul(0:4,0:4,0:4,0:4)	

c einziger Unterschied xxmap70, xxmap35
      if(iord.eq.4)then
       inum=70
      else
       inum=35
      endif
      
      isube1=1
      ianz=0
      do n1=0,iord
       do n2=0,iord-n1
        do n3=0,iord-n1-n2
         do n4=0,iord-n1-n2-n3
          facul(n1,n2,n3,n4)=
     &           facult(n1)*facult(n2)*facult(n3)*facult(n4)	
          ianz=ianz+1
          ispalte(n1,n2,n3,n4)=ianz
c     type*,ianz,facul(n1,n2,n3,n4)
         enddo
        enddo
       enddo
      enddo

c***************************************************************
c             Potenzen von dzp
c***************************************************************

      do n1=0,iord
       do n2=0,iord-n1
        do n3=0,iord-n1-n2
         do n4=0,iord-n1-n2-n3
          cc(0,0,0,1,ispalte(n1,n2,n3,n4))=dzpc(n1,n2,n3,n4)
         enddo
        enddo
       enddo
      enddo

      do l4=2,iord
       do n1=0,iord
        do n2=0,iord-n1
         do n3=0,iord-n1-n2
          do n4=0,iord-n1-n2-n3
           p1(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &          cc(0,0,0,1,ispalte(n1,n2,n3,n4))
           p2(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &          cc(0,0,0,l4-1,ispalte(n1,n2,n3,n4))
c	write(6,*)ispalte(n1,n2,n3,n4),
c     &     p1(n1,n2,n3,n4),p2(n1,n2,n3,n4),
c     &     facul(n1,n2,n3,n4),
c     &     cc(0,0,0,1,ispalte(n1,n2,n3,n4)),
c     &     dzpc(n1,n2,n3,n4)
          enddo
         enddo
        enddo
       enddo

c	type*,isube1,' call sube1'
c	isube1=isube1+1

       call sube1(p1,p2,p11)
       do n1=0,iord
        do n2=0,iord-n1
         do n3=0,iord-n1-n2
          do n4=0,iord-n1-n2-n3
           cc(0,0,0,l4,ispalte(n1,n2,n3,n4))=p11(n1,n2,n3,n4)
          enddo
         enddo
        enddo
       enddo
      enddo

c***********************************************************
c        Potenzen von dyp und dzp
c***********************************************************
      do n1=0,iord
       do n2=0,iord-n1
        do n3=0,iord-n1-n2
         do n4=0,iord-n1-n2-n3
          cc(0,0,1,0,ispalte(n1,n2,n3,n4))=dypc(n1,n2,n3,n4)
         enddo
        enddo
       enddo
      enddo

      do l3=1,iord
       do l4=0,iord-l3
        if((l3+l4).gt.1)then
         do n1=0,iord
          do n2=0,iord-n1
           do n3=0,iord-n1-n2
            do n4=0,iord-n1-n2-n3
             p1(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &           cc(0,0,1,0,ispalte(n1,n2,n3,n4))
             p2(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &           cc(0,0,l3-1,l4,ispalte(n1,n2,n3,n4))
            enddo
           enddo
          enddo
         enddo

c	type*,isube1,' call sube1'
c	isube1=isube1+1

         call sube1(p1,p2,p11)

         do n1=0,iord
          do n2=0,iord-n1
           do n3=0,iord-n1-n2
            do n4=0,iord-n1-n2-n3
             cc(0,0,l3,l4,ispalte(n1,n2,n3,n4))=p11(n1,n2,n3,n4)
            enddo
           enddo
          enddo
         enddo

        endif
       enddo
      enddo

c***********************************************************
c        Potenzen von zp, dyp und dzp
c***********************************************************

      do n1=0,iord
       do n2=0,iord-n1
        do n3=0,iord-n1-n2
         do n4=0,iord-n1-n2-n3
          cc(0,1,0,0,ispalte(n1,n2,n3,n4))=zpc1(n1,n2,n3,n4)
         enddo
        enddo
       enddo
      enddo

      do l2=1,iord
       do l3=0,iord-l2
        do l4=0,iord-l2-l3
         if((l2+l3+l4).gt.1)then
          do n1=0,iord
           do n2=0,iord-n1
            do n3=0,iord-n1-n2
             do n4=0,iord-n1-n2-n3
              p1(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &          cc(0,1,0,0,ispalte(n1,n2,n3,n4))
              p2(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &          cc(0,l2-1,l3,l4,ispalte(n1,n2,n3,n4))
             enddo
            enddo
           enddo
          enddo

c	type*,isube1,' call sube1'
c	isube1=isube1+1

          call sube1(p1,p2,p11)

          do n1=0,iord
           do n2=0,iord-n1
            do n3=0,iord-n1-n2
             do n4=0,iord-n1-n2-n3
              cc(0,l2,l3,l4,ispalte(n1,n2,n3,n4))=p11(n1,n2,n3,n4)
             enddo
            enddo
           enddo
          enddo

         endif
        enddo
       enddo
      enddo

c***********************************************************
c        Potenzen von yp, zp, dyp und dzp
c***********************************************************
      do n1=0,iord
       do n2=0,iord-n1
        do n3=0,iord-n1-n2
         do n4=0,iord-n1-n2-n3
          cc(1,0,0,0,ispalte(n1,n2,n3,n4))=ypc1(n1,n2,n3,n4)
         enddo
        enddo
       enddo
      enddo

      do l1=1,iord
       do l2=0,iord-l1
        do l3=0,iord-l1-l2
         do l4=0,iord-l1-l2-l3
          if((l1+l2+l3+l4).gt.1)then

           do n1=0,iord
            do n2=0,iord-n1
             do n3=0,iord-n1-n2
              do n4=0,iord-n1-n2-n3
               p1(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &                 cc(1,0,0,0,ispalte(n1,n2,n3,n4))
               p2(n1,n2,n3,n4)=facul(n1,n2,n3,n4)*
     &                 cc(l1-1,l2,l3,l4,ispalte(n1,n2,n3,n4))
              enddo
             enddo
            enddo
           enddo

c	type*,isube1,' call sube1'
c	isube1=isube1+1

           call sube1(p1,p2,p11)

           do n1=0,iord
            do n2=0,iord-n1
             do n3=0,iord-n1-n2
              do n4=0,iord-n1-n2-n3
               cc(l1,l2,l3,l4,ispalte(n1,n2,n3,n4))=p11(n1,n2,n3,n4)
              enddo
             enddo
            enddo
           enddo

          endif
         enddo
        enddo
       enddo
      enddo

c*************************************************************

      izeile=1
      xmap70(izeile,1)=1.
      do i=2,inum
       xmap70(izeile,i)=0.
      enddo

      do l1=0,iord                ! Zeilennummerierung
       do l2=0,iord-l1
        do l3=0,iord-l1-l2
         do l4=0,iord-l1-l2-l3
          if((l1+l2+l3+l4).gt.0)then
           izeile=izeile+1
           do nn=1,inum                 ! Spaltennummerierung
            xmap70(izeile,nn)=cc(l1,l2,l3,l4,nn)
           enddo
          endif
         enddo
        enddo
       enddo
      enddo
      return
      end
c**** end xxmap70 ******************************


c*********************************************************
        subroutine fdet(imodus,iord,fdetc,fdetphc,fdet1phc,
     &			ypc1,zpc1,dypc,dzpc)
c	Stand 29.1.97
c*********************************************************
	implicit none
c frage warum zweimal aufrufen

        integer iord,n1,n2,n3,n4,i,j,k,l,isig,imodus
	real*8  ypc1,zpc1,dypc,dzpc,xmec,fdetc,fdetphc,
     &		fdet1phc,p1,p2,p3,p4,p11,p22,fdetcc,
     &		fdetphcc,fdet1phcc

        dimension ypc1(0:4,0:4,0:4,0:4),
     &            zpc1(0:4,0:4,0:4,0:4),
     &            dypc(0:4,0:4,0:4,0:4),
     &            dzpc(0:4,0:4,0:4,0:4)
        dimension xmec(1:4,1:4,0:4,0:4,0:4,0:4)       ! map9
        dimension fdetc(0:4,0:4,0:4,0:4),
     &		  fdetphc(0:4,0:4,0:4,0:4), 
     &  	  fdet1phc(0:4,0:4,0:4,0:4) 
 
        dimension p1(0:4,0:4,0:4,0:4)
        dimension p2(0:4,0:4,0:4,0:4)
        dimension p3(0:4,0:4,0:4,0:4)
        dimension p4(0:4,0:4,0:4,0:4)
        dimension p11(0:4,0:4,0:4,0:4)
        dimension p22(0:4,0:4,0:4,0:4)
        dimension fdetcc(0:4,0:4,0:4,0:4)
        dimension fdetphcc(0:4,0:4,0:4,0:4) 
        dimension fdet1phcc(0:4,0:4,0:4,0:4) 


        call subl(ypc1,zpc1,dypc,dzpc,xmec) 
		       ! Berechnung der Matrixelemente xmec
                       ! Indizes 1 und 2: Reihe, Spalte
                       ! Indizes 3 bis 6: Koeffizienten von
                       ! yp, zp,dyp,dzp
	
        do n1=0,iord
         do n2=0,iord
          do n3=0,iord
           do n4=0,iord
            fdetc(n1,n2,n3,n4)=0.
            p1(n1,n2,n3,n4)=0.d0
            p2(n1,n2,n3,n4)=0.d0
            p3(n1,n2,n3,n4)=0.d0
            p4(n1,n2,n3,n4)=0.d0
	   enddo
          enddo
         enddo
        enddo   

c------------------------------------------------------------------

        if(imodus.eq.1)then
        do i=1,4
        do j=1,4
        if(j.ne.i)then
         do k=1,4
          if((k.ne.i).and.(k.ne.j))then
           do l=1,4
            if((l.ne.i).and.(l.ne.j).and.(l.ne.k))then
             call sig(i,j,k,l,isig)
             do n1=0,3
              do n2=0,3
               do n3=0,3
                do n4=0,3
                 p1(n1,n2,n3,n4)=xmec(1,i,n1,n2,n3,n4)
                 p2(n1,n2,n3,n4)=xmec(2,j,n1,n2,n3,n4)
                 p3(n1,n2,n3,n4)=xmec(3,k,n1,n2,n3,n4)
                 p4(n1,n2,n3,n4)=xmec(4,l,n1,n2,n3,n4)
                enddo
               enddo
              enddo
             enddo
             call p_m_4(p1,p2,p11)
             call p_m_4(p3,p4,p22)
             call p_m_4(p11,p22,fdetcc)
             do n1=0,3
              do n2=0,3
               do n3=0,3
                do n4=0,3
                 fdetc(n1,n2,n3,n4)=fdetc(n1,n2,n3,n4) +
     &           floatj(isig) * fdetcc(n1,n2,n3,n4)
                enddo
               enddo
              enddo
             enddo
            endif
           enddo
          endif
         enddo
        endif
        enddo
        enddo
c---------------- neu 29.1.97 -----------------------------
c---------- functional determinant for 
c---------- amplitude and phase transformation
c---------- ( d yp / d dyi ) * ( d zp / d dzi )  
        do n1=0,iord
         do n2=0,iord
          do n3=0,iord
           do n4=0,iord
            fdetphc(n1,n2,n3,n4)=0.
	   enddo
          enddo
         enddo
        enddo   

        do n1=0,iord
        do n2=0,iord
        do n3=0,iord
        do n4=0,iord
          p1(n1,n2,n3,n4)=xmec(1,3,n1,n2,n3,n4)
	  p2(n1,n2,n3,n4)=xmec(2,4,n1,n2,n3,n4)
	enddo
        enddo
        enddo
        enddo

c       do n1=0,iord
c       do n2=0,iord-n1
c       do n3=0,iord-n1-n2
c       do n4=0,iord-n1-n2-n3
c	write(6,*)n1,n2,n3,n4,p1(n1,n2,n3,n4)
c	write(6,*)p2(n1,n2,n3,n4)
c	enddo
c	enddo
c	enddo
c	enddo

	call p_m_4(p1,p2,fdetphcc)
        do n1=0,iord
        do n2=0,iord
        do n3=0,iord
         do n4=0,iord
          fdetphc(n1,n2,n3,n4)=fdetphc(n1,n2,n3,n4) +
     &         	                 fdetphcc(n1,n2,n3,n4)
         enddo
        enddo
        enddo
        enddo

c       do n1=0,iord
c       do n2=0,iord-n1
c       do n3=0,iord-n1-n2
c       do n4=0,iord-n1-n2-n3
c	write(6,*)n1,n2,n3,n4,fdetphc(n1,n2,n3,n4)
c	enddo
c	enddo
c	enddo
c	enddo

c---------- ( d yp / d dzi ) * ( d zp / d dyi )
      do n1=0,iord
       do n2=0,iord
        do n3=0,iord
         do n4=0,iord
          p1(n1,n2,n3,n4)=xmec(1,4,n1,n2,n3,n4)
          p2(n1,n2,n3,n4)=xmec(2,3,n1,n2,n3,n4)
         enddo
        enddo
       enddo
      enddo
      call p_m_4(p1,p2,fdetphcc)
      do n1=0,iord
       do n2=0,iord
        do n3=0,iord
         do n4=0,iord
          fdetphc(n1,n2,n3,n4)=fdetphc(n1,n2,n3,n4) -
     &         	                 fdetphcc(n1,n2,n3,n4)
         enddo
        enddo
       enddo
      enddo

      endif	! (imodus.eq.1)
       
c-----------------------------------------------------------
      if(imodus.eq.0)then	! downstream
       do n1=0,iord
        do n2=0,iord
         do n3=0,iord
          do n4=0,iord
           fdet1phc(n1,n2,n3,n4)=0.
          enddo
         enddo
        enddo
       enddo
c--------------------------------------------
c---------- functional determinant1 for 
c---------- amplitude and phase transformation

c---------------------------------------------------
c---------- ( d yp / d dyi ) * ( d zp / d dzi )

       do n1=0,iord
        do n2=0,iord
         do n3=0,iord
          do n4=0,iord
           fdet1phc(n1,n2,n3,n4)=0.
          enddo
         enddo
        enddo
       enddo

       do n1=0,iord
        do n2=0,iord
         do n3=0,iord
          do n4=0,iord
           p1(n1,n2,n3,n4)=xmec(1,3,n1,n2,n3,n4)
           p2(n1,n2,n3,n4)=xmec(2,4,n1,n2,n3,n4)
          enddo
         enddo
        enddo
       enddo
       call p_m_4(p1,p2,fdet1phcc)
       do n1=0,iord
        do n2=0,iord
         do n3=0,iord
          do n4=0,iord
           fdet1phc(n1,n2,n3,n4)=fdet1phc(n1,n2,n3,n4) +
     &         	                 fdet1phcc(n1,n2,n3,n4)
          enddo
         enddo
        enddo
       enddo

c---------- ( d yp / d dzi ) * ( d zp / d dyi )

       do n1=0,iord
        do n2=0,iord
         do n3=0,iord
          do n4=0,iord
           p1(n1,n2,n3,n4)=xmec(1,4,n1,n2,n3,n4)
           p2(n1,n2,n3,n4)=xmec(2,3,n1,n2,n3,n4)
         enddo
        enddo
       enddo
      enddo
      call p_m_4(p1,p2,fdet1phcc)
      do n1=0,iord
       do n2=0,iord
        do n3=0,iord
         do n4=0,iord
          fdet1phc(n1,n2,n3,n4)=fdet1phc(n1,n2,n3,n4) -
     &         	                 fdet1phcc(n1,n2,n3,n4)
         enddo
        enddo
       enddo
      enddo
      do n1=0,iord
       do n2=0,iord
       	do n3=0,iord
         do n4=0,iord
	  do k=1,4
	   do l=1,4
c	was soll das
c	    xmec1(k,l,n1,n2,n3,n4)=xmec(k,l,n1,n2,n3,n4)
	   enddo
	  enddo
	 enddo
	enddo
       enddo
      enddo
      endif	! (imodus.eq.0)
c--------------------------------------------

c      do n1=0,iord
c       do n2=0,iord-n1
c        do n3=0,iord-n1-n2
c         do n4=0,iord-n1-n2-n3
c          write(6,*)n1,n2,n3,n4,fdet1phc(n1,n2,n3,n4)
c	  write(6,*)fdetphc(n1,n2,n3,n4)
c         enddo
c        enddo
c       enddo
c      enddo

      return
      end
c********** end fdet **************************************

c**********************************************************
      subroutine sig(i,j,k,l,isig)
c**********************************************************
c*******Zaehlen der Permutationen ***********************
      implicit real*8(a-h,o-z)  

      i1=i
      i2=j
      i3=k
      i4=l
c     write(*,*)' i1,i2,i3,i4 ', i1,i2,i3,i4
      isig=1

c************i1 = 1 ******************************************

      if(i1.ne.1)then
         isig=-isig
         if(i2.eq.1)then
            i2=i1
         endif
         if(i3.eq.1)then
            i3=i1
         endif
         if(i4.eq.1)then
            i4=i1
         endif
         i1=1
      endif
c************i2 = 2 ******************************************

      if(i2.ne.2)then
         isig=-isig
         if(i3.eq.2)then
            i3=i2
         endif
         if(i4.eq.2)then
            i4=i2
         endif
         i2=2
      endif
c************i3 = 3 ******************************************

      if(i3.ne.3)then
         isig=-isig
         i4=i3
         i3=3
      endif
c     write(*,*)' isig ',isig
      return
      end
c******* end sig  ***************************************** 

c*********readfg34_par ******************************
      subroutine readfg34_par(src,apr,ifl,xi,epsilon) 
c liest ein Parameterfile von Johannes, Button im edit- menu
c add epsilon feb 04 

      implicit none
ccc      include 'phase_struct.for'
c a trick to have the struct only in the ./phase and find it from opti 
c if it works under vms is not yet tested
#ifdef VMS
        include '[-.phase]phase_struct.for'
c        include 'phase_struct.for'
#else
cc        include '../phase/phase_struct.for'
#include '../phase/phase_struct.F'
#endif
      
      record /control_flags/ ifl
      record /sources/ src
      record /integration/ xi
      record /apertures/ apr
      character*80 dfile
      character*1 dummy
      integer iname,clen1
      real *8 epsilon

 1    format(1a1)	
 3    format(a80)

      write(*,*)' reading parameters from fg34.par (Version 23.12.99)'
      open(unit=16,name='fg34.par',type='old',err=777)
      read(16,1)dummy
      read(16,*)epsilon
      read(16,*)ifl.iord,ifl.iordsc,ifl.iexpand
      read(16,*)ifl.iplmode
      read(16,*)src.isrctype
      read(16,3)dfile
      iname=clen1(dfile)
      src.so4.fsource4a(:iname)=dfile(:iname)
      read(16,3)dfile
      iname=clen1(dfile)
      src.so4.fsource4b(:iname)=dfile(:iname)
      read(16,3)dfile
      iname=clen1(dfile)
      src.so4.fsource4c(:iname)=dfile(:iname)
      read(16,3)dfile
      iname=clen1(dfile)
      src.so4.fsource4d(:iname)=dfile(:iname)

c-- UF 17.11.08 start
      read(16,*)src.so4.nfreqtot
      read(16,*)src.so4.nfreqpos,src.so4.nfreqneg
      read(16,*)src.so4.nsource,src.so4.nimage
      read(16,*)src.so4.deltatime
      read(16,*)src.so4.iconj
c-- UF 17.11.08 end

      write(6,*)src.so4.nfreqtot

c-- UF 17.11.08 start
      read(16,3)dfile
      iname=clen1(dfile)
      src.so6.fsource6(:iname)=dfile(:iname)
c-- UF 17.11.08 end

c--------pinhole in source plane	
      read(16,*)apr.rpin
      read(16,*)apr.srcymin,apr.srcymax
      read(16,*)apr.srczmin,apr.srczmax

c--------pinhole in aperture plane
      read(16,*)apr.rpin_ap
      read(16,*)apr.ymin_ap,apr.ymax_ap
      read(16,*)apr.zmin_ap,apr.zmax_ap

      read(16,*)src.so5.dipcy
      read(16,*)src.so5.dipcz
      read(16,*)src.so5.dipdisy
      read(16,*)src.so5.dipdisz
      read(16,*)src.so5.dipymin,src.so5.dipymax
      read(16,*)src.so5.dipzmin,src.so5.dipzmax

      read(16,*)ifl.igrating
      read(16,*)ifl.inorm
      read(16,*)ifl.inorm1
      read(16,*)ifl.inorm2
      read(16,*)ifl.matrel

      read(16,*)src.so1.isrcy,src.so1.isrcdy
      read(16,*)src.so1.sigmay,src.so1.sigmayp
      src.so1.sigmayp=src.so1.sigmayp/1000. ! mrad -> rad
      read(16,*)xi.ymin,xi.ymax ! mrad -> rad
      xi.ymin=xi.ymin/1000.
      xi.ymax=xi.ymax/1000.
c      read(16,*)xi.inumy
c      read(16,*)xi.itery0
      read(16,*)xi.ianzy0
c      read(16,*)xi.imaxy
c      read(16,*)xi.fracy
c      read(16,*)xi.frac1y

      read(16,*)src.so1.isrcz,src.so1.isrcdz
      read(16,*)src.so1.sigmaz,src.so1.sigmazp
      src.so1.sigmazp=src.so1.sigmazp/1000. ! mrad -> rad
      read(16,*)xi.zmin,xi.zmax ! mrad -> rad
      xi.zmin=xi.zmin/1000.		
      xi.zmax=xi.zmax/1000.
c      read(16,*)xi.inumz
c      read(16,*)xi.iterz0
      read(16,*)xi.ianzz0
c      read(16,*)xi.imaxz
c      read(16,*)xi.fracz
c      read(16,*)xi.frac1z

      read(16,*)ifl.ibright

      read(16,*)ifl.ispline
      read(16,*)xi.d12_max
      read(16,*)xi.id12
      read(16,*)xi.ianz0_cal
      read(16,*)xi.ianz0_fixed
      read(16,*)xi.iamp_smooth
      read(16,*)xi.iord_amp
      read(16,*)xi.ifm_amp
c      read(16,*)xi.amp_change
      read(16,*)xi.iord_pha
      read(16,*)xi.ifm_pha
c      read(16,*)xi.phase_change_1,xi.phase_change_2
c      read(16,*)xi.iphase_curv
c      read(16,*)xi.iphase_pi2
c      read(16,*)xi.iordap
c      read(16,*)xi.dphi_min

      read(16,*)xi.distfocy,xi.distfocz

      read(16,*)ifl.ipinarr
      read(16,*)src.pin_yl0,src.pin_yl
      read(16,*)src.pin_zl0,src.pin_zl
      close(16)
      return
 777  write(*,*)'readfg34_par: error open file fg34.par '
      return
      end
c**** end readfg34_par *************************************


c*************************************************************
      integer function clen1(str)
c*************************************************************
      character*(*) str
      do i=1,80
         clen1=i
         if( (ichar(str(i:i)).eq.32).or.
     &        (ichar(str(i:i)).eq.10).or.
     &        (ichar(str(i:i)).eq.9) )then
            clen1=clen1-1
            goto 9999
         endif
      enddo
 9999 continue
      return
      end
  
c * end clen1 

c*** end phasefor ***********************************************
                
