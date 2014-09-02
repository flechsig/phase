C Datei: USERDISK_3:[FLECHSIG.PHAS.PHASEFOR.LIB]PHLIB.FOR
C Datum: 19.NOV.1996
C Stand: 19-NOV-1996
C Autor: FLECHSIG, BESSY Berlin

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


c Diese Datei enthaelt Routinen die in die PHASELIB eingehen 
c (alphabetisch geordnet)

c subroutine extractmap35(xmap35,ypc1,zpc1,dypc,dzpc,iord) 
c subroutine extractmap70(xmap70,ypc1,zpc1,dypc,dzpc,iord)
c function   facult(nn)
c subroutine fdet(iord,fdetc,ypc1,zpc1,dypc,dzpc)      
c subroutine fgmapidp(iord,imodus,acc,a,g,wc,xlc,ypc1,zpc1,dypc,dzpc)
c subroutine intersection(a,wc,xlc,rin,iord,uu,ww,xll) 
c subroutine reduce_1(iord,a,g,acc,wc,xlc,ypc1,zpc1,dypc,dzpc)
c subroutine pathlen0(a,g,iord,wc,xlc,ypc1,zpc1,xlm) 
c subroutine sig(i,j,k,l,isig)   
c subroutine transponiere(a,g)
c subroutine xxmap35(xmap35,ypc1,zpc1,dypc,dzpc) 
c subroutine xxmap70(xmap70,ypc1,zpc1,dypc,dzpc)

C
C obsolete ??? UF 5.9.2011
C


c*************************************************************** 
	subroutine extractmap35(xmap35,ypc1,zpc1,dypc,dzpc,iord)  
c---------------------------------------------------------------      
	implicit real*8(a-h,o-z)    

        dimension xmap35(35,35),
     &            ypc1(0:4,0:4,0:4,0:4),
     &            zpc1(0:4,0:4,0:4,0:4),
     &            dypc(0:4,0:4,0:4,0:4),
     &            dzpc(0:4,0:4,0:4,0:4) 

           nn=0
           do i=0,iord
            do j=0,iord-i
             do k=0,iord-i-j
              do l=0,iord-i-j-k
			nn=nn+1
         		ypc1(i,j,k,l)=xmap35(21,nn)
         		zpc1(i,j,k,l)=xmap35(11,nn)
         		dypc(i,j,k,l)=xmap35(5,nn)
         		dzpc(i,j,k,l)=xmap35(2,nn)    ! map7,8
               enddo
              enddo        
             enddo
            enddo        

        return
        end
c******* extractmap35 ************************************     

c*************************************************************** 
	subroutine extractmap70(xmap70,ypc1,zpc1,dypc,dzpc,iord)  
c---------------------------------------------------------------      
	implicit real*8(a-h,o-z)    

        dimension xmap70(70,70),
     &            ypc1(0:4,0:4,0:4,0:4),
     &            zpc1(0:4,0:4,0:4,0:4),
     &            dypc(0:4,0:4,0:4,0:4),
     &            dzpc(0:4,0:4,0:4,0:4) 

	write(*,*)'extractmap70 ist noch nicht fertig!'
           nn=0
           do i=0,iord
            do j=0,iord-i
             do k=0,iord-i-j
              do l=0,iord-i-j-k
			nn=nn+1

	write(*,*)'extractmap70 ist noch nicht fertig!'

         		ypc1(i,j,k,l)=xmap70(21,nn)
         		zpc1(i,j,k,l)=xmap70(11,nn)
         		dypc(i,j,k,l)=xmap70(5,nn)
         		dzpc(i,j,k,l)=xmap70(2,nn)    ! map7,8
               enddo
              enddo        
             enddo
            enddo        

        return
        end
c******* extractmap70 ************************************     

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

c*********************************************************
        subroutine fdet(iord,fdetc,ypc1,zpc1,dypc,dzpc)
c*********************************************************
	implicit real*8(a-h,o-z)

        integer iord
        dimension fdetc(0:4,0:4,0:4,0:4) 
	dimension ypc1(0:4,0:4,0:4,0:4),
     &            zpc1(0:4,0:4,0:4,0:4),
     &            dypc(0:4,0:4,0:4,0:4),
     &            dzpc(0:4,0:4,0:4,0:4)
        
	dimension xmec(1:4,1:4,0:4,0:4,0:4,0:4)       ! map9
        dimension p1(0:4,0:4,0:4,0:4)
        dimension p2(0:4,0:4,0:4,0:4)
        dimension p3(0:4,0:4,0:4,0:4)
        dimension p4(0:4,0:4,0:4,0:4)
        dimension p11(0:4,0:4,0:4,0:4)
        dimension p22(0:4,0:4,0:4,0:4)
        dimension fdetcc(0:4,0:4,0:4,0:4)

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
            enddo
           enddo
          enddo
         enddo
c------------------------------------------------------------------
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
             call sube1(p1,p2,p11)
             call sube1(p3,p4,p22)
             call sube1(p11,p22,fdetcc)
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
      return
      end
c********** end fdet **************************************

c*************************** neu **************************
 	subroutine fgmapidp(iord,imodus,acc,a,g,
     &             wc,xlc,ypc1,zpc1,dypc,dzpc)       
c*************************** neu **************************  
c       acc ist epsilon
c 	10.10.96 umbenannt und iord eingefuegt
c	berechnet die Entwicklungskoeffizienten
c**********************************************************
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
	if(imodus.eq.2)then
          write(*,*)' make map for image to source iord=',iord 
          call transponiere(a,g)    ! bild zur quelle
        else 
          write(*,*)' make map for source to image iord=',iord 
	endif 
c------------------------------------------------------- 
        call reduce_1(iord,a,g,acc,wc,xlc,ypc1,zpc1,dypc,dzpc)
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
        return
        end
c-------- end fgmapidp()-----------------------------------

c**************************************************************
	subroutine intersection(a,wc,xlc,rin,iord,
     &  			uu,ww,xll)   

c 	27.6.96
c 	intersection returns uu,ww,xll
c	berechnet die Durchstosspunkte eines Strahles am OE
c	aus den Entwicklungskoeffizienten
c	rin sollte in der neuen version als raytype uebergeben
c	werden
c	Stand: 19.11.96
c**************************************************************
	implicit real*8(a-h,o-z)

        dimension a(0:5,0:5)         ! mirror
	dimension wc(0:4,0:4,0:4,0:4),
     &            xlc(0:4,0:4,0:4,0:4)

        real*8 yi_global,zi_global,dyi_global,dzi_global,rin(4)

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

c*************************************************************
	subroutine pathlen0(a,g,iord,wc,xlc,ypc1,zpc1,xlm)
c*************************************************************
c	(ini = 0 : calculate the transformation coefficients)
c*************************************************************
c	Berechnung der Transformationskoeffizienten der 
c	Pfadlaenge fuer ein opt. Element
c-------------------------------------------------------------
c       input: a: mirrortype
c	       g: geometrytype
c	       wc,xlc,ypc1,zpc1: Entwicklungskoeffizienten
c       return: xlm: xlen1c, xlen2c (Koeffizientenstruktur)   
c**************************************************************
c	J. Bahrdt  xx.xx.9x
c	Stand:     19.11.96	UF
c**************************************************************
	implicit none        

c---------- Typen ---------------------------------------------
       	
	structure/geometryst/
           real*8 sina,cosa,sinb,cosb,
     &            r,rp,xdens(0:4),xlam
	   integer idefl   
        end structure 
	
	structure/xlenmap/                 ! Entw. Koeffizienten
	   real*8 xlen1c(0:4,0:4,0:4,0:4), ! der Pfadlaenge
     &  	  xlen2c(0:4,0:4,0:4,0:4)
	end structure      

        dimension wc(0:4,0:4,0:4,0:4),
     &            xlc(0:4,0:4,0:4,0:4),
     &            ypc1(0:4,0:4,0:4,0:4),
     &            zpc1(0:4,0:4,0:4,0:4)  
     
        dimension dpl0(0:4,0:4,0:4,0:4,0:4,0:4),
     &            dsqrl(0:4,0:4,0:4,0:4,0:4,0:4)
        dimension dql1(0:4,0:4,0:4,0:4,0:4,0:4),
     &            dql2(0:4,0:4,0:4,0:4,0:4,0:4)
        dimension xlen1cc(0:4,0:4,0:4,0:4)
        dimension xlen2cc(0:4,0:4,0:4,0:4)

c--------- Variablen ------------------------------------------   

	record /xlenmap/ xlm     
        record /geometryst/ g 

        integer iord,n1,n2,n3,n4,n5,n6

	real*8 a(0:5,0:5),                     ! mirrortype 
     &         wc,xlc,ypc1,zpc1
        real*8 dpl0,dsqrl,dql1,dql2,           ! locale vars
     &         xlen1cc,xlen2cc

	 
c*****************************************************************

	call lsubb(a,g.sina,g.cosa,g.sinb,g.cosb,
     &             g.r,g.rp,g.idefl,dql1,dql2)    
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
 	call lsubc(wc,xlc,xlen1cc,xlm.xlen1c)
        call lsubd00(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)
        call lsubd01(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)
	call lsubd02(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)
	call lsubd10(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)
	call lsubd11(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)
	call lsubd12(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)
	call lsubd2(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)
	call lsubd3(wc,xlc,ypc1,zpc1,xlen2cc,xlm.xlen2c)

	return
	end
c************ end pathlen0 ********************************

c********************************************************** 
	subroutine reduce_1(iord,a,g,acc,wc,xlc,ypc1,zpc1,dypc,dzpc)    
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
	return
	end
c ***************** end reduce_1 **************************

c**********************************************************
      subroutine sig(i,j,k,l,isig)
c**********************************************************
c*******  Zaehlen der Permutationen ***********************
      implicit real*8(a-h,o-z)  

      i1=i
      i2=j
      i3=k
      i4=l
c	write(*,*)' i1,i2,i3,i4 ', i1,i2,i3,i4
      isig=1

c************ i1 = 1 ******************************************

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
c************ i2 = 2 ******************************************

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
c************ i3 = 3 ******************************************

      if(i3.ne.3)then
        isig=-isig
        i4=i3
        i3=3
      endif
c	write(*,*)' isig ',isig
      return
      end
c******* end sig  ***************************************** 

c********************************************************** 
	subroutine transponiere (a,g)   
c----------------------------------------------------------
c       "transponiert" die Geometriedaten und Spiegel- 
c	koeffizienten fuer Rechnung      Bild-> Quelle
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
      subroutine xxmap70(xmap70,ypc1,zpc1,dypc,dzpc)  
c**********************************************************
c     Berechnung der vollstaendigen 70x70 Matrix
c     zur Transformation 4. Ordnung
c**********************************************************
	implicit real*8(a-h,o-z)

      	dimension   ypc1(0:4,0:4,0:4,0:4),
     &              zpc1(0:4,0:4,0:4,0:4),
     &              dypc(0:4,0:4,0:4,0:4),
     &              dzpc(0:4,0:4,0:4,0:4),   
     & 		    xmap70(70,70)
ccccccccccccccccccc alt cccccccccccccccccccccccccccccc
c        common/map7/wc(0:4,0:4,0:4,0:4),
c     &              xlc(0:4,0:4,0:4,0:4),
c        common/map8/dyp1c(0:4,0:4,0:4,0:4),
c     &              dzp1c(0:4,0:4,0:4,0:4), 
c        common/map70/   
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension cc(0:4,0:4,0:4,0:4,35)
      dimension p1(0:4,0:4,0:4,0:4)
      dimension p2(0:4,0:4,0:4,0:4)
      dimension p11(0:4,0:4,0:4,0:4)
      dimension ispalte(0:4,0:4,0:4,0:4)
      dimension facul(0:4,0:4,0:4,0:4)	

      write(*,*)'iord=4 fest'
      iord=4
      inum=70
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
c	type*,ianz,facul(n1,n2,n3,n4)
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
 
           do nn=1,70                 ! Spaltennummerierung
            xmap70(izeile,nn)=cc(l1,l2,l3,l4,nn)
           enddo

          endif
         enddo
        enddo
       enddo
      enddo
      
      return
      end
c**** end xxmap70() ***************************************  

c************** end phlib.for *************************
