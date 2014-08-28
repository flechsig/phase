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


c-------------------------------------------------------------
        subroutine fgmapidp_8(acc,
     &             wc,xlc,ypc1,zpc1,ypc,zpc,dypc,dzpc,xlm,
     &             opl6,dfdw6,dfdl6,dfdww6,dfdwl6,dfdll6,dfdwww6,
     &		   dfdwidlj,dfdww,dfdwl,dfdll,
     &             am,g,iord,imodus,iplmode)
c--------------------------------------------------------------------
c
c       this routine evaluates
c       - the transformation ypc1,zpc1,dypc,dzpc expanded in (y,z,dy,dz)
c       - the intersection points wc, xlc        expanded in (y,z,dy,dz)
c	- the path length and the derivatives opl6,dfdw6,dfdl6,dfdww6,dfdwl6,dfdll6,dfdwww6 
c						 expanded in (w,l,y,z,yp,zp)       
c	- the path length opl                    expanded in (y,z,dy,dz)
c       - the derivatives dfdw, dfdl, dfdww, dfdwl, dfdll 
c						 expanded in (y,z,dy,dz)
c	- further derivatives dfdwidlj		 expanded in (w,l,y,z,yp,zp)
c
c       Variables read in this routine
c               q0(w,l,y,z,yp,zp)
c               q1(w,l,y,z,yp,zp)
c               q2(w,l,y,z,yp,zp)
c      
c       intermediate variables:
c               ypc(w,l,y,z)
c               zpc(w,l,y,z)
c
c               opl4(w,l,y,z)
c               dfdw4(w,l,y,z)
c               dfdl4(w,l,y,z)
c              
c       Output variables:
c               wc(y,z,dy,dz)
c               xlc(y,z,dy,dz)
c               ypc1(y,z,dy,dz)
c               zpc1(y,z,dy,dz)
c               dypc(y,z,dy,dz)
c               dzpc(y,z,dy,dz)
c
c               opl6(w,l,y,z,yp,zp)
c               dfdw6(w,l,y,z,yp,zp)
c               dfdl6(w,l,y,z,yp,zp)
c		dfdww6(w,l,y,z,yp,zp)
c		dfdll6(w,l,y,z,yp,zp)
c		dfdwl6(w,l,y,z,yp,zp)
c		dfdwww6(w,l,y,z,yp,zp)
c
c		dfdwidlj(w,l,y,z,yp,zp)
c
c               opl(y,z,dy,dz)
c               dfdw(y,z,dy,dz)
c               dfdl(y,z,dy,dz)
c		dfdww(y,z,dy,dz)
c		dfdll(y,z,dy,dz)
c		dfdwl(y,z,dy,dz)
c
c---------------------------------------------------------------------

        implicit real*8(a-h,o-z)
c        integer elindex
#include '../phase/phase_struct.F'
        structure/pathlength/
            real*8 xlen1c(0:7,0:7,0:7,0:7),
     &             xlen2c(0:7,0:7,0:7,0:7)
        end structure
c       structure/geometryst/
c         real*8 sina,cosa,sinb,cosb,
c     &      r,rp,xdens(0:4),xlam
c         integer idefl
c       end structure
        record /geometryst/ g
        record /mirrortype/ am
        record /pathlength/xlm
c       dimension a(0:8,0:8)              ! mirror
        dimension wc(0:7,0:7,0:7,0:7),
     &            xlc(0:7,0:7,0:7,0:7),
     &            ypc(0:7,0:7,0:7,0:7),
     &            zpc(0:7,0:7,0:7,0:7),
     &            ypc1(0:7,0:7,0:7,0:7),
     &            zpc1(0:7,0:7,0:7,0:7),
     &            dypc(0:7,0:7,0:7,0:7),
     &            dzpc(0:7,0:7,0:7,0:7)
        dimension q0_8(0:8,0:8,0:8,0:8,0:8,0:8),
     &            q1_8(0:8,0:8,0:8,0:8,0:8,0:8),
     &            q2_8(0:8,0:8,0:8,0:8,0:8,0:8)
        dimension q0(0:7,0:7,0:7,0:7,0:7,0:7),
     &            q1(0:7,0:7,0:7,0:7,0:7,0:7),
     &            q2(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension dq0w_8(0:8,0:8,0:8,0:8,0:8,0:8),
     &            dq1w_8(0:8,0:8,0:8,0:8,0:8,0:8),
     &            dq2w_8(0:8,0:8,0:8,0:8,0:8,0:8),
     &            dq1l_8(0:8,0:8,0:8,0:8,0:8,0:8),
     &            dq2l_8(0:8,0:8,0:8,0:8,0:8,0:8)
        dimension dq0w(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dq1w(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dq2w(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dq1l(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dq2l(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension rtq1(0:7,0:7,0:7,0:7,0:7,0:7),
     &            rtq2(0:7,0:7,0:7,0:7,0:7,0:7),
     &            rtq1i(0:7,0:7,0:7,0:7,0:7,0:7),
     &            rtq2i(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension eq27(0:7,0:7,0:7,0:7,0:7,0:7),
     &            eq28(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension dfdw1(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdw2(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdw6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdl1(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdl2(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdl6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdww6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdwl6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdll6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdwww6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdwidlj(0:7,0:7,0:7,0:7,0:7,0:7),
     &            opl6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            opl4(0:7,0:7,0:7,0:7),
     &            opl(0:7,0:7,0:7,0:7),
     &            xloc1_6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            xloc2_6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            xloc1_4(0:7,0:7,0:7,0:7),
     &            xloc2_4(0:7,0:7,0:7,0:7),
     &            dfdw4(0:7,0:7,0:7,0:7),
     &            dfdl4(0:7,0:7,0:7,0:7),
     &            dfdww4(0:7,0:7,0:7,0:7),
     &            dfdwl4(0:7,0:7,0:7,0:7),
     &            dfdll4(0:7,0:7,0:7,0:7),
     &            dfdw(0:7,0:7,0:7,0:7),
     &            dfdl(0:7,0:7,0:7,0:7),
     &            dfdww(0:7,0:7,0:7,0:7),
     &            dfdwl(0:7,0:7,0:7,0:7),
     &            dfdll(0:7,0:7,0:7,0:7)
                
c----------------------------------------------------------
c----------------------------------------------------------
c
c       imodus = 1 : ray tracing von der Quelle zum Bild
c       imodus = 2 : ray tracing vom Bild zur Quelle
c       imodus = 5 : Beamline Optimierung
c
c----------------------------------------------------------

#ifdef DEBUG
c         write(*,*)'=============================== '
c         write(*,*)' fgmapidp: imodus   = ',imodus
c         write(*,*)' fgmapidp: iord     = ',iord
c         write(*,*)' fgmapidp: iplmode  = ',iplmode
c         write(*,*)' fgmapidp: acc      = ',acc
c          write(*,*)' fgmapidp: ielindex  = ',ielindex
c          write(6,*)' fgmapidp: gridsize = ',GRIDSIZE
c         write(6,*)'=============================== '
#endif
        if(acc.lt.1d-12)then
           write(*,*)'epsilon for Newton routine < 1d-12'
           write(*,*)'put epsilon to default: 0.0001'
           acc=0.0001d0
        endif

        idrift=0
        if(imodus.ge.1000)then
          imodus=imodus-1000
          idrift=1
        endif

        if(idrift.eq.0)then

        if(imodus.eq.2)then
          g.xdens(1)=-g.xdens(1)
          g.xdens(3)=-g.xdens(3)
          call transponiere_8(am,g)
        endif

        xdefl=dflotj(g.idefl)
        call q0_q1_q2_8(am.a,g.sina,g.sinb,g.cosa,
     &       g.cosb,g.r,g.rp,g.xdens,g.xlam,xdefl,
     &       q0_8,q1_8,q2_8)

c------------- get optical path length for phase factor
c       OPL=F=sqrt(Q1)+sqrt(Q2)+Q0
c       OPL has six variables (w,l,y,z,yp,zp)
c
        call tay_copy_6_87(q0_8,q0,iord)
        call tay_copy_6_87(q1_8,q1,iord)
        call tay_copy_6_87(q2_8,q2,iord)

        call Tay_sqrt_6(q1,rtq1,iord)  
        call Tay_sqrt_6(q2,rtq2,iord)  

        call tay_copy_6(rtq1,xloc1_6,iord)
        call tay_copy_6(rtq2,xloc2_6,iord)

        call tay_sum_6(rtq1,rtq2,opl6,iord)

c---------- get dfdw6 and dfdl6 in seventh order
c       dfdw6=(dQ1/dw)/sqrt(Q1) + (dQ2/dw)/sqrt(Q2) + dQ0/dw
c       dfdl6=(dQ1/dl)/sqrt(Q1) + (dQ2/dl)/sqrt(Q2)
c       dfdw6 and dfdl6 have six variables (w,l,y,z,yp,zp)
c
        call tay_deri_6a(q0_8,dq0w_8,1,1,iord)
        call tay_deri_6a(q1_8,dq1w_8,1,1,iord)
        call tay_deri_6a(q2_8,dq2w_8,1,1,iord)
        call tay_deri_6a(q1_8,dq1l_8,2,1,iord)
        call tay_deri_6a(q2_8,dq2l_8,2,1,iord)

        call tay_copy_6_87(dq0w_8,dq0w,iord)
        call tay_copy_6_87(dq1w_8,dq1w,iord)
        call tay_copy_6_87(dq2w_8,dq2w,iord)
        call tay_copy_6_87(dq1l_8,dq1l,iord)
        call tay_copy_6_87(dq2l_8,dq2l,iord)

        call Tay_inv_6(rtq1,rtq1i,iord)
        call Tay_inv_6(rtq2,rtq2i,iord)

        call Tay_const_6(rtq1i,0.5d0,iord)
        call Tay_const_6(rtq2i,0.5d0,iord)

        call Tay_mult_6(rtq1i,dq1w,dfdw1,iord)
        call Tay_mult_6(rtq2i,dq2w,dfdw2,iord)

        call Tay_const_6(dq0w,-1.d0,iord)

        call Tay_sum_6(dfdw1,dfdw2,dfdw6,iord)
        call Tay_add_6(dq0w,dfdw6,iord)

        call Tay_mult_6(rtq1i,dq1l,dfdl1,iord)
        call Tay_mult_6(rtq2i,dq2l,dfdl2,iord)
        call Tay_sum_6(dfdl1,dfdl2,dfdl6,iord)

c---------------------------------------------------- 
        call tay_deri_6(dfdw6,dfdww6,1,1,iord)
        call tay_deri_6(dfdw6,dfdwl6,2,1,iord)
        call tay_deri_6(dfdl6,dfdll6,2,1,iord)
        call tay_deri_6(dfdww6,dfdwww6,1,1,iord)

c-------------- solve system of linear equations
c       imode = 1, use 1D Newton Raphson
c       imode = 2, use 2D Newton Raphson

c-------------- solve eqs. 19 & 20 iteratively
c       input:   dfdw6(w,l,y,z,yp,zp) & dfdl6(w,l,y,z,yp,zp)
c       output:  ypc(w,l,y,z)       &  zpc(w,l,y,z)
c
        imode=2
c#ifdef DEBUG       
c UF 20.5.13
c        write(*,*)'debug: fgmapidp_8: call subd345a'
c#endif

        call subd345a(acc,dfdw6,dfdl6,ypc,zpc,imode,iord)

c#ifdef DEBUG
c        write(*,*)'debug: fgmapidp_8: back from subd345a'
c#endif

c        write(6,*)'================ am.a(0,0)',am.a(0,0)
c        write(6,*)'================ q0_8(1,0,0,0,0,0)',q0_8(1,0,0,0,0,0)
c        write(6,*)'================ dq0w_8(0,0,0,0,0,0)',dq0w_8(0,0,0,0,0,0)
c        write(6,*)'================ dq0w(0,0,0,0,0,0)',dq0w(0,0,0,0,0,0)
c        write(6,*)'================ dfdw1(0,0,0,0,0,0) ',dfdw1(0,0,0,0,0,0)
c        write(6,*)'================ dfdw2(0,0,0,0,0,0) ',dfdw2(0,0,0,0,0,0)
c        write(6,*)'================ dfdw6(0,0,0,0,0,0) ',dfdw6(0,0,0,0,0,0)
c        write(6,*)'================ ypc(0,0,0,0) ',ypc(0,0,0,0)

c------------- set up eqs. 27 & 28
c       output: eq(27)(w,l,y,z,dy,dz) & eq(28)(w,l,y,z,dy,dz)
c
        call get_dydz(am.a,g,eq27,eq28,iord)

c------------- get Taylor expansion of w and l
c       solve set of linear equations
c       input:  eqs. 27 & 28
c       output: wc(y,z,dy,dz) & xlc(y,z,dy,dz)
c
        imode=1
        call subd345a(acc,eq27,eq28,wc,xlc,imode,iord)

c------------- replace w and l by power series in (y,z,dy,dz)
c       input:   ypc(w,l,y,z)   &  zpc(w,l,y,z)
c       output: ypc1(y,z,dy,dz) & zpc1(y,z,dy,dz)
c	diese Zeile verschoben
        call replace_wl_in_ypzp(ypc,zpc,wc,xlc,
     &                        ypc1,zpc1,2,iord)

c------------- get Taylor expansion of dyp and dzp
c       input:  wc(y,z,dy,dz),xlc(y,z,dy,dz),
c               ypc1(y,z,dy,dz),zpc2(y,z,dy,dz)
c       output: dypc(y,z,dy,dz) & dzpc(y,z,dy,dz)
c
        call get_dypdzp(am.a,g,wc,xlc,ypc1,zpc1,dypc,dzpc,iord) 

c------ hier ist Berechnung für Ray Tracing zu Ende
        if(imodus.eq.1)goto 1111

c------------- get path length in variable (w,l,y,z)
c       input:  opl6(w,l,y,z,yp,zp),
c               ypc(w,l,y,z),zpc1(w,l,y,z)
c       output: opl4(w,l,y,z)
c	
        call replace_6v4v(opl6,ypc,zpc,opl4,iord)  
        call replace_6v4v(xloc1_6,ypc,zpc,xloc1_4,iord)
        call replace_6v4v(xloc2_6,ypc,zpc,xloc2_4,iord)

         do i=0,2
         do j=0,2-i
         do k=0,2-i-j
         do l=0,2-i-j-k
c            write(6,*)i,j,k,l,xloc1_4(i,j,k,l),xloc2_4(i,j,k,l)
         enddo
         enddo
         enddo
         enddo

c------------- get path length in variables (y,z,dy,dz)
c       input:  opl4(w,l,y,z)
c       output: opl(y,z,dy,dz)
c
        call replace_wl_in_ypzp(opl4,opl4,wc,xlc,
     &                          opl,opl,1,iord) 
        call replace_wl_in_ypzp(xloc1_4,xloc1_4,wc,xlc,
     &                          xlm.xlen1c,xlm.xlen1c,1,iord)
        call replace_wl_in_ypzp(xloc2_4,xloc2_4,wc,xlc,
     &                          xlm.xlen2c,xlm.xlen2c,1,iord)
	
c------------- get partial derivatives in variables (w,l,y,z)
        call replace_6v4v(dfdw6,ypc,zpc,dfdw4,iord)
        call replace_6v4v(dfdl6,ypc,zpc,dfdl4,iord)

        call replace_6v4v(dfdww6,ypc,zpc,dfdww4,iord)
        call replace_6v4v(dfdwl6,ypc,zpc,dfdwl4,iord)
        call replace_6v4v(dfdll6,ypc,zpc,dfdll4,iord)
       
c------------- get partial derivatives in variables (y,z,dy,dz)
        call replace_wl_in_ypzp(dfdw4,dfdw4,wc,xlc,
     &                          dfdw,dfdw,1,iord)
        call replace_wl_in_ypzp(dfdl4,dfdl4,wc,xlc,
     &                          dfdl,dfdl,1,iord)

        call replace_wl_in_ypzp(dfdww4,dfdww4,wc,xlc,
     &                          dfdww,dfdww,1,iord)
        call replace_wl_in_ypzp(dfdwl4,dfdwl4,wc,xlc,
     &                          dfdwl,dfdwl,1,iord)
        call replace_wl_in_ypzp(dfdll4,dfdll4,wc,xlc,
     &                          dfdll,dfdll,1,iord)

        if(iplmode.eq.1)then
          opl(0,0,0,0)=opl(0,0,0,0)-g.r-g.rp
          xlm.xlen1c(0,0,0,0)=xlm.xlen1c(0,0,0,0)-g.r
          xlm.xlen2c(0,0,0,0)=xlm.xlen2c(0,0,0,0)-g.rp
        endif

c-------- get further partial derivatives

	call get_partial_derivatives(opl6,ypc1,zpc1,wc,xlc,
     &  dfdwidlj,iord)
     
c------------------------------------------------------------

1111    continue
        endif   ! idrift = 0

c------------------------------------------------------------

        if(idrift.eq.1)then
          call drift_8(g,acc,wc,xlc,ypc1,zpc1,
     &                        dypc,dzpc,imodus,iord)

        do i=0,iord
         do j=0,iord-i
          do k=0,iord-i-j
           do l=0,iord-i-j-k
            xlm.xlen1c(i,j,k,l)=0.d0
            xlm.xlen2c(i,j,k,l)=0.d0
           enddo
          enddo
         enddo
        enddo

        cc0=g.r+g.rp
        cc1=dsqrt((g.r+g.rp)**2+((g.r+g.rp)*dypc(0,0,1,0))**2)
        cc2=dsqrt((g.r+g.rp)**2+((g.r+g.rp)*dzpc(0,0,0,1))**2)
        cc3=dsqrt((g.r+g.rp)**2+
     &     ((g.r+g.rp)*dypc(0,0,1,0))**2+
     &     ((g.r+g.rp)*dypc(0,0,1,0))**2)

        do i=0,iord
         do j=0,iord-i
            xlm.xlen1c(i,j,0,0)=cc0/2.d0
            xlm.xlen2c(i,j,0,0)=cc0/2.d0
            xlm.xlen1c(i,j,1,0)=cc1/2.d0
            xlm.xlen2c(i,j,1,0)=cc1/2.d0
            xlm.xlen1c(i,j,0,1)=cc2/2.d0
            xlm.xlen2c(i,j,0,1)=cc2/2.d0
            xlm.xlen1c(i,j,1,1)=cc3/2.d0
            xlm.xlen2c(i,j,1,1)=cc3/2.d0
         enddo
        enddo

        endif   ! idrift = 1

c-----------------------------------------------------------------
c---------------------------------------------------------------

        if(imodus.eq.2)then
c#ifdef DEBUG
c          write(*,*)'fgmapidp: make map for image to source iord=',iord
c#endif
          call transponiere_8(am,g)    ! bild zur quelle
          g.xdens(1)=-g.xdens(1)
          g.xdens(3)=-g.xdens(3)
          do n1=0,iord
           do n2=0,iord-n1
            do n3=0,iord-n1-n2
             do n4=0,iord-n1-n2-n3
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
              dfdw(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                         dfdw(n1,n2,n3,n4)
              dfdl(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                         dfdl(n1,n2,n3,n4)
              dfdww(n1,n2,n3,n4)=((-1)**(n2+n3))*
     &                         dfdww(n1,n2,n3,n4)
              dfdwl(n1,n2,n3,n4)=((-1)**(n2+n3))*
     &                         dfdwl(n1,n2,n3,n4)
              dfdll(n1,n2,n3,n4)=((-1)**(n2+n3))*
     &                         dfdll(n1,n2,n3,n4)
              xlm.xlen1c(n1,n2,n3,n4)=((-1)**(n2+n3))*
     &                         xlm.xlen1c(n1,n2,n3,n4)
              xlm.xlen2c(n1,n2,n3,n4)=((-1)**(n2+n3))*
     &                         xlm.xlen2c(n1,n2,n3,n4)
             enddo
            enddo
           enddo
          enddo

        else
c#ifdef DEBUG
c          write(*,*)' fgmapidp: S2Imap done  iord = ',iord
c#endif
        endif   ! imodus 

        return
        end
