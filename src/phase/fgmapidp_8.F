c-------------------------------------------------------------
	subroutine fgmapidp_8(acc,a,g,
     &             wc,xlc,ypc1,zpc1,dypc,dzpc,xlm,
     &             opl,dfdw,dfdl,dfdww,dfdwl,dfdll,dfdwidlj,
     &             iord,imodus,iplmode)
c--------------------------------------------------------------------
c
c	ACHTUNG in get_partial_derivatives muss npch Arbeit 
c	reingesteckt werden
cc
c	this routine evaluates
c	- the transformation ypc1,zpc1,dypc,dzpc
c	- the intersection points wc, xlc
c	- the path length opl
c	- the first derivatives of the pathlength dfdw, dfdl
c	- further partial derivatives of the optical path length
c
c	Variables read in this routine
c		q0(w,l,y,z,yp,zp)
c		q1(w,l,y,z,yp,zp)
c		q2(w,l,y,z,yp,zp)
c	
c	intermediate variables:
c		ypc(w,l,y,z)
c		zpc(w,l,y,z)
c
c		opl6(w,l,y,z,yp,zp)
c		dfdw6(w,l,y,z,yp,zp)
c		dfdl6(w,l,y,z,yp,zp)
c
c		opl4(w,l,y,z)
c		dfdw4(w,l,y,z)
c		dfdl4(w,l,y,z)
c		
c	Output variables:
c		wc(y,z,dy,dz)
c		xlc(y,z,dy,dz)
c		ypc1(y,z,dy,dz)
c		zpc1(y,z,dy,dz)
c		dypc(y,z,dy,dz)
c		dzpc(y,z,dy,dz)
c
c		opl(y,z,dy,dz)
c		dfdw()
c		dfdl()
c		dfdwidlj(i,j,y,z,dy,dz)	i,j derivatives to w and l
c
c---------------------------------------------------------------------

	implicit real*8(a-h,o-z)
	structure/pathlength/
	    real*8 xlen1c(0:7,0:7,0:7,0:7),
     &             xlen2c(0:7,0:7,0:7,0:7)
	end structure
	structure/geometryst/
	  real*8 sina,cosa,sinb,cosb,
     &      r,rp,xdens(0:4),xlam
	  integer idefl
	end structure
	record /geometryst/ g
	record /pathlength/xlm
	dimension a(0:8,0:8)              ! mirror
        dimension wc(0:7,0:7,0:7,0:7),
     &            xlc(0:7,0:7,0:7,0:7),
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
	dimension dfdw1(0:7,0:7,0:7,0:7,0:7,0:7),
     &		  dfdw2(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdw6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdl1(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdl2(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdl6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdww6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdwl6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdll6(0:7,0:7,0:7,0:7,0:7,0:7),
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
     &            dfdll(0:7,0:7,0:7,0:7),
     &            dfdwidlj(0:7,0:7,0:7,0:7,0:7,0:7)
                 
c----------------------------------------------------------
c----------------------------------------------------------
c
c       imodus = 1 : ray tracing von der Quelle zum Bild
c       imodus = 2 : ray tracing vom Bild zur Quelle
c       imodus = 5 : Beamline Optimierung
c
c----------------------------------------------------------
c        write(*,*)'fortran gerufen'
#ifdef DEBUG
         write(6,*)' fgmapidp: imodus = ',imodus
#endif
        if(acc.lt.1e-8)then
           write(*,*)'epsilon for Newton routine < 1e-8'
           write(*,*)'put epsilon to default: 0.01'
           acc=0.01
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
	  call transponiere(a,g)
	endif

	xdefl=dflotj(g.idefl)
	call q0_q1_q2_8(a,g.sina,g.sinb,g.cosa,
     &       g.cosb,g.r,g.rp,g.xdens,g.xlam,xdefl,
     &       q0_8,q1_8,q2_8)

c------------- get optical path length for phase factor
c	OPL=F=sqrt(Q1)+sqrt(Q2)+Q0
c	OPL has six variables (w,l,y,z,yp,zp)

	call tay_copy_6_87(q0_8,q0,iord)
	call tay_copy_6_87(q1_8,q1,iord)
	call tay_copy_6_87(q2_8,q2,iord)

	call Tay_sqrt_6(q1,rtq1,iord)	
	call Tay_sqrt_6(q2,rtq2,iord)	

	call tay_sum_6(rtq1,rtq2,opl6,iord)
	call tay_add_6(opl6,q0,iord)

	call tay_sum_6(rtq1,q0,xloc1_6,iord)
        call tay_copy_6(rtq2,xloc2_6,iord)

c---------- get dfdw6 and dfdl6 in seventh order
c	dfdw6=(dQ1/dw)/sqrt(Q1) + (dQ2/dw)/sqrt(Q2) + dQ0/dw 
c	dfdl6=(dQ1/dl)/sqrt(Q1) + (dQ2/dl)/sqrt(Q2)
c	dfdw6 and dfdl6 have six variables (w,l,y,z,yp,zp)

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

	call Tay_mult_6(rtq1i,dq1w,dfdw1,iord)
	call Tay_mult_6(rtq2i,dq2w,dfdw2,iord)

	call Tay_sum_6(dfdw1,dfdw2,dfdw6,iord)
	call Tay_add_6(dq0w,dfdw6,iord)

	call Tay_mult_6(rtq1i,dq1l,dfdl1,iord)
	call Tay_mult_6(rtq2i,dq2l,dfdl2,iord)
	call Tay_sum_6(dfdl1,dfdl2,dfdl6,iord)

c----------------------------------------------------
C UF 27.5.11 tay_deri_6 gibt es nicht	
	call tay_deri_6(dfdw6,dfdww6,1,1,iord)
	call tay_deri_6(dfdw6,dfdwl6,2,1,iord)
	call tay_deri_6(dfdl6,dfdll6,2,1,iord)

c-------------- solve system of linear equations
c	imode = 1, use 1D Newton Raphson 
c	imode = 2, use 2D Newton Raphson 

c-------------- solve eqs. 19 & 20 iteratively
c	input: 	dfdw6(w,l,y,z,yp,zp) & dfdl6(w,l,y,z,yp,zp)
c	output:  ypc(w,l,y,z)       &  zpc(w,l,y,z)
c
	imode=2
	call subd345a(acc,dfdw6,dfdl6,ypc,zpc,imode,iord)

c------------- replace w and l by power series in (y,z,dy,dz)
c	input:	 ypc(w,l,y,z)   &  zpc(w,l,y,z) 
c	output:	ypc1(y,z,dy,dz) & zpc1(y,z,dy,dz)

        call replace_wl_in_ypzp(ypc,zpc,wc,xlc,
     &                        ypc1,zpc1,2,iord)

c------------- set up eqs. 27 & 28 
c	output:	eq(27)(w,l,y,z,dy,dz) & eq(28)(w,l,y,z,dy,dz)

	call get_dydz(a,eq27,eq28,iord)

c------------- get Taylor expansion of w and l
c	solve set of linear equations
c	input:	eqs. 27 & 28
c	output:	wc(y,z,dy,dz) & xlc(y,z,dy,dz)
	imode=1
	call subd345a(acc,eq27,eq28,wc,xlc,imode,iord)

c------------- get Taylor expansion of dyp and dzp
c	input:	wc(y,z,dy,dz),xlc(y,z,dy,dz),
c		ypc1(y,z,dy,dz),zpc2(y,z,dy,dz)
c	output:	dypc(y,z,dy,dz) & dzpc(y,z,dy,dz)

	call get_dypdzp(a,wc,xlc,ypc1,zpc1,dypc,dzpc,iord)

c------------- get path length in variable (w,l,y,z)
c	input:  opl6(w,l,y,z,yp,zp),
c		ypc1(y,z,dy,dz),zpc1(y,z,dy,dz)
c	output:	opl4(w,l,y,z)

	call replace_6v4v(opl6,ypc1,zpc1,opl4,iord)

        call replace_6v4v(xloc1_6,ypc1,zpc1,xloc1_4,iord)
        call replace_6v4v(xloc2_6,ypc1,zpc1,xloc2_4,iord)

c------------- get path length in variables (y,z,dy,dz)
c	input:	opl4(w,l,y,z)
c	output:	opl(y,z,dy,dz)

	call replace_wl_in_ypzp(opl4,opl4,wc,xlc,
     &                          opl,opl,1,iord)

        call replace_wl_in_ypzp(xloc1_4,xloc1_4,wc,xlc,
     &                          xlm.xlen1c,xlm.xlen1c,1,iord)
        call replace_wl_in_ypzp(xloc2_4,xloc2_4,wc,xlc,
     &                          xlm.xlen2c,xlm.xlen2c,1,iord)

c------------- get partial derivatives in variables (w,l,y,z)
	call replace_6v4v(dfdw6,ypc1,zpc1,dfdw4,iord)
	call replace_6v4v(dfdl6,ypc1,zpc1,dfdl4,iord)

	call replace_6v4v(dfdww6,ypc1,zpc1,dfdww4,iord)
	call replace_6v4v(dfdwl6,ypc1,zpc1,dfdwl4,iord)
	call replace_6v4v(dfdll6,ypc1,zpc1,dfdll4,iord)
	
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

c	do i=0,iord
c	 do j=0,iord-i
c         do k=0,iord-i-j
c	   do l=0,iord-i-j-k
c	    xlm.xlen1c(i,j,k,l)=opl(i,j,k,l)/2.d0
c	    xlm.xlen2c(i,j,k,l)=xlm.xlen1c(i,j,k,l)
c	   enddo
c         enddo
c        enddo
c       enddo
c
c--------------------- get higher order partial derivatives of path length
c	input:	opl6(w,l,y,z,yp,zp)
c	output:	dfdwidlj(i,j,y,z,dy,dz)

	call get_partial_derivatives(opl6,dfdwidlj,iord)

c------------------------------------------------------------

	endif	! idrift = 0

c-----------------------------------------------------------------
c-----------------------------------------------------------------

        if(idrift.eq.1)then
          call drift_8(a,g,acc,wc,xlc,ypc1,zpc1,
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

        endif	! idrift = 1

c-----------------------------------------------------------------
c---------------------------------------------------------------

        if(imodus.eq.2)then
#ifdef DEBUG
          write(*,*)'fgmapidp: make map for image to source iord=',iord
#endif
          call transponiere(a,g)    ! bild zur quelle
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
             enddo
            enddo
           enddo
          enddo

        else
#ifdef DEBUG
          write(*,*)'fgmapidp: make map for source to image iord=',iord
#endif
        endif	! imodus

c-------------------------------------------------------

	return 
	end
