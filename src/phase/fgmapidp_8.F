c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

c-------------------------------------------------------------
	subroutine fgmapidp_8(acc,a,g,
     &             wc,xlc,ypc1,zpc1,dypc,dzpc,
     &             xlm,dfdw,dfdl,iord,imodus,iplmode)
c-------------------------------------------------------------
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
	dimension rtq1(0:7,0:7,0:7,0:7,0:7,0:7),
     &            rtq2(0:7,0:7,0:7,0:7,0:7,0:7),
     &            rtq1i(0:7,0:7,0:7,0:7,0:7,0:7),
     &            rtq2i(0:7,0:7,0:7,0:7,0:7,0:7)
	dimension dfdw1(0:7,0:7,0:7,0:7,0:7,0:7),
     &		  dfdw2(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdw(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdl1(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdl2(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdl(0:7,0:7,0:7,0:7,0:7,0:7),
     &            opl6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            opl4(0:7,0:7,0:7,0:7),
     &            opl(0:7,0:7,0:7,0:7)
                 
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

	call tay_copy_6_87(q0_8,q0,iord)
	call tay_copy_6_87(q1_8,q1,iord)
	call tay_copy_6_87(q2_8,q2,iord)

	call Tay_sqrt_6(q1,rtq1,iord)	
	call Tay_sqrt_6(q2,rtq2,iord)	

	call tay_sum_6(rtq1,rtq2,opl6,iord)
	call tay_add_6(opl6,q0,iord)

c---------- get dfdw and dfdl in seventh order

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

	call Tay_sqrt_6(q1,rtq1,iord)	
	call Tay_sqrt_6(q2,rtq2,iord)	
	call Tay_inv_6(rtq1,rtq1i,iord)	
	call Tay_inv_6(rtq2,rtq2i,iord)	

	call Tay_mult_6(rtq1i,dq1w,dfdw1,iord)
	call Tay_mult_6(rtq2i,dq2w,dfdw2,iord)

	call Tay_sum_6(dfdw1,dfdw2,dfdw,iord)
	call Tay_add_6(dq0w,dfdw,iord)

c--------------

	call Tay_mult_6(rtq1i,dq1l,dfdl1,iord)
	call Tay_mult_6(rtq2i,dq2l,dfdl2,iord)
	call Tay_sum_6(dfdl1,dfdl2,dfdl,iord)

c-------------- solve system of linear equations
c	imode = 1, use 1D Newton Raphson 
c	imode = 2, use 2D Newton Raphson 

	imode=2
	call subd345a(acc,dfdw,dfdl,ypc,zpc,imode,iord)

        call replace_wl_in_ypzp(ypc,zpc,wc,xlc,
     &                        ypc1,zpc1,2,iord)

	call get_dydz(a,eq27,eq28,iord)

	imode=1
	call subd345a(acc,eq27,eq28,wc,xlc,imode,iord)

	call get_dypdzp(a1,wc,xlc,ypc1,zpc1,dypc,dzpc,iord)

	call replace_6v4v(opl6,ypc1,zpc1,opl4,iord)
	call replace_wl_in_ypzp(opl4,opl4,wc,xlc,
     &                          opl,opl,1,iord)
	if(iplmode.eq.1)opl(0,0,0,0)=opl(0,0,0,0)-g.r-g.rp

	do i=0,iord
	 do j=0,iord-i
          do k=0,iord-i-j
	   do l=0,iord-i-j-k
	    xlm.xlen1c(i,j,k,l)=opl(i,j,k,l)/2.d0
	    xlm.xlen2c(i,j,k,l)=xlm.xlen1c(i,j,k,l)
	   enddo
          enddo
         enddo
        enddo

	endif	! idrift = 0

        if(idrift.eq.1)then
          call drift_8(a,g,acc,wc,xlc,ypc1,zpc1,
     &                        dypc,dzpc,imodus,iord)
	xlm.xlen1c(0,0,0,0)=g.r*(1.d0+dypc(0,0,1,0)+dzpc(0,0,1,0))
        xlm.xlen2c(0,0,0,0)=g.rp*(1.d0+dypc(0,0,1,0)+dzpc(0,0,1,0))
        endif	! idrift = 1

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
             enddo
            enddo       
           enddo
          enddo 
        
c----------------------------------
c         Reihenfolge der Indizes:
c         w,l,yi,zi,yf,zf
c----------------------------------
         do n1=0,iord
          do n2=0,iord-n1
           do n3=0,iord-n1-n2
            do n4=0,iord-n1-n2-n3
             do n5=0,iord-n1-n2-n3-n4
              do n6=0,iord-n1-n2-n3-n4-n5
      dfdw(n1,n2,n3,n4,n5,n6)=((-1)**(n1+n2+n4+n6+1))*
     &                         dfdw(n1,n2,n3,n4,n5,n6)

      dfdl(n1,n2,n3,n4,n5,n6)=((-1)**(n1+n2+n4+n6+1))*
     &                         dfdl(n1,n2,n3,n4,n5,n6)
              enddo
             enddo
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
