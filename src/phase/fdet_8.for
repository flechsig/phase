c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

c------------------------------------------------------------------
	subroutine fdet_8(ypc1,zpc1,dypc,dzpc,dfdww,dfdwl,dfdll,
     &                    fdetc,fdetphc,fdet1phc,inorm1,inorm2,iord)
c------------------------------------------------------------------
	implicit real*8(a-h,o-z)

        dimension ypc1(0:7,0:7,0:7,0:7),
     &            zpc1(0:7,0:7,0:7,0:7),
     &            dypc(0:7,0:7,0:7,0:7),
     &            dzpc(0:7,0:7,0:7,0:7)

        dimension f1(0:7,0:7,0:7,0:7),
     &            f2(0:7,0:7,0:7,0:7),
     &            f12(0:7,0:7,0:7,0:7),
     &            f34(0:7,0:7,0:7,0:7)

        dimension fdetc(0:7,0:7,0:7,0:7),
     &            fdetphc(0:7,0:7,0:7,0:7),
     &            fdetph1c(0:7,0:7,0:7,0:7)

        dimension dfdww(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdwl(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdll(0:7,0:7,0:7,0:7,0:7,0:7)

c------------------------------------------------------------------
c	evaluate factors
c------------------------------------------------------------------
c
c	0) fdetc wird nicht benutzt, frueher eventuell
c	   fuer isource.lt.4 oder fuer isource.eq.6
c	   jetzt wird dieser Faktor auf 1 gesetzt
c
c	1) dyp/ddy*dzp/ddz-dyp/ddz*dzp/ddy
c
c	2) stationary phase approximation
c	   d2F/dw**2 * d2F/dl**2 - (d2F/dw dl)**2
c
c------------------------------------------------------------------	
c	factor 0)
c------------------------------------------------------------------
	do i=0,iord
	 do j=0,iord-i
	  do k=0,iord-i-j
	   do l=0,iord-i-j-k
	    fdetc(i,j,k,l)=0.d0
	   enddo
	  enddo
	 enddo
	enddo

	fdetc(0,0,0,0)=1.d0

c---------------------------------------------------
c	factor 1)
c---------------------------------------------------	

	if(inorm1.eq.1)then
	do i=0,iord
	 do j=0,iord-i
	  do k=0,iord-i-j
	   do l=0,iord-i-j-k
	    fdetphc(i,j,k,l)=0.d0
	   enddo
	  enddo
	 enddo
	enddo
	fdetphc(0,0,0,0)=1.d0
	endif

	if(inorm1.eq.2)then

	call Tay_deri_4(ypc1,f1,3,1,iord)
	call Tay_deri_4(zpc1,f2,4,1,iord)
	call Tay_mult_4(f1,f2,f12,iord)

	call Tay_deri_4(ypc1,f1,4,1,iord)
	call Tay_deri_4(zpc1,f2,3,1,iord)
	call Tay_mult_4(f1,f2,f34,iord)

	call Tay_const_4(f34,-1.,iord)
	call Tay_sum_4(f12,f34,fdetphc,iord)

	endif

c	bei diesem Faktor braucht man jetzt nur noch:
c	- Taylorreihe berechnen
c	- Absolutbetrag berechnen

c------------------------------------------------------------------	
c	factor 2)
c------------------------------------------------------------------

	if(inorm2.eq.1)then
	do i=0,iord
	 do j=0,iord-i
	  do k=0,iord-i-j
	   do l=0,iord-i-j-k
	    fdetph1c(i,j,k,l)=0.d0
	   enddo
	  enddo
	 enddo
	enddo
	fdetph1c(0,0,0,0)=1.d0
	endif

	if(inorm2.eq.2)then
	 do i=0,iord
	  do j=0,iord-i
	   do k=0,iord-i-j
	    do l=0,iord-i-j-k
	     f1(i,j,k,l)=fdetphc(i,j,k,l)
	    enddo
	   enddo
	  enddo
	 enddo

	 call Tay_inv_4(f1,f2,iord)
	 call Tay_sqrt_4(f2,fdetph1c,iord)
	endif

	if(inorm2.eq.3)then
	 call Tay_mult_4(dfdww,dfdll,f1,iord)
	 call Tay_inv_4(f1,f2,iord)
	 call Tay_sqrt_4(f2,fdetph1c,iord)
	endif

	if(inorm2.eq.4)then
	 call Tay_mult_4(dfdww,dfdll,f1,iord)
	 call Tay_mult_4(dfdwl,dfdwl,f2,iord)
	 call Tay_const_4(f2,-1.,iord)
	 call Tay_sum_4(f1,f2,f12,iord)
	 call Tay_inv_4(f12,f34,iord)
	 call Tay_sqrt_4(f34,fdetph1c,iord)
	endif

c	bei diesem Faktor braucht man jetzt nur noch:
c	- die Taylorreihe berechnen
c	- Betrag bilden
c	- mit Vorfaktor und lambda multiplizieren	
c	  Vorfaktor = sqrt(cosa*cosb)/(r*rp)

	return
	end

