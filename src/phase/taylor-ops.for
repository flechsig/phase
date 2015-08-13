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

c---------------------------------------------------------------
c
c       operations with 6 variables
c       Achtung Operationen fuer Taylorrihen generell alle bis
c       zur 7. Ordnung ausser Ableitung, da man da mit
c       8. Ordnung anfangen muss
c
c---------------------------------------------------------------
c-----------------------------------------------------------------
        subroutine Tay_abs_6(a,iord)
c-----------------------------------------------------------------

        implicit real*8(a-h,o-z)
        dimension a(0:7,0:7,0:7,0:7,0:7,0:7)

	if(a(0,0,0,0,0,0).lt.0.d0)then
	do i=0,iord
	 do j=0,iord-i
	  do k=0,iord-i-j
	   do l=0,iord-i-j-k
	    do m=0,iord-i-j-k-l
             do n=0,iord-i-j-k-l-m
	     a(i,j,k,l,m,n)=-a(i,j,k,l,m,n)
	     enddo
            enddo
           enddo
          enddo
         enddo
        enddo
	endif	 

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_cbrt_6(a,d,iord)
c-----------------------------------------------------------------
c
c	assumption: a(0,0,0,0,0,0) > 0
c
c-----------------------------------------------------------------

        implicit real*8(a-h,o-z)
       dimension a(0:7,0:7,0:7,0:7,0:7,0:7),
     &            b(0:7,0:7,0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7,0:7,0:7),
     &            d(0:7,0:7,0:7,0:7,0:7,0:7),
     &            x(0:7,0:7,0:7,0:7,0:7,0:7)

        call Tay_clear_6(d,iord)                ! array for results

        f0=a(0,0,0,0,0,0)                       
        cbrtf0=dabs(a(0,0,0,0,0,0))**(1.d0/3.d0) ! prefactor

c------ iord = 0
	if(iord.eq.0)then
          d(0,0,0,0,0,0)=1.d0
          goto 100
	endif
        
c------ iord = 1
	if(iord.ge.1)then       
 	  call Tay_copy_6(a,x,iord)
          call Tay_const_6(x,1.d0/f0,iord) 
          x(0,0,0,0,0,0)=0.d0                     ! abs(x) lt. 1, contains orders up to iord
          d(0,0,0,0,0,0)=1.d0
          call Tay_copy_6(x,c,iord)
	  cc=1.d0/3.d0
          call Tay_const_6(c,cc,iord)        
          call Tay_add_6(c,d,iord)
	  if(iord.eq.1)goto 100
        endif

c--------- iord gt. 1
c          start summation,
c          store intermediate exponentials in b
c          collecting data in d
c
        if(iord.gt.1)then
	  call Tay_copy_6(x,b,iord)
	  do n=2,iord
            call Tay_mult_6(x,b,c,iord)
            call Tay_copy_6(c,b,iord)
            call Tay_const_6(c,Tay_cbrt_fact(n),iord)
            call Tay_add_6(c,d,iord)
          enddo       
	  goto 100
        endif
        
100     call Tay_const_6(d,cbrtf0,iord)

        return
        end

c------------------------------------------------------
        function Tay_cbrt_fact(n)
c------------------------------------------------------

        implicit real*8(a-h,o-z)

        if(n.eq.0)Tay_cbrt_fact=1.d0
        if(n.eq.1)Tay_cbrt_fact=1.d0/3.d0
        if(n.ge.2)then
	r0=1.d0/3.d0
	fact=r0
	do m=2,n
	fact=fact*((r0-dble(m-1))/dble(m))
	enddo

c         Tay_sqrt_fact=((-1.d0)**n*facult(2*n))/
c     &                 ((1-2*n)*facult(n)**2*4**n)

         Tay_cbrt_fact=fact
        
        endif

        return
        end


c-----------------------------------------------------------------
        subroutine Tay_clear_6(c,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)

        dimension c(0:7,0:7,0:7,0:7,0:7,0:7)

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
        do m=0,iord-i-j-k-l
        do n=0,iord-i-j-k-l-m

        c(i,j,k,l,m,n)=0.d0

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_clear_6a(c,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)

        dimension c(0:8,0:8,0:8,0:8,0:8,0:8)

        iord1=iord+1

        do i=0,iord1
        do j=0,iord1-i
        do k=0,iord1-i-j
        do l=0,iord1-i-j-k
        do m=0,iord1-i-j-k-l
        do n=0,iord1-i-j-k-l-m

        c(i,j,k,l,m,n)=0.d0

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_fill_6(c,val,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)

        dimension c(0:7,0:7,0:7,0:7,0:7,0:7)

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
        do m=0,iord-i-j-k-l
        do n=0,iord-i-j-k-l-m

        c(i,j,k,l,m,n)=val

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_copy_6(a,c,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)

        dimension a(0:7,0:7,0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7,0:7,0:7)

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
        do m=0,iord-i-j-k-l
        do n=0,iord-i-j-k-l-m
       
        c(i,j,k,l,m,n)=a(i,j,k,l,m,n)

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_copy_6_87(a8,c,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)

        dimension a8(0:8,0:8,0:8,0:8,0:8,0:8),
     &            c(0:7,0:7,0:7,0:7,0:7,0:7)

        do i=0,7
        do j=0,7-i
        do k=0,7-i-j
        do l=0,7-i-j-k
        do m=0,7-i-j-k-l
        do n=0,7-i-j-k-l-m
       
        c(i,j,k,l,m,n)=a8(i,j,k,l,m,n)

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        return
        end

c---------------------------------------------------------------
        subroutine Tay_sum_6(a,b,c,iord)
c---------------------------------------------------------------
c       Sum of two Taylor series with six Variables
c       the order of expansion is iord
cc--------------------------------------------------------------

        implicit real*8(a-h,o-z)

        dimension a(0:7,0:7,0:7,0:7,0:7,0:7),
     &            b(0:7,0:7,0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7,0:7,0:7)

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
        do m=0,iord-i-j-k-l
        do n=0,iord-i-j-k-l-m

        c(i,j,k,l,m,n)=a(i,j,k,l,m,n)+b(i,j,k,l,m,n)

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        return
        end

c---------------------------------------------------------------
        subroutine Tay_add_6(a,c,iord)
c---------------------------------------------------------------
c       adds one Taylor series with six Variables
c       to another one of
c       the order of expansion is iord
cc--------------------------------------------------------------

        implicit real*8(a-h,o-z)

        dimension a(0:7,0:7,0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7,0:7,0:7)


        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
        do m=0,iord-i-j-k-l
        do n=0,iord-i-j-k-l-m

        c(i,j,k,l,m,n)=c(i,j,k,l,m,n)+a(i,j,k,l,m,n)

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_const_6(a,const,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)

        dimension a(0:7,0:7,0:7,0:7,0:7,0:7)

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
        do m=0,iord-i-j-k-l
        do n=0,iord-i-j-k-l-m
       
        a(i,j,k,l,m,n)=a(i,j,k,l,m,n)*const

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        return
        end

c---------------------------------------------------------------
        subroutine Tay_mult_6(a,b,c,iord)
c---------------------------------------------------------------
c       Multiplication of two Taylor series with six Variables
c       the order of expansion is iord
cc--------------------------------------------------------------

        implicit real*8(a-h,o-z)

        dimension a(0:7,0:7,0:7,0:7,0:7,0:7),
     &            b(0:7,0:7,0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7,0:7,0:7)

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
        do m=0,iord-i-j-k-l
        do n=0,iord-i-j-k-l-m

        c(i,j,k,l,m,n)=0.d0

        do m1=0,i
        do m2=0,j
        do m3=0,k
        do m4=0,l
        do m5=0,m
        do m6=0,n

        n1=i-m1
        n2=j-m2
        n3=k-m3
        n4=l-m4
        n5=m-m5
        n6=n-m6

        c(i,j,k,l,m,n)=c(i,j,k,l,m,n)+
     &     a(m1,m2,m3,m4,m5,m6)*b(n1,n2,n3,n4,n5,n6)

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        return
        end

c---------------------------------------------------------------
        subroutine Tay_exp_6(a,c,iexp,iord)
c---------------------------------------------------------------
c       Exponentiation of a Taylor series with six variables
c       with an exponent iexp
c       the order of expansion is iord
c---------------------------------------------------------------

        implicit real*8(a-h,o-z)

        dimension a(0:7,0:7,0:7,0:7,0:7,0:7),
     &            b(0:7,0:7,0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7,0:7,0:7)

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
        do m=0,iord-i-j-k-l
        do n=0,iord-i-j-k-l-m
       
        if(iexp.eq.0)then
          c(i,j,k,l,m,n)=0.d0
          else
          b(i,j,k,l,m,n)=a(i,j,k,l,m,n)
        endif

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        if(iexp.eq.0)then
          c(0,0,0,0,0,0)=1.d0
          goto 100
        endif

        if(iexp.eq.1)then
          call Tay_copy_6(b,c,iord)
          goto 100
        endif

        do ii=2,iexp
          call Tay_mult_6(a,b,c,iord)
          call Tay_copy_6(c,b,iord)
        enddo
        call Tay_copy_6(b,c,iord)
       
100     continue

        return
        end


c-----------------------------------------------------------------
        subroutine Tay_sqrt_6(a,d,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)
       dimension a(0:7,0:7,0:7,0:7,0:7,0:7),
     &            b(0:7,0:7,0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7,0:7,0:7),
     &            d(0:7,0:7,0:7,0:7,0:7,0:7),
     &            x(0:7,0:7,0:7,0:7,0:7,0:7)

        call Tay_clear_6(d,iord)                ! array for results

        f0=a(0,0,0,0,0,0)                       ! prefactor
        sqf0=dsqrt(a(0,0,0,0,0,0))

c----- iord = 0
        if(iord.eq.0)then
          d(0,0,0,0,0,0)=sqf0
          goto 100
        endif
        
c----- iord = 1 
        call Tay_copy_6(a,x,iord)
        call Tay_const_6(x,1.d0/f0,iord) 
        x(0,0,0,0,0,0)=0.d0                     ! abs(x) lt. 1
        d(0,0,0,0,0,0)=1.d0

        call Tay_copy_6(x,b,iord)
        call Tay_copy_6(x,c,iord)
        call Tay_const_6(c,0.5d0,iord)        
        call Tay_add_6(c,d,iord)

c--------- iord gt. 1
c          start summation,
c          store intermediate exponentials in b
c          collecting data in d
c
        if(iord.gt.1)then
        do n=2,iord
          call Tay_mult_6(x,b,c,iord)
          call Tay_copy_6(c,b,iord)
          call Tay_const_6(c,Tay_sqrt_fact(n),iord)
          call Tay_add_6(c,d,iord)
        enddo       
        endif
        
        call Tay_const_6(d,sqf0,iord)

100     continue

        return
        end


c------------------------------------------------------
        function Tay_sqrt_fact(n)
c------------------------------------------------------

        implicit real*8(a-h,o-z)

        if(n.eq.0)Tay_sqrt_fact=1.d0
        if(n.eq.1)Tay_sqrt_fact=0.5d0
        if(n.ge.2)then
        
         Tay_sqrt_fact=((-1.d0)**n*facult(2*n))/
     &                 ((1-2*n)*facult(n)**2*4**n)
        
        endif

        return
        end


c UF remove facult- it is already in phasefor.F
        
c-----------------------------------------------------------------
        subroutine Tay_inv_6(a,d,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)

        dimension a(0:7,0:7,0:7,0:7,0:7,0:7),
     &            b(0:7,0:7,0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7,0:7,0:7),
     &            d(0:7,0:7,0:7,0:7,0:7,0:7),
     &            x(0:7,0:7,0:7,0:7,0:7,0:7)

        call Tay_clear_6(d,iord)                ! array for results

        f0=a(0,0,0,0,0,0)                       ! prefactor
 
c----- iord = 0
        if(iord.eq.0)then
          d(0,0,0,0,0,0)=1.d0/f0
          goto 100
        endif
        
c----- iord = 1 
        call Tay_copy_6(a,x,iord)
        call Tay_const_6(x,1.d0/f0,iord) 
        x(0,0,0,0,0,0)=0.d0                     ! abs(x) lt. 1
        d(0,0,0,0,0,0)=1.d0

        call Tay_copy_6(x,b,iord)
        call Tay_copy_6(x,c,iord)
        call Tay_const_6(c,-1.d0,iord)        
        call Tay_add_6(c,d,iord)

c--------- iord gt. 1
c          start summation,
c          store intermediate exponentials in b
c          collecting data in d
c
        if(iord.gt.1)then
        do n=2,iord
          call Tay_mult_6(x,b,c,iord)
          call Tay_copy_6(c,b,iord)
          call Tay_const_6(c,Tay_inv_fact(n),iord)
          call Tay_add_6(c,d,iord)
        enddo        
        endif      
        
        call Tay_const_6(d,1.d0/f0,iord)

100     continue

        return
        end

c------------------------------------------------------
        function Tay_inv_fact(n)
c------------------------------------------------------

        implicit real*8(a-h,o-z)

        if(n.eq.0)Tay_inv_fact=1.d0
        if(n.ge.1)Tay_inv_fact=(-1.d0)**n

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_deri_6a(a8,c8,m0,n0,iord)
c-----------------------------------------------------------------
c
c       n0-times partial derivative of the Taylor series a
c       with respect to the m0-th variable
c
        implicit real*8(a-h,o-z)

        dimension a8(0:8,0:8,0:8,0:8,0:8,0:8),
     &            c8(0:8,0:8,0:8,0:8,0:8,0:8)

        call Tay_clear_6a(c8,iord)

        iord1=iord+1
 
        do i=0,iord1
        do j=0,iord1-i
        do k=0,iord1-i-j
        do l=0,iord1-i-j-k
        do m=0,iord1-i-j-k-l
        do n=0,iord1-i-j-k-l-m
       
        if(m0.eq.1)then
        if(i-n0.ge.0)then
        c8(i-n0,j,k,l,m,n)=a8(i,j,k,l,m,n)*(facult(i)/facult(i-n0))
        goto 100
        endif
        endif

        if(m0.eq.2)then
        if(j-n0.ge.0)then
        c8(i,j-n0,k,l,m,n)=a8(i,j,k,l,m,n)*(facult(j)/facult(j-n0))
        goto 100
        endif
        endif

        if(m0.eq.3)then
        if(k-n0.ge.0)then
        c8(i,j,k-n0,l,m,n)=a8(i,j,k,l,m,n)*(facult(k)/facult(k-n0))
        goto 100
        endif
        endif

        if(m0.eq.4)then
        if(l-n0.ge.0)then
        c8(i,j,k,l-n0,m,n)=a8(i,j,k,l,m,n)*(facult(l)/facult(l-n0))
        goto 100
        endif
        endif

        if(m0.eq.5)then
        if(m-n0.ge.0)then
        c8(i,j,k,l,m-n0,n)=a8(i,j,k,l,m,n)*(facult(m)/facult(m-n0))
        goto 100
        endif
        endif

        if(m0.eq.6)then
        if(n-n0.ge.0)then
        c8(i,j,k,l,m,n-n0)=a8(i,j,k,l,m,n)*(facult(n)/facult(n-n0))
        goto 100
        endif
        endif

100     continue

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_deri_6(a8,c8,m0,n0,iord)
c-----------------------------------------------------------------
c
c       n0-times partial derivative of the Taylor series a
c       with respect to the m0-th variable
c
        implicit real*8(a-h,o-z)

        dimension a8(0:7,0:7,0:7,0:7,0:7,0:7),
     &            c8(0:7,0:7,0:7,0:7,0:7,0:7)

        call Tay_clear_6(c8,iord)
 
        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
        do m=0,iord-i-j-k-l
        do n=0,iord-i-j-k-l-m
       
        if(m0.eq.1)then
        if(i-n0.ge.0)then
        c8(i-n0,j,k,l,m,n)=a8(i,j,k,l,m,n)*(facult(i)/facult(i-n0))
        goto 100
        endif
        endif

        if(m0.eq.2)then
        if(j-n0.ge.0)then
        c8(i,j-n0,k,l,m,n)=a8(i,j,k,l,m,n)*(facult(j)/facult(j-n0))
        goto 100
        endif
        endif

        if(m0.eq.3)then
        if(k-n0.ge.0)then
        c8(i,j,k-n0,l,m,n)=a8(i,j,k,l,m,n)*(facult(k)/facult(k-n0))
        goto 100
        endif
        endif

        if(m0.eq.4)then
        if(l-n0.ge.0)then
        c8(i,j,k,l-n0,m,n)=a8(i,j,k,l,m,n)*(facult(l)/facult(l-n0))
        goto 100
        endif
        endif

        if(m0.eq.5)then
        if(m-n0.ge.0)then
        c8(i,j,k,l,m-n0,n)=a8(i,j,k,l,m,n)*(facult(m)/facult(m-n0))
        goto 100
        endif
        endif

        if(m0.eq.6)then
        if(n-n0.ge.0)then
        c8(i,j,k,l,m,n-n0)=a8(i,j,k,l,m,n)*(facult(n)/facult(n-n0))
        goto 100
        endif
        endif

100     continue

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_deri_4(a7,c7,m0,n0,iord)
c-----------------------------------------------------------------
c
c       n0-times partial derivative of the Taylor series a
c       with respect to the m0-th variable
c
        implicit real*8(a-h,o-z)

        dimension a7(0:7,0:7,0:7,0:7),
     &            c7(0:7,0:7,0:7,0:7)

        call Tay_clear_4(c7,iord)
 
        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
       
        if(m0.eq.1)then
        if(i-n0.ge.0)then
        c7(i-n0,j,k,l)=a7(i,j,k,l)*(facult(i)/facult(i-n0))
        goto 100
        endif
        endif

        if(m0.eq.2)then
        if(j-n0.ge.0)then
        c7(i,j-n0,k,l)=a7(i,j,k,l)*(facult(j)/facult(j-n0))
        goto 100
        endif
        endif

        if(m0.eq.3)then
        if(k-n0.ge.0)then
        c7(i,j,k-n0,l)=a7(i,j,k,l)*(facult(k)/facult(k-n0))
        goto 100
        endif
        endif

        if(m0.eq.4)then
        if(l-n0.ge.0)then
        c7(i,j,k,l-n0)=a7(i,j,k,l)*(facult(l)/facult(l-n0))
        goto 100
        endif
        endif

100     continue

        enddo
        enddo
        enddo
        enddo

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_deri_6a_2(a8,c8,m1,n1,m2,n2,iord)
c-----------------------------------------------------------------
c
c       n1-times partial derivative with respect to variable m1
c       n2-times partial derivative with respeft to variable m2
c       of the Taylor series a
c
        implicit real*8(a-h,o-z)

        dimension a8(0:8,0:8,0:8,0:8,0:8,0:8),
     &            b8(0:8,0:8,0:8,0:8,0:8,0:8),
     &            c8(0:8,0:8,0:8,0:8,0:8,0:8)


        iord1=iord+1

        call Tay_clear_6a(b8,iord)
        call Tay_clear_6a(c8,iord)

        do i=0,iord1
        do j=0,iord1-i
        do k=0,iord1-i-j
        do l=0,iord1-i-j-k
        do m=0,iord1-i-j-k-l
        do n=0,iord1-i-j-k-l-m
       
        if(m1.eq.1)then
        if(i-n1.ge.0)then
        b8(i-n1,j,k,l,m,n)=a8(i,j,k,l,m,n)*(facult(i)/facult(i-n1))
        goto 100
        endif
        endif

        if(m1.eq.2)then
        if(j-n1.ge.0)then
        b8(i,j-n1,k,l,m,n)=a8(i,j,k,l,m,n)*(facult(j)/facult(j-n1))
        goto 100
        endif
        endif

        if(m1.eq.3)then
        if(k-n1.ge.0)then
        b8(i,j,k-n1,l,m,n)=a8(i,j,k,l,m,n)*(facult(k)/facult(k-n1))
        goto 100
        endif
        endif

        if(m1.eq.4)then
        if(l-n1.ge.0)then
        b8(i,j,k,l-n1,m,n)=a8(i,j,k,l,m,n)*(facult(l)/facult(l-n1))
        goto 100
        endif
        endif

        if(m1.eq.5)then
        if(m-n1.ge.0)then
        b8(i,j,k,l,m-n1,n)=a8(i,j,k,l,m,n)*(facult(m)/facult(m-n1))
        goto 100
        endif
        endif

        if(m1.eq.6)then
        if(n-n1.ge.0)then
        b8(i,j,k,l,m,n-n1)=a8(i,j,k,l,m,n)*(facult(n)/facult(n-n1))
        goto 100
        endif
        endif

100     continue

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

c------------------------------------------------------------------

        do i=0,iord1
        do j=0,iord1-i
        do k=0,iord1-i-j
        do l=0,iord1-i-j-k
        do m=0,iord1-i-j-k-l
        do n=0,iord1-i-j-k-l-m
       
        if(m2.eq.1)then
        if(i-n2.ge.0)then
        c8(i-n2,j,k,l,m,n)=b8(i,j,k,l,m,n)*(facult(i)/facult(i-n2))
        goto 200
        endif
        endif

        if(m2.eq.2)then
        if(j-n2.ge.0)then
        c8(i,j-n2,k,l,m,n)=b8(i,j,k,l,m,n)*(facult(j)/facult(j-n2))
        goto 200
        endif
        endif

        if(m2.eq.3)then
        if(k-n2.ge.0)then
        c8(i,j,k-n2,l,m,n)=b8(i,j,k,l,m,n)*(facult(k)/facult(k-n2))
        goto 200
        endif
        endif

        if(m2.eq.4)then
        if(l-n2.ge.0)then
        c8(i,j,k,l-n2,m,n)=b8(i,j,k,l,m,n)*(facult(l)/facult(l-n2))
        goto 200
        endif
        endif

        if(m2.eq.5)then
        if(m-n2.ge.0)then
        c8(i,j,k,l,m-n2,n)=b8(i,j,k,l,m,n)*(facult(m)/facult(m-n2))
        goto 200
        endif
        endif

        if(m2.eq.6)then
        if(n-n2.ge.0)then
        c8(i,j,k,l,m,n-n2)=b8(i,j,k,l,m,n)*(facult(n)/facult(n-n2))
        goto 200
        endif
        endif

200     continue

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        return
        end


c-----------------------------------------------------------------
        subroutine Tay_deri_6_2(a8,c8,m1,n1,m2,n2,iord)
c-----------------------------------------------------------------
c
c       n1-times partial derivative with respect to variable m1
c       n2-times partial derivative with respeft to variable m2
c       of the Taylor series a
c
        implicit real*8(a-h,o-z)

        dimension a8(0:7,0:7,0:7,0:7,0:7,0:7),
     &            b8(0:7,0:7,0:7,0:7,0:7,0:7),
     &            c8(0:7,0:7,0:7,0:7,0:7,0:7)

        call Tay_clear_6(b8,iord)
        call Tay_clear_6(c8,iord)

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
        do m=0,iord-i-j-k-l
        do n=0,iord-i-j-k-l-m
       
        if(m1.eq.1)then
        if(i-n1.ge.0)then
        b8(i-n1,j,k,l,m,n)=a8(i,j,k,l,m,n)*(facult(i)/facult(i-n1))
        goto 100
        endif
        endif

        if(m1.eq.2)then
        if(j-n1.ge.0)then
        b8(i,j-n1,k,l,m,n)=a8(i,j,k,l,m,n)*(facult(j)/facult(j-n1))
        goto 100
        endif
        endif

        if(m1.eq.3)then
        if(k-n1.ge.0)then
        b8(i,j,k-n1,l,m,n)=a8(i,j,k,l,m,n)*(facult(k)/facult(k-n1))
        goto 100
        endif
        endif

        if(m1.eq.4)then
        if(l-n1.ge.0)then
        b8(i,j,k,l-n1,m,n)=a8(i,j,k,l,m,n)*(facult(l)/facult(l-n1))
        goto 100
        endif
        endif

        if(m1.eq.5)then
        if(m-n1.ge.0)then
        b8(i,j,k,l,m-n1,n)=a8(i,j,k,l,m,n)*(facult(m)/facult(m-n1))
        goto 100
        endif
        endif

        if(m1.eq.6)then
        if(n-n1.ge.0)then
        b8(i,j,k,l,m,n-n1)=a8(i,j,k,l,m,n)*(facult(n)/facult(n-n1))
        goto 100
        endif
        endif

100     continue

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

c------------------------------------------------------------

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
        do m=0,iord-i-j-k-l
        do n=0,iord-i-j-k-l-m
       
        if(m2.eq.1)then
        if(i-n2.ge.0)then
        c8(i-n2,j,k,l,m,n)=b8(i,j,k,l,m,n)*(facult(i)/facult(i-n2))
        goto 200
        endif
        endif

        if(m2.eq.2)then
        if(j-n2.ge.0)then
        c8(i,j-n2,k,l,m,n)=b8(i,j,k,l,m,n)*(facult(j)/facult(j-n2))
        goto 200
        endif
        endif

        if(m2.eq.3)then
        if(k-n2.ge.0)then
        c8(i,j,k-n2,l,m,n)=b8(i,j,k,l,m,n)*(facult(k)/facult(k-n2))
        goto 200
        endif
        endif

        if(m2.eq.4)then
        if(l-n2.ge.0)then
        c8(i,j,k,l-n2,m,n)=b8(i,j,k,l,m,n)*(facult(l)/facult(l-n2))
        goto 200
        endif
        endif

        if(m2.eq.5)then
        if(m-n2.ge.0)then
        c8(i,j,k,l,m-n2,n)=b8(i,j,k,l,m,n)*(facult(m)/facult(m-n2))
        goto 200
        endif
        endif

        if(m2.eq.6)then
        if(n-n2.ge.0)then
        c8(i,j,k,l,m,n-n2)=b8(i,j,k,l,m,n)*(facult(n)/facult(n-n2))
        goto 200
        endif
        endif

200     continue

        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        return
        end

c------------------------------------------------------------
c------------------------------------------------------------
c       Operations with four variables
c------------------------------------------------------------
c------------------------------------------------------------
c-----------------------------------------------------------------
        subroutine Tay_abs_4(a,iord)
c-----------------------------------------------------------------

        implicit real*8(a-h,o-z)
        dimension a(0:7,0:7,0:7,0:7)

	if(a(0,0,0,0).lt.0.d0)then
	do i=0,iord
	 do j=0,iord-i
	  do k=0,iord-i-j
	   do l=0,iord-i-j-k
	     a(i,j,k,l)=-a(i,j,k,l)
	     enddo
            enddo
           enddo
          enddo
	endif	 

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_cbrt_4(a,d,iord)
c-----------------------------------------------------------------
c
c	assumption: a(0,0,0,0) > 0
c
c-----------------------------------------------------------------

        implicit real*8(a-h,o-z)
       dimension a(0:7,0:7,0:7,0:7),
     &            b(0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7),
     &            d(0:7,0:7,0:7,0:7),
     &            x(0:7,0:7,0:7,0:7)

        call Tay_clear_4(d,iord)                ! array for results

        f0=a(0,0,0,0)                       
        cbrtf0=dabs(a(0,0,0,0))**(1.d0/3.d0) ! prefactor

c------ iord = 0
	if(iord.eq.0)then
          d(0,0,0,0)=1.d0
          goto 100
	endif
        
c------ iord = 1
	if(iord.ge.1)then       
 	  call Tay_copy_4(a,x,iord)
          call Tay_const_4(x,1.d0/f0,iord) 
          x(0,0,0,0)=0.d0                     ! abs(x) lt. 1, contains orders up to iord
          d(0,0,0,0)=1.d0
          call Tay_copy_4(x,c,iord)
	  cc=1.d0/3.d0
          call Tay_const_4(c,cc,iord)        
          call Tay_add_4(c,d,iord)
	  if(iord.eq.1)goto 100
        endif

c--------- iord gt. 1
c          start summation,
c          store intermediate exponentials in b
c          collecting data in d
c
        if(iord.gt.1)then
	  call Tay_copy_4(x,b,iord)
	  do n=2,iord
            call Tay_mult_4(x,b,c,iord)
            call Tay_copy_4(c,b,iord)
            call Tay_const_4(c,Tay_cbrt_fact(n),iord)
            call Tay_add_4(c,d,iord)
          enddo       
	  goto 100
        endif
        
100     call Tay_const_4(d,cbrtf0,iord)

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_clear_4(c,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)

        dimension c(0:7,0:7,0:7,0:7)

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k

        c(i,j,k,l)=0.d0

        enddo
        enddo
        enddo
        enddo

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_clear_4a(c,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)

        dimension c(0:8,0:8,0:8,0:8)

        iord1=iord+1

        do i=0,iord1
        do j=0,iord1-i
        do k=0,iord1-i-j
        do l=0,iord1-i-j-k

        c(i,j,k,l)=0.d0

        enddo
        enddo
        enddo
        enddo

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_fill_4(c,val,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)

        dimension c(0:7,0:7,0:7,0:7)

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k

        c(i,j,k,l)=val

        enddo
        enddo
        enddo
        enddo

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_copy_4(a,c,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)

        dimension a(0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7)

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
       
        c(i,j,k,l)=a(i,j,k,l)

        enddo
        enddo
        enddo
        enddo

        return
        end

c---------------------------------------------------------------
        subroutine Tay_sum_4(a,b,c,iord)
c---------------------------------------------------------------
c       Sum of two Taylor series with six Variables
c       the order of expansion is iord
cc--------------------------------------------------------------

        implicit real*8(a-h,o-z)

        dimension a(0:7,0:7,0:7,0:7),
     &            b(0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7)

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k

        c(i,j,k,l)=a(i,j,k,l)+b(i,j,k,l)

        enddo
        enddo
        enddo
        enddo

        return
        end

c---------------------------------------------------------------
        subroutine Tay_add_4(a,c,iord)
c---------------------------------------------------------------
c       adds one Taylor series with six Variables
c       to another one of
c       the order of expansion is iord
cc--------------------------------------------------------------

        implicit real*8(a-h,o-z)

        dimension a(0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7)


        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k

        c(i,j,k,l)=c(i,j,k,l)+a(i,j,k,l)

        enddo
        enddo
        enddo
        enddo

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_const_4(a,const,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)

        dimension a(0:7,0:7,0:7,0:7)

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
       
        a(i,j,k,l)=a(i,j,k,l)*const

        enddo
        enddo
        enddo
        enddo

        return
        end

c---------------------------------------------------------------
        subroutine Tay_mult_4(a,b,c,iord)
c---------------------------------------------------------------
c       Multiplication of two Taylor series with six Variables
c       the order of expansion is iord
cc--------------------------------------------------------------

        implicit real*8(a-h,o-z)

        dimension a(0:7,0:7,0:7,0:7),
     &            b(0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7)

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k

        c(i,j,k,l)=0.d0

        do m1=0,i
        do m2=0,j
        do m3=0,k
        do m4=0,l

        n1=i-m1
        n2=j-m2
        n3=k-m3
        n4=l-m4

        c(i,j,k,l)=c(i,j,k,l)+
     &    a(m1,m2,m3,m4)*b(n1,n2,n3,n4)

         enddo
        enddo
        enddo
        enddo

        enddo
        enddo
        enddo
        enddo

        return
        end

c---------------------------------------------------------------
        subroutine Tay_exp_4(a,c,iexp,iord)
c---------------------------------------------------------------
c       Exponentiation of a Taylor series with six variables
c       with an exponent iexp
c       the order of expansion is iord
c---------------------------------------------------------------

        implicit real*8(a-h,o-z)

        dimension a(0:7,0:7,0:7,0:7),
     &            b(0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7)

        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
       
        if(iexp.eq.0)then
          c(i,j,k,l)=0.d0
          else
          b(i,j,k,l)=a(i,j,k,l)
        endif

        enddo
        enddo
        enddo
        enddo

        if(iexp.eq.0)then
          c(0,0,0,0)=1.d0
          goto 100
        endif

        if(iexp.eq.1)then
          call Tay_copy_4(b,c,iord)
          goto 100
        endif

        do ii=2,iexp
          call Tay_mult_4(a,b,c,iord)
          call Tay_copy_4(c,b,iord)
        enddo
        call Tay_copy_4(b,c,iord)
       
100     continue

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_sqrt_4(a,d,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)
       dimension a(0:7,0:7,0:7,0:7),
     &            b(0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7),
     &            d(0:7,0:7,0:7,0:7),
     &            x(0:7,0:7,0:7,0:7)

        call Tay_clear_4(d,iord)                ! array for results

        f0=a(0,0,0,0)                           ! prefactor
        sqf0=dsqrt(a(0,0,0,0))

c----- iord = 0
        if(iord.eq.0)then
          d(0,0,0,0)=sqf0
          goto 100
        endif
        
c----- iord = 1 
        call Tay_copy_4(a,x,iord)
        call Tay_const_4(x,1.d0/f0,iord) 
        x(0,0,0,0)=0.d0                     ! abs(x) lt. 1
        d(0,0,0,0)=1.d0

        call Tay_copy_4(x,b,iord)
        call Tay_copy_4(x,c,iord)
        call Tay_const_4(c,0.5d0,iord)        
        call Tay_add_4(c,d,iord)

c--------- iord gt. 1
c          start summation,
c          store intermediate exponentials in b
c          collecting data in d
c
        if(iord.gt.1)then
        do n=2,iord
          call Tay_mult_4(x,b,c,iord)
          call Tay_copy_4(c,b,iord)
          call Tay_const_4(c,Tay_sqrt_fact(n),iord)
          call Tay_add_4(c,d,iord)
        enddo       
        endif
        
        call Tay_const_4(d,sqf0,iord)

100     continue

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_inv_4(a,d,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)

        dimension a(0:7,0:7,0:7,0:7),
     &            b(0:7,0:7,0:7,0:7),
     &            c(0:7,0:7,0:7,0:7),
     &            d(0:7,0:7,0:7,0:7),
     &            x(0:7,0:7,0:7,0:7)

        call Tay_clear_4(d,iord)                ! array for results

        f0=a(0,0,0,0)                           ! prefactor
 
c----- iord = 0
        if(iord.eq.0)then
          d(0,0,0,0)=1.d0/f0
          goto 100
        endif
        
c----- iord = 1 
        call Tay_copy_4(a,x,iord)
        call Tay_const_4(x,1.d0/f0,iord) 
        x(0,0,0,0)=0.d0                     ! abs(x) lt. 1
        d(0,0,0,0)=1.d0

        call Tay_copy_4(x,b,iord)
        call Tay_copy_4(x,c,iord)
        call Tay_const_4(c,-1.d0,iord)        
        call Tay_add_4(c,d,iord)

c--------- iord gt. 1
c          start summation,
c          store intermediate exponentials in b
c          collecting data in d
c
        if(iord.gt.1)then
        do n=2,iord
          call Tay_mult_4(x,b,c,iord)
          call Tay_copy_4(c,b,iord)
          call Tay_const_4(c,Tay_inv_fact(n),iord)
          call Tay_add_4(c,d,iord)
        enddo        
        endif     
          
        call Tay_const_4(d,1.d0/f0,iord)

100     continue

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_copy_vm_4(cc1,cc,k0,l0,m0,n0,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)

        dimension cc1(0:7,0:7,0:7,0:7),
     &            cc(0:7,0:7,0:7,0:7,1:330)
 
        icol=0
        do k=0,iord
        do l=0,iord-k
        do m=0,iord-k-l
        do n=0,iord-k-l-m 
          icol=icol+1
          cc(k0,l0,m0,n0,icol)=cc1(k,l,m,n)
        enddo
        enddo
        enddo
        enddo

        return
        end

c-----------------------------------------------------------------
        subroutine Tay_copy_mv_4(cc,cc1,k0,l0,m0,n0,iord)
c-----------------------------------------------------------------
        implicit real*8(a-h,o-z)

        dimension cc1(0:7,0:7,0:7,0:7),
     &            cc(0:7,0:7,0:7,0:7,1:330)

        icol=0

        do k=0,iord
        do l=0,iord-k
        do m=0,iord-k-l
        do n=0,iord-k-l-m

        icol=icol+1    
        cc1(k,l,m,n)=cc(k0,l0,m0,n0,icol)

        enddo
        enddo
        enddo
        enddo

        return
        end
c end of file
