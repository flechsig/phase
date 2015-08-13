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
        subroutine get_partial_derivatives(opl6,ypc1,zpc1,wc,xlc,
     &  dfdwidlj,iord)
c---------------------------------------------------------------
c
c       Input:
c               opl6(w,l,y,z,yp,zp)
c
c       intermediate variable:
c               dfdwidlj0(i,j,w,l,y,z,yp,zp)
c
c       replace yp, zp:
c         Zwischenergebnis:      dfdwidlj1(i,j,w,l,y,z)
c
c       replace w,l:   
c
c         Output:                dfdwidlj(i,j,y,z,dy,dz)
c
c----------------------------------------------------------------

        implicit real*8(a-h,o-z)

        dimension opl6(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension dopl6(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension dopl4(0:7,0:7,0:7,0:7),dopl4a(0:7,0:7,0:7,0:7)
        dimension wc(0:7,0:7,0:7,0:7),
     &            xlc(0:7,0:7,0:7,0:7),
     &            ypc1(0:7,0:7,0:7,0:7),
     &            zpc1(0:7,0:7,0:7,0:7)
c        dimension dfdwidlj0(0:64,0:7,0:7,0:7,0:7,0:7,0:7)
        dimension dfdwidlj1(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension dfdwidlj(0:7,0:7,0:7,0:7,0:7,0:7)

c---------------------------------------------------------------
c
c       Die ersten 2 indizes werden zu einem zusammengefasst,
c       da der Kompliler maximal 7 Dimensionen verkraftet
c
c
c	Achtung!! checken ob bei den hoeheren Ableitungen die 
c	hoeheren Terme auch auf konsequent null gesetzt werden
c
c
c============= Routine wird zur Zeit nicht gebraucht. Sie wird erst benötigt,
c       wenn die komplette asymptotische Entwicklung realisiert wird.
c       dfdwidlj0 frisst zuviel Speicher. Das muss man dann spaeter neu programmieren.

        goto 1111

        do i1=0,iord
        do j1=0,iord-i1
        do k=0,iord
        do l=0,iord-k
        do m=0,iord
        do n=0,iord-m
c          dfdwidlj0(0,i1,j1,k,l,m,n)=opl6(i1,j1,k,l,m,n)
        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        do i=0,iord
        do j=0,iord
          if(i+j.gt.0)then
          index=i*(iord+1)+j
          call tay_deri_6_2(opl6,dopl6,1,i,2,j,iord)
          do i1=0,iord
          do j1=0,iord-i1
          do k=0,iord
          do l=0,iord-k
          do m=0,iord
          do n=0,iord-m
c            dfdwidlj0(index,i1,j1,k,l,m,n)=
c     &        dopl6(i1,j1,k,l,m,n)
          enddo
          enddo
          enddo
          enddo
          enddo
          enddo
          endif
        enddo
        enddo

c----------------- replace yp, zp

        do i=0,iord
        do j=0,iord
          index=i*(iord+1)+j
          do i1=0,iord
          do j1=0,iord-i1
          do k=0,iord
          do l=0,iord-k
          do m=0,iord
          do n=0,iord-m
c           dopl6(i1,j1,k,l,m,n)=dfdwidlj0(index,i1,j1,k,l,m,n)
          enddo
          enddo
          enddo
          enddo
          enddo
          enddo

          call replace_6v4v(dopl6,ypc1,zpc1,dopl4,iord)

          do i1=0,iord
          do j1=0,iord-i1
          do k=0,iord
          do l=0,iord-k
          do m=0,iord
          do n=0,iord-m
           dfdwidlj1(i,j,i1,j1,k,l)=dopl4(i1,j1,k,l)
          enddo
          enddo
          enddo
          enddo
          enddo
          enddo

        enddo
        enddo

c----------------- replace w, l

        do i=0,iord
        do j=0,iord

          do i1=0,iord
          do j1=0,iord-i1
          do k=0,iord
          do l=0,iord-k
            dopl4(i1,j1,k,l)=dfdwidlj1(i,j,i1,j1,k,l)
          enddo
          enddo
          enddo
          enddo

          call replace_wl_in_ypzp(dopl4,dopl4,wc,xlc,
     &          dopl4a,dopl4a,1,iord)

          do k=0,iord
          do l=0,iord-k
          do m=0,iord
          do n=0,iord-m
            dfdwidlj(i,j,k,l,m,n)=dopl4a(k,l,m,n)
          enddo
          enddo
          enddo
          enddo

        enddo
        enddo

1111    continue
c--------------------------------------------------------------

        return
        end

c--------------------------------------------------------
        subroutine get_dydz(a,g,eq27,eq28,iord)
c--------------------------------------------------------
c
c       Input:  a, g, iord
c       Output: eq27(y,z,dy,dz,w,l) und eq28(y,z,dy,dz,w,l)
c               daraus werden im Weiteren wc und xlc berechnet
c
c       i: y
c       j: z
c       k: dy
c       l: dz
c       m: w
c       n: l
c
c--------------------------------------------------------

        implicit real*8(a-h,o-z)

        TYPE geometryst
          real*8 sina,cosa,sinb,cosb,
     &            r,rp,xdens(0:4),xlam
          integer idefl
        end TYPE
        TYPE (geometryst) g

        dimension a(0:8,0:8)

        dimension eq27(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension eq27n(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension eq27dn(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension eq27dni(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension eq28(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension eq28n(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension eq28dn(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension eq28dni(0:7,0:7,0:7,0:7,0:7,0:7)

        xdefl=dble(g%idefl)
        
        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
        do m=0,iord-i-j-k-l
        do n=0,iord-i-j-k-l-m
          eq27n(i,j,k,l,m,n)=0.d0
          eq27dn(i,j,k,l,m,n)=0.d0
          eq28n(i,j,k,l,m,n)=0.d0
          eq28dn(i,j,k,l,m,n)=0.d0
        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

c------- eq27
c------- eq27 nominator
        do m=0,iord
        do n=0,iord-m
          eq27n(0,0,0,0,m,n)=-xdefl*g%sina*a(m,n)
        enddo
        enddo

        eq27n(0,0,0,0,1,0)=eq27n(0,0,0,0,1,0)+xdefl*g%cosa
        eq27n(1,0,0,0,0,0)=eq27n(1,0,0,0,0,0)-1.d0
       
c------- eq27 denominator
        do m=0,iord
        do n=0,iord-m
          eq27dn(0,0,0,0,m,n)=-g%cosa*a(m,n)
        enddo
        enddo

        eq27dn(0,0,0,0,0,0)=eq27dn(0,0,0,0,0,0)+g%r
        eq27dn(0,0,0,0,1,0)=eq27dn(0,0,0,0,1,0)-g%sina
       
c-------- eq27
        call Tay_inv_6(eq27dn,eq27dni,iord)
        call Tay_mult_6(eq27n,eq27dni,eq27,iord)

c------- eq28
c------- eq28 nominator

        eq28n(0,0,0,0,0,1)=eq28n(0,0,0,0,0,1)+1.d0
        eq28n(0,1,0,0,0,0)=eq28n(0,1,0,0,0,0)-1.d0
       
c------- eq28 denominator
        do m=0,iord
        do n=0,iord-m
          eq28dn(0,0,0,0,m,n)=-g%cosa*a(m,n)
        enddo
        enddo

        eq28dn(0,0,0,0,0,0)=eq28dn(0,0,0,0,0,0)+g%r
        eq28dn(0,0,0,0,1,0)=eq28dn(0,0,0,0,1,0)-g%sina

c-------- eq28
        call Tay_inv_6(eq28dn,eq28dni,iord)
        call Tay_mult_6(eq28n,eq28dni,eq28,iord)

        eq27(0,0,1,0,0,0)=eq27(0,0,1,0,0,0)-1.d0
        eq28(0,0,0,1,0,0)=eq28(0,0,0,1,0,0)-1.d0

        return
        end

c--------------------------------------------------------
        subroutine get_dypdzp(a,g,wc,xlc,
     &      ypc1,zpc1,dypc,dzpc,iord)
c--------------------------------------------------------
c
c       Input:  a,g,wc,xlc,ypc1,zpc1,iord
c       Output: dypc,dzpc
c
c       i: y
c       j: z
c       k: dy
c       l: dz
c       m: w
c       n: l
c
c--------------------------------------------------------

        implicit real*8(a-h,o-z)

        TYPE geometryst
          real*8 sina,cosa,sinb,cosb,
     &            r,rp,xdens(0:4),xlam
          integer idefl
        end TYPE
        TYPE (geometryst) g

        dimension a(0:8,0:8)
        dimension a1(0:7,0:7,0:7,0:7)

        dimension eq33(0:7,0:7,0:7,0:7)
        dimension eq33n(0:7,0:7,0:7,0:7)
        dimension eq33dn(0:7,0:7,0:7,0:7)
        dimension eq33dni(0:7,0:7,0:7,0:7)
        dimension eq34(0:7,0:7,0:7,0:7)
        dimension eq34n(0:7,0:7,0:7,0:7)
        dimension eq34dn(0:7,0:7,0:7,0:7)
        dimension eq34dni(0:7,0:7,0:7,0:7)

        dimension wc(0:7,0:7,0:7,0:7),
     &            xlc(0:7,0:7,0:7,0:7),
     &            ypc1(0:7,0:7,0:7,0:7),
     &            zpc1(0:7,0:7,0:7,0:7),
     &            dypc(0:7,0:7,0:7,0:7),
     &            dzpc(0:7,0:7,0:7,0:7)
        
c------------------ replace variables
c
c       0)      wc(y,z,dy,dz) and xlc(y,z,dy,dz) are known
c
c       1)      replace w and l in u(w,l) getting
c               a1(y,z,dy,dz)
        call replace_wl_in_u(a,wc,xlc,a1,iord)
c
c       2)       then, evaluate the quotient getting dypc and dzpc
c
c-----------------------------------------------------------------
        xdefl=dble(g%idefl)
         
        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
          eq33n(i,j,k,l)=0.d0
          eq33dn(i,j,k,l)=0.d0
          eq34n(i,j,k,l)=0.d0
          eq34dn(i,j,k,l)=0.d0
        enddo
        enddo
        enddo
        enddo

c------- eq33
c------- eq33 nominator
        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
          eq33n(i,j,k,l)=-xdefl*a1(i,j,k,l)*g%sinb
     &         +xdefl*wc(i,j,k,l)*g%cosb+ypc1(i,j,k,l)
        enddo
        enddo
        enddo
        enddo

c------- eq33 denominator
        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
          eq33dn(i,j,k,l)=-a1(i,j,k,l)*g%cosb
     &         -wc(i,j,k,l)*g%sinb
        enddo
        enddo
        enddo
        enddo

        eq33dn(0,0,0,0)=eq33dn(0,0,0,0)+g%rp
       
c-------- eq33
        call Tay_inv_4(eq33dn,eq33dni,iord)
        call Tay_mult_4(eq33n,eq33dni,dypc,iord)

c------- eq34
c------- eq34 nominator
        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
          eq34n(i,j,k,l)=-xlc(i,j,k,l)+zpc1(i,j,k,l)
        enddo
        enddo
        enddo
        enddo
       
c------- eq34 denominator
        do i=0,iord
        do j=0,iord-i
        do k=0,iord-i-j
        do l=0,iord-i-j-k
          eq34dn(i,j,k,l)=-wc(i,j,k,l)*g%sinb
     &                    -a1(i,j,k,l)*g%cosb
        enddo
        enddo
        enddo
        enddo

        eq34dn(0,0,0,0)=eq34dn(0,0,0,0)+g%rp
       
c-------- eq34
        call Tay_inv_4(eq34dn,eq34dni,iord)
        call Tay_mult_4(eq34n,eq34dni,dzpc,iord)

        return
        end

