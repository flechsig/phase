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

c------------------------------------------------------
	subroutine make_matrix_8(xmap7,ypca,zpca,
     &     dypca,dzpca,iord)
c------------------------------------------------------
c       input:  ypca, zpca, dypca, dzpca
c       output: xmap7
c------------------------------------------------------

	implicit real*8(a-h,o-z)

	dimension ypca(0:7,0:7,0:7,0:7),
     &            zpca(0:7,0:7,0:7,0:7),
     &            dypca(0:7,0:7,0:7,0:7),
     &            dzpca(0:7,0:7,0:7,0:7)

	dimension xmap7t(1:330,1:330),xmap7(1:330,1:330)

	dimension cc(0:7,0:7,0:7,0:7,330)
	dimension cc0(0:7,0:7,0:7,0:7)
	dimension cc1(0:7,0:7,0:7,0:7)

c----------------------------------------------------
c
c	Bedeutung der Indizes
c	i: Zeilen
c	j: Spalten
c	k: y
c	l: z
c	m: dy
c	n: dz
c
c--------------- get dimension of square matrix

c        ypca(0,0,0,0)=1111.
	
	idim=0

	call matrix_dim(iord,idim)

c-------------- get cc row vectors

c---------- loop over y
	do k=0,iord
	 if(k.eq.0)then

c---------- loop over z
	 do l=0,iord-k
	  if((k.eq.0).and.(l.eq.0))then

c---------- loop over dy
	  do m=0,iord-k-l
	   if((k.eq.0).and.(l.eq.0).and.(m.eq.0))then

c---------- loop over dz
	 do n=0,iord-k-l-m

	  if(n.eq.0)goto 1000
          if(n.eq.1)then
            call Tay_copy_4(dzpca,cc1,iord)
            call Tay_copy_vm_4(cc1,cc,k,l,m,n,iord)
            call Tay_copy_4(cc1,cc0,iord)
          else
	    call Tay_mult_4(cc0,dzpca,cc1,iord)
            call Tay_copy_vm_4(cc1,cc,k,l,m,n,iord) 
	    call Tay_copy_4(cc1,cc0,iord)		
	  endif	! n-Schleife nur einmal durchlaufen
1000     continue

	 enddo	! loop over dz, cc(0,0,0,n,idim) berechnen
	
	   else   ! k=0, l=0, m.ne.0
	 
	    do n=0,iord-k-l-m

            if((m.eq.1).and.(n.eq.0))then
              call Tay_copy_4(dypca,cc1,iord)
              call Tay_copy_vm_4(cc1,cc,k,l,m,n,iord)
              call Tay_copy_4(cc1,cc0,iord)          
            else
      	      call Tay_copy_mv_4(cc,cc0,k,l,m-1,n,iord)
              call Tay_mult_4(cc0,dypca,cc1,iord)
	      call Tay_copy_vm_4(cc1,cc,k,l,m,n,iord)
	      call Tay_copy_4(cc1,cc0,iord)		
            endif  
	    enddo
	   endif

	  enddo	! loop over m

	   else   ! k=0, l.ne.0

	    do m=0,iord-k-l
	     do n=0,iord-k-l-m
	    
	      if((l.eq.1).and.(m.eq.0).and.(n.eq.0))then
              call Tay_copy_4(zpca,cc1,iord)
              call Tay_copy_vm_4(cc1,cc,k,l,m,n,iord)
              call Tay_copy_4(cc1,cc0,iord)
            else 	     
	      call Tay_copy_mv_4(cc,cc0,k,l-1,m,n,iord) 
	      call Tay_mult_4(cc0,zpca,cc1,iord)
	      call Tay_copy_vm_4(cc1,cc,k,l,m,n,iord)
	      call Tay_copy_4(cc1,cc0,iord)		
            endif
	   enddo
	  enddo
	 endif

	 enddo		! loop over l

	  else        ! k.ne.0
	  
	   do l=0,iord-k
	    do m=0,iord-k-l
	     do n=0,iord-k-l-m

	    if((k.eq.1).and.(l.eq.0).and.(m.eq.0).and.(n.eq.0))then
              call Tay_copy_4(ypca,cc1,iord)
              call Tay_copy_vm_4(cc1,cc,k,l,m,n,iord)
              call Tay_copy_4(cc1,cc0,iord)
            else 	  
	      call Tay_copy_mv_4(cc,cc0,k-1,l,m,n,iord)
	      call Tay_mult_4(cc0,ypca,cc1,iord)	      
	      call Tay_copy_vm_4(cc1,cc,k,l,m,n,iord) 
	      call Tay_copy_4(cc1,cc0,iord)	
	    endif	
	     enddo
	    enddo
	   enddo
	  endif

	enddo		! loop over k

        goto 100

c====================================================

c---------------- replace first line
	xmap7t(1,1)=1.d0
	do j=2,idim
	 xmap7t(1,j)=0.d0
	enddo

c---------------- fill other lines

      iline=1
	  do k=0,iord
	  do l=0,iord-k
          do m=0,iord-k-l
	  do n=0,iord-k-l-m

          if(k+l+m+n.gt.0)then
          iline=iline+1
          icol=0
          do k1=0,iord
          do l1=0,iord-k1
          do m1=0,iord-k1-l1
          do n1=0,iord-k1-l1-m1
          icol=icol+1	   
	    xmap7t(iline,icol)=cc(k1,l1,m1,n1,iline)
          enddo
          enddo
          enddo
          enddo 
          endif
          
	  enddo
	  enddo
	  enddo
	  enddo


c=================================================

100     continue


c---------------- replace first column
        xmap7t(1,1)=1.d0
        do j=2,idim
         xmap7t(j,1)=0.d0
        enddo

c---------------- fill other columns

      iline=0
          do k=0,iord
          do l=0,iord-k
          do m=0,iord-k-l
          do n=0,iord-k-l-m

c          if(k+l+m+n.gt.0)then
          iline=iline+1
          icol=1
          do k1=0,iord
          do l1=0,iord-k1
          do m1=0,iord-k1-l1
          do n1=0,iord-k1-l1-m1
          if(k1+l1+m1+n1.gt.0)then
           icol=icol+1      
           xmap7t(iline,icol)=cc(k1,l1,m1,n1,iline)
          endif
          enddo
          enddo
          enddo
          enddo 
         
          enddo
          enddo
          enddo
          enddo

	do i=1,idim
	do j=1,idim
	  xmap7(i,j)=xmap7t(j,i)
	enddo
	enddo

	return
	end

c---------------------------------------------------
        subroutine matrix_dim(iord,idim)
c---------------------------------------------------

        idim=0

        do i=0,iord
         do j=0,iord-i
          do k=0,iord-i-j
           do l=0,iord-i-j-k
            idim=idim+1
           enddo
          enddo
         enddo
        enddo

        return
        end
