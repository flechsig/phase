c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

c------------------------------------------------------
	subroutine make_matrix_8(xmap7,ypca,zpca,
     &     dypca,dzpca,iord)
c------------------------------------------------------
	implicit real*8(a-h,o-z)

	dimension ypca(0:7,0:7,0:7,0:7),
     &            zpca(0:7,0:7,0:7,0:7),
     &            dypca(0:7,0:7,0:7,0:7),
     &            dzpca(0:7,0:7,0:7,0:7)

	dimension xmap7(1:330,1:330)

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
	call matrix_dim(iord,idim)

c-------------- get cc row vectors

	do k=0,iord
	 if(k.eq.0)then

	 do l=0,iord-k
	  if((k.eq.0).and.(l.eq.0))then

	  do m=0,iord-k-l
	   if((k.eq.0).and.(l.eq.0).and.(m.eq.0))then

	   do n=0,iord-k-l-m
	    if((k.eq.0).and.(l.eq.0).and.
     &         (m.eq.0).and.(n.eq.0))then
	 	call Tay_fill_4(cc0,1.d0,iord)	! fuelle Arbeitsvektor
		call Tay_copy_vm_4(cc0,cc,k,l,m,n,iord)	! copy in erste Zeile
	       else
		call Tay_mult_4(cc0,dzpca,cc1,iord)
		call Tay_copy_vm_4(cc1,cc,k,l,m,n,iord)	! copy in erste Zeile
		call Tay_copy_4(cc1,cc0,iord)		
	    endif	! n-Schleife nur einmal durchlaufen
	   enddo	! und cc(0,0,0,n,idim) berechnen
	
	   else

	    do n=0,iord-k-l-m
	     call Tay_copy_mv_4(cc,cc0,k,l,m-1,n,iord)
	     call Tay_mult_4(cc0,dypca,cc1,iord)
	     call Tay_copy_vm_4(cc1,cc,k,l,m,n,iord)
	     call Tay_copy_4(cc1,cc0,iord)		
	    enddo
	   endif

	   enddo	! loop over m

	   else
	    do m=0,iord-k-l
	     do n=0,iord-k-l-m
	      call Tay_copy_mv_4(cc,cc0,k,l-1,m,n,iord)
	      call Tay_mult_4(cc0,zpca,cc1,iord)
	      call Tay_copy_vm_4(cc1,cc,k,l,m,n,iord)
	      call Tay_copy_4(cc1,cc0,iord)		
	     enddo
	    enddo
	   endif

	  enddo		! loop over l

	  else
	   do l=0,iord-k
	    do m=0,iord-k-l
	     do n=0,iord-k-l-m
	      call Tay_copy_mv_4(cc,cc0,k-1,l,m,n,iord)
	      call Tay_mult_4(cc0,ypca,cc1,iord)
	      call Tay_copy_vm_4(cc1,cc,k,l,m,n,iord) 
	      call Tay_copy_4(cc1,cc0,iord)		
	     enddo
	    enddo
	   enddo
	  endif

	 enddo		! loop over k

c---------------- replace first line
	xmap7(1,1)=1.d0
	do j=1,idim
	 xmap7(1,j)=0.d0
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
