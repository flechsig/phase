c file from JB 1.7.2011
c ich habe ein Programm geschrieben, mit dem  man die Koeffizientenfiles fuer die horizonale Ablenkung
c (MAP35_lh.omx etc) fuer Ordnungen bis 7 berechnen kann (im Code muss man angeben, welche Ordnung man 
c haben will und ob die Ablenkung nach rechts oder links erfolgen soll). Das Beste waere es
c natuerlich, diesen Code in PHASE zu integrieren, damit man die Koeffizientenfiles, die ja immer mehr werden,
c nicht immer mit rumschleppen muss.	

c-----------------------------------------------------------------------------------
c     right hand deflection (rh): idefl = 1
c     left hand deflection (lh):  idefl = 2	
c-----------------------------------------------------------------------------------	
	
	implicit real*8(a-h,o-z)
	implicit integer(i-n)

        dimension ypc1(0:7,0:7,0:7,0:7),
     &            zpc1(0:7,0:7,0:7,0:7),
     &            dypc(0:7,0:7,0:7,0:7),
     &            dzpc(0:7,0:7,0:7,0:7),
     &            xmap7(1:330,1:330)

      iord = 4
      idefl=2
      
        	do i=0,iord
        	do j=0,iord
        	do k=0,iord
        	do l=0,iord
         	if((i+j+k+l).le.iord)then
                ypc1(i,j,k,l)=0.
                zpc1(i,j,k,l)=0.
                dypc(i,j,k,l)=0.
                dzpc(i,j,k,l)=0.
         	endif
        	enddo
        	enddo        
        	enddo
        	enddo        

c---------- rh deflection 

        if(idefl.eq.1)then
		ypc1(0,1,0,0)=1.
		zpc1(1,0,0,0)=-1.
		dypc(0,0,0,1)=1.
		dzpc(0,0,1,0)=-1.
		endif

c---------- lh deflection
        if(idefl.eq.2)then
		ypc1(0,1,0,0)=-1.
		zpc1(1,0,0,0)=1.
		dypc(0,0,0,1)=-1.
		dzpc(0,0,1,0)=1.
		endif
            
	call make_matrix(xmap7,ypc1,zpc1,dypc,dzpc,iord,idim)

c------------ write transformation matrix to file or keep it in memory

		  type*,' writing transformation matrix to mapn.OUT'
		  open(unit=10,name='mapn.out',type='unknown')
		  do i=1,idim
		  do j=1,idim
			write(10,*)i,j,xmap7(i,j)
		  enddo
		  enddo
		  close(10)

      stop
      end
      
c------------------------------------------------------
	subroutine make_matrix(xmap7,ypca,zpca,dypca,dzpca,iord,idim)
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
	 	call Tay_fill_4(cc0,0.d0,iord)	! fuelle Arbeitsvektor mit Nullen
	 	    cc0(0,0,0,0)=1.d0           ! ausser erstem Element
		call Tay_copy_vm_4(cc0,cc,k,l,m,n,iord)	! und kopiere ihn in die erste Zeile		
	       else
		call Tay_mult_4(cc0,dzpca,cc1,iord)

		call Tay_copy_vm_4(cc1,cc,k,l,m,n,iord)	! copy in naechste Zeile
		call Tay_copy_4(cc1,cc0,iord)		    ! und aufheben fuer naechste Multiplikation
		
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

	   enddo	! m-loop nur einmal durchlaufen

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

	  enddo		! l-loop nur einmal durchlaufen

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

c------------------------------------------------------

      ii=0
      do k=0,iord
       do l=0,iord-k
	  do m=0,iord-k-l
	   do n=0,iord-k-l-m   

         ii=ii+1      
         do j=1,idim
         xmap7(ii,j)=cc(k,l,m,n,j)
         enddo
   
         enddo
        enddo
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

      
      
c File      : /import/home/flechsig/phase/src/phase/create_hor_defl_file.for
 
