c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

c UF stack problem- example in doc: (default is 500 double values)
c    COMMON /CSTAK/DSTAK(1500)
c DOUBLE PRECISION DSTAK
c .
c .
c CALL ISTKIN(1500, 4)
c end uf


#include "pha4idlFORTRANdefines.h.f"

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	subroutine pha_src4_modgrid_nostructs( zezre,zezim,zeyre,zeyim
     &                                          ,nz1,nz2,zmin,zmax
     &                                          ,ny1,ny2,ymin,ymax )

	implicit none
	
c UF	common/cstak/dstak(6144)
c 	6144=6*1024
c	common/cstak/dstak(6*MaxDim)  | UF das geht nicht- man muss feste Werte eintragen 
	common/cstak/dstak

	integer k,nz2,ny2,nz1,ny1,k1
	real*8  dz1,dz2,dy1,dy2,ymax,ymin,zmax,zmin
     &	 ,z1(MaxDim),z2(MaxDim),y1(MaxDim),y2(MaxDim)
     &	 ,data_in(MaxDim),data_out(MaxDim)
     &	 ,dstak(STACK4SPLINE)
     
           
c      real*8 zezre(256,256),zezim(256,256)
c     &	,zeyre(256,256),zeyim(256,256)
     
	real*8 zezre(MaxDim,MaxDim),zezim(MaxDim,MaxDim)
     &	,zeyre(MaxDim,MaxDim),zeyim(MaxDim,MaxDim)


c      real*8 zezre(1024,1024),zezim(1024,1024)
c     &	,zeyre(1024,1024),zeyim(1024,1024)
	write(*,*)'debug: pha_src4_modgrid_nostructs called ...'
	write(*,*)'Changing number of grid-points ...'

c	call ISTKIN(NumberOfItems,Type) Type: 4 = double
c	call ISTKIN(6144,4)
c	call ISTKIN(6*MaxDim,4)                    | UF das sollte gehen
	call ISTKIN(STACK4SPLINE,4)
    

	write(*,*)'debug: after ISTKIN'
c in C:	call pha_extract_src4_grid(src4,nz1,zmin,zmax,ny1,ymin,ymax)
	
	dz1=(zmax-zmin)/(nz1-1)
	dy1=(ymax-ymin)/(ny1-1)
	do k=1,MaxDim	
	   z1(k)=0.d0
	   y1(k)=0.d0
	   z2(k)=0.d0
	   y2(k)=0.d0
	enddo
	
	do k=1,nz1
	   z1(k)=zmin + dble(k-1)*dz1
c	   write(*,*)'debug: z1(k): ', k, z1(k)
	enddo

	do k=1,ny1
	   y1(k)=ymin + dble(k-1)*dy1
	enddo


	dz2=(zmax-zmin)/(nz2-1)
	dy2=(ymax-ymin)/(ny2-1)
	
	do k=1,nz2
	   z2(k)=zmin + dble(k-1)*dz2
c	   write(*,*)'debug: z2(k): ', k, z2(k)
	enddo

	do k=1,ny2
	   y2(k)=ymin + dble(k-1)*dy2
	enddo

	do k=1,ny1  ! Interpoliere alle Vektoren in z-Richtung
		
#ifdef ABSOFT
#else
		data_in(1:nz1) = zezre(1:nz1,k)
		zezre(:,k)= 0.d0
		do k1=1,nz1
		   write(*,*)'debug: data_in(k): ', k1, data_in(k1)
		enddo
		write(*,*)'debug: call DCSPIN', nz1, nz2, z1(1), z2(1)
c UF dcspin needs 6*MaxDim double precision stack
c error 4 means: for some J, XX(J) does not lie inside the interval
c [X(1), X(N)]
c Usage: CALL CSPIN (X, Y, N, XX, YY, NN)
c X, Y are vectors, N the number of data
		call DCSPIN(z1,data_in,nz1,z2,data_out,nz2)
c uf		k1=127
c uf		call DCSPIN(z1,data_in,nz1,z2,data_out,k1)
		write(*,*)'debug: after DCSPIN'
		zezre(1:nz2,k)=data_out(1:nz2)
		
		data_in(1:nz1) = zezim(1:nz1,k)
		zezim(:,k)= 0.d0
		call DCSPIN(z1,data_in,nz1,z2,data_out,nz2)
		zezim(1:nz2,k)=data_out(1:nz2)
		
		data_in(1:nz1) = zeyre(1:nz1,k)
		zeyre(:,k)= 0.d0
		call DCSPIN(z1,data_in,nz1,z2,data_out,nz2)
		zeyre(1:nz2,k)=data_out(1:nz2)
		
		data_in(1:nz1) = zeyim(1:nz1,k)
		zeyim(:,k)= 0.d0
		call DCSPIN(z1,data_in,nz1,z2,data_out,nz2)
		zeyim(1:nz2,k)=data_out(1:nz2)
#endif	
	enddo
        do k=1,MaxDim
	data_in(k) =0.d0
	data_out(k)=0.d0
        enddo	
	do k=1,nz2  ! Interpoliere alle Vektoren in y-Richtung
			! Beachte: nz1 -> nz2 bereits durchgefuehrt 
		
#ifdef ABSOFT
#else
		data_in(1:ny1) = zezre(k,1:ny1)
		zezre(k,:)= 0.d0
		call DCSPIN(y1,data_in,ny1,y2,data_out,ny2)
		zezre(k,1:ny2)=data_out(1:ny2)
		
		data_in(1:ny1) = zezim(k,1:ny1)
		zezim(k,:)= 0.d0
		call DCSPIN(y1,data_in,ny1,y2,data_out,ny2)
		zezim(k,1:ny2)=data_out(1:ny2)
		
		data_in(1:ny1) = zeyre(k,1:ny1)
		zeyre(k,:)= 0.d0
		call DCSPIN(y1,data_in,ny1,y2,data_out,ny2)
		zeyre(k,1:ny2)=data_out(1:ny2)
		
		data_in(1:ny1) = zeyim(k,1:ny1)
		zeyim(k,:)= 0.d0
		call DCSPIN(y1,data_in,ny1,y2,data_out,ny2)
		zeyim(k,1:ny2)=data_out(1:ny2)
#endif	
	enddo
	
c in C:	call pha_define_src4_grid(src4,nz2,zmin,zmax,ny2,ymin,ymax)
      
	return
	end   !pha_src4_modgrid_structfree
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77






cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
c Cuts a part out of the grid ... from nz/ymin to nz/ymax
	subroutine pha_src4_cut_nostructs( zezre,zezim,zeyre,zeyim
     &                           , nz,zmin,zmax,nzmin,nzmax
     &                           , ny,ymin,ymax,nymin,nymax)

	
	implicit none

	integer nz,ny,nzmax,nzmin,nymax,nymin
	real*8  zmin,zmax,ymin,ymax,dz,dy
	
	real*8 zezre(MaxDim,MaxDim),zezim(MaxDim,MaxDim)
     &	,zeyre(MaxDim,MaxDim),zeyim(MaxDim,MaxDim)
     &	, temp(MaxDim,MaxDim)


c change indices from C to Fortran convention
      nzmin = nzmin + 1
      nzmax = nzmax + 1
      nymin = nymin + 1
      nymax = nymax + 1

c      write(*,*)'Enter pha_src4_cut_nostructs'
c      write(*,*)'nzmax, nzmin, nymax, nymin'
c      write(*,*) nzmax, nzmin, nymax, nymin
	if ((nzmax.gt.nzmin).and.(nymax.gt.nymin)) then
	! Eingangsbedingung !
	! Nur falls obere Grenzen goesser als untere wird gestartet ... !
	
	
c in C:call pha_extract_src4_grid(src4,nz1,zmin,zmax,ny1,ymin,ymax)
	if (nzmax.gt.nz) nzmax=nz
	if (nzmin.lt.1 ) nzmin=1
	if (nymax.gt.ny) nymax=ny
	if (nymin.lt.1 ) nymin=1
	
c	write(*,*)'Cutting grid ...'
	
	dz=(zmax-zmin)/(nz-1)
      dy=(ymax-ymin)/(ny-1)
      
      zmin=zmin+(nzmin-1)*dz
      zmax=zmax-(nz-nzmax)*dz
      
      ymin=ymin+(nymin-1)*dy
      ymax=ymax-(ny-nymax)*dy        

      
	nz=nzmax-nzmin+1
	ny=nymax-nymin+1
c      write(*,*)'nz, ny'
c      write(*,*) nz, ny

	temp(1:nz,1:ny)   =  zeyre(nzmin:nzmax,nymin:nymax)
	zeyre(:,:)     =  0
	zeyre(1:nz,1:ny)=temp(1:nz,1:ny)
	
	temp(1:nz,1:ny)   =  zeyim(nzmin:nzmax,nymin:nymax)
	zeyim(:,:)     =  0
	zeyim(1:nz,1:ny)=temp(1:nz,1:ny)
	
	temp(1:nz,1:ny)   =  zezre(nzmin:nzmax,nymin:nymax)
	zezre(:,:)     =  0
	zezre(1:nz,1:ny)=temp(1:nz,1:ny)
	
	temp(1:nz,1:ny)   =  zezim(nzmin:nzmax,nymin:nymax)
	zezim(:,:)     =  0
	zezim(1:nz,1:ny)=temp(1:nz,1:ny)

c in C:call pha_define_src4_grid(src4,nz2,zmin,zmax,ny2,ymin,ymax  )	


	endif ! Eingangsbedingung !
	return
	end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77




cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	subroutine pha_src4_addzeros_nostructs( zezre,zezim,zeyre,zeyim
     &                           , nz1,nz2,zmin,zmax
     &                           , ny1,ny2,ymin,ymax)


	implicit none
	
	integer nz2,ny2,nz1,ny1,nz_delta,ny_delta
	real*8  zmin,zmax,ymin,ymax,dz,dy
	
	real*8 zezre(MaxDim,MaxDim),zezim(MaxDim,MaxDim)
     &	,zeyre(MaxDim,MaxDim),zeyim(MaxDim,MaxDim)
     &	, temp(MaxDim,MaxDim)

	


c           ,dz1,dy1
	
	write(*,*)'Adding zeros ...'
c	write(*,*)nz1, nz2
c	write(*,*)ny1, ny2
	
c in c:call pha_extract_src4_grid(src4,nz1,zmin,zmax,ny1,ymin,ymax)
	
	if (nz2.lt.nz1) nz2=nz1
	if (ny2.lt.ny1) ny2=ny1
	! falls eine Grenze kleiner stuerzts ab...
	! Setze kleinere Grenze auf Urspruengliche 

      if (mod((nz2-nz1),2).eq.1) nz2=nz2-1  ! nxd=delta(nx1,nx2)=gerade
      if (mod((ny2-ny1),2).eq.1) ny2=ny2-1
	
      nz_delta=(nz2-nz1)/2    ! Halber Unterschied der Anzahl der Datenpunkte
      ny_delta=(ny2-ny1)/2    ! Immer ganzzahlig, da N_delta gerade oben 
					! erzwungen wird

	    dz=(zmax-zmin)/(nz1-1)
      dy=(ymax-ymin)/(ny1-1)
      
      zmin=zmin-nz_delta*dz
      zmax=zmax+nz_delta*dz
      
      ymin=ymin-ny_delta*dy
      ymax=ymax+ny_delta*dy

	    write(*,*)'New boundaries:'
    	write(*,*)zmin, zmax
    	write(*,*)ymin, ymax
    
c..... AddZeros.....
#ifdef ABSOFT
#else
	temp(1:nz1,1:ny1)   =   zeyre(1:nz1,1:ny1)
	 zeyre(:,:)     =  0
	 zeyre(1+nz_delta:1+nz_delta+nz1,1+ny_delta:1+ny_delta+ny1) =
     &		 temp(1:ny1,1:nz1)
      
	temp(1:nz1,1:ny1)   =   zeyim(1:nz1,1:ny1)
	 zeyim(:,:)     =  0
	 zeyim(1+nz_delta:1+nz_delta+nz1,1+ny_delta:1+ny_delta+ny1) =
     &		 temp(1:ny1,1:nz1)
      
	temp(1:nz1,1:ny1)   =   zezre(1:nz1,1:ny1)
	 zezre(:,:)     =  0
	 zezre(1+nz_delta:1+nz_delta+nz1,1+ny_delta:1+ny_delta+ny1) =
     &		 temp(1:ny1,1:nz1)
    
	temp(1:nz1,1:ny1)   =   zezim(1:nz1,1:ny1)
	 zezim(:,:)     =  0
	 zezim(1+nz_delta:1+nz_delta+nz1,1+ny_delta:1+ny_delta+ny1) =
     &		 temp(1:ny1,1:nz1)
#endif     
c in C:call pha_define_src4_grid(src4,nz2,zmin,zmax,ny2,ymin,ymax)

	return
	end    ! pha_src4_addzero
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77




