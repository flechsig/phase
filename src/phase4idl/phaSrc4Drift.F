c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77










ccc Verwende vorlaufig prefix IN* fuer uebergabevariablen um konflikte 
ccc   mit namen im commonblock zuvermeiden




ccc   DER Common block kann raus, falls auf unterroutinen verzichtet wird ...

ccc  Die unterroutine remap verwendet den commonblock -> rausfinden, was wirklich benoetigt wird!
ccc  Rest raus !!!





	
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
c      subroutine phadrift_propagate_fk( beam4 ,dist 
c     &				,nz2 ,zmin2 ,zmax2 ,ny2 ,ymin2 ,ymax2)
	subroutine phadrift_propagate_fk_nostructs( MaxDim
     &					,INezre,INezim,INeyre,INeyim
     &					,nz1,zmin1,zmax1,ny1,ymin1,ymax1     
     &					,nz2,zmin2,zmax2,ny2,ymin2,ymax2
     &					,INdist, INxlam)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
c	z/y-min/max 1 : old grid  ||  z/y-min/max 2 : new grid
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	implicit none
      

c      integer i,j,k,l,nz2,ny2
c      real*8 zmin2,zmax2,ymin2,ymax2
c	real*8 phase, dist,dist1
	
c      real*8  pi,pihalf,signum,const,a1_sav
	
	
	
      integer i,j,k,l
     &       ,nz1,ny1,nz2,ny2
     &       ,MaxDim

ccccc DEBUG ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      real*8  small, big, tmp
ccccc DEBUG ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      real*8  pi,pihalf
     &       , distance, INdist
     &       , dist1, phase
     &       , xlam,     INxlam
     &       ,   zmin1,   zmax1,   ymin1,   ymax1
     &       ,   zmin2,   zmax2,   ymin2,   ymax2
     &       , dy1  , y1(MaxDim) 
     &       , dz1  , z1(MaxDim)     
     &       , dy2  , y2(MaxDim) 
     &       , dz2  , z2(MaxDim)     
     &       ,INeyre(MaxDim,MaxDim), INeyim(MaxDim,MaxDim)
     &       ,INezre(MaxDim,MaxDim), INezim(MaxDim,MaxDim)
     &       , factre,factim,a10

      complex*16  cc,xm1,sqrtm1,fact,quad_fac,quad_fac_1
     &       ,ey1(MaxDim,MaxDim), ez1(MaxDim,MaxDim)
     &       ,ey2(MaxDim,MaxDim), ez2(MaxDim,MaxDim)
    
 


	write(*,*)'phaPropagateFresnellKirchhoff started...' 

ccccc DEBUG ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc	
	small = 1.0d-16
	big   = 1.0d+16
ccccc DEBUG ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	
	pi=4.d0*datan(1.d0)
	pihalf=pi/2.d0
	sqrtm1=dcmplx(0,1)
        distance=dble(INdist)
	xlam=dble(INxlam)*1.0d-6
	  cc=(2.d0*pi)/xlam
c	write(*,*),xlam,' ... xlam'  
	do i=1, MaxDim
	  do j=1, MaxDim
	    ez1(i,j)=dcmplx( INezre(i,j) , INezim(i,j) )
	    ey1(i,j)=dcmplx( INeyre(i,j) , INeyim(i,j) )
        enddo
	enddo

	dy1=(ymax1-ymin1)/dble( ny1-1 )
	dz1=(zmax1-zmin1)/dble( nz1-1 )
c	write(*,*),dy1,' ...dy1 ',dz1,' ... dz1'  
	do i=1, nz1
	   z1(i)=zmin1+dble(i-1)*dz1
	enddo
	do i=1, ny1
	   y1(i)=ymin1+dble(i-1)*dy1
        enddo   

	dy2=(ymax2-ymin2)/dble(ny2-1)
	dz2=(zmax2-zmin2)/dble(nz2-1)
c	write(*,*),dy2,' ...dy2 ',dz2,' ... dz2'   
   
ccc   START PROPAGATE FK    ccc
	do i=1,ny2
          y2(i)=ymin2+dble(i-1)*dy2      
          do j=1,nz2
            z2(j)=zmin2+dble(j-1)*dz2    
            ey2(i,j)=0.d0
            ez2(i,j)=0.d0
            do k=1,ny1
            do l=1,nz1

c    Intrinsische Wurzelfkt. dsqrt ist schneller als Entwicklung ...
c    dsqrt benoetigt nur rund 66% der Zeit, die eine Entwicklung zur 5.Ordnung braucht !    

ccccc DEBUG ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      tmp=distance**2 + dabs(y1(k)-y2(i))**2 + dabs(z1(l)-z2(j))**2
	if (tmp.lt.small) then
		write(*,*) tmp,' ... small sqrtarg at (i,j,k,l) ',i,j,k,l
c		fact=cmplx(0,0)
	endif

	if (tmp.gt.big)  then
		write(*,*)tmp,' ...   big sqrtarg at (i,j,k,l) ',i,j,k,l
c		fact=cmplx(0,0)
	endif  
ccccc DEBUG ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      dist1=dsqrt( distance**2 + dabs(y1(k)-y2(i))**2 + dabs(z1(l)-z2(j))**2 )
     &     - dabs(distance)

      phase=dist1*cc
      fact=cmplx( dcos(phase) , dsin(phase) )/(dist1+distance)
      fact=(fact*dz1*dy1)/xlam

ccccc DEBUG ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      tmp=dsqrt(dreal(fact)**2 + dimag(fact)**2) 	
	if (tmp.lt.small) then
		write(*,*)fact,' ... small fact at (i,j,k,l) ',i,j,k,l
c		fact=cmplx(0,0)
	endif

	if (tmp.gt.big)  then
		write(*,*)fact,' ...   big fact at (i,j,k,l) ',i,j,k,l
c		fact=cmplx(0,0)
	endif
ccccc DEBUG ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c	write(*,*),fact,' ...fact'
c      if((i.eq.(ianzy+1)/2).and.(j.eq.(ianzz+1)/2).and.
c     &        (k.eq.(ianzy1+1)/2))then 
c        xarg(l)=dreal(ez(l,k)*fact)
c      endif

      ey2(i,j)=ey2(i,j)+ey1(k,l)*fact            
      ez2(i,j)=ez2(i,j)+ez1(k,l)*fact            
      
              enddo
            enddo
        enddo
      enddo
ccc   END PROPAGATE FK   ccc
      
	do j=1,MaxDim
	  do i=1,MaxDim
	    INezre(i,j)=dreal(ez2(i,j))
	    INezim(i,j)=dimag(ez2(i,j))
	    INeyre(i,j)=dreal(ey2(i,j))
	    INeyim(i,j)=dimag(ey2(i,j))
	  enddo
	enddo	  

      write(*,*)'phaPropagateFresnellKirchhoff finished...'
      write(*,*)'.'
      end !phadrift_propagate_fk  (prop1)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77













ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	subroutine phadrift_propagate_fft_near_nostructs(MaxDim
     &						,INezre,INezim,INeyre,INeyim
     &						,INnz,INzmin,INzmax
     &						,INny,INymin,INymax
     &						,dist, INxlam)
c****************************************************************************
c
c	During FFT the integration is performed 
c	1. in y- direction
c	2. in z- direction
c
c	Routine uses the FFT-routine DMFTCC from PORT-3 library
c	DMFTCC performs a Fourier transform of a set of 
c	nns complex vectors with length of n each
c
c	n	= dimension of each vector
c	nns 	= number of vectors to be transformed
c	a	= matrix of real parts of input data
c	b	= matrix of imaginary parts of input data
c	iab	= the spacing between elements within each input vector
c		  a and b
c	jab	= the spacing between independent output vectors a and b
c	c	= matrix of real parts of output data
c	d	= matrix of imaginary parts of output data
c	icd	= the spacing between elements within each output vector
c		  c and d
c	jcd	= the spacing between independent output vectors c and d
c
c	maximum array size: 512 * 512
c	dimensions of array should be products of small prime factors
c	(e.g. factors of 2 and 3) 
c
c------------------------------------------------------------------------
	implicit none !real*8(a-h,o-z)
	
	common/cstak/dstak

      integer i,j,k,l,icomp
     &       ,ifx(25), nz00, ny00
     &       , INnz, INny
     &       , iyz,nflag,MaxDim,nz,ny

      real*8  pi,pihalf,signum,const,a1_sav
     &       ,alpha,dalpha,beta,dbeta
     &       ,dstak(524288),t(2048)
     &       ,y_1_min,y_1_max,z_1_min,z_1_max
     &       ,dist,distance
     &       ,width,ycenter,zcenter
     &       , xlam, INxlam
     &       , INzmin, INzmax, INymin, INymax
     &       ,   zmin,   zmax,   ymin,   ymax
     &       , dy , y(1024) 
     &       , dz , z(1024)     
     &       , zmin_1, zmax_1, ymin_1, ymax_1
     &       , dy_1  , y_1(1024) 
     &       , dz_1  , z_1(1024)     
     &       ,INeyre(MaxDim,MaxDim), INeyim(MaxDim,MaxDim)
     &       ,INezre(MaxDim,MaxDim), INezim(MaxDim,MaxDim)
     &       ,a1  (1024,1024)  , b1  (1024,1024)
     &       ,a2  (1024,1024)  , b2  (1024,1024)
     &       ,atmp(1024,1024)  , btmp(1024,1024)   
     &       , factre,factim,a10



      complex*16  cc,xm1,sqrtm1,fact,quad_fac,quad_fac_1
    
      
	 ny00=1024
	 nz00=1024

c------------ allocate storage
	call istkin(524288,4)	! (2*n*nns, itype=double)
	
	write(*,*)'phaPropagateFFTnear started...'
	
	distance=dble(dist)

	
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
c      	call phadrift_get_input(beam4)
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	pi=4.d0*datan(1.d0)
	pihalf=pi/2.d0
	sqrtm1=dcmplx(0,1)
        distance=dble(dist)
c	write(*,*),INxlam,' ... INxlam'	  
	xlam=dble(INxlam)*1.0d-6
	  cc=(2.d0*pi)/xlam
c	write(*,*),xlam,' ... xlam'	  
	
	zmin=INzmin
	zmax=INzmax
	ymax=INymax	
	zmin=INymin
	
	 nz=INnz
	 ny=INny
	
	dy=(ymax-ymin)/dble( ny-1)
c	write(*,*),dy,' ... dy'
	dz=(zmax-zmin)/dble( nz-1)
c	write(*,*),dz,' ... dz'	
	do i=1, nz
	   z(i)=zmin+dble(i-1)*dz
	enddo
	do i=1, ny
	   y(i)=ymin+dble(i-1)*dy
      enddo   
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
c      END of call phadrift_get_input(beam4)
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77	
	
	dy_1=dy
	dz_1=dz

c	a1/2 Real Fields ... b1/2 Complex Parts 
	do j=1,MaxDim
	  do i=1,MaxDim
             a1(j,i)=INeyre(j,i)
             b1(j,i)=INeyim(j,i)
             a2(j,i)=INezre(j,i)
             b2(j,i)=INezim(j,i)
	  enddo
	enddo

c-- actualize coordinates
	y_1(:)=y(:)
        z_1(:)=z(:)

	do icomp=1,2	! loop over y and z-component of electric field
c	  start with ey component
c	  then ez component

	if(icomp.eq.2)then
	  do k=1,ny
	  do l=1,nz
	    a1(l,k)=a2(l,k)
	    b1(l,k)=b2(l,k)
	  enddo
	  enddo
	endif

c	bis hier ok, beim 2. Durchgang werden a1 b1 wieder richtig 
c	initialisiert

c------------ step 1: FFT
	signum=-1.d0
c---------- get prime factors of n
	call DMFTCI(ny,ifx,t)

	call DMFTCC(ny,nz,a1(1,1),b1(1,1),1,ny00,
     &		    a1(1,1),b1(1,1),1,ny00,ifx,t,signum)

	call DMFTCC(ny,nz,a1(1,1),b1(1,1),nz00,1,
     & 		          a1(1,1),b1(1,1),nz00,1,ifx,t,signum)


c---- jetzt haben wir die 2D-Wikelverteilung, die noch ausgewertet werden 
c---- muss

c------------ step 2: phase modification

	dalpha=xlam/(dy*dflotj(ny))
c	write(*,*),dalpha,' ... dalpha'
	dbeta=xlam/(dz*dflotj(nz))
c	write(*,*),dbeta,' ... dbeta'

	do i=1,ny
	do j=1,nz
	  if(i.le.ny/2)then
	    alpha=dflotj(i-1)*dalpha
	    else
	    alpha=-dflotj(ny-i+1)*dalpha
	  endif
	  
	  if(j.le.nz/2)then
	    beta=dflotj(j-1)*dbeta
	    else
	    beta=-dflotj(nz-j+1)*dbeta
	  endif

	  factre=dcos(((2.d0*pi)/xlam)*
     &		distance*dsqrt(1.d0-alpha**2-beta**2))
	  factim=dsin(((2.d0*pi)/xlam)*
     &		distance*dsqrt(1.d0-alpha**2-beta**2))

c	write(*,*),factre,' ... factre'	  
c	write(*,*),factim,' ... factim'
	  a10=a1(i,j)
	  a1(i,j)=a1(i,j)*factre-b1(i,j)*factim
	  b1(i,j)=a10*factim+b1(i,j)*factre

	enddo
	enddo


c------------ step 3: FFTI
	
	call DMFTCC(ny,nz,a1(1,1),b1(1,1),1,ny00,
     &		          a1(1,1),b1(1,1),1,ny00,ifx,t,-signum)
	call DMFTCC(ny,nz,a1(1,1),b1(1,1),nz00,1,
     &		          a1(1,1),b1(1,1),nz00,1,ifx,t,-signum)

	do i=1,ny
	do j=1,ny
	  a1(i,j)=a1(i,j)/dflotj(ny*nz)
	  b1(i,j)=b1(i,j)/dflotj(ny*nz)
	enddo
	enddo


	if(icomp.eq.1)then
	  do k=1,ny
	  do l=1,nz
	    INeyre(l,k)=a1(l,k)
	    INeyim(l,k)=b1(l,k)
	  enddo
	  enddo
	endif

	if(icomp.eq.2)then

	  do k=1,ny
	  do l=1,nz
	    INezre(l,k)=a1(l,k)
	    INezim(l,k)=b1(l,k)
	  enddo
	  enddo

	endif

	enddo		! icomp, loop over components of electric field
	
	write(*,*)'phaPropagateFFTnear finished...'
	write(*,*)' '
	return
	end   !phadrift_propagate_fft_near (prop2)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	
	





ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
c	subroutine phadrift_propagate_fft_far( beam4 ,dist ) 
	subroutine phadrift_propagate_fft_far_nostructs(MaxDim
     &						,INezre,INezim,INeyre,INeyim
     &						,INnz,INzmin,INzmax
     &						,INny,INymin,INymax
     &						,dist, INxlam)
c****************************************************************************
c
c	During FFT the integration is performed 
c	1. in y- direction
c	2. in z- direction
c
c	Routine uses the FFT-routine DMFTCC from PORT-3 library
c	DMFTCC performs a Fourier transform of a set of 
c	nns complex vectors with length of n each
c
c	n	= dimension of each vector
c	nns 	= number of vectors to be transformed
c	a	= matrix of real parts of input data
c	b	= matrix of imaginary parts of input data
c	iab	= the spacing between elements within each input vector
c		  a and b
c	jab	= the spacing between independent output vectors a and b
c	c	= matrix of real parts of output data
c	d	= matrix of imaginary parts of output data
c	icd	= the spacing between elements within each output vector
c		  c and d
c	jcd	= the spacing between independent output vectors c and d
c
c	maximum array size: 512 * 512
c	dimensions of array should be products of small prime factors
c	(e.g. factors of 2 and 3) 
c
c
c	the number of gridpoints is observed during this routine, but
c		dx,dy and therefore the axis changes !!!
c
c------------------------------------------------------------------------
	
	implicit none 
	
	common/cstak/dstak

      integer i,j,k,l,icomp
     &       ,ifx(25), nz00, ny00
     &       , INnz, INny
     &       , iyz,nflag,MaxDim,nz,ny

      real*8  pi,pihalf,signum,const,a1_sav
     &       ,alpha,dalpha,beta,dbeta
     &       ,dstak(524288),t(2048)
     &       ,y_1_min,y_1_max,z_1_min,z_1_max
     &       ,dist,distance
     &       ,width,ycenter,zcenter
     &       , xlam, INxlam
     &       , INzmin, INzmax, INymin, INymax
     &       ,   zmin,   zmax,   ymin,   ymax
     &       , dy , y(1024) 
     &       , dz , z(1024)     
     &       , zmin_1, zmax_1, ymin_1, ymax_1
     &       , dy_1  , y_1(1024) 
     &       , dz_1  , z_1(MaxDim)     
     &       ,INeyre(MaxDim,MaxDim), INeyim(MaxDim,MaxDim)
     &       ,INezre(MaxDim,MaxDim), INezim(MaxDim,MaxDim)
     &       ,a1  (1024,1024)  , b1  (1024,1024)
     &       ,a2  (1024,1024)  , b2  (1024,1024)
     &       ,atmp(1024,1024)  , btmp(1024,1024)   

      complex*16  cc,xm1,sqrtm1,fact,quad_fac,quad_fac_1
    
      
      write(*,*)'phaPropagateFFTfar started...'

	 ny00=1024
	 nz00=1024
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
c      	call phadrift_get_input(beam4)
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	pi=4.d0*datan(1.d0)
	pihalf=pi/2.d0
	sqrtm1=dcmplx(0,1)
      distance=dble(dist)
	xlam=dble(INxlam)*1.0d-6
	  cc=(2.d0*pi)/xlam
c	write(*,*),xlam,' ...xlam'  
	zmin=INzmin
	zmax=INzmax
	ymax=INymax	
	zmin=INymin
	  nz=INnz
	  ny=INny

	dy=(ymax-ymin)/dble( ny-1)
	dz=(zmax-zmin)/dble( nz-1)
	
	do i=1, nz
	   z(i)=zmin+dble(i-1)*dz
	enddo
	do i=1, ny
	   y(i)=ymin+dble(i-1)*dy
      enddo   
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
c      END of call phadrift_get_input(beam4)
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77

c	a1/2 Real Fields ... b1/2 Complex Parts 
	do j=1,MaxDim
	  do i=1,MaxDim
             a1(j,i)=INeyre(j,i)
             b1(j,i)=INeyim(j,i)
             a2(j,i)=INezre(j,i)
             b2(j,i)=INezim(j,i)
	  enddo
	enddo

c------------ allocate storage
	call istkin(524288,4)	! (2*n*nns, itype=double)
c	call istkin(512*MaxDim,4)	! (2*n*nns, itype=double)
c-----	get new grid points
c
c      calculations -> GLAD-Manual:  dx2 = (xlam*dist)/(N*dx1)
c        ->  Width of new Interval:   w2 = (N-1)*dx2 

c --- y-axis
	   ycenter = (ymax+ymin)/2.d0 
	     width = dble( ny-1)*(xlam*distance/(dble( ny)*dy))
	   y_1_min = ycenter - 0.5d0*width 
	   y_1_max = ycenter + 0.5d0*width
	      dy_1 = width / dble( ny-1)
	
	do k=1, ny
	    y_1(k) = y_1_min + dble(k-1)*dy_1
	enddo
	
c --- z-axis
	   zcenter = (zmax+zmin)/2.d0 
	     width = dble( nz-1)*(xlam*distance/(dble( nz)*dz))
	   z_1_min = zcenter - 0.5d0*width 
	   z_1_max = zcenter + 0.5d0*width
	      dz_1 = width / dble( nz-1)
	
	do l=1, nz
          z_1(l) = z_1_min + dble(l-1)*dz_1
	enddo
c
c -----------------  New Grid calculated ...
	
	
	do icomp=1,2	! loop over y and z-component of electric field
c	  start with ey component
c	  then ez component

	if(icomp.eq.2)then
	  do k=1, ny
	  do l=1, nz
	    a1(l,k)=a2(l,k)
	    b1(l,k)=b2(l,k)
	  enddo
	  enddo
	endif

c------------ step 1: apply quadratic factor

	do k=1, ny
	do l=1, nz
	if((jmod( ny,2).eq.1).and.(jmod( nz,2).eq.1))then
	  quad_fac=exp(((sqrtm1*2.d0*pi)/xlam)*
     &              (((z(l)-zcenter)**2 + 
     &                (y(k)-ycenter)**2)/(2.d0*distance)))
	endif
	if((jmod( ny,2).eq.0).and.(jmod( nz,2).eq.0))then
	  quad_fac=exp(((sqrtm1*2.d0*pi)/xlam)*
     &        (((z(l)-zcenter-0.5d0*dz)**2 + 
     &          (y(k)-ycenter-0.5d0*dy)**2)/(2.d0*distance)))
	endif

	  a1_sav=a1(l,k)
	  a1(l,k)=a1(l,k)*dreal(quad_fac)-b1(l,k)*dimag(quad_fac)
	  b1(l,k)=a1_sav*dimag(quad_fac)+b1(l,k)*dreal(quad_fac)
	enddo
	enddo

c------------ step 2: mapping of input data to get central distribution
c	only cos terms, symmetrically around zero
c

c Joh:	call phadrift_remap(1,2)
	call phadrift_remap(1,2,
     &                    nz,ny,
     &                    a1,b1,atmp,btmp )

c Joh:	call phadrift_remap(2,2)
	call phadrift_remap(2,2,
     &                    nz,ny,
     &                    a1,b1,atmp,btmp )
	
c------------ step 3: FFT
	if(distance.gt.0.d0)then
	  signum=-1.d0
  	  else
	  signum=1.d0
	endif

c---------- get prime factors of n
	call DMFTCI( ny,ifx,t)

	call DMFTCC( ny, nz,a1(1,1),b1(1,1),1, ny00,
     &		        a1(1,1),b1(1,1),1, ny00,ifx,t,signum)

	call DMFTCC( ny, nz,a1(1,1),b1(1,1), nz00,1,
     & 		        a1(1,1),b1(1,1), nz00,1,ifx,t,signum)


c-------------- step 4: phadrift_remapping of a1 and b1 in z
c Joh:	call phadrift_remap(1,1)
	call phadrift_remap(1,1,
     &                    nz,ny,
     &                    a1,b1,atmp,btmp )
c------------- remapping in y
c Joh:	call phadrift_remap(2,1)
	call phadrift_remap(2,1,
     &                    nz,ny,
     &                    a1,b1,atmp,btmp )


c------------ step 5: multiplication with prefactor

	do k=1, ny
	do l=1, nz
	if((jmod( ny,2).eq.1).and.(jmod( nz,2).eq.1))then
	  quad_fac_1=exp(((sqrtm1*2.d0*pi)/xlam)*
     &              (((z_1(l)-zcenter)**2 + 
     &                (y_1(k)-ycenter)**2)/(2.d0*distance)))
	endif

	if((jmod( ny,2).eq.0).and.(jmod( nz,2).eq.0))then
	  quad_fac_1=exp(((sqrtm1*2.d0*pi)/xlam)*
     &              (((z_1(l)-zcenter-0.5d0*dz)**2+
     &                (y_1(k)-ycenter-0.5d0*dy)**2)/(2.d0*distance)))
	endif

	  quad_fac_1=(quad_fac_1/(sqrtm1*xlam*distance))*
     &               exp((sqrtm1*3.d0*pi*distance)/xlam)
	  a1_sav=a1(l,k)
	  a1(l,k)=a1(l,k)*dreal(quad_fac_1)-b1(l,k)*dimag(quad_fac_1)
	  b1(l,k)=a1_sav*dimag(quad_fac_1)+b1(l,k)*dreal(quad_fac_1)

	  a1(l,k)=a1(l,k)*dy*dz
	  b1(l,k)=b1(l,k)*dy*dz

	enddo
	enddo

	if(icomp.eq.1)then
	  do k=1, ny
	  do l=1, nz
	    INeyre(l,k)=a1(k,l) 
	    INeyim(l,k)=b1(k,l) 
	  enddo
	  enddo
	endif

	if(icomp.eq.2)then
	  do k=1, ny
	  do l=1, nz
	    INezre(l,k)=a1(l,k)
	    INezim(l,k)=b1(l,k)
	  enddo
	  enddo
	endif

	enddo		! icomp, loop over components of electric field

c --- New Gridparameters ...
	INymin=y_1_min      
        INymax=y_1_max
	INzmin=z_1_min
	INzmax=z_1_max
	
	write(*,*)'phaPropagateFFTfar finished...'
	write(*,*)' '
	return
	end   ! phadrift_propagate_fft_far (prop3)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77





ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	subroutine phadrift_remap(iyz,nflag,
     &                          nz,ny,
     &                          a1,b1,a11,b11)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	
	integer k,l,iyz,nflag,nz,ny
	
	real*8  a1 (1024,1024),b1 (1024,1024)
     &         ,a11(1024,1024),b11(1024,1024)   

c------------------------------
	if(iyz.eq.1)then
c------------ phadrift_remap z

	if(nflag.eq.1)then
	if(jmod( nz,2).eq.1)then	! ungerade
	do k=1, ny
	do l=1,( nz+1)/2
	  a11(l+( nz+1)/2-1,k)=a1(l,k)	
	  b11(l+( nz+1)/2-1,k)=b1(l,k)	
	enddo	
	enddo	

	do k=1, ny
	do l=( nz+1)/2+1, nz
	  a11(l-( nz+1)/2,k)=a1(l,k)
	  b11(l-( nz+1)/2,k)=b1(l,k)
	enddo	
	enddo	
	endif				! ungerade
	endif		!nflag=1

	if(nflag.eq.2)then
	if(jmod( nz,2).eq.1)then	! ungerade
	do k=1, ny
	do l=1,( nz+1)/2-1
	  a11(l+( nz+1)/2,k)=a1(l,k)	
	  b11(l+( nz+1)/2,k)=b1(l,k)	
	enddo	
	enddo	

	do k=1, ny
	do l=( nz+1)/2, nz
	  a11(l-( nz+1)/2+1,k)=a1(l,k)
	  b11(l-( nz+1)/2+1,k)=b1(l,k)
	enddo	
	enddo	
	endif				! ungerade
	endif		!nflag=2


	if(nflag.eq.1)then
	if(jmod( nz,2).eq.0)then	! gerade
	do k=1, ny
	do l=1, nz/2
	  a11(l+( nz)/2,k)=a1(l,k)	
	  b11(l+( nz)/2,k)=b1(l,k)	
	enddo	
	enddo	

	do k=1, ny
	do l= nz/2+1, nz
	  a11(l- nz/2,k)=a1(l,k)
	  b11(l- nz/2,k)=b1(l,k)
	enddo	
	enddo	
	endif				! gerade
	endif		! nflag=1

	if(nflag.eq.2)then
	if(jmod( nz,2).eq.0)then	! gerade
	do k=1, ny
	do l=1, nz/2
	  a11(l+( nz)/2,k)=a1(l,k)	
	  b11(l+( nz)/2,k)=b1(l,k)	
	enddo	
	enddo	

	do k=1, ny
	do l= nz/2+1, nz
	  a11(l- nz/2,k)=a1(l,k)
	  b11(l- nz/2,k)=b1(l,k)
	enddo	
	enddo	
	endif				! gerade
	endif		! nflag=2


	endif

c------------------------------
	if(iyz.eq.2)then
c------------ phadrift_remap y

	if(nflag.eq.1)then
	if(jmod( nz,2).eq.1)then	! ungerade
	do k=1,( ny+1)/2
	do l=1, nz
	  a1(l,k+( ny+1)/2-1)=a11(l,k)	
	  b1(l,k+( ny+1)/2-1)=b11(l,k)	
	enddo	
	enddo	

	do k=( ny+1)/2+1, ny
	do l=1, nz
	  a1(l,k-( ny+1)/2)=a11(l,k)
	  b1(l,k-( ny+1)/2)=b11(l,k)
	enddo	
	enddo	
	endif				! ungerade
	endif 	! nflag=1

	if(nflag.eq.2)then
	if(jmod( nz,2).eq.1)then	! ungerade
	do k=1,( ny+1)/2-1
	do l=1, nz
	  a1(l,k+( ny+1)/2)=a11(l,k)	
	  b1(l,k+( ny+1)/2)=b11(l,k)	
	enddo	
	enddo	

	do k=( ny+1)/2, ny
	do l=1, nz
	  a1(l,k-( ny+1)/2+1)=a11(l,k)
	  b1(l,k-( ny+1)/2+1)=b11(l,k)
	enddo	
	enddo	
	endif				! ungerade
	endif 	! nflag=2

	if(nflag.eq.1)then
	if(jmod( nz,2).eq.0)then	! gerade
	do k=1, ny/2
	do l=1, nz
	  a1(l,k+ ny/2)=a11(l,k)	
	  b1(l,k+ ny/2)=b11(l,k)	
	enddo	
	enddo	

	do k= ny/2+1, ny
	do l=1, nz
	  a1(l,k- ny/2)=a11(l,k)
	  b1(l,k- ny/2)=b11(l,k)
	enddo	
	enddo	
	endif				! gerade
	endif		! nflag=1

	if(nflag.eq.2)then
	if(jmod( nz,2).eq.0)then	! gerade
	do k=1, ny/2
	do l=1, nz
	  a1(l,k+ ny/2)=a11(l,k)	
	  b1(l,k+ ny/2)=b11(l,k)	
	enddo	
	enddo	

	do k= ny/2+1, ny
	do l=1, nz
	  a1(l,k- ny/2)=a11(l,k)
	  b1(l,k- ny/2)=b11(l,k)
	enddo	
	enddo	
	endif				! gerade
	endif		! nflag=2

	endif

	return
	end ! phadrift_remap
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc	




