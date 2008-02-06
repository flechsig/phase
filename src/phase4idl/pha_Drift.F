ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
c   Verwendete Module (globale Variablen, fuer Subroutines aus dieser Datei) 
c--------------------------------------------------------------------------77 
#ifdef ABSOFT
#else
      include 'pha_drift_modules.for'
#endif
c--------------------------------------------------------------------------77


	
	

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	subroutine phadrift_propagate_fft_far( beam4 ,dist ) 
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
#ifdef ABSOFT
        include 'pha_drift_include.f' 
#else
        use drift_common_mod
#endif
	
	implicit none !real*8(a-h,o-z)

	common/cstak/dstak

      include 'myphase_struct.for'

#ifdef ABSOFT
      record /source4/ beam4
#else      
      type(source4)::beam4 
#endif
	
	integer i,j,k,l,icomp
     &       ,ifx(25),ianzz00,ianzy00

      real*8  signum,const,a1_sav
     &       ,alpha,dalpha,beta,dbeta
     &       ,t(2048),dstak(524288)
     &       ,y_1_min,y_1_max,z_1_min,z_1_max
     &       ,dist,width,ycenter,zcenter
      
      write(*,*)'phaPropagateFFTfar started...'
	    
      distance=dble(dist)

	call phadrift_get_input(beam4)

c	a1/2 Real Fields ... b1/2 Complex Parts 
#ifdef ABSOFT
#else
	a1(:,:)=eyre(:,:)
        b1(:,:)=eyim(:,:)
        a2(:,:)=ezre(:,:)
        b2(:,:)=ezim(:,:)
#endif	

c------------ allocate storage
	call istkin(524288,4)	! (2*n*nns, itype=double)

	
c-----	get new grid points
c
c      calculations -> GLAD-Manual:  dx2 = (xlam*dist)/(N*dx1)
c        ->  Width of new Interval:   w2 = (N-1)*dx2 

c --- y-axis
	   ycenter = (ymax+ymin)/2.d0 
	     width = dble(ianzy0-1)*(xlam*distance/(dble(ianzy0)*dy))
	   y_1_min = ycenter - 0.5d0*width 
	   y_1_max = ycenter + 0.5d0*width
	      dy_1 = width / dflotj(ianzy0-1)
	
	do k=1,ianzy0
	    y_1(k) = y_1_min + dflotj(k-1)*dy_1
	enddo
	
c --- z-axis
	   zcenter = (zmax+zmin)/2.d0 
	     width = dble(ianzz0-1)*(xlam*distance/(dble(ianzz0)*dz))
	   z_1_min = zcenter - 0.5d0*width 
	   z_1_max = zcenter + 0.5d0*width
	      dz_1 = width / dflotj(ianzz0-1)
	
	do l=1,ianzz0
          z_1(l) = z_1_min + dflotj(l-1)*dz_1
	enddo
c
c -----------------  New Grid calculated ...
	
	
	do icomp=1,2	! loop over y and z-component of electric field
c	  start with ey component
c	  then ez component

	if(icomp.eq.2)then
	  do k=1,ianzy0
	  do l=1,ianzz0
	    a1(l,k)=a2(l,k)
	    b1(l,k)=b2(l,k)
	  enddo
	  enddo
	endif

c------------ step 1: apply quadratic factor

	do k=1,ianzy0
	do l=1,ianzz0
	if((jmod(ianzy0,2).eq.1).and.(jmod(ianzz0,2).eq.1))then
	  quad_fac=exp(((sqrtm1*2.d0*pi)/xlam)*
     &              (((z(l)-zcenter)**2 + 
     &                (y(k)-ycenter)**2)/(2.d0*distance)))
	endif
	if((jmod(ianzy0,2).eq.0).and.(jmod(ianzz0,2).eq.0))then
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

	call phadrift_remap(1,2)

	call phadrift_remap(2,2)
	
c------------ step 3: FFT
	if(distance.gt.0.d0)then
	  signum=-1.d0
  	  else
	  signum=1.d0
	endif

c---------- get prime factors of n
	call DMFTCI(ianzy0,ifx,t)


	ianzy00=1024	
	ianzz00=1024
	
	call DMFTCC(ianzy0,ianzz0,a1(1,1),b1(1,1),1,ianzy00,
     &		    a1(1,1),b1(1,1),1,ianzy00,ifx,t,signum)

	call DMFTCC(ianzy0,ianzz0,a1(1,1),b1(1,1),ianzz00,1,
     & 		          a1(1,1),b1(1,1),ianzz00,1,ifx,t,signum)


c-------------- step 4: phadrift_remapping of a1 and b1 in z
	call phadrift_remap(1,1)
c------------- remapping in y
	call phadrift_remap(2,1)


c------------ step 5: multiplication with prefactor

	do k=1,ianzy0
	do l=1,ianzz0
	if((jmod(ianzy0,2).eq.1).and.(jmod(ianzz0,2).eq.1))then
	  quad_fac_1=exp(((sqrtm1*2.d0*pi)/xlam)*
     &              (((z_1(l)-zcenter)**2 + 
     &                (y_1(k)-ycenter)**2)/(2.d0*distance)))
	endif

	if((jmod(ianzy0,2).eq.0).and.(jmod(ianzz0,2).eq.0))then
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
	  do k=1,ianzy0
	  do l=1,ianzz0
	    eyre_1(l,k)=a1(k,l) 
	    eyim_1(l,k)=b1(k,l) 
	  enddo
	  enddo
	endif

	if(icomp.eq.2)then

	  do k=1,ianzy0
	  do l=1,ianzz0
	    ezre_1(l,k)=a1(k,l)
	    ezim_1(l,k)=b1(k,l)
	  enddo
	  enddo

	endif

	enddo		! icomp, loop over components of electric field

#ifdef ABSOFT
#else	
	ey_1(:,:)=dcmplx(eyre_1(:,:),eyim_1(:,:))            
        ez_1(:,:)=dcmplx(ezre_1(:,:),ezim_1(:,:))
#endif
c --- New Gridparameters ...
	ymin=y_1_min      
      ymax=y_1_max
	zmin=z_1_min
	zmax=z_1_max
	
	call phadrift_write_output(beam4)
	write(*,*)'phaPropagateFFTfar finished...'
	return
	end   ! phadrift_propagate_fft_far (prop3)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77




ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	subroutine phadrift_propagate_fft_near( beam4 ,dist ) 
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
#ifdef ABSOFT
        include 'pha_drift_include.f' 
#else
        use drift_common_mod
#endif	
	
	implicit none !real*8(a-h,o-z)

	common/cstak/dstak(524288)

      include 'myphase_struct.for'
      
#ifdef ABSOFT
      record /source4/ beam4
#else      
      type(source4)::beam4 
#endif      
	
	integer i,j,k,l,icomp,n,nns 
     &       ,ifx(25),Nmax,ianzz00,ianzy00

      real*8  factre,factim,a10
     &       ,alpha,dalpha,beta,dbeta
     &       ,t(2048),dstak,signum
     &       ,dist
           
c	complex*16  abcmplx(1024,1024),factcmplx(1024,1024) 

c------------ allocate storage
	call istkin(524288,4)	! (2*n*nns, itype=double)
	
	write(*,*)'phaPropagateFFTnear started...'

	
	distance=dble(dist)

	call phadrift_get_input(beam4)
	
	dy_1=dy
	dz_1=dz
	Nmax=1024

c	a1/2 Real Fields ... b1/2 Complex Parts 
#ifdef ABSOFT
#else
	a1(:,:)=eyre(:,:)
      b1(:,:)=eyim(:,:)
      a2(:,:)=ezre(:,:)
      b2(:,:)=ezim(:,:)

c-- actualize coordinates
	y_1(:)=y(:)
      z_1(:)=z(:)
	
#endif
	do icomp=1,2	! loop over y and z-component of electric field
c	  start with ey component
c	  then ez component

	if(icomp.eq.2)then
	  do k=1,ianzy0
	  do l=1,ianzz0
	    a1(l,k)=a2(l,k)
	    b1(l,k)=b2(l,k)
	  enddo
	  enddo
	endif

c	bis hier ok, beim 2. Durchgang werden a1 b1 wieder richtig 
c	initialisiert

c------------ step 1: FFT
	signum=-1.d0

	n=ianzy0
	nns=ianzz0
c---------- get prime factors of n
	call DMFTCI(n,ifx,t)


	ianzy00=1024
	ianzz00=1024

	
	call DMFTCC(ianzy0,ianzz0,a1(1,1),b1(1,1),1,ianzy00,
     &		    a1(1,1),b1(1,1),1,ianzy00,ifx,t,signum)

	call DMFTCC(ianzy0,ianzz0,a1(1,1),b1(1,1),ianzz00,1,
     & 		          a1(1,1),b1(1,1),ianzz00,1,ifx,t,signum)


c---- jetzt haben wir die 2D-Wikelverteilung, die noch ausgewertet werden 
c---- muss

c------------ step 2: phase modification

	dalpha=xlam/(dy*dflotj(ianzy0))
	dbeta=xlam/(dz*dflotj(ianzz0))

	do i=1,ianzy0
	do j=1,ianzz0
	  if(i.le.ianzy0/2)then
	    alpha=dflotj(i-1)*dalpha
	    else
	    alpha=-dflotj(ianzy0-i+1)*dalpha
	  endif
	  
	  if(j.le.ianzz0/2)then
	    beta=dflotj(j-1)*dbeta
	    else
	    beta=-dflotj(ianzz0-j+1)*dbeta
	  endif
  
	  factre=dcos(((2.d0*pi)/xlam)*
     &		distance*dsqrt(1.d0-alpha**2-beta**2))
	  factim=dsin(((2.d0*pi)/xlam)*
     &		distance*dsqrt(1.d0-alpha**2-beta**2))
	  
	  a10=a1(i,j)
	  a1(i,j)=a1(i,j)*factre-b1(i,j)*factim
	  b1(i,j)=a10*factim+b1(i,j)*factre

	enddo
	enddo


c------------ step 3: FFTI
	
	call DMFTCC(ianzy0,ianzz0,a1(1,1),b1(1,1),1,ianzy00,
     &		          a1(1,1),b1(1,1),1,ianzy00,ifx,t,-signum)
	call DMFTCC(ianzy0,ianzz0,a1(1,1),b1(1,1),ianzz00,1,
     &		          a1(1,1),b1(1,1),ianzz00,1,ifx,t,-signum)

	do i=1,ianzy0
	do j=1,ianzy0
	  a1(i,j)=a1(i,j)/dflotj(ianzy0*ianzz0)
	  b1(i,j)=b1(i,j)/dflotj(ianzy0*ianzz0)
	enddo
	enddo


	if(icomp.eq.1)then
	  do k=1,ianzy0
	  do l=1,ianzz0
	    eyre_1(l,k)=a1(k,l)
	    eyim_1(l,k)=b1(k,l)
	  enddo
	  enddo
	endif

	if(icomp.eq.2)then

	  do k=1,ianzy0
	  do l=1,ianzz0
	    ezre_1(l,k)=a1(k,l)
	    ezim_1(l,k)=b1(k,l)
	  enddo
	  enddo

	endif

	enddo		! icomp, loop over components of electric field

#ifdef ABSOFT
#else
	ey_1(:,:)=dcmplx(eyre_1(:,:),eyim_1(:,:))         
      ez_1(:,:)=dcmplx(ezre_1(:,:),ezim_1(:,:))      
#endif
	
	call phadrift_write_output(beam4)
	
	write(*,*)'phaPropagateFFTnear finished...'
	return
	end   !phadrift_propagate_fft_near (prop2)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	
	
	
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
c----------------------------------------------------------------------------
      subroutine phadrift_propagate_fk( beam4 ,dist 
     &				,nz2 ,zmin2 ,zmax2 ,ny2 ,ymin2 ,ymax2)

#ifdef ABSOFT
        include 'pha_drift_include.f' 
#else
        use drift_common_mod
#endif     

      implicit none
      
      include 'myphase_struct.for'
      
#ifdef ABSOFT
      record /source4/ beam4
#else      
      type(source4)::beam4 
#endif     
      
      integer i,j,k,l,nz2,ny2
      real*8 zmin2,zmax2,ymin2,ymax2
	real*8 phase, dist,dist1
c      real*8 x1,x2,x3,x4,x5     

	write(*,*)'phaPropagateFresnellKirchhoff started...' 
     
      distance=dble(dist)

	call phadrift_get_input(beam4)
      
      zmin    = dble(zmin2)
      zmax    = dble(zmax2)
      ymin    = dble(ymin2)
      ymax    = dble(ymax2)
      ianzz   = int(nz2)
      ianzy   = int(ny2)
c   ymax, zmax, ... sind neue Grenzen ......
	dy_1=(ymax-ymin)/dble(ianzy-1)
	dz_1=(zmax-zmin)/dble(ianzz-1)
      
ccc   START PROPAGATE FK    ccc
	do i=1,ianzy
        y_1(i)=ymin+dble(i-1)*dy_1      

        do j=1,ianzz
          z_1(j)=zmin+dble(j-1)*dz_1    
            ey_1(j,i)=0.d0
            ez_1(j,i)=0.d0
            do k=1,ianzy0
            do l=1,ianzz0

c Entwicklung der Wurzel ist langsamer als dsqrt !!!

c            x1=((y(k)-y_1(i))**2+
c     &                  (z(l)-z_1(j))**2)/distance**2
c            x2=x1*x1
c            x3=x2*x1
c            x4=x3*x1
c            x5=x4*x1
c
c      dist1=(x1       ) *( 1.d0 /(2.d0*(x1+1.d0))) +
c     &           (X2/  2.d0) *(-1.d0 /(4.d0*(x2+2.d0*x1+1.d0))) +
c     &       (x3/  6.d0) *( 3.d0 /(8.d0*(x3+3.d0*x2+3.d0*x1+1.d0))) +
c     &           (x4/ 24.d0) *(-15.d0/(16.d0*
c     &            (x4+4.d0*x3+6.d0*x2+4.d0*x1+1.d0))) +
c     &           (x5/120.d0) *(105.d0/(32.d0*
c     &            (x5+5.d0*x4+10.d0*x3+10.d0*x2+5.d0*x1+1.d0)))    
c      dist1=dist1*distance


c    Intrinsische Wurzelfkt. dsqrt ist schneller als Entwicklung ...
c    dsqrt benoetigt nur rund 66% der Zeit, die die Entwicklung braucht !      

      dist1=dsqrt(distance**2+dabs(y(k)-y_1(i))**2+
     &          dabs(z(l)-z_1(j))**2)-dabs(distance)

      phase=dist1*cc
      fact=(dcos(phase)+sqrtm1*dsin(phase))/
     &            (dist1+distance)
      fact=(fact*dz*dy)/xlam

c      if((i.eq.(ianzy+1)/2).and.(j.eq.(ianzz+1)/2).and.
c     &        (k.eq.(ianzy0+1)/2))then 
c        xarg(l)=dreal(ez(l,k)*fact)
c      endif

      ey_1(j,i)=ey_1(j,i)+ey(l,k)*fact            
      ez_1(j,i)=ez_1(j,i)+ez(l,k)*fact            
      
              enddo
            enddo
        enddo
      enddo
ccc   END PROPAGATE FK   ccc
      
	call phadrift_write_output(beam4)
	
      write(*,*)'phaPropagateFresnellKirchhoff finished...'      
      end !phadrift_propagate_fk  (prop1)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77





ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	subroutine phadrift_get_input(beam4)

#ifdef ABSOFT
        include 'pha_drift_include.f' 
#else
        use drift_common_mod
#endif

      
      implicit none
      
            
      include 'myphase_struct.for'

      integer i,j
      real*8 zmin0,zmax0,ymin0,ymax0
#ifdef ABSOFT
      record /source4/ beam4
#else      
      type(source4)::beam4 
#endif	
	
	pi=4.d0*datan(1.d0)
	pihalf=pi/2.d0
	sqrtm1=dcmplx(0,1)
	 
	xlam=dble(beam4.xlam)*1.0d-6
	  cc=(2.d0*pi)/xlam
c----------- change form divergencies (mrad) to distances (mm)
c	ymin=(ymin/1000.)*abs(distance)
c	ymax=(ymax/1000.)*abs(distance)
c	zmin=(zmin/1000.)*abs(distance)
c	zmax=(zmax/1000.)*abs(distance)
	call pha_extract_src4_grid(beam4
     &				,ianzz0,zmin0,zmax0,ianzy0,ymin0,ymax0)
	
	dy=(ymax0-ymin0)/dble(ianzy0-1)
	dz=(zmax0-zmin0)/dble(ianzz0-1)
	
	ianzz=ianzz0
	ianzy=ianzy0
	
	zmin=zmin0
	zmax=zmax0
	ymin=ymin0
	ymax=ymax0
	
	do i=1,ianzz0
	   z(i)=zmin0+dble(i-1)*dz
	enddo
	do i=1,ianzy0
	   y(i)=ymin0+dble(i-1)*dy
      enddo   
	
c  Auslesen der Felder ...
	do i=1,ianzy0
	do j=1,ianzz0
          eyre(j,i)=beam4.zeyre(j,i)
          eyim(j,i)=beam4.zeyim(j,i)
	    ezre(j,i)=beam4.zezre(j,i)
	    ezim(j,i)=beam4.zezim(j,i)
	enddo
	enddo

c--------- get complex fields
	do i=1,ianzy0
	do j=1,ianzz0
	  ey(j,i)=eyre(j,i)+sqrtm1*eyim(j,i)
	enddo
	enddo

	do i=1,ianzy0
	do j=1,ianzz0
	  ez(j,i)=ezre(j,i)+sqrtm1*ezim(j,i)
	enddo
	enddo
	
	return
	end     !phadrift_get_input
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
      subroutine phadrift_write_output(beam4)
c---------------------------------------------------
#ifdef ABSOFT
        include 'pha_drift_include.f' 
#else
        use drift_common_mod
#endif

      
      implicit none
	include 'myphase_struct.for'
#ifdef ABSOFT
      record /source4/ beam4
#else      
      type(source4)::beam4 
#endif	
      integer i,j
	
	do j=1,ianzy
	do i=1,ianzz
	  beam4.zezre(j,i)=dreal(ez_1(i,j))
	  beam4.zezim(j,i)=dimag(ez_1(i,j))
	  beam4.zeyre(j,i)=dreal(ey_1(i,j))  
	  beam4.zeyim(j,i)=dimag(ey_1(i,j))
	enddo
	enddo
	
      call pha_define_src4_grid(beam4,ianzz,zmin,zmax,ianzy,ymin,ymax)
	
	call pha_adjust_src4_grid(beam4)
	
	return
	end   ! phadrift_write_output
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77

c-----------------------------------------
	subroutine phadrift_remap(iyz,nflag)
c-----------------------------------------
#ifdef ABSOFT
        include 'pha_drift_include.f' 
#else
        use drift_common_mod
#endif


	implicit real*8(a-h,o-z)

c------------------------------
	if(iyz.eq.1)then
c------------ phadrift_remap z

	if(nflag.eq.1)then
	if(jmod(ianzz0,2).eq.1)then	! ungerade
	do k=1,ianzy0
	do l=1,(ianzz0+1)/2
	  a11(l+(ianzz0+1)/2-1,k)=a1(l,k)	
	  b11(l+(ianzz0+1)/2-1,k)=b1(l,k)	
	enddo	
	enddo	

	do k=1,ianzy0
	do l=(ianzz0+1)/2+1,ianzz0
	  a11(l-(ianzz0+1)/2,k)=a1(l,k)
	  b11(l-(ianzz0+1)/2,k)=b1(l,k)
	enddo	
	enddo	
	endif				! ungerade
	endif		!nflag=1

	if(nflag.eq.2)then
	if(jmod(ianzz0,2).eq.1)then	! ungerade
	do k=1,ianzy0
	do l=1,(ianzz0+1)/2-1
	  a11(l+(ianzz0+1)/2,k)=a1(l,k)	
	  b11(l+(ianzz0+1)/2,k)=b1(l,k)	
	enddo	
	enddo	

	do k=1,ianzy0
	do l=(ianzz0+1)/2,ianzz0
	  a11(l-(ianzz0+1)/2+1,k)=a1(l,k)
	  b11(l-(ianzz0+1)/2+1,k)=b1(l,k)
	enddo	
	enddo	
	endif				! ungerade
	endif		!nflag=2


	if(nflag.eq.1)then
	if(jmod(ianzz0,2).eq.0)then	! gerade
	do k=1,ianzy0
	do l=1,ianzz0/2
	  a11(l+(ianzz0)/2,k)=a1(l,k)	
	  b11(l+(ianzz0)/2,k)=b1(l,k)	
	enddo	
	enddo	

	do k=1,ianzy0
	do l=ianzz0/2+1,ianzz0
	  a11(l-ianzz0/2,k)=a1(l,k)
	  b11(l-ianzz0/2,k)=b1(l,k)
	enddo	
	enddo	
	endif				! gerade
	endif		! nflag=1

	if(nflag.eq.2)then
	if(jmod(ianzz0,2).eq.0)then	! gerade
	do k=1,ianzy0
	do l=1,ianzz0/2
	  a11(l+(ianzz0)/2,k)=a1(l,k)	
	  b11(l+(ianzz0)/2,k)=b1(l,k)	
	enddo	
	enddo	

	do k=1,ianzy0
	do l=ianzz0/2+1,ianzz0
	  a11(l-ianzz0/2,k)=a1(l,k)
	  b11(l-ianzz0/2,k)=b1(l,k)
	enddo	
	enddo	
	endif				! gerade
	endif		! nflag=2


	endif

c------------------------------
	if(iyz.eq.2)then
c------------ phadrift_remap y

	if(nflag.eq.1)then
	if(jmod(ianzz0,2).eq.1)then	! ungerade
	do k=1,(ianzy0+1)/2
	do l=1,ianzz0
	  a1(l,k+(ianzy0+1)/2-1)=a11(l,k)	
	  b1(l,k+(ianzy0+1)/2-1)=b11(l,k)	
	enddo	
	enddo	

	do k=(ianzy0+1)/2+1,ianzy0
	do l=1,ianzz0
	  a1(l,k-(ianzy0+1)/2)=a11(l,k)
	  b1(l,k-(ianzy0+1)/2)=b11(l,k)
	enddo	
	enddo	
	endif				! ungerade
	endif 	! nflag=1

	if(nflag.eq.2)then
	if(jmod(ianzz0,2).eq.1)then	! ungerade
	do k=1,(ianzy0+1)/2-1
	do l=1,ianzz0
	  a1(l,k+(ianzy0+1)/2)=a11(l,k)	
	  b1(l,k+(ianzy0+1)/2)=b11(l,k)	
	enddo	
	enddo	

	do k=(ianzy0+1)/2,ianzy0
	do l=1,ianzz0
	  a1(l,k-(ianzy0+1)/2+1)=a11(l,k)
	  b1(l,k-(ianzy0+1)/2+1)=b11(l,k)
	enddo	
	enddo	
	endif				! ungerade
	endif 	! nflag=2

	if(nflag.eq.1)then
	if(jmod(ianzz0,2).eq.0)then	! gerade
	do k=1,ianzy0/2
	do l=1,ianzz0
	  a1(l,k+ianzy0/2)=a11(l,k)	
	  b1(l,k+ianzy0/2)=b11(l,k)	
	enddo	
	enddo	

	do k=ianzy0/2+1,ianzy0
	do l=1,ianzz0
	  a1(l,k-ianzy0/2)=a11(l,k)
	  b1(l,k-ianzy0/2)=b11(l,k)
	enddo	
	enddo	
	endif				! gerade
	endif		! nflag=1

	if(nflag.eq.2)then
	if(jmod(ianzz0,2).eq.0)then	! gerade
	do k=1,ianzy0/2
	do l=1,ianzz0
	  a1(l,k+ianzy0/2)=a11(l,k)	
	  b1(l,k+ianzy0/2)=b11(l,k)	
	enddo	
	enddo	

	do k=ianzy0/2+1,ianzy0
	do l=1,ianzz0
	  a1(l,k-ianzy0/2)=a11(l,k)
	  b1(l,k-ianzy0/2)=b11(l,k)
	enddo	
	enddo	
	endif				! gerade
	endif		! nflag=2

	endif

	return
	end ! phadrift_remap
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc	
