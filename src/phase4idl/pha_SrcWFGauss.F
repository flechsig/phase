














c----------------------------------------------------------------------
c
c	create field distribution of Gauss'schen beam to compare with
c	commercial codes like ZEMAX or GLAD
c	ampplitudes normalized to one
c	GLAD manual p38, 39
c
c	Inputparameters:
c
c	Double:	w0		Waist-Size [mm] 
c		deltax		Distance to waist [mm]
c
c	source4:	gb
c
c	Derived from gb:
c
c	Integer: ianzy , ianzz  Number of datapoints in 
c				 y- and z- direction
c
c	Double:	ymin, ymax	y-coordinatesystem limits
c		zmin, zmax	z-coordinatesystem limits
c		xlam 		Wavelength [nm]
c
c
c	Outputparameters:	Fields in gb will be set up.
c
c----------------------------------------------------------------------

c	This is the routine that does the work. It in principle 
c   needs to know nothing about its call from IDL.
c
	
	
	


      SUBROUTINE phasesrcwfgauss(gb ,ianzz,zmin,zmax,ianzy,ymin,ymax,
     &							  w0, deltax, xlambda)
      implicit none    
	
	include 'myphase_struct.for'

	integer i,j,ianzz,ianzy
	real*8 z,dz,zmin,zmax,y,dy,ymin,ymax,w0,w,deltax,xlambda,xlam
     &	,xk,xr,small1,small2,small3,pi,pow,dpow,R,theta,arg
     &      ,rr2,z0,y0 
	dimension z(1000),y(1000)
	complex*16 ez(1000,1000),xi,fact  !,ey(1000,1000)
	type(source4)::gb
	
	write(*,*)'***'
	write(*,*)'phaSrcWFGauss started...'
	
      call pha_define_src4_grid(gb,ianzz,zmin,zmax,ianzy,ymin,ymax)
	
	z0=(zmax+zmin)/2.d0
	y0=(ymax+ymin)/2.d0
	
	pi=4.d0*datan(1.d0)

	small1=-72.
	small2=1.e-32
	small3=1.e-16 
	! Umspeichern -> lambda im rufenden Prog, nicht veraendert ...
	  xlam=dble(xlambda)
	  gb.xlam=dble(xlambda) 
	! wavelength in nm als Eingabeparameter
	  xlam=xlam*1.d-6	! units in mm (from nm)
	  xk=(2.*pi)/xlam	! wave vector
	! size of waist
	 ! w0=w0/1000.		! units in m
	  xr=(pi*w0**2)/xlam	! rayleigh length
c	  write(*,*)' rayleigh range / m = ',xr
	! vertical dimension
	! ymin=ymin/1000.
	! ymax=ymax/1000.
	  dy=(ymax-ymin)/dble(ianzy-1)
	! horizontal dimension
	!  zmin=zmin/1000.
	!  zmax=zmax/1000.
	  dz=(zmax-zmin)/dble(ianzz-1)
	! distance to waist
	!  deltax=deltax/1000.
	  w=w0*dsqrt(1.d0+(deltax/xr)**2)
	  write(*,*)' waist radius / m = ',w

	  theta=datan(deltax/xr)	! Gouy phase  --zurueck
	  if(dabs(deltax).gt.small3)then
	    R=deltax+xr**2/deltax	! phase radius
	    else
	    R=0.d0
	  endif
	  write(*,*)' phase radius / m = ',R

	! Rescaling of Inputparameters done .......
	  
	  do i=1,ianzz
	  z(i)=zmin +dble(i-1)*dz
	  enddo
	  do j=1,ianzy
	  y(j)=ymin +dble(j-1)*dy
	  enddo
	  
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc	
	xi=(0.0d0,1.0d0)
	pow=0.d0
	do i=1,ianzz
	do j=1,ianzy
	rr2=((z(i)-z0)**2+(y(j)-y0)**2)
	arg=-rr2/w**2
	if(arg.gt.small1)then
	if(dabs(R).gt.small3)then
	  fact=cdexp((xi*xk*rr2)/(2.d0*R))
	  else
	  fact=1.d0
	endif
	  ez(i,j)=cdexp(xi*(xk*deltax-theta))*fact*dexp(arg)
	 else
	  ez(i,j)=0.d0
	endif
	!ey(i,j)=0.  ! location of waist - isn't needed
	dpow= max(dabs(dreal(ez(i,j))),small3)**2+
     &          max(dabs(dimag(ez(i,j))),small3)**2
	pow=pow+dpow
c	write(*,*) pow
	enddo
	enddo
	pow=dsqrt(pow*dz*dy)
	
	ez(:,:)=ez(:,:)/pow
	
	do i=1,ianzz
	do j=1,ianzy  
	  gb.zezre(i,j)=dreal(ez(i,j))
	  gb.zezim(i,j)=dimag(ez(i,j))
	  gb.zeyre(i,j)=dble(0)  ! Initialize Ey(i,j)=0 
	  gb.zeyim(i,j)=dble(0)
	enddo
	enddo
	
	call pha_adjust_src4_grid(gb)
	
	write(*,*)'phaSrcWFGauss finished...'
	return
	end

     