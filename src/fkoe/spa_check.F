c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/fkoe/spa_check.F
c$$$ Date      : <01 Apr 14 09:29:33 flechsig> 
c$$$ Time-stamp: <01 Apr 14 09:29:50 flechsig> 
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

      include 'spa-check.cmn'
      
      write(6,*)' create Gaussian beam           (1)'
      write(6,*)' create opticae element         (2)'
      write(6,*)' propagate via Frenel Kirchhoff (3)'
      write(6,*)' evaluate optical path length   (4)'
      
      read(5,*)iflag
      if(iflag.eq.1)call Gauss_beam
      if(iflag.eq.2)call mk_surf
      if(iflag.eq.3)call fkoe
      if(iflag.eq.4)call pathlength

      stop
      end
      
c----------------------------------------------------------------------
      subroutine Gauss_beam
c----------------------------------------------------------------------
c	create field distribution of Gauss'schen beam to compare with
c	commercial codes like ZEMAX or GLAD
c	ampplitudes normalized to one
c	GLAD manual p38, 39
c----------------------------------------------------------------------
      include 'spa-check.cmn'
      
	complex*16 ez(1024,1024),ey(1024,1024),xi

	pi=4.*atan(1.)

	small1=-72.
	small2=1.e-32
	small3=1.e-16

	open(unit=10,name='gauss_beam.par',type='old',readonly)
	read(10,*)xlam		! wavelength in nm
	  xlam=xlam*1.e-9	! units in m
	  xk=(2.*pi)/xlam	! wave vector
	read(10,*)w0		! size of waist
	  w0=w0/1000.		! units in m
	  xr=(pi*w0**2)/xlam	! rayleigh length

	type*,' rayleigh range / m = ',xr

	read(10,*)ianzy,ianzz	! number of data points
	read(10,*)ymin,ymax	! vertical dimension
	  ymin=ymin/1000.
	  ymax=ymax/1000.
	  dy=(ymax-ymin)/floatj(ianzy-1)
	read(10,*)zmin,zmax	! horizontal dimension
	  zmin=zmin/1000.
	  zmax=zmax/1000.
	  dz=(zmax-zmin)/floatj(ianzz-1)
	read(10,*)offy,offz
	  offy=offy/1000.
	  offz=offz/1000.
	read(10,*)deltax	! distance to waist
	  deltax=deltax/1000.
	  w=w0*sqrt(1.+(deltax/xr)**2)
	
	type*,' waist radius / m = ',w

	  theta=atan(deltax/xr)	! Gouy phase
	  if(abs(deltax).gt.small3)then
	    R=deltax+xr**2/deltax	! phase radius
	    else
	    R=0.d0
	  endif

	  type*,' phase radius / m = ',R

	close(10)

	do i=1,ianzz
	z(i)=zmin+floatj(i-1)*dz
	enddo	
	do j=1,ianzy
	y(j)=ymin+floatj(j-1)*dy
	enddo	

	xi=csqrt((-1.,0.))

	do i=1,ianzz
	do j=1,ianzy
	arg=-((z(i)-offz)**2+(y(j)-offy)**2)/w**2
	if(arg.gt.small1)then
	if(abs(R).gt.small3)then
	  fact=cdexp((xi*xk*((z(i)-offz)**2+(y(j)-offy)**2))/(2.*R))
	  else
	  fact=1.d0
	endif
	  ez(i,j)=cdexp(xi*(xk*deltax-theta))*fact*exp(arg)
	 else
	  ez(i,j)=0.
	endif
	ey(i,j)=0.	! location of waist

	enddo
	enddo

	pow=0.d0
	open(unit=10,name='ez2_gb.dat')
	write(10,*)ianzz,ianzy
	do j=1,ianzy
	do i=1,ianzz
	  dpow= dmax1(dabs(dreal(ez(i,j))),small3)**2+
     &          dmax1(dabs(dimag(ez(i,j))),small3)**2
	  write(10,*)z(i)*1000.,y(j)*1000.,2.d0*dpow
	  pow=pow+dpow
	enddo
	enddo
	close(10)
	pow=pow*(z(2)-z(1))*(y(2)-y(1))
	type*,' power = ',pow
	spow=dsqrt(pow)

	open(unit=10,name='eph_gb.dat')
	write(10,*)ianzz,ianzy
	do j=1,ianzy
	do i=1,ianzz
	if( (abs(imag(ez(i,j))).lt.small3).or.
     &      (abs(real(ez(i,j))).lt.small3) )then
	  wert=0.
	  else
	  wert=atan2(imag(ez(i,j)),real(ez(i,j)))
	endif
	  write(10,*)z(i)*1000.,y(j)*1000.,wert
	enddo
	enddo
	close(10)

	type*,pow

	pow=pow*(z(2)-z(1))*(y(2)-y(1))
	type*,' power = ',pow

	open(unit=10,name='eyre_gb.s4a')
	write(10,*)ianzz,ianzy
	do j=1,ianzy
	do i=1,ianzz
	  write(10,*)z(i)*1000.,y(j)*1000.,real(ez(i,j))/spow
	enddo
	enddo
	close(10)

	open(unit=10,name='eyim_gb.s4b')
	write(10,*)ianzz,ianzy
	do j=1,ianzy
	do i=1,ianzz
	  write(10,*)z(i)*1000.,y(j)*1000.,imag(ez(i,j))/spow
	enddo
	enddo
	close(10)

	open(unit=10,name='ezre_gb.s4c')
	write(10,*)ianzz,ianzy
	do j=1,ianzy
	do i=1,ianzz
	  write(10,*)z(i)*1000.,y(j)*1000.,real(ez(i,j))/spow
	enddo
	enddo
	close(10)

	open(unit=10,name='ezim_gb.s4d')
	write(10,*)ianzz,ianzy
	do j=1,ianzy
	do i=1,ianzz
	  write(10,*)z(i)*1000.,y(j)*1000.,imag(ez(i,j))/spow
	enddo
	enddo
	close(10)

	return
	end

c-------------------------------------------------------------
      subroutine mk_surf	
c-------------------------------------------------------------
c     create toroidal surface
c------------------------------
      include 'spa-check.cmn'
      
      dimension a(0:5,0:5)
	dimension z_1(1024),y_1(1024)
	
      pi=4.*datan(1.d0)

      open(unit=10,file='mk_surf.par')
      read(10,*)ianzy1
	read(10,*)ymin1
	read(10,*)ymax1
	read(10,*)ianzz1
	read(10,*)zmin1
      read(10,*)zmax1
      read(10,*)r1
      read(10,*)r2
      read(10,*)alpha
      read(10,*)itoroid
      close(10)
      alpha=alpha*(pi/180.d0)
      r=(2.*r1*r2)/((r1+r2)*dcos(alpha))
      rho=(2.*r1*r2*dcos(alpha))/(r1+r2)
      
      write(6,*)' r = ',r,' rho = ',rho

      do i=0,5
      do j=0,5
       if((i+j).le.5)a(i,j)=0.
      enddo
      enddo
          
      if(itoroid.eq.1)then
      a(0,2)=1.d0/(2.d0*rho)
      a(0,4)=1.d0/(8.d0*rho**3)
      a(2,0)=1.d0/(2.d0*r)
      a(2,2)=1.d0/(4.d0*r**2*rho)
      a(4,0)=1.d0/(8.d0*r**3)
      endif
      
      if(itoroid.eq.2)then
      a(2,0)=1.d0/(2.d0*r)
      a(4,0)=1.d0/(8.d0*r**3)
      endif
      
      write(6,*)'a(0,2) = ',a(0,2)
      write(6,*)'a(0,4) = ',a(0,4)
      write(6,*)'a(2,0) = ',a(2,0)
      write(6,*)'a(2,2) = ',a(2,2)  
      write(6,*)'a(4,0) = ',a(4,0)  

	dz_1=(zmax1-zmin1)/floatj(ianzz1-1)
	dy_1=(ymax1-ymin1)/floatj(ianzy1-1)
      
	do i=1,ianzz1
	z_1(i)=zmin1+floatj(i-1)*dz_1
	enddo

	do i=1,ianzy1
	y_1(i)=ymin1+floatj(i-1)*dy_1
	enddo

      write(6,*)'writing surf.dat'
	open(unit=10,file='surf.dat')
	write(10,*)ianzz1,ianzy1
	do i=1,ianzy1
	do j=1,ianzz1
		u=0.d0
	  do k=0,4
	  do l=0,4-k
         if(itoroid.eq.1)u=u+a(k,l)*dabs(y_1(i))**k*dabs(z_1(j))**l
         enddo
        enddo
        write(10,*)z_1(j),y_1(i),u  
      enddo
	enddo
	close(10)

	return
	end
	
c-----------------------------------------------------------------------
      subroutine fkoe
c-----------------------------------------------------------------------
      include 'spa-check.cmn'

      call phadrift_get_input_special
      call phadrift_mode
      call phadrift_write_output_special

      return
      end

c----------------------------------------------------------------
      subroutine phadrift_get_input_special
c----------------------------------------------------------------
      include 'spa-check.cmn'

	integer clen1,i,j,iname

	pi=4.d0*datan(1.d0)
	pihalf=pi/2.d0
	sqrtm1=dcmplx(0,1)

	open(unit=10,file='fkoe.par')
        read(10,3)dfile1	! eyre_0
        read(10,3)dfile2	! eyim_0
        read(10,3)dfile3	! ezre_0
        read(10,3)dfile4	! ezim_0

c----------- get source
	iname=clen1(dfile1)
	filnam1=''
        filnam1(:iname)=dfile1(:iname)

	iname=clen1(dfile2)
	filnam2=''
        filnam2(:iname)=dfile2(:iname)

	iname=clen1(dfile3)
	filnam3=''
        filnam3(:iname)=dfile3(:iname)

	iname=clen1(dfile4)
	filnam4=''
        filnam4(:iname)=dfile4(:iname)

	open(unit=11,file=filnam1)
	read(11,*)ianzz0,ianzy0
	  do i=1,ianzy0
	  do j=1,ianzz0
	    read(11,*)z0(j),y0(i),eyre0(j,i)
	  enddo
	  enddo
	close(11)

	open(unit=11,file=filnam2)
	read(11,*)ianzz0,ianzy0
	  do i=1,ianzy0
	  do j=1,ianzz0
	    read(11,*)z0(j),y0(i),eyim0(j,i)
	  enddo
	  enddo
	close(11)

	open(unit=11,file=filnam3)
	read(11,*)ianzz0,ianzy0
	  do i=1,ianzy0
	  do j=1,ianzz0
	    read(11,*)z0(j),y0(i),ezre0(j,i)
	  enddo
	  enddo
	close(11)

	open(unit=11,file=filnam4)
	read(11,*)ianzz0,ianzy0
	  do i=1,ianzy0
	  do j=1,ianzz0
	    read(11,*)z0(j),y0(i),ezim0(j,i)
	  enddo
	  enddo
	close(11)

c--------- write intensity in initial plane
      write(6,*)' writing data to eyz2_0.dat'
        open(unit=14,file='eyz2_0.dat')
	  write(14,*)ianzz0,ianzy0
	  do i=1,ianzy0
	  do j=1,ianzz0  
	  write(14,*)z0(j),y0(i),eyre0(j,i)**2+eyim0(j,i)**2+
     &                         ezre0(j,i)**2+ezim0(j,i)**2
	  enddo
	  enddo
      close(14) 

c--------- get complex fields of source
	do i=1,ianzy0
	do j=1,ianzz0
	  ey0(j,i)=eyre0(j,i)+sqrtm1*eyim0(j,i)
	enddo
	enddo

	do i=1,ianzy0
	do j=1,ianzz0
	  ez0(j,i)=ezre0(j,i)+sqrtm1*ezim0(j,i)
	enddo
	enddo

c-------- extract source grid
	zmin0=z0(1)
	zmax0=z0(ianzz0)
	ymin0=y0(1)
	ymax0=y0(ianzy0)
	dy0=(ymax0-ymin0)/dble(ianzy0-1)
	dz0=(zmax0-zmin0)/dble(ianzz0-1)

c--------------- get distances, angle, wavelength
	read(10,*)kmod
	if((kmod.ne.1).and.(kmod.ne.2).and.
     &     (kmod.ne.3).and.(kmod.ne.10))then
	  write(6,*)' kmod = ',kmod,' is not implemeted'
	  close(10)
	  stop
	endif

	read(10,*)dist,dista
      read(10,*)irrpmod
      read(10,*)winkel
	  winkel=winkel*(pi/180.0d0)
	read(10,*)xlam
	  xlam=xlam*1.0d-6
          cc=(2.d0*pi)/xlam

c--------------- extract image grid
	read(10,*)ymin1,ymax1
	read(10,*)ianzy1
	read(10,*)zmin1,zmax1
	read(10,*)ianzz1
	  dy1=(ymax1-ymin1)/dble(ianzy1-1)
	  dz1=(zmax1-zmin1)/dble(ianzz1-1)
	  do i=1,ianzy1
	    y1(i)=ymin1+dble(i-1)*dy1      
	  enddo
	  do j=1,ianzz1
            z1(j)=zmin1+dble(j-1)*dz1    
	  enddo

c--------------- get surface profile files
3       format(a80)
        read(10,3)dfile1

	close(10)

	iname=clen1(dfile1)
	filnam1=''
        filnam1(:iname)=dfile1(:iname)
	open(unit=10,file=filnam1)
	read(10,*)ianzz,ianzy
	do i=1,ianzy
	do j=1,ianzz
	  read(10,*)z(j),y(i),surf(j,i)
	enddo
	enddo
	close(10)

c-------- extract surface grid
	zmin=z(1)
	zmax=z(ianzz)
	ymin=y(1)
	ymax=y(ianzy)
	dy=(ymax-ymin)/dble(ianzy-1)
	dz=(zmax-zmin)/dble(ianzz-1)

c-----------------------------------------------------------
c-----------------------------------------------------------

c----- rearrange grids
	if(kmod.eq.1)then	! source to oe
	  ianzy1=ianzy
	  ianzz1=ianzz
	  ymin1=y(1)
	  ymax1=y(ianzy)
	  zmin1=z(1)
	  zmax1=z(ianzz)
	  dy1=dy
	  dz1=dz
	  do i=1,ianzy1
	    y1(i)=y(i)
	  enddo
	  do j=1,ianzz1
            z1(j)=z(j)
	  enddo
	endif

	if(kmod.eq.2)then	! oe to image
	  dist=dista
	  ianzy0=ianzy
	  ianzz0=ianzz
	  ymin0=y(1)
	  ymax0=y(ianzy)
	  zmin0=z(1)
	  zmax0=z(ianzz)
	  dy0=dy
	  dz0=dz
	  do i=1,ianzy0
	    y0(i)=y(i)
	  enddo
	  do j=1,ianzz0
            z0(j)=z(j)
	  enddo
	endif

	if(kmod.eq.3)then	! oe to image
c--------- step 1
	  ianzy0a=ianzy0
	  ianzz0a=ianzz0
	  ymin0a=y0(1)
	  ymax0a=y0(ianzy0)
	  zmin0a=z0(1)
	  zmax0a=z0(ianzz0)
	  dy0a=dy0
	  dz0a=dz0
	  do i=1,ianzy0a
	    y0a(i)=y0(i)
	  enddo
	  do j=1,ianzz0a
            z0a(j)=z0(j)
	  enddo

	  ianzy1a=ianzy
	  ianzz1a=ianzz
	  ymin1a=y(1)
	  ymax1a=y(ianzy)
	  zmin1a=z(1)
	  zmax1a=z(ianzz)
	  dy1a=dy
	  dz1a=dz
	  do i=1,ianzy1a
	    y1a(i)=y(i)
	  enddo
	  do j=1,ianzz1a
            z1a(j)=z(j)
	  enddo

c--------- step 2
	  ianzy0b=ianzy
	  ianzz0b=ianzz
	  ymin0b=y(1)
	  ymax0b=y(ianzy)
	  zmin0b=z(1)
	  zmax0b=z(ianzz)
	  dy0b=dy
	  dz0b=dz
	  do i=1,ianzy0b
	    y0b(i)=y(i)
	  enddo
	  do j=1,ianzz0b
            z0b(j)=z(j)
	  enddo

	  ianzy1b=ianzy1
	  ianzz1b=ianzz1
	  ymin1b=y1(1)
	  ymax1b=y1(ianzy1)
	  zmin1b=z1(1)
	  zmax1b=z1(ianzz1)
	  dy1b=dy1
	  dz1b=dz1
	  do i=1,ianzy1b
	    y1b(i)=y1(i)
	  enddo
	  do j=1,ianzz1b
            z1b(j)=z1(j)
	  enddo
	endif

	return
	end     !phadrift_get_input_special

c---------------------------------------------------
      subroutine phadrift_write_output_special
c---------------------------------------------------
      include 'spa-check.cmn'
     
      integer i,j
      open(unit=10,file='eyre_1.dat')
      open(unit=11,file='eyim_1.dat')
      open(unit=12,file='ezre_1.dat')
      open(unit=13,file='ezim_1.dat')
      open(unit=14,file='eyz2_1.dat')    
      write(6,*)' writing data to eyre_1.dat etc'
      write(6,*)' writing data to eyz2_1.dat'
      write(10,*)ianzz1,ianzy1	
      write(11,*)ianzz1,ianzy1	
      write(12,*)ianzz1,ianzy1	
      write(13,*)ianzz1,ianzy1	
      write(14,*)ianzz1,ianzy1	
 	do i=1,ianzy1
 	do j=1,ianzz1
 	  write(10,*)z1(j),y1(i),dreal(ey1(j,i))
 	  write(11,*)z1(j),y1(i),dimag(ey1(j,i))
 	  write(12,*)z1(j),y1(i),dreal(ez1(j,i))
 	  write(13,*)z1(j),y1(i),dimag(ez1(j,i))
  	  write(14,*)z1(j),y1(i),dreal(ez1(j,i))**2+dimag(ez1(j,i))**2+
     &                         dreal(ey1(j,i))**2+dimag(ey1(j,i))**2
 	enddo
 	enddo
      close(14)
      close(13)
      close(12)
      close(11)
      close(10)
 	
      return
      end 	! phadrift_write_output_special


c----------------------------------------------------------------
	subroutine phadrift_mode
c----------------------------------------------------------------
      include 'spa-check.cmn'
      
	integer i,j

      write(6,*)' kmod = ',kmod

	if((kmod.eq.1).or.(kmod.eq.2).or.(kmod.eq.10))then
	  call phadrift_propagate_fk_oe
	  goto 100
	endif

	if(kmod.eq.3)then
c------- step 1
	  ianzy0=ianzy0a
	  ianzz0=ianzz0a
	  ymin0=ymin0a
	  ymax0=ymax0a
	  zmin0=zmin0a
	  zmax0=zmax0a
	  dy0=dy0a
	  dz0=dz0a
	  do i=1,ianzy0
	    y0(i)=y0a(i)
	  enddo
	  do j=1,ianzz0
          z0(j)=z0a(j)
	  enddo

	  ianzy1=ianzy1a
	  ianzz1=ianzz1a
	  ymin1=ymin1a
	  ymax1=ymax1a
	  zmin1=zmin1a
	  zmax1=zmax1a
	  dy1=dy1a
	  dz1=dz1a
	  do i=1,ianzy1
	    y1(i)=y1a(i)
	  enddo
	  do j=1,ianzz1
          z1(j)=z1a(j)
	  enddo

	  kmod=1 
        write(6,*)' start step ',kmod 
	  call phadrift_propagate_fk_oe

c      open(unit=10,file='ezre_0a.dat')
c      write(10,*)ianzz1,ianzy1	
c      do i=1,ianzy1
c 	 do j=1,ianzz1
c 	  write(10,*)z1(j),y1(i),dreal(ez1(j,i))  
c 	 enddo
c      enddo      
c      close(10)
            
c------- step 2	  

	  ianzy0=ianzy0b
	  ianzz0=ianzz0b
	  ymin0=ymin0b
	  ymax0=ymax0b
	  zmin0=zmin0b
	  zmax0=zmax0b
	  dy0=dy0b
	  dz0=dz0b
	  do i=1,ianzy0
	    y0(i)=y0b(i)
	  enddo
	  do j=1,ianzz0
          z0(j)=z0b(j)
	  enddo

	  ianzy1=ianzy1b
	  ianzz1=ianzz1b
	  ymin1=ymin1b
	  ymax1=ymax1b
	  zmin1=zmin1b
	  zmax1=zmax1b
	  dy1=dy1b
	  dz1=dz1b
	  do i=1,ianzy1
	    y1(i)=y1b(i)
	  enddo
	  do j=1,ianzz1
            z1(j)=z1b(j)
	  enddo

 	do i=1,ianzy0
 	do j=1,ianzz0
	  ey0(j,i)=ey1(j,i)
	 	ez0(j,i)=ez1(j,i) 
 	enddo
 	enddo
      
	kmod=2
	dist=dista
      write(6,*)' start step ',kmod 	
	call phadrift_propagate_fk_oe

	endif	! kmod = 3

100	continue

	return
	end

c----------------------------------------------------------------------------
      subroutine phadrift_propagate_fk_oe
c----------------------------------------------------------------------------
c
c	kmod = 1 propagation from source to optical element
c
c	kmod = 2 propagation from optical element to image
c
c	kmod = 3 propagation from sourc to image via application of
c		 two steps: 1) from source to oe, 2) from oe to image
c
c	kmod = 10 no optical element
c
c--------------------------------------------------------------------------
      include 'spa-check.cmn'

	integer i,j,k,l
	
	if(kmod.eq.2)then
      do i1=1,ianzy0
 	do j1=1,ianzz0
c	  write(*,*) dreal(ez0(j1,i1))
 	enddo
 	enddo
      endif
      
	print*,'phaPropagateFresnelKirchhoff-oe started...' 

	fact0=(dz0*dy0)/xlam
        if((kmod.eq.1).or.(kmod.eq.2))then
	  fact0=fact0*dsqrt(dcos(winkel))
	endif
111   Format('iy = ',i6,' of ',i6)	
	
	if(dist.gt.0)then
        pmf = 1
      else
        pmf = -1
      endif

c----------------------------------------------------------
	if(kmod.eq.10)then
c--------------- propagation between plane parallel surfaces
      
        do i=1,ianzy1
          write(6,*)'i = ', i  
          do j=1,ianzz1
            ey1(j,i)=0.d0
            ez1(j,i)=0.d0
            do k=1,ianzy0
              do l=1,ianzz0

                dist1=dsqrt(dist**2+dabs(y0(k)-y1(i))**2+
     &          dabs(z0(l)-z1(j))**2)-dabs(dist)

                phase=dist1*cc*pmf
  
                fact=fact0*(dcos(phase)+sqrtm1*dsin(phase))/
     &           (dist+dist1) 
      
                ey1(j,i)=ey1(j,i)+ey0(l,k)*fact            
                ez1(j,i)=ez1(j,i)+ez0(l,k)*fact            
              enddo
            enddo
          enddo
        enddo

        endif

c-------------------------------------------------------------
	if(kmod.eq.1)then	! source to oe
c-------------------------------------------------------------

        do i=1,ianzy1
          write(6,*)'i = ', i, ' of ',ianzy1  
          do j=1,ianzz1
            ey1(j,i)=0.d0
            ez1(j,i)=0.d0
        
            xend=y1(i)
            yend=-surf(j,i)
            zend=z1(j)
            do k=1,ianzy0                
              do l=1,ianzz0
                xstart=-dist*dsin(winkel)-y0(k)*dcos(winkel)
                ystart=-dist*dcos(winkel)+y0(k)*dsin(winkel)
                zstart=z0(l)      

        dist1=dsqrt((xstart-xend)**2+(ystart-yend)**2+(zstart-zend)**2)
     &  -dabs(dist)
                phase=dist1*cc*pmf                
                if(irrpmod.eq.0)then
                  fact=fact0*(dcos(phase)+sqrtm1*dsin(phase))/
     &            dist 
                  else
                  fact=fact0*(dcos(phase)+sqrtm1*dsin(phase))/
     &            (dist+dist1)
                endif
                ey1(j,i)=ey1(j,i)+ey0(l,k)*fact            
                ez1(j,i)=ez1(j,i)+ez0(l,k)*fact            
              enddo
            enddo
          enddo
        enddo

	endif	! kmod.eq.1

c------------------------------------------------------------
	if(kmod.eq.2)then
c------------------------------------------------------------

        do i=1,ianzy1
          write(6,*)'i = ', i  
          do j=1,ianzz1
            ey1(j,i)=0.d0
            ez1(j,i)=0.d0
            do k=1,ianzy0
              do l=1,ianzz0

                xstart=y0(k)
                ystart=-surf(l,k)
                zstart=z0(l)

                xend=dist*dsin(winkel)+y1(i)*dcos(winkel)
                yend=-dist*dcos(winkel)+y1(i)*dsin(winkel)
                zend=z1(j)

       dist1=dsqrt((xstart-xend)**2+(ystart-yend)**2+(zstart-zend)**2)
     &   -dabs(dist)

                 phase=dist1*cc*pmf
  
                  if(irrpmod.eq.0)then
                   fact=fact0*(dcos(phase)+sqrtm1*dsin(phase))/
     &             dist 
                   else
                   fact=fact0*(dcos(phase)+sqrtm1*dsin(phase))/
     &             (dist+dist1)            
                  endif
                ey1(j,i)=ey1(j,i)+ey0(l,k)*fact            
                ez1(j,i)=ez1(j,i)+ez0(l,k)*fact            
              enddo
            enddo
          enddo
        enddo
	
      endif
      
      print*,'phaPropagateFresnelKirchhoff-oe finished...'      

      return
      end !phadrift_propagate_fk_oe 

c*************************************************************
      integer function clen1(str)
c*************************************************************
        character*(*) str
        do i=1,80
        clen1=i
        if( (ichar(str(i:i)).eq.32).or.
     &      (ichar(str(i:i)).eq.10).or.
     &      (ichar(str(i:i)).eq.9) )then
        clen1=clen1-1
        goto 9999
        endif
        enddo
9999    continue
        return
      end
             
c----------------------------------------------------------------------
      subroutine pathlength     
c----------------------------------------------------------------------
      implicit real*8(a-h,o-z)
      
	dimension w(10024),xl(124),opl(10024,124)
	dimension ww(0:5),xll(0:5),a(0:5,0:5)
      dimension opl_ref(10000),fourth(10000)

      open(unit=10,name='opl.par')
	read(10,*)xlam
	read(10,*)yi,zi
	read(10,*)yp,zp
	read(10,*)r1,r2
	read(10,*)alpha,beta
	read(10,*)xdefl
	read(10,*)wmin,wmax
	read(10,*)ianzw
	read(10,*)xlmin,xlmax
	read(10,*)ianzl
	close(10)
      
	pi = 4.d0*datan(1.d0)
	grarad=pi/180.d0
	alpha=alpha*grarad
	beta=beta*grarad      
      r=(2.*r1*r2)/((r1+r2)*dcos(alpha))
      rho=(2.*r1*r2*dcos(alpha))/(r1+r2)
      
      write(6,*)' r = ',r,' rho = ',rho
      
      do i=0,5
      do j=0,5
       if((i+j).le.5)a(i,j)=0.
      enddo
      enddo
          
      a(0,2)=1.d0/(2.d0*rho)
      a(0,4)=1.d0/(8.d0*rho**3)
      a(2,0)=1.d0/(2.d0*r)
      a(2,2)=1.d0/(4.d0*r**2*rho)
      a(4,0)=1.d0/(8.d0*r**3)
 
	cosa=dcos(alpha)
	cosb=dcos(beta)
	sina=dsin(alpha)
	sinb=dsin(beta)

	xk=(2.d0*pi)/xlam
      write(6,*)' xk ',xk
      
	dw=(wmax-wmin)/dflotj(ianzw-1)
	dl=(xlmax-xlmin)/dflotj(ianzl-1)

      dd=5000.
	xxi=(r1-dd)*cosa+xdefl*yi*sina
	yyi=(r1-dd)*sina-xdefl*yi*cosa
	xxp=r2*cosb-xdefl*yp*sinb
	yyp=r2*sinb+xdefl*yp*cosb

      write(6,*)xxi,yyi,xxp,yyp      
            
	do i=1,ianzw
	w(i)=wmin+dflotj(i-1)*dw

	do j=1,ianzl
	xl(j)=xlmin+dflotj(j-1)*dl

c	type*,w(i),xl(j)

	ww(0)=1.d0
	ww(1)=w(i)
	ww(2)=ww(1)*w(i)
	ww(3)=ww(2)*w(i)
	ww(4)=ww(3)*w(i)
	ww(5)=ww(4)*w(i)

	xll(0)=1.d0
	xll(1)=xl(j)
	xll(2)=xll(1)*xl(j)
	xll(3)=xll(2)*xl(j)
	xll(4)=xll(3)*xl(j)
	xll(5)=xll(4)*xl(j)

	u=0.d0
	do m=0,5
	do n=0,5
	 if((m+n).le.5)u=u+a(m,n)*ww(m)*xll(n)
	enddo
	enddo

c	type*,w(i),xl(j),u

	opl1=dsqrt(    (  dabs(xxi-u)      )**2  +
     &                 (  dabs(yyi-w(i))   )**2  +
     &                 (  dabs(zi-xl(j))   )**2   )
	opl2=dsqrt(    (  dabs(xxp-u)      )**2  +
     &                 (  dabs(yyp-w(i))   )**2  +
     &                 (  dabs(zp-xl(j))   )**2   )

      opl(i,j)=opl1+opl2
	
      enddo
      enddo
      
c     referenz von phase
      w0=17.2423213570012
      aa=9.007282324815280E-004
      bb=5.528361357035603E-002
      
      do i=1,ianzw
        opl_ref(i)=aa*(w(i)-w0)**3+bb*(w(i)-w0)**2     
      enddo
      
      write(6,*)' writing pathlength to opl.dat'
	open(unit=10,name='opl.dat',type='unknown')
	write(10,*)ianzw,ianzl
	do j=1,ianzl
	do i=1,ianzw
	 write(10,*)w(i),xl(j),xk*(opl(i,j)-r1-r2+dd)
	enddo
	enddo
	close(10)

	open(unit=10,name='opl.cos',type='unknown')
	write(10,*)ianzw,ianzl
	do j=1,ianzl
	do i=1,ianzw
	 write(10,*)w(i),xl(j),cos(xk*(opl(i,j)-r1-r2+dd))
	enddo
	enddo
	close(10)

	csum=0.
	ssum=0.
      csumr=0.
	ssumr=0.
	open(unit=10,name='opl_1d.cos',type='unknown')
	j0=(ianzl+1)/2
	do i=1,ianzw
	 write(10,*)w(i),cos(xk*(opl(i,j0)-r1-r2+dd)),
     &                 sin(xk*(opl(i,j0)-r1-r2+dd))
	csum=csum+cos(xk*(opl(i,j0)-r1-r2+dd))
	ssum=ssum+sin(xk*(opl(i,j0)-r1-r2+dd))
      csumr=csumr+cos(opl_ref(i))
	ssumr=ssumr+sin(opl_ref(i))
	enddo
	close(10)
      
	open(unit=10,name='opl_1d.dat',type='unknown')
	j0=(ianzl+1)/2
	do i=1,ianzw   
	 write(10,*)w(i),xk*(opl(i,j0)-r1-r2+dd),opl_ref(i)
	enddo
	close(10)
      
	open(unit=10,name='opl_1d_ref.cos',type='unknown')
	do i=1,ianzw   
	 write(10,*)w(i),cos(opl_ref(i)-r1-r2+dd),
     &                 sin(opl_ref(i)-r1-r2+dd)
	enddo
	close(10)

      csum=csum*dw
      ssum=ssum*dw
      csumr=csumr*dw
      ssumr=ssumr*dw
	type*,' cos-sum ',csum
	type*,' sin-sum ',ssum
	type*,' cos-sumr ',csumr
	type*,' sin-sumr ',ssumr
          
	return
	end

     