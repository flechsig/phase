


	
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



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	subroutine phadrift_get_input(beam4)

     
      implicit none
      
            
      include 'myphase_struct.for'

      integer i,j
      real*8 zmin0,zmax0,ymin0,ymax0
	
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










