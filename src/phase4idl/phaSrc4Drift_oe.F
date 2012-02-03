#include "pha4idlFORTRANdefines.h.f"


c----------------------------------------------------------------
      subroutine phadrift_get_input_special
c----------------------------------------------------------------

        include 'fkoe.cmn'

	integer clen1,i,j,iname

c	pi=4.d0*datan(1.d0)
c	pihalf=pi/2.d0
	sqrtm1=dcmplx(0,1)

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

c--------- interpolate positions
        do i=1,ianzz0
          z0(i) = z0(1)+dz0*dble(i-1);
       	enddo

        do i=1,ianzy0
          y0(i) = y0(1)+dy0*dble(i-1);
       	enddo

c--------------- extract image grid

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
c        read(10,3)dfile1

c	close(10)

	iname=clen1(filenamesurf)
	filnam1=''
        filnam1(:iname)=filenamsurf(:iname)
	open(unit=10,name=filnam1,type='old',readonly)
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

c----------------------------------------------------------------
	subroutine phadrift_mode
c----------------------------------------------------------------

      include 'fkoe.cmn'

	integer i,j

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
	  call phadrift_propagate_fk_oe

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
	  ey0(i,j)=ey1(i,j)
	  ez0(i,j)=ez1(i,j)
 	enddo
 	enddo

	kmod=2
	dist=dista
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

	include 'fkoe.cmn'

	print*,'phaPropagateFresnelKirchhoff-oe mp started...' 

	fact0=(dz0*dy0)/xlam
        if((kmod.eq.1).or.(kmod.eq.2))then
	  fact0=fact0*dsqrt(dcos(winkel))
	endif

c----------------------------------------------------------
	if(kmod.eq.10)then 	!propagation between plane parallel surfaces
	  call phadrift_propagate_fk_oe_10
	endif

	if(kmod.eq.1)then	! source to oe
	  call phadrift_propagate_fk_oe_1
	endif	! kmod.eq.1

	if(kmod.eq.2)then
	  call phadrift_propagate_fk_oe_2
	endif

      print*,'phaPropagateFresnelKirchhoff-oe finished...'      

      return
      end !phadrift_propagate_fk_oe 


c----------------------------------------------------------------------------
      subroutine phadrift_propagate_fk_oe_10
c----------------------------------------------------------------------------
c	kmod = 10 no optical element
c--------------------------------------------------------------------------

	include 'fkoe.cmn'

	integer i,j,k,l

	fact0=(dz0*dy0)/xlam
        if((kmod.eq.1).or.(kmod.eq.2))then
	  fact0=fact0*dsqrt(dcos(winkel))
	endif
!$OMP PARALLEL DO
	do i=1,ianzy1
c	  PRINT *, 'i = ', i
          do j=1,ianzz1
            
	    ey1(j,i)=0.d0
            ez1(j,i)=0.d0
            do k=1,ianzy0
              do l=1,ianzz0
      
c---- propagation between plane parallel surfaces
      dist1=dsqrt(dist**2+dabs(y0(k)-y1(i))**2+
     &          dabs(z0(l)-z1(j))**2)-dabs(dist)
c-------------------------------------------------------------

      if(dist.gt.0)then
        phase=dist1*cc
	else
        phase=-dist1*cc
      endif

      fact=fact0*(dcos(phase)+sqrtm1*dsin(phase))/
     &   (dist+dist1)
      ey1(j,i)=ey1(j,i)+ey0(l,k)*fact            
      ez1(j,i)=ez1(j,i)+ez0(l,k)*fact            

              enddo
            enddo
        enddo
      enddo

      return
      end !phadrift_propagate_fk_oe_10

c----------------------------------------------------------------------------
      subroutine phadrift_propagate_fk_oe_1
c----------------------------------------------------------------------------
c	kmod = 1 propagation from source to optical element
c--------------------------------------------------------------------------

	include 'fkoe.cmn'

	integer i,j,k,l
	
	fact0=(dz0*dy0)/xlam
        if((kmod.eq.1).or.(kmod.eq.2))then
	  fact0=fact0*dsqrt(dcos(winkel))
	endif
!$OMP PARALLEL DO
	do i=1,ianzy1
c	  PRINT *, 'i = ', i
          do j=1,ianzz1
            
	    ey1(j,i)=0.d0
            ez1(j,i)=0.d0
            do k=1,ianzy0
              do l=1,ianzz0

c----! source to oe
	xstart=-dist*dsin(winkel)-y0(k)*dcos(winkel)
	ystart=-dist*dcos(winkel)+y0(k)*dsin(winkel)
	zstart=z0(l)

	xend=y1(i)
	yend=-surf(j,i)
	zend=z1(j)

	dist1=dsqrt((xstart-xend)**2+(ystart-yend)**2+(zstart-zend)**2)
     &      -dabs(dist)
c------------------------------------------------------------

      if(dist.gt.0)then
        phase=dist1*cc
      else
        phase=-dist1*cc
      endif

      fact=fact0*(dcos(phase)+sqrtm1*dsin(phase))/
     &   (dist+dist1)
      ey1(j,i)=ey1(j,i)+ey0(l,k)*fact            
      ez1(j,i)=ez1(j,i)+ez0(l,k)*fact            

              enddo
            enddo
        enddo
      enddo

      return
      end !phadrift_propagate_fk_oe_1

c----------------------------------------------------------------------------
      subroutine phadrift_propagate_fk_oe_2
c----------------------------------------------------------------------------
c	kmod = 2 propagation from optical element to image
c--------------------------------------------------------------------------

	include 'fkoe.cmn'

	integer i,j,k,l

	fact0=(dz0*dy0)/xlam
        if((kmod.eq.1).or.(kmod.eq.2))then
	  fact0=fact0*dsqrt(dcos(winkel))
	endif
!$OMP PARALLEL DO
	do i=1,ianzy1
c	  PRINT *, 'i = ', i
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
     &      -dabs(dist)
c-------------------------------------------------------------

      if(dist.gt.0)then
        phase=dist1*cc
      else
        phase=-dist1*cc
      endif

      fact=fact0*(dcos(phase)+sqrtm1*dsin(phase))/
     &   (dist+dist1)
      ey1(j,i)=ey1(j,i)+ey0(l,k)*fact            
      ez1(j,i)=ez1(j,i)+ez0(l,k)*fact            

              enddo
            enddo
        enddo
      enddo

      return
      end !phadrift_propagate_fk_oe_2


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


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
      subroutine phadrift_propagate_fk_oe_nostructs(
     &						 INezre,INezim,INeyre,INeyim
     &						,INnz,INzmin,INzmax
     &						,INny,INymin,INymax
     &						,INdist, INdista, INxlam
     &            ,INangle, INmode
     &            ,INsurffilename, INstringlength
     &						,INnz2,INzmin2,INzmax2
     &						,INny2,INymin2,INymax2 )
c****************************************************************************
      include 'fkoe.cmn'

c      implicit none

      real*8 INeyre(MaxDim,MaxDim), INeyim(MaxDim,MaxDim),
     &       INezre(MaxDim,MaxDim), INezim(MaxDim,MaxDim)
      real*8 INzmin, INzmax, INymin, INymax, INdist, INdista
      real*8 INzmin2, INzmax2, INymin2, INymax2
      real*8 INxlam
      real*8 INangle
      
      integer i, j, INnz, INny, INnz2, INny2, INmode, INstringlength
      CHARACTER*(*) INsurffilename      
            

c     some variables have to be set before calling subfunction
      kmod = INmode
      filenamsurf = ''
      filenamsurf(1:INstringlength) = INsurffilename(1:INstringlength)      

      ianzz0 = INnz
      ianzy0 = INny
      z0(1) = dble(INzmin)
      y0(1) = dble(INymin)
      z0(ianzz0) = dble(INzmax)
      y0(ianzy0) = dble(INymax)


      ymin1 = INymin2
      ymax1 = INymax2
	    ianzy1 = INny2
      zmin1 = INzmin2
      zmax1 = INzmax2
	    ianzz1 = INnz2

      ezre0 = INezre
      ezim0 = INezim
      eyre0 = INeyre
      eyim0 = INeyim

      call phadrift_get_input_special

c     jetzt Parameter ueberschreiben
      dist = INdist
      dista = INdista
      xlam = INxlam
      winkel = INangle
    
      pi=4.d0*datan(1.d0)
      winkel=winkel*(pi/180.0d0)
     
      xlam=xlam*1.0d-6
      cc=(2.d0*pi)/xlam

      call phadrift_mode

      INzmin = zmin1
      INzmax = zmax1
      INymin = ymin1
      INymax = ymax1
        
      INnz = ianzz1
      INny = ianzy1

    	do i=1,INnz
     	do j=1,INny
        INeyre(j, i) = dreal(ey1(j,i))         
        INeyim(j, i) = dimag(ey1(j,i))        
        INezre(j, i) = dreal(ez1(j,i))         
        INezim(j, i) = dimag(ez1(j,i))       
      enddo
 	    enddo

      end !phadrift_propagate_oe_nostructs

             
