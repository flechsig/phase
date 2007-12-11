c     test_phase.f
c
c     Compiled via 'phase4idl_fortran_master.f'
c     --> All Subroutines etc are in there ...

	
	subroutine write_line_dat(nz,ny,field)
	implicit none
	integer ii,nz,jj,ny
	real*8 field(256,256)
	      
      open(unit=11,name='fieldline.dat')!,type='new')
      do  ii=  1,nz
       do jj= 1,ny
c      write(*,*)'(i,j)',ii,jj,'    Ez^2(i,j):',drift_zint(ii,jj)
c      write(1,*),(drift_zint(ii,jj) , jj=15,16)!drift_zint(ii,jj+1)
       write(11,*),field(ii,jj)
       end do
      end do
      close(11)
      
	return
	
	end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	program test_phase

	implicit real*8(a-h,o-z)    
c	implicit integer*2(i-n)
	include 'include/myphase_struct.for'
      type(source4)::beam4
      
c      integer ianzz,ianzy,ii,jj,mid,imode
c      real*8    zmax,zmin,ymax,ymin,waist,distance,d0,xlambda

      imode=2   ! 1=FresnelKirchhoff;  2=FFTnear;  3=FFTfar
	
	zmax=dble(1)
	zmin=-zmax
	ymax=zmax
	ymin=-ymax
	
      waist =dble(0.2)
	xlambda=20.d0
	
	distance=dble(5000.)
	d0=dble(0.)
	
	call pha_init_src4(beam4)
cccccccccccccccccccccccccccccccccccccccccccccc benchmark ccccccccccccccccccccccccccccccccccccccccccccc
	
	do nexp=4,4
	
	ianzz=2**nexp
	ianzy=int(ianzz)
	
      jjmax=20
      t=0
      do jj=1,jjmax
	call phasesrcwfgauss(beam4, ianzz,zmin,zmax, ianzy,ymin,ymax, 
     &					            waist, d0, xlambda)
      call cpu_time(tstart)
      call phadrift_propagate_fk( beam4 ,distance 
     &				,ianzz ,zmin ,zmax ,ianzy ,ymin ,ymax)
      call cpu_time(tstop)
      t=t+tstop-tstart
	enddo !jj
      
	t=t/jjmax
	
	
      enddo !nexp
	
      write(*,*),t 
      write(*,*),t 
      write(*,*),t 
      write(*,*),t 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77      
c   Propagiere Gauss-Quelle 'beam4' nach Abstd. 'distance' mit FresnelKirch.
c      propagate_frekirch(beam4,nz1,zmin1,zmax1,ny1,ymin1,ymax1, distance)
c	if(imode.eq.1) call phadrift_propagate_fk( beam4 ,distance 
c     &				,ianzz ,zmin ,zmax ,ianzy ,ymin ,ymax)
c	if(imode.eq.2) call phadrift_propagate_fft_near( beam4 ,distancce )
c	if(imode.eq.3) call phadrift_propagate_fft_far(  beam4 ,distance )  
	
      stop 
      end program
	