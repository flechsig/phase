c     test_phase.f
c
c     Compiled via 'phase4idl_fortran_master.f'
c     --> All Subroutines etc are in there ...
	
ccccc      Tests: z.B.: schreib, lese tests ...
      include 'diverse_testroutinen.f'
ccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
	program test_phase
	
	implicit none
	
	include 'myphase_struct.for'
      
	type(source4)::beam4
      
      integer ianzz,ianzy,nz2,ny2
      real*8    zmin,zmax,ymin,ymax,waist,d0,xlambda
     &	   ,dz1,dy1,dz2,dy2
	
	zmax=dble(1)
	zmin=-zmax
	ymax=zmax
	ymin=-ymax
	waist =dble(0.2)
	xlambda=20.d0
	d0=dble(0.)

	
	ianzz=101
	ianzy=int(ianzz)
      
	nz2=201
	ny2=51
	
c   Erstelle Gauss-Quelle in 'beam4' bei Abstd. 'd0'
	call pha_init_src4(beam4)
	call phasesrcwfgauss(beam4, ianzz,zmin,zmax, ianzy,ymin,ymax, 
     &					            waist, d0, xlambda)
	dz1=beam4%dxezre
	dy1=beam4%dyezre
     
     
c    subroutine pha_src4_modgrid(src4,nz2,ny2) 
	call pha_src4_modgrid(beam4,nz2,ny2)    
	
	dz2=beam4%dxezre
	dy2=beam4%dyezre

c ------------------- Output -----------------------------------------------c 
      write(*,*)'' 
	write(*,*)'******************** test_phase ************************'
	write(*,*)''
	write(*,*)'dz1 , dy1   vorher : ',dz1,dy1
	write(*,*)''
	write(*,*)''
	write(*,*)'dz2 , dy2  nachher : ',dz2,dy2
	write(*,*)''
	write(*,*)''
	write(*,*)'********************************************************'
      write(*,*)''
	
	call test_fortran_write(5.d0)
	
	stop
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
