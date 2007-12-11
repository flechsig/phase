cccccccccccccccccccccccccccccccccccccccccccccccccccccc	
c	Fortran-Structures for Phase
cccccccccccccccccccccccccccccccccccccccccccccccccccccc	
  
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	type  source4 
	structure/source4/
c      
	character*80  fsource4a, fsource4b, 
     &              fsource4c, fsource4d
     
	real*8  xeyremin,xeyremax,dxeyre,   
     &	  xeyimmin,xeyimmax,dxeyim,
     &	  yeyremin,yeyremax,dyeyre,
     &	  yeyimmin,yeyimmax,dyeyim,
c     
     &        zeyre(1024,1024),zeyim(1024,1024),
c     
     &	  xezremin,xezremax,dxezre,
     &	  xezimmin,xezimmax,dxezim,
     &	  yezremin,yezremax,dyezre,
     &	  yezimmin,yezimmax,dyezim,
c     
     &        zezre(1024,1024),zezim(1024,1024),
c     
     &	  xlam
c     
      integer   ieyrex,ieyimx,ieyrey,ieyimy,
     &	    iezrex,iezimx,iezrey,iezimy
c
	end structure  ! source4
c	end type source4  
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
