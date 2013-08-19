c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

cc TL 26.3.08 fixed the dimension problem
cc 1024 is the Dimension of the fieldsw in the src4-structure
cc #define 1024 256
cc Big1024 is for internal temporary fields
cc #define Big1024 1024

c UF revision 1.7
c date: 2013/08/16 07:33:21;  author: flechsig;  state: Exp;  lines: +2 -8
c no change- checked that the routine is completely independent of the phase c-
c code, the field dimensions come from the include file with hardcoded static
c defined fields

cc TL 29.01.2009
#include "pha4idlFORTRANdefines.h.f"
cc#define 2048 1024


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

      SUBROUTINE phasesrcwfgauss_nostructs(zezre,zezim,zeyre,zeyim
     &					,ianzz,zmin,zmax,ianzy,ymin,ymax
     &					,w0, deltax, xlambda
     &                                  ,ez0, ey0, dphi_zy )
     
      implicit none    

        integer kkk
	integer i,j,ianzz,ianzy
	real*8 z,dz,zmin,zmax,y,dy,ymin,ymax,w0,w,deltax,xlambda,xlam
     &	,xk,xr,small1,small3,pi,pow,dpow,R,theta,arg, ez0, ey0, dphi_zy
     &      ,rr2,z0,y0 
	dimension z(MaxDim),y(MaxDim)

	complex*16 egauss(MaxDim,MaxDim),xi,fact

	real*8 zezre(MaxDim,MaxDim),zezim(MaxDim,MaxDim)
     &	      ,zeyre(MaxDim,MaxDim),zeyim(MaxDim,MaxDim)

#ifdef DEBUG	
	write(*,*)'***'
	write(*,*)'phaSrcWFGauss started...'
	write(*,*)'max gridsize= ', MaxDim
#endif
	
c in C:      call pha_define_src4_grid(gb,ianzz,zmin,zmax,ianzy,ymin,ymax)
	
	z0=(zmax+zmin)/2.d0
	y0=(ymax+ymin)/2.d0
	
	pi=4.d0*datan(1.d0)

	small1=-72.
	small3=1.e-16 
	! Umspeichern -> lambda im rufenden Prog, nicht veraendert ...
	  xlam=dble(xlambda)
	  
	! wavelength in nm als Eingabeparameter
	  xlam=xlam*1.d-6	! units in mm (from nm)
c	write(*,*),xlam,' ... xlam'
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
	  egauss(i,j)=cdexp(xi*(xk*deltax-theta))*fact*dexp(arg)
	 else
	  egauss(i,j)=0.d0
	endif

c	dpow= max(dabs(dreal(egauss(i,j))),small3)**2+
c     &          max(dabs(dimag(egauss(i,j))),small3)**2
     	dpow= (dabs(dreal(egauss(i,j))))**2 +
     &          (dabs(dimag(egauss(i,j))))**2

	pow=pow+dpow

	enddo
	enddo
	
	pow  = dsqrt( pow*dz*dy*(ez0*ez0 + ey0*ey0) )
	fact = cdexp( xi * dphi_zy )

	do i=1,ianzz
	do j=1,ianzy  
	  zezre(i,j)=ez0*dreal(egauss(i,j))/pow
	  zezim(i,j)=ez0*dimag(egauss(i,j))/pow
	  zeyre(i,j)=ey0*dreal(fact*egauss(i,j))/pow
	  zeyim(i,j)=ey0*dimag(fact*egauss(i,j))/pow
	enddo
	enddo

cc	write(*,*)'phaSrcWFGauss finished...'
cc	write(*,*)'***'
cc	write(*,*)' '

	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

     
