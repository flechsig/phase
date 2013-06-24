c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/int_2d.F
c$$$ Date      : <24 Jun 13 14:59:41 flechsig> 
c$$$ Time-stamp: <24 Jun 13 15:00:08 flechsig> 
c$$$ Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

c UF 22.6.13 not used

c**************************************************************
	subroutine int_2d(f,xint,dy,dz,ny,nz)
c**************************************************************

	implicit real*8(a-h,o-z)

        dimension f(4096,4096),fz(4096)

        do i=1,ny
	 fz(i)=0.5d0*(f(i,1)+f(i,nz))
         do j=2,nz-1
          fz(i)=fz(i)+f(i,j)
         enddo
	 fz(i)=fz(i)*dz
	enddo

	xint=0.5d0*(fz(1)+fz(ny))
	do i=2,ny-1
         xint=xint+fz(i)
	enddo

	xint=xint*dy

	return
	end
c------------------------- end int_2d ----------------------
