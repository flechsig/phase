c$$$ File      : /afs/psi.ch/project/phase/src/phase/misali_8.f
c$$$ Date      : <07 Mar 12 11:13:47 flechsig> 
c$$$ Time-stamp: <07 Mar 12 11:14:03 flechsig> 

c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

	subroutine misali_8(a,anew,drz,drl,drw,dw,dl,dz)
	implicit real*8(a-h,o-z)

	dimension a(0:8,0:8),anew(0:8,0:8)

c----------- subtract offset
	a00=a(0,0)
	a(0,0)=0.d0

c----------- rotation around normal

	call misali1_8(a,anew,drz)

c----------- rotation around l-axis
	call misali2_8(anew,anew,drl)

c----------- rotation around w-axis
	call misali3_8(anew,anew,drw)

c----------- translation

	dz=dz+a00     ! UF not good we change input variable
	call misali4_8(anew,anew,dw,dl,dz)

	return
	end

