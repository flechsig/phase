c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/misali.F
c$$$ Date      : <16 Feb 04 16:37:12 flechsig> 
c$$$ Time-stamp: <16 Feb 04 16:44:01 flechsig> 

c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

	subroutine misali(a,anew,drz,drl,drw,dw,dl,dz)
	implicit real*8(a-h,o-z)

	dimension a(0:5,0:5),anew(0:5,0:5)

c----------- subtract offset
	a00=a(0,0)
	a(0,0)=0.d0

c----------- rotation around normal

	call misali1(a,anew,drz)

c----------- rotation around l-axis
	call misali2(anew,anew,drl)

c----------- rotation around w-axis
	call misali3(anew,anew,drw)

c----------- translation

	dz=dz+a00
	call misali4(anew,anew,dw,dl,dz)

	return
	end
c /afs/psi.ch/user/f/flechsig/phase/src/phase/misali.for
