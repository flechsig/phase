c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/mis.F
c$$$ Date      : <16 Feb 04 16:27:42 flechsig> 
c$$$ Time-stamp: <16 Feb 04 16:28:07 flechsig> 

c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

	implicit real*8(a-h,o-z)	

	dimension anew(0:5,0:5)

	common/mis/dw,dl,dz,drw,drl,drz
	common/mirror/a(0:5,0:5),c1,c2
	common/plot/wmin,wmax,deltaw,xlmin,xlmax,deltal,ianzw,ianzl

	call get_input

	call misali(a,anew,drz,drl,drw,dw,dl,dz)

	do i=0,5
	do j=0,5
	  type*,i,j,a(i,j),anew(i,j)
	enddo
	enddo

	open(unit=10,name='mold.dat',type='new')
	open(unit=11,name='mnew.dat',type='new')
	write(10,*)ianzw,ianzl
	write(11,*)ianzw,ianzl

	do j=1,ianzl
	do i=1,ianzw
	  w=wmin+dflotj(i-1)*deltaw
	  xl=xlmin+dflotj(j-1)*deltal

	  zold=0.
	  znew=0.
	  do k=0,5
	  do l=0,5
	    fact=1.
	    if(k.gt.0)fact=fact*w**k
	    if(l.gt.0)fact=fact*xl**l
	    zold=zold+a(k,l)*fact
	    znew=znew+anew(k,l)*fact
	  enddo
	  enddo

	write(10,*)w,xl,zold
	write(11,*)w,xl,znew

	enddo
	enddo

	close(11)
	close(10)

	stop
	end


c-------------------------------------------------------
	subroutine get_input
c-------------------------------------------------------
	implicit real*8(a-h,o-z)	

	common/mis/dw,dl,dz,drw,drl,drz
	common/mirror/a(0:5,0:5),c1,c2
	common/plot/wmin,wmax,deltaw,xlmin,xlmax,deltal,ianzw,ianzl

	open(unit=10,name='misali.par',type='old')
	read(10,*)dw
	read(10,*)dl
	read(10,*)dz

	read(10,*)drw
	read(10,*)drl
	read(10,*)drz

	read(10,*)c1
	read(10,*)c2

	read(10,*)wmin
	read(10,*)wmax
	read(10,*)xlmin
	read(10,*)xlmax

	ianzw=21
	ianzl=21
	
	deltaw=(wmax-wmin)/dflotj(ianzw-1)
	deltal=(xlmax-xlmin)/dflotj(ianzl-1)
	
	do i=0,5
	do j=0,5
	  a(i,j)=0.d0
	enddo
	enddo

	a(2,0)=c1
	a(0,2)=c2

	return
	end
c end /afs/psi.ch/user/f/flechsig/phase/src/phase/mis.F
