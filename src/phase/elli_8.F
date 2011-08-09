c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/elli_8.F
c$$$ Date      : <08 Aug 11 12:08:09 flechsig> 
c$$$ Time-stamp: <09 Aug 11 08:05:05 flechsig> 

c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

      subroutine elli_8(r1,r2,sina,cosa,a,b,c,x0,y0,tandel,a1)
      implicit none
      real *8 r1,r2,sina,cosa,a,b,c,x0,y0,tandel,a1,sina1,cosa1,sd,cd,ax0,bx0,cx0,dx0,cx1,ax2,ax1,az2
      dimension a1(0:8,0:8)
c---------------------------------------------------------------------------
c
c	Inputparameter:
c
c	r1
c	r2
c	sina
c	cosa
c
c	Outputparameter:
c
c	Ellipsenparameter a, b, c
c	Auftreffpunkt auf Ellipse (x0,y0)	
c	tangentensteigung an Auftreffpunkt tendel
c	Spiegelkoordinaten a1(i,j)
c
c---------------------------------------------------------------------------

      sina1=dabs(cosa)
      cosa1=dabs(sina)

c     get ellipse parameters
      c=0.5d0*dsqrt((r1+r2*(cosa1**2-sina1**2))**2+(r2*2.d0*sina1*cosa1)**2)
      a=(r1+r2)/2.d0
      b=dsqrt(a**2-c**2)

      x0=(r1**2-r2**2)/(4.d0*c)
      y0=-b*dsqrt(1.d0-x0**2/a**2)
      tandel=(b/a)*(x0/dsqrt(a**2-x0**2))
      sd=tandel/dsqrt(1.d0+tandel**2)
      cd=dsqrt(1.d0-sd**2)

      ax0=a**2*b**2*cd**2+b**4*sd**2-b**2*cd**2*x0**2-
     .2*b**2*cd*sd*x0*y0-b**2*sd**2*y0**2
      az2=-a**2*cd**2-b**2*sd**2
      ax1=-2*b**2*cd**3*x0-2*b**2*cd**2*sd*y0-
     .2*b**2*cd*sd**2*x0-2*b**2*sd**3*y0
      ax2=-b**2*cd**4-2*b**2*cd**2*sd**2-b**2*sd**4
      bx0=-a
      cx0=-a**2*cd*y0+b**2*sd*x0
      cx1=-a**2*cd*sd+b**2*cd*sd
      dx0=a**2*cd**2+b**2*sd**2

      type*,ax0,ax1,ax2,bx0,cx0,cx1,dx0
      a1(0.0,0.0)=(dsqrt(ax0)*bx0+cx0)/dx0
      a1(0.0,1.0)=0.0
      a1(0.0,2.0)=(dsqrt(ax0)*az2*bx0)/(2.0*ax0*dx0)
      a1(0.0,3.0)=0.0
      a1(0.0,4.0)=(-dsqrt(ax0)*az2**2*bx0)/(8.0*ax0**2*dx0)
      a1(0.0,5.0)=0.0
      a1(0.0,6.0)=(dsqrt(ax0)*az2**3*bx0)/(16.0*ax0**3*dx0)
      a1(0.0,7.0)=0.0
      a1(0.0,8.0)=(-5.0*dsqrt(ax0)*az2**4*bx0)/(128.0*ax0**4*dx0)
      a1(1.0,0.0)=(dsqrt(ax0)*ax1*bx0+2.0*ax0*cx1)/(2.0*ax0*dx0)
      a1(1.0,1.0)=0.0
      a1(1.0,2.0)=(-dsqrt(ax0)*ax1*az2*bx0)/(4.0*ax0**2*dx0)
      a1(1.0,3.0)=0.0
      a1(1.0,4.0)=(3.0*dsqrt(ax0)*ax1*az2**2*bx0)/(16.0*ax0**3*dx0)
      a1(1.0,5.0)=0.0
      a1(1.0,6.0)=(-5.0*dsqrt(ax0)*ax1*az2**3*bx0)/(32.0*ax0**4*dx0)
      a1(1.0,7.0)=0.0
      a1(2.0,0.0)=(dsqrt(ax0)*bx0*(4.0*ax0*ax2-ax1**2))/(8.0*ax0**2*
     . dx0)
      a1(2.0,1.0)=0.0
      a1(2.0,2.0)=(dsqrt(ax0)*az2*bx0*(-4.0*ax0*ax2+3.0*ax1**2))/(16.0
     . *ax0**3*dx0)
      a1(2.0,3.0)=0.0
      a1(2.0,4.0)=(3.0*dsqrt(ax0)*az2**2*bx0*(4.0*ax0*ax2-5.0*ax1**2))
     . /(64.0*ax0**4*dx0)
      a1(2.0,5.0)=0.0
      a1(2.0,6.0)=(5.0*dsqrt(ax0)*az2**3*bx0*(-4.0*ax0*ax2+7.0*ax1**2)
     . )/(128.0*ax0**5*dx0)
      a1(3.0,0.0)=(dsqrt(ax0)*ax1*bx0*(-4.0*ax0*ax2+ax1**2))/(16.0*ax0
     . **3*dx0)
      a1(3.0,1.0)=0.0
      a1(3.0,2.0)=(dsqrt(ax0)*ax1*az2*bx0*(12.0*ax0*ax2-5.0*ax1**2))/(
     . 32.0*ax0**4*dx0)
      a1(3.0,3.0)=0.0
      a1(3.0,4.0)=(5.0*dsqrt(ax0)*ax1*az2**2*bx0*(-12.0*ax0*ax2+7.0*
     . ax1**2))/(128.0*ax0**5*dx0)
      a1(3.0,5.0)=0.0
      a1(4.0,0.0)=(dsqrt(ax0)*bx0*(-16.0*ax0**2*ax2**2+24.0*ax0*ax1**2
     . *ax2-5.0*ax1**4))/(128.0*ax0**4*dx0)
      a1(4.0,1.0)=0.0
      a1(4.0,2.0)=(dsqrt(ax0)*az2*bx0*(48.0*ax0**2*ax2**2-120.0*ax0*
     . ax1**2*ax2+35.0*ax1**4))/(256.0*ax0**5*dx0)
      a1(4.0,3.0)=0.0
      a1(4.0,4.0)=(15.0*dsqrt(ax0)*az2**2*bx0*(-16.0*ax0**2*ax2**2+
     . 56.0*ax0*ax1**2*ax2-21.0*ax1**4))/(1024.0*ax0**6*dx0)
      a1(5.0,0.0)=(dsqrt(ax0)*ax1*bx0*(48.0*ax0**2*ax2**2-40.0*ax0*ax1
     . **2*ax2+7.0*ax1**4))/(256.0*ax0**5*dx0)
      a1(5.0,1.0)=0.0
      a1(5.0,2.0)=(dsqrt(ax0)*ax1*az2*bx0*(-240.0*ax0**2*ax2**2+280.0*
     . ax0*ax1**2*ax2-63.0*ax1**4))/(512.0*ax0**6*dx0)
      a1(5.0,3.0)=0.0
      a1(6.0,0.0)=(dsqrt(ax0)*bx0*(64.0*ax0**3*ax2**3-240.0*ax0**2*ax1
     . **2*ax2**2+140.0*ax0*ax1**4*ax2-21.0*ax1**6))/(1024.0*ax0**6*
     . dx0)
      a1(6.0,1.0)=0.0
      a1(6.0,2.0)=(dsqrt(ax0)*az2*bx0*(-320.0*ax0**3*ax2**3+1680.0*ax0
     . **2*ax1**2*ax2**2-1260.0*ax0*ax1**4*ax2+231.0*ax1**6))/(2048.0
     . *ax0**7*dx0)
      a1(7.0,0.0)=(dsqrt(ax0)*ax1*bx0*(-320.0*ax0**3*ax2**3+560.0*ax0
     . **2*ax1**2*ax2**2-252.0*ax0*ax1**4*ax2+33.0*ax1**6))/(2048.0*
     . ax0**7*dx0)
      a1(7.0,1.0)=0.0
      a1(8.0,0.0)=(dsqrt(ax0)*bx0*(-1280.0*ax0**4*ax2**4+8960.0*ax0**3
     . *ax1**2*ax2**3-10080.0*ax0**2*ax1**4*ax2**2+3696.0*ax0*ax1**6*
     . ax2-429.0*ax1**8))/(32768.0*ax0**8*dx0)

      return
      end
