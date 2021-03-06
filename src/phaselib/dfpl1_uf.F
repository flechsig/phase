c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phaselib/dfpl1_uf.F
c$$$ Date      : <06 Feb 04 09:38:12 flechsig> 
c$$$ Time-stamp: <02 Sep 14 12:02:11 flechsig> 
c$$$ Author    : Uwe Flechsig, flechsig@psi.ch
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

c ******************************************************************************
c
c   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
c                      Paul Scherrer Institut Villigen, Switzerland
c   
c   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
c          Uwe Flechsig,    uwe.flechsig@psi.ch
c
c ------------------------------------------------------------------------------
c
c   This file is part of PHASE.
c
c   PHASE is free software: you can redistribute it and/or modify
c   it under the terms of the GNU General Public License as published by
c   the Free Software Foundation, version 3 of the License, or
c   (at your option) any later version.
c
c   PHASE is distributed in the hope that it will be useful,
c   but WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU General Public License for more details.
c
c   You should have received a copy of the GNU General Public License
c   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
c
c ******************************************************************************


	subroutine subdfpl1(g,a,ra,pd)
	implicit real*8(a-h,o-z)
	real*8 l
	dimension a(0:5,0:5)


#include '../phase/phase_struct.F'

        record /geometryst/ g
        record /rayst/ ra
        record /parder4/ pd
 
      xdefl=dflotj(g.idefl)
      yi=ra.ri.yi
      zi=ra.ri.zi
      yp=ra.rf.yp
      zp=ra.rf.zp
      w=ra.oe.w
      l=ra.oe.xl
      ans1=l*(-2.*a(1.,1.)*g.cosb*g.rp+2.*a(1.,0.)*a(0.,1.))+
     . yi*(-2.*a(1.,0.)*g.sinb*xdefl+2.*g.cosb*xdefl)-2.*a(1.,
     . 0.)*g.cosb*g.rp-2.*g.rp*g.sinb
      pd.arg1w1=-8.*w**3*yi*a(4.,0.)*g.sinb*xdefl+w**3*(-8.*a(
     . 4.,0.)*g.cosb*g.rp+8.*a(3.,0.)*a(1.,0.)+4.*a(2.,0.)**2)-
     . 6.*w**2*l*yi*a(3.,1.)*g.sinb*xdefl+w**2*l*(-6.*a(3.,1.
     . )*g.cosb*g.rp+6.*a(3.,0.)*a(0.,1.)+6.*a(2.,1.)*a(1.,0.)
     . +6.*a(2.,0.)*a(1.,1.))-6.*w**2*yi*a(3.,0.)*g.sinb*
     . xdefl+w**2*(-6.*a(3.,0.)*g.cosb*g.rp+6.*a(2.,0.)*a(1.,
     . 0.))-4.*w*l**2*yi*a(2.,2.)*g.sinb*xdefl+w*l**2*(-4.*a(
     . 2.,2.)*g.cosb*g.rp+4.*a(2.,1.)*a(0.,1.)+4.*a(2.,0.)*a(0.
     . ,2.)+4.*a(1.,2.)*a(1.,0.)+2.*a(1.,1.)**2)-4.*w*l*yi
     . *a(2.,1.)*g.sinb*xdefl+w*l*(-4.*a(2.,1.)*g.cosb*g.rp+4.*a
     . (2.,0.)*a(0.,1.)+4.*a(1.,1.)*a(1.,0.))-4.*w*yi*a(2.
     . ,0.)*g.sinb*xdefl+w*(-4.*a(2.,0.)*g.cosb*g.rp+2.*a(1.,0.)
     . **2+2.)-2.*l**3*yi*a(1.,3.)*g.sinb*xdefl+l**3*(-2.*a(
     . 1.,3.)*g.cosb*g.rp+2.*a(1.,2.)*a(0.,1.)+2.*a(1.,1.)*a(0.
     . ,2.)+2.*a(1.,0.)*a(0.,3.))-2.*l**2*yi*a(1.,2.)*g.sinb
     . *xdefl+l**2*(-2.*a(1.,2.)*g.cosb*g.rp+2.*a(1.,1.)*a(0.,
     . 1.)+2.*a(1.,0.)*a(0.,2.))-2.*l*yi*a(1.,1.)*g.sinb*
     . xdefl+ans1
      ans1=l*(-4.*a(0.,2.)*g.cosb*g.rp+2.*a(0.,1.)**2+2.)-2.*
     . yi*a(0.,1.)*g.sinb*xdefl-2.*zi-2.*a(0.,1.)*g.cosb*g.rp
      pd.arg1l1=-2.*w**3*yi*a(3.,1.)*g.sinb*xdefl+w**3*(-2.*a(
     . 3.,1.)*g.cosb*g.rp+2.*a(3.,0.)*a(0.,1.)+2.*a(2.,1.)*a(1.
     . ,0.)+2.*a(2.,0.)*a(1.,1.))-4.*w**2*l*yi*a(2.,2.)*
     . g.sinb*xdefl+w**2*l*(-4.*a(2.,2.)*g.cosb*g.rp+4.*a(2.,1.)
     . *a(0.,1.)+4.*a(2.,0.)*a(0.,2.)+4.*a(1.,2.)*a(1.,0.)
     . +2.*a(1.,1.)**2)-2.*w**2*yi*a(2.,1.)*g.sinb*xdefl+w**
     . 2*(-2.*a(2.,1.)*g.cosb*g.rp+2.*a(2.,0.)*a(0.,1.)+2.*a(
     . 1.,1.)*a(1.,0.))-6.*w*l**2*yi*a(1.,3.)*g.sinb*xdefl+w*
     . l**2*(-6.*a(1.,3.)*g.cosb*g.rp+6.*a(1.,2.)*a(0.,1.)+6.*
     . a(1.,1.)*a(0.,2.)+6.*a(1.,0.)*a(0.,3.))-4.*w*l*yi*a
     . (1.,2.)*g.sinb*xdefl+w*l*(-4.*a(1.,2.)*g.cosb*g.rp+4.*a(
     . 1.,1.)*a(0.,1.)+4.*a(1.,0.)*a(0.,2.))-2.*w*yi*a(1.,
     . 1.)*g.sinb*xdefl+w*(-2.*a(1.,1.)*g.cosb*g.rp+2.*a(1.,0.)*a
     . (0.,1.))-8.*l**3*yi*a(0.,4.)*g.sinb*xdefl+l**3*(-8.*a
     . (0.,4.)*g.cosb*g.rp+8.*a(0.,3.)*a(0.,1.)+4.*a(0.,2.)**2
     . )-6.*l**2*yi*a(0.,3.)*g.sinb*xdefl+l**2*(-6.*a(0.,3.)
     . *g.cosb*g.rp+6.*a(0.,2.)*a(0.,1.))-4.*l*yi*a(0.,2.)*
     . g.sinb*xdefl+ans1
      pd.arg1w2=-24.*w**2*yi*a(4.,0.)*g.sinb*xdefl+w**2*(-24.*a
     . (4.,0.)*g.cosb*g.rp+24.*a(3.,0.)*a(1.,0.)+12.*a(2.,0.)
     . **2)-12.*w*l*yi*a(3.,1.)*g.sinb*xdefl+w*l*(-12.*a(3.,
     . 1.)*g.cosb*g.rp+12.*a(3.,0.)*a(0.,1.)+12.*a(2.,1.)*a(1.,
     . 0.)+12.*a(2.,0.)*a(1.,1.))-12.*w*yi*a(3.,0.)*g.sinb*
     . xdefl+w*(-12.*a(3.,0.)*g.cosb*g.rp+12.*a(2.,0.)*a(1.,0.
     . ))-4.*l**2*yi*a(2.,2.)*g.sinb*xdefl+l**2*(-4.*a(2.,2.
     . )*g.cosb*g.rp+4.*a(2.,1.)*a(0.,1.)+4.*a(2.,0.)*a(0.,2.)
     . +4.*a(1.,2.)*a(1.,0.)+2.*a(1.,1.)**2)-4.*l*yi*a(2.,
     . 1.)*g.sinb*xdefl+l*(-4.*a(2.,1.)*g.cosb*g.rp+4.*a(2.,0.)*a
     . (0.,1.)+4.*a(1.,1.)*a(1.,0.))-4.*yi*a(2.,0.)*g.sinb*
     . xdefl-4.*a(2.,0.)*g.cosb*g.rp+2.*a(1.,0.)**2+2.
      pd.arg1l2=-4.*w**2*yi*a(2.,2.)*g.sinb*xdefl+w**2*(-4.*a(
     . 2.,2.)*g.cosb*g.rp+4.*a(2.,1.)*a(0.,1.)+4.*a(2.,0.)*a(0.
     . ,2.)+4.*a(1.,2.)*a(1.,0.)+2.*a(1.,1.)**2)-12.*w*l*
     . yi*a(1.,3.)*g.sinb*xdefl+w*l*(-12.*a(1.,3.)*g.cosb*g.rp+
     . 12.*a(1.,2.)*a(0.,1.)+12.*a(1.,1.)*a(0.,2.)+12.*a(1.
     . ,0.)*a(0.,3.))-4.*w*yi*a(1.,2.)*g.sinb*xdefl+w*(-4.*a
     . (1.,2.)*g.cosb*g.rp+4.*a(1.,1.)*a(0.,1.)+4.*a(1.,0.)*a(
     . 0.,2.))-24.*l**2*yi*a(0.,4.)*g.sinb*xdefl+l**2*(-24.*a
     . (0.,4.)*g.cosb*g.rp+24.*a(0.,3.)*a(0.,1.)+12.*a(0.,2.)
     . **2)-12.*l*yi*a(0.,3.)*g.sinb*xdefl+l*(-12.*a(0.,3.)*
     . g.cosb*g.rp+12.*a(0.,2.)*a(0.,1.))-4.*yi*a(0.,2.)*g.sinb*
     . xdefl-4.*a(0.,2.)*g.cosb*g.rp+2.*a(0.,1.)**2+2.
      ans1=yp*(2.*a(1.,0.)*g.sina*xdefl-2.*g.cosa*xdefl)-2.*a(
     . 1.,0.)*g.cosa*g.r-2.*g.r*g.sina
      pd.arg2w1=8.*w**3*yp*a(4.,0.)*g.sina*xdefl+w**3*(-8.*a(4.
     . ,0.)*g.cosa*g.r+8.*a(3.,0.)*a(1.,0.)+4.*a(2.,0.)**2)+6.
     . *w**2*l*yp*a(3.,1.)*g.sina*xdefl+w**2*l*(-6.*a(3.,1.)
     . *g.cosa*g.r+6.*a(3.,0.)*a(0.,1.)+6.*a(2.,1.)*a(1.,0.)+
     . 6.*a(2.,0.)*a(1.,1.))+6.*w**2*yp*a(3.,0.)*g.sina*xdefl
     . +w**2*(-6.*a(3.,0.)*g.cosa*g.r+6.*a(2.,0.)*a(1.,0.))+4.
     . *w*l**2*yp*a(2.,2.)*g.sina*xdefl+w*l**2*(-4.*a(2.,2.)
     . *g.cosa*g.r+4.*a(2.,1.)*a(0.,1.)+4.*a(2.,0.)*a(0.,2.)+
     . 4.*a(1.,2.)*a(1.,0.)+2.*a(1.,1.)**2)+4.*w*l*yp*a(2.,
     . 1.)*g.sina*xdefl+w*l*(-4.*a(2.,1.)*g.cosa*g.r+4.*a(2.,0.)*
     . a(0.,1.)+4.*a(1.,1.)*a(1.,0.))+4.*w*yp*a(2.,0.)*
     . g.sina*xdefl+w*(-4.*a(2.,0.)*g.cosa*g.r+2.*a(1.,0.)**2+2.
     . )+2.*l**3*yp*a(1.,3.)*g.sina*xdefl+l**3*(-2.*a(1.,3.)
     . *g.cosa*g.r+2.*a(1.,2.)*a(0.,1.)+2.*a(1.,1.)*a(0.,2.)+
     . 2.*a(1.,0.)*a(0.,3.))+2.*l**2*yp*a(1.,2.)*g.sina*xdefl
     . +l**2*(-2.*a(1.,2.)*g.cosa*g.r+2.*a(1.,1.)*a(0.,1.)+2.*
     . a(1.,0.)*a(0.,2.))+2.*l*yp*a(1.,1.)*g.sina*xdefl+l*(-
     . 2.*a(1.,1.)*g.cosa*g.r+2.*a(1.,0.)*a(0.,1.))+ans1
      ans1=-2.*zp-2.*a(0.,1.)*g.cosa*g.r
      pd.arg2l1=2.*w**3*yp*a(3.,1.)*g.sina*xdefl+w**3*(-2.*a(3.
     . ,1.)*g.cosa*g.r+2.*a(3.,0.)*a(0.,1.)+2.*a(2.,1.)*a(1.,
     . 0.)+2.*a(2.,0.)*a(1.,1.))+4.*w**2*l*yp*a(2.,2.)*g.sina
     . *xdefl+w**2*l*(-4.*a(2.,2.)*g.cosa*g.r+4.*a(2.,1.)*a(0.
     . ,1.)+4.*a(2.,0.)*a(0.,2.)+4.*a(1.,2.)*a(1.,0.)+2.*a
     . (1.,1.)**2)+2.*w**2*yp*a(2.,1.)*g.sina*xdefl+w**2*(-
     . 2.*a(2.,1.)*g.cosa*g.r+2.*a(2.,0.)*a(0.,1.)+2.*a(1.,1.)*
     . a(1.,0.))+6.*w*l**2*yp*a(1.,3.)*g.sina*xdefl+w*l**2*(
     . -6.*a(1.,3.)*g.cosa*g.r+6.*a(1.,2.)*a(0.,1.)+6.*a(1.,1.
     . )*a(0.,2.)+6.*a(1.,0.)*a(0.,3.))+4.*w*l*yp*a(1.,2.)
     . *g.sina*xdefl+w*l*(-4.*a(1.,2.)*g.cosa*g.r+4.*a(1.,1.)*a(
     . 0.,1.)+4.*a(1.,0.)*a(0.,2.))+2.*w*yp*a(1.,1.)*g.sina*
     . xdefl+w*(-2.*a(1.,1.)*g.cosa*g.r+2.*a(1.,0.)*a(0.,1.))+
     . 8.*l**3*yp*a(0.,4.)*g.sina*xdefl+l**3*(-8.*a(0.,4.)*
     . g.cosa*g.r+8.*a(0.,3.)*a(0.,1.)+4.*a(0.,2.)**2)+6.*l**2
     . *yp*a(0.,3.)*g.sina*xdefl+l**2*(-6.*a(0.,3.)*g.cosa*g.r+
     . 6.*a(0.,2.)*a(0.,1.))+4.*l*yp*a(0.,2.)*g.sina*xdefl+l*
     . (-4.*a(0.,2.)*g.cosa*g.r+2.*a(0.,1.)**2+2.)+2.*yp*a(0.,
     . 1.)*g.sina*xdefl+ans1
      pd.arg2w2=24.*w**2*yp*a(4.,0.)*g.sina*xdefl+w**2*(-24.*a(
     . 4.,0.)*g.cosa*g.r+24.*a(3.,0.)*a(1.,0.)+12.*a(2.,0.)**2)
     . +12.*w*l*yp*a(3.,1.)*g.sina*xdefl+w*l*(-12.*a(3.,1.)*
     . g.cosa*g.r+12.*a(3.,0.)*a(0.,1.)+12.*a(2.,1.)*a(1.,0.)+
     . 12.*a(2.,0.)*a(1.,1.))+12.*w*yp*a(3.,0.)*g.sina*xdefl+
     . w*(-12.*a(3.,0.)*g.cosa*g.r+12.*a(2.,0.)*a(1.,0.))+4.*l
     . **2*yp*a(2.,2.)*g.sina*xdefl+l**2*(-4.*a(2.,2.)*g.cosa*
     . g.r+4.*a(2.,1.)*a(0.,1.)+4.*a(2.,0.)*a(0.,2.)+4.*a(1.
     . ,2.)*a(1.,0.)+2.*a(1.,1.)**2)+4.*l*yp*a(2.,1.)*g.sina
     . *xdefl+l*(-4.*a(2.,1.)*g.cosa*g.r+4.*a(2.,0.)*a(0.,1.)+
     . 4.*a(1.,1.)*a(1.,0.))+4.*yp*a(2.,0.)*g.sina*xdefl-4.*a
     . (2.,0.)*g.cosa*g.r+2.*a(1.,0.)**2+2.
      pd.arg2l2=4.*w**2*yp*a(2.,2.)*g.sina*xdefl+w**2*(-4.*a(2.
     . ,2.)*g.cosa*g.r+4.*a(2.,1.)*a(0.,1.)+4.*a(2.,0.)*a(0.,
     . 2.)+4.*a(1.,2.)*a(1.,0.)+2.*a(1.,1.)**2)+12.*w*l*yp*
     . a(1.,3.)*g.sina*xdefl+w*l*(-12.*a(1.,3.)*g.cosa*g.r+12.*a
     . (1.,2.)*a(0.,1.)+12.*a(1.,1.)*a(0.,2.)+12.*a(1.,0.)
     . *a(0.,3.))+4.*w*yp*a(1.,2.)*g.sina*xdefl+w*(-4.*a(1.,
     . 2.)*g.cosa*g.r+4.*a(1.,1.)*a(0.,1.)+4.*a(1.,0.)*a(0.,2.)
     . )+24.*l**2*yp*a(0.,4.)*g.sina*xdefl+l**2*(-24.*a(0.,
     . 4.)*g.cosa*g.r+24.*a(0.,3.)*a(0.,1.)+12.*a(0.,2.)**2)+
     . 12.*l*yp*a(0.,3.)*g.sina*xdefl+l*(-12.*a(0.,3.)*g.cosa*g.r
     . +12.*a(0.,2.)*a(0.,1.))+4.*yp*a(0.,2.)*g.sina*xdefl-
     . 4.*a(0.,2.)*g.cosa*g.r+2.*a(0.,1.)**2+2.
      ans1=w*l*(-2.*a(1.,1.)*g.cosb*g.rp+2.*a(1.,0.)*a(0.,1.))
     . +w*yi*(-2.*a(1.,0.)*g.sinb*xdefl+2.*g.cosb*xdefl)+w*(-
     . 2.*a(1.,0.)*g.cosb*g.rp-2.*g.rp*g.sinb)-2.*l**4*yi*a(0.,4.)*
     . g.sinb*xdefl+l**4*(-2.*a(0.,4.)*g.cosb*g.rp+2.*a(0.,3.)*a
     . (0.,1.)+a(0.,2.)**2)-2.*l**3*yi*a(0.,3.)*g.sinb*xdefl
     . +l**3*(-2.*a(0.,3.)*g.cosb*g.rp+2.*a(0.,2.)*a(0.,1.))-
     . 2.*l**2*yi*a(0.,2.)*g.sinb*xdefl+l**2*(-2.*a(0.,2.)*
     . g.cosb*g.rp+a(0.,1.)**2+1.)-2.*l*yi*a(0.,1.)*g.sinb*xdefl
     . -2.*l*zi-2.*l*a(0.,1.)*g.cosb*g.rp+yi**2*xdefl**2+zi**2
     . +g.rp**2
      pd.arg1=-2.*w**4*yi*a(4.,0.)*g.sinb*xdefl+w**4*(-2.*a(4.,
     . 0.)*g.cosb*g.rp+2.*a(3.,0.)*a(1.,0.)+a(2.,0.)**2)-2.*w**
     . 3*l*yi*a(3.,1.)*g.sinb*xdefl+w**3*l*(-2.*a(3.,1.)*
     . g.cosb*g.rp+2.*a(3.,0.)*a(0.,1.)+2.*a(2.,1.)*a(1.,0.)+
     . 2.*a(2.,0.)*a(1.,1.))-2.*w**3*yi*a(3.,0.)*g.sinb*xdefl
     . +w**3*(-2.*a(3.,0.)*g.cosb*g.rp+2.*a(2.,0.)*a(1.,0.))-
     . 2.*w**2*l**2*yi*a(2.,2.)*g.sinb*xdefl+w**2*l**2*(-2.*a
     . (2.,2.)*g.cosb*g.rp+2.*a(2.,1.)*a(0.,1.)+2.*a(2.,0.)*a(
     . 0.,2.)+2.*a(1.,2.)*a(1.,0.)+a(1.,1.)**2)-2.*w**2*l*
     . yi*a(2.,1.)*g.sinb*xdefl+w**2*l*(-2.*a(2.,1.)*g.cosb*g.rp
     . +2.*a(2.,0.)*a(0.,1.)+2.*a(1.,1.)*a(1.,0.))-2.*w**2
     . *yi*a(2.,0.)*g.sinb*xdefl+w**2*(-2.*a(2.,0.)*g.cosb*g.rp+
     . a(1.,0.)**2+1.)-2.*w*l**3*yi*a(1.,3.)*g.sinb*xdefl+w*
     . l**3*(-2.*a(1.,3.)*g.cosb*g.rp+2.*a(1.,2.)*a(0.,1.)+2.*
     . a(1.,1.)*a(0.,2.)+2.*a(1.,0.)*a(0.,3.))-2.*w*l**2*
     . yi*a(1.,2.)*g.sinb*xdefl+w*l**2*(-2.*a(1.,2.)*g.cosb*g.rp
     . +2.*a(1.,1.)*a(0.,1.)+2.*a(1.,0.)*a(0.,2.))-2.*w*l*
     . yi*a(1.,1.)*g.sinb*xdefl+ans1
      ans1=w*l*(-2.*a(1.,1.)*g.cosa*g.r+2.*a(1.,0.)*a(0.,1.))+
     . w*yp*(2.*a(1.,0.)*g.sina*xdefl-2.*g.cosa*xdefl)+w*(-2.*
     . a(1.,0.)*g.cosa*g.r-2.*g.r*g.sina)+2.*l**4*yp*a(0.,4.)*g.sina
     . *xdefl+l**4*(-2.*a(0.,4.)*g.cosa*g.r+2.*a(0.,3.)*a(0.,
     . 1.)+a(0.,2.)**2)+2.*l**3*yp*a(0.,3.)*g.sina*xdefl+l**3
     . *(-2.*a(0.,3.)*g.cosa*g.r+2.*a(0.,2.)*a(0.,1.))+2.*l**2
     . *yp*a(0.,2.)*g.sina*xdefl+l**2*(-2.*a(0.,2.)*g.cosa*g.r+a
     . (0.,1.)**2+1.)+2.*l*yp*a(0.,1.)*g.sina*xdefl-2.*l*zp-
     . 2.*l*a(0.,1.)*g.cosa*g.r+yp**2*xdefl**2+zp**2+g.r**2
      pd.arg2=2.*w**4*yp*a(4.,0.)*g.sina*xdefl+w**4*(-2.*a(4.,
     . 0.)*g.cosa*g.r+2.*a(3.,0.)*a(1.,0.)+a(2.,0.)**2)+2.*w**3
     . *l*yp*a(3.,1.)*g.sina*xdefl+w**3*l*(-2.*a(3.,1.)*g.cosa
     . *g.r+2.*a(3.,0.)*a(0.,1.)+2.*a(2.,1.)*a(1.,0.)+2.*a(
     . 2.,0.)*a(1.,1.))+2.*w**3*yp*a(3.,0.)*g.sina*xdefl+w**3
     . *(-2.*a(3.,0.)*g.cosa*g.r+2.*a(2.,0.)*a(1.,0.))+2.*w**2
     . *l**2*yp*a(2.,2.)*g.sina*xdefl+w**2*l**2*(-2.*a(2.,2.
     . )*g.cosa*g.r+2.*a(2.,1.)*a(0.,1.)+2.*a(2.,0.)*a(0.,2.)+
     . 2.*a(1.,2.)*a(1.,0.)+a(1.,1.)**2)+2.*w**2*l*yp*a(2.,
     . 1.)*g.sina*xdefl+w**2*l*(-2.*a(2.,1.)*g.cosa*g.r+2.*a(2.,
     . 0.)*a(0.,1.)+2.*a(1.,1.)*a(1.,0.))+2.*w**2*yp*a(2.,
     . 0.)*g.sina*xdefl+w**2*(-2.*a(2.,0.)*g.cosa*g.r+a(1.,0.)**2
     . +1.)+2.*w*l**3*yp*a(1.,3.)*g.sina*xdefl+w*l**3*(-2.*a
     . (1.,3.)*g.cosa*g.r+2.*a(1.,2.)*a(0.,1.)+2.*a(1.,1.)*a(
     . 0.,2.)+2.*a(1.,0.)*a(0.,3.))+2.*w*l**2*yp*a(1.,2.)*
     . g.sina*xdefl+w*l**2*(-2.*a(1.,2.)*g.cosa*g.r+2.*a(1.,1.)*
     . a(0.,1.)+2.*a(1.,0.)*a(0.,2.))+2.*w*l*yp*a(1.,1.)*
     . g.sina*xdefl+ans1
      pd.arg1w1l1=-6.*w**2*yi*a(3.,1.)*g.sinb*xdefl+w**2*(-6.*a
     . (3.,1.)*g.cosb*g.rp+6.*a(3.,0.)*a(0.,1.)+6.*a(2.,1.)*a(
     . 1.,0.)+6.*a(2.,0.)*a(1.,1.))-8.*w*l*yi*a(2.,2.)*g.sinb
     . *xdefl+w*l*(-8.*a(2.,2.)*g.cosb*g.rp+8.*a(2.,1.)*a(0.,
     . 1.)+8.*a(2.,0.)*a(0.,2.)+8.*a(1.,2.)*a(1.,0.)+4.*a(
     . 1.,1.)**2)-4.*w*yi*a(2.,1.)*g.sinb*xdefl+w*(-4.*a(2.,
     . 1.)*g.cosb*g.rp+4.*a(2.,0.)*a(0.,1.)+4.*a(1.,1.)*a(1.,0.
     . ))-6.*l**2*yi*a(1.,3.)*g.sinb*xdefl+l**2*(-6.*a(1.,3.
     . )*g.cosb*g.rp+6.*a(1.,2.)*a(0.,1.)+6.*a(1.,1.)*a(0.,2.)
     . +6.*a(1.,0.)*a(0.,3.))-4.*l*yi*a(1.,2.)*g.sinb*xdefl+
     . l*(-4.*a(1.,2.)*g.cosb*g.rp+4.*a(1.,1.)*a(0.,1.)+4.*a(
     . 1.,0.)*a(0.,2.))-2.*yi*a(1.,1.)*g.sinb*xdefl-2.*a(1.,
     . 1.)*g.cosb*g.rp+2.*a(1.,0.)*a(0.,1.)
      pd.arg2w1l1=6.*w**2*yp*a(3.,1.)*g.sina*xdefl+w**2*(-6.*a(
     . 3.,1.)*g.cosa*g.r+6.*a(3.,0.)*a(0.,1.)+6.*a(2.,1.)*a(1.,
     . 0.)+6.*a(2.,0.)*a(1.,1.))+8.*w*l*yp*a(2.,2.)*g.sina*
     . xdefl+w*l*(-8.*a(2.,2.)*g.cosa*g.r+8.*a(2.,1.)*a(0.,1.)
     . +8.*a(2.,0.)*a(0.,2.)+8.*a(1.,2.)*a(1.,0.)+4.*a(1.,
     . 1.)**2)+4.*w*yp*a(2.,1.)*g.sina*xdefl+w*(-4.*a(2.,1.)*
     . g.cosa*g.r+4.*a(2.,0.)*a(0.,1.)+4.*a(1.,1.)*a(1.,0.))+
     . 6.*l**2*yp*a(1.,3.)*g.sina*xdefl+l**2*(-6.*a(1.,3.)*
     . g.cosa*g.r+6.*a(1.,2.)*a(0.,1.)+6.*a(1.,1.)*a(0.,2.)+6.
     . *a(1.,0.)*a(0.,3.))+4.*l*yp*a(1.,2.)*g.sina*xdefl+l*(
     . -4.*a(1.,2.)*g.cosa*g.r+4.*a(1.,1.)*a(0.,1.)+4.*a(1.,0.
     . )*a(0.,2.))+2.*yp*a(1.,1.)*g.sina*xdefl-2.*a(1.,1.)*
     . g.cosa*g.r+2.*a(1.,0.)*a(0.,1.)
      return
      end
