c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/guess.F
c$$$ Date      : <24 Jun 13 14:45:15 flechsig> 
c$$$ Time-stamp: <02 Sep 14 11:57:17 flechsig> 
c$$$ Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;
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


c UF ausgelagert von phase_integration am 22.6.13 

c------------------------------------------------------
	subroutine guess(m4,g,am,src,apr,cs,ra,ifl,xi,xir,st,blp)
c------------------------------------------------------
c	variable grid generation for dyi and dzi:
c	- calculate tza, tzp for central ray: yi = zi = 0
c	  with ymin ymax zmin zmax ianz etc like in 
c	  parameter file
c	- befor first call of subroutine guess
c	  determine distfocy and distfocz:

c	  dyi_loc=dyi_loc+yi_loc/xi.distfocy
c	  dzi_loc=dzi_loc+zi_loc/xi.distfocz

c	- adapt grid in size and location accordingly
c	  and get new ymin, ymax, zmin, zmax
c	- grid for yi + dyi etc
c
c
c	Routine gelegentlich rausschmeissen
c
c---------------------------------------------------------------------
c UF 22.6.13 will not work with common blocks etc
c---------------------------------------------------------------------

        implicit real*8(a-h,o-z)
        implicit integer(i-n)
        integer  blp

#include '../phase/phase_struct.F'


      common/xlengthc/xlen1cc(0:4,0:4,0:4,0:4),
     &                 xlen1c(0:4,0:4,0:4,0:4),
     &                 xlen1c_r(0:4,0:4),
     &                 xlen1c_rr(0:4),xlength1

      common/xlengthd/xlen2cc(0:4,0:4,0:4,0:4),
     &                 xlen2c(0:4,0:4,0:4,0:4),
     &                 xlen2c_r(0:4,0:4),
     &                 xlen2c_rr(0:4),xlength2

        common/map7/wc(0:4,0:4,0:4,0:4),
     &              xlc(0:4,0:4,0:4,0:4),
     &              ypc1(0:4,0:4,0:4,0:4),
     &              zpc1(0:4,0:4,0:4,0:4)

   	common/geometry/sina,cosa,sinb,cosb,
     &                  r,rp,xdens(0:4),xlam,idefl
        common/orders/iord,iexpand

        record /constants/ cs
        record /geometryst/ g
        record /rayst/ ra
        record /source_results/ sr
        record /integration_results/ xir
        record /control_flags/ ifl
        record /sources/ src
        record /integration/ xi
        record /source1/ so1
        record /source2/ so2
        record /source3/ so3
        record /source4/ so4
        record /source5/ so5
        record /source6/ so6
        record /apertures/ apr
        record /statistics/ st
	record /mirrortype/ am
        record /map4/ m4

	dimension dyi_loc(1001),dzi_loc(1001)

c-----------------------------------------------------------------
	yi_loc=ra.ri.yi
	zi_loc=ra.ri.zi

	ianzy_loc=xi.ianzy0
	ianzz_loc=xi.ianzz0
        dyy=(xi.ymax-xi.ymin)/dflotj(ianzy_loc-1)
        dzz=(xi.zmax-xi.zmin)/dflotj(ianzz_loc-1)

	do ii=1,ianzy_loc
	do jj=1,ianzz_loc

	dyi_loc(ii)=xi.ymin+dflotj(ii-1)*dyy
	dzi_loc(jj)=xi.zmin+dflotj(jj-1)*dzz

	if(abs(xi.distfocy).gt.1.e-12)then
	dyi_loc(ii)=dyi_loc(ii)+yi_loc/xi.distfocy
	endif
	if(abs(xi.distfocy).gt.1.e-12)then
	dzi_loc(jj)=dzi_loc(jj)+zi_loc/xi.distfocy
	endif

        yp=0.d0
	zp=0.d0
        xxlength=0.d0

          var0=1.
        do i=0,iord
          var1=var0
        do j=0,iord-i
          var2=var1
        do k=0,iord-i-j
          var3=var2
        do l=0,iord-i-j-k

         yp=yp+ypc1(i,j,k,l)*var3
         zp=zp+zpc1(i,j,k,l)*var3
         xxlength=xxlength+
     &           (xlen1c(i,j,k,l)+xlen2c(i,j,k,l))*var3
          var3=var3*dzi_loc(jj)
        enddo
          var2=var2*dyi_loc(ii)
        enddo
          var1=var1*zi_loc
        enddo
          var0=var0*yi_loc
        enddo
    
	ra.rf.yp=yp
	ra.rf.zp=zp

	call psdi(g,src,apr,cs,ifl,ra,sr,blp)
 
	enddo
	enddo

	return
	end
c------------------------ end guess -------------------------


