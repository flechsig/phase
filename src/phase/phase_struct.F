c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/phase_struct.F
c$$$ Date      : <27 Oct 10 11:39:08 flechsig> 
c$$$ Time-stamp: <11 Aug 15 13:54:41 flechsig> 
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


c UF revert from 1.39
c !!!! stuct statstics die Feldgrenzen halbiert da sonst
c run time error UF 7.1.2000 */

c------------------------------------------------------------
c 
c	variable structures for PHASE
c
c------------------------------------------------------------

c remember to change phase_struct.h as well
#define MAX_INTEGRATION_SIZE 4096 
#define SMALL_PHA 1.0d-15
#define SMALL 1.0d-300 

c#ifdef QTGUI
c#define MAX_GRIDSIZE 256
c#else
c#define MAX_GRIDSIZE 2048 
c#endif
c UF 17.4.12 GRIDSIZE defined by configure --with-gridsize=255  (default 255)

c--------------- fourth order map ---------------------------


#ifdef SEVEN_ORDER
#define MAPDIM   7
#define MAPDIM1  8
#else
#define MAPDIM   4
#define MAPDIM1  5
#endif

	TYPE map4
	real*8      wc(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              xlc(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              ypc1(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              zpc1(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              dyp1c(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              dzp1c(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              dypc(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              dzpc(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              xlen1cc(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              xlen1c(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              xlen1c_r(0:MAPDIM,0:MAPDIM),
     &              xlen1c_rr(0:MAPDIM),
     &              xlength1,
     &              xlen2cc(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              xlen2c(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              xlen2c_r(0:MAPDIM,0:MAPDIM),
     &              xlen2c_rr(0:MAPDIM),
     &              xlength2,
     &              xlen3c(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              xlen4c(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              xlen5c(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              ypc_ap(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              zpc_ap(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              ypc_ap_r(0:MAPDIM,0:MAPDIM),
     &              zpc_ap_r(0:MAPDIM,0:MAPDIM),
     &              ypc_ap_rr(0:MAPDIM),
     &              zpc_ap_rr(0:MAPDIM),
     &              fdetc(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              fdetphc(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              fdet1phc(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              fdet1phca(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              fdet1phcb(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              fdetrc(0:MAPDIM,0:MAPDIM),
     &              fdetphrc(0:MAPDIM,0:MAPDIM),
     &              fdet1phrc(0:MAPDIM,0:MAPDIM),
     &              fdet1phrca(0:MAPDIM,0:MAPDIM),
     &              fdet1phrcb(0:MAPDIM,0:MAPDIM),
     &              yprc1(0:MAPDIM,0:MAPDIM),
     &              zprc1(0:MAPDIM,0:MAPDIM),
     &              dyprc(0:MAPDIM,0:MAPDIM),
     &              dzprc(0:MAPDIM,0:MAPDIM),
     &              fdtrrc(0:MAPDIM),
     &              fdtphrrc(0:MAPDIM),
     &              fdt1phrrc(0:MAPDIM),
     &              fdt1phrrca(0:MAPDIM),
     &              fdt1phrrcb(0:MAPDIM),
     &              yprrc1(0:MAPDIM),
     &              zprrc1(0:MAPDIM),
     &              dyprrc(0:MAPDIM),
     &              dzprrc(0:MAPDIM),
     &              wrc(0:MAPDIM,0:MAPDIM),
     &              xlrc(0:MAPDIM,0:MAPDIM),
     &              wrrc(0:MAPDIM),
     &              xlrrc(0:MAPDIM),
     &              xmec(1:4,1:4,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &              xmec1(1:4,1:4,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM)
     	end TYPE
    
c--------------- partial derivatives ------------------------
c UF 27.5.11 achtung die dimensionen scheinen mir nicht zu passen- unterschiede zu phase.h
c bsp dfdll
	TYPE parder
           real*8 pl1w2,pl1l2,pl2w2,pl2l2,
     &            pl1w1l1,pl2w1l1,arg1,arg2,
     &   arg1w1,arg1l1,arg1w2,arg1l2,arg1w1l1,
     &   arg2w1,arg2l1,arg2w2,arg2l2,arg2w1l1,
     &   opl6(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &	 dfdw6(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &	 dfdl6(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &	 dfdww6(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),     
     &	 dfdwl6(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),   
     &	 dfdll6(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM), 
     &   dfdwww6(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &   opl(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &	 dfdw(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &	 dfdl(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &	 dfdww(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &	 dfdwl(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &	 dfdll(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM),
     &   dfdwidlj(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM)
        end TYPE  

c spezielle Version fuer 4. order fuer die benutzung in der phaselib
	TYPE parder4
           real*8 pl1w2,pl1l2,pl2w2,pl2l2,
     &            pl1w1l1,pl2w1l1,arg1,arg2,
     &   arg1w1,arg1l1,arg1w2,arg1l2,arg1w1l1,
     &   arg2w1,arg2l1,arg2w2,arg2l2,arg2w1l1,
     &   opl(0:4,0:4,0:4,0:4),
     &	 dfdw(0:4,0:4,0:4,0:4),
     &	 dfdl(0:4,0:4,0:4,0:4),
     &	 dfdww(0:4,0:4,0:4,0:4),
     &	 dfdwl(0:4,0:4,0:4,0:4),
     &	 dfdll(0:4,0:4,0:4,0:4),
     &   dfdwidlj(0:4,0:4,0:4,0:4,0:4,0:4)
        end TYPE  

c--------------- geometry -----------------------------------

        TYPE geometryst
           real*8 sina,cosa,sinb,cosb,
     &            r,rp,xdens(0:4),xlam
	   integer idefl, alignment   ! alignment dummy to avoid warnings
        end TYPE  

        TYPE psimagest               ! Bilddimensionen 
           real*8  disty1,disty2,distz1,distz2                
	   integer iheigh,iwidth   
        end TYPE 

#ifdef OBSOLETE
c---------------- sources -----------------------------------

        TYPE source1                 ! Gauss + Hard edge  
           real*8 sigmay,sigmayp,sigmaz,sigmazp
	   integer isrcy,isrcdy,isrcz,isrcdz
        end TYPE

c	TYPE source1c
c        structure/source1c/                 ! Gauss  
c           real*8 waist, widthyz, dist
c	   integer nyz
c        end structure
c	END TYPE

	TYPE source2
	   character*80 fsource2a,fsource2b
     	   real*8 ceyre(0:60,-60:60),ceyim(0:60,-60:60),
     &		  fscale,small
	   integer iordzern,iflagl
        end TYPE

	TYPE source3
	   character*80 fsource3a,fsource3b
	   real*8 xsrcreal(1024),ysrcreal(1024),
     &	   	  xsrcimag(1024),ysrcimag(1024)
	   integer isrcreal,isrcimag,iactreal,iactimag
        end TYPE


	TYPE source4
	   character*80 fsource4a,fsource4b,fsource4c,fsource4d
	   real*8 xeyremin,xeyremax,dxeyre,
     &		  xeyimmin,xeyimmax,dxeyim,
     &		  yeyremin,yeyremax,dyeyre,
     &		  yeyimmin,yeyimmax,dyeyim,
     &       zeyre(GRIDSIZE,GRIDSIZE),zeyim(GRIDSIZE,GRIDSIZE),
     &		  xezremin,xezremax,dxezre,
     &		  xezimmin,xezimmax,dxezim,
     &		  yezremin,yezremax,dyezre,
     &		  yezimmin,yezimmax,dyezim,
     &       zezre(GRIDSIZE,GRIDSIZE),zezim(GRIDSIZE,GRIDSIZE),
     &       gridx(GRIDSIZE),gridy(GRIDSIZE),deltatime,
     &       ampeyre,ampeyim,ampezre,ampezim
     &      ,xlam
	  integer ieyrex,ieyimx,ieyrey,ieyimy,
     &            iezrex,iezimx,iezrey,iezimy,
     &            nsource,nimage,nfreqtot,nfreqpos,nfreqneg,iconj
	end TYPE
   		
        TYPE source5                 ! Dipol Quelle  
           real*8 dipcy,dipcz,dipdisy,dipdisz,
     &            dipymin,dipymax,dipzmin,dipzmax
	end TYPE

	TYPE source6
	   character*80 fsource6
           real*8 br(16,16,16,16),
     &		  brxmin,brxmax,brdx,
     &  	  brymin,brymax,brdy,
     &  	  brpxmin,brpxmax,brdpx,
     &  	  brpymin,brpymax,brdpy,
     &       abr(16,16,16,16),
     &  	  abrxmin,abrxmax,abrdx,
     &  	  abrymin,abrymax,abrdy,
     &  	  abrpxmin,abrpxmax,abrdpx,
     &  	  abrpymin,abrpymax,abrdpy
	   integer ibrpy,ibrpx,ibry,ibrx,
     &		   iabrpy,iabrpx,iabry,iabrx
	end TYPE

c        structure/sources/                 ! Sammelstruktur
c        real*8 pin_yl0,pin_yl,pin_zl0,pin_zl ! changed position 17.12.2005
c           record/source1/so1              ! Gauss + Hard edge
c           record/source2/so2              ! Zernike
c           record/source3/so3              ! radiale Vert. vom File
c           record/source4/so4              ! Quelle von File
c	   record/source5/so5	           ! Dipol Quelle
c           record/source6/so6              ! Brightness
c
c	   integer isrctype
c        end structure   
#endif
c---------------- integration --------------------------------

	TYPE integration
		real*8 distfocy,distfocz
		integer ianzy0
		integer ianzz0
		real*8  ymin, ymax
		real*8  zmin, zmax
		real*8  phase_change_1,phase_change_2,d12_max
		real*8  amp_change
		integer iamp_smooth,iord_amp,iord_pha
		integer iphase_curv,iphase_pi2,ifm_amp,ifm_pha
		integer id12,ianz0_cal,ianz0_fixed
	end TYPE

c---------------- apertures ----------------------------------

	TYPE apertures
		real*8 srcymin,srcymax,srczmin,srczmax,rpin
		real*8 ymin_ap,ymax_ap,zmin_ap,zmax_ap,rpin_ap
		real*8 w_min,w_max,xl_min,xl_max
	end TYPE

c---------------- rays  --------------------------------------

	TYPE ray_i
		real*8 yi,zi,dyi,dzi
	end TYPE
	
	TYPE ray_f
		real*8 yp,zp,dyp,dzp
	end TYPE

	TYPE opt_el
		real*8 w,xl
	end TYPE

	TYPE aperture
		real*8 yp_ap,zp_ap
	end TYPE

	TYPE rayst
		TYPE(ray_i) ri
		TYPE(ray_f) rf
		TYPE(opt_el) oe
		TYPE(aperture) ap
		real*8 xlength1,xlength2
                real*8 fd(4096),fdph(4096),fd1ph(256,256)
		real*8 fd1(4096),fd2(4096)
		real*8 fdph1(4096),fdph2(4096)
		integer n1,n2,n3,n4
	end TYPE

c------------------ path length coefficients (notwendig ???) ---------

        TYPE xlenmap                 ! Entw. Koeffizienten
	   real*8 xlen1c(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM), ! der Pfadlaenge
     &  	  xlen2c(0:MAPDIM,0:MAPDIM,0:MAPDIM,0:MAPDIM)
       	end TYPE 

c------------------ mirror --------------------------------------
c UF 27.5.11 hier in fortran ist das zwar formal OK aber ich mache besser eine structur draus
c wir sollten hier nur typen definieren nicht die variablen selbst 
c	   real*8 a(0:MAPDIM1,0:MAPDIM1)
	TYPE mirrortype	! Entw. Koeffizienten
	   real*8 a(0:MAPDIM1,0:MAPDIM1)
       	end TYPE 
	
c------------------ results from sources ------------------------

        TYPE source_results                 
           complex*16 densy,densz
           real*8 dens,eya,eza,eyp,ezp,
     &       densyre,densyim,denszre,denszim,
     &       xintyre,xintyim,xintzre,xintzim
        end TYPE
c----------------- U. F. 21.12.99 -------------------------------
c common simps  
       TYPE simps1
c       real*8 fya1(MAX_INTEGRATION_SIZE),fyp1(MAX_INTEGRATION_SIZE),fza1(MAX_INTEGRATION_SIZE),fzp1(MAX_INTEGRATION_SIZE)
c       real*8 fya2(MAX_INTEGRATION_SIZE),fyp2(MAX_INTEGRATION_SIZE),fza2(MAX_INTEGRATION_SIZE),fzp2(MAX_INTEGRATION_SIZE)
c       real*8 z1,z2
c           real*8 tya(256,256),tza(256,256)
c           real*8 typ(256,256),tzp(256,256)
c           integer ianz0_save(301,301)
           integer iiheigh,iiwidth
c	   integer jmult
        end TYPE
c------------------ results from integration --------------------
	TYPE integration_results
c	TYPE integration_results/
	   complex*16 yzintey,yzintez         ! results
c Apr 15	   real*8 yzintya,yzintyp,yzintza,yzintzp
	   integer isimp(100),iisimp,nsimp
c           real*8 sintre(4,2,MAX_INTEGRATION_SIZE),sintim(4,2,MAX_INTEGRATION_SIZE)
c  Apr 15         real*8 simpa(4,2,MAX_INTEGRATION_SIZE),simpp(4,2,MAX_INTEGRATION_SIZE)
c           real*8 simpre(4,2,MAX_INTEGRATION_SIZE),simpim(4,2,MAX_INTEGRATION_SIZE)
	   integer isintre(4),isintim(4)
c       ,isimpa(4),isimpp(4)
	   integer isimpre(4),isimpim(4)
c           real*8 d12(2,3,MAX_INTEGRATION_SIZE)
c           real*8 ianzd12(3)
c	   record/simps1/si1                  ! UF 21.12.99
           integer fill_results, alignment    ! alignment dummy to avoid warnings
c	end structure
	end TYPE
c------------------ flags ---------------------------------------
	TYPE control_flags
c        structure/control_flags/
	   integer iord,iordsc,iexpand,iplmode,ibright,ispline
	   integer inorm,inorm1,inorm2
	   integer matrel
	   integer igrating,ipinarr,ilimits
	   integer ipath
           integer pst_mode
c        end structure
	end TYPE
c------------------ statistics ----------------------------------
c /* 23.12.99 diese Structur ist zu gross - run time error auf PC   */
c so wars bei Johannes - verkleinere felder

c	structure/statistics/
c	   real*8 fd1phmax(512,512)
c	   integer nn1,nn2,inumb(1100,1100)
c	   integer inumzit,inumyit,inumzan,inumyan
c	end structure

c	structure/statistics/
c        real*8 fd1phmax(128,128)
c        integer nn1,nn2,inumb(256,256)
c	   integer inumzit,inumyit,inumzan,inumyan
c	end structure

c------------------ constants -----------------------------------

        TYPE constants
	   real*8 pi,gam(0:165),fs(0:165),fc(0:165)
	   complex*16 sqrtm1
        end TYPE


c end phase_struct.F
