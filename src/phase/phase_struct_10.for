c File      : /home/vms/flechsig/vms/phas/phasec/phase_struct_10.for
c Date      : <22 Oct 97 14:56:27 flechsig> 
c Time-stamp: <20 Dec 99 10:00:18 flechsig> 
c Author    : Uwe Flechsig, flechsig@exp.bessy.de
c------------------------------------------------------------
c
c	variable structures for PHASE
c
c------------------------------------------------------------

c--------------- fourth order map ---------------------------

	structure/map4/
	real*8      wc(0:4,0:4,0:4,0:4),
     &              xlc(0:4,0:4,0:4,0:4),
     &              ypc1(0:4,0:4,0:4,0:4),
     &              zpc1(0:4,0:4,0:4,0:4),
     &              dyp1c(0:4,0:4,0:4,0:4),
     &              dzp1c(0:4,0:4,0:4,0:4),
     &              dypc(0:4,0:4,0:4,0:4),
     &              dzpc(0:4,0:4,0:4,0:4),
     &              xlen1cc(0:4,0:4,0:4,0:4),
     &              xlen1c(0:4,0:4,0:4,0:4),
     &              xlen1c_r(0:4,0:4),
     &              xlen1c_rr(0:4),xlength1,
     &              xlen2cc(0:4,0:4,0:4,0:4),
     &              xlen2c(0:4,0:4,0:4,0:4),
     &              xlen2c_r(0:4,0:4),
     &              xlen2c_rr(0:4),xlength2,
     &              xlen3c(0:4,0:4,0:4,0:4),
     &              xlen4c(0:4,0:4,0:4,0:4),
     &              xlen5c(0:4,0:4,0:4,0:4),
     &              ypc_ap(0:4,0:4,0:4,0:4),zpc_ap(0:4,0:4,0:4,0:4),
     &              ypc_ap_r(0:4,0:4),zpc_ap_r(0:4,0:4),
     &              ypc_ap_rr(0:4),zpc_ap_rr(0:4),
     &              fdetc(0:4,0:4,0:4,0:4),
     &              fdetphc(0:4,0:4,0:4,0:4),
     &              fdet1phc(0:4,0:4,0:4,0:4),
     &              fdetrc(0:4,0:4),
     &              fdetphrc(0:4,0:4),fdet1phrc(0:4,0:4),
     &              yprc1(0:4,0:4),zprc1(0:4,0:4),
     &              dyprc(0:4,0:4),dzprc(0:4,0:4),
     &              fdtrrc(0:4),
     &              fdtphrrc(0:4),fdt1phrrc(0:4),
     &              yprrc1(0:4),zprrc1(0:4),
     &              dyprrc(0:4),dzprrc(0:4),
     &              wrc(0:4,0:4),xlrc(0:4,0:4),
     &              wrrc(0:4),xlrrc(0:4),
     &              xmec(1:4,1:4,0:4,0:4,0:4,0:4),
     &              xmec1(1:4,1:4,0:4,0:4,0:4,0:4)
     	end structure
    
c--------------- partial derivatives ------------------------

	structure/parder/
           real*8 pl1w2,pl1l2,pl2w2,pl2l2,
     &            pl1w1l1,pl2w1l1,arg1,arg2,
     &   arg1w1,arg1l1,arg1w2,arg1l2,arg1w1l1,
     &   arg2w1,arg2l1,arg2w2,arg2l2,arg2w1l1
        end structure  

c--------------- geometry -----------------------------------

        structure/geometryst/
           real*8 sina,cosa,sinb,cosb,
     &            r,rp,xdens(0:4),xlam
	   integer idefl   
        end structure  

        structure/psimagest/               ! Bilddimensionen 
           real*8  disty1,disty2,distz1,distz2                
	   integer iheigh,iwidth   
        end structure 

c---------------- sources -----------------------------------

        structure/source1/                 ! Gauss + Hard edge  
           real*8 sigmay,sigmayp,sigmaz,sigmazp
	   integer isrcy,isrcdy,isrcz,isrcdz
        end structure
 
	structure/source2/
	   character*80 fsource2a,fsource2b
     	   real*8 ceyre(0:60,-60:60),ceyim(0:60,-60:60),
     &		  fscale,small
	   integer iordzern,iflagl
        end structure

	structure/source3/
	   character*80 fsource3a,fsource3b
	   real*8 xsrcreal(1024),ysrcreal(1024),
     &	   	  xsrcimag(1024),ysrcimag(1024)
	   integer isrcreal,isrcimag,iactreal,iactimag
        end structure

	structure/source4/
	   character*80 fsource4a,fsource4b,fsource4c,fsource4d
	   real*8 xeyremin,xeyremax,dxeyre,
     &		  xeyimmin,xeyimmax,dxeyim,
     &		  yeyremin,yeyremax,dyeyre,
     &		  yeyimmin,yeyimmax,dyeyim,
     &		  zeyre(501,501),zeyim(501,501),
     &		  xezremin,xezremax,dxezre,
     &		  xezimmin,xezimmax,dxezim,
     &		  yezremin,yezremax,dyezre,
     &		  yezimmin,yezimmax,dyezim,
     &		  zezre(501,501),zezim(501,501)
	  integer ieyrex,ieyimx,ieyrey,ieyimy,
     &		  iezrex,iezimx,iezrey,iezimy
	end structure
   		
        structure/source5/                 ! Dipol Quelle  
           real*8 dipcy,dipcz,dipdisy,dipdisz,
     &            dipymin,dipymax,dipzmin,dipzmax
	end structure

	structure/source6/
	   character*80 fsource6
	   real*8 br(29,29,29,29),
     &		  brxmin,brxmax,brdx,
     &  	  brymin,brymax,brdy,
     &  	  brpxmin,brpxmax,brdpx,
     &  	  brpymin,brpymax,brdpy,
     &		  abr(29,29,29,29),
     &  	  abrxmin,abrxmax,abrdx,
     &  	  abrymin,abrymax,abrdy,
     &  	  abrpxmin,abrpxmax,abrdpx,
     &  	  abrpymin,abrpymax,abrdpy
	   integer ibrpy,ibrpx,ibry,ibrx,
     &		   iabrpy,iabrpx,iabry,iabrx
	end structure

        structure/sources/                 ! Sammelstruktur
           record/source1/so1              ! Gauss + Hard edge
           record/source2/so2              ! Zernike
           record/source3/so3              ! radiale Vert. vom File
           record/source4/so4              ! Quelle von File
           record/source5/so5              ! Dipol Quelle
           record/source6/so6              ! Brightness
	   integer isrctype
	   real*8 pin_yl0,pin_yl,pin_zl0,pin_zl
        end structure   

c---------------- integration --------------------------------

	structure/integration/
		real*8 distfoc
		integer ianzy0
		integer ianzz0
		real*8 ymin,ymax
		real*8 zmin,zmax
		real*8 phase_change_1,phase_change_2,d12_max
		real*8 amp_change
		integer iamp_smooth,iord_amp,iord_pha
		integer iphase_curv,iphase_pi2,ifm_amp,ifm_pha
		integer id12,ianz0_cal,ianz0_fixed
	end structure

c---------------- apertures ----------------------------------

	structure/apertures/
		real*8 srcymin,srcymax,srczmin,srczmax,rpin
		real*8 ymin_ap,ymax_ap,zmin_ap,zmax_ap,rpin_ap
		real*8 w_min,w_max,xl_min,xl_max
	end structure

c---------------- rays  --------------------------------------

	structure/ray_i/
		real*8 yi,zi,dyi,dzi
	end structure
	
	structure/ray_f/
		real*8 yp,zp,dyp,dzp
	end structure

	structure/opt_el/
		real*8 w,xl
	end structure

	structure/aperture/
		real*8 yp_ap,zp_ap
	end structure

	structure/rayst/
		record/ray_i/ri
		record/ray_f/rf
		record/opt_el/oe
		record/aperture/ap
		real*8 xlength1,xlength2
		real*8 xlam_test
		real*8 fd(4096),fdph(4096),fd1ph(501,501)
		real*8 fd1(4096),fd2(4096)
		real*8 fdph1(4096),fdph2(4096)
		integer n1,n2,n3,n4
	end structure

c------------------ path length coefficients (notwendig ???) ---------

        structure/xlenmap/                 ! Entw. Koeffizienten
	   real*8 xlen1c(0:4,0:4,0:4,0:4), ! der Pfadlaenge
     &  	  xlen2c(0:4,0:4,0:4,0:4)
       	end structure 

c------------------ results from sources ------------------------

        structure/source_results/                 
           complex*16 densy,densz
	   real*8 dens,eya,eza,eyp,ezp
        end structure

c------------------ results from integration --------------------

	structure/integration_results/
	   complex*16 yzintey,yzintez
	   real*8 yzintya,yzintyp,yzintza,yzintzp
	   integer isimp(100),iisimp,nsimp
	   real*8 sintre(4,2,4096),sintim(4,2,4096)
	   real*8 simpa(4,2,4096),simpp(4,2,4096)
	   real*8 simpre(4,2,4096),simpim(4,2,4096)
	   integer isintre(4),isintim(4),isimpa(4),isimpp(4)
	   integer isimpre(4),isimpim(4)
	   real*8 d12(2,3,4096),ianzd12(3)
	   record/simps1/si1                  ! UF 21.12.99
	end structure

c------------------ flags ---------------------------------------

        structure/control_flags/
	   integer iord,iordsc,iexpand,iplmode,ibright,ispline
	   integer inorm,inorm1,inorm2
	   integer matrel
	   integer igrating,ipinarr,ilimits
	   integer ipath
        end structure

c------------------ statistics ----------------------------------

	structure/statistics/
	   real*8 fd1phmax(512,512)
	   integer nn1,nn2,inumb(1100,1100)
	   integer inumzit,inumyit,inumzan,inumyan
	end structure

c------------------ constants -----------------------------------

        structure/constants/
	   real*8 pi
	   complex*16 sqrtm1
        end structure
c----------------- U. F. 21.12.99 -------------------------------
c common simps 	
	structure/simps1/
	   real*8 fya1(501),fyp1(501),fza1(501),fzp1(501)
	   real*8 fya2(501),fyp2(501),fza2(501),fzp2(501)
	   real*8 z1,z2
     	   real*8 tya(301,301),tza(301,301)
     	   real*8 typ(301,301),tzp(301,301)
	   integer ianz0_save(301,301)
     	   integer iiheigh,iiwidth,jmult
        end structure
c end phase_struct_10.for
