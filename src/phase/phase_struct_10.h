/*  File      : /home/vms/flechsig/vms/phas/phasec/phase_struct_10.h */
/*  Date      : <22 Oct 97 15:02:57 flechsig>  */
/*  Time-stamp: <23 Dec 99 15:59:02 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@exp.bessy.de */

/* neu eingefuegt */
typedef struct ComplexStruct {
	double re,im; 
	} COMPLEX ;

/* strings sind kritisch */
/* chraracter[80] durch FStrings ersetzen !! in FString wird 
   nur ein pointer gemerkt !! 
/* modification: 16 Oct 97 15:27:15 flechsig */
/* character[80] ist doch richtig 
 */ 

/*-----------------------------------------------------------*/
/*                                                           */
/*	variable structures for PHASE                        */
/*                                                           */
/*-----------------------------------------------------------*/

/*-------------- fourth order map ---------------------------*/

	struct  map4 {
	double      wc[5][5][5][5],
                    xlc[5][5][5][5],
                    ypc1[5][5][5][5],
                    zpc1[5][5][5][5],
                    dyp1c[5][5][5][5],
                    dzp1c[5][5][5][5],
                    dypc[5][5][5][5],
                    dzpc[5][5][5][5],
                    xlen1cc[5][5][5][5],
                    xlen1c[5][5][5][5],
                    xlen1c_r[5][5],
                    xlen1c_rr[5],xlength1,
                    xlen2cc[5][5][5][5],
                    xlen2c[5][5][5][5],
                    xlen2c_r[5][5],
                    xlen2c_rr[5],xlength2,
	            xlen3c[5][5][5][5],
                    xlen4c[5][5][5][5],
                    xlen5c[5][5][5][5],
                    ypc_ap[5][5][5][5],zpc_ap[5][5][5][5],
                    ypc_ap_r[5][5],zpc_ap_r[5][5],
                    ypc_ap_rr[5],zpc_ap_rr[5],
                    fdetc[5][5][5][5],
                    fdetphc[5][5][5][5],
                    fdet1phc[5][5][5][5],
                    fdetrc[5][5],
                    fdetphrc[5][5],fdet1phrc[5][5],
                    yprc1[5][5],zprc1[5][5],
                    dyprc[5][5],dzprc[5][5],
                    fdtrrc[5],
                    fdtphrrc[5],fdt1phrrc[5],
                    yprrc1[5],zprrc1[5],
                    dyprrc[5],dzprrc[5],
                    wrc[5][5],xlrc[5][5],
                    wrrc[5],xlrrc[5],
                    xmec[4][4][5][5][5][5],
                    xmec1[4][4][5][5][5][5];
     	};  
    
/* -------------- partial derivatives ------------------------*/

	struct  parder {
           double pl1w2, pl1l2, pl2w2, pl2l2,
                  pl1w1l1, pl2w1l1, arg1, arg2,
         	  arg1w1, arg1l1, arg1w2, arg1l2, arg1w1l1,
         	  arg2w1, arg2l1, arg2w2, arg2l2, arg2w1l1;
        };    

/* -------------- geometry -----------------------------------*/

        struct  geometryst {
           double sina,cosa,sinb,cosb,
                  r,rp,xdens[5],xlam;
	   int  idefl;   
        };    

        struct  psimagest {               /* Bilddimensionen */
           double  disty1,disty2,distz1,distz2;                
	   int  iheigh,iwidth;   
        };   

/* --------------- sources -----------------------------------*/

        struct  source1 {                 /* Gauss + Hard edge */ 
           double sigmay,sigmayp,sigmaz,sigmazp;
	   int  isrcy,isrcdy,isrcz,isrcdz;
        };  
 
	struct  source2 {
	  char fsource2a[80], fsource2b[80];
     	   double ceyre[61][121], ceyim[61][121],
      		  fscale,small;
	   int  iordzern,iflagl;
        };  

	struct  source3 {
	  char fsource3a[80],fsource3b[80];
	   double xsrcreal[1024],ysrcreal[1024],
      	   	  xsrcimag[1024],ysrcimag[1024];
	   int  isrcreal,isrcimag,iactreal,iactimag;
        };  

struct  source4 {
  char fsource4a[80], fsource4b[80], fsource4c[80],fsource4d[80];
  double xeyremin,xeyremax,dxeyre,
	xeyimmin,xeyimmax,dxeyim,
	yeyremin,yeyremax,dyeyre,
	yeyimmin,yeyimmax,dyeyim,
	zeyre[501][501],zeyim[501][501],
	xezremin,xezremax,dxezre,
	xezimmin,xezimmax,dxezim,
	yezremin,yezremax,dyezre,
	yezimmin,yezimmax,dyezim,
	zezre[501][501],zezim[501][501];
  int  ieyrex,ieyimx,ieyrey,ieyimy,
    iezrex,iezimx,iezrey,iezimy;
};  
   		
        struct  source5 {                 /* Dipol Quelle  */
           double dipcy,   dipcz,   dipdisy, dipdisz,
                  dipymin, dipymax, dipzmin, dipzmax;
	};  

	struct  source6 {
	   char fsource6[80];
	   double br[29][29][29][29],
      		  brxmin,brxmax,brdx,
        	  brymin,brymax,brdy,
        	  brpxmin,brpxmax,brdpx,
        	  brpymin,brpymax,brdpy,
      		  abr[29][29][29][29],
        	  abrxmin,abrxmax,abrdx,
        	  abrymin,abrymax,abrdy,
        	  abrpxmin,abrpxmax,abrdpx,
        	  abrpymin,abrpymax,abrdpy;
	   int  ibrpy,ibrpx,ibry,ibrx,
      		   iabrpy,iabrpx,iabry,iabrx;
	};  

        struct  sources {                   /* Sammelstruktur         */
           struct source1 so1;              /* Gauss + Hard edge      */
           struct source2 so2;              /* Zernike                */
           struct source3 so3;              /* radiale Vert. vom File */
           struct source4 so4;              /* Quelle von File        */
           struct source5 so5;              /* Dipol Quelle           */
           struct source6 so6;              /* Brightness             */
	   int  isrctype;
	   double pin_yl0,pin_yl,pin_zl0,pin_zl;
        };     

/* --------------- integration --------------------------------*/

	struct  integration {
		double distfoc;
		int  ianzy0;
		int  ianzz0;
		double ymin,ymax;
		double zmin,zmax;
		double phase_change_1,phase_change_2,d12_max;
		double amp_change;
		int  iamp_smooth,iord_amp,iord_pha,iphase_curv;
		int  iphase_pi2,ifm_amp,ifm_pha,id12,ianz0_cal,ianz0_fixed;
	};  

/* --------------- apertures ----------------------------------*/

	struct  apertures {
		double srcymin,srcymax,srczmin,srczmax,rpin;
		double ymin_ap,ymax_ap,zmin_ap,zmax_ap,rpin_ap;
		double w_min,w_max,xl_min,xl_max;
	};  

/* --------------- rays  --------------------------------------*/

	struct  ray_i {
		double yi,zi,dyi,dzi;
	};  
	
	struct  ray_f {
		double yp,zp,dyp,dzp;
	};  

	struct  opt_el {
		double w,xl;
	};  

	struct  aperture {
		double yp_ap,zp_ap;
	};  

	struct  rayst {
		struct ray_i ri;
		struct ray_f rf;
		struct opt_el oe;
		struct aperture ap;
		double xlength1,xlength2;
		double xlam_test;
		double fd[4096],fdph[4096],fd1ph[501][501];
		double fd1[4096],fd2[4096];
		double fdph1[4096],fdph2[4096];
		int  n1,n2,n3,n4;
	};  

/* ----------------- path length coefficients -------------------- */

        struct  xlenmap {                  /* Entw. Koeffizienten */
	   double xlen1c[5][5][5][5],  /* der Pfadlaenge     */
        	  xlen2c[5][5][5][5];
       	};   

/* ----------------- results from sources ------------------------*/

        struct  source_results {                 
           COMPLEX densy,densz;
	   double dens,eya,eza,eyp,ezp;
        };  
/* ----------------- U. F. 21.12.99 ----------------------------- */
struct simps1 {
  double fya1[501],fyp1[501],fza1[501],fzp1[501];
  double fya2[501],fyp2[501],fza2[501],fzp2[501];
  double z1,z2;
  double tya[301][301],tza[301][301];
  double typ[301][301],tzp[301][301];
  int ianz0_save[301][301];
  int iiheigh,iiwidth,jmult;
};
/* ----------------- results from integration --------------------*/

	struct  integration_results {
	   COMPLEX yzintey,yzintez;
	   double yzintya,yzintyp,yzintza,yzintzp;
	   int  isimp[100],iisimp,nsimp;
	   double sintre[4][2][4096],sintim[4][2][4096];
	   double simpa[4][2][4096],simpp[4][2][4096];
	   double simpre[4][2][4096],simpim[4][2][4096];
	   int  isintre[4],isintim[4],isimpa[4],isimpp[4];
	   int  isimpre[4],isimpim[4];
	   double d12[2][3][4096],ianzd12[3];
	   struct simps1 si1;            /* UF 21.12.99 */
	};  

/* ----------------- flags ---------------------------------------*/

        struct  control_flags {
	   int  iord,iordsc,iexpand,iplmode,ibright,ispline;
	   int  inorm,inorm1,inorm2;
	   int  matrel;
	   int  igrating,ipinarr,ilimits;
	   int  ipath; 
        };  

/* ----------------- statistics ----------------------------------*/
/* 23.12.99 diese Structur ist zu gross - run time error auf PC   */
#ifdef VMS
	struct  statistics {
	   double fd1phmax[512][512];
	   int  nn1,nn2,inumb[1100][1100];
	   int  inumzit,inumyit,inumzan,inumyan;
	};  
#else
        struct  statistics {
	   double fd1phmax[128][128];
	   int  nn1,nn2,inumb[128][128];
	   int  inumzit,inumyit,inumzan,inumyan;
	};  
#endif
/* ----------------- constants -----------------------------------*/

        struct  constants {
	   double pi;
	   COMPLEX sqrtm1;
        };  

/* end phase_struct_10.h */
