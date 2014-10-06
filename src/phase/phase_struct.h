/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/phase_struct_10.h */
/*   Date      : <31 Oct 03 12:31:32 flechsig>  */
/*   Time-stamp: <06 Oct 14 08:42:25 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */

// ******************************************************************************
//
//   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
//                      Paul Scherrer Institut Villigen, Switzerland
//   
//   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
//          Uwe Flechsig,    uwe.flechsig@psi.ch
//
// ------------------------------------------------------------------------------
//
//   This file is part of PHASE.
//
//   PHASE is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, version 3 of the License, or
//   (at your option) any later version.
//
//   PHASE is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
//
// ******************************************************************************


/* revert from 1.41 */
#ifndef __PHASE_STRUCT
#define __PHASE_STRUCT

// maximum gridsize for simpson's integration (phase_integration.F)
#define MAX_INTEGRATION_SIZE 4096 //remember to change phase_struct.F as well
#define SMALL_PHA 1.0d-15
/* #ifdef QTGUI
#define MAX_GRIDSIZE 256
#else
#define MAX_GRIDSIZE 2048 
#endif*/
/* UF 17.4.12 GRIDSIZE defined by configure --with-gridsize=255  (default 255) */


/* we should put a similar section in each header file which has to be upgraded in order 
   to be independent from the calling sequence */
#ifdef SEVEN_ORDER
/* number to be checked */
  #define MAPDIM   8
  #define MAPDIM1  9
#else
  #define MAPDIM   5
  #define MAPDIM1  6
#endif


/* neu eingefuegt */
/*typedef struct ComplexStruct {
	double re,im; 
	} COMPLEX ; 
geht nach cutils.h
*/

/* !!!! stuct statstics die Feldgrenzen verkleinert
   run time error UF 7.1.2000 */

/*-----------------------------------------------------------*/
/*                                                           */
/*	variable structures for PHASE                        */
/*                                                           */
/*-----------------------------------------------------------*/

/*-------------- fourth order map ---------------------------*/

struct map4 {
  double wc[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    xlc[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    ypc1[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    zpc1[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dyp1c[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dzp1c[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dypc[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dzpc[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    xlen1cc[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    xlen1c[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    xlen1c_r[MAPDIM][MAPDIM],
    xlen1c_rr[MAPDIM],
    xlength1,
    xlen2cc[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    xlen2c[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    xlen2c_r[MAPDIM][MAPDIM],
    xlen2c_rr[MAPDIM],
    xlength2,
    xlen3c[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    xlen4c[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    xlen5c[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    ypc_ap[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    zpc_ap[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    ypc_ap_r[MAPDIM][MAPDIM],
    zpc_ap_r[MAPDIM][MAPDIM],
    ypc_ap_rr[MAPDIM],
    zpc_ap_rr[MAPDIM],
    fdetc[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    fdetphc[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    fdet1phc[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    fdet1phca[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    fdet1phcb[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    fdetrc[MAPDIM][MAPDIM],
    fdetphrc[MAPDIM][MAPDIM],
    fdet1phrc[MAPDIM][MAPDIM],
    fdet1phrca[MAPDIM][MAPDIM],
    fdet1phrcb[MAPDIM][MAPDIM],
    yprc1[MAPDIM][MAPDIM],
    zprc1[MAPDIM][MAPDIM],
    dyprc[MAPDIM][MAPDIM],
    dzprc[MAPDIM][MAPDIM],
    fdtrrc[MAPDIM],
    fdtphrrc[MAPDIM],
    fdt1phrrc[MAPDIM],
    fdt1phrrca[MAPDIM],
    fdt1phrrcb[MAPDIM],
    yprrc1[MAPDIM],
    zprrc1[MAPDIM],
    dyprrc[MAPDIM],
    dzprrc[MAPDIM],
    wrc[MAPDIM][MAPDIM],
    xlrc[MAPDIM][MAPDIM],
    wrrc[MAPDIM],
    xlrrc[MAPDIM],
  /* UF ist die 4 richtig */
    xmec[4][4][MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    xmec1[4][4][MAPDIM][MAPDIM][MAPDIM][MAPDIM];
};  
    
/* -------------- partial derivatives ------------------------*/

struct  parder {
  double pl1w2, pl1l2, pl2w2, pl2l2,
    pl1w1l1, pl2w1l1, arg1, arg2,
    arg1w1, arg1l1, arg1w2, arg1l2, arg1w1l1,
    arg2w1, arg2l1, arg2w2, arg2l2, arg2w1l1,
    opl6[MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dfdw6[MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dfdl6[MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dfdww6[MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dfdwl6[MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dfdll6[MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dfdwww6[MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM],		  
    opl[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dfdw[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dfdl[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dfdww[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dfdwl[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dfdll[MAPDIM][MAPDIM][MAPDIM][MAPDIM],
    dfdwidlj[MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM][MAPDIM];
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

struct  source1c {                 /* Gauss  */ 
  double waist, widthyz, dist;
  int    nyz;
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

struct source4 {
  char fsource4a[80], fsource4b[80], fsource4c[80],fsource4d[80];
  double xeyremin,xeyremax,dxeyre,
    xeyimmin,xeyimmax,dxeyim,
    yeyremin,yeyremax,dyeyre,
    yeyimmin,yeyimmax,dyeyim,
    zeyre[GRIDSIZE][GRIDSIZE],zeyim[GRIDSIZE][GRIDSIZE],
    xezremin,xezremax,dxezre,
    xezimmin,xezimmax,dxezim,
    yezremin,yezremax,dyezre,
    yezimmin,yezimmax,dyezim,
    zezre[GRIDSIZE][GRIDSIZE],zezim[GRIDSIZE][GRIDSIZE],
    gridx[GRIDSIZE],gridy[GRIDSIZE],deltatime,
    ampeyre,ampeyim,ampezre,ampezim
    ,xlam
    ;
  int  ieyrex,ieyimx,ieyrey,ieyimy,
    iezrex,iezimx,iezrey,iezimy,
       nsource,nimage,nfreqtot,nfreqpos,nfreqneg,iconj;
};  

/* 1204 the c version with dynamic memory */
/* this struct uses the c memory model    */
struct source4c {
  double xemin, xemax, dx, yemin, yemax, dy, 
    *zeyre, *zeyim,
    *zezre, *zezim,
    *gridx, *gridy, deltatime,
    ampeyre, ampeyim, ampezre, ampezim,
    xlam;
  int iex, iey,
    nsource, nimage, nfreqtot, nfreqpos, nfreqneg, iconj;
};  

struct  source5 {                 /* Dipol Quelle  */
  double dipcy,   dipcz,   dipdisy, dipdisz,
    dipymin, dipymax, dipzmin, dipzmax;
};  

struct  source6 {
  char fsource6[80];
  double br[16][16][16][16],
    brxmin,brxmax,brdx,
    brymin,brymax,brdy,
    brpxmin,brpxmax,brdpx,
    brpymin,brpymax,brdpy,
    abr[16][16][16][16],
    abrxmin,abrxmax,abrdx,
    abrymin,abrymax,abrdy,
    abrpxmin,abrpxmax,abrdpx,
    abrpymin,abrpymax,abrdpy;
  int  ibrpy,ibrpx,ibry,ibrx,
    iabrpy,iabrpx,iabry,iabrx;
};  

struct  sources {                            /* Sammelstruktur         */
  //  double pin_yl0, pin_yl, pin_zl0, pin_zl;   /* changed position 17.12.2005 */
  
  /* struct source1 so1;     */                   /* Gauss + Hard edge      */
  /* struct source2 so2;     */                /* Zernike                */
  /* struct source3 so3;     */                /* radiale Vert. vom File */
#ifdef OLD_PO_SOURCE
   struct source4 so4;                        /* Quelle von File        */
#endif
  /* struct source5 so5;     */                   /* Dipol Quelle           */
  /* struct source6 so6;     */                   /* Brightness             */

  int  isrctype;
};     

/* --------------- integration --------------------------------*/

struct  integration {
  double distfocy,distfocz;
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
  double fd[4096],fdph[4096],fd1ph[256][256];
  double fd1[4096],fd2[4096];
  double fdph1[4096],fdph2[4096];
  int  n1,n2,n3,n4;
};  

/* ----------------- path length coefficients -------------------- */

struct  xlenmap {                  /* Entw. Koeffizienten */
#ifdef SEVEN_ORDER
	/* UF ist das  richtig? */
#endif
  
  double xlen1c[MAPDIM][MAPDIM][MAPDIM][MAPDIM],  /* der Pfadlaenge     */
    xlen2c[MAPDIM][MAPDIM][MAPDIM][MAPDIM];
};   

/* ----------------- mirror --------------------------------------*/
/* UF 27.5.2011 so macht man eine globale variable- ist nicht gut */
/* double a[MAPDIM1][MAPDIM1];                                    */
/* ausserdem gibt es das schon in phase.h struct mirrortype */


/* ----------------- results from sources ------------------------*/

struct  source_results {                 
  COMPLEX densy,densz;
  double dens,eya,eza,eyp,ezp,
    densyre,densyim,denszre,denszim,
    xintyre,xintyim,xintzre,xintzim;
};  
/* ----------------- U. F. 21.12.99 ----------------------------- */
// this struct seems to be obsolete and probably can be removed
struct simps1 {
  double fya1[MAX_INTEGRATION_SIZE],fyp1[MAX_INTEGRATION_SIZE],fza1[MAX_INTEGRATION_SIZE],fzp1[MAX_INTEGRATION_SIZE];
  double fya2[MAX_INTEGRATION_SIZE],fyp2[MAX_INTEGRATION_SIZE],fza2[MAX_INTEGRATION_SIZE],fzp2[MAX_INTEGRATION_SIZE];
  double z1,z2;
  //  double tya[301][301],tza[301][301];
  //  double typ[301][301],tzp[301][301];
  int ianz0_save[301][301];
  int iiheigh,iiwidth,jmult;
};
/* ----------------- results from integration --------------------*/

struct  integration_results {
  COMPLEX yzintey,yzintez;
  double yzintya,yzintyp,yzintza,yzintzp;
  int  isimp[100],iisimp,nsimp;
  double sintre[4][2][MAX_INTEGRATION_SIZE],sintim[4][2][MAX_INTEGRATION_SIZE];
  double simpa[4][2][MAX_INTEGRATION_SIZE],simpp[4][2][MAX_INTEGRATION_SIZE];
  double simpre[4][2][MAX_INTEGRATION_SIZE],simpim[4][2][MAX_INTEGRATION_SIZE];
  int  isintre[4],isintim[4],isimpa[4],isimpp[4];
  int  isimpre[4],isimpim[4];
  double d12[2][3][MAX_INTEGRATION_SIZE],ianzd12[3]; //SG: is MAX_INTEGRATION_SIZE correct here?
  struct simps1 si1;            /* UF 21.12.99 */
};  

/* ----------------- flags ---------------------------------------*/

struct  control_flags {
  int  iord,iordsc,iexpand,iplmode,ibright,ispline;
  int  inorm,inorm1,inorm2;
  int  matrel;
  int  igrating,ipinarr,ilimits;
  int  ipath; 
  int  pst_mode;
};  

/* ----------------- statistics ----------------------------------*/
/* 23.12.99 diese Structur ist zu gross - run time error auf PC   */
/*
  #ifdef VMS
*/
/* so wars bei Johannes, ich verkleinere die feldgrenzen
   struct  statistics {
   double fd1phmax[512][512];
   int  nn1,nn2,inumb[1100][1100];
   int  inumzit,inumyit,inumzan,inumyan;
   };  
*/

/*
struct  statistics {
  double fd1phmax[128][128];
  int  nn1,nn2,inumb[256][256];
  int  inumzit,inumyit,inumzan,inumyan;
};  
*/
/* ----------------- constants -----------------------------------*/

struct  constants {
  double pi,gam[166],fs[166],fc[166];
  COMPLEX sqrtm1;
};  

#endif 
/* end phase_struct_10.h */
