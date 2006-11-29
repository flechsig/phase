/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/baselib/phabasedefs.h */
/*  Date      : <13 Mar 06 08:29:09 flechsig>  */
/*  Time-stamp: <14 Mar 06 13:05:54 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */


#ifndef PHABASEDEFS_H
 #define PHABASEDEFS_H

#define PI (4.0*atan(1.0))
#define exithplot      exithplot_ 
  #define extractmap     extractmap_
  #define fdet           fdet_
  #define fgmapidp       fgmapidp_
  #define hlimit         hlimit_
  #define hplint         hplint_
  #define hplotdisplayf  hplotdisplayf_
  #define hplotpsdf      hplotpsdf_
  #define hplotpssimf    hplotpssimf_
  #define inithplot      inithplot_
  #define intersection   intersection_
  #define pathlen0       pathlen0_
  #define pathlen1       pathlen1_
  #define pstf           pstf_ 
  #define readfg34_par   readfg34_par_
  #define readmatrixfile readmatrixfile_
  #define src_ini        src_ini_
  #define xxmap70        xxmap70_ 
#define misali         misali_
#define ray_tracef ray_tracef_
 

  #define MaxPathLength           255

  
  #define kEOETM                  66
  #define kEOETG                  67
  #define kEOEVLSG                68
#define kEOEElli                78
#define kEOEPElli               79
#define kEOESlit                99
#define kEOEGeneral              350
#define kEOECone                 351
#define kEOEPElliG               352
#define kEOEPM                   353
#define kEOEPG                   354
#define kEOEPGV                  355
#define kEOEDrift               999


#define sourceOK   	1
#define mapOK      	2                /* werden und verknuepft */ 
#define resultOK   	4
#define elementOK  	8
#define geometryOK 	16          

#endif  /* PHABASEDEFS_H */
