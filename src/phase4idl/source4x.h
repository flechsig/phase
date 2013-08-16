/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase4idl/source4x.h */
/*  Date      : <16 Aug 13 10:05:33 flechsig>  */
/*  Time-stamp: <16 Aug 13 10:08:37 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */


/* extrtact the source4 definitions to be independent from configure */

#ifndef SOURCE4X_H
#define SOURCE4X_H

#define GRIDSIZE 2048

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


#ENDIF
