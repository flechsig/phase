/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/initdatset.c */
/*   Date      : <12 Feb 04 12:17:45 flechsig>  */
/*   Time-stamp: <04 Aug 09 13:54:16 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */


#include <stdio.h>
#include <stdlib.h> 
#include <string.h>


#include <Xm/Text.h>                                                  
#include <Xm/FileSB.h>                /*FileBox*/     
#include <Xm/List.h>   
#include <Xm/ToggleB.h>   
#include <Mrm/MrmAppl.h>  
#include <X11/Xlib.h>      
#include <X11/Xutil.h>      
#ifdef VMS 
  #include <DXm/DECspecific.h>                  
  #include <descrip.h>
#endif

#include "cutils.h"
#include "phase_struct.h"
#include "fg3pck.h" 
#include "mirrorpck.h" 
#include "geometrypck.h"    
#include "phase.h"                                
#include "rtrace.h"            

void initdatset(struct datset *x, struct BeamlineType *bl)
{
  int i;
  
  struct UndulatorSourceType *up;
  struct DipolSourceType     *dp;
  struct PointSourceType     *pp;
  struct HardEdgeSourceType  *hp;     
  struct SRSourceType        *sp;  
  struct PSImageType	     *ip;
  struct FileSourceType      *fp;
  
  if (bl->RTSource.QuellTyp != bl->RTSource.QuellTyp_old) 
    {
      x->itrans	= D0itrans;     
      x->idir  	= D0idir;
      x->imodus	= D0imodus;
      x->disty 	= D0disty;   
      x->iheigh	= D0iheigh;
      x->distz 	= D0distz;
      x->iwidth	= D0iwidth;
      x->divy  	= D0divy;
      x->idivy 	= D0idivy;
      x->divz  	= D0divz;
      x->idivz 	= D0idivz;
      x->intmod	= D0intmod;
      x->disty1	= D0disty1;
      x->disty2	= D0disty2;        
      x->distz1	= D0distz1;        
      x->distz2	= D0distz2;        
      x->yi    	= D0yi;            
      x->zi    	= D0zi;            
      x->dyi   	= D0dyi;           
      x->dzi   	= D0dzi;           
      x->w     	= D0w;             
      x->xl    	= D0xl; 
      x->isourcefile = D0isourcefile;  
      x->xlam_test   = D0xlam_test;  
      
      x->sigmay 	= D0sigmay;
      x->sigmayp	= D0sigmayp;
      x->ymin	= D0ymin;
      x->ymax	= D0ymax;
      x->sigmaz	= D0sigmaz;
      x->sigmazp 	= D0sigmazp;
      x->zmin	= D0zmin;
      x->zmax	= D0zmax;
      x->epsilon	= D0epsilon;
      x->fracy 	= D0fracy;
      x->frac1y	= D0frac1y;
      x->fracz	= D0fracz;
      x->frac1z	= D0frac1z;
      x->iord	= D0iord;
      x->isrcy 	= D0isrcy;
      x->isrcdy	= D0isrcdy;
      x->inumy	= D0inumy;
      x->itery0	= D0itery0;
      x->ianzy0	= D0ianzy0;
      x->imaxy	= D0imaxy;
      x->isrcz	= D0isrcz;
      x->isrcdz	= D0isrcdz;
      x->inumz	= D0inumz;
      x->iterz0	= D0iterz0;                
      x->ianzz0	= D0ianzz0;
      x->imaxz	= D0imaxz;
      
      x->SR2out.y= x->SR2out.z= x->SR2out.dy= x->SR2out.dz= 0.0;  
      
      bl->RTSource.raynumber= 1000;
      switch(bl->RTSource.QuellTyp)
	{
	case 'U': 
	case 'u':
	  up= (struct UndulatorSourceType *)bl->RTSource.Quellep; 
	  up->length= 3800.0;
	  up->lambda= 12.4e-6;  
	break;   
	case 'L': 
	case 'M':
	  up= (struct UndulatorSourceType *)bl->RTSource.Quellep; 
	  up->length= 3800.0;
	  up->lambda= 12.4e-6;
	  up->deltaz= 0.0;
	break;
	case 'D': dp=(struct DipolSourceType *)bl->RTSource.Quellep; 
	  dp->sigy		= 0.093;  
	  dp->sigdy	        = 1.;  
	  dp->sigz        	= 0.05;
	  dp->dz          	= 4.0;
	  break;  
	case 'o': pp=(struct PointSourceType *)bl->RTSource.Quellep; 
	  pp->sigy	= 0.093;  
	  pp->sigdy	= 1.;  
	  pp->sigz      = 0.05;
	  pp->sigdz     = 1.0;
	  break;  
	case 'S': sp= (struct SRSourceType *)bl->RTSource.Quellep;
	  sp->y	        =0.1;  
	  sp->dy	=0.1;  
	  sp->z	        =0.1;  
	  sp->dz	=0.1; 
	  break;   
	case 'I': ip= (struct PSImageType*)bl->RTSource.Quellep; 
	  ip->ymin	= -1.0e-1;  
	  ip->ymax	=  1.0e-1;  
	  ip->zmin	= -1.0e-1;  
	  ip->zmax	=  1.0e-1;
	  ip->iy   =   15;
	  ip->iz   =   15;
	  break;   
	case 'H': 
	  hp= (struct HardEdgeSourceType *)bl->RTSource.Quellep; 
	  bl->RTSource.QuellTyp= 'H';
	  hp->disty	= .1;  
	  hp->iy 		= 3;   
	  hp->distz	= .2;  
	  hp->iz		= 3;   
	  hp->divy	= 1.;  
	  hp->idy		= 7;   
	  hp->divz	= 4.;  
	  hp->idz		= 7;   
	  bl->RTSource.raynumber= hp->iy * hp->iz * hp->idy * hp->idz;
	  break;   
	case 'F'
	  fp= (struct FileSourceType *)bl->RTSource.Quellep;
	  strncpy(fp->filename, PHASESet.sourceraysname, MaxPathLength);
	  /* we may add a test if the file exists */
	break;   
	}  /* end case */
      bl->RTSource.QuellTyp_old =  bl->RTSource.QuellTyp;
      printf("initdatset: put defaults\n");
    } else  printf("initdatset: same source- no defaults set\n");
  /* 24.6.96 */
  printf("initdatset.c: set  idir, iord, epsilon to defaults\n");
  bl->BLOptions.SourcetoImage= x->idir; 
  bl->BLOptions.ifl.iord=      x->iord;
  bl->BLOptions.epsilon=       x->epsilon;  
}
/* end /home/vms/flechsig/vms/phas/phasec/initdatset.c */
