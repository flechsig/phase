/*  File      : /home/pss060/sls/flechsig/phase/src/phase/initdatset.c */
/*  Date      : <28 Oct 99 09:58:52 flechsig>  */
/*  Time-stamp: <12 Feb 04 12:14:08 flechsig>  */
/*  Author    : Flechsig Uwe OVGA/203a 4535, flechsig@psi.ch */

/*  File      : /home/vms/flechsig/vms/phas/phasec/initdatset.c */
/*  Date      : <13 Feb 98 12:27:39 flechsig>  */
/*  Time-stamp: <28 Oct 99 09:58:51 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@exp.bessy.de */

/*  File      : /home/vms/flechsig/vms/phas/phasec/fg3pck.c */
/*  Date      : <23 Apr 97 09:15:51 flechsig>  */
/*  Time-stamp: <13 Feb 98 12:27:29 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@exp.bessy.de */

/* Datei: USERDISK_3:[FLECHSIG.PHASE.PHASEC]FG3PCK.C           */
/* Datum: 19.JUL.1994                                          */
/* Stand: 17-FEB-1997                                          */
/* Autor: FLECHSIG, BESSY Berlin                               */

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

void initdatset(struct datset *x, struct BeamlineType *bl, int sw)
{
  int i;
  
  struct UndulatorSourceType *up;
  struct DipolSourceType     *dp;
  struct PointSourceType     *pp;
  struct HardEdgeSourceType  *hp;     
  struct SRSourceType        *sp;  
  struct PSImageType	     *ip;
  
  switch (sw) 
    {
    case kESDipolSourceButton:     i= 'D'; break;
    case kESPointSourceButton:     i= 'o'; break;
    case kESUndulatorSourceButton: i= 'U'; break; 
    case kESundulatorSourceButton: i= 'u'; break; 
    case kESUndulatorSISButton:    i= 'L'; break; 
    case kESUndulatorSIMButton:    i= 'M'; break; 
    case kESSR2Button:             i= 'S'; break; 
    case kESPhaseSpaceImageButton: i= 'I'; break;
    case kESDefaults:              i=  0 ; break;
    case kESRayTraceButton:    
    default:                       i= 'H'; break; 
    }
  if ((bl->RTSource.QuellTyp != i) || (i == 0)) 
    {
      if (i != 0) bl->RTSource.QuellTyp = i;
      
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
      
      
      switch(bl->RTSource.QuellTyp)
	{
	case 'U': 
	case 'u':
	  up= (struct UndulatorSourceType *) 
		    &(bl->RTSource.Quelle.UndulatorSource);
	up->length= 3800.0;
	up->lambda= 12.4e-6;  
	break;   
	case 'L': 
	case 'M':
	  up= (struct UndulatorSourceType *) 
		    &(bl->RTSource.Quelle.UndulatorSource);
	up->length= 3800.0;
	up->lambda= 12.4e-6;
	up->deltaz= 0.0;
	break;
	case 'D': dp=(struct DipolSourceType *) 
		    &(bl->RTSource.Quelle.DipolSource);
	  dp->sigy		= 0.093;  
	  dp->sigdy	= 1.;  
	  dp->sigz        	= 0.05;
	  dp->dz          	= 4.0;
	  break;  
	case 'o': pp=(struct PointSourceType *) 
		    &(bl->RTSource.Quelle.PointSource);
	  dp->sigy	= 0.093;  
	  dp->sigdy	= 1.;  
	  dp->sigz      = 0.05;
	  dp->sigdz     = 1.0;
	  break;  
	case 'S': sp= (struct SRSourceType *) &(bl->RTSource.Quelle.SRSource);
	  sp->y	=1.;  
	  sp->dy	=1.;  
	  sp->z	=1.;  
	  sp->dz	=1.; 
	  break;   
	case 'I': ip= (struct PSImageType*) &(bl->RTSource.Quelle.PSImage);
	  ip->ymin	= -1.0e-1;  
	  ip->ymax	=  1.0e-1;  
	  ip->zmin	= -1.0e-1;  
	  ip->zmax	=  1.0e-1;
	  ip->iy   =   15;
	  ip->iz   =   15;
	  break;   
	case 'H': 
	default : hp= (struct HardEdgeSourceType *) 
		    &(bl->RTSource.Quelle.HardEdgeSource);
	bl->RTSource.QuellTyp= 'H';
	hp->disty	= .1;  
	hp->iy 		= 3;   
	hp->distz	= .2;  
	hp->iz		= 3;   
	hp->divy	= 1.;  
	hp->idy		= 7;   
	hp->divz	= 4.;  
	hp->idz		= 7;   
	break;          
	}
      bl->RTSource.raynumber= (bl->RTSource.QuellTyp == 'H') ? 
	hp->iy * hp->iz * hp->idy * hp->idz : 1000;
    }
  /* 24.6.96 */
  bl->BLOptions.SourcetoImage= x->idir; 
  bl->BLOptions.ifl.iord=      x->iord;
  bl->BLOptions.epsilon=       x->epsilon;  
}
/* end /home/vms/flechsig/vms/phas/phasec/initdatset.c */
