/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/opti/optisubc.c */
/*   Date      : <31 Oct 03 08:15:40 flechsig>  */
/*   Time-stamp: <19 Sep 19 17:26:28 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */

/****************************************************************************
**
** Copyright (C) 2005-2006 Trolltech ASA. All rights reserved.
**
** This file is part of the example classes of the Qt Toolkit.
**
** This file may be used under the terms of the GNU General Public
** License version 2.0 as published by the Free Software Foundation
** and appearing in the file LICENSE.GPL included in the packaging of
** this file.  Please review the following information to ensure GNU
** General Public Licensing requirements will be met:
** http://www.trolltech.com/products/qt/opensource.html
**
** If you are unsure which license is appropriate for your use, please
** review the following information:
** http://www.trolltech.com/products/qt/licensing.html or contact the
** sales department at sales@trolltech.com.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
****************************************************************************/

/* 31.3.99 GetRMS erweitert auf z */

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include <stdio.h>                    /* For printf and so on. */
#include <stdlib.h>	    	      /* needed for fopen      */
#include <string.h>
#include <math.h>

#include "../phase/cutils.h"  
#include "../phase/phase_struct.h"
#include "../phase/phase.h"
#include "../phase/rtrace.h"
#include "../opti/optisubc.h"     


/* abgespecktes (schnelles) buildbeamline  */
/* baut beamline und extrahiert map        */
void buildsystem(struct BeamlineType *bl) 
{
  int     elcounter, i, mdim;
  struct  ElementType *listpt;    

  mdim= 330;

  elcounter= 1; 
  listpt= bl->ElementList; 
  while (elcounter<= bl->elementzahl)
    { 
      bl->position=elcounter;         /* parameter fuer MakeMapandMatrix */
      if ((listpt->ElementOK & mapOK & elementOK) == 0)       /* map must be rebuild */
	{ 
	  DefMirrorC(&listpt->MDat, &listpt->mir, listpt->MDat.Art, listpt->GDat.theta0, 
			 bl->BLOptions.REDUCE_maps, bl->BLOptions.WithAlign, (elcounter-1));    
	  DefGeometryC(&listpt->GDat, &listpt->geo, &bl->BLOptions); 
	  --elcounter;
	  MakeMapandMatrix(listpt, bl, &elcounter);
	  ++elcounter;

	  listpt->ElementOK|= (mapOK | elementOK); 
	}
      if (listpt->MDat.Art != kEOESlit)
	{
	  if (elcounter == 1)
	    memcpy(&bl->M_StoI, &listpt->M_StoI, sizeof(MAP70TYPE)); 
	  else		                          /* bline zusammenbauen */
	    GlueLeft((double *)bl->M_StoI, (double *)listpt->M_StoI, &bl->BLOptions.ifl.iord);  
            /* A= B* A */
      	  SetDeltaLambda(bl, listpt);              /* resolutionfactor */
	} 
      elcounter++; listpt++; 
    } /* Schleife ueber alle Elemente fertig */
 
   extractmap(bl->M_StoI, bl->ypc1, bl->zpc1, bl->dypc, bl->dzpc, 
	     &bl->BLOptions.ifl.iord); 
   bl->beamlineOK |= mapOK;
  /* bline ist fertig, map ist erzeugt */
} /* end buildsystem */

/*                                                           */
/* change one value in beamline structure addressed by index */
/*                                                           */   
void in_struct(struct BeamlineType* bl, double *z, int index)
{
  int elnumber, mtype, ipos;  
  double *xd, cl, alpha, beta, teta, fi, cff;
  struct mirrortype *m;                      /* matrix   */
  struct mdatset *mdat;                      /* Rohdaten */
  struct gdatset *gdat;
  struct geometrytype *g;  
  struct ElementType  *listpt;

  elnumber= index >> 8;      
  mtype   = index & 0x80;
  ipos    = index & 0x7f;

  listpt= &bl->ElementList[elnumber]; 
  listpt->ElementOK &= (~(mapOK | elementOK));
  if (mtype)
    {
      mdat= &listpt->MDat; 
      switch (ipos)
	{  
	case 81:			/* r */
	  mdat->rmi= *z;
	  break;
	case 82:			/* rp */
	  mdat->rho= *z;
	  break;
	case 83:			/* 2w */
	  mdat->w1= -0.5* *z;
	  mdat->w2=  0.5* *z;
	  break;
	case 84:			/* 2l */
	  mdat->l1= -0.5* *z;
	  mdat->l2=  0.5* *z;
	  break;
	case 85:			/* slopew */
	  mdat->slopew= *z;
	  break;
	case 86:			/* slopel */
	  mdat->slopel= *z;
	  break;
	case 87:			/* rowland slits */
	  /*  listpt->ElementOK |= mapOK | elementOK; */
	  bl->ElementList[elnumber- 1].ElementOK &= (~mapOK); 
	  bl->ElementList[elnumber- 1].ElementOK &= (~elementOK);
	  bl->ElementList[elnumber+ 1].ElementOK &= (~mapOK); 
	  bl->ElementList[elnumber+ 1].ElementOK &= (~elementOK);

	  bl->ElementList[elnumber- 1].MDat.l1= -0.5* *z;
	  bl->ElementList[elnumber- 1].MDat.l2=  0.5* *z;
	  bl->ElementList[elnumber+ 1].MDat.l1= -0.5* *z;
	  bl->ElementList[elnumber+ 1].MDat.l2=  0.5* *z;
	  bl->ElementList[elnumber+ 1].MDat.w1= -0.5* *z;
	  bl->ElementList[elnumber+ 1].MDat.w2=  0.5* *z;
	  printf("insert rowland slits: %f\n", *z);
	  break;

	default:
	  /*direktes beschreiben einzelner matrixelemente */
	  xd= (double *)&listpt->mir;       /******** in mirrortype */
	  xd[ipos] = *z;
	  listpt->ElementOK |= elementOK; 
	  /* damit DefMirrorC nicht gerufen wird */
	  break;
	} /* end switch */
    }
  else  /* gdat */
    {
      gdat= &listpt->GDat;
      /*   listpt->ElementOK &= (~geometryOK); */
      switch (ipos)
	{
	case 0:                 	/* theta */
	  gdat->theta0= *z;       
	  break;
	case 1:   /* gesamtlaenge bleibt const.,  Eintritt variabel */
	  cl= gdat->r+ gdat->rp;
	  gdat->rp= cl- (*z); 
	  gdat->r= *z;
	  break;
	case 2: /* gesamtlaenge bleibt const.,  Austritt variabel */ 
	  cl= gdat->r+ gdat->rp;
	  gdat->r= cl- (*z); 
	  gdat->rp= *z;
	  break;
	case 3:                                    /* E (eV) */
	  bl->BLOptions.lambda= 
	    (*z > 0.0) ? (1240.0e-6/ *z) : 0.0;
	  break;  
	case 4:                               
	  gdat->r= *z;
	  break; 
	case 5:                              
	  gdat->rp= *z;
	  break; 
	case 6:            			     /* dens */
	  gdat->xdens[ipos- 6]= *z;   
	  break;  
	case 7:            			     /* dens */
	  gdat->xdens[ipos- 6]= *z;   
	  break;  
	case 8:            			     /* dens */
	  gdat->xdens[ipos- 6]= *z;   
	  break;  
	case 9:            			     /* dens */
	  gdat->xdens[ipos- 6]= *z;   
	  break;  
	case 10:            			     /* dens */
	  gdat->xdens[ipos- 6]= *z;   
	  break;  
	case 11:        	           /* m lambda in nm */ 
	  bl->BLOptions.lambda= *z * 1e-6;   
	  break;
	case 12:  /* theta CLRCM bei BESSY I dy= 44.2 mm, Planspiegel in r2 */
	          /* L= 6500 mm */
	  cl= *z * PI/180.0;
	  gdat->rp= 6500.0- gdat->r+ 2.0* 44.2* cos(cl)* cos(cl)/sin(2.0* cl);
/*        cl= gdat->theta0* PI/180.0;     /* alter winkel     */ 
/* 	  gdat->rp+= 44.2 *                                   */
/* 	  ( (1.0 + cos(*z * PI/180.0))/ sin(*z * PI/180.0) -  */
/* 	  (1.0 + cos(cl))/ sin(cl) );                         */
	  gdat->theta0 = *z;			     /* theta */
	  break;
	
	case 13:                          /*cl r1 fest*/
	  gdat->rp= (*z) - gdat->r;	 
	  break;
	case 14:  	/* theta sgm vodar*/
	  gdat->theta0 = *z; 
	  gdat->r= 2300.4* 
	    (sin((2.0* (*z)+ 1.12)* PI/180.0)+ sin(1.12* PI/180.0))/ 
	    sin((*z)* PI/90.0);
	  break;
	case 15:       /* cff constant */
	  fprintf(stderr, "in_struct: cff %g\n", *z);
	  FixFocus(*z, bl->BLOptions.lambda, gdat->xdens[0], gdat->inout, 
		   &alpha, &beta);
	  gdat->theta0= fabs(alpha- beta)* 90.0/ PI;  /* 0.5 * (a-b) */
	  if (gdat->azimut > 1) gdat->theta0= -fabs(gdat->theta0);
	  fprintf(stderr, "in_struct: theta %g????\n", gdat->theta0);
	  break;
	case 16:       /* Energiescan mit cff constant */
	  /* berechne cff aus alten werten */
	  teta= fabs(gdat->theta0* PI/ 180.0);   /* theta */
	  fi  = (double)(gdat->inout)* asin(bl->BLOptions.lambda* gdat->xdens[0]/
					    (2.0* cos(teta)));
	  cff= cos(fi- teta)/ cos(fi+ teta); 
	  bl->BLOptions.lambda=  
	    (*z > 0.0) ? (1240.0e-6/ *z) : 0.0;
	  fprintf(stderr, 
		  "\n in_struct: energy %g, NB: cff= const = %g\n", *z, cff);
	  FixFocus(cff, bl->BLOptions.lambda, gdat->xdens[0], gdat->inout, 
		   &alpha, &beta);
	  gdat->theta0= fabs(beta- alpha)* 90.0/ PI;  /* 0.5 * (a-b) */
	  if (gdat->azimut > 1) gdat->theta0= -fabs(gdat->theta0);
	  break;
	case 17: xd[4]= (*z) - xd[5];     /*cl r2 fest*/
	  gdat->r= (*z)- gdat->rp;
	  break;
	case 18: /* STXM special M1- S1 */
	  
	  gdat->rp= *z;                    /* new distance */
	  mdat= &listpt->MDat;             /* new radius r */
	  teta= fabs(gdat->theta0* PI/ 180.0);
	  listpt->ElementOK &= (~elementOK);  
	  mdat->rmi= 2.0/ cos(teta) * gdat->rp * gdat->r /(gdat->rp + gdat->r);
	  /* the spectrometer is 2 elements later- get the length */
	  cl= bl->ElementList[elnumber+ 2].GDat.r+ 
	    bl->ElementList[elnumber+ 2].GDat.rp + gdat->rp;  /* focal length rho */
	  mdat->rho= 2.0* cos(teta) * cl * gdat->r /(cl + gdat->r);
          printf("in_struct: (STXM special M1-S1): %.2f, M1 r, rho: %.2f, %.2f, focus %.2f \n", 
		 gdat->rp, mdat->rmi, mdat->rho, cl);
	  break;
	case 19: /* STXM special S1-S2 spectrometer length */
	  mdat= &listpt->MDat;
	  teta= fabs(gdat->theta0* PI/ 180.0);
	  fi  = (double)(gdat->inout)* asin(bl->BLOptions.lambda* gdat->xdens[0]/
					    (2.0* cos(teta)));
	  cl= mdat->rmi;  /* old radius */  
	  /* new grating radius assuming keep rowland conditions */
	  listpt->ElementOK &= (~elementOK); 
	  mdat->rmi= mdat->rho= *z/ (2* cos(teta) * cos(fi));  
	  gdat->r= mdat->rmi * cos(teta + fi);
	  gdat->rp= *z- gdat->r;
	  printf("in_struct: (STXM special S1-S2): %.2f, radius: %.2f, old R: %.2f\n", 
		 *z, mdat->rmi, cl);
	  bl->BLOptions.displength= gdat->rp;
	  /* change radius rho of mirror 2 elements upstream */
	  teta= fabs(bl->ElementList[elnumber- 2].GDat.theta0* PI/ 180.0);
	  cl= bl->ElementList[elnumber- 2].GDat.rp+ *z; /* focal length rho */
	  bl->ElementList[elnumber- 2].MDat.rho= 2.0* cos(teta) * 
	    cl * bl->ElementList[elnumber- 2].GDat.r /
	    (cl + bl->ElementList[elnumber- 2].GDat.r);
	  printf("in_struct: (STXM special S1-S2) M1-R, rho: %.2f, %.2f, focal l. rho: %.2f\n", 
		 bl->ElementList[elnumber- 2].MDat.rmi,
		 bl->ElementList[elnumber- 2].MDat.rho, cl);
	  bl->ElementList[elnumber- 2].ElementOK &= (~elementOK);
	  /* bl->ElementList[elnumber- 2].ElementOK &= (~geometryOK); */
	  bl->ElementList[elnumber- 2].ElementOK &= (~mapOK);
	  break;
	default:
	  printf("in_struct: index %d not found\n", ipos);
	  break;
	}          
    }
} /* end in_struct */

/*                                                              */
/* returns one value from beamline structure addressed by index */
/*                                                              */  
double out_struct(struct BeamlineType  *bl, double *z, int index)    
{
  int elnumber, mtype, ipos;
  double *xd, teta, fi, cff;
  struct mirrortype   *m;
  struct geometrytype *g;  
  struct mdatset *mdat;                      /* Rohdaten */
  struct gdatset *gdat;
  struct ElementType *listpt;

  elnumber= index >> 8;
  mtype   = index & 0x80;
  ipos    = index & 0x7f;

  listpt= &bl->ElementList[elnumber]; 
  if (mtype)
    {
      mdat= &listpt->MDat; 
      switch (ipos)
	{ 
	case 81:			/* r = 36 */
	  *z= mdat->rmi;    
	  break;  
	case 82:			/* rp = 37 */
	  *z= mdat->rho;  
	  break;
	case 83:			/* 2w */
	  *z=  mdat->w2- mdat->w1;
	  break;
	case 84:			/* 2l */
	  *z=  mdat->l2- mdat->l1;
	  break;
	case 85:			/* slopew */
	  *z= mdat->slopew;
	  break;
	case 86:			/* slopel */
	  *z= mdat->slopel;
	  break;
	case 87:			/* rowland slits */
	  *z= bl->ElementList[elnumber- 1].MDat.l2- 
	    bl->ElementList[elnumber- 1].MDat.l1;
	  break;

	default: 
	  /*direktes beschreiben einzelner matrixelemente */
	  xd= (double *)&listpt->mir;       /******** in mirrortype */
	  *z= xd[ipos];
	  break;
	}
    } 
  else
    {
      gdat= &listpt->GDat;
      switch (ipos)
	{
	case 0:   	                      /* theta */ 
	*z= gdat->theta0;
	  break; 
	case 1:                               /* gesamtlaenge */
	  *z= gdat->r;  	  
	  break;
	case 2:   
	  *z= gdat->rp; 
	  break;    
	case 3:                               /* E in eV */
	  *z= (bl->BLOptions.lambda > 0.0) ? 
	    (1240.0e-6/ bl->BLOptions.lambda) : 0.0;  
	  break; 
	case 4:                               
	  *z= gdat->r;
	  break; 
	case 5:                               
	  *z= gdat->rp;
	  break; 
	case 6:
	  *z= gdat->xdens[ipos- 6];
	  break;
	case 7:
	  *z= gdat->xdens[ipos- 6];
	  break;
	case 8:
	  *z= gdat->xdens[ipos- 6];
	  break;
	case 9:
	  *z= gdat->xdens[ipos- 6];
	  break;
	case 10:
	  *z= gdat->xdens[ipos- 6];
	  break;
	case 11: 
	  *z= bl->BLOptions.lambda* 1e6;               /* m lambda in nm */ 
	  break;
	case 12: 	 	     /*  theta CLRCM */ 
	  *z= gdat->theta0;
	  break; 
	case 17:
	case 13:  
	
	  *z= gdat->r+ gdat->rp; 
	  break;
	case 14:   	/*sgm vodar*/
	  *z= gdat->theta0;	
	break;	
	case 15:       /* cff constant */
	  teta= fabs(gdat->theta0* PI/ 180.0);   /* theta */
	  fi  = (double)(gdat->inout)* asin(bl->BLOptions.lambda* gdat->xdens[0]/
					    (2.0* cos(teta)));
	  *z= cos(fi- teta)/ cos(fi+ teta);    /* cos(beta)/ cos(alpha); */
	  printf("outstruct_15: cff= %f,??? theta= %f rad, phi= %f rad\n", *z, teta, fi);
	  break;
	case 16:       /* Energiescan mit cff constant */
	  teta= fabs(gdat->theta0* PI/ 180.0);   /* theta */
	  fi  = (double)(gdat->inout)* asin(bl->BLOptions.lambda* gdat->xdens[0]/
					    (2.0* cos(teta)));
	  cff= cos(fi- teta)/ cos(fi+ teta);   /* cos(beta)/ cos(alpha); */
	  *z= (bl->BLOptions.lambda > 0.0) ? 
	    (1240.0e-6/ bl->BLOptions.lambda) : 0.0;
	  printf("outstruct_16: cff= %f ??? theta= %f rad, phi= %f rad\n", cff, teta, fi);
	  break;
	  /*	case 17: siehe weiter oben */
	case 18: /* STXM special M1- S1 */
	  *z= gdat->rp;
	  printf("outstruct: (STXM special M1-S1): %.2f\n", *z);
	  break;
	  /* case 19: /* STXM special S1-S2 spectrometer length */
	  /* case 19: siehe weiter oben analog 13 und 17 */  
	case 19:  
	  *z= gdat->r+ gdat->rp;
	  printf("outstruct: (STXM special S1-S2): %.2f\n", *z);
	  break;
	default:
	  printf("out_struct: index %d not found\n", ipos);
	  break;
	  }          
      }
  return *z;
}     /* end out_struct */


void Get_dydz_fromSource(struct BeamlineType *bl, double *dy, double *dz)
{
  struct HardEdgeSourceType  *hs;
  struct DipolSourceType     *ds;
  struct UndulatorSourceType *us; 
  struct SRSourceType        *sp; 
  struct PointSourceType     *ps;
  double h, beugung; 
  
  switch (bl->RTSource.QuellTyp)
    {
    case 'U':
    case 'u':
      us= (struct UndulatorSourceType *) 
	bl->RTSource.Quellep;
      us->lambda= bl->BLOptions.lambda;
      *dz= *dy= sqrt(us->lambda/ us->length);
      printf("Get_dydz_fromSource: undulator at BII, high/low beta s.\n");
      printf("Get_dydz_fromSource: lambda= %g mm?, dy,dz= %g rad\n", 
	     us->lambda, *dy);
      /* high beta */
      /* eigentlich brauche ich die Verteilung der rotierenden Verteilung */
      break; 
    case 'D':
      ds= (struct DipolSourceType *) bl->RTSource.Quellep; 
      *dy= ds->sigdy/ 1000.0;  
      *dz= ds->dz/ 1000.0; 
      printf("Get_dydz_fromSource: dipol source \n");
      printf("Get_dydz_fromSource: dy= %g, dz= %g (rad)\n", *dy, *dz);
      break; 
    case 'S':   /* single ray */
      sp= (struct SRSourceType *)bl->RTSource.Quellep;
      *dy= sp->dy/ 1000.0;
      *dz= sp->dz/ 1000.0;
      printf("Get_dydz_fromSource: single ray\n");
      printf("Get_dydz_fromSource: dy= %g, dz= %g (rad)\n", *dy, *dz);
      break;
    case 'o':   /* point source */
      ps= (struct PointSourceType *)bl->RTSource.Quellep;
      *dy= ps->sigdy/ 1000.0;
      *dz= ps->sigdz/ 1000.0;
      printf("Get_dydz_fromSource: point source\n");
      printf("Get_dydz_fromSource: dy= %g, dz= %g (rad)\n", *dy, *dz);
      break;
    default:
      hs= (struct HardEdgeSourceType *) bl->RTSource.Quellep;
      printf("Get_dydz_fromSource: default dy=dz= 0.001 rad\n");
      *dy= *dz= 0.001;         	 
      break;
    }
} /* end Get_dydz_fromSource */

/* GetRMS wrapper */
void GetFWHM(struct BeamlineType *bl, char *ch, double *chi)
{
  GetRMS(bl, ch, chi);
  *chi*= 2.35;
}

/* 31.3.99  erweitert auf z  */
/* 13.12.07 erweitert  auf r */
/*  3.1.08 fun -> proc       */
void GetRMS(struct BeamlineType *bl, char *ch, double *chi)
{
  double EX, EX2, rms, tmp;
  int    i, n;
  struct RayType *rays;

  n   = bl->RESULT.points1;
  rays= (struct RayType *)bl->RESULT.RESp;
  EX= EX2= 0.0;
  if (n > 0)
    {
      switch (*ch)
	{
	case 'z':
	  for (i= 0; i< n; i++)
	    {
	      /*   EX += bl->RESULT.RESUnion.Rays[i].z;
		   EX2+= bl->RESULT.RESUnion.Rays[i].z* 
		   bl->RESULT.RESUnion.Rays[i].z;*/
	      EX += rays[i].z;
	      EX2+= rays[i].z* rays[i].z;
	    }
	  break;
	case 'y':
	  for (i= 0; i< n; i++)
	    {
	      EX += rays[i].y;
	      EX2+= rays[i].y * rays[i].y;
	    }
	  break;
	default: /* r */
	  for (i= 0; i< n; i++)
	    {
	      tmp= rays[i].y * rays[i].y + rays[i].z * rays[i].z;
	      EX += sqrt(tmp);
	      EX2+= tmp;
	    }
	  break;
	}
      EX  /= (double) n;
      EX2 /= (double) n;
      rms= sqrt(EX2- (EX * EX));
    } else rms= 0.0;

#ifdef DEBUG 
  fprintf(stderr, "%c->rms= %g, EX= %g, EX2= %g, n= %d\n", *ch, rms, EX, EX2, n);
#endif

  *chi= rms;
} /* end GetRMS */

/*
   calculates the focussize depending on the divergence 
   (for optimization of the spot size)
   UF 11.12.07 add averaging with mirrored input i.e. do not only 
   optimize the top right ray also the bottom right, bottom left 
   and top left 
*/

void FocusSize(double *chi, struct BeamlineType *bl, 
	       double *dyin, double *dzin)
{
  
  struct RayType Rayin[4], Rayout[4];
  int i;
  double yout, zout;

  Rayin[0].y= Rayin[0].z= Rayin[1].y= Rayin[1].z= 
    Rayin[2].y= Rayin[2].z= Rayin[3].y= Rayin[3].z= 0.0;

/* empiric value optimization with 2 sigma gives better result */  

  *dyin*= 2;    
  *dzin*= 2; 

  Rayin[0].dy= Rayin[1].dy=  *dyin;
  Rayin[2].dy= Rayin[3].dy= -*dyin;
  Rayin[0].dz= Rayin[3].dz= -*dzin;
  Rayin[1].dz= Rayin[2].dz=  *dzin;

  yout= zout= *chi=0.0;

  for (i= 0; i< 4; i++)
    {    
      ray_tracef(&Rayin[i], &Rayout[i], &bl->BLOptions.ifl.iord, 
		 (double *)bl->ypc1, (double *)bl->zpc1, 
		 (double *)bl->dypc, (double *)bl->dzpc);
    
      yout+= fabs(Rayout[i].y);
      zout+= fabs(Rayout[i].z);
      *chi+= sqrt(yout * yout + zout * zout);
    }
  
  *chi *= 0.25;
} /* end FocusSize */

/*
   calculates the focussize depending on the divergence 
   (for optimization of the spot size)
*/

void FullRTOpti(double *chi, struct BeamlineType *bl)
{
  double transmittance;
  
  printf("************ FullRTOpti ************\n");
  ReAllocResult(bl, PLrttype, bl->RTSource.raynumber, 0);
  RayTraceFull(bl);
  transmittance= (double)bl->RESULT.points1/
    (double)bl->RTSource.raynumber;
  *chi= 1.0- transmittance;
} /* end FullRTOpti */

/* do a normal ray trace */
/* ch goes to GetRMS     */
void RTOpti(double *chi, struct BeamlineType *bl, char *ch)
{
  printf("************ RTOpti with target: %c ***********\n", *ch);
  RayTracec(bl);
  GetFWHM(bl, ch, chi);
} /* end RTOpti */
/* end optisubc.c */
