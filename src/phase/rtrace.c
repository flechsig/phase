/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/rtrace.c */
/*   Date      : <23 Mar 04 11:27:42 flechsig>  */
/*   Time-stamp: <2023-08-10 12:05:45 flechsig>  */
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


/*  UF 0804 no X11 left  */

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include <stdio.h> 
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

    
#include "cutils.h" 
#include "phase_struct.h"
/*#include "fg3pck.h"  */
                 
#include "phase.h"         
#include "rtrace.h"
#include "common.h" 

#ifdef HAVE_HDF5
   #include "hdf5.h"
   #include "myhdf5.h"
#endif 
 
/***********************************************************************/
/* Hard edge Quelle                   	                 19.3.96       */
/***********************************************************************/ 
void MakeHardEdgeSource(struct RTSourceType *y)   
{
   int i,j,k,l,rnumber;
   double ly, lz, ldy, ldz, tdy, tdz, yi, zi, dyi, dzi;
   struct HardEdgeSourceType *x;

   /*  x= (struct HardEdgeSourceType *) &(y->Quelle.HardEdgeSource); */
   x= (struct HardEdgeSourceType *)y->Quellep;
   tdy= x->divy/ 1000.0;
   tdz= x->divz/ 1000.0; 
   ly = (x->iy == 1)  ? 0.0 : (x->disty / (x->iy- 1.0)); 
   lz = (x->iz == 1)  ? 0.0 : (x->distz / (x->iz- 1.0)); 
   ldy= (x->idy == 1) ? 0.0 : (tdy / (x->idy- 1.0)); 
   ldz= (x->idz == 1) ? 0.0 : (tdz / (x->idz- 1.0)); 
   rnumber= 0;

   yi=(x->iy- 1.0)/ (-2.0)* ly;  
   for(i= 0; i< x->iy; i++)
   {                            
      zi= (x->iz- 1.0)/(-2.0)* lz;  
      for(j= 0; j< x->iz; j++)
      { 
         dyi= (x->idy- 1.0)/ (-2.0)* ldy;  
         for (k= 0; k< x->idy; k++)
         { 
            dzi= (x->idz- 1.0)/ (-2.0)* ldz;  
            for(l= 0; l< x->idz; l++)
            { 
               y->SourceRays[rnumber].y = yi;  y->SourceRays[rnumber].z = zi;   
               y->SourceRays[rnumber].dy= dyi; y->SourceRays[rnumber].dz= dzi; 
               y->SourceRays[rnumber].phi= 0.0; 
               dzi+= ldz; rnumber++;
            }  
            dyi+= ldy;
         }                  
         zi+= lz;
      } 
      yi+= ly;
   }
}   /*end */                                                 

/***********************************************************************/
/* einfaches Modell einer Undulator Quelle an BESSY II   19.3.96       */
/* erweitert auf SLS                  Nov/Dez. 1998                    */
/* emittance einbezogen                                                */
/***********************************************************************/ 
void MakeUndulatorSource(struct RTSourceType *y, char high)       
/* modification: 24 Sep 97 09:33:17 flechsig */
/* modification: 21 Oct 97 07:44:25 flechsig */
/* 24.11.98, 3.12.98, 15.12.98  UF           */
{
   int i;                          
   double beugung, zweipi, deltax, deltaz, sigdy, sigdz,
          sigey, sigez, sigedz, sigedy, factsig, factsigp;   /* e- emittance */
   time_t zeit;
   struct UndulatorSourceType  *x;  
   struct UndulatorSource0Type *x0;  
   
   x=  (struct UndulatorSourceType *)  y->Quellep;  
   x0= (struct UndulatorSource0Type *) y->Quellep;
   time(&zeit); srand(zeit);                 /* Random initialisieren */
   zweipi= 2.0* PI;                          /* 8.0* atan(1.0);       */
   deltax= 0.0;                              

   switch (high)
   {
     case 'H': 	               /* Bessy II langes gerades Stueck high beta */
       printf("BESSY II undulator in high beta section\n");
       sigez  = 0.31;                                                /* mm */
       sigey  = 0.021;             /* 3% Kopplung ohne Beugungsbegrenzung  */ 
       sigedy = 7.1e-6;
       sigedz = 1.7e-5;                    
     break;
     case 'L':
       /* Uwe 29.3.99 */
       printf("SLS undulator, long straight, lambda= %f mm\n", x->lambda);
       sigez   = (fabs(x->deltaz) > 1e-6) ? 0.165  : 0.1404;         /* mm */
       sigey   = (fabs(x->deltaz) > 1e-6) ? 0.0185 : 0.017;    
       deltax  = 5000.0;                                    /* Quellabstand */  
       sigedz  = (fabs(x->deltaz) > 1e-6) ? 2.9e-5  : 3.38e-5;   
       sigedy  = (fabs(x->deltaz) > 1e-6) ? 2.59e-6 : 2.79e-6;  
     break;
     case 'M':
       printf("SLS undulator, medium straight\n");
       sigez   = (fabs(x->deltaz) > 1e-6) ? 0.107  : 0.093;          /* mm */
       sigey   = (fabs(x->deltaz) > 1e-6) ? 0.0148 : 0.0143;
       deltax  = 2000.0;                                    /* Quellabstand */ 
       sigedz  = (fabs(x->deltaz) > 1e-6) ?  4.5e-5 : 5.1e-5;   
       sigedy  = (fabs(x->deltaz) > 1e-6) ? 3.24e-6 : 3.31e-6; 
     break;
     case 'G':
       printf("Generic Undulator\n");
       sigez   = x0->sigmaez;          /* mm */
       sigey   = x0->sigmaey;
       sigedz  = (x0->sigmaedz)/1000.;  /* rad */   
       sigedy  = (x0->sigmaedy)/1000.;  /* rad */
     break;
     default:
       printf("BESSY II undulator in low beta section\n");
       sigez  = 0.076; 
       sigey  = 0.015;
       sigedy = 7.1e-6;
       sigedz = 1.7e-5;  
     break;
   }

/******** hier gibt es verschieden Definitionen **********************/
/*** Coisson factor wegen energyspread ??  */
//   factsig=0.21;              /* = 0.210 */
//   factsigp=sqrt(0.34);       /* = 0.583 */
   
/*** fitten von Gaussglocken, Buch von Elleaume oder Rechnungen mit WAVE **/
// Onuki, H. & Elleaume, P. (2003). Undulators, Wigglers and Their
// Applications. London: Taylor and Francis.
   factsig= sqrt(2.)/zweipi;    /* = 0.225 */
   factsigp= 1./sqrt(2.);       /* = 0.707 */
   
   beugung    = sqrt(x->lambda* x->length)* factsig;
   x->sigvert = sqrt(sigey* sigey + beugung * beugung);
   x->sighor  = sqrt(sigez* sigez + beugung * beugung);
/********************************************************************/
   sigdy= sqrt(factsigp*factsigp * x->lambda/ x->length + sigedy* sigedy);
   sigdz= sqrt(factsigp*factsigp * x->lambda/ x->length + sigedz* sigedz);
   i= 0;
   
   if (fabs(x->deltaz) > 1e-6)                     /* SLS 1.0 Sonderfall SIM */
   {
      printf("MakeUndulatorSource: special: SLS 1.0 double undulator configuration\n");
      printf("                     deltaz= %f, deltax= %f\n", x->deltaz, deltax);
      deltax *= 0.5;
      deltaz= x->deltaz* 0.5;	
      while (i < y->raynumber)
      {
      /* Strahlen werden paarweise erzeugt */
        
         y->SourceRays[i].dy = gauss(sigdy); 
         y->SourceRays[i].dz = gauss(sigdz); 
	 y->SourceRays[i].y  = gauss(x->sigvert)+ deltax* y->SourceRays[i].dy;
         y->SourceRays[i].z  = gauss(x->sighor) + deltaz+
                               deltax* y->SourceRays[i].dz;   /**/
         y->SourceRays[i].phi= 0.0;
         i++;
         /* zweiter Undulator */
         y->SourceRays[i].dy = gauss(sigdy); 
         y->SourceRays[i].dz = gauss(sigdz); 
         y->SourceRays[i].y  = gauss(x->sigvert)- deltax* y->SourceRays[i].dy;
         y->SourceRays[i].z  = gauss(x->sighor) - deltaz- 
                               deltax* y->SourceRays[i].dz;   /**/
         y->SourceRays[i].phi= 0.0;
         i++;
      }
   }    
   else
      while (i < y->raynumber)                    /* einzelner Undulator */
      {
        y->SourceRays[i].dy = gauss(sigdy); 
        y->SourceRays[i].dz = gauss(sigdz); 
        y->SourceRays[i].y  = gauss(x->sigvert);  /**/   
        y->SourceRays[i].z  = gauss(x->sighor);   /**/
        y->SourceRays[i].phi= 0.0;
        i++;
      } /* end while */
    /* end if */  
} /* end UndulatorSource() */

/***********************************************************************/
/* Modell einer Punkt Quelle Eingaben alles Sigma Werte,               */
/***********************************************************************/ 
void MakePointSource(struct RTSourceType *y)       
{
   int i; 
   double zz, tdy, tdz;
   time_t zeit;
   struct PointSourceType *x;  
   
   x= (struct PointSourceType *) y->Quellep;
  
   time(&zeit); srand(zeit);
   tdy= x->sigdy/ 1000.0; 
   tdz= x->sigdz/ 1000.0; 
   i= 0;
   while (i < y->raynumber)
   {
      zz= uRandom(tdz);  
      y->SourceRays[i].y = gauss(x->sigy);     
      y->SourceRays[i].z = gauss(x->sigz); 
      y->SourceRays[i].dy= gauss(tdy); 
      y->SourceRays[i].dz= gauss(tdz);    
      y->SourceRays[i].phi= 0.0;
      i++;
   }
} 

/***********************************************************************/
/* Modell einer Dipol Quelle Eingaben als Sigma Werte, die horizontale */
/* Divergenz ist hard edge 		    19.3.96      	       */
/***********************************************************************/ 
void MakeDipolSource(struct RTSourceType *y)       
{
   int i; 
   double zz, tdy, tdz;
   time_t zeit;
   struct DipolSourceType *x;  

   x= (struct DipolSourceType *) y->Quellep;  
   time(&zeit); srand(zeit);
   tdy= x->sigdy/ 1000.0; 
   tdz= x->dz/ 1000.0; 
   i= 0;
   while (i < y->raynumber)
   {
      zz= uRandom(tdz);  
      y->SourceRays[i].y = gauss(x->sigy);     
      y->SourceRays[i].z = gauss(x->sigz); 
      y->SourceRays[i].dy= gauss(tdy); 
      y->SourceRays[i].dz= zz- tdz/ 2.0;    
      y->SourceRays[i].phi= 0.0;
      i++;
   }
} /* end MakePointSource */ 


/**************************************************************************/
/* Modell einer Ring Quelle dy, dz sind die Halbachsen einer elliptischen */
/* Divergenz Verteilung- y,z sind immer null,                             */
/* der erste Strahl zeigt nach 45 grad 		      15.12.2007          */
/**************************************************************************/ 
void MakeRingSource(struct RTSourceType *y)       
{
   int i; 
   double a, b, t, dt;
   struct RingSourceType *x;  

   x= (struct RingSourceType *) y->Quellep;  
  
   b= x->dy/ 1000.0; /* grosse kleine halbachse */
   a= x->dz/ 1000.0; 
  
   dt= 2.0* PI/y->raynumber;

   i= 0;
   while (i < y->raynumber)
   {
     t= i* dt+ PI/4.0;  /* wegen Optimierung 1 Strahl reicht (manchmal) */
      y->SourceRays[i].y = 0.0;     
      y->SourceRays[i].z = 0.0; 
      y->SourceRays[i].dy= b* cos(t); 
      y->SourceRays[i].dz= a* sin(t);    
      y->SourceRays[i].phi= 0.0;
      i++;
   }
} /* end MakeRingSource */ 


/* erzeugt Gauss verteilte Zufallszahl zw. +- 6 sigma                   */
/* max auf 1 gesetzt wegen Geschwindigkeitsvorteil 10.5.96              */
                                                                      
double gauss(double sigma)
/* erzeugt normalverteilte Zufallszahl 	*/
/* Uwe 11.7.96 				*/ 
{
   double zz1, zz2, w, zweisigmamalsigma, zwoelfsigma, sechssigma;

   zz1= 0.0;
   if (sigma > 0)
   {
     zweisigmamalsigma=  2.0* sigma* sigma;
     zwoelfsigma      = 12.0* sigma;
     sechssigma       =  6.0* sigma;

     do 
     {
        zz1= uRandom(zwoelfsigma)- sechssigma;              /* +- 6 sigma */
        zz2= uRandom(1.0);                                  
        w  = exp(-(zz1 * zz1)/ zweisigmamalsigma );        
     } while ((w - zz2) <= 0.0);
   }
   return  zz1;
}                                                        

int MakeRTSource(struct PHASEset *xp, struct BeamlineType *bl) 
{
  /*struct RayType *rays;*/
   struct UndulatorSourceType  *up;
   /*   struct UndulatorSource0Type *up0; */
   struct SRSourceType         *sp;
   struct FileSourceType       *fp;
   /* 24.11.06 */
   /* UF , TL ausschalten ueber switch 15.5. 07 */

#ifdef DEBUG
   printf("MakeRTSource start: beamlineOK: %X, raynumber: %d\n", bl->beamlineOK, bl->RTSource.raynumber);
#endif

   if (bl->RTSource.QuellTyp == 'I')
     {
       printf("MakeRTSource: select a GO source (you slected PO image plane) - return\n");
       return 0;
     }

   if (bl->localalloc == DOALLOC) 
     {
       
#ifdef DEBUG
   printf("MakeRTSource: realloc source\n");
#endif


       bl->RTSource.SourceRays= XREALLOC(struct RayType, 
					 bl->RTSource.SourceRays, 
					 bl->RTSource.raynumber);
     }
     
   /* hier gibt es bestimmt eine elegantere Loesung */
   switch (bl->RTSource.QuellTyp)
     {
     case 'U':
     case 'u':  
     case 'L':
     case 'M':
       /* Uwe 29.3.99 setzt die Wellenlaenge in der Undulator 
	  Structur noch mal explizit (optimierung) */
       up= (struct UndulatorSourceType *) bl->RTSource.Quellep;
       up->lambda= bl->BLOptions.lambda;
       break;
     }

   switch (bl->RTSource.QuellTyp)
     {
     case 'F':
       fp= (struct FileSourceType *)bl->RTSource.Quellep;
       ReadRayFile(fp->filename, &bl->RTSource.raynumber, 
		  &bl->RESULT); 
      
       if (bl->localalloc == DOALLOC) 
	 {
	   bl->RTSource.SourceRays= XREALLOC(struct RayType, 
					     bl->RTSource.SourceRays, 
					     bl->RTSource.raynumber);
	 }

       /* UF weiss nicht ob das noch OK ist UF 28.11.06 */
       
      memcpy(bl->RTSource.SourceRays, bl->RESULT.RESp,
      sizeof(struct RayType)* bl->RTSource.raynumber); 
       break;
     case 'U':
       MakeUndulatorSource(&(bl->RTSource), 'H');       /* high beta */
       break; 
     case 'u':
       MakeUndulatorSource(&(bl->RTSource), 'l');       /* low beta */
       break; 
     case 'L':
       MakeUndulatorSource(&(bl->RTSource), 'L');       /* SIS */
       break; 
     case 'M':
       MakeUndulatorSource(&(bl->RTSource), 'M');       /* SIM */
       break; 
     case 'G':
       MakeUndulatorSource(&(bl->RTSource), 'G');       /* generic */
       break; 
     case 'D':
       MakeDipolSource(&(bl->RTSource));       
       break; 
     case 'o':
       MakePointSource(&(bl->RTSource));       
       break; 
     case 'R':
       MakeRingSource(&(bl->RTSource));       
       break; 
     case 'S':   /* single ray */
       sp= (struct SRSourceType *)bl->RTSource.Quellep;
       bl->RTSource.SourceRays->y  = sp->y; 
       bl->RTSource.SourceRays->z  = sp->z; 
       bl->RTSource.SourceRays->dy = sp->dy/ 1000.0; 
       bl->RTSource.SourceRays->dz = sp->dz/ 1000.0;
       bl->RTSource.SourceRays->phi= 0.0; 
       break;
     default:
       MakeHardEdgeSource(&(bl->RTSource));       	 
       break;
     }
   bl->beamlineOK |= sourceOK;
   if (bl->BLOptions.wrSource == 1)
   {
     printf("write RT- source to file %s\n", xp->sourceraysname);
     
     WriteRayFile(xp->sourceraysname, &bl->RTSource.raynumber, 
		  bl->RTSource.SourceRays); 

   }
#ifdef DEBUG
   printf("MakeRTSource   end: beamlineOK: %X, raynumber: %d\n", bl->beamlineOK, bl->RTSource.raynumber);
#endif
   /* 2.5.96 free(bl->RTSource.SourceRays);           */
   return 1;
}  /* end makertsource */


int OnElement(struct mdatset *mi, double w, double l) 
/* Uwe 1.7.96 					*/
/* testet ob ein ray auf dem element ist 	*/
{
   int on;
   on= ((mi->w1 <= w) && (w <= mi->w2) && 
        (mi->l1 <= l) && (l <= mi->l2)) ? 1 : 0;
   return on;
} /* end OnElement */

/* for cpp we have a different version in singleray.cpp */
void RayTraceSingleRay(struct BeamlineType *bl)
{
  struct RayType *Raysin, *Raysout, Tmpsource, Tmpresult;   
  int      elnumber;
  unsigned int elcounter;
  double uu, ww, ll, xlength, xlength1, xlength2, phase, raylen, 
    slopelen, dela, res, dphase;
  struct ElementType *ds; 
  struct RESULTType  *Re; 

  /*********************************************************************/

  fprintf(stderr, "RayTraceSingleRay: beamlineOK: %X\n", bl->beamlineOK); 
  Re= &bl->RESULT;   

  if ((bl->beamlineOK & (sourceOK | mapOK)) == 0)
    { 
      fprintf(stderr, "rtrace.c: beamline is not OK: beamlineOK: %X\n", 
	      bl->beamlineOK);       /*  exit(-1); */ 
    } else  
      {
	Re->points1= 1;
	Re->typ= PLrttype; 
	raylen= phase= 0.0;
	memcpy(&Tmpsource, bl->RTSource.SourceRays, sizeof(struct RayType));
			        	
	/* Schleife ueber die elemente */
	elcounter= 0;
	printf("\n**************** Single Ray *****************\n");

	if (bl->BLOptions.SourcetoImage != 1) 
	  printf("RayTraceSingleRay:   Image to source calculation.\n");
	else 
	  printf("RayTraceSingleRay:   Source to image calculation.\n");

	printf("RayTraceSingleRay:   Beamline contains %u element(s).\n", 
	       bl->elementzahl);

	while (elcounter< bl->elementzahl)
	  {                               /* Richtung beruecksichtigen */
	    elnumber= (bl->BLOptions.SourcetoImage == 1) ?
	      elcounter+ 1 : bl->elementzahl- elcounter; 
	    printf("\n==>element number %d ==> results:\n", elnumber);
	    ds= &(bl->ElementList[elnumber- 1]);
	    
	    /* ab hier wird ein voller rt durchgefuehrt */
	    Raysin= &Tmpsource; Raysout= &Tmpresult;
	    if (ds->MDat.Art == kEOESlit) /* Sonderbehandlung Spalt */
	      {
		uu = 0.0; ww= Raysin->y; ll= Raysin->z;
	      }
	    else


	    intersection(&ds->mir, ds->wc, ds->xlc, Raysin, 
			 &uu, &ww, &ll, &bl->BLOptions.ifl.iord); 


	    printf("  intersection: u= %.4g (mum), w= %.4g (mm), l= %.4g (mm)\n", 
		   uu* 1e3 , ww, ll);
	    if (OnElement(&ds->MDat, ww, ll) == 0)
	      {
		beep(1);
		printf("!!! ray got lost on element number %d ==> exit\n", 
		       elnumber);
		/* ich muss Re->RESUnion.Rays irgendwie initialisieren */
		memcpy(Re->RESp, &Tmpsource, sizeof(struct RayType)); 
		elcounter= bl->elementzahl+ 1; /* Abbruch */
	      }
	    else
	      {
		if (ds->MDat.Art == kEOESlit) /* Sonderbehandlung Spalt */
		  {
		    memcpy(Raysout, Raysin, sizeof(struct RayType));
		    xlength1= xlength2= xlength= 0.0;
		  }
		else 
		  {

		    ray_tracef(Raysin, Raysout, &bl->BLOptions.ifl.iord, 
			       (double *)ds->ypc1, (double *)ds->zpc1, 
			       (double *)ds->dypc, (double *)ds->dzpc);
		    pathlen1(&ds->xlm, Raysin, &bl->BLOptions.ifl.iord, 
			     &xlength1, &xlength2, &xlength); 
		     
       /* if ((bl->BLOptions.CalcMod & SlopeMod) == SlopeMod) */
		    slopelen= (bl->BLOptions.SourcetoImage == 1) ? 
		      ds->GDat.rp : ds->GDat.r; 

		     Slope(Raysout, ds->MDat.slopew, ds->MDat.slopel, 
			  slopelen, ds->geo.cosb, ds->GDat.azimut); 

		  }
		/* calculate phase */
		dphase= (bl->BLOptions.lambda > 0) ? 
		  ((xlength/ bl->BLOptions.lambda)* 2.0* PI) : (2.0* PI);
   
		raylen+= xlength;
                phase += dphase;
                
		/* 2PI entspricht lambda= 0 */
		/* Ausgabe */

		printf("  ray trace: \n"); 
	        printf("    yi : % .4g,\tyo : % .4g\t (mm)\n", 
		       Raysin->y, Raysout->y);  
		printf("    zi : % .4g,\tzo : % .4g\t (mm)\n", 
		       Raysin->z, Raysout->z);  
		printf("    dyi: % .4g,\tdyo: % .4g\t (mrad)\n", 
		       Raysin->dy  * 1e3, Raysout->dy * 1e3);    
		printf("    dzi: % .4g,\tdzo: % .4g\t (mrad)\n", 
		       Raysin->dz * 1e3, Raysout->dz  * 1e3); 
		
		printf("  pathlength over element: %.4g, \ttotal: %.4g \t(nm)\n", 
		          xlength* 1e6, raylen* 1e6);
		printf("  time delay over element: %.4g, \ttotal: %.4g \t(fs)\n", 
		          xlength* 1e15/LIGHT_VELO, raylen* 1e15/LIGHT_VELO);
		printf("  phaseshift over element: %.4g, \ttotal: %.4g \t(rad)\n",
		          dphase, phase);
#ifdef DEBUG
		printf("      debug: xlength1: %g, xlength2: %g\n", 
		       xlength1, xlength2);    
		printf("             slopelength: %g mm, wavelength: %g nm\n", 
		       slopelen, bl->BLOptions.lambda);       
#endif		
		memcpy(&Tmpsource, &Tmpresult, sizeof(struct RayType));
		memcpy(Re->RESp, &Tmpresult, sizeof(struct RayType)); 
		elcounter++;
	      }
	  } /* end while  */
	/*	if (bl->deltalambdafactor < 1e12)     wurde neu gesetzt */
	  {
	    printf("  energy resolution: \t");
	    dela= Raysout->y * bl->deltalambdafactor* 1e6;
	    if (fabs(dela) < 1e-18)
		{
		  printf("infinity\n");
	          printf("     DeltaLambda= 0.0 nm ==> Lambda= %g (nm)\n", 
			 bl->BLOptions.lambda * 1e6);
		} 
	      else
		{
		  res= fabs(bl->BLOptions.lambda/ dela);
		  printf("%.4g\n", res);
	          printf("     DeltaLambda= %g nm ==> Lambda= %g nm\n", 
			 dela, bl->BLOptions.lambda+ dela);
		}
	  }
	bl->beamlineOK|= resultOK; /* resultrays in memory */
      } /* beamline OK*/
  printf("********** end RayTraceSingleRay *******************\n\n"); 
} /* end RayTraceSingleRay */

void RayTracec(struct BeamlineType *bl)
/* normal RT                             */
/* umgeschrieben auf pointer UF 28.11.06 */
/* phaseset wird nicht mehr benutzt      */
/* erweitert auf mehrere raysets Jun 12  */
{
  struct RayType *Raysin, *Raysout;   
  int i, set;
  struct RESULTType *Re; 

  bl->beamlineOK &= ~resultOK;

  /*********************************************************************/
#ifdef DEBUG
  fprintf(stderr, "RayTracec start: beamlineOK: %X, expect: %X, act_ray_set: %d\n", 
	  bl->beamlineOK, (sourceOK | mapOK), bl->BLOptions.act_ray_set); 
#endif
 
  Re= &bl->RESULT;   
  if ((bl->beamlineOK & (sourceOK | mapOK)) != (sourceOK | mapOK))
    { 
      fprintf(stderr, "RayTracec: beamline is not OK: beamlineOK: %X != %X\nwe do nothing\n", 
	      bl->beamlineOK, (sourceOK | mapOK));       
      return;
    } 
  
  Re->points1= bl->RTSource.raynumber;
  Re->typ   = PLrttype;  
    
#ifdef DEBUG	
  printf("debug: RayTracec: calculate %d ray(s), source: %d rays\n", Re->points1, bl->RTSource.raynumber);
#endif 

  Raysin = bl->RTSource.SourceRays; 
  Raysout= (struct RayType *)Re->RESp; 

  if (bl->BLOptions.act_ray_set == 2)
     {
       Re->points2= bl->RTSource.raynumber;
       for (i= 0; i < Re->dim1; i++) Raysout++;
     }
   else
     Re->points1= bl->RTSource.raynumber;
     
   
  for (i= 0; i< bl->RTSource.raynumber; i++ )
    { 
      
      ray_tracef(Raysin, Raysout, &bl->BLOptions.ifl.iord, 
		 (double *)bl->ypc1, (double *)bl->zpc1, 
		 (double *)bl->dypc, (double *)bl->dzpc); 
      
      Raysout->phi= Raysin->phi;
      Raysin++, Raysout++;
    }
  /*   free(raysin);      */
  bl->beamlineOK |= resultOK; /* resultrays in memory */
  
#ifdef DEBUG1
  printf("RayTracec:   end: beamlineOK: %X\n", bl->beamlineOK); 
#endif
}

/********************************************************/

void RayTraceFull(struct BeamlineType *bl)  

/********************************************************/
/* macht komplettes ray trace mit clipping und slope 	*/
/* Uwe 1.7.96 						*/
/********************************************************/
{
   struct RayType *Raysin, *Raysout, *tmpsource, *tmpresult;   
   int i, lost, zahl, elnumber, mypoints;
   unsigned int elcounter;
   double uu, ww, ll, xlength, xlength1, xlength2, dphase, slopelen;
   struct ElementType *ds; 
   struct RESULTType *Re; 
   /*********************************************************************/
   
   bl->beamlineOK &= ~resultOK;
   fprintf(stderr, "RayTraceFull: beamlineOK: %X, iord=%d\n", 
	   bl->beamlineOK, bl->BLOptions.ifl.iord); 
   Re= &bl->RESULT;

   if ((bl->beamlineOK & (sourceOK | mapOK)) == 0)
   { 
     fprintf(stderr, "rtrace.c: beamline is not OK: beamlineOK: %X\n", 
	     bl->beamlineOK); 
     return;
   } 

   mypoints= zahl= bl->RTSource.raynumber;
   
   tmpsource= XMALLOC(struct RayType, mypoints);
   tmpresult= XMALLOC(struct RayType, mypoints);
   
   printf("RayTraceFull: start with \t\t%d ray(s) \t= 100 %s \n", zahl, "%"); 
   memcpy(tmpsource, bl->RTSource.SourceRays, zahl* sizeof(struct RayType));
   
   elcounter= 0;
   
   while ((elcounter < bl->elementzahl) && (zahl > 1))
     {
       lost= 0;
       elnumber= (bl->BLOptions.SourcetoImage == 1) ?
	 elcounter+ 1 : bl->elementzahl- elcounter; 
       ds= &(bl->ElementList[elnumber-1]); 
       Raysin= tmpsource; Raysout= tmpresult;  
       
       if (ds->MDat.Art == kEOESlit) /* Sonderbehandlung Spalt */
	 {
	   printf("RayTraceFull: aperture/ slit in interface plane\n");
	   for (i= 0; i< zahl; i++)
	     { 
	       ww= Raysin->y; ll= Raysin->z;
	       if (OnElement(&ds->MDat, ww, ll))
		 memcpy(Raysout++, Raysin, sizeof(struct RayType));
	       else lost++; 
	       Raysin++; 
	     } /* schleife ueber alle rays */
	 }
       else 
	 {
	   slopelen= (bl->BLOptions.SourcetoImage == 1) ? 
	     ds->GDat.rp : ds->GDat.r; 
	   for (i= 0; i< zahl; i++)
	     { 
	       intersection(&ds->mir, ds->wc, ds->xlc, Raysin, 
			    &uu, &ww, &ll, &bl->BLOptions.ifl.iord); 
	       
	       if (OnElement(&ds->MDat, ww, ll))
		 {
		   ray_tracef(Raysin, Raysout, &bl->BLOptions.ifl.iord, 
			      (double *)ds->ypc1, (double *)ds->zpc1, 
			      (double *)ds->dypc, (double *)ds->dzpc);  
		   Slope(Raysout, ds->MDat.slopew, ds->MDat.slopel, slopelen,  
			 ds->geo.cosb, ds->GDat.azimut);
		   pathlen1(&ds->xlm, Raysin, &bl->BLOptions.ifl.iord, 
			    &xlength1, &xlength2, &xlength);  
		   
		   /* calculate phase */
		   dphase= (bl->BLOptions.lambda > 0) ? 
		     ((xlength/ bl->BLOptions.lambda)* 2.0* PI) : (2.0* PI);
		   
		   Raysout->phi= Raysin->phi+ dphase;
		   
		   Raysout++;  
		 } else lost++;
	       Raysin++; 
	     } /* schleife ueber alle rays */
	 } /* end Sonderbehandlung spalt */
       zahl-= lost;
       printf("   element %d: lost \t%d, residual \t%d  ray(s) \t= %d %s \n", 
	      elnumber, lost, zahl, zahl*100/bl->RTSource.raynumber, "%"); 
       /* verzichte hier auf realloc */
       memcpy(tmpsource, tmpresult, zahl* sizeof(struct RayType));  
       elcounter++;
     } /* schleife ueber elemente */
   XFREE(tmpsource); 
   
   if (bl->BLOptions.act_ray_set == 2)
     {
       Re->points2= zahl;
       Raysout= Re->RESp;
       for (i= 0; i < Re->dim1; i++) Raysout++; /* copy after dim1 */
       memcpy(Raysout, tmpresult, zahl* sizeof(struct RayType));
       printf("RayTraceFull: second set ==> done\n");
     }
   else
     {
        Re->points1= zahl;
	memcpy(Re->RESp, tmpresult, zahl* sizeof(struct RayType));
	printf("RayTraceFull: first set ==> done\n");
     }
   
   if (zahl > 0) bl->beamlineOK |= resultOK; 
    
   XFREE(tmpresult);
   /* resultrays in memory */
   
   /* printf("RayTraceFull: ==> done\n"); */
}  /* end raytracefull */

void Slope(struct RayType *ray, double slopew, double slopel, double xlen, 
	   double cosb, int azimut)
/* simuliert slope errors durch Winkelaenderung am Strahl 	*/
/* slope gegeben in arcsec- rms 				*/
/* Uwe 1.7.96 */
/* Fehler beseitigt 3.12.98 UF */
     /* azimut eingefuegt 19.2.99 */
{
   double wsl, lsl;

   //   printf("debug: call to slope: %f, %f, %f, %f, %f, %f\n", ray->y, ray->z, ray->dy, ray->dz, slopew, slopel);


   wsl= gauss(slopew)* PI/ 324000.0;	/* 2* PI/(3600.* 180.);       */
   lsl= gauss(slopel)* PI/ 324000.0;    /* 2- da doppelter winkel     */
   /* fallunterscheidung notwendig */

   //  printf("wsl= %f, lsl= %f\n", wsl, lsl);

   if ((azimut == 1) || (azimut == 3))  /* horizontale Ablenkung */
   {
     ray->y+= xlen * lsl* cosb;         /* naehere tan(wsl) durch wsl */
     ray->z+= xlen * wsl;
     ray->dy+= lsl * cosb; 
     ray->dz+= wsl;
   } 
   else
     {
       ray->y+= xlen * wsl;                 /* naehere tan(wsl) durch wsl */
       ray->z+= xlen * lsl* cosb;
       ray->dy+= wsl; 
       ray->dz+= lsl * cosb; 
     }
   //  printf("debug: return from slope: %f, %f, %f, %f\n", ray->y, ray->z, ray->dy, ray->dz);
} /* end slope */


void WriteRayFile(char *name, int *zahl, struct RayType *Rp) 
/* wird von MakeRTSource gerufen 				*/
/* Parameter: filename, number of rays, vektor mit rays         */ 
/* last mod. Uwe 13.8.96 					*/
{
    FILE *f;
    int i;                                                
    
    if ((f= fopen(name, "w+")) == NULL)
    {
       fprintf(stderr, "error: open file %s\n", name); exit(-1);   
    } else
    {  					/* 12.3.96 filesystemAenderung */
       printf("  write rays to %s", name);     
       fprintf(f, "%d %d\n", *zahl, 0);    
      
       for (i= 0; i< *zahl; i++)
           fprintf(f, "% .6lE % .6lE % .6lE % .6lE % .6lE\n", 
		Rp[i].y, Rp[i].z, Rp[i].dy, Rp[i].dz, Rp[i].phi);         
       fclose(f);   
       printf("  --> done\n");     
    }
}  /* end WriteRayFile */

void WriteRayFileHdf5(char *fname, struct RESULTType *Res) 
{
  hid_t  file_id, e_dataspace_id, e_dataset_id;
  double *yvec1, *zvec1, *dyvec1, *dzvec1, *yvec2, *zvec2, *dyvec2, *dzvec2;
  int i;
  struct RayType *Rp;
  
  OUTDBGC("202308 NEW function: not yet debugged");

  Rp= (struct RayType *)Res->RESp;
  printf("points1= %d, points2= %d\n", Res->points1, Res->points2); 

  /* Create a new file using default properties. */
  /* specifies that if the file already exists, 
     the current contents will be deleted so that the application can rewrite the file with new data. */
  file_id= H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  if (file_id < 0)
    {
      fprintf(stderr, "error: can't open %s - exit\n", fname);
      exit(-1);
    }

  yvec1  = XMALLOC(double, Res->points1);
  zvec1  = XMALLOC(double, Res->points1);
  dyvec1 = XMALLOC(double, Res->points1);
  dzvec1 = XMALLOC(double, Res->points1);
  
  for (i= 0; i< Res->points1; i++)
    {
      yvec1[i] = Rp[i].y* 1e3;
      zvec1[i] = Rp[i].z* 1e3;
      dyvec1[i]= Rp[i].dy;
      dzvec1[i]= Rp[i].dz;
    }
 
  writeDataDouble(file_id, "/y_vec1",  yvec1,  Res->points1, "y vector", "m");
  writeDataDouble(file_id, "/z_vec1",  zvec1,  Res->points1, "z vector", "m");
  writeDataDouble(file_id, "/dy_vec1", dyvec1, Res->points1, "dy vector", "rad");
  writeDataDouble(file_id, "/dz_vec1", dzvec1, Res->points1, "dz vector", "rad");

  XFREE(yvec1);
  XFREE(zvec1);
  XFREE(dyvec1);
  XFREE(dzvec1);

   
  if (Res->points2 > 0)
    {
      yvec2  = XMALLOC(double, Res->points2);
      zvec2  = XMALLOC(double, Res->points2);
      dyvec2 = XMALLOC(double, Res->points2);
      dzvec2 = XMALLOC(double, Res->points2);
  
      for (i= 0; i< Res->points2; i++)
	{
	  yvec2[i] = Rp[Res->points1+ i].y* 1e3;
	  zvec2[i] = Rp[Res->points1+ i].z* 1e3;
	  dyvec2[i]= Rp[Res->points1+ i].dy;
	  dzvec2[i]= Rp[Res->points1+ i].dz;
	}
 
      writeDataDouble(file_id, "/y_vec2",  yvec2,  Res->points2, "y vector  + deltalambda", "m");
      writeDataDouble(file_id, "/z_vec2",  zvec2,  Res->points2, "z vector  + deltalambda", "m");
      writeDataDouble(file_id, "/dy_vec2", dyvec2, Res->points2, "dy vector + deltalambda", "rad");
      writeDataDouble(file_id, "/dz_vec2", dzvec2, Res->points2, "dz vector + deltalambda", "rad");

      XFREE(yvec2);
      XFREE(zvec2);
      XFREE(dyvec2);
      XFREE(dzvec2);
    } // end 2nd set
  add_string_attribute_f(file_id, "/", "file_type", "ray_hdf5");
  H5Fclose(file_id);

  printf("wrote phase_hdf5 file: %s\n", fname);
} // end WriteRayFileHdf5

/* reserves memory for a RTSource */
/* use the XREALLOC macro         */
void AllocRTSource(struct BeamlineType *bl)
{
  switch (bl->RTSource.QuellTyp)
    {
    case 'G':
      bl->RTSource.Quellep= 
	XREALLOC(struct UndulatorSource0Type, bl->RTSource.Quellep, 1);
      break;
    case 'L':
    case 'l':
    case 'M':
    case 'U': 
    case 'u': 
      bl->RTSource.Quellep= 
	XREALLOC(struct UndulatorSourceType, bl->RTSource.Quellep, 1);
      break;
    case 'D':
      bl->RTSource.Quellep= 
	XREALLOC(struct DipolSourceType, bl->RTSource.Quellep, 1);
      break;
    case 'o':
      bl->RTSource.Quellep= 
	XREALLOC(struct PointSourceType, bl->RTSource.Quellep, 1);
      break;
    case 'R':
      bl->RTSource.Quellep= 
	XREALLOC(struct RingSourceType, bl->RTSource.Quellep, 1);
      break;
    case 'H':
      bl->RTSource.Quellep= 
	XREALLOC(struct HardEdgeSourceType, bl->RTSource.Quellep, 1);
      break; 
    case 'S':
      bl->RTSource.Quellep= 
	XREALLOC(struct SRSourceType, bl->RTSource.Quellep, 1);
      break; 
    case 'I':
      bl->RTSource.Quellep= 
	XREALLOC(struct PSImageType, bl->RTSource.Quellep, 1);
      break; 
    case 'F':
      bl->RTSource.Quellep= 
	XREALLOC(struct FileSourceType, bl->RTSource.Quellep, 1);
      break; 
    case 'P':
      printf("AllocRTSource: >>P<< (PhaseSpaceSource) no memory reserved\n");
      break; 

    default: fprintf(stderr, 
		     "AllocRTSource: error: unknown source type! %c \n", 
		     bl->RTSource.QuellTyp);
             fprintf(stderr, 
		     "AllocRTSource: set sourcetype to >>H<<\n");
	     bl->RTSource.QuellTyp='H';
	     AllocRTSource(bl);
      
    }
} /* AllocRTSource */

/* frees and reserves memory for a RESULT    */
/* use the XMALLOC macros in common.h        */
/* dims is a field with dimensions           */
/* in rt mode only dim1 is used              */
/* ps mode: dim1: iy, dim2: iz               */ 
void ReAllocResult(struct BeamlineType *bl, int newtype, int dim1, int dim2)
{
  struct PSDType *PSDp;
  int ii, iy, iz, type;                   /* to make the code clearer */

#ifdef DEBUG 
  printf("debug: ReAllocResult, newtype= %d, file=%s\n", newtype, __FILE__);
#endif

  FreeResultMem(&bl->RESULT); 

#ifdef DEBUG 
  printf("debug: start allocating\n");
#endif

  type = newtype & ~1; // strip off last bit
  
  switch (type)
    {
    case PLrttype:
      ii= (newtype & 1) ? 2 * dim1 : dim1;   /* deltalambda mode */
      bl->RESULT.RESp= XMALLOC(struct RayType, ii);
      bl->RESULT.dim1= dim1;
#ifdef DEBUG
      printf("allocate %d\n", ii);
#endif
      break;
    case PLphspacetype:
      printf("info: obsolete call to ReAllocResult, file=%s\n", __FILE__);
      break;
    default: 
      fprintf(stderr, 
	      "AllocResult: error: unknown type! %c -> exit\n", 
	      bl->RESULT.typ);
      exit(-1);
    }
  bl->RESULT.typ= newtype;
#ifdef DEBUG 
  printf("\ndebug %s AllocResult, pointer bl->RESULT.RESp=0x%x\n", __FILE__, (long)bl->RESULT.RESp);
#endif
} /* AllocResult */

void FreeResultMem(struct RESULTType *Re) 
/* macht den Speicher frei */
/* uwe 8.8.96 */
{
#ifdef DEBUG  
  printf("debug: FreeResultMem called, file=%s\n", __FILE__); 
#endif
  
  if (Re->RESp == NULL)
    {
      printf("FreeResultMem: Re->RESp == NULL - nothing to free- return\n");
      return;
    }

#ifdef DEBUG1  
  printf("debug: FreeResultMem: type=%d, type=0x%X, Re->RESp=0x%x\n", Re->typ, Re->typ, (long)Re->RESp); 
#endif

  if (Re->typ & PLrttype) 
    {
      printf("debug: FreeResultMem: clean PLrttype\n");
      XFREE(Re->RESp); 
    }
  

  Re->typ= 0;
} /* end freeResultmem */

/* end rtrace.c */
