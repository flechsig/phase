/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/rtrace.c */
/*   Date      : <23 Mar 04 11:27:42 flechsig>  */
/*   Time-stamp: <19 Aug 11 10:54:44 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */


/*  UF 0804 no X11 left  */

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif 

#include <stdio.h> 
#include <stdlib.h>
#include <math.h>
#include <time.h>

    
#include "cutils.h" 
#include "phase_struct.h"
/*#include "fg3pck.h"  */
                 
   
#include "phase.h"         
#include "rtrace.h"
#include "common.h" 
 
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
   ly = (x->iy == 1) ?  0.0 : (x->disty / (x->iy- 1.0)); 
   lz = (x->iz == 1) ?  0.0 : (x->distz / (x->iz- 1.0)); 
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
       deltax = 5000.0;                                    /* Quellabstand */  
       sigedz  = (fabs(x->deltaz) > 1e-6) ? 2.9e-5  : 3.38e-5;   
       sigedy  = (fabs(x->deltaz) > 1e-6) ? 2.59e-6 : 2.79e-6;  
     break;
     case 'M':
       printf("SLS undulator, medium straight\n");
       sigez   = (fabs(x->deltaz) > 1e-6) ? 0.107  : 0.093;          /* mm */
       sigey   = (fabs(x->deltaz) > 1e-6) ? 0.0148 : 0.0143;
       deltax = 2000.0;                                    /* Quellabstand */ 
       sigedz  = (fabs(x->deltaz) > 1e-6) ?  4.5e-5 : 5.1e-5;   
       sigedy  = (fabs(x->deltaz) > 1e-6) ? 3.24e-6 : 3.31e-6; 
     break;
     case 'G':
       printf("Generic Undulator\n");
       sigez   = x0->sigmaez;          /* mm */
       sigey   = x0->sigmaey;
       sigedz  = (x0->sigmaedz)/1000.;  /* rad */   
       sigedy  = (x0->sigmaedy)/1000.;  /* rad */
       x->deltaz=0; /* kein horizontaler Versatz */
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
   factsig=0.21;              /* = 0.210 */
   factsigp=sqrt(0.34);       /* = 0.583 */
   
/*** fitten von Gaussglocken, Buch von Elleaume oder Rechnungen mit WAVE **/

   factsig=sqrt(2.)/zweipi;   /* = 0.225 */
   factsigp=1./sqrt(2.);       /* = 0.707 */
   
   beugung    = sqrt(x->lambda* x->length)* factsig;
   x->sigvert = sqrt(sigey* sigey + beugung * beugung);
   x->sighor  = sqrt(sigez* sigez + beugung * beugung);
/********************************************************************/
   sigdy= sqrt(factsigp*factsigp * x->lambda/ x->length + sigedy* sigedy);
   sigdz= sqrt(factsigp*factsigp * x->lambda/ x->length + sigedz* sigedz);
   i= 0;
   
   if (fabs(x->deltaz) > 1e-6)                     /* sls sonderfall */
   {
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

void MakeRTSource(struct PHASEset *xp, struct BeamlineType *bl) 
/* 20.5.96 
   3.7.96 phase hinzugefuegt
   Source rays werden allociert! */      
/* last modification: 24 Sep 97 09:32:34 flechsig */
/* modification: 17 Oct 97 09:21:43 flechsig */
/* 29.3.99 wellenlaenge beim Undulator explizit aus bloptions setzen */ 
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
	Re->points= 1;
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

	printf("RayTraceSingleRay:   Beamline contains %d element(s).\n", 
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

#ifdef SEVEN_ORDER
	      intersection_8(&ds->mir, ds->wc, ds->xlc, Raysin, 
			     &uu, &ww, &ll, &bl->BLOptions.ifl.iord);
#else
	    intersection(&ds->mir, ds->wc, ds->xlc, Raysin, 
			 &uu, &ww, &ll, &bl->BLOptions.ifl.iord); 
#endif

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
{
  struct RayType *Raysin, *Raysout;   
  int i/*, iord*/;
  /*double uu, ww, ll, xlength, xlength1, xlength2, phase;*/
  /*struct ElementType *ds; */
  struct RESULTType *Re; 

  bl->beamlineOK &= ~resultOK;

  /*********************************************************************/
#ifdef DEBUG
  fprintf(stderr, "RayTracec start: beamlineOK: %X, expect: %X\n", bl->beamlineOK, (sourceOK | mapOK)); 
#endif
 
  Re= &bl->RESULT;   
  if ((bl->beamlineOK & (sourceOK | mapOK)) != (sourceOK | mapOK))
    { 
      fprintf(stderr, "RayTracec: beamline is not OK: beamlineOK: %X != %X\nwe do nothing\n", 
	      bl->beamlineOK, (sourceOK | mapOK));       /*  exit(-1); */ 
    } 
    else  
      {
	Re->points= bl->RTSource.raynumber;
	Re->typ   = PLrttype;      
#ifdef DEBUG	
	printf("RayTracec: calculate %d ray(s) \n", Re->points);
#endif 
	Raysin = bl->RTSource.SourceRays; 
	Raysout= Re->RESp;    

	for (i= 0; i< bl->RTSource.raynumber; i++)
	  { 

	    ray_tracef(Raysin, Raysout, &bl->BLOptions.ifl.iord, 
		       (double *)bl->ypc1, (double *)bl->zpc1, 
		       (double *)bl->dypc, (double *)bl->dzpc); 

	    Raysout->phi= Raysin->phi;
	    Raysin++; Raysout++;  
	  }
      	/*   free(raysin);      */
	bl->beamlineOK|= resultOK; /* resultrays in memory */
      } /* beamline OK*/
#ifdef DEBUG
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
   int i, lost, zahl, elnumber;
   unsigned int elcounter;
   double uu, ww, ll, xlength, xlength1, xlength2, dphase, slopelen;
   struct ElementType *ds; 
   struct RESULTType *Re; 

/*********************************************************************/
   bl->beamlineOK &= ~resultOK;
   fprintf(stderr, "RayTraceFull: beamlineOK: %X, iord=%d\n", bl->beamlineOK, bl->BLOptions.ifl.iord); 
   Re= &bl->RESULT;
   if ((bl->beamlineOK & (sourceOK | mapOK)) == 0)
   { 
     fprintf(stderr, "rtrace.c: beamline is not OK: beamlineOK: %X\n", 
	     bl->beamlineOK); 
                  /*  exit(-1); */ 
   } else  
     {
       Re->points= zahl= bl->RTSource.raynumber;
             
       tmpsource= XMALLOC(struct RayType, Re->points);
       tmpresult= XMALLOC(struct RayType, Re->points);
           
       printf("RayTraceFull: start with \t\t%d ray(s) \t= 100 %s \n", 
	      zahl, "%"); 
       memcpy(tmpsource, bl->RTSource.SourceRays, zahl* 
	      sizeof(struct RayType));
       
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

#ifdef SEVEN_ORDER
	       intersection_8(&ds->mir, ds->wc, ds->xlc, Raysin, 
			    &uu, &ww, &ll, &bl->BLOptions.ifl.iord); 
#else

	       intersection(&ds->mir, ds->wc, ds->xlc, Raysin, 
			    &uu, &ww, &ll, &bl->BLOptions.ifl.iord); 
#endif 
  
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
     free(tmpsource); 
     Re->points= zahl;

     if (zahl > 0) bl->beamlineOK |= resultOK; 
     memcpy(Re->RESp, tmpresult, zahl* sizeof(struct RayType)); 

     free(tmpresult);
     /* resultrays in memory */
   } /* beamline OK*/
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

   wsl= gauss(slopew)* PI/ 324000.0;	/* 2* PI/(3600.* 180.);       */
   lsl= gauss(slopel)* PI/ 324000.0;    /* 2- da doppelter winkel     */
   /* fallunterscheidung notwendig */
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
  int ii, iy, iz;                   /* to make the code clearer */

  FreeResultMem(&bl->RESULT); 

  switch (newtype)
    {
    case PLrttype:
      ii= dim1;
      bl->RESULT.RESp= XMALLOC(struct RayType, ii);
      bl->RESULT.points= ii;
      break;
    case PLphspacetype:
      iy= dim1;
      iz= dim2;
      ii= iy * iz;
#ifdef DEBUG 
      printf("debug 0711: phase space allocate iy iz ii %d %d %d\n", iy, iz, ii);
#endif
      bl->RESULT.RESp= XMALLOC(struct PSDType, 1);
      PSDp= bl->RESULT.RESp;
      /* PSDp->y  = XMALLOC(double, psip->iy); */
      PSDp->y          = XMALLOC(double, iy);
      PSDp->z          = XMALLOC(double, iz);
      PSDp->psd        = XMALLOC(double, ii);
      PSDp->stfd1phmaxc= XMALLOC(double, ii);
      PSDp->stinumbc=    XMALLOC(double, ii);
      PSDp->s1c=         XMALLOC(double, ii);
      PSDp->s2c=         XMALLOC(double, ii);
      PSDp->s3c=         XMALLOC(double, ii);
      PSDp->eyrec=       XMALLOC(double, ii);
      PSDp->ezrec=       XMALLOC(double, ii);
      PSDp->eyimc=       XMALLOC(double, ii);
      PSDp->ezimc=       XMALLOC(double, ii);
      PSDp->iy=iy;     /* initialize UF 26.11.07 */
      PSDp->iz=iz;     /* initialize UF 26.11.07 */
      break;
    default: 
      fprintf(stderr, 
	      "AllocResult: error: unknown type! %c -> exit\n", 
	      bl->RESULT.typ);
      exit(-1);
    }
  bl->RESULT.typ= newtype;
} /* AllocResult */

void FreeResultMem(struct RESULTType *Re) 
/* macht den Speicher frei */
/* uwe 8.8.96 */
{
  struct PSDType *PSDp;

   if (((Re->typ & PLrttype) > 0) && (Re->RESp != NULL))
      free(Re->RESp); 
   if (((Re->typ & PLphspacetype) > 0) && (Re->RESp != NULL))
   {  
     PSDp= Re->RESp;
     if (PSDp->psd != NULL)
/* da der Speicher gemeinsam allociert wird teste ich nur einmal */
/* man muss alle reservierten pointer einzeln freigeben!         */
       {  
	 free(PSDp->y);  
	 free(PSDp->z);  
	 free(PSDp->psd);
	 free(PSDp->stfd1phmaxc);
	 free(PSDp->stinumbc)  ;
	 free(PSDp->s1c)       ;
	 free(PSDp->s2c)       ;
	 free(PSDp->s3c)       ;
	 free(PSDp->eyrec)     ;
	 free(PSDp->ezrec)     ;
	 free(PSDp->eyimc)     ;
	 free(PSDp->ezimc)     ;
       }
     free(Re->RESp);
   }
   Re->typ= 0;
} /* end freeResultmem */

/* end rtrace.c */
