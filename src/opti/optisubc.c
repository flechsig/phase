/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/opti/optisubc.c */
/*   Date      : <31 Oct 03 08:15:40 flechsig>  */
/*   Time-stamp: <17 Feb 04 14:49:50 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */


/* 31.3.99 GetRMS erweitert auf z */


#include <stdio.h>                    /* For printf and so on. */
#include <stdlib.h>	    	      /* needed for fopen      */
#include <string.h>
#include <math.h>
#include <Xm/Text.h>                  /* fileBox               */
#include <Xm/List.h>   
#include <Mrm/MrmAppl.h> 
#include <X11/Xlib.h>      
#include <X11/Xutil.h>      
/* DEC specific */
#ifdef VMS
  #include <descrip.h>                  /* for FORTRAN- String   */ 
  #include <DXm/DXmHelpB.h>      
  #include <DXm/DXmPrint.h>      
  #include <DXm/DXmColor.h>   
  #include <DXm/DECspecific.h>  
  #include <sys$library/DECw$Cursor.h>
#endif

#ifdef VMS
  #include "[-.phase]cutils.h"  
  #include "[-.phase]phase_struct.h"
  #include "[-.phase]fg3pck.h"   
  #include "[-.phase]mirrorpck.h"                 
  #include "[-.phase]geometrypck.h"   
  #include "[-.phase]PHASE.h"
#else
  #include "../phase/cutils.h"  
  #include "../phase/phase_struct.h"
  #include "../phase/fg3pck.h"   
  #include "../phase/mirrorpck.h"                 
  #include "../phase/geometrypck.h"   
  #include "../phase/phase.h"
#endif

#include "phaseopti.h"     


   
void getoptipickfile(struct optistruct *x, char *pickname)    
/* modification: 24 Oct 97 14:04:37 flechsig */
{                              
  FILE *f;
  int ii, *indexlist, version;
 
  if ((f= fopen(pickname, "r")) == NULL)
    {
      fprintf(stderr,"Error: read %s\n", pickname);
      exit(-1);
    }  
  if( CheckFileHeader(f, OptiPickFileHeader, &version) == 0) 
    {
      fscanf(f, "%s\n", &x->beamlinefilename); 
      fscanf(f, "%s\n", &x->minuitfilename); 
      fscanf(f, "%s\n", &x->resultfilename); 
      fscanf(f, "%d %d %lf\n", &x->xindex, &x->xpoints, &x->dx);  
      fscanf(f, "%d %d %lf\n", &x->yindex, &x->ypoints, &x->dy);  
      fscanf(f, "%d\n", &x->npars); 
      
      x->parindex= (int *) malloc(x->npars * sizeof(int));
      if (x->parindex == NULL)
   	{	
	  fprintf(stderr, "malloc error \n"); exit(-1);  
   	}         /* speicher allocieren */
      
      indexlist= x->parindex;  
      for (ii= 0; ii< x->npars; ii++, indexlist++)
	fscanf(f, "%d\n", indexlist);  
      fclose(f); 
    }
  else 
    exit(-1); 
}
   
void in_struct(struct BeamlineType* bl, double *z, int index)
/* modification: 24 Oct 97 15:51:55 flechsig */
/* modification: 27 Oct 97 09:33:59 flechsig */
/* modification: 05 Nov 97 08:09:59 flechsig */
/* modification: 13 Feb 98 13:59:23 flechsig */
/* modification: 17 Feb 98 08:47:02 flechsig   (cff) */
/* modification: 04 Mar 98 08:42:15 flechsig */
/* modification: 09 Mar 98 09:08:41 flechsig */
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
  listpt->ElementOK &= (~mapOK);
  if (mtype)
    {
      listpt->ElementOK &= (~elementOK); 
      mdat= &listpt->MDat; 
      switch (ipos)
	{  
	case 36:			/* r */
	  mdat->rmi= *z;
	  break;
	case 37:			/* rp */
	  mdat->rho= *z;
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
      listpt->ElementOK &= (~geometryOK); 
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
	  bl->BLOptions.lambda= gdat->lambda= 
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
	  bl->BLOptions.lambda=gdat->lambda= *z * 1e-6;   
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
	  FixFocus(*z, gdat->lambda, gdat->xdens[0], gdat->inout, 
		   &alpha, &beta);
	  gdat->theta0= fabs(alpha- beta)* 90.0/ PI;  /* 0.5 * (a-b) */
	  if (gdat->azimut > 1) gdat->theta0= -fabs(gdat->theta0);
	  fprintf(stderr, "in_struct: theta %g????\n", gdat->theta0);
	  break;
	case 16:       /* Energiescan mit cff constant */
	  /* berechne cff aus alten werten */
	  teta= fabs(gdat->theta0* PI/ 180.0);   /* theta */
	  fi  = (double)(gdat->inout)* asin(gdat->lambda* gdat->xdens[0]/
					    (2.0* cos(teta)));
	  cff= cos(fi- teta)/ cos(fi+ teta); 
	  bl->BLOptions.lambda= gdat->lambda= 
	    (*z > 0.0) ? (1240.0e-6/ *z) : 0.0;
	  fprintf(stderr, 
		  "\n in_struct: energy %g, NB: cff= const = %g\n", *z, cff);
	  FixFocus(cff, gdat->lambda, gdat->xdens[0], gdat->inout, 
		   &alpha, &beta);
	  gdat->theta0= fabs(beta- alpha)* 90.0/ PI;  /* 0.5 * (a-b) */
	  if (gdat->azimut > 1) gdat->theta0= -fabs(gdat->theta0);
	  break;
	case 17: xd[4]= (*z) - xd[5];     /*cl r2 fest*/
	  gdat->r= (*z)- gdat->rp;
	  break;
	default:
	  printf("index %d not found\n", ipos);
	  break;
	}          
    }
} /* end in_struct */

double out_struct(struct BeamlineType  *bl, double *z, int index)    
/* modification: 24 Oct 97 14:39:54 flechsig */
/* holt den Wert z entsprechend dem index aus der beamlinestruct */
/* modification: 27 Oct 97 11:15:34 flechsig */
/* modification: 13 Feb 98 14:03:41 flechsig */
/* modification: 17 Feb 98 08:47:02 flechsig   (cff) */
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
	case 36:			/* r = 36 */
	  *z= mdat->rmi;    
	  break;  
	case 37:			/* rp = 37 */
	  *z= mdat->rho;  
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
	  fi  = (double)(gdat->inout)* asin(gdat->lambda* gdat->xdens[0]/
					    (2.0* cos(teta)));
	  *z= cos(fi- teta)/ cos(fi+ teta);    /* cos(beta)/ cos(alpha); */
	  printf("outstruct: cff= %f?????\n", *z);
	  break;
	case 16:       /* Energiescan mit cff constant */
	  teta= fabs(gdat->theta0* PI/ 180.0);   /* theta */
	  fi  = (double)(gdat->inout)* asin(gdat->lambda* gdat->xdens[0]/
					    (2.0* cos(teta)));
	  cff= cos(fi- teta)/ cos(fi+ teta);   /* cos(beta)/ cos(alpha); */
	  *z= (bl->BLOptions.lambda > 0.0) ? 
	    (1240.0e-6/ bl->BLOptions.lambda) : 0.0;
	  printf("outstruct: cff= %f ?????\n", cff);
	  break;
	default:
	  printf("index %d not found\n", ipos);
	  break;
	  }          
      }
  return *z;
}     /* end outstruct */

void buildsystem(struct BeamlineType *bl) 
/* modification: 16 Oct 97 14:48:09 flechsig            */
/* durch abgespecktes (schnelles) buildbeamline ersetzt */
/* baut beamline und extrahiert map                     */
/* modification: 24 Oct 97 13:58:02 flechsig            */
/* modification: 04 Feb 98 08:46:30 flechsig */
{
  int     elcounter, i, mdim;
  struct  ElementType *listpt;    

  mdim= (bl->BLOptions.ifl.iord == 4) ? 70 : 35;
  elcounter= 1; 
  listpt= bl->ElementList; 
  while (elcounter<= bl->elementzahl)
    { 
      if ((listpt->ElementOK & mapOK) == 0)       /* map must be rebuild */
	{ 
	  if ((listpt->ElementOK & elementOK) == 0)   /* element rebuild */
	    {
	      DefMirrorC(&listpt->MDat, &listpt->mir, listpt->Art, 
			 listpt->elementname);    
	      listpt->ElementOK |= elementOK; 
	    }
	  if ((listpt->ElementOK & geometryOK) == 0) /* geometry rebuild */
	    {
	      DefGeometryC(&listpt->GDat, &listpt->geo);  
	      listpt->ElementOK |= geometryOK; 
	    }                          /* Elementdaten sind ok jetzt map */ 
	  MakeMapandMatrix(listpt, bl);
	  listpt->ElementOK|= mapOK; 
	}
      if (listpt->Art != kEOESlit)
	{
	  if (elcounter == 1)
	    memcpy(&bl->map70, &listpt->matrix, sizeof(MAP70TYPE)); 
	  else		                          /* bline zusammenbauen */
	    GlueLeft((double *)bl->map70, (double *)listpt->matrix);  
            /* A= B* A */
      	  SetDeltaLambda(bl, listpt);              /* resolutionfactor */
	} 
      elcounter++; listpt++; 
    } /* Schleife ueber alle Elemente fertig */
 
  extractmap(bl->map70, bl->ypc1, bl->zpc1, bl->dypc, bl->dzpc, 
	     &bl->BLOptions.ifl.iord);    
  /* bline ist fertig, map ist erzeugt */
} /* end buildsystem */

void Get_dydz_fromSource(struct BeamlineType *bl, double *dy, double *dz)
/* modification: 21 Jan 98 12:02:16 flechsig */
/* modification: 03 Feb 98 12:31:29 flechsig */
{
  struct HardEdgeSourceType *hs;
  struct DipolSourceType *ds;
  struct UndulatorSourceType *us; 
  double h, beugung; 
  
  switch (bl->RTSource.QuellTyp)
    {
    case 'U':
    case 'u':
      us= (struct UndulatorSourceType *) 
	&(bl->RTSource.Quelle.UndulatorSource);
      us->lambda= bl->BLOptions.lambda;
      *dz= *dy= sqrt(us->lambda/ us->length);
      printf("Get_dydz_fromSource: undulator at BII, high/low beta s.\n");
      printf("Get_dydz_fromSource: lambda= %g mm?, dy,dz= %g rad\n", 
	     us->lambda, *dy);
      /* high beta */
      /* eigentlich brauche ich die Verteilung der rotierenden Verteilung */
      break; 
    case 'D':
      ds= (struct DipolSourceType *) &(bl->RTSource.Quelle.DipolSource); 
      *dy= ds->sigdy/ 2000.0;  /* 2000 da totale Divergenz angegeben */
      *dz= ds->dz/ 2000.0; 
      printf("Get_dydz_fromSource: dipol source \n");
      printf("Get_dydz_fromSource: dy= %g, dz= %g (rad)\n", *dy, *dz);
      break; 
    case 'S':   /* single ray */
      *dy= bl->RTSource.Quelle.SRSource.dy/ 1000.0;
      *dz= bl->RTSource.Quelle.SRSource.dz/ 1000.0;
      printf("Get_dydz_fromSource: single ray\n");
      printf("Get_dydz_fromSource: dy= %g, dz= %g (rad)\n", *dy, *dz);
      break;
    default:
      hs= (struct HardEdgeSourceType *) &(bl->RTSource.Quelle.HardEdgeSource);
      printf("Get_dydz_fromSource: default dy=dz= 0.001 rad\n");
      *dy= *dz= 0.001;         	 
      break;
    }
} /* end Get_dydz_fromSource */

/* 31.3.99 erweitert auf z*/
double GetRMS(struct BeamlineType *bl, char ch)
{
  double EX, EX2, rms;
  int i, n;

  n= bl->RESULT.points;
  EX= EX2= 0.0;
  if (n > 0)
    {
      if (ch == 'z')
	{
	  for (i= 0; i< n; i++)
	    {
	      EX += bl->RESULT.RESUnion.Rays[i].z;
	      EX2+= bl->RESULT.RESUnion.Rays[i].z* 
		bl->RESULT.RESUnion.Rays[i].z;
	    }
	}
      else             /* y */
	{
	  for (i= 0; i< n; i++)
	    {
	      EX += bl->RESULT.RESUnion.Rays[i].y;
	      EX2+= bl->RESULT.RESUnion.Rays[i].y* 
		bl->RESULT.RESUnion.Rays[i].y;
	    }
	}
      EX  /= (double) n;
      EX2 /= (double) n;
      rms= sqrt(EX2- (EX * EX));
    } else rms= 0.0;
  fprintf(stderr, "%c- rms= %f, EX= %f, EX2= %f\n", ch, rms, EX, EX2);
  return rms;
} /* end GetRMS */


/* dummy routinen aus phase.c um linker Fehler abzufangen */




/* end optisubc.c */
