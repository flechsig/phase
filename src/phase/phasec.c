/*  File      : /home/pss060/sls/flechsig/phase/src/phase/phasec.c */
/*  Date      : <28 Oct 99 10:04:05 flechsig>  */
/*  Time-stamp: <15 Dec 99 09:51:40 flechsig>  */
/*  Author    : Flechsig Uwe OVGA/203a 4535, flechsig@psi.ch */

/* File      : /home/vms/flechsig/vms/phas/phasec/phasec.c */
/* Date      : <18 Mar 97 12:27:02 flechsig>  */
/* Time-stamp: <22 Oct 99 15:42:55 flechsig>  */
/* Author    : Uwe Flechsig, flechsig@exp.bessy.de */

/* Datei: USERDISK_3:[FLECHSIG.PHASE.PHASEC]PHASEC.C           */
/* Datum: 19.JUL.1994                                          */
/* Stand: 21-FEB-1997                                          */
/* Autor: FLECHSIG, BESSY Berlin                               */

/*23.11.98 UF */
                                         
#include <stdio.h>                    /* For printf and so on. */
#include <stdlib.h> 	      	      /* needed for fopen      */  
#include <string.h>                           
#include <math.h>                                                 
#include <ctype.h>
#include <stdarg.h> 
#include <Xm/Text.h>                                                  
#include <Xm/FileSB.h>                /*FileBox*/     
#include <Xm/List.h>   
#include <Xm/ToggleB.h>   
#include <Mrm/MrmAppl.h>  
#include <X11/Xlib.h>      
#include <X11/Xutil.h>  
#ifdef VMS    
  #include <processes.h>  /* eingefuegt fuer Starts anderer Programme*/
  #include <descrip.h>                      /* for FORTRAN- String */
  #include <DXm/DECspecific.h>                  
#endif
#include "cutils.h"   
#include "phase_struct.h"
#include "fg3pck.h"   
#include "mirrorpck.h"                 
#include "geometrypck.h"   
#include "phase.h"
#include "rtrace.h"

void BatchMode(char *fname, int cmode)
/* Uwe 2.10.96 */
/* Batchmodus */
/* modification: 14 Oct 97 13:41:13 flechsig */
/* modification: 23 Oct 97 07:47:23 flechsig */
{
  printf("BatchMode: datafilename  : %s\n", PHASESet.beamlinename);
  printf("BatchMode: resultfilename: %s\n", PHASESet.imageraysname);
  /*  InitDataSets(&PHASESet, fname); /* initialisiert auch Beamline */
  /* habe die Initialisierung hier extra */
#ifdef LOGFILE 
  CheckUser(logfilename, "Phase Batch");            /* user logfile  */
#endif
  Beamline.ElementList= NULL;                       /* 15.12.99 */
  Beamline.raysout= NULL;
  Beamline.RTSource.SourceRays= NULL;
  Beamline.beamlineOK= 0;
  ReadBLFile(PHASESet.beamlinename, &Beamline, &PHASESet);

  BuildBeamline(&Beamline); 
  if (cmode == -1) cmode= Beamline.BLOptions.CalcMod;
  switch (cmode)
    {
    case 1:
      printf("Ray Tracing\n");
      MakeRTSource(&PHASESet, &Beamline); 
      RayTracec(&PHASESet, &Beamline);
      WriteRayFile(PHASESet.imageraysname, &Beamline.RESULT.points,
		   Beamline.RESULT.RESUnion.Rays); 
      break;
    case 2:
      printf("Full Ray Tracing\n");
      MakeRTSource(&PHASESet, &Beamline); 
      RayTraceFull(&Beamline);
      WriteRayFile(PHASESet.imageraysname, &Beamline.RESULT.points,
		   Beamline.RESULT.RESUnion.Rays); 
      break;
    case 5:
      printf("Footprint Ray Tracing\n");
      Footprint(&Beamline, Beamline.position);
      break;
    case 3: 
      printf("Phase Space Transformation\n");
      src_ini(&Beamline.src); 
      PST(&Beamline);
      WritePsd(PHASESet.imageraysname, &Beamline.RESULT.RESUnion.PSD, 
	       Beamline.RESULT.RESUnion.PSD.iy, 
	       Beamline.RESULT.RESUnion.PSD.iz);
      break;
    default: 
      printf("unknown CalcMod: %d\n", cmode);
    }
  beep(5);
  printf("program end\n");
} /* end Batchmode */

void FixFocus(double cff, double lambda, double ldens, int m,
	      double *alpha, double *beta)
/* berechnet alpha und  beta ( in rad )aus cff         */
/* modification: 17 Feb 98 10:06:46 flechsig */
/* modification: 20 Feb 98 11:08:10 flechsig */
{
  double mld, c1, p, q, u;

  mld= m* lambda * ldens;
  if ((cff != 1.0) || (mld == 0.0))
    {
      c1 = 1.0- cff* cff;
      p  = -2.0* mld/ c1;
      q  = mld* mld/ c1 - 1.0;
      u  = -p/ 2.0+ sqrt(p* p/ 4.0- q); 
      *alpha= asin(u);
      *beta = asin(mld- u);
    } else printf("FixFocus error: cff==1.0 or zero order ...?\n");
} /* end FixFocus */

int ProcComandLine(unsigned int ac, char *av[])
/* Uwe 2.10.96 */
/* Wertet Kommandozeile aus */
/* modification: 14 Oct 97 13:29:39 flechsig */
/* modification: 23 Oct 97 07:48:12 flechsig */
{
  int  ret, i, cmode;
  char *ch, *dfname, *resultname, *pfname; 
  
  i= ret= 1;
  cmode= -1;
  /* defaults */
  pfname=     (char *) MainPickName; 
  resultname= (char *) PHASESet.imageraysname;
  dfname=     (char *) PHASESet.beamlinename;
  while (i < ac)
    {
      ch= av[i];
      if (*ch == '-') ch++; 
      switch (*ch)
	{
	case 'N':             /* keine Info */
	case 'n':
	  ret= 0;
	  break;
	case 'M':             /* rechenmodus */
	case 'm':
	  ch++; 
	  sscanf(ch, "%d", &cmode);
	  printf("calculation mode: %d\n", cmode);
	  break;
	case 'F':
	case 'f':
	  ch++; dfname= ch;
	  strcpy(PHASESet.beamlinename, ch);
	  printf("filename: >>%s<<\n", ch);
	  break;
	case 'O':
	case 'o':
	  ch++; resultname= ch;
	  strcpy(PHASESet.imageraysname, ch);
	  printf("result- filename: >>%s<<\n", ch);
	  break;
	case 'B':             /* Batch Job ohne X */
	case 'b':
	  ret= -8;
	  break;
	case 'H':             /* Help */
	case 'h':
	case '?':
	  printf("usage: phase [options]\n");
	  printf("       options: -n, -N:           no startup- info (X11)\n");
	  printf("                -h, -H, -?:       show this message\n");
	  printf("                -f, -Ffilename:   use datafile (*.phase)\n");
	  printf("                -o, -Oresultfile: filename\n");
	  printf("                -b, -B:           batch mode (no X11)\n");
	  printf("                -m, -Mcalcmode:   calculation mode \n");
	  printf("                                  1: ray trace\n");
	  printf("                                  2: full ray trace\n");
	  printf("                                  3: phase space density\n");
	  exit(3);
	  break;
	default: 
	  printf("unknown option %c \n", *ch);
	}
      i++;
    }

  if (ret == -8)
    {
      BatchMode(pfname, cmode);
      exit(3);
    }
  return ret;
}


void DefGeometryC(struct gdatset *x, struct geometrytype *gout)  
     /* Uwe 25.6.96 							*/
     /* umgeschrieben - keine fileausgabe mehr 			        */
     /* datenstruktur soll gleich sin und cosinus werte enthalten 	*/
  
     /* last modification: 20 Jun 97 12:05:30 flechsig */
/* modification: 19 Feb 98 11:07:44 flechsig Vorzeichenfehler alpha, beta */
{
  double delta, alpha, beta, theta0, trans, radius;
  int i;

  theta0= fabs(x->theta0* PI/ 180.0);   
  delta= (double)(x->inout)* asin(x->lambda* x->xdens[0]/(2.0* cos(theta0)));
    alpha= (-theta0- delta);   /* eigentlich fi+ theta */
    beta = ( theta0- delta);   /* nicht eher fi- theta???*/
/* modification: 17 Feb 98 09:33:48 flechsig */
/* modification: 19 Feb 98 11:08:59 flechsig */
/*   alpha= (theta0+ delta); */
/*   beta = (delta- theta0); */
  printf("DefGeometryC: alpha: %f, beta: %f ?????, lambda= %g mm\n", 
	 alpha* 180.0/ PI, beta* 180.0/ PI, x->lambda);
  if ((x->iflag) == 1)
    {
      radius   = (2.0* x->r* x->rp)/ ((x->r+ x->rp)* cos(theta0));   
      trans    = radius* (1.0- cos(delta));      
      gout->r  = x->r-  trans; 
      gout->rp = x->rp- trans;    
      printf("NIM translation enabled, trans= %d mm\nr1= %d mm, r2= %d mm\n", 
             trans, gout->r, gout->rp);  
    }  else 
      {
	gout->r  = x->r; 
	gout->rp = x->rp;    
      }

  gout->sina= sin(alpha);   
  gout->cosa= cos(alpha);   
  gout->sinb= sin(beta);   
  gout->cosb= cos(beta);   
  for (i= 0; i< 5; i++) 
    gout->x[i]= x->xdens[i]; 
  gout->xlam = x->lambda* (double)(x->inout);  
  gout->idefl= (x->theta0 > 0.0) ? 1 : -1;  
} /* end DefGeometryC */ 

void DefMirrorC(struct mdatset *x, struct mirrortype *a, int etype)  
     /* erzeugt elementmatrix im Fortran Speichermodell 	*/    
     /* Uwe 10.6.96 						*/
     /* letzte Aenderung: 30.4.97 				*/
     /* last modification: 17 Jun 97 08:10:41 flechsig */
     /* umgeschrieben auf memory 				*/
     /* etype auf defines umgeschrieben 17.6.97 */
{
  double r, rho, *dp,
    alpha, aellip, bellip, cellip, eellip, f, z0, small;
  int i, j;

  printf("DefMirrorC called\n");   
  small= 1e-15;   
  r= x->rmi;
  rho= x->rho;
  dp= (double *)a;
  alpha= x->alpha * PI/ 180.0;
      
  for (i= 0; i< 36; i++) dp[i]= 0.0;        /* initialisieren */

  /* Radien < small dann planspiegel */
  if ((etype != kEOEElli) && (etype != kEOEPElli) )     
    {                                                   /* index a(i,j) */
      printf("toroidal shape\n");                       /* = i+ j* 6    */
      if (fabs(rho) > small) 
	{
	  dp[12]= 0.5/ rho;                		/* 0,2 */
	  dp[24]= 1.0/ (8.0* rho* rho* rho);              /* 0,4 */
	}  
      if (fabs(r) > small)  
	{
	  dp[2]= 0.5/ r;   
	  dp[4]= 1.0/ (8.0* r* r* r);   
	}  
      if ((fabs(rho) > small) && (fabs(r) > small))  
	{
	  dp[14]= 1.0/(4.0* r * r* rho);                 /* 2, 2 */
	} 
    } /* end toroid */ else 
  /**********************************************************/
  if (etype == kEOEElli)     
  {
    printf("elliptical shape\n");  
    if (alpha < small) 
    {
      beep(1);	
      fprintf(stderr, "theta = 0, elliptical shape makes no sense!\n");
    } else
    {       
      aellip= (x->r1+ x->r2)/ 2.0;
      bellip= sqrt(aellip* aellip- 0.25* (x->r1* x->r1+ x->r2* x->r2- 
					  2.0* x->r1* x->r2* cos(2.0* alpha)));
      cellip= sqrt(aellip* aellip- bellip* bellip);
      eellip= sqrt(1.0- bellip* bellip/ (aellip* aellip));
      f     = (x->r1* x->r2)/ (x->r1+ x->r2);
      z0    = (x->r1* x->r1- x->r2* x->r2)/ (4.0* cellip);

      printf("ell. parameter: a= %f, b= %f, c= %f, e= %f, f= %f, z0= %f\n",
	     aellip, bellip, cellip, eellip, f, z0);

      dp[12]= 1.0/ (4.0* f* cos(alpha));    		/* 0,2 */
      dp[2] = cos(alpha)/ (4.0* f);          		/* 2,0 */
      dp[13]= (tan(alpha)* sqrt(pow(eellip, 2.0)- pow(sin(alpha), 2.0)))/
	(8.0* pow(f, 2.0)* cos(alpha));                  /* 1,2 */
      dp[3] = (sin(alpha)* sqrt(pow(eellip, 2.0)- pow(sin(alpha), 2.0)))/
	(8.0* f* f);                              /* 3,0 */
      dp[4] = (pow(bellip, 2.0)/ (64.0* pow(f, 3.0)* cos(alpha)))  * 
	      ((5.0* pow(sin(alpha), 2.0)* pow(cos(alpha),2.0))/ 
	      pow(bellip, 2.0)- (5.0* pow(sin(alpha), 2.0))/ 
	      pow(aellip, 2.0)+ 1.0/ pow(aellip, 2.0));  	/* 4,0 */ 
      dp[14]= (pow(sin(alpha), 2.0)/ (16.0* pow(f, 3.0)* 
	       pow(cos(alpha), 3.0)))* (1.50* pow(cos(alpha), 2.0)- 
	      (pow(bellip, 2.0)/ pow(aellip, 2.0))* (1.0- 1.0/ 
	      (2.0* pow(tan(alpha), 2.0))));  			/*2,2 */
      dp[24]= (pow(bellip, 2.0)/ (64.0* pow(f, 3.0)* pow(cos(alpha), 3.0)))* 
	(pow(sin(alpha), 2.0)/ pow(bellip, 2.0) + 
	 1.0/ pow(aellip, 2.0));  				/* 0,4 */

    } 
  } else 
  if (etype == kEOEPElli )     
  {
    printf("plane- elliptical shape\n");  
    if (alpha < small) 
    {
      beep(1);	
      fprintf(stderr, "theta = 0, elliptical shape makes no sense!\n");
    } else
    {     
      aellip= (x->r1+ x->r2)/ 2.0;
      bellip= sqrt(aellip* aellip- 0.25* (x->r1* x->r1+ x->r2* x->r2- 
					  2.0* x->r1* x->r2* cos(2.0* alpha)));
      cellip= sqrt(aellip* aellip- bellip* bellip);
      eellip= sqrt(1.0- bellip* bellip/ (aellip* aellip));
      f     = (x->r1* x->r2)/ (x->r1+ x->r2);

      z0    = (x->r1* x->r1- x->r2* x->r2)/ (4.0* cellip);

      printf("ell. parameter: a= %f, b= %f, c= %f, e= %f, f= %f, z0= %f\n",
	     aellip, bellip, cellip, eellip, f, z0);

      dp[2] = cos(alpha)/ (4.0* f);          		/* 2,0 */
      dp[3] = (sin(alpha)* sqrt(pow(eellip, 2.0)- pow(sin(alpha), 2.0)))/
	(8.0* f* f);                              /* 3,0 */
      dp[4] = (pow(bellip, 2.0)/ (64.0* pow(f, 3.0)* cos(alpha)))  * (
	      (5.0* pow(sin(alpha), 2.0)* pow(cos(alpha),2.0))/ 
	       pow(bellip, 2.0)- (5.0* pow(sin(alpha), 2.0))/ 
	       pow(aellip, 2.0)+ 1.0/ pow(aellip, 2.0));  	/* 4,0 */ 
    } 
  } else 
    { 
      fprintf(stderr, "defmirrorc: %d - unknown shape:", etype); 
      exit(-1);
    }
} /* end defmirrorc */

void GetOptiBox(struct PHASEset *x) 
/* modification: 28 Oct 97 11:30:33 flechsig */
{
  FILE *oppickfile, *minfile;
  char *opresname= NULL,  *minname= NULL, *zeile= NULL, *subzeile, 
    puffer[MaxPathLength], *ch; 
  XmString label;
  XmStringTable list2items;
  int parameterzahl, i, index, k;     
 
  get_something(widget_array[kCOptiResultButton], XmNlabelString, &label);
  if (!XmStringGetLtoR(label, XmFONTLIST_DEFAULT_TAG, &opresname)) 
    return; 
  get_something(widget_array[kCOptiMinuitButton], XmNlabelString, &label);
  if (!XmStringGetLtoR(label, XmFONTLIST_DEFAULT_TAG, &minname)) 
    return;  
  
  /*  XtVaGetValues(widget_array[kCOptiList], 
		XmNitems, &listitems,  XmNitemCount, &elementzahl, NULL); */ 
  XtVaGetValues(widget_array[kCOptiList2], 
		XmNitems, &list2items, XmNitemCount, &parameterzahl, NULL);  
   
  parameterzahl-= 2;
  if ((oppickfile= fopen(x->optipckname, "w+")) == NULL)
    {
      fprintf(stderr, "error: open file %s\n", opresname); exit(-1);   
    }    
  if ((minfile= fopen(minname, "w+")) == NULL)
    {
      fprintf(stderr, "error: open file %s\n", minname); exit(-1);
    }

  fprintf(oppickfile, "%s\n%s\n%s\n%s\n", 
	  OptiPickFileHeader, x->beamlinename, minname, opresname);  
  fprintf(minfile,                                     
	  "SET TITLE\nFit with program \"PHASE\"\nPARAMETERS\n");    

  for (i= 0; i < 2; i++)  
    {
      if (!XmStringGetLtoR(list2items[i], XmFONTLIST_DEFAULT_TAG, &zeile)) 
	  return; 
      subzeile= zeile; k= 0; 
      while ((isdigit(*subzeile) == 0) && (k < 10)) 
	{++subzeile; ++k;}  
      if (k < 10) 
	fprintf(oppickfile, "%s\n", subzeile); 
      else     
	fprintf(oppickfile, "0 0 0.0\n"); 
      XtFree(zeile);   
    }

  fprintf(oppickfile, "%d\n", parameterzahl);     
  for (i= 0; i < parameterzahl; i++)  
    {
      if (!XmStringGetLtoR(list2items[i+2], XmFONTLIST_DEFAULT_TAG, &zeile))
	return;  
      sscanf(zeile,"%d", &index);
      subzeile= zeile; k= 0; 
      while ((*subzeile != ' ') && (k < 10)) 
	{++subzeile; ++k;}                      /* remove index */
      ++subzeile; 
      if (k < 10) 
	fprintf(minfile, "%d %s\n", i+ 1, subzeile);  
      else     
	fprintf(minfile, "99 'error!'\n");  
      fprintf(oppickfile, "%d\n", index);      /* index in pickfile */
      XtFree(zeile);  
    } 
         
 /*  for (i= 0; i < elementzahl; i++)   */
/*     { */
/*       zeile= DXmCvtCStoFC(listitems[i], &bc, &status);  */
/*       strcpy(puffer, zeile);   */
/*       if ((subzeile= strrchr(puffer, ']')) == NULL) subzeile= puffer; */
/*       if ((ch = strrchr(subzeile, '.'))    != NULL) *ch=   '\0';    */
/*       fprintf(oppickfile, "%s.datg\n", puffer);   */
/*       fprintf(oppickfile, "%s.date\n", puffer);   */
/*       XtFree(zeile);   */
/*     }            */

  fprintf(minfile, "\nMIGRAD\nRETURN\n");
  fclose(oppickfile); fclose(minfile);
  XmStringFree(label); XtFree(minname); XtFree(opresname); 
} /* end GetOptiBox */

int GetPHASE(struct PHASEset *x, char *mainpickname)
     /* return 1: file gelesen- OK		*/
     /* 30.5.96 				*/
     /* last mod. 26.7.96			*/
{
  FILE *f;
  int  rcode;
   
  rcode= -1;
  if ((f= fopen(mainpickname, "r")) == NULL)
    fprintf(stderr, "File %s not found- defaults used!\n", mainpickname); 
  else 
    {
      if( CheckFileHeader(f, MainPickFileHeader) == 0)   
	{      
	  fscanf(f,"%s\n", &x->matrixname);     
	  fscanf(f,"%s\n", &x->mapname);    
	  fscanf(f,"%s\n", &x->sourceraysname);       
	  fscanf(f,"%s\n", &x->imageraysname);	   
	  fscanf(f,"%s\n", &x->intersecname); 
	  fscanf(f,"%s\n", &x->geometryname);   
	  fscanf(f,"%s\n", &x->elementname);  
	  fscanf(f,"%s\n", &x->sourcepckname);  
	  fscanf(f,"%s\n", &x->geometrypckname);   
	  fscanf(f,"%s\n", &x->elementpckname);  
	  fscanf(f,"%s\n", &x->pssourcename);   
	  fscanf(f,"%s\n", &x->plotpsname);  
	  fscanf(f,"%s\n", &x->printpclname); 
	  fscanf(f,"%s\n", &x->optipckname); 
	  if (!feof(f))    /* neu */
	    fscanf(f,"%s\n", &x->beamlinename); 
	  else strcpy(x->beamlinename, "SGM.PHASE"); 
	  rcode= 1;       /* OK zurueck */
	}
      fclose(f);   
    }	
  return rcode;
}     /* end  GetPHASE */

int iindex(int e, int p)             
/* berechnet den index aus den selektierten Positionen */
/* modification: 17 Feb 98 10:36:07 flechsig */
/* modification: 23 Apr 98 14:14:32 flechsig */

{
  int iret, k, mtp;

  iret = e * p;        				/* test ob 0 */
  mtp  = (p > 17) ? (1 << 7) : 0;      /* position fuer mdaten */
  if (mtp != 0) 
    {
      switch (p)
        {
	case 18: p=  36;  break;     
	case 19: p=  37;  break;  
	case 20: p=   0;  break;
        }
      p++;                             /* fuer kompatibil. mit  (p-1) */
    }                                  /* ende mtyp */
  k    = ((e- 1) << 8) | mtp | (p- 1);
  iret = (iret == 0) ? -1 : k;
  printf("index:  %d\n", iret);
  return iret;
} /* end index */

void InitDataSets(struct PHASEset *x, char *mainpickname)   
     /* initialisiert die globalen Variablen */
     /* last mod. Uwe 21.1.97 		*/
{
#ifdef LOGFILE 
  CheckUser(logfilename, "Phase");                      /* user logfile  */
#endif

  if (GetPHASE(x, mainpickname) != 1)           /* filenamen */
    {  
      InitPHASE(x);   				/* set default names */
      PutPHASE(x, mainpickname);          	/* write names */   
    }
  /* neu beamline pointer initialisieren 7.6.96*/
  Beamline.ElementList= NULL;                       /* 15.12.99 */
  Beamline.raysout    = NULL;
  Beamline.RTSource.SourceRays= NULL;
  Beamline.beamlineOK= 0;

  ginitdatset(&GDefDat);           /* init defaults */
  minitdatset(&MDefDat);                   

  ReadBLFile(x->beamlinename, &Beamline, x);
    
 
  grdatstruct.status= 0;
  SetGrDatStruct(x->imageraysname, &Beamline, &grdatstruct);  
  /* PHASEgraf.c */
  /*   optistructure.fileliste= NULL;     */
  printf("InitDataSets end\n");     
}

void InitOptiBox1(char *pickname)   
{
  printf("dummy\n");
}

void InitOptiBox(char *pickname, struct BeamlineType *bl)   
/* modification: 28 Oct 97 14:56:23 flechsig */
{
  FILE *f, *f1;
  char minname[MaxPathLength], opresname[MaxPathLength], 
    buffer[MaxPathLength], *subzeile; 
  XmString label;
  int  parameterzahl, i, index, k;   
  double eps;
  struct ElementType *list;

  printf("InitOptiBox called\n");
  XmListDeleteAllItems(widget_array[kCOptiList]);  
  list= bl->ElementList;
  for (i= 0; i< bl->elementzahl; i++, list++)
    {
      label= XmStringCreateLocalized(list->elementname);
      XmListAddItem(widget_array[kCOptiList], label, 0); 
      XmStringFree(label);
    }

  if ((f= fopen(pickname, "r")) != NULL)   
    {
      if( CheckFileHeader(f, OptiPickFileHeader) == 0) 
	{   
	  fscanf(f, "%s\n", &minname); /* hier beamlinename ueberlesen */
	  fscanf(f, "%s\n", &minname);   

	  label= XmStringCreateLocalized(minname);   

	  XtVaSetValues(widget_array[kCOptiMinuitButton], 
			XmNlabelString, label, NULL); 
	  XmStringFree(label);  
	  fscanf(f, "%s\n", &opresname);   

	  label= XmStringCreateLocalized(opresname);   

	  XtVaSetValues(widget_array[kCOptiResultButton], 
			XmNlabelString, label, NULL);  
	  XmStringFree(label);  
	 
	  fgets(buffer, 80, f); buffer[strlen(buffer)-1]= '\0';
	  sprintf(opresname, "x : %s", buffer); 
	  XmListDeleteAllItems(widget_array[kCOptiList2]);

	  label= XmStringCreateLocalized(opresname);   

	  XmListAddItem(widget_array[kCOptiList2], label, 0); 
	  XmStringFree(label);  
	  fgets(buffer, 80, f); buffer[strlen(buffer)-1]= '\0';     
	  sprintf(opresname, "y : %s", buffer); 
	 
	  label= XmStringCreateLocalized(opresname);   

	  XmListAddItem(widget_array[kCOptiList2], label, 0); 
	  XmStringFree(label);                                 /* y zeile ok */
	  printf("hallo\n");
	  fscanf(f, "%d\n", &parameterzahl);      
	  if ((f1= fopen(minname, "r")) == NULL)   
	    fprintf(stderr, "Minfile: %s- not found\n", minname);
	  else     
	    {
	      if( CheckFileHeader(f1, "SET") != 0) 
		fprintf(stderr, "error: InitOptiBox- minfile- fileheader\n");
	      else 
		{  
		  fscanf(f1, "%s\n", &buffer);        
		  fgets(buffer, MaxPathLength, f1);    
		  fgets(buffer, MaxPathLength, f1);    
		  for (i= 0; i < parameterzahl; i++)
		    {
		      fgets(buffer, MaxPathLength, f1); 
		      buffer[strlen(buffer) -1]= '\0';     
		      subzeile= buffer; k= 0; 
		      fscanf(f, "%d\n", &index);       /* index vom pickfile */
		      while ((*subzeile != ' ') && (k < 10)) 
			{++subzeile; ++k;}             /* remove number      */
		      ++subzeile; 
		      sprintf(opresname, "%d %s", index, subzeile);
 
		      label= XmStringCreateLocalized(opresname);   

		      XmListAddItem(widget_array[kCOptiList2], label, 0); 
		      XmStringFree(label);  
		    }       
		}  /* end checkfileheader(f1, set);*/
	      fclose(f1);
	    }  /* end open minfile */
	}  
      else 
	fprintf(stderr, "error: InitOptiBox- fileheader\n");
      fclose(f);
    }  
  else 
    fprintf(stderr, "InitOptiBox not possible\n");
}

void InitOptiList2(int selpos, char *neu)   
{
  XmString str;                      /*%%% *str */
  int i, *itemlist[2], pos;
  char rbuffer[100], ch, *text,
    *inhalt[]= {    "xindex nx dx", 
		    "yindex ny dy",	
		    "pindex 'name' p0 dp pmin pmax", };

  if (XmListGetSelectedPos(widget_array[kCOptiList2], itemlist, &i) == True)
    {
      pos= **itemlist;   XtFree((char *)*itemlist);  
    }  else   pos= 0; 
   
  switch (selpos)
    {
    case -2:        /* Add Item */
      text= XmTextGetString(widget_array[kCOptiT1]); 

      str = XmStringCreateLocalized(text);              

      XmListAddItem(widget_array[kCOptiList2], str, pos); 
      XtFree(text); XmStringFree(str);  
      break;

    case -1:       /* replace Item */
      text= XmTextGetString(widget_array[kCOptiT1]); 

      str = XmStringCreateLocalized(text);

      XmListDeletePos(widget_array[kCOptiList2], pos);
      XmListAddItem(widget_array[kCOptiList2], str, pos); 
      XtFree(text); XmStringFree(str);  
           
      break;
     
    default:              /* selection */
      printf("sel\n");
      i= selpos- 1;
      if (i > 1) i= 2;
      sprintf(rbuffer, "%s", inhalt[i]);   

      str= XmStringCreateLocalized(rbuffer);  

      sprintf(rbuffer, "%s", neu);   
      set_something(widget_array[kCOptiT1], XmNvalue, rbuffer);   
      set_something(widget_array[kCOptiEditLabel], 
		    XmNlabelString, str);    
      XmStringFree(str);  
	
    }
}

void InitPHASE(struct PHASEset *x)                   /* set defaults */
     /* 30.5.96 */
{
  strcpy(x->matrixname,      D0matrixname);
  strcpy(x->mapname,         D0mapname);    
  strcpy(x->sourceraysname,  D0sourceraysname);       
  strcpy(x->imageraysname,   D0imageraysname);	   
  strcpy(x->intersecname,    D0intersecname);
  strcpy(x->geometryname,    D0geometryname);   
  strcpy(x->elementname,     D0elementname);        
  strcpy(x->sourcepckname,   D0sourcepckname);       
  strcpy(x->elementpckname,  D0elementpckname);
  strcpy(x->geometrypckname, D0geometrypckname); 
  strcpy(x->pssourcename,    D0pssourcename);   
  strcpy(x->plotpsname,      D0plotpsname);   
  strcpy(x->printpclname,    D0printpclname);   
  strcpy(x->optipckname,     D0optipckname);   
  strcpy(x->beamlinename,    "SGM.PHASE"); 
}
   
/******* read matrixfile ************************************/ 
void readmatrixfilec(char *fname, double *map, int dim)  
/*------------------------------------------------------------*/
/* located in fgmap3dpp.for,phasefor.for               */
/* umgeschrieben auf c, liest eine in fortran Speichermodell
   abgespeicherte transformationsmatrix
   Uwe 14.6.96
/**************************************************************/    
{
  int i, j, k, dim2;
  FILE *f;
  double tmp;

  if ((f= fopen(fname, "r")) == NULL)
    {
      fprintf(stderr,"\aError: read %s\n", fname);
      exit(-1);
    } 
  dim2= dim* dim;
  k= 0;
  while (k < dim2)   /* kein Test auf fileende */
    {
      fscanf(f, "%d %d %lf\n", &i, &j, &tmp);
      /*     printf("i, j, tmp: %d %d %lf\n", i,j,tmp);   */   
      map[(i- 1)+ (j- 1)* dim]= tmp;
      k++;
    }
  fclose(f);     
}
/******** read matrixfile ************************************/


void PutPHASE(struct PHASEset *x, char *mainpickname)  /* write mainpickfile */
     /* 30.5.96 */
{                              
  FILE *f;

  printf("putphase: write filenames\n");

  if ((f= fopen(mainpickname, "w")) == NULL)
    {
      fprintf(stderr,"\afatal Error: write %s\n", mainpickname);
      exit(-1);
    } else 
      {
        fprintf(f,"%s\n", MainPickFileHeader); 
        fprintf(f,"%s\n", x->matrixname); 
        fprintf(f,"%s\n", x->mapname);    
 	fprintf(f,"%s\n", x->sourceraysname);       
 	fprintf(f,"%s\n", x->imageraysname);	   
 	fprintf(f,"%s\n", x->intersecname);
 	fprintf(f,"%s\n", x->geometryname);   
        fprintf(f,"%s\n", x->elementname);  
        fprintf(f,"%s\n", x->sourcepckname);
        fprintf(f,"%s\n", x->geometrypckname);  
        fprintf(f,"%s\n", x->elementpckname);  
        fprintf(f,"%s\n", x->pssourcename);    
        fprintf(f,"%s\n", x->plotpsname);  
        fprintf(f,"%s\n", x->printpclname);   
        fprintf(f,"%s\n", x->optipckname);   
        fprintf(f,"%s\n", x->beamlinename);   
       	fclose(f);  
      }
}    /* end putphase */	



/******* druckt text in das Anzeigefenster  MainList  ******************/

void UpdateMainList()   
{
  FILE *file;
  char line[80], buffer[80];

  sprintf(line, "%s;1", MainListFileName);   
  if ((file= fopen(line, "r")) != NULL)	
    {
      fprintf(stderr,"\nStart output\n\n");
      while (!feof(file))
        {
	  fgets(line, 80, file);
          /* puts(line);  */
	  sprintf(buffer, "%s\0", line);    
	  fprintf(stderr,"%s", buffer);    
	  xprintf(buffer);
	}
      fclose(file); 
      sprintf(line,"del/noconf %s;*", MainListFileName); 
      system(line);   
    }  /*else fprintf(stderr, "%s not found\n", MainListFileName);  */
}

void xprintf(char *text)
{
  char *inhalt, *neuinhalt, zeile[80];
  int maxlen, lpos, len;
    
  sprintf(zeile,"%s", text);
  maxlen= XmTextGetMaxLength(widget_array[kMainList]);
  lpos  = XmTextGetLastPosition(widget_array[kMainList]); 
  XmTextInsert(widget_array[kMainList], lpos, zeile);  
  inhalt= XmTextGetString(widget_array[kMainList]);
  if ((len= strlen(inhalt)) > maxlen) 
    {
      neuinhalt= &inhalt[len- maxlen];
      XmTextSetString(widget_array[kMainList], neuinhalt);  
    }
  lpos= XmTextGetLastPosition(widget_array[kMainList]); 
  XmTextShowPosition(widget_array[kMainList], lpos);     
  XtFree(inhalt);
}    

void xxprintf(char *text)
{
  XmString	topic;

  topic= XmStringCreateLocalized(text); 

  XmListAddItem(widget_array[kMainList], topic, 1); 
  /*XmListDeselectAllItems(widget_array[kMainList]); 
    XmListSetBPHASEmPos(widget_array[kMainList], 0); */
  XmStringFree(topic);   
}  /* end xxprintf() */  

void SetDefaultParameter(struct BeamlineType *bl) 
     /* setzt defaults fuer Parameterbox */ 
     /* Uwe 21. 10. 96 */ 
     /* Stand 17. 2. 97 */ 
{
  bl->BLOptions.epsilon= 1e-5; 
  bl->BLOptions.ifl.iord= 4; 
  bl->BLOptions.PSO.intmod= 1;
  bl->BLOptions.PSO.PSSource.yhard=  0;
  bl->BLOptions.PSO.PSSource.dyhard= 0;
  bl->BLOptions.PSO.PSSource.zhard=  0;
  bl->BLOptions.PSO.PSSource.dzhard= 0; 
  bl->BLOptions.PSO.PSSource.sigy=   0.03;
  bl->BLOptions.PSO.PSSource.sigdy=  4e-5;
  bl->BLOptions.PSO.PSSource.sigz=   0.3;
  bl->BLOptions.PSO.PSSource.sigdz=  4e-5; 
  bl->BLOptions.PSO.ndyfix=          30;
  bl->BLOptions.PSO.dyminfix= -1e-2;
  bl->BLOptions.PSO.dymaxfix=  1e-2;
  bl->BLOptions.PSO.ndzfix=    30;
  bl->BLOptions.PSO.dzminfix= -1e-2;
  bl->BLOptions.PSO.dzmaxfix=  1e-2;

  /*bl->BLOptions.xi.itery0=  5;*/
  bl->BLOptions.xi.ianzy0=  15;
  /*  bl->BLOptions.xi.imaxy=   25;*/
  /*bl->BLOptions.xi.fracy=   0.7;*/
  /*bl->BLOptions.xi.frac1y=  0.2;*/

  /*bl->BLOptions.xi.iterz0=  5;*/
  bl->BLOptions.xi.ianzz0=  15;
  /*bl->BLOptions.xi.imaxz=   25;*/
  /*bl->BLOptions.xi.fracz=   0.7;*/
  /*bl->BLOptions.xi.frac1z=  0.2;*/

  printf("\nDefaults sind nicht mehr aktuell!\n");
 
} /* end SetDefaultParameter  */

void InitParameterBox(struct BeamlineType *bl, char *neu) 
/* modification: 23 Apr 97 09:20:47 flechsig */
/* modification: 09 Jun 97 17:18:29 flechsig */
/* modification: 31 Oct 97 08:25:50 flechsig */

{
  struct OptionsType   *op; 
  struct PSOptionsType *pop;
  struct PSSourceType  *psp;
  XmString str[73], str1[4];  	
  char *inhalt[]= {	
    "epsilon for Newton routine",
    "calculation up to (3,4) order", 
    "iordsc", 
    "expansion of pathlength (1),",
    "subtraction of ideal path length (1)",
    "source type",
    "radius of pinhole in source plane (mm)",
    "aperture in source plane, ymin (mm)",
    "aperture in source plane, ymax (mm)",
    "aperture in source plane, zmin (mm)",
    "aperture in source plane, zmax (mm)",
    "radius in aperture plane",
    "op->apr.ymin_ap",
    "op->apr.ymax_ap",
    "op->apr.zmin_ap",
    "op->apr.zmax_ap",
    "Dipole: Cy",
    "Dipole: Cz",
    "Dipole: y-Distance (virtual) between Dipole and source plane",
    "Dipole: z-Distance (real) between Dipole and source plane",
   /*  "ymin of dipole source (in mm)", */
/*     "ymax of dipole source (in mm)", */
/*     "zmin of dipole source (in mm)", */
/*     "zmax of dipole source (in mm)", */
    "(1): phase advance for grating, (0): mirror",
    "(1) normalize output, (0) do not normalize",
    "inorm1",
    "inorm2 (0, 1, 2)",
    "derive matrix elements in 3 different ways (1) (for debugging)",
    "source type (size/divergence):(0)sigma val.,(1)hard edge,(2)file",
    "source type (size/divergence):(0)sigma val.,(1)hard edge,(2)file",
    "source size/div.: sigmay(mm) / sigmayp or half height/angle",
    "source size/div.: sigmay(mm) / sigmayp or half height/angle",
    "ymin, zu integrierender Winkelbereich in mrad",
    "ymax, zu integrierender Winkelbereich in mrad",
 /*   "number of integration points for equidistant grid", */
 /*   "itery0,     Anzahl der Iterationen", */
    "ianzy0,     Anzahl der Stuetzstellen im ersten Raster",
 /*   "imaxy", */
 /*   "fracy,      Verhaeltnis von fmin zu fmax", */
 /*   "frac1y,     minimaler Quotient benachbarter Werte", */
    "source type (size/div.):(0)sigma val.,(1)hard edge, etc...",
    "source type (size/div.):(0)sigma val.,(1)hard edge, etc...",
    "source size/div.: sigmay(mm) / sigmayp or half height/angle",
    "source size/div.: sigmay(mm) / sigmayp or half height/angle",
    "zmin, ",
    "zmax",
 /*   "number of integration points for equidistant grid",*/
 /*   "iterz01",*/
    "ianzz0",
 /*   "imaxz",*/
 /*   "fracz",*/
 /*   "frac1z",*/
    "(1) write 4-dim brightness to file",
    "(0) simpson integration, (1) spline integration",
    "d12_max",
    "id12; (1) print d12 on file, (0) do not print",
    "ianz0_cal",
    "ianz0_fixed",
    "iamp_smooth (0,1,2)",
    "iord_amp",
    "ifm_amp",
 /*    "amp_change", */
    "iord_pha",
    "ifm_pha",
  /*   "phase_change_1, ", */
/*     "phase_change_2", */
/*     "(0) do not allow, (1) allow change of curvature sign of phase", */
/*     "(1) correct phase for pi and 2pi, (0) correct only for 2 pi", */
  /*  "order of amplitude expansion", */
 /*    "dphi_min", */
    "distance to focus",
    "insert pinhole array in source plane",
    "pin_yl0, ",
    "pin_yl",
    "pin_zl0, ",
    "pin_zl",
      
      
    /*********************************************
      hier gehen meine frueheren Bezeichnungen los
	
      "Integration grid (1) equidistant, (2) adaptiv",
      "y  source size (0) sigma, (1) +/- hard edge", 
      "dy source div. (0) sigma, (1) +/- hard edge",
      "z  source size (0) sigma, (1) +/- hard edge", 
      "dz source div. (0) sigma, (1) +/- hard edge",
      "y  source [mm]",
      "dy source [mrad]",
      "z  source [mm]",
      "dz source [mrad]", 
      "dy integration points for equidistant grid",  
      "dymin integration border [mrad]",
      "dymax integration border [mrad]",
      "z integration points for equidistant grid", 
      "dzmin integration border [mrad]",
      "dzmax integration border [mrad]",
	
      "y iterations",           
      "y point number first grid",  
      "y max. point number", 
      "y ratio fmin / fmax",
      "y min. ratio of adjacent points", 
      "z iterations",
      "z point number first grid",  
      "z max. point number", 
      "z ratio:  fmin / fmax",
      "z min. ratio of adjacent points", 
      "read Source from File (1), Sourcebox (0)",
      hier enden die frueheren Bezeichnungen 	
      *******************************/                        
  }, /* ende der Liste */ 

  *labels[]= {"Parameter:", "Defaults", "Dismiss", 
	      "Edit only the values after ':'", }, 
  pvals[73][21], rbuffer[100], *cp; 
  int i, k, itemzahl;

  /* ende der Variablendefinitionen */
		      
  itemzahl= 56;  /* Eintraege in der Liste */
		      
  op= (struct OptionsType *)    &(bl->BLOptions); 
  pop= (struct PSOptionsType *) &(bl->BLOptions.PSO);  
  psp= (struct PSSourceType *)  &(bl->BLOptions.PSO.PSSource);
#ifdef DEBUG 		      
  printf("InitParameterBox: begin\n"); 
#endif
  /* Werte von Variablen auf Strings uebertragen */ 
  k= 0;
  sprintf(pvals[k++], "%g", op->epsilon);
  sprintf(pvals[k++], "%d", op->ifl.iord); 
  sprintf(pvals[k++], "%d", op->ifl.iordsc); 
  sprintf(pvals[k++], "%d", op->ifl.iexpand);  
  sprintf(pvals[k++], "%d", op->ifl.iplmode);
  sprintf(pvals[k++], "%d", bl->src.isrctype);
  
  sprintf(pvals[k++], "%g", op->apr.rpin);
  sprintf(pvals[k++], "%g", op->apr.srcymin);
  sprintf(pvals[k++], "%g", op->apr.srcymax);
  sprintf(pvals[k++], "%g", op->apr.srczmin);
  sprintf(pvals[k++], "%g", op->apr.srczmax);
  
  sprintf(pvals[k++], "%g", op->apr.rpin_ap);
  sprintf(pvals[k++], "%g", op->apr.ymin_ap);
  sprintf(pvals[k++], "%g", op->apr.ymax_ap);
  sprintf(pvals[k++], "%g", op->apr.zmin_ap);
  sprintf(pvals[k++], "%g", op->apr.zmax_ap);
  
  sprintf(pvals[k++], "%g", bl->src.so5.dipcy);
  sprintf(pvals[k++], "%g", bl->src.so5.dipcz);
  sprintf(pvals[k++], "%g", bl->src.so5.dipdisy);
  sprintf(pvals[k++], "%g", bl->src.so5.dipdisz);
  
/*   sprintf(pvals[k++], "%g", bl->src.so5.dipymin); */
/*   sprintf(pvals[k++], "%g", bl->src.so5.dipymax); */
/*   sprintf(pvals[k++], "%g", bl->src.so5.dipzmin); */
/*   sprintf(pvals[k++], "%g", bl->src.so5.dipzmax); */
  
  sprintf(pvals[k++], "%d", op->ifl.igrating);
  sprintf(pvals[k++], "%d", op->ifl.inorm);
  sprintf(pvals[k++], "%d", op->ifl.inorm1);
  sprintf(pvals[k++], "%d", op->ifl.inorm2);
  sprintf(pvals[k++], "%d", op->ifl.matrel);
  
  sprintf(pvals[k++], "%d", bl->src.so1.isrcy);
  sprintf(pvals[k++], "%d", bl->src.so1.isrcdy);
  sprintf(pvals[k++], "%g", bl->src.so1.sigmay);
  sprintf(pvals[k++], "%g", bl->src.so1.sigmayp * 1e3);
  
  sprintf(pvals[k++], "%g", op->xi.ymin * 1e3);
  sprintf(pvals[k++], "%g", op->xi.ymax * 1e3);
  /*  sprintf(pvals[k++], "%d", op->xi.inumy);  */
  /*  sprintf(pvals[k++], "%d", op->xi.itery0); */
  sprintf(pvals[k++], "%d", op->xi.ianzy0);
 /* sprintf(pvals[k++], "%d", op->xi.imaxy);  */
 /* sprintf(pvals[k++], "%g", op->xi.fracy);  */
 /* sprintf(pvals[k++], "%g", op->xi.frac1y); */ 
  
  sprintf(pvals[k++], "%d", bl->src.so1.isrcz);
  sprintf(pvals[k++], "%d", bl->src.so1.isrcdz);
  sprintf(pvals[k++], "%g", bl->src.so1.sigmaz);
  sprintf(pvals[k++], "%g", bl->src.so1.sigmazp * 1e3);
  
  sprintf(pvals[k++], "%g", op->xi.zmin * 1e3);
  sprintf(pvals[k++], "%g", op->xi.zmax * 1e3);
  /* sprintf(pvals[k++], "%d", op->xi.inumz);  */
  /* sprintf(pvals[k++], "%d", op->xi.iterz0); */ 
  sprintf(pvals[k++], "%d", op->xi.ianzz0);
  /* sprintf(pvals[k++], "%d", op->xi.imaxz);  */
  /* sprintf(pvals[k++], "%g", op->xi.fracz);  */
  /* sprintf(pvals[k++], "%g", op->xi.frac1z); */ 
  
  sprintf(pvals[k++], "%d", op->ifl.ibright); 
  sprintf(pvals[k++], "%d", op->ifl.ispline); 
  
  sprintf(pvals[k++], "%g", op->xi.d12_max);
  sprintf(pvals[k++], "%d", op->xi.id12);
  sprintf(pvals[k++], "%d", op->xi.ianz0_cal);
  sprintf(pvals[k++], "%d", op->xi.ianz0_fixed);
  sprintf(pvals[k++], "%d", op->xi.iamp_smooth);
  sprintf(pvals[k++], "%d", op->xi.iord_amp);
  sprintf(pvals[k++], "%d", op->xi.ifm_amp);
/*   sprintf(pvals[k++], "%g", op->xi.amp_change); */
  sprintf(pvals[k++], "%d", op->xi.iord_pha);
  /* sprintf(pvals[k++], "%d", op->xi.iordap); */
   /* modification: 29 Oct 98 14:43:25 flechsig */

 /*  sprintf(pvals[k++], "%g", op->xi.phase_change_1); */
/*   sprintf(pvals[k++], "%g", op->xi.phase_change_2); */
 /*  sprintf(pvals[k++], "%d", op->xi.iphase_curv); */
/*   sprintf(pvals[k++], "%d", op->xi.iphase_pi2); */
  sprintf(pvals[k++], "%d", op->xi.ifm_pha);
  /*  sprintf(pvals[k++], "%g", op->xi.dphi_min); */
  
  sprintf(pvals[k++], "%g", op->xi.distfoc);
  sprintf(pvals[k++], "%d", op->ifl.ipinarr);
  
  sprintf(pvals[k++], "%g", bl->src.pin_yl0);
  sprintf(pvals[k++], "%g", bl->src.pin_yl);
  sprintf(pvals[k++], "%g", bl->src.pin_zl0);
  sprintf(pvals[k++], "%g", bl->src.pin_zl);
#ifdef DEBUG  
  printf("InitParameterBox: parameter : %d\n", k); /* fuer debug */
#endif
  /*****************************************
    ende fg34 */
  /*
    sprintf(pvals[k++], "%19d", pop->intmod);  
    sprintf(pvals[k++], "%19d", psp->yhard); 
    sprintf(pvals[k++], "%19d", psp->dyhard);    
    sprintf(pvals[k++], "%19d", psp->zhard); 
    sprintf(pvals[k++], "%19d", psp->dzhard);    
    sprintf(pvals[k++], "%19g", psp->sigy);    
    sprintf(pvals[k++], "%19g", psp->sigdy    * 1e3);    
    sprintf(pvals[k++], "%19g", psp->sigz);    
    sprintf(pvals[k++], "%19g", psp->sigdz    * 1e3);    
    sprintf(pvals[k++], "%19d", pop->ndyfix); 
    sprintf(pvals[k++], "%19g", pop->dyminfix * 1e3); 
    sprintf(pvals[k++], "%19g", pop->dymaxfix * 1e3);    
    sprintf(pvals[k++], "%19d", pop->ndzfix); 
    sprintf(pvals[k++], "%19g", pop->dzminfix * 1e3); 
    sprintf(pvals[k++], "%19g", pop->dzmaxfix * 1e3);
    
    sprintf(pvals[k++], "%19d", pop->itery0);    
    sprintf(pvals[k++], "%19d", pop->ianzy0);
    sprintf(pvals[k++], "%19d", pop->imaxy);    
    sprintf(pvals[k++], "%19g", pop->fracy);    
    sprintf(pvals[k++], "%19g", pop->frac1y); 
    sprintf(pvals[k++], "%19d", pop->iterz0); 
    sprintf(pvals[k++], "%19d", pop->ianzz0);    
    sprintf(pvals[k++], "%19d", pop->imaxz); 
    sprintf(pvals[k++], "%19g", pop->fracz); 
    sprintf(pvals[k++], "%19g", pop->frac1z);  
    **************************************************/
  if ((neu != NULL) && (*neu != 0)) 
    {
#ifdef DEBUG  
      printf("InitParameterBox: initparameterbox value change\n");   
#endif      
      /* geaenderten Wert aus der Edit- Zeile entnehmen und den
	 betreffenden String ersetzen, die Zahl dient der Zuordnung */
      
      sscanf(neu, "%d %s", &k, &rbuffer);     
      cp = strrchr(neu, ':'); cp++; 
      sprintf(pvals[k], "%s", cp);
      printf( "No: %d new: %s\n", k, pvals[k]);
      
      /* alle Werte wieder von den Strings uebernehmen */
      
      k= 0;
      sscanf(pvals[k++], "%lf", &op->epsilon);
      sscanf(pvals[k++], "%d",  &op->ifl.iord);
      sscanf(pvals[k++], "%d",  &op->ifl.iordsc);
      sscanf(pvals[k++], "%d",  &op->ifl.iexpand);  
      sscanf(pvals[k++], "%d",  &op->ifl.iplmode);
      sscanf(pvals[k++], "%d",  &bl->src.isrctype);
      
      sscanf(pvals[k++], "%lf", &op->apr.rpin);
      sscanf(pvals[k++], "%lf", &op->apr.srcymin);
      sscanf(pvals[k++], "%lf", &op->apr.srcymax);
      sscanf(pvals[k++], "%lf", &op->apr.srczmin);
      sscanf(pvals[k++], "%lf", &op->apr.srczmax);
      
      sscanf(pvals[k++], "%lf", &op->apr.rpin_ap);
      sscanf(pvals[k++], "%lf", &op->apr.ymin_ap);
      sscanf(pvals[k++], "%lf", &op->apr.ymax_ap);
      sscanf(pvals[k++], "%lf", &op->apr.zmin_ap);
      sscanf(pvals[k++], "%lf", &op->apr.zmax_ap);
      
      sscanf(pvals[k++], "%lf", &bl->src.so5.dipcy);
      sscanf(pvals[k++], "%lf", &bl->src.so5.dipcz);
      sscanf(pvals[k++], "%lf", &bl->src.so5.dipdisy);
      sscanf(pvals[k++], "%lf", &bl->src.so5.dipdisz);
      
 /*      sscanf(pvals[k++], "%lf", &bl->src.so5.dipymin); */
/*       sscanf(pvals[k++], "%lf", &bl->src.so5.dipymax); */
/*       sscanf(pvals[k++], "%lf", &bl->src.so5.dipzmin); */
/*       sscanf(pvals[k++], "%lf", &bl->src.so5.dipzmax); */
      
      sscanf(pvals[k++], "%d",  &op->ifl.igrating);
      sscanf(pvals[k++], "%d",  &op->ifl.inorm);
      sscanf(pvals[k++], "%d",  &op->ifl.inorm1);
      sscanf(pvals[k++], "%d",  &op->ifl.inorm2);
      sscanf(pvals[k++], "%d",  &op->ifl.matrel);
      
      sscanf(pvals[k++], "%d",  &bl->src.so1.isrcy);
      sscanf(pvals[k++], "%d",  &bl->src.so1.isrcdy);
      sscanf(pvals[k++], "%lf", &bl->src.so1.sigmay);
      sscanf(pvals[k++], "%lf", &bl->src.so1.sigmayp);
      bl->src.so1.sigmayp*= 1e-3;
      sscanf(pvals[k++], "%lf", &op->xi.ymin); op->xi.ymin*= 1e-3;
      sscanf(pvals[k++], "%lf", &op->xi.ymax); op->xi.ymax*= 1e-3;
   /*    sscanf(pvals[k++], "%d",  &op->xi.inumy);*/
   /*    sscanf(pvals[k++], "%d",  &op->xi.itery0);*/
      sscanf(pvals[k++], "%d",  &op->xi.ianzy0);
   /*    sscanf(pvals[k++], "%d",  &op->xi.imaxy);*/
   /*    sscanf(pvals[k++], "%lf", &op->xi.fracy);*/
   /*    sscanf(pvals[k++], "%lf", &op->xi.frac1y);*/
      
      sscanf(pvals[k++], "%d",  &bl->src.so1.isrcz);
      sscanf(pvals[k++], "%d",  &bl->src.so1.isrcdz);
      sscanf(pvals[k++], "%lf", &bl->src.so1.sigmaz);
      sscanf(pvals[k++], "%lf", &bl->src.so1.sigmazp);
      bl->src.so1.sigmazp*= 1e-3;
      sscanf(pvals[k++], "%lf", &op->xi.zmin); op->xi.zmin*= 1e-3;
      sscanf(pvals[k++], "%lf", &op->xi.zmax); op->xi.zmax*= 1e-3;
    /*  sscanf(pvals[k++], "%d",  &op->xi.inumz);*/
    /*  sscanf(pvals[k++], "%d",  &op->xi.iterz0);*/
      sscanf(pvals[k++], "%d",  &op->xi.ianzz0);
    /*  sscanf(pvals[k++], "%d",  &op->xi.imaxz);*/
    /*  sscanf(pvals[k++], "%lf", &op->xi.fracz);*/
    /*  sscanf(pvals[k++], "%lf", &op->xi.frac1z);*/
      
      sscanf(pvals[k++], "%d",  &op->ifl.ibright); 
      sscanf(pvals[k++], "%d",  &op->ifl.ispline); 
      
      sscanf(pvals[k++], "%lf", &op->xi.d12_max);
      sscanf(pvals[k++], "%d",  &op->xi.id12);
      sscanf(pvals[k++], "%d",  &op->xi.ianz0_cal);
      sscanf(pvals[k++], "%d",  &op->xi.ianz0_fixed);
      sscanf(pvals[k++], "%d",  &op->xi.iamp_smooth);
      sscanf(pvals[k++], "%d",  &op->xi.iord_amp);
      sscanf(pvals[k++], "%d",  &op->xi.ifm_amp);
      /*     sscanf(pvals[k++], "%lf", &op->xi.amp_change); */
      sscanf(pvals[k++], "%d",  &op->xi.iord_pha);
   /*    sscanf(pvals[k++], "%d",  &op->xi.ifm_pha);*/
    /*   sscanf(pvals[k++], "%lf", &op->xi.phase_change_1); */
/*       sscanf(pvals[k++], "%lf", &op->xi.phase_change_2); */
  /*     sscanf(pvals[k++], "%d",  &op->xi.iphase_curv); */
/*       sscanf(pvals[k++], "%d",  &op->xi.iphase_pi2); */
      sscanf(pvals[k++], "%d",  &op->xi.ifm_pha);
/*      sscanf(pvals[k++], "%lf", &op->xi.dphi_min);*/
      
      sscanf(pvals[k++], "%lf", &op->xi.distfoc);
      sscanf(pvals[k++], "%d",  &op->ifl.ipinarr);
      
      sscanf(pvals[k++], "%lf", &bl->src.pin_yl0);
      sscanf(pvals[k++], "%lf", &bl->src.pin_yl);
      sscanf(pvals[k++], "%lf", &bl->src.pin_zl0);
      sscanf(pvals[k++], "%lf", &bl->src.pin_zl);
      
      /**************** ende von Johannes fg34.par */
      /*
	sscanf(pvals[ 1], "%d",  &op->ifl.iord);  
	sscanf(pvals[ 2], "%d",  &pop->intmod);  
	sscanf(pvals[ 3], "%d",  &psp->yhard); 
	sscanf(pvals[ 4], "%d",  &psp->dyhard);    
	sscanf(pvals[ 5], "%d",  &psp->zhard); 
	sscanf(pvals[ 6], "%d",  &psp->dzhard);    
	sscanf(pvals[ 7], "%lf", &psp->sigy);    
	sscanf(pvals[ 8], "%lf", &psp->sigdy);    
	sscanf(pvals[ 9], "%lf", &psp->sigz);                         
	sscanf(pvals[10], "%lf", &psp->sigdz);    
	sscanf(pvals[11], "%d",  &pop->ndyfix); 
	sscanf(pvals[12], "%lf", &pop->dyminfix); 
	sscanf(pvals[13], "%lf", &pop->dymaxfix);    
	sscanf(pvals[14], "%d",  &pop->ndzfix); 
	sscanf(pvals[15], "%lf", &pop->dzminfix); 
	sscanf(pvals[16], "%lf", &pop->dzmaxfix); 
	
	sscanf(pvals[17], "%d",  &pop->itery0);    
	sscanf(pvals[18], "%d",  &pop->ianzy0);    
	sscanf(pvals[19], "%d",  &pop->imaxy);    
	sscanf(pvals[20], "%lf", &pop->fracy);    
	sscanf(pvals[21], "%lf", &pop->frac1y); 
	sscanf(pvals[22], "%d",  &pop->iterz0); 
	sscanf(pvals[23], "%d",  &pop->ianzz0);    
	sscanf(pvals[24], "%d",  &pop->imaxz); 
	sscanf(pvals[25], "%lf", &pop->fracz); 
	sscanf(pvals[26], "%lf", &pop->frac1z);   */
      /* !!!!! wichtig !!!! die Winkel werden in rad gespeichert */
      /*   psp->sigdy*= 1e-3;  
	   psp->sigdz*= 1e-3;  
	   pop->dyminfix*= 1e-3;
	   pop->dymaxfix*= 1e-3;
	   pop->dzminfix*= 1e-3;
	   pop->dzmaxfix*= 1e-3;
	   *************************************************      */
    } /* end if neu */  
  
  /* ausgelesen und wieder eingelesen beendet, Listeneintraege
     erzeugen */
 /* modification: 31 Oct 97 08:29:45 flechsig */
  if (op->xi.iord_amp > 0)
    {
      printf("\n!!!iord_amp must be negative!!!\n");
      op->xi.iord_amp*= -1;
      printf("set iord_amp to %d\n\n", op->xi.iord_amp);
    }
  if (op->xi.iord_pha > 0)
    {
      printf("\n!!!iord_pha must be negative!!!\n");
      op->xi.iord_pha*= -1;
      printf("set iord_pha to %d\n\n", op->xi.iord_pha);
    }

  for (i= 0; i < itemzahl; i++) 
    {
      sprintf(rbuffer, "%d  %s: %s", i, inhalt[i], pvals[i]);    
      printf("%s\n", rbuffer);    /* debug */
      str[i]= XmStringCreateLocalized(rbuffer);  
    }    
  
  /* Button- XmStrings erzeugen */
  
  for (i= 0; i < 4; i++) str1[i]= XmStringCreateLocalized(labels[i]);     
  
  /* Parameterbox fuellen */
  
  XtVaSetValues(	widget_array[kEParameterBox],
			XmNlistLabelString,	 str1[0], 
			XmNselectionLabelString, str1[3],  
			XmNapplyLabelString, 	 str1[1],  
			XmNcancelLabelString, 	 str1[2],   
			XmNlistItems, 		 str,
			XmNlistItemCount, 	 itemzahl,
			XmNlistVisibleItemCount, 20,
			NULL);  
  
  /* Speicherplatz fuer Xstrings freigeben */  
#ifdef DEBUG  
  printf("InitParameterBox: free memory: %d\n"); /* fuer debug */
#endif  
  i= itemzahl; while(--i >= 0) XmStringFree(str[i]);  /*XtFree(str );*/
  i= 4;        while(--i >= 0) XmStringFree(str1[i]); /*XtFree(str1);*/
}  /* end initparameterbox */


void InitFileBox(struct PHASEset *x)  
{
  int i;
  char TextField[13][MaxPathLength];
  XmString label;  	
        
  sprintf(TextField[0], "%s", x->matrixname);    
  sprintf(TextField[1], "%s", x->mapname);    
  sprintf(TextField[2], "%s", x->sourceraysname);    
  sprintf(TextField[3], "%s", x->imageraysname);    
  sprintf(TextField[4], "%s", x->intersecname);    
  sprintf(TextField[5], "%s", x->geometryname);    
  sprintf(TextField[6], "%s", x->elementname);     
  sprintf(TextField[7], "%s", x->sourcepckname);    
  sprintf(TextField[8], "%s", x->geometrypckname);    
  sprintf(TextField[9], "%s", x->elementpckname);     
  sprintf(TextField[10], "%s", x->pssourcename);     
  sprintf(TextField[11], "%s", x->printpclname);     
  sprintf(TextField[12], "%s", x->optipckname);     
  for (i= 0; i < 13; i++)
    {	

      label= XmStringCreateLocalized(TextField[i]);    

      set_something(widget_array[kFFileButton1+ i], XmNlabelString, label);  
    }
  XmStringFree(label);   
}   

void ActivateFileSelection(int task, char *pattern)     
{
  XmString topic;

  FetchWidget(kFileSelectionDialog, "FileSelectionDialog");  
  if (ActualTask != task)
    {
      ActualTask= task;    			/* globale Variable */    
      topic= XmStringCreateLocalized(pattern);  
      set_something(widget_array[kFileSelectionDialog], XmNpattern, topic); 
      XmStringFree(topic); 
    }
  XtManageChild(widget_array[kFileSelectionDialog]); 
  /*  FileSelectionProc callback der dialog box */ 
}

void UpdateFilenames(struct PHASEset *x) 
/* callback der Fileselection */
/* modification: 17 Oct 97 14:12:36 flechsig */
{
  int i; 
  XmString label;  
  /*  char     **lab;                 < 15.12.99 UF */
  char *fname= NULL, *lab[13];       /* 15.12.99 UF */

  for (i= 0; i < 13; i++)      /* liest + konvertiert Tastenlabel in *lab */
    {	
      get_something(widget_array[kFFileButton1+ i], XmNlabelString, &label);

      /* 15.12.99 das geht nicht     
	 if (!XmStringGetLtoR(label, XmFONTLIST_DEFAULT_TAG, &lab[i])) 
	 return; */
      if (!XmStringGetLtoR(label, XmFONTLIST_DEFAULT_TAG, &fname)) 
	 return;
      lab[i]= fname;    
           /*delversion(lab[i]);*/ 
      printf("%s\n", lab[i]);  
      XmStringFree(label);
    }                              
  strcpy(x->matrixname, 	lab[0]);    
  strcpy(x->mapname,    	lab[1]);    
  strcpy(x->sourceraysname, 	lab[2]);    
  strcpy(x->imageraysname, 	lab[3]);    
  strcpy(x->intersecname, 	lab[4]);    
  strcpy(x->geometryname, 	lab[5]);    
  strcpy(x->elementname, 	lab[6]);    
  strcpy(x->sourcepckname, 	lab[7]);    
  strcpy(x->geometrypckname, 	lab[8]);    
  strcpy(x->elementpckname, 	lab[9]);    
  strcpy(x->pssourcename, 	lab[10]);    
  strcpy(x->printpclname, 	lab[11]);  
  strcpy(x->optipckname, 	lab[12]);  
  for (i= 0; i < 13; i++) 
    XtFree(lab[i]);
}  /* end UpdateFilenames */


void ExpandFileNames(struct PHASEset *x, char *pfad)   
     /* beseitigt extension und Versionsnummer von path      */
     /* setzt neue namen fuer Einzelelemente                 */
     /* ICON- Taste (pfad == \0), save as,  AddBLElement     */
     /* Uwe 3.6.96 					     */
     /* last mod. 26.7.96 				     */
     /* 26.7.96 quellnamen, image werden ausgenommen 	     */
/* modification: 17 Oct 97 13:11:13 flechsig */
{
  XmString label, lfeld[13];  
  int i;
  char *name, *ch, *ch1, puffer[MaxPathLength], puffer1[MaxPathLength],
    exfeld[13][6]= {".omx",".map",".inp",".out",".isec",".datg",".date",
		    ".pcks",".pckg",".pcke",".brig",".pcl",".pcko",};
 
  if (*pfad == '\0') /* icon */
    {
      /* get_something(widget_array[kFFileButton1], XmNlabelString, &label);
      name= DXmCvtCStoFC(label, &bc, &status);                          
      strcpy(puffer, name);       
      XtFree(name); 
      auf beamlinename geaendert */

      strcpy(puffer, x->beamlinename); 
      FnameBody(puffer);
           
      for (i= 0; i < 13; i++) 
	{
	  strcpy(puffer1, puffer); strcat(puffer1, exfeld[i]); 
	  label= XmStringCreateLocalized(puffer1); 
	  set_something(widget_array[kFFileButton1+ i], 
			XmNlabelString, label);
	}
      XmStringFree(label); 
    } else
      {                                     
	strcpy(puffer, pfad);       
	FnameBody(puffer);
	/* strrchr-> strchr 12.6.96 */
	strcpy(puffer1, puffer); 
	sprintf(x->matrixname,	  "%s%s\0", puffer1, exfeld[0]); 
	sprintf(x->mapname, 	  "%s%s\0", puffer1, exfeld[1]);    
	/*  sprintf(x->sourceraysname,  "%s%s\0", puffer1, exfeld[2]);  */  
	/*  sprintf(x->imageraysname,   "%s%s\0", puffer1, exfeld[3]);  */  
	sprintf(x->intersecname, 	  "%s%s\0", puffer1, exfeld[4]);    
	sprintf(x->geometryname, 	  "%s%s\0", puffer1, exfeld[5]);    
	sprintf(x->elementname, 	  "%s%s\0", puffer1, exfeld[6]);    
	/*  sprintf(x->sourcepckname,   "%s%s\0", puffer1, exfeld[7]);  */ 
	sprintf(x->geometrypckname, "%s%s\0", puffer1, exfeld[8]);    
	sprintf(x->elementpckname,  "%s%s\0", puffer1, exfeld[9]);    
	/*  sprintf(x->pssourcename, 	  "%s%s\0", puffer1, exfeld[10]); */   
	/* sprintf(x->printpclname, 	  "%s%s\0", puffer1, exfeld[11]); */ 
	/* sprintf(x->optipckname,     "%s%s\0", puffer1, exfeld[12]); */ 
      } 
}

/* initialisiert ein indexfeld mit n Integern die als variable Liste  	*/
/* eingelesen werden 	  						*/ 
void SetIndexField(int *field, int n,...)  
{
  va_list v;
  int *ip; 
   
  ip= field; 
  va_start(v, n);     		/* n- letzter Pflichtparameter */
  for (; n> 0; n--)
    *ip++= va_arg(v, int);
  va_end(v);     
} /* end SetIndexField */
     
void InitSourceBox(struct datset *x, struct BeamlineType *bl, int source)   
     /* 3. 5. 96 					*/
     /* last modif. 26.7.96 				*/
/* last modification: 11 Apr 97 15:54:55 flechsig */
/* last modification: 24 Sep 97 14:47:04 flechsig */

{
  int i, header, IFeld[8], sou;
  struct RayType *Raysout;  
  XmString label;
  Widget w;  
  char TextField[8][40],            /* 8 editfelder */
   
    *LabelField1 [35] =  {	"", "-> points",         		/*0,1*/
				"height [mm]",     "width [mm]",   	/*2,3*/
				"v. div. [mrad]", "h. div. [mrad]", 
				"ray number", 				/* 6 */
				"length [mm]", "lambda [nm]",           /*7,8*/
				
				"yi [mm]",   "zi [mm]", 
				"dyi [rad]", "dzi [rad]",             /*11,12*/
				
				"yo [mm]", "zo [mm]",
				"dyi [mrad]", "dyo [mrad]", 
				"dzi [mrad]", "dzo [mrad]",          /*17,18*/
				
				"sigy [mm]",    "sigdyp [mrad]",     /*19,20*/
				"dymin [mrad]", "dymax [mrad]", 
				"sigz [mm]",    "sigdzp [mrad]", 
				"dzmin [mrad]", "dzmax [mrad]",      /*25,26*/
				
				"ymin [mm]", "ymax [mm]",
				"zmin [mm]", "zmax [mm]",
				"y points",  "z points", 
				
				"Delta z [mm]", };                     /*33*/
 
    printf("initsource0 wdgnumber: %d, bl->RTSource.QuellTyp: %c\n", 
	   source, bl->RTSource.QuellTyp);   
    
    switch (source) 
      {
      case kESDipolSourceButton: 	sou= 'D'; break;
      case kESUndulatorSourceButton: 	sou= 'U'; break;  
      case kESUndulatorSISButton: 	sou= 'L'; break;  /*23.11.98 UF*/
      case kESUndulatorSIMButton: 	sou= 'M'; break;  
      case kESundulatorSourceButton: 	sou= 'u'; break;
      case kESSR2Button: 		sou= 'S'; break;  
      case kESPhaseSpaceButton:	        sou= 'P'; break;  
      case kESPhaseSpaceImageButton: 	sou= 'I'; break; 
      case kESFileButton:               sou= 'F'; break;
      case kESDefaults:
      case kESourceMenuButton:		sou= bl->RTSource.QuellTyp; 
		break;
      case kESRayTraceButton:
      default : 			sou= 'H'; 
      }
    bl->RTSource.QuellTyp= sou;
       
    for (i= 0; i< 8; i++) TextField[i][0]= '\0';
    printf("initsourcebox with source = %c\n", sou);  
    set_something(widget_array[kEST2], XmNsensitive, True);
 
    switch (sou) {
    case 'F':
      w= widget_array[kESFileButton];
      SetIndexField(IFeld, 8, 0, 0, 0, 0, 0, 0, 0, 0); 
      for (i= 0; i< 8; i++) sprintf(TextField[i], "%s", "***"); 
      break; 
    case 'D':
      w= widget_array[kESDipolSourceButton];  
      header= 1;
      SetIndexField(IFeld, 8, 2, 4, 3, 5, 6, 0, 0, 0);      
      sprintf(TextField[0], "%f", bl->RTSource.Quelle.DipolSource.sigy);    
      sprintf(TextField[1], "%f", bl->RTSource.Quelle.DipolSource.sigdy);    
      sprintf(TextField[2], "%f", bl->RTSource.Quelle.DipolSource.sigz);    
      sprintf(TextField[3], "%f", bl->RTSource.Quelle.DipolSource.dz);    
      sprintf(TextField[4], "%d", bl->RTSource.raynumber);    
      break;    
    case 'U':  
      w= widget_array[kESUndulatorSourceButton]; 
      header= 6;
      SetIndexField(IFeld, 8, 7, 8, 6, 0, 0, 0, 0, 0);      
      sprintf(TextField[0], "%f", bl->RTSource.Quelle.UndulatorSource.length);
      sprintf(TextField[1], "%f", bl->BLOptions.lambda* 1e6);
      sprintf(TextField[2], "%d", bl->RTSource.raynumber);   
      set_something(widget_array[kEST2], XmNsensitive, False); 
      break;     
    case 'u':  
      w= widget_array[kESundulatorSourceButton]; 
      SetIndexField(IFeld, 8, 7, 8, 6, 0, 0, 0, 0, 0);      
      sprintf(TextField[0], "%f", bl->RTSource.Quelle.UndulatorSource.length);
      sprintf(TextField[1], "%f", bl->BLOptions.lambda* 1e6);
      sprintf(TextField[2], "%d", bl->RTSource.raynumber);    
      set_something(widget_array[kEST2], XmNsensitive, False); 
      break;  
      
    /*23.11.98 */  
    case 'L':  
      w= widget_array[kESUndulatorSISButton]; 
      header= 6;
      SetIndexField(IFeld, 8, 7, 8, 6, 33, 0, 0, 0, 0);      
      sprintf(TextField[0], "%f", bl->RTSource.Quelle.UndulatorSource.length);
      sprintf(TextField[1], "%f", bl->BLOptions.lambda* 1e6);
      sprintf(TextField[2], "%d", bl->RTSource.raynumber); 
      sprintf(TextField[3], "%f", bl->RTSource.Quelle.UndulatorSource.deltaz);  
      set_something(widget_array[kEST2], XmNsensitive, False); 
      break;     
        case 'M':  
      w= widget_array[kESUndulatorSIMButton]; 
      header= 6;
      SetIndexField(IFeld, 8, 7, 8, 6, 33, 0, 0, 0, 0);      
      sprintf(TextField[0], "%f", bl->RTSource.Quelle.UndulatorSource.length);
      sprintf(TextField[1], "%f", bl->BLOptions.lambda* 1e6);
      sprintf(TextField[2], "%d", bl->RTSource.raynumber); 
      sprintf(TextField[3], "%f", bl->RTSource.Quelle.UndulatorSource.deltaz);  
      set_something(widget_array[kEST2], XmNsensitive, False); 
      break;        
    case 'S':
      w= widget_array[kESSR2Button]; 
      Raysout= bl->RESULT.RESUnion.Rays;  
      SetIndexField(IFeld, 8, 9, 13, 10, 14, 15, 16, 17, 18); 
      sprintf(TextField[0], "%lf", bl->RTSource.Quelle.SRSource.y);
      sprintf(TextField[2], "%lf", bl->RTSource.Quelle.SRSource.z);
      sprintf(TextField[4], "%lf", bl->RTSource.Quelle.SRSource.dy);  
      sprintf(TextField[6], "%lf", bl->RTSource.Quelle.SRSource.dz);
      if (Raysout != NULL)  /* noch nichts berechnet */
	{ 
	  sprintf(TextField[1], "%lf", Raysout->y);    
	  sprintf(TextField[3], "%lf", Raysout->z); 
	  sprintf(TextField[5], "%lf", Raysout->dy* 1e3);
	  sprintf(TextField[7], "%lf", Raysout->dz* 1e3);  
	} 
      break; 
    case 'P':
      w= widget_array[kESPhaseSpaceButton]; 
      SetIndexField(IFeld, 8, 19, 20, 21, 22, 23, 24, 25, 26); 
      sprintf(TextField[0], "%f", x->sigmay);
      sprintf(TextField[1], "%f", x->sigmayp);
      sprintf(TextField[2], "%f", x->ymin); 
      sprintf(TextField[3], "%f", x->ymax); 
      sprintf(TextField[4], "%f", x->sigmaz); 
      sprintf(TextField[5], "%f", x->sigmazp);
      sprintf(TextField[6], "%f", x->zmin);
      sprintf(TextField[7], "%f", x->zmax);   
      break;    
    case 'I':                          /*hard edge */
      w= widget_array[kESPhaseSpaceImageButton]; 
      SetIndexField(IFeld, 8, 27, 28, 29, 30, 31, 32, 0, 0); 
      sprintf(TextField[0], "%f", bl->RTSource.Quelle.PSImage.ymin);    
      sprintf(TextField[1], "%f", bl->RTSource.Quelle.PSImage.ymax);    
      sprintf(TextField[2], "%f", bl->RTSource.Quelle.PSImage.zmin);   
      sprintf(TextField[3], "%f", bl->RTSource.Quelle.PSImage.zmax);   
      sprintf(TextField[4], "%d", bl->RTSource.Quelle.PSImage.iy);   
      sprintf(TextField[5], "%d", bl->RTSource.Quelle.PSImage.iz); 
      /* sprintf(TextField[6], "%f", x->xlam_test);   */ 
      break; 
    case 'H':
    default:
      w= widget_array[kESRayTraceButton]; 
      SetIndexField(IFeld, 8, 2, 1, 3, 1, 4, 1, 5, 1);  
      sprintf(TextField[0], "%f", bl->RTSource.Quelle.HardEdgeSource.disty);
      sprintf(TextField[1], "%d", bl->RTSource.Quelle.HardEdgeSource.iy);    
      sprintf(TextField[2], "%f", bl->RTSource.Quelle.HardEdgeSource.distz);  
      sprintf(TextField[3], "%d", bl->RTSource.Quelle.HardEdgeSource.iz);    
      sprintf(TextField[4], "%f", bl->RTSource.Quelle.HardEdgeSource.divy);   
      sprintf(TextField[5], "%d", bl->RTSource.Quelle.HardEdgeSource.idy);    
      sprintf(TextField[6], "%f", bl->RTSource.Quelle.HardEdgeSource.divz);   
      sprintf(TextField[7], "%d", bl->RTSource.Quelle.HardEdgeSource.idz);    
      break;   
    }
    /* set history */
    XtVaSetValues(widget_array[kESOptMenu], XmNmenuHistory, w, NULL);
   
    for (i= 0; i< 8; i++)
      {	
	set_something(widget_array[kEST1+ i], XmNvalue, TextField[i]);      

	label= XmStringCreateLocalized(LabelField1[IFeld[i]]); 

	set_something(widget_array[kEST1Label+ i], XmNlabelString, label);  
      }    /* */
    /*  label= DXmCvtFCtoCS(HeaderField[header], &bc, &status);     
    /* set_something(widget_array[kESInputLabel], XmNlabelString, label);  */
    XmStringFree(label);      
} /* end initsourcebox */

void InitGeometryBox(struct gdatset *gx)  
/* modification: 20 Feb 98 09:23:35 flechsig */
/* modification: 13 Mar 98 08:40:42 flechsig */
{
  int i;
  double cff, teta, fi;
  char TextField[4][40], *text;
  XmString label;	
  char LabelField1 [5][20] =	{ "Prec. [mm]", "Succ. [mm]", 
				  "theta [deg]", "cff (PGM)",
                                  "geometry", 
  };

  teta= fabs(gx->theta0* PI/ 180.0);   /* theta */

/* modification: 13 Mar 98 08:38:13 flechsig */
  text= XmTextGetString(widget_array[kEBLT31a]);    
  sscanf(text, "%lf", &gx->lambda); 
  gx->lambda*= 1e-6;
  XtFree(text); 

  fi  = (double)(gx->inout)* asin(gx->lambda* gx->xdens[0]/
				  (2.0* cos(teta)));
  cff= cos(fi- teta)/ cos(fi+ teta);    /* cos(beta)/ cos(alpha); */

  sprintf(TextField[0], "%f", gx->r    );    
  sprintf(TextField[1], "%f", gx->rp   );    
  sprintf(TextField[2], "%f", gx->theta0);  
  sprintf(TextField[3], "%.3f", cff);    
    	        
  for (i= 0; i< 4; i++)
    {	
      set_something(widget_array[kEGT1+ i], XmNvalue, TextField[i]);  

      label= XmStringCreateLocalized(LabelField1[i]);

      set_something(widget_array[kEGT1Label+ i], XmNlabelString, label);  
    }     

  label= XmStringCreateLocalized(LabelField1[4]);

  set_something(widget_array[kEGInputLabel], XmNlabelString, label); 	
  XmStringFree(label); 
  XmToggleButtonSetState(widget_array[kEGNITranslation], 
			 gx->iflag == 1, FALSE);   
} /* end InitGeometryBox */

void InitOElementBox(struct mdatset *x, struct gdatset *y, int sw)  
/* sw wird mit art initialisiert */    
/* wird von activate_proc mit der widget- nummer der Taste aufgerufen */
/* last modification: 09 Jun 97 15:58:12 flechsig */
/* last modification: 11 Apr 97 15:37:07 flechsig */
/* last modification: 17 Jun 97 08:27:46 flechsig */
/* last modification: 30 Sep 97 08:24:02 flechsig */
{
  static int ActualTask; 
  int i, ih, imax;
  char TextField[12][40];
  XmString label; 	
  Widget w;
  char LabelField1 [19][50] = {  
    "source [mm]", "r [mm]",
    "image [mm]",  "rho [mm]",
    "diff. order", "xdens[0]",
    "xdens[1]",    "xdens[2]", 
    "xdens[3]",    "xdens[4]", 
    "theta",     "dummy12",  
    "toroidal mirror", 
    "toroidal grating",
    "variable linespace grating",  
    "elliptical mirror (r, rho- ignored)",  
    "plane- ellipt. mirror (r, rho- ignored)", 
    "aperture/ slit (only w, l are valid)" 
  };
       
  if ((sw != kEOEDefaults) && (sw != kFFileBoxOK)) 
    {      
#ifdef DEBUG
      printf(" InitOElementBox: %d \n", sw);
#endif      
      
      switch (sw) {  
      case  kEOETM:    ih= 12; imax= 11;  break;
      case  kEOETG:    ih= 13; imax= 11;  break;  
      case  kEOEVLSG:  ih= 14; imax= 11;  break;  
      case  kEOEElli:  ih= 15; imax= 11;  break; 
      case  kEOEPElli: ih= 16; imax= 11;  break;  
      case  kEOESlit:  ih= 17; imax= 11;  break;  
      default:         ih= 14; imax= 11; sw= kEOEVLSG;   
      }
    } else  
      {
        ih= 14; imax= 11; 
	sw= kEOEVLSG;
	printf("defaults nur fuer tovlsg 09 Jun 97\n");
	/*w= widget_array[sw];*/
      }
  w= widget_array[sw];    /* verschoben 24.11.99 */            
#ifdef DEBUG
  printf("initialisiere Elementbox mit typ %d\n", sw);    
#endif 
  sprintf(TextField[0], "%.3f", x->r1);   
  sprintf(TextField[1], "%.3f", x->rmi);      
  sprintf(TextField[2], "%.3f", x->r2);
  sprintf(TextField[3], "%.3f", x->rho);          
  sprintf(TextField[4], "%d", y->inout);      	   
  sprintf(TextField[5], "%.5f", y->xdens[0]);    
  sprintf(TextField[6], "%.5f", y->xdens[1]);    
  sprintf(TextField[7], "%.5f", y->xdens[2]);    
  sprintf(TextField[8], "%.5f", y->xdens[3]);    
  sprintf(TextField[9], "%.5f", y->xdens[4]); 
  sprintf(TextField[10], "%.5f", x->alpha); 

  for (i= 0; i < imax; i++)
    {	
      set_something(widget_array[kEOET1+ i], XmNvalue, TextField[i]);      
      label= XmStringCreateLocalized(LabelField1[i]);    
      set_something(widget_array[kEOET1Label+ i], XmNlabelString, label);  
    }
  label= XmStringCreateLocalized(" ");  
  for (i= imax; i < 12; i++)
    {	
      set_something(widget_array[kEOET1+ i], XmNvalue, " "); 
      set_something(widget_array[kEOET1Label+ i], XmNlabelString, label);  
    }    
  label= XmStringCreateLocalized(LabelField1[ih]);  
  set_something(widget_array[kEOEInputLabel], XmNlabelString, label); 
  XmStringFree(label);     
  /* set history */
  printf("InitOElementBox: vor history\n"); 
  XtVaSetValues(widget_array[kEOOptMenu], XmNmenuHistory, w, NULL); 
} /* end InitOElementBox */

void GetSource(struct BeamlineType *bl)  
     /****************************************************************/
     /* wertet die sourcebox aus					*/
     /* 20.5.96                              			*/  
     /* letzte Aenderung: 1.8.96 					*/     
/* last modification: 24 Sep 97 14:52:21 flechsig */
/* 24.11.98 UF*/
{
  char *textf[8];
  struct datset x; 
  int i;
 
  printf("GetSource, type: %c\n", bl->RTSource.QuellTyp); 
   
  bl->BLOptions.wrSource= 
    (XmToggleButtonGetState(widget_array[kESFile]) == TRUE) ? 1 : 0; 
  /* geschrieben wird die quelle von makertrace */
       
  for (i= 0; i< 8; i++) textf[i]= XmTextGetString(widget_array[kEST1+ i]);  
  /* alles eingelesen */
   
  switch (bl->RTSource.QuellTyp) 
    {
    case 'F':
      printf("source from file not yet fully implemented!\n");
      /*   bl->RTSource.SourceRays= (struct RayType *)     */
      ReadRayFile(PHASESet.sourceraysname, &bl->RTSource.raynumber, 
		  &bl->RESULT); 
      if (bl->RTSource.SourceRays != NULL) free(bl->RTSource.SourceRays);
      if ((bl->RTSource.SourceRays= (struct RayType *)
	   malloc(bl->RTSource.raynumber* sizeof(struct RayType))) == NULL)
	{    fprintf(stderr, "malloc error in ReadRayFile\n"); exit(-1); }  
      memcpy(bl->RTSource.SourceRays, bl->RESULT.RESUnion.Rays,
	     sizeof(struct RayType)* bl->RTSource.raynumber);
      break; 
    case 'H': 	
      sscanf(textf[0], "%lf", &bl->RTSource.Quelle.HardEdgeSource.disty);   
      sscanf(textf[1],  "%d", &bl->RTSource.Quelle.HardEdgeSource.iy);     
      sscanf(textf[2], "%lf", &bl->RTSource.Quelle.HardEdgeSource.distz);   
      sscanf(textf[3],  "%d", &bl->RTSource.Quelle.HardEdgeSource.iz);     
      sscanf(textf[4], "%lf", &bl->RTSource.Quelle.HardEdgeSource.divy);   
      sscanf(textf[5],  "%d", &bl->RTSource.Quelle.HardEdgeSource.idy);     
      sscanf(textf[6], "%lf", &bl->RTSource.Quelle.HardEdgeSource.divz);   
      sscanf(textf[7],  "%d", &bl->RTSource.Quelle.HardEdgeSource.idz);   
      bl->RTSource.raynumber= bl->RTSource.Quelle.HardEdgeSource.iy* 
	bl->RTSource.Quelle.HardEdgeSource.idy*
	bl->RTSource.Quelle.HardEdgeSource.iz* 
	bl->RTSource.Quelle.HardEdgeSource.idz;
      MakeRTSource(&PHASESet, bl);    
      break;
    case 'D':
      sscanf(textf[0], "%lf", &bl->RTSource.Quelle.DipolSource.sigy);   
      sscanf(textf[1], "%lf", &bl->RTSource.Quelle.DipolSource.sigdy);   
      sscanf(textf[2], "%lf", &bl->RTSource.Quelle.DipolSource.sigz);   
      sscanf(textf[3], "%lf", &bl->RTSource.Quelle.DipolSource.dz);  
      sscanf(textf[4],  "%d", &bl->RTSource.raynumber);   
      MakeRTSource(&PHASESet, bl);   
      break;
    case 'U':
    case 'u':
      sscanf(textf[0], "%lf", &bl->RTSource.Quelle.UndulatorSource.length);   
      sscanf(textf[1], "%lf", &bl->RTSource.Quelle.UndulatorSource.lambda);  
/*    modification: 04 Mar 98 13:24:43 flechsig */
/*    bl->RTSource.Quelle.UndulatorSource.lambda*= 1e-6; */
      bl->RTSource.Quelle.UndulatorSource.lambda= bl->BLOptions.lambda;
      sscanf(textf[2],  "%d", &bl->RTSource.raynumber);   
      MakeRTSource(&PHASESet, bl);   /* */ 
      break;
    case 'L':
    case 'M':
     sscanf(textf[0], "%lf", &bl->RTSource.Quelle.UndulatorSource.length);   
      sscanf(textf[1], "%lf", &bl->RTSource.Quelle.UndulatorSource.lambda);  
      bl->RTSource.Quelle.UndulatorSource.lambda= bl->BLOptions.lambda;
      sscanf(textf[2], "%d", &bl->RTSource.raynumber);   
      sscanf(textf[3], "%lf", &bl->RTSource.Quelle.UndulatorSource.deltaz);
      MakeRTSource(&PHASESet, bl);   /* */ 
      break;
    case 'S':
      sscanf(textf[0], "%lf", &bl->RTSource.Quelle.SRSource.y);   
      sscanf(textf[2], "%lf", &bl->RTSource.Quelle.SRSource.z);   
      sscanf(textf[4], "%lf", &bl->RTSource.Quelle.SRSource.dy);   
      sscanf(textf[6], "%lf", &bl->RTSource.Quelle.SRSource.dz);   
      bl->RTSource.raynumber= 1;   
      /* Fg3ActDat= x;      /** hier auch rt **/
      MakeRTSource(&PHASESet, bl);
      RayTraceSingleRay(bl);
      /* putpickfile(&Fg3ActDat, bl, PHASESet.sourcepckname);   */
      FetchWidget(kESourceBox, "ESourceBox");  
      InitSourceBox(&Fg3ActDat, bl, kESSR2Button);       
      XtManageChild(widget_array[kESourceBox]);      
      break;
    case 'I':
      sscanf(textf[0], "%lf", &bl->RTSource.Quelle.PSImage.ymin);
      sscanf(textf[1], "%lf", &bl->RTSource.Quelle.PSImage.ymax); 
      sscanf(textf[2], "%lf", &bl->RTSource.Quelle.PSImage.zmin); 
      sscanf(textf[3], "%lf", &bl->RTSource.Quelle.PSImage.zmax); 
      sscanf(textf[4], "%d", &bl->RTSource.Quelle.PSImage.iy); 
      sscanf(textf[5], "%d", &bl->RTSource.Quelle.PSImage.iz);
      bl->RTSource.raynumber= bl->RTSource.Quelle.PSImage.iy* 
	bl->RTSource.Quelle.PSImage.iz;
      bl->BLOptions.SourcetoImage= 0;
      break;
    case 'P':
      sscanf(textf[0], "%lf", &x.sigmay);
      sscanf(textf[1], "%lf", &x.sigmayp);
      sscanf(textf[2], "%lf", &x.ymin); 
      sscanf(textf[3], "%lf", &x.ymax); 
      sscanf(textf[4], "%lf", &x.sigmaz); 
      sscanf(textf[5], "%lf", &x.sigmazp);
      sscanf(textf[6], "%lf", &x.zmin);
      sscanf(textf[7], "%lf", &x.zmax);
      break;
    } 
  /* Fg3ActDat= x; 
     putpickfile(&Fg3ActDat, &Beamline, PHASESet.sourcepckname);     */
  WriteBLFile(PHASESet.beamlinename, bl); 
  xprintf("Beamline- file updated\n");  
  for (i= 0; i< 8; i++) XtFree(textf[i]);  
}

void GetGeometry(struct PHASEset *ph, struct gdatset *gp) 
    
/* modification: 20 Feb 98 09:23:44 flechsig */
{
  char *text;
 
  text= XmTextGetString(widget_array[kEGT1]); 
  sscanf(text, "%lf", &gp->r); 
  text= XmTextGetString(widget_array[kEGT2]); 
  sscanf(text, "%lf", &gp->rp);  
  text= XmTextGetString(widget_array[kEGT3]); 
  sscanf(text, "%lf", &gp->theta0);
  /*  text= XmTextGetString(widget_array[kEGT4]); 
  sscanf(text, "%lf", &cff); 
  cff wird hier nicht ausgelesen */
 
 /* aus rudimentaeren Gruenden */
  gp->lambda= Beamline.BLOptions.lambda;
  gp->iflag= 
    (XmToggleButtonGetState(widget_array[kEGNITranslation]) == TRUE) ? 1 : 0; 
  XtFree(text);
  /*  printf("getGeo: GActDat.theta0: %f\n", GActDat.theta0);    */
} /* end GetGeometry */

int GetOElement(struct PHASEset *ph, struct mdatset *mp, struct gdatset *gp)
     /* Uwe 4.6.96 						*/
     /* das Unterprogramm  wurde auf Pointer umgeschrieben,
	keine globalen  Variablen werden mehr genutzt 	*/
          
/* last modification: 16 Jun 97 18:07:30 flechsig */
/* last modification: 17 Jun 97 08:24:27 flechsig */
/* last modification: 30 Sep 97 08:22:03 flechsig */
{
  char *text= NULL; 
  XmString label;   
  Widget w;
  int etype;
 
  text= XmTextGetString(widget_array[kEOET1]); sscanf(text, "%lf", &mp->r1);  
  text= XmTextGetString(widget_array[kEOET2]); sscanf(text, "%lf", &mp->rmi); 
  text= XmTextGetString(widget_array[kEOET3]); sscanf(text, "%lf", &mp->r2);  
  text= XmTextGetString(widget_array[kEOET4]); sscanf(text, "%lf", &mp->rho); 
  text= XmTextGetString(widget_array[kEOET5]); 
  sscanf(text, "%d", &gp->inout); 
  text= XmTextGetString(widget_array[kEOET6]); 
  sscanf(text, "%lf", &gp->xdens[0]);
  text= XmTextGetString(widget_array[kEOET7]); 
  sscanf(text, "%lf", &gp->xdens[1]);
  text= XmTextGetString(widget_array[kEOET8]); 
  sscanf(text, "%lf", &gp->xdens[2]);
  text= XmTextGetString(widget_array[kEOET9]); 
  sscanf(text, "%lf", &gp->xdens[3]);
  text= XmTextGetString(widget_array[kEOET10]);
  sscanf(text, "%lf", &gp->xdens[4]);
  text= XmTextGetString(widget_array[kEOET11]); 
  sscanf(text, "%lf", &mp->alpha);
  XtFree(text);

  printf("getoelement: vor history\n"); 
  get_something(widget_array[kEOEMenu], XmNmenuHistory, &w);/*kEOOptMenu*/
  /*printf("getoelement: vor history 1\n");*/
  get_something(w, XmNlabelString, &label);  
  /*printf("getoelement: vor history 2\n");*/
  if (!XmStringGetLtoR(label, XmFONTLIST_DEFAULT_TAG, &text)) 
    return; 
  XmStringFree(label);
  printf("text: %s\n", text);
  if (strcmp(text, "plane- ellipt. m.") == 0) etype= kEOEPElli; else 
    if (strcmp(text, "elliptical mirror") == 0) etype= kEOEElli; else 
     if (strcmp(text, "toroidal mirror") == 0) etype= kEOETM; else 
       if (strcmp(text, "toroidal grating") == 0) etype= kEOETG; else 
	 if (strcmp(text, "aperture/ slit (RT)") == 0) etype= kEOESlit; else 
	   etype= kEOEVLSG;
	
  XtFree(text);
  printf("GetOelement: Elementtype: %d\n", etype);
  return etype;
} /* end GetOelement */

void SetRadius(int wn)               /*berechnet Radien von Toroiden */
     /* Uwe 7.6.96 */
{
  char *text;     
  double r1, r2, alpha, r, alrad;

  wn= (wn== kEOEAB2) ? kEOET2: kEOET4;
  text= XmTextGetString(widget_array[kEOET1]); sscanf(text, "%lf", &r1);     
  text= XmTextGetString(widget_array[kEOET3]); sscanf(text, "%lf", &r2);
  text= XmTextGetString(widget_array[kEOET11]); sscanf(text, "%lf", &alpha);
                                               
  /*     printf("GActDat.theta0: %f\n", GActDat.theta0);  /*tmp*/
  if (alpha >= 90.0)
    printf("error: theta >= 90\n");
  else
    {
      alrad= alpha*  PI/180.0;   
      if (wn == kEOET2) r= (2.0* r1* r2)/ ((r1+ r2)* cos(alrad));  
      else r= 2.0* r1* r2* cos(alrad)/ (r1+ r2);          /*eigentlich rho*/
      sprintf(text,"%.3f", r);  
      set_something(widget_array[wn], XmNvalue, text); 
    }
  XtFree(text);
}      

void SetTheta(struct gdatset *gdat)          /* setzt theta aus cff */
/* modification: 20 Feb 98 10:03:58 flechsig */
/* modification: 06 Mar 98 15:35:10 flechsig */
{
  char *text;     
  double cff, alpha, beta, theta0;
  
  text= XmTextGetString(widget_array[kEGT4]); 
  sscanf(text, "%lf", &cff);
  if (cff != 1.0)
    {
      FixFocus(cff, gdat->lambda, gdat->xdens[0], gdat->inout, &alpha, &beta);
      theta0= (alpha- beta)* 90.0/ PI;
      if (gdat->azimut > 1) theta0= -fabs(theta0);
      sprintf(text, "%.4f", theta0);  
      set_something(widget_array[kEGT3], XmNvalue, text); 
    }
  XtFree(text);
} /* end SetTheta */


void FetchWidget(int wnr, char *boxname) 
{
  start_watch();
  if (widget_array[wnr] == NULL)
    if (MrmFetchWidget(s_MrmHierarchy, boxname, 
		       toplevel_widget, &widget_array[wnr], &dummy_class) != MrmSUCCESS) 
      s_error("can't fetch  widget");  
  stop_watch();
}

void  MultiplyMatrix() 
     /* wird nicht mehr gebraucht ??? */
{
  XmString label, *list;
  int      itemcount, ac= 0; 
  char *lab= NULL,          
    copstr[MaxPathLength+ 15],
    labfield[3][MaxPathLength];


  XtVaGetValues(widget_array[kCCGList],
		XmNitemCount, &itemcount, XmNitems, &list, NULL);       
  
  get_something(widget_array[kCCGResultButton], XmNlabelString, &label);  
  if (!XmStringGetLtoR(label, XmFONTLIST_DEFAULT_TAG, &lab)) 
    return;
  sprintf(labfield[0], "%s", lab); 
  XtFree(lab);
  printf("itemcount:%d\n", itemcount);   
  ac= 0; 
  if (itemcount < 2) xprintf("minimum: 2 Items! \n");    
  else 
    {
      if (!XmStringGetLtoR(list[ac], XmFONTLIST_DEFAULT_TAG, &lab)) 
	return;
      sprintf(labfield[1],"%s", lab);
      printf("itemnr: %d  :: %s\n", ac++, lab );          /*1. Matrix */ 
      XtFree(lab);
      if (!XmStringGetLtoR(list[ac], XmFONTLIST_DEFAULT_TAG, &lab))
	return;
      sprintf(labfield[2],"%s", lab);
      printf("itemnr: %d  :: %s\n", ac++, lab );          /*2. Matrix */ 
      XtFree(lab);
      printf("mmatr: %s\n%s\n%s\n", labfield[0], labfield[1], labfield[2]);
      /*  MMatrix(labfield);  */
      while (ac < itemcount)    
        {        
	  sprintf(copstr, "copy %s tmp.tmp", labfield[0]); 
	  system(copstr);
	  if (!XmStringGetLtoR(list[ac], XmFONTLIST_DEFAULT_TAG, &lab))
	    return;
	  printf("itemnr: %d  :: %s\n", ac++, lab);   
	  sprintf(labfield[1], "tmp.tmp");   
	  sprintf(labfield[2],"%s", lab);   
	  XtFree(lab); 
	  /*  MMatrix(labfield); */ 
	  system("delete/noconfirm tmp.tmp;*"); 
	}       
    }
}                                 

void GeneratePrintDataFile(char *name)
     /* last mod. Uwe 16.8.96 */
{
  FILE *pdf;
  int i;

  if ((pdf= fopen(name, "w+")) == NULL)
    {
      fprintf(stderr,"error: open %s\n", name); exit(-1);
    }

  for (i= 0; i< 80; i++) fprintf(pdf,"*");  
  fputs("\nInput Data \n", pdf);
  for (i= 0; i< 80; i++) fprintf(pdf,"*");  

  fprintf(pdf, "\n\nMain Data File (Version higher 0.902: %s\n\n", 
	  PHASESet.beamlinename);
  gpd(PHASESet.beamlinename, pdf);    
  for (i= 0; i< 80; i++) fprintf(pdf,"*");    

  fprintf(pdf, "\n\nSource File: %s\n\n", PHASESet.sourcepckname);
  gpd(PHASESet.sourcepckname, pdf);    
  for (i= 0; i< 80; i++) fprintf(pdf,"*");    
  fprintf(pdf, "\n\nGeometry File: %s\n\n", PHASESet.geometrypckname);
  gpd(PHASESet.geometrypckname, pdf); 
  for (i= 0; i< 80; i++) fprintf(pdf,"*");  
  fprintf(pdf, "\n\n\n\nopt. Element File: %s\n\n", PHASESet.elementpckname);
  gpd(PHASESet.elementpckname, pdf);   
  for (i= 0; i< 80; i++) fprintf(pdf,"*");  
  /*   fprintf(pdf, "\nParameter File: %s\n\n", PHASESet.pssourcename);
       gpd(PHASESet.pssourcename, pdf); */   
  fclose(pdf);
}

void gpd(char *ifname, FILE *af)  
{
  char puffer[80];
  FILE *ifile;

  if ((ifile= fopen(ifname, "r")) == NULL)
    {
      fprintf(stderr,"error: open %s\n", ifname); exit(-1);
    }    
  while (!feof(ifile))
    {
      fgets(puffer, 80, ifile); fputs(puffer, af);
    }
  fclose(ifile);
}

void PrintFileInMainList(char *fname)    
{
  FILE *f;
  char zeile[80];

  if ((f= fopen(fname, "r")) == NULL)
    {
      sprintf(zeile,"error: open %s\n", fname);
      xprintf(zeile); 
    } else
      {
	while (!feof(f))
	  { 
	    fgets(zeile, 79, f); xprintf(zeile);
	  }
	fclose(f);
      }
}

/************************************************************************/
void writemapc(char *fname, int iord, 
               double *ypc1, double *zpc1, double *dypc, double *dzpc,
	       double *wc, double *xlc, double *xlen1c, double *xlen2c)   
     /************************************************************************/
     /* schreibt die Transformationskoeffizienten auf ein file       	*/
     /* Uwe 28.6.96 							*/ 
     /* letzte Aenderung: Uwe 28.6.96 					*/ 
     /************************************************************************/
{
  FILE *f;
  int i, j, k, l, index;
  
  if ((f= fopen(fname, "w+")) == NULL)
    {
      fprintf(stderr, "error: open file %s\n", fname); exit(-1);   
    }    
  fprintf(stdout,"write transformation map to: %s\n", fname);
  fprintf(f,
	  "i j k l          ypc             zpc            dypc            dzpc\n");
  for (i= 0; i <= iord; i++)
    for (j= 0; j <= iord; j++)
      for (k= 0; k <= iord; k++)
	for (l= 0; l <= iord; l++)
	  if((i+j+k+l) <= iord)
	    {
	      index=i+ j*5+ k*25 + l*125;
	      /*     printf("%d\n", index); */
	      fprintf(f, "%d %d %d %d % 15.5E % 15.5E % 15.5E % 15.5E\n", 
                      i, j, k, l, ypc1[index], zpc1[index],
		      dypc[index], dzpc[index]);
	    }
  fprintf(f, "\n\n\n");
  fprintf(f,
	  "i j k l          wc              xlc            xlen1c          xlen2c\n");
  for (i= 0; i <= iord; i++)
    for (j= 0; j <= iord; j++)
      for (k= 0; k <= iord; k++)
	for (l= 0; l <= iord; l++)
	  if((i+j+k+l) <= iord)
	    {
	      index=i+ j*5+ k*25 + l*125;
	      fprintf(f, "%d %d %d %d % 15.5E % 15.5E % 15.5E % 15.5E\n", 
                      i, j, k, l, wc[index], xlc[index],
		      xlen1c[index], xlen2c[index]);
	    }
  fclose(f);
}	
/* ******** end writemapc() ************************************** */


void WriteMKos(struct mirrortype *a, char *name)
     /* schreibt mirrorkoordinaten auf file */
{
  FILE *f;
  int i, j;
  double *dp; 

  printf("WriteMKos called: write to %s\n", name);
  dp= (double *)a;

  if ((f= fopen(name, "w+")) == NULL)
    {
      fprintf(stderr, "error: open file %s\n", name); exit(-1);   
    }    
  for (i= 0; i <= 5; i++) 
    for (j= 0; j <= 5; j++) 
      if ((i + j) <= 5) fprintf(f, "%lE\n", dp[i+j*6]);
  fclose(f); 
}    

void ReadMKos(struct mirrortype *a, char *name)
     /* liest mirrorkoordinaten von file */
{
  FILE *f;
  int i, j;
  double *dp; 
  printf("read mkos from %s ? <1>", name);
  scanf("%d", &i);
  if (i == 1)
    {
      printf("READMKos called: read from %s\n", name); 
      dp= (double *)a;
      if ((f= fopen(name, "r+")) == NULL)
	{
	  fprintf(stderr, "error: open file %s\n", name); exit(-1);   
	}   
      for (i= 0; i <= 5; i++) 
	for (j= 0; j <= 5; j++) 
	  if ((i + j) <= 5) fscanf(f, "%lf\n", &dp[i+j*6]);
      fclose(f); 
    }
}    

/* Alloc allociert/ reallociert einen pointer */
/* bei fehler exit -1 */
void *Alloc(void *p, int size)
{
  /*  if (p != NULL) free(p); */
  if ((p= malloc(size)) == NULL)
    {  fprintf(stderr, "Alloc error- exit(-1)\n"); exit(-1);    } 
  return p;
} /* end Alloc */

/* end of file phasec.c */                                
