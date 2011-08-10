/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/phasec.c */
/*   Date      : <24 Jun 02 09:51:36 flechsig>  */
/*   Time-stamp: <10 Aug 11 14:25:58 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */
 
/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif

#include <stdio.h>                    /* For printf and so on. */
#include <stdlib.h> 	      	      /* needed for fopen      */  
#include <string.h>                           
#include <math.h>                                                 
#include <ctype.h>
#include <stdarg.h> 
#include <unistd.h>

#ifndef QTGUI
#include <Xm/Text.h>                                                  
#include <Xm/FileSB.h>                /*FileBox*/     
#include <Xm/List.h>   
#include <Xm/ToggleB.h>   
#include <Mrm/MrmAppl.h>  
#include <X11/Xlib.h>      
#include <X11/Xutil.h>  
#endif
#include "cutils.h"   
#include "phase_struct.h"
#include "fg3pck.h"   
#include "mirrorpck.h"                 
#include "geometrypck.h"   
#include "phase.h"
#ifndef QTGUI
#include "phaseX.h"
#endif
#include "rtrace.h"
#include "version.h"


void BatchMode(struct PHASEset *ps, struct BeamlineType *bl,  int cmode, int selected)
/* Uwe 2.10.96 */
/* Batchmodus */
{
#ifndef QTGUI
  struct PSDType     *PSDp;
#endif
  struct PSImageType *psip;

  printf("BatchMode: datafilename  : %s\n", ps->beamlinename);
  printf("BatchMode: resultfilename: %s\n", ps->imageraysname);
  /*  InitDataSets(&PHASESet, fname);  initialisiert auch Beamline */
  /* habe die Initialisierung hier extra */
#ifdef LOGFILE 
  CheckUser(logfilename, "Phase Batch");            /* user logfile  */
#endif
  bl->ElementList= NULL;                       /* 15.12.99 */
  bl->raysout= NULL;
  bl->RTSource.SourceRays= NULL;
  bl->beamlineOK= 0;
  ReadBLFile(ps->beamlinename, bl);
#ifndef QTGUI
  strcpy(ps->pssourcename, bl->src.so6.fsource6);
#endif
  BuildBeamline(bl); 
  if (cmode == -1) cmode= bl->BLOptions.CalcMod;
  switch (cmode)
    {
    case 1:
      printf("BatchMode: Ray Tracing\n");
      MakeRTSource(ps, bl); 
      ReAllocResult(bl, PLrttype, bl->RTSource.raynumber, 0);
      RayTracec(bl);
      WriteRayFile(ps->imageraysname, &bl->RESULT.points,
		   bl->RESULT.RESp);
      break;
    case 2:
      printf("BatchMode: Full Ray Tracing\n");
      MakeRTSource(ps, bl); 
      ReAllocResult(bl, PLrttype, bl->RTSource.raynumber, 0);
      RayTraceFull(bl);
      WriteRayFile(ps->imageraysname, &bl->RESULT.points,
		   bl->RESULT.RESp); 
      break;
    
    case 3: 
      printf("BatchMode: Phase Space Transformation\n");
#ifndef QTGUI
      src_ini(&bl->src); 
      psip = (struct PSImageType *)bl->RTSource.Quellep;
      ReAllocResult(bl, PLphspacetype, psip->iy, psip->iz);
      PST(bl);
      PSDp= (struct PSDType *)bl->RESULT.RESp;
      WritePsd(ps->imageraysname, PSDp, PSDp->iy, PSDp->iz);
#endif
      break;

    case 4:
      printf("BatchMode: Footprint at element %d\n", selected);
      bl->position= selected;
      MakeRTSource(ps, bl);
      ReAllocResult(bl, PLrttype, bl->RTSource.raynumber, 0);
      Footprint(bl, bl->position);
      WriteRayFile(ps->imageraysname, &bl->RESULT.points,
		   bl->RESULT.RESp);
      break;

    case 5:
      printf("BatchMode: multiple Phase Space Imaging\n");
#ifndef QTGUI
      src_ini(&bl->src); 
#endif
      psip = (struct PSImageType *)bl->RTSource.Quellep;
      ReAllocResult(bl, PLphspacetype, psip->iy, psip->iz);
#ifndef QTGUI
      MPST(bl);

      PSDp= (struct PSDType *)bl->RESULT.RESp;
      WritePsd(ps->imageraysname, PSDp, PSDp->iy, PSDp->iz);
#endif
      break;

    default: 
      printf("BatchMode: unknown CalcMod: %d\n", cmode);
    }
  beep(5);
  printf("BatchMode: program end\n");
} /* end Batchmode */


int ProcComandLine(struct PHASEset *ps, struct BeamlineType *bl, unsigned int argc, char *argv[], int *cmode, int *selected)
/* Uwe new version 10.8.2011 using getopt */
{
  int bflag = 0;
  char *fvalue = NULL;
  char *mvalue = NULL;
  char *ovalue = NULL;
  char *svalue = NULL;
  int index;
  int c;

  int ret= 1;
  *cmode= -1;
  *selected= -1;

  
  opterr = 0;
  
  while ((c = getopt (argc, argv, "BbF:f:HhM:m:NnO:o:S:s:V")) != -1)
    switch (c)
      {
      case 'B':
      case 'b':
	bflag = 1;
	ret= -8;
	printf("option -%c\n", c);
	break;
      case 'F':
      case 'f':
	printf("option -%c\n", c);
	fvalue = optarg;
	strncpy(ps->beamlinename, fvalue, MaxPathLength);
	printf("ProcComandLine: use input_filename from parameter: >>%s<<\n", fvalue);
	break;
      case 'H':
      case 'h':
	printf("option -%c\n", c);
	printf("usage: phase [options] [input_filename] [output_filename]\n");
	printf("       options: -b, -B:           batch mode (no X11)\n");
	printf("                -f, -Ffilename:   use datafile (*.phase)\n");
	printf("                -h, -H:           show this message\n");
	printf("                -m, -Mcalcmode:   calculation mode \n");
	printf("                                  1: ray trace\n");
	printf("                                  2: full ray trace\n");
	printf("                                  3: phase space imaging\n");
	printf("                                  4: footprint (requires option s)\n");
	printf("                                  5: multiple phase space imaging\n");
	printf("                -n, -N:           no startup- info (X11)\n");
	printf("                -o, -Oresultfile: filename\n");
	printf("                -s, -Snumber:     selected element number (for footprint)\n");
	printf("                -V:               Version\n");
	exit(0);
	break;
      case 'M':
      case 'm':
	mvalue = optarg;
	sscanf(mvalue, "%d", cmode);
	printf("ProcComandLine: calculation mode: %d\n", *cmode);
	break;
      case 'N':
      case 'n':
	ret= 0;
	printf("option -n is obsolete\n");
	break;
      case 'O':
      case 'o':
	printf("option -%c\n", c);
	ovalue = optarg;
	strncpy(ps->beamlinename, ovalue, MaxPathLength);
	printf("ProcComandLine: use output_filename from parameter: >>%s<<\n", ovalue);
	break;
      case 'S':
      case 's':
	svalue = optarg;
	sscanf(svalue, "%d", selected);
	printf("ProcComandLine: selected element: %d\n", *selected);
	break;
      case 'V':
	printf("Version: %s\n", VERSION);
	break;	
      case '?':
	switch (optopt)
	  {
	  case 'F':
	  case 'f':
	  case 'M':
	  case 'm':
	  case 'O':
	  case 'o':
	  case 'S':
	  case 's':
	    fprintf (stderr, "Option -%c requires an argument.\n", optopt);
	    break;
	  default:
	    if (isprint (optopt))
	      fprintf (stderr, "Unknown option `-%c'.\n", optopt);
	    else
	      fprintf (stderr,
		       "Unknown option character `\\x%x'.\n", optopt);
	    exit (-1);
	  }
      default:
	abort ();
      }
  
  /*printf ("aflag = %d, bflag = %d, cvalue = %s\n",
    aflag, bflag, cvalue);*/

  /* no option arguments */   
  index = optind;   
  if (index < argc)  /* wir nehmen das erste argument als filename und ueberschreiben damit den parameter -f */
    {
      strncpy(ps->beamlinename, argv[index], MaxPathLength);
      printf("ProcComandLine: use input_filename from argument: >>%s<<\n", argv[index]);
    }
  index++;
  if (index < argc)  /* wir nehmen das zweite argument als output_filename und ueberschreiben damit den parameter -o */
    {
      strncpy(ps->imageraysname, argv[index], MaxPathLength);
      printf("ProcComandLine: use output_filename from argument: >>%s<<\n", argv[index]);
    }

  return ret;
} /* end ProcComandLine */


#ifdef OBSOLETE
int ProcComandLine(struct PHASEset *ps, struct BeamlineType *bl, unsigned int ac, char *av[])
/* Uwe 2.10.96 */
/* Wertet Kommandozeile aus */
/* UF 10.6.11 remove global var */
{
  int  ret, i, cmode, selected;
  char *ch, *dfname, *resultname, *pfname; 
  
  i= ret= 1;
  cmode= -1;
  selected= -1;
  /* defaults */
  pfname=     (char *) MainPickName; 
  /*  resultname= (char *) PHASESet.imageraysname;
      dfname=     (char *) PHASESet.beamlinename; */
  resultname= (char *) ps->imageraysname;
  dfname=     (char *) ps->beamlinename; 
  while (i < (int)ac)
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
	case 'G':             /* rechenmodus */
	case 'm':
	  ch++; 
	  sscanf(ch, "%d", &cmode);
	  printf("ProcComandLine: calculation mode: %d\n", cmode);
	  break;
	case 'S':             /* selected element */
	case 's':
	  ch++; 
	  sscanf(ch, "%d", &selected);
	  printf("ProcComandLine: selected element: %d\n", selected);
	  break;
	case 'F':
	case 'f':
	  ch++; dfname= ch;
	  /*	  strcpy(PHASESet.beamlinename, ch);*/
	  strcpy(ps->beamlinename, ch);
	  printf("ProcComandLine: filename: >>%s<<\n", ch);
	  break;
	case 'O':
	case 'o':
	  ch++; resultname= ch;
	  strcpy(ps->imageraysname, ch);
	  printf("ProcComandLine: result- filename: >>%s<<\n", ch);
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
	  printf("                                  3: phase space imaging\n");
	  printf("                                  4: footprint (requires option s)\n");
	  printf("                                  5: multiple phase space imaging\n");
	  printf("                -s, -Snumber:     selected element number (for footprint)\n");
	  exit(3);
	  break;
	default: 
	  printf("unknown option %c \n", *ch);
	}
      i++;
    }

  if (ret == -8)
    {
      BatchMode(ps, bl, cmode, selected);
      exit(3);
    }
  return ret;
} /* end ProcComandLine */
#endif

int GetPHASE(struct PHASEset *x, char *mainpickname)
     /* return 1: file gelesen- OK		*/
     /* 30.5.96 				*/
     /* last mod. 26.7.96			*/
{
  FILE *f;
  int  rcode, version;
   
  rcode= -1;
  if ((f= fopen(mainpickname, "r")) == NULL)
    fprintf(stderr, "File %s not found- defaults used!\n", mainpickname); 
  else 
    {
      if(CheckFileHeader(f, MainPickFileHeader, &version) == 0)   
	{      
	  fscanf(f,"%s\n", (char *) &x->matrixname);     
	  fscanf(f,"%s\n", (char *) &x->mapname);    
	  fscanf(f,"%s\n", (char *) &x->sourceraysname);       
	  fscanf(f,"%s\n", (char *) &x->imageraysname);	   
	  fscanf(f,"%s\n", (char *) &x->intersecname); 
	  fscanf(f,"%s\n", (char *) &x->geometryname);   
	  fscanf(f,"%s\n", (char *) &x->elementname);  
	  fscanf(f,"%s\n", (char *) &x->sourcepckname);  
	  fscanf(f,"%s\n", (char *) &x->geometrypckname);   
	  fscanf(f,"%s\n", (char *) &x->elementpckname);  
	  fscanf(f,"%s\n", (char *) &x->pssourcename);   
	  fscanf(f,"%s\n", (char *) &x->plotpsname);  
	  fscanf(f,"%s\n", (char *) &x->printpclname); 
	  fscanf(f,"%s\n", (char *) &x->optipckname); 
	  if (!feof(f))    /* neu */
	    fscanf(f,"%s\n", (char *) &x->beamlinename); 
	  else strcpy(x->beamlinename, "SGM.PHASE"); 
	  if (version >= 20081119)
	    {
	      fscanf(f,"%s\n", (char *) &x->so4_fsource4a);     
	      fscanf(f,"%s\n", (char *) &x->so4_fsource4b);    
	      fscanf(f,"%s\n", (char *) &x->so4_fsource4c);       
	      fscanf(f,"%s\n", (char *) &x->so4_fsource4d);
	      fscanf(f,"%s\n", (char *) &x->so6_fsource6);
	    }
	  rcode= 1;       /* OK zurueck */
	}
      fclose(f);   
    }	
  return rcode;
}     /* end  GetPHASE */

/* berechnet den index aus den selektierten Positionen */
/* el: selektiertes element, pos: position             */
/* pos und el starten mit 1                            */
int iindex(int el, int pos)             
{
  int iret, k, mtp, p;
  int start_mtype= 21;                          /* position of first mtype */

  iret = el * pos;        			/* test ob 0 */
  mtp  = (pos >= start_mtype) ? (1 << 7) : 0;   /* position fuer mdaten 128 */
  if (mtp > 0) 
    p= (pos == start_mtype) ? 1 : (81 + pos- start_mtype);
  /*   p= (pos == start_mtype) ? 1 : (36 + pos- start_mtype); bis jul 2011 */
  else 
    p= pos;    
                               
  k    = ((el- 1) << 8) | mtp | (p- 1);
  iret = (iret == 0) ? -1 : k;
  printf("iindex: return: %d, element: %d, selected item: %d\n", 
	 iret, el, pos);
  return iret;
} /* end iindex */


void InitDataSets(struct PHASEset *x, struct BeamlineType *bl, char *mainpickname)   
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
  bl->ElementList= NULL;                       /* 15.12.99 */
  bl->raysout    = NULL;
  bl->RTSource.SourceRays= NULL;
  bl->beamlineOK= 0;
#ifndef QTGUI
  ginitdatset(&GDefDat);           /* init defaults */
  minitdatset(&MDefDat);
#endif

  ReadBLFile(x->beamlinename, bl);
#ifndef QTGUI
  strcpy(x->pssourcename, bl->src.so6.fsource6);  
#endif
#ifndef QTGUI
  grdatstruct.status= 0;
  SetGrDatStruct(x->imageraysname, bl, &grdatstruct);  
#endif
  /* PHASEgraf.c */
  /*   optistructure.fileliste= NULL;     */
#ifdef DEBUG
  printf("InitDataSets end\n");   
#endif  
}


void InitOptiBox1(char *pickname)   
{
  printf("dummy %s\n", pickname);
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
   


void PutPHASE(struct PHASEset *x, char *mainpickname)  /* write mainpickfile */
    
{                              
  FILE *f;
  int version= 20081119;
  printf("putphase: write filenames\n");

  if ((f= fopen(mainpickname, "w")) == NULL)
    {
      fprintf(stderr,"\afatal Error: write %s\n", mainpickname);
      exit(-1);
    } else 
      {
	fprintf(f,"%s %d\n", MainPickFileHeader, version);
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
	fprintf(f,"%s\n", x->so4_fsource4a);
	fprintf(f,"%s\n", x->so4_fsource4b);
	fprintf(f,"%s\n", x->so4_fsource4c);
	fprintf(f,"%s\n", x->so4_fsource4d);
	fprintf(f,"%s\n", x->so6_fsource6);
       	fclose(f);  
      }
}    /* end putphase */	







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









#ifndef QTGUI
void GeneratePrintDataFile(char *name)
     /* last mod. Uwe 16.8.96 */
{
  FILE *pdf;
  int i;

  if ((pdf= fopen(name, "w+")) == NULL)
    {
      fprintf(stderr, "GeneratePrintDataFile: error: open file %s\n", name); 
      exit(-1);
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
#endif

/*80 in Maxpathlengt 16.11.01 UF*/
void gpd(char *ifname, FILE *af)  
{
  char puffer[MaxPathLength];
  FILE *ifile;

  if ((ifile= fopen(ifname, "r")) == NULL)
    {
      fprintf(stderr,"gpd: error: open file %s\n", ifname); 
      /* 11.1.2000       exit(-1); */
    }  
  else
    {  
      while (!feof(ifile))
	{
	  fgets(puffer, MaxPathLength, ifile); fputs(puffer, af);
	}
      fclose(ifile);
    }
}


/************************************************************************/
void writemapc(char *fname, char *header, int iord, 
               double *ypc1, double *zpc1, double *dypc,   double *dzpc,
	       double *wc,   double *xlc,  double *xlen1c, double *xlen2c)   
/************************************************************************/
/* schreibt die Transformationskoeffizienten auf ein file       	*/
/* Anpassung an SEVEN_ORDER 8.7.2011 UF                                 */     
/************************************************************************/
{
  FILE *f;
  int i, j, k, l, index;
  
  if ((f= fopen(fname, "w+")) == NULL)
    {
      fprintf(stderr, "writemapc: error: open file %s\n", fname); exit(-1);   
    }    
  fprintf(stdout, "writemapc: write transformation map to: %s\n", fname);
  fprintf(f, "#\n# %s\n#\n", header);
  fprintf(f,
	  "i j k l          ypc             zpc            dypc            dzpc\n");
  for (i= 0; i <= iord; i++)
    for (j= 0; j <= iord; j++)
      for (k= 0; k <= iord; k++)
	for (l= 0; l <= iord; l++)
	  if((i+j+k+l) <= iord)
	    {
#ifdef SEVEN_ORDER
	      index=i+ j*8+ k*64 + l*512;
#else
	      index=i+ j*5+ k*25 + l*125;
#endif
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
#ifdef SEVEN_ORDER
	      index=i+ j*8+ k*64 + l*512;
#else
	      index=i+ j*5+ k*25 + l*125;
#endif
	    
	      fprintf(f, "%d %d %d %d % 15.5E % 15.5E % 15.5E % 15.5E\n", 
                      i, j, k, l, wc[index], xlc[index],
		      xlen1c[index], xlen2c[index]);
	    }
  fclose(f);
}	
/* ******** end writemapc() ************************************** */




/* setzt den sourcetype in der struktur abhanegig von einem Schalter */ 
/* UF 28.11.06 */
void InitSourceType(struct BeamlineType *bl, int widget_num)
{
  int sou;

  switch (widget_num) 
    {
    case kESDipolSourceButton: 	   sou= 'D'; break;
    case kESPointSourceButton: 	   sou= 'o'; break;
    case kESRingSourceButton: 	   sou= 'R'; break;
    case kESUndulatorSourceButton: sou= 'U'; break;  
    case kESUndulatorSISButton:    sou= 'L'; break;  /*23.11.98 UF*/
    case kESUndulatorSIMButton:    sou= 'M'; break;  
    case kESundulatorSourceButton: sou= 'u'; break;
    case kESUndulatorButton:       sou= 'G'; break;
    case kESSR2Button: 		   sou= 'S'; break;  
    case kESPhaseSpaceButton:	   sou= 'P'; break;  
    case kESPhaseSpaceImageButton: sou= 'I'; break; 
    case kESFileButton:            sou= 'F'; break;
    case kESDefaults:
    case kESourceMenuButton:	   sou= bl->RTSource.QuellTyp; 
      break;
    case kESRayTraceButton:
    default : 			   sou= 'H'; 
    }
  if (sou == 0) sou= 'H'; 
  printf("InitSourceType: new: %c, old: %c, num: %d, int(old): %d\n", 
	 sou, bl->RTSource.QuellTyp, widget_num, bl->RTSource.QuellTyp);
  bl->RTSource.QuellTyp= sou;

  

} /* end InitSourceType */

int CheckBLOK(int blok, int target, char *message)
{
  int ret;
  ret= 1;
  if ((blok & target) != target)
    {
      fprintf(stderr, "%s beamline is not OK: 0x%X & 0x%X = 0x%X\n", 
	      message, blok, target, (blok & target)); 
      if (((target & sourceOK) > 0) && ((blok & sourceOK) == 0)) 
	fprintf(stderr, "  %s\n", "RT source not defined!");
      if (((target & mapOK) > 0) && ((blok & mapOK) == 0)) 
	fprintf(stderr, "  %s\n", "matrix and map not defined!");
      if (((target & resultOK) > 0) && ((blok & resultOK) == 0)) 
	fprintf(stderr, "  %s\n", "result not defined!");
      if (((target & elementOK) > 0) && ((blok & elementOK) == 0)) 
	fprintf(stderr, "  %s\n", "optical element not defined!");
      if (((target & geometryOK) > 0) && ((blok & geometryOK) == 0))
	fprintf(stderr, "  %s\n", "geometry of optical element not defined!");
      if (((target & pstsourceOK) > 0) && ((blok & pstsourceOK) == 0)) 
	fprintf(stderr, "  %s\n", "PST source not defined!");
      if (((target & pstimageOK) > 0) && ((blok & pstimageOK) == 0))
	fprintf(stderr, "  %s\n", "PST image plane not defined (in Source menu)!");
      ret= 0;
    }
  return ret;
}


/* end of file phasec.c */     
                           
