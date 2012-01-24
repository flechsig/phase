//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/qtphase.cpp
//  Date      : <08 Jun 11 16:14:16 flechsig> 
//  Time-stamp: <24 Jan 12 09:35:44 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 


#include "phaseqt.h"

// initialize the c structure of filenames with a name
// removes a possible extension
void PhaseQt::initSet(const char *fname)
{
  char name[MaxPathLength];
  strncpy(name, fname, MaxPathLength);
  FnameBody(name);

  snprintf(matrixname,      MaxPathLength, "%s.%s", name, "omx");
  snprintf(mapname,         MaxPathLength, "%s.%s", name, "map");
  snprintf(sourceraysname,  MaxPathLength, "%s.%s", name, "inp"); 
  snprintf(imageraysname,   MaxPathLength, "%s.%s", name, "out");	  
  snprintf(intersecname,    MaxPathLength, "%s.%s", name, "isec");	  
  snprintf(geometryname,    MaxPathLength, "%s.%s", name, "datg"); 
  snprintf(elementname,     MaxPathLength, "%s.%s", name, "date");	  
  snprintf(sourcepckname,   MaxPathLength, "%s.%s", name, "pcks"); 
  snprintf(geometrypckname, MaxPathLength, "%s.%s", name, "pckg"); 
  snprintf(elementpckname,  MaxPathLength, "%s.%s", name, "pcke"); 
  snprintf(pssourcename,    MaxPathLength, "%s.%s", name, "pss"); 
  snprintf(plotpsname,      MaxPathLength, "%s.%s", name, "ps");	  
  snprintf(printpclname,    MaxPathLength, "%s.%s", name, "pcl"); 
  snprintf(optipckname,     MaxPathLength, "%s.%s", name, "pcko"); 
  snprintf(beamlinename,    MaxPathLength, "%s.%s", name, "phase");	  
  snprintf(so4_fsource4a,   MaxPathLength, "%s.%s", name, "s4a");	  
  snprintf(so4_fsource4b,   MaxPathLength, "%s.%s", name, "s4b");	  
  snprintf(so4_fsource4c,   MaxPathLength, "%s.%s", name, "s4c");	  
  snprintf(so4_fsource4d,   MaxPathLength, "%s.%s", name, "s4d");	  
  snprintf(so6_fsource6,    MaxPathLength, "%s.%s", name, "s6");
  snprintf(opresname,       MaxPathLength, "%s.%s", name, "opti");	  
  snprintf(minname,         MaxPathLength, "%s.%s", name, "minu");
}

// print the contents of the data for debugging
void PhaseQt::printSet()
{
  printf("=> PhaseQt::printSet()\n");
  printf("matrixname:      %s\n", matrixname);
  printf("mapname:         %s\n", mapname);
  printf("sourceraysname:  %s\n", sourceraysname ); 
  printf("imageraysname:   %s\n", imageraysname);	  
  printf("intersecname:    %s\n", intersecname);	  
  printf("geometryname:    %s\n", geometryname);    
  printf("elementname:     %s\n", elementname);  	  
  printf("sourcepckname:   %s\n", sourcepckname);   
  printf("geometrypckname: %s\n", geometrypckname); 
  printf("elementpckname:  %s\n", elementpckname);  
  printf("pssourcename:    %s\n", pssourcename);    
  printf("plotpsname:      %s\n", plotpsname); 	  
  printf("printpclname:    %s\n", printpclname);    
  printf("optipckname:     %s\n", optipckname);     
  printf("beamlinename:    %s\n", beamlinename);	  
  printf("so4_fsource4a:   %s\n", so4_fsource4a);	  
  printf("so4_fsource4b:   %s\n", so4_fsource4b);	  
  printf("so4_fsource4c:   %s\n", so4_fsource4c);	  
  printf("so4_fsource4d:   %s\n", so4_fsource4d);	  
  printf("so6_fsource6:    %s\n", so6_fsource6);    
  printf("opresname:       %s\n", opresname);
  printf("minname:         %s\n", minname);
  printf("<= myPHASEset::print()\n");
}

void PhaseQt::initBeamline()
{
  beamlineOK         = 0;
  elementzahl        = 0;
  hormapsloaded      = 0;
  ElementList        = NULL;
  raysout            = NULL;
  RTSource.SourceRays= NULL;
  RTSource.Quellep   = NULL;
  RESULT.points      = 0;
  RESULT.RESp        = NULL;
  localalloc         = DOALLOC;
  tp                 = NULL;
} // end initBeamline


// returns a pointer to the beamline struct
struct BeamlineType *PhaseQt::myBeamline()
{
  return (struct BeamlineType *)this;
} // end myBeamline

// returns a pointer to the PHASEset struct
struct PHASEset *PhaseQt::myPHASEset()
{
  return (struct PHASEset *)this;
} // end myPHASEset

// returns a pointer to the beamline struct
struct OptionsType *PhaseQt::myOptions()
{
  return &(this->BLOptions);
} // end myBeamline


// constructor
//QtPhase::QtPhase()
//{
  //char *matrixname= new char[MaxPathLength];
//}

PhaseQt::PhaseQt()
{
#ifdef DEBUG  
  printf("debug: PhaseQt constructor called\n");
#endif
  initSet("default");   // initialize the filename structure
  initBeamline();       // initialize the beamline structure
} // end constructor

#ifdef xxx  
  Beamline.localalloc= DOALLOC;       /* init should go somwhere else */

#ifdef EXPIRE
       printf(" defined expire date: %d\n", EXPIRE);
       time(&timev);
       local_time= localtime(&timev);
       /* debug       printf("%d %d %d\n\n", local_time->tm_year, local_time->tm_mon, local_time->tm_mday); */

       if (  local_time->tm_mday + 
	    (local_time->tm_mon  + 1) * 100 + 
	    (local_time->tm_year + 1900) * 10000 > EXPIRE )
	 {
	   printf("\n Program PHASE expired..., terminating\n Please, contact Johannes Bahrdt\n\n");
	   exit(1);
	 } else printf("  The program is not expired!\n");

       /*       already_expired(); */
#else
    printf(" The program does not expire!\n\n");
#endif

#ifdef SEVEN_ORDER
    printf(" PHASE version > Nov: 2010: SEVEN_ORDER defined\n\n");
#else
    printf(" PHASE version > Nov: 2010: SEVEN_ORDER undefined\n\n");
#endif
/* Mar 10: get the data directory at runtime from ENV variable again, not at compile time*/
    if ((global_rundir = getenv(PHASE_HOME)) == NULL)
    {
      printf("\nphase.c: needed environment variable %s not defined -- exiting\n", PHASE_HOME);
      exit(-1);
    } 
    else printf("Runtime directory is %s\n", global_rundir);
    
    
    
    //  InitDataSets(&PHASESet, (char*) MainPickName);             /* PHASEc.c */

}

#endif


// rewrite of initdatsets
void PhaseQt::sourceSetDefaults()
{
  char sou;
  struct UndulatorSourceType  *up;
  struct UndulatorSource0Type *up0;
  struct DipolSourceType      *dp;
  struct PointSourceType      *pp;
  struct RingSourceType       *rp;
  struct HardEdgeSourceType   *hp;    
  struct FileSourceType       *fp;
  struct SRSourceType         *sp;
  struct PSImageType *ip;

  this->RTSource.raynumber= 25000;
  sou= this->RTSource.QuellTyp;

  printf(" set defaults for source: %c\n", sou);
  switch(sou)
    {
    case 'U': 
    case 'u':
      up= (struct UndulatorSourceType *)this->RTSource.Quellep; 
      up->length= 3800.0;
      up->lambda= 12.4e-6;  
      break;   
    case 'L': 
    case 'M':
      up= (struct UndulatorSourceType *)this->RTSource.Quellep; 
      up->length= 3800.0;
      up->lambda= 12.4e-6;
      up->deltaz= 0.0;
      break;
    case 'D': dp=(struct DipolSourceType *)this->RTSource.Quellep; 
      dp->sigy		= 0.093;  
      dp->sigdy	        = 1.;  
      dp->sigz        	= 0.05;
      dp->dz          	= 4.0;
      break;  
    case 'o': pp=(struct PointSourceType *)this->RTSource.Quellep; 
      pp->sigy	= 0.093;  
      pp->sigdy	= 1.;  
      pp->sigz  = 0.05;
      pp->sigdz = 1.0;
      break;  
    case 'S': sp= (struct SRSourceType *)this->RTSource.Quellep;
      sp->y	=0.1;  
      sp->dy	=0.1;  
      sp->z	=0.1;  
      sp->dz	=0.1; 
      break;   
    case 'I': ip= (struct PSImageType*)this->RTSource.Quellep; 
      ip->ymin	= -1.0e-1;  
      ip->ymax	=  1.0e-1;  
      ip->zmin	= -1.0e-1;  
      ip->zmax	=  1.0e-1;
      ip->iy   =   15;
      ip->iz   =   15;
      break;   
    case 'H': 
      hp= (struct HardEdgeSourceType *)this->RTSource.Quellep; 
      hp->disty	= .1;  
      hp->iy 	= 3;   
      hp->distz	= .2;  
      hp->iz	= 3;   
      hp->divy	= 1.;  
      hp->idy	= 7;   
      hp->divz	= 4.;  
      hp->idz	= 7;   
      this->RTSource.raynumber= hp->iy * hp->iz * hp->idy * hp->idz;
      break;   
    case 'G': 
      up0= (struct UndulatorSource0Type *)this->RTSource.Quellep; 
      up0->sigmaez= 0.05;
      up0->sigmaey= 0.01;
      up0->sigmaedz= 0.05;
      up0->sigmaedy= 0.01;
      break;
    case 'R': 
      rp= (struct RingSourceType *)this->RTSource.Quellep;
      rp->dy= 0.1;
      rp->dz= 0.1;
      break;
    case 'F':
      fp= (struct FileSourceType *)this->RTSource.Quellep;
      strncpy(fp->filename, this->sourceraysname, MaxPathLength);
      /* we may add a test if the file exists */
      break;   
    default:
      printf("sourceSetDefaults-> error: source %c not found\n", sou);
    }  /* end case */
} // sourceSetDefaults

// write a backupfile
void PhaseQt::writeBackupFile()
{
  char buffer[MaxPathLength];
  strncpy(buffer, this->beamlinename, (MaxPathLength-1));
  strcat(buffer, "~");
  
#ifdef DEBUG
  printf("writeBackupFile: -> ");
#endif
  WriteBLFile(buffer, this);
} // writeBackupFile()

// end 
