//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/qtphase.cpp
//  Date      : <08 Jun 11 16:14:16 flechsig> 
//  Time-stamp: <07 Jul 11 16:59:39 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 


#include "qtphase.h"

// initialize the c structure of filenames with a name
void myPHASEset::init(char *name)
{
  sprintf(matrixname,      "%s.%s", name, "omx");
  sprintf(mapname,         "%s.%s", name, "map");
  sprintf(sourceraysname,  "%s.%s", name, "inp"); 
  sprintf(imageraysname,   "%s.%s", name, "out");	  
  sprintf(intersecname,    "%s.%s", name, "isec");	  
  sprintf(geometryname,    "%s.%s", name, "datg"); 
  sprintf(elementname,     "%s.%s", name, "date");	  
  sprintf(sourcepckname,   "%s.%s", name, "pcks"); 
  sprintf(geometrypckname, "%s.%s", name, "pckg"); 
  sprintf(elementpckname,  "%s.%s", name, "pcke"); 
  sprintf(pssourcename,    "%s.%s", name, "pss"); 
  sprintf(plotpsname,      "%s.%s", name, "ps");	  
  sprintf(printpclname,    "%s.%s", name, "pcl"); 
  sprintf(optipckname,     "%s.%s", name, "pcko"); 
  sprintf(beamlinename,    "%s.%s", name, "phase");	  
  sprintf(so4_fsource4a,   "%s.%s", name, "s4a");	  
  sprintf(so4_fsource4b,   "%s.%s", name, "s4b");	  
  sprintf(so4_fsource4c,   "%s.%s", name, "s4c");	  
  sprintf(so4_fsource4d,   "%s.%s", name, "s4d");	  
  sprintf(so6_fsource6,    "%s.%s", name, "s6");
}

// print the contents of the data for debugging
void myPHASEset::print()
{
  printf("=> myPHASEset::print()\n");
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
  printf("<= myPHASEset::print()\n");
}

void myBeamline::init()
{
  beamlineOK         = 0;
  elementzahl        = 0;
  ElementList        = NULL;
  raysout            = NULL;
  RTSource.SourceRays= NULL;
  RTSource.Quellep   = NULL;
  RESULT.points      = 0;
  RESULT.RESp        = NULL;
  localalloc         = DOALLOC;
}



#ifdef xxx

// constructor
//QtPhase::QtPhase()
//{
  //char *matrixname= new char[MaxPathLength];
//}

QtPhase::ijQtPhase()
{
  
  printf("QtPhase constructor called\n");
  
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
// end 
