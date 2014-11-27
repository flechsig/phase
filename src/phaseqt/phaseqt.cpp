//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/qtphase.cpp
//  Date      : <08 Jun 11 16:14:16 flechsig> 
//  Time-stamp: <27 Nov 14 15:46:32 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 

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


#include "phaseqt.h"


using namespace std;   // fuer cout z.B.


// for tests  
  int PhaseQt::myeval(const int &ii)
  {
    int j= ii+3;
    return j;
  }


// builtBeamline using QT threads
void PhaseQt::buildBeamlineParallel()
{
  unsigned int elcounter;
  int          imodus;
  struct ElementType *listpt; 
  struct TmpMapType  *ltp;              /* local pointer */

#ifdef DEBUG
  cout << endl << "debug: " << __FILE__ << " buildBeamlineParallel() called, beamlineOK=" << this->beamlineOK << endl;
#endif

  Check_iord(this);                                 /* check the range of iord */
  printf("BuildBeamline: Beamline contains %d element(s), %d order calculation\n", 
	 this->elementzahl,  this->BLOptions.ifl.iord);
  /*--------------------------------------------------------*/ 
  if (this->elementzahl < 1)
    return;

  if (this->beamlineOK & mapOK)  
    {   
      printf("BuildBeamline: all beamline elements are already OK- return\n");
      return;  /* nothing to do */
    }
  


  /* 1st loop */  
  elcounter= 1; 
  listpt= this->ElementList;  
  while (elcounter<= this->elementzahl)  /* Schleife ueber alle Elemente */
    { 
      if (listpt->ElementOK == 0)  /* element rebuild */
	{
	  //buildElement(listpt); // add to thread pool
	  	  
	   /* listpt-> wc,xlc,matrix,MtoSource,xlm sind erzeugt */
	   /* wc,xlc,xlm sind richtungsabhaengig !!*/
	  
#ifdef DEBUG
	  cout <<  "debug: " << __FILE__ << " BeamlineParallel() matrixes and maps of element "<< elcounter << " created" << endl;
#endif 
	}             /* map ist OK */
      else
	{
#ifdef DEBUG
	  cout <<  "debug: " << __FILE__ << " BeamlineParallel() element "<< elcounter << " already OK- keep matrix" << endl;
#endif 
	  
	} /* end if (listpt->ElementOK == 0) */
      elcounter++; listpt++; 
    } /* Schleife ueber alle Elemente fertig */

  //typedef QFutureWatcher<int> ElementWatcher;
  
  QVector <int> intvec(10);
  ElementWatcher *watcher = new ElementWatcher();
  //connect(watcher, SIGNAL(progressValueChanged(int)), progressBar, SLOT(setValue(int)));
  //QFuture<int> result   = QtConcurrent::map(intvec, myeval);
  //QFuture<int> result   = QtConcurrent::mapped(intvec, myeval);
  //watcher->setFuture(result);

#ifdef DEBUG
  cout << "debug: " << __FILE__ << " buildBeamlineParallel() end, beamlineOK=" << beamlineOK << endl << endl;
#endif
} //  end buildBeamlineParallel

// this routine should be calle parallel
void PhaseQt::buildElement(struct ElementType *listpt)
{
  int number= 99;
#ifdef DEBUG
  cout <<  "debug: " << __FILE__ << " buildElement called" << endl;
#endif

  DefMirrorC(&listpt->MDat, &listpt->mir, listpt->MDat.Art, listpt->GDat.theta0, 
	     this->BLOptions.REDUCE_maps, this->BLOptions.WithAlign, -1);    
  DefGeometryC(&listpt->GDat, &listpt->geo, &(this->BLOptions));  
  MakeMapandMatrix(listpt, this, &number); // I guess it is not used UF
} //  end buildElement

// initialize the c structure of filenames with a name
// removes a possible extension
void PhaseQt::initSet(const char *fname, const int all)
{
  char name0[MaxPathLength], *name, *ch;
  strncpy(name0, fname, MaxPathLength);
  //  FnameBody(name);

  name= basename(name0);                            // remove path
  if ((ch= strrchr(name, '.')) != NULL) *ch= '\0';  // remove extension

#ifdef DEBUG
  cout << "debug: PhaseQt::initSet called, all= " << all << endl; 
#endif

  snprintf(this->filenames.matrixname,      MaxPathLength, "%s.%s", name, "omx");
  snprintf(this->filenames.mapname,         MaxPathLength, "%s.%s", name, "map");
  if (all) snprintf(this->filenames.sourceraysname,  MaxPathLength, "%s.%s", name, "inp"); 
  snprintf(this->filenames.imageraysname,   MaxPathLength, "%s.%s", name, "out");	  
  snprintf(this->filenames.intersecname,    MaxPathLength, "%s.%s", name, "isec");	  
  snprintf(this->filenames.geometryname,    MaxPathLength, "%s.%s", name, "datg"); 
  snprintf(this->filenames.elementname,     MaxPathLength, "%s.%s", name, "date");	  
  snprintf(this->filenames.sourcepckname,   MaxPathLength, "%s.%s", name, "pcks"); 
  snprintf(this->filenames.geometrypckname, MaxPathLength, "%s.%s", name, "pckg"); 
  snprintf(this->filenames.elementpckname,  MaxPathLength, "%s.%s", name, "pcke"); 
  if (all) snprintf(this->filenames.pssourcename,    MaxPathLength, "%s.%s", name, "pss"); 
  snprintf(this->filenames.plotpsname,      MaxPathLength, "%s.%s", name, "ps");	  
  snprintf(this->filenames.printpclname,    MaxPathLength, "%s.%s", name, "pcl"); 
  snprintf(this->filenames.optipckname,     MaxPathLength, "%s.%s", name, "pcko"); 
  snprintf(this->filenames.beamlinename,    MaxPathLength, "%s.%s", name, "phase");
  //snprintf(this->filenames.beamlinename,    MaxPathLength, "%s", name0);  
  if (all) snprintf(this->filenames.so4_fsource4a,   MaxPathLength, "%s.%s", name, "s4a");	  
  if (all) snprintf(this->filenames.so4_fsource4b,   MaxPathLength, "%s.%s", name, "s4b");	  
  if (all) snprintf(this->filenames.so4_fsource4c,   MaxPathLength, "%s.%s", name, "s4c");	  
  if (all) snprintf(this->filenames.so4_fsource4d,   MaxPathLength, "%s.%s", name, "s4d");	  
  if (all) snprintf(this->filenames.so6_fsource6,    MaxPathLength, "%s.%s", name, "s6");
  if (all) snprintf(this->filenames.so7_hdf5,        MaxPathLength, "%s.%s", name, "h5");
  snprintf(this->filenames.hdf5_out,        MaxPathLength, "%s_out.%s", name, "h5");
  snprintf(this->filenames.h5surfacename,   MaxPathLength, "%s_surf_err.%s", name, "h5");
  snprintf(this->filenames.opresname,       MaxPathLength, "%s.%s", name, "opti");	  
} // initSet

// print the contents of the data for debugging
void PhaseQt::printSet()
{
  printf("=> PhaseQt::printSet()\n");
  printf("matrixname:      %s\n", this->filenames.matrixname);
  printf("mapname:         %s\n", this->filenames.mapname);
  printf("sourceraysname:  %s\n", this->filenames.sourceraysname ); 
  printf("imageraysname:   %s\n", this->filenames.imageraysname);	  
  printf("intersecname:    %s\n", this->filenames.intersecname);	  
  printf("geometryname:    %s\n", this->filenames.geometryname);    
  printf("elementname:     %s\n", this->filenames.elementname);  	  
  printf("sourcepckname:   %s\n", this->filenames.sourcepckname);   
  printf("geometrypckname: %s\n", this->filenames.geometrypckname); 
  printf("elementpckname:  %s\n", this->filenames.elementpckname);  
  printf("pssourcename:    %s\n", this->filenames.pssourcename);    
  printf("plotpsname:      %s\n", this->filenames.plotpsname); 	  
  printf("printpclname:    %s\n", this->filenames.printpclname);    
  printf("optipckname:     %s\n", this->filenames.optipckname);     
  printf("beamlinename:    %s\n", this->filenames.beamlinename);	  
  printf("so4_fsource4a:   %s\n", this->filenames.so4_fsource4a);	  
  printf("so4_fsource4b:   %s\n", this->filenames.so4_fsource4b);	  
  printf("so4_fsource4c:   %s\n", this->filenames.so4_fsource4c);	  
  printf("so4_fsource4d:   %s\n", this->filenames.so4_fsource4d);	  
  printf("so6_fsource6:    %s\n", this->filenames.so6_fsource6);
  printf("so7_hdf5:        %s\n", this->filenames.so7_hdf5); 
  printf("hdf5_out:        %s\n", this->filenames.hdf5_out); 
  printf("opresname:       %s\n", this->filenames.opresname);
  
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
  RESULT.points1     = 0;
  RESULT.points2     = 0;
  RESULT.RESp        = NULL;
  localalloc         = DOALLOC;
  tp                 = NULL;
  posrc.zeyre        = NULL;
  posrc.zeyim        = NULL;
  posrc.zezre        = NULL;
  posrc.zezim        = NULL;
  posrc.gridx        = NULL;
  posrc.gridy        = NULL;
  posrc.iconj        = 0;
  spa3table.tab      = NULL;
  emfp               = NULL;
  emf.nz= 0;
  emf.ny= 0;
  emf.y= NULL;
  emf.z= NULL;
  emf.eyre= NULL;
  emf.eyim= NULL;
  emf.ezre= NULL;
  emf.ezim= NULL;
} // end initBeamline


// returns a pointer to the beamline struct
struct BeamlineType *PhaseQt::myBeamline()
{
  return (struct BeamlineType *)this;
} // end myBeamline

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
  ActualTask= 0;
  mainWin= NULL;

  initSet("default", INIT_ALL);   // initialize the filename structure
  initBeamline();       // initialize the beamline structure
} // end constructor

/*
#ifdef xxx  
  Beamline.localalloc= DOALLOC;       // init should go somwhere else 

#ifdef EXPIRE
       printf(" defined expire date: %d\n", EXPIRE);
       time(&timev);
       local_time= localtime(&timev);
       // debug       printf("%d %d %d\n\n", local_time->tm_year, local_time->tm_mon, local_time->tm_mday); 

       if (  local_time->tm_mday + 
	    (local_time->tm_mon  + 1) * 100 + 
	    (local_time->tm_year + 1900) * 10000 > EXPIRE )
	 {
	   printf("\n Program PHASE expired..., terminating\n Please, contact Johannes Bahrdt\n\n");
	   exit(1);
	 } else printf("  The program is not expired!\n");

       //       already_expired(); 
#else
    printf(" The program does not expire!\n\n");
#endif

#ifdef SEVEN_ORDER
    printf(" PHASE version > Nov: 2010: SEVEN_ORDER defined\n\n");
#else
    printf(" PHASE version > Nov: 2010: SEVEN_ORDER undefined\n\n");
#endif
// Mar 10: get the data directory at runtime from ENV variable again, not at compile time
    if ((global_rundir = getenv(PHASE_HOME)) == NULL)
    {
      printf("\nphase.c: needed environment variable %s not defined -- exiting\n", PHASE_HOME);
      exit(-1);
    } 
    else printf("Runtime directory is %s\n", global_rundir);
    
    
    
    //  InitDataSets(&PHASESet, (char*) MainPickName);             /* PHASEc.c 

//}

#endif
*/

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
      strncpy(fp->filename, this->filenames.sourceraysname, MaxPathLength);
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

  strncpy(buffer, this->filenames.beamlinename, (MaxPathLength-1));
  buffer[MaxPathLength-2]= '\0';
  strcat(buffer, "~");
  
#ifdef DEBUG
  printf("writeBackupFile: -> ");
#endif
  WriteBLFile(buffer, this);
} // writeBackupFile()

// end 
