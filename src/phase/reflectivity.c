/* File      : /afs/psi.ch/project/phase/src/phase/reflectivity.c */
/* Date      : <05 May 14 16:40:19 flechsig>  */
/* Time-stamp: <06 May 14 16:27:40 flechsig>  */
/* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/* $Source$  */
/* $Date$ */
/* $Revision$  */
/* $Author$  */
 
 
#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "cutils.h"
#include "phase_struct.h"
#include "phase.h"
#include "posrc.h"
#include "rtrace.h"
#include "common.h"
#include "reflectivity.h"

void apply_reflectivity_(int *blp, double *eyre, double *eyim, double *ezre, double *ezim)
{
  struct BeamlineType *bl;
  double yamp, ypha, zamp, zpha;
  double f1, f2, f1tab, f2tab, entab, a, rho;
  int    z;
  char   *element= "Au";

  bl = (struct BeamlineType *)blp;

#ifdef DEBUG
  printf("\ndebug: %s, apply_reflectivity_ called\n", __FILE__);
#endif
  
  yamp= sqrt(pow(*eyre, 2.0) + pow(*eyim, 2.0));
  zamp= sqrt(pow(*ezre, 2.0) + pow(*ezim, 2.0));
  ypha= atan2(*eyim, *eyre);
  zpha= atan2(*ezim, *ezre);

  //  ReadHenke(element, &entab, &f1tab, &f2tab);
  // ReadMaterial(element, &z, &a, &rho);

  /*
  *eyre= yamp* cos(ypha);
  *ezre= zamp* cos(zpha);
  *eyim= yamp* sin(ypha);
  *ezim= zamp* sin(zpha);
  */

#ifdef DEBUG
  printf("debug: %s, apply_reflectivity_ end\n", __FILE__);
#endif
} // end apply_reflectivity

void ReadHenke(char *element, double *en0, double *f10, double *f20)
{
  char tabname[MaxPathLength];
  FILE *f;

#ifdef DEBUG
  printf("debug: ReadHenke called for %s\n", element);
#endif

  snprintf(tabname, (MaxPathLength- 1), "%s/share/phase/%s.f12\0", getenv("PHASE_HOME"), element);
  if ((f= fopen(tabname, "r")) == NULL) 
    {
      fprintf(stderr, "error can't find Henke Table %s - return\n", tabname);
      return;
    }  

  fclose(f);
} // ReadHenke

void ReadMaterial(char *element, int *z, double *a, double *rho)
{
  char tabname[MaxPathLength], buffer[MaxPathLength], str[10];
  FILE *f;
  int  found;

#ifdef DEBUG
  printf("debug: ReadMaterial called for %s\n", element);
#endif
  *a= *rho= *z= -1;
  snprintf(tabname, (MaxPathLength- 1), "%s/share/phase/rhoatom.dat\0", getenv("PHASE_HOME"));
  if ((f= fopen(tabname, "r")) == NULL) 
    {
      fprintf(stderr, "error can't find Material Table %s - return\n", tabname);
      return;
    }

  fgets(buffer, (MaxPathLength-1), f);   // read first line
  found= strncmp(buffer, "Element\tZ\tA\t\trho", 16); // check 1st line
  if (found != 0)
    {
      fprintf(stderr, "error: Material Table %s has not the expected format- return\n", tabname);
      return;
    }

  while (! feof(f) && ! found)
    {
      fgets(buffer, (MaxPathLength-1), f);
      found= (strncmp(buffer, element, 2) == 0) ? 1 : 0;
    }

  fclose(f);

  if (! found)
    {
      fprintf(stderr, "error: Element %s not found in Material Table %s - return\n", element, tabname);
      return;
    }

  sscanf(buffer, "%9s\t%d\t%lf\t\t%lf", str, z, a, rho);

#ifdef DEBUG1
  printf("debug: found=%sparsed: %s, %d, %lf, %lf\n", buffer, str, *z, *a, *rho);
#endif
} // ReadMaterial
// end /afs/psi.ch/project/phase/src/phase/reflectivity.c
