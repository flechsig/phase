/* File      : /afs/psi.ch/project/phase/src/phase/reflectivity.c */
/* Date      : <05 May 14 16:40:19 flechsig>  */
/* Time-stamp: <08 May 14 16:11:33 flechsig>  */
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
  struct ElementType  *ep;
  struct ReflecType   *rp;
  double yamp, ypha, zamp, zpha;
    

  bl= (struct BeamlineType *)blp;
  ep= (struct ElementType *)bl->ElementList;
  rp= (struct ReflecType *)&ep->reflec;

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

void ReadHenke(char *element, double energy, double *f1, double *f2)
{
  char   tabname[MaxPathLength], buffer[MaxPathLength];
  FILE   *f;
  double e1, e2, f11, f12, f21, f22, de;
  int    found;

#ifdef DEBUG
  printf("debug: ReadHenke called for >>%s<<\n", element);
#endif

  *f1= *f2= -1;
  snprintf(tabname, (MaxPathLength- 1), "%s/share/phase/%s.f12\0", getenv("PHASE_HOME"), element);

#ifdef DEBUG
  printf("debug: open table >>%s<<\n", tabname);
#endif

  if ((f= fopen(tabname, "r")) == NULL) 
    {
      fprintf(stderr, "error can't find Henke Table %s - return\n", tabname);
      return;
    }  

  if ((energy < 10) || (energy > 30000)) 
    {
      fprintf(stderr, "error ReadHenke- energy- %f out of tabulated range (10eV..30keV) - return\n", energy);
      return;
    }  
 
  fgets(buffer, (MaxPathLength-1), f);                 // read first line
  if (strstr(buffer, "(Energy (eV),f1,f2)") == NULL)   // check 1st line
    {
      fprintf(stderr, "error: Henke Table %s has not the expected format- return\n", tabname);
      return;
    }
  
  fgets(buffer, (MaxPathLength-1), f);                 // read first data
  sscanf(buffer, "%lf %lf %lf", &e1, &f11, &f21);
  found= 0;
  while (! feof(f) && ! found)
    {
      fgets(buffer, (MaxPathLength-1), f);
      sscanf(buffer, "%lf %lf %lf", &e2, &f12, &f22);
      if (e2 < energy)
	{
	  e1 = e2;
	  f11= f12;
	  f21= f22;
	}
      else 
	found= 1;
    }
  fclose(f);

  if (! found)
    {
      fprintf(stderr, "error: parsing Henke Table, energy %f out of range- return\n", energy);
      return;
    }
  // interpolate
  de= e2- e1; 

  //  printf("%lf %lf %lf de= %lf\n",     e1, f11, f21, de);
  //  printf("%lf %lf %lf energy= %lf\n", e2, f12, f22, energy);
  if (de > 0.0)
    {
      *f1= f11+ (f12- f11)/de * (energy- e1);
      *f2= f21+ (f22- f21)/de * (energy- e1);
    }

#ifdef DEBUG
  printf("debug: ReadHenke end: material=%s, energy=%lf, f1=%lf, f2=%lf\n", element, energy, *f1, *f2);
#endif

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

  fgets(buffer, (MaxPathLength-1), f);                  // read first line
  found= strncmp(buffer, "Element\tZ\tA\t\trho", 16);   // check 1st line
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
      fprintf(stderr, "error: Element >>%s<< not found in Material Table %s - return\n", element, tabname);
      return;
    }

  sscanf(buffer, "%9s\t%d\t%lf\t\t%lf", str, z, a, rho);

#ifdef DEBUG
  printf("debug: found: material=%s, z=%d, a=%lf, rho=%lf\n", str, *z, *a, *rho);
#endif
} // ReadMaterial

// expect wavelength in m
void SetReflectivity(struct ElementType *ep, double wavelength)
{
  double f1, f2, a, rho, energy, nt, delta, beta, ac, sinag, cosag;
  int    z;
  COMPLEX n, n2, wu, crs, cts, crp, ctp, c1, c2;
  char *material;
  struct ReflecType *rp;

  material= ep->MDat.material;
  rp= (struct ReflecType *)&ep->reflec;

#ifdef DEBUG
  printf("debug: SetReflectivity called, material= >%s<, file= %s\n", material, __FILE__);
#endif

  if (!(wavelength > 0.0))
    {
      fprintf(stderr, "error SetReflectivity: wavelength not defined (%f)- return");
      return;
    }

  energy= 1240e-9/ wavelength; 
  ReadMaterial(material, &z, &a, &rho);
  ReadHenke(material, energy, &f1, &f2);

  
  nt= 1e6* rho * NA / a;              // Teilchendichte  (1/m^3), rho is in (g/cm^3)

  delta= RE * pow(wavelength, 2) * nt * f1 / (2.0 * PI);
  beta = RE * pow(wavelength, 2) * nt * f2 / (2.0 * PI);
  ac   = acos(1.0 - delta);            // critical (grazing) angle in rad
  complex_in(&n, (1.0- delta), beta);  // complex index of refraction

  sinag= ep->geo.cosa;           // sin(grazing angle) grazing angle in rad
  cosag= ep->geo.sina;           // sin <-> cos change for grazing angle

  complex_x(&n, &n, &n2);        // n^2
  complex_in(&c1, pow(cosag, 2.0), 0.0);
  complex_minus(&n2, &c1, &c2);
  //complex_pow(&c2, 0.5, &wu);
  
  // fill double ryamp, ryphas, rzamp, rzphas, runpol;

} // SetReflectivity
// end /afs/psi.ch/project/phase/src/phase/reflectivity.c
