/* File      : /afs/psi.ch/project/phase/src/phase/reflectivity.c */
/* Date      : <05 May 14 16:40:19 flechsig>  */
/* Time-stamp: <27 Jun 14 14:09:10 flechsig>  */
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

//void apply_reflectivity_(int *blp, double *eyre, double *eyim, double *ezre, double *ezim)
void apply_reflectivity(struct BeamlineType *bl, double *eyre, double *eyim, double *ezre, double *ezim)
{
  //  struct BeamlineType *bl;
  struct ElementType  *ep;
  struct ReflecType   *rp;
  double yamp, ypha, zamp, zpha;
    
  //bl= (struct BeamlineType *)blp;
  ep= (struct ElementType *)bl->ElementList;
  rp= (struct ReflecType *)&ep->reflec;

  // works that way only for one element (we read the reflectivity of the first element)!!!

#ifdef DEBUG1
  printf("debug: %s, apply_reflectivity_ called (single element)\n", __FILE__);
#endif

  if (! bl->BLOptions.PSO.with_coating) 
    {
#ifdef DEBUG1
      printf("debug: %s reflectivity calculation switched off - return\n", __FILE__);
#endif
      return;
    }
  
  yamp= sqrt(pow(*eyre, 2.0) + pow(*eyim, 2.0)) * rp->ryamp;
  zamp= sqrt(pow(*ezre, 2.0) + pow(*ezim, 2.0)) * rp->rzamp;
  ypha= atan2(*eyim, *eyre)  + rp->rypha;
  zpha= atan2(*ezim, *ezre)  + rp->rzpha;

  *eyre= yamp* cos(ypha);
  *ezre= zamp* cos(zpha);
  *eyim= yamp* sin(ypha);
  *ezim= zamp* sin(zpha);
  
} // end apply_reflectivity

int ReadHenke(char *element, double energy, double *f1, double *f2)
{
  char   tabname[MaxPathLength], buffer[MaxPathLength];
  FILE   *f;
  double e1, e2, f11, f12, f21, f22, de;
  int    found;

#ifdef DEBUG1
  printf("debug: ReadHenke called for >>%s<<\n", element);
#endif

  *f1= *f2= -1;
  snprintf(tabname, (MaxPathLength- 1), "%s/share/phase/%s.f12\0", getenv("PHASE_HOME"), element);

#ifdef DEBUG1
  printf("debug: open table >>%s<<\n", tabname);
#endif


  // do some tests
  if ((f= fopen(tabname, "r")) == NULL) 
    {
      fprintf(stderr, "error can't find Henke Table %s - return\n", tabname);
      return 0;
    }  

  if ((energy < 10) || (energy > 30000)) 
    {
      fprintf(stderr, "error ReadHenke- energy- %f out of tabulated range (10eV..30keV) - return\n", energy);
      return 0;
    }  
 
  fgets(buffer, (MaxPathLength- 1), f);                // read first line (header)
  if (strstr(buffer, "(Energy (eV),f1,f2)") == NULL)   // check 1st line !! do not edit pattern !! 
    {
      fprintf(stderr, "error: Henke Table %s has not the expected format- return\n", tabname);
      return 0;
    }
  
  fgets(buffer, (MaxPathLength-1), f);                 // read first data line
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
      return 0;
    }
  // interpolate
  de= e2- e1; 
  if (de > 0.0)
    {
      *f1= f11+ (f12- f11)/de * (energy- e1);
      *f2= f21+ (f22- f21)/de * (energy- e1);
    }

#ifdef DEBUG1
  printf("debug: ReadHenke end: material=%s, energy=%lf, f1=%lf, f2=%lf\n", element, energy, *f1, *f2);
#endif
  return 1;
} // ReadHenke

int ReadMaterial(char *element, int *z, double *a, double *rho)
{
  char tabname[MaxPathLength], buffer[MaxPathLength], str[10];
  FILE *f;
  int  found;

#ifdef DEBUG1
  printf("debug: ReadMaterial called for %s\n", element);
#endif

  *a= *rho= *z= -1;
  snprintf(tabname, (MaxPathLength- 1), "%s/share/phase/rhoatom.dat\0", getenv("PHASE_HOME"));
  if ((f= fopen(tabname, "r")) == NULL) 
    {
      fprintf(stderr, "error can't find Material Table %s - return\n", tabname);
      return 0;
    }

  fgets(buffer, (MaxPathLength-1), f);                  // read first line
  found= strncmp(buffer, "Element\tZ\tA\t\trho", 16);   // check 1st line
  if (found != 0)
    {
      fprintf(stderr, "error: Material Table %s has not the expected format- return\n", tabname);
      return 0;
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
      return 0;
    }

  sscanf(buffer, "%9s\t%d\t%lf\t\t%lf", str, z, a, rho);

#ifdef DEBUG1
  printf("debug: found: material=%s, z=%d, a=%lf, rho=%lf\n", str, *z, *a, *rho);
#endif
  return 1;
} // ReadMaterial

// expect wavelength in m
int SetReflectivity(struct ElementType *ep, double wavelength)
{
  double f1, f2, a, rho, energy, nt, delta, beta, ac, sinag, cosag, Rs, Rp;
  int    z, myreturn;
  COMPLEX cn, cn2, cwu, crs, cts, crp, ctp, c1, c2, c3, csinag;
  char    *material;
  struct ReflecType *rp;

  material= ep->MDat.material;
  rp= (struct ReflecType *)&ep->reflec;

#ifdef DEBUG
  printf("debug: SetReflectivity called, material= >%s<, file= %s\n", material, __FILE__);
#endif

  if (!(wavelength > 0.0))
    {
      fprintf(stderr, "error SetReflectivity: wavelength not defined (%f)- return");
      return 0;
    }

  energy= 1240e-9/ wavelength; 
  myreturn=   ReadMaterial(material, &z, &a, &rho);
  myreturn &= ReadHenke(material, energy, &f1, &f2);

  nt= 1e6* rho * NA / a;              // Teilchendichte  (1/m^3), rho is in (g/cm^3)

  delta= RE * pow(wavelength, 2) * nt * f1 / (2.0 * PI);
  beta = RE * pow(wavelength, 2) * nt * f2 / (2.0 * PI);
  ac   = acos(1.0 - delta);            // critical (grazing) angle in rad
  complex_in(&cn, (1.0- delta), beta);  // complex index of refraction

  sinag= ep->geo.cosa;           // sin(grazing angle) grazing angle in rad
  cosag= ep->geo.sina;           // sin <-> cos change for grazing angle

  // we calculate the compex reflectivity and transmission coefficients
  // transmission coefficients not used so far
  complex_x    (&cn,  &cn, &cn2);              // n^2
  complex_in   (&c1,  pow(cosag, 2.0), 0.0);   // cos(theta))^2 saved in c1
  complex_minus(&cn2, &c1, &c2);               // c2= n2- c1
  complex_pow  (&c2,  0.5, &cwu);              // wu= sqrt(c2)

  complex_in   (&csinag, sinag, 0.0);        // sin(theta) saved in csinag
  complex_minus(&csinag, &cwu,  &c1);        // zehler in c1
  complex_plus (&csinag, &cwu,  &c2);        // nenner in c2
  complex_div  (&c1,     &c2,   &crs);       // calc crs

  complex_in (&c1, (2* sinag), 0.0);         // zehler in c1
  complex_div(&c1, &c2,        &cts);        // calc cts

  complex_x    (&cn2, &csinag, &c3);         // c3
  complex_minus(&c3,  &cwu,    &c1);         // zehler in c1  
  complex_plus (&c3,  &cwu,    &c2);         // nenner in c2  
  complex_div  (&c1,  &c2,     &crp);        // calc crp

  complex_in (&c1, 2.0,     0.0);           // 2.0 in c1
  complex_x  (&c1, &cn,     &c3);           // 2n in c3
  complex_x  (&c3, &csinag, &c1);           // zaehler in c1
  complex_div(&c1, &c2,     &ctp);          // calc ctp

  Rs= pow(crs.re, 2)+ pow(crs.im, 2);       // abs()^2
  Rp= pow(crp.re, 2)+ pow(crp.im, 2);       // abs()^2;

  rp->runpol= 0.5 * (Rs + Rp);

  // fill double ryamp, rypha, rzamp, rzpha, runpol;
  switch (ep->GDat.azimut) /* vertikal 0; nach links 1; nach unten 2 ; nach rechts 3 */
    {
    case 0: 
    case 2:
      rp->ryamp= sqrt(pow(crp.re, 2)+ pow(crp.im, 2));
      rp->rypha= atan2(crp.im, crp.re);
      rp->rzamp= sqrt(pow(crs.re, 2)+ pow(crs.im, 2));
      rp->rzpha= atan2(crs.im, crs.re);
      break;
    case 1:
    case 3:
      rp->rzamp= sqrt(pow(crp.re, 2)+ pow(crp.im, 2));
      rp->rzpha= atan2(crp.im, crp.re);
      rp->ryamp= sqrt(pow(crs.re, 2)+ pow(crs.im, 2));
      rp->rypha= atan2(crs.im, crs.re);
      break;
    
    default: 
      fprintf(stderr, "error in file %s- azimut >>%d<<out of range\n", __FILE__, ep->GDat.azimut);
      exit(-1);
    }

  return myreturn;
} // SetReflectivity
// end /afs/psi.ch/project/phase/src/phase/reflectivity.c
