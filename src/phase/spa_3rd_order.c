 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/spa_3rd_order.c */
 /* Date      : <21 Feb 14 14:33:47 flechsig>  */
 /* Time-stamp: <24 Feb 14 15:32:49 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */

/* zur Erläuterung:

In den Spalten stehen die Funktionswerte von

Integral(-infinity, infinity) cos(x^3)*cos(b*x^2) dx

Integral(-infinity, infinity) cos(x^3)*sin(b*x^2) dx

Bereich der b-Werte ist: 0 bis 20, Abstand der Datenpunkte 0.001 (also 20.001 Datenpunkte).

*/

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include <stdio.h>                              /* For printf and so on */
#include <stdlib.h> 	      	    	    	/* needed for fopen     */  

#include "cutils.h"   
#include "phase_struct.h"
#include "phase.h"
#include "common.h"
#include "spa_3rd_order.h"

/* does linear interpoaltion in table */
/* underscore for fortran call        */
void spa_3rd_order_(double *x, double *y1, double *y2, int *bli)
{
  double *yt1, *yt2, weight, max;
  unsigned int idx;
  struct BeamlineType *bl;

#ifdef DEBUG1
  printf("debug: spa_3rd_order called\n");
#endif

  bl= (struct BeamlineType *)bli;
  
  yt1= &bl->spa3table.tab[0];
  yt2= &bl->spa3table.tab[bl->spa3table.datapoints];
  max= bl->spa3table.datapoints* bl->spa3table.dx;
  if ((*x < 0) || (*x > max))
    {
      *y1= *y2= 0;          /* return defined output */
      printf("warning: %f out of range (0...%g)- return, file: %s\n", *x, max, __FILE__);
      return;           /* oder besser exit ?? */
    }

  idx= (bl->spa3table.dx > 0) ? (unsigned int)(*x/bl->spa3table.dx) : 0;
  
  if (idx < (bl->spa3table.datapoints-1))
    {
      weight= *x * 1e3 - (double)idx;                                /* (*x- idx* 1e-3)/ 1e-3; */
      *y1= weight* yt1[idx] + (1.0- weight)* yt1[idx+1];
      *y2= weight* yt2[idx] + (1.0- weight)* yt2[idx+1];
      //printf("weight: %f, idx= %d\n", weight, idx);
    } else
    {
      *y1= yt1[bl->spa3table.datapoints- 1];
      *y2= yt2[bl->spa3table.datapoints- 1];
    }
} /* end spa_3rd_order_ */

void spa3TableInit(struct BeamlineType *bl)
{
  double *yt1, *yt2, dx;
  FILE   *f;
  int    i, version, datapoints;
  char   buffer[MaxPathLength], *phase_home, *ch;

  if ((phase_home = getenv(PHASE_HOME)) == NULL)
    {
      printf("\n:spa3TableInit environment variable %s not defined -- exit\n", PHASE_HOME);
      exit(-1);
    } 

  snprintf(buffer, (MaxPathLength-1), "%s/share/phase/spa3table.tab", phase_home);
  printf("read sp3table: %s\n", buffer);
  
  if ((f= fopen(buffer, "r")) == NULL)
    {
      fprintf(stderr, "fatal Error: read %s\n", buffer);
      exit(-1);
    } 
  
  if( CheckFileHeader(f, (Spa3FileHeader), &version) != 0)
    {
      fprintf(stderr, "error: file: %s has not the right file header (required is: %s)- exit()\n", buffer, Spa3FileHeader);
      exit(-1);
    }
  
  fgets(buffer, MaxPathLength, f); sscanf(buffer, "# N= %d",   &bl->spa3table.datapoints);
  fgets(buffer, MaxPathLength, f); sscanf(buffer, "# dx= %le", &bl->spa3table.dx);
  
  bl->spa3table.tab= XMALLOC(double, 2* bl->spa3table.datapoints);
  yt1= bl->spa3table.tab;
  yt2= &bl->spa3table.tab[bl->spa3table.datapoints];

  i= 0;    /* hier kommt das einlesen */
  while ((i < bl->spa3table.datapoints) && !feof(f)) 
    {
      fgets(buffer, MaxPathLength, f);
      if (*buffer != '#')
	{
	  sscanf(buffer, "%le %le", &yt1[i], &yt2[i]);
	  i++;
	}
    }

  if (i != bl->spa3table.datapoints) printf("!! warning: inconsistent table !!\n");
  printf("spa3TableInit->read: %d points out of %d, dx= %g\n", i, bl->spa3table.datapoints, bl->spa3table.dx);
 
  fclose(f);
} /* spa3TableInit */

/* free pointer */
void spa3TableFree(struct BeamlineType *bl)
{
  XFREE(bl->spa3table.tab);
} /* end spa3TableFree */

/* test for debugging */
void spa3TableTest(struct BeamlineType *bl)
{
  double x, y1, y2;
  int i;

  x= uRandom(20);
  spa_3rd_order_(&x, &y1, &y2, (int *)bl);
  printf("\nspa3TableTest: x= %f, y1= %f, y2= %f\n\n", x, y1, y2);
} /* end spa3TableTest */

/* end */
