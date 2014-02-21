 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/spa_3rd_order.c */
 /* Date      : <21 Feb 14 14:33:47 flechsig>  */
 /* Time-stamp: <21 Feb 14 15:32:37 flechsig>  */
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

#include "spa_3rd_order.h"

/* does linear interpoaltion in table */
/* underscore for fortran call        */
void spa_3rd_order_(double *x, double *y1, double *y2, int *bli)
{
  double *yt1, *yt2, weight;
  unsigned int idx;
  struct BeamlineType *bl;

#ifdef DEBUG
  printf("debug: spa_3rd_order called\n");
#endif

  bl= (struct BeamlineType *)bli;
  
  yt1= &bl->spa3table[0];
  yt2= &bl->spa3table[NSPA3RDORDER];

  if ((*x < 0) || (*x > 20))
    {
      *y1= *y2= 0;
      printf("warning: %f out of range (0...20)- return, file: %s\n", *x, __FILE__);
      return;           /* oder besser exit ?? */
    }

  idx= (unsigned int) *x/1e-3;
  
  if (idx < (NSPA3RDORDER-1))
    {
      weight= *x * 1e3 - (double)idx;                                /* (*x- idx* 1e-3)/ 1e-3; */
      *y1= weight* yt1[idx] + (1.0- weight)* yt1[idx+1];
      *y2= weight* yt2[idx] + (1.0- weight)* yt2[idx+1];
      printf("weight: %f\n", weight);
    } else
    {
      *y1= yt1[NSPA3RDORDER-1];
      *y2= yt2[NSPA3RDORDER-1];
    }
} /* end spa_3rd_order_ */

void spa3TableInit(struct BeamlineType *bl)
{
  double *yt1, *yt2;
  FILE *f;
  char fname[]= "spa3table.dat";
  int i;

  bl->spa3table= XMALLOC(double, 2* NSPA3RDORDER);
  yt1= &bl->spa3table[0];
  yt2= &bl->spa3table[NSPA3RDORDER];

  if ((f= fopen(fname, "r")) == NULL)
   {
      fprintf(stderr, "fatal Error: read %s\n", fname);
      exit(-1);
   } 
  
  /* hier kommt das einlesen */
  for (i= 0; i < NSPA3RDORDER; i++) fscanf(f, "%f %f\n", &yt1[i], &yt2[i]);
  fclose(f);
} /* spa3TableInit */

/* free pointer */
void spa3TableFree(int *bl)
{
  XFREE(bl->spa3table);
} /* end spa3TableFree */

/* end */
