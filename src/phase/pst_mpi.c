/*  File      : /afs/psi.ch/project/phase/src/phase/pst_mpi.c */
/*  Date      : <21 Mar 13 15:03:19 flechsig>  */
/*  Time-stamp: <24 Jun 13 10:32:23 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include <stdio.h> 


#include "cutils.h" 
#include "phase_struct.h"
#include "phase.h"
#include "pst_mpi.h"
#include "common.h"

/* we split the number of points in the pst into tasks */
int pst_mpi(struct BeamlineType *bl, struct MpiData *data)
{
  struct psimagest  *sp;
  int i, npoints;
  
#ifdef DEBUG
  printf("debug: pst_mpi file: %s, line: %d called\n", __FILE__, __LINE__);
#endif

  sp= (struct psimagest *)bl->RTSource.Quellep;
  npoints= sp->iheigh * sp->iwidth;
  bl->BLOptions.PSO.intmod= 2;   
  Test4Grating(bl);     
     
  data= XMALLOC(struct MpiData, npoints);
 
  /* Divide work for mpis, prepare parameters */
  for (i=0; i < npoints; i++) 
    {
      data[i].bl    = bl;
      data[i].mpi_no= i;
    }
   
  printf("\nmpis done, npoints= %d\n", npoints);
  bl->beamlineOK |= resultOK;
  
#ifdef DEBUG
  printf("debug: pst_mpi file: %s, line: %d done\n", __FILE__, __LINE__);
#endif
  return npoints;
} /* end pst_mpi */

/* mpi wrapper for pst_i with only one parameter */
// void *pst_it(struct MpiData *td)
void pst_impi(struct MpiData *arg, int iindex)
{
  int    index;
  struct BeamlineType *bl;
  struct map4 *m4p;
  struct constants cs;
  struct MpiData *td;
  
  td= (struct MpiData *)arg;                     /* to avoid warning */

#ifdef DEBUG
  printf("debug: pst_it file: %s, line: %d\n", __FILE__, __LINE__);
  printf("debug: calculate index  %d\n", td->mpi_no);
#endif

  bl= td->bl;
  initconstants(&cs);

  if (bl->BLOptions.ifl.pst_mode == 1)                       /* pst_mode == 1 pst with external mp4 */
    { 
      printf("allocate and fill m4p in pstc\n");
      m4p = XMALLOC(struct map4, 1);
      fill_m4(bl, m4p);
    }

  index= td->mpi_no;
  pstc_i(index, bl, m4p, &cs);
  
  if (bl->BLOptions.ifl.pst_mode == 1) XFREE(m4p);
  return NULL;
} /* pst_impi */

/* end pst_mpi.c */
