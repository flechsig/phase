/*  File      : /afs/psi.ch/project/phase/src/phase/pst_thread.c */
/*  Date      : <21 Mar 13 15:03:19 flechsig>  */
/*  Time-stamp: <26 Mar 13 10:47:21 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif 

#include <stdio.h> 
#include <pthread.h>

#include "cutils.h" 
#include "phase_struct.h"
#include "phase.h"
#include "pst_thread.h"
#include "common.h"

void pst_thread(struct BeamlineType *bl, int numthreads)
{
  pthread_t *thread;
  struct ThreadData *data;
  struct psimagest *sp;
  int i, tasksPerThread, npoints;

  thread= XMALLOC(pthread_t, numthreads);
  data  = XMALLOC(struct ThreadData, numthreads);

#ifdef DEBUG
  printf("debug: pst_thread file: %s, line: %d called\n", __FILE__, __LINE__);
#endif

  sp=   (struct psimagest *)bl->RTSource.Quellep;
  npoints= sp->iheigh * sp->iwidth;

  
  /*
    this has the effect of rounding up the number of tasks
    per thread, which is useful in case ARRAYSIZE does not
    divide evenly by NUMTHREADS.
  */
  tasksPerThread=(npoints+ numthreads-1)/numthreads;
  
  /* Divide work for threads, prepare parameters */
  for (i=0; i<numthreads; i++) 
    {
      data[i].start=i*tasksPerThread;
      data[i].stop=(i+1)*tasksPerThread;
      data[i].bl=bl;
    }
  /* the last thread must not go past the end of the array */
  data[numthreads-1].stop= npoints;
  
  /* Launch Threads */
  for (i=0; i<numthreads; i++) 
    {
      pthread_create(&thread[i], NULL, pst_it, &data[i]);
    }
  
  /* Wait for Threads to Finish */
  for (i=0; i<numthreads; i++) 
    {
      pthread_join(thread[i], NULL);
    }
  
  printf("threads done, npoints= %d\n", npoints);
  bl->beamlineOK |= resultOK;

  XFREE(data);
  XFREE(thread);

#ifdef DEBUG
  printf("debug: pst_thread file: %s, line: %d done\n", __FILE__, __LINE__);
#endif
  
} /* end pst_thread */

/* thread wrapper for pst_i */
void *pst_it(struct ThreadData *td)
{
  int index;
  struct BeamlineType *bl;
  struct map4 *m4p;
  struct constants cs;
  struct mirrortype *am;
  struct geometryst *g;

#ifdef DEBUG
  printf("debug: pst_it file: %s, line: %d done\n", __FILE__, __LINE__);
  printf("debug: calculate from index %d to %d\n", td->start, td->stop);
#endif

  bl= td->bl;
  bl->BLOptions.PSO.intmod= 2;

  initconstants(&cs);
  if (bl->BLOptions.ifl.pst_mode == 1)                       /* pst_mode == 1 pst with external mp4 */
    { 
      printf("allocate and fill m4p in pstc\n");
      m4p = XMALLOC(struct map4, 1);
      fill_m4(bl, m4p);
    }

    Test4Grating(bl, &am, &g);


  for (index= td->start; index < td->stop; index++) 
    {
      printf("calc: %d\n", index);
      pstc_i(index, bl, m4p, &cs, am, g);
    }

  //XFREE(m4p);
  return NULL;
} /* pst_it */

/* end pst_thread.c */
