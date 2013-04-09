/*  File      : /afs/psi.ch/project/phase/src/phase/pst_thread.c */
/*  Date      : <21 Mar 13 15:03:19 flechsig>  */
/*  Time-stamp: <09 Apr 13 16:11:37 flechsig>  */
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

/* we split the number of ponts in the pst into numthreads threads or tasks */
/* for speed it may help to allow more threads than cores */ 
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

  sp= (struct psimagest *)bl->RTSource.Quellep;
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
      data[i].start    = i     * tasksPerThread;
      data[i].stop     = (i+ 1)* tasksPerThread;
      data[i].bl       = bl;
      data[i].thread_no= i;
    }
  /* the last thread must not go past the end of the array */
  data[numthreads-1].stop= npoints;
  
  for (i=0; i<numthreads; i++) 
    pthread_create(&thread[i], NULL, pst_it, &data[i]); /* Launch Threads */
    
  for (i= 0; i< numthreads; i++) 
    pthread_join(thread[i], NULL);          /* Wait for Threads to Finish */
    
  printf("\nthreads done, npoints= %d\n", npoints);
  bl->beamlineOK |= resultOK;
  
  XFREE(thread);  /* braucht es das ? */
  XFREE(data);
  
#ifdef DEBUG
  printf("debug: pst_thread file: %s, line: %d done\n", __FILE__, __LINE__);
#endif
} /* end pst_thread */

/* thread wrapper for pst_i with only one parameter */
void *pst_it(struct ThreadData *td)
{
  int index;
  struct BeamlineType *bl;
  struct map4 *m4p;
  struct constants cs;
  struct mirrortype *am;
  struct geometryst *g;

#ifdef DEBUG
  printf("debug: pst_it file: %s, line: %d\n", __FILE__, __LINE__);
  printf("debug: calculate from index %d to %d in thread %d\n", td->start, td->stop, td->thread_no);
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
#ifdef DEBUG1
      printf("calc: %d\n", index);
#endif
      pstc_i(index, bl, m4p, &cs, am, g);
    }

  if (bl->BLOptions.ifl.pst_mode == 1) XFREE(m4p);
  return NULL;
} /* pst_it */

/* end pst_thread.c */
