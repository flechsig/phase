/*  File      : /afs/psi.ch/project/phase/src/phase/pst_thread.c */
/*  Date      : <21 Mar 13 15:03:19 flechsig>  */
/*  Time-stamp: <15 Jan 15 11:09:45 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

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


#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include <stdio.h> 
#include <pthread.h>

#include "cutils.h" 
#include "phase_struct.h"
#include "phase.h"
#include "pst_thread.h"
#include "pst.h"
#include "posrc.h"
#include "common.h"

/* we split the number of points in the pst into numthreads threads or tasks */
/* for speed it may help to allow more threads than cores */ 
void pst_thread(struct BeamlineType *bl, int numthreads)
{
  pthread_t         *thread;
  struct ThreadData *data;
  struct psimagest  *sp;
  int i, tasksPerThread, npoints;
  
  if (numthreads < 1)
    {
      fprintf(stderr, "error: number of threads= %d < 1, -- exit\n", numthreads);
      exit(-1);
    }

  thread= XMALLOC(pthread_t, numthreads);
  data  = XMALLOC(struct ThreadData, numthreads);

#ifdef DEBUG
  printf("debug: pst_thread file: %s, line: %d called\n", __FILE__, __LINE__);
#endif

  sp= (struct psimagest *)bl->RTSource.Quellep;
  npoints= sp->iheigh * sp->iwidth;
  bl->BLOptions.PSO.intmod= 2;   
  Test4Grating(bl);     

  bl->RESULT.outside_wl= 0;  
  if (bl->emfp) 
    {
      emfp_free(bl->emfp);
      bl->emfp= NULL;
    }
  bl->emfp= (struct EmfType *)emfp_construct(bl->source_emfp->nz, bl->source_emfp->ny);
  emfp_cpy(bl->emfp, bl->source_emfp); // source-> emfp
  if (bl->result_emfp) bl->result_emfp= (struct EmfType *)emfp_free(bl->result_emfp);  // clean up result
  bl->result_emfp= (struct EmfType *)emfp_construct(sp->iwidth, sp->iheigh); // !! image plane - not source
  bl->position= 0;
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
  
  printf("\nthreads created- wait until finished\n\n");
  
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
// void *pst_it(struct ThreadData *td)
void *pst_it(void *arg)
{
  int    index;
  struct BeamlineType *bl;
  struct map4 *m4p;
  struct constants cs;
  struct ThreadData *td;
  
  td= (struct ThreadData *)arg;                     /* to avoid warning */

#ifdef DEBUG
  printf("debug: pst_it file=%s, line=%d", __FILE__, __LINE__);
  printf(" calc. from index %d to %d in thread %d\n", td->start, td->stop, td->thread_no);
#endif

  bl= td->bl;
  initconstants(&cs);

  if (bl->BLOptions.ifl.pst_mode == 1)                       /* pst_mode == 1 pst with external mp4 */
    { 
      printf("allocate and fill m4p in pstc\n");
      m4p = XMALLOC(struct map4, 1);
      printf("UF Warning- critical fix line 153 pst_thread.c\n");
      fill_m4(bl, m4p, NULL);
    }

  for (index= td->start; index < td->stop; index++) 
    {
#ifdef DEBUG1
      printf("calc: %d\n", index);
#endif
      pstc_i(index, bl, m4p, &cs);
    }

  if (bl->BLOptions.ifl.pst_mode == 1) XFREE(m4p);
  return NULL;
} /* pst_it */

/* end pst_thread.c */
