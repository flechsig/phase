/*  File      : /afs/psi.ch/project/phase/src/phase/pst_thread.h */
/*  Date      : <21 Mar 13 15:05:12 flechsig>  */
/*  Time-stamp: <27 Mar 13 14:49:38 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifndef PST_THREAD_H
#define PST_THREAD_H

struct ThreadData
{
  int start, stop, thread_no;
  struct BeamlineType *bl;
};

void *pst_it(struct ThreadData *);
void  pst_thread(struct BeamlineType *, int);

#endif
/* end pst_thread.h */
