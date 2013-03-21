/*  File      : /afs/psi.ch/project/phase/src/phase/pst_thread.c */
/*  Date      : <21 Mar 13 15:03:19 flechsig>  */
/*  Time-stamp: <21 Mar 13 15:09:05 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif 

#include <stdio.h> 

#include "pst_thread.h"

void pst_thread()
{
#ifdef DEBUG
  printf("debug: file: %s, line: %d called\n", __FILE__, __LINE__);
#endif

#ifdef DEBUG
  printf("debug: file: %s, line: %d done\n", __FILE__, __LINE__);
#endif

} /* end pst_thread */

/* end pst_thread.c */
