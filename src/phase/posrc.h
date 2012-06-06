/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/posrc.h */
/*  Date      : <23 Apr 12 10:44:55 flechsig>  */
/*  Time-stamp: <06 Jun 12 15:58:11 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

/* soure routines for physical optics */

#ifndef POSRC_H
#define POSRC_H

void psdi4c(struct BeamlineType *, struct constants *, struct source_results *);
void source4c(struct BeamlineType *, struct constants *, struct source_results *);
void source4c_ini(struct BeamlineType *);
void source4c_inter_2d(struct source4c *, struct source_results *, double *, double *);
#endif 
/* end POSRC_H */
