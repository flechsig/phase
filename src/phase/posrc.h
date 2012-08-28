/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/posrc.h */
/*  Date      : <23 Apr 12 10:44:55 flechsig>  */
/*  Time-stamp: <28 Aug 12 12:22:50 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

/* soure routines for physical optics */
/* replaces routines in phase_source.F, only source4 is implemented so far, the reason is the extension to unlimited gridsize */

#ifndef POSRC_H
#define POSRC_H

void source4c_ini(struct BeamlineType *);
void source7c_ini(struct BeamlineType *);
void source4c_inter_2d_(struct source_results *, double *, double *, int *);
#endif 
/* end POSRC_H */
