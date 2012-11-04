/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/opti_root/opti_root.h */
/*  Date      : <28 Sep 12 15:04:18 flechsig>  */
/*  Time-stamp: <2012-11-04 16:58:33 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifndef PHASEOPTI_H
#define PHASEOPTI_H

#include "TMinuit.h"

/*prototypes*/
double Qfunction(struct BeamlineType *, int);

void   FCN(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
void   SaveOptimizedBeamlineRoot(struct BeamlineType *, struct optistruct *);
void   save_output(TMinuit *, struct BeamlineType *, struct optistruct *);

#endif
