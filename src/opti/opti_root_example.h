/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/opti_root/opti_root.h */
/*  Date      : <28 Sep 12 15:04:18 flechsig>  */
/*  Time-stamp: <28 Sep 12 15:27:29 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifndef OPTI_ROOT_H
#define OPTI_ROOT_H

#include "TMinuit.h"

Float_t z[5], x[5], y[5], errorz[5];

void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
void myfill();
Double_t func(float x,float y,Double_t *par);

#endif
