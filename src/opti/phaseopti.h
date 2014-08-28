/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/opti_root/opti_root.h */
/*  Date      : <28 Sep 12 15:04:18 flechsig>  */
/*  Time-stamp: <28 Aug 14 16:52:02 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

/****************************************************************************
**
** Copyright (C) 2005-2006 Trolltech ASA. All rights reserved.
**
** This file is part of the example classes of the Qt Toolkit.
**
** This file may be used under the terms of the GNU General Public
** License version 2.0 as published by the Free Software Foundation
** and appearing in the file LICENSE.GPL included in the packaging of
** this file.  Please review the following information to ensure GNU
** General Public Licensing requirements will be met:
** http://www.trolltech.com/products/qt/opensource.html
**
** If you are unsure which license is appropriate for your use, please
** review the following information:
** http://www.trolltech.com/products/qt/licensing.html or contact the
** sales department at sales@trolltech.com.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
****************************************************************************/

#ifndef PHASEOPTI_H
#define PHASEOPTI_H

#include "TMinuit.h"

/*prototypes*/
double Qfunction(struct BeamlineType *, int);

void   FCN(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
void   SaveOptimizedBeamlineRoot(struct BeamlineType *, struct optistruct *);
void   save_output(TMinuit *, struct BeamlineType *, struct optistruct *);

#endif
