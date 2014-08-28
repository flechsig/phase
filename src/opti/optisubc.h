/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/opti/optisub.h */
/*  Date      : <04 Jan 08 14:04:24 flechsig>  */
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

/* fuer optimierung und extrahierung */


#ifndef OPTISUBC_H
#define OPTISUBC_H  

/* double */      
double DeltaLambda(struct optistruct *, double, int);
double out_struct(struct BeamlineType  *, double *,  int); 

/* void */
void buildsystem(struct BeamlineType *);
void FocusSize(double *, struct BeamlineType *, double *, double *); 
void FullRTOpti(double *, struct BeamlineType *);
void Get_dydz_fromSource(struct BeamlineType *, double *, double *);
void GetFWHM(struct BeamlineType *, char *, double *);
void GetResults(struct BeamlineType *, double *, double *, double *, 
		double *, double *, double *);
void GetRMS(struct BeamlineType *, char *, double *);
void in_struct(struct BeamlineType *, double *, int);
void RTOpti(double *, struct BeamlineType *, char *);
void SaveOptimizedBeamline(struct BeamlineType *, struct optistruct *);

#endif       
/* end optisubc.h */
