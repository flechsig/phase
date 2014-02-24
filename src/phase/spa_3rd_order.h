 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/spa_3rd_order.h */
 /* Date      : <21 Feb 14 14:35:30 flechsig>  */
 /* Time-stamp: <24 Feb 14 15:25:53 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */

#ifndef SPA_3RD_ORDER_H
#define SPA_3RD_ORDER_H

#define Spa3FileHeader "#_Spa3TableType"  

void spa_3rd_order_(double *, double *, double *, int *);
void spa3TableFree(struct BeamlineType *);
void spa3TableInit(struct BeamlineType *);
void spa3TableTest(struct BeamlineType *);
#endif
/* end */
