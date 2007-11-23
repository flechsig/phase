/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/baselib/SetDeltaLambda.c */
/*  Date      : <14 Mar 06 11:59:48 flechsig>  */
/*  Time-stamp: <14 Mar 06 12:00:54 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#include "../phase/common.h"
#include "../phase/mirrorpck.h"
#include "../phase/geometrypck.h"
#include "../phase/phase_struct.h"


#include "phabasedefs.h"
#include "phabasestructs.h"

/**/
/* berechnet den Energieaufloesungsfaktor R=tan(fi)/displength * y */
/* last modification: 20 Oct 04 13:08:51 flechsig */
/**/
void SetDeltaLambda(struct BeamlineType *bl, struct ElementType *listpt)
{
      
   if ((listpt->GDat.inout != 0) && (bl->BLOptions.displength > 0.0) && 
       (listpt->GDat.xdens[0] > 0.0))
   {
       bl->deltalambdafactor= 1.0/(listpt->GDat.xdens[0]* 
				   (double)(abs(listpt->GDat.inout)))* 
	 (listpt->geo.cosb/ bl->BLOptions.displength); 
#ifdef DEBUG
       printf("\n**** SetdeltaLambda *******\n");
       printf(
"   debug: line dens.: %g, dl %g, cosb %g, lambda %g mm, diffr. order: %lg\n",
		listpt->GDat.xdens[0], bl->BLOptions.displength, 
		listpt->geo.cosb, listpt->GDat.lambda, 
		(double)(listpt->GDat.inout));
#endif						
       printf("SetDeltaLambda: Delta Lambda (nm) = %lg * y [mm], ", 
		bl->deltalambdafactor* 1e6);
       printf("Resolution = %lg / y (mm)\n",  
	      listpt->GDat.lambda/ bl->deltalambdafactor);  
       /* printf("*** end SetDeltaLambda ***\n");*/
   } 
   
 /* else printf("SetDeltaLambda end\n");    */
} /* end SetDeltaLambda */

/* end */
