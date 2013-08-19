/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase4idl/debug_phase4idl.c */
/*  Date      : <16 Aug 13 16:40:16 flechsig>  */
/*  Time-stamp: <16 Aug 13 17:40:48 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#include <stdio.h>
#include "source4x.h"

main ()
{
  struct source4  s4;
  //struct source4c s4c;
  int i;
  int ianzz= 10, ianzy= 10;
  double zmin= -1.0, zmax= 1.0, ymin= -1.0, ymax= 1.0, w0= 1.0, deltax= 1.0, xlambda=1e-10, ez0= 1.0, ey0= 1.0, dphi_zy=1.0;
                                        

  printf("debug_phase4idl.c called\n");

  i= phaSrcWFGauss( &s4, &ianzz, &zmin, &zmax, &ianzy, &ymin, &ymax,
					 &w0, &deltax, &xlambda,
					 &ez0, &ey0, &dphi_zy);


}
