/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phasesrv/phasesrv.c */
/*  Date      : <14 Sep 12 16:34:45 flechsig>  */
/*  Time-stamp: <2012-09-19 14:30:29 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif 

#include <stdio.h>
#include <stdlib.h>

#include "cutils.h" 
#include "phase_struct.h"
#include "phase.h"
#include "rtrace.h"
#include "phasesrv.h"

int main(unsigned int argc, char *argv[])
{
  int setupswitch, cmode, selected, iord;
  struct BeamlineType Beamline;
  
  Beamline.localalloc= DOALLOC;
  
  printf("phasesrv start\n");
  
  setupswitch= ProcComandLine(&Beamline.filenames, argc, argv, &cmode, &selected, &iord);
  
  switch (setupswitch)
    {
    case 3:
    case 7:
    case 15:
      printf("main: Batchmode  called\n");
      BatchMode(&Beamline, cmode, selected, iord);
      exit(3);
      break;
    case 5:          // only filename given
      printf("main: filename provided- do not read %s\n", (char*) MainPickName); 
      printf("not implemented so far - usage: phasesrv -h\nexit()\n");
      exit(3);
      //myphaseQt.initSet(myphaseQt.myBeamline()->filenames.beamlinename);
      break;
    default:
      printf("\nusage: phasesrv -h\n\n");
      //myphaseQt.myGetPHASE((char*) MainPickName);
    }

  printf("phasesrv done\n");
}
/* end */
