/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phasesrv/phasesrv.c */
/*  Date      : <14 Sep 12 16:34:45 flechsig>  */
/*  Time-stamp: <03 Dec 14 13:47:13 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

// ******************************************************************************
//
//   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
//                      Paul Scherrer Institut Villigen, Switzerland
//   
//   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
//          Uwe Flechsig,    uwe.flechsig@psi.ch
//
// ------------------------------------------------------------------------------
//
//   This file is part of PHASE.
//
//   PHASE is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, version 3 of the License, or
//   (at your option) any later version.
//
//   PHASE is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
//
// ******************************************************************************

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "cutils.h" 
#include "phase_struct.h"
#include "phase.h"
#include "rtrace.h"
#include "phasesrv.h"

int main(int argc, char *argv[])
{
  int setupswitch, cmode, selected, iord, numthreads, format;
  struct BeamlineType Beamline;
  
  Beamline.localalloc= DOALLOC;  /* phasesrv should reserve the memory */ 
  cmode= selected= iord= numthreads= format= 0;
  printf("phasesrv start\n");
  
  setupswitch= ProcComandLine(&Beamline.filenames, argc, argv, &cmode, 
			      &selected, &iord, &numthreads, &format);

#ifdef DEBUGX 
  strncpy(Beamline.filenames.beamlinename, "test_5000.phase", MaxPathLength- 1);  /* for debugging */
#endif 

  switch (setupswitch)
    {
    case 1:  // ohne -b
    case 3:
    case 5:  // option -b braucht ist optional
    case 7:
    case 13: // ohne -b
    case 15:
      printf("main: Batchmode  called (switch= %d)\n", setupswitch);
      BatchMode(&Beamline, cmode, selected, iord, numthreads, format);
      break;
    default:
      printf("\nusage: phasesrv -h\n\n");
    }

  printf("phasesrv done (switch= %d)\n", setupswitch);
  exit(0);
}
/* end */
