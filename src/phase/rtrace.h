/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/rtrace.h */
/*  Date      : <28 Nov 06 09:06:56 flechsig>  */
/*  Time-stamp: <2023-08-09 16:09:05 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

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



#ifndef __RTRACE_LOADED
#define __RTRACE_LOADED	1         

int MakeRTSource(struct PHASEset *, struct BeamlineType *);

void RayTracec(struct BeamlineType *), 
        
     WritePlotFile(char *, int *, struct RayType *),     
     WriteRayFile (char *, int *, struct RayType *),
     WriteRayFileHdf5 (char *, int *, struct RayType *),
     MakeHardEdgeSource (struct RTSourceType *), 
     MakeUndulatorSource(struct RTSourceType *, char),  
     MakeDipolSource    (struct RTSourceType *),
     AllocRTSource(struct BeamlineType *),
     ReAllocResult(struct BeamlineType *, int, int, int),
     FreeResultMem(struct RESULTType *);
             
#ifdef LINUX
  #define ray_tracef ray_tracef_
#endif

extern void extractmap35(double *, double*, double *, double *, double*, 
			 int *);
extern void ray_tracef(struct RayType *, struct RayType *, int *, 
                       double *, double*, double *, double *);
	    
double gauss(double);
#endif
/* end rtrace.h */
