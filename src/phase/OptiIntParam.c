/* File      : /afs/psi.ch/project/phase/GIT/phase/src/phase/OptiIntParam.c */
/* Date      : <15 Dec 14 11:59:33 flechsig>  */
/* Time-stamp: <15 Dec 14 14:07:40 flechsig>  */
/* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

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
#include <math.h>

#include "cutils.h"
#include "phase_struct.h"
#include "phase.h"

void FindIntRange(struct BeamlineType *bl)
{
//  int fr_mat[2][2]; // matrix for utilisation results
  double yedge = 1.0E-3;   // start values
  double zedge = 1.0E-3;
  double ff;
  int i, n, nonzeros, imodus;  
  int ianzy0,ianzz0;
  double y1, y2, z1, z2;
  //struct PSDType *psdp;  
  struct PSImageType *psip;
  struct PSImageType backup_psip; 
  
#ifdef DEBUG  
  printf("DEBUG: enter [%s]:FindIntRange()\n", __FILE__);
#endif 

  // backup input struct in case we can't obtain reasonable results
  psip = (struct PSImageType *)bl->RTSource.Quellep;
  memcpy(&backup_psip, psip, sizeof(struct PSImageType));
  
  // use only one gridpoint and central ray for test procedure
  psip->ymin = psip->ymax = psip->zmin = psip->zmax = 0.0;  
  psip->iy = psip->iz = 1;

  ianzy0 = bl->BLOptions.xi.ianzy0;
  ianzz0 = bl->BLOptions.xi.ianzz0;
  
  imodus = 2;
    
  if(imodus == 1)
    {
      // simple loop approach (fixed step size)
      for (i=0; i<100; i++)
	{
	  nonzeros = 0;
	  
	  y1 = -yedge+ i* 1E-4;
	  y2 = -y1;
	  
	  bl->BLOptions.xi.ymin = y1;
	  bl->BLOptions.xi.ymax = y2;
	  
	  PST(bl);
	  
	  
	  // pointer to simpsons data and vector length
	  //psdp = (struct PSDType *)bl->RESULT.RESp;
	  
	  // determine unused (near zero) percentage of result   
	  for (n=0; n<ianzy0; n++) 
	    {
	      double c2y = bl->simpre[8* n+ 5];
	      if (fabs(c2y) >= 1E-10) nonzeros++;
	    }
	  printf("Autorange: pass %d, utilisation: %d\n", i, nonzeros);
	  
	  if (nonzeros == ianzy0) break;           
	} // for loop 
    } // imodus = 1
  
  if(imodus == 2)
    {
      // more effective strategy (variable step size)
      
      nonzeros = ianzy0;
      //   y1 = -yedge/2.;
      //   y2 = -y1; 
      
      y1= bl->BLOptions.xi.ymin;
      y2= bl->BLOptions.xi.ymax; 
      
      while (nonzeros == ianzy0)  // while 1
	{
	  // printf("Autorange ya: ymin %d, ymax  %d, nonzeros: %d\n", y1, y2, nonzeros);  
	  y1=y1*2.;
	  y2=y2*2.;
	  
	  bl->BLOptions.xi.ymin = y1;
	  bl->BLOptions.xi.ymax = y2;
	  
	  PST(bl);
	  
	  // determine unused (near zero) percentage of result
	  // pointer to simpsons data and vector length
	  //psdp = (struct PSDType *)bl->RESULT.RESp;
	  nonzeros=0;
	  for (n=0; n<ianzy0; n++) 
	    {
	      double c2y = bl->simpre[8*n+5];
	      if (fabs(c2y) >= 1E-10) nonzeros++;
	    }
	  
	  printf("===== Autorange ya =======: ymin %e, ymax %e, nonzeros: %d\n", y1, y2, nonzeros);  
	  
          
	}  // end while 1
      
      while (nonzeros*2 < ianzy0) // while 2
	{
	  y1=y1/2.;
	  y2=y2/2.;
	  
	  bl->BLOptions.xi.ymin = y1;
	  bl->BLOptions.xi.ymax = y2;
	  
	  PST(bl);
	  
	  // determine unused (near zero) percentage of result
	  // pointer to simpsons data and vector length
	  //psdp = (struct PSDType *)bl->RESULT.RESp;
	  nonzeros=0;
	  for (n=0; n<ianzy0; n++) 
	    {
	      double c2y = bl->simpre[8*n+5];
	      if (fabs(c2y) >= 1E-10) nonzeros++;
	    }
	  printf("===== Autorange yb =======: ymin %e, ymax %e, nonzeros: %d\n", y1, y2, nonzeros);     
	  
	}  // end while 2
      
      ff=1.1* ((double) nonzeros / (double) ianzy0);
      y1=y1*ff;
      y2=y2*ff;  
      bl->BLOptions.xi.ymin = y1;
      bl->BLOptions.xi.ymax = y2; 
      
    } // end modus = 2
  
  if(imodus == 1)
    {
      // loop for dz
      // dz should be small enough so that plots contains only zeroes,
      // as the plots correspond to left/right-most edge 
      for (i=0; i<100; i++)
	{
	  int nonzeros_l = 0;
	  int nonzeros_r = 0;
	  int nonzeros_i = 0;
	  
	  z1 = -zedge+i*1E-4;
	  z2 = -z1;
	  
	  bl->BLOptions.xi.zmin = z1;
	  bl->BLOptions.xi.zmax = z2;
	  
	  PST(bl);
	  
	  // determine unused (near zero) percentage of result
	  // pointer to simpsons data and vector length
	  //psdp = (struct PSDType *)bl->RESULT.RESp;
	  for (n=0; n<ianzy0; n++) 
	    {
	      double c1y = bl->simpre[8*n+4]; // upper left
	      double c3y = bl->simpre[8*n+6]; // lower left
	      double c4y = bl->simpre[8*n+7]; // lower right (yellow)
	      
	      if (fabs(c1y) >= 1E-10) nonzeros_l++;
	      if (fabs(c3y) >= 1E-10) nonzeros_r++;
	      if (fabs(c3y) >= 1E-10) nonzeros_i++;
	    }
	  
	  printf("Autorange z: pass %d, utilisation: %d, %d, %d\n", i, nonzeros_l, nonzeros_r, nonzeros_i);
	  
	  if ((nonzeros_l == 0) && (nonzeros_r == 0) && (nonzeros_i == ianzy0)) break;      
	  if ((nonzeros_l > 0) || (nonzeros_r > 0)) {z1 -= 1E-4; z2 = -z1; break;};          
	}  
      
      bl->BLOptions.xi.zmin = z1;
      bl->BLOptions.xi.zmax = z2;
      
    } // end imodus = 1
  
  
  if(imodus == 2)
    {
      // more effective strategy (variable step size)
      
      nonzeros = ianzz0;
      
      z1= bl->BLOptions.xi.zmin;
      z2= bl->BLOptions.xi.zmax; 
      
      while (nonzeros == ianzz0)  // while 1
	{
	  // printf("Autorange ya: ymin %d, ymax  %d, nonzeros: %d\n", y1, y2, nonzeros);  
	  z1=z1*2.;
	  z2=z2*2.;
	  
	  bl->BLOptions.xi.zmin = z1;
	  bl->BLOptions.xi.zmax = z2;
	  
	  PST(bl);
	  
	  // determine unused (near zero) percentage of result
	  // pointer to simpsons data and vector length
	  //psdp = (struct PSDType *)bl->RESULT.RESp;
	  nonzeros=0;
	  for (n=0; n<ianzz0; n++) 
	    {
	      double c4y = bl->simpre[8*n+7];
	      if (fabs(c4y) >= 1E-10) nonzeros++;
	    }
	  
	  printf("===== Autorange za =======: zmin %e, zmax %e, nonzeros: %d\n", z1, z2, nonzeros);  
	  
          
	}  // end while 1
      
      while (nonzeros*2 < ianzz0) // while 2
	{
	  z1=z1/2.;
	  z2=z2/2.;
	  
	  bl->BLOptions.xi.zmin = z1;
	  bl->BLOptions.xi.zmax = z2;
	  
	  PST(bl);
	  
	  // determine unused (near zero) percentage of result
	  // pointer to simpsons data and vector length
	  //psdp = (struct PSDType *)bl->RESULT.RESp;
	  nonzeros=0;
	  for (n=0; n<ianzz0; n++) 
	    {
	      double c4y = bl->simpre[8*n+7];
	      if (fabs(c4y) >= 1E-10) nonzeros++;
	    }
	  printf("===== Autorange zb =======: zmin %e, zmax %e, nonzeros: %d\n", z1, z2, nonzeros);     
	  
	}  // end while 2
      
      ff=1.1* ((double) nonzeros / (double) ianzz0);
      z1=z1*ff;
      z2=z2*ff;  
      bl->BLOptions.xi.zmin = z1;
      bl->BLOptions.xi.zmax = z2;
      
    } // end modus = 2
  
  printf("Autorange final: best set found: (%.2f, %.2f, %.2f, %.2f)mrad\n", y1*1E3, y2*1E3, z1*1E3, z2*1E3);
  
  //restoring range struct
  memcpy(psip, &backup_psip, sizeof(struct PSImageType));
}

// end of file
