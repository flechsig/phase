//  File      : /afs/psi.ch/user/f/flechsig/phase/src/opti_root/opti_root.cpp
//  Date      : <28 Sep 12 14:29:13 flechsig> 
//  Time-stamp: <01 Sep 14 14:25:27 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 

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


#include <iostream>

#include "opti_root_example.h"

using namespace std;

int main(int argc, char *argv[])
{
  cout << "start optimization with root" << endl;
  
  TMinuit *gMinuit = new TMinuit(5);  //initialize TMinuit with a maximum of 5 params
  gMinuit->SetFCN(fcn);
  myfill();  // fill data into global vectors
  
  Double_t arglist[10];
  Int_t    ierflg = 0;
  
  arglist[0] = 1;
  gMinuit->mnexcm("SET ERR", arglist ,1,ierflg);
  
  // Set starting values and step sizes for parameters
  static Double_t vstart[4] = {3, 1 , 0.1 , 0.01};
  static Double_t step[4]   = {0.1 , 0.1 , 0.01 , 0.001};

  gMinuit->mnparm(0, "a1", vstart[0], step[0], 0, 0, ierflg);
  gMinuit->mnparm(1, "a2", vstart[1], step[1], 0, 0, ierflg);
  gMinuit->mnparm(2, "a3", vstart[2], step[2], 0, 0, ierflg);
  gMinuit->mnparm(3, "a4", vstart[3], step[3], 0, 0, ierflg);
  
  // Now ready for minimization step
  arglist[0] = 500;
  arglist[1] = 1.;
  gMinuit->mnexcm("MIGRAD", arglist , 2, ierflg);
  
  // Print results
  Double_t amin, edm, errdef;
  Int_t    nvpar, nparx, icstat;

  gMinuit->mnstat(amin, edm, errdef, nvpar, nparx, icstat);
  //gMinuit->mnprin(3,amin);
  
  cout << "end" << endl;
}
// end main

//______________________________________________________________________________
Double_t func(float x, float y, Double_t *par)
{
 Double_t value=( (par[0]*par[0])/(x*x)-1)/ ( par[1]+par[2]*y-par[3]*y*y);
 return value;
}

//______________________________________________________________________________
void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag)
{
   const Int_t nbins = 5;
   Int_t i;

//calculate chisquare
   Double_t chisq = 0;
   Double_t delta;
   for (i= 0; i < nbins; i++) 
     {
       delta  = (z[i]- func(x[i], y[i], par))/errorz[i];
       chisq += delta*delta;
     }
   f = chisq;
}

// fill the example data
void myfill()
{
// The z values
	z[0]=1;
	z[1]=0.96;
	z[2]=0.89;
	z[3]=0.85;
	z[4]=0.78;
// The errors on z values
        Float_t error = 0.01;
	errorz[0]=error;
	errorz[1]=error;
	errorz[2]=error;
	errorz[3]=error;
	errorz[4]=error;
// the x values
	x[0]=1.5751;
	x[1]=1.5825;
	x[2]=1.6069;
	x[3]=1.6339;
	x[4]=1.6706;
// the y values
	y[0]=1.0642;
	y[1]=0.97685;
	y[2]=1.13168;
	y[3]=1.128654;
	y[4]=1.44016;
}
// end myfill   

