 /* File      : /afs/psi.ch/project/phase/src/phase/heighterror.c */
 /* Date      : <05 May 14 14:12:11 flechsig>  */
 /* Time-stamp: <03 Jun 14 09:48:28 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */


#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include "hdf5.h"
#include "myhdf5.h"

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "cutils.h"
#include "phase_struct.h"
#include "phase.h"
#include "posrc.h"
#include "rtrace.h"
#include "common.h"
#include "heighterror.h"

// called from fortran in phase_source.F
// function: aperture check and height error phase shift
// uf rewrite to amplitude and phase for debugging and consistency (compl. math)
void apply_height_error_(int *blp, double *wwert, double *lwert, 
			 double *eyre, double *eyim, double *ezre, double *ezim)
{
  double u_interp, phaseshift;
  double yamp, ypha, zamp, zpha;
  struct BeamlineType *bl;
  struct ElementType  *el;
  struct SurfaceType  *sf;
    
  // lvars for local copies
  double lambda, cosa, cosb;
  
#ifdef DEBUG1
  printf("\ndebug: apply_height_error_ called! file: %s\n", __FILE__);
#endif
  
   bl= (struct BeamlineType *)blp;
   el= &bl->ElementList[0];
   sf= &el->surf;
   
   // check w, l, set field outside to zero
   // increase the number of lost principle rays
   if ( (*wwert < el->MDat.w1) || (*wwert > el->MDat.w2) || 
        (*lwert < el->MDat.l1) || (*lwert > el->MDat.l2) ) 
     {
       ((struct PSDType *)bl->RESULT.RESp)->outside_wl++;  // increase lost "rays"
       *eyre= *eyim= *ezre= *ezim= 0.0;
       return;
     }

   // return if height error is switched off
   if ( !bl->BLOptions.PSO.with_herror )
    {
#ifdef DEBUG1
      printf("debug: %s slope errors calculation switched off - return\n", __FILE__);
#endif
      return;
    }
  
   // calc ampl and phase from re and im
   yamp= sqrt(pow(*eyre, 2.0) + pow(*eyim, 2.0));
   zamp= sqrt(pow(*ezre, 2.0) + pow(*ezim, 2.0));
   ypha= atan2(*eyim, *eyre);
   zpha= atan2(*ezim, *ezre);

   lambda = bl->BLOptions.lambda; // in [mm]
   cosa   = el->geo.cosa;
   cosb   = el->geo.cosb;
  
#ifdef DEBUG1
  printf("debug: apply_height_error_ received values of wwert, lwert: %g, %g\n", 
	 *wwert, *lwert);
#endif

  // get interpolated height depending on w and l 
  // should work for 1d and 2d maps
  surf_height_interp(sf, wwert, lwert, &u_interp);

  // apply the phase shift=  -k * u * (sin(alpha_g) + sin(beta_g)) ???  
  // uf do we really need fabs(cos()) ???
  phaseshift= -2* PI/ lambda* u_interp* (1.0/fabs(cosa)+ 1.0/fabs(cosb));

  // apply phase shift
  ypha+= phaseshift;
  zpha+= phaseshift;

  // back to re and im
  *eyre= yamp* cos(ypha);
  *ezre= zamp* cos(zpha);
  *eyim= yamp* sin(ypha);
  *ezim= zamp* sin(zpha);

} // end apply_height_error_

// interpolate the height depending on w and l
// function should work for 1d and 2d maps
void surf_height_interp(struct SurfaceType *sf, double *wwert, double *lwert, double *u_interp)
{
  double *wvecp, *lvecp, *uvecp;
  int    nw, nl, nu;  
  int    i, j;
  int    iw1, iw2, il1, il2, diw, dil, diwl;
  double factor1, factor2, factor3, factor4; 
  double w1, w2, l1, l2, u1, u2, u3, u4, dw, dl, dwl;
    
#ifdef DEBUG1
  printf("debug: surf_height_interp received values of wwert, lwert: %g, %g\n", *wwert, *lwert);
#endif

  // copy the variables locally
  wvecp = sf->w;
  nw    = sf->nw; 
  lvecp = sf->l;
  nl    = sf->nl; 
  uvecp = sf->u;
  nu    = nl* nw;

#ifdef DEBUG1  
  printf("debug: surf_height_interp called! file: %s\n", __FILE__);
#endif 
  
  // start interpolation
  // search closest values to wwert and lwert in the vectors for w, l and u
  
  iw1= iw2= il1= il2= 0; // initialize the index

  // added some test to avoid memory overrun
  while ( *wwert > wvecp[iw2] ) 
    if (iw2 < (nw- 1)) iw2++; else break;  // break ends the loop

  while ( *lwert > lvecp[il2] ) 
    if (il2 < (nl- 1)) il2++; else break;  // break ends the loop

  if (iw2 > 0) iw1= iw2- 1;
  if (il2 > 0) il1= il2- 1;

  // index are now determined !! iw1== iw2 is allowed, same for l !!
  // fill variables
  w1 = wvecp[iw1];
  w2 = wvecp[iw2];
  l1 = lvecp[il1];
  l2 = lvecp[il2];
  u1 = uvecp[il1* nw + iw1];	// u1 = u(w1,l1)
  u2 = uvecp[il1* nw + iw2];	// u2 = u(w2,l1)
  u3 = uvecp[il2* nw + iw1];	// u3 = u(w1,l2)
  u4 = uvecp[il2* nw + iw2];	// u4 = u(w2,l2)
  dw = w2- w1;
  dl = l2- l1;
  dwl= dw * dl;
  diw= iw2- iw1;
  dil= il2- il1;
  diwl= diw* dil;

  // calculation of weight factors case dependent
  *u_interp= u1;        // default value for return
  if (diwl && (fabs(dwl) > ZERO)) // 2d case
    {
      factor1 = (*wwert- w1)* (*lwert- l1) / dwl;
      factor2 = (w2- *wwert)* (*lwert- l1) / dwl;
      factor3 = (*wwert- w1)* (l2- *lwert) / dwl;
      factor4 = (w2- *wwert)* (l2- *lwert) / dwl;
      *u_interp = factor1* u4 + factor2* u3 + factor3* u2 + factor4* u1; // weighted sum 
    } else        // 1d case     
    {
      if (diw && !dil && (fabs(dw) > ZERO)) // 1d in w
	{
	  factor3 = (*wwert- w1) / dw;
	  factor4 = (w2- *wwert) / dw;
	  *u_interp = factor3* u2 + factor4* u1; // weighted sum 
	} else
	{
	  if (dil && !diw && (fabs(dl) > ZERO)) // 1d in l
	    {
	      factor2 = (*lwert- l1) / dl;
	      factor4 = (l2- *lwert) / dl;
	      *u_interp = factor2* u3 + factor4* u1; // weighted sum 
	    } // no else since we have a default
	} // end 1d in l
    }
  // end of the interpolation
  
#ifdef DEBUG1
  printf("i, j, p  = %d, %d, %d\n", iw1, il1, il1* nw + iw1);
  printf("w1 , l1, u1 = %g, %g, %.4g\n", w1 , l1, u1);
  printf("w2 , l1, u2 = %g, %g, %.4g\n", w2 , l1, u2);
  printf("w1 , l2, u3 = %g, %g, %.4g\n", w1 , l2, u3);
  printf("w2 , l2, u4 = %g, %g, %.4g\n", w2 , l2, u4);
  printf("fact1 = %g\n", factor1);
  printf("fact2 = %g\n", factor2);
  printf("fact3 = %g\n", factor3);
  printf("fact4 = %g\n", factor4);
  printf("u_interp = %.4g\n", *u_interp);
#endif
  
#ifdef DEBUG1
  printf("debug: %s, surf_height_interp end\n", __FILE__);
#endif
} /* surf_height_interp */

// read file data into element struct
void read_hdf5_height_file(char *fname, struct ElementType *elmp)
{
  hid_t  file_id;           /* group_id       */
  int    nw, nl, nu, i, j;  /* slicecount= 1, */
  struct SurfaceType *sf;
  char   buffer[255];

#ifdef DEBUG  
  printf("debug: read_hdf5_height_file called! file: %s\n", __FILE__);
#endif 

  sf = &elmp->surf;
  
  // clean up
  if (sf->u) XFREE(sf->u);
  if (sf->w) XFREE(sf->w);
  if (sf->l) XFREE(sf->l);
  sf->nw= sf->nl= 0;
  
  if ( !fexists(fname) )
    { 
      beep(5);
      fprintf(stderr, "warning: surface error file >>%s<< not found - return\n", fname);
      return;
    }

  if ( !check_hdf5_4_height(fname, elmp->elementname, 1) ) return;

#ifdef HAVE_HDF5
 /* Open an existing file. */
  file_id = myH5Fopen(fname);

  snprintf(buffer, 254, "/%s/wvec", elmp->elementname);
  nw = getDatasetSize(file_id, buffer);
  snprintf(buffer, 254, "/%s/lvec", elmp->elementname);
  nl = getDatasetSize(file_id, buffer);
#endif
  nu = nw*nl;
  
//   double buffer_w[nw];
//   double buffer_l[nl];
//   double buffer_u[nu];
  
  if (nu != nw*nl)
  {
    printf("stderr: read_hdf5_height_file: error on the vector sizes of the surface profile %s\n", __FILE__);
    printf("stderr: read_hdf5_height_file: surface file %s\n", fname);
  }
    

#ifdef DEBUG1  
  printf("debug: read_hdf5_height_file: number of points wvec= %d, lvec= %d\n", nw, nl);;
  printf("debug: read_hdf5_height_file: number of height points %d\n", nu);
#endif 
  

  sf->w = XMALLOC(double, nw); // allocate memory
  sf->l = XMALLOC(double, nl);
  sf->u = XMALLOC(double, nu);

  // load the values to the beamline->element->surface object WG
  snprintf(buffer, 254, "/%s/height_vec", elmp->elementname);
#ifdef HAVE_HDF5
  readDataDouble(file_id, buffer, sf->u, nu); 
  snprintf(buffer, 254, "/%s/wvec", elmp->elementname);
  readDataDouble(file_id, buffer, sf->w, nw);
  snprintf(buffer, 254, "/%s/lvec", elmp->elementname);
  readDataDouble(file_id, buffer, sf->l, nl);
#endif  
  sf->nw= nw;
  sf->nl= nl;
  
  for (i= 0; i < nw; i++) sf->w[i] = sf->w[i]* 1e3;
  for (i= 0; i < nl; i++) sf->l[i] = sf->l[i]* 1e3;
  for (i= 0; i < nu; i++) sf->u[i] = sf->u[i]* 1e3;
  
  
//   *sf->u=buffer_u; //convertion from m (hdf5) to mm (phase standard)
//   *sf->w=buffer_w;
//   *sf->l=buffer_l;

#ifdef HAVE_HDF5  
  H5Fclose(file_id);
#endif
  
  printf("read_hdf5_height_file: %s => done\n", fname);
  

// #ifdef DEBUG1  
//   for (i= 0; i < nw - 1; i = i + 1)
//   {
//       for (j= 0; j < nl - 1; j= j + 1)
// 	{
// 	  printf("(i, j, p) = %d, %d, %d\n", i, j, j*nw+i);
// 	  printf("(w, l, u) = %g, %g, %g\n", sf->w[i], sf->l[j], sf->u[j*nw+i]);
// 	}
//   }
// #endif 
  
}  /* read_hdf5_file */

/* returns true if mirror surface type has been detected */
int check_hdf5_4_height(char *fname, char *mname, int verbose)
{
  int myreturn;

#ifdef HAVE_HDF5
  hid_t file_id;
  
#ifdef DEBUG
  printf("debug: file %s => check type of file %s\n", __FILE__, fname);
#endif

  file_id= myH5Fopen(fname);
 
  myreturn= (    check4Field(file_id, mname, "wvec")
	      && check4Field(file_id, mname, "lvec")
	      && check4Field(file_id, mname, "height_vec") );
  
  if (!myreturn)
    {
      fprintf(stderr, "file %s does not contain surface data for element >>%s<<\n", fname, mname);
    }
    
  H5Fclose(file_id);

  if (verbose && (myreturn == 0)) printf("file %s has not the expected type\n", fname);

#ifdef DEBUG
  printf("debug: file %s check type of file returns %d\n", __FILE__, myreturn);
#endif

#endif
  return myreturn;
}  /* check_hdf5_4_height */
// end /afs/psi.ch/project/phase/src/phase/heighterror.c
