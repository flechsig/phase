/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/baselib/DefMirrorC.c */
/*  Date      : <13 Mar 06 10:01:58 flechsig>  */
/*  Time-stamp: <13 Mar 06 10:18:58 flechsig>  */
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

/* UF add BeamlineType *bl 13.3.06 */
void DefMirrorC(struct mdatset *x, struct mirrortype *a, 
		int etype, char *fname, struct BeamlineType *bl)  
{
  double r, rho, *dp, cone, l,
    alpha, aellip, bellip, eellip, epsilon, f, xpole, ypole, 
    rpole, fipole, small, kellip, Rellip;
  int i, j;
  struct mirrortype mirror;

#ifdef DEBUG
  /*  printf("DefMirrorC: called\n");*/
#endif  
 
  small= 1e-15;   
  r    = x->rmi;
  rho  = x->rho;
  dp   = (double *)a;
  alpha= x->alpha * PI/ 180.0;

  /* UF 26. 11. 04        */
  /* ellipsen Problem     */
  /* alpha= -fabs(alpha); */ 
  /* UF 26. 11. 04 end    */

      
  for (i= 0; i< 36; i++) dp[i]= 0.0;  /* initialisieren alles 0.0 */
                               /* Radien < small dann planspiegel */

  /* index fuer a(i,j) = i+ j* 6    */

  switch (etype)
    {
    case kEOEPM:               /* plane mirror      */
    case kEOEPG:               /* plane grating     */
    case kEOEPGV:              /* plane VLS grating */
      printf("DefMirrorC: flat shape ");
      break;  /* end plane */

    case kEOESlit:
      printf("DefMirrorC: slit- geometry and element data are ignored - ");
      printf("fill dummy entries from toroid\n"); 
    case kEOEDrift:
      printf("DefMirrorC: drift- geometry and element data are ignored - ");
      printf("fill dummy entries from toroid\n"); 
    case kEOETM:                          /* index a(i,j) */
    case kEOETG:                          /* = i+ j* 6    */
    case kEOEVLSG:  
      printf("DefMirrorC: generic toroidal shape ");                 
      if (fabs(rho) > small) 
	{
	  dp[12]= 0.5/ rho;                		  /* 0,2 */
	  dp[24]= 1.0/ (8.0* rho* rho* rho);              /* 0,4 */
	}  
      if (fabs(r) > small)  
	{
	  dp[2]= 0.5/ r;   
	  dp[4]= 1.0/ (8.0* r* r* r);   
	}  
      if ((fabs(rho) > small) && (fabs(r) > small))  
	{
	  dp[14]= 1.0/(4.0* r * r* rho);                 /* 2, 2 */
	} 
      break; /* end toroid */ 

    case kEOEGeneral:           /* read coefficients from file */
      printf("DefMirrorC: read general coefficient file\n");
      ReadCoefficientFile(dp, fname);
      break;

    case kEOECone:  
      l= 100;
      printf("DefMirrorC: special conical cylinder (not tested)\n");
      printf("fixed cone length l= %f mm\n");
      printf("r, rho are the two radii\n");
      if (fabs(l) > small)
	{
	  cone= (r - rho)/ l;
	  cone*= cone;
	  dp[1]= 1.0- cone;
          dp[2]= 1.0- 2* cone;
	  dp[3]= sqrt(cone- cone* cone);
	  dp[4]= -(r/sqrt(cone)- l/2.0)* sqrt(cone- cone* cone);
	}
#ifdef DEBUG
      printf("end cone shape\n");
#endif
      break; /* end cone */

    case kEOEElli: 
      printf("DefMirrorC: elliptical shape\n");  
      if (fabs(alpha) < small) 
	{
	  beep(1);	
	  fprintf(stderr, "theta = 0, elliptical shape makes no sense!\n");
	} 
      else
	{       
	  aellip= (x->r1+ x->r2)/ 2.0;
	  bellip= sqrt(aellip* aellip- 0.25* 
		       (x->r1* x->r1+ x->r2* x->r2- 
			2.0* x->r1* x->r2* cos(2.0* alpha)));

	  /* lineare Exzentrizitaet oder brennweite e= sqrt(a^2-b^2) */
	  eellip= sqrt(aellip* aellip- bellip* bellip);

	  /* Parameter k und R in der generic cone equation */
	  /* y(x)= \frac{x^2/R}{1+\sqrt{1-(k+1)(x/R)^2}} */
	  kellip= (aellip* aellip)/(bellip* bellip) - 1.0;
	  Rellip= (aellip* aellip)/ bellip;

	  /* numerische exzentrizitaet epsilon= e/a */ 
	  epsilon= eellip/ aellip;

	  f     = (x->r1* x->r2)/ (x->r1+ x->r2);
	  xpole    = (x->r1* x->r1- x->r2* x->r2)/ (4.0* eellip);
	  ypole = sqrt(x->r1* x->r1-(eellip+ xpole)*(eellip+ xpole));
	  rpole = sqrt(xpole* xpole + ypole* ypole);
	  fipole= atan2(ypole, xpole)* 180.0/ PI;

	  printf("DefMirrorC: ell. parameter: \n");
	  printf("major axis:                   a = % f mm\n", aellip); 
	  printf("minor axis:                   b = % f mm\n", bellip);
	  printf("linear eccentricity:          e = % f mm\n", eellip);
	  printf("numerical eccentricity: epsilon = % f   \n", epsilon);

	  printf("cone parameter:               k = % f   \n", kellip);
	  printf("cone radius:                  R = % f mm\n", Rellip);

	  printf("pole:                         x = % f mm\n", xpole);
	  printf("pole:                         y = % f mm\n", ypole);
	  printf("pole:                         r = % f mm\n", rpole);
	  printf("pole:                       phi = % f deg.\n", fipole);
	  printf("f = %f\n", f);

	  dp[12]= 1.0/ (4.0* f* cos(alpha));    		/* 0,2 */
	  dp[2] = cos(alpha)/ (4.0* f);          		/* 2,0 */

	  dp[13]= (tan(alpha)* sqrt(pow(epsilon, 2.0)- pow(sin(alpha), 2.0)))/
	    (8.0* pow(f, 2.0)* cos(alpha));                     /* 1,2 */
	  /** UF 26.11.04 Vorzeichen ist vermutlich falsch    */
	  /* bei negativen alpha scheint es richtig zu sein   */
	  /* ist u(w,l) abhaengig vom Vorzeichen von alpha ?? */

	  dp[3] = (sin(alpha)* sqrt(pow(epsilon, 2.0)- pow(sin(alpha), 2.0)))/
	    (8.0* f* f);                              /* 3,0 */
	  dp[4] = (pow(bellip, 2.0)/ (64.0* pow(f, 3.0)* cos(alpha)))  * 
	    ((5.0* pow(sin(alpha), 2.0)* pow(cos(alpha),2.0))/ 
	     pow(bellip, 2.0)- (5.0* pow(sin(alpha), 2.0))/ 
	     pow(aellip, 2.0)+ 1.0/ pow(aellip, 2.0));  	/* 4,0 */ 
	  dp[14]= (pow(sin(alpha), 2.0)/ 
		   (16.0* pow(f, 3.0)* pow(cos(alpha), 3.0)))* 
	    (1.50* pow(cos(alpha), 2.0)- (pow(bellip, 2.0)/ 
					  pow(aellip, 2.0))* 
	     (1.0- 1.0/ (2.0* pow(tan(alpha), 2.0))));  	/*2,2 */
	  dp[24]= (pow(bellip, 2.0)/ 
		   (64.0* pow(f, 3.0)* pow(cos(alpha), 3.0)))* 
	    (pow(sin(alpha), 2.0)/ pow(bellip, 2.0) + 
	     1.0/ pow(aellip, 2.0));  				/* 0,4 */
	}
      break; /* end ellipsoid */

    case kEOEPElli:
    case kEOEPElliG:
      printf("DefMirrorC: plane- elliptical shape\n");  
      if (fabs(alpha) < small) 
	{
	  beep(1);	
	  fprintf(stderr, 
		  "DefMirrorC: theta = 0, elliptical shape makes no sense!\n");
	} 
      else
	{     
	  aellip= (x->r1+ x->r2)/ 2.0;
	  bellip= sqrt(aellip* aellip- 0.25* 
		       (x->r1* x->r1+ x->r2* x->r2- 
			2.0* x->r1* x->r2* cos(2.0* alpha)));
	  eellip= sqrt(aellip* aellip- bellip* bellip);

	  /* Parameter k und R in der generic cone equation */
	  /* y(x)= \frac{x^2/R}{1+\sqrt{1-(k+1)(x/R)^2}} */
	  kellip= (aellip* aellip)/(bellip* bellip) - 1.0;
	  Rellip= (aellip* aellip)/ bellip;

	  epsilon= eellip/ aellip;
	  f     = (x->r1* x->r2)/ (x->r1+ x->r2);
	  xpole = (x->r1* x->r1- x->r2* x->r2)/ (4.0* eellip);
	  ypole = sqrt(x->r1* x->r1-(eellip+ xpole)*(eellip+ xpole));
	  rpole = sqrt(xpole* xpole + ypole* ypole);
	  fipole= atan2(ypole, xpole)* 180.0/ PI;
	  
	  printf("DefMirrorC: ell. parameter: \n");
	  printf("major axis:                   a = %f mm\n", aellip); 
	  printf("minor axis:                   b = %f mm\n", bellip);
	  printf("linear eccentricity:          e = %f mm\n", eellip);
	  printf("numerical eccentricity: epsilon = %f   \n", epsilon);

	  printf("cone parameter:               k = % f   \n", kellip);
	  printf("cone radius:                  R = % f mm\n", Rellip);

	  printf("pole:                         x = %f mm\n", xpole);
	  printf("pole:                         y = %f mm\n", ypole);
	  printf("pole:                         r = %f mm\n", rpole);
	  printf("pole:                       phi = %f deg.\n", fipole);
	  printf("f = %f\n", f);
	  
	  dp[2] = cos(alpha)/ (4.0* f);          		/* 2,0 */
/** Vorzeichen vermutlich falsch UF 26.11.04 - siehe oben */
	  dp[3] = (sin(alpha)* sqrt(pow(epsilon, 2.0)- pow(sin(alpha), 2.0)))/
	    (8.0* f* f);                                        /* 3,0 */
	  dp[4] = (pow(bellip, 2.0)/ (64.0* pow(f, 3.0)* cos(alpha)))* 
	    ((5.0* pow(sin(alpha), 2.0)* pow(cos(alpha),2.0))/ 
	     pow(bellip, 2.0)- (5.0* pow(sin(alpha), 2.0))/ 
	     pow(aellip, 2.0)+ 1.0/ pow(aellip, 2.0));  	/* 4,0 */ 
	}
      break;

    default:
      fprintf(stderr, "defmirrorc: %d - unknown shape:", etype); 
      exit(-1);
    } /* end switch */ 
#ifdef DEBUG
  printf("DEBUG: mirror coefficients\n");
  for (i= 0; i < 15; i++) printf("%d %le\n", i, dp[i]);
  
#endif
  /* misalignment */
  if (bl->BLOptions.WithAlign == 1)
    {
      printf("            with misalignment\n");
      memcpy(&mirror, a, sizeof(struct mirrortype));
      misali(&mirror, a, &x->dRu, &x->dRl, &x->dRw, &x->dw, &x->dl, &x->du);
    } else
      printf("            without misalignment\n");
#ifdef DEBUG
  printf("DEBUG: mirror coefficients\n");
  for (i= 0; i < 15; i++) printf("%d %le\n", i, dp[i]);
  printf("DEBUG: end defmirrorc\n");
#endif
} /* end defmirrorc */
