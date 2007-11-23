#include "../phase/common.h"
#include "../phase/mirrorpck.h"
#include "../phase/geometrypck.h"
#include "../phase/phase_struct.h"


#include "phabasedefs.h"
#include "phabasestructs.h"

void DefGeometryC(struct gdatset *x, struct geometrytype *gout)  
     /* Uwe 25.6.96 							*/
     /* umgeschrieben - keine fileausgabe mehr 			        */
     /* datenstruktur soll gleich sin und cosinus werte enthalten 	*/
    /* modification: 19 Feb 98 11:07:44 flechsig Vorzeichenfehler alpha, beta */
{
  double delta, alpha, beta, theta0, trans, radius;
  int i;

  theta0= fabs(x->theta0* PI/ 180.0);   
  delta= (double)(x->inout)* asin(x->lambda* x->xdens[0]/(2.0* cos(theta0)));
  alpha= (-theta0- delta);   /* eigentlich fi+ theta */
  beta = ( theta0- delta);   /* nicht eher fi- theta???*/
/* modification: 17 Feb 98 09:33:48 flechsig */
/* modification: 19 Feb 98 11:08:59 flechsig */
/*   alpha= (theta0+ delta); */
/*   beta = (delta- theta0); */
  printf("DefGeometryC: alpha: %f, beta: %f, lambda= %g nm ==> correct?\n", 
	 alpha* 180.0/ PI, beta* 180.0/ PI, x->lambda* 1e6);
  if ((x->iflag) == 1)
    {
      radius   = (2.0* x->r* x->rp)/ ((x->r+ x->rp)* cos(theta0));   
      trans    = radius* (1.0- cos(delta));      
      gout->r  = x->r-  trans; 
      gout->rp = x->rp- trans;    
      printf("DefGeometryC: NIM translation enabled, trans= %d mm\nr1= %d mm, r2= %d mm\n", 
             trans, gout->r, gout->rp);  
    }  else 
      {
	gout->r  = x->r; 
	gout->rp = x->rp;    
      }

  gout->sina= sin(alpha);   
  gout->cosa= cos(alpha);   
  gout->sinb= sin(beta);   
  gout->cosb= cos(beta);   
  for (i= 0; i< 5; i++) 
    gout->x[i]= x->xdens[i]; 
  gout->xlam = x->lambda* (double)(x->inout);  
  gout->idefl= (x->theta0 > 0.0) ? 1 : -1;  
} /* end DefGeometryC */
