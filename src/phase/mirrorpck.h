/* Datei: USERDISK_3:[FLECHSIG.PHASE.PHASEC]MIRRORPCK.H        */
/* Datum: 19.JUL.1994                                          */
/* Stand:  1-JUL-1996                                          */
/* Autor: FLECHSIG, BESSY Berlin                               */


#ifndef __MIRRORPCK_LOADED
#define __MIRRORPCK_LOADED	1 

#define 	MirrorPickFileHeader	"MirrorPickFileType"    

#define 	D0r1            10000    /* defaults in mm, degree */
#define 	D0r2            1000
#define 	D0alpha         80
#define 	D0rmi           2000
#define 	D0rho           2000   
#define 	D0iflagmi       0   

struct grating
{
    double alpha, beta, lambda, n[5], r, rho;
    int    order;
};

struct mdatset 
{
        double r1;
	double r2;
	double alpha;        
	double rmi;        
	double rho;  
        int    iflagmi;
      /*  double l1h, l2h, thetah;
        double n[5];
        int order;  */
        double w1, w2, l1, l2;
        double slopew, slopel;
};   

int  	mgetpickfile(struct mdatset *, char *); 
   
void 	mputpickfile(struct mdatset *, char *),
	minitdatset (struct mdatset *);    
                                      
#endif 

