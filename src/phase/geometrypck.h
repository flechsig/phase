/*  File      : /home/vms/flechsig/vms/phas/phasec/geometrypck.h */
/*  Date      : <21 Mar 97 15:22:51 flechsig>  */
/*  Time-stamp: <16 Feb 04 15:15:06 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@exp.bessy.de */

/* Datei: USERDISK_3:[FLECHSIG.PHASE.PHASEC]GEOMETRYPCK.H      */
/* Datum: 19.JUL.1994                                          */
/* Stand: 11-JUN-1996                                          */
/* Autor: FLECHSIG, BESSY Berlin                               */


#ifndef __GEOMETRYPCK_LOADED
#define __GEOMETRYPCK_LOADED	1  

#define 	GeometryPickFileHeader	"GeometryPickFileType"    

#define 	D0theta0        88   /* defaults in mm, degree, eV */
#define 	D0r             10000
#define 	D0rp            1000
#define 	D0xdens         0 
#define		D0lambda	0
#define 	D0inout		1
#define 	D0iflag		0
#define		D0azimut	0
                                      

struct gdatset 
{
        double theta0;
	double r;
	double rp; 
       	double xdens[5];  
        double lambda; 
        int    inout;                                 
	int    iflag; 
        int    azimut;     /* vertikal 0; nach links 1; nach unten 2 */
};   

int  	ggetpickfile(struct gdatset *, char *); 
   
void 	gputpickfile(struct gdatset *, char *),
 	ginitdatset (struct gdatset *);    
                                      
#endif        
