/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/cutils.h */
/*   Date      : <08 Apr 04 15:05:08 flechsig>  */
/*   Time-stamp: <08 May 14 16:23:43 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */

#ifndef CUTILS_H
#define CUTILS_H    

#define max(a,b) (((a) > (b)) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))

#define		FASTNULL		1.0e-15

                                              /* linux */
#define 	logfilename 		"/tmp/phaseuser.log" 
typedef struct FortranString
{   
  char *string;
  int length;
} FString; 

typedef struct ComplexStruct {
	double re,im; 
	} COMPLEX ;


char    *delversion(char *); /* entfernt Versionsnummer von VMS- Filenamen */
char    *FnameBody(char *);  /* holt Rumpf von VMS- - Filenamen            */
char    *PrependEnv(char* , char *);
void 	 beep(int); 
void	 CheckUser(char *, char *); 
int 	 CheckFileHeader(FILE *, char *, int *);   
FString *CreateFString(FString *, char *);
void     complex_in(COMPLEX *, double, double);
void     complex_minus(COMPLEX *, COMPLEX *, COMPLEX *);
void     complex_plus(COMPLEX *, COMPLEX *, COMPLEX *);
void     complex_x(COMPLEX *, COMPLEX *, COMPLEX *);
void     complex_pow(COMPLEX *, double, COMPLEX *);
int      fidx_mX4(int, int, int, int, int);
int      fexists(char *); 
double   uRandom(double), RVZ();   
   
#endif  /*  CUTILS_H */     
/* end /home/pss060/sls/flechsig/phase/src/phase/cutils.h */
