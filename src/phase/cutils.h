/*  File      : /home/pss060/sls/flechsig/phase/src/phase/cutils.h */
/*  Date      : <28 Oct 99 12:24:49 flechsig>  */
/*  Time-stamp: <15 Nov 99 08:06:03 flechsig>  */
/*  Author    : Flechsig Uwe OVGA/203a 4535, flechsig@psi.ch */

#ifndef CUTILS_H
#define CUTILS_H    

#define max(a,b) (((a) > (b)) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))

#define		FASTNULL		1.0e-15

#ifdef VMS
   #define 	logfilename 		"golden\"name password\"::userdisk_3:\
					 [flechsig.PHAS]PHASEuser.log"   

   typedef struct dsc$descriptor_s FString;         /* FORTRAN- String */
#else                   /* linux */
   #define 	logfilename 		"/tmp/phaseuser.log" 
   typedef      char FString; 
#endif                  /*  VMS  */

char    *delversion(char *); /* entfernt Versionsnummer von VMS- Filenamen */
char    *FnameBody(char *);  /* holt Rumpf von VMS- - Filenamen            */
char    *PrependEnv(char* , char *);
void 	 beep(int); 
void	 CheckUser(char *, char *); 
int 	 CheckFileHeader(FILE *, char *);   
FString *CreateFString(FString *, char *); 
double   uRandom(double), RVZ();   
   
#endif  /*  CUTILS_H */     
/* end /home/vms/flechsig/vms/phas/phasec/cutils.h */
