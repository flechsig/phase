/*  File      : /home/vms/flechsig/vms/phas/phasec/cutils.h */
/*  Date      : <23 Apr 98 14:11:42 flechsig>  */
/*  Time-stamp: <12 Oct 98 15:57:40 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@exp.bessy.de */

/* Datei: USERDISK_3:[FLECHSIG.PHASE.PHASEC]CUTILS.H           */
/* Datum: 21.JUL.1994                                          */
/* Stand: 17-FEB-1997                                          */
/* Autor: FLECHSIG, BESSY Berlin                               */

#ifndef CUTILS_H
#define CUTILS_H    

#define max(a,b) (((a) > (b)) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))


#define		FASTNULL		1.0e-15
#define 	logfilename 		"golden\"name password\"::userdisk_3:\
					 [flechsig.PHAS]PHASEuser.log"   

typedef struct dsc$descriptor_s FString;                /* FORTRAN- String */

char    *delversion(char *); /* entfernt Versionsnummer von VMS- Filenamen */
char    *FnameBody(char *);  /* holt Rumpf von VMS- - Filenamen            */
void 	 beep(int); 
void	 CheckUser(char *, char *); 
int 	 CheckFileHeader(FILE *, char *);   
FString *CreateFString(FString *, char *); 
double   uRandom(double), RVZ();   
   
#endif  /*  CUTILS_H */     
/* end /home/vms/flechsig/vms/phas/phasec/cutils.h */
