/*  File      : /home/vms/flechsig/vms/phas/phasec/cutils.c */
/*  Date      : <27 Mar 97 11:01:52 flechsig>  */
/*  Time-stamp: <28 Oct 99 09:55:24 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@exp.bessy.de */
 
/* Datei: USERDISK_3:[FLECHSIG.PHASE.PHASEC]CUTILS.C           */
/* Datum: 21.JUL.1994                                          */
/* Stand: 24-APR-1998                                          */
/* Autor: FLECHSIG, BESSY Berlin                               */

#include <stdio.h>  
#include <stdlib.h>   
#include <string.h>
#include <time.h> 
#include <math.h>
#include <descrip.h>                      /* for FORTRAN- String */    

#include "cutils.h"


void beep(int n)
{
  while ((n--) > 0)
    fprintf(stderr,"%c", 7);
}

char *delversion(char *fname) /* entfernt Versionsnummer von VMS- Filenamen */ 
{
  char *semi;
  if ((semi= strchr(fname, ';')) != NULL) *semi= '\0'; 
  return fname;
}   

char *FnameBody(char *pfad)      /* holt Rumpf von VMS- - Filenamen  */
/* modification: 17 Oct 97 11:30:45 flechsig */
{
  char *ch, *ch1;

  if ((ch1= strrchr(pfad, ']')) == NULL) ch1= pfad;
  if ((ch = strchr(ch1, '.'))    != NULL)  *ch=   '\0'; 
  return pfad;
}

double uRandom(double max)         /* Zufallszahl zw 0... max */
/* musste am 24.4.98 umgenannt werden da es bei der neuen vms- Version 
   offenbar eine Funktion mit gleichem Namen gibt */ 
{
   double zz, maxrand;

   maxrand= (double)(RAND_MAX);	     /*2147483647.0;	   (1 << 31)-1; */
   zz= (double) rand() * max/(maxrand-1.0);
   return zz;
}

double RVZ()
{
   double vz;
   vz= (rand() < ((RAND_MAX) >> 1)) ? -1.0: 1.0; 
   return vz;
}    

int CheckFileHeader(FILE *f, char *header)              /* bei Gleichheit 0 */
{
  char testzeile[50];
  int rcode;
  
  fscanf(f,"%s\n", &testzeile);     
  rcode= strcmp(header, testzeile);
  if (rcode != 0)
    fprintf(stderr,"error: fileheader: %s != %s\n", testzeile, header);
  return rcode;
}

void CheckUser(char *logname, char *progname)   
{    
  char puffer[100], *p;   
  struct tm *lokal;
  time_t Times;
  FILE *f;
  
  time(&Times); lokal= localtime(&Times); p= asctime(lokal);
  cuserid(puffer); 
  if ((f= fopen(logname, "r")) == NULL)
    {
      fprintf(stderr, 
	      "error: %s - version expired or installation error \n", 
	      progname); 
      exit(-1);
    } else fclose(f);
  if ((f= fopen(logname, "a+")) != NULL)
    {
      fprintf(f,"%s\t%s\t%s", puffer, progname, p);  
      fclose(f);
    }   
}

FString *CreateFString(FString *fstring, char *cstring)  
{        /* erzeugt VMS Fortran- String */
  int l= strlen(cstring);
  fstring->dsc$b_dtype  = DSC$K_DTYPE_T;    
  fstring->dsc$b_class  = DSC$K_CLASS_S;    
  fstring->dsc$w_length = l;    
  fstring->dsc$a_pointer= cstring;    
  return (fstring);
}         

int fexists(char *path)
     /* Uwe 3.6.96 						*/
     /* testet ob eine Datei mit namen path existiert        */
     /* return: 1 wenn existiert, sonst 0                    */
{
  FILE *f;
  int ex;
  
  if ((f= fopen(path, "r")) != NULL) 
    {
      ex= 1; fclose(f);
    }  else ex= 0;
  return ex;
}
/* end cutils.c */
