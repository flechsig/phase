/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/cutils.c */
/*   Date      : <25 Jun 02 08:20:05 flechsig>  */
/*   Time-stamp: <20 Nov 08 17:23:42 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */


#include <stdio.h>  
#include <stdlib.h>   
#include <string.h>
#include <time.h> 
#include <math.h>
#ifdef VMS
  #include <descrip.h>                      /* for FORTRAN- String */    
#endif
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

char *PrependEnv(char* env, char *str)
/* prepend the contents of an environment variable on a string, 
   the string will be altered!!!, if no environment variable found- 
   the original string will be returned  */
{
   char string[255], *stp;
   stp= getenv(env);
   if (stp != NULL)
     {
       strcpy(string, stp);
       strcat(string, str);
       strcpy(str, string);
     }
   return str;
} /* end PrependEnv */

/* filename without extension */
char *FnameBody(char *pfad)      

/* modification: 25 Jun 02 07:52:49 flechsig , sucht letztes Vorkommen von . 
   wegen afs */

{
  char *ch, *ch1;
#ifdef VMS 
  if ((ch1= strrchr(pfad, ']')) == NULL) ch1= pfad;
#else
  if ((ch1= strrchr(pfad, '/')) == NULL) ch1= pfad;
#endif
  if ((ch = strrchr(ch1, '.'))  != NULL) *ch= '\0'; 
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

/*
 check file header with headerstring
 return a file version (optional) FEB 2004, Format YYYYMMDD
 UF fscanf did not stop at newline- replace by fgets
*/
int CheckFileHeader(FILE *f, char *header, int *version)              /* bei Gleichheit 0 */
{
  char headerread[50];
  char headerbuffer[50];
  int rcode, myversion, headerfields;
  
  fgets(headerbuffer, 50, f);
  headerfields= sscanf(headerbuffer, "%s %d\n", &headerread, &myversion);     
  rcode= strncmp(header, headerread, strlen(header));
  if (rcode != 0)
    fprintf(stderr,"error: fileheader: %s != %s\n", headerread, header);
  /*  *version= myversion;*/

  *version= (headerfields == 2) ? myversion : 0;

#ifdef DEBUG
  printf("CheckFileHeader: \n");
  printf("     header: %s read: %s \n", header, headerread);
  printf("     headerfields: %d, version: %d\n", headerfields, *version);
#endif
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
/* erzeugt einen Fortran- String fuer VMS */
/* unter Unix funktioniert das nur falls nur ein Parameter uebergeben wird,
   der Absoft Compiler erwartet den char * an der richtigen Position
   die Laengen Parameter werden als Liste am Ende angehaengt
*/

{        
  int l= strlen(cstring);

#ifdef VMS 
  fstring->dsc$b_dtype  = DSC$K_DTYPE_T;    
  fstring->dsc$b_class  = DSC$K_CLASS_S;    
  fstring->dsc$w_length = l;    
  fstring->dsc$a_pointer= cstring; 
#else
  fstring->string= cstring;
  fstring->length= l;
#endif   
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
