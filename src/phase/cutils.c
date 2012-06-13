/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/cutils.c */
/*   Date      : <25 Jun 02 08:20:05 flechsig>  */
/*   Time-stamp: <06 Jun 12 17:18:07 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif 

#include <stdio.h>  
#include <stdlib.h>   
#include <string.h>
#include <time.h> 
#include <math.h>

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
   the original string will be returned  
   !! we assume the buffer of str is at least 255 char long !!
*/
{
   char string[255], *stp;
   int lstr, lstp;

   stp= getenv(env);
   lstp= strlen(stp); /* len without '\0' */
   lstr= strlen(str);

   if ((stp != NULL) && (lstp < 255))
     {
       strncpy(string, stp, (255- lstp));
       strncat(string, str, (255- lstp- 1));
       strncpy(str, string, 255);
     }
   return str;
} /* end PrependEnv */

/* filename without extension */
char *FnameBody(char *pfad)      
/* a) sucht von rechts /   */
/* b) replace . with \0    */
/* !! modifiziert pfad!    */
{
  char *ch, *ch1;

  if ((ch1= strrchr(pfad, '/')) == NULL) ch1= pfad;
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
  
  fgets(headerbuffer, 50, f);                                         /* read one line */
  headerfields= sscanf(headerbuffer, "%s %d\n", headerread, &myversion); 
  rcode= strncmp(header, headerread, strlen(header));
  if (rcode != 0)
    fprintf(stderr, "error: fileheader: %s != %s\n", headerread, header);
 
  *version= (headerfields == 2) ? myversion : 0;

#ifdef DEBUG
  printf("CheckFileHeader: \n");
  printf("     header: %s read: %s \n", header, headerread);
  printf("     headerfields: %d, version: %d\n", headerfields, *version);
#endif

  return rcode;
} /* CheckFileHeader */

void CheckUser(char *logname, char *progname)   
{    
  char puffer[100], *p;   
  struct tm *lokal;
  time_t Times;
  FILE *f;
  
  time(&Times); lokal= localtime(&Times); p= asctime(lokal);
#ifndef QTGUI
  cuserid(puffer); 
#endif
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

  fstring->string= cstring;
  fstring->length= l;
   
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

/* calculates the index of a mX4 fortran- array */
/* i,j,k,l is the index, m the dimension        */
int fidx_mX4(int i, int  j, int k, int l, int m)
{
  int idx;

  idx= i+ m* (j+ m* (k+ m* l));
  return idx;
} /* end fidx_mX4 */

void complex_in(COMPLEX *a, double re, double im)
{
  a->re= re;
  a->im= im;
} /* complex_in */

/* c= a-b */
void complex_minus(COMPLEX *a, COMPLEX *b, COMPLEX *c)
{
  c->re= a->re- b->re;
  c->im= a->im- b->im;
} /* end complex_minus */

/* c= a+b */
void complex_plus(COMPLEX *a, COMPLEX *b, COMPLEX *c)
{
  c->re= a->re+ b->re;
  c->im= a->im+ b->im;
} /* end complex_plus */

/* c= a*b */
void complex_x(COMPLEX *a, COMPLEX *b, COMPLEX *c)
{
  c->re= a->re* b->re- a->im* b->im;
  c->im= a->re* b->im+ a->im* b->re;
} /* end complex_x */
/* end cutils.c */