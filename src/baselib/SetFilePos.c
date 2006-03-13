/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/baselib/SetFilePos.c */
/*  Date      : <13 Mar 06 08:26:57 flechsig>  */
/*  Time-stamp: <13 Mar 06 08:35:30 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#include <stdio.h>                              /* For printf and so on */
#include "phabasedefs.h"


int SetFilePos(FILE *f, char *s)   
/**************************************************************************/
/* setzt den dateizeiger auf eine zeile nach "string" , datei offen
/* bei fehler 0 							  */    
/* Uwe 30.5.96 								  */
/* last change: 13.3.06 						  */ 
/**************************************************************************/
{
  int  rcode, notfound, len;
  char buffer[MaxPathLength];

  len= strlen(s);
  notfound= 1;
  while (!feof(f) && notfound)    
  {
    fgets(buffer, MaxPathLength, f); 
    /* printf("SetFilePos, search: >> %s << - found: >> %s ", s, buffer); */
    notfound= strncmp(buffer, s, len);     /* notfound=0 dann gefunden */
  }          
  if (feof(f) && notfound)
  {
     printf("SetFilePos: rewind and search for >> %s << from top\n", s);  
     rewind(f); /* und noch mal von vorn*/
     while (!feof(f) && notfound)    
     {
       fgets(buffer, MaxPathLength, f); printf("2: %s", buffer);
       notfound= strncmp(buffer, s, len);     /* notfound=0 dann gefunden */
     }  
     printf("SetFilePos: %s not found -> return\n", s);  
  }
  rcode= (notfound == 0) ? 1 : 0;
  return rcode;
} /* end SetFilePos */
