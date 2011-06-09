//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/qtphase.cpp
//  Date      : <08 Jun 11 16:14:16 flechsig> 
//  Time-stamp: <09 Jun 11 11:23:07 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 


#include "qtphase.h"


// constructor
QtPhase::QtPhase()
{
  printf("QtPhase constructor called\n");
#ifdef EXPIRE
       printf(" defined expire date: %d\n", EXPIRE);
       time(&timev);
       local_time= localtime(&timev);
       /* debug       printf("%d %d %d\n\n", local_time->tm_year, local_time->tm_mon, local_time->tm_mday); */

       if (  local_time->tm_mday + 
	    (local_time->tm_mon  + 1) * 100 + 
	    (local_time->tm_year + 1900) * 10000 > EXPIRE )
	 {
	   printf("\n Program PHASE expired..., terminating\n Please, contact Johannes Bahrdt\n\n");
	   exit(1);
	 } else printf("  The program is not expired!\n");

       /*       already_expired(); */
#else
    printf(" The program does not expire!\n\n");
#endif

#ifdef SEVEN_ORDER
    printf(" PHASE version > Nov: 2010: SEVEN_ORDER defined\n\n");
#else
    printf(" PHASE version > Nov: 2010: SEVEN_ORDER undefined\n\n");
#endif

}


// end 
