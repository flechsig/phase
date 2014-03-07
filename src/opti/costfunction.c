/*  File      : /home/vms/flechsig/vms/phas/opti/costfunction.c */
/*  Date      : <29 Oct 97 10:10:13 flechsig>  */
/*  Time-stamp: <29 Oct 97 10:26:49 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@exp.bessy.de */


/* 
   Die Funktion erhaelt die fuer die Beamline errechneten
   Entwicklungskoeffizienten fuer y, z, dy, dz 
   der Qualitaetswert chi muss daraus berechnet, 
   und zurueckgegeben werden!
*/

typedef double MAP7TYPE [5][5][5][5]; 

double costfunction(MAP7TYPE ypc1, MAP7TYPE zpc1, 
		    MAP7TYPE dypc, MAP7TYPE dzpc)
/* modification: 29 Oct 97 10:16:50 flechsig */
{
  double chi;
  double dy, dz;      /* Divergenz */

  dy= 0.001;          /* !! Angaben in rad !! */
  dz= 0.001;

  


  return chi;
}
/* end /home/vms/flechsig/vms/phas/opti/costfunction.c */
