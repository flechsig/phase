/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/extr/phaseextract.c */
/*   Date      : <31 Oct 03 10:22:38 flechsig>  */
/*   Time-stamp: <31 Oct 03 10:27:32 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */

/*  File      : /home/vms/flechsig/vms/phas/extr.dir/phaseextract.c */
/*  Date      : <16 Oct 97 14:10:56 flechsig>  */
/*  Time-stamp: <31 Oct 03 10:22:37 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@exp.bessy.de */

/* Datei: USERDISK_3:[FLECHSIG.PHASE.EXTRACT]EXTRACT.C         */
/* Datum: 21.JUL.1994                                          */
/* Stand: 25-MAR-1996                                          */
/* Autor: FLECHSIG, BESSY Berlin                               */

/* !!!!!!!!!!!!!!!!! bei mehrdimensionale Matritzen schreibt Fortran
   zuerst die Spalten (umgekehrt wie c )
   c: a[zeilen][spalten] for: (spalten,zeilen)
*/ 

#include <stdio.h>                    /* For printf and so on. */
#include <stdlib.h>	    	      /* needed for fopen      */
#include <string.h>
#include <math.h>

#include <Xm/Text.h>                  /* fileBox               */
#include <Xm/FileSB.h>    
#include <Xm/List.h>   
#include <Xm/MessageB.h>    
#include <Xm/SelectioB.h>   
#include <Mrm/MrmAppl.h> 
#include <X11/Xlib.h>      
#include <X11/Xutil.h>      
/* DEC specific */
#ifdef VMS
  #include <descrip.h>                  /* for FORTRAN- String   */ 
  #include <DXm/DXmHelpB.h>      
  #include <DXm/DXmPrint.h>      
  #include <DXm/DXmColor.h>   
  #include <DXm/DECspecific.h>  
  #include <sys$library/DECw$Cursor.h>
#endif

#ifdef VMS
  #include "[-.phase]cutils.h"  
  #include "[-.phase]phase_struct_10.h"
  #include "[-.phase]fg3pck.h"   
  #include "[-.phase]mirrorpck.h"                 
  #include "[-.phase]geometrypck.h"   
  #include "[-.phase]PHASE.h"
  #include "[-.opti]phaseopti.h" 
#else
  #include "../phase/cutils.h"  
  #include "../phase/phase_struct_10.h"
  #include "../phase/fg3pck.h"   
  #include "../phase/mirrorpck.h"                 
  #include "../phase/geometrypck.h"   
  #include "../phase/phase.h"
  #include "../opti/phaseopti.h" 
#endif

     

/*          Der Index     

     index= (elnumber << 8) & ((mtype & 1) << 7) & (ipos & 0x7f);    
     oder
     elnumber= index >> 8;       oberes byte
     mtype   = index & 0x80;     128 wenn Spiegel
     ipos    = index & 0x7f;
     index == 0 : theta in grad;
     index == 1 : CL mit eingangsarm variabel
     index == 2 : CL mit austrittsarm variabel
     index == 3 : energy in ev (1240 / m * lambda) 
     index == 4 : Quellabstand
     index == 5 : Bildabstand
     index == 6 : linien/mm
     index == 7,8,9,10 : variable lieniendicht
     index == 11 : lambda   
     index == 12 : theta am CLRCM bei BESSY I dy= 43 mm      
     index == 13 : clvariabel mit festem r1   

     index == 128: elementparameter
     index == 256: elementnummer
     index= 128+ 1: r
     index= 128+ 11: rho
*/
                                                                   
/* Prototype */

unsigned int main(argc, argv)
     unsigned int argc;                  /* Command line argument count. */
     char *argv[];                       /* Pointers to command line args. */
{ 
  double 		ax, ay, ax0, CHI, dy, dz, yfwhm, zfwhm;
  int 			ix, iy;
 
  PI= 4.0* atan(1.0); 
#ifdef LOGFILE   
  CheckUser(logfilename, "Extraction"); 
#endif  
  if (argc != 2)
    {
      fprintf(stderr, "syntax: phaseextract <inputfilename>\n");
      exit(-1);    
    }   
  printf("read from file: %s\n", argv[1]);    
  
  getoptipickfile(&optistructure, argv[1]);   
  ReadBLFile(optistructure.beamlinefilename, &Beamline, &PHASESet);
  /* oeffnen des Ausgabefiles */
  if ((optistructure.filepointer= 
       fopen(optistructure.resultfilename, "w")) == NULL)
    {
      fprintf(stderr,"\aError: write %s\n", optistructure.resultfilename);
      exit(-1);
    }  
  /*  initoptidata(&optistructure);
   */
  out_struct(&Beamline, &ax0, optistructure.xindex);  
  /* get x, y aus */
  out_struct(&Beamline, &ay, optistructure.yindex);  /* index    */
  
  fprintf(optistructure.filepointer, "%d %d\n", 
	  optistructure.xpoints, optistructure.ypoints);  
  
  for (iy= 0; iy< optistructure.ypoints; iy++)   
    {
      in_struct(&Beamline, &ay, optistructure.yindex);    
      ax= ax0;
      for (ix= 0; ix< optistructure.xpoints; ix++)   
	{
	  printf("ax: %g\n", ax);
	  in_struct(&Beamline, &ax, optistructure.xindex); 
	  /* extrakt */
	  buildsystem(&Beamline); 
	  
	  /* orginale Variante 30.4.98 */
	  /*  Get_dydz_fromSource(&Beamline, &dy, &dz);
	  costfor(&CHI, &Beamline.ypc1, &Beamline.zpc1, &Beamline.dypc, 
		  &Beamline.dzpc, &dy, &dz, &Beamline.deltalambdafactor); */
          /* end orginale Variante 30.4.98 */
	  /* hier koennte ein volles Raytrace eingeschoben werden  */
	  MakeRTSource(&PHASESet, &Beamline); /* Quelle herstellen */
	  RayTraceFull(&Beamline);                           /* rt */
	  zfwhm= 2.35* GetRMS(&Beamline, 'z');
	  CHI = yfwhm= 2.35* GetRMS(&Beamline, 'y');                               /* vert. spot  */
	  
	  /* Uwe 29.3.99, 2.35 eingefuegt */
	  CHI *= Beamline.deltalambdafactor;         /* deltalambda */
	  if (CHI > 0.0)                             /* Aufloesung  */
	    CHI= Beamline.BLOptions.lambda/ CHI; 
	  else 
	    CHI= 1e12;
	
	  /* Ergebnisse holen und in CHI packen */
	  /* bis hier her */
	  /*
	    fprintf(optistructure.filepointer, "%lf %lf %le\n", ax, ay, CHI);
	  */
	  /* 20.4.99 */
	  fprintf(optistructure.filepointer, "%lf %lf %le %le %le\n", 
		  ax, ay, yfwhm, zfwhm, CHI);
	  ax+= optistructure.dx; 
	}  
      ay+= optistructure.dy;  
      printf("ay= %f\n", ay); 
    }
  free(optistructure.parindex);
  fclose(optistructure.filepointer);
  beep(4);
  printf("end extraction: results in file: %s\n", 
	 optistructure.resultfilename);
  exit(1); 
}
/* end main */





/* double DeltaLambda(struct optistruct *x, double dy, int index) */
/* { */
/*    double arm, cosb, n, dl; */
/*    struct mirrortype   *m; */
/*    struct geometrytype *g;   */
/*    int elnumber, mtype, ipos, ord;   */
   
/*    elnumber= index >> 8;       */
/*    g= &x->geoz[elnumber];   */
/*    arm = 1;         /* auf armlaenge normiert */ 
/*    cosb= g->cosb; */
/*    n   = g->x[0]; */
/*    ord = g->idefl; */

/*    dl  = 1.0/ (arm * n * ord) * cosb * dy; */
/*    return dl; */
/* } */
/* end phaseextract.c */
