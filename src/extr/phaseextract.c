/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/extr/phaseextract.c */
/*   Date      : <31 Oct 03 10:22:38 flechsig>  */
/*   Time-stamp: <30 Apr 04 13:56:01 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */

/*



*/

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
  #include "[-.phase]phase_struct.h"
  #include "[-.phase]fg3pck.h"   
  #include "[-.phase]mirrorpck.h"                 
  #include "[-.phase]geometrypck.h"   
  #include "[-.phase]PHASE.h"
  #include "[-.opti]phaseopti.h" 
#else
  #include "../phase/cutils.h"  
  #include "../phase/phase_struct.h"
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

int main(argc, argv)
     unsigned int argc;                  /* Command line argument count. */
     char *argv[];                       /* Pointers to command line args. */
{ 
  double ax, ay, ax0, dy, dz, yfwhm, zfwhm, rpy, rpz, transmittance, ddone;
  int 	 ix, iy, done;
 
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
  /* get start for x, y from beamline */
  out_struct(&Beamline, &ax0, optistructure.xindex);  
  out_struct(&Beamline, &ay,  optistructure.yindex); 
  MakeRTSource(&PHASESet, &Beamline);      /* Quelle herstellen */

  /* oeffnen des Ausgabefiles */
  if ((optistructure.filepointer= 
       fopen(optistructure.resultfilename, "w")) == NULL)
    {
      fprintf(stderr,"\aError: write %s\n", optistructure.resultfilename);
      exit(-1);
    }  
  /*  initoptidata(&optistructure);   */
   
  fprintf(optistructure.filepointer, "%d %d\n", 
	  optistructure.xpoints, optistructure.ypoints); /* write header */

  ddone= 100.0/(optistructure.ypoints * optistructure.xpoints);
  done= 0;
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
	  
	  RayTraceFull(&Beamline);  /* rt */

	  /* determine results */
	  transmittance= (double)Beamline.RESULT.points/
			      (double)Beamline.RTSource.raynumber;

	  zfwhm= 2.35* GetRMS(&Beamline, 'z');
	  yfwhm= 2.35* GetRMS(&Beamline, 'y'); 
          
	  rpy= (yfwhm > 0.0) ? Beamline.BLOptions.lambda/ 
	    Beamline.deltalambdafactor/ yfwhm : 1e12;
	  rpz= (zfwhm > 0.0) ? Beamline.BLOptions.lambda/ 
	    Beamline.deltalambdafactor/ zfwhm : 1e12;

	  /* write result */  
	  fprintf(optistructure.filepointer, "%lf %lf %le %le %le %le %le\n", 
		  ax, ay, yfwhm, zfwhm, rpy, rpz, transmittance);
	  printf("******** done: %d \% ********\n", done);
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
  printf("file format: ax, ay, yfwhm, zfwhm, rpy, rpz, transmittance\n");
  exit(1); 
}
/* end main */
