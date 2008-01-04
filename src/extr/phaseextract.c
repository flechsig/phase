/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/extr/phaseextract.c */
/*   Date      : <31 Oct 03 10:22:38 flechsig>  */
/*   Time-stamp: <04 Jan 08 16:25:27 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */


/* !!!!!!!!!!!!!!!!! bei mehrdimensionale Matritzen schreibt Fortran
   zuerst die Spalten (umgekehrt wie c )
   c: a[zeilen][spalten] for: (spalten,zeilen)
*/ 

#include <stdio.h>                    /* For printf and so on. */
#include <stdlib.h>	    	      /* needed for fopen      */
#include <string.h>
#include <math.h>
#include <time.h>

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
  double ax, ay, ax0, ay0, dy, dz, rfwhm, yfwhm, zfwhm, rpy, rpz, 
    transmittance, done, ddone;
  int 	 ix, iy;
  time_t start, l, h, m, s;
  char ch;
 
  start= time(NULL);
  PI= 4.0* atan(1.0); 

  Beamline.localalloc= DOALLOC;       /* init should go somwhere else */

#ifdef LOGFILE   
  CheckUser(logfilename, "Extraction"); 
#endif  

  if (argc < 2)
    {
      fprintf(stderr, "syntax: phaseextract <inputfilename> [calculation_methode]\n");
      exit(-1);    
    }   
  printf("read from file: %s\n", argv[1]);    
  getoptipickfile(&optistructure, argv[1]); 
  if (argc == 3)
     {
       sscanf(argv[2], "%d", &optistructure.methode);
       printf("take calculation methode from command line: %d\n", 
	      optistructure.methode);
     }

  if ((optistructure.methode > OptiRpZ) || (optistructure.methode < 0))
     {
       printf("error: %d unknown calculation methode -- set it to %d\n", 
	      optistructure.methode, OptiR);
       optistructure.methode= OptiR;
     }

  ReadBLFile(optistructure.beamlinefilename, &Beamline);
  /* get start for x, y from beamline */
  out_struct(&Beamline, &ax0, optistructure.xindex);  
  out_struct(&Beamline, &ay0,  optistructure.yindex); 
  ReAllocResult(&Beamline, PLrttype, Beamline.RTSource.raynumber, 0);
  MakeRTSource(&PHASESet, &Beamline);      /* Quelle herstellen */

  /* oeffnen des Ausgabefiles */
  if ((optistructure.filepointer= 
       fopen(optistructure.resultfilename, "w")) == NULL)
    {
      fprintf(stderr,"\aError: write %s\n", optistructure.resultfilename);
      exit(-1);
    }  
  /*  initoptidata(&optistructure);   */
  fprintf(optistructure.filepointer, 
	   "#########################################################################\n"); 
  fprintf(optistructure.filepointer, 
	  "# file name: %s\n", optistructure.resultfilename);
  fprintf(optistructure.filepointer,
	  "# beamline:  %s\n", optistructure.beamlinefilename);
  fprintf(optistructure.filepointer,
	   "# format:    nx ny columns\n");
  fprintf(optistructure.filepointer,
	   "# format:    x y rfwhm, yfwhm, zfwhm, rpy, rpz, transmittance\n");
  fprintf(optistructure.filepointer,
	  "#########################################################################\n");
  fprintf(optistructure.filepointer, "%d %d %d\n", 
	  optistructure.xpoints, optistructure.ypoints, 8); /* write header */

  ddone= 100.0/(optistructure.ypoints * optistructure.xpoints);
  done= 0;
  /* initialize start symmetric to original value */
  ay = ay0- 0.5* (optistructure.ypoints- 1)* optistructure.dy;
  ax0= ax0- 0.5* (optistructure.xpoints- 1)* optistructure.dx;
  for (iy= 0; iy< optistructure.ypoints; iy++)   
    {
      printf("************************ ay= %g **********************\n", ay);
      in_struct(&Beamline, &ay, optistructure.yindex);    
      ax= ax0;
      for (ix= 0; ix< optistructure.xpoints; ix++)   
	{
	  printf("phaseextract: ax: %g\n", ax);
	  in_struct(&Beamline, &ax, optistructure.xindex); 
	  /* extrakt */
	  buildsystem(&Beamline); 
	    switch(optistructure.methode) 
	      {
	      case OptiR:
	      case OptiY:
	      case OptiZ:
	      case OptiRpY:
	      case OptiRpZ:
		RayTracec(&Beamline);
		GetResults(&Beamline, &rfwhm, &yfwhm, &zfwhm, 
			   &rpy, &rpz, &transmittance);
		break;
	      case OptiTrans:
		/*	printf("**** Full RT ****\n"); */
		ReAllocResult(&Beamline, PLrttype, Beamline.RTSource.raynumber, 0);
		RayTraceFull(&Beamline);
		GetResults(&Beamline, &rfwhm, &yfwhm, &zfwhm, 
			   &rpy, &rpz, &transmittance);
		break;
	      case OptiFocus:
		printf("OptiFocus (methode %d) not used in phaseextract\n", optistructure.methode);
		break;
	      case OptiCost:
		Get_dydz_fromSource(&Beamline, &dy, &dz);
		costfor(&rfwhm, &Beamline.ypc1, &Beamline.zpc1, &Beamline.dypc, 
			&Beamline.dzpc, &dy, &dz, &Beamline.deltalambdafactor);
		yfwhm= zfwhm= rpy= rpz= transmittance= 0.0;
		break;
	      default:
		printf("methode %d not used in phaseextract\n", optistructure.methode);
	      }

	  /* write result */  
	  fprintf(optistructure.filepointer, "%lf %lf %le %le %le %le %le %le\n", 
		  ax, ay, rfwhm, yfwhm, zfwhm, rpy, rpz, transmittance);
	  fflush(optistructure.filepointer);
	  printf("                          ******** done: %d %s ********\n", 
		 (int)done, "%");
	  done+= ddone; 
	  ax+= optistructure.dx; 
	}  
      ay+= optistructure.dy;  
       
    }
  free(optistructure.parindex);
  fprintf(optistructure.filepointer,
	  "################################# end ###################################\n");
  fclose(optistructure.filepointer);
  beep(4);

  /* calc the time */
  l= time(NULL)- start;
  h= l/ 3600;
  m= (l- (h * 3600))/ 60;
  s= l- h * 3600- m * 60;
  printf("calculation ready in: %d:%d:%d (h:m:s)\n", h, m, s);

  printf("end extraction: results in file: %s\n", 
	 optistructure.resultfilename);
  printf("file format: ax, ay, rfwhm, yfwhm, zfwhm, rpy, rpz, transmittance\n");
  exit(1); 
}

void GetResults(struct BeamlineType *bl, double *rfwhm, double *yfwhm, double *zfwhm, 
		double *rpy, double *rpz, double *trans)
{
  char target;

  target= 'r';
  GetFWHM(bl, &target, rfwhm);
  target= 'y';
  GetFWHM(bl, &target, yfwhm);
  target= 'z';
  GetFWHM(bl, &target, zfwhm);
  if (bl->deltalambdafactor > 0.0)
    {
      *rpy= (*yfwhm > 0.0) ? bl->BLOptions.lambda/ 
	bl->deltalambdafactor/ *yfwhm : 1e12;
      *rpz= (*zfwhm > 0.0) ? bl->BLOptions.lambda/ 
	bl->deltalambdafactor/ *zfwhm : 1e12;
    } 
  else 
    {
      *rpy= *rpz= 0.0; 
    }
  *trans= (double)bl->RESULT.points/
    (double)bl->RTSource.raynumber;
  *trans= 1.0- *trans;
}

/* end main */
