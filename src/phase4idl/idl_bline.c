/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase4idl/idl_bline.c */
/*  Date      : <31 Aug 11 16:16:00 flechsig>  */
/*  Time-stamp: <12 Aug 13 17:49:41 flechsig>  */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif 

#include <stdio.h>                              /* For printf and so on */
#include <stdlib.h> 	      	    	    	/* needed for fopen     */  
#include <string.h>                           
#include <math.h> 


#include "../phase/cutils.h"   
#include "../phase/phase_struct.h"
  
#include "../phase/phase.h"
#include "../phase/rtrace.h"                 
#include "../phase/common.h" 

#include <idl_export.h>

#include "phase4idl.h"

#include "Constants.h"

/*
 typedef struct {  
  IDL_STRING_SLEN_T slen;  
  unsigned short stype;  
  char *s;  
} IDL_STRING;  
*/



// **************************************************************************************

// int pha4idlWriteBLFile(char *fname, struct BeamlineType *bl)
int pha4idlWriteBLFile(IDL_STRING *name, struct pha4idlBeamlineFile *bl)
{
/**************************************************************************/
/* schreibt den datensatz auf ein file 		                          */
/* written: Uwe 29.5.96				                          */
/* UF 28.11.06 unions durch pointer ersetzt                               */
/* TL 20.02.2008 : es wird immer quelltyp I geschrieben, phase-space-img  */
/*                 ggfalls werden unbekannte mit 0 belegt                 */
/**************************************************************************/
{
   char *fname = name->s;
   FILE *f;
   int elnumber, i, version= 20040217;

   if ((f= fopen(fname, "w")) == NULL)
   {
      fprintf(stderr, "fatal Error: write %s\n", fname);
      exit(-1);
   } 
#ifdef DEBUG   
   printf("WriteBLFile: write data to %s ", fname);
#endif

   fprintf(f, "%s %d\n", Fg3PickFileHeader, version); // einige Infos ins file //
   fprintf(f, "This is a datafile of PHASE version FEB 04\n\n");


   fprintf(f, "SOURCE\n");
// 
/*
   switch( bl->SourceType )
     {
     case 'I': 
//       psip= (struct PSImageType *)bl->RTSource.Quellep;
       fprintf(f, "%20c    *** Phase Space Transformation Image***\n", bl->SourceType);
       fprintf(f, "%20lg    ymin\n", bl->PSImage.ymin);
       fprintf(f, "%20lg    ymax\n", bl->PSImage.ymax); 
       fprintf(f, "%20lg    zmin\n", bl->PSImage.zmin);
       fprintf(f, "%20lg    zmax\n", bl->PSImage.zmax); 
       fprintf(f, "%20d    y points\n", bl->PSImage.iy);
       fprintf(f, "%20d    z points\n", bl->PSImage.iz); 
     break; 

     case 'U': 
     case 'u':
//       up= (struct UndulatorSourceType *)bl->RTSource.Quellep;  
       fprintf(f, "%20c    ***Undulator Source for Ray Tracing***\n", bl->SourceType);
       fprintf(f, "%20lg    Undulator length     (mm)\n", bl->UndulatorSrc.length);
       fprintf(f, "%20lg    Undulator wavelength (nm)\n", bl->UndulatorSrc.lambda* 1e6);
      break;   
     case 'L':
     case 'M':
//       up= (struct UndulatorSourceType *)bl->RTSource.Quellep;  
       fprintf(f, "%20c    ***Undulator Source for Ray Tracing***\n", bl->SourceType);
       fprintf(f, "%20lg    Undulator length     (mm)\n", bl->UndulatorSrc.length);
       fprintf(f, "%20lg    Undulator wavelength (nm)\n", bl->UndulatorSrc.lambda* 1e6);
       fprintf(f, "%20lg    Undulator SLS offset (mm)\n", bl->UndulatorSrc.deltaz);
       break;
     case 'G':
//       up0= (struct UndulatorSource0Type *)bl->RTSource.Quellep;  
       fprintf(f, "%20c    ***Undulator Source for Ray Tracing***\n", bl->SourceType);
       fprintf(f, "%20lg    Undulator length     (mm)\n", bl->UndulatorSrc.length);
       fprintf(f, "%20lg    Undulator wavelength (nm)\n", bl->UndulatorSrc.lambda* 1e6);
       fprintf(f, "%20lg    Undulator offset (mm)\n", bl->UndulatorSrc.deltaz);
       fprintf(f, "%20lg    hor. e-beam size (mm)\n", bl->UndulatorSrc.sigmaez);
       fprintf(f, "%20lg    vert. e-beam size (mm)\n", bl->UndulatorSrc.sigmaey);
       fprintf(f, "%20lg    hor. e-beam divergence (mrad)\n", bl->UndulatorSrc.sigmaedz);
       fprintf(f, "%20lg    vert. e-beam divergence (mrad)\n", bl->UndulatorSrc.sigmaedy);
       break;
     case 'H': 
//       hp= (struct HardEdgeSourceType *)bl->RTSource.Quellep;
       fprintf(f, "%20c    ***Hard Edge Source for Ray Tracing***\n", bl->SourceType);
       fprintf(f, "%20lg    total height\n", bl->HardEdgeSrc.disty);
       fprintf(f,  "%20d    points \n"     , bl->HardEdgeSrc.iy);  
       fprintf(f, "%20lg    total width \n", bl->HardEdgeSrc.distz);
       fprintf(f,  "%20d    points \n"     , bl->HardEdgeSrc.iz);   
       fprintf(f, "%20lg    total vert. divergency.\n", bl->HardEdgeSrc.divy);
       fprintf(f,  "%20d    points \n"     , bl->HardEdgeSrc.idy);  
       fprintf(f, "%20lg    total hor. divergency\n", bl->HardEdgeSrc.divz);
       fprintf(f,  "%20d    points \n"     , bl->HardEdgeSrc.idz);  
     break;   
     case 'D': 
//       dp= (struct DipolSourceType *)bl->RTSource.Quellep;
       fprintf(f, "%20c    *** Dipol Source for Ray Tracing***\n", bl->SourceType);
       fprintf(f, "%20lg    sigma y\n",  bl->DipolSrc.sigy);
       fprintf(f, "%20lg    sigma dy\n", bl->DipolSrc.sigdy);  
       fprintf(f, "%20lg    sigma z\n",  bl->DipolSrc.sigz);
       fprintf(f, "%20lg    dz (hard)\n", bl->DipolSrc.dz);
     break;   
     case 'o': 
//       sop= (struct PointSourceType *)bl->RTSource.Quellep;
       fprintf(f, "%20c    *** Point Source for Ray Tracing ***\n", bl->SourceType);
       fprintf(f, "%20lg    sigma y\n",  bl->PointSrc.sigy);
       fprintf(f, "%20lg    sigma dy\n", bl->PointSrc.sigdy);  
       fprintf(f, "%20lg    sigma z\n",  bl->PointSrc.sigz);
       fprintf(f, "%20lg    sigma dz\n", bl->PointSrc.sigdz);
     break;  
     case 'R': 
//       rp= (struct RingSourceType *)bl->RTSource.Quellep;
       fprintf(f, "%20c    *** Ring Source for Ray Tracing ***\n", bl->SourceType);
       fprintf(f, "%20lg    dy\n", bl->RingSrc.dy);  
       fprintf(f, "%20lg    dz\n", bl->RingSrc.dz);
     break;  
     case 'S': 
//       sp= (struct SRSourceType *)bl->RTSource.Quellep; 
       fprintf(f, "%20c    *** Single Ray for Ray Tracing***\n", bl->SourceType);
       fprintf(f, "%20lg    y  single ray\n", bl->SRSrc.y);
       fprintf(f, "%20lg    dy single ray\n", bl->SRSrc.dy);
       fprintf(f, "%20lg    z  single ray\n", bl->SRSrc.z);
       fprintf(f, "%20lg    dz single ray\n", bl->SRSrc.dz);
     break;    
   case 'F': 
     fprintf(f, "%20c    *** Rays from file ***\n", bl->SourceType);
     break;  
     default:
       fprintf(f, "%20c    *** Error: Unknown Source ***\n", bl->SourceType);
   }
// 
*/
   fprintf(f,"%20d    number of points\n", bl->raynumber); 
   // end source section // 
//---------------------------------------------------------------------// 


// /*

//   listpt= bl->ElementList;

   fprintf(f, "\nData of optical Elements\n"); 
   fprintf(f, "ELEMENTS\n");
   fprintf(f, "%20d   number of elements \n\n"     , bl->NumElements);  

   elnumber= 1;
   while (elnumber<= bl->NumElements) 
   {
     fprintf(f, "\nElement %d\n", elnumber);
     fprintf(f, "%20s     name of elem. \n", bl->ElementList[elnumber-1].elementname); 
     fprintf(f, "\nGEOMETRY %d\n", elnumber); 
     fprintf(f, "%20lg     theta              \n", bl->ElementList[elnumber-1].GDat.theta0); 
     fprintf(f, "%20lg     source distance    \n", bl->ElementList[elnumber-1].GDat.r);
     fprintf(f, "%20lg     image  distance    \n", bl->ElementList[elnumber-1].GDat.rp);
     for (i= 0; i< 5; i++) 
       fprintf(f, "%20lg     line density x[%d] \n", bl->ElementList[elnumber-1].GDat.xdens[i], i);
     //fprintf(f, "%20lg     lambda [nm]         \n", bl->ElementList[elnumber-1].GDat.lambdag* 1e6); 
     fprintf(f, "%20d     diffraction order  \n", bl->ElementList[elnumber-1].GDat.inout);
     fprintf(f, "%20d     flag               \n", bl->ElementList[elnumber-1].GDat.iflag);   
     fprintf(f, "%20d     azimut * Pi/2      \n", bl->ElementList[elnumber-1].GDat.azimut);   
 // end geometry section //  	  

     fprintf(f, "\nMIRROR %d  \n", elnumber);  
     fprintf(f, "%20d     element type\n", bl->ElementList[elnumber-1].MDat.Art);   
     fprintf(f, "%20lg     source distance (ARC)\n", bl->ElementList[elnumber-1].MDat.r1);     
     fprintf(f, "%20lg     image  distance (ARC)\n", bl->ElementList[elnumber-1].MDat.r2);
#ifdef OBSOLETE
     fprintf(f, "%20lg     theta (ARC)\n", bl->ElementList[elnumber-1].MDat.alpha);
#endif
     fprintf(f, "%20lg     radius rw (r)       \n", bl->ElementList[elnumber-1].MDat.rmi);
     fprintf(f, "%20lg     radius rl (rho)     \n", bl->ElementList[elnumber-1].MDat.rho);    
     fprintf(f, "%20d     translation flag\n", bl->ElementList[elnumber-1].MDat.iflagmi);    
     fprintf(f, "%20lg     wmin\n", bl->ElementList[elnumber-1].MDat.w1);    
     fprintf(f, "%20lg     wmax\n", bl->ElementList[elnumber-1].MDat.w2);  
     fprintf(f, "%20lg     lmin\n", bl->ElementList[elnumber-1].MDat.l1);  
     fprintf(f, "%20lg     lmax\n", bl->ElementList[elnumber-1].MDat.l2);  
     fprintf(f, "%20lg     slope w (arcsec rms)\n", bl->ElementList[elnumber-1].MDat.slopew);  
     fprintf(f, "%20lg     slope l (arcsec rms)\n", bl->ElementList[elnumber-1].MDat.slopel);

     fprintf(f, "%20lg     misalignment du\n", bl->ElementList[elnumber-1].MDat.du);
     fprintf(f, "%20lg     misalignment dw\n", bl->ElementList[elnumber-1].MDat.dw);
     fprintf(f, "%20lg     misalignment dl\n", bl->ElementList[elnumber-1].MDat.dl);
     fprintf(f, "%20lg     misalignment dRu (rad)\n", bl->ElementList[elnumber-1].MDat.dRu);
     fprintf(f, "%20lg     misalignment dRw (rad)\n", bl->ElementList[elnumber-1].MDat.dRw);
     fprintf(f, "%20lg     misalignment dRl (rad)\n", bl->ElementList[elnumber-1].MDat.dRl);

     // end mirror section // 
     // end element        //
     elnumber++; //listpt++;
   }
// */

// 
/*
//   op= (struct OptionsType *) &(bl->BLOptions);

   fprintf(f, "\nCONTROL_FLAGS\n");
   fprintf(f, "%20d     map- order (3, 4)\n", bl->BLOptions.ifl.iord); 
   fprintf(f, "%20d     iordsc (4)\n", bl->BLOptions.ifl.iordsc); 
   fprintf(f, "%20d     expansion of pathlength (1)\n", bl->BLOptions.ifl.iexpand); 
   fprintf(f,
  "%20d numerical (0), analytical (1) subtraction of ideal path length\n", 
	  bl->BLOptions.ifl.iplmode);
   fprintf(f, "%20d     write 4-dim brightness to file (1)\n", 
	   bl->BLOptions.ifl.ibright); 
   fprintf(f, "%20d     Integration simps (0), spline 1, -1\n", 
	   bl->BLOptions.ifl.ispline); 
   fprintf(f, "%20d (1) normalize output, (0) do not normalize\n", 
	   bl->BLOptions.ifl.inorm); 
   fprintf(f, "%20d inorm1\n", bl->BLOptions.ifl.inorm1); 
   fprintf(f, "%20d inorm2 (0, 1, 2)\n", bl->BLOptions.ifl.inorm2); 
   fprintf(f, 
   "%20d derive matrix elements in 3 different ways (1) (for debugging)\n",
	   bl->BLOptions.ifl.matrel);
   fprintf(f, "%20d (1): phase advance for grating, (0): mirror\n", 
	   bl->BLOptions.ifl.igrating);
   fprintf(f, "%20d  insert pinhole array in source plane (0)\n", 
	   bl->BLOptions.ifl.ipinarr);
   
   // end control_flags //

   fprintf(f,"\nAPERTURES\n"); 
   fprintf(f, "%20lg radius of pinhole in source plane (mm)\n", bl->BLOptions.apr.rpin);
   fprintf(f, "%20lg aperture in source plane, ymin (mm)\n", bl->BLOptions.apr.srcymin);
   fprintf(f, "%20lg aperture in source plane, ymax (mm)\n", bl->BLOptions.apr.srcymax);
   fprintf(f, "%20lg aperture in source plane, zmin (mm)\n", bl->BLOptions.apr.srczmin);
   fprintf(f, "%20lg aperture in source plane, zmax (mm)\n", bl->BLOptions.apr.srczmax);

   fprintf(f, "%20lg radius in aperture plane\n", bl->BLOptions.apr.rpin_ap);
   fprintf(f, "%20lg aperture in ap. plane, ymin (mm)\n", bl->BLOptions.apr.ymin_ap);
   fprintf(f, "%20lg aperture in ap. plane, ymax (mm)\n", bl->BLOptions.apr.ymax_ap); 
   fprintf(f, "%20lg aperture in ap. plane, zmin (mm)\n", bl->BLOptions.apr.zmin_ap);
   fprintf(f, "%20lg aperture in ap. plane, zmax (mm)\n", bl->BLOptions.apr.zmax_ap);

   // end apertures //
   fprintf(f,"\nINTEGRATION\n"); 
   fprintf(f, "%20lg distance to focus \n", bl->BLOptions.xi.distfocy);
   fprintf(f, "%20lg distance to focus \n", bl->BLOptions.xi.distfocz);
  
   // fprintf(f,"%20d     itery0\n", bl->BLOptions.xi.itery0); //
   fprintf(f,"%20d     ianzy0\n", bl->BLOptions.xi.ianzy0);   
   // fprintf(f,"%20d     imaxy\n",  bl->BLOptions.xi.imaxy); 
   // fprintf(f,"%20d     inumy\n",  bl->BLOptions.xi.inumy); //

   // fprintf(f,"%20d     iterz0\n", bl->BLOptions.xi.iterz0); //
   fprintf(f,"%20d     ianzz0\n", bl->BLOptions.xi.ianzz0);   
   // fprintf(f,"%20d     imaxz\n",  bl->BLOptions.xi.imaxz); //
   // fprintf(f,"%20d     inumz\n",  bl->BLOptions.xi.inumz); //

   fprintf(f,"%20lg     ymin \n", bl->BLOptions.xi.ymin);   
   fprintf(f,"%20lg     ymax \n", bl->BLOptions.xi.ymax); 
   // fprintf(f,"%20lg     fracy \n", bl->BLOptions.xi.fracy);  // 
   // fprintf(f,"%20lg     frac1y\n", bl->BLOptions.xi.frac1y); //

   fprintf(f,"%20lg     zmin \n", bl->BLOptions.xi.zmin);   
   fprintf(f,"%20lg     zmax \n", bl->BLOptions.xi.zmax); 
   // fprintf(f,"%20lg     fracz \n", bl->BLOptions.xi.fracz); //  
   // fprintf(f,"%20lg     frac1z\n", bl->BLOptions.xi.frac1z);// 

//    fprintf(f, "%20lg     phase_change_1 \n", bl->BLOptions.xi.phase_change_1);  //
//    fprintf(f, "%20lg     phase_change_2 \n", bl->BLOptions.xi.phase_change_2);   // 
   fprintf(f, "%20lg     d12_max \n", bl->BLOptions.xi.d12_max); 
 //   fprintf(f, "%20lg     amp_change \n", bl->BLOptions.xi.amp_change);   // 
 //  fprintf(f, "%20lg     dphi_min \n", bl->BLOptions.xi.dphi_min); //

   fprintf(f, "%20d  iamp_smooth (0,1,2)   \n", bl->BLOptions.xi.iamp_smooth);  
   fprintf(f, "%20d  iord_amp   \n", bl->BLOptions.xi.iord_amp);  
   fprintf(f, "%20d  iord_pha   \n", bl->BLOptions.xi.iord_pha);  
   // fprintf(f, "%20d  order of amplitude expansion   \n", bl->BLOptions.xi.iordap); //
//    fprintf(f,  //
//    "%20d  (0) do not allow, (1) allow change of curvature sign of phase   \n",  //
//            bl->BLOptions.xi.iphase_curv);   //
//    fprintf(f,  //
//      "%20d  (1) correct phase for pi and 2pi, (0) correct only for 2 pi   \n", //
//            bl->BLOptions.xi.iphase_pi2); //
   fprintf(f, "%20d  ifm_amp   \n", bl->BLOptions.xi.ifm_amp);
   fprintf(f, "%20d  ifm_pha   \n", bl->BLOptions.xi.ifm_pha);
   fprintf(f, "%20d  id12; (1) print d12 on file \n", bl->BLOptions.xi.id12);
   fprintf(f, "%20d  ianz0_cal \n", bl->BLOptions.xi.ianz0_cal);
   fprintf(f, "%20d  ianz0_fixed \n", bl->BLOptions.xi.ianz0_fixed);
      // end integration //

   fprintf(f, "\nPSSOURCES\n"); 
   fprintf(f, "%20d  source type \n", bl->src.isrctype);
   // source 1 //

   fprintf(f, "%20d  so1: isrcy   \n", bl->src.so1.isrcy);
   fprintf(f, "%20d  so1: isrcdy  \n", bl->src.so1.isrcdy);
   
   //fprintf(f, "%20lg so1: sigmay  \n", bl->src.so1.sigmay);
   //fprintf(f, "%20lg so1: sigmayp \n", bl->src.so1.sigmayp);
   fprintf(f, "%20g so1: sigmay  \n", bl->src.so1.sigmay);
   fprintf(f, "%20g so1: sigmayp \n", bl->src.so1.sigmayp);
   
   fprintf(f, "%20d  so1: isrcz   \n", bl->src.so1.isrcz);
   fprintf(f, "%20d  so1: isrcdz  \n", bl->src.so1.isrcdz);
   fprintf(f, "%20lg so1: sigmaz  \n", bl->src.so1.sigmaz);
   fprintf(f, "%20lg so1: sigmazp \n", bl->src.so1.sigmazp);
   // source 4 //
   fprintf(f, "%s so4.a\n", bl->src.so4.fsource4a);
   fprintf(f, "%s so4.b\n", bl->src.so4.fsource4b);
   fprintf(f, "%s so4.c\n", bl->src.so4.fsource4c);
   fprintf(f, "%s so4.d\n", bl->src.so4.fsource4d);
   // source 5 //
   fprintf(f, "%20lg so5: (Dipol) dipcy   \n", bl->src.so5.dipcy);
   fprintf(f, "%20lg so5: (Dipol) dipcz   \n", bl->src.so5.dipcz);
   fprintf(f, "%20lg so5: (Dipol) dipdisy \n", bl->src.so5.dipdisy);
   fprintf(f, "%20lg so5: (Dipol) dipdisz \n", bl->src.so5.dipdisz);
//    fprintf(f, "%20lg so5: (Dipol) dipymin \n", bl->src.so5.dipymin); //
//    fprintf(f, "%20lg so5: (Dipol) dipymax \n", bl->src.so5.dipymax); //
//    fprintf(f, "%20lg so5: (Dipol) dipzmin \n", bl->src.so5.dipzmin); //
//    fprintf(f, "%20lg so5: (Dipol) dipzmax \n", bl->src.so5.dipzmax); //
// source 6 //
   fprintf(f, "%s so6\n", bl->src.so6.fsource6);

   fprintf(f, "%20lg pin_yl0 \n", bl->src.pin_yl0);
   fprintf(f, "%20lg pin_yl  \n", bl->src.pin_yl);
   fprintf(f, "%20lg pin_zl0 \n", bl->src.pin_zl0);
   fprintf(f, "%20lg pin_zl  \n", bl->src.pin_zl);

// end PSSOURCES //
// ende neu //
  
   fprintf(f,"\nOPTIONS\n"); 
   fprintf(f,"%20d     (1) RT Source to Image \n", bl->BLOptions.SourcetoImage);  
   
   fprintf(f,"%20lg     epsilon\n", bl->BLOptions.epsilon);     
   fprintf(f,"%20d     flag calculation modus\n", bl->BLOptions.CalcMod);    
   fprintf(f,"%20lg     lambda [nm]\n", bl->BLOptions.lambda* 1e6);  
   fprintf(f,"%20lg     dispersive length\n", bl->BLOptions.displength); 
   fprintf(f,"%20lg     * y = dlambda\n", bl->deltalambdafactor);
   // new feb 04 //
   fprintf(f,"%20d     with alignment\n", bl->BLOptions.WithAlign);

   fprintf(f,"%20d     dy integr. points (PS fixed grid)\n", bl->BLOptions.PSO.ndyfix);  
   fprintf(f,"%20d     dz integr. points (PS fixed grid)\n", bl->BLOptions.PSO.ndzfix); 
   fprintf(f,"%20lg     dymin [rad] (PS fixed grid)\n", bl->BLOptions.PSO.dyminfix);   
   fprintf(f,"%20lg     dymax [rad] (PS fixed grid)\n", bl->BLOptions.PSO.dymaxfix);   
   fprintf(f,"%20lg     dzmin [rad] (PS fixed grid)\n", bl->BLOptions.PSO.dzminfix);   
   fprintf(f,"%20lg     dzmax [rad] (PS fixed grid)\n", bl->BLOptions.PSO.dzmaxfix); 
  
   fprintf(f,"%20lg     y  [mm]   (PS Source)\n", bl->BLOptions.PSO.PSSource.sigy);   
   fprintf(f,"%20lg     dy [rad] (PS Source)\n", bl->BLOptions.PSO.PSSource.sigdy);   
   fprintf(f,"%20lg     z  [mm]   (PS Source)\n", bl->BLOptions.PSO.PSSource.sigz);   
   fprintf(f,"%20lg     dz [rad] (PS Source)\n", bl->BLOptions.PSO.PSSource.sigdz);   

   fprintf(f,"%20d     flag y  1^= +/-hard, 0^= sigma (0,1)\n", 
                                          bl->BLOptions.PSO.PSSource.yhard);
   fprintf(f,"%20d     flag dy '' (PS Source)\n", bl->BLOptions.PSO.PSSource.dyhard);   
   fprintf(f,"%20d     flag z  '' (PS Source)\n", bl->BLOptions.PSO.PSSource.zhard);   
   fprintf(f,"%20d     flag dz '' (PS Source)\n", bl->BLOptions.PSO.PSSource.dzhard);  
   fprintf(f,"%20d     flag <> 2 fixed grid integr.\n", bl->BLOptions.PSO.intmod); 
  
// end options section // 

// 
*/
   fprintf(f,"\n*** end of file ***\n");
   fclose(f); 
#ifdef DEBUG
   printf(" ==> done\n");
#endif
return 0;
} // end WriteBLFile //
}


// **************************************************************************************

// int pha4idlReadBLFile(char *fname, struct pha4idlBeamlineFile *bl)
int pha4idlReadBLFile(IDL_STRING *name, struct pha4idlBeamlineFile *bl)
{
   FILE *f; 
   int  rcode, elnumber, alle, i, version;
   char buffer[255], buf;
   double *pd; 

   rcode= -1;

   char *fname = name->s;
   //char *tmpchar;

   printf("pha4idlReadBLFile: filename: %s\n", fname);
   
   
   fprintf(stderr, "routine not implemented!\n");
   // 2bdone add a call to the actual routine
   
   return rcode;  
}  //  end ReadBLFile //

// **************************************************************************************





