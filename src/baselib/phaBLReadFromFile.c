/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phasecommon/phaBLInit.c */
/*  Date      : <06 Mar 06 16:14:16 flechsig>  */
/*  Time-stamp: <24 Nov 06 16:56:31 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#include "../phase/common.h"
#include "../phase/mirrorpck.h"
#include "../phase/geometrypck.h"
#include "../phase/fg3pck.h"
#include "../phase/phase_struct.h"

#include "phabasedefs.h"
#include "phabasestructs.h"
#include "idl_export.h"

/* 
   idl wrapper routine fuer phaBLReadFromFile 
*/
int phaBLReadFromFileIDL(IDL_STRING *fname, IDL_LONG *beamlineptr) 
{
    struct BeamlineType *bl= (struct BeamlineType *) *beamlineptr;
    return phaBLReadFromFile(fname->s, bl);
} /* end phaBLReadFromFile */


/* 
  wrapper routine fuer ReadBLFile 
*/
int phaBLReadFromFile(char *fname, struct BeamlineType *bl) 
{
    return ReadBLFile(fname, bl);
} /* end phaBLReadFromFile */

/************************************************************************/
/* liest den datensatz vom file 					*/     
/* PHASEset.psourcename sollte danach mit brightnessnamen initialisert  */
/* werden: strcpy(PHASESet.pssourcename, Beamline.src.so6.fsource6)     */
/* Uwe 30.5.96, UF 10.3.06			                        */     
/************************************************************************/ 
int ReadBLFile(char *fname, struct BeamlineType *bl)  
{   
   FILE   *f; 
   int    rcode, elnumber, alle, i, version;
   char   buffer[MaxPathLength], buf;  
   double *pd; 
   
   struct UndulatorSourceType  *up;
   struct UndulatorSource0Type *up0;
   struct DipolSourceType      *dp;
   struct PointSourceType      *sop;
   struct HardEdgeSourceType   *hp;     
   struct SRSourceType         *sp; 
   struct PSImageType          *psip;
   struct PSSourceType         *pssp;    
   struct ElementType 	       *listpt;   
   struct OptionsType          *op;     

   rcode= -1;   
   printf("ReadBLFile: filename: %s\n", fname);

   /* initialisiere Strings */
   i= sizeof(bl->src.so4.fsource4a);
   memset(&bl->src.so4.fsource4a, 0, i);
   memset(&bl->src.so4.fsource4b, 0, i);
   memset(&bl->src.so4.fsource4c, 0, i);
   memset(&bl->src.so4.fsource4d, 0, i);
   memset(&bl->src.so6.fsource6,  0, i); 
   
   if ((f= fopen(fname, "r")) == NULL) 
   {
     fprintf(stderr, "File %s not found!\n", fname);
     bl->elementzahl= 0;
     /*UF 10.3.06 take it out  initdatset(&Fg3DefDat, &Beamline, 0); 		/* source init */
   }
   else 
   {   
     if((rcode= CheckFileHeader(f, Fg3PickFileHeader, &version)) == 0)   
     {
       printf("ReadBLFile: file version: %d\n", version);
       if (SetFilePos(f, "SOURCE"))
       { 
         fscanf(f, " %c %[^\n]s %c", &bl->RTSource.QuellTyp, buffer, &buf); 
         printf("source type: %c >> %s\n", bl->RTSource.QuellTyp, buffer);
         switch(bl->RTSource.QuellTyp)
	   {
	   case 'U': 
	   case 'u':
	     up= (struct UndulatorSourceType *) 
	       &(bl->RTSource.Quelle.UndulatorSource);
             fscanf(f, " %lf %[^\n]s %c", &up->length, buffer, &buf);
	     fscanf(f, " %lf %[^\n]s %c", &up->lambda, buffer, &buf);  
	     
	     printf("%20lf    Undulator length     (mm)\n", up->length);
	     printf("%20lf    Undulator wavelength (nm)\n", up->lambda); 
	      
	     up->lambda*= 1e-6;                       /* intern in mm */
	   break;   
	   case 'L': 
	   case 'M':
             up= (struct UndulatorSourceType *) 
	       &(bl->RTSource.Quelle.UndulatorSource);
             fscanf(f, " %lf %[^\n]s %c", &up->length, buffer, &buf);
	     fscanf(f, " %lf %[^\n]s %c", &up->lambda, buffer, &buf);  
	     fscanf(f, " %lf %[^\n]s %c", &up->deltaz, buffer, &buf);  
	     printf("%20lf    Undulator length     (mm)\n", up->length);
	     printf("%20lf    Undulator wavelength (nm)\n", up->lambda); 
	     printf("%20lf    Undulator SLS offset (mm)\n", up->deltaz);  
	     up->lambda*= 1e-6;                       /* intern in mm */
	   break;   
	   case 'G':
             up0= (struct UndulatorSource0Type *) 
	       &(bl->RTSource.Quelle.UndulatorSource0);
             fscanf(f, " %lf %[^\n]s %c", &up0->length, buffer, &buf);
	     fscanf(f, " %lf %[^\n]s %c", &up0->lambda, buffer, &buf);  
	     fscanf(f, " %lf %[^\n]s %c", &up0->deltaz, buffer, &buf);  
	     fscanf(f, " %lf %[^\n]s %c", &up0->sigmaez, buffer, &buf);  
	     fscanf(f, " %lf %[^\n]s %c", &up0->sigmaey, buffer, &buf);  
	     fscanf(f, " %lf %[^\n]s %c", &up0->sigmaedz, buffer, &buf);  
	     fscanf(f, " %lf %[^\n]s %c", &up0->sigmaedy, buffer, &buf);  
	     printf("%20lf    Undulator length     (mm)\n", up0->length);
	     printf("%20lf    Undulator wavelength (nm)\n", up0->lambda); 
	     printf("%20lf    Undulator SLS offset (mm)\n", up0->deltaz);  
	     printf("%20lf    hor. e-beam size (mm)\n", up0->sigmaez);  
	     printf("%20lf    vert. e-beam size (mm)\n", up0->sigmaey);  
	     printf("%20lf    hor. e-beam divergence (mrad)\n", up0->sigmaedz);  
	     printf("%20lf    vert. e-beam divergence (mrad)\n", up0->sigmaedy);  

	      
	      up0->lambda*= 1e-6;                       /* intern in mm */
	   break;   
	 case 'H': 
             hp= (struct HardEdgeSourceType *) 
	       &(bl->RTSource.Quelle.HardEdgeSource);
             fscanf(f, " %lf %[^\n]s %c", &hp->disty, buffer, &buf);  
	     fscanf(f, " %d  %[^\n]s %c", &hp->iy   , buffer, &buf);   
             fscanf(f, " %lf %[^\n]s %c", &hp->distz, buffer, &buf);  
	     fscanf(f, " %d  %[^\n]s %c", &hp->iz   , buffer, &buf);   
             fscanf(f, " %lf %[^\n]s %c", &hp->divy , buffer, &buf);  
	     fscanf(f, " %d  %[^\n]s %c", &hp->idy  , buffer, &buf);   
             fscanf(f, " %lf %[^\n]s %c", &hp->divz , buffer, &buf);  
	     fscanf(f, " %d  %[^\n]s %c", &hp->idz  , buffer, &buf);   
	   break;   
           case 'D': 
             dp= (struct DipolSourceType *) &(bl->RTSource.Quelle.DipolSource);
             fscanf(f, " %lf %[^\n]s %c", &dp->sigy , buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &dp->sigdy, buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &dp->sigz, buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &dp->dz, buffer, &buf);  
           break; 
	   case 'o': 
             sop= (struct PointSourceType *) 
	       &(bl->RTSource.Quelle.PointSource);
             fscanf(f, " %lf %[^\n]s %c", &sop->sigy , buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &sop->sigdy, buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &sop->sigz , buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &sop->sigdz, buffer, &buf);  
           break; 
           case 'S': 
             sp= (struct SRSourceType *) &(bl->RTSource.Quelle.SRSource);
             fscanf(f, " %lf %[^\n]s %c", &sp->y,  buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &sp->dy, buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &sp->z,  buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &sp->dz, buffer, &buf);    
           break;  
           case 'I':
             psip= (struct PSImageType *) &(bl->RTSource.Quelle.PSImage);
             fscanf(f, " %lf %[^\n]s %c", &psip->ymin,  buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &psip->ymax, buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &psip->zmin,  buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &psip->zmax, buffer, &buf);    
             fscanf(f, " %d %[^\n]s %c", &psip->iy, buffer, &buf);  
	     fscanf(f, " %d %[^\n]s %c", &psip->iz, buffer, &buf);  
           break;
	   case 'F':
	     break;
           default: 
	     fprintf(stderr, "error: unknown source type!\n"); /* exit(-1); */
	 }
         fscanf(f, " %d %[^\n]s %c", &bl->RTSource.raynumber, buffer, &buf);
      	 printf("source read- rays: %d\n", bl->RTSource.raynumber);

       } else rcode= -1;  /* source data not found in file */ 
/* source eingelesen,- nun Elemente einlesen */
/*---------------------------------------------------------------------*/ 
       
       if (SetFilePos(f, "ELEMENTS"))                       
         fscanf(f, " %d %[^\n]s %c", &bl->elementzahl, buffer, &buf); 
       else rcode= -1;  /* data not found in file */     

       if (bl->elementzahl > 0)   	/* allociere memory */
       {
	 if ((bl->ElementList= (struct ElementType *) 
	      realloc(bl->ElementList, 
		      bl->elementzahl* sizeof(struct ElementType))) == NULL)
	   {  fprintf(stderr, "realloc error in ReadBLFile \n"); exit(-1); }   
       }
       elnumber= 1;
       listpt= bl->ElementList;

       while (elnumber<= bl->elementzahl) 
       {
          listpt->ElementOK= 0;       /* reset OK */

	  sprintf(buffer, "Element %d", elnumber);	
	  if (SetFilePos(f, buffer)) 
          {  /* lese ein ... */
             fscanf(f, " %s %[^\n]s %c", &listpt->elementname, buffer, &buf);
#ifdef DEBUG 
	     /*    printf("   Name read: %s\n", listpt->elementname); */
#endif
          } else rcode= -1;

          sprintf(buffer, "GEOMETRY %d", elnumber); 
          if (SetFilePos(f, buffer)) 
          {  /* lese ein ... */
             pd= (double *) &listpt->GDat.theta0; 
             for (i= 0; i < 9; i++, pd++) /* !!Fehleranfaellig !! */
             {
               fgets(buffer, 80, f); sscanf(buffer, "%lf", pd);    
             } 
	     listpt->GDat.lambda*= 1e-6;
             fgets(buffer, 80, f); sscanf(buffer, "%d", &listpt->GDat.inout);  
             fgets(buffer, 80, f); sscanf(buffer, "%d", &listpt->GDat.iflag);  
             fgets(buffer, 80, f); sscanf(buffer, "%d", &listpt->GDat.azimut); 
	     /*  printf("   geometry read\n"); */
          } else rcode= -1;  

          sprintf(buffer, "MIRROR %d", elnumber);  
          if (SetFilePos(f, buffer)) 
          {  /* lese ein ... */
	    fgets(buffer, 80, f); sscanf(buffer, "%d", &listpt->MDat.Art);
            /* fscanf(f, " %d %[^\n]s %c", &listpt->Art, buffer, &buf);*/
	     
	    pd= (double *) &listpt->MDat.r1;                 
	    for (i= 0; i < 5; i++, pd++) 
	      {
	       fgets(buffer, 80, f); sscanf(buffer, "%lf", pd);    
	      }
	    fscanf(f, " %d %[^\n]s %c", &listpt->MDat.iflagmi, buffer, &buf); 
	    fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.w1, buffer, &buf); 
	    fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.w2, buffer, &buf); 
	    fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.l1, buffer, &buf); 
	    fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.l2, buffer, &buf); 
	    fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.slopew, buffer, &buf); 
	    fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.slopel, buffer, &buf); 
	    if (version >= 20040217)
	      {
		fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.du, buffer, &buf);
		fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.dw, buffer, &buf);
		fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.dl, buffer, &buf);
		fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.dRu, buffer, &buf);
		fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.dRw, buffer, &buf);
		fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.dRl, buffer, &buf);
	      }
#ifdef DEBUG
	    /*  printf("   mirror read\n"); */
#endif
	    printf("Elementtype: %d, name: %s\n", listpt->MDat.Art, listpt->elementname);
          } else rcode= -1;
          elnumber++; listpt++;
       } 
/* last modification: 18 Jul 97 09:54:14 flechsig */
       if (SetFilePos(f, "CONTROL_FLAGS"))
         { 
           op= (struct OptionsType *) &(bl->BLOptions); 
           fscanf(f, " %d %[^\n]s %c",  &op->ifl.iord, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.iordsc, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.iexpand, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.iplmode, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.ibright, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.ispline, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.inorm, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.inorm1, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.inorm2, buffer, &buf); 
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.matrel, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.igrating, buffer, &buf);  
           fscanf(f, " %d %[^\n]s %c",  &op->ifl.ipinarr, buffer, &buf); 
         } else  rcode= -1;
       
       if (SetFilePos(f, "APERTURES"))
         { 
           op= (struct OptionsType *) &(bl->BLOptions); 
           fscanf(f, " %lf %[^\n]s %c", &op->apr.rpin, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.srcymin, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.srcymax, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.srczmin, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.srczmax, buffer, &buf);

           fscanf(f, " %lf %[^\n]s %c", &op->apr.rpin_ap, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.ymin_ap, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.ymax_ap, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.zmin_ap, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.zmax_ap, buffer, &buf);
         } else  rcode= -1;

       if (SetFilePos(f, "INTEGRATION"))
       { 
         op= (struct OptionsType *) &(bl->BLOptions); 
         fscanf(f, " %lf %[^\n]s %c", &op->xi.distfocy, buffer, &buf);
         fscanf(f, " %lf %[^\n]s %c", &op->xi.distfocz, buffer, &buf);
	 /* fscanf(f, " %d %[^\n]s %c", &op->xi.itery0, buffer, &buf); */
	 fscanf(f, " %d %[^\n]s %c", &op->xi.ianzy0, buffer, &buf);
	 /* fscanf(f, " %d %[^\n]s %c", &op->xi.imaxy, buffer, &buf); */
	 /* fscanf(f, " %d %[^\n]s %c", &op->xi.inumy, buffer, &buf); */

	 /* fscanf(f, " %d %[^\n]s %c", &op->xi.iterz0, buffer, &buf); */
	 fscanf(f, " %d %[^\n]s %c", &op->xi.ianzz0, buffer, &buf);
	 /* fscanf(f, " %d %[^\n]s %c", &op->xi.imaxz, buffer, &buf); */
	 /* fscanf(f, " %d %[^\n]s %c", &op->xi.inumz, buffer, &buf); */
       
	 fscanf(f, " %lf %[^\n]s %c", &op->xi.ymin, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->xi.ymax, buffer, &buf);
	 /* fscanf(f, " %lf %[^\n]s %c", &op->xi.fracy, buffer, &buf);  */ 
         /* fscanf(f, " %lf %[^\n]s %c", &op->xi.frac1y, buffer, &buf); */ 
	 
	 fscanf(f, " %lf %[^\n]s %c", &op->xi.zmin, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->xi.zmax, buffer, &buf);
	/* fscanf(f, " %lf %[^\n]s %c", &op->xi.fracz, buffer, &buf);  */
        /* fscanf(f, " %lf %[^\n]s %c", &op->xi.frac1z, buffer, &buf); */

/*     fscanf(f, " %lf %[^\n]s %c", &op->xi.phase_change_1, buffer, &buf);   */
/*     fscanf(f, " %lf %[^\n]s %c", &op->xi.phase_change_2, buffer, &buf);   */
         fscanf(f, " %lf %[^\n]s %c", &op->xi.d12_max, buffer, &buf);
/* 	 fscanf(f, " %lf %[^\n]s %c", &op->xi.amp_change, buffer, &buf);   */
	 /* fscanf(f, " %lf %[^\n]s %c", &op->xi.dphi_min, buffer, &buf); */

         fscanf(f, " %d %[^\n]s %c", &op->xi.iamp_smooth, buffer, &buf);
         fscanf(f, " %d %[^\n]s %c", &op->xi.iord_amp, buffer, &buf);
         fscanf(f, " %d %[^\n]s %c", &op->xi.iord_pha, buffer, &buf);
         /* fscanf(f, " %d %[^\n]s %c", &op->xi.iordap, buffer, &buf); */
/*          fscanf(f, " %d %[^\n]s %c", &op->xi.iphase_curv, buffer, &buf); */
/*          fscanf(f, " %d %[^\n]s %c", &op->xi.iphase_pi2, buffer, &buf); */
         fscanf(f, " %d %[^\n]s %c", &op->xi.ifm_amp, buffer, &buf);
         fscanf(f, " %d %[^\n]s %c", &op->xi.ifm_pha, buffer, &buf);
         fscanf(f, " %d %[^\n]s %c", &op->xi.id12, buffer, &buf);
	 if (!feof(f))
	   { /* voruebergehend */
	     fscanf(f, " %d %[^\n]s %c", &op->xi.ianz0_cal, buffer, &buf);
	     fscanf(f, " %d %[^\n]s %c", &op->xi.ianz0_fixed, buffer, &buf);
	   }
       } else  rcode= -1;

       if (SetFilePos(f, "PSSOURCES"))
       { 
	 fscanf(f, " %d %[^\n]s %c", &bl->src.isrctype, buffer, &buf); 
	 /* source 1 */
	 fscanf(f, " %d %[^\n]s %c",  &bl->src.so1.isrcy, buffer, &buf);
	 fscanf(f, " %d %[^\n]s %c",  &bl->src.so1.isrcdy, buffer, &buf);
	 fscanf(f, " %lf %[^\n]s %c", &bl->src.so1.sigmay, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &bl->src.so1.sigmayp, buffer, &buf); 
	 fscanf(f, " %d %[^\n]s %c",  &bl->src.so1.isrcz, buffer, &buf);
	 fscanf(f, " %d %[^\n]s %c",  &bl->src.so1.isrcdz, buffer, &buf);
	 fscanf(f, " %lf %[^\n]s %c", &bl->src.so1.sigmaz, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &bl->src.so1.sigmazp, buffer, &buf);
	 /* source 4 */
	 
	 fscanf(f, " %s %[^\n]s %c", &bl->src.so4.fsource4a, buffer, &buf);
	 fscanf(f, " %s %[^\n]s %c", &bl->src.so4.fsource4b, buffer, &buf);
	 fscanf(f, " %s %[^\n]s %c", &bl->src.so4.fsource4c, buffer, &buf);
	 fscanf(f, " %s %[^\n]s %c", &bl->src.so4.fsource4d, buffer, &buf);
	 /* source 5 */
	 fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipcy, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipcz, buffer, &buf);
	 fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipdisy, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipdisz, buffer, &buf);
/* 	 fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipymin, buffer, &buf);   */
/*          fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipymax, buffer, &buf); */
/* 	 fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipzmin, buffer, &buf);   */
/*          fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipzmax, buffer, &buf); */
	 /* source 6 */
	 fscanf(f, " %s %[^\n]s %c", &bl->src.so6.fsource6, buffer, &buf);

	 fscanf(f, " %lf %[^\n]s %c", &bl->src.pin_yl0, buffer, &buf);  
	 fscanf(f, " %lf %[^\n]s %c", &bl->src.pin_yl, buffer, &buf);
	 fscanf(f, " %lf %[^\n]s %c", &bl->src.pin_zl0, buffer, &buf);  
	 fscanf(f, " %lf %[^\n]s %c", &bl->src.pin_zl, buffer, &buf);

	 /* UF 10.3.06 put it outside	 strcpy(phset->pssourcename, bl->src.so6.fsource6); */ 
	 /* PutPHASE(phset, MainPickName); */ 
       } else  rcode= -1;   /* end PSSOURCES */

       if (SetFilePos(f, "OPTIONS"))
       { 
         op= (struct OptionsType *) &(bl->BLOptions); 
	 fscanf(f, " %d %[^\n]s %c", &op->SourcetoImage, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->epsilon, buffer, &buf);   
         fscanf(f, " %d %[^\n]s %c", &op->CalcMod, buffer, &buf);
         fscanf(f, " %lf %[^\n]s %c", &op->lambda, buffer, &buf); 
	 op->lambda*= 1e-6;
         fscanf(f, " %lf %[^\n]s %c", &op->displength, buffer, &buf); 
         fscanf(f, " %lf %[^\n]s %c", &bl->deltalambdafactor, buffer, &buf); 
	 if (version >= 20040217)
	   fscanf(f, " %d %[^\n]s %c", &op->WithAlign, buffer, &buf);
	 /* printf("ReadBLFile: file version: %d, WithAlign: %d\n", version, op->WithAlign);*/
 
         fscanf(f, " %d %[^\n]s %c", &op->PSO.ndyfix, buffer, &buf);  
         fscanf(f, " %d %[^\n]s %c", &op->PSO.ndzfix, buffer, &buf);    
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.dyminfix, buffer, &buf);   
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.dymaxfix, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.dzminfix, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.dzmaxfix, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.PSSource.sigy, buffer, &buf);   
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.PSSource.sigdy, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.PSSource.sigz, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.PSSource.sigdz, buffer, &buf); 

         fscanf(f, " %d %[^\n]s %c", &op->PSO.PSSource.yhard, buffer, &buf);  
         fscanf(f, " %d %[^\n]s %c", &op->PSO.PSSource.dyhard, buffer, &buf);
         fscanf(f, " %d %[^\n]s %c", &op->PSO.PSSource.zhard, buffer, &buf);  
         fscanf(f, " %d %[^\n]s %c", &op->PSO.PSSource.dzhard, buffer, &buf); 
         fscanf(f, " %d %[^\n]s %c", &op->PSO.intmod, buffer, &buf); 
#ifdef DEBUG
	 /*    printf("   options read\n"); */
#endif
       } else rcode= -1;  /* data not found in file */     
     } /* file ok */
     fclose(f);  
   }
   return rcode;  
}  /* end ReadBLFile */
