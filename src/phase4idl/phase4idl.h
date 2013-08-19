/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase4idl/phase4idl.h */
/*  Date      : <16 Aug 13 09:47:36 flechsig>  */
/*  Time-stamp: <19 Aug 13 14:12:30 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

//
// pha4idl_prototypes.h
//
// (c) 2008 : Torsten.Leitner@email.de
//


#ifndef PHASE4IDL_H_
#define PHASE4IDL_H_

#include <idl_export.h>
//#include "source4x.h"
#include "Constants.h"


#define MaxPathLength 255
#define MaximumOptElements 64


// structures
/*
struct UndulatorSourceType { double length, lambda, sigvert, sighor, deltaz;};  
struct UndulatorSource0Type { double length, lambda, sigvert, sighor, deltaz,
   sigmaez, sigmaey, sigmaedz, sigmaedy; };  
struct DipolSourceType     { double sigy, sigdy, sigz, dz; }; 
struct PointSourceType     { double sigy, sigdy, sigz, sigdz; };
struct RingSourceType      { double dy, dz; }; 
struct SRSourceType        { double y, z, dy, dz; };   
struct HardEdgeSourceType  { double disty, distz, divy, divz;
  int    iy, iz, idy, idz; };
struct FileSourceType { char *filename; };
// */

// 
/*
struct pha4idlBeamlineFile
{
//  unsigned int beamlineOK, position, hormapsloaded ;
    int    NumElements;
    int    raynumber;
    char   SourceType;
    double deltalambdafactor; 
//  double xlen0 ;
//  MAP70TYPE map70, lmap, rmap, MtoSource , ypc1, zpc1, 
//            dypc , dzpc, wc, xlc, fdetc, fdetphc, fdet1phc ;
    struct ElementType ElementList[MaximumOptElements] ;
    struct OptionsType BLOptions;
    struct sources src;
    struct PSImageType PSImage;
    struct UndulatorSource0Type UndulatorSrc; // Undu0Src enthaelt alle elemente von norm undu
    struct DipolSourceType DipolSrc;
    struct PointSourceType PointSrc;
    struct SRSourceType SRSrc;
    struct HardEdgeSourceType HardEdgeSrc;
    struct RingSourceType RingSrc;
    struct IDL_STRING 
            *fnamesrc4ezre, *fnamesrc4ezim
           ,*fnamesrc4eyre, *fnamesrc4eyim
           ,*fnamesrc6 
           ,*blfname;
}; 
*/
// *************************************************************************/







// function Prototypes

//int pha4idlWriteBLFile(IDL_STRING *name, struct pha4idlBeamlineFile *bl);

//int pha4idlReadBLFile(IDL_STRING *name, struct pha4idlBeamlineFile *bl);





// ************** Start of drift routines ************************************
// See phaSrc4Drift.f

// /* *** Fortran-Access ***  --- Former Propagate_1
int phaPropWFFresnelKirchhoff (struct source4 *beam4, double *distance, 
					int *nz2, double *zmin2, double *zmax2, 
					int *ny2, double *ymin2, double *ymax2) ;

// /* *** Fortran-Access ***  --- Former Propagate_2
int phaPropFFTnear (struct source4 *beam, double *distance) ;

// /* *** Fortran-Access ***  --- Former Propagate_3
int phaPropFFTfar  (struct source4 *beam, double *distance) ;

// *************** END of drift routines *************************************


/**/
// ***************************************************************************



// ************** Start of pha Src Initializations ***************************
// 
int phaSrcWFGauss (struct source4 *, int *, double *, double *, 
		   int *, double *, double *, 
		   double *, double *, double *,
		   double *, double *, double *);
// 

int phaSrcWFGauss_source4c (struct source4c *, int *, double *, double *,
			    int *, double *, double *,
			    double *, double *, double *,
			    double *,double *, double *);

// *************** END of pha Src Initializations ****************************



// ***************************************************************************



// ************* START of pha Src4 Tools *************************************

int phaModSizeAddZeros (struct source4 *beam, int *nz2, int *ny2);


 
int phaModSizeCut (struct source4 *beam, 
				int *nzmin, int *nzmax, 
				int *nymin, int *nymax);
  
int phaModGrid (struct source4 *beam, int *nz2in, int *ny2in);

int pha_c_extract_src4_grid(struct source4 *src4,
                            int *nz, double *zmin, double *zmax,
				    int *ny, double *ymin, double *ymax);

int pha_c_define_src4_grid_source4c(struct source4c *,int, double, double, int, double, double);
int pha_c_define_src4_grid(struct source4 *src4,
			   int nz, double zmin, double zmax,
			   int ny, double ymin, double ymax);

int pha_c_adjust_src4_grid(struct source4 *src4);

// ************** END of  pha Src4 Tools *************************************

/* UF prototype of the fortran routines should be in the header */
extern void phadrift_propagate_fft_far_nostructs_(double *, double *, double *, double *,
						  int *, double *, double *, int *, double *, double *, 
						  double *, double *);
extern void phadrift_propagate_fft_near_nostructs_(double *, double *, double *, double *,
						   int *, double *, double *, int *, double *, 
						   double *, double *, double *);
extern void phadrift_propagate_fk_nostructs_(double *, double *, double *, double *,
                                             int *, double *, double *, int *, double *, double *, 
                                             int *, double *, double *, int *, double *, double *, 
                                             double *, double *);
extern void phadrift_propagate_fk_oe_nostructs_(double *, double *, double *, double *, 
						int *, double *, double *,
						int *, double *, double *,
						double *, double *, double *, double *, int *,
						const char* , int *,
						int *, double *, double *,
						int *, double *, double *);
extern void phasesrcwfgauss_nostructs_(double *, double *, double *, double *, 
				       int *,    double *, double *, int *, double *, double *,
				       double *, double *, double *, 
				       double *, double *, double *); 
extern void pha_src4_addzeros_nostructs_(double *, double *, double *, double *,
					 int *, int *, double *, double *,
					 int *, int *, double *, double *);
extern void pha_src4_cut_nostructs_(double *, double *, double *, double *, 
				    int *, double *, double *, int *, int *, int *,
				    double *, double *, int *, int *); 
extern void pha_src4_modgrid_nostructs_(double *, double *, double *, double *, 
					int *, int *, double *, double *,
					int *, int *, double *, double *); 
#endif // PHASE4IDL_H_


