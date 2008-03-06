
//
// pha4idl_prototypes.h
//
// (c) 2008 : Torsten.Leitner@email.de
//


#ifndef PHASE4IDL_H_
#define PHASE4IDL_H_

#include <idl_export.h>
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

// /*
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
// *************************************************************************/







// function Prototypes

int pha4idlWriteBLFile(IDL_STRING *name, struct pha4idlBeamlineFile *bl);

int pha4idlReadBLFile(IDL_STRING *name, struct pha4idlBeamlineFile *bl);





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



// ***************************************************************************



// ************** Start of pha Src Initializations ***************************
// /*
int phaSrcWFGauss (struct source4 *beam, int *ianzz, double *zmin, double *zmax, 
                                    int *ianzy, double *ymin, double *ymax, 
						double *w0, double *deltax, double *xlambda) ;
// */
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



int pha_c_define_src4_grid(struct source4 *src4,
                            int nz, double zmin, double zmax,
				    int ny, double ymin, double ymax);



int pha_c_adjust_src4_grid(struct source4 *src4);

// ************** END of  pha Src4 Tools *************************************




#endif // PHASE4IDL_H_


