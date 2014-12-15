/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/phase.h */
/*   Date      : <08 Mar 04 13:35:03 flechsig>  */
/*   Time-stamp: <15 Dec 14 14:08:38 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */

// ******************************************************************************
//
//   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
//                      Paul Scherrer Institut Villigen, Switzerland
//   
//   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
//          Uwe Flechsig,    uwe.flechsig@psi.ch
//
// ------------------------------------------------------------------------------
//
//   This file is part of PHASE.
//
//   PHASE is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, version 3 of the License, or
//   (at your option) any later version.
//
//   PHASE is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
//
// ******************************************************************************

#ifndef PHASE_H
#define PHASE_H   

#define ON  1
#define OFF 0

#define DefGeometryC DefGeometryC_UF
//#define DefGeometryC DefGeometryC_JB

#define FIRST  1
#define SECOND 2

#define DOALLOC 1
#define NOALLOC 0

/*#define ZERO            1e-30    */        /* a small number, 3.4.13 for double it can be as low as 1.7e-308 */
#define ZERO            2e-308           /* a small number, 3.4.13 for double it can be as low as 1.7e-308 */
#define LIGHT_VELO      2.998e11         /* light velocity in mm/s   */   
#define PI 3.141592653589793238462643383279502884197169399375105820974944592   /* double precision */
#define VAC_IMPEDANCE   377.0              /* vacuum impedance z0; E^2 = z0 * Intensity (377 Ohm)  */
           
#define NEWARTBIT          1024             /* bit for new type of optical element in Art */
#define GRATINGBIT         4096             /* grating bit */
#define VLSBIT             8192             /* VLS bit */

/* #define LOGFILE              compile with logfile: logfilename  */
#define PHASE_HOME      "PHASE_HOME"     /* name of environment */
#define sourceOK   	1
#define mapOK      	2                /* werden und verknuepft */ 
#define resultOK   	4
#define elementOK  	8
/* #define geometryOK 	16  */           /* no longer used */
#define pstsourceOK     32
#define pstimageOK      64  

#define RTMod   		1        /* CalcMod */
#define PSMod      		2        /* werden und verknuepft */ 
#define SourceToImageMod  	4
#define FootPrintMod	  	8
#define PhaseMod	       16             
#define ClipMod	       	       32
#define SlopeMod	       64   

#define PLdatfromfile           0x100   /* 256 */   
#define PLsource                0x200   /* 512 */   
#define PLrttype                0x400   /* 1024 */  
#define PLphspacetype           0x800   /* 2048 */ 
#define PLautoscale            0x1000   /* 4096 */
#define PLfd1phmax             0x2000   /* 8192 */
#define PLinumb                0x4000   /* 16384 */
#define PLS1                   0x8000   /*  */
#define PLS2                  0x10000   /*  */
#define PLS3                  0x20000   /*  */
#define PLEyreal              0x40000   /*  */
#define PLEzreal              0x80000   /*  */
#define PLEyimag             0x100000   /*  */
#define PLEzimag             0x200000   /*  */
#define PLsimpre             0x400000   /*  */
#define PLsimpim             0x800000   /*  */
#define PLsintre            0x1000000   /*  */
#define PLsintim            0x2000000   /*  */
#define PLsimpam            0x4000000   /*  */
#define PLsimpph            0x8000000   /*  */
#define PLd12              0x10000000   /*  */

#define PLRaySet1                   1
#define PLRaySet2                   2
#define HORMAPFILENAMEBASE	"/lib/map"

/*******************  fileheader *******************************/

#define MainPickFileHeader	"MainPickFileType"        
#define Fg3PickFileHeader	"Fg3PickFileType"        /* auch in f*pck.h */
#define OptiPickFileHeader	"OptiPickFileType"       /* auch in ph*ti.h */
#define RTPlotFileHeader	"RTPlotFileType" 
#define RayFileHeader		"RayFileType"  
/*  */

#define kESRayTraceButton       61
#define kESPhaseSpaceButton     62 
#define kESFileButton           63
#define kESSR2Button            64 
#define kEOEMenu                65   
#define kEOETM                  66     
#define kEOETG                  67   
#define kEOEVLSG                68     
#define kEGeometry              69  
#define kESPhaseSpaceImageButton     70 
#define kEParameterBox	  	71
#define kEParameterButton       72 
#define kEOptiButton	        73       
#define kESDipolSourceButton	74       
#define kESUndulatorSourceButton 75  
#define kFPFBox			76
#define kInfoMenuEntry          77 
#define kEOEElli                78
#define kEOEPElli               79
#define kEOESlit                99
#define kEOEAuto                100
#define kEOEFourier             101
#define kEOEFresnel             102
#define kEOEFraunhofer          103


#define kESundulatorSourceButton 107      
#define kESUndulatorSISButton   108 
#define kESUndulatorSIMButton   109 
#define kESUndulatorButton      110
#define kESourceMenuButton      132   
#define kESDefaults         	143 
#define        kEOElement                 156   
#define        kEOOptMenu	          192
#define        kEOEAB2                    193     /*! noch opt. Element  */
#define        kEOEAB4                    194                             

#define        kEOEInputLabel         	  195 
#define        kEOElementButton      	  196  


#define kEOEGeneral              350
#define kEOECone                 351
#define kEOEPElliG               352
#define kEOEPM                   353
#define kEOEPG                   354
#define kEOEPGV                  355

#define kCWriteMCoButton         358
#define kESRingSourceButton      359
#define kESPointSourceButton     360
#define kPreAB                   361
#define kSucAB                   362
#define kMisalignmentButton      363
#define kEOEDrift                999   //


/* optimization methodes, I do not want to use the defines of the buttons twice */
#define OptiR       0
#define OptiY       1
#define OptiZ       2
#define OptiTrans   3
#define OptiFocus   4
#define OptiCost    5
#define OptiRpY     6
#define OptiRpZ     7 


#define MaxPathLength           255         
#define MainPickName 		"phase.pck"

#define	MainListFileName        "MainListFileName"

#define D0matrixname		"test.omx"     
#define D0mapname		"test.map"     
#define D0sourceraysname 	"test.inp"     
#define D0imageraysname		"test.out"     
#define D0intersecname		"test.dat"   
#define D0geometryname		"test.dat"   
#define D0elementname   	"test.dat"      
#define D0sourcepckname 	"test.pck"     
#define D0geometrypckname 	"test.pck" 
#define D0elementpckname 	"test.pck" 
#define D0pssourcename 		"test.sour"      
#define D0plotpsname            "pmeta.ps"
#define D0printpclname          "test.pcl"   
#define D0optipckname           "test.pcko"   


/********************** Strukturen **************************************/
           
struct PHASEset                       /* Datensatz in MainPickName 	*/
{
  char matrixname[MaxPathLength];
  char mapname[MaxPathLength];    
  char sourceraysname[MaxPathLength];    
  char imageraysname[MaxPathLength];	   
  char intersecname[MaxPathLength];
  char geometryname[MaxPathLength];   
  char elementname[MaxPathLength];  
  char sourcepckname[MaxPathLength];  
  char geometrypckname[MaxPathLength];  
  char elementpckname[MaxPathLength];    
  char pssourcename[MaxPathLength];   
  char plotpsname[MaxPathLength]; 
  char printpclname[MaxPathLength];   
  char optipckname[MaxPathLength];   
  char beamlinename[MaxPathLength];
  char so4_fsource4a[MaxPathLength];
  char so4_fsource4b[MaxPathLength];
  char so4_fsource4c[MaxPathLength];
  char so4_fsource4d[MaxPathLength];
  char so6_fsource6[MaxPathLength];
  char so7_hdf5[MaxPathLength];
  char opresname[MaxPathLength];
  char hdf5_out[MaxPathLength]; 
  char h5surfacename[MaxPathLength]; 
};                                                                   

typedef double MAPTYPE_330X2 [330][330];
typedef double MAPTYPE_8X4   [8][8][8][8];
typedef double MAPTYPE_8X6   [8][8][8][8][8][8];
typedef double MAPTYPE_70X2  [70][70];  
typedef double MAPTYPE_5X4   [5][5][5][5]; 

#ifdef SEVEN_ORDER
  typedef double MAP70TYPE    [330][330];  
  typedef double MAP7TYPE     [8][8][8][8];
#else
  typedef double MAP70TYPE    [70][70];  
  typedef double MAP7TYPE     [5][5][5][5]; 
#endif

typedef struct grdatstructtype {
  double ymi,yma,zmi,zma,dymi,dyma,dzmi,dzma,tmi,tma;
  int    zeilen, psfile, status, plotstyle, iykanal, izkanal;
  char   titel[80];
} GRDATSTRUCTTYPE;  

struct mirrortype {
#ifdef SEVEN_ORDER
  double a[9][9];
#else
  double a[6][6];
#endif
};

/* in phase_struct.h ist die analoge structur geometryst */
struct geometrytype {
  double sina, cosa, sinb, cosb, r, rp, x[5], xlam;
  int idefl;
}; 

struct gdatset 
{
  double theta0;
  double r;
  double rp; 
  double xdens[5];  
  //double lambda; //TODO: i think this is the same as lambdag, should be removed
  //double lambdag; 
  //double dlambda;
  //int    el_dlambdaflag;
  int    inout;                                 
  int    iflag; 
  int    azimut;     /* vertikal 0; nach links 1; nach unten 2 */
}; 

struct grating
{
    double alpha, beta, lambda, n[5], r, rho;
    int    order;
};

struct mdatset 
{
  double r1;
  double r2;
  /*double alpha; */       
  double rmi;        
  double rho;  
  int    iflagmi;
  double w1, w2, l1, l2;
  double slopew, slopel;
  double du, dw, dl, dRu, dRw, dRl;
  int Art;           /* UF 9.7.04 */
  char material[10]; /* UF 8.5.14 */
};  

struct xlenmaptype {
#ifdef SEVEN_ORDER
  MAPTYPE_8X4 xlen1c, xlen2c;
#else
  MAPTYPE_5X4 xlen1c, xlen2c;
#endif
};

struct RayType { double y, z, dy, dz, phi; };  
struct UndulatorSourceType { double length, lambda, sigvert, sighor, deltaz;};  
struct UndulatorSource0Type { double length, lambda, sigvert, sighor, deltaz,
   sigmaez, sigmaey, sigmaedz, sigmaedy; };  
struct DipolSourceType     { double sigy, sigdy, sigz, dz; }; 
struct PointSourceType     { double sigy, sigdy, sigz, sigdz; };
struct RingSourceType      { double dy, dz; }; 
struct SRSourceType        { double y, z, dy, dz; };   
struct HardEdgeSourceType  { double disty, distz, divy, divz;
  int    iy, iz, idy, idz; };  
struct PSImageType         { double ymin, ymax, zmin, zmax;
  int    iy, iz; };         
struct PSSourceType       { double sigy, sigdy, sigz, sigdz;
  int    yhard, dyhard, zhard, dzhard; };         
struct FileSourceType { char filename[MaxPathLength]; };
    
struct RTSourceType {
  void *Quellep;          
  struct RayType *SourceRays;          
  int QuellTyp_old, modified, raynumber; 
  char QuellTyp;
};       


//struct PSDType  {
  //  double *y, *z; //*stfd1phmaxc, *stinumbc,  //*psd, *s1c, *s2c, *s3c, 
  //*eyrec, *ezrec, *eyimc, *ezimc;
//  double simpre[MAX_INTEGRATION_SIZE*2*4], simpim[MAX_INTEGRATION_SIZE*2*4], 
//  sintre[MAX_INTEGRATION_SIZE*2*4], sintim[MAX_INTEGRATION_SIZE*2*4], 
//  simpa[MAX_INTEGRATION_SIZE*2*4], simpp[MAX_INTEGRATION_SIZE*2*4];
    //;;, d12[MAX_INTEGRATION_SIZE*2*3];
  //  int iy, iz;
  //long outside_wl;
//};


struct RESULTType {
  void *RESp;         
  int  typ, points1, points2, dim1; 
  long outside_wl;
};  

struct ReflecType {
  //  char material[10];
  double ryamp, rypha, rzamp, rzpha, runpol;
};

struct SurfaceType {
  double *u, *w, *l;
  int nw, nl;
};

struct Spa3TableType {
  unsigned int datapoints;
  double dx;
  double *tab;
};       

struct datset 
{
  int itrans;     
  int idir;
  int imodus;
  int iheigh; 
  int iwidth;  
  int idivy;
  int idivz;   
  int intmod;  
  int isourcefile;                                               
  int iord, isrcy, isrcdy, inumy, itery0, ianzy0, imaxy; 
  int       isrcz, isrcdz, inumz, iterz0, ianzz0, imaxz; 

  double disty;
  double distz;
  double divy;
  double divz;
  double disty1;
  double disty2;        
  double distz1;        
  double distz2;        
  double yi;            
  double zi;            
  double dyi;           
  double dzi;           
  double w;             
  double xl;
  double xlam_test;
  double sigmay, sigmayp, ymin, ymax, sigmaz, sigmazp, zmin, zmax; 
  double epsilon, fracy, frac1y, fracz, frac1z;
  struct RayType SR2out;    
  }; 

struct ElementType   
{
  int ElementOK;
  MAP70TYPE M_StoI, M_ItoS;
  MAP7TYPE ypc1, zpc1, dypc, dzpc, wc, xlc; 
  struct xlenmaptype xlm; 
  struct mirrortype mir;
  struct mdatset MDat;                           
  struct geometrytype geo; 
  struct gdatset GDat;
  char elementname[MaxPathLength];
  struct TmpMapType *tpe;
  struct ReflecType reflec;
  struct SurfaceType surf;
};

struct PSOptionsType                   /* 20.9.96 */
{
  int    intmod, ndyfix, ndzfix, with_coating, with_herror, iconj;
  struct PSSourceType PSSource;
  double dyminfix, dymaxfix, dzminfix, dzmaxfix;
};     

struct OptionsType                   			/* Jun 2012 add ray_sets and deltalambda */
{
  int SourcetoImage, wrMatrix, CalcMod, wrSource, WithAlign, REDUCE_maps, act_ray_set, dlambdaflag, 
    plrayset, need_another_run;
  double epsilon, lambda, xlam_save, displength, dlambda;
  struct PSOptionsType PSO;
  struct control_flags ifl;
  struct apertures apr;
  struct integration xi;
};    

// basic electromagnetic field
struct EmfType
{
  int    nz, ny;
  double *y, *z, *eyre, *eyim, *ezre, *ezim;
};
                                            
struct BeamlineType
{
  struct ElementType *ElementList;   
  struct RTSourceType RTSource; 
  MAP70TYPE lmap, rmap, M_StoI, M_ItoS;                          
  MAP7TYPE ypc1, zpc1, dypc, dzpc;
  MAP7TYPE wc, xlc, fdetc, fdetphc, fdet1phc, fdet1phca, fdet1phcb;
  struct xlenmaptype xlm; 
  struct RayType *raysout; 
  struct RESULTType RESULT;
  unsigned int beamlineOK, elementzahl, position, hormapsloaded, localalloc, gratingpos, isrctype_c; 
  struct OptionsType BLOptions;
  double deltalambdafactor, xlen0;
  
  //struct source4c posrc;   /* 120423 */
  struct source1c poso1c;
  struct TmpMapType *tp;
  struct PHASEset filenames;
  struct Spa3TableType spa3table;
  struct EmfType *emfp, *source_emfp, *result_emfp;  /* new for po */
  double *int_details, *simpre, *simpim, *sintre, *sintim;
};

/* mod 20121102 */
struct optistruct
{
  double dx, dy, chistart, chistop, *start, *step, *min, *max;
  int elementzahl, xpoints, ypoints, npars, 
    xindex, yindex, *parindex, methode, fcncall;   
  char beamlinefilename[MaxPathLength], 
    minuitfilename[MaxPathLength],  
    resultfilename[MaxPathLength],
    optiblfilename[MaxPathLength], 
    *parnames;    
  FILE 	 *filepointer;
};  

struct TmpMapType
{
  MAPTYPE_8X6 opl6, dfdw6, dfdl6, dfdww6, dfdwl6, dfdll6, dfdwww6, dfdwidlj;
  MAPTYPE_8X4 dfdww, dfdwl, dfdll, ypc, zpc ; 
};

int getpickfile(struct datset *, struct BeamlineType *, char *);  

void putpickfile(struct datset *, struct BeamlineType *, char *),    
  initdatset (struct datset *, struct BeamlineType *);    

/********* FORTRAN calls ******************************************/
/* in der CERN lib fuer LINUX werden werden FORTRAN Symbole mit einem 
   underscore am Ende abgelegt. Man muss daher auch beim compilieren 
   der eigenen Routinen den entsprechenden Compiler Schalter fuer 
   trailing underscore setzen. Bei c nach FORTRAN calls werden im 
   Folgenden mit Makros die Routinennamen umdefiniert, d. h. alle 
   FORTRAN Routinen die aus c gerufen werden muessen hier erscheinen.
*/
#ifdef LINUX
  #define adaptive_int   adaptive_int_ 
  #define debug_beamline_type_f debug_beamline_type_f_
  #define exithplot      exithplot_ 
  #define extractmap     extractmap_
  #define idnum          idnum_
  #define hlimit         hlimit_
  #define hplint         hplint_
  #define hplotdisplayf  hplotdisplayf_
  #define hplotpsdf      hplotpsdf_
  #define hplotpssimf    hplotpssimf_
  #define inithplot      inithplot_
  
  #define pathlen0       pathlen0_
  #define pathlen1       pathlen1_
  
  #define readfg34_par   readfg34_par_
  #define readmatrixfile readmatrixfile_
  #define src_ini        src_ini_
  #define create_hormap  create_hormap_
  
  #define fdet_8          fdet_8_
  #define fgmapidp_8      fgmapidp_8_
  #define initconstants   initconstants_
  #define make_matrix_8   make_matrix_8_
  #define map7to4         map7to4_
  #define map4to7         map4to7_
  #define mat4to7         mat4to7_
  #define mat7to4         mat7to4_
  #define matrix_dim      matrix_dim_
  #define mirror7to4      mirror7to4_
  #define mirror4to7      mirror4to7_
  #define misali_8        misali_8_
  #define elli_8          elli_8_

  #define fdet_4         fdet_4_
  #define fgmapidp_4     fgmapidp_4_
  #define intersection   intersection_
  #define misali_4       misali_4_
  #define writematrixfile writematrixfile_
  #define xxmap70        xxmap70_ 

/*  #define already_expired already_expired_ */
#endif
/************************* Prototypen *****************************/
int  adaptive_int(struct map4 *, struct geometryst *, int *, struct apertures *, 
		  struct constants *, struct rayst *, struct control_flags *, struct integration *, 
		  struct integration_results *, struct psimagest *, int *, int *);
int  CheckBLOK(int, int, char*); 
int  BuildBeamline(struct BeamlineType *);
int  getgrfiletype(char *);
int  GetOElement();
int  GetPHASE(struct  PHASEset *, char *);
int  iindex(int, int);
void InitBeamline(struct BeamlineType *);
int  OnElement(struct mdatset *, double, double);
int  ProcComandLine(struct PHASEset *, int, char **, int *,  int *, int *, int *, int *);
int  ReadBLFile(char *, struct BeamlineType *);
int  SetFilePos(FILE *, char *);
 
void *SetGrDatStruct(char *, struct BeamlineType *, GRDATSTRUCTTYPE *);
int  StackTest(); 


	
void 	  
  
  AutoScale(struct RayType *, GRDATSTRUCTTYPE *, struct BeamlineType *),  
  BatchMode(struct BeamlineType *, int, int, int, int, int),
  Beauty(double *, double *), 
  
  BuildElement(unsigned int, struct BeamlineType *),
  Check_iord(struct BeamlineType *),
  create_hormap(),
  debug_beamline_type_f(int *),
  debug_beamline_type_c_(int *),
  DefGeometryC_JB(struct gdatset *, struct geometrytype *, struct OptionsType *),
  DefGeometryC_UF(struct gdatset *, struct geometrytype *, struct OptionsType *),
  DefGeometryCM(double, double, struct gdatset *, struct geometrytype *),
  DefMirrorC(struct mdatset *, struct mirrortype *, int, double, int, int, int),  
  elli_8(),
  ExpandFileNames(),
  extractmap(),
  fdet_8(MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4,
         MAPTYPE_8X4, MAPTYPE_8X4, 
	 MAPTYPE_8X6, MAPTYPE_8X6, MAPTYPE_8X6, MAPTYPE_8X6, 
	 MAPTYPE_8X6, MAPTYPE_8X6, MAPTYPE_8X6, MAPTYPE_8X6, 
	 MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4,
	 MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4,
	 struct geometrytype *, int *, int *, int *),
  fgmapidp(int *, 
	   int *, double *, struct mirrortype *, struct geometrytype *,
	   MAP7TYPE, MAP7TYPE, MAP7TYPE, MAP7TYPE, MAP7TYPE, MAP7TYPE),
  fgmapidp_8(double *,  
	     MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4, 
             MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4, 
	     struct xlenmaptype *, 
	     MAPTYPE_8X6, MAPTYPE_8X6, MAPTYPE_8X6, MAPTYPE_8X6, MAPTYPE_8X6, MAPTYPE_8X6, MAPTYPE_8X6,
	     MAPTYPE_8X6, MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4, 
	     struct mirrortype *, struct geometrytype *, int *, int *, int *),
  
  
  FixFocus(double, double, double, int, double *, double *),
  Footprint(struct BeamlineType *, unsigned int),
  GeneratePrintDataFile(),
  getoptipickfile(struct optistruct *, char *),
  ginitdatset (struct gdatset *),
  GlueLeft(double *, double *, int *), 
  GlueXlen(struct xlenmaptype *, struct xlenmaptype *, double *, int *, int), 
  GlueWcXlc(double *, double *, double *, double *, double *, int *),
  gpd(), 
  GetUserName(), 
  idnum(int *, int *, int *, int *, int *),
  intersection(struct mirrortype *, MAP7TYPE, MAP7TYPE, 
	       struct RayType *, double *, double *, double *, int *),   
  intersection_8(struct mirrortype *, MAP7TYPE, MAP7TYPE, 
	       struct RayType *, double *, double *, double *, int *),  
  initconstants(struct  constants *),  
  InitDataSets(struct BeamlineType *, char *),   
  InitSourceType(struct BeamlineType *, int),  
                       
  InitPHASE(struct PHASEset *), 
  LoadHorMaps(struct BeamlineType *, int), 
  MakeHorMaps(struct BeamlineType *),
  MakeMapandMatrix(struct ElementType *, struct BeamlineType *, int *),
  make_matrix_8(),
  map4to7(),
  map7to4(),
  mat4to7(),
  mat7to4(),
  matrix_dim(int *, int *),
  minitdatset (struct mdatset *),
  mirror4to7(),
  mirror7to4(),
  misali_8(),
  MultiplyMatrix(), 
/* pathlen0(struct mirrortype *, struct geometrytype *, int *, int *, int *,
   MAP7TYPE, MAP7TYPE, MAP7TYPE, MAP7TYPE, struct xlenmaptype *), */

  FindIntRange(struct BeamlineType *),

  MPST(struct BeamlineType *), 
  
  pathlen0(),
  pathlen1(struct xlenmaptype *, struct RayType *, int *, 
	   double *, double *, double *), 
     
  PST(struct BeamlineType *), 
  
  
// keep this
/*  adaptive_int(struct map4 *, struct geometryst *, struct sources *, struct apertures *, 
	       struct constants *, struct rayst *, struct control_flags *, struct integration *, 
	       struct integration_results *, struct statistics *, struct psimagest *, int *), */
/*
  adaptive_int(struct map4 *, struct geometryst *, struct sources *, struct apertures *, 
	       struct constants *, struct rayst *, struct control_flags *, struct integration *, 
	       struct integration_results *, struct psimagest *, int *),*/
//

  PutPHASE(struct  PHASEset *, char *), 
  RayTraceFull(struct BeamlineType *), 
  RayTraceSingleRay(struct BeamlineType *),
  ReadCoefficientFile(double *, char *),
  readfg34_par(struct sources *, struct apertures *,
	       struct control_flags *,  struct integration *,
	       double *),
  ReadpsFile(char *, struct RESULTType *),
  ReadRayFile(char *, int *, struct RESULTType *), 
  readmatrixfilec(char *, double *, int),
  SetDefaultParameter(struct BeamlineType *),
  SetDeltaLambda(struct BeamlineType *, struct ElementType *),
  Slope(struct RayType *, double, double, double, double, int),
  src_ini(struct sources *),  
  
  MMatrix(),
  UpdateFlags(struct BeamlineType *, int),	        
  WriteBLFile(char *, struct BeamlineType *),
  WriteMKos(struct mirrortype *, char *),
  writemapc(char *, char *, int, double *, double *, double *, double *,
	    double *, double *, double *, double *),
/* writematrixfile(char *, int, char *, int, double *),  */
  writematrixfile(double *, char *, char *, int, int),
       
  
  SetIndexField(int *, int,...);
	      
#endif  /* PHASE_H */      
