/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phasecommon/phasecommon.h */
/*  Date      : <10 Mar 06 08:59:22 flechsig>  */
/*  Time-stamp: <10 Mar 06 09:05:07 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

/* 
   Target: collect all routines and definitions which are 
   independent of the gui
*/
/* uebernommen aus phase.h */      

#define MaxPathLength           255

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

};                                                                   

typedef double MAP70TYPE [70][70];  
typedef double MAP7TYPE [5][5][5][5];    

typedef struct grdatstructtype {
  double ymi,yma,zmi,zma,dymi,dyma,dzmi,dzma,tmi,tma;
  int    zeilen, psfile, status, plotstyle, iykanal, izkanal;
  char   titel[80];
} GRDATSTRUCTTYPE;  

struct mirrortype {
  double a[6][6];
};

/* in phase_struct.h ist die analoge structur geometryst */
struct geometrytype {
  double sina, cosa, sinb, cosb, r, rp, x[5], xlam;
  int idefl;
  }; 


struct xlenmaptype {
  MAP7TYPE xlen1c, xlen2c;
};

struct optistruct_xx
/* modification: 24 Oct 97 15:38:40 flechsig */

{
  char resultfile[MaxPathLength];
  int elementzahl;
  char *fileliste;
  struct mirrortype *mirz;
  struct geometrytype *geoz;
};      

struct RayType { double y, z, dy, dz, phi; };  
/*23.11.98  deltaz neu UF */
struct UndulatorSourceType { double length, lambda, sigvert, sighor, deltaz;};  
struct UndulatorSource0Type { double length, lambda, sigvert, sighor, deltaz,
   sigmaez, sigmaey, sigmaedz, sigmaedy; };  
struct DipolSourceType     { double sigy, sigdy, sigz, dz; }; 
struct PointSourceType     { double sigy, sigdy, sigz, sigdz; }; 
struct SRSourceType        { double y, z, dy, dz; };   
struct HardEdgeSourceType  { double disty, distz, divy, divz;
  int    iy, iz, idy, idz; };  
struct PSImageType         { double ymin, ymax, zmin, zmax;
  int    iy, iz; };         
struct PSSourceType        { double sigy, sigdy, sigz, sigdz;
  int    yhard, dyhard, zhard, dzhard; };         
struct FileSourceType { char *filename; };
    
struct RTSourceType {
  union               	{ 
    struct HardEdgeSourceType  HardEdgeSource; 
    struct UndulatorSourceType UndulatorSource;
    struct UndulatorSource0Type UndulatorSource0;
    struct DipolSourceType     DipolSource;     
    struct SRSourceType        SRSource;
    struct PSImageType         PSImage;
    struct PointSourceType     PointSource;
    struct FileSourceType      FileSource;
  } Quelle;
  struct RayType *SourceRays;          
  int QuellTyp, modified, raynumber; };       

struct PSDType  {
  double *y, *z, *psd, *stfd1phmaxc, *stinumbc, *s1c, *s2c, *s3c, 
    *eyrec, *ezrec, *eyimc, *ezimc;
  double simpre[0x8000], simpim[0x8000], sintre[0x8000], sintim[0x8000], 
    simpa[0x8000], simpp[0x8000], d12[24576]; 
  int iy, iz;
};

struct RESULTType {
  union               	{ 
    struct RayType *Rays;  
    struct PSDType PSD; 
  } RESUnion; 
  int points, typ; };       


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
  MAP70TYPE matrix, MtoSource;
  MAP7TYPE ypc1, zpc1, dypc, dzpc, wc, xlc; 
  struct xlenmaptype xlm; 
  struct mirrortype mir;
  struct mdatset MDat;                           
  struct geometrytype geo; 
  struct gdatset GDat;
  char elementname[MaxPathLength];
};

struct PSOptionsType                   /* 20.9.96 */
/* last modification: 18 Jul 97 15:29:03 flechsig */
{
  int    intmod, ndyfix, ndzfix;
  struct PSSourceType PSSource;
  double dyminfix, dymaxfix, dzminfix, dzmaxfix;
};     

struct OptionsType                   			/* 24.6.96 */
{
  int SourcetoImage, wrMatrix, CalcMod, wrSource, WithAlign;
  double epsilon, lambda, xlam_save, displength;
  struct PSOptionsType PSO;
  struct control_flags ifl;
  struct apertures apr;
  struct integration xi;
};    
                                            
struct BeamlineType
{
  struct ElementType *ElementList;   
  struct RTSourceType RTSource; 
  MAP70TYPE map70, *lmap, *rmap, MtoSource;                          
  MAP7TYPE ypc1, zpc1, dypc, dzpc, wc, xlc, fdetc, fdetphc, fdet1phc;
  struct xlenmaptype xlm; 
  struct RayType *raysout; 
  struct RESULTType RESULT;
  unsigned int beamlineOK, elementzahl, position, hormapsloaded; 
  struct OptionsType BLOptions;
  double deltalambdafactor, xlen0;
  struct sources src;
};

