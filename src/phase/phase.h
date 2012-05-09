/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/phase.h */
/*   Date      : <08 Mar 04 13:35:03 flechsig>  */
/*   Time-stamp: <09 May 12 16:54:48 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */


#ifndef PHASE_H

#define PHASE_H   

#define DOALLOC 1
#define NOALLOC 0

#define ZERO            1e-30            /* a small number */
#define LIGHT_VELO      2.998e11         /* light velocity in mm/s   */   
#define PI 3.141592653589793238462643383279502884197169399375105820974944592   /* double precision */
           
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

#define HORMAPFILENAMEBASE	"/lib/map"

/*******************  fileheader *******************************/

#define MainPickFileHeader	"MainPickFileType"        
#define Fg3PickFileHeader	"Fg3PickFileType"        /* auch in f*pck.h */
#define GeometryPickFileHeader	"GeometryPickFileType"   /* auch in g*pck.h */
#define MirrorPickFileHeader	"MirrorPickFileType"     /* auch in m*pck.h */
#define OptiPickFileHeader	"OptiPickFileType"       /* auch in ph*ti.h */
#define RTPlotFileHeader	"RTPlotFileType" 
#define PSPlotFileHeader	"PSPlotFileType"   
#define RayFileHeader		"RayFileType"  

#define	kMainList		1 
#define	kMenuBar		2   
#define	kFilePDMe		3
#define	kEditPDMe		4       
#define	kCommandPDMe		5   
#define	k_options_pdme		6                
#define	k_help_pdme		7     
#define	k_options_menu		8   
#define	k_create_options	9 
#define	kNyi			10   
#define	kFileMenu		11
#define	kFPrint			12  
#define kFFile                  13
#define kFileSelectionDialog    14   
#define kFileSelectionOk        15   
#define kFileSelectionCancel    16 
#define kFFileBox		17    
#define kFFileBoxLabel		18 
#define kFFileBoxOK	        19  
#define kFFileBoxCancel         20          
#define kFFileLabel1            21  
#define kFFileLabel2            22  
#define kFFileLabel3            23  
#define kFFileLabel4            24  
#define kFFileLabel5            25 
#define kFFileLabel6            26 
#define kFFileLabel7            27      
#define kFFileLabel8            28 
#define kFFileLabel9            29 
#define kFFileLabel10           30             
#define kFSaveAsButton          31             
#define kFLoadButton            32
  
#define kFPFList                54
#define kFPFOK                  55
#define kFPFAdd                 56
#define kFPFDel                 57
#define kFPFCancel              58
#define kSetupInfo              59  
#define	kEditMenu		60 
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

#define	kCommandMenu		80                       
#define	kCProductMatrixButton	81  
#define kCMatrixRTButton	82     
#define	kCMatrixPSButton        83  
#define kCCRayTraceButton       84  
#define	kCCPTButton             85    
#define	kCCMPTButton            1085    
#define	kCCSR1Button            86
#define	kCCSR2Button            87  
#define kCPurge			88  
#define kDirCommand   	 	89   
#define kGraphicCommand  	90 
#define kCCOptiButton 	  	91       
#define kCCExtractButton 	92       
#define kCWriteMapButton        93
#define kCMatrixReadButton      94    

#define kCCSRDialog             95             /*!single Ray Result  */ 
#define kCCOptiDialog           96   
#define kCWriteResultButton     97
#define kEParameterInitButton   98
#define kEOESlit                99

#define kCCGlueBox              100  
#define kCCGResultButton        101   
#define kCCGList                102   
#define kCCGOK                  103   
#define kCCGAdd                 104   
#define kCCGDel                 105   
#define kCCGCancel              106    
#define kESundulatorSourceButton 107      /* low beta */
#define kESUndulatorSISButton   108 
#define kESUndulatorSIMButton   109 
#define kESUndulatorButton      110

#define	k_help_overview		119
#define	k_help_about		120
#define	k_help_onhelp		121
#define	k_help_sensitive	122
#define	k_help_menu		123    
                                 
#define kESourceBox         	131 
#define kESourceMenuButton      132   
#define kEST1                   133 
#define kEST2                   134 
#define kEST3                   135 
#define kEST4                   136 
#define kEST5                   137 
#define kEST6                   138 
#define kEST7                   139 
#define kEST8                   140 
#define kESOK	        	141 
#define kESApply    		142 
#define kESDefaults         	143 
#define kESCancel           	144  
#define kESFile                 145   
#define kESOptMenu              146   

#define kEST1Label              148  
#define kEST2Label              149
#define kEST3Label              150  
#define kEST4Label              151  
#define kEST5Label              152  
#define kEST6Label              153  
#define kEST7Label              154  
#define kEST8Label              155  

/******************** opt. Element     Box **********************************/  

#define        kEOElement                 156      
#define        kEOET1                     157 
#define        kEOET2                     158 
#define        kEOET3                     159 
#define        kEOET4                     160 
#define        kEOET5                     161 
#define        kEOET6                     162 
#define        kEOET7                     163 
#define        kEOET8                     164 
#define        kEOET9                     165
#define        kEOET10                    166
#define        kEOET11                    167
#define        kEOET12                    168  
#define        kEOET13                    169
#define        kEOET14                    170

#define    kEOET15                171
#define    kEOET16                172
#define    kEOET17                173
#define    kEOET18                174
#define    kEOET19                175
#define    kEOET20                176
#define    kEOET21                177
#define    kEOET22                178
#define    kEOET23                179
#define    kEOET24                180
#define    kEOET25                181
#define    kEOET26                182
#define    kEOElementBox          183

#define        kEOEOK	        	  186 
#define        kEOEApply    		  187 
#define        kEOEDefaults         	  188 
#define        kEOECancel           	  189  

#define        kEOOptMenu	          192
#define        kEOEAB2                    193     /*! noch opt. Element  */
#define        kEOEAB4                    194                             

#define        kEOEInputLabel         	  195 
#define        kEOElementButton      	  196  



/************************Geometry Box ***********************/      
#define kEGT1                     197 
#define kEGT2                     198 
#define kEGT3                     199 
#define kEGT4                     200 
#define kEGNITranslation          201 
#define kEGT3Button               202 
#define kEGT7                     203 
#define kEGT8                     204 
#define kEGOK	        	  205 
#define kEGApply    		  206 
#define kEGDefaults         	  207 
#define kEGCancel           	  208  
#define kEGInputLabel         	  209
 
#define kEGeometryBox         	  211  
#define kEGT1Label                212  
#define kEGT2Label                213  
#define kEGT3Label                214  
#define kEGT4Label                215  
#define kEGT5Label                216  
#define kEGT6Label                217  
#define kEGT7Label                218  
#define kEGT8Label                219  

/************************ Graphic Box ***********************/  
#define     kCGrBox		 225
#define     kCGrT5               226
#define     kCGrT1               227
#define     kCGrT2               228
#define     kCGrT3               229
#define     kCGrT4               230   
       
#define     kCGrT2Label          236   
#define     kCGrT4Label          237   
#define     kCGrT5Label          238  
#define     kCGrPSFileButton     239 
#define     kCGrPSFile           240 
#define     kCGrOK               241
#define     kCGrApply            242
#define     kCGrDefaults         243
#define     kCGrCancel           244 


#define     kCGrOptMenu          250  
#define     kCGrOptMenu1         251    

#define     kFFileBoxExpand      270

/************************OPti Dialog ***********************/
                 
#define kCOptiResultButton      	  271   
#define kCOptiList                        272   
#define kCOptiList2                       273   
#define kCOptiAdd                         274   
#define kCOptiDel                         275  
#define kCOptiOK                          276  
#define kCOptiCancel                      277   
#define kCOptiMinuitButton   		  278  
#define kCOptiSelectedLabel 		  279
#define kCOptiList1                       280   
#define kCOptiEditLabel                   281   
#define kCOptiT1                          282   
#define kCOptiT2            		  283 
#define kCOptiAdd1          		  284
#define kCOptiDel1           		  285 
#define kCOptiAddOK          		  286 

#define kEBLButton          		299 
#define kEBLDialog          		300  
#define kEBLNameButton      	        301   
#define kEBLList                        302   
#define kEBLList2                       303   
#define kEBLAdd                         304   
#define kEBLDel                         305  
#define kEBLOK                          306  
#define kEBLCancel                      307   
#define kEBLApply	   		308  
#define kEBLSelectedLabel 		309
#define kEBLList1                       310   
#define kEBLEditLabel                   311   
#define kEBLT1                          312   
#define kEBLT2            		313 
#define kEBLAdd1          		314
#define kEBLDel1           		315 
#define kEBLAddOK          		316 

#define kEBLup			 320  
#define	kEBLleft		 321  
#define kEBLdown		 322  
#define kEBLright		 323        
#define kEBLstoim                324
#define kEBLimtos                325  

#define kEBLT11		 	 330        
#define kEBLT12		 	 331 
#define kEBLT21		 	 332 
#define kEBLT22		 	 333 
#define kEBLT31		 	 334 
#define kEBLT41		 	 335 
#define kEBLT31a		 336 
#define kEBLT41a		 337    

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

#define kCOptiRButton            380
#define kCOptiYButton            381
#define kCOptiZButton            382
#define kCOptiTransButton        383
#define kCOptiFocusButton        384
#define kCOptiCostButton         385
#define kCOptiRpYButton          386
#define kCOptiRpZButton          387

#define kCOptiMenu               390

#define kFFileButton1            401
#define kFFileButton2           402  
#define kFFileButton3           403  
#define kFFileButton4           404  
#define kFFileButton5           405  
#define kFFileButton6           406 
#define kFFileButton7           407 
#define kFFileButton8           408 
#define kFFileButton9           409   
#define kFFileButton10          410    
#define kFFileButton11          411    
#define kFFileButton12          412    
#define kFFileButton13          413
#define kFFileButton14          414    
#define kFFileButton15          415    
#define kFFileButton16          416    
#define kFFileButton17          417
#define kFFileButton18          418

#define kEOEDrift                999


/* optimization methodes, I do not want to use the defines of the buttons twice */
#define OptiR       0
#define OptiY       1
#define OptiZ       2
#define OptiTrans   3
#define OptiFocus   4
#define OptiCost    5
#define OptiRpY     6
#define OptiRpZ     7 

/******************** end defines uil **********************************/

#define k_max_widget            2000  		/* geaendert von 50 */
#define MAX_WIDGETS (k_max_widget + 1)
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

#define PHASE_help  		"$PHASE_HOME/lib/phase.hlb"

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
  char opresname[MaxPathLength];
  char minname[MaxPathLength];
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
  double lambda; 
  double dlambda;
  int    dlambdaflag;
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
  int Art;   /* UF 9.7.04 */
};  

struct xlenmaptype {
#ifdef SEVEN_ORDER
  MAPTYPE_8X4 xlen1c, xlen2c;
#else
  MAPTYPE_5X4 xlen1c, xlen2c;
#endif
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

struct PSDType  {
  double *y, *z, *psd, *stfd1phmaxc, *stinumbc, *s1c, *s2c, *s3c, 
    *eyrec, *ezrec, *eyimc, *ezimc;
  double simpre[4008], simpim[4008], sintre[4008], sintim[4008], 
    simpa[4008], simpp[4008], d12[3006]; 
  int iy, iz;
};

struct RESULTType {
  void *RESp;         
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
  MAP70TYPE M_StoI, M_ItoS;
  MAP7TYPE ypc1, zpc1, dypc, dzpc, wc, xlc; 
  struct xlenmaptype xlm; 
  struct mirrortype mir;
  struct mdatset MDat;                           
  struct geometrytype geo; 
  struct gdatset GDat;
  char elementname[MaxPathLength];
  struct TmpMapType *tpe;
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
  int SourcetoImage, wrMatrix, CalcMod, wrSource, WithAlign, REDUCE_maps, pst_mode;
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
  MAP70TYPE lmap, rmap, M_StoI, M_ItoS;                          
  MAP7TYPE ypc1, zpc1, dypc, dzpc;
  MAP7TYPE wc, xlc, fdetc, fdetphc, fdet1phc, fdet1phca, fdet1phcb;
  struct xlenmaptype xlm; 
  struct RayType *raysout; 
  struct RESULTType RESULT;
  unsigned int beamlineOK, elementzahl, position, hormapsloaded, localalloc; 
  struct OptionsType BLOptions;
  double deltalambdafactor, xlen0;
  struct sources src;
  struct source4c posrc;   /* 120423 */
  struct TmpMapType *tp;
  struct PHASEset filenames;
};

struct optistruct
{
  double dx, dy, chistart, chistop;
  int elementzahl, xpoints, ypoints, npars, 
    xindex, yindex, *parindex, methode, fcncall;   
  char beamlinefilename[MaxPathLength], 
    minuitfilename[MaxPathLength],  
    resultfilename[MaxPathLength],
    optiblfilename[MaxPathLength];    
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


/******************** globale Variable *****************************/

#ifndef QTGUI

int ActualTask;         		/* haelt aktuelle Aufgabe fest */    

#ifdef global
struct PHASEset PHASESet;  
#endif
struct datset Fg3ActDat, Fg3DefDat;  
struct gdatset GActDat,   GDefDat;  
struct mdatset MActDat,   MDefDat;  
struct BeamlineType Beamline;
#endif

/*struct optistruct optistructure; */ 

/*double map35[35][35];  */
/*int pawc[200000]; */

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
  #define exithplot      exithplot_ 
  #define extractmap     extractmap_
  #define hlimit         hlimit_
  #define hplint         hplint_
  #define hplotdisplayf  hplotdisplayf_
  #define hplotpsdf      hplotpsdf_
  #define hplotpssimf    hplotpssimf_
  #define inithplot      inithplot_
  
  #define pathlen0       pathlen0_
  #define pathlen1       pathlen1_
  #define pstf           pstf_ 
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

int	  
  getgrfiletype(char *), 
  GetOElement(),  GetPHASE(struct  PHASEset *, char *),
  iindex(int, int), 
  OnElement(struct mdatset *, double, double), 
  ReadBLFile(char *, struct BeamlineType *), 
  SetFilePos(FILE *, char *),
  CheckBLOK(int, int, char*);   
int ProcComandLine(struct PHASEset *, int, char **, int *,  int *, int *); 
 
void *SetGrDatStruct(char *, struct BeamlineType *, GRDATSTRUCTTYPE *);
 	
void 	  
  
  AutoScale(struct RayType *, GRDATSTRUCTTYPE *, struct BeamlineType *),  
  BatchMode(struct BeamlineType *, int, int, int),
  Beauty(double *, double *), 
  BuildBeamline(struct BeamlineType *),
  BuildElement(int, struct BeamlineType *),
  Check_iord(struct BeamlineType *),
  create_hormap(),
  DefGeometryC(struct gdatset *, struct geometrytype *),
  DefGeometryCM(double, double, struct gdatset *, struct geometrytype *),
  DefMirrorC(struct mdatset *, struct mirrortype *, int, double, int, int, int),  
  elli_8(),
  ExpandFileNames(),
  extractamp(),
  fdet_8(MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4,
         MAPTYPE_8X4, MAPTYPE_8X4, 
	 MAPTYPE_8X6, MAPTYPE_8X6, MAPTYPE_8X6, MAPTYPE_8X6, 
	 MAPTYPE_8X6, MAPTYPE_8X6, MAPTYPE_8X6, MAPTYPE_8X6, 
	 MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4,
	 MAPTYPE_8X4, MAPTYPE_8X4, MAPTYPE_8X4,MAPTYPE_8X4, MAPTYPE_8X4,
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
	     struct mirrortype *, struct geometrytype *, int *, int *, int *, int *),
  fill_m4(struct BeamlineType *, struct map4 *),
  fill_xirp(struct BeamlineType *, struct integration_results *),
  FixFocus(double, double, double, int, double *, double *),
  Footprint(struct BeamlineType *, unsigned int),
  GeneratePrintDataFile(),
  getoptipickfile(struct optistruct *, char *),
  ginitdatset (struct gdatset *),
  GlueLeft(double *, double *), 
  GlueXlen(struct xlenmaptype *, struct xlenmaptype *, double *, int *, int), 
  GlueWcXlc(double *, double *, double *, double *, double *, int *),
  gpd(), 
  GetUserName(), 
  intersection(struct mirrortype *, MAP7TYPE, MAP7TYPE, 
	       struct RayType *, double *, double *, double *, int *),   
  intersection_8(struct mirrortype *, MAP7TYPE, MAP7TYPE, 
	       struct RayType *, double *, double *, double *, int *),  
  initconstants(struct  constants *),  
  init_posrc(struct source4c *),
  InitDataSets(struct BeamlineType *, char *),   
  InitSourceType(struct BeamlineType *, int),  
                       
  InitPHASE(struct PHASEset *), 
  LoadHorMaps(struct BeamlineType *, int), 
  MakeHorMaps(struct BeamlineType *),
  MakeMapandMatrix(struct ElementType *, struct BeamlineType *, unsigned int),
  make_matrix_8(),
  minitdatset (struct mdatset *),
  misali_8(),
  MultiplyMatrix(), 
/* pathlen0(struct mirrortype *, struct geometrytype *, int *, int *, int *,
   MAP7TYPE, MAP7TYPE, MAP7TYPE, MAP7TYPE, struct xlenmaptype *), */

  MPST(struct BeamlineType *), 
  norm_output(struct BeamlineType *),
  pathlen0(),
  pathlen1(struct xlenmaptype *, struct RayType *, int *, 
	   double *, double *, double *), 
     
  PST(struct BeamlineType *), 
  pstc(struct BeamlineType *, struct mirrortype *, struct geometryst *),
  pstc_i(int, struct BeamlineType *, struct map4 *, struct constants *, struct mirrortype *, struct geometryst *),
  pstf(struct PSImageType *psip, struct PSOptionsType *PSO,
       double *lambda, int *iord, 
#ifdef SEVEN_ORDER
       MAPTYPE_8X4 *xlen1c, MAPTYPE_8X4 *xlen2c,        
#else
       MAPTYPE_5X4 *xlen1c, MAPTYPE_8X4 *xlen2c,        
#endif
       double *xlen0, MAP7TYPE *ypc1, MAP7TYPE *zpc1, MAP7TYPE *dypc, MAP7TYPE *dzpc,
       MAP7TYPE *wc, MAP7TYPE *xlc,
       double *y, double *z,
       double *psd, double *stfd1phmaxc,
       double *stinumbc, double *s1c,
       double *s2c, double *s3c,
       double *eyrec, double *ezrec,
       double *eyimc, double *ezimc,
       struct geometryst *gp, struct mirrortype *mirp,
       struct sources *src, struct apertures *apr, struct rayst *ra, struct control_flags *ifl,
       struct integration *xi, struct integration_results *xir, struct statistics *st, 
       MAP7TYPE *fdetc, MAP7TYPE *fdetphc, MAP7TYPE *fdet1phc, MAP7TYPE *fdetphca, MAP7TYPE *fdetphcb),
  

  adaptive_int(struct map4 *, struct geometryst *, struct mirrortype *, struct sources *, struct apertures *, struct constants *, struct rayst *, struct control_flags *, struct integration *, struct integration_results *, struct statistics *, struct psimagest *),
  PutPHASE(struct  PHASEset *, char *), 
  RayTracec(struct BeamlineType *),
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
  Test4Grating(struct BeamlineType *, struct mirrortype **, struct geometryst **),
  MMatrix(),
	        
  WriteBLFile(char *, struct BeamlineType *),
  WriteMKos(struct mirrortype *, char *),
  writemapc(char *, char *, int, double *, double *, double *, double *,
	    double *, double *, double *, double *),
/* writematrixfile(char *, int, char *, int, double *),  */
  writematrixfile(double *, char *, char *, int, int),
  WritePsd(char *, struct PSDType *, int, int),     
  
  SetIndexField(int *, int,...);
	      
#endif  /* PHASE_H */      
