/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/phase.h */
/*   Date      : <08 Mar 04 13:35:03 flechsig>  */
/*   Time-stamp: <16 Nov 07 09:15:27 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */

#ifndef PHASE_H
#define PHASE_H   

#define DOALLOC 1
#define NOALLOC 0

/* dont show a watch (work around static variables) */
#define NOWATCH     

#define LIGHT_VELO      2.998e11         /* light velocity in mm/s   */    
/* #define LOGFILE             /* compile with logfile: logfilename  */
#define PHASE_HOME      "PHASE_HOME"     /* name of environment */
#define sourceOK   	1
#define mapOK      	2                /* werden und verknuepft */ 
#define resultOK   	4
#define elementOK  	8
#define geometryOK 	16             

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
#ifdef VMS
  #define HORMAPFILENAMEBASE	"PHASE$LIB:MAP"        
#else
  #define HORMAPFILENAMEBASE	"/lib/map"
#endif
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

#define kFFileButton1           41  
#define kFFileButton2           42  
#define kFFileButton3           43  
#define kFFileButton4           44  
#define kFFileButton5           45  
#define kFFileButton6           46 
#define kFFileButton7           47 
#define kFFileButton8           48 
#define kFFileButton9           49   
#define kFFileButton10          50    
#define kFFileButton11          51    
#define kFFileButton12          52    
#define kFFileButton13          53   
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

#define kESPointSourceButton     360
#define kPreAB                   361
#define kSucAB                   362
#define kMisalignmentButton      363

#define kEOEDrift               999




/******************** end defines uil **********************************/

#define k_max_widget            2000  		/* geaendert von 50 */
#define MAX_WIDGETS (k_max_widget + 1)
#define MaxPathLength           255         
#define MainPickName 		"phase.pck"
#ifdef VMS
  #define	MainListFileName        "PHASE$disk:PHASElist.pck"
#else
  #define	MainListFileName        "MainListFileName"
#endif
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
#ifdef VMS
  #define PHASE_help  		"PHASE$lib:PHASE.hlb"
#else
  #define PHASE_help  		"$PHASE_HOME/lib/phase.hlb"
#endif
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
struct PSSourceType       { double sigy, sigdy, sigz, sigdz;
  int    yhard, dyhard, zhard, dzhard; };         
struct FileSourceType { char *filename; };
    
struct RTSourceType {
  /*  union               	{ 
    struct HardEdgeSourceType  HardEdgeSource; 
    struct UndulatorSourceType UndulatorSource;
    struct UndulatorSource0Type UndulatorSource0;
    struct DipolSourceType     DipolSource;     
    struct SRSourceType        SRSource;
    struct PSImageType         PSImage;
    struct PointSourceType     PointSource;
    struct FileSourceType      FileSource;
    } Quelle; */
  void *Quellep;           /* UF 24.11.06 */
  struct RayType *SourceRays;          
  int QuellTyp, QuellTyp_old, modified, raynumber; };       

struct PSDType  {
  double *y, *z, *psd, *stfd1phmaxc, *stinumbc, *s1c, *s2c, *s3c, 
    *eyrec, *ezrec, *eyimc, *ezimc;
  double simpre[0x8000], simpim[0x8000], sintre[0x8000], sintim[0x8000], 
    simpa[0x8000], simpp[0x8000], d12[24576]; 
  int iy, iz;
};

struct RESULTType {
  /*  union               	{ 
    struct RayType *Rays;  
    struct PSDType PSD; 
    } RESUnion; */
  void *RESp;          /* UF 24.11.06 */
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
  MAP70TYPE map70, lmap, rmap, MtoSource;                          
  MAP7TYPE ypc1, zpc1, dypc, dzpc, wc, xlc, fdetc, fdetphc, fdet1phc;
  struct xlenmaptype xlm; 
  struct RayType *raysout; 
  struct RESULTType RESULT;
  unsigned int beamlineOK, elementzahl, position, hormapsloaded, localalloc; 
  struct OptionsType BLOptions;
  double deltalambdafactor, xlen0;
  struct sources src;
};


int	getpickfile(struct datset *, struct BeamlineType *, char *);  

void putpickfile(struct datset *, struct BeamlineType *, char *),    
  initdatset (struct datset *, struct BeamlineType *);    


/******************** globale Variable *****************************/

Widget
toplevel_widget,            	/* Root widget ID of our application. */
  main_window_widget,		/* Root widget ID of main MRM fetch   */
  widget_array[MAX_WIDGETS],	/* Place to keep all other widget IDs */
  main_help_widget,		/* Primary help widget		      */
  help_widget[MAX_WIDGETS],	/* Array of help widgets	      */
  help_array[MAX_WIDGETS],	/* Array of help widgets for Toolkit  */
  print_widget,			/* Print widget			      */
  color_widget;    		/* Color Mix widget		      */


MrmHierarchy s_MrmHierarchy;	         /* MRM database hierarchy ID */     
MrmType      dummy_class;	    /* %%%*dumm...and class variable. */   
Opaque       help_context;         	/* Global help system context */  

                                                                             
long status;        			/* fuer Stringumwandlung   */
long bc;      

static Cursor watch = 0;
static Screen	*the_screen;		/* Pointer to screen data  */  
static Display	*the_display;		/* Pointer to display data */  
static XColor    savecolor;   
static int help_num = 0;                /* make sure they start zero */   
static int low_num  = 0;
static XmString latin_create;		/* Variables for */
static XmString latin_dismiss;	  	/* compound strings. */
static XmString latin_space;
static XmString latin_zero;
/* #ifdef VMS */
/*  static char *db_filename_vec[] =	/* Mrm.heirachy file list. */     
/*  {"PHASE$prg:PHASE.uid"};	/* uid Namen eintragen*/
                                /* There is only one UID file for */
                                /* this application. */
/* #else */
/* static char *db_filename_vec[] =	/* Mrm.heirachy file list. */     
/*  {"phase.uid"};	*/
/* #endif */

char *db_filename_vec[1];

static int db_filename_num = 1;
/*
(sizeof db_filename_vec / sizeof db_filename_vec [0]); */
                                                                 
double PI;
int ActualTask;         		/* haelt aktuelle Aufgabe fest */    

struct PHASEset PHASESet;  
struct datset Fg3ActDat, Fg3DefDat;  
struct gdatset GActDat,   GDefDat;  
struct mdatset MActDat,   MDefDat;  
struct BeamlineType Beamline;
/*struct optistruct optistructure; */ 
GRDATSTRUCTTYPE grdatstruct;  
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
  #define exithplot      exithplot_ 
  #define extractmap     extractmap_
  #define fdet           fdet_
  #define fgmapidp       fgmapidp_
  #define hlimit         hlimit_
  #define hplint         hplint_
  #define hplotdisplayf  hplotdisplayf_
  #define hplotpsdf      hplotpsdf_
  #define hplotpssimf    hplotpssimf_
  #define inithplot      inithplot_
  #define intersection   intersection_
  #define pathlen0       pathlen0_
  #define pathlen1       pathlen1_
  #define pstf           pstf_ 
  #define readfg34_par   readfg34_par_
  #define readmatrixfile readmatrixfile_
  #define src_ini        src_ini_
  #define xxmap70        xxmap70_ 
  #define misali         misali_
/*  #define already_expired already_expired_ */
#endif
/************************* Prototypen *****************************/

int	  
  GetGrafBox(struct PHASEset *, GRDATSTRUCTTYPE *, int),
  getgrfiletype(char *), 
  GetOElement(),  GetPHASE(struct  PHASEset *, char *),
  iindex(int, int), 
  OnElement(struct mdatset *, double, double), 
  ReadBLFile(char *, struct BeamlineType *), 
  SetFilePos(FILE *, char *);   
 
void    *SetGrDatStruct(char *, struct BeamlineType *, GRDATSTRUCTTYPE *);
 	
void 	ActivateFileSelection(int, char *), 
  activate_proc(),  
  AddBLElement(struct BeamlineType *, char *),
  AutoScale(struct RayType *, GRDATSTRUCTTYPE *, struct BeamlineType *),  
  BatchMode(char *, int, int),
  Beauty(double *, double *), 
  BuildBeamline(struct BeamlineType *),
  DefGeometryC(struct gdatset *, struct geometrytype *),  
  DefMirrorC(struct mdatset *, struct mirrortype *, int),  
  DelBLElement(struct BeamlineType *bl),  
  exithplot(), 
  ExpandFileNames(),  
  FetchWidget(int, char *),   
  fgmapidp(int *, 
	   int *, double *, struct mirrortype *, struct geometrytype *,
	   MAP7TYPE, MAP7TYPE, MAP7TYPE, MAP7TYPE, MAP7TYPE, MAP7TYPE),
  FixFocus(double, double, double, int, double *, double *),
  Footprint(struct BeamlineType *, int),
  GeneratePrintDataFile(),
  GetBLBox(char *, struct BeamlineType *),  
  
  GetOptiBox(struct PHASEset *), 
  GetGeometry(), 
  GetSlope(struct ElementType *), 
  GetSource(struct BeamlineType *), 
  GlueLeft(double *, double *), 
  GlueXlen(struct xlenmaptype *, struct xlenmaptype *, double *, int *, int), 
  GlueWcXlc(double *, double *, double *, double *, double *, int *),
  gpd(), 
  GetUserName(), 
  hplint(int *), 
  hplotdisplay(struct BeamlineType *, GRDATSTRUCTTYPE *, 
	       struct PHASEset *, struct RayType *), 

  hlimit(int *),
  hplotpsdc(struct BeamlineType *, GRDATSTRUCTTYPE *, 
	    struct PHASEset *, struct PSDType *, double *),
  hplotpssimc(struct BeamlineType *, GRDATSTRUCTTYPE *, 
	      struct PHASEset *, double *), 
  
  InitBLBox(char *, struct BeamlineType *), 
  intersection(struct mirrortype *, MAP7TYPE, MAP7TYPE, 
	       struct RayType *, int *,double *, double *, double *),   
  init_application(),  
  initconstants(),  
  InitDataSets(struct PHASEset *, char *),   
  InitFileBox(struct PHASEset *),  
  inithplotc(int),
  InitOElementBox(struct mdatset *, struct gdatset *, int), 
  InitOptiBox(char *, struct BeamlineType *),
  InitOptiList2(int, char *),
  InitParameterBox(struct BeamlineType *, char *), 
  InitSourceBox(struct datset *, struct BeamlineType *),
  InitSourceType(struct BeamlineType *, int),  
  InitGeometryBox(struct gdatset *), 
  InitGrafBox(struct PHASEset *, GRDATSTRUCTTYPE *),                     
  InitPHASE(struct PHASEset *), 
  LoadHorMaps(struct BeamlineType *, int), 
  MakeMapandMatrix(struct ElementType *, struct BeamlineType *),
  MultiplyMatrix(), 
  pathlen0(struct mirrortype *, struct geometrytype *, int *, int *, int *,
	   MAP7TYPE, MAP7TYPE, MAP7TYPE, MAP7TYPE, struct xlenmaptype *), 
  pathlen1(struct xlenmaptype *, struct RayType *, int *, 
	   double *, double *, double *), 
  PrintFileInMainList(char *),    
  PST(struct BeamlineType *), 
  adaptive_int(),
  PutPHASE(struct  PHASEset *, char *), 
  RayTraceFull(struct BeamlineType *), 
  RayTraceSingleRay(struct BeamlineType *),
  ReadCoefficientFile(double *, char *),
  ReadpsFile(char *, struct RESULTType *),
  ReadRayFile(char *, int *, struct RESULTType *), 
  SetDefaultParameter(struct BeamlineType *),
  SetDeltaLambda(struct BeamlineType *, struct ElementType *),
  SetInfoString(),
  SetOElementBoxSensitivity(int),
  Slope(struct RayType *, double, double, double, double, int),
  UpdateBLBox(struct BeamlineType *, int),  
  UpdateFilenames(struct PHASEset *),
  MMatrix(),
	        
  xprintf(char *),
  UpdateMainList(),  
  WriteBLFile(char *, struct BeamlineType *),
  writemapc(char *, int, double *, double *, double *, double *,
	    double *, double *, double *, double *),  
  WritePsd(char *, struct PSDType *, int, int),     
  get_something(),
  set_something(),                         
  SetIndexField(int *, int,...),  
  SetRadius(int),
	      
  s_error(),  
  start_watch(), 
  stop_watch(),
  help_error(),
  create_help(),
  tracking_help(), 
  sens_help_proc(), 
  help_system_proc(),
  create_proc(),
  create_print(),
  activate_print(),
  create_color(),
  ok_color_proc(),
  apply_color_proc(),
  cancel_color_proc(),
  xmstring_append(),
  exit_proc(),
  list_proc(),
  toggle_proc(),
  SelectionProc(Widget, int *, XmSelectionBoxCallbackStruct *), 
  FileSelectionProc(Widget, int *,        /*callback der Fileselection*/ 
		    XmFileSelectionBoxCallbackStruct *); 

extern void inithplot(),    
#ifdef VMS
  hplotdisplayf(GRDATSTRUCTTYPE *,
		struct RayType *, double*, double*, FString *, FString *);  
#else 
  hplotdisplayf();
#endif
/* The names and addresses of things that Mrm has to bind.  The names do
 * not have to be in alphabetical command.  */

/* wird auf dem Laptop benoetigt */

#ifdef CADDR_T
/* #define caddr_t "char *" */
   typedef XtPointer caddr_t;
#endif

static MrmRegisterArg reglist[] = {
  {"activate_proc",     (caddr_t) activate_proc}, 
  {"create_proc",       (caddr_t) create_proc}, 
  {"exit_proc",         (caddr_t) exit_proc},   
  {"list_proc",         (caddr_t) list_proc},   
  {"sens_help_proc",    (caddr_t) sens_help_proc},
  {"help_system_proc",  (caddr_t) help_system_proc},
  {"cancel_color_proc", (caddr_t) cancel_color_proc},
  {"apply_color_proc",  (caddr_t) apply_color_proc},
  {"ok_color_proc",     (caddr_t) ok_color_proc},
  {"FileSelectionProc", (caddr_t) FileSelectionProc},
  {"SelectionProc", 	  (caddr_t) SelectionProc},   
  {"toggle_proc", 	  (caddr_t) toggle_proc}
};

static int reglist_num = (sizeof reglist / sizeof reglist [0]);
static font_unit = 400;

/*
 * OS transfer point.  The main routine does all the one-time setup and
 * then calls XtMainLoop. 
 */

static String fallback[] =  {
  "PHASE.title	: PHASE (fallback)",                      
  "PHASE.x		: 15",
  "PHASE.y		: 40",
  "*fontlist :-Adobe-Times-medium-R-Normal--*-180-*-*-P-*-ISO8859-1=normal,-Adobe-Times-medium-o-Normal--*-180-*-*-P-*-ISO8859-1=italic,-Adobe-Times-medium-R-Normal--*-180-*-*-P-*-ISO8859-7=greek,-Adobe-Times-Bold-R-Normal--*-180-*-*-P-*-ISO8859-1=bold", 
  "*MainList.rows	: 5",
  "*MainList.columns : 60",  
  (char *) NULL};


#endif  /* PHASE_H */      
