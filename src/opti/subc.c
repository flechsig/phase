/* Datei: USERDISK_3:[FLECHSIG.PHASE.OPTI]SUBC.C               */
/* Datum: 22.JUL.1994                                          */
/* Stand: 20-FEB-1995                                          */
/* Autor: FLECHSIG, BESSY Berlin                               */

#include stdio                                                
#include string 
#include stdlib 
#include math
#include descrip                      /* for FORTRAN- String */    

#include "[-.phasec]cutils.h"  
#include "phaseopti.h"  
   
void getoptipickfile(struct optistruct *x, char *pickname)    
{                              
   FILE *f;
   int ii, *indexlist;
   char *puffer1;      

   if ((f= fopen(pickname, "r")) == NULL)
   {
	fprintf(stderr,"Error: read %s\n", pickname);
	exit(-1);
   }  
   if( CheckFileHeader(f, OptiPickFileHeader) == 0) 
   {
        fscanf(f,"%s\n", &x->readfile); 
       	fscanf(f,"%s\n", &x->resultfile); 
        fscanf(f,"%lf\n", &x->epsilon); 
        fscanf(f,"%d\n", &x->elementzahl);   
       	fscanf(f,"%d %d %lf\n", &x->xindex, &x->xpoints, &x->dx);  
  	fscanf(f,"%d %d %lf\n", &x->yindex, &x->ypoints, &x->dy);  
   	fscanf(f,"%d\n", &x->npars); 
   	 
   	puffer1= x->fileliste= 
			(char *) malloc(2* MaxPathLength* x->elementzahl); 
   	x->parindex= (int *) malloc(x->npars * sizeof(int));
   	if ((puffer1 == NULL) || (x->parindex == NULL))
   	{	
   		fprintf(stderr, "malloc error \n"); exit(-1);  
   	}         /* speicher allocieren */
        
	indexlist= x->parindex;  
   	for (ii= 0; ii< x->npars; ii++, indexlist++)
             fscanf(f,"%d\n", indexlist);  

   	for (ii= 0; ii< (x->elementzahl * 2); ii++)
             fscanf(f,"%s\n", &puffer1[ii* MaxPathLength]);  
   	   	 
        fclose(f); 
   }    else exit(-1); 
}

void fitmir3c(char *name, struct mirrortype *a)   /* fortran Version */
{
   FILE *f;
   extern struct mirrortype mirror;
   FString ffilename;

   if ((f= fopen(name, "r")) == NULL)                                    
   {
	fprintf(stderr,"File %s not found\n", name);
        exit(-1);
   } 
   fclose(f);
   CreateFString(&ffilename, name);     
   mirror4d(&ffilename);            /* fortran initialisiert mirror */
   memcpy(a, &mirror, sizeof(mirror));  
}

void fitgeo3c(char *name, struct geometrytype *a, double *th)
{
   FILE *f;
   int i, j;
   double *k, alpha, beta;

   if ((f= fopen(name, "r")) == NULL)
   {
	fprintf(stderr,"File %s not found\n", name);
        exit(-1);
   } 
   k= (double *) a; 
    
   for (i= 2; i<= 11; i++) fscanf(f,"%lf\n", &k[i]);  
   fscanf(f,"%d\n", &a->idefl);  
   fclose(f);
   alpha= k[2]* pi/180.0;
   beta = k[3]* pi/180.0;
   *th  = 0.5 * a->idefl * fabs(k[3]- k[2]); /* theta */    
   k[0] = sin(alpha); 
   k[1] = cos(alpha); 
   k[2] = sin(beta); 
   k[3] = cos(beta); 
   k[11]*= 1e-6;
}
   
void initoptidata(struct optistruct *x) 
{
   int i, elementzahl;
   struct geometrytype *g;
   struct mirrortype   *m;

   if ((x->filepointer= fopen(x->resultfile, "w")) == NULL)
   {
	fprintf(stderr,"\aError: write %s\n", x->resultfile);
	exit(-1);
   }  
 
   m= x->mirz= (struct mirrortype *) 
                malloc(x->elementzahl* sizeof(struct mirrortype));
   g= x->geoz= (struct geometrytype *) 
                malloc(x->elementzahl* sizeof(struct geometrytype));
   x->thetalist= (double *) 
                malloc(x->elementzahl* sizeof(double)); 
   x->modifylist= (int *) 
                malloc(x->elementzahl* sizeof(int)); 
   x->matrixlist= (double *) 
                malloc((x->elementzahl + 1) * 35 * 35 * sizeof(double)); 

   if ((x->mirz == NULL)      || (x->geoz == NULL) || 
       (x->thetalist == NULL) || (x->modifylist == NULL) ||
       (x->matrixlist == NULL)) 
   {	
	fprintf(stderr, "realloc error ");
        exit(-1);  
   }    /* Speicher allociert */
   for (i= 0; i< x->elementzahl; i++)
   {
        fitgeo3c(&x->fileliste[2* i* MaxPathLength], g, &x->thetalist[i]); 
	fitmir3c(&x->fileliste[(2* i+ 1)* MaxPathLength], m); 
        x->modifylist[i]= 1; 
        g++; m++;
   }  printf("Speicher initialisiert: %d element(e)\n", i);    
   free(x->fileliste);
   printf("init ok\n");    
} 

void in_struct(struct optistruct *x, double *z, int index)
{
   int elnumber, mtype, ipos;
   double *xd, cl;
   struct mirrortype   *m;
   struct geometrytype *g;  

   elnumber= index >> 8;
   mtype   = index & 0x80;
   ipos    = index & 0x7f;
   if (mtype)
   {
       m = &x->mirz[elnumber]; 
       xd= (double *) m;
       switch (ipos)
       {  
           case 36:			/* r */
                if (fabs(*z) > FASTNULL)
                {              
		   xd[2]   = 0.5/ (*z);  
                   xd[4]   = 1.0/8.0/(*z)/(*z)/(*z); 
                   xd[14]  = xd[12]/(2.0* (*z)* (*z) );  
                } else { xd[2]= xd[4]= xd[14]= 0.0;  }    
                break;
           case 37:			/* rp */
		if (fabs(*z) > FASTNULL)
                {              
		   xd[12]  = 0.5/ *z;      
                   xd[14]  = xd[2]*xd[2]/ (*z);  
		   xd[24]  = 1.0/8.0/(*z)/(*z)/(*z); 
                } else { xd[12]= xd[14]= xd[24]= 0.0;  }    
                break;
           default: 
		  xd[ipos] = *z;
       }
   } else
   {
       g = &x->geoz[elnumber]; 
       xd= (double *) g;
       switch (ipos)
       {
       	   case 0:  x->thetalist[elnumber] = *z;	/* theta */
                    modifygstruct(g, z);       
             break;

           case 1:  cl= xd[4]+ xd[5]; /* gesamtlaenge mit Eintritt variabel */
		    xd[5]= cl- (*z); 
                    xd[4]= *z;
             break;

           case 2:  cl= xd[4]+ xd[5]; 
		    xd[4]= cl- (*z);
		    xd[5]= *z; 
             break;

           case 3:  xd[11]= (fabs(*z) > 1e-20) ? (1240.0e-6 / *z) : 1.0e20;
		    modifygstruct(g, &x->thetalist[elnumber]);    /* E in eV */
	     break;  
  
           case 6:   xd[ipos]= *z;            			     /* dens */
                     modifygstruct(g, &x->thetalist[elnumber]);  
             break;  

           case 11:  xd[ipos]= *z*1e-6;       		   /* m lambda in nm */ 
                     modifygstruct(g, &x->thetalist[elnumber]);   
             break;

           case 12:  /* theta CLRCM bei BESSY I dy= 43 mm, Planspiegel in r2 */
                   cl= x->thetalist[elnumber]* pi/180.0;     /* alter winkel */
                   xd[5]+= 43.0 * 
			   ( (1.0 + cos(*z * pi/180.0))/ sin(*z * pi/180.0) - 
			     (1.0 + cos(cl))/ sin(cl) ); 
		   x->thetalist[elnumber] = *z;			    /* theta */
                   modifygstruct(g, z);       
		break;

           case 17: xd[4]= (*z) - xd[5];     /*cl r2 fest*/
		break;
           case 13: xd[5]= (*z) - xd[4];     /*cl r1 fest*/
		break;

           case 14:  x->thetalist[elnumber] = *z;	/* theta sgm vodar*/
                     xd[4]=2300.4* 
          (sin((2.0* (*z)+ 1.12)* pi/180.0)+ sin(1.12* pi/180.0))/ 
           sin((*z)* pi/90.0);
                      modifygstruct(g, z);     
             break;
 	
 	   default:  xd[ipos]= *z;       
	     break;
       }          
   }
   x->modifylist[elnumber]= 1;      /* merken ob geaendert */
} 
    
double out_struct(struct optistruct *x, double *z, int index)    
{
   int elnumber, mtype, ipos;
   double *xd;
   struct mirrortype   *m;
   struct geometrytype *g;  

   elnumber= index >> 8;
   mtype   = index & 0x80;
   ipos    = index & 0x7f;
   if (mtype)
   {
       m = &x->mirz[elnumber];
       xd= (double *) m;
       switch (ipos)
       {  
              case 36:			/* r = 36 */
		    *z= (fabs(xd[2]) > FASTNULL) ? (0.5/ xd[2]): 0.0;      
                break;  
              case 37:			/* rp = 37 */
		    *z= (fabs(xd[12]) > FASTNULL) ? (0.5/ xd[12]): 0.0;  
	        break;
              default: *z= xd[ipos];
       }
   } else
   {
       g = &x->geoz[elnumber]; 
       xd= (double *) g;
       switch (ipos)
       {
       	   case 0:  *z= x->thetalist[elnumber];	 	            /* theta */ 
             break; 
           case 1:   *z= xd[4];                              /* gesamtlaenge */
             break;
           case 2:   *z= xd[5]; 
             break;    
	   case 3:  
		    *z= (fabs(xd[11]) < 1e-20) ? 1e20 :      /* willkuerlich */
				    (1240.0e-6 / xd[11]);    /*      E in eV */
	     break; 

           case 11: *z= xd[11] * 1e6;                      /* m lambda in nm */ 
             break;
 
           case 12: *z= x->thetalist[elnumber];	 	     /*  theta CLRCM */ 
             break; 
           
	   case 17:
	   case 13:  *z= xd[5]+ xd[4]; 
             break;
           case 14:  *z= x->thetalist[elnumber];	 	/*sgm vodar*/
	     break;	
	   
           
  	   default: *z= xd[ipos]; 
	     break;
       }          
    }
   return *z;
}     

void modifygstruct(struct geometrytype *gx, double *tp)  
{
   double theta, delta, alpha, beta;
   
   theta= *tp * (pi/ 180.0);
   delta= asin(gx->mxlam * gx->x[0] / (2.0 * cos(theta)));
   alpha= (-theta- delta);
   beta = (theta - delta);
   gx->cosa= cos(alpha);  
   gx->cosb= cos(beta);  
   gx->sina= sin(alpha);  
   gx->sinb= sin(beta);  
} 
  
void buildsystem(struct optistruct *x) 
{
    extern double map35[35][35]; 
    extern struct mirrortype   mirror;
    extern struct geometrytype geometry;
    void *zeiger;
    int elnumber, matrixsize;  

    matrixsize= 35 * 35 * sizeof(double);
    for (elnumber= 0; elnumber < x->elementzahl; elnumber++)
    {                                                                          
        if (optistructure.modifylist[elnumber]== 1)  /*modified */
	{
	    optistructure.modifylist[elnumber]= 0;  
            /* create matrix xmap35*/      
            zeiger= (struct mirrortype *)&mirror;
            memcpy(zeiger, &x->mirz[elnumber], sizeof(mirror));  
            zeiger= (struct geometrytype *) &geometry;
            memcpy(zeiger, &x->geoz[elnumber], sizeof(geometry));   
            /* common bloecke gefuellt */
	    fgmap_and_xxmap_for(&x->epsilon); 
            /* matrix xmap35 fertig */
            zeiger= (double *) &x->matrixlist[matrixsize* (elnumber+1)]; 
            memcpy(zeiger, map35, sizeof(map35));   
         }  /* modified or not */
         zeiger= (double *) &x->matrixlist[matrixsize* (elnumber+1)]; 
         if (elnumber == 0)            
              memcpy(x->matrixlist, zeiger, sizeof(map35));  
              /*ergebnis bei  0 */
         else
              glue3c(x->matrixlist, &x->matrixlist[matrixsize* (elnumber+1)]);
              /* multiply matrix */         
    }
    memcpy(map35, x->matrixlist, sizeof(map35));
    /* printf("buildsystem end\n"); */
} 

void glue3c (double A[35][35], double B[35][35])     /* A * B = A */
{
   double *zeigera, *zeigerb;
   extern double gluea[35][35], glueb[35][35], map35[35][35];
   
   zeigera= (double *)&gluea[0][0];  
   zeigerb= (double *)&glueb[0][0];  

   memcpy(gluea, A, sizeof(A)); 
   memcpy(glueb, B, sizeof(A));        /* fill commons */
   glue3();                              /* fortran      */
   memcpy(A, map35, sizeof(A));         
}

