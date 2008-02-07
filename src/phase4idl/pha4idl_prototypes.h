
//
// pha4idl_prototypes.h
//
// (c) 2008 : Torsten.Leitner@email.de
//


#ifndef PHA4IDL_PROTOTYPES_H_
#define PHA4IDL_PROTOTYPES_H_



// function Prototypes


// from file:  pha_Src4Tools.c

int pha_c_extract_src4_grid(struct source4 *src4,
                            int nz, double zmin, double zmax,
				    int ny, double ymin, double ymax);

int pha_c_define_src4_grid(struct source4 *src4,
                            int nz, double zmin, double zmax,
				    int ny, double ymin, double ymax);

int pha_c_adjust_src4_grid(struct source4 *src4);



// from file:  pha_Src4WFGauss.c

int phaSrcWFGauss (struct source4 *gb4, int *ianzz, double *zmini, double *zmaxi, 
                                    int *ianzy, double *ymini, double *ymaxi, 
						double *w0, double *deltax, double *xlambda );



#endif // PHA4IDL_PROTOTYPES_H_


