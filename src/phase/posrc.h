/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/posrc.h */
/*  Date      : <23 Apr 12 10:44:55 flechsig>  */
/*  Time-stamp: <14 Mar 13 12:29:18 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

/* soure routines for physical optics */
/* replaces routines in phase_source.F, only source4 is implemented so far, the reason is the extension to unlimited gridsize */

#ifndef POSRC_H
#define POSRC_H

#ifdef HAVE_HDF5
   #include "hdf5.h"
#endif 

void check_file_consistency(struct BeamlineType *, int, int);
void posrc_fill4(struct BeamlineType *, double *, FILE *,   int);
void posrc_fill7(struct BeamlineType *, double *, double *, int, int, int);
void posrc_fill8(struct BeamlineType *, double *, double *, int);
void posrc_fill_min_max(struct BeamlineType *);
FILE *posrc_fopen(char *);
void posrc_ini(struct BeamlineType *);
void reallocate_posrc(struct BeamlineType *, int, int);
int  source4c_ini(struct BeamlineType *);
void source7c_ini(struct BeamlineType *);
void source8c_ini(struct BeamlineType *);
void source4c_inter_2d_(struct source_results *, double *, double *, int *);

#ifdef HAVE_HDF5
int check_hdf5_type(char *, int, int);
void readDataDouble(hid_t, char *, double *, int);
void readDataInt(hid_t, char *, int *, int);
int  getDatasetSize(hid_t, char *);
int  hasDataset(hid_t, char *);
hid_t myH5Fopen(char *);
#endif

#endif 
/* end POSRC_H */
