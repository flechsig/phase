/*  File      : /afs/psi.ch/project/phase/src/phase/pst_thread.h */
/*  Date      : <21 Mar 13 15:05:12 flechsig>  */
/*  Time-stamp: <10 Apr 13 13:55:38 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifndef PST_MPI_H
#define PST_MPI_H

struct MpiData
{
  int    mpi_no;
  struct BeamlineType *bl;
};

void pst_impi(struct MpiData *, int);
int  pst_mpi(struct BeamlineType *, struct MpiData *);

#endif
/* end pst_mpi.h */
