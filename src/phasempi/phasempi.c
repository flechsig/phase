/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phasesrv/phasesrv.c */
/*  Date      : <14 Sep 12 16:34:45 flechsig>  */
/*  Time-stamp: <2013-07-04 23:21:41 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <mpi.h>

#include "cutils.h" 
#include "phase_struct.h"
#include "phase.h"
#include "rtrace.h"
#include "phasempi.h"
#include "common.h"

#define  N_RESULTS 8 

int main(int argc, char *argv[])
{
  int        i, size, size_save, rank, numtasks, taskid, sender, index, ny, nz, resultid;
  MPI_Status status;
  double     starttime, endtime, duration, results[N_RESULTS];
  struct BeamlineType Beamline, *bl;
  struct PSImageType *psip;
  struct PSDType     *PSDp;
      
  MPI_Init(&argc, &argv);
  starttime= MPI_Wtime();
  MPI_Comm_size(MPI_COMM_WORLD, &size); // the number of involved cores
  MPI_Comm_rank(MPI_COMM_WORLD, &rank); // the if of the core
  size_save= size;                      // remember the size
  for (i= 0; i < N_RESULTS; i++) results[i]= 0.0;  // initialize result
  numtasks= 5;
  printf("%d: phasempi start, size= %d, rank= %d, tasks= %d\n", rank, size, rank, numtasks);

  if  (size <= 1)     // abort if no slaves available
    {
      fprintf(stderr, "size= %d, no slaves available- exit\n\n", size);
      MPI_Finalize();
      exit(0);
    }

  /* phase copy from batchmode */
  /* build beamline on each host */
  bl= &Beamline;
  Beamline.localalloc= DOALLOC;  /* phasesrv should reserve the memory */ 
#ifdef DEBUG 
  strncpy(Beamline.filenames.beamlinename, "/gpfs/home/flechsig/phase/data/test_5000.phase", MaxPathLength- 1);  /* for debugging */
  strncpy(Beamline.filenames.imageraysname, "/gpfs/home/flechsig/phase/data/test_5000.phase.h5", MaxPathLength- 1);
#endif
  bl->ElementList= NULL;                       /* 15.12.99 */
  bl->raysout= NULL;
  bl->RESULT.RESp= NULL;
  bl->RTSource.SourceRays= NULL;
  bl->beamlineOK= 0;
  bl->tp= NULL;
  bl->RTSource.Quellep= NULL;
  bl->RTSource.QuellTyp= '0';
  bl->posrc.iconj= 0;
  bl->BLOptions.ifl.inorm= 0;
  bl->BLOptions.ifl.pst_mode= 2;

  ReadBLFile(bl->filenames.beamlinename, bl);
  BuildBeamline(bl);

  posrc_construct(bl);
  posrc_ini(bl);
  psip = (struct PSImageType *)bl->RTSource.Quellep;

  ReAllocResult(bl, PLphspacetype, psip->iy, psip->iz);
  numtasks= psip->iy * psip->iz;
 
  PSDp= (struct PSDType *)bl->RESULT.RESp;
  //write_phase_hdf5_file(bl, bl->filenames.imageraysname);
  /* end phase */
  Test4Grating(bl);
 
  printf("%d >>>>>>>>>>>>>\n\n\n", rank);

  numtasks= 2;  // for debugging
  /* mpi */
  taskid= numtasks;
  while (1)  // main loop
    {
      if (rank == 0)   // master
	{
	  printf("\n\nmaster -> wait for slave\n");
	  MPI_Recv(&results, N_RESULTS, MPI_DOUBLE, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status); // get sender
	  resultid= (int)results[0];
	  sender  = status.MPI_TAG;
	  printf("master -> resultid= %d\n", resultid );
	  if ( resultid > 0) /* at the beginning all hosts have resultid == 0, at the end it is negative */    
	     {
	       printf("master -> save result with id= %d\n", resultid);
	       index=  abs(resultid- 1);
	       nz= index % psip->iz; // c loop
	       ny= index / psip->iz; // c loop
	     
	       PSDp->eyrec[ny+nz*psip->iy]= results[1];
	       PSDp->eyimc[ny+nz*psip->iy]= results[2];
	       PSDp->ezrec[ny+nz*psip->iy]= results[3];
	       PSDp->ezimc[ny+nz*psip->iy]= results[4]; 
	       PSDp->psd[ny+nz*psip->iy]  = results[5]; 
	       PSDp->y[ny]		  = results[6];
	       PSDp->z[nz]		  = results[7];
	       printf("master -> save result with id= %d saved\n\n", resultid);
	     } else 
	    printf("master -> nothing to save\n");

	  if ( resultid < 0 ) 
	    {
	      size--; // a slave said good bye
	      printf("master -> slave %d said good bye, %d processor(s) left\n", sender, size);
	    }
	  else
	    {
	      if ( taskid )  // tasks left submit a new task
		{
		  MPI_Send(&taskid, 1, MPI_INT, sender, 0, MPI_COMM_WORLD);  // send taskid
		  printf("master -> submit task %d to %d\n", taskid, sender);
		  --taskid;
		}
	      else  // no tasks left
		{
		  //taskid = -1;
		  printf("master ->  submit task %d (stop) to %d\n", taskid, sender);
		  MPI_Send(&taskid, 1, MPI_INT, sender, 0, MPI_COMM_WORLD);  // send taskid = -1 to slave
		}
	    } /* end sender < 0 */
	  if (size <= 1) 
	    {
	      printf("master ->  ==> all slaves said good bye\n");
	      break;   // only the master is left- end the loop
	    }
	}
      else   /* rank != 0 (slave) */
      	{
	  MPI_Send(&results, N_RESULTS, MPI_DOUBLE, 0, rank, MPI_COMM_WORLD);      // send id to master in tag to get new task
	  MPI_Recv(&taskid, 1, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, &status); // receive task
	  if ( taskid > 0 )
	    {
	      printf("rank= %d, solve task %d\n", rank, taskid);
	      index= taskid- 1;
	      pstc_ii(index, bl);             // the integration
	      /* handle results */
	      nz= index % psip->iz; // c loop
	      ny= index / psip->iz; // c loop
	      results[0]= (double)taskid;                 // first is taskid
	      results[1]= PSDp->eyrec[ny+nz*psip->iy];
	      results[2]= PSDp->eyimc[ny+nz*psip->iy];
	      results[3]= PSDp->ezrec[ny+nz*psip->iy];
	      results[4]= PSDp->ezimc[ny+nz*psip->iy]; 
	      results[5]= PSDp->psd[ny+nz*psip->iy];
	      results[6]= PSDp->y[ny];
	      results[7]= PSDp->z[nz];
	      printf("rank= %d, solve task %d done\n", rank, taskid);
	      /* result is sent in the next call */
	    }
	  else  /* received task 0 */
	    {
	      printf("rank= %d, received task %d => say good bye\n", rank, taskid);
              results[0]= -1.0;       /* negative first index terminates master */
	      MPI_Send(&results, N_RESULTS, MPI_DOUBLE, 0, rank, MPI_COMM_WORLD);  // send id to master to get new task
	      printf("rank= %d, results sent, index= %f\n", rank,  results[0]);
	      break;                   // slave: escape from loop - close slave 
	    }
	} /* fi rank == 0 */
    } /* while */
 
  printf("%d: phasempi done\n", rank);
  endtime= MPI_Wtime();
 
  MPI_Finalize();
 
  if (rank == 0)        /* master only */
    {
      write_phase_hdf5_file(bl, bl->filenames.imageraysname);
      duration= endtime- starttime;
      printf("elapsed time= %f s = %f h with %d processors\n", duration, duration/3600., size_save );
    }

  exit(0);
}
/* end */
