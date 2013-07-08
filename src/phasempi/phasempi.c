/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phasesrv/phasesrv.c */
/*  Date      : <14 Sep 12 16:34:45 flechsig>  */
/*  Time-stamp: <08 Jul 13 17:43:02 flechsig>  */
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

 
int main(int argc, char *argv[])
{
  int        i, size, size_save, rank, numtasks, taskid, sender, index, ny, nz, resultid;
  int        setupswitch, cmode, selected, iord, numthreads, format;
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

  printf("%d: phasempi start, size= %d, rank= %d\n", rank, size, rank);

  setupswitch= ProcComandLine(&Beamline.filenames, argc, argv, &cmode, 
			      &selected, &iord, &numthreads, &format);
  /* ignore parameters: cmode, selected, numthreads and also setupswitch */
  /* we evaluaete the filenames, iord and format */

#ifdef DEBUG 
  strncpy(Beamline.filenames.beamlinename, "test_5000.phase", MaxPathLength- 1);  /* for debugging */
#endif


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

  if (iord  != -1) bl->BLOptions.ifl.iord= iord;  /* overwrite iord if provided */

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
 
  //printf("%d >>>>>>>>>>>>>\n\n\n", rank);
  //numtasks= 2;  // for debugging

  /* mpi */
  taskid= numtasks;
  while (1)  /* the main loop executed on each host */
    {
      if (rank == 0)   /* master special */
	{

#ifdef DEBUG
	  printf("\nmaster -> wait for slave\n");
#endif

	  /* get sender_id  from slave in the status tag, and results */
          /* the first item in the results is the resultid= taskid    */
	  /* if resultid is positive the array is stored              */
          /* resultid negative means the slave terminated             */
	  MPI_Recv(&results, N_RESULTS, MPI_DOUBLE, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status); 
	  resultid= (int)results[0];                /* integer number */
	  sender  = status.MPI_TAG;                 /* slave id       */

#ifdef DEBUG
	  printf("master -> resultid= %d\n", resultid );
#endif

	  if ( resultid > 0 ) /* store the result */    
	     {

#ifdef DEBUG
	       printf("master -> save result with id= %d\n", resultid);
#endif

	       index=  resultid- 1;  /* index starts from 0, the tasks from 1 */
	       nz= index % psip->iz; // c loop
	       ny= index / psip->iz; // c loop
	     
	       PSDp->eyrec[ny+nz*psip->iy]= results[1];
	       PSDp->eyimc[ny+nz*psip->iy]= results[2];
	       PSDp->ezrec[ny+nz*psip->iy]= results[3];
	       PSDp->ezimc[ny+nz*psip->iy]= results[4]; 
	       PSDp->psd[ny+nz*psip->iy]  = results[5]; 
	       PSDp->y[ny]		  = results[6];
	       PSDp->z[nz]		  = results[7];
	       //printf("master -> save result with id= %d saved\n", resultid);
	     } 

#ifdef DEBUG
	  else 
	    printf("master -> nothing to save\n");
#endif

	  if ( resultid < 0 )     /* a slave said good bye */
	    {
	      size--; /* the number of hosts */

#ifdef DEBUG
	      printf("master -> received good bye from slave %d, %d processor(s) left\n", sender, size);
#endif

	    }
	  else  /* resultid == 0 or positive */
	    {
	      if ( taskid )  /* tasks left, submit a new task */
		{
		  MPI_Send(&taskid, 1, MPI_INT, sender, 0, MPI_COMM_WORLD);  /* send taskid to slave "sender" */

#ifdef DEBUG
		  printf("master -> submit task %d to %d\n", taskid, sender);
#endif

		  --taskid;
		}
	      else  /* no tasks left */
		{
		  
#ifdef DEBUG
		  printf("master -> submit task %d (stop) to %d\n", taskid, sender);
#endif

		  MPI_Send(&taskid, 1, MPI_INT, sender, 0, MPI_COMM_WORLD);  /* send taskid = 0 to slave */
		}
	    } /* end sender < 0 */

	  if (size <= 1)   /* only master left over */
	    {

#ifdef DEBUG
	      printf("master ==> all slaves said good bye\n");
#endif

	      break;   /* end the loop on master */
	    }
	}
      else   /* rank != 0 (slave) */
      	{
	  /* the slave sends its id in rank and the results of any previous calculation in results */
	  MPI_Send(&results, N_RESULTS, MPI_DOUBLE, 0, rank, MPI_COMM_WORLD);      
	  MPI_Recv(&taskid, 1, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, &status);  // receive new task
	  if ( taskid > 0 )
	    {

#ifdef DEBUG
	      printf("%d: solve task %d\n", rank, taskid);
#endif

	      index= taskid- 1;               /* index counts from 0 */
	      pstc_ii(index, bl);             /* do the integration for an index */
	      /* handle results which are in the beamline struct */
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

#ifdef DEBUG
	      printf("%d: solve task %d done\n", rank, taskid);
#endif

	      /* result is sent in the next call */
	    }
	  else  /* received task 0 */
	    {

#ifdef DEBUG
	      printf("%d: received task %d => say good bye\n", rank, taskid);
#endif

              results[0]= -1.0;       /* negative first index terminates master */
	      MPI_Send(&results, N_RESULTS, MPI_DOUBLE, 0, rank, MPI_COMM_WORLD);  /* acknowledge termination */

#ifdef DEBUG
	      printf("%d: results sent, index= %f\n", rank,  results[0]);
#endif
	      break;                   // slave: escape from loop - close slave 
	    }
	} /* fi rank == 0 */
    } /* while main loop */

#ifdef DEBUG 
  printf("%d: phasempi done\n", rank);
#endif

  endtime= MPI_Wtime();
 
  MPI_Finalize();
 
  if (rank == 0)        /* master only */
    {
      switch (format)
	{
	case 1:
	  WritePsd(bl->filenames.imageraysname, PSDp, PSDp->iy, PSDp->iz);
	  break;
	case 2:
	  write_phase_hdf5_file(bl, bl->filenames.imageraysname);
	  break;
	case 3:
	  write_genesis_hdf5_file(bl, bl->filenames.imageraysname);
	  break;
	default:
	  printf("error: %d output format not defined- use default\n", format);
	  WritePsd(bl->filenames.imageraysname, PSDp, PSDp->iy, PSDp->iz);
	}
      
      duration= endtime- starttime;
      printf("%d: elapsed time= %f s = %f h with %d processors\n", rank, duration, duration/3600., size_save );
    }

  /* clean up memory */
  XFREE(bl->ElementList);
  XFREE(bl->raysout);
  XFREE(bl->RESULT.RESp);
  XFREE(bl->RTSource.SourceRays);
  XFREE(bl->tp);
  XFREE(bl->RTSource.Quellep);
  
  exit(0);
}
/* end */
