/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phasesrv/phasesrv.c */
/*  Date      : <14 Sep 12 16:34:45 flechsig>  */
/*  Time-stamp: <03 Jul 13 15:07:44 flechsig>  */
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

int main(int argc, char *argv[])
{
  int        size, size_save, rank, numtasks, taskid, sender;
  MPI_Status status;
  double     starttime, endtime, duration;
  
  numtasks= 5;

  MPI_Init(&argc, &argv);
  starttime= MPI_Wtime();
  MPI_Comm_size(MPI_COMM_WORLD, &size); // the number of involved cores
  MPI_Comm_rank(MPI_COMM_WORLD, &rank); // the if of the core
  size_save= size;                      // remember the size
 
  printf("%d: phasempi start, size= %d, rank= %d, tasks= %d\n", rank, size, rank, numtasks);

  if  (size <= 1)
    {
      fprintf(stderr, "size= %d, no slaves available- exit\n\n", size);
      MPI_Finalize();
      exit(0);
    }

  taskid= numtasks;
  while (1)  // main loop
    {
      if (rank == 0)   // master
	{
	  printf("master -> ");
	  MPI_Recv(&sender, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status); // get sender
	  if (sender < 0) 
	    {
	      size--; // a slave said good bye
	      printf("slave %d said good bye, %d processor(s) left\n", status.MPI_TAG, size);
	    }
	  else
	    {
	      if ( taskid )  // tasks left submit a new task
		{
		  MPI_Send(&taskid, 1, MPI_INT, sender, 0, MPI_COMM_WORLD);  // send taskid
		  printf(" submit task %d to %d\n", taskid, sender);
		  --taskid;
		}
	      else  // no tasks left
		{
		  //taskid = -1;
		  printf(" submit task %d (stop) to %d\n", taskid, sender);
		  MPI_Send(&taskid, 1, MPI_INT, sender, 0, MPI_COMM_WORLD);  // send taskid = -1 to slave
		}
	    } /* end sender < 0 */
	  if (size <= 1) 
	    {
	      printf(" ==> all slaves said good bye\n");
	      break;   // only the master is left- end the loop
	    }
	}
      else   /* rank != 0 (slave) */
      	{
	  MPI_Send(&rank,   1, MPI_INT, 0, rank, MPI_COMM_WORLD);  // send id to master to get new task
	  MPI_Recv(&taskid, 1, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, &status); // receive task
	  if ( taskid > 0 )
	    printf("rank= %d, solve task %d\n", rank, taskid);
	  else
	    {
	      printf("rank= %d, received task %d => say good bye\n", rank, taskid);
              taskid= -1;
	      MPI_Send(&taskid, 1, MPI_INT, 0, rank, MPI_COMM_WORLD);  // send id to master to get new task
	      break;                   // slave: escape from loop - close slave 
	    }
	} /* fi rank == 0 */
    } /* while */
 
  printf("%d: phasempi done\n", rank);
  endtime= MPI_Wtime();
 
  MPI_Finalize();

  if (rank == 0)
    {
      duration= endtime- starttime;
      printf("elapsed time= %f s = %f h with %d processors\n", duration, duration/3600., size_save );
    }
  exit(0);
}
/* end */
