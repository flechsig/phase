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
  int numprocs, myid, numtasks, taskid;
  
  numtasks= 5;

  
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myid);

 
  printf("phasempi start, numprocs= %d, myid= %d\n", numprocs, myid);
  
  
  while (1)
    {
      if (myid == 0)
	{
	  printf("myid == 0\n");
	  if ( numtasks )
	    {
	      /* submit a new task */
	      MPI_Bcast();
	      --numtask;
	    }
	  else
	    {
	      printf("myid == 0 --> all tasks done\n");
	      break;
	    }
	}
      else   /* myid != 0 */
      	{
	  printf("myid= %d, solve task %d\n", myid, taskid);
	  MPI_Reduce();
	} /* fi myid == 0 */

    } /* while */
 
  
 

  printf("phasempi done\n");

  MPI_Finalize();

  exit(0);
}
/* end */
