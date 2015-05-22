/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phasesrv/phasesrv.c */
/*  Date      : <14 Sep 12 16:34:45 flechsig>  */
/*  Time-stamp: <22 May 15 14:15:15 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

// ******************************************************************************
//
//   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
//                      Paul Scherrer Institut Villigen, Switzerland
//   
//   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
//          Uwe Flechsig,    uwe.flechsig@psi.ch
//
// ------------------------------------------------------------------------------
//
//   This file is part of PHASE.
//
//   PHASE is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, version 3 of the License, or
//   (at your option) any later version.
//
//   PHASE is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
//
// ******************************************************************************


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
  int        i, size, size_save, rank, numtasks, taskid, sender, index, ny, nz, resultid, logid;
  int        setupswitch, cmode, selected, iord, numthreads, format, idx;
  MPI_Status status;
  double     starttime, endtime, duration, results[N_RESULTS];
  FILE       *log;
  struct BeamlineType Beamline, *bl;
  struct PSImageType *psip;
        
  MPI_Init(&argc, &argv);               // mpi
  starttime= MPI_Wtime();               // mpi time     
  MPI_Comm_size(MPI_COMM_WORLD, &size); // the number of involved cores
  MPI_Comm_rank(MPI_COMM_WORLD, &rank); // the if of the core
  size_save= size;                      // remember the size
  for (i= 0; i< N_RESULTS; i++) results[i]= 0.0;  // mpi initialize result
  if (size <= 1)     // abort if no slaves available
    {
      fprintf(stderr, "size= %d, no slaves available- exit\n\n", size);
      MPI_Finalize();
      exit(0);
    }                                     

  printf("%d: phasempi start, size= %d, rank= %d\n", rank, size, rank);

  setupswitch= ProcComandLine(&Beamline.filenames, argc, argv, &cmode, 
			      &selected, &iord, &numthreads, &format);
  /* ignore parameters: cmode, selected, numthreads and also setupswitch */
  /* we evaluate the filenames, iord and format */

  /* build beamline on each host (copy from batchmode)                 */
  bl= &Beamline;                                    /* init bl pointer */
  bl->localalloc= DOALLOC;       /* phasesrv should reserve the memory */ 
  bl->ElementList= NULL;                       
  bl->raysout= NULL;
  bl->RESULT.RESp= NULL;
  bl->RTSource.SourceRays= NULL;
  bl->beamlineOK= 0;
  bl->tp= NULL;
  bl->RTSource.Quellep= NULL;
  bl->RTSource.QuellTyp= '0';
  bl->BLOptions.ifl.inorm= 0;
  bl->BLOptions.ifl.pst_mode= 2;
  bl->emfp= NULL;
  bl->source_emfp= NULL;
  bl->result_emfp= NULL;
  bl->simpre= NULL;
  bl->simpim= NULL;
  bl->sintre= NULL;
  bl->sintim= NULL;
  bl->vdy   = NULL;
  bl->vdz   = NULL;
  ReadBLFile(bl->filenames.beamlinename, bl);    // read the data file

  if (iord != -1) bl->BLOptions.ifl.iord= iord;  /* overwrite iord if provided */

  BuildBeamline(bl);

  posrc_ini(bl);
  psip = (struct PSImageType *)bl->RTSource.Quellep;
  numtasks= psip->iy * psip->iz;
 
  Test4Grating(bl);
  bl->position= 0;
  if (bl->emfp) 
    {
      printf("!!!!!!!!! warning: das sollte nie gerufen werden\n");
      emfp_free(bl->emfp);
      bl->emfp= NULL;
    }
  bl->emfp= (struct EmfType *)emfp_construct(bl->source_emfp->nz, bl->source_emfp->ny);
  emfp_cpy(bl->emfp, bl->source_emfp);                                 // source-> emfp
  
  if (bl->result_emfp) bl->result_emfp= (struct EmfType *)emfp_free(bl->result_emfp);  // clean up result
  bl->result_emfp= (struct EmfType *)emfp_construct(psip->iz, psip->iy); // !! image plane - not source
   
  /* mpi */
  taskid= numtasks;
  logid= 0;

  /* master special - open incremental logfile to have some results in case of a crash */
  if (rank == 0) log= openlogfile(bl, numtasks);

  while (1)  /* the main loop executed on each host */
    {
      if (rank == 0)   /* master special */
	{

#ifdef DEBUG
	  printf("\n");
	  //printf("\nmaster -> wait for slave(s)\n");
#endif

	  /* get sender_id  from slave in the status tag, and results */
          /* the first item in the results is the resultid= taskid    */
	  /* if resultid is positive the array is stored              */
          /* resultid negative means the slave terminated             */
	  MPI_Recv(&results, N_RESULTS, MPI_DOUBLE, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status); 
	  resultid= (int)results[0];                /* integer number */
	  sender  = status.MPI_TAG;                 /* slave id       */

#ifdef DEBUG
	  printf("master -> received msg_id= %d from slave_id= %d\n", resultid, sender);
#endif

	  if ( resultid > 0 ) /* store the result */    
	     {

#ifdef DEBUG1
	       printf("master -> save result with id= %d\n", resultid);
#endif

	       index=  resultid- 1;  /* index starts from 0, the tasks from 1 */
	       nz= index % psip->iz; // c loop
	       ny= index / psip->iz; // c loop

	       idx= nz+ ny*bl->result_emfp->nz;
	       
	       bl->result_emfp->eyre[idx]= results[1];
	       bl->result_emfp->eyim[idx]= results[2];
	       bl->result_emfp->ezre[idx]= results[3];   
	       bl->result_emfp->ezim[idx]= results[4];
	       bl->result_emfp->y[ny]    = results[6];
	       bl->result_emfp->z[nz]    = results[7];
	       fprintf(log, "%-8d %-8d % e % e % e % e % e % e\n",
		       logid, index, results[6], results[7], results[1], results[2], results[3], results[4]);
	       fflush(log);
	       logid++;
 	       //printf("master -> save result with id= %d saved\n", resultid);
	     } 

#ifdef DEBUG
	  else 
	    printf("master -> nothing to save\n");
#endif

	  if ( resultid < 0 )     /* a slave said good bye */
	    {
	      size--;             /* reduce the number of hosts */

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
		  printf("master -> submit task %d to slave %d\n", taskid, sender);
#endif

		  --taskid;
		}
	      else  /* no tasks left */
		{
		  
#ifdef DEBUG
		  printf("master -> submit task %d (stop) to slave %d\n", taskid, sender);
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
	      printf("slave %d: received task %d\n", rank, taskid);
#endif

	      index= taskid- 1;               /* index counts from 0 */
	      pstc_ii(index, bl);             /* do the integration for an index */
	      /* handle results which are in the beamline struct */
	      nz= index % psip->iz; // c loop
	      ny= index / psip->iz; // c loop
	      results[0]= (double)taskid;                 // first is taskid

	      idx= nz+ ny*bl->result_emfp->nz;
	      
	      results[1]= bl->result_emfp->eyre[idx];
	      results[2]= bl->result_emfp->eyim[idx];
	      results[3]= bl->result_emfp->ezre[idx];
	      results[4]= bl->result_emfp->ezim[idx]; 
	      results[6]= bl->result_emfp->y[ny];
	      results[7]= bl->result_emfp->z[nz];
	      
#ifdef DEBUG
	      printf("slave %d: finished task %d \n", rank, taskid);
#endif

	      /* result is sent in the next call */
	    }
	  else  /* received task 0 */
	    {

#ifdef DEBUG
	      printf("slave %d: received task %d => say good bye\n", rank, taskid);
#endif

              results[0]= -1.0;       /* negative first index terminates master */
	      MPI_Send(&results, N_RESULTS, MPI_DOUBLE, 0, rank, MPI_COMM_WORLD);  /* acknowledge termination */

#ifdef DEBUG
	      printf("slave %d: results sent, index= %f\n", rank,  results[0]);
#endif
	      break;                   // slave: escape from loop - close slave 
	    }
	} /* fi rank == 0 */
    } /* while main loop */

#ifdef DEBUG 
  printf("      %d: phasempi done\n", rank);
#endif

  endtime= MPI_Wtime();
 
  MPI_Finalize();
 
  if (rank == 0)        /* master only */
    {
      switch (format)
	{
	case 1:
	  printf("error: %d output format obsolete- use default\n", format);
	  //for (i=0; i< 10; i++) printf("%d, %f\n", i, bl->result_emfp->y[i]);
	  write_phase_hdf5_file(bl, bl->filenames.imageraysname, NULL);
	  break;
	case 2:
	  write_phase_hdf5_file(bl, bl->filenames.imageraysname, NULL);
	  break;
	case 3:
	  printf("write results to file %s\n",  bl->filenames.imageraysname);
	  write_genesis_hdf5_file(bl, bl->filenames.imageraysname, NULL);
	  break;
	default:
	  printf("error: %d output format not defined- use default\n", format);
	  write_phase_hdf5_file(bl, bl->filenames.imageraysname, NULL);
	}
      fprintf(log, "# ====================================================================================================\n");
      fclose(log);
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

FILE *openlogfile(struct BeamlineType *bl, int numtasks)
{
  char mylogfilename[MaxPathLength];
  FILE *log;

  snprintf(mylogfilename, MaxPathLength- 1, "%s%s", bl->filenames.imageraysname, LOGFILE_EXT);
  printf("master -> open logfile %s\n", mylogfilename); 
  
  if ((log= fopen(mylogfilename, "w")) == NULL) 
    { 
      fprintf(stderr, "error: can't open file: %s - exit\n", mylogfilename); 
      MPI_Finalize();
      exit(-1); 
    }
  fprintf(log, "# ====================================================================================================\n");
  fprintf(log, "# phasempi textlog ==> numtasks= %d ==> wavelength= %lf ==> cols= %d ==> rows= %d\n", 
	  numtasks, bl->BLOptions.lambda*1e-3, bl->RTSource.Quellep->iz, bl->RTSource.Quellep->iy);
  fprintf(log, "# logid  taskid         y             z             yre           yim           zre           zim\n");
  fprintf(log, "# ====================================================================================================\n");

  return log;
} // end openlogfile
/* end */
