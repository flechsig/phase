//  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/mainwindow_slots.cpp
//  Date      : <09 Sep 11 15:22:29 flechsig> 
//  Time-stamp: <19 Jul 12 13:40:31 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 

//
// only the slots
//

#include <QtGui>
#include <cmath>                     // for abs
#include <tr1/functional>            // for std::tr1
#include <qwt_plot_grid.h>

#include "mainwindow.h"
#include "phaseqt.h"
#include "plotmatrix.h"
#include "plot2x2.h"

using namespace std;   // fuer cout z.B.

//////////////////////////////////////////////
// begin slots section (callback functions) //
//////////////////////////////////////////////

// slot
void MainWindow::about()
{
#ifdef SEVEN_ORDER
  const char *onoff= "on";
#else
  const char *onoff= "off";
#endif

   QMessageBox::about(this, tr("About PhaseQt"),
            tr("<b>phaseqt</b> is the new graphical user interface for the software package <b>phase</b>-"
               "the wave front propagation and ray tracing code developed by "
               "<center><a href=mailto:Johannes.Bahrdt@helmholtz-berlin.de>Johannes Bahrdt</a>, <a href=mailto:uwe.flechsig&#64;psi.&#99;&#104;>Uwe Flechsig</a> and others. </center><hr>"
               "<center>phaseqt version: '%1',<br>"
               "configured: '%2',<br>"
               "SEVEN_ORDER: '%3',<br>"
	       "GRIDSIZE: '%4'</center>").arg(VERSION).arg(CONFIGURED).arg(onoff).arg(GRIDSIZE));
} // about

// UF slot
// here we call our own code dependign on which button has been pressed
void MainWindow::activateProc(const QString &action)
{
  char buffer[MaxPathLength], header[MaxPathLength];
  struct PSImageType *psip;
  struct constants *csp;
  struct mirrortype *am;
  struct geometryst *g;
  struct map4 *m4p;

  int filesOK;
  QEventLoop q;

#ifdef DEBUG
  //  cout << "debug: " << __FILE__ << " MainWindow::activateProc: (old) mwplotsubject: " << mwplotsubject << endl;
  cout << "debug: " << __FILE__ << " MainWindow::activateProc called " << endl;
#endif
  
  if (action.isEmpty())
          return;
  
  if (!action.compare("asynMapAct")) 
    { 
      if (elementListIsEmpty())
	return;

      fillTaskVector(myparent->myBeamline()->elementzahl);
      if (myparent->myBeamline()->hormapsloaded != myparent->myBeamline()->BLOptions.ifl.iord) 
	MakeHorMaps(myparent->myBeamline());
      
      qDebug() << __FILE__ << "asynMapAct: create maps in parallel threads";

      // *future= QtConcurrent::map(vector, my_funcv);
      // to pass additional parameters we have to use boost or std::tr1
      *future= QtConcurrent::map(vector, std::tr1::bind(BuildElement, 
							std::tr1::placeholders::_1, myparent->myBeamline())); // one additional par 
      watcher->setFuture(*future);
    }
  
  if (!action.compare("asynTestAct")) 
    { 
      fillTaskVector(1000);

      qDebug() << "test asynchronous tasks with threads";
      // *future= QtConcurrent::map(vector, my_funcv);
      // to pass additional parameters we have to use boost or std::tr1
      *future= QtConcurrent::map(vector, std::tr1::bind(my_funcv, 
							std::tr1::placeholders::_1, 1000)); // one additional par 
      watcher->setFuture(*future);
    }

  if (!action.compare("asynPOAct")) 
    { 
#ifdef DEBUG  
      cout << "debug: " << __FILE__ << " asynPOAct button pressed" << endl; 
#endif
      if (elementListIsEmpty()) 
	return;

      myparent->myBeamline()->beamlineOK &= ~resultOK;
      UpdateStatus();

      myparent->myBuildBeamline();

      if (!(myparent->myBeamline()->beamlineOK & pstsourceOK))
	{
#ifdef OLD_PO_SOURCE
	  myparent->mysrc_ini(&myparent->myBeamline()->src); 
#else
	  myparent->myposrc_ini();
#endif
          myparent->myBeamline()->beamlineOK |= pstsourceOK;
	}

      if (!(myparent->myBeamline()->beamlineOK & pstimageOK)) 
	sourceApplyBslot();
      
      if (CheckBLOK(myparent->myBeamline()->beamlineOK, 
		    (pstsourceOK | mapOK | pstimageOK), (char *)"act_pr: ") > 0)
	{
	  psip = (struct PSImageType *)myparent->myBeamline()->RTSource.Quellep;
	  myparent->myReAllocResult(PLphspacetype, psip->iy, psip->iz);
	  //myparent->myPST();
	  fillTaskVector(psip->iy * psip->iz);

	  qDebug() << "test asynchronous PO with threads " << psip->iy * psip->iz << " points";; 
	  
	  // *future= QtConcurrent::map(vector, my_funcv);
	  // to pass additional parameters we have to use boost or std::tr1
	  
	  Test4Grating(myparent->myBeamline(), &am, &g);
          
	  if ((m4p_cpp == NULL) && (myparent->myBeamline()->BLOptions.ifl.pst_mode < 2)) m4p_cpp= XMALLOC(struct map4, 1);
	  if (myparent->myBeamline()->BLOptions.ifl.pst_mode < 2) fill_m4(myparent->myBeamline(), m4p_cpp);
	    
	  if (csp_cpp == NULL) csp_cpp= XMALLOC(struct constants, 1);
	  initconstants(csp_cpp);
	 
	  myparent->myBeamline()->BLOptions.PSO.intmod= 2;
	  *future= QtConcurrent::map(vector, std::tr1::bind(pstc_i, std::tr1::placeholders::_1, myparent->myBeamline(), 
							    m4p_cpp, csp_cpp, am, g 
							    )); // one additional par 
	  
	  watcher->setFuture(*future);
	}
    }

  if (!action.compare("normPOAct")) norm_output(myparent->myBeamline());
  
  if (!action.compare("raytracesimpleAct")) 
    { 
      if (elementListIsEmpty())
	return;
      
      printf("\nraytracesimpleAct button  pressed, localalloc: %d hormaps_loaded: %d\n", 
	     myparent->myBeamline()->localalloc, myparent->myBeamline()->hormapsloaded);
      
      myparent->myBeamline()->beamlineOK &= ~resultOK;
      UpdateStatus();
      //QMessageBox *mmBox = nebw QMessageBox;
      //mmBox->setText(tr("calculation running- be patient!"));
      //mmBox->show();
      if (!(myparent->myBeamline()->beamlineOK & sourceOK)) myparent->myMakeRTSource();
      
      //statusBar()->clearMessage();
      
      cout << "********** plrayset=" << myparent->myBeamline()->BLOptions.plrayset << endl;
      statusBar()->showMessage(tr("Quick ray trace->calculation running - be patient"), 0);
		
      myparent->myReAllocResult((PLrttype | myparent->myBeamline()->BLOptions.plrayset), 
				myparent->myBeamline()->RTSource.raynumber, 0);  
      
      myparent->myUpdateFlags(FIRST);
      
      //myparent->buildBeamlineParallel();      // for tests so far
      myparent->myBuildBeamline();
      myparent->myRayTracec(); 
      
      if (myparent->myBeamline()->BLOptions.plrayset == 3)  /* double wavelength calculation */
	{
	  myparent->myUpdateFlags(SECOND);
	  
	  //myparent->buildBeamlineParallel();      // for tests so far
	  myparent->myBuildBeamline();
	  myparent->myRayTracec(); 
	}
#ifdef DEBUG      
      cout << "debug: ray trace-> done" << endl;
#endif
      //mmBox->close();
      
      statusBar()->showMessage(tr("Quick ray trace-> done!"), 4000);
    }
  
  if (!action.compare("raytracefullAct")) 
    { 
      printf("\nraytracefullAct button  pressed\n");

      if (elementListIsEmpty())	return;

      myparent->myBeamline()->beamlineOK &= ~resultOK;
      UpdateStatus();

      if (!(myparent->myBeamline()->beamlineOK & sourceOK)) myparent->myMakeRTSource();

      myparent->myReAllocResult((PLrttype | myparent->myBeamline()->BLOptions.plrayset), 
				myparent->myBeamline()->RTSource.raynumber, 0);
      
      myparent->myUpdateFlags(FIRST);
      myparent->myBuildBeamline();
      myparent->myRayTraceFull(); 

      if (myparent->myBeamline()->BLOptions.plrayset == 3)  /* double wavelength calculation */
	{
	  myparent->myUpdateFlags(SECOND);
	  myparent->myBuildBeamline();
	  myparent->myRayTraceFull();
	}

      printf("full ray trace-> done\n");
      statusBar()->showMessage(tr("full ray trace-> done!"), 4000);

    } /* full ray trace */

  if (!action.compare("footprintAct")) 
    { 
      printf("\nfootprintAct button  pressed\n");
      if (elementListIsEmpty())
	return;
      if (elementListNotSelected())
	return;

      if (!(myparent->myBeamline()->beamlineOK & sourceOK))
	myparent->myMakeRTSource();

      myparent->myReAllocResult(PLrttype, myparent->myBeamline()->RTSource.raynumber* 
				myparent->myBeamline()->BLOptions.plrayset, 0);  
      myparent->myBuildBeamline();
      myparent->myFootprint((elementList->currentRow()+1));
      cout << "footprint-> done" << endl;
      statusBar()->showMessage(tr("Footprint-> done!"), 4000);
    }

  if (!action.compare("singleRayAct")) 
    { 
      printf("singleRayAct button pressed\n");
      if (elementListIsEmpty())
	return;
      if (!s_ray) 
	s_ray= new SingleRay(myparent, this); 
      else 
	s_ray->singleRayBox->show();
    }

  if (!action.compare("optiInputAct")) 
    { 
      cout << "optiInputAct button pressed, elementzahl=" << myparent->myBeamline()->elementzahl << endl;
      if (!o_input) 
	o_input= new OptiInput(myparent->myBeamline()->ElementList, myparent->myBeamline()->elementzahl,
			       myparent->myBeamline()->filenames.beamlinename, myparent->myBeamline()->filenames.optipckname, 
			       myparent->myBeamline()->filenames.opresname, myparent->myBeamline()->filenames.minname); 
      else 
	o_input->optiInputBox->show();
    }

  if (!action.compare("phasespaceAct"))
    {   
#ifdef DEBUG  
      cout << "debug: " << __FILE__ << " phasespaceAct button pressed" << endl; 
#endif
      if (elementListIsEmpty()) 
	return;

      myparent->myBeamline()->beamlineOK &= ~resultOK;
      UpdateStatus();

      myparent->myBuildBeamline();

      if (!(myparent->myBeamline()->beamlineOK & pstsourceOK))
	{
#ifdef OLD_PO_SOURCE
	  myparent->mysrc_ini(&myparent->myBeamline()->src); 
#else
	  myparent->myposrc_ini();
#endif
	  myparent->myBeamline()->beamlineOK |= pstsourceOK;
	}

      if (!(myparent->myBeamline()->beamlineOK & pstimageOK)) 
	sourceApplyBslot();
			
      if (CheckBLOK(myparent->myBeamline()->beamlineOK, 
		    (pstsourceOK | mapOK | pstimageOK), (char *)"act_pr: ") > 0)
	{
	  psip = (struct PSImageType *)myparent->myBeamline()->RTSource.Quellep;
	  myparent->myReAllocResult(PLphspacetype, psip->iy, psip->iz);
	  myparent->myPST();
	}
    }

  if (!action.compare("mphasespaceAct")) 
  {   
//#ifdef DEBUG  
      cout << "debug: " << __FILE__ << " mphasespaceAct button pressed" << endl; 
//#endif
      if (elementListIsEmpty()) 
	return;

      myparent->myBeamline()->beamlineOK &= ~resultOK;
      UpdateStatus();

      myparent->myBuildBeamline();

      if (!(myparent->myBeamline()->beamlineOK & pstsourceOK))
	{
#ifdef OLD_PO_SOURCE
	  myparent->mysrc_ini(&myparent->myBeamline()->src); 
#else
	  myparent->myposrc_ini();
#endif
	  myparent->myBeamline()->beamlineOK |= pstsourceOK;
	}

      if (!(myparent->myBeamline()->beamlineOK & pstimageOK)) 
	sourceApplyBslot();
			
      if (CheckBLOK(myparent->myBeamline()->beamlineOK, 
		    (pstsourceOK | mapOK | pstimageOK), (char *)"act_pr: ") > 0)
	{
	  psip = (struct PSImageType *)myparent->myBeamline()->RTSource.Quellep;
	  myparent->myReAllocResult(PLphspacetype, psip->iy, psip->iz);
	  myparent->myMPST();
	}
  }

  if (!action.compare("rthAct")) { myparent->myBeamline()->RTSource.QuellTyp= 'H'; UpdateSourceBox(); }
  if (!action.compare("dipAct")) { myparent->myBeamline()->RTSource.QuellTyp= 'D'; UpdateSourceBox(); }
  if (!action.compare("poiAct")) { myparent->myBeamline()->RTSource.QuellTyp= 'o'; UpdateSourceBox(); }
  if (!action.compare("rinAct")) { myparent->myBeamline()->RTSource.QuellTyp= 'R'; UpdateSourceBox(); }   
  if (!action.compare("genAct")) { myparent->myBeamline()->RTSource.QuellTyp= 'G'; UpdateSourceBox(); }    
  if (!action.compare("b2hAct")) { myparent->myBeamline()->RTSource.QuellTyp= 'U'; UpdateSourceBox(); }   
  if (!action.compare("b2lAct")) { myparent->myBeamline()->RTSource.QuellTyp= 'U'; UpdateSourceBox(); }    
  if (!action.compare("sisAct")) { myparent->myBeamline()->RTSource.QuellTyp= 'L'; UpdateSourceBox(); }   
  if (!action.compare("simAct")) { myparent->myBeamline()->RTSource.QuellTyp= 'M'; UpdateSourceBox(); }   
  if (!action.compare("sffAct")) { myparent->myBeamline()->RTSource.QuellTyp= 'F'; UpdateSourceBox(); }  
  if (!action.compare("impAct")) { myparent->myBeamline()->RTSource.QuellTyp= 'I'; UpdateSourceBox(); }  

  if (!action.compare("writemapAct")) 
    { 
#ifdef DEBUG
      cout << "debug: writemapAct button pressed" << endl;
#endif 
      if (elementListIsEmpty())
	return;
      if ((myparent->myBeamline()->position <= myparent->myBeamline()->elementzahl) && 
	  (myparent->myBeamline()->position != 0))
	{
	  printf("write map of element %d to file\n", myparent->myBeamline()->position); 

	  snprintf(header, MaxPathLength, "beamline: %s, map of element %d, iord: %d%d", 
		  myparent->myBeamline()->filenames.beamlinename, myparent->myBeamline()->position, 
		  myparent->myBeamline()->BLOptions.ifl.iord,0);
	  snprintf(buffer, MaxPathLength, "%s-%d", myparent->myBeamline()->filenames.mapname, myparent->myBeamline()->position);


	  /* casting 15.12.99 ist noch nicht OK */
	  writemapc(buffer, header, myparent->myBeamline()->BLOptions.ifl.iord, 
		    (double *)(myparent->myBeamline()->ElementList[myparent->myBeamline()->position- 1].ypc1), 
		    (double *) myparent->myBeamline()->ElementList[myparent->myBeamline()->position- 1].zpc1, 
		    (double *) myparent->myBeamline()->ElementList[myparent->myBeamline()->position- 1].dypc, 
		    (double *) myparent->myBeamline()->ElementList[myparent->myBeamline()->position- 1].dzpc,
		    (double *) myparent->myBeamline()->ElementList[myparent->myBeamline()->position- 1].wc, 
		    (double *) myparent->myBeamline()->ElementList[myparent->myBeamline()->position- 1].xlc, 
		    (double *) myparent->myBeamline()->ElementList[myparent->myBeamline()->position- 1].xlm.xlen1c, 
		    (double *) myparent->myBeamline()->ElementList[myparent->myBeamline()->position- 1].xlm.xlen2c);
	} 
      
      //  else wir schreiben hier immer beides
	{ 
	  printf("write map of beamline to file\n"); 

	  snprintf(header, MaxPathLength, "beamline: %s, map of beamline, iord: %d", 
		  myparent->myBeamline()->filenames.beamlinename, myparent->myBeamline()->BLOptions.ifl.iord);
	  snprintf(buffer, MaxPathLength, "%s-0", myparent->myBeamline()->filenames.mapname);

	  myparent->mywritemapc(buffer,  header,  
				myparent->myBeamline()->BLOptions.ifl.iord, 
				(double *) myparent->myBeamline()->ypc1, (double *) myparent->myBeamline()->zpc1, 
				(double *) myparent->myBeamline()->dypc, (double *) myparent->myBeamline()->dzpc,
				(double *) myparent->myBeamline()->wc,   (double *) myparent->myBeamline()->xlc, 
				(double *) myparent->myBeamline()->xlm.xlen1c, 
				(double *) myparent->myBeamline()->xlm.xlen2c);
	}
    } 

  if (!action.compare("writematAct")) 
    { 
      printf("writematAct button pressed\n");
      if (elementListIsEmpty())
	return;
      if ((myparent->myBeamline()->position <= myparent->myBeamline()->elementzahl) && 
	  (myparent->myBeamline()->position != 0))
	{
	  printf("write matrix of element %d to file\n", myparent->myBeamline()->position); 


	  snprintf(header, MaxPathLength, "beamline: %s, matrix of element %d, iord: %d, REDUCE_maps: %d\x00", 
		  myparent->myBeamline()->filenames.beamlinename, myparent->myBeamline()->position, 
		  myparent->myBeamline()->BLOptions.ifl.iord,
		  myparent->myBeamline()->BLOptions.REDUCE_maps);
	  snprintf(buffer, MaxPathLength, "%s-%d\x00", myparent->myBeamline()->filenames.matrixname, myparent->myBeamline()->position);

          writematrixfile((double *)myparent->myBeamline()->ElementList[myparent->myBeamline()->position- 1].M_StoI,
			  buffer, header, strlen(buffer), strlen(header)); // add hidden length parameter 
	} 
      
      //  else wir schreiben hier immer beides
	{ 
	  printf("activateProc: write matrix of beamline to file\n"); 

	  snprintf(header, MaxPathLength, "beamline: %s, matrix of beamline, iord: %d, REDUCE_maps: %d\x00", 
		  myparent->myBeamline()->filenames.beamlinename, myparent->myBeamline()->BLOptions.ifl.iord, 
		  myparent->myBeamline()->BLOptions.REDUCE_maps);
	  snprintf(buffer, MaxPathLength, "%s-0\x00", myparent->myBeamline()->filenames.matrixname);

	  writematrixfile((double *)myparent->myBeamline()->M_StoI, buffer, header, strlen(buffer), strlen(header));
	}
    } 

  if (!action.compare("writecoeffAct")) 
    { 
#ifdef DEBUG
      cout << "debug: writecoeffmapAct button pressed" << endl;
#endif
      if (elementListIsEmpty())
	return;
      if ((myparent->myBeamline()->position <= myparent->myBeamline()->elementzahl) && 
	  (myparent->myBeamline()->position != 0))
	{
	  printf("write coefficients of element %d to file\n", myparent->myBeamline()->position);
      //  snprintf(buffer, MaxPathLength, "%s", "mirror-coefficients.dat");
	  snprintf(buffer, MaxPathLength, "%s.coeff", elementList->currentItem()->text().toAscii().data());
	  printf("write coefficients to file: %s\n", buffer);
	  WriteMKos((struct mirrortype *)&myparent->myBeamline()->ElementList[myparent->myBeamline()->position- 1].mir, buffer);
	  statusBar()->showMessage(tr("Wrote mirror coefficients to file '%1'.").arg(buffer), 4000);
	} else fprintf(stderr, "%d: no valid position\n", myparent->myBeamline()->position); 
    }


  if (!action.compare("writesimpAct")) 
    { 
      printf("writesimpAct button pressed\n");
      writeSimp();
    }


  if (!action.compare("writeResultAct")) 
    { 
      printf("writereResultAct button pressed, result type: %d\n", myparent->myBeamline()->RESULT.typ); 
      if ((myparent->myBeamline()->RESULT.typ & PLphspacetype) > 0)
	{

	  cout << "write PO result to file " << myparent->myBeamline()->filenames.imageraysname << endl;
	  myparent->myWritePsd(myparent->myBeamline()->filenames.imageraysname, 
			       (struct PSDType *)myparent->myBeamline()->RESULT.RESp);
	}
      else
	{

	  cout << "write GO result to file " << myparent->myBeamline()->filenames.imageraysname << endl;
	  myparent->myWriteRayFile(myparent->myBeamline()->filenames.imageraysname, &myparent->myBeamline()->RESULT.points1,
				   (struct RayType *)myparent->myBeamline()->RESULT.RESp);
	}
    } 

  if (!action.compare("grscatterAct")) 
    { 
      printf("grscatterAct button pressed\n"); 
      mwplotstyle= PLOT_SCATTER;
      //delete d_plot;
      
      //d_plot =  new ScatterPlot(this);
      //d_plot->showSpectrogram(true);
    } 

  if (!action.compare("grcontourAct")) 
    { 
      mwplotstyle= PLOT_CONTOUR;
      d_plot->showContour(false);
      d_plot->showSpectrogram(true);
    } 

  if (!action.compare("grcontourisoAct")) 
    { 
      mwplotstyle= PLOT_CONTOURISO;
      d_plot->showSpectrogram(true);
      d_plot->showContour(true);
    } 
  if (!action.compare("grisoAct")) 
    { 
      mwplotstyle= PLOT_ISO;
      d_plot->showSpectrogram(false);
      d_plot->showContour(true);
    }

  if (!action.compare("grHorProfAct")) 
    { 
      mwplotstyle= PLOT_HPROF;
    }

  if (!action.compare("grVerProfAct")) 
    { 
      mwplotstyle= PLOT_VPROF;
    }

  if (!action.compare("grGoSourceSpaAct")) 
    {
      mwplotsubject= PLOT_GO_SOURCE | PLOT_GO_SPA ;
      updateGraphicsInput(mwplotsubject);
    }
  if (!action.compare("grGoSourceDivAct")) 
    {
      mwplotsubject= PLOT_GO_SOURCE | PLOT_GO_DIV ;
      updateGraphicsInput(mwplotsubject);
    }
  if (!action.compare("grGoSourcePhiAct")) 
    {
      mwplotsubject= PLOT_GO_SOURCE | PLOT_GO_PHI ;  
      updateGraphicsInput(mwplotsubject);
    }
  if (!action.compare("grGoResultSpaAct")) 
    {
      mwplotsubject= PLOT_GO_RESULT | PLOT_GO_SPA ;
      updateGraphicsInput(mwplotsubject);
      // cout << "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" << endl;
    }
  if (!action.compare("grGoResultDivAct")) 
    {
      mwplotsubject= PLOT_GO_RESULT | PLOT_GO_DIV ;
      updateGraphicsInput(mwplotsubject);
    }
  if (!action.compare("grGoResultPhiAct")) 
    {
      mwplotsubject= PLOT_GO_RESULT | PLOT_GO_PHI ;  
      updateGraphicsInput(mwplotsubject); 
    }
  if (!action.compare("grPoResultAct"   )) 
    {
      mwplotsubject= PLOT_PO_RESULT;
      updateGraphicsInput(mwplotsubject);
    }

  if (!action.compare("grPoSimpreAct")) 
    {
      mwplotsubject= PLOT_PO_SIMPRE;
      updateGraphicsInput(mwplotsubject);
    }

  if (!action.compare("grPoSimpimAct")) 
    {
      mwplotsubject= PLOT_PO_SIMPIM;
      updateGraphicsInput(mwplotsubject);
    }

  if (!action.compare("grPoSintreAct")) 
    {
      mwplotsubject= PLOT_PO_SINTRE;
      updateGraphicsInput(mwplotsubject);
    }

  if (!action.compare("grPoSintimAct")) 
    {
      mwplotsubject= PLOT_PO_SINTIM;
      updateGraphicsInput(mwplotsubject);
    }
  
  
  if (!action.compare("grexample1Act")) 
    { 
      mwplotsubject= PLOT_EXAMPLE1;
      //   d_plot->setTitle(tr("PhaseQt: example 1"));
      //   d_plot->setdefaultData();
    }

  if (!action.compare("grexample2Act")) 
    { 
      mwplotsubject= PLOT_EXAMPLE2;
      //  d_plot->setTitle(tr("PhaseQt: example 2"));
      //   d_plot->setdefaultData2();
    }

  if (!action.compare("grexample3Act")) 
    { 
      mwplotsubject= PLOT_EXAMPLE3;
      //  d_plot->setTitle(tr("PhaseQt: example 2"));
      //   d_plot->setdefaultData2();
    }

  if (!action.compare("readFg34Act")) 
    { 
      printf("readFg34Act button pressed\n"); 
      printf("Initialize parameters with fg34.par from J. Bahrdt\n"); 
      if (fexists((char *)"fg34.par") == 1)
	{
	  //	  correct but src not yet implemented readfg34_par(this->src, this->BLOptions.apr,
	  myparent->myreadfg34_par(&myparent->myBeamline()->src, &myparent->myBeamline()->BLOptions.apr,
				   &myparent->myBeamline()->BLOptions.ifl, &myparent->myBeamline()->BLOptions.xi,
				   &myparent->myBeamline()->BLOptions.epsilon);

	  strncpy(myparent->myBeamline()->filenames.so4_fsource4a, myparent->myBeamline()->src.so4.fsource4a, 80);
	  strncpy(myparent->myBeamline()->filenames.so4_fsource4b, myparent->myBeamline()->src.so4.fsource4b, 80);
	  strncpy(myparent->myBeamline()->filenames.so4_fsource4c, myparent->myBeamline()->src.so4.fsource4c, 80);
	  strncpy(myparent->myBeamline()->filenames.so4_fsource4d, myparent->myBeamline()->src.so4.fsource4d, 80);
	  strncpy(myparent->myBeamline()->filenames.so6_fsource6,  myparent->myBeamline()->src.so6.fsource6,  80);
	  if (c_window) c_window->updateList();
	  parameterUpdateAll(NPARS);
	} else
	QMessageBox::warning(this, tr("readFg34Act"),
			     tr("file fg34.par not found!"));
    } 

  if (!action.compare("poInitSourceAct")) 
    { 
      printf("poInitSourceAct button pressed\n"); 
            
      switch (myparent->myBeamline()->src.isrctype)
	{
#ifdef OBSOLETE
	case 2:
	  filesOK= fexists(myparent->myBeamline()->src.so2.fsource2a) & 
	    fexists(myparent->myBeamline()->src.so2.fsource2b);
	  break;
	case 3:
	  filesOK= fexists(myparent->myBeamline()->src.so3.fsource3a) & 
	    fexists(myparent->myBeamline()->src.so3.fsource3b);
 	  break;
#endif
	case 4: 
	  filesOK= fexists(myparent->myBeamline()->src.so4.fsource4a) & 
	    fexists(myparent->myBeamline()->src.so4.fsource4b) & 
	    fexists(myparent->myBeamline()->src.so4.fsource4c) &
	    fexists(myparent->myBeamline()->src.so4.fsource4d);
	  break;
	case 6:
	  filesOK= fexists(myparent->myBeamline()->src.so6.fsource6);
	  break;
	default:
	  QMessageBox::warning(this, tr("warning src_ini"),
			     tr("source type %1 : no files need to be read!\nreturn").
			     arg(myparent->myBeamline()->src.isrctype));
	  return;
	}

      if ( !filesOK )
	QMessageBox::warning(this, tr("error src_ini"),
			     tr("source type %1 : source file(s) not found!\nreturn").
			     arg(myparent->myBeamline()->src.isrctype));
      else
	{
#ifdef OLD_PO_SOURCE	  
	  cout << "call mysrc_ini" << endl;
	  myparent->mysrc_ini(&myparent->myBeamline()->src);
#else
	  cout << "call myposrc_ini" << endl;
	  myparent->myposrc_ini();
#endif
	  myparent->myBeamline()->beamlineOK |= pstsourceOK;
	}
    } // end poInitSourceAct

  if (!action.compare("configureAct")) 
    { 
      printf("configure button pressed\n");
      if (!c_window) 
	c_window= new ConfigWindow(myparent); 
      else 
	c_window->configWindowBox->show();
    }
  
  UpdateStatus();
} // end activateProc


// UF slot append a new optical element in the beamline box
void MainWindow::appendElement()
{
  struct ElementType *tmplist, *listpt, *tmplistpt;
  int i;
  int pos= elementList->count();
  if (pos < 0) pos= 0;  // empty list 
  if (abs((int)(myparent->myBeamline()->elementzahl)) > 1000) 
    myparent->myBeamline()->elementzahl= 0;  // fix falls elementzahl nicht initialisiert

#ifdef DEBUG
  printf("AddBLElement: AddItem at pos %d, out of %u\n", pos, myparent->myBeamline()->elementzahl);  
#endif 
 
  QListWidgetItem *item= new QListWidgetItem("New Element");
  elementList->insertItem(pos, item);
  item->setFlags (item->flags () | Qt::ItemIsEditable);               // edit item
  tmplist= XMALLOC(struct ElementType, myparent->myBeamline()->elementzahl); // alloc memory
  memcpy(tmplist, myparent->myBeamline()->ElementList, 
	 myparent->myBeamline()->elementzahl* sizeof(struct ElementType)); // copy contents
  myparent->myBeamline()->elementzahl++;
  myparent->myBeamline()->ElementList= XREALLOC(struct ElementType, myparent->myBeamline()->ElementList, 
						myparent->myBeamline()->elementzahl);
  listpt= myparent->myBeamline()->ElementList; tmplistpt= tmplist; 
  for (i= 0; i< (int)myparent->myBeamline()->elementzahl; i++, listpt++)
    {
#ifdef DEBUG
      printf("i= %d, pos= %d, nmax %u\n", i, pos, myparent->myBeamline()->elementzahl);
#endif
      if (i == pos)
	{
	  listpt->ElementOK= 0;
	  snprintf(listpt->elementname, MaxPathLength, "%s", "New Element");
	  minitdatset(&listpt->MDat);
	  listpt->MDat.Art= kEOETM;   // overwrite kEOEDefaults
	  ginitdatset(&listpt->GDat);
	  
	}
      else
	memcpy(listpt, tmplistpt++, sizeof(struct ElementType)); 
    }
  myparent->myBeamline()->beamlineOK &= ~(mapOK | resultOK);
  //  WriteBLFile(PHASESet.beamlinename, bl); 
  XFREE(tmplist);
  printf("inserElement: end list should have %u elements\n", myparent->myBeamline()->elementzahl);
  UpdateStatus();
  myparent->writeBackupFile();
} // appendElement


// calc slots in element box
void MainWindow::thetaBslot()  // SetTheta from cff
{
  double cff, alpha, beta, theta0;
  int number= elementList->currentRow();
  QString cffstr;

#ifdef DEBUG
  cout << "thetaBslot" << endl;
#endif

  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			 tr("(nothing selected)"));
      return;
    }

  struct gdatset *gdat= &(myparent->myBeamline()->ElementList[number].GDat);
  cff= cffE->text().toDouble();
  // !! we take other relevant data (gdat->lambda, gdat->xdens[0], gdat->inout) from dataset and not from widget
  
  if (cff != 1.0)
    {
      printf("fixfocus: %f, %lg\n", gdat->xdens[0], myparent->myBeamline()->BLOptions.lambda);
      FixFocus(cff, myparent->myBeamline()->BLOptions.lambda, gdat->xdens[0], gdat->inout, &alpha, &beta);
      theta0= (alpha- beta)* 90.0/ PI;
      if (gdat->azimut > 1) theta0= -fabs(theta0);
      thetaE->setText(cffstr.setNum(theta0, 'f', 6));  // update widget
      gdat->theta0= theta0;                            // update data
    } 
  else
    QMessageBox::warning(this, tr("Calculate theta from cff"),
			 tr("cff=1 is undefined\ntake no action"));
}

void MainWindow::sourceBslot() // copy source distance
{
  sourceE->setText(preE->text()); // copy text from widget, no update of datasets
}

void MainWindow::imageBslot()
{
  imageE->setText(sucE->text()); // copy text from widget, no update of datasets
}

void MainWindow::rhoBslot()  // calculate roho
{
  double theta, rho, source, image;
  QString rhostr;

#ifdef DEBUG
  cout << "debug: " << __FILE__ << " rhoBslot called" << endl;
#endif
    
  theta=  thetaE ->text().toDouble();
  source= sourceE->text().toDouble();
  image=  imageE ->text().toDouble();

  if (theta >= 90.0)
    QMessageBox::warning(this, tr("Calculate Radius"),
			 tr("theta %1 >= 90 deg.\ntake no action").arg(thetaE ->text()));
  else
    {
      rho= 2.0* source* image* cos(theta * PI/180.0)/ (source+ image); 
      rhoE->setText(rhostr.setNum(rho, 'g', 6));
    }
 } // rhoBslot

void MainWindow::rBslot()
{
  double theta, rmi, source, image, nenner;
  QString qst;
 
#ifdef DEBUG
  cout << "debug: " << __FILE__ << " rBslot called" << endl;
#endif

  theta=  thetaE ->text().toDouble();
  source= sourceE->text().toDouble();
  image=  imageE ->text().toDouble();
  
  if (theta >= 90.0)
    {
      QMessageBox::warning(this, tr("Calculate Radius"),
			   tr("theta %1 >= 90 deg.\ntake no action").arg(thetaE ->text()));
      return;
    }
  
  nenner= (source+ image)* cos(theta * PI/180.0);
  rmi= (fabs(nenner) > ZERO) ? (2.0* source* image)/ nenner : (nenner/fabs(nenner))/ZERO; 
  rE->setText(qst.setNum(rmi, 'g', 6));
} // rBslot  
// end calc slots

// debug
void MainWindow::debugslot()
{
  printf("debugslot activated\n");
} // end debugslot()

//
// UF slot delete optical element in the beamline box
//
void MainWindow::deleteElement()
{
  struct ElementType *tmplist, *listpt, *tmplistpt;
  QListWidgetItem *item;
  int i;
  int pos= elementList->currentRow();
 
#ifdef DEBUG
  printf("debug: deleteElement: delete element with idx %d out of %u\n", pos, myparent->myBeamline()->elementzahl);
#endif

  if (pos >= 0)
    {
      item= elementList->takeItem(pos);
      myparent->myBeamline()->elementzahl= elementList->count();
      if (item)
	{
	  printf("remove item %d, new count: %d\n", pos, myparent->myBeamline()->elementzahl);
	  delete item;
	} 
      else 
	printf("item unvalid \n");
      
      //#ifdef XXX
      printf ("widget deleted\n");

      if (myparent->myBeamline()->elementzahl > 0)
	{
	  tmplist= XMALLOC(struct ElementType, (myparent->myBeamline()->elementzahl + 1)); // alloc memory  of prev. length
	  memcpy(tmplist, myparent->myBeamline()->ElementList, 
		 (myparent->myBeamline()->elementzahl + 1)* sizeof(struct ElementType));   // copy contents of prev. length
	}

      XFREE(myparent->myBeamline()->ElementList);         // clean pointer

      if (myparent->myBeamline()->elementzahl > 0) 
	{
	  myparent->myBeamline()->ElementList= XMALLOC(struct ElementType,  
						       myparent->myBeamline()->elementzahl); // alloc memory of new length
	  
	  /* neu fuellen */
	  listpt= myparent->myBeamline()->ElementList; tmplistpt= tmplist; 
	  for (i= 0; i< (int)myparent->myBeamline()->elementzahl; i++, listpt++)  // in new list
	    {
	      if (i == pos)  tmplistpt++;  /* ueberlesen */
	      memcpy(listpt, tmplistpt++, sizeof(struct ElementType)); 
	    }
	  XFREE(tmplist);
	}
      myparent->myBeamline()->beamlineOK &= ~(mapOK | resultOK);
      printf("done\n");
    } 
  else
    QMessageBox::warning(this, tr("deleteElement"),
			 tr("can't delete anything, list is empty or nothing is selected!\n"));
  UpdateStatus();
  myparent->writeBackupFile();
} // deleteElement()

// slot changed dispersive length
void MainWindow::dislenSlot()
{
#ifdef DEBUG
  cout << "debug: " << __FILE__ << " dislenSlot called" << endl;
#endif

  myparent->myBeamline()->BLOptions.displength= dislenE->text().toDouble();
  myparent->myBeamline()->beamlineOK &= ~resultOK;
  UpdateStatus();
  myparent->writeBackupFile();
} // dislenSlot

// UF slot
void MainWindow::doubleclickElement()
{
  cout << "doubleclickElement: called" << endl;
} // doubleclickElement

// apply slot for optical element
void MainWindow::elementApplyBslot()
{
  int number= elementList->currentRow();
  
#ifdef DEBUG
  cout << "debug: " << __FILE__ << " elementApplyBslot activated" << endl;
#endif

  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			 tr("(nothing selected)"));
      return;
    }

  struct gdatset *gd= &(myparent->myBeamline()->ElementList[number].GDat);
  struct mdatset *md= &(myparent->myBeamline()->ElementList[number].MDat);

  myparent->myBeamline()->beamlineOK &= ~mapOK;
  myparent->myBeamline()->beamlineOK &= ~resultOK;
  myparent->myBeamline()->ElementList[number].ElementOK = 0;

  strncpy(myparent->myBeamline()->ElementList[number].elementname, 
	  elementList->currentItem()->text().toAscii().data(), MaxPathLength); // the name of the element
  
  cout << "elementApplyBslot: feed data from widget into dataset" << endl;

  gd->r     = preE   ->text().toDouble();
  gd->rp    = sucE   ->text().toDouble();
  gd->theta0= thetaE ->text().toDouble();      
  md->r1    = sourceE->text().toDouble(); 
  md->r2    = imageE ->text().toDouble(); 
  md->rmi   = rE     ->text().toDouble();         
  md->rho   = rhoE   ->text().toDouble(); 

  //  gd->lambdag = lambdagE->text().toDouble()*1E-6;
  gd->xdens[0]= lineDensity->text().toDouble(); 
  gd->xdens[1]= vls1->text().toDouble();
  gd->xdens[2]= vls2->text().toDouble();
  gd->xdens[3]= vls3->text().toDouble();
  gd->xdens[4]= vls4->text().toDouble();
  
  md->du    = duE ->text().toDouble();
  md->dw    = dwE ->text().toDouble();
  md->dl    = dlE ->text().toDouble();
  md->dRu   = dRuE->text().toDouble();
  md->dRw   = dRwE->text().toDouble();
  md->dRl   = dRlE->text().toDouble();
  md->w1    = w1E ->text().toDouble();
  md->w2    = w2E ->text().toDouble();
  md->slopew= wsE ->text().toDouble();
  md->l1    = l1E ->text().toDouble();
  md->l2    = l2E ->text().toDouble();
  md->slopel= lsE ->text().toDouble();

  md->dRu*= 1e-3;
  md->dRw*= 1e-3;
  md->dRl*= 1e-3;
  gd->inout= integerSpinBox->value();
  gd->iflag= (nimBox->isChecked() == true) ? 1 : 0;
  // build the element
  if (md->Art == kEOEGeneral )
    ReadCoefficientFile((double *)&(myparent->myBeamline()->ElementList[number].mir), 
			myparent->myBeamline()->ElementList[number].elementname);
  // we always call DefMirrorC to apply misalignment
  myparent->myDefMirrorC(md,   &(myparent->myBeamline()->ElementList[number].mir), md->Art, 
			   gd->theta0, myparent->myBeamline()->BLOptions.REDUCE_maps);
  
  myparent->myDefGeometryC(gd, &(myparent->myBeamline()->ElementList[number].geo));
  myparent->myMakeMapandMatrix(&(myparent->myBeamline()->ElementList[number]), number);
  //  myparent->myBeamline()->ElementList[number].ElementOK |= elementOK;
  UpdateStatus();
  myparent->writeBackupFile();
} // elementApplyBslot

// sigmabutton 
void MainWindow::fwhmslot()
{
#ifdef DEBUG
  cout << "debug: " << __FILE__ << " fwhmslot called" << endl;
#endif
  d_plot->fwhmon= 1;
  grapplyslot();
} // fwhmslot

// slot goButton
void MainWindow::goButtonslot()
{
#ifdef DEBUG
  cout << "debug: goButtonslot called" << endl;
#endif

  myparent->myBeamline()->BLOptions.SourcetoImage= 1;
  myparent->myBeamline()->beamlineOK &= ~resultOK;
  statGroup->show();
  
  if ( myparent->myBeamline()->BLOptions.dlambdaflag )
    {
      dlambdaBox1->setEnabled(true);
      dlambdaBox2->setEnabled(true);
    }
  dislenE->setEnabled(true);
  UpdateStatus();
  myparent->writeBackupFile();
} // goButtonslot


// gr apply
void MainWindow::grapplyslot()
{
  struct PSDType *psdp;

#ifdef DEBUG
  cout << endl << "debug: " << __FILE__ << " grapplyslot called, mwplotsubject=" << mwplotsubject << endl;
#endif

  // (1) a few tests
  
  if ((mwplotsubject & PLOT_GO_SOURCE) && !(myparent->myBeamline()->beamlineOK & sourceOK))
    {
      QMessageBox::warning(this, tr("grapplyslot"), tr("No GO source data available"));
      return;
    }

  if ((mwplotsubject & PLOT_GO_RESULT) && !(myparent->myBeamline()->beamlineOK & resultOK))
    {
      QMessageBox::warning(this, tr("grapplyslot"), tr("No valid GO results available"));
      return;
    }

  if ((mwplotsubject & PLOT_GO_RESULT) && 
      !checkResultType((struct RESULTType *)&myparent->myBeamline()->RESULT, PLrttype))
    return;

  if ((mwplotsubject & PLOT_PO_RESULT) && 
      !checkResultType((struct RESULTType *)&myparent->myBeamline()->RESULT, PLphspacetype))
    return;

  // (2) construct/destruct objects do common tasks
  switch (mwplotsubject)
    {
    case PLOT_PO_SIMPRE:
    case PLOT_PO_SIMPIM:
    case PLOT_PO_SINTRE:
    case PLOT_PO_SINTIM:
      if (d_plot) delete (d_plot); d_plot= NULL;
      if (zone)   delete (zone);   zone= NULL;
      zone= new Plot2x2(plotBox);
      plotLayout->addWidget(zone, 0, 0);
      psdp= (struct PSDType *)myparent->myBeamline()->RESULT.RESp;
      break;
    default: 
      //if (d_plot) delete (d_plot); d_plot= NULL;
      if (zone)   delete (zone);   zone= NULL;
      if (!d_plot) d_plot= new Plot(plotBox);
      // fill values from manual scaling or autoscale
      d_plot->Plot::ymin= gryminE->text().toDouble();
      d_plot->Plot::ymax= grymaxE->text().toDouble();
      d_plot->Plot::zmin= grzminE->text().toDouble();
      d_plot->Plot::zmax= grzmaxE->text().toDouble();
      d_plot->setPlotStyle(mwplotstyle);
      d_plot->setPlotSubject(mwplotsubject);
      plotLayout->addWidget(d_plot, 0, 0);
      break;
    }
  
    // (3) fill data, update statistics, update header

  if (mwplotsubject & PLOT_GO_SOURCE) // generic for GO source
    {
      d_plot->statistics((struct RayType *)myparent->myBeamline()->RTSource.SourceRays, 
			 myparent->myBeamline()->RTSource.raynumber, 
			 myparent->myBeamline()->deltalambdafactor,
			 myparent->myBeamline()->BLOptions.lambda);
      UpdateStatistics(d_plot, "Source", myparent->myBeamline()->RTSource.raynumber);
      d_plot->setTitle(tr("GO Source"));
      d_plot->fillGoPlotArrays((struct RayType *)myparent->myBeamline()->RTSource.SourceRays, 
			       myparent->myBeamline()->RTSource.raynumber, 0, 1);
    }

  if (mwplotsubject & PLOT_GO_RESULT) // generic for GO result
    {  
      d_plot->Plot::statistics((struct RayType *)myparent->myBeamline()->RESULT.RESp, 
			       myparent->myBeamline()->RESULT.points1, 
			       myparent->myBeamline()->deltalambdafactor,
			       myparent->myBeamline()->BLOptions.lambda);
      UpdateStatistics(d_plot, "Image", myparent->myBeamline()->RESULT.points1);
      d_plot->setTitle(tr("GO Result"));
      d_plot->fillGoPlotArrays((struct RayType *)myparent->myBeamline()->RESULT.RESp, 
			       myparent->myBeamline()->RESULT.points1, 
			       myparent->myBeamline()->RESULT.points2, 
			       myparent->myBeamline()->BLOptions.plrayset);
    }

  // (4) GO statistics done, [xyz]data arrays filled
  
  if (mwplotsubject & (PLOT_GO_SOURCE | PLOT_GO_RESULT)) // GO only
    {
      //plotLayout->removeWidget(d_plot);
      //if (plotLayout->count() > 0) plotLayout->removeWidget(btnLogy);
      switch (mwplotstyle)
	{
	case PLOT_ISO:
	case PLOT_CONTOUR:
	case PLOT_CONTOURISO:
	  d_plot->h2a_nx= d_plot->h2a_ny= BINS2;
	  d_plot->hfill2(myparent->myBeamline()->BLOptions.plrayset);
	  d_plot->setGoData("grsourceAct");
	  btnLogy->hide();
	  d_plot->contourPlot();
	  //plotLayout->addWidget(d_plot,0,0);
	  break;
	case PLOT_SCATTER:
	  btnLogy->hide();
	  d_plot->scatterPlot(myparent->myBeamline()->BLOptions.plrayset);
	  //plotLayout->addWidget(d_plot,0,0);
	  break;
	case PLOT_HPROF:
	  d_plot->hfill1(d_plot->getXdata(FIRST), d_plot->zmin, d_plot->zmax, FIRST);
	  if (myparent->myBeamline()->BLOptions.plrayset == 3) 
	    d_plot->hfill1(d_plot->getXdata(SECOND), d_plot->zmin, d_plot->zmax, SECOND);
	  btnLogy->show();
	  //plotLayout->addWidget(btnLogy,0,0);
	  //plotLayout->addWidget(d_plot,1,0,3,3);
	  d_plot->profilePlot(mwplotsubject, mwplotstyle, myparent->myBeamline()->BLOptions.plrayset);
	  break;
	case PLOT_VPROF:
	  d_plot->hfill1(d_plot->getYdata(FIRST), d_plot->ymin, d_plot->ymax, FIRST);
	  if (myparent->myBeamline()->BLOptions.plrayset == 3) 
	    d_plot->hfill1(d_plot->getYdata(SECOND), d_plot->ymin, d_plot->ymax, SECOND);
	  btnLogy->show();
	  //plotLayout->addWidget(btnLogy,0,0);
	  //plotLayout->addWidget(d_plot,1,0,3,3);
	  d_plot->profilePlot(mwplotsubject, mwplotstyle, myparent->myBeamline()->BLOptions.plrayset);
	  break;
	default:
	  cout << "error no valid mwplotstyle: " << mwplotstyle << endl;
	}
      
    } // end GO only
  else
    btnLogy->hide();
  
  // (5) example plots and PO
  switch (mwplotsubject)
    {   
    case PLOT_EXAMPLE1:
      d_plot->setTitle(tr("PhaseQt: example 1"));
      printf("aaaa1\n");
      d_plot->setdefaultData();
      printf("aaaa2\n");
      d_plot->contourPlot();
      break;
      
    case PLOT_EXAMPLE2:
      d_plot->setTitle(tr("PhaseQt: example 2"));
      d_plot->setdefaultData2();
      d_plot->contourPlot();
      break;
      
    case PLOT_EXAMPLE3:
      d_plot->setTitle(tr("PhaseQt: example 3"));
      d_plot->d_spectrogram->hide(); 
      d_plot->example3(); // fills the data
      d_plot->d_curve1->show();
      d_plot->d_curve2->show();
      //     d_plot->zoomer->setZoomBase(d_plot->canvas());
      d_plot->replot();
      break;
      
    case PLOT_PO_RESULT:
      cout << "plot PO_RESULT experimental start " << endl;
      cout << "use manual saling " << endl;
      cout << "!!! no tests !!! program may die if no PO data available! " << endl;
      
      d_plot->hfill2((struct PSDType *)myparent->myBeamline()->RESULT.RESp);
      d_plot->setPoData("grsourceAct");
      d_plot->contourPlot();
      
      cout << "plot PO_RESULT experimental end " << endl;
      break;

    case PLOT_PO_SIMPRE:
      cout << "plot PLOT_PO_SIMPRE start " << endl;
      zone->hfill4(psdp->simpre, myparent->myBeamline()->BLOptions.xi.ianzy0);
      //zone->setWindowTitle(QString("SIMPRE"));
      zone->myattach();
      break;

    case PLOT_PO_SIMPIM:
      cout << "plot PLOT_PO_SIMPIM start " << endl;
      zone->hfill4(psdp->simpim, myparent->myBeamline()->BLOptions.xi.ianzy0);
      // zone->setWindowTitle(QString("SIMPIM"));
      zone->myattach();
      break;

    case PLOT_PO_SINTRE:
      cout << "plot PLOT_PO_SINTRE start " << endl;
      zone->hfill4(psdp->sintre, myparent->myBeamline()->BLOptions.xi.ianzy0- 1);
      zone->myattach();
      break;

    case PLOT_PO_SINTIM:
      cout << "plot PLOT_PO_SINTIM start" << endl;
      zone->hfill4(psdp->sintim, myparent->myBeamline()->BLOptions.xi.ianzy0- 1);
      zone->myattach();
      break;
      
    } // end switch example data
  
    //d_plot->replot();
#ifdef DEBUG
  cout << "debug: grapplyslot end with replot" << endl;
#endif
  
} // grapply

// slot autscale
void MainWindow::grautoscaleslot()
{
  QString yminqst, ymaxqst, zminqst, zmaxqst;
  struct PSImageType *psip;
  struct PSDType     *psdp;

#ifdef DEBUG
  cout << "debug: " << __FILE__ << " grautoscaleslot called, mwplotsubject: " << mwplotsubject << endl;
#endif

// a few tests
  if (!d_plot)
    {
      cout << "no autoscale required for plotsubject= " <<  mwplotsubject << " return " << endl;
      return;
    }

  if ((mwplotsubject & PLOT_GO_SOURCE) && !(myparent->myBeamline()->beamlineOK & sourceOK))
    {
      QMessageBox::warning(this, tr("grautoscaleslot"), tr("No GO source data available"));
      return;
    }

  if ((mwplotsubject & PLOT_GO_RESULT) && !(myparent->myBeamline()->beamlineOK & resultOK))
    {
      QMessageBox::warning(this, tr("grautoscaleslot"), tr("No valid GO results available"));
      return;
    }
  // tests done 

  d_plot->setPlotSubject(mwplotsubject);

  if (mwplotsubject & PLOT_GO_SOURCE ) // generic for GO source
    {
      d_plot->fillGoPlotArrays((struct RayType *)myparent->myBeamline()->RTSource.SourceRays, 
			       myparent->myBeamline()->RTSource.raynumber, 0, 1);
      d_plot->autoScale();
    }

  if (mwplotsubject & PLOT_GO_RESULT  && 
      checkResultType((struct RESULTType *)&myparent->myBeamline()->RESULT, PLrttype))    // generic for GO result
    { 
      d_plot->fillGoPlotArrays((struct RayType *)myparent->myBeamline()->RESULT.RESp, 
			       myparent->myBeamline()->RESULT.points1, 
			       myparent->myBeamline()->RESULT.points2, 
			       myparent->myBeamline()->BLOptions.plrayset);
      d_plot->autoScale();
    }

  if (mwplotsubject & PLOT_PO_RESULT && 
      checkResultType((struct RESULTType *)&myparent->myBeamline()->RESULT, PLphspacetype)) // generic for PO result
    { 
      psip= (struct PSImageType *)myparent->myBeamline()->RTSource.Quellep;
      d_plot->autoScale(psip->zmin, psip->zmax, psip->ymin, psip->ymax);
    }

  
  // update the widget
  gryminE->setText(yminqst.setNum(d_plot->Plot::ymin, 'g', 4));
  grymaxE->setText(ymaxqst.setNum(d_plot->Plot::ymax, 'g', 4));
  grzminE->setText(zminqst.setNum(d_plot->Plot::zmin, 'g', 4));
  grzmaxE->setText(zmaxqst.setNum(d_plot->Plot::zmax, 'g', 4));
} // end slot autoscale

// slot grating
void MainWindow::grslot()
{
  int number= elementList->currentRow();

#ifdef DEBUG
  cout << "debug: grslot called" << endl;
#endif

  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			 tr("(nothing selected)"));
      return;
    }

  if (gratingGroup->isChecked() == true)
    myparent->myBeamline()->ElementList[number].MDat.Art |= GRATINGBIT; 
  else
    myparent->myBeamline()->ElementList[number].MDat.Art &= ~(GRATINGBIT); 

  
UpdateElementBox(number);
} // grslot

// slot grating vls
void MainWindow::grvlsslot()
{
  int number= elementList->currentRow();

#ifdef DEBUG
  printf("debug: grvlsslot called\n");
#endif

  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			 tr("(nothing selected)"));
      return;
    }

  if (vlsGroup->isChecked() == true)
    myparent->myBeamline()->ElementList[number].MDat.Art |= VLSBIT ; 
  else
    myparent->myBeamline()->ElementList[number].MDat.Art &= ~(VLSBIT) ;
 
  UpdateElementBox(number);
} // end grvlsslot

// UF slot insert a new optical element in the beamline box
void MainWindow::insertElement()
{

#ifdef DEBUG
  printf("debug: insertElement called\n");
#endif


  struct ElementType *tmplist, *listpt, *tmplistpt;
  int i;
  int pos= elementList->currentRow();
  if (pos < 0) pos= 0;  // empty list 
  if (abs((int)(myparent->myBeamline()->elementzahl)) > 1000) myparent->myBeamline()->elementzahl= 0;  // fix falls elementzahl nicht initialisiert

#ifdef DEBUG
  printf("AddBLElement: AddItem at pos %d, out of %u\n", pos, myparent->myBeamline()->elementzahl);  
#endif 
 
  QListWidgetItem *item= new QListWidgetItem("New Element");
  elementList->insertItem(pos, item);
  item->setFlags (item->flags () | Qt::ItemIsEditable);               // edit item
  tmplist= XMALLOC(struct ElementType, myparent->myBeamline()->elementzahl); // alloc memory
  memcpy(tmplist, myparent->myBeamline()->ElementList, 
	 myparent->myBeamline()->elementzahl* sizeof(struct ElementType)); // copy contents
  myparent->myBeamline()->elementzahl++;
  myparent->myBeamline()->ElementList= XREALLOC(struct ElementType, 
						myparent->myBeamline()->ElementList, 
						myparent->myBeamline()->elementzahl);
  listpt= myparent->myBeamline()->ElementList; tmplistpt= tmplist; 
  for (i= 0; i< (int)myparent->myBeamline()->elementzahl; i++, listpt++)
    {
#ifdef DEBUG
      printf("i= %d, pos= %d, nmax %u\n", i, pos, myparent->myBeamline()->elementzahl);
#endif
      if (i == pos)
	{
	  listpt->ElementOK= 0;
	  snprintf(listpt->elementname, MaxPathLength, "%s", "New Element");
	  minitdatset(&listpt->MDat);
	  listpt->MDat.Art= kEOETM;   // overwrite kEOEDefaults
	  ginitdatset(&listpt->GDat);
	  
	}
      else
	memcpy(listpt, tmplistpt++, sizeof(struct ElementType)); 
    }
  myparent->myBeamline()->beamlineOK &= ~(mapOK | resultOK);
  //  WriteBLFile(PHASESet.beamlinename, bl); 
  XFREE(tmplist);
  printf("inserElement: end list should have %u elements\n", myparent->myBeamline()->elementzahl);
} // insertElement

// slot changed  lambda
void MainWindow::lambdaSlot()
{
  unsigned int i;

#ifdef DEBUG
  cout << "lambdaSlot called" << endl;
#endif

  myparent->myBeamline()->BLOptions.lambda= lambdaE->text().toDouble()* 1e-6;
  myparent->myBeamline()->beamlineOK= 0;
  for (i=0; i< myparent->myBeamline()->elementzahl; i++) 
    myparent->myBeamline()->ElementList[i].ElementOK= 0;
  UpdateStatus();
} // lambdaSlot

// slot changed  lambda
void MainWindow::dlambdaSlot()
{
  unsigned int i;

#ifdef DEBUG
  cout << "dlambdaSlot called" << endl;
#endif

  myparent->myBeamline()->BLOptions.dlambda= dlambdaE->text().toDouble()* 1e-6;
  myparent->myBeamline()->beamlineOK= 0;
  for (i=0; i< myparent->myBeamline()->elementzahl; i++) 
    if (myparent->myBeamline()->ElementList[i].MDat.Art & GRATINGBIT) 
      myparent->myBeamline()->ElementList[i].ElementOK= 0;
  UpdateStatus();
#ifdef DEBUG
  cout << "debug: dlambda= " << myparent->myBeamline()->BLOptions.dlambda *1e6 << " nm" << endl;
#endif
} // lambdaSlot

// misaliBoxslot
void MainWindow::misaliBoxslot(int newstate)
{
#ifdef DEBUG
  cout << "misaliBoxslot called: new state: " << newstate << endl;
#endif
  
  myparent->myBeamline()->BLOptions.WithAlign= (newstate == Qt::Checked) ? 1 : 0;
  myparent->myBeamline()->beamlineOK= 0;
  for (int i=0; i< myparent->myBeamline()->elementzahl; i++) 
    myparent->myBeamline()->ElementList[i].ElementOK= 0;
  UpdateStatus();
} // misaliBoxslot

// dlambdaBoxslot
void MainWindow::dlambdaBoxslot(int newstate)
{
#ifdef DEBUG
  cout << "dlambdaBoxslot called: new state: " << newstate << endl;
#endif
  
  myparent->myBeamline()->BLOptions.dlambdaflag= (newstate == Qt::Checked) ? 1 : 0;
  UpdateBeamlineBox();
#ifdef DEBUG
  cout << "debug: dlambdaBoxslot out:dlambdaflag= " <<  myparent->myBeamline()->BLOptions.dlambdaflag <<  endl;
#endif
} // dlambdaBoxslot

// dlambdaBoxslot
void MainWindow::dlambdaBox1slot(int newstate)
{
  myparent->myBeamline()->BLOptions.plrayset= (newstate == Qt::Checked) ? 
    myparent->myBeamline()->BLOptions.plrayset | PLRaySet1 : myparent->myBeamline()->BLOptions.plrayset & PLRaySet2;
#ifdef DEBUG
  cout << "debug: dlambdaBox1slot out:= plrayset" <<  myparent->myBeamline()->BLOptions.plrayset <<  endl;
#endif
} // dlambdaBox1slot

// dlambdaBoxslot
void MainWindow::dlambdaBox2slot(int newstate)
{
  myparent->myBeamline()->BLOptions.plrayset= (newstate == Qt::Checked) ? 
    myparent->myBeamline()->BLOptions.plrayset | PLRaySet2 : myparent->myBeamline()->BLOptions.plrayset & PLRaySet1;
#ifdef DEBUG
  cout << "debug: dlambdaBox2slot out:= plrayset" <<  myparent->myBeamline()->BLOptions.plrayset <<  endl;
#endif
} // dlambdaBox2slot

// slot called to read in a new beamline
void MainWindow::newBeamline()
{
  // int rcode;
  const char *name= "new_beamline.phase";

  if ( fexists((char *)name)) 
    QMessageBox::warning(this, tr("Phase: newBeamline"),
			 tr("File %1. already exists but we do not read it!\n 'Save as' will overwite it!").arg(name));
  
  myparent->myBeamline()->beamlineOK= 0;
  //myparent->myBeamline()->myPHASEset::init(name);
  myparent->initSet(name);
  myparent->myPutPHASE((char*) MainPickName);
  myparent->myBeamline()->RTSource.QuellTyp = 'H';                /* set default Quelltyp   */
  myparent->myAllocRTSource();                          /* reserves source memory */
  myparent->myBeamline()->RTSource.raynumber= 0;                  /* set default raynumber  */
  XFREE(myparent->myBeamline()->RTSource.SourceRays);
  myparent->myBeamline()->elementzahl = 0; 
  XFREE(myparent->myBeamline()->ElementList);                     /* clean up memory of elements  */
  myparent->myBeamline()->RESULT.points1= 0;
  myparent->myBeamline()->RESULT.points2= 0;
  FreeResultMem(&myparent->myBeamline()->RESULT);
  myparent->myBeamline()->BLOptions.lambda= 3.1e-6;                /* 400 ev */
  myparent->myBeamline()->BLOptions.displength= 5000;
  myparent->myBeamline()->BLOptions.SourcetoImage= 1;
  myparent->myBeamline()->BLOptions.WithAlign= 0;
  UpdateElementList();
  UpdateBeamlineBox();
  UpdateSourceBox();
  parameterUpdateAll(NPARS);
  statusBar()->showMessage(tr("New Beamline '%1' created!").arg(name), 4000);
} // end newBeamline()

// slot called to read in a new beamline
void MainWindow::openBeamline()
{
  int rcode;
  
#ifdef DEBUG
  cout << "Debug: slot openBeamline activated" << endl;
  //  myQtPhase->myPHASEset::print();
#endif
  QString fileName = QFileDialog::getOpenFileName(this, tr("Open File"), 
						  QDir::currentPath(),
						  //"/afs/psi.ch/user/f/flechsig/phase/data",
						  tr("Phase files (*.phase);;(*)")
						  );
  char *name;
  //  int result;
  //myparent->myBeamline()->QtPhase::print();

  #warning unresolved spurious bug
  //TODO: SG found spurious bug: on pc7753, if the string in variable filename 
  // has the length 56, toAscii().data() points to a C-string of the length 57
  // and this filename will then not be found;
  // this is either a bug in QT on pc7753 or some buffer overwrite in PhaseQT;
  // for now, I'll just check that the lengths match and leave a note
  // QString str56 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
  name= fileName.toAscii().data();
  if (strlen(name) != fileName.length())
  {
    cerr << "ERROR: Conversation error of fileName string in " << __FILE__ << '.' << endl;
    cerr << "Please report the bug and use a filename with a different length for now." << endl;
    exit(-1);
  }
  
  
  if (!fileName.isEmpty()) 
    {
      name= fileName.toAscii().data();
      cout << "MainWindow::newBeamline: try to read file: " << name << endl;
      rcode= myparent->myReadBLFile(name);
      if (rcode != -1)
	{
	  //myparent->myBeamline()->myPHASEset::init(name);
	  myparent->initSet(name);
	  UpdateElementList();
	  UpdateBeamlineBox();
	  UpdateSourceBox();
	  parameterUpdateAll(NPARS);
	  myparent->myBeamline()->beamlineOK= 0;
	  myparent->myPutPHASE((char*) MainPickName);
	} 
      else
	QMessageBox::information(this, tr("Phase: newBeamline"),
				 tr("Cannot load %1.\n Wrong file type or file not found!").arg(fileName));
    }
  //myparent->myBeamline()->myPHASEset::print();
  UpdateStatus();
} // end openBeamline()

// slot parameter update, callback des Editors
void MainWindow::parameterUpdateSlot()
{
  unsigned int i;
  int index;

#ifdef DEBUG
  cout << "debug: parameterUpdateSlot called, file: " <<  __FILE__ << endl;
#endif

  index= parameterList->currentRow();      //old list model
  index= parameterModel->getActualIndex(); // treemodel
  parameterUpdate(index, parameterE->text().toAscii().data(), 0); // updates the old list
  parameterModel->updateItemVal(parameterE->text(), parameterModel->getActualIndex());

  myparent->myBeamline()->beamlineOK &= ~mapOK;
  myparent->myBeamline()->beamlineOK &= ~resultOK;
  for (i=0; i< myparent->myBeamline()->elementzahl; i++) 
    myparent->myBeamline()->ElementList[i].ElementOK= 0;
  UpdateStatus();
  myparent->writeBackupFile();
} // end parameterUpdateSlot

// slot goButton
void MainWindow::poButtonslot()
{
#ifdef DEBUG
  cout << "poButtonslot called" << endl;
#endif

  myparent->myBeamline()->BLOptions.SourcetoImage= 2;
  myparent->myBeamline()->beamlineOK &= ~resultOK;
  statGroup->hide();
  dlambdaBox1->setEnabled(false);
  dlambdaBox2->setEnabled(false);
  dislenE->setEnabled(false);
  UpdateStatus();
  myparent->writeBackupFile();
} // poButtonslot


// slot orientation radio buttons
void MainWindow::rup1slot()
{
  int number= elementList->currentRow();
  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			 tr("(nothing selected)"));
      return;
    }
  myparent->myBeamline()->ElementList[number].GDat.azimut= 0;
  myparent->myBeamline()->ElementList[number].GDat.theta0= 
    fabs(myparent->myBeamline()->ElementList[number].GDat.theta0);
  UpdateElementBox(number);
}

void MainWindow::rleft2slot()
{
  int number= elementList->currentRow();
  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			   tr("(nothing selected)"));
      return;
    }
  myparent->myBeamline()->ElementList[number].GDat.azimut= 1;
  myparent->myBeamline()->ElementList[number].GDat.theta0= 
    fabs(myparent->myBeamline()->ElementList[number].GDat.theta0);
  UpdateElementBox(number);
}

void MainWindow::rdown3slot()
{
  int number= elementList->currentRow();
  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			   tr("(nothing selected)"));
      return;
    }
  myparent->myBeamline()->ElementList[number].GDat.azimut= 2;
  myparent->myBeamline()->ElementList[number].GDat.theta0= 
    -fabs(myparent->myBeamline()->ElementList[number].GDat.theta0);
  UpdateElementBox(number);
}

void MainWindow::rright4slot()
{
  int number= elementList->currentRow();
  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			   tr("(nothing selected)"));
      return;
    }
  myparent->myBeamline()->ElementList[number].GDat.azimut= 3;
  myparent->myBeamline()->ElementList[number].GDat.theta0= 
    -fabs(myparent->myBeamline()->ElementList[number].GDat.theta0);
  UpdateElementBox(number);
} 
// end slot orientation radio buttons

// slot
void MainWindow::print()
{
#ifdef DEBUG
  cout << "debug: MainWindow::print called" << endl;
#endif

#ifndef QT_NO_PRINTDIALOG
  //QTextDocument *document = textEdit->document();
  //   QPrinter printer;
#if 1
    QPrinter printer;
#else
    QPrinter printer(QPrinter::HighResolution);
#endif

    QPrintDialog *dlg = new QPrintDialog(&printer, this);
    
    if (dlg->exec() != QDialog::Accepted)
        return;

    //  document->print(&printer);
    //printer.setOrientation(QPrinter::Landscape);
    //render( &printer );
    d_plot->printPlot( printer );
    statusBar()->showMessage(tr("Ready"), 4000);
#endif
} // end print()

// slot
void MainWindow::printMain()
{
#ifdef DEBUG
  cout << "debug: MainWindow::printMain called" << endl;
#endif

  QPrinter printer(QPrinter::ScreenResolution);
  //QPrinter printer(QPrinter::HighResolution);
  printer.setOrientation(QPrinter::Landscape);
  printer.setResolution((int)(printer.resolution()*1.3));
  
  QPrintDialog *dlg = new QPrintDialog(&printer, this);
    
  if (dlg->exec() != QDialog::Accepted)
    return;
  
  this->render( &printer );
  
  statusBar()->showMessage(tr("Ready"), 4000);
} // end printMain()

// slot
void MainWindow::screenshotMain()
{
#ifdef DEBUG
  cout << "debug: MainWindow::screenshotMain called" << endl;
#endif

  QString format = "png";
  QPixmap pixmap = QPixmap::grabWindow(this->winId());
 
  QString initialPath = QDir::currentPath() + tr("/screenshot.") + format;

  QString fileName = QFileDialog::getSaveFileName(this, tr("Save As"),
                                initialPath,
                                tr("%1 Files (*.%2);;All Files (*)")
                                .arg(format.toUpper())
                                .arg(format));
  if (!fileName.isEmpty())
    pixmap.save(fileName, format.toAscii().constData());

} // end printMain()


// slot
void MainWindow::save()
{
  char *name= myparent->myBeamline()->filenames.beamlinename;
  myparent->myWriteBLFile(name);
  statusBar()->showMessage(tr("Saved '%1'").arg(name), 4000);
} // end save

// slot
void MainWindow::saveas()
{
    char *name;
    QString fileName = QFileDialog::getSaveFileName(this,
                        tr("Choose a file name"), ".",
                        tr("PHASE (*.phase)"));
    if (fileName.isEmpty())
        return;

    name= fileName.toAscii().data();

    QFile file(fileName);
    if (!file.open(QFile::WriteOnly | QFile::Text)) {
        QMessageBox::warning(this, tr("PHASE Qt"),
                             tr("Cannot write file %1:\n%2.")
                             .arg(fileName)
                             .arg(file.errorString()));
        return;
    }
    //myparent->myBeamline()->myPHASEset::init(name);
    myparent->initSet(name);
    myparent->myPutPHASE((char*) MainPickName);
    myparent->myWriteBLFile(name);
    UpdateBeamlineBox();
    //    QTextStream out(&file);
    //    QApplication::setOverrideCursor(Qt::WaitCursor);
    //    out << textEdit->toHtml();
    //   QApplication::restoreOverrideCursor();

    statusBar()->showMessage(tr("Saved '%1'").arg(fileName), 4000);
} // end save

// UF selection slot
void MainWindow::selectElement()
{
  QListWidgetItem *item;
  int elementnumber= elementList->currentRow();
 
#ifdef DEBUG
  cout << "debug: selectElement called, selected: " << elementnumber << endl;
#endif
  
  if (elementnumber < 0) return;

  myparent->myBeamline()->position= elementnumber+ 1;
  item= elementList->currentItem();
  groupBox1->setTitle(item->text());  // set text header
  UpdateElementBox(elementnumber);
  debug_beamline_type_f((int *)myparent->myBeamline());
} // selectElement

// UF selection slot
void MainWindow::selectParameter()
{
#ifdef DEBUG
  cout << "debug: selectParameter called" << endl;
#endif
  char buffer[MaxPathLength], *ch;
  int parameternumber= parameterList->currentRow();
  
  if (parameternumber < 0) 
    return;

  strncpy(buffer, parameterList->currentItem()->text().toAscii().data(), MaxPathLength);
  ch= strchr(buffer, ':');
  if (ch != NULL) 
    {
      *ch= '\0';
      parameterE->setText(buffer);
    }
 } // selectParameter

// slot shapeMenu
// slot shapeMenu plane mirror
void MainWindow::pmslot()
{
  int number= elementList->currentRow();
if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			 tr("(nothing selected)"));
      return;
    }
  myparent->myBeamline()->ElementList[number].MDat.Art= kEOEPM;
  myparent->myBeamline()->ElementList[number].MDat.rmi= 0.0;
  myparent->myBeamline()->ElementList[number].MDat.rho= 0.0;
  UpdateElementBox(number);
}

// slot shapeMenu toroidal mirror
void MainWindow::toslot()
{
  int number= elementList->currentRow();
  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			   tr("(nothing selected)"));
      return;
    }
  myparent->myBeamline()->ElementList[number].MDat.Art= kEOETM;
  UpdateElementBox(number); 
}

// slot shapeMenu plane elliptical mirror
void MainWindow::peslot()
{
  int number= elementList->currentRow();
  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			   tr("(nothing selected)"));
      return;
    }
  myparent->myBeamline()->ElementList[number].MDat.Art= kEOEPElli;
  UpdateElementBox(number); 
}

// slot shapeMenu elliptical mirror
void MainWindow::elslot()
{
  int number= elementList->currentRow();
  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			   tr("(nothing selected)"));
      return;
    }
  myparent->myBeamline()->ElementList[number].MDat.Art= kEOEElli;
  UpdateElementBox(number); 
}

// slot shapeMenu conical mirror
void MainWindow::coslot()
{
  int number= elementList->currentRow();
  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			   tr("(nothing selected)"));
      return;
    }
  myparent->myBeamline()->ElementList[number].MDat.Art= kEOECone;
  UpdateElementBox(number); 
}

// slot shapeMenu generic shape
void MainWindow::geslot()
{
  int number= elementList->currentRow();
  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			   tr("(nothing selected)"));
      return;
    }
  myparent->myBeamline()->ElementList[number].MDat.Art= kEOEGeneral;
  QMessageBox::information(this, tr("gerneric element slot"),
			   tr("The elementname must be the file name!\nsave and reload the beamline!"));
  UpdateElementBox(number); 
}

// slot shapeMenu generic shape
void MainWindow::apslot()
{
  int number= elementList->currentRow();
  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			   tr("(nothing selected)"));
      return;
    }
  myparent->myBeamline()->ElementList[number].MDat.Art= kEOESlit;
  UpdateElementBox(number); 
}
// end slots shapeMenu


// sigmabutton 
void MainWindow::sigmaslot()
{
#ifdef DEBUG
  cout << "debug: " << __FILE__ << " sigmaslot called" << endl;
#endif
  d_plot->fwhmon= 0;
  grapplyslot();
} // sigmaslot

// apply slot for source
void MainWindow::sourceDefaultBslot()
{
#ifdef DEBUG
  printf("sourceDefaultBslot activated\n");
#endif
  //sourceSetDefaults();
  oldsource='0';    // something not valid
  UpdateSourceBox();
  //  QMessageBox::warning(this, tr("sourceDefaultBslot"),
  //			   tr("no function so far"));
} // end sourceDefaultBslot


// apply slot for source
void MainWindow::sourceApplyBslot()
{
  int sou;
  double lambda;
  struct UndulatorSourceType  *up;
  struct UndulatorSource0Type *up0;
  struct DipolSourceType      *dp;
  struct PointSourceType      *sop;
  struct RingSourceType       *rp;
  struct HardEdgeSourceType   *hp;  
  struct PSImageType          *psip;
  //struct SRSourceType         *sp; 
  //struct PSSourceType         *pssp; 

#ifdef DEBUG
  cout << "debug: " << __FILE__ << " sourceApplyBslot activated" << endl;
#endif

  if (myparent->myBeamline()->RTSource.Quellep == NULL )
    {
      cout << "error: sourceApplyBslot: Quellep == NULL" << endl;
      return;
    }
    
  sou= myparent->myBeamline()->RTSource.QuellTyp;
  switch (sou) 
    {
    case 'D':
      dp= (struct DipolSourceType *)myparent->myBeamline()->RTSource.Quellep;
      dp->sigy = S1E->text().toDouble();
      dp->sigdy= S2E->text().toDouble();
      dp->sigz = S3E->text().toDouble();
      dp->dz   = S4E->text().toDouble();
      myparent->myBeamline()->RTSource.raynumber= S5E->text().toInt();
      break;
    case 'G':
      up0= (struct UndulatorSource0Type *)myparent->myBeamline()->RTSource.Quellep;
      up0->length= S1E->text().toDouble();
      lambda     = S2E->text().toDouble();
      myparent->myBeamline()->RTSource.raynumber= S3E->text().toInt();
      up0->deltaz  = S4E->text().toDouble();
      up0->sigmaey = S5E->text().toDouble();
      up0->sigmaez = S6E->text().toDouble();
      up0->sigmaedy= S7E->text().toDouble();
      up0->sigmaedz= S8E->text().toDouble();
      break;
    case 'H':
      hp= (struct  HardEdgeSourceType *)myparent->myBeamline()->RTSource.Quellep;
      hp->disty= S1E->text().toDouble();
      hp->iy   = S2E->text().toInt();
      hp->distz= S3E->text().toDouble();
      hp->iz   = S4E->text().toInt();
      hp->divy = S5E->text().toDouble();
      hp->idy  = S6E->text().toInt();
      hp->divz = S7E->text().toDouble();
      hp->idz  = S8E->text().toInt();
      myparent->myBeamline()->RTSource.raynumber=  hp->iy* hp->idy* hp->iz* hp->idz;
      break;
    case 'I':
      psip= (struct PSImageType *)myparent->myBeamline()->RTSource.Quellep;
      psip->ymin= S1E->text().toDouble();
      psip->zmin= S2E->text().toDouble();
      psip->ymax= S3E->text().toDouble();
      psip->zmax= S4E->text().toDouble();
      psip->iy  = S5E->text().toInt();
      psip->iz  = S6E->text().toInt();
      break;
      
    case 'L':
    case 'M':
      up= (struct UndulatorSourceType *)myparent->myBeamline()->RTSource.Quellep;
      up->length= S1E->text().toDouble();
      lambda    = S2E->text().toDouble();
      myparent->myBeamline()->RTSource.raynumber= S3E->text().toInt();
      up->deltaz= S4E->text().toDouble();
      break;
      
    case 'o':
      sop= (struct PointSourceType *)myparent->myBeamline()->RTSource.Quellep;
      sop->sigy  = S1E->text().toDouble();
      sop->sigdy = S2E->text().toDouble();
      sop->sigz  = S3E->text().toDouble();
      sop->sigdz = S4E->text().toDouble();
      myparent->myBeamline()->RTSource.raynumber= S5E->text().toInt();
      break;
      
    case 'R':
      rp= (struct RingSourceType *)myparent->myBeamline()->RTSource.Quellep;
      rp->dy= S1E->text().toDouble();
      rp->dz= S2E->text().toDouble();
      myparent->myBeamline()->RTSource.raynumber= S3E->text().toInt();
      break;
      
    case 'U':
      up= (struct UndulatorSourceType *)myparent->myBeamline()->RTSource.Quellep;
      up->length= S1E->text().toDouble();
      lambda    = S2E->text().toDouble();
      myparent->myBeamline()->RTSource.raynumber= S3E->text().toInt();
      break;
    case 'F':
      // hier muss nichts gemacht werden
      break;
      
    default:
      QMessageBox::warning(this, tr("sourceApplyBslot"),
			   tr("Source type %1 not recognized.\nreport bug to uwe.flechsig@psi.ch")
			   .arg(sou));
      return;
    }
  
  myparent->myBeamline()->BLOptions.wrSource = (sourceFileBox->isChecked() == true) ?  1 : 0;  
  if (sou != 'I') 
    {
      myparent->myMakeRTSource();
      myparent->myBeamline()->beamlineOK |= sourceOK;
    } 
  else
    {
      myparent->myBeamline()->beamlineOK |= pstimageOK;
    }
  myparent->myBeamline()->beamlineOK &= ~resultOK;
  UpdateStatus();
  myparent->writeBackupFile();
} //sourceApplyBslot

// AutoGuess button slot for source (OE image plane only)
void MainWindow::sourceAutoGuessBslot()
{
  struct BeamlineType *bl;
  struct PSImageType *psip;
  
#ifdef DEBUG
  cout << "debug: " << __FILE__ << " sourceAutoGuessBslot activated" << endl;
#endif

  bl = myparent->myBeamline();

  if (elementListIsEmpty()) 
    return;

  // check beamline status
  bl->beamlineOK &= ~resultOK;
  UpdateStatus();

 
  myparent->myBuildBeamline();

  if (!(bl->beamlineOK & pstsourceOK))
  {
#ifdef OLD_PO_SOURCE
    myparent->mysrc_ini(&bl->src); 
#else
    myparent->myposrc_ini();
#endif
    bl->beamlineOK |= pstsourceOK;
  }

  if (!(bl->beamlineOK & pstimageOK)) 
    sourceApplyBslot();
      
  if (CheckBLOK(bl->beamlineOK, 
    (pstsourceOK | mapOK | pstimageOK), (char *)"act_pr: ") > 0)
  {
    psip = (struct PSImageType *)bl->RTSource.Quellep;
    myparent->myReAllocResult(PLphspacetype, psip->iy, psip->iz);
    
    // get guessed values for range
    myparent->myFindIntRange();
  }
  
  // update GUI  
/*  S1E->setText(QString().setNum(psip->ymin, 'g', 4));
  S2E->setText(QString().setNum(psip->zmin, 'g', 4));    
  S3E->setText(QString().setNum(psip->ymax, 'g', 4));  
  S4E->setText(QString().setNum(psip->zmax, 'g', 4));  */
   
  parameterModel->updateItemVal(QString().setNum(bl->BLOptions.xi.ymin*1e3, 'g', 4), 28);
  parameterModel->updateItemVal(QString().setNum(bl->BLOptions.xi.ymax*1e3, 'g', 4), 29);
  parameterModel->updateItemVal(QString().setNum(bl->BLOptions.xi.zmin*1e3, 'g', 4), 35);
  parameterModel->updateItemVal(QString().setNum(bl->BLOptions.xi.zmax*1e3, 'g', 4), 36);
    

  UpdateStatus();
  myparent->writeBackupFile();
} // sourceApplyBslot



// slot
void MainWindow::undo()
{
  cout << "undo button- no action so far" << endl;
  QMessageBox::information(this, tr("Undo Button"),
			   tr("no action so far")
			  );
  //QTextDocument *document = textEdit->document();
  //  document->undo();
} // undo

void MainWindow::abort_thread()
{
  future->cancel();
  qDebug() << "Task aborted";
}

void MainWindow::finished_thread()
{
  statusBar()->showMessage(tr("asynchronous task finished!"));
  qDebug() << "Task finished";
  UpdateStatus();
}

void MainWindow::pause_thread()
{
  future->pause();
  qDebug() << "Task paused- press Resume or Abort";
}

void MainWindow::resume_thread()
{
  qDebug() << "Task resumed- wait for finsihed or  Abort";
  future->resume();
}


///////////////////////
// end slots section //
///////////////////////


// end /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/mainwindow_slots.cpp
