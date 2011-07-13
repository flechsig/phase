//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/mainwindow.cpp
//  Date      : <31 May 11 17:02:14 flechsig> 
//  Time-stamp: <2011-07-14 00:06:21 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 

// this file contains the QT gui
// the widgets and slots
// we try to keep the functions in alphabetic order within each section
// 1) constructor
// 2) slots
// 3) widget definition
// 4) widget handling

#include <QtGui>

#include "mainwindow.h"
#include "qtphase.h"

// the constructor of the main window
MainWindow::MainWindow()
{
  graphicBox= createGraphicBox();
  setCentralWidget(graphicBox);
  createActions();
  createMenus();
  createToolBars();
  createStatusBar();
  createDockWindows();
  setWindowTitle(tr("PHASE Qt"));
  resize(1800,1000);

//InitDataSets(this, this, (char*) MainPickName);   // I call the contents here 
  if (GetPHASE(this, (char*) MainPickName) != 1)           /* filenamen lesen */
    { 
      this->myPHASEset::init("default");              // default filenames
      PutPHASE(this, (char*) MainPickName);          	/* write names */   
    }
  this->QtPhase::print();
  this->myBeamline::init();                           // empty pointers
  ReadBLFile(this->beamlinename, this);
  UpdateElementList();
  UpdateBeamlineBox();
  UpdateSourceBox();
} // end MainWindow

//////////////////////////////////////////////
// begin slots section (callback functions) //
//////////////////////////////////////////////

// slot
void MainWindow::about()
{
   QMessageBox::about(this, tr("About PHASE Qt"),
            tr("The <b>PHASE Qt</b> example demonstrates how to "
               "use Qt's dock widgets. You can enter your own text, "
               "click a customer to add a customer name and "
               "address, and click standard paragraphs to add them."));
} // about

// UF slot
// here we call our own code dependign on which button has been pressed
void MainWindow::activateProc(const QString &action)
{
  char buffer[MaxPathLength], header[MaxPathLength];

  if (action.isEmpty())
          return;
  
  if (!action.compare("raytracesimpleAct")) 
    { 
      printf("\nraytracesimpleAct button  pressed, localalloc: %d hormaps_loaded: %d\n", this->localalloc, this->hormapsloaded);
      
      MakeRTSource(this, this);
      ReAllocResult(this, PLrttype, this->RTSource.raynumber, 0);  
      BuildBeamline(this);
      RayTracec(this); 
      printf("ray trace-> done\n");
    }
  if (!action.compare("raytracefullAct")) 
    { 
      printf("\nraytracefullAct button  pressed\n");
      
      MakeRTSource(this, this);
      ReAllocResult(this, PLrttype, this->RTSource.raynumber, 0);  
      BuildBeamline(this);
      RayTraceFull(this); 
      printf("full ray trace-> done\n");
    }
  if (!action.compare("footprintAct")) 
    { 
      printf("\nfootprintAct button  pressed\n");
      
      this->localalloc= DOALLOC;   // fix wrong initialization
      if (this->hormapsloaded != 1) this->hormapsloaded= 0;      // fix

      MakeRTSource(this, this);
      ReAllocResult(this, PLrttype, this->RTSource.raynumber, 0);  
      BuildBeamline(this);
      Footprint(this, this->position);
      printf("footprint-> done\n");
    }


  if (!action.compare("phasespaceAct"))     printf("phasespaceAct button pressed\n"); 
  if (!action.compare("mphasespaceAct"))    printf("mphasespaceAct button pressed\n"); 

  if (!action.compare("rthAct")) { this->RTSource.QuellTyp= 'H'; UpdateSourceBox(); }
  if (!action.compare("dipAct")) { this->RTSource.QuellTyp= 'D'; UpdateSourceBox(); }
  if (!action.compare("poiAct")) { this->RTSource.QuellTyp= 'o'; UpdateSourceBox(); }
  if (!action.compare("rinAct")) { this->RTSource.QuellTyp= 'R'; UpdateSourceBox(); }   
  if (!action.compare("genAct")) { this->RTSource.QuellTyp= 'G'; UpdateSourceBox(); }    
  if (!action.compare("b2hAct")) { this->RTSource.QuellTyp= 'U'; UpdateSourceBox(); }   
  if (!action.compare("b2lAct")) { this->RTSource.QuellTyp= 'U'; UpdateSourceBox(); }    
  if (!action.compare("sisAct")) { this->RTSource.QuellTyp= 'L'; UpdateSourceBox(); }   
  if (!action.compare("simAct")) { this->RTSource.QuellTyp= 'M'; UpdateSourceBox(); }   
  if (!action.compare("sffAct")) { this->RTSource.QuellTyp= 'F'; UpdateSourceBox(); }  

  if (!action.compare("writemapAct")) 
    { 
      printf("writemapAct button pressed\n");
      if ((this->position <= this->elementzahl) && (this->position != 0))
	{
	  printf("write map of element %d to file\n", this->position); 
	  sprintf(header, "beamline: %s, map of element %d, iord: %d", 
		    this->beamlinename, this->position, this->BLOptions.ifl.iord);
	  sprintf(buffer, "%s-%d", this->mapname, this->position);
	  /* casting 15.12.99 ist noch nicht OK */
	  writemapc(buffer, header, this->BLOptions.ifl.iord, 
		    (double *)(this->ElementList[this->position- 1].ypc1), 
		    (double *) this->ElementList[this->position- 1].zpc1, 
		    (double *) this->ElementList[this->position- 1].dypc, 
		    (double *) this->ElementList[this->position- 1].dzpc,
		    (double *) this->ElementList[this->position- 1].wc, 
		    (double *) this->ElementList[this->position- 1].xlc, 
		    (double *) this->ElementList[this->position- 1].xlm.xlen1c, 
		    (double *) this->ElementList[this->position- 1].xlm.xlen2c);
	} 
      
      //  else wir schreiben hier immer beides
	{ 
	  printf("write map of beamline to file\n"); 
	  sprintf(buffer, "beamline: %s, map of beamline, iord: %d\n", 
		    this->beamlinename, this->BLOptions.ifl.iord);
	  sprintf(buffer, "%s-0", this->mapname);
	  writemapc(buffer, header, this->BLOptions.ifl.iord, 
		    (double *) this->ypc1, (double *) this->zpc1, 
		    (double *) this->dypc, (double *) this->dzpc,
		    (double *) this->wc,   (double *) this->xlc, 
		    (double *) this->xlm.xlen1c, 
		    (double *) this->xlm.xlen2c);
	}
    } 
  if (!action.compare("writecoeffAct")) 
    { 
      printf("writecoeffmapAct button pressed\n"); 
      sprintf(buffer, "%s", "mirror-coefficients.dat");
      printf("write coefficients to file: %s\n", buffer);
      WriteMKos((struct mirrortype *)&this->ElementList[this->position- 1].mir, buffer);
    } 

  if (!action.compare("writeRTresultAct")) 
    { 
      printf("writereRTsultAct button pressed\n"); 
      printf("write result to file: %s\n", this->imageraysname);
      WriteRayFile(this->imageraysname, &this->RESULT.points,
		   (struct RayType *)this->RESULT.RESp);
    } 
  if (!action.compare("grfootprintAct")) 
    { 
      printf("grcontourAct button pressed\n"); 
      //d_plot->showSpectrogram(true);
    } 


  if (!action.compare("grcontourAct")) 
    { 
      d_plot->showContour(false);
      d_plot->showSpectrogram(true);
    } 

  if (!action.compare("grcontourisoAct")) 
    { 
      d_plot->showSpectrogram(true);
      d_plot->showContour(true);
    } 
  if (!action.compare("grisoAct")) 
    { 
      d_plot->showSpectrogram(false);
      d_plot->showContour(true);
    } 

  if (!action.compare("grsourceAct")) 
    { 
      d_plot->plotsubject= 0;
      //  d_plot->setTitle(tr("Source Plane"));
      //  d_plot->setphaseData("grsourceAct");
    }

  if (!action.compare("grimageAct")) 
    { 
      d_plot->plotsubject= 1;
      //  d_plot->setTitle(tr("Image Plane"));
      //  d_plot->setphaseData("grimageAct");
    }

  if (!action.compare("grexample1Act")) 
    { 
      d_plot->plotsubject= 2;
      //   d_plot->setTitle(tr("PhaseQt: example 1"));
      //   d_plot->setdefaultData();
    }

  if (!action.compare("grexample2Act")) 
    { 
      d_plot->plotsubject= 3;
      //  d_plot->setTitle(tr("PhaseQt: example 2"));
      //   d_plot->setdefaultData2();
    }

  if (!action.compare("readFg34Act")) 
    { 
      printf("readFg34Act button pressed\n"); 
      printf("Initialize parameters with fg34.par from J. Bahrdt\n"); 
      if (fexists("fg34.par") == 1)
	{
	  //	  correct but src not yet implemented readfg34_par(this->src, this->BLOptions.apr,
	  readfg34_par(this, &this->BLOptions.apr,
		       &this->BLOptions.ifl, &this->BLOptions.xi,
		       &this->BLOptions.epsilon);
	} else
	QMessageBox::warning(this, tr("readFg34Act"),
			     tr("file fg34.par not found!"));
    } 

} // end activateProc

// calc slots in element box
void MainWindow::thetaBslot()  // SetTheta from cff
{
  double cff, alpha, beta, theta0;
  int number= elementList->currentRow();

  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			 tr("(nothing selected)"));
      return;
    }

  char *text= cffE->text().toAscii().data();          // get string from widget
  struct gdatset *gdat= &(this->ElementList[number].GDat);
  char  buffer[9];
  printf("text: %s\n", text);

  // !! we take other relevant data (gdat->lambda, gdat->xdens[0], gdat->inout) from dataset and not from widget
  sscanf(text, "%lf", &cff);
  if (cff != 1.0)
    {
      FixFocus(cff, gdat->lambda, gdat->xdens[0], gdat->inout, &alpha, &beta);
      theta0= (alpha- beta)* 90.0/ PI;
      if (gdat->azimut > 1) theta0= -fabs(theta0);
      sprintf(buffer, "%8.4f", theta0);  
      thetaE->setText(QString(tr(buffer)));  // update widget
      gdat->theta0= theta0;                  // update data
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
  char buffer[10];
  
  sscanf(thetaE ->text().toAscii().data(), "%lf", &theta);  // get theta  from widget text buffer
  sscanf(sourceE->text().toAscii().data(), "%lf", &source); // get source from widget text buffer
  sscanf(imageE ->text().toAscii().data(), "%lf", &image);  // get image  from widget text buffer

  sprintf(buffer, "%9.3f", theta); // for message box
 
  if (theta >= 90.0)
    QMessageBox::warning(this, tr("Calculate Radius"),
			 tr("theta %1 >= 90 deg.\ntake no action").arg(buffer));
  else
    {
      rho= 2.0* source* image* cos(theta * PI/180.0)/ (source+ image); 
      sprintf(buffer, "%9.3f", rho);
      rhoE->setText(QString(tr(buffer)));
    }
 
} // rhoBslot

void MainWindow::rBslot()
{
  double theta, rmi, source, image;
  char buffer[10];
  
  sscanf(thetaE ->text().toAscii().data(), "%lf", &theta);  // get theta  from widget text buffer
  sscanf(sourceE->text().toAscii().data(), "%lf", &source); // get source from widget text buffer
  sscanf(imageE ->text().toAscii().data(), "%lf", &image);  // get image  from widget text buffer

  sprintf(buffer, "%9.3f", theta); // for message box

  if (theta >= 90.0)
    QMessageBox::warning(this, tr("Calculate Radius"),
			 tr("theta %1 >= 90 deg.\ntake no action").arg(buffer));
  else
    {
      rmi= (2.0* source* image)/ ((source+ image)* cos(theta * PI/180.0)); 
      sprintf(buffer, "%9.3f", rmi);
      rE->setText(QString(tr(buffer)));
    }
} // rBslot  
// end calc slots

// debug
void MainWindow::debugslot()
{
  printf("debugslot activated\n");
}

//
// UF slot delete optical element in the beamline box
//
void MainWindow::deleteElement()
{
  struct ElementType *tmplist, *listpt, *tmplistpt;
  QListWidgetItem *item;
  int i;
  int pos= elementList->currentRow();
  //  char *text;

#ifdef DEBUG
  printf("deleteElement: delete element with idx %d out of %u\n", pos, this->elementzahl);
#endif

  if (pos >= 0)
    {
      item= elementList->takeItem(pos);
      this->elementzahl= this->elementList->count();
      if (item)
	{
	  printf("remove item %d, new count: %d\n", pos, this->elementzahl);
	  delete item;
	} 
      else 
	printf("item unvalid \n");
      
      
      //#ifdef XXX
      printf ("widget deleted\n");
      tmplist= XMALLOC(struct ElementType, this->elementzahl); // alloc memory
      memcpy(tmplist, this->ElementList, this->elementzahl* sizeof(struct ElementType)); // copy contents
      
      if (this->elementzahl == 0) 
	XFREE(this->ElementList);
      else
	this->ElementList= XREALLOC(struct ElementType, this->ElementList, this->elementzahl);
      
      /* umsortieren */
      listpt= this->ElementList; tmplistpt= tmplist; 
      for (i= 1; i<= (int)this->elementzahl; i++, listpt++)
	{
	  if (i == pos)  tmplistpt++;  /* ueberlesen */
	  memcpy(listpt, tmplistpt++, sizeof(struct ElementType)); 
	}
      this->beamlineOK &= ~(mapOK | resultOK);
      //  WriteBLFile(PHASESet.beamlinename, bl); 
      XFREE(tmplist);
      //#endif
      printf("done\n");
    } 
  else
    QMessageBox::warning(this, tr("deleteElement"),
			 tr("can't delete anything, list is empty or nothing is selected!\n"));
} // deleteElement()


// apply slot for optical element
void MainWindow::elementApplyBslot()
{
  int number= elementList->currentRow();

  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			 tr("(nothing selected)"));
      return;
    }

  struct gdatset *gd= &(this->ElementList[number].GDat);
  struct mdatset *md= &(this->ElementList[number].MDat);

  printf("elementApplyBslot activated\nfeed data from widget into dataset\n");

#ifdef DEBUG
  printf("elementApplyBslot activated\n");
#endif

  sscanf(preE   ->text().toAscii().data(), "%lf", &gd->r);  
  sscanf(sucE   ->text().toAscii().data(), "%lf", &gd->rp);
  sscanf(thetaE ->text().toAscii().data(), "%lf", &gd->theta0);       
  sscanf(sourceE->text().toAscii().data(), "%lf", &md->r1);   
  sscanf(imageE ->text().toAscii().data(), "%lf", &md->r2);
  sscanf(rE     ->text().toAscii().data(), "%lf", &md->rmi);          
  sscanf(rhoE   ->text().toAscii().data(), "%lf", &md->rho);  
  sscanf(lineDensity->text().toAscii().data(), "%lf", &gd->xdens[0]);
  sscanf(vls1->text().toAscii().data(), "%lf", &gd->xdens[1]);
  sscanf(vls2->text().toAscii().data(), "%lf", &gd->xdens[2]);
  sscanf(vls3->text().toAscii().data(), "%lf", &gd->xdens[3]);
  sscanf(vls4->text().toAscii().data(), "%lf", &gd->xdens[4]);
  
  sscanf(duE ->text().toAscii().data(), "%lf", &md->du);
  sscanf(dwE ->text().toAscii().data(), "%lf", &md->dw);
  sscanf(dlE ->text().toAscii().data(), "%lf", &md->dl);
  sscanf(dRuE->text().toAscii().data(), "%lf", &md->dRu);
  sscanf(dRwE->text().toAscii().data(), "%lf", &md->dRw);
  sscanf(dRlE->text().toAscii().data(), "%lf", &md->dRl);
  sscanf(w1E ->text().toAscii().data(), "%lf", &md->w1);
  sscanf(w2E ->text().toAscii().data(), "%lf", &md->w2);
  sscanf(wsE ->text().toAscii().data(), "%lf", &md->slopew);
  sscanf(l1E ->text().toAscii().data(), "%lf", &md->l1);
  sscanf(l2E ->text().toAscii().data(), "%lf", &md->l2);
  sscanf(lsE ->text().toAscii().data(), "%lf", &md->slopel);
  md->dRu*= 1e-3;
  md->dRw*= 1e-3;
  md->dRl*= 1e-3;
  gd->inout= integerSpinBox->value();
  gd->iflag= (nimBox->isChecked() == true) ? 1 : 0;
  
} // elementApplyBslot

// gr apply
void MainWindow::grapplyslot()
{
  printf("apply activated\n");
  sscanf(gryminE->text().toAscii().data(), "%lf", &d_plot->Plot::ymin);
  sscanf(grymaxE->text().toAscii().data(), "%lf", &d_plot->Plot::ymax);
  sscanf(grzminE->text().toAscii().data(), "%lf", &d_plot->Plot::zmin);
  sscanf(grzmaxE->text().toAscii().data(), "%lf", &d_plot->Plot::zmax);

  if (d_plot->plotsubject == 0) 
    if (this->beamlineOK & sourceOK)
      {
	d_plot->Plot::hfill((struct RayType *)this->RTSource.SourceRays, this->RTSource.raynumber);
	d_plot->Plot::statistics((struct RayType *)this->RTSource.SourceRays, this->RTSource.raynumber);
	UpdateStatistics(d_plot, "Source", this->RTSource.raynumber);
	d_plot->setTitle(tr("Source Plane"));
	d_plot->setphaseData("grsourceAct");
      }
    else
      QMessageBox::warning(this, tr("grapplyslot"), tr("No source data available"));
  if (d_plot->plotsubject == 1) 
    if (this->beamlineOK & resultOK)
    {
      d_plot->Plot::hfill((struct RayType *)this->RESULT.RESp, this->RESULT.points);
      d_plot->Plot::statistics((struct RayType *)this->RESULT.RESp, this->RESULT.points);
      UpdateStatistics(d_plot, "Image", this->RESULT.points);
      d_plot->setTitle(tr("Image Plane"));
      d_plot->setphaseData("grimageAct");
    }
    else
      QMessageBox::warning(this, tr("grapplyslot"), tr("No results available"));
  if (d_plot->plotsubject == 2) 
    {
      d_plot->setTitle(tr("PhaseQt: example 1"));
      d_plot->setdefaultData();
    }

  if (d_plot->plotsubject == 3) 
    {
      d_plot->setTitle(tr("PhaseQt: example 2"));
      d_plot->setdefaultData2();
    }

  d_plot->replot();
} // grapply

// slot autscale
void MainWindow::grautoscaleslot()
{
  char buffer[10];
  printf("autscale activated\n");

  if (d_plot->plotsubject == 1) 
    if (this->beamlineOK & resultOK)
      d_plot->Plot::autoScale((struct RayType *)this->RESULT.RESp, this->RESULT.points); 
    else
      QMessageBox::warning(this, tr("grautoscaleslot"), tr("No results available"));
			
  if (d_plot->plotsubject == 0) 
    if (this->beamlineOK & sourceOK)
      d_plot->Plot::autoScale((struct RayType *)this->RTSource.SourceRays, this->RTSource.raynumber);
    else
      QMessageBox::warning(this, tr("grautoscaleslot"), tr("No source data available"));
  
  sprintf(buffer, "%9.3f", d_plot->Plot::ymin);
  gryminE->setText(QString(tr(buffer)));
  sprintf(buffer, "%9.3f", d_plot->Plot::ymax);
  grymaxE->setText(QString(tr(buffer)));
  sprintf(buffer, "%9.3f", d_plot->Plot::zmin);
  grzminE->setText(QString(tr(buffer)));
  sprintf(buffer, "%9.3f", d_plot->Plot::zmax);
  grzmaxE->setText(QString(tr(buffer)));
} // end slot autoscale

// slot grating
void MainWindow::grslot()
{
  int number= elementList->currentRow();
  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			 tr("(nothing selected)"));
      return;
    }

  if (gratingGroup->isChecked() == true)
    this->ElementList[number].MDat.Art |= GRATINGBIT; 
  else
    this->ElementList[number].MDat.Art &= ~(GRATINGBIT); 

  UpdateElementBox(number);
} // grslot

// slot grating vls
void MainWindow::grvlsslot()
{
  int number= elementList->currentRow();
  if (number < 0) 
    {
      QMessageBox::warning(this, tr("No valid dataset!"),
			 tr("(nothing selected)"));
      return;
    }

  if (vlsGroup->isChecked() == true)
    this->ElementList[number].MDat.Art |= VLSBIT ; 
  else
    this->ElementList[number].MDat.Art &= ~(VLSBIT) ;
 
  UpdateElementBox(number);
} // end grvlsslot

// UF slot insert a new optical element in the beamline box
void MainWindow::insertElement()
{
  struct ElementType *tmplist, *listpt, *tmplistpt;
  int i;
  int pos= elementList->currentRow();
  if (pos < 0) pos= 0;  // empty list 
  if (abs(this->elementzahl) > 1000) this->elementzahl= 0;  // fix falls elementzahl nicht initialisiert

#ifdef DEBUG
  printf("AddBLElement: AddItem at pos %d, out of %u\n", pos, this->elementzahl);  
#endif 
 
  QListWidgetItem *item= new QListWidgetItem("New Element");
  elementList->insertItem(pos, item);
  item->setFlags (item->flags () | Qt::ItemIsEditable);               // edit item
  tmplist= XMALLOC(struct ElementType, this->elementzahl); // alloc memory
  memcpy(tmplist, this->ElementList, this->elementzahl* sizeof(struct ElementType)); // copy contents
  this->elementzahl++;
  this->ElementList= XREALLOC(struct ElementType, this->ElementList, this->elementzahl);
  listpt= this->ElementList; tmplistpt= tmplist; 
  for (i= 0; i< (int)this->elementzahl; i++, listpt++)
    {
#ifdef DEBUG
      printf("i= %d, pos= %d, nmax %u\n", i, pos, this->elementzahl);
#endif
      if (i == pos)
	{
	  listpt->ElementOK= 0;
	  sprintf(listpt->elementname, "%s", "New Element");
	  minitdatset(&listpt->MDat);
	  listpt->MDat.Art= kEOETM;   // overwrite kEOEDefaults
	  ginitdatset(&listpt->GDat);
	  
	}
      else
	memcpy(listpt, tmplistpt++, sizeof(struct ElementType)); 
    }
  this->beamlineOK &= ~(mapOK | resultOK);
  //  WriteBLFile(PHASESet.beamlinename, bl); 
  XFREE(tmplist);
  printf("inserElement: end list should have %u elements\n", this->elementzahl);
} // insertElement

// slot called to read in a new beamline
void MainWindow::newBeamline()
{
  int rcode;
  
#ifdef DEBUG
  printf("Debug: slot newBeamline activated\n");
  //  myQtPhase->myPHASEset::print();
#endif
  QString fileName = QFileDialog::getOpenFileName(this, tr("Open File"), 
						  // QDir::currentPath()
						  "/afs/psi.ch/user/f/flechsig/phase/data",
						  tr("Text files (*.phase);;(*)")
						  );
  char *name;
  //  int result;
  //this->QtPhase::print();

  if (!fileName.isEmpty()) 
    {
      name= fileName.toAscii().data();
      printf("MainWindow::newBeamline: try to read file: %s\n", name);
      rcode= ReadBLFile(name, this);
      if (rcode != -1)
	{
	  UpdateElementList();
	  UpdateBeamlineBox();
	  UpdateSourceBox();
	  this->beamlineOK= 0;
          this->myPHASEset::init(name);
	  PutPHASE(this, (char*) MainPickName);
	} 
      else
	QMessageBox::information(this, tr("Phase: newBeamline"),
				 tr("Cannot load %1.\n wrong file type!").arg(fileName));
    }
  this->myPHASEset::print();
} // end newBeamline()

// slot parameter update, callback des Editors
void MainWindow::parameterUpdateSlot()
{
  parameterUpdate(parameterList->currentRow(), parameterE->text().toAscii().data());
} // end parameterUpdateSlot

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
  this->ElementList[number].GDat.azimut= 0;
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
  this->ElementList[number].GDat.azimut= 1;
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
  this->ElementList[number].GDat.azimut= 2;
  this->ElementList[number].GDat.theta0= -fabs(this->ElementList[number].GDat.theta0);
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
  this->ElementList[number].GDat.azimut= 3;
  this->ElementList[number].GDat.theta0= -fabs(this->ElementList[number].GDat.theta0);
  UpdateElementBox(number);
} 
// end slot orientation radio buttons

// slot
void MainWindow::print()
{
#ifndef QT_NO_PRINTDIALOG
  //QTextDocument *document = textEdit->document();
    QPrinter printer;

    QPrintDialog *dlg = new QPrintDialog(&printer, this);
    if (dlg->exec() != QDialog::Accepted)
        return;

    //  document->print(&printer);
    d_plot->printPlot();
    statusBar()->showMessage(tr("Ready"), 2000);
#endif
} // end print()

// slot
void MainWindow::save()
{
  char *name= this->beamlinename;

  WriteBLFile(name, this);
 

  statusBar()->showMessage(tr("Saved '%1'").arg(name), 2000);
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
 
    WriteBLFile(name, this);

    //    QTextStream out(&file);
    //    QApplication::setOverrideCursor(Qt::WaitCursor);
    //    out << textEdit->toHtml();
    //   QApplication::restoreOverrideCursor();

    statusBar()->showMessage(tr("Saved '%1'").arg(fileName), 2000);
} // end save

// UF selection slot
void MainWindow::selectElement()
{
  QListWidgetItem *item;
  int elementnumber= elementList->currentRow();
  char *text;
  
  if (elementnumber < 0) 
    return;

  item= elementList->currentItem();
  text= item->text().toAscii().data();
  elementnumber= elementList->currentRow();
  groupBox1->setTitle(item->text());  // set text header
  UpdateElementBox(elementnumber);
} // selectElement

// UF selection slot
void MainWindow::selectParameter()
{
  char buffer[MaxPathLength], *ch;
  int parameternumber= parameterList->currentRow();
  
  if (parameternumber < 0) 
    return;

  strcpy(buffer, parameterList->currentItem()->text().toAscii().data());
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
  this->ElementList[number].MDat.Art= kEOEPM;
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
  this->ElementList[number].MDat.Art= kEOETM;
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
  this->ElementList[number].MDat.Art= kEOEPElli;
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
  this->ElementList[number].MDat.Art= kEOEElli;
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
  this->ElementList[number].MDat.Art= kEOECone;
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
  this->ElementList[number].MDat.Art= kEOEGeneral;
  UpdateElementBox(number); 
}
// end slots shapeMenu

// apply slot for source
void MainWindow::sourceDefaultBslot()
{
#ifdef DEBUG
  printf("sourceDefaultBslot activated\n");
#endif
  QMessageBox::warning(this, tr("sourceDefaultBslot"),
			   tr("no function so far"));
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
  //struct SRSourceType         *sp; 
  //struct PSImageType          *psip;
  //struct PSSourceType         *pssp; 

#ifdef DEBUG
  printf("sourceApplyBslot activated\n");
#endif

  if (RTSource.Quellep == NULL )
    {
      printf("error: sourceApplyBslot: Quellep == NULL\n");
      return;
    }
    
  sou= this->RTSource.QuellTyp;
  switch (sou) {
    
  case 'D':
    dp= (struct DipolSourceType *)this->RTSource.Quellep;
    sscanf(S1E->text().toAscii().data(), "%lf", &dp->sigy);
    sscanf(S2E->text().toAscii().data(), "%lf", &dp->sigdy);
    sscanf(S3E->text().toAscii().data(), "%lf", &dp->sigz);
    sscanf(S4E->text().toAscii().data(), "%lf", &dp->dz);
    sscanf(S5E->text().toAscii().data(), "%d",  &this->RTSource.raynumber);
    break;
    
  case 'G':
    up0= (struct UndulatorSource0Type *)this->RTSource.Quellep;
    sscanf(S1E->text().toAscii().data(), "%lf", &up0->length);
    sscanf(S2E->text().toAscii().data(), "%lf", &lambda);
    sscanf(S3E->text().toAscii().data(), "%d",  &this->RTSource.raynumber);
    sscanf(S4E->text().toAscii().data(), "%lf", &up0->deltaz);
    sscanf(S5E->text().toAscii().data(), "%lf", &up0->sigmaey);
    sscanf(S6E->text().toAscii().data(), "%lf", &up0->sigmaez);
    sscanf(S7E->text().toAscii().data(), "%lf", &up0->sigmaedy);
    sscanf(S8E->text().toAscii().data(), "%lf", &up0->sigmaedz);
    break;
    
  case 'H':
    hp= (struct  HardEdgeSourceType *)this->RTSource.Quellep;
    sscanf(S1E->text().toAscii().data(), "%lf", &hp->disty);
    sscanf(S2E->text().toAscii().data(), "%d",  &hp->iy);
    sscanf(S3E->text().toAscii().data(), "%lf", &hp->distz);
    sscanf(S4E->text().toAscii().data(), "%d",  &hp->iz);
    sscanf(S5E->text().toAscii().data(), "%lf", &hp->divy);
    sscanf(S6E->text().toAscii().data(), "%d",  &hp->idy);
    sscanf(S7E->text().toAscii().data(), "%lf", &hp->divz);
    sscanf(S8E->text().toAscii().data(), "%d",  &hp->idz);
    this->RTSource.raynumber=  hp->iy* hp->idy* hp->iz* hp->idz;
    break;
    
  case 'L':
  case 'M':
    up= (struct UndulatorSourceType *)this->RTSource.Quellep;
    sscanf(S1E->text().toAscii().data(), "%lf", &up->length);
    sscanf(S2E->text().toAscii().data(), "%lf", &lambda);
    sscanf(S3E->text().toAscii().data(), "%d",  &this->RTSource.raynumber);
    sscanf(S4E->text().toAscii().data(), "%lf", &up->deltaz);
    break;
    
  case 'o':
    sop= (struct PointSourceType *)this->RTSource.Quellep;
    sscanf(S1E->text().toAscii().data(), "%lf", &sop->sigy);
    sscanf(S2E->text().toAscii().data(), "%lf", &sop->sigdy);
    sscanf(S3E->text().toAscii().data(), "%lf", &sop->sigz);
    sscanf(S4E->text().toAscii().data(), "%lf", &sop->sigdz);
    sscanf(S5E->text().toAscii().data(), "%d",  &this->RTSource.raynumber);
    break;
    
  case 'R':
    rp= (struct RingSourceType *)this->RTSource.Quellep;
    sscanf(S1E->text().toAscii().data(), "%lf", &rp->dy);
    sscanf(S2E->text().toAscii().data(), "%lf", &rp->dz);
    sscanf(S3E->text().toAscii().data(), "%d",  &this->RTSource.raynumber);
    break;
    
  case 'U':
    up= (struct UndulatorSourceType *)this->RTSource.Quellep;
    sscanf(S1E->text().toAscii().data(), "%lf", &up->length);
    sscanf(S2E->text().toAscii().data(), "%lf", &lambda);
    sscanf(S3E->text().toAscii().data(), "%d",  &this->RTSource.raynumber);
    break;
    
  default:
    QMessageBox::warning(this, tr("sourceApplyBslot"),
			 tr("Source type %1 not recognized.\nreport bug to uwe.flechsig@psi.ch")
			 .arg(sou));
    return;
  }

  this->BLOptions.wrSource = (sourceFileBox->isChecked() == true) ?  1 : 0;  
  MakeRTSource(this, this);
} //sourceApplyBslot

// slot
void MainWindow::undo()
{
  //QTextDocument *document = textEdit->document();
  //  document->undo();
} // undo

///////////////////////
// end slots section //
///////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
// begin widget definition section //
/////////////////////////////////////

/////////////////////////////////
// define action buttons
void MainWindow::createActions()
{
    newLetterAct = new QAction(QIcon(":/images/new.png"), tr("&New Beamline"),
                               this);
    newLetterAct->setShortcuts(QKeySequence::New);
    newLetterAct->setStatusTip(tr("Create a new Beamline"));

    connect(newLetterAct, SIGNAL(triggered()), this, SLOT(newBeamline()));

    saveAct = new QAction(QIcon(":/images/save.png"), tr("&Save..."), this);
    saveAct->setShortcuts(QKeySequence::Save);
    saveAct->setStatusTip(tr("Save the current beamline"));
    connect(saveAct, SIGNAL(triggered()), this, SLOT(save()));

    saveasAct = new QAction(QIcon(":/images/save.png"), tr("Save &as..."), this);
    saveasAct->setStatusTip(tr("Save the current beamline in new file"));
    connect(saveasAct, SIGNAL(triggered()), this, SLOT(saveas()));

    printAct = new QAction(QIcon(":/images/print.png"), tr("&Print..."), this);
    printAct->setShortcuts(QKeySequence::Print);
    printAct->setStatusTip(tr("Print the current beamline"));
    connect(printAct, SIGNAL(triggered()), this, SLOT(print()));

    undoAct = new QAction(QIcon(":/images/undo.png"), tr("&Undo"), this);
    undoAct->setShortcuts(QKeySequence::Undo);
    undoAct->setStatusTip(tr("Undo the last editing action"));
    connect(undoAct, SIGNAL(triggered()), this, SLOT(undo()));

    quitAct = new QAction(tr("&Quit"), this);
    quitAct->setShortcuts(QKeySequence::Quit);
    quitAct->setStatusTip(tr("Quit the application"));
    connect(quitAct, SIGNAL(triggered()), this, SLOT(close()));

    aboutAct = new QAction(tr("&About"), this);
    aboutAct->setStatusTip(tr("Show the application's About box"));
    connect(aboutAct, SIGNAL(triggered()), this, SLOT(about()));

    aboutQtAct = new QAction(tr("About &Qt"), this);
    aboutQtAct->setStatusTip(tr("Show the Qt library's About box"));
    connect(aboutQtAct, SIGNAL(triggered()), qApp, SLOT(aboutQt()));

// UF 
// we use the signal mapper to determin which button has been clicked
// and pass the name of the button as string which is then evaluated by activateProc
    signalMapper = new QSignalMapper(this);

    raytracesimpleAct = new QAction(tr("GO &Simple ray tracing"), this);
    raytracesimpleAct->setStatusTip(tr("geometrical optics, simple ray tracing"));
    signalMapper->setMapping(raytracesimpleAct, QString("raytracesimpleAct"));
    connect(raytracesimpleAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    raytracefullAct = new QAction(tr("GO &Full ray tracing"), this);
    raytracefullAct->setStatusTip(tr("geometrical optics, full ray tracing"));
    signalMapper->setMapping(raytracefullAct, QString("raytracefullAct"));
    connect(raytracefullAct, SIGNAL(triggered()), signalMapper, SLOT(map()));
   
    footprintAct = new QAction(tr("GO &footprint at selected element"), this);
    footprintAct->setStatusTip(tr("geometrical optics, footprint at selected element"));
    signalMapper->setMapping(footprintAct, QString("footprintAct"));
    connect(footprintAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    phasespaceAct = new QAction(tr("PO &phase space imaging"), this);
    phasespaceAct->setStatusTip(tr("physical optics, phase space imaging"));
    signalMapper->setMapping(phasespaceAct, QString("phasespaceAct"));
    connect(phasespaceAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    mphasespaceAct = new QAction(tr("PO &multiple phase space imaging"), this);
    mphasespaceAct->setStatusTip(tr("physical optics, multiple phase space imaging"));
    signalMapper->setMapping(mphasespaceAct, QString("mphasespaceAct"));
    connect(mphasespaceAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    writemapAct = new QAction(tr("&Write Map"), this);
    writemapAct->setStatusTip(tr("Write file with transfer map"));
    signalMapper->setMapping(writemapAct, QString("writemapAct"));
    connect(writemapAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    writecoeffAct = new QAction(tr("Write Mirr&or Coefficients"), this);
    writecoeffAct->setStatusTip(tr("Write file with mirror coefficients"));
    signalMapper->setMapping(writecoeffAct, QString("writecoeffAct"));
    connect(writecoeffAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    writeRTresultAct = new QAction(tr("Write &RT results "), this);
    writeRTresultAct->setStatusTip(tr("Write file with ray trace results"));
    signalMapper->setMapping(writeRTresultAct, QString("writeRTresultAct"));
    connect(writeRTresultAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    readFg34Act = new QAction(tr("&Read file fg34.par"), this);
    readFg34Act->setStatusTip(tr("Read parameter file fg34.par (for compatibility with previous phase versions)"));
    signalMapper->setMapping(readFg34Act, QString("readFg34Act"));
    connect(readFg34Act, SIGNAL(triggered()), signalMapper, SLOT(map()));

    //    rthardedgeAct = new QAction();
    //    signalMapper->setMapping(rthardedgeAct, QString("rthardedgeAct"));
    //    connect(mphasespaceAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    // finaly connect mapper to acticateProc
    connect(signalMapper, SIGNAL(mapped(QString)), this, SLOT(activateProc(QString)));
} // createActions



// beamline box
QWidget *MainWindow::createBeamlineBox()
{
  beamlineBox = new QWidget();

  // upper part
  QGroupBox   *beamlineElementGroup  = new QGroupBox(tr("optical element list"));
  QVBoxLayout *beamlineElementLayout = new QVBoxLayout;

  elementList = new QListWidget();
  /*
    elementList->addItems(QStringList()
			<< "Mirror 1"
			<< "Mirror 2"
			<< "Grating 1"
			<< "Mirror 3"
			<< "Mirror 4"
			);
  
  */
  QGroupBox   *beamlineButtomGroup  = new QGroupBox();
  QHBoxLayout *beamlineButtomLayout = new QHBoxLayout;
  QPushButton *addB  = new QPushButton(QIcon(":/images/up-32.png"), tr("Add"), this);
  QPushButton *delB  = new QPushButton(QIcon(":/images/down-32.png"), tr("Del"), this);
  beamlineButtomLayout->addWidget(addB);
  beamlineButtomLayout->addWidget(delB);
  beamlineButtomGroup->setLayout(beamlineButtomLayout);

  beamlineElementLayout->addWidget(elementList);
  beamlineElementLayout->addWidget(beamlineButtomGroup);
  beamlineElementGroup->setLayout(beamlineElementLayout);

// central part
    QGroupBox   *beamlineGenericGroup  = new QGroupBox(tr("generic parameters"));
    QGridLayout *beamlineGenericLayout = new QGridLayout;

    lambdaE = new QLineEdit;
    dislenE = new QLineEdit;

    QLabel *lambdaLabel  = new QLabel(tr("wavelength (nm)"));
    QLabel *dislenLabel  = new QLabel(tr("dispersive length (mm)"));

    beamlineGenericLayout->addWidget(lambdaLabel, 0, 0);
    beamlineGenericLayout->addWidget(dislenLabel, 1, 0);
    beamlineGenericLayout->addWidget(lambdaE,     0, 1);
    beamlineGenericLayout->addWidget(dislenE,     1, 1);
    beamlineGenericGroup->setLayout(beamlineGenericLayout);

// bottom part
    QGroupBox   *beamlineCalcGroup  = new QGroupBox(tr("calculation parameters"));
    QVBoxLayout *beamlineCalcLayout = new QVBoxLayout;

    goButton = new QRadioButton(tr("&geometrical optic (GO)"));
    poButton = new QRadioButton(tr("&physical optic (PO)"));
    goButton->setChecked(true);
    misaliBox = new QCheckBox(tr("with misalignment"));

    beamlineCalcLayout->addWidget(goButton);
    beamlineCalcLayout->addWidget(poButton);
    beamlineCalcLayout->addWidget(misaliBox);
    beamlineCalcGroup->setLayout(beamlineCalcLayout);

    QVBoxLayout *vbox = new QVBoxLayout;
    vbox->addWidget(beamlineElementGroup);

    vbox->addWidget(beamlineGenericGroup);
    vbox->addWidget(beamlineCalcGroup);
    vbox->addStretch(1);
    beamlineBox->setLayout(vbox);

    // slots
    connect(addB, SIGNAL(pressed()), this, SLOT(insertElement()));
    connect(delB, SIGNAL(pressed()), this, SLOT(deleteElement()));
    connect(elementList, SIGNAL(itemSelectionChanged()), this, SLOT(selectElement()));

    return beamlineBox;
} // end createbeamline box


// dock widgets
void MainWindow::createDockWindows()
{
  // the beamline box on the left
    QDockWidget *dock = new QDockWidget(tr("Beamline Box"), this);
    dock->setWidget(createBeamlineBox());
    addDockWidget(Qt::LeftDockWidgetArea, dock);
    viewMenu->addAction(dock->toggleViewAction());
  
// the source box
    dock = new QDockWidget(tr("Source Box"), this);
    dock->setWidget(createSourceBox());
    addDockWidget(Qt::RightDockWidgetArea, dock);
    viewMenu->addAction(dock->toggleViewAction());

    // the optical element box
    dock = new QDockWidget(tr("Optical Element Box"), this);
    dock->setWidget(createOpticalElementBox());
    addDockWidget(Qt::RightDockWidgetArea, dock);
    viewMenu->addAction(dock->toggleViewAction());

// the parameter box
    dock = new QDockWidget(tr("Parameter Box"), this);
    dock->setWidget(createParameterBox());
    dock->setAllowedAreas(Qt::LeftDockWidgetArea | Qt::RightDockWidgetArea);
    addDockWidget(Qt::LeftDockWidgetArea, dock);
    viewMenu->addAction(dock->toggleViewAction());
} // createDockwindows


// graphic box
QWidget *MainWindow::createGraphicBox()
{
  graphicBox = new QWidget();

  // upper part
  QGroupBox   *graphicGroup  = new QGroupBox(tr("Graphics"));
  QGridLayout *graphicLayout = new QGridLayout;
  
  QLabel *zminLabel  = new QLabel(tr("zmin (mm)"));
  QLabel *zmaxLabel  = new QLabel(tr("zmax (mm)"));
  QLabel *yminLabel  = new QLabel(tr("ymin (mm)"));
  QLabel *ymaxLabel  = new QLabel(tr("ymax (mm)"));
  
  grzminE  = new QLineEdit;
  grzmaxE  = new QLineEdit;
  gryminE  = new QLineEdit;
  grymaxE  = new QLineEdit;
  
  grautoButton  = new QPushButton(tr("A&utoscale"));
  grapplyButton = new QPushButton(tr("&Apply"));
  QPushButton *popupButton = new QPushButton(tr("&PlotStyle"));

  connect(grapplyButton, SIGNAL(clicked()), this, SLOT(grapplyslot()));
  connect(grautoButton,  SIGNAL(clicked()), this, SLOT(grautoscaleslot()));

  plotstyleMenu   = new QMenu(this);
  grfootprintAct  = new QAction(tr("&footprint"), this);
  grcontourAct    = new QAction(tr("&contour"), this);
  grcontourisoAct = new QAction(tr("contour + iso &lines"), this);
  grisoAct        = new QAction(tr("&iso lines"), this);

  plotstyleMenu->addAction(grfootprintAct);
  plotstyleMenu->addAction(grcontourAct);
  plotstyleMenu->addAction(grcontourisoAct);
  plotstyleMenu->addAction(grisoAct);
  
  popupButton->setMenu(plotstyleMenu);
  grsignalMapper = new QSignalMapper(this);
  connect(grsignalMapper, SIGNAL(mapped(QString)), this, SLOT(activateProc(QString)));

  connect(grfootprintAct,  SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grcontourAct,    SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grcontourisoAct, SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grisoAct,        SIGNAL(triggered()), grsignalMapper, SLOT(map()));

  grsignalMapper->setMapping(grfootprintAct,  QString("grfootprintAct"));
  grsignalMapper->setMapping(grcontourAct,    QString("grcontourAct"));
  grsignalMapper->setMapping(grcontourisoAct, QString("grcontourisoAct"));
  grsignalMapper->setMapping(grisoAct,        QString("grisoAct"));
  
  QPushButton *subjectpopupButton = new QPushButton(tr("&PlotSubject"));
  QMenu *subject = new QMenu(this);
  grsourceAct  = new QAction(tr("&source"), this);
  grimageAct   = new QAction(tr("&image"), this);
  grexample1Act = new QAction(tr("&example 1"), this);
  grexample2Act = new QAction(tr("&example 2"), this);

  subject->addAction(grsourceAct);
  subject->addAction(grimageAct);
  subject->addAction(grexample1Act);
  subject->addAction(grexample2Act);
  subjectpopupButton->setMenu(subject);

  connect(grsourceAct,   SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grimageAct,    SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grexample1Act, SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grexample2Act, SIGNAL(triggered()), grsignalMapper, SLOT(map()));

  grsignalMapper->setMapping(grsourceAct,   QString("grsourceAct"));
  grsignalMapper->setMapping(grimageAct,    QString("grimageAct"));
  grsignalMapper->setMapping(grexample1Act, QString("grexample1Act"));
  grsignalMapper->setMapping(grexample2Act, QString("grexample2Act"));
 
  graphicLayout->addWidget(zminLabel, 0, 0);
  graphicLayout->addWidget(zmaxLabel, 0, 2);
  graphicLayout->addWidget(yminLabel, 1, 0);
  graphicLayout->addWidget(ymaxLabel, 1, 2);
  graphicLayout->addWidget(grzminE, 0, 1);
  graphicLayout->addWidget(grzmaxE, 0, 3);
  graphicLayout->addWidget(gryminE, 1, 1);
  graphicLayout->addWidget(grymaxE, 1, 3);
  graphicLayout->addWidget(grautoButton, 2, 2);
  graphicLayout->addWidget(grapplyButton, 2, 3);
  graphicLayout->addWidget(subjectpopupButton, 2, 0);
  graphicLayout->addWidget(popupButton, 2, 1);
  //  graphicLayout->setRowStretch(1,1);
  graphicGroup->setLayout(graphicLayout);
  
  d_plot = new Plot(this);
  d_plot->setAxisTitle(2, tr("z (mm)"));
  d_plot->setAxisTitle(0, tr("y (mm)"));
  d_plot->setTitle(tr("PhaseQt"));

  statGroup  = new QGroupBox(tr("Statistics"));
  QGridLayout *statLayout = new QGridLayout;
  
  czLabel   = new QLabel(tr("z center (mm)"));
  cyLabel   = new QLabel(tr("y center (mm)"));
  wzLabel   = new QLabel(tr("z FWHM (mm)"));
  wyLabel   = new QLabel(tr("y FWHM (mm)"));
  cdzLabel  = new QLabel(tr("dz center (mm)"));
  cdyLabel  = new QLabel(tr("dy center (mm)"));
  wdzLabel  = new QLabel(tr("dz FWHM (mm)"));
  wdyLabel  = new QLabel(tr("dy FWHM (mm)"));
  rayLabel  = new QLabel(tr("rays"));
  traLabel  = new QLabel(tr("transmittance"));
  ryLabel   = new QLabel(tr("y E/dE FWHM"));
  rzLabel   = new QLabel(tr("z E/dE FWHM"));

  statLayout->addWidget(czLabel,  0, 0);
  statLayout->addWidget(cyLabel,  0, 1);
  statLayout->addWidget(wzLabel,  1, 0);
  statLayout->addWidget(wyLabel,  1, 1);
  statLayout->addWidget(cdzLabel, 2, 0);
  statLayout->addWidget(cdyLabel, 2, 1);
  statLayout->addWidget(wdzLabel, 3, 0);
  statLayout->addWidget(wdyLabel, 3, 1);
  statLayout->addWidget(rayLabel, 4, 0);
  statLayout->addWidget(traLabel, 4, 1);
  statLayout->addWidget(ryLabel, 5, 0);
  statLayout->addWidget(rzLabel, 5, 1);
  statGroup->setLayout(statLayout);

  QVBoxLayout *vbox = new QVBoxLayout;
  vbox->addWidget(graphicGroup);
  vbox->addWidget(d_plot);
  vbox->addWidget(statGroup);
  vbox->addStretch(1);
  graphicBox->setLayout(vbox);
  return graphicBox;
} // end creagraphic box




// create menus with buttons
void MainWindow::createMenus()
{
    fileMenu = menuBar()->addMenu(tr("&File"));
    fileMenu->addAction(newLetterAct);
    fileMenu->addAction(saveAct);
    fileMenu->addAction(saveasAct);
    fileMenu->addAction(printAct);
    fileMenu->addSeparator();
    fileMenu->addAction(quitAct);

    editMenu = menuBar()->addMenu(tr("&Edit"));
    editMenu->addAction(undoAct);
    editMenu->addAction(readFg34Act);

    calcMenu = menuBar()->addMenu(tr("&Calc"));
    calcMenu->addAction(raytracesimpleAct);
    calcMenu->addAction(raytracefullAct);
    calcMenu->addAction(footprintAct);
    calcMenu->addSeparator();
    calcMenu->addAction(phasespaceAct);
    calcMenu->addAction(mphasespaceAct);

    cmdMenu = menuBar()->addMenu(tr("C&ommands"));
    cmdMenu->addAction(writeRTresultAct);
    cmdMenu->addAction(writemapAct);
    cmdMenu->addAction(writecoeffAct);
    

    viewMenu = menuBar()->addMenu(tr("&View"));

    menuBar()->addSeparator();

    helpMenu = menuBar()->addMenu(tr("&Help"));
    helpMenu->addAction(aboutAct);
    helpMenu->addAction(aboutQtAct);
} // createMenus

// the optical element box
QWidget *MainWindow::createOpticalElementBox()
{
  elementBox = new QWidget();

  //radio buttons
  //QGroupBox *groupBox   = new QGroupBox(tr("orientation (reflection to)"));
  QGroupBox *orientationBox   = new QGroupBox(tr("orientation (reflection to)"));
  rup1    = new QRadioButton(tr("&up"));
  rleft2  = new QRadioButton(tr("&left"));
  rdown3  = new QRadioButton(tr("&down"));
  rright4 = new QRadioButton(tr("&right"));
  rup1->setChecked(true);
  
  connect(rup1,    SIGNAL(clicked()), this, SLOT(rup1slot()));
  connect(rleft2,  SIGNAL(clicked()), this, SLOT(rleft2slot()));
  connect(rdown3,  SIGNAL(clicked()), this, SLOT(rdown3slot()));
  connect(rright4, SIGNAL(clicked()), this, SLOT(rright4slot()));
  
  QHBoxLayout *orientationLayout = new QHBoxLayout;
  orientationLayout->addWidget(rup1);
  orientationLayout->addWidget(rleft2);
  orientationLayout->addWidget(rdown3);
  orientationLayout->addWidget(rright4);
  orientationBox->setLayout(orientationLayout);

  // popup button
  QPushButton *shapeButton = new QPushButton(tr("&Shape"));
  QMenu *shapeMenu = new QMenu(this);
  pmAct = new QAction(tr("&flat"), this);
  toAct = new QAction(tr("&toroidal"), this);
  peAct = new QAction(tr("&plane- elliptical"), this);
  elAct = new QAction(tr("&elliptical"), this);
  coAct = new QAction(tr("&conical"), this);
  geAct = new QAction(tr("&generic"), this);    
  shapeMenu->addAction(pmAct);
  shapeMenu->addAction(toAct);
  shapeMenu->addAction(peAct);
  shapeMenu->addAction(elAct);
  shapeMenu->addAction(coAct);
  shapeMenu->addSeparator();
  shapeMenu->addAction(geAct);
  shapeButton->setMenu(shapeMenu);

  connect(pmAct, SIGNAL(triggered()), this, SLOT(pmslot()));
  connect(toAct, SIGNAL(triggered()), this, SLOT(toslot()));
  connect(peAct, SIGNAL(triggered()), this, SLOT(peslot()));
  connect(elAct, SIGNAL(triggered()), this, SLOT(elslot()));
  connect(coAct, SIGNAL(triggered()), this, SLOT(coslot()));
  connect(geAct, SIGNAL(triggered()), this, SLOT(geslot()));

  shapeLabel = new QLabel(tr("unknown")); // return value of menu

  groupBox1 = new QGroupBox(tr("&Element")); 
  QHBoxLayout *hbox1 = new QHBoxLayout;
  hbox1->addWidget(shapeButton);
  hbox1->addStretch(1);
  hbox1->addWidget(shapeLabel);
  hbox1->addStretch(1);
  hbox1->addWidget(orientationBox);
  groupBox1->setLayout(hbox1);

  //radius
  QGroupBox *geometryGroup = new QGroupBox(tr("&geometry and shape parameters (support fields in red)"));
  QGridLayout *geometryLayout = new QGridLayout;
  QLabel *cffLabel    = new QLabel(tr("cff (PGM)"));
  QLabel *preLabel    = new QLabel(tr("Prec (mm)"));
  QLabel *sucLabel    = new QLabel(tr("Succ (mm)"));
  QLabel *thetaLabel  = new QLabel(tr("theta (deg)"));
  QLabel *sourceLabel = new QLabel(tr("Source (mm)"));
  QLabel *imageLabel  = new QLabel(tr("Image (mm)"));
  QLabel *rLabel      = new QLabel(tr("r (mm)"));
  QLabel *rhoLabel    = new QLabel(tr("rho (mm)"));

  cffE    = new QLineEdit;
  preE    = new QLineEdit;
  sucE    = new QLineEdit;
  thetaE  = new QLineEdit;
  sourceE = new QLineEdit;
  imageE  = new QLineEdit;
  rE      = new QLineEdit;
  rhoE    = new QLineEdit;

  thetaB  = new QPushButton(QIcon(":/images/Blue-arrow-right-32.png"), tr("calc"), this);
  sourceB = new QPushButton(QIcon(":/images/Blue-arrow-right-32.png"), tr("calc"), this);
  imageB  = new QPushButton(QIcon(":/images/Blue-arrow-right-32.png"), tr("calc"), this);
  rB      = new QPushButton(QIcon(":/images/Blue-arrow-right-32.png"), tr("calc"), this);
  rhoB    = new QPushButton(QIcon(":/images/Blue-arrow-right-32.png"), tr("calc"), this);

  elementApplyB = new QPushButton(tr("Apply"), this);

  connect(thetaB,  SIGNAL(clicked()), this, SLOT(thetaBslot()));
  connect(sourceB, SIGNAL(clicked()), this, SLOT(sourceBslot()));
  connect(imageB,  SIGNAL(clicked()), this, SLOT(imageBslot()));
  connect(rB,      SIGNAL(clicked()), this, SLOT(rBslot()));
  connect(rhoB,    SIGNAL(clicked()), this, SLOT(rhoBslot()));
  //connect(elementApplyB, SIGNAL(clicked()), this, SLOT(debugslot()));
  connect(elementApplyB, SIGNAL(clicked()), this, SLOT(elementApplyBslot()));

  geometryLayout->addWidget(cffLabel,0,0);
  geometryLayout->addWidget(preLabel,1,0);
  geometryLayout->addWidget(sucLabel,2,0);

  geometryLayout->addWidget(cffE,0,1);
  geometryLayout->addWidget(preE,1,1);
  geometryLayout->addWidget(sucE,2,1);

  geometryLayout->addWidget(thetaB, 0,2);
  geometryLayout->addWidget(sourceB,1,2);
  geometryLayout->addWidget(imageB, 2,2);

  geometryLayout->addWidget(thetaLabel, 0,3);
  geometryLayout->addWidget(sourceLabel,1,3);
  geometryLayout->addWidget(imageLabel, 2,3);

  geometryLayout->addWidget(thetaE, 0,4);
  geometryLayout->addWidget(sourceE,1,4);
  geometryLayout->addWidget(imageE, 2,4);

  geometryLayout->addWidget(rB,  0,5);
  geometryLayout->addWidget(rhoB,1,5);

  geometryLayout->addWidget(rLabel,  0,6);
  geometryLayout->addWidget(rhoLabel,1,6);
  geometryLayout->addWidget(rE,      0,7);
  geometryLayout->addWidget(rhoE,    1,7);

  geometryLayout->addWidget(elementApplyB, 2,7);

  geometryGroup->setLayout(geometryLayout);
  //radius

  // grating
  gratingGroup = new QGroupBox(tr("&Grating"));
  gratingGroup->setCheckable(true);
  gratingGroup->setChecked(true);
  //connect(gratingGroup, SIGNAL(clicked(bool on)), this, SLOT(grslot(bool on)));
  connect(gratingGroup, SIGNAL(clicked()), this, SLOT(grslot()));

  QLabel *orderLabel   = new QLabel(tr("Diffraction order"));
  QLabel *densityLabel = new QLabel(tr("line density (1/mm)"));
  integerSpinBox = new QSpinBox;
  integerSpinBox->setRange(-20, 20);
  integerSpinBox->setSingleStep(1);
  integerSpinBox->setValue(1);
  lineDensity = new QLineEdit;
  nimBox      = new QCheckBox(tr("&NIM Translation"));

  // vls
  vlsGroup = new QGroupBox(tr("&VLS Grating"));
  vlsGroup->setCheckable(true);
  vlsGroup->setChecked(false);
  connect(vlsGroup, SIGNAL(clicked()), this, SLOT(grvlsslot()));

  QHBoxLayout *vlslayout = new QHBoxLayout;
  QLabel *vlsLabel = new QLabel(tr("coeff(1)...coeff(4)"));
  vls1 = new QLineEdit;
  vls2 = new QLineEdit;
  vls3 = new QLineEdit;
  vls4 = new QLineEdit;
  vlslayout->addWidget(vlsLabel);
  vlslayout->addWidget(vls1);
  vlslayout->addWidget(vls2);
  vlslayout->addWidget(vls3);
  vlslayout->addWidget(vls4);
  vlsGroup->setLayout(vlslayout);
  // end VLS

  QGroupBox *gratingGroup1 = new QGroupBox(tr("g&rating parameter"));
  QHBoxLayout *gratingLayout1 = new QHBoxLayout;
  gratingLayout1->addWidget(densityLabel);
  gratingLayout1->addWidget(lineDensity);
  gratingLayout1->addWidget(orderLabel);
  gratingLayout1->addWidget(integerSpinBox);
  gratingLayout1->addStretch(1);
  gratingLayout1->addWidget(nimBox);
  gratingGroup1->setLayout(gratingLayout1);

  QVBoxLayout *gratingLayout2 = new QVBoxLayout;
  gratingLayout2->addWidget(gratingGroup1);
  gratingLayout2->addWidget(vlsGroup);
  gratingGroup->setLayout(gratingLayout2);
  // end grating

  // misalignment
 QGroupBox *alignmentGroup = new QGroupBox(tr("&misalignment, opt. surface size, slope errors (arcsec)"));
 QGridLayout *alignmentLayout = new QGridLayout;
 QLabel *duLabel  = new QLabel(tr("du (mm)"));
 QLabel *dwLabel  = new QLabel(tr("dw (mm)"));
 QLabel *dlLabel  = new QLabel(tr("dl (mm)"));
 QLabel *dRuLabel = new QLabel(tr("du (mrad)"));
 QLabel *dRwLabel = new QLabel(tr("dw (mrad)"));
 QLabel *dRlLabel = new QLabel(tr("dl (mrad)"));
 QLabel *w1Label  = new QLabel(tr("w1 (mm)"));
 QLabel *w2Label  = new QLabel(tr("w2 (mm)"));
 QLabel *wsLabel  = new QLabel(tr("w slope"));
 QLabel *l1Label  = new QLabel(tr("l1 (mm)"));
 QLabel *l2Label  = new QLabel(tr("l2 (mm)"));
 QLabel *lsLabel  = new QLabel(tr("l slope"));

 duE  = new QLineEdit;
 dwE  = new QLineEdit;
 dlE  = new QLineEdit;
 dRuE = new QLineEdit;
 dRwE = new QLineEdit;
 dRlE = new QLineEdit;
 w1E  = new QLineEdit;
 w2E  = new QLineEdit;
 wsE  = new QLineEdit;
 l1E  = new QLineEdit;
 l2E  = new QLineEdit;
 lsE  = new QLineEdit;

 alignmentLayout->addWidget(duLabel,0,0);
 alignmentLayout->addWidget(dwLabel,1,0);
 alignmentLayout->addWidget(dlLabel,2,0);
 alignmentLayout->addWidget(duE,0,1);
 alignmentLayout->addWidget(dwE,1,1);
 alignmentLayout->addWidget(dlE,2,1);

 alignmentLayout->addWidget(dRuLabel,0,2);
 alignmentLayout->addWidget(dRwLabel,1,2);
 alignmentLayout->addWidget(dRlLabel,2,2);
 alignmentLayout->addWidget(dRuE,0,3);
 alignmentLayout->addWidget(dRwE,1,3);
 alignmentLayout->addWidget(dRlE,2,3);

 alignmentLayout->addWidget(w1Label,0,4);
 alignmentLayout->addWidget(w2Label,1,4);
 alignmentLayout->addWidget(wsLabel,2,4);
 alignmentLayout->addWidget(w1E,0,5);
 alignmentLayout->addWidget(w2E,1,5);
 alignmentLayout->addWidget(wsE,2,5);

 alignmentLayout->addWidget(l1Label,0,6);
 alignmentLayout->addWidget(l2Label,1,6);
 alignmentLayout->addWidget(lsLabel,2,6);
 alignmentLayout->addWidget(l1E,0,7);
 alignmentLayout->addWidget(l2E,1,7);
 alignmentLayout->addWidget(lsE,2,7);

 alignmentGroup->setLayout(alignmentLayout);
 // end misalignment
 
 QVBoxLayout *vbox = new QVBoxLayout;
 vbox->addWidget(groupBox1);
 vbox->addWidget(geometryGroup);
 vbox->addWidget(gratingGroup);
 vbox->addWidget(alignmentGroup);
 elementBox->setLayout(vbox);
 
 return elementBox;
} // end createopticalelementbox

// parameter box
QWidget *MainWindow::createParameterBox()
{
  parameterBox = new QWidget();

  // upper part
  QGroupBox   *parameterGroup  = new QGroupBox(tr("Parameters"));
  QVBoxLayout *parameterLayout = new QVBoxLayout;

  parameterList = new QListWidget();
  parameterList->addItems(QStringList()
			<< "0.001 : (epsilon) for Newton routine (1e-4)"
			<< "7: (iord) calculation up to order (3..7)"
			<< "1 : Grating 1"
			<< "1 : Mirror 3"
			<< "1 : Mirror 4"
			<< "1 : Mirror 4"
			<< "Mirror 4"
			<< "Mirror 4"
			<< "Mirror 4"

			);
#ifdef HEINZ
  QTableView *view = new QTableView;
  QSqlTableModel *model = new QSqlTableModel;
     view->setModel(model);
     view->show();
  model->setHeaderData(0, Qt::Horizontal, QObject::tr("ID"));
     model->setHeaderData(1, Qt::Horizontal, QObject::tr("Name"));
     model->setHeaderData(2, Qt::Horizontal, QObject::tr("City"));
     model->setHeaderData(3, Qt::Horizontal, QObject::tr("Country"));
#endif

  QLabel *parameterLabel  = new QLabel(tr("edit value"));
  parameterE  = new QLineEdit;

  parameterLayout->addWidget(parameterList);
  parameterLayout->addWidget(parameterLabel);
  parameterLayout->addWidget(parameterE);
  //  parameterLayout->addWidget(view);

  parameterGroup->setLayout(parameterLayout);

  QVBoxLayout *vbox = new QVBoxLayout;
  vbox->addWidget(parameterGroup);
  
  vbox->addStretch(1);
  parameterBox->setLayout(vbox);
  connect(parameterList, SIGNAL(itemSelectionChanged()), this, SLOT(selectParameter()));
  connect(parameterE, SIGNAL(editingFinished()), this, SLOT(parameterUpdateSlot()));
  return parameterBox;
} // end createparameter box




// the optical element box
QWidget *MainWindow::createSourceBox()
{
  sourceBox = new QWidget();

  // upper part
  QGroupBox *sourceTypeGroup = new QGroupBox(tr("&Source type"));
  QHBoxLayout *sourceTypeLayout = new QHBoxLayout;

// popup button
  QPushButton *sourceTypeButton = new QPushButton(tr("&Type"));
  sourceTypeLabel = new QLabel(tr("RT hard edge")); // return value of menu
  sourceMenu = new QMenu(this);
  rthAct = new QAction(tr("RT hard edge"), this);
  dipAct = new QAction(tr("Dipol"), this);
  poiAct = new QAction(tr("Point"), this);
  rinAct = new QAction(tr("Ring"), this);
  genAct = new QAction(tr("generic Undulador"), this);    
  b2hAct = new QAction(tr("Undulator BESSY II (H)"), this);
  b2lAct = new QAction(tr("Undulator BESSY II (L)"), this);
  sisAct = new QAction(tr("Undulator SLS - SIS"), this);  
  simAct = new QAction(tr("Undulator SLS - SIM"), this);  
  sffAct = new QAction(tr("Source from file"), this);  

  sourceMenu->addAction(rthAct);
  sourceMenu->addAction(dipAct);
  sourceMenu->addAction(poiAct);
  sourceMenu->addAction(rinAct);
  sourceMenu->addSeparator();
  sourceMenu->addAction(genAct);
  sourceMenu->addAction(b2hAct);
  sourceMenu->addAction(b2lAct);
  sourceMenu->addAction(sisAct);
  sourceMenu->addAction(simAct);  
  sourceMenu->addSeparator();
  sourceMenu->addAction(sffAct);
  sourceTypeButton->setMenu(sourceMenu);

  connect(rthAct, SIGNAL(triggered()), signalMapper, SLOT(map()));
  connect(dipAct, SIGNAL(triggered()), signalMapper, SLOT(map()));
  connect(poiAct, SIGNAL(triggered()), signalMapper, SLOT(map()));
  connect(rinAct, SIGNAL(triggered()), signalMapper, SLOT(map()));
  connect(genAct, SIGNAL(triggered()), signalMapper, SLOT(map()));
  connect(b2hAct, SIGNAL(triggered()), signalMapper, SLOT(map()));
  connect(b2lAct, SIGNAL(triggered()), signalMapper, SLOT(map()));
  connect(sisAct, SIGNAL(triggered()), signalMapper, SLOT(map()));
  connect(simAct, SIGNAL(triggered()), signalMapper, SLOT(map()));
  connect(sffAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

  signalMapper->setMapping(rthAct, QString("rthAct"));
  signalMapper->setMapping(dipAct, QString("dipAct"));
  signalMapper->setMapping(poiAct, QString("poiAct"));
  signalMapper->setMapping(rinAct, QString("rinAct"));
  signalMapper->setMapping(genAct, QString("genAct"));

  signalMapper->setMapping(b2hAct, QString("b2hAct"));
  signalMapper->setMapping(b2lAct, QString("b2lAct"));
  signalMapper->setMapping(sisAct, QString("sisAct"));
  signalMapper->setMapping(simAct, QString("simAct"));
  signalMapper->setMapping(sffAct, QString("sffAct"));
  
  sourceFileBox = new QCheckBox(tr("create Source file"));

  sourceTypeLayout->addWidget(sourceTypeButton);
  sourceTypeLayout->addStretch(1);
  sourceTypeLayout->addWidget(sourceTypeLabel);
  sourceTypeLayout->addStretch(1);
  sourceTypeLayout->addWidget(sourceFileBox);
  sourceTypeGroup->setLayout(sourceTypeLayout); // end upper part
  
  QGroupBox *sourceParsGroup = new QGroupBox(tr("Parameters"));
  QGridLayout *sourceParsLayout = new QGridLayout;

  S1Label = new QLabel(tr("height (mm)"));
  S3Label = new QLabel(tr("width (mm)"));
  S5Label = new QLabel(tr("vert. div. (mrad)"));
  S7Label = new QLabel(tr("hor. div. (mrad)"));
  S2Label = new QLabel(tr("-> points"));
  S4Label = new QLabel(tr("-> points"));
  S6Label = new QLabel(tr("-> points"));
  S8Label = new QLabel(tr("-> points"));

  S1E = new QLineEdit;
  S2E = new QLineEdit;
  S3E = new QLineEdit;
  S4E = new QLineEdit;
  S5E = new QLineEdit;
  S6E = new QLineEdit;
  S7E = new QLineEdit;
  S8E = new QLineEdit;

  sourceApplyB   = new QPushButton(tr("Apply"));
  sourceDefaultB = new QPushButton(tr("Defaults"));

  sourceParsLayout->addWidget(S1Label,0,0);
  sourceParsLayout->addWidget(S3Label,1,0);
  sourceParsLayout->addWidget(S5Label,2,0);
  sourceParsLayout->addWidget(S7Label,3,0);
  sourceParsLayout->addWidget(S2Label,0,2);
  sourceParsLayout->addWidget(S4Label,1,2);
  sourceParsLayout->addWidget(S6Label,2,2);
  sourceParsLayout->addWidget(S8Label,3,2);

  sourceParsLayout->addWidget(S1E,0,1);
  sourceParsLayout->addWidget(S2E,0,3);
  sourceParsLayout->addWidget(S3E,1,1);
  sourceParsLayout->addWidget(S4E,1,3);
  sourceParsLayout->addWidget(S5E,2,1);
  sourceParsLayout->addWidget(S6E,2,3);
  sourceParsLayout->addWidget(S7E,3,1);
  sourceParsLayout->addWidget(S8E,3,3);

  sourceParsLayout->addWidget(sourceApplyB,  3, 4);
  sourceParsLayout->addWidget(sourceDefaultB,2, 4);

    sourceParsGroup->setLayout(sourceParsLayout);

  QVBoxLayout *vbox = new QVBoxLayout;
  vbox->addWidget(sourceTypeGroup);
  vbox->addWidget(sourceParsGroup);

  sourceBox->setLayout(vbox);
  connect(sourceApplyB,   SIGNAL(clicked()), this, SLOT(sourceApplyBslot()));
  connect(sourceDefaultB, SIGNAL(clicked()), this, SLOT(sourceDefaultBslot()));

 return sourceBox;
} // end createsource box


// statusbar at the bottom
void MainWindow::createStatusBar()
{
    statusBar()->showMessage(tr("Ready"));
} // createStatusBar

// toolbar with icons
void MainWindow::createToolBars()
{
    fileToolBar = addToolBar(tr("File"));
    fileToolBar->addAction(newLetterAct);
    fileToolBar->addAction(saveAct);
    fileToolBar->addAction(printAct);

    editToolBar = addToolBar(tr("Edit"));
    editToolBar->addAction(undoAct);
} // createToolBars


///////////////////////////////////
// end widget definition section //
///////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////
// begin widget handling section //
///////////////////////////////////

// helper function for the parameterUpdateSlot
void MainWindow::parameterUpdate(int pos, char *text)
{
  char buffer[MaxPathLength];
  int scanned;
  QListWidgetItem *item= parameterList->item(pos);
  printf("parameterUpdate: pos: %d\n", pos);
  switch (pos)
    {
    case 0:
      scanned= sscanf(text, "%lf", &this->BLOptions.epsilon);
      if ((scanned == EOF) || (scanned == 0)) this->BLOptions.epsilon= 1e-4; // default
      sprintf(buffer, "%-5lg : %s", this->BLOptions.epsilon, "(epsilon) for Newton routine (1e-4)");
      break;
    case 1:
      scanned= sscanf(text, "%d", &this->BLOptions.ifl.iord);
      printf("scanned: %d\n",scanned);
      if ((scanned == EOF) || (scanned == 0) || (this->BLOptions.ifl.iord < 1) || 
	  (this->BLOptions.ifl.iord > 7)) this->BLOptions.ifl.iord= 4;             // set default
      sprintf(buffer, "%d : %s", this->BLOptions.ifl.iord, "(iord) calculation up to order (1..7)");
      break;
    case 2:
      scanned= sscanf(text, "%d", &this->BLOptions.ifl.iord);
      if (scanned == EOF) this->BLOptions.ifl.iord= 4; // default
      sprintf(buffer, "%d : %s", this->BLOptions.ifl.iord, "(iord) calculation up to order (3..7)");
      break;
    default:
      sprintf(buffer, "%d : unknown parameter", pos);
    }
  item->setText(buffer);
} // end parameterUpdate

// UpdateBeamlineBox()
// the box on the left
void MainWindow::UpdateBeamlineBox()
{
  struct OptionsType *blo;
  char   buffer[5];
    
  blo= &(this->BLOptions);

  sprintf(buffer, "%4f", blo->lambda* 1e6);
  lambdaE->setText(QString(buffer));

  sprintf(buffer, "%4f", blo->displength);
  dislenE->setText(QString(buffer));

  if (blo->SourcetoImage) goButton->setChecked(true); else poButton->setChecked(true);
  if (blo->WithAlign) misaliBox->setChecked(true);    else misaliBox->setChecked(false);
} // end UpdateBeamlineBox

// UpdateElementBox
// box at right buttom
// called with the index of the optical element in the list
void MainWindow::UpdateElementBox(int number)
{
  double cff, teta, fi;
  char TextField [26][40];            /* 26 editfelder */

  if (number < 0) return;
  this->ElementList[number].ElementOK = 0;
  this->beamlineOK                    &= ~mapOK;

  struct mdatset *md= &(this->ElementList[number].MDat);
  struct gdatset *gd= &(this->ElementList[number].GDat);

  teta= fabs(gd->theta0* PI/ 180.0);
  fi  = (double)(gd->inout)* 
    asin(gd->lambda* gd->xdens[0]/ (2.0* cos(teta)));
  cff = cos(fi- teta)/ cos(fi+ teta);

  // create strings
  sprintf(TextField[0],  "%.2f", cff);
  sprintf(TextField[1],  "%.1f", gd->r);
  sprintf(TextField[2],  "%.1f", gd->rp);
  sprintf(TextField[3],  "%.3f", gd->theta0);
  sprintf(TextField[4],  "%.1f", md->r1);   
  sprintf(TextField[5],  "%.1f", md->r2);
  sprintf(TextField[6],  "%.1f", md->rmi);      
  sprintf(TextField[7],  "%.2f", md->rho);
  sprintf(TextField[8],  "%d",   gd->inout);   // not used   	   
  sprintf(TextField[9],  "%.2f", gd->xdens[0]); 
  sprintf(TextField[10], "%.2f", gd->xdens[1]);    
  sprintf(TextField[11], "%.2f", gd->xdens[2]);    
  sprintf(TextField[12], "%.2f", gd->xdens[3]);    
  sprintf(TextField[13], "%.2f", gd->xdens[4]);
  sprintf(TextField[14], "%.2f", md->du);    
  sprintf(TextField[15], "%.2f", md->dw);    
  sprintf(TextField[16], "%.2f", md->dl);
  sprintf(TextField[17], "%.2f", md->dRu * 1e3);    
  sprintf(TextField[18], "%.2f", md->dRw * 1e3);    
  sprintf(TextField[19], "%.2f", md->dRl * 1e3);
  sprintf(TextField[20], "%.2f", md->w1);    
  sprintf(TextField[21], "%.2f", md->w2);    
  sprintf(TextField[22], "%.3f", md->slopew);
  sprintf(TextField[23], "%.2f", md->l1);    
  sprintf(TextField[24], "%.2f", md->l2);    
  sprintf(TextField[25], "%.3f", md->slopel);

  // update widgets
  cffE   ->setText(QString(tr(TextField[0])));
  preE   ->setText(QString(tr(TextField[1])));
  sucE   ->setText(QString(tr(TextField[2])));
  thetaE ->setText(QString(tr(TextField[3])));
  sourceE->setText(QString(tr(TextField[4])));
  imageE ->setText(QString(tr(TextField[5])));
  rE     ->setText(QString(tr(TextField[6])));
  rhoE   ->setText(QString(tr(TextField[7])));
  integerSpinBox->setValue(gd->inout);
  lineDensity->setText(QString(tr(TextField[9])));
  vls1->setText(QString(tr(TextField[10])));
  vls2->setText(QString(tr(TextField[11])));
  vls3->setText(QString(tr(TextField[12])));
  vls4->setText(QString(tr(TextField[13])));
  duE ->setText(QString(tr(TextField[14])));
  dwE ->setText(QString(tr(TextField[15])));
  dlE ->setText(QString(tr(TextField[16])));
  dRuE->setText(QString(tr(TextField[17])));
  dRwE->setText(QString(tr(TextField[18])));
  dRlE->setText(QString(tr(TextField[19])));
  w1E ->setText(QString(tr(TextField[20])));
  w2E ->setText(QString(tr(TextField[21])));
  wsE ->setText(QString(tr(TextField[22])));
  l1E ->setText(QString(tr(TextField[23])));
  l2E ->setText(QString(tr(TextField[24])));
  lsE ->setText(QString(tr(TextField[25])));

  if (gd->iflag) nimBox->setChecked(true); else nimBox->setChecked(false);

#ifdef DEBUG 		      
  printf("debug: UpdateElementBox: gd->azimut: %d\n", gd->azimut); 
  printf("debug: UpdateElementBox: md->Art:    %d\n", md->Art); 
  printf("debug: UpdateElementBox: md->Art:    %d\n", md->Art & 1023); 
#endif

  // orientation
  switch (gd->azimut)
    {
    case 0: rup1   ->setChecked(true); break;
    case 1: rleft2 ->setChecked(true); break;
    case 2: rdown3 ->setChecked(true); break;
    case 3: rright4->setChecked(true); break;
    }

  // element type set strings and sensitivity
  gratingGroup->setChecked(false);
  vlsGroup    ->setChecked(false);
  switch (md->Art & 1023)          // strip off higher bits
    {
    case kEOEPG:                   // for compatibility with old datasets
      md->Art= kEOEPM + GRATINGBIT ;   
    case kEOEPM:
      shapeLabel->setText(QString(tr("flat"))); break;
    case kEOETG:                   // for compatibility with old datasets
      md->Art= kEOETM + GRATINGBIT ;
    case kEOEVLSG:                 // for compatibility with old datasets 
      md->Art= kEOETM + GRATINGBIT + VLSBIT ;
    case kEOETM:   
      shapeLabel->setText(QString(tr("toroidal"))); break;
    case kEOEPElli:   
      shapeLabel->setText(QString(tr("plane-elliptical"))); break;
    case kEOEElli:   
      shapeLabel->setText(QString(tr("elliptical"))); break;
    case kEOECone:   
      shapeLabel->setText(QString(tr("conical"))); break;
    case kEOEGeneral:
      shapeLabel->setText(QString(tr("generic"))); break;
    default: 
      shapeLabel->setText(QString(tr("unknown")));
      QMessageBox::warning(this, tr("UpdateElementBox"),
			   tr("Shape type %1 not recognized.\nreport bug to uwe.flechsig@psi.ch")
			   .arg(md->Art));
      break;
    }
  if (md->Art & GRATINGBIT ) gratingGroup->setChecked(true);
  if (md->Art & VLSBIT     )     vlsGroup->setChecked(true);

} // end UpdateElementBox



// updates the elementlist
void MainWindow::UpdateElementList()
{
  unsigned int ui;
  struct ElementType *list;
  
#ifdef DEBUG
  printf("MainWindow::UpdateElementList(): elements in widget:  %d\n", elementList->count());
  printf("MainWindow::UpdateElementList(): elements in dataset: %d\n", elementzahl);
#endif

  // loesche alles
   while (elementList->count()) 
    delete elementList->takeItem(0);

  list= this->ElementList;
  for (ui= 0; ui < elementzahl; ui++, list++)
    elementList->addItem(QString(list->elementname));
} // end UpdateElementList()

// update the source box
void MainWindow::UpdateSourceBox()
{
  char sou;
  struct UndulatorSourceType  *up;
  struct UndulatorSource0Type *up0;
  struct DipolSourceType      *dp;
  struct PointSourceType      *sop;
  struct RingSourceType       *rp;
  struct HardEdgeSourceType   *hp;     
  //  struct SRSourceType         *sp; 
  //  struct PSImageType          *psip;
  //  struct PSSourceType         *pssp; 

  char TextField [8][40];            /* 8 editfelder */
  char LabelField[9][40]; 
   
  this->beamlineOK &= ~sourceOK; 
        
#ifdef DEBUG 
    printf("InitSourceBox: bl->RTSource.QuellTyp: %c, beamlineOK: %X\n", 
	   this->RTSource.QuellTyp, this->beamlineOK);   
#endif   
 
    if (RTSource.Quellep == NULL)
      {
	printf("error: UpdateSourceBox: Quellep == NULL\n");
	return;
      }
    AllocRTSource(this);
    sou= this->RTSource.QuellTyp;

    switch (sou) {
 
    case 'D':
      dp= (struct DipolSourceType *)this->RTSource.Quellep;
      sprintf(TextField[0],  "%f", dp->sigy);
      sprintf(TextField[1],  "%f", dp->sigdy);    
      sprintf(TextField[2],  "%f", dp->sigz);  
      sprintf(TextField[3],  "%f", dp->dz);    
      sprintf(TextField[4],  "%d", this->RTSource.raynumber);   
      sprintf(TextField[5],  "%s", "");    
      sprintf(TextField[6],  "%s", "");   
      sprintf(TextField[7],  "%s", ""); 
      sprintf(LabelField[0], "%s", "sigmay (mm)");
      sprintf(LabelField[1], "%s", "sigmady (mrad)");  
      sprintf(LabelField[2], "%s", "sigmaz (mm)");  
      sprintf(LabelField[3], "%s", "dz hard edge (mrad)");  
      sprintf(LabelField[4], "%s", "ray number");   
      sprintf(LabelField[5], "%s", "");   
      sprintf(LabelField[6], "%s", "");   
      sprintf(LabelField[7], "%s", "");
      sprintf(LabelField[8], "%s", "Dipol (Bending Magnet)");
      break;  
    case 'G':
      up0= (struct UndulatorSource0Type *)this->RTSource.Quellep;
      sprintf(TextField[0],  "%f", up0->length);
      sprintf(TextField[1],  "%f", this->BLOptions.lambda* 1e6);    
      sprintf(TextField[2],  "%d", this->RTSource.raynumber);  
      sprintf(TextField[3],  "%f", up0->deltaz);    
      sprintf(TextField[4],  "%f", up0->sigmaey);   
      sprintf(TextField[5],  "%f", up0->sigmaez);    
      sprintf(TextField[6],  "%f", up0->sigmaedy);   
      sprintf(TextField[7],  "%f", up0->sigmaedz); 
      sprintf(LabelField[0], "%s", "length (mm)");
      sprintf(LabelField[1], "%s", "lambda (nm)");  
      sprintf(LabelField[2], "%s", "ray number");  
      sprintf(LabelField[3], "%s", "deltaz (mm)");  
      sprintf(LabelField[4], "%s", "sigmaey (mm)");   
      sprintf(LabelField[5], "%s", "sigmaez (mm)");   
      sprintf(LabelField[6], "%s", "sigmaedy (mrad)");   
      sprintf(LabelField[7], "%s", "sigmaedz (mrad)");
      sprintf(LabelField[8], "%s", "Generic undulator");
      break;  
    case 'H':
      hp= (struct HardEdgeSourceType *)this->RTSource.Quellep;
      sprintf(TextField[0],  "%f", hp->disty);
      sprintf(TextField[1],  "%d", hp->iy);    
      sprintf(TextField[2],  "%f", hp->distz);  
      sprintf(TextField[3],  "%d", hp->iz);    
      sprintf(TextField[4],  "%f", hp->divy);   
      sprintf(TextField[5],  "%d", hp->idy);    
      sprintf(TextField[6],  "%f", hp->divz);   
      sprintf(TextField[7],  "%d", hp->idz); 
      sprintf(LabelField[0], "%s", "height (mm)");
      sprintf(LabelField[1], "%s", "-> points");  
      sprintf(LabelField[2], "%s", "width (mm)");  
      sprintf(LabelField[3], "%s", "-> points");  
      sprintf(LabelField[4], "%s", "vert. div. (mrad)");   
      sprintf(LabelField[5], "%s", "-> points");   
      sprintf(LabelField[6], "%s", "hor. div. (mrad)");   
      sprintf(LabelField[7], "%s", "-> points");
      sprintf(LabelField[8], "%s", "Ray Trace hard edge");
      break;  

    case 'L':
      up= (struct UndulatorSourceType *)this->RTSource.Quellep;
      sprintf(TextField[0],  "%f", up->length);
      sprintf(TextField[1],  "%f", this->BLOptions.lambda* 1e6);    
      sprintf(TextField[2],  "%d", this->RTSource.raynumber);  
      sprintf(TextField[3],  "%f", up->deltaz);    
      sprintf(TextField[4],  "%s", "");   
      sprintf(TextField[5],  "%s", "");    
      sprintf(TextField[6],  "%s", "");   
      sprintf(TextField[7],  "%s", ""); 
      sprintf(LabelField[0], "%s", "length (mm)");
      sprintf(LabelField[1], "%s", "lambda (nm)");  
      sprintf(LabelField[2], "%s", "ray number");  
      sprintf(LabelField[3], "%s", "deltaz (mm)");  
      sprintf(LabelField[4], "%s", "");   
      sprintf(LabelField[5], "%s", "");   
      sprintf(LabelField[6], "%s", "");   
      sprintf(LabelField[7], "%s", "");
      sprintf(LabelField[8], "%s", "SLS SIS undulator");
      break; 

    case 'M':
      up= (struct UndulatorSourceType *)this->RTSource.Quellep;
      sprintf(TextField[0],  "%f", up->length);
      sprintf(TextField[1],  "%f", this->BLOptions.lambda* 1e6);    
      sprintf(TextField[2],  "%d", this->RTSource.raynumber);  
      sprintf(TextField[3],  "%f", up->deltaz);    
      sprintf(TextField[4],  "%s", "");   
      sprintf(TextField[5],  "%s", "");    
      sprintf(TextField[6],  "%s", "");   
      sprintf(TextField[7],  "%s", ""); 
      sprintf(LabelField[0], "%s", "length (mm)");
      sprintf(LabelField[1], "%s", "lambda (nm)");  
      sprintf(LabelField[2], "%s", "ray number");  
      sprintf(LabelField[3], "%s", "deltaz (mm)");  
      sprintf(LabelField[4], "%s", "");   
      sprintf(LabelField[5], "%s", "");   
      sprintf(LabelField[6], "%s", "");   
      sprintf(LabelField[7], "%s", "");
      sprintf(LabelField[8], "%s", "SLS SIM undulator");
      break;  

    case 'o':
      sop= (struct PointSourceType *)this->RTSource.Quellep;
      sprintf(TextField[0],  "%f", sop->sigy);
      sprintf(TextField[1],  "%f", sop->sigdy);    
      sprintf(TextField[2],  "%f", sop->sigz);  
      sprintf(TextField[3],  "%f", sop->sigdz);    
      sprintf(TextField[4],  "%d", this->RTSource.raynumber);   
      sprintf(TextField[5],  "%s", "");    
      sprintf(TextField[6],  "%s", "");   
      sprintf(TextField[7],  "%s", ""); 
      sprintf(LabelField[0], "%s", "sigy (mm)");
      sprintf(LabelField[1], "%s", "sigdy (mrad)");  
      sprintf(LabelField[2], "%s", "sigz (mm)");  
      sprintf(LabelField[3], "%s", "sigdz (mrad)");  
      sprintf(LabelField[4], "%s", "ray number");   
      sprintf(LabelField[5], "%s", "");   
      sprintf(LabelField[6], "%s", "");   
      sprintf(LabelField[7], "%s", "");
      sprintf(LabelField[8], "%s", "Point Source: all sigma values");
      break;  

 case 'R':
      rp= (struct RingSourceType *)this->RTSource.Quellep;
      sprintf(TextField[0],  "%lf", rp->dy);
      sprintf(TextField[1],  "%lf", rp->dz);    
      sprintf(TextField[2],  "%d", this->RTSource.raynumber);  
      sprintf(TextField[3],  "%s", "");    
      sprintf(TextField[4],  "%s", "");   
      sprintf(TextField[5],  "%s", "");    
      sprintf(TextField[6],  "%s", "");   
      sprintf(TextField[7],  "%s", ""); 
      sprintf(LabelField[0], "%s", "dy (mrad)");
      sprintf(LabelField[1], "%s", "dz (mrad)");  
      sprintf(LabelField[2], "%s", "ray number");  
      sprintf(LabelField[3], "%s", "");  
      sprintf(LabelField[4], "%s", "");   
      sprintf(LabelField[5], "%s", "");   
      sprintf(LabelField[6], "%s", "");   
      sprintf(LabelField[7], "%s", "");
      sprintf(LabelField[8], "%s", "Ring Source: half axis of the divergence ellipse, y,z are always 0");
      break;  
   
    case 'U':
      up= (struct UndulatorSourceType *)this->RTSource.Quellep;
      sprintf(TextField[0],  "%f", up->length);
      sprintf(TextField[1],  "%f", this->BLOptions.lambda* 1e6);    
      sprintf(TextField[2],  "%d", this->RTSource.raynumber);  
      sprintf(TextField[3],  "%s", "");    
      sprintf(TextField[4],  "%s", "");   
      sprintf(TextField[5],  "%s", "");    
      sprintf(TextField[6],  "%s", "");   
      sprintf(TextField[7],  "%s", ""); 
      sprintf(LabelField[0], "%s", "length (mm)");
      sprintf(LabelField[1], "%s", "lambda (nm)");  
      sprintf(LabelField[2], "%s", "ray number");  
      sprintf(LabelField[3], "%s", "");  
      sprintf(LabelField[4], "%s", "");   
      sprintf(LabelField[5], "%s", "");   
      sprintf(LabelField[6], "%s", "");   
      sprintf(LabelField[7], "%s", "");
      sprintf(LabelField[8], "%s", "Undulator");
      break;  


    default:
      QMessageBox::warning(this, tr("UpdateSourceBox"),
			   tr("Source type %1 not recognized.")
			 .arg(sou));
      break;
    }

  // update widgets
  S1E->setText(QString(tr(TextField[0])));
  S2E->setText(QString(tr(TextField[1])));
  S3E->setText(QString(tr(TextField[2])));
  S4E->setText(QString(tr(TextField[3])));
  S5E->setText(QString(tr(TextField[4])));
  S6E->setText(QString(tr(TextField[5])));
  S7E->setText(QString(tr(TextField[6])));
  S8E->setText(QString(tr(TextField[7])));
  S1Label->setText(QString(tr(LabelField[0])));
  S2Label->setText(QString(tr(LabelField[1])));
  S3Label->setText(QString(tr(LabelField[2])));
  S4Label->setText(QString(tr(LabelField[3])));
  S5Label->setText(QString(tr(LabelField[4])));
  S6Label->setText(QString(tr(LabelField[5])));
  S7Label->setText(QString(tr(LabelField[6])));
  S8Label->setText(QString(tr(LabelField[7])));
  sourceTypeLabel->setText(QString(tr(LabelField[8])));
} // end UpdateSourceBox

// update the statistics in graphic box
void MainWindow::UpdateStatistics(Plot *pp, char *label, int rays)
{
  char buffer[255];
  double trans;

  trans= (this->RTSource.raynumber > 0) ? this->RESULT.points/ this->RTSource.raynumber : 0;
  sprintf(buffer, "%s Statistics", label);  
  statGroup->setTitle(QString(tr(buffer)));
  sprintf(buffer, "%s %8.3f", "z center (mm): ",    pp->cz);  
  czLabel->setText(QString(tr(buffer)));
  sprintf(buffer, "%s %8.3f", "y center (mm): ",    pp->cy);  
  cyLabel->setText(QString(tr(buffer)));
  sprintf(buffer, "%s %8.3f", "z FWHM (mm): ",      pp->wz);  
  wzLabel->setText(QString(tr(buffer)));
  sprintf(buffer, "%s %8.3f", "y FWHM (mm): ",      pp->wy);  
  wyLabel->setText(QString(tr(buffer)));
  sprintf(buffer, "%s %8.3f", "dz center (mrad): ", pp->cdz * 1e3);  
  cdzLabel->setText(QString(tr(buffer)));
  sprintf(buffer, "%s %8.3f", "dy center (mrad): ", pp->cdy * 1e3);  
  cdyLabel->setText(QString(tr(buffer)));
  sprintf(buffer, "%s %8.3f", "dz FWHM (mrad): ",   pp->wdz * 1e3);  
  wdzLabel->setText(QString(tr(buffer)));
  sprintf(buffer, "%s %8.3f", "dy FWHM (mrad): ",   pp->wdy * 1e3);  
  wdyLabel->setText(QString(tr(buffer)));
  sprintf(buffer, "%s %d", "rays: ", rays);  
  rayLabel->setText(QString(tr(buffer)));
  sprintf(buffer, "%s %8.3f", "transmittance: ",    trans);  
  traLabel->setText(QString(tr(buffer)));

  sprintf(buffer, "%s %8.3f", "y E/dE FWHM: ",      pp->ry);  
  ryLabel->setText(QString(tr(buffer)));

  sprintf(buffer, "%s %8.3f", "z E/dE FWHM: ",      pp->rz);  
  rzLabel->setText(QString(tr(buffer)));

} // UpdateStatistics

/////////////////////////////////
// end widget handling section //
/////////////////////////////////

// /afs/psi.ch/user/f/flechsig/phase/src/qtgui/mainwindow.cpp
