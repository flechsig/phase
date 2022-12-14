//  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/configwindow.cpp
//  Date      : <16 Aug 11 12:20:33 flechsig> 
//  Time-stamp: <2022-12-14 16:59:49 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 

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

// the configure window

#include <string.h>
#if (QT_VERSION < 0x050000)
#include <QtGui>
#else
#include <QtWidgets>
#endif
#include "configwindow.h"

using namespace std;

// the constructor
ConfigWindow::ConfigWindow(PhaseQt *parent)
{
#ifdef DEBUG
  cout << "debug: ConfigWindow constructor called, file: " << __FILE__ << " line: " << __LINE__ << endl;
#endif

  configWindowBox = new QWidget();
  
  configWindowBox->setWindowTitle(tr("PHASE configuration"));
  QLabel *configLabel = new QLabel(tr("configure auxiliary files"));
  
  sourceView = new QTreeView;
  sourceView->setRootIsDecorated(false);
  sourceView->setAlternatingRowColors(true);
    
  QGroupBox   *lowerButtonGroup  = new QGroupBox(tr(""));
  QHBoxLayout *lowerButtonLayout = new QHBoxLayout;
  configApplyB   = new QPushButton(tr("&Apply"));
  configQuitB    = new QPushButton(tr("&Quit"));
  configDefaultB = new QPushButton(tr("&Defaults"));
  lowerButtonLayout->addWidget(configDefaultB);
  lowerButtonLayout->addWidget(configApplyB);
  lowerButtonLayout->addWidget(configQuitB);
  lowerButtonGroup->setLayout(lowerButtonLayout);

  QVBoxLayout *vbox = new QVBoxLayout;
  vbox->addWidget(configLabel);
  vbox->addWidget(sourceView);
  vbox->addWidget(lowerButtonGroup);
  configWindowBox->setLayout(vbox);
  
  myparent= parent;
  mymodel= createConfigModel(this); // create the dataset
  sourceView->setModel(mymodel);    // attach dataset to view
  fillList();
  
  connect(configDefaultB, SIGNAL(clicked()), this,  SLOT(defaultSlot()));
  connect(configApplyB,   SIGNAL(clicked()), this,  SLOT(applySlot()));
  connect(configQuitB,    SIGNAL(clicked()), this,  SLOT(quitSlot()));
  connect(sourceView,     SIGNAL(clicked(QModelIndex)), this, SLOT(selectSlot(QModelIndex)));

  configWindowBox->resize(600, 310); // show all rows with reasonable width  
  sourceView->resizeColumnToContents(0);
  sourceView->resizeColumnToContents(1);
  sourceView->resizeColumnToContents(2);
  configWindowBox->show();
 } // end constructor

// slots
void ConfigWindow::applySlot()
{
#ifdef DEBUG
  cout << "debug: applySlot called" << endl;
#endif

  checkFileNames();
  myparent->writeBackupFile();
} // applySlot

// set filenames to defaults based on beamlinename
void ConfigWindow::defaultSlot()
{
#ifdef DEBUG
  cout << "debug: defaultSlot called" << endl;
#endif

  myparent->initSet(myparent->myBeamline()->filenames.beamlinename, INIT_ALL);
  updateList();
} // defaultSlot

void ConfigWindow::quitSlot()
{
  this->configWindowBox->close();
} // quitSlot

// does everything
void ConfigWindow::selectSlot(const QModelIndex &index)
{
  char *fname, description[MaxPathLength], oldname[MaxPathLength], extension[10], filter[50];
  int row;

  if (!index.isValid())
    return;
   
  row= index.row();

#ifdef DEBUG1 
  cout << "debug: " << __FILE__ << " row, column " << row << "," << index.column() << endl;
#endif  

  QStandardItem *des = mymodel->item(row, 0); // the description
  QStandardItem *fna = mymodel->item(row, 1); // the filename
  QStandardItem *ext = mymodel->item(row, 2); // the extension

   if (!fna || !ext || !des) 
     return;

   //    QMessageBox::warning(this, tr("item"),  item1->text());

   strncpy(description, des->text().toLocal8Bit().data(), MaxPathLength- 1);
   strncpy(oldname,     fna->text().toLocal8Bit().data(), MaxPathLength- 1);
   strncpy(extension,   ext->text().toLocal8Bit().data(), 9);

   description[(MaxPathLength- 1)]= '\0';  // ensure termination
   oldname[(MaxPathLength- 1)]= '\0';      // ensure termination
   extension[9]= '\0';                    // ensure termination
   snprintf(filter, 49, "Files (*.%s);;(*)", extension);

#ifdef DEBUG1 
   cout << "debug: file: " << __FILE__ << ", line: " << __LINE__ <<  " item: " <<  oldname << endl;
#endif 
   
  QFileDialog *dialog = new QFileDialog(this);
  dialog->selectFile(oldname);

  QString selectedFilter;
  QString fileName= ( strstr(description, "input") || strstr(description, "fsource") || strstr(description, "errors")) ? 
    dialog->getOpenFileName(this, tr("Define Input File"), QDir::currentPath(), tr(filter), 
			    &selectedFilter, QFileDialog::DontConfirmOverwrite) :
    dialog->getSaveFileName(this, tr("Define Output File"), QDir::currentPath(), tr(filter));   // different dialogs for in/out

  if (!fileName.isEmpty()) 
    {
      fname= fileName.toLocal8Bit().data();

      // update data
      if ( !strncmp(description, "optimization input", 16) ) 
	strncpy(myparent->myBeamline()->filenames.optipckname, fname, MaxPathLength- 1); else
	if ( !strncmp(description, "optimization results", 16) ) 
	  strncpy(myparent->myBeamline()->filenames.opresname, fname, MaxPathLength- 1); else
	  //if ( !strncmp(description, "minuit input", 10) ) 
	  //  strncpy(myparent->myBeamline()->filenames.minname, fname, MaxPathLength); else
	    if ( !strncmp(description, "GO input (source)", 6) ) 
	      strncpy(myparent->myBeamline()->filenames.sourceraysname, fname, MaxPathLength- 1); else
	      if ( !strncmp(description, "GO/PO output (image)", 6) )
		strncpy(myparent->myBeamline()->filenames.imageraysname, fname, MaxPathLength- 1); else
		if ( !strncmp(description, "matrix", 4) ) 
		  strncpy(myparent->myBeamline()->filenames.matrixname, fname, MaxPathLength- 1); else
		  if ( !strncmp(description, "mapname", 4) ) 
		    strncpy(myparent->myBeamline()->filenames.mapname, fname, MaxPathLength- 1); else
		    if ( !strncmp(description, "so4_fsource4a", 13) )
		      { 
			strncpy(myparent->myBeamline()->filenames.so4_fsource4a, fname, MaxPathLength- 1);
			//strncpy(myparent->myBeamline()->src.so4.fsource4a, fname, 80);
		      } else
		      if ( !strncmp(description, "so4_fsource4b", 13) ) 
			{
			  strncpy(myparent->myBeamline()->filenames.so4_fsource4b, fname, MaxPathLength- 1);
			  //strncpy(myparent->myBeamline()->src.so4.fsource4b, fname, 80);
			} else
			if ( !strncmp(description, "so4_fsource4c", 13) )
			  { 
			    strncpy(myparent->myBeamline()->filenames.so4_fsource4c, fname, MaxPathLength- 1);
			    //strncpy(myparent->myBeamline()->src.so4.fsource4c, fname, 80);
			  } else
			  if ( !strncmp(description, "so4_fsource4d", 13) ) 
			    {
			      strncpy(myparent->myBeamline()->filenames.so4_fsource4d, fname, MaxPathLength- 1);
			      //strncpy(myparent->myBeamline()->src.so4.fsource4d, fname, 80);
			    } else
			    if ( !strncmp(description, "so6_fsource6", 3) ) 
			      {
				strncpy(myparent->myBeamline()->filenames.so6_fsource6, fname, MaxPathLength- 1); 
				// strncpy(myparent->myBeamline()->src.so6.fsource6, fname, 80);
			      } else
			      if ( !strncmp(description, "so7", 3) ) 
				strncpy(myparent->myBeamline()->filenames.so7_hdf5, fname, MaxPathLength- 1); 
			      else
				if ( !strncmp(description, "hdf5", 4) ) 
				  strncpy(myparent->myBeamline()->filenames.hdf5_out, fname, MaxPathLength- 1);
				else
				  if ( !strncmp(description, "surface errors", 14) ) 
				    strncpy(myparent->myBeamline()->filenames.h5surfacename, fname, MaxPathLength- 1);
				  else
				    {
				      cout << "selectSlot: error: no matching description: >>" 
					   << description << "<< - exit" << endl;
				      exit(-1);
				    }
      
      // update widget
      mymodel->setData(mymodel->index(index.row(), 1), fname);
    }
} // end selectslot
////////////// end slots ////////////

// create the data model
QStandardItemModel *ConfigWindow::createConfigModel(QObject *parent)
{
    QStandardItemModel *model = new QStandardItemModel(0, 3, parent);

    model->setHeaderData(0, Qt::Horizontal, QObject::tr("Description"));
    model->setHeaderData(1, Qt::Horizontal, QObject::tr("Filename"));
    model->setHeaderData(2, Qt::Horizontal, QObject::tr("Extension"));
    return model;
} // end createConfigModel

// add all files to be configured with description
// !! any modification requires modification of updateList()
void ConfigWindow::fillList()
{
  // add in reverse order
  addRow("surface errors",       myparent->myBeamline()->filenames.h5surfacename,  "h5");
  addRow("hdf5 output",          myparent->myBeamline()->filenames.hdf5_out,       "h5");
  addRow("so7 (hdf5 input)",     myparent->myBeamline()->filenames.so7_hdf5,       "h5");
  addRow("so6_fsource6",         myparent->myBeamline()->filenames.so6_fsource6,   "s6");
  addRow("so4_fsource4d",        myparent->myBeamline()->filenames.so4_fsource4d,  "s4d");
  addRow("so4_fsource4c",        myparent->myBeamline()->filenames.so4_fsource4c,  "s4c");
  addRow("so4_fsource4b",        myparent->myBeamline()->filenames.so4_fsource4b,  "s4b");
  addRow("so4_fsource4a",        myparent->myBeamline()->filenames.so4_fsource4a,  "s4a");
  addRow("map name",             myparent->myBeamline()->filenames.mapname,        "map");
  addRow("matrix name",          myparent->myBeamline()->filenames.matrixname,     "omx");
  addRow("GO/PO output (image)", myparent->myBeamline()->filenames.imageraysname,  "out");
  addRow("GO input (source)",    myparent->myBeamline()->filenames.sourceraysname, "inp");
  //  addRow("minuit input",         myparent->myBeamline()->filenames.minname,        "minu");
  addRow("optimization results", myparent->myBeamline()->filenames.opresname,      "opti");
  addRow("optimization input",   myparent->myBeamline()->filenames.optipckname,    "pcko");
} // fillList

// helper function
// add a row on top
void ConfigWindow::addRow(const char *desc, const char *fname, const char *ext)
{
  mymodel->insertRow(0);
  mymodel->setData(mymodel->index(0, 0), desc);
  mymodel->setData(mymodel->index(0, 1), fname);
  mymodel->setData(mymodel->index(0, 2), ext);
} // addRow

// updates the widget
// add all files to be configured with description
void ConfigWindow::updateList()
{
  mymodel->setData(mymodel->index(0,  1), myparent->myBeamline()->filenames.optipckname);
  mymodel->setData(mymodel->index(1,  1), myparent->myBeamline()->filenames.opresname);
  //mymodel->setData(mymodel->index(2,  1), myparent->myBeamline()->filenames.minname);
  mymodel->setData(mymodel->index(2,  1), myparent->myBeamline()->filenames.sourceraysname);
  mymodel->setData(mymodel->index(3,  1), myparent->myBeamline()->filenames.imageraysname);
  mymodel->setData(mymodel->index(4,  1), myparent->myBeamline()->filenames.matrixname);
  mymodel->setData(mymodel->index(5,  1), myparent->myBeamline()->filenames.mapname);
  mymodel->setData(mymodel->index(6,  1), myparent->myBeamline()->filenames.so4_fsource4a);
  mymodel->setData(mymodel->index(7,  1), myparent->myBeamline()->filenames.so4_fsource4b);
  mymodel->setData(mymodel->index(8,  1), myparent->myBeamline()->filenames.so4_fsource4c);
  mymodel->setData(mymodel->index(9,  1), myparent->myBeamline()->filenames.so4_fsource4d);
  mymodel->setData(mymodel->index(10, 1), myparent->myBeamline()->filenames.so6_fsource6);
  mymodel->setData(mymodel->index(11, 1), myparent->myBeamline()->filenames.so7_hdf5);
  mymodel->setData(mymodel->index(12, 1), myparent->myBeamline()->filenames.hdf5_out);
  mymodel->setData(mymodel->index(13, 1), myparent->myBeamline()->filenames.h5surfacename);
} // updateList

void ConfigWindow::checkFileNames()
{
  int i, hits;
  char name[MaxPathLength], extension[10], *ret;
  QStandardItem *fna, *ext;

  hits= 0;
  for (i= 0; i < 13; i++ )
    {
      fna = mymodel->item(i, 1);
      ext = mymodel->item(i, 2);

      strncpy(name,      fna->text().toLocal8Bit().data(), MaxPathLength- 1);
      strncpy(extension, ext->text().toLocal8Bit().data(), 9);
      name[MaxPathLength- 1]= '\0';  // ensure termination
      extension[9]= '\0';            // ensure termination

      ret= strstr(name, extension);

      if (ret == NULL) ++hits;
#ifdef DEBUG
      else
	cout << "ret " << ret << " " << name << " " << extension << endl;
#endif
   }
  cout << "hits " << hits << endl;

  if (hits > 0)
    {
      QMessageBox *msgBox = new QMessageBox;
      msgBox->setText(tr("<b>Warning</b>"));
      msgBox->setInformativeText(tr("Found %1 file(s) without default extension!").arg(hits));
      msgBox->setStandardButtons(QMessageBox::Ignore | QMessageBox::Help);
      msgBox->setIcon(QMessageBox::Warning);
      
      int ret = msgBox->exec();
      if (ret == QMessageBox::Help) 
	QMessageBox::information(this, tr("Help"),
				 tr("Some of your filenames (the extensions) differ from the recommended defaults."
				    "This is no problem in principle but there is the risk that you overwrite your "
				    "datafiles by accident. In case of temporary output files <b>phase</b> overwrites "
				    "them without always checking if they exist, i.e. mixing input and output files "
				    "can cause data loss. You can press <i>Ignore</i> if you know what you are doing- "
				    "otherwise it is recommended to go back and press <i>Defaults</i> or choose filenames "
				    "with default extensions.")
				 ); 
    }
} // end checkFileNames
// end
