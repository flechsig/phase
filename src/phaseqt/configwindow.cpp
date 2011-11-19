//  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/configwindow.cpp
//  Date      : <16 Aug 11 12:20:33 flechsig> 
//  Time-stamp: <2011-11-20 00:12:03 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 

// the configure window

#include <string.h>
#include <QtGui>
#include "configwindow.h"

using namespace std;

// the constructor
ConfigWindow::ConfigWindow(PhaseQt *parent)
{
#ifdef DEBUG
  printf("debug: ConfigWindow constructor called, file: %s, line: %d\n", __FILE__,  __LINE__);
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

  connect(configApplyB, SIGNAL(clicked()), this,  SLOT(applySlot()));
  connect(configQuitB,  SIGNAL(clicked()), this,  SLOT(quitSlot()));
  connect(sourceView,   SIGNAL(clicked(QModelIndex)), this, SLOT(selectSlot(QModelIndex)));
  
  configWindowBox->show();
 } // end constructor

// slots
void ConfigWindow::applySlot()
{
#ifdef DEBUG
  cout << "debug: applySlot called" << endl;
#endif

  myparent->writeBackupFile();
} // applySlot

void ConfigWindow::quitSlot()
{
  this->configWindowBox->close();
} // quitSlot

void ConfigWindow::selectSlot(const QModelIndex &index)
{
  char *fname, description[MaxPathLength], oldname[MaxPathLength], extension[10], filter[50];

  if (!index.isValid())
    return;
   
#ifdef DEBUG 
  cout << "debug: " << __FILE__ << " row, column " << index.row() << "," << index.column() << endl;
#endif  

  /* keep the next 3 lines to show the priciple howto get one individual item
  QStandardItem *item = mymodel->itemFromIndex(index); // returns the item
  if (!item) return;
  cout << "item: " << item->text().toAscii().data() << endl; */

  QStandardItem *des = mymodel->item(index.row(), 0); // the description
  QStandardItem *fna = mymodel->item(index.row(), 1); // the filename
  QStandardItem *ext = mymodel->item(index.row(), 2); // the extension

   if (!fna || !ext || !des) 
     return;

   //    QMessageBox::warning(this, tr("item"),  item1->text());
   strncpy(description, des->text().toAscii().data(), MaxPathLength);
   strncpy(oldname,     fna->text().toAscii().data(), MaxPathLength);
   strncpy(extension,   ext->text().toAscii().data(), 10);

   description[(MaxPathLength-1)]= '\0';  // ensure termination
   oldname[(MaxPathLength-1)]= '\0';      // ensure termination
   extension[9]= '\0';                    // ensure termination
   sprintf(filter, "Files (*.%s);;(*)", extension);

#ifdef DEBUG 
  cout << "debug: " << __FILE__ << " item: " <<  oldname << endl;
#endif 
   
  QFileDialog *dialog = new QFileDialog(this);
  dialog->selectFile(oldname);
  QString fileName = dialog->getSaveFileName(this, tr("Define File Name"), 
					     QDir::currentPath(),
					     tr(filter)
					     );
  if (!fileName.isEmpty()) 
    {
      //printf("description: >>%s<<\nevaluate  \n", buffer);
      fname= fileName.toAscii().data();
      // update data
      
      if ( !strncmp(description, "optimization input", 16) ) 
	strncpy(myparent->optipckname, fname, MaxPathLength); else
	if ( !strncmp(description, "optimization results", 16) ) 
	  strncpy(myparent->opresname, fname, MaxPathLength); else
	  if ( !strncmp(description, "minuit input", 10) ) 
	    strncpy(myparent->minname, fname, MaxPathLength); else
	    if ( !strncmp(description, "ray input (source)", 6) ) 
	      strncpy(myparent->sourceraysname, fname, MaxPathLength); else
	      if ( !strncmp(description, "ray output (image)", 6) )
		strncpy(myparent->imageraysname, fname, MaxPathLength); else
		if ( !strncmp(description, "matrix", 4) ) 
		  strncpy(myparent->matrixname, fname, MaxPathLength); else
		  if ( !strncmp(description, "mapname", 4) ) 
		    strncpy(myparent->mapname, fname, MaxPathLength); else
		    if ( !strncmp(description, "so4_fsource4a", 13) )
		      { 
			strncpy(myparent->so4_fsource4a, fname, MaxPathLength);
			strncpy(myparent->myBeamline()->src.so4.fsource4a, fname, 80);
		      } else
		      if ( !strncmp(description, "so4_fsource4b", 13) ) 
			{
			  strncpy(myparent->so4_fsource4b, fname, MaxPathLength);
			  strncpy(myparent->myBeamline()->src.so4.fsource4b, fname, 80);
			} else
			if ( !strncmp(description, "so4_fsource4c", 13) )
			  { 
			    strncpy(myparent->so4_fsource4c, fname, MaxPathLength);
			    strncpy(myparent->myBeamline()->src.so4.fsource4c, fname, 80);
			  } else
			  if ( !strncmp(description, "so4_fsource4d", 13) ) 
			    {
			      strncpy(myparent->so4_fsource4d, fname, MaxPathLength);
			      strncpy(myparent->myBeamline()->src.so4.fsource4d, fname, 80);
			    } else
			    if ( !strncmp(description, "so6_fsource6", 3) ) 
			      {
				strncpy(myparent->so6_fsource6, fname, MaxPathLength); 
				strncpy(myparent->myBeamline()->src.so6.fsource6, fname, 80);
			      }
			    else
			      cout << "selectSlot: error: no matching description: >>" 
				   << description << "<<" << endl;
      
      // update widget
      mymodel->setData(mymodel->index(index.row(), 1), fname);
    }
} // end selectslot
////////////// end slots ////////////

//QAbstractItemModel *ConfigWindow::createConfigModel(QObject *parent)
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
  addRow("so6_fsource6",         myparent->myPHASEset()->so6_fsource6,   "s6");
  addRow("so4_fsource4d",        myparent->myPHASEset()->so4_fsource4d,  "s4d");
  addRow("so4_fsource4c",        myparent->myPHASEset()->so4_fsource4c,  "s4c");
  addRow("so4_fsource4b",        myparent->myPHASEset()->so4_fsource4b,  "s4b");
  addRow("so4_fsource4a",        myparent->myPHASEset()->so4_fsource4a,  "s4a");
  addRow("map name",             myparent->myPHASEset()->mapname,        "map");
  addRow("matrix name",          myparent->myPHASEset()->matrixname,     "omx");
  addRow("ray output (image)",   myparent->myPHASEset()->imageraysname,  "out");
  addRow("ray input (source)",   myparent->myPHASEset()->sourceraysname, "inp");
  addRow("minuit input",         myparent->myPHASEset()->minname,        "minu");
  addRow("optimization results", myparent->myPHASEset()->opresname,      "opti");
  addRow("optimization input",   myparent->myPHASEset()->optipckname,    "pcko");
} // fillList

// add a row on top
void ConfigWindow::addRow(const char *desc, const char *fname, const char *ext)
{
  mymodel->insertRow(0);
  mymodel->setData(mymodel->index(0, 0), desc);
  mymodel->setData(mymodel->index(0, 1), fname);
  mymodel->setData(mymodel->index(0, 2), ext);
} // addRow

// add all files to be configured with description
void ConfigWindow::updateList()
{
  mymodel->setData(mymodel->index(0, 1), myparent->myPHASEset()->optipckname);
  mymodel->setData(mymodel->index(1, 1), myparent->myPHASEset()->opresname);
  mymodel->setData(mymodel->index(2, 1), myparent->myPHASEset()->minname);
  mymodel->setData(mymodel->index(3, 1), myparent->myPHASEset()->sourceraysname);
  mymodel->setData(mymodel->index(4, 1), myparent->myPHASEset()->imageraysname);
  mymodel->setData(mymodel->index(5, 1), myparent->myPHASEset()->matrixname);
  mymodel->setData(mymodel->index(6, 1), myparent->myPHASEset()->mapname);
  mymodel->setData(mymodel->index(7, 1), myparent->myPHASEset()->so4_fsource4a);
  mymodel->setData(mymodel->index(8, 1), myparent->myPHASEset()->so4_fsource4b);
  mymodel->setData(mymodel->index(9, 1), myparent->myPHASEset()->so4_fsource4c);
  mymodel->setData(mymodel->index(10, 1), myparent->myPHASEset()->so4_fsource4d);
  mymodel->setData(mymodel->index(11, 1), myparent->myPHASEset()->so6_fsource6);
} // updateList
// end
