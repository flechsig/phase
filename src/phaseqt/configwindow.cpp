//  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/configwindow.cpp
//  Date      : <16 Aug 11 12:20:33 flechsig> 
//  Time-stamp: <18 Aug 11 09:19:07 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 

// the configure window

#include <QtGui>
#include "configwindow.h"

// the constructor
ConfigWindow::ConfigWindow(struct PHASEset *parent)
{
  printf("ConfigWindow constructor called\n");
  configWindowBox = new QWidget();
  configWindowBox->setWindowTitle(tr("PHASE configuration"));
  QLabel *configLabel = new QLabel(tr("configure auxiliary files"));
  fileList = new QListWidget();
  QGroupBox   *lowerButtonGroup  = new QGroupBox(tr(""));
  QHBoxLayout *lowerButtonLayout = new QHBoxLayout;
  configApplyB   = new QPushButton(tr("&Apply"));
  configQuitB    = new QPushButton(tr("&Quit"));
  lowerButtonLayout->addWidget(configApplyB);
  lowerButtonLayout->addWidget(configQuitB);
  lowerButtonGroup->setLayout(lowerButtonLayout);

  QVBoxLayout *vbox = new QVBoxLayout;
  vbox->addWidget(configLabel);
  vbox->addWidget(fileList);
  vbox->addWidget(lowerButtonGroup);
  configWindowBox->setLayout(vbox);

  connect(configApplyB, SIGNAL(clicked()), this,  SLOT(applySlot()));
  connect(configQuitB,  SIGNAL(clicked()), this,  SLOT(quitSlot()));
  connect(fileList,     SIGNAL(itemSelectionChanged()), this, SLOT(selectSlot()));
  myparent= parent;
  fillList();

  configWindowBox->show();
} // end constructor

void ConfigWindow::applySlot()
{
  printf("applySlot called\nno function so far\n");
} // applySlot

void ConfigWindow::quitSlot()
{
  this->configWindowBox->close();
} // quitSlot

// does almost everything here
void ConfigWindow::selectSlot()
{
  QListWidgetItem *item;
  int elementnumber= fileList->currentRow();
  char *text, *cp, *fname, buffer[310], oldname[MaxPathLength], extension[10], filter[50];

if (elementnumber < 0) 
    return;

 item= fileList->currentItem();
 text= item->text().toAscii().data();
 strncpy(buffer, text, 310);                 // save old entry buffer
 cp= strchr(buffer, ':');
 ++cp; 
 ++cp;                                    // cp points now to the start of the filename
 if (cp != NULL) 
   {
     strncpy(oldname, cp, MaxPathLength);
     printf("selected file: >>%s<<\n", cp);
     *cp='\0';                            // description in buffer string ends here
     ++cp;
     cp= strchr(cp, '.');
     strncpy(extension, cp, 10);
     sprintf(filter, "Files (*%s);;(*)", extension);
   }
 else 
   return;

#ifdef DEBUG
 printf("DEBUG selectSlot:\n  description: >>%s<<, oldname: >>%s<<, extension: >>%s<<\n", buffer, oldname, extension);
#endif
 
 QFileDialog *dialog = new QFileDialog(this);
 dialog->selectFile(oldname);

 QString fileName = dialog->getSaveFileName(this, tr("Define File Name"), 
						 QDir::currentPath(),
						 //"/afs/psi.ch/user/f/flechsig/phase/data",
						 tr(filter)
						  );


 if (!fileName.isEmpty()) 
   {
     printf("description: %s\n", text);
     fname= fileName.toAscii().data();
// update data
     if (strcmp(text, "optimization input") >= 0) 
       strncpy(myparent->optipckname, fname, MaxPathLength); else
       if (strcmp(text, "optimization results") >= 0) 
	 strncpy(myparent->opresname, fname, MaxPathLength); else
	 if (strcmp(text, "minuit input") >= 0) 
	   strncpy(myparent->minname, fname, MaxPathLength); else
	   if (strcmp(text, "ray input (source)") >= 0) 
	     strncpy(myparent->sourceraysname, fname, MaxPathLength); else
	     if (strcmp(text, "ray output (image)") >= 0) 
	       strncpy(myparent->imageraysname, fname, MaxPathLength); else
	       if (strcmp(text, "matrix") >= 0) 
		 strncpy(myparent->matrixname, fname, MaxPathLength); else
		 if (strcmp(text, "mapname") >= 0) 
		   strncpy(myparent->mapname, fname, MaxPathLength); 
     
     // update widget
     mkRow(buffer, buffer, fname);
     item->setText(buffer);
   }
} // selectSlot

// add all files to be configured with description
void ConfigWindow::fillList()
{

  char slist[10][310];
  // we allow 50 characters description and 20 for the filename

  mkRow(slist[0], "beamline name \t: ",        myparent->beamlinename);
  mkRow(slist[3], "optimization input \t: ",   myparent->optipckname);
  mkRow(slist[4], "optimization results \t: ", myparent->opresname);
  mkRow(slist[5], "minuit input \t: ",         myparent->minname);
  mkRow(slist[1], "ray input (source) \t: ",   myparent->sourceraysname);
  mkRow(slist[2], "ray output (image) \t: ",   myparent->imageraysname);
  mkRow(slist[6], "matrix \t: ",               myparent->matrixname);
  mkRow(slist[7], "mapname \t: ",              myparent->mapname);
  
  fileList->setAlternatingRowColors(true);
  fileList->addItems(QStringList()
		     //	     << slist[0]  // beamline name should not be changed here
		     << slist[1]
		     << slist[2]
		     << slist[3]
		     << slist[4]
		     << slist[5]
		     << slist[6]
		     << slist[7]
		     );
} // fillList

// helper function
void ConfigWindow::mkRow(char *out, char *desc, char *fname)
{
  strncpy(out, desc,  50);
  strncat(out, fname, 260);
} // mkRow

// end
