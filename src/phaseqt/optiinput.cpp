//  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/optiinput.cpp
//  Date      : <29 Jul 11 13:55:53 flechsig> 
//  Time-stamp: <28 Aug 14 16:39:30 flechsig> 
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

//
// the optimization input box
//
#if (QT_VERSION < 0x050000)
#include <QtGui>
#else
#include <QtWidgets>
#endif

#include "optiinput.h"

using namespace std;

// constructor
//OptiInput::OptiInput(struct )
OptiInput::OptiInput(struct ElementType *list, unsigned int elementnumber, 
		     char *beamlinename, char *optipckname, char *opresname)
{
  printf("OptiInput constructor called\n");
  optiInputBox = new QWidget();
  optiInputBox->setWindowTitle(tr("Optimization Input"));
  QGroupBox   *filesGroup   = new QGroupBox(tr("Files"));
  QVBoxLayout *filesLayout  = new QVBoxLayout;

  outputfileLabel= new QLabel(tr("Optimization output file: opti_out.dat"));
  
  filesLayout->addWidget(outputfileLabel); 
  
  QGroupBox   *targetGroup  = new QGroupBox(tr("Target"));
  QHBoxLayout *targetLayout = new QHBoxLayout;

  QPushButton *optTargetButton = new QPushButton(tr("&optimization target"));
  optTargetMenu = new QMenu(this);
  focusAct   = new QAction(tr("&Focus"), this);
  focusvAct  = new QAction(tr("Focus &vertical"), this);
  focushAct  = new QAction(tr("Focus &horizontal"), this);
  focusSAct  = new QAction(tr("Focus &special"), this);
  transAct   = new QAction(tr("&Transmittance"), this);
  costAct    = new QAction(tr("use &cost.F"), this);
  rpowervAct = new QAction(tr("Res. power vert."), this);
  rpowerhAct = new QAction(tr("Res. power hor."), this);
 
  optTargetMenu->addAction(focusAct);
  optTargetMenu->addAction(focusvAct);
  optTargetMenu->addAction(focushAct);
  optTargetMenu->addAction(focusSAct);
  optTargetMenu->addSeparator();
  optTargetMenu->addAction(transAct);
  optTargetMenu->addAction(costAct);
  optTargetMenu->addSeparator();
  optTargetMenu->addAction(rpowervAct);
  optTargetMenu->addAction(rpowerhAct);
  optTargetButton->setMenu(optTargetMenu);

  targetLabel= new QLabel(tr("selected"));

  targetLayout->addWidget(optTargetButton);
  targetLayout->addWidget(targetLabel);
  targetGroup->setLayout(targetLayout);
 
  //filesLayout->addWidget(targetGroup);
  filesGroup->setLayout(filesLayout);

  QGroupBox *elementGroup  = new QGroupBox(tr("Elements"));
  QHBoxLayout *elementLayout = new QHBoxLayout;
  optielementList = new QListWidget();
  optielementList->setAlternatingRowColors(true);
  optiUpdateB   = new QPushButton(tr("&Update"));
  elementLayout->addWidget(optielementList);
  elementLayout->addWidget(optiUpdateB);
  elementGroup->setLayout(elementLayout);

  parameterList = new QListWidget();
  parameterList->setAlternatingRowColors(true);
  parameterList->addItems(QStringList()
			<< "theta (deg.)"
			<< "constant length (mm), r1 variable"
			<< "constant length (mm), r2 variable"
			<< "photon energy (eV), use either energy or lambda!"
			<< "dist. to prec. element r1 (mm)"
			<< "dist. to succ. element r2 (mm)"
			<< "line density xdens[0] (1/mm)"
                        << "VLS xdens[1]"
			  << "VLS xdens[2]"
			  << "VLS xdens[3]"
			  << "VLS xdens[4]"
			  << "lambda (nm), use either energy or lambda!"
			  << "theta (deg), BESSY I, CLRCM, dy= 43 mm"
			  << "CL- r1 constant"
			  << "SGM Vodar mode"
			  << "fixfocus constant cff"
			  << "energy at cff == constant"
			  << "CL- r2 constant"
			  << "STXM special: M1- S1"
			  << "STXM special: S1- S2"
			  << "a(i,j): index= index+ i + j * 9, i,j=0..8"
			  << "radius r (mm)"
			  << "radius rho (mm)"
			  << "aperture 2 w (mm)"
			  << "aperture 2 l (mm)"
			  << "slope w (arcsec)"
			  << "slope l (arcsec)"
			  << "STXM slits (grating, S1, S2)"
			);
  selectionLabel  = new QLabel(tr("selection"));
  QGroupBox *parameterGroup  = new QGroupBox(tr("Optimization Parameters"));
  QVBoxLayout *parameterLayout = new QVBoxLayout;
  parameterLayout->addWidget(elementGroup);
  parameterLayout->addWidget(parameterList); 
  parameterLayout->addWidget(selectionLabel); 
  parameterGroup->setLayout(parameterLayout);
  QGroupBox *inputGroup  = new QGroupBox(tr("Inputs"));
  QGroupBox *iButtonGroup  = new QGroupBox();
  QVBoxLayout *iButtonLayout = new QVBoxLayout;
  QHBoxLayout *inputLayout = new QHBoxLayout;
  inputList = new QListWidget();
  plusButton= new QPushButton(tr("Append"));
  minusButton= new QPushButton(tr("Delete"));
  iButtonLayout->addWidget(plusButton);
  iButtonLayout->addWidget(minusButton);
  iButtonGroup->setLayout(iButtonLayout);
  inputLayout->addWidget(inputList);
  inputLayout->addWidget(iButtonGroup);
  //inputLayout->addWidget(templateLabel);
  //inputLayout->addWidget(inputE);
  inputGroup->setLayout(inputLayout);

  inputList->setAlternatingRowColors(true);
  inputList->addItems(QStringList()
			<< "x : 0 1 0.0"
			<< "y : 0 1 0.0"
	);
  templateLabel  = new QLabel(tr("template: x : index nx dx"));
  inputE = new QLineEdit;		

  QGroupBox *lowerButtonGroup  = new QGroupBox(tr(""));
  QHBoxLayout *lowerButtonLayout = new QHBoxLayout;
  optiApplyB   = new QPushButton(tr("&Apply"));
  optiQuitB    = new QPushButton(tr("&Quit"));
  lowerButtonLayout->addWidget(optiApplyB);
  lowerButtonLayout->addWidget(optiQuitB);
  lowerButtonGroup->setLayout(lowerButtonLayout);

  QVBoxLayout *vbox = new QVBoxLayout;
  //  vbox->addWidget(filesGroup);
  vbox->addWidget(targetGroup);
  //vbox->addWidget(elementGroup);
  
  vbox->addWidget(parameterGroup);
  vbox->addWidget(inputGroup);
  //vbox->addWidget(inputList);
  vbox->addWidget(templateLabel);
  vbox->addWidget(inputE);
  vbox->addWidget(lowerButtonGroup);
  optiInputBox->setLayout(vbox);

  connect(focusAct,   SIGNAL(triggered()), this, SLOT(focusActSlot()));
  connect(focusvAct,  SIGNAL(triggered()), this, SLOT(focusvActSlot()));
  connect(focushAct,  SIGNAL(triggered()), this, SLOT(focushActSlot()));
  connect(focusSAct,  SIGNAL(triggered()), this, SLOT(focusSActSlot()));
  connect(transAct,   SIGNAL(triggered()), this, SLOT(transActSlot()));
  connect(costAct,    SIGNAL(triggered()), this, SLOT(costActSlot()));
  connect(rpowervAct, SIGNAL(triggered()), this, SLOT(rpowervActSlot()));
  connect(rpowerhAct, SIGNAL(triggered()), this, SLOT(rpowerhActSlot()));
  
  connect(optiUpdateB,  SIGNAL(clicked()), this,  SLOT(updateSlot()));
  connect(optiApplyB,   SIGNAL(clicked()), this,  SLOT(applySlot()));
  connect(optiQuitB,    SIGNAL(clicked()), this,  SLOT(quitSlot()));
  connect(optielementList, SIGNAL(itemSelectionChanged()), this, SLOT(selectElementSlot()));
  connect(optielementList, SIGNAL(itemSelectionChanged()), this, SLOT(calcIndexSlot()));
  connect(parameterList,   SIGNAL(itemSelectionChanged()), this, SLOT(selectParameterSlot()));
  connect(parameterList,   SIGNAL(itemSelectionChanged()), this, SLOT(calcIndexSlot()));
  connect(inputList,       SIGNAL(itemSelectionChanged()), this, SLOT(selectInputSlot()));

  connect(plusButton,       SIGNAL(clicked()), this, SLOT(appendLine()));
  connect(minusButton,      SIGNAL(clicked()), this, SLOT(deleteLine()));
  connect(inputE, SIGNAL(returnPressed()), this, SLOT(inputUpdateSlot()));
  mylist= list;
  myelementnumber= elementnumber;
  myoptipckname  = optipckname;
  
  mybeamlinename= beamlinename;
  myopresname= opresname;

  fillElementList();
  fillInputs();
  optiInputBox->show();
  
  printf("OptiInput: constructor finished\n");
}

// slots
void OptiInput::appendLine()
{
  printf("slot appendLine called\n");
  QListWidgetItem *item= new QListWidgetItem("pindex 'name' p0 dp pmin pmax");
  inputList->insertItem(inputList->count(), item);
}

void OptiInput::applySlot()
{
  FILE *oppickfile, *minfile;
  char buffer[MaxPathLength];
  char *subzeile;
  int i, k, index, methode, parameterzahl, version= 20121102; //version= 20110729;
  
  printf("applySlot called\n");

  char *text= targetLabel->text().toLatin1().data();
  if (strcmp(text, "use cost.F") == 0) methode= OptiCost; else
    if (strcmp(text, "Focus special") == 0) methode= OptiFocus; else
      if (strcmp(text, "Transmittance") == 0) methode= OptiTrans; else
	if (strcmp(text, "Focus vertical") == 0) methode= OptiY; else
	  if (strcmp(text, "Focus horizontal") == 0) methode= OptiZ; else
	    if (strcmp(text, "Resolving power vertical") == 0) methode= OptiRpY; else
	      if (strcmp(text, "Resolving power horizontal)") == 0) methode= OptiRpZ; else
		if (strcmp(text, "Focus (round)") == 0) methode= OptiR; else methode= -1;

  printf("debug: text: %s =. methode: %d\n", text, methode);
  
  // open files
  if ((oppickfile= fopen(this->myoptipckname, "w+")) == NULL)
    {
      fprintf(stderr, "error: open file >>%s<<\n", this->myoptipckname); 
      exit(-1);   
    }    
  
  fprintf(oppickfile, "%s %d\n", OptiPickFileHeader, version);
  fprintf(oppickfile, "%d    Optimization methode\n", methode);
  fprintf(oppickfile, "%s    beamline filename (input)\n",     this->mybeamlinename);
  fprintf(oppickfile, "%s    optimization results (output)\n", this->myopresname);
  fprintf(oppickfile, "# scan parameter\n");
  fprintf(oppickfile, "# axis : index number delta\n");

  for (i= 0; i < 2; i++)  
    {
      strncpy(buffer, inputList->item(i)->text().toLatin1().data(), MaxPathLength);
      fprintf(oppickfile, "%s\n", buffer);
    }
  
  parameterzahl= inputList->count()- 2;
  fprintf(oppickfile, "%d   parameter number\n", parameterzahl);
  fprintf(oppickfile, "# parameter list\n");
  fprintf(oppickfile, "# index 'name' start step [min] [max]\n");
  for (i= 0; i < parameterzahl; i++)  
    {
      inputList->setCurrentRow(i+2);
      strncpy(buffer, inputList->currentItem()->text().toLatin1().data(), MaxPathLength);
      fprintf(oppickfile, "%s\n", buffer);  
    }
  fprintf(oppickfile, "# end\n");
  fclose(oppickfile); 
 
  printf("GetOptiBox: wrote file:\n  %s\n", this->myoptipckname);
} // end applySlot


void OptiInput::calcIndexSlot()
{
  int index;
  char buffer[MaxPathLength];

  printf("calcIndexSlot\n");
  int elpos = optielementList->currentRow();
  int parpos= parameterList->currentRow();
  
  if (elpos  >= 0) elpos++;
  if (parpos >= 0) parpos++;
  index= iindex(elpos, parpos);
  snprintf(buffer, MaxPathLength, "selection: (%d, %d) ==> index: %d", elpos, parpos, index);
  selectionLabel->setText(tr(buffer));
} // calcIndexSlot()

//
// UF slot delete line
//
void OptiInput::deleteLine()
{
  printf("slot deleteline called\n");
  int pos= inputList->currentRow();
  QListWidgetItem *item;

  if (pos > 1)
    {
      item= inputList->takeItem(pos);
      if (item) 
	delete item;
      else
	printf("item unvalid or delete not allowed (first two lines) \n");
    }
#ifdef xxx
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
  UpdateStatus();
  writeBackupFile();
#endif
} // deleteLine()

// popup menu
void OptiInput::focusActSlot()
{
  //  printf("focusActSlot called\n");
  targetLabel->setText(QString(tr("Focus (round)")));
} 

void OptiInput::focushActSlot()
{
  //  printf("focusActSlot called\n");
  targetLabel->setText(QString(tr("Focus horizontal")));
} 

void OptiInput::focusSActSlot()
{
  //  printf("focusActSlot called\n");
  targetLabel->setText(QString(tr("Focus special")));
} 

void OptiInput::focusvActSlot()
{
  targetLabel->setText(QString(tr("Focus vertical")));
} 

void OptiInput::transActSlot()
{
  targetLabel->setText(QString(tr("Transmittance")));
} 

void OptiInput::costActSlot()
{
  targetLabel->setText(QString(tr("use cost.F")));
} 

void OptiInput::rpowervActSlot()
{
  targetLabel->setText(QString(tr("Resolving power vertical")));
} 

void OptiInput::rpowerhActSlot()
{
  targetLabel->setText(QString(tr("Resolving power horizontal")));
} 

void OptiInput::inputUpdateSlot()
{
  int i;
  char buffer[MaxPathLength];
  int pos= inputList->currentRow();
  QListWidgetItem *item= inputList->item(pos);

  printf("inputUpdateSlot\n");
  if (pos < 0) 
    return;
  i= pos - 1;
  switch (pos)
    {
    case 0: 
      snprintf(buffer, MaxPathLength, "x : %s", inputE->text().toLatin1().data());
      break;
    case 1: 
      snprintf(buffer, MaxPathLength, "y : %s", inputE->text().toLatin1().data());
      break;
    default:
      snprintf(buffer, MaxPathLength, "%s", inputE->text().toLatin1().data());
    };
  item->setText(buffer);
  inputList->clearSelection();
  inputList->clearFocus();
} // inputUpdateSlot


void OptiInput::quitSlot()
{
  this->optiInputBox->close();
} // quitSlot

void OptiInput::selectElementSlot()
{
  int parameternumber= optielementList->currentRow();
  printf("elementSlot\n");
  if (parameternumber < 0) 
    return;
  printf("selectElementSlot: parameter: %d\n", parameternumber);
} // selectElementSlot() 

void OptiInput::selectInputSlot()
{
  char buffer[MaxPathLength];
  int parameternumber= inputList->currentRow();
  printf("selectinputSlot\n");
  if (parameternumber < 0) 
    return;
  strncpy(buffer, inputList->currentItem()->text().toLatin1().data(), MaxPathLength);
  if (buffer != NULL) inputE->setText(buffer);
  
  switch (parameternumber)
    {
    case 0:  templateLabel->setText(QString(tr("template => x: index nx dx"))); break;
    case 1:  templateLabel->setText(QString(tr("template => y: index ny dy"))); break;
    default: 
      templateLabel->setText(QString(tr("template => pindex 'name' p0 dp [pmin] [pmax]"))); 
      inputE->setText(buffer);
    }
} // selectInputSlot()

void OptiInput::selectParameterSlot()
{
  int parameternumber= parameterList->currentRow();
  printf("parameterSlot\n");
  if (parameternumber < 0) 
    return;
  printf("selectparameterSlot: parameter: %d\n", parameternumber);
} // selectParameterSlot() 


void OptiInput::updateSlot()
{
  printf("updateSlot- updates the elementlist\n");
  fillElementList();
} // end updateSlot

void OptiInput::fillElementList()
{
  unsigned int ui;
  struct ElementType *list;
  
  printf("fillElementList %d\n", optielementList->count() );
  // loesche alles
  while (optielementList->count()) 
    delete optielementList->takeItem(0);
  
  printf("fillElementList xxxx\n");
  
  list= mylist;
  
  cout << "fillElementList with " << myelementnumber << " elements" << endl;
  
  for (ui= 0; ui < myelementnumber; ui++, list++)
    {
      QListWidgetItem *item= new QListWidgetItem(QString(list->elementname));
      item->setFlags (item->flags () | Qt::ItemIsEditable); 
      optielementList->addItem(item);
    }
  
} // end fillElementList


void OptiInput::fillInputs()
{
  struct optistruct os;
  char buffer[MaxPathLength];
  QListWidgetItem *item;
  int i, no, version;
  
  cout << __FILE__ << " fillInputs called- read file: " << this->myoptipckname << endl; 

  getoptipickfile(&os, this->myoptipckname);

  switch (os.methode)   /* set history */
    {
    case OptiY:     focusvActSlot();  break;
    case OptiZ:     focushActSlot();  break;
    case OptiRpY:   rpowervActSlot(); break;
    case OptiRpZ:   rpowerhActSlot(); break;
    case OptiFocus: focusSActSlot();  break;
    case OptiTrans: transActSlot();   break;
    case OptiCost:  costActSlot();    break;
    case OptiR:     focusActSlot();   break;
    default:        focusActSlot();   break;
    }

  item= inputList->item(0);
  snprintf(buffer, MaxPathLength, "x : %d %d %g", os.xindex, os.xpoints, os.dx);
  item->setText(buffer);
  item= inputList->item(1);
  snprintf(buffer, MaxPathLength, "y : %d %d %g", os.yindex, os.ypoints, os.dy);
  item->setText(buffer);

  // clear the rest of the list
 
  while (inputList->count() > 2)
    {
      item= inputList->takeItem(inputList->count()-1);
      if (item) 
	delete item;
    }

  for (i= 0; i < os.npars; i++)
    {
      item= new QListWidgetItem("pindex 'name' p0 dp pmin pmax");
      if (fabs(os.min[i]- os.max[i]) < ZERO) 
	snprintf(buffer, MaxPathLength, "%d %s %g %g", os.parindex[i], &os.parnames[i * 50], os.start[i], os.step[i]);
      else
	snprintf(buffer, MaxPathLength, "%d %s %g %g", os.parindex[i], &os.parnames[i * 50], os.start[i], os.step[i], os.min[i], os.max[i]);

      item->setText(buffer);
      inputList->insertItem(inputList->count(), item);
    }

  // clean up
  XFREE(os.start);
  XFREE(os.step);
  XFREE(os.min);
  XFREE(os.max);
  XFREE(os.parindex);
  XFREE(os.parnames);
  
} // fillInputs



// end
