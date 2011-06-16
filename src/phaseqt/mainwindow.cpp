//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/mainwindow.cpp
//  Date      : <31 May 11 17:02:14 flechsig> 
//  Time-stamp: <16 Jun 11 18:45:49 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 

// this file contains the QT gui
// the widgets and slots


#include <QtGui>

#include "mainwindow.h"
#include "qtphase.h"

// the constructor??
MainWindow::MainWindow()
{
  //  textEdit = new QTextEdit;
  //  setCentralWidget(textEdit);
  graphicBox= createGraphicBox();
  
  setCentralWidget(graphicBox);
  createActions();
  createMenus();
  createToolBars();
  createStatusBar();
  createDockWindows();
  
  setWindowTitle(tr("PHASE Qt"));
  
  //   newLetter();
  //setUnifiedTitleAndToolBarOnMac(true);
  
  myQtPhase= new QtPhase;    // constructor QtPhase
  //  this->myQtPhase=myQtPhase;
  myQtPhase->myPHASEset::init("default");
  myQtPhase->myPHASEset::print();
  myQtPhase->myBeamline::init();
} // end MainWindow


// slot called to read in a new beamline
void MainWindow::newBeamline()
{
#ifdef DEBUG
  printf("Debug: slot newBeamline activated\n");
#endif
  QString fileName = QFileDialog::getOpenFileName(this,
						  tr("Open File"), QDir::currentPath());
  char *name;
  //  int result;
  //this->QtPhase::print();

  if (!fileName.isEmpty()) 
    {
      name= fileName.toAscii().data();
      printf("MainWindow::newBeamline: try to read file: %s\n", name);
      ReadBLFile(name, this);
      UpdateElementList();
      UpdateBeamlineBox();
      UpdateSourceBox();
      // QImage image(fileName);
      //    if (image.isNull()) {
      //QMessageBox::information(this, tr("Phase"),
      //			       tr("Cannot load %1.").arg(fileName));
      //return;
      //}
    }
 }


// slot?? the main widget obsolete
void MainWindow::newLetter()
{
    textEdit->clear();

    QTextCursor cursor(textEdit->textCursor());
    cursor.movePosition(QTextCursor::Start);
    QTextFrame *topFrame = cursor.currentFrame();
    QTextFrameFormat topFrameFormat = topFrame->frameFormat();
    topFrameFormat.setPadding(16);
    topFrame->setFrameFormat(topFrameFormat);

    QTextCharFormat textFormat;
    QTextCharFormat boldFormat;
    boldFormat.setFontWeight(QFont::Bold);
    QTextCharFormat italicFormat;
    italicFormat.setFontItalic(true);

    QTextTableFormat tableFormat;
    tableFormat.setBorder(1);
    tableFormat.setCellPadding(16);
    tableFormat.setAlignment(Qt::AlignRight);
    cursor.insertTable(1, 1, tableFormat);
    cursor.insertText("The Firm", boldFormat);
    cursor.insertBlock();
    cursor.insertText("321 City Street", textFormat);
    cursor.insertBlock();
    cursor.insertText("Industry Park");
    cursor.insertBlock();
    cursor.insertText("Some Country");
    cursor.setPosition(topFrame->lastPosition());
    cursor.insertText(QDate::currentDate().toString("d MMMM yyyy"), textFormat);
    cursor.insertBlock();
    cursor.insertBlock();
    cursor.insertText("Dear ", textFormat);
    cursor.insertText("NAME", italicFormat);
    cursor.insertText(",", textFormat);
    for (int i = 0; i < 3; ++i)
        cursor.insertBlock();
    cursor.insertText(tr("Yours sincerely,"), textFormat);
    for (int i = 0; i < 3; ++i)
        cursor.insertBlock();
    cursor.insertText("The Boss", textFormat);
    cursor.insertBlock();
    cursor.insertText("ADDRESS", italicFormat);
}

// slot
void MainWindow::print()
{
#ifndef QT_NO_PRINTDIALOG
    QTextDocument *document = textEdit->document();
    QPrinter printer;

    QPrintDialog *dlg = new QPrintDialog(&printer, this);
    if (dlg->exec() != QDialog::Accepted)
        return;

    document->print(&printer);

    statusBar()->showMessage(tr("Ready"), 2000);
#endif
}

// slot
void MainWindow::save()
{
    QString fileName = QFileDialog::getSaveFileName(this,
                        tr("Choose a file name"), ".",
                        tr("HTML (*.html *.htm)"));
    if (fileName.isEmpty())
        return;
    QFile file(fileName);
    if (!file.open(QFile::WriteOnly | QFile::Text)) {
        QMessageBox::warning(this, tr("PHASE Qt"),
                             tr("Cannot write file %1:\n%2.")
                             .arg(fileName)
                             .arg(file.errorString()));
        return;
    }

    QTextStream out(&file);
    QApplication::setOverrideCursor(Qt::WaitCursor);
    out << textEdit->toHtml();
    QApplication::restoreOverrideCursor();

    statusBar()->showMessage(tr("Saved '%1'").arg(fileName), 2000);
}

// slot
void MainWindow::undo()
{
    QTextDocument *document = textEdit->document();
    document->undo();
}

// UF slot insert a new optical element in the beamline box
void MainWindow::insertElement()
{
  QListWidgetItem *item= new QListWidgetItem("New Element");
  elementList->insertItem(elementList->currentRow(), item);
  item->setFlags (item->flags () | Qt::ItemIsEditable);               // edit item
} 

// UF slot delete optical element in the beamline box
void MainWindow::deleteElement()
{
  delete elementList->takeItem(elementList->currentRow());
} 

// UF slot delete optical element in the beamline box
void MainWindow::selectElement()
{
  QListWidgetItem *item;
  int elementnumber;
  char *text;
  
  item= elementList->currentItem();
  text= item->text().toAscii().data();
  elementnumber= elementList->currentRow();
  groupBox1->setTitle(item->text());  // set text header
 
  printf("elementindex: %d, %s\n", elementnumber, text);
  UpdateElementBox(elementnumber);
} 

// slot
// insert customer from list into letter
void MainWindow::insertCustomer(const QString &customer)
{
    if (customer.isEmpty())
        return;
    QStringList customerList = customer.split(", ");
    QTextDocument *document = textEdit->document();
    QTextCursor cursor = document->find("NAME");
    if (!cursor.isNull()) {
        cursor.beginEditBlock();
        cursor.insertText(customerList.at(0));
        QTextCursor oldcursor = cursor;
        cursor = document->find("ADDRESS");
        if (!cursor.isNull()) {
            for (int i = 1; i < customerList.size(); ++i) {
                cursor.insertBlock();
                cursor.insertText(customerList.at(i));
            }
            cursor.endEditBlock();
        }
        else
            oldcursor.endEditBlock();
    }
}

// slot
// insert standard text from list into letter
void MainWindow::addParagraph(const QString &paragraph)
{
    if (paragraph.isEmpty())
        return;
    QTextDocument *document = textEdit->document();
    QTextCursor cursor = document->find(tr("Yours sincerely,"));
    if (cursor.isNull())
        return;
    cursor.beginEditBlock();
    cursor.movePosition(QTextCursor::PreviousBlock, QTextCursor::MoveAnchor, 2);
    cursor.insertBlock();
    cursor.insertText(paragraph);
    cursor.insertBlock();
    cursor.endEditBlock();

}

// slot
void MainWindow::about()
{
   QMessageBox::about(this, tr("About PHASE Qt"),
            tr("The <b>PHASE Qt</b> example demonstrates how to "
               "use Qt's dock widgets. You can enter your own text, "
               "click a customer to add a customer name and "
               "address, and click standard paragraphs to add them."));
}


// UF slot
// here we call our own code dependign on which button has been pressed
void MainWindow::activateProc(const QString &action)

{
  if (action.isEmpty())
          return;
  
  if (!action.compare("raytracesimpleAct")) printf("raytracesimpleAct button  pressed\n"); 
  if (!action.compare("raytracefullAct"))   printf("raytracefullAct button pressed\n"); 
  if (!action.compare("footprintAct"))      printf("footprintAct button pressed\n"); 
  if (!action.compare("phasespaceAct"))     printf("phasespaceAct button pressed\n"); 
  if (!action.compare("mphasespaceAct"))    printf("mphasespaceAct button pressed\n"); 
  
} // end activateProc

///////////////////////////////////////////////////////////////////////////////////
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

    //    rthardedgeAct = new QAction();
    //    signalMapper->setMapping(rthardedgeAct, QString("rthardedgeAct"));
    //    connect(mphasespaceAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    // finaly connect mapper to acticateProc
    connect(signalMapper, SIGNAL(mapped(QString)), this, SLOT(activateProc(QString)));
}

// create menus with buttons
void MainWindow::createMenus()
{
    fileMenu = menuBar()->addMenu(tr("&File"));
    fileMenu->addAction(newLetterAct);
    fileMenu->addAction(saveAct);
    fileMenu->addAction(printAct);
    fileMenu->addSeparator();
    fileMenu->addAction(quitAct);

    editMenu = menuBar()->addMenu(tr("&Edit"));
    editMenu->addAction(undoAct);

    calcMenu = menuBar()->addMenu(tr("&Calc"));
    calcMenu->addAction(raytracesimpleAct);
    calcMenu->addAction(raytracefullAct);
    calcMenu->addAction(footprintAct);
    calcMenu->addSeparator();
    calcMenu->addAction(phasespaceAct);
    calcMenu->addAction(mphasespaceAct);

    viewMenu = menuBar()->addMenu(tr("&View"));

    menuBar()->addSeparator();

    helpMenu = menuBar()->addMenu(tr("&Help"));
    helpMenu->addAction(aboutAct);
    helpMenu->addAction(aboutQtAct);
}


// toolbar with icons
void MainWindow::createToolBars()
{
    fileToolBar = addToolBar(tr("File"));
    fileToolBar->addAction(newLetterAct);
    fileToolBar->addAction(saveAct);
    fileToolBar->addAction(printAct);

    editToolBar = addToolBar(tr("Edit"));
    editToolBar->addAction(undoAct);
}

// statusbar at the bottom
void MainWindow::createStatusBar()
{
    statusBar()->showMessage(tr("Ready"));
}

// dock widgets
void MainWindow::createDockWindows()
{
  /*    QDockWidget *dock = new QDockWidget(tr("Customers"), this);
    dock->setAllowedAreas(Qt::LeftDockWidgetArea | Qt::RightDockWidgetArea);
    customerList = new QListWidget(dock);
    customerList->addItems(QStringList()
            << "John Doe, Harmony Enterprises, 12 Lakeside, Ambleton"
            << "Jane Doe, Memorabilia, 23 Watersedge, Beaton"
            << "Tammy Shea, Tiblanka, 38 Sea Views, Carlton"
            << "Tim Sheen, Caraba Gifts, 48 Ocean Way, Deal"
            << "Sol Harvey, Chicos Coffee, 53 New Springs, Eccleston"
            << "Sally Hobart, Tiroli Tea, 67 Long River, Fedula");
      dock->setWidget(customerList);
       addDockWidget(Qt::RightDockWidgetArea, dock);
       viewMenu->addAction(dock->toggleViewAction());

 

    dock = new QDockWidget(tr("Paragraphs"), this);
    paragraphsList = new QListWidget(dock);
    paragraphsList->addItems(QStringList()
            << "Thank you for your payment which we have received today."
			     << "You made an overpayment (more than $5). Do you wish to "
               "buy more items, or should we return the excess to you?");
        dock->setWidget(paragraphsList);
       addDockWidget(Qt::RightDockWidgetArea, dock);
       viewMenu->addAction(dock->toggleViewAction());
  */
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


    //  connect(customerList, SIGNAL(currentTextChanged(QString)),
    //     this, SLOT(insertCustomer(QString)));
    //connect(paragraphsList, SIGNAL(currentTextChanged(QString)),
    //        this, SLOT(addParagraph(QString)));
}

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
  
  QHBoxLayout *orientationLayout = new QHBoxLayout;
  orientationLayout->addWidget(rup1);
  orientationLayout->addWidget(rleft2);
  orientationLayout->addWidget(rdown3);
  orientationLayout->addWidget(rright4);
  orientationBox->setLayout(orientationLayout);

  // popup button
  QPushButton *shapeButton = new QPushButton(tr("&Shape"));
  QMenu *shapeMenu = new QMenu(this);
  shapeMenu->addAction(tr("&flat mirror"));
  shapeMenu->addAction(tr("&toroidal mirror"));
  shapeMenu->addAction(tr("&plane- elliptical mirror"));
  shapeMenu->addAction(tr("&elliptical mirror"));
  shapeMenu->addAction(tr("&conical mirror"));
  shapeMenu->addSeparator();
  shapeMenu->addAction(tr("&plane grating"));
  shapeButton->setMenu(shapeMenu);

  shapeLabel = new QLabel(tr("flat mirror")); // return value of menu

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

  //  QPushButton *thetaB = new QPushButton(tr("next"));
  QPushButton *thetaB  = new QPushButton(QIcon(":/images/Blue-arrow-right-32.png"), tr("calc"), this);
  QPushButton *sourceB = new QPushButton(QIcon(":/images/Blue-arrow-right-32.png"), tr("calc"), this);
  QPushButton *imageB  = new QPushButton(QIcon(":/images/Blue-arrow-right-32.png"), tr("calc"), this);
  QPushButton *rB      = new QPushButton(QIcon(":/images/Blue-arrow-right-32.png"), tr("calc"), this);
  QPushButton *rhoB    = new QPushButton(QIcon(":/images/Blue-arrow-right-32.png"), tr("calc"), this);

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

  geometryGroup->setLayout(geometryLayout);
  //radius

  // grating
  gratingGroup = new QGroupBox(tr("&Grating"));
  gratingGroup->setCheckable(true);
  gratingGroup->setChecked(true);

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
}

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
  sourceMenu->addAction(tr("RT hard edge"));
  sourceMenu->addAction(tr("Dipol"));
  sourceMenu->addAction(tr("Point"));
  sourceMenu->addAction(tr("Ring"));
  sourceMenu->addSeparator();
  sourceMenu->addAction(tr("generic Undulador"));
  sourceMenu->addAction(tr("Undulator BESSY II (H)"));
  sourceMenu->addAction(tr("Undulator BESSY II (L)"));
  sourceMenu->addAction(tr("Undulator SLS - SIS"));
  sourceMenu->addAction(tr("Undulator SLS - SIM"));
  sourceMenu->addSeparator();
  sourceMenu->addAction(tr("Source from file"));
  sourceTypeButton->setMenu(sourceMenu);

  QCheckBox *sourceFileBox = new QCheckBox(tr("create Source file"));

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

  sourceParsGroup->setLayout(sourceParsLayout);

  QVBoxLayout *vbox = new QVBoxLayout;
  vbox->addWidget(sourceTypeGroup);
  vbox->addWidget(sourceParsGroup);

  sourceBox->setLayout(vbox);

 return sourceBox;
} // end source box


// beamline box
QWidget *MainWindow::createBeamlineBox()
{
  beamlineBox = new QWidget();

  // upper part
  QGroupBox   *beamlineElementGroup  = new QGroupBox(tr("optical element list"));
  QVBoxLayout *beamlineElementLayout = new QVBoxLayout;

  elementList = new QListWidget();
  elementList->addItems(QStringList()
			<< "Mirror 1"
			<< "Mirror 2"
			<< "Grating 1"
			<< "Mirror 3"
			<< "Mirror 4"
			);
  
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
} // end beamline box


// parameter box
QWidget *MainWindow::createParameterBox()
{
  parameterBox = new QWidget();

  // upper part
  QGroupBox   *parameterGroup  = new QGroupBox(tr("Parameters"));
  QVBoxLayout *parameterLayout = new QVBoxLayout;

  parameterList = new QListWidget();
  parameterList->addItems(QStringList()
			<< "Mirror 1"
			<< "Mirror 2"
			<< "Grating 1"
			<< "Mirror 3"
			<< "Mirror 4"
			<< "Mirror 4"
			<< "Mirror 4"
			<< "Mirror 4"
			<< "Mirror 4"

			);
  QLabel *parameterLabel  = new QLabel(tr("edit only the value after ':'"));
  QLineEdit *parameterE  = new QLineEdit;

  parameterLayout->addWidget(parameterList);
  parameterLayout->addWidget(parameterLabel);
  parameterLayout->addWidget(parameterE);
  parameterGroup->setLayout(parameterLayout);

  QVBoxLayout *vbox = new QVBoxLayout;
  vbox->addWidget(parameterGroup);
  
  vbox->addStretch(1);
  parameterBox->setLayout(vbox);
  return parameterBox;
} // end parameter box


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
  
  QLineEdit *zminE  = new QLineEdit;
  QLineEdit *zmaxE  = new QLineEdit;
  QLineEdit *yminE  = new QLineEdit;
  QLineEdit *ymaxE  = new QLineEdit;
  
  QPushButton *autoButton = new QPushButton(tr("A&utoscale"));
  QPushButton *applyButton = new QPushButton(tr("&Apply"));
  QPushButton *popupButton = new QPushButton(tr("&PlotStyle"));
  QMenu *menu = new QMenu(this);
  menu->addAction(tr("&footprint"));
  menu->addAction(tr("&contour"));
  popupButton->setMenu(menu);
  
  QPushButton *subjectpopupButton = new QPushButton(tr("&PlotSubject"));
  QMenu *subject = new QMenu(this);
  subject->addAction(tr("&source"));
  subject->addAction(tr("&image"));
  subjectpopupButton->setMenu(subject);
  
  graphicLayout->addWidget(zminLabel, 0, 0);
  graphicLayout->addWidget(zmaxLabel, 0, 2);
  graphicLayout->addWidget(yminLabel, 1, 0);
  graphicLayout->addWidget(ymaxLabel, 1, 2);
  graphicLayout->addWidget(zminE, 0, 1);
  graphicLayout->addWidget(zmaxE, 0, 3);
  graphicLayout->addWidget(yminE, 1, 1);
  graphicLayout->addWidget(ymaxE, 1, 3);
  graphicLayout->addWidget(autoButton, 2, 2);
  graphicLayout->addWidget(applyButton, 2, 3);
  graphicLayout->addWidget(subjectpopupButton, 2, 0);
  graphicLayout->addWidget(popupButton, 2, 1);
  //  graphicLayout->setRowStretch(1,1);
  graphicGroup->setLayout(graphicLayout);
  
  QVBoxLayout *vbox = new QVBoxLayout;
  vbox->addWidget(graphicGroup);
  
  vbox->addStretch(1);
  graphicBox->setLayout(vbox);
  return graphicBox;
} // end graphic box

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
}

void MainWindow::UpdateElementBox(int number)
{
  double cff, teta, fi;
  char TextField [26][40];            /* 26 editfelder */
  struct mdatset *md;
  struct gdatset *gd;
 
  md= &(this->ElementList[number].MDat);
  gd= &(this->ElementList[number].GDat);

  teta= fabs(gd->theta0* PI/ 180.0);
  fi  = (double)(gd->inout)* 
    asin(gd->lambda* gd->xdens[0]/ (2.0* cos(teta)));
  cff = cos(fi- teta)/ cos(fi+ teta);

  sprintf(TextField[0], "%.2f", cff);
  sprintf(TextField[1], "%.1f", gd->r);
  sprintf(TextField[2], "%.1f", gd->rp);
  sprintf(TextField[3], "%.3f", gd->theta0);
  sprintf(TextField[4], "%.1f", md->r1);   
  sprintf(TextField[5], "%.1f", md->r2);
  sprintf(TextField[6], "%.1f", md->rmi);      
  sprintf(TextField[7], "%.2f", md->rho);

  sprintf(TextField[8], "%d",   gd->inout);   // not used   	   
  sprintf(TextField[9], "%.2f", gd->xdens[0]); 
  sprintf(TextField[10],"%.2f", gd->xdens[1]);    
     
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
  
  duE->setText(QString(tr(TextField[14])));
  dwE->setText(QString(tr(TextField[15])));
  dlE->setText(QString(tr(TextField[16])));
  dRuE->setText(QString(tr(TextField[17])));
  dRwE->setText(QString(tr(TextField[18])));
  dRlE->setText(QString(tr(TextField[19])));
  w1E->setText(QString(tr(TextField[20])));
  w2E->setText(QString(tr(TextField[21])));
  wsE->setText(QString(tr(TextField[22])));
  l1E->setText(QString(tr(TextField[23])));
  l2E->setText(QString(tr(TextField[24])));
  lsE->setText(QString(tr(TextField[25])));

  if (gd->iflag) nimBox->setChecked(true); else nimBox->setChecked(false);

#ifdef DEBUG 		      
  printf("debug: UpdateElementBox: gd->azimut: %d\n", gd->azimut); 
  printf("debug: UpdateElementBox: md->Art:    %d\n", md->Art); 
#endif

  switch (gd->azimut)
    {
    case 0: rup1   ->setChecked(true); break;
    case 1: rleft2 ->setChecked(true); break;
    case 2: rdown3 ->setChecked(true); break;
    case 3: rright4->setChecked(true); break;
    }

  switch (md->Art)
    {
    case kEOETM:   
      shapeLabel->setText(QString(tr("toroidal mirror"))); 
      gratingGroup->setChecked(false);
      vlsGroup    ->setChecked(false);
      break;
    case kEOETG:
      shapeLabel->setText(QString(tr("toroidal grating")));  
      gratingGroup->setChecked(true);
      vlsGroup    ->setChecked(false);
      break;
    case kEOEVLSG: 
      shapeLabel->setText(QString(tr("toroidal VLS grating")));
      gratingGroup->setChecked(true);
      vlsGroup    ->setChecked(true);
      break;
    default: 
      QMessageBox::warning(this, tr("UpdateElementBox"),
			   tr("Shape type %1 not recognized.")
			   .arg(md->Art));
      printf("error: md->Art: %d not recognized\n", md->Art);
      break;
    }

} // end UpdateElementBox

void MainWindow::UpdateSourceBox()
{

  int sou;
  struct UndulatorSourceType  *up;
  struct UndulatorSource0Type *up0;
  struct DipolSourceType      *dp;
  struct PointSourceType      *sop;
  struct RingSourceType       *rp;
  struct HardEdgeSourceType   *hp;     
  struct SRSourceType         *sp; 
  struct PSImageType          *psip;
  struct PSSourceType         *pssp; 

  char TextField [8][40];            /* 8 editfelder */
  char LabelField[9][40]; 
   
  char  *LabelField1 [39] =  {	"", "-> points",         		/*0,1*/
				"height [mm]",     "width [mm]",   	/*2,3*/
				"v. div. [mrad]", "h. div. [mrad]", 
				"ray number", 				/* 6 */
				"length [mm]", "lambda [nm]",           /*7,8*/
				
				"yi [mm]",   "zi [mm]", 
				"dyi [rad]", "dzi [rad]",             /*11,12*/
				
				"yo [mm]", "zo [mm]",
				"dyi [mrad]", "dyo [mrad]", 
				"dzi [mrad]", "dzo [mrad]",          /*17,18*/
				
				"sigy [mm]",    "sigdyp [mrad]",     /*19,20*/
				"dymin [mrad]", "dymax [mrad]", 
				"sigz [mm]",    "sigdzp [mrad]", 
				"dzmin [mrad]", "dzmax [mrad]",      /*25,26*/
				
				"ymin [mm]", "ymax [mm]",
				"zmin [mm]", "zmax [mm]",
				"y points",  "z points", 
				
				"Delta z [mm]",                      /*33*/  
                                "sigmaez [mm]", "sigmaey [mm]",      /*34,35 */         
                                "sigmaedz [mrad]", "sigmaedy [mrad]" /*36,37 */         
    };                    
#ifdef DEBUG 
    printf("InitSourceBox: bl->RTSource.QuellTyp: %c\n", 
	   this->RTSource.QuellTyp);   
#endif   
 
    sou= this->RTSource.QuellTyp;

  switch (sou) {
 
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
} // end Updatesourcebox


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
}


// /afs/psi.ch/user/f/flechsig/phase/src/qtgui/mainwindow.cpp
