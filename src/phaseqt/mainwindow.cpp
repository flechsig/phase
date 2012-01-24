//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/mainwindow.cpp
//  Date      : <31 May 11 17:02:14 flechsig> 
//  Time-stamp: <24 Jan 12 09:08:00 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 

// this file contains the QT gui
// the widgets and slots
// we try to keep the functions in alphabetic order within each section
// 1) constructor
// 2) slots       --> moved to mainwindow_slots.cpp
// 3) widget definition
// 4) widget handling

#include <QtGui>

#include "mainwindow.h"
#include "phaseqt.h"
#include "treemodel.h"

using namespace std;

// the constructor of the main window
MainWindow::MainWindow(PhaseQt *parent)
{
  graphicBox= createGraphicBox();
  setCentralWidget(graphicBox);
  createActions();
  createMenus();
  createToolBars();
  createStatusBar();
  createDockWindows();
  setWindowTitle(tr("PHASE Qt"));
  resize(1400,940);

  //this->myPHASEset::init("default");
  //this->initSet("default");
  //  this->QtPhase::print();
  //this->myBeamline::init();                           // empty pointers
  //this->initBeamline();
  // this->hormapsloaded = 0;
  this->s_ray= NULL;
  this->o_input= NULL;
  this->c_window= NULL;
  myparent= parent;
  setAttribute(Qt::WA_DeleteOnClose);
} // end MainWindow

// destructor
MainWindow::~MainWindow()
{
  cout << "MainWindow destructor called" << endl;
  if (c_window) c_window->configWindowBox->close();
  if (s_ray)    s_ray->singleRayBox->close();
  if (o_input)  o_input->optiInputBox->close();
}

// checks the resulttype
// if correct returns 1 otherwise 0 and messagebox with diagnostics
int MainWindow::checkResultType(struct RESULTType *rp, int typ)
{
  int myret= (rp->typ == typ) ? 1 : 0;
  
  if (!myret)
    QMessageBox::warning(this, tr("checkResultType"),
			 tr("Available calculation results incompatible with selected plotsubject!<p>info: points= %1, type= %2, requested type= %3").arg(rp->points).arg(rp->typ).arg(typ));

  return myret;
} // checkResultType


/////////////////////////////////////
// begin widget definition section //
/////////////////////////////////////

/////////////////////////////////
// define action buttons
void MainWindow::createActions()
{
    newBeamlineAct = new QAction(QIcon(":/images/new.png"), tr("&New Beamline"), this);
    newBeamlineAct->setShortcuts(QKeySequence::New);
    newBeamlineAct->setStatusTip(tr("Create a new Beamline"));
    connect(newBeamlineAct, SIGNAL(triggered()), this, SLOT(newBeamline()));

    openBeamlineAct = new QAction(QIcon(":/images/new.png"), tr("&Open Beamline"), this);
    openBeamlineAct->setShortcuts(QKeySequence::New);
    openBeamlineAct->setStatusTip(tr("Open Beamline"));
    connect(openBeamlineAct, SIGNAL(triggered()), this, SLOT(openBeamline()));

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

    // UF test code 11.1.2012
#ifdef DEBUG
    printf("\nQT_VERSION: 0x%X\n\n", QT_VERSION);
#endif
#if QT_VERSION > 0x406000        
    quitAct->setShortcuts(QKeySequence::Quit);  // not available for QT_VERSION < 0x406000
#endif

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

    raytracesimpleAct = new QAction(QIcon(":/images/quickrtrace.png"),tr("GO &Quick ray tracing"), this);
    //raytracesimpleAct->setStatusTip(tr("geometrical optics, Quick ray tracing"));
    signalMapper->setMapping(raytracesimpleAct, QString("raytracesimpleAct"));
    connect(raytracesimpleAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    //  raytracefullAct = new QAction(tr("GO &Full ray tracing"), this);
    raytracefullAct = new QAction(QIcon(":/images/rtrace.png"), tr("GO &Full ray tracing"), this);
    raytracefullAct->setStatusTip(tr("geometrical optics, full ray tracing"));
    signalMapper->setMapping(raytracefullAct, QString("raytracefullAct"));
    connect(raytracefullAct, SIGNAL(triggered()), signalMapper, SLOT(map()));
   
    footprintAct = new QAction(QIcon(":/images/footprint.png"),tr("GO &footprint at selected element"), this);
    footprintAct->setStatusTip(tr("geometrical optics, footprint at selected element"));
    signalMapper->setMapping(footprintAct, QString("footprintAct"));
    connect(footprintAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    singleRayAct = new QAction(tr("GO single Ray &trace"), this);
    singleRayAct->setStatusTip(tr("geometrical optics, single Ray trace"));
    signalMapper->setMapping(singleRayAct, QString("singleRayAct"));
    connect(singleRayAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    optiInputAct = new QAction(tr("&optimization input"), this);
    optiInputAct->setStatusTip(tr("optimization input"));
    signalMapper->setMapping(optiInputAct, QString("optiInputAct"));
    connect(optiInputAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    phasespaceAct = new QAction(tr("PO imaging"), this);
    phasespaceAct->setStatusTip(tr("physical optics, phase space imaging"));
    signalMapper->setMapping(phasespaceAct, QString("phasespaceAct"));
    connect(phasespaceAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    mphasespaceAct = new QAction(tr("PO &multiple imaging"), this);
    mphasespaceAct->setStatusTip(tr("physical optics, multiple phase space imaging"));
    signalMapper->setMapping(mphasespaceAct, QString("mphasespaceAct"));
    connect(mphasespaceAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    writemapAct = new QAction(tr("Write Ma&p"), this);
    writemapAct->setStatusTip(tr("Write file with transfer map"));
    signalMapper->setMapping(writemapAct, QString("writemapAct"));
    connect(writemapAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    writematAct = new QAction(tr("Write Matri&x"), this);
    writematAct->setStatusTip(tr("Write file with transfer matrix"));
    signalMapper->setMapping(writematAct, QString("writematAct"));
    connect(writematAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    writecoeffAct = new QAction(tr("Write Mirr&or Coefficients"), this);
    writecoeffAct->setStatusTip(tr("Write file with mirror coefficients"));
    signalMapper->setMapping(writecoeffAct, QString("writecoeffAct"));
    connect(writecoeffAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    writesimpAct = new QAction(tr("Write &PO density cuts (real/im.)"), this);
    writesimpAct->setStatusTip(tr("Write simpre/simpim file in PO mode"));
    signalMapper->setMapping(writesimpAct, QString("writesimpAct"));
    connect(writesimpAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    writeResultAct = new QAction(tr("Write &Results "), this);
    writeResultAct->setStatusTip(tr("Write file with GO or PO results"));
    signalMapper->setMapping(writeResultAct, QString("writeResultAct"));
    connect(writeResultAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    readFg34Act = new QAction(tr("&Read file fg34.par"), this);
    readFg34Act->setStatusTip(tr("Read parameter file fg34.par (for compatibility with previous phase versions)"));
    signalMapper->setMapping(readFg34Act, QString("readFg34Act"));
    connect(readFg34Act, SIGNAL(triggered()), signalMapper, SLOT(map()));

    poInitSourceAct = new QAction(tr("PO &init source"), this);
    poInitSourceAct->setStatusTip(tr("Read Source files in physical optics mode"));
    signalMapper->setMapping(poInitSourceAct, QString("poInitSourceAct"));
    connect(poInitSourceAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    configureAct = new QAction(tr("&Configure..."), this);
    configureAct->setStatusTip(tr("Configure auxiliary files"));
    signalMapper->setMapping(configureAct, QString("configureAct"));
    connect(configureAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

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

  QGroupBox   *fileGroup       = new QGroupBox(tr("file"));
  QVBoxLayout *fileGroupLayout = new QVBoxLayout;
  fileNameLabel                = new QLabel(tr("undefined"));
  fileGroupLayout->addWidget(fileNameLabel);
  fileGroup->setLayout(fileGroupLayout);

  QGroupBox   *beamlineElementGroup  = new QGroupBox(tr("optical element list"));
  QVBoxLayout *beamlineElementLayout = new QVBoxLayout;

  elementList = new QListWidget();
  elementList->setAlternatingRowColors(true);
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
  QPushButton *insB  = new QPushButton(QIcon(":/images/up-32.png"),   tr("Ins"), this);
  QPushButton *appB  = new QPushButton(QIcon(":/images/up-32.png"),   tr("App"), this);
  QPushButton *delB  = new QPushButton(QIcon(":/images/down-32.png"), tr("Del"), this);
  beamlineButtomLayout->addWidget(insB);
  beamlineButtomLayout->addWidget(appB);
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
    vbox->addWidget(fileGroup);
    vbox->addWidget(beamlineElementGroup);

    vbox->addWidget(beamlineGenericGroup);
    vbox->addWidget(beamlineCalcGroup);
    vbox->addStretch(1);
    beamlineBox->setLayout(vbox);

    // slots
    connect(insB, SIGNAL(pressed()), this, SLOT(insertElement()));
    connect(appB, SIGNAL(pressed()), this, SLOT(appendElement()));
    connect(delB, SIGNAL(pressed()), this, SLOT(deleteElement()));
    connect(elementList, SIGNAL(itemSelectionChanged()), this, SLOT(selectElement()));
    connect(lambdaE, SIGNAL(editingFinished()), this, SLOT(lambdaSlot()));
    connect(dislenE, SIGNAL(editingFinished()), this, SLOT(dislenSlot()));
    connect(goButton, SIGNAL(clicked()), this, SLOT(goButtonslot()));
    connect(poButton, SIGNAL(clicked()), this, SLOT(poButtonslot()));
    connect(misaliBox, SIGNAL(stateChanged(int)), this, SLOT(misaliBoxslot(int)));
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
  QGroupBox   *statusGroup  = new QGroupBox(tr("Status"));
  QHBoxLayout *statusLayout = new QHBoxLayout;
  sourceStatLabel   = new QLabel(tr("<b><FONT COLOR=red>source</FONT></b>"));
  imageStatLabel    = new QLabel(tr("<b><FONT COLOR=red>image</FONT></b>"));
  mapStatLabel      = new QLabel(tr("<b><FONT COLOR=red>maps</FONT></b>"));
  elementStatLabel  = new QLabel(tr("<b><FONT COLOR=red>OE-X</FONT></b>"));
  statusLayout->addWidget(mapStatLabel);
  statusLayout->addWidget(elementStatLabel);
  statusLayout->addWidget(sourceStatLabel);
  statusLayout->addWidget(imageStatLabel);
  statusGroup->setLayout(statusLayout);

  QGroupBox   *graphicGroup  = new QGroupBox(tr("Graphics"));
  QGridLayout *graphicLayout = new QGridLayout;
  
  zminLabel  = new QLabel(tr("zmin (mm)"));
  zmaxLabel  = new QLabel(tr("zmax (mm)"));
  yminLabel  = new QLabel(tr("ymin (mm)"));
  ymaxLabel  = new QLabel(tr("ymax (mm)"));
  
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
  grscatterAct    = new QAction(tr("&scatter"), this);
  grcontourAct    = new QAction(tr("&contour"), this);
  grcontourisoAct = new QAction(tr("contour + iso &lines"), this);
  grisoAct        = new QAction(tr("&iso lines"), this);
  grHorProfAct    = new QAction(tr("profile hor."), this);
  grVerProfAct    = new QAction(tr("profile ver."), this);

  plotstyleMenu->addAction(grscatterAct);
  plotstyleMenu->addAction(grcontourAct);
  plotstyleMenu->addAction(grcontourisoAct);
  plotstyleMenu->addAction(grisoAct);
  
  plotstyleMenu->setDefaultAction(grcontourAct);
  plotstyleMenu->addSeparator();
  plotstyleMenu->addAction(grHorProfAct);
  plotstyleMenu->addAction(grVerProfAct);

  popupButton->setMenu(plotstyleMenu);
  grsignalMapper = new QSignalMapper(this);
  connect(grsignalMapper, SIGNAL(mapped(QString)), this, SLOT(activateProc(QString)));

  connect(grscatterAct,  SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grcontourAct,    SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grcontourisoAct, SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grisoAct,        SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grHorProfAct,    SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grVerProfAct,    SIGNAL(triggered()), grsignalMapper, SLOT(map()));

  grsignalMapper->setMapping(grscatterAct,    QString("grscatterAct"));
  grsignalMapper->setMapping(grcontourAct,    QString("grcontourAct"));
  grsignalMapper->setMapping(grcontourisoAct, QString("grcontourisoAct"));
  grsignalMapper->setMapping(grisoAct,        QString("grisoAct"));
  grsignalMapper->setMapping(grHorProfAct,    QString("grHorProfAct"));
  grsignalMapper->setMapping(grVerProfAct,    QString("grVerProfAct"));
  
  QPushButton *subjectpopupButton = new QPushButton(tr("&PlotSubject"));
  QMenu *subject = new QMenu(this);
  grGoSourceSpaAct  = new QAction(tr("GO &source"), this);
  grGoSourceDivAct  = new QAction(tr("GO s&ource divergence"), this);
  grGoSourcePhiAct  = new QAction(tr("GO so&urce phase"), this);
  
  grGoResultSpaAct  = new QAction(tr("GO &result"), this);
  grGoResultDivAct  = new QAction(tr("GO r&esult divergence"), this);
  grGoResultPhiAct  = new QAction(tr("GO re&sult phase"), this);

  grPoResultAct  = new QAction(tr("PO resu&lt"), this);

  grexample1Act  = new QAction(tr("example &1"), this);
  grexample2Act  = new QAction(tr("example &2"), this);
  grexample3Act  = new QAction(tr("example &3"), this);

  subject->addAction(grGoSourceSpaAct);
  subject->addAction(grGoSourceDivAct);
  subject->addAction(grGoSourcePhiAct);
  subject->addSeparator();
  subject->addAction(grGoResultSpaAct);
  subject->addAction(grGoResultDivAct);
  subject->addAction(grGoResultPhiAct);
  subject->addSeparator();
  subject->addAction(grPoResultAct);
  subject->addSeparator();
  subject->addAction(grexample1Act);
  subject->addAction(grexample2Act);
  subject->addAction(grexample3Act);
  subject->setDefaultAction(grGoResultSpaAct);
  subjectpopupButton->setMenu(subject);

  connect(grGoSourceSpaAct,   SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grGoSourceDivAct,   SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grGoSourcePhiAct,   SIGNAL(triggered()), grsignalMapper, SLOT(map()));

  connect(grGoResultSpaAct,   SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grGoResultDivAct,   SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grGoResultPhiAct,   SIGNAL(triggered()), grsignalMapper, SLOT(map()));

  connect(grPoResultAct,   SIGNAL(triggered()), grsignalMapper, SLOT(map()));

  connect(grexample1Act, SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grexample2Act, SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grexample3Act, SIGNAL(triggered()), grsignalMapper, SLOT(map()));

  grsignalMapper->setMapping(grGoSourceSpaAct,   QString("grGoSourceSpaAct"));
  grsignalMapper->setMapping(grGoSourceDivAct,   QString("grGoSourceDivAct"));
  grsignalMapper->setMapping(grGoSourcePhiAct,   QString("grGoSourcePhiAct"));

  grsignalMapper->setMapping(grGoResultSpaAct,   QString("grGoResultSpaAct"));
  grsignalMapper->setMapping(grGoResultDivAct,   QString("grGoResultDivAct"));
  grsignalMapper->setMapping(grGoResultPhiAct,   QString("grGoResultPhiAct"));

  grsignalMapper->setMapping(grPoResultAct,   QString("grPoResultAct"));

  grsignalMapper->setMapping(grexample1Act, QString("grexample1Act"));
  grsignalMapper->setMapping(grexample2Act, QString("grexample2Act"));
  grsignalMapper->setMapping(grexample3Act, QString("grexample3Act"));
 
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
  
  QGroupBox   *plotGroup  = new QGroupBox(tr("plot"));
  QGridLayout *plotGroupLayout = new QGridLayout(plotGroup);

  //a few buttons
  //QToolButton* btnLogx = new QToolButton(plotGroup);
  // btnLogx->setText("Log Scale X");
  //btnLogx->setCheckable(true);
  //  connect(btnLogx, SIGNAL(toggled(bool)), plot, SLOT(SetLogX(bool)));
  QToolButton* btnLogy = new QToolButton(plotGroup);
  //btnLogy->setText("Log Scale Y");
  btnLogy->setText("Log Scale");
  btnLogy->setCheckable(true);
  

  d_plot = new Plot(this);
  d_plot->setAxisTitle(2, tr("z (mm)"));
  d_plot->setAxisTitle(0, tr("y (mm)"));
  d_plot->setTitle(tr("PhaseQt"));

  connect(btnLogy, SIGNAL(toggled(bool)), d_plot, SLOT(SetLog(bool)));

//plotGroupLayout->addWidget(label,1,1,1,1);
//  plotGroupLayout->addWidget(btnLogx,1,2,1,1);
//  plotGroupLayout->addWidget(btnLogy,1,3,1,1);
  plotGroupLayout->addWidget(d_plot,2,1,3,3);
  //plotGroupLayout->addWidget(label2,5,1,1,3);  
  plotGroupLayout->setMargin(12);

  statGroup  = new QGroupBox(tr("Statistics"));
  QGridLayout *statLayout = new QGridLayout;
  
  sigmaButton = new QRadioButton(tr("&RMS"));
  fwhmButton  = new QRadioButton(tr("&FWHM"));
  fwhmButton->setChecked(true);
  connect(sigmaButton,    SIGNAL(clicked()), this, SLOT(sigmaslot()));
  connect(fwhmButton,     SIGNAL(clicked()), this, SLOT(fwhmslot()));

  czLabel   = new QLabel("0");
  cyLabel   = new QLabel("0");
  wzLabel   = new QLabel("0");
  wyLabel   = new QLabel("0");
  cdzLabel  = new QLabel("0");
  cdyLabel  = new QLabel("0");
  wdzLabel  = new QLabel("0");
  wdyLabel  = new QLabel("0");
  rayLabel  = new QLabel("0");
  traLabel  = new QLabel("0");
  ryLabel   = new QLabel("0");
  rzLabel   = new QLabel("0");

  czLabel0   = new QLabel(tr("z center (mm)"));
  cyLabel0   = new QLabel(tr("y center (mm)"));
  wzLabel0   = new QLabel(tr("z FWHM (mm)"));
  wyLabel0   = new QLabel(tr("y FWHM (mm)"));
  cdzLabel0  = new QLabel(tr("dz center (mm)"));
  cdyLabel0  = new QLabel(tr("dy center (mm)"));
  wdzLabel0  = new QLabel(tr("dz FWHM (mm)"));
  wdyLabel0  = new QLabel(tr("dy FWHM (mm)"));
  rayLabel0  = new QLabel(tr("rays"));
  traLabel0  = new QLabel(tr("transmittance"));
  ryLabel0   = new QLabel(tr("y E/dE FWHM"));
  rzLabel0   = new QLabel(tr("z E/dE FWHM"));

  statLayout->addWidget(sigmaButton, 0, 0);
  statLayout->addWidget(fwhmButton,  0, 1);

  statLayout->addWidget(czLabel0,  1, 0);
  statLayout->addWidget(cyLabel0,  1, 2);
  statLayout->addWidget(wzLabel0,  2, 0);
  statLayout->addWidget(wyLabel0,  2, 2);
  statLayout->addWidget(cdzLabel0, 3, 0);
  statLayout->addWidget(cdyLabel0, 3, 2);
  statLayout->addWidget(wdzLabel0, 4, 0);
  statLayout->addWidget(wdyLabel0, 4, 2);
  statLayout->addWidget(rayLabel0, 5, 0);
  statLayout->addWidget(traLabel0, 5, 2);
  statLayout->addWidget(ryLabel0,  6, 2);
  statLayout->addWidget(rzLabel0,  6, 0);

  statLayout->addWidget(czLabel,  1, 1);
  statLayout->addWidget(cyLabel,  1, 3);
  statLayout->addWidget(wzLabel,  2, 1);
  statLayout->addWidget(wyLabel,  2, 3);
  statLayout->addWidget(cdzLabel, 3, 1);
  statLayout->addWidget(cdyLabel, 3, 3);
  statLayout->addWidget(wdzLabel, 4, 1);
  statLayout->addWidget(wdyLabel, 4, 3);
  statLayout->addWidget(rayLabel, 5, 1);
  statLayout->addWidget(traLabel, 5, 3);
  statLayout->addWidget(ryLabel,  6, 3);
  statLayout->addWidget(rzLabel,  6, 1);

  statGroup->setLayout(statLayout);

  QVBoxLayout *vbox = new QVBoxLayout;
  vbox->addWidget(statusGroup);
  vbox->addWidget(graphicGroup);
  //vbox->addWidget(d_plot);
  vbox->addWidget(plotGroup);
  vbox->addStretch(1);
  vbox->addWidget(statGroup);
  //vbox->addStretch(1);
  graphicBox->setLayout(vbox);
  return graphicBox;
} // end creategraphic box

// create menus with buttons
void MainWindow::createMenus()
{
    fileMenu = menuBar()->addMenu(tr("&File"));
    fileMenu->addAction(newBeamlineAct);
    fileMenu->addAction(openBeamlineAct);
    fileMenu->addAction(saveAct);
    fileMenu->addAction(saveasAct);
    fileMenu->addAction(printAct);
    fileMenu->addSeparator();
    fileMenu->addAction(quitAct);

    editMenu = menuBar()->addMenu(tr("&Edit"));
    editMenu->addAction(undoAct);
    editMenu->addAction(readFg34Act);
    editMenu->addAction(poInitSourceAct);
    editMenu->addAction(optiInputAct);
    editMenu->addSeparator();
    editMenu->addAction(configureAct);

    calcMenu = menuBar()->addMenu(tr("&Calc"));
    calcMenu->addAction(raytracesimpleAct);
    calcMenu->addAction(raytracefullAct);
    calcMenu->addAction(footprintAct);
    calcMenu->addAction(singleRayAct);
    
    calcMenu->addSeparator();
    calcMenu->addAction(phasespaceAct);
    calcMenu->addAction(mphasespaceAct);
    //calcMenu->addSeparator();
    //calcMenu->addAction(optiInputAct);

    cmdMenu = menuBar()->addMenu(tr("C&ommands"));
    cmdMenu->addAction(writeResultAct);
    cmdMenu->addAction(writemapAct);
    cmdMenu->addAction(writematAct);
    cmdMenu->addAction(writecoeffAct);
    cmdMenu->addAction(writesimpAct);
    
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
  apAct = new QAction(tr("&Aperture/Slit"), this); 
  shapeMenu->addAction(pmAct);
  shapeMenu->addAction(toAct);
  shapeMenu->addAction(peAct);
  shapeMenu->addAction(elAct);
  shapeMenu->addAction(coAct);
  shapeMenu->addSeparator();
  shapeMenu->addAction(geAct);
  shapeMenu->addSeparator();
  shapeMenu->addAction(apAct);
  shapeMenu->setDefaultAction(toAct);
  shapeButton->setMenu(shapeMenu);

  connect(pmAct, SIGNAL(triggered()), this, SLOT(pmslot()));
  connect(toAct, SIGNAL(triggered()), this, SLOT(toslot()));
  connect(peAct, SIGNAL(triggered()), this, SLOT(peslot()));
  connect(elAct, SIGNAL(triggered()), this, SLOT(elslot()));
  connect(coAct, SIGNAL(triggered()), this, SLOT(coslot()));
  connect(geAct, SIGNAL(triggered()), this, SLOT(geslot()));
  connect(apAct, SIGNAL(triggered()), this, SLOT(apslot()));

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
  sourceB = new QPushButton(QIcon(":/images/Blue-arrow-right-32.png"), tr("copy"), this);
  imageB  = new QPushButton(QIcon(":/images/Blue-arrow-right-32.png"), tr("copy"), this);
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
  QListWidgetItem *item;
  char buffer[50];
  int i;

#ifdef DEBUG
  printf("debug: createParameterBox called\n");
#endif

  // upper part
  QGroupBox   *parameterGroup  = new QGroupBox(tr("Parameters"));
  QVBoxLayout *parameterLayout = new QVBoxLayout;

  parameterList = new QListWidget();
  parameterList->setAlternatingRowColors(true);

  QLabel *parameterLabel  = new QLabel(tr("edit value"));
  parameterE  = new QLineEdit;

  QFile file(":/parameter.default");
  file.open(QIODevice::ReadOnly);
  parameterModel= new TreeModel(file.readAll(), this, parameterE);
  //TreeModel model(file.readAll());
  file.close();

  QTreeView *parameterView = new QTreeView();
  parameterView->setAlternatingRowColors(true);
  parameterView->setModel(parameterModel);
  parameterView->resizeColumnToContents(1);
    parameterView->resizeColumnToContents(3);
#ifndef DEBUG1  
  parameterView->setColumnHidden(4,true);  // dont display index
#endif
  connect(parameterView, SIGNAL(clicked(QModelIndex)), parameterModel, SLOT(selectSlot(QModelIndex))); 


  for (i= 0; i< NPARS; i++)
    {
      snprintf(buffer, 4, "%d : parameter",  i);
      item= new QListWidgetItem(buffer);
      parameterList->insertItem(i, item);
    }

  //parameterLayout->addWidget(parameterList);
  parameterLayout->addWidget(parameterView);
  parameterLayout->addWidget(parameterLabel);
  parameterLayout->addWidget(parameterE);
  //  parameterLayout->addWidget(view);

  parameterGroup->setLayout(parameterLayout);

  QVBoxLayout *vbox = new QVBoxLayout;
  vbox->addWidget(parameterGroup);
  
  vbox->addStretch(1);
  parameterBox->setLayout(vbox);
  connect(parameterList, SIGNAL(itemSelectionChanged()), this, SLOT(selectParameter()));
  // connect(parameterE, SIGNAL(editingFinished()), this, SLOT(parameterUpdateSlot()));
  connect(parameterE, SIGNAL(returnPressed()), this, SLOT(parameterUpdateSlot()));
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
  impAct = new QAction(tr("PO image plane"), this);

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
  sourceMenu->addSeparator();
  sourceMenu->addAction(impAct);
  sourceMenu->setDefaultAction(poiAct);
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
  connect(impAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

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
  signalMapper->setMapping(impAct, QString("impAct"));

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
    //fileToolBar->addAction(newBeamlineAct);
    fileToolBar->addAction(openBeamlineAct);
    fileToolBar->addAction(saveAct);
    fileToolBar->addAction(printAct);

    editToolBar = addToolBar(tr("Edit"));
    //    editToolBar->addAction(undoAct);
    editToolBar->addAction(footprintAct);
    editToolBar->addAction(raytracesimpleAct);
    editToolBar->addAction(raytracefullAct);
} // createToolBars


///////////////////////////////////
// end widget definition section //
///////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////
// begin widget handling section //
///////////////////////////////////

// update all parameters from dataset
void MainWindow::parameterUpdateAll(int zahl)
{
#ifdef DEBUG
  printf("debug: parameterUpdateAll: zahl: %d\n", zahl);
#endif
  int i;
  for (i=0; i< zahl; i++) parameterUpdate(i, " ", 1);
} // parameterUpdateAll


// helper function for the parameterUpdateSlot
// init=1:  does not scan the text - for initialization 
// defaults can be set with an empty text
// function: 
// a) read selected item
// b) copy contents into data structure
// c) update the item in the list
void MainWindow::parameterUpdate(int pos, const char *text, int init)
{
  char buffer[MaxPathLength];
  int scanned= 1;
 
  struct OptionsType   *op = myparent->myOptions();
  struct sources    *mysrc = &(myparent->myBeamline()->src);
  
#ifdef DEBUG1
  cout << __FILE__ << " debug: parameterUpdate: pos: " << pos << endl;
#endif

  switch (pos)
    {
    case 0: 
      if (!init) scanned= sscanf(text, "%lf", &op->epsilon);
      if ((scanned == EOF) || (scanned == 0)) op->epsilon= 1e-4; // default
      snprintf(buffer, MaxPathLength,  "%-5lg", op->epsilon);
      break;
    case 1:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.iord);
      printf("parameterUpdate: scanned_pos: %d\n", scanned);
      if ((scanned == EOF) || (scanned == 0) || (op->ifl.iord < 1) || 
	  (op->ifl.iord > 7)) op->ifl.iord= 4;             // set default
      snprintf(buffer, MaxPathLength,  "%d", op->ifl.iord);
      break;
    case 2:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.iordsc);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.iordsc= 4; // default
      snprintf(buffer, MaxPathLength,  "%d", op->ifl.iordsc);
      break;
    case 3:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.iexpand);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.iexpand= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->ifl.iexpand);
      break;
    case 4:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.iplmode);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.iplmode= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->ifl.iplmode);
      break;
    case 5:
      if (!init) scanned= sscanf(text, "%d", &(myparent->myBeamline()->src.isrctype));
      if ((scanned == EOF) || (scanned == 0)) myparent->myBeamline()->src.isrctype= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", myparent->myBeamline()->src.isrctype);
      break; 
    case 6:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.rpin);
      if ((scanned == EOF) || (scanned == 0)) op->apr.rpin= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->apr.rpin);
      break;
    case 7:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.srcymin);
      if ((scanned == EOF) || (scanned == 0)) op->apr.srcymin= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->apr.srcymin);
      break;
    case 8:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.srcymax);
      if ((scanned == EOF) || (scanned == 0)) op->apr.srcymax= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->apr.srcymax);
      break;
    case 9:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.srczmin);
      if ((scanned == EOF) || (scanned == 0)) op->apr.srczmin= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg",  op->apr.srczmin);
      break;
    case 10:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.srczmax);
      if ((scanned == EOF) || (scanned == 0)) op->apr.srczmax= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->apr.srczmax);
      break;
    case 11:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.rpin_ap);
      if ((scanned == EOF) || (scanned == 0)) op->apr.rpin_ap= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->apr.rpin_ap);
      break;
    case 12:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.ymin_ap);
      if ((scanned == EOF) || (scanned == 0)) op->apr.ymin_ap= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->apr.ymin_ap);
      break;
    case 13:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.ymax_ap);
      if ((scanned == EOF) || (scanned == 0)) op->apr.ymax_ap= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->apr.ymax_ap);
      break;
    case 14:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.zmin_ap);
      if ((scanned == EOF) || (scanned == 0)) op->apr.zmin_ap= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->apr.zmin_ap);
      break;
    case 15:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.zmax_ap);
      if ((scanned == EOF) || (scanned == 0)) op->apr.zmax_ap= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->apr.zmax_ap);
      break;

    case 16:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->so5.dipcy);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so5.dipcy= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", mysrc->so5.dipcy);
      break;
    case 17:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->so5.dipcz);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so5.dipcz= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", mysrc->so5.dipcz);
      break;
    case 18:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->so5.dipdisy);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so5.dipdisy= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", mysrc->so5.dipdisy);
      break;
    case 19:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->so5.dipdisz);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so5.dipdisz= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", mysrc->so5.dipdisz);
      break;

    case 20:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.inorm);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.inorm= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->ifl.inorm);
      break;
    case 21:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.inorm1);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.inorm1= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->ifl.inorm1);
      break;
    case 22:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.inorm2);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.inorm2= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->ifl.inorm2);
      break;
    case 23:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.matrel);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.matrel= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->ifl.matrel);
      break;

    case 24:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so1.isrcy);
      if ((scanned == EOF) || (scanned == 0))  mysrc->so1.isrcy= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d",  mysrc->so1.isrcy);
      break;
    case 25:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so1.isrcdy);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so1.isrcdy= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", mysrc->so1.isrcdy);
      break;
    case 26:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->so1.sigmay);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so1.sigmay= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", mysrc->so1.sigmay);
      break;
    case 27:
      if (!init) 
	{ 
	  scanned= sscanf(text, "%lg", &mysrc->so1.sigmayp);
	  mysrc->so1.sigmayp*= 1e-3;
	}
      if ((scanned == EOF) || (scanned == 0)) mysrc->so1.sigmayp= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", mysrc->so1.sigmayp*1e3);
      break;

    case 28:
      if (!init) 
	{
	  scanned= sscanf(text, "%lg", &op->xi.ymin);
	  op->xi.ymin*= 1e-3;
	}
      if ((scanned == EOF) || (scanned == 0)) op->xi.ymin= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->xi.ymin* 1e3);
      break;
    case 29:
      if (!init)
	{
	  scanned= sscanf(text, "%lg", &op->xi.ymax);
	  op->xi.ymax*= 1e-3;
	}
      if ((scanned == EOF) || (scanned == 0)) op->xi.ymax= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->xi.ymax* 1e3);
      break;
    case 30:
      if (!init) scanned= sscanf(text, "%d", &op->xi.ianzy0);
      if ((scanned == EOF) || (scanned == 0)) op->xi.ianzy0= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->xi.ianzy0);
      break;

    case 31:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so1.isrcz);
      if ((scanned == EOF) || (scanned == 0))  mysrc->so1.isrcz= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d",  mysrc->so1.isrcz);
      break;
    case 32:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so1.isrcdz);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so1.isrcdz= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", mysrc->so1.isrcdz);
      break;
    case 33:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->so1.sigmaz);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so1.sigmaz= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", mysrc->so1.sigmaz);
      break;
    case 34:
      if (!init)
	{
	  scanned= sscanf(text, "%lg", &mysrc->so1.sigmazp);
	  mysrc->so1.sigmazp*= 1e-3;
	}
      if ((scanned == EOF) || (scanned == 0)) mysrc->so1.sigmazp= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", mysrc->so1.sigmazp*1e3);
      break;

    case 35:
      if (!init)
	{
	  scanned= sscanf(text, "%lg", &op->xi.zmin);
	  op->xi.zmin*=1e-3;
	}
      if ((scanned == EOF) || (scanned == 0)) op->xi.zmin= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->xi.zmin* 1e3);
      break;
    case 36:
      if (!init) 
	{
	  scanned= sscanf(text, "%lg", &op->xi.zmax);
	  op->xi.zmax*= 1e-3;
	}
      if ((scanned == EOF) || (scanned == 0)) op->xi.zmax= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->xi.zmax* 1e3);
      break;
    case 37:
      if (!init) scanned= sscanf(text, "%d", &op->xi.ianzz0);
      if ((scanned == EOF) || (scanned == 0)) op->xi.ianzz0= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->xi.ianzz0);
      break;

    case 38:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.ibright);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.ibright= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->ifl.ibright);
      break;

    case 39:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.ispline);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.ispline= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->ifl.ispline);
      break;

    case 40:
      if (!init) scanned= sscanf(text, "%lg", &op->xi.d12_max);
      if ((scanned == EOF) || (scanned == 0)) op->xi.d12_max= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->xi.d12_max);
      break;
    case 41:
      if (!init) scanned= sscanf(text, "%d", &op->xi.id12);
      if ((scanned == EOF) || (scanned == 0)) op->xi.id12= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->xi.id12);
      break;
    case 42:
      if (!init) scanned= sscanf(text, "%d", &op->xi.ianz0_cal);
      if ((scanned == EOF) || (scanned == 0)) op->xi.ianz0_cal= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->xi.ianz0_cal);
      break;
    case 43:
      if (!init) scanned= sscanf(text, "%d", &op->xi.ianz0_fixed);
      if ((scanned == EOF) || (scanned == 0)) op->xi.ianz0_fixed= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->xi.ianz0_fixed);
      break;
    case 44:
      if (!init) scanned= sscanf(text, "%d", &op->xi.iamp_smooth);
      if ((scanned == EOF) || (scanned == 0)) op->xi.iamp_smooth= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->xi.iamp_smooth);
      break;
    case 45:
      if (!init) scanned= sscanf(text, "%d", &op->xi.iord_amp);
      if ((scanned == EOF) || (scanned == 0)) op->xi.iord_amp= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->xi.iord_amp);
      break;
    case 46:
      if (!init) scanned= sscanf(text, "%d", &op->xi.ifm_amp);
      if ((scanned == EOF) || (scanned == 0)) op->xi.ifm_amp= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->xi.ifm_amp);
      break;
      
    case 47:
      if (!init) scanned= sscanf(text, "%d", &op->xi.iord_pha);
      if ((scanned == EOF) || (scanned == 0)) op->xi.iord_pha= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->xi.iord_pha);
      break;
      
    case 48:
      if (!init) scanned= sscanf(text, "%d", &op->xi.ifm_pha);
      if ((scanned == EOF) || (scanned == 0)) op->xi.ifm_pha= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->xi.ifm_pha);
      break;
    case 49:
      if (!init) scanned= sscanf(text, "%lg", &op->xi.distfocy);
      if ((scanned == EOF) || (scanned == 0)) op->xi.distfocy= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->xi.distfocy);
      break;
    case 50:
      if (!init) scanned= sscanf(text, "%lg", &op->xi.distfocz);
      if ((scanned == EOF) || (scanned == 0)) op->xi.distfocz= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", op->xi.distfocz);
      break;
    case 51:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.ipinarr);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.ipinarr= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->ifl.ipinarr);
      break;    
    case 52:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->pin_yl0);
      if ((scanned == EOF) || (scanned == 0)) mysrc->pin_yl0= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", mysrc->pin_yl0);
      break;
    case 53:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->pin_yl);
      if ((scanned == EOF) || (scanned == 0)) mysrc->pin_yl= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", mysrc->pin_yl);
      break;
    case 54:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->pin_zl0);
      if ((scanned == EOF) || (scanned == 0)) mysrc->pin_zl0= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", mysrc->pin_zl0);
      break;
    case 55:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->pin_zl);
      if ((scanned == EOF) || (scanned == 0)) mysrc->pin_zl= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", mysrc->pin_zl);
      break;
    case 56:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so4.nfreqtot);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so4.nfreqtot= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", mysrc->so4.nfreqtot);
      break;
    case 57:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so4.nfreqpos);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so4.nfreqpos= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", mysrc->so4.nfreqpos);
      break;
    case 58:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so4.nfreqneg);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so4.nfreqneg= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", mysrc->so4.nfreqneg);
      break;
    case 59:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so4.nsource);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so4.nsource= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", mysrc->so4.nsource);
      break;
    case 60:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so4.nimage);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so4.nimage= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", mysrc->so4.nimage);
      break;
      
    case 61:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->so4.deltatime);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so4.deltatime= 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", mysrc->so4.deltatime);
      break;
      
    case 62:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so4.iconj);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so4.iconj= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", mysrc->so4.iconj);
      break;

    case 63:
      if (!init) scanned= sscanf(text, "%d", &op->REDUCE_maps);
      if ((scanned == EOF) || (scanned == 0)) op->REDUCE_maps= 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", op->REDUCE_maps);
      myparent->myBeamline()->hormapsloaded= 0;
      break;

#ifdef XXXTEMPLATE
case 10:
      if (!init) scanned= sscanf(text, "%d", &);
      if ((scanned == EOF) || (scanned == 0)) = 0;   // default
      snprintf(buffer, MaxPathLength,  "%d", );
      break;

case 10:
      if (!init) scanned= sscanf(text, "%lg", &);
      if ((scanned == EOF) || (scanned == 0)) = 0;   // default
      snprintf(buffer, MaxPathLength,  "%lg", );
      break;
#endif
    default:
      snprintf(buffer, MaxPathLength,  "%d \t: unknown parameter", pos);
    }
    
  parameterModel->updateItemVal(QString(buffer), pos);

} // end parameterUpdate

// interactive version checks for backupfile
void MainWindow::ReadBLFileInteractive(char *blname)
{
  char fname[MaxPathLength], oname[MaxPathLength];  
  char buffer[300];      
  struct stat fstatus;
  time_t mtime_data, mtime_backup;
  
#ifdef DEBUG
  cout << "debug: ReadBLFileInteractive called with file >>" << blname << "<<" << endl;
#endif

  strncpy(fname, blname, (MaxPathLength - 1));
  strncpy(oname, blname, (MaxPathLength - 1));
  strcat(fname, "~");

  if (fexists(fname))
    {
      if (stat(fname, &fstatus) == 0)
	{
	  mtime_backup= fstatus.st_mtime;
	  if (stat(blname, &fstatus) == 0)
	    {
	      mtime_data= fstatus.st_mtime;
	      if (mtime_data < mtime_backup)
		{
		  QMessageBox *msgBox = new QMessageBox;
		  snprintf(buffer, 300, "<b>We found a newer backupfile</b>\n%s", fname);
		  msgBox->setText(buffer);
		  msgBox->setInformativeText("Do you want to use the backup?");
		  msgBox->setStandardButtons(QMessageBox::Yes | QMessageBox::No);
		  msgBox->setDefaultButton(QMessageBox::Yes);
		  msgBox->setIcon(QMessageBox::Question);
		  int ret = msgBox->exec();
		  if (ret == QMessageBox::Yes) strncpy(oname, fname, (MaxPathLength - 1));
		}
	    }
	}
    }
  myparent->myReadBLFile(oname);
  strncpy(myparent->myPHASEset()->so4_fsource4a, myparent->myBeamline()->src.so4.fsource4a, 80);
  strncpy(myparent->myPHASEset()->so4_fsource4b, myparent->myBeamline()->src.so4.fsource4b, 80);
  strncpy(myparent->myPHASEset()->so4_fsource4c, myparent->myBeamline()->src.so4.fsource4c, 80);
  strncpy(myparent->myPHASEset()->so4_fsource4d, myparent->myBeamline()->src.so4.fsource4d, 80);
  strncpy(myparent->myPHASEset()->so6_fsource6,  myparent->myBeamline()->src.so6.fsource6,  80);
  if (c_window) c_window->updateList();
  myparent->myWriteBLFile(blname);  // to reset the time
} // ReadBLFileInteractive


// UpdateBeamlineBox()
// the box on the left
void MainWindow::UpdateBeamlineBox()
{
  struct OptionsType *blo;
  QString lambdaEqst, dislenEqst;
  
  fileNameLabel->setText(QString(tr(myparent->myPHASEset()->beamlinename)));
  blo= &(myparent->myBeamline()->BLOptions);

  lambdaE->setText(lambdaEqst.setNum(blo->lambda* 1e6, 'g', 4));
  dislenE->setText(dislenEqst.setNum(blo->displength,  'g', 4));

  if (blo->SourcetoImage == 1) goButton->setChecked(true); else poButton->setChecked(true);
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
  myparent->myBeamline()->ElementList[number].ElementOK = 0;
  myparent->myBeamline()->beamlineOK &= ~(mapOK | resultOK);

  struct mdatset *md= &(myparent->myBeamline()->ElementList[number].MDat);
  struct gdatset *gd= &(myparent->myBeamline()->ElementList[number].GDat);

  teta= fabs(gd->theta0* PI/ 180.0);
  fi  = (double)(gd->inout)* 
    asin(myparent->myBeamline()->BLOptions.lambda* gd->xdens[0]/ (2.0* cos(teta)));
  cff = cos(fi- teta)/ cos(fi+ teta);

  // create strings
  snprintf(TextField[0], 40,  "%.2f", cff);
  snprintf(TextField[1], 40,  "%.1f", gd->r);
  snprintf(TextField[2], 40,  "%.1f", gd->rp);
  snprintf(TextField[3], 40,  "%.3f", gd->theta0);
  snprintf(TextField[4], 40,  "%.1f", md->r1);   
  snprintf(TextField[5], 40,  "%.1f", md->r2);
  snprintf(TextField[6], 40,  "%.1f", md->rmi);      
  snprintf(TextField[7], 40,  "%.2f", md->rho);
  snprintf(TextField[8], 40,  "%d",   gd->inout);   // not used   	   
  snprintf(TextField[9], 40,  "%.2f", gd->xdens[0]); 
  snprintf(TextField[10], 40, "%.2f", gd->xdens[1]);    
  snprintf(TextField[11], 40, "%.2f", gd->xdens[2]);    
  snprintf(TextField[12], 40, "%.2f", gd->xdens[3]);    
  snprintf(TextField[13], 40, "%.2f", gd->xdens[4]);
  snprintf(TextField[14], 40, "%.2f", md->du);    
  snprintf(TextField[15], 40, "%.2f", md->dw);    
  snprintf(TextField[16], 40, "%.2f", md->dl);
  snprintf(TextField[17], 40, "%.2f", md->dRu * 1e3);    
  snprintf(TextField[18], 40, "%.2f", md->dRw * 1e3);    
  snprintf(TextField[19], 40, "%.2f", md->dRl * 1e3);
  snprintf(TextField[20], 40, "%.2f", md->w1);    
  snprintf(TextField[21], 40, "%.2f", md->w2);    
  snprintf(TextField[22], 40, "%.3f", md->slopew);
  snprintf(TextField[23], 40, "%.2f", md->l1);    
  snprintf(TextField[24], 40, "%.2f", md->l2);    
  snprintf(TextField[25], 40, "%.3f", md->slopel);

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

  // element type set default sensitivity
  gratingGroup->setChecked(false);
  vlsGroup    ->setChecked(false);
  rhoE->setEnabled(false);
  rE->setEnabled(false);
  cffE->setEnabled(false);
  rB->setEnabled(false);
  rhoB->setEnabled(false);
  sourceB->setEnabled(false);
  imageB->setEnabled(false);
  sourceE->setEnabled(false);
  imageE->setEnabled(false);
  thetaB->setEnabled(false);
  switch (md->Art & 1023)          // strip off higher bits
    {
    case kEOEPG:                   // for compatibility with old datasets
      md->Art= kEOEPM + GRATINGBIT ; 
      cffE->setEnabled( true );
    case kEOEPM:
      shapeLabel->setText(QString(tr("flat"))); 
      break;
    case kEOETG:                   // for compatibility with old datasets
      md->Art= kEOETM + GRATINGBIT ;
      cffE->setEnabled( true );
      rhoE->setEnabled( true );
      rE->setEnabled( true );
      rB->setEnabled( true );
      rhoB->setEnabled( true );
    case kEOEVLSG:                 // for compatibility with old datasets 
      md->Art= kEOETM + GRATINGBIT + VLSBIT ;
      cffE->setEnabled( true );
      rhoE->setEnabled( true );
      rE->setEnabled( true );
      rB->setEnabled( true );
      rhoB->setEnabled( true );
    case kEOETM:   
      shapeLabel->setText(QString(tr("toroidal"))); 
      rhoE->setEnabled(true);
      rE->setEnabled(true);
      rB->setEnabled(true);
      rhoB->setEnabled(true);
      sourceB->setEnabled(true);
      imageB->setEnabled(true);
      sourceE->setEnabled(true);
      imageE->setEnabled(true);
      break;
    case kEOEPElli:   
      shapeLabel->setText(QString(tr("plane-elliptical"))); 
      sourceB->setEnabled(true);
      imageB->setEnabled(true);
      sourceE->setEnabled(true);
      imageE->setEnabled(true);
      break;
    case kEOEElli:   
      shapeLabel->setText(QString(tr("elliptical"))); 
      sourceB->setEnabled(true);
      imageB->setEnabled(true);
      sourceE->setEnabled(true);
      imageE->setEnabled(true);
      break;
    case kEOECone:   
      shapeLabel->setText(QString(tr("conical"))); break;
    case kEOEGeneral:
      shapeLabel->setText(QString(tr("generic"))); break;
    case kEOESlit:
      shapeLabel->setText(QString(tr("Aperture/Slit"))); 
      break;
    default: 
      shapeLabel->setText(QString(tr("unknown")));
      QMessageBox::warning(this, tr("UpdateElementBox"),
			   tr("Shape type %1 not recognized.\nreport bug to uwe.flechsig@psi.ch")
			   .arg(md->Art));
      break;
    }
  if (md->Art & GRATINGBIT ) 
    {
      gratingGroup->setChecked(true); 
      cffE->setEnabled( true ); 
      thetaB->setEnabled(true);
    }
  if (md->Art & VLSBIT ) 
    {
      vlsGroup->setChecked(true); 
      cffE->setEnabled( true ); 
      thetaB->setEnabled(true);
    }
  UpdateStatus();
} // end UpdateElementBox

// updates the elementlist
void MainWindow::UpdateElementList()
{
  unsigned int ui;
  struct ElementType *list;
  
#ifdef DEBUG
  printf("MainWindow::UpdateElementList(): elements in widget:  %d\n", elementList->count());
  printf("MainWindow::UpdateElementList(): elements in dataset: %d\n", myparent->myBeamline()->elementzahl);
#endif

  // loesche alles
   while (elementList->count()) 
    delete elementList->takeItem(0);

  list= myparent->myBeamline()->ElementList;
  for (ui= 0; ui < myparent->myBeamline()->elementzahl; ui++, list++)
    {
      QListWidgetItem *item= new QListWidgetItem(QString(list->elementname));
      item->setFlags (item->flags () | Qt::ItemIsEditable); 
      elementList->addItem(item);
    }
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
  struct FileSourceType       *fp; 
  struct PSImageType          *psip;
  //  struct SRSourceType         *sp; 
  //  
  //  struct PSSourceType         *pssp; 

  char TextField [8][40];            /* 8 editfelder */
  char LabelField[9][100]; 
   
  myparent->myBeamline()->beamlineOK &= ~(sourceOK | resultOK | pstimageOK); 
        
#ifdef DEBUG 
  printf("debug: InitSourceBox: bl->RTSource.QuellTyp: %c, beamlineOK: %X, oldsource: %c\n", 
	 myparent->myBeamline()->RTSource.QuellTyp, myparent->myBeamline()->beamlineOK, oldsource);   
#endif   
 
    if (myparent->myBeamline()->RTSource.Quellep == NULL)
      {
	printf("error: UpdateSourceBox: Quellep == NULL\n");
	return;
      }
    myparent->myAllocRTSource();
    sou= myparent->myBeamline()->RTSource.QuellTyp;
    
    if (sou != oldsource)
      {
	printf("source changed: set defaults\n");
	oldsource= sou;
	myparent->sourceSetDefaults();
      }

    S1E->setEnabled(false);
    S2E->setEnabled(false);
    S3E->setEnabled(false);
    S4E->setEnabled(false);
    S5E->setEnabled(false);
    S6E->setEnabled(false);
    S7E->setEnabled(false);
    S8E->setEnabled(false);

    switch (sou) {
 
    case 'D':
      dp= (struct DipolSourceType *)myparent->myBeamline()->RTSource.Quellep;
      snprintf(TextField[0],  40,  "%f", dp->sigy);
      snprintf(TextField[1],  40,  "%f", dp->sigdy);    
      snprintf(TextField[2],  40,  "%f", dp->sigz);  
      snprintf(TextField[3],  40,  "%f", dp->dz);    
      snprintf(TextField[4],  40,  "%d", myparent->myBeamline()->RTSource.raynumber);   
      snprintf(TextField[5],  40,  "%s", "");    
      snprintf(TextField[6],  40,  "%s", "");   
      snprintf(TextField[7],  40,  "%s", ""); 
      snprintf(LabelField[0], 100, "%s", "sigmay (mm)");
      snprintf(LabelField[1], 100, "%s", "sigmady (mrad)");  
      snprintf(LabelField[2], 100, "%s", "sigmaz (mm)");  
      snprintf(LabelField[3], 100, "%s", "dz hard edge (mrad)");  
      snprintf(LabelField[4], 100, "%s", "ray number");   
      snprintf(LabelField[5], 100, "%s", "");   
      snprintf(LabelField[6], 100, "%s", "");   
      snprintf(LabelField[7], 100, "%s", "");
      snprintf(LabelField[8], 100, "%s", "Dipol (Bending Magnet)");
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      S4E->setEnabled(true);
      S5E->setEnabled(true);
      break;  
    case 'G':
      up0= (struct UndulatorSource0Type *)myparent->myBeamline()->RTSource.Quellep;
      snprintf(TextField[0],  40,  "%f", up0->length);
      snprintf(TextField[1],  40,  "%f", myparent->myBeamline()->BLOptions.lambda* 1e6);    
      snprintf(TextField[2],  40,  "%d", myparent->myBeamline()->RTSource.raynumber);  
      snprintf(TextField[3],  40,  "%f", up0->deltaz);    
      snprintf(TextField[4],  40,  "%f", up0->sigmaey);   
      snprintf(TextField[5],  40,  "%f", up0->sigmaez);    
      snprintf(TextField[6],  40,  "%f", up0->sigmaedy);   
      snprintf(TextField[7],  40,  "%f", up0->sigmaedz); 
      snprintf(LabelField[0], 100, "%s", "length (mm)");
      snprintf(LabelField[1], 100, "%s", "lambda (nm)");  
      snprintf(LabelField[2], 100, "%s", "ray number");  
      snprintf(LabelField[3], 100, "%s", "deltaz (mm)");  
      snprintf(LabelField[4], 100, "%s", "sigmaey (mm)");   
      snprintf(LabelField[5], 100, "%s", "sigmaez (mm)");   
      snprintf(LabelField[6], 100, "%s", "sigmaedy (mrad)");   
      snprintf(LabelField[7], 100, "%s", "sigmaedz (mrad)");
      snprintf(LabelField[8], 100, "%s", "Generic undulator");
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      S4E->setEnabled(true);
      S5E->setEnabled(true);
      S6E->setEnabled(true);
      S7E->setEnabled(true);
      S8E->setEnabled(true);
      break;  
    case 'H':
      hp= (struct HardEdgeSourceType *)myparent->myBeamline()->RTSource.Quellep;
      snprintf(TextField[0],  40,  "%f", hp->disty);
      snprintf(TextField[1],  40,  "%d", hp->iy);    
      snprintf(TextField[2],  40,  "%f", hp->distz);  
      snprintf(TextField[3],  40,  "%d", hp->iz);    
      snprintf(TextField[4],  40,  "%f", hp->divy);   
      snprintf(TextField[5],  40,  "%d", hp->idy);    
      snprintf(TextField[6],  40,  "%f", hp->divz);   
      snprintf(TextField[7],  40,  "%d", hp->idz); 
      snprintf(LabelField[0], 100, "%s", "height (mm)");
      snprintf(LabelField[1], 100, "%s", "-> points");  
      snprintf(LabelField[2], 100, "%s", "width (mm)");  
      snprintf(LabelField[3], 100, "%s", "-> points");  
      snprintf(LabelField[4], 100, "%s", "vert. div. (mrad)");   
      snprintf(LabelField[5], 100, "%s", "-> points");   
      snprintf(LabelField[6], 100, "%s", "hor. div. (mrad)");   
      snprintf(LabelField[7], 100, "%s", "-> points");
      snprintf(LabelField[8], 100, "%s", "Ray Trace hard edge");
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      S4E->setEnabled(true);
      S5E->setEnabled(true);
      S6E->setEnabled(true);
      S7E->setEnabled(true);
      S8E->setEnabled(true);
      break;  
 case 'I':
      psip= (struct PSImageType *)myparent->myBeamline()->RTSource.Quellep;
      snprintf(TextField[0],  40,  "%f", psip->ymin);
      snprintf(TextField[1],  40,  "%f", psip->zmin);    
      snprintf(TextField[2],  40,  "%f", psip->ymax);  
      snprintf(TextField[3],  40,  "%f", psip->zmax);    
      snprintf(TextField[4],  40,  "%d", psip->iy);   
      snprintf(TextField[5],  40,  "%d", psip->iz);    
      snprintf(TextField[6],  40,  "%s", "");   
      snprintf(TextField[7],  40,  "%s", ""); 
      snprintf(LabelField[0], 100, "%s", "ymin (mm)");
      snprintf(LabelField[1], 100, "%s", "zmin (mm)");  
      snprintf(LabelField[2], 100, "%s", "ymax (mm)");  
      snprintf(LabelField[3], 100, "%s", "zmax (mm)");  
      snprintf(LabelField[4], 100, "%s", "y points");   
      snprintf(LabelField[5], 100, "%s", "z points");   
      snprintf(LabelField[6], 100, "%s", "");   
      snprintf(LabelField[7], 100, "%s", "");
      snprintf(LabelField[8], 100, "%s", "PO image plane");
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      S4E->setEnabled(true);
      S5E->setEnabled(true);
      S6E->setEnabled(true);
      break;  

    case 'L':
      up= (struct UndulatorSourceType *)myparent->myBeamline()->RTSource.Quellep;
      snprintf(TextField[0],  40,  "%f", up->length);
      snprintf(TextField[1],  40,  "%f", myparent->myBeamline()->BLOptions.lambda* 1e6);    
      snprintf(TextField[2],  40,  "%d", myparent->myBeamline()->RTSource.raynumber);  
      snprintf(TextField[3],  40,  "%f", up->deltaz);    
      snprintf(TextField[4],  40,  "%s", "");   
      snprintf(TextField[5],  40,  "%s", "");    
      snprintf(TextField[6],  40,  "%s", "");   
      snprintf(TextField[7],  40,  "%s", ""); 
      snprintf(LabelField[0], 100, "%s", "length (mm)");
      snprintf(LabelField[1], 100, "%s", "lambda (nm)");  
      snprintf(LabelField[2], 100, "%s", "ray number");  
      snprintf(LabelField[3], 100, "%s", "deltaz (mm)");  
      snprintf(LabelField[4], 100, "%s", "");   
      snprintf(LabelField[5], 100, "%s", "");   
      snprintf(LabelField[6], 100, "%s", "");   
      snprintf(LabelField[7], 100, "%s", "");
      snprintf(LabelField[8], 100, "%s", "SLS SIS undulator");
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      S4E->setEnabled(true);
      break; 

    case 'M':
      up= (struct UndulatorSourceType *)myparent->myBeamline()->RTSource.Quellep;
      snprintf(TextField[0],  40,  "%f", up->length);
      snprintf(TextField[1],  40,  "%f", myparent->myBeamline()->BLOptions.lambda* 1e6);    
      snprintf(TextField[2],  40,  "%d", myparent->myBeamline()->RTSource.raynumber);  
      snprintf(TextField[3],  40,  "%f", up->deltaz);    
      snprintf(TextField[4],  40,  "%s", "");   
      snprintf(TextField[5],  40,  "%s", "");    
      snprintf(TextField[6],  40,  "%s", "");   
      snprintf(TextField[7],  40,  "%s", ""); 
      snprintf(LabelField[0], 100, "%s", "length (mm)");
      snprintf(LabelField[1], 100, "%s", "lambda (nm)");  
      snprintf(LabelField[2], 100, "%s", "ray number");  
      snprintf(LabelField[3], 100, "%s", "deltaz (mm)");  
      snprintf(LabelField[4], 100, "%s", "");   
      snprintf(LabelField[5], 100, "%s", "");   
      snprintf(LabelField[6], 100, "%s", "");   
      snprintf(LabelField[7], 100, "%s", "");
      snprintf(LabelField[8], 100, "%s", "SLS SIM undulator");
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      S4E->setEnabled(true);
      break;  

    case 'o':
      sop= (struct PointSourceType *)myparent->myBeamline()->RTSource.Quellep;
      snprintf(TextField[0],  40,  "%f", sop->sigy);
      snprintf(TextField[1],  40,  "%f", sop->sigdy);    
      snprintf(TextField[2],  40,  "%f", sop->sigz);  
      snprintf(TextField[3],  40,  "%f", sop->sigdz);    
      snprintf(TextField[4],  40,  "%d", myparent->myBeamline()->RTSource.raynumber);   
      snprintf(TextField[5],  40,  "%s", "");    
      snprintf(TextField[6],  40,  "%s", "");   
      snprintf(TextField[7],  40,  "%s", ""); 
      snprintf(LabelField[0], 100, "%s", "sigy (mm)");
      snprintf(LabelField[1], 100, "%s", "sigdy (mrad)");  
      snprintf(LabelField[2], 100, "%s", "sigz (mm)");  
      snprintf(LabelField[3], 100, "%s", "sigdz (mrad)");  
      snprintf(LabelField[4], 100, "%s", "ray number");   
      snprintf(LabelField[5], 100, "%s", "");   
      snprintf(LabelField[6], 100, "%s", "");   
      snprintf(LabelField[7], 100, "%s", "");
      snprintf(LabelField[8], 100, "%s", "Point Source: all sigma values");
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      S4E->setEnabled(true);
      S5E->setEnabled(true);
      break;  

 case 'R':
      rp= (struct RingSourceType *)myparent->myBeamline()->RTSource.Quellep;
      snprintf(TextField[0],  40,  "%lf", rp->dy);
      snprintf(TextField[1],  40,  "%lf", rp->dz);    
      snprintf(TextField[2],  40,  "%d", myparent->myBeamline()->RTSource.raynumber);  
      snprintf(TextField[3],  40,  "%s", "");    
      snprintf(TextField[4],  40,  "%s", "");   
      snprintf(TextField[5],  40,  "%s", "");    
      snprintf(TextField[6],  40,  "%s", "");   
      snprintf(TextField[7],  40,  "%s", ""); 
      snprintf(LabelField[0], 100, "%s", "dy (mrad)");
      snprintf(LabelField[1], 100, "%s", "dz (mrad)");  
      snprintf(LabelField[2], 100, "%s", "ray number");  
      snprintf(LabelField[3], 100, "%s", "");  
      snprintf(LabelField[4], 100, "%s", "");   
      snprintf(LabelField[5], 100, "%s", "");   
      snprintf(LabelField[6], 100, "%s", "");   
      snprintf(LabelField[7], 100, "%s", "");
      snprintf(LabelField[8], 100, "%s", "Ring Source: half axis of the divergence ellipse, y,z are always 0");
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      break;  
   
    case 'U':
      up= (struct UndulatorSourceType *)myparent->myBeamline()->RTSource.Quellep;
      snprintf(TextField[0],  40,  "%f", up->length);
      snprintf(TextField[1],  40,  "%f", myparent->myBeamline()->BLOptions.lambda* 1e6);    
      snprintf(TextField[2],  40,  "%d", myparent->myBeamline()->RTSource.raynumber);  
      snprintf(TextField[3],  40,  "%s", "");    
      snprintf(TextField[4],  40,  "%s", "");   
      snprintf(TextField[5],  40,  "%s", "");    
      snprintf(TextField[6],  40,  "%s", "");   
      snprintf(TextField[7],  40,  "%s", ""); 
      snprintf(LabelField[0], 100, "%s", "length (mm)");
      snprintf(LabelField[1], 100, "%s", "lambda (nm)");  
      snprintf(LabelField[2], 100, "%s", "ray number");  
      snprintf(LabelField[3], 100, "%s", "");  
      snprintf(LabelField[4], 100, "%s", "");   
      snprintf(LabelField[5], 100, "%s", "");   
      snprintf(LabelField[6], 100, "%s", "");   
      snprintf(LabelField[7], 100, "%s", "");
      snprintf(LabelField[8], 100, "%s", "Undulator");
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      break;  
    case 'F':
      fp= (struct FileSourceType *)myparent->myBeamline()->RTSource.Quellep;
      printf("source from file %s\n", myparent->myPHASEset()->sourceraysname);
      strncpy(fp->filename, myparent->myPHASEset()->sourceraysname, MaxPathLength);
      snprintf(TextField[0],  40,  "%s", "");
      snprintf(TextField[1],  40,  "%s", "");    
      snprintf(TextField[2],  40,  "%s", "");  
      snprintf(TextField[3],  40,  "%s", "");    
      snprintf(TextField[4],  40,  "%s", "");   
      snprintf(TextField[5],  40,  "%s", "");    
      snprintf(TextField[6],  40,  "%s", "");   
      snprintf(TextField[7],  40,  "%s", ""); 
      snprintf(LabelField[0], 100, "%s", "");
      snprintf(LabelField[1], 100, "%s", "");  
      snprintf(LabelField[2], 100, "%s", "");  
      snprintf(LabelField[3], 100, "%s", "");  
      snprintf(LabelField[4], 100, "%s", "");   
      snprintf(LabelField[5], 100, "%s", "");   
      snprintf(LabelField[6], 100, "%s", "");   
      snprintf(LabelField[7], 100, "%s", "");
      snprintf(LabelField[8], 100, "%s", "Source from file");
      break;
    case 'S':
      QMessageBox::warning(this, tr("UpdateSourceBox"),
			   tr("Source type %1 is obsolete.\nenable point source with defaults")
			 .arg(sou));
      myparent->myBeamline()->RTSource.QuellTyp= 'o';
      sop= (struct PointSourceType *)myparent->myBeamline()->RTSource.Quellep;
      snprintf(TextField[0],  40,  "%f", 0.1);
      snprintf(TextField[1],  40,  "%f", 0.1);    
      snprintf(TextField[2],  40,  "%f", 0.1);  
      snprintf(TextField[3],  40,  "%f", 0.1);    
      snprintf(TextField[4],  40,  "%d", 25000);   
      snprintf(TextField[5],  40,  "%s", "");    
      snprintf(TextField[6],  40,  "%s", "");   
      snprintf(TextField[7],  40,  "%s", ""); 
      snprintf(LabelField[0], 100, "%s", "sigy (mm)");
      snprintf(LabelField[1], 100, "%s", "sigdy (mrad)");  
      snprintf(LabelField[2], 100, "%s", "sigz (mm)");  
      snprintf(LabelField[3], 100, "%s", "sigdz (mrad)");  
      snprintf(LabelField[4], 100, "%s", "ray number");   
      snprintf(LabelField[5], 100, "%s", "");   
      snprintf(LabelField[6], 100, "%s", "");   
      snprintf(LabelField[7], 100, "%s", "");
      snprintf(LabelField[8], 100, "%s", "Point Source: all sigma values");
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      S4E->setEnabled(true);
      S5E->setEnabled(true);
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
void MainWindow::UpdateStatistics(Plot *pp, const char *label, int rays)
{
  char buffer[255];
  double trans;

  trans= (myparent->myBeamline()->RTSource.raynumber > 0) ? 
    (double)myparent->myBeamline()->RESULT.points/ 
    (double)myparent->myBeamline()->RTSource.raynumber : -1.0;
  
  snprintf(buffer, 255, "%s Statistics", label);  
  statGroup->setTitle(QString(tr(buffer)));
  snprintf(buffer, 255, "<FONT COLOR=blue>%8.3f</FONT>", pp->cz);  
  czLabel->setText(QString(tr(buffer)));
  snprintf(buffer, 255, "<FONT COLOR=blue>%8.3f</FONT>", pp->cy);  
  cyLabel->setText(QString(tr(buffer)));
  snprintf(buffer, 255, "<FONT COLOR=blue>%9.4f</FONT>", pp->wz);  
  wzLabel->setText(QString(tr(buffer)));
  snprintf(buffer, 255, "<FONT COLOR=blue>%9.4f</FONT>", pp->wy);  
  wyLabel->setText(QString(tr(buffer)));
  snprintf(buffer, 255, "<FONT COLOR=blue>%8.3f</FONT>", pp->cdz * 1e3);  
  cdzLabel->setText(QString(tr(buffer)));
  snprintf(buffer, 255, "<FONT COLOR=blue>%8.3f</FONT>", pp->cdy * 1e3);  
  cdyLabel->setText(QString(tr(buffer)));
  snprintf(buffer, 255, "<FONT COLOR=blue>%8.3f</FONT>", pp->wdz * 1e3);  
  wdzLabel->setText(QString(tr(buffer)));
  snprintf(buffer, 255, "<FONT COLOR=blue>%8.3f</FONT>", pp->wdy * 1e3);  
  wdyLabel->setText(QString(tr(buffer)));
  snprintf(buffer, 255, "<FONT COLOR=blue>%8d</FONT>", rays);  
  rayLabel->setText(QString(tr(buffer)));
  snprintf(buffer, 255, "<FONT COLOR=blue>%8.3f</FONT>", trans);  
  traLabel->setText(QString(tr(buffer)));
  snprintf(buffer, 255, "<FONT COLOR=blue>%8.3f</FONT>", pp->ry);  
  ryLabel->setText(QString(tr(buffer)));
  snprintf(buffer, 255, "<FONT COLOR=blue>%8.3f</FONT>", pp->rz);  
  rzLabel->setText(QString(tr(buffer)));

  if (pp->fwhmon)
    {
      wzLabel0->setText(QString(tr("z FWHM (mm)")));
      wyLabel0->setText(QString(tr("y FWHM (mm)")));
      wdzLabel0->setText(QString(tr("dz FWHM (mm)")));
      wdyLabel0->setText(QString(tr("dy FWHM (mm)")));
    } else
    {
      wzLabel0->setText(QString(tr("z RMS (mm)")));
      wyLabel0->setText(QString(tr("y RMS (mm)")));
      wdzLabel0->setText(QString(tr("dz RMS (mm)")));
      wdyLabel0->setText(QString(tr("dy RMS (mm)")));
    }
} // UpdateStatistics

void MainWindow::UpdateStatus()
{
  int elementnumber= elementList->currentRow();
  char buffer[100];

  //  printf("UpdateStatus: element: %d, of %d\n", elementnumber, elementList->count());

  if ((elementnumber < 0) || (elementnumber > elementList->count()- 1)) 
    elementnumber= 0;

  if (myparent->myBeamline()->elementzahl > 0)
    {
      if (myparent->myBeamline()->ElementList[elementnumber].ElementOK & elementOK) 
	snprintf(buffer, 100, "<b><FONT COLOR=green>OE_%d</FONT></b>", elementnumber+1); 
      else 
	snprintf(buffer, 100, "<b><FONT COLOR=red>OE_%d</FONT></b>", elementnumber+1); 
    }
  else
    snprintf(buffer, 100, "<b><FONT COLOR=red>OE_X</FONT></b>");

  elementStatLabel->setText(QString(tr(buffer)));

  if (myparent->myBeamline()->beamlineOK & sourceOK) 
    sourceStatLabel->setText(QString(tr("<b><FONT COLOR=green>source</FONT></b>"))); 
  else 
    sourceStatLabel->setText(QString(tr("<b><FONT COLOR=red>source</FONT></b>")));

  if (myparent->myBeamline()->beamlineOK & resultOK) 
    imageStatLabel->setText(QString(tr("<b><FONT COLOR=green>image</FONT></b>"))); 
  else 
    imageStatLabel->setText(QString(tr("<b><FONT COLOR=red>image</FONT></b>")));

  if (myparent->myBeamline()->beamlineOK & mapOK) 
    mapStatLabel->setText(QString(tr("<b><FONT COLOR=green>maps</FONT></b>"))); 
  else 
    mapStatLabel->setText(QString(tr("<b><FONT COLOR=red>maps</FONT></b>")));
} // UpdateStatus


int MainWindow::elementListIsEmpty()
{
  int rval= 0;
  if (elementList->count() <= 0)
    {
      QMessageBox::warning(this, tr("No valid Elements!"),
			   tr("add optical elements to the list!"));
      rval= 1;
    }
  return rval;
}

int MainWindow::elementListNotSelected()
{
  int rval= 0;
  if (elementList->currentRow() < 0)
    {
      QMessageBox::warning(this, tr("No valid Element!"),
			   tr("(nothing selected)"));
      rval= 1;
    }
  return rval;
}

// change labels of Graphics input
void MainWindow::updateGraphicsInput(int style)
{
     
  if (style & PLOT_GO_DIV)
    {   
      zminLabel->setText(QString(tr("dzmin (mrad)"))); 
      zmaxLabel->setText(QString(tr("dzmax (mrad)")));
      yminLabel->setText(QString(tr("dymin (mrad)"))); 
      ymaxLabel->setText(QString(tr("dymax (mrad)")));
    } 
  else
    {
      zminLabel->setText(QString(tr("zmin (mm)"))); 
      zmaxLabel->setText(QString(tr("zmax (mm)")));
      yminLabel->setText(QString(tr("ymin (mm)"))); 
      ymaxLabel->setText(QString(tr("ymax (mm)")));
    }
  
} // updateGraphicsInput


// write files simpre.dat and simpim.dat
void MainWindow::writeSimp()
{
  int ret, i, idx, idy, j, k;
  FILE *f1, *f2;
  
  struct PSDType    *psd; 

#ifdef DEBUG
  cout << "debug: writeSimp called" << endl;
  cout << "debug: result type: " << myparent->myBeamline()->RESULT.typ << endl;
#endif

  psd= (struct PSDType *)myparent->myBeamline()->RESULT.RESp;

  char simprefilename[]= "simpre.dat";
  char simpimfilename[]= "simpim.dat";

  if (fexists(simprefilename) || fexists(simpimfilename))
    {
      QMessageBox *msgBox = new QMessageBox;
      msgBox->setText(tr("file simpre.dat and/or simpim.dat exists!"));
      msgBox->setInformativeText(tr("replace file(s)"));
      msgBox->setStandardButtons(QMessageBox::Ok | QMessageBox::Cancel); 
      ret= msgBox->exec();
      delete msgBox;
      if (ret == QMessageBox::Cancel)
	{ 
	  cout << "writeSimp canceled" << endl;
	  return;
	}
    }
  
  if ((f1= fopen(simprefilename, "w+")) == NULL)
    {
      cout << "cant open " << simprefilename<< endl;
      return;
    }

  if ((f2= fopen(simpimfilename, "w+")) == NULL)
    {
      cout << "cant open " << simpimfilename<< endl;
      return;
    }

  // header
  fprintf(f1, "# file simpre.dat written by phase\n");
  fprintf(f2, "# file simpim.dat written by phase\n");

  // psd(i,j,k) in fortran memory model
  for (k= 0; k < 512; k++)
    {
      // idx= k + 0 * 4096+ i * (4096 * 2)
      // idy= k + 1 * 4096+ i * (4096 * 2)
      fprintf(f1, "%d %f\n", k, psd->simpre[i]);
      fprintf(f2, "%d %f\n", k, psd->simpim[i]);
    }
  fprintf(f1, "# end\n");
  fprintf(f2, "# end\n");
  // add code to write the data

  fclose(f1);
  fclose(f2);

  //  count << "return: " << ret << endl; 
  cout << "writeSimp done" << endl; 
} // end writeSimp

/////////////////////////////////
// end widget handling sctieon //
/////////////////////////////////

// /afs/psi.ch/user/f/flechsig/phase/src/qtgui/mainwindow.cpp
