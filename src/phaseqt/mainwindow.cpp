//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/mainwindow.cpp
//  Date      : <31 May 11 17:02:14 flechsig> 
//  Time-stamp: <19 Jul 12 13:59:53 flechsig> 
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
  this->s_ray= NULL;
  this->o_input= NULL;
  this->c_window= NULL;
  this->m4p_cpp= NULL;
  this->csp_cpp= NULL;
  this->zone= NULL;
  graphicBox= createGraphicBox();
  setCentralWidget(graphicBox);
  createActions();
  createMenus();
  createToolBars();
  createStatusBar();
  createDockWindows();
  createProgress();
  setWindowTitle(tr("PHASE Qt"));
  resize(1400,940);
  // progress etc.
  mwplotsubject= PLOT_GO_RESULT | PLOT_GO_SPA;
  mwplotstyle= PLOT_CONTOUR;

  
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
  //  int myret= (rp->typ == typ) ? 1 : 0;
  int myret= (rp->typ & typ) ? 1 : 0;

  if (!myret)
    QMessageBox::warning(this, tr("checkResultType"),
			 tr("Available calculation results incompatible with selected plotsubject!<p>info: points1= %1, type= %2, requested type= %3").arg(rp->points1).arg(rp->typ).arg(typ));

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
    printAct->setStatusTip(tr("Print the graphics output"));
    connect(printAct, SIGNAL(triggered()), this, SLOT(print()));

    printMainAct = new QAction(QIcon(":/images/print.png"), tr("Print Mainwindow..."), this);
    //printMainAct->setShortcuts(QKeySequence::PrintMain);
    printMainAct->setStatusTip(tr("Print the PHASE Qt window"));
    connect(printMainAct, SIGNAL(triggered()), this, SLOT(printMain()));

    screenshotAct = new QAction(QIcon(":/images/print.png"), tr("Screenshot Mainwindow..."), this);
    screenshotAct->setStatusTip(tr("Create a screenshot of PHASE Qt window"));
    connect(screenshotAct, SIGNAL(triggered()), this, SLOT(screenshotMain()));

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

    asynMapAct = new QAction(tr("make maps in parallel (test)"), this);
    asynMapAct->setStatusTip(tr("make maps in parallel (test)"));
    signalMapper->setMapping(asynMapAct, QString("asynMapAct"));
    connect(asynMapAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    asynPOAct = new QAction(QIcon(":/images/Sunrise-icon.png"),tr("make PO in parallel (experimental)"), this);
    asynPOAct->setStatusTip(tr("make PO in parallel (experimental)"));
    signalMapper->setMapping(asynPOAct, QString("asynPOAct"));
    connect(asynPOAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    normPOAct = new QAction(tr("norm PO (test)"), this);
    normPOAct->setStatusTip(tr("norm PO in parallel (test)"));
    signalMapper->setMapping(normPOAct, QString("normPOAct"));
    connect(normPOAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    asynTestAct = new QAction(tr("asyn Test"), this);
    asynTestAct->setStatusTip(tr("asyn Test"));
    signalMapper->setMapping(asynTestAct, QString("asynTestAct"));
    connect(asynTestAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    optiInputAct = new QAction(tr("&optimization input"), this);
    optiInputAct->setStatusTip(tr("optimization input"));
    signalMapper->setMapping(optiInputAct, QString("optiInputAct"));
    connect(optiInputAct, SIGNAL(triggered()), signalMapper, SLOT(map()));

    phasespaceAct = new QAction(QIcon(":/images/physical-optics.png"),tr("PO imaging"), this);
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

    lambdaE  = new QLineEdit;
    dlambdaE = new QLineEdit;
    dislenE  = new QLineEdit;

    QLabel *lambdaLabel  = new QLabel(tr("lambda (nm)"));
    QLabel *dlambdaLabel = new QLabel(tr("dla (nm)"));
    QLabel *dislenLabel  = new QLabel(tr("dispersive length (mm)"));
    
    beamlineGenericLayout->addWidget(lambdaLabel,  0, 0);
    beamlineGenericLayout->addWidget(lambdaE,      0, 1);
    beamlineGenericLayout->addWidget(dlambdaLabel, 0, 2);
    beamlineGenericLayout->addWidget(dlambdaE,     0, 3);
    beamlineGenericLayout->addWidget(dislenLabel,  1, 0, 1, 2);
    beamlineGenericLayout->addWidget(dislenE,      1, 3, 1, 1);
    
    beamlineGenericGroup->setLayout(beamlineGenericLayout);

// bottom part
    QGroupBox   *beamlineCalcGroup  = new QGroupBox(tr("calculation parameters"));
    //QVBoxLayout *beamlineCalcLayout = new QVBoxLayout;
    QGridLayout *beamlineCalcLayout = new QGridLayout;

    goButton = new QRadioButton(tr("&geometrical optic (GO)"));
    poButton = new QRadioButton(tr("&physical optic (PO)"));
    goButton->setChecked(true);
    misaliBox   = new QCheckBox(tr("with misalignment"));
    dlambdaBox  = new QCheckBox(tr("dla mode"));
    dlambdaBox1 = new QCheckBox(tr("la"));
    dlambdaBox2 = new QCheckBox(tr("la+dla"));

    beamlineCalcLayout->addWidget(goButton,  0, 0);
    beamlineCalcLayout->addWidget(poButton,  1, 0);
    beamlineCalcLayout->addWidget(misaliBox, 2, 0);
    beamlineCalcLayout->addWidget(dlambdaBox, 0, 1);
    beamlineCalcLayout->addWidget(dlambdaBox1, 1, 1);
    beamlineCalcLayout->addWidget(dlambdaBox2, 2, 1);
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
    connect(dlambdaE, SIGNAL(editingFinished()), this, SLOT(dlambdaSlot()));
    connect(dislenE, SIGNAL(editingFinished()), this, SLOT(dislenSlot()));
    connect(goButton, SIGNAL(clicked()), this, SLOT(goButtonslot()));
    connect(poButton, SIGNAL(clicked()), this, SLOT(poButtonslot()));
    connect(misaliBox, SIGNAL(stateChanged(int)), this, SLOT(misaliBoxslot(int)));
    connect(dlambdaBox, SIGNAL(stateChanged(int)), this, SLOT(dlambdaBoxslot(int)));
    connect(dlambdaBox1, SIGNAL(stateChanged(int)), this, SLOT(dlambdaBox1slot(int)));
    connect(dlambdaBox2, SIGNAL(stateChanged(int)), this, SLOT(dlambdaBox2slot(int)));
    return beamlineBox;
} // end createbeamline box


// dock widgets
void MainWindow::createDockWindows()
{
  // redefines the corners of the dockwidget to have the top widget in the center only
  setCorner(Qt::TopLeftCorner,     Qt::LeftDockWidgetArea);
  setCorner(Qt::BottomLeftCorner,  Qt::LeftDockWidgetArea);
  setCorner(Qt::TopRightCorner,    Qt::RightDockWidgetArea);
  setCorner(Qt::BottomRightCorner, Qt::RightDockWidgetArea);

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
  
  // plot box
  dock = new QDockWidget(tr("Plot Box"), this);
  dock->setWidget(createPlotBox());
  addDockWidget(Qt::TopDockWidgetArea, dock);
  viewMenu->addAction(dock->toggleViewAction());
} // createDockwindows


// graphic box
QWidget *MainWindow::createGraphicBox()
{
  graphicBox = new QWidget();

  // upper part (status)
  statusGroup  = new QGroupBox(tr("Status"));
  QHBoxLayout *statusLayout = new QHBoxLayout;
  sourceStatLabel   = new QLabel(tr("<b><FONT COLOR=red>source</FONT></b>"));
  imageStatLabel    = new QLabel(tr("<b><FONT COLOR=red>image</FONT></b>"));
  mapStatLabel      = new QLabel(tr("<b><FONT COLOR=red>maps</FONT></b>"));
  elementStatLabel  = new QLabel(tr("<b><FONT COLOR=red>OE-X</FONT></b>"));
  poimageStatLabel  = new QLabel(tr("<b><FONT COLOR=red>POimpl</FONT></b>"));
  statusLayout->addWidget(mapStatLabel);
  statusLayout->addWidget(elementStatLabel);
  statusLayout->addWidget(sourceStatLabel);
  statusLayout->addWidget(poimageStatLabel);
  statusLayout->addWidget(imageStatLabel);
  statusGroup->setLayout(statusLayout);

  // scaling area
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

  connect(grscatterAct,    SIGNAL(triggered()), grsignalMapper, SLOT(map()));
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
  grPoSimpreAct  = new QAction(tr("PO Simpre"), this);
  grPoSimpimAct  = new QAction(tr("PO Simpim"), this);
  grPoSintreAct  = new QAction(tr("PO Sintre"), this);
  grPoSintimAct  = new QAction(tr("PO Sintim"), this);

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
  subject->addAction(grPoSimpreAct);
  subject->addAction(grPoSimpimAct);
  subject->addAction(grPoSintreAct);
  subject->addAction(grPoSintimAct);
  subject->addSeparator();
  subject->addAction(grexample1Act);
  subject->addAction(grexample2Act);
  subject->addAction(grexample3Act);
  subject->setDefaultAction(grGoResultSpaAct);
  subjectpopupButton->setMenu(subject);

  connect(grGoSourceSpaAct, SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grGoSourceDivAct, SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grGoSourcePhiAct, SIGNAL(triggered()), grsignalMapper, SLOT(map()));

  connect(grGoResultSpaAct, SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grGoResultDivAct, SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grGoResultPhiAct, SIGNAL(triggered()), grsignalMapper, SLOT(map()));

  connect(grPoResultAct,   SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grPoSimpreAct,   SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grPoSimpimAct,   SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grPoSintreAct,   SIGNAL(triggered()), grsignalMapper, SLOT(map()));
  connect(grPoSintimAct,   SIGNAL(triggered()), grsignalMapper, SLOT(map()));

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
  grsignalMapper->setMapping(grPoSintreAct,   QString("grPoSintreAct"));
  grsignalMapper->setMapping(grPoSintimAct,   QString("grPoSintimAct"));
  grsignalMapper->setMapping(grPoSimpreAct,   QString("grPoSimpreAct"));
  grsignalMapper->setMapping(grPoSimpimAct,   QString("grPoSimpimAct"));

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
  // end scaling area
  /*
  // the plot box
  QGroupBox   *plotGroup  = new QGroupBox(tr("plot"));
  QGridLayout *plotGroupLayout = new QGridLayout(plotGroup);
  QToolButton* btnLogy = new QToolButton(plotGroup);
  btnLogy->setText("Log Scale");
  btnLogy->setCheckable(true);
  
  // der eigentliche plot
  d_plot = new Plot(this);
  d_plot->setAxisTitle(2, tr("z (mm)"));
  d_plot->setAxisTitle(0, tr("y (mm)"));
  d_plot->setTitle(tr("PhaseQt"));

  connect(btnLogy, SIGNAL(toggled(bool)), d_plot, SLOT(SetLog(bool)));
  plotGroupLayout->addWidget(d_plot,2,1,3,3);
  plotGroupLayout->setMargin(12);
  // end of plot box
  */
  // statistics
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
  //vbox->addWidget(plotGroup);
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
    fileMenu->addAction(printMainAct);
    fileMenu->addAction(screenshotAct);
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
    calcMenu->addSeparator();
    calcMenu->addAction(asynMapAct);
    calcMenu->addAction(asynPOAct);
    calcMenu->addAction(normPOAct);
    calcMenu->addAction(asynTestAct);
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
  QLabel *lambdagLabel  = new QLabel(tr("lambda_g (nm)"));    
  lambdagE  = new QLineEdit;
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
//  QHBoxLayout *gratingLayout1 = new QHBoxLayout;
  QGridLayout *gratingLayout1 = new QGridLayout;
  
  //gratingLayout1->addWidget(lambdagLabel, 0, 0);
  //gratingLayout1->addWidget(lambdagE, 0, 1);
  //lambdagE->setEnabled(false);  // UF should go away
  gratingLayout1->addWidget(densityLabel, 0, 0);  
  gratingLayout1->addWidget(lineDensity,  0, 1);
  gratingLayout1->addWidget(orderLabel,   0, 3);
  gratingLayout1->addWidget(integerSpinBox, 0, 4);
//  gratingLayout1->addStretch(1);
  gratingLayout1->addWidget(nimBox, 0, 2);  
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
 QLabel *dRuLabel = new QLabel(tr("dRu (mrad)"));
 QLabel *dRwLabel = new QLabel(tr("dRw (mrad)"));
 QLabel *dRlLabel = new QLabel(tr("dRl (mrad)"));
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
  QString qst;
  //char buffer[50];

  int i;

#ifdef DEBUG
  cout << "debug: " << __FILE__ << " createParameterBox called" << endl;
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
      //snprintf(buffer, 4, "%d : parameter",  i);
      qst.setNum(i);
      qst+= QString(" : parameter");
      //   item= new QListWidgetItem(buffer);
      item= new QListWidgetItem(qst);
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

// the progress widget
void MainWindow::createProgress()
{
  myProgressDialog= new QWidget();
  progressLabel   = new QLabel();
  myProgressDialog->setWindowTitle(tr("Progress of asynchronous task"));
  progressLabel->setText(QString("Progressing using %1 thread(s)...").arg(QThread::idealThreadCount()));
  progressPauseButton  = new QPushButton("Pause");
  progressResumeButton = new QPushButton("Resume");
  progressAbortButton  = new QPushButton("Abort");
  dialogProgressBar    = new QProgressBar();
  QGridLayout *layout  = new QGridLayout;
  
  layout->addWidget(progressLabel,       1, 1, 1, 3);
  layout->addWidget(progressAbortButton, 3, 3);
  layout->addWidget(progressPauseButton, 3, 1);
  layout->addWidget(progressResumeButton,3, 2);
  layout->addWidget(dialogProgressBar,   2, 1, 1, 3);
  
  myProgressDialog->setLayout(layout);

  future  = new QFuture<void>;
  watcher = new QFutureWatcher<void>;

  //connect(myProgressDialog, SIGNAL(canceled()), watcher, SLOT(cancel()));
  //connect(watcher, SIGNAL(finished()), myProgressDialog, SLOT(reset()));
  connect(watcher, SIGNAL(finished()), myProgressDialog, SLOT(close()));
  connect(watcher, SIGNAL(finished()), this, SLOT(finished_thread()));
  connect(watcher, SIGNAL(started()),  myProgressDialog, SLOT(show()));
  connect(watcher, SIGNAL(progressRangeChanged(int,int)), dialogProgressBar, SLOT(setRange(int,int)));
  connect(watcher, SIGNAL(progressValueChanged(int)),     dialogProgressBar, SLOT(setValue(int)));
  connect(progressPauseButton,  SIGNAL(clicked()), this, SLOT(pause_thread()));
  connect(progressResumeButton, SIGNAL(clicked()), this, SLOT(resume_thread()));
  connect(progressAbortButton,  SIGNAL(clicked()), watcher, SLOT(cancel()));

  for (int i = 0; i < 1000; ++i)          // for testing submit 1000 tasks
    vector.append(i);

  dialogProgressBar->setMaximum(vector.size());

  //myProgressDialog->show();
} // createProgress()


// the plot box
QWidget *MainWindow::createPlotBox()
{
  plotBox = new QWidget();
  plotLayout = new QGridLayout;
   
  d_plot = new Plot(plotBox);
  //zone   = new PlotMatrix( 2  , 2 );
  d_plot->setAxisTitle(2, tr("z (mm)"));
  d_plot->setAxisTitle(0, tr("y (mm)"));
  d_plot->setTitle(tr("PhaseQt"));

  btnLogy = new QToolButton();
  btnLogy->setText("Log Scale");
  btnLogy->setCheckable(true);
  connect(btnLogy, SIGNAL(toggled(bool)), d_plot, SLOT(SetLog(bool)));

  plotLayout->addWidget(btnLogy,0,0, Qt::AlignTop);
  plotLayout->addWidget(d_plot,1,0,3,3);
  btnLogy->hide();
  //plotLayout->addWidget(d_plot,0,0);
  //plotLayout->addWidget(zone,1,0);
  plotBox->setLayout(plotLayout);
  return plotBox;
} // end createPlotBox

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
  sourceAutoGuessB = new QPushButton(tr("Autorange"));

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

  sourceParsLayout->addWidget(sourceApplyB,     3, 4);
  sourceParsLayout->addWidget(sourceDefaultB,   2, 4);
  sourceParsLayout->addWidget(sourceAutoGuessB,  0, 4);
  
  sourceParsGroup->setLayout(sourceParsLayout);

  QVBoxLayout *vbox = new QVBoxLayout;
  vbox->addWidget(sourceTypeGroup);
  vbox->addWidget(sourceParsGroup);

  sourceBox->setLayout(vbox);
  connect(sourceApplyB,   SIGNAL(clicked()), this, SLOT(sourceApplyBslot()));
  connect(sourceDefaultB, SIGNAL(clicked()), this, SLOT(sourceDefaultBslot()));
  connect(sourceAutoGuessB, SIGNAL(clicked()), this, SLOT(sourceAutoGuessBslot()));

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
    fileToolBar->addAction(printMainAct);
    fileToolBar->addAction(screenshotAct);

    editToolBar = addToolBar(tr("Edit"));
    //    editToolBar->addAction(undoAct);
    editToolBar->addAction(footprintAct);
    editToolBar->addAction(raytracesimpleAct);
    editToolBar->addAction(raytracefullAct);
    editToolBar->addAction(phasespaceAct);
    editToolBar->addAction(asynPOAct);
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
  
  int scanned= 1;
  QString qst;
 
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
      qst.setNum(op->epsilon, 'g', 5);
      break;
    case 1:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.iord);
      printf("parameterUpdate: scanned_pos: %d\n", scanned);
      if ((scanned == EOF) || (scanned == 0) || (op->ifl.iord < 1) || 
	  (op->ifl.iord > 7)) op->ifl.iord= 4;             // set default
      qst.setNum(op->ifl.iord);
      break;
    case 2:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.iordsc);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.iordsc= 4; // default
      qst.setNum(op->ifl.iordsc); 
      break;
    case 3:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.iexpand);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.iexpand= 0;   // default
      qst.setNum(op->ifl.iexpand);
      break;
    case 4:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.iplmode);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.iplmode= 0;   // default
      qst.setNum(op->ifl.iplmode);
      break;
    case 5:
      if (!init) scanned= sscanf(text, "%d", &(myparent->myBeamline()->src.isrctype));
      if ((scanned == EOF) || (scanned == 0)) myparent->myBeamline()->src.isrctype= 0;   // default
      qst.setNum(myparent->myBeamline()->src.isrctype);
      break; 
    case 6:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.rpin);
      if ((scanned == EOF) || (scanned == 0)) op->apr.rpin= 0;   // default
      qst.setNum(op->apr.rpin, 'g', 4);
      break;
    case 7:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.srcymin);
      if ((scanned == EOF) || (scanned == 0)) op->apr.srcymin= 0;   // default
      qst.setNum(op->apr.srcymin, 'g', 4);
      break;
    case 8:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.srcymax);
      if ((scanned == EOF) || (scanned == 0)) op->apr.srcymax= 0;   // default
      qst.setNum(op->apr.srcymax, 'g', 4);
      break;
    case 9:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.srczmin);
      if ((scanned == EOF) || (scanned == 0)) op->apr.srczmin= 0;   // default
      qst.setNum(op->apr.srczmin, 'g', 4);
      break;
    case 10:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.srczmax);
      if ((scanned == EOF) || (scanned == 0)) op->apr.srczmax= 0;   // default
      qst.setNum(op->apr.srczmax, 'g', 4);
      break;
    case 11:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.rpin_ap);
      if ((scanned == EOF) || (scanned == 0)) op->apr.rpin_ap= 0;   // default
      qst.setNum(op->apr.rpin_ap, 'g', 4);
      break;
    case 12:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.ymin_ap);
      if ((scanned == EOF) || (scanned == 0)) op->apr.ymin_ap= 0;   // default
      qst.setNum(op->apr.ymin_ap, 'g', 4);
      break;
    case 13:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.ymax_ap);
      if ((scanned == EOF) || (scanned == 0)) op->apr.ymax_ap= 0;   // default
      qst.setNum(op->apr.ymax_ap, 'g', 4);
      break;
    case 14:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.zmin_ap);
      if ((scanned == EOF) || (scanned == 0)) op->apr.zmin_ap= 0;   // default
      qst.setNum(op->apr.zmin_ap, 'g', 4);
      break;
    case 15:
      if (!init) scanned= sscanf(text, "%lg", &op->apr.zmax_ap);
      if ((scanned == EOF) || (scanned == 0)) op->apr.zmax_ap= 0;   // default
      qst.setNum(op->apr.zmax_ap, 'g', 4);
      break;

    case 16:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->so5.dipcy);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so5.dipcy= 0;   // default
      qst.setNum(mysrc->so5.dipcy, 'g', 4);
      break;
    case 17:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->so5.dipcz);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so5.dipcz= 0;   // default
      qst.setNum(mysrc->so5.dipcz, 'g', 4);
      break;
    case 18:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->so5.dipdisy);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so5.dipdisy= 0;   // default
      qst.setNum(mysrc->so5.dipdisy, 'g', 4);
      break;
    case 19:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->so5.dipdisz);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so5.dipdisz= 0;   // default
      qst.setNum(mysrc->so5.dipdisz, 'g', 4);
      break;

    case 20:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.inorm);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.inorm= 0;   // default
      qst.setNum(op->ifl.inorm);
      break;
    case 21:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.inorm1);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.inorm1= 0;   // default
      qst.setNum(op->ifl.inorm1);
      break;
    case 22:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.inorm2);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.inorm2= 0;   // default
      qst.setNum(op->ifl.inorm2);
      break;
    case 23:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.matrel);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.matrel= 0;   // default
      qst.setNum(op->ifl.matrel);
      break;

    case 24:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so1.isrcy);
      if ((scanned == EOF) || (scanned == 0))  mysrc->so1.isrcy= 0;   // default
      qst.setNum(mysrc->so1.isrcy);
      break;
    case 25:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so1.isrcdy);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so1.isrcdy= 0;   // default
      qst.setNum(mysrc->so1.isrcdy);
      break;
    case 26:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->so1.sigmay);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so1.sigmay= 0;   // default
      qst.setNum(mysrc->so1.sigmay, 'g', 4);
      break;
    case 27:
      if (!init) 
	{ 
	  scanned= sscanf(text, "%lg", &mysrc->so1.sigmayp);
	  mysrc->so1.sigmayp*= 1e-3;
	}
      if ((scanned == EOF) || (scanned == 0)) mysrc->so1.sigmayp= 0;   // default
      qst.setNum(mysrc->so1.sigmayp*1e3,  'g', 4);
      break;

    case 28:
      if (!init) 
	{
	  scanned= sscanf(text, "%lg", &op->xi.ymin);
	  op->xi.ymin*= 1e-3;
	}
      if ((scanned == EOF) || (scanned == 0)) op->xi.ymin= 0;   // default
      qst.setNum(op->xi.ymin* 1e3, 'g', 4);
      break;
    case 29:
      if (!init)
	{
	  scanned= sscanf(text, "%lg", &op->xi.ymax);
	  op->xi.ymax*= 1e-3;
	}
      if ((scanned == EOF) || (scanned == 0)) op->xi.ymax= 0;   // default
      qst.setNum(op->xi.ymax* 1e3, 'g', 4);
      break;
    case 30:
      if (!init) scanned= sscanf(text, "%d", &op->xi.ianzy0);
      if ((scanned == EOF) || (scanned == 0)) op->xi.ianzy0= 0;   // default
      qst.setNum(op->xi.ianzy0);
      break;

    case 31:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so1.isrcz);
      if ((scanned == EOF) || (scanned == 0))  mysrc->so1.isrcz= 0;   // default
      qst.setNum(mysrc->so1.isrcz);
      break;
    case 32:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so1.isrcdz);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so1.isrcdz= 0;   // default
      qst.setNum(mysrc->so1.isrcdz);
      break;
    case 33:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->so1.sigmaz);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so1.sigmaz= 0;   // default
      qst.setNum(mysrc->so1.sigmaz, 'g', 4);
      break;
    case 34:
      if (!init)
	{
	  scanned= sscanf(text, "%lg", &mysrc->so1.sigmazp);
	  mysrc->so1.sigmazp*= 1e-3;
	}
      if ((scanned == EOF) || (scanned == 0)) mysrc->so1.sigmazp= 0;   // default
      qst.setNum(mysrc->so1.sigmazp*1e3, 'g', 4);
      break;

    case 35:
      if (!init)
	{
	  scanned= sscanf(text, "%lg", &op->xi.zmin);
	  op->xi.zmin*=1e-3;
	}
      if ((scanned == EOF) || (scanned == 0)) op->xi.zmin= 0;   // default
      qst.setNum(op->xi.zmin* 1e3, 'g', 4);
      break;
    case 36:
      if (!init) 
	{
	  scanned= sscanf(text, "%lg", &op->xi.zmax);
	  op->xi.zmax*= 1e-3;
	}
      if ((scanned == EOF) || (scanned == 0)) op->xi.zmax= 0;   // default
      qst.setNum(op->xi.zmax* 1e3, 'g', 4);
      break;
    case 37:
      if (!init) scanned= sscanf(text, "%d", &op->xi.ianzz0);
      if ((scanned == EOF) || (scanned == 0)) op->xi.ianzz0= 0;   // default
      qst.setNum(op->xi.ianzz0);
      break;

    case 38:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.ibright);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.ibright= 0;   // default
      qst.setNum(op->ifl.ibright);
      break;

    case 39:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.ispline);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.ispline= 0;   // default
      qst.setNum(op->ifl.ispline);
      break;

    case 40:
      if (!init) scanned= sscanf(text, "%lg", &op->xi.d12_max);
      if ((scanned == EOF) || (scanned == 0)) op->xi.d12_max= 0;   // default
      qst.setNum(op->xi.d12_max, 'g', 4);
      break;
    case 41:
      if (!init) scanned= sscanf(text, "%d", &op->xi.id12);
      if ((scanned == EOF) || (scanned == 0)) op->xi.id12= 0;   // default
      qst.setNum(op->xi.id12);
      break;
    case 42:
      if (!init) scanned= sscanf(text, "%d", &op->xi.ianz0_cal);
      if ((scanned == EOF) || (scanned == 0)) op->xi.ianz0_cal= 0;   // default
      qst.setNum(op->xi.ianz0_cal);
      break;
    case 43:
      if (!init) scanned= sscanf(text, "%d", &op->xi.ianz0_fixed);
      if ((scanned == EOF) || (scanned == 0)) op->xi.ianz0_fixed= 0;   // default
      qst.setNum(op->xi.ianz0_fixed);
      break;
    case 44:
      if (!init) scanned= sscanf(text, "%d", &op->xi.iamp_smooth);
      if ((scanned == EOF) || (scanned == 0)) op->xi.iamp_smooth= 0;   // default
      qst.setNum(op->xi.iamp_smooth);
      break;
    case 45:
      if (!init) scanned= sscanf(text, "%d", &op->xi.iord_amp);
      if ((scanned == EOF) || (scanned == 0)) op->xi.iord_amp= 0;   // default
      qst.setNum(op->xi.iord_amp);
      break;
    case 46:
      if (!init) scanned= sscanf(text, "%d", &op->xi.ifm_amp);
      if ((scanned == EOF) || (scanned == 0)) op->xi.ifm_amp= 0;   // default
      qst.setNum(op->xi.ifm_amp);
      break;
      
    case 47:
      if (!init) scanned= sscanf(text, "%d", &op->xi.iord_pha);
      if ((scanned == EOF) || (scanned == 0)) op->xi.iord_pha= 0;   // default
      qst.setNum(op->xi.iord_pha);
      break;
      
    case 48:
      if (!init) scanned= sscanf(text, "%d", &op->xi.ifm_pha);
      if ((scanned == EOF) || (scanned == 0)) op->xi.ifm_pha= 0;   // default
      qst.setNum(op->xi.ifm_pha);
      break;
    case 49:
      if (!init) scanned= sscanf(text, "%lg", &op->xi.distfocy);
      if ((scanned == EOF) || (scanned == 0)) op->xi.distfocy= 0;   // default
      qst.setNum(op->xi.distfocy, 'g', 4);
      break;
    case 50:
      if (!init) scanned= sscanf(text, "%lg", &op->xi.distfocz);
      if ((scanned == EOF) || (scanned == 0)) op->xi.distfocz= 0;   // default
      qst.setNum(op->xi.distfocz, 'g', 4);
      break;
    case 51:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.ipinarr);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.ipinarr= 0;   // default
      qst.setNum(op->ifl.ipinarr);
      break;    
    case 52:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->pin_yl0);
      if ((scanned == EOF) || (scanned == 0)) mysrc->pin_yl0= 0;   // default
      qst.setNum(mysrc->pin_yl0, 'g', 4);
      break;
    case 53:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->pin_yl);
      if ((scanned == EOF) || (scanned == 0)) mysrc->pin_yl= 0;   // default
      qst.setNum(mysrc->pin_yl, 'g', 4);
      break;
    case 54:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->pin_zl0);
      if ((scanned == EOF) || (scanned == 0)) mysrc->pin_zl0= 0;   // default
      qst.setNum(mysrc->pin_zl0, 'g', 4);
      break;
    case 55:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->pin_zl);
      if ((scanned == EOF) || (scanned == 0)) mysrc->pin_zl= 0;   // default
      qst.setNum(mysrc->pin_zl, 'g', 4);
      break;
    case 56:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so4.nfreqtot);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so4.nfreqtot= 0;   // default
      qst.setNum(mysrc->so4.nfreqtot);
      break;
    case 57:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so4.nfreqpos);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so4.nfreqpos= 0;   // default
      qst.setNum(mysrc->so4.nfreqpos);
      break;
    case 58:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so4.nfreqneg);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so4.nfreqneg= 0;   // default
      qst.setNum(mysrc->so4.nfreqneg);
      break;
    case 59:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so4.nsource);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so4.nsource= 0;   // default
      qst.setNum(mysrc->so4.nsource);
      break;
    case 60:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so4.nimage);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so4.nimage= 0;   // default
      qst.setNum(mysrc->so4.nimage);
      break;
      
    case 61:
      if (!init) scanned= sscanf(text, "%lg", &mysrc->so4.deltatime);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so4.deltatime= 0;   // default
      qst.setNum(mysrc->so4.deltatime, 'g', 4);
      break;
      
    case 62:
      if (!init) scanned= sscanf(text, "%d", &mysrc->so4.iconj);
      if ((scanned == EOF) || (scanned == 0)) mysrc->so4.iconj= 0;   // default
      qst.setNum(mysrc->so4.iconj);
      break;

    case 63:
      if (!init) scanned= sscanf(text, "%d", &op->REDUCE_maps);
      if ((scanned == EOF) || (scanned == 0)) op->REDUCE_maps= 0;   // default
      qst.setNum(op->REDUCE_maps);
      myparent->myBeamline()->hormapsloaded= 0;
      break;

    case 64:
      if (!init) scanned= sscanf(text, "%d", &op->ifl.pst_mode);
      if ((scanned == EOF) || (scanned == 0)) op->ifl.pst_mode= 0;   // default
      qst.setNum(op->ifl.pst_mode);
      break;

#ifdef XXXTEMPLATE
case 10:
      if (!init) scanned= sscanf(text, "%d", &);
      if ((scanned == EOF) || (scanned == 0)) = 0;   // default
      qst.setNum( "%d", );
      break;

case 10:
      if (!init) scanned= sscanf(text, "%lg", &);
      if ((scanned == EOF) || (scanned == 0)) = 0;   // default
      qst.setNum( "%lg", );
      break;
#endif
    default:
      qst.setNum(pos);
      qst.append(" undefined parameter number");
    }
    
  parameterModel->updateItemVal(qst, pos);

} // end parameterUpdate

// interactive version checks for backupfile
void MainWindow::ReadBLFileInteractive(char *blname)
{
  char fname[MaxPathLength], oname[MaxPathLength];  
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
		  msgBox->setText(QString(tr("<b>We found a newer backupfile:</b>\n"))+ QString(fname));
		  msgBox->setInformativeText(QString(tr("Do you want to use the backup?")));
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
  /* UF 21.3.12
  strncpy(myparent->myBeamline()->filenames.so4_fsource4a, myparent->myBeamline()->src.so4.fsource4a, 80);
  strncpy(myparent->myBeamline()->filenames.so4_fsource4b, myparent->myBeamline()->src.so4.fsource4b, 80);
  strncpy(myparent->myBeamline()->filenames.so4_fsource4c, myparent->myBeamline()->src.so4.fsource4c, 80);
  strncpy(myparent->myBeamline()->filenames.so4_fsource4d, myparent->myBeamline()->src.so4.fsource4d, 80);
  strncpy(myparent->myBeamline()->filenames.so6_fsource6,  myparent->myBeamline()->src.so6.fsource6,  80);*/

  if (c_window) c_window->updateList();
  myparent->myWriteBLFile(blname);  // to reset the time
} // ReadBLFileInteractive


// UpdateBeamlineBox()
// the box on the left
void MainWindow::UpdateBeamlineBox()
{
  struct OptionsType *blo;
  QString lambdaEqst, dlambdaEqst, dislenEqst;

  fileNameLabel->setText(QString(tr(myparent->myBeamline()->filenames.beamlinename)));

  blo= &(myparent->myBeamline()->BLOptions);

  lambdaE->setText(lambdaEqst.setNum(blo->lambda* 1e6,     'g', 12));
  dislenE->setText(dislenEqst.setNum(blo->displength,      'g', 12));
  dlambdaE->setText(dlambdaEqst.setNum(blo->dlambda* 1e6,  'g', 12));

  if (blo->SourcetoImage == 1) goButton->setChecked(true); else poButton->setChecked(true);
  if (blo->WithAlign) misaliBox->setChecked(true);         else misaliBox->setChecked(false);
  
  if (blo->dlambdaflag) 
    {
      dlambdaBox->setChecked(true); 
      if (blo->SourcetoImage == 1) 
	{
	  dlambdaBox1->setEnabled(true);
	  dlambdaBox2->setEnabled(true);
	} else
	{
	  dlambdaBox1->setEnabled(false);
	  dlambdaBox2->setEnabled(false);
	}
      dlambdaE->setEnabled(true);
    }
  else 
    {
      blo->plrayset= PLRaySet1;           /* force rayset 1 */
      dlambdaBox->setChecked(false);
      dlambdaBox1->setEnabled(false);
      dlambdaBox2->setEnabled(false);
      dlambdaE->setEnabled(false);
    }
  if (blo->plrayset & PLRaySet1) dlambdaBox1->setChecked(true); else dlambdaBox1->setChecked(false);
  if (blo->plrayset & PLRaySet2) dlambdaBox2->setChecked(true); else dlambdaBox2->setChecked(false);
} // end UpdateBeamlineBox

// UpdateElementBox
// box at right buttom
// called with the index of the optical element in the list
void MainWindow::UpdateElementBox(int number)
{
  double cff, teta, fi;
  QString qst;
  
  if (number < 0) return;

  myparent->myBeamline()->ElementList[number].ElementOK = 0;
  myparent->myBeamline()->beamlineOK &= ~(mapOK | resultOK);

  struct mdatset *md= &(myparent->myBeamline()->ElementList[number].MDat);
  struct gdatset *gd= &(myparent->myBeamline()->ElementList[number].GDat);

  teta= fabs(gd->theta0* PI/ 180.0);
  fi  = (double)(gd->inout)* 
    asin(myparent->myBeamline()->BLOptions.lambda* gd->xdens[0]/ (2.0* cos(teta)));
  cff = cos(fi- teta)/ cos(fi+ teta);

   // update widgets
  cffE   ->setText(qst.setNum(cff,        'f', 6));
  preE   ->setText(qst.setNum(gd->r,      'g', 6));
  sucE   ->setText(qst.setNum(gd->rp,     'g', 6));
  thetaE ->setText(qst.setNum(gd->theta0, 'f', 6));
  sourceE->setText(qst.setNum(md->r1,     'g', 6));
  imageE ->setText(qst.setNum(md->r2,     'g', 6));
  rE     ->setText(qst.setNum(md->rmi,    'g', 6));
  rhoE   ->setText(qst.setNum(md->rho,    'g', 6));
  integerSpinBox->setValue(gd->inout);
  lineDensity->setText(qst.setNum(gd->xdens[0], 'g', 10));
  vls1->setText(qst.setNum(gd->xdens[1], 'g', 10));
  vls2->setText(qst.setNum(gd->xdens[2], 'g', 10));
  vls3->setText(qst.setNum(gd->xdens[3], 'g', 10));
  vls4->setText(qst.setNum(gd->xdens[4], 'g', 10));
  duE ->setText(qst.setNum(md->du, 'g', 4));
  dwE ->setText(qst.setNum(md->dw, 'g', 4));
  dlE ->setText(qst.setNum(md->dl, 'g', 4));
  dRuE->setText(qst.setNum(md->dRu * 1e3, 'g', 4));
  dRwE->setText(qst.setNum(md->dRw * 1e3, 'g', 4));
  dRlE->setText(qst.setNum(md->dRl * 1e3, 'g', 4));
  w1E ->setText(qst.setNum(md->w1, 'g', 4));
  w2E ->setText(qst.setNum(md->w2, 'g', 4));
  wsE ->setText(qst.setNum(md->slopew, 'g', 4));
  l1E ->setText(qst.setNum(md->l1, 'g', 4));
  l2E ->setText(qst.setNum(md->l2, 'g', 4));
  lsE ->setText(qst.setNum(md->slopel, 'g', 4));

  //  lambdagE ->setText(qst.setNum(gd->lambdag*1e6,   'f', 6));
  
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
  QString qst;

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

  myparent->myBeamline()->beamlineOK &= ~(sourceOK | resultOK | pstimageOK); 
        
#ifdef DEBUG 
  printf("debug: %s, UpdateSourceBox: bl->RTSource.QuellTyp: %c, beamlineOK: %X, oldsource: %c\n", 
	 __FILE__, myparent->myBeamline()->RTSource.QuellTyp, myparent->myBeamline()->beamlineOK, oldsource);   
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
	cout << "source changed: set defaults" << endl;
	oldsource= sou;
	myparent->sourceSetDefaults();
      }

    // deactivate / clear all
    S1E->setEnabled(false); S1Label->clear(); S1E->clear();
    S2E->setEnabled(false); S2Label->clear(); S2E->clear();
    S3E->setEnabled(false); S3Label->clear(); S3E->clear();
    S4E->setEnabled(false); S4Label->clear(); S4E->clear();
    S5E->setEnabled(false); S5Label->clear(); S5E->clear();
    S6E->setEnabled(false); S6Label->clear(); S6E->clear();
    S7E->setEnabled(false); S7Label->clear(); S7E->clear();
    S8E->setEnabled(false); S8Label->clear(); S8E->clear();
    sourceAutoGuessB->setEnabled(false);    

    switch (sou) {
    case 'D':
      dp= (struct DipolSourceType *)myparent->myBeamline()->RTSource.Quellep;
      sourceTypeLabel->setText(QString(tr("Dipol (Bending Magnet)")));
      S1E->setText(qst.setNum(dp->sigy,  'g', 4));
      S2E->setText(qst.setNum(dp->sigdy, 'g', 4));    
      S3E->setText(qst.setNum(dp->sigz,  'g', 4));  
      S4E->setText(qst.setNum(dp->dz,    'g', 4));    
      S5E->setText(qst.setNum(myparent->myBeamline()->RTSource.raynumber));   
      S1Label->setText(QString("sigmay (mm)"));
      S2Label->setText(QString("sigmady (mrad)"));
      S3Label->setText(QString("sigmaz (mm)"));
      S4Label->setText(QString("dz hard edge (mrad)"));
      S5Label->setText(QString("ray number"));
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      S4E->setEnabled(true);
      S5E->setEnabled(true);
      break;  
    case 'G':
      up0= (struct UndulatorSource0Type *)myparent->myBeamline()->RTSource.Quellep;
      sourceTypeLabel->setText(QString(tr("Generic undulator")));
      S1E->setText(qst.setNum(up0->length,   'g', 4));
      S2E->setText(qst.setNum(myparent->myBeamline()->BLOptions.lambda* 1e6, 'g', 4));    
      S3E->setText(qst.setNum(myparent->myBeamline()->RTSource.raynumber));  
      S4E->setText(qst.setNum(up0->deltaz,   'g', 4));    
      S5E->setText(qst.setNum(up0->sigmaey,  'g', 4));   
      S6E->setText(qst.setNum(up0->sigmaez,  'g', 4));    
      S7E->setText(qst.setNum(up0->sigmaedy, 'g', 4));   
      S8E->setText(qst.setNum(up0->sigmaedz, 'g', 4)); 

      S1Label->setText(QString("length (mm)"));
      S2Label->setText(QString("lambda (nm)"));  
      S3Label->setText(QString("ray number"));  
      S4Label->setText(QString("deltaz (mm)"));  
      S5Label->setText(QString("sigmaey (mm)"));   
      S6Label->setText(QString("sigmaez (mm)"));   
      S7Label->setText(QString("sigmaedy (mrad)"));   
      S8Label->setText(QString("sigmaedz (mrad)"));
     
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
      sourceTypeLabel->setText(QString(tr("Ray Trace hard edge")));		   
      S1E->setText(qst.setNum(hp->disty, 'g', 4));
      S2E->setText(qst.setNum(hp->iy));    
      S3E->setText(qst.setNum(hp->distz, 'g', 4));  
      S4E->setText(qst.setNum(hp->iz));    
      S5E->setText(qst.setNum(hp->divy,  'g', 4));   
      S6E->setText(qst.setNum(hp->idy));    
      S7E->setText(qst.setNum(hp->divz,  'g', 4));   
      S8E->setText(qst.setNum(hp->idz)); 

      S1Label->setText(QString("height (mm)"));
      S2Label->setText(QString("-> points"));  
      S3Label->setText(QString("width (mm)"));  
      S4Label->setText(QString("-> points"));  
      S5Label->setText(QString("vert. div. (mrad)"));   
      S6Label->setText(QString("-> points"));   
      S7Label->setText(QString("hor. div. (mrad)"));   
      S8Label->setText(QString("-> points"));
      
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
      sourceTypeLabel->setText(QString(tr("PO image plane")));
      S1E->setText(qst.setNum(psip->ymin, 'g', 4));
      S2E->setText(qst.setNum(psip->zmin, 'g', 4));    
      S3E->setText(qst.setNum(psip->ymax, 'g', 4));  
      S4E->setText(qst.setNum(psip->zmax, 'g', 4));    
      S5E->setText(qst.setNum(psip->iy));   
      S6E->setText(qst.setNum(psip->iz));    
       
      S1Label->setText(QString("ymin (mm)"));
      S2Label->setText(QString("zmin (mm)"));  
      S3Label->setText(QString("ymax (mm)"));  
      S4Label->setText(QString("zmax (mm)"));  
      S5Label->setText(QString("y points"));   
      S6Label->setText(QString("z points"));   
      
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      S4E->setEnabled(true);
      S5E->setEnabled(true);
      S6E->setEnabled(true);
      
      sourceAutoGuessB->setEnabled(true);
      break;  

    case 'L':
      up= (struct UndulatorSourceType *)myparent->myBeamline()->RTSource.Quellep;
      sourceTypeLabel->setText(QString(tr("SLS SIS undulator")));
      S1E->setText(qst.setNum(up->length, 'g', 4));
      S2E->setText(qst.setNum(myparent->myBeamline()->BLOptions.lambda* 1e6, 'g', 4));    
      S3E->setText(qst.setNum(myparent->myBeamline()->RTSource.raynumber));  
      S4E->setText(qst.setNum(up->deltaz, 'g', 4));    
       
      S1Label->setText(QString("length (mm)"));
      S2Label->setText(QString("lambda (nm)"));  
      S3Label->setText(QString("ray number"));  
      S4Label->setText(QString("deltaz (mm)"));  
      
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      S4E->setEnabled(true);
      break; 

    case 'M':
      up= (struct UndulatorSourceType *)myparent->myBeamline()->RTSource.Quellep;
      sourceTypeLabel->setText(QString(tr("SLS SIM undulator")));
      S1E->setText(qst.setNum(up->length, 'g', 4));
      S2E->setText(qst.setNum(myparent->myBeamline()->BLOptions.lambda* 1e6, 'g', 4));    
      S3E->setText(qst.setNum(myparent->myBeamline()->RTSource.raynumber));  
      S4E->setText(qst.setNum(up->deltaz, 'g', 4));
       
      S1Label->setText(QString("length (mm)"));
      S2Label->setText(QString("lambda (nm)"));  
      S3Label->setText(QString("ray number"));  
      S4Label->setText(QString("deltaz (mm)"));  

      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      S4E->setEnabled(true);
      break;  

    case 'o':
      sop= (struct PointSourceType *)myparent->myBeamline()->RTSource.Quellep;
      sourceTypeLabel->setText(QString(tr("Point Source: all sigma values")));
      S1E->setText(qst.setNum(sop->sigy,  'g', 4));
      S2E->setText(qst.setNum(sop->sigdy, 'g', 4));    
      S3E->setText(qst.setNum(sop->sigz,  'g', 4));  
      S4E->setText(qst.setNum(sop->sigdz, 'g', 4));    
      S5E->setText(qst.setNum(myparent->myBeamline()->RTSource.raynumber));   
      
      S1Label->setText(QString("sigy (mm)"));
      S2Label->setText(QString("sigdy (mrad)"));  
      S3Label->setText(QString("sigz (mm)"));  
      S4Label->setText(QString("sigdz (mrad)"));  
      S5Label->setText(QString("ray number"));   
      
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      S4E->setEnabled(true);
      S5E->setEnabled(true);
      break;  

    case 'R':
      rp= (struct RingSourceType *)myparent->myBeamline()->RTSource.Quellep;
      sourceTypeLabel->setText(QString(tr("Ring Source: half axis of the divergence ellipse, y,z are always 0")));
      S1E->setText(qst.setNum(rp->dy, 'g', 4));
      S2E->setText(qst.setNum(rp->dz, 'g', 4));    
      S3E->setText(qst.setNum(myparent->myBeamline()->RTSource.raynumber));  
       
      S1Label->setText(QString("dy (mrad)"));
      S2Label->setText(QString("dz (mrad)"));  
      S3Label->setText(QString("ray number"));  
      
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      break;  
   
    case 'U':
      up= (struct UndulatorSourceType *)myparent->myBeamline()->RTSource.Quellep;
      sourceTypeLabel->setText(QString(tr("Undulator")));
      S1E->setText(qst.setNum(up->length, 'g', 6));
      S2E->setText(qst.setNum(myparent->myBeamline()->BLOptions.lambda* 1e6, 'g', 4));    
      S3E->setText(qst.setNum(myparent->myBeamline()->RTSource.raynumber));  
       
      S1Label->setText(QString("length (mm)"));
      S2Label->setText(QString("lambda (nm)"));  
      S3Label->setText(QString("ray number"));  
      
      S1E->setEnabled(true);
      S2E->setEnabled(true);
      S3E->setEnabled(true);
      break;  
    case 'F':
      fp= (struct FileSourceType *)myparent->myBeamline()->RTSource.Quellep;
      cout << "source from file " << myparent->myBeamline()->filenames.sourceraysname << endl;
      strncpy(fp->filename, myparent->myBeamline()->filenames.sourceraysname, MaxPathLength);
      sourceTypeLabel->setText(QString(tr("Source from file")));
      break;
    case 'S':
      QMessageBox::warning(this, tr("UpdateSourceBox"),
			   tr("Source type %1 is obsolete.\nenable point source with defaults")
			   .arg(sou));
      myparent->myBeamline()->RTSource.QuellTyp= 'o';
      sop= (struct PointSourceType *)myparent->myBeamline()->RTSource.Quellep;
      sourceTypeLabel->setText(QString(tr("Point Source: all sigma values")));
      S1E->setText(QString("0.1"));
      S2E->setText(QString("0.1"));    
      S3E->setText(QString("0.1"));  
      S4E->setText(QString("25000"));     
      
      S1Label->setText(QString("sigy (mm)"));
      S2Label->setText(QString("sigdy (mrad)"));  
      S3Label->setText(QString("sigz (mm)"));  
      S4Label->setText(QString("sigdz (mrad)"));  
      S5Label->setText(QString("ray number"));   
      
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
} // end UpdateSourceBox

// update the statistics in graphic box
void MainWindow::UpdateStatistics(Plot *pp, const char *label, int rays)
{
  double trans;
  QString qst;

  trans= (myparent->myBeamline()->RTSource.raynumber > 0) ? 
    (double)myparent->myBeamline()->RESULT.points1/ 
    (double)myparent->myBeamline()->RTSource.raynumber : -1.0;
  
  statGroup->setTitle(QString(label)+= QString(tr(" Statistics")));
  czLabel->setText("<FONT COLOR=blue>"+ qst.setNum(pp->cz, 'g', 4)+ "</FONT>");
  cyLabel->setText("<FONT COLOR=blue>"+ qst.setNum(pp->cy, 'g', 4)+ "</FONT>");		   
  wzLabel->setText("<FONT COLOR=blue>"+ qst.setNum(pp->wz, 'g', 4)+ "</FONT>");
  wyLabel->setText("<FONT COLOR=blue>"+ qst.setNum(pp->wy, 'g', 4)+ "</FONT>");	

  cdzLabel->setText("<FONT COLOR=blue>"+ qst.setNum(pp->cdz* 1e3, 'g', 4)+ "</FONT>");	
  cdyLabel->setText("<FONT COLOR=blue>"+ qst.setNum(pp->cdy* 1e3, 'g', 4)+ "</FONT>");
  wdzLabel->setText("<FONT COLOR=blue>"+ qst.setNum(pp->wdz* 1e3, 'g', 4)+ "</FONT>");	
  wdyLabel->setText("<FONT COLOR=blue>"+ qst.setNum(pp->wdy* 1e3, 'g', 4)+ "</FONT>");
  
  rayLabel->setText("<FONT COLOR=blue>"+ qst.setNum(rays)+ "</FONT>");
  traLabel->setText("<FONT COLOR=blue>"+ qst.setNum(trans, 'g', 4)+ "</FONT>");
  
  rzLabel->setText("<FONT COLOR=blue>"+ qst.setNum(pp->rz, 'g', 4)+ "</FONT>");
  ryLabel->setText("<FONT COLOR=blue>"+ qst.setNum(pp->ry, 'g', 4)+ "</FONT>");	

  if (pp->fwhmon)
    {
      wzLabel0->setText(QString(tr("z FWHM (mm)")));
      wyLabel0->setText(QString(tr("y FWHM (mm)")));
      wdzLabel0->setText(QString(tr("dz FWHM (mm)")));
      wdyLabel0->setText(QString(tr("dy FWHM (mm)")));
    } 
  else
    {
      wzLabel0->setText(QString(tr("z RMS (mm)")));
      wyLabel0->setText(QString(tr("y RMS (mm)")));
      wdzLabel0->setText(QString(tr("dz RMS (mm)")));
      wdyLabel0->setText(QString(tr("dy RMS (mm)")));
    }
} // UpdateStatistics

// Update the status window
void MainWindow::UpdateStatus()
{
  int elementnumber= elementList->currentRow();
  QString qst;

  if ((elementnumber < 0) || (elementnumber > elementList->count()- 1)) 
    elementnumber= 0;

  // first the common items for GO and PO
  if (myparent->myBeamline()->elementzahl > 0)
    {
      qst.setNum(elementnumber+1);
      qst= (myparent->myBeamline()->ElementList[elementnumber].ElementOK & elementOK) ?
	QString("<b><FONT COLOR=green>OE_")+ qst : QString("<b><FONT COLOR=red>OE_")+ qst;
    }
  else
    qst= QString("<b><FONT COLOR=red>OE_X");

  elementStatLabel->setText(qst+ "</FONT></b>");

  if (myparent->myBeamline()->beamlineOK & resultOK) 
    imageStatLabel->setText(QString(tr("<b><FONT COLOR=green>image</FONT></b>"))); 
  else 
    imageStatLabel->setText(QString(tr("<b><FONT COLOR=red>image</FONT></b>")));

  if (myparent->myBeamline()->beamlineOK & mapOK) 
    mapStatLabel->setText(QString(tr("<b><FONT COLOR=green>maps</FONT></b>"))); 
  else 
    mapStatLabel->setText(QString(tr("<b><FONT COLOR=red>maps</FONT></b>")));

  // the special items for GO or PO
  if (myparent->myBeamline()->BLOptions.SourcetoImage == 2)    // PO
    {
      statusGroup->setTitle(QString(tr("PO Status")));         // update header
      poimageStatLabel->show();

      if (myparent->myBeamline()->beamlineOK & pstimageOK) 
	poimageStatLabel->setText(QString(tr("<b><FONT COLOR=green>POimpl</FONT></b>"))); 
      else 
	poimageStatLabel->setText(QString(tr("<b><FONT COLOR=red>POimpl</FONT></b>")));

      if (myparent->myBeamline()->beamlineOK & pstsourceOK) 
	sourceStatLabel->setText("<b><FONT COLOR=green>"+ QString(tr("POsource"))+ "</FONT></b>");  // correct version
      else 
	sourceStatLabel->setText(QString("<b><FONT COLOR=red>POsource</FONT></b>")); // sloppy

    }
  else   // GO
    {
      statusGroup->setTitle(QString(tr("GO Status"))); 
      poimageStatLabel->hide();

      if (myparent->myBeamline()->beamlineOK & sourceOK) 
	sourceStatLabel->setText("<b><FONT COLOR=green>"+ QString(tr("source"))+ "</FONT></b>");  
      else 
	sourceStatLabel->setText(QString("<b><FONT COLOR=red>source</FONT></b>")); 
    }
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


// write files si* density cuts
void MainWindow::writeSimp()
{
  int k, ret;
  FILE *f1, *f2, *f3, *f4, *f5, *f6;
  struct PSDType    *psd; 
  char *name= myparent->myBeamline()->filenames.imageraysname;
  char fname1[MaxPathLength], fname2[MaxPathLength], fname3[MaxPathLength], fname4[MaxPathLength], fname5[MaxPathLength], fname6[MaxPathLength];
  char infostr[MaxPathLength];

#ifdef DEBUG
  cout << "debug: writeSimp called" << endl;
  cout << "debug: result type: " << myparent->myBeamline()->RESULT.typ << endl;
#endif

  snprintf(fname1,  MaxPathLength, "%s-simpre", name);
  snprintf(fname2,  MaxPathLength, "%s-simpim", name);
  snprintf(fname3,  MaxPathLength, "%s-sintre", name);
  snprintf(fname4,  MaxPathLength, "%s-sintim", name);
  snprintf(fname5,  MaxPathLength, "%s-simpa",  name);
  snprintf(fname6,  MaxPathLength, "%s-simpp",  name);
  snprintf(infostr, MaxPathLength, "file(s) %s-si* exists!",  name);

  psd= (struct PSDType *)myparent->myBeamline()->RESULT.RESp;

  if (fexists(fname1) || fexists(fname2) || fexists(fname3) || fexists(fname4) || fexists(fname5) || fexists(fname6))
    {
      QMessageBox *msgBox = new QMessageBox;
      msgBox->setText(tr(infostr));
      msgBox->setInformativeText(tr("replace file(s)"));
      msgBox->setStandardButtons(QMessageBox::Ok | QMessageBox::Cancel); 
      ret= msgBox->exec();
      delete msgBox;
      if (ret == QMessageBox::Cancel) { cout << "writeSimp canceled" << endl; return; }
    }
  
  if ((f1= fopen(fname1, "w+")) == NULL) { cout << "can't open " << fname1 << endl; return; }
  if ((f2= fopen(fname2, "w+")) == NULL) { cout << "can't open " << fname2 << endl; return; }
  if ((f3= fopen(fname3, "w+")) == NULL) { cout << "can't open " << fname3 << endl; return; }
  if ((f4= fopen(fname4, "w+")) == NULL) { cout << "can't open " << fname4 << endl; return; }
  if ((f5= fopen(fname5, "w+")) == NULL) { cout << "can't open " << fname5 << endl; return; }
  if ((f6= fopen(fname6, "w+")) == NULL) { cout << "can't open " << fname6 << endl; return; }

  // header
  fprintf(f1, "# file simpre written by phase, IDL plotroutine: plotsi.pro\n#\n");
  fprintf(f2, "# file simpim written by phase, IDL plotroutine: plotsi.pro\n#\n");
  fprintf(f3, "# file sintre written by phase, IDL plotroutine: plotsi.pro\n#\n");
  fprintf(f4, "# file sintim written by phase, IDL plotroutine: plotsi.pro\n#\n");
  fprintf(f5, "# file simpa  written by phase, IDL plotroutine: plotsi.pro\n#\n");
  fprintf(f6, "# file simpp  written by phase, IDL plotroutine: plotsi.pro\n#\n");
  fprintf(f1, "#    dy           I_dzmin    I_dz_center     I_dzmax         dz        I_dy_center\n#\n");
  fprintf(f2, "#    dy           I_dzmin    I_dz_center     I_dzmax         dz        I_dy_center\n#\n");
  fprintf(f3, "#    dy           I_dzmin    I_dz_center     I_dzmax         dz        I_dy_center\n#\n");
  fprintf(f4, "#    dy           I_dzmin    I_dz_center     I_dzmax         dz        I_dy_center\n#\n");
  fprintf(f5, "#    dy           I_dzmin    I_dz_center     I_dzmax         dz        I_dy_center\n#\n");
  fprintf(f6, "#    dy           I_dzmin    I_dz_center     I_dzmax         dz        I_dy_center\n#\n");
  // psd(i,j,k) in fortran memory model

  for (k= 0; k < myparent->myBeamline()->BLOptions.xi.ianzy0; k++)    // UF was passiert wenn ianzy0 != ianzz0 ??
    {
      fprintf(f1, "% e % e % e % e % e % e\n", psd->simpre[k*8], psd->simpre[k*8+4], psd->simpre[k*8+5], psd->simpre[k*8+6], psd->simpre[k*8+3], psd->simpre[k*8+7]);
      fprintf(f2, "% e % e % e % e % e % e\n", psd->simpim[k*8], psd->simpim[k*8+4], psd->simpim[k*8+5], psd->simpim[k*8+6], psd->simpim[k*8+3], psd->simpim[k*8+7]);
      fprintf(f3, "% e % e % e % e % e % e\n", psd->sintre[k*8], psd->sintre[k*8+4], psd->sintre[k*8+5], psd->sintre[k*8+6], psd->sintre[k*8+3], psd->simpre[k*8+7]);
      fprintf(f4, "% e % e % e % e % e % e\n", psd->sintim[k*8], psd->sintim[k*8+4], psd->sintim[k*8+5], psd->sintim[k*8+6], psd->sintim[k*8+3], psd->sintim[k*8+7]);
      fprintf(f5, "% e % e % e % e % e % e\n", psd->simpa[k*8],  psd->simpa[k*8+4],  psd->simpa[k*8+5],  psd->simpa[k*8+6],  psd->simpa[k*8+3],  psd->simpa[k*8+7]);
      fprintf(f6, "% e % e % e % e % e % e\n", psd->simpp[k*8],  psd->simpp[k*8+4],  psd->simpp[k*8+5],  psd->simpp[k*8+6],  psd->simpp[k*8+3],  psd->simpp[k*8+7]);     
    }

  fprintf(f1, "# end\n");
  fprintf(f2, "# end\n");
  fprintf(f3, "# end\n");
  fprintf(f4, "# end\n");
  fprintf(f5, "# end\n");
  fprintf(f6, "# end\n");
  
  fclose(f1);
  fclose(f2);
  fclose(f3);
  fclose(f4);
  fclose(f5);
  fclose(f6);

  cout << "writeSimp done, wrote files: " << name << "-si[mpre|mpim|ntre|ntim|mpa|mpp]" << endl; 
} // end writeSimp

/////////////////////////////////
// end widget handling section //
/////////////////////////////////

void MainWindow::fillTaskVector(const int nr)
{
  vector.resize(0);
  for (int i = 0; i < nr; ++i)          // for testing submit 1000 tasks
    vector.append(i);
} // fillTaskVector

int MainWindow::getPlotSubject()
{
  return mwplotsubject;
}

int MainWindow::getPlotStyle()
{
  return mwplotstyle;
}

// /afs/psi.ch/user/f/flechsig/phase/src/qtgui/mainwindow.cpp
