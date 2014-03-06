/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/mainwindow.h */
/*  Date      : <31 May 11 17:01:23 flechsig>  */
/*  Time-stamp: <06 Mar 14 08:08:26 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <qsignalmapper.h>
#include <QKeySequence>
#include <QListWidget>
#include <QGroupBox>
#include <QMessageBox>
#include <sys/stat.h>


#include "phaseqt.h"
#include "singleray.h"
#include "optiinput.h"
#include "configwindow.h"
#include "plot.h"
#include "treemodel.h"
#include "plotmatrix.h"
#include "plot2x2.h"

QT_BEGIN_NAMESPACE
class QAction;
class QListWidget;
class QMenu;
class QTextEdit;
class QCheckBox;
class QRadioButton;
class QLabel;
class QSpinBox;
class QGroupBox;
class QPushButton;
QT_END_NAMESPACE

void my_funcv(int &, int &);  // the test function for qfuture

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(PhaseQt *parent);
    ~MainWindow();
    int checkResultType(struct RESULTType *, int);
    void fillTaskVector(const int);
    void parameterUpdateAll(int);
    //  void print();
    void UpdateBeamlineBox();
    void UpdateElementList();
    void UpdateSourceBox();
    void UpdateStatus();
    void writeBackupFile();
    void ReadBLFileInteractive(char *);
    //   int FileExistCheckOK(char *);
    int FileExistCheckOK(std::string);
    //    int FileExistCheckOK(char *, char *);
    int FileExistCheckOK(std::string, std::string);
    int getPlotSubject();
    int getPlotStyle();
    char oldsource;
    QLineEdit     *parameterE;
    //QProgressBar  *progressBar;
    //QProgressDialog* progressD;

private slots:
    void appendElement();
    void dislenSlot();
    void doubleclickElement();
    void goButtonslot();
    void grautoscaleslot();
    void grapplyslot();
    void lambdaSlot();
    void dlambdaSlot();
    void misaliBoxslot(int);
    void dlambdaBoxslot(int);
    void dlambdaBox1slot(int);
    void dlambdaBox2slot(int);
    void newBeamline(); // UF
    void openBeamline(); // UF
    void poButtonslot();
    void print();
    void printMain();
    void screenshotMain();
    void save();
    void saveas();
    
    void undo();
    void about();
    //void insertCustomer(const QString &customer);
    void insertElement();   // UF
    void deleteElement();   // UF
    void selectElement();   // UF
    void selectParameter(); // UF
    void pmslot();          // shape slots
    void toslot();
    void peslot();
    void elslot();
    void coslot();
    void geslot();
    void apslot();
    void frslot();
    void grslot();
    void grvlsslot();
   
    void rup1slot();
    void rleft2slot();
    void rdown3slot();
    void rright4slot();
    void parameterUpdateSlot();

    void thetaBslot();
    void sourceBslot();
    void imageBslot();
    void rBslot();
    void rhoBslot();
    void elementApplyBslot();    
    void sigmaslot();
    void fwhmslot();
    void sourceApplyBslot();
    void sourceDefaultBslot();
    void sourceAutoGuessBslot();
    void debugslot();
    void pause_thread();
    void resume_thread();
    void abort_thread();
    void finished_thread();

    //void addParagraph(const QString &paragraph);
    void activateProc(const QString &action);
    void UpdateStatistics(Plot *, const char *, int);
    
private:
    void createActions();
    void createMenus();
    void createToolBars();
    void createStatusBar();
    void createDockWindows();
    void parameterUpdate(int, const char *, int);
    
    
    void UpdateElementBox(int);
    void updateGraphicsInput(int);
    void writeSimp();

    QTextEdit   *textEdit;
    QListWidget *customerList;
    QListWidget *paragraphsList;

    QSignalMapper *signalMapper;  // UF
    QListWidget   *elementList;
    QListWidget   *parameterList;
    
    QWidget       *elementBox;
    QWidget       *createOpticalElementBox(); 
    QWidget       *plotBox;
    QWidget       *createPlotBox(); 
    QWidget       *sourceBox;
    QWidget       *createSourceBox(); 
    QWidget       *beamlineBox;
    QWidget       *createBeamlineBox();
    QWidget       *parameterBox;
    QWidget       *createParameterBox();
    QWidget       *graphicBox;
    QWidget       *createGraphicBox();
    QGroupBox     *groupBox1;
    QGroupBox     *statusGroup;
    //   QtPhase       *myQtPhase;

    QLineEdit    *lambdaE;  // generic parameters
    QLineEdit    *dlambdaE;
    QLineEdit    *dislenE;
    QRadioButton *goButton;
    QRadioButton *poButton;
    QRadioButton *sigmaButton;
    QRadioButton *fwhmButton;
    QCheckBox    *misaliBox;
    QCheckBox    *dlambdaBox;
    QCheckBox    *dlambdaBox1;
    QCheckBox    *dlambdaBox2;
    QLabel       *fileNameLabel;

    QLabel    *sourceTypeLabel;
    QCheckBox *sourceFileBox;
    QLabel    *S1Label;        // source box
    QLabel    *S2Label;
    QLabel    *S3Label;
    QLabel    *S4Label;
    QLabel    *S5Label;
    QLabel    *S6Label;
    QLabel    *S7Label;
    QLabel    *S8Label;
    QLineEdit *S1E;
    QLineEdit *S2E;
    QLineEdit *S3E;
    QLineEdit *S4E;
    QLineEdit *S5E;
    QLineEdit *S6E;
    QLineEdit *S7E;
    QLineEdit *S8E;
    QPushButton *sourceApplyB;
    QPushButton *sourceDefaultB;
    QPushButton *sourceAutoGuessB;
    QAction *rthAct;
    QAction *dipAct;
    QAction *poiAct;
    QAction *rinAct;
    QAction *genAct;
    QAction *b2hAct;
    QAction *b2lAct;
    QAction *sisAct;
    QAction *simAct;
    QAction *sffAct;
    QAction *impAct;
    QLineEdit *cffE;     // element box
    QLineEdit *preE;    
    QLineEdit *sucE;    
    QLineEdit *thetaE;  
    QLineEdit *sourceE; 
    QLineEdit *imageE;  
    QLineEdit *rE;      
    QLineEdit *rhoE; 

    QPushButton *thetaB;
    QPushButton *sourceB;
    QPushButton *imageB;
    QPushButton *rB;
    QPushButton *rhoB;    
    QPushButton *elementApplyB;

    QAction *pmAct;
    QAction *toAct;
    QAction *peAct;
    QAction *elAct;
    QAction *coAct;
    QAction *geAct;
    QAction *apAct;
    QAction *frAct;
    
    QSpinBox  *integerSpinBox;   // grating
    QGroupBox *gratingGroup;
    QGroupBox *vlsGroup;
    QLineEdit *lineDensity;
    QLineEdit *lambdagE;
    QCheckBox *nimBox;
    QLineEdit *vls1;
    QLineEdit *vls2;
    QLineEdit *vls3;
    QLineEdit *vls4;
    QLineEdit *duE;  
    QLineEdit *dwE;  
    QLineEdit *dlE;  
    QLineEdit *dRuE; 
    QLineEdit *dRwE; 
    QLineEdit *dRlE; 
    QLineEdit *w1E;  
    QLineEdit *w2E;  
    QLineEdit *wsE;  
    QLineEdit *l1E;  
    QLineEdit *l2E;  
    QLineEdit *lsE;  

    QRadioButton *rup1;            //orientation
    QRadioButton *rleft2;
    QRadioButton *rdown3;
    QRadioButton *rright4; 
    QLabel *shapeLabel;
    QLabel *typeLabel;
  // UF end

    QMenu *fileMenu;
    QMenu *editMenu;
    QMenu *calcMenu;
    QMenu *cmdMenu;
    QMenu *viewMenu;
    QMenu *helpMenu;
    QMenu *sourceMenu;

    QToolBar *fileToolBar;
    QToolBar *editToolBar;
    QAction *newBeamlineAct;
    QAction *openBeamlineAct;
    QAction *saveAct;
    QAction *saveasAct;
    QAction *printAct;
    QAction *printMainAct;
    QAction *screenshotAct;
    QAction *undoAct;
    QAction *aboutAct;
    QAction *aboutQtAct;
    QAction *quitAct;

    // UF
    QToolButton *btnLogy;
    QGridLayout *plotLayout;
    QAction *raytracesimpleAct;
    QAction *raytracefullAct;
    QAction *footprintAct;
    QAction *singleRayAct;
    QAction *asynMapAct;
    QAction *asynPOAct;
    QAction *fresnelAct;
    QAction *fourierAct;
    QAction *normPOAct;
    QAction *asynTestAct;
    QAction *optiInputAct;
    QAction *phasespaceAct;
    QAction *mphasespaceAct;
    QAction *readFg34Act;
    QAction *poInitSourceAct;
    QAction *configureAct;

    QAction *writemapAct;
    QAction *writematAct;
    QAction *writecoeffAct;
    QAction *writesimpAct;
    QAction *writeResultAct;
    QAction *writeResultPh5Act;
    QAction *writeResultGh5Act;
    QAction *readResulth5Act;

    QMenu   *plotstyleMenu;
    QAction *grscatterAct;
    QAction *grcontourAct;
    QAction *grcontourisoAct;
    QAction *grisoAct;
    QAction *grHorProfAct;
    QAction *grVerProfAct;

    QAction *grGoSourceSpaAct;
    QAction *grGoSourceDivAct;
    QAction *grGoSourcePhiAct;
    QAction *grGoSourceHpsAct;
    QAction *grGoSourceVpsAct;
    QAction *grGoResultSpaAct;
    QAction *grGoResultDivAct;
    QAction *grGoResultPhiAct;
    QAction *grGoResultHpsAct;
    QAction *grGoResultVpsAct;
    QAction *grPoSourceAct;
    QAction *grPoSourceS1Act;
    QAction *grPoSourceS2Act;
    QAction *grPoSourceS3Act;
    QAction *grPoSourcePZAct;
    QAction *grPoSourcePYAct;
    QAction *grPoResultAct;
    QAction *grPoResultS1Act;
    QAction *grPoResultS2Act;
    QAction *grPoResultS3Act;
    QAction *grPoResultPZAct;
    QAction *grPoResultPYAct;
    QAction *grPoSimpreAct;
    QAction *grPoSimpimAct;
    QAction *grPoSintreAct;
    QAction *grPoSintimAct;
    QAction *grexample1Act;
    QAction *grexample2Act;
    QAction *grexample3Act;
    Plot    *d_plot;
    SingleRay *s_ray;
    OptiInput *o_input;
    ConfigWindow *c_window;
    Plot2x2  *zone;

    QPushButton *grautoButton;
    QPushButton *grapplyButton;
    QLineEdit *grzminE;
    QLineEdit *grzmaxE;
    QLineEdit *gryminE;
    QLineEdit *grymaxE;

    QLabel *zminLabel;
    QLabel *zmaxLabel;
    QLabel *yminLabel;
    QLabel *ymaxLabel;

    QGroupBox   *statGroup;
    QLabel *czLabel;   
    QLabel *cyLabel;   
    QLabel *wzLabel;   
    QLabel *wyLabel;   
    QLabel *cdzLabel;  
    QLabel *cdyLabel;  
    QLabel *wdzLabel;  
    QLabel *wdyLabel;  
    QLabel *rayLabel;  
    QLabel *traLabel;
    QLabel *ryLabel;
    QLabel *rzLabel;

    QLabel *czLabel0;   
    QLabel *cyLabel0;   
    QLabel *wzLabel0;   
    QLabel *wyLabel0;   
    QLabel *cdzLabel0;  
    QLabel *cdyLabel0;  
    QLabel *wdzLabel0;  
    QLabel *wdyLabel0;  
    QLabel *rayLabel0;  
    QLabel *traLabel0;
    QLabel *ryLabel0;
    QLabel *rzLabel0;
    
    QLabel *sourceStatLabel;  
    QLabel *imageStatLabel;  
    QLabel *mapStatLabel;
    QLabel *elementStatLabel;
    QLabel *poimageStatLabel;

    QSignalMapper *grsignalMapper;  // UF
    
    PhaseQt *myparent;

    int elementListNotSelected();
    int elementListIsEmpty();

    TreeModel *parameterModel;

    QWidget      *myProgressDialog;         // ProgressDialog
    QPushButton  *progressAbortButton;
    QPushButton  *progressPauseButton;
    QPushButton  *progressResumeButton;
    QProgressBar *dialogProgressBar;
    QLabel       *progressLabel;
    QFuture<void>        *future;
    QFutureWatcher<void> *watcher;
    void createProgress();
    QVector <int> vector;
    struct map4      *m4p_cpp;
    struct constants *csp_cpp;

    int    mwplotsubject;  // mainwindow variable
    int    mwplotstyle;    // mainwindow variable
};


#endif
