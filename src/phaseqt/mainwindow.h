/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/mainwindow.h */
/*  Date      : <31 May 11 17:01:23 flechsig>  */
/*  Time-stamp: <2011-07-14 00:22:28 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <qsignalmapper.h>
#include <QListWidget>
#include <QGroupBox>

#include "qtphase.h"

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


class MainWindow : public QMainWindow, public QtPhase 
{
    Q_OBJECT

public:
    MainWindow();

private slots:
    void grautoscaleslot();
    void grapplyslot();
    void newBeamline(); // UF
    void save();
    void saveas();
    void print();
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
    void sourceApplyBslot();
    void sourceDefaultBslot();
    void debugslot();

    //void addParagraph(const QString &paragraph);
    void activateProc(const QString &action);
    void UpdateStatistics(Plot *, char *, int);
    
private:
    void createActions();
    void createMenus();
    void createToolBars();
    void createStatusBar();
    void createDockWindows();
    void parameterUpdate(int, char *, int);
    void UpdateElementList();
    void UpdateBeamlineBox();
    void UpdateSourceBox();
    void UpdateElementBox(int);

    QTextEdit   *textEdit;
    QListWidget *customerList;
    QListWidget *paragraphsList;

    QSignalMapper *signalMapper;  // UF
    QListWidget   *elementList;
    QListWidget   *parameterList;
    QLineEdit     *parameterE;
    QWidget       *elementBox;
    QWidget       *createOpticalElementBox(); 
    QWidget       *sourceBox;
    QWidget       *createSourceBox(); 
    QWidget       *beamlineBox;
    QWidget       *createBeamlineBox();
    QWidget       *parameterBox;
    QWidget       *createParameterBox();
    QWidget       *graphicBox;
    QWidget       *createGraphicBox();
    QGroupBox     *groupBox1;
    //   QtPhase       *myQtPhase;

    QLineEdit    *lambdaE;  // generic parameters
    QLineEdit    *dislenE;
    QRadioButton *goButton;
    QRadioButton *poButton;
    QCheckBox    *misaliBox;

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
    
    QSpinBox  *integerSpinBox;   // grating
    QGroupBox *gratingGroup;
    QGroupBox *vlsGroup;
    QLineEdit *lineDensity;
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
    QAction *newLetterAct;
    QAction *saveAct;
    QAction *saveasAct;
    QAction *printAct;
    QAction *undoAct;
    QAction *aboutAct;
    QAction *aboutQtAct;
    QAction *quitAct;

    // UF
    QAction *raytracesimpleAct;
    QAction *raytracefullAct;
    QAction *footprintAct;
    QAction *phasespaceAct;
    QAction *mphasespaceAct;
    QAction *readFg34Act;

    QAction *writemapAct;
    QAction *writecoeffAct;
    QAction *writeRTresultAct;

    QMenu   *plotstyleMenu;
    QAction *grfootprintAct;
    QAction *grcontourAct;
    QAction *grcontourisoAct;
    QAction *grisoAct;
    QAction *grsourceAct;
    QAction *grimageAct;
    QAction *grexample1Act;
    QAction *grexample2Act;
    Plot    *d_plot;

    QPushButton *grautoButton;
    QPushButton *grapplyButton;
    QLineEdit *grzminE;
    QLineEdit *grzmaxE;
    QLineEdit *gryminE;
    QLineEdit *grymaxE;

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

    QSignalMapper *grsignalMapper;  // UF
};


#endif
