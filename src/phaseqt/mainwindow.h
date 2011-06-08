/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/mainwindow.h */
/*  Date      : <31 May 11 17:01:23 flechsig>  */
/*  Time-stamp: <07 Jun 11 17:28:07 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <qsignalmapper.h>


QT_BEGIN_NAMESPACE
class QAction;
class QListWidget;
class QMenu;
class QTextEdit;
QT_END_NAMESPACE


class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow();

private slots:
    void newLetter();
    void newBeamline(); // UF
    void save();
    void print();
    void undo();
    void about();
    void insertCustomer(const QString &customer);
    void addParagraph(const QString &paragraph);
    void activateProc(const QString &action);
    
private:
    void createActions();
    void createMenus();
    void createToolBars();
    void createStatusBar();
    void createDockWindows();

    QTextEdit   *textEdit;
    QListWidget *customerList;
    QListWidget *paragraphsList;

    QSignalMapper *signalMapper;  // UF
    QListWidget   *elementList;   
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
      // UF end

    QMenu *fileMenu;
    QMenu *editMenu;
    QMenu *calcMenu;
    QMenu *viewMenu;
    QMenu *helpMenu;
    QToolBar *fileToolBar;
    QToolBar *editToolBar;
    QAction *newLetterAct;
    QAction *saveAct;
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
};


#endif
