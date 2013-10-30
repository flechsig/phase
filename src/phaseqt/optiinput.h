/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/optiinput.h */
/*  Date      : <29 Jul 11 13:56:13 flechsig>  */
/*  Time-stamp: <30 Oct 13 14:16:28 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifndef OPTIINPUT_H
#define OPTIINPUT_H

#if (QT_VERSION < 0x050000)
#include <QtGui>
#else
#include <QtWidgets>
#endif

#include <QPushButton>
#include <QListWidget>
#include <QLabel>

#include "phaseqt.h"

class OptiInput : public QWidget
{
    Q_OBJECT

public:
    QWidget     *optiInputBox;
    //    OptiInput(struct BeamlineType *);
    //OptiInput(QtPhase*);
    OptiInput(struct ElementType *, unsigned int, char*, char*, char*);

private slots:
    void appendLine();
    void applySlot();
    void calcIndexSlot();
    void deleteLine();
    void focusActSlot();
    void focusvActSlot();
    void focushActSlot();
    void focusSActSlot();
    void transActSlot();
    void costActSlot();
    void rpowervActSlot();
    void rpowerhActSlot();
    void inputUpdateSlot();
    void quitSlot();
    void selectElementSlot();
    void selectInputSlot();
    void selectParameterSlot();
    void updateSlot();
private:
    struct BeamlineType  *myparent;
    struct ElementType *mylist;
    unsigned int myelementnumber;
    
    char *mybeamlinename;
    char *myopresname;
    char *myoptipckname;
    QPushButton *plusButton;
    QPushButton *minusButton;
    QPushButton *optiApplyB;
    QPushButton *optiQuitB;
    QPushButton *optiUpdateB;
    QListWidget *optielementList;
    QListWidget *parameterList;
    QListWidget *inputList;
    QLabel      *selectionLabel;
    QLabel      *templateLabel;
    QLineEdit   *inputE;
    void fillElementList();
    void fillInputs();
    QMenu *optTargetMenu;
    QAction *focusAct;
    QAction *focusvAct;
    QAction *focushAct;
    QAction *focusSAct;
    QAction *transAct;
    QAction *costAct;
    QAction *rpowervAct;
    QAction *rpowerhAct;
    QLabel  *targetLabel;
    QLabel  *outputfileLabel;
    QLabel  *minuitfileLabel;
};
#endif
// end
