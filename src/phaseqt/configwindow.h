/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/configwindow.h */
/*  Date      : <16 Aug 11 12:20:20 flechsig>  */
/*  Time-stamp: <2011-11-11 22:39:43 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifndef CONFIGWINDOW_H
#define CONFIGWINDOW_H

#include <QtGui>

#include "phaseqt.h"

class ConfigWindow : public QWidget
{
    Q_OBJECT

public:
    QWidget  *configWindowBox;
    ConfigWindow(PhaseQt *);
    void             updateList();

private slots:
    void applySlot();
    void quitSlot();
    void selectSlot();

private:
    QPushButton      *configApplyB;
    QPushButton      *configQuitB;
    QListWidget      *fileList;
    PhaseQt          *myparent;
    void             fillList();
    void             mkRow(char *, const char *, const char *);
    
};  
#endif
// end
