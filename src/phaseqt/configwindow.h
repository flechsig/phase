/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/configwindow.h */
/*  Date      : <16 Aug 11 12:20:20 flechsig>  */
/*  Time-stamp: <09 Sep 11 15:16:44 flechsig>  */
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
    ConfigWindow(struct PHASEset *);

private slots:
    void applySlot();
    void quitSlot();
    void selectSlot();

private:
    QPushButton      *configApplyB;
    QPushButton      *configQuitB;
    QListWidget      *fileList;
    struct PHASEset  *myparent;
    void             fillList();
    void             mkRow(char *, const char *, const char *);
};
#endif
// end
