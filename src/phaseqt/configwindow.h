/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/configwindow.h */
/*  Date      : <16 Aug 11 12:20:20 flechsig>  */
/*  Time-stamp: <2011-11-20 20:22:24 flechsig>  */
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
    void       updateList();

private slots:
    void applySlot();
    void defaultSlot();
    void quitSlot();
    void selectSlot(const QModelIndex &index);

private:
    QTreeView          *sourceView;
    QPushButton        *configApplyB;
    QPushButton        *configQuitB;
    QPushButton        *configDefaultB;
    PhaseQt            *myparent;
    QStandardItemModel *createConfigModel(QObject *);
    QStandardItemModel *mymodel;
    void               fillList();
    void               addRow(const char *, const char *, const char *);
    void               checkFileNames();
};  
#endif
// end
