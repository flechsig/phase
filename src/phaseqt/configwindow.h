/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/configwindow.h */
/*  Date      : <16 Aug 11 12:20:20 flechsig>  */
/*  Time-stamp: <18 Nov 11 12:36:21 flechsig>  */
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
    void selectSlot1(const QModelIndex &index);

private:
    QSortFilterProxyModel *proxyModel;
    QTreeView        *proxyView;
    QPushButton      *configApplyB;
    QPushButton      *configQuitB;
    QListWidget      *fileList;
    PhaseQt          *myparent;
    
    void             fillList();
    void             mkRow(char *, const char *, const char *);
    void             addRow(const char *, const char *);
    //void             addRow(QAbstractItemModel *, const char *, const char *);
    //    QAbstractItemModel *createConfigModel(QObject *);
    //QAbstractItemModel *mymodel;
    QStandardItemModel *createConfigModel(QObject *);
    QStandardItemModel *mymodel;
    
};  
#endif
// end
