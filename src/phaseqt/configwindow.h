/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/configwindow.h */
/*  Date      : <16 Aug 11 12:20:20 flechsig>  */
/*  Time-stamp: <28 Aug 14 16:42:35 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

// ******************************************************************************
//
//   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
//                      Paul Scherrer Institut Villigen, Switzerland
//   
//   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
//          Uwe Flechsig,    uwe.flechsig@psi.ch
//
// ------------------------------------------------------------------------------
//
//   This file is part of PHASE.
//
//   PHASE is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, version 3 of the License, or
//   (at your option) any later version.
//
//   PHASE is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
//
// ******************************************************************************

#ifndef CONFIGWINDOW_H
#define CONFIGWINDOW_H

#if (QT_VERSION < 0x050000)
#include <QtGui>
#else
#include <QtWidgets>
#endif

#include <QTreeView>
#include <QPushButton>

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
