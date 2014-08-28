/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/optiinput.h */
/*  Date      : <29 Jul 11 13:56:13 flechsig>  */
/*  Time-stamp: <28 Aug 14 16:42:11 flechsig>  */
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
