/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/singleray.h */
/*  Date      : <15 Jul 11 14:16:20 flechsig>  */
/*  Time-stamp: <30 Oct 13 14:20:28 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

//
// this SingleRay class defines the SingleRay widget
//

#ifndef SINGLERAY_H
#define SINGLERAY_H

#if (QT_VERSION < 0x050000)
#include <QtGui>
#else
#include <QtWidgets>
#endif

#include <QLabel>
#include <QLineEdit>
#include <QPushButton>

#include "phaseqt.h"

class SingleRay : public QWidget
{
    Q_OBJECT
    
    struct RayType rayin, rayout;

public:
    QWidget     *singleRayBox;
    SingleRay(PhaseQt *, QWidget *);
    ~SingleRay();

private slots:
    void defaultSlot();
    void applySlot();
    void quitSlot();

private:
    QLabel      *S1Label;        // source box
    QLabel      *S2Label;
    QLabel      *S3Label;
    QLabel      *S4Label;
    QLabel      *S5Label;
    QLabel      *S6Label;
    QLabel      *S7Label;
    QLabel      *S8Label;
    QLabel      *S9Label;
    QLabel      *S10Label;
    QLineEdit   *S1E;
    QLineEdit   *S2E;
    QLineEdit   *S3E;
    QLineEdit   *S4E;
    QLineEdit   *S5E;
    QPushButton *sourceDefaultB;
    QPushButton *sourceApplyB;
    QPushButton *sourceQuitB;
    //    struct BeamlineType  *myparent;
    void        RayTraceSingleRayCpp(PhaseQt *);

    PhaseQt *myparent;
       
};
#endif
