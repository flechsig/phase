/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/singleray.h */
/*  Date      : <15 Jul 11 14:16:20 flechsig>  */
/*  Time-stamp: <09 Sep 11 14:59:15 flechsig>  */
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

#include <QtGui>

#include "phaseqt.h"

class SingleRay : public QWidget
{
    Q_OBJECT
    
    struct RayType rayin, rayout;

public:
    QWidget     *singleRayBox;
    SingleRay(struct BeamlineType *, QWidget *);
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
    struct BeamlineType  *myparent;
    void        RayTraceSingleRayCpp(struct BeamlineType *);
       
};
#endif
