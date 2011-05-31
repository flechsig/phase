//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/main.cpp
//  Date      : <31 May 11 16:51:36 flechsig> 
//  Time-stamp: <31 May 11 17:00:59 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 


#include <QApplication>

#include "mainwindow.h"

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    Q_INIT_RESOURCE(qtgui);
    MainWindow mainWin;
    mainWin.show();
    return app.exec();
}
// end 
