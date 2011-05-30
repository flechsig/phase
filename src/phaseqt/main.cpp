//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/main.cpp
//  Date      : <30 May 11 09:27:24 flechsig> 
//  Time-stamp: <30 May 11 09:29:14 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 

#include "phasegui.h"


int main(int argc, char **argv)
{
    QApplication a(argc, argv);

    MainWindow mainWindow;

    //    mainWindow.resize(500,500);
    mainWindow.show();

    return a.exec(); 
}
