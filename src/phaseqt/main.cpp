//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/main.cpp
//  Date      : <31 May 11 16:51:36 flechsig> 
//  Time-stamp: <15 Jun 11 14:32:06 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 


#include <QApplication>

#include "mainwindow.h"
#include "qtphase.h"

int main(int argc, char *argv[])
{
  //  int setupswitch;
    //struct PHASEset PHASESetM;
    //struct BeamlineType BeamlineM;
    QApplication app(argc, argv);
    Q_INIT_RESOURCE(qtgui);
    MainWindow mainWin;
    //   QtPhase *qtppo= new QtPhase;   // call constructor
    //    qtppo->init("default");
    //   qtppo->print();
    //   qtppo->qtpp= qtppo;
    //   qtppo->qtppp= qtppo;
    //    myPHASEset myPHASEseto;            // default constructor
    //   myPHASEseto.init("default");
    //   myPHASEseto.print();

    //   myBeamline myBeamlineo;


    //    myPHASEseto.myPHASEsetp= &myPHASEseto;

    //    setupswitch= ProcComandLine(&PHASESetM, &BeamlineM, argc, argv); 
    //  setupswitch= 0;
/* im Batch (-b) und  Help -h -? 
						modus wird exit(3) gerufen 
						*/
    mainWin.show();
    return app.exec();
}
// end 
