c File      : /home/vms/flechsig/vms/phas/extr/cost.for
c Date      : <29 Oct 97 10:13:17 flechsig> 
c Time-stamp: <09 Mar 98 14:20:18 flechsig> 
c Author    : Uwe Flechsig, flechsig@exp.bessy.de

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Der Funktion werden  die fuer die Beamline errechneten
c   Entwicklungskoeffizienten fuer y, z, dy, dz uebergeben
c   der Qualitaetswert chi muss daraus berechnet, 
c   und im Parameter dres zurueckgegeben werden!
c   !!! dres darf nicht negativ werden !!!
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   21.1.98 dy, dz eingefuegt (sigma Werte der Quelle)
c   4.2.98 dlf eingefuegt (deltalambda Faktor)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

C**********************************************************************
      subroutine costfor(dres,ypc1,zpc1,dypc,dzpc,
     &                   dy,dz,dlf)                      !nicht aendern
C**********************************************************************
C
C       BERECHNUNG VON CHI\_SQUARE
C
C**********************************************************************
      
      real*8 dres,dy,dz,dlf,
     &     ypc1(0:4,0:4,0:4,0:4),
     &     zpc1(0:4,0:4,0:4,0:4),
     &     dypc(0:4,0:4,0:4,0:4),
     &     dzpc(0:4,0:4,0:4,0:4)
      
      
c*************calculate chi-square ************************
c          ! einheiten mm und rad !
      
c      dz= 0.00032              !  +- 0.32 mrad 
c      dy= 0.0013               !  +- 1.30 mrad

      dres=ypc1(0,0,1,0)*dy       +   ! DEFOKUSIERUNG 
     &     ypc1(0,0,2,0)*dy**2    +   ! coma
     &     ypc1(0,0,3,0)*dy**3    +   ! sph. abb	
     &     ypc1(0,0,0,1)*dz       +
     &     ypc1(0,0,0,2)*dz**2    +
     &     ypc1(0,0,0,3)*dz**3    +
     &     ypc1(0,0,1,1)*dy*dz    +
     &     ypc1(0,0,1,2)*dy*dz**2 +
     &     ypc1(0,0,2,1)*dy**2*dz 	
 
      dres=dlf*dabs(dres)  * 2.38     ! scaling slope factor
c      write(*,*)'cost.for: ',dres,dlf
c      dres=dlf
      write(*,*)'cost.for: ',dres
      RETURN
      END

c end /home/vms/flechsig/vms/phas/extr/cost.for
