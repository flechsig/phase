C Datei: USERDISK_3:[FLECHSIG.PHAS.PHASEFOR.LIB]WDFGMAPM24.FOR
C Datum: 18.NOV.1996
C Stand: 18-NOV-1996
C Autor: FLECHSIG, BESSY Berlin

      subroutine subm2(p2c,dz,p2rc)
      implicit real*8(a-h,o-z)

      dimension   p2c(0:4,0:4)
      dimension   p2rc(0:4) 

      P2RC(0.)=P2C(0.,4.)*DZ**4+P2C(0.,3.)*DZ**3+P2C(0.,2.
     . )*DZ**2+P2C(0.,1.)*DZ+P2C(0.,0.)
      P2RC(1.)=P2C(1.,3.)*DZ**3+P2C(1.,2.)*DZ**2+P2C(1.,1.
     . )*DZ+P2C(1.,0.)
      P2RC(2.)=P2C(2.,2.)*DZ**2+P2C(2.,1.)*DZ+P2C(2.,0.)
      P2RC(3.)=P2C(3.,1.)*DZ+P2C(3.,0.)
      P2RC(4.)=P2C(4.,0.)
      return
      end
