c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/misali1.f
c$$$ Date      : <16 Feb 04 16:39:00 flechsig> 
c$$$ Time-stamp: <18 Feb 04 11:19:32 flechsig> 

c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

      subroutine misali1(a,anew,drz)
      implicit real*8(a-h,o-z)
      dimension a(0:5,0:5),anew(0:5,0:5)

c UF correction of the direction
      drz=- drz

      anew(0.0,0.0)=a(0.0,0.0)
      anew(0.0,1.0)=a(1.0,0.0)*dsin(drz)+a(0.0,1.0)*dcos(drz)
      anew(0.0,2.0)=a(2.0,0.0)*dsin(drz)**2+a(1.0,1.0)*dcos(drz)*dsin
     . (drz)+a(0.0,2.0)*dcos(drz)**2
      anew(0.0,3.0)=a(3.0,0.0)*dsin(drz)**3+a(2.0,1.0)*dcos(drz)*dsin
     . (drz)**2+a(1.0,2.0)*dcos(drz)**2*dsin(drz)+a(0.0,3.0)*dcos(drz
     . )**3
      anew(0.0,4.0)=a(4.0,0.0)*dsin(drz)**4+a(3.0,1.0)*dcos(drz)*dsin
     . (drz)**3+a(2.0,2.0)*dcos(drz)**2*dsin(drz)**2+a(1.0,3.0)*dcos(
     . drz)**3*dsin(drz)+a(0.0,4.0)*dcos(drz)**4
      anew(0.0,5.0)=a(5.0,0.0)*dsin(drz)**5+a(4.0,1.0)*dcos(drz)*dsin
     . (drz)**4+a(3.0,2.0)*dcos(drz)**2*dsin(drz)**3+a(2.0,3.0)*dcos(
     . drz)**3*dsin(drz)**2+a(1.0,4.0)*dcos(drz)**4*dsin(drz)+a(0.0,
     . 5.0)*dcos(drz)**5
      anew(1.0,0.0)=a(1.0,0.0)*dcos(drz)-a(0.0,1.0)*dsin(drz)
      anew(1.0,1.0)=2.0*a(2.0,0.0)*dcos(drz)*dsin(drz)+a(1.0,1.0)*
     . dcos(drz)**2-a(1.0,1.0)*dsin(drz)**2-2.0*a(0.0,2.0)*dcos(drz)*
     . dsin(drz)
      anew(1.0,2.0)=3.0*a(3.0,0.0)*dcos(drz)*dsin(drz)**2+2.0*a(2.0,
     . 1.0)*dcos(drz)**2*dsin(drz)-a(2.0,1.0)*dsin(drz)**3+a(1.0,2.0)
     . *dcos(drz)**3-2.0*a(1.0,2.0)*dcos(drz)*dsin(drz)**2-3.0*a(0.0,
     . 3.0)*dcos(drz)**2*dsin(drz)
      anew(1.0,3.0)=4.0*a(4.0,0.0)*dcos(drz)*dsin(drz)**3+3.0*a(3.0,
     . 1.0)*dcos(drz)**2*dsin(drz)**2-a(3.0,1.0)*dsin(drz)**4+2.0*a(
     . 2.0,2.0)*dcos(drz)**3*dsin(drz)-2.0*a(2.0,2.0)*dcos(drz)*dsin(
     . drz)**3+a(1.0,3.0)*dcos(drz)**4-3.0*a(1.0,3.0)*dcos(drz)**2*
     . dsin(drz)**2-4.0*a(0.0,4.0)*dcos(drz)**3*dsin(drz)
      anew(1.0,4.0)=5.0*a(5.0,0.0)*dcos(drz)*dsin(drz)**4+4.0*a(4.0,
     . 1.0)*dcos(drz)**2*dsin(drz)**3-a(4.0,1.0)*dsin(drz)**5+3.0*a(
     . 3.0,2.0)*dcos(drz)**3*dsin(drz)**2-2.0*a(3.0,2.0)*dcos(drz)*
     . dsin(drz)**4+2.0*a(2.0,3.0)*dcos(drz)**4*dsin(drz)-3.0*a(2.0,
     . 3.0)*dcos(drz)**2*dsin(drz)**3+a(1.0,4.0)*dcos(drz)**5-4.0*a(
     . 1.0,4.0)*dcos(drz)**3*dsin(drz)**2-5.0*a(0.0,5.0)*dcos(drz)**4
     . *dsin(drz)
      anew(2.0,0.0)=a(2.0,0.0)*dcos(drz)**2-a(1.0,1.0)*dcos(drz)*dsin
     . (drz)+a(0.0,2.0)*dsin(drz)**2
      anew(2.0,1.0)=3.0*a(3.0,0.0)*dcos(drz)**2*dsin(drz)+a(2.0,1.0)*
     . dcos(drz)**3-2.0*a(2.0,1.0)*dcos(drz)*dsin(drz)**2-2.0*a(1.0,
     . 2.0)*dcos(drz)**2*dsin(drz)+a(1.0,2.0)*dsin(drz)**3+3.0*a(0.0,
     . 3.0)*dcos(drz)*dsin(drz)**2
      anew(2.0,2.0)=6.0*a(4.0,0.0)*dcos(drz)**2*dsin(drz)**2+3.0*a(
     . 3.0,1.0)*dcos(drz)**3*dsin(drz)-3.0*a(3.0,1.0)*dcos(drz)*dsin(
     . drz)**3+a(2.0,2.0)*dcos(drz)**4-4.0*a(2.0,2.0)*dcos(drz)**2*
     . dsin(drz)**2+a(2.0,2.0)*dsin(drz)**4-3.0*a(1.0,3.0)*dcos(drz)
     . **3*dsin(drz)+3.0*a(1.0,3.0)*dcos(drz)*dsin(drz)**3+6.0*a(0.0,
     . 4.0)*dcos(drz)**2*dsin(drz)**2
      anew(2.0,3.0)=10.0*a(5.0,0.0)*dcos(drz)**2*dsin(drz)**3+6.0*a(
     . 4.0,1.0)*dcos(drz)**3*dsin(drz)**2-4.0*a(4.0,1.0)*dcos(drz)*
     . dsin(drz)**4+3.0*a(3.0,2.0)*dcos(drz)**4*dsin(drz)-6.0*a(3.0,
     . 2.0)*dcos(drz)**2*dsin(drz)**3+a(3.0,2.0)*dsin(drz)**5+a(2.0,
     . 3.0)*dcos(drz)**5-6.0*a(2.0,3.0)*dcos(drz)**3*dsin(drz)**2+3.0
     . *a(2.0,3.0)*dcos(drz)*dsin(drz)**4-4.0*a(1.0,4.0)*dcos(drz)**4
     . *dsin(drz)+6.0*a(1.0,4.0)*dcos(drz)**2*dsin(drz)**3+10.0*a(0.0
     . ,5.0)*dcos(drz)**3*dsin(drz)**2
      anew(3.0,0.0)=a(3.0,0.0)*dcos(drz)**3-a(2.0,1.0)*dcos(drz)**2*
     . dsin(drz)+a(1.0,2.0)*dcos(drz)*dsin(drz)**2-a(0.0,3.0)*dsin(
     . drz)**3
      anew(3.0,1.0)=4.0*a(4.0,0.0)*dcos(drz)**3*dsin(drz)+a(3.0,1.0)*
     . dcos(drz)**4-3.0*a(3.0,1.0)*dcos(drz)**2*dsin(drz)**2-2.0*a(
     . 2.0,2.0)*dcos(drz)**3*dsin(drz)+2.0*a(2.0,2.0)*dcos(drz)*dsin(
     . drz)**3+3.0*a(1.0,3.0)*dcos(drz)**2*dsin(drz)**2-a(1.0,3.0)*
     . dsin(drz)**4-4.0*a(0.0,4.0)*dcos(drz)*dsin(drz)**3
      anew(3.0,2.0)=10.0*a(5.0,0.0)*dcos(drz)**3*dsin(drz)**2+4.0*a(
     . 4.0,1.0)*dcos(drz)**4*dsin(drz)-6.0*a(4.0,1.0)*dcos(drz)**2*
     . dsin(drz)**3+a(3.0,2.0)*dcos(drz)**5-6.0*a(3.0,2.0)*dcos(drz)
     . **3*dsin(drz)**2+3.0*a(3.0,2.0)*dcos(drz)*dsin(drz)**4-3.0*a(
     . 2.0,3.0)*dcos(drz)**4*dsin(drz)+6.0*a(2.0,3.0)*dcos(drz)**2*
     . dsin(drz)**3-a(2.0,3.0)*dsin(drz)**5+6.0*a(1.0,4.0)*dcos(drz)
     . **3*dsin(drz)**2-4.0*a(1.0,4.0)*dcos(drz)*dsin(drz)**4-10.0*a(
     . 0.0,5.0)*dcos(drz)**2*dsin(drz)**3
      anew(4.0,0.0)=a(4.0,0.0)*dcos(drz)**4-a(3.0,1.0)*dcos(drz)**3*
     . dsin(drz)+a(2.0,2.0)*dcos(drz)**2*dsin(drz)**2-a(1.0,3.0)*dcos
     . (drz)*dsin(drz)**3+a(0.0,4.0)*dsin(drz)**4
      anew(4.0,1.0)=5.0*a(5.0,0.0)*dcos(drz)**4*dsin(drz)+a(4.0,1.0)*
     . dcos(drz)**5-4.0*a(4.0,1.0)*dcos(drz)**3*dsin(drz)**2-2.0*a(
     . 3.0,2.0)*dcos(drz)**4*dsin(drz)+3.0*a(3.0,2.0)*dcos(drz)**2*
     . dsin(drz)**3+3.0*a(2.0,3.0)*dcos(drz)**3*dsin(drz)**2-2.0*a(
     . 2.0,3.0)*dcos(drz)*dsin(drz)**4-4.0*a(1.0,4.0)*dcos(drz)**2*
     . dsin(drz)**3+a(1.0,4.0)*dsin(drz)**5+5.0*a(0.0,5.0)*dcos(drz)*
     . dsin(drz)**4
      anew(5.0,0.0)=a(5.0,0.0)*dcos(drz)**5-a(4.0,1.0)*dcos(drz)**4*
     . dsin(drz)+a(3.0,2.0)*dcos(drz)**3*dsin(drz)**2-a(2.0,3.0)*dcos
     . (drz)**2*dsin(drz)**3+a(1.0,4.0)*dcos(drz)*dsin(drz)**4-a(0.0,
     . 5.0)*dsin(drz)**5

c UF back correction of the direction
      drz=- drz
      return
      end
c /afs/psi.ch/user/f/flechsig/phase/src/phase/misali1.for
