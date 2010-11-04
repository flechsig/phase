      subroutine misali1_8(a,anew,drz)
      implicit real*8(a-h,o-z)
      dimension a(0:8,0:8),anew(0:8,0:8)
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
      anew(0.0,6.0)=a(6.0,0.0)*dsin(drz)**6+a(5.0,1.0)*dcos(drz)*dsin
     . (drz)**5+a(4.0,2.0)*dcos(drz)**2*dsin(drz)**4+a(3.0,3.0)*dcos(
     . drz)**3*dsin(drz)**3+a(2.0,4.0)*dcos(drz)**4*dsin(drz)**2+a(
     . 1.0,5.0)*dcos(drz)**5*dsin(drz)+a(0.0,6.0)*dcos(drz)**6
      anew(0.0,7.0)=a(7.0,0.0)*dsin(drz)**7+a(6.0,1.0)*dcos(drz)*dsin
     . (drz)**6+a(5.0,2.0)*dcos(drz)**2*dsin(drz)**5+a(4.0,3.0)*dcos(
     . drz)**3*dsin(drz)**4+a(3.0,4.0)*dcos(drz)**4*dsin(drz)**3+a(
     . 2.0,5.0)*dcos(drz)**5*dsin(drz)**2+a(1.0,6.0)*dcos(drz)**6*
     . dsin(drz)+a(0.0,7.0)*dcos(drz)**7
      anew(0.0,8.0)=a(8.0,0.0)*dsin(drz)**8+a(7.0,1.0)*dcos(drz)*dsin
     . (drz)**7+a(6.0,2.0)*dcos(drz)**2*dsin(drz)**6+a(5.0,3.0)*dcos(
     . drz)**3*dsin(drz)**5+a(4.0,4.0)*dcos(drz)**4*dsin(drz)**4+a(
     . 3.0,5.0)*dcos(drz)**5*dsin(drz)**3+a(2.0,6.0)*dcos(drz)**6*
     . dsin(drz)**2+a(1.0,7.0)*dcos(drz)**7*dsin(drz)+a(0.0,8.0)*dcos
     . (drz)**8
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
      anew(1.0,5.0)=6.0*a(6.0,0.0)*dcos(drz)*dsin(drz)**5+5.0*a(5.0,
     . 1.0)*dcos(drz)**2*dsin(drz)**4-a(5.0,1.0)*dsin(drz)**6+4.0*a(
     . 4.0,2.0)*dcos(drz)**3*dsin(drz)**3-2.0*a(4.0,2.0)*dcos(drz)*
     . dsin(drz)**5+3.0*a(3.0,3.0)*dcos(drz)**4*dsin(drz)**2-3.0*a(
     . 3.0,3.0)*dcos(drz)**2*dsin(drz)**4+2.0*a(2.0,4.0)*dcos(drz)**5
     . *dsin(drz)-4.0*a(2.0,4.0)*dcos(drz)**3*dsin(drz)**3+a(1.0,5.0)
     . *dcos(drz)**6-5.0*a(1.0,5.0)*dcos(drz)**4*dsin(drz)**2-6.0*a(
     . 0.0,6.0)*dcos(drz)**5*dsin(drz)
      anew(1.0,6.0)=7.0*a(7.0,0.0)*dcos(drz)*dsin(drz)**6+6.0*a(6.0,
     . 1.0)*dcos(drz)**2*dsin(drz)**5-a(6.0,1.0)*dsin(drz)**7+5.0*a(
     . 5.0,2.0)*dcos(drz)**3*dsin(drz)**4-2.0*a(5.0,2.0)*dcos(drz)*
     . dsin(drz)**6+4.0*a(4.0,3.0)*dcos(drz)**4*dsin(drz)**3-3.0*a(
     . 4.0,3.0)*dcos(drz)**2*dsin(drz)**5+3.0*a(3.0,4.0)*dcos(drz)**5
     . *dsin(drz)**2-4.0*a(3.0,4.0)*dcos(drz)**3*dsin(drz)**4+2.0*a(
     . 2.0,5.0)*dcos(drz)**6*dsin(drz)-5.0*a(2.0,5.0)*dcos(drz)**4*
     . dsin(drz)**3+a(1.0,6.0)*dcos(drz)**7-6.0*a(1.0,6.0)*dcos(drz)
     . **5*dsin(drz)**2-7.0*a(0.0,7.0)*dcos(drz)**6*dsin(drz)
      anew(1.0,7.0)=8.0*a(8.0,0.0)*dcos(drz)*dsin(drz)**7+7.0*a(7.0,
     . 1.0)*dcos(drz)**2*dsin(drz)**6-a(7.0,1.0)*dsin(drz)**8+6.0*a(
     . 6.0,2.0)*dcos(drz)**3*dsin(drz)**5-2.0*a(6.0,2.0)*dcos(drz)*
     . dsin(drz)**7+5.0*a(5.0,3.0)*dcos(drz)**4*dsin(drz)**4-3.0*a(
     . 5.0,3.0)*dcos(drz)**2*dsin(drz)**6+4.0*a(4.0,4.0)*dcos(drz)**5
     . *dsin(drz)**3-4.0*a(4.0,4.0)*dcos(drz)**3*dsin(drz)**5+3.0*a(
     . 3.0,5.0)*dcos(drz)**6*dsin(drz)**2-5.0*a(3.0,5.0)*dcos(drz)**4
     . *dsin(drz)**4+2.0*a(2.0,6.0)*dcos(drz)**7*dsin(drz)-6.0*a(2.0,
     . 6.0)*dcos(drz)**5*dsin(drz)**3+a(1.0,7.0)*dcos(drz)**8-7.0*a(
     . 1.0,7.0)*dcos(drz)**6*dsin(drz)**2-8.0*a(0.0,8.0)*dcos(drz)**7
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
      anew(2.0,4.0)=15.0*a(6.0,0.0)*dcos(drz)**2*dsin(drz)**4+10.0*a(
     . 5.0,1.0)*dcos(drz)**3*dsin(drz)**3-5.0*a(5.0,1.0)*dcos(drz)*
     . dsin(drz)**5+6.0*a(4.0,2.0)*dcos(drz)**4*dsin(drz)**2-8.0*a(
     . 4.0,2.0)*dcos(drz)**2*dsin(drz)**4+a(4.0,2.0)*dsin(drz)**6+3.0
     . *a(3.0,3.0)*dcos(drz)**5*dsin(drz)-9.0*a(3.0,3.0)*dcos(drz)**3
     . *dsin(drz)**3+3.0*a(3.0,3.0)*dcos(drz)*dsin(drz)**5+a(2.0,4.0)
     . *dcos(drz)**6-8.0*a(2.0,4.0)*dcos(drz)**4*dsin(drz)**2+6.0*a(
     . 2.0,4.0)*dcos(drz)**2*dsin(drz)**4-5.0*a(1.0,5.0)*dcos(drz)**5
     . *dsin(drz)+10.0*a(1.0,5.0)*dcos(drz)**3*dsin(drz)**3+15.0*a(
     . 0.0,6.0)*dcos(drz)**4*dsin(drz)**2
      anew(2.0,5.0)=21.0*a(7.0,0.0)*dcos(drz)**2*dsin(drz)**5+15.0*a(
     . 6.0,1.0)*dcos(drz)**3*dsin(drz)**4-6.0*a(6.0,1.0)*dcos(drz)*
     . dsin(drz)**6+10.0*a(5.0,2.0)*dcos(drz)**4*dsin(drz)**3-10.0*a(
     . 5.0,2.0)*dcos(drz)**2*dsin(drz)**5+a(5.0,2.0)*dsin(drz)**7+6.0
     . *a(4.0,3.0)*dcos(drz)**5*dsin(drz)**2-12.0*a(4.0,3.0)*dcos(drz
     . )**3*dsin(drz)**4+3.0*a(4.0,3.0)*dcos(drz)*dsin(drz)**6+3.0*a(
     . 3.0,4.0)*dcos(drz)**6*dsin(drz)-12.0*a(3.0,4.0)*dcos(drz)**4*
     . dsin(drz)**3+6.0*a(3.0,4.0)*dcos(drz)**2*dsin(drz)**5+a(2.0,
     . 5.0)*dcos(drz)**7-10.0*a(2.0,5.0)*dcos(drz)**5*dsin(drz)**2+
     . 10.0*a(2.0,5.0)*dcos(drz)**3*dsin(drz)**4-6.0*a(1.0,6.0)*dcos(
     . drz)**6*dsin(drz)+15.0*a(1.0,6.0)*dcos(drz)**4*dsin(drz)**3+
     . 21.0*a(0.0,7.0)*dcos(drz)**5*dsin(drz)**2
      anew(2.0,6.0)=28.0*a(8.0,0.0)*dcos(drz)**2*dsin(drz)**6+21.0*a(
     . 7.0,1.0)*dcos(drz)**3*dsin(drz)**5-7.0*a(7.0,1.0)*dcos(drz)*
     . dsin(drz)**7+15.0*a(6.0,2.0)*dcos(drz)**4*dsin(drz)**4-12.0*a(
     . 6.0,2.0)*dcos(drz)**2*dsin(drz)**6+a(6.0,2.0)*dsin(drz)**8+
     . 10.0*a(5.0,3.0)*dcos(drz)**5*dsin(drz)**3-15.0*a(5.0,3.0)*dcos
     . (drz)**3*dsin(drz)**5+3.0*a(5.0,3.0)*dcos(drz)*dsin(drz)**7+
     . 6.0*a(4.0,4.0)*dcos(drz)**6*dsin(drz)**2-16.0*a(4.0,4.0)*dcos(
     . drz)**4*dsin(drz)**4+6.0*a(4.0,4.0)*dcos(drz)**2*dsin(drz)**6+
     . 3.0*a(3.0,5.0)*dcos(drz)**7*dsin(drz)-15.0*a(3.0,5.0)*dcos(drz
     . )**5*dsin(drz)**3+10.0*a(3.0,5.0)*dcos(drz)**3*dsin(drz)**5+a(
     . 2.0,6.0)*dcos(drz)**8-12.0*a(2.0,6.0)*dcos(drz)**6*dsin(drz)**
     . 2+15.0*a(2.0,6.0)*dcos(drz)**4*dsin(drz)**4-7.0*a(1.0,7.0)*
     . dcos(drz)**7*dsin(drz)+21.0*a(1.0,7.0)*dcos(drz)**5*dsin(drz)
     . **3+28.0*a(0.0,8.0)*dcos(drz)**6*dsin(drz)**2
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
      anew(3.0,3.0)=20.0*a(6.0,0.0)*dcos(drz)**3*dsin(drz)**3+10.0*a(
     . 5.0,1.0)*dcos(drz)**4*dsin(drz)**2-10.0*a(5.0,1.0)*dcos(drz)**
     . 2*dsin(drz)**4+4.0*a(4.0,2.0)*dcos(drz)**5*dsin(drz)-12.0*a(
     . 4.0,2.0)*dcos(drz)**3*dsin(drz)**3+4.0*a(4.0,2.0)*dcos(drz)*
     . dsin(drz)**5+a(3.0,3.0)*dcos(drz)**6-9.0*a(3.0,3.0)*dcos(drz)
     . **4*dsin(drz)**2+9.0*a(3.0,3.0)*dcos(drz)**2*dsin(drz)**4-a(
     . 3.0,3.0)*dsin(drz)**6-4.0*a(2.0,4.0)*dcos(drz)**5*dsin(drz)+
     . 12.0*a(2.0,4.0)*dcos(drz)**3*dsin(drz)**3-4.0*a(2.0,4.0)*dcos(
     . drz)*dsin(drz)**5+10.0*a(1.0,5.0)*dcos(drz)**4*dsin(drz)**2-
     . 10.0*a(1.0,5.0)*dcos(drz)**2*dsin(drz)**4-20.0*a(0.0,6.0)*dcos
     . (drz)**3*dsin(drz)**3
      anew(3.0,4.0)=35.0*a(7.0,0.0)*dcos(drz)**3*dsin(drz)**4+20.0*a(
     . 6.0,1.0)*dcos(drz)**4*dsin(drz)**3-15.0*a(6.0,1.0)*dcos(drz)**
     . 2*dsin(drz)**5+10.0*a(5.0,2.0)*dcos(drz)**5*dsin(drz)**2-20.0*
     . a(5.0,2.0)*dcos(drz)**3*dsin(drz)**4+5.0*a(5.0,2.0)*dcos(drz)*
     . dsin(drz)**6+4.0*a(4.0,3.0)*dcos(drz)**6*dsin(drz)-18.0*a(4.0,
     . 3.0)*dcos(drz)**4*dsin(drz)**3+12.0*a(4.0,3.0)*dcos(drz)**2*
     . dsin(drz)**5-a(4.0,3.0)*dsin(drz)**7+a(3.0,4.0)*dcos(drz)**7-
     . 12.0*a(3.0,4.0)*dcos(drz)**5*dsin(drz)**2+18.0*a(3.0,4.0)*dcos
     . (drz)**3*dsin(drz)**4-4.0*a(3.0,4.0)*dcos(drz)*dsin(drz)**6-
     . 5.0*a(2.0,5.0)*dcos(drz)**6*dsin(drz)+20.0*a(2.0,5.0)*dcos(drz
     . )**4*dsin(drz)**3-10.0*a(2.0,5.0)*dcos(drz)**2*dsin(drz)**5+
     . 15.0*a(1.0,6.0)*dcos(drz)**5*dsin(drz)**2-20.0*a(1.0,6.0)*dcos
     . (drz)**3*dsin(drz)**4-35.0*a(0.0,7.0)*dcos(drz)**4*dsin(drz)**
     . 3
      anew(3.0,5.0)=56.0*a(8.0,0.0)*dcos(drz)**3*dsin(drz)**5+35.0*a(
     . 7.0,1.0)*dcos(drz)**4*dsin(drz)**4-21.0*a(7.0,1.0)*dcos(drz)**
     . 2*dsin(drz)**6+20.0*a(6.0,2.0)*dcos(drz)**5*dsin(drz)**3-30.0*
     . a(6.0,2.0)*dcos(drz)**3*dsin(drz)**5+6.0*a(6.0,2.0)*dcos(drz)*
     . dsin(drz)**7+10.0*a(5.0,3.0)*dcos(drz)**6*dsin(drz)**2-30.0*a(
     . 5.0,3.0)*dcos(drz)**4*dsin(drz)**4+15.0*a(5.0,3.0)*dcos(drz)**
     . 2*dsin(drz)**6-a(5.0,3.0)*dsin(drz)**8+4.0*a(4.0,4.0)*dcos(drz
     . )**7*dsin(drz)-24.0*a(4.0,4.0)*dcos(drz)**5*dsin(drz)**3+24.0*
     . a(4.0,4.0)*dcos(drz)**3*dsin(drz)**5-4.0*a(4.0,4.0)*dcos(drz)*
     . dsin(drz)**7+a(3.0,5.0)*dcos(drz)**8-15.0*a(3.0,5.0)*dcos(drz)
     . **6*dsin(drz)**2+30.0*a(3.0,5.0)*dcos(drz)**4*dsin(drz)**4-
     . 10.0*a(3.0,5.0)*dcos(drz)**2*dsin(drz)**6-6.0*a(2.0,6.0)*dcos(
     . drz)**7*dsin(drz)+30.0*a(2.0,6.0)*dcos(drz)**5*dsin(drz)**3-
     . 20.0*a(2.0,6.0)*dcos(drz)**3*dsin(drz)**5+21.0*a(1.0,7.0)*dcos
     . (drz)**6*dsin(drz)**2-35.0*a(1.0,7.0)*dcos(drz)**4*dsin(drz)**
     . 4-56.0*a(0.0,8.0)*dcos(drz)**5*dsin(drz)**3
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
      anew(4.0,2.0)=15.0*a(6.0,0.0)*dcos(drz)**4*dsin(drz)**2+5.0*a(
     . 5.0,1.0)*dcos(drz)**5*dsin(drz)-10.0*a(5.0,1.0)*dcos(drz)**3*
     . dsin(drz)**3+a(4.0,2.0)*dcos(drz)**6-8.0*a(4.0,2.0)*dcos(drz)
     . **4*dsin(drz)**2+6.0*a(4.0,2.0)*dcos(drz)**2*dsin(drz)**4-3.0*
     . a(3.0,3.0)*dcos(drz)**5*dsin(drz)+9.0*a(3.0,3.0)*dcos(drz)**3*
     . dsin(drz)**3-3.0*a(3.0,3.0)*dcos(drz)*dsin(drz)**5+6.0*a(2.0,
     . 4.0)*dcos(drz)**4*dsin(drz)**2-8.0*a(2.0,4.0)*dcos(drz)**2*
     . dsin(drz)**4+a(2.0,4.0)*dsin(drz)**6-10.0*a(1.0,5.0)*dcos(drz)
     . **3*dsin(drz)**3+5.0*a(1.0,5.0)*dcos(drz)*dsin(drz)**5+15.0*a(
     . 0.0,6.0)*dcos(drz)**2*dsin(drz)**4
      anew(4.0,3.0)=35.0*a(7.0,0.0)*dcos(drz)**4*dsin(drz)**3+15.0*a(
     . 6.0,1.0)*dcos(drz)**5*dsin(drz)**2-20.0*a(6.0,1.0)*dcos(drz)**
     . 3*dsin(drz)**4+5.0*a(5.0,2.0)*dcos(drz)**6*dsin(drz)-20.0*a(
     . 5.0,2.0)*dcos(drz)**4*dsin(drz)**3+10.0*a(5.0,2.0)*dcos(drz)**
     . 2*dsin(drz)**5+a(4.0,3.0)*dcos(drz)**7-12.0*a(4.0,3.0)*dcos(
     . drz)**5*dsin(drz)**2+18.0*a(4.0,3.0)*dcos(drz)**3*dsin(drz)**4
     . -4.0*a(4.0,3.0)*dcos(drz)*dsin(drz)**6-4.0*a(3.0,4.0)*dcos(drz
     . )**6*dsin(drz)+18.0*a(3.0,4.0)*dcos(drz)**4*dsin(drz)**3-12.0*
     . a(3.0,4.0)*dcos(drz)**2*dsin(drz)**5+a(3.0,4.0)*dsin(drz)**7+
     . 10.0*a(2.0,5.0)*dcos(drz)**5*dsin(drz)**2-20.0*a(2.0,5.0)*dcos
     . (drz)**3*dsin(drz)**4+5.0*a(2.0,5.0)*dcos(drz)*dsin(drz)**6-
     . 20.0*a(1.0,6.0)*dcos(drz)**4*dsin(drz)**3+15.0*a(1.0,6.0)*dcos
     . (drz)**2*dsin(drz)**5+35.0*a(0.0,7.0)*dcos(drz)**3*dsin(drz)**
     . 4
      anew(4.0,4.0)=70.0*a(8.0,0.0)*dcos(drz)**4*dsin(drz)**4+35.0*a(
     . 7.0,1.0)*dcos(drz)**5*dsin(drz)**3-35.0*a(7.0,1.0)*dcos(drz)**
     . 3*dsin(drz)**5+15.0*a(6.0,2.0)*dcos(drz)**6*dsin(drz)**2-40.0*
     . a(6.0,2.0)*dcos(drz)**4*dsin(drz)**4+15.0*a(6.0,2.0)*dcos(drz)
     . **2*dsin(drz)**6+5.0*a(5.0,3.0)*dcos(drz)**7*dsin(drz)-30.0*a(
     . 5.0,3.0)*dcos(drz)**5*dsin(drz)**3+30.0*a(5.0,3.0)*dcos(drz)**
     . 3*dsin(drz)**5-5.0*a(5.0,3.0)*dcos(drz)*dsin(drz)**7+a(4.0,4.0
     . )*dcos(drz)**8-16.0*a(4.0,4.0)*dcos(drz)**6*dsin(drz)**2+36.0*
     . a(4.0,4.0)*dcos(drz)**4*dsin(drz)**4-16.0*a(4.0,4.0)*dcos(drz)
     . **2*dsin(drz)**6+a(4.0,4.0)*dsin(drz)**8-5.0*a(3.0,5.0)*dcos(
     . drz)**7*dsin(drz)+30.0*a(3.0,5.0)*dcos(drz)**5*dsin(drz)**3-
     . 30.0*a(3.0,5.0)*dcos(drz)**3*dsin(drz)**5+5.0*a(3.0,5.0)*dcos(
     . drz)*dsin(drz)**7+15.0*a(2.0,6.0)*dcos(drz)**6*dsin(drz)**2-
     . 40.0*a(2.0,6.0)*dcos(drz)**4*dsin(drz)**4+15.0*a(2.0,6.0)*dcos
     . (drz)**2*dsin(drz)**6-35.0*a(1.0,7.0)*dcos(drz)**5*dsin(drz)**
     . 3+35.0*a(1.0,7.0)*dcos(drz)**3*dsin(drz)**5+70.0*a(0.0,8.0)*
     . dcos(drz)**4*dsin(drz)**4
      anew(5.0,0.0)=a(5.0,0.0)*dcos(drz)**5-a(4.0,1.0)*dcos(drz)**4*
     . dsin(drz)+a(3.0,2.0)*dcos(drz)**3*dsin(drz)**2-a(2.0,3.0)*dcos
     . (drz)**2*dsin(drz)**3+a(1.0,4.0)*dcos(drz)*dsin(drz)**4-a(0.0,
     . 5.0)*dsin(drz)**5
      anew(5.0,1.0)=6.0*a(6.0,0.0)*dcos(drz)**5*dsin(drz)+a(5.0,1.0)*
     . dcos(drz)**6-5.0*a(5.0,1.0)*dcos(drz)**4*dsin(drz)**2-2.0*a(
     . 4.0,2.0)*dcos(drz)**5*dsin(drz)+4.0*a(4.0,2.0)*dcos(drz)**3*
     . dsin(drz)**3+3.0*a(3.0,3.0)*dcos(drz)**4*dsin(drz)**2-3.0*a(
     . 3.0,3.0)*dcos(drz)**2*dsin(drz)**4-4.0*a(2.0,4.0)*dcos(drz)**3
     . *dsin(drz)**3+2.0*a(2.0,4.0)*dcos(drz)*dsin(drz)**5+5.0*a(1.0,
     . 5.0)*dcos(drz)**2*dsin(drz)**4-a(1.0,5.0)*dsin(drz)**6-6.0*a(
     . 0.0,6.0)*dcos(drz)*dsin(drz)**5
      anew(5.0,2.0)=21.0*a(7.0,0.0)*dcos(drz)**5*dsin(drz)**2+6.0*a(
     . 6.0,1.0)*dcos(drz)**6*dsin(drz)-15.0*a(6.0,1.0)*dcos(drz)**4*
     . dsin(drz)**3+a(5.0,2.0)*dcos(drz)**7-10.0*a(5.0,2.0)*dcos(drz)
     . **5*dsin(drz)**2+10.0*a(5.0,2.0)*dcos(drz)**3*dsin(drz)**4-3.0
     . *a(4.0,3.0)*dcos(drz)**6*dsin(drz)+12.0*a(4.0,3.0)*dcos(drz)**
     . 4*dsin(drz)**3-6.0*a(4.0,3.0)*dcos(drz)**2*dsin(drz)**5+6.0*a(
     . 3.0,4.0)*dcos(drz)**5*dsin(drz)**2-12.0*a(3.0,4.0)*dcos(drz)**
     . 3*dsin(drz)**4+3.0*a(3.0,4.0)*dcos(drz)*dsin(drz)**6-10.0*a(
     . 2.0,5.0)*dcos(drz)**4*dsin(drz)**3+10.0*a(2.0,5.0)*dcos(drz)**
     . 2*dsin(drz)**5-a(2.0,5.0)*dsin(drz)**7+15.0*a(1.0,6.0)*dcos(
     . drz)**3*dsin(drz)**4-6.0*a(1.0,6.0)*dcos(drz)*dsin(drz)**6-
     . 21.0*a(0.0,7.0)*dcos(drz)**2*dsin(drz)**5
      anew(5.0,3.0)=56.0*a(8.0,0.0)*dcos(drz)**5*dsin(drz)**3+21.0*a(
     . 7.0,1.0)*dcos(drz)**6*dsin(drz)**2-35.0*a(7.0,1.0)*dcos(drz)**
     . 4*dsin(drz)**4+6.0*a(6.0,2.0)*dcos(drz)**7*dsin(drz)-30.0*a(
     . 6.0,2.0)*dcos(drz)**5*dsin(drz)**3+20.0*a(6.0,2.0)*dcos(drz)**
     . 3*dsin(drz)**5+a(5.0,3.0)*dcos(drz)**8-15.0*a(5.0,3.0)*dcos(
     . drz)**6*dsin(drz)**2+30.0*a(5.0,3.0)*dcos(drz)**4*dsin(drz)**4
     . -10.0*a(5.0,3.0)*dcos(drz)**2*dsin(drz)**6-4.0*a(4.0,4.0)*dcos
     . (drz)**7*dsin(drz)+24.0*a(4.0,4.0)*dcos(drz)**5*dsin(drz)**3-
     . 24.0*a(4.0,4.0)*dcos(drz)**3*dsin(drz)**5+4.0*a(4.0,4.0)*dcos(
     . drz)*dsin(drz)**7+10.0*a(3.0,5.0)*dcos(drz)**6*dsin(drz)**2-
     . 30.0*a(3.0,5.0)*dcos(drz)**4*dsin(drz)**4+15.0*a(3.0,5.0)*dcos
     . (drz)**2*dsin(drz)**6-a(3.0,5.0)*dsin(drz)**8-20.0*a(2.0,6.0)*
     . dcos(drz)**5*dsin(drz)**3+30.0*a(2.0,6.0)*dcos(drz)**3*dsin(
     . drz)**5-6.0*a(2.0,6.0)*dcos(drz)*dsin(drz)**7+35.0*a(1.0,7.0)*
     . dcos(drz)**4*dsin(drz)**4-21.0*a(1.0,7.0)*dcos(drz)**2*dsin(
     . drz)**6-56.0*a(0.0,8.0)*dcos(drz)**3*dsin(drz)**5
      anew(6.0,0.0)=a(6.0,0.0)*dcos(drz)**6-a(5.0,1.0)*dcos(drz)**5*
     . dsin(drz)+a(4.0,2.0)*dcos(drz)**4*dsin(drz)**2-a(3.0,3.0)*dcos
     . (drz)**3*dsin(drz)**3+a(2.0,4.0)*dcos(drz)**2*dsin(drz)**4-a(
     . 1.0,5.0)*dcos(drz)*dsin(drz)**5+a(0.0,6.0)*dsin(drz)**6
      anew(6.0,1.0)=7.0*a(7.0,0.0)*dcos(drz)**6*dsin(drz)+a(6.0,1.0)*
     . dcos(drz)**7-6.0*a(6.0,1.0)*dcos(drz)**5*dsin(drz)**2-2.0*a(
     . 5.0,2.0)*dcos(drz)**6*dsin(drz)+5.0*a(5.0,2.0)*dcos(drz)**4*
     . dsin(drz)**3+3.0*a(4.0,3.0)*dcos(drz)**5*dsin(drz)**2-4.0*a(
     . 4.0,3.0)*dcos(drz)**3*dsin(drz)**4-4.0*a(3.0,4.0)*dcos(drz)**4
     . *dsin(drz)**3+3.0*a(3.0,4.0)*dcos(drz)**2*dsin(drz)**5+5.0*a(
     . 2.0,5.0)*dcos(drz)**3*dsin(drz)**4-2.0*a(2.0,5.0)*dcos(drz)*
     . dsin(drz)**6-6.0*a(1.0,6.0)*dcos(drz)**2*dsin(drz)**5+a(1.0,
     . 6.0)*dsin(drz)**7+7.0*a(0.0,7.0)*dcos(drz)*dsin(drz)**6
      anew(6.0,2.0)=28.0*a(8.0,0.0)*dcos(drz)**6*dsin(drz)**2+7.0*a(
     . 7.0,1.0)*dcos(drz)**7*dsin(drz)-21.0*a(7.0,1.0)*dcos(drz)**5*
     . dsin(drz)**3+a(6.0,2.0)*dcos(drz)**8-12.0*a(6.0,2.0)*dcos(drz)
     . **6*dsin(drz)**2+15.0*a(6.0,2.0)*dcos(drz)**4*dsin(drz)**4-3.0
     . *a(5.0,3.0)*dcos(drz)**7*dsin(drz)+15.0*a(5.0,3.0)*dcos(drz)**
     . 5*dsin(drz)**3-10.0*a(5.0,3.0)*dcos(drz)**3*dsin(drz)**5+6.0*a
     . (4.0,4.0)*dcos(drz)**6*dsin(drz)**2-16.0*a(4.0,4.0)*dcos(drz)
     . **4*dsin(drz)**4+6.0*a(4.0,4.0)*dcos(drz)**2*dsin(drz)**6-10.0
     . *a(3.0,5.0)*dcos(drz)**5*dsin(drz)**3+15.0*a(3.0,5.0)*dcos(drz
     . )**3*dsin(drz)**5-3.0*a(3.0,5.0)*dcos(drz)*dsin(drz)**7+15.0*a
     . (2.0,6.0)*dcos(drz)**4*dsin(drz)**4-12.0*a(2.0,6.0)*dcos(drz)
     . **2*dsin(drz)**6+a(2.0,6.0)*dsin(drz)**8-21.0*a(1.0,7.0)*dcos(
     . drz)**3*dsin(drz)**5+7.0*a(1.0,7.0)*dcos(drz)*dsin(drz)**7+
     . 28.0*a(0.0,8.0)*dcos(drz)**2*dsin(drz)**6
      anew(7.0,0.0)=a(7.0,0.0)*dcos(drz)**7-a(6.0,1.0)*dcos(drz)**6*
     . dsin(drz)+a(5.0,2.0)*dcos(drz)**5*dsin(drz)**2-a(4.0,3.0)*dcos
     . (drz)**4*dsin(drz)**3+a(3.0,4.0)*dcos(drz)**3*dsin(drz)**4-a(
     . 2.0,5.0)*dcos(drz)**2*dsin(drz)**5+a(1.0,6.0)*dcos(drz)*dsin(
     . drz)**6-a(0.0,7.0)*dsin(drz)**7
      anew(7.0,1.0)=8.0*a(8.0,0.0)*dcos(drz)**7*dsin(drz)+a(7.0,1.0)*
     . dcos(drz)**8-7.0*a(7.0,1.0)*dcos(drz)**6*dsin(drz)**2-2.0*a(
     . 6.0,2.0)*dcos(drz)**7*dsin(drz)+6.0*a(6.0,2.0)*dcos(drz)**5*
     . dsin(drz)**3+3.0*a(5.0,3.0)*dcos(drz)**6*dsin(drz)**2-5.0*a(
     . 5.0,3.0)*dcos(drz)**4*dsin(drz)**4-4.0*a(4.0,4.0)*dcos(drz)**5
     . *dsin(drz)**3+4.0*a(4.0,4.0)*dcos(drz)**3*dsin(drz)**5+5.0*a(
     . 3.0,5.0)*dcos(drz)**4*dsin(drz)**4-3.0*a(3.0,5.0)*dcos(drz)**2
     . *dsin(drz)**6-6.0*a(2.0,6.0)*dcos(drz)**3*dsin(drz)**5+2.0*a(
     . 2.0,6.0)*dcos(drz)*dsin(drz)**7+7.0*a(1.0,7.0)*dcos(drz)**2*
     . dsin(drz)**6-a(1.0,7.0)*dsin(drz)**8-8.0*a(0.0,8.0)*dcos(drz)*
     . dsin(drz)**7
      anew(8.0,0.0)=a(8.0,0.0)*dcos(drz)**8-a(7.0,1.0)*dcos(drz)**7*
     . dsin(drz)+a(6.0,2.0)*dcos(drz)**6*dsin(drz)**2-a(5.0,3.0)*dcos
     . (drz)**5*dsin(drz)**3+a(4.0,4.0)*dcos(drz)**4*dsin(drz)**4-a(
     . 3.0,5.0)*dcos(drz)**3*dsin(drz)**5+a(2.0,6.0)*dcos(drz)**2*
     . dsin(drz)**6-a(1.0,7.0)*dcos(drz)*dsin(drz)**7+a(0.0,8.0)*dsin
     . (drz)**8
      return
      end
