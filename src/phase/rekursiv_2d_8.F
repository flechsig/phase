c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

c ******************************************************************************
c
c   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
c                      Paul Scherrer Institut Villigen, Switzerland
c   
c   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
c          Uwe Flechsig,    uwe.flechsig@psi.ch
c
c ------------------------------------------------------------------------------
c
c   This file is part of PHASE.
c
c   PHASE is free software: you can redistribute it and/or modify
c   it under the terms of the GNU General Public License as published by
c   the Free Software Foundation, version 3 of the License, or
c   (at your option) any later version.
c
c   PHASE is distributed in the hope that it will be useful,
c   but WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU General Public License for more details.
c
c   You should have received a copy of the GNU General Public License
c   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
c
c ******************************************************************************


#define DEBUG1 
#define IADIM  0:1716
cc#define IADIM 1000000
ccc max ianzl1 is  1716, 

c---------------------------------------------------------------
c
c	Spiegelkoordinaten bis 8. Ordnung
c	Entwicklung der Abbildung bis 7.Ordnung 
c	hier rekursive Loesung der Gleichungssysteme
c
c--------------------------------------------------------
	subroutine subd345a(acc,eq1c,eq2c,ypca,zpca,imode,iord)
c UF	subroutine subd345a(acc,eq1c,ypca,zpca,imode,iord)
c--------------------------------------------------------
c	imode = 1: use 1D Newton Raphson
c	imode = 2: use 2D Newton Raphson

        implicit real*8 (a-h,o-z)

	integer ianz1max  
        integer iac1,iac2,iac3,iac4

        dimension eq1c(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension eq2c(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension ypca(0:7,0:7,0:7,0:7)
        dimension zpca(0:7,0:7,0:7,0:7)
c        dimension ia(0:7,0:7),ic(0:7,0:7)
       
        dimension iac1(IADIM,0:7),iac2(IADIM,0:7),
     &            iac3(IADIM,0:7),iac4(IADIM,0:7)
c        dimension iac1l(IADIM,0:7)

c UF 20.5.13
c#ifdef DEBUG1       
c        write(*,*)'debug: rekursiv_2d_8.F subd345a called'
c#endif
	ianz1max=0
c end UF

	if(imode.eq.1)then
c--------- get ypca(0,0,0,0)
	call newton_1D(acc,eq1c,eq2c,ypca,zpca,iord)
	endif

	if(imode.eq.2)then
c--------- get ypca(0,0,0,0) and zpca(0,0,0,0)

	call newton_2D(acc,eq1c,eq2c,ypca,zpca,iord)
	endif

c--------------------------------------------------------
c	Vorfaktoren
c--------------------------------------------------------

	a1=0.d0
	a2=0.d0
	b1=0.d0
	b2=0.d0

	do n5=0,iord
	do n6=0,iord-n5

c UF
c#ifdef DEBUG1
c	   write(*,*)'debug: loop n5,n6', n5,n6
c#endif
c end uf

	if(n5-1.ge.0)then
	a1=a1+
     &          eq1c(0,0,0,0,n5,n6)*
     &          exp_special(ypca(0,0,0,0),(n5-1))*
     &          dflotj(n5)*
     &          exp_special(zpca(0,0,0,0),n6)
	a2=a2+
     &          eq2c(0,0,0,0,n5,n6)*
     &          exp_special(ypca(0,0,0,0),(n5-1))*
     &          dflotj(n5)*
     &          exp_special(zpca(0,0,0,0),n6)
	endif



c---------------------------------------------------------

	if(n6-1.ge.0)then
	b1=b1+
     &          eq1c(0,0,0,0,n5,n6)*
     &          exp_special(ypca(0,0,0,0),n5)*
     &          dflotj(n6)*
     &          exp_special(zpca(0,0,0,0),(n6-1))
	b2=b2+
     &          eq2c(0,0,0,0,n5,n6)*
     &          exp_special(ypca(0,0,0,0),n5)*
     &          dflotj(n6)*
     &          exp_special(zpca(0,0,0,0),(n6-1))
	endif

c---------------------------------------------------------

	enddo
	enddo

c uf
c#ifdef DEBUG1
c	   write(*,*)'end loop n5,n6', n5,n6
c#endif
c end uf

c--------------------------------------------------------
c	Ende Vorfaktoren
c	jetzt Konstanten c1 und c2 bestimmen
c--------------------------------------------------------

c	zu bestimmende Konstanten festlegen (ikst)
	do ii=0,iord
	do ik=0,iord-ii
	do is=0,iord-ii-ik
	do it=0,iord-ii-ik-is

	if(ii+ik+is+it.gt.0)then

	c1=0.d0
	c2=0.d0

c------- aeussere Summen -------------------------------

	do ij=0,ii
	do im=0,ik
	do iu=0,is
	do iv=0,it

	do n5=0,iord-ij-im-iu-iv
	do n6=0,iord-ij-im-iu-iv-n5

c------- innere Summe ----------------------------------
c	Die innere Summe nennen wir sum
c
c	Die innere Summe berechnen fuer gegebene n5 und n6, 
c	d.h. gegebene Anzahl von Faktoren
c	n5+n6 laeuft von null bis ord
c	ACHTUNG: 
c	- falls n5=0 oder n6=0, d.h. Zahl der Faktoren a oder c gleich null	
c	tritt hier trotzdem jeweils der Faktor 1 auf. 
c	Es gibt also einen Beitrag zu sum
c--------------------------------------------------------
       sum=0.d0

	ianz1=0		! Kombis in 1 Spalte
	ianz2=0		! in zweiter Spalte
	ianz3=0
	ianz4=0

	if(n5+n6.eq.0)then
	  if((ii-ij.ne.0).or.(ik-im.ne.0).or.
     &       (is-iu.ne.0).or.(it-iv.ne.0))then
	  sum=0.d0
	  else 
	  sum=1.d0
	  endif
	  goto 200
	endif

c--------- erste Spalte
	call spaltenkombi(n5,n6,ii,ij,ianz1,iac1)

c uf
#ifdef DEBUG1
	if(ianz1.gt.ianz1max)then 
	   ianz1max=ianz1
	endif 
#endif
c end uf

c--------- zweite Spalte
	call spaltenkombi(n5,n6,ik,im,ianz2,iac2)

c--------- dritte Spalte
	call spaltenkombi(n5,n6,is,iu,ianz3,iac3)

c--------- vierte Spalte
	call spaltenkombi(n5,n6,it,iv,ianz4,iac4)

c-----------------------------------------------------------
c	jetzt gueltige Kombinationen der verschiedenen Kombinationen 
c	fuer die Spalten 1.-4. suchen
c	bei jeder gueltigen Kombination gleich sum anpassen, damit 
c	Kombi-Kombinationen nicht zwischengespeichert werden muessen
c-----------------------------------------------------------
c	ACHTUNG, es gibt immer mindesten eine gueltige Spaltenkombination
c	und zwar: (0,0,0,0)  (0,0,0,0)
c-----------------------------------------------------------

	do m1=1,ianz1	! loop ueber alle gueltige Spaltenkombinationen
	do m2=1,ianz2
	do m3=1,ianz3
	do m4=1,ianz4

	iflag=1		! 1) gueltige Kombination
	  do mm1=1,n5
	    if(iac1(m1,mm1)+iac2(m2,mm1)+iac3(m3,mm1)+iac4(m4,mm1).
     &       ge.ii+ik+is+it)iflag=0
	  enddo

	  do mm2=1,n6
	    if(iac1(m1,mm2)+iac2(m2,mm2)+iac3(m3,mm2)+iac4(m4,mm2).
     &       ge.ii+ik+is+it)iflag=0
	  enddo

c uf
#ifdef DEBUG2
	   write(*,*)'trace 1'
#endif
c end uf

	  if(iflag.eq.1)then

	    prod=1.d0

	    do mm1=1,n5	
	    prod=prod*ypca(iac1(m1,mm1),iac2(m2,mm1),
     &		          iac3(m3,mm1),iac4(m4,mm1))
	    enddo
	    do mm2=n5+1,n5+n6	
	    prod=prod*zpca(iac1(m1,mm2),iac2(m2,mm2),
     &                    iac3(m3,mm2),iac4(m4,mm2))
	    enddo

	    sum=sum+prod

	  endif

	enddo
	enddo
	enddo
	enddo

c uf
#ifdef DEBUG2
	   write(*,*)'trace 2'
#endif
c end uf

200	continue	
c------------------- innere Summe sum(n5,n6) ist jetzt bekannt

	c1=c1+eq1c(ij,im,iu,iv,n5,n6)*sum

	c2=c2+eq2c(ij,im,iu,iv,n5,n6)*sum

	enddo	! loop ueber n6
	enddo	! loop ueber n5		

	enddo	! loop ueber j
	enddo	! m
	enddo	! u
	enddo	! v

c uf
#ifdef DEBUG3
	write(*,*)'trace 3'
#endif
c end uf

c---------------------------------------------------------
c	jetz sind die Konstanten c1(i,k,s,t) und c2(i,k,s,t) bekannt
c	Gleichungssystem loesen
c---------------------------------------------------------

      acc_loc=1.d-12

	if((dabs(a1).gt.acc_loc).and.(dabs(a2).gt.acc_loc))then
	  zpca(ii,ik,is,it)=-(c1/a1-c2/a2)/(b1/a1-b2/a2)
	  ypca(ii,ik,is,it)=-c1/a1-(b1/a1)* zpca(ii,ik,is,it)
	 else
	  if((dabs(a1).lt.acc_loc).and.(dabs(a2).gt.acc_loc))then	  
	    zpca(ii,ik,is,it)=-c1/b1
	    ypca(ii,ik,is,it)=-c2/a2-(b2/a2)*zpca(ii,ik,is,it)
	  endif
	  if((dabs(a2).lt.acc_loc).and.(dabs(a1).gt.acc_loc))then
	    zpca(ii,ik,is,it)=-c2/b2
	    ypca(ii,ik,is,it)=-c1/a1-(b1/a1)*zpca(ii,ik,is,it)
	  endif
	endif

	endif

c uf
c#ifdef DEBUG1
c	   write(*,*)'debug: trace 4, it,is,ik,ii',it,is,ik,ii
c#endif
c end uf

	enddo		! loop �ber it
	enddo		! is
	enddo		! ik
	enddo		! ii
c uf
c#ifdef DEBUG1       
c        write(*,*)'debug: rekursiv_2d_8.F subd345a end ianz1max=',ianz1max
c#endif
c end uf

	return
	end
c-----  end  subd345a -----

c-----------------------------------------------------------
	subroutine spaltenkombi(n5,n6,ii,ij,ianz1l,iac1l)
c-----------------------------------------------------------
c	Berechnung aller erlaubten Kombinationen einer Spalte 
c-----------------------------------------------------------
c	BEISPIEL
c-----------------------------------------------------------
c	Namensgebung der gueltigen Spaltenkombination (m) 
c	fuer (n5,n6)=(3,2);
c	n5 und n6 sind unabhaengig von i,k,s,t 
c	und es kann sogar gelten n5 oder n6 > i+k+s+t
c	
c	n5=3
c
c			Spalte 1	Spalte 2	Spalte 3	Spalte 4
c
c	Faktor 1	iac1(m1,1)	iac2(m2,1)	iac3(m3,1)	iac4(m4,1)		
c	Faktor 2	iac1(m1,2)	iac2(m2,2)	iac3(m3,2)	iac4(m4,2)
c	Faktor 3	iac1(m1,3)	iac2(m2,3)	iac3(m3,3)	iac4(m4,3)
c
c	n6=2	
c
c			Spalte 1	Spalte 2	Spalte 3	Spalte 4
c
c	Faktor 1	iac1(m1,1)	iac2(m2,1)	iac3(m3,1)	iac4(m4,2)
c	Faktor 2	iac1(m1,2)	iac2(m2,2)	iac3(m3,2)	iac4(m4,2)
c
c	Anzahl gueltiger Spaltenkombinationen 
c	(wird wieder auf null gesetzt fuer jede neue Kombination (n5,n6))ue
c
c	Spalte 1	Spalte 2	Spalte 3	Spalte 4
c	ianz1		ianz2		ianz3		ianz4
c	
c	Wenn alle Spaltenkombinationen ermittelt sind, diese Kombinationen
c	miteinander kombinieren und dabei pruefen, ob die Bedingungen gelten:
c	iac1(m,1)+lac2(m,1)+lac3(m,1)+lac4(m,1) .lt. i+k+s+t 
c	iac1(m,2)+lac2(m,2)+lac3(m,2)+lac4(m,2) .lt. i+k+s+t 
c	iac1(m,3)+lac2(m,3)+lac3(m,3)+lac4(m,3) .lt. i+k+s+t 
c
c	iac1(m,1)+lac2(m,1)+lac3(m,1)+lac4(m,1) .lt. i+k+s+t 
c	iac1(m,2)+lac2(m,2)+lac3(m,2)+lac4(m,2) .lt. i+k+s+t 
c	etc	
c	Fall das der Fall ist gleich das Produkt der Klammer berechnen und
c	zu sum dazuaddieren
c------------------------------------------------------------------------

        implicit real*8 (a-h,o-z)

c        dimension eq1c(0:7,0:7,0:7,0:7,0:7,0:7)
c        dimension eq2c(0:7,0:7,0:7,0:7,0:7,0:7)
c        dimension ypca(0:7,0:7,0:7,0:7)
c        dimension zpca(0:7,0:7,0:7,0:7)
c        dimension ia(0:7,0:7),ic(0:7,0:7)
c        dimension iac1(IADIM,0:7),iac2(IADIM,0:7),
c     &            iac3(IADIM,0:7),iac4(IADIM,0:7)
        integer   iac1l
        dimension iac1l(IADIM,0:7)

        if(ij.lt.0)then
          idelta=ii
          else
          idelta=ii-ij
        endif

	ianz1l=0

	  do l1=0,ii
	    if(n5+n6.eq.1)then		! 1 Faktor
	      if(l1.eq.idelta)then
		  ianz1l=ianz1l+1
		  iac1l(ianz1l,1)=l1
	 	  goto 100
            endif
	    endif
		
	  do l2=0,ii-l1
	    if(n5+n6.eq.2)then		! 2 Faktoren
		if(l1+l2.eq.idelta)then
		  ianz1l=ianz1l+1
		  iac1l(ianz1l,1)=l1
		  iac1l(ianz1l,2)=l2
 		  goto 101
		endif
	    endif

	  do l3=0,ii-l1-l2
	    if(n5+n6.eq.3)then		! 3 Faktoren
		if(l1+l2+l3.eq.idelta)then
		  ianz1l=ianz1l+1
		  iac1l(ianz1l,1)=l1
		  iac1l(ianz1l,2)=l2
		  iac1l(ianz1l,3)=l3
 		  goto 102
		endif
	    endif

  	  do l4=0,ii-l1-l2-l3
	    if(n5+n6.eq.4)then		! 4 Faktoren
		if(l1+l2+l3+l4.eq.idelta)then
		  ianz1l=ianz1l+1
		  iac1l(ianz1l,1)=l1
		  iac1l(ianz1l,2)=l2
		  iac1l(ianz1l,3)=l3
		  iac1l(ianz1l,4)=l4
 		  goto 103
		endif
	    endif

  	  do l5=0,ii-l1-l2-l3-l4
	    if(n5+n6.eq.5)then		! 5 Faktoren
		if(l1+l2+l3+l4+l5.eq.idelta)then
		  ianz1l=ianz1l+1
		  iac1l(ianz1l,1)=l1
		  iac1l(ianz1l,2)=l2
		  iac1l(ianz1l,3)=l3
		  iac1l(ianz1l,4)=l4
		  iac1l(ianz1l,5)=l5
 		  goto 104
		endif
	    endif

	  do l6=0,ii-l1-l2-l3-l4-l5
	    if(n5+n6.eq.6)then		! 6 Faktoren
		if(l1+l2+l3+l4+l5+l6.eq.idelta)then
		  ianz1l=ianz1l+1
		  iac1l(ianz1l,1)=l1
		  iac1l(ianz1l,2)=l2
		  iac1l(ianz1l,3)=l3
		  iac1l(ianz1l,4)=l4
		  iac1l(ianz1l,5)=l5
		  iac1l(ianz1l,6)=l6
		  goto 105
		endif
	    endif

	  do l7=0,ii-l1-l2-l3-l4-l5-l6
	    if(n5+n6.eq.7)then		! 7 Faktoren
		if(l1+l2+l3+l4+l5+l6+l7.eq.idelta)then
		  ianz1l=ianz1l+1
		  iac1l(ianz1l,1)=l1
		  iac1l(ianz1l,2)=l2
		  iac1l(ianz1l,3)=l3
		  iac1l(ianz1l,4)=l4
		  iac1l(ianz1l,5)=l5
		  iac1l(ianz1l,6)=l6
		  iac1l(ianz1l,7)=l7
		endif
	    endif

	  enddo
105	  enddo
104	  enddo
103	  enddo
102	  enddo
101	  enddo
100	  enddo

c uf
#ifdef DEBUG2
	  write(*,*)'debug end spaltenkombi, index ianz1l= ', ianz1l
#endif
c end uf

       return
       end

c-------------------------------------------------
	subroutine newton_1D(acc,eq1c,eq2c,ypca,zpca,iord)
c
c	one dimensional Newton-Raphson Method
c-------------------------------------------------

        implicit real*8 (a-h,o-z)

        dimension eq1c(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension eq2c(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension ypca(0:7,0:7,0:7,0:7)
        dimension zpca(0:7,0:7,0:7,0:7)

	ypca(0,0,0,0)=0.d0
	zpca(0,0,0,0)=0.d0

10	continue

	e1=0.d0
	e1y=0.d0

	do i=0,iord
	e1=e1+eq1c(0,0,0,0,i,0)*
     &     exp_special(ypca(0,0,0,0),i)

	if(i.gt.0)then
	e1y=e1y+eq1c(0,0,0,0,i,0)*
     &     dflotj(i)*exp_special(ypca(0,0,0,0),(i-1))
	endif

	enddo

	deltay=-e1/e1y

	ypca(0,0,0,0)=ypca(0,0,0,0)+deltay

	if(dabs(deltay).gt.acc)goto 10

	return
	end

c-------------------------------------------------
	subroutine newton_2D(acc,eq1c,eq2c,ypca,zpca,iord)
c	two dimensional Newton-Raphson Method
c     rather unprecise on a PC
c-------------------------------------------------

        implicit real*8 (a-h,o-z)

        dimension eq1c(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension eq2c(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension ypca(0:7,0:7,0:7,0:7)
        dimension zpca(0:7,0:7,0:7,0:7)

	ypca(0,0,0,0)=0.d0
	zpca(0,0,0,0)=0.d0

ccUF 9.8.2011      acc=1.e-10
10	continue

	e1=0.d0
	e2=0.d0
	e1y=0.d0
	e2y=0.d0
	e1z=0.d0
	e2z=0.d0

	do i=0,iord
	do j=0,iord-i
	e1=e1+eq1c(0,0,0,0,i,j)*
     &     exp_special(ypca(0,0,0,0),i)*
     &     exp_special(zpca(0,0,0,0),j)
	e2=e2+eq2c(0,0,0,0,i,j)*
     &     exp_special(ypca(0,0,0,0),i)*
     &     exp_special(zpca(0,0,0,0),j)
	if(i.gt.0)then
	e1y=e1y+eq1c(0,0,0,0,i,j)*
     &     dflotj(i)*exp_special(ypca(0,0,0,0),(i-1))*
     &     exp_special(zpca(0,0,0,0),j)
	e2y=e2y+eq2c(0,0,0,0,i,j)*
     &     dflotj(i)*exp_special(ypca(0,0,0,0),(i-1))*
     &     exp_special(zpca(0,0,0,0),j)
	endif

	if(j.gt.0)then
	e1z=e1z+eq1c(0,0,0,0,i,j)*
     &     dflotj(j)*exp_special(ypca(0,0,0,0),i)*
     &     exp_special(zpca(0,0,0,0),(j-1))
	e2z=e2z+eq2c(0,0,0,0,i,j)*
     &     dflotj(j)*exp_special(ypca(0,0,0,0),i)*
     &     exp_special(zpca(0,0,0,0),(j-1))
	endif

	enddo
	enddo

	deltay=(-e1*e2z+e1z*e2)/(e1y*e2z-e1z*e2y)
	deltaz=(e1*e2y-e1y*e2)/(e1y*e2z-e1z*e2y)

	ypca(0,0,0,0)=ypca(0,0,0,0)+deltay
	zpca(0,0,0,0)=zpca(0,0,0,0)+deltaz

	if((dabs(deltay).gt.acc).or.(dabs(deltaz).gt.acc))goto 10

	return
	end

c------------------------------------------------------
	function exp_special(xx,i)
c------------------------------------------------------

	implicit real*8(a-h,o-z)

	if(i.eq.0)then
	exp_special=1.d0
	else
	exp_special=xx**i
	endif	

	return
	end
