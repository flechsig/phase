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


#define IADIM  0:1716
c--------------------------------------------------------
	subroutine replace_6v4v(fun6,ypc1,zpc1,fun4,iord)
c--------------------------------------------------------

        implicit real*8 (a-h,o-z)

        dimension fun6(0:7,0:7,0:7,0:7,0:7,0:7)
        dimension fun4(0:7,0:7,0:7,0:7)
        dimension ypc1(0:7,0:7,0:7,0:7)
        dimension zpc1(0:7,0:7,0:7,0:7)
c UF        dimension ia(0:7,0:7),ic(0:7,0:7)
        dimension iac1(IADIM,0:7),iac2(IADIM,0:7),
     &            iac3(IADIM,0:7),iac4(IADIM,0:7)
c UF        dimension iac1l(IADIM,0:7)
		
c	zu bestimmende Konstanten festlegen (ikst)
	do ii=0,iord
	do ik=0,iord-ii
	do is=0,iord-ii-ik
	do it=0,iord-ii-ik-is

	fun4(ii,ik,is,it)=0.d0

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
c	n5+n6 läuft von null bis ord
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
c	ACHTUNG, es gibt immer mindesten eine gültige Spaltenkombination
c	und zwar: (0,0,0,0)  (0,0,0,0)
c-----------------------------------------------------------

	do m1=1,ianz1	! loop über alle gültige Spaltenkombinationen
	do m2=1,ianz2
	do m3=1,ianz3
	do m4=1,ianz4

	iflag=1		! 1) gueltige Kombination
	  do mm1=1,n5
	    if(iac1(m1,mm1)+iac2(m2,mm1)+iac3(m3,mm1)+iac4(m4,mm1).
c     Achtung. hier "gt"
     &       gt.ii+ik+is+it)iflag=0
	  enddo

	  do mm2=1,n6
	    if(iac1(m1,mm2)+iac2(m2,mm2)+iac3(m3,mm2)+iac4(m4,mm2).
c     Achtung. hier "gt"
     &       gt.ii+ik+is+it)iflag=0
	  enddo

	  if(iflag.eq.1)then

	    prod=1.d0

	    do mm1=1,n5	
	    prod=prod*ypc1(iac1(m1,mm1),iac2(m2,mm1),
     &		            iac3(m3,mm1),iac4(m4,mm1))

	    enddo
	    do mm2=n5+1,n5+n6	
	    prod=prod*zpc1(iac1(m1,mm2),iac2(m2,mm2),
     &                      iac3(m3,mm2),iac4(m4,mm2))

	    enddo

	    sum=sum+prod

	  endif		! iflag = 1

	enddo	! loops ueber Spaltenkombis
	enddo
	enddo
	enddo

200	continue	
c------------------- innere Summe sum(n5,n6) ist jetzt bekannt

	fun4(ii,ik,is,it)=fun4(ii,ik,is,it)+
     &    fun6(ij,im,iu,iv,n5,n6)*sum
	
	enddo	! loop ueber n6
	enddo	! loop ueber n5		

	enddo	! loop ueber j
	enddo	! m
	enddo	! u
	enddo	! v

	enddo		! loop über it
	enddo		! is
	enddo		! ik
	enddo		! ii

	return
	end
