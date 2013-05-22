c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

#define IADIM  0:1716
c--------------------------------------------------------
	subroutine replace_wl_in_u(a,wca,xlca,a1,iord)
c--------------------------------------------------------

        implicit real*8 (a-h,o-z)

        dimension a(0:8,0:8)
        dimension a1(0:7,0:7,0:7,0:7)
        dimension wca(0:7,0:7,0:7,0:7)
        dimension xlca(0:7,0:7,0:7,0:7)
c uf        dimension ypca1(0:7,0:7,0:7,0:7)
c uf        dimension zpca1(0:7,0:7,0:7,0:7)
c uf        dimension ia(0:7,0:7),ic(0:7,0:7)
        dimension iac1(IADIM,0:7),iac2(IADIM,0:7),
     &            iac3(IADIM,0:7),iac4(IADIM,0:7)
c uf        dimension iac1l(IADIM,0:7)

c	zu bestimmende Konstanten festlegen (ikst)
	do ii=0,iord
	do ik=0,iord-ii
	do is=0,iord-ii-ik
	do it=0,iord-ii-ik-is

	a1(ii,ik,is,it)=0.d0

c------- aeussere Summen -------------------------------

	do n5=0,iord
	do n6=0,iord-n5

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
	  sum=0.d0
	  goto 200
	endif

c--------- erste Spalte
	call spaltenkombi(n5,n6,ii,-1,ianz1,iac1)

c--------- zweite Spalte
	call spaltenkombi(n5,n6,ik,-1,ianz2,iac2)

c--------- dritte Spalte
	call spaltenkombi(n5,n6,is,-1,ianz3,iac3)

c--------- vierte Spalte
	call spaltenkombi(n5,n6,it,-1,ianz4,iac4)

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

c------------- Isabelle fragen, vermutlich auskommentieren
c	  do mm1=1,n5
c	    if(iac1(m1,mm1)+iac2(m2,mm1)+iac3(m3,mm1)+iac4(m4,mm1).
c     &       ge.ii+ik+is+it)iflag=0
c	  enddo
c
c	  do mm2=1,n6
c	    if(iac1(m1,mm2)+iac2(m2,mm2)+iac3(m3,mm2)+iac4(m4,mm2).
c     &       ge.ii+ik+is+it)iflag=0
c	  enddo
c--------------------------------------------

	  if(iflag.eq.1)then

	    prod=1.d0

	    do mm1=1,n5	
	    prod=prod*wca(iac1(m1,mm1),iac2(m2,mm1),
     &		          iac3(m3,mm1),iac4(m4,mm1))
	    enddo
	    do mm2=n5+1,n5+n6	
	    prod=prod*xlca(iac1(m1,mm2),iac2(m2,mm2),
     &                    iac3(m3,mm2),iac4(m4,mm2))
	    enddo

	    sum=sum+prod

	  endif

	enddo
	enddo
	enddo
	enddo

200	continue	
c------------------- innere Summe sum(n5,n6) ist jetzt bekannt

	a1(ii,ik,is,it)=a1(ii,ik,is,it)+a(n5,n6)*sum

	enddo	! loop ueber n6
	enddo	! loop ueber n5		

	enddo	! loop ueber t
	enddo	! s
	enddo	! k
	enddo	! i

	return
	end

