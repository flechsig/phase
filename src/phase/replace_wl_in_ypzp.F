c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

c--------------------------------------------------------
	subroutine replace_wl_in_ypzp(ypc,zpc,wc,xlc,
     &                        ypc1,zpc1,ionetwo,iord)
c--------------------------------------------------------
c	ionetwo = 1: replace variables only in first function
c	ionetwo = 2: replace variables in first and second function
c--------------------------------------------------------

        implicit real*8 (a-h,o-z)

        dimension ypc(0:7,0:7,0:7,0:7)
        dimension zpc(0:7,0:7,0:7,0:7)
        dimension wc(0:7,0:7,0:7,0:7)
        dimension xlc(0:7,0:7,0:7,0:7)
        dimension ypc1(0:7,0:7,0:7,0:7)
        dimension zpc1(0:7,0:7,0:7,0:7)
        dimension ia(0:7,0:7),ic(0:7,0:7)
        dimension iac1(1000000,0:7),iac2(1000000,0:7),
     &            iac3(1000000,0:7),iac4(1000000,0:7)
        dimension iac1l(1000000,0:7)

c	zu bestimmende Konstanten festlegen (ikst)
	do ii=0,iord              ! y
	do ik=0,iord-ii           ! z
	do is=0,iord-ii-ik        ! dy
	do it=0,iord-ii-ik-is     ! dz

	ypc1(ii,ik,is,it)=0.d0
	if(ionetwo.eq.2)zpc1(ii,ik,is,it)=0.d0

c------- aeussere Summen -------------------------------

	do ij=0,ii
	do im=0,ik

	do n5=0,iord-ij-im
	do n6=0,iord-ij-im-n5

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
	  if((ii-ij.ne.0).or.(ik-im.ne.0).or.(is.ne.0).or.(it.ne.0))then
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
	call spaltenkombi(n5,n6,is,-1,ianz3,iac3)

c--------- vierte Spalte
	call spaltenkombi(n5,n6,it,-1,ianz4,iac4)

c-----------------------------------------------------------
c	jetzt gueltige Kombinationen der verschiedenen Kombinationen 
c	fuer die Spalten 1 - 4 suchen
c	bei jeder gueltigen Kombination gleich sum anpassen, damit 
c	Kombi-Kombinationen nicht zwischengespeichert werden muessen
c-----------------------------------------------------------
c	ACHTUNG, es gibt immer mindesten eine gültige Spaltenkombination
c	und zwar: (0,0,0,0)  (0,0,0,0)
c-----------------------------------------------------------

	do m1=1,ianz1	! loop über alle gültigen Spaltenkombinationen
	do m2=1,ianz2
	do m3=1,ianz3
	do m4=1,ianz4

	iflag=1		! 1) gueltige Kombination

c--------------	Isabelle fragen, 
c		vermutlich muss man das auskommentieren
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
	    prod=prod*wc(iac1(m1,mm1),iac2(m2,mm1),
     &		          iac3(m3,mm1),iac4(m4,mm1))
	    enddo
	    do mm2=n5+1,n5+n6	
	    prod=prod*xlc(iac1(m1,mm2),iac2(m2,mm2),
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

	ypc1(ii,ik,is,it)=ypc1(ii,ik,is,it)+ypc(n5,n6,ij,im)*sum
	if(ionetwo.eq.2)then
	zpc1(ii,ik,is,it)=zpc1(ii,ik,is,it)+zpc(n5,n6,ij,im)*sum
	
	endif

	enddo	! loop ueber n6
	enddo	! loop ueber n5		

	enddo	! loop ueber m
	enddo	! j

	enddo	! loop ueber t
	enddo	! s
	enddo	! k
	enddo	! i

	return
	end

