c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

c------------------------------------------------------------------
	subroutine fdet_8(wc,xlc,ypc1,zpc1,dypc,dzpc,
     &         opl6,dfdw6,dfdl6,dfdww6,dfdwl6,dfdll6,dfdwww6,
     &         dfdwidlj,dfdww,dfdwl,dfdll,
     &         fdetc,fdetphc,fdet1phc,g,
     &         inorm1,inorm2,iord)
c------------------------------------------------------------------
	implicit real*8(a-h,o-z)

	structure/geometryst/
	  real*8 sina,cosa,sinb,cosb,
     &      r,rp,xdens(0:4),xlam
	  integer idefl
	end structure
	record /geometryst/g

       dimension wc(0:7,0:7,0:7,0:7),
     &            xlc(0:7,0:7,0:7,0:7),        
     &            ypc1(0:7,0:7,0:7,0:7),
     &            zpc1(0:7,0:7,0:7,0:7),
     &            dypc(0:7,0:7,0:7,0:7),
     &            dzpc(0:7,0:7,0:7,0:7)
        dimension f1(0:7,0:7,0:7,0:7),f2(0:7,0:7,0:7,0:7),
     &            f3(0:7,0:7,0:7,0:7),f4(0:7,0:7,0:7,0:7),
     &            f12(0:7,0:7,0:7,0:7),f34(0:7,0:7,0:7,0:7),
     &            fdetc(0:7,0:7,0:7,0:7),
     &            fdetphc(0:7,0:7,0:7,0:7),
     &            fdet1phc(0:7,0:7,0:7,0:7)
        dimension opl6(0:7,0:7,0:7,0:7,0:7,0:7),
     &	          dfdw6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdl6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdww6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdwl6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdll6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdwww6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdwidlj(0:7,0:7,0:7,0:7,0:7,0:7),
     &            dfdww(0:7,0:7,0:7,0:7),
     &            dfdwl(0:7,0:7,0:7,0:7),
     &            dfdll(0:7,0:7,0:7,0:7)
     
	dimension PL0a(0:7,0:7,0:7,0:7),
     &		PLwa(0:7,0:7,0:7,0:7),PLla(0:7,0:7,0:7,0:7),
     &		PLw2a(0:7,0:7,0:7,0:7),PLl2a(0:7,0:7,0:7,0:7),
     &		PLwla(0:7,0:7,0:7,0:7),
     &		PLw3a(0:7,0:7,0:7,0:7)
	dimension PL0(0:7,0:7,0:7,0:7),
     &		PLw(0:7,0:7,0:7,0:7),PLl(0:7,0:7,0:7,0:7),
     &		PLw2(0:7,0:7,0:7,0:7),PLl2(0:7,0:7,0:7,0:7),
     &		PLwl(0:7,0:7,0:7,0:7),
     &		PLw3(0:7,0:7,0:7,0:7)
	dimension QL0(0:7,0:7,0:7,0:7),
     &		QLw(0:7,0:7,0:7,0:7),QLl(0:7,0:7,0:7,0:7),
     &		QLw2(0:7,0:7,0:7,0:7),QLl2(0:7,0:7,0:7,0:7),
     &		QLwl(0:7,0:7,0:7,0:7),
     &		QLw3(0:7,0:7,0:7,0:7)
	dimension RL0(0:7,0:7,0:7,0:7),
     &		RLw(0:7,0:7,0:7,0:7),RLl(0:7,0:7,0:7,0:7),
     &		RLw2(0:7,0:7,0:7,0:7),RLl2(0:7,0:7,0:7,0:7),
     &		RLwl(0:7,0:7,0:7,0:7),
     &		RLw3(0:7,0:7,0:7,0:7)
	dimension T4(0:7,0:7,0:7,0:7),T4a(0:7,0:7,0:7,0:7),
     &            T4b(0:7,0:7,0:7,0:7)

	dimension gam(0:7)

c------------------------------------------------------------------
c	evaluate factors
c------------------------------------------------------------------
c
c	0) fdetc wird nicht benutzt, frueher eventuell
c	   fuer isource.lt.4 oder fuer isource.eq.6
c	   jetzt wird dieser Faktor auf 1 gesetzt
c
c	1) dyp/ddy*dzp/ddz-dyp/ddz*dzp/ddy
c	   dieser Faktor stammt von der Koordinatentransformation 
c	   und wird immer in dieser Form benutzt
c	   Die Transformation bei der SPA geschieht von der Bildebene in die Quellebene
c	   Integriert wird ueber die Winkel und die Koordinaten in der Bildebene.
c	   ypc1, zpc1, dypc und dzpc sind die Fourtierkoeffizienten für Koordinaten/Winkel 
c	   in der Quellebene entwickelt nach Koordinaten/Winkel in der Bildebene 
c	   hier gibt es 2 Fälle:
c	   inorm1 = 0: Normalfall, in dem die Funktionaldeterminante berechnet wird
c	   inorm1 = 1: Faktor auf 1 gesetzt (zum Debugging)

c	2) stationary phase approximation
c	   d2F/dw**2 * d2F/dl**2 - (d2F/dw dl)**2
c	   hier sind drei Fälle zu unterscheiden
c	   inorm2 = 0: Normalfall, der fuer ein Element numerisch plausibel ist,
c		       aber mathematisch nicht bewiesen ist
c		       wird bis auf Weiteres benutzt fuer Kombination von OEs 
c	   inorm2 = 1: Faktor auf 1 gesetzt (zum debugging)
c	   inorm2 = 2: 1/Wurzel(d2PL/dw2*d2PL/dl2)
c	   inorm2 = 3: 1/Wurzel(d2PL/dw2*d2PL/dl2-(d2PL/dwdl)**2)
c	   inorm2 = 4: einschließlich 3. Ordnungsterm in optischer Weglänge
c	   inorm2 = 5: vollständige asymptotische Entwicklung bis order ord
c
c------------------------------------------------------------------	
c	factor 0)
c------------------------------------------------------------------
	
	do i=0,iord
	 do j=0,iord-i
	  do k=0,iord-i-j
	   do l=0,iord-i-j-k
	    fdetc(i,j,k,l)=0.d0
	   enddo
	  enddo
	 enddo
	enddo

	fdetc(0,0,0,0)=1.d0

c---------------------------------------------------
c	factor 1) 
c---------------------------------------------------	

	if(inorm1.eq.0)then

	call Tay_deri_4(ypc1,f1,3,1,iord)
	call Tay_deri_4(zpc1,f2,4,1,iord)
	call Tay_mult_4(f1,f2,f12,iord)
	
	call Tay_deri_4(ypc1,f1,4,1,iord)
	call Tay_deri_4(zpc1,f2,3,1,iord)
	call Tay_mult_4(f1,f2,f34,iord)

	call Tay_const_4(f34,-1.,iord)
	call Tay_sum_4(f12,f34,fdetphc,iord)

	endif

	if(inorm1.eq.1)then

	do i=0,iord
	 do j=0,iord-i
	  do k=0,iord-i-j
	   do l=0,iord-i-j-k
	    fdetphc(i,j,k,l)=0.d0
	   enddo
	  enddo
	 enddo
	enddo
	fdetphc(0,0,0,0)=1.d0
		
	endif
	
c	bei diesem Faktor braucht man jetzt nur noch:
c	- Taylorreihe berechnen
c	- Absolutbetrag berechnen

c------------------------------------------------------------------	
c	factor 2)
c------------------------------------------------------------------

c-----------------------------
	if(inorm2.eq.0)then
c-----------------------------
	  if(inorm1.eq.0)then
	   do i=0,iord
	    do j=0,iord-i
	     do k=0,iord-i-j
	      do l=0,iord-i-j-k
	       f1(i,j,k,l)=fdetphc(i,j,k,l)
	      enddo
	     enddo
	    enddo
	   enddo
	  else
	   call Tay_deri_4(ypc1,f1,3,1,iord)
	   call Tay_deri_4(zpc1,f2,4,1,iord)
	   call Tay_mult_4(f1,f2,f12,iord)

	   call Tay_deri_4(ypc1,f1,4,1,iord)
	   call Tay_deri_4(zpc1,f2,3,1,iord)
	   call Tay_mult_4(f1,f2,f34,iord)

	   call Tay_const_4(f34,-1.,iord)
	   call Tay_sum_4(f12,f34,f1,iord)

	  endif

	  if(f1(0,0,0,0).lt.0.d0)then
	  do i=0,iord
	   do j=0,iord-i
	    do k=0,iord-i-j
	     do l=0,iord-i-j-k
	      f1(i,j,k,l)=-f1(i,j,k,l)
	     enddo
	    enddo
	   enddo
	  enddo
	 endif

	 call Tay_sqrt_4(f1,f2,iord)
	 call Tay_inv_4(f2,fdet1phc,iord)		

	 endif

c-----------------------------
	if(inorm2.eq.1)then
c-----------------------------
	do i=0,iord
	 do j=0,iord-i
	  do k=0,iord-i-j
	   do l=0,iord-i-j-k
	    fdet1phc(i,j,k,l)=0.d0
	   enddo
	  enddo
	 enddo
	enddo
	fdet1phc(0,0,0,0)=1.d0
		
	endif

c----------------------------
	if(inorm2.eq.2)then
c----------------------------
	  call Tay_mult_4(dfdww,dfdll,f3,iord)	  
	  
	endif

c----------------------------
	if(inorm2.eq.3)then
c----------------------------
	  call Tay_mult_4(dfdww,dfdll,f1,iord)
	  call Tay_mult_4(dfdwl,dfdwl,f2,iord)
	  call Tay_const_4(f2,-1.,iord)
	  call Tay_sum_4(f1,f2,f3,iord)
	endif

c----------------------------------------------
	if((inorm2.eq.2).or.(inorm2.eq.3))then
c----------------------------------------------

	  if(f3(0,0,0,0).lt.0.d0)then
	   do i=0,iord
	    do j=0,iord-i
	     do k=0,iord-i-j
	      do l=0,iord-i-j-k
	  	f3(i,j,k,l)=-f3(i,j,k,l)
	      enddo
	     enddo
	    enddo
	   enddo
	  endif

	  call Tay_sqrt_4(f3,f4,iord)
	  call Tay_inv_4(f4,fdet1phc,iord)	

	endif

c-------------------------
	if(inorm2.le.3)then
c-------------------------
	fact=(dsqrt(dabs(g.cosa))*
     &        dsqrt(dabs(g.cosb)))/(g.r*g.rp)
     
	do i=0,iord
	 do j=0,iord-i
	  do k=0,iord-i-j
	   do l=0,iord-i-j-k
	    fdet1phc(i,j,k,l)=fdet1phc(i,j,k,l)*fact
	   enddo
	  enddo
	 enddo
	enddo
	endif

c	jetzt braucht man nur noch:
c	- die Taylorreihe berechnen
c	- Betrag bilden	

c----------------------------
	if(inorm2.eq.4)then
c----------------------------
	fact=(dsqrt(dabs(g.cosa))*
     &        dsqrt(dabs(g.cosb)))/(g.r*g.rp)

c	wir haben schon mal in Abhängigkeit der Variablen
c	w, l, y, z, yp, zp die Ausdruecke:
c 
c	dfdw,dfdl		!sollten null sein
c	d2fdw2,d2fdwdl,d2fdl2
c	d3fdw3

c	zunächst:
c	- yp und zp ersetzen durch Taylorreihe in y,z,dy,dz
c	jetzt hängen die Auddrücke ab von (w,l,y,z,dy,dz)

c	rund um den kritischen Punkt (w0,l0):
c	- w ersetzen mit w0+delta_w und l mit l0+delta_l 
c	jetzt hängen die Auddrücke ab von (w,l,dw,dl,y,z,dy,dz) (leider 8 Variablen)

c	schließlich:
c	- w0, l0 ersetzen mit Taylorreihe in y, z, dy, dz
c	ergibt die Ausdrücke d2fdw2 etc in den Variablen dw,dl,y,z,dy,dz
	
c	names as in SPIE Paper:
c	a0, b0, c0, d0 are functions of the four variables y, z, dy, dz 
	   do i=0,iord
	    do j=0,iord-i
	     do k=0,iord-i-j
	      do l=0,iord-i-j-k
	PL0a(k,l,i,j)=opl6(0,0,i,j,k,l)
	PLwa(k,l,i,j)=dfdw6(0,0,i,j,k,l)
	PLla(k,l,i,j)=dfdl6(0,0,i,j,k,l)
	PLw2a(k,l,i,j)=dfdww6(0,0,i,j,k,l)/2.d0
	PLl2a(k,l,i,j)=dfdll6(0,0,i,j,k,l)/2.d0
	PLwla(k,l,i,j)=dfdwl6(0,0,i,j,k,l)
	PLw3a(k,l,i,j)=dfdwww6(0,0,i,j,k,l)/6.d0
	      enddo
	     enddo
	    enddo
	   enddo

c--------- yp and zp Taylor series expansion in y,z,dy,dz	
	ionetwo=1

	call replace_wl_in_ypzp(PL0a,PL0a,ypc1,zpc1,
     &                        PL0,PL0,ionetwo,iord)
	call replace_wl_in_ypzp(PLwa,PLwa,ypc1,zpc1,
     &                        PLw,PLw,ionetwo,iord)
	call replace_wl_in_ypzp(PLla,PLla,ypc1,zpc1,
     &                        PLl,PLl,ionetwo,iord)
	call replace_wl_in_ypzp(PLwla,PLwla,ypc1,zpc1,
     &                        PLwl,PLwl,ionetwo,iord)
	call replace_wl_in_ypzp(PLw2a,PLw2a,ypc1,zpc1,
     &                        PLw2,PLw2,ionetwo,iord)
	call replace_wl_in_ypzp(PLl2a,PLl2a,ypc1,zpc1,
     &                        PLl2,PLl2,ionetwo,iord)
	call replace_wl_in_ypzp(PLwla,PLwla,ypc1,zpc1,
     &                        PLwl,PLwl,ionetwo,iord)
	call replace_wl_in_ypzp(PLw3a,PLw3a,ypc1,zpc1,
     &                        PLw3,PLw3,ionetwo,iord)

c---------- Prefactors

	call Tay_mult_4(wc,wc,wwc,iord)
	call Tay_mult_4(wc,xlc,wlc,iord)
	call Tay_mult_4(xlc,xlc,xllc,iord)
	call Tay_mult_4(wwc,wc,wwwc,iord)

	call Tay_copy_4(PL0,QL0,iord)
	  call Tay_mult_4(PLw,wc,T4,iord)
	call Tay_add_4(T4,QL0,iord)
	  call Tay_mult_4(PLl,xlc,T4,iord)
	call Tay_add_4(T4,QL0,iord)
	  call Tay_mult_4(PLW2,wwc,T4,iord)
	call Tay_add_4(T4,QL0,iord)
	  call Tay_mult_4(PLl2,xllc,T4,iord)
	call Tay_add_4(T4,QL0,iord)
	  call Tay_mult_4(PLwl,wlc,T4,iord)
	call Tay_add_4(T4,QL0,iord)	
	  call Tay_mult_4(PLw3,wwwc,T4,iord)
	call Tay_add_4(T4,QL0,iord)

c----------------------

	call Tay_copy_4(PLw,QLw,iord)
	  call Tay_mult_4(PLw2,wc,T4,iord)
	  call Tay_const_4(T4,2.d0,iord)
	call Tay_add_4(T4,QLw,iord)
	  call Tay_mult_4(PLwl,xlc,T4,iord)
	call Tay_add_4(T4,QLw,iord)
	  call Tay_mult_4(PLw3,wwc,T4,iord)
	  call Tay_const_4(T4,3.d0,iord)
	call Tay_add_4(T4,QLw,iord)

c-----------------------

	call Tay_copy_4(PLl,QLl,iord)
	  call Tay_mult_4(PLl2,xlc,T4,iord)
	  call Tay_const_4(T4,2.d0,iord)
	call Tay_add_4(T4,QLl,iord)
	  call Tay_mult_4(PLwl,wc,T4,iord)
	call Tay_add_4(T4,QLl,iord)

c-----------------------

	call Tay_copy_4(PLw2,QLw2,iord)
	  call Tay_mult_4(PLw3,wc,T4,iord)
	  call Tay_const_4(T4,3.d0,iord)
	call Tay_add_4(T4,QLw2,iord)

c-----------------------

	call Tay_copy_4(PLl2,QLl2,iord)

c-----------------------

	call Tay_copy_4(PLwl,QLwl,iord)
	  call Tay_mult_4(wc,xlc,T4,iord)
	call Tay_add_4(T4,QLwl,iord)

c-----------------------

	call Tay_copy_4(PLw3,QLw3,iord)

c---------- next: coordinate transformation

	call Tay_copy_4(QLw3,RLw3,iord)	! w3
	call Tay_copy_4(QLl2,RLl2,iord)	! l2
	call Tay_inv_4(QLl2,T4,iord)
cccccccc UF Tay_mult_4 ???
c	call Tay_mult(T4,QLwl,T4a,iord)	
c	call Tay_mult(T4a,QLwl,T4b,iord)
	call Tay_mult_4(T4,QLwl,T4a,iord)	
	call Tay_mult_4(T4a,QLwl,T4b,iord)

	call Tay_const_4(T4b,-0.25d0,iord)
	call Tay_sum_4(T4b,QLw2,RLw2,iord)	! w2

c------------------------------------------------------------------------------
	call gamma_func(gam)

c	analytische Berechnung des Integrals ist dann straight forward
c	mit dem Resultat: f1(y,z,dy,dz)
c
c	Berechnung der Exponenten:
c
c	k	exponent	break down in
c	0	1/3		Taylor series expansion	
c		2/3		1/3 & 1/3
c	1	3/3=1
c	2	5/3		1 & 2/3
c	3	7/3		2 & 1/3	
c	4	9/3		3
c	5	11/3		3 & 2/3
c	6	13/3		4 & 1/3
c	7	15/3		5

	endif
     
	return
	end

c--------------------------------------------------------
	subroutine gamma_func(gam)
c--------------------------------------------------------
c
c	Evaluation of gamma function, take only one value from table
c	evaluate the rest from
c	gamma(1) = 1
c	gamma(2/3) = (2*pi)/(sqrt(3)*gamma(1/3))
c	gamma(z+1)=z*gamma(z)
c
c	0	gamma(1/3) from table
c		  gamma(2/3) = (2*pi)/(sqrt(3)*gamma(1/3))
c	1	gamma(3/3/ = 1
c	2	gamma(5/3) = 2/3 * gamma(2/3) 
c	3	gamma(7/3) = 4/3 * gamma(4/3) = 4/3 * 1/3 * gamma(1/3)
c	4	gamma(9/3) = 2 * gamma(2) = 2 * 1 * gamma(1)
c	5	gamma(11/3)= 8/3 * gamma(8/3) = 8/3 * 5/3 *gamma(5/3) = 
c					8/3 * 5/3 * 2/3 * gamma(2/3)
c	6	gamma(13/3)= 10/3 * 7/3 * 4/3 * 1/3 * gamma(1/3)
c	7	gamma(15/3)= 12/3 * 9/3 * 6/3 * gamma(1) = 4 * 3 * 2 
c
c---------------------------------------------------------
	implicit real*8(a-h,o-z)

	dimension gam(0:7)

	pi=4.d0*atan(1.d0)
	gam(0)= 2.6789385347077476d0
	  g23=(2*pi)/(dsqrt(3.d0)*gam(0))
	gam(1)=1.d0
	gam(2)=(2.d0/3.d0)*g23 
	gam(3)=(4.d0/9.d0)*gam(0)
	gam(4)=2.d0
	gam(5)=(80.d0/27.d0)*g23
	gam(6)=(280.d0/81.d0)*gam(0)
	gam(7)=24.d0

c	for checking:
c	gam(0) = gamma(1/3) = 2.67893853470774763365569294097467764412873728E0 
c	gam(1) = gamma(1)   = 1.d0
c	gam(2) = gamma(5/3) = 9.02745292950933611296858685436342523680680403E-1
c	gam(3) = gamma(7/3) = 1.19063934875899894829141908487763450850745094E0
c	gam(4) = gamma(3)   = 2.d0
c	gam(5) = gamma(11/3)= 4.01220130200414938354159415749485566095803944E0
c	gam(6) = gamma(13/3)= 9.26052826812554737559992621571493506701451058E0
c	gam(7) = gamma(5)   = 24.d0

	return
	end

