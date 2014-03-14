c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

c------------------------------------------------------------------
	subroutine fdet_8(wc,xlc,ypc1,zpc1,ypc,zpc,dypc,dzpc,
     &         opl6,dfdw6,dfdl6,dfdww6,dfdwl6,dfdll6,dfdwww6,
     &         dfdwidlj,dfdww,dfdwl,dfdll,
     &         fdetc,fdetphc,fdet1phc,fdet1phca,fdet1phcb,g,
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
     &            ypc(0:7,0:7,0:7,0:7),
     &            zpc(0:7,0:7,0:7,0:7),
     &            dypc(0:7,0:7,0:7,0:7),
     &            dzpc(0:7,0:7,0:7,0:7)
        dimension f1(0:7,0:7,0:7,0:7),f2(0:7,0:7,0:7,0:7),
     &            f3(0:7,0:7,0:7,0:7),f4(0:7,0:7,0:7,0:7),
     &            f12(0:7,0:7,0:7,0:7),f34(0:7,0:7,0:7,0:7),
     &            fdetc(0:7,0:7,0:7,0:7),
     &            fdetphc(0:7,0:7,0:7,0:7),
     &            fdet1phc(0:7,0:7,0:7,0:7),
     &            fdet1phca(0:7,0:7,0:7,0:7),
     &            fdet1phcb(0:7,0:7,0:7,0:7)
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
c     &            dfdwidlj(0:7,0:7,0:7,0:7,0:7,0:7),
	dimension a0(0:7,0:7,0:7,0:7,0:7,0:7),
     &            b0(0:7,0:7,0:7,0:7,0:7,0:7),
     &		  c0(0:7,0:7,0:7,0:7,0:7,0:7),
     &		  d0(0:7,0:7,0:7,0:7,0:7,0:7),
     &		  at(0:7,0:7,0:7,0:7,0:7,0:7),
     &		  ct(0:7,0:7,0:7,0:7,0:7,0:7),
     &		  dt(0:7,0:7,0:7,0:7,0:7,0:7)
	dimension T6(0:7,0:7,0:7,0:7,0:7,0:7),
     &            T6a(0:7,0:7,0:7,0:7,0:7,0:7),
     &            T6b(0:7,0:7,0:7,0:7,0:7,0:7)
	dimension au(0:7,0:7,0:7,0:7),
     &            cu(0:7,0:7,0:7,0:7),
     &            du(0:7,0:7,0:7,0:7),
     &            av(0:7,0:7,0:7,0:7),
     &            cv(0:7,0:7,0:7,0:7),
     &            dv(0:7,0:7,0:7,0:7)

c	dimension gam(0:7)
c	dimension atilde(0:7,0:7,0:7,0:7),
c     &            aa0(0:7,0:7,0:7,0:7),
c     &            a02(0:7,0:7,0:7,0:7),
c     &		  a1(0:7,0:7,0:7,0:7),
c     &		  a2(0:7,0:7,0:7,0:7),
c     &		  a3(0:7,0:7,0:7,0:7),
c     &		  a4(0:7,0:7,0:7,0:7),
c     &		  a5(0:7,0:7,0:7,0:7),
c     &		  a6(0:7,0:7,0:7,0:7),
c     &		  a7(0:7,0:7,0:7,0:7),
c     &		  ab(0:7,0:7,0:7,0:7)
c	dimension btilde(0:7,0:7,0:7,0:7),
c     &            bb0(0:7,0:7,0:7,0:7),
c     &		  b1(0:7,0:7,0:7,0:7),
c     &		  b2(0:7,0:7,0:7,0:7),
c     &		  b3(0:7,0:7,0:7,0:7),
c     &		  b4(0:7,0:7,0:7,0:7),
c     &		  b5(0:7,0:7,0:7,0:7),
c     &		  b6(0:7,0:7,0:7,0:7),
c     &		  b7(0:7,0:7,0:7,0:7)
 
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
c	   inorm2 = 2: 1/Wurzel(d2PL/dw2*d2PL/dl2), komplette Taylorentwicklung
c          inorm2 = 21: wie 2, aber Kehrwert und Wurzel nicht Taylor entsickelt
c	   inorm2 = 3: 1/Wurzel(d2PL/dw2*d2PL/dl2-(d2PL/dwdl)**2) komplette Taylorentsicklung
c          inorm2 = 31: wie 3, aber Kehrwert und Wurzel nicht Taylor entsickelt
c	   inorm2 = 4: einschließlich 3. Ordnungsterm in optischer Weglänge
c	   inorm2 = 5: vollständige asymptotische Entwicklung bis order ord
c

c	write(*,*)'fdet_8 called'


c---- routine wird nur gerufen wenn imodus = 2 (PO)
       pi_loc=datan(1.d0)*4.d0
       fact=(dsqrt(dabs(g.cosa))*
     &        dsqrt(dabs(g.cosb)))/(g.r*g.rp)

       fact1=fact
       fact2=fact*dsqrt(pi_loc)/dsqrt(2.0d0)
       fact3=2.d0/pi_loc
       fact4=1.d0/((g.r+g.rp)*g.xlam) 
       fact5=1.d0/g.xlam    

c------------- change signs of ypc1 and zpc1
          do n1=0,iord
           do n2=0,iord-n1
            do n3=0,iord-n1-n2
             do n4=0,iord-n1-n2-n3
              ypc1(n1,n2,n3,n4)=((-1)**(n2+n3))*
     &                        ypc1(n1,n2,n3,n4)
              zpc1(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                        zpc1(n1,n2,n3,n4)
            enddo
           enddo
          enddo
        enddo

       if((inorm2.eq.4 ).or.(inorm2.eq.40).or.
     &    (inorm2.eq.41).or.(inorm2.eq.42).or.
     &    (inorm2.eq.43).or.(inorm2.eq.44).or.
     &    (inorm2.eq.45))then

c------------- change signs of wc and xlc 
        do n1=0,iord
         do n2=0,iord-n1
          do n3=0,iord-n1-n2
           do n4=0,iord-n1-n2-n3
             wc(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                      wc(n1,n2,n3,n4)
             xlc(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                       xlc(n1,n2,n3,n4)
           enddo
          enddo
         enddo
        enddo

      endif

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
	call Tay_deri_4(ypc1,f3,4,1,iord)
	call Tay_deri_4(zpc1,f4,3,1,iord)

c------------- change signs of f1, f2, f3, f4 
          do n1=0,iord
           do n2=0,iord-n1
            do n3=0,iord-n1-n2
             do n4=0,iord-n1-n2-n3
              f1(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                        f1(n1,n2,n3,n4)
              f2(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                        f2(n1,n2,n3,n4)
              f3(n1,n2,n3,n4)=((-1)**(n2+n3))*
     &                        f3(n1,n2,n3,n4)
              f4(n1,n2,n3,n4)=((-1)**(n2+n3))*
     &                        f4(n1,n2,n3,n4)
            enddo
           enddo
          enddo
        enddo

        call Tay_mult_4(f1,f2,f12,iord)
        call Tay_mult_4(f3,f4,f34,iord)
	call Tay_const_4(f34,-1.d0,iord)
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
	   call Tay_deri_4(ypc1,f3,4,1,iord)
	   call Tay_deri_4(zpc1,f4,3,1,iord)

c------------- change signs of f1, f2, f3, f4 
          do n1=0,iord
           do n2=0,iord-n1
            do n3=0,iord-n1-n2
             do n4=0,iord-n1-n2-n3
              f1(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                        f1(n1,n2,n3,n4)
              f2(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                        f2(n1,n2,n3,n4)
              f3(n1,n2,n3,n4)=((-1)**(n2+n3))*
     &                        f3(n1,n2,n3,n4)
              f4(n1,n2,n3,n4)=((-1)**(n2+n3))*
     &                        f4(n1,n2,n3,n4)
            enddo
           enddo
          enddo
        enddo

           call Tay_mult_4(f1,f2,f12,iord)
	   call Tay_mult_4(f3,f4,f34,iord)
	   call Tay_const_4(f34,-1.d0,iord)
	   call Tay_sum_4(f12,f34,f1,iord)
	  endif
	  if(f1(0,0,0,0).lt.0.d0)call tay_const_4(f1,-1.d0,iord)
          call Tay_sqrt_4(f1,f2,iord)
          call Tay_inv_4(f2,fdet1phc,iord)		
          call Tay_const_4(fdet1phc,fact5,iord)
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
	fdet1phc(0,0,0,0)=fact4
		
	endif

c hier haben wir die Variablen schon in den endgueltigen Parametern y, z, dy, dz
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
	  call Tay_const_4(f2,-1.d0,iord)
	  call Tay_sum_4(f1,f2,f3,iord)
	endif

c----------------------------------------------
	if((inorm2.eq.2).or.(inorm2.eq.3))then
c----------------------------------------------
	  if(f3(0,0,0,0).lt.0.d0)then
            call tay_const_4(f3,-1.d0,iord)
	  endif
	  call Tay_sqrt_4(f3,f4,iord)
	  call Tay_inv_4(f4,fdet1phc,iord)	
        endif

c-------------------------
	if((inorm2.eq.2).or.(inorm2.eq.3))then
c----------------------                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ---
          call tay_const_4(fdet1phc,fact1,iord)
	endif

c	jetzt braucht man nur noch:
c	- die Taylorreihe berechnen
c	- Betrag bilden	
c       das geschieht in fywert

c----------------------------
        if(inorm2.eq.20)then
c----------------------------
c         only for debugging   
          call tay_copy_4(dfdww,f3,iord)
          if(f3(0,0,0,0).lt.0.d0)then
          call tay_const_4(f3,-1.d0,iord)
          endif
          call Tay_sqrt_4(f3,f4,iord)
          call Tay_inv_4(f4,fdet1phca,iord)
          call Tay_const_4(fdet1phca,fact1,iord)       

          call tay_copy_4(dfdll,f3,iord)
          if(f3(0,0,0,0).lt.0.d0)then
          call tay_const_4(f3,-1.d0,iord)
          endif
          call Tay_sqrt_4(f3,f4,iord)
          call Tay_inv_4(f4,fdet1phcb,iord)             
        endif

c----------------------------
        if(inorm2.eq.21)then
c----------------------------
c         diesen Ausdruck gegenüber inorm2 = 2 bevorzugen   
          call tay_copy_4(dfdww,fdet1phca,iord)
          call tay_const_4(fdet1phca,1.d0/fact1,iord)
          call tay_copy_4(dfdll,fdet1phcb,iord)          
          call tay_const_4(fdet1phcb,1.d0/fact1,iord)
        endif

c----------------------------
        if((inorm2.eq.31).or.(inorm2.eq.32).or.
     &     (inorm2.eq.33))then
c----------------------------
c         diesen Ausdruck gegenüber inorm2 = 3 bevorzugen   
          call tay_copy_4(dfdww,fdet1phca,iord)
          call tay_const_4(fdet1phca,1.d0/fact1,iord)
          call tay_copy_4(dfdll,fdet1phcb,iord)      
          call tay_const_4(fdet1phcb,1.d0/fact1,iord)
          call Tay_copy_4(dfdwl,fdet1phc,iord)
          call tay_const_4(fdet1phc,2.d0/fact1,iord)
        endif

c----------------------------
        if((inorm2.eq.4 ).or.(inorm2.eq.40).or.
     &     (inorm2.eq.41).or.(inorm2.eq.42).or.
     &     (inorm2.eq.43).or.(inorm2.eq.44).or.
     &     (inorm2.eq.45))then

c	wir haben schon mal in Abhängigkeit der Variablen
c	w, l, y, z, yp, zp die folgenden Ausdruecke:
c 
c	dfdw,dfdl		!sollten null sein
c	d2fdw2,d2fdwdl,d2fdl2
c	d3fdw3	
c	a0, b0, c0, d0 are functions of the six variables w, l, y, z, yp, zp 
c
c       in the following we use old coordinate system 
c       as in fgmapidp_8 during map evaluation
c

c---------- coordinate transformation in variables w and l

c---------------------------------------------------------------------------
c---------- including 3rd order terms in PL, alt, nur noch zum testen
c---------------------------------------------------------------------------
         if((inorm2.eq.4 ).or.
     &     (inorm2.eq.40).or.(inorm2.eq.41).or.
     &     (inorm2.eq.43).or.(inorm2.eq.44).or.
     &     (inorm2.eq.45))then

c---------- get prefactors as of SAN Diego paper
           do i=0,iord
            do j=0,iord-i
             do k=0,iord-i-j
              do l=0,iord-i-j-k
               do m=0,iord-i-j-k-l
                do n=0,iord-i-j-k-l-m
        c0(i,j,k,l,m,n)=dfdww6(i,j,k,l,m,n)/2.d0        
        a0(i,j,k,l,m,n)=dfdll6(i,j,k,l,m,n)/2.d0        
        b0(i,j,k,l,m,n)=dfdwl6(i,j,k,l,m,n)/2.d0                        ! 
        d0(i,j,k,l,m,n)=dfdwww6(i,j,k,l,m,n)/6.d0       
                enddo
               enddo
              enddo
             enddo
            enddo
           enddo


       call Tay_copy_6(d0,dt,iord)     

        if(inorm2.ne.41)then
c       Variante 1
	call Tay_copy_6(a0,at,iord)						
	call Tay_inv_6(a0,T6,iord)
	call Tay_mult_6(T6,b0,T6a,iord)	
	call Tay_mult_6(T6a,b0,T6b,iord)
	call Tay_const_6(T6b,-1.d0,iord)
        call Tay_sum_6(T6b,c0,ct,iord) 

        else
c       Variante 2, liefert schlechtere Ergebnisse als Variante 1
        call Tay_copy_6(c0,ct,iord)                                                
        call Tay_inv_6(c0,T6,iord)
        call Tay_mult_6(T6,b0,T6a,iord) 
        call Tay_mult_6(T6a,b0,T6b,iord)
        call Tay_const_6(T6b,-1.d0,iord)
        call Tay_sum_6(T6b,a0,at,iord)  
 
        endif

c------------- get partial derivatives in variables (w,l,y,z)
c------------- replace yp and zp
        call replace_6v4v(at,ypc,zpc,au,iord)
        call replace_6v4v(ct,ypc,zpc,cu,iord)
        call replace_6v4v(dt,ypc,zpc,du,iord)

c------------- get partial derivatives in variables (y,z,dy,dz)
c------------- replace w and l with Taylor series in y,z,dy,dz
        call replace_wl_in_ypzp(au,au,wc,xlc,av,av,1,iord)
        call replace_wl_in_ypzp(cu,cu,wc,xlc,cv,cv,1,iord)
        call replace_wl_in_ypzp(du,du,wc,xlc,dv,dv,1,iord)

c------------- av*l**2+cv*w**2+dv*w**3

c-------------- sichern des quadratischen Terms in l
        call Tay_copy_4(av,fdet1phc,iord)
        call tay_const_4(fdet1phc,1.d0/fact1,iord)

c-------------- jetzt die w-abhängigen Terme
        call Tay_copy_4(dv,fdet1phca,iord)      ! w**3 Term     
        call tay_const_4(fdet1phca,1.d0/fact1,iord)
        call Tay_copy_4(cv,fdet1phcb,iord)      ! w**2 Term
        call tay_const_4(fdet1phcb,1.d0/fact1,iord)

c------------- change signs of results for inorm2 = 4
          do n1=0,iord
           do n2=0,iord-n1
            do n3=0,iord-n1-n2
             do n4=0,iord-n1-n2-n3
              fdet1phc(n1,n2,n3,n4)=((-1)**(n2+n3))*
     &                        fdet1phc(n1,n2,n3,n4)
              fdet1phca(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                        fdet1phca(n1,n2,n3,n4)
              fdet1phcb(n1,n2,n3,n4)=((-1)**(n2+n3))*
     &                        fdet1phcb(n1,n2,n3,n4)

c            if(n1+n2+n3+n4.le.2)write(6,*)n1,n2,n3,n4,fdet1phc(n1,n2,n3,n4)
c            if(n1+n2+n3+n4.le.2)write(6,*)n1,n2,n3,n4,fdet1phcb(n1,n2,n3,n4)

            enddo
           enddo
          enddo
        enddo

        endif

	endif		! inorm2 = 4

       if((inorm2.eq.4 ).or.(inorm2.eq.40).or.
     &    (inorm2.eq.41).or.(inorm2.eq.42).or.
     &    (inorm2.eq.43).or.(inorm2.eq.44).or.
     &    (inorm2.eq.45))then

c--------- change signs of wc, xlc back 
          do n1=0,iord
           do n2=0,iord-n1
            do n3=0,iord-n1-n2
             do n4=0,iord-n1-n2-n3
              wc(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                        wc(n1,n2,n3,n4)
              xlc(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                         xlc(n1,n2,n3,n4)
            enddo
           enddo
          enddo
        enddo

        endif

c------------- change signs of ypc1 and zpc1 back
          do n1=0,iord
           do n2=0,iord-n1
            do n3=0,iord-n1-n2
             do n4=0,iord-n1-n2-n3
              ypc1(n1,n2,n3,n4)=((-1)**(n2+n3))*
     &                        ypc1(n1,n2,n3,n4)
              zpc1(n1,n2,n3,n4)=((-1)**(n2+n3+1))*
     &                        zpc1(n1,n2,n3,n4)
            enddo
           enddo
          enddo
        enddo

	return
	end

c-------------------------------------------------------------------
	subroutine atilde_exp(atilde,a0,a1,a2,a3,a4,a5,a6,a7,iord)
c-------------------------------------------------------------------
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
c--------------------------------------------------------

	implicit real*8(a-h,o-z)

	dimension atilde(0:7,0:7,0:7,0:7),
     &            a0(0:7,0:7,0:7,0:7),
     &            a02(0:7,0:7,0:7,0:7),
     &		  a1(0:7,0:7,0:7,0:7),
     &		  a2(0:7,0:7,0:7,0:7),
     &		  a3(0:7,0:7,0:7,0:7),
     &		  a4(0:7,0:7,0:7,0:7),
     &		  a5(0:7,0:7,0:7,0:7),
     &		  a6(0:7,0:7,0:7,0:7),
     &		  a7(0:7,0:7,0:7,0:7),
     &		  ab(0:7,0:7,0:7,0:7)
 
	call Tay_cbrt_4(atilde,ab,iord)
	call Tay_inv_4(ab,a0,iord)		! a0=1/(1/3)
	call Tay_mult_4(a0,a0,a02,iord)		! a02=1/(2/3)

	call Tay_mult_4(a0,a02,a1,iord)	
	call Tay_mult_4(a1,a02,a2,iord)		
	call Tay_mult_4(a2,a02,a3,iord)	
	call Tay_mult_4(a3,a02,a4,iord)		
	call Tay_mult_4(a4,a02,a5,iord)
	call Tay_mult_4(a5,a02,a6,iord)
	call Tay_mult_4(a6,a02,a7,iord)

	return
	end

c-------------------------------------------------------------------
	subroutine btilde_exp(btilde,b0,b1,b2,b3,b4,b5,b6,b7,iord)
c-------------------------------------------------------------------

	implicit real*8(a-h,o-z)

	dimension btilde(0:7,0:7,0:7,0:7),
     &            b0(0:7,0:7,0:7,0:7),
     &		  b1(0:7,0:7,0:7,0:7),
     &		  b2(0:7,0:7,0:7,0:7),
     &		  b3(0:7,0:7,0:7,0:7),
     &		  b4(0:7,0:7,0:7,0:7),
     &		  b5(0:7,0:7,0:7,0:7),
     &		  b6(0:7,0:7,0:7,0:7),
     &		  b7(0:7,0:7,0:7,0:7)
 
	do i=0,iord
	 do j=0,iord-i
	  do k=0,iord-i-j
	   do l=0,iord-i-j-k
	    b0(i,j,k,l)=0.d0
	   enddo
	  enddo
	 enddo
	enddo
	b0(0,0,0,0)=1.d0

	call Tay_copy_4(btilde,b1,iord)

	call Tay_mult_4(b1,b1,b2,iord)	
	call Tay_mult_4(b2,b1,b3,iord)	
	call Tay_mult_4(b3,b1,b4,iord)		
	call Tay_mult_4(b4,b1,b5,iord)
	call Tay_mult_4(b5,b1,b6,iord)
	call Tay_mult_4(b6,b1,b7,iord)

	return
	end
