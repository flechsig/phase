c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/pstf.F
c$$$ Date      : <07 Apr 08 15:37:17 flechsig> 
c$$$ Time-stamp: <15 Nov 11 15:14:09 flechsig> 
c$$$ Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

c*************************** neu **************************
        subroutine pstf(s,pso,xlam_test,iord,xlen1c,xlen2c,
     &                  xlen0,ypc1,zpc1,dypc,dzpc,
     &                  wc,xlc,
     &			yipc,zipc,psdc,stfd1phmaxc,stinumbc,
     .			s1c,s2c,s3c,eyrec,ezrec,
     &                  eyimc,ezimc,
     .                  m4,g,am,src,apr,
     .                  ra,ifl,xi,xir,st,
     .                  fdetc,fdetphc,fdet1phc)       
c*************************** neu **************************  

        implicit none

c a trick to have the struct only in the ./phase and find it from opti 
c if it works under vms is not yet tested

cc        include '../phase/phase_struct.for'
#include '../phase/phase_struct.F'

        record /constants/ cs
        record /geometryst/ g
        record /rayst/ ra
c        record /source_results/ sr
        record /integration_results/ xir
        record /control_flags/ ifl
        record /sources/ src
	record /integration/ xi
	record /apertures/ apr
	record /statistics/ st
	record /map4/ m4          
        record /mirrortype/ am 
        record /psimagest/ s 
  
        structure/PSSourceType/
            real *8    sigy,sigdy,sigz,sigdz
            integer    yhard,dyhard,zhard,dzhard 
        end structure   

        structure/psoptions/
          record /PSSourceType/ PSSource
          real *8 dyminfix, dymaxfix, dzminfix, dzmaxfix  
          integer intmod, ndyfix, ndzfix
        end structure  
        record /psoptions/ pso   

        integer i,j,k,l,iord,iheigh,iwidth,
     &       n1,n2,icheckk,ii,jj,iinumb

        real *8 xlam_test,xlen0,
     &       wc,xlc,ypc1,zpc1,dypc,dzpc,
     &       xlen1c,xlen2c,fdetc,fdetphc,fdet1phc,
     &       ddisty,ddistz,yi,zi,
     &       yipc,zipc,yiplot,ziplot,yi_global,zi_global,
     &       psd2,surfmax,psdc,
     &       stfd1phmaxc,stinumbc,s1c,s2c,s3c,eyrec,ezrec,
     &       eyimc,ezimc

        dimension ypc1(0:4,0:4,0:4,0:4),
     &            zpc1(0:4,0:4,0:4,0:4),
     &            dypc(0:4,0:4,0:4,0:4),
     &            dzpc(0:4,0:4,0:4,0:4), 
     &            wc(0:4,0:4,0:4,0:4),
     &            xlc(0:4,0:4,0:4,0:4), 
     &            xlen1c(0:4,0:4,0:4,0:4),  
     &            xlen2c(0:4,0:4,0:4,0:4),  
     &		  fdetc(0:4,0:4,0:4,0:4),        	! fdet
     &		  fdetphc(0:4,0:4,0:4,0:4),        	! fdet
     &		  fdet1phc(0:4,0:4,0:4,0:4)        	! fdet  

        dimension yiplot(1024),ziplot(1024)        ! bildpunkte
        dimension yipc(1),zipc(1),psdc(1),
     &            stfd1phmaxc(1),stinumbc(1),s1c(1),
     &            s2c(1),s3c(1),eyrec(1),ezrec(1),
     &            eyimc(1),ezimc(1)
        dimension psd2(1024,1024)  !psd2 ist ergebnis   
        
        complex*16 psd2c(1024,1024)
        complex*16 psd2cy(512,512)
	complex*16 psd2cz(512,512) 


c        write(*,*)'pstf (FORTRAN) called'
c        write(*,*)'show input, sum pathlen, wc, xlc'
c      do i=0,4
c          do j=0,4-i
c             do k=0,4-i-j
c                do l=0,4-i-j-k
c                   write(*,*)'xlm',i,j,k,l,
c     &   xlen1c(i,j,k,l)+xlen2c(i,j,k,l)
c                   write(*,*)'wc ',wc(i,j,k,l)
c                   write(*,*)'xlc',xlc(i,j,k,l)
c                enddo
c             enddo
c          enddo
c       enddo

        ra.xlam_test=xlam_test 

#ifdef DEBUG
        write(*,*)'pstf called'
        write(*,*)'grating: (line density)',g.xdens(0)
        write(*,*)'xi.ymin,xi.ymax,xi.zmin,xi.zmax'
        write(*,*)'   ',xi.ymin,xi.ymax,xi.zmin,xi.zmax

c        pi= 4.d0* datan(1.d0)    
c        call readfg34_par(src,apr,ifl,xi) 
c        ifl.iord=iord   ! Uwe

c***************** phase space imaging ************************

	pso.intmod=2
        write(*,*)'pstf: intmod fixed'
	write(*,*)'*** integration method',pso.intmod
	write(*,*)'test_wavelength (mm), xl0 (mm)',xlam_test,xlen0  
        write(*,*)' image dimension in y-direction (mm),' 
	write(*,*)'*** minimum ',s.disty1
	write(*,*)'*** maximum ',s.disty2
	write(*,*)'*** number of grid points',s.iheigh
        write(*,*)' image dimensions in z-direction (mm),' 
	write(*,*)'*** minimum ',s.distz1
	write(*,*)'*** maximum ',s.distz2     
	write(*,*)'*** number of grid points',s.iwidth         
c        write(*,*)' winkelgrid und punktgrid maximal 1024^2'
        write(*,*)'ra.xlam_test (mm)',ra.xlam_test
#endif
        call initconstants(cs)

c     von Johannes 24 Mar 97 09:23:31 flechsig ir --> xir
c     von Johannes 4.2.00 imaxz entfernt
        xir.nsimp=0
        xir.iisimp=4
	xir.isimp(1)=2
        xir.isimp(2)=xi.ianzy0+1
        xir.isimp(3)=2*xi.ianzy0
        xir.isimp(4)=2*xi.ianzz0+2


c------ copy stuff
	do i=0,iord
           do j=0,iord-i
              do k=0,iord-i-j
                 do l=0,iord-i-j-k
                    
                    m4.wc(i,j,k,l)=wc(i,j,k,l)
                    m4.xlc(i,j,k,l)=xlc(i,j,k,l)
                    m4.ypc1(i,j,k,l)=ypc1(i,j,k,l)
                    m4.zpc1(i,j,k,l)=zpc1(i,j,k,l)
c     m4.dyp1c(i,j,k,l)=dyp1c(i,j,k,l)
c     m4.dzp1c(i,j,k,l)=dzp1c(i,j,k,l)
                    m4.dypc(i,j,k,l)=dypc(i,j,k,l)
                    m4.dzpc(i,j,k,l)=dzpc(i,j,k,l)
c     m4.xlen1cc(i,j,k,l)=xlen1cc(i,j,k,l)
                    m4.xlen1c(i,j,k,l)=xlen1c(i,j,k,l)
c     m4.xlen2cc(i,j,k,l)=xlen2cc(i,j,k,l)
                    m4.xlen2c(i,j,k,l)=xlen2c(i,j,k,l)
c     m4.ypc_ap(i,j,k,l)=ypc_ap(i,j,k,l)
c     m4.zpc_ap(i,j,k,l)=zpc_ap(i,j,k,l)
                    m4.fdetc(i,j,k,l)=fdetc(i,j,k,l)
                    m4.fdetphc(i,j,k,l)=fdetphc(i,j,k,l)
                    m4.fdet1phc(i,j,k,l)=fdet1phc(i,j,k,l)
                 enddo
              enddo        
           enddo
	enddo  
#ifdef DEBUG      
        write(*,*)'phasefor.for: wc 4000',wc(4,0,0,0)
#endif
c--------------------------------------------------
c Schrittweiten in Bildkoordinaten bestimmen 
c---  parameter ddisty,z Schrittweiten--------------
       	if(s.iheigh.eq.1)then
           ddisty=0.
	else
           ddisty=(s.disty2-s.disty1)/floatj(s.iheigh-1)
	endif
	if(s.iwidth.eq.1)then
           ddistz=0.
	else
           ddistz=(s.distz2-s.distz1)/floatj(s.iwidth-1)
	endif
c---  schrittweiten im Bild bestimmt -----------------------   
c      ddistz, ddisty sind berechnet
c----------------------------------------------------------

c************* Bildpunkt generieren *******************************
#ifdef DEBUG
	write(*,*)'pstf: start'
#endif
	st.inumzit=0
	st.inumyit=0
	st.inumzan=0
	st.inumyan=0
c merken da die parameter im fehlerfall auf 1 gesetzt werden
        iheigh=s.iheigh
        iwidth=s.iwidth

        yi=s.disty1-ddisty       ! punkt disty unter minimum
        do n1=1,s.iheigh         ! y- Raster im Bild
          yi=yi+ddisty           ! begin bei disty1(minimum bildpunkt)
	  yiplot(n1)=yi          ! bildpunkt merken
          yipc(n1)=yi
#ifdef DEBUG
c          write(*,*)'debug 0711 yipc n1', yipc(n1), n1
#endif
          zi=s.distz1-ddistz     ! z- Raster im Bild
          do n2=1,s.iwidth
            zi=zi+ddistz
	    ziplot(n2)=zi        ! plotkoord. generiert + gespeichert
            zipc(n2)=zi   
c************* save initial coordinates ****************
	    yi_global=yi	
	    zi_global=zi

            ra.ri.yi=yi
            ra.ri.zi=zi
	    ra.n1=n2
	    ra.n2=n1

            st.nn1=n1
	    st.nn2=n2
c******* start of adaptive grid integration *************************** 
            call adaptive_int(m4,g,am,src,apr,
     .           cs,ra,ifl,xi,xir,st,s)  

            if(ifl.ispline.eq.-1)then
               xir.yzintey=xir.yzintya*exp(cs.sqrtm1*xir.yzintyp)
               xir.yzintez=xir.yzintza*exp(cs.sqrtm1*xir.yzintzp)
            endif
            
            if(cdabs(xir.yzintez).lt.1.e-10)icheckk=1
            psd2c(n1,n2)=cdabs(xir.yzintey)
            psd2cy(n1,n2)=xir.yzintey
            psd2cz(n1,n2)=xir.yzintez
            
c-------------intensity:
            
            if(src.isrctype.eq.1)then
               psd2(n1,n2)=cdabs(xir.yzintey)
            endif
            
            if((src.isrctype.ge.2).and.(src.isrctype.lt.4))then
               psd2(n1,n2)=cdabs(xir.yzintey)**2
            endif
            
            if((src.isrctype.eq.4).or.(src.isrctype.eq.5))then
               psd2(n1,n2)=
     &              (cdabs(xir.yzintey))**2+(cdabs(xir.yzintez))**2
            endif
            
            if(src.isrctype.eq.6)then
c-----------------changed 23.4.1996 ------
c     psd2(n1,n2)=cdabs(xir.yzintey)
               psd2(n1,n2)=dreal(xir.yzintey)
            endif	
c------------------------------------------
            
c******Ende der Integration ueber die Winkel ******************
         enddo                  ! raster
         write(*,*)'finished row ',n1,' of ', iheigh
      enddo                     ! raster
c     restore parameter in case of error
      s.iheigh= iheigh
      s.iwidth= iwidth
c****** Berechnung der Intensitaetsverteilung fertig ***********
      write(*,*)'stop intensity calculation'
c psd2 enthaelt intensitaet
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        if(ifl.inorm.eq.1)then
           write(*,*)'normalized output'
           surfmax=0.d0
           do n1=1,s.iheigh
              do n2=1,s.iwidth
                 surfmax=dmax1(surfmax,dabs(psd2(n1,n2)))
              enddo
           enddo
        else 
           surfmax=1.
        endif
        if(surfmax.le.1d-15)surfmax=1. ! surfmax bestimmt
    
c---------------- OUTPUT --------------------------------
c-----------------------------
c	if(src.isrctype.lt.4)then
c dies macht er jetzt immer, die bedeutung von psdc aendert sich
c fd1phmax nur bei quelle 4 und 5 sinnvoll ?
        iinumb=0
        do n1=1,s.iheigh
           do n2=1,s.iwidth
              iinumb=iinumb+st.inumb(n1,n2)
              psdc(n1+(n2-1)*s.iheigh)=psd2(n1,n2)/surfmax 
              stfd1phmaxc(n1+(n2-1)*s.iheigh)=st.fd1phmax(n1,n2)
              stinumbc(n1+(n2-1)*s.iheigh)=1.0*st.inumb(n1,n2)
           enddo
        enddo
c        endif
c
        if((src.isrctype.eq.4).or.(src.isrctype.eq.5))then
           do n1=1,s.iheigh
              do n2=1,s.iwidth
                 s1c(n1+(n2-1)*s.iheigh)=((cdabs(psd2cz(n1,n2)))**2 -
     &              (cdabs(psd2cy(n1,n2)))**2 )/surfmax
                 s2c(n1+(n2-1)*s.iheigh)=2.0*dreal(psd2cz(n1,n2)* 
     &                dconjg(psd2cy(n1,n2)))/surfmax
                 s3c(n1+(n2-1)*s.iheigh)=2.0*dimag(psd2cz(n1,n2)* 
     &                dconjg(psd2cy(n1,n2)))/surfmax
                 eyrec(n1+(n2-1)*s.iheigh)=dreal(psd2cy(n1,n2))/surfmax
                 eyimc(n1+(n2-1)*s.iheigh)=dimag(psd2cy(n1,n2))/surfmax
                 ezrec(n1+(n2-1)*s.iheigh)=dreal(psd2cz(n1,n2))/surfmax
                 ezimc(n1+(n2-1)*s.iheigh)=dimag(psd2cz(n1,n2))/surfmax
              enddo
           enddo
        endif

        if(src.isrctype.eq.6)then
           if(ifl.ibright.eq.1)then
              if( (src.so6.iabrx.le.51).and.
     &             (src.so6.iabry.le.51).and.
     &             (src.so6.iabrpx.le.51).and.
     &             (src.so6.iabrpy.le.51) )then
c     write 4-dim brightness

                 open(unit=10,name='BR_RE_BIN_IMAGE.DAT',err=557,
     &                type='new',form='unformatted',access='sequential')
                 write(10)src.so6.abrxmin,src.so6.abrxmax,
     &                src.so6.abrdx,src.so6.iabrx
                 write(10)src.so6.abrymin,src.so6.abrymax,
     &                src.so6.abrdy,src.so6.iabry
                 write(10)src.so6.abrpxmin,src.so6.abrpxmax,
     &                src.so6.abrdpx,src.so6.iabrpx
                 write(10)src.so6.abrpymin,src.so6.abrpymax,
     &                src.so6.abrdpy,src.so6.iabrpy
                 do i=1,src.so6.iabrx
                    do ii=1,src.so6.iabry
                       do j=1,src.so6.iabrpx
                          do jj=1,src.so6.iabrpy
                             write(10)src.so6.abr(i,ii,j,jj)
                          enddo
                       enddo
                    enddo
                 enddo
                 close(10)

              endif
           endif
        endif
        goto 600
 557    write(*,*)'pstf: error open BR_RE_BIN_IMAGE.DAT '
 600    continue
c-------------- file schreiben  ------------------------
c        open(unit=15,file='fd1ph.paw',status='new')     !surf.paw <<x 
c	open(unit=16,file='surf_s0.paw',status='new')     !surf.paw <<x 
c 	write(15,*)s.iwidth,s.iheigh 
c        write(16,*)s.iwidth,s.iheigh 
c        do n1=1,s.iheigh
c	 do n2=1,s.iwidth
c           write(15,*)st.fd1phmax(n1,n2)      !statistik
c           write(16,*)ziplot(n2),yiplot(n1),
c     &          psd2(n1,n2)/surfmax
c      	 enddo
c	enddo
c	close(15)
c        close(16)
c        write(*,*)'file fd1ph.paw und surf_s0.paw erzeugt'  
	
        write(*,*)'pstf: surfmax= ',surfmax 
	write(*,*)' total number of grid points = ',iinumb
	write(*,*)' total number of complete'
        write(*,*)'        z-iteration cycles = ',st.inumzit
	write(*,*)' total number of complete'
        write(*,*)'        y-iteration cycles = ',st.inumyit
	write(*,*)' reached maximum number of grid points'
	write(*,*)'        in z ',st.inumzan,' times '
	write(*,*)' reached maximum number of grid points'
	write(*,*)'        in y ',st.inumyan,' times '

#ifdef DEBUG
c          write(*,*)'debug 0711 yipc(1) yipc(2)', yipc(1), yipc(2) 
#endif


1111	format(8f15.6)
c        write(*,*)'xir.iisimp:',xir.iisimp
c	do ii=1,xir.iisimp
           open(unit=10,name='simpre.dat',type='unknown',err=123)
c           do i=1,xir.isimpre(ii)
           do i=1,xir.isimpre(1)
c              write(10,*)xir.simpre(ii,1,i),xir.simpre(ii,2,i)
              write(10,1111)xir.simpre(1,1,i),xir.simpre(1,2,i),
     .             xir.simpre(2,1,i),xir.simpre(2,2,i),
     .             xir.simpre(3,1,i),xir.simpre(3,2,i),
     .             xir.simpre(4,1,i),xir.simpre(4,2,i)
           enddo
           close(10)
           open(unit=10,name='simpim.dat',type='unknown',err=123)
c           do i=1,xir.isimpim(ii)
           do i=1,xir.isimpim(1)
c              write(10,*)xir.simpim(ii,1,i),xir.simpim(ii,2,i)
              write(10,1111)xir.simpim(1,1,i),xir.simpim(1,2,i),
     .             xir.simpim(2,1,i),xir.simpim(2,2,i),
     .             xir.simpim(3,1,i),xir.simpim(3,2,i),
     .             xir.simpim(4,1,i),xir.simpim(4,2,i)

           enddo
           close(10)
c	enddo
        write(*,*)'simpre.dat + simpim.dat erzeugt'

c        do ii=1,xir.iisimp
           open(unit=10,name='sintre.dat',type='unknown',err=123)
c           do i=1,xir.isintre(ii)
           do i=1,xir.isintre(1)
c              write(10,*)xir.sintre(ii,1,i),xir.sintre(ii,2,i)
              write(10,1111)xir.sintre(1,1,i),xir.sintre(1,2,i),
     .             xir.sintre(2,1,i),xir.sintre(2,2,i),
     .             xir.sintre(3,1,i),xir.sintre(3,2,i),
     .             xir.sintre(4,1,i),xir.sintre(4,2,i)
           enddo
           close(10)
           open(unit=10,name='sintim.dat',type='unknown',err=123)
c           do i=1,xir.isintim(ii)
           do i=1,xir.isintim(1)
c              write(10,*)xir.sintim(ii,1,i),xir.sintim(ii,2,i)
              write(10,1111)xir.sintim(1,1,i),xir.sintim(1,2,i),
     .             xir.sintim(2,1,i),xir.sintim(2,2,i),
     .             xir.sintim(3,1,i),xir.sintim(3,2,i),
     .             xir.sintim(4,1,i),xir.sintim(4,2,i)
           enddo
           close(10)
c        enddo
        write(*,*)'sintre.dat + sintim.dat erzeugt'
       goto 200
 123   write(*,*)'pstf: error open files'
 200    continue

c        open(unit=10,name='d12.dat',type='new')
        open(unit=10,name='d12.dat',err=555,type='unknown')
        do i=1,xi.ianzz0
           write(10,*)xir.d12(1,3,i),xir.d12(2,3,i)
        enddo
        close(10)
        write(*,*)'pstf: d12.dat generated'
        return
 555    write(*,*)'pstf: error open d12.dat '
        return
        end
c**** end pstf() ******************************************    
