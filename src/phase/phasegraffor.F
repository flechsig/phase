c $$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/phasegraffor.fpp
c $$$ Date      : <11 Nov 03 09:30:31 flechsig> 
c $$$ Time-stamp: <17 Jan 08 17:38:56 flechsig> 
c $$$ Author    : Uwe Flechsig, flechsig@psi.ch
c $$$
c $$$ $Source$ 
c $$$ $Date$
c $$$ $Revision$ 
c $$$ $Author$ 

c
c fortran file which will be preprocessed #define LINUX #define VMS
c !!! wird die Struktur grdatstructtype in phase.h veraendert
c     muss dies hier von Hand in den betroffenen Subroutinen 
c     nachgefuehrt werden (UF 24.3.04)


       	subroutine inithplot 
        PARAMETER (NWPAW=200000)
        COMMON/PAWC/RPAW(NWPAW)      
 	CALL HLIMIT(200000) 
        CALL HPLINT(1) 
       	return
	end    
      
       	subroutine inithplotold 
        PARAMETER (NWORDS=50000)
        COMMON/PAWC/RPAW(NWORDS)      
        call MZEBRA(-3)
        call MZPAW(NWORDS,' ')
        call IGINIT(0)
        call IGWKTY(ITYPE)
        call IGSSE(6,ITYPE)  
	end    

        subroutine exithplotold 
        call IGTERM
        call IGEND
        end

        subroutine exithplot
        call hplend
        end

c*********************** Grafik Display ********************************
        subroutine hplotdisplayf(s,raysout,dlf,lambda,titel,metaname) 
c
c       Uwe 3.7.96 raytype auf dim 5 erweitert (phase)
c       4.7.96 kein lesen von file mehr
c
        implicit none

	structure/struct/
            real*8 ymi,yma,zmi,zma,dymi,dyma,dzmi,dzma,tmi,tma 
            integer zeilen,psfile,status,plotsyle,
     &              iykanal,izkanal,ititel(80)
        end structure
        record /struct/ s
	dimension raysout(5,1)                  ! gummilaenge
	character*(*) titel,metaname
        real*4        y1,y2,z1,z2,scale       
        real*8	      dlfac,lambda,dlf,raysout,scaled
        integer       ifile,i,iy,iz,istat

c temporarely fixed
c	parameter (dtitel = 'PHASE')
c        parameter (dmetaname= 'pmeta.ps')
c************************************************************************
 
        y1=s.ymi
        y2=s.yma
        z1=s.zmi
        z2=s.zma  

        dlfac=dlf
        
c        print*,'title and plotfilename are temporarely fixed'
#ifdef DEBUG
        print*,'hplotdisplayf:',y1,y2,z1,z2    
        print*,'title         ',titel 
        print*,'ps filename   ',metaname    
        print*,'status, zeilen ',s.status, s.zeilen
        print*,'style ', s.plotsyle
        write(*,*)'dlfactor (mm)',dlfac,' lambda (mm)',lambda
#endif
c********************* kanaele ****************************************
c postscriptfile 
        ifile=s.psfile             ! merker setzen
        if(ifile.eq.1)then  

#ifdef VMS
           print*,'open postsciptfile under VMS ',metaname
           open(unit=11,file=metaname,
     1          Carriagecontrol='LIST',FORM='FORMATTED',STATUS='NEW') 
#else
           print*,'open postsciptfile under UNIX ',metaname
c           open(unit=11,file=metaname,FORM='FORMATTED',
c     1          STATUS='UNKNOWN',err=4444)
           call kuopen(11,metaname,'UNKNOWN',istat)
#endif
c           call igmeta(-11,-111) UF 12.1.2001
           call igmeta(11,-111)
        endif
              
c        write(*,*)'only for ray trace data'   
        goto 4445
4444  write(*,*)'hplotdisplayf: error open file: ',metaname
c---- buche histogramme -------------------------------------------
4445  call kanalzahlf(s.plotsyle,iy,iz) ! iy,iz neu belegt
1001    y1=s.ymi
        y2=s.yma
        z1=s.zmi
        z2=s.zma
c bei postscript ab hier doppelt
#ifdef DEBUG
        write(*,*)'prior hbook2 ny nz      : ',iy,iz        
        write(*,*)'prior hbook2 y1,y2,z1,z2: ',y1,y2,z1,z2  
#endif   
      	call hbook2(200,'PHASE',iz,z1,z2,iy,y1,y2,0.)
        call hbook2(201,'PHASE',iz,z1,z2,iy,y1,y2,0.)
        call hbook2(202,'PHASE',iz,z1,z2,iy,y1,y2,0.)
        call hbook1(300,'PHASE',iy,y1,y2,0.)   
        call hbook1(400,'PHASE',iz,z1,z2,0.)  
        y1=(y1*dlfac+lambda)*1e6
        y2=(y2*dlfac+lambda)*1e6
        call hbook1(700,'PHASE',iy,y1,y2,0.)   ! dlambda 
c use projections- book it
        call hbpro(200,0.)
         
        y1=s.dymi*1e3
        y2=s.dyma*1e3
        z1=s.dzmi*1e3
        z2=s.dzma*1e3
#ifdef DEBUG
        write(*,*)'diverg.: book dy1,dy2,dz1,dz2: ',y1,y2,z1,z2     
#endif
        call hbook1(500,'PHASE',iy,y1,y2,0.)   
        call hbook1(600,'PHASE',iz,z1,z2,0.)  

c time structure
        y1=s.tmi*1e15         ! output in fs
        y2=s.tma*1e15
#ifdef DEBUG
        write(*,*)'time: book iy,y1,y2: ',iy,y1,y2     
#endif
        call hbook1(800,'PHASE',iy,y1,y2,0.)   
     
c--------- histogramme gebucht ------------------------

c--------- fuellen ------------------------------------
#ifdef DEBUG
        write(*,*)'fill histogram'
#endif  
        if (s.plotsyle.ge.20)then 
	   do i=1,s.zeilen 
c           write(*,*)'fill histogr.',i  
              call rtfillnorm(raysout(1,i),raysout(2,i),
     &                        raysout(5,i),lambda)
           enddo
           if (s.plotsyle.eq.20)then
               call hopera(201,'/',200,202,1.0,1.0)
           else
               scaled=lambda/1.8849e-3      !1.8849e-3= 1e15(fs)/(2 pi c(mm/s))
               scale=sngl(scaled)
c manchmal funktioniert das skalieren nicht- keine Ahnung warum
c               write(*,*)'scaled,scale',scaled,scale
               call hopera(201,'/',200,202,scale,1.0) 
           endif
 
        else
           do i=1,s.zeilen 
c           write(*,*)'fill histogr.',i  
              call rtfill(raysout(1,i),raysout(2,i),
     &                    raysout(3,i),raysout(4,i),
     &	                  lambda,dlfac,raysout(5,i))
           enddo


        endif
#ifdef DEBUG	
        write(*,*)'for: histograms filled'  
#endif      
c--------- end fuellen --------------------------------  
        call makeplotf(titel,s.plotsyle) ! eigentliches plotten
        call iuwk(0,1)                   ! puffer leeren update alle ws S.12

        if(ifile.eq.1)then  
           print*,'close postscriptfile'
           call igmeta(0,0)
           call hdelet(0)
           call hplend()
#ifdef VMS
           close(11) 
#else
           call kuclos(11,' ',1)
#endif
           ifile= 0                         ! setze schalter zurueck
           call inithplot()   
           goto 1001
	endif

5000    call hdelet(0)                    !Speicher freigeben
        RETURN
	END

c           if (s.idatfile.eq.1)then
c--------- lesen von file------------------------------
c             open(unit=10,file=datname,status='old') 
c	     read(10,*) ianz,iz            ! header,dummy
c	     write(*,*)'for: data from file' 
c	     do i=1,ianz 
c              read(10,*) yd,zd,dyd,dzd
c              call rtfill(yd,zd,dyd,dzd,0.0)
c             enddo          
c             close(10)
c           else
c       if(s.jftyp.gt.0)then         			! phase space  file 
c           print*,'read phase space data'   
c            y und z sind bei psd vertauscht
c             
c           open(unit=10,file=datname,status='old') 
c           read(10,*)iz,iy            ! richtig
c tausch
c           y1=s.ymi-(s.yma-s.ymi)/(2.0*iy) 
c           y2=s.yma+(s.yma-s.ymi)/(2.0*iy) 
c           z1=s.zmi-(s.zma-s.zmi)/(2.0*iz) 
c           z2=s.zma+(s.zma-s.zmi)/(2.0*iz) 
c           print*,'vor hbook2 ny nz      : ',iy,iz
c           print*,'vor hbook2 y1,y2,z1,z2: ',y1,y2,z1,z2  
c    	   call hbook2(200,'intensity',iz,z1,z2,iy,y1,y2,0.0)    
c           do i=1,ianz 
c              read(10,*) zd,yd,dzd        ! richtig
c              y=sngl(yd)
c              z=sngl(zd) 
c              dz=sngl(dzd)     ! *100.0
c              call hfill(200,z,y,dz) 
c           enddo
c           print*,'gelesene Zeilen: ',ianz 
c           close(10)
c        else                                                   ! rtrace file
c***** end   subroutine hplotdisplayf *********************************** 

c************************************************************************

        subroutine hplotpsdf(titel,metaname,s,yd,zd,psd,iy,iz) 
c     wird bei phasespace genutzt 
c
	implicit real*8(a-h,o-z)    
  
	structure/struct/
            real*8 ymi,yma,zmi,zma,dymi,dyma,dzmi,dzma,tmi,tma 
            integer zeilen,psfile,status,plotsyle,
     &              iykanal,izkanal,ititel(80)
        end structure
        record /struct/ s

c	changed dimension form 1 to 128  5.11.2003
	dimension yd(1), zd(1), psd(1)        ! gummilaenge
c	end changes
	character*(*) titel,metaname
        real*4     y1,y2,z1,z2,y,z,p       
        integer istat
	
c************************************************************************
 
        write(*,*)'hplotpsdf called (for)'
        print*,'ps-filename  ',metaname    
        print*,'titel       ',titel  
        print*,'status,iy,iz',s.status,iy,iz
        print*,'plotstyle ', s.plotsyle
c********************* kanaele ****************************************
c postscriptfile 
        ifile=s.psfile             ! merker setzen
        if(ifile.eq.1)then  

#ifdef VMS
           print*,'open postsciptfile under VMS '
           open(unit=11,file=metaname,STATUS='UNKNOWN',
     1          Carriagecontrol='LIST',FORM='FORMATTED',err=1111) 
#else
           print*,'open postsciptfile under UNIX '
c           open(unit=11,file=metaname,STATUS='UNKNOWN',err=1111)
           call kuopen(11,metaname,'UNKNOWN',istat)
#endif
           
           call igmeta(-11,-111) 
           print*,'generate Postscript- File: ',metaname 
        endif
        goto 5001
1111   write(*,*)'hplotpsdf: error open file: ',metaname
 
              
c---- buche histogramme -------------------------------------------
        
c bei postscript ab hier doppelt
5001       y1=s.ymi-(s.yma-s.ymi)/(2.0*iy) 
           y2=s.yma+(s.yma-s.ymi)/(2.0*iy) 
           z1=s.zmi-(s.zma-s.zmi)/(2.0*iz) 
           z2=s.zma+(s.zma-s.zmi)/(2.0*iz) 

           print*,'vor hbook2 ny nz      : ',iy,iz
           print*,'vor hbook2 y1,y2,z1,z2: ',y1,y2,z1,z2  
    	   call hbook2(200,'intensity',iz,z1,z2,iy,y1,y2,0.0)    
           do i=1,iy 
              y=sngl(yd(i))
              do j=1,iz
                 z=sngl(zd(j)) 
                 p=sngl(psd(i+(j-1)*iy))     ! *100.0
c                 write(*,*)'zyp',z,y,p
                 call hfill(200,z,y,p)
              enddo
           enddo
 
c--------- histogramme gebucht ------------------------

c--------- fuellen ------------------------------------
       
c        if (ist.eq.0)then 
c          write(*,*)'selected plotstyle not'
cc          s.plotsyle=0
c        endif

        call makeplotf(titel,s.plotsyle)    ! eigentliches plotten
        call iuwk(0,1)                      !puffer leeren update alle ws S.12

        if(ifile.eq.1)then  
           print*,'close postscriptfile'
           call igmeta(0,0)
           call hdelet(0)
           call hplend()
#ifdef VMS
           close(11)
#else
           call kuclos(11,' ',1)
#endif
           ifile= 0                         ! setze schalter zurueck
           call inithplot()   
           goto 5001
	endif

5000    call hdelet(0)                    !Speicher freigeben
        RETURN
	END

c           print*,'read phase space data'   
c            y und z sind bei psd vertauscht
c             
c           open(unit=10,file=datname,status='old') 
c           read(10,*)iz,iy            ! richtig
c tausch
c           y1=s.ymi-(s.yma-s.ymi)/(2.0*iy) 
c           y2=s.yma+(s.yma-s.ymi)/(2.0*iy) 
c           z1=s.zmi-(s.zma-s.zmi)/(2.0*iz) 
c           z2=s.zma+(s.zma-s.zmi)/(2.0*iz) 
c           print*,'vor hbook2 ny nz      : ',iy,iz
c           print*,'vor hbook2 y1,y2,z1,z2: ',y1,y2,z1,z2  
c    	   call hbook2(200,'intensity',iz,z1,z2,iy,y1,y2,0.0)    
c           do i=1,ianz 
c              read(10,*) zd,yd,dzd        ! richtig
c              y=sngl(yd)
c              z=sngl(zd) 
c              dz=sngl(dzd)     ! *100.0
c              call hfill(200,z,y,dz) 
c           enddo
c           print*,'gelesene Zeilen: ',ianz 
c           close(10)
c        else                                                   ! rtrace file
c***** end   subroutine hplotpsdff *********************************** 
c************************************************************************

c************************************************************************

        subroutine hplotpssimf(titel,metaname,psfile,
     &     ymin,ymax,iy,zmin,zmax,iz,
     &     psd) 
c     wird bei phasespace genutzt fuer simpson ausdruck
c     UF 11.2.00
c----------------------------------------------------------
        implicit none

        real*8 ymin,ymax,zmin,zmax,psd(4,2,4096)
        real*4 minx,maxx,tmp,x,y,dx
	character*(*) titel,metaname
        character*(9),subtitle
        integer ifile,psfile,ii,i,id,ix,iy,iz,istat
        
        write(*,*)'hplotpssimf called (for)'

c 10.2.2000 scale in mrad
c     postscriptfile 
        ifile=psfile             ! merker setzen
        if(ifile.eq.1)then 
 
#ifdef VMS
           print*,'open postsciptfile under VMS '
           open(unit=11,file=metaname,STATUS='UNKNOWN',
     1     Carriagecontrol='LIST',FORM='FORMATTED',err=6666)
#else 
           print*,'open postsciptfile under UNIX '
c           open(unit=11,file=metaname,STATUS='UNKNOWN',err=6666)
           call kuopen(11,metaname,'UNKNOWN',istat)
#endif
           call igmeta(-11,-111) 
           print*,'generate Postscript- File: ',metaname 
        endif
        goto 6667
 6666   write(*,*)'hplotpssimf: error open file: ',metaname
 6667   continue
c buchen fuellen,  10.2.output in mrad daher 1000*
        subtitle= 'dy (mrad)'
        do ii=1,4
           if(ii.eq.4)then
              minx= sngl(zmin)*1000
              maxx= sngl(zmax)*1000
              ix  = iz
           else
              minx= sngl(ymin)*1000
              maxx= sngl(ymax)*1000
              ix  = iy
           endif
           dx= (maxx-minx)/ ix
           minx=minx- dx/2.0
           maxx=maxx+ dx/2.0
           if(minx.gt.maxx)then
              tmp=minx
              minx=maxx
              maxx=tmp
           endif
           if (ii.eq.4)subtitle='dz (mrad)'
           id=1000+ii
           write(*,*)'call hbook1(',id,subtitle,ix,minx,maxx,',0.)' 
           call hbook1(id,subtitle,ix,minx,maxx,0.)

c ---------- fuellen --------------------------------
           do i=1,ix
              x=1000*sngl(psd(ii,1,i))
              y=sngl(psd(ii,2,i))
              call hf1(id,x,y)
           enddo
        enddo

c bei postscript ab hier doppelt
6001    call htitle(titel) 
        call hplzon(2,2,1,' ')
        call hplot(0,' ',' ',0)
        call iuwk(0,1)                      !puffer leeren update alle ws S.12

        if(ifile.eq.1)then  
           print*,'close postscriptfile'
           call igmeta(0,0)
           call hdelet(0)
           call hplend()
#ifdef VMS
           close(11) 
#else
           call kuclos(11,' ',1)
#endif
           ifile= 0                         ! setze schalter zurueck
           call inithplot()   
           goto 6001
	endif

        call hplzon(1,1,1,' ')
6000    call hdelet(0)                    !Speicher freigeben
        RETURN
	END

c****** end hplotpssimf ***********************************
 
c************************************************************************
        subroutine makeplotf(titel,ist)        
	implicit real*8(a-h,o-z)  
        character*(*) titel                 
c************************************************************************
 
        call htitle(titel) 
        call hplopt('DATE',1)
        if(ist.eq.0)then 
          call hplot(200,'SCAT ',' ',0)          !plot all ,1d punkte'' oder *
          call hplax('z "M# mm "N#', 'y "M# mm "N#') 
        endif  
        if(ist.eq.1)then 
           call hplot(200,'BOX',' ',0)  
           call hplax('z "M# mm "N#', 'y "M# mm "N#') 
        endif
        if(ist.eq.2)then 
           call hplot(200,'ARR',' ',0) 
	   call hplax('z "M# mm "N#', 'y "M# mm "N#') 
        endif
        if(ist.eq.3)then 
           call hplot(200,'CHAR',' ',0)         
           call hplax('z "M# mm "N#', 'y "M# mm "N#')
	endif
        if(ist.eq.4)then 
           call hplot(200,'TEXT',' ',0) 
	   call hplax('z "M# mm "N#', 'y "M# mm "N#')        
        endif
        if(ist.eq.5)then 
           call hplot(200,'CONT',' ',0)   
           call hplax('z "M# mm "N#', 'y "M# mm "N#')  
        endif
        if(ist.eq.6)then 
           call hplot(200,'COLZ',' ',0)
           call hplax('z "M# mm "N#', 'y "M# mm "N#')
        endif      
        if(ist.eq.7)then 
           call hplot(200,'LEGO',' ',0)  
           call hplax('z "M# mm "N#', 'y "M# mm "N#') 
        endif 
        if(ist.eq.8)then 
           call hplot(200,'LEGO1',' ',0) 
           call hplax('z "M# mm "N#', 'y "M# mm "N#') 
        endif 
        if(ist.eq.9)then 
           call hplot(200,'LEGO2',' ',0)
           call hplax('z "M# mm "N#', 'y "M# mm "N#') 
        endif 
	if(ist.eq.10)then 
           call hplot(200,'SURF',' ',0)
           call hplax('z "M# mm "N#', 'y "M# mm "N#') 
        endif     
        if(ist.eq.11)then 
           call hplot(200,'SURF1',' ',0) 
           call hplax('z "M# mm "N#', 'y "M# mm "N#') 
        endif 
	if(ist.eq.12)then 
           call hplot(200,'SURF2',' ',0)
           call hplax('z "M# mm "N#', 'y "M# mm "N#') 
        endif                                 
        if(ist.eq.13)then 
           call hplot(200,'SURF3',' ',0)   
           call hplax('z "M# mm "N#', 'y "M# mm "N#','intensity')  
        endif
        if(ist.eq.14)then 
           call hplot(200,'SURF4',' ',0) 
           call hplax('z "M# mm "N#', 'y "M# mm "N#') 
        endif  
	if(ist.eq.15) then 
           call hplopt('STAT',1) 
	   call hplot(400,' ',' ',0)          !plot all ,1d punkte'' oder *
           call hplax('z "M# mm "N#', 'rays') 
           call hplopt('NSTA',1) 
        endif          
	if(ist.eq.16)then 
           call hplopt('STAT',1) 
           call hplot(300,' ',' ',0)          !plot all ,1d punkte'' oder *
           call hplax('y "M# mm "N#', 'rays') 
           call hplopt('NSTA',1) 
        endif  
        if(ist.eq.177)then 
           write(*,*)'test hplpro'
           call hplzon(2,2,1,' ')
           call hplot(200,' ',' ',0)          !plot all ,1d punkte'' oder *
           call hplopt('STAT',1) 
           call hplot(200,' ','PROY',0) 
           call hplax('y "M# mm "N#', 'rays')
           call hplot(200,' ','PROX',0) 
           call hplax('z "M# mm "N#', 'rays')
           call hplot(700,' ',' ',0) 
           call hplax('[l] "M# nm "N#', 'rays') 
           call hplopt('NSTA',1) 
c           call hplpro(200,'xp','yp') ! scatter plot with projections
           call hplzon(1,1,1,' ')
        endif  
        if(ist.eq.17)then 
           call hplzon(2,2,1,' ')
           call hplot(200,' ',' ',0)          !plot all ,1d punkte'' oder *
           call hplax('z "M# mm "N#', 'y "M# mm "N#') 
           call hplopt('STAT',1) 
	   call hplot(300,' ',' ',0)   
           call hplax('y "M# mm "N#', 'rays') 
	   call hplot(400,' ',' ',0) 
           call hplax('z "M# mm "N#', 'rays') 
           call hplot(700,' ',' ',0) 
           call hplax('[l] "M# nm "N#', 'rays') 
           call hplopt('NSTA',1) 
           call hplzon(1,1,1,' ')
        endif      
        if(ist.eq.18)then 
           call hplzon(2,1,1,' ')
           call hplopt('STAT',1) 
	   call hplot(500,' ',' ',0)   
           call hplax('dy "M# mrad "N#', 'rays') 
	   call hplot(600,' ',' ',0) 
           call hplax('dz "M# mrad "N#', 'rays') 
           call hplopt('NSTA',1) 
           call hplzon(1,1,1,' ')
        endif      
        if(ist.eq.19)then 
           call hplopt('STAT',1) 
           call hplot(700,' ',' ',0) 
           call hplax('[l] "M# nm "N#', 'rays') 
           call hplopt('NSTA',1) 
        endif  
        if(ist.eq.20)then 
           call hplot(202,'SURF3',' ',0)
           call hplax('z (mm)', 'y (mm)') 
        endif   
        if(ist.eq.21)then 
           call hplot(202,'SURF3',' ',0)
           call hplax('z (mm)', 'y (mm)')
        endif   
        if(ist.eq.22)then 
           call hplot(800,' ',' ',0) 
           call hplax('time (fs)', 'rays') 
        endif 
        RETURN
	END 
c****** end   subroutine makeplotf(titel,ist) ***************************       


c************************************************************************
        subroutine kanalzahlf(ist,ixkanal,iykanal)                   
	implicit real*8(a-h,o-z)  
c************************************************************************
 
        if(ist.eq.0)then   
	  ixkanal=300
        endif 
        if(ist.ge.1.AND.ist.le.6)then
	  ixkanal=60 
        endif 
        if(ist.ge.7.and.ist.le.14)then
          ixkanal=30 
        endif 
        if(ist.ge.15)then
	  ixkanal=100 
        endif 
        if(ist.eq.19)then
	  ixkanal=300 
        endif
        if(ist.eq.20.or.ist.eq.21)then
	  ixkanal=30 
        endif
        iykanal=ixkanal 

        RETURN
	END 
c************  end  subroutine kanalzahlf(ist,ixkanal,iykanal) ************    

c************************************************************************
        subroutine rtfill(yd,zd,dyd,dzd,lam,dla,dt)                   
c last modification: 30 Sep 97 10:00:14 flechsig
c************************************************************************
        real*4 dy,dz,y,z,la,t       
        real*8 yd,zd,dyd,dzd,lam,dla,dt

        y=sngl(yd)
        z=sngl(zd)      
        dy=sngl(dyd)*1e3
        dz=sngl(dzd)*1e3  
        la=sngl(yd*dla+lam)*1e6 
c        t=sngl(dt)*5.309e2       ! 1e15/(2pi c)  time in fs
c        phi= 1.0
		     
        call hfill(200,z,y,1.0)  
        call hfill(300,y,0.0,1.0)  
        call hfill(400,z,0.0,1.0)   
        call hfill(500,dy,0.0,1.0)  
        call hfill(600,dz,0.0,1.0)   
        call hfill(700,la,0.0,1.0)
c        call hfill(800,t,0.0,1.0) 
                                  
        RETURN
	END 
c************  end  subroutine rtfill() ************    

c************************************************************************
        subroutine rtfillnorm(yd,zd,pd,lambda)  
c        implicit real*8(a-h,o-z)
C special for phase and time fill histogram and average
c************************************************************************
        real*4 y,z,t,p       
        real*8 yd,zd,pd,td,lambda

        y=sngl(yd)
        z=sngl(zd) 
        p=sngl(pd)              
        td= pd* lambda/ 1.8849e-3
        t=sngl(td)         ! (1e15/1.8849e12 = 2*3.1415*3e11))   2pi c  t in fs

c        write(*,*)'p,t,td,pd,lambda',p,t,td,pd,lambda	
        call hfill(201,z,y,p) 
        call hfill(200,z,y,1.0) 
        call hfill(800,t,0.0,1.0) 
                                  
        RETURN
	END 
c************  end  subroutine rtfillnorm() ************    


c*************** end phasegraffor.for   ***********************************
