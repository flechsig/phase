c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/phasegraffor.fpp
c$$$ Date      : <11 Nov 03 09:30:31 flechsig> 
c$$$ Time-stamp: <11 Nov 03 09:30:53 flechsig> 
c$$$ Author    : Uwe Flechsig, flechsig@psi.ch
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 

c
c fortran file which will be preprocessed #define LINUX #define VMS
c


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
            real*8 ymi,yma,zmi,zma,dymi,dyma,dzmi,dzma 
            integer zeilen,psfile,status,plotsyle,
     &              iykanal,izkanal,ititel(80)
        end structure
        record /struct/ s
	dimension raysout(5,1)                  ! gummilaenge
	character*(*) titel,metaname
        real*4        y1,y2,z1,z2       
        real*8	      dlfac,lambda,dlf,raysout
        integer       ifile,i,iy,iz,istat

c temporarely fixed
c	parameter (dtitel = 'PHASE')
c        parameter (dmetaname= 'pmeta.ps')
c************************************************************************
 
        y1=s.ymi
        y2=s.yma
        z1=s.zmi
        z2=s.zma  
        
c        print*,'title and plotfilename are temporarely fixed'
        print*,'hplotdisplayf:',y1,y2,z1,z2    
        print*,'title         ',titel 
        print*,'ps filename   ',metaname    
        print*,'status, zeilen ',s.status, s.zeilen
        print*,'style ', s.plotsyle
        dlfac=dlf
        write(*,*)'dlfactor (mm)',dlfac,' lambda (mm)',lambda
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
              
        write(*,*)'only for ray trace data'   
        goto 4445
4444  write(*,*)'hplotdisplayf: error open file: ',metaname
c---- buche histogramme -------------------------------------------
4445  call kanalzahlf(s.plotsyle,iy,iz) ! iy,iz neu belegt
1001    y1=s.ymi
        y2=s.yma
        z1=s.zmi
        z2=s.zma
c bei postscript ab hier doppelt
        write(*,*)'vor hbook2 ny nz      : ',iy,iz        
        write(*,*)'vor hbook2 y1,y2,z1,z2: ',y1,y2,z1,z2     
      	call hbook2(200,'PHASE',iz,z1,z2,iy,y1,y2,5000.)         
        call hbook1(300,'PHASE',iy,y1,y2,5000.)   
        call hbook1(400,'PHASE',iz,z1,z2,5000.)  
        y1=(y1*dlfac+lambda)*1e6
        y2=(y2*dlfac+lambda)*1e6
        call hbook1(700,'PHASE',iy,y1,y2,5000.)   ! dlambda 
         
        y1=s.dymi*1e3
        y2=s.dyma*1e3
        z1=s.dzmi*1e3
        z2=s.dzma*1e3

        write(*,*)'diverg.: book dy1,dy2,dz1,dz2: ',y1,y2,z1,z2     

        call hbook1(500,'PHASE',iy,y1,y2,5000.)   
        call hbook1(600,'PHASE',iz,z1,z2,5000.)       
c--------- histogramme gebucht ------------------------

c--------- fuellen ------------------------------------
        write(*,*)'fill histogram'        
        do i=1,s.zeilen 
c           write(*,*)'fill histogr.',i  
	call rtfill(raysout(1,i),raysout(2,i),
     &                 raysout(3,i),raysout(4,i),
     &	               lambda,dlfac)
     	enddo
	
        write(*,*)'for: histograms filled'  
        
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
            real*8 ymi,yma,zmi,zma,dymi,dyma,dzmi,dzma 
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
           
           id=1000+ii
           write(*,*)'call hbook1(',id,',mrad,',ix,minx,maxx,',0.)' 
           call hbook1(id,'mrad',ix,minx,maxx,0.)

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
           call hplot(200,'COL',' ',0)
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
        iykanal=ixkanal 

        RETURN
	END 
c************  end  subroutine kanalzahlf(ist,ixkanal,iykanal) ************    

c************************************************************************
        subroutine rtfill(yd,zd,dyd,dzd,lam,dla)                   
c last modification: 30 Sep 97 10:00:14 flechsig
	implicit real*8(a-h,o-z)  
c************************************************************************
        real*4 dy,dz,y,z,la       
        real*8 lam,dla

        y=sngl(yd)
        z=sngl(zd)      
        dy=sngl(dyd)*1e3
        dz=sngl(dzd)*1e3  
        la=sngl(yd*dla+lam)*1e6
c        phi= 1.0
		     
        call hfill(200,z,y,1.0)  
        call hfill(300,y,0.0,1.0)  
        call hfill(400,z,0.0,1.0)   
        call hfill(500,dy,0.0,1.0)  
        call hfill(600,dz,0.0,1.0)   
        call hfill(700,la,0.0,1.0)  
                                  
        RETURN
	END 
c************  end  subroutine rtfill() ************    

c*************** end phasegraffor.for   ***********************************
