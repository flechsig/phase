c File      : /home/vms/flechsig/vms/phas/opti/treiber.for
c Date      : <09 Oct 97 13:00:45 flechsig> 
c Time-stamp: <11 Nov 97 08:57:53 flechsig> 
c Author    : Uwe Flechsig, flechsig@exp.bessy.de

C Datei: USERDISK_3:[FLECHSIG.PHASE.OPTI]TREIBER.FOR
C Datum: 19.JUL.1994
C Stand: 12-APR-1996
C Autor: FLECHSIG, BESSY Berlin


        subroutine fminuinit(iread, readname) 
        character*(*) readname
        integer iread

        open(unit=iread,file=readname,STATUS='OLD')    
        write(*,*),'read from: ',readname
        call MINTIO(iread,6,7)  			!6,7 are the defaults  
	return
	end     
   
        subroutine fminuend(iread)
        integer iread

        close(iread)
        return
	end    

        subroutine rewindinput(iread) 
        integer iread

        rewind(iread)
        return
	end           
