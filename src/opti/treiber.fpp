c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/opti/treiber.fpp
c$$$ Date      : <29 Oct 03 16:40:13 flechsig> 
c$$$ Time-stamp: <29 Oct 03 16:40:18 flechsig> 
c$$$ Author    : Uwe Flechsig, flechsig@psi.ch
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 



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
