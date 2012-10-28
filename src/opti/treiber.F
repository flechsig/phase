c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/opti/treiber.fpp
c$$$ Date      : <29 Oct 03 16:40:13 flechsig> 
c$$$ Time-stamp: <2012-10-28 14:45:45 flechsig> 
c$$$ Author    : Uwe Flechsig, flechsig@psi.ch
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 



        subroutine fminuinit(iread, readname) 
        character*(*) readname
        integer iread,istat 
        call kuopen(iread,readname,'UNKNOWN',istat)  
        write(*,*)'fminuinit::read from: ',readname
        call MINTIO(iread,6,7)  			!6,7 are the defaults  
	return
	end     
   
        subroutine fminuend(iread)
        integer iread,istatus

c     keep close for VMS version since kuclos not tested in VMS UF 11.11.03
        call kuclos(iread,' ',istatus)
        return
	end    

        subroutine rewindinput(iread) 
        integer iread

        write(*,*)'rewind unit ',iread
        rewind(iread)
        return
	end           

c end /afs/psi.ch/user/f/flechsig/phase/src/opti/treiber.fpp
