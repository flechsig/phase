c$$$ File      : /afs/psi.ch/user/f/flechsig/phase/src/opti/treiber.fpp
c$$$ Date      : <29 Oct 03 16:40:13 flechsig> 
c$$$ Time-stamp: <04 Nov 03 15:03:03 flechsig> 
c$$$ Author    : Uwe Flechsig, flechsig@psi.ch
c$$$
c$$$ $Source$ 
c$$$ $Date$
c$$$ $Revision$ 
c$$$ $Author$ 



        subroutine fminuinit(iread, readname) 
        character*(*) readname
        integer iread,istat 
#ifdef VMS
        open(unit=iread,file=readname,STATUS='OLD') 
#else
        call kuopen(iread,readname,'UNKNOWN',istat)
#endif   
        write(*,*)'fminuinit::read from: ',readname
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

        write(*,*)'rewind unit ',iread
        rewind(iread)
        return
	end           

c end /afs/psi.ch/user/f/flechsig/phase/src/opti/treiber.fpp
