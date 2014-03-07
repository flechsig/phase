C Datei: USERDISK_3:[FLECHSIG.PHASE.OPTI]SUBFOR.FOR
C Datum: 21.JUL.1994
C Stand: 23-APR-1996
C Autor: FLECHSIG, BESSY Berlin

        subroutine fgmap_and_xxmap_for(a,g,xeps,xmap35)      !epsilon
        implicit real*8(a-h,o-z)  

        structure/geometryst/
           real*8 sina,cosa,sinb,cosb,
     &            r,rp,xdens(0:4),xlam
	   integer idefl   
        end structure
        record /geometryst/ g   
        dimension a(0:5,0:5)              ! mirror     

        dimension wc(0:4,0:4,0:4,0:4),
     &            xlc(0:4,0:4,0:4,0:4),  
     &            ypc1(0:4,0:4,0:4,0:4),
     &            zpc1(0:4,0:4,0:4,0:4),
     &            dypc(0:4,0:4,0:4,0:4),
     &            dzpc(0:4,0:4,0:4,0:4)    

        dimension xmap35(35,35)

        call fgmap3dp(5,xeps,a,g,
     &                  wc,xlc,ypc1,zpc1,dypc,dzpc)    !5-> optimierung
        call xxmap35(xmap35,ypc1,zpc1,dypc,dzpc) 
                                     
       	return
	end    

c*** Routinen die aus fg3dp_phase uebernommen wurden *****
c    glue3                                               *
c*********************************************************

c*********************************************************
	subroutine glue3(xmap35a,xmap35b,xmap35)
c*********************************************************
        IMPLICIT REAL*8(A-H,O-Z)

        dimension xmap35(35,35),xmap35a(35,35),xmap35b(35,35)

	do i=1,35
	do j=1,35
	 xmap35(i,j)=0.
	 do k=1,35
	  xmap35(i,j)=xmap35(i,j)+xmap35b(i,k)*xmap35a(k,j)
	 enddo
	enddo
	enddo

	return
	end     


