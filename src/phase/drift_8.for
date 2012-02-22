c-------------------------------------------------------------
	subroutine drift_8(g,acc,wc,
     &  xlc,ypc1,zpc1,dypc,dzpc,imodus,iord)
c-------------------------------------------------------------
	implicit real*8(a-h,o-z)
	structure/geometryst/
           real*8 sina,cosa,sinb,cosb,
     &            r,rp,xdens(0:4),xlam
           integer idefl
        end structure
        record /geometryst/g
        dimension   wc(0:7,0:7,0:7,0:7),
     &              xlc(0:7,0:7,0:7,0:7),
     &              ypc1(0:7,0:7,0:7,0:7),
     &              zpc1(0:7,0:7,0:7,0:7),
     &              dypc(0:7,0:7,0:7,0:7),
     &              dzpc(0:7,0:7,0:7,0:7)

        drift=g.r+g.rp
        do n1=0,iord
         do n2=0,iord-n1
          do n3=0,iord-n1-n2
           do n4=0,iord-n1-n2-n3
            ypc1(n1,n2,n3,n4)=0.d0
            zpc1(n1,n2,n3,n4)=0.d0
            dypc(n1,n2,n3,n4)=0.d0
            dzpc(n1,n2,n3,n4)=0.d0
            wc(n1,n2,n3,n4)=0.d0
            xlc(n1,n2,n3,n4)=0.d0
           enddo
          enddo
         enddo
        enddo

        ypc1(1,0,0,0)=1.d0
        ypc1(0,0,1,0)=drift
        zpc1(0,1,0,0)=1.d0
        zpc1(0,0,0,1)=drift

        dypc(0,0,1,0)=1.d0
        dzpc(0,0,0,1)=1.d0

	return
	end
