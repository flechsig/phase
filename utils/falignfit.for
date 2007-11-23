C Datei: USERDISK_3:[FLECHSIG.PHAS]FALIGNFIT.FOR
C Datum: 14.MAY.1996
C Stand: 14-MAY-1996
C Autor: FLECHSIG, BESSY Berlin


	REAL function falignfit(x,y)
        common/pawpar/par(21)
        double precision xi
	vector a
        integer i,j,k
c       x ist w und y ist l (wegen Paw)

        xi=0.0
        k=1
        do 1111 i=0,5
          do 2222 j=0,5
            if ((i+j).le.5) then
	      a((i+1),(j+1))=par(k) 
c             xi=xi+a(k)*(x**i)*(y**j)   ! Klammern sind wichtig
	      xi=xi+a((i+1),(j+1))*(x**i)*(y**j) ! Klammern sind wichtig
	      k=k+1    
            endif
2222	  continue
1111    continue  
        falignfit=xi

	return
	end
