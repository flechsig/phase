C Datei: USERDISK_3:[FLECHSIG.PHAS]SERROR.FOR
C Datum: 15.MAY.1996
C Stand: 15-MAY-1996
C Autor: FLECHSIG, BESSY Berlin

c********************************************************
c berechnet eine Achsensymmetrische Funktion 4. Grades
c********************************************************
	real function serror(x,y)
   	vector p
        double precision z  

        z=p(1)+p(2)*(x**2)+p(3)*(y**2)
     &        +p(4)*(x**4)+p(5)*(y**4)+p(6)*(x**2)*(y**2)
               
	serror=z
        return 
	end
         
