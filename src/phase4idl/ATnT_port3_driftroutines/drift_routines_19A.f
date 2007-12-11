C$TEST NSNM                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SMNSX                             
C                                                                       
C***********************************************************************
C *** SMNSX EXAMPLE PROGRAM ***                                         
C                                                                       
C *** MINIMIZE F(X) = 0.1*S(X)**4 + SUM(I = 1(1)3) (I * (X(I) - 10)**2),
C *** WHERE S(X) = SUM(I = 1(1)3) X(I),                                 
C *** STARTING FROM     X = (2, 30, 9).                                 
C                                                                       
      INTEGER I, J, IWRITE, P                                           
      REAL FX, S(3,4), STEP, TOL, X(3)                                  
      EXTERNAL I1MACH, MNSX, QF, SMNSX                                  
      INTEGER I1MACH                                                    
      REAL MNSX, SMNSX                                                  
C                                                                       
C *** USE COMMON TO FIND NUMBER OF TIMES F(X) IS EVALUATED...           
C                                                                       
      INTEGER NF                                                        
      COMMON /SXCOMN/ NF                                                
C                                                                       
      DATA P/3/                                                         
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
C                                                                       
C *** FIRST SOLVE THE PROBLEM USING SMNSX...                            
C                                                                       
      X(1) = 2.E0                                                       
      X(2) = 3.E1                                                       
      X(3) = 9.E0                                                       
C                                                                       
      NF = 0                                                            
C     *** STEP AND TOL ARE USED AS BOTH INPUT AND OUTPUT PARAMETERS,    
C     *** SO WE MUST NOT PASS CONSTANTS FOR THEM.                       
      STEP = 1.E0                                                       
      TOL = 1.E-10                                                      
C                                                                       
      FX = SMNSX(QF, P, STEP, TOL, X)                                   
C                                                                       
C *** PRINT OUT THE SOLUTION (ON THE STANDARD OUTPUT UNIT) ***          
C                                                                       
      IWRITE = I1MACH(2)                                                
      WRITE(IWRITE,10) FX, TOL, STEP, X, NF                             
 10   FORMAT(21H SMNSX RETURNS F(X) =, E13.6,7H, TOL =, E10.3/          
     1       11H AND STEP =, E10.3/7H AT X =, 3E14.6/6H AFTER, I5,      
     2       21H FUNCTION EVALUATIONS)                                  
C                                                                       
C *** SOLVE THE PROBLEM AGAIN, THIS TIME USING MNSX...                  
C                                                                       
      X(1) = 2.0E0                                                      
      X(2) = 30.0E0                                                     
      X(3) = 9.0E0                                                      
C                                                                       
C                                                                       
C *** CREATE INITIAL SIMPLEX...                                         
C                                                                       
      DO 30 J = 1, 4                                                    
         DO 20 I = 1, 3                                                 
            S(I,J) = X(I) - 0.5E0                                       
 20         CONTINUE                                                    
         IF (J .LE. 3) S(J,J) = X(J) + 0.5E0                            
 30      CONTINUE                                                       
C                                                                       
      NF = 0                                                            
      TOL = 1.E-10                                                      
C                                                                       
      FX = MNSX(QF, 1000, P, P, S, TOL, X)                              
C                                                                       
C *** PRINT OUT THE SOLUTION ***                                        
C                                                                       
      WRITE(IWRITE,40) FX, TOL, X, NF                                   
 40   FORMAT(/20H MNSX RETURNS F(X) =, E13.6,10H AND TOL =, E10.3/      
     1       7H AT X =,3E14.6/6H AFTER, I5, 21H FUNCTION EVALUATIONS)   
 999  STOP                                                              
      END                                                               
      REAL FUNCTION QF(P, X)                                            
C                                                                       
C *** THIS ROUTINE COMPUTES THE OBJECTIVE FUNCTION, F(X)                
C                                                                       
      INTEGER P                                                         
      REAL X(P)                                                         
C                                                                       
      INTEGER NF                                                        
      COMMON /SXCOMN/ NF                                                
C                                                                       
      INTEGER I                                                         
      REAL PT1, TEN, ZERO                                               
C                                                                       
      DATA PT1 /0.1E0/, TEN/1.E1/, ZERO/0.E0/                           
C                                                                       
C                                                                       
      NF = NF + 1                                                       
      QF = ZERO                                                         
      DO 10 I = 1, P                                                    
 10      QF = QF + X(I)                                                 
      QF = PT1 * QF**4                                                  
      DO 20 I = 1, P                                                    
 20      QF = QF + I*(X(I) - TEN)**2                                    
 999  RETURN                                                            
      END                                                               
C$TEST NSFA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM NSF1                              
C                                                                       
C***********************************************************************
C  EXAMPLE PROGRAM FOR NSF1 TO FIT                                      
C  N DATA POINTS (T,Y) TO CURVE                                         
C  C(1)*EXP(T*X)  +  C(2)                                               
C                                                                       
       INTEGER IWRITE                                                   
       REAL C(2), T(8), Y(8), TT(8), YY(8)                              
       DOUBLE PRECISION S                                               
       EXTERNAL GETAY                                                   
       COMMON /DATBLK/TT,YY                                             
       DATA T(1) /12.0/, T(2) /20.0/ ,T(3) /28.0/, T(4) /48.0/,         
     1 T(5)/120.0/, T(6) /240.0/, T(7) /900.0/, T(8) /2400.0/           
       DATA Y(1) /0.2342/, Y(2) /0.2244/ , Y(3) /0.2204/,               
     1 Y(4) /0.2149/, Y(5) /0.2063/, Y(6) /0.1983/,                     
     2 Y(7) /0.1842/, Y(8)/0.1761/                                      
C                                                                       
C  SET UP OUTPUT UNIT                                                   
C                                                                       
       IWRITE = I1MACH(2)                                               
C                                                                       
C  MOVE T AND Y VECTORS TO COMMON                                       
C                                                                       
       DO 2 I=1,8                                                       
         TT(I) = T(I)                                                   
         YY(I) = Y(I)                                                   
  2    CONTINUE                                                         
C                                                                       
        N = 8                                                           
        L =  2                                                          
       X1 = -10.0                                                       
       X2 =   0.001                                                     
C                                                                       
C  DO THE FIT                                                           
C                                                                       
       CALL NSF1(N, L, X, X1, X2, 1.E-6, C)                             
       WRITE(IWRITE, 4) X, C(1), C(2)                                   
  4      FORMAT(5H X = , E20.10/8H C(1) = ,E20.10/8H C(2) = , E20.10)   
C                                                                       
       WRITE(IWRITE, 5)                                                 
  5      FORMAT(//,19X,1HT,14X,6HREAL Y,14X,5HEST.Y,15X,5HERROR,/)      
       DO 100 I=1,N                                                     
         YEST = C(1)*EXP(T(I)*X)+C(2)                                   
         YERR = ABS(Y(I)-YEST)                                          
         WRITE(IWRITE, 6) T(I), Y(I), YEST, YERR                        
 100     S = S + YERR*YERR                                              
   6     FORMAT (4E20.10)                                               
       WRITE(IWRITE, 7) S                                               
   7     FORMAT(//,24HSUM OF ERRORS SQUARED = ,D20.10)                  
       STOP                                                             
       END                                                              
       SUBROUTINE GETAY(N,L,X,A,Y)                                      
       INTEGER N,L                                                      
       REAL A(N,L),X,Y(N)                                               
       REAL T(8),YY(8)                                                  
       COMMON /DATBLK/T,YY                                              
       DO 100 I=1,N                                                     
          A(I,1)=EXP(X*T(I))                                            
          A(I,2)=1.0                                                    
          Y(I)=YY(I)                                                    
 100   CONTINUE                                                         
       RETURN                                                           
       END                                                              
C$TEST NP2A                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SN2F                              
C                                                                       
C***********************************************************************
        INTEGER N,P                                                     
        EXTERNAL OSBN                                                   
        REAL Y(10),T(10),YY(10),X(5)                                    
        COMMON /YT/YY,T                                                 
C GENERATE DATA FOR PROBLEM                                             
        DATA Y(1)/8.44E-1/, Y(2) /9.36E-1/, Y(3) /8.81E-1/              
     1  Y(4)/7.84E-1/, Y(5)/ 6.85E-1/, Y(6)/6.03E-1/,                   
     2  Y(7) /5.38E-1/ , Y(8) /4.90E-1/, Y(9)/4.57E-1/                  
        P=5                                                             
        N=9                                                             
        DO 10 I=1,9                                                     
           YY(I) = Y(I)                                                 
           T(I)=-30.E0*FLOAT(I-1)                                       
 10     CONTINUE                                                        
C INITIALIZE X                                                          
        X(1)=0.5                                                        
        X(2)=1.5                                                        
        X(3)=-1.                                                        
        X(4)=.01                                                        
        X(5)=.02                                                        
C                                                                       
C SOLVE THE PROBLEM                                                     
C                                                                       
        CALL SN2F(N, P, X, OSBN, 100, 1.E-4)                            
C       PRINT RESULTS ON STANDARD OUTPUT UNIT                           
        IWRITE = I1MACH(2)                                              
        WRITE(IWRITE, 20)(X(I),I=1,P)                                   
 20     FORMAT(10H SOLUTION-,5E15.5)                                    
        STOP                                                            
        END                                                             
        SUBROUTINE OSBN(N,P,X,NF,R)                                     
C THIS SUBROUTINE COMPUTES THE MODEL                                    
        INTEGER P, N, NF                                                
        REAL X(P), R(N)                                                 
        REAL Y(10), T(10)                                               
        COMMON /YT/ Y, T                                                
        DO 10 I=1,N                                                     
           R(I)=Y(I)-(X(1)+X(2)*EXP(X(4)*T(I))+X(3)*EXP(X(5)*T(I)))     
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
C$TEST NP2B                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SN2FB                             
C                                                                       
C***********************************************************************
        INTEGER N,P                                                     
        EXTERNAL OSBN                                                   
        REAL Y(10),YY(10),T(10),X(5),B(2,5)                             
        COMMON /YT/YY,T                                                 
C GENERATE DATA FOR PROBLEM                                             
        DATA Y(1)/8.44E-1/, Y(2) /9.36E-1/, Y(3) /8.81E-1/              
     1  Y(4)/7.84E-1/, Y(5)/ 6.85E-1/, Y(6)/6.03E-1/,                   
     2  Y(7) /5.38E-1/ , Y(8) /4.90E-1/, Y(9)/4.57E-1/                  
        P=5                                                             
        N=9                                                             
        DO 10 I=1,9                                                     
           YY(I) = Y(I)                                                 
           T(I)=-30.E0*FLOAT(I-1)                                       
 10     CONTINUE                                                        
C INITIALIZE X                                                          
        X(1)=0.5                                                        
        X(2)=1.5                                                        
        X(3)=-1.                                                        
        X(4)=.01                                                        
        X(5)=.03                                                        
C SUPPLY BOUNDS                                                         
C                                                                       
C SET VARIABLES WE DO NOT WANT TO BE BOUNDED TO BIGGEST AND             
C AND SMALLEST NUMBERS IN THE MACHINE                                   
        BIG=R1MACH(2)                                                   
        DO 20 I=1, P                                                    
           B(1,I)=-BIG                                                  
           B(2,I)=BIG                                                   
 20     CONTINUE                                                        
        B(2,4)=0.125                                                    
        B(1,5)=0.03                                                     
C                                                                       
C SOLVE THE PROBLEM                                                     
C                                                                       
        CALL SN2FB(N, P, X, B, OSBN, 100, 1.E-4)                        
C       PRINT RESULTS ON STANDARD OUTPUT UNIT                           
        IWRITE = I1MACH(2)                                              
        WRITE(IWRITE, 30)(X(I),I=1,P)                                   
 30     FORMAT(10H SOLUTION-,5E15.5)                                    
        STOP                                                            
        END                                                             
        SUBROUTINE OSBN(N,P,X,NF,R)                                     
        INTEGER P, N, NF                                                
        REAL X(P), R(N)                                                 
        REAL Y(10), T(10)                                               
        COMMON /YT/ Y, T                                                
        DO 10 I=1,N                                                     
           R(I)=Y(I)-(X(1)+X(2)*EXP(X(4)*T(I))+X(3)*EXP(X(5)*T(I)))     
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
C$TEST NP2E                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SN2G                              
C                                                                       
C***********************************************************************
        INTEGER N,P                                                     
        EXTERNAL OSBN, OSBNJ                                            
        REAL Y(10),YY(10),T(10),X(5)                                    
        COMMON /YT/YY,T                                                 
C GENERATE DATA FOR PROBLEM                                             
        DATA Y(1)/8.44E-1/, Y(2) /9.36E-1/, Y(3) /8.81E-1/              
     1  Y(4)/7.84E-1/, Y(5)/ 6.85E-1/, Y(6)/6.03E-1/,                   
     2  Y(7) /5.38E-1/ , Y(8) /4.90E-1/, Y(9)/4.57E-1/                  
        P=5                                                             
        N=9                                                             
        DO 10 I=1,9                                                     
           YY(I) = Y(I)                                                 
           T(I)=-30.E0*FLOAT(I-1)                                       
 10     CONTINUE                                                        
C INITIALIZE X                                                          
        X(1)=0.5                                                        
        X(2)=1.5                                                        
        X(3)=-1.                                                        
        X(4)=.01                                                        
        X(5)=.02                                                        
C                                                                       
C SOLVE THE PROBLEM                                                     
C                                                                       
        CALL SN2G(N, P, X, OSBN, OSBNJ, 100, 1.E-4)                     
C       PRINT RESULTS ON STANDARD OUTPUT UNIT                           
        IWRITE = I1MACH(2)                                              
        WRITE(IWRITE, 20)(X(I),I=1,P)                                   
 20     FORMAT(10H SOLUTION-,5E15.5)                                    
        STOP                                                            
        END                                                             
        SUBROUTINE OSBN(N,P,X,NF,R)                                     
C THIS SUBROUTINE COMPUTES THE MODEL                                    
        INTEGER P, N, NF                                                
        REAL X(P), R(N)                                                 
        REAL Y(10), T(10)                                               
        COMMON /YT/ Y, T                                                
        DO 10 I=1,N                                                     
           R(I)=Y(I)-(X(1)+X(2)*EXP(X(4)*T(I))+X(3)*EXP(X(5)*T(I)))     
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        SUBROUTINE OSBNJ(N,P,X,NF,J)                                    
C THIS SUBROUTINE COMPUTES THE JACOBIAN OF THE MODEL                    
        INTEGER P, N, NF                                                
        REAL X(P), J(N,P)                                               
        REAL Y(10), T(10)                                               
        COMMON /YT/ Y, T                                                
        DO 10 I=1,N                                                     
           J(I,1)=-1.0E0                                                
           J(I,2)=-EXP(X(4)*T(I))                                       
           J(I,3)=-EXP(X(5)*T(I))                                       
           J(I,4)=-T(I)*X(2)*EXP(X(4)*T(I))                             
           J(I,5)=-T(I)*X(3)*EXP(X(5)*T(I))                             
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
C$TEST NP2F                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SN2GB                             
C                                                                       
C***********************************************************************
        INTEGER N,P                                                     
        EXTERNAL OSBN,OSBNJ                                             
        REAL Y(10),YY(10),T(10),X(5),B(2,5)                             
        COMMON /YT/YY,T                                                 
C GENERATE DATA FOR PROBLEM                                             
        DATA Y(1)/8.44E-1/, Y(2) /9.36E-1/, Y(3) /8.81E-1/              
     1  Y(4)/7.84E-1/, Y(5)/ 6.85E-1/, Y(6)/6.03E-1/,                   
     2  Y(7) /5.38E-1/ , Y(8) /4.90E-1/, Y(9)/4.57E-1/                  
        P=5                                                             
        N=9                                                             
        DO 10 I=1,9                                                     
           YY(I) = Y(I)                                                 
           T(I)=-30.E0*FLOAT(I-1)                                       
 10     CONTINUE                                                        
C INITIALIZE X                                                          
        X(1)=0.5                                                        
        X(2)=1.5                                                        
        X(3)=-1.                                                        
        X(4)=.01                                                        
        X(5)=.03                                                        
C SUPPLY BOUNDS                                                         
C                                                                       
C SET VARIABLES WE DO NOT WANT TO BE BOUNDED TO BIGGEST AND             
C AND SMALLEST NUMBERS IN THE MACHINE                                   
        BIG=R1MACH(2)                                                   
        DO 20 I=1, P                                                    
           B(1,I)=-BIG                                                  
           B(2,I)=BIG                                                   
 20     CONTINUE                                                        
        B(2,4)=0.125                                                    
        B(1,5)=0.03                                                     
C                                                                       
C SOLVE THE PROBLEM                                                     
C                                                                       
        CALL SN2GB(N, P, X, B, OSBN, OSBNJ, 100, 1.E-4)                 
C       PRINT RESULTS ON STANDARD OUTPUT UNIT                           
        IWRITE = I1MACH(2)                                              
        WRITE(IWRITE, 30)(X(I),I=1,P)                                   
 30     FORMAT(10H SOLUTION-,5E15.5)                                    
        STOP                                                            
        END                                                             
        SUBROUTINE OSBN(N,P,X,NF,R)                                     
C THIS SUBROUTINE COMPUTES THE MODEL                                    
        INTEGER P, N, NF                                                
        REAL X(P), R(N)                                                 
        REAL Y(10), T(10)                                               
        COMMON /YT/ Y, T                                                
        DO 10 I=1,N                                                     
           R(I)=Y(I)-(X(1)+X(2)*EXP(X(4)*T(I))+X(3)*EXP(X(5)*T(I)))     
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        SUBROUTINE OSBNJ(N,P,X,NF,J)                                    
C THIS SUBROUTINE COMPUTES THE JACOBIAN OF THE MODEL                    
        INTEGER P, N, NF                                                
        REAL X(P), J(N,P)                                               
        REAL Y(10), T(10)                                               
        COMMON /YT/ Y, T                                                
        DO 10 I=1,N                                                     
           J(I,1)=-1.0E0                                                
           J(I,2)=-EXP(X(4)*T(I))                                       
           J(I,3)=-EXP(X(5)*T(I))                                       
           J(I,4)=-T(I)*X(2)*EXP(X(4)*T(I))                             
           J(I,5)=-T(I)*X(3)*EXP(X(5)*T(I))                             
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
C$TEST NTLE                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SMNFB                             
C                                                                       
C***********************************************************************
        INTEGER N                                                       
        EXTERNAL ROSN                                                   
        REAL X(2), B(2,2)                                               
        N=2                                                             
C INITIALIZE X                                                          
        X(1)=-1.2                                                       
        X(2)=1.0                                                        
C SET UP THE BOUND ARRAY                                                
C R1MACH(2) CONTAINS THE LARGEST NUMBER IN THE MACHINE                  
        B(1,1)=-R1MACH(2)                                               
        B(2,1)=0.5                                                      
        B(1,2)=0.0                                                      
        B(2,2)=1.0                                                      
C                                                                       
C SOLVE THE PROBLEM                                                     
C                                                                       
        CALL SMNFB(N, X, B, ROSN, 100, 1.E-4)                           
C       PRINT RESULTS ON STANDARD OUTPUT UNIT                           
        IWRITE=I1MACH(2)                                                
        WRITE(IWRITE,10)(X(I),I=1,N)                                    
 10     FORMAT(10H SOLUTION-,5E15.5)                                    
        STOP                                                            
        END                                                             
        SUBROUTINE ROSN(N,X,NF,F)                                       
C THIS SUBROUTINE COMPUTES THE  FUNCTION                                
        INTEGER N, NF                                                   
        REAL X(N), F                                                    
        F=100.0*(X(2)-X(1)*X(1))**2 + (1.0 - X(1))**2                   
        RETURN                                                          
        END                                                             
C$TEST NTLF                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SMNG                              
C                                                                       
C***********************************************************************
        INTEGER N                                                       
        EXTERNAL ROSN,ROSG                                              
        REAL X(2)                                                       
        N=2                                                             
C INITIALIZE X                                                          
        X(1)=-1.2                                                       
        X(2)=1.0                                                        
C                                                                       
C SOLVE THE PROBLEM                                                     
C                                                                       
        CALL SMNG(N, X, ROSN, ROSG, 100, 1.E-4)                         
C       PRINT RESULTS ON STANDARD OUTPUT UNIT                           
        IWRITE=I1MACH(2)                                                
        WRITE(IWRITE,10)(X(I),I=1,N)                                    
 10     FORMAT(10H SOLUTION-,5E15.5)                                    
        STOP                                                            
        END                                                             
        SUBROUTINE ROSN(N,X,NF,F)                                       
C THIS SUBROUTINE COMPUTES THE  FUNCTION                                
        INTEGER N, NF                                                   
        REAL X(N), F                                                    
        F=100.0*(X(2)-X(1)*X(1))**2 + (1.0 - X(1))**2                   
        RETURN                                                          
        END                                                             
        SUBROUTINE ROSG(N,X,NF,G)                                       
C THIS SUBROUTINE COMPUTES THE GRADIENT                                 
        INTEGER N,NF                                                    
        REAL X(N), G(N)                                                 
        G(1)=200.0*(X(2)-X(1)*X(1))*(-2.0)*X(1) - 2.0*(1-X(1))          
        G(2)=200.0*(X(2)-X(1)*X(1))                                     
        RETURN                                                          
        END                                                             
C$TEST NTLH                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SMNGB                             
C                                                                       
C***********************************************************************
        INTEGER N                                                       
        EXTERNAL ROSN, ROSG                                             
        REAL X(2), B(2,2)                                               
        N=2                                                             
C INITIALIZE X                                                          
        X(1)=-1.2                                                       
        X(2)=1.0                                                        
C SET UP THE BOUND ARRAY                                                
C R1MACH(2) CONTAINS THE LARGEST NUMBER IN THE MACHINE                  
        B(1,1)=-R1MACH(2)                                               
        B(2,1)=0.5                                                      
        B(1,2)=0.0                                                      
        B(2,2)=1.0                                                      
C                                                                       
C SOLVE THE PROBLEM                                                     
C                                                                       
        CALL SMNGB(N, X, B, ROSN, ROSG, 100, 1.E-4)                     
C       PRINT RESULTS ON STANDARD OUTPUT UNIT                           
        IWRITE=I1MACH(2)                                                
        WRITE(IWRITE,10)(X(I),I=1,N)                                    
 10     FORMAT(10H SOLUTION-,5E15.5)                                    
        STOP                                                            
        END                                                             
        SUBROUTINE ROSN(N,X,NF,F)                                       
C THIS SUBROUTINE COMPUTES THE  FUNCTION                                
        INTEGER N, NF                                                   
        REAL X(N), F                                                    
        F=100.0*(X(2)-X(1)*X(1))**2 + (1.0 - X(1))**2                   
        RETURN                                                          
        END                                                             
        SUBROUTINE ROSG(N,X,NF,G)                                       
C THIS SUBROUTINE COMPUTES THE GRADIENT                                 
        INTEGER N,NF                                                    
        REAL X(N), G(N)                                                 
        G(1)=200.0*(X(2)-X(1)*X(1))*(-2.0)*X(1) - 2.0*(1-X(1))          
        G(2)=200.0*(X(2)-X(1)*X(1))                                     
        RETURN                                                          
        END                                                             
C$TEST NTLK                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SMNH                              
C                                                                       
C***********************************************************************
        INTEGER N                                                       
        EXTERNAL ROSN,ROSGH                                             
        REAL X(2)                                                       
        N=2                                                             
C INITIALIZE X                                                          
        X(1)=-1.2                                                       
        X(2)=1.0                                                        
C                                                                       
C SOLVE THE PROBLEM                                                     
C                                                                       
        CALL SMNH(N, X, ROSN, ROSGH, 100, 1.E-4)                        
C       PRINT RESULTS ON STANDARD OUTPUT UNIT                           
        IWRITE=I1MACH(2)                                                
        WRITE(IWRITE,10)(X(I),I=1,N)                                    
 10     FORMAT(10H SOLUTION-,5E15.5)                                    
        STOP                                                            
        END                                                             
        SUBROUTINE ROSN(N,X,NF,F)                                       
C THIS SUBROUTINE COMPUTES THE  FUNCTION                                
        INTEGER N, NF                                                   
        REAL X(N), F                                                    
        F=100.0*(X(2)-X(1)*X(1))**2 + (1.0 - X(1))**2                   
        RETURN                                                          
        END                                                             
        SUBROUTINE ROSGH(N,X,NF,G,H)                                    
C THIS SUBROUTINE COMPUTES THE GRADIENT AND THE HESSIAN                 
        INTEGER N,NF                                                    
        REAL X(N), G(N), H(1)                                           
        G(1)=200.0*(X(2)-X(1)*X(1))*(-2.0)*X(1) - 2.0*(1-X(1))          
        G(2)=200.0*(X(2)-X(1)*X(1))                                     
C H(1) HAS THE (1,1) ELEMENT, H(2) HAS THE (2,1) ELEMENT,               
C H(3) HAS THE (2,2) ELEMENT OF THE MATRIX OF SECOND PARTIALS           
        H(1)=200.0*(X(2)-X(1)*X(1))*(-2.0)+800.0*X(1)*X(1)+2.0          
        H(2)=-400.0*X(1)                                                
        H(3)=200.0                                                      
        RETURN                                                          
        END                                                             
C$TEST NTLM                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SMNHB                             
C                                                                       
C***********************************************************************
        INTEGER N                                                       
        EXTERNAL ROSN, ROSGH                                            
        REAL X(2), B(2,2)                                               
        N=2                                                             
C INITIALIZE X                                                          
        X(1)=-1.2                                                       
        X(2)=1.0                                                        
C SET UP THE BOUND ARRAY                                                
C R1MACH(2) CONTAINS THE LARGEST NUMBER IN THE MACHINE                  
        B(1,1)=-R1MACH(2)                                               
        B(2,1)=0.5                                                      
        B(1,2)=0.0                                                      
        B(2,2)=1.0                                                      
C                                                                       
C SOLVE THE PROBLEM                                                     
C                                                                       
        CALL SMNHB(N, X, B, ROSN, ROSGH, 100, 1.E-4)                    
C       PRINT RESULTS ON STANDARD OUTPUT UNIT                           
        IWRITE=I1MACH(2)                                                
        WRITE(IWRITE,10)(X(I),I=1,N)                                    
 10     FORMAT(10H SOLUTION-,5E15.5)                                    
        STOP                                                            
        END                                                             
        SUBROUTINE ROSN(N,X,NF,F)                                       
C THIS SUBROUTINE COMPUTES THE  FUNCTION                                
        INTEGER N, NF                                                   
        REAL X(N), F                                                    
        F=100.0*(X(2)-X(1)*X(1))**2 + (1.0 - X(1))**2                   
        RETURN                                                          
        END                                                             
        SUBROUTINE ROSGH(N,X,NF,G,H)                                    
C THIS SUBROUTINE COMPUTES THE GRADIENT AND THE HESSIAN                 
        INTEGER N,NF                                                    
        REAL X(N), G(N), H(1)                                           
        G(1)=200.0*(X(2)-X(1)*X(1))*(-2.0)*X(1) - 2.0*(1-X(1))          
        G(2)=200.0*(X(2)-X(1)*X(1))                                     
C H(1) HAS THE (1,1) ELEMENT, H(2) HAS THE (2,1) ELEMENT,               
C H(3) HAS THE (2,2) ELEMENT OF THE MATRIX OF SECOND PARTIALS           
        H(1)=200.0*(X(2)-X(1)*X(1))*(-2.0)+800.0*X(1)*X(1)+2.0          
        H(2)=-400.0*X(1)                                                
        H(3)=200.0                                                      
        RETURN                                                          
        END                                                             
C$TEST NTLP                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SNSF                              
C                                                                       
C***********************************************************************
        INTEGER N,P,L                                                   
        INTEGER LP1, IINC,INC(4,2)                                      
        EXTERNAL OSBA                                                   
        REAL Y(10),T(10),X(2),C(3)                                      
        COMMON /TT/T                                                    
C GENERATE DATA FOR PROBLEM                                             
        DATA Y(1)/8.44E-1/, Y(2) /9.36E-1/, Y(3) /8.81E-1/              
     1  Y(4)/7.84E-1/, Y(5)/ 6.85E-1/, Y(6)/6.03E-1/,                   
     2  Y(7) /5.38E-1/ , Y(8) /4.90E-1/, Y(9)/4.57E-1/                  
        P=2                                                             
        N=9                                                             
        L=3                                                             
        DO 10 I=1,9                                                     
           T(I)=-30.E0*FLOAT(I-1)                                       
 10     CONTINUE                                                        
C INITIALIZE X                                                          
        X(1)=.01                                                        
        X(2)=.02                                                        
C GENERATE THE INCIDENCE MATRIX                                         
        LP1=L+1                                                         
        DO 30 J=1,P                                                     
           DO 20 I=1,LP1                                                
              INC(I,J)=0                                                
 20        CONTINUE                                                     
 30     CONTINUE                                                        
        INC(2,1)=1                                                      
        INC(3,2)=1                                                      
        IINC=LP1                                                        
C                                                                       
C SOLVE THE PROBLEM                                                     
C                                                                       
        CALL SNSF(N, P, L, X, C, Y, OSBA, INC, IINC, 100, 1.E-4)        
C       PRINT RESULTS ON STANDARD OUTPUT UNIT                           
        IWRITE = I1MACH(2)                                              
        WRITE(IWRITE, 40)(X(I),I=1,P)                                   
 40     FORMAT(22H NONLINEAR PARAMETERS-,2E15.5)                        
        WRITE(IWRITE, 50)(C(I),I=1,L)                                   
 50     FORMAT(19H LINEAR PARAMETERS-, 3E15.5)                          
        STOP                                                            
        END                                                             
        SUBROUTINE OSBA(N,P,L,X,NF,A)                                   
C THIS SUBROUTINE COMPUTES THE MODEL                                    
        INTEGER P, N, NF, L                                             
        REAL X(P), A(N,L)                                               
        REAL  T(10)                                                     
        COMMON /TT/  T                                                  
        DO 10 I=1,N                                                     
           A(I,1)=1.0                                                   
           A(I,2)=EXP(X(1)*T(I))                                        
           A(I,3)=EXP(X(2)*T(I))                                        
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
C$TEST NTLR                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SNSG                              
C                                                                       
C***********************************************************************
        INTEGER N,P,L                                                   
        INTEGER LP1, IINC,INC(4,2)                                      
        EXTERNAL OSBA, OSBB                                             
        REAL Y(10),T(10),X(2),C(3)                                      
        COMMON /TT/T                                                    
C GENERATE DATA FOR PROBLEM                                             
        DATA Y(1)/8.44E-1/, Y(2) /9.36E-1/, Y(3) /8.81E-1/              
     1  Y(4)/7.84E-1/, Y(5)/ 6.85E-1/, Y(6)/6.03E-1/,                   
     2  Y(7) /5.38E-1/ , Y(8) /4.90E-1/, Y(9)/4.57E-1/                  
        P=2                                                             
        N=9                                                             
        L=3                                                             
        DO 10 I=1,9                                                     
           T(I)=-30.E0*FLOAT(I-1)                                       
 10     CONTINUE                                                        
C INITIALIZE X                                                          
        X(1)=.01                                                        
        X(2)=.02                                                        
C GENERATE THE INCIDENCE MATRIX                                         
        LP1=L+1                                                         
        DO 30 J=1,P                                                     
           DO 20 I=1,LP1                                                
              INC(I,J)=0                                                
 20        CONTINUE                                                     
 30     CONTINUE                                                        
        INC(2,1)=1                                                      
        INC(3,2)=1                                                      
        IINC=LP1                                                        
C                                                                       
C SOLVE THE PROBLEM                                                     
C                                                                       
        CALL SNSG(N, P, L, X, C, Y, OSBA, OSBB, INC, IINC, 100, 1.E-4)  
C       PRINT RESULTS ON STANDARD OUTPUT UNIT                           
        IWRITE = I1MACH(2)                                              
        WRITE(IWRITE, 40)(X(I),I=1,P)                                   
 40     FORMAT(22H NONLINEAR PARAMETERS-,2E15.5)                        
        WRITE(IWRITE, 50)(C(I),I=1,L)                                   
 50     FORMAT(19H LINEAR PARAMETERS-, 3E15.5)                          
        STOP                                                            
        END                                                             
        SUBROUTINE OSBA(N,P,L,X,NF,A)                                   
C THIS SUBROUTINE COMPUTES THE MODEL                                    
        INTEGER P, N, NF, L                                             
        REAL X(P), A(N,L)                                               
        REAL  T(10)                                                     
        COMMON /TT/  T                                                  
        DO 10 I=1,N                                                     
           A(I,1)=1.0                                                   
           A(I,2)=EXP(X(1)*T(I))                                        
           A(I,3)=EXP(X(2)*T(I))                                        
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
       SUBROUTINE OSBB(N,P,L,X,NF,B)                                    
C THIS SUBROUTINE COMPUTES THE NONZERO DERIVATIVES OF B                 
       INTEGER N,P,L,NF                                                 
       REAL X(P), B(N,L)                                                
       REAL T(10)                                                       
       COMMON /TT/ T                                                    
       DO 10 I=1,N                                                      
          B(I,1)=T(I)*EXP(T(I)*X(1))                                    
          B(I,2)=T(I)*EXP(T(I)*X(2))                                    
 10    CONTINUE                                                         
       RETURN                                                           
       END                                                              
C$TEST NTLT                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SNSFB                             
C                                                                       
C***********************************************************************
        INTEGER N,P,L                                                   
        INTEGER LP1, IINC,INC(4,2)                                      
        EXTERNAL OSBA                                                   
        REAL Y(10),T(10),X(2),C(3),B(2,2)                               
        COMMON /TT/T                                                    
C GENERATE DATA FOR PROBLEM                                             
        DATA Y(1)/8.44E-1/, Y(2) /9.36E-1/, Y(3) /8.81E-1/              
     1  Y(4)/7.84E-1/, Y(5)/ 6.85E-1/, Y(6)/6.03E-1/,                   
     2  Y(7) /5.38E-1/ , Y(8) /4.90E-1/, Y(9)/4.57E-1/                  
        P=2                                                             
        N=9                                                             
        L=3                                                             
        DO 10 I=1,9                                                     
           T(I)=-30.E0*FLOAT(I-1)                                       
 10     CONTINUE                                                        
C INITIALIZE X                                                          
        X(1)=.01                                                        
        X(2)=.03                                                        
C GENERATE THE INCIDENCE MATRIX                                         
        LP1=L+1                                                         
        DO 30 J=1,P                                                     
           DO 20 I=1,LP1                                                
              INC(I,J)=0                                                
 20        CONTINUE                                                     
 30     CONTINUE                                                        
        INC(2,1)=1                                                      
        INC(3,2)=1                                                      
        IINC=LP1                                                        
C SUPPLY BOUNDS                                                         
        B(1,1)=-R1MACH(2)                                               
        B(2,1)=0.125                                                    
        B(1,2)=.03                                                      
        B(2,2)=R1MACH(2)                                                
C                                                                       
C SOLVE THE PROBLEM                                                     
C                                                                       
        CALL SNSFB(N, P, L, X, B, C, Y, OSBA, INC, IINC, 100, 1.E-4)    
C       PRINT RESULTS ON STANDARD OUTPUT UNIT                           
        IWRITE = I1MACH(2)                                              
        WRITE(IWRITE, 40)(X(I),I=1,P)                                   
 40     FORMAT(22H NONLINEAR PARAMETERS-,2E15.5)                        
        WRITE(IWRITE, 50)(C(I),I=1,L)                                   
 50     FORMAT(19H LINEAR PARAMETERS-, 3E15.5)                          
        STOP                                                            
        END                                                             
        SUBROUTINE OSBA(N,P,L,X,NF,A)                                   
C THIS SUBROUTINE COMPUTES THE MODEL                                    
        INTEGER P, N, NF, L                                             
        REAL X(P), A(N,L)                                               
        REAL  T(10)                                                     
        COMMON /TT/  T                                                  
        DO 10 I=1,N                                                     
           A(I,1)=1.0                                                   
           A(I,2)=EXP(X(1)*T(I))                                        
           A(I,3)=EXP(X(2)*T(I))                                        
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
C$TEST NTLU                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SNSGB                             
C                                                                       
C***********************************************************************
        INTEGER N,P,L                                                   
        INTEGER LP1, IINC,INC(4,2)                                      
        EXTERNAL OSBA, OSBB                                             
        REAL Y(10),T(10),X(2),C(3),B(2,2)                               
        COMMON /TT/T                                                    
C GENERATE DATA FOR PROBLEM                                             
        DATA Y(1)/8.44E-1/, Y(2) /9.36E-1/, Y(3) /8.81E-1/              
     1  Y(4)/7.84E-1/, Y(5)/ 6.85E-1/, Y(6)/6.03E-1/,                   
     2  Y(7) /5.38E-1/ , Y(8) /4.90E-1/, Y(9)/4.57E-1/                  
        P=2                                                             
        N=9                                                             
        L=3                                                             
        DO 10 I=1,9                                                     
           T(I)=-30.E0*FLOAT(I-1)                                       
 10     CONTINUE                                                        
C INITIALIZE X                                                          
        X(1)=.01                                                        
        X(2)=.03                                                        
C GENERATE THE INCIDENCE MATRIX                                         
        LP1=L+1                                                         
        DO 30 J=1,P                                                     
           DO 20 I=1,LP1                                                
              INC(I,J)=0                                                
 20        CONTINUE                                                     
 30     CONTINUE                                                        
        INC(2,1)=1                                                      
        INC(3,2)=1                                                      
        IINC=LP1                                                        
C SPECIFY BOUNDS                                                        
        B(1,1)=-R1MACH(2)                                               
        B(2,1)=0.125                                                    
        B(1,2)=.03                                                      
        B(2,2)=R1MACH(2)                                                
C                                                                       
C SOLVE THE PROBLEM                                                     
C                                                                       
        CALL SNSGB(N,P,L,X,B,C,Y,OSBA,OSBB,INC,IINC,100,1.E-4)          
C       PRINT RESULTS ON STANDARD OUTPUT UNIT                           
        IWRITE = I1MACH(2)                                              
        WRITE(IWRITE, 40)(X(I),I=1,P)                                   
 40     FORMAT(22H NONLINEAR PARAMETERS-,2E15.5)                        
        WRITE(IWRITE, 50)(C(I),I=1,L)                                   
 50     FORMAT(19H LINEAR PARAMETERS-, 3E15.5)                          
        STOP                                                            
        END                                                             
        SUBROUTINE OSBA(N,P,L,X,NF,A)                                   
C THIS SUBROUTINE COMPUTES THE MODEL                                    
        INTEGER P, N, NF, L                                             
        REAL X(P), A(N,L)                                               
        REAL  T(10)                                                     
        COMMON /TT/  T                                                  
        DO 10 I=1,N                                                     
           A(I,1)=1.0                                                   
           A(I,2)=EXP(X(1)*T(I))                                        
           A(I,3)=EXP(X(2)*T(I))                                        
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
       SUBROUTINE OSBB(N,P,L,X,NF,B)                                    
C THIS SUBROUTINE COMPUTES THE NONZERO DERIVATIVES OF B                 
       INTEGER N,P,L,NF                                                 
       REAL X(P), B(N,L)                                                
       REAL T(10)                                                       
       COMMON /TT/ T                                                    
       DO 10 I=1,N                                                      
          B(I,1)=T(I)*EXP(T(I)*X(1))                                    
          B(I,2)=T(I)*EXP(T(I)*X(2))                                    
 10    CONTINUE                                                         
       RETURN                                                           
       END                                                              
C$TEST NLSA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAMS MNF, MNG AND MNH                 
C                                                                       
C***********************************************************************
C *** MNF, MNG, MNH EXAMPLE PROGRAM                                     
C                                                                       
C *** MINIMIZE F(X) = 0.1*S(X)**4 + SUM(I = 1(1)3) (I * (X(I) - 10)**2),
C *** WHERE S(X) = SUM(I = 1(1)3) X(I),                                 
C *** STARTING FROM     X = (2, 30, 9),                                 
C *** WITH SCALE VECTOR D = (1, 2, 3).                                  
C                                                                       
      INTEGER LIV, LV                                                   
      INTEGER IV(59), P, UI(1)                                          
      REAL D(3), UR(1), V(123), X(3)                                    
      EXTERNAL DUMMY, QF, QGH                                           
C                                                                       
      DATA LIV/59/, LV/123/, P/3/                                       
C                                                                       
      DATA X(1)/2.E+0/, X(2)/3.E+1/, X(3)/9.E+0/                        
      DATA D(1)/1.E+0/, D(2)/2.E+0/, D(3)/3.E+0/                        
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
C  ***  SET IV(1) TO 0 TO USE ALL DEFAULT INPUTS...                     
C                                                                       
      IV(1) = 0                                                         
C                                                                       
C ... HINDSIGHT (THE PRINTED OUTPUT FROM THIS EXAMPLE) SUGGESTS THAT    
C ... THE ALGORITHM MIGHT TAKE FEWER FUNCTION EVALUATIONS ON THIS       
C ... PROBLEM IF THE INITIAL STEP BOUND, V(LMAX0), WERE INCREASED       
C ... FROM ITS DEFAULT VALUE OF 1.0 TO 10.0 .  WE WOULD DO THIS BY      
C ... REPLACING THE ABOVE ASSIGNMENT OF 0 TO IV(1) WITH THE TWO LINES...
C                                                                       
C     CALL IVSET(2, IV, LIV, LV, V)                                     
C     V(35) = 10.0                                                      
C                                                                       
C                                                                       
C *** SOLVE THE PROBLEM -- MNH WILL PRINT THE SOLUTION FOR US...        
C                                                                       
      CALL MNH(P, D, X, QF, QGH, IV, LIV, LV, V, UI, UR, DUMMY)         
C                                                                       
C *** FOR MNF AND MNG, THE CORRESPONDING CALLS WOULD BE...              
C                                                                       
C     CALL MNF(P, D, X, QF, IV, LIV, LV, V, UI, UR, DUMMY)              
C     CALL MNG(P, D, X, QF, QG, IV, LIV, LV, V, UI, UR, DUMMY)          
C                                                                       
C *** QG WOULD BE A SUBROUTINE, DECLARED EXTERNAL IN PLACE OF QGH ABOVE,
C *** THAT WOULD BE THE SAME AS QGH (SEE BELOW) EXCEPT FOR HAVING       
C *** THE PARAMETER  H  OMITTED.                                        
C                                                                       
C *** NOTE -- ON MOST SYSTEMS, WE COULD SIMPLY PASS QF OR QGH           
C *** AS THE LAST PARAMETER TO MNH, SINCE QF AND QGH IGNORE             
C *** THEIR UF PARAMETER.  BUT THERE EXIST SYSTEMS (E.G. UNIVAC)        
C *** THAT WOULD GIVE A RUN-TIME ERROR IF WE DID THIS.  HENCE WE        
C *** PASS THE IMMEDIATELY FOLLOWING DUMMY SUBROUTINE AS UF.            
C                                                                       
 999  STOP                                                              
      END                                                               
      SUBROUTINE DUMMY                                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE QF(P, X, NF, F, UI, UR, UF)                            
C                                                                       
C *** THIS ROUTINE COMPUTES THE OBJECTIVE FUNCTION, F(X)                
C                                                                       
      INTEGER P, NF, UI(1)                                              
      REAL X(P), F, UR(1)                                               
      EXTERNAL UF                                                       
C                                                                       
      INTEGER I                                                         
      REAL PT1, TEN, ZERO                                               
C                                                                       
      DATA PT1 /0.1E+0/, TEN/1.E+1/, ZERO/0.E+0/                        
C                                                                       
C                                                                       
      F = ZERO                                                          
      DO 10 I = 1, P                                                    
 10      F = F + X(I)                                                   
      F = PT1 * F**4                                                    
      DO 20 I = 1, P                                                    
 20      F = F + I*(X(I) - TEN)**2                                      
 999  RETURN                                                            
      END                                                               
      SUBROUTINE QGH(P, X, NF, G, H, UI, UR, UF)                        
C                                                                       
C *** THIS ROUTINE COMPUTES THE GRADIENT, G(X), AND THE LOWER TRIANGLE  
C *** OF THE HESSIAN, H(X).                                             
C                                                                       
      INTEGER P, NF, UI(1)                                              
      REAL X(P), G(P), H(1), UR(1)                                      
      EXTERNAL UF                                                       
C                                                                       
      INTEGER I, K                                                      
      REAL S, S34                                                       
      REAL ONEPT2, PT4, TEN, TWO, ZERO                                  
C                                                                       
      DATA ONEPT2/1.2E+0/,PT4/0.4E+0/,TEN/1.E+1/,TWO/2.E+0/,ZERO/0.E+0/ 
C                                                                       
C                                                                       
      S = ZERO                                                          
      DO 10 I = 1, P                                                    
 10      S = S + X(I)                                                   
C                                                                       
C     *** INITIALIZE H TO 1.2*S**2 ***                                  
C                                                                       
      CALL SETR(P*(P+1)/2, ONEPT2*S**2, H)                              
C                                                                       
C     *** NOW COMPUTE G AND ADD (2, 4, ..., 2*P) TO THE DIAGONAL OF H   
C                                                                       
      S34 = PT4 * S**3                                                  
      K = 0                                                             
      DO 20 I = 1, P                                                    
         G(I) = S34  +  TWO * I * (X(I) - TEN)                          
         K = K + I                                                      
         H(K) = H(K) + TWO*I                                            
 20      CONTINUE                                                       
 999  RETURN                                                            
      END                                                               
C$TEST NLSB                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAMS MNFB, MNGB, AND MNHB             
C                                                                       
C***********************************************************************
C *** MNFB, MNGB, MNHB EXAMPLE PROGRAM                                  
C                                                                       
C *** MINIMIZE F(X) = 0.1*S(X)**4 + SUM(I = 1(1)3) (I * (X(I) - 10)**2),
C *** WHERE S(X) = SUM(I = 1(1)3) X(I)                                  
C *** SUBJECT TO                                                        
C ***    1 .LE. X(1) .LE. 3,                                            
C ***   -2 .LE. X(2) .LE. 10,                                           
C ***    1 .LE. X(3) .LE. 21,                                           
C *** STARTING FROM     X = (2, 30, 9),                                 
C *** WITH SCALE VECTOR D = (1, 2, 3)                                   
C                                                                       
      INTEGER LIV, LV                                                   
      INTEGER IV(68), P, UI(1)                                          
      REAL B(2,3), D(3), UR(1), V(132), X(3)                            
      EXTERNAL DUMMY, QF, QGH                                           
C                                                                       
      DATA LIV/68/, LV/132/, P/3/                                       
C                                                                       
      DATA B(1,1)/1.E+0/,  B(2,1)/3.E+0/,                               
     1     B(1,2)/-2.E+0/, B(2,2)/1.E+1/,                               
     2     B(1,3)/1.E+0/,  B(2,3)/2.1E+1/                               
C                                                                       
      DATA X(1)/2.E+0/, X(2)/3.E+1/, X(3)/9.E+0/                        
      DATA D(1)/1.E+0/, D(2)/2.E+0/, D(3)/3.E+0/                        
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
C  ***  SET IV(1) TO 0 TO USE ALL DEFAULT INPUTS...                     
C                                                                       
      IV(1) = 0                                                         
C                                                                       
C ... WE COULD HAVE MNHB INITIALIZE THE SCALE VECTOR D TO ALL ONES      
C ... BY SETTING V(DINIT) TO 1.0 .  WE WOULD DO THIS BY REPLACING       
C ... THE ABOVE ASSIGNMENT OF 0 TO IV(1) WITH THE FOLLOWING TWO LINES...
C                                                                       
C     CALL IVSET(2, IV, LIV, LV, V)                                     
C     V(38) = 1.0                                                       
C                                                                       
C                                                                       
C *** SOLVE THE PROBLEM -- MNHB WILL PRINT THE SOLUTION FOR US...       
C                                                                       
      CALL MNHB(P, D, X, B, QF, QGH, IV, LIV, LV, V, UI, UR, DUMMY)     
C                                                                       
C *** FOR MNFB AND MNGB, THE CORRESPONDING CALLS WOULD BE...            
C                                                                       
C     CALL MNFB(P, D, X, B, QF, IV, LIV, LV, V, UI, UR, DUMMY)          
C     CALL MNGB(P, D, X, B, QF, QG, IV, LIV, LV, V, UI, UR, DUMMY)      
C                                                                       
C *** QG WOULD BE A SUBROUTINE, DECLARED EXTERNAL IN PLACE OF QGH ABOVE,
C *** THAT WOULD BE THE SAME AS QGH (SEE BELOW) EXCEPT FOR HAVING       
C *** THE PARAMETER  H  OMITTED.                                        
C                                                                       
C *** NOTE -- ON MOST SYSTEMS, WE COULD SIMPLY PASS QF OR QGH           
C *** AS THE LAST PARAMETER TO MNHB, SINCE QF AND QGH IGNORE            
C *** THEIR UF PARAMETER.  BUT THERE EXIST SYSTEMS (E.G. UNIVAC)        
C *** THAT WOULD GIVE A RUN-TIME ERROR IF WE DID THIS.  HENCE WE        
C *** PASS THE IMMEDIATELY FOLLOWING DUMMY SUBROUTINE AS UF.            
C                                                                       
 999  STOP                                                              
      END                                                               
      SUBROUTINE DUMMY                                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE QF(P, X, NF, F, UI, UR, UF)                            
C                                                                       
C *** THIS ROUTINE COMPUTES THE OBJECTIVE FUNCTION, F(X)                
C                                                                       
      INTEGER P, NF, UI(1)                                              
      REAL X(P), F, UR(1)                                               
      EXTERNAL UF                                                       
C                                                                       
      INTEGER I                                                         
      REAL PT1, TEN, ZERO                                               
C                                                                       
      DATA PT1 /0.1E+0/, TEN/1.E+1/, ZERO/0.E+0/                        
C                                                                       
C                                                                       
      F = ZERO                                                          
      DO 10 I = 1, P                                                    
 10      F = F + X(I)                                                   
      F = PT1 * F**4                                                    
      DO 20 I = 1, P                                                    
 20      F = F + I*(X(I) - TEN)**2                                      
 999  RETURN                                                            
      END                                                               
      SUBROUTINE QGH(P, X, NF, G, H, UI, UR, UF)                        
C                                                                       
C *** THIS ROUTINE COMPUTES THE GRADIENT, G(X), AND THE LOWER TRIANGLE  
C *** OF THE HESSIAN, H(X).                                             
C                                                                       
      INTEGER P, NF, UI(1)                                              
      REAL X(P), G(P), H(1), UR(1)                                      
      EXTERNAL UF                                                       
C                                                                       
      INTEGER I, K                                                      
      REAL S, S34                                                       
      REAL ONEPT2, PT4, TEN, TWO, ZERO                                  
C                                                                       
      DATA ONEPT2/1.2E+0/,PT4/0.4E+0/,TEN/1.E+1/,TWO/2.E+0/,ZERO/0.E+0/ 
C                                                                       
C                                                                       
      S = ZERO                                                          
      DO 10 I = 1, P                                                    
 10      S = S + X(I)                                                   
C                                                                       
C     *** INITIALIZE H TO 1.2*S**2 ***                                  
C                                                                       
      CALL SETR(P*(P+1)/2, ONEPT2*S**2, H)                              
C                                                                       
C     *** NOW COMPUTE G AND ADD (2, 4, ..., 2*P) TO THE DIAGONAL OF H   
C                                                                       
      S34 = PT4 * S**3                                                  
      K = 0                                                             
      DO 20 I = 1, P                                                    
         G(I) = S34  +  TWO * I * (X(I) - TEN)                          
         K = K + I                                                      
         H(K) = H(K) + TWO*I                                            
 20      CONTINUE                                                       
 999  RETURN                                                            
      END                                                               
C$TEST NLSJ                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAMS N2F AND N2G                      
C                                                                       
C***********************************************************************
C *** N2F AND N2G, EXAMPLE PROGRAM ***                                  
C                                                                       
C *** FIT N = 33 DATA POINTS (T,Y) TO THE CURVE                         
C *** X(1) + X(2)*EXP(T*X(4)) + X(3)*EXP(T*X(5))                        
C                                                                       
C *** THE FOLLOWING CODE IS FOR CALLING N2G.  DIFFERENCES FOR           
C *** CALLING N2F ARE EXPLAINED IN COMMENTS.                            
C                                                                       
      INTEGER I, IV(87), LIV, LTY, LV, UI(1)                            
      REAL TY(50,2), V(471), X(5)                                       
      EXTERNAL DUMMY, OSB1J, OSB1R                                      
      DATA LIV/87/, LTY/50/, LV/471/                                    
C                                                                       
C *** FOR N2F, OMIT OSB1J FROM THE EXTERNAL STATEMENT.                  
C                                                                       
C                                                                       
C *** TO MAKE THIS EXAMPLE SELF-CONTAINED, WE USE A DATA STATEMENT      
C *** AND DO LOOP TO SUPPLY (T,Y) PAIRS TO THE ARRAY TY.                
C                                                                       
C *** Y VALUES...                                                       
C                                                                       
      DATA TY(1,2) /8.44E-1/, TY(2,2) /9.08E-1/, TY(3,2)/9.32E-1/,      
     1     TY(4,2) /9.36E-1/, TY(5,2) /9.25E-1/, TY(6,2)/9.08E-1/,      
     2     TY(7,2) /8.81E-1/, TY(8,2) /8.50E-1/, TY(9,2)/8.18E-1/,      
     3     TY(10,2)/7.84E-1/, TY(11,2)/7.51E-1/, TY(12,2)/7.18E-1/,     
     4     TY(13,2)/6.85E-1/, TY(14,2)/6.58E-1/, TY(15,2)/6.28E-1/,     
     5     TY(16,2)/6.03E-1/, TY(17,2)/5.80E-1/, TY(18,2)/5.58E-1/,     
     6     TY(19,2)/5.38E-1/, TY(20,2)/5.22E-1/, TY(21,2)/5.06E-1/,     
     7     TY(22,2)/4.90E-1/, TY(23,2)/4.78E-1/, TY(24,2)/4.67E-1/,     
     8     TY(25,2)/4.57E-1/, TY(26,2)/4.48E-1/, TY(27,2)/4.38E-1/,     
     9     TY(28,2)/4.31E-1/, TY(29,2)/4.24E-1/, TY(30,2)/4.20E-1/,     
     A     TY(31,2)/4.14E-1/, TY(32,2)/4.11E-1/, TY(33,2)/4.06E-1/      
C                                                                       
C ***  T VALUES...                                                      
C                                                                       
      DO 10 I = 1, 33                                                   
         TY(I,1) = -10.E+0 * FLOAT(I-1)                                 
 10      CONTINUE                                                       
C                                                                       
C *** SUPPLY LEAD DIMENSION OF TY IN UI(1)...                           
C *** (MOST COMPILERS WOULD LET US SIMPLY PASS LTY FOR UI,              
C *** BUT SOME, E.G. WATFIV, WILL NOT.)                                 
C                                                                       
      UI(1) = LTY                                                       
C                                                                       
C *** SPECIFY ALL DEFAULT IV AND V INPUT COMPONENTS (N2G AND N2F        
C *** ONLY)...                                                          
C                                                                       
      IV(1) = 0                                                         
C                                                                       
C *** SUPPLY INITIAL GUESS...                                           
C                                                                       
      X(1) = 0.5E+0                                                     
      X(2) = 1.5E+0                                                     
      X(3) = -1.E+0                                                     
      X(4) = 1.E-2                                                      
      X(5) = 2.E-2                                                      
C                                                                       
C *** SOLVE THE PROBLEM -- N2G WILL PRINT THE SOLUTION FOR US...        
C                                                                       
      CALL N2G(33, 5, X, OSB1R, OSB1J, IV, LIV, LV, V, UI, TY, DUMMY)   
C                                                                       
C *** FOR N2F, THE CORRESPONDING CALLS WOULD BE...                      
C                                                                       
C     CALL N2F(33, 5, X, OSB1R, IV, LIV, LV, V, UI, TY, DUMMY)          
C                                                                       
C                                                                       
C *** NOTE -- ON MOST SYSTEMS, WE COULD SIMPLY PASS OSB1R (OR OSB1J)    
C *** AS THE UF PARAMETER, SINCE OSB1R AND OSB1J IGNORE THIS            
C *** PARAMETER.  BUT THERE EXIST SYSTEMS (E.G. UNIVAC) THAT WOULD      
C *** GIVE A RUN-TIME ERROR IF WE DID THIS.  HENCE WE PASS THE          
C *** IMMEDIATELY FOLLOWING DUMMY SUBROUTINE AS UF.                     
C                                                                       
      STOP                                                              
      END                                                               
      SUBROUTINE DUMMY                                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE OSB1R(N, P, X, NF, R, LTY, TY, UF)                     
C                                                                       
C *** THIS ROUTINE COMPUTES THE RESIDUAL VECTOR, R = R(X),              
C *** FOR TEST PROBLEM OSBORNE1.                                        
C                                                                       
      INTEGER N, P, NF, LTY                                             
      REAL X(P), R(N), TY(LTY,2)                                        
      EXTERNAL UF                                                       
C                                                                       
      INTEGER I                                                         
      REAL TI, YI                                                       
C                                                                       
      DO 10 I = 1, N                                                    
         TI = TY(I,1)                                                   
         YI = TY(I,2)                                                   
         R(I) = YI - (X(1) + X(2)* EXP(X(4)*TI) + X(3)* EXP(X(5)*TI))   
 10      CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE OSB1J(N, P, X, NF, J, LTY, TY, UF)                     
C                                                                       
C *** THIS ROUTINE COMPUTES THE JACOBIAN MATRIX, J = J(X),              
C *** FOR TEST PROBLEM OSBORNE1.  J(I,K) IS SET TO THE PARTIAL          
C *** DERIVATIVE OF COMPONENT I OF R WITH RESPECT TO X(K).              
C                                                                       
      INTEGER N, P, NF, LTY                                             
      REAL X(P), J(N,P), TY(LTY,2)                                      
      EXTERNAL UF                                                       
C                                                                       
      INTEGER I                                                         
      REAL NEGONE, TI                                                   
      DATA NEGONE/-1.E+0/                                               
C                                                                       
      DO 10 I = 1, N                                                    
         TI = TY(I,1)                                                   
         J(I,1) = NEGONE                                                
         J(I,2) = - EXP(X(4)*TI)                                        
         J(I,3) = - EXP(X(5)*TI)                                        
         J(I,4) = TI*X(2)*J(I,2)                                        
         J(I,5) = TI*X(3)*J(I,3)                                        
 10      CONTINUE                                                       
      RETURN                                                            
      END                                                               
C$TEST NLSK                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAMS N2FB AND N2GB                    
C                                                                       
C***********************************************************************
C *** N2FB AND N2GB EXAMPLE PROGRAM ***                                 
C                                                                       
C *** FIT N = 33 DATA POINTS (T,Y) TO THE CURVE                         
C *** X(1) + X(2)*EXP(T*X(4)) + X(3)*EXP(T*X(5))                        
C                                                                       
C *** THE FOLLOWING CODE IS FOR CALLING N2GB.  DIFFERENCES FOR          
C *** CALLING N2FB ARE EXPLAINED IN COMMENTS.                           
C                                                                       
      INTEGER I, IV(102), LIV, LTY, LV, UI(1)                           
      REAL B(2,5), BIG, TY(50,2), V(491), X(5)                          
      EXTERNAL DUMMY, OSB1J, OSB1R, R1MACH                              
      REAL R1MACH                                                       
      DATA LIV/102/, LTY/50/, LV/491/                                   
C                                                                       
C *** FOR N2FB, OMIT OSB1J FROM THE EXTERNAL STATEMENT.                 
C                                                                       
C                                                                       
C *** TO MAKE THIS EXAMPLE SELF-CONTAINED, WE USE A DATA STATEMENT      
C *** AND DO LOOP TO SUPPLY (T,Y) PAIRS TO THE ARRAY TY.                
C                                                                       
C *** Y VALUES...                                                       
C                                                                       
      DATA TY(1,2) /8.44E-1/, TY(2,2) /9.08E-1/, TY(3,2)/9.32E-1/,      
     1     TY(4,2) /9.36E-1/, TY(5,2) /9.25E-1/, TY(6,2)/9.08E-1/,      
     2     TY(7,2) /8.81E-1/, TY(8,2) /8.50E-1/, TY(9,2)/8.18E-1/,      
     3     TY(10,2)/7.84E-1/, TY(11,2)/7.51E-1/, TY(12,2)/7.18E-1/,     
     4     TY(13,2)/6.85E-1/, TY(14,2)/6.58E-1/, TY(15,2)/6.28E-1/,     
     5     TY(16,2)/6.03E-1/, TY(17,2)/5.80E-1/, TY(18,2)/5.58E-1/,     
     6     TY(19,2)/5.38E-1/, TY(20,2)/5.22E-1/, TY(21,2)/5.06E-1/,     
     7     TY(22,2)/4.90E-1/, TY(23,2)/4.78E-1/, TY(24,2)/4.67E-1/,     
     8     TY(25,2)/4.57E-1/, TY(26,2)/4.48E-1/, TY(27,2)/4.38E-1/,     
     9     TY(28,2)/4.31E-1/, TY(29,2)/4.24E-1/, TY(30,2)/4.20E-1/,     
     A     TY(31,2)/4.14E-1/, TY(32,2)/4.11E-1/, TY(33,2)/4.06E-1/      
C                                                                       
C ***  T VALUES...                                                      
C                                                                       
      DO 10 I = 1, 33                                                   
         TY(I,1) = -10.E+0 * FLOAT(I-1)                                 
 10      CONTINUE                                                       
C                                                                       
C *** SUPPLY LEAD DIMENSION OF TY IN UI(1)...                           
C *** (MOST COMPILERS WOULD LET US SIMPLY PASS LTY FOR UI,              
C *** BUT SOME, E.G. WATFIV, WILL NOT.)                                 
C                                                                       
      UI(1) = LTY                                                       
C                                                                       
C *** SPECIFY ALL DEFAULT IV AND V INPUT COMPONENTS (N2GB AND N2FB      
C *** ONLY)...                                                          
C                                                                       
      IV(1) = 0                                                         
C                                                                       
C *** SUPPLY INITIAL GUESS...                                           
C                                                                       
      X(1) = 0.5E+0                                                     
      X(2) = 1.5E+0                                                     
      X(3) = -1.E+0                                                     
      X(4) = 1.E-2                                                      
      X(5) = 2.E-2                                                      
C                                                                       
C *** SET BIG TO LARGEST POSITIVE (MODEL) NUMBER...                     
C                                                                       
      BIG = R1MACH(2)                                                   
C                                                                       
C *** SUPPLY BOUNDS -- INCLUDING LOWER BOUNDS OF -BIG AND UPPER         
C *** BOUNDS OF BIG WHERE WE DO NOT WISH TO IMPOSE BOUNDS...            
C                                                                       
      DO 20 I = 1, 5                                                    
         B(1,I) = -BIG                                                  
         B(2,I) = BIG                                                   
 20      CONTINUE                                                       
C                                                                       
      B(2,4) = .0125                                                    
      B(1,5) = .03                                                      
C                                                                       
C *** SOLVE THE PROBLEM -- N2GB WILL PRINT THE SOLUTION FOR US...       
C                                                                       
      CALL N2GB(33, 5, X, B, OSB1R, OSB1J, IV, LIV, LV, V, UI, TY,      
     1            DUMMY)                                                
C                                                                       
C *** FOR N2FB, THE CORRESPONDING CALL WOULD BE...                      
C                                                                       
C     CALL N2FB(33, 5, X, B, OSB1R, IV, LIV, LV, V, UI, TY, DUMMY)      
C                                                                       
C                                                                       
C *** NOTE -- ON MOST SYSTEMS, WE COULD SIMPLY PASS OSB1R (OR OSB1J)    
C *** AS THE UF PARAMETER, SINCE OSB1R AND OSB1J IGNORE THIS            
C *** PARAMETER.  BUT THERE EXIST SYSTEMS (E.G. UNIVAC) THAT WOULD      
C *** GIVE A RUN-TIME ERROR IF WE DID THIS.  HENCE WE PASS THE          
C *** IMMEDIATELY FOLLOWING DUMMY SUBROUTINE AS UF.                     
C                                                                       
      STOP                                                              
      END                                                               
      SUBROUTINE DUMMY                                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE OSB1R(N, P, X, NF, R, LTY, TY, UF)                     
C                                                                       
C *** THIS ROUTINE COMPUTES THE RESIDUAL VECTOR, R = R(X),              
C *** FOR TEST PROBLEM OSBORNE1.                                        
C                                                                       
      INTEGER N, P, NF, LTY                                             
      REAL X(P), R(N), TY(LTY,2)                                        
      EXTERNAL UF                                                       
C                                                                       
      INTEGER I                                                         
      REAL TI, YI                                                       
C                                                                       
      DO 10 I = 1, N                                                    
         TI = TY(I,1)                                                   
         YI = TY(I,2)                                                   
         R(I) = YI - (X(1) + X(2)* EXP(X(4)*TI) + X(3)* EXP(X(5)*TI))   
 10      CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE OSB1J(N, P, X, NF, J, LTY, TY, UF)                     
C                                                                       
C *** THIS ROUTINE COMPUTES THE JACOBIAN MATRIX, J = J(X),              
C *** FOR TEST PROBLEM OSBORNE1.  J(I,K) IS SET TO THE PARTIAL          
C *** DERIVATIVE OF COMPONENT I OF R WITH RESPECT TO X(K).              
C                                                                       
      INTEGER N, P, NF, LTY                                             
      REAL X(P), J(N,P), TY(LTY,2)                                      
      EXTERNAL UF                                                       
C                                                                       
      INTEGER I                                                         
      REAL NEGONE, TI                                                   
      DATA NEGONE/-1.E+0/                                               
C                                                                       
      DO 10 I = 1, N                                                    
         TI = TY(I,1)                                                   
         J(I,1) = NEGONE                                                
         J(I,2) = - EXP(X(4)*TI)                                        
         J(I,3) = - EXP(X(5)*TI)                                        
         J(I,4) = TI*X(2)*J(I,2)                                        
         J(I,5) = TI*X(3)*J(I,3)                                        
 10      CONTINUE                                                       
      RETURN                                                            
      END                                                               
C$TEST NLSP                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM N2PB                              
C                                                                       
C***********************************************************************
C *** N2PB EXAMPLE PROGRAM ***                                          
C                                                                       
C *** FIT N = 33 DATA POINTS (T,Y) TO THE CURVE                         
C *** X(1) + X(2)*EXP(T*X(4)) + X(3)*EXP(T*X(5))                        
C                                                                       
      INTEGER I, IV(102), LIV, LTY, LV, UI(1)                           
      REAL B(2,5), BIG, TY(50,2), V(302), X(5)                          
      EXTERNAL DUMMY, OSB1J, OSB1R, R1MACH                              
      REAL R1MACH                                                       
      DATA LIV/102/, LTY/50/, LV/302/                                   
C                                                                       
C *** TO MAKE THIS EXAMPLE SELF-CONTAINED, WE USE A DATA STATEMENT      
C *** AND DO LOOP TO SUPPLY (T,Y) PAIRS TO THE ARRAY TY.                
C                                                                       
C *** Y VALUES...                                                       
C                                                                       
      DATA TY(1,2) /8.44E-1/, TY(2,2) /9.08E-1/, TY(3,2)/9.32E-1/,      
     1     TY(4,2) /9.36E-1/, TY(5,2) /9.25E-1/, TY(6,2)/9.08E-1/,      
     2     TY(7,2) /8.81E-1/, TY(8,2) /8.50E-1/, TY(9,2)/8.18E-1/,      
     3     TY(10,2)/7.84E-1/, TY(11,2)/7.51E-1/, TY(12,2)/7.18E-1/,     
     4     TY(13,2)/6.85E-1/, TY(14,2)/6.58E-1/, TY(15,2)/6.28E-1/,     
     5     TY(16,2)/6.03E-1/, TY(17,2)/5.80E-1/, TY(18,2)/5.58E-1/,     
     6     TY(19,2)/5.38E-1/, TY(20,2)/5.22E-1/, TY(21,2)/5.06E-1/,     
     7     TY(22,2)/4.90E-1/, TY(23,2)/4.78E-1/, TY(24,2)/4.67E-1/,     
     8     TY(25,2)/4.57E-1/, TY(26,2)/4.48E-1/, TY(27,2)/4.38E-1/,     
     9     TY(28,2)/4.31E-1/, TY(29,2)/4.24E-1/, TY(30,2)/4.20E-1/,     
     A     TY(31,2)/4.14E-1/, TY(32,2)/4.11E-1/, TY(33,2)/4.06E-1/      
C                                                                       
C ***  T VALUES...                                                      
C                                                                       
      DO 10 I = 1, 33                                                   
         TY(I,1) = -10.E+0 * FLOAT(I-1)                                 
 10      CONTINUE                                                       
C                                                                       
C *** SUPPLY LEAD DIMENSION OF TY IN UI(1)...                           
C *** (MOST COMPILERS WOULD LET US SIMPLY PASS LTY FOR UI,              
C *** BUT SOME, E.G. WATFIV, WILL NOT.)                                 
C                                                                       
      UI(1) = LTY                                                       
C                                                                       
C *** SPECIFY ALL DEFAULT IV AND V INPUT COMPONENTS...                  
C                                                                       
      IV(1) = 0                                                         
C                                                                       
C ... TO LIMIT THE NUMBER OF ITERATIONS TO 100, WE WOULD REPLACE THE    
C ... ABOVE ASSIGNMENT OF 0 TO IV(1) WITH THE FOLLOWING TWO LINES...    
C                                                                       
C     CALL IVSET(1, IV, LIV, LV, V)                                     
C     IV(18) = 100                                                      
C                                                                       
C                                                                       
C *** SUPPLY INITIAL GUESS...                                           
C                                                                       
      X(1) = 0.5E+0                                                     
      X(2) = 1.5E+0                                                     
      X(3) = -1.E+0                                                     
      X(4) = 1.E-2                                                      
      X(5) = 2.E-2                                                      
C                                                                       
C *** SET BIG TO LARGEST POSITIVE (MODEL) NUMBER...                     
C                                                                       
      BIG = R1MACH(2)                                                   
C                                                                       
C *** SUPPLY BOUNDS -- INCLUDING LOWER BOUNDS OF -BIG AND UPPER         
C *** BOUNDS OF BIG WHERE WE DO NOT WISH TO IMPOSE BOUNDS...            
C                                                                       
      DO 20 I = 1, 5                                                    
         B(1,I) = -BIG                                                  
         B(2,I) = BIG                                                   
 20      CONTINUE                                                       
C                                                                       
      B(2,4) = .0125                                                    
      B(1,5) = .03                                                      
C                                                                       
C *** SOLVE THE PROBLEM -- N2PB WILL PRINT THE SOLUTION FOR US.         
C *** WE COMPUTE 7 RESIDUAL COMPONENTS OR JACOBIAN ROWS PER CALL...     
C                                                                       
      CALL N2PB(33, 7, 5, X, B, OSB1R, OSB1J, IV, LIV, LV, V, UI, TY,   
     1            DUMMY)                                                
C                                                                       
C                                                                       
C *** NOTE -- ON MOST SYSTEMS, WE COULD SIMPLY PASS OSB1R OR OSB1J      
C *** AS THE LAST PARAMETER TO N2PB, SINCE OSB1R AND OSB1J IGNORE       
C *** THEIR UF PARAMETER.  BUT THERE EXIST SYSTEMS (E.G. UNIVAC)        
C *** THAT WOULD GIVE A RUN-TIME ERROR IF WE DID THIS.  HENCE WE        
C *** PASS THE IMMEDIATELY FOLLOWING DUMMY SUBROUTINE AS UF.            
C                                                                       
      STOP                                                              
      END                                                               
      SUBROUTINE DUMMY                                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE OSB1R(N, ND1, N1, N2, P, X, NF, R, LTY, TY, UF)        
C                                                                       
C *** THIS ROUTINE COMPUTES CHUNKS OF THE RESIDUAL VECTOR, R = R(X),    
C *** FOR TEST PROBLEM OSBORNE1.                                        
C                                                                       
      INTEGER N, ND1, N1, N2, P, NF, LTY                                
      REAL X(P), R(ND1), TY(LTY,2)                                      
      EXTERNAL UF                                                       
C                                                                       
      INTEGER I, I1                                                     
      REAL TI, YI                                                       
C                                                                       
      I1 = 1                                                            
      DO 10 I = N1, N2                                                  
         TI = TY(I,1)                                                   
         YI = TY(I,2)                                                   
         R(I1) = YI - (X(1) + X(2)* EXP(X(4)*TI) + X(3)* EXP(X(5)*TI))  
         I1 = I1 + 1                                                    
 10      CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE OSB1J(N, ND1, N1, N2, P, X, NF, J, LTY, TY, UF)        
C                                                                       
C *** THIS ROUTINE COMPUTES CHUNKS OF THE JACOBIAN MATRIX, J = J(X),    
C *** FOR TEST PROBLEM OSBORNE1.                                        
C                                                                       
      INTEGER N, ND1, N1, N2, P, NF, LTY                                
      REAL X(P), J(ND1,P), TY(LTY,2)                                    
      EXTERNAL UF                                                       
C                                                                       
      INTEGER I, I1                                                     
      REAL NEGONE, TI                                                   
      DATA NEGONE/-1.E+0/                                               
C                                                                       
      I1 = 1                                                            
      DO 10 I = N1, N2                                                  
         TI = TY(I,1)                                                   
         J(I1,1) = NEGONE                                               
         J(I1,2) = - EXP(X(4)*TI)                                       
         J(I1,3) = - EXP(X(5)*TI)                                       
         J(I1,4) = TI*X(2)*J(I1,2)                                      
         J(I1,5) = TI*X(3)*J(I1,3)                                      
         I1 = I1 + 1                                                    
 10      CONTINUE                                                       
      RETURN                                                            
      END                                                               
C$TEST NLSR                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAMS NSG AND NSF                      
C                                                                       
C***********************************************************************
C *** NSG AND NSF EXAMPLE PROGRAM ***                                   
C                                                                       
C *** FIT N = 33 DATA POINTS (T,Y) TO THE CURVE                         
C *** X(1) + X(2)*DEXP(T*X(4)) + X(3)*DEXP(T*X(5))                      
C                                                                       
C *** THE FOLLOWING CODE IS FOR CALLING NSG.  DIFFERENCES FOR           
C *** CALLING NSF ARE EXPLAINED IN COMMENTS.                            
C                                                                       
      INTEGER I, J, INC(4,2), IV(124), LIV, LTY, LV, UI(1)              
      DOUBLE PRECISION C(3), T(33), Y(33), V(612), X(5)                 
      EXTERNAL DUMMY, OSB1A, OSB1B                                      
      DATA LIV/124/, LTY/50/, LV/612/                                   
C                                                                       
C *** FOR NSF, OMIT OSB1B FROM THE EXTERNAL STATEMENT.                  
C                                                                       
C                                                                       
C *** TO MAKE THIS EXAMPLE SELF-CONTAINED, WE USE A DATA STATEMENT      
C *** AND DO LOOP TO SUPPLY (T(I),Y(I)) PAIRS.                          
C                                                                       
C *** Y VALUES...                                                       
C                                                                       
      DATA Y(1) /8.44D-1/, Y(2) /9.08D-1/, Y(3)/9.32D-1/,               
     1     Y(4) /9.36D-1/, Y(5) /9.25D-1/, Y(6)/9.08D-1/,               
     2     Y(7) /8.81D-1/, Y(8) /8.50D-1/, Y(9)/8.18D-1/,               
     3     Y(10)/7.84D-1/, Y(11)/7.51D-1/, Y(12)/7.18D-1/,              
     4     Y(13)/6.85D-1/, Y(14)/6.58D-1/, Y(15)/6.28D-1/,              
     5     Y(16)/6.03D-1/, Y(17)/5.80D-1/, Y(18)/5.58D-1/,              
     6     Y(19)/5.38D-1/, Y(20)/5.22D-1/, Y(21)/5.06D-1/,              
     7     Y(22)/4.90D-1/, Y(23)/4.78D-1/, Y(24)/4.67D-1/,              
     8     Y(25)/4.57D-1/, Y(26)/4.48D-1/, Y(27)/4.38D-1/,              
     9     Y(28)/4.31D-1/, Y(29)/4.24D-1/, Y(30)/4.20D-1/,              
     A     Y(31)/4.14D-1/, Y(32)/4.11D-1/, Y(33)/4.06D-1/               
C                                                                       
C ***  T VALUES...                                                      
C                                                                       
      DO 10 I = 1, 33                                                   
         T(I) = -10.D+0 *FLOAT(I-1)                                     
 10      CONTINUE                                                       
C                                                                       
C     ***  SET UP INC  ***                                              
C                                                                       
      DO 30 J = 1, 2                                                    
         DO 20 I = 1, 4                                                 
 20           INC(I,J) = 0                                              
 30      CONTINUE                                                       
      INC(2,1) = 1                                                      
      INC(3,2) = 1                                                      
C                                                                       
C *** SPECIFY ALL DEFAULT IV AND V INPUT COMPONENTS ***                 
C                                                                       
      IV(1) = 0                                                         
C                                                                       
C ... TO TURN OFF THE DEFAULT COMPUTATION AND PRINTING OF THE           
C ... REGRESSION DIAGNOSTIC VECTOR, WE WOULD REPLACE THE ABOVE          
C ... ASSIGNMENT OF 0 TO IV(1) WITH THE FOLLOWING THREE LINES...        
C                                                                       
C     CALL IVSET(1, IV, LIV, LV, V)                                     
C     IV(57) = 1                                                        
C     IV(14) = 1                                                        
C                                                                       
C ... THAT IS, WE SET IV(RDREQ) AND IV(COVPRT) TO 1, THUS REQUESTING    
C ... COMPUTATION AND PRINTING OF JUST A COVARIANCE MATRIX.             
C                                                                       
C                                                                       
C *** SUPPLY INITIAL GUESS...                                           
C                                                                       
      X(1) = 1.D-2                                                      
      X(2) = 2.D-2                                                      
C                                                                       
C *** SOLVE THE PROBLEM -- NSG WILL PRINT THE SOLUTION FOR US...        
C                                                                       
      CALL DNSG(33, 2, 3, X, C, Y, OSB1A, OSB1B, INC, 4,                
     1           IV, LIV, LV, V, UI, T, DUMMY)                          
C                                                                       
C *** FOR NSF, THE CORRESPONDING CALL WOULD BE...                       
C                                                                       
C     CALL NSF(33, 2, 3, X, C, Y, OSB1A, INC, 4,                        
C    1           IV, LIV, LV, V, UI, T, DUMMY)                          
C                                                                       
C                                                                       
C *** NOTE -- ON MOST SYSTEMS, WE COULD SIMPLY PASS OSB1A (OR OSB1B)    
C *** AS THE UF PARAMETER, SINCE OSB1A AND OSB1B IGNORE THIS            
C *** PARAMETER.  BUT THERE EXIST SYSTEMS (E.G. UNIVAC) THAT WOULD      
C *** GIVE A RUN-TIME ERROR IF WE DID THIS.  HENCE WE PASS THE          
C *** IMMEDIATELY FOLLOWING DUMMY SUBROUTINE AS UF.                     
C                                                                       
      STOP                                                              
      END                                                               
      SUBROUTINE DUMMY                                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE OSB1A(N, P, L, X, NF, A, UI, T, UF)                    
C                                                                       
C *** THIS ROUTINE COMPUTES THE A MATRIX, A = A(X),                     
C *** FOR TEST PROBLEM OSBORNE1.                                        
C                                                                       
      INTEGER L, N, NF, P, UI(1)                                        
      DOUBLE PRECISION A(N,1), T(N), X(P)                               
      EXTERNAL UF                                                       
C                                                                       
      INTEGER I                                                         
      DOUBLE PRECISION ONE, TI                                          
      DATA ONE/1.D+0/                                                   
C                                                                       
      DO 10 I = 1, N                                                    
         TI = T(I)                                                      
         A(I,1) = ONE                                                   
         A(I,2) = DEXP(TI*X(1))                                         
         A(I,3) = DEXP(TI*X(2))                                         
 10      CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE OSB1B(N, P, L, X, NF, B, UI, T, UF)                    
C                                                                       
C *** THIS ROUTINE COMPUTES THE JACOBIAN TENSOR, B = B(X),              
C *** FOR TEST PROBLEM OSBORNE1.                                        
C                                                                       
      INTEGER L, N, NF, P, UI(1)                                        
      DOUBLE PRECISION B(N,2), T(N), X(P)                               
      EXTERNAL UF                                                       
C                                                                       
      INTEGER I                                                         
      DOUBLE PRECISION TI                                               
C                                                                       
      DO 10 I = 1, N                                                    
         TI = T(I)                                                      
         B(I,1) = TI * DEXP(TI*X(1))                                    
         B(I,2) = TI * DEXP(TI*X(2))                                    
 10      CONTINUE                                                       
      RETURN                                                            
      END                                                               
C$TEST NMSK                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAMS NSGB AND NSFB                    
C                                                                       
C***********************************************************************
C *** NSGB AND NSFB EXAMPLE PROGRAM ***                                 
C                                                                       
C *** FIT N = 33 DATA POINTS (T,Y) TO THE CURVE                         
C *** X(1) + X(2)*EXP(T*X(4)) + X(3)*EXP(T*X(5))                        
C                                                                       
C *** THE FOLLOWING CODE IS FOR CALLING NSGB.  DIFFERENCES FOR          
C *** CALLING NSFB ARE EXPLAINED IN COMMENTS.                           
C                                                                       
      INTEGER I, J, INC(4,2), IV(130), LIV, LTY, LV, UI(1)              
      REAL BX(2,2), BIG, C(3), T(33), Y(33), V(461), X(5)               
      EXTERNAL DUMMY, OSB1A, OSB1B, R1MACH                              
      REAL R1MACH                                                       
C                                                                       
C *** FOR NSFB, OMIT OSB1B FROM THE EXTERNAL STATEMENT.                 
C                                                                       
      DATA LIV/130/, LTY/50/, LV/461/                                   
C                                                                       
C *** TO MAKE THIS EXAMPLE SELF-CONTAINED, WE USE A DATA STATEMENT      
C *** AND DO LOOP TO SUPPLY (T(I),Y(I)) PAIRS.                          
C                                                                       
C *** Y VALUES...                                                       
C                                                                       
      DATA Y(1) /8.44E-1/, Y(2) /9.08E-1/, Y(3)/9.32E-1/,               
     1     Y(4) /9.36E-1/, Y(5) /9.25E-1/, Y(6)/9.08E-1/,               
     2     Y(7) /8.81E-1/, Y(8) /8.50E-1/, Y(9)/8.18E-1/,               
     3     Y(10)/7.84E-1/, Y(11)/7.51E-1/, Y(12)/7.18E-1/,              
     4     Y(13)/6.85E-1/, Y(14)/6.58E-1/, Y(15)/6.28E-1/,              
     5     Y(16)/6.03E-1/, Y(17)/5.80E-1/, Y(18)/5.58E-1/,              
     6     Y(19)/5.38E-1/, Y(20)/5.22E-1/, Y(21)/5.06E-1/,              
     7     Y(22)/4.90E-1/, Y(23)/4.78E-1/, Y(24)/4.67E-1/,              
     8     Y(25)/4.57E-1/, Y(26)/4.48E-1/, Y(27)/4.38E-1/,              
     9     Y(28)/4.31E-1/, Y(29)/4.24E-1/, Y(30)/4.20E-1/,              
     A     Y(31)/4.14E-1/, Y(32)/4.11E-1/, Y(33)/4.06E-1/               
C                                                                       
C ***  T VALUES...                                                      
C                                                                       
      DO 10 I = 1, 33                                                   
         T(I) = -10.E+0 * FLOAT(I-1)                                    
 10      CONTINUE                                                       
C                                                                       
C     ***  SET UP INC  ***                                              
C                                                                       
      DO 30 J = 1, 2                                                    
         DO 20 I = 1, 4                                                 
 20           INC(I,J) = 0                                              
 30      CONTINUE                                                       
      INC(2,1) = 1                                                      
      INC(3,2) = 1                                                      
C                                                                       
C *** SPECIFY ALL DEFAULT IV AND V INPUT COMPONENTS ***                 
C                                                                       
      IV(1) = 0                                                         
C                                                                       
C ... TO SET THE MAXIMUM NUMBER OF ITERATIONS TO 100 AND TURN OFF       
C ... THE PRINTING OF THE ITERATION SUMMARY, WE WOULD REPLACE THE       
C ... ABOVE ASSIGNMENT OF 0 TO IV(1) WITH THE FOLLOWING THREE LINES...  
C                                                                       
C     CALL IVSET(1, IV, LIV, LV, V)                                     
C     IV(18) = 100                                                      
C     IV(19) = 0                                                        
C                                                                       
C                                                                       
C *** SUPPLY INITIAL GUESS...                                           
C                                                                       
      X(1) = 1.E-2                                                      
      X(2) = 2.E-2                                                      
C                                                                       
C *** SET BIG TO LARGEST POSITIVE (MODEL) NUMBER...                     
C                                                                       
      BIG = R1MACH(2)                                                   
C                                                                       
C *** SUPPLY BOUNDS -- INCLUDING LOWER BOUNDS OF -BIG AND UPPER         
C *** BOUNDS OF BIG WHERE WE DO NOT WISH TO IMPOSE BOUNDS...            
C                                                                       
      BX(1,1) = -BIG                                                    
      BX(2,1) = .0125E+0                                                
      BX(1,2) = .03E+0                                                  
      BX(2,2) = BIG                                                     
C                                                                       
C *** SOLVE THE PROBLEM -- NSGB WILL PRINT THE SOLUTION FOR US...       
C                                                                       
      CALL NSGB(33, 2, 3, X, BX, C, Y, OSB1A, OSB1B, INC, 4,            
     1           IV, LIV, LV, V, UI, T, DUMMY)                          
C                                                                       
C *** FOR NSFB, THE CORRESPONDING CALL WOULD BE...                      
C                                                                       
C     CALL NSFB(33, 2, 3, X, BX, C, Y, OSB1A, INC, 4,                   
C    1           IV, LIV, LV, V, UI, T, DUMMY)                          
C                                                                       
C                                                                       
C *** NOTE -- ON MOST SYSTEMS, WE COULD SIMPLY PASS OSB1A (OR OSB1B)    
C *** AS THE UF PARAMETER, SINCE OSB1A AND OSB1B IGNORE THIS            
C *** PARAMETER.  BUT THERE EXIST SYSTEMS (E.G. UNIVAC) THAT WOULD      
C *** GIVE A RUN-TIME ERROR IF WE DID THIS.  HENCE WE PASS THE          
C *** IMMEDIATELY FOLLOWING DUMMY SUBROUTINE AS UF.                     
C                                                                       
      STOP                                                              
      END                                                               
      SUBROUTINE DUMMY                                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE OSB1A(N, P, L, X, NF, A, UI, T, UF)                    
C                                                                       
C *** THIS ROUTINE COMPUTES THE A MATRIX, A = A(X),                     
C *** FOR TEST PROBLEM OSBORNE1.                                        
C                                                                       
      INTEGER L, N, NF, P, UI(1)                                        
      REAL A(N,1), T(N), X(P)                                           
      EXTERNAL UF                                                       
C                                                                       
      INTEGER I                                                         
      REAL ONE, TI                                                      
      DATA ONE/1.E+0/                                                   
C                                                                       
      DO 10 I = 1, N                                                    
         TI = T(I)                                                      
         A(I,1) = ONE                                                   
         A(I,2) =  EXP(TI*X(1))                                         
         A(I,3) =  EXP(TI*X(2))                                         
 10      CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE OSB1B(N, P, L, X, NF, B, UI, T, UF)                    
C                                                                       
C *** THIS ROUTINE COMPUTES THE JACOBIAN TENSOR, B = B(X),              
C *** FOR TEST PROBLEM OSBORNE1.                                        
C                                                                       
      INTEGER L, N, NF, P, UI(1)                                        
      REAL B(N,2), T(N), X(P)                                           
      EXTERNAL UF                                                       
C                                                                       
      INTEGER I                                                         
      REAL TI                                                           
C                                                                       
      DO 10 I = 1, N                                                    
         TI = T(I)                                                      
         B(I,1) = TI *  EXP(TI*X(1))                                    
         B(I,2) = TI *  EXP(TI*X(2))                                    
 10      CONTINUE                                                       
      RETURN                                                            
      END                                                               
C$TEST LRPA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM LINPR                             
C                                                                       
C***********************************************************************
       REAL X(4),B(3),C(4),A(3,4),SIMP(8)                               
       INTEGER ISIMP(8)                                                 
       N=4                                                              
       IA=3                                                             
       M=3                                                              
       IE=1                                                             
C                                                                       
C SET UP GENERAL CONSTRAINTS                                            
C                                                                       
       DO 10 J=1,N                                                      
          A(1,J)=FLOAT(J)                                               
          A(2,J)=0.0                                                    
          A(3,J)=0.0                                                    
  10   CONTINUE                                                         
       A(2,1)=1.0                                                       
       A(2,2)=1.0                                                       
       A(3,2)=-1.0                                                      
       A(3,4)=-1.0                                                      
       B(1)=5                                                           
       B(2)=1.0                                                         
       B(3)=-5.0                                                        
C                                                                       
C SET UP SIMPLE CONSTRAINTS                                             
C                                                                       
      IS=8                                                              
      DO 20 I=1,N                                                       
         SIMP(I)=FLOAT(-I)                                              
         ISIMP(I)=I                                                     
         SIMP(I+N)=10.0                                                 
         ISIMP(I+N)=-I                                                  
  20   CONTINUE                                                         
C                                                                       
C SET UP COST VECTOR AND INITIAL GUESS                                  
C                                                                       
      DO 30 I=1,N                                                       
         C(I)=FLOAT(I+1)                                                
         X(I)=1.0                                                       
  30  CONTINUE                                                          
C                                                                       
C CALL LINEAR PROGRAMMING PACKAGE                                       
C                                                                       
      CALL LINPR(A,M,N,IA,B,C,X,15,CTX,IS,SIMP,ISIMP,IE)                
      WRITE(6,21)(X(I),I=1,N)                                           
 21   FORMAT(11H SOLUTION: ,4E15.6)                                     
      WRITE(6,22)CTX                                                    
 22   FORMAT(17H FUNCTION VALUE: ,E15.5)                                
      STOP                                                              
      END                                                               
C$TEST LRPB                                                             
C***********************************************************************
C                                                                       
C  FIRST EXAMPLE OF USE OF THE PORT PROGRAM LINPA                       
C                                                                       
C***********************************************************************
       REAL X(4),B(3),C(4),A(3,4),SIMP(8)                               
       EXTERNAL LPMAN, PRINT                                            
       INTEGER ISIMP(8),IPTG(3)                                         
       REAL U(4)                                                        
       N=4                                                              
       IA=3                                                             
       M=3                                                              
       IE=1                                                             
C                                                                       
C SET UP GENERAL CONSTRAINTS                                            
C                                                                       
       DO 10 J=1,N                                                      
          A(1,J)=FLOAT(J)                                               
          A(2,J)=0.0                                                    
          A(3,J)=0.0                                                    
  10   CONTINUE                                                         
       A(2,1)=1.0                                                       
       A(2,2)=1.0                                                       
       A(3,2)=-1.0                                                      
       A(3,4)=-1.0                                                      
       B(1)=5                                                           
       B(2)=1.0                                                         
       B(3)=-5.0                                                        
C                                                                       
C SET UP SIMPLE CONSTRAINTS                                             
C                                                                       
      IS=8                                                              
      DO 20 I=1,N                                                       
         SIMP(I)=FLOAT(-I)                                              
         ISIMP(I)=I                                                     
         SIMP(I+N)=10.0                                                 
         ISIMP(I+N)=-I                                                  
  20   CONTINUE                                                         
C                                                                       
C SET UP COST VECTOR AND INITIAL GUESS                                  
C                                                                       
      DO 30 I=1,N                                                       
         C(I)=FLOAT(I+1)                                                
         X(I)=1.0                                                       
  30  CONTINUE                                                          
C                                                                       
C CALL LINEAR PROGRAMMING PACKAGE                                       
C                                                                       
      CALL LINPA(A,M,N,LPMAN,IA,B,C,X,15,CTX,IS,SIMP,ISIMP,IE,          
     1PRINT,IAG,IAS,IPTG,U)                                             
      IWRITE=I1MACH(2)                                                  
      WRITE(IWRITE,21)(X(I),I=1,N)                                      
 21   FORMAT(11H SOLUTION: ,4E15.6)                                     
      WRITE(IWRITE,22)CTX                                               
 22   FORMAT(17H FUNCTION VALUE: ,E15.5)                                
      STOP                                                              
      END                                                               
       SUBROUTINE PRINT(A,M,N,AMAN,IA,B,C,X,CTX,IS,SIMP,ISIMP,IE,       
     1 ITER,IPTG,IAG,IAS,U,IEND)                                        
C                                                                       
C THIS IS A PRINT ROUTINE                                               
C                                                                       
       REAL CTX,A(1),X(N),B(1)                                          
       LOGICAL IEND                                                     
       EXTERNAL AMAN                                                    
       INTEGER IA(1),IPTG(N),ISIMP(1),S                                 
       REAL SIMP(1),C(1),U(1)                                           
       IEND = .FALSE.                                                   
       IWRITE=I1MACH(2)                                                 
       TOL = -R1MACH(4)*(5.0+4.0*SASUM(N,X,1))*10.0                     
       IAGPE=IAG+IE                                                     
       WRITE(IWRITE,1)ITER,CTX,IAGPE,IAS                                
  1    FORMAT(/14H AT ITERATION ,I5,6H CTX= ,E15.5,                     
     1  /18H NO.OF ACT. GEN.= ,I5,15H NO.OF ACT.SIM=,I5)                
       WRITE(IWRITE,2)(X(I),I=1,N)                                      
  2    FORMAT(3H X ,5E15.5)                                             
       DO 10 I=1,M                                                      
          CALL AMAN(.TRUE.,A,IA,N,I,X,TOUT)                             
          TOUT=TOUT-B(I)                                                
          IF (TOUT .LT. TOL)IEND=.TRUE.                                 
          WRITE(IWRITE,9)I,TOUT                                         
  9       FORMAT(15H AT CONSTRAINT ,I5,11H RESIDUAL= ,E15.5)            
 10    CONTINUE                                                         
       IF (IAGPE .EQ. 0)GO TO 12                                        
       WRITE(IWRITE,11)(IPTG(I),I=1,IAGPE)                              
 11    FORMAT(29H  ACTIVE GENERAL CONSTRAINTS ,10I4)                    
 12    IF (IAS .LT. 1)RETURN                                            
       DO 15 I=1,IAS                                                    
          IP=IABS(ISIMP(I))                                             
          IF (ISIMP(I) .GT. 0)WRITE(IWRITE,13)IP                        
 13       FORMAT(18H LOWER BOUND ON X(,I2,11H) IS ACTIVE)               
          IF (ISIMP(I) .LT. 0)WRITE(IWRITE,14)IP                        
 14       FORMAT(18H UPPER BOUND ON X(,I2,11H) IS ACTIVE)               
 15    CONTINUE                                                         
       RETURN                                                           
       END                                                              
C$TEST LRPG                                                             
C***********************************************************************
C                                                                       
C  SECOND EXAMPLE OF USE OF THE PORT PROGRAM LINPA                      
C                                                                       
C***********************************************************************
        REAL X(30), C(30), B(29), SIMP(31), U(30)                       
        INTEGER ISIMP(31), IPTG(30)                                     
        EXTERNAL LPRNT,AMAN                                             
        COMMON /CSTAK/DSTAK                                             
        DOUBLE PRECISION DSTAK(2000)                                    
C                                                                       
C  GET WORK SPACE FROM THE STACK                                        
C                                                                       
        CALL ISTKIN(2000,4)                                             
        N=30                                                            
        M=29                                                            
        IE=0                                                            
        IS=31                                                           
C                                                                       
C SET UP RIGHT HAND SIDE                                                
C                                                                       
        DO 10 I =1,M                                                    
           B(I) = FLOAT(I)/10.0                                         
  10    CONTINUE                                                        
C                                                                       
C SET UP INITIAL GUESS, OBJECTIVE FUNCTION AND SIMPLE CONSTRAINTS       
C                                                                       
       SIGN=-1.0                                                        
       DO 20 I=1,N                                                      
          X(I)=3.0*FLOAT(I)                                             
          C(I)=SIGN*FLOAT(I)                                            
          SIGN=-SIGN                                                    
          ISIMP(I)=I                                                    
          SIMP(I)=FLOAT(I)                                              
  20   CONTINUE                                                         
       ISIMP(N+1)=-N                                                    
       SIMP(N+1)=3.0*FLOAT(N)                                           
C                                                                       
C SOLVE THE PROBLEM AND PRINT OUT THE RESULTS                           
C                                                                       
       CALL LINPA(A,M,N,AMAN,IA,B,C,X,100,CTX,IS,SIMP,ISIMP,IE,         
     1 LPRNT,IAG,IAS,IPTG,U)                                            
       IWRITE=I1MACH(2)                                                 
       WRITE(IWRITE,21)(X(I),I=1,N)                                     
  21   FORMAT(10H SOLUTION ,5E15.5)                                     
C                                                                       
       IF(IAG .GT. 1)WRITE(IWRITE,22)(IPTG(I),I=1,IAG)                  
  22   FORMAT( 28H ACTIVE GENERAL CONSTRAINTS ,15I3)                    
C                                                                       
       IF (IAS .EQ. 0)STOP                                              
       DO 30 I=1,IAS                                                    
          IP=IABS(ISIMP(I))                                             
          WRITE(IWRITE,23)IP                                            
  23      FORMAT(12H BOUND ON X(,I2,11H) IS ACTIVE)                     
  30   CONTINUE                                                         
       STOP                                                             
       END                                                              
       SUBROUTINE AMAN(L,A,IA,N,I,TVEC,T)                               
       LOGICAL L                                                        
       REAL TVEC(N)                                                     
       IF (L) GOTO 20                                                   
C                                                                       
C THE ITH ROW IS REQUESTED                                              
C                                                                       
       DO 10 J=1,N                                                      
          TVEC(J)=0.0                                                   
  10   CONTINUE                                                         
       TVEC(I+1)=1.0                                                    
       TVEC(I)=-1.0                                                     
       RETURN                                                           
C                                                                       
C THIS IS INNERPRODUCT REQUEST                                          
C                                                                       
  20   T=TVEC(I+1)-TVEC(I)                                              
       RETURN                                                           
       END                                                              
C$TEST LRPF                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM FEASA                             
C                                                                       
C***********************************************************************
       REAL X(4),B(5),A(5,4),SIMP(8)                                    
       EXTERNAL LPMAN, FPRNT                                            
       INTEGER ISIMP(8), IPTG(10)                                       
       DATA B(1)/5.0/,B(2)/9.0/,B(3)/9.0/,B(4)/1.0/,B(5)/-5.0/          
       N=4                                                              
       IA=5                                                             
       M=5                                                              
       IE=2                                                             
       IWRITE=I1MACH(2)                                                 
C                                                                       
C SET UP GENERAL CONSTRAINTS                                            
C                                                                       
       DO 10 J=1,N                                                      
          A(1,J)=FLOAT(J)                                               
          A(2,J)=FLOAT(J+1)                                             
          A(3,J)=FLOAT(J*J)                                             
          A(4,J)=0.0                                                    
          A(5,J)=0.0                                                    
  10   CONTINUE                                                         
       A(4,1)=1.0                                                       
       A(4,2)=1.0                                                       
       A(5,2)=-1.0                                                      
       A(5,4)=-1.0                                                      
C                                                                       
C SET UP SIMPLE CONSTRAINTS                                             
C                                                                       
      IS=8                                                              
      DO 20 I=1,N                                                       
         SIMP(I)=FLOAT(-I)                                              
         ISIMP(I)=I                                                     
         SIMP(I+N)=FLOAT(I+2)                                           
         ISIMP(I+N)=-I                                                  
  20   CONTINUE                                                         
C                                                                       
C SET UP INITIAL GUESS                                                  
C                                                                       
      DO 30 I=1,N                                                       
         X(I)=1.0                                                       
  30  CONTINUE                                                          
C                                                                       
C CALL FEASIBLE POINT ALGORITHM                                         
C                                                                       
      CALL FEASA(A,M,N,LPMAN,IA,B,X,15,IS,SIMP,ISIMP,IE,                
     1 FPRNT,IAG,IAS,IPTG)                                              
      WRITE(IWRITE,31)(X(I),I=1,N)                                      
 31   FORMAT(11H SOLUTION: ,4E15.6)                                     
C                                                                       
C CHECK ANSWER                                                          
C                                                                       
      DO 40 I=1,M                                                       
         S = SDOT(N, A(I,1), IA, X, 1) -B(I)                            
         WRITE(IWRITE,41)I, S                                           
 40   CONTINUE                                                          
 41   FORMAT(28H THE RESIDUAL AT CONSTRAINT ,I4,4H IS ,E15.5)           
      STOP                                                              
      END                                                               
       SUBROUTINE FPRNT(A,M,N,AMAN,IA,B,C,X,CTX,IS,SIMP,ISIMP,IE,       
     1 ITER,IPTG,IAG,IAS,U,IEND,IPHAS)                                  
C                                                                       
C THIS IS A PRINT ROUTINE                                               
C                                                                       
       REAL CTX,A(1),X(N),B(1)                                          
       LOGICAL IEND                                                     
       EXTERNAL AMAN                                                    
       INTEGER IA(1),IPTG(N),ISIMP(1),S                                 
       REAL SIMP(1),C(1),U(1)                                           
       IEND = .FALSE.                                                   
       IWRITE=I1MACH(2)                                                 
       IAGPE=IAG+IE                                                     
       WRITE(IWRITE,1)ITER,IAGPE,IAS                                    
  1    FORMAT(/14H AT ITERATION ,I5,                                    
     1  /18H NO.OF ACT. GEN.= ,I5,15H NO.OF ACT.SIM= I5)                
       WRITE(IWRITE,2)(X(I),I=1,N)                                      
 2     FORMAT(3H X ,5E15.5)                                             
       DO 10 I=1,M                                                      
          CALL AMAN(.TRUE.,A,IA,N,I,X,TOUT)                             
          TOUT=TOUT-B(I)                                                
          WRITE(IWRITE,9)I,TOUT                                         
  9       FORMAT(15H AT CONSTRAINT ,I5,11H RESIDUAL= ,E15.5)            
 10    CONTINUE                                                         
       IF (IAGPE.EQ.0)GO TO 12                                          
       WRITE(IWRITE,11)(IPTG(I),I=1,IAGPE)                              
 11    FORMAT(29H  ACTIVE GENERAL CONSTRAINTS ,10I4)                    
 12    IF (IAS.LT.1)RETURN                                              
       DO 15 I=1,IAS                                                    
          IP=IABS(ISIMP(I))                                             
          IF (ISIMP(I).GT.0)WRITE(IWRITE,13)IP                          
 13       FORMAT(18H LOWER BOUND ON X(,I2,11H) IS ACTIVE)               
          IF (ISIMP(I).LT.0)WRITE(IWRITE,14)IP                          
 14       FORMAT(18H UPPER BOUND ON X(,I2,11H) IS ACTIVE)               
 15    CONTINUE                                                         
       RETURN                                                           
       END                                                              
C$TEST LRPE                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM FEAS                              
C                                                                       
C***********************************************************************
       REAL X(4),B(5),A(5,4),SIMP(8)                                    
       INTEGER ISIMP(8)                                                 
       DATA B(1)/5.0/,B(2)/9.0/,B(3)/9.0/,B(4)/1.0/,B(5)/-5.0/          
       N=4                                                              
       IA=5                                                             
       M=5                                                              
       IE=2                                                             
       IWRITE=I1MACH(2)                                                 
C                                                                       
C SET UP GENERAL CONSTRAINTS                                            
C                                                                       
       DO 10 J=1,N                                                      
          A(1,J)=FLOAT(J)                                               
          A(2,J)=FLOAT(J+1)                                             
          A(3,J)=FLOAT(J*J)                                             
          A(4,J)=0.0                                                    
          A(5,J)=0.0                                                    
  10   CONTINUE                                                         
       A(4,1)=1.0                                                       
       A(4,2)=1.0                                                       
       A(5,2)=-1.0                                                      
       A(5,4)=-1.0                                                      
C                                                                       
C SET UP SIMPLE CONSTRAINTS                                             
C                                                                       
      IS=8                                                              
      DO 20 I=1,N                                                       
         SIMP(I)=FLOAT(-I)                                              
         ISIMP(I)=I                                                     
         SIMP(I+N)=FLOAT(I+2)                                           
         ISIMP(I+N)=-I                                                  
  20   CONTINUE                                                         
C                                                                       
C SET UP INITIAL GUESS                                                  
C                                                                       
      DO 30 I=1,N                                                       
         X(I)=1.0                                                       
  30  CONTINUE                                                          
C                                                                       
C CALL FEASIBLE POINT ALGORITHM                                         
C                                                                       
      CALL FEAS(A,M,N,IA,B,X,15,IS,SIMP,ISIMP,IE)                       
      WRITE(IWRITE,31)(X(I),I=1,N)                                      
 31   FORMAT(11H SOLUTION: ,4E15.6)                                     
C                                                                       
C CHECK ANSWER                                                          
C                                                                       
      DO 40 I=1,M                                                       
         S = SDOT(N, A(I,1), IA, X, 1) -B(I)                            
         WRITE(IWRITE,41)I, S                                           
 40   CONTINUE                                                          
 41   FORMAT(28H THE RESIDUAL AT CONSTRAINT ,I4,4H IS ,E15.5)           
      STOP                                                              
      END                                                               
C$TEST QPRA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM IQP                               
C                                                                       
C***********************************************************************
C TEST PROGRAM FOR IQP                                                  
        REAL X(10), Q(10,10), A(10,10), BL(10), BU(10)                  
        REAL C(10), B(10)                                               
        REAL SUM(10), FUNCT                                             
        INTEGER N, I, J, IPRINT, MAXITR, IQ, M, IA                      
        INTEGER IEQ                                                     
        DOUBLE PRECISION DSTAK(2000)                                    
        COMMON /CSTAK/DSTAK                                             
C                                                                       
        IWRITE = I1MACH(2)                                              
        CALL ISTKIN(2000,4)                                             
        N = 4                                                           
        M = 3                                                           
C SET UP INITIAL GUESS AND QUADRATIC FUNCTION                           
        DO 1 I=1,N                                                      
            X(I) = I + 1.                                               
            C(I) = 8. - I                                               
            DO 2 J=1,N                                                  
               Q(I,J) = FLOAT(IABS(I-J))                                
 2          CONTINUE                                                    
            Q(I,I) = 1.69                                               
C SET UP GENERAL CONSTRAINTS                                            
            DO 16 J=1,M                                                 
               A(J,I) = 0.                                              
 16         CONTINUE                                                    
 1      CONTINUE                                                        
        DO 3 I=1,M                                                      
            B(I) = -1. - (I - 1.) * .05                                 
            A(I,I) = -1.                                                
            A(I,I+1) = 1.                                               
 3      CONTINUE                                                        
        IQ = 10                                                         
        IA = 10                                                         
        IEQ = 1                                                         
C SET UP SIMPLE CONSTRAINTS                                             
        DO 4 I=1,N                                                      
            BL(I) = -I - (I - 1.) * .1                                  
            BU(I) = I                                                   
 4      CONTINUE                                                        
C GET MACHINE INFINITY FROM PORT                                        
        BU(1) = R1MACH(2)                                               
        IPRINT = 1                                                      
        MAXITR = 3*N                                                    
C CALL THE QUADRATIC PROGRAMMING PACKAGE                                
        CALL IQP(N, X, Q, IQ, C, M, A, IA, B, BL, BU, IPRINT,           
     1          MAXITR, IEQ)                                            
C COMPUTE FINAL FUNCTION VALUE                                          
        DO 6 J=1,N                                                      
            SUM(J) = X(J) * Q(J,J)                                      
 6      CONTINUE                                                        
        DO 7 I=2,N                                                      
            DO 9 J=1,I-1                                                
                SUM(I) = SUM(I) + X(J)*Q(J,I)                           
                SUM(J) = SUM(J) + X(I)*Q(J,I)                           
 9          CONTINUE                                                    
 7      CONTINUE                                                        
        FUNCT = 0.                                                      
        DO 10 I=1,N                                                     
            FUNCT = SUM(I) * X(I)/2. + FUNCT + C(I) * X(I)              
 10     CONTINUE                                                        
        WRITE (IWRITE,1000)                                             
 1000    FORMAT (16H FINAL SOLUTION:)                                   
        DO 11 I=1,N                                                     
           WRITE (IWRITE, 1001) I, X(I)                                 
 1001       FORMAT (I5,D14.4)                                           
 11     CONTINUE                                                        
        WRITE (IWRITE,1002)                                             
 1002    FORMAT (22H FINAL FUNCTION VALUE:)                             
        WRITE (IWRITE,1003) FUNCT                                       
 1003    FORMAT (D14.4)                                                 
        STOP                                                            
        END                                                             
C$TEST MFTE                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM MFTCC                             
C                                                                       
C***********************************************************************
      REAL A(25,25,25),B(25,25,25),T(50)                                
      REAL AA(25,25,25),BB(25,25,25)                                    
      REAL RSTAK(1262)                                                  
      REAL SUM,FN1                                                      
      INTEGER IFX(25)                                                   
      INTEGER I,J,K,L,IFB                                               
      DOUBLE PRECISION DSTAK(631)                                       
      COMMON /CSTAK/DSTAK                                               
      EQUIVALENCE (RSTAK(1),DSTAK(1))                                   
C                                                                       
C  N IS THE DIMENSION OF THE COMPLEX CUBE                               
C                                                                       
      N = 25                                                            
C                                                                       
C  GET ONE PLANE OF WORKSPACE                                           
C                                                                       
      NWK = 2*N*N + 12                                                  
      CALL ISTKIN(NWK,3)                                                
C                                                                       
C  SET REAL AND IMAGINARY PARTS TO YOUR FAVORITE VALUES HERE            
C  AA AND BB ARE COPIES TO COMPARE WITH UNNORMALIZED OUTPUT             
C  THIS EXAMPLE USES RANDOM DATA, SEE THE UTILITY CHAPTER OF PORT 3.    
C                                                                       
      DO 1 K = 1,N                                                      
      DO 1 J = 1,N                                                      
      DO 1 I = 1,N                                                      
         A(I,J,K)  = UNI(0)                                             
         AA(I,J,K) = A(I,J,K)                                           
         B(I,J,K)  = UNI(0)                                             
         BB(I,J,K) = B(I,J,K)                                           
 1    CONTINUE                                                          
C                                                                       
C  INITIALIZE TRIGONOMETRIC TABLES AND FACTOR N                         
C                                                                       
      CALL MFTCI(N,IFX,T)                                               
C                                                                       
C  OUTER LOOP COMPUTES A FORWARD (SIGN=1.0), THEN BACKWARD (SIGN=-1.0)  
C  TRANSFORM. FIRST X, THEN Y, THEN Z.                                  
C                                                                       
      SIGN = 1.0E0                                                      
      N2   = N*N                                                        
      NT   = N                                                          
C                                                                       
C                                                                       
      DO 2 IFB = 1,2                                                    
C  X-DIRECTION TRANSFORMS FOR EACH X-Y PLANE                            
         DO 3 L = 1,N                                                   
            CALL MFTCC(NT,NT,A(1,1,L),B(1,1,L),1,NT,                    
     *                       A(1,1,L),B(1,1,L),1,NT,IFX,T,SIGN)         
 3       CONTINUE                                                       
C  Y-DIRECTION TRANSFORMS FOR EACH X-Y PLANE                            
         DO 4 L = 1,N                                                   
            CALL MFTCC(NT,NT,A(1,1,L),B(1,1,L),NT,1,                    
     *                       A(1,1,L),B(1,1,L),NT,1,IFX,T,SIGN)         
 4       CONTINUE                                                       
C  Z-DIRECTION TRANSFORMS FOR EACH Y-Z PLANE                            
         DO 5 L = 1,N                                                   
            CALL MFTCC(NT,NT,A(L,1,1),B(L,1,1),NT,N2,                   
     *                       A(L,1,1),B(L,1,1),NT,N2,IFX,T,SIGN)        
 5       CONTINUE                                                       
C                                                                       
         SIGN = -1.0E0                                                  
 2    CONTINUE                                                          
C                                                                       
C                                                                       
C  COMPARE INPUT TO UNNORMALIZED OUTPUT FROM FORWARD/BACKWARD FFT       
C                                                                       
      FN1 = 1.0E0/FLOAT(N*N*N)                                          
      DO 6 K = 1,N                                                      
      DO 6 J = 1,N                                                      
      DO 6 I = 1,N                                                      
         AA(I,J,K) = AA(I,J,K) - FN1*A(I,J,K)                           
         BB(I,J,K) = BB(I,J,K) - FN1*B(I,J,K)                           
 6    CONTINUE                                                          
C                                                                       
C  ERR IS THE RMS ERROR, SDOT COMPUTES THE SUM OF SQUARES, SEE THE      
C  LINEAR ALGEBRA CHAPTER OF PORT 3                                     
C                                                                       
      ERR = SDOT(N2,AA,1,AA,1) + SDOT(N2,BB,1,BB,1)                     
      ERR = SQRT(FN1*ERR)                                               
C                                                                       
C  PRINT RESULTS                                                        
C                                                                       
      IWRITE = I1MACH(2)                                                
      WRITE(IWRITE,1000)N,N,N,ERR                                       
 1000 FORMAT(1X,18H FOR LATTICE SIZE ,I3,2(2H X,I3),9H ERROR = ,E11.5)  
      STOP                                                              
      END                                                               
C$TEST MFTF                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM MFTRC                             
C                                                                       
C***********************************************************************
      REAL A(200,100)                                                   
      REAL AA(200,100)                                                  
      REAL TX(200),TY(200)                                              
      REAL RSTAK(20014)                                                 
      REAL FN2,RMSERR,SIGN,SUM                                          
      INTEGER IFX(25),IFY(25)                                           
      INTEGER I,J,N,N2,NP2,NP3,N2MK                                     
      DOUBLE PRECISION DSTAK(10007)                                     
      COMMON /CSTAK/DSTAK                                               
      EQUIVALENCE (RSTAK(1),DSTAK(1))                                   
C                                                                       
C                                                                       
      CALL ISTKIN(20014,3)                                              
C                                                                       
      N   = 100                                                         
      NP2 = 102                                                         
      N2  = 200                                                         
C                                                                       
C   SET INPUT VECTORS TO YOUR FAVORITE VALUES HERE, THIS EXAMPLE        
C   USES RANDOM INITIAL VALUES.                                         
C                                                                       
      DO 1 J = 1,N                                                      
         DO 2 I = 1,N                                                   
            A(I,J)  = UNI(0)                                            
            AA(I,J) = A(I,J)                                            
 2       CONTINUE                                                       
 1    CONTINUE                                                          
C                                                                       
      SIGN = 1.0E0                                                      
      CALL MFTRI(N,IFX,TX)                                              
      CALL MFTCI(N,IFY,TY)                                              
C                                                                       
C  X-DIMENSION                                                          
C                                                                       
      CALL MFTRC(N,N,A,1,N2,A(1,1),A(2,1),2,N2,IFX,TX,SIGN)             
C                                                                       
C  FILL-IN FROM CONJUGATION OF TERMS                                    
C                                                                       
      NP3  = N+3                                                        
      N2MK = N-1                                                        
      DO 3 I = NP3,N2,2                                                 
         DO 4 J = 1,N                                                   
            A(I,J)   = A(N2MK,J)                                        
            A(I+1,J) = - A(N2MK+1,J)                                    
 4       CONTINUE                                                       
         N2MK = N2MK - 2                                                
 3    CONTINUE                                                          
C                                                                       
C  DO COMPLEX PART IN Y-DIRECTION                                       
C                                                                       
      CALL MFTCC(N,N,A(1,1),A(2,1),N2,2,A(1,1),A(2,1),N2,2,IFY,TY,SIGN) 
C                                                                       
C  NOW GO BACKWARDS, COMPLEX TO COMPLEX FIRST                           
C                                                                       
      SIGN = -1.0E0                                                     
      CALL MFTCC(N,N,A(1,1),A(2,1),N2,2,A(1,1),A(2,1),N2,2,IFY,TY,SIGN) 
C                                                                       
C  AND BACK TO REAL                                                     
C                                                                       
      CALL MFTCR(N,N,A(1,1),A(2,1),2,N2,A,1,N2,IFX,TX,SIGN)             
C                                                                       
C  COMPARE TO INPUT                                                     
C                                                                       
      FN2 = 1./FLOAT(N*N)                                               
      SUM = 0.0E0                                                       
      DO 5 J = 1,N                                                      
         DO 6 I = 1,N                                                   
            SUM = SUM + (AA(I,J) - FN2*A(I,J))**2                       
 6       CONTINUE                                                       
 5    CONTINUE                                                          
C                                                                       
C   PRINT ROOT MEAN SQUARE ERROR                                        
C                                                                       
      RMSERR = SQRT(SUM*FN2)                                            
      IWRITE   = I1MACH(2)                                              
      WRITE(IWRITE,1000) N,N,RMSERR                                     
 1000 FORMAT(1X,5H FOR ,I3,1HX,I3,20H ARRAY, RMS ERROR = ,1PE12.3)      
      STOP                                                              
      END                                                               
C$TEST MFTG                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM MFTCR                             
C                                                                       
C***********************************************************************
      REAL A(200,100)                                                   
      REAL AA(200,100)                                                  
      REAL TX(200),TY(200)                                              
      REAL RSTAK(3212)                                                  
      REAL FN2,RMSERR,SIGN,SUM                                          
      INTEGER IFX(25),IFY(25)                                           
      INTEGER I,J,N,N2,NP2,NP3,N2MK,NNS0,NNS,NSEGS                      
      DOUBLE PRECISION DSTAK(1606)                                      
      COMMON /CSTAK/DSTAK                                               
      EQUIVALENCE (RSTAK(1),DSTAK(1))                                   
C                                                                       
C                                                                       
      CALL ISTKIN(3212,3)                                               
C                                                                       
      N   = 100                                                         
      NP2 = 102                                                         
      N2  = 200                                                         
C                                                                       
C   THE SEGMENT SIZE IS ARBITRARILY CHOSEN TO BE 16 X N - I.E. USING    
C   MFTCC, MFTRC, MFTCR TO COMPUTE UP TO 16 INDEPENDENT VECTORS AT A    
C   TIME.                                                               
C                                                                       
      NSEGS = (N-1)/16 + 1                                              
      NNS0  = MOD(N-1,16) + 1                                           
C                                                                       
C   EXAMPLE USES RANDOM INPUT DATA                                      
C                                                                       
      DO 1 J = 1,N                                                      
         DO 2 I = 1,N                                                   
            A(I,J)  = UNI(0)                                            
            AA(I,J) = A(I,J)                                            
 2       CONTINUE                                                       
 1    CONTINUE                                                          
C                                                                       
      SIGN = 1.0E0                                                      
      CALL MFTRI(N,IFX,TX)                                              
      CALL MFTCI(N,IFY,TY)                                              
C                                                                       
C  X-DIMENSION                                                          
C                                                                       
      NNS = NNS0                                                        
      L   = 1                                                           
      DO 3 LL = 1,NSEGS                                                 
         CALL MFTRC(N,NNS,A(1,L),1,N2,A(1,L),A(2,L),2,N2,IFX,TX,SIGN)   
         L = L + NNS                                                    
         NNS = 16                                                       
 3    CONTINUE                                                          
C                                                                       
C  FILL-IN FROM CONJUGATION OF TERMS                                    
C                                                                       
      NP3  = N+3                                                        
      N2MK = N-1                                                        
      DO 4 I = NP3,N2,2                                                 
         DO 5 J = 1,N                                                   
            A(I,J)   = A(N2MK,J)                                        
            A(I+1,J) = - A(N2MK+1,J)                                    
 5       CONTINUE                                                       
         N2MK = N2MK - 2                                                
 4    CONTINUE                                                          
C                                                                       
C  DO COMPLEX PART IN Y-DIRECTION                                       
C                                                                       
      NNS = NNS0                                                        
      L   = 1                                                           
      DO 6 LL = 1,NSEGS                                                 
         CALL MFTCC(N,NNS,A(L,1),A(L+1,1),N2,2,                         
     *              A(L,1),A(L+1,1),N2,2,IFY,TY,SIGN)                   
         L   = L + 2*NNS                                                
         NNS = 16                                                       
 6    CONTINUE                                                          
C                                                                       
C  NOW GO BACKWARDS, COMPLEX TO COMPLEX FIRST                           
C                                                                       
      SIGN = -1.0E0                                                     
      NNS  = NNS0                                                       
      L    = 1                                                          
      DO 7 LL = 1,NSEGS                                                 
         CALL MFTCC(N,NNS,A(L,1),A(L+1,1),N2,2,                         
     *              A(L,1),A(L+1,1),N2,2,IFY,TY,SIGN)                   
         L   = L + 2*NNS                                                
         NNS = 16                                                       
 7    CONTINUE                                                          
C                                                                       
C  AND BACK TO REAL                                                     
C                                                                       
      NNS  = NNS0                                                       
      L    = 1                                                          
      DO 8 LL = 1,NSEGS                                                 
         CALL MFTCR(N,NNS,A(1,L),A(2,L),2,N2,A(1,L),1,N2,IFX,TX,SIGN)   
         L   = L + NNS                                                  
         NNS = 16                                                       
 8    CONTINUE                                                          
C                                                                       
C  COMPARE TO INPUT                                                     
C                                                                       
      FN2 = 1./FLOAT(N*N)                                               
      SUM = 0.0E0                                                       
      DO 9 J = 1,N                                                      
         DO 10 I = 1,N                                                  
            SUM = SUM + (AA(I,J) - FN2*A(I,J))**2                       
 10      CONTINUE                                                       
 9    CONTINUE                                                          
C                                                                       
C   PRINT ROOT MEAN SQUARE ERROR                                        
C                                                                       
      RMSERR = SQRT(SUM*FN2)                                            
      IWRITE   = I1MACH(2)                                              
      WRITE(IWRITE,1000) N,N,RMSERR                                     
 1000 FORMAT(1X,5H FOR ,I3,1HX,I3,20H ARRAY, RMS ERROR = ,1PE12.3)      
      STOP                                                              
      END                                                               
C$TEST FFTA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM FFTR                              
C                                                                       
C***********************************************************************
      REAL A(34),B(17),C(32)                                            
C                                                                       
C SET UP THE INPUT DATA FOR E**(-T)                                     
C AND SAVE IT IN THE VECTOR C FOR LATER COMPARISON.                     
C                                                                       
      A(1) = .5                                                         
      C(1) = A(1)                                                       
      DO 10 K=2,32                                                      
      A(K) = EXP(-.25*FLOAT(K-1))                                       
  10  C(K) = A(K)                                                       
C                                                                       
C CALL FOR THE TRANSFORM AND PRINT THE FOURIER COEFFICIENTS             
C                                                                       
      CALL FFTR(34,A,B)                                                 
C                                                                       
      IWRITE = I1MACH(2)                                                
      WRITE (IWRITE, 997)                                               
  997 FORMAT (1X,9HFREQUENCY,5X,20HFOURIER COEFFICIENTS//)              
      WRITE (IWRITE, 998)                                               
  998 FORMAT (1X,7H(=N/NT),5X,9HREAL PART,6X,9HIMAGINARY///)            
C                                                                       
      ENT = 1.0/(32. * 0.25)                                            
      DO 20 K=1,17                                                      
      FREQ = FLOAT(K-1) * ENT                                           
  20  WRITE (IWRITE,98) FREQ, A(K), B(K)                                
  98  FORMAT (2X,F6.3,2F15.8)                                           
C                                                                       
C DO THE INVERSE TRANSFORM                                              
C                                                                       
      CALL FFTRI(32,A,B)                                                
C                                                                       
C SCALE THE RESULTS, FIND THE ERROR, AND PRINT                          
C                                                                       
      WRITE (IWRITE, 999)                                               
  999 FORMAT (///4X,1HT,9X,5HINPUT,10X,5HERROR//)                       
C                                                                       
      EN  = 4*16                                                        
      ENI = 1./EN                                                       
      DO 30 K=1,32                                                      
      A(K) = ENI*A(K)                                                   
      ERR  = A(K) - C(K)                                                
      T = .25*FLOAT(K-1)                                                
  30  WRITE (IWRITE,99) T,C(K),ERR                                      
  99  FORMAT (2X,F4.2,1X,F15.8,4X,1PE10.2)                              
C                                                                       
      STOP                                                              
      END                                                               
C$TEST FFTC                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM FFTC                              
C                                                                       
C***********************************************************************
      REAL A(32),B(32),C(32),D(32)                                      
C                                                                       
C THE REAL DATA IS ALL ZERO AND THE                                     
C IMAGINARY PART IS E**(-T).                                            
C                                                                       
C SAVE THE IMAGINARY DATA IN THE VECTOR C FOR LATER COMPARISON          
C                                                                       
      DO 5 K=1,32                                                       
   5  A(K) = 0.                                                         
C                                                                       
      B(1) = .5                                                         
      C(1) = B(1)                                                       
      DO 10 K=2,32                                                      
      B(K) = EXP(-.25*FLOAT(K-1))                                       
  10  C(K) = B(K)                                                       
C                                                                       
C CALL FOR THE TRANSFORM AND PRINT THE FOURIER COEFFICIENTS             
C                                                                       
      CALL FFTC(32,A,B)                                                 
C                                                                       
      IWRITE = I1MACH(2)                                                
      WRITE (IWRITE, 997)                                               
  997 FORMAT (1X,9HFREQUENCY,5X,20HFOURIER COEFFICIENTS//)              
      WRITE (IWRITE, 998)                                               
  998 FORMAT (1X,7H(=N/NT),5X,9HREAL PART,6X,9HIMAGINARY///)            
C                                                                       
      ENT = 1.0/(32. * 0.25)                                            
      DO 20 K=1,32                                                      
      FREQ = FLOAT(K-1) * ENT                                           
      IF (FREQ .GT. 2.) FREQ = -4.0 + FREQ                              
  20  WRITE (IWRITE,98) FREQ, A(K), B(K)                                
  98  FORMAT (2X,F6.3,2F15.8)                                           
C                                                                       
C DO THE INVERSE TRANSFORM                                              
C                                                                       
      CALL FFTCI(32,A,B)                                                
C                                                                       
C SCALE THE RESULTS, FIND THE ERROR, AND PRINT                          
C                                                                       
      WRITE (IWRITE, 999)                                               
  999 FORMAT (///4X,1HT,7X,18HERROR IN REAL PART,                       
     1    4X,23HERROR IN IMAGINARY PART//)                              
C                                                                       
      ENI = 1./FLOAT(32)                                                
      DO 30 K=1,32                                                      
      A(K) = ENI*A(K)                                                   
      B(K) = ENI*B(K)                                                   
      ERR1  = A(K) - 0.0                                                
      ERR2  = B(K) - C(K)                                               
      T = .25*FLOAT(K-1)                                                
  30  WRITE (IWRITE,99) T,ERR1,ERR2                                     
  99  FORMAT (2X,F4.2,8X,1PE10.2,14X,1PE10.2)                           
C                                                                       
      STOP                                                              
      END                                                               
C$TEST LNAB                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM LSTSQ                             
C                                                                       
C***********************************************************************
      REAL X(10,2), Y(10), C(2), XX(10,2), YY(10)                       
C                                                                       
C  SET THE FIRST COLUMN OF THE X ARRAY TO THE ACTUAL X,                 
C  AND THE SECOND COLUMN TO 1.0                                         
C                                                                       
      DO 10  K=1,6                                                      
      X(K,1) = FLOAT(K)                                                 
 10   X(K,2) = 1.                                                       
C                                                                       
C  SET THE VALUES OF THE RIGHT-HAND SIDE, Y                             
C                                                                       
      Y(1) =  .3                                                        
      Y(2) =  .95                                                       
      Y(3) = 2.6                                                        
      Y(4) = 2.5                                                        
      Y(5) = 2.3                                                        
      Y(6) = 3.95                                                       
C                                                                       
C  SINCE LSTSQ WRITES OVER THE ARRAYS X AND Y,                          
C  SAVE THEM FOR LATER DEMONSTRATION COMPUTATION.                       
C                                                                       
      DO 15 K=1,6                                                       
      YY(K) = Y(K)                                                      
      DO 15 J=1,2                                                       
 15   XX(K,J)=X(K,J)                                                    
C                                                                       
      CALL LSTSQ (10,2,6,2,X,Y,1,C)                                     
C                                                                       
      IWRITE = I1MACH(2)                                                
      WRITE(IWRITE,97) C(1), C(2)                                       
 97   FORMAT (8H0C(1) = ,E16.8, 11H    C(2) = ,E16.8)                   
C                                                                       
C  COMPUTE THE SUM OF THE SQUARES OF THE ERROR                          
C  USING BRUTE FORCE.                                                   
C                                                                       
      ERR = 0.                                                          
      DO 20 J=1,6                                                       
      ADD = (C(1)*XX(J,1)+C(2)-YY(J))**2                                
 20   ERR = ERR + ADD                                                   
C                                                                       
      WRITE(IWRITE,98) ERR                                              
 98   FORMAT(35H0LEAST-SQUARES ERROR (VERSION 1) = ,E16.8)              
C                                                                       
C  COMPUTE THE LEAST-SQUARES ERROR USING THE PROGRAM SOLUTION.          
C                                                                       
      ERR = 0.                                                          
      DO 30 L=3,6                                                       
      ERR = ERR + Y(L)*Y(L)                                             
 30   CONTINUE                                                          
C                                                                       
      WRITE(IWRITE,99) ERR                                              
 99   FORMAT(35H0LEAST-SQUARES ERROR (VERSION 2) = ,E16.8)              
C                                                                       
      STOP                                                              
      END                                                               
C$TEST BURA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BURAM                             
C                                                                       
C***********************************************************************
      INTEGER IWRITE,I,M,N,NPTS                                         
      REAL XMESH(11), F(11), P(3), Q(3), DELTA, STEP, X, XL, XR,        
     1   ERROR(11), TCHBP                                               
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      NPTS = 11                                                         
      M = 2                                                             
      N = 2                                                             
      XL = -1.0E0                                                       
      XR = 1.0E0                                                        
      STEP = (XR-XL)/FLOAT(10)                                          
C                                                                       
      DO 10 I=1,11                                                      
         XMESH(I) = XL + FLOAT(I-1)*STEP                                
         F(I) = EXP(XMESH(I))                                           
  10  CONTINUE                                                          
C                                                                       
C                                                                       
C   COMPUTE THE APPROXIMATION.                                          
C                                                                       
      CALL BURAM(NPTS, XMESH, F, M, N, P, Q, DELTA)                     
C                                                                       
C   PRINT OUT THE ERRORS.                                               
C                                                                       
      WRITE (IWRITE,99)                                                 
  99  FORMAT (7H   MESH, 4X, 3HEXP, 7X, 5HERROR)                        
      DO 20 I=1,NPTS                                                    
         X = XMESH(I)                                                   
C                                                                       
C   NOTE THAT TO EVALUATE THE APPROXIMATION WE MUST USE THE             
C   FUNCTION TCHBP, WHICH EVALUATES A POLYNOMIAL GIVEN IN               
C   TERMS OF ITS TCHEBYCHEFF EXPANSION.                                 
C                                                                       
         ERROR(I) = F(I) - TCHBP(M,P,X,XL,XR)/TCHBP(N,Q,X,XL,XR)        
         WRITE (IWRITE,98) XMESH(I), F(I), ERROR(I)                     
  98     FORMAT (2F8.4,1PE12.2)                                         
  20  CONTINUE                                                          
      STOP                                                              
      END                                                               
C$TEST BURB                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BURM1                             
C                                                                       
C***********************************************************************
      INTEGER IWRITE,I,ITOL,MAXITR                                      
      REAL XMESH(11), F(11), P(3), Q(3), DELTA, STEP, X, XL, XR,        
     1 ERR1(11), ERR2(11), TCHBP                                        
C                                                                       
      DATAP(1)/ 25.0/,  P(2)/ 12.0/,  P(3)/ 1.0/                        
      DATAQ(1)/ 25.0/,  Q(2)/-12.0/,  Q(3)/ 1.0/                        
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      XL = -1.0E0                                                       
      XR = 1.0E0                                                        
      STEP = (XR-XL)/FLOAT(10)                                          
      DO 10 I=1,11                                                      
         XMESH(I) = XL + FLOAT(I-1)*STEP                                
         F(I) = EXP(XMESH(I))                                           
  10  CONTINUE                                                          
C                                                                       
C    COMPUTE THE ERROR IN THE INITIAL APPROXIMATION.                    
C    USE THE FUNCTION TCHBP TO EVALUATE A POLYNOMIAL                    
C    GIVEN IN TERMS OF ITS TCHEBYCHEFF EXPANSION.                       
C                                                                       
      DO 20 I=1,11                                                      
         X = XMESH(I)                                                   
  20     ERR1(I) = F(I) - TCHBP(2,P,X,XL,XR)/TCHBP(2,Q,X,XL,XR)         
C                                                                       
C    COMPUTE THE APPROXIMATION.  USE NO MORE THAN 10 ITERATIONS         
C    AND STOP WHEN THE EXTREMALS AGREE TO 10 PER CENT.                  
C                                                                       
      MAXITR = 10                                                       
      ITOL = 1                                                          
      CALL BURM1(11, XMESH, F, MAXITR, ITOL, 2, 2, P, Q, DELTA)         
C                                                                       
C    PRINT OUT THE ERRORS.                                              
C                                                                       
      WRITE (IWRITE,99)                                                 
  99  FORMAT (7H   MESH, 4X, 3HEXP, 7X, 4HERR1, 8X, 4HERR2)             
      DO 30 I=1,11                                                      
         X = XMESH(I)                                                   
         ERR2(I) = F(I) - TCHBP(2,P,X,XL,XR)/TCHBP(2,Q,X,XL,XR)         
         WRITE (IWRITE,98) XMESH(I), F(I), ERR1(I), ERR2(I)             
  98      FORMAT (2F8.4,1P2E12.2)                                       
  30  CONTINUE                                                          
      STOP                                                              
      END                                                               
C$TEST VDSA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM VDSS1                             
C                                                                       
C***********************************************************************
C  SIN(X) + X**2 + 3                                                    
C                                                                       
       REAL X, A(10), F, FPRIME, PT, H, SOL                             
       INTEGER IWRITE, N, I                                             
       REAL U, L, DIST, DSOL                                            
C                                                                       
       IWRITE = I1MACH(2)                                               
       N = 10                                                           
       X = 1.3                                                          
       H = 1./ FLOAT(N-1)                                               
       L = 1.0                                                          
       U = 5.0                                                          
       DIST = U - L                                                     
C SET UP THE MESH AND DATA VALUES                                       
       DO 100 I=1,N                                                     
              PT = L + DIST*FLOAT(I-1)*H                                
              A(I) = SIN(PT) + PT**2 + 3.0                              
 100   CONTINUE                                                         
       CALL VDSS1 (X,N,U,L,A,F,FPRIME)                                  
C CHECK THE SOLUTION                                                    
       SOL = SIN(X) + X**2 + 3.0                                        
       DSOL = COS(X) + 2.0*X                                            
       WRITE (IWRITE,101)                                               
 101   FORMAT (45H                     ACTUAL          COMPUTED//)      
       WRITE (IWRITE,102) SOL,F                                         
 102   FORMAT (17H          F(X) = ,2E16.8)                             
       WRITE (IWRITE,103) DSOL,FPRIME                                   
 103   FORMAT (17H    DERIVATIVE = ,2E16.8)                             
       STOP                                                             
       END                                                              
C$TEST VDSB                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM VDSS2                             
C                                                                       
C***********************************************************************
C  SIN(X(1)) + X(2)**2 + 3                                              
C                                                                       
       REAL X(2), A(10,10), F, FPRIME(2), PT1, PT2, H1, H2, SOL         
       REAL DSOL1, DSOL2                                                
       INTEGER IWRITE, N1, N2, NA1, I, J                                
       REAL U(2),L(2),DIST(2)                                           
C                                                                       
       IWRITE = I1MACH(2)                                               
       N1 = 10                                                          
       N2 = 10                                                          
       X(1) = 1.7                                                       
       X(2) = 1.7                                                       
       H1 = 1./ FLOAT(N1-1)                                             
       H2 = 1./ FLOAT(N2-1)                                             
       L(1) = 1.0                                                       
       U(1) = 5.0                                                       
       L(2) = 1.0                                                       
       U(2) = 5.0                                                       
       DIST(1) = U(1) - L(1)                                            
       DIST(2) = U(2) - L(2)                                            
C SET UP THE MESH AND DATA VALUES                                       
       DO 100 I=1,N1                                                    
              PT1 = L(1) + DIST(1)*FLOAT(I-1)*H1                        
              DO 100 J=1,N2                                             
                     PT2 = L(2) + DIST(2)*FLOAT(J-1)*H2                 
                     A(I,J) = SIN(PT1) + PT2**2 + 3.0                   
 100   CONTINUE                                                         
       NA1 = 10                                                         
       CALL VDSS2 (X,N1,N2,U,L,A,NA1,F,FPRIME)                          
C CHECK THE SOLUTION                                                    
       SOL = SIN(X(1)) + X(2)**2 + 3.0                                  
       DSOL1 = COS(X(1))                                                
       DSOL2 = 2.0*X(2)                                                 
       WRITE (IWRITE,101)                                               
 101   FORMAT (45H                     ACTUAL          COMPUTED//)      
       WRITE (IWRITE,102) SOL,F                                         
 102   FORMAT (17H          F(X) = ,2E16.8)                             
       WRITE (IWRITE,103) DSOL1,FPRIME(1)                               
 103   FORMAT (17H     PARTIAL X = ,2E16.8)                             
       WRITE (IWRITE,104) DSOL2,FPRIME(2)                               
 104   FORMAT (17H     PARTIAL Y = ,2E16.8)                             
       STOP                                                             
       END                                                              
C$TEST VDSE                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM VDSS3                             
C                                                                       
C***********************************************************************
C  X(1)*X(2) + EXP(X(2)) + X(3)**2                                      
C                                                                       
       REAL X(3), A(10,10,10), F, FPRIME(3), PT1, PT2, PT3              
       REAL H1, H2, H3, SOL                                             
       REAL DSOL1, DSOL2, DSOL3                                         
       INTEGER IWRITE, N1, N2, N3, NA1, NA2                             
       INTEGER I,J,K                                                    
       REAL U(3),L(3),DIST(3)                                           
C                                                                       
       IWRITE = I1MACH(2)                                               
       N1 = 10                                                          
       N2 = 10                                                          
       N3 = 10                                                          
       X(1) = 2.3                                                       
       X(2) = 2.3                                                       
       X(3) = 2.3                                                       
       H1 = 1./ FLOAT(N1-1)                                             
       H2 = 1./ FLOAT(N2-1)                                             
       H3 = 1./ FLOAT(N3-1)                                             
       L(1) = 1.0                                                       
       U(1) = 3.0                                                       
       L(2) = 2.0                                                       
       U(2) = 4.0                                                       
       L(3) = 1.5                                                       
       U(3) = 3.5                                                       
       DIST(1) = U(1) - L(1)                                            
       DIST(2) = U(2) - L(2)                                            
       DIST(3) = U(3) - L(3)                                            
C SET UP THE MESH AND DATA VALUES                                       
       DO 100 I=1,N1                                                    
              PT1 = L(1) + DIST(1)*FLOAT(I-1)*H1                        
              DO 100 J=1,N2                                             
                     PT2 = L(2) + DIST(2)*FLOAT(J-1)*H2                 
                     DO 100 K=1,N3                                      
                            PT3 = L(3) + DIST(3)*FLOAT(K-1)*H3          
                            A(I,J,K) = PT1*PT2 + EXP(PT2) + PT3**2      
 100   CONTINUE                                                         
       NA1 = 10                                                         
       NA2 = 10                                                         
       CALL VDSS3 (X,N1,N2,N3,U,L,A,NA1,NA2,F,FPRIME)                   
C CHECK THE SOLUTION                                                    
       SOL = X(1)*X(2) + EXP(X(2)) + X(3)**2                            
       DSOL1 = X(2)                                                     
       DSOL2 = X(1) + EXP(X(2))                                         
       DSOL3 = 2.0*X(3)                                                 
       WRITE (IWRITE,101)                                               
 101   FORMAT (45H                     ACTUAL          COMPUTED//)      
       WRITE (IWRITE,102) SOL,F                                         
 102   FORMAT (17H          F(X) = ,2E16.8)                             
       WRITE (IWRITE,103) DSOL1,FPRIME(1)                               
 103   FORMAT (17H     PARTIAL X = ,2E16.8)                             
       WRITE (IWRITE,104) DSOL2,FPRIME(2)                               
 104   FORMAT (17H     PARTIAL Y = ,2E16.8)                             
       WRITE (IWRITE,105) DSOL3,FPRIME(3)                               
 105   FORMAT (17H     PARTIAL Z = ,2E16.8)                             
       STOP                                                             
       END                                                              
C$TEST PRSA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SPMLE                             
C                                                                       
C***********************************************************************
      INTEGER JCOL(5000),ISTAK(18000),IROW(626), IREAD, IWRITE, M       
      INTEGER I1MACH, I, J, K, L, ISIZE, MP1                            
      REAL AROW(5000), B(625), SASUM, A1                                
      COMMON /CSTAK/ ISTAK                                              
      CALL ISTKIN(18000,2)                                              
      IREAD=I1MACH(1)                                                   
      IWRITE=I1MACH(2)                                                  
  10  READ(IREAD,11)M,A1                                                
  11  FORMAT(I3,E15.5)                                                  
      IF (M .EQ. 0) STOP                                                
      MP1=M+1                                                           
      N= MP1*MP1                                                        
C                                                                       
C SET UP MATRIX OF SIZE N                                               
C                                                                       
      IROW(1) = 1                                                       
      K=1                                                               
      L=0                                                               
      DO 70 I=1,MP1                                                     
         DO 60 J = 1, MP1                                               
            L=L+1                                                       
            AROW(K) = -2.0*A1 - FLOAT(I+J-2)                            
            JCOL(K) = L                                                 
            K=K+1                                                       
            IF (J .EQ. 1) GO TO 20                                      
               AROW(K) = A1                                             
               JCOL(K) = L - 1                                          
               K=K+1                                                    
  20        IF (J .EQ. MP1) GO TO 30                                    
               AROW(K) = J                                              
               JCOL(K) = L+1                                            
               K=K+1                                                    
  30        IF (I.EQ.1) GO TO 40                                        
               AROW(K) = A1                                             
               JCOL(K) = L - MP1                                        
               K=K+1                                                    
  40        IF (I.EQ.MP1) GO TO 50                                      
               AROW(K) = I                                              
               JCOL(K) = L+MP1                                          
               K=K+1                                                    
  50        IROW(L+1)=K                                                 
  60     CONTINUE                                                       
  70  CONTINUE                                                          
C                                                                       
C SET UP RIGHT HAND SIDE AND LAST ROW OF THE MATRIX                     
C                                                                       
      L=IROW(N)                                                         
      DO 80 I=1,N                                                       
         AROW(L)=1.0                                                    
         JCOL(L)=I                                                      
         L=L+1                                                          
         B(I)=0.0                                                       
  80  CONTINUE                                                          
      IROW(N+1)=L                                                       
      B(N)=1.0                                                          
C                                                                       
C SOLVE THE SYSTEM                                                      
C                                                                       
      CALL SPMLE(N,.TRUE.,IROW,JCOL,AROW,ISIZE,B,625,1)                 
C                                                                       
C PRINT RESULTS                                                         
C                                                                       
      WRITE(IWRITE,81)N,L                                               
  81  FORMAT(/19HNO. OF EQUATIONS = ,I3,19HNO. OF NONZEROES = ,I5)      
      WRITE(IWRITE,82)ISIZE                                             
  82  FORMAT(9H ISIZE = , I5)                                           
      WRITE(IWRITE,83)B(N),SASUM(M,B(M),M)                              
  83  FORMAT(6H L1 = ,E15.5,6H L2 = ,E15.5)                             
      GO TO 10                                                          
      END                                                               
C$TEST PRSF                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SPFLE                             
C                                                                       
C***********************************************************************
       INTEGER N1, N2, N, I, ITIME, ITIME1, ITIME2, ILAPSZ              
       INTEGER I1MACH, IREAD, IWRITE, ISTAK(25000), ISIZE, ISIZE2       
       REAL RSTAK(25000), B(500), A1, A2                                
       EXTERNAL QUEA                                                    
       COMMON /QUE/ A1, A2, N1, N2, N                                   
       COMMON /CSTAK/ ISTAK                                             
       EQUIVALENCE (ISTAK(1),RSTAK(1))                                  
       IREAD = I1MACH(1)                                                
       IWRITE = I1MACH(2)                                               
       CALL ISTKIN(25000,2)                                             
  10   READ(IREAD, 11)N1, N2, A1, A2                                    
  11   FORMAT(2I3,2E15.5)                                               
       IF (N1 .EQ. 0) STOP                                              
       WRITE(IWRITE,12)N1, N2, A1, A2                                   
  12   FORMAT(/4H N1=,I3,4H N2=,I3,4H A1=,E15.5,4H A2=,E15.5)           
       N = (N1+1)*(N2+1)                                                
C                                                                       
C GENERATE THE RIGHT HAND SIDE                                          
C                                                                       
       DO 20 I=1, N                                                     
          B(I) = 0.0                                                    
  20   CONTINUE                                                         
       B(N) = 1.0                                                       
C                                                                       
C SOLVE THE SYSTEM WITH PIVOTING FOR SPARSITY AND TIME IT               
C                                                                       
       ITIME=ILAPSZ(0)                                                  
       CALL SPFLE(N, .TRUE., QUEA, ISIZE, B, 500, 1)                    
       ITIME1=ILAPSZ(0)-ITIME                                           
C                                                                       
C FIND THE PROBABILITIES                                                
C                                                                       
       IT = (N2+1)*N1+1                                                 
       WRITE(IWRITE,21)B(N),SASUM(N1+1,B(N2+1),N2+1),SASUM(N2,B(IT),1)  
  21   FORMAT(6H L1 = ,E15.5,6H L2 = ,E15.5,7H I12 = ,E15.5)            
       WRITE(IWRITE,22)                                                 
  22   FORMAT(22H PIVOTING FOR SPARSITY)                                
       WRITE(IWRITE,23)ISIZE                                            
  23   FORMAT(34H SPACE NEEDED FOR DECOMPOSITION - ,I8)                 
       WRITE(IWRITE,24)ITIME1                                           
  24   FORMAT(15H TIME NEEDED - ,I8)                                    
C                                                                       
C FOR COMPARISON, REDO PROBLEM WITHOUT REQUESTING PIVOTING              
C                                                                       
      DO 30 I= 1,N                                                      
         B(I)=0.0                                                       
  30  CONTINUE                                                          
      B(N)=1.0                                                          
      ITIM=ILAPSZ(0)                                                    
      CALL SPFLE(N,.FALSE.,QUEA, ISIZE2, B, 500, 1)                     
      ITIME2=ILAPSZ(0)-ITIME                                            
      WRITE(IWRITE, 31)                                                 
  31  FORMAT(25H NO PIVOTING FOR SPARSITY)                              
      WRITE(IWRITE,23)ISIZE2                                            
      WRITE(IWRITE, 24)ITIME2                                           
       GO TO 10                                                         
       END                                                              
       SUBROUTINE QUEA(I, ROW, JCOL, NUM)                               
       INTEGER I, NUM, JCOL(100), N, N1, N2, II, JJ, J                  
       REAL ROW(100)                                                    
       COMMON /QUE/ A1,A2, N1,N2, N                                     
       IF (I.NE.N) GO TO 20                                             
C TREAT LAST ROW AS SPECIAL CASE                                        
       DO 10 J=1, N                                                     
          JCOL(J) = J                                                   
          ROW(J) = 1.0                                                  
  10   CONTINUE                                                         
       NUM = N                                                          
       RETURN                                                           
  20   N2P1=N2+1                                                        
C DETERMINE WHICH MAJOR BLOCK                                           
       II=(I-1)/N2P1                                                    
C DETERMINE POSITION IN BLOCK                                           
       JJ = MOD(I-1, N2P1)                                              
C FILL IN DIAGONAL                                                      
       JCOL(1) = I                                                      
       ROW(1) = -A1-A2-FLOAT(II+JJ)                                     
       IF (JJ.EQ.N2) ROW(1)=ROW(1)+A2                                   
       NUM = 1                                                          
       IF (II .EQ. 0) GO TO 30                                          
C THIS IS NOT THE FIRST BLOCK                                           
       JCOL(2) = I-N2P1                                                 
       ROW(2) = A1                                                      
       NUM = 2                                                          
  30   IF (JJ.EQ.0) GO TO 40                                            
C THIS IS NOT FIRST ROW IN THE BLOCK                                    
       NUM = NUM+1                                                      
       JCOL(NUM) = I-1                                                  
       ROW(NUM) = A2                                                    
  40   IF (JJ.EQ. N2) GO TO 50                                          
C THIS IS NOT LAST ROW IN THE BLOCK                                     
       NUM=NUM+1                                                        
       JCOL(NUM)= I+1                                                   
       ROW(NUM) = JJ+1                                                  
  50   IF (II .EQ. N1) RETURN                                           
C THIS IS NOT THE LAST BLOCK                                            
       NUM = NUM +1                                                     
       JCOL(NUM) = I+N2P1                                               
       ROW(NUM)= II+1                                                   
       RETURN                                                           
       END                                                              
      INTEGER FUNCTION ILAPSZ(N)                                        
      INTEGER N                                                         
      ILAPSZ = 0                                                        
      RETURN                                                            
      END                                                               
C                                                                       
C DATA FOR THE EXAMPLE IN THE PORT SHEET...  (REMOVE THE C              
C IN COLUMN 1 BEFORE FEEDING THIS DATA TO THE PROGRAM ABOVE.)           
C$DATA                                                                  
C10 10   9.             9.                                              
C20 20  19.            19.                                              
C 0  0   0.             0.                                              
C$TEST PRSJ                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SPMCE                             
C                                                                       
C***********************************************************************
      INTEGER JCOL(10000),IROW(626), IREAD, IWRITE, M                   
      INTEGER I1MACH, I, J, K, L, ISIZE, MP1                            
      INTEGER MRP(625), MCP(625), IL(625)                               
      REAL AROW(10000), A1, Z(625)                                      
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(3000)                                          
      CALL ISTKIN(3000,4)                                               
      IREAD=I1MACH(1)                                                   
      IWRITE=I1MACH(2)                                                  
  10  READ(IREAD,11)M,A1                                                
  11  FORMAT(I3,E15.5)                                                  
      IF (M .EQ. 0) STOP                                                
      MP1=M+1                                                           
      N= MP1*MP1-1                                                      
C                                                                       
C SET UP MATRIX OF SIZE N                                               
C                                                                       
      IROW(1) = 1                                                       
      K=1                                                               
      L=0                                                               
      DO 70 I=1,MP1                                                     
         DO 60 J = 1, MP1                                               
            L=L+1                                                       
            AROW(K) = -2.0*A1 - FLOAT(I+J-2)                            
            IF (J. EQ. MP1)AROW(K)=AROW(K) + A1                         
            JCOL(K) = L                                                 
            K=K+1                                                       
            IF (J .EQ. 1) GO TO 20                                      
               AROW(K) = A1                                             
               JCOL(K) = L-1                                            
               K=K+1                                                    
  20        IF (J .EQ. MP1 .OR. J.EQ.M. AND .I.EQ.MP1) GO TO 30         
               AROW(K) = J                                              
               JCOL(K) = L+1                                            
               K=K+1                                                    
  30        IF (I.EQ.1) GO TO 40                                        
               AROW(K) = A1                                             
               JCOL(K) = L-MP1                                          
               K=K+1                                                    
  40        IF (I.EQ.MP1.OR.J.EQ.MP1. AND. I.EQ.M) GO TO 50             
               AROW(K) = I                                              
               JCOL(K) = L+MP1                                          
               K=K+1                                                    
  50        IROW(L+1)=K                                                 
  60     CONTINUE                                                       
  70  CONTINUE                                                          
C                                                                       
C REORDER ROWS OF THE MATRIX                                            
C                                                                       
      CALL SPMOR(N,IROW,JCOL,MRP,MCP)                                   
C                                                                       
C SOLVE THE SYSTEM                                                      
C                                                                       
      CALL SPMCE(N,MRP,MCP,AROW,IROW,JCOL,10000,IL,ISIZE,COND,Z)        
C                                                                       
C PRINT RESULTS                                                         
C                                                                       
      WRITE(IWRITE,71)N,IROW(N+1)                                       
  71  FORMAT(/19HNO. OF EQUATIONS = ,I3,20H NO. OF NONZEROES = ,I5)     
      WRITE(IWRITE,72)A1,ISIZE                                          
  72  FORMAT(6H A1 = ,E15.5,9H ISIZE = , I5)                            
      WRITE(IWRITE,73)COND                                              
  73  FORMAT(16H CONDITION NO = ,E15.5)                                 
      GO TO 10                                                          
      END                                                               
C                                                                       
C DATA FOR THE EXAMPLE IN THE PORT SHEET...  (REMOVE THE C              
C IN COLUMN 1 BEFORE FEEDING THIS DATA TO THE PROGRAM ABOVE.)           
C$DATA                                                                  
C10   2.0                                                               
C20   2.0                                                               
C22   3.0                                                               
C 0   0.0                                                               
C$TEST PRSM                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SPFCE                             
C                                                                       
C***********************************************************************
       INTEGER N1, N2, N, MCP(500), MRP(500), MAXIW, IW, I              
       INTEGER I1MACH, IREAD, IWRITE, TEMP, ISTAK(20000)                
       REAL A1, RSTAK(20000)                                            
       DOUBLE PRECISION DSTAK(10000)                                    
       EXTERNAL QUEI, QUEA                                              
       COMMON /QUE/ A1, K                                               
       COMMON /CSTAK/ DSTAK                                             
       EQUIVALENCE(RSTAK(1),DSTAK(1),ISTAK(1))                          
       CALL ISTKIN(10000,4)                                             
       IREAD = I1MACH(1)                                                
       IWRITE = I1MACH(2)                                               
       CALL ISTKIN(20000, 2)                                            
       READ(IREAD,11)K                                                  
  11   FORMAT(I3)                                                       
       IF (K. EQ. 0) STOP                                               
       N = K*K - 1                                                      
       WRITE(IWRITE,12)N                                                
  12   FORMAT(20H NO. OF EQUATIONS = ,I3)                               
C                                                                       
C DETERMINE THE ORDERING                                                
C                                                                       
       CALL SPFOR(N, QUEI, MRP)                                         
C                                                                       
C GET THE WORK SPACE FROM THE STORAGE STACK                             
C                                                                       
       MAXIW = (ISTKQU(2)-3*N-50)/2                                     
       IW = ISTKGT(MAXIW, 2)                                            
       IUL = ISTKGT(MAXIW, 3)                                           
C                                                                       
C READ IN PARAMETER                                                     
C                                                                       
  20   READ(IREAD, 21)A1                                                
  21   FORMAT(E15.7)                                                    
       IF (A1.EQ.0.0E0) STOP                                            
       WRITE(IWRITE, 22)A1                                              
  22   FORMAT(/4H A1=, E15.5)                                           
C                                                                       
C GET THE CONDITION NUMBER                                              
C                                                                       
       CALL SPFCE(N, MRP, MCP, QUEA, ISTAK(IW), RSTAK(IUL), MAXIW,      
     1 ISIZE, COND)                                                     
       WRITE(IWRITE,23)COND                                             
  23   FORMAT(20H CONDITION NUMBER = ,E15.5)                            
       GO TO 20                                                         
       END                                                              
C$TEST PRSP                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SPMLU                             
C                                                                       
C***********************************************************************
      INTEGER IA(401), JA(2500), ISTAK(18500), I1MACH, IWRITE, N        
      INTEGER MRP(400), MCP(400), ITEMP, INMCP(400)                     
      INTEGER ILAPSZ, IT, IT2, IT3, IT4, IT5, IT6, I, NUMBER            
      INTEGER NUM, IPOINT, NP1                                          
      REAL RSTAK(18500), A(2500), GROWTH                                
      DOUBLE PRECISION DSTAK(9250)                                      
      COMMON /CSTAK/DSTAK                                               
      EQUIVALENCE(ISTAK(1), RSTAK(1), DSTAK(1))                         
      IWRITE = I1MACH(2)                                                
      CALL ISTKIN(18500,2)                                              
      CALL ENTSRC(NEW,1)                                                
      DO 40 K = 9,19,5                                                  
         N = (K+1)*(K+1)                                                
         CALL SETUP(K, N, IA, JA, A)                                    
         NUMBER = IA(N+1)-1                                             
         WRITE(IWRITE,11)N,NUMBER                                       
  11     FORMAT(5H N = ,I4,22H NUMBER OF NONZEROS = ,I7)                
C                                                                       
C ORDER THE ROWS AND COLUMNS OF THE MATRIX                              
C TO DECREASE FILL-IN                                                   
C                                                                       
         CALL SPMOR(N, IA, JA, MRP, INMCP)                              
C ALLOCATE THE AVAILABLE SPACE FOR THE WORK SPACE IN SPMSF              
C BUT MAKE SURE THERE IS EMOUGH FOR SPMSF'S ALLOCATIONS                 
C                                                                       
         MAXIW = ISTKQU(2)-3*N-5                                        
         IW = ISTKGT(MAXIW,2)                                           
C                                                                       
C TIME THE SYMBOLIC FACTORIZATION                                       
C                                                                       
         IT = ILAPSZ(0)                                                 
         CALL SPMSF(N, MRP, INMCP, IA, JA, ISTAK(IW), MAXIW, ISIZE)     
         IT2= ILAPSZ(0)-IT                                              
         WRITE(IWRITE, 12)ISIZE                                         
  12     FORMAT(37H NUMBER OF NONZEROS IN DECOMPOSITION=,I5)            
         WRITE(IWRITE,13)IT2                                            
  13     FORMAT(23H ELAPSED TIME FOR SPMSF,I7)                          
C                                                                       
C MODIFY THE WORK STACK TO REFLECT THE AMOUNT NEEDED BY                 
C SPMSF AND ALLOCATE SPACE FOR THE NUMERICAL FACTORIZATION              
C                                                                       
         ISPAC= 2*N+2+ISIZE                                             
         IW = ISTKMD(ISPAC,2)                                           
         IUL = ISTKGT(ISIZE, 3)                                         
C                                                                       
C COMPUTE THE TIME NEEDED TO INSERT THE NUMERICAL ELEMENTS              
C IN THEIR PROPER PLACES                                                
C                                                                       
         IT3 = ILAPSZ(0)                                                
         DO 20 I=1, N                                                   
            MCP(I) = MRP(I)                                             
            IR = MRP(I)                                                 
            NUM = IA(IR+1)-IA(IR)                                       
            IPOINT = IA(IR)                                             
            CALL SPMIN(N, INMCP, ISTAK(IW), I, A(IPOINT),               
     1        JA(IPOINT), NUM, I, RSTAK(IUL))                           
  20     CONTINUE                                                       
         IT4 = ILAPSZ(0)-IT3                                            
         WRITE(IWRITE,21)IT4                                            
  21     FORMAT(23H ELAPSED TIME FOR SPMIN,I7)                          
C                                                                       
C TIME THE SUBROUTINE WHICH COMPUTES THE NUMERICAL                      
C FACTORIZATION                                                         
C                                                                       
         IT5 =ILAPSZ(0)                                                 
         CALL SPMNF(N, ISTAK(IW), RSTAK(IUL), 0.0, GROWTH)              
         IT6 =ILAPSZ(0)-IT5                                             
         WRITE(IWRITE, 22)IT6                                           
  22     FORMAT(23H ELAPSED TIME FOR SPMNF,I7)                          
         IT6 = IT2 + IT4 +IT6                                           
         WRITE(6,23)IT6                                                 
  23     FORMAT(26H ELAPSED TIME FOR SF-IN-NF,I7)                       
         NP1 = N+1                                                      
C                                                                       
C REDO THE FACTORIZATION WITH SPMLU AND TIME IT                         
C                                                                       
         CALL MOVEFR(NUMBER,A,RSTAK(IUL))                               
         CALL MOVEFI(NUMBER,JA,ISTAK(IW))                               
         IL = ISTKGT(N+1,2)                                             
         IT5 =ILAPSZ(0)                                                 
         CALL SPMLU(N, MRP, MCP, IA, ISTAK(IW), RSTAK(IUL), ISPAC,      
     1   ISTAK(IL), 0.0, 0.0, ISIZE, GROWTH)                            
         IT6 = ILAPSZ(0)-IT5                                            
         WRITE(IWRITE, 31)IT6                                           
  31     FORMAT(23H ELAPSED TIME FOR SPMLU, I7)                         
         CALL ISTKRL(3)                                                 
  40  CONTINUE                                                          
      STOP                                                              
      END                                                               
C$TEST PRST                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SPFLU                             
C                                                                       
C***********************************************************************
       INTEGER K, N, I1MACH, IWRITE, MAXUL, I, NEW, IREAD, IERR         
       INTEGER MRP(101), MCP(101), IWORK(5000), ISIZE, NERROR           
       DOUBLE PRECISION UL(5000), THRESH, EPS, GROWTH                   
       DOUBLE PRECISION B(101), ERROR, X                                
       EXTERNAL TOY                                                     
       COMMON /TOYS/ X, N, K                                            
       MAXUL = 4000                                                     
       IREAD = I1MACH(1)                                                
       IWRITE = I1MACH(2)                                               
C SET THE RECOVERY MODE                                                 
       CALL ENTSRC(NEW, 1)                                              
  10   READ(IREAD,11)K                                                  
  11   FORMAT(I2)                                                       
       IF (K .EQ. 0) STOP                                               
       N = K*K + 1                                                      
C                                                                       
       READ(IREAD,12)X, THRESH, EPS                                     
  12   FORMAT(3D10.2)                                                   
       WRITE(IWRITE,13)K, N, X, THRESH, EPS                             
  13   FORMAT(3H K=,I3,3H N=,I3,3H X=,D10.2,8H THRESH=,D10.2,           
     1 5H EPS=,D10.2)                                                   
C SET UP PERMUTATION VECTORS TO INDICATE NO PRIOR PIVOTING              
       DO 20 I=1,N                                                      
          MRP(I) = I                                                    
          MCP(I) = I                                                    
  20   CONTINUE                                                         
       CALL DSPFLU(N, MRP, MCP, TOY, IWORK, UL, MAXUL, THRESH, EPS,     
     1 ISIZE, GROWTH)                                                   
       IF (NERROR(IERR) .EQ. 0) GO TO 30                                
C                                                                       
C TEST FOR SINGULARITY                                                  
C                                                                       
          CALL ERROFF                                                   
          WRITE(IWRITE,21)                                              
  21      FORMAT(16H SINGULAR MATRIX)                                   
          GO TO 10                                                      
C                                                                       
  30   WRITE(IWRITE,31) ISIZE, GROWTH                                   
  31   FORMAT(7H ISIZE=,I5,8H GROWTH=,1PD25.15)                         
       CALL GETB(N, K, B, X)                                            
C                                                                       
C GENERATE THE RIGHT HAND SIDE AND SOLVE THE SYSTEM                     
C                                                                       
       CALL DSPFSL(N, MRP, MCP, IWORK, UL, B, N, 1)                     
       ERROR = 0.0D0                                                    
C                                                                       
C COMPUTE THE ERROR IN THE SOLUTION                                     
C                                                                       
       DO 40 I = 1, N                                                   
          ERROR = DMAX1(ERROR, DABS(B(I)-1.D0))                         
  40   CONTINUE                                                         
       WRITE(IWRITE,41)ERROR                                            
  41   FORMAT(19H ERROR IN SOLUTION=,1PD25.15)                          
       GO TO 10                                                         
       END                                                              
       SUBROUTINE TOY(I, ROW, JCOL, NUM)                                
       INTEGER I, NUM, JCOL(101)                                        
       INTEGER N, K, J, MODK                                            
       DOUBLE PRECISION ROW(101)                                        
       DOUBLE PRECISION X                                               
       COMMON /TOYS/ X, N, K                                            
       IF (I .LT. N) GO TO 20                                           
C LAST ROW                                                              
          DO 10 J=1,N                                                   
             ROW(J) = 1.D0                                              
             JCOL(J) = J                                                
  10      CONTINUE                                                      
          NUM = N                                                       
          RETURN                                                        
  20   JCOL(1) = I                                                      
       JCOL(2) = N                                                      
       ROW(1) = 2.D0                                                    
       ROW(2) = 1.D0                                                    
       MODK = MOD(I, K)                                                 
       JCOL(3) = I-1                                                    
       ROW(3) = -1.D0                                                   
       JCOL(4) = I+1                                                    
       ROW(4) = -1.D0                                                   
       NUM = 4                                                          
       IF (MODK .GT. 1) GO TO 30                                        
          ROW(1) = 1.D0 + X                                             
          IF (MODK .EQ. 1) JCOL(3) = I+1                                
          NUM = 3                                                       
  30   IF (I .LE. K) RETURN                                             
       IF ((I-1)/K .EQ. 1) GO TO 40                                     
       NUM = NUM + 1                                                    
       JCOL(NUM) = I-K                                                  
       ROW(NUM) = 1.D0                                                  
  40   IF (I .GE. N-K) RETURN                                           
       NUM = NUM + 1                                                    
       JCOL(NUM) = I+K                                                  
       ROW(NUM) = 2.D0                                                  
       RETURN                                                           
       END                                                              
C$TEST PRSY                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SPFNF                             
C                                                                       
C***********************************************************************
       INTEGER N1, N2, N, MCP(500), MRP(500)                            
       INTEGER I1MACH, IREAD, IWRITE, TEMP, ISTAK(19000)                
       REAL A1, A2, GROWTH                                              
       INTEGER ISPAC,IW,MAXIW, IUL                                      
       REAL EL1, EL2, TR, SASUM, RSTAK(19000), B(500)                   
       EXTERNAL QUEI, QUEA                                              
       DOUBLE PRECISION DSTAK(9500)                                     
       COMMON /CSTAK/ DSTAK                                             
       COMMON /QUE/ A1, A2, N1, N2, N                                   
       EQUIVALENCE(RSTAK(1), ISTAK(1), DSTAK(1))                        
       IREAD = I1MACH(1)                                                
       IWRITE = I1MACH(2)                                               
       CALL ISTKIN(19000, 2)                                            
  10   READ(IREAD,11)N1, N2                                             
  11   FORMAT(2I3)                                                      
       IF (N1. EQ. 0) STOP                                              
       WRITE(IWRITE,12)N1, N2                                           
  12   FORMAT(/4H N1=,I3,4H N2=,I3)                                     
       N = (N1+1)*(N2+1)                                                
C                                                                       
C DETERMINE THE ORDERING                                                
C                                                                       
       CALL SPFOR(N, QUEI, MCP)                                         
C                                                                       
C GET THE WORK SPACE FROM THE STORAGE STACK                             
C                                                                       
       MAXIW = ISTKQU(2)-3*N-50                                         
       IW = ISTKGT(MAXIW, 2)                                            
C                                                                       
C DETERMINE THE SYMBOLIC FACTORIZATION                                  
C                                                                       
       DO 20 I=1,N                                                      
          MRP(I) = MCP(I)                                               
  20   CONTINUE                                                         
       CALL SPFSF(N, MRP, MCP, QUEI, ISTAK(IW), MAXIW, ISIZE)           
C                                                                       
C DETERMINE THE ACTUAL AMOUNT OF SPACE USED, MODIFY THE                 
C INTEGER WORK SPACE AND ALLOCATE SPACE TO SAVE THE                     
C FACTORIZATION                                                         
C                                                                       
       ISPAC = 2*N+1+ISIZE                                              
       IW = ISTKMD(ISPAC,2)                                             
       IUL= ISTKGT(ISIZE,3)                                             
  30   READ(IREAD,31)A1, A2                                             
  31   FORMAT(2F10.3)                                                   
       IF (A1.EQ.0.0)GO TO 50                                           
       WRITE(IWRITE,32)A1, A2                                           
  32   FORMAT(/4H A1=,F10.3,4H A2=,F10.3)                               
C                                                                       
C COMPUTE THE NUMERICAL FACTORIZATION                                   
C                                                                       
       CALL SPFNF(N, MRP, MCP, QUEA, ISTAK(IW), RSTAK(IUL),             
     1  GROWTH, 0.0)                                                    
       WRITE(IWRITE,33)GROWTH                                           
  33   FORMAT(7H GROWTH,E25.7)                                          
C                                                                       
C GENERATE RIGHT HAND SIDE                                              
C                                                                       
       DO 40 I=1,N                                                      
          B(I) = 0.0                                                    
  40   CONTINUE                                                         
       B(N) = 1.0                                                       
C                                                                       
C SOLVE THE PROBLEM                                                     
C                                                                       
       CALL SPSOL(N, MRP, MCP, ISTAK(IW), RSTAK(IUL), B, N, 1)          
C                                                                       
C FIND PROBABILITY OF BEING LOST                                        
C                                                                       
       EL1 = B(N)                                                       
       EL2 = SASUM(N1+1,B(N2+1),N2+1)                                   
       TEMP = (N2+1)*N1+1                                               
       TR = SASUM(N2,B(TEMP),1)                                         
       WRITE(IWRITE,41)EL1,EL2,TR                                       
  41   FORMAT(6H L1 = ,E15.5,6H L2 = ,E15.5,7H I12 = ,E15.5)            
       GO TO 30                                                         
C RELEASE STACK SPACE                                                   
  50   CALL ISTKRL(2)                                                   
       GO TO 10                                                         
       END                                                              
C$TEST PRSZ                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SPFML                             
C                                                                       
C***********************************************************************
      INTEGER I, IWRITE, I1MACH, NEQ                                    
      REAL X(100), B(100), ERR, SASUM                                   
      EXTERNAL AROW                                                     
      REAL RSTACK(2500)                                                 
      COMMON/CSTAK/ RSTACK                                              
      COMMON /NN/ NX                                                    
      CALL ISTKIN(2500,3)                                               
      NX = 10                                                           
      NEQ = 100                                                         
C                                                                       
C CONSTRUCT A RANDOM VECTOR FOR X                                       
C                                                                       
      DO 10 I=1,NEQ                                                     
         X(I)=UNI(0)                                                    
  10  CONTINUE                                                          
C                                                                       
C FIND THE VECTOR B=AX                                                  
C                                                                       
      CALL SPFML(NEQ,AROW,X,B)                                          
C                                                                       
C SOLVE THE SYSTEM AX=B                                                 
C                                                                       
      CALL SPFLE(NEQ,.TRUE.,AROW,ISIZE,B,NEQ,1)                         
C                                                                       
C FIND THE NORM OF THE ERROR OF THE SOLUTION                            
C                                                                       
      ERR=0.0                                                           
      IWRITE = I1MACH(2)                                                
      DO 20 I=1,NEQ                                                     
         ERR=ERR + ABS(B(I)-X(I))                                       
  20  CONTINUE                                                          
      ERR=ERR/SASUM(NEQ,X,1)                                            
      WRITE(IWRITE,21)ERR                                               
  21  FORMAT(19H RELATIVE ERROR IS ,1PE15.7)                            
      STOP                                                              
      END                                                               
      SUBROUTINE AROW(I, ROW, JCOL, NUM)                                
      REAL ROW(5)                                                       
      INTEGER JCOL(5)                                                   
      COMMON /NN/ N                                                     
C                                                                       
C IN THE BLOCK TRIDIAGONAL MATRIX THERE ARE AT MOST 5                   
C NONZERO ELEMENTS PER ROW AND EACH ROW HAS A DIAGONAL                  
C OF -4.                                                                
C THE VARIABLE IN INDICATES WHICH BLOCK ONE IS IN AND                   
C THE VARIABLE JN INDICATES WHERE IN THE BLOCK ONE IS AT                
C                                                                       
      IN = (I-1)/N+1                                                    
      JN = I - (IN-1) * N                                               
      JCOL(1)=I                                                         
      ROW(1)=-4.0                                                       
      NUM=2                                                             
C                                                                       
C DO THE OFF DIAGONAL ELEMENTS IN THE CURRENT BLOCK                     
C                                                                       
      JCOL(2)=I-1                                                       
      ROW(2)=1.0                                                        
      IF (JN.GT.1) NUM=NUM+1                                            
      JCOL(NUM)=I+1                                                     
      ROW(NUM)=1.0                                                      
      IF (JN.LT.N)NUM=NUM+1                                             
C                                                                       
C DO THE BLOCK TO THE LEFT                                              
C                                                                       
      JCOL(NUM)=I-N                                                     
      ROW(NUM)=1.0                                                      
      IF (IN.GT.1)NUM=NUM+1                                             
C                                                                       
C DO THE BLOCK TO THE RIGHT                                             
C                                                                       
      JCOL(NUM)=I+N                                                     
      ROW(NUM)=1.0                                                      
      IF(IN.EQ.N) NUM=NUM-1                                             
      RETURN                                                            
      END                                                               
C$TEST PRS1                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SPMML                             
C                                                                       
C***********************************************************************
      INTEGER I, IWRITE, I1MACH, NEQ, J, NX, L                          
      INTEGER IROW(101), JCOL(500)                                      
      REAL A(500)                                                       
      REAL X(100), B(100), ERR, SASUM                                   
      REAL RSTACK(1800)                                                 
      COMMON/CSTAK/ RSTACK                                              
      CALL ISTKIN(1800,3)                                               
      NX = 10                                                           
C                                                                       
C CONSTRUCT THE MATRIX                                                  
C                                                                       
      L=1                                                               
      NEQ=1                                                             
      DO 20 I=1, NX                                                     
         DO 10 J= 1,NX                                                  
            IROW(NEQ)=L                                                 
            JCOL(L)=NEQ                                                 
            A(L)=-4.0                                                   
            L=L+1                                                       
            JCOL(L)=NEQ-1                                               
            A(L)=1.0                                                    
            IF (J.GT.1)L=L+1                                            
            JCOL(L)=NEQ+1                                               
            A(L)=1.0                                                    
            IF (J.LT.NX)L=L+1                                           
            JCOL(L)=NEQ-NX                                              
            A(L)=1.0                                                    
            IF (I.GT.1)L=L+1                                            
            JCOL(L)=NEQ+NX                                              
            A(L)=1.0                                                    
            IF(I.LT.NX)L=L+1                                            
            NEQ=NEQ+1                                                   
  10     CONTINUE                                                       
  20  CONTINUE                                                          
      IROW(NEQ)=L                                                       
      NEQ=NEQ-1                                                         
C                                                                       
C CONSTRUCT A RANDOM VECTOR FOR X                                       
C                                                                       
      DO 30 I=1,NEQ                                                     
         X(I)=UNI(0)                                                    
  30  CONTINUE                                                          
C                                                                       
C FIND THE VECTOR B=AX                                                  
C                                                                       
      CALL SPMML(NEQ,IROW,JCOL,A,X,B)                                   
C                                                                       
C SOLVE THE SYSTEM AX=B                                                 
C                                                                       
      CALL SPMLE(NEQ,.TRUE.,IROW,JCOL,A,ISIZE,B,NEQ,1)                  
C                                                                       
C FIND THE NORM OF THE ERROR OF THE SOLUTION                            
C                                                                       
      ERR=0.0                                                           
      IWRITE = I1MACH(2)                                                
      DO 40 I=1,NEQ                                                     
         ERR=ERR + ABS(B(I)-X(I))                                       
  40  CONTINUE                                                          
      ERR=ERR/SASUM(NEQ,X,1)                                            
      WRITE(IWRITE,41)ERR                                               
  41  FORMAT(19H RELATIVE ERROR IS ,1PE15.7)                            
      STOP                                                              
      END                                                               
C$TEST PRS3                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SPMNF                             
C                                                                       
C***********************************************************************
      INTEGER IROW(301), JCOL(2500), ISTAK(23000), I1MACH, IWRITE       
      INTEGER MRP(300), MCP(300), ITEMP, INMCP(300), N                  
      INTEGER ILAPSZ, IT, IT2, IT3, IT4, IT5, IT6, I, NUMBER            
      INTEGER NUM, IPOINT, NP1                                          
      REAL RSTAK(23000), A(2500), GROWTH                                
      DOUBLE PRECISION DSTAK(11500)                                     
      COMMON /CSTAK/DSTAK                                               
      EQUIVALENCE(ISTAK(1), RSTAK(1), DSTAK(1))                         
      IWRITE = I1MACH(2)                                                
      CALL ISTKIN(23000,2)                                              
      DO 40 N=100,300,100                                               
         CALL SETUP(N, IROW, JCOL, A)                                   
         NUMBER = IROW(N+1)-1                                           
         WRITE(IWRITE,11)N,NUMBER                                       
  11     FORMAT(/5H N = ,I4,22H NUMBER OF NONZEROS = ,I7)               
C                                                                       
C ORDER THE ROWS AND COLUMNS OF THE MATRIX                              
C TO DECREASE FILL-IN                                                   
C                                                                       
         CALL SPMOR(N, IROW, JCOL, MRP, INMCP)                          
C                                                                       
C ALLOCATE THE AVAILABLE SPACE FOR THE WORK VECTOR IN SPMSF             
C BUT MAKE SURE THERE IS ENOUGH FOR SPMSF'S ALLOCATIONS                 
C                                                                       
         MAXIW = ISTKQU(2)-3*N-5                                        
         IW = ISTKGT(MAXIW,2)                                           
C                                                                       
C TIME THE SYMBOLIC FACTORIZATION                                       
C                                                                       
         IT = ILAPSZ(0)                                                 
         CALL SPMSF(N, MRP, INMCP, IROW, JCOL, ISTAK(IW), MAXIW, ISIZE) 
         IT2=ILAPSZ(0)-IT                                               
         WRITE(IWRITE, 12)ISIZE                                         
  12     FORMAT(37H NUMBER OF NONZEROS IN DECOMPOSITION=,I5)            
         WRITE(IWRITE,13)IT2                                            
  13     FORMAT(23H ELAPSED TIME FOR SPMSF,I7)                          
C                                                                       
C MODIFY THE WORK STACK TO REFLECT THE AMOUNT NEEDED BY SPMSF           
C AND ALLOCATE SPACE FOR THE NUMERICAL FACTORIZATION                    
C                                                                       
         ISPAC= 2*N+2+ISIZE                                             
         IW = ISTKMD(ISPAC,2)                                           
         IUL = ISTKGT(ISIZE, 3)                                         
C                                                                       
C COMPUTE THE TIME NEEDED TO INSERT THE NUMERICAL ELEMENTS              
C IN THEIR PROPER PLACES                                                
C                                                                       
         IT3 = ILAPSZ(0)                                                
         DO 20 I=1, N                                                   
            MCP(I) = MRP(I)                                             
            IR = MRP(I)                                                 
            NUM = IROW(IR+1)-IROW(IR)                                   
            IPOINT = IROW(IR)                                           
            CALL SPMIN(N, INMCP, ISTAK(IW), I, A(IPOINT),               
     1        JCOL(IPOINT), NUM, I, RSTAK(IUL))                         
  20     CONTINUE                                                       
         IT4 = ILAPSZ(0)-IT3                                            
         WRITE(IWRITE,21)IT4                                            
  21     FORMAT(23H ELAPSED TIME FOR SPMIN,I7)                          
C                                                                       
C TIME THE SUBROUTINE WHICH COMPUTES THE NUMERICAL                      
C FACTORIZATION                                                         
C                                                                       
         IT5 =ILAPSZ(0)                                                 
         CALL SPMNF(N, ISTAK(IW), RSTAK(IUL), 0.0, GROWTH)              
         IT6 =ILAPSZ(0)-IT5                                             
         WRITE(IWRITE, 22)IT6                                           
  22     FORMAT(23H ELAPSED TIME FOR SPMNF,I7)                          
         IT6 = IT2 + IT4 +IT6                                           
         WRITE(6,23)IT6                                                 
  23     FORMAT(26H ELAPSED TIME FOR SF-IN-NF,I7)                       
C                                                                       
C REDO THE FACTORIZATION WITH THE SUBROUTINE THAT PERMITS               
C PIVOTING FOR STABILITY AND TIME IT                                    
C                                                                       
         CALL MOVEFR(NUMBER,A,RSTAK(IUL))                               
         CALL MOVEFI(NUMBER,JCOL,ISTAK(IW))                             
         IL = ISTKGT(N+1,2)                                             
         IT5 =ILAPSZ(0)                                                 
         CALL SPMLU(N, MRP, MCP, IROW, ISTAK(IW), RSTAK(IUL), ISPAC,    
     1   ISTAK(IL), 0.0, 0.0, ISIZE, GROWTH)                            
         IT6 = ILAPSZ(0)-IT5                                            
         WRITE(IWRITE, 31)IT6                                           
  31     FORMAT(23H ELAPSED TIME FOR SPMLU, I7)                         
         CALL ISTKRL(3)                                                 
  40  CONTINUE                                                          
      STOP                                                              
      END                                                               
C$TEST PREA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SPFOR                             
C                                                                       
C***********************************************************************
       INTEGER N1, N2, N, MCP(500), MRP(500), MAXIW, IW, I              
       INTEGER I1MACH, IREAD, IWRITE, ISTAK(19000)                      
       EXTERNAL QUEI                                                    
       COMMON /QUE/ A1, A2, N1, N2, N                                   
       COMMON /CSTAK/ ISTAK                                             
       IREAD = I1MACH(1)                                                
       IWRITE = I1MACH(2)                                               
       CALL ISTKIN(19000, 2)                                            
  10   READ(IREAD,11)N1, N2                                             
  11   FORMAT(2I3)                                                      
       IF (N1. EQ. 0) STOP                                              
       WRITE(IWRITE,12)N1, N2                                           
  12   FORMAT(4H N1=,I3,4H N2=,I3)                                      
       N = (N1+1)*(N2+1)                                                
C                                                                       
C DETERMINE THE ORDERING                                                
C                                                                       
       CALL SPFOR(N, QUEI, MCP)                                         
C                                                                       
C GET THE WORK SPACE FROM THE STORAGE STACK                             
C                                                                       
       MAXIW = ISTKQU(2)-3*N-50                                         
       IW = ISTKGT(MAXIW, 2)                                            
C                                                                       
C DETERMINE THE SYMBOLIC FACTORIZATION                                  
C                                                                       
       DO 20 I=1,N                                                      
          MRP(I) = MCP(I)                                               
  20   CONTINUE                                                         
       CALL SPFSF(N, MRP, MCP, QUEI, ISTAK(IW), MAXIW, ISIZE)           
       WRITE(IWRITE,21)ISIZE                                            
  21   FORMAT(34H SPACE NEEDED FOR DECOMPOSITION - ,I8)                 
C                                                                       
C REDO THE FACTORIZATION WITHOUT PIVOTING                               
C                                                                       
       DO 30 I = 1,N                                                    
          MCP(I) = I                                                    
          MRP(I) = I                                                    
  30   CONTINUE                                                         
       CALL SPFSF(N, MRP, MCP, QUEI, ISTAK(IW), MAXIW, ISIZE)           
       WRITE(IWRITE, 31) ISIZE                                          
  31   FORMAT(34H SPACE NEEDED WITHOUT ORDERING -   ,I8)                
       CALL ISTKRL(1)                                                   
       GO TO 10                                                         
       END                                                              
       SUBROUTINE QUEI(I, JCOL, NUM)                                    
       INTEGER I, NUM, JCOL(100), N, N1, N2, II, JJ, J                  
       COMMON /QUE/ A1, A2, N1, N2, N                                   
       IF (I.NE.N) GO TO 20                                             
C PROCESS LAST ROW                                                      
       DO 10 J=1, N                                                     
          JCOL(J) = J                                                   
  10   CONTINUE                                                         
       NUM = N                                                          
       RETURN                                                           
  20   N2P1=N2+1                                                        
C DETERMINE WHICH BLOCK                                                 
       II=(I-1)/N2P1                                                    
C DETERMINE THE POSITION IN THE BLOCK                                   
       JJ = MOD(I-1, N2P1)                                              
       JCOL(1) = I                                                      
C INSERT THE DIAGONAL ELEMENT                                           
       NUM = 1                                                          
       IF (II .EQ. 0) GO TO 30                                          
C THIS IS NOT THE FIRST ROW OF THE CURRENT BLOCK                        
       JCOL(2) = I-N2P1                                                 
       NUM = 2                                                          
  30   IF (JJ.EQ.0) GO TO 40                                            
C THIS IS NOT THE FIRST ROW OF THE CURRENT BLOCK                        
       NUM = NUM+1                                                      
       JCOL(NUM) = I-1                                                  
  40   IF (JJ.EQ. N2) GO TO 50                                          
C THIS IS NOT THE LAST ROW OF THE CURRENT BLOCK                         
       NUM=NUM+1                                                        
       JCOL(NUM)= I+1                                                   
  50   IF (II .EQ. N1) RETURN                                           
       NUM = NUM +1                                                     
       JCOL(NUM) = I+N2P1                                               
       RETURN                                                           
       END                                                              
C                                                                       
C DATA FOR THE EXAMPLE IN THE PORT SHEET...  (REMOVE THE C              
C IN COLUMN 1 BEFORE FEEDING THIS DATA TO THE PROGRAM ABOVE.)           
C$DATA                                                                  
C10 10                                                                  
C15 15                                                                  
C19 19                                                                  
C 0  0                                                                  
C$TEST PRMA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SPMSF                             
C                                                                       
C***********************************************************************
       INTEGER MRP(32), MCP(32), INMCP(32)                              
       INTEGER IROW(33), JA(200), IWORK(500)                            
       IREAD = I1MACH(1)                                                
       IWRITE = I1MACH(2)                                               
       N = 32                                                           
       MAXIW = 500                                                      
C                                                                       
C READ IN THE VECTORS DEFINING THE NONZERO BLOCKS                       
C                                                                       
       READ(IREAD,11)(IROW(I),I=1,33)                                   
  11   FORMAT(20I3)                                                     
       IEND=IROW(33) - 1                                                
       READ(IREAD,11)(JA(I),I=1,IEND)                                   
       WRITE(IWRITE,12)IEND                                             
  12   FORMAT(29H NUMBER OF NONZEROS IN MATRIX,I5)                      
C                                                                       
C SET UP THE PERMUTATION VECTORS TO REFLECT NO REORDERING               
C                                                                       
       DO 20 I=1, N                                                     
          MRP(I) = I                                                    
          MCP(I) = I                                                    
          INMCP(I) = I                                                  
  20   CONTINUE                                                         
C                                                                       
C DETERMINE THE SYMBOLIC FACTORIZATION                                  
C                                                                       
       CALL SPMSF(N, MRP, INMCP, IROW, JA, IWORK, MAXIW, ISIZE)         
       WRITE(IWRITE,21)ISIZE                                            
  21   FORMAT(35H BLOCKS NEEDED WITHOUT PERMUTATIONS,I5)                
C                                                                       
C FIND AN ORDERING WHICH WOULD DECREASE FILL-IN AND RECOMPUTE THE       
C SYMBOLIC FACTORIZATION                                                
C                                                                       
       CALL SPMOR(N, IROW, JA, MCP, INMCP)                              
       CALL SPMSF(N, MCP, INMCP, IROW, JA, IWORK, MAXIW, ISIZE)         
       WRITE(IWRITE,22)ISIZE                                            
  22   FORMAT(32H BLOCKS NEEDED WITH PERMUTATIONS,I5)                   
       STOP                                                             
       END                                                              
C$TEST EBEA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM EEBSF                             
C                                                                       
C***********************************************************************
      INTEGER I1MACH,IWRITE,K,NT1,NT2                                   
      EXTERNAL F                                                        
      REAL EESFF,EEBSF,T1(100),A1(100),T2(100),A2(100),                 
     1 ERROR(2),ERREST(2)                                               
C                                                                       
C MAKE THE MESH                                                         
C                                                                       
      K = 4                                                             
      CALL UMB(0.0E0,3.14E0,16,K,T1,NT1)                                
      CALL UMB(0.0E0,3.14E0,21,K,T2,NT2)                                
C                                                                       
C DO THE FITTING                                                        
C                                                                       
      CALL L2SFF(F,K,T1,NT1,A1)                                         
      CALL L2SFF(F,K,T2,NT2,A2)                                         
C                                                                       
C GET THE ERROR                                                         
C                                                                       
      ERROR(1) = EESFF(K,T1,NT1,A1,F)                                   
      ERROR(2) = EESFF(K,T2,NT2,A2,F)                                   
C                                                                       
      ERREST(1) = EEBSF(K,T1,NT1,A1,T2,NT2,A2)                          
      ERREST(2) = ERREST(1)*(FLOAT(NT1-2*K+1)/FLOAT(                    
     1 NT2-2*K-1))**K                                                   
      IWRITE = I1MACH(2)                                                
      WRITE(IWRITE,99)ERROR(1),ERROR(2),ERREST(1),ERREST(2)             
   99  FORMAT(8H ERROR = ,2E10.2,8H ESTER = ,2E10.2)                    
C                                                                       
      STOP                                                              
C                                                                       
      END                                                               
      SUBROUTINE F(X,NX,FX,WX)                                          
C                                                                       
      REAL X(NX),FX(NX),WX(NX)                                          
C                                                                       
      DO 10 I = 1,NX                                                    
      FX(I) = SIN(X(I))                                                 
   10 CONTINUE                                                          
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
C$TEST LSFA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM L2SFF                             
C                                                                       
C***********************************************************************
      EXTERNAL F                                                        
      INTEGER K,IWRITE,I1MACH,NT                                        
      REAL EESFF, T(100), A(100), ERROR                                 
C                                                                       
C MAKE THE MESH                                                         
C                                                                       
      K = 4                                                             
      CALL UMB (0.0E0,3.14E0,21,K,T,NT)                                 
C                                                                       
C DO THE FITTING                                                        
C                                                                       
      CALL L2SFF (F, K, T, NT, A)                                       
C                                                                       
C GET THE ERROR                                                         
C                                                                       
      ERROR = EESFF (K, T, NT, A, F)                                    
C                                                                       
      IWRITE = I1MACH(2)                                                
      WRITE (IWRITE, 1000) ERROR                                        
 1000 FORMAT (9H ERROR = ,E10.2)                                        
C                                                                       
      STOP                                                              
C                                                                       
      END                                                               
      SUBROUTINE F(X, NX, FX, WX)                                       
C                                                                       
      INTEGER I,NX                                                      
      REAL X(NX), FX(NX), WX(NX)                                        
C                                                                       
      DO 1000 I = 1,NX                                                  
      FX(I) = SIN(X(I))                                                 
 1000 CONTINUE                                                          
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
C$TEST SDBA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM DL2SF                             
C                                                                       
C***********************************************************************
      INTEGER I,I1MACH,IWRITE,K,N,NT                                    
      REAL X(51),Y(51),T(100),A(100),                                   
     1 XCHECK(101),YCHECK(101,2),ERR(2)                                 
C                                                                       
      K = 4                                                             
C                                                                       
C MAKE THE ABSCISSAE FOR THE FIT.                                       
C                                                                       
      CALL UMD(0.0E0,3.14E0,51,X)                                       
C                                                                       
C MAKE THE DATA.                                                        
C                                                                       
      DO 1000 I = 1, 51                                                 
      Y(I) = SIN(X(I))                                                  
 1000 CONTINUE                                                          
C                                                                       
C MAKE THE CHECK POINTS                                                 
C                                                                       
      CALL UMD(X(1),X(51),101,XCHECK)                                   
C                                                                       
C MAKE THE MESH.                                                        
C                                                                       
      N = 2                                                             
C                                                                       
      CALL MNPB(X,51,N,K,T,NT)                                          
C                                                                       
C DO THE FIT.                                                           
C                                                                       
      CALL DL2SF(X,Y,51,K,T,NT,A)                                       
C                                                                       
C EVALUATE THE ERROR IN THE FIT AND ITS DERIVATIVES                     
C AT THE CHECK POINTS                                                   
C                                                                       
      CALL SPLND(K,T,NT,A,XCHECK,101,2,YCHECK)                          
C                                                                       
      CALL SETR(2,0.0E0,ERR)                                            
      DO 1001 I = 1, 101                                                
      ERR(1) = AMAX1(ERR(1),ABS(YCHECK(I,1)-SIN(XCHECK(I))))            
      ERR(2) = AMAX1(ERR(2),ABS(YCHECK(I,2)-COS(XCHECK(I))))            
 1001 CONTINUE                                                          
C                                                                       
      IWRITE = I1MACH(2)                                                
      WRITE(IWRITE,1006) ERR(1),ERR(2)                                  
 1006 FORMAT(9H ERROR = ,2E10.2)                                        
C                                                                       
      STOP                                                              
C                                                                       
      END                                                               
C$TEST PDEA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM POST                              
C                                                                       
C***********************************************************************
      REAL TSTOP,V( 1 ),DT,MESH( 100 ),U( 100 )                         
      REAL ERRPAR( 2 )                                                  
      INTEGER K,NMESH,NDX,NU,NV                                         
      EXTERNAL AF,BC,DEE,HANDLE,UOFX                                    
C                                                                       
      COMMON/TIME/TT                                                    
      REAL TT                                                           
      COMMON/CSTAK/DS( 2000 )                                           
      DOUBLEPRECISION DS                                                
      REAL WS( 1000 )                                                   
      REAL RS( 1000 )                                                   
      INTEGER IS( 1000 )                                                
      LOGICAL LS( 1000 )                                                
      EQUIVALENCE( DS( 1 ),WS( 1 ),RS( 1 ),IS( 1 ),LS( 1 ) )            
C                                                                       
C  INITIALIZE THE PORT STACK LENGTH                                     
C                                                                       
      CALL ISTKIN( 2000,4 )                                             
C                                                                       
      NU = 1                                                            
      NV = 1                                                            
C                                                                       
C  SET THE ERROR CRITERION FOR ABSOLUTE ERROR                           
C                                                                       
      ERRPAR( 1 ) = 0                                                   
      ERRPAR( 2 ) = 1.E-2                                               
C                                                                       
      TSTOP = 8.*ATAN( 1.E0 )                                           
      DT = 0.4                                                          
C                                                                       
C MAKE A MESH OF NDX UNIFORM POINTS ON (-PI, +PI)                       
C                                                                       
      K = 4                                                             
      NDX = 7                                                           
      CALL UMB(  - 4.*ATAN( 1.E0 ), + 4.*ATAN( 1.E0 ),NDX,K,MESH,NMESH )
      TT = 0                                                            
C                                                                       
C  SET THE INITIAL CONDITIONS FOR U                                     
C                                                                       
      CALL L2SFF( UOFX,K,MESH,NMESH,U )                                 
C                                                                       
C  SET THE INITIAL CONDITIONS FOR V                                     
C                                                                       
      V( 1 ) =  - 1.                                                    
C                                                                       
      CALL POST( U,NU,K,MESH,NMESH,V,NV,0E0,TSTOP,DT,AF,BC,DEE,ERRPAR,HA
     *NDLE )                                                            
C                                                                       
      STOP                                                              
      END                                                               
      SUBROUTINE AF( T,X,NX,U,UX,UT,UTX,NU,V,VT,NV,A,AU,AUX,AUT,AUTX,AV,
     *AVT,F,FU,FUX,FUT,FUTX,FV,FVT )                                    
      REAL T,X( NX ),U( NX,NU ),UX( NX,NU ),UT( NX,NU ),UTX( NX,NU ),V( 
     *NV ),VT( NV ),A( NX,NU ),AU( NX,NU,NU ),AUX( NX,NU,NU ),AUT( NX,NU
     *,NU ),AUTX( NX,NU,NU ),AV( NX,NU,NV ),AVT( NX,NU,NV ),F( NX,NU ),F
     *U( NX,NU,NU ),FUX( NX,NU,NU ),FUT( NX,NU,NU ),FUTX( NX,NU,NU ),FV(
     * NX,NU,NV ),FVT( NX,NU,NV )                                       
      INTEGER NU,NV,NX                                                  
      INTEGER I                                                         
      DO 23000 I = 1,NX                                                 
      A( I,1 ) =  - UX( I,1 )                                           
      AUX( I,1,1 ) =  - 1                                               
      F( I,1 ) =  - UT( I,1 ) - U( I,1 )**3 + SIN( X( I ) )*( COS( T ) -
     * SIN( T ) + SIN( X( I ) )**2*COS( T )**3 )                        
      FUT( I,1,1 ) =  - 1                                               
      FU( I,1,1 ) =  - 3*U( I,1 )**2                                    
23000 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE BC( T,L,R,U,UX,UT,UTX,NU,V,VT,NV,B,BU,BUX,BUT,BUTX,BV,B
     *VT )                                                              
      REAL T,L,R,U( NU,2 ),UX( NU,2 ),UT( NU,2 ),UTX( NU,2 ),V( NV ),VT(
     * NV ),B( NU,2 ),BU( NU,NU,2 ),BUX( NU,NU,2 ),BUT( NU,NU,2 ),BUTX( 
     *NU,NU,2 ),BV( NU,NV,2 ),BVT( NU,NV,2 )                            
      INTEGER NU,NV                                                     
      B( 1,1 ) = UX( 1,1 ) - V( 1 )                                     
      B( 1,2 ) = UX( 1,2 ) - V( 1 )                                     
      BUX( 1,1,1 ) = 1                                                  
      BV( 1,1,1 ) =  - 1                                                
      BUX( 1,1,2 ) = 1                                                  
      BV( 1,1,2 ) =  - 1                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE DEE( T,K,X,NX,U,UT,NU,NXMK,V,VT,NV,D,DU,DUT,DV,DVT )   
      REAL T,X( NX ),U( NXMK,NU ),UT( NXMK,NU ),V( NV ),VT( NV ),D( NV )
     *,DU( NV,NXMK,NU ),DUT( NV,NXMK,NU ),DV( NV,NV ),DVT( NV,NV )      
      INTEGER K,NX,NU,NXMK,NV                                           
      D( 1 ) = U( 1,1 ) - U( NX - K,1 )                                 
      DU( 1,1,1 ) = 1                                                   
      DU( 1,NX - K,1 ) =  - 1                                           
      RETURN                                                            
      END                                                               
      SUBROUTINE HANDLE( T0,U0,V0,T,U,V,NU,NXMK,NV,K,X,NX,DT,TSTOP )    
      REAL T0,U0( NXMK,NU ),V0( NV ),T,U( NXMK,NU ),V( NV ),X( NX ),DT,T
     *STOP                                                              
      INTEGER NU,NXMK,NV,K,NX                                           
      COMMON/TIME/TT                                                    
      REAL TT                                                           
      REAL EU,EESFF,EV                                                  
      INTEGER I1MACH                                                    
      EXTERNAL UOFX                                                     
      IF( T0 .EQ. T )GO TO 23002                                        
      GO TO 23003                                                       
23002 CONTINUE                                                          
      RETURN                                                            
23003 CONTINUE                                                          
      TT = T                                                            
      EU = EESFF( K,X,NX,U,UOFX )                                       
      EV = V( 1 ) + COS( T )                                            
      IWUNIT = I1MACH( 2 )                                              
      WRITE( IWUNIT,9001 )T,EU,EV                                       
9001  FORMAT( 14H ERROR IN U(X,,1P1E10.2,4H ) =,1P1E10.2,6H   V =,1P4E10
     *.2 )                                                              
      RETURN                                                            
      END                                                               
      SUBROUTINE UOFX( X,NX,U,W )                                       
      REAL X( NX ),U( NX ),W( NX )                                      
      INTEGER NX                                                        
      COMMON/TIME/T                                                     
      REAL T                                                            
      INTEGER I                                                         
      DO 23005 I = 1,NX                                                 
      U( I ) = SIN( X( I ) )*COS( T )                                   
23005 CONTINUE                                                          
      RETURN                                                            
      END                                                               
C$TEST PDEW                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM POSTU                             
C                                                                       
C***********************************************************************
C  THE PORT STACK                                                       
C                                                                       
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(4000)                                         
      REAL WS(1000)                                                     
      EQUIVALENCE (DS(1),WS(1))                                         
C                                                                       
C  TIME FOR THE FUNCTION UOFX.                                          
C                                                                       
      COMMON /TIME/ T                                                   
      REAL T                                                            
C                                                                       
C  MAPPING PARAMETERS FOR UOFX.                                         
C                                                                       
      COMMON /PARAM/ VC, X                                              
      REAL VC(4), X(3)                                                  
      EXTERNAL DEE, HANDLE, UOFX, BC, AF                                
      INTEGER NDX, K, IMMM, ISTKGT                                      
      INTEGER NU, NV, IMESH, ILUMB, NMESH                               
      REAL ERRPAR(2), TSTART, TSTOP, V(4), DT, XB(3), U(1000)           
C INITIALIZE THE PORT LIBRARY STACK LENGTH.                             
      CALL ISTKIN(4000, 4)                                              
      CALL ENTER(1)                                                     
      NU = 1                                                            
      NV = 4                                                            
      ERRPAR(1) = 0                                                     
C ABSOLUTE ERROR.                                                       
      ERRPAR(2) = 1E-2                                                  
      TSTART = 0                                                        
      TSTOP = 3.14                                                      
      DT = 0.4                                                          
      K = 4                                                             
C NDX UNIFORM MESH POINTS ON EACH INTERVAL OF XB.                       
      NDX = 6                                                           
      XB(1) = 0                                                         
      XB(2) = 1                                                         
      XB(3) = 2                                                         
C GET MESH ON PORT STACK.                                               
      IMESH = ILUMB(XB, 3, NDX, K, NMESH)                               
C MAKE 1 OF MULTIPLICITY K-1.                                           
      IMESH = IMMM(IMESH, NMESH, 1E0, K-1)                              
      X(1) = -3.14                                                      
      X(2) = 3.14/2.                                                    
      X(3) = 3.14                                                       
C INITIAL VALUES FOR V.                                                 
      CALL LPLMG(3, X, VC)                                              
C GET U ON THE PORT STACK.                                              
      IU = ISTKGT(NMESH-K, 3)                                           
C UOFX NEEDS TIME.                                                      
      T = TSTART                                                        
C THE INITIAL HEIGHT OF THE JUMP.                                       
      VC(4) = 1                                                         
C UOFX NEEDS V FOR MAPPING.                                             
      CALL MOVEFR(NV, VC, V)                                            
C INITIAL CONDITIONS FOR U.                                             
      CALL L2SFF(UOFX, K, WS(IMESH), NMESH, U)                          
C OUTPUT ICS.                                                           
      CALL HANDLE(T-1., U, V, T, U, V, NU, NMESH-K, NV, K, WS(          
     1   IMESH), NMESH, DT, TSTOP)                                      
      CALL POST(U, NU, K, WS(IMESH), NMESH, V, NV, TSTART, TSTOP,       
     1   DT, AF, BC, DEE, ERRPAR, HANDLE)                               
      CALL LEAVE                                                        
      CALL WRAPUP                                                       
      STOP                                                              
      END                                                               
      SUBROUTINE AF(T, XI, NX, U, UX, UT, UTX, NU, V, VT, NV,           
     *              A, AU, AUX, AUT, AUTX, AV, AVT,                     
     *              F, FU, FUX, FUT, FUTX, FV, FVT)                     
      INTEGER NU, NV, NX                                                
      REAL T, XI(NX), U(NX, NU), UX(NX, NU), UT(NX, NU), UTX(NX, NU)    
      REAL V(NV), VT(NV),                                               
     *     A(NX,NU),AU(NX,NU,NU),AUX(NX,NU,NU),AUT(NX,NU,NU),           
     *     AUTX(NX,NU,NU),AV(NX,NU,NV),AVT(NX,NU,NV),                   
     *     F(NX,NU),FU(NX,NU,NU),FUX(NX,NU,NU),FUT(NX,NU,NU),           
     *     FUTX(NX,NU,NU),FV(NX,NU,NV),FVT(NX,NU,NV)                    
      COMMON /POSTF/ FAILED                                             
      LOGICAL FAILED                                                    
      INTEGER I                                                         
      REAL COS, SIN, XXI(99), XTV(99), XVV(99), X(99)                   
      REAL XXIV(99), AX(99), FX(99), XT(99), XV(99)                     
      LOGICAL TEMP                                                      
      TEMP = V(2) .LE. V(1)                                             
      IF (.NOT. TEMP) TEMP = V(2) .GE. V(3)                             
      IF (.NOT. TEMP) GOTO 1                                            
         FAILED = .TRUE.                                                
         RETURN                                                         
C MAP XI INTO X.                                                        
   1  CALL LPLM(XI, NX, V, 3, X, XXI, XXIV, XV, XVV, XT, XTV)           
C MAP U INTO X SYSTEM.                                                  
      CALL POSTU(XI, X, XT, XXI, XV, VT, NX, 3, UX, UT, NU, AX, FX)     
      DO  4 I = 1, NX                                                   
         A(I, 1) = -U(I, 1)                                             
         AU(I, 1, 1) = -1                                               
         F(I, 1) = UT(I, 1)                                             
         FUT(I, 1, 1) = 1                                               
         IF (XI(I) .GT. 1.) GOTO 2                                      
            F(I, 1) = F(I, 1)-2.*COS(X(I)+T)                            
            FX(I) = 2.*SIN(X(I)+T)                                      
            GOTO  3                                                     
   2        F(I, 1) = F(I, 1)-VT(4)                                     
            FVT(I, 1, 4) = -1                                           
            F(I, 1) = F(I, 1)+2.*SIN(X(I)+T)                            
            FX(I) = 2.*COS(X(I)+T)                                      
   3     CONTINUE                                                       
   4     CONTINUE                                                       
C MAP A AND F INTO XI SYSTEM.                                           
      CALL POSTI(XI, X, XT, XXI, XV, XTV, XXIV, XVV, NX, UX, UT, NU, V  
     1   , VT, NV, 1, 3, A, AX, AU, AUX, AUT, AUTX, AV, AVT, F, FX, FU  
     2   , FUX, FUT, FUTX, FV, FVT)                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE BC(T, L, R, U, UX, UT, UTX, NU, V, VT, NV,             
     *              B, BU, BUX, BUT, BUTX, BV, BVT)                     
      INTEGER NU, NV                                                    
      REAL T,L,R,U(NU,2),UX(NU,2),UT(NU,2),UTX(NU,2),V(NV),VT(NV)       
      REAL B(NU,2),BU(NU,NU,2),BUX(NU,NU,2),BUT(NU,NU,2),BUTX(NU,NU,2), 
     *     BV(NU,NV,2),BVT(NU,NV,2)                                     
      B(1, 1) = U(1, 1)-SIN(T-3.14)                                     
C U(-PI,T) = SIN(-PI+T).                                                
      BU(1, 1, 1) = 1                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE DEE(T, K, X, NX, U, UT, NU, NXMK, V, VT, NV,           
     *               D, DU, DUT, DV, DVT)                               
      INTEGER NXMK, NU, NV, NX, K                                       
      REAL T, X(NX), U(NXMK, NU), UT(NXMK, NU), V(NV), VT(NV)           
      REAL D(NV),DU(NV,NXMK,NU),DUT(NV,NXMK,NU),DV(NV,NV),DVT(NV,NV)    
      INTEGER INTRVR, I, ILEFT                                          
      REAL BX(10), XX(1), R1MACH                                        
      INTEGER TEMP                                                      
      D(1) = V(1)+3.14                                                  
C X(0,V) = -PI.                                                         
      DV(1, 1) = 1                                                      
C XX(1) = 1 + A ROUNDING ERROR.                                         
      XX(1) = R1MACH(4)+1.                                              
      ILEFT = INTRVR(NX, X, XX(1))                                      
C GET THE B-SPLINE BASIS AT XX.                                         
      CALL BSPLN(K, X, NX, XX, 1, ILEFT, BX)                            
      D(2) = -V(4)                                                      
C U(X(T)+,T) - JUMP = 0.                                                
      DV(2, 4) = -1                                                     
      DO  1 I = 1, K                                                    
         TEMP = ILEFT+I-K                                               
         D(2) = D(2)+U(TEMP, 1)*BX(I)                                   
         TEMP = ILEFT+I-K                                               
         DU(2, TEMP, 1) = BX(I)                                         
   1     CONTINUE                                                       
      D(3) = V(3)-3.14                                                  
C X(2,V) = +PI.                                                         
      DV(3, 3) = 1                                                      
C JUMP + D( X(1,V(T)) )/DT = 0.                                         
      D(4) = VT(2)+V(4)                                                 
      DVT(4, 2) = 1                                                     
      DV(4, 4) = 1                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE HANDLE(T0, U0, V0, T, U, V, NU, NXMK, NV,              
     *                  K, X, NX, DT, TSTOP)                            
      INTEGER NXMK, NU, NV, NX, K                                       
      REAL T0, U0(NXMK, NU), V0(NV), T, U(NXMK, NU), V(NV),             
     *     X(NX), DT, TSTOP                                             
      COMMON /PARAM/ VC, XX                                             
      REAL VC(4), XX(3)                                                 
      COMMON /TIME/ TT                                                  
      REAL TT                                                           
      EXTERNAL UOFX                                                     
      INTEGER I1MACH                                                    
      REAL EU, EESFF, EV(2)                                             
      INTEGER TEMP                                                      
C OUTPUT AND CHECKING ROUTINE.                                          
      IF (T0 .NE. T) GOTO 2                                             
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  1) T, DT                                         
   1     FORMAT (16H RESTART FOR T =, 1PE10.2, 7H   DT =, 1PE10.2)      
         RETURN                                                         
   2  TT = T                                                            
C UOFX NEEDS V FOR MAPPING.                                             
      CALL MOVEFR(NV, V, VC)                                            
      EU = EESFF(K, X, NX, U, UOFX)                                     
C ERROR IN POSITION OF SHOCK.                                           
      EV(1) = V(2)-(3.14/2.-T)                                          
C ERROR IN HEIGHT OF SHOCK.                                             
      EV(2) = V(4)-1.                                                   
      TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  3) T, EU, EV                                        
   3  FORMAT (14H ERROR IN U(X,, 1PE10.2, 4H ) =, 1PE10.2, 6H   V =, 2( 
     1   1PE10.2))                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE UOFX(XI, NX, U, W)                                     
      INTEGER NX                                                        
      REAL XI(NX), U(NX), W(NX)                                         
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(4000)                                         
      COMMON /PARAM/ VC, X                                              
      REAL VC(4), X(3)                                                  
      COMMON /TIME/ T                                                   
      REAL T                                                            
      INTEGER IXV, IXX, ISTKGT, I, IS(1000)                             
      REAL EWE, RS(1000), WS(1000)                                      
      LOGICAL LS(1000)                                                  
      INTEGER TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      CALL ENTER(1)                                                     
      IXX = ISTKGT(NX, 3)                                               
C SPACE FOR X AND XV.                                                   
      IXV = ISTKGT(3*NX, 3)                                             
C MAP INTO USER SYSTEM.                                                 
      CALL LPLMX(XI, NX, VC, 3, WS(IXX), WS(IXV))                       
      DO  1 I = 1, NX                                                   
         TEMP = IXX+I                                                   
         U(I) = EWE(T, WS(TEMP-1), VC(2))                               
         IF (XI(I) .GT. 1.) U(I) = U(I)+1.                              
   1     CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      REAL FUNCTION EWE(T, X, XBREAK)                                   
      REAL T, X, XBREAK                                                 
      REAL COS, SIN                                                     
      IF (X .GE. XBREAK) GOTO 1                                         
         EWE = SIN(X+T)                                                 
         RETURN                                                         
   1     IF (X .LE. XBREAK) GOTO 2                                      
            EWE = COS(X+T)                                              
            RETURN                                                      
C/6S                                                                    
C  2        CALL SETERR(17HEWE - X == XBREAK, 17, 1, 2)                 
C/7S                                                                    
   2        CALL SETERR('EWE - X == XBREAK', 17, 1, 2)                  
C/                                                                      
   3  CONTINUE                                                          
   4  STOP                                                              
      END                                                               
C$TEST DDEA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM IODE                              
C                                                                       
C***********************************************************************
      REAL TSTOP,V(2),DT                                                
      REAL ERRPAR(2)                                                    
      INTEGER NV                                                        
      EXTERNAL DEE,HANDLE                                               
C                                                                       
      NV = 2                                                            
C                                                                       
C  SET FOR 1E-2 ABSOLUTE ERROR.                                         
C                                                                       
      ERRPAR(1) = 0                                                     
      ERRPAR(2) = 1E-2                                                  
C                                                                       
      TSTOP = 1E+20                                                     
      DT = 1E-7                                                         
C                                                                       
C  INITIAL CONDITIONS FOR V.                                            
C                                                                       
      V(1) = 1                                                          
      V(2) = 1                                                          
C                                                                       
      CALL IODE (V,NV,                                                  
     *           0E0,TSTOP,DT,                                          
     *           DEE,                                                   
     *           ERRPAR,                                                
     *           HANDLE)                                                
C                                                                       
      STOP                                                              
C                                                                       
      END                                                               
      SUBROUTINE DEE(T,                                                 
     *               V,VT,NV,                                           
     *               D,DV,DVT)                                          
C                                                                       
      REAL T,V(NV),VT(NV),D(NV),DV(NV,NV),DVT(NV,NV)                    
      INTEGER NV                                                        
C                                                                       
      D(1) = VT(1)+2E0*VT(2) + V(1) + 2E+6*V(2)                         
      D(2) = 3E0*VT(1)+VT(2) + 3E0*V(1) + 1E+6*V(2)                     
C                                                                       
      DVT(1,1) = 1                                                      
      DVT(1,2) = 2                                                      
      DV(1,1) = 1                                                       
      DV(1,2) = 2E+6                                                    
C                                                                       
      DVT(2,1) = 3                                                      
      DVT(2,2) = 1                                                      
      DV(2,1) = 3                                                       
      DV(2,2) = 1E+6                                                    
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE HANDLE(T0,V0,T,V,NV,DT,TSTOP)                          
C                                                                       
C OUTPUT AND CHECKING ROUTINE.                                          
C                                                                       
      REAL T0,V0(NV),T,V(NV),DT,TSTOP                                   
      INTEGER NV                                                        
C                                                                       
      REAL EV(2)                                                        
      INTEGER I1MACH                                                    
C                                                                       
      IF ( T0 .EQ. T ) RETURN                                           
C                                                                       
      EV(1) = V(1) - EXP(-T)                                            
      EV(2) = V(2) - EXP(-1E+6*T)                                       
C                                                                       
      IWUNIT = I1MACH(2)                                                
      WRITE(IWUNIT,9000) T,EV(1),EV(2)                                  
 9000 FORMAT(13H ERROR IN V( ,1P1E10.2,4H ) =,1P2E10.2)                 
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
C$TEST DESA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM ODES                              
C                                                                       
C***********************************************************************
      EXTERNAL EQNS, PRINT                                              
      COMMON /NMBR/NMFNS                                                
      COMMON /DATA/ERRPAR,DT,X,TSTART                                   
C                                                                       
      INTEGER KASE,NMFNS                                                
      REAL X(2),DX,DT,TSTART                                            
      REAL ERRPAR(2)                                                    
C                                                                       
      DO 1 KASE=1,3                                                     
      X(1) = 1.E0                                                       
      X(2) = -1.E0                                                      
      CALL CASE(KASE)                                                   
C                                                                       
      CALL ODES (EQNS, X, 2, TSTART, 2.0E0, DT, ERRPAR, PRINT)          
C                                                                       
    1 CONTINUE                                                          
C                                                                       
      STOP                                                              
      END                                                               
      SUBROUTINE EQNS (T, X, N, DX)                                     
      COMMON /NMBR/NMFNS                                                
C                                                                       
      INTEGER NMFNS,N                                                   
      REAL T,X(2),DX(2)                                                 
C                                                                       
      DX(1) = X(2)                                                      
      DX(2) = X(1)                                                      
      NMFNS = NMFNS + 1                                                 
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE CASE(KASE)                                             
      COMMON /NMBR/NMFNS                                                
      COMMON /DATA/ERRPAR,DT,X,TSTART                                   
C                                                                       
      INTEGER IWRITE,I1MACH,NMFNS,KASE                                  
      REAL X(2),DT,TSTART                                               
      REAL ERRPAR(2)                                                    
C                                                                       
      IWRITE = I1MACH(2)                                                
      NMFNS = 0                                                         
      TSTART = 0.0E0                                                    
      GO TO (10, 20, 30), KASE                                          
C                                                                       
C  SET UP CASE 1                                                        
   10 ERRPAR(1) = 1.E-2                                                 
      ERRPAR(2) = 1.E-3                                                 
      DT = 1.E0                                                         
      GO TO 40                                                          
C                                                                       
C  SET UP CASE 2                                                        
   20 ERRPAR(1) = 1.E-4                                                 
      ERRPAR(2) = 1.E-6                                                 
      DT = 1.E-7                                                        
      GO TO 40                                                          
C                                                                       
C  SET UP CASE 3                                                        
   30 ERRPAR(1) = 1.E-4                                                 
      ERRPAR(2) = 1.E-6                                                 
      DT = 1.E0                                                         
C                                                                       
C  WRITE OUT ERRPAR AND DT                                              
   40 WRITE (IWRITE, 9997) ERRPAR(1), ERRPAR(2), DT                     
 9997 FORMAT(15X,28H FOR THE VALUES, ERRPAR(1) =, 1PE9.2,               
     *   16H AND ERRPAR(2) =,1PE9.2  //16X,22HWITH INITIAL DT SET TO,   
     *   1PE10.2//)                                                     
C                                                                       
C  WRITE OUT COLUMN HEADINGS FOR THE SOLUTION                           
      WRITE (IWRITE,9998)                                               
 9998 FORMAT(12X, 5H TIME,14X, 5H X(1),15X, 5H X(2),14X, 3H DT//)       
C                                                                       
C  WRITE OUT THE INITIAL VALUES OF T AND X                              
      WRITE (IWRITE,9999) TSTART, X(1), X(2), DT                        
 9999 FORMAT(2X,1P3E20.8,1PE14.2)                                       
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE PRINT (T0, X0, T1, X1, N, DT, TSTOP, E)                
      COMMON /NMBR/NMFNS                                                
C                                                                       
      INTEGER IWRITE,I1MACH,N,NMFNS                                     
      REAL T0,X0(N),T1,X1(N),DT,TSTOP,E(N)                              
C                                                                       
      IF(T0 .EQ. T1) RETURN                                             
C                                                                       
      IWRITE = I1MACH(2)                                                
      WRITE (IWRITE,9998) T1,X1(1),X1(2),DT                             
 9998 FORMAT (2X,1P3E20.8,1PE14.2)                                      
C                                                                       
      IF (T1 .LT. TSTOP) RETURN                                         
C                                                                       
      WRITE (IWRITE,9999) NMFNS                                         
 9999 FORMAT (1H0,15X,  39H THE NUMBER OF FUNCTION EVALUATIONS WAS,I4)  
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
C$TEST CSPQ                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM CSPFE                             
C                                                                       
C***********************************************************************
       INTEGER I1MACH,J,IWRITE                                          
      REAL X(9),Y(9),YP(9),YPP(9),BC(6),XX(4),YY(4),ERR(4),PI           
C                                                                       
      PI=4.0*ATAN(1.0)                                                  
C                                                                       
C COMPUTE THE POINTS AT WHICH THE SPLINE IS TO BE FITTED                
C                                                                       
      DO 10 J=1,9                                                       
         X(J)=FLOAT(J-1)/8.0                                            
   10    Y(J)=SIN(X(J)*PI/2.0)                                          
C                                                                       
C SET THE END CONDITIONS FOR THE INTERPOLATION                          
C (SPECIFY FIRST DERIVATIVE AT X=0, SECOND AT X=1)                      
C                                                                       
      BC(1)=1.0                                                         
      BC(2)=0.0                                                         
      BC(3)=PI/2.0                                                      
      BC(4)=0.0                                                         
      BC(5)=1.0                                                         
      BC(6)=-X(9)*(PI/2.0)**2                                           
C                                                                       
C DO THE CUBIC SPLINE FIT                                               
C                                                                       
      CALL CSPFI(X,Y,9,BC,YP,YPP)                                       
C                                                                       
C SET THE POINTS AT WHICH TO INTERPOLATE                                
C                                                                       
      XX(1)=0.1                                                         
      XX(2)=0.3                                                         
      XX(3)=0.6                                                         
      XX(4)=0.9                                                         
C                                                                       
C DO THE INTERPOLATION                                                  
C                                                                       
      CALL CSPFE(X,Y,YP,YPP,9,XX,YY,4)                                  
C                                                                       
C COMPUTE THE INTERPOLATION ERROR                                       
C                                                                       
      DO 20 K=1,4                                                       
   20    ERR(K)=YY(K)-SIN(XX(K)*PI/2.0)                                 
C                                                                       
C SET THE OUTPUT UNIT                                                   
C                                                                       
      IWRITE=I1MACH(2)                                                  
C                                                                       
      WRITE(IWRITE,9997)                                                
 9997    FORMAT(2X,2HXX,10X,13HINTERPOLATION)                           
      WRITE(IWRITE,9998)                                                
 9998    FORMAT(12X,5HVALUE,6X,5HERROR//)                               
C                                                                       
      WRITE(IWRITE,9999) (XX(K),YY(K),ERR(K),K=1,4)                     
 9999    FORMAT(0PF6.3,0PF12.6,1PE12.3/)                                
C                                                                       
      STOP                                                              
      END                                                               
C$TEST CSPE                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM CSPDI                             
C                                                                       
C***********************************************************************
       INTEGER IWRITE,I1MACH,J,K                                        
       REAL PI,X(9),Y(9),YY(9),XX(4),YYP(4),ZZ(4),ZZD(4)                
C                                                                       
       PI=3.14159265                                                    
C                                                                       
C COMPUTE THE POINTS AT WHICH THE SPLINE IS TO BE FITTED                
C                                                                       
       DO 10 J=1,9                                                      
       X(J)=FLOAT(J-1)/8.                                               
       Y(J)=SIN(X(J)*PI/2.)                                             
   10  CONTINUE                                                         
C                                                                       
C                                                                       
C SET THE POINTS AT WHICH THE INTERPOLATION AND                         
C DIFFERENTIATION ARE TO BE DONE                                        
C                                                                       
       XX(1)=.1                                                         
       XX(2)=.3                                                         
       XX(3)=.6                                                         
       XX(4)=.9                                                         
C                                                                       
C THE INTERPOLATION:                                                    
C                                                                       
       CALL CSPIN(X,Y,9,XX,YY,4)                                        
C                                                                       
C COMPUTE THE INTERPOLATION ERROR                                       
C                                                                       
       DO 20 K=1,4                                                      
   20  ZZ(K)=YY(K)-SIN(XX(K)*PI/2.)                                     
C                                                                       
C THE DIFFERENTIATION:                                                  
C                                                                       
       CALL CSPDI(X,Y,9,XX,YY,YYP,4)                                    
C                                                                       
C COMPUTE THE DIFFERENTIATION ERROR                                     
C                                                                       
       DO 30 K=1,4                                                      
   30  ZZD(K)=(2./PI)*YYP(K)-COS(XX(K)*PI/2.)                           
C                                                                       
C                                                                       
C SET THE OUTPUT UNIT                                                   
C                                                                       
       IWRITE=I1MACH(2)                                                 
C                                                                       
       WRITE (IWRITE,9997)                                              
 9997     FORMAT(2X,2HXX,10X,13HINTERPOLATION,9X,15HDIFFERENTIATION/)   
C                                                                       
       WRITE (IWRITE,9998)                                              
 9998     FORMAT(12X,5HVALUE,6X,5HERROR,7X,5HVALUE,6X,6H ERROR//)       
C                                                                       
       WRITE (IWRITE,9999)(XX(K),YY(K),ZZ(K),YYP(K),ZZD(K), K=1,4)      
 9999     FORMAT(0PF6.3,0PF12.6,1PE12.3,0PF12.6,1PE12.3/)               
C                                                                       
       STOP                                                             
       END                                                              
C$TEST CSPG                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM CSPIN                             
C                                                                       
C***********************************************************************
C                                                                       
       INTEGER IWRITE,I1MACH,I                                          
       REAL X(9),Y(9),YY(9),XX(9)                                       
C                                                                       
C COMPUTED THE POINTS AT WHICH THE SPLINE IS TO BE FITTED               
C                                                                       
       DO 10 J=1,9                                                      
       X(J)=FLOAT(J-1)/8.                                               
       Y(J)=X(J)**3                                                     
   10  CONTINUE                                                         
C                                                                       
C SET THE POINTS AT WHICH INTERPOLATION IS TO BE DONE                   
C                                                                       
       XX(1)=.3                                                         
       XX(2)=.6                                                         
       XX(3)=.9                                                         
C                                                                       
C PERFORM THE INTERPOLATION                                             
C                                                                       
       CALL CSPIN(X,Y,9,XX,YY,3)                                        
C                                                                       
C SET THE OUTPUT UNIT                                                   
C                                                                       
       IWRITE=I1MACH(2)                                                 
C                                                                       
       WRITE (IWRITE,9998)                                              
 9998     FORMAT(2X,2HXX,5X,11HINTERPOLATE//)                           
C                                                                       
       WRITE (IWRITE,9999) (XX(J), YY(J), J=1,3)                        
 9999     FORMAT(F6.3,F12.6)                                            
C                                                                       
       STOP                                                             
       END                                                              
C$TEST MNNA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM FMIN                              
C                                                                       
C***********************************************************************
       EXTERNAL F                                                       
       INTEGER IWRITE,I1MACH                                            
       REAL A,B,T,ANS,X                                                 
       IWRITE = I1MACH(2)                                               
       A    =  .8                                                       
       B    = 1.2                                                       
       T    =  .0000001                                                 
       ANS  = FMIN(F,X,A,B,T)                                           
       WRITE (IWRITE,9999) A,B,T                                        
9999      FORMAT (5H A = ,1PE14.8,5H B = ,1PE14.8,5H T = ,1PE9.3)       
       WRITE (IWRITE,9998) ANS                                          
9998      FORMAT(16H THE MINIMUM IS ,1PE16.8)                           
       WRITE (IWRITE,9997) X                                            
9997      FORMAT(14H IT OCCURS AT ,1PE18.8)                             
       STOP                                                             
       END                                                              
       FUNCTION F(X)                                                    
       F = -X * EXP(-X)                                                 
       RETURN                                                           
       END                                                              
C$TEST EVAA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM EIGEN                             
C                                                                       
C***********************************************************************
      REAL A(4,4),ORT(4),Z(4,4)                                         
      REAL H(4,4),WR(4),WI(4)                                           
C                                                                       
      DATA A(1,1),A(1,2),A(1,3),A(1,4) / 3., 1., 2., 5. /               
      DATA A(2,1),A(2,2),A(2,3),A(2,4) / 2., 1., 3., 7. /               
      DATA A(3,1),A(3,2),A(3,3),A(3,4) / 3., 1., 2., 4. /               
      DATA A(4,1),A(4,2),A(4,3),A(4,4) / 4., 1., 3., 2. /               
C                                                                       
      NM=4                                                              
      N=4                                                               
C                                                                       
C     SET OUTPUT WRITE UNIT                                             
C                                                                       
       IWUNIT=I1MACH(2)                                                 
C                                                                       
      CALL EIGEN(NM,N,A,WR,WI,Z)                                        
C                                                                       
      WRITE (IWUNIT,96)                                                 
  96  FORMAT (22H0THE EIGENVALUES ARE -/)                               
C                                                                       
      WRITE (IWUNIT,97) (WR(J),WI(J),J=1,N)                             
  97  FORMAT (/1X,2E20.8)                                               
C                                                                       
      DO 20 K=1,N                                                       
      SCALE=AMAX1(ABS(Z(1,K)),ABS(Z(2,K)),ABS(Z(3,K)),ABS(Z(4,K)))      
      DO 20 J=1,N                                                       
 20   Z(J,K)=Z(J,K)/SCALE                                               
C                                                                       
      WRITE (IWUNIT,98)                                                 
  98  FORMAT (30H0THE SCALED EIGENVECTORS ARE -//)                      
C                                                                       
      WRITE (IWUNIT,99) ((Z(J,K),K=1,N),J=1,N)                          
  99  FORMAT (1X,1P4E18.8/)                                             
C                                                                       
      STOP                                                              
      END                                                               
C$TEST LLZA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM LZ                                
C                                                                       
C***********************************************************************
       COMPLEX B(5,5),A(5,5),EIGA(5),EIGB(5),X,EIG                      
       IIN=I1MACH(1)                                                    
       IOUT=I1MACH(2)                                                   
C                                                                       
C READ IN MATRICES                                                      
C                                                                       
       READ(IIN,10)((A(I,J),J=1,5),I=1,5)                               
       READ(IIN,10)((B(I,J),J=1,5),I=1,5)                               
  10   FORMAT(10F6.0)                                                   
C                                                                       
C PRINT MATRICES                                                        
C                                                                       
       WRITE(IOUT,20)                                                   
  20   FORMAT(13H THE A MATRIX)                                         
       WRITE(IOUT,30)((A(I,J),J=1,5),I=1,5)                             
  30   FORMAT(5(F6.0,2H+ ,F6.0,1HI))                                    
       WRITE(IOUT,40)                                                   
  40   FORMAT(13H THE B MATRIX)                                         
       WRITE(IOUT,30)((B(I,J),J=1,5), I=1,5)                            
C                                                                       
C SOLVE THE EIGENVALUE PROBLEM                                          
C                                                                       
       CALL LZ(5,A,5,B,5,X,1,.FALSE.,EIGA,EIGB)                         
       WRITE(IOUT,50)                                                   
  50   FORMAT(10X,4HEIGA,16X,4HEIGB,22X,10HEIGENVALUE)                  
       DO 60 I=1,5                                                      
          EIG=CMPLX(R1MACH(2),R1MACH(2))                                
          IF(REAL(EIGB(I)).NE.0.0.OR.AIMAG(EIGB(I)).NE.0.0)             
     1    EIG=EIGA(I)/EIGB(I)                                           
          WRITE(IOUT,70)EIGA(I),EIGB(I),EIG                             
  60   CONTINUE                                                         
  70   FORMAT(1H ,2E10.3,2X,2E10.3,2X,2E16.8)                           
       STOP                                                             
       END                                                              
C                                                                       
C DATA FOR THE EXAMPLE IN THE PORT SHEET...  (REMOVE THE C              
C IN COLUMN 1 BEFORE FEEDING THIS DATA TO THE PROGRAM ABOVE.)           
C$DATA                                                                  
C   41. -369. -143. -747.  -20.-1368.   20.  486.  104. -432.           
C  148.  261.  144.  666.   -6.-1152.  -78.   45.    8. -540.           
C  -19.  819.   87.  243.    4. 1548.  -56. -954. -164.  180.           
C  -60. -945.  -81. -279.   99.  171.   34.  441.   84. -144.           
C    1. -468.  133.  747.  132.  774.  -46.  -45.  -12. -216.           
C   90.  161.  180.  335.   36.  182.  -90. -162.  -72.  -36.           
C -105. -169. -210. -322.  -42.   24.  105.  167.   84.  204.           
C  -90. -211. -180. -307.  -36. -160.   90.  186.   72.   36.           
C   75.  205.  150.  215.   30.   45.  -75. -165.  -60.  -80.           
C  -75.  -48. -150. -299.  -30. -102.   75.   89.   60.   88.           
C$TEST LGEA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM GESS                              
C                                                                       
C***********************************************************************
       INTEGER N, IREAD, I1MACH, I, NB, IWRITE, J                       
       REAL A(5,5), B(5,2), COND                                        
       N=5                                                              
       IREAD=I1MACH(1)                                                  
C                                                                       
       DO 10 I=1,N                                                      
           READ(IREAD,1) (A(I,J),J=1,N)                                 
    1      FORMAT(1X,5F10.0)                                            
   10  CONTINUE                                                         
C                                                                       
       NB=2                                                             
       DO 20 I=1,N                                                      
           READ(IREAD,11) (B(I,J),J=1,NB)                               
   11      FORMAT(1X,2F10.3)                                            
   20  CONTINUE                                                         
C                                                                       
C SOLVE AX = B  BY CALLING GESS                                         
C                                                                       
       CALL GESS(N,A,N,B,N,NB,COND)                                     
       IWRITE=I1MACH(2)                                                 
       WRITE(IWRITE,21) COND                                            
   21  FORMAT(52H AN ESTIMATE OF THE CONDITION NUMBER OF THE MATRIX =,  
     1          E14.7)                                                  
C                                                                       
       WRITE(IWRITE,22)                                                 
   22  FORMAT(27H THE COMPUTED SOLUTION X IS,//)                        
       DO 30 I=1,N                                                      
           WRITE(IWRITE,23) (B(I,J),J=1,NB)                             
   23      FORMAT(1H ,5F20.7)                                           
   30  CONTINUE                                                         
C                                                                       
       STOP                                                             
       END                                                              
C                                                                       
C DATA FOR THE EXAMPLE IN THE PORT SHEET...  (REMOVE THE C              
C IN COLUMN 1 BEFORE FEEDING THIS DATA TO THE PROGRAM ABOVE.)           
C$DATA                                                                  
C        1.       -2.        3.        7.       -9.                     
C       -2.        8.       -6.        9.       50.                     
C       11.       -6.       18.      -15.      -18.                     
C        7.        2.      -15.      273.      173.                     
C       -9.       50.      -18.        6.     1667.                     
C       30.    29.419                                                   
C     -191.  -190.994                                                   
C      133.   133.072                                                   
C     -986.  -985.775                                                   
C    -6496. -6495.553                                                   
C$TEST LGEB                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM GECE                              
C                                                                       
C***********************************************************************
       INTEGER N, IA, IB, NB, INTER(5), IREAD, I1MACH                   
       INTEGER I, J, IWRITE, ITER, IEND                                 
       REAL A(5, 5), SAVEA(5, 5), B(5), SAVEB(5), R(5)                  
       REAL COND, BNORM, R1MACH, ABS, RNORM                             
       DOUBLE PRECISION DSDOT                                           
C                                                                       
       N=5                                                              
       IA=5                                                             
       IB=5                                                             
       NB=1                                                             
       IREAD=I1MACH(1)                                                  
C                                                                       
       DO 10 I=1,N                                                      
   10     READ(IREAD,11) (A(I,J),J=1,N)                                 
   11     FORMAT(1X,5F8.0)                                              
       DO 20 I=1,IB                                                     
   20     READ(IREAD,21) B(I)                                           
   21     FORMAT(F8.0)                                                  
C                                                                       
C SAVE THE MATRIX AND RIGHT-HAND SIDE (WHICH WILL BE OVERWRITTEN)       
C                                                                       
       DO 40 I=1,N                                                      
         SAVEB(I)=B(I)                                                  
         DO 30 J=1,N                                                    
   30       SAVEA(I,J)=A(I,J)                                           
   40  CONTINUE                                                         
C                                                                       
C SOLVE AX = B USING SEPARATE CALLS TO GECE, GEFS, GEBS                 
C                                                                       
       CALL GECE(N,A,IA,INTER,COND)                                     
       IWRITE=I1MACH(2)                                                 
       IF (COND.GE.1.0/R1MACH(4)) WRITE(IWRITE,41)                      
   41  FORMAT(49H CONDITION NUMBER HIGH,ACCURATE SOLUTION UNLIKELY)     
C                                                                       
       CALL GEFS(N,A,IA,B,IB,NB,INTER)                                  
C                                                                       
       CALL GEBS(N,A,IA,B,IB,NB)                                        
       WRITE(IWRITE,42)                                                 
   42  FORMAT(44H ESTIMATED CONDITION NUMBER OF THE MATRIX A,)          
       WRITE(IWRITE,43) COND                                            
   43  FORMAT(27H  USING ONE CALL TO GECE = ,E15.7)                     
       BNORM=0.0                                                        
       WRITE(IWRITE,44)                                                 
   44  FORMAT(/22H THE FIRST SOLUTION X,)                               
       WRITE(IWRITE,45)                                                 
   45  FORMAT(41H (USING CALLS TO GECE, GEFS, AND GEBS) = )             
C                                                                       
C COMPUTE NORM OF SOLUTION                                              
C                                                                       
       DO 50 I=1,N                                                      
          BNORM=BNORM + ABS(B(I))                                       
   50     WRITE(IWRITE,51) B(I)                                         
   51     FORMAT(1X, 5F20.7)                                            
C                                                                       
C REFINE THE SOLUTION DEPENDING ON THE LENGTH OF THE MANTISSA           
C                                                                       
              IEND=I1MACH(11)*IFIX(R1MACH(5)/ALOG10(2.0) + 1.0)         
       DO 90 ITER=1,IEND                                                
C  COMPUTE RESIDUAL R = B - AX, IN DOUBLE PRECISION                     
C                                                                       
          WRITE(IWRITE,52)                                              
   52     FORMAT(/27H THE RESIDUAL R = B - AX = )                       
          DO 70 I=1,IA                                                  
                DSDOT=0.0                                               
                DO 60 J=1,N                                             
   60              DSDOT = DSDOT  +  DBLE(SAVEA(I,J))*B(J)              
                R(I) = SAVEB(I) - DSDOT                                 
   70           WRITE(IWRITE,51) R(I)                                   
C                                                                       
C  SOLVE LU*(DELTA X) = R USING SEPARATE CALLS TO GEFS AND GEBS         
C                                                                       
          CALL GEFS(N,A,IA,R,IB,NB,INTER)                               
          CALL GEBS(N,A,IA,R,IB,NB)                                     
C                                                                       
C  THE NEW SOLUTION X = X  +  DELTA X                                   
C                                                                       
          WRITE(IWRITE,71)                                              
   71     FORMAT(/38H THE NEW SOLUTION X = X  +  DELTA X = )            
C                                                                       
C DETERMINE NORM OF CORRECTION AND ADD IN CORRECTION                    
C                                                                       
          RNORM=0.0                                                     
          DO 80 I=1,N                                                   
                B(I) = B(I)  +  R(I)                                    
                RNORM=RNORM + ABS(R(I))                                 
   80           WRITE(IWRITE,51) B(I)                                   
C                                                                       
C TEST FOR CONVERGENCE                                                  
C                                                                       
          IF(RNORM.LT.R1MACH(4)*BNORM) GO TO 100                        
   90  CONTINUE                                                         
       WRITE(IWRITE,91)                                                 
   91  FORMAT(/29H ITERATIVE IMPROVEMENT FAILED)                        
  100  CONTINUE                                                         
       STOP                                                             
       END                                                              
C                                                                       
C DATA FOR THE EXAMPLE IN THE PORT SHEET...  (REMOVE THE C              
C IN COLUMN 1 BEFORE FEEDING THIS DATA TO THE PROGRAM ABOVE.)           
C$DATA                                                                  
C     1.     -2.      3.      7.     -9.                                
C    -2.      8.     -6.      2.     50.                                
C     3.     -6.     18.    -15.    -18.                                
C     7.      2.    -15.    273.    174.                                
C    -9.     50.    -18.    173.   1667.                                
C    78.                                                                
C  -320.                                                                
C   -81.                                                                
C   215.                                                                
C-10856.                                                                
C$TEST LGEF                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM GELE                              
C                                                                       
C***********************************************************************
        INTEGER IA, IB, I1MACH, N, I, J, IT, ILAPSZ, IWRITE             
        REAL A(100, 100), AA(100, 100), B(100), BB(100)                 
        REAL SUM, ERR, COND, ABS, TIME, TIMES, AMAX1                    
        IA=100                                                          
        IB =100                                                         
C                                                                       
C GENERATE THE MATRIX AND RIGHT-HAND SIDE                               
C                                                                       
        DO 40 N=10,90,40                                                
           DO 20 I=1,N                                                  
              SUM=0.0                                                   
              DO 10 J=1,N                                               
                 A(I,J)=IABS(I-J)                                       
                 IF (I.GE.J) A(I,J)=A(I,J) + 1.0                        
                 AA(I,J)=A(I,J)                                         
                 SUM=SUM + AA(I,J)                                      
  10          CONTINUE                                                  
              B(I)=SUM                                                  
              BB(I)=SUM                                                 
  20       CONTINUE                                                     
C                                                                       
C CALL  GELE AND TIME IT                                                
           IT =ILAPSZ(0)                                                
           CALL GELE(N,A,IA,B,IB,1)                                     
           TIME=FLOAT(ILAPSZ(0)-IT)/64.0                                
C                                                                       
C COMPUTE THE MAXIMUM ERROR                                             
C                                                                       
           ERR=0.0                                                      
           DO 30 I=1,N                                                  
              ERR=AMAX1(ERR, ABS(B(I)-1.0))                             
  30       CONTINUE                                                     
C                                                                       
C CALL GESS                                                             
C                                                                       
           IT =ILAPSZ(0)                                                
           CALL GESS(N,AA,IA,BB,IB,1,COND)                              
           TIMES=FLOAT(ILAPSZ(0)-IT)/64.0                               
           IWRITE=I1MACH(2)                                             
           WRITE(IWRITE,31)N,COND                                       
  31       FORMAT(8H FOR N= ,I4,20H CONDITION NUMBER = ,E15.7)          
           WRITE(IWRITE,32)ERR                                          
  32       FORMAT(30H MAXIMUM ERROR IN SOLUTION IS ,F15.7)              
           WRITE(IWRITE,33)TIME                                         
  33       FORMAT(34H TIME IN MILLISECONDS FOR GELE IS ,F10.2)          
           WRITE(IWRITE,34)TIMES                                        
  34       FORMAT(34H TIME IN MILLISECONDS FOR GESS IS ,F10.2)          
  40    CONTINUE                                                        
        STOP                                                            
        END                                                             
      INTEGER FUNCTION ILAPSZ(N)                                        
      INTEGER N                                                         
      ILAPSZ = 0                                                        
      RETURN                                                            
      END                                                               
C$TEST LGEH                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM GENM                              
C                                                                       
C***********************************************************************
        INTEGER I, J, L, N, IA, IWRITE, I1MACH                          
        REAL A(50, 50), AA(50, 50), B(50), X(50)                        
        REAL RELERR, RELRES, XNORM, RNORM, ERR, R(50)                   
        REAL GENM, SAMAX                                                
        IA = 50                                                         
C                                                                       
C GENERATE MATRIX                                                       
C                                                                       
        N=50                                                            
        DO 20 I=1,N                                                     
           DO 10 J=I,N                                                  
              A(I,J)=J-I                                                
              A(J,I)=J-I + 1                                            
              AA(I,J)=A(I,J)                                            
              AA(J,I)=A(J,I)                                            
 10        CONTINUE                                                     
           B(I)=I                                                       
 20     CONTINUE                                                        
C                                                                       
C GENERATE RIGHT HAND SIDE                                              
C                                                                       
        CALL GEML(N,A,IA,B,X)                                           
C                                                                       
C MAKE COPY OF RIGHT HAND SIDE                                          
C                                                                       
        CALL MOVEFR(N,X,B)                                              
C                                                                       
C SOLVE THE SYSTEM                                                      
C                                                                       
        CALL GELE(N,A,IA,B,N,1)                                         
C                                                                       
C COMPUTE THE RELATIVE ERROR AND THE RELATIVE RESIDUAL                  
C                                                                       
        CALL GEML(N,AA,IA,B,R)                                          
        ERR=0.0                                                         
        DO 30 I=1,N                                                     
           ERR=AMAX1(ERR,ABS(B(I)-FLOAT(I)))                            
           R(I)=R(I)-X(I)                                               
 30     CONTINUE                                                        
        XNORM=SAMAX(N,X,1)                                              
        RNORM=SAMAX(N,R,1)                                              
        RELERR=ERR/XNORM                                                
        RELRES=RNORM/(XNORM*GENM(N,AA,IA))                              
        IWRITE=I1MACH(2)                                                
        WRITE(IWRITE,31)RELERR,RELRES                                   
 31     FORMAT(16H RELATIVE ERROR=,E15.5,19H RELATIVE RESIDUAL=,        
     1   E15.5)                                                         
        STOP                                                            
        END                                                             
C$TEST LGEJ                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM GELU                              
C                                                                       
C***********************************************************************
       SUBROUTINE DET(N,A,IA,DETMAN,IDETEX)                             
C                                                                       
C THIS SUBROUTINE COMPUTES THE DETERMINANT OF A                         
C THE RESULT IS GIVEN BY DETMAN*BETA**IDETEX                            
C WHERE BETA IS THE BASE OF THE MACHINE                                 
C AND DETMAN IS BETWEEN 1/BETA AND 1                                    
C                                                                       
       INTEGER N, IA, IDETEX                                            
       INTEGER E, IPOINT, ISTKGT, I1MACH, ISIGN, I                      
       INTEGER IN(1000)                                                 
       REAL A(IA, N), DETMAN, BETA, FLOAT, ONOVBE, M, ABS               
       DOUBLE PRECISION D(500)                                          
       COMMON /CSTAK/ D                                                 
       EQUIVALENCE(D(1),IN(1))                                          
C                                                                       
C ALLOCATE SPACE FROM THE STACK FOR THE PIVOT ARRAY                     
C                                                                       
       IPOINT=ISTKGT(N,2)                                               
       CALL GELU(N,A,IA,IN(IPOINT),0.0)                                 
C                                                                       
C THE DETERMINANT IS THE PRODUCT OF THE DIAGONAL ELEMENTS               
C AND THE LAST ELEMENT OF THE INTERCHANGE ARRAY                         
C WE TRY TO COMPUTE THIS PRODUCT IN A WAY THAT WILL                     
C AVOID UNDERFLOW AND OVERFLOW                                          
C                                                                       
       BETA=FLOAT(I1MACH(10))                                           
       ONOVBE=1.0/BETA                                                  
       ISIGN=IPOINT + N-1                                               
       DETMAN=IN(ISIGN)*ONOVBE                                          
       IDETEX=1                                                         
       DO 10 I=1,N                                                      
           CALL UMKFL(A(I,I),E,M)                                       
           DETMAN=DETMAN*M                                              
           IDETEX=IDETEX+E                                              
           IF(ABS(DETMAN).GE.ONOVBE) GO TO 10                           
              IDETEX=IDETEX-1                                           
              DETMAN=DETMAN*BETA                                        
  10   CONTINUE                                                         
       RETURN                                                           
       END                                                              
C$TEST LGEL                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM GEBS                              
C                                                                       
C***********************************************************************
        INTEGER N, I, J, IWRITE, I1MACH                                 
        REAL A(15,15), B(15)                                            
        N=15                                                            
C                                                                       
C FORM THE MATRIX AND SET THE RIGHT-HAND SIDE                           
C TO THE LAST COLUMN OF THE IDENTITY MATRIX                             
        DO 20 I=1,N                                                     
           DO 10 J=I,N                                                  
              A(I,J) = -1.0                                             
  10       CONTINUE                                                     
           A(I,I) = 1.0                                                 
           B(I)   = 0.0                                                 
  20    CONTINUE                                                        
        B(N)=1.0                                                        
C FIND THE LAST COLUMN OF THE INVERSE MATRIX                            
        CALL GEBS(N,A,15,B,N,1)                                         
        IWRITE=I1MACH(2)                                                
        WRITE(IWRITE,21)(I,B(I),I=1,N)                                  
  21    FORMAT(3H B(,I3,3H )=,F15.4)                                    
        STOP                                                            
        END                                                             
C$TEST LGEM                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM GEML                              
C                                                                       
C***********************************************************************
        INTEGER I, J, IWRITE, I1MACH, N                                 
        REAL A(10, 10), X(10), B(10)                                    
        REAL ERR, SASUM, UNI, COND                                      
        N=10                                                            
C                                                                       
C CONSTRUCT A MATRIX                                                    
C                                                                       
        DO 20 I=1,N                                                     
           DO 10 J=I,N                                                  
              A(I,J)=J-I                                                
              A(J,I)=J-I + 1                                            
  10       CONTINUE                                                     
  20    CONTINUE                                                        
C                                                                       
C CONSTRUCT A RANDOM VECTOR X                                           
C                                                                       
        DO 30 I=1,N                                                     
           X(I)=UNI(0)                                                  
  30    CONTINUE                                                        
C                                                                       
C FIND THE VECTOR B=AX                                                  
C                                                                       
       CALL GEML(N,A,10,X,B)                                            
C                                                                       
C SOLVE THE SYSTEM AX=B                                                 
C                                                                       
       CALL GESS(N,A,10,B,N,1,COND)                                     
C                                                                       
C PRINT THE COMPUTED AND TRUE SOLUTION                                  
C                                                                       
       IWRITE=I1MACH(2)                                                 
       WRITE(IWRITE,31)                                                 
  31   FORMAT(34H TRUE SOLUTION   COMPUTED SOLUTION)                    
       WRITE(IWRITE,32)(X(I),B(I),I=1,N)                                
  32   FORMAT(1H ,2E17.8)                                               
C                                                                       
C COMPUTE THE RELATIVE ERROR                                            
C                                                                       
       ERR=0.0                                                          
       DO 40 I=1,N                                                      
          ERR=ERR + ABS(B(I)-X(I))                                      
  40   CONTINUE                                                         
       ERR=ERR/SASUM(N,X,1)                                             
       WRITE(IWRITE,41)ERR                                              
  41   FORMAT(19H RELATIVE ERROR IS ,1PE15.7)                           
       WRITE(6,42)COND                                                  
  42   FORMAT(21H CONDITION NUMBER IS ,1PE15.7)                         
       STOP                                                             
       END                                                              
C$TEST LYMA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SYSS                              
C                                                                       
C***********************************************************************
         INTEGER N, L, I, IWRITE, I1MACH                                
         REAL C(5000), B(100)                                           
         REAL SUM, FLOAT, ABS, ERR, COND                                
         DO 40  N=10,90,40                                              
C                                                                       
C CREATE THE MATRIX A(I,J)=ABS(I-J), PACK IT INTO                       
C THE VECTOR C AND FORM THE RIGHT-HAND SIDE SO THE                      
C SOLUTION HAS ALL ONES.                                                
C                                                                       
           L=1                                                          
           SUM=(N*(N-1))/2                                              
           DO 20 I=1,N                                                  
              DO 10 J=I,N                                               
                 C(L)=J-I                                               
                 L=L+1                                                  
  10          CONTINUE                                                  
              B(I)=SUM                                                  
              SUM=SUM+FLOAT(I-(N-I))                                    
  20       CONTINUE                                                     
C                                                                       
C SOLVE THE SYSTEM AND GET THE CONDITION NUMBER OF THE MATRIX           
           CALL SYSS(N,C,B,100,1,COND)                                  
C                                                                       
C COMPUTE THE ERROR IN THE SOLUTION                                     
           ERR=0.0                                                      
           DO 30 I=1,N                                                  
  30           ERR=ERR+ABS(B(I)-1.0)                                    
           ERR=ERR/FLOAT(N)                                             
           IWRITE=I1MACH(2)                                             
           WRITE(IWRITE,31)N                                            
  31       FORMAT(/8H FOR N= ,I5)                                       
           WRITE(IWRITE,32)COND                                         
  32       FORMAT(23H CONDITION ESTIMATE IS 1PE15.7)                    
           WRITE(IWRITE,33)ERR                                          
  33       FORMAT(30H RELATIVE ERROR IN SOLUTION IS,1PE15.7)            
  40     CONTINUE                                                       
         STOP                                                           
         END                                                            
C$TEST LYMB                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SYCE                              
C                                                                       
C***********************************************************************
       INTEGER N, JEND, IREAD, I1MACH, I, JBEGIN, J, IWRITE             
       INTEGER INTER(6), IEND, ITER, L, IFIX                            
       REAL C(20), SAVEC(36), B(6), SAVEB(6), R(6)                      
       REAL COND, R1MACH, BNORM, RNORM, ABS, ALOG10                     
       DOUBLE PRECISION D(6)                                            
       N=5                                                              
C                                                                       
C READ IN A SYMMETRIC MATRIX WHOSE UPPER TRIANGULAR                     
C PORTION IS STORED ONE ROW PER CARD. MAKE A                            
C COPY OF THE MATRIX SO THAT IT CAN BE USED LATER                       
C                                                                       
        JEND=0                                                          
        IREAD=I1MACH(1)                                                 
        DO 20 I=1,N                                                     
           JBEGIN=JEND+1                                                
           JEND=JBEGIN+N - I                                            
           READ(IREAD,1)(C(J),J=JBEGIN,JEND)                            
  1        FORMAT(5F8.0)                                                
           DO 10 J=JBEGIN,JEND                                          
                SAVEC(J)=C(J)                                           
  10       CONTINUE                                                     
  20    CONTINUE                                                        
C READ IN RIGHT HAND SIDE AND SAVE IT                                   
        DO 30 I=1,N                                                     
           READ(IREAD,1)B(I)                                            
           SAVEB(I)=B(I)                                                
  30    CONTINUE                                                        
C                                                                       
C  SOLVE AX = B USING SEPARATE CALLS TO SYCE AND SYFBS                  
C                                                                       
       CALL SYCE(N,C,INTER,COND)                                        
       CALL SYFBS(N,C,B,6,1,INTER)                                      
       IWRITE=I1MACH(2)                                                 
       IF(COND.GE.1.0/R1MACH(4))WRITE(IWRITE,31)                        
  31   FORMAT(49H CONDITION NUMBER HIGH,ACCURATE SOLUTION UNLIKELY)     
       WRITE(IWRITE,32) COND                                            
  32   FORMAT(21H CONDITION NUMBER IS ,1PE16.8)                         
C      COMPUTE NORM OF SOLUTION                                         
       BNORM=0.0                                                        
       WRITE(IWRITE,33)                                                 
  33   FORMAT(43H THE FIRST SOLUTION X, FROM SYCE AND SYFBS=)           
       DO 40 I=1,N                                                      
          BNORM=BNORM+ABS(B(I))                                         
  40      WRITE(IWRITE,41)B(I)                                          
  41   FORMAT(1H ,F20.7)                                                
C                                                                       
C IEND IS THE UPPER BOUND ON THE NUMBER OF BITS PER WORD                
C                                                                       
       IEND=I1MACH(11)*IFIX(R1MACH(5)/ALOG10(2.0)+1.0)                  
C                                                                       
C REFINE SOLUTION                                                       
C                                                                       
       DO 90 ITER=1,IEND                                                
C                                                                       
C COMPUTE RESIDUAL R = B - AX, IN DOUBLE PRECISION                      
C                                                                       
          DO 50 I=1,N                                                   
  50         D(I)=DBLE(SAVEB(I))                                        
          L=1                                                           
          DO 70 I=1,N                                                   
             DO 60 J=I,N                                                
                IF (I.NE.J) D(J)=D(J) - DBLE(SAVEC(L))*B(I)             
                D(I) = D(I) - DBLE(SAVEC(L))*B(J)                       
  60         L=L+1                                                      
             R(I) = D(I)                                                
  70      CONTINUE                                                      
C                                                                       
C SOLVE A(DELTAX) =R                                                    
C                                                                       
          CALL SYFBS(N,C,R,8,1,INTER)                                   
C                                                                       
C DETERMINE NORM OF CORRECTION AND ADD IN CORRECTION                    
C                                                                       
          WRITE(IWRITE,71)ITER                                          
  71      FORMAT(30H THE SOLUTION AFTER ITERATION ,I5)                  
          RNORM=0.0                                                     
          DO 80 I=1,N                                                   
                B(I) = B(I) + R(I)                                      
                RNORM=RNORM+ABS(R(I))                                   
                WRITE(IWRITE,41)B(I)                                    
  80      CONTINUE                                                      
       IF(RNORM.LT.R1MACH(4)*BNORM) STOP                                
  90   CONTINUE                                                         
       WRITE(IWRITE,91)                                                 
  91   FORMAT(29H ITERATIVE IMPROVEMENT FAILED)                         
       STOP                                                             
       END                                                              
C                                                                       
C DATA FOR THE EXAMPLE IN THE PORT SHEET...  (REMOVE THE C              
C IN COLUMN 1 BEFORE FEEDING THIS DATA TO THE PROGRAM ABOVE.)           
C$DATA                                                                  
C    -4.      0.    -16.    -32.     28.                                
C     1.      5.     10.     -6.                                        
C   -37.    -66.     64.                                                
C   -85.     53.                                                        
C   -15.                                                                
C   448.                                                                
C  -111.                                                                
C  1029.                                                                
C  1207.                                                                
C  -719.                                                                
C$TEST LYMK                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SYNM                              
C                                                                       
C***********************************************************************
        INTEGER I, J, L, N, I1MACH, IWRITE                              
        REAL C(1300), CC(1300), B(50), X(50)                            
        REAL RELERR, RELRES, XNORM, RNORM, ERR, R(50)                   
        REAL SYNM, SAMAX                                                
        L=0                                                             
C                                                                       
C GENERATE MATRIX                                                       
C                                                                       
        N=50                                                            
        DO 20 I=1,N                                                     
           DO 10 J=I,N                                                  
              L=L+1                                                     
              C(L)=J-I                                                  
              CC(L)=C(L)                                                
 10        CONTINUE                                                     
           B(I)=I                                                       
 20     CONTINUE                                                        
C                                                                       
C GENERATE RIGHT HAND SIDE                                              
C                                                                       
        CALL SYML(N,C,B,X)                                              
C                                                                       
C MAKE COPY OF RIGHT HAND SIDE                                          
C                                                                       
        CALL MOVEFR(N,X,B)                                              
C                                                                       
C SOLVE THE SYSTEM                                                      
C                                                                       
        CALL SYLE(N,C,B,N,1)                                            
C                                                                       
C COMPUTE THE RELATIVE ERROR AND THE RELATIVE RESIDUAL                  
C                                                                       
        CALL SYML(N,CC,B,R)                                             
        ERR=0.0                                                         
        DO 30 I=1,N                                                     
           ERR=AMAX1(ERR,ABS(B(I)-FLOAT(I)))                            
           R(I)=R(I)-X(I)                                               
 30     CONTINUE                                                        
        XNORM=SAMAX(N,X,1)                                              
        RNORM=SAMAX(N,R,1)                                              
        RELERR=ERR/XNORM                                                
        RELRES=RNORM/(XNORM*SYNM(N,CC))                                 
        IWRITE=I1MACH(2)                                                
        WRITE(IWRITE,31)RELERR,RELRES                                   
 31     FORMAT(16H RELATIVE ERROR=,E15.5,19H RELATIVE RESIDUAL=,        
     1   E15.5)                                                         
        STOP                                                            
        END                                                             
C$TEST LYMP                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SYML                              
C                                                                       
C***********************************************************************
        INTEGER N, L, I, J, IWRITE, I1MACH                              
        REAL C(55), X(10), B(10)                                        
        REAL UNI, ERR, SASUM, ABS                                       
        N=10                                                            
C                                                                       
C CONSTRUCT THE MATRIX A(I,J)=ABS(J-I) AND PACK INTO C                  
C                                                                       
        L=0                                                             
        DO 20 I=1,N                                                     
           DO 10 J=I,N                                                  
              L=L+1                                                     
              C(L)=J-I                                                  
  10       CONTINUE                                                     
  20    CONTINUE                                                        
C                                                                       
C CONSTRUCT A RANDOM VECTOR X                                           
C                                                                       
        DO 30 I=1,N                                                     
           X(I)=UNI(0)                                                  
  30    CONTINUE                                                        
C                                                                       
C FIND THE VECTOR B=AX                                                  
C                                                                       
       CALL SYML(N,C,X,B)                                               
C                                                                       
C SOLVE THE SYSTEM AX=B                                                 
C                                                                       
       CALL SYLE(N,C,B,N,1)                                             
C                                                                       
C PRINT THE COMPUTED AND TRUE SOLUTION                                  
C                                                                       
       IWRITE=I1MACH(2)                                                 
       WRITE(IWRITE,31)                                                 
  31   FORMAT(34H TRUE SOLUTION   COMPUTED SOLUTION)                    
       WRITE(IWRITE,32)(X(I),B(I),I=1,N)                                
  32   FORMAT(1H ,2E17.8)                                               
C                                                                       
C COMPUTE THE RELATIVE ERROR                                            
C                                                                       
       ERR=0.0                                                          
       DO 40 I=1,N                                                      
          ERR=ERR+ABS(B(I)-X(I))                                        
  40   CONTINUE                                                         
       ERR=ERR/SASUM(N,X,1)                                             
       WRITE(IWRITE,41)ERR                                              
  41   FORMAT(19H RELATIVE ERROR IS ,1PE15.7)                           
       STOP                                                             
       END                                                              
C$TEST LBAA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BASS                              
C                                                                       
C***********************************************************************
       INTEGER N, IG, ML, M, I, J, IWRITE, I1MACH                       
       REAL G(13,80), B(80,2), X(80)                                    
       REAL START, FLOAT, ERR, ERR2, ABS, COND                          
       IG=13                                                            
       N=80                                                             
       DO 60 ML=2,6                                                     
C                                                                       
C CONSTRUCT THE MATRIX A(I,J)=I+J AND PACK IT INTO G                    
C                                                                       
            M=2*ML-1                                                    
            START=-FLOAT(M-ML)                                          
            DO 20 I=1,N                                                 
               G(1,I)=START+FLOAT(2*I)                                  
               IF(M.EQ.1) GO TO 20                                      
               DO 10 J=2,M                                              
                  G(J,I)=G(J-1,I)+1.                                    
  10           CONTINUE                                                 
  20        CONTINUE                                                    
C CONSTRUCT FIRST RIGHT-HAND SIDE SO SOLUTION IS ALL 1S                 
            DO 30 I=1,N                                                 
  30           X(I)=1                                                   
            CALL BAML(N,ML,M,G,IG,X,B)                                  
C CONSTRUCT THE SECOND COLUMN SO X(I)=I                                 
            DO 40 I=1,N                                                 
  40           X(I)=I                                                   
            CALL BAML(N,ML,M,G,IG,X,B(1,2))                             
C SOLVE THE SYSTEM                                                      
            CALL BASS(N,ML,M,G,IG,B,80,2,COND)                          
C COMPUTE THE ERRORS IN THE SOLUTION                                    
            ERR=0.0                                                     
            ERR2=0.0                                                    
            DO 50 I=1,N                                                 
               ERR=ERR+ABS(B(I,1)-1.0)                                  
               ERR2=ERR2+ABS(B(I,2)-FLOAT(I))                           
  50        CONTINUE                                                    
            ERR=ERR/FLOAT(N)                                            
            ERR2=ERR2/FLOAT(N*(N+1))*2.0                                
            IWRITE=I1MACH(2)                                            
            WRITE(IWRITE,51)ML,COND                                     
  51        FORMAT(/9H WHEN ML=,I4,21H THE CONDITION NO. IS,1PE15.7)    
            WRITE(IWRITE,52)ERR                                         
  52        FORMAT(38H REL. ERROR IN THE FIRST SOLUTION IS  ,1PE15.7)   
            WRITE(IWRITE,53)ERR2                                        
  53        FORMAT(38H REL. ERROR IN THE SECOND SOLUTION IS ,1PE15.7)   
  60     CONTINUE                                                       
  70  CONTINUE                                                          
      STOP                                                              
      END                                                               
C$TEST LBAB                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BACE                              
C                                                                       
C***********************************************************************
       INTEGER IG, IGL, N, ML, M, I, J, MU, IWRITE, I1MACH              
       INTEGER INTER(80)                                                
       REAL G(13, 80), B(80), X(80), GL(6, 80)                          
       REAL START, FLOAT, AINNO, COND, CONDNO, ABS, AINNOI              
       IG=13                                                            
       IGL=6                                                            
       N=80                                                             
       IWRITE=I1MACH(2)                                                 
       DO 60 ML=2,6                                                     
C                                                                       
C CONSTRUCT THE MATRIX A(I,J)=I+J AND PACK IT INTO G                    
            M=2*ML - 1                                                  
            START=-FLOAT(M-ML)                                          
            DO 20 I=1,N                                                 
               G(1,I)=START+FLOAT(2*I)                                  
               DO 10 J=2,M                                              
                  G(J,I)=G(J-1,I)+1.                                    
  10           CONTINUE                                                 
  20        CONTINUE                                                    
C                                                                       
C DETERMINE AN ESTIMATE OF THE CONDITION NUMBER                         
C AND COMPUTE THE LU DECOMPOSITION                                      
C                                                                       
            CALL BACE(N,ML,M,G,IG,GL,IGL,INTER,MU,COND)                 
C                                                                       
C DETERMINE THE NORM OF THE INVERSE MATRIX BY                           
C SOLVING FOR ONE COLUMN OF THE INVERSE MATRIX                          
C AT A TIME                                                             
C                                                                       
            AINNO=0.0                                                   
            DO 50 I=1,N                                                 
C                                                                       
C FIND THE ITH COLUMN OF THE INVERSE MATRIX BY                          
C SETTING THE RIGHT HAND SIDE TO THE ITH COLUMN                         
C OF THE IDENTITY MATRIX                                                
C                                                                       
                DO 30 J=1,N                                             
                   B(J)=0.0                                             
  30            CONTINUE                                                
                B(I)=1.0                                                
                CALL BAFS(N,ML,GL,IGL,INTER,B,80,1)                     
                CALL BABS(N,G,IG,B,80,1,MU)                             
C FIND THE NORM OF THE ITH COLUMN                                       
                AINNOI=0.0                                              
                DO 40 J=1,N                                             
                   AINNOI=AINNOI+ABS(B(J))                              
  40            CONTINUE                                                
                IF(AINNOI.GT.AINNO)AINNO=AINNOI                         
  50         CONTINUE                                                   
             WRITE(IWRITE,51)ML                                         
  51         FORMAT(/6H ML IS ,I4)                                      
             WRITE(IWRITE,52)COND                                       
  52         FORMAT(22H CONDITION ESTIMATE IS,1PE15.7)                  
             CONDNO=AINNO*FLOAT(M*(N-ML+1)*2)                           
             WRITE(IWRITE,53)CONDNO                                     
  53         FORMAT(22H TRUE CONDITION NO. IS,1PE15.7)                  
  60      CONTINUE                                                      
          STOP                                                          
          END                                                           
C$TEST LBAF                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BALE                              
C                                                                       
C***********************************************************************
         INTEGER IG, IWRITE, I1MACH, N, ML, II, MP1, I, K               
         INTEGER IB, NB, IT, ILAPSZ                                     
         REAL G(19, 100), B(100, 10), BB(100, 10), GG(19, 100)          
         REAL COND, TIME1, TIME2, UNI                                   
C                                                                       
C THIS PROGRAM SOLVES BANDED SYSTEMS USING BALE AND                     
C BASS AND COMPARES THE TIME FOR EACH OF THEM. THE                      
C SYSTEMS HAVE VARIOUS BANDWIDTHS,DIMENSIONS, AND                       
C NUMBERS OF RIGHT-HAND SIDES                                           
         DOUBLE PRECISION D(600)                                        
         COMMON /CSTAK/ D                                               
C MAKE SURE THE STACK MECHANISM HAS SUFFICIENT SPACE                    
C FOR BASS                                                              
         CALL ISTKIN(1200,3)                                            
         IG=19                                                          
         IWRITE=I1MACH(2)                                               
         IB=100                                                         
         DO 70 N=50,100,50                                              
            DO 60 ML=2,10,8                                             
               M=2*ML - 1                                               
               MP1=M+1                                                  
               DO 50 NB=1,10,9                                          
                  WRITE(IWRITE,1)N,M,NB                                 
  1               FORMAT(/5H N IS,I4,6H M IS ,I3,7H NB IS ,I3)          
C                                                                       
C CONSTRUCT THE MATRIX A(I,J)=ABS(I-J) AND PACK IT INTO G               
C AND MAKE A COPY OF THE MATRIX SO THE SYSTEM CAN BE                    
C SOLVED WITH BOTH BALE AND BASS                                        
C                                                                       
                  K=ML - 1                                              
                  DO 20 I=1,ML                                          
                     II=MP1 - I                                         
                     DO 10 J=1,N                                        
                        G(I,J)=K                                        
                        GG(I,J)=K                                       
                        G(II,J)=K                                       
                        GG(II,J)=K                                      
  10                 CONTINUE                                           
                     K=K - 1                                            
  20              CONTINUE                                              
C                                                                       
C CONSTRUCT RANDOM RIGHT-HAND SIDES                                     
C AND MAKE A COPY                                                       
C                                                                       
                  DO 40 I=1,NB                                          
                     DO 30 II=1,N                                       
                        B(II,I)=UNI(0)                                  
                        BB(II,I)=B(II,I)                                
  30                 CONTINUE                                           
  40              CONTINUE                                              
C                                                                       
C SOLVE THE SYSTEM USING BOTH BASS AND BALE                             
C                                                                       
                  IT=ILAPSZ(0)                                          
                  CALL BASS(N,ML,M,G,IG,B,IB,NB,COND)                   
                  TIME1=(ILAPSZ(0)-IT)/64.0                             
                  WRITE(IWRITE,41)TIME1                                 
  41              FORMAT(34H TIME FOR BASS IN MILLISECONDS IS ,F10.1)   
                  IT=ILAPSZ(0)                                          
                  CALL BALE(N,ML,M,GG,IG,BB,IB,NB)                      
                  TIME2=(ILAPSZ(0)-IT)/64.0                             
                  WRITE(IWRITE,42)TIME2                                 
  42              FORMAT(34H TIME FOR BALE IN MIILISECONDS IS ,F10.1)   
  50           CONTINUE                                                 
  60        CONTINUE                                                    
  70     CONTINUE                                                       
         STOP                                                           
         END                                                            
      INTEGER FUNCTION ILAPSZ(N)                                        
      INTEGER N                                                         
      ILAPSZ = 0                                                        
      RETURN                                                            
      END                                                               
C$TEST LBAJ                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BADC                              
C                                                                       
C***********************************************************************
       SUBROUTINE NEWTON(N,M,ML,X,EPS,FUN,JAC,LIMIT,F)                  
C                                                                       
C THIS SUBROUTINE IMPLEMENTS A LINEARIZED FORM OF NEWTONS               
C METHOD TO FIND THE ZERO OF A FUNCTION F DEFINED BY                    
C FUN, WHOSE BAND JACOBIAN (WITH BANDWIDTH M AND ML                     
C LOWER DIAGONALS) IS EVALUATED IN JAC. LIMIT GIVES                     
C A BOUND ON THE NUMBER OF ITERATIONS AND IN F THE                      
C FINAL FUNCTION VALUE IS RETURNED.                                     
C                                                                       
       INTEGER N, ML, M, LIMIT                                          
       INTEGER JG, JAL, JINTER, ISTKGT, MU, LIM, I                      
       INTEGER IST(1000)                                                
       REAL EPS, X(N), F(N)                                             
       REAL FU, SNRM2, R(1000)                                          
       DOUBLE PRECISION D(500)                                          
       EXTERNAL FUN,JAC                                                 
       COMMON /CSTAK/ D                                                 
       EQUIVALENCE (D(1),R(1)),(D(1),IST(1))                            
C                                                                       
C GET SPACE FOR G,INTER, AND AL FROM                                    
C THE STORAGE STACK                                                     
C                                                                       
       JG= ISTKGT(M*N,3)                                                
       JAL = ISTKGT ((ML-1)*N,3)                                        
       JINTER = ISTKGT(N,2)                                             
       CALL JAC(N,M,ML,X,R(JG),M)                                       
       CALL BADC(N,ML,M,R(JH),M,R(JAL),ML-1,IST(JINTER),MU)             
       LIM=0                                                            
  10   CALL FUN(N,X,F)                                                  
       FU=SNRM2(N,F,1)                                                  
C                                                                       
C CHECK FOR CONVERGENCE OR IF ITERATION LIMIT IS REACHED                
C                                                                       
      IF (FU.LE.EPS.OR.LIM.GT.LIMIT) RETURN                             
      LIM=LIM+1                                                         
C SOLVE THE LINEAR SYSTEM                                               
      CALL BAFS(N,ML,R(JAL),ML-1,IST(JINTER),F,N,1)                     
      CALL BABS(N,R(JG),M,F,N,1,MU)                                     
C CORRECT THE CURRENT ESTIMATE OF THE SOLUTION                          
      DO 20 I=1,N                                                       
         X(I)=X(I)-F(I)                                                 
  20  CONTINUE                                                          
      GO TO 10                                                          
      END                                                               
C$TEST LBAK                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BANM                              
C                                                                       
C***********************************************************************
      INTEGER IG,  ML,  M,  N,  I,  J, IWRITE, I1MACH                   
      REAL G(13,  80),  START,  BANM,  TRNORM                           
      IG=13                                                             
      N=80                                                              
      DO 30 ML=2,6                                                      
C                                                                       
C CONSTRUCT THE MATRIX A(I,J)=I+J AND PACK IT INTO G                    
C                                                                       
         M=2*ML-1                                                       
         START=-FLOAT(M-ML)                                             
         DO 20 I=1,N                                                    
            G(1,I)=START+FLOAT(2*I)                                     
            DO 10 J=2,M                                                 
               G(J,I)=G(J-1,I)+1.0                                      
 10         CONTINUE                                                    
 20      CONTINUE                                                       
C                                                                       
C PRINT OUT THE NORM CALCULATED FROM BANM AND THE TRUE NORM             
C                                                                       
         TRNORM=M*(N-ML+1)*2                                            
         IWRITE=I1MACH(2)                                               
         WRITE(IWRITE,21)ML                                             
 21      FORMAT(/6H ML IS ,I4)                                          
         WRITE(IWRITE,22)TRNORM,BANM(N,ML,M,G,IG)                       
 22      FORMAT(15H THE TRUE NORM=,E15.5,15H COMPUTED NORM=,E15.5)      
 30   CONTINUE                                                          
      STOP                                                              
      END                                                               
C$TEST LBAL                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BALU                              
C                                                                       
C***********************************************************************
       SUBROUTINE BADET(N,ML,M,A,IA,DETMAN,IDETEX)                      
C                                                                       
C THIS SUBROUTINE COMPUTES THE DETERMINANT OF A                         
C BANDED MATRIX STORED IN PACKED FORM IN A                              
C THE DETERMINANT IS COMPUTED AS DETMAN*BETA**IDETEX,                   
C WHERE BETA IS THE BASE OF THE MACHINE AND                             
C DETMAN IS BETWEEN 1/BETA AND 1 IN ABSOLUTE VALUE                      
C                                                                       
       INTEGER ML, M, N, IA, IDETEX                                     
       INTEGER E, ISPAC, IALOW, ISTKGT, ISIGN, INTER, I, MU             
       INTEGER IN(1000)                                                 
       REAL A(IA,1), DETMAN, BETA, ONOVBE, S                            
       REAL R(1000)                                                     
       DOUBLE PRECISION D(500)                                          
       COMMON /CSTAK/D                                                  
       EQUIVALENCE(D(1),R(1)),(D(1),IN(1))                              
C                                                                       
C ALLOCATE SPACE FROM THE STACK FOR THE PIVOT ARRAY                     
C AND THE EXTRA SPACE TO HOLD THE LOWER TRIANGLE                        
C                                                                       
       ISPAC=(ML-1)*N                                                   
       IALOW=ISTKGT(ISPAC,3)                                            
       INTER=ISTKGT(N,2)                                                
       CALL BALU(N,ML,M,A,IA,R(IALOW),ML-1,IN(INTER),MU,0.0)            
C                                                                       
C THE DETERMINANT IS THE PRODUCT OF THE ELEMENTS OF                     
C ROW 1 OF A TIMES THE LAST ELEMENT IN THE ARRAY INTER.                 
C WE TRY TO COMPUTE THIS PRODUCT IN A WAY THAT WILL                     
C AVOID UNDERFLOW AND OVERFLOW.                                         
C                                                                       
       BETA=FLOAT(I1MACH(10))                                           
       ONOVBE=1.0/BETA                                                  
       ISIGN=INTER+N-1                                                  
       DETMAN=IN(ISIGN)*ONOVBE                                          
       IDETEX=1                                                         
       DO 10 I=1,N                                                      
          CALL UMKFL(A(1,I),E,S)                                        
          DETMAN=DETMAN*S                                               
          IDETEX=IDETEX+E                                               
          IF (ABS(DETMAN).GE.ONOVBE) GO TO 10                           
             IDETEX=IDETEX-1                                            
             DETMAN=DETMAN*BETA                                         
   10   CONTINUE                                                        
        CALL ISTKRL(2)                                                  
        RETURN                                                          
        END                                                             
C$TEST LBAN                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BABS                              
C                                                                       
C***********************************************************************
      INTEGER N, I, IWRITE, I1MACH                                      
      REAL G(3, 200), EVEC(100)                                         
      N=10                                                              
      DO 10 I=1,N                                                       
         G(1,I)=-1.0                                                    
         G(2,I)=1.0                                                     
         G(3,I)=-1.0                                                    
 10   CONTINUE                                                          
      G(2,1)=-.75                                                       
      G(2,N)=-.75                                                       
      G(3,1)=-.5                                                        
      G(1,2)=-.5                                                        
      G(1,N)=-.5                                                        
      G(3,N-1)=-.5                                                      
      IWRITE=I1MACH(2)                                                  
      CALL EIGVEC(N,3,2,G,3,-1.0,EVEC,2)                                
      DO 20 I=1,N                                                       
         WRITE(IWRITE,21)EVEC(I)                                        
 20   CONTINUE                                                          
 21   FORMAT(12H EIGENVECTOR,F16.8)                                     
      STOP                                                              
      END                                                               
      SUBROUTINE EIGVEC(N,M,ML,G,IG,EVAL,EVEC,LIMIT)                    
C                                                                       
C GIVEN A BANDED MATRIX PACKED INTO G WITH                              
C N ROWS, M NONZERO DIAGONALS AND ML NONZERO DIAGONALS                  
C ON AND BELOW THE DIAGONAL AND GIVEN AN EIGENVALUE OF THE              
C MATRIX IN EVAL, THIS SUBROUTINE USES INVERSE ITERATION TO             
C DETERMINE THE CORRESPONDING EIGENVECTOR AND RETURNS IT                
C IN EVEC.                                                              
C LIMIT IS A BOUND ON THE NUMBER OF ITERATIONS                          
C                                                                       
      INTEGER N, M, ML, IG, LIMIT                                       
      INTEGER I, JAL, ISTKGT, JINTER, JX, MU, IERR, NERROR              
      INTEGER LIM, JJ, ISAMAX, JXI, IST(1000)                           
      REAL G(IG, N), EVEC(N), EVAL                                      
      REAL SIZE, R1MACH, EPS, SC, BET, D1, SC2, ABS                     
      REAL R(1000)                                                      
      DOUBLE PRECISION D(500)                                           
      COMMON /CSTAK/ D                                                  
      EQUIVALENCE (D(1),IST(1)),(R(1),D(1))                             
      CALL ENTER(1)                                                     
C DETERMINE ITERATION TOLERANCE                                         
      SIZE = BANM(N,ML,M,G,IG)                                          
      EPS=SIZE*R1MACH(4)                                                
C SUBTRACT EIGENVALUE FROM DIAGONAL OF G                                
      DO 10 I=1,N                                                       
         G(ML,I)=G(ML,I) - EVAL                                         
 10   CONTINUE                                                          
C GET SPACE FROM STACK FOR AL,INTER, AND SCRATCH VECTOR                 
      JAL =ISTKGT(N*(ML-1),3)                                           
      JINTER=ISTKGT(N,2)                                                
      JX=ISTKGT(N,3)                                                    
C GET LU DECOMPOSITION OF MATRIX                                        
      CALL BALU(N,ML,M,G,IG,R(JAL),ML-1,IST(JINTER),MU,EPS)             
C OBTAIN INITIAL RIGHT HAND SIDE                                        
      IF (NERROR(IERR).NE.0) CALL ERROFF                                
      DO 20 I=1,N                                                       
         EVEC(I)=1.0                                                    
 20   CONTINUE                                                          
      CALL BABS(N,G,IG,EVEC,N,1,MU)                                     
      LIM=0                                                             
      JJ=ISAMAX(N,EVEC,1)                                               
      SC=1.0/EVEC(JJ)                                                   
C SCALE FIRST RHS TO HAVE INFINITY NORM OF 1                            
      CALL SSCAL(N,SC,EVEC,1)                                           
C ITERATIVE PHASE BEGINS HERE                                           
 30   LIM=LIM+1                                                         
C MAKE A COPY OF OLD APPROXIMATION                                      
      CALL MOVEFR(N,EVEC,R(JX))                                         
C GET NEW APPROXIMATION OF EIGNVECTOR                                   
      CALL BAFS(N,ML,R(JAL),ML-1,IST(JINTER),EVEC,N,1)                  
      CALL BABS(N,G,IG,EVEC,N,1,MU)                                     
      BET=1.0/EVEC(JJ)                                                  
      JJ=ISAMAX(N,EVEC,1)                                               
      SC2=1.0/EVEC(JJ)                                                  
C COMPUTE CONVERGENCE CRITERIA                                          
      D1=0.0                                                            
      DO 40 I=1,N                                                       
         JXI=JX-1+I                                                     
         D1=AMAX1(D1,ABS((R(JXI)-BET*EVEC(I))*SC2))                     
 40   CONTINUE                                                          
      SC=SC2                                                            
      CALL SSCAL(N,SC,EVEC,1)                                           
C TEST FOR CONVERGENCE AND IF ITERATION LIMIT EXCEEDED                  
      IF (D1.GT.EPS.AND.LIM.LT.LIMIT) GO TO 30                          
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
C$TEST LBAP                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BAML                              
C                                                                       
C***********************************************************************
         INTEGER IG, M, ML, N, I, IWRITE, I1MACH                        
         REAL G(5,20), X(20), B(20), UNI, ERR, SASUM, ABS, COND         
         IG=5                                                           
         M=5                                                            
         N=10                                                           
         ML=3                                                           
C                                                                       
C CONSTRUCT THE A MATRIX AND PACK IT INTO G                             
C                                                                       
          DO 10 I=1,N                                                   
             G(1,I)=2.0                                                 
             G(2,I)=1.0                                                 
             G(3,I)=0.0                                                 
             G(4,I)=1.0                                                 
             G(5,I)=2.0                                                 
  10      CONTINUE                                                      
C                                                                       
C CONSTRUCT A RANDOM VECTOR                                             
C                                                                       
          DO 20 I=1,N                                                   
             X(I)=UNI(0)                                                
  20      CONTINUE                                                      
C                                                                       
C CONSTRUCT B=AX                                                        
C                                                                       
          CALL BAML(N,ML,M,G,IG,X,B)                                    
C                                                                       
C SOLVE THE SYSTEM AX=B                                                 
C                                                                       
          CALL BASS(N,ML,M,G,IG,B,N,1,COND)                             
C                                                                       
C PRINT OUT THE TRUE SOLUTION AND THE COMPUTED SOLUTION                 
C                                                                       
          IWRITE=I1MACH(2)                                              
          WRITE(IWRITE,21)                                              
  21      FORMAT(34H TRUE SOLUTION   COMPUTED SOLUTION)                 
          WRITE(IWRITE,22)(X(I),B(I),I=1,N)                             
  22      FORMAT(1H ,2E17.8)                                            
C                                                                       
C COMPUTE THE RELATIVE ERROR                                            
C                                                                       
          ERR=0.0                                                       
          DO 30 I=1,N                                                   
             ERR=ERR+ABS(B(I)-X(I))                                     
  30      CONTINUE                                                      
          ERR=ERR/SASUM(N,X,1)                                          
          WRITE(IWRITE,31)ERR                                           
  31      FORMAT(19H RELATIVE ERROR IS ,1PE15.7)                        
          WRITE(IWRITE,32)COND                                          
  32      FORMAT(20H CONDITION NUMBER IS,1PE15.7)                       
          STOP                                                          
          END                                                           
C$TEST LPSA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BPSS                              
C                                                                       
C***********************************************************************
       INTEGER N, K, I, IWRITE, I1MACH, MU                              
       REAL G(2,100), B(200)                                            
       REAL X, COND, ERR, AMAX1                                         
C CONSTRUCT MATRIX AND RIGHT-HAND SIDE SO TRUE SOLUTION IS              
C COMPOSED ENTIRELY OF ONES                                             
       N=100                                                            
       X=1                                                              
       DO 30  K=1,3                                                     
          DO 10 I=1,N                                                   
             G(1,I)=2.0                                                 
             G(2,I)=-1.0                                                
             B(I)=0.0                                                   
  10      CONTINUE                                                      
          G(1,1)=1.0+X                                                  
          G(1,N)=1.0+X                                                  
          B(1)=X                                                        
          B(N)=X                                                        
C SOLVE THE SYSTEM                                                      
          MU=2                                                          
          CALL BPSS(N,MU,G,2,B,N,1,COND)                                
          IWRITE=I1MACH(2)                                              
          WRITE(IWRITE,11)X                                             
  11      FORMAT(/5H X IS,F15.7)                                        
          WRITE(IWRITE,12)COND                                          
  12      FORMAT(20H CONDITION NUMBER IS,1PE15.7)                       
C COMPUTE THE ERROR                                                     
          ERR=0.0                                                       
          DO 20 I=1,N                                                   
             ERR=AMAX1(ERR,ABS(B(I)-1.0))                               
  20      CONTINUE                                                      
          WRITE(IWRITE,21)ERR                                           
  21      FORMAT(22H FOR BPSS THE ERROR IS,F16.8)                       
          X=X/100.                                                      
  30   CONTINUE                                                         
       STOP                                                             
       END                                                              
C$TEST LPSB                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BPCE                              
C                                                                       
C***********************************************************************
        INTEGER N, MU, IG, K, I, IWRITE, I1MACH, J                      
        REAL G(2,100), B(200)                                           
        REAL X, COND, AINVNO, AINORM, ABS                               
        N=100                                                           
        X=1.0                                                           
        MU=2                                                            
        IG=2                                                            
        DO 50 K=1,3                                                     
C CONSTRUCT MATRIX                                                      
            DO 10 I=1,N                                                 
               G(1,I)=2.0                                               
               G(2,I)=-1.0                                              
  10        CONTINUE                                                    
            G(1,1)=1.0+X                                                
            G(1,N)=1.0+X                                                
C GET ESTIMATE OF CONDITION NUMBER FROM BPCE                            
            CALL BPCE(N,MU,G,IG,COND)                                   
            IWRITE=I1MACH(2)                                            
            WRITE(IWRITE,11)X                                           
  11        FORMAT(/10H WHEN X IS,E14.6)                                
            WRITE(IWRITE,12)COND                                        
  12        FORMAT(25H CONDITION ESTIMATE IS   ,E15.8)                  
C SINCE CONDITION NUMBER IS NORM(A)*NORM(INVERSE(A)),                   
C FIND THE NORM OF EACH COLUMN OF INVERSE(A). GENERATE                  
C THE COLUMNS ONE AT A TIME AND REUSE SPACE                             
            AINVNO=0.0                                                  
            DO 40 I=1,N                                                 
C GENERATE ITH COLUMN OF IDENTITY MATRIX AS RIGHT HAND SIDE             
                DO 20 J=1,N                                             
                   B(J)=0.0                                             
  20            CONTINUE                                                
                B(I)=1.0                                                
C SOLVE AX=B TO GET ITH COLUMN OF A(INVERSE)                            
                CALL BPFS(N,MU,G,IG,B,N,1)                              
                CALL BPBS(N,MU,G,IG,B,N,1)                              
C FIND NORM OF COLUMN                                                   
                AINORM=0.0                                              
                DO 30 J=1,N                                             
                   AINORM=AINORM+ABS(B(J))                              
  30            CONTINUE                                                
                IF(AINVNO.LT.AINORM)AINVNO=AINORM                       
  40        CONTINUE                                                    
            COND=4.0*AINVNO                                             
            WRITE(IWRITE,41)COND                                        
  41        FORMAT(25H TRUE CONDITION NUMBER IS,E15.8)                  
            X=X/100.0                                                   
  50     CONTINUE                                                       
         STOP                                                           
         END                                                            
C$TEST LPSF                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BPLE                              
C                                                                       
C***********************************************************************
           INTEGER IG, N, MU, MLM1, I, KBLOK, KK, J                     
           INTEGER IWRITE, I1MACH                                       
           REAL G(11,100), B(100), X(100)                               
           REAL ERR, AMAX1                                              
           IG=11                                                        
           N=100                                                        
           MU=11                                                        
C                                                                       
C SET UP MATRIX FOR ELLIPTIC PDE IN 2 DIMENSIONS                        
C                                                                       
           MLM1=MU-1                                                    
           I=0                                                          
           DO 30 KBLOK=1,MLM1                                           
              DO 20 KK=1,MLM1                                           
                 I=I+1                                                  
                 G(1,I)=4.0                                             
                 G(2,I)=-1.0                                            
                 DO 10 J=3,MLM1                                         
                    G(J,I)=0.0                                          
   10            CONTINUE                                               
                 G(MU,I)=-1.0                                           
   20         CONTINUE                                                  
              G(2,I)=0.0                                                
   30      CONTINUE                                                     
C                                                                       
C SET UP RIGHT HAND SIDE SO SOLUTION IS ALL 1'S                         
C                                                                       
           DO 40 I=1,N                                                  
              X(I)=1.0                                                  
  40       CONTINUE                                                     
           CALL BPML(N,MU,G,IG,X,B)                                     
C                                                                       
C SOLVE THE SYSTEM                                                      
C                                                                       
           CALL BPLE(N,MU,G,IG,B,100,1)                                 
C                                                                       
C COMPUTE THE ERROR                                                     
C                                                                       
           ERR=0.0                                                      
           DO 50 I=1,N                                                  
              ERR=AMAX1(ERR,ABS(B(I)-1.0))                              
  50       CONTINUE                                                     
           IWRITE=I1MACH(2)                                             
           WRITE(IWRITE,51)ERR                                          
  51       FORMAT(31H ERROR IN SOLUTION FROM BPLE IS,F15.8)             
           STOP                                                         
           END                                                          
C$TEST LPSG                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BPDC                              
C                                                                       
C***********************************************************************
       INTEGER IG, MLM1, IWRITE, I1MACH, K, N, MU                       
       INTEGER NBLOK, KBLOK, KK, I, J, IT, ILAPSZ, IT2                  
       REAL G(17, 100), G2(17, 100), G3(17, 100)                        
       REAL COND                                                        
       IG=17                                                            
       MLM1=4                                                           
       IWRITE=I1MACH(2)                                                 
       DO 70  K=1,3                                                     
          DO 60 N=48,96,48                                              
             MU=MLM1+1                                                  
             I=0                                                        
             NBLOK=N/MLM1                                               
C                                                                       
C SET UP THREE MATRICES FOR ELLIPTIC PDE IN 2 DIMENSION                 
C                                                                       
             DO 30 KBLOK=1,NBLOK                                        
                DO 20 KK=1,MLM1                                         
                   I=I+1                                                
                   G(1,I)=4.0                                           
                   G(2,I)=-1.0                                          
                   G(MU,I)=-1.0                                         
                   DO 10 J=3,MLM1                                       
                      G(J,I)=0.0                                        
  10               CONTINUE                                             
  20            CONTINUE                                                
                G(2,I)=0.0                                              
  30         CONTINUE                                                   
             DO 50 I=1,N                                                
                DO 40 J=1,MU                                            
                   G2(J,I)=G(J,I)                                       
                   G3(J,I)=G(J,I)                                       
  40            CONTINUE                                                
  50         CONTINUE                                                   
             WRITE(IWRITE,51)N,MU                                       
  51         FORMAT(/6H N IS ,I4,30H ,NUMBER OF UPPER DIAGONALS IS,I3)  
C TIME DECOMPOSITION BY BPLD                                            
             IT=ILAPSZ(0)                                               
             CALL BPLD(N,MU,G,IG,0.0)                                   
             IT=ILAPSZ(0)-IT                                            
             WRITE(IWRITE,52)IT                                         
  52         FORMAT(14H TIME FOR BPLD,I7)                               
C TIME DECOMPOSITION BY BPDC                                            
             IT2=ILAPSZ(0)                                              
             CALL BPDC(N,MU,G2,IG)                                      
             IT2=ILAPSZ(0)-IT2                                          
             WRITE(IWRITE,53)IT2                                        
  53         FORMAT(14H TIME FOR BPDC,I7)                               
C TIME DECOMPOSITION BY BPCE                                            
             IT3=ILAPSZ(0)                                              
             CALL BPCE(N,MU,G3,IG,COND)                                 
             IT3=ILAPSZ(0)-IT3                                          
             WRITE(IWRITE,54)IT3                                        
  54         FORMAT(14H TIME FOR BPCE,I7)                               
  60     CONTINUE                                                       
         MLM1=MLM1*2                                                    
  70  CONTINUE                                                          
      STOP                                                              
      END                                                               
      INTEGER FUNCTION ILAPSZ(N)                                        
      INTEGER N                                                         
      ILAPSZ = 0                                                        
      RETURN                                                            
      END                                                               
C$TEST LPSJ                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BPLD                              
C                                                                       
C***********************************************************************
       SUBROUTINE BPDET(N,MU,G,IG,DETMAN,IDETEX)                        
C                                                                       
C THIS SUBROUTINE COMPUTES THE DETERMINANT OF A                         
C BAND SYMMETRIC POSITIVE DEFINITE MATRIX STORED IN G.                  
C IT IS GIVEN BY DETMAN*BETA**IDETEX                                    
C WHERE BETA IS THE BASE OF THE MACHINE                                 
C AND DETMAN IS BETWEEN 1/BETA AND 1                                    
C                                                                       
       REAL G(IG,N),DETMAN                                              
       REAL ONOVBE,M                                                    
       INTEGER E                                                        
       INTEGER IDETEX                                                   
       CALL BPLD(N,MU,G,IG,0.0)                                         
C                                                                       
C THE DETERMINANT IS THE PRODUCT OF THE ELEMENTS OF ROW 1 OF G          
C WE TRY TO COMPUTE THIS PRODUCT IN A WAY THAT WILL                     
C AVOID UNDERFLOW AND OVERFLOW                                          
C                                                                       
       ONOVBE=1.0/FLOAT(I1MACH(10))                                     
       DETMAN=ONOVBE                                                    
       BETA=FLOAT(I1MACH(10))                                           
       IDETEX=1                                                         
       DO 10 I=1,N                                                      
           CALL UMKFL(G(1,I),E,M)                                       
           DETMAN=DETMAN*M                                              
           IDETEX=IDETEX+E                                              
           IF(DETMAN.GE.ONOVBE) GO TO 10                                
              IDETEX=IDETEX-1                                           
              DETMAN=DETMAN*BETA                                        
  10   CONTINUE                                                         
       RETURN                                                           
       END                                                              
C$TEST LPSK                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BPFS                              
C                                                                       
C***********************************************************************
       INTEGER N, ML, IG, NM1, K, I, IWRITE, I1MACH, IT, IEND, ITER     
       REAL G(2,100), B(200), R(200)                                    
       REAL X, ERR, AMAX1, RNORM, BNORM, R1MACH, ABS                    
       DOUBLE PRECISION DBLE                                            
C CONSTRUCT MATRIX AND RIGHT HAND SIDE SO TRUE SOLUTION IS              
C COMPOSED ENTIRELY OF 1S                                               
       N=100                                                            
       X=1                                                              
       ML=2                                                             
       IG=2                                                             
       NM1=N-1                                                          
       DO 90 K=1,3                                                      
          DO 10 I=1,N                                                   
             G(1,I)=2.0                                                 
             G(2,I)=-1.0                                                
             B(I)=0.0                                                   
  10      CONTINUE                                                      
          G(1,1)=1.0+X                                                  
          G(1,N)=1.0+X                                                  
          B(1)=X                                                        
          B(N)=X                                                        
C SOLVE THE SYSTEM                                                      
          CALL BPLE(N,ML,G,IG,B,N,1)                                    
          IWRITE=I1MACH(2)                                              
          WRITE(IWRITE,11)X                                             
  11      FORMAT(/5H X IS,F16.8)                                        
C COMPUTE THE ERROR                                                     
          ERR=0.0                                                       
          DO 20 I=1,N                                                   
             ERR=AMAX1(ERR,ABS(B(I)-1.0))                               
  20      CONTINUE                                                      
          WRITE(IWRITE,21)ERR                                           
  21      FORMAT(22H FOR BPLE THE ERROR IS,F16.8)                       
          IEND=I1MACH(11)*IFIX(R1MACH(5)/ALOG10(2.0)+1.0)               
C FIND THE NORM OF THE SOLUTION                                         
          BNORM=0.0                                                     
          DO 30 I=1,N                                                   
              BNORM=AMAX1(BNORM,ABS(B(I)))                              
  30      CONTINUE                                                      
C REFINE THE SOLUTION                                                   
          DO 60 ITER=1,IEND                                             
             IT=ITER                                                    
C COMPUTE THE RESIDUAL R=B-AX, IN DOUBLE PRECISION                      
             DO 40 I=2,NM1                                              
                R(I)=DBLE(B(I-1))+DBLE(B(I+1))-2.D0*DBLE(B(I))          
  40         CONTINUE                                                   
             R(1)=X+B(2)-DBLE(1.0+X)*DBLE(B(1))                         
             R(N)=X+B(N-1)-DBLE(1.+X)*DBLE(B(N))                        
C SOLVE A(DELTAX)=R                                                     
             CALL BPFS(N,ML,G,IG,R,N,1)                                 
             CALL BPBS(N,ML,G,IG,R,N,1)                                 
C DETERMINE NORM OF CORRECTION AND ADD IN CORRECTION                    
             RNORM=0.0                                                  
             DO 50 I=1,N                                                
                B(I)=B(I)+R(I)                                          
                RNORM=RNORM+ABS(R(I))                                   
  50         CONTINUE                                                   
             IF(RNORM.LT.R1MACH(4)*BNORM) GO TO 70                      
  60      CONTINUE                                                      
          WRITE(IWRITE,61)                                              
  61      FORMAT(18H REFINEMENT FAILED)                                 
C COMPUTE NEW ERROR                                                     
  70      ERR=0.0                                                       
          DO 80 I=1,N                                                   
             ERR=AMAX1(ERR,ABS(B(I)-1.0))                               
  80      CONTINUE                                                      
          WRITE(IWRITE,81)IT,ERR                                        
  81      FORMAT(24H ERROR AFTER REFINEMENT ,I4,3H IS,E14.7)            
          X=X/100.0                                                     
  90   CONTINUE                                                         
       STOP                                                             
       END                                                              
C$TEST LPSM                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BPML                              
C                                                                       
C***********************************************************************
         INTEGER IG, N, MU, I, IWRITE, I1MACH                           
         REAL G(3,20), X(20), B(20)                                     
         REAL UNI, ERR, COND, SASUM, ABS                                
         IG=3                                                           
         N=10                                                           
         MU=3                                                           
C                                                                       
C CONSTRUCT MATRIX A AND PACK IT INTO G                                 
C                                                                       
          DO 10 I=1,N                                                   
             G(1,I)=4.0                                                 
             G(2,I)=-1.0                                                
             G(3,I)=-1.0                                                
  10      CONTINUE                                                      
C                                                                       
C CONSTRUCT A RANDOM VECTOR                                             
C                                                                       
          DO 20 I=1,N                                                   
             X(I)=UNI(0)                                                
  20      CONTINUE                                                      
C                                                                       
C CONSTRUCT B=AX                                                        
C                                                                       
          CALL BPML(N,MU,G,IG,X,B)                                      
C                                                                       
C SOLVE THE SYSTEM AX=B                                                 
C                                                                       
          CALL BPSS(N,MU,G,IG,B,N,1,COND)                               
C                                                                       
C PRINT OUT THE TRUE SOLUTION AND THE COMPUTED SOLUTION                 
C                                                                       
          IWRITE=I1MACH(2)                                              
          WRITE(IWRITE,21)                                              
  21      FORMAT(34H TRUE SOLUTION   COMPUTED SOLUTION)                 
          WRITE(IWRITE,22)(X(I),B(I),I=1,N)                             
  22      FORMAT(1H ,2E16.8)                                            
          ERR=0.0                                                       
          DO 30 I=1,N                                                   
              ERR=ERR+ABS(B(I)-X(I))                                    
  30      CONTINUE                                                      
          ERR=ERR/SASUM(N,X,1)                                          
          WRITE(IWRITE,31)ERR                                           
  31      FORMAT(19H RELATIVE ERROR IS ,1PE15.7)                        
          WRITE(IWRITE,32)COND                                          
  32      FORMAT(20H CONDITION NUMBER IS,1PE15.7)                       
          STOP                                                          
          END                                                           
C$TEST QBLC                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM BQUAD                             
C                                                                       
C***********************************************************************
      INTEGER NCALL,IWRITE,I1MACH                                       
      REAL SFUNC,X(3),ANS,ERREST,TRUERR                                 
      EXTERNAL SFUNC                                                    
      COMMON/COUNT/NCALL                                                
      NCALL=0                                                           
      X(1) = -1.0E0                                                     
      X(2) =   0.0E0                                                    
      X(3) = +1.0E0                                                     
C                                                                       
C  BQUAD WILL TAKE INTO ACCOUNT THE BREAK AT X=0                        
C                                                                       
      CALL BQUAD (SFUNC,3,X,1.E-6,ANS,ERREST)                           
      TRUERR=EXP(1.E0) - ANS                                            
      IWRITE=I1MACH(2)                                                  
      WRITE(IWRITE, 99) ANS, ERREST, TRUERR, NCALL                      
  99   FORMAT(1X,4HANS=,1PE15.7,10H   ERREST=,1PE12.3,                  
     1     10H   TRUERR=,1PE12.3/1X,6HNCALL=,I4)                        
      STOP                                                              
      END                                                               
      REAL FUNCTION SFUNC(X)                                            
      REAL X                                                            
      COMMON/COUNT/NCALL                                                
      NCALL = NCALL+1                                                   
      SFUNC = AMAX1(1.E0, EXP(X))                                       
      RETURN                                                            
      END                                                               
C$TEST CSPA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM CSPQU                             
C                                                                       
C***********************************************************************
      INTEGER NPTS,J,IWRITE,I1MACH                                      
      REAL PI,X(65),Y(65),ANS,ZZI                                       
      PI=3.14159265                                                     
C                                                                       
      NPTS=9                                                            
C                                                                       
C COMPUTE THE POINTS AT WHICH THE SPLINE IS TO BE FITTED                
C                                                                       
      DO 10 J=1,NPTS                                                    
      X(J)=FLOAT(J-1)/FLOAT(NPTS-1)                                     
      Y(J)=SIN(X(J)*PI/2.)                                              
   10 CONTINUE                                                          
C                                                                       
C THE INTEGRATION:                                                      
C                                                                       
      CALL CSPQU(X,Y,NPTS,X(1),X(NPTS),ANS)                             
C                                                                       
C ERROR IN INTEGRATION                                                  
C                                                                       
      ZZI=ANS-2./PI                                                     
C                                                                       
C                                                                       
C SET THE OUTPUT UNIT                                                   
C                                                                       
      IWRITE=I1MACH(2)                                                  
C                                                                       
      WRITE (IWRITE,9998) ANS                                           
 9998 FORMAT(48H THE INTEGRAL OF SINE(X*PI/2) FROM X=0 TO X=1 IS,E16.8) 
C                                                                       
      WRITE (IWRITE,9999) ZZI                                           
 9999 FORMAT(17H WITH AN ERROR OF,1PE10.2)                              
C                                                                       
      STOP                                                              
      END                                                               
C$TEST QODD                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM DODEQ                             
C                                                                       
C***********************************************************************
C                                                                       
      EXTERNAL F                                                        
C                                                                       
      INTEGER I1MACH,K,IWRITE                                           
      DOUBLE PRECISION TWOPI,EM1,ANS(10),EPS                            
      DOUBLE PRECISION CHK1,CHK2,DK,DTEMP                               
      DOUBLE PRECISION DATAN, DEXP, DSIN, DCOSP                         
C                                                                       
C  INITIALIZE 2*PI AND EXP - 1                                          
C                                                                       
      TWOPI = 8.0D0*DATAN(1.0D0)                                        
      EM1 = DEXP(1.0D0) - 1.0D0                                         
C                                                                       
C  SET OUTPUT WRITE UNIT                                                
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
C  SET ACCURACY PARAMETER                                               
C                                                                       
      EPS = 1.0D-10                                                     
C                                                                       
      WRITE(IWRITE,97) EPS                                              
 97   FORMAT(11H FOR EPS = ,1PD10.2,21H THE COEFFICIENTS ARE)           
      WRITE(IWRITE,98)                                                  
 98   FORMAT(/7X,1HK,11X,9HSIN COEFF,16X,9HCOS COEFF,10X,9HMAX ERROR)   
C                                                                       
      CALL DODEQ(10,F,0.0D0,1.0D0,EPS,ANS)                              
C                                                                       
      DO 10 K=1,5                                                       
      DK=K                                                              
      DTEMP = 1.D0 + (TWOPI*DK)**2                                      
      CHK1 = TWOPI*DK*(-EM1)/DTEMP - ANS(2*K-1)                         
      CHK2 = EM1/DTEMP - ANS(2*K)                                       
C                                                                       
      DTEMP = DMAX1( DABS(CHK1), DABS(CHK2) )                           
C                                                                       
 10   WRITE(IWRITE,99) K, ANS(2*K-1), ANS(2*K), DTEMP                   
 99   FORMAT (1H0,2X,I5,2D25.14,1PD15.4)                                
      STOP                                                              
      END                                                               
      SUBROUTINE F(X,Y,N,FVAL)                                          
C                                                                       
      INTEGER J, N                                                      
      DOUBLE PRECISION TWOPI,X,Y,EXPX,ANGL,FVAL(10)                     
C                                                                       
C  INITIALIZE 2*PI AND E**X                                             
C                                                                       
      TWOPI = 8.0D0*DATAN(1.0D0)                                        
      EXPX = DEXP(X)                                                    
C                                                                       
C  COMPUTE THE TWO INTEGRANDS, E**X * SIN AND E**X * COS,               
C                                                                       
      DO 20 J=1,5                                                       
         ANGL   = J                                                     
         ANGL   = TWOPI*ANGL*X                                          
         FVAL(2*J-1)   = EXPX*DSIN(ANGL)                                
  20     FVAL(2*J)     = EXPX*DCOS(ANGL)                                
C                                                                       
      RETURN                                                            
      END                                                               
C$TEST QGSG                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM GQ1                               
C                                                                       
C***********************************************************************
      REAL X(5),W(5),CALC,TRUE,ERR                                      
C                                                                       
      CALL  GQ1(5,X,W)                                                  
      IWRITE=I1MACH(2)                                                  
      WRITE(IWRITE,30)                                                  
      DO 10 J=1,5                                                       
   10    WRITE(IWRITE,40) J, X(J),W(J)                                  
      CALC = 0.E0                                                       
      DO 20 J=1,5                                                       
   20    CALC = CALC+W(J)*(1.0/(2.0+X(J)))                              
      TRUE = ALOG(3.E0)                                                 
      ERR  = TRUE-CALC                                                  
      WRITE(IWRITE,50) TRUE,CALC,ERR                                    
      STOP                                                              
   30 FORMAT(///13H TEST OF  GQ1//30H0ABSCISSAS AND WEIGHTS FOR N=5)    
   40 FORMAT(I4,0P2E16.7)                                               
   50 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/                       
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)                             
      END                                                               
C$TEST QGSH                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM GQEX                              
C                                                                       
C***********************************************************************
      REAL X(5),W(5),CALC,TRUE,PI,ERR                                   
C                                                                       
      CALL  GQEX(5,X,W)                                                 
      IWRITE=I1MACH(2)                                                  
      WRITE(IWRITE,30)                                                  
      DO 10 J=1,5                                                       
   10    WRITE(IWRITE,40) J, X(J),W(J)                                  
      CALC = 0.E0                                                       
      DO 20 J=1,5                                                       
   20    CALC = CALC+W(J)*X(J)/(1.0 - EXP(-X(J)))                       
      PI   = 2.E0*ATAN2(1.E0,0.E0)                                      
      TRUE = PI**2/6.E0                                                 
      ERR  = TRUE - CALC                                                
      WRITE(IWRITE,50) TRUE,CALC,ERR                                    
      STOP                                                              
   30 FORMAT(///14H TEST OF  GQEX//30H0ABSCISSAS AND WEIGHTS FOR N=5)   
   40 FORMAT(I4,0P2E16.7)                                               
   50 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/                       
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)                             
      END                                                               
C$TEST QGSJ                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM GQEX2                             
C                                                                       
C***********************************************************************
      REAL X(5),W(5),FEX2,CALC,TRUE,PI,ERR                              
C                                                                       
      CALL  GQEX2(5,X,W)                                                
      IWRITE=I1MACH(2)                                                  
      WRITE(IWRITE,1)                                                   
      DO 10 J=1,5                                                       
   10    WRITE(IWRITE,2) J, X(J),W(J)                                   
      CALC=0.E0                                                         
      DO 20 J=1,5                                                       
   20    CALC=CALC+W(J)*FEX2(X(J))                                      
      PI=2.E0*ATAN2(1.E0,0.E0)                                          
      TRUE=SQRT(PI)*EXP(-.25E0)                                         
      ERR=TRUE - CALC                                                   
      WRITE(IWRITE,3) TRUE,CALC,ERR                                     
      STOP                                                              
    1 FORMAT(///15H TEST OF  GQEX2//30H0ABSCISSAS AND WEIGHTS FOR N=5)  
    2 FORMAT(I4,0P2E16.7)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/                       
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)                             
      END                                                               
      REAL FUNCTION FEX2(X)                                             
      REAL X                                                            
      FEX2=COS(X)                                                       
      RETURN                                                            
      END                                                               
C$TEST QGSM                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM GQEXA                             
C                                                                       
C***********************************************************************
      REAL X(5),W(5),FEXA,CALC,TRUE,PI,ERR                              
C                                                                       
      CALL  GQEXA(5,-0.5E0,X,W)                                         
      IWRITE=I1MACH(2)                                                  
      WRITE(IWRITE,1)                                                   
      DO 10 J=1,5                                                       
   10    WRITE(IWRITE,2) J, X(J),W(J)                                   
      CALC = 0.E0                                                       
      DO 20 J=1,5                                                       
   20    CALC = CALC+W(J)*FEXA(X(J))                                    
      PI   = 2.E0*ATAN2(1.E0,0.E0)                                      
      TRUE = 0.5E0*SQRT(PI)*(1.E0-1.E0/SQRT(3.E0))                      
      ERR  = TRUE-CALC                                                  
      WRITE(IWRITE,3) TRUE,CALC,ERR                                     
      STOP                                                              
    1 FORMAT(///15H TEST OF  GQEXA//30H0ABSCISSAS AND WEIGHTS FOR N=5)  
    2 FORMAT(I4,0P2E16.7)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/                       
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)                             
      END                                                               
      REAL FUNCTION FEXA(X)                                             
      REAL X                                                            
      FEXA=0.5E0*(1.E0-EXP(-2.E0*X))                                    
      RETURN                                                            
      END                                                               
C$TEST QGSP                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM GQLOG                             
C                                                                       
C***********************************************************************
      REAL X(5),W(5),FLOG,CALC,TRUE,PI2,ERR                             
C                                                                       
      CALL  GQLOG(5,X,W)                                                
      IWRITE = I1MACH(2)                                                
      WRITE(IWRITE,1)                                                   
      DO 10 J = 1,5                                                     
   10    WRITE(IWRITE,2) J, X(J),W(J)                                   
      CALC = 0.E0                                                       
      DO 20 J = 1,5                                                     
   20    CALC = CALC+W(J)*FLOG(X(J))                                    
      PI2  = ATAN2(1.E0,0.E0)                                           
      TRUE = -(PI2**2/3.E0)                                             
      ERR  = TRUE - CALC                                                
      WRITE(IWRITE,3) TRUE,CALC,ERR                                     
      STOP                                                              
    1 FORMAT(///15H TEST OF  GQLOG//30H0ABSCISSAS AND WEIGHTS FOR N=5)  
    2 FORMAT(I4,0P2E16.7)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/                       
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)                             
      END                                                               
      REAL FUNCTION FLOG(X)                                             
      REAL X                                                            
      FLOG = -1.E0/(1.E0+X)                                             
      RETURN                                                            
      END                                                               
C$TEST QGSR                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM GQXA                              
C                                                                       
C***********************************************************************
      REAL X(5),W(5),FXA,CALC,TRUE,B(1),PI2,ERR                         
C                                                                       
      CALL  GQXA(5,-0.5E0,X,W)                                          
      IWRITE = I1MACH(2)                                                
      WRITE(IWRITE,1)                                                   
      DO 10 J=1,5                                                       
   10    WRITE(IWRITE,2) J, X(J),W(J)                                   
      CALC = 0.E0                                                       
      DO 20 J=1,5                                                       
   20    CALC = CALC+W(J)*FXA(X(J))                                     
      CALL BESRJ(1.E0,1,B)                                              
      PI2  = ATAN2(1.E0,0.E0)                                           
      TRUE = PI2*B(1)                                                   
      ERR  = TRUE-CALC                                                  
      WRITE(IWRITE,3) TRUE,CALC,ERR                                     
      STOP                                                              
    1 FORMAT(///14H TEST OF  GQXA//30H0ABSCISSAS AND WEIGHTS FOR N=5)   
    2 FORMAT(I4,0P2E16.7)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/                       
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)                             
      END                                                               
      REAL FUNCTION FXA(X)                                              
      REAL X                                                            
      FXA=COS(1.E0-X)/SQRT(2.E0-X)                                      
      RETURN                                                            
      END                                                               
C$TEST QGST                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM GQXAB                             
C                                                                       
C***********************************************************************
      REAL X(5),W(5),FXAB,CALC,TRUE,PI,ERR                              
C                                                                       
      CALL  GQXAB(5,-0.5E0,0.5E0,X,W)                                   
      IWRITE=I1MACH(2)                                                  
      WRITE(IWRITE,1)                                                   
      DO 10 J=1,5                                                       
   10    WRITE(IWRITE,2) J, X(J),W(J)                                   
      CALC = 0.E0                                                       
      DO 20 J=1,5                                                       
   20    CALC = CALC+W(J)*FXAB(X(J))                                    
      PI   = 2.E0*ATAN2(1.E0,0.E0)                                      
      TRUE = PI*(1.E0-1.E0/SQRT(3.E0))                                  
      ERR  = TRUE - CALC                                                
      WRITE(IWRITE,3) TRUE,CALC,ERR                                     
      STOP                                                              
    1 FORMAT(///15H TEST OF  GQXAB//30H0ABSCISSAS AND WEIGHTS FOR N=5)  
    2 FORMAT(I4,0P2E16.7)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/                       
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)                             
      END                                                               
      REAL FUNCTION FXAB(X)                                             
      REAL X                                                            
      FXAB = 1.E0/(2.E0+X)                                              
      RETURN                                                            
      END                                                               
C$TEST QBLG                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM QUAD                              
C                                                                       
C***********************************************************************
      EXTERNAL F                                                        
      COMMON /COUNTS/NMEVAL                                             
      INTEGER NMEVAL,IWRITE,I1MACH                                      
      REAL ERROR,RESULT                                                 
C                                                                       
C  INITIALIZE COUNT TO ZERO                                             
      NMEVAL = 0                                                        
C                                                                       
      CALL QUAD(F, 0.0, 1.0, 1.0E-4, RESULT, ERROR)                     
C                                                                       
      IWRITE = I1MACH(2)                                                
      WRITE (IWRITE, 10)                                                
   10 FORMAT(11X, 48H THE INTEGRAL OF X**0.5(LOGX) BETWEEN 0 AND 1 IS)  
      WRITE (IWRITE, 20) RESULT, ERROR                                  
   20 FORMAT(1H0,10X, E15.7, 21H WITH ESTIMATED ERROR, 1PE9.2)          
      WRITE (IWRITE, 30) NMEVAL                                         
   30 FORMAT(1H0, 14X,1H(,I2,38H FUNCTION EVALUATIONS WERE REQUIRED TO  
     1      /17X, 26H   PERFORM THE QUADRATURE) )                       
      STOP                                                              
      END                                                               
      REAL FUNCTION F(X)                                                
      COMMON /COUNTS/NMEVAL                                             
      INTEGER NMEVAL                                                    
      REAL X                                                            
C                                                                       
C  COMPUTE THE INTEGRAND                                                
      F = 0.0                                                           
      IF (X .NE. 0.0) F = SQRT(X)*ALOG(X)                               
C                                                                       
C  COUNT THE NUMBER OF TIMES F WAS CALLED                               
      NMEVAL = NMEVAL + 1                                               
      RETURN                                                            
      END                                                               
C$TEST QBLA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM RQUAD                             
C                                                                       
C***********************************************************************
      EXTERNAL F                                                        
      COMMON /COUNTS/NMEVAL                                             
      INTEGER NMEVAL,IWRITE,I1MACH                                      
      REAL F,RESULT,ERROR                                               
C                                                                       
C  INITIALIZE COUNT TO ZERO                                             
      NMEVAL = 0                                                        
C                                                                       
C  SET OUTPUT UNIT TO IWRITE                                            
      IWRITE = I1MACH(2)                                                
C                                                                       
      CALL RQUAD(F, 0.0, 1.0, 0.0, 1.0E-7, RESULT, ERROR)               
C                                                                       
      WRITE (IWRITE, 9996)                                              
 9996 FORMAT(13X, 42H THE INTEGRAL OF EXP(X) BETWEEN 0 AND 1 IS)        
      WRITE (IWRITE, 9997) RESULT, ERROR                                
 9997 FORMAT(1H0,11X, 1PE15.8, 20H WITH RELATIVE ERROR, 1PE9.2)         
      WRITE (IWRITE,9998) NMEVAL                                        
 9998 FORMAT(1H0, 13X,1H(,I2,38H FUNCTION EVALUATIONS WERE REQUIRED TO) 
      WRITE (IWRITE,9999)                                               
 9999 FORMAT(17X, 26H   PERFORM THE QUADRATURE) )                       
      STOP                                                              
      END                                                               
      REAL FUNCTION F(X)                                                
      COMMON /COUNTS/NMEVAL                                             
      INTEGER NMEVAL                                                    
      REAL X                                                            
C                                                                       
C  COMPUTE THE INTEGRAND                                                
      F = EXP( X )                                                      
C                                                                       
C  COUNT THE NUMBER OF TIMES F WAS CALLED                               
      NMEVAL = NMEVAL + 1                                               
      RETURN                                                            
      END                                                               
C$TEST SPLF                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM SPLNI                             
C                                                                       
C***********************************************************************
      INTEGER K,I,N,IWRITE,I1MACH,NT                                    
      REAL X(51),Y(51),T(100),A(100),SINT,TINT                          
C                                                                       
      K = 4                                                             
C                                                                       
C MAKE THE ABSCISSAE FOR THE FIT.                                       
C                                                                       
      CALL UMD(0.0E0,3.14E0,51,X)                                       
C                                                                       
C MAKE THE DATA.                                                        
C                                                                       
      DO 1000 I = 1, 51                                                 
      Y(I) = SIN(X(I))                                                  
 1000 CONTINUE                                                          
C                                                                       
C MAKE THE MESH.                                                        
C                                                                       
      N = 2                                                             
C                                                                       
      CALL MNPB(X,51,N,K,T,NT)                                          
C                                                                       
C DO THE FIT.                                                           
C                                                                       
      CALL DL2SF(X,Y,51,K,T,NT,A)                                       
C                                                                       
C EVALUATE THE SPLINE INTEGRAL AND THE TRUE INTEGRAL.                   
C                                                                       
      CALL SPLNI(K,T,NT,A,T(NT),1,SINT)                                 
C                                                                       
      TINT = 1.0E0-COS(3.14E0)                                          
C                                                                       
      IWRITE = I1MACH(2)                                                
      WRITE(IWRITE,1003) SINT,TINT                                      
 1003 FORMAT(18H SPLINE INTEGRAL =,E20.8//                              
     1   18H TRUE INTEGRAL   =,E20.8)                                   
C                                                                       
      STOP                                                              
C                                                                       
      END                                                               
C$TEST CDEX                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM CDEXP                             
C                                                                       
C***********************************************************************
      DOUBLE PRECISION A(2),EXPON(2)                                    
      IWRITE = I1MACH(2)                                                
C                                                                       
      A(1) = 3.D0                                                       
      A(2) = -1.D0                                                      
      CALL CDEXP(A,EXPON)                                               
C                                                                       
      WRITE(IWRITE,9999) A, EXPON                                       
 9999 FORMAT (18H THE EXPONENTIAL (,1PD10.4,2H, ,1PD11.4,8H) IS    //   
     1           4H   (,2PD25.18,2H, ,2PD26.18,1H))                     
C                                                                       
      STOP                                                              
      END                                                               
C$TEST CDLG                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM CDLOG                             
C                                                                       
C***********************************************************************
      DOUBLE PRECISION A(2),LOG(2)                                      
      IWRITE = I1MACH(2)                                                
C                                                                       
      A(1) = 2.D0                                                       
      A(2) = -1.D0                                                      
      CALL CDLOG(A,LOG)                                                 
C                                                                       
      WRITE(IWRITE,9999) A, LOG                                         
 9999 FORMAT (13H THE LOG OF (,1PD10.4,2H, ,1PD11.4,8H) IS    //        
     1           4H   (,1PD24.18,2H, ,1PD25.18,1H))                     
C                                                                       
      STOP                                                              
      END                                                               
C$TEST CPLA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM CPOLY                             
C                                                                       
C***********************************************************************
      INTEGER IWRITE,I1MACH,K                                           
      REAL CR(4), CI(4), ZR(3), ZI(3)                                   
C                                                                       
      CR(1) = 2.0                                                       
      CI(1) = 0.0                                                       
C                                                                       
      CR(2) = -8.0                                                      
      CI(2) = 13.0                                                      
C                                                                       
      CR(3) = 3.0                                                       
      CI(3) = 74.0                                                      
C                                                                       
      CR(4) = 135.0                                                     
      CI(4) = 105.0                                                     
C                                                                       
      CALL CPOLY(3, CR, CI, ZR, ZI)                                     
C                                                                       
      IWRITE = I1MACH(2)                                                
      WRITE(IWRITE,99) (ZR(K),ZI(K),K = 1,3)                            
  99  FORMAT(1H ,1P2E15.7)                                              
C                                                                       
      STOP                                                              
      END                                                               
C$TEST RPAD                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM DRPOLY                            
C                                                                       
C***********************************************************************
       INTEGER IWRITE,I1MACH,K                                          
       DOUBLE PRECISION COEFF(6), ZR(5), ZI(5)                          
C                                                                       
      COEFF(1) = 8.D0                                                   
      COEFF(2) = -84.D0                                                 
      COEFF(3) = 9.D0                                                   
      COEFF(4) = - 589.D0                                               
      COEFF(5) = 331.D0                                                 
      COEFF(6) = -2915.D0                                               
C                                                                       
      CALL DRPOLY( 5, COEFF, ZR, ZI )                                   
C                                                                       
      IWRITE = I1MACH(2)                                                
      WRITE(IWRITE,99) (ZR(K),ZI(K),K = 1,5)                            
  99  FORMAT(1H0,1P2E27.18)                                             
      STOP                                                              
      END                                                               
C$TEST MLLR                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM MULLR                             
C                                                                       
C***********************************************************************
      INTEGER ITER, MAXITR, IWRITE                                      
      REAL EPSF, EPSZ                                                   
      COMPLEX MULLR, TESTF, Z1, Z2, Z3, ZANS                            
      COMPLEX CEXP, CSIN                                                
      EXTERNAL TESTF                                                    
C                                                                       
C SET UP THE INITIAL GUESSES AND TOLERANCES                             
C                                                                       
      Z1 = CMPLX(2.0, 1.0)                                              
      Z2 = CMPLX(5.0, 4.0)                                              
      Z3 = CMPLX(3.0, 2.0)                                              
C                                                                       
      EPSZ = .00001                                                     
      EPSF = .000001                                                    
      MAXITR = 50                                                       
C                                                                       
      ZANS = MULLR(TESTF, Z1, Z2, Z3, EPSZ, EPSF, MAXITR, ITER)         
C                                                                       
C WRITE IWRITE THE ANSWER AND THE NUMBER OF ITERATIONS                  
C                                                                       
      IWRITE = I1MACH(2)                                                
      WRITE (IWRITE, 999) ZANS, ITER                                    
 999    FORMAT(1H ,12HTHE ZERO IS ,2F11.8, 3X,                          
     1     I3,21H ITERATIONS WERE USED)                                 
C                                                                       
C                                                                       
      STOP                                                              
      END                                                               
      COMPLEX FUNCTION TESTF(Z)                                         
C                                                                       
      COMPLEX Z                                                         
C                                                                       
       TESTF = Z*CEXP(Z) - CSIN(Z)                                      
C                                                                       
      RETURN                                                            
      END                                                               
C$TEST ZERA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM ZERO                              
C                                                                       
C***********************************************************************
      EXTERNAL F                                                        
      INTEGER IWRITE,I1MACH                                             
      REAL A,B,F,T,X,ZERO                                               
C                                                                       
      IWRITE = I1MACH(2)                                                
      A = 1.0                                                           
      B = 3.0                                                           
      T = 1.0E-7                                                        
      X=ZERO(F,A,B,T)                                                   
C                                                                       
      WRITE (IWRITE,9999) X                                             
 9999 FORMAT (17H THE ROOT IS X = ,1PE15.8)                             
C                                                                       
      STOP                                                              
      END                                                               
C                                                                       
      REAL FUNCTION F(X)                                                
      REAL X                                                            
      F=X*X - 4.                                                        
      RETURN                                                            
      END                                                               
C$TEST ZONA                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM ZONE                              
C                                                                       
C***********************************************************************
      EXTERNAL ROSEN                                                    
      INTEGER IWRITE, I1MACH                                            
      REAL X(2), FNORM                                                  
      IWRITE = I1MACH(2)                                                
C                                                                       
      X(1) = -1.2                                                       
      X(2) = +1.0                                                       
C                                                                       
      CALL ZONE( ROSEN, 2, X, 1.E-2, 100, FNORM )                       
C                                                                       
      WRITE ( IWRITE, 9999 ) X(1), X(2), FNORM                          
 9999 FORMAT ( 1P3E15.6 )                                               
      STOP                                                              
      END                                                               
      SUBROUTINE ROSEN ( N, X, F )                                      
      INTEGER N                                                         
      REAL X(2), F(2)                                                   
      F(1) = 10.0* ( X(2) - X(1)**2 )                                   
      F(2) = 1.0 - X(1)                                                 
      RETURN                                                            
      END                                                               
C$TEST ZONB                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM ZONEJ                             
C                                                                       
C***********************************************************************
      EXTERNAL ROSEN,MYJAC                                              
      INTEGER IWRITE,I1MACH                                             
      REAL X(2), FNORM                                                  
      IWRITE  =  I1MACH(2)                                              
C                                                                       
      X(1)  =  -1.2                                                     
      X(2)  =  +1.0                                                     
C                                                                       
      CALL ZONEJ( ROSEN, MYJAC, 2, X, 1.E-2, 100, FNORM )               
C                                                                       
      WRITE ( IWRITE, 9999 ) X(1), X(2), FNORM                          
 9999 FORMAT ( 1P3E15.6 )                                               
      STOP                                                              
      END                                                               
      SUBROUTINE ROSEN ( N, X, F )                                      
      INTEGER N                                                         
      REAL X(2), F(2)                                                   
      F(1)  =  10.0 * ( X(2) - X(1)**2 )                                
      F(2)  =  1.0 - X(1)                                               
      RETURN                                                            
      END                                                               
      SUBROUTINE MYJAC(ROSEN, N, X, F, DFDX, JUSED)                     
      EXTERNAL  ROSEN                                                   
      INTEGER N,JUSED                                                   
      REAL X(2), F(2), DFDX(2,2)                                        
C                                                                       
C  JACOBIAN OF ROSEN AT X                                               
C                                                                       
      DFDX(1,1)  =  -20.0*X(1)                                          
      DFDX(1,2)  =   10.0                                               
      DFDX(2,1)  =  -1.0                                                
      DFDX(2,2)  =   0.0                                                
      JUSED = 1                                                         
      RETURN                                                            
      END                                                               
C$TEST RNRM                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM RNORM                             
C                                                                       
C***********************************************************************
C  RNORM - FIRST 10 RANDOM DEVIATES                                     
C                                                                       
      REAL X                                                            
      IWRITE = I1MACH(2)                                                
C                                                                       
      DO 10 K=1,10                                                      
      X = RNORM(0)                                                      
      WRITE (IWRITE,99) X                                               
 99     FORMAT(1H ,F11.8)                                               
 10   CONTINUE                                                          
C                                                                       
      STOP                                                              
      END                                                               
C$TEST RANC                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM RANBYT                            
C                                                                       
C***********************************************************************
      INTEGER IBYTE(4),IWRITE,I1MACH,K                                  
      REAL R,RAND,UNI                                                   
C                                                                       
C  SET THE CORRECT OUTPUT UNIT                                          
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
C  PRINT OUT THE FIRST FIVE UNIFORM RANDOM VARIATES                     
C                                                                       
      DO 1  K = 1,5                                                     
      RAND = UNI(0)                                                     
  1   WRITE (IWRITE, 9997) RAND                                         
 9997 FORMAT(1H , E15.8)                                                
C                                                                       
C  NOW RESET TO THE ORIGINAL SEEDS                                      
C  AND SEE HOW THE VARIATES LOOK AS BIT PATTERNS                        
C  (WRITTEN IN OCTAL WITH INTEGER VALUES GIVEN UNDERNEATH)              
C                                                                       
      CALL RANSET(12345,1073)                                           
      DO 2  K = 1,5                                                     
      CALL RANBYT(R,IBYTE)                                              
      WRITE (IWRITE, 9998) R, IBYTE                                     
 9998 FORMAT(1H0, E15.8, 4(3X, O3))                                     
C                                                                       
      WRITE(IWRITE, 9999) IBYTE                                         
 9999 FORMAT(16X, 4(3X, I3))                                            
  2   CONTINUE                                                          
C                                                                       
      STOP                                                              
      END                                                               
C$TEST EXTR                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM EXTRMX                            
C                                                                       
C***********************************************************************
       INTEGER  IWRITE,IEXT(100),NEX,IMAX,IMIN,IMAG                     
       INTEGER  I1MACH,I,J                                              
       REAL     PI,STEP,X,F(100)                                        
C                                                                       
       IWRITE = I1MACH(2)                                               
       PI = 3.1415926532                                                
       STEP = 2.0*PI/99.0                                               
       DO 10 I=1,100                                                    
          X = STEP*FLOAT(I-1)                                           
  10      F(I) = EXP(-X)*COS(X)                                         
C                                                                       
       CALL EXTRMR(100,F,NEX,IEXT,IMAX,IMIN,IMAG)                       
C                                                                       
       WRITE(IWRITE,20)                                                 
  20   FORMAT(6X,9HEXTREMALS/5X,1HX,10X,4HF(X))                         
       DO 30 J=1,NEX                                                    
          I =IEXT(J)                                                    
          X = STEP*FLOAT(I-1)                                           
  30      WRITE(IWRITE,40) X,F(I)                                       
  40   FORMAT(2F10.5)                                                   
       STOP                                                             
       END                                                              
C$TEST FMTR                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM FRMATT                            
C                                                                       
C***********************************************************************
C  EXAMPLE PROGRAM TO FIND THE CORRECT FORMAT SPECIFICATIONS            
C  AND THEN PRINT OUT AN INTEGER, REAL AND DOUBLE-PRECISION ARRAY.      
C                                                                       
       INTEGER K, XINT(9), IWIDTH, IWRITE                               
       INTEGER WSP, MANTSP, WDP, MANTDP                                 
       REAL XREAL(9)                                                    
       DOUBLE PRECISION DFLOAT, XDP(9)                                  
C                                                                       
       IWRITE = I1MACH(2)                                               
C                                                                       
       DO 10 K=1,9                                                      
        XINT(K) = K                                                     
        XREAL(K) = FLOAT(K)                                             
        XDP(K) = DFLOAT(K)                                              
  10   CONTINUE                                                         
C                                                                       
       CALL FRMATI(IWIDTH)                                              
       CALL FRMATR(WSP, MANTSP)                                         
       CALL FRMATD(WDP, MANTDP)                                         
C                                                                       
C  TAKE ONE OFF THE MANTISSA WIDTH TO ALLOW FOR 1PEW.D FORMAT.          
C                                                                       
       MANTSP = MANTSP - 1                                              
       MANTDP = MANTDP - 1                                              
C                                                                       
       CALL APRNTI(XINT,  9, IWRITE, 80, IWIDTH)                        
       CALL APRNTR(XREAL, 9, IWRITE, 80, WSP, MANTSP)                   
       CALL APRNTD(XDP,   9, IWRITE, 80, WDP, MANTDP)                   
C                                                                       
       STOP                                                             
       END                                                              
C$TEST XKHI                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM IXKTH                             
C                                                                       
C***********************************************************************
C                                                                       
      COMMON/CSTAK/DSTAK(500)                                           
      INTEGER IWRITE, N, K                                              
      INTEGER X(10), XK                                                 
      INTEGER ISTAK(1000)                                               
      DOUBLE PRECISION DSTAK                                            
C                                                                       
      EQUIVALENCE (DSTAK(1),ISTAK(1))                                   
C                                                                       
C  SET OUTPUT UNIT TO IWRITE .                                          
      IWRITE = I1MACH(2)                                                
C                                                                       
      N = 8                                                             
      X(1) = 3.                                                         
      X(2) = 2.                                                         
      X(3) = 9.                                                         
      X(4) = 7.                                                         
      X(5) = 8.                                                         
      X(6) = 8.                                                         
      X(7) = 5.                                                         
      X(8) = 8.                                                         
C                                                                       
      WRITE (IWRITE,98)                                                 
 98    FORMAT(1H0,15H  K       IXKTH,//)                                
C                                                                       
      DO 10 K=1,8                                                       
      XK = IXKTH(N,K,X)                                                 
      WRITE (IWRITE,99) K, XK                                           
 99    FORMAT(1H ,I3,I10)                                               
 10   CONTINUE                                                          
      STOP                                                              
      END                                                               
C$TEST XKTH                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM XKTH                              
C                                                                       
C***********************************************************************
C                                                                       
      COMMON/CSTAK/DSTAK(500)                                           
      INTEGER IWRITE, I1MACH, N, K                                      
      REAL X(10), XK, XKTH                                              
      REAL RSTAK(1000)                                                  
      DOUBLE PRECISION DSTAK                                            
C                                                                       
      EQUIVALENCE (DSTAK(1),RSTAK(1))                                   
C                                                                       
C  SET OUTPUT UNIT TO IWRITE .                                          
      IWRITE = I1MACH(2)                                                
C                                                                       
      N = 8                                                             
      X(1) = 3.                                                         
      X(2) = 2.                                                         
      X(3) = 9.                                                         
      X(4) = 7.                                                         
      X(5) = 8.                                                         
      X(6) = 8.                                                         
      X(7) = 5.                                                         
      X(8) = 8.                                                         
C                                                                       
      WRITE (IWRITE,98)                                                 
 98    FORMAT(1H0,14H  K       XKTH//)                                  
C                                                                       
      DO 10 K=1,8                                                       
      XK = XKTH(N,K,X)                                                  
      WRITE (IWRITE,99) K, XK                                           
 99    FORMAT(1H ,I3,F10.1)                                             
 10   CONTINUE                                                          
      STOP                                                              
      END                                                               
C$TEST XKHD                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM DXKTH                             
C                                                                       
C***********************************************************************
C                                                                       
      COMMON/CSTAK/DSTAK(500)                                           
      INTEGER IWRITE, N, K                                              
      DOUBLE PRECISION X(10), XK, DXKTH                                 
      DOUBLE PRECISION DSTAK                                            
C                                                                       
C  SET OUTPUT UNIT TO IWRITE .                                          
      IWRITE = I1MACH(2)                                                
C                                                                       
      N = 8                                                             
      X(1) = 3.                                                         
      X(2) = 2.                                                         
      X(3) = 9.                                                         
      X(4) = 7.                                                         
      X(5) = 8.                                                         
      X(6) = 8.                                                         
      X(7) = 5.                                                         
      X(8) = 8.                                                         
C                                                                       
      WRITE (IWRITE,98)                                                 
 98    FORMAT(1H0,14H  K       XKTH//)                                  
C                                                                       
      DO 10 K=1,8                                                       
      XK = DXKTH(N,K,X)                                                 
      WRITE (IWRITE,99) K, XK                                           
 99    FORMAT(1H ,I3,D25.14)                                            
 10   CONTINUE                                                          
      STOP                                                              
      END                                                               
C$TEST APNR                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT APRNTX ROUTINES                           
C                                                                       
C***********************************************************************
      LOGICAL A(25)                                                     
      INTEGER B(25), IWRITE, I1MACH                                     
      REAL C(25)                                                        
      DOUBLE PRECISION D(25)                                            
      COMPLEX E(25)                                                     
C                                                                       
      IWRITE = I1MACH(2)                                                
      WRITE(IWRITE, 10)                                                 
   10 FORMAT(14H0LOGICAL ARRAY )                                        
      CALL SETL(25, .FALSE., A)                                         
      CALL APRNTL(A, 25, IWRITE, 80)                                    
C                                                                       
      WRITE(IWRITE, 20)                                                 
   20 FORMAT(14H0INTEGER ARRAY )                                        
      CALL SETI(25, -1, B)                                              
      CALL APRNTI(B, 25, IWRITE, 80, 4)                                 
C                                                                       
      WRITE(IWRITE, 30)                                                 
   30 FORMAT(11H0REAL ARRAY )                                           
      CALL SETR(25, 1.0, C)                                             
      CALL APRNTR(C, 25, IWRITE, 80, 12, 4)                             
C                                                                       
      WRITE(IWRITE, 40)                                                 
   40 FORMAT(23H0DOUBLE PRECISION ARRAY )                               
      CALL SETD(25, 1.0D0, D)                                           
      CALL APRNTD(D, 25, IWRITE, 80, 12, 4)                             
C                                                                       
      WRITE(IWRITE, 50)                                                 
   50 FORMAT(14H0COMPLEX ARRAY )                                        
      CALL SETC(25, CMPLX(1.0, -1.0), E)                                
      CALL APRNTC(E, 25, IWRITE, 80, 12, 4)                             
C                                                                       
      STOP                                                              
      END                                                               
C$TEST ERRK                                                             
C***********************************************************************
C                                                                       
C  EXAMPLE OF USE OF THE PORT PROGRAM STKDMP                            
C                                                                       
C***********************************************************************
C     SAMPLE USE OF THE STACK DUMP                                      
      INTEGER IPTR, ISTKGT                                              
C                                                                       
      COMMON  /CSTAK/   DSTAK                                           
      DOUBLE PRECISION  DSTAK(500)                                      
      INTEGER           ISTAK(1000)                                     
      LOGICAL           LSTAK(1000)                                     
      REAL              RSTAK(1000)                                     
      COMPLEX           CMSTAK(500)                                     
      EQUIVALENCE (DSTAK(1), ISTAK(1))                                  
      EQUIVALENCE (DSTAK(1), LSTAK(1))                                  
      EQUIVALENCE (DSTAK(1), RSTAK(1))                                  
      EQUIVALENCE (DSTAK(1), CMSTAK(1))                                 
C                                                                       
      IPTR = ISTKGT(25, 1)                                              
      CALL SETL(25, .FALSE., LSTAK(IPTR))                               
      IPTR = ISTKGT(25, 2)                                              
      CALL SETI(25, -1, ISTAK(IPTR))                                    
      IPTR = ISTKGT(25, 3)                                              
      CALL SETR(25, 1.0, RSTAK(IPTR))                                   
      IPTR = ISTKGT(25, 4)                                              
      CALL SETD(25, 1.0D0, DSTAK(IPTR))                                 
      IPTR = ISTKGT(25, 5)                                              
      CALL SETC(25, CMPLX(1.0, -1.0), CMSTAK(IPTR))                     
      IPTR = ISTKGT(25, 5)                                              
      CALL SETC(25, CMPLX(1.0, -1.0), CMSTAK(IPTR))                     
      CALL ISTKRL(1)                                                    
C                                                                       
      CALL STKDMP                                                       
      STOP                                                              
      END                                                               
C************END OF PORT 3 EXAMPLE PROGRAMS*****************************
