      REAL FUNCTION GAMMA(X)                                            
      REAL X                                                            
      INTEGER IWRITE, NTOTAL, N, MOD, NDEGR, I1MACH                     
      REAL PI, ALOG, XBIG, XMED, FACTOR, XX                             
      REAL YY, GAMINV, A(18), BIG                                       
      REAL TCHBP, SMALL, CONST                                          
      REAL P0, P1, P2, P3, P4, P5, P6, R1MACH                           
C J. L. BLUE, 23 DEC 77                                                 
C FOR X .LE. (-XMED), USE GAMMA(X)*GAMMA(1-X)=PI/SIN(PI*X)              
C FOR (-XMED).LE.X.LE.1 AND 2.LE.X.LE.XMED, USE X*GAMMA(X)=GAMMA(X+1)   
C FOR 1.LE.X.LE.2, USE CHEBYSHEV EXPANSION                              
C FOR XMED.LE.X, USE ASYMPTOTIC FORMULA                                 
C CALCULATE INVERSE OF GAMMA, THEN SEE IF IT IS INVERTIBLE,             
C TO AVOID OVERFLOW.                                                    
      DATA A( 1)/+1.06377 30078 05261 97553 E0/                         
      DATA A( 2)/-0.00498 55872 86840 03595 E0/                         
      DATA A( 3)/-0.06419 25436 10915 82288 E0/                         
      DATA A( 4)/+0.00506 57986 40286 08725 E0/                         
      DATA A( 5)/+0.00041 66091 38709 68886 E0/                         
      DATA A( 6)/-0.00008 04814 12497 84711 E0/                         
      DATA A( 7)/+0.00000 29600 11775 18802 E0/                         
      DATA A( 8)/+0.00000 02689 75996 44060 E0/                         
      DATA A( 9)/-0.00000 00333 96463 06868 E0/                         
      DATA A(10)/+0.00000 00010 89653 86454 E0/                         
      DATA A(11)/+0.00000 00000 51385 01863 E0/                         
      DATA A(12)/-0.00000 00000 06600 74100 E0/                         
      DATA A(13)/+0.00000 00000 00247 69163 E0/                         
      DATA A(14)/+0.00000 00000 00002 20039 E0/                         
      DATA A(15)/-0.00000 00000 00000 67072 E0/                         
      DATA A(16)/+0.00000 00000 00000 03132 E0/                         
      DATA A(17)/-0.00000 00000 00000 00039 E0/                         
      DATA A(18)/-0.00000 00000 00000 00003 E0/                         
      DATA NTOTAL/18/                                                   
      DATA P0/+0.83333 33333 33333 33315 54247 E-1/                     
      DATA P1/-0.27777 77777 77768 16225 53 E-2/                        
      DATA P2/+0.79365 07935 00350 248 E-3/                             
      DATA P3/-0.59523 79913 04301 2 E-3/                               
      DATA P4/+0.84171 38778 1295 E-3/                                  
      DATA P5/-0.19104 44077 728 E-2/                                   
      DATA P6/+0.57083 83526 1 E-2/                                     
      DATA PI   /+3.14159 26535 89793 23846E0/                          
      DATA NDEGR,XBIG,XMED,CONST,SMALL,BIG/0,0.E0,0.E0,0.E0,0.E0,0.E0/  
C FIRST-TIME SWITCH                                                     
      IF (NDEGR .GT. 0) GOTO 7                                          
         IF (I1MACH(12)+I1MACH(13) .LE. 0) GOTO 1                       
            SMALL = R1MACH(1)                                           
            GOTO  2                                                     
   1        SMALL = (2.0*R1MACH(4)+1.0)/R1MACH(2)                       
   2     BIG = (1.0-2.0*R1MACH(4))*R1MACH(2)                            
         XMED = 12.0E0                                                  
         XBIG = SQRT(R1MACH(2))                                         
         CONST = ALOG(SQRT(2.*PI))                                      
         YY = 0.                                                        
         NDEGR = NTOTAL+1                                               
   3     IF (YY .GE. R1MACH(3) .OR. NDEGR .LT. 4) GOTO  4               
            NDEGR = NDEGR-1                                             
            YY = YY+ABS(A(NDEGR))                                       
            GOTO  3                                                     
   4     NDEGR = NDEGR-1                                                
         IF (R1MACH(4) .GE. 1.E-20) GOTO 6                              
            IWRITE = I1MACH(2)                                          
            WRITE(IWRITE,  5)                                           
   5        FORMAT(48H GAMMA IS NOT FULL PRECISION, BUT ONLY 20 DIGITS) 
   6     CONTINUE                                                       
   7  XX = X                                                            
      IF (ABS(XX) .GE. XMED) GOTO 11                                    
         FACTOR = 1.                                                    
   8     IF (XX .GE. 1.) GOTO  9                                        
            FACTOR = FACTOR*XX                                          
            XX = XX+1.                                                  
            GOTO  8                                                     
   9     IF (XX .LE. 2.) GOTO  10                                       
            XX = XX-1.                                                  
            FACTOR = FACTOR*XX                                          
            GOTO  9                                                     
  10     GAMINV = TCHBP(NDEGR, A, XX, 1.0, 2.0)                         
         IF (X .LT. 1.) GAMINV = GAMINV*FACTOR                          
         IF (X .GT. 2.) GAMINV = GAMINV/FACTOR                          
         GOTO  14                                                       
  11     IF (X .LT. 0.) XX = 1.-X                                       
C ASYMPTOTIC REGION                                                     
         IF (XX .LE. XBIG) GOTO 12                                      
            GAMINV = 0.                                                 
            GOTO  13                                                    
  12        YY = 1./XX**2                                               
            GAMINV = EXP((0.5-XX)*ALOG(XX)+XX-CONST-(P0+YY*(P1+YY*(P2+  
     1         YY*(P3+YY*(P4+YY*(P5+YY*P6))))))/XX)                     
  13     CONTINUE                                                       
  14  IF (X .LE. (-XMED)) GOTO 17                                       
         IF (ABS(GAMINV) .GE. SMALL) GOTO 15                            
C/6S                                                                    
C           CALL SETERR(17H GAMMA - OVERFLOW, 17, 1, 1)                 
C/7S                                                                    
            CALL SETERR(' GAMMA - OVERFLOW', 17, 1, 1)                  
C/                                                                      
            GOTO  16                                                    
  15        GAMMA = 1.0/GAMINV                                          
  16     CONTINUE                                                       
         GOTO  20                                                       
C RANGE REDUCTION FOR SIN                                               
  17     N = 0.5-X                                                      
         XX = X+FLOAT(N)                                                
         IF (MOD(N, 2) .EQ. 1) XX = -XX                                 
         YY = SIN(PI*XX)/PI                                             
         IF (GAMINV .GT. ABS(YY)*BIG) GOTO 18                           
            GAMMA = GAMINV/YY                                           
            GOTO  19                                                    
C/6S                                                                    
C 18        CALL SETERR(17H GAMMA - OVERFLOW, 17, 1, 1)                 
C/7S                                                                    
  18        CALL SETERR(' GAMMA - OVERFLOW', 17, 1, 1)                  
C/                                                                      
  19     CONTINUE                                                       
  20  RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DGAMMA(X)                               
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION DFLOAT, DSQRT, DLOG, DEXP, DSIN                  
      DOUBLE PRECISION PI, XBIG, DTCHBP, XMED                           
      DOUBLE PRECISION FACTOR, XX, YY, GAMINV                           
      DOUBLE PRECISION A(18), BIG, SMALL, CONST, D1MACH                 
      DOUBLE PRECISION P0, P1, P2, P3, P4, P5, P6                       
      INTEGER IWRITE, NTOTAL, N, MOD, NDEGR, I1MACH                     
C FOR X .LE.(-XMED), USE DGAMMA(X)*DGAMMA(1-X)=PI/SIN(PI*X)             
C FOR (-XMED).LE.X.LE.1 AND 2.LE.X.LE.XMED, USE X*DGAMMA(X)=DGAMMA(X+1) 
C FOR 1.LE.X.LE.2, USE CHEBYSHEV EXPANSION                              
C FOR XMED.LE.X, USE ASYMPTOTIC FORMULA                                 
C CALCULATE INVERSE OF DGAMMA, THEN SEE IF IT IS INVERTIBLE,            
C TO AVOID OVERFLOW.                                                    
      DATA A( 1)/+1.06377 30078 05261 97553 D0/                         
      DATA A( 2)/-0.00498 55872 86840 03595 D0/                         
      DATA A( 3)/-0.06419 25436 10915 82288 D0/                         
      DATA A( 4)/+0.00506 57986 40286 08725 D0/                         
      DATA A( 5)/+0.00041 66091 38709 68886 D0/                         
      DATA A( 6)/-0.00008 04814 12497 84711 D0/                         
      DATA A( 7)/+0.00000 29600 11775 18802 D0/                         
      DATA A( 8)/+0.00000 02689 75996 44060 D0/                         
      DATA A( 9)/-0.00000 00333 96463 06868 D0/                         
      DATA A(10)/+0.00000 00010 89653 86454 D0/                         
      DATA A(11)/+0.00000 00000 51385 01863 D0/                         
      DATA A(12)/-0.00000 00000 06600 74100 D0/                         
      DATA A(13)/+0.00000 00000 00247 69163 D0/                         
      DATA A(14)/+0.00000 00000 00002 20039 D0/                         
      DATA A(15)/-0.00000 00000 00000 67072 D0/                         
      DATA A(16)/+0.00000 00000 00000 03132 D0/                         
      DATA A(17)/-0.00000 00000 00000 00039 D0/                         
      DATA A(18)/-0.00000 00000 00000 00003 D0/                         
      DATA NTOTAL/18/                                                   
      DATA P0/+0.83333 33333 33333 33315 54247 D-1/                     
      DATA P1/-0.27777 77777 77768 16225 53 D-2/                        
      DATA P2/+0.79365 07935 00350 248 D-3/                             
      DATA P3/-0.59523 79913 04301 2 D-3/                               
      DATA P4/+0.84171 38778 1295 D-3/                                  
      DATA P5/-0.19104 44077 728 D-2/                                   
      DATA P6/+0.57083 83526 1 D-2/                                     
      DATA PI/+3.14159 26535 89793 23846 D0/                            
      DATA NDEGR,XBIG,XMED,CONST,SMALL,BIG/0,0.D0,0.D0,0.D0,0.D0,0.D0/  
C FIRST-TIME SWITCH                                                     
      IF (NDEGR .GT. 0) GOTO 7                                          
         IF (I1MACH(14)+I1MACH(15) .LE. 0) GOTO 1                       
            SMALL = D1MACH(1)                                           
            GOTO  2                                                     
   1        SMALL = (2.0D0*D1MACH(4)+1.0D0)/D1MACH(2)                   
   2     BIG = (1.0D0-2.0D0*D1MACH(4))*D1MACH(2)                        
         XMED = 12.0D0                                                  
         XBIG = DSQRT(D1MACH(2))                                        
         CONST = DLOG(DSQRT(2.*PI))                                     
         YY = 0.                                                        
         NDEGR = NTOTAL+1                                               
   3     IF (YY .GE. D1MACH(3) .OR. NDEGR .LT. 4) GOTO  4               
            NDEGR = NDEGR-1                                             
            YY = YY+DABS(A(NDEGR))                                      
            GOTO  3                                                     
   4     NDEGR = NDEGR-1                                                
         IF (D1MACH(4) .GE. 1.D-20) GOTO 6                              
            IWRITE = I1MACH(2)                                          
            WRITE(IWRITE,  5)                                           
   5        FORMAT(49H DGAMMA IS NOT FULL PRECISION, BUT ONLY 20 DIGITS)
   6     CONTINUE                                                       
   7  XX = X                                                            
      IF (DABS(XX) .GE. XMED) GOTO 11                                   
         FACTOR = 1.D0                                                  
   8     IF (XX .GE. 1.D0) GOTO  9                                      
            FACTOR = FACTOR*XX                                          
            XX = XX+1.D0                                                
            GOTO  8                                                     
   9     IF (XX .LE. 2.D0) GOTO  10                                     
            XX = XX-1.D0                                                
            FACTOR = FACTOR*XX                                          
            GOTO  9                                                     
  10     GAMINV = DTCHBP(NDEGR, A, XX, 1.0D0, 2.0D0)                    
         IF (X .LT. 1.D0) GAMINV = GAMINV*FACTOR                        
         IF (X .GT. 2.D0) GAMINV = GAMINV/FACTOR                        
         GOTO  14                                                       
  11     IF (X .LT. 0.D0) XX = 1.D0-X                                   
C ASYMPTOTIC REGION                                                     
         IF (XX .LE. XBIG) GOTO 12                                      
            GAMINV = 0.D0                                               
            GOTO  13                                                    
  12        YY = 1.D0/XX**2                                             
            GAMINV = DEXP((0.5D0-XX)*DLOG(XX)+XX-CONST-(P0+YY*(P1+YY*(  
     1         P2+YY*(P3+YY*(P4+YY*(P5+YY*P6))))))/XX)                  
  13     CONTINUE                                                       
  14  IF (X .LE. (-XMED)) GOTO 17                                       
         IF (DABS(GAMINV) .GE. SMALL) GOTO 15                           
C/6S                                                                    
C           CALL SETERR(18H DGAMMA - OVERFLOW, 18, 1, 1)                
C/7S                                                                    
            CALL SETERR(' DGAMMA - OVERFLOW', 18, 1, 1)                 
C/                                                                      
            GOTO  16                                                    
  15        DGAMMA = 1.0D0/GAMINV                                       
  16     CONTINUE                                                       
         GOTO  20                                                       
C RANGE REDUCTION FOR SIN                                               
  17     N = 0.5-X                                                      
         XX = X+DFLOAT(N)                                               
         IF (MOD(N, 2) .EQ. 1) XX = -XX                                 
         YY = DSIN(PI*XX)/PI                                            
         IF (GAMINV .GT. DABS(YY)*BIG) GOTO 18                          
            DGAMMA = GAMINV/YY                                          
            GOTO  19                                                    
C/6S                                                                    
C 18        CALL SETERR(18H DGAMMA - OVERFLOW, 18, 1, 1)                
C/7S                                                                    
  18        CALL SETERR(' DGAMMA - OVERFLOW', 18, 1, 1)                 
C/                                                                      
  19     CONTINUE                                                       
  20  RETURN                                                            
      END                                                               
      SUBROUTINE BESRJ(X, NB, B)                                        
C                                                                       
C THIS ROUTINE CALCULATES BESSEL FUNCTIONS J OF REAL                    
C ARGUMENT AND INTEGER ORDER.                                           
C                                                                       
C      EXPLANATION OF VARIABLES IN THE CALLING SEQUENCE -               
C                                                                       
C X  -  REAL ARGUMENT FOR WHICH THE J*S                                 
C       ARE TO BE CALCULATED.                                           
C                                                                       
C NB -  INTEGER TYPE.  1 + HIGHEST ORDER TO BE CALCULATED.              
C       IT MUST BE POSITIVE.                                            
C                                                                       
C B  -  REAL VECTOR OF LENGTH NB, NEED NOT BE                           
C       INITIALIZED BY USER.  IF THE ROUTINE TERMINATES                 
C       NORMALLY IT RETURNS J(OR I)-SUB-ZERO                            
C       THROUGH J(OR I)-SUB-NB-MINUS-ONE OF X IN THIS                   
C       VECTOR.                                                         
C                                                                       
C                  ACCURACY OF THE COMPUTED VALUES -                    
C                                                                       
C       IN CASE OF AN ERROR, NOT ALL J*S                                
C       ARE CALCULATED TO THE DESIRED ACCURACY.                         
C                                                                       
C       THE SUBPROGRAM CALLED BY BESRJ, B1SLR,                          
C       RETURNS IN THE VARIABLE, NCALC, THE NUMBER CALCULATED CORRECTLY.
C                                                                       
C       LET NTEN BE THE LARGEST INTEGER K SUCH THAT 10**K IS MACHINE-   
C       REPRESENTABLE IN SINGLE PRECISION.                              
C       THEN NB.GT.NCALC.GT.0 WILL OCCUR IF NB.GT.MAGX AND ABS(J-       
C       SUB-NB-OF-X/J-SUB-MAGX+NP-OF-X).LT.10.**(NTEN/2), I.E. NB       
C       IS MUCH GREATER THAN MAGX.  IN THIS CASE, B(N) IS CALCU-        
C       LATED TO THE DESIRED ACCURACY FOR N.LE.NCALC, BUT FOR           
C       NCALC.LT.N.LE.NB, PRECISION IS LOST.  IF N.GT.NCALC AND         
C       ABS(B(NCALC)/B(N)).EQ.10**-K, THEN THE LAST K SIGNIFICANT       
C       FIGURES OF B(N) ARE ERRONEOUS.  IF THE USER WISHES TO           
C       CALCULATE B(N) TO HIGHER ACCURACY, HE SHOULD USE AN             
C       ASYMPTOTIC FORMULA FOR LARGE ORDER.                             
C                                                                       
      REAL X,B(1)                                                       
C                                                                       
C CHECK INPUT VALUES                                                    
C                                                                       
C       AN UPPER LIMIT OF 10000 IS SET ON THE MAGNITUDE OF X.           
C       BEAR IN MIND THAT IF ABS(X)=N, THEN AT LEAST N ITERATIONS       
C       OF THE BACKWARD RECURSION WILL BE EXECUTED.                     
C                                                                       
      MAGX = ABS(X)                                                     
C                                                                       
C/6S                                                                    
C     IF  (MAGX .GT. 10000) CALL SETERR(                                
C    1    33H BESRJ - X IS TOO BIG (MAGNITUDE),33,1,2)                  
C/7S                                                                    
      IF  (MAGX .GT. 10000) CALL SETERR(                                
     1    ' BESRJ - X IS TOO BIG (MAGNITUDE)',33,1,2)                   
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF  (NB .LT. 1) CALL SETERR(                                      
C    1    28H BESRJ - NB SHOULD = ORDER+1,28,2,2)                       
C/7S                                                                    
      IF  (NB .LT. 1) CALL SETERR(                                      
     1    ' BESRJ - NB SHOULD = ORDER+1',28,2,2)                        
C/                                                                      
C                                                                       
C BESRJ CALLS ON THE SUBPROGRAM,B1SLR,                                  
C WHICH IS SOOKNES  ORIGINAL BESLRI.                                    
C                                                                       
C THE ADDITIONAL INPUT ARGUMENTS REQUIRED FOR IT ARE -                  
C                                                                       
C IZE   INTEGER TYPE.  ZERO IF J*S ARE TO BE CALCULATED, 1              
C       IF I*S ARE TO BE CALCULATED.(THIRD ARGUMENT BELOW)              
C                                                                       
C NCALC INTEGER TYPE, NEED NOT BE INITIALIZED BY USER.                  
C       BEFORE USING THE RESULTS, IT SHOULD BE CHECKED THAT             
C       NCALC=NB, I.E. ALL ORDERS HAVE BEEN CALCULATED TO               
C       THE DESIRED ACCURACY.                                           
C                                                                       
      CALL B1SLR (X, NB, 0, B, NCALC)                                   
C                                                                       
C TEST IF ALL GOT COMPUTED OK                                           
C (SINCE SOME VALUES MAY BE OK, THIS IS A RECOVERABLE ERROR.)           
C                                                                       
      IF (NB .EQ. NCALC) RETURN                                         
C                                                                       
      NCALC = NCALC+10                                                  
C/6S                                                                    
C     CALL SETERR(                                                      
C    1    38H BESRJ - ONLY THIS MANY ANSWERS ARE OK,38,NCALC,1)         
C/7S                                                                    
      CALL SETERR(                                                      
     1    ' BESRJ - ONLY THIS MANY ANSWERS ARE OK',38,NCALC,1)          
C/                                                                      
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE BESRI(X, NB, B)                                        
C                                                                       
C THIS ROUTINE CALCULATES MODIFIED BESSEL FUNCTIONS I OF REAL           
C ARGUMENT AND INTEGER ORDER.                                           
C                                                                       
C      EXPLANATION OF VARIABLES IN THE CALLING SEQUENCE -               
C                                                                       
C X  -  REAL ARGUMENT FOR WHICH THE I*S                                 
C       ARE TO BE CALCULATED.                                           
C                                                                       
C NB -  INTEGER TYPE.  1 + HIGHEST ORDER TO BE CALCULATED.              
C       IT MUST BE POSITIVE.                                            
C                                                                       
C B  -  REAL VECTOR OF LENGTH NB, NEED NOT BE                           
C       INITIALIZED BY USER.  IF THE ROUTINE TERMINATES                 
C       NORMALLY IT RETURNS I-SUB-ZERO                                  
C       THROUGH I-SUB-NB-MINUS-ONE OF X IN THIS                         
C       VECTOR.                                                         
C                                                                       
C                  ACCURACY OF THE COMPUTED VALUES -                    
C                                                                       
C       IN CASE OF AN ERROR, NOT ALL I*S                                
C       ARE CALCULATED TO THE DESIRED ACCURACY.                         
C                                                                       
C       THE SUBPROGRAM CALLED BY BESRI, B1SLR,                          
C       RETURNS IN THE VARIABLE, NCALC, THE NUMBER CALCULATED CORRECTLY.
C                                                                       
C       LET NTEN BE THE LARGEST INTEGER K SUCH THAT 10**K IS MACHINE-   
C       REPRESENTABLE IN REAL.                                          
C       THEN NB.GT.NCALC.GT.0 WILL OCCUR IF NB.GT.MAGX AND ABS(I-       
C       SUB-NB-OF-X/I-SUB-MAGX+NP-OF-X).LT.10.**(NTEN/2), I.E. NB       
C       IS MUCH GREATER THAN MAGX.  IN THIS CASE, B(N) IS CALCU-        
C       LATED TO THE DESIRED ACCURACY FOR N.LE.NCALC, BUT FOR           
C       NCALC.LT.N.LE.NB, PRECISION IS LOST.  IF N.GT.NCALC AND         
C       ABS(B(NCALC)/B(N)).EQ.10**-K, THEN THE LAST K SIGNIFICANT       
C       FIGURES OF B(N) ARE ERRONEOUS.  IF THE USER WISHES TO           
C       CALCULATE B(N) TO HIGHER ACCURACY, HE SHOULD USE AN             
C       ASYMPTOTIC FORMULA FOR LARGE ORDER.                             
C                                                                       
      REAL X,B(1),R1MACH                                                
C                                                                       
C CHECK INPUT VALUES                                                    
C                                                                       
C       AN UPPER LIMIT OF 10000 IS SET ON THE MAGNITUDE OF X.           
C       BEAR IN MIND THAT IF ABS(X)=N, THEN AT LEAST N ITERATIONS       
C       OF THE BACKWARD RECURSION WILL BE EXECUTED.                     
C                                                                       
      MAGX = ABS(X)                                                     
      MEXP = ALOG(R1MACH(2))                                            
C                                                                       
C/6S                                                                    
C     IF  (MAGX .GT. 10000 .OR. MAGX .GT. MEXP) CALL SETERR(            
C    1    33H BESRI - X IS TOO BIG (MAGNITUDE),33,1,2)                  
C/7S                                                                    
      IF  (MAGX .GT. 10000 .OR. MAGX .GT. MEXP) CALL SETERR(            
     1    ' BESRI - X IS TOO BIG (MAGNITUDE)',33,1,2)                   
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF  (NB .LT. 1) CALL SETERR(                                      
C    1    28H BESRI - NB SHOULD = ORDER+1,28,2,2)                       
C/7S                                                                    
      IF  (NB .LT. 1) CALL SETERR(                                      
     1    ' BESRI - NB SHOULD = ORDER+1',28,2,2)                        
C/                                                                      
C                                                                       
C BESRJ CALLS ON THE SUBPROGRAM,B1SLR,                                  
C WHICH IS SOOKNES  ORIGINAL BESLRI.                                    
C                                                                       
C THE ADDITIONAL INPUT ARGUMENTS REQUIRED FOR IT ARE -                  
C                                                                       
C IZE   INTEGER TYPE.  ZERO IF J*S ARE TO BE CALCULATED, 1              
C       IF I*S ARE TO BE CALCULATED.(THIRD ARGUMENT BELOW)              
C                                                                       
C NCALC INTEGER TYPE, NEED NOT BE INITIALIZED BY USER.                  
C       BEFORE USING THE RESULTS, IT SHOULD BE CHECKED THAT             
C       NCALC=NB, I.E. ALL ORDERS HAVE BEEN CALCULATED TO               
C       THE DESIRED ACCURACY.                                           
C                                                                       
      CALL B1SLR (X, NB, 1, B, NCALC)                                   
C                                                                       
C TEST IF ALL GOT COMPUTED OK                                           
C (SINCE SOME VALUES MAY BE OK, THIS IS A RECOVERABLE ERROR.)           
C                                                                       
      IF (NB .EQ. NCALC) RETURN                                         
C                                                                       
      NCALC = NCALC+10                                                  
C/6S                                                                    
C     CALL SETERR(                                                      
C    1    38H BESRI - ONLY THIS MANY ANSWERS ARE OK,38,NCALC,1)         
C/7S                                                                    
      CALL SETERR(                                                      
     1    ' BESRI - ONLY THIS MANY ANSWERS ARE OK',38,NCALC,1)          
C/                                                                      
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE BESCJ(XR, XI, NB, BR, BI)                              
C                                                                       
C THIS ROUTINE CALCULATES BESSEL FUNCTIONS J OF COMPLEX                 
C ARGUMENT AND INTEGER ORDER.                                           
C                                                                       
C      EXPLANATION OF VARIABLES IN THE CALLING SEQUENCE -               
C                                                                       
C XR -  REAL PART OF THE COMPLEX ARGUMENT                               
C       FOR WHICH THE J*S ARE TO BE CALCULATED.                         
C                                                                       
C XI -  REAL IMAGINARY PART OF THE COMPLEX ARGUMENT                     
C       FOR WHICH THE J*S ARE TO BE CALCULATED.                         
C                                                                       
C NB -  INTEGER TYPE.  1 + HIGHEST ORDER TO BE CALCULATED.              
C       IT MUST BE POSITIVE.                                            
C                                                                       
C BR -  REAL VECTOR OF LENGTH NB, NEED NOT BE                           
C       INITIALIZED BY USER.  IF THE ROUTINE TERMINATES                 
C       NORMALLY IT RETURNS THE REAL PART OF J-SUB-ZERO                 
C       THROUGH J-SUB-NB-MINUS-ONE OF Z IN THIS                         
C       VECTOR.                                                         
C                                                                       
C BI -  IMAGINARY ANALOG OF BR.                                         
C                                                                       
C                  ACCURACY OF THE COMPUTED VALUES -                    
C                                                                       
C       IN CASE OF AN ERROR, NOT ALL J*S                                
C       ARE CALCULATED TO THE DESIRED ACCURACY.                         
C                                                                       
C       THE SUBPROGRAM CALLED BY BESCJ, B1SLC,                          
C       RETURNS IN THE VARIABLE, NCALC, THE NUMBER CALCULATED CORRECTLY.
C                                                                       
C       LET NTEN BE THE LARGEST INTEGER K SUCH THAT 10**K IS MACHINE-   
C       REPRESENTABLE IN SINGLE PRECISION.                              
C       THEN NB.GT.NCALC.GT.0 WILL OCCUR IF NB.GT.MAGZ AND ABS(J-       
C       SUB-NB-OF-Z/J-SUB-MAGZ+NP-OF-Z).LT.10.**(NTEN/2), I.E. NB       
C       IS MUCH GREATER THAN MAGZ.  IN THIS CASE, BR(N) AND BI(N)       
C       ARE CALCULATED TO THE DESIRED ACCURACY FOR N.LE.NCALC, BUT FOR  
C       NCALC.LT.N.LE.NB, PRECISION IS LOST.  IF N.GT.NCALC AND         
C       ABS(B(NCALC-1)/B(N-1)).EQ.10**-K, THEN THE LAST K SIGNIFICANT   
C       FIGURES OF B(N-1) (HERE B(N)=BR(N)+BI(N)) ARE ERRONEOUS.        
C       IF THE USER WISHES TO CALCULATE B(N) TO HIGHER ACCURACY,        
C       HE SHOULD USE AN ASYMPTOTIC FORMULA FOR LARGE ORDER.            
C                                                                       
      REAL XR,XI,BR(1),BI(1)                                            
C                                                                       
C CHECK INPUT VALUES                                                    
C                                                                       
C       AN UPPER LIMIT OF 10000 IS SET ON THE MAGNITUDE OF Z.           
C       BEAR IN MIND THAT IF ABS(Z)=N, THEN AT LEAST N ITERATIONS       
C       OF THE BACKWARD RECURSION WILL BE EXECUTED.                     
C                                                                       
      MAGZ = SQRT(XR*XR+XI*XI)                                          
      MEXP = ALOG(R1MACH(2))                                            
C                                                                       
C/6S                                                                    
C     IF  (MAGZ .GT. 10000 .OR. IFIX(ABS(XI)) .GT. MEXP) CALL SETERR(   
C    1    33H BESCJ - Z IS TOO BIG (MAGNITUDE),33,1,2)                  
C/7S                                                                    
      IF  (MAGZ .GT. 10000 .OR. IFIX(ABS(XI)) .GT. MEXP) CALL SETERR(   
     1    ' BESCJ - Z IS TOO BIG (MAGNITUDE)',33,1,2)                   
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF  (NB .LT. 1) CALL SETERR(                                      
C    1    28H BESCJ - NB SHOULD = ORDER+1,28,2,2)                       
C/7S                                                                    
      IF  (NB .LT. 1) CALL SETERR(                                      
     1    ' BESCJ - NB SHOULD = ORDER+1',28,2,2)                        
C/                                                                      
C                                                                       
C BESCJ CALLS ON THE SUBPROGRAM,B1SLC,                                  
C WHICH IS SOOKNES  ORIGINAL BESLCI.                                    
C                                                                       
C THE ADDITIONAL INPUT ARGUMENTS REQUIRED FOR IT ARE -                  
C                                                                       
C IZE   INTEGER TYPE.  ZERO IF J*S ARE TO BE CALCULATED, 1              
C       IF I*S ARE TO BE CALCULATED.(FOURTH ARGUMENT BELOW)             
C                                                                       
C NCALC INTEGER TYPE, NEED NOT BE INITIALIZED BY USER.                  
C       BEFORE USING THE RESULTS, IT SHOULD BE CHECKED THAT             
C       NCALC=NB, I.E. ALL ORDERS HAVE BEEN CALCULATED TO               
C       THE DESIRED ACCURACY.                                           
C                                                                       
      CALL  B1SLC (XR, XI, NB, 0, BR, BI, NCALC)                        
C                                                                       
C TEST IF ALL GOT COMPUTED OK                                           
C (SINCE SOME VALUES MAY BE OK, THIS IS A RECOVERABLE ERROR.)           
C                                                                       
      IF (NB .EQ. NCALC) RETURN                                         
C                                                                       
      NCALC = NCALC+10                                                  
C/6S                                                                    
C     CALL SETERR(                                                      
C    1    38H BESCJ - ONLY THIS MANY ANSWERS ARE OK,38,NCALC,1)         
C/7S                                                                    
      CALL SETERR(                                                      
     1    ' BESCJ - ONLY THIS MANY ANSWERS ARE OK',38,NCALC,1)          
C/                                                                      
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE BESCI(XR, XI, NB, BR, BI)                              
C                                                                       
C THIS ROUTINE CALCULATES BESSEL FUNCTIONS I OF COMPLEX                 
C ARGUMENT AND INTEGER ORDER.                                           
C                                                                       
C      EXPLANATION OF VARIABLES IN THE CALLING SEQUENCE -               
C                                                                       
C XR -  REAL PART OF THE COMPLEX ARGUMENT                               
C       FOR WHICH THE I*S ARE TO BE CALCULATED.                         
C                                                                       
C XI -  REAL IMAGINARY PART OF THE COMPLEX ARGUMENT                     
C       FOR WHICH THE I*S ARE TO BE CALCULATED.                         
C                                                                       
C NB -  INTEGER TYPE.  1 + HIGHEST ORDER TO BE CALCULATED.              
C       IT MUST BE POSITIVE.                                            
C                                                                       
C BR -  REAL VECTOR OF LENGTH NB, NEED NOT BE                           
C       INITIALIZED BY USER.  IF THE ROUTINE TERMINATES                 
C       NORMALLY IT RETURNS THE REAL PART OF I-SUB-ZERO                 
C       THROUGH I-SUB-NB-MINUS-ONE OF Z IN THIS                         
C       VECTOR.                                                         
C                                                                       
C BI -  IMAGINARY ANALOG OF BR.                                         
C                                                                       
C                  ACCURACY OF THE COMPUTED VALUES -                    
C                                                                       
C       IN CASE OF AN ERROR, NOT ALL I*S                                
C       ARE CALCULATED TO THE DESIRED ACCURACY.                         
C                                                                       
C       THE SUBPROGRAM CALLED BY BESCI, B1SLC,                          
C       RETURNS IN THE VARIABLE, NCALC, THE NUMBER CALCULATED CORRECTLY.
C                                                                       
C       LET NTEN BE THE LARGEST INTEGER K SUCH THAT 10**K IS MACHINE-   
C       REPRESENTABLE IN SINGLE PRECISION.                              
C       THEN NB.GT.NCALC.GT.0 WILL OCCUR IF NB.GT.MAGZ AND ABS(I-       
C       SUB-NB-OF-Z/I-SUB-MAGZ+NP-OF-Z).LT.10.**(NTEN/2), I.E. NB       
C       IS MUCH GREATER THAN MAGZ.  IN THIS CASE, BR(N) AND BI(N)       
C       ARE CALCULATED TO THE DESIRED ACCURACY FOR N.LE.NCALC, BUT FOR  
C       NCALC.LT.N.LE.NB, PRECISION IS LOST.  IF N.GT.NCALC AND         
C       ABS(B(NCALC-1)/B(N-1)).EQ.10**-K, THEN THE LAST K SIGNIFICANT   
C       FIGURES OF B(N-1) (HERE B(N)=BR(N)+BI(N)) ARE ERRONEOUS.        
C       IF THE USER WISHES TO CALCULATE B(N) TO HIGHER ACCURACY,        
C       HE SHOULD USE AN ASYMPTOTIC FORMULA FOR LARGE ORDER.            
C                                                                       
      REAL XR,XI,BR(1),BI(1)                                            
C                                                                       
C CHECK INPUT VALUES                                                    
C                                                                       
C       AN UPPER LIMIT OF 10000 IS SET ON THE MAGNITUDE OF Z.           
C       BEAR IN MIND THAT IF ABS(Z)=N, THEN AT LEAST N ITERATIONS       
C       OF THE BACKWARD RECURSION WILL BE EXECUTED.                     
C                                                                       
      MAGZ = SQRT(XR*XR+XI*XI)                                          
      MEXP = ALOG(R1MACH(2))                                            
C                                                                       
C/6S                                                                    
C     IF  (MAGZ .GT. 10000 .OR. IFIX(ABS(XR)) .GT. MEXP) CALL SETERR(   
C    1    33H BESCI - Z IS TOO BIG (MAGNITUDE),33,1,2)                  
C/7S                                                                    
      IF  (MAGZ .GT. 10000 .OR. IFIX(ABS(XR)) .GT. MEXP) CALL SETERR(   
     1    ' BESCI - Z IS TOO BIG (MAGNITUDE)',33,1,2)                   
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF  (NB .LT. 1) CALL SETERR(                                      
C    1    28H BESCI - NB SHOULD = ORDER+1,28,2,2)                       
C/7S                                                                    
      IF  (NB .LT. 1) CALL SETERR(                                      
     1    ' BESCI - NB SHOULD = ORDER+1',28,2,2)                        
C/                                                                      
C                                                                       
C BESCI CALLS ON THE SUBPROGRAM,B1SLC,                                  
C WHICH IS SOOKNES  ORIGINAL BESLCI.                                    
C                                                                       
C THE ADDITIONAL INPUT ARGUMENTS REQUIRED FOR IT ARE -                  
C                                                                       
C IZE   INTEGER TYPE.  ZERO IF I*S ARE TO BE CALCULATED, 1              
C       IF I*S ARE TO BE CALCULATED.(FOURTH ARGUMENT BELOW)             
C                                                                       
C NCALC INTEGER TYPE, NEED NOT BE INITIALIZED BY USER.                  
C       BEFORE USING THE RESULTS, IT SHOULD BE CHECKED THAT             
C       NCALC=NB, I.E. ALL ORDERS HAVE BEEN CALCULATED TO               
C       THE DESIRED ACCURACY.                                           
C                                                                       
      CALL  B1SLC (XR, XI, NB, 1, BR, BI, NCALC)                        
C                                                                       
C TEST IF ALL GOT COMPUTED OK                                           
C (SINCE SOME VALUES MAY BE OK, THIS IS A RECOVERABLE ERROR.)           
C                                                                       
      IF (NB .EQ. NCALC) RETURN                                         
C                                                                       
      NCALC = NCALC+10                                                  
C/6S                                                                    
C     CALL SETERR(                                                      
C    1    38H BESCI - ONLY THIS MANY ANSWERS ARE OK,38,NCALC,1)         
C/7S                                                                    
      CALL SETERR(                                                      
     1    ' BESCI - ONLY THIS MANY ANSWERS ARE OK',38,NCALC,1)          
C/                                                                      
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE B1SLR(X,NB,IZE,B,NCALC)                                
C THIS ROUTINE CALCULATES BESSEL FUNCTIONS I AND J OF REAL              
C ARGUMENT AND INTEGER ORDER.                                           
C                                                                       
C                                                                       
C      EXPLANATION OF VARIABLES IN THE CALLING SEQUENCE                 
C                                                                       
C X     REAL ARGUMENT FOR WHICH I*S OR J*S                              
C       ARE TO BE CALCULATED.  IF I*S ARE TO BE CALCULATED,             
C       ABS(X) MUST BE LESS THAN EXPARG (WHICH SEE BELOW).              
C NB    INTEGER TYPE.  1 + HIGHEST ORDER TO BE CALCULATED.              
C       IT MUST BE POSITIVE.                                            
C IZE   INTEGER TYPE.  ZERO IF J*S ARE TO BE CALCULATED, 1              
C       IF I*S ARE TO BE CALCULATED.                                    
C B     REAL VECTOR OF LENGTH NB, NEED NOT BE                           
C       INITIALIZED BY USER.  IF THE ROUTINE TERMINATES                 
C       NORMALLY (NCALC=NB), IT RETURNS J(OR I)-SUB-ZERO                
C       THROUGH J(OR I)-SUB-NB-MINUS-ONE OF X IN THIS                   
C       VECTOR.                                                         
C NCALC INTEGER TYPE, NEED NOT BE INITIALIZED BY USER.                  
C       BEFORE USING THE RESULTS, THE USER SHOULD CHECK THAT            
C       NCALC=NB, I.E. ALL ORDERS HAVE BEEN CALCULATED TO               
C       THE DESIRED ACCURACY.  SEE ERROR RETURNS BELOW.                 
C                                                                       
C                                                                       
C     EXPLANATION OF MACHINE-DEPENDENT CONSTANTS                        
C                                                                       
C NSIG  DECIMAL SIGNIFICANCE DESIRED.  SHOULD BE SET TO                 
C       IFIX(ALOG10(2)*NBIT+1), WHERE NBIT IS THE NUMBER OF             
C       BITS IN THE MANTISSA OF A REAL VARIABLE.                        
C       SETTING NSIG LOWER WILL RESULT IN DECREASED ACCURACY            
C       WHILE SETTING NSIG HIGHER WILL INCREASE CPU TIME                
C       WITHOUT INCREASING ACCURACY.  THE TRUNCATION ERROR              
C       IS LIMITED TO T=.5*10**-NSIG FOR J*S OF ORDER LESS              
C       THAN ARGUMENT, AND TO A RELATIVE ERROR OF T FOR                 
C       I*S AND THE OTHER J*S.                                          
C NTEN  LARGEST INTEGER K SUCH THAT 10**K IS MACHINE-                   
C       REPRESENTABLE IN SINGLE PRECISION.                              
C LARGEX UPPER LIMIT ON THE MAGNITUDE OF X.  BEAR IN MIND               
C       THAT IF ABS(X)=N, THEN AT LEAST N ITERATIONS OF THE             
C       BACKWARD RECURSION WILL BE EXECUTED.                            
C EXPARG LARGEST REAL ARGUMENT THAT THE LIBRARY                         
C       EXP ROUTINE CAN HANDLE.                                         
C                                                                       
C PORT NOTE, SEPTEMBER 8,1976 -                                         
C THE LARGEX AND EXPARG TESTS ARE MADE IN THE OUTER ROUTINES -          
C BESRJ AND BESRI, WHICH CALL B1SLR.                                    
C                                                                       
C                                                                       
C                  ERROR RETURNS                                        
C                                                                       
C PORT NOTE, SEPTEMBER 8, 1976 -                                        
C THE NOTES BELOW ARE KEPT IN FOR THE RECORD, BUT, AS ABOVE,            
C THE ACTUAL TESTS ARE NOW IN THE OUTER CALLING ROUTINES.               
C                                                                       
C       LET G DENOTE EITHER I OR J.                                     
C       IN CASE OF AN ERROR, NCALC.NE.NB, AND NOT ALL G*S               
C  ARE CALCULATED TO THE DESIRED ACCURACY.                              
C       IF NCALC.LT.0, AN ARGUMENT IS OUT OF RANGE.  NB.LE.0            
C  OR IZE IS NEITHER 0 NOR 1 OR IZE=1 AND ABS(X).GE.EXPARG.             
C  IN THIS CASE, THE B-VECTOR IS NOT CALCULATED, AND NCALC              
C  IS SET TO MIN0(NB,0)-1 SO NCALC.NE.NB.                               
C       NB.GT.NCALC.GT.0 WILL OCCUR IF NB.GT.MAGX AND ABS(G-            
C  SUB-NB-OF-X/G-SUB-MAGX+NP-OF-X).LT.10.**(NTEN/2), I.E. NB            
C  IS MUCH GREATER THAN MAGX.  IN THIS CASE, B(N) IS CALCU-             
C  LATED TO THE DESIRED ACCURACY FOR N.LE.NCALC, BUT FOR                
C  NCALC.LT.N.LE.NB, PRECISION IS LOST.  IF N.GT.NCALC AND              
C  ABS(B(NCALC)/B(N)).EQ.10**-K, THEN THE LAST K SIGNIFICANT            
C  FIGURES OF B(N) ARE ERRONEOUS.  IF THE USER WISHES TO                
C  CALCULATE B(N) TO HIGHER ACCURACY, HE SHOULD USE AN                  
C  ASYMPTOTIC FORMULA FOR LARGE ORDER.                                  
C                                                                       
      REAL                                                              
     1 X,B,P,TEST,TEMPA,TEMPB,TEMPC,SIGN,SUM,TOVER,                     
     2 PLAST,POLD,PSAVE,PSAVEL,R1MACH                                   
      DIMENSION B(NB)                                                   
      DATA NSIG/0/, NTEN/0/                                             
      IF(NSIG .NE. 0) GO TO 1                                           
      NSIG = IFIX(-ALOG10(R1MACH(3))+1.)                                
      NTEN = ALOG10(R1MACH(2))                                          
    1 TEMPA=ABS(X)                                                      
      MAGX=IFIX((TEMPA))                                                
C                                                                       
      SIGN=FLOAT(1-2*IZE)                                               
      NCALC=NB                                                          
C USE 2-TERM ASCENDING SERIES FOR SMALL X                               
      IF(TEMPA**4.LT..1E0**NSIG) GO TO 30                               
C INITIALIZE THE CALCULATION OF P*S                                     
      NBMX=NB-MAGX                                                      
      N=MAGX+1                                                          
      PLAST=1.E0                                                        
      P=FLOAT(2*N)/TEMPA                                                
C CALCULATE GENERAL SIGNIFICANCE TEST                                   
      TEST=2.E0*1.E1**NSIG                                              
      IF(IZE.EQ.1.AND.2*MAGX.GT.5*NSIG) TEST=SQRT(TEST*P)               
      IF(IZE.EQ.1.AND.2*MAGX.LE.5*NSIG) TEST=TEST/1.585**MAGX           
      M=0                                                               
      IF(NBMX.LT.3) GO TO 4                                             
C CALCULATE P*S UNTIL N=NB-1.  CHECK FOR POSSIBLE OVERFLOW.             
      TOVER=1.E1**(NTEN-NSIG)                                           
      NSTART=MAGX+2                                                     
      NEND=NB-1                                                         
      DO 3 N=NSTART,NEND                                                
      POLD=PLAST                                                        
      PLAST=P                                                           
      P=FLOAT(2*N)*PLAST/TEMPA-SIGN*POLD                                
      IF(P-TOVER) 3,3,5                                                 
    3 CONTINUE                                                          
C CALCULATE SPECIAL SIGNIFICANCE TEST FOR NBMX.GT.2.                    
      TEST=AMAX1(TEST,SQRT(PLAST*1.E1**NSIG)*SQRT(2.E0*P))              
C CALCULATE P*S UNTIL SIGNIFICANCE TEST PASSES                          
    4 N=N+1                                                             
      POLD=PLAST                                                        
      PLAST=P                                                           
      P=FLOAT(2*N)*PLAST/TEMPA-SIGN*POLD                                
      IF(P.LT.TEST) GO TO 4                                             
      IF(IZE.EQ.1.OR.M.EQ.1) GO TO 12                                   
C FOR J*S, A STRONG VARIANT OF THE TEST IS NECESSARY.                   
C CALCULATE IT, AND CALCULATE P*S UNTIL THIS TEST IS PASSED.            
      M=1                                                               
      TEMPB=P/PLAST                                                     
      TEMPC=FLOAT(N+1)/TEMPA                                            
      IF(TEMPB+1.E0/TEMPB.GT.2.E0*TEMPC)TEMPB=TEMPC+SQRT(TEMPC**2-1.E0) 
      TEST=TEST/SQRT(TEMPB-1.E0/TEMPB)                                  
      IF(P-TEST) 4,12,12                                                
C TO AVOID OVERFLOW, DIVIDE P*S BY TOVER.  CALCULATE P*S                
C UNTIL ABS(P).GT.1.                                                    
    5 TOVER=1.E1**NTEN                                                  
      P=P/TOVER                                                         
      PLAST=PLAST/TOVER                                                 
      PSAVE=P                                                           
      PSAVEL=PLAST                                                      
      NSTART=N+1                                                        
    6 N=N+1                                                             
      POLD=PLAST                                                        
      PLAST=P                                                           
      P=FLOAT(2*N)*PLAST/TEMPA-SIGN*POLD                                
      IF(P.LE.1.E0) GO TO 6                                             
      TEMPB=FLOAT(2*N)/TEMPA                                            
      IF(IZE.EQ.1) GO TO 8                                              
      TEMPC=.5E0*TEMPB                                                  
      TEMPB=PLAST/POLD                                                  
      IF(TEMPB+1.E0/TEMPB.GT.2.E0*TEMPC)TEMPB=TEMPC+SQRT(TEMPC**2-1.E0) 
C CALCULATE BACKWARD TEST, AND FIND NCALC, THE HIGHEST N                
C SUCH THAT THE TEST IS PASSED.                                         
    8 TEST=.5E0*POLD*PLAST*(1.E0-1.E0/TEMPB**2)/1.E1**NSIG              
      P=PLAST*TOVER                                                     
      N=N-1                                                             
      NEND=MIN0(NB,N)                                                   
      DO 9 NCALC=NSTART,NEND                                            
      POLD=PSAVEL                                                       
      PSAVEL=PSAVE                                                      
      PSAVE=FLOAT(2*N)*PSAVEL/TEMPA-SIGN*POLD                           
      IF(PSAVE*PSAVEL-TEST) 9,9,10                                      
    9 CONTINUE                                                          
      NCALC=NEND+1                                                      
   10 NCALC=NCALC-1                                                     
C THE SUM B(1)+2B(3)+2B(5)... IS USED TO NORMALIZE.  M, THE             
C COEFFICIENT OF B(N), IS INITIALIZED TO 2 OR 0.                        
   12 N=N+1                                                             
      M=2*N-4*(N/2)                                                     
C INITIALIZE THE BACKWARD RECURSION AND THE NORMALIZATION               
C SUM                                                                   
      TEMPB=0.E0                                                        
      TEMPA=1.E0/P                                                      
      SUM=FLOAT(M)*TEMPA                                                
      NEND=N-NB                                                         
      IF(NEND) 17,15,13                                                 
C RECUR BACKWARD VIA DIFFERENCE EQUATION, CALCULATING (BUT              
C NOT STORING) B(N), UNTIL N=NB.                                        
   13 DO 14 L=1,NEND                                                    
      N=N-1                                                             
      TEMPC=TEMPB                                                       
      TEMPB=TEMPA                                                       
      TEMPA=FLOAT(2*N)*TEMPB/X-SIGN*TEMPC                               
      M=2-M                                                             
   14 SUM=SUM+FLOAT(M)*TEMPA                                            
C STORE B(NB)                                                           
   15 B(N)=TEMPA                                                        
      IF(NB.GT.1) GO TO 16                                              
C NB=1.  SINCE 2*TEMPA WAS ADDED TO THE SUM, TEMPA MUST BE              
C SUBTRACTED                                                            
      SUM=SUM-TEMPA                                                     
      GO TO 23                                                          
C CALCULATE AND STORE B(NB-1)                                           
   16 N=N-1                                                             
      B(N) =FLOAT(2*N)*TEMPA/X-SIGN*TEMPB                               
      IF(N.EQ.1) GO TO 22                                               
      M=2-M                                                             
      SUM=SUM+FLOAT(M)*B(N)                                             
      GO TO 19                                                          
C N.LT.NB, SO STORE B(N) AND SET HIGHER ORDERS TO ZERO                  
   17 B(N)=TEMPA                                                        
      NEND=-NEND                                                        
      DO 18 L=1,NEND                                                    
      K=N+L                                                             
   18 B(K)=0.E0                                                         
   19 NEND=N-2                                                          
      IF(NEND.EQ.0) GO TO 21                                            
C CALCULATE VIA DIFFERENCE EQUATION AND STORE B(N),                     
C UNTIL N=2                                                             
      DO 20 L=1,NEND                                                    
      N=N-1                                                             
      B(N)=(FLOAT(2*N)*B(N+1))/X-SIGN*B(N+2)                            
      M=2-M                                                             
   20 SUM=SUM+FLOAT(M)*B(N)                                             
C CALCULATE B(1)                                                        
   21 B(1)=2.E0*B(2)/X-SIGN*B(3)                                        
   22 SUM=SUM+B(1)                                                      
C NORMALIZE--IF IZE=1, DIVIDE SUM BY COSH(X).  DIVIDE ALL               
C B(N) BY SUM.                                                          
   23 IF(IZE.EQ.0) GO TO 25                                             
      TEMPA=EXP(ABS(X))                                                 
      SUM=2.E0*SUM/(TEMPA+1.E0/TEMPA)                                   
   25 DO 26 N=1,NB                                                      
   26 B(N)=B(N)/SUM                                                     
      RETURN                                                            
C                                                                       
C TWO-TERM ASCENDING SERIES FOR SMALL X                                 
   30 TEMPA=1.E0                                                        
      TEMPB=-.25E0*X*X*SIGN                                             
      B(1)=1.E0+TEMPB                                                   
      IF(NB.EQ.1) GO TO 32                                              
      DO 31 N=2,NB                                                      
      TEMPA=TEMPA*X/FLOAT(2*N-2)                                        
   31 B(N)=TEMPA*(1.E0+TEMPB/FLOAT(N))                                  
   32 RETURN                                                            
      END                                                               
      SUBROUTINE B1SLC(X,Y,NB,IZE,BR,BI,NCALC)                          
C THIS ROUTINE CALCULATES BESSEL FUNCTIONS I AND J OF                   
C COMPLEX ARGUMENT AND INTEGER ORDER.                                   
C                                                                       
C                                                                       
C      EXPLANATION OF VARIABLES IN THE CALLING SEQUENCE                 
C                                                                       
C X     REAL PART OF THE COMPLEX ARGUMENT                               
C       FOR WHICH I*S OR J*S ARE TO BE CALCULATED.  IF I*S              
C       ARE TO BE CALCULATED, ABS(X) MUST NOT EXCEED EXPARG             
C       (WHICH SEE BELOW).                                              
C Y     IMAGINARY PART OF THE ARGUMENT.  IF J*S ARE TO BE               
C       CALCULATED, ABS(Y) MUST NOT EXCEED EXPARG.                      
C NB    INTEGER TYPE.  1 + HIGHEST ORDER TO BE CALCULATED.              
C       IT MUST BE POSITIVE.                                            
C IZE   INTEGER TYPE.  ZERO IF J*S ARE TO BE CALCULATED, 1              
C       IF I*S ARE TO BE CALCULATED.                                    
C BR    REAL VECTOR OF LENGTH NB, NEED NOT BE                           
C       INITIALIZED BY USER.  IF THE ROUTINE TERMINATES                 
C       NORMALLY, (NCALC=NB), IT RETURNS THE REAL PART OF               
C       J(OR I)-SUB-ZERO THROUGH J(OR I)-SUB-NB-MINUS-ONE               
C       OF Z IN THIS VECTOR.                                            
C BI    IMAGINARY ANALOG OF BR.                                         
C NCALC INTEGER TYPE, NEED NOT BE INITIALIZED BY USER.                  
C       BEFORE USING THE RESULTS, THE USER SHOULD CHECK THAT            
C       NCALC=NB, I.E. ALL ORDERS HAVE BEEN CALCULATED TO               
C       THE DESIRED ACCURACY.  SEE ERROR RETURNS BELOW.                 
C                                                                       
C                                                                       
C       EXPLANATION OF MACHINE-DEPENDENT CONSTANTS                      
C                                                                       
C NSIG  DECIMAL SIGNIFICANCE DESIRED.  SHOULD BE SET TO                 
C       IFIX(ALOG10(2)*NBIT+1), WHERE NBIT IS THE NUMBER OF             
C       BITS IN THE MANTISSA OF A SINGLE PRECISION VARIABLE.            
C       SETTING NSIG HIGHER WILL INCREASE CPU TIME WITHOUT              
C       INCREASING ACCURACY, WHILE SETTING NSIG LOWER WILL              
C       DECREASE ACCURACY.  IF ONLY SINGLE-PRECISION                    
C       ACCURACY IS DESIRED, REPLACE NBIT BY THE NUMBER OF              
C       BITS IN THE MANTISSA OF A SINGLE-PRECISION VARIABLE.            
C       THE RELATIVE TRUNCATION ERROR IS LIMITED TO T=.5*10             
C       **-NSIG FOR ORDER GREATER THAN ABS(Z), AND FOR ORDER            
C       LESS THAN ABS(Z) (GENERAL TEST), THE RELATIVE ERROR             
C       IS LIMITED TO T FOR FUNCTION VALUES OF MAGNITUDE AT             
C       LEAST 1, AND THE ABSOLUTE ERROR IS LIMITED TO T FOR             
C       SMALLER VALUES.                                                 
C NTEN  LARGEST INTEGER K SUCH THAT 10**K IS MACHINE-                   
C       REPRESENTABLE IN SINGLE PRECISION.                              
C LARGEZ UPPER LIMIT ON THE MAGNITUDE OF Z.  BEAR IN MIND               
C       THAT IF ABS(Z)=N, THEN AT LEAST N ITERATIONS OF THE             
C       BACKWARD RECURSION WILL BE EXECUTED.                            
C EXPARG LARGEST REAL ARGUMENT THAT THE LIBRARY                         
C       EXP ROUTINE CAN HANDLE.                                         
C                                                                       
C PORT NOTE, SEPTEMBER 16, 1976 -                                       
C THE LARGEX AND EXPARG TESTS ARE MADE IN THE OUTER ROUTINES -          
C DBESCJ AND DBESCI, WHICH CALL DB1SLR.                                 
C                                                                       
C                                                                       
C                                                                       
C                            ERROR RETURNS                              
C                                                                       
C PORT NOTE, SEPTEMBER 16, 1976 -                                       
C THE NOTES BELOW ARE KEPT IN FOR THE RECORD, BUT, AS ABOVE,            
C THE ACTUAL TESTS ARE NOW IN THE OUTER CALLING ROUTINES.               
C                                                                       
C       LET G DENOTE EITHER I OR J.                                     
C       IN CASE OF AN ERROR, NCALC.NE.NB, AND NOT ALL G*S               
C  ARE CALCULATED TO THE DESIRED ACCURACY.                              
C       IF NCALC.LT.0, AN ARGUMENT IS OUT OF RANGE.  NB.LE.0            
C  OR IZE IS NEITHER 0 NOR 1 OR IZE=0 AND ABS(Y).GT.EXPARG,             
C  OR IZE=1 AND ABS(X).GT.EXPARG.  IN THIS CASE, THE VECTORS            
C  BR AND BI ARE NOT CALCULATED, AND NCALC IS SET TO                    
C  MIN0(NB,0)-1 SO NCALC.NE.NB.                                         
C       NB.GT.NCALC.GT.0 WILL OCCUR IF NB.GT.MAGZ AND ABS(G-            
C  SUB-NB-OF-Z/G-SUB-MAGX+NP-OF-Z).LT.10.**(NTEN/2), I.E. NB            
C  IS MUCH GREATER THAN MAGZ.  IN THIS CASE, BR(N) AND BI(N)            
C  ARE CALCULATED TO THE DESIRED ACCURACY FOR N.LE.NCALC,               
C  BUT FOR NCALC.LT.N.LE.NB, PRECISION IS LOST.  IF N.GT.               
C  NCALC AND ABS(G(NCALC-1)/G(N-1)).EQ.10**-K, THEN THE LAST            
C  K SIGNIFICANT FIGURES OF G(N-1) (=BR(N)+I*BI(N)) ARE                 
C  ERRONEOUS.  IF THE USER WISHES TO CALCULATE G(N-1) TO                
C  HIGHER ACCURACY, HE SHOULD USE AN ASYMPTOTIC FORMULA FOR             
C  LARGE ORDER.                                                         
C                                                                       
      REAL                                                              
     1 X,Y,BR,BI,PR,PI,PLASTR,PLASTI,POLDR,POLDI,PSAVER,                
     2 PSAVEI,TEST,TOVER,TEMPAR,TEMPAI,TEMPBR,TEMPBI,                   
     3 TEMPCR,TEMPCI,SIGN,SUMR,SUMI,ZINVR,ZINVI,R1MACH                  
      DIMENSION BR(NB),BI(NB)                                           
      DATA NSIG/0/, NTEN/0/                                             
      IF(NSIG .NE. 0) GO TO 1                                           
      NSIG = IFIX(-ALOG10((R1MACH(3)))+1.)                              
      NTEN = ALOG10(R1MACH(2))                                          
    1 TEMPAR=SQRT(X*X+Y*Y)                                              
      MAGZ=IFIX(TEMPAR)                                                 
      SIGN=FLOAT(1-2*IZE)                                               
      NCALC=NB                                                          
C USE 2-TERM ASCENDING SERIES FOR SMALL Z                               
      IF(TEMPAR**4.LT..1E0**NSIG) GO TO 50                              
C INITIALIZE THE CALCULATION OF THE P*S                                 
      NBMZ=NB-MAGZ                                                      
      N=MAGZ+1                                                          
      IF(ABS(X).LT.ABS(Y)) GO TO 2                                      
      ZINVR=1.E0/(X+Y*Y/X)                                              
      ZINVI=-Y*ZINVR/X                                                  
      GO TO 3                                                           
    2 ZINVI=-1.E0/(Y+X*X/Y)                                             
      ZINVR=-X*ZINVI/Y                                                  
    3 PLASTR=1.E0                                                       
      PLASTI=0.E0                                                       
      PR=SIGN*FLOAT(2*N)*ZINVR                                          
      PI=SIGN*FLOAT(2*N)*ZINVI                                          
      TEST=2.E0*1.E1**NSIG                                              
      M=0                                                               
      IF(NBMZ.LT.3) GO TO 6                                             
C CALCULATE P*S UNTIL N=NB-1.  CHECK FOR POSSIBLE OVERFLOW.             
      TOVER=1.E1**(NTEN-NSIG)                                           
      NSTART=MAGZ+2                                                     
      NEND=NB-1                                                         
      DO 5 N=NSTART,NEND                                                
      POLDR=PLASTR                                                      
      POLDI=PLASTI                                                      
      PLASTR=PR                                                         
      PLASTI=PI                                                         
      PR=SIGN*(FLOAT(2*N)*(PLASTR*ZINVR-PLASTI*ZINVI)-POLDR)            
      PI=SIGN*(FLOAT(2*N)*(PLASTI*ZINVR+PLASTR*ZINVI)-POLDI)            
      IF((PR/TOVER)**2+(PI/TOVER)**2-1.E0) 5,5,7                        
    5 CONTINUE                                                          
      N=NEND                                                            
C CALCULATE SPECIAL SIGNIFICANCE TEST FOR NBMZ.GT.2.                    
      TEMPBI=AMAX1(ABS(PR),ABS(PI))                                     
      TEMPBI=TEMPBI*SQRT(2.E0*1.E1**NSIG*SQRT(((PR/TEMPBI)**2           
     1+(PI/TEMPBI)**2)*((PLASTR/TEMPBI)**2+(PLASTI/TEMPBI)**2)))        
      TEST=AMAX1(TEST,TEMPBI)                                           
C CALCULATE P*S UNTIL SIGNIFICANCE TEST IS PASSED.                      
    6 N=N+1                                                             
      POLDR=PLASTR                                                      
      POLDI=PLASTI                                                      
      PLASTR=PR                                                         
      PLASTI=PI                                                         
      PR=SIGN*(FLOAT(2*N)*(PLASTR*ZINVR-PLASTI*ZINVI)-POLDR)            
      PI=SIGN*(FLOAT(2*N)*(PLASTI*ZINVR+PLASTR*ZINVI)-POLDI)            
      IF((PR/TEST)**2+(PI/TEST)**2.LT.1.E0) GO TO 6                     
      IF(M.EQ.1) GO TO 12                                               
C CALCULATE STRICT VARIANT OF SIGNIFICANCE TEST, AND                    
C CALCULATE P*S UNTIL THIS TEST IS PASSED.                              
      M=1                                                               
      TEMPBI=AMAX1(ABS(PR),ABS(PI))                                     
      TEMPBR=SQRT(((PR/TEMPBI)**2+(PI/TEMPBI)**2)/                      
     1 ((PLASTR/TEMPBI)**2+(PLASTI/TEMPBI)**2))                         
      TEMPBI=FLOAT(N+1)/TEMPAR                                          
      IF(TEMPBR+1.E0/TEMPBR.GT.2.E0*TEMPBI) TEMPBR=TEMPBI               
     1 +SQRT(TEMPBI**2-1.E0)                                            
      TEST=TEST/SQRT(TEMPBR-1.E0/TEMPBR)                                
      IF((PR/TEST)**2+(PI/TEST)**2-1.E0) 6,12,12                        
    7 NSTART=N+1                                                        
C TO AVOID OVERFLOW, NORMALIZE P*S BY DIVIDING BY TOVER.                
C CALCULATE P*S UNTIL UNNORMALIZED P WOULD OVERFLOW.                    
      PR=PR/TOVER                                                       
      PI=PI/TOVER                                                       
      PLASTR=PLASTR/TOVER                                               
      PLASTI=PLASTI/TOVER                                               
      PSAVER=PR                                                         
      PSAVEI=PI                                                         
      TEMPCR=PLASTR                                                     
      TEMPCI=PLASTI                                                     
      TEST=1.E1**(2*NSIG)                                               
    8 N=N+1                                                             
      POLDR=PLASTR                                                      
      POLDI=PLASTI                                                      
      PLASTR=PR                                                         
      PLASTI=PI                                                         
      PR=SIGN*(FLOAT(2*N)*(PLASTR*ZINVR-PLASTI*ZINVI)-POLDR)            
      PI=SIGN*(FLOAT(2*N)*(PLASTI*ZINVR+PLASTR*ZINVI)-POLDI)            
      IF(PR**2+PI**2.LE.TEST) GO TO 8                                   
C CALCULATE BACKWARD TEST, AND FIND NCALC, THE HIGHEST N                
C SUCH THAT THE TEST IS PASSED.                                         
      TEMPBR=SQRT((PLASTR**2+PLASTI**2)/(POLDR**2+POLDI**2))            
      TEMPBI=FLOAT(N)/TEMPAR                                            
      IF(TEMPBR+1.E0/TEMPBR.GT.2.E0*TEMPBI) TEMPBR=TEMPBI+              
     1 SQRT(TEMPBI**2-1.E0)                                             
      TEST=.5E0*(1.E0-1.E0/TEMPBR**2)/1.E1**NSIG                        
      TEST=((PLASTR**2+PLASTI**2)*TEST)*((POLDR**2+POLDI**2)*TEST)      
      PR=PLASTR*TOVER                                                   
      PI=PLASTI*TOVER                                                   
      N=N-1                                                             
      NEND=MIN0(NB,N)                                                   
      DO 9 NCALC=NSTART,NEND                                            
      POLDR=TEMPCR                                                      
      POLDI=TEMPCI                                                      
      TEMPCR=PSAVER                                                     
      TEMPCI=PSAVEI                                                     
      PSAVER=SIGN*(FLOAT(2*N)*(TEMPCR*ZINVR-TEMPCI*ZINVI)-POLDR)        
      PSAVEI=SIGN*(FLOAT(2*N)*(TEMPCI*ZINVR+TEMPCR*ZINVI)-POLDI)        
      IF((PSAVER**2+PSAVEI**2)*(TEMPCR**2+TEMPCI**2)-TEST) 9,9,10       
    9 CONTINUE                                                          
      NCALC=NEND+1                                                      
   10 NCALC=NCALC-1                                                     
C THE COEFFICIENT OF B(N) IN THE NORMALIZATION SUM IS                   
C M*SQRT(-1)**IMAG, WHERE M=-2,0, OR 2, AND IMAG IS 0 OR 1.             
C CALCULATE RECURSION RULES FOR M AND IMAG, AND INITIALIZE              
C THEM.                                                                 
   12 N=N+1                                                             
      TEMPBR=FLOAT(IZE)*X+FLOAT(1-IZE)*Y                                
      IPOS=0                                                            
      IF(TEMPBR) 13,14,13                                               
   13 IPOS=IFIX((1.1E0*TEMPBR/ABS(TEMPBR)))                             
   14 MRECUR=4*((2+IZE+IPOS)/2)-3-2*(IZE+IPOS)                          
      K=2+IPOS+2*IZE*IPOS**2-IZE                                        
      L=N-4*(N/4)                                                       
      MLAST=2+8*((K*L)/4)-4*((K*L)/2)                                   
      IF(IPOS.EQ.0.AND.(L.EQ.1.OR.L.EQ.3)) MLAST=0                      
      L=L+3-4*((L+3)/4)                                                 
      M    =2+8*((K*L)/4)-4*((K*L)/2)                                   
      IF(IPOS.EQ.0.AND.(L.EQ.1.OR.L.EQ.3)) M=0                          
      IMRECR=(1-IZE)*IPOS**2                                            
      IMAG=IMRECR*(L-2*(L/2))                                           
C INITIALIZE THE BACKWARD RECURSION AND THE NORMALIZATION               
C SUM.                                                                  
      TEMPBR=0.E0                                                       
      TEMPBI=0.E0                                                       
      IF(ABS(PI).GT.ABS(PR)) GO TO 15                                   
      TEMPAR=1.E0/(PR+PI*(PI/PR))                                       
      TEMPAI=-(PI*TEMPAR)/PR                                            
      GO TO 16                                                          
   15 TEMPAI=-1.E0/(PI+PR*(PR/PI))                                      
      TEMPAR=-(PR*TEMPAI)/PI                                            
   16 IF(IMAG.NE.0) GO TO 17                                            
      SUMR=FLOAT(M)*TEMPAR                                              
      SUMI=FLOAT(M)*TEMPAI                                              
      GO TO 18                                                          
   17 SUMR=-FLOAT(M)*TEMPAI                                             
      SUMI=FLOAT(M)*TEMPAR                                              
   18 NEND=N-NB                                                         
      IF(NEND) 26,22,19                                                 
C RECUR BACKWARD VIA DIFFERENCE EQUATION CALCULATING (BUT               
C NOT STORING) BR(N) AND BI(N) UNTIL N=NB.                              
   19 DO 21 L=1,NEND                                                    
      N=N-1                                                             
      TEMPCR=TEMPBR                                                     
      TEMPCI=TEMPBI                                                     
      TEMPBR=TEMPAR                                                     
      TEMPBI=TEMPAI                                                     
      PR=FLOAT(2*N)*ZINVR                                               
      PI=FLOAT(2*N)*ZINVI                                               
      TEMPAR=PR*TEMPBR-PI*TEMPBI-SIGN*TEMPCR                            
      TEMPAI=PR*TEMPBI+PI*TEMPBR-SIGN*TEMPCI                            
      IMAG=(1-IMAG)*IMRECR                                              
      K=MLAST                                                           
      MLAST=M                                                           
      M=K*MRECUR                                                        
      IF(IMAG.NE.0) GO TO 20                                            
      SUMR=SUMR+FLOAT(M)*TEMPAR                                         
      SUMI=SUMI+FLOAT(M)*TEMPAI                                         
      GO TO 21                                                          
   20 SUMR=SUMR-FLOAT(M)*TEMPAI                                         
      SUMI=SUMI+FLOAT(M)*TEMPAR                                         
   21 CONTINUE                                                          
C STORE BR(NB), BI(NB)                                                  
   22 BR(N)=TEMPAR                                                      
      BI(N)=TEMPAI                                                      
      IF(N.GT.1) GO TO 23                                               
C NB=1.  SINCE 2*TEMPAR AND 2*TEMPAI WERE ADDED TO SUMR AND             
C SUMI RESPECTIVELY, WE MUST SUBTRACT TEMPAR AND TEMPAI                 
      SUMR=SUMR-TEMPAR                                                  
      SUMI=SUMI-TEMPAI                                                  
      GO TO 35                                                          
C CALCULATE AND STORE BR(NB-1),BI(NB-1)                                 
   23 N=N-1                                                             
      PR=FLOAT(2*N)*ZINVR                                               
      PI=FLOAT(2*N)*ZINVI                                               
      BR(N)=PR*TEMPAR-PI*TEMPAI-SIGN*TEMPBR                             
      BI(N)=PR*TEMPAI+PI*TEMPAR-SIGN*TEMPBI                             
      IF(N.EQ.1) GO TO 34                                               
      IMAG=(1-IMAG)*IMRECR                                              
      K=MLAST                                                           
      MLAST=M                                                           
      M=K*MRECUR                                                        
      IF(IMAG.NE.0) GO TO 24                                            
      SUMR=SUMR+FLOAT(M)*BR(N)                                          
      SUMI=SUMI+FLOAT(M)*BI(N)                                          
      GO TO 30                                                          
   24 SUMR=SUMR-FLOAT(M)*BI(N)                                          
      SUMI=SUMI+FLOAT(M)*BR(N)                                          
      GO TO 30                                                          
C N.LT.NB, SO STORE BR(N), BI(N), AND SET HIGHER ORDERS ZERO            
   26 BR(N)=TEMPAR                                                      
      BI(N)=TEMPAI                                                      
      NEND=-NEND                                                        
      DO 27 L=1,NEND                                                    
      NPL=N+L                                                           
      BR(NPL)=0.E0                                                      
   27 BI(NPL)=0.E0                                                      
   30 NEND=N-2                                                          
      IF(NEND.EQ.0) GO TO 33                                            
C CALCULATE VIA DIFFERENCE EQUATION AND STORE BR(N),BI(N),              
C UNTIL N=2                                                             
      DO 32 L=1,NEND                                                    
      N=N-1                                                             
      PR=FLOAT(2*N)*ZINVR                                               
      PI=FLOAT(2*N)*ZINVI                                               
      BR(N)=PR*BR(N+1)-PI*BI(N+1)-SIGN*BR(N+2)                          
      BI(N)=PR*BI(N+1)+PI*BR(N+1)-SIGN*BI(N+2)                          
      IMAG=(1-IMAG)*IMRECR                                              
      K=MLAST                                                           
      MLAST=M                                                           
      M=K*MRECUR                                                        
      IF(IMAG.NE.0) GO TO 31                                            
      SUMR=SUMR+FLOAT(M)*BR(N)                                          
      SUMI=SUMI+FLOAT(M)*BI(N)                                          
      GO TO 32                                                          
   31 SUMR=SUMR-FLOAT(M)*BI(N)                                          
      SUMI=SUMI+FLOAT(M)*BR(N)                                          
   32 CONTINUE                                                          
C CALCULATE AND STORE BR(1), BI(1)                                      
   33 BR(1)=2.E0*(BR(2)*ZINVR-BI(2)*ZINVI)-SIGN*BR(3)                   
      BI(1)=2.E0*(BR(2)*ZINVI+BI(2)*ZINVR)-SIGN*BI(3)                   
   34 SUMR=SUMR+BR(1)                                                   
      SUMI=SUMI+BI(1)                                                   
C CALCULATE NORMALIZATION FACTOR, TEMPAR +I*TEMPAI                      
   35 IF(IZE.EQ.1) GO TO 36                                             
      TEMPCR=FLOAT(IPOS)*Y                                              
      TEMPCI=FLOAT(-IPOS)*X                                             
      GO TO 37                                                          
   36 TEMPCR=FLOAT(IPOS)*X                                              
      TEMPCI=FLOAT(IPOS)*Y                                              
   37 TEMPCR=EXP(TEMPCR)                                                
      TEMPBR=COS(TEMPCI)                                                
      TEMPBI=SIN(TEMPCI)                                                
      IF(ABS(SUMR).LT.ABS(SUMI)) GO TO 38                               
      TEMPCI=SUMI/SUMR                                                  
      TEMPCR=(TEMPCR/SUMR)/(1.E0+TEMPCI*TEMPCI)                         
      TEMPAR=TEMPCR*(TEMPBR+TEMPBI*TEMPCI)                              
      TEMPAI=TEMPCR*(TEMPBI-TEMPBR*TEMPCI)                              
      GO TO 39                                                          
   38 TEMPCI=SUMR/SUMI                                                  
      TEMPCR=(TEMPCR/SUMI)/(1.E0+TEMPCI*TEMPCI)                         
      TEMPAR=TEMPCR*(TEMPBR*TEMPCI+TEMPBI)                              
      TEMPAI=TEMPCR*(TEMPBI*TEMPCI-TEMPBR)                              
C NORMALIZE                                                             
   39 DO 40 N=1,NB                                                      
      TEMPBR=BR(N)*TEMPAR-BI(N)*TEMPAI                                  
      BI(N)=BR(N)*TEMPAI+BI(N)*TEMPAR                                   
   40 BR(N)=TEMPBR                                                      
      RETURN                                                            
C TWO-TERM ASCENDING SERIES FOR SMALL Z                                 
   50 TEMPAR=1.E0                                                       
      TEMPAI=0.E0                                                       
      TEMPCR=.25E0*(X*X-Y*Y)                                            
      TEMPCI=.5E0*X*Y                                                   
      BR(1)=1.E0-SIGN*TEMPCR                                            
      BI(1)=-SIGN*TEMPCI                                                
      IF(NB.EQ.1) GO TO 52                                              
      DO 51 N=2,NB                                                      
      TEMPBR=(TEMPAR*X-TEMPAI*Y)/FLOAT(2*N-2)                           
      TEMPAI=(TEMPAR*Y+TEMPAI*X)/FLOAT(2*N-2)                           
      TEMPAR=TEMPBR                                                     
      TEMPBR=FLOAT(N)                                                   
      BR(N)=TEMPAR*(1.E0-SIGN*TEMPCR/TEMPBR)+TEMPAI*TEMPCI/TEMPBR       
   51 BI(N)=TEMPAI*(1.E0-SIGN*TEMPCR/TEMPBR)-TEMPAR*TEMPCI/TEMPBR       
   52 RETURN                                                            
      END                                                               
      SUBROUTINE DBESRJ(X, NB, B)                                       
C                                                                       
C THIS ROUTINE CALCULATES BESSEL FUNCTIONS J OF REAL                    
C ARGUMENT AND INTEGER ORDER.                                           
C                                                                       
C      EXPLANATION OF VARIABLES IN THE CALLING SEQUENCE -               
C                                                                       
C X  -  DOUBLE PRECISION REAL ARGUMENT FOR WHICH THE J*S                
C       ARE TO BE CALCULATED.                                           
C                                                                       
C NB -  INTEGER TYPE.  1 + HIGHEST ORDER TO BE CALCULATED.              
C       IT MUST BE POSITIVE.                                            
C                                                                       
C B  -  DOUBLE PRECISION VECTOR OF LENGTH NB, NEED NOT BE               
C       INITIALIZED BY USER.  IF THE ROUTINE TERMINATES                 
C       NORMALLY IT RETURNS J-SUB-ZERO                                  
C       THROUGH J-SUB-NB-MINUS-ONE OF X IN THIS                         
C       VECTOR.                                                         
C                                                                       
C                  ACCURACY OF THE COMPUTED VALUES -                    
C                                                                       
C       IN CASE OF AN ERROR, NOT ALL J*S                                
C       ARE CALCULATED TO THE DESIRED ACCURACY.                         
C                                                                       
C       THE SUBPROGRAM CALLED BY DBESRJ, DB1SLR,                        
C       RETURNS IN THE VARIABLE, NCALC, THE NUMBER CALCULATED CORRECTLY.
C                                                                       
C       LET NTEN BE THE LARGEST INTEGER K SUCH THAT 10**K IS MACHINE-   
C       REPRESENTABLE IN DOUBLE PRECISION.                              
C       THEN NB.GT.NCALC.GT.0 WILL OCCUR IF NB.GT.MAGX AND ABS(J-       
C       SUB-NB-OF-X/J-SUB-MAGX+NP-OF-X).LT.10.**(NTEN/2), I.E. NB       
C       IS MUCH GREATER THAN MAGX.  IN THIS CASE, B(N) IS CALCU-        
C       LATED TO THE DESIRED ACCURACY FOR N.LE.NCALC, BUT FOR           
C       NCALC.LT.N.LE.NB, PRECISION IS LOST.  IF N.GT.NCALC AND         
C       ABS(B(NCALC)/B(N)).EQ.10**-K, THEN THE LAST K SIGNIFICANT       
C       FIGURES OF B(N) ARE ERRONEOUS.  IF THE USER WISHES TO           
C       CALCULATE B(N) TO HIGHER ACCURACY, HE SHOULD USE AN             
C       ASYMPTOTIC FORMULA FOR LARGE ORDER.                             
C                                                                       
      DOUBLE PRECISION X,B(1)                                           
C                                                                       
C CHECK INPUT VALUES                                                    
C                                                                       
C       AN UPPER LIMIT OF 10000 IS SET ON THE MAGNITUDE OF X.           
C       BEAR IN MIND THAT IF ABS(X)=N, THEN AT LEAST N ITERATIONS       
C       OF THE BACKWARD RECURSION WILL BE EXECUTED.                     
C                                                                       
      MAGX = DABS(X)                                                    
C                                                                       
C/6S                                                                    
C     IF  (MAGX .GT. 10000) CALL SETERR(                                
C    1    33HDBESRJ - X IS TOO BIG (MAGNITUDE),33,1,2)                  
C/7S                                                                    
      IF  (MAGX .GT. 10000) CALL SETERR(                                
     1    'DBESRJ - X IS TOO BIG (MAGNITUDE)',33,1,2)                   
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF  (NB .LT. 1) CALL SETERR(                                      
C    1    28HDBESRJ - NB SHOULD = ORDER+1,28,2,2)                       
C/7S                                                                    
      IF  (NB .LT. 1) CALL SETERR(                                      
     1    'DBESRJ - NB SHOULD = ORDER+1',28,2,2)                        
C/                                                                      
C                                                                       
C DBESRJ CALLS ON THE SUBPROGRAM,DB1SLR,                                
C WHICH IS SOOKNES  ORIGINAL BESLRI.                                    
C                                                                       
C THE ADDITIONAL INPUT ARGUMENTS REQUIRED FOR IT ARE -                  
C                                                                       
C IZE   INTEGER TYPE.  ZERO IF J*S ARE TO BE CALCULATED, 1              
C       IF I*S ARE TO BE CALCULATED.(THIRD ARGUMENT BELOW)              
C                                                                       
C NCALC INTEGER TYPE, NEED NOT BE INITIALIZED BY USER.                  
C       BEFORE USING THE RESULTS, IT SHOULD BE CHECKED THAT             
C       NCALC=NB, I.E. ALL ORDERS HAVE BEEN CALCULATED TO               
C       THE DESIRED ACCURACY.                                           
C                                                                       
      CALL DB1SLR (X, NB, 0, B, NCALC)                                  
C                                                                       
C TEST IF ALL GOT COMPUTED OK                                           
C (SINCE SOME VALUES MAY BE OK, THIS IS A RECOVERABLE ERROR.)           
C                                                                       
      IF (NB .EQ. NCALC) RETURN                                         
C                                                                       
      NCALC = NCALC+10                                                  
C/6S                                                                    
C     CALL SETERR(                                                      
C    1    38HDBESRJ - ONLY THIS MANY ANSWERS ARE OK,38,NCALC,1)         
C/7S                                                                    
      CALL SETERR(                                                      
     1    'DBESRJ - ONLY THIS MANY ANSWERS ARE OK',38,NCALC,1)          
C/                                                                      
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DBESRI(X, NB, B)                                       
C                                                                       
C THIS ROUTINE CALCULATES MODIFIED BESSEL FUNCTIONS I OF REAL           
C ARGUMENT AND INTEGER ORDER.                                           
C                                                                       
C      EXPLANATION OF VARIABLES IN THE CALLING SEQUENCE -               
C                                                                       
C X  -  DOUBLE PRECISION REAL ARGUMENT FOR WHICH THE I*S                
C       ARE TO BE CALCULATED.                                           
C                                                                       
C NB -  INTEGER TYPE.  1 + HIGHEST ORDER TO BE CALCULATED.              
C       IT MUST BE POSITIVE.                                            
C                                                                       
C B  -  DOUBLE PRECISION VECTOR OF LENGTH NB, NEED NOT BE               
C       INITIALIZED BY USER.  IF THE ROUTINE TERMINATES                 
C       NORMALLY IT RETURNS I-SUB-ZERO                                  
C       THROUGH I-SUB-NB-MINUS-ONE OF X IN THIS                         
C       VECTOR.                                                         
C                                                                       
C                  ACCURACY OF THE COMPUTED VALUES -                    
C                                                                       
C       IN CASE OF AN ERROR, NOT ALL I*S                                
C       ARE CALCULATED TO THE DESIRED ACCURACY.                         
C                                                                       
C       THE SUBPROGRAM CALLED BY DBESRI, DB1SLR,                        
C       RETURNS IN THE VARIABLE, NCALC, THE NUMBER CALCULATED CORRECTLY.
C                                                                       
C       LET NTEN BE THE LARGEST INTEGER K SUCH THAT 10**K IS MACHINE-   
C       REPRESENTABLE IN DOUBLE PRECISION.                              
C       THEN NB.GT.NCALC.GT.0 WILL OCCUR IF NB.GT.MAGX AND ABS(I-       
C       SUB-NB-OF-X/I-SUB-MAGX+NP-OF-X).LT.10.**(NTEN/2), I.E. NB       
C       IS MUCH GREATER THAN MAGX.  IN THIS CASE, B(N) IS CALCU-        
C       LATED TO THE DESIRED ACCURACY FOR N.LE.NCALC, BUT FOR           
C       NCALC.LT.N.LE.NB, PRECISION IS LOST.  IF N.GT.NCALC AND         
C       ABS(B(NCALC)/B(N)).EQ.10**-K, THEN THE LAST K SIGNIFICANT       
C       FIGURES OF B(N) ARE ERRONEOUS.  IF THE USER WISHES TO           
C       CALCULATE B(N) TO HIGHER ACCURACY, HE SHOULD USE AN             
C       ASYMPTOTIC FORMULA FOR LARGE ORDER.                             
C                                                                       
      DOUBLE PRECISION X,B(1),D1MACH, DLOG                              
C                                                                       
C CHECK INPUT VALUES                                                    
C                                                                       
C       AN UPPER LIMIT OF 10000 IS SET ON THE MAGNITUDE OF X.           
C       BEAR IN MIND THAT IF ABS(X)=N, THEN AT LEAST N ITERATIONS       
C       OF THE BACKWARD RECURSION WILL BE EXECUTED.                     
C                                                                       
      MAGX = DABS(X)                                                    
      MEXP = DLOG(D1MACH(2))                                            
C                                                                       
C/6S                                                                    
C     IF  (MAGX .GT. 10000 .OR. MAGX .GT. MEXP) CALL SETERR(            
C    1    33HDBESRI - X IS TOO BIG (MAGNITUDE),33,1,2)                  
C/7S                                                                    
      IF  (MAGX .GT. 10000 .OR. MAGX .GT. MEXP) CALL SETERR(            
     1    'DBESRI - X IS TOO BIG (MAGNITUDE)',33,1,2)                   
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF  (NB .LT. 1) CALL SETERR(                                      
C    1    28HDBESRI - NB SHOULD = ORDER+1,28,2,2)                       
C/7S                                                                    
      IF  (NB .LT. 1) CALL SETERR(                                      
     1    'DBESRI - NB SHOULD = ORDER+1',28,2,2)                        
C/                                                                      
C                                                                       
C DBESRJ CALLS ON THE SUBPROGRAM,DB1SLR,                                
C WHICH IS SOOKNES  ORIGINAL BESLRI.                                    
C                                                                       
C THE ADDITIONAL INPUT ARGUMENTS REQUIRED FOR IT ARE -                  
C                                                                       
C IZE   INTEGER TYPE.  ZERO IF J*S ARE TO BE CALCULATED, 1              
C       IF I*S ARE TO BE CALCULATED.(THIRD ARGUMENT BELOW)              
C                                                                       
C NCALC INTEGER TYPE, NEED NOT BE INITIALIZED BY USER.                  
C       BEFORE USING THE RESULTS, IT SHOULD BE CHECKED THAT             
C       NCALC=NB, I.E. ALL ORDERS HAVE BEEN CALCULATED TO               
C       THE DESIRED ACCURACY.                                           
C                                                                       
      CALL DB1SLR (X, NB, 1, B, NCALC)                                  
C                                                                       
C TEST IF ALL GOT COMPUTED OK                                           
C (SINCE SOME VALUES MAY BE OK, THIS IS A RECOVERABLE ERROR.)           
C                                                                       
      IF (NB .EQ. NCALC) RETURN                                         
C                                                                       
      NCALC = NCALC+10                                                  
C/6S                                                                    
C     CALL SETERR(                                                      
C    1    38HDBESRI - ONLY THIS MANY ANSWERS ARE OK,38,NCALC,1)         
C/7S                                                                    
      CALL SETERR(                                                      
     1    'DBESRI - ONLY THIS MANY ANSWERS ARE OK',38,NCALC,1)          
C/                                                                      
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DBESCJ(XR, XI, NB, BR, BI)                             
C                                                                       
C THIS ROUTINE CALCULATES BESSEL FUNCTIONS J OF COMPLEX                 
C ARGUMENT AND INTEGER ORDER.                                           
C                                                                       
C      EXPLANATION OF VARIABLES IN THE CALLING SEQUENCE -               
C                                                                       
C XR -  DOUBLE PRECISION REAL PART OF THE COMPLEX ARGUMENT              
C       FOR WHICH THE J*S ARE TO BE CALCULATED.                         
C                                                                       
C XI -  DOUBLE PRECISION IMAGINARY PART OF THE COMPLEX ARGUMENT         
C       FOR WHICH THE J*S ARE TO BE CALCULATED.                         
C                                                                       
C NB -  INTEGER TYPE.  1 + HIGHEST ORDER TO BE CALCULATED.              
C       IT MUST BE POSITIVE.                                            
C                                                                       
C BR -  DOUBLE PRECISION VECTOR OF LENGTH NB, NEED NOT BE               
C       INITIALIZED BY USER.  IF THE ROUTINE TERMINATES                 
C       NORMALLY IT RETURNS THE REAL PART OF J-SUB-ZERO                 
C       THROUGH J-SUB-NB-MINUS-ONE OF Z IN THIS                         
C       VECTOR.                                                         
C                                                                       
C BI -  IMAGINARY ANALOG OF BR.                                         
C                                                                       
C                  ACCURACY OF THE COMPUTED VALUES -                    
C                                                                       
C       IN CASE OF AN ERROR, NOT ALL J*S                                
C       ARE CALCULATED TO THE DESIRED ACCURACY.                         
C                                                                       
C       THE SUBPROGRAM CALLED BY DBESCJ, DB1SLC,                        
C       RETURNS IN THE VARIABLE, NCALC, THE NUMBER CALCULATED CORRECTLY.
C                                                                       
C       LET NTEN BE THE LARGEST INTEGER K SUCH THAT 10**K IS MACHINE-   
C       REPRESENTABLE IN DOUBLE PRECISION.                              
C       THEN NB.GT.NCALC.GT.0 WILL OCCUR IF NB.GT.MAGZ AND ABS(J-       
C       SUB-NB-OF-Z/J-SUB-MAGZ+NP-OF-Z).LT.10.**(NTEN/2), I.E. NB       
C       IS MUCH GREATER THAN MAGZ.  IN THIS CASE, BR(N) AND BI(N)       
C       ARE CALCULATED TO THE DESIRED ACCURACY FOR N.LE.NCALC, BUT FOR  
C       NCALC.LT.N.LE.NB, PRECISION IS LOST.  IF N.GT.NCALC AND         
C       ABS(B(NCALC-1)/B(N-1)).EQ.10**-K, THEN THE LAST K SIGNIFICANT   
C       FIGURES OF B(N-1) (HERE B(N)=BR(N)+BI(N)) ARE ERRONEOUS.        
C       IF THE USER WISHES TO CALCULATE B(N) TO HIGHER ACCURACY,        
C       HE SHOULD USE AN ASYMPTOTIC FORMULA FOR LARGE ORDER.            
C                                                                       
      DOUBLE PRECISION XR,XI,D1MACH,BR(1),BI(1), DSQRT, DLOG            
C                                                                       
C CHECK INPUT VALUES                                                    
C                                                                       
C       AN UPPER LIMIT OF 10000 IS SET ON THE MAGNITUDE OF Z.           
C       BEAR IN MIND THAT IF ABS(Z)=N, THEN AT LEAST N ITERATIONS       
C       OF THE BACKWARD RECURSION WILL BE EXECUTED.                     
C                                                                       
      MAGZ = DSQRT(XR*XR+XI*XI)                                         
      MEXP = DLOG(D1MACH(2))                                            
C                                                                       
C/6S                                                                    
C     IF  (MAGZ .GT. 10000 .OR. IFIX(SNGL(DABS(XI))) .GT. MEXP)         
C    1    CALL SETERR(33HDBESCJ - Z IS TOO BIG (MAGNITUDE),33,1,2)      
C/7S                                                                    
      IF  (MAGZ .GT. 10000 .OR. IFIX(SNGL(DABS(XI))) .GT. MEXP)         
     1    CALL SETERR('DBESCJ - Z IS TOO BIG (MAGNITUDE)',33,1,2)       
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF  (NB .LT. 1) CALL SETERR(                                      
C    1    28HDBESCJ - NB SHOULD = ORDER+1,28,2,2)                       
C/7S                                                                    
      IF  (NB .LT. 1) CALL SETERR(                                      
     1    'DBESCJ - NB SHOULD = ORDER+1',28,2,2)                        
C/                                                                      
C                                                                       
C DBESCJ CALLS ON THE SUBPROGRAM,DB1SLC,                                
C WHICH IS SOOKNES  ORIGINAL BESLCI.                                    
C                                                                       
C THE ADDITIONAL INPUT ARGUMENTS REQUIRED FOR IT ARE -                  
C                                                                       
C IZE   INTEGER TYPE.  ZERO IF J*S ARE TO BE CALCULATED, 1              
C       IF I*S ARE TO BE CALCULATED.(FOURTH ARGUMENT BELOW)             
C                                                                       
C NCALC INTEGER TYPE, NEED NOT BE INITIALIZED BY USER.                  
C       BEFORE USING THE RESULTS, IT SHOULD BE CHECKED THAT             
C       NCALC=NB, I.E. ALL ORDERS HAVE BEEN CALCULATED TO               
C       THE DESIRED ACCURACY.                                           
C                                                                       
      CALL DB1SLC (XR, XI, NB, 0, BR, BI, NCALC)                        
C                                                                       
C TEST IF ALL GOT COMPUTED OK                                           
C (SINCE SOME VALUES MAY BE OK, THIS IS A RECOVERABLE ERROR.)           
C                                                                       
      IF (NB .EQ. NCALC) RETURN                                         
C                                                                       
      NCALC = NCALC+10                                                  
C/6S                                                                    
C     CALL SETERR(                                                      
C    1    38HDBESCJ - ONLY THIS MANY ANSWERS ARE OK,38,NCALC,1)         
C/7S                                                                    
      CALL SETERR(                                                      
     1    'DBESCJ - ONLY THIS MANY ANSWERS ARE OK',38,NCALC,1)          
C/                                                                      
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DBESCI(XR, XI, NB, BR, BI)                             
C                                                                       
C THIS ROUTINE CALCULATES BESSEL FUNCTIONS I OF COMPLEX                 
C ARGUMENT AND INTEGER ORDER.                                           
C                                                                       
C      EXPLANATION OF VARIABLES IN THE CALLING SEQUENCE -               
C                                                                       
C XR -  DOUBLE PRECISION REAL PART OF THE COMPLEX ARGUMENT              
C       FOR WHICH THE I*S ARE TO BE CALCULATED.                         
C                                                                       
C XI -  DOUBLE PRECISION IMAGINARY PART OF THE COMPLEX ARGUMENT         
C       FOR WHICH THE I*S ARE TO BE CALCULATED.                         
C                                                                       
C NB -  INTEGER TYPE.  1 + HIGHEST ORDER TO BE CALCULATED.              
C       IT MUST BE POSITIVE.                                            
C                                                                       
C BR -  DOUBLE PRECISION VECTOR OF LENGTH NB, NEED NOT BE               
C       INITIALIZED BY USER.  IF THE ROUTINE TERMINATES                 
C       NORMALLY IT RETURNS THE REAL PART OF I-SUB-ZERO                 
C       THROUGH I-SUB-NB-MINUS-ONE OF Z IN THIS                         
C       VECTOR.                                                         
C                                                                       
C BI -  IMAGINARY ANALOG OF BR.                                         
C                                                                       
C                  ACCURACY OF THE COMPUTED VALUES -                    
C                                                                       
C       IN CASE OF AN ERROR, NOT ALL I*S                                
C       ARE CALCULATED TO THE DESIRED ACCURACY.                         
C                                                                       
C       THE SUBPROGRAM CALLED BY DBESCI, DB1SLC,                        
C       RETURNS IN THE VARIABLE, NCALC, THE NUMBER CALCULATED CORRECTLY.
C                                                                       
C       LET NTEN BE THE LARGEST INTEGER K SUCH THAT 10**K IS MACHINE-   
C       REPRESENTABLE IN DOUBLE PRECISION.                              
C       THEN NB.GT.NCALC.GT.0 WILL OCCUR IF NB.GT.MAGZ AND ABS(I-       
C       SUB-NB-OF-Z/I-SUB-MAGZ+NP-OF-Z).LT.10.**(NTEN/2), I.E. NB       
C       IS MUCH GREATER THAN MAGZ.  IN THIS CASE, BR(N) AND BI(N)       
C       ARE CALCULATED TO THE DESIRED ACCURACY FOR N.LE.NCALC, BUT FOR  
C       NCALC.LT.N.LE.NB, PRECISION IS LOST.  IF N.GT.NCALC AND         
C       ABS(B(NCALC-1)/B(N-1)).EQ.10**-K, THEN THE LAST K SIGNIFICANT   
C       FIGURES OF B(N-1) (HERE B(N)=BR(N)+BI(N)) ARE ERRONEOUS.        
C       IF THE USER WISHES TO CALCULATE B(N) TO HIGHER ACCURACY,        
C       HE SHOULD USE AN ASYMPTOTIC FORMULA FOR LARGE ORDER.            
C                                                                       
      DOUBLE PRECISION XR,XI,D1MACH,BR(1),BI(1), DSQRT, DLOG            
C                                                                       
C CHECK INPUT VALUES                                                    
C                                                                       
C       AN UPPER LIMIT OF 10000 IS SET ON THE MAGNITUDE OF Z.           
C       BEAR IN MIND THAT IF ABS(Z)=N, THEN AT LEAST N ITERATIONS       
C       OF THE BACKWARD RECURSION WILL BE EXECUTED.                     
C                                                                       
      MAGZ = DSQRT(XR*XR+XI*XI)                                         
      MEXP = DLOG(D1MACH(2))                                            
C                                                                       
C/6S                                                                    
C     IF  (MAGZ .GT. 10000 .OR. IFIX(SNGL(DABS(XR))) .GT. MEXP)         
C    1    CALL SETERR(33HDBESCI - Z IS TOO BIG (MAGNITUDE),33,1,2)      
C/7S                                                                    
      IF  (MAGZ .GT. 10000 .OR. IFIX(SNGL(DABS(XR))) .GT. MEXP)         
     1    CALL SETERR('DBESCI - Z IS TOO BIG (MAGNITUDE)',33,1,2)       
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF  (NB .LT. 1) CALL SETERR(                                      
C    1    28HDBESCI - NB SHOULD = ORDER+1,28,2,2)                       
C/7S                                                                    
      IF  (NB .LT. 1) CALL SETERR(                                      
     1    'DBESCI - NB SHOULD = ORDER+1',28,2,2)                        
C/                                                                      
C                                                                       
C DBESCI CALLS ON THE SUBPROGRAM,DB1SLC,                                
C WHICH IS SOOKNES  ORIGINAL BESLCI.                                    
C                                                                       
C THE ADDITIONAL INPUT ARGUMENTS REQUIRED FOR IT ARE -                  
C                                                                       
C IZE   INTEGER TYPE.  ZERO IF I*S ARE TO BE CALCULATED, 1              
C       IF I*S ARE TO BE CALCULATED.(FOURTH ARGUMENT BELOW)             
C                                                                       
C NCALC INTEGER TYPE, NEED NOT BE INITIALIZED BY USER.                  
C       BEFORE USING THE RESULTS, IT SHOULD BE CHECKED THAT             
C       NCALC=NB, I.E. ALL ORDERS HAVE BEEN CALCULATED TO               
C       THE DESIRED ACCURACY.                                           
C                                                                       
      CALL DB1SLC (XR, XI, NB, 1, BR, BI, NCALC)                        
C                                                                       
C TEST IF ALL GOT COMPUTED OK                                           
C (SINCE SOME VALUES MAY BE OK, THIS IS A RECOVERABLE ERROR.)           
C                                                                       
      IF (NB .EQ. NCALC) RETURN                                         
C                                                                       
      NCALC = NCALC+10                                                  
C/6S                                                                    
C     CALL SETERR(                                                      
C    1    38HDBESCI - ONLY THIS MANY ANSWERS ARE OK,38,NCALC,1)         
C/7S                                                                    
      CALL SETERR(                                                      
     1    'DBESCI - ONLY THIS MANY ANSWERS ARE OK',38,NCALC,1)          
C/                                                                      
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DB1SLR(X,NB,IZE,B,NCALC)                               
C THIS ROUTINE CALCULATES BESSEL FUNCTIONS I AND J OF REAL              
C ARGUMENT AND INTEGER ORDER.                                           
C                                                                       
C                                                                       
C      EXPLANATION OF VARIABLES IN THE CALLING SEQUENCE                 
C                                                                       
C X     DOUBLE PRECISION REAL ARGUMENT FOR WHICH I*S OR J*S             
C       ARE TO BE CALCULATED.  IF I*S ARE TO BE CALCULATED,             
C       ABS(X) MUST BE LESS THAN EXPARG (WHICH SEE BELOW).              
C NB    INTEGER TYPE.  1 + HIGHEST ORDER TO BE CALCULATED.              
C       IT MUST BE POSITIVE.                                            
C IZE   INTEGER TYPE.  ZERO IF J*S ARE TO BE CALCULATED, 1              
C       IF I*S ARE TO BE CALCULATED.                                    
C B     DOUBLE PRECISION VECTOR OF LENGTH NB, NEED NOT BE               
C       INITIALIZED BY USER.  IF THE ROUTINE TERMINATES                 
C       NORMALLY (NCALC=NB), IT RETURNS J(OR I)-SUB-ZERO                
C       THROUGH J(OR I)-SUB-NB-MINUS-ONE OF X IN THIS                   
C       VECTOR.                                                         
C NCALC INTEGER TYPE, NEED NOT BE INITIALIZED BY USER.                  
C       BEFORE USING THE RESULTS, THE USER SHOULD CHECK THAT            
C       NCALC=NB, I.E. ALL ORDERS HAVE BEEN CALCULATED TO               
C       THE DESIRED ACCURACY.  SEE ERROR RETURNS BELOW.                 
C                                                                       
C                                                                       
C     EXPLANATION OF MACHINE-DEPENDENT CONSTANTS                        
C                                                                       
C NSIG  DECIMAL SIGNIFICANCE DESIRED.  SHOULD BE SET TO                 
C       IFIX(ALOG10(2)*NBIT+1), WHERE NBIT IS THE NUMBER OF             
C       BITS IN THE MANTISSA OF A DOUBLE PRECISION VARIABLE.            
C       SETTING NSIG LOWER WILL RESULT IN DECREASED ACCURACY            
C       WHILE SETTING NSIG HIGHER WILL INCREASE CPU TIME                
C       WITHOUT INCREASING ACCURACY.  THE TRUNCATION ERROR              
C       IS LIMITED TO T=.5*10**-NSIG FOR J*S OF ORDER LESS              
C       THAN ARGUMENT, AND TO A RELATIVE ERROR OF T FOR                 
C       I*S AND THE OTHER J*S.                                          
C NTEN  LARGEST INTEGER K SUCH THAT 10**K IS MACHINE-                   
C       REPRESENTABLE IN DOUBLE PRECISION.                              
C LARGEX UPPER LIMIT ON THE MAGNITUDE OF X.  BEAR IN MIND               
C       THAT IF ABS(X)=N, THEN AT LEAST N ITERATIONS OF THE             
C       BACKWARD RECURSION WILL BE EXECUTED.                            
C EXPARG LARGEST DOUBLE PRECISION ARGUMENT THAT THE LIBRARY             
C       DEXP ROUTINE CAN HANDLE.                                        
C                                                                       
C PORT NOTE, SEPTEMBER 8,1976 -                                         
C THE LARGEX AND EXPARG TESTS ARE MADE IN THE OUTER ROUTINES -          
C DBESRJ AND DBESRI, WHICH CALL DB1SLR.                                 
C                                                                       
C                                                                       
C                  ERROR RETURNS                                        
C                                                                       
C PORT NOTE, SEPTEMBER 8, 1976 -                                        
C THE NOTES BELOW ARE KEPT IN FOR THE RECORD, BUT, AS ABOVE,            
C THE ACTUAL TESTS ARE NOW IN THE OUTER CALLING ROUTINES.               
C                                                                       
C       LET G DENOTE EITHER I OR J.                                     
C       IN CASE OF AN ERROR, NCALC.NE.NB, AND NOT ALL G*S               
C  ARE CALCULATED TO THE DESIRED ACCURACY.                              
C       IF NCALC.LT.0, AN ARGUMENT IS OUT OF RANGE.  NB.LE.0            
C  OR IZE IS NEITHER 0 NOR 1 OR IZE=1 AND ABS(X).GE.EXPARG.             
C  IN THIS CASE, THE B-VECTOR IS NOT CALCULATED, AND NCALC              
C  IS SET TO MIN0(NB,0)-1 SO NCALC.NE.NB.                               
C       NB.GT.NCALC.GT.0 WILL OCCUR IF NB.GT.MAGX AND ABS(G-            
C  SUB-NB-OF-X/G-SUB-MAGX+NP-OF-X).LT.10.**(NTEN/2), I.E. NB            
C  IS MUCH GREATER THAN MAGX.  IN THIS CASE, B(N) IS CALCU-             
C  LATED TO THE DESIRED ACCURACY FOR N.LE.NCALC, BUT FOR                
C  NCALC.LT.N.LE.NB, PRECISION IS LOST.  IF N.GT.NCALC AND              
C  ABS(B(NCALC)/B(N)).EQ.10**-K, THEN THE LAST K SIGNIFICANT            
C  FIGURES OF B(N) ARE ERRONEOUS.  IF THE USER WISHES TO                
C  CALCULATE B(N) TO HIGHER ACCURACY, HE SHOULD USE AN                  
C  ASYMPTOTIC FORMULA FOR LARGE ORDER.                                  
C                                                                       
      DOUBLE PRECISION DLOG10, DSQRT, DEXP,                             
     1 X,B,P,TEST,TEMPA,TEMPB,TEMPC,SIGN,SUM,TOVER,                     
     2 PLAST,POLD,PSAVE,PSAVEL,D1MACH                                   
      DIMENSION B(NB)                                                   
      DATA NSIG/0/, NTEN/0/                                             
      IF(NSIG .NE. 0) GO TO 1                                           
      NSIG = IFIX(-ALOG10(SNGL(D1MACH(3)))+1.)                          
      NTEN = DLOG10(D1MACH(2))                                          
    1 TEMPA=DABS(X)                                                     
      MAGX=IFIX(SNGL(TEMPA))                                            
C                                                                       
      SIGN=DBLE(FLOAT(1-2*IZE))                                         
      NCALC=NB                                                          
C USE 2-TERM ASCENDING SERIES FOR SMALL X                               
      IF(TEMPA**4.LT..1D0**NSIG) GO TO 30                               
C INITIALIZE THE CALCULATION OF P*S                                     
      NBMX=NB-MAGX                                                      
      N=MAGX+1                                                          
      PLAST=1.D0                                                        
      P=DBLE(FLOAT(2*N))/TEMPA                                          
C CALCULATE GENERAL SIGNIFICANCE TEST                                   
      TEST=2.D0*1.D1**NSIG                                              
      IF(IZE.EQ.1.AND.2*MAGX.GT.5*NSIG) TEST=DSQRT(TEST*P)              
      IF(IZE.EQ.1.AND.2*MAGX.LE.5*NSIG) TEST=TEST/1.585**MAGX           
      M=0                                                               
      IF(NBMX.LT.3) GO TO 4                                             
C CALCULATE P*S UNTIL N=NB-1.  CHECK FOR POSSIBLE OVERFLOW.             
      TOVER=1.D1**(NTEN-NSIG)                                           
      NSTART=MAGX+2                                                     
      NEND=NB-1                                                         
      DO 3 N=NSTART,NEND                                                
      POLD=PLAST                                                        
      PLAST=P                                                           
      P=DBLE(FLOAT(2*N))*PLAST/TEMPA-SIGN*POLD                          
      IF(P-TOVER) 3,3,5                                                 
    3 CONTINUE                                                          
C CALCULATE SPECIAL SIGNIFICANCE TEST FOR NBMX.GT.2.                    
      TEST=DMAX1(TEST,DSQRT(PLAST*1.D1**NSIG)*DSQRT(2.D0*P))            
C CALCULATE P*S UNTIL SIGNIFICANCE TEST PASSES                          
    4 N=N+1                                                             
      POLD=PLAST                                                        
      PLAST=P                                                           
      P=DBLE(FLOAT(2*N))*PLAST/TEMPA-SIGN*POLD                          
      IF(P.LT.TEST) GO TO 4                                             
      IF(IZE.EQ.1.OR.M.EQ.1) GO TO 12                                   
C FOR J*S, A STRONG VARIANT OF THE TEST IS NECESSARY.                   
C CALCULATE IT, AND CALCULATE P*S UNTIL THIS TEST IS PASSED.            
      M=1                                                               
      TEMPB=P/PLAST                                                     
      TEMPC=DBLE(FLOAT(N+1))/TEMPA                                      
      IF(TEMPB+1.D0/TEMPB.GT.2.D0*TEMPC)TEMPB=TEMPC+DSQRT(TEMPC**2-1.D0)
      TEST=TEST/DSQRT(TEMPB-1.D0/TEMPB)                                 
      IF(P-TEST) 4,12,12                                                
C TO AVOID OVERFLOW, DIVIDE P*S BY TOVER.  CALCULATE P*S                
C UNTIL ABS(P).GT.1.                                                    
    5 TOVER=1.D1**NTEN                                                  
      P=P/TOVER                                                         
      PLAST=PLAST/TOVER                                                 
      PSAVE=P                                                           
      PSAVEL=PLAST                                                      
      NSTART=N+1                                                        
    6 N=N+1                                                             
      POLD=PLAST                                                        
      PLAST=P                                                           
      P=DBLE(FLOAT(2*N))*PLAST/TEMPA-SIGN*POLD                          
      IF(P.LE.1.D0) GO TO 6                                             
      TEMPB=DBLE(FLOAT(2*N))/TEMPA                                      
      IF(IZE.EQ.1) GO TO 8                                              
      TEMPC=.5D0*TEMPB                                                  
      TEMPB=PLAST/POLD                                                  
      IF(TEMPB+1.D0/TEMPB.GT.2.D0*TEMPC)TEMPB=TEMPC+DSQRT(TEMPC**2-1.D0)
C CALCULATE BACKWARD TEST, AND FIND NCALC, THE HIGHEST N                
C SUCH THAT THE TEST IS PASSED.                                         
    8 TEST=.5D0*POLD*PLAST*(1.D0-1.D0/TEMPB**2)/1.D1**NSIG              
      P=PLAST*TOVER                                                     
      N=N-1                                                             
      NEND=MIN0(NB,N)                                                   
      DO 9 NCALC=NSTART,NEND                                            
      POLD=PSAVEL                                                       
      PSAVEL=PSAVE                                                      
      PSAVE=DBLE(FLOAT(2*N))*PSAVEL/TEMPA-SIGN*POLD                     
      IF(PSAVE*PSAVEL-TEST) 9,9,10                                      
    9 CONTINUE                                                          
      NCALC=NEND+1                                                      
   10 NCALC=NCALC-1                                                     
C THE SUM B(1)+2B(3)+2B(5)... IS USED TO NORMALIZE.  M, THE             
C COEFFICIENT OF B(N), IS INITIALIZED TO 2 OR 0.                        
   12 N=N+1                                                             
      M=2*N-4*(N/2)                                                     
C INITIALIZE THE BACKWARD RECURSION AND THE NORMALIZATION               
C SUM                                                                   
      TEMPB=0.D0                                                        
      TEMPA=1.D0/P                                                      
      SUM=DBLE(FLOAT(M))*TEMPA                                          
      NEND=N-NB                                                         
      IF(NEND) 17,15,13                                                 
C RECUR BACKWARD VIA DIFFERENCE EQUATION, CALCULATING (BUT              
C NOT STORING) B(N), UNTIL N=NB.                                        
   13 DO 14 L=1,NEND                                                    
      N=N-1                                                             
      TEMPC=TEMPB                                                       
      TEMPB=TEMPA                                                       
      TEMPA=(DBLE(FLOAT(2*N))*TEMPB)/X-SIGN*TEMPC                       
      M=2-M                                                             
   14 SUM=SUM+DBLE(FLOAT(M))*TEMPA                                      
C STORE B(NB)                                                           
   15 B(N)=TEMPA                                                        
      IF(NB.GT.1) GO TO 16                                              
C NB=1.  SINCE 2*TEMPA WAS ADDED TO THE SUM, TEMPA MUST BE              
C SUBTRACTED                                                            
      SUM=SUM-TEMPA                                                     
      GO TO 23                                                          
C CALCULATE AND STORE B(NB-1)                                           
   16 N=N-1                                                             
      B(N) =(DBLE(FLOAT(2*N))*TEMPA)/X-SIGN*TEMPB                       
      IF(N.EQ.1) GO TO 22                                               
      M=2-M                                                             
      SUM=SUM+DBLE(FLOAT(M))*B(N)                                       
      GO TO 19                                                          
C N.LT.NB, SO STORE B(N) AND SET HIGHER ORDERS TO ZERO                  
   17 B(N)=TEMPA                                                        
      NEND=-NEND                                                        
      DO 18 L=1,NEND                                                    
      K=N+L                                                             
   18 B(K)=0.D0                                                         
   19 NEND=N-2                                                          
      IF(NEND.EQ.0) GO TO 21                                            
C CALCULATE VIA DIFFERENCE EQUATION AND STORE B(N),                     
C UNTIL N=2                                                             
      DO 20 L=1,NEND                                                    
      N=N-1                                                             
      B(N)=(DBLE(FLOAT(2*N))*B(N+1))/X-SIGN*B(N+2)                      
      M=2-M                                                             
   20 SUM=SUM+DBLE(FLOAT(M))*B(N)                                       
C CALCULATE B(1)                                                        
   21 B(1)=2.D0*B(2)/X-SIGN*B(3)                                        
   22 SUM=SUM+B(1)                                                      
C NORMALIZE--IF IZE=1, DIVIDE SUM BY COSH(X).  DIVIDE ALL               
C B(N) BY SUM.                                                          
   23 IF(IZE.EQ.0) GO TO 25                                             
      TEMPA=DEXP(DABS(X))                                               
      SUM=2.D0*SUM/(TEMPA+1.D0/TEMPA)                                   
   25 DO 26 N=1,NB                                                      
   26 B(N)=B(N)/SUM                                                     
      RETURN                                                            
C                                                                       
C TWO-TERM ASCENDING SERIES FOR SMALL X                                 
   30 TEMPA=1.D0                                                        
      TEMPB=-.25D0*X*X*SIGN                                             
      B(1)=1.D0+TEMPB                                                   
      IF(NB.EQ.1) GO TO 32                                              
      DO 31 N=2,NB                                                      
      TEMPA=TEMPA*X/DBLE(FLOAT(2*N-2))                                  
   31 B(N)=TEMPA*(1.D0+TEMPB/DBLE(FLOAT(N)))                            
   32 RETURN                                                            
      END                                                               
      SUBROUTINE DB1SLC(X,Y,NB,IZE,BR,BI,NCALC)                         
C THIS ROUTINE CALCULATES BESSEL FUNCTIONS I AND J OF                   
C COMPLEX ARGUMENT AND INTEGER ORDER.                                   
C                                                                       
C                                                                       
C      EXPLANATION OF VARIABLES IN THE CALLING SEQUENCE                 
C                                                                       
C X     DOUBLE PRECISION REAL PART OF THE COMPLEX ARGUMENT              
C       FOR WHICH I*S OR J*S ARE TO BE CALCULATED.  IF I*S              
C       ARE TO BE CALCULATED, ABS(X) MUST NOT EXCEED EXPARG             
C       (WHICH SEE BELOW).                                              
C Y     IMAGINARY PART OF THE ARGUMENT.  IF J*S ARE TO BE               
C       CALCULATED, ABS(Y) MUST NOT EXCEED EXPARG.                      
C NB    INTEGER TYPE.  1 + HIGHEST ORDER TO BE CALCULATED.              
C       IT MUST BE POSITIVE.                                            
C IZE   INTEGER TYPE.  ZERO IF J*S ARE TO BE CALCULATED, 1              
C       IF I*S ARE TO BE CALCULATED.                                    
C BR    DOUBLE PRECISION VECTOR OF LENGTH NB, NEED NOT BE               
C       INITIALIZED BY USER.  IF THE ROUTINE TERMINATES                 
C       NORMALLY, (NCALC=NB), IT RETURNS THE REAL PART OF               
C       J(OR I)-SUB-ZERO THROUGH J(OR I)-SUB-NB-MINUS-ONE               
C       OF Z IN THIS VECTOR.                                            
C BI    IMAGINARY ANALOG OF BR.                                         
C NCALC INTEGER TYPE, NEED NOT BE INITIALIZED BY USER.                  
C       BEFORE USING THE RESULTS, THE USER SHOULD CHECK THAT            
C       NCALC=NB, I.E. ALL ORDERS HAVE BEEN CALCULATED TO               
C       THE DESIRED ACCURACY.  SEE ERROR RETURNS BELOW.                 
C                                                                       
C                                                                       
C       EXPLANATION OF MACHINE-DEPENDENT CONSTANTS                      
C                                                                       
C NSIG  DECIMAL SIGNIFICANCE DESIRED.  SHOULD BE SET TO                 
C       IFIX(ALOG10(2)*NBIT+1), WHERE NBIT IS THE NUMBER OF             
C       BITS IN THE MANTISSA OF A DOUBLE PRECISION VARIABLE.            
C       SETTING NSIG HIGHER WILL INCREASE CPU TIME WITHOUT              
C       INCREASING ACCURACY, WHILE SETTING NSIG LOWER WILL              
C       DECREASE ACCURACY.  IF ONLY SINGLE-PRECISION                    
C       ACCURACY IS DESIRED, REPLACE NBIT BY THE NUMBER OF              
C       BITS IN THE MANTISSA OF A SINGLE-PRECISION VARIABLE.            
C       THE RELATIVE TRUNCATION ERROR IS LIMITED TO T=.5*10             
C       **-NSIG FOR ORDER GREATER THAN ABS(Z), AND FOR ORDER            
C       LESS THAN ABS(Z) (GENERAL TEST), THE RELATIVE ERROR             
C       IS LIMITED TO T FOR FUNCTION VALUES OF MAGNITUDE AT             
C       LEAST 1, AND THE ABSOLUTE ERROR IS LIMITED TO T FOR             
C       SMALLER VALUES.                                                 
C NTEN  LARGEST INTEGER K SUCH THAT 10**K IS MACHINE-                   
C       REPRESENTABLE IN DOUBLE PRECISION.                              
C LARGEZ UPPER LIMIT ON THE MAGNITUDE OF Z.  BEAR IN MIND               
C       THAT IF ABS(Z)=N, THEN AT LEAST N ITERATIONS OF THE             
C       BACKWARD RECURSION WILL BE EXECUTED.                            
C EXPARG LARGEST DOUBLE PRECISION ARGUMENT THAT THE LIBRARY             
C       DEXP ROUTINE CAN HANDLE.                                        
C                                                                       
C PORT NOTE, SEPTEMBER 16, 1976 -                                       
C THE LARGEX AND EXPARG TESTS ARE MADE IN THE OUTER ROUTINES -          
C DBESCJ AND DBESCI, WHICH CALL DB1SLR.                                 
C                                                                       
C                                                                       
C                                                                       
C                            ERROR RETURNS                              
C                                                                       
C PORT NOTE, SEPTEMBER 16, 1976 -                                       
C THE NOTES BELOW ARE KEPT IN FOR THE RECORD, BUT, AS ABOVE,            
C THE ACTUAL TESTS ARE NOW IN THE OUTER CALLING ROUTINES.               
C                                                                       
C       LET G DENOTE EITHER I OR J.                                     
C       IN CASE OF AN ERROR, NCALC.NE.NB, AND NOT ALL G*S               
C  ARE CALCULATED TO THE DESIRED ACCURACY.                              
C       IF NCALC.LT.0, AN ARGUMENT IS OUT OF RANGE.  NB.LE.0            
C  OR IZE IS NEITHER 0 NOR 1 OR IZE=0 AND ABS(Y).GT.EXPARG,             
C  OR IZE=1 AND ABS(X).GT.EXPARG.  IN THIS CASE, THE VECTORS            
C  BR AND BI ARE NOT CALCULATED, AND NCALC IS SET TO                    
C  MIN0(NB,0)-1 SO NCALC.NE.NB.                                         
C       NB.GT.NCALC.GT.0 WILL OCCUR IF NB.GT.MAGZ AND ABS(G-            
C  SUB-NB-OF-Z/G-SUB-MAGX+NP-OF-Z).LT.10.**(NTEN/2), I.E. NB            
C  IS MUCH GREATER THAN MAGZ.  IN THIS CASE, BR(N) AND BI(N)            
C  ARE CALCULATED TO THE DESIRED ACCURACY FOR N.LE.NCALC,               
C  BUT FOR NCALC.LT.N.LE.NB, PRECISION IS LOST.  IF N.GT.               
C  NCALC AND ABS(G(NCALC-1)/G(N-1)).EQ.10**-K, THEN THE LAST            
C  K SIGNIFICANT FIGURES OF G(N-1) (=BR(N)+I*BI(N)) ARE                 
C  ERRONEOUS.  IF THE USER WISHES TO CALCULATE G(N-1) TO                
C  HIGHER ACCURACY, HE SHOULD USE AN ASYMPTOTIC FORMULA FOR             
C  LARGE ORDER.                                                         
C                                                                       
      DOUBLE PRECISION DLOG10, DSQRT, DEXP, DCOS, DSIN,                 
     1 X,Y,BR,BI,PR,PI,PLASTR,PLASTI,POLDR,POLDI,PSAVER,                
     2 PSAVEI,TEST,TOVER,TEMPAR,TEMPAI,TEMPBR,TEMPBI,                   
     3 TEMPCR,TEMPCI,SIGN,SUMR,SUMI,ZINVR,ZINVI,D1MACH                  
      DIMENSION BR(NB),BI(NB)                                           
      DATA NSIG/0/, NTEN/0/                                             
      IF(NSIG .NE. 0) GO TO 1                                           
      NSIG = IFIX(-ALOG10(SNGL(D1MACH(3)))+1.)                          
      NTEN = DLOG10(D1MACH(2))                                          
    1 TEMPAR=DSQRT(X*X+Y*Y)                                             
      MAGZ=IFIX(SNGL(TEMPAR))                                           
      SIGN=DBLE(FLOAT(1-2*IZE))                                         
      NCALC=NB                                                          
C USE 2-TERM ASCENDING SERIES FOR SMALL Z                               
      IF(TEMPAR**4.LT..1D0**NSIG) GO TO 50                              
C INITIALIZE THE CALCULATION OF THE P*S                                 
      NBMZ=NB-MAGZ                                                      
      N=MAGZ+1                                                          
      IF(DABS(X).LT.DABS(Y)) GO TO 2                                    
      ZINVR=1.D0/(X+Y*Y/X)                                              
      ZINVI=-Y*ZINVR/X                                                  
      GO TO 3                                                           
    2 ZINVI=-1.D0/(Y+X*X/Y)                                             
      ZINVR=-X*ZINVI/Y                                                  
    3 PLASTR=1.D0                                                       
      PLASTI=0.D0                                                       
      PR=SIGN*DBLE(FLOAT(2*N))*ZINVR                                    
      PI=SIGN*DBLE(FLOAT(2*N))*ZINVI                                    
      TEST=2.D0*1.D1**NSIG                                              
      M=0                                                               
      IF(NBMZ.LT.3) GO TO 6                                             
C CALCULATE P*S UNTIL N=NB-1.  CHECK FOR POSSIBLE OVERFLOW.             
      TOVER=1.D1**(NTEN-NSIG)                                           
      NSTART=MAGZ+2                                                     
      NEND=NB-1                                                         
      DO 5 N=NSTART,NEND                                                
      POLDR=PLASTR                                                      
      POLDI=PLASTI                                                      
      PLASTR=PR                                                         
      PLASTI=PI                                                         
      PR=SIGN*(DBLE(FLOAT(2*N))*(PLASTR*ZINVR-PLASTI*ZINVI)-POLDR)      
      PI=SIGN*(DBLE(FLOAT(2*N))*(PLASTI*ZINVR+PLASTR*ZINVI)-POLDI)      
      IF((PR/TOVER)**2+(PI/TOVER)**2-1.D0) 5,5,7                        
    5 CONTINUE                                                          
      N=NEND                                                            
C CALCULATE SPECIAL SIGNIFICANCE TEST FOR NBMZ.GT.2.                    
      TEMPBI=DMAX1(DABS(PR),DABS(PI))                                   
      TEMPBI=TEMPBI*DSQRT(2.D0*1.D1**NSIG*DSQRT(((PR/TEMPBI)**2         
     1+(PI/TEMPBI)**2)*((PLASTR/TEMPBI)**2+(PLASTI/TEMPBI)**2)))        
      TEST=DMAX1(TEST,TEMPBI)                                           
C CALCULATE P*S UNTIL SIGNIFICANCE TEST IS PASSED.                      
    6 N=N+1                                                             
      POLDR=PLASTR                                                      
      POLDI=PLASTI                                                      
      PLASTR=PR                                                         
      PLASTI=PI                                                         
      PR=SIGN*(DBLE(FLOAT(2*N))*(PLASTR*ZINVR-PLASTI*ZINVI)-POLDR)      
      PI=SIGN*(DBLE(FLOAT(2*N))*(PLASTI*ZINVR+PLASTR*ZINVI)-POLDI)      
      IF((PR/TEST)**2+(PI/TEST)**2.LT.1.D0) GO TO 6                     
      IF(M.EQ.1) GO TO 12                                               
C CALCULATE STRICT VARIANT OF SIGNIFICANCE TEST, AND                    
C CALCULATE P*S UNTIL THIS TEST IS PASSED.                              
      M=1                                                               
      TEMPBI=DMAX1(DABS(PR),DABS(PI))                                   
      TEMPBR=DSQRT(((PR/TEMPBI)**2+(PI/TEMPBI)**2)/                     
     1 ((PLASTR/TEMPBI)**2+(PLASTI/TEMPBI)**2))                         
      TEMPBI=DBLE(FLOAT(N+1))/TEMPAR                                    
      IF(TEMPBR+1.D0/TEMPBR.GT.2.D0*TEMPBI) TEMPBR=TEMPBI               
     1 +DSQRT(TEMPBI**2-1.D0)                                           
      TEST=TEST/DSQRT(TEMPBR-1.D0/TEMPBR)                               
      IF((PR/TEST)**2+(PI/TEST)**2-1.D0) 6,12,12                        
    7 NSTART=N+1                                                        
C TO AVOID OVERFLOW, NORMALIZE P*S BY DIVIDING BY TOVER.                
C CALCULATE P*S UNTIL UNNORMALIZED P WOULD OVERFLOW.                    
      PR=PR/TOVER                                                       
      PI=PI/TOVER                                                       
      PLASTR=PLASTR/TOVER                                               
      PLASTI=PLASTI/TOVER                                               
      PSAVER=PR                                                         
      PSAVEI=PI                                                         
      TEMPCR=PLASTR                                                     
      TEMPCI=PLASTI                                                     
      TEST=1.D1**(2*NSIG)                                               
    8 N=N+1                                                             
      POLDR=PLASTR                                                      
      POLDI=PLASTI                                                      
      PLASTR=PR                                                         
      PLASTI=PI                                                         
      PR=SIGN*(DBLE(FLOAT(2*N))*(PLASTR*ZINVR-PLASTI*ZINVI)-POLDR)      
      PI=SIGN*(DBLE(FLOAT(2*N))*(PLASTI*ZINVR+PLASTR*ZINVI)-POLDI)      
      IF(PR**2+PI**2.LE.TEST) GO TO 8                                   
C CALCULATE BACKWARD TEST, AND FIND NCALC, THE HIGHEST N                
C SUCH THAT THE TEST IS PASSED.                                         
      TEMPBR=DSQRT((PLASTR**2+PLASTI**2)/(POLDR**2+POLDI**2))           
      TEMPBI=DBLE(FLOAT(N))/TEMPAR                                      
      IF(TEMPBR+1.D0/TEMPBR.GT.2.D0*TEMPBI) TEMPBR=TEMPBI+              
     1 DSQRT(TEMPBI**2-1.D0)                                            
      TEST=.5D0*(1.D0-1.D0/TEMPBR**2)/1.D1**NSIG                        
      TEST=((PLASTR**2+PLASTI**2)*TEST)*((POLDR**2+POLDI**2)*TEST)      
      PR=PLASTR*TOVER                                                   
      PI=PLASTI*TOVER                                                   
      N=N-1                                                             
      NEND=MIN0(NB,N)                                                   
      DO 9 NCALC=NSTART,NEND                                            
      POLDR=TEMPCR                                                      
      POLDI=TEMPCI                                                      
      TEMPCR=PSAVER                                                     
      TEMPCI=PSAVEI                                                     
      PSAVER=SIGN*(DBLE(FLOAT(2*N))*(TEMPCR*ZINVR-TEMPCI*ZINVI)-POLDR)  
      PSAVEI=SIGN*(DBLE(FLOAT(2*N))*(TEMPCI*ZINVR+TEMPCR*ZINVI)-POLDI)  
      IF((PSAVER**2+PSAVEI**2)*(TEMPCR**2+TEMPCI**2)-TEST) 9,9,10       
    9 CONTINUE                                                          
      NCALC=NEND+1                                                      
   10 NCALC=NCALC-1                                                     
C THE COEFFICIENT OF B(N) IN THE NORMALIZATION SUM IS                   
C M*SQRT(-1)**IMAG, WHERE M=-2,0, OR 2, AND IMAG IS 0 OR 1.             
C CALCULATE RECURSION RULES FOR M AND IMAG, AND INITIALIZE              
C THEM.                                                                 
   12 N=N+1                                                             
      TEMPBR=DBLE(FLOAT(IZE))*X+DBLE(FLOAT(1-IZE))*Y                    
      IPOS=0                                                            
      IF(TEMPBR) 13,14,13                                               
   13 IPOS=IFIX(SNGL(1.1D0*TEMPBR/DABS(TEMPBR)))                        
   14 MRECUR=4*((2+IZE+IPOS)/2)-3-2*(IZE+IPOS)                          
      K=2+IPOS+2*IZE*IPOS**2-IZE                                        
      L=N-4*(N/4)                                                       
      MLAST=2+8*((K*L)/4)-4*((K*L)/2)                                   
      IF(IPOS.EQ.0.AND.(L.EQ.1.OR.L.EQ.3)) MLAST=0                      
      L=L+3-4*((L+3)/4)                                                 
      M    =2+8*((K*L)/4)-4*((K*L)/2)                                   
      IF(IPOS.EQ.0.AND.(L.EQ.1.OR.L.EQ.3)) M=0                          
      IMRECR=(1-IZE)*IPOS**2                                            
      IMAG=IMRECR*(L-2*(L/2))                                           
C INITIALIZE THE BACKWARD RECURSION AND THE NORMALIZATION               
C SUM.                                                                  
      TEMPBR=0.D0                                                       
      TEMPBI=0.D0                                                       
      IF(DABS(PI).GT.DABS(PR)) GO TO 15                                 
      TEMPAR=1.D0/(PR+PI*(PI/PR))                                       
      TEMPAI=-(PI*TEMPAR)/PR                                            
      GO TO 16                                                          
   15 TEMPAI=-1.D0/(PI+PR*(PR/PI))                                      
      TEMPAR=-(PR*TEMPAI)/PI                                            
   16 IF(IMAG.NE.0) GO TO 17                                            
      SUMR=DBLE(FLOAT(M))*TEMPAR                                        
      SUMI=DBLE(FLOAT(M))*TEMPAI                                        
      GO TO 18                                                          
   17 SUMR=-DBLE(FLOAT(M))*TEMPAI                                       
      SUMI=DBLE(FLOAT(M))*TEMPAR                                        
   18 NEND=N-NB                                                         
      IF(NEND) 26,22,19                                                 
C RECUR BACKWARD VIA DIFFERENCE EQUATION CALCULATING (BUT               
C NOT STORING) BR(N) AND BI(N) UNTIL N=NB.                              
   19 DO 21 L=1,NEND                                                    
      N=N-1                                                             
      TEMPCR=TEMPBR                                                     
      TEMPCI=TEMPBI                                                     
      TEMPBR=TEMPAR                                                     
      TEMPBI=TEMPAI                                                     
      PR=DBLE(FLOAT(2*N))*ZINVR                                         
      PI=DBLE(FLOAT(2*N))*ZINVI                                         
      TEMPAR=PR*TEMPBR-PI*TEMPBI-SIGN*TEMPCR                            
      TEMPAI=PR*TEMPBI+PI*TEMPBR-SIGN*TEMPCI                            
      IMAG=(1-IMAG)*IMRECR                                              
      K=MLAST                                                           
      MLAST=M                                                           
      M=K*MRECUR                                                        
      IF(IMAG.NE.0) GO TO 20                                            
      SUMR=SUMR+DBLE(FLOAT(M))*TEMPAR                                   
      SUMI=SUMI+DBLE(FLOAT(M))*TEMPAI                                   
      GO TO 21                                                          
   20 SUMR=SUMR-DBLE(FLOAT(M))*TEMPAI                                   
      SUMI=SUMI+DBLE(FLOAT(M))*TEMPAR                                   
   21 CONTINUE                                                          
C STORE BR(NB), BI(NB)                                                  
   22 BR(N)=TEMPAR                                                      
      BI(N)=TEMPAI                                                      
      IF(N.GT.1) GO TO 23                                               
C NB=1.  SINCE 2*TEMPAR AND 2*TEMPAI WERE ADDED TO SUMR AND             
C SUMI RESPECTIVELY, WE MUST SUBTRACT TEMPAR AND TEMPAI                 
      SUMR=SUMR-TEMPAR                                                  
      SUMI=SUMI-TEMPAI                                                  
      GO TO 35                                                          
C CALCULATE AND STORE BR(NB-1),BI(NB-1)                                 
   23 N=N-1                                                             
      PR=DBLE(FLOAT(2*N))*ZINVR                                         
      PI=DBLE(FLOAT(2*N))*ZINVI                                         
      BR(N)=PR*TEMPAR-PI*TEMPAI-SIGN*TEMPBR                             
      BI(N)=PR*TEMPAI+PI*TEMPAR-SIGN*TEMPBI                             
      IF(N.EQ.1) GO TO 34                                               
      IMAG=(1-IMAG)*IMRECR                                              
      K=MLAST                                                           
      MLAST=M                                                           
      M=K*MRECUR                                                        
      IF(IMAG.NE.0) GO TO 24                                            
      SUMR=SUMR+DBLE(FLOAT(M))*BR(N)                                    
      SUMI=SUMI+DBLE(FLOAT(M))*BI(N)                                    
      GO TO 30                                                          
   24 SUMR=SUMR-DBLE(FLOAT(M))*BI(N)                                    
      SUMI=SUMI+DBLE(FLOAT(M))*BR(N)                                    
      GO TO 30                                                          
C N.LT.NB, SO STORE BR(N), BI(N), AND SET HIGHER ORDERS ZERO            
   26 BR(N)=TEMPAR                                                      
      BI(N)=TEMPAI                                                      
      NEND=-NEND                                                        
      DO 27 L=1,NEND                                                    
      NPL=N+L                                                           
      BR(NPL)=0.D0                                                      
   27 BI(NPL)=0.D0                                                      
   30 NEND=N-2                                                          
      IF(NEND.EQ.0) GO TO 33                                            
C CALCULATE VIA DIFFERENCE EQUATION AND STORE BR(N),BI(N),              
C UNTIL N=2                                                             
      DO 32 L=1,NEND                                                    
      N=N-1                                                             
      PR=DBLE(FLOAT(2*N))*ZINVR                                         
      PI=DBLE(FLOAT(2*N))*ZINVI                                         
      BR(N)=PR*BR(N+1)-PI*BI(N+1)-SIGN*BR(N+2)                          
      BI(N)=PR*BI(N+1)+PI*BR(N+1)-SIGN*BI(N+2)                          
      IMAG=(1-IMAG)*IMRECR                                              
      K=MLAST                                                           
      MLAST=M                                                           
      M=K*MRECUR                                                        
      IF(IMAG.NE.0) GO TO 31                                            
      SUMR=SUMR+DBLE(FLOAT(M))*BR(N)                                    
      SUMI=SUMI+DBLE(FLOAT(M))*BI(N)                                    
      GO TO 32                                                          
   31 SUMR=SUMR-DBLE(FLOAT(M))*BI(N)                                    
      SUMI=SUMI+DBLE(FLOAT(M))*BR(N)                                    
   32 CONTINUE                                                          
C CALCULATE AND STORE BR(1), BI(1)                                      
   33 BR(1)=2.D0*(BR(2)*ZINVR-BI(2)*ZINVI)-SIGN*BR(3)                   
      BI(1)=2.D0*(BR(2)*ZINVI+BI(2)*ZINVR)-SIGN*BI(3)                   
   34 SUMR=SUMR+BR(1)                                                   
      SUMI=SUMI+BI(1)                                                   
C CALCULATE NORMALIZATION FACTOR, TEMPAR +I*TEMPAI                      
   35 IF(IZE.EQ.1) GO TO 36                                             
      TEMPCR=DBLE(FLOAT(IPOS))*Y                                        
      TEMPCI=DBLE(FLOAT(-IPOS))*X                                       
      GO TO 37                                                          
   36 TEMPCR=DBLE(FLOAT(IPOS))*X                                        
      TEMPCI=DBLE(FLOAT(IPOS))*Y                                        
   37 TEMPCR=DEXP(TEMPCR)                                               
      TEMPBR=DCOS(TEMPCI)                                               
      TEMPBI=DSIN(TEMPCI)                                               
      IF(DABS(SUMR).LT.DABS(SUMI)) GO TO 38                             
      TEMPCI=SUMI/SUMR                                                  
      TEMPCR=(TEMPCR/SUMR)/(1.D0+TEMPCI*TEMPCI)                         
      TEMPAR=TEMPCR*(TEMPBR+TEMPBI*TEMPCI)                              
      TEMPAI=TEMPCR*(TEMPBI-TEMPBR*TEMPCI)                              
      GO TO 39                                                          
   38 TEMPCI=SUMR/SUMI                                                  
      TEMPCR=(TEMPCR/SUMI)/(1.D0+TEMPCI*TEMPCI)                         
      TEMPAR=TEMPCR*(TEMPBR*TEMPCI+TEMPBI)                              
      TEMPAI=TEMPCR*(TEMPBI*TEMPCI-TEMPBR)                              
C NORMALIZE                                                             
   39 DO 40 N=1,NB                                                      
      TEMPBR=BR(N)*TEMPAR-BI(N)*TEMPAI                                  
      BI(N)=BR(N)*TEMPAI+BI(N)*TEMPAR                                   
   40 BR(N)=TEMPBR                                                      
      RETURN                                                            
C TWO-TERM ASCENDING SERIES FOR SMALL Z                                 
   50 TEMPAR=1.D0                                                       
      TEMPAI=0.D0                                                       
      TEMPCR=.25D0*(X*X-Y*Y)                                            
      TEMPCI=.5D0*X*Y                                                   
      BR(1)=1.D0-SIGN*TEMPCR                                            
      BI(1)=-SIGN*TEMPCI                                                
      IF(NB.EQ.1) GO TO 52                                              
      DO 51 N=2,NB                                                      
      TEMPBR=(TEMPAR*X-TEMPAI*Y)/DBLE(FLOAT(2*N-2))                     
      TEMPAI=(TEMPAR*Y+TEMPAI*X)/DBLE(FLOAT(2*N-2))                     
      TEMPAR=TEMPBR                                                     
      TEMPBR=DBLE(FLOAT(N))                                             
      BR(N)=TEMPAR*(1.D0-SIGN*TEMPCR/TEMPBR)+TEMPAI*TEMPCI/TEMPBR       
   51 BI(N)=TEMPAI*(1.D0-SIGN*TEMPCR/TEMPBR)-TEMPAR*TEMPCI/TEMPBR       
   52 RETURN                                                            
      END                                                               
      REAL FUNCTION SINH(X)                                             
      REAL X                                                            
      REAL T, CT, X2, S, FACT, SUM                                      
      REAL SIGN, ABS, EXP                                               
C    THIS FUNCTION SUBPROGRAM COMPUTES SINH(X) IN REAL.                 
C    THE GENERAL FORMULA USED HERE IS                                   
C        SINH(X) = (EXP(X)-EXP(-X))/2                                   
C    EXCEPT FOR THE CASE WHEN X IS SMALL.                               
C    IN THE GENERAL CASE, SINCE SINH(X) IS AN ODD FUNCTION, AND FOR     
C    REASONS OF OVERFLOW IN THE MACHINE, IT IS REWRITTEN AS             
C    SINH(X) = SIGN(X)*(EXP(ABS(X/2))*                                  
C      (EXP(ABS(X/2))*0.5))-(0.5/EXP(ABS(X/2)))/EXP(ABS(X/2))           
C    IN THE CASE FOR SMALL X, A SERIES EXPANSION                        
C        SINH(X) = X * (SUM(FROM 0 TO INFINITY) (X**2N)/FACT(2N+1))     
C    IS USED IN PLACE OF THE GENERAL FORMULA.  THIS SERIES EXPANSION    
C    GIVES ACCURATE RESULTS WITH RELATIVELY FEW TERMS.                  
C    INPUT-                                                             
C      X - THE VALUE WHOSE SINH IS DESIRED                              
C    OUTPUT-                                                            
C       SINH = SINH(X)                                                  
      IF (ABS(X) .LT. 0.125E0) GOTO 1                                   
         T = EXP(ABS(X/2.0E0))                                          
         SINH = SIGN(T*(0.5E0*T)-(0.5E0/T)/T, X)                        
         RETURN                                                         
   1  SUM = 1.0E0                                                       
      FACT = 1.0E0                                                      
      CT = 1.0E0                                                        
      X2 = X*X                                                          
      GOTO  3                                                           
   2     CONTINUE                                                       
   3     CT = CT+2.0E0                                                  
         FACT = (FACT*X2)/((CT-1.0E0)*CT)                               
         S = SUM+FACT                                                   
         IF (S .EQ. SUM) GOTO  4                                        
         SUM = S                                                        
         GOTO  2                                                        
   4  SINH = X*SUM                                                      
      RETURN                                                            
      END                                                               
      REAL FUNCTION COSH(X)                                             
      REAL X                                                            
      REAL T, EXP, ABS                                                  
C    THIS FUNCTION SUBPROGRAM COMPUTES COSH(X) IN REAL.                 
C    SINCE COSH(X) IS A WELL BEHAVED FUNCTION ON THE ENTIRE REAL        
C    INTERVAL, THE ONLY FORMULA USED IN THIS COMPUTATION IS-            
C        COSH(X) = (EXP(X)+EXP(-X))/2                                   
C    HOWEVER, IN ORDER TO AVOID OVERFLOW IN THE MACHINE WITH THE ABOVE  
C    REPRESENTATION, COSH(X) IS REWRITTEN AS-                           
C        COSH(X)= (EXP(ABS(X/2))*                                       
C  (EXP(ABS(X/2))*(EXP(ABS(X/2))*0.5))+(0.5/EXP(ABS(X/2)))/EXP(ABS(X/2))
C    INPUT-                                                             
C      X - VALUE WHOSE COSH IS DESIRED                                  
C    OUTPUT-                                                            
C       COSH = COSH(X)                                                  
      T = EXP(ABS(X/2.0E0))                                             
      COSH = T*(0.5E0*T)+(0.5E0/T)/T                                    
      RETURN                                                            
      END                                                               
      REAL FUNCTION TANH(X)                                             
      REAL X                                                            
      REAL T, S, CT, FACT, SUM, X2                                      
      REAL SIGN, ABS, EXP                                               
C    THIS FUNCTION SUBPROGRAM COMPUTES TANH(X) IN REAL.                 
C    THE GENERAL FORMULA USED HERE IS                                   
C        TANH(X) = (EXP(X)-EXP(-X))/(EXP(X)+EXP(-X))                    
C    EXCEPT FOR THE CASE WHEN X IS SMALL.                               
C    IN THE GENERAL CASE, SINCE TANH(X) IS AN ODD FUNCTION, AND FOR     
C    REASONS OF OVERFLOW IN THE MACHINE, IT IS REWRITTEN AS             
C        TANH(X) = SIGN(X) * (1-(EXP(-ABS(X))**2)/(1+(EXP(-ABS(X))**2)  
C    IN CASE FOR SMALL X, THE SERIES EXPANSION                          
C    TANH(X) =                                                          
C      2X*(SUM(FROM 0 TO INF)(X**2N)/FACT(2N+1))  / (EXP(X)+EXP(-X))    
C    IS USED IN PLACE OF THE GENERAL FORMULA.  THIS SERIES EXPANSION    
C    GIVES ACCURATE RESULTS WITH RELATIVELY FEW TERMS.                  
C    INPUT-                                                             
C      X - THE VALUE WHOSE TANH IS DESIRED                              
C    OUTPUT-                                                            
C       TANH = TANH(X)                                                  
      IF (ABS(X) .LT. 0.125E0) GOTO 1                                   
         T = EXP(-ABS(X))**2                                            
         TANH = SIGN((1.0E0-T)/(T+1.0E0), X)                            
         RETURN                                                         
   1  SUM = 1.0E0                                                       
      FACT = 1.0E0                                                      
      CT = 1.0E0                                                        
      X2 = X*X                                                          
      GOTO  3                                                           
   2     CONTINUE                                                       
   3     CT = CT+2.0E0                                                  
         FACT = (FACT*X2)/((CT-1.0E0)*CT)                               
         S = SUM+FACT                                                   
         IF (S .EQ. SUM) GOTO  4                                        
         SUM = S                                                        
         GOTO  2                                                        
   4  T = EXP(X)                                                        
      TANH = (2.0E0*X*SUM)/(T+1.0E0/T)                                  
      RETURN                                                            
      END                                                               
      REAL FUNCTION ACOSH(X)                                            
      REAL X                                                            
      REAL T, SQRT, ALOG                                                
C    THIS FUNCTION SUBPROGRAM COMPUTES ARCCOSH(X) IN REAL.              
C    THE FORMULA USED IN THIS COMPUTATION IS -                          
C      ARCCOSH(X) = LN(X+ SQRT(X**2 -1))                                
C    THE FUNCTION ONLY EXISTS FOR X GREATER THAN OR EQUAL TO 1.         
C    IN ORDER TO AVOID OVERFLOW IN THE MACHINE WITH THE ABOVE           
C    REPRESENTATION, IT IS REWRITTEN AS -                               
C      ARCCOSH(X) = ALOG(X) + ALOG(1 + SQRT((1+1/X)*(1-1/X)))           
C    INPUT-                                                             
C      X - VALUE WHOSE ARCCOSH IS DESIRED                               
C    OUTPUT-                                                            
C       ACOSH = ARCCOSH(X)                                              
C    ERROR STATES-                                                      
C      1 - X .LT. 1                                                     
C/6S                                                                    
C     IF (X .LT. 1.0E0) CALL SETERR(                                    
C    1   35H ACOSH - NO SOLUTION FOR X.LT.1.0E0, 35, 1, 2)              
C/7S                                                                    
      IF (X .LT. 1.0E0) CALL SETERR(                                    
     1   ' ACOSH - NO SOLUTION FOR X.LT.1.0E0', 35, 1, 2)               
C/                                                                      
      T = SQRT((1.0E0/X+1.0E0)*(1.0E0-1.0E0/X))                         
      ACOSH = ALOG(X)+ALOG(T+1.0E0)                                     
      RETURN                                                            
      END                                                               
      REAL FUNCTION ASINH(X)                                            
      REAL X                                                            
      REAL T, X2, SUM, S, CT, FACT                                      
      REAL ABS, SQRT, ALOG                                              
C    THIS FUNCTION SUBPROGRAM COMPUTES ARCSINH(X) IN REAL.              
C    THE GENERAL FORMULA USED HERE IS -                                 
C      ARCSINH(X) = LN(X + SQRT(X**2 +1))                               
C    EXCEPT FOR THE CASE WHEN X IS SMALL.                               
C    TO AVOID OVERFLOW IN THE MACHINE, ARCSINH(X) IS REWRITTEN AS -     
C      ARCSINH(X) = SIGN(X) * LN(ABS(X) + ABS(X)*SQRT(1+(1/X)**2))      
C    IN CASE OF SMALL X, THE SERIES EXPANSION                           
C    ARCSINH(X) =                                                       
C      X * (SUM(FROM 0 TO INF)(1*3...(2N-1)*X**2N)/(2*4...2N(2N+1)))    
C    IS USED IN PLACE OF THE GENERAL FORMULA.                           
C    INPUT-                                                             
C      X - THE VALUE WHOSE ARCSINH IS DESIRED                           
C    OUTPUT-                                                            
C       ASINH = ARCSINH(X)                                              
      IF (X .NE. 0.0E0) GOTO 1                                          
         ASINH = 0.0E0                                                  
         RETURN                                                         
   1  IF (ABS(X) .LE. 0.25E0) GOTO 2                                    
         T = ABS(X)*SQRT((1.0E0/X)**2+1.0E0)                            
         ASINH = ALOG(ABS(X)+T)*(X/ABS(X))                              
         RETURN                                                         
   2  SUM = 1.0E0                                                       
      CT = 0.0E0                                                        
      FACT = 1.0E0                                                      
      X2 = X*X                                                          
      GOTO  4                                                           
   3     CONTINUE                                                       
   4     CT = CT+2.0E0                                                  
         FACT = (-(FACT*X2*(CT-1.0E0)))/CT                              
         S = SUM+FACT/(CT+1.0E0)                                        
         IF (S .EQ. SUM) GOTO  5                                        
         SUM = S                                                        
         GOTO  3                                                        
   5  ASINH = X*SUM                                                     
      RETURN                                                            
      END                                                               
      REAL FUNCTION ATANH(X)                                            
      REAL X                                                            
      REAL SUM, CT, S, FACT, X2, ABS                                    
      REAL ALOG                                                         
C    THIS FUNCTION SUBPROGRAM COMPUTES ARCTANH(X) IN REAL.              
C    THE GENERAL FORMULA USED HERE IS -                                 
C      ARCTANH(X) = 0.5 * LN((1+X)/(1-X))                               
C    EXCEPT FOR THE CASE WHEN X IS SMALL.                               
C    IN CASE OF SMALL X, THE SERIES EXPANSION                           
C      ARCTANH(X) = X * (SUM(FROM 0 TO INF) (X**2N)/(2N+1))             
C    IS USED IN PLACE OF THE GENERAL FORMULA.                           
C    INPUT-                                                             
C      X - THE VALUE WHOSE ARCTANH IS DESIRED                           
C    OUTPUT-                                                            
C       ATANH = ARCTANH(X)                                              
C    ERROR STATES-                                                      
C      1 - ABS(X) .GE. 1                                                
C/6S                                                                    
C     IF (ABS(X) .GE. 1.0E0) CALL SETERR(                               
C    1   41H ATANH - NO SOLUTION FOR  ABS(X).GE.1.0E0, 41, 1, 2)        
C/7S                                                                    
      IF (ABS(X) .GE. 1.0E0) CALL SETERR(                               
     1   ' ATANH - NO SOLUTION FOR  ABS(X).GE.1.0E0', 41, 1, 2)         
C/                                                                      
      IF (ABS(X) .LE. 0.25E0) GOTO 1                                    
         ATANH = 0.5E0*ALOG((X+1.0E0)/(1.0E0-X))                        
         RETURN                                                         
   1  SUM = 1.0E0                                                       
      CT = 1.0E0                                                        
      X2 = X*X                                                          
      FACT = 1.0E0                                                      
      GOTO  3                                                           
   2     CONTINUE                                                       
   3     FACT = FACT*X2                                                 
         CT = CT+2.0E0                                                  
         S = SUM+FACT/CT                                                
         IF (S .EQ. SUM) GOTO  4                                        
         SUM = S                                                        
         GOTO  2                                                        
   4  ATANH = X*SUM                                                     
      RETURN                                                            
      END                                                               
      REAL FUNCTION ARSIN(X)                                            
      REAL X                                                            
      REAL T, ATAN2, ABS, SQRT                                          
C    THIS FUNCTION SUBPROGRAM COMPUTES ARCSIN(X) MAKING USE OF THE      
C    BUILT-IN FUNCTION ATAN2(A1,A2), WHICH COMPUTES THE ARCTAN(A1/A2).  
C    THE RANGE OF ARCSIN(X) IS BETWEEN (-PI/2) AND (PI/2).              
C    THE FORMULA USED IS  ARCSIN(X) = ARCTAN(X/SQRT(1-X**2)).           
C    NOTE THAT ARCSIN(X) LOSES HALF ITS MEANINGFUL DIGITS WHEN X        
C    APPROACHES -1 OR +1.                                               
C    ERROR STATES-                                                      
C      1 - IF ABS(X) .GT. 1                                             
C    INPUT-                                                             
C      X - THE VALUE WHOSE ARCSIN IS DESIRED                            
C    OUTPUT-                                                            
C       ARSIN = ARCSIN(X)                                               
C    CHECK FOR ERROR STATES, AND PRINT OUT ERROR MESSAGE                
C/6S                                                                    
C     IF (ABS(X) .GT. 1.0E0) CALL SETERR(                               
C    1   41H ARSIN - NO SOLUTION FOR  ABS(X).GT.1.0E0, 41, 1, 2)        
C/7S                                                                    
      IF (ABS(X) .GT. 1.0E0) CALL SETERR(                               
     1   ' ARSIN - NO SOLUTION FOR  ABS(X).GT.1.0E0', 41, 1, 2)         
C/                                                                      
C    TO COMPUTE ARCSIN(X)                                               
      T = SQRT(1.0E0-X*X)                                               
      ARSIN = ATAN2(X, T)                                               
      RETURN                                                            
      END                                                               
      REAL FUNCTION ARCOS(X)                                            
      REAL X                                                            
      REAL T, ATAN2, ABS, SQRT                                          
C    THIS FUNCTION SUBPROGRAM COMPUTES ARCCOS(X) MAKING USE OF THE      
C    BUILT-IN FUNCTION ATAN2(A1,A2), WHICH COMPUTES THE ARCTAN(A1/A2).  
C    THE RANGE OF ARCCOS(X) IS BETWEEN 0 AND PI.                        
C    THE FORMULA USED IS  ARCCOS(X) = ARCTAN(SQRT(1-X**2)/X)            
C    NOTE THAT ARCCOS(X) LOSES HALF ITS MEANINGFUL DIGITS WHEN X        
C    APPROACHES -1 OR +1.                                               
C    ERROR STATES-                                                      
C      1 - IF ABS(X) .GT. 1                                             
C    INPUT-                                                             
C      X - THE VALUE WHOSE ARCCOS IS DESIRED                            
C    OUTPUT-                                                            
C       ARCOS=ARCCOS(X)                                                 
C    CHECK FOR ERROR STATES, AND PRINT OUT ERROR MESSAGE                
C/6S                                                                    
C     IF (ABS(X) .GT. 1.0E0) CALL SETERR(                               
C    1   41H ARCOS - NO SOLUTION FOR  ABS(X).GT.1.0E0, 41, 1, 2)        
C/7S                                                                    
      IF (ABS(X) .GT. 1.0E0) CALL SETERR(                               
     1   ' ARCOS - NO SOLUTION FOR  ABS(X).GT.1.0E0', 41, 1, 2)         
C/                                                                      
C    TO COMPUTE ARCCOS(X)                                               
      T = SQRT(1.0E0-X*X)                                               
      ARCOS = ATAN2(T, X)                                               
      RETURN                                                            
      END                                                               
      REAL FUNCTION TAN(X)                                              
      REAL X                                                            
      REAL SIN, COS                                                     
C    THIS FUNCTION SUBPROGRAM COMPUTES TAN(X) MAKING USE OF THE TWO     
C    BUILT-IN SINE AND COSINE FUNCTIONS.  THE FORMULA USED IS           
C        TAN(X)  =  SIN(X) / COS(X)                                     
      TAN = SIN(X)/COS(X)                                               
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DSINH(X)                                
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION T, CT, X2, S, FACT, SUM                          
      DOUBLE PRECISION DEXP                                             
C    THIS FUNCTION SUBPROGRAM COMPUTES SINH(X) IN DOUBLE PRECISION.     
C    THE GENERAL FORMULA USED HERE IS                                   
C        SINH(X) = (EXP(X)-EXP(-X))/2                                   
C    EXCEPT FOR THE CASE WHEN X IS SMALL.                               
C    IN THE GENERAL CASE, SINCE SINH(X) IS AN ODD FUNCTION, AND FOR     
C    REASONS OF OVERFLOW IN THE MACHINE, IT IS REWRITTEN AS             
C    SINH(X)  =  SIGN(X)*(EXP(ABS(X/2))*(EXP(ABS(X/2))*0.5))-           
C                (0.5/EXP(ABS(X/2)))/EXP(ABS(X/2))                      
C    IN THE CASE FOR SMALL X, A SERIES EXPANSION                        
C        SINH(X) = X * (SUM(FROM 0 TO INFINITY) (X**2N)/FACT(2N+1))     
C    IS USED IN PLACE OF THE GENERAL FORMULA.  THIS SERIES EXPANSION    
C    GIVES ACCURATE RESULTS WITH RELATIVELY FEW TERMS.                  
C    INPUT-                                                             
C      X - THE VALUE WHOSE SINH IS DESIRED                              
C    OUTPUT-                                                            
C      DSINH = SINH(X)                                                  
      IF (DABS(X) .LT. 0.125D0) GOTO 1                                  
         T = DEXP(DABS(X/2.0D0))                                        
         DSINH = DSIGN(T*(0.5D0*T)-(0.5D0/T)/T, X)                      
         RETURN                                                         
   1  SUM = 1.0D0                                                       
      FACT = 1.0D0                                                      
      CT = 1.0D0                                                        
      X2 = X*X                                                          
      GOTO  3                                                           
   2     CONTINUE                                                       
   3     CT = CT+2.0D0                                                  
         FACT = (FACT*X2)/((CT-1.0D0)*CT)                               
         S = SUM+FACT                                                   
         IF (S .EQ. SUM) GOTO  4                                        
         SUM = S                                                        
         GOTO  2                                                        
   4  DSINH = X*SUM                                                     
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DCOSH(X)                                
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION T, DEXP                                          
C    THIS FUNCTION SUBPROGRAM COMPUTES COSH(X) IN DOUBLE PRECISION.     
C    SINCE COSH(X) IS A WELL BEHAVED FUNCTION ON THE ENTIRE REAL        
C    INTERVAL, THE ONLY FORMULA USED IN THIS COMPUTATION IS-            
C        COSH(X) = (EXP(X)+EXP(-X))/2                                   
C    HOWEVER, IN ORDER TO AVOID OVERFLOW IN THE MACHINE WITH THE ABOVE  
C    REPRESENTATION, COSH(X) IS REWRITTEN AS-                           
C        COSH(X)= (EXP(ABS(X/2))*                                       
C           (EXP(ABS(X/2))*0.5))+(0.5/EXP(ABS(X/2)))/EXP(ABS(X/2)       
C    INPUT-                                                             
C      X - VALUE WHOSE COSH IS DESIRED                                  
C    OUTPUT-                                                            
C      DCOSH = COSH(X)                                                  
      T = DEXP(DABS(X/2.0D0))                                           
      DCOSH = T*(0.5D0*T)+(0.5D0/T)/T                                   
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DTANH(X)                                
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION T, S, CT, FACT, SUM, X2                          
      DOUBLE PRECISION DEXP                                             
C    THIS FUNCTION SUBPROGRAM COMPUTES TANH(X) IN DOUBLE PRECISION.     
C    THE GENERAL FORMULA USED HERE IS                                   
C        TANH(X) = (EXP(X)-EXP(-X))/(EXP(X)+EXP(-X))                    
C    EXCEPT FOR THE CASE WHEN X IS SMALL.                               
C    IN THE GENERAL CASE, SINCE TANH(X) IS AN ODD FUNCTION, AND FOR     
C    REASONS OF OVERFLOW IN THE MACHINE, IT IS REWRITTEN AS             
C        TANH(X) = SIGN(X) * (1-(EXP(-ABS(X))**2)/(1+(EXP(-ABS(X))**2)  
C    IN CASE FOR SMALL X, THE SERIES EXPANSION                          
C    TANH(X) =                                                          
C      2X*(SUM(FROM 0 TO INF)(X**2N)/FACT(2N+1))  / (EXP(X)+EXP(-X))    
C    IS USED IN PLACE OF THE GENERAL FORMULA.  THIS SERIES EXPANSION    
C    GIVES ACCURATE RESULTS WITH RELATIVELY FEW TERMS.                  
C    INPUT-                                                             
C      X - THE VALUE WHOSE TANH IS DESIRED                              
C    OUTPUT-                                                            
C      DTANH = TANH(X)                                                  
      IF (DABS(X) .LT. 0.125D0) GOTO 1                                  
         T = DEXP(-DABS(X))**2                                          
         DTANH = DSIGN((1.0D0-T)/(T+1.0D0), X)                          
         RETURN                                                         
   1  SUM = 1.0D0                                                       
      FACT = 1.0D0                                                      
      CT = 1.0D0                                                        
      X2 = X*X                                                          
      GOTO  3                                                           
   2     CONTINUE                                                       
   3     CT = CT+2.0D0                                                  
         FACT = (FACT*X2)/((CT-1.0D0)*CT)                               
         S = SUM+FACT                                                   
         IF (S .EQ. SUM) GOTO  4                                        
         SUM = S                                                        
         GOTO  2                                                        
   4  T = DEXP(X)                                                       
      DTANH = (2.0D0*X*SUM)/(T+1.0D0/T)                                 
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DACOSH(X)                               
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION T, DSQRT, DLOG                                   
C    THIS FUNCTION SUBPROGRAM COMPUTES ARCCOSH(X) IN DOUBLE PRECISION.  
C    THE FORMULA USED IN THIS COMPUTATION IS -                          
C      ARCCOSH(X) = LN(X+ SQRT(X**2 -1))                                
C    THE FUNCTION ONLY EXISTS FOR X GREATER THAN OR EQUAL TO 1.         
C    IN ORDER TO AVOID OVERFLOW IN THE MACHINE WITH THE ABOVE           
C    REPRESENTATION, IT IS REWRITTEN AS -                               
C      ARCCOSH(X) = DLOG(X) + DLOG(1 + SQRT((1+1/X)*(1-1/X)))           
C    INPUT-                                                             
C      X - VALUE WHOSE ARCCOSH IS DESIRED                               
C    OUTPUT-                                                            
C      DACOSH = ARCCOSH(X)                                              
C    ERROR STATES-                                                      
C      1 - X .LT. 1                                                     
C/6S                                                                    
C     IF (X .LT. 1.0D0) CALL SETERR(                                    
C    1   35HDACOSH - NO SOLUTION FOR X.LT.1.0D0, 35, 1, 2)              
C/7S                                                                    
      IF (X .LT. 1.0D0) CALL SETERR(                                    
     1   'DACOSH - NO SOLUTION FOR X.LT.1.0D0', 35, 1, 2)               
C/                                                                      
      T = DSQRT((1.0D0/X+1.0D0)*(1.0D0-1.0D0/X))                        
      DACOSH = DLOG(X)+DLOG(T+1.0D0)                                    
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DASINH(X)                               
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION T, X2, SUM, S, CT, FACT                          
      DOUBLE PRECISION DSQRT, DLOG                                      
C    THIS FUNCTION SUBPROGRAM COMPUTES ARCSINH(X) IN DOUBLE PRECISION.  
C    THE GENERAL FORMULA USED HERE IS -                                 
C      ARCSINH(X) = LN(X + SQRT(X**2 +1))                               
C    EXCEPT FOR THE CASE WHEN X IS SMALL.                               
C    TO AVOID OVERFLOW IN THE MACHINE, ARCSINH(X) IS REWRITTEN AS -     
C      ARCSINH(X) = SIGN(X) * LN(ABS(X) + ABS(X)*SQRT(1+(1/X)**2))      
C    IN CASE OF SMALL X, THE SERIES EXPANSION                           
C    ARCSINH(X) =                                                       
C      X * (SUM(FROM 0 TO INF)(1*3...(2N-1)*X**2N)/(2*4...2N(2N+1)))    
C    IS USED IN PLACE OF THE GENERAL FORMULA.                           
C    INPUT-                                                             
C      X - THE VALUE WHOSE ARCSINH IS DESIRED                           
C    OUTPUT-                                                            
C      DASINH = ARCSINH(X)                                              
      IF (X .NE. 0.0D0) GOTO 1                                          
         DASINH = 0.0D0                                                 
         RETURN                                                         
   1  IF (DABS(X) .LE. 0.25D0) GOTO 2                                   
         T = DABS(X)*DSQRT((1.0D0/X)**2+1.0D0)                          
         DASINH = DLOG(DABS(X)+T)*(X/DABS(X))                           
         RETURN                                                         
   2  SUM = 1.0D0                                                       
      CT = 0.0D0                                                        
      FACT = 1.0D0                                                      
      X2 = X*X                                                          
      GOTO  4                                                           
   3     CONTINUE                                                       
   4     CT = CT+2.0D0                                                  
         FACT = (-(FACT*X2*(CT-1.0D0)))/CT                              
         S = SUM+FACT/(CT+1.0D0)                                        
         IF (S .EQ. SUM) GOTO  5                                        
         SUM = S                                                        
         GOTO  3                                                        
   5  DASINH = X*SUM                                                    
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DATANH(X)                               
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION SUM, CT, S, FACT, X2                             
      DOUBLE PRECISION DLOG                                             
C    THIS FUNCTION SUBPROGRAM COMPUTES ARCTANH(X) IN DOUBLE PRECISION.  
C    THE GENERAL FORMULA USED HERE IS -                                 
C      ARCTANH(X) = 0.5 * LN((1+X)/(1-X))                               
C    EXCEPT FOR THE CASE WHEN X IS SMALL.                               
C    IN CASE OF SMALL X, THE SERIES EXPANSION                           
C      ARCTANH(X) = X * (SUM(FROM 0 TO INF) (X**2N)/(2N+1))             
C    IS USED IN PLACE OF THE GENERAL FORMULA.                           
C    INPUT-                                                             
C      X - THE VALUE WHOSE ARCTANH IS DESIRED                           
C    OUTPUT-                                                            
C      DATANH = ARCTANH(X)                                              
C    ERROR STATES-                                                      
C      1 - ABS(X) .GE. 1                                                
C/6S                                                                    
C     IF (DABS(X) .GE. 1.0D0) CALL SETERR(                              
C    1   41HDATANH - NO SOLUTION FOR DABS(X).GE.1.0D0, 41, 1, 2)        
C/7S                                                                    
      IF (DABS(X) .GE. 1.0D0) CALL SETERR(                              
     1   'DATANH - NO SOLUTION FOR DABS(X).GE.1.0D0', 41, 1, 2)         
C/                                                                      
      IF (DABS(X) .LE. 0.25D0) GOTO 1                                   
         DATANH = 0.5D0*DLOG((X+1.0D0)/(1.0D0-X))                       
         RETURN                                                         
   1  SUM = 1.0D0                                                       
      CT = 1.0D0                                                        
      X2 = X*X                                                          
      FACT = 1.0D0                                                      
      GOTO  3                                                           
   2     CONTINUE                                                       
   3     FACT = FACT*X2                                                 
         CT = CT+2.0D0                                                  
         S = SUM+FACT/CT                                                
         IF (S .EQ. SUM) GOTO  4                                        
         SUM = S                                                        
         GOTO  2                                                        
   4  DATANH = X*SUM                                                    
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DARSIN(X)                               
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION T, DATAN2, DSQRT                                 
C    THIS FUNCTION SUBPROGRAM COMPUTES ARCSIN(X) MAKING USE OF THE      
C    BUILT-IN FUNCTION DATAN2(A1,A2), WHICH COMPUTES THE ARCTAN(A1/A2). 
C    THE RANGE OF ARCSIN(X) IS BETWEEN (-PI/2) AND (PI/2).              
C    THE FORMULA USED IS  ARCSIN(X) = ARCTAN(X/SQRT(1-X**2)).           
C    NOTE THAT ARCSIN(X) LOSES HALF ITS MEANINGFUL DIGITS WHEN X        
C    APPROACHES -1 OR +1.                                               
C    ERROR STATES-                                                      
C      1 - IF ABS(X) .GT. 1                                             
C    INPUT-                                                             
C      X - THE VALUE WHOSE ARCSIN IS DESIRED                            
C    OUTPUT-                                                            
C      DARSIN = ARCSIN(X)                                               
C    CHECK FOR ERROR STATES, AND PRINT OUT ERROR MESSAGE                
C/6S                                                                    
C     IF (DABS(X) .GT. 1.0D0) CALL SETERR(                              
C    1   41HDARSIN - NO SOLUTION FOR DABS(X).GT.1.0D0, 41, 1, 2)        
C/7S                                                                    
      IF (DABS(X) .GT. 1.0D0) CALL SETERR(                              
     1   'DARSIN - NO SOLUTION FOR DABS(X).GT.1.0D0', 41, 1, 2)         
C/                                                                      
C    TO COMPUTE ARCSIN(X)                                               
      T = DSQRT(1.0D0-X*X)                                              
      DARSIN = DATAN2(X, T)                                             
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DARCOS(X)                               
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION T, DATAN2, DSQRT                                 
C    THIS FUNCTION SUBPROGRAM COMPUTES ARCCOS(X) MAKING USE OF THE      
C    BUILT-IN FUNCTION DATAN2(A1,A2), WHICH COMPUTES THE ARCTAN(A1/A2). 
C    THE RANGE OF ARCCOS(X) IS BETWEEN 0 AND PI.                        
C    THE FORMULA USED IS  ARCCOS(X) = ARCTAN(SQRT(1-X**2)/X)            
C    NOTE THAT ARCCOS(X) LOSES HALF ITS MEANINGFUL DIGITS WHEN X        
C    APPROACHES -1 OR +1.                                               
C    ERROR STATES-                                                      
C      1 - IF ABS(X) .GT. 1                                             
C    INPUT-                                                             
C      X - THE VALUE WHOSE ARCCOS IS DESIRED                            
C    OUTPUT-                                                            
C      DARCOS=ARCCOS(X)                                                 
C    CHECK FOR ERROR STATES, AND PRINT OUT ERROR MESSAGE                
C/6S                                                                    
C     IF (DABS(X) .GT. 1.0D0) CALL SETERR(                              
C    1   41HDARCOS - NO SOLUTION FOR DABS(X).GT.1.0D0, 41, 1, 2)        
C/7S                                                                    
      IF (DABS(X) .GT. 1.0D0) CALL SETERR(                              
     1   'DARCOS - NO SOLUTION FOR DABS(X).GT.1.0D0', 41, 1, 2)         
C/                                                                      
C    TO COMPUTE ARCCOS(X)                                               
      T = DSQRT(1.0D0-X*X)                                              
      DARCOS = DATAN2(T, X)                                             
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DTAN(X)                                 
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION DSIN, DCOS                                       
C    THIS FUNCTION SUBPROGRAM COMPUTES TAN(X) MAKING USE OF THE TWO     
C    BUILT-IN SINE AND COSINE FUNCTIONS.  THE FORMULA USED IS           
C        TAN(X)  =  SIN(X) / COS(X)                                     
      DTAN = DSIN(X)/DCOS(X)                                            
      RETURN                                                            
      END                                                               
      SUBROUTINE CDEXP(X,EXP)                                           
      DOUBLE PRECISION X(2),EXP(2),R,DEXP,DCOS,DSIN                     
C                                                                       
C COMPLEX DOUBLE PRECISION EXPONENTIAL                                  
C                                                                       
      R = DEXP(X(1))                                                    
      EXP(1) = R*DCOS(X(2))                                             
      EXP(2) = R*DSIN(X(2))                                             
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE CDLOG(X,LOG)                                           
      DOUBLE PRECISION X(2),LOG(2),R,DLOG,DSQRT, DATAN2                 
C                                                                       
C COMPLEX DOUBLE PRECISION LOGARITHM                                    
C                                                                       
      IF (DABS(X(1)) .GT. DABS(X(2))) GO TO 10                          
      R = X(1)/X(2)                                                     
      LOG(1) = DLOG(DABS ( X(2) )*DSQRT( 1.D0 + R*R ) )                 
      GO TO 20                                                          
C                                                                       
 10   R = X(2)/X(1)                                                     
      LOG(1) = DLOG(DABS ( X(1) )*DSQRT( 1.D0 + R*R ) )                 
 20   LOG(2) = DATAN2( X(2),X(1) )                                      
C                                                                       
      RETURN                                                            
      END                                                               
C****END OF ROUTINES NEEDED FOR PORT 3 SPECIAL FUNCTIONS CHAPTER********
