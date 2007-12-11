         SUBROUTINE MFTCI(N,IFX,T)                                      
C                                                                       
C   INITIALIZATION ROUTINE FOR MFTCC (MULTIPLE FOURIER TRANSFORM,       
C   COMPLEX TO COMPLEX DATA). REAL ARRAY T() GETS N COMPLEX             
C   EXPONENTIAL FACTORS                                                 
C                                                                       
C                T(1,J) = COS(2*PI*(J-1)/N)                             
C                T(2,J) = SIN(2*PI*(J-1)/N)                             
C                                                                       
C   FOR J = 1,N. THE INTEGER ARRAY IFX() GETS A FACTORIZATION           
C   OF TRANSFORM DIMENSION N = 2**P*3**Q*5**RR*... INTO FACTORS OF      
C   2,3,5, AND GENERAL PRIME FACTORS (GREATER THAN 5). THE CONTENTS     
C   OF IFX() ON EXIT ARE                                                
C                                                                       
C                IFX(1) = N                                             
C                IFX(2) = M   THE NUMBER OF FACTORS                     
C                IFX(3) THROUGH IFX(M+2) ARE THE FACTORS                
C                                                                       
C   FOR EXAMPLE IF N = 30, THE CONTENTS OF IFX() ARE IFX(1) = 30,       
C   IFX(2) = 3, IFX(3) = 2, IFX(4) = 3, AND IFX(5) = 5.                 
C                                                                       
C   ERROR CONDITIONS                                                    
C                                                                       
C       (1) IF N.LE.0, THIS ROUTINE SIMPLY RETURNS, BUT GENERATES A     
C           RECOVERABLE ERROR.                                          
C                                                                       
         REAL T(2,1)                                                    
         REAL ANG,ARG,PI2                                               
         REAL C36,S36,C72,S72,S60                                       
         INTEGER IFX(25)                                                
         INTEGER I,N,N2                                                 
C                                                                       
         COMMON /M55FT/C36,S36,C72,S72,S60                              
C                                                                       
C  INPUT PARAMETER CHECKS                                               
C                                                                       
C/6S                                                                    
C        IF(N.LE.0)CALL SETERR(18H  MFTCI - N .LE. 0,18,1,1)            
C/7S                                                                    
         IF(N.LE.0)CALL SETERR('  MFTCI - N .LE. 0',18,1,1)             
C/                                                                      
C                                                                       
C  COMPUTE COMMON FACTORS                                               
C                                                                       
         PI2  = 8.0E0*ATAN(1.0E0)                                       
         ANG  = 0.1E0*PI2                                               
         C36  = COS(ANG)                                                
         S36  = SQRT(1.0E0 - C36*C36)                                   
         ANG  = 0.2E0*PI2                                               
         C72  = COS(ANG)                                                
         S72  = SQRT(1.0E0 - C72*C72)                                   
         ANG  = PI2/6.0E0                                               
         S60  = SIN(ANG)                                                
C                                                                       
C   FACTOR N                                                            
C                                                                       
         CALL M66FT(N,IFX)                                              
C                                                                       
C   COMPUTE EXPONENTIAL FACTORS                                         
C                                                                       
         ANG  = PI2/FLOAT(N)                                            
         DO 1 I = 1,N                                                   
            ARG  = ANG*FLOAT(I-1)                                       
            T(1,I) = COS(ARG)                                           
            T(2,I) = SIN(ARG)                                           
 1       CONTINUE                                                       
         RETURN                                                         
         END                                                            
         SUBROUTINE MFTRI(N,IFX,T)                                      
C                                                                       
C   INITIALIZATION ROUTINE FOR MFTRC (MULTIPLE FOURIER TRANSFORM,       
C   REAL TO COMPLEX DATA), AND MFTCR (MULTIPLE FOURIER TRANSFORM,       
C   COMPLEX TO REAL DATA). REAL ARRAY T() GETS N COMPLEX                
C   EXPONENTIAL FACTORS                                                 
C                                                                       
C                T(1,J) = COS(2*PI*(J-1)/N)                             
C                T(2,J) = SIN(2*PI*(J-1)/N)                             
C                                                                       
C   FOR J = 1,N. THE INTEGER ARRAY IFX() GETS A FACTORIZATION           
C   OF TRANSFORM DIMENSION N/2 = 2**P*3**Q*5**RR*... INTO FACTORS OF    
C   2,3,5, AND GENERAL PRIME FACTORS (GREATER THAN 5). THE CONTENTS     
C   OF IFX() ON EXIT ARE                                                
C                                                                       
C                IFX(1) = N/2                                           
C                IFX(2) = M   THE NUMBER OF FACTORS                     
C                IFX(3) THROUGH IFX(M+2) ARE THE FACTORS OF N/2         
C                                                                       
C   FOR EXAMPLE IF N = 60, THE CONTENTS OF IFX() ARE IFX(1) = 30,       
C   IFX(2) = 3, IFX(3) = 2, IFX(4) = 3, AND IFX(5) = 5.                 
C                                                                       
C   ERROR CONDITIONS                                                    
C                                                                       
C       (1) IF N.LE.0, THIS ROUTINE SIMPLY RETURNS, BUT GENERATES A     
C           RECOVERABLE ERROR.                                          
C       (2) IF N.GE.1, BUT IS NOT EVEN, A FATAL ERROR IS GENERATED.     
C           THIS CONDITION IS IMPOSED BECAUSE OF THE COOLEY-TUKEY-      
C           LEWIS-WELCH ALGORITHM FOR REAL-COMPLEX FFT WHICH COMPUTES   
C           AN N/2 DIMENSIONAL COMPLEX TRANSFORM PLUS ANOTHER STEP.     
C                                                                       
         REAL T(2,1)                                                    
         REAL ANG,ARG,PI2                                               
         REAL C36,S36,C72,S72,S60                                       
         INTEGER IFX(25)                                                
         INTEGER I,N,N2                                                 
C                                                                       
         COMMON /M55FT/C36,S36,C72,S72,S60                              
C                                                                       
C   INPUT PARAMETER CHECKS                                              
C                                                                       
C/6S                                                                    
C        IF(N.LE.0)CALL SETERR(18H  MFTRI - N .LE. 0,18,1,1)            
C        IF((2*(N/2)).NE.N)                                             
C    1   CALL SETERR(24H  MFTRI - N MUST BE EVEN,24,2,2)                
C/7S                                                                    
         IF(N.LE.0)CALL SETERR('  MFTRI - N .LE. 0',18,1,1)             
         IF((2*(N/2)).NE.N)                                             
     1   CALL SETERR('  MFTRI - N MUST BE EVEN',24,2,2)                 
C/                                                                      
C                                                                       
C  COMPUTE COMMON FACTORS                                               
C                                                                       
         PI2  = 8.0E0*ATAN(1.0E0)                                       
         ANG  = 0.1E0*PI2                                               
         C36  = COS(ANG)                                                
         S36  = SQRT(1.0E0 - C36*C36)                                   
         ANG  = 0.2E0*PI2                                               
         C72  = COS(ANG)                                                
         S72  = SQRT(1.0E0 - C72*C72)                                   
         ANG  = PI2/6.0E0                                               
         S60  = SIN(ANG)                                                
C                                                                       
C   FACTOR N                                                            
C                                                                       
         N2 = N/2                                                       
         CALL M66FT(N2,IFX)                                             
C                                                                       
C   COMPUTE EXPONENTIAL FACTORS                                         
C                                                                       
         ANG  = PI2/FLOAT(N)                                            
         DO 1 I = 1,N                                                   
            ARG  = ANG*FLOAT(I-1)                                       
            T(1,I) = COS(ARG)                                           
            T(2,I) = SIN(ARG)                                           
 1       CONTINUE                                                       
         RETURN                                                         
         END                                                            
      SUBROUTINE MFTCC(N,NNS,A,B,IAB,JAB,C,D,ICD,JCD,IFX,T,SGN)         
C                                                                       
C   THIS SUBROUTINE COMPUTES MULTIPLE DISCRETE FOURIER TRANSFORMS OF    
C   NNS (DOUBLE) COMPLEX INPUT VECTORS (A + I*B) OF LENGTH N. THE OUTPUT
C   IS ALSO A SET OF COMPLEX VECTORS (C + I*D). THAT IS, IF X(J,K)      
C   ARE A SET OF COMPLEX INPUT VECTORS (K COUNTS VECTORS, K = 1,NNS,    
C   WHOSE ELEMENTS ARE X(J,K), J = 1,N) WITH REAL AND IMAGINARY PARTS   
C   A(J,K) AND B(J,K) RESPECTIVELY, (X(J,K) = A(J,K) + I*B(J,K)),       
C   THE OUTPUT FROM MFTCC IS GIVEN FOR L = 1,N BY                       
C                                                                       
C        Y(L,K) =   SUM  EXP(+-2*PI*I*(J-1)*(L-1)/N)*X(J,K)             
C                  J=1,N                                                
C                                                                       
C   WHERE Y(L,K) = C(L,K) + I*D(L,K) AND K = 1,NNS, AND THE SIGN OF THE 
C   OF THE EXPONENTIAL IS THAT OF PARAMETER SGN. THAT IS, MFTCC         
C   COMPUTES NNS (K = 1,NNS) REAL DISCRETE FOURIER TRANSFORMS OF        
C   LENGTH N (L = 1,N) SIMULTANEOUSLY.                                  
C                                                                       
C INPUT PARAMETERS -                                                    
C                                                                       
C   N     - THE NUMBER OF ELEMENTS IN EACH VECTOR WHOSE FOURIER         
C           TRANSFORM IS DESIRED. THIS PARAMETER MUST BE GREATER        
C           THAN 1.E0                                                   
C                                                                       
C   NNS   - THE NUMBER OF VECTORS WHOSE FOURIER TRANSFORM IS            
C           DESIRED - NNS MUST BE GREATER THAN 0.E0                     
C                                                                       
C   A     - A REAL ARRAY CONTAINING THE REAL PARTS OF THE               
C           INPUT VECTORS. THE SPACING BETWEEN EACH ELEMENT OF EVERY    
C           VECTOR IS IAB, WHILE THE SPACING BETWEEN VECTORS IS         
C           JAB. USUALLY, THE COLUMNS OF A WILL BE DISTINCT INPUT       
C           VECTORS - ALTHOUGH INTERCHANGING THE IAB AND JAB            
C           PARAMETERS WILL AFFECT A TRANSPOSITION.                     
C                                                                       
C   B     - A REAL ARRAY CONTAINING THE IMAGINARY PARTS OF              
C           THE INPUT VECTORS. THE SPACING BETWEEN EACH ELEMENT OF EVERY
C           VECTOR IS IAB, WHILE THE SPACING BETWEEN VECTORS IS         
C           JAB. SEE THE DISCUSSION OF ARRAY A ABOVE                    
C                                                                       
C   IAB   - THE SPACING BETWEEN ELEMENTS WITHIN EACH INPUT VECTOR OF    
C           REAL PARTS (COLUMNS OF A), AND THE SPACING BETWEEN THE      
C           ELEMENTS WITHIN EACH VECTOR OF IMAGINARY PARTS (COLUMNS     
C           OF B).                                                      
C                                                                       
C   JAB   - THE SPACING BETWEEN INPUT VECTORS OF REAL PARTS (SPACING    
C           BETWEEN ROW ELEMENTS OF A), AND THE SPACING BETWEEN         
C           INPUT VECTORS OF IMAGINARY PARTS (SPACING BETWEEN ROW       
C           ELEMENTS OF B).                                             
C                                                                       
C   C     - A REAL ARRAY CONTAINING THE REAL PARTS OF THE               
C           OUTPUT VECTORS. THE SPACING BETWEEN EACH ELEMENT OF EVERY   
C           VECTOR IS ICD, WHILE THE SPACING BETWEEN VECTORS IS         
C           JCD. SEE THE DISCUSSION ABOVE FOR THE INPUT ARRAY A.        
C                                                                       
C   D     - A REAL ARRAY CONTAINING THE IMAGINARY PARTS OF              
C           THE OUTPUT VECTORS. THE SPACING BETWEEN EACH ELEMENT OF     
C           EVERY VECTOR IS ICD, WHILE THE SPACING BETWEEN VECTORS IS   
C           JCD. SEE THE DISCUSSION ABOVE FOR THE INPUT ARRAY A.        
C                                                                       
C   ICD   - THE SPACING BETWEEN ELEMENTS WITHIN EACH OUTPUT VECTOR OF   
C           REAL PARTS (COLUMNS OF C), AND THE SPACING BETWEEN THE      
C           ELEMENTS WITHIN EACH VECTOR OF IMAGINARY PARTS (COLUMNS     
C           OF D).                                                      
C                                                                       
C   JCD   - THE SPACING BETWEEN OUTPUT VECTORS OF REAL PARTS (SPACING   
C           BETWEEN ROW ELEMENTS OF C), AND THE SPACING BETWEEN         
C           OUTPUT VECTORS OF IMAGINARY PARTS (SPACING BETWEEN ROW      
C           ELEMENTS OF D).                                             
C                                                                       
C   IFX   - AN INTEGER ARRAY OF LENGTH 25 IN WHICH THE FACTORS OF       
C           THE INTEGER PARAMETER N WERE SAVED BY THE INITIALIZATION    
C           SUBROUTINE MFTCI. THE ELEMENTS OF IFX ARE                   
C                      IFX(1) = N                                       
C                      IFX(2) = M, THE NUMBER OF FACTORS OF N           
C                      IFX(3) THROUGH IFX(M+2) ARE THE FACTORS OF N     
C                                                                       
C   T     - A REAL ARRAY OF LENGTH N (N EVEN) OR N+2 (N ODD)            
C           WHICH CONTAINS THE TRIGONOMETRIC TABLES COMPUTED BY THE     
C           INITIALIZATION SUBROUTINE MFTCI. THE ELEMENTS OF T ARE      
C                      T(2*(K-1)+1) = COS(2*PI*(K-1)/N)                 
C                      T(2*(K-1)+2) = SIN(2*PI*(K-1)/N)                 
C           WHERE K = 1,N.                                              
C                                                                       
C    SGN  - A REAL PARAMETER WHICH MUST BE EITHER +1.0E0                
C           OR -1.0E0.  THIS PARAMETER DETERMINES THE SIGN OF THE       
C           ARGUMENT IN THE EXPONENTIAL DEFINING THE DISCRETE FOURIER   
C           TRANSFORM.  (SEE ABOVE EXPRESSION FOR Y(L,K)) IF SGN IS     
C           NEITHER 1.0E0 NOR -1.0E0 A FATAL ERROR RESULTS.             
C                                                                       
C   WRITTEN BY - W. P. PETERSEN, BELL LABORATORIES,                     
C               MURRAY HILL, N. J. , 1 SEPT. 1983E0                     
C                                                                       
C                                                                       
      COMMON /CSTAK/DSTAK(500)                                          
      DOUBLE PRECISION DSTAK                                            
C                                                                       
      EXTERNAL ISTKGT                                                   
      INTEGER ISTKGT                                                    
      REAL A(1),B(1),C(1),D(1)                                          
      REAL T(2,1),W(1)                                                  
      REAL SGN                                                          
C/6S                                                                    
C     INTEGER IFX(1)                                                    
C/7S                                                                    
      INTEGER IFX(*)                                                    
C/                                                                      
      INTEGER I,IAB,ICD,IFAC,IW,IWN,JAB,JCD,LA,N,N2,NF2,NFX,NNS,NWK     
      LOGICAL ITGLE                                                     
      EQUIVALENCE (W(1),DSTAK(1))                                       
C                                                                       
C   INPUT PARAMETER CHECKS                                              
C                                                                       
C/6S                                                                    
C     IF (N .LT. 0 .OR. NNS .LT. 0)                                     
C    1   CALL SETERR(33H MFTCC - N .LT. 0 .OR. NNS .LT. 0,33,3,2)       
C/7S                                                                    
      IF (N .LT. 0 .OR. NNS .LT. 0)                                     
     1   CALL SETERR(' MFTCC - N .LT. 0 .OR. NNS .LT. 0',33,3,2)        
C/                                                                      
      IF (N .EQ. 0) GO TO 999                                           
      IF (NNS .EQ. 0) GO TO 999                                         
C/6S                                                                    
C     IF(N.NE.IFX(1))                                                   
C    1   CALL SETERR(36H MFTCC - IFX INITIALIZED INCORRECTLY,36,1,2)    
C     IF( ABS(SGN).NE.1.0E0)                                            
C    1   CALL SETERR(38H MFTCC - SIGN PARAMETER .NE. 1. OR -1.,38,2,2)  
C/7S                                                                    
      IF(N.NE.IFX(1))                                                   
     1   CALL SETERR(' MFTCC - IFX INITIALIZED INCORRECTLY',36,1,2)     
      IF( ABS(SGN).NE.1.0E0)                                            
     1   CALL SETERR(' MFTCC - SIGN PARAMETER .NE. 1. OR -1.',38,2,2)   
C/                                                                      
C                                                                       
      IF (N .GT. 1) GO TO 10                                            
         CALL M88FT(N, NNS, A, B, IAB, JAB, C, D, ICD, JCD)             
         GO TO 999                                                      
 10   IFAC = IFX(3)                                                     
      LA   = 1                                                          
      NFX  = IFX(2)                                                     
      NWK  = 2*N*NNS                                                    
      N2   = 2*N                                                        
C                                                                       
C   GET WORK SPACE                                                      
C                                                                       
      IW = ISTKGT(NWK,3)                                                
      IWN = IW + N                                                      
C                                                                       
C   FIRST PASS THROUGH DATA -                                           
C      IF NFX (THE NUMBER OF FACTORS) IS ODD, COPY INTO WORK SPACE TO   
C      ALLOW INPUT AND OUTPUT TO COINCIDE.  LOGICAL VARIABLE ITGLE IS   
C      SET BY NFX BEING EVEN/ODD.                                       
C                                                                       
      NF2  = NFX/2                                                      
      IF(NFX .NE. NF2+NF2) GO TO 20                                     
C                                                                       
C        *** NFX IS EVEN ***                                            
C                                                                       
         CALL M1FT(A,B,W(IW),W(IWN),T,1,                                
     1             IAB,1,JAB,N2,NNS,N,IFAC,LA,SGN)                      
         ITGLE = .TRUE.                                                 
         GO TO 30                                                       
C                                                                       
 20   CALL M88FT(N,NNS,A,B,IAB,JAB,W(IW),W(IWN),1,N2)                   
      CALL M1FT(W(IW),W(IWN),C,D,T,1,1,ICD,N2,JCD,NNS,N,IFAC,LA,SGN)    
      IF (NFX .EQ. 1) GO TO 60                                          
      ITGLE = .FALSE.                                                   
C                                                                       
C   *** MAIN LOOP ***                                                   
C                                                                       
 30   CONTINUE                                                          
      LA = LA*IFAC                                                      
C                                                                       
      DO 50 I = 2,NFX                                                   
         IFAC = IFX(I+2)                                                
         IF(ITGLE) GO TO 40                                             
            CALL M1FT(C,D,W(IW),W(IWN),T,1,                             
     1                ICD,1,JCD,N2,NNS,N,IFAC,LA,SGN)                   
            LA = LA*IFAC                                                
            ITGLE = .TRUE.                                              
            GO TO 50                                                    
 40      CONTINUE                                                       
            CALL M1FT(W(IW),W(IWN),C,D,T,1,                             
     1                1,ICD,N2,JCD,NNS,N,IFAC,LA,SGN)                   
            LA = LA*IFAC                                                
            ITGLE = .FALSE.                                             
C           ENDIF                                                       
 50   CONTINUE                                                          
C                                                                       
 60   CALL ISTKRL(1)                                                    
C                                                                       
 999  RETURN                                                            
      END                                                               
         SUBROUTINE MFTRC(N,NNS,A,IA,JA,C,D,ICD,JCD,IFX,T,SGN)          
C                                                                       
C   SUBROUTINE MFTRC(N,NNS,A,IA,JA,C,D,ICD,JCD,IFX,T,SGN)               
C                                                                       
C   THIS SUBROUTINE COMPUTES MULTIPLE DISCRETE FOURIER TRANSFORMS       
C   OF NNS REAL INPUT VECTORS (A) OF LENGTH N. THE OUTPUT               
C   IS A SET OF (DOUBLE) COMPLEX VECTORS (C + I*D). THAT IS, IF A(J,K)  
C   ARE A SET OF REAL INPUT VECTORS (K COUNTS VECTORS, K = 1,NNS,       
C   WHOSE ELEMENTS ARE A(J,K), J = 1,N) THEN THE OUTPUT FROM MFTRC IS   
C                                                                       
C        Y(L,K) =   SUM  EXP(+-2*PI*I*(J-1)*(L-1)/N)*A(J,K)             
C                  J=1,N                                                
C                                                                       
C   WHERE Y(L,K) = C(L,K) + I*D(L,K) AND K = 1,NNS, AND THE SIGN OF THE 
C   OF THE EXPONENTIAL IS THAT OF PARAMETER  SGN. THAT IS, MFTRC        
C   COMPUTES NNS (K = 1,NNS) COMPLEX DISCRETE FOURIER TRANSFORMS OF     
C   LENGTH N (L = 1,N) SIMULTANEOUSLY.                                  
C                                                                       
C   IT IS IMPORTANT TO NOTE THAT SINCE THE INPUT VECTORS ARE REAL,      
C   THE OUTPUT Y HAS THE FOLLOWING PROPERTY                             
C                                                                       
C        Y(L,K) = CONJG(Y(N+2-L,K))                                     
C                                                                       
C   FOR L = 1,N. HENCE, ONLY THE FIRST N/2+1 ELEMENTS OF THE COMPLEX    
C   OUTPUT VECTORS (Y = C+I*D) ARE COMPUTED BY MFTRC. FURTHERMORE, FROM 
C   THE ABOVE RELATION, Y(1,K) AND Y(N/2+1,K) HAVE ZERO IMAGINARY PARTS.
C   ON EXIT, D(1,K) = D(N/2+1+1,K) = 0, THAT IS - D(1+(K-1)*JCD) =      
C   D(ICD*(N/2+1)+(K-1)*JCD) = 0, FOR EVERY K = 1,NNS.                  
C                                                                       
C INPUT PARAMETERS -                                                    
C                                                                       
C   N     - THE NUMBER OF ELEMENTS IN EACH VECTOR WHOSE FOURIER         
C           TRANSFORM IS DESIRED. THIS PARAMETER MUST BE GREATER        
C           THAN 1, AND MUST BE EVEN.                                   
C                                                                       
C   NNS   - THE NUMBER OF VECTORS WHOSE FOURIER TRANSFORM IS            
C           DESIRED - NNS MUST BE GREATER THAN 0.                       
C   A     - A REAL ARRAY CONTAINING THE INPUT VECTORS.  THE             
C           SPACING BETWEEN EACH ELEMENT OF EVERY VECTOR IS IA, WHILE   
C           SPACING BETWEEN VECTORS IS JA. USUALLY, THE COLUMNS         
C           OF A WILL BE DISTINCT INPUT VECTORS - ALTHOUGH AN           
C           INTERCHANGE OF IA AND JA WILL AFFECT A TRANSPOSITION.       
C                                                                       
C   IA    - THE SPACING BETWEEN ELEMENTS WITHIN EACH INPUT VECTOR       
C           (COLUMNS OF A).                                             
C                                                                       
C   JA    - THE SPACING BETWEEN INPUT VECTORS OF A (SPACING BETWEEN     
C           ROW ELEMENTS OF A).                                         
C                                                                       
C   C     - A REAL ARRAY CONTAINING THE REAL PARTS OF THE               
C           OUTPUT VECTORS. THE SPACING BETWEEN EACH ELEMENT OF EVERY   
C           VECTOR IS ICD, WHILE THE SPACING BETWEEN VECTORS IS         
C           JCD. SEE THE DISCUSSION ABOVE FOR THE INPUT ARRAY A.        
C                                                                       
C   D     - A REAL ARRAY CONTAINING THE IMAGINARY PARTS OF              
C           THE OUTPUT VECTORS. THE SPACING BETWEEN EACH ELEMENT OF     
C           EVERY VECTOR IS ICD, WHILE THE SPACING BETWEEN VECTORS IS   
C           JCD. SEE THE DISCUSSION ABOVE FOR THE INPUT ARRAY A.        
C                                                                       
C   ICD   - THE SPACING BETWEEN ELEMENTS WITHIN EACH OUTPUT VECTOR OF   
C           REAL PARTS (COLUMNS OF C), AND THE SPACING BETWEEN THE      
C           ELEMENTS WITHIN EACH VECTOR OF IMAGINARY PARTS (COLUMNS     
C           OF D).                                                      
C                                                                       
C   JCD   - THE SPACING BETWEEN OUTPUT VECTORS OF REAL PARTS (SPACING   
C           BETWEEN ROW ELEMENTS OF C), AND THE SPACING BETWEEN         
C           OUTPUT VECTORS OF IMAGINARY PARTS (SPACING BETWEEN ROW      
C           ELEMENTS OF D).                                             
C                                                                       
C   IFX   - AN INTEGER ARRAY OF LENGTH 25 IN WHICH THE FACTORS OF       
C           N/2 WERE SAVED BY THE INITIALIZATION SUBROUTINE MFTRI       
C           THE ELEMENTS OF IFX ARE                                     
C                      IFX(1) = N/2                                     
C                      IFX(2) = M, THE NUMBER OF FACTORS OF N/2         
C                      IFX(3) THROUGH IFX(M+2) ARE THE FACTORS OF N/2   
C                                                                       
C   T     - A REAL ARRAY OF LENGTH N WHICH CONTAINS THE                 
C           TRIGONOMETRIC TABLES COMPUTED BY THE INITIALIZATION         
C           SUBROUTINE MFTRI.  THE ELEMENTS OF T ARE                    
C                      T(2*(K-1)+1) = COS(PI*(K-1)/N)                   
C                      T(2*(K-1)+2) = SIN(PI*(K-1)/N)                   
C           WHERE K = 1,N.                                              
C                                                                       
C   SGN  -  A REAL PARAMETER WHICH MUST BE EITHER +1.0 OR               
C           -1.0.  THIS PARAMETER DETERMINES THE SIGN OF THE ARGUMENT IN
C           THE EXPONENTIAL DEFINING THE DISCRETE FOURIER TRANSFORM.    
C           (SEE ABOVE EXPRESSION FOR Y(L,K)) IF SGN IS                 
C           NEITHER 1.0 NOR -1.0 A FATAL ERROR RESULTS.                 
C                                                                       
      COMMON /CSTAK/DSTAK(500)                                          
      DOUBLE PRECISION DSTAK                                            
C                                                                       
      EXTERNAL ISTKGT                                                   
      INTEGER ISTKGT                                                    
      REAL A(1),C(1),D(1)                                               
      REAL T(2,1),W(1000)                                               
      REAL SGN                                                          
C/6S                                                                    
C     INTEGER IFX(1)                                                    
C/7S                                                                    
      INTEGER IFX(*)                                                    
C/                                                                      
      INTEGER I,I2,IA,IB,ICD,IFAC,IW,IWN,JA,JCD,LA                      
      INTEGER N,N2,NF2,NFX,NNS,NP,NP2,NWK                               
      LOGICAL ITGLE                                                     
      EQUIVALENCE (W(1),DSTAK(1))                                       
C                                                                       
C  INPUT PARAMETER CHECKS                                               
C                                                                       
C/6S                                                                    
C     IF (N .LT. 0 .OR. NNS .LT. 0)                                     
C    1   CALL SETERR(33H MFTRC - N .LT. 0 .OR. NNS .LT. 0,33,3,2)       
C/7S                                                                    
      IF (N .LT. 0 .OR. NNS .LT. 0)                                     
     1   CALL SETERR(' MFTRC - N .LT. 0 .OR. NNS .LT. 0',33,3,2)        
C/                                                                      
      IF (N .EQ. 0) GO TO 999                                           
      IF (NNS .EQ. 0) GO TO 999                                         
C/6S                                                                    
C     IF(N .NE. 2*IFX(1))                                               
C    1   CALL SETERR(36H MFTRC - IFX INITIALIZED INCORRECTLY,36,1,2)    
C     IF( ABS(SGN).NE.1.0E0)                                            
C    1   CALL SETERR(38H MFTRC - SIGN PARAMETER .NE. 1. OR -1.,38,2,2)  
C/7S                                                                    
      IF(N .NE. 2*IFX(1))                                               
     1   CALL SETERR(' MFTRC - IFX INITIALIZED INCORRECTLY',36,1,2)     
      IF( ABS(SGN).NE.1.0E0)                                            
     1   CALL SETERR(' MFTRC - SIGN PARAMETER .NE. 1. OR -1.',38,2,2)   
C/                                                                      
C                                                                       
      I2   = 2*IA                                                       
      IB   = IA + 1                                                     
      LA   = 1                                                          
      N2   = N/2                                                        
      NP2  = N+2                                                        
      NWK  = NP2*NNS                                                    
      NP   = NP2/2                                                      
C                                                                       
C  GET WORK SPACE                                                       
C                                                                       
      IW = ISTKGT(NWK,3)                                                
      IWN = IW+NP                                                       
C                                                                       
C   FIRST PASS THROUGH DATA.  LOGICAL VARIABLE ITGLE IS SET BY          
C      NFX (THE NUMBER OF FACTORS) BEING EVEN/ODD                       
C                                                                       
      NFX  = IFX(2)                                                     
      IF (NFX .GT. 0) GO TO 10                                          
C        *** SPECIAL CASE -- N = 2 ***                                  
         CALL M88FT(N2,NNS,A(1),A(IB),I2,JA,W(IW),W(IWN),1,NP2)         
         GO TO 60                                                       
 10   IFAC = IFX(3)                                                     
      NF2  = NFX/2                                                      
      IF(NFX .EQ. NF2+NF2) GO TO 20                                     
C                                                                       
C        *** ODD NUMBER OF FACTORS ***                                  
C                                                                       
         CALL M1FT(A(1),A(IB),W(IW),W(IWN),T,2,                         
     1                I2,1,JA,NP2,NNS,N2,IFAC,LA,SGN)                   
         ITGLE = .TRUE.                                                 
         IF(NFX .EQ. 1) GO TO 60                                        
         GO TO 30                                                       
C                                                                       
C     *** EVEN NUMBER OF FACTORS ***                                    
C                                                                       
C     *** COPY INTO WORK SPACE TO ALLOW INPUT AND OUTPUT TO COINCIDE ***
C                                                                       
 20   CALL M88FT(N2,NNS,A(1),A(IB),I2,JA, W(IW),W(IWN),1,NP2)           
      CALL M1FT(W(IW),W(IWN),C,D,T,2,1,ICD,NP2,JCD,NNS,N2,IFAC,LA,SGN)  
      ITGLE = .FALSE.                                                   
C                                                                       
C *** MAIN LOOP ***                                                     
C                                                                       
 30   DO 50 I = 2,NFX                                                   
         LA = LA*IFAC                                                   
         IFAC = IFX(I+2)                                                
         IF(ITGLE) GO TO 40                                             
            CALL M1FT(C,D,W(IW),W(IWN),T,2,                             
     1                   ICD,1,JCD,NP2,NNS,N2,IFAC,LA,SGN)              
            ITGLE = .TRUE.                                              
            GO TO 50                                                    
C                                                                       
 40         CALL M1FT(W(IW),W(IWN),C,D,T,2,                             
     1               1,ICD,NP2,JCD,NNS,N2,IFAC,LA,SGN)                  
            ITGLE = .FALSE.                                             
 50   CONTINUE                                                          
C                                                                       
C  **POST PROCESSING IS VIA - COOLEY, TUKEY, LEWIS, AND WELCH           
C                                                                       
 60   CALL M33FT(N,NNS,W(IW),W(IWN),W(IW),W(IWN),1,NP2,                 
     1           C,D,ICD,JCD,T,SGN)                                     
C                                                                       
      CALL ISTKRL(1)                                                    
C                                                                       
 999  RETURN                                                            
      END                                                               
      SUBROUTINE MFTCR(N,NNS,A,B,IAB,JAB,C,IC,JC,IFX,T,SGN)             
C                                                                       
C   SUBROUTINE MFTCR(N,NNS,A,B,IA,JA,C,IC,JC,IFX,T,SGN)                 
C                                                                       
C   THIS SUBROUTINE COMPUTES MULTIPLE DISCRETE FOURIER TRANSFORMS       
C   OF NNS HALF-COMPLEX INPUT VECTORS (A+I*B) OF LENGTH N/2+1. THE      
C   OUTPUT IS A SET OF REAL VECTORS (C). THAT IS, IF X = A + I*B        
C   ARE A SET OF COMPLEX INPUT VECTORS (K COUNTS VECTORS, K = 1,NNS,    
C   WHOSE ELEMENTS ARE X(J,K), J = 1,N) THEN THE OUTPUT FROM MFTCR IS   
C                                                                       
C        C(L,K) =   SUM  EXP(+-2*PI*I*(J-1)*(L-1)/N)*X(J,K)             
C                  J=1,N                                                
C                                                                       
C   WHERE C(L,K) IS PURELY REAL, AND K = 1,NNS -- THE SIGN OF THE       
C   OF THE EXPONENTIAL IS THAT OF PARAMETER  SGN. THAT IS, MFTCR        
C   COMPUTES NNS (K = 1,NNS) COMPLEX DISCRETE FOURIER TRANSFORMS OF     
C   LENGTH N (L = 1,N) SIMULTANEOUSLY.                                  
C                                                                       
C   IT IS IMPORTANT TO NOTE THAT SINCE THE OUTPUT VECTORS ARE REAL, THAT
C   THE INPUT X HAS THE FOLLOWING PROPERTY                              
C                                                                       
C        X(L,K) = CONJG(X(N+2-L,K))                                     
C                                                                       
C   FOR L = 1,N. HENCE, ONLY THE FIRST N/2+1 ELEMENTS OF THE COMPLEX    
C   INPUT VECTORS (X = A+I*B) ARE USED BY MFTCR. FURTHERMORE, FROM      
C   THE ABOVE RELATION, X(1,K) AND X(N/2+1,K) MUST HAVE ZERO IMAGINARY  
C   PARTS. IT IS THE USERS RESPONSIBILITY TO ASSURE B(1,K) = B(N/2+1,K) 
C   ARE ZERO, THAT IS - B(1+(K-1)*JAB) = B(IAB*(N/2+1)+(K-1)*JAB) = 0.  
C                                                                       
C INPUT PARAMETERS -                                                    
C                                                                       
C   N     - THE NUMBER OF ELEMENTS IN EACH VECTOR WHOSE FOURIER         
C           TRANSFORM IS DESIRED. THIS PARAMETER MUST BE GREATER        
C           THAN 1, AND MUST BE EVEN.                                   
C                                                                       
C   NNS   - THE NUMBER OF VECTORS WHOSE FOURIER TRANSFORM IS            
C           DESIRED - NNS MUST BE GREATER THAN 0.                       
C                                                                       
C   A     - A REAL ARRAY CONTAINING THE REAL PARTS OF THE               
C           INPUT VECTORS. THE SPACING BETWEEN ELEMENTS OF EACH VECTOR  
C           IS IAB, WHILE THE SPACING BETWEEN VECTORS IS JAB.           
C           USUALLY, THE COLUMNS OF A WILL BE THE INPUT VECTORS -       
C           ALTHOUGH AN INTERCHANGE OF IAB AND JAB WILL AFFECT A        
C           TRANSPOSITION.                                              
C                                                                       
C   B     - A REAL ARRAY CONTAINING THE IMAGINARY PARTS OF              
C           THE INPUT VECTORS. THE SPACING BETWEEN EACH ELEMENT OF EVERY
C           VECTOR IS IAB, WHILE THE SPACING BETWEEN VECTORS IS         
C           JAB. SEE THE DISCUSSION ABOVE FOR THE INPUT ARRAY A.        
C                                                                       
C   IAB   - THE SPACING BETWEEN ELEMENTS WITHIN EACH INPUT VECTOR OF    
C           REAL PARTS (COLUMNS OF A), AND THE SPACING BETWEEN THE      
C           ELEMENTS WITHIN EACH VECTOR OF IMAGINARY PARTS (COLUMNS     
C           OF B).                                                      
C                                                                       
C   JAB   - THE SPACING BETWEEN INPUT VECTORS OF REAL PARTS (SPACING    
C           BETWEEN ROW ELEMENTS OF A), AND THE SPACING BETWEEN         
C           INPUT VECTORS OF IMAGINARY PARTS (SPACING BETWEEN ROW       
C           ELEMENTS OF B).                                             
C                                                                       
C   C     - A REAL ARRAY CONTAINING THE OUTPUT VECTORS. THE             
C           SPACING BETWEEN EACH ELEMENT OF EVERY VECTOR IS IC, WHILE   
C           THE SPACING BETWEEN VECTORS IS JC.                          
C                                                                       
C   IC    - THE SPACING BETWEEN ELEMENTS WITHIN EACH OUTPUT VECTOR      
C           (COLUMNS OF C).                                             
C                                                                       
C   JC    - THE SPACING BETWEEN OUTPUT VECTORS OF C (SPACING BETWEEN    
C           ROW ELEMENTS OF C).                                         
C                                                                       
C   IFX   - AN INTEGER ARRAY OF LENGTH 25 IN WHICH THE FACTORS OF       
C           N/2 WERE SAVED BY THE INITIALIZATION SUBROUTINE MFTRI       
C           THE ELEMENTS OF IFX ARE                                     
C                      IFX(1) = N/2                                     
C                      IFX(2) = M, THE NUMBER OF FACTORS OF N/2         
C                      IFX(3) THROUGH IFX(M+2) ARE THE FACTORS OF N/2   
C                                                                       
C   T     - A REAL ARRAY OF LENGTH N WHICH CONTAINS THE                 
C           TRIGONOMETRIC TABLES COMPUTED BY THE INITIALIZATION         
C           SUBROUTINE MFTRI.  THE ELEMENTS OF T ARE                    
C                      T(2*(K-1)+1) =  COS(PI*(K-1)/N)                  
C                      T(2*(K-1)+2) = SIN(PI*(K-1)/N)                   
C           WHERE K = 1,N.                                              
C                                                                       
C   SGN  -  A REAL PARAMETER WHICH MUST BE EITHER +1.0 OR               
C           -1.0.  THIS PARAMETER DETERMINES THE SIGN OF THE ARGUMENT IN
C           THE EXPONENTIAL DEFINING THE DISCRETE FOURIER TRANSFORM.    
C           (SEE ABOVE EXPRESSION FOR C(L,K)). IF SGN IS                
C           NEITHER 1.0 NOR -1.0 A FATAL ERROR RESULTS.                 
C                                                                       
      COMMON /CSTAK/DSTAK(500)                                          
      DOUBLE PRECISION DSTAK                                            
C                                                                       
      EXTERNAL ISTKGT                                                   
      INTEGER ISTKGT                                                    
      REAL A(1),B(1),C(1)                                               
      REAL T(2,1),W(1000)                                               
      REAL SGN                                                          
C/6S                                                                    
C     INTEGER IFX(1)                                                    
C/7S                                                                    
      INTEGER IFX(*)                                                    
C/                                                                      
      INTEGER I,I2,IAB,IC,ID,IFAC,IW,IWN,JAB,JC,LA,N                    
      INTEGER N2,NF2,NFX,NNS,NP,NP2,NWK                                 
      LOGICAL EVEN, ITGLE                                               
      EQUIVALENCE (W(1),DSTAK(1))                                       
C                                                                       
C  INPUT PARAMETER CHECKS                                               
C                                                                       
C/6S                                                                    
C     IF (N .LT. 0 .OR. NNS .LT. 0)                                     
C    1   CALL SETERR(33H MFTCR - N .LT. 0 .OR. NNS .LT. 0,33,3,2)       
C/7S                                                                    
      IF (N .LT. 0 .OR. NNS .LT. 0)                                     
     1   CALL SETERR(' MFTCR - N .LT. 0 .OR. NNS .LT. 0',33,3,2)        
C/                                                                      
      IF (N .EQ. 0) GO TO 999                                           
      IF (NNS .EQ. 0) GO TO 999                                         
C/6S                                                                    
C     IF(N .NE. 2*IFX(1))                                               
C    1   CALL SETERR(36H MFTCR - IFX INITIALIZED INCORRECTLY,36,1,2)    
C     IF( ABS(SGN).NE.1.0E0)                                            
C    1   CALL SETERR(38H MFTCR - SIGN PARAMETER .NE. 1. OR -1.,38,2,2)  
C/7S                                                                    
      IF(N .NE. 2*IFX(1))                                               
     1   CALL SETERR(' MFTCR - IFX INITIALIZED INCORRECTLY',36,1,2)     
      IF( ABS(SGN).NE.1.0E0)                                            
     1   CALL SETERR(' MFTCR - SIGN PARAMETER .NE. 1. OR -1.',38,2,2)   
C/                                                                      
C                                                                       
      I2   = 2*IC                                                       
      ID   = IC + 1                                                     
      N2   = N/2                                                        
      NFX  = IFX(2)                                                     
      NF2  = NFX/2                                                      
      EVEN = NFX .EQ. NF2+NF2                                           
      NP2  = N+2                                                        
      NP   = NP2/2                                                      
      NWK  = NP2*NNS                                                    
C                                                                       
C   GET WORK SPACE                                                      
C                                                                       
      IW = ISTKGT(NWK,3)                                                
      IWN = IW+NP                                                       
C                                                                       
C**PRE-PROCESSING IS VIA - COOLEY, TUKEY, LEWIS, AND WELCH              
C                                                                       
      CALL M44FT(N,NNS,A,B,IAB,JAB,W(IW),W(IWN),1,NP2,T,SGN)            
      ITGLE = .TRUE.                                                    
      IFAC = 1                                                          
      LA   = 1                                                          
C                                                                       
C   MAIN LOOP                                                           
C                                                                       
      DO 20 I = 1,NFX                                                   
         LA = LA*IFAC                                                   
         IFAC = IFX(I+2)                                                
         IF(ITGLE) GO TO 10                                             
            CALL M1FT(C(1),C(ID),W(IW),W(IWN),T,2,                      
     1                I2,1,JC,NP2,NNS,N2,IFAC,LA,SGN)                   
            ITGLE = .TRUE.                                              
            GO TO 20                                                    
 10         CALL M1FT(W(IW),W(IWN),C(1),C(ID),T,2,                      
     1                1,I2,NP2,JC,NNS,N2,IFAC,LA,SGN)                   
            ITGLE = .FALSE.                                             
C           ENDIF                                                       
 20      CONTINUE                                                       
C                                                                       
      IF (EVEN) CALL M88FT(N2,NNS,W(IW),W(IWN),1,NP2,C(1),C(ID),I2,JC)  
C                                                                       
      CALL ISTKRL(1)                                                    
C                                                                       
 999  RETURN                                                            
      END                                                               
      SUBROUTINE M1FT(A,B,C,D,T,IT,                                     
     *           IC1,IC2,IC3,IC4,NNS,N,IFAC,LA,SGN)                     
C                                                                       
C  SUBROUTINE M1FT PERFORMS ONE PASS OF RADIX IFAC THROUGH DATA         
C  IN THE COMPUTATION OF A MULTIPLE DISCRETE FOURIER TRANSFORM.         
C  EACH CALLED SUBROUTINE M2FT, M3FT, ETC. ARE RESPONSIBLE FOR          
C  ACTUAL COMPUTATION OF RADIX IFAC = 2, IFAC = 3, ETC. IN THE          
C  CASE THAT IFAC.GE.7, A GENERAL ODD FACTOR (PRIME) SUBROUTINE         
C  M7FT IS CALLED. THIS IS A LOW LEVEL ROUTINE, WITH NO ERROR           
C  CONDITIONS.                                                          
C                                                                       
      REAL A(1),B(1),C(1),D(1),T(1)                                     
      REAL SGN                                                          
      REAL C36,S36,C72,S72,S60                                          
      INTEGER IA,IB,IC,IC1,IC2,IC3,IC4,ID,IE,IFAC,IGO,II,IT,IFACT       
      INTEGER JA,JB,JC,JD,JE,JJ,LA,M,N,NNS                              
C                                                                       
      COMMON /M55FT/C36,S36,C72,S72,S60                                 
C                                                                       
      M   = N/IFAC                                                      
      II  = M*IC1                                                       
      JJ  = LA*IC2                                                      
      IFACT = IFAC+1                                                    
      IF(IFAC.GT.7)IFACT=8                                              
      IGO = IFACT/2                                                     
      GO TO (10,20,30,40),IGO                                           
C                                                                       
C      FACTOR 2                                                         
C                                                                       
 10   IA = 1                                                            
      JA = 1                                                            
      IB = IA+II                                                        
      JB = JA+JJ                                                        
      CALL M2FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,                 
     *          A(IA),B(IA),A(IB),B(IB),                                
     *          C(JA),D(JA),C(JB),D(JB))                                
      GO TO 50                                                          
C                                                                       
C      FACTOR 3                                                         
C                                                                       
 20   IA = 1                                                            
      JA = 1                                                            
      IB = IA+II                                                        
      JB = JA+JJ                                                        
      IC = IB+II                                                        
      JC = JB+JJ                                                        
      CALL M3FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,                 
     *          A(IA),B(IA),A(IB),B(IB),A(IC),B(IC),                    
     *          C(JA),D(JA),C(JB),D(JB),C(JC),D(JC))                    
      GO TO 50                                                          
C                                                                       
C      FACTOR 5                                                         
C                                                                       
 30   IA = 1                                                            
      JA = 1                                                            
      IB = IA+II                                                        
      JB = JA+JJ                                                        
      IC = IB+II                                                        
      JC = JB+JJ                                                        
      ID = IC+II                                                        
      JD = JC+JJ                                                        
      IE = ID+II                                                        
      JE = JD+JJ                                                        
      CALL M5FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,                 
     *    A(IA),B(IA),A(IB),B(IB),A(IC),B(IC),A(ID),B(ID),A(IE),B(IE),  
     *    C(JA),D(JA),C(JB),D(JB),C(JC),D(JC),C(JD),D(JD),C(JE),D(JE))  
      GO TO 50                                                          
C                                                                       
C      GENERAL ODD FACTOR                                               
C                                                                       
 40   CONTINUE                                                          
      CALL M7FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,                 
     *          A(1),B(1),A(1),B(1),A(1),B(1),                          
     *          C(1),D(1),C(1),D(1),C(1),D(1))                          
 50   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE M2FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,           
     *                AIA,BIA,AIB,BIB,CJA,DJA,CJB,DJB)                  
C                                                                       
C  RADIX 2 SUBROUTINE FOR MULTIPLE DISCRETE FOURIER TRANSFORM           
C                                                                       
      REAL AIA(1),BIA(1),AIB(1),BIB(1)                                  
      REAL CJA(1),DJA(1),CJB(1),DJB(1)                                  
      REAL T(1)                                                         
      REAL SGN,C1,S1                                                    
      INTEGER I,IB0,IC1,IC2,IC3,IC4,IFAC,IINK,IJK,IT                    
      INTEGER J,JB0,JINK,JUMP,K,KB,L,LA,LA1,M,N,NNS                     
C                                                                       
      M    = N/IFAC                                                     
      IINK = M*IC1                                                      
      JINK = LA*IC2                                                     
      JUMP = (IFAC-1)*JINK                                              
      IB0  = 1                                                          
      JB0  = 1                                                          
      DO 20 L = 1,LA                                                    
         I = IB0                                                        
         J = JB0                                                        
         DO 10 IJK = 1,NNS                                              
            CJA(J) = AIA(I)+AIB(I)                                      
            DJA(J) = BIA(I)+BIB(I)                                      
            CJB(J) = AIA(I)-AIB(I)                                      
            DJB(J) = BIA(I)-BIB(I)                                      
            I = I+IC3                                                   
            J = J+IC4                                                   
 10      CONTINUE                                                       
         IB0 = IB0+IC1                                                  
         JB0 = JB0+IC2                                                  
 20   CONTINUE                                                          
      IF (LA.EQ.M) RETURN                                               
      LA1 = LA+1                                                        
      JB0 = JB0+JUMP                                                    
      DO 50 K = LA1,M,LA                                                
         KB = (K+K-2)*IT                                                
         C1 = T(KB+1)                                                   
         S1 = SGN*T(KB+2)                                               
         DO 40 L = 1,LA                                                 
            I = IB0                                                     
            J = JB0                                                     
            DO 30 IJK = 1,NNS                                           
               CJA(J) = AIA(I)+AIB(I)                                   
               DJA(J) = BIA(I)+BIB(I)                                   
               CJB(J) = C1*(AIA(I)-AIB(I))-S1*(BIA(I)-BIB(I))           
               DJB(J) = S1*(AIA(I)-AIB(I))+C1*(BIA(I)-BIB(I))           
               I = I+IC3                                                
               J = J+IC4                                                
 30         CONTINUE                                                    
            IB0 = IB0+IC1                                               
            JB0 = JB0+IC2                                               
 40      CONTINUE                                                       
         JB0 = JB0+JUMP                                                 
 50   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE M3FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,           
     *                AIA,BIA,AIB,BIB,AIC,BIC,                          
     *                CJA,DJA,CJB,DJB,CJC,DJC)                          
C                                                                       
C  RADIX 3 SUBROUTINE FOR MULTIPLE DISCRETE FOURIER TRANSFORM           
C                                                                       
      REAL AIA(1),BIA(1),AIB(1),BIB(1),AIC(1),BIC(1)                    
      REAL CJA(1),DJA(1),CJB(1),DJB(1),CJC(1),DJC(1)                    
      REAL T(1)                                                         
      REAL C1,C2,S1,S2,SGN                                              
      REAL C36,S36,C72,S72,S60,SIN60                                    
      INTEGER I,IB0,IC1,IC2,IC3,IC4,IFAC,IINK,IJK,IT                    
      INTEGER J,JB0,JINK,JUMP,K,KB,KC,L,LA,LA1,M,N,NNS                  
C                                                                       
      COMMON /M55FT/C36,S36,C72,S72,S60                                 
C                                                                       
      M     = N/IFAC                                                    
      IINK  = M*IC1                                                     
      JINK  = LA*IC2                                                    
      JUMP  = (IFAC-1)*JINK                                             
      IB0   = 1                                                         
      JB0   = 1                                                         
      SIN60 = S60*SGN                                                   
C                                                                       
      DO 20 L = 1,LA                                                    
         I = IB0                                                        
         J = JB0                                                        
         DO 10 IJK = 1,NNS                                              
            CJA(J) = AIA(I)+(AIB(I)+AIC(I))                             
            DJA(J) = BIA(I)+(BIB(I)+BIC(I))                             
            CJB(J) = (AIA(I)-0.5E0*(AIB(I)+AIC(I)))                     
     *               -(SIN60*(BIB(I)-BIC(I)))                           
            CJC(J) = (AIA(I)-0.5E0*(AIB(I)+AIC(I)))                     
     *               +(SIN60*(BIB(I)-BIC(I)))                           
            DJB(J) = (BIA(I)-0.5E0*(BIB(I)+BIC(I)))                     
     *               +(SIN60*(AIB(I)-AIC(I)))                           
            DJC(J) = (BIA(I)-0.5E0*(BIB(I)+BIC(I)))                     
     *               -(SIN60*(AIB(I)-AIC(I)))                           
            I = I+IC3                                                   
            J = J+IC4                                                   
 10      CONTINUE                                                       
         IB0 = IB0+IC1                                                  
         JB0 = JB0+IC2                                                  
 20   CONTINUE                                                          
      IF (LA.EQ.M) RETURN                                               
      LA1 = LA+1                                                        
      JB0 = JB0+JUMP                                                    
      DO 50 K = LA1,M,LA                                                
         KB = (K+K-2)*IT                                                
         KC = KB+KB                                                     
         C1 = T(KB+1)                                                   
         S1 = SGN*T(KB+2)                                               
         C2 = T(KC+1)                                                   
         S2 = SGN*T(KC+2)                                               
         DO 40 L = 1,LA                                                 
            I = IB0                                                     
            J = JB0                                                     
            DO 30 IJK = 1,NNS                                           
               CJA(J) = AIA(I)+(AIB(I)+AIC(I))                          
               DJA(J) = BIA(I)+(BIB(I)+BIC(I))                          
               CJB(J) =                                                 
     *                  C1*((AIA(I)-0.5E0*(AIB(I)+AIC(I)))              
     *                 -(SIN60*(BIB(I)-BIC(I))))                        
     *                 -S1*((BIA(I)-0.5E0*(BIB(I)+BIC(I)))              
     *                 +(SIN60*(AIB(I)-AIC(I))))                        
               DJB(J) =                                                 
     *                  S1*((AIA(I)-0.5E0*(AIB(I)+AIC(I)))              
     *                 -(SIN60*(BIB(I)-BIC(I))))                        
     *                 +C1*((BIA(I)-0.5E0*(BIB(I)+BIC(I)))              
     *                 +(SIN60*(AIB(I)-AIC(I))))                        
               CJC(J) =                                                 
     *                  C2*((AIA(I)-0.5E0*(AIB(I)+AIC(I)))              
     *                 +(SIN60*(BIB(I)-BIC(I))))                        
     *                 -S2*((BIA(I)-0.5E0*(BIB(I)+BIC(I)))              
     *                 -(SIN60*(AIB(I)-AIC(I))))                        
               DJC(J) =                                                 
     *                  S2*((AIA(I)-0.5E0*(AIB(I)+AIC(I)))              
     *                 +(SIN60*(BIB(I)-BIC(I))))                        
     *                 +C2*((BIA(I)-0.5E0*(BIB(I)+BIC(I)))              
     *                 -(SIN60*(AIB(I)-AIC(I))))                        
               I = I+IC3                                                
               J = J+IC4                                                
 30         CONTINUE                                                    
            IB0 = IB0+IC1                                               
            JB0 = JB0+IC2                                               
 40      CONTINUE                                                       
         JB0 = JB0+JUMP                                                 
 50   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE M5FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,           
     *                AIA,BIA,AIB,BIB,AIC,BIC,AID,BID,AIE,BIE,          
     *                CJA,DJA,CJB,DJB,CJC,DJC,CJD,DJD,CJE,DJE)          
C                                                                       
C   RADIX 5 SUBROUTINE FOR MULTIPLE DISCRETE FOURIER TRANSFORM          
C                                                                       
      REAL AIA(1),BIA(1),AIB(1),BIB(1),AIC(1)                           
      REAL BIC(1),AID(1),BID(1),AIE(1),BIE(1)                           
      REAL CJA(1),DJA(1),CJB(1),DJB(1),CJC(1)                           
      REAL DJC(1),CJD(1),DJD(1),CJE(1),DJE(1)                           
      REAL T(1)                                                         
      REAL SGN                                                          
      REAL C1,C2,C3,C4,COS36,COS72,S1,S2,S3,S4,SIN36,SIN72              
      REAL C36,S36,C72,S72,S60                                          
      INTEGER I,IB0,IC1,IC2,IC3,IC4,IFAC,IINK,IJK,IT                    
      INTEGER J,JB0,JINK,JUMP,K,KB,KC,KD,KE,L,LA,LA1,M,N,NNS            
C                                                                       
      COMMON /M55FT/C36,S36,C72,S72,S60                                 
C                                                                       
      M     = N/IFAC                                                    
      IINK  = M*IC1                                                     
      JINK  = LA*IC2                                                    
      JUMP  = (IFAC-1)*JINK                                             
      IB0   = 1                                                         
      JB0   = 1                                                         
      COS36 = C36                                                       
      SIN36 = SGN*S36                                                   
      COS72 = C72                                                       
      SIN72 = SGN*S72                                                   
C                                                                       
      DO 20 L = 1,LA                                                    
         I = IB0                                                        
         J = JB0                                                        
         DO 10 IJK = 1,NNS                                              
            CJA(J) = AIA(I)+(AIB(I)+AIE(I))+(AIC(I)+AID(I))             
            DJA(J) = BIA(I)+(BIB(I)+BIE(I))+(BIC(I)+BID(I))             
            CJB(J) = (AIA(I)+COS72*(AIB(I)+AIE(I))                      
     *               -COS36*(AIC(I)+AID(I)))                            
     *               -(SIN72*(BIB(I)-BIE(I))                            
     *               +SIN36*(BIC(I)-BID(I)))                            
            CJE(J) = (AIA(I)+COS72*(AIB(I)+AIE(I))                      
     *               -COS36*(AIC(I)+AID(I)))                            
     *               +(SIN72*(BIB(I)-BIE(I))                            
     *               +SIN36*(BIC(I)-BID(I)))                            
            DJB(J) = (BIA(I)+COS72*(BIB(I)+BIE(I))                      
     *               -COS36*(BIC(I)+BID(I)))                            
     *               +(SIN72*(AIB(I)-AIE(I))                            
     *               +SIN36*(AIC(I)-AID(I)))                            
            DJE(J) = (BIA(I)+COS72*(BIB(I)+BIE(I))                      
     *               -COS36*(BIC(I)+BID(I)))                            
     *               -(SIN72*(AIB(I)-AIE(I))                            
     *               +SIN36*(AIC(I)-AID(I)))                            
            CJC(J) = (AIA(I)-COS36*(AIB(I)+AIE(I))                      
     *               +COS72*(AIC(I)+AID(I)))                            
     *               -(SIN36*(BIB(I)-BIE(I))                            
     *               -SIN72*(BIC(I)-BID(I)))                            
            CJD(J) = (AIA(I)-COS36*(AIB(I)+AIE(I))                      
     *               +COS72*(AIC(I)+AID(I)))                            
     *               +(SIN36*(BIB(I)-BIE(I))                            
     *               -SIN72*(BIC(I)-BID(I)))                            
            DJC(J) = (BIA(I)-COS36*(BIB(I)+BIE(I))                      
     *               +COS72*(BIC(I)+BID(I)))                            
     *               +(SIN36*(AIB(I)-AIE(I))                            
     *               -SIN72*(AIC(I)-AID(I)))                            
            DJD(J) = (BIA(I)-COS36*(BIB(I)+BIE(I))                      
     *               +COS72*(BIC(I)+BID(I)))                            
     *               -(SIN36*(AIB(I)-AIE(I))                            
     *               -SIN72*(AIC(I)-AID(I)))                            
            I = I+IC3                                                   
            J = J+IC4                                                   
 10      CONTINUE                                                       
         IB0 = IB0+IC1                                                  
         JB0 = JB0+IC2                                                  
 20   CONTINUE                                                          
      IF (LA.EQ.M) GOTO 60                                              
      LA1 = LA+1                                                        
      JB0 = JB0+JUMP                                                    
      DO 50 K = LA1,M,LA                                                
         KB = (K+K-2)*IT                                                
         KC = KB+KB                                                     
         KD = KC+KB                                                     
         KE = KD+KB                                                     
         C1 = T(KB+1)                                                   
         S1 = SGN*T(KB+2)                                               
         C2 = T(KC+1)                                                   
         S2 = SGN*T(KC+2)                                               
         C3 = T(KD+1)                                                   
         S3 = SGN*T(KD+2)                                               
         C4 = T(KE+1)                                                   
         S4 = SGN*T(KE+2)                                               
         DO 40 L = 1,LA                                                 
            I = IB0                                                     
            J = JB0                                                     
            DO 30 IJK = 1,NNS                                           
               CJA(J) = AIA(I)+(AIB(I)+AIE(I))+(AIC(I)+AID(I))          
               DJA(J) = BIA(I)+(BIB(I)+BIE(I))+(BIC(I)+BID(I))          
               CJB(J) =                                                 
     *             C1*((AIA(I)                                          
     *             +COS72*(AIB(I)+AIE(I))-COS36*(AIC(I)+AID(I)))        
     *             -(SIN72*(BIB(I)-BIE(I))+SIN36*(BIC(I)-BID(I))))      
     *             -S1*((BIA(I)                                         
     *             +COS72*(BIB(I)+BIE(I))-COS36*(BIC(I)+BID(I)))        
     *             +(SIN72*(AIB(I)-AIE(I))+SIN36*(AIC(I)-AID(I))))      
               DJB(J) =                                                 
     *             S1*((AIA(I)                                          
     *             +COS72*(AIB(I)+AIE(I))-COS36*(AIC(I)+AID(I)))        
     *             -(SIN72*(BIB(I)-BIE(I))+SIN36*(BIC(I)-BID(I))))      
     *             +C1*((BIA(I)                                         
     *             +COS72*(BIB(I)+BIE(I))-COS36*(BIC(I)+BID(I)))        
     *             +(SIN72*(AIB(I)-AIE(I))+SIN36*(AIC(I)-AID(I))))      
               CJE(J) =                                                 
     *             C4*((AIA(I)                                          
     *             +COS72*(AIB(I)+AIE(I))-COS36*(AIC(I)+AID(I)))        
     *             +(SIN72*(BIB(I)-BIE(I))+SIN36*(BIC(I)-BID(I))))      
     *             -S4*((BIA(I)                                         
     *             +COS72*(BIB(I)+BIE(I))-COS36*(BIC(I)+BID(I)))        
     *             -(SIN72*(AIB(I)-AIE(I))+SIN36*(AIC(I)-AID(I))))      
               DJE(J) =                                                 
     *             S4*((AIA(I)                                          
     *             +COS72*(AIB(I)+AIE(I))-COS36*(AIC(I)+AID(I)))        
     *             +(SIN72*(BIB(I)-BIE(I))+SIN36*(BIC(I)-BID(I))))      
     *             +C4*((BIA(I)                                         
     *             +COS72*(BIB(I)+BIE(I))-COS36*(BIC(I)+BID(I)))        
     *             -(SIN72*(AIB(I)-AIE(I))+SIN36*(AIC(I)-AID(I))))      
               CJC(J) =                                                 
     *             C2*((AIA(I)                                          
     *             -COS36*(AIB(I)+AIE(I))+COS72*(AIC(I)+AID(I)))        
     *             -(SIN36*(BIB(I)-BIE(I))-SIN72*(BIC(I)-BID(I))))      
     *             -S2*((BIA(I)                                         
     *             -COS36*(BIB(I)+BIE(I))+COS72*(BIC(I)+BID(I)))        
     *             +(SIN36*(AIB(I)-AIE(I))-SIN72*(AIC(I)-AID(I))))      
               DJC(J) =                                                 
     *             S2*((AIA(I)                                          
     *             -COS36*(AIB(I)+AIE(I))+COS72*(AIC(I)+AID(I)))        
     *             -(SIN36*(BIB(I)-BIE(I))-SIN72*(BIC(I)-BID(I))))      
     *             +C2*((BIA(I)                                         
     *             -COS36*(BIB(I)+BIE(I))+COS72*(BIC(I)+BID(I)))        
     *             +(SIN36*(AIB(I)-AIE(I))-SIN72*(AIC(I)-AID(I))))      
               CJD(J) =                                                 
     *             C3*((AIA(I)                                          
     *             -COS36*(AIB(I)+AIE(I))+COS72*(AIC(I)+AID(I)))        
     *             +(SIN36*(BIB(I)-BIE(I))-SIN72*(BIC(I)-BID(I))))      
     *             -S3*((BIA(I)                                         
     *             -COS36*(BIB(I)+BIE(I))+COS72*(BIC(I)+BID(I)))        
     *             -(SIN36*(AIB(I)-AIE(I))-SIN72*(AIC(I)-AID(I))))      
               DJD(J) =                                                 
     *             S3*((AIA(I)                                          
     *             -COS36*(AIB(I)+AIE(I))+COS72*(AIC(I)+AID(I)))        
     *             +(SIN36*(BIB(I)-BIE(I))-SIN72*(BIC(I)-BID(I))))      
     *             +C3*((BIA(I)                                         
     *             -COS36*(BIB(I)+BIE(I))+COS72*(BIC(I)+BID(I)))        
     *             -(SIN36*(AIB(I)-AIE(I))-SIN72*(AIC(I)-AID(I))))      
               I = I+IC3                                                
               J = J+IC4                                                
 30         CONTINUE                                                    
            IB0 = IB0+IC1                                               
            JB0 = JB0+IC2                                               
 40      CONTINUE                                                       
         JB0 = JB0+JUMP                                                 
 50   CONTINUE                                                          
 60   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE M7FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,           
     *                A1,B1,A2,B2,A3,B3,C1,D1,C2,D2,C3,D3)              
C                                                                       
C   GENERAL ODD PRIME FACTOR SUBROUTINE FOR MULTIPLE DISCRETE           
C   FOURIER TRANSFORM.                                                  
C                                                                       
      REAL A1(1),B1(1),A2(1),B2(1),A3(1),B3(1)                          
      REAL C1(1),D1(1),C2(1),D2(1),C3(1),D3(1)                          
      REAL T(1)                                                         
      REAL SGN,T1,T2,TEMPI,TEMPR                                        
      INTEGER IA,IAI,IB,IC1,IC2,IC3,IC4,IFAC,II,IINK,IJK,IM,IP,IPI      
      INTEGER IQ,IQI,IT,IZ,J,JA,JAJ,JINK,JJ,JP,JPJ,JQ,JQJ,JUMP          
      INTEGER K,KINK,KT,KU,KV,KX,L,LA,LINK,M,MINK,N,N2,NN,NNS           
C                                                                       
      M    = N/IFAC                                                     
      IINK = M*IC1                                                      
      JINK = LA*IC2                                                     
      JUMP = (IFAC-1)*JINK                                              
      IA   = 1                                                          
      JA   = 1                                                          
      KT   = M+M                                                        
      LINK = (IFAC-1)*IINK                                              
      MINK = LINK/2                                                     
      N2   = (N+1)/2                                                    
      NN   = N+N                                                        
      DO 140 K = 1,M,LA                                                 
         KINK = K+K-2                                                   
         DO 130 L = 1,LA                                                
            IB = IA+IINK                                                
            IM = IA+MINK                                                
            IZ = IA+LINK                                                
            IQ = IZ                                                     
            IAI = IA                                                    
            JAJ = JA                                                    
            DO 10 IJK = 1,NNS                                           
               C1(JAJ) = A1(IAI)                                        
               D1(JAJ) = B1(IAI)                                        
               IAI = IAI+IC3                                            
               JAJ = JAJ+IC4                                            
 10         CONTINUE                                                    
            DO 30 IP = IB,IM,IINK                                       
               IPI = IP                                                 
               IQI = IQ                                                 
               JAJ = JA                                                 
               DO 20 IJK = 1,NNS                                        
                  TEMPR = A1(IPI)+A1(IQI)                               
                  A2(IQI) = A1(IPI)-A1(IQI)                             
                  A3(IPI) = TEMPR                                       
                  C1(JAJ) = C1(JAJ)+TEMPR                               
                  TEMPI = B1(IPI)+B1(IQI)                               
                  B2(IQI) = B1(IPI)-B1(IQI)                             
                  B3(IPI) = TEMPI                                       
                  D1(JAJ) = D1(JAJ)+TEMPI                               
                  IPI = IPI+IC3                                         
                  IQI = IQI+IC3                                         
                  JAJ = JAJ+IC4                                         
 20            CONTINUE                                                 
               IQ = IQ-IINK                                             
 30         CONTINUE                                                    
            JP = JA+JINK                                                
            JQ = JA+JUMP                                                
            KU = KT                                                     
            DO 90 II = IB,IM,IINK                                       
               IAI = IA                                                 
               JPJ = JP                                                 
               JQJ = JQ                                                 
               DO 40 IJK = 1,NNS                                        
                  C2(JPJ) = A1(IAI)                                     
                  D2(JPJ) = B1(IAI)                                     
                  C3(JQJ) = A1(IAI)                                     
                  D3(JQJ) = B1(IAI)                                     
                  IAI = IAI+IC3                                         
                  JPJ = JPJ+IC4                                         
                  JQJ = JQJ+IC4                                         
 40            CONTINUE                                                 
               KV = KU                                                  
               IQ = IZ                                                  
               DO 80 IP = IB,IM,IINK                                    
                  KX = MOD(KV,NN)                                       
                  IF(KX.LE.N) GO TO 50                                  
                     KX = (NN - KX)*IT                                  
                     T1 = T(KX+1)                                       
                     T2 = -SGN*T(KX+2)                                  
                     GO TO 60                                           
 50               CONTINUE                                              
                     KX = KX*IT                                         
                     T1 = T(KX+1)                                       
                     T2 = SGN*T(KX+2)                                   
 60               CONTINUE                                              
                  IPI = IP                                              
                  IQI = IQ                                              
                  JPJ = JP                                              
                  JQJ = JQ                                              
                  DO 70 IJK = 1,NNS                                     
                     C2(JPJ) = C1(JPJ)+A1(IPI)*T1                       
     *                                -B1(IQI)*T2                       
                     D2(JPJ) = D1(JPJ)+B1(IPI)*T1                       
     *                                +A1(IQI)*T2                       
                     C3(JQJ) = C1(JQJ)+A1(IPI)*T1                       
     *                                +B1(IQI)*T2                       
                     D3(JQJ) = D1(JQJ)+B1(IPI)*T1                       
     *                                -A1(IQI)*T2                       
                     IPI = IPI+IC3                                      
                     IQI = IQI+IC3                                      
                     JPJ = JPJ+IC4                                      
                     JQJ = JQJ+IC4                                      
 70               CONTINUE                                              
                  KV = KV+KU                                            
                  IQ = IQ-IINK                                          
 80            CONTINUE                                                 
               JP = JP+JINK                                             
               JQ = JQ-JINK                                             
               KU = KU+KT                                               
 90         CONTINUE                                                    
            IA = IA+IC1                                                 
            IF(KINK.EQ.0)GO TO 120                                      
            KU = KINK                                                   
            JP = JA+JINK                                                
            JQ = JA+JUMP                                                
            DO 110 JJ = JP,JQ,JINK                                      
               J = JJ                                                   
               KX = KU*IT                                               
               T1 = T(KX+1)                                             
               T2 = SGN*T(KX+2)                                         
               DO 100 IJK = 1,NNS                                       
                  TEMPR = C1(J)*T1-D1(J)*T2                             
                  TEMPI = C1(J)*T2+D1(J)*T1                             
                  C2(J) = TEMPR                                         
                  D2(J) = TEMPI                                         
                  J = J+IC4                                             
 100           CONTINUE                                                 
               KU = KU+KINK                                             
 110        CONTINUE                                                    
 120        JA = JA+IC2                                                 
 130     CONTINUE                                                       
         JA = JA+JUMP                                                   
 140  CONTINUE                                                          
      RETURN                                                            
      END                                                               
         SUBROUTINE M33FT(N,NNS,W1,W2,W3,W4,IW,JW,Y1,Y2,IY,JY,T,SGN)    
C                                                                       
C   THIS SUBROUTINE PERFORMS THE LAST PASS THROUGH DATA FOR A           
C   MULTIPLE DISCRETE FOURIER TRANSFORM, FOR REAL INPUT DATA.           
C   IT RELIES ON THE METHOD OF COOLEY, TUKEY, LEWIS, AND WELCH.         
C                                                                       
         REAL W1(1),W2(1),W3(1),W4(1)                                   
         REAL Y1(1),Y2(1)                                               
         REAL T(2,1)                                                    
         REAL QI,QR,RI,RR,SGN,T1,T2                                     
         INTEGER I,I1,I3,I4,IA,IB,IW,IY,J,JW,JY,N,N2,NNS                
C                                                                       
C**SET LAST ELEMENTS OF W                                               
C                                                                       
         IA  = 1                                                        
         IB  = (N/2)*IW + 1                                             
         DO 10 J = 1,NNS                                                
            W3(IB) = W1(IA)                                             
            W4(IB) = W2(IA)                                             
            IA = IA + JW                                                
            IB = IB + JW                                                
 10      CONTINUE                                                       
C                                                                       
C**COOLEY, TUKEY, LEWIS, AND WELCH                                      
C                                                                       
         N2  = N/2 + 1                                                  
         DO 30 J = 1,N2                                                 
            I1  = (J-1)*IY + 1                                          
            I3  = (J-1)*IW + 1                                          
            I4  = (N2-J)*IW + 1                                         
            T1  = T(1,J)                                                
            T2  = SGN*T(2,J)                                            
            DO 20 I = 1,NNS                                             
               QR     = 0.5E0*(W1(I3) + W1(I4))                         
               QI     = 0.5E0*(W2(I3) - W2(I4))                         
               RR     = 0.5E0*(W2(I3) + W2(I4))                         
               RI     = -0.5E0*(W1(I3) - W1(I4))                        
               Y1(I1) = QR + T1*RR - T2*RI                              
               Y2(I1) = QI + T1*RI + T2*RR                              
               I1     = I1 + JY                                         
               I3     = I3 + JW                                         
               I4     = I4 + JW                                         
 20         CONTINUE                                                    
 30      CONTINUE                                                       
         RETURN                                                         
         END                                                            
         SUBROUTINE M44FT(N,NNS,A,B,IAB,JAB,W1,W2,IW,JW,T,SGN)          
C                                                                       
C   THIS SUBROUTINE PERFORMS A PRE-PROCESSING PASS IN A MULTIPLE        
C   DISCRETE FOURIER TRANSFORM, FOR HALF-COMPLEX INPUT DATA. IT         
C   RELIES ON THE METHOD OF COOLEY, TUKEY, LEWIS, AND WELCH.            
C                                                                       
         REAL A(1),B(1),W1(1),W2(1),T(2,1)                              
         REAL GI,GR,HI,HR,SGN,T1,T2                                     
         INTEGER I,IAB,IAM,IAP,IW,IW1,J,JAB,JW,N,N2,NNS                 
C**COOLEY, TUKEY, LEWIS, AND WELCH                                      
         N2  = N/2+1                                                    
         DO 20 J = 1,N2                                                 
            IAP = (J-1)*IAB + 1                                         
            IAM = (N2-J)*IAB + 1                                        
            IW1 = (J-1)*IW + 1                                          
            T1  = T(1,J)                                                
            T2  = SGN*T(2,J)                                            
            DO 10 I = 1,NNS                                             
               HR      = A(IAP) + A(IAM)                                
               HI      = B(IAP) - B(IAM)                                
               GR      = B(IAP) + B(IAM)                                
               GI      = A(IAM) - A(IAP)                                
               W1(IW1) = HR - T1*GR + T2*GI                             
               W2(IW1) = HI - T2*GR - T1*GI                             
               IAP     = IAP + JAB                                      
               IAM     = IAM + JAB                                      
               IW1     = IW1 + JW                                       
 10         CONTINUE                                                    
 20      CONTINUE                                                       
C                                                                       
         RETURN                                                         
         END                                                            
      SUBROUTINE M66FT(N,IFX)                                           
C                                                                       
C   THIS SUBROUTINE FACTORS AN INTEGER N INTO FACTORS OF 2,3,5 AND      
C   GENERAL PRIME FACTORS (GREATER THAN 5). ON EXIT, THE CONTENTS OF    
C   INTEGER ARRAY IFX() ARE                                             
C                                                                       
C               IFX(1) = N                                              
C               IFX(2) = M THE NUMBER OF FACTORS                        
C               IFX(3) THROUGH IFX(M+2) ARE THE FACTORS                 
C                                                                       
C   FOR EXAMPLE, IF N = 30 - IFX(1) = 30, IFX(2) = 3, IFX(3) = 2,       
C   IFX(4) = 3, AND IFX(5) = 5.                                         
C                                                                       
C   THIS IS A LOW LEVEL ROUTINE WITH NO ERROR CONDITIONS.               
C                                                                       
C/6S                                                                    
C     INTEGER IFX(1)                                                    
C/7S                                                                    
      INTEGER IFX(*)                                                    
C/                                                                      
      INTEGER I,II,IL,IT,K,L,N,NL,NN                                    
C                                                                       
      NN=N                                                              
      IFX(1)=NN                                                         
      IF (NN .LE. 1) GO TO 100                                          
      K=2                                                               
C  FACTORS OF 2                                                         
 10   IF (MOD(NN,2).NE.0) GO TO 20                                      
         K=K+1                                                          
         IFX(K)=2                                                       
         NN=NN/2                                                        
      IF (NN.EQ.1) GO TO 60                                             
         GO TO 10                                                       
C  FACTORS OF 3                                                         
 20   IF (MOD(NN,3).NE.0) GO TO 30                                      
         K=K+1                                                          
         IFX(K)=3                                                       
         NN=NN/3                                                        
      IF (NN.EQ.1) GO TO 60                                             
         GO TO 20                                                       
C  REMAINING ODD FACTORS                                                
 30   L=5                                                               
      I=2                                                               
C  I IS ALTERNATELY 2 OR 4                                              
 40   IF (MOD(NN,L).NE.0) GO TO 50                                      
         K=K+1                                                          
         IFX(K)=L                                                       
         NN=NN/L                                                        
      IF (NN.EQ.1) GO TO 60                                             
         GO TO 40                                                       
 50   L=L+I                                                             
      I=6-I                                                             
         GO TO 40                                                       
 60   IFX(2)=K-2                                                        
C  NFAX IS SET TO THE NUMBER OF FACTORS                                 
      NL=K-2                                                            
C  SORT FACTORS IN ASCENDING ORDER                                      
      IF (NL.EQ.1) GO TO 90                                             
      DO 80 II=2,NL                                                     
         IL=NL+3-II                                                     
         DO 70 I=3,IL                                                   
            IF (IFX(I+1).GE.IFX(I)) GO TO 70                            
            IT=IFX(I)                                                   
            IFX(I)=IFX(I+1)                                             
            IFX(I+1)=IT                                                 
 70      CONTINUE                                                       
 80   CONTINUE                                                          
 90   CONTINUE                                                          
      GO TO 999                                                         
 100  IFX(2) = 0                                                        
 999  RETURN                                                            
      END                                                               
         SUBROUTINE M88FT(N,NNS,A,B,IAB,JAB,C,D,ICD,JCD)                
C                                                                       
C   THIS ROUTINE IS A DOUBLE ARRAY COPY FOR MULTIPLE FOURIER            
C   TRANSFORM PACKAGE - MFT..                                           
C                                                                       
         REAL A(1),B(1),C(1),D(1)                                       
         INTEGER I,IAB,ICD,J,J1,J2,JAB,JCD,N,NNS                        
C                                                                       
         DO 1 J = 1,NNS                                                 
            J1 = (J-1)*JCD+1                                            
            J2 = (J-1)*JAB+1                                            
            DO 2 I = 1,N                                                
               C(J1) = A(J2)                                            
               D(J1) = B(J2)                                            
               J1 = J1 + ICD                                            
               J2 = J2 + IAB                                            
 2          CONTINUE                                                    
 1       CONTINUE                                                       
         RETURN                                                         
         END                                                            
         SUBROUTINE DMFTCI(N,IFX,T)                                     
C                                                                       
C   INITIALIZATION ROUTINE FOR MFTCC (MULTIPLE FOURIER TRANSFORM,       
C   COMPLEX TO COMPLEX DATA). REAL ARRAY T() GETS N COMPLEX             
C   EXPONENTIAL FACTORS                                                 
C                                                                       
C                T(1,J) = DCOS(2*PI*(J-1)/N)                            
C                T(2,J) = SIN(2*PI*(J-1)/N)                             
C                                                                       
C   FOR J = 1,N. THE INTEGER ARRAY IFX() GETS A FACTORIZATION           
C   OF TRANSFORM DIMENSION N = 2**P*3**Q*5**RR*... INTO FACTORS OF      
C   2,3,5, AND GENERAL PRIME FACTORS (GREATER THAN 5). THE CONTENTS     
C   OF IFX() ON EXIT ARE                                                
C                                                                       
C                IFX(1) = N                                             
C                IFX(2) = M   THE NUMBER OF FACTORS                     
C                IFX(3) THROUGH IFX(M+2) ARE THE FACTORS                
C                                                                       
C   FOR EXAMPLE IF N = 30, THE CONTENTS OF IFX() ARE IFX(1) = 30,       
C   IFX(2) = 3, IFX(3) = 2, IFX(4) = 3, AND IFX(5) = 5.                 
C                                                                       
C   ERROR CONDITIONS                                                    
C                                                                       
C       (1) IF N.LE.0, THIS ROUTINE SIMPLY RETURNS, BUT GENERATES A     
C           RECOVERABLE ERROR.                                          
C                                                                       
         DOUBLE PRECISION T(2,1)                                        
         DOUBLE PRECISION ANG,ARG,PI2                                   
         DOUBLE PRECISION C36,S36,C72,S72,S60                           
         DOUBLE PRECISION DATAN,DCOS,DSQRT,DSIN                         
         INTEGER IFX(25)                                                
         INTEGER I,N,N2                                                 
C                                                                       
         COMMON /DM55FT/C36,S36,C72,S72,S60                             
C                                                                       
C  INPUT PARAMETER CHECKS                                               
C                                                                       
C/6S                                                                    
C        IF(N.LE.0)CALL SETERR(19H  DMFTCI - N .LE. 0,18,1,1)           
C/7S                                                                    
         IF(N.LE.0)CALL SETERR('  DMFTCI - N .LE. 0',18,1,1)            
C/                                                                      
C                                                                       
C  COMPUTE COMMON FACTORS                                               
C                                                                       
         PI2  = 8.0D0*DATAN(1.0D0)                                      
         ANG  = 0.1D0*PI2                                               
         C36  = DCOS(ANG)                                               
         S36  = DSQRT(1.0D0 - C36*C36)                                  
         ANG  = 0.2D0*PI2                                               
         C72  = DCOS(ANG)                                               
         S72  = DSQRT(1.0D0 - C72*C72)                                  
         ANG  = PI2/6.0D0                                               
         S60  = DSIN(ANG)                                               
C                                                                       
C   FACTOR N                                                            
C                                                                       
         CALL DM66FT(N,IFX)                                             
C                                                                       
C   COMPUTE EXPONENTIAL FACTORS                                         
C                                                                       
         ANG  = PI2/FLOAT(N)                                            
         DO 1 I = 1,N                                                   
            ARG  = ANG*FLOAT(I-1)                                       
            T(1,I) = DCOS(ARG)                                          
            T(2,I) = DSIN(ARG)                                          
 1       CONTINUE                                                       
         RETURN                                                         
         END                                                            
         SUBROUTINE DMFTRI(N,IFX,T)                                     
C                                                                       
C   INITIALIZATION ROUTINE FOR MFTRC (MULTIPLE FOURIER TRANSFORM,       
C   REAL TO COMPLEX DATA), AND MFTCR (MULTIPLE FOURIER TRANSFORM,       
C   COMPLEX TO REAL DATA). REAL ARRAY T() GETS N COMPLEX                
C   EXPONENTIAL FACTORS                                                 
C                                                                       
C                T(1,J) = DCOS(2*PI*(J-1)/N)                            
C                T(2,J) = SIN(2*PI*(J-1)/N)                             
C                                                                       
C   FOR J = 1,N. THE INTEGER ARRAY IFX() GETS A FACTORIZATION           
C   OF TRANSFORM DIMENSION N/2 = 2**P*3**Q*5**RR*... INTO FACTORS OF    
C   2,3,5, AND GENERAL PRIME FACTORS (GREATER THAN 5). THE CONTENTS     
C   OF IFX() ON EXIT ARE                                                
C                                                                       
C                IFX(1) = N/2                                           
C                IFX(2) = M   THE NUMBER OF FACTORS                     
C                IFX(3) THROUGH IFX(M+2) ARE THE FACTORS OF N/2         
C                                                                       
C   FOR EXAMPLE IF N = 60, THE CONTENTS OF IFX() ARE IFX(1) = 30,       
C   IFX(2) = 3, IFX(3) = 2, IFX(4) = 3, AND IFX(5) = 5.                 
C                                                                       
C   ERROR CONDITIONS                                                    
C                                                                       
C       (1) IF N.LE.0, THIS ROUTINE SIMPLY RETURNS, BUT GENERATES A     
C           RECOVERABLE ERROR.                                          
C       (2) IF N.GE.1, BUT IS NOT EVEN, A FATAL ERROR IS GENERATED.     
C           THIS CONDITION IS IMPOSED BECAUSE OF THE COOLEY-TUKEY-      
C           LEWIS-WELCH ALGORITHM FOR REAL-COMPLEX FFT WHICH COMPUTES   
C           AN N/2 DIMENSIONAL COMPLEX TRANSFORM PLUS ANOTHER STEP.     
C                                                                       
         DOUBLE PRECISION T(2,1)                                        
         DOUBLE PRECISION ANG,ARG,PI2                                   
         DOUBLE PRECISION C36,S36,C72,S72,S60                           
         DOUBLE PRECISION DCOS,DATAN,DSQRT,DSIN                         
         INTEGER IFX(25)                                                
         INTEGER I,N,N2                                                 
C                                                                       
         COMMON /DM55FT/C36,S36,C72,S72,S60                             
C                                                                       
C   INPUT PARAMETER CHECKS                                              
C                                                                       
C/6S                                                                    
C        IF(N.LE.0)CALL SETERR(19H  DMFTRI - N .LE. 0,18,1,1)           
C        IF((2*(N/2)).NE.N)                                             
C    1   CALL SETERR(25H  DMFTRI - N MUST BE EVEN,24,2,2)               
C/7S                                                                    
         IF(N.LE.0)CALL SETERR('  DMFTRI - N .LE. 0',18,1,1)            
         IF((2*(N/2)).NE.N)                                             
     1   CALL SETERR('  DMFTRI - N MUST BE EVEN',24,2,2)                
C/                                                                      
C                                                                       
C  COMPUTE COMMON FACTORS                                               
C                                                                       
         PI2  = 8.0D0*DATAN(1.0D0)                                      
         ANG  = 0.1D0*PI2                                               
         C36  = DCOS(ANG)                                               
         S36  = DSQRT(1.0D0 - C36*C36)                                  
         ANG  = 0.2D0*PI2                                               
         C72  = DCOS(ANG)                                               
         S72  = DSQRT(1.0D0 - C72*C72)                                  
         ANG  = PI2/6.0D0                                               
         S60  = DSIN(ANG)                                               
C                                                                       
C   FACTOR N                                                            
C                                                                       
         N2 = N/2                                                       
         CALL DM66FT(N2,IFX)                                            
C                                                                       
C   COMPUTE EXPONENTIAL FACTORS                                         
C                                                                       
         ANG  = PI2/FLOAT(N)                                            
         DO 1 I = 1,N                                                   
            ARG  = ANG*FLOAT(I-1)                                       
            T(1,I) = DCOS(ARG)                                          
            T(2,I) = DSIN(ARG)                                          
 1       CONTINUE                                                       
         RETURN                                                         
         END                                                            
      SUBROUTINE DMFTCC(N,NNS,A,B,IAB,JAB,C,D,ICD,JCD,IFX,T,SGN)        
C                                                                       
C   THIS SUBROUTINE COMPUTES MULTIPLE DISCRETE FOURIER TRANSFORMS OF    
C   NNS (DOUBLE) COMPLEX INPUT VECTORS (A + I*B) OF LENGTH N. THE OUTPUT
C   IS ALSO A SET OF COMPLEX VECTORS (C + I*D). THAT IS, IF X(J,K)      
C   ARE A SET OF COMPLEX INPUT VECTORS (K COUNTS VECTORS, K = 1,NNS,    
C   WHOSE ELEMENTS ARE X(J,K), J = 1,N) WITH REAL AND IMAGINARY PARTS   
C   A(J,K) AND B(J,K) RESPECTIVELY, (X(J,K) = A(J,K) + I*B(J,K)),       
C   THE OUTPUT FROM DMFTCC IS GIVEN FOR L = 1,N BY                      
C                                                                       
C        Y(L,K) =   SUM DEXP(+-2*PI*I*(J-1)*(L-1)/N)*X(J,K)             
C                  J=1,N                                                
C                                                                       
C   WHERE Y(L,K) = C(L,K) + I*D(L,K) AND K = 1,NNS, AND THE SIGN OF THE 
C   OF THE EXPONENTIAL IS THAT OF PARAMETER SGN. THAT IS, DMFTCC        
C   COMPUTES NNS (K = 1,NNS) REAL DISCRETE FOURIER TRANSFORMS OF        
C   LENGTH N (L = 1,N) SIMULTANEOUSLY.                                  
C                                                                       
C INPUT PARAMETERS -                                                    
C                                                                       
C   N     - THE NUMBER OF ELEMENTS IN EACH VECTOR WHOSE FOURIER         
C           TRANSFORM IS DESIRED. THIS PARAMETER MUST BE GREATER        
C           THAN 1.D0                                                   
C                                                                       
C   NNS   - THE NUMBER OF VECTORS WHOSE FOURIER TRANSFORM IS            
C           DESIRED - NNS MUST BE GREATER THAN 0.D0                     
C                                                                       
C   A     - A DOUBLE PRECISION ARRAY CONTAINING THE REAL PARTS OF THE   
C           INPUT VECTORS. THE SPACING BETWEEN EACH ELEMENT OF EVERY    
C           VECTOR IS IAB, WHILE THE SPACING BETWEEN VECTORS IS         
C           JAB. USUALLY, THE COLUMNS OF A WILL BE DISTINCT INPUT       
C           VECTORS - ALTHOUGH INTERCHANGING THE IAB AND JAB            
C           PARAMETERS WILL AFFECT A TRANSPOSITION.                     
C                                                                       
C   B     - A DOUBLE PRECISION ARRAY CONTAINING THE IMAGINARY PARTS OF  
C           THE INPUT VECTORS. THE SPACING BETWEEN EACH ELEMENT OF EVERY
C           VECTOR IS IAB, WHILE THE SPACING BETWEEN VECTORS IS         
C           JAB. SEE THE DISCUSSION OF ARRAY A ABOVE                    
C                                                                       
C   IAB   - THE SPACING BETWEEN ELEMENTS WITHIN EACH INPUT VECTOR OF    
C           REAL PARTS (COLUMNS OF A), AND THE SPACING BETWEEN THE      
C           ELEMENTS WITHIN EACH VECTOR OF IMAGINARY PARTS (COLUMNS     
C           OF B).                                                      
C                                                                       
C   JAB   - THE SPACING BETWEEN INPUT VECTORS OF REAL PARTS (SPACING    
C           BETWEEN ROW ELEMENTS OF A), AND THE SPACING BETWEEN         
C           INPUT VECTORS OF IMAGINARY PARTS (SPACING BETWEEN ROW       
C           ELEMENTS OF B).                                             
C                                                                       
C   C     - A DOUBLE PRECISION ARRAY CONTAINING THE REAL PARTS OF THE   
C           OUTPUT VECTORS. THE SPACING BETWEEN EACH ELEMENT OF EVERY   
C           VECTOR IS ICD, WHILE THE SPACING BETWEEN VECTORS IS         
C           JCD. SEE THE DISCUSSION ABOVE FOR THE INPUT ARRAY A.        
C                                                                       
C   D     - A DOUBLE PRECISION ARRAY CONTAINING THE IMAGINARY PARTS OF  
C           THE OUTPUT VECTORS. THE SPACING BETWEEN EACH ELEMENT OF     
C           EVERY VECTOR IS ICD, WHILE THE SPACING BETWEEN VECTORS IS   
C           JCD. SEE THE DISCUSSION ABOVE FOR THE INPUT ARRAY A.        
C                                                                       
C   ICD   - THE SPACING BETWEEN ELEMENTS WITHIN EACH OUTPUT VECTOR OF   
C           REAL PARTS (COLUMNS OF C), AND THE SPACING BETWEEN THE      
C           ELEMENTS WITHIN EACH VECTOR OF IMAGINARY PARTS (COLUMNS     
C           OF D).                                                      
C                                                                       
C   JCD   - THE SPACING BETWEEN OUTPUT VECTORS OF REAL PARTS (SPACING   
C           BETWEEN ROW ELEMENTS OF C), AND THE SPACING BETWEEN         
C           OUTPUT VECTORS OF IMAGINARY PARTS (SPACING BETWEEN ROW      
C           ELEMENTS OF D).                                             
C                                                                       
C   IFX   - AN INTEGER ARRAY OF LENGTH 25 IN WHICH THE FACTORS OF       
C           THE INTEGER PARAMETER N WERE SAVED BY THE INITIALIZATION    
C           SUBROUTINE DMFTCI. THE ELEMENTS OF IFX ARE                  
C                      IFX(1) = N                                       
C                      IFX(2) = M, THE NUMBER OF FACTORS OF N           
C                      IFX(3) THROUGH IFX(M+2) ARE THE FACTORS OF N     
C                                                                       
C   T     - A DOUBLE PRECISION ARRAY OF LENGTH N (N EVEN) OR N+2 (N ODD)
C           WHICH CONTAINS THE TRIGONOMETRIC TABLES COMPUTED BY THE     
C           INITIALIZATION SUBROUTINE DMFTCI. THE ELEMENTS OF T ARE     
C                      T(2*(K-1)+1) = COS(2*PI*(K-1)/N)                 
C                      T(2*(K-1)+2) = SIN(2*PI*(K-1)/N)                 
C           WHERE K = 1,N.                                              
C                                                                       
C    SGN  - A DOUBLE PRECISION PARAMETER WHICH MUST BE EITHER +1.0D0    
C           OR -1.0D0.  THIS PARAMETER DETERMINES THE SIGN OF THE       
C           ARGUMENT IN THE EXPONENTIAL DEFINING THE DISCRETE FOURIER   
C           TRANSFORM.  (SEE ABOVE EXPRESSION FOR Y(L,K)) IF SGN IS     
C           NEITHER 1.0D0 NOR -1.0D0 A FATAL ERROR RESULTS.             
C                                                                       
C   WRITTEN BY - W. P. PETERSEN, BELL LABORATORIES,                     
C               MURRAY HILL, N. J. , 1 SEPT. 1983D0                     
C                                                                       
C                                                                       
      COMMON /CSTAK/DSTAK(500)                                          
      DOUBLE PRECISION DSTAK                                            
C                                                                       
      EXTERNAL ISTKGT                                                   
      INTEGER ISTKGT                                                    
      DOUBLE PRECISION A(1),B(1),C(1),D(1)                              
      DOUBLE PRECISION T(2,1),W(1)                                      
      DOUBLE PRECISION SGN                                              
C/6S                                                                    
C     INTEGER IFX(1)                                                    
C/7S                                                                    
      INTEGER IFX(*)                                                    
C/                                                                      
      INTEGER I,IAB,ICD,IFAC,IW,IWN,JAB,JCD,LA,N,N2,NF2,NFX,NNS,NWK     
      LOGICAL ITGLE                                                     
      EQUIVALENCE (W(1),DSTAK(1))                                       
C                                                                       
C   INPUT PARAMETER CHECKS                                              
C                                                                       
C/6S                                                                    
C     IF (N .LT. 0 .OR. NNS .LT. 0)                                     
C    1   CALL SETERR(33HDMFTCC - N .LT. 0 .OR. NNS .LT. 0,33,3,2)       
C/7S                                                                    
      IF (N .LT. 0 .OR. NNS .LT. 0)                                     
     1   CALL SETERR('DMFTCC - N .LT. 0 .OR. NNS .LT. 0',33,3,2)        
C/                                                                      
      IF (N .EQ. 0) GO TO 999                                           
      IF (NNS .EQ. 0) GO TO 999                                         
C/6S                                                                    
C     IF(N.NE.IFX(1))                                                   
C    1   CALL SETERR(36HDMFTCC - IFX INITIALIZED INCORRECTLY,36,1,2)    
C     IF(DABS(SGN).NE.1.0D0)                                            
C    1   CALL SETERR(38HDMFTCC - SIGN PARAMETER .NE. 1. OR -1.,38,2,2)  
C/7S                                                                    
      IF(N.NE.IFX(1))                                                   
     1   CALL SETERR('DMFTCC - IFX INITIALIZED INCORRECTLY',36,1,2)     
      IF(DABS(SGN).NE.1.0D0)                                            
     1   CALL SETERR('DMFTCC - SIGN PARAMETER .NE. 1. OR -1.',38,2,2)   
C/                                                                      
C                                                                       
      IF (N .GT. 1) GO TO 10                                            
         CALL DM88FT(N, NNS, A, B, IAB, JAB, C, D, ICD, JCD)            
         GO TO 999                                                      
 10   IFAC = IFX(3)                                                     
      LA   = 1                                                          
      NFX  = IFX(2)                                                     
      NWK  = 2*N*NNS                                                    
      N2   = 2*N                                                        
C                                                                       
C   GET WORK SPACE                                                      
C                                                                       
      IW = ISTKGT(NWK,4)                                                
      IWN = IW + N                                                      
C                                                                       
C   FIRST PASS THROUGH DATA -                                           
C      IF NFX (THE NUMBER OF FACTORS) IS ODD, COPY INTO WORK SPACE TO   
C      ALLOW INPUT AND OUTPUT TO COINCIDE.  LOGICAL VARIABLE ITGLE IS   
C      SET BY NFX BEING EVEN/ODD.                                       
C                                                                       
      NF2  = NFX/2                                                      
      IF(NFX .NE. NF2+NF2) GO TO 20                                     
C                                                                       
C        *** NFX IS EVEN ***                                            
C                                                                       
         CALL DM1FT(A,B,W(IW),W(IWN),T,1,                               
     1             IAB,1,JAB,N2,NNS,N,IFAC,LA,SGN)                      
         ITGLE = .TRUE.                                                 
         GO TO 30                                                       
C                                                                       
 20   CALL DM88FT(N,NNS,A,B,IAB,JAB,W(IW),W(IWN),1,N2)                  
      CALL DM1FT(W(IW),W(IWN),C,D,T,1,1,ICD,N2,JCD,NNS,N,IFAC,LA,SGN)   
      IF (NFX .EQ. 1) GO TO 60                                          
      ITGLE = .FALSE.                                                   
C                                                                       
C   *** MAIN LOOP ***                                                   
C                                                                       
 30   CONTINUE                                                          
      LA = LA*IFAC                                                      
C                                                                       
      DO 50 I = 2,NFX                                                   
         IFAC = IFX(I+2)                                                
         IF(ITGLE) GO TO 40                                             
            CALL DM1FT(C,D,W(IW),W(IWN),T,1,                            
     1                ICD,1,JCD,N2,NNS,N,IFAC,LA,SGN)                   
            LA = LA*IFAC                                                
            ITGLE = .TRUE.                                              
            GO TO 50                                                    
 40      CONTINUE                                                       
            CALL DM1FT(W(IW),W(IWN),C,D,T,1,                            
     1                1,ICD,N2,JCD,NNS,N,IFAC,LA,SGN)                   
            LA = LA*IFAC                                                
            ITGLE = .FALSE.                                             
C           ENDIF                                                       
 50   CONTINUE                                                          
C                                                                       
 60   CALL ISTKRL(1)                                                    
C                                                                       
 999  RETURN                                                            
      END                                                               
         SUBROUTINE DMFTRC(N,NNS,A,IA,JA,C,D,ICD,JCD,IFX,T,SGN)         
C                                                                       
C   SUBROUTINE DMFTRC(N,NNS,A,IA,JA,C,D,ICD,JCD,IFX,T,SGN)              
C                                                                       
C   THIS SUBROUTINE COMPUTES MULTIPLE DISCRETE FOURIER TRANSFORMS       
C   OF NNS DOUBLE PRECISION INPUT VECTORS (A) OF LENGTH N. THE OUTPUT   
C   IS A SET OF (DOUBLE) COMPLEX VECTORS (C + I*D). THAT IS, IF A(J,K)  
C   ARE A SET OF REAL INPUT VECTORS (K COUNTS VECTORS, K = 1,NNS,       
C   WHOSE ELEMENTS ARE A(J,K), J = 1,N) THEN THE OUTPUT FROM DMFTRC IS  
C                                                                       
C        Y(L,K) =   SUM DEXP(+-2*PI*I*(J-1)*(L-1)/N)*A(J,K)             
C                  J=1,N                                                
C                                                                       
C   WHERE Y(L,K) = C(L,K) + I*D(L,K) AND K = 1,NNS, AND THE SIGN OF THE 
C   OF THE EXPONENTIAL IS THAT OF PARAMETER  SGN. THAT IS, DMFTRC       
C   COMPUTES NNS (K = 1,NNS) COMPLEX DISCRETE FOURIER TRANSFORMS OF     
C   LENGTH N (L = 1,N) SIMULTANEOUSLY.                                  
C                                                                       
C   IT IS IMPORTANT TO NOTE THAT SINCE THE INPUT VECTORS ARE REAL,      
C   THE OUTPUT Y HAS THE FOLLOWING PROPERTY                             
C                                                                       
C        Y(L,K) = CONJG(Y(N+2-L,K))                                     
C                                                                       
C   FOR L = 1,N. HENCE, ONLY THE FIRST N/2+1 ELEMENTS OF THE COMPLEX    
C   OUTPUT VECTORS (Y = C+I*D) ARE COMPUTED BY DMFTRC. FURTHERMORE, FROM
C   THE ABOVE RELATION, Y(1,K) AND Y(N/2+1,K) HAVE ZERO IMAGINARY PARTS.
C   ON EXIT, D(1,K) = D(N/2+1+1,K) = 0, THAT IS - D(1+(K-1)*JCD) =      
C   D(ICD*(N/2+1)+(K-1)*JCD) = 0, FOR EVERY K = 1,NNS.                  
C                                                                       
C INPUT PARAMETERS -                                                    
C                                                                       
C   N     - THE NUMBER OF ELEMENTS IN EACH VECTOR WHOSE FOURIER         
C           TRANSFORM IS DESIRED. THIS PARAMETER MUST BE GREATER        
C           THAN 1, AND MUST BE EVEN.                                   
C                                                                       
C   NNS   - THE NUMBER OF VECTORS WHOSE FOURIER TRANSFORM IS            
C           DESIRED - NNS MUST BE GREATER THAN 0.                       
C   A     - A DOUBLE PRECISION ARRAY CONTAINING THE INPUT VECTORS.  THE 
C           SPACING BETWEEN EACH ELEMENT OF EVERY VECTOR IS IA, WHILE   
C           SPACING BETWEEN VECTORS IS JA. USUALLY, THE COLUMNS         
C           OF A WILL BE DISTINCT INPUT VECTORS - ALTHOUGH AN           
C           INTERCHANGE OF IA AND JA WILL AFFECT A TRANSPOSITION.       
C                                                                       
C   IA    - THE SPACING BETWEEN ELEMENTS WITHIN EACH INPUT VECTOR       
C           (COLUMNS OF A).                                             
C                                                                       
C   JA    - THE SPACING BETWEEN INPUT VECTORS OF A (SPACING BETWEEN     
C           ROW ELEMENTS OF A).                                         
C                                                                       
C   C     - A DOUBLE PRECISION ARRAY CONTAINING THE REAL PARTS OF THE   
C           OUTPUT VECTORS. THE SPACING BETWEEN EACH ELEMENT OF EVERY   
C           VECTOR IS ICD, WHILE THE SPACING BETWEEN VECTORS IS         
C           JCD. SEE THE DISCUSSION ABOVE FOR THE INPUT ARRAY A.        
C                                                                       
C   D     - A DOUBLE PRECISION ARRAY CONTAINING THE IMAGINARY PARTS OF  
C           THE OUTPUT VECTORS. THE SPACING BETWEEN EACH ELEMENT OF     
C           EVERY VECTOR IS ICD, WHILE THE SPACING BETWEEN VECTORS IS   
C           JCD. SEE THE DISCUSSION ABOVE FOR THE INPUT ARRAY A.        
C                                                                       
C   ICD   - THE SPACING BETWEEN ELEMENTS WITHIN EACH OUTPUT VECTOR OF   
C           REAL PARTS (COLUMNS OF C), AND THE SPACING BETWEEN THE      
C           ELEMENTS WITHIN EACH VECTOR OF IMAGINARY PARTS (COLUMNS     
C           OF D).                                                      
C                                                                       
C   JCD   - THE SPACING BETWEEN OUTPUT VECTORS OF REAL PARTS (SPACING   
C           BETWEEN ROW ELEMENTS OF C), AND THE SPACING BETWEEN         
C           OUTPUT VECTORS OF IMAGINARY PARTS (SPACING BETWEEN ROW      
C           ELEMENTS OF D).                                             
C                                                                       
C   IFX   - AN INTEGER ARRAY OF LENGTH 25 IN WHICH THE FACTORS OF       
C           N/2 WERE SAVED BY THE INITIALIZATION SUBROUTINE DMFTRI      
C           THE ELEMENTS OF IFX ARE                                     
C                      IFX(1) = N/2                                     
C                      IFX(2) = M, THE NUMBER OF FACTORS OF N/2         
C                      IFX(3) THROUGH IFX(M+2) ARE THE FACTORS OF N/2   
C                                                                       
C   T     - A DOUBLE PRECISION ARRAY OF LENGTH N WHICH CONTAINS THE     
C           TRIGONOMETRIC TABLES COMPUTED BY THE INITIALIZATION         
C           SUBROUTINE DMFTRI.  THE ELEMENTS OF T ARE                   
C                      T(2*(K-1)+1) =DCOS(PI*(K-1)/N)                   
C                      T(2*(K-1)+2) =DSIN(PI*(K-1)/N)                   
C           WHERE K = 1,N.                                              
C                                                                       
C   SGN  -  A DOUBLE PRECISION PARAMETER WHICH MUST BE EITHER +1.0 OR   
C           -1.0.  THIS PARAMETER DETERMINES THE SIGN OF THE ARGUMENT IN
C           THE EXPONENTIAL DEFINING THE DISCRETE FOURIER TRANSFORM.    
C           (SEE ABOVE EXPRESSION FOR Y(L,K)) IF SGN IS                 
C           NEITHER 1.0 NOR -1.0 A FATAL ERROR RESULTS.                 
C                                                                       
      COMMON /CSTAK/DSTAK(500)                                          
      DOUBLE PRECISION DSTAK                                            
C                                                                       
      EXTERNAL ISTKGT                                                   
      INTEGER ISTKGT                                                    
      DOUBLE PRECISION A(1),C(1),D(1)                                   
      DOUBLE PRECISION T(2,1),W(500)                                    
      DOUBLE PRECISION SGN                                              
C/6S                                                                    
C     INTEGER IFX(1)                                                    
C/7S                                                                    
      INTEGER IFX(*)                                                    
C/                                                                      
      INTEGER I,I2,IA,IB,ICD,IFAC,IW,IWN,JA,JCD,LA                      
      INTEGER N,N2,NF2,NFX,NNS,NP,NP2,NWK                               
      LOGICAL ITGLE                                                     
      EQUIVALENCE (W(1),DSTAK(1))                                       
C                                                                       
C  INPUT PARAMETER CHECKS                                               
C                                                                       
C/6S                                                                    
C     IF (N .LT. 0 .OR. NNS .LT. 0)                                     
C    1   CALL SETERR(33HDMFTRC - N .LT. 0 .OR. NNS .LT. 0,33,3,2)       
C/7S                                                                    
      IF (N .LT. 0 .OR. NNS .LT. 0)                                     
     1   CALL SETERR('DMFTRC - N .LT. 0 .OR. NNS .LT. 0',33,3,2)        
C/                                                                      
      IF (N .EQ. 0) GO TO 999                                           
      IF (NNS .EQ. 0) GO TO 999                                         
C/6S                                                                    
C     IF(N .NE. 2*IFX(1))                                               
C    1   CALL SETERR(36HDMFTRC - IFX INITIALIZED INCORRECTLY,36,1,2)    
C     IF(DABS(SGN).NE.1.0D0)                                            
C    1   CALL SETERR(38HDMFTRC - SIGN PARAMETER .NE. 1. OR -1.,38,2,2)  
C/7S                                                                    
      IF(N .NE. 2*IFX(1))                                               
     1   CALL SETERR('DMFTRC - IFX INITIALIZED INCORRECTLY',36,1,2)     
      IF(DABS(SGN).NE.1.0D0)                                            
     1   CALL SETERR('DMFTRC - SIGN PARAMETER .NE. 1. OR -1.',38,2,2)   
C/                                                                      
C                                                                       
      I2   = 2*IA                                                       
      IB   = IA + 1                                                     
      LA   = 1                                                          
      N2   = N/2                                                        
      NP2  = N+2                                                        
      NWK  = NP2*NNS                                                    
      NP   = NP2/2                                                      
C                                                                       
C  GET WORK SPACE                                                       
C                                                                       
      IW = ISTKGT(NWK,4)                                                
      IWN = IW+NP                                                       
C                                                                       
C   FIRST PASS THROUGH DATA.  LOGICAL VARIABLE ITGLE IS SET BY          
C      NFX (THE NUMBER OF FACTORS) BEING EVEN/ODD                       
C                                                                       
      NFX  = IFX(2)                                                     
      IF (NFX .GT. 0) GO TO 10                                          
C        *** SPECIAL CASE -- N = 2 ***                                  
         CALL DM88FT(N2,NNS,A(1),A(IB),I2,JA,W(IW),W(IWN),1,NP2)        
         GO TO 60                                                       
 10   IFAC = IFX(3)                                                     
      NF2  = NFX/2                                                      
      IF(NFX .EQ. NF2+NF2) GO TO 20                                     
C                                                                       
C        *** ODD NUMBER OF FACTORS ***                                  
C                                                                       
         CALL DM1FT(A(1),A(IB),W(IW),W(IWN),T,2,                        
     1                I2,1,JA,NP2,NNS,N2,IFAC,LA,SGN)                   
         ITGLE = .TRUE.                                                 
         IF(NFX .EQ. 1) GO TO 60                                        
         GO TO 30                                                       
C                                                                       
C     *** EVEN NUMBER OF FACTORS ***                                    
C                                                                       
C     *** COPY INTO WORK SPACE TO ALLOW INPUT AND OUTPUT TO COINCIDE ***
C                                                                       
 20   CALL DM88FT(N2,NNS,A(1),A(IB),I2,JA, W(IW),W(IWN),1,NP2)          
      CALL DM1FT(W(IW),W(IWN),C,D,T,2,1,ICD,NP2,JCD,NNS,N2,IFAC,LA,SGN) 
      ITGLE = .FALSE.                                                   
C                                                                       
C *** MAIN LOOP ***                                                     
C                                                                       
 30   DO 50 I = 2,NFX                                                   
         LA = LA*IFAC                                                   
         IFAC = IFX(I+2)                                                
         IF(ITGLE) GO TO 40                                             
            CALL DM1FT(C,D,W(IW),W(IWN),T,2,                            
     1                   ICD,1,JCD,NP2,NNS,N2,IFAC,LA,SGN)              
            ITGLE = .TRUE.                                              
            GO TO 50                                                    
C                                                                       
 40         CALL DM1FT(W(IW),W(IWN),C,D,T,2,                            
     1               1,ICD,NP2,JCD,NNS,N2,IFAC,LA,SGN)                  
            ITGLE = .FALSE.                                             
 50   CONTINUE                                                          
C                                                                       
C  **POST PROCESSING IS VIA - COOLEY, TUKEY, LEWIS, AND WELCH           
C                                                                       
 60   CALL DM33FT(N,NNS,W(IW),W(IWN),W(IW),W(IWN),1,NP2,                
     1           C,D,ICD,JCD,T,SGN)                                     
C                                                                       
      CALL ISTKRL(1)                                                    
C                                                                       
 999  RETURN                                                            
      END                                                               
      SUBROUTINE DMFTCR(N,NNS,A,B,IAB,JAB,C,IC,JC,IFX,T,SGN)            
C                                                                       
C   SUBROUTINE DMFTCR(N,NNS,A,B,IA,JA,C,IC,JC,IFX,T,SGN)                
C                                                                       
C   THIS SUBROUTINE COMPUTES MULTIPLE DISCRETE FOURIER TRANSFORMS       
C   OF NNS HALF-COMPLEX INPUT VECTORS (A+I*B) OF LENGTH N/2+1. THE      
C   OUTPUT IS A SET OF REAL VECTORS (C). THAT IS, IF X = A + I*B        
C   ARE A SET OF COMPLEX INPUT VECTORS (K COUNTS VECTORS, K = 1,NNS,    
C   WHOSE ELEMENTS ARE X(J,K), J = 1,N) THEN THE OUTPUT FROM MFTCR IS   
C                                                                       
C        C(L,K) =   SUM  EXP(+-2*PI*I*(J-1)*(L-1)/N)*X(J,K)             
C                  J=1,N                                                
C                                                                       
C   WHERE C(L,K) IS PURELY REAL, AND K = 1,NNS -- THE SIGN OF THE       
C   OF THE EXPONENTIAL IS THAT OF PARAMETER  SGN. THAT IS, MFTCR        
C   COMPUTES NNS (K = 1,NNS) COMPLEX DISCRETE FOURIER TRANSFORMS OF     
C   LENGTH N (L = 1,N) SIMULTANEOUSLY.                                  
C                                                                       
C   IT IS IMPORTANT TO NOTE THAT SINCE THE OUTPUT VECTORS ARE REAL, THAT
C   THE INPUT X HAS THE FOLLOWING PROPERTY                              
C                                                                       
C        X(L,K) = CONJG(X(N+2-L,K))                                     
C                                                                       
C   FOR L = 1,N. HENCE, ONLY THE FIRST N/2+1 ELEMENTS OF THE COMPLEX    
C   INPUT VECTORS (X = A+I*B) ARE USED BY MFTCR. FURTHERMORE, FROM      
C   THE ABOVE RELATION, X(1,K) AND X(N/2+1,K) MUST HAVE ZERO IMAGINARY  
C   PARTS. IT IS THE USERS RESPONSIBILITY TO ASSURE B(1,K) = B(N/2+1,K) 
C   ARE ZERO, THAT IS - B(1+(K-1)*JAB) = B(IAB*(N/2+1)+(K-1)*JAB) = 0.  
C                                                                       
C INPUT PARAMETERS -                                                    
C                                                                       
C   N     - THE NUMBER OF ELEMENTS IN EACH VECTOR WHOSE FOURIER         
C           TRANSFORM IS DESIRED. THIS PARAMETER MUST BE GREATER        
C           THAN 1, AND MUST BE EVEN.                                   
C                                                                       
C   NNS   - THE NUMBER OF VECTORS WHOSE FOURIER TRANSFORM IS            
C           DESIRED - NNS MUST BE GREATER THAN 0.                       
C                                                                       
C   A     - A DOUBLE PRECISION ARRAY CONTAINING THE REAL PARTS OF THE   
C           INPUT VECTORS. THE SPACING BETWEEN ELEMENTS OF EACH VECTOR  
C           IS IAB, WHILE THE SPACING BETWEEN VECTORS IS JAB.           
C           USUALLY, THE COLUMNS OF A WILL BE THE INPUT VECTORS -       
C           ALTHOUGH AN INTERCHANGE OF IAB AND JAB WILL AFFECT A        
C           TRANSPOSITION.                                              
C                                                                       
C   B     - A DOUBLE PRECISION ARRAY CONTAINING THE IMAGINARY PARTS OF  
C           THE INPUT VECTORS. THE SPACING BETWEEN EACH ELEMENT OF EVERY
C           VECTOR IS IAB, WHILE THE SPACING BETWEEN VECTORS IS         
C           JAB. SEE THE DISCUSSION ABOVE FOR THE INPUT ARRAY A.        
C                                                                       
C   IAB   - THE SPACING BETWEEN ELEMENTS WITHIN EACH INPUT VECTOR OF    
C           REAL PARTS (COLUMNS OF A), AND THE SPACING BETWEEN THE      
C           ELEMENTS WITHIN EACH VECTOR OF IMAGINARY PARTS (COLUMNS     
C           OF B).                                                      
C                                                                       
C   JAB   - THE SPACING BETWEEN INPUT VECTORS OF REAL PARTS (SPACING    
C           BETWEEN ROW ELEMENTS OF A), AND THE SPACING BETWEEN         
C           INPUT VECTORS OF IMAGINARY PARTS (SPACING BETWEEN ROW       
C           ELEMENTS OF B).                                             
C                                                                       
C   C     - A DOUBLE PRECISION ARRAY CONTAINING THE OUTPUT VECTORS. THE 
C           SPACING BETWEEN EACH ELEMENT OF EVERY VECTOR IS IC, WHILE   
C           THE SPACING BETWEEN VECTORS IS JC.                          
C                                                                       
C   IC    - THE SPACING BETWEEN ELEMENTS WITHIN EACH OUTPUT VECTOR      
C           (COLUMNS OF C).                                             
C                                                                       
C   JC    - THE SPACING BETWEEN OUTPUT VECTORS OF C (SPACING BETWEEN    
C           ROW ELEMENTS OF C).                                         
C                                                                       
C   IFX   - AN INTEGER ARRAY OF LENGTH 25 IN WHICH THE FACTORS OF       
C           N/2 WERE SAVED BY THE INITIALIZATION SUBROUTINE DMFTRI      
C           THE ELEMENTS OF IFX ARE                                     
C                      IFX(1) = N/2                                     
C                      IFX(2) = M, THE NUMBER OF FACTORS OF N/2         
C                      IFX(3) THROUGH IFX(M+2) ARE THE FACTORS OF N/2   
C                                                                       
C   T     - A DOUBLE PRECISION ARRAY OF LENGTH N WHICH CONTAINS THE     
C           TRIGONOMETRIC TABLES COMPUTED BY THE INITIALIZATION         
C           SUBROUTINE DMFTRI.  THE ELEMENTS OF T ARE                   
C                      T(2*(K-1)+1) = DCOS(PI*(K-1)/N)                  
C                      T(2*(K-1)+2) = SIN(PI*(K-1)/N)                   
C           WHERE K = 1,N.                                              
C                                                                       
C   SGN  -  A DOUBLE PRECISION PARAMETER WHICH MUST BE EITHER +1.0 OR   
C           -1.0.  THIS PARAMETER DETERMINES THE SIGN OF THE ARGUMENT IN
C           THE EXPONENTIAL DEFINING THE DISCRETE FOURIER TRANSFORM.    
C           (SEE ABOVE EXPRESSION FOR C(L,K)). IF SGN IS                
C           NEITHER 1.0 NOR -1.0 A FATAL ERROR RESULTS.                 
C                                                                       
      COMMON /CSTAK/DSTAK(500)                                          
      DOUBLE PRECISION DSTAK                                            
C                                                                       
      EXTERNAL ISTKGT                                                   
      INTEGER ISTKGT                                                    
      DOUBLE PRECISION A(1),B(1),C(1)                                   
      DOUBLE PRECISION T(2,1),W(500)                                    
      DOUBLE PRECISION SGN                                              
C/6S                                                                    
C     INTEGER IFX(1)                                                    
C/7S                                                                    
      INTEGER IFX(*)                                                    
C/                                                                      
      INTEGER I,I2,IAB,IC,ID,IFAC,IW,IWN,JAB,JC,LA,N                    
      INTEGER N2,NF2,NFX,NNS,NP,NP2,NWK                                 
      LOGICAL EVEN, ITGLE                                               
      EQUIVALENCE (W(1),DSTAK(1))                                       
C                                                                       
C  INPUT PARAMETER CHECKS                                               
C                                                                       
C/6S                                                                    
C     IF (N .LT. 0 .OR. NNS .LT. 0)                                     
C    1   CALL SETERR(33HDMFTCR - N .LT. 0 .OR. NNS .LT. 0,33,3,2)       
C/7S                                                                    
      IF (N .LT. 0 .OR. NNS .LT. 0)                                     
     1   CALL SETERR('DMFTCR - N .LT. 0 .OR. NNS .LT. 0',33,3,2)        
C/                                                                      
      IF (N .EQ. 0) GO TO 999                                           
      IF (NNS .EQ. 0) GO TO 999                                         
C/6S                                                                    
C     IF(N .NE. 2*IFX(1))                                               
C    1   CALL SETERR(36HDMFTCR - IFX INITIALIZED INCORRECTLY,36,1,2)    
C     IF(DABS(SGN).NE.1.0D0)                                            
C    1   CALL SETERR(38HDMFTCR - SIGN PARAMETER .NE. 1. OR -1.,38,2,2)  
C/7S                                                                    
      IF(N .NE. 2*IFX(1))                                               
     1   CALL SETERR('DMFTCR - IFX INITIALIZED INCORRECTLY',36,1,2)     
      IF(DABS(SGN).NE.1.0D0)                                            
     1   CALL SETERR('DMFTCR - SIGN PARAMETER .NE. 1. OR -1.',38,2,2)   
C/                                                                      
C                                                                       
      I2   = 2*IC                                                       
      ID   = IC + 1                                                     
      N2   = N/2                                                        
      NFX  = IFX(2)                                                     
      NF2  = NFX/2                                                      
      EVEN = NFX .EQ. NF2+NF2                                           
      NP2  = N+2                                                        
      NP   = NP2/2                                                      
      NWK  = NP2*NNS                                                    
C                                                                       
C   GET WORK SPACE                                                      
C                                                                       
      IW = ISTKGT(NWK,4)                                                
      IWN = IW+NP                                                       
C                                                                       
C**PRE-PROCESSING IS VIA - COOLEY, TUKEY, LEWIS, AND WELCH              
C                                                                       
      CALL DM44FT(N,NNS,A,B,IAB,JAB,W(IW),W(IWN),1,NP2,T,SGN)           
      ITGLE = .TRUE.                                                    
      IFAC = 1                                                          
      LA   = 1                                                          
C                                                                       
C   MAIN LOOP                                                           
C                                                                       
      DO 20 I = 1,NFX                                                   
         LA = LA*IFAC                                                   
         IFAC = IFX(I+2)                                                
         IF(ITGLE) GO TO 10                                             
            CALL DM1FT(C(1),C(ID),W(IW),W(IWN),T,2,                     
     1                I2,1,JC,NP2,NNS,N2,IFAC,LA,SGN)                   
            ITGLE = .TRUE.                                              
            GO TO 20                                                    
 10         CALL DM1FT(W(IW),W(IWN),C(1),C(ID),T,2,                     
     1                1,I2,NP2,JC,NNS,N2,IFAC,LA,SGN)                   
            ITGLE = .FALSE.                                             
C           ENDIF                                                       
 20      CONTINUE                                                       
C                                                                       
      IF (EVEN) CALL DM88FT(N2,NNS,W(IW),W(IWN),1,NP2,C(1),C(ID),I2,JC) 
C                                                                       
      CALL ISTKRL(1)                                                    
C                                                                       
 999  RETURN                                                            
      END                                                               
      SUBROUTINE DM1FT(A,B,C,D,T,IT,                                    
     *           IC1,IC2,IC3,IC4,NNS,N,IFAC,LA,SGN)                     
C                                                                       
C  SUBROUTINE DM1FT PERFORMS ONE PASS OF RADIX IFAC THROUGH DATA        
C  IN THE COMPUTATION OF A MULTIPLE DISCRETE FOURIER TRANSFORM.         
C  EACH CALLED SUBROUTINE DM2FT, M3FT, ETC. ARE RESPONSIBLE FOR         
C  ACTUAL COMPUTATION OF RADIX IFAC = 2, IFAC = 3, ETC. IN THE          
C  CASE THAT IFAC.GE.7, A GENERAL ODD FACTOR (PRIME) SUBROUTINE         
C  M7FT IS CALLED. THIS IS A LOW LEVEL ROUTINE, WITH NO ERROR           
C  CONDITIONS.                                                          
C                                                                       
      DOUBLE PRECISION A(1),B(1),C(1),D(1),T(1)                         
      DOUBLE PRECISION SGN                                              
      DOUBLE PRECISION C36,S36,C72,S72,S60                              
      INTEGER IA,IB,IC,IC1,IC2,IC3,IC4,ID,IE,IFAC,IGO,II,IT,IFACT       
      INTEGER JA,JB,JC,JD,JE,JJ,LA,M,N,NNS                              
C                                                                       
      COMMON /DM55FT/C36,S36,C72,S72,S60                                
C                                                                       
      M   = N/IFAC                                                      
      II  = M*IC1                                                       
      JJ  = LA*IC2                                                      
      IFACT = IFAC+1                                                    
      IF(IFAC.GT.7)IFACT=8                                              
      IGO = IFACT/2                                                     
      GO TO (10,20,30,40),IGO                                           
C                                                                       
C      FACTOR 2                                                         
C                                                                       
 10   IA = 1                                                            
      JA = 1                                                            
      IB = IA+II                                                        
      JB = JA+JJ                                                        
      CALL DM2FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,                
     *          A(IA),B(IA),A(IB),B(IB),                                
     *          C(JA),D(JA),C(JB),D(JB))                                
      GO TO 50                                                          
C                                                                       
C      FACTOR 3                                                         
C                                                                       
 20   IA = 1                                                            
      JA = 1                                                            
      IB = IA+II                                                        
      JB = JA+JJ                                                        
      IC = IB+II                                                        
      JC = JB+JJ                                                        
      CALL DM3FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,                
     *          A(IA),B(IA),A(IB),B(IB),A(IC),B(IC),                    
     *          C(JA),D(JA),C(JB),D(JB),C(JC),D(JC))                    
      GO TO 50                                                          
C                                                                       
C      FACTOR 5                                                         
C                                                                       
 30   IA = 1                                                            
      JA = 1                                                            
      IB = IA+II                                                        
      JB = JA+JJ                                                        
      IC = IB+II                                                        
      JC = JB+JJ                                                        
      ID = IC+II                                                        
      JD = JC+JJ                                                        
      IE = ID+II                                                        
      JE = JD+JJ                                                        
      CALL DM5FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,                
     *    A(IA),B(IA),A(IB),B(IB),A(IC),B(IC),A(ID),B(ID),A(IE),B(IE),  
     *    C(JA),D(JA),C(JB),D(JB),C(JC),D(JC),C(JD),D(JD),C(JE),D(JE))  
      GO TO 50                                                          
C                                                                       
C      GENERAL ODD FACTOR                                               
C                                                                       
 40   CONTINUE                                                          
      CALL DM7FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,                
     *          A(1),B(1),A(1),B(1),A(1),B(1),                          
     *          C(1),D(1),C(1),D(1),C(1),D(1))                          
 50   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE DM2FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,          
     *                AIA,BIA,AIB,BIB,CJA,DJA,CJB,DJB)                  
C                                                                       
C  RADIX 2 SUBROUTINE FOR MULTIPLE DISCRETE FOURIER TRANSFORM           
C                                                                       
      DOUBLE PRECISION AIA(1),BIA(1),AIB(1),BIB(1)                      
      DOUBLE PRECISION CJA(1),DJA(1),CJB(1),DJB(1)                      
      DOUBLE PRECISION T(1)                                             
      DOUBLE PRECISION SGN,C1,S1                                        
      INTEGER I,IB0,IC1,IC2,IC3,IC4,IFAC,IINK,IJK,IT                    
      INTEGER J,JB0,JINK,JUMP,K,KB,L,LA,LA1,M,N,NNS                     
C                                                                       
      M    = N/IFAC                                                     
      IINK = M*IC1                                                      
      JINK = LA*IC2                                                     
      JUMP = (IFAC-1)*JINK                                              
      IB0  = 1                                                          
      JB0  = 1                                                          
      DO 20 L = 1,LA                                                    
         I = IB0                                                        
         J = JB0                                                        
         DO 10 IJK = 1,NNS                                              
            CJA(J) = AIA(I)+AIB(I)                                      
            DJA(J) = BIA(I)+BIB(I)                                      
            CJB(J) = AIA(I)-AIB(I)                                      
            DJB(J) = BIA(I)-BIB(I)                                      
            I = I+IC3                                                   
            J = J+IC4                                                   
 10      CONTINUE                                                       
         IB0 = IB0+IC1                                                  
         JB0 = JB0+IC2                                                  
 20   CONTINUE                                                          
      IF (LA.EQ.M) RETURN                                               
      LA1 = LA+1                                                        
      JB0 = JB0+JUMP                                                    
      DO 50 K = LA1,M,LA                                                
         KB = (K+K-2)*IT                                                
         C1 = T(KB+1)                                                   
         S1 = SGN*T(KB+2)                                               
         DO 40 L = 1,LA                                                 
            I = IB0                                                     
            J = JB0                                                     
            DO 30 IJK = 1,NNS                                           
               CJA(J) = AIA(I)+AIB(I)                                   
               DJA(J) = BIA(I)+BIB(I)                                   
               CJB(J) = C1*(AIA(I)-AIB(I))-S1*(BIA(I)-BIB(I))           
               DJB(J) = S1*(AIA(I)-AIB(I))+C1*(BIA(I)-BIB(I))           
               I = I+IC3                                                
               J = J+IC4                                                
 30         CONTINUE                                                    
            IB0 = IB0+IC1                                               
            JB0 = JB0+IC2                                               
 40      CONTINUE                                                       
         JB0 = JB0+JUMP                                                 
 50   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE DM3FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,          
     *                AIA,BIA,AIB,BIB,AIC,BIC,                          
     *                CJA,DJA,CJB,DJB,CJC,DJC)                          
C                                                                       
C  RADIX 3 SUBROUTINE FOR MULTIPLE DISCRETE FOURIER TRANSFORM           
C                                                                       
      DOUBLE PRECISION AIA(1),BIA(1),AIB(1),BIB(1),AIC(1),BIC(1)        
      DOUBLE PRECISION CJA(1),DJA(1),CJB(1),DJB(1),CJC(1),DJC(1)        
      DOUBLE PRECISION T(1)                                             
      DOUBLE PRECISION C1,C2,S1,S2,SGN                                  
      DOUBLE PRECISION C36,S36,C72,S72,S60,SIN60                        
      INTEGER I,IB0,IC1,IC2,IC3,IC4,IFAC,IINK,IJK,IT                    
      INTEGER J,JB0,JINK,JUMP,K,KB,KC,L,LA,LA1,M,N,NNS                  
C                                                                       
      COMMON /DM55FT/C36,S36,C72,S72,S60                                
C                                                                       
      M     = N/IFAC                                                    
      IINK  = M*IC1                                                     
      JINK  = LA*IC2                                                    
      JUMP  = (IFAC-1)*JINK                                             
      IB0   = 1                                                         
      JB0   = 1                                                         
      SIN60 = S60*SGN                                                   
C                                                                       
      DO 20 L = 1,LA                                                    
         I = IB0                                                        
         J = JB0                                                        
         DO 10 IJK = 1,NNS                                              
            CJA(J) = AIA(I)+(AIB(I)+AIC(I))                             
            DJA(J) = BIA(I)+(BIB(I)+BIC(I))                             
            CJB(J) = (AIA(I)-0.5D0*(AIB(I)+AIC(I)))                     
     *               -(SIN60*(BIB(I)-BIC(I)))                           
            CJC(J) = (AIA(I)-0.5D0*(AIB(I)+AIC(I)))                     
     *               +(SIN60*(BIB(I)-BIC(I)))                           
            DJB(J) = (BIA(I)-0.5D0*(BIB(I)+BIC(I)))                     
     *               +(SIN60*(AIB(I)-AIC(I)))                           
            DJC(J) = (BIA(I)-0.5D0*(BIB(I)+BIC(I)))                     
     *               -(SIN60*(AIB(I)-AIC(I)))                           
            I = I+IC3                                                   
            J = J+IC4                                                   
 10      CONTINUE                                                       
         IB0 = IB0+IC1                                                  
         JB0 = JB0+IC2                                                  
 20   CONTINUE                                                          
      IF (LA.EQ.M) RETURN                                               
      LA1 = LA+1                                                        
      JB0 = JB0+JUMP                                                    
      DO 50 K = LA1,M,LA                                                
         KB = (K+K-2)*IT                                                
         KC = KB+KB                                                     
         C1 = T(KB+1)                                                   
         S1 = SGN*T(KB+2)                                               
         C2 = T(KC+1)                                                   
         S2 = SGN*T(KC+2)                                               
         DO 40 L = 1,LA                                                 
            I = IB0                                                     
            J = JB0                                                     
            DO 30 IJK = 1,NNS                                           
               CJA(J) = AIA(I)+(AIB(I)+AIC(I))                          
               DJA(J) = BIA(I)+(BIB(I)+BIC(I))                          
               CJB(J) =                                                 
     *                  C1*((AIA(I)-0.5D0*(AIB(I)+AIC(I)))              
     *                 -(SIN60*(BIB(I)-BIC(I))))                        
     *                 -S1*((BIA(I)-0.5D0*(BIB(I)+BIC(I)))              
     *                 +(SIN60*(AIB(I)-AIC(I))))                        
               DJB(J) =                                                 
     *                  S1*((AIA(I)-0.5D0*(AIB(I)+AIC(I)))              
     *                 -(SIN60*(BIB(I)-BIC(I))))                        
     *                 +C1*((BIA(I)-0.5D0*(BIB(I)+BIC(I)))              
     *                 +(SIN60*(AIB(I)-AIC(I))))                        
               CJC(J) =                                                 
     *                  C2*((AIA(I)-0.5D0*(AIB(I)+AIC(I)))              
     *                 +(SIN60*(BIB(I)-BIC(I))))                        
     *                 -S2*((BIA(I)-0.5D0*(BIB(I)+BIC(I)))              
     *                 -(SIN60*(AIB(I)-AIC(I))))                        
               DJC(J) =                                                 
     *                  S2*((AIA(I)-0.5D0*(AIB(I)+AIC(I)))              
     *                 +(SIN60*(BIB(I)-BIC(I))))                        
     *                 +C2*((BIA(I)-0.5D0*(BIB(I)+BIC(I)))              
     *                 -(SIN60*(AIB(I)-AIC(I))))                        
               I = I+IC3                                                
               J = J+IC4                                                
 30         CONTINUE                                                    
            IB0 = IB0+IC1                                               
            JB0 = JB0+IC2                                               
 40      CONTINUE                                                       
         JB0 = JB0+JUMP                                                 
 50   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE DM5FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,          
     *                AIA,BIA,AIB,BIB,AIC,BIC,AID,BID,AIE,BIE,          
     *                CJA,DJA,CJB,DJB,CJC,DJC,CJD,DJD,CJE,DJE)          
C                                                                       
C   RADIX 5 SUBROUTINE FOR MULTIPLE DISCRETE FOURIER TRANSFORM          
C                                                                       
      DOUBLE PRECISION AIA(1),BIA(1),AIB(1),BIB(1),AIC(1)               
      DOUBLE PRECISION BIC(1),AID(1),BID(1),AIE(1),BIE(1)               
      DOUBLE PRECISION CJA(1),DJA(1),CJB(1),DJB(1),CJC(1)               
      DOUBLE PRECISION DJC(1),CJD(1),DJD(1),CJE(1),DJE(1)               
      DOUBLE PRECISION T(1)                                             
      DOUBLE PRECISION SGN                                              
      DOUBLE PRECISION C1,C2,C3,C4,COS36,COS72,S1,S2,S3,S4,SIN36,SIN72  
      DOUBLE PRECISION C36,S36,C72,S72,S60                              
      INTEGER I,IB0,IC1,IC2,IC3,IC4,IFAC,IINK,IJK,IT                    
      INTEGER J,JB0,JINK,JUMP,K,KB,KC,KD,KE,L,LA,LA1,M,N,NNS            
C                                                                       
      COMMON /DM55FT/C36,S36,C72,S72,S60                                
C                                                                       
      M     = N/IFAC                                                    
      IINK  = M*IC1                                                     
      JINK  = LA*IC2                                                    
      JUMP  = (IFAC-1)*JINK                                             
      IB0   = 1                                                         
      JB0   = 1                                                         
      COS36 = C36                                                       
      SIN36 = SGN*S36                                                   
      COS72 = C72                                                       
      SIN72 = SGN*S72                                                   
C                                                                       
      DO 20 L = 1,LA                                                    
         I = IB0                                                        
         J = JB0                                                        
         DO 10 IJK = 1,NNS                                              
            CJA(J) = AIA(I)+(AIB(I)+AIE(I))+(AIC(I)+AID(I))             
            DJA(J) = BIA(I)+(BIB(I)+BIE(I))+(BIC(I)+BID(I))             
            CJB(J) = (AIA(I)+COS72*(AIB(I)+AIE(I))                      
     *               -COS36*(AIC(I)+AID(I)))                            
     *               -(SIN72*(BIB(I)-BIE(I))                            
     *               +SIN36*(BIC(I)-BID(I)))                            
            CJE(J) = (AIA(I)+COS72*(AIB(I)+AIE(I))                      
     *               -COS36*(AIC(I)+AID(I)))                            
     *               +(SIN72*(BIB(I)-BIE(I))                            
     *               +SIN36*(BIC(I)-BID(I)))                            
            DJB(J) = (BIA(I)+COS72*(BIB(I)+BIE(I))                      
     *               -COS36*(BIC(I)+BID(I)))                            
     *               +(SIN72*(AIB(I)-AIE(I))                            
     *               +SIN36*(AIC(I)-AID(I)))                            
            DJE(J) = (BIA(I)+COS72*(BIB(I)+BIE(I))                      
     *               -COS36*(BIC(I)+BID(I)))                            
     *               -(SIN72*(AIB(I)-AIE(I))                            
     *               +SIN36*(AIC(I)-AID(I)))                            
            CJC(J) = (AIA(I)-COS36*(AIB(I)+AIE(I))                      
     *               +COS72*(AIC(I)+AID(I)))                            
     *               -(SIN36*(BIB(I)-BIE(I))                            
     *               -SIN72*(BIC(I)-BID(I)))                            
            CJD(J) = (AIA(I)-COS36*(AIB(I)+AIE(I))                      
     *               +COS72*(AIC(I)+AID(I)))                            
     *               +(SIN36*(BIB(I)-BIE(I))                            
     *               -SIN72*(BIC(I)-BID(I)))                            
            DJC(J) = (BIA(I)-COS36*(BIB(I)+BIE(I))                      
     *               +COS72*(BIC(I)+BID(I)))                            
     *               +(SIN36*(AIB(I)-AIE(I))                            
     *               -SIN72*(AIC(I)-AID(I)))                            
            DJD(J) = (BIA(I)-COS36*(BIB(I)+BIE(I))                      
     *               +COS72*(BIC(I)+BID(I)))                            
     *               -(SIN36*(AIB(I)-AIE(I))                            
     *               -SIN72*(AIC(I)-AID(I)))                            
            I = I+IC3                                                   
            J = J+IC4                                                   
 10      CONTINUE                                                       
         IB0 = IB0+IC1                                                  
         JB0 = JB0+IC2                                                  
 20   CONTINUE                                                          
      IF (LA.EQ.M) GOTO 60                                              
      LA1 = LA+1                                                        
      JB0 = JB0+JUMP                                                    
      DO 50 K = LA1,M,LA                                                
         KB = (K+K-2)*IT                                                
         KC = KB+KB                                                     
         KD = KC+KB                                                     
         KE = KD+KB                                                     
         C1 = T(KB+1)                                                   
         S1 = SGN*T(KB+2)                                               
         C2 = T(KC+1)                                                   
         S2 = SGN*T(KC+2)                                               
         C3 = T(KD+1)                                                   
         S3 = SGN*T(KD+2)                                               
         C4 = T(KE+1)                                                   
         S4 = SGN*T(KE+2)                                               
         DO 40 L = 1,LA                                                 
            I = IB0                                                     
            J = JB0                                                     
            DO 30 IJK = 1,NNS                                           
               CJA(J) = AIA(I)+(AIB(I)+AIE(I))+(AIC(I)+AID(I))          
               DJA(J) = BIA(I)+(BIB(I)+BIE(I))+(BIC(I)+BID(I))          
               CJB(J) =                                                 
     *             C1*((AIA(I)                                          
     *             +COS72*(AIB(I)+AIE(I))-COS36*(AIC(I)+AID(I)))        
     *             -(SIN72*(BIB(I)-BIE(I))+SIN36*(BIC(I)-BID(I))))      
     *             -S1*((BIA(I)                                         
     *             +COS72*(BIB(I)+BIE(I))-COS36*(BIC(I)+BID(I)))        
     *             +(SIN72*(AIB(I)-AIE(I))+SIN36*(AIC(I)-AID(I))))      
               DJB(J) =                                                 
     *             S1*((AIA(I)                                          
     *             +COS72*(AIB(I)+AIE(I))-COS36*(AIC(I)+AID(I)))        
     *             -(SIN72*(BIB(I)-BIE(I))+SIN36*(BIC(I)-BID(I))))      
     *             +C1*((BIA(I)                                         
     *             +COS72*(BIB(I)+BIE(I))-COS36*(BIC(I)+BID(I)))        
     *             +(SIN72*(AIB(I)-AIE(I))+SIN36*(AIC(I)-AID(I))))      
               CJE(J) =                                                 
     *             C4*((AIA(I)                                          
     *             +COS72*(AIB(I)+AIE(I))-COS36*(AIC(I)+AID(I)))        
     *             +(SIN72*(BIB(I)-BIE(I))+SIN36*(BIC(I)-BID(I))))      
     *             -S4*((BIA(I)                                         
     *             +COS72*(BIB(I)+BIE(I))-COS36*(BIC(I)+BID(I)))        
     *             -(SIN72*(AIB(I)-AIE(I))+SIN36*(AIC(I)-AID(I))))      
               DJE(J) =                                                 
     *             S4*((AIA(I)                                          
     *             +COS72*(AIB(I)+AIE(I))-COS36*(AIC(I)+AID(I)))        
     *             +(SIN72*(BIB(I)-BIE(I))+SIN36*(BIC(I)-BID(I))))      
     *             +C4*((BIA(I)                                         
     *             +COS72*(BIB(I)+BIE(I))-COS36*(BIC(I)+BID(I)))        
     *             -(SIN72*(AIB(I)-AIE(I))+SIN36*(AIC(I)-AID(I))))      
               CJC(J) =                                                 
     *             C2*((AIA(I)                                          
     *             -COS36*(AIB(I)+AIE(I))+COS72*(AIC(I)+AID(I)))        
     *             -(SIN36*(BIB(I)-BIE(I))-SIN72*(BIC(I)-BID(I))))      
     *             -S2*((BIA(I)                                         
     *             -COS36*(BIB(I)+BIE(I))+COS72*(BIC(I)+BID(I)))        
     *             +(SIN36*(AIB(I)-AIE(I))-SIN72*(AIC(I)-AID(I))))      
               DJC(J) =                                                 
     *             S2*((AIA(I)                                          
     *             -COS36*(AIB(I)+AIE(I))+COS72*(AIC(I)+AID(I)))        
     *             -(SIN36*(BIB(I)-BIE(I))-SIN72*(BIC(I)-BID(I))))      
     *             +C2*((BIA(I)                                         
     *             -COS36*(BIB(I)+BIE(I))+COS72*(BIC(I)+BID(I)))        
     *             +(SIN36*(AIB(I)-AIE(I))-SIN72*(AIC(I)-AID(I))))      
               CJD(J) =                                                 
     *             C3*((AIA(I)                                          
     *             -COS36*(AIB(I)+AIE(I))+COS72*(AIC(I)+AID(I)))        
     *             +(SIN36*(BIB(I)-BIE(I))-SIN72*(BIC(I)-BID(I))))      
     *             -S3*((BIA(I)                                         
     *             -COS36*(BIB(I)+BIE(I))+COS72*(BIC(I)+BID(I)))        
     *             -(SIN36*(AIB(I)-AIE(I))-SIN72*(AIC(I)-AID(I))))      
               DJD(J) =                                                 
     *             S3*((AIA(I)                                          
     *             -COS36*(AIB(I)+AIE(I))+COS72*(AIC(I)+AID(I)))        
     *             +(SIN36*(BIB(I)-BIE(I))-SIN72*(BIC(I)-BID(I))))      
     *             +C3*((BIA(I)                                         
     *             -COS36*(BIB(I)+BIE(I))+COS72*(BIC(I)+BID(I)))        
     *             -(SIN36*(AIB(I)-AIE(I))-SIN72*(AIC(I)-AID(I))))      
               I = I+IC3                                                
               J = J+IC4                                                
 30         CONTINUE                                                    
            IB0 = IB0+IC1                                               
            JB0 = JB0+IC2                                               
 40      CONTINUE                                                       
         JB0 = JB0+JUMP                                                 
 50   CONTINUE                                                          
 60   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE DM7FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,          
     *                A1,B1,A2,B2,A3,B3,C1,D1,C2,D2,C3,D3)              
C                                                                       
C   GENERAL ODD PRIME FACTOR SUBROUTINE FOR MULTIPLE DISCRETE           
C   FOURIER TRANSFORM.                                                  
C                                                                       
      DOUBLE PRECISION A1(1),B1(1),A2(1),B2(1),A3(1),B3(1)              
      DOUBLE PRECISION C1(1),D1(1),C2(1),D2(1),C3(1),D3(1)              
      DOUBLE PRECISION T(1)                                             
      DOUBLE PRECISION SGN,T1,T2,TEMPI,TEMPR                            
      INTEGER IA,IAI,IB,IC1,IC2,IC3,IC4,IFAC,II,IINK,IJK,IM,IP,IPI      
      INTEGER IQ,IQI,IT,IZ,J,JA,JAJ,JINK,JJ,JP,JPJ,JQ,JQJ,JUMP          
      INTEGER K,KINK,KT,KU,KV,KX,L,LA,LINK,M,MINK,N,N2,NN,NNS           
C                                                                       
      M    = N/IFAC                                                     
      IINK = M*IC1                                                      
      JINK = LA*IC2                                                     
      JUMP = (IFAC-1)*JINK                                              
      IA   = 1                                                          
      JA   = 1                                                          
      KT   = M+M                                                        
      LINK = (IFAC-1)*IINK                                              
      MINK = LINK/2                                                     
      N2   = (N+1)/2                                                    
      NN   = N+N                                                        
      DO 140 K = 1,M,LA                                                 
         KINK = K+K-2                                                   
         DO 130 L = 1,LA                                                
            IB = IA+IINK                                                
            IM = IA+MINK                                                
            IZ = IA+LINK                                                
            IQ = IZ                                                     
            IAI = IA                                                    
            JAJ = JA                                                    
            DO 10 IJK = 1,NNS                                           
               C1(JAJ) = A1(IAI)                                        
               D1(JAJ) = B1(IAI)                                        
               IAI = IAI+IC3                                            
               JAJ = JAJ+IC4                                            
 10         CONTINUE                                                    
            DO 30 IP = IB,IM,IINK                                       
               IPI = IP                                                 
               IQI = IQ                                                 
               JAJ = JA                                                 
               DO 20 IJK = 1,NNS                                        
                  TEMPR = A1(IPI)+A1(IQI)                               
                  A2(IQI) = A1(IPI)-A1(IQI)                             
                  A3(IPI) = TEMPR                                       
                  C1(JAJ) = C1(JAJ)+TEMPR                               
                  TEMPI = B1(IPI)+B1(IQI)                               
                  B2(IQI) = B1(IPI)-B1(IQI)                             
                  B3(IPI) = TEMPI                                       
                  D1(JAJ) = D1(JAJ)+TEMPI                               
                  IPI = IPI+IC3                                         
                  IQI = IQI+IC3                                         
                  JAJ = JAJ+IC4                                         
 20            CONTINUE                                                 
               IQ = IQ-IINK                                             
 30         CONTINUE                                                    
            JP = JA+JINK                                                
            JQ = JA+JUMP                                                
            KU = KT                                                     
            DO 90 II = IB,IM,IINK                                       
               IAI = IA                                                 
               JPJ = JP                                                 
               JQJ = JQ                                                 
               DO 40 IJK = 1,NNS                                        
                  C2(JPJ) = A1(IAI)                                     
                  D2(JPJ) = B1(IAI)                                     
                  C3(JQJ) = A1(IAI)                                     
                  D3(JQJ) = B1(IAI)                                     
                  IAI = IAI+IC3                                         
                  JPJ = JPJ+IC4                                         
                  JQJ = JQJ+IC4                                         
 40            CONTINUE                                                 
               KV = KU                                                  
               IQ = IZ                                                  
               DO 80 IP = IB,IM,IINK                                    
                  KX = MOD(KV,NN)                                       
                  IF(KX.LE.N) GO TO 50                                  
                     KX = (NN - KX)*IT                                  
                     T1 = T(KX+1)                                       
                     T2 = -SGN*T(KX+2)                                  
                     GO TO 60                                           
 50               CONTINUE                                              
                     KX = KX*IT                                         
                     T1 = T(KX+1)                                       
                     T2 = SGN*T(KX+2)                                   
 60               CONTINUE                                              
                  IPI = IP                                              
                  IQI = IQ                                              
                  JPJ = JP                                              
                  JQJ = JQ                                              
                  DO 70 IJK = 1,NNS                                     
                     C2(JPJ) = C1(JPJ)+A1(IPI)*T1                       
     *                                -B1(IQI)*T2                       
                     D2(JPJ) = D1(JPJ)+B1(IPI)*T1                       
     *                                +A1(IQI)*T2                       
                     C3(JQJ) = C1(JQJ)+A1(IPI)*T1                       
     *                                +B1(IQI)*T2                       
                     D3(JQJ) = D1(JQJ)+B1(IPI)*T1                       
     *                                -A1(IQI)*T2                       
                     IPI = IPI+IC3                                      
                     IQI = IQI+IC3                                      
                     JPJ = JPJ+IC4                                      
                     JQJ = JQJ+IC4                                      
 70               CONTINUE                                              
                  KV = KV+KU                                            
                  IQ = IQ-IINK                                          
 80            CONTINUE                                                 
               JP = JP+JINK                                             
               JQ = JQ-JINK                                             
               KU = KU+KT                                               
 90         CONTINUE                                                    
            IA = IA+IC1                                                 
            IF(KINK.EQ.0)GO TO 120                                      
            KU = KINK                                                   
            JP = JA+JINK                                                
            JQ = JA+JUMP                                                
            DO 110 JJ = JP,JQ,JINK                                      
               J = JJ                                                   
               KX = KU*IT                                               
               T1 = T(KX+1)                                             
               T2 = SGN*T(KX+2)                                         
               DO 100 IJK = 1,NNS                                       
                  TEMPR = C1(J)*T1-D1(J)*T2                             
                  TEMPI = C1(J)*T2+D1(J)*T1                             
                  C2(J) = TEMPR                                         
                  D2(J) = TEMPI                                         
                  J = J+IC4                                             
 100           CONTINUE                                                 
               KU = KU+KINK                                             
 110        CONTINUE                                                    
 120        JA = JA+IC2                                                 
 130     CONTINUE                                                       
         JA = JA+JUMP                                                   
 140  CONTINUE                                                          
      RETURN                                                            
      END                                                               
         SUBROUTINE DM33FT(N,NNS,W1,W2,W3,W4,IW,JW,Y1,Y2,IY,JY,T,SGN)   
C                                                                       
C   THIS SUBROUTINE PERFORMS THE LAST PASS THROUGH DATA FOR A           
C   MULTIPLE DISCRETE FOURIER TRANSFORM, FOR REAL INPUT DATA.           
C   IT RELIES ON THE METHOD OF COOLEY, TUKEY, LEWIS, AND WELCH.         
C                                                                       
         DOUBLE PRECISION W1(1),W2(1),W3(1),W4(1)                       
         DOUBLE PRECISION Y1(1),Y2(1)                                   
         DOUBLE PRECISION T(2,1)                                        
         DOUBLE PRECISION QI,QR,RI,RR,SGN,T1,T2                         
         INTEGER I,I1,I3,I4,IA,IB,IW,IY,J,JW,JY,N,N2,NNS                
C                                                                       
C**SET LAST ELEMENTS OF W                                               
C                                                                       
         IA  = 1                                                        
         IB  = (N/2)*IW + 1                                             
         DO 10 J = 1,NNS                                                
            W3(IB) = W1(IA)                                             
            W4(IB) = W2(IA)                                             
            IA = IA + JW                                                
            IB = IB + JW                                                
 10      CONTINUE                                                       
C                                                                       
C**COOLEY, TUKEY, LEWIS, AND WELCH                                      
C                                                                       
         N2  = N/2 + 1                                                  
         DO 30 J = 1,N2                                                 
            I1  = (J-1)*IY + 1                                          
            I3  = (J-1)*IW + 1                                          
            I4  = (N2-J)*IW + 1                                         
            T1  = T(1,J)                                                
            T2  = SGN*T(2,J)                                            
            DO 20 I = 1,NNS                                             
               QR     = 0.5D0*(W1(I3) + W1(I4))                         
               QI     = 0.5D0*(W2(I3) - W2(I4))                         
               RR     = 0.5D0*(W2(I3) + W2(I4))                         
               RI     = -0.5D0*(W1(I3) - W1(I4))                        
               Y1(I1) = QR + T1*RR - T2*RI                              
               Y2(I1) = QI + T1*RI + T2*RR                              
               I1     = I1 + JY                                         
               I3     = I3 + JW                                         
               I4     = I4 + JW                                         
 20         CONTINUE                                                    
 30      CONTINUE                                                       
         RETURN                                                         
         END                                                            
         SUBROUTINE DM44FT(N,NNS,A,B,IAB,JAB,W1,W2,IW,JW,T,SGN)         
C                                                                       
C   THIS SUBROUTINE PERFORMS A PRE-PROCESSING PASS IN A MULTIPLE        
C   DISCRETE FOURIER TRANSFORM, FOR HALF-COMPLEX INPUT DATA. IT         
C   RELIES ON THE METHOD OF COOLEY, TUKEY, LEWIS, AND WELCH.            
C                                                                       
         DOUBLE PRECISION A(1),B(1),W1(1),W2(1),T(2,1)                  
         DOUBLE PRECISION GI,GR,HI,HR,SGN,T1,T2                         
         INTEGER I,IAB,IAM,IAP,IW,IW1,J,JAB,JW,N,N2,NNS                 
C**COOLEY, TUKEY, LEWIS, AND WELCH                                      
         N2  = N/2+1                                                    
         DO 20 J = 1,N2                                                 
            IAP = (J-1)*IAB + 1                                         
            IAM = (N2-J)*IAB + 1                                        
            IW1 = (J-1)*IW + 1                                          
            T1  = T(1,J)                                                
            T2  = SGN*T(2,J)                                            
            DO 10 I = 1,NNS                                             
               HR      = A(IAP) + A(IAM)                                
               HI      = B(IAP) - B(IAM)                                
               GR      = B(IAP) + B(IAM)                                
               GI      = A(IAM) - A(IAP)                                
               W1(IW1) = HR - T1*GR + T2*GI                             
               W2(IW1) = HI - T2*GR - T1*GI                             
               IAP     = IAP + JAB                                      
               IAM     = IAM + JAB                                      
               IW1     = IW1 + JW                                       
 10         CONTINUE                                                    
 20      CONTINUE                                                       
C                                                                       
         RETURN                                                         
         END                                                            
      SUBROUTINE DM66FT(N,IFX)                                          
C                                                                       
C   THIS SUBROUTINE FACTORS AN INTEGER N INTO FACTORS OF 2,3,5 AND      
C   GENERAL PRIME FACTORS (GREATER THAN 5). ON EXIT, THE CONTENTS OF    
C   INTEGER ARRAY IFX() ARE                                             
C                                                                       
C               IFX(1) = N                                              
C               IFX(2) = M THE NUMBER OF FACTORS                        
C               IFX(3) THROUGH IFX(M+2) ARE THE FACTORS                 
C                                                                       
C   FOR EXAMPLE, IF N = 30 - IFX(1) = 30, IFX(2) = 3, IFX(3) = 2,       
C   IFX(4) = 3, AND IFX(5) = 5.                                         
C                                                                       
C   THIS IS A LOW LEVEL ROUTINE WITH NO ERROR CONDITIONS.               
C                                                                       
C/6S                                                                    
C     INTEGER IFX(1)                                                    
C/7S                                                                    
      INTEGER IFX(*)                                                    
C/                                                                      
      INTEGER I,II,IL,IT,K,L,N,NL,NN                                    
C                                                                       
      NN=N                                                              
      IFX(1)=NN                                                         
      IF (NN .LE. 1) GO TO 100                                          
      K=2                                                               
C  FACTORS OF 2                                                         
 10   IF (MOD(NN,2).NE.0) GO TO 20                                      
         K=K+1                                                          
         IFX(K)=2                                                       
         NN=NN/2                                                        
      IF (NN.EQ.1) GO TO 60                                             
         GO TO 10                                                       
C  FACTORS OF 3                                                         
 20   IF (MOD(NN,3).NE.0) GO TO 30                                      
         K=K+1                                                          
         IFX(K)=3                                                       
         NN=NN/3                                                        
      IF (NN.EQ.1) GO TO 60                                             
         GO TO 20                                                       
C  REMAINING ODD FACTORS                                                
 30   L=5                                                               
      I=2                                                               
C  I IS ALTERNATELY 2 OR 4                                              
 40   IF (MOD(NN,L).NE.0) GO TO 50                                      
         K=K+1                                                          
         IFX(K)=L                                                       
         NN=NN/L                                                        
      IF (NN.EQ.1) GO TO 60                                             
         GO TO 40                                                       
 50   L=L+I                                                             
      I=6-I                                                             
         GO TO 40                                                       
 60   IFX(2)=K-2                                                        
C  NFAX IS SET TO THE NUMBER OF FACTORS                                 
      NL=K-2                                                            
C  SORT FACTORS IN ASCENDING ORDER                                      
      IF (NL.EQ.1) GO TO 90                                             
      DO 80 II=2,NL                                                     
         IL=NL+3-II                                                     
         DO 70 I=3,IL                                                   
            IF (IFX(I+1).GE.IFX(I)) GO TO 70                            
            IT=IFX(I)                                                   
            IFX(I)=IFX(I+1)                                             
            IFX(I+1)=IT                                                 
 70      CONTINUE                                                       
 80   CONTINUE                                                          
 90   CONTINUE                                                          
      GO TO 999                                                         
 100  IFX(2) = 0                                                        
 999  RETURN                                                            
      END                                                               
         SUBROUTINE DM88FT(N,NNS,A,B,IAB,JAB,C,D,ICD,JCD)               
C                                                                       
C   THIS ROUTINE IS A DOUBLE ARRAY COPY FOR MULTIPLE FOURIER            
C   TRANSFORM PACKAGE - MFT..                                           
C                                                                       
         DOUBLE PRECISION A(1),B(1),C(1),D(1)                           
         INTEGER I,IAB,ICD,J,J1,J2,JAB,JCD,N,NNS                        
C                                                                       
         DO 1 J = 1,NNS                                                 
            J1 = (J-1)*JCD+1                                            
            J2 = (J-1)*JAB+1                                            
            DO 2 I = 1,N                                                
               C(J1) = A(J2)                                            
               D(J1) = B(J2)                                            
               J1 = J1 + ICD                                            
               J2 = J2 + IAB                                            
 2          CONTINUE                                                    
 1       CONTINUE                                                       
         RETURN                                                         
         END                                                            
      SUBROUTINE FFTR(NNP2,A,B)                                         
C                                                                       
C FFTC FINDS THE FOURIER TRANSFORM OF THE 2N REAL DATA                  
C POINTS STORED IN THE REAL ARRAY, A, OF DIMENSION N PLUS 2,            
C BY SIMPLY SETTING UP THE CORRECT CALLS TO FFT AND RLTR.               
C                                                                       
C STORAGE IN THE DYNAMIC STORAGE STACK IS REQUIRED FOR                  
C 220 INTEGER VARIABLES AND 92 REAL VARIABLES.                          
C                                                                       
C     INPUT                                                             
C                                                                       
C       NNP2  -FOR 2N INPUT POINTS, NNP2 MUST BE INPUT AS 2*N+2         
C              NOTE THAT THE NUMBER OF INPUT POINTS MUST BE EVEN.       
C       A     -A VECTOR OF LENGTH NNP2 CONTAINING THE 2N REAL           
C              DATA IN THE FIRST 2N LOCATIONS                           
C                                                                       
C     OUTPUT                                                            
C                                                                       
C       A     -THE FIRST N+1 LOCATIONS OF A CONTAIN THE REAL (COSINE)   
C              COMPONENTS OF THE FOURIER COEFFICIENTS                   
C       B     -A VECTOR OF LENGTH N+1 CONTAINING THE N+1 IMAGINARY      
C              (SINE) COMPONENTS OF THE FOURIER COEFFICIENTS            
C              ( WITH B(N+1) = 0.)                                      
C                                                                       
C                                                                       
C NUMBER OF FACTORS OF N MUST NOT EXCEED 11.                            
C MAXIMUM PRIME FACTOR OF N MUST NOT EXCEED 23.                         
C PRODUCT OF THE SQUARE-FREE FACTORS, IF THERE EXISTS MORE THAN ONE,    
C MUST NOT EXCEED 210.                                                  
C                                                                       
C                                                                       
C     ERROR STATES                                                      
C          1.  NNP2 (2N+2) IS LESS THAN 6                               
C          2.  NNP2 IS NOT EVEN                                         
C          3.  PRIME FACTOR .GT. 23                                     
C              (RECOVERABLE)                                            
C          4.  SQUARE-FREE FACTOR PRODUCT .GT. 210                      
C              (RECOVERABLE)                                            
C                                                                       
C                                                                       
C COMMON AREA                                                           
      COMMON/CSTAK/DSTAK(500)                                           
C                                                                       
      REAL A(2),B(2)                                                    
      DOUBLE PRECISION DSTAK                                            
C                                                                       
C  TEST THE VALIDITY OF THE INPUTS                                      
C                                                                       
C/6S                                                                    
C     IF(NNP2 .LT. 6) CALL SETERR(                                      
C    1   27H FFTR - NNP2 IS LESS THAN 6,27,1,2)                         
C/7S                                                                    
      IF(NNP2 .LT. 6) CALL SETERR(                                      
     1   ' FFTR - NNP2 IS LESS THAN 6',27,1,2)                          
C/                                                                      
      J = NNP2                                                          
C/6S                                                                    
C     IF (J/2*2 .NE. J) CALL SETERR(                                    
C    1   34H FFTR - NNP2 IS NOT AN EVEN NUMBER,34,2,2)                  
C/7S                                                                    
      IF (J/2*2 .NE. J) CALL SETERR(                                    
     1   ' FFTR - NNP2 IS NOT AN EVEN NUMBER',34,2,2)                   
C/                                                                      
C                                                                       
C ENTER THE RECOVERY MODE (STORING THE PREVIOUS)                        
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C  CALL THE SUBPROGRAMS, FFT AND RLTR                                   
C                                                                       
      J = NNP2/2 - 1                                                    
      CALL FFT(A,A(2),J,J,J,2)                                          
      CALL RLTR(A,A(2),J,2)                                             
C                                                                       
C                                                                       
C  CHECK FOR ERRORS FROM FFT                                            
C                                                                       
      IF (NERROR(NERR) .EQ. 0) GO TO 10                                 
C                                                                       
      CALL ERROFF                                                       
C/6S                                                                    
C     IF (NERR .EQ. 5) CALL SETERR(                                     
C    1   36H FFTR - PRIME FACTOR GREATER THAN 23,36,3,1)                
C/7S                                                                    
      IF (NERR .EQ. 5) CALL SETERR(                                     
     1   ' FFTR - PRIME FACTOR GREATER THAN 23',36,3,1)                 
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 6) CALL SETERR(                                     
C    1   44H FFTR - SQUARE-FREE PRODUCT GREATER THAN 210,44,4,1)        
C/7S                                                                    
      IF (NERR .EQ. 6) CALL SETERR(                                     
     1   ' FFTR - SQUARE-FREE PRODUCT GREATER THAN 210',44,4,1)         
C/                                                                      
C                                                                       
      GO TO 40                                                          
C                                                                       
 10   CONTINUE                                                          
C  FOR CONVENIENCE TO THE USER, PUT THE REAL COEFFICIENTS,              
C  WHICH RLTR STORES IN THE LOCATIONS A(1), A(3),...,A(2N+1)            
C  INTO THE BEGINNING OF THE A ARRAY, AFTER FIRST PUTTING               
C  THE IMAGINARY COEFFICIENTS, RETURNED BY RLTR IN LOCATIONS            
C  A(2),A(4),...,A(2N+2), INTO THE ARRAY B.                             
C                                                                       
      J = 2                                                             
      KK = NNP2/2                                                       
      DO 20 K=1,KK                                                      
      B(K) = A(J)                                                       
      J = J+2                                                           
 20   CONTINUE                                                          
C                                                                       
      J=3                                                               
      DO 30 K=2,KK                                                      
      A(K) = A(J)                                                       
      J = J+2                                                           
 30   CONTINUE                                                          
C                                                                       
 40   CALL LEAVE                                                        
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE FFTRI(NN,FR,FI)                                        
C                                                                       
C FFTCI FINDS THE INVERSE FOURIER TRANSFORM BY CALLING                  
C RLTR AND FFT WITH APPROPRIATE INFORMATION.                            
C THE RESULT OF THE INVERSE TRANSFORM IS EXPECTED TO BE                 
C REAL, AND TO CONTAIN AN EVEN NUMBER 2N, OF DATA POINTS.               
C THE RESULT OF THE INVERSE TRANSFORM WILL BE PLACED, BY                
C FFTRI, IN THE FIRST ARRAY, FR, WHICH MUST THEREFORE                   
C BE DIMENSIONED TO AT LEAST 2N, ALTHOUGH ONLY N+1 FOURIER              
C COEFFICIENTS ARE INPUT IN THE ARRAYS, FR AND FI.                      
C                                                                       
C STORAGE IN THE DYNAMIC STORAGE STACK IS REQUIRED FOR                  
C 220 INTEGER VARIABLES AND 92 REAL VARIABLES.                          
C                                                                       
C     INPUT                                                             
C                                                                       
C       NN    -2N, THE NUMBER OF REAL DATA POINTS                       
C              RESULTING FROM THE REQUESTED INVERSE TRANSFORM           
C       FR    -A VECTOR OF LENGTH 2N CONTAINING, IN ITS FIRST N+1       
C              LOCATIONS, THE REAL PART OF THE FOURIER COEFFICIENTS     
C       FI    -A VECTOR OF LENGTH N+1 CONTAINING THE IMAGINARY          
C              PART OF THE FOURIER COEFFICIENTS                         
C                                                                       
C     OUTPUT                                                            
C                                                                       
C       FR    -THE 2N REAL DATA RESULTING FROM THE INVERSE TRANSFORM    
C                                                                       
C NUMBER OF FACTORS OF N MUST NOT EXCEED 11.                            
C MAXIMUM PRIME FACTOR OF N MUST NOT EXCEED 23.                         
C PRODUCT OF THE SQUARE-FREE FACTORS, IF THERE EXISTS MORE THAN ONE,    
C MUST NOT EXCEED 210.                                                  
C                                                                       
C                                                                       
C     ERROR STATES                                                      
C          1.  NN (2N) IS LESS THAN 4                                   
C          2.  NN IS NOT EVEN                                           
C          3.  PRIME FACTOR .GT. 23                                     
C              (RECOVERABLE)                                            
C          4.  SQUARE-FREE FACTOR PRODUCT .GT. 210                      
C              (RECOVERABLE)                                            
C                                                                       
C                                                                       
C COMMON AREA                                                           
      COMMON/CSTAK/DSTAK(500)                                           
C                                                                       
      REAL FR(1),FI(1)                                                  
      DOUBLE PRECISION DSTAK                                            
C                                                                       
C  TEST THE VALIDITY OF THE INPUTS                                      
C                                                                       
C/6S                                                                    
C     IF(NN .LT. 4) CALL SETERR(                                        
C    1   26H FFTRI - NN IS LESS THAN 4,26,1,2)                          
C/7S                                                                    
      IF(NN .LT. 4) CALL SETERR(                                        
     1   ' FFTRI - NN IS LESS THAN 4',26,1,2)                           
C/                                                                      
      J = NN                                                            
C/6S                                                                    
C     IF (J/2*2 .NE. J) CALL SETERR(                                    
C    1   33H FFTRI - NN IS NOT AN EVEN NUMBER,33,2,2)                   
C/7S                                                                    
      IF (J/2*2 .NE. J) CALL SETERR(                                    
     1   ' FFTRI - NN IS NOT AN EVEN NUMBER',33,2,2)                    
C/                                                                      
C                                                                       
C ENTER THE RECOVERY MODE (STORING THE PREVIOUS)                        
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C  CALL THE SUBPROGRAMS, FFT AND RLTR                                   
C                                                                       
      N = NN/2                                                          
      CALL RLTR(FR,FI,N,-1)                                             
      CALL FFT(FR,FI,N,N,N,-1)                                          
C                                                                       
C                                                                       
C  CHECK FOR ERRORS FROM FFT                                            
C                                                                       
      IF (NERROR(NERR) .EQ. 0) GO TO 10                                 
C                                                                       
      CALL ERROFF                                                       
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 5) CALL SETERR(                                     
C    1   37H FFTRI - PRIME FACTOR GREATER THAN 23,37,3,1)               
C/7S                                                                    
      IF (NERR .EQ. 5) CALL SETERR(                                     
     1   ' FFTRI - PRIME FACTOR GREATER THAN 23',37,3,1)                
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 6) CALL SETERR(                                     
C    1   45H FFTRI - SQUARE-FREE PRODUCT GREATER THAN 210,45,4,1)       
C/7S                                                                    
      IF (NERR .EQ. 6) CALL SETERR(                                     
     1   ' FFTRI - SQUARE-FREE PRODUCT GREATER THAN 210',45,4,1)        
C/                                                                      
C                                                                       
      GO TO 30                                                          
C                                                                       
 10   CONTINUE                                                          
C                                                                       
C THE TIME DOMAIN RESULTS, RETURNED BY THIS PROCESS, ALTERNATE          
C IN THE VECTORS FR AND FI AS FR(1),FI(1),FR(2),FI(2),...FR(N),FI(N).   
C THE FOLLOWING FRAGMENT RECOMBINES THEM INTO FR                        
C                                                                       
      K = N                                                             
      DO 20 KK=1,N                                                      
      FR(2*K) = FI(K)                                                   
      FR(2*K-1) = FR(K)                                                 
 20   K = K-1                                                           
 30   CALL LEAVE                                                        
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE FFTC(N,AR,AI)                                          
C                                                                       
C FFTC FINDS THE FOURIER TRANSFORM OF THE COMPLEX                       
C DATA STORED IN THE REAL AND IMAGINARY ARRAYS, AR AND AI               
C BY SIMPLY SETTING UP THE CORRECT CALL TO FFT.                         
C                                                                       
C STORAGE IN THE DYNAMIC STORAGE STACK IS REQUIRED FOR                  
C 220 INTEGER VARIABLES AND 92 REAL VARIABLES.                          
C                                                                       
C     INPUT                                                             
C                                                                       
C       N     -NUMBER OF COMPLEX DATA POINTS                            
C       AR    -VECTOR OF LENGTH N CONTAINING REAL COMPONENTS            
C       AI    -VECTOR OF LENGTH N CONTAINING IMAGINARY COMPONENTS       
C                                                                       
C     OUTPUT                                                            
C                                                                       
C       AR    -CONTAINS THE REAL (COSINE)                               
C              COMPONENTS OF THE FOURIER COEFFICIENTS                   
C       AI    -CONTAINS THE IMAGINARY (SINE)                            
C              COMPONENTS OF THE FOURIER COEFFICIENTS                   
C                                                                       
C                                                                       
C NUMBER OF FACTORS OF N MUST NOT EXCEED 11.                            
C MAXIMUM PRIME FACTOR OF N MUST NOT EXCEED 23.                         
C PRODUCT OF THE SQUARE-FREE FACTORS, IF THERE EXISTS MORE THAN ONE,    
C MUST NOT EXCEED 210.                                                  
C                                                                       
C                                                                       
C     ERROR STATES                                                      
C          1.  N IS LESS THAN 2                                         
C          2.  PRIME FACTOR .GT. 23                                     
C              (RECOVERABLE)                                            
C          3.  SQUARE-FREE FACTOR PRODUCT .GT. 210                      
C              (RECOVERABLE)                                            
C                                                                       
C                                                                       
C COMMON AREA                                                           
      COMMON/CSTAK/DSTAK(500)                                           
C                                                                       
      REAL AR(1),AI(1)                                                  
      DOUBLE PRECISION DSTAK                                            
C                                                                       
C  TEST THE VALIDITY OF THE INPUTS                                      
C                                                                       
C/6S                                                                    
C     IF(N .LT. 2) CALL SETERR(                                         
C    1   24H FFTC - N IS LESS THAN 2,24,1,2)                            
C/7S                                                                    
      IF(N .LT. 2) CALL SETERR(                                         
     1   ' FFTC - N IS LESS THAN 2',24,1,2)                             
C/                                                                      
C ENTER THE RECOVERY MODE (STORING THE PREVIOUS)                        
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C  CALL THE SUBPROGRAM, FFT.                                            
C                                                                       
      CALL FFT(AR,AI,N,N,N,1)                                           
C                                                                       
C  CHECK FOR ERRORS FROM FFT                                            
C                                                                       
      IF (NERROR(NERR) .EQ. 0) GO TO 10                                 
C                                                                       
      CALL ERROFF                                                       
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 5) CALL SETERR(                                     
C    1   36H FFTC - PRIME FACTOR GREATER THAN 23,36,2,1)                
C/7S                                                                    
      IF (NERR .EQ. 5) CALL SETERR(                                     
     1   ' FFTC - PRIME FACTOR GREATER THAN 23',36,2,1)                 
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 6) CALL SETERR(                                     
C    1   44H FFTC - SQUARE-FREE PRODUCT GREATER THAN 210,44,3,1)        
C/7S                                                                    
      IF (NERR .EQ. 6) CALL SETERR(                                     
     1   ' FFTC - SQUARE-FREE PRODUCT GREATER THAN 210',44,3,1)         
C/                                                                      
C                                                                       
 10   CALL LEAVE                                                        
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE FFTCI(N,FR,FI)                                         
C                                                                       
C FFTCI FINDS THE INVERSE FOURIER TRANSFORM BY                          
C CALLING FFT TO INVERT THE TRANSFORM WHOSE REAL                        
C AND COMPLEX FOURIER COEFFICIENTS ARE PROVIDED                         
C IN THE ARRAYS FR AND FI.                                              
C                                                                       
C STORAGE IN THE DYNAMIC STORAGE STACK IS REQUIRED FOR                  
C 220 INTEGER VARIABLES AND 92 REAL VARIABLES.                          
C                                                                       
C     INPUT                                                             
C                                                                       
C       N     -NUMBER OF COEFFICIENTS                                   
C       FR    -VECTOR OF LENGTH N CONTAINING THE REAL COEFFICIENTS      
C       FI    -VECTOR OF LENGTH N CONTAINING THE IMAGINARY COEFFICIENTS 
C                                                                       
C     OUTPUT                                                            
C                                                                       
C       FR    -CONTAINS THE REAL COMPONENTS OF THE INVERSE TRANSFORM    
C       FI    -CONTAINS THE IMAGINARY COMPONENTS                        
C                                                                       
C                                                                       
C NUMBER OF FACTORS OF N MUST NOT EXCEED 11.                            
C MAXIMUM PRIME FACTOR OF N MUST NOT EXCEED 23.                         
C PRODUCT OF THE SQUARE-FREE FACTORS, IF THERE EXISTS MORE THAN ONE,    
C MUST NOT EXCEED 210.                                                  
C                                                                       
C                                                                       
C     ERROR STATES                                                      
C          1.  N IS LESS THAN 2                                         
C          2.  PRIME FACTOR .GT. 23                                     
C              (RECOVERABLE)                                            
C          3.  SQUARE-FREE FACTOR PRODUCT .GT. 210                      
C              (RECOVERABLE)                                            
C                                                                       
C                                                                       
C COMMON AREA                                                           
      COMMON/CSTAK/DSTAK(500)                                           
C                                                                       
      REAL FR(1),FI(1)                                                  
      DOUBLE PRECISION DSTAK                                            
C                                                                       
C  TEST THE VALIDITY OF THE INPUTS                                      
C                                                                       
C/6S                                                                    
C     IF(N .LT. 2) CALL SETERR(                                         
C    1   25H FFTCI - N IS LESS THAN 2,25,1,2)                           
C/7S                                                                    
      IF(N .LT. 2) CALL SETERR(                                         
     1   ' FFTCI - N IS LESS THAN 2',25,1,2)                            
C/                                                                      
C ENTER THE RECOVERY MODE (STORING THE PREVIOUS)                        
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C  CALL THE SUBPROGRAM, FFT.                                            
C                                                                       
      CALL FFT(FR,FI,N,N,N,-1)                                          
C                                                                       
C  CHECK FOR ERRORS FROM FFT                                            
C                                                                       
      IF (NERROR(NERR) .EQ. 0) GO TO 10                                 
C                                                                       
      CALL ERROFF                                                       
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 5) CALL SETERR(                                     
C    1   37H FFTCI - PRIME FACTOR GREATER THAN 23,37,2,1)               
C/7S                                                                    
      IF (NERR .EQ. 5) CALL SETERR(                                     
     1   ' FFTCI - PRIME FACTOR GREATER THAN 23',37,2,1)                
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 6) CALL SETERR(                                     
C    1   45H FFTCI - SQUARE-FREE PRODUCT GREATER THAN 210,45,3,1)       
C/7S                                                                    
      IF (NERR .EQ. 6) CALL SETERR(                                     
     1   ' FFTCI - SQUARE-FREE PRODUCT GREATER THAN 210',45,3,1)        
C/                                                                      
C                                                                       
 10   CALL LEAVE                                                        
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE FFT(A,B,NTOT,N,NSPAN,ISN)                              
C                                                                       
C FFT SETS UP STACK STORAGE FOR F1FT (WHICH IS                          
C SINGLETON S ORIGINAL FFT), GOES INTO RECOVERY MODE,                   
C CALLS F1FT, AND ON RETURN CHECKS FOR, AND REINTERPRETS                
C ANY ERRORS THAT HAVE OCCURRED, THEN RESTORES THE                      
C PREVIOUS RECOVERY MODE AND RETURNS.                                   
C                                                                       
C FFT MAKES ERRORS IN THE CALLING PARAMETERS FATAL,                     
C BUT OTHER ERRORS RECOVERABLE, SINCE FFT, IN TURN, IS CALLED BY        
C OUTER LEVEL ROUTINES.                                                 
C                                                                       
C STORAGE IN THE DYNAMIC STORAGE STACK IS SET UP FOR                    
C NFAC(11),NP(209),AT(23),CK(23),BT(23),SK(23), REQUIRING               
C 220 INTEGER LOCATIONS AND 92 REAL LOCATIONS.                          
C                                                                       
C                                                                       
C                                                                       
C     INPUT                                                             
C                                                                       
C       A     -VECTOR OF SIZE NTOT CONTAINING REAL ELEMENTS OF DATA     
C       B     -VECTOR OF SIZE NTOT CONTAINING IMAGINARY ELEMENTS OF DATA
C      NTOT   -TOTAL NUMBER OF COMPLEX DATA POINTS                      
C       N     -NUMBER OF COMPLEX DATA POINTS OF THE CURRENT VARIABLE    
C     NSPAN  -NUMBER OF ELEMENTS OF BOTH A AND B NECESSARY TO SPAN      
C              ALL VALUES OF THE CURRENT VARIABLE                       
C              (FOR A SINGLE VARIATE TRANSFORM NTOT=N=NSPAN.  FOR A     
C              MULTIVARIATE TRANSFORM N AND NSPAN ARE DIFFERENT IN      
C              EACH CALL STATEMENT.  SEE EXAMPLE BELOW).                
C      ISN   -DETERMINES BY SIGN THE TYPE OF TRANSFORM BEING COMPUTED,  
C              FORWARD OR INVERSE                                       
C             -INDICATES BY ABSOLUTE VALUE THE VECTOR ARRANGEMENT OF    
C              INPUT DATA (AND TRANSFORM OUTPUT)                        
C             =+1 COMPLEX INPUT DATA ARE IN TWO VECTORS, A AND B.       
C              REAL COMPONENTS IN A, IMAGINARY COMPONENTS IN B.         
C             =-1 FOURIER COEFFICIENT INPUT VALUES ARE IN TWO VECTORS,  
C              A AND B.  COSINE VALUES IN A, SINE VALUES IN B.          
C             =+2 COMPLEX INPUT DATA ARE STORED ALTERNATELY IN A        
C              SINGLE COMPLEX VECTOR, A.  REAL VALUES ARE               
C              IN A(1),A(3),... AND IMAGINARY VALUES ARE                
C              IN A(2),A(4),....  SECOND ARGUMENT OF                    
C              FFT SHOULD BE A(2).  (SEE EXAMPLE.)                      
C             =-2 FOURIER COEFFICIENT INPUT VALUES ARE STORED ALTER-    
C              NATELY IN A SINGLE VECTOR, A.  COSINE VALUES ARE IN      
C              A(1),A(3),... AND SINE VALUES ARE IN A(2),A(4),....      
C              SECOND ARGUMENT OF FFT SHOULD BE A(2).                   
C                                                                       
C                                                                       
C     OUTPUT                                                            
C                                                                       
C       A     -VECTOR OF SIZE NTOT CONTAINING COSINE COMPONENTS OF      
C              FOURIER COEFFICIENTS                                     
C       B     -VECTOR OF SIZE NTOT CONTAINING SINE COMPONENTS OF FOURIER
C              COEFFICIENTS                                             
C                                                                       
C IF ISN=1 MULTIPLY OUTPUT BY 2/N FOR UNIT MAGNITUDE.                   
C                                                                       
C IF ISN=2 OUTPUT WILL ALTERNATE IN A SINGLE VECTOR, A.  COSINE         
C COEFFICIENTS IN A(1),A(3),... AND SINE COEFFICIENTS IN A(2),A(4),.... 
C OUTPUT SHOULD BE MULTIPLIED BY 4/N FOR UNIT MAGNITUDE.                
C                                                                       
C                                                                       
C NUMBER OF FACTORS OF N MUST NOT EXCEED 11.                            
C MAXIMUM PRIME FACTOR OF N MUST NOT EXCEED 23.                         
C PRODUCT OF THE SQUARE-FREE FACTORS, IF THERE EXISTS MORE THAN ONE,    
C MUST NOT EXCEED 210.                                                  
C                                                                       
C                                                                       
C     ERROR STATES                                                      
C          1.  N IS LESS THAN 2                                         
C          2.  NTOT IS LESS THAN N                                      
C          3.  NSPAN IS LESS THAN N                                     
C          4.  ABS(ISN) IS GREATER THAN 2                               
C          5.  PRIME FACTOR .GT. 23                                     
C              (RECOVERABLE)                                            
C          6.  SQUARE-FREE FACTOR PRODUCT .GT. 210                      
C              (RECOVERABLE)                                            
C                                                                       
C                                                                       
C EXAMPLES -                                                            
C                                                                       
C TRANSFORM OF N COMPLEX DATA VALUES STORED IN TWO VECTORS -            
C                CALL FFT(A,B,N,N,N,1)                                  
C                                                                       
C INVERSE TRANSFORM OF FOURIER COEFFICIENTS STORED IN TWO VECTORS -     
C                CALL FFT(A,B,N,N,N,-1)                                 
C                                                                       
C TRANSFORM OF N COMPLEX DATA VALUES STORED ALTERNATELY IN A SINGLE     
C COMPLEX VECTOR -                                                      
C                CALL FFT(A,A(2),2*N,N,2*N,2)                           
C                                                                       
C INVERSE TRANSFORM OF N FOURIER COEFFICIENTS, COSINE AND SINE COMPO-   
C NENTS STORED ALTERNATELY IN A SINGLE VECTOR -                         
C                CALL FFT(A,A(2),2*N,N,2*N,-2)                          
C                                                                       
C FOR A MULTIVARIATE TRANSFORM THERE IS NO LIMIT ON THE NUMBER OF       
C IMPLIED SUBSCRIPTS.  FFT SHOULD BE CALLED ONCE FOR EACH VARIABLE      
C AND THE CALLS MAY BE MADE IN ANY ORDER.                               
C                                                                       
C TRANSFORM OF A TRI-VARIATE, A(N1,N2,N3), B(N1,N2,N3) -                
C                NTOT=N1*N2*N3                                          
C                CALL FFT(A,B,NTOT,N1,N1,1)                             
C                CALL FFT(A,B,NTOT,N2,N1*N2,1)                          
C                CALL FFT(A,B,NTOT,N3,NTOT,1)                           
C                                                                       
C                                                                       
C FOR TRANSFORM OF COMPLETELY REAL DATA USE RLTR IN CONJUNCTION WITH    
C FFT.                                                                  
C                                                                       
C                                                                       
C OTHER ROUTINES CALLED -  SETERR                                       
C                                                                       
C                                                                       
C THE PORT LIBRARY VERSION OF FFT IS AS SHOWN IN THE SINGLETON          
C REFERENCE EXCEPT FOR CHANGES IN THE ERROR HANDLING.                   
C                                                                       
C                                                                       
C REFERENCE-  SINGLETON, R. C.,  AN ALGORITHM FOR COMPUTING THE MIXED   
C             RADIX FAST FOURIER TRANSFORM , IEEE TRANSACTIONS ON AUDIO 
C             AND ELECTROACOUSTICS, VOL. AU-17, NO. 2, JUNE, 1969       
C             PP. 93-103.                                               
C                                                                       
C COMMON AREA                                                           
      COMMON/CSTAK/DSTAK(500)                                           
C                                                                       
      INTEGER ISTAK(1000)                                               
      REAL RSTAK(1000)                                                  
      REAL A(1),B(1)                                                    
      DOUBLE PRECISION DSTAK                                            
C                                                                       
      EQUIVALENCE (DSTAK(1),ISTAK(1))                                   
      EQUIVALENCE (DSTAK(1),RSTAK(1))                                   
C                                                                       
C  THE FOLLOWING TWO CONSTANTS SHOULD AGREE WITH THE ARRAY DIMENSIONS.  
C                                                                       
C                                                                       
C  TEST THE VALIDITY OF THE INPUTS                                      
C                                                                       
C/6S                                                                    
C     IF(N .LT. 2) CALL SETERR(                                         
C    1   23H FFT - N IS LESS THAN 2,23,1,2)                             
C     IF(NTOT .LT. N) CALL SETERR(                                      
C    1   26H FFT - NTOT IS LESS THAN N,26,2,2)                          
C     IF(NSPAN .LT. N) CALL SETERR(                                     
C    1   27H FFT - NSPAN IS LESS THAN N,27,3,2)                         
C     IF(IABS(ISN) .GT. 2) CALL SETERR(                                 
C    1   39H FFT - ISN HAS MAGNITUDE GREATER THAN 2,39,4,2)             
C/7S                                                                    
      IF(N .LT. 2) CALL SETERR(                                         
     1   ' FFT - N IS LESS THAN 2',23,1,2)                              
      IF(NTOT .LT. N) CALL SETERR(                                      
     1   ' FFT - NTOT IS LESS THAN N',26,2,2)                           
      IF(NSPAN .LT. N) CALL SETERR(                                     
     1   ' FFT - NSPAN IS LESS THAN N',27,3,2)                          
      IF(IABS(ISN) .GT. 2) CALL SETERR(                                 
     1   ' FFT - ISN HAS MAGNITUDE GREATER THAN 2',39,4,2)              
C/                                                                      
C                                                                       
C ENTER THE RECOVERY MODE (STORING THE PREVIOUS)                        
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C SET UP STORAGE IN THE DYNAMIC STORAGE STACK                           
C                                                                       
      INFAC = ISTKGT(220,2)                                             
      INP   = INFAC + 11                                                
C                                                                       
      IAT   = ISTKGT(92,3)                                              
      ICK   = IAT   + 23                                                
      IBT   = ICK   + 23                                                
      ISK   = IBT   + 23                                                
C                                                                       
C  CALL THE SUBPROGRAM, F1FT, WHICH DOES THE WORK.                      
C                                                                       
      CALL F1FT(A,B,NTOT,N,NSPAN,ISN,                                   
     1         ISTAK(INFAC),ISTAK(INP),RSTAK(IAT),                      
     1         RSTAK(ICK),RSTAK(IBT),RSTAK(ISK))                        
C                                                                       
C  CHECK FOR ERRORS FROM F1FT                                           
C                                                                       
      IF (NERROR(NERR) .EQ. 0) GO TO 10                                 
      CALL ERROFF                                                       
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 1) CALL SETERR(                                     
C    1   35H FFT - PRIME FACTOR GREATER THAN 23,35,5,1)                 
C/7S                                                                    
      IF (NERR .EQ. 1) CALL SETERR(                                     
     1   ' FFT - PRIME FACTOR GREATER THAN 23',35,5,1)                  
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 2) CALL SETERR(                                     
C    1   43H FFT - SQUARE-FREE PRODUCT GREATER THAN 210,43,6,1)         
C/7S                                                                    
      IF (NERR .EQ. 2) CALL SETERR(                                     
     1   ' FFT - SQUARE-FREE PRODUCT GREATER THAN 210',43,6,1)          
C/                                                                      
C                                                                       
 10   CALL LEAVE                                                        
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE F1FT(A,B,NTOT,N,NSPAN,ISN,NFAC,NP,AT,CK,BT,SK)         
C                                                                       
C THIS SUBROUTINE COMPUTES THE MIXED-RADIX FOURIER TRANSFORM OR ITS     
C INVERSE FOR COMPLEX DATA.  IT CAN COMPUTE A SINGLE VARIATE TRANS-     
C FORM WITH ONE CALL STATEMENT OR A MULTIVARIATE TRANSFORM WITH A       
C SERIES OF CALL STATEMENTS.                                            
C                                                                       
C THE METHOD USED IS SINGLETON S ALGORITHM TO EVALUATE                  
C                                                                       
C                      N-1                                              
C                 Y(K)=   X(J)EXP(I2(PI)JK/N) K=0,1,. . .,N-1           
C                      J=0                                              
C                                                                       
C WHERE Y(K) AND X(J) ARE BOTH COMPLEX VALUED VECTORS.                  
C                                                                       
C                                                                       
C     INPUT                                                             
C                                                                       
C       A     -VECTOR OF SIZE NTOT CONTAINING REAL ELEMENTS OF DATA     
C       B     -VECTOR OF SIZE NTOT CONTAINING IMAGINARY ELEMENTS OF DATA
C      NTOT   -TOTAL NUMBER OF COMPLEX DATA POINTS                      
C       N     -NUMBER OF COMPLEX DATA POINTS OF THE CURRENT VARIABLE    
C     NSPAN  -NUMBER OF ELEMENTS OF BOTH A AND B NECESSARY TO SPAN      
C              ALL VALUES OF THE CURRENT VARIABLE                       
C              (FOR A SINGLE VARIATE TRANSFORM NTOT=N=NSPAN.  FOR A     
C              MULTIVARIATE TRANSFORM N AND NSPAN ARE DIFFERENT IN      
C              EACH CALL STATEMENT.  SEE EXAMPLE BELOW).                
C      ISN   -DETERMINES BY SIGN THE TYPE OF TRANSFORM BEING COMPUTED,  
C              FORWARD OR INVERSE                                       
C             -INDICATES BY ABSOLUTE VALUE THE VECTOR ARRANGEMENT OF    
C              INPUT DATA (AND TRANSFORM OUTPUT)                        
C             =+1 COMPLEX INPUT DATA ARE IN TWO VECTORS, A AND B.       
C              REAL COMPONENTS IN A, IMAGINARY COMPONENTS IN B.         
C             =-1 FOURIER COEFFICIENT INPUT VALUES ARE IN TWO VECTORS,  
C              A AND B.  COSINE VALUES IN A, SINE VALUES IN B.          
C             =+2 COMPLEX INPUT DATA ARE STORED ALTERNATELY IN A        
C              SINGLE COMPLEX VECTOR, A.  REAL VALUES ARE               
C              IN A(1),A(3),... AND IMAGINARY VALUES ARE                
C              IN A(2),A(4),....  SECOND ARGUMENT OF                    
C              F1FT SHOULD BE A(2).  (SEE EXAMPLE.)                     
C             =-2 FOURIER COEFFICIENT INPUT VALUES ARE STORED ALTER-    
C              NATELY IN A SINGLE VECTOR, A.  COSINE VALUES ARE IN      
C              A(1),A(3),... AND SINE VALUES ARE IN A(2),A(4),....      
C              SECOND ARGUMENT OF F1FT SHOULD BE A(2).                  
C                                                                       
C                                                                       
C     OUTPUT                                                            
C                                                                       
C       A     -VECTOR OF SIZE NTOT CONTAINING COSINE COMPONENTS OF      
C              FOURIER COEFFICIENTS                                     
C       B     -VECTOR OF SIZE NTOT CONTAINING SINE COMPONENTS OF FOURIER
C              COEFFICIENTS                                             
C                                                                       
C IF ISN=1 MULTIPLY OUTPUT BY 2/N FOR UNIT MAGNITUDE.                   
C                                                                       
C IF ISN=2 OUTPUT WILL ALTERNATE IN A SINGLE VECTOR, A.  COSINE         
C COEFFICIENTS IN A(1),A(3),... AND SINE COEFFICIENTS IN A(2),A(4),.... 
C OUTPUT SHOULD BE MULTIPLIED BY 4/N FOR UNIT MAGNITUDE.                
C                                                                       
C                                                                       
C NUMBER OF FACTORS OF N MUST NOT EXCEED 11.                            
C MAXIMUM PRIME FACTOR OF N MUST NOT EXCEED 23.                         
C PRODUCT OF THE SQUARE-FREE FACTORS, IF THERE EXISTS MORE THAN ONE,    
C MUST NOT EXCEED 210.                                                  
C                                                                       
C                                                                       
C     ERROR STATES                                                      
C          1.   PRIME FACTOR .GT. 23                                    
C          2.   SQUARE-FREE FACTOR PRODUCT .GT. 2.  10                  
C                                                                       
C     STACK STORAGE - NONE                                              
C                                                                       
C                                                                       
C                                                                       
C EXAMPLES -                                                            
C                                                                       
C TRANSFORM OF N COMPLEX DATA VALUES STORED IN TWO VECTORS -            
C                CALL FFT(A,B,N,N,N,1)                                  
C                                                                       
C INVERSE TRANSFORM OF FOURIER COEFFICIENTS STORED IN TWO VECTORS -     
C                CALL FFT(A,B,N,N,N,-1)                                 
C                                                                       
C TRANSFORM OF N COMPLEX DATA VALUES STORED ALTERNATELY IN A SINGLE     
C COMPLEX VECTOR -                                                      
C                CALL FFT(A,A(2),2*N,N,2*N,2)                           
C                                                                       
C INVERSE TRANSFORM OF N FOURIER COEFFICIENTS, COSINE AND SINE COMPO-   
C NENTS STORED ALTERNATELY IN A SINGLE VECTOR -                         
C                CALL FFT(A,A(2),2*N,N,2*N,-2)                          
C                                                                       
C FOR A MULTIVARIATE TRANSFORM THERE IS NO LIMIT ON THE NUMBER OF       
C IMPLIED SUBSCRIPTS.  FFT SHOULD BE CALLED ONCE FOR EACH VARIABLE      
C AND THE CALLS MAY BE MADE IN ANY ORDER.                               
C                                                                       
C TRANSFORM OF A TRI-VARIATE, A(N1,N2,N3), B(N1,N2,N3) -                
C                NTOT=N1*N2*N3                                          
C                CALL FFT(A,B,NTOT,N1,N1,1)                             
C                CALL FFT(A,B,NTOT,N2,N1*N2,1)                          
C                CALL FFT(A,B,NTOT,N3,NTOT,1)                           
C                                                                       
C                                                                       
C FOR TRANSFORM OF COMPLETELY REAL DATA USE RLTR IN CONJUNCTION WITH    
C FFT.                                                                  
C                                                                       
C                                                                       
C OTHER ROUTINES CALLED -  SETERR                                       
C                                                                       
C                                                                       
C THE PORT LIBRARY VERSION OF FFT IS AS SHOWN IN THE SINGLETON          
C REFERENCE EXCEPT FOR CHANGES IN THE ERROR HANDLING.                   
C                                                                       
C                                                                       
C REFERENCE -  SINGLETON, R. C.,  AN ALGORITHM FOR COMPUTING THE MIXED  
C             RADIX FAST FOURIER TRANSFORM , IEEE TRANSACTIONS ON AUDIO 
C             AND ELECTROACOUSTICS, VOL. AU-17, NO. 2, JUNE, 1969       
C             PP. 93-103.                                               
C                                                                       
C                                                                       
      DIMENSION A(1),B(1)                                               
      DIMENSION NFAC(2),NP(2)                                           
      DIMENSION AT(1),CK(1),BT(1),SK(1)                                 
      EQUIVALENCE (I,II)                                                
C  THE FOLLOWING TWO CONSTANTS SHOULD AGREE WITH THE ARRAY DIMENSIONS.  
C                                                                       
      MAXF=23                                                           
      MAXP=209                                                          
      IF(N .LT. 2) RETURN                                               
      RAD=8.0*ATAN(1.0)                                                 
      S72=RAD/5.0                                                       
      C72=COS(S72)                                                      
      S72=SIN(S72)                                                      
      S120=SQRT(0.75)                                                   
      INC=ISN                                                           
      IF(ISN .GE. 0) GO TO 10                                           
      S72=-S72                                                          
      S120=-S120                                                        
      RAD=-RAD                                                          
      INC=-INC                                                          
   10 NT=INC*NTOT                                                       
      KS=INC*NSPAN                                                      
      KSPAN=KS                                                          
      NN=NT-INC                                                         
      JC=KS/N                                                           
      RADF=RAD*FLOAT(JC)*0.5                                            
      I=0                                                               
      JF=0                                                              
C  DETERMINE THE FACTORS OF N                                           
      M=0                                                               
      K=N                                                               
      GO TO 20                                                          
   15 M=M+1                                                             
      NFAC(M)=4                                                         
      K=K/16                                                            
   20 IF(K-(K/16)*16 .EQ. 0) GO TO 15                                   
      J=3                                                               
      JJ=9                                                              
      GO TO 30                                                          
   25 M=M+1                                                             
      NFAC(M)=J                                                         
      K=K/JJ                                                            
   30 IF(MOD(K,JJ) .EQ. 0) GO TO 25                                     
      J=J+2                                                             
      JJ=J**2                                                           
      IF(JJ .LE. K) GO TO 30                                            
      IF(K .GT. 4) GO TO 40                                             
      KT=M                                                              
      NFAC(M+1)=K                                                       
      IF(K .NE. 1) M=M+1                                                
      GO TO 80                                                          
   40 IF(K-(K/4)*4 .NE. 0) GO TO 50                                     
      M=M+1                                                             
      NFAC(M)=2                                                         
      K=K/4                                                             
   50 KT=M                                                              
      J=2                                                               
   60 IF(MOD(K,J) .NE. 0) GO TO 70                                      
      M=M+1                                                             
      NFAC(M)=J                                                         
      K=K/J                                                             
   70 J=((J+1)/2)*2+1                                                   
      IF(J .LE. K) GO TO 60                                             
   80 IF(KT .EQ. 0) GO TO 100                                           
      J=KT                                                              
   90 M=M+1                                                             
      NFAC(M)=NFAC(J)                                                   
      J=J-1                                                             
      IF(J .NE. 0) GO TO 90                                             
C  COMPUTE FOURIER TRANSFORM                                            
  100 SD=RADF/FLOAT(KSPAN)                                              
      CD=2.0*SIN(SD)**2                                                 
      SD=SIN(SD+SD)                                                     
      KK=1                                                              
      I=I+1                                                             
      IF(NFAC(I) .NE. 2) GO TO 400                                      
C  TRANSFORM FOR FACTOR OF 2 (INCLUDING ROTATION FACTOR)                
      KSPAN=KSPAN/2                                                     
      K1=KSPAN+2                                                        
  210 K2=KK+KSPAN                                                       
      AK=A(K2)                                                          
      BK=B(K2)                                                          
      A(K2)=A(KK)-AK                                                    
      B(K2)=B(KK)-BK                                                    
      A(KK)=A(KK)+AK                                                    
      B(KK)=B(KK)+BK                                                    
      KK=K2+KSPAN                                                       
      IF(KK .LE. NN) GO TO 210                                          
      KK=KK-NN                                                          
      IF(KK .LE. JC) GO TO 210                                          
      IF(KK .GT. KSPAN) GO TO 800                                       
  220 C1=1.0-CD                                                         
      S1=SD                                                             
  230 K2=KK+KSPAN                                                       
      AK=A(KK)-A(K2)                                                    
      BK=B(KK)-B(K2)                                                    
      A(KK)=A(KK)+A(K2)                                                 
      B(KK)=B(KK)+B(K2)                                                 
      A(K2)=C1*AK-S1*BK                                                 
      B(K2)=S1*AK+C1*BK                                                 
      KK=K2+KSPAN                                                       
      IF(KK .LT. NT) GO TO 230                                          
      K2=KK-NT                                                          
      C1=-C1                                                            
      KK=K1-K2                                                          
      IF(KK .GT. K2) GO TO 230                                          
      AK=C1-(CD*C1+SD*S1)                                               
      S1=(SD*C1-CD*S1)+S1                                               
C  THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION             
C  ERROR. IF ROUNDED ARITHMETIC IS USED, ONE CAN SUBSTITUTE             
C     C1=AK                                                             
      C1=0.5/(AK**2+S1**2)+0.5                                          
      S1=C1*S1                                                          
      C1=C1*AK                                                          
      KK=KK+JC                                                          
      IF(KK .LT. K2) GO TO 230                                          
      K1=K1+INC+INC                                                     
      KK=(K1-KSPAN)/2+JC                                                
      IF(KK .LE. JC+JC) GO TO 220                                       
      GO TO 100                                                         
C  TRANSFORM FOR FACTOR OF 3 (OPTIONAL CODE)                            
  320 K1=KK+KSPAN                                                       
      K2=K1+KSPAN                                                       
      AK=A(KK)                                                          
      BK=B(KK)                                                          
      AJ=A(K1)+A(K2)                                                    
      BJ=B(K1)+B(K2)                                                    
      A(KK)=AK+AJ                                                       
      B(KK)=BK+BJ                                                       
      AK=-0.5*AJ+AK                                                     
      BK=-0.5*BJ+BK                                                     
      AJ=(A(K1)-A(K2))*S120                                             
      BJ=(B(K1)-B(K2))*S120                                             
      A(K1)=AK-BJ                                                       
      B(K1)=BK+AJ                                                       
      A(K2)=AK+BJ                                                       
      B(K2)=BK-AJ                                                       
      KK=K2+KSPAN                                                       
      IF(KK .LT. NN) GO TO 320                                          
      KK=KK-NN                                                          
      IF(KK .LE. KSPAN) GO TO 320                                       
      GO TO 700                                                         
C  TRANSFORM FOR FACTOR OF 4                                            
  400 IF(NFAC(I) .NE. 4) GO TO 600                                      
      KSPNN=KSPAN                                                       
      KSPAN=KSPAN/4                                                     
  410 C1=1.0                                                            
      S1=0                                                              
  420 K1=KK+KSPAN                                                       
      K2=K1+KSPAN                                                       
      K3=K2+KSPAN                                                       
      AKP=A(KK)+A(K2)                                                   
      AKM=A(KK)-A(K2)                                                   
      AJP=A(K1)+A(K3)                                                   
      AJM=A(K1)-A(K3)                                                   
      A(KK)=AKP+AJP                                                     
      AJP=AKP-AJP                                                       
      BKP=B(KK)+B(K2)                                                   
      BKM=B(KK)-B(K2)                                                   
      BJP=B(K1)+B(K3)                                                   
      BJM=B(K1)-B(K3)                                                   
      B(KK)=BKP+BJP                                                     
      BJP=BKP-BJP                                                       
      IF(ISN .LT. 0) GO TO 450                                          
      AKP=AKM-BJM                                                       
      AKM=AKM+BJM                                                       
      BKP=BKM+AJM                                                       
      BKM=BKM-AJM                                                       
      IF(S1 .EQ. 0.0) GO TO 460                                         
  430 A(K1)=AKP*C1-BKP*S1                                               
      B(K1)=AKP*S1+BKP*C1                                               
      A(K2)=AJP*C2-BJP*S2                                               
      B(K2)=AJP*S2+BJP*C2                                               
      A(K3)=AKM*C3-BKM*S3                                               
      B(K3)=AKM*S3+BKM*C3                                               
      KK=K3+KSPAN                                                       
      IF(KK .LE. NT) GO TO 420                                          
  440 C2=C1-(CD*C1+SD*S1)                                               
      S1=(SD*C1-CD*S1)+S1                                               
C  THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION             
C  ERROR. IF ROUNDED ARITHMETIC IS USED, ONE CAN SUBSTITUTE             
C     C1=C2                                                             
      C1=0.5/(C2**2+S1**2)+0.5                                          
      S1=C1*S1                                                          
      C1=C1*C2                                                          
      C2=C1**2-S1**2                                                    
      S2=2.0*C1*S1                                                      
      C3=C2*C1-S2*S1                                                    
      S3=C2*S1+S2*C1                                                    
      KK=KK-NT+JC                                                       
      IF(KK .LE. KSPAN) GO TO 420                                       
      KK=KK-KSPAN+INC                                                   
      IF(KK .LE. JC) GO TO 410                                          
      IF(KSPAN .EQ. JC) GO TO 800                                       
      GO TO 100                                                         
  450 AKP=AKM+BJM                                                       
      AKM=AKM-BJM                                                       
      BKP=BKM-AJM                                                       
      BKM=BKM+AJM                                                       
      IF(S1 .NE. 0.0) GO TO 430                                         
  460 A(K1)=AKP                                                         
      B(K1)=BKP                                                         
      A(K2)=AJP                                                         
      B(K2)=BJP                                                         
      A(K3)=AKM                                                         
      B(K3)=BKM                                                         
      KK=K3+KSPAN                                                       
      IF(KK .LE. NT) GO TO 420                                          
      GO TO 440                                                         
C  TRANSFORM FOR FACTOR OF 5 (OPTIONAL CODE)                            
  510 C2=C72**2-S72**2                                                  
      S2=2.0*C72*S72                                                    
  520 K1=KK+KSPAN                                                       
      K2=K1+KSPAN                                                       
      K3=K2+KSPAN                                                       
      K4=K3+KSPAN                                                       
      AKP=A(K1)+A(K4)                                                   
      AKM=A(K1)-A(K4)                                                   
      BKP=B(K1)+B(K4)                                                   
      BKM=B(K1)-B(K4)                                                   
      AJP=A(K2)+A(K3)                                                   
      AJM=A(K2)-A(K3)                                                   
      BJP=B(K2)+B(K3)                                                   
      BJM=B(K2)-B(K3)                                                   
      AA=A(KK)                                                          
      BB=B(KK)                                                          
      A(KK)=AA+AKP+AJP                                                  
      B(KK)=BB+BKP+BJP                                                  
      AK=AKP*C72+AJP*C2+AA                                              
      BK=BKP*C72+BJP*C2+BB                                              
      AJ=AKM*S72+AJM*S2                                                 
      BJ=BKM*S72+BJM*S2                                                 
      A(K1)=AK-BJ                                                       
      A(K4)=AK+BJ                                                       
      B(K1)=BK+AJ                                                       
      B(K4)=BK-AJ                                                       
      AK=AKP*C2+AJP*C72+AA                                              
      BK=BKP*C2+BJP*C72+BB                                              
      AJ=AKM*S2-AJM*S72                                                 
      BJ=BKM*S2-BJM*S72                                                 
      A(K2)=AK-BJ                                                       
      A(K3)=AK+BJ                                                       
      B(K2)=BK+AJ                                                       
      B(K3)=BK-AJ                                                       
      KK=K4+KSPAN                                                       
      IF(KK .LT. NN) GO TO 520                                          
      KK=KK-NN                                                          
      IF(KK .LE. KSPAN) GO TO 520                                       
      GO TO 700                                                         
C  TRANSFORM FOR ODD FACTORS                                            
  600 K=NFAC(I)                                                         
      KSPNN=KSPAN                                                       
      KSPAN=KSPAN/K                                                     
      IF(K .EQ. 3) GO TO 320                                            
      IF(K .EQ. 5) GO TO 510                                            
      IF(K .EQ. JF) GO TO 640                                           
      JF=K                                                              
      S1=RAD/FLOAT(K)                                                   
      C1=COS(S1)                                                        
      S1=SIN(S1)                                                        
      IF(JF .LE. MAXF)  GO TO 625                                       
      CALL ERROFF                                                       
C/6S                                                                    
C     CALL SETERR(27HF1FT - PRIME FACTOR .GT. 23,27,1,1)                
C/7S                                                                    
      CALL SETERR('F1FT - PRIME FACTOR .GT. 23',27,1,1)                 
C/                                                                      
      RETURN                                                            
  625 CK(JF)=1.0                                                        
      SK(JF)=0.0                                                        
      J=1                                                               
  630 CK(J)=CK(K)*C1+SK(K)*S1                                           
      SK(J)=CK(K)*S1-SK(K)*C1                                           
      K=K-1                                                             
      CK(K)=CK(J)                                                       
      SK(K)=-SK(J)                                                      
      J=J+1                                                             
      IF(J .LT. K) GO TO 630                                            
  640 K1=KK                                                             
      K2=KK+KSPNN                                                       
      AA=A(KK)                                                          
      BB=B(KK)                                                          
      AK=AA                                                             
      BK=BB                                                             
      J=1                                                               
      K1=K1+KSPAN                                                       
  650 K2=K2-KSPAN                                                       
      J=J+1                                                             
      AT(J)=A(K1)+A(K2)                                                 
      AK=AT(J)+AK                                                       
      BT(J)=B(K1)+B(K2)                                                 
      BK=BT(J)+BK                                                       
      J=J+1                                                             
      AT(J)=A(K1)-A(K2)                                                 
      BT(J)=B(K1)-B(K2)                                                 
      K1=K1+KSPAN                                                       
      IF(K1 .LT. K2) GO TO 650                                          
      A(KK)=AK                                                          
      B(KK)=BK                                                          
      K1=KK                                                             
      K2=KK+KSPNN                                                       
      J=1                                                               
  660 K1=K1+KSPAN                                                       
      K2=K2-KSPAN                                                       
      JJ=J                                                              
      AK=AA                                                             
      BK=BB                                                             
      AJ=0.0                                                            
      BJ=0.0                                                            
      K=1                                                               
  670 K=K+1                                                             
      AK=AT(K)*CK(JJ)+AK                                                
      BK=BT(K)*CK(JJ)+BK                                                
      K=K+1                                                             
      AJ=AT(K)*SK(JJ)+AJ                                                
      BJ=BT(K)*SK(JJ)+BJ                                                
      JJ=JJ+J                                                           
      IF(JJ .GT. JF) JJ=JJ-JF                                           
      IF(K .LT. JF) GO TO 670                                           
      K=JF-J                                                            
      A(K1)=AK-BJ                                                       
      B(K1)=BK+AJ                                                       
      A(K2)=AK+BJ                                                       
      B(K2)=BK-AJ                                                       
      J=J+1                                                             
      IF(J .LT. K) GO TO 660                                            
      KK=KK+KSPNN                                                       
      IF(KK .LE. NN) GO TO 640                                          
      KK=KK-NN                                                          
      IF(KK .LE. KSPAN) GO TO 640                                       
C  MULTIPLY BY ROTATION FACTOR (EXCEPT FOR FACTORS OF 2 AND 4)          
  700 IF(I .EQ. M) GO TO 800                                            
      KK=JC+1                                                           
  710 C2=1.0-CD                                                         
      S1=SD                                                             
  720 C1=C2                                                             
      S2=S1                                                             
      KK=KK+KSPAN                                                       
  730 AK=A(KK)                                                          
      A(KK)=C2*AK-S2*B(KK)                                              
      B(KK)=S2*AK+C2*B(KK)                                              
      KK=KK+KSPNN                                                       
      IF(KK .LE. NT) GO TO 730                                          
      AK=S1*S2                                                          
      S2=S1*C2+C1*S2                                                    
      C2=C1*C2-AK                                                       
      KK=KK-NT+KSPAN                                                    
      IF(KK .LE. KSPNN) GO TO 730                                       
      C2=C1-(CD*C1+SD*S1)                                               
      S1=S1+(SD*C1-CD*S1)                                               
C  THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION             
C  ERROR. IF ROUNDED ARITHMETIC IS USED, THEY MAY BE DELETED.           
      C1=0.5/(C2**2+S1**2)+0.5                                          
      S1=C1*S1                                                          
      C2=C1*C2                                                          
      KK=KK-KSPNN+JC                                                    
      IF(KK .LE. KSPAN) GO TO 720                                       
      KK=KK-KSPAN+JC+INC                                                
      IF(KK .LE. JC+JC) GO TO 710                                       
      GO TO 100                                                         
C  PERMUTE THE RESULTS TO NORMAL ORDER---DONE IN TWO STAGES             
C  PERMUTATION FOR SQUARE FACTORS OF N                                  
  800 NP(1)=KS                                                          
      IF(KT .EQ. 0) GO TO 890                                           
      K=KT+KT+1                                                         
      IF(M .LT. K) K=K-1                                                
      J=1                                                               
      NP(K+1)=JC                                                        
  810 NP(J+1)=NP(J)/NFAC(J)                                             
      NP(K)=NP(K+1)*NFAC(J)                                             
      J=J+1                                                             
      K=K-1                                                             
      IF(J .LT. K) GO TO 810                                            
      K3=NP(K+1)                                                        
      KSPAN=NP(2)                                                       
      KK=JC+1                                                           
      K2=KSPAN+1                                                        
      J=1                                                               
      IF(N .NE. NTOT) GO TO 850                                         
C  PERMUTATION FOR SINGLE-VARIATE TRANSFORM (OPTIONAL CODE)             
  820 AK=A(KK)                                                          
      A(KK)=A(K2)                                                       
      A(K2)=AK                                                          
      BK=B(KK)                                                          
      B(KK)=B(K2)                                                       
      B(K2)=BK                                                          
      KK=KK+INC                                                         
      K2=KSPAN+K2                                                       
      IF(K2 .LT. KS) GO TO 820                                          
  830 K2=K2-NP(J)                                                       
      J=J+1                                                             
      K2=NP(J+1)+K2                                                     
      IF(K2 .GT. NP(J)) GO TO 830                                       
      J=1                                                               
  840 IF(KK .LT. K2) GO TO 820                                          
      KK=KK+INC                                                         
      K2=KSPAN+K2                                                       
      IF(K2 .LT. KS) GO TO 840                                          
      IF(KK .LT. KS) GO TO 830                                          
      JC=K3                                                             
      GO TO 890                                                         
C  PERMUTATION FOR MULTIVARIATE TRANSFORM                               
  850 K=KK+JC                                                           
  860 AK=A(KK)                                                          
      A(KK)=A(K2)                                                       
      A(K2)=AK                                                          
      BK=B(KK)                                                          
      B(KK)=B(K2)                                                       
      B(K2)=BK                                                          
      KK=KK+INC                                                         
      K2=K2+INC                                                         
      IF(KK .LT. K) GO TO 860                                           
      KK=KK+KS-JC                                                       
      K2=K2+KS-JC                                                       
      IF(KK .LT. NT) GO TO 850                                          
      K2=K2-NT+KSPAN                                                    
      KK=KK-NT+JC                                                       
      IF(K2 .LT. KS) GO TO 850                                          
  870 K2=K2-NP(J)                                                       
      J=J+1                                                             
      K2=NP(J+1)+K2                                                     
      IF(K2 .GT. NP(J)) GO TO 870                                       
      J=1                                                               
  880 IF(KK .LT. K2) GO TO 850                                          
      KK=KK+JC                                                          
      K2=KSPAN+K2                                                       
      IF(K2 .LT. KS) GO TO 880                                          
      IF(KK .LT. KS) GO TO 870                                          
      JC=K3                                                             
  890 IF(2*KT+1 .GE. M) RETURN                                          
      KSPNN=NP(KT+1)                                                    
C  PERMUTATION FOR SQUARE-FREE FACTORS OF N                             
      J=M-KT                                                            
      NFAC(J+1)=1                                                       
  900 NFAC(J)=NFAC(J)*NFAC(J+1)                                         
      J=J-1                                                             
      IF(J .NE. KT) GO TO 900                                           
      KT=KT+1                                                           
      NN=NFAC(KT)-1                                                     
      IF(NN .LE. MAXP)  GO TO 901                                       
      CALL ERROFF                                                       
C/6S                                                                    
C     CALL SETERR(42HF1FT - SQUARE-FREE FACTOR PRODUCT .GT. 210,42,2,1) 
C/7S                                                                    
      CALL SETERR('F1FT - SQUARE-FREE FACTOR PRODUCT .GT. 210',42,2,1)  
C/                                                                      
      RETURN                                                            
  901 JJ=0                                                              
      J=0                                                               
      GO TO 906                                                         
  902 JJ=JJ-K2                                                          
      K2=KK                                                             
      K=K+1                                                             
      KK=NFAC(K)                                                        
  904 JJ=KK+JJ                                                          
      IF(JJ .GE. K2) GO TO 902                                          
      NP(J)=JJ                                                          
  906 K2=NFAC(KT)                                                       
      K=KT+1                                                            
      KK=NFAC(K)                                                        
      J=J+1                                                             
      IF(J .LE. NN) GO TO 904                                           
C  DETERMINE THE PERMUTATION CYCLES OF LENGTH GREATER THAN 1            
      J=0                                                               
      GO TO 914                                                         
 910  K=KK                                                              
      KK=NP(K)                                                          
      NP(K)=-KK                                                         
      IF(KK .NE. J) GO TO 910                                           
      K3=KK                                                             
  914 J=J+1                                                             
      KK=NP(J)                                                          
      IF(KK .LT. 0) GO TO 914                                           
      IF(KK .NE. J) GO TO 910                                           
      NP(J)=-J                                                          
      IF(J .NE. NN) GO TO 914                                           
      MAXF=INC*MAXF                                                     
C  REORDER A AND B, FOLLOWING THE PERMUTATION CYCLES                    
      GO TO 950                                                         
  924 J=J-1                                                             
      IF(NP(J) .LT. 0) GO TO 924                                        
      JJ=JC                                                             
  926 KSPAN=JJ                                                          
      IF(JJ .GT. MAXF) KSPAN=MAXF                                       
      JJ=JJ-KSPAN                                                       
      K=NP(J)                                                           
      KK=JC*K+II+JJ                                                     
      K1=KK+KSPAN                                                       
      K2=0                                                              
  928 K2=K2+1                                                           
      AT(K2)=A(K1)                                                      
      BT(K2)=B(K1)                                                      
      K1=K1-INC                                                         
      IF(K1 .NE. KK) GO TO 928                                          
  932 K1=KK+KSPAN                                                       
      K2=K1-JC*(K+NP(K))                                                
      K=-NP(K)                                                          
  936 A(K1)=A(K2)                                                       
      B(K1)=B(K2)                                                       
      K1=K1-INC                                                         
      K2=K2-INC                                                         
      IF(K1 .NE. KK) GO TO 936                                          
      KK=K2                                                             
      IF(K .NE. J) GO TO 932                                            
      K1=KK+KSPAN                                                       
      K2=0                                                              
  940 K2=K2+1                                                           
      A(K1)=AT(K2)                                                      
      B(K1)=BT(K2)                                                      
      K1=K1-INC                                                         
      IF(K1 .NE. KK) GO TO 940                                          
      IF(JJ .NE. 0) GO TO 926                                           
      IF(J .NE. 1) GO TO 924                                            
  950 J=K3+1                                                            
      NT=NT-KSPNN                                                       
      II=NT-INC+1                                                       
      IF(NT .GE. 0) GO TO 924                                           
      RETURN                                                            
       END                                                              
      SUBROUTINE RLTR(A,B,N,ISN)                                        
C                                                                       
C THIS SUBROUTINE COMPLETES THE FOURIER TRANSFORM OF 2*N REAL           
C DATA VALUES.                                                          
C IT PERFORMS A RADIX-2 TRANSFORM ON THE FOURIER COEFFICIENTS WHICH     
C RESULT FROM INITIAL TRANSFORM OF THE 2*N REAL DATA VALUES BY AN       
C N DIMENSIONAL COMPLEX FOURIER TRANSFORM.                              
C THIS PROGRAM MUST BE USED IN CONJUNCTION WITH FFT.                    
C                                                                       
C THIS IS SINGLETON S PROGRAM, WHICH USES THE SANDE FACTORING.          
C                                                                       
C                                                                       
C   ISN  -DETERMINES BY SIGN THE TYPE OF TRANSFORM BEING COMPUTED,      
C         FORWARD OR INVERSE.                                           
C        -INDICATES BY ABSOLUTE VALUE THE VECTOR ARRANGEMENT OF         
C         INPUT DATA (AND TRANSFORM RESULTS).                           
C                                                                       
C                                                                       
C   IF ISN=1    CALLING SEQUENCE IS-                                    
C                CALL FFT(A,B,N,N,N,1)                                  
C                CALL RLTR(A,B,N,1).                                    
C                                                                       
C        THE 2*N REAL INPUT VALUES TO FFT ARE STORED ALTERNATELY IN TWO 
C        VECTORS, A AND B-  A(1),B(1),A(2),B(2),...,A(N),B(N).          
C                                                                       
C        SINCE 2(N+1) COEFFICIENTS RESULT FROM THE TRANSFORM            
C        OF THE ORIGINAL 2N DATA POINTS, THE COSINE COEFFICIENTS        
C        WILL BE IN A(1),A(2),...,A(N+1) - NOTE THAT A MUST BE          
C        DIMENSIONED APPROPRIATELY, AND THE SINE COEFFICIENTS WILL      
C        BE IN B(1),B(2),...,B(N+1), BUT B(N+1) = 0.                    
C                                                                       
C                                                                       
C   IF ISN=2    CALLING SEQUENCE IS-                                    
C                CALL FFT(A,A(2),N,N,N,2)                               
C                CALL RLTR(A,A(2),N,2).                                 
C                                                                       
C        THE 2*N REAL INPUT VALUES TO FFT ARE STORED IN A SINGLE        
C        VECTOR, A.                                                     
C        THE OUTPUT WILL BE STORED ALTERNATELY IN A,                    
C        COSINE COEFFICIENTS IN A(1),A(3),...,A(2N+1) AND               
C        SINE COEFFICIENTS IN A(2),A(4),...,A(2N+2).                    
C                                                                       
C                                                                       
C IF ISN=1   DIVIDE OUTPUT BY 2*N FOR UNIT MAGNITUDE.                   
C IF ISN=2   DIVIDE OUTPUT BY N FOR UNIT MAGNITUDE.                     
C                                                                       
C                                                                       
C   IF ISN=-1   CALLING SEQUENCE IS-                                    
C                CALL RLTR(A,B,N,-1)                                    
C                CALL FFT(A,B,N,N,N,-1).                                
C                                                                       
C        THE OUTPUT OF REAL VALUES ALTERNATES IN TWO VECTORS, A AND B-  
C        A(1),B(1),A(2),B(2),...,A(N),B(N).                             
C                                                                       
C                                                                       
C   IF ISN=-2   CALLING SEQUENCE IS-                                    
C                CALL RLTR(A,A(2),N,-2)                                 
C                CALL FFT(A,A(2),N,N,N,-2).                             
C                                                                       
C        THE OUTPUT OF REAL VALUES IS IN A SINGLE VECTOR, A AND         
C        SHOULD BE DIVIDED BY 2 FOR PROPER SCALING.                     
C                                                                       
C                                                                       
C REFERENCE-  SINGLETON, R. C.,  AN ALGORITHM FOR COMPUTING THE MIXED   
C             RADIX FAST FOURIER TRANSFORM , IEEE TRANSACTIONS ON       
C             AUDIO AND ELECTROACOUSTICS, VOL. AU-17, NO. 2,            
C             JUNE, 1969, PP. 93-103.                                   
C                                                                       
C                                                                       
      DIMENSION A(1),B(1)                                               
      REAL IM                                                           
C                                                                       
      INC=IABS(ISN)                                                     
      NK=N*INC+2                                                        
      NH=NK/2                                                           
      SD=2.0*ATAN(1.0)/FLOAT(N)                                         
      CD=2.0*SIN(SD)**2                                                 
      SD=SIN(SD+SD)                                                     
      SN=0.0                                                            
      IF(ISN .LT. 0) GO TO 30                                           
      CN=1.0                                                            
      A(NK-1)=A(1)                                                      
      B(NK-1)=B(1)                                                      
   10 DO 20 J=1,NH,INC                                                  
      K=NK-J                                                            
      AA=A(J)+A(K)                                                      
      AB=A(J)-A(K)                                                      
      BA=B(J)+B(K)                                                      
      BB=B(J)-B(K)                                                      
      RE=CN*BA+SN*AB                                                    
      IM=SN*BA-CN*AB                                                    
      B(K)=IM-BB                                                        
      B(J)=IM+BB                                                        
      A(K)=AA-RE                                                        
      A(J)=AA+RE                                                        
      AA=CN-(CD*CN+SD*SN)                                               
      SN=(SD*CN-CD*SN)+SN                                               
C    THE FOLLOWING 3 STATEMENTS COMPENSATE FOR TRUNCATION               
C    ERROR. IF ROUNDED ARITHMETIC IS USED, SUBSTITUTE                   
C 20 CN=AA FOR THE FOLLOWING THREE STATEMENTS.                          
      CN=0.5/(AA**2+SN**2)+0.5                                          
      SN=CN*SN                                                          
   20 CN=CN*AA                                                          
      RETURN                                                            
   30 CN=-1.0                                                           
      SD=-SD                                                            
      GO TO 10                                                          
       END                                                              
      SUBROUTINE DFFTR(NNP2,A,B)                                        
C                                                                       
C DFFTC FINDS THE FOURIER TRANSFORM OF THE 2N REAL DATA                 
C POINTS STORED IN THE REAL ARRAY, A, OF DIMENSION N PLUS 2,            
C BY SIMPLY SETTING UP THE CORRECT CALLS TO DFFT AND DRLTR.             
C                                                                       
C STORAGE IN THE DYNAMIC STORAGE STACK IS REQUIRED FOR                  
C 220 INTEGER VARIABLES AND 92 DOUBLE PRECISION VARIABLES.              
C                                                                       
C     INPUT                                                             
C                                                                       
C       NNP2  -FOR 2N INPUT POINTS, NNP2 MUST BE INPUT AS 2*N+2         
C              NOTE THAT THE NUMBER OF INPUT POINTS MUST BE EVEN.       
C       A     -A VECTOR OF LENGTH NNP2 CONTAINING THE 2N REAL           
C              DATA IN THE FIRST 2N LOCATIONS                           
C                                                                       
C     OUTPUT                                                            
C                                                                       
C       A     -THE FIRST N+1 LOCATIONS OF A CONTAIN THE REAL (COSINE)   
C              COMPONENTS OF THE FOURIER COEFFICIENTS                   
C       B     -A VECTOR OF LENGTH N+1 CONTAINING THE N+1 IMAGINARY      
C              (SINE) COMPONENTS OF THE FOURIER COEFFICIENTS            
C              ( WITH B(N+1) = 0.)                                      
C                                                                       
C                                                                       
C NUMBER OF FACTORS OF N MUST NOT EXCEED 11.                            
C MAXIMUM PRIME FACTOR OF N MUST NOT EXCEED 23.                         
C PRODUCT OF THE SQUARE-FREE FACTORS, IF THERE EXISTS MORE THAN ONE,    
C MUST NOT EXCEED 210.                                                  
C                                                                       
C                                                                       
C     ERROR STATES                                                      
C          1.  NNP2 (2N+2) IS LESS THAN 6                               
C          2.  NNP2 IS NOT EVEN                                         
C          3.  PRIME FACTOR .GT. 23                                     
C              (RECOVERABLE)                                            
C          4.  SQUARE-FREE FACTOR PRODUCT .GT. 210                      
C              (RECOVERABLE)                                            
C                                                                       
C                                                                       
C COMMON AREA                                                           
      COMMON/CSTAK/DSTAK(500)                                           
C                                                                       
      DOUBLE PRECISION A(2),B(2)                                        
      DOUBLE PRECISION DSTAK                                            
C                                                                       
C  TEST THE VALIDITY OF THE INPUTS                                      
C                                                                       
C/6S                                                                    
C     IF(NNP2 .LT. 6) CALL SETERR(                                      
C    1   27HDFFTR - NNP2 IS LESS THAN 6,27,1,2)                         
C/7S                                                                    
      IF(NNP2 .LT. 6) CALL SETERR(                                      
     1   'DFFTR - NNP2 IS LESS THAN 6',27,1,2)                          
C/                                                                      
      J = NNP2                                                          
C/6S                                                                    
C     IF (J/2*2 .NE. J) CALL SETERR(                                    
C    1   34HDFFTR - NNP2 IS NOT AN EVEN NUMBER,34,2,2)                  
C/7S                                                                    
      IF (J/2*2 .NE. J) CALL SETERR(                                    
     1   'DFFTR - NNP2 IS NOT AN EVEN NUMBER',34,2,2)                   
C/                                                                      
C                                                                       
C ENTER THE RECOVERY MODE (STORING THE PREVIOUS)                        
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C  CALL THE SUBPROGRAMS, DFFT AND DRLTR                                 
C                                                                       
      J = NNP2/2 - 1                                                    
      CALL DFFT(A,A(2),J,J,J,2)                                         
      CALL DRLTR(A,A(2),J,2)                                            
C                                                                       
C                                                                       
C  CHECK FOR ERRORS FROM DFFT                                           
C                                                                       
      IF (NERROR(NERR) .EQ. 0) GO TO 10                                 
C                                                                       
      CALL ERROFF                                                       
C/6S                                                                    
C     IF (NERR .EQ. 5) CALL SETERR(                                     
C    1   36HDFFTR - PRIME FACTOR GREATER THAN 23,36,3,1)                
C/7S                                                                    
      IF (NERR .EQ. 5) CALL SETERR(                                     
     1   'DFFTR - PRIME FACTOR GREATER THAN 23',36,3,1)                 
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 6) CALL SETERR(                                     
C    1   44HDFFTR - SQUARE-FREE PRODUCT GREATER THAN 210,44,4,1)        
C/7S                                                                    
      IF (NERR .EQ. 6) CALL SETERR(                                     
     1   'DFFTR - SQUARE-FREE PRODUCT GREATER THAN 210',44,4,1)         
C/                                                                      
C                                                                       
      GO TO 40                                                          
C                                                                       
 10   CONTINUE                                                          
C  FOR CONVENIENCE TO THE USER, PUT THE REAL COEFFICIENTS,              
C  WHICH DRLTR STORES IN THE LOCATIONS A(1), A(3),...,A(2N+1)           
C  INTO THE BEGINNING OF THE A ARRAY, AFTER FIRST PUTTING               
C  THE IMAGINARY COEFFICIENTS, RETURNED BY DRLTR IN LOCATIONS           
C  A(2),A(4),...,A(2N+2), INTO THE ARRAY B.                             
C                                                                       
      J = 2                                                             
      KK = NNP2/2                                                       
      DO 20 K=1,KK                                                      
      B(K) = A(J)                                                       
      J = J+2                                                           
 20   CONTINUE                                                          
C                                                                       
      J=3                                                               
      DO 30 K=2,KK                                                      
      A(K) = A(J)                                                       
      J = J+2                                                           
 30   CONTINUE                                                          
C                                                                       
 40   CALL LEAVE                                                        
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DFFTRI(NN,FR,FI)                                       
C                                                                       
C DFFTCI FINDS THE INVERSE FOURIER TRANSFORM BY CALLING                 
C DRLTR AND DFFT WITH APPROPRIATE INFORMATION.                          
C THE RESULT OF THE INVERSE TRANSFORM IS EXPECTED TO BE                 
C REAL, AND TO CONTAIN AN EVEN NUMBER 2N, OF DATA POINTS.               
C THE RESULT OF THE INVERSE TRANSFORM WILL BE PLACED, BY                
C DFFTRI, IN THE FIRST ARRAY, FR, WHICH MUST THEREFORE                  
C BE DIMENSIONED TO AT LEAST 2N, ALTHOUGH ONLY N+1 FOURIER              
C COEFFICIENTS ARE INPUT IN THE ARRAYS, FR AND FI.                      
C                                                                       
C STORAGE IN THE DYNAMIC STORAGE STACK IS REQUIRED FOR                  
C 220 INTEGER VARIABLES AND 92 DOUBLE PRECISION VARIABLES.              
C                                                                       
C     INPUT                                                             
C                                                                       
C       NN    -2N, THE NUMBER OF REAL DATA POINTS                       
C              RESULTING FROM THE REQUESTED INVERSE TRANSFORM           
C       FR    -A VECTOR OF LENGTH 2N CONTAINING, IN ITS FIRST N+1       
C              LOCATIONS, THE REAL PART OF THE FOURIER COEFFICIENTS     
C       FI    -A VECTOR OF LENGTH N+1 CONTAINING THE IMAGINARY          
C              PART OF THE FOURIER COEFFICIENTS                         
C                                                                       
C     OUTPUT                                                            
C                                                                       
C       FR    -THE 2N REAL DATA RESULTING FROM THE INVERSE TRANSFORM    
C                                                                       
C NUMBER OF FACTORS OF N MUST NOT EXCEED 11.                            
C MAXIMUM PRIME FACTOR OF N MUST NOT EXCEED 23.                         
C PRODUCT OF THE SQUARE-FREE FACTORS, IF THERE EXISTS MORE THAN ONE,    
C MUST NOT EXCEED 210.                                                  
C                                                                       
C                                                                       
C     ERROR STATES                                                      
C          1.  NN (2N) IS LESS THAN 4                                   
C          2.  NN IS NOT EVEN                                           
C          3.  PRIME FACTOR .GT. 23                                     
C              (RECOVERABLE)                                            
C          4.  SQUARE-FREE FACTOR PRODUCT .GT. 210                      
C              (RECOVERABLE)                                            
C                                                                       
C                                                                       
C COMMON AREA                                                           
      COMMON/CSTAK/DSTAK(500)                                           
C                                                                       
      DOUBLE PRECISION FR(1),FI(1)                                      
      DOUBLE PRECISION DSTAK                                            
C                                                                       
C  TEST THE VALIDITY OF THE INPUTS                                      
C                                                                       
C/6S                                                                    
C     IF(NN .LT. 4) CALL SETERR(                                        
C    1   26HDFFTRI - NN IS LESS THAN 4,26,1,2)                          
C/7S                                                                    
      IF(NN .LT. 4) CALL SETERR(                                        
     1   'DFFTRI - NN IS LESS THAN 4',26,1,2)                           
C/                                                                      
      J = NN                                                            
C/6S                                                                    
C     IF (J/2*2 .NE. J) CALL SETERR(                                    
C    1   33HDFFTRI - NN IS NOT AN EVEN NUMBER,33,2,2)                   
C/7S                                                                    
      IF (J/2*2 .NE. J) CALL SETERR(                                    
     1   'DFFTRI - NN IS NOT AN EVEN NUMBER',33,2,2)                    
C/                                                                      
C                                                                       
C ENTER THE RECOVERY MODE (STORING THE PREVIOUS)                        
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C  CALL THE SUBPROGRAMS, DFFT AND DRLTR                                 
C                                                                       
      N = NN/2                                                          
      CALL DRLTR(FR,FI,N,-1)                                            
      CALL DFFT(FR,FI,N,N,N,-1)                                         
C                                                                       
C                                                                       
C  CHECK FOR ERRORS FROM DFFT                                           
C                                                                       
      IF (NERROR(NERR) .EQ. 0) GO TO 10                                 
C                                                                       
      CALL ERROFF                                                       
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 5) CALL SETERR(                                     
C    1   37HDFFTRI - PRIME FACTOR GREATER THAN 23,37,3,1)               
C/7S                                                                    
      IF (NERR .EQ. 5) CALL SETERR(                                     
     1   'DFFTRI - PRIME FACTOR GREATER THAN 23',37,3,1)                
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 6) CALL SETERR(                                     
C    1   45HDFFTRI - SQUARE-FREE PRODUCT GREATER THAN 210,45,4,1)       
C/7S                                                                    
      IF (NERR .EQ. 6) CALL SETERR(                                     
     1   'DFFTRI - SQUARE-FREE PRODUCT GREATER THAN 210',45,4,1)        
C/                                                                      
C                                                                       
      GO TO 30                                                          
C                                                                       
 10   CONTINUE                                                          
C                                                                       
C THE TIME DOMAIN RESULTS, RETURNED BY THIS PROCESS, ALTERNATE          
C IN THE VECTORS FR AND FI AS FR(1),FI(1),FR(2),FI(2),...FR(N),FI(N).   
C THE FOLLOWING FRAGMENT RECOMBINES THEM INTO FR                        
C                                                                       
      K = N                                                             
      DO 20 KK=1,N                                                      
      FR(2*K) = FI(K)                                                   
      FR(2*K-1) = FR(K)                                                 
 20   K = K-1                                                           
 30   CALL LEAVE                                                        
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DFFTC(N,AR,AI)                                         
C                                                                       
C DFFTC FINDS THE FOURIER TRANSFORM OF THE COMPLEX                      
C DATA STORED IN THE REAL AND IMAGINARY ARRAYS, AR AND AI               
C BY SIMPLY SETTING UP THE CORRECT CALL TO DFFT.                        
C                                                                       
C STORAGE IN THE DYNAMIC STORAGE STACK IS REQUIRED FOR                  
C 220 INTEGER VARIABLES AND 92 DOUBLE PRECISION VARIABLES.              
C                                                                       
C     INPUT                                                             
C                                                                       
C       N     -NUMBER OF COMPLEX DATA POINTS                            
C       AR    -VECTOR OF LENGTH N CONTAINING REAL COMPONENTS            
C       AI    -VECTOR OF LENGTH N CONTAINING IMAGINARY COMPONENTS       
C                                                                       
C     OUTPUT                                                            
C                                                                       
C       AR    -CONTAINS THE REAL (COSINE)                               
C              COMPONENTS OF THE FOURIER COEFFICIENTS                   
C       AI    -CONTAINS THE IMAGINARY (SINE)                            
C              COMPONENTS OF THE FOURIER COEFFICIENTS                   
C                                                                       
C                                                                       
C NUMBER OF FACTORS OF N MUST NOT EXCEED 11.                            
C MAXIMUM PRIME FACTOR OF N MUST NOT EXCEED 23.                         
C PRODUCT OF THE SQUARE-FREE FACTORS, IF THERE EXISTS MORE THAN ONE,    
C MUST NOT EXCEED 210.                                                  
C                                                                       
C                                                                       
C     ERROR STATES                                                      
C          1.  N IS LESS THAN 2                                         
C          2.  PRIME FACTOR .GT. 23                                     
C              (RECOVERABLE)                                            
C          3.  SQUARE-FREE FACTOR PRODUCT .GT. 210                      
C              (RECOVERABLE)                                            
C                                                                       
C                                                                       
C COMMON AREA                                                           
      COMMON/CSTAK/DSTAK(500)                                           
C                                                                       
      DOUBLE PRECISION AR(1),AI(1)                                      
      DOUBLE PRECISION DSTAK                                            
C                                                                       
C  TEST THE VALIDITY OF THE INPUTS                                      
C                                                                       
C/6S                                                                    
C     IF(N .LT. 2) CALL SETERR(                                         
C    1   24HDFFTC - N IS LESS THAN 2,24,1,2)                            
C/7S                                                                    
      IF(N .LT. 2) CALL SETERR(                                         
     1   'DFFTC - N IS LESS THAN 2',24,1,2)                             
C/                                                                      
C ENTER THE RECOVERY MODE (STORING THE PREVIOUS)                        
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C  CALL THE SUBPROGRAM, DFFT.                                           
C                                                                       
      CALL DFFT(AR,AI,N,N,N,1)                                          
C                                                                       
C  CHECK FOR ERRORS FROM FFT                                            
C                                                                       
      IF (NERROR(NERR) .EQ. 0) GO TO 10                                 
C                                                                       
      CALL ERROFF                                                       
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 5) CALL SETERR(                                     
C    1   36HDFFTC - PRIME FACTOR GREATER THAN 23,36,2,1)                
C/7S                                                                    
      IF (NERR .EQ. 5) CALL SETERR(                                     
     1   'DFFTC - PRIME FACTOR GREATER THAN 23',36,2,1)                 
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 6) CALL SETERR(                                     
C    1   44HDFFTC - SQUARE-FREE PRODUCT GREATER THAN 210,44,3,1)        
C/7S                                                                    
      IF (NERR .EQ. 6) CALL SETERR(                                     
     1   'DFFTC - SQUARE-FREE PRODUCT GREATER THAN 210',44,3,1)         
C/                                                                      
C                                                                       
 10   CALL LEAVE                                                        
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DFFTCI(N,FR,FI)                                        
C                                                                       
C DFFTCI FINDS THE INVERSE FOURIER TRANSFORM BY                         
C CALLING DFFT TO INVERT THE TRANSFORM WHOSE REAL                       
C AND COMPLEX FOURIER COEFFICIENTS ARE PROVIDED                         
C IN THE ARRAYS FR AND FI.                                              
C                                                                       
C STORAGE IN THE DYNAMIC STORAGE STACK IS REQUIRED FOR                  
C 220 INTEGER VARIABLES AND 92 DOUBLE PRECISION VARIABLES.              
C                                                                       
C     INPUT                                                             
C                                                                       
C       N     -NUMBER OF COEFFICIENTS                                   
C       FR    -VECTOR OF LENGTH N CONTAINING THE REAL COEFFICIENTS      
C       FI    -VECTOR OF LENGTH N CONTAINING THE IMAGINARY COEFFICIENTS 
C                                                                       
C     OUTPUT                                                            
C                                                                       
C       FR    -CONTAINS THE REAL COMPONENTS OF THE INVERSE TRANSFORM    
C       FI    -CONTAINS THE IMAGINARY COMPONENTS                        
C                                                                       
C                                                                       
C NUMBER OF FACTORS OF N MUST NOT EXCEED 11.                            
C MAXIMUM PRIME FACTOR OF N MUST NOT EXCEED 23.                         
C PRODUCT OF THE SQUARE-FREE FACTORS, IF THERE EXISTS MORE THAN ONE,    
C MUST NOT EXCEED 210.                                                  
C                                                                       
C                                                                       
C     ERROR STATES                                                      
C          1.  N IS LESS THAN 2                                         
C          2.  PRIME FACTOR .GT. 23                                     
C              (RECOVERABLE)                                            
C          3.  SQUARE-FREE FACTOR PRODUCT .GT. 210                      
C              (RECOVERABLE)                                            
C                                                                       
C                                                                       
C COMMON AREA                                                           
      COMMON/CSTAK/DSTAK(500)                                           
C                                                                       
      DOUBLE PRECISION FR(1),FI(1)                                      
      DOUBLE PRECISION DSTAK                                            
C                                                                       
C  TEST THE VALIDITY OF THE INPUTS                                      
C                                                                       
C/6S                                                                    
C     IF(N .LT. 2) CALL SETERR(                                         
C    1   25HDFFTCI - N IS LESS THAN 2,25,1,2)                           
C/7S                                                                    
      IF(N .LT. 2) CALL SETERR(                                         
     1   'DFFTCI - N IS LESS THAN 2',25,1,2)                            
C/                                                                      
C ENTER THE RECOVERY MODE (STORING THE PREVIOUS)                        
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C  CALL THE SUBPROGRAM, DFFT.                                           
C                                                                       
      CALL DFFT(FR,FI,N,N,N,-1)                                         
C                                                                       
C  CHECK FOR ERRORS FROM DFFT                                           
C                                                                       
      IF (NERROR(NERR) .EQ. 0) GO TO 10                                 
C                                                                       
      CALL ERROFF                                                       
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 5) CALL SETERR(                                     
C    1   37HDFFTCI - PRIME FACTOR GREATER THAN 23,37,2,1)               
C/7S                                                                    
      IF (NERR .EQ. 5) CALL SETERR(                                     
     1   'DFFTCI - PRIME FACTOR GREATER THAN 23',37,2,1)                
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 6) CALL SETERR(                                     
C    1   45HDFFTCI - SQUARE-FREE PRODUCT GREATER THAN 210,45,3,1)       
C/7S                                                                    
      IF (NERR .EQ. 6) CALL SETERR(                                     
     1   'DFFTCI - SQUARE-FREE PRODUCT GREATER THAN 210',45,3,1)        
C/                                                                      
C                                                                       
 10   CALL LEAVE                                                        
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DFFT(A,B,NTOT,N,NSPAN,ISN)                             
C                                                                       
C DFFT IS THE DOUBLE-PRECISION VERSION OF                               
C SINGLETON S ORIGINAL FFT.                                             
C                                                                       
C DFFT SETS UP STACK STORAGE FOR DF1FT, GOES INTO RECOVERY MODE,        
C CALLS DF1FT, AND ON RETURN CHECKS FOR, AND REINTERPRETS               
C ANY ERRORS THAT HAVE OCCURRED, THEN RESTORES THE                      
C PREVIOUS RECOVERY MODE AND RETURNS.                                   
C                                                                       
C DFFT MAKES ERRORS IN THE CALLING PARAMETERS FATAL,                    
C BUT OTHER ERRORS RECOVERABLE, SINCE DFFT, IN TURN, IS CALLED BY       
C OUTER LEVEL ROUTINES.                                                 
C                                                                       
C STORAGE IN THE DYNAMIC STORAGE STACK IS SET UP FOR                    
C NFAC(11),NP(209),AT(23),CK(23),BT(23),SK(23), REQUIRING               
C 220 INTEGER LOCATIONS AND 92 DOUBLE-PRECISION LOCATIONS.              
C                                                                       
C                                                                       
C                                                                       
C     INPUT                                                             
C                                                                       
C       A     -VECTOR OF SIZE NTOT CONTAINING REAL ELEMENTS OF DATA     
C       B     -VECTOR OF SIZE NTOT CONTAINING IMAGINARY ELEMENTS OF DATA
C      NTOT   -TOTAL NUMBER OF COMPLEX DATA POINTS                      
C       N     -NUMBER OF COMPLEX DATA POINTS OF THE CURRENT VARIABLE    
C     NSPAN  -NUMBER OF ELEMENTS OF BOTH A AND B NECESSARY TO SPAN      
C              ALL VALUES OF THE CURRENT VARIABLE                       
C              (FOR A SINGLE VARIATE TRANSFORM NTOT=N=NSPAN.  FOR A     
C              MULTIVARIATE TRANSFORM N AND NSPAN ARE DIFFERENT IN      
C              EACH CALL STATEMENT.  SEE EXAMPLE BELOW).                
C      ISN   -DETERMINES BY SIGN THE TYPE OF TRANSFORM BEING COMPUTED,  
C              FORWARD OR INVERSE                                       
C             -INDICATES BY ABSOLUTE VALUE THE VECTOR ARRANGEMENT OF    
C              INPUT DATA (AND TRANSFORM OUTPUT)                        
C             =+1 COMPLEX INPUT DATA ARE IN TWO VECTORS, A AND B.       
C              REAL COMPONENTS IN A, IMAGINARY COMPONENTS IN B.         
C             =-1 FOURIER COEFFICIENT INPUT VALUES ARE IN TWO VECTORS,  
C              A AND B.  COSINE VALUES IN A, SINE VALUES IN B.          
C             =+2 COMPLEX INPUT DATA ARE STORED ALTERNATELY IN A        
C              SINGLE COMPLEX VECTOR, A.  REAL VALUES ARE               
C              IN A(1),A(3),... AND IMAGINARY VALUES ARE                
C              IN A(2),A(4),....  SECOND ARGUMENT OF                    
C              DFFT SHOULD BE A(2).  (SEE EXAMPLE.)                     
C             =-2 FOURIER COEFFICIENT INPUT VALUES ARE STORED ALTER-    
C              NATELY IN A SINGLE VECTOR, A.  COSINE VALUES ARE IN      
C              A(1),A(3),... AND SINE VALUES ARE IN A(2),A(4),....      
C              SECOND ARGUMENT OF DFFT SHOULD BE A(2).                  
C                                                                       
C                                                                       
C     OUTPUT                                                            
C                                                                       
C       A     -VECTOR OF SIZE NTOT CONTAINING COSINE COMPONENTS OF      
C              FOURIER COEFFICIENTS                                     
C       B     -VECTOR OF SIZE NTOT CONTAINING SINE COMPONENTS OF FOURIER
C              COEFFICIENTS                                             
C                                                                       
C IF ISN=1 MULTIPLY OUTPUT BY 2/N FOR UNIT MAGNITUDE.                   
C                                                                       
C IF ISN=2 OUTPUT WILL ALTERNATE IN A SINGLE VECTOR, A.  COSINE         
C COEFFICIENTS IN A(1),A(3),... AND SINE COEFFICIENTS IN A(2),A(4),.... 
C OUTPUT SHOULD BE MULTIPLIED BY 4/N FOR UNIT MAGNITUDE.                
C                                                                       
C                                                                       
C NUMBER OF FACTORS OF N MUST NOT EXCEED 11.                            
C MAXIMUM PRIME FACTOR OF N MUST NOT EXCEED 23.                         
C PRODUCT OF THE SQUARE-FREE FACTORS, IF THERE EXISTS MORE THAN ONE,    
C MUST NOT EXCEED 210.                                                  
C                                                                       
C                                                                       
C     ERROR STATES                                                      
C          1.  N IS LESS THAN 2                                         
C          2.  NTOT IS LESS THAN N                                      
C          3.  NSPAN IS LESS THAN N                                     
C          4.  ABS(ISN) IS GREATER THAN 2                               
C          5.  PRIME FACTOR .GT. 23                                     
C              (RECOVERABLE)                                            
C          6.  SQUARE-FREE FACTOR PRODUCT .GT. 210                      
C              (RECOVERABLE)                                            
C                                                                       
C                                                                       
C EXAMPLES -                                                            
C                                                                       
C TRANSFORM OF N COMPLEX DATA VALUES STORED IN TWO VECTORS -            
C                CALL DFFT(A,B,N,N,N,1)                                 
C                                                                       
C INVERSE TRANSFORM OF FOURIER COEFFICIENTS STORED IN TWO VECTORS -     
C                CALL DFFT(A,B,N,N,N,-1)                                
C                                                                       
C TRANSFORM OF N COMPLEX DATA VALUES STORED ALTERNATELY IN A SINGLE     
C COMPLEX VECTOR -                                                      
C                CALL DFFT(A,A(2),2*N,N,2*N,2)                          
C                                                                       
C INVERSE TRANSFORM OF N FOURIER COEFFICIENTS, COSINE AND SINE COMPO-   
C NENTS STORED ALTERNATELY IN A SINGLE VECTOR -                         
C                CALL DFFT(A,A(2),2*N,N,2*N,-2)                         
C                                                                       
C FOR A MULTIVARIATE TRANSFORM THERE IS NO LIMIT ON THE NUMBER OF       
C IMPLIED SUBSCRIPTS.  DFFT SHOULD BE CALLED ONCE FOR EACH VARIABLE     
C AND THE CALLS MAY BE MADE IN ANY ORDER.                               
C                                                                       
C TRANSFORM OF A TRI-VARIATE, A(N1,N2,N3), B(N1,N2,N3) -                
C                NTOT=N1*N2*N3                                          
C                CALL DFFT(A,B,NTOT,N1,N1,1)                            
C                CALL DFFT(A,B,NTOT,N2,N1*N2,1)                         
C                CALL DFFT(A,B,NTOT,N3,NTOT,1)                          
C                                                                       
C                                                                       
C FOR TRANSFORM OF COMPLETELY REAL DATA USE DRLTR IN CONJUNCTION WITH   
C DFFT.                                                                 
C                                                                       
C                                                                       
C OTHER ROUTINES CALLED -  SETERR                                       
C                                                                       
C                                                                       
C THE PORT LIBRARY VERSION OF DFFT IS AS SHOWN IN THE SINGLETON         
C REFERENCE EXCEPT FOR CHANGES IN THE ERROR HANDLING, AND THE           
C FACT THAT IT HAS BEEN MADE INTO DOUBLE PRECISION.                     
C                                                                       
C                                                                       
C REFERENCE-  SINGLETON, R. C.,  AN ALGORITHM FOR COMPUTING THE MIXED   
C             RADIX FAST FOURIER TRANSFORM , IEEE TRANSACTIONS ON AUDIO 
C             AND ELECTROACOUSTICS, VOL. AU-17, NO. 2, JUNE, 1969       
C             PP. 93-103.                                               
C                                                                       
C COMMON AREA                                                           
      COMMON/CSTAK/DSTAK(500)                                           
C                                                                       
      INTEGER ISTAK(1000)                                               
      DOUBLE PRECISION A(1),B(1)                                        
      DOUBLE PRECISION DSTAK                                            
C                                                                       
      EQUIVALENCE (DSTAK(1),ISTAK(1))                                   
C                                                                       
C  THE FOLLOWING TWO CONSTANTS SHOULD AGREE WITH THE ARRAY DIMENSIONS.  
C                                                                       
C                                                                       
C  TEST THE VALIDITY OF THE INPUTS                                      
C                                                                       
C/6S                                                                    
C     IF(N .LT. 2) CALL SETERR(                                         
C    1   23HDFFT - N IS LESS THAN 2,23,1,2)                             
C     IF(NTOT .LT. N) CALL SETERR(                                      
C    1   26HDFFT - NTOT IS LESS THAN N,26,2,2)                          
C     IF(NSPAN .LT. N) CALL SETERR(                                     
C    1   27HDFFT - NSPAN IS LESS THAN N,27,3,2)                         
C     IF(IABS(ISN) .GT. 2) CALL SETERR(                                 
C    1   39HDFFT - ISN HAS MAGNITUDE GREATER THAN 2,39,4,2)             
C/7S                                                                    
      IF(N .LT. 2) CALL SETERR(                                         
     1   'DFFT - N IS LESS THAN 2',23,1,2)                              
      IF(NTOT .LT. N) CALL SETERR(                                      
     1   'DFFT - NTOT IS LESS THAN N',26,2,2)                           
      IF(NSPAN .LT. N) CALL SETERR(                                     
     1   'DFFT - NSPAN IS LESS THAN N',27,3,2)                          
      IF(IABS(ISN) .GT. 2) CALL SETERR(                                 
     1   'DFFT - ISN HAS MAGNITUDE GREATER THAN 2',39,4,2)              
C/                                                                      
C                                                                       
C ENTER THE RECOVERY MODE (STORING THE PREVIOUS)                        
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C SET UP STORAGE IN THE DYNAMIC STORAGE STACK                           
C                                                                       
      INFAC = ISTKGT(220,2)                                             
      INP   = INFAC + 11                                                
C                                                                       
      IAT   = ISTKGT(92,4)                                              
      ICK   = IAT   + 23                                                
      IBT   = ICK   + 23                                                
      ISK   = IBT   + 23                                                
C                                                                       
C  CALL THE SUBPROGRAM, DF1FT, WHICH DOES THE WORK.                     
C                                                                       
      CALL DF1FT(A,B,NTOT,N,NSPAN,ISN,                                  
     1         ISTAK(INFAC),ISTAK(INP),DSTAK(IAT),                      
     1         DSTAK(ICK),DSTAK(IBT),DSTAK(ISK))                        
C                                                                       
C  CHECK FOR ERRORS FROM DF1FT                                          
C                                                                       
      IF (NERROR(NERR) .EQ. 0) GO TO 10                                 
      CALL ERROFF                                                       
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 1) CALL SETERR(                                     
C    1   35HDFFT - PRIME FACTOR GREATER THAN 23,35,5,1)                 
C/7S                                                                    
      IF (NERR .EQ. 1) CALL SETERR(                                     
     1   'DFFT - PRIME FACTOR GREATER THAN 23',35,5,1)                  
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NERR .EQ. 2) CALL SETERR(                                     
C    1   43HDFFT - SQUARE-FREE PRODUCT GREATER THAN 210,43,6,1)         
C/7S                                                                    
      IF (NERR .EQ. 2) CALL SETERR(                                     
     1   'DFFT - SQUARE-FREE PRODUCT GREATER THAN 210',43,6,1)          
C/                                                                      
C                                                                       
 10   CALL LEAVE                                                        
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DF1FT(A,B,NTOT,N,NSPAN,ISN,NFAC,NP,AT,CK,BT,SK)        
C                                                                       
C THIS SUBROUTINE COMPUTES THE MIXED-RADIX FOURIER TRANSFORM OR ITS     
C INVERSE FOR COMPLEX DATA.  IT CAN COMPUTE A SINGLE VARIATE TRANS-     
C FORM WITH ONE CALL STATEMENT OR A MULTIVARIATE TRANSFORM WITH A       
C SERIES OF CALL STATEMENTS.                                            
C                                                                       
C THE METHOD USED IS SINGLETON S ALGORITHM TO EVALUATE                  
C                                                                       
C                      N-1                                              
C                 Y(K)=   X(J)EXP(I2(PI)JK/N) K=0,1,. . .,N-1           
C                      J=0                                              
C                                                                       
C WHERE Y(K) AND X(J) ARE BOTH COMPLEX VALUED VECTORS.                  
C                                                                       
C                                                                       
C     INPUT                                                             
C                                                                       
C       A     -VECTOR OF SIZE NTOT CONTAINING REAL ELEMENTS OF DATA     
C       B     -VECTOR OF SIZE NTOT CONTAINING IMAGINARY ELEMENTS OF DATA
C      NTOT   -TOTAL NUMBER OF COMPLEX DATA POINTS                      
C       N     -NUMBER OF COMPLEX DATA POINTS OF THE CURRENT VARIABLE    
C     NSPAN  -NUMBER OF ELEMENTS OF BOTH A AND B NECESSARY TO SPAN      
C              ALL VALUES OF THE CURRENT VARIABLE                       
C              (FOR A SINGLE VARIATE TRANSFORM NTOT=N=NSPAN.  FOR A     
C              MULTIVARIATE TRANSFORM N AND NSPAN ARE DIFFERENT IN      
C              EACH CALL STATEMENT.  SEE EXAMPLE BELOW).                
C      ISN   -DETERMINES BY SIGN THE TYPE OF TRANSFORM BEING COMPUTED,  
C              FORWARD OR INVERSE                                       
C             -INDICATES BY ABSOLUTE VALUE THE VECTOR ARRANGEMENT OF    
C              INPUT DATA (AND TRANSFORM OUTPUT)                        
C             =+1 COMPLEX INPUT DATA ARE IN TWO VECTORS, A AND B.       
C              REAL COMPONENTS IN A, IMAGINARY COMPONENTS IN B.         
C             =-1 FOURIER COEFFICIENT INPUT VALUES ARE IN TWO VECTORS,  
C              A AND B.  COSINE VALUES IN A, SINE VALUES IN B.          
C             =+2 COMPLEX INPUT DATA ARE STORED ALTERNATELY IN A        
C              SINGLE COMPLEX VECTOR, A.  REAL VALUES ARE               
C              IN A(1),A(3),... AND IMAGINARY VALUES ARE                
C              IN A(2),A(4),....  SECOND ARGUMENT OF                    
C              DF1FT SHOULD BE A(2).  (SEE EXAMPLE.)                    
C             =-2 FOURIER COEFFICIENT INPUT VALUES ARE STORED ALTER-    
C              NATELY IN A SINGLE VECTOR, A.  COSINE VALUES ARE IN      
C              A(1),A(3),... AND SINE VALUES ARE IN A(2),A(4),....      
C              SECOND ARGUMENT OF DF1FT SHOULD BE A(2).                 
C                                                                       
C                                                                       
C     OUTPUT                                                            
C                                                                       
C       A     -VECTOR OF SIZE NTOT CONTAINING COSINE COMPONENTS OF      
C              FOURIER COEFFICIENTS                                     
C       B     -VECTOR OF SIZE NTOT CONTAINING SINE COMPONENTS OF FOURIER
C              COEFFICIENTS                                             
C                                                                       
C IF ISN=1 MULTIPLY OUTPUT BY 2/N FOR UNIT MAGNITUDE.                   
C                                                                       
C IF ISN=2 OUTPUT WILL ALTERNATE IN A SINGLE VECTOR, A.  COSINE         
C COEFFICIENTS IN A(1),A(3),... AND SINE COEFFICIENTS IN A(2),A(4),.... 
C OUTPUT SHOULD BE MULTIPLIED BY 4/N FOR UNIT MAGNITUDE.                
C                                                                       
C                                                                       
C NUMBER OF FACTORS OF N MUST NOT EXCEED 11.                            
C MAXIMUM PRIME FACTOR OF N MUST NOT EXCEED 23.                         
C PRODUCT OF THE SQUARE-FREE FACTORS, IF THERE EXISTS MORE THAN ONE,    
C MUST NOT EXCEED 210.                                                  
C                                                                       
C                                                                       
C     ERROR STATES                                                      
C          1.   PRIME FACTOR .GT. 23                                    
C          2.   SQUARE-FREE FACTOR PRODUCT .GT. 2.  10                  
C                                                                       
C     STACK STORAGE - NONE                                              
C                                                                       
C                                                                       
C                                                                       
C EXAMPLES -                                                            
C                                                                       
C TRANSFORM OF N COMPLEX DATA VALUES STORED IN TWO VECTORS -            
C                CALL DFFT(A,B,N,N,N,1)                                 
C                                                                       
C INVERSE TRANSFORM OF FOURIER COEFFICIENTS STORED IN TWO VECTORS -     
C                CALL DFFT(A,B,N,N,N,-1)                                
C                                                                       
C TRANSFORM OF N COMPLEX DATA VALUES STORED ALTERNATELY IN A SINGLE     
C COMPLEX VECTOR -                                                      
C                CALL DFFT(A,A(2),2*N,N,2*N,2)                          
C                                                                       
C INVERSE TRANSFORM OF N FOURIER COEFFICIENTS, COSINE AND SINE COMPO-   
C NENTS STORED ALTERNATELY IN A SINGLE VECTOR -                         
C                CALL DFFT(A,A(2),2*N,N,2*N,-2)                         
C                                                                       
C FOR A MULTIVARIATE TRANSFORM THERE IS NO LIMIT ON THE NUMBER OF       
C IMPLIED SUBSCRIPTS.  DFFT SHOULD BE CALLED ONCE FOR EACH VARIABLE     
C AND THE CALLS MAY BE MADE IN ANY ORDER.                               
C                                                                       
C TRANSFORM OF A TRI-VARIATE, A(N1,N2,N3), B(N1,N2,N3) -                
C                NTOT=N1*N2*N3                                          
C                CALL DFFT(A,B,NTOT,N1,N1,1)                            
C                CALL DFFT(A,B,NTOT,N2,N1*N2,1)                         
C                CALL DFFT(A,B,NTOT,N3,NTOT,1)                          
C                                                                       
C                                                                       
C FOR TRANSFORM OF COMPLETELY REAL DATA USE DRLTR IN CONJUNCTION WITH   
C DFFT.                                                                 
C                                                                       
C                                                                       
C OTHER ROUTINES CALLED -  SETERR                                       
C                                                                       
C                                                                       
C REFERENCE -  SINGLETON, R. C.,  AN ALGORITHM FOR COMPUTING THE MIXED  
C             RADIX FAST FOURIER TRANSFORM , IEEE TRANSACTIONS ON AUDIO 
C             AND ELECTROACOUSTICS, VOL. AU-17, NO. 2, JUNE, 1969       
C             PP. 93-103.                                               
C                                                                       
      INTEGER NFAC(2),NP(2)                                             
      DOUBLE PRECISION A(1),B(1)                                        
      DOUBLE PRECISION AT(1),CK(1),BT(1),SK(1)                          
      DOUBLE PRECISION AA,AJ,AJM,AJP,AK,AKM,AKP                         
      DOUBLE PRECISION BB,BJ,BJM,BJP,BK,BKM,BKP                         
      DOUBLE PRECISION C1,C2,C3,C72,CD                                  
      DOUBLE PRECISION RAD,RADF,S120,S1,S2,S3,S72,SD                    
      DOUBLE PRECISION DATAN,DCOS,DSIN,DSQRT                            
      EQUIVALENCE (I,II)                                                
C  THE FOLLOWING TWO CONSTANTS SHOULD AGREE WITH THE ARRAY DIMENSIONS.  
C                                                                       
      MAXF=23                                                           
      MAXP=209                                                          
      IF(N .LT. 2) RETURN                                               
      RAD=8.0D0*DATAN(1.0D0)                                            
      S72=RAD/5.0D0                                                     
      C72=DCOS(S72)                                                     
      S72=DSIN(S72)                                                     
      S120=DSQRT(0.75D0)                                                
      INC=ISN                                                           
      IF(ISN .GE. 0) GO TO 10                                           
      S72=-S72                                                          
      S120=-S120                                                        
      RAD=-RAD                                                          
      INC=-INC                                                          
   10 NT=INC*NTOT                                                       
      KS=INC*NSPAN                                                      
      KSPAN=KS                                                          
      NN=NT-INC                                                         
      JC=KS/N                                                           
      RADF=FLOAT(JC)                                                    
      RADF=RAD*RADF*0.5D0                                               
      I=0                                                               
      JF=0                                                              
C  DETERMINE THE FACTORS OF N                                           
      M=0                                                               
      K=N                                                               
      GO TO 20                                                          
   15 M=M+1                                                             
      NFAC(M)=4                                                         
      K=K/16                                                            
   20 IF(K-(K/16)*16 .EQ. 0) GO TO 15                                   
      J=3                                                               
      JJ=9                                                              
      GO TO 30                                                          
   25 M=M+1                                                             
      NFAC(M)=J                                                         
      K=K/JJ                                                            
   30 IF(MOD(K,JJ) .EQ. 0) GO TO 25                                     
      J=J+2                                                             
      JJ=J**2                                                           
      IF(JJ .LE. K) GO TO 30                                            
      IF(K .GT. 4) GO TO 40                                             
      KT=M                                                              
      NFAC(M+1)=K                                                       
      IF(K .NE. 1) M=M+1                                                
      GO TO 80                                                          
   40 IF(K-(K/4)*4 .NE. 0) GO TO 50                                     
      M=M+1                                                             
      NFAC(M)=2                                                         
      K=K/4                                                             
   50 KT=M                                                              
      J=2                                                               
   60 IF(MOD(K,J) .NE. 0) GO TO 70                                      
      M=M+1                                                             
      NFAC(M)=J                                                         
      K=K/J                                                             
   70 J=((J+1)/2)*2+1                                                   
      IF(J .LE. K) GO TO 60                                             
   80 IF(KT .EQ. 0) GO TO 100                                           
      J=KT                                                              
   90 M=M+1                                                             
      NFAC(M)=NFAC(J)                                                   
      J=J-1                                                             
      IF(J .NE. 0) GO TO 90                                             
C  COMPUTE FOURIER TRANSFORM                                            
  100 SD=FLOAT(KSPAN)                                                   
      SD=RADF/SD                                                        
      CD=2.0D0*DSIN(SD)**2                                              
      SD=DSIN(SD+SD)                                                    
      KK=1                                                              
      I=I+1                                                             
      IF(NFAC(I) .NE. 2) GO TO 400                                      
C  TRANSFORM FOR FACTOR OF 2 (INCLUDING ROTATION FACTOR)                
      KSPAN=KSPAN/2                                                     
      K1=KSPAN+2                                                        
  210 K2=KK+KSPAN                                                       
      AK=A(K2)                                                          
      BK=B(K2)                                                          
      A(K2)=A(KK)-AK                                                    
      B(K2)=B(KK)-BK                                                    
      A(KK)=A(KK)+AK                                                    
      B(KK)=B(KK)+BK                                                    
      KK=K2+KSPAN                                                       
      IF(KK .LE. NN) GO TO 210                                          
      KK=KK-NN                                                          
      IF(KK .LE. JC) GO TO 210                                          
      IF(KK .GT. KSPAN) GO TO 800                                       
  220 C1=1.0D0-CD                                                       
      S1=SD                                                             
  230 K2=KK+KSPAN                                                       
      AK=A(KK)-A(K2)                                                    
      BK=B(KK)-B(K2)                                                    
      A(KK)=A(KK)+A(K2)                                                 
      B(KK)=B(KK)+B(K2)                                                 
      A(K2)=C1*AK-S1*BK                                                 
      B(K2)=S1*AK+C1*BK                                                 
      KK=K2+KSPAN                                                       
      IF(KK .LT. NT) GO TO 230                                          
      K2=KK-NT                                                          
      C1=-C1                                                            
      KK=K1-K2                                                          
      IF(KK .GT. K2) GO TO 230                                          
      AK=C1-(CD*C1+SD*S1)                                               
      S1=(SD*C1-CD*S1)+S1                                               
C  THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION             
C  ERROR. IF ROUNDED ARITHMETIC IS USED, ONE CAN SUBSTITUTE             
C     C1=AK                                                             
      C1=0.5D0/(AK**2+S1**2)+0.5D0                                      
      S1=C1*S1                                                          
      C1=C1*AK                                                          
      KK=KK+JC                                                          
      IF(KK .LT. K2) GO TO 230                                          
      K1=K1+INC+INC                                                     
      KK=(K1-KSPAN)/2+JC                                                
      IF(KK .LE. JC+JC) GO TO 220                                       
      GO TO 100                                                         
C  TRANSFORM FOR FACTOR OF 3 (OPTIONAL CODE)                            
  320 K1=KK+KSPAN                                                       
      K2=K1+KSPAN                                                       
      AK=A(KK)                                                          
      BK=B(KK)                                                          
      AJ=A(K1)+A(K2)                                                    
      BJ=B(K1)+B(K2)                                                    
      A(KK)=AK+AJ                                                       
      B(KK)=BK+BJ                                                       
      AK=-0.5D0*AJ+AK                                                   
      BK=-0.5D0*BJ+BK                                                   
      AJ=(A(K1)-A(K2))*S120                                             
      BJ=(B(K1)-B(K2))*S120                                             
      A(K1)=AK-BJ                                                       
      B(K1)=BK+AJ                                                       
      A(K2)=AK+BJ                                                       
      B(K2)=BK-AJ                                                       
      KK=K2+KSPAN                                                       
      IF(KK .LT. NN) GO TO 320                                          
      KK=KK-NN                                                          
      IF(KK .LE. KSPAN) GO TO 320                                       
      GO TO 700                                                         
C  TRANSFORM FOR FACTOR OF 4                                            
  400 IF(NFAC(I) .NE. 4) GO TO 600                                      
      KSPNN=KSPAN                                                       
      KSPAN=KSPAN/4                                                     
  410 C1=1.0D0                                                          
      S1=0                                                              
  420 K1=KK+KSPAN                                                       
      K2=K1+KSPAN                                                       
      K3=K2+KSPAN                                                       
      AKP=A(KK)+A(K2)                                                   
      AKM=A(KK)-A(K2)                                                   
      AJP=A(K1)+A(K3)                                                   
      AJM=A(K1)-A(K3)                                                   
      A(KK)=AKP+AJP                                                     
      AJP=AKP-AJP                                                       
      BKP=B(KK)+B(K2)                                                   
      BKM=B(KK)-B(K2)                                                   
      BJP=B(K1)+B(K3)                                                   
      BJM=B(K1)-B(K3)                                                   
      B(KK)=BKP+BJP                                                     
      BJP=BKP-BJP                                                       
      IF(ISN .LT. 0) GO TO 450                                          
      AKP=AKM-BJM                                                       
      AKM=AKM+BJM                                                       
      BKP=BKM+AJM                                                       
      BKM=BKM-AJM                                                       
      IF(S1 .EQ. 0.0D0) GO TO 460                                       
  430 A(K1)=AKP*C1-BKP*S1                                               
      B(K1)=AKP*S1+BKP*C1                                               
      A(K2)=AJP*C2-BJP*S2                                               
      B(K2)=AJP*S2+BJP*C2                                               
      A(K3)=AKM*C3-BKM*S3                                               
      B(K3)=AKM*S3+BKM*C3                                               
      KK=K3+KSPAN                                                       
      IF(KK .LE. NT) GO TO 420                                          
  440 C2=C1-(CD*C1+SD*S1)                                               
      S1=(SD*C1-CD*S1)+S1                                               
C  THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION             
C  ERROR. IF ROUNDED ARITHMETIC IS USED, ONE CAN SUBSTITUTE             
C     C1=C2                                                             
      C1=0.5D0/(C2**2+S1**2)+0.5D0                                      
      S1=C1*S1                                                          
      C1=C1*C2                                                          
      C2=C1**2-S1**2                                                    
      S2=2.0D0*C1*S1                                                    
      C3=C2*C1-S2*S1                                                    
      S3=C2*S1+S2*C1                                                    
      KK=KK-NT+JC                                                       
      IF(KK .LE. KSPAN) GO TO 420                                       
      KK=KK-KSPAN+INC                                                   
      IF(KK .LE. JC) GO TO 410                                          
      IF(KSPAN .EQ. JC) GO TO 800                                       
      GO TO 100                                                         
  450 AKP=AKM+BJM                                                       
      AKM=AKM-BJM                                                       
      BKP=BKM-AJM                                                       
      BKM=BKM+AJM                                                       
      IF(S1 .NE. 0.0D0) GO TO 430                                       
  460 A(K1)=AKP                                                         
      B(K1)=BKP                                                         
      A(K2)=AJP                                                         
      B(K2)=BJP                                                         
      A(K3)=AKM                                                         
      B(K3)=BKM                                                         
      KK=K3+KSPAN                                                       
      IF(KK .LE. NT) GO TO 420                                          
      GO TO 440                                                         
C  TRANSFORM FOR FACTOR OF 5 (OPTIONAL CODE)                            
  510 C2=C72**2-S72**2                                                  
      S2=2.0D0*C72*S72                                                  
  520 K1=KK+KSPAN                                                       
      K2=K1+KSPAN                                                       
      K3=K2+KSPAN                                                       
      K4=K3+KSPAN                                                       
      AKP=A(K1)+A(K4)                                                   
      AKM=A(K1)-A(K4)                                                   
      BKP=B(K1)+B(K4)                                                   
      BKM=B(K1)-B(K4)                                                   
      AJP=A(K2)+A(K3)                                                   
      AJM=A(K2)-A(K3)                                                   
      BJP=B(K2)+B(K3)                                                   
      BJM=B(K2)-B(K3)                                                   
      AA=A(KK)                                                          
      BB=B(KK)                                                          
      A(KK)=AA+AKP+AJP                                                  
      B(KK)=BB+BKP+BJP                                                  
      AK=AKP*C72+AJP*C2+AA                                              
      BK=BKP*C72+BJP*C2+BB                                              
      AJ=AKM*S72+AJM*S2                                                 
      BJ=BKM*S72+BJM*S2                                                 
      A(K1)=AK-BJ                                                       
      A(K4)=AK+BJ                                                       
      B(K1)=BK+AJ                                                       
      B(K4)=BK-AJ                                                       
      AK=AKP*C2+AJP*C72+AA                                              
      BK=BKP*C2+BJP*C72+BB                                              
      AJ=AKM*S2-AJM*S72                                                 
      BJ=BKM*S2-BJM*S72                                                 
      A(K2)=AK-BJ                                                       
      A(K3)=AK+BJ                                                       
      B(K2)=BK+AJ                                                       
      B(K3)=BK-AJ                                                       
      KK=K4+KSPAN                                                       
      IF(KK .LT. NN) GO TO 520                                          
      KK=KK-NN                                                          
      IF(KK .LE. KSPAN) GO TO 520                                       
      GO TO 700                                                         
C  TRANSFORM FOR ODD FACTORS                                            
  600 K=NFAC(I)                                                         
      KSPNN=KSPAN                                                       
      KSPAN=KSPAN/K                                                     
      IF(K .EQ. 3) GO TO 320                                            
      IF(K .EQ. 5) GO TO 510                                            
      IF(K .EQ. JF) GO TO 640                                           
      JF=K                                                              
      S1=RAD/FLOAT(K)                                                   
      C1=DCOS(S1)                                                       
      S1=DSIN(S1)                                                       
      IF(JF .LE. MAXF)  GO TO 625                                       
      CALL ERROFF                                                       
C/6S                                                                    
C     CALL SETERR(28HDF1FT - PRIME FACTOR .GT. 23,28,1,1)               
C/7S                                                                    
      CALL SETERR('DF1FT - PRIME FACTOR .GT. 23',28,1,1)                
C/                                                                      
      RETURN                                                            
  625 CK(JF)=1.0D0                                                      
      SK(JF)=0.0D0                                                      
      J=1                                                               
  630 CK(J)=CK(K)*C1+SK(K)*S1                                           
      SK(J)=CK(K)*S1-SK(K)*C1                                           
      K=K-1                                                             
      CK(K)=CK(J)                                                       
      SK(K)=-SK(J)                                                      
      J=J+1                                                             
      IF(J .LT. K) GO TO 630                                            
  640 K1=KK                                                             
      K2=KK+KSPNN                                                       
      AA=A(KK)                                                          
      BB=B(KK)                                                          
      AK=AA                                                             
      BK=BB                                                             
      J=1                                                               
      K1=K1+KSPAN                                                       
  650 K2=K2-KSPAN                                                       
      J=J+1                                                             
      AT(J)=A(K1)+A(K2)                                                 
      AK=AT(J)+AK                                                       
      BT(J)=B(K1)+B(K2)                                                 
      BK=BT(J)+BK                                                       
      J=J+1                                                             
      AT(J)=A(K1)-A(K2)                                                 
      BT(J)=B(K1)-B(K2)                                                 
      K1=K1+KSPAN                                                       
      IF(K1 .LT. K2) GO TO 650                                          
      A(KK)=AK                                                          
      B(KK)=BK                                                          
      K1=KK                                                             
      K2=KK+KSPNN                                                       
      J=1                                                               
  660 K1=K1+KSPAN                                                       
      K2=K2-KSPAN                                                       
      JJ=J                                                              
      AK=AA                                                             
      BK=BB                                                             
      AJ=0.0D0                                                          
      BJ=0.0D0                                                          
      K=1                                                               
  670 K=K+1                                                             
      AK=AT(K)*CK(JJ)+AK                                                
      BK=BT(K)*CK(JJ)+BK                                                
      K=K+1                                                             
      AJ=AT(K)*SK(JJ)+AJ                                                
      BJ=BT(K)*SK(JJ)+BJ                                                
      JJ=JJ+J                                                           
      IF(JJ .GT. JF) JJ=JJ-JF                                           
      IF(K .LT. JF) GO TO 670                                           
      K=JF-J                                                            
      A(K1)=AK-BJ                                                       
      B(K1)=BK+AJ                                                       
      A(K2)=AK+BJ                                                       
      B(K2)=BK-AJ                                                       
      J=J+1                                                             
      IF(J .LT. K) GO TO 660                                            
      KK=KK+KSPNN                                                       
      IF(KK .LE. NN) GO TO 640                                          
      KK=KK-NN                                                          
      IF(KK .LE. KSPAN) GO TO 640                                       
C  MULTIPLY BY ROTATION FACTOR (EXCEPT FOR FACTORS OF 2 AND 4)          
  700 IF(I .EQ. M) GO TO 800                                            
      KK=JC+1                                                           
  710 C2=1.0D0-CD                                                       
      S1=SD                                                             
  720 C1=C2                                                             
      S2=S1                                                             
      KK=KK+KSPAN                                                       
  730 AK=A(KK)                                                          
      A(KK)=C2*AK-S2*B(KK)                                              
      B(KK)=S2*AK+C2*B(KK)                                              
      KK=KK+KSPNN                                                       
      IF(KK .LE. NT) GO TO 730                                          
      AK=S1*S2                                                          
      S2=S1*C2+C1*S2                                                    
      C2=C1*C2-AK                                                       
      KK=KK-NT+KSPAN                                                    
      IF(KK .LE. KSPNN) GO TO 730                                       
      C2=C1-(CD*C1+SD*S1)                                               
      S1=S1+(SD*C1-CD*S1)                                               
C  THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION             
C  ERROR. IF ROUNDED ARITHMETIC IS USED, THEY MAY BE DELETED.           
      C1=0.5D0/(C2**2+S1**2)+0.5D0                                      
      S1=C1*S1                                                          
      C2=C1*C2                                                          
      KK=KK-KSPNN+JC                                                    
      IF(KK .LE. KSPAN) GO TO 720                                       
      KK=KK-KSPAN+JC+INC                                                
      IF(KK .LE. JC+JC) GO TO 710                                       
      GO TO 100                                                         
C  PERMUTE THE RESULTS TO NORMAL ORDER---DONE IN TWO STAGES             
C  PERMUTATION FOR SQUARE FACTORS OF N                                  
  800 NP(1)=KS                                                          
      IF(KT .EQ. 0) GO TO 890                                           
      K=KT+KT+1                                                         
      IF(M .LT. K) K=K-1                                                
      J=1                                                               
      NP(K+1)=JC                                                        
  810 NP(J+1)=NP(J)/NFAC(J)                                             
      NP(K)=NP(K+1)*NFAC(J)                                             
      J=J+1                                                             
      K=K-1                                                             
      IF(J .LT. K) GO TO 810                                            
      K3=NP(K+1)                                                        
      KSPAN=NP(2)                                                       
      KK=JC+1                                                           
      K2=KSPAN+1                                                        
      J=1                                                               
      IF(N .NE. NTOT) GO TO 850                                         
C  PERMUTATION FOR SINGLE-VARIATE TRANSFORM (OPTIONAL CODE)             
  820 AK=A(KK)                                                          
      A(KK)=A(K2)                                                       
      A(K2)=AK                                                          
      BK=B(KK)                                                          
      B(KK)=B(K2)                                                       
      B(K2)=BK                                                          
      KK=KK+INC                                                         
      K2=KSPAN+K2                                                       
      IF(K2 .LT. KS) GO TO 820                                          
  830 K2=K2-NP(J)                                                       
      J=J+1                                                             
      K2=NP(J+1)+K2                                                     
      IF(K2 .GT. NP(J)) GO TO 830                                       
      J=1                                                               
  840 IF(KK .LT. K2) GO TO 820                                          
      KK=KK+INC                                                         
      K2=KSPAN+K2                                                       
      IF(K2 .LT. KS) GO TO 840                                          
      IF(KK .LT. KS) GO TO 830                                          
      JC=K3                                                             
      GO TO 890                                                         
C  PERMUTATION FOR MULTIVARIATE TRANSFORM                               
  850 K=KK+JC                                                           
  860 AK=A(KK)                                                          
      A(KK)=A(K2)                                                       
      A(K2)=AK                                                          
      BK=B(KK)                                                          
      B(KK)=B(K2)                                                       
      B(K2)=BK                                                          
      KK=KK+INC                                                         
      K2=K2+INC                                                         
      IF(KK .LT. K) GO TO 860                                           
      KK=KK+KS-JC                                                       
      K2=K2+KS-JC                                                       
      IF(KK .LT. NT) GO TO 850                                          
      K2=K2-NT+KSPAN                                                    
      KK=KK-NT+JC                                                       
      IF(K2 .LT. KS) GO TO 850                                          
  870 K2=K2-NP(J)                                                       
      J=J+1                                                             
      K2=NP(J+1)+K2                                                     
      IF(K2 .GT. NP(J)) GO TO 870                                       
      J=1                                                               
  880 IF(KK .LT. K2) GO TO 850                                          
      KK=KK+JC                                                          
      K2=KSPAN+K2                                                       
      IF(K2 .LT. KS) GO TO 880                                          
      IF(KK .LT. KS) GO TO 870                                          
      JC=K3                                                             
  890 IF(2*KT+1 .GE. M) RETURN                                          
      KSPNN=NP(KT+1)                                                    
C  PERMUTATION FOR SQUARE-FREE FACTORS OF N                             
      J=M-KT                                                            
      NFAC(J+1)=1                                                       
  900 NFAC(J)=NFAC(J)*NFAC(J+1)                                         
      J=J-1                                                             
      IF(J .NE. KT) GO TO 900                                           
      KT=KT+1                                                           
      NN=NFAC(KT)-1                                                     
      IF(NN .LE. MAXP)  GO TO 901                                       
      CALL ERROFF                                                       
C/6S                                                                    
C     CALL SETERR(43HDF1FT - SQUARE-FREE FACTOR PRODUCT .GT. 210,43,2,1)
C/7S                                                                    
      CALL SETERR('DF1FT - SQUARE-FREE FACTOR PRODUCT .GT. 210',43,2,1) 
C/                                                                      
      RETURN                                                            
  901 JJ=0                                                              
      J=0                                                               
      GO TO 906                                                         
  902 JJ=JJ-K2                                                          
      K2=KK                                                             
      K=K+1                                                             
      KK=NFAC(K)                                                        
  904 JJ=KK+JJ                                                          
      IF(JJ .GE. K2) GO TO 902                                          
      NP(J)=JJ                                                          
  906 K2=NFAC(KT)                                                       
      K=KT+1                                                            
      KK=NFAC(K)                                                        
      J=J+1                                                             
      IF(J .LE. NN) GO TO 904                                           
C  DETERMINE THE PERMUTATION CYCLES OF LENGTH GREATER THAN 1            
      J=0                                                               
      GO TO 914                                                         
 910  K=KK                                                              
      KK=NP(K)                                                          
      NP(K)=-KK                                                         
      IF(KK .NE. J) GO TO 910                                           
      K3=KK                                                             
  914 J=J+1                                                             
      KK=NP(J)                                                          
      IF(KK .LT. 0) GO TO 914                                           
      IF(KK .NE. J) GO TO 910                                           
      NP(J)=-J                                                          
      IF(J .NE. NN) GO TO 914                                           
      MAXF=INC*MAXF                                                     
C  REORDER A AND B, FOLLOWING THE PERMUTATION CYCLES                    
      GO TO 950                                                         
  924 J=J-1                                                             
      IF(NP(J) .LT. 0) GO TO 924                                        
      JJ=JC                                                             
  926 KSPAN=JJ                                                          
      IF(JJ .GT. MAXF) KSPAN=MAXF                                       
      JJ=JJ-KSPAN                                                       
      K=NP(J)                                                           
      KK=JC*K+II+JJ                                                     
      K1=KK+KSPAN                                                       
      K2=0                                                              
  928 K2=K2+1                                                           
      AT(K2)=A(K1)                                                      
      BT(K2)=B(K1)                                                      
      K1=K1-INC                                                         
      IF(K1 .NE. KK) GO TO 928                                          
  932 K1=KK+KSPAN                                                       
      K2=K1-JC*(K+NP(K))                                                
      K=-NP(K)                                                          
  936 A(K1)=A(K2)                                                       
      B(K1)=B(K2)                                                       
      K1=K1-INC                                                         
      K2=K2-INC                                                         
      IF(K1 .NE. KK) GO TO 936                                          
      KK=K2                                                             
      IF(K .NE. J) GO TO 932                                            
      K1=KK+KSPAN                                                       
      K2=0                                                              
  940 K2=K2+1                                                           
      A(K1)=AT(K2)                                                      
      B(K1)=BT(K2)                                                      
      K1=K1-INC                                                         
      IF(K1 .NE. KK) GO TO 940                                          
      IF(JJ .NE. 0) GO TO 926                                           
      IF(J .NE. 1) GO TO 924                                            
  950 J=K3+1                                                            
      NT=NT-KSPNN                                                       
      II=NT-INC+1                                                       
      IF(NT .GE. 0) GO TO 924                                           
      RETURN                                                            
       END                                                              
      SUBROUTINE DRLTR(A,B,N,ISN)                                       
C                                                                       
C THIS SUBROUTINE COMPLETES THE FOURIER TRANSFORM OF 2*N REAL           
C DATA VALUES.                                                          
C IT PERFORMS A RADIX-2 TRANSFORM ON THE FOURIER COEFFICIENTS WHICH     
C RESULT FROM INITIAL TRANSFORM OF THE 2*N REAL DATA VALUES BY AN       
C N DIMENSIONAL COMPLEX FOURIER TRANSFORM.                              
C THIS PROGRAM MUST BE USED IN CONJUNCTION WITH DFFT.                   
C                                                                       
C THIS IS SINGLETON S PROGRAM, WHICH USES THE SANDE FACTORING.          
C                                                                       
C                                                                       
C   ISN  -DETERMINES BY SIGN THE TYPE OF TRANSFORM BEING COMPUTED,      
C         FORWARD OR INVERSE.                                           
C        -INDICATES BY ABSOLUTE VALUE THE VECTOR ARRANGEMENT OF         
C         INPUT DATA (AND TRANSFORM RESULTS).                           
C                                                                       
C                                                                       
C   IF ISN=1    CALLING SEQUENCE IS-                                    
C                CALL DFFT(A,B,N,N,N,1)                                 
C                CALL DRLTR(A,B,N,1).                                   
C                                                                       
C        THE 2*N REAL INPUT VALUES TO DFFT ARE STORED ALTERNATELY IN TWO
C        VECTORS, A AND B-  A(1),B(1),A(2),B(2),...,A(N),B(N).          
C                                                                       
C        SINCE 2(N+1) COEFFICIENTS RESULT FROM THE TRANSFORM            
C        OF THE ORIGINAL 2N DATA POINTS, THE COSINE COEFFICIENTS        
C        WILL BE IN A(1),A(2),...,A(N+1) - NOTE THAT A MUST BE          
C        DIMENSIONED APPROPRIATELY, AND THE SINE COEFFICIENTS WILL      
C        BE IN B(1),B(2),...,B(N+1), BUT B(N+1) = 0.                    
C                                                                       
C                                                                       
C   IF ISN=2    CALLING SEQUENCE IS-                                    
C                CALL DFFT(A,A(2),N,N,N,2)                              
C                CALL DRLTR(A,A(2),N,2).                                
C                                                                       
C        THE 2*N REAL INPUT VALUES TO DFFT ARE STORED IN A SINGLE       
C        VECTOR, A.                                                     
C        THE OUTPUT WILL BE STORED ALTERNATELY IN A,                    
C        COSINE COEFFICIENTS IN A(1),A(3),...,A(2N+1) AND               
C        SINE COEFFICIENTS IN A(2),A(4),...,A(2N+2).                    
C                                                                       
C                                                                       
C IF ISN=1   DIVIDE OUTPUT BY 2*N FOR UNIT MAGNITUDE.                   
C IF ISN=2   DIVIDE OUTPUT BY N FOR UNIT MAGNITUDE.                     
C                                                                       
C                                                                       
C   IF ISN=-1   CALLING SEQUENCE IS-                                    
C                CALL DRLTR(A,B,N,-1)                                   
C                CALL DFFT(A,B,N,N,N,-1).                               
C                                                                       
C        THE OUTPUT OF REAL VALUES ALTERNATES IN TWO VECTORS, A AND B-  
C        A(1),B(1),A(2),B(2),...,A(N),B(N).                             
C                                                                       
C                                                                       
C   IF ISN=-2   CALLING SEQUENCE IS-                                    
C                CALL DRLTR(A,A(2),N,-2)                                
C                CALL DFFT(A,A(2),N,N,N,-2).                            
C                                                                       
C        THE OUTPUT OF REAL VALUES IS IN A SINGLE VECTOR, A AND         
C        SHOULD BE DIVIDED BY 2 FOR PROPER SCALING.                     
C                                                                       
C                                                                       
C REFERENCE-  SINGLETON, R. C.,  AN ALGORITHM FOR COMPUTING THE MIXED   
C             RADIX FAST FOURIER TRANSFORM , IEEE TRANSACTIONS ON       
C             AUDIO AND ELECTROACOUSTICS, VOL. AU-17, NO. 2,            
C             JUNE, 1969, PP. 93-103.                                   
C                                                                       
C                                                                       
      DOUBLE PRECISION A(1),B(1)                                        
      DOUBLE PRECISION CD,SD,CN,SN                                      
      DOUBLE PRECISION AA,AB,BA,BB,RE,IM                                
      DOUBLE PRECISION DATAN,DSIN                                       
C                                                                       
      INC=IABS(ISN)                                                     
      NK=N*INC+2                                                        
      NH=NK/2                                                           
      SD=FLOAT(N)                                                       
      SD=2.0D0*DATAN(1.0D0)/SD                                          
      CD=2.0D0*DSIN(SD)**2                                              
      SD=DSIN(SD+SD)                                                    
      SN=0.0D0                                                          
      IF(ISN .LT. 0) GO TO 30                                           
      CN=1.0D0                                                          
      A(NK-1)=A(1)                                                      
      B(NK-1)=B(1)                                                      
   10 DO 20 J=1,NH,INC                                                  
      K=NK-J                                                            
      AA=A(J)+A(K)                                                      
      AB=A(J)-A(K)                                                      
      BA=B(J)+B(K)                                                      
      BB=B(J)-B(K)                                                      
      RE=CN*BA+SN*AB                                                    
      IM=SN*BA-CN*AB                                                    
      B(K)=IM-BB                                                        
      B(J)=IM+BB                                                        
      A(K)=AA-RE                                                        
      A(J)=AA+RE                                                        
      AA=CN-(CD*CN+SD*SN)                                               
      SN=(SD*CN-CD*SN)+SN                                               
C    THE FOLLOWING 3 STATEMENTS COMPENSATE FOR TRUNCATION               
C    ERROR. IF ROUNDED ARITHMETIC IS USED, SUBSTITUTE                   
C 20 CN=AA FOR THE FOLLOWING THREE STATEMENTS.                          
      CN=0.5D0/(AA**2+SN**2)+0.5D0                                      
      SN=CN*SN                                                          
   20 CN=CN*AA                                                          
      RETURN                                                            
   30 CN=-1.0D0                                                         
      SD=-SD                                                            
      GO TO 10                                                          
       END                                                              
C****END OF ROUTINES NEEDED FOR PORT 3 TRANSFORMS CHAPTER***************
