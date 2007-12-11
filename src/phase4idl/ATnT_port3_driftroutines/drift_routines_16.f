      SUBROUTINE WRAPUP                                                 
      INTEGER LUSED, I1MACH, NERROR, ISTKST, LMAX, NERR                 
      INTEGER TEMP                                                      
C TO WRAP-UP A RUN BY CHECKING THE STACK AND ERROR STATES.              
C/6S                                                                    
C     IF (NERROR(NERR) .NE. 0) CALL SETERR(                             
C    1   36HWRAPUP - AN ERROR STATE IS LEFT OVER, 36, 1, 2)             
C     IF (ISTKST(1) .GT. 0) CALL SETERR(                                
C    1   32HWRAPUP - STUFF LEFT ON THE STACK, 32, 2, 2)                 
C/7S                                                                    
      IF (NERROR(NERR) .NE. 0) CALL SETERR(                             
     1   'WRAPUP - AN ERROR STATE IS LEFT OVER', 36, 1, 2)              
      IF (ISTKST(1) .GT. 0) CALL SETERR(                                
     1   'WRAPUP - STUFF LEFT ON THE STACK', 32, 2, 2)                  
C/                                                                      
      LUSED = ISTKST(3)                                                 
      LMAX = ISTKST(4)                                                  
      TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  1) LUSED, LMAX                                      
   1  FORMAT (6H USED , I8, 3H / , I8, 22H OF THE STACK ALLOWED.)       
      RETURN                                                            
      END                                                               
      FUNCTION RNORM(IDUMMY)                                            
C                                                                       
      INTEGER ISW                                                       
      REAL F, TEMP, S, V1, V2                                           
      DATA ISW/0/, F/0.E0/, V2/0.E0/                                    
C                                                                       
      IF(ISW .NE. 0) GO TO 20                                           
 10   V1 = UNI(0)                                                       
      V2 = UNI(0)                                                       
C                                                                       
      V1 = V1 + V1-1.0E0                                                
      V2 = V2 + V2-1.0E0                                                
C                                                                       
C  IF THE SUM OF THE SQUARES IS NOT LESS THAN 1,                        
C  TRY AGAIN.                                                           
C                                                                       
      S = V1*V1 + V2*V2                                                 
      IF(S .GE. 1.0E0) GO TO 10                                         
C                                                                       
C  OK - SO GIVE BACK ONE NORMAL DEVIATE AND                             
C  SAVE THE FACTOR, F FOR THE NEXT REQUEST.                             
C                                                                       
      TEMP = ALOG(S)                                                    
      F = SQRT((-TEMP-TEMP)/S)                                          
C                                                                       
C  SET THE SWITCH TO USE THE SAVED STUFF NEXT TIME.                     
C                                                                       
      ISW = 1                                                           
      RNORM = V1*F                                                      
      RETURN                                                            
C                                                                       
C                                                                       
C  ON THE EVEN TIMES IT COMES HERE TO USE THE SAVED STUFF.              
C                                                                       
 20   ISW = 0                                                           
      RNORM = V2*F                                                      
      RETURN                                                            
      END                                                               
      FUNCTION UNI(K)                                                   
      INTEGER IBYTE(4)                                                  
      DATA ICSEED/0/, ITSEED/0/, IFCN/1/                                
C                                                                       
C  UNI IS RETURNED AS A SINGLE REAL RANDOM VARIATE                      
C  FROM THE UNIFORM DISTRIBUTION 0.0 .LE. UNI .LT. 1.0 .                
C                                                                       
C  IFCN = 1 IMPLIES THAT ICSEED, ITSEED, IBYTE, AND K ARE IGNORED.      
C                                                                       
      UNI=R1UNIF(ICSEED,ITSEED,IBYTE,IFCN)                              
      RETURN                                                            
      END                                                               
      SUBROUTINE RANSET(ICSEED,ITSEED)                                  
      INTEGER IBYTE(4)                                                  
      DATA IFCN/0/                                                      
C                                                                       
C  TO (RE)INITIALIZE THE UNIFORM RANDOM NUMBER GENERATOR, R1UNIF        
C  (TO OTHER THAN THE DEFAULT INITIAL VALUES).                          
C                                                                       
C    ICSEED IS THE NEW SEED FOR CONGRUENTIAL GENERATOR.                 
C    ITSEED IS THE NEW SEED FOR TAUSWORTHE GENERATOR.                   
C                                                                       
C  ONE, BUT NOT BOTH, OF THE NEW SEEDS CAN BE ZERO                      
C                                                                       
C/6S                                                                    
C     IF (ICSEED.EQ.0 .AND. ITSEED.EQ.0)                                
C    1   CALL SETERR(34HRANSET - INPUT SEEDS ARE BOTH ZERO,34,1,2)      
C/7S                                                                    
      IF (ICSEED.EQ.0 .AND. ITSEED.EQ.0)                                
     1   CALL SETERR('RANSET - INPUT SEEDS ARE BOTH ZERO',34,1,2)       
C/                                                                      
C                                                                       
C  IFCN = 0 IMPLIES THAT UNI AND IBYTE ARE NOT COMPUTED.                
C                                                                       
      UNI=R1UNIF(ICSEED,ITSEED,IBYTE,IFCN)                              
      RETURN                                                            
      END                                                               
      SUBROUTINE RANBYT(UNI,IBYTE)                                      
      DIMENSION IBYTE(4)                                                
      DATA ICSEED/0/, ITSEED/0/, IFCN/2/                                
C                                                                       
C  UNI IS RETURNED AS A SINGLE UNIFORM RANDOM VARIATE IN UNI.           
C                                                                       
C  IBYTE IS RETURNED WITH THE BITS OF UNI, 8 BITS PER WORD.             
C  UNI=(IBYTE(1)*256**3+IBYTE(2)*256**2+IBYTE(3)*256+IBYTE(4))/2**32    
C                                                                       
C  IFCN = 2 IMPLIES THAT ICSEED AND ITSEED ARE IGNORED.                 
C                                                                       
      UNI=R1UNIF(ICSEED,ITSEED,IBYTE,IFCN)                              
      RETURN                                                            
      END                                                               
      FUNCTION R1UNIF(ICSEED,ITSEED,IBYTE,IFCN)                         
C                                                                       
C  R1UNIF - OUTPUT, THE UNIFORM RANDOM NUMBER IF IFCN .NE. 0            
C  ICSEED - INPUT, THE NEW CONGRUENTIAL SEED IF IFCN = 0                
C  ITSEED - INPUT, THE NEW TAUSWORTHE SEED IF IFCN = 0                  
C  IBYTE  - OUTPUT, THE BITS OF R1UNIF, 8 PER WORD, IF IFCN = 2         
C  IFCN   - INPUT, = 0 FOR INITIALIZATION                               
C                  = 1 IF ONLY THE VALUE OF R1UNIF IS OF INTEREST       
C                  = 2 IF BOTH R1UNIF AND IBYTE ARE OF INTEREST         
C                                                                       
C  THIS IS A PORTABLE FORTRAN IMPLEMENTATION OF UNI, A                  
C  UNIFORM RANDOM NUMBER GENERATOR ON (0.0, 1.0) DEVISED                
C  BY MARSAGLIA, ET. AL., AND INCLUDED IN THEIR PACKAGE                 
C  CALLED  SUPER-DUPER .                                                
C                                                                       
C  TWO INDEPENDENT 32 BIT GENERATORS ARE MAINTAINED INTERNALLY AND      
C  UPDATED FOR EACH CALL.                                               
C                                                                       
C  THE FIRST OF THESE IS A CONGRUENTIAL GENERATOR WITH                  
C  MULTIPLIER 69069 (=16*64**2 + 55*64 + 13).                           
C                                                                       
C  THE SECOND IS A TAUSWORTHE OR SHIFT-REGISTER GENERATOR.              
C  THIS GENERATOR TAKES THE SEED, SHIFTS IT RIGHT 15 BITS, EXCLUSIVE    
C  ORS IT WITH ITSELF, SHIFTS THE RESULT 17 BITS TO THE LEFT, AND       
C  EXCLUSIVE ORS THE SHIFTED RESULT WITH ITSELF (NOT WITH THE           
C  ORIGINAL SEED).  THE OUTPUT OF THE PROCEDURE IS THE TAUSWORTHE       
C  RANDOM NUMBER AND IS USED AS THE SEED FOR THE NEXT CALL.             
C                                                                       
C  FINALLY, THE OUTPUT FROM THE TWO GENERATORS IS                       
C  EXCLUSIVELY OR-ED TOGETHER.                                          
C                                                                       
C  THE FOLLOWING PROGRAM SHOULD WORK ON ANY 16+ BIT COMPUTER.           
C                                                                       
      LOGICAL  FIRST                                                    
      INTEGER CSEED(6), TSEED(32), XOR(29), IBYTE(4), ISCR(5)           
      DATA XOR(1)/1/,XOR(2)/2/,XOR(3)/3/,XOR(4)/3/,XOR(5)/2/,           
     1 XOR(6)/1/,XOR(7)/4/,XOR(8)/5/,XOR(9)/6/,XOR(10)/7/,XOR(11)/5/,   
     2 XOR(12)/4/,XOR(13)/7/,XOR(14)/6/,XOR(15)/1/,XOR(16)/6/,          
     3 XOR(17)/7/,XOR(18)/4/,XOR(19)/5/,XOR(20)/2/,XOR(21)/3/,          
     4 XOR(22)/7/,XOR(23)/6/,XOR(24)/5/,XOR(25)/4/,XOR(26)/3/,          
     5 XOR(27)/2/,XOR(28)/1/,XOR(29)/0/                                 
      DATA FIRST/.TRUE./, JCSEED/12345/, JTSEED/1073/                   
C  INITIALIZE CSEED AND TSEED FOR PORTABILITY                           
      DATA CSEED(1)/0/,CSEED(2)/0/,CSEED(3)/0/,CSEED(4)/0/,             
     1 CSEED(5)/0/,CSEED(6)/0/,TSEED(1)/0/,TSEED(2)/0/,TSEED(3)/0/,     
     2 TSEED(4)/0/,TSEED(5)/0/,TSEED(6)/0/,TSEED(7)/0/,TSEED(8)/0/,     
     3 TSEED(9)/0/,TSEED(10)/0/,TSEED(11)/0/,TSEED(12)/0/,              
     4 TSEED(13)/0/,TSEED(14)/0/,TSEED(15)/0/,TSEED(16)/0/,             
     5 TSEED(17)/0/,TSEED(18)/0/,TSEED(19)/0/,TSEED(20)/0/,             
     6 TSEED(21)/0/,TSEED(22)/0/,TSEED(23)/0/,TSEED(24)/0/,             
     7 TSEED(25)/0/,TSEED(26)/0/,TSEED(27)/0/,TSEED(28)/0/,             
     8 TSEED(29)/0/,TSEED(30)/0/,TSEED(31)/0/,TSEED(32)/0/              
C                                                                       
      R1UNIF=0.0                                                        
      IF((.NOT.FIRST) .AND. (IFCN.GT.0)) GO TO 50                       
      IF(IFCN.GT.0) GO TO 10                                            
C     TAKE USER VALUES AS SEEDS                                         
      JCSEED=IABS(ICSEED)                                               
      JTSEED=IABS(ITSEED)                                               
   10 FIRST=.FALSE.                                                     
C                                                                       
C.....DECODE SEEDS                                                      
C                                                                       
      CSEED(1)=JCSEED                                                   
      DO 20 I=1,5                                                       
      CSEED(I+1)=CSEED(I)/64                                            
   20 CSEED(I)=CSEED(I)-CSEED(I+1)*64                                   
      CSEED(6)=MOD(CSEED(6),4)                                          
C     ENSURE ODD UNLESS ZERO                                            
      IF(JCSEED.NE.0 .AND. MOD(CSEED(1),2).EQ.0) CSEED(1)=CSEED(1)+1    
      TSEED(1)=JTSEED                                                   
      DO 30 I=1,11                                                      
      TSEED(I+1)=TSEED(I)/2                                             
   30 TSEED(I)=TSEED(I)-TSEED(I+1)*2                                    
C     ONLY USE INITIAL VALUE MOD 2048                                   
      DO 40 I=12,32                                                     
   40 TSEED(I)=0                                                        
C     ENSURE ODD UNLESS ZERO                                            
      IF(JTSEED.NE.0) TSEED(1)=1                                        
C     END OF INITIALIZATION                                             
      IF(IFCN.EQ.0) RETURN                                              
   50 CONTINUE                                                          
C                                                                       
C.....TAUSWORTHE GENERATOR -- SHIFT RIGHT 15, THEN LEFT 17              
C                                                                       
      DO 60 I=1,17                                                      
   60 TSEED(I)=IABS(TSEED(I)-TSEED(I+15))                               
      DO 70 I=18,32                                                     
   70 TSEED(I)=IABS(TSEED(I)-TSEED(I-17))                               
C                                                                       
C.....CONGRUENTIAL GENERATOR -- MULTIPLICATION IN BASE 64               
C                                                                       
C     MULTIPLY BASE 64                                                  
      CSEED(6)=13*CSEED(6)+55*CSEED(5)+16*CSEED(4)                      
      CSEED(5)=13*CSEED(5)+55*CSEED(4)+16*CSEED(3)                      
      CSEED(4)=13*CSEED(4)+55*CSEED(3)+16*CSEED(2)                      
      CSEED(3)=13*CSEED(3)+55*CSEED(2)+16*CSEED(1)                      
      CSEED(2)=13*CSEED(2)+55*CSEED(1)                                  
      CSEED(1)=13*CSEED(1)                                              
      K=-5                                                              
      ICARRY=0                                                          
      DO 80 I=1,5                                                       
      K=K+6                                                             
      CSEED(I)=CSEED(I)+ICARRY                                          
      ICARRY=CSEED(I)/64                                                
      CSEED(I)=CSEED(I)-64*ICARRY                                       
      I2=CSEED(I)/8                                                     
      I1=CSEED(I)-8*I2                                                  
      J1=4*TSEED(K+2)+TSEED(K+1)+TSEED(K+1)+TSEED(K)                    
      J2=4*TSEED(K+5)+TSEED(K+4)+TSEED(K+4)+TSEED(K+3)                  
      IT1=28                                                            
      IF(I1.GT.J1) IT1=(I1*I1-I1)/2+J1                                  
      IF(I1.LT.J1) IT1=(J1*J1-J1)/2+I1                                  
      IT2=28                                                            
      IF(I2.GT.J2) IT2=(I2*I2-I2)/2+J2                                  
      IF(I2.LT.J2) IT2=(J2*J2-J2)/2+I2                                  
      ISCR(I)=8*XOR(IT2+1)+XOR(IT1+1)                                   
   80 R1UNIF=(R1UNIF+FLOAT(ISCR(I)))/64.0                               
      CSEED(6)=MOD(CSEED(6)+ICARRY,4)                                   
      J1=TSEED(31)+TSEED(32)+TSEED(32)                                  
      IT1=IABS(CSEED(6)-J1)                                             
      IF((IT1.EQ.1) .AND. (CSEED(6)+J1.EQ.3)) IT1=3                     
      R1UNIF=(R1UNIF+FLOAT(IT1))/4.0                                    
      IF(IFCN.EQ.1) RETURN                                              
      IBYTE(4)=ISCR(1)+MOD(ISCR(2),4)*64                                
      IBYTE(3)=ISCR(2)/4+MOD(ISCR(3),16)*16                             
      IBYTE(2)=ISCR(3)/16+ISCR(4)*4                                     
      IBYTE(1)=ISCR(5)+IT1*64                                           
      RETURN                                                            
      END                                                               
      REAL FUNCTION TRIGP(N, ALPHA, BETA, THETA)                        
      INTEGER N                                                         
      REAL ALPHA(1), BETA(1), THETA                                     
      INTEGER K                                                         
      REAL C, S, MU, GK, GK1, DK                                        
      REAL DK1, COS, SIN                                                
C  TRIGP IS A REAL FORTRAN FUNCTION WHICH RETURNS THE                   
C  VALUE OF A TRIGONOMETRIC POLYNOMIAL, P(THETA), OF DEGREE N,          
C  WHERE P(THETA) = SUM ALPHA(K)COS(K THETA) + BETA(K)SIN(K THETA).     
C  N      - THE DEGREE OF P(X).                                         
C  ALPHA  - THE N+1 COEFFICIENTS OF THE COSINE TERMS.                   
C  BETA   - THE N+1 COEFFICIENTS OF THE SINE TERMS.                     
C           BETA(1) IS NOT USED.                                        
C  THETA  - THE ARGUMENT AT WHICH P(THETA) IS TO BE EVALUATED.          
C  ERROR STATES-                                                        
C       1 - INVALID DEGREE.                                             
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(22HTRIGP - INVALID DEGREE, 22, 1, 2)    
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('TRIGP - INVALID DEGREE', 22, 1, 2)     
C/                                                                      
      IF (N .NE. 0) GOTO 1                                              
         TRIGP = ALPHA(1)                                               
         GOTO  9                                                        
   1     C = COS(THETA)                                                 
         S = SIN(THETA)                                                 
         IF (S .LT. 0.0) GOTO 2                                         
            MU = C/(S+1.0)                                              
            GOTO  3                                                     
   2        MU = (-C)/(1.0-S)                                           
   3     GK1 = ALPHA(N+1)                                               
         DK1 = BETA(N+1)                                                
         K = N                                                          
            GOTO  5                                                     
   4        K = K-1                                                     
   5        IF (K .LT. 2)GOTO  8                                        
            GK = C*GK1+S*DK1                                            
            IF (S .LT. 0.0) GOTO 6                                      
               DK = MU*(GK+DK1)-GK1                                     
               GOTO  7                                                  
   6           DK = MU*(GK-DK1)+GK1                                     
   7        GK1 = GK+ALPHA(K)                                           
            DK1 = DK+BETA(K)                                            
            GOTO  4                                                     
   8     TRIGP = ALPHA(1)+C*GK1+S*DK1                                   
   9  RETURN                                                            
      END                                                               
      REAL FUNCTION TCHBP(N, ALPHA, X, X0, X1)                          
      INTEGER N                                                         
      REAL ALPHA(1), X, X0, X1                                          
      INTEGER K                                                         
      REAL Z, TWOZ, BETAK, BETAK1, BETAK2                               
C  TCHBP IS A REAL FORTRAN FUNCTION WHICH RETURNS THE VALUE             
C  OF A POLYNOMIAL, P(X), OF DEGREE N GIVEN BY ITS TCHEBYCHEFF          
C  EXPANSION ON THE INTERVAL (X0,X1).                                   
C  N      - THE DEGREE OF P(X).                                         
C  ALPHA  - THE N+1 COEFFICIENTS IN THE EXPANSION OF P(X).              
C  X      - THE ARGUMENT AT WHICH P(X) IS TO BE EVALUATED.              
C  X0,X1  - THE LEFT AND RIGHT HAND END POINTS OF THE INTERVAL          
C           FOR WHICH THE EXPANSION IS CONSTRUCTED.                     
C  ERROR STATES-                                                        
C       1 - INVALID DEGREE.                                             
C       2 - INVALID INTERVAL.                                           
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(22HTCHBP - INVALID DEGREE, 22, 1, 2)    
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('TCHBP - INVALID DEGREE', 22, 1, 2)     
C/                                                                      
      IF (N .NE. 0) GOTO 1                                              
         TCHBP = ALPHA(1)                                               
         GOTO  5                                                        
   1     Z = X1-X0                                                      
C  SCALE X TO THE INTERVAL (X0,X1).                                     
C/6S                                                                    
C        IF (Z .LE. 0.0) CALL SETERR(24HTCHBP - INVALID INTERVAL, 24, 2,
C    1      2)                                                          
C/7S                                                                    
         IF (Z .LE. 0.0) CALL SETERR('TCHBP - INVALID INTERVAL', 24, 2, 
     1      2)                                                          
C/                                                                      
         Z = ((X-X0)+(X-X1))/Z                                          
         TWOZ = 2.0*Z                                                   
         BETAK2 = 0.0                                                   
         BETAK1 = ALPHA(N+1)                                            
         K = N                                                          
            GOTO  3                                                     
   2        K = K-1                                                     
   3        IF (K .LT. 2)GOTO  4                                        
            BETAK = TWOZ*BETAK1-BETAK2+ALPHA(K)                         
            BETAK2 = BETAK1                                             
            BETAK1 = BETAK                                              
            GOTO  2                                                     
   4     TCHBP = Z*BETAK1-BETAK2+ALPHA(1)                               
   5  RETURN                                                            
      END                                                               
      REAL FUNCTION ORTHP(N, ALPHA, X, A, B, C)                         
      INTEGER N                                                         
      REAL ALPHA(1), X, A(1), B(1), C(1)                                
      INTEGER K                                                         
      REAL BETAK, BETAK1, BETAK2, Z                                     
C  ORTHP IS A REAL FORTRAN FUNCTION WHICH RETURNS THE                   
C  VALUE OF A POLYNOMIAL, P(X), OF DEGREE N GIVEN BY AN                 
C  ORTHOGONAL EXPANSION.  THE ORTHOGONAL POLYNOMIALS OF THIS            
C  EXPANSION ARE DEFINED BY THE THREE-TERM RECURRENCE                   
C  P0 = 1,  P1 = A(1)X + B(1), AND                                      
C  PK+1 = (A(K+1)X + B(K+1))PK + C(K+1)PK-1.                            
C  N        - THE DEGREE OF P(X).                                       
C  ALPHA    - THE N+1 COEFFICIENTS IN THE EXPANSION OF P(X).            
C  X        - THE ARGUMENT AT WHICH P(X) IS TO BE EVALUATED.            
C  A,B,C    - THE THREE SETS OF N COEFFICIENTS USED TO                  
C             DEFINE THE THREE-TERM RECURRENCE.                         
C  ERROR STATES-                                                        
C       1 - INVALID DEGREE.                                             
C       2 - INVALID RECURRENCE.                                         
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(22HORTHP - INVALID DEGREE, 22, 1, 2)    
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('ORTHP - INVALID DEGREE', 22, 1, 2)     
C/                                                                      
      IF (N .NE. 0) GOTO 1                                              
         ORTHP = ALPHA(1)                                               
         GOTO  5                                                        
   1     BETAK2 = ALPHA(N+1)                                            
C/6S                                                                    
C        IF (A(N) .EQ. 0.0) CALL SETERR(26HORTHP - INVALID RECURRENCE,  
C    1      26, 2, 2)                                                   
C/7S                                                                    
         IF (A(N) .EQ. 0.0) CALL SETERR('ORTHP - INVALID RECURRENCE',   
     1      26, 2, 2)                                                   
C/                                                                      
         BETAK1 = BETAK2*(A(N)*X+B(N))+ALPHA(N)                         
         K = N-1                                                        
            GOTO  3                                                     
   2        K = K-1                                                     
   3        IF (K .LT. 1)GOTO  4                                        
C/6S                                                                    
C           IF (A(K) .EQ. 0.0) CALL SETERR(                             
C    1         26HORTHP - INVALID RECURRENCE, 26, 2, 2)                 
C/7S                                                                    
            IF (A(K) .EQ. 0.0) CALL SETERR(                             
     1         'ORTHP - INVALID RECURRENCE', 26, 2, 2)                  
C/                                                                      
            Z = A(K)*X+B(K)                                             
            BETAK = BETAK1*Z+BETAK2*C(K+1)+ALPHA(K)                     
            BETAK2 = BETAK1                                             
            BETAK1 = BETAK                                              
            GOTO  2                                                     
   4     ORTHP = BETAK1                                                 
   5  RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DTRIGP(N, ALPHA, BETA, THETA)           
      INTEGER N                                                         
      DOUBLE PRECISION ALPHA(1), BETA(1), THETA                         
      INTEGER K                                                         
      DOUBLE PRECISION C, S, MU, GK, GK1, DK                            
      DOUBLE PRECISION DK1, DCOS, DSIN                                  
C  DTRIGP IS A DOUBLE PRECISION FORTRAN FUNCTION WHICH RETURNS THE      
C  VALUE OF A TRIGONOMETRIC POLYNOMIAL, P(THETA), OF DEGREE N,          
C  WHERE P(THETA) = SUM ALPHA(K)COS(K THETA) + BETA(K)SIN(K THETA).     
C  N      - THE DEGREE OF P(X).                                         
C  ALPHA  - THE N+1 COEFFICIENTS OF THE COSINE TERMS.                   
C  BETA   - THE N+1 COEFFICIENTS OF THE SINE TERMS.                     
C           BETA(1) IS NOT USED.                                        
C  THETA  - THE ARGUMENT AT WHICH P(THETA) IS TO BE EVALUATED.          
C  ERROR STATES-                                                        
C       1 - INVALID DEGREE.                                             
      IF (N .GE. 0) GOTO 1                                              
C/6S                                                                    
C        CALL SETERR(23HDTRIGP - INVALID DEGREE, 23, 1, 2)              
C/7S                                                                    
         CALL SETERR('DTRIGP - INVALID DEGREE', 23, 1, 2)               
C/                                                                      
         GOTO  11                                                       
   1     IF (N .NE. 0) GOTO 2                                           
            DTRIGP = ALPHA(1)                                           
            GOTO  10                                                    
   2        C = DCOS(THETA)                                             
            S = DSIN(THETA)                                             
            IF (S .LT. 0.0D0) GOTO 3                                    
               MU = C/(S+1.0D0)                                         
               GOTO  4                                                  
   3           MU = (-C)/(1.0D0-S)                                      
   4        GK1 = ALPHA(N+1)                                            
            DK1 = BETA(N+1)                                             
            K = N                                                       
               GOTO  6                                                  
   5           K = K-1                                                  
   6           IF (K .LT. 2)GOTO  9                                     
               GK = C*GK1+S*DK1                                         
               IF (S .LT. 0.0D0) GOTO 7                                 
                  DK = MU*(GK+DK1)-GK1                                  
                  GOTO  8                                               
   7              DK = MU*(GK-DK1)+GK1                                  
   8           GK1 = GK+ALPHA(K)                                        
               DK1 = DK+BETA(K)                                         
               GOTO  5                                                  
   9        DTRIGP = ALPHA(1)+C*GK1+S*DK1                               
  10  CONTINUE                                                          
  11  RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DTCHBP(N, ALPHA, X, X0, X1)             
      INTEGER N                                                         
      DOUBLE PRECISION ALPHA(1), X, X0, X1                              
      INTEGER K                                                         
      DOUBLE PRECISION Z, TWOZ, BETAK, BETAK1, BETAK2                   
C  DTCHBP IS A DOUBLE PRECISION FORTRAN FUNCTION WHICH RETURNS THE VALUE
C  OF A POLYNOMIAL, P(X), OF DEGREE N GIVEN BY ITS TCHEBYCHEFF          
C  EXPANSION ON THE INTERVAL (X0,X1).                                   
C  N      - THE DEGREE OF P(X).                                         
C  ALPHA  - THE N+1 COEFFICIENTS IN THE EXPANSION OF P(X).              
C  X      - THE ARGUMENT AT WHICH P(X) IS TO BE EVALUATED.              
C  X0,X1  - THE LEFT AND RIGHT HAND END POINTS OF THE INTERVAL          
C           FOR WHICH THE EXPANSION IS CONSTRUCTED.                     
C  ERROR STATES-                                                        
C       1 - INVALID DEGREE.                                             
C       2 - INVALID INTERVAL.                                           
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(23HDTCHBP - INVALID DEGREE, 23, 1, 2)   
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('DTCHBP - INVALID DEGREE', 23, 1, 2)    
C/                                                                      
      IF (N .NE. 0) GOTO 1                                              
         DTCHBP = ALPHA(1)                                              
         GOTO  5                                                        
   1     Z = X1-X0                                                      
C  SCALE X TO THE INTERVAL (X0,X1).                                     
C/6S                                                                    
C        IF (Z .LE. 0.0) CALL SETERR(25HDTCHBP - INVALID INTERVAL, 25, 2
C    1      , 2)                                                        
C/7S                                                                    
         IF (Z .LE. 0.0) CALL SETERR('DTCHBP - INVALID INTERVAL', 25, 2 
     1      , 2)                                                        
C/                                                                      
         Z = ((X-X0)+(X-X1))/Z                                          
         TWOZ = 2.0D0*Z                                                 
         BETAK2 = 0.0D0                                                 
         BETAK1 = ALPHA(N+1)                                            
         K = N                                                          
            GOTO  3                                                     
   2        K = K-1                                                     
   3        IF (K .LT. 2)GOTO  4                                        
            BETAK = TWOZ*BETAK1-BETAK2+ALPHA(K)                         
            BETAK2 = BETAK1                                             
            BETAK1 = BETAK                                              
            GOTO  2                                                     
   4     DTCHBP = Z*BETAK1-BETAK2+ALPHA(1)                              
   5  RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DORTHP(N, ALPHA, X, A, B, C)            
      INTEGER N                                                         
      DOUBLE PRECISION ALPHA(1), X, A(1), B(1), C(1)                    
      INTEGER K                                                         
      DOUBLE PRECISION BETAK, BETAK1, BETAK2, Z                         
C  DORTHP IS A DOUBLE PRECISION FORTRAN FUNCTION WHICH RETURNS THE      
C  VALUE OF A POLYNOMIAL, P(X), OF DEGREE N GIVEN BY AN                 
C  ORTHOGONAL EXPANSION.  THE ORTHOGONAL POLYNOMIALS OF THIS            
C  EXPANSION ARE DEFINED BY THE THREE-TERM RECURRENCE                   
C  P0 = 1,  P1 = A(1)X + B(1), AND                                      
C  PK+1 = (A(K+1)X + B(K+1))PK + C(K+1)PK-1.                            
C  N      - THE DEGREE OF P(X).                                         
C  ALPHA  - THE N+1 COEFFICIENTS IN THE EXPANSION OF P(X).              
C  X      - THE ARGUMENT AT WHICH P(X) IS TO BE EVALUATED.              
C  A,B,C  - THE THREE SETS OF N COEFFICIENTS USED TO                    
C           DEFINE THE THREE-TERM RECURRENCE.                           
C  ERROR STATES-                                                        
C       1 - INVALID DEGREE.                                             
C       2 - INVALID RECURRENCE.                                         
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(23HDORTHP - INVALID DEGREE, 23, 1, 2)   
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('DORTHP - INVALID DEGREE', 23, 1, 2)    
C/                                                                      
      IF (N .NE. 0) GOTO 1                                              
         DORTHP = ALPHA(1)                                              
         GOTO  5                                                        
   1     BETAK2 = ALPHA(N+1)                                            
C/6S                                                                    
C        IF (A(N) .EQ. 0.0D0) CALL SETERR(                              
C    1      27HDORTHP - INVALID RECURRENCE, 27, 2, 2)                   
C/7S                                                                    
         IF (A(N) .EQ. 0.0D0) CALL SETERR(                              
     1      'DORTHP - INVALID RECURRENCE', 27, 2, 2)                    
C/                                                                      
         BETAK1 = BETAK2*(A(N)*X+B(N))+ALPHA(N)                         
         K = N-1                                                        
            GOTO  3                                                     
   2        K = K-1                                                     
   3        IF (K .LT. 1)GOTO  4                                        
C/6S                                                                    
C           IF (A(K) .EQ. 0.0D0) CALL SETERR(                           
C    1         27HDORTHP - INVALID RECURRENCE, 27, 2, 2)                
C/7S                                                                    
            IF (A(K) .EQ. 0.0D0) CALL SETERR(                           
     1         'DORTHP - INVALID RECURRENCE', 27, 2, 2)                 
C/                                                                      
            Z = A(K)*X+B(K)                                             
            BETAK = BETAK1*Z+BETAK2*C(K+1)+ALPHA(K)                     
            BETAK2 = BETAK1                                             
            BETAK1 = BETAK                                              
            GOTO  2                                                     
   4     DORTHP = BETAK1                                                
   5  RETURN                                                            
      END                                                               
      SUBROUTINE  SYM(N, X)                                             
      INTEGER N, MID, JJ, J                                             
      REAL X(1)                                                         
C                                                                       
C MAKE X ARRAY SYMMETRIC FRONT-TO-BACK                                  
C                                                                       
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(16H  SYM - N .LT. 0, 16, 1, 2)          
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('  SYM - N .LT. 0', 16, 1, 2)           
C/                                                                      
      IF (N .LE. 1) RETURN                                              
C                                                                       
      MID = N/2                                                         
      JJ = N                                                            
      DO 10 J = 1, MID                                                  
         X(JJ) = X(JJ) + 0.5E0*(X(J)-X(JJ))                             
         X(J) = X(JJ)                                                   
         JJ = JJ-1                                                      
   10 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE ASYM(N, X)                                             
      INTEGER N, MID, JJ, J                                             
      REAL X(1)                                                         
C                                                                       
C MAKE X ARRAY ANTISYMMETRIC FRONT-TO-BACK                              
C                                                                       
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(16H ASYM - N .LT. 0, 16, 1, 2)          
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR(' ASYM - N .LT. 0', 16, 1, 2)           
C/                                                                      
      IF (N .EQ. 1) X(1) = 0.E0                                         
      IF (N .LE. 1) RETURN                                              
C                                                                       
      MID = N/2                                                         
      JJ = N                                                            
      DO 10 J = 1, MID                                                  
         X(JJ) = X(JJ) - 0.5E0*(X(J)+X(JJ))                             
         X(J) = -X(JJ)                                                  
         JJ = JJ-1                                                      
   10 CONTINUE                                                          
      IF (JJ .GT. MID) X(JJ) = 0.E0                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE  DSYM(N, X)                                            
      INTEGER N, MID, JJ, J                                             
      DOUBLE PRECISION X(1)                                             
C                                                                       
C MAKE X ARRAY SYMMETRIC FRONT-TO-BACK                                  
C                                                                       
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(16H DSYM - N .LT. 0, 16, 1, 2)          
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR(' DSYM - N .LT. 0', 16, 1, 2)           
C/                                                                      
      IF (N .LE. 1) RETURN                                              
C                                                                       
      MID = N/2                                                         
      JJ = N                                                            
      DO 10 J = 1, MID                                                  
         X(JJ) = X(JJ) + 0.5D0*(X(J)-X(JJ))                             
         X(J) = X(JJ)                                                   
         JJ = JJ-1                                                      
   10 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE DASYM(N, X)                                            
      INTEGER N, MID, JJ, J                                             
      DOUBLE PRECISION X(1)                                             
C                                                                       
C MAKE X ARRAY ANTISYMMETRIC FRONT-TO-BACK                              
C                                                                       
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(16HDASYM - N .LT. 0, 16, 1, 2)          
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('DASYM - N .LT. 0', 16, 1, 2)           
C/                                                                      
      IF (N .EQ. 1 ) X(1) = 0.D0                                        
      IF (N .LE. 1) RETURN                                              
C                                                                       
      MID = N/2                                                         
      JJ = N                                                            
      DO 10 J = 1, MID                                                  
         X(JJ) = X(JJ) - 0.5D0*(X(J)+X(JJ))                             
         X(J) = -X(JJ)                                                  
         JJ = JJ-1                                                      
   10 CONTINUE                                                          
      IF (JJ .GT. MID) X(JJ) = 0.D0                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE SRTAD ( A, SA, N )                                     
C                                                                       
C     SRTAD SORTS THE N ITEMS                                           
C     A(1), A(SA+1), ..., A((N-1)*SA+1)                                 
C     INTO ASCENDING ORDER                                              
C                                                                       
      INTEGER SA, H                                                     
      DOUBLE PRECISION A(1), TEMP                                       
C                                                                       
C     CHECK INPUT PARAMETERS                                            
C                                                                       
      NN = I0SRT( SA, N, H )                                            
C                                                                       
 10     IF ( H .LT. SA ) RETURN                                         
        K = NN - H                                                      
C                                                                       
C       COMPARE                                                         
C                                                                       
        DO 30 J = 1, K, SA                                              
          I = J                                                         
 20         IH = I + H                                                  
            IF ( A(IH) .GE. A(I) ) GOTO 30                              
C                                                                       
C           EXCHANGE                                                    
C                                                                       
            TEMP = A(IH)                                                
            A(IH) = A(I)                                                
            A(I) = TEMP                                                 
C                                                                       
C           PERCOLATE EXCHANGED ITEM UP LIST TO PROPER PLACE            
C                                                                       
            I = I - H                                                   
            IF (I .GE. 1) GOTO 20                                       
 30       CONTINUE                                                      
C                                                                       
        H = (H - SA) / 3                                                
        GOTO 10                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTAH ( A, L, SA, N )                                  
C                                                                       
C     SRTAH SORTS THE N HOLLERITH DATA ITEMS,                           
C     EACH L WORDS IN LENGTH, STARTING AT                               
C     A(1), A(SA+1), ..., A((N-1)*SA+1)                                 
C     INTO ASCENDING COLLATING ORDER                                    
C                                                                       
      INTEGER A(1), SA, H                                               
C                                                                       
      NN = I0SRT( SA, N, H )                                            
C                                                                       
C/6S                                                                    
C     IF ( L .GT. SA  .OR.  L .LT. 1 )                                  
C    1   CALL SETERR( 26HSRTAH - ILLEGAL VALUE OF L, 26, 3, 2 )         
C/7S                                                                    
      IF ( L .GT. SA  .OR.  L .LT. 1 )                                  
     1   CALL SETERR( 'SRTAH - ILLEGAL VALUE OF L', 26, 3, 2 )          
C/                                                                      
C                                                                       
 10     IF ( H .LT. SA ) RETURN                                         
        K = NN - H                                                      
C                                                                       
C       MAIN LOOP - INSERTION SORT ON DATA ITEMS H APART IN A           
C                                                                       
        DO 90 J = 1, K, SA                                              
          I = J                                                         
 20         M = I + L - 1                                               
C                                                                       
C           COMPARE DATA ITEMS AT A(I+H) AND A(I)                       
C                                                                       
            DO 60 K1 = I, M                                             
              K2 = K1 + H                                               
              IF ( A(K1) ) 30, 40, 40                                   
 30           IF ( A(K2) ) 50, 70, 70                                   
 40           IF ( A(K2) ) 90, 50, 50                                   
 50           IF ( A(K1) - A(K2) ) 90, 60, 70                           
 60           CONTINUE                                                  
C                                                                       
C           SWITCH DATA ITEMS AT A(I+H) AND A(I)                        
C                                                                       
 70         DO 80 K1 = I, M                                             
              K2 = K1 + H                                               
              NTEMP = A(K1)                                             
              A(K1) = A(K2)                                             
              A(K2) = NTEMP                                             
 80           CONTINUE                                                  
C                                                                       
            I = I - H                                                   
            IF ( I .GE. 1 ) GOTO 20                                     
 90       CONTINUE                                                      
C                                                                       
        H = (H - SA) / 3                                                
        GOTO 10                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTAI ( A, SA, N )                                     
C                                                                       
C     SRTAI SORTS THE N ITEMS                                           
C     A(1), A(SA+1), ..., A((N-1)*SA+1)                                 
C     INTO ASCENDING ORDER                                              
C                                                                       
      INTEGER SA, H                                                     
      INTEGER A(1), TEMP                                                
C                                                                       
C     CHECK INPUT PARAMETERS                                            
C                                                                       
      NN = I0SRT( SA, N, H )                                            
C                                                                       
 10     IF ( H .LT. SA ) RETURN                                         
        K = NN - H                                                      
C                                                                       
C       COMPARE                                                         
C                                                                       
        DO 30 J = 1, K, SA                                              
          I = J                                                         
 20         IH = I + H                                                  
            IF ( A(IH) .GE. A(I) ) GOTO 30                              
C                                                                       
C           EXCHANGE                                                    
C                                                                       
            TEMP = A(IH)                                                
            A(IH) = A(I)                                                
            A(I) = TEMP                                                 
C                                                                       
C           PERCOLATE EXCHANGED ITEM UP LIST TO PROPER PLACE            
C                                                                       
            I = I - H                                                   
            IF (I .GE. 1) GOTO 20                                       
 30       CONTINUE                                                      
C                                                                       
        H = (H - SA) / 3                                                
        GOTO 10                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTAR ( A, SA, N )                                     
C                                                                       
C     SRTAR SORTS THE N ITEMS                                           
C     A(1), A(SA+1), ..., A((N-1)*SA+1)                                 
C     INTO ASCENDING ORDER                                              
C                                                                       
      INTEGER SA, H                                                     
      REAL A(1), TEMP                                                   
C                                                                       
C     CHECK INPUT PARAMETERS                                            
C                                                                       
      NN = I0SRT( SA, N, H )                                            
C                                                                       
 10     IF ( H .LT. SA ) RETURN                                         
        K = NN - H                                                      
C                                                                       
C       COMPARE                                                         
C                                                                       
        DO 30 J = 1, K, SA                                              
          I = J                                                         
 20         IH = I + H                                                  
            IF ( A(IH) .GE. A(I) ) GOTO 30                              
C                                                                       
C           EXCHANGE                                                    
C                                                                       
            TEMP = A(IH)                                                
            A(IH) = A(I)                                                
            A(I) = TEMP                                                 
C                                                                       
C           PERCOLATE EXCHANGED ITEM UP LIST TO PROPER PLACE            
C                                                                       
            I = I - H                                                   
            IF (I .GE. 1) GOTO 20                                       
 30       CONTINUE                                                      
C                                                                       
        H = (H - SA) / 3                                                
        GOTO 10                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTDD ( A, SA, N )                                     
C                                                                       
C     SRTDD SORTS THE N ITEMS                                           
C     A(1), A(SA+1), ..., A((N-1)*SA+1)                                 
C     INTO DESCENDING ORDER                                             
C                                                                       
      INTEGER SA, H                                                     
      DOUBLE PRECISION A(1), TEMP                                       
C                                                                       
C     CHECK INPUT PARAMETERS                                            
C                                                                       
      NN = I0SRT( SA, N, H )                                            
C                                                                       
 10     IF ( H .LT. SA ) RETURN                                         
        K = NN - H                                                      
C                                                                       
C       COMPARE                                                         
C                                                                       
        DO 30 J = 1, K, SA                                              
          I = J                                                         
 20         IH = I + H                                                  
            IF ( A(IH) .LE. A(I) ) GOTO 30                              
C                                                                       
C           EXCHANGE                                                    
C                                                                       
            TEMP = A(IH)                                                
            A(IH) = A(I)                                                
            A(I) = TEMP                                                 
C                                                                       
C           PERCOLATE EXCHANGED ITEM UP LIST TO PROPER PLACE            
C                                                                       
            I = I - H                                                   
            IF (I .GE. 1) GOTO 20                                       
 30       CONTINUE                                                      
C                                                                       
        H = (H - SA) / 3                                                
        GOTO 10                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTDH ( A, L, SA, N )                                  
C                                                                       
C     SRTDH SORTS THE N HOLLERITH DATA ITEMS,                           
C     EACH L WORDS IN LENGTH, STARTING AT                               
C     A(1), A(SA+1), ..., A((N-1)*SA+1)                                 
C     INTO DESCENDING COLLATING ORDER                                   
C                                                                       
      INTEGER A(1), SA, H                                               
C                                                                       
      NN = I0SRT( SA, N, H )                                            
C/6S                                                                    
C     IF ( L .GT. SA  .OR.  L .LT. 1 )                                  
C    1   CALL SETERR( 26HSRTDH - ILLEGAL VALUE OF L, 26, 3, 2 )         
C/7S                                                                    
      IF ( L .GT. SA  .OR.  L .LT. 1 )                                  
     1   CALL SETERR( 'SRTDH - ILLEGAL VALUE OF L', 26, 3, 2 )          
C/                                                                      
C                                                                       
 10     IF ( H .LT. SA ) RETURN                                         
        K = NN - H                                                      
C                                                                       
C       MAIN LOOP - INSERTION SORT ON DATA ITEMS H APART IN A           
C                                                                       
        DO 90 J = 1, K, SA                                              
          I = J                                                         
 20         M = I + L - 1                                               
C                                                                       
C           COMPARE DATA ITEMS AT A(I+H) AND A(I)                       
C                                                                       
            DO 60 K1 = I, M                                             
              K2 = K1 + H                                               
              IF ( A(K1) ) 30, 40, 40                                   
 30           IF ( A(K2) ) 50, 90, 90                                   
 40           IF ( A(K2) ) 70, 50, 50                                   
 50           IF ( A(K1) - A(K2) ) 70, 60, 90                           
 60           CONTINUE                                                  
C                                                                       
C           SWITCH DATA ITEMS AT A(I+H) AND A(I)                        
C                                                                       
 70         DO 80 K1 = I, M                                             
              K2 = K1 + H                                               
              NTEMP = A(K1)                                             
              A(K1) = A(K2)                                             
              A(K2) = NTEMP                                             
 80           CONTINUE                                                  
C                                                                       
            I = I - H                                                   
            IF ( I .GE. 1 ) GOTO 20                                     
 90       CONTINUE                                                      
C                                                                       
        H = (H - SA) / 3                                                
        GOTO 10                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTDI ( A, SA, N )                                     
C                                                                       
C     SRTDI SORTS THE N ITEMS                                           
C     A(1), A(SA+1), ..., A((N-1)*SA+1)                                 
C     INTO DESCENDING ORDER                                             
C                                                                       
      INTEGER SA, H                                                     
      INTEGER A(1), TEMP                                                
C                                                                       
C     CHECK INPUT PARAMETERS                                            
C                                                                       
      NN = I0SRT( SA, N, H )                                            
C                                                                       
 10     IF ( H .LT. SA ) RETURN                                         
        K = NN - H                                                      
C                                                                       
C       COMPARE                                                         
C                                                                       
        DO 30 J = 1, K, SA                                              
          I = J                                                         
 20         IH = I + H                                                  
            IF ( A(IH) .LE. A(I) ) GOTO 30                              
C                                                                       
C           EXCHANGE                                                    
C                                                                       
            TEMP = A(IH)                                                
            A(IH) = A(I)                                                
            A(I) = TEMP                                                 
C                                                                       
C           PERCOLATE EXCHANGED ITEM UP LIST TO PROPER PLACE            
C                                                                       
            I = I - H                                                   
            IF (I .GE. 1) GOTO 20                                       
 30       CONTINUE                                                      
C                                                                       
        H = (H - SA) / 3                                                
        GOTO 10                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTDR ( A, SA, N )                                     
C                                                                       
C     SRTDR SORTS THE N ITEMS                                           
C     A(1), A(SA+1), ..., A((N-1)*SA+1)                                 
C     INTO DESCENDING ORDER                                             
C                                                                       
      INTEGER SA, H                                                     
      REAL A(1), TEMP                                                   
C                                                                       
C     CHECK INPUT PARAMETERS                                            
C                                                                       
      NN = I0SRT( SA, N, H )                                            
C                                                                       
 10     IF ( H .LT. SA ) RETURN                                         
        K = NN - H                                                      
C                                                                       
C       COMPARE                                                         
C                                                                       
        DO 30 J = 1, K, SA                                              
          I = J                                                         
 20         IH = I + H                                                  
            IF ( A(IH) .LE. A(I) ) GOTO 30                              
C                                                                       
C           EXCHANGE                                                    
C                                                                       
            TEMP = A(IH)                                                
            A(IH) = A(I)                                                
            A(I) = TEMP                                                 
C                                                                       
C           PERCOLATE EXCHANGED ITEM UP LIST TO PROPER PLACE            
C                                                                       
            I = I - H                                                   
            IF (I .GE. 1) GOTO 20                                       
 30       CONTINUE                                                      
C                                                                       
        H = (H - SA) / 3                                                
        GOTO 10                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTPAD ( A, SA, P, SP, N )                             
C                                                                       
C     SRTPAD SETS P(1) = 1, P(SP+1) = 2, ..., P((N-1)*SP+1) = N         
C     AND THEN REARRANGES P(1), P(SP+1), ..., P((N-1)*SP+1) SO THAT     
C     A( (P(I)-1)*SA+1 ) .LE. A( (P(J)-1)*SA+1 ) IF AND ONLY IF         
C     I .LT. J, WHERE I AND J SUBSCRIPT PROPER ELEMENTS OF P            
C                                                                       
      INTEGER SP, P(SP, 1), SA, H, PIH, PI                              
      DOUBLE PRECISION A(SA, 1)                                         
C                                                                       
C     CHECK INPUT PARAMETERS AND INITIALIZE H                           
C                                                                       
      CALL I1SRT( SA, SP, N )                                           
      IF ( I0SRT( 1, N, H ) .LT. 1 ) RETURN                             
C                                                                       
C     INITIALIZE P                                                      
C                                                                       
      DO 10 I = 1, N                                                    
 10     P(1, I) = I                                                     
C                                                                       
C       CHECK IF DONE WITH SORT                                         
C                                                                       
 20     IF ( H .LT. 1 ) RETURN                                          
        K = N - H                                                       
C                                                                       
C       COMPARE                                                         
C                                                                       
        DO 40 J = 1, K                                                  
          I = J                                                         
 30         IH = I + H                                                  
            PI = P(1, I)                                                
            PIH = P(1, IH)                                              
            IF ( A(1, PI) .LE. A(1, PIH) ) GOTO 40                      
C                                                                       
C           EXCHANGE                                                    
C                                                                       
            P(1, I) = PIH                                               
            P(1, IH) = PI                                               
C                                                                       
C           PERCOLATE EXCHANGED LIST ELEMENT UP TO PROPER PLACE         
C                                                                       
            I = I - H                                                   
            IF ( I .GE. 1 ) GOTO 30                                     
 40       CONTINUE                                                      
C                                                                       
        H = ( H - 1 ) / 3                                               
        GOTO 20                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTPAH ( A, L, SA, P, SP, N )                          
C                                                                       
C     SRTPAH SETS P(1) = 1, P(SP+1) = 2, ..., P((N-1)*SP+1) = N         
C     AND THEN REARRANGES P(1), P(SP+1), ..., P((N-1)*SP+1) SO THAT     
C     A( (P(I)-1)*SA+1 ) .LE. A( (P(J)-1)*SA+1 ) IF AND ONLY IF         
C     I .LT. J, WHERE I AND J SUBSCRIPT PROPER ELEMENTS OF P            
C                                                                       
      INTEGER SP, P(SP, 1), SA, H, PIH, PI                              
      INTEGER A(SA, 1)                                                  
C                                                                       
C     CHECK INPUT PARAMETERS AND INITIALIZE H                           
C                                                                       
      CALL I1SRT( SA, SP, N )                                           
C/6S                                                                    
C     IF ( L .GT. SA  .OR.  L .LT. 1 )                                  
C    1   CALL SETERR( 27HSRTPAH - ILLEGAL VALUE OF L, 27, 4, 2)         
C/7S                                                                    
      IF ( L .GT. SA  .OR.  L .LT. 1 )                                  
     1   CALL SETERR( 'SRTPAH - ILLEGAL VALUE OF L', 27, 4, 2)          
C/                                                                      
      IF ( I0SRT( 1, N, H ) .LT. 1 ) RETURN                             
C                                                                       
C     INITIALIZE P                                                      
C                                                                       
      DO 10 I = 1, N                                                    
 10     P(1, I) = I                                                     
C                                                                       
C       CHECK IF DONE WITH SORT                                         
C                                                                       
 20     IF ( H .LT. 1 ) RETURN                                          
        K = N - H                                                       
C                                                                       
C       COMPARE                                                         
C                                                                       
        DO 100 J = 1, K                                                 
          I = J                                                         
 30         IH = I + H                                                  
            PI = P(1, I)                                                
            PIH = P(1, IH)                                              
C                                                                       
C           CHECK ORDER OF HOLLERITH DATA                               
C                                                                       
            DO 70 M = 1, L                                              
              IF ( A(M, PI) ) 40, 50, 50                                
 40           IF ( A(M, PIH) ) 60, 80, 80                               
 50           IF ( A(M, PIH) ) 100, 60, 60                              
 60           IF ( A(M, PI) - A(M, PIH) ) 100, 70, 80                   
 70           CONTINUE                                                  
C                                                                       
C           EXCHANGE                                                    
C                                                                       
 80         P(1, I) = P(1, IH)                                          
            P(1, IH) = PI                                               
C                                                                       
C           PERCOLATE EXCHANGED LIST ELEMENT UP TO PROPER PLACE         
C                                                                       
            I = I - H                                                   
            IF ( I .GE. 1 ) GOTO 30                                     
 100      CONTINUE                                                      
C                                                                       
        H = ( H - 1 ) / 3                                               
        GOTO 20                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTPAI ( A, SA, P, SP, N )                             
C                                                                       
C     SRTPAI SETS P(1) = 1, P(SP+1) = 2, ..., P((N-1)*SP+1) = N         
C     AND THEN REARRANGES P(1), P(SP+1), ..., P((N-1)*SP+1) SO THAT     
C     A( (P(I)-1)*SA+1 ) .LE. A( (P(J)-1)*SA+1 ) IF AND ONLY IF         
C     I .LT. J, WHERE I AND J SUBSCRIPT PROPER ELEMENTS OF P            
C                                                                       
      INTEGER SP, P(SP, 1), SA, H, PIH, PI                              
      INTEGER A(SA, 1)                                                  
C                                                                       
C     CHECK INPUT PARAMETERS AND INITIALIZE H                           
C                                                                       
      CALL I1SRT( SA, SP, N )                                           
      IF ( I0SRT( 1, N, H ) .LT. 1 ) RETURN                             
C                                                                       
C     INITIALIZE P                                                      
C                                                                       
      DO 10 I = 1, N                                                    
 10     P(1, I) = I                                                     
C                                                                       
C       CHECK IF DONE WITH SORT                                         
C                                                                       
 20     IF ( H .LT. 1 ) RETURN                                          
        K = N - H                                                       
C                                                                       
C       COMPARE                                                         
C                                                                       
        DO 40 J = 1, K                                                  
          I = J                                                         
 30         IH = I + H                                                  
            PI = P(1, I)                                                
            PIH = P(1, IH)                                              
            IF ( A(1, PI) .LE. A(1, PIH) ) GOTO 40                      
C                                                                       
C           EXCHANGE                                                    
C                                                                       
            P(1, I) = PIH                                               
            P(1, IH) = PI                                               
C                                                                       
C           PERCOLATE EXCHANGED LIST ELEMENT UP TO PROPER PLACE         
C                                                                       
            I = I - H                                                   
            IF ( I .GE. 1 ) GOTO 30                                     
 40       CONTINUE                                                      
C                                                                       
        H = ( H - 1 ) / 3                                               
        GOTO 20                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTPAR ( A, SA, P, SP, N )                             
C                                                                       
C     SRTPAR SETS P(1) = 1, P(SP+1) = 2, ..., P((N-1)*SP+1) = N         
C     AND THEN REARRANGES P(1), P(SP+1), ..., P((N-1)*SP+1) SO THAT     
C     A( (P(I)-1)*SA+1 ) .LE. A( (P(J)-1)*SA+1 ) IF AND ONLY IF         
C     I .LT. J, WHERE I AND J SUBSCRIPT PROPER ELEMENTS OF P            
C                                                                       
      INTEGER SP, P(SP, 1), SA, H, PIH, PI                              
      REAL A(SA, 1)                                                     
C                                                                       
C     CHECK INPUT PARAMETERS AND INITIALIZE H                           
C                                                                       
      CALL I1SRT( SA, SP, N )                                           
      IF ( I0SRT( 1, N, H ) .LT. 1 ) RETURN                             
C                                                                       
C     INITIALIZE P                                                      
C                                                                       
      DO 10 I = 1, N                                                    
 10     P(1, I) = I                                                     
C                                                                       
C       CHECK IF DONE WITH SORT                                         
C                                                                       
 20     IF ( H .LT. 1 ) RETURN                                          
        K = N - H                                                       
C                                                                       
C       COMPARE                                                         
C                                                                       
        DO 40 J = 1, K                                                  
          I = J                                                         
 30         IH = I + H                                                  
            PI = P(1, I)                                                
            PIH = P(1, IH)                                              
            IF ( A(1, PI) .LE. A(1, PIH) ) GOTO 40                      
C                                                                       
C           EXCHANGE                                                    
C                                                                       
            P(1, I) = PIH                                               
            P(1, IH) = PI                                               
C                                                                       
C           PERCOLATE EXCHANGED LIST ELEMENT UP TO PROPER PLACE         
C                                                                       
            I = I - H                                                   
            IF ( I .GE. 1 ) GOTO 30                                     
 40       CONTINUE                                                      
C                                                                       
        H = ( H - 1 ) / 3                                               
        GOTO 20                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTPDD ( A, SA, P, SP, N )                             
C                                                                       
C     SRTPDD SETS P(1) = 1, P(SP+1) = 2, ..., P((N-1)*SP+1) = N         
C     AND THEN REARRANGES P(1), P(SP+1), ..., P((N-1)*SP+1) SO THAT     
C     A( (P(I)-1)*SA+1 ) .GE. A( (P(J)-1)*SA+1 ) IF AND ONLY IF         
C     I .GT. J, WHERE I AND J SUBSCRIPT PROPER ELEMENTS OF P            
C                                                                       
      INTEGER SP, P(SP, 1), SA, H, PIH, PI                              
      DOUBLE PRECISION A(SA, 1)                                         
C                                                                       
C     CHECK INPUT PARAMETERS AND INITIALIZE H                           
C                                                                       
      CALL I1SRT( SA, SP, N )                                           
      IF ( I0SRT( 1, N, H ) .LT. 1 ) RETURN                             
C                                                                       
C     INITIALIZE P                                                      
C                                                                       
      DO 10 I = 1, N                                                    
 10     P(1, I) = I                                                     
C                                                                       
C       CHECK IF DONE WITH SORT                                         
C                                                                       
 20     IF ( H .LT. 1 ) RETURN                                          
        K = N - H                                                       
C                                                                       
C       COMPARE                                                         
C                                                                       
        DO 40 J = 1, K                                                  
          I = J                                                         
 30         IH = I + H                                                  
            PI = P(1, I)                                                
            PIH = P(1, IH)                                              
            IF ( A(1, PI) .GE. A(1, PIH) ) GOTO 40                      
C                                                                       
C           EXCHANGE                                                    
C                                                                       
            P(1, I) = PIH                                               
            P(1, IH) = PI                                               
C                                                                       
C           PERCOLATE EXCHANGED LIST ELEMENT UP TO PROPER PLACE         
C                                                                       
            I = I - H                                                   
            IF ( I .GE. 1 ) GOTO 30                                     
 40       CONTINUE                                                      
C                                                                       
        H = ( H - 1 ) / 3                                               
        GOTO 20                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTPDH ( A, L, SA, P, SP, N )                          
C                                                                       
C     SRTPDH SETS P(1) = 1, P(SP+1) = 2, ..., P((N-1)*SP+1) = N         
C     AND THEN REARRANGES P(1), P(SP+1), ..., P((N-1)*SP+1) SO THAT     
C     A( (P(I)-1)*SA+1 ) .GE. A( (P(J)-1)*SA+1 ) IF AND ONLY IF         
C     I .GT. J, WHERE I AND J SUBSCRIPT PROPER ELEMENTS OF P            
C                                                                       
      INTEGER SP, P(SP, 1), SA, H, PIH, PI                              
      INTEGER A(SA, 1)                                                  
C                                                                       
C     CHECK INPUT PARAMETERS AND INITIALIZE H                           
C                                                                       
      CALL I1SRT( SA, SP, N )                                           
C/6S                                                                    
C     IF ( L .GT. SA  .OR.  L .LT. 1 )                                  
C    1   CALL SETERR( 27HSRTPDH - ILLEGAL VALUE OF L, 27, 4, 2)         
C/7S                                                                    
      IF ( L .GT. SA  .OR.  L .LT. 1 )                                  
     1   CALL SETERR( 'SRTPDH - ILLEGAL VALUE OF L', 27, 4, 2)          
C/                                                                      
      IF ( I0SRT( 1, N, H ) .LT. 1 ) RETURN                             
C                                                                       
C     INITIALIZE P                                                      
C                                                                       
      DO 10 I = 1, N                                                    
 10     P(1, I) = I                                                     
C                                                                       
C       CHECK IF DONE WITH SORT                                         
C                                                                       
 20     IF ( H .LT. 1 ) RETURN                                          
        K = N - H                                                       
C                                                                       
C       COMPARE                                                         
C                                                                       
        DO 100 J = 1, K                                                 
          I = J                                                         
 30         IH = I + H                                                  
            PI = P(1, I)                                                
            PIH = P(1, IH)                                              
C                                                                       
C           CHECK ORDER OF HOLLERITH DATA                               
C                                                                       
            DO 70 M = 1, L                                              
              IF ( A(M, PI) ) 40, 50, 50                                
 40           IF ( A(M, PIH) ) 60, 100, 100                             
 50           IF ( A(M, PIH) ) 80, 60, 60                               
 60           IF ( A(M, PI) - A(M, PIH) ) 80, 70, 100                   
 70           CONTINUE                                                  
C                                                                       
C           EXCHANGE                                                    
C                                                                       
 80         P(1, I) = P(1, IH)                                          
            P(1, IH) = PI                                               
C                                                                       
C           PERCOLATE EXCHANGED LIST ELEMENT UP TO PROPER PLACE         
C                                                                       
            I = I - H                                                   
            IF ( I .GE. 1 ) GOTO 30                                     
 100      CONTINUE                                                      
C                                                                       
        H = ( H - 1 ) / 3                                               
        GOTO 20                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTPDI ( A, SA, P, SP, N )                             
C                                                                       
C     SRTPDI SETS P(1) = 1, P(SP+1) = 2, ..., P((N-1)*SP+1) = N         
C     AND THEN REARRANGES P(1), P(SP+1), ..., P((N-1)*SP+1) SO THAT     
C     A( (P(I)-1)*SA+1 ) .GE. A( (P(J)-1)*SA+1 ) IF AND ONLY IF         
C     I .GT. J, WHERE I AND J SUBSCRIPT PROPER ELEMENTS OF P            
C                                                                       
      INTEGER SP, P(SP, 1), SA, H, PIH, PI                              
      INTEGER A(SA, 1)                                                  
C                                                                       
C     CHECK INPUT PARAMETERS AND INITIALIZE H                           
C                                                                       
      CALL I1SRT( SA, SP, N )                                           
      IF ( I0SRT( 1, N, H ) .LT. 1 ) RETURN                             
C                                                                       
C     INITIALIZE P                                                      
C                                                                       
      DO 10 I = 1, N                                                    
 10     P(1, I) = I                                                     
C                                                                       
C       CHECK IF DONE WITH SORT                                         
C                                                                       
 20     IF ( H .LT. 1 ) RETURN                                          
        K = N - H                                                       
C                                                                       
C       COMPARE                                                         
C                                                                       
        DO 40 J = 1, K                                                  
          I = J                                                         
 30         IH = I + H                                                  
            PI = P(1, I)                                                
            PIH = P(1, IH)                                              
            IF ( A(1, PI) .GE. A(1, PIH) ) GOTO 40                      
C                                                                       
C           EXCHANGE                                                    
C                                                                       
            P(1, I) = PIH                                               
            P(1, IH) = PI                                               
C                                                                       
C           PERCOLATE EXCHANGED LIST ELEMENT UP TO PROPER PLACE         
C                                                                       
            I = I - H                                                   
            IF ( I .GE. 1 ) GOTO 30                                     
 40       CONTINUE                                                      
C                                                                       
        H = ( H - 1 ) / 3                                               
        GOTO 20                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTPDR ( A, SA, P, SP, N )                             
C                                                                       
C     SRTPDR SETS P(1) = 1, P(SP+1) = 2, ..., P((N-1)*SP+1) = N         
C     AND THEN REARRANGES P(1), P(SP+1), ..., P((N-1)*SP+1) SO THAT     
C     A( (P(I)-1)*SA+1 ) .GE. A( (P(J)-1)*SA+1 ) IF AND ONLY IF         
C     I .GT. J, WHERE I AND J SUBSCRIPT PROPER ELEMENTS OF P            
C                                                                       
      INTEGER SP, P(SP, 1), SA, H, PIH, PI                              
      REAL A(SA, 1)                                                     
C                                                                       
C     CHECK INPUT PARAMETERS AND INITIALIZE H                           
C                                                                       
      CALL I1SRT( SA, SP, N )                                           
      IF ( I0SRT( 1, N, H ) .LT. 1 ) RETURN                             
C                                                                       
C     INITIALIZE P                                                      
C                                                                       
      DO 10 I = 1, N                                                    
 10     P(1, I) = I                                                     
C                                                                       
C       CHECK IF DONE WITH SORT                                         
C                                                                       
 20     IF ( H .LT. 1 ) RETURN                                          
        K = N - H                                                       
C                                                                       
C       COMPARE                                                         
C                                                                       
        DO 40 J = 1, K                                                  
          I = J                                                         
 30         IH = I + H                                                  
            PI = P(1, I)                                                
            PIH = P(1, IH)                                              
            IF ( A(1, PI) .GE. A(1, PIH) ) GOTO 40                      
C                                                                       
C           EXCHANGE                                                    
C                                                                       
            P(1, I) = PIH                                               
            P(1, IH) = PI                                               
C                                                                       
C           PERCOLATE EXCHANGED LIST ELEMENT UP TO PROPER PLACE         
C                                                                       
            I = I - H                                                   
            IF ( I .GE. 1 ) GOTO 30                                     
 40       CONTINUE                                                      
C                                                                       
        H = ( H - 1 ) / 3                                               
        GOTO 20                                                         
C                                                                       
      END                                                               
      SUBROUTINE SRTRD ( A, SA, P, SP, N )                              
C                                                                       
C     SRTRD REARRANGES A(1), A(SA+1), ..., A((N-1)*SA+1)                
C     ACCORDING TO PERMUTATION OF INTEGERS 1 TO N STORED                
C     IN P(1), P(SP+1), ..., P((N-1)*SP+1)                              
C                                                                       
      INTEGER SA, SP, P(SP, 1)                                          
      DOUBLE PRECISION A(SA, 1), TEMP                                   
C                                                                       
C     CHECK INPUT PARAMETERS                                            
C                                                                       
      CALL I1SRT( SA, SP, N )                                           
C                                                                       
C     CHECK IF SORTING NECESSARY                                        
C                                                                       
      IF ( N .LE. 1 ) RETURN                                            
      I = 0                                                             
C                                                                       
C       CHECK IF HAVE COMPLETED PERMUTATION                             
C                                                                       
 10     I = I + 1                                                       
        IF ( I .GT. N ) GOTO 40                                         
        IF ( P(1, I) .LE. 0 ) GOTO 10                                   
C                                                                       
C       REARRANGE A ACCORDING TO CYCLE STARTING AT I                    
C                                                                       
        TEMP = A(1, I)                                                  
 20       L = I                                                         
          I = P(1, I)                                                   
          P(1, L) = -P(1, L)                                            
          IF ( P(1, I) .LE. 0 ) GOTO 30                                 
          A(1, L) = A(1, I)                                             
          GOTO 20                                                       
C                                                                       
C       FINISH CYCLE                                                    
C                                                                       
 30     A(1, L) = TEMP                                                  
        GOTO 10                                                         
C                                                                       
C     RESTORE P ARRAY                                                   
C                                                                       
 40   DO 50 I = 1, N                                                    
 50     P(1, I) = - P(1, I)                                             
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE SRTRH ( A, L, SA, P, SP, N )                           
C                                                                       
C     SRTRH REARRANGES THE N HOLLERITH DATA ITEMS, EACH                 
C     L WORDS IN LENGTH, AT A(1), A(SA+1), ..., A((N-1)*SA+1)           
C     ACCORDING TO PERMUTATION OF INTEGERS 1 TO N STORED                
C     IN P(1), P(SP+1), ..., P((N-1)*SP+1)                              
C                                                                       
      COMMON /CSTAK/ DSTAK(500)                                         
C                                                                       
      INTEGER SA, SP, P(SP, 1)                                          
      INTEGER A(SA, 1)                                                  
      INTEGER ISTAK(1000)                                               
      DOUBLE PRECISION DSTAK                                            
C                                                                       
      EQUIVALENCE (DSTAK(1), ISTAK(1))                                  
C                                                                       
C     CHECK INPUT PARAMETERS                                            
C                                                                       
      CALL I1SRT( SA, SP, N )                                           
C/6S                                                                    
C     IF ( L .GT. SA  .OR.  L .LT. 1 )                                  
C    1   CALL SETERR( 26HSRTRH - ILLEGAL VALUE OF L, 26, 4, 2 )         
C/7S                                                                    
      IF ( L .GT. SA  .OR.  L .LT. 1 )                                  
     1   CALL SETERR( 'SRTRH - ILLEGAL VALUE OF L', 26, 4, 2 )          
C/                                                                      
C                                                                       
C     CHECK IF SORTING NECESSARY                                        
C                                                                       
      IF ( N .LE. 1 ) RETURN                                            
C                                                                       
C     FETCH TEMPORARY STORAGE FROM STACK FOR FIRST ELEMENT              
C     IN EACH CYCLE                                                     
C                                                                       
      IBR = ISTKGT( L, 2 )                                              
      I = 0                                                             
C                                                                       
C       CHECK IF HAVE COMPLETED PERMUTATION                             
C                                                                       
 10     I = I + 1                                                       
        IF ( I .GT. N ) GOTO 40                                         
        IF ( P(1, I) .LE. 0 ) GOTO 10                                   
C                                                                       
C       REARRANGE A ACCORDING TO CYCLE STARTING AT I                    
C                                                                       
        CALL MOVEFI ( L, A(1, I), ISTAK(IBR) )                          
C                                                                       
 20       LL = I                                                        
          I = P(1, I)                                                   
          P(1, LL) = -P(1, LL)                                          
          IF ( P(1, I) .LE. 0 ) GOTO 30                                 
          CALL MOVEFI ( L, A(1, I), A(1, LL) )                          
          GOTO 20                                                       
C                                                                       
 30     CALL MOVEFI ( L, ISTAK(IBR), A(1, LL) )                         
C                                                                       
        GOTO 10                                                         
C                                                                       
C     RESTORE P ARRAY                                                   
C                                                                       
 40   DO 50 I = 1, N                                                    
 50     P(1, I) = - P(1, I)                                             
C                                                                       
      CALL ISTKRL(1)                                                    
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE SRTRI ( A, SA, P, SP, N )                              
C                                                                       
C     SRTRI REARRANGES A(1), A(SA+1), ..., A((N-1)*SA+1)                
C     ACCORDING TO PERMUTATION OF INTEGERS 1 TO N STORED                
C     IN P(1), P(SP+1), ..., P((N-1)*SP+1)                              
C                                                                       
      INTEGER SA, SP, P(SP, 1)                                          
      INTEGER A(SA, 1), TEMP                                            
C                                                                       
C     CHECK INPUT PARAMETERS                                            
C                                                                       
      CALL I1SRT( SA, SP, N )                                           
C                                                                       
C     CHECK IF SORTING NECESSARY                                        
C                                                                       
      IF ( N .LE. 1 ) RETURN                                            
      I = 0                                                             
C                                                                       
C       CHECK IF HAVE COMPLETED PERMUTATION                             
C                                                                       
 10     I = I + 1                                                       
        IF ( I .GT. N ) GOTO 40                                         
        IF ( P(1, I) .LE. 0 ) GOTO 10                                   
C                                                                       
C       REARRANGE A ACCORDING TO CYCLE STARTING AT I                    
C                                                                       
        TEMP = A(1, I)                                                  
 20       L = I                                                         
          I = P(1, I)                                                   
          P(1, L) = -P(1, L)                                            
          IF ( P(1, I) .LE. 0 ) GOTO 30                                 
          A(1, L) = A(1, I)                                             
          GOTO 20                                                       
C                                                                       
C       FINISH CYCLE                                                    
C                                                                       
 30     A(1, L) = TEMP                                                  
        GOTO 10                                                         
C                                                                       
C     RESTORE P ARRAY                                                   
C                                                                       
 40   DO 50 I = 1, N                                                    
 50     P(1, I) = - P(1, I)                                             
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE SRTRR ( A, SA, P, SP, N )                              
C                                                                       
C     SRTRR REARRANGES A(1), A(SA+1), ..., A((N-1)*SA+1)                
C     ACCORDING TO PERMUTATION OF INTEGERS 1 TO N STORED                
C     IN P(1), P(SP+1), ..., P((N-1)*SP+1)                              
C                                                                       
      INTEGER SA, SP, P(SP, 1)                                          
      REAL A(SA, 1), TEMP                                               
C                                                                       
C     CHECK INPUT PARAMETERS                                            
C                                                                       
      CALL I1SRT( SA, SP, N )                                           
C                                                                       
C     CHECK IF SORTING NECESSARY                                        
C                                                                       
      IF ( N .LE. 1 ) RETURN                                            
      I = 0                                                             
C                                                                       
C       CHECK IF HAVE COMPLETED PERMUTATION                             
C                                                                       
 10     I = I + 1                                                       
        IF ( I .GT. N ) GOTO 40                                         
        IF ( P(1, I) .LE. 0 ) GOTO 10                                   
C                                                                       
C       REARRANGE A ACCORDING TO CYCLE STARTING AT I                    
C                                                                       
        TEMP = A(1, I)                                                  
 20       L = I                                                         
          I = P(1, I)                                                   
          P(1, L) = -P(1, L)                                            
          IF ( P(1, I) .LE. 0 ) GOTO 30                                 
          A(1, L) = A(1, I)                                             
          GOTO 20                                                       
C                                                                       
C       FINISH CYCLE                                                    
C                                                                       
 30     A(1, L) = TEMP                                                  
        GOTO 10                                                         
C                                                                       
C     RESTORE P ARRAY                                                   
C                                                                       
 40   DO 50 I = 1, N                                                    
 50     P(1, I) = - P(1, I)                                             
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      INTEGER FUNCTION I0SRT ( SA, N, H )                               
C                                                                       
C     I0SRT CHECKS INPUT PARAMETERS N, SA AND CALCULATES H              
C     RETURNS H = 0 IF NO SORTING NECESSARY, ELSE                       
C     RETURNS SPACING, H, FOR FIRST INSERTION SORT.                     
C     I0SRT RETURNS TOTAL NUMBER OF ELEMENTS IN ARRAY = N * SA          
C                                                                       
      INTEGER SA, H                                                     
C                                                                       
C/6S                                                                    
C     IF ( N .LT. 0 )                                                   
C    1   CALL SETERR( 27HSRTXXX - ILLEGAL VALUE OF N, 27, 1, 2 )        
C     IF ( SA .LE. 0 )                                                  
C    1   CALL SETERR( 28HSRTXXX - ILLEGAL VALUE OF SA, 28, 2, 2 )       
C/7S                                                                    
      IF ( N .LT. 0 )                                                   
     1   CALL SETERR( 'SRTXXX - ILLEGAL VALUE OF N', 27, 1, 2 )         
      IF ( SA .LE. 0 )                                                  
     1   CALL SETERR( 'SRTXXX - ILLEGAL VALUE OF SA', 28, 2, 2 )        
C/                                                                      
C                                                                       
C     CHECK IF SORTING IS NECESSARY                                     
C                                                                       
      H = 0                                                             
      I0SRT = N * SA                                                    
      IF ( N .LE. 1 ) RETURN                                            
C                                                                       
C     CALCULATE H USING H NEW = 3 * H OLD + SA                          
C                                                                       
      H = 4 * SA                                                        
C                                                                       
 10     H = 3 * H + SA                                                  
        IF ( H .LT. I0SRT ) GOTO 10                                     
C                                                                       
      H = ( H - 4 * SA ) / 9                                            
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE I1SRT ( SA, SP, N )                                    
C                                                                       
C     I1SRT CHECKS LEGALITY OF VALUES OF SA, SP, N                      
C                                                                       
      INTEGER SA, SP                                                    
C                                                                       
C/6S                                                                    
C     IF ( N .LT. 0 )                                                   
C    1   CALL SETERR( 27HSRTXXX - ILLEGAL VALUE OF N, 27, 1, 2 )        
C     IF ( SA .LE. 0 )                                                  
C    1   CALL SETERR( 28HSRTXXX - ILLEGAL VALUE OF SA, 28, 2, 2 )       
C     IF ( SP .LE. 0 )                                                  
C    1   CALL SETERR( 28HSRTXXX - ILLEGAL VALUE OF SP, 28, 3, 2 )       
C/7S                                                                    
      IF ( N .LT. 0 )                                                   
     1   CALL SETERR( 'SRTXXX - ILLEGAL VALUE OF N', 27, 1, 2 )         
      IF ( SA .LE. 0 )                                                  
     1   CALL SETERR( 'SRTXXX - ILLEGAL VALUE OF SA', 28, 2, 2 )        
      IF ( SP .LE. 0 )                                                  
     1   CALL SETERR( 'SRTXXX - ILLEGAL VALUE OF SP', 28, 3, 2 )        
C/                                                                      
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE EXTRMI(NPTS, FN, NEX, IEXT, IMAX, IMIN, IMAG)          
      INTEGER NPTS                                                      
      INTEGER FN(NPTS), NEX, IEXT(NPTS), IMAX, IMIN, IMAG               
      INTEGER EXT, SEXT, EMAX, EMIN, IABS, I                            
C   THIS SUBROUTINE FINDS THE EXTREME POINTS OF THE DISCRETE            
C   FUNCTION FN(I).                                                     
C   INPUT-                                                              
C   NPTS   - THE NUMBER OF POINTS                                       
C   FN     - THE DISCRETE FUNCTION.                                     
C   OUTPUT-                                                             
C   NEX    - THE NUMBER OF EXTREMA.                                     
C   IEXT   - THE INDICES OF THE EXTREMAL POINTS IN THE DISCRETE         
C            FUNCTION FN(I).                                            
C   IMAX   - THE INDEX OF THE MAXIMUM FUNCTION VALUE.                   
C   IMIN   - THE INDEX OF THE MINIMUM FUNCTION VALUE.                   
C   IMAG   - THE INDEX OF THE FUNCTION VALUE WITH                       
C            MAXIMUM MAGNITUDE.                                         
C   ERROR STATES                                                        
C   1      - INVALID DIMENSION.                                         
C/6S                                                                    
C     IF (NPTS .LT. 1) CALL SETERR(26HEXTREM - INVALID DIMENSION, 26, 1,
C    1   2)                                                             
C/7S                                                                    
      IF (NPTS .LT. 1) CALL SETERR('EXTREM - INVALID DIMENSION', 26, 1, 
     1   2)                                                             
C/                                                                      
      NEX = 1                                                           
      IEXT(1) = 1                                                       
      EXT = FN(1)                                                       
      IMAX = 1                                                          
      EMAX = EXT                                                        
      IMIN = 1                                                          
      EMIN = EXT                                                        
      IF (EXT .EQ. 0) GOTO 1                                            
         SEXT = EXT/IABS(EXT)                                           
         GOTO  2                                                        
   1     SEXT = 0                                                       
   2  I = 2                                                             
         GOTO  4                                                        
   3     I = I+1                                                        
   4     IF (I .GT. NPTS)GOTO  14                                       
         IF (FN(I)*SEXT .GE. 0) GOTO 5                                  
            NEX = NEX+1                                                 
            GOTO  9                                                     
   5        IF (IABS(FN(I)) .GT. IABS(EXT)) GOTO 8                      
               IF (NEX .LE. 1) GOTO 6                                   
                  GOTO  3                                               
   6              GOTO  10                                              
   8     CONTINUE                                                       
   9     IEXT(NEX) = I                                                  
         EXT = FN(I)                                                    
         SEXT = EXT/IABS(EXT)                                           
  10     IF (EMAX .GE. FN(I)) GOTO 11                                   
            IMAX = I                                                    
            EMAX = FN(I)                                                
            GOTO  13                                                    
  11        IF (FN(I) .GE. EMIN) GOTO 12                                
               IMIN = I                                                 
               EMIN = FN(I)                                             
  12     CONTINUE                                                       
  13     CONTINUE                                                       
         GOTO  3                                                        
  14  IF (EMAX .GE. IABS(EMIN)) GOTO 15                                 
         IMAG = IMIN                                                    
         GOTO  16                                                       
  15     IMAG = IMAX                                                    
  16  RETURN                                                            
      END                                                               
      SUBROUTINE EXTRMR(NPTS, FN, NEX, IEXT, IMAX, IMIN, IMAG)          
      INTEGER NPTS                                                      
      INTEGER NEX, IEXT(NPTS), IMAX, IMIN, IMAG                         
      REAL FN(NPTS)                                                     
      INTEGER I                                                         
      REAL EXT, SEXT, EMAX, EMIN, ABS                                   
C   THIS SUBROUTINE FINDS THE EXTREME POINTS OF THE DISCRETE            
C   FUNCTION FN(I).                                                     
C   INPUT-                                                              
C   NPTS   - THE NUMBER OF POINTS                                       
C   FN     - THE DISCRETE FUNCTION.                                     
C   OUTPUT-                                                             
C   NEX    - THE NUMBER OF EXTREMA.                                     
C   IEXT   - THE INDICES OF THE EXTREMAL POINTS IN THE DISCRETE         
C            FUNCTION FN(I).                                            
C   IMAX   - THE INDEX OF THE MAXIMUM FUNCTION VALUE.                   
C   IMIN   - THE INDEX OF THE MINIMUM FUNCTION VALUE.                   
C   IMAG   - THE INDEX OF THE FUNCTION VALUE WITH                       
C            MAXIMUM MAGNITUDE.                                         
C   ERROR STATES                                                        
C   1      - INVALID DIMENSION.                                         
C/6S                                                                    
C     IF (NPTS .LT. 1) CALL SETERR(26HEXTREM - INVALID DIMENSION, 26, 1,
C    1   2)                                                             
C/7S                                                                    
      IF (NPTS .LT. 1) CALL SETERR('EXTREM - INVALID DIMENSION', 26, 1, 
     1   2)                                                             
C/                                                                      
      NEX = 1                                                           
      IEXT(1) = 1                                                       
      EXT = FN(1)                                                       
      IMAX = 1                                                          
      EMAX = EXT                                                        
      IMIN = 1                                                          
      EMIN = EXT                                                        
      IF (EXT .EQ. 0.0) GOTO 1                                          
         SEXT = EXT/ABS(EXT)                                            
         GOTO  2                                                        
   1     SEXT = 0.0                                                     
   2  I = 2                                                             
         GOTO  4                                                        
   3     I = I+1                                                        
   4     IF (I .GT. NPTS)GOTO  14                                       
         IF (FN(I)*SEXT .GE. 0.0) GOTO 5                                
            NEX = NEX+1                                                 
            GOTO  9                                                     
   5        IF (ABS(FN(I)) .GT. ABS(EXT)) GOTO 8                        
               IF (NEX .LE. 1) GOTO 6                                   
                  GOTO  3                                               
   6              GOTO  10                                              
   8     CONTINUE                                                       
   9     IEXT(NEX) = I                                                  
         EXT = FN(I)                                                    
         SEXT = EXT/ABS(EXT)                                            
  10     IF (EMAX .GE. FN(I)) GOTO 11                                   
            IMAX = I                                                    
            EMAX = FN(I)                                                
            GOTO  13                                                    
  11        IF (FN(I) .GE. EMIN) GOTO 12                                
               IMIN = I                                                 
               EMIN = FN(I)                                             
  12     CONTINUE                                                       
  13     CONTINUE                                                       
         GOTO  3                                                        
  14  IF (EMAX .GE. ABS(EMIN)) GOTO 15                                  
         IMAG = IMIN                                                    
         GOTO  16                                                       
  15     IMAG = IMAX                                                    
  16  RETURN                                                            
      END                                                               
      SUBROUTINE EXTRMD(NPTS, FN, NEX, IEXT, IMAX, IMIN, IMAG)          
      INTEGER NPTS                                                      
      INTEGER NEX, IEXT(NPTS), IMAX, IMIN, IMAG                         
      DOUBLE PRECISION FN(NPTS)                                         
      INTEGER I                                                         
      DOUBLE PRECISION EXT, SEXT, EMAX, EMIN                            
C   THIS SUBROUTINE FINDS THE EXTREME POINTS OF THE DISCRETE            
C   FUNCTION FN(I).                                                     
C   INPUT-                                                              
C   NPTS   - THE NUMBER OF POINTS                                       
C   FN     - THE DISCRETE FUNCTION.                                     
C   OUTPUT-                                                             
C   NEX    - THE NUMBER OF EXTREMA.                                     
C   IEXT   - THE INDICES OF THE EXTREMAL POINTS IN THE DISCRETE         
C            FUNCTION FN(I).                                            
C   IMAX   - THE INDEX OF THE MAXIMUM FUNCTION VALUE.                   
C   IMIN   - THE INDEX OF THE MINIMUM FUNCTION VALUE.                   
C   IMAG   - THE INDEX OF THE FUNCTION VALUE WITH                       
C            MAXIMUM MAGNITUDE.                                         
C   ERROR STATES                                                        
C   1      - INVALID DIMENSION.                                         
C/6S                                                                    
C     IF (NPTS .LT. 1) CALL SETERR(26HEXTREM - INVALID DIMENSION, 26, 1,
C    1   2)                                                             
C/7S                                                                    
      IF (NPTS .LT. 1) CALL SETERR('EXTREM - INVALID DIMENSION', 26, 1, 
     1   2)                                                             
C/                                                                      
      NEX = 1                                                           
      IEXT(1) = 1                                                       
      EXT = FN(1)                                                       
      IMAX = 1                                                          
      EMAX = EXT                                                        
      IMIN = 1                                                          
      EMIN = EXT                                                        
      IF (EXT .EQ. 0.0D0) GOTO 1                                        
         SEXT = EXT/DABS(EXT)                                           
         GOTO  2                                                        
   1     SEXT = 0.0D0                                                   
   2  I = 2                                                             
         GOTO  4                                                        
   3     I = I+1                                                        
   4     IF (I .GT. NPTS)GOTO  14                                       
         IF (FN(I)*SEXT .GE. 0.0D0) GOTO 5                              
            NEX = NEX+1                                                 
            GOTO  9                                                     
   5        IF (DABS(FN(I)) .GT. DABS(EXT)) GOTO 8                      
               IF (NEX .LE. 1) GOTO 6                                   
                  GOTO  3                                               
   6              GOTO  10                                              
   8     CONTINUE                                                       
   9     IEXT(NEX) = I                                                  
         EXT = FN(I)                                                    
         SEXT = EXT/DABS(EXT)                                           
  10     IF (EMAX .GE. FN(I)) GOTO 11                                   
            IMAX = I                                                    
            EMAX = FN(I)                                                
            GOTO  13                                                    
  11        IF (FN(I) .GE. EMIN) GOTO 12                                
               IMIN = I                                                 
               EMIN = FN(I)                                             
  12     CONTINUE                                                       
  13     CONTINUE                                                       
         GOTO  3                                                        
  14  IF (EMAX .GE. DABS(EMIN)) GOTO 15                                 
         IMAG = IMIN                                                    
         GOTO  16                                                       
  15     IMAG = IMAX                                                    
  16  RETURN                                                            
      END                                                               
      INTEGER FUNCTION IXKTH (N, K, X)                                  
C                                                                       
C  IXKTH FINDS THE KTH SMALLEST ELEMENT IN A SET S OF N NUMBERS.        
C                                                                       
C ERROR SITUATIONS                                                      
C        ERROR 1 - N .LE. 0     (FATAL)                                 
C        ERROR 2 - K .GT. N     (FATAL)                                 
C        ERROR 3 - K .LE. 0     (FATAL)                                 
C                                                                       
C STACK STORAGE                                                         
C        N LOCATIONS                                                    
C                                                                       
C  PHYLLIS FOX           AUGUST 10, 1982                                
C                                                                       
C  THE ALGORITHM USES THE FIRST STAGE OF QUICKSORT - SEE, FOR EXAMPLE   
C  KNUTH VOL. 3, PAGES 114-116.                                         
C                                                                       
C  AFTER THE FIRST CYCLE OF THE ALGORITHM, THE (ORIGINAL) FIRST ELEMENT 
C  IS POSITIONED AT ITS FINAL RESTING PLACE, WITH ALL SMALLER ITEMS     
C  BEFORE IT AND ALL LARGER AFTERWARDS.                                 
C  IF THIS RESTING PLACE IS THE KTH, THEN ONE IS DONE.                  
C  OTHERWISE THE KTH IS ONE OF THE OTHER SEGMENTS AND THE PROCESS       
C  ITERATES.                                                            
C                                                                       
C  THE STACK IS USED IN SUCH A WAY THAT THE SEGMENT BEING WORKED AT     
C  IS ALWAYS IN THE BEGINNING OF THE CURRENT STACK ALLOCATION.          
C                                                                       
C  UNDER THE CONSTRAINT OF USING FORTRAN 66, THIS METHOD TURNED OUT     
C  TO BE FASTER THAN DIVIDE-AND-CONQUER, AND (OF COURSE) FASTER         
C  THAN SORTING.                                                        
C                                                                       
      COMMON /CSTAK/DSTAK(500)                                          
      INTEGER IPNT, IBASE, IFIRST, I, J, JJ, K, KK, N                   
      INTEGER ISTKGT                                                    
      INTEGER ISTAK(1000), X(1), TEMP                                   
      DOUBLE PRECISION DSTAK                                            
C                                                                       
      EQUIVALENCE (DSTAK(1),ISTAK(1))                                   
C                                                                       
C/6S                                                                    
C     IF (N .LE. 0) CALL SETERR(                                        
C    1    18H  IXKTH - N .LE. 0, 18, 1, 2)                              
C/7S                                                                    
      IF (N .LE. 0) CALL SETERR(                                        
     1    '  IXKTH - N .LE. 0', 18, 1, 2)                               
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (K .GT. N) CALL SETERR(                                        
C    1    18H  IXKTH - K .GT. N, 18, 2, 2)                              
C/7S                                                                    
      IF (K .GT. N) CALL SETERR(                                        
     1    '  IXKTH - K .GT. N', 18, 2, 2)                               
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (K .LE. 0) CALL SETERR(                                        
C    1    18H  IXKTH - K .LE. 0, 18, 3, 2)                              
C/7S                                                                    
      IF (K .LE. 0) CALL SETERR(                                        
     1    '  IXKTH - K .LE. 0', 18, 3, 2)                               
C/                                                                      
C                                                                       
C  IF THERE IS ONLY ONE ELEMENT IN S, IT IS THE ANSWER.                 
C                                                                       
      IF (N .NE. 1) GO TO 1                                             
      IXKTH = X(1)                                                      
      RETURN                                                            
C                                                                       
C  NOW SET UP A LOCATION OF LENGTH N IN THE STACK -                     
C                                                                       
  1   IPNT = ISTKGT(N, 2)                                               
      IBASE = IPNT - 1                                                  
C                                                                       
C  MOVE THE ARRAY X TO THE STACK.                                       
C                                                                       
      CALL MOVEFI(N, X(1), ISTAK(IPNT))                                 
C                                                                       
C  SET KK TO K AND JJ TO N.                                             
C                                                                       
      KK = K + IBASE                                                    
      JJ = N + IBASE                                                    
C                                                                       
C  SET UP THE SEARCH IN THE J-DECREASING DIRECTION.                     
C                                                                       
 10   J = JJ                                                            
      I = 1 + IBASE                                                     
C                                                                       
 20   IF (ISTAK(I) .GT. ISTAK(J)) GO TO 40                              
C                                                                       
C  OTHERWISE KEEP DECREASING J.                                         
C                                                                       
 30   J = J - 1                                                         
      IF (I .LE. J) GO TO 20                                            
C                                                                       
      GO TO 60                                                          
C                                                                       
C  COMES HERE WHEN, IN J-DECREASING SEARCH, FINDS INTERCHANGE.          
C                                                                       
 40   TEMP = ISTAK(J)                                                   
      ISTAK(J) = ISTAK(I)                                               
      ISTAK(I) = TEMP                                                   
C                                                                       
C  NOW DO I-INCREASING STAGE                                            
C                                                                       
 50   I = I + 1                                                         
      IF (I .GE. J) GO TO 60                                            
      IF (ISTAK(I) .LE. ISTAK(J)) GO TO 50                              
C                                                                       
C  OTHERWISE, INTERCHANGE, AND GO BECK TO INCREASING J.                 
C                                                                       
      TEMP = ISTAK(J)                                                   
      ISTAK(J) = ISTAK(I)                                               
      ISTAK(I) = TEMP                                                   
C                                                                       
      GO TO 30                                                          
C  COMES HERE WHEN I=J.                                                 
C                                                                       
C  SEE IF THE KTH SMALLEST IS FOUND, OR IS IN THE FIRST OR              
C  SECOND SEGMENT.                                                      
C                                                                       
 60   IF (I .EQ. KK) GO TO 80                                           
      IF (I .GT. KK) GO TO 70                                           
C                                                                       
C  HERE I IS LESS THAN KK SO KTH IS IN SECOND SEGMENT.                  
C                                                                       
C  MOVE IT INTO THE STACK AND SET NEW LENGTH.                           
C                                                                       
      IFIRST = I + 1                                                    
      CALL MOVEFI (JJ-I, ISTAK(IFIRST), ISTAK(IPNT))                    
      JJ = JJ - I + IBASE                                               
C                                                                       
C  REDUCE THE KTH COUNTER BY THE NUMBER OF ITEMS IN THE                 
C  FIRST SEGMENT.                                                       
C                                                                       
      KK = KK - I + IBASE                                               
      GO TO 10                                                          
C                                                                       
C  HERE KTH IS IN FIRST SEGMENT.                                        
C                                                                       
 70   JJ = I - 1                                                        
      GO TO 10                                                          
C                                                                       
C  HERE KTH IS FOUND                                                    
C                                                                       
 80   IXKTH = ISTAK(I)                                                  
      CALL ISTKRL(1)                                                    
      RETURN                                                            
      END                                                               
      REAL FUNCTION XKTH (N, K, X)                                      
C                                                                       
C  XKTH FINDS THE KTH SMALLEST ELEMENT IN A SET S OF N NUMBERS.         
C                                                                       
C ERROR SITUATIONS                                                      
C        ERROR 1 - N .LE. 0     (FATAL)                                 
C        ERROR 2 - K .GT. N     (FATAL)                                 
C        ERROR 3 - K .LE. 0     (FATAL)                                 
C                                                                       
C STACK STORAGE                                                         
C        N LOCATIONS                                                    
C                                                                       
C  PHYLLIS FOX           AUGUST 10, 1982                                
C                                                                       
C  THE ALGORITHM USES THE FIRST STAGE OF QUICKSORT - SEE, FOR EXAMPLE   
C  KNUTH VOL. 3, PAGES 114-116.                                         
C                                                                       
C  AFTER THE FIRST CYCLE OF THE ALGORITHM, THE (ORIGINAL) FIRST ELEMENT 
C  IS POSITIONED AT ITS FINAL RESTING PLACE, WITH ALL SMALLER ITEMS     
C  BEFORE IT AND ALL LARGER AFTERWARDS.                                 
C  IF THIS RESTING PLACE IS THE KTH, THEN ONE IS DONE.                  
C  OTHERWISE THE KTH IS ONE OF THE OTHER SEGMENTS AND THE PROCESS       
C  ITERATES.                                                            
C                                                                       
C  THE STACK IS USED IN SUCH A WAY THAT THE SEGMENT BEING WORKED AT     
C  IS ALWAYS IN THE BEGINNING OF THE CURRENT STACK ALLOCATION.          
C                                                                       
C  UNDER THE CONSTRAINT OF USING FORTRAN 66, THIS METHOD TURNED OUT     
C  TO BE FASTER THAN DIVIDE-AND-CONQUER, AND (OF COURSE) FASTER         
C  THAN SORTING.                                                        
C                                                                       
      COMMON /CSTAK/DSTAK(500)                                          
      INTEGER IPNT, IBASE, IFIRST, I, J, JJ, K, KK, N                   
      INTEGER ISTKGT                                                    
      REAL RSTAK(1000), X(1), TEMP                                      
      DOUBLE PRECISION DSTAK                                            
C                                                                       
      EQUIVALENCE (DSTAK(1),RSTAK(1))                                   
C                                                                       
C/6S                                                                    
C     IF (N .LE. 0) CALL SETERR(                                        
C    1    17H  XKTH - N .LE. 0, 17, 1, 2)                               
C/7S                                                                    
      IF (N .LE. 0) CALL SETERR(                                        
     1    '  XKTH - N .LE. 0', 17, 1, 2)                                
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (K .GT. N) CALL SETERR(                                        
C    1    17H  XKTH - K .GT. N, 17, 2, 2)                               
C/7S                                                                    
      IF (K .GT. N) CALL SETERR(                                        
     1    '  XKTH - K .GT. N', 17, 2, 2)                                
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (K .LE. 0) CALL SETERR(                                        
C    1    17H  XKTH - K .LE. 0, 17, 3, 2)                               
C/7S                                                                    
      IF (K .LE. 0) CALL SETERR(                                        
     1    '  XKTH - K .LE. 0', 17, 3, 2)                                
C/                                                                      
C                                                                       
C  IF THERE IS ONLY ONE ELEMENT IN S, IT IS THE ANSWER.                 
C                                                                       
      IF (N .NE. 1) GO TO 1                                             
      XKTH = X(1)                                                       
      RETURN                                                            
C                                                                       
C  NOW SET UP A LOCATION OF LENGTH N IN THE STACK -                     
C                                                                       
  1   IPNT = ISTKGT(N, 3)                                               
      IBASE = IPNT - 1                                                  
C                                                                       
C  MOVE THE ARRAY X TO THE STACK.                                       
C                                                                       
      CALL MOVEFR(N, X(1), RSTAK(IPNT))                                 
C                                                                       
C  SET KK TO K AND JJ TO N.                                             
C                                                                       
      KK = K + IBASE                                                    
      JJ = N + IBASE                                                    
C                                                                       
C  SET UP THE SEARCH IN THE J-DECREASING DIRECTION.                     
C                                                                       
 10   J = JJ                                                            
      I = 1 + IBASE                                                     
C                                                                       
 20   IF (RSTAK(I) .GT. RSTAK(J)) GO TO 40                              
C                                                                       
C  OTHERWISE KEEP DECREASING J.                                         
C                                                                       
 30   J = J - 1                                                         
      IF (I .LE. J) GO TO 20                                            
C                                                                       
      GO TO 60                                                          
C                                                                       
C  COMES HERE WHEN, IN J-DECREASING SEARCH, FINDS INTERCHANGE.          
C                                                                       
 40   TEMP = RSTAK(J)                                                   
      RSTAK(J) = RSTAK(I)                                               
      RSTAK(I) = TEMP                                                   
C                                                                       
C  NOW DO I-INCREASING STAGE                                            
C                                                                       
 50   I = I + 1                                                         
      IF (I .GE. J) GO TO 60                                            
      IF (RSTAK(I) .LE. RSTAK(J)) GO TO 50                              
C                                                                       
C  OTHERWISE, INTERCHANGE, AND GO BECK TO INCREASING J.                 
C                                                                       
      TEMP = RSTAK(J)                                                   
      RSTAK(J) = RSTAK(I)                                               
      RSTAK(I) = TEMP                                                   
C                                                                       
      GO TO 30                                                          
C  COMES HERE WHEN I=J.                                                 
C                                                                       
C  SEE IF THE KTH SMALLEST IS FOUND, OR IS IN THE FIRST OR              
C  SECOND SEGMENT.                                                      
C                                                                       
 60   IF (I .EQ. KK) GO TO 80                                           
      IF (I .GT. KK) GO TO 70                                           
C                                                                       
C  HERE I IS LESS THAN KK SO KTH IS IN SECOND SEGMENT.                  
C                                                                       
C  MOVE IT INTO THE STACK AND SET NEW LENGTH.                           
C                                                                       
      IFIRST = I + 1                                                    
      CALL MOVEFR (JJ-I, RSTAK(IFIRST), RSTAK(IPNT))                    
      JJ = JJ - I + IBASE                                               
C                                                                       
C  REDUCE THE KTH COUNTER BY THE NUMBER OF ITEMS IN THE                 
C  FIRST SEGMENT.                                                       
C                                                                       
      KK = KK - I + IBASE                                               
      GO TO 10                                                          
C                                                                       
C  HERE KTH IS IN FIRST SEGMENT.                                        
C                                                                       
 70   JJ = I - 1                                                        
      GO TO 10                                                          
C                                                                       
C  HERE KTH IS FOUND                                                    
C                                                                       
 80   XKTH = RSTAK(I)                                                   
      CALL ISTKRL(1)                                                    
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DXKTH (N, K, X)                         
C                                                                       
C  DXKTH FINDS THE KTH SMALLEST ELEMENT IN A SET S OF N NUMBERS.        
C                                                                       
C ERROR SITUATIONS                                                      
C        ERROR 1 - N .LE. 0     (FATAL)                                 
C        ERROR 2 - K .GT. N     (FATAL)                                 
C        ERROR 3 - K .LE. 0     (FATAL)                                 
C                                                                       
C STACK STORAGE                                                         
C        N LOCATIONS                                                    
C                                                                       
C  PHYLLIS FOX           AUGUST 10, 1982                                
C                                                                       
C  THE ALGORITHM USES THE FIRST STAGE OF QUICKSORT - SEE, FOR EXAMPLE   
C  KNUTH VOL. 3, PAGES 114-116.                                         
C                                                                       
C  AFTER THE FIRST CYCLE OF THE ALGORITHM, THE (ORIGINAL) FIRST ELEMENT 
C  IS POSITIONED AT ITS FINAL RESTING PLACE, WITH ALL SMALLER ITEMS     
C  BEFORE IT AND ALL LARGER AFTERWARDS.                                 
C  IF THIS RESTING PLACE IS THE KTH, THEN ONE IS DONE.                  
C  OTHERWISE THE KTH IS ONE OF THE OTHER SEGMENTS AND THE PROCESS       
C  ITERATES.                                                            
C                                                                       
C  THE STACK IS USED IN SUCH A WAY THAT THE SEGMENT BEING WORKED AT     
C  IS ALWAYS IN THE BEGINNING OF THE CURRENT STACK ALLOCATION.          
C                                                                       
C  UNDER THE CONSTRAINT OF USING FORTRAN 66, THIS METHOD TURNED OUT     
C  TO BE FASTER THAN DIVIDE-AND-CONQUER, AND (OF COURSE) FASTER         
C  THAN SORTING.                                                        
C                                                                       
      COMMON /CSTAK/DSTAK(500)                                          
      INTEGER IPNT, IBASE, IFIRST, I, J, JJ, K, KK, N                   
      INTEGER ISTKGT                                                    
      DOUBLE PRECISION DSTAK, X(1), TEMP                                
C                                                                       
C                                                                       
C/6S                                                                    
C     IF (N .LE. 0) CALL SETERR(                                        
C    1    18H  DXKTH - N .LE. 0, 18, 1, 2)                              
C/7S                                                                    
      IF (N .LE. 0) CALL SETERR(                                        
     1    '  DXKTH - N .LE. 0', 18, 1, 2)                               
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (K .GT. N) CALL SETERR(                                        
C    1    18H  DXKTH - K .GT. N, 18, 2, 2)                              
C/7S                                                                    
      IF (K .GT. N) CALL SETERR(                                        
     1    '  DXKTH - K .GT. N', 18, 2, 2)                               
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (K .LE. 0) CALL SETERR(                                        
C    1    18H  DXKTH - K .LE. 0, 18, 3, 2)                              
C/7S                                                                    
      IF (K .LE. 0) CALL SETERR(                                        
     1    '  DXKTH - K .LE. 0', 18, 3, 2)                               
C/                                                                      
C                                                                       
C  IF THERE IS ONLY ONE ELEMENT IN S, IT IS THE ANSWER.                 
C                                                                       
      IF (N .NE. 1) GO TO 1                                             
      DXKTH = X(1)                                                      
      RETURN                                                            
C                                                                       
C  NOW SET UP A LOCATION OF LENGTH N IN THE STACK -                     
C                                                                       
  1   IPNT = ISTKGT(N, 4)                                               
      IBASE = IPNT - 1                                                  
C                                                                       
C  MOVE THE ARRAY X TO THE STACK.                                       
C                                                                       
      CALL MOVEFD(N, X(1), DSTAK(IPNT))                                 
C                                                                       
C  SET KK TO K AND JJ TO N.                                             
C                                                                       
      KK = K + IBASE                                                    
      JJ = N + IBASE                                                    
C                                                                       
C  SET UP THE SEARCH IN THE J-DECREASING DIRECTION.                     
C                                                                       
 10   J = JJ                                                            
      I = 1 + IBASE                                                     
C                                                                       
 20   IF (DSTAK(I) .GT. DSTAK(J)) GO TO 40                              
C                                                                       
C  OTHERWISE KEEP DECREASING J.                                         
C                                                                       
 30   J = J - 1                                                         
      IF (I .LE. J) GO TO 20                                            
C                                                                       
      GO TO 60                                                          
C                                                                       
C  COMES HERE WHEN, IN J-DECREASING SEARCH, FINDS INTERCHANGE.          
C                                                                       
 40   TEMP = DSTAK(J)                                                   
      DSTAK(J) = DSTAK(I)                                               
      DSTAK(I) = TEMP                                                   
C                                                                       
C  NOW DO I-INCREASING STAGE                                            
C                                                                       
 50   I = I + 1                                                         
      IF (I .GE. J) GO TO 60                                            
      IF (DSTAK(I) .LE. DSTAK(J)) GO TO 50                              
C                                                                       
C  OTHERWISE, INTERCHANGE, AND GO BECK TO INCREASING J.                 
C                                                                       
      TEMP = DSTAK(J)                                                   
      DSTAK(J) = DSTAK(I)                                               
      DSTAK(I) = TEMP                                                   
C                                                                       
      GO TO 30                                                          
C  COMES HERE WHEN I=J.                                                 
C                                                                       
C  SEE IF THE KTH SMALLEST IS FOUND, OR IS IN THE FIRST OR              
C  SECOND SEGMENT.                                                      
C                                                                       
 60   IF (I .EQ. KK) GO TO 80                                           
      IF (I .GT. KK) GO TO 70                                           
C                                                                       
C  HERE I IS LESS THAN KK SO KTH IS IN SECOND SEGMENT.                  
C                                                                       
C  MOVE IT INTO THE STACK AND SET NEW LENGTH.                           
C                                                                       
      IFIRST = I + 1                                                    
      CALL MOVEFD (JJ-I, DSTAK(IFIRST), DSTAK(IPNT))                    
      JJ = JJ - I + IBASE                                               
C                                                                       
C  REDUCE THE KTH COUNTER BY THE NUMBER OF ITEMS IN THE                 
C  FIRST SEGMENT.                                                       
C                                                                       
      KK = KK - I + IBASE                                               
      GO TO 10                                                          
C                                                                       
C  HERE KTH IS IN FIRST SEGMENT.                                        
C                                                                       
 70   JJ = I - 1                                                        
      GO TO 10                                                          
C                                                                       
C  HERE KTH IS FOUND                                                    
C                                                                       
 80   DXKTH = DSTAK(I)                                                  
      CALL ISTKRL(1)                                                    
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DFLOAT(I)                               
      INTEGER I                                                         
C    THIS FUNCTION SUBPROGRAM CONVERTS INTEGER INTO DOUBLE PRECISION.   
      DFLOAT = I                                                        
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DDIM(X, Y)                              
      DOUBLE PRECISION X, Y                                             
C    THIS FUNCTION SUBPROGRAM COMPUTES THE POSITIVE DIFFERENCE          
C    OF TWO NUMBERS IN DOUBLE PRECISION.                                
      DDIM = X-DMIN1(X, Y)                                              
      RETURN                                                            
      END                                                               
      INTEGER FUNCTION IDFIX(X)                                         
      DOUBLE PRECISION X                                                
C    THIS FUNCTION SUBPROGRAM CONVERTS DOUBLE PRECISION INTO INTEGER.   
      IDFIX = X                                                         
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION SMONOR(X, N, INC)                                
      INTEGER INC                                                       
      INTEGER N                                                         
      REAL X(INC, 1)                                                    
      INTEGER I                                                         
C   SMONOR IS AN LOGICAL FUNCTION WHICH INSPECTS THE REAL               
C   VECTOR, X, AND RETURNS THE VALUE, TRUE, IF THE ARRAY IS STRICTLY    
C   MONOTONE AND THE VALUE, FALSE, OTHERWISE.  THE VALUE, FALSE, IS     
C   RETURNED ONLY IF THE ARRAY CONTAINS ELEMENTS WHICH CONTRADICT       
C   THE PROPERTY.  IN PARTICULAR, VECTORS OF LENGTH 0 OR 1 RETURN TRUE. 
C   X   - THE VECTOR OF POINTS TO BE CHECKED.                           
C   N   - THE DIMENSION OF THE VECTOR X.                                
C         N .LT. 0    IS A FATAL ERROR.                                 
C   INC - THE INCREMENT IN THE VECTOR.                                  
C         INC .LT. 1  IS A FATAL ERROR.                                 
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(26HSMONOR - INVALID DIMENSION, 26, 1, 2)
C     IF (INC .LT. 1) CALL SETERR(26HSMONOR - INVALID INCREMENT, 26, 2  
C    1   , 2)                                                           
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('SMONOR - INVALID DIMENSION', 26, 1, 2) 
      IF (INC .LT. 1) CALL SETERR('SMONOR - INVALID INCREMENT', 26, 2   
     1   , 2)                                                           
C/                                                                      
      SMONOR = .TRUE.                                                   
      IF (N .LT. 2) RETURN                                              
C STRICTLY MONOTONE BY DEFAULT.                                         
C CHECK FOR STRICTLY INCREASING.                                        
      DO  1 I = 2, N                                                    
         IF (X(1, I-1) .GE. X(1, I)) GOTO  2                            
   1     CONTINUE                                                       
      RETURN                                                            
C CHECK FOR STRICLY DECREASING.                                         
   2  DO  3 I = 2, N                                                    
         IF (X(1, I-1) .LE. X(1, I)) GOTO  4                            
   3     CONTINUE                                                       
      RETURN                                                            
   4  SMONOR = .FALSE.                                                  
C NOT STRICTLY MONOTONE.                                                
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION MONOR(X, N, INC)                                 
      INTEGER INC                                                       
      INTEGER N                                                         
      REAL X(INC, 1)                                                    
      INTEGER J, I                                                      
C   MONOR IS AN LOGICAL FUNCTION WHICH INSPECTS THE REAL                
C   VECTOR, X, AND RETURNS THE VALUE, TRUE, IF THE ARRAY IS             
C   MONOTONE AND THE VALUE, FALSE, OTHERWISE.  THE VALUE, FALSE, IS     
C   RETURNED ONLY IF THE ARRAY CONTAINS ELMENTS WHICH CONTRADICT        
C   THE PROPERTY.  IN PARTICULAR, VECTORS OF LENGTH 0, 1, OR 2          
C   RETURN TRUE.                                                        
C   X   - THE VECTOR OF POINTS TO BE CHECKED.                           
C   N   - THE DIMENSION OF THE VECTOR X.                                
C         N .LT. 0    IS A FATAL ERROR.                                 
C   INC - THE INCREMENT IN THE VECTOR.                                  
C         INC .LT. 1  IS A FATAL ERROR.                                 
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(25HMONOR - INVALID DIMENSION, 25, 1, 2) 
C     IF (INC .LT. 1) CALL SETERR(25HMONOR - INVALID INCREMENT, 25, 2, 2
C    1   )                                                              
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('MONOR - INVALID DIMENSION', 25, 1, 2)  
      IF (INC .LT. 1) CALL SETERR('MONOR - INVALID INCREMENT', 25, 2, 2 
     1   )                                                              
C/                                                                      
      MONOR = .TRUE.                                                    
      IF (N .LT. 3) RETURN                                              
C MONOTONE BY DEFAULT.                                                  
C PASS OVER INITIAL EQUALITIES.                                         
      DO  1 J = 2, N                                                    
         IF (X(1, J-1) .NE. X(1, J)) GOTO  2                            
   1     CONTINUE                                                       
      RETURN                                                            
C CHECK FOR MONOTONE INCREASING.                                        
   2  DO  3 I = J, N                                                    
         IF (X(1, I-1) .GT. X(1, I)) GOTO  4                            
   3     CONTINUE                                                       
      RETURN                                                            
C CHECK FOR MONOTONE DECREASING.                                        
   4  DO  5 I = J, N                                                    
         IF (X(1, I-1) .LT. X(1, I)) GOTO  6                            
   5     CONTINUE                                                       
      RETURN                                                            
   6  MONOR = .FALSE.                                                   
C NOT MONOTONE.                                                         
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION SMONOI(X, N, INC)                                
      INTEGER INC                                                       
      INTEGER X(INC, 1), N                                              
      INTEGER I                                                         
C   SMONOI IS AN LOGICAL FUNCTION WHICH INSPECTS THE INTEGER            
C   VECTOR, X, AND RETURNS THE VALUE, TRUE, IF THE ARRAY IS STRICTLY    
C   MONOTONE AND THE VALUE, FALSE, OTHERWISE.  THE VALUE, FALSE, IS     
C   RETURNED ONLY IF THE ARRAY CONTAINS ELEMENTS WHICH CONTRADICT       
C   THE PROPERTY.  IN PARTICULAR, VECTORS OF LENGTH 0 OR 1 RETURN TRUE. 
C   X   - THE VECTOR OF POINTS TO BE CHECKED.                           
C   N   - THE DIMENSION OF THE VECTOR X.                                
C         N .LT. 0    IS A FATAL ERROR.                                 
C   INC - THE INCREMENT IN THE VECTOR.                                  
C         INC .LT. 1  IS A FATAL ERROR.                                 
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(26HSMONOI - INVALID DIMENSION, 26, 1, 2)
C     IF (INC .LT. 1) CALL SETERR(26HSMONOI - INVALID INCREMENT, 26, 2  
C    1   , 2)                                                           
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('SMONOI - INVALID DIMENSION', 26, 1, 2) 
      IF (INC .LT. 1) CALL SETERR('SMONOI - INVALID INCREMENT', 26, 2   
     1   , 2)                                                           
C/                                                                      
      SMONOI = .TRUE.                                                   
      IF (N .LT. 2) RETURN                                              
C STRICTLY MONOTONE BY DEFAULT.                                         
C CHECK FOR STRICTLY INCREASING.                                        
      DO  1 I = 2, N                                                    
         IF (X(1, I-1) .GE. X(1, I)) GOTO  2                            
   1     CONTINUE                                                       
      RETURN                                                            
C CHECK FOR STRICLY DECREASING.                                         
   2  DO  3 I = 2, N                                                    
         IF (X(1, I-1) .LE. X(1, I)) GOTO  4                            
   3     CONTINUE                                                       
      RETURN                                                            
   4  SMONOI = .FALSE.                                                  
C NOT STRICTLY MONOTONE.                                                
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION MONOI(X, N, INC)                                 
      INTEGER INC                                                       
      INTEGER X(INC, 1), N                                              
      INTEGER J, I                                                      
C   MONOI IS AN LOGICAL FUNCTION WHICH INSPECTS THE INTEGER             
C   VECTOR, X, AND RETURNS THE VALUE, TRUE, IF THE ARRAY IS             
C   MONOTONE AND THE VALUE, FALSE, OTHERWISE.  THE VALUE, FALSE, IS     
C   RETURNED ONLY IF THE ARRAY CONTAINS ELMENTS WHICH CONTRADICT        
C   THE PROPERTY.  IN PARTICULAR, VECTORS OF LENGTH 0, 1, OR 2          
C   RETURN TRUE.                                                        
C   X   - THE VECTOR OF POINTS TO BE CHECKED.                           
C   N   - THE DIMENSION OF THE VECTOR X.                                
C         N .LT. 0    IS A FATAL ERROR.                                 
C   INC - THE INCREMENT IN THE VECTOR.                                  
C         INC .LT. 1  IS A FATAL ERROR.                                 
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(25HMONOI - INVALID DIMENSION, 25, 1, 2) 
C     IF (INC .LT. 1) CALL SETERR(25HMONOI - INVALID INCREMENT, 25, 2, 2
C    1   )                                                              
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('MONOI - INVALID DIMENSION', 25, 1, 2)  
      IF (INC .LT. 1) CALL SETERR('MONOI - INVALID INCREMENT', 25, 2, 2 
     1   )                                                              
C/                                                                      
      MONOI = .TRUE.                                                    
      IF (N .LT. 3) RETURN                                              
C MONOTONE BY DEFAULT.                                                  
C PASS OVER INITIAL EQUALITIES.                                         
      DO  1 J = 2, N                                                    
         IF (X(1, J-1) .NE. X(1, J)) GOTO  2                            
   1     CONTINUE                                                       
      RETURN                                                            
C CHECK FOR MONOTONE INCREASING.                                        
   2  DO  3 I = J, N                                                    
         IF (X(1, I-1) .GT. X(1, I)) GOTO  4                            
   3     CONTINUE                                                       
      RETURN                                                            
C CHECK FOR MONOTONE DECREASING.                                        
   4  DO  5 I = J, N                                                    
         IF (X(1, I-1) .LT. X(1, I)) GOTO  6                            
   5     CONTINUE                                                       
      RETURN                                                            
   6  MONOI = .FALSE.                                                   
C NOT MONOTONE.                                                         
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION SMONOD(X, N, INC)                                
      INTEGER INC                                                       
      INTEGER N                                                         
      DOUBLE PRECISION X(INC, 1)                                        
      INTEGER I                                                         
C   SMONOD IS AN LOGICAL FUNCTION WHICH INSPECTS THE DOUBLE PRECISION   
C   VECTOR, X, AND RETURNS THE VALUE, TRUE, IF THE ARRAY IS STRICTLY    
C   MONOTONE AND THE VALUE, FALSE, OTHERWISE.  THE VALUE, FALSE, IS     
C   RETURNED ONLY IF THE ARRAY CONTAINS ELEMENTS WHICH CONTRADICT       
C   THE PROPERTY.  IN PARTICULAR, VECTORS OF LENGTH 0 OR 1 RETURN TRUE. 
C   X   - THE VECTOR OF POINTS TO BE CHECKED.                           
C   N   - THE DIMENSION OF THE VECTOR X.                                
C         N .LT. 0    IS A FATAL ERROR.                                 
C   INC - THE INCREMENT IN THE VECTOR.                                  
C         INC .LT. 1  IS A FATAL ERROR.                                 
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(26HSMONOD - INVALID DIMENSION, 26, 1, 2)
C     IF (INC .LT. 1) CALL SETERR(26HSMONOD - INVALID INCREMENT, 26, 2  
C    1   , 2)                                                           
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('SMONOD - INVALID DIMENSION', 26, 1, 2) 
      IF (INC .LT. 1) CALL SETERR('SMONOD - INVALID INCREMENT', 26, 2   
     1   , 2)                                                           
C/                                                                      
      SMONOD = .TRUE.                                                   
      IF (N .LT. 2) RETURN                                              
C STRICTLY MONOTONE BY DEFAULT.                                         
C CHECK FOR STRICTLY INCREASING.                                        
      DO  1 I = 2, N                                                    
         IF (X(1, I-1) .GE. X(1, I)) GOTO  2                            
   1     CONTINUE                                                       
      RETURN                                                            
C CHECK FOR STRICLY DECREASING.                                         
   2  DO  3 I = 2, N                                                    
         IF (X(1, I-1) .LE. X(1, I)) GOTO  4                            
   3     CONTINUE                                                       
      RETURN                                                            
   4  SMONOD = .FALSE.                                                  
C NOT STRICTLY MONOTONE.                                                
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION MONOD(X, N, INC)                                 
      INTEGER INC                                                       
      INTEGER N                                                         
      DOUBLE PRECISION X(INC, 1)                                        
      INTEGER J, I                                                      
C   MONOD IS AN LOGICAL FUNCTION WHICH INSPECTS THE DOUBLE PRECISION    
C   VECTOR, X, AND RETURNS THE VALUE, TRUE, IF THE ARRAY IS             
C   MONOTONE AND THE VALUE, FALSE, OTHERWISE.  THE VALUE, FALSE, IS     
C   RETURNED ONLY IF THE ARRAY CONTAINS ELMENTS WHICH CONTRADICT        
C   THE PROPERTY.  IN PARTICULAR, VECTORS OF LENGTH 0, 1, OR 2          
C   RETURN TRUE.                                                        
C   X   - THE VECTOR OF POINTS TO BE CHECKED.                           
C   N   - THE DIMENSION OF THE VECTOR X.                                
C         N .LT. 0    IS A FATAL ERROR.                                 
C   INC - THE INCREMENT IN THE VECTOR.                                  
C         INC .LT. 1  IS A FATAL ERROR.                                 
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(25HMONOD - INVALID DIMENSION, 25, 1, 2) 
C     IF (INC .LT. 1) CALL SETERR(25HMONOD - INVALID INCREMENT, 25, 2, 2
C    1   )                                                              
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('MONOD - INVALID DIMENSION', 25, 1, 2)  
      IF (INC .LT. 1) CALL SETERR('MONOD - INVALID INCREMENT', 25, 2, 2 
     1   )                                                              
C/                                                                      
      MONOD = .TRUE.                                                    
      IF (N .LT. 3) RETURN                                              
C MONOTONE BY DEFAULT.                                                  
C PASS OVER INITIAL EQUALITIES.                                         
      DO  1 J = 2, N                                                    
         IF (X(1, J-1) .NE. X(1, J)) GOTO  2                            
   1     CONTINUE                                                       
      RETURN                                                            
C CHECK FOR MONOTONE INCREASING.                                        
   2  DO  3 I = J, N                                                    
         IF (X(1, I-1) .GT. X(1, I)) GOTO  4                            
   3     CONTINUE                                                       
      RETURN                                                            
C CHECK FOR MONOTONE DECREASING.                                        
   4  DO  5 I = J, N                                                    
         IF (X(1, I-1) .LT. X(1, I)) GOTO  6                            
   5     CONTINUE                                                       
      RETURN                                                            
   6  MONOD = .FALSE.                                                   
C NOT MONOTONE.                                                         
      RETURN                                                            
      END                                                               
      INTEGER FUNCTION INTRVR(N, R, RR)                                 
      INTEGER N                                                         
      REAL R(N), RR                                                     
      INTEGER LEFT, RIGHT, DELTA, NEW, MIN0, MAX0                       
      DATA LEFT/1/                                                      
C IN THE FOLLOWING *LB* IS USED FOR LEFT BRACKET, AND *RB* FOR RIGHT    
C THE ARRAY *LB* R(1), R(2), ..., R(N) *RB* DIVIDES THE                 
C INTERVAL *LB* R(1), R(2) *RB*                                         
C INTO NON-EMPTY SUB-INTERVALS.  THE R-VALUES NEED NOT BE DISTINCT,     
C BUT IN MOST APPLICATIONS R WILL BE MONOTONE INCREASING.               
C RETURN INTRVR=LEFT, WHERE                                             
C    R(LEFT) .LE. RR .LT. R(LEFT+1) .LE. R(N)                           
C OR                                                                    
C    R(LEFT) .LT.  RR = R(LEFT+1) = ... = R(N)                          
C EXCEPT                                                                
C    RETURN 0   IF RR.LT.R(1)                                           
C    RETURN N   IF RR>R(N)                                              
C/6S                                                                    
C     IF (N .LE. 1) CALL SETERR(17HINTRVR - N .LE. 1, 17, 1, 2)         
C     IF (R(1) .GE. R(N)) CALL SETERR(23HINTRVR - R(1) .GE. R(N), 23, 2,
C    1   2)                                                             
C/7S                                                                    
      IF (N .LE. 1) CALL SETERR('INTRVR - N .LE. 1', 17, 1, 2)          
      IF (R(1) .GE. R(N)) CALL SETERR('INTRVR - R(1) .GE. R(N)', 23, 2, 
     1   2)                                                             
C/                                                                      
      IF (RR .GE. R(1)) GOTO 1                                          
         INTRVR = 0                                                     
         RETURN                                                         
   1  IF (RR .LT. R(N)) GOTO 6                                          
         IF (RR .LE. R(N)) GOTO 2                                       
            INTRVR = N                                                  
            GOTO  5                                                     
   2        LEFT = N-1                                                  
   3        IF (R(LEFT) .LT. R(LEFT+1)) GOTO  4                         
               LEFT = LEFT-1                                            
               GOTO  3                                                  
   4        INTRVR = LEFT                                               
   5     RETURN                                                         
C FIND A STARTING INTERVAL OF WIDTH 1.                                  
C (IF LEFT IS LEFT-OVER FROM LAST TIME, THE STARTING INTERVAL           
C  IS LIKELY TO BE CLOSE.)                                              
   6  RIGHT = LEFT+1                                                    
      IF (RIGHT .LE. N) GOTO 7                                          
         LEFT = N-1                                                     
         RIGHT = N                                                      
   7  DELTA = 1                                                         
   8  IF (RR .LT. R(RIGHT)) GOTO  9                                     
C EXPLODE RIGHT IF NECESSARY.                                           
         LEFT = RIGHT                                                   
         RIGHT = MIN0(N, RIGHT+DELTA)                                   
         DELTA = 2*DELTA                                                
         GOTO  8                                                        
   9  IF (RR .GE. R(LEFT)) GOTO  10                                     
C EXPLODE LEFT IF NECESSARY.                                            
         RIGHT = LEFT                                                   
         LEFT = MAX0(1, LEFT-DELTA)                                     
         DELTA = 2*DELTA                                                
         GOTO  9                                                        
  10  IF (RIGHT-LEFT .LE. 1) GOTO  13                                   
C IF EXPLODED, BISECT.                                                  
         NEW = (LEFT+RIGHT)/2                                           
         IF (R(NEW) .GT. RR) GOTO 11                                    
            LEFT = NEW                                                  
            GOTO  12                                                    
  11        RIGHT = NEW                                                 
  12     CONTINUE                                                       
         GOTO  10                                                       
  13  INTRVR = LEFT                                                     
      RETURN                                                            
      END                                                               
      INTEGER FUNCTION INTRVI(N, I, II)                                 
      INTEGER N                                                         
      INTEGER I(N), II                                                  
      INTEGER LEFT, RIGHT, DELTA, NEW, MIN0, MAX0                       
      DATA LEFT/1/                                                      
C IN THE FOLLOWING *LB* IS USED FOR LEFT BRACKET, AND *RB* FOR RIGHT    
C THE ARRAY *LB* I(1), I(2), ..., I(N) *RB* DIVIDES THE                 
C INTERVAL *LB* I(1), I(2) *RB*                                         
C INTO NON-EMPTY SUB-INTERVALS.  THE I-VALUES NEED NOT BE DISTINCT,     
C BUT IN MOST APPLICATIONS I WILL BE MONOTONE INCREASING.               
C RETURN INTRVI=LEFT, WHERE                                             
C    I(LEFT) .LE. II .LT. I(LEFT+1) .LE. I(N)                           
C OR                                                                    
C    I(LEFT) .LT.  II = I(LEFT+1) = ... = I(N)                          
C EXCEPT                                                                
C    RETURN 0   IF II.LT.I(1)                                           
C    RETURN N   IF II>I(N)                                              
C/6S                                                                    
C     IF (N .LE. 1) CALL SETERR(17HINTRVI - N .LE. 1, 17, 1, 2)         
C     IF (I(1) .GE. I(N)) CALL SETERR(23HINTRVI - I(1) .GE. I(N), 23, 2,
C    1   2)                                                             
C/7S                                                                    
      IF (N .LE. 1) CALL SETERR('INTRVI - N .LE. 1', 17, 1, 2)          
      IF (I(1) .GE. I(N)) CALL SETERR('INTRVI - I(1) .GE. I(N)', 23, 2, 
     1   2)                                                             
C/                                                                      
      IF (II .GE. I(1)) GOTO 1                                          
         INTRVI = 0                                                     
         RETURN                                                         
   1  IF (II .LT. I(N)) GOTO 6                                          
         IF (II .LE. I(N)) GOTO 2                                       
            INTRVI = N                                                  
            GOTO  5                                                     
   2        LEFT = N-1                                                  
   3        IF (I(LEFT) .LT. I(LEFT+1)) GOTO  4                         
               LEFT = LEFT-1                                            
               GOTO  3                                                  
   4        INTRVI = LEFT                                               
   5     RETURN                                                         
C FIND A STARTING INTERVAL OF WIDTH 1.                                  
C (IF LEFT IS LEFT-OVER FROM LAST TIME, THE STARTING INTERVAL           
C  IS LIKELY TO BE CLOSE.)                                              
   6  RIGHT = LEFT+1                                                    
      IF (RIGHT .LE. N) GOTO 7                                          
         LEFT = N-1                                                     
         RIGHT = N                                                      
   7  DELTA = 1                                                         
   8  IF (II .LT. I(RIGHT)) GOTO  9                                     
C EXPLODE RIGHT IF NECESSARY.                                           
         LEFT = RIGHT                                                   
         RIGHT = MIN0(N, RIGHT+DELTA)                                   
         DELTA = 2*DELTA                                                
         GOTO  8                                                        
   9  IF (II .GE. I(LEFT)) GOTO  10                                     
C EXPLODE LEFT IF NECESSARY.                                            
         RIGHT = LEFT                                                   
         LEFT = MAX0(1, LEFT-DELTA)                                     
         DELTA = 2*DELTA                                                
         GOTO  9                                                        
  10  IF (RIGHT-LEFT .LE. 1) GOTO  13                                   
C IF EXPLODED, BISECT.                                                  
         NEW = (LEFT+RIGHT)/2                                           
         IF (I(NEW) .GT. II) GOTO 11                                    
            LEFT = NEW                                                  
            GOTO  12                                                    
  11        RIGHT = NEW                                                 
  12     CONTINUE                                                       
         GOTO  10                                                       
  13  INTRVI = LEFT                                                     
      RETURN                                                            
      END                                                               
      INTEGER FUNCTION INTRVD(N, D, DD)                                 
      INTEGER N                                                         
      DOUBLE PRECISION D(N), DD                                         
      INTEGER LEFT, RIGHT, DELTA, NEW, MIN0, MAX0                       
      DATA LEFT/1/                                                      
C IN THE FOLLOWING *LB* IS USED FOR LEFT BRACKET, AND *RB* FOR RIGHT    
C THE ARRAY *LB* D(1), D(2), ..., D(N) *RB* DIVIDES THE                 
C INTERVAL *LB* D(1), D(2) *RB*                                         
C INTO NON-EMPTY SUB-INTERVALS.  THE D-VALUES NEED NOT BE DISTINCT,     
C BUT IN MOST APPLICATIONS D WILL BE MONOTONE INCREASING.               
C RETURN INTRVD=LEFT, WHERE                                             
C    D(LEFT) .LE. DD .LT. D(LEFT+1) .LE. D(N)                           
C OR                                                                    
C    D(LEFT) .LT.  DD = D(LEFT+1) = ... = D(N)                          
C EXCEPT                                                                
C    RETURN 0   IF DD.LT.D(1)                                           
C    RETURN N   IF DD>D(N)                                              
C/6S                                                                    
C     IF (N .LE. 1) CALL SETERR(17HINTRVD - N .LE. 1, 17, 1, 2)         
C     IF (D(1) .GE. D(N)) CALL SETERR(23HINTRVD - D(1) .GE. D(N), 23, 2,
C    1   2)                                                             
C/7S                                                                    
      IF (N .LE. 1) CALL SETERR('INTRVD - N .LE. 1', 17, 1, 2)          
      IF (D(1) .GE. D(N)) CALL SETERR('INTRVD - D(1) .GE. D(N)', 23, 2, 
     1   2)                                                             
C/                                                                      
      IF (DD .GE. D(1)) GOTO 1                                          
         INTRVD = 0                                                     
         RETURN                                                         
   1  IF (DD .LT. D(N)) GOTO 6                                          
         IF (DD .LE. D(N)) GOTO 2                                       
            INTRVD = N                                                  
            GOTO  5                                                     
   2        LEFT = N-1                                                  
   3        IF (D(LEFT) .LT. D(LEFT+1)) GOTO  4                         
               LEFT = LEFT-1                                            
               GOTO  3                                                  
   4        INTRVD = LEFT                                               
   5     RETURN                                                         
C FIND A STARTING INTERVAL OF WIDTH 1.                                  
C (IF LEFT IS LEFT-OVER FROM LAST TIME, THE STARTING INTERVAL           
C  IS LIKELY TO BE CLOSE.)                                              
   6  RIGHT = LEFT+1                                                    
      IF (RIGHT .LE. N) GOTO 7                                          
         LEFT = N-1                                                     
         RIGHT = N                                                      
   7  DELTA = 1                                                         
   8  IF (DD .LT. D(RIGHT)) GOTO  9                                     
C EXPLODE RIGHT IF NECESSARY.                                           
         LEFT = RIGHT                                                   
         RIGHT = MIN0(N, RIGHT+DELTA)                                   
         DELTA = 2*DELTA                                                
         GOTO  8                                                        
   9  IF (DD .GE. D(LEFT)) GOTO  10                                     
C EXPLODE LEFT IF NECESSARY.                                            
         RIGHT = LEFT                                                   
         LEFT = MAX0(1, LEFT-DELTA)                                     
         DELTA = 2*DELTA                                                
         GOTO  9                                                        
  10  IF (RIGHT-LEFT .LE. 1) GOTO  13                                   
C IF EXPLODED, BISECT.                                                  
         NEW = (LEFT+RIGHT)/2                                           
         IF (D(NEW) .GT. DD) GOTO 11                                    
            LEFT = NEW                                                  
            GOTO  12                                                    
  11        RIGHT = NEW                                                 
  12     CONTINUE                                                       
         GOTO  10                                                       
  13  INTRVD = LEFT                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE VBTOD( E, M, E10, M10 )                                
C                                                                       
C     VBTOD CONVERTS A MACHINE BASE REPRESENTATION                      
C     OF A FLOATING POINT NUMBER INTO A BASE 10                         
C     REPRESENTATION.                                                   
C                                                                       
      REAL M, M10                                                       
      DOUBLE PRECISION D1MACH, EL10B, DLOG10                            
      INTEGER E, E10                                                    
C                                                                       
      IF( M .NE. 0.0E0 ) GO TO 5                                        
        M10 = 0.0E0                                                     
        E10 = 0                                                         
        RETURN                                                          
C                                                                       
  5   CALL UMKFL(M,E10,M10)                                             
      EL10B = DBLE(FLOAT(E+E10))*D1MACH(5)                              
C                                                                       
      M10 = ABS(M10)                                                    
      E10 = IDFLR( EL10B + DLOG10(DBLE(M10)) )                          
      M10 = (10.0D0 ** (EL10B - DBLE(FLOAT(E10)))) * M10                
C                                                                       
 10   IF( M10 .LT. 1.0E0 ) GO TO 20                                     
        M10 = M10/10.0E0                                                
        E10 = E10 + 1                                                   
        GO TO 10                                                        
C                                                                       
 20   IF( M .LT. 0.0E0 ) M10 = -M10                                     
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE DVBTOD( E, M, E10, M10 )                               
C                                                                       
C     DVBTOD CONVERTS A MACHINE BASE REPRESENTATION                     
C     OF A FLOATING POINT NUMBER INTO A BASE 10                         
C     REPRESENTATION.                                                   
C                                                                       
      DOUBLE PRECISION M, M10, EL10B, DLOG10                            
      DOUBLE PRECISION D1MACH                                           
      INTEGER E, E10                                                    
C                                                                       
      IF( M .NE. 0.0D0 ) GO TO 5                                        
        M10 = 0.0D0                                                     
        E10 = 0                                                         
        RETURN                                                          
C                                                                       
  5   CALL DUMKFL(M,E10,M10)                                            
      EL10B = DBLE(FLOAT(E+E10))*D1MACH(5)                              
C                                                                       
      M10 = DABS(M10)                                                   
      E10 = IDFLR( EL10B + DLOG10(M10) )                                
      M10 = (10.0D0 ** (EL10B - DBLE(FLOAT(E10)))) * M10                
C                                                                       
 10   IF( M10 .LT. 1.0D0 ) GO TO 20                                     
        M10 = M10/10.0D0                                                
        E10 = E10 + 1                                                   
        GO TO 10                                                        
C                                                                       
 20   IF( M .LT. 0.0D0 ) M10 = -M10                                     
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE VDTOB( E10, M10, E, M )                                
C                                                                       
C     VDTOB CONVERTS A BASE 10 REPRESENTATION                           
C     OF A FLOATING POINT NUMBER INTO A MACHINE BASE                    
C     REPRESENTATION.                                                   
C                                                                       
      REAL M, M10, BASE                                                 
      DOUBLE PRECISION D1MACH, L10B, DLOG10                             
      INTEGER E, E10                                                    
C                                                                       
      DATA L10B / 0.0D0 /                                               
      DATA BASE / 0.0E0 /                                               
C                                                                       
      IF( BASE .NE. 0.0E0 ) GO TO 1                                     
        L10B = D1MACH(5)                                                
        BASE = I1MACH(10)                                               
C                                                                       
 1    IF( M10 .NE. 0.0E0 ) GO TO 5                                      
        M = 0.0E0                                                       
        E = 0                                                           
        RETURN                                                          
C                                                                       
 5    M = ABS(M10)                                                      
      E = IDFLR( (DBLE(FLOAT(E10)) + DLOG10(DBLE(M)))/L10B )            
      M = ( 10.0D0 ** (DBLE(FLOAT(E10)) - DBLE(FLOAT(E))*L10B) ) * M    
C                                                                       
 10   IF( M .LT. 1.0E0 ) GO TO 20                                       
        M = M / BASE                                                    
        E = E + 1                                                       
        GO TO 10                                                        
C                                                                       
 20   IF( M10 .LT. 0.0E0 ) M = -M                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE DVDTOB( E10, M10, E, M )                               
C                                                                       
C     DVDTOB CONVERTS A BASE 10 REPRESENTATION                          
C     OF A FLOATING POINT NUMBER INTO A MACHINE BASE                    
C     REPRESENTATION.                                                   
C                                                                       
      DOUBLE PRECISION M, M10, L10B, BASE, DLOG10                       
      DOUBLE PRECISION D1MACH                                           
      INTEGER E, E10                                                    
C                                                                       
      DATA L10B / 0.0D0 /                                               
      DATA BASE / 0.0D0 /                                               
C                                                                       
      IF( BASE .NE. 0.0D0 ) GO TO 1                                     
        L10B = D1MACH(5)                                                
        BASE = I1MACH(10)                                               
C                                                                       
 1    IF( M10 .NE. 0.0D0 ) GO TO 5                                      
        M = 0.0D0                                                       
        E = 0                                                           
        RETURN                                                          
C                                                                       
 5    M = DABS(M10)                                                     
      E = IDFLR( (DBLE(FLOAT(E10)) + DLOG10(M))/L10B )                  
      M = ( 10.0D0 ** (DBLE(FLOAT(E10)) - DBLE(FLOAT(E))*L10B) ) * M    
C                                                                       
 10   IF( M .LT. 1.0D0 ) GO TO 20                                       
        M = M / BASE                                                    
        E = E + 1                                                       
        GO TO 10                                                        
C                                                                       
 20   IF( M10 .LT. 0.0D0 ) M = -M                                       
      RETURN                                                            
C                                                                       
      END                                                               
      REAL FUNCTION MKFL( E, M )                                        
C                                                                       
C     MKFL RETURNS (B**E) * M, WHERE B = I1MACH(10)                     
C                                                                       
C     CHANGED JUNE 4, 1976                                              
C                                                                       
C     INPUT NEED NOT BE NORMALIZED                                      
C                                                                       
      REAL M                                                            
      REAL S2MACH                                                       
      INTEGER E, TE                                                     
C                                                                       
      REAL TBASE                                                        
      INTEGER BASE, MINEXP, MAXEXP                                      
C                                                                       
      DATA TBASE / 0.0E0 /                                              
      DATA BASE, MINEXP, MAXEXP / 3*0 /                                 
C                                                                       
      IF( BASE .NE. 0 ) GO TO 5                                         
        BASE   = I1MACH(10)                                             
        MINEXP = I1MACH(12)                                             
        MAXEXP = I1MACH(13)                                             
        TBASE  = BASE                                                   
C                                                                       
  5   TE = E                                                            
      MKFL = M                                                          
C                                                                       
      IF( ABS(M) .GE. 1.0E0 ) GO TO 10                                  
C                                                                       
      IF( TBASE*ABS(M) .GE. 1.0E0 ) GO TO 20                            
      IF( ABS(M) .EQ. 0.0E0 ) RETURN                                    
C                                                                       
C     COMES TO HERE IF ABS(M) TOO LARGE OR TOO SMALL.                   
C                                                                       
 10   CALL UMKFL( M, TE, MKFL )                                         
      TE = E + TE                                                       
C                                                                       
C     NOW M AND TE ARE OK.                                              
C                                                                       
C/6S                                                                    
C20   IF( TE .GT. MAXEXP ) CALL SETERR( 15HMKFL - OVERFLOW, 15, 1, 2 )  
C/7S                                                                    
 20   IF( TE .GT. MAXEXP ) CALL SETERR( 'MKFL - OVERFLOW', 15, 1, 2 )   
C/                                                                      
C                                                                       
      IF( TE .LT. MINEXP )  MKFL = 0.0E0                                
C                                                                       
      MKFL = S2MACH( MKFL, BASE, TE )                                   
C                                                                       
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DMKFL( E, M )                           
C                                                                       
C     DMKFL RETURNS (B**E) * M, WHERE B = I1MACH(10)                    
C                                                                       
C     CHANGED JUNE 4, 1976                                              
C                                                                       
C     INPUT NEED NOT BE NORMALIZED                                      
C                                                                       
      DOUBLE PRECISION M                                                
      DOUBLE PRECISION S3MACH                                           
      INTEGER E, TE                                                     
C                                                                       
      DOUBLE PRECISION TBASE                                            
      INTEGER BASE, MINEXP, MAXEXP                                      
C                                                                       
      DATA TBASE / 0.0D0 /                                              
      DATA BASE, MINEXP, MAXEXP / 3*0 /                                 
C                                                                       
      IF( BASE .NE. 0 ) GO TO 5                                         
        BASE   = I1MACH(10)                                             
        MINEXP = I1MACH(15)                                             
        MAXEXP = I1MACH(16)                                             
        TBASE  = BASE                                                   
C                                                                       
 5    TE = E                                                            
      DMKFL = M                                                         
C                                                                       
      IF( DABS(DMKFL) .GE. 1.0D0 ) GO TO 10                             
C                                                                       
      IF( TBASE*DABS(M) .GE. 1.0D0 ) GO TO 20                           
      IF( DABS(M) .EQ. 0.0D0 ) RETURN                                   
C                                                                       
C     COMES TO HERE IF DABS(M) TOO LARGE OR TOO SMALL.                  
C                                                                       
 10   CALL DUMKFL( M, TE, DMKFL )                                       
      TE = E + TE                                                       
C                                                                       
C     NOW M AND TE ARE OK.                                              
C                                                                       
C/6S                                                                    
C20   IF( TE .GT. MAXEXP ) CALL SETERR( 16HDMKFL - OVERFLOW, 16, 1, 2 ) 
C/7S                                                                    
 20   IF( TE .GT. MAXEXP ) CALL SETERR( 'DMKFL - OVERFLOW', 16, 1, 2 )  
C/                                                                      
C                                                                       
      IF( TE .LT. MINEXP )  DMKFL = 0.0D0                               
C                                                                       
      DMKFL = S3MACH( DMKFL, BASE, TE )                                 
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE UMKFL( F, E, M )                                       
C                                                                       
C     UMKFL DECOMPOSES A FLOATING POINT NUMBER, F, INTO                 
C     ITS EXPONENT, E, AND MANTISSA, M.                                 
C                                                                       
C     IF F .EQ. 0, E = 0 AND M = 0.0D0                                  
C                                                                       
C     IF F .NE. 0, F = (B**E) * M, WHERE                                
C                  B = I1MACH(10) AND                                   
C                  1/B .LE. ABS(M) .LT. 1.0                             
C                                                                       
      REAL F, M                                                         
      REAL S2MACH                                                       
      INTEGER E                                                         
C                                                                       
      REAL    BEXP(6)                                                   
      INTEGER EXP(6)                                                    
      INTEGER NMAX                                                      
C                                                                       
      DATA NMAX /0/                                                     
C                                                                       
      DATA EXP(1), BEXP(1) /1024, 1.0E0 /                               
      DATA EXP(2), BEXP(2) / 256, 1.0E0 /                               
      DATA EXP(3), BEXP(3) /  64, 1.0E0 /                               
      DATA EXP(4), BEXP(4) /  16, 1.0E0 /                               
      DATA EXP(5), BEXP(5) /   4, 1.0E0 /                               
      DATA EXP(6), BEXP(6) /   1, 1.0E0 /                               
C                                                                       
C     INITIALIZE TABLES IF FIRST TIME THROUGH                           
C                                                                       
      IF( NMAX .NE. 0 ) GO TO 20                                        
C                                                                       
      DO 10 I = 1, 6                                                    
      IF( EXP(I) .GE. I1MACH(13) ) GO TO 10                             
        NMAX = NMAX + 1                                                 
        EXP(NMAX) = EXP(I)                                              
        BEXP(NMAX) = S2MACH( 1.0E0, I1MACH(10), EXP(I) )                
 10   CONTINUE                                                          
C                                                                       
C     HERE WE GO                                                        
C                                                                       
 20   M = ABS(F)                                                        
      IF( M .NE. 0.0E0 ) GO TO 30                                       
      E = 0                                                             
      RETURN                                                            
C                                                                       
C     M .NE. 0, COMPUTE E                                               
C                                                                       
 30   E = 1                                                             
 40   IF( M .GE. 1.0E0 ) GO TO 50                                       
        M = M * BEXP(1)                                                 
        E = E - EXP(1)                                                  
        GO TO 40                                                        
C                                                                       
 50   DO 70 I = 1, NMAX                                                 
 60   IF( M .LT. BEXP(I) ) GO TO 70                                     
        M = M / BEXP(I)                                                 
        E = E + EXP(I)                                                  
        GO TO 60                                                        
 70   CONTINUE                                                          
C                                                                       
      M = M / BEXP(NMAX)                                                
      IF( F .LT. 0.0E0 ) M = -M                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE DUMKFL( F, E, M )                                      
C                                                                       
C     DUMKFL DECOMPOSES A FLOATING POINT NUMBER, F, INTO                
C     ITS EXPONENT, E, AND MANTISSA, M.                                 
C                                                                       
C     IF F .EQ. 0, E = 0 AND M = 0.0D0                                  
C                                                                       
C     IF F .NE. 0, F = (B**E) * M, WHERE                                
C                  B = I1MACH(10) AND                                   
C                  1/B .LE. DABS(M) .LT. 1.0                            
C                                                                       
      DOUBLE PRECISION F, M                                             
      DOUBLE PRECISION S3MACH                                           
      INTEGER E                                                         
C                                                                       
      DOUBLE PRECISION BEXP(6)                                          
      INTEGER           EXP(6)                                          
      INTEGER           NMAX                                            
C                                                                       
      DATA NMAX /0/                                                     
C                                                                       
      DATA EXP(1), BEXP(1) /1024, 1.0D0 /                               
      DATA EXP(2), BEXP(2) / 256, 1.0D0 /                               
      DATA EXP(3), BEXP(3) /  64, 1.0D0 /                               
      DATA EXP(4), BEXP(4) /  16, 1.0D0 /                               
      DATA EXP(5), BEXP(5) /   4, 1.0D0 /                               
      DATA EXP(6), BEXP(6) /   1, 1.0D0 /                               
C                                                                       
C     INITIALIZE TABLES IF FIRST TIME THROUGH                           
C                                                                       
      IF( NMAX .NE. 0 ) GO TO 20                                        
C                                                                       
      DO 10 I = 1, 6                                                    
      IF( EXP(I) .GE. I1MACH(16) ) GO TO 10                             
        NMAX = NMAX + 1                                                 
        EXP(NMAX) = EXP(I)                                              
        BEXP(NMAX) = S3MACH( 1.0D0, I1MACH(10), EXP(I) )                
 10   CONTINUE                                                          
C                                                                       
C     HERE WE GO                                                        
C                                                                       
 20   M = DABS(F)                                                       
      IF( M .NE. 0.0D0 ) GO TO 30                                       
      E = 0                                                             
      RETURN                                                            
C                                                                       
C     M .NE. 0, COMPUTE E                                               
C                                                                       
 30   E = 1                                                             
 40   IF( M .GE. 1.0D0 ) GO TO 50                                       
        M = M * BEXP(1)                                                 
        E = E - EXP(1)                                                  
        GO TO 40                                                        
C                                                                       
 50   DO 70 I = 1, NMAX                                                 
 60   IF( M .LT. BEXP(I) ) GO TO 70                                     
        M = M/BEXP(I)                                                   
        E = E + EXP(I)                                                  
        GO TO 60                                                        
 70   CONTINUE                                                          
C                                                                       
      M = M/BEXP(NMAX)                                                  
      IF( F .LT. 0.0D0 ) M = -M                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE CNVBDC(N,A,B)                                          
C                                                                       
C     CNVBDC CONVERTS THE N DOUBLE PRECISION ITEMS IN A                 
C     TO COMPLEX ITEMS PUTTING THE RESULT IN B.                         
C     A BACKWARDS DO LOOP IS USED.                                      
C                                                                       
      DOUBLE PRECISION A(1)                                             
C/R                                                                     
C     REAL B(2,N)                                                       
C/C                                                                     
      COMPLEX B(1)                                                      
C/                                                                      
C                                                                       
      I = N                                                             
C                                                                       
 10   IF(I .LE. 0) RETURN                                               
C/R                                                                     
C       B(1,I) = SNGL(A(I))                                             
C       B(2,I) = A(I)                                                   
C/C                                                                     
        B(I) = CMPLX( SNGL(A(I)), 0.0 )                                 
C/                                                                      
        I = I - 1                                                       
        GO TO 10                                                        
C                                                                       
      END                                                               
      SUBROUTINE CNVBDI(N,A,B)                                          
C                                                                       
C     CNVBDI CONVERTS THE N DOUBLE PRECISION ITEMS IN A                 
C     TO INTEGER ITEMS PUTTING THE RESULT IN B.                         
C     A BACKWARDS DO LOOP IS USED.                                      
C                                                                       
      DOUBLE PRECISION A(1)                                             
      INTEGER B(1)                                                      
C                                                                       
      I = N                                                             
C                                                                       
 10   IF(I .LE. 0) RETURN                                               
        B(I) = A(I)                                                     
        I = I - 1                                                       
        GO TO 10                                                        
C                                                                       
      END                                                               
      SUBROUTINE CNVBDR(N,A,B)                                          
C                                                                       
C     CNVBDR CONVERTS THE N DOUBLE PRECISION ITEMS IN A                 
C     TO REAL ITEMS PUTTING THE RESULT IN B.                            
C     A BACKWARDS DO LOOP IS USED.                                      
C                                                                       
      DOUBLE PRECISION A(1)                                             
      REAL B(1)                                                         
C                                                                       
      I = N                                                             
C                                                                       
 10   IF(I .LE. 0) RETURN                                               
        B(I) = A(I)                                                     
        I = I - 1                                                       
        GO TO 10                                                        
C                                                                       
      END                                                               
      SUBROUTINE CNVBIC(N,A,B)                                          
C                                                                       
C     CNVBIC CONVERTS THE N INTEGER ITEMS IN A                          
C     TO COMPLEX ITEMS PUTTING THE RESULT IN B.                         
C     A BACKWARDS DO LOOP IS USED.                                      
C                                                                       
      INTEGER A(1)                                                      
C/R                                                                     
C     REAL B(2,N)                                                       
C/C                                                                     
      COMPLEX B(1)                                                      
C/                                                                      
C                                                                       
      I = N                                                             
C                                                                       
 10   IF(I .LE. 0) RETURN                                               
C/R                                                                     
C       B(1,I) = FLOAT(A(I))                                            
C       B(2,I) = 0.0                                                    
C/C                                                                     
        B(I) = CMPLX( FLOAT(A(I)), 0.0 )                                
C/                                                                      
        I = I - 1                                                       
        GO TO 10                                                        
C                                                                       
      END                                                               
      SUBROUTINE CNVBID(N,A,B)                                          
C                                                                       
C     CNVBID CONVERTS THE N INTEGER ITEMS IN A                          
C     TO DOUBLE PRECISION ITEMS PUTTING THE RESULT IN B.                
C     A BACKWARDS DO LOOP IS USED.                                      
C                                                                       
      INTEGER A(1)                                                      
      DOUBLE PRECISION B(1)                                             
C                                                                       
      I = N                                                             
C                                                                       
 10   IF(I .LE. 0) RETURN                                               
        B(I) = A(I)                                                     
        I = I - 1                                                       
        GO TO 10                                                        
C                                                                       
      END                                                               
      SUBROUTINE CNVBIR(N,A,B)                                          
C                                                                       
C     CNVBIR CONVERTS THE N INTEGER ITEMS IN A                          
C     TO REAL ITEMS PUTTING THE RESULT IN B.                            
C     A BACKWARDS DO LOOP IS USED.                                      
C                                                                       
      INTEGER A(1)                                                      
      REAL B(1)                                                         
C                                                                       
      I = N                                                             
C                                                                       
 10   IF(I .LE. 0) RETURN                                               
        B(I) = A(I)                                                     
        I = I - 1                                                       
        GO TO 10                                                        
C                                                                       
      END                                                               
      SUBROUTINE CNVBRC(N,A,B)                                          
C                                                                       
C     CNVBRC CONVERTS THE N REAL ITEMS IN A                             
C     TO COMPLEX ITEMS PUTTING THE RESULT IN B.                         
C     A BACKWARDS DO LOOP IS USED.                                      
C                                                                       
      REAL A(1)                                                         
C/R                                                                     
C     REAL B(2,N)                                                       
C/C                                                                     
      COMPLEX B(1)                                                      
C/                                                                      
C                                                                       
      I = N                                                             
C                                                                       
 10   IF(I .LE. 0) RETURN                                               
C/R                                                                     
C       B(1,I) = A(I)                                                   
C       B(2,I) = 0.0                                                    
C/C                                                                     
        B(I) = CMPLX( A(I), 0.0 )                                       
C/                                                                      
        I = I - 1                                                       
        GO TO 10                                                        
C                                                                       
      END                                                               
      SUBROUTINE CNVBRD(N,A,B)                                          
C                                                                       
C     CNVBRD CONVERTS THE N REAL ITEMS IN A                             
C     TO DOUBLE PRECISION ITEMS PUTTING THE RESULT IN B.                
C     A BACKWARDS DO LOOP IS USED.                                      
C                                                                       
      REAL A(1)                                                         
      DOUBLE PRECISION B(1)                                             
C                                                                       
      I = N                                                             
C                                                                       
 10   IF(I .LE. 0) RETURN                                               
        B(I) = A(I)                                                     
        I = I - 1                                                       
        GO TO 10                                                        
C                                                                       
      END                                                               
      SUBROUTINE CNVBRI(N,A,B)                                          
C                                                                       
C     CNVBRI CONVERTS THE N REAL ITEMS IN A                             
C     TO INTEGER ITEMS PUTTING THE RESULT IN B.                         
C     A BACKWARDS DO LOOP IS USED.                                      
C                                                                       
      REAL A(1)                                                         
      INTEGER B(1)                                                      
C                                                                       
      I = N                                                             
C                                                                       
 10   IF(I .LE. 0) RETURN                                               
        B(I) = A(I)                                                     
        I = I - 1                                                       
        GO TO 10                                                        
C                                                                       
      END                                                               
      SUBROUTINE CNVFDC(N,A,B)                                          
C                                                                       
C     CNVFDC CONVERTS THE N DOUBLE PRECISION ITEMS IN A                 
C     TO COMPLEX ITEMS PUTTING THE RESULT IN B.                         
C     A FOWARDS DO LOOP IS USED.                                        
C                                                                       
      DOUBLE PRECISION A(1)                                             
C/R                                                                     
C     REAL B(2,N)                                                       
C/C                                                                     
      COMPLEX B(1)                                                      
C/                                                                      
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
C/R                                                                     
C       B(1,I) = SNGL(A(I))                                             
C10     B(2,I) = 0.0                                                    
C/C                                                                     
 10     B(I) = CMPLX( SNGL(A(I)), 0.0 )                                 
C/                                                                      
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE CNVFDI(N,A,B)                                          
C                                                                       
C     CNVFDI CONVERTS THE N DOUBLE PRECISION ITEMS IN A                 
C     TO INTEGER ITEMS PUTTING THE RESULT IN B.                         
C     A FOWARDS DO LOOP IS USED.                                        
C                                                                       
      DOUBLE PRECISION A(1)                                             
      INTEGER B(1)                                                      
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = A(I)                                                     
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE CNVFDR(N,A,B)                                          
C                                                                       
C     CNVFDR CONVERTS THE N DOUBLE PRECISION ITEMS IN A                 
C     TO REAL ITEMS PUTTING THE RESULT IN B.                            
C     A FOWARDS DO LOOP IS USED.                                        
C                                                                       
      DOUBLE PRECISION A(1)                                             
      REAL B(1)                                                         
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = A(I)                                                     
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE CNVFIC(N,A,B)                                          
C                                                                       
C     CNVFIC CONVERTS THE N INTEGER ITEMS IN A                          
C     TO COMPLEX ITEMS PUTTING THE RESULT IN B.                         
C     A FORWARD DO LOOP IS USED.                                        
C                                                                       
      INTEGER A(1)                                                      
C/R                                                                     
C     REAL B(2,N)                                                       
C/C                                                                     
      COMPLEX B(1)                                                      
C/                                                                      
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
C/R                                                                     
C       B(1,I) = FLOAT(A(I))                                            
C10     B(2,I) = 0.0                                                    
C/C                                                                     
 10     B(I) = CMPLX( FLOAT(A(I)), 0.0 )                                
C/                                                                      
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE CNVFID(N,A,B)                                          
C                                                                       
C     CNVFID CONVERTS THE N INTEGER ITEMS IN A                          
C     TO DOUBLE PRECISION ITEMS PUTTING THE RESULT IN B.                
C     A FOWARDS DO LOOP IS USED.                                        
C                                                                       
      INTEGER A(1)                                                      
      DOUBLE PRECISION B(1)                                             
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = A(I)                                                     
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE CNVFIR(N,A,B)                                          
C                                                                       
C     CNVFIR CONVERTS THE N INTEGER ITEMS IN A                          
C     TO REAL ITEMS PUTTING THE RESULT IN B.                            
C     A FOWARDS DO LOOP IS USED.                                        
C                                                                       
      INTEGER A(1)                                                      
      REAL B(1)                                                         
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = A(I)                                                     
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE CNVFRC(N,A,B)                                          
C                                                                       
C     CNVFRC CONVERTS THE N REAL ITEMS IN A                             
C     TO COMPLEX ITEMS PUTTING THE RESULT IN B.                         
C     A FOWARDS DO LOOP IS USED.                                        
C                                                                       
      REAL A(1)                                                         
C/R                                                                     
C     REAL B(2,N)                                                       
C/C                                                                     
      COMPLEX B(1)                                                      
C/                                                                      
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
C/R                                                                     
C       B(1,I) = A(I)                                                   
C10     B(2,I) = 0.0                                                    
C/C                                                                     
 10     B(I) = CMPLX( A(I), 0.0 )                                       
C/                                                                      
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE CNVFRD(N,A,B)                                          
C                                                                       
C     CNVFRD CONVERTS THE N REAL ITEMS IN A                             
C     TO DOUBLE PRECISION ITEMS PUTTING THE RESULT IN B.                
C     A FOWARDS DO LOOP IS USED.                                        
C                                                                       
      REAL A(1)                                                         
      DOUBLE PRECISION B(1)                                             
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = A(I)                                                     
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE CNVFRI(N,A,B)                                          
C                                                                       
C     CNVFRI CONVERTS THE N REAL ITEMS IN A                             
C     TO INTEGER ITEMS PUTTING THE RESULT IN B.                         
C     A FOWARDS DO LOOP IS USED.                                        
C                                                                       
      REAL A(1)                                                         
      INTEGER B(1)                                                      
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = A(I)                                                     
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE MOVEBC(N,A,B)                                          
C                                                                       
C     MOVEBC MOVES N COMPLEX ITEMS FROM A TO B                          
C     USING A BACKWARDS DO LOOP                                         
C                                                                       
C/R                                                                     
C     REAL A(2,N), B(2,N)                                               
C/C                                                                     
      COMPLEX A(1),B(1)                                                 
C/                                                                      
C                                                                       
      I = N                                                             
C                                                                       
 10   IF(I .LE. 0) RETURN                                               
C/R                                                                     
C       B(2,I) = A(2,I)                                                 
C       B(1,I) = A(1,I)                                                 
C/C                                                                     
        B(I) = A(I)                                                     
C/                                                                      
        I = I - 1                                                       
        GO TO 10                                                        
C                                                                       
      END                                                               
      SUBROUTINE MOVEBD(N,A,B)                                          
C                                                                       
C     MOVEBD MOVES N DOUBLE PRECISION ITEMS FROM A TO B                 
C     USING A BACKWARDS DO LOOP                                         
C                                                                       
      DOUBLE PRECISION A(1),B(1)                                        
C                                                                       
      I = N                                                             
C                                                                       
 10   IF(I .LE. 0) RETURN                                               
        B(I) = A(I)                                                     
        I = I - 1                                                       
        GO TO 10                                                        
C                                                                       
      END                                                               
      SUBROUTINE MOVEBI(N,A,B)                                          
C                                                                       
C     MOVEBI MOVES N INTEGER ITEMS FROM A TO B                          
C     USING A BACKWARDS DO LOOP                                         
C                                                                       
      INTEGER A(1),B(1)                                                 
C                                                                       
      I = N                                                             
C                                                                       
 10   IF(I .LE. 0) RETURN                                               
        B(I) = A(I)                                                     
        I = I - 1                                                       
        GO TO 10                                                        
C                                                                       
      END                                                               
      SUBROUTINE MOVEBL(N,A,B)                                          
C                                                                       
C     MOVEBL MOVES N LOGICAL ITEMS FROM A TO B                          
C     USING A BACKWARDS DO LOOP                                         
C                                                                       
      LOGICAL A(1),B(1)                                                 
C                                                                       
      I = N                                                             
C                                                                       
 10   IF(I .LE. 0) RETURN                                               
        B(I) = A(I)                                                     
        I = I - 1                                                       
        GO TO 10                                                        
C                                                                       
      END                                                               
      SUBROUTINE MOVEBR(N,A,B)                                          
C                                                                       
C     MOVEBR MOVES N REAL ITEMS FROM A TO B                             
C     USING A BACKWARDS DO LOOP                                         
C                                                                       
      REAL A(1),B(1)                                                    
C                                                                       
      I = N                                                             
C                                                                       
 10   IF(I .LE. 0) RETURN                                               
        B(I) = A(I)                                                     
        I = I - 1                                                       
        GO TO 10                                                        
C                                                                       
      END                                                               
      SUBROUTINE MOVEFC(N,A,B)                                          
C                                                                       
C     MOVEFC MOVES N COMPLEX ITEMS FROM A TO B                          
C     USING A FORWARDS DO LOOP                                          
C                                                                       
C/R                                                                     
C     REAL A(2,N), B(2,N)                                               
C/C                                                                     
      COMPLEX A(1),B(1)                                                 
C/                                                                      
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
C/R                                                                     
C       B(1,I) = A(1,I)                                                 
C10     B(2,I) = A(2,I)                                                 
C/C                                                                     
 10     B(I) = A(I)                                                     
C/                                                                      
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE MOVEFD(N,A,B)                                          
C                                                                       
C     MOVEFD MOVES N DOUBLE PRECISION ITEMS FROM A TO B                 
C     USING A FORWARDS DO LOOP                                          
C                                                                       
      DOUBLE PRECISION A(1),B(1)                                        
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = A(I)                                                     
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE MOVEFI(N,A,B)                                          
C                                                                       
C     MOVEFI MOVES N INTEGER ITEMS FROM A TO B                          
C     USING A FORWARDS DO LOOP                                          
C                                                                       
      INTEGER A(1),B(1)                                                 
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = A(I)                                                     
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE MOVEFL(N,A,B)                                          
C                                                                       
C     MOVEFL MOVES N LOGICAL ITEMS FROM A TO B                          
C     USING A FORWARDS DO LOOP                                          
C                                                                       
      LOGICAL A(1),B(1)                                                 
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = A(I)                                                     
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE MOVEFR(N,A,B)                                          
C                                                                       
C     MOVEFR MOVES N REAL ITEMS FROM A TO B                             
C     USING A FORWARDS DO LOOP                                          
C                                                                       
      REAL A(1),B(1)                                                    
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = A(I)                                                     
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE SETC(N,V,B)                                            
C                                                                       
C     SETC SETS THE N COMPLEX ITEMS IN B TO V                           
C                                                                       
C/R                                                                     
C     REAL B(2,N), V(2), V1, V2                                         
C     V1 = V(1)                                                         
C     V2 = V(2)                                                         
C/C                                                                     
      COMPLEX B(1),V                                                    
C/                                                                      
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
C/R                                                                     
C       B(1,I) = V1                                                     
C10     B(2,I) = V2                                                     
C/C                                                                     
 10     B(I) = V                                                        
C/                                                                      
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE SETD(N,V,B)                                            
C                                                                       
C     SETD SETS THE N DOUBLE PRECISION ITEMS IN B TO V                  
C                                                                       
      DOUBLE PRECISION B(1),V                                           
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = V                                                        
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE SETI(N,V,B)                                            
C                                                                       
C     SETI SETS THE N INTEGER ITEMS IN B TO V                           
C                                                                       
      INTEGER B(1),V                                                    
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = V                                                        
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE SETL(N,V,B)                                            
C                                                                       
C     SETL SETS THE N LOGICAL ITEMS IN B TO V                           
C                                                                       
      LOGICAL B(1),V                                                    
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = V                                                        
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE SETR(N,V,B)                                            
C                                                                       
C     SETR SETS THE N REAL ITEMS IN B TO V                              
C                                                                       
      REAL B(1),V                                                       
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = V                                                        
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      REAL FUNCTION CR1MOD(R,I)                                         
C MODULUS OF A COMPLEX NUMBER AVOIDING OVERFLOW.                        
      REAL R,I,AR,AI,ABS,SQRT                                           
      AR = ABS(R)                                                       
      AI = ABS(I)                                                       
      IF (AR .GE. AI) GO TO 10                                          
          CR1MOD = AI*SQRT(1.0E0+(AR/AI)**2)                            
          RETURN                                                        
   10 IF (AR .LE. AI) GO TO 20                                          
          CR1MOD = AR*SQRT(1.0E0+(AI/AR)**2)                            
          RETURN                                                        
   20 CR1MOD = AR*SQRT(2.0E0)                                           
      RETURN                                                            
      END                                                               
      SUBROUTINE CR1DIV(AR,AI,BR,BI,CR,CI)                              
C                                                                       
C COMPLEX DIVISION C = A/B, AVOIDING OVERFLOW.                          
C                                                                       
      REAL AR,AI,BR,BI,CR,CI,R,D,R1MACH,ABS                             
C                                                                       
      IF (BR .NE. 0.0E0  .OR. BI .NE. 0.0E0) GO TO 10                   
C                                                                       
C DIVISION BY ZERO, C = INFINITY -                                      
          CR = R1MACH(2)                                                
          CI = CR                                                       
          RETURN                                                        
C                                                                       
   10 IF (ABS(BR) .GE. ABS(BI)) GO TO 20                                
          R = BR/BI                                                     
          D = BI+R*BR                                                   
          CR = (AR*R+AI)/D                                              
          CI = (AI*R-AR)/D                                              
          RETURN                                                        
C                                                                       
   20 R = BI/BR                                                         
      D = BR+R*BI                                                       
      CR = (AR+AI*R)/D                                                  
      CI = (AI-AR*R)/D                                                  
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION CD1MOD(R,I)                             
C MODULUS OF A COMPLEX NUMBER AVOIDING OVERFLOW.                        
      DOUBLE PRECISION R,I,AR,AI,DSQRT                                  
      AR = DABS(R)                                                      
      AI = DABS(I)                                                      
      IF (AR .GE. AI) GO TO 10                                          
          CD1MOD = AI*DSQRT(1.0D0+(AR/AI)**2)                           
          RETURN                                                        
   10 IF (AR .LE. AI) GO TO 20                                          
          CD1MOD = AR*DSQRT(1.0D0+(AI/AR)**2)                           
          RETURN                                                        
   20 CD1MOD = AR*DSQRT(2.0D0)                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE CD1DIV(AR,AI,BR,BI,CR,CI)                              
C                                                                       
C COMPLEX DIVISION C = A/B, AVOIDING OVERFLOW.                          
C                                                                       
      DOUBLE PRECISION AR,AI,BR,BI,CR,CI,R,D,D1MACH                     
C                                                                       
      IF (BR .NE. 0.0D0  .OR. BI .NE. 0.0D0) GO TO 10                   
C                                                                       
C DIVISION BY ZERO, C = INFINITY -                                      
          CR = D1MACH(2)                                                
          CI = CR                                                       
          RETURN                                                        
C                                                                       
   10 IF (DABS(BR) .GE. DABS(BI)) GO TO 20                              
          R = BR/BI                                                     
          D = BI+R*BR                                                   
          CR = (AR*R+AI)/D                                              
          CI = (AI*R-AR)/D                                              
          RETURN                                                        
C                                                                       
   20 R = BI/BR                                                         
      D = BR+R*BI                                                       
      CR = (AR+AI*R)/D                                                  
      CI = (AI-AR*R)/D                                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE CDADD(A,B,C)                                           
      DOUBLE PRECISION A(2),B(2),C(2)                                   
C                                                                       
C COMPLEX DOUBLE PRECISION ADDITION                                     
C                                                                       
      C(1) = A(1) + B(1)                                                
      C(2) = A(2) + B(2)                                                
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE CDSUB(A,B,C)                                           
      DOUBLE PRECISION A(2),B(2),C(2)                                   
C                                                                       
C COMPLEX DOUBLE PRECISION SUBTRACTION                                  
C                                                                       
      C(1) = A(1) - B(1)                                                
      C(2) = A(2) - B(2)                                                
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE CDMUL(A,B,C)                                           
      DOUBLE PRECISION A(2),B(2),C(2),T(2)                              
C                                                                       
      T(1) = A(1)*B(1) - A(2)*B(2)                                      
      T(2) = A(2)*B(1) + A(1)*B(2)                                      
C                                                                       
      C(1) = T(1)                                                       
      C(2) = T(2)                                                       
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE CDDIV(A,B,C)                                           
      DOUBLE PRECISION A(2),B(2),C(2),G,H,T                             
C                                                                       
C THIS ROUTINE DOES COMPLEX DOUBLE PRECISION                            
C DIVISION (C=A/B), FOLLOWING THE METHOD                                
C GIVEN IN ALGOL ON PAGES 357 AND 358 OF                                
C WILKINSON AND REINSCHS  BOOK-                                         
C HANDBOOK FOR AUTOMATIC COMPUTATION                                    
C SPRINGER-VERLAG 1971                                                  
C                                                                       
C THIS VERSION HAS BEEN CHANGED SLIGHTLY TO PREVENT                     
C INPUTS A AND B FROM BEING DESTROYED.                                  
C WRITTEN MARCH 20, 1975 BY P. FOX                                      
C                                                                       
C FOR ACCURACY THE COMPUTATION IS DONE DIFFERENTLY                      
C DEPENDING ON WHETHER THE REAL OR IMAGINARY PART OF                    
C B IS LARGER                                                           
C                                                                       
      IF ( DABS(B(1)) .GT. DABS(B(2)) ) GO TO 10                        
      H = B(1)/B(2)                                                     
      G = H*B(1) + B(2)                                                 
      T = A(1)                                                          
      C(1) = (H * T  + A(2))/G                                          
      C(2) = (H * A(2) - T)/G                                           
      RETURN                                                            
C                                                                       
C IF THE REAL PART OF B IS LARGER THAN THE IMAGINARY-                   
   10 H = B(2)/B(1)                                                     
      G = H*B(2) + B(1)                                                 
      T = A(1)                                                          
      C(1) = ( T  + H * A(2))/G                                         
      C(2) = (A(2) - H * T)/G                                           
      RETURN                                                            
      END                                                               
      SUBROUTINE APRNTC(A, NITEMS, IOUT, MCOL, W, D)                    
C                                                                       
C  THIS SUBROUTINE PRINTS OUT NITEMS FROM THE COMPLEX ARRAY, A, ON      
C  OUTPUT UNIT IOUT, USING A MAXIMUM OF MCOL PRINT SPACES.              
C  THE OUTPUT FORMAT IS 2(1PEW.D).                                      
C  THE PROGRAM PUTS AS MANY VALUES ON A LINE AS POSSIBLE.               
C  W SHOULD BE INPUT AS THE ACTUAL WIDTH +1 FOR A SPACE BETWEEN VALUES. 
C                                                                       
C  DUPLICATE LINES ARE NOT ALL PRINTED, BUT ARE INDICATED BY ASTERISKS. 
C                                                                       
C  WRITTEN BY DAN WARNER, REVISED BY PHYL FOX, OCTOBER 21, 1982.        
C                                                                       
C  THE LINE WIDTH IS COMPUTED AS THE MINIMUM OF THE INPUT MCOL AND 160. 
C  IF THE LINE WIDTH IS TO BE INCREASED ABOVE 160, THE BUFFERS LINE()   
C  AND LAST(), WHICH THE VALUES TO BE PRINTED ON ONE LINE, MUST         
C  BE DIMENSIONED ACCORDINGLY.                                          
C                                                                       
C  INPUT PARAMETERS -                                                   
C                                                                       
C    A        - THE START OF THE COMPLEX ARRAY TO BE PRINTED            
C                                                                       
C    NITEMS   - THE NUMBER OF ITEMS TO BE PRINTED                       
C                                                                       
C    IOUT     - THE OUTPUT UNIT FOR PRINTING                            
C                                                                       
C    MCOL     - THE NUMBER OF SPACES ACROSS THE LINE                    
C                                                                       
C    W        - THE WIDTH OF THE PRINTED VALUE (1PEW.D)                 
C                                                                       
C    D        - THE NUMBER OF DIGITS AFTER THE DECIMAL POINT (1PEW.D)   
C                                                                       
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - NITEMS .LE. ZERO                                               
C                                                                       
C    2 - W .GT. MCOL                                                    
C                                                                       
C    3 - D .LT. ZERO                                                    
C                                                                       
C    4 - W .LT. D+6                                                     
C                                                                       
      INTEGER  NITEMS, IOUT, MCOL, W, D                                 
C/R                                                                     
C     REAL  A(2,NITEMS)                                                 
C/C                                                                     
      COMPLEX  A(NITEMS)                                                
C/                                                                      
C                                                                       
      INTEGER  MAX0, MIN0, WW, DD, EMIN, EMAX,                          
     1         EXPENT, I1MACH, ICEIL, IABS, I10WID                      
C/6S                                                                    
C     INTEGER  IFMT1(20), IFMT2(18), BLANK, STAR                        
C     INTEGER IFMT1C(20), IFMT2C(18)                                    
C     EQUIVALENCE (IFMT1(1),IFMT1C(1)), (IFMT2(1),IFMT2C(1))            
C/7S                                                                    
      CHARACTER*1  IFMT1(20), IFMT2(18), BLANK, STAR                    
      CHARACTER*20 IFMT1C                                               
      CHARACTER*18 IFMT2C                                               
      EQUIVALENCE (IFMT1(1),IFMT1C), (IFMT2(1),IFMT2C)                  
C/                                                                      
      INTEGER  INDW, NCOL, COUNT, I, J, K, ILINE, ILAST                 
      LOGICAL  DUP                                                      
C/R                                                                     
C     REAL LINE(2,18), LAST(2,18)                                       
C/C                                                                     
      COMPLEX  LINE(18), LAST(18)                                       
C/                                                                      
      REAL  LOGETA                                                      
C                                                                       
C/6S                                                                    
C     DATA BLANK/1H /, STAR/1H*/, INDW/7/, EXPENT/0/                    
C/7S                                                                    
      DATA BLANK/' '/, STAR/'*'/, INDW/7/, EXPENT/0/                    
C/                                                                      
C                                                                       
C  IFMT1 IS FOR THE ASTERISK LINES, IFMT2 FOR THE DATA LINES            
C                                                                       
C/6S                                                                    
C     DATA IFMT1( 1) /1H(/,  IFMT2( 1) /1H(/                            
C     DATA IFMT1( 2) /1H1/,  IFMT2( 2) /1H1/                            
C     DATA IFMT1( 3) /1HA/,  IFMT2( 3) /1HA/                            
C     DATA IFMT1( 4) /1H1/,  IFMT2( 4) /1H1/                            
C     DATA IFMT1( 5) /1H,/,  IFMT2( 5) /1H,/                            
C     DATA IFMT1( 6) /1H5/,  IFMT2( 6) /1HI/                            
C     DATA IFMT1( 7) /1HX/,  IFMT2( 7) /1H7/                            
C     DATA IFMT1( 8) /1H,/,  IFMT2( 8) /1H,/                            
C     DATA IFMT1( 9) /1H2/,  IFMT2( 9) /1H1/                            
C     DATA IFMT1(10) /1HA/,  IFMT2(10) /1HP/                            
C     DATA IFMT1(11) /1H1/,  IFMT2(11) /1H /                            
C     DATA IFMT1(12) /1H,/,  IFMT2(12) /1HE/                            
C     DATA IFMT1(13) /1H /,  IFMT2(13) /1H /                            
C     DATA IFMT1(14) /1H /,  IFMT2(14) /1H /                            
C     DATA IFMT1(15) /1HX/,  IFMT2(15) /1H./                            
C     DATA IFMT1(16) /1H,/,  IFMT2(16) /1H /                            
C     DATA IFMT1(17) /1H2/,  IFMT2(17) /1H /                            
C     DATA IFMT1(18) /1HA/,  IFMT2(18) /1H)/                            
C     DATA IFMT1(19) /1H1/                                              
C     DATA IFMT1(20) /1H)/                                              
C/7S                                                                    
      DATA IFMT1( 1) /'('/,  IFMT2( 1) /'('/                            
      DATA IFMT1( 2) /'1'/,  IFMT2( 2) /'1'/                            
      DATA IFMT1( 3) /'A'/,  IFMT2( 3) /'A'/                            
      DATA IFMT1( 4) /'1'/,  IFMT2( 4) /'1'/                            
      DATA IFMT1( 5) /','/,  IFMT2( 5) /','/                            
      DATA IFMT1( 6) /'5'/,  IFMT2( 6) /'I'/                            
      DATA IFMT1( 7) /'X'/,  IFMT2( 7) /'7'/                            
      DATA IFMT1( 8) /','/,  IFMT2( 8) /','/                            
      DATA IFMT1( 9) /'2'/,  IFMT2( 9) /'1'/                            
      DATA IFMT1(10) /'A'/,  IFMT2(10) /'P'/                            
      DATA IFMT1(11) /'1'/,  IFMT2(11) /' '/                            
      DATA IFMT1(12) /','/,  IFMT2(12) /'E'/                            
      DATA IFMT1(13) /' '/,  IFMT2(13) /' '/                            
      DATA IFMT1(14) /' '/,  IFMT2(14) /' '/                            
      DATA IFMT1(15) /'X'/,  IFMT2(15) /'.'/                            
      DATA IFMT1(16) /','/,  IFMT2(16) /' '/                            
      DATA IFMT1(17) /'2'/,  IFMT2(17) /' '/                            
      DATA IFMT1(18) /'A'/,  IFMT2(18) /')'/                            
      DATA IFMT1(19) /'1'/                                              
      DATA IFMT1(20) /')'/                                              
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NITEMS .LE. 0) CALL                                           
C    1  SETERR(27H  APRNTC - NITEMS .LE. ZERO, 27, 1, 2)                
C/7S                                                                    
      IF (NITEMS .LE. 0) CALL                                           
     1  SETERR('  APRNTC - NITEMS .LE. ZERO', 27, 1, 2)                 
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (W .GT. MCOL) CALL                                             
C    1  SETERR(22H  APRNTC - W .GT. MCOL, 22, 2, 2)                     
C/7S                                                                    
      IF (W .GT. MCOL) CALL                                             
     1  SETERR('  APRNTC - W .GT. MCOL', 22, 2, 2)                      
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (D .LT. 0) CALL                                                
C    1  SETERR(22H  APRNTC - D .LT. ZERO, 22, 3, 2)                     
C/7S                                                                    
      IF (D .LT. 0) CALL                                                
     1  SETERR('  APRNTC - D .LT. ZERO', 22, 3, 2)                      
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (W .LT. D+6) CALL                                              
C    1  SETERR(21H  APRNTC - W .LT. D+6, 21, 4, 2)                      
C/7S                                                                    
      IF (W .LT. D+6) CALL                                              
     1  SETERR('  APRNTC - W .LT. D+6', 21, 4, 2)                       
C/                                                                      
C                                                                       
C                                                                       
C     EXPENT IS USED AS A FIRST-TIME SWITCH TO SIGNAL IF THE            
C     MACHINE-VALUE CONSTANTS HAVE BEEN COMPUTED.                       
C                                                                       
      IF (EXPENT .GT. 0) GO TO 10                                       
         LOGETA = ALOG10(FLOAT(I1MACH(10)))                             
         EMIN   = ICEIL(LOGETA*FLOAT(IABS(I1MACH(12)-1)))               
         EMAX   = ICEIL(LOGETA*FLOAT(I1MACH(13)))                       
         EXPENT = I10WID(MAX0(EMIN, EMAX))                              
C                                                                       
C     COMPUTE THE FORMATS.                                              
C                                                                       
   10 WW = MIN0(99, MAX0(W, 5+EXPENT))                                  
      CALL S88FMT(2, WW, IFMT2(13))                                     
      DD = MIN0(D, (WW-(5+EXPENT)))                                     
      CALL S88FMT(2, DD, IFMT2(16))                                     
C                                                                       
C  NCOL IS THE NUMBER OF VALUES TO BE PRINTED ACROSS THE LINE.          
C                                                                       
      NCOL = MAX0(1, MIN0(9, (MIN0(MCOL,160)-INDW)/(2*WW)))             
      CALL S88FMT(1, (2*NCOL), IFMT2(11))                               
      WW = WW-2                                                         
C                                                                       
C  THE ASTERISKS ARE POSITIONED RIGHT-ADJUSTED IN THE W-WIDTH SPACE.    
      CALL S88FMT(2, WW, IFMT1(13))                                     
C                                                                       
C  I COUNTS THE NUMBER OF ITEMS TO BE PRINTED,                          
C  J COUNTS THE NUMBER ON A GIVEN LINE,                                 
C  COUNT COUNTS THE NUMBER OF DUPLICATE LINES.                          
C                                                                       
      I = 1                                                             
      J = 0                                                             
      COUNT = 0                                                         
C                                                                       
C  THE LOGICAL OF THE FOLLOWING IS ROUGHLY THIS -                       
C  IF THERE ARE STILL MORE ITEMS TO BE PRINTED, A LINE-                 
C  FULL IS PUT INTO THE ARRAY, LINE.                                    
C  WHENEVER A LINE IS PRINTED OUT, IT IS ALSO STUFFED INTO              
C  THE ARRAY, LAST, TO COMPARE WITH THE NEXT ONE COMING IN              
C  TO CHECK FOR REPEAT OR DUPLICATED LINES.                             
C  ALSO WHENEVER A LINE IS WRITTEN OUT, THE DUPLICATION                 
C  COUNTER, COUNT, IS SET TO ONE.                                       
C  THE ONLY MILDLY TRICKY PART IS TO NOTE THAT COUNT HAS TO             
C  GO TO 3 BEFORE A LINE OF ASTERISKS IS PRINTED BECAUSE                
C  OF COURSE NO SUCH LINE IS PRINTED FOR JUST A PAIR OF                 
C  DUPLICATE LINES.                                                     
C                                                                       
C  ILINE IS PRINTED AS THE INDEX OF THE FIRST ARRAY ELEMENT             
C  IN A LINE.                                                           
C                                                                       
C                                                                       
   20 IF (I .GT. NITEMS)  GO TO 90                                      
        J = J+1                                                         
C/R                                                                     
C       LINE(1,J) = A(1,J)                                              
C       LINE(2,J) = A(2,J)                                              
C/C                                                                     
        LINE(J) = A(I)                                                  
C/                                                                      
        IF (J .EQ. 1) ILINE = I                                         
        IF (J .LT. NCOL .AND. I .LT. NITEMS) GO TO 80                   
          IF (COUNT .EQ. 0) GO TO 50                                    
            DUP = .TRUE.                                                
            DO 30 K=1,NCOL                                              
C/R                                                                     
C             IF (LAST(1,K) .NE. LINE(1,K)  .OR.                        
C    1            LAST(2,K) .NE. LINE(2,K))                             
C    2            DUP = .FALSE.                                         
C/C                                                                     
              IF (REAL(LAST(K)) .NE. REAL(LINE(K))  .OR.                
     1            AIMAG(LAST(K)) .NE. AIMAG(LINE(K)))                   
     2            DUP = .FALSE.                                         
C/                                                                      
   30       CONTINUE                                                    
            IF (I .EQ. NITEMS  .AND.  J .LT. NCOL) DUP = .FALSE.        
            IF (.NOT. DUP .AND. COUNT .EQ. 1) GO TO 50                  
              IF (.NOT. DUP) GO TO 40                                   
                COUNT = COUNT+1                                         
                IF (COUNT .EQ. 3) WRITE(IOUT, IFMT1C) BLANK,            
     1                                 STAR, STAR, STAR, STAR           
                IF (I .EQ. NITEMS)  GO TO 50                            
                  GO TO 70                                              
C/R                                                                     
C  40         WRITE(IOUT, IFMT2C) BLANK, ILAST, (LAST(1,K),             
C    1              LAST(2,K), K=1,NCOL)                                
C  50     WRITE(IOUT, IFMT2C) BLANK, ILINE, (LINE(1,K),                 
C    1              LINE(2,K), K=1,J)                                   
C/C                                                                     
   40         WRITE(IOUT, IFMT2C) BLANK, ILAST, (LAST(K), K=1,NCOL)     
   50     WRITE(IOUT, IFMT2C) BLANK, ILINE, (LINE(K), K=1,J)            
C/                                                                      
          COUNT = 1                                                     
          DO 60 K=1,NCOL                                                
C/R                                                                     
C           LAST(1,K) = LINE(1,K)                                       
C  60       LAST(2,K) = LINE(2,K)                                       
C/C                                                                     
   60       LAST(K) = LINE(K)                                           
C/                                                                      
   70     ILAST = ILINE                                                 
          J = 0                                                         
   80   I = I+1                                                         
        GO TO 20                                                        
   90 RETURN                                                            
      END                                                               
      SUBROUTINE APRNTD(A, NITEMS, IOUT, MCOL, W, D)                    
C                                                                       
C  THIS SUBROUTINE PRINTS OUT NITEMS FROM THE DOUBLE PRECISION ARRAY,   
C  A, ON OUTPUT UNIT IOUT, USING A MAXIMUM OF MCOL PRINT SPACES.        
C  THE OUTPUT FORMAT IS 1PDW.D.                                         
C  THE PROGRAM PUTS AS MANY VALUES ON A LINE AS POSSIBLE.               
C  W SHOULD BE INPUT AS THE ACTUAL WIDTH +1 FOR A SPACE BETWEEN VALUES. 
C                                                                       
C  DUPLICATE LINES ARE NOT ALL PRINTED, BUT ARE INDICATED BY ASTERISKS. 
C                                                                       
C  WRITTEN BY DAN WARNER, REVISED BY PHYL FOX, OCTOBER 21, 1982.        
C                                                                       
C  THE LINE WIDTH IS COMPUTED AS THE MINIMUM OF THE INPUT MCOL AND 160. 
C  IF THE LINE WIDTH IS TO BE INCREASED ABOVE 160, THE BUFFERS LINE()   
C  AND LAST(), WHICH THE VALUES TO BE PRINTED ON ONE LINE, MUST         
C  BE DIMENSIONED ACCORDINGLY.                                          
C                                                                       
C  INPUT PARAMETERS -                                                   
C                                                                       
C    A        - THE START OF THE DOUBLE PRECISION ARRAY TO BE PRINTED   
C                                                                       
C    NITEMS   - THE NUMBER OF ITEMS TO BE PRINTED                       
C                                                                       
C    IOUT     - THE OUTPUT UNIT FOR PRINTING                            
C                                                                       
C    MCOL     - THE NUMBER OF SPACES ACROSS THE LINE                    
C                                                                       
C    W        - THE WIDTH OF THE PRINTED VALUE (1PDW.D)                 
C                                                                       
C    D        - THE NUMBER OF DIGITS AFTER THE DECIMAL POINT (1PDW.D)   
C                                                                       
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - NITEMS .LE. ZERO                                               
C                                                                       
C    2 - W .GT. MCOL                                                    
C                                                                       
C    3 - D .LT. ZERO                                                    
C                                                                       
C    4 - W .LT. D+6                                                     
C                                                                       
      INTEGER  NITEMS, IOUT, MCOL, W, D                                 
      DOUBLE PRECISION  A(NITEMS)                                       
C                                                                       
      INTEGER  MAX0, MIN0, WW, DD, EMIN, EMAX,                          
     1         EXPENT, I1MACH, ICEIL, IABS, I10WID                      
C/6S                                                                    
C     INTEGER  IFMT1(20), IFMT1C(20), IFMT2(18), IFMT2C(18), BLANK, STAR
C     EQUIVALENCE (IFMT1(1),IFMT1C(1)), (IFMT2(1),IFMT2C(1))            
C/7S                                                                    
      CHARACTER*1  IFMT1(20), IFMT2(18), BLANK, STAR                    
      CHARACTER*20 IFMT1C                                               
      CHARACTER*18 IFMT2C                                               
      EQUIVALENCE (IFMT1(1),IFMT1C), (IFMT2(1),IFMT2C)                  
C/                                                                      
      INTEGER  INDW, NCOL, COUNT, I, J, K, ILINE, ILAST                 
      LOGICAL  DUP                                                      
      DOUBLE PRECISION  LINE(18), LAST(18)                              
      REAL  LOGETA                                                      
C                                                                       
C/6S                                                                    
C     DATA BLANK/1H /, STAR/1H*/, INDW/7/, EXPENT/0/                    
C/7S                                                                    
      DATA BLANK/' '/, STAR/'*'/, INDW/7/, EXPENT/0/                    
C/                                                                      
C                                                                       
C  IFMT1 IS FOR THE ASTERISK LINES, IFMT2 FOR THE DATA LINES            
C                                                                       
C/6S                                                                    
C     DATA IFMT1( 1) /1H(/,  IFMT2( 1) /1H(/                            
C     DATA IFMT1( 2) /1H1/,  IFMT2( 2) /1H1/                            
C     DATA IFMT1( 3) /1HA/,  IFMT2( 3) /1HA/                            
C     DATA IFMT1( 4) /1H1/,  IFMT2( 4) /1H1/                            
C     DATA IFMT1( 5) /1H,/,  IFMT2( 5) /1H,/                            
C     DATA IFMT1( 6) /1H5/,  IFMT2( 6) /1HI/                            
C     DATA IFMT1( 7) /1HX/,  IFMT2( 7) /1H7/                            
C     DATA IFMT1( 8) /1H,/,  IFMT2( 8) /1H,/                            
C     DATA IFMT1( 9) /1H2/,  IFMT2( 9) /1H1/                            
C     DATA IFMT1(10) /1HA/,  IFMT2(10) /1HP/                            
C     DATA IFMT1(11) /1H1/,  IFMT2(11) /1H /                            
C     DATA IFMT1(12) /1H,/,  IFMT2(12) /1HD/                            
C     DATA IFMT1(13) /1H /,  IFMT2(13) /1H /                            
C     DATA IFMT1(14) /1H /,  IFMT2(14) /1H /                            
C     DATA IFMT1(15) /1HX/,  IFMT2(15) /1H./                            
C     DATA IFMT1(16) /1H,/,  IFMT2(16) /1H /                            
C     DATA IFMT1(17) /1H2/,  IFMT2(17) /1H /                            
C     DATA IFMT1(18) /1HA/,  IFMT2(18) /1H)/                            
C     DATA IFMT1(19) /1H1/                                              
C     DATA IFMT1(20) /1H)/                                              
C/7S                                                                    
      DATA IFMT1( 1) /'('/,  IFMT2( 1) /'('/                            
      DATA IFMT1( 2) /'1'/,  IFMT2( 2) /'1'/                            
      DATA IFMT1( 3) /'A'/,  IFMT2( 3) /'A'/                            
      DATA IFMT1( 4) /'1'/,  IFMT2( 4) /'1'/                            
      DATA IFMT1( 5) /','/,  IFMT2( 5) /','/                            
      DATA IFMT1( 6) /'5'/,  IFMT2( 6) /'I'/                            
      DATA IFMT1( 7) /'X'/,  IFMT2( 7) /'7'/                            
      DATA IFMT1( 8) /','/,  IFMT2( 8) /','/                            
      DATA IFMT1( 9) /'2'/,  IFMT2( 9) /'1'/                            
      DATA IFMT1(10) /'A'/,  IFMT2(10) /'P'/                            
      DATA IFMT1(11) /'1'/,  IFMT2(11) /' '/                            
      DATA IFMT1(12) /','/,  IFMT2(12) /'D'/                            
      DATA IFMT1(13) /' '/,  IFMT2(13) /' '/                            
      DATA IFMT1(14) /' '/,  IFMT2(14) /' '/                            
      DATA IFMT1(15) /'X'/,  IFMT2(15) /'.'/                            
      DATA IFMT1(16) /','/,  IFMT2(16) /' '/                            
      DATA IFMT1(17) /'2'/,  IFMT2(17) /' '/                            
      DATA IFMT1(18) /'A'/,  IFMT2(18) /')'/                            
      DATA IFMT1(19) /'1'/                                              
      DATA IFMT1(20) /')'/                                              
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NITEMS .LE. 0) CALL                                           
C    1  SETERR(27H  APRNTD - NITEMS .LE. ZERO, 27, 1, 2)                
C/7S                                                                    
      IF (NITEMS .LE. 0) CALL                                           
     1  SETERR('  APRNTD - NITEMS .LE. ZERO', 27, 1, 2)                 
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (W .GT. MCOL) CALL                                             
C    1  SETERR(22H  APRNTD - W .GT. MCOL, 22, 2, 2)                     
C/7S                                                                    
      IF (W .GT. MCOL) CALL                                             
     1  SETERR('  APRNTD - W .GT. MCOL', 22, 2, 2)                      
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (D .LT. 0) CALL                                                
C    1  SETERR(22H  APRNTD - D .LT. ZERO, 22, 3, 2)                     
C/7S                                                                    
      IF (D .LT. 0) CALL                                                
     1  SETERR('  APRNTD - D .LT. ZERO', 22, 3, 2)                      
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (W .LT. D+6) CALL                                              
C    1  SETERR(21H  APRNTD - W .LT. D+6, 21, 4, 2)                      
C/7S                                                                    
      IF (W .LT. D+6) CALL                                              
     1  SETERR('  APRNTD - W .LT. D+6', 21, 4, 2)                       
C/                                                                      
C                                                                       
C     EXPENT IS USED AS A FIRST-TIME SWITCH TO SIGNAL IF THE            
C     MACHINE-VALUE CONSTANTS HAVE BEEN COMPUTED.                       
C                                                                       
      IF (EXPENT .GT. 0) GO TO 10                                       
         LOGETA = ALOG10(FLOAT(I1MACH(10)))                             
         EMIN = ICEIL(LOGETA*FLOAT(IABS(I1MACH(15)-1)))                 
         EMAX = ICEIL(LOGETA*FLOAT(I1MACH(16)))                         
         EXPENT = I10WID(MAX0(EMIN, EMAX))                              
C                                                                       
C     COMPUTE THE FORMATS.                                              
C                                                                       
   10 WW = MIN0(99, MAX0(W, 5+EXPENT))                                  
      CALL S88FMT(2, WW, IFMT2(13))                                     
      DD = MIN0(D, (WW-(5+EXPENT)))                                     
      CALL S88FMT(2, DD, IFMT2(16))                                     
C                                                                       
C  NCOL IS THE NUMBER OF VALUES TO BE PRINTED ACROSS THE LINE.          
C                                                                       
      NCOL = MAX0(1, MIN0(9, (MIN0(MCOL,160)-INDW)/WW))                 
      CALL S88FMT(1, NCOL, IFMT2(11))                                   
      WW = WW-2                                                         
C  THE ASTERISKS ARE POSITIONED RIGHT-ADJUSTED IN THE W-WIDTH SPACE.    
      CALL S88FMT(2, WW, IFMT1(13))                                     
C                                                                       
C  I COUNTS THE NUMBER OF ITEMS TO BE PRINTED,                          
C  J COUNTS THE NUMBER ON A GIVEN LINE,                                 
C  COUNT COUNTS THE NUMBER OF DUPLICATE LINES.                          
C                                                                       
      I = 1                                                             
      J = 0                                                             
      COUNT = 0                                                         
C                                                                       
C  THE LOGICAL OF THE FOLLOWING IS ROUGHLY THIS -                       
C  IF THERE ARE STILL MORE ITEMS TO BE PRINTED, A LINE-                 
C  FULL IS PUT INTO THE ARRAY, LINE.                                    
C  WHENEVER A LINE IS PRINTED OUT, IT IS ALSO STUFFED INTO              
C  THE ARRAY, LAST, TO COMPARE WITH THE NEXT ONE COMING IN              
C  TO CHECK FOR REPEAT OR DUPLICATED LINES.                             
C  ALSO WHENEVER A LINE IS WRITTEN OUT, THE DUPLICATION                 
C  COUNTER, COUNT, IS SET TO ONE.                                       
C  THE ONLY MILDLY TRICKY PART IS TO NOTE THAT COUNT HAS TO             
C  GO TO 3 BEFORE A LINE OF ASTERISKS IS PRINTED BECAUSE                
C  OF COURSE NO SUCH LINE IS PRINTED FOR JUST A PAIR OF                 
C  DUPLICATE LINES.                                                     
C                                                                       
C  ILINE IS PRINTED AS THE INDEX OF THE FIRST ARRAY ELEMENT             
C  IN A LINE.                                                           
C                                                                       
   20 IF (I .GT. NITEMS)  GO TO 90                                      
        J = J+1                                                         
        LINE(J) = A(I)                                                  
        IF (J .EQ. 1) ILINE = I                                         
        IF (J .LT. NCOL .AND. I .LT. NITEMS) GO TO 80                   
          IF (COUNT .EQ. 0) GO TO 50                                    
            DUP = .TRUE.                                                
            DO 30 K=1,NCOL                                              
   30         IF (LAST(K) .NE. LINE(K)) DUP = .FALSE.                   
            IF (I .EQ. NITEMS  .AND.  J .LT. NCOL) DUP = .FALSE.        
            IF (.NOT. DUP .AND. COUNT .EQ. 1) GO TO 50                  
              IF (.NOT. DUP) GO TO 40                                   
                COUNT = COUNT+1                                         
                IF (COUNT .EQ. 3) WRITE(IOUT, IFMT1C) BLANK,            
     1                                 STAR, STAR, STAR, STAR           
                IF (I .EQ. NITEMS)  GO TO 50                            
                  GO TO 70                                              
   40         WRITE(IOUT, IFMT2C) BLANK, ILAST, (LAST(K), K=1,NCOL)     
   50     WRITE(IOUT, IFMT2C) BLANK, ILINE, (LINE(K), K=1,J)            
          COUNT = 1                                                     
          DO 60 K=1,NCOL                                                
   60       LAST(K) = LINE(K)                                           
   70     ILAST = ILINE                                                 
          J = 0                                                         
   80   I = I+1                                                         
        GO TO 20                                                        
   90 RETURN                                                            
      END                                                               
      SUBROUTINE APRNTI(A, NITEMS, IOUT, MCOL, W)                       
C                                                                       
C  THIS SUBROUTINE PRINTS OUT NITEMS FROM THE INTEGER ARRAY, A, ON      
C  OUTPUT UNIT IOUT, USING A MAXIMUM OF MCOL PRINT SPACES.              
C  THE OUTPUT FORMAT IS IW.                                             
C  THE PROGRAM PUTS AS MANY VALUES ON A LINE AS POSSIBLE.               
C  W SHOULD BE INPUT AS THE ACTUAL WIDTH +1 FOR A SPACE BETWEEN VALUES. 
C                                                                       
C  DUPLICATE LINES ARE NOT ALL PRINTED, BUT ARE INDICATED BY ASTERISKS. 
C                                                                       
C  WRITTEN BY DAN WARNER, REVISED BY PHYL FOX, OCTOBER 21, 1982.        
C                                                                       
C  THE LINE WIDTH IS COMPUTED AS THE MINIMUM OF THE INPUT MCOL AND 160. 
C  IF THE LINE WIDTH IS TO BE INCREASED ABOVE 160, THE BUFFERS LINE()   
C  AND LAST(), WHICH THE VALUES TO BE PRINTED ON ONE LINE, MUST         
C  BE DIMENSIONED ACCORDINGLY.                                          
C                                                                       
C  INPUT PARAMETERS -                                                   
C                                                                       
C    A        - THE START OF THE INTEGER ARRAY TO BE PRINTED            
C                                                                       
C    NITEMS   - THE NUMBER OF ITEMS TO BE PRINTED                       
C                                                                       
C    IOUT     - THE OUTPUT UNIT FOR PRINTING                            
C                                                                       
C    MCOL     - THE NUMBER OF SPACES ACROSS THE LINE                    
C                                                                       
C    W        - THE WIDTH OF THE PRINTED VALUE (IW)                     
C                                                                       
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - NITEMS .LE. ZERO                                               
C                                                                       
C    2 - W .GT. MCOL                                                    
C                                                                       
      INTEGER  NITEMS, IOUT, MCOL, W                                    
      INTEGER  A(NITEMS)                                                
C                                                                       
      INTEGER  MAX0, MIN0, WW                                           
C/6S                                                                    
C     INTEGER  IFMT1(20), IFMT1C(20), IFMT2(14), IFMT2C(14), BLANK, STAR
C     EQUIVALENCE (IFMT1(1),IFMT1C(1)), (IFMT2(1),IFMT2C(1))            
C/7S                                                                    
      CHARACTER*1  IFMT1(20), IFMT2(14), BLANK, STAR                    
      CHARACTER*20 IFMT1C                                               
      CHARACTER*14 IFMT2C                                               
      EQUIVALENCE (IFMT1(1),IFMT1C), (IFMT2(1),IFMT2C)                  
C/                                                                      
      INTEGER  INDW, NCOL, COUNT, I, J, K, ILINE, ILAST                 
      LOGICAL  DUP                                                      
      INTEGER  LINE(40), LAST(40)                                       
C                                                                       
C/6S                                                                    
C     DATA BLANK/1H /, STAR/1H*/, INDW/7/                               
C/7S                                                                    
      DATA BLANK/' '/, STAR/'*'/, INDW/7/                               
C/                                                                      
C                                                                       
C  IFMT1 IS FOR THE ASTERISK LINES, IFMT2 FOR THE DATA LINES            
C                                                                       
C/6S                                                                    
C     DATA IFMT1( 1) /1H(/,  IFMT2( 1) /1H(/                            
C     DATA IFMT1( 2) /1H1/,  IFMT2( 2) /1H1/                            
C     DATA IFMT1( 3) /1HA/,  IFMT2( 3) /1HA/                            
C     DATA IFMT1( 4) /1H1/,  IFMT2( 4) /1H1/                            
C     DATA IFMT1( 5) /1H,/,  IFMT2( 5) /1H,/                            
C     DATA IFMT1( 6) /1H5/,  IFMT2( 6) /1HI/                            
C     DATA IFMT1( 7) /1HX/,  IFMT2( 7) /1H7/                            
C     DATA IFMT1( 8) /1H,/,  IFMT2( 8) /1H,/                            
C     DATA IFMT1( 9) /1H2/,  IFMT2( 9) /1H /                            
C     DATA IFMT1(10) /1HA/,  IFMT2(10) /1H /                            
C     DATA IFMT1(11) /1H1/,  IFMT2(11) /1HI/                            
C     DATA IFMT1(12) /1H,/,  IFMT2(12) /1H /                            
C     DATA IFMT1(13) /1H /,  IFMT2(13) /1H /                            
C     DATA IFMT1(14) /1H /,  IFMT2(14) /1H)/                            
C     DATA IFMT1(15) /1HX/                                              
C     DATA IFMT1(16) /1H,/                                              
C     DATA IFMT1(17) /1H2/                                              
C     DATA IFMT1(18) /1HA/                                              
C     DATA IFMT1(19) /1H1/                                              
C     DATA IFMT1(20) /1H)/                                              
C/7S                                                                    
      DATA IFMT1( 1) /'('/,  IFMT2( 1) /'('/                            
      DATA IFMT1( 2) /'1'/,  IFMT2( 2) /'1'/                            
      DATA IFMT1( 3) /'A'/,  IFMT2( 3) /'A'/                            
      DATA IFMT1( 4) /'1'/,  IFMT2( 4) /'1'/                            
      DATA IFMT1( 5) /','/,  IFMT2( 5) /','/                            
      DATA IFMT1( 6) /'5'/,  IFMT2( 6) /'I'/                            
      DATA IFMT1( 7) /'X'/,  IFMT2( 7) /'7'/                            
      DATA IFMT1( 8) /','/,  IFMT2( 8) /','/                            
      DATA IFMT1( 9) /'2'/,  IFMT2( 9) /' '/                            
      DATA IFMT1(10) /'A'/,  IFMT2(10) /' '/                            
      DATA IFMT1(11) /'1'/,  IFMT2(11) /'I'/                            
      DATA IFMT1(12) /','/,  IFMT2(12) /' '/                            
      DATA IFMT1(13) /' '/,  IFMT2(13) /' '/                            
      DATA IFMT1(14) /' '/,  IFMT2(14) /')'/                            
      DATA IFMT1(15) /'X'/                                              
      DATA IFMT1(16) /','/                                              
      DATA IFMT1(17) /'2'/                                              
      DATA IFMT1(18) /'A'/                                              
      DATA IFMT1(19) /'1'/                                              
      DATA IFMT1(20) /')'/                                              
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NITEMS .LE. 0) CALL                                           
C    1  SETERR(27H  APRNTI - NITEMS .LE. ZERO, 27, 1, 2)                
C/7S                                                                    
      IF (NITEMS .LE. 0) CALL                                           
     1  SETERR('  APRNTI - NITEMS .LE. ZERO', 27, 1, 2)                 
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (W .GT. MCOL) CALL                                             
C    1  SETERR(22H  APRNTI - W .GT. MCOL, 22, 2, 2)                     
C/7S                                                                    
      IF (W .GT. MCOL) CALL                                             
     1  SETERR('  APRNTI - W .GT. MCOL', 22, 2, 2)                      
C/                                                                      
C                                                                       
C     COMPUTE THE FORMATS.                                              
C                                                                       
        WW = MIN0(99, MAX0(W, 2))                                       
        CALL S88FMT(2, WW, IFMT2(12))                                   
        NCOL = MAX0(1, MIN0(99, (MIN0(MCOL,160) - INDW)/WW))            
        CALL S88FMT(2, NCOL, IFMT2(9))                                  
        WW = WW-2                                                       
        CALL S88FMT(2, WW, IFMT1(13))                                   
C                                                                       
C  THE ASTERISKS ARE POSITIONED RIGHT-ADJUSTED IN THE W-WIDTH SPACE.    
      CALL S88FMT(2, WW, IFMT1(13))                                     
C                                                                       
C  I COUNTS THE NUMBER OF ITEMS TO BE PRINTED,                          
C  J COUNTS THE NUMBER ON A GIVEN LINE,                                 
C  COUNT COUNTS THE NUMBER OF DUPLICATE LINES.                          
C                                                                       
  10  I = 1                                                             
      J = 0                                                             
      COUNT = 0                                                         
C                                                                       
C  THE LOGICAL OF THE FOLLOWING IS ROUGHLY THIS -                       
C  IF THERE ARE STILL MORE ITEMS TO BE PRINTED, A LINE-                 
C  FULL IS PUT INTO THE ARRAY, LINE.                                    
C  WHENEVER A LINE IS PRINTED OUT, IT IS ALSO STUFFED INTO              
C  THE ARRAY, LAST, TO COMPARE WITH THE NEXT ONE COMING IN              
C  TO CHECK FOR REPEAT OR DUPLICATED LINES.                             
C  ALSO WHENEVER A LINE IS WRITTEN OUT, THE DUPLICATION                 
C  COUNTER, COUNT, IS SET TO ONE.                                       
C  THE ONLY MILDLY TRICKY PART IS TO NOTE THAT COUNT HAS TO             
C  GO TO 3 BEFORE A LINE OF ASTERISKS IS PRINTED BECAUSE                
C  OF COURSE NO SUCH LINE IS PRINTED FOR JUST A PAIR OF                 
C  DUPLICATE LINES.                                                     
C                                                                       
C  ILINE IS PRINTED AS THE INDEX OF THE FIRST ARRAY ELEMENT             
C  IN A LINE.                                                           
C                                                                       
   20 IF (I .GT. NITEMS)  GO TO 90                                      
        J = J+1                                                         
        LINE(J) = A(I)                                                  
        IF (J .EQ. 1) ILINE = I                                         
        IF (J .LT. NCOL .AND. I .LT. NITEMS) GO TO 80                   
          IF (COUNT .EQ. 0) GO TO 50                                    
            DUP = .TRUE.                                                
            DO 30 K=1,NCOL                                              
   30         IF (LAST(K) .NE. LINE(K)) DUP = .FALSE.                   
            IF (I .EQ. NITEMS  .AND.  J .LT. NCOL) DUP = .FALSE.        
            IF (.NOT. DUP .AND. COUNT .EQ. 1) GO TO 50                  
              IF (.NOT. DUP) GO TO 40                                   
                COUNT = COUNT+1                                         
                IF (COUNT .EQ. 3) WRITE(IOUT, IFMT1C) BLANK,            
     1                                 STAR, STAR, STAR, STAR           
                IF (I .EQ. NITEMS)  GO TO 50                            
                  GO TO 70                                              
   40         WRITE(IOUT, IFMT2C) BLANK, ILAST, (LAST(K), K=1,NCOL)     
   50     WRITE(IOUT, IFMT2C) BLANK, ILINE, (LINE(K), K=1,J)            
          COUNT = 1                                                     
          DO 60 K=1,NCOL                                                
   60       LAST(K) = LINE(K)                                           
   70     ILAST = ILINE                                                 
          J = 0                                                         
   80   I = I+1                                                         
        GO TO 20                                                        
   90 RETURN                                                            
      END                                                               
      SUBROUTINE APRNTL(A, NITEMS, IOUT, MCOL)                          
C                                                                       
C  THIS SUBROUTINE PRINTS OUT NITEMS FROM THE LOGICAL ARRAY, A, ON      
C  OUTPUT UNIT IOUT, USING A MAXIMUM OF MCOL PRINT SPACES.              
C  THE T OR F VALUES ARE PRINTED RIGHT-ADJUSTED IN A FIELD OF WIDTH 4.  
C                                                                       
C  DUPLICATE LINES ARE NOT ALL PRINTED, BUT ARE INDICATED BY ASTERISKS. 
C                                                                       
C  WRITTEN BY DAN WARNER, REVISED BY PHYL FOX, OCTOBER 21, 1982.        
C                                                                       
C  THE LINE WIDTH IS COMPUTED AS THE MINIMUM OF THE INPUT MCOL AND 160. 
C  IF THE LINE WIDTH IS TO BE INCREASED ABOVE 160, THE BUFFERS LINE()   
C  AND LAST(), WHICH THE VALUES TO BE PRINTED ON ONE LINE, MUST         
C  BE DIMENSIONED ACCORDINGLY.                                          
C                                                                       
C  INPUT PARAMETERS -                                                   
C                                                                       
C    A        - THE START OF THE LOGICAL ARRAY TO BE PRINTED            
C                                                                       
C    NITEMS   - THE NUMBER OF ITEMS TO BE PRINTED                       
C                                                                       
C    IOUT     - THE OUTPUT UNIT FOR PRINTING                            
C                                                                       
C    MCOL     - THE NUMBER OF SPACES ACROSS THE LINE                    
C                                                                       
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - NITEMS .LE. ZERO                                               
C                                                                       
      INTEGER  NITEMS, IOUT, MCOL                                       
      LOGICAL  A(NITEMS)                                                
C                                                                       
      INTEGER  MAX0, MIN0                                               
C/6S                                                                    
C     INTEGER  IFMT1(20), IFMT1C(20), IFMT2(19), IFMT2C(19)             
C     EQUIVALENCE (IFMT1(1),IFMT1C(1)), (IFMT2(1),IFMT2C(1))            
C     INTEGER  BLANK, FCHAR, STAR, TCHAR                                
C/7S                                                                    
      CHARACTER*1  IFMT1(20), IFMT2(19), BLANK, FCHAR, STAR, TCHAR      
      CHARACTER*20 IFMT1C                                               
      CHARACTER*19 IFMT2C                                               
      EQUIVALENCE (IFMT1(1),IFMT1C), (IFMT2(1),IFMT2C)                  
C/                                                                      
      INTEGER  INDW, NCOL, COUNT, I, J, K, ILINE, ILAST                 
      LOGICAL  DUP                                                      
C/6S                                                                    
C     INTEGER  LINE(40), LAST(40)                                       
C     DATA BLANK/1H /, STAR/1H*/, TCHAR/1HT/, FCHAR/1HF/, INDW/7/       
C/7S                                                                    
      CHARACTER*1  LINE(40), LAST(40)                                   
      DATA BLANK/' '/, STAR/'*'/, TCHAR/'T'/, FCHAR/'F'/, INDW/7/       
C/                                                                      
C                                                                       
C                                                                       
C  IFMT1 IS FOR THE ASTERISK LINES, IFMT2 FOR THE DATA LINES            
C                                                                       
C/6S                                                                    
C     DATA IFMT1( 1) /1H(/,  IFMT2( 1) /1H(/                            
C     DATA IFMT1( 2) /1H1/,  IFMT2( 2) /1H1/                            
C     DATA IFMT1( 3) /1HA/,  IFMT2( 3) /1HA/                            
C     DATA IFMT1( 4) /1H1/,  IFMT2( 4) /1H1/                            
C     DATA IFMT1( 5) /1H,/,  IFMT2( 5) /1H,/                            
C     DATA IFMT1( 6) /1H5/,  IFMT2( 6) /1HI/                            
C     DATA IFMT1( 7) /1HX/,  IFMT2( 7) /1H7/                            
C     DATA IFMT1( 8) /1H,/,  IFMT2( 8) /1H,/                            
C     DATA IFMT1( 9) /1H2/,  IFMT2( 9) /1H /                            
C     DATA IFMT1(10) /1HA/,  IFMT2(10) /1H /                            
C     DATA IFMT1(11) /1H1/,  IFMT2(11) /1H(/                            
C     DATA IFMT1(12) /1H,/,  IFMT2(12) /1H3/                            
C     DATA IFMT1(13) /1H /,  IFMT2(13) /1HX/                            
C     DATA IFMT1(14) /1H2/,  IFMT2(14) /1H,/                            
C     DATA IFMT1(15) /1HX/,  IFMT2(15) /1H1/                            
C     DATA IFMT1(16) /1H,/,  IFMT2(16) /1HA/                            
C     DATA IFMT1(17) /1H2/,  IFMT2(17) /1H1/                            
C     DATA IFMT1(18) /1HA/,  IFMT2(18) /1H)/                            
C     DATA IFMT1(19) /1H1/,  IFMT2(19) /1H)/                            
C     DATA IFMT1(20) /1H)/                                              
C/7S                                                                    
      DATA IFMT1( 1) /'('/,  IFMT2( 1) /'('/                            
      DATA IFMT1( 2) /'1'/,  IFMT2( 2) /'1'/                            
      DATA IFMT1( 3) /'A'/,  IFMT2( 3) /'A'/                            
      DATA IFMT1( 4) /'1'/,  IFMT2( 4) /'1'/                            
      DATA IFMT1( 5) /','/,  IFMT2( 5) /','/                            
      DATA IFMT1( 6) /'5'/,  IFMT2( 6) /'I'/                            
      DATA IFMT1( 7) /'X'/,  IFMT2( 7) /'7'/                            
      DATA IFMT1( 8) /','/,  IFMT2( 8) /','/                            
      DATA IFMT1( 9) /'2'/,  IFMT2( 9) /' '/                            
      DATA IFMT1(10) /'A'/,  IFMT2(10) /' '/                            
      DATA IFMT1(11) /'1'/,  IFMT2(11) /'('/                            
      DATA IFMT1(12) /','/,  IFMT2(12) /'3'/                            
      DATA IFMT1(13) /' '/,  IFMT2(13) /'X'/                            
      DATA IFMT1(14) /'2'/,  IFMT2(14) /','/                            
      DATA IFMT1(15) /'X'/,  IFMT2(15) /'1'/                            
      DATA IFMT1(16) /','/,  IFMT2(16) /'A'/                            
      DATA IFMT1(17) /'2'/,  IFMT2(17) /'1'/                            
      DATA IFMT1(18) /'A'/,  IFMT2(18) /')'/                            
      DATA IFMT1(19) /'1'/,  IFMT2(19) /')'/                            
      DATA IFMT1(20) /')'/                                              
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NITEMS .LE. 0) CALL                                           
C    1  SETERR(27H  APRNTL - NITEMS .LE. ZERO, 27, 1, 2)                
C/7S                                                                    
      IF (NITEMS .LE. 0) CALL                                           
     1  SETERR('  APRNTL - NITEMS .LE. ZERO', 27, 1, 2)                 
C/                                                                      
C                                                                       
C  COMPUTE THE NUMBER OF FIELDS OF 4 ACROSS A LINE.                     
C                                                                       
      NCOL = MAX0(1, MIN0(99, (MIN0(MCOL,160)-INDW)/4))                 
C                                                                       
C  THE ASTERISKS ARE POSITIONED RIGHT-ADJUSTED IN THE 4-CHARACTER SPACE.
      CALL S88FMT(2, NCOL, IFMT2(9))                                    
C                                                                       
C  I COUNTS THE NUMBER OF ITEMS TO BE PRINTED,                          
C  J COUNTS THE NUMBER ON A GIVEN LINE,                                 
C  COUNT COUNTS THE NUMBER OF DUPLICATE LINES.                          
C                                                                       
  10  I = 1                                                             
      J = 0                                                             
      COUNT = 0                                                         
C                                                                       
C  THE LOGICAL OF THE FOLLOWING IS ROUGHLY THIS -                       
C  IF THERE ARE STILL MORE ITEMS TO BE PRINTED, A LINE-                 
C  FULL IS PUT INTO THE ARRAY, LINE.                                    
C  WHENEVER A LINE IS PRINTED OUT, IT IS ALSO STUFFED INTO              
C  THE ARRAY, LAST, TO COMPARE WITH THE NEXT ONE COMING IN              
C  TO CHECK FOR REPEAT OR DUPLICATED LINES.                             
C  ALSO WHENEVER A LINE IS WRITTEN OUT, THE DUPLICATION                 
C  COUNTER, COUNT, IS SET TO ONE.                                       
C  THE ONLY MILDLY TRICKY PART IS TO NOTE THAT COUNT HAS TO             
C  GO TO 3 BEFORE A LINE OF ASTERISKS IS PRINTED BECAUSE                
C  OF COURSE NO SUCH LINE IS PRINTED FOR JUST A PAIR OF                 
C  DUPLICATE LINES.                                                     
C                                                                       
C  ILINE IS PRINTED AS THE INDEX OF THE FIRST ARRAY ELEMENT             
C  IN A LINE.                                                           
C                                                                       
   20 IF (I .GT. NITEMS)  GO TO 90                                      
        J = J+1                                                         
        LINE(J) = FCHAR                                                 
        IF ( A(I) )  LINE(J) = TCHAR                                    
        IF (J .EQ. 1) ILINE = I                                         
        IF (J .LT. NCOL .AND. I .LT. NITEMS) GO TO 80                   
          IF (COUNT .EQ. 0) GO TO 50                                    
            DUP = .TRUE.                                                
            DO 30 K=1,NCOL                                              
   30         IF (LAST(K) .NE. LINE(K)) DUP = .FALSE.                   
            IF (I .EQ. NITEMS  .AND.  J .LT. NCOL) DUP = .FALSE.        
            IF (.NOT. DUP .AND. COUNT .EQ. 1) GO TO 50                  
              IF (.NOT. DUP) GO TO 40                                   
                COUNT = COUNT+1                                         
                IF (COUNT .EQ. 3) WRITE(IOUT, IFMT1C) BLANK,            
     1                                 STAR, STAR, STAR, STAR           
                IF (I .EQ. NITEMS)  GO TO 50                            
                  GO TO 70                                              
   40         WRITE(IOUT, IFMT2C) BLANK, ILAST, (LAST(K), K=1,NCOL)     
   50     WRITE(IOUT, IFMT2C) BLANK, ILINE, (LINE(K), K=1,J)            
          COUNT = 1                                                     
          DO 60 K=1,NCOL                                                
   60       LAST(K) = LINE(K)                                           
   70     ILAST = ILINE                                                 
          J = 0                                                         
   80   I = I+1                                                         
        GO TO 20                                                        
   90 RETURN                                                            
      END                                                               
      SUBROUTINE APRNTR(A, NITEMS, IOUT, MCOL, W, D)                    
C                                                                       
C  THIS SUBROUTINE PRINTS OUT NITEMS FROM THE REAL ARRAY, A, ON         
C  OUTPUT UNIT IOUT, USING A MAXIMUM OF MCOL PRINT SPACES.              
C  THE OUTPUT FORMAT IS 1PEW.D.                                         
C  THE PROGRAM PUTS AS MANY VALUES ON A LINE AS POSSIBLE.               
C  W SHOULD BE INPUT AS THE ACTUAL WIDTH +1 FOR A SPACE BETWEEN VALUES. 
C                                                                       
C  DUPLICATE LINES ARE NOT ALL PRINTED, BUT ARE INDICATED BY ASTERISKS. 
C                                                                       
C  WRITTEN BY DAN WARNER, REVISED BY PHYL FOX, OCTOBER 21, 1982.        
C                                                                       
C  THE LINE WIDTH IS COMPUTED AS THE MINIMUM OF THE INPUT MCOL AND 160. 
C  IF THE LINE WIDTH IS TO BE INCREASED ABOVE 160, THE BUFFERS LINE()   
C  AND LAST(), WHICH THE VALUES TO BE PRINTED ON ONE LINE, MUST         
C  BE DIMENSIONED ACCORDINGLY.                                          
C                                                                       
C  INPUT PARAMETERS -                                                   
C                                                                       
C    A        - THE START OF THE REAL ARRAY TO BE PRINTED               
C                                                                       
C    NITEMS   - THE NUMBER OF ITEMS TO BE PRINTED                       
C                                                                       
C    IOUT     - THE OUTPUT UNIT FOR PRINTING                            
C                                                                       
C    MCOL     - THE NUMBER OF SPACES ACROSS THE LINE                    
C                                                                       
C    W        - THE WIDTH OF THE PRINTED VALUE (1PEW.D)                 
C                                                                       
C    D        - THE NUMBER OF DIGITS AFTER THE DECIMAL POINT (1PEW.D)   
C                                                                       
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - NITEMS .LE. ZERO                                               
C                                                                       
C    2 - W .GT. MCOL                                                    
C                                                                       
C    3 - D .LT. ZERO                                                    
C                                                                       
C    4 - W .LT. D+6                                                     
C                                                                       
      INTEGER  NITEMS, IOUT, MCOL, W, D                                 
      REAL     A(NITEMS)                                                
C                                                                       
      INTEGER  MAX0, MIN0, WW, DD, EMIN, EMAX,                          
     1         EXPENT, I1MACH, ICEIL, IABS, I10WID                      
C/6S                                                                    
C     INTEGER  IFMT1(20), IFMT1C(20), IFMT2(18), IFMT2C(18), BLANK, STAR
C     EQUIVALENCE (IFMT1(1),IFMT1C(1)), (IFMT2(1),IFMT2C(1))            
C/7S                                                                    
      CHARACTER*1  IFMT1(20), IFMT2(18), BLANK, STAR                    
      CHARACTER*20 IFMT1C                                               
      CHARACTER*18 IFMT2C                                               
      EQUIVALENCE (IFMT1(1),IFMT1C), (IFMT2(1),IFMT2C)                  
C/                                                                      
      INTEGER  INDW, NCOL, COUNT, I, J, K, ILINE, ILAST                 
      LOGICAL  DUP                                                      
      REAL     LINE(18), LAST(18), LOGETA                               
C                                                                       
C/6S                                                                    
C     DATA BLANK/1H /, STAR/1H*/, INDW/7/, EXPENT/0/                    
C/7S                                                                    
      DATA BLANK/' '/, STAR/'*'/, INDW/7/, EXPENT/0/                    
C/                                                                      
C                                                                       
C  IFMT1 IS FOR THE ASTERISK LINES, IFMT2 FOR THE DATA LINES            
C                                                                       
C/6S                                                                    
C     DATA IFMT1( 1) /1H(/,  IFMT2( 1) /1H(/                            
C     DATA IFMT1( 2) /1H1/,  IFMT2( 2) /1H1/                            
C     DATA IFMT1( 3) /1HA/,  IFMT2( 3) /1HA/                            
C     DATA IFMT1( 4) /1H1/,  IFMT2( 4) /1H1/                            
C     DATA IFMT1( 5) /1H,/,  IFMT2( 5) /1H,/                            
C     DATA IFMT1( 6) /1H5/,  IFMT2( 6) /1HI/                            
C     DATA IFMT1( 7) /1HX/,  IFMT2( 7) /1H7/                            
C     DATA IFMT1( 8) /1H,/,  IFMT2( 8) /1H,/                            
C     DATA IFMT1( 9) /1H2/,  IFMT2( 9) /1H1/                            
C     DATA IFMT1(10) /1HA/,  IFMT2(10) /1HP/                            
C     DATA IFMT1(11) /1H1/,  IFMT2(11) /1H /                            
C     DATA IFMT1(12) /1H,/,  IFMT2(12) /1HE/                            
C     DATA IFMT1(13) /1H /,  IFMT2(13) /1H /                            
C     DATA IFMT1(14) /1H /,  IFMT2(14) /1H /                            
C     DATA IFMT1(15) /1HX/,  IFMT2(15) /1H./                            
C     DATA IFMT1(16) /1H,/,  IFMT2(16) /1H /                            
C     DATA IFMT1(17) /1H2/,  IFMT2(17) /1H /                            
C     DATA IFMT1(18) /1HA/,  IFMT2(18) /1H)/                            
C     DATA IFMT1(19) /1H1/                                              
C     DATA IFMT1(20) /1H)/                                              
C/7S                                                                    
      DATA IFMT1( 1) /'('/,  IFMT2( 1) /'('/                            
      DATA IFMT1( 2) /'1'/,  IFMT2( 2) /'1'/                            
      DATA IFMT1( 3) /'A'/,  IFMT2( 3) /'A'/                            
      DATA IFMT1( 4) /'1'/,  IFMT2( 4) /'1'/                            
      DATA IFMT1( 5) /','/,  IFMT2( 5) /','/                            
      DATA IFMT1( 6) /'5'/,  IFMT2( 6) /'I'/                            
      DATA IFMT1( 7) /'X'/,  IFMT2( 7) /'7'/                            
      DATA IFMT1( 8) /','/,  IFMT2( 8) /','/                            
      DATA IFMT1( 9) /'2'/,  IFMT2( 9) /'1'/                            
      DATA IFMT1(10) /'A'/,  IFMT2(10) /'P'/                            
      DATA IFMT1(11) /'1'/,  IFMT2(11) /' '/                            
      DATA IFMT1(12) /','/,  IFMT2(12) /'E'/                            
      DATA IFMT1(13) /' '/,  IFMT2(13) /' '/                            
      DATA IFMT1(14) /' '/,  IFMT2(14) /' '/                            
      DATA IFMT1(15) /'X'/,  IFMT2(15) /'.'/                            
      DATA IFMT1(16) /','/,  IFMT2(16) /' '/                            
      DATA IFMT1(17) /'2'/,  IFMT2(17) /' '/                            
      DATA IFMT1(18) /'A'/,  IFMT2(18) /')'/                            
      DATA IFMT1(19) /'1'/                                              
      DATA IFMT1(20) /')'/                                              
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NITEMS .LE. 0) CALL                                           
C    1  SETERR(27H  APRNTR - NITEMS .LE. ZERO, 27, 1, 2)                
C/7S                                                                    
      IF (NITEMS .LE. 0) CALL                                           
     1  SETERR('  APRNTR - NITEMS .LE. ZERO', 27, 1, 2)                 
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (W .GT. MCOL) CALL                                             
C    1  SETERR(22H  APRNTR - W .GT. MCOL, 22, 2, 2)                     
C/7S                                                                    
      IF (W .GT. MCOL) CALL                                             
     1  SETERR('  APRNTR - W .GT. MCOL', 22, 2, 2)                      
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (D .LT. 0) CALL                                                
C    1  SETERR(22H  APRNTR - D .LT. ZERO, 22, 3, 2)                     
C/7S                                                                    
      IF (D .LT. 0) CALL                                                
     1  SETERR('  APRNTR - D .LT. ZERO', 22, 3, 2)                      
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (W .LT. D+6) CALL                                              
C    1  SETERR(21H  APRNTR - W .LT. D+6, 21, 4, 2)                      
C/7S                                                                    
      IF (W .LT. D+6) CALL                                              
     1  SETERR('  APRNTR - W .LT. D+6', 21, 4, 2)                       
C/                                                                      
C                                                                       
C                                                                       
C     EXPENT IS USED AS A FIRST-TIME SWITCH TO SIGNAL IF THE            
C     MACHINE-VALUE CONSTANTS HAVE BEEN COMPUTED.                       
C                                                                       
      IF (EXPENT .GT. 0) GO TO 10                                       
         LOGETA = ALOG10(FLOAT(I1MACH(10)))                             
         EMIN   = ICEIL(LOGETA*FLOAT(IABS(I1MACH(12)-1)))               
         EMAX   = ICEIL(LOGETA*FLOAT(I1MACH(13)))                       
         EXPENT = I10WID(MAX0(EMIN, EMAX))                              
C                                                                       
C     COMPUTE THE FORMATS.                                              
C                                                                       
   10 WW = MIN0(99, MAX0(W, 5+EXPENT))                                  
      CALL S88FMT(2, WW, IFMT2(13))                                     
      DD = MIN0(D, (WW-(5+EXPENT)))                                     
      CALL S88FMT(2, DD, IFMT2(16))                                     
C                                                                       
C  NCOL IS THE NUMBER OF VALUES TO BE PRINTED ACROSS THE LINE.          
C                                                                       
      NCOL = MAX0(1, MIN0(9, (MIN0(MCOL,160)-INDW)/WW))                 
      CALL S88FMT(1, NCOL, IFMT2(11))                                   
      WW = WW-2                                                         
C                                                                       
C  THE ASTERISKS ARE POSITIONED RIGHT-ADJUSTED IN THE W-WIDTH SPACE.    
      CALL S88FMT(2, WW, IFMT1(13))                                     
C                                                                       
C  I COUNTS THE NUMBER OF ITEMS TO BE PRINTED,                          
C  J COUNTS THE NUMBER ON A GIVEN LINE,                                 
C  COUNT COUNTS THE NUMBER OF DUPLICATE LINES.                          
C                                                                       
      I = 1                                                             
      J = 0                                                             
      COUNT = 0                                                         
C                                                                       
C  THE LOGICAL OF THE FOLLOWING IS ROUGHLY THIS -                       
C  IF THERE ARE STILL MORE ITEMS TO BE PRINTED, A LINE-                 
C  FULL IS PUT INTO THE ARRAY, LINE.                                    
C  WHENEVER A LINE IS PRINTED OUT, IT IS ALSO STUFFED INTO              
C  THE ARRAY, LAST, TO COMPARE WITH THE NEXT ONE COMING IN              
C  TO CHECK FOR REPEAT OR DUPLICATED LINES.                             
C  ALSO WHENEVER A LINE IS WRITTEN OUT, THE DUPLICATION                 
C  COUNTER, COUNT, IS SET TO ONE.                                       
C  THE ONLY MILDLY TRICKY PART IS TO NOTE THAT COUNT HAS TO             
C  GO TO 3 BEFORE A LINE OF ASTERISKS IS PRINTED BECAUSE                
C  OF COURSE NO SUCH LINE IS PRINTED FOR JUST A PAIR OF                 
C  DUPLICATE LINES.                                                     
C                                                                       
C  ILINE IS PRINTED AS THE INDEX OF THE FIRST ARRAY ELEMENT             
C  IN A LINE.                                                           
C                                                                       
   20 IF (I .GT. NITEMS)  GO TO 90                                      
        J = J+1                                                         
        LINE(J) = A(I)                                                  
        IF (J .EQ. 1) ILINE = I                                         
        IF (J .LT. NCOL .AND. I .LT. NITEMS) GO TO 80                   
          IF (COUNT .EQ. 0) GO TO 50                                    
            DUP = .TRUE.                                                
            DO 30 K=1,NCOL                                              
   30         IF (LAST(K) .NE. LINE(K)) DUP = .FALSE.                   
            IF (I .EQ. NITEMS  .AND.  J .LT. NCOL) DUP = .FALSE.        
            IF (.NOT. DUP .AND. COUNT .EQ. 1) GO TO 50                  
              IF (.NOT. DUP) GO TO 40                                   
                COUNT = COUNT+1                                         
                IF (COUNT .EQ. 3) WRITE(IOUT, IFMT1C) BLANK,            
     1                                 STAR, STAR, STAR, STAR           
                IF (I .EQ. NITEMS)  GO TO 50                            
                  GO TO 70                                              
   40         WRITE(IOUT, IFMT2C) BLANK, ILAST, (LAST(K), K=1,NCOL)     
   50     WRITE(IOUT, IFMT2C) BLANK, ILINE, (LINE(K), K=1,J)            
          COUNT = 1                                                     
          DO 60 K=1,NCOL                                                
   60       LAST(K) = LINE(K)                                           
   70     ILAST = ILINE                                                 
          J = 0                                                         
   80   I = I+1                                                         
        GO TO 20                                                        
   90 RETURN                                                            
      END                                                               
C****END OF ROUTINES NEEDED FOR PORT 3 UTILITIES CHAPTER****************
