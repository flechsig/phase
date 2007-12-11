      SUBROUTINE SPLNI(K,T,N,A,                                         
     1                  X,NX,                                           
     2                  FIX)                                            
C                                                                       
C  TO EVALUATE THE B-SPLINE INTEGRAL                                    
C                                                                       
C            FIX(IX) = INTEGRAL ( T(1) TO X(IX) ) F(ZETA) DZETA         
C                                                                       
C  FOR IX=1,...,NX, WHERE                                               
C                                                                       
C        F(X)=SUM(I=1,...,N-K)(A(I)*B(I)(X)).                           
C                                                                       
C  INPUT -                                                              
C                                                                       
C    K   - THE ORDER OF THE B-SPLINES TO BE USED.                       
C          2.LE.K IS ASSUMED.                                           
C    T   - THE B-SPLINE MESH.                                           
C    N   - THE NUMBER OF POINTS IN THE MESH T.                          
C    A   - THE B-SPLINE COEFFICIENTS, N-K OF THEM.                      
C    X   - POINTS OF EVALUATION FOR THE INTEGRAL OF THE B-SPLINE.       
C          X MUST BE MONOTONE INCREASING.                               
C    NX  - THE NUMBER OF POINTS IN X.                                   
C                                                                       
C  OUTPUT -                                                             
C                                                                       
C    FIX - THE VALUES OF OF THE INTEGRAL OF F.                          
C                                                                       
C  SCRATCH SPACE ALLOCATED - 4*K REAL WORDS.                            
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - K.LT.2.                                                        
C    2 - N.LE.K.                                                        
C    3 - NX.LT.1.                                                       
C    4 - T IS NOT MONOTONE INCREASING.                                  
C    5 - X IS NOT MONOTONE INCREASING.                                  
C                                                                       
      REAL T(N),A(1),X(NX),FIX(NX)                                      
C     REAL A(N-K)                                                       
C                                                                       
      REAL SUM,XX(1)                                                    
      LOGICAL A3PLNI                                                    
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL WS(1)                                                        
      EQUIVALENCE (DS(1),WS(1))                                         
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (K.LT.2)                                                       
C    1   CALL SETERR(15H SPLNI - K.LT.2,15,1,2)                         
C     IF (N.LE.K)                                                       
C    1   CALL SETERR(15H SPLNI - N.LE.K,15,2,2)                         
C     IF (NX.LT.1)                                                      
C    1   CALL SETERR(16H SPLNI - NX.LT.1,16,3,2)                        
C     IF (T(1).GE.T(N)) CALL SETERR                                     
C    1   (37H SPLNI - T IS NOT MONOTONE INCREASING,37,4,2)              
C/7S                                                                    
      IF (K.LT.2)                                                       
     1   CALL SETERR(' SPLNI - K.LT.2',15,1,2)                          
      IF (N.LE.K)                                                       
     1   CALL SETERR(' SPLNI - N.LE.K',15,2,2)                          
      IF (NX.LT.1)                                                      
     1   CALL SETERR(' SPLNI - NX.LT.1',16,3,2)                         
      IF (T(1).GE.T(N)) CALL SETERR                                     
     1   (' SPLNI - T IS NOT MONOTONE INCREASING',37,4,2)               
C/                                                                      
C                                                                       
C ... ALLOCATE SCRATCH SPACE.                                           
C                                                                       
      IBIX=ISTKGT(4*K,3)                                                
      ICOL=IBIX+K                                                       
      IDM=ICOL+K                                                        
      IDP=IDM+K                                                         
C                                                                       
      CALL SETR(NX,0.0E0,FIX)                                           
C                                                                       
      ILEFTP=0                                                          
      SUM=0                                                             
C                                                                       
      DO 50 IX=1,NX                                                     
         XX(1)=AMIN1(X(IX),T(N))                                        
C                                                                       
         IF (X(IX).LE.T(1)) GO TO 50                                    
C                                                                       
         IF (IX.EQ.1) GO TO 10                                          
C/6S                                                                    
C        IF (X(IX).LT.X(IX-1)) CALL SETERR                              
C    1     (37H SPLNI - X IS NOT MONOTONE INCREASING,37,5,2)            
C/7S                                                                    
         IF (X(IX).LT.X(IX-1)) CALL SETERR                              
     1     (' SPLNI - X IS NOT MONOTONE INCREASING',37,5,2)             
C/                                                                      
C                                                                       
 10      ILEFT=INTRVR(N,T,XX(1))                                        
C                                                                       
         I=MAX0(ILEFTP-K,0)                                             
C                                                                       
 20         I=I+1                                                       
            IF (I.GT.ILEFT-K) GO TO 30                                  
            IDX1=I+K                                                    
            SUM=SUM+A(I)*(T(IDX1)-T(I))/FLOAT(K)                        
C                                                                       
            GO TO 20                                                    
C                                                                       
C/6S                                                                    
C30      IF (.NOT.A3PLNI(K,T,N,                                         
C    1                   XX,1,ILEFT,                                    
C    2                   WS(IBIX),WS(ICOL),WS(IDM),WS(IDP)))            
C    3     CALL SETERR                                                  
C    4        (37H SPLNI - T IS NOT MONOTONE INCREASING,37,4,2)         
C/7S                                                                    
 30      IF (.NOT.A3PLNI(K,T,N,                                         
     1                   XX,1,ILEFT,                                    
     2                   WS(IBIX),WS(ICOL),WS(IDM),WS(IDP)))            
     3     CALL SETERR                                                  
     4        (' SPLNI - T IS NOT MONOTONE INCREASING',37,4,2)          
C/                                                                      
C                                                                       
         LLO=MAX0(1,K+1-ILEFT)                                          
         LHI=MIN0(K,N-ILEFT)                                            
         FIX(IX)=SUM                                                    
         DO 40 L=LLO,LHI                                                
            IDX1=ILEFT+L-K                                              
            IDX2=IBIX+L-1                                               
 40         FIX(IX)=FIX(IX)+A(IDX1)*WS(IDX2)                            
         ILEFTP=ILEFT                                                   
 50      CONTINUE                                                       
C                                                                       
      CALL ISTKRL(1)                                                    
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE DSPLNI(K,T,N,A,                                        
     1                  X,NX,                                           
     2                  FIX)                                            
C                                                                       
C  TO EVALUATE THE B-SPLINE INTEGRAL                                    
C                                                                       
C            FIX(IX) = INTEGRAL ( T(1) TO X(IX) ) F(ZETA) DZETA         
C                                                                       
C  FOR IX=1,...,NX, WHERE                                               
C                                                                       
C        F(X)=SUM(I=1,...,N-K)(A(I)*B(I)(X)).                           
C                                                                       
C  INPUT -                                                              
C                                                                       
C    K   - THE ORDER OF THE B-SPLINES TO BE USED.                       
C          2.LE.K IS ASSUMED.                                           
C    T   - THE B-SPLINE MESH.                                           
C    N   - THE NUMBER OF POINTS IN THE MESH T.                          
C    A   - THE B-SPLINE COEFFICIENTS, N-K OF THEM.                      
C    X   - POINTS OF EVALUATION FOR THE INTEGRAL OF THE B-SPLINE.       
C          X MUST BE MONOTONE INCREASING.                               
C    NX  - THE NUMBER OF POINTS IN X.                                   
C                                                                       
C  OUTPUT -                                                             
C                                                                       
C    FIX - THE VALUES OF OF THE INTEGRAL OF F.                          
C                                                                       
C  SCRATCH SPACE ALLOCATED - 4*K DOUBLE PRECISION WORDS.                
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - K.LT.2.                                                        
C    2 - N.LE.K.                                                        
C    3 - NX.LT.1.                                                       
C    4 - T IS NOT MONOTONE INCREASING.                                  
C    5 - X IS NOT MONOTONE INCREASING.                                  
C                                                                       
      DOUBLE PRECISION T(N),A(1),X(NX),FIX(NX)                          
C     DOUBLE PRECISION A(N-K)                                           
C                                                                       
      DOUBLE PRECISION SUM,XX(1)                                        
      LOGICAL D3PLNI                                                    
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      DOUBLE PRECISION WS(1)                                            
      EQUIVALENCE (DS(1),WS(1))                                         
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (K.LT.2)                                                       
C    1   CALL SETERR(15HDSPLNI - K.LT.2,15,1,2)                         
C     IF (N.LE.K)                                                       
C    1   CALL SETERR(15HDSPLNI - N.LE.K,15,2,2)                         
C     IF (NX.LT.1)                                                      
C    1   CALL SETERR(16HDSPLNI - NX.LT.1,16,3,2)                        
C     IF (T(1).GE.T(N)) CALL SETERR                                     
C    1   (37HDSPLNI - T IS NOT MONOTONE INCREASING,37,4,2)              
C/7S                                                                    
      IF (K.LT.2)                                                       
     1   CALL SETERR('DSPLNI - K.LT.2',15,1,2)                          
      IF (N.LE.K)                                                       
     1   CALL SETERR('DSPLNI - N.LE.K',15,2,2)                          
      IF (NX.LT.1)                                                      
     1   CALL SETERR('DSPLNI - NX.LT.1',16,3,2)                         
      IF (T(1).GE.T(N)) CALL SETERR                                     
     1   ('DSPLNI - T IS NOT MONOTONE INCREASING',37,4,2)               
C/                                                                      
C                                                                       
C ... ALLOCATE SCRATCH SPACE.                                           
C                                                                       
      IBIX=ISTKGT(4*K,4)                                                
      ICOL=IBIX+K                                                       
      IDM=ICOL+K                                                        
      IDP=IDM+K                                                         
C                                                                       
      CALL SETD(NX,0.0D0,FIX)                                           
C                                                                       
      ILEFTP=0                                                          
      SUM=0                                                             
C                                                                       
      DO 50 IX=1,NX                                                     
         XX(1)=DMIN1(X(IX),T(N))                                        
C                                                                       
         IF (X(IX).LE.T(1)) GO TO 50                                    
C                                                                       
         IF (IX.EQ.1) GO TO 10                                          
C/6S                                                                    
C        IF (X(IX).LT.X(IX-1)) CALL SETERR                              
C    1     (37HDSPLNI - X IS NOT MONOTONE INCREASING,37,5,2)            
C/7S                                                                    
         IF (X(IX).LT.X(IX-1)) CALL SETERR                              
     1     ('DSPLNI - X IS NOT MONOTONE INCREASING',37,5,2)             
C/                                                                      
C                                                                       
 10      ILEFT=INTRVD(N,T,XX(1))                                        
C                                                                       
         I=MAX0(ILEFTP-K,0)                                             
C                                                                       
 20         I=I+1                                                       
            IF (I.GT.ILEFT-K) GO TO 30                                  
            IDX1=I+K                                                    
            SUM=SUM+A(I)*(T(IDX1)-T(I))/FLOAT(K)                        
C                                                                       
            GO TO 20                                                    
C                                                                       
C/6S                                                                    
C30      IF (.NOT.D3PLNI(K,T,N,                                         
C    1                   XX,1,ILEFT,                                    
C    2                   WS(IBIX),WS(ICOL),WS(IDM),WS(IDP)))            
C    3     CALL SETERR                                                  
C    4        (37HDSPLNI - T IS NOT MONOTONE INCREASING,37,4,2)         
C/7S                                                                    
 30      IF (.NOT.D3PLNI(K,T,N,                                         
     1                   XX,1,ILEFT,                                    
     2                   WS(IBIX),WS(ICOL),WS(IDM),WS(IDP)))            
     3     CALL SETERR                                                  
     4        ('DSPLNI - T IS NOT MONOTONE INCREASING',37,4,2)          
C/                                                                      
C                                                                       
         LLO=MAX0(1,K+1-ILEFT)                                          
         LHI=MIN0(K,N-ILEFT)                                            
         FIX(IX)=SUM                                                    
         DO 40 L=LLO,LHI                                                
            IDX1=ILEFT+L-K                                              
            IDX2=IBIX+L-1                                               
 40         FIX(IX)=FIX(IX)+A(IDX1)*WS(IDX2)                            
         ILEFTP=ILEFT                                                   
 50      CONTINUE                                                       
C                                                                       
      CALL ISTKRL(1)                                                    
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE ODEQ(N, F, A, B, EPS, ANS)                             
C                                                                       
C  USING THE DIFFERENTIAL-EQUATION ROUTINE, ODES1,                      
C  TO INTEGRATE A SET OF INTEGRALS.                                     
C                                                                       
C  INPUT                                                                
C                                                                       
C    N      - THE NUMBER OF INTEGRALS                                   
C    F      - CALL F(X,ANS,N,FVAL) SHOULD RETURN FVAL(I)=F(X)(I),       
C             FOR I=1,...,N.                                            
C             F SHOULD BE DECLARED EXTERNAL IN THE SUBPROGRAM           
C             CALLING ODEQ.                                             
C    A      - THE LOWER LIMIT FOR THE INTEGRAL                          
C    B      - THE UPPER LIMIT                                           
C    EPS    - ERROR TOLERATED IN THE ANSWERS                            
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    ANS    - THE VECTOR OF VALUES FOR THE INTEGRALS                    
C                                                                       
C                                                                       
C  SCRATCH SPACE (USED IN ODES1)                                        
C                                                                       
C     THE SPACE USED, IN TERMS OF REAL LOCATIONS, IS                    
C                                                                       
C    133 + 12N + MAX(2N + S(F), 11N + 20)                               
C                                                                       
C     WHERE S(F) IS THE STORAGE USED IN THE USERS F PROGRAM.            
C                                                                       
C                                                                       
C  ERROR STATES                                                         
C                                                                       
C    1 - N.LT.1.                                                        
C    2 - EPS IS ZERO OR NEGATIVE                                        
C    3 - ODES1 ERROR 7, INTEGRATION TROUBLE (RECOVERABLE)               
C                                                                       
      REAL ANS(N),A,B,DT,EPS                                            
      REAL ERRPAR(2)                                                    
      EXTERNAL F, ODESQ, ODESH                                          
C                                                                       
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(16H ODEQ - N .LT. 1,16,1,2)             
C     IF (EPS .LE. 0.0E0)                                               
C    1    CALL SETERR(31H ODEQ - EPS IS ZERO OR NEGATIVE,31,2,2)        
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' ODEQ - N .LT. 1',16,1,2)              
      IF (EPS .LE. 0.0E0)                                               
     1    CALL SETERR(' ODEQ - EPS IS ZERO OR NEGATIVE',31,2,2)         
C/                                                                      
C                                                                       
C  SET THE INITIAL VALUES OF THE INTEGRALS TO ZERO                      
C                                                                       
      DO 10 K=1,N                                                       
 10     ANS(K) = 0.0E0                                                  
C                                                                       
C  IF A EQUALS B, RETURN                                                
C                                                                       
      IF(A .EQ. B) RETURN                                               
C                                                                       
C  PUT IN AN ESTIMATE FOR THE INITIAL DT                                
C                                                                       
      DT = SIGN( AMAX1( R1MACH(1),EPS*ABS(B-A)), B-A)                   
C                                                                       
      ERRPAR(2) = EPS                                                   
      CALL ENTSRC(IRSAVE, 1)                                            
C                                                                       
      CALL ODES1(F,ANS,N,A,B,DT,ODESQ,ERRPAR,ODESH,.FALSE.,.FALSE.)     
C                                                                       
      IF (NERROR(NERR) .NE. 7) GO TO 20                                 
      CALL ERROFF                                                       
C/6S                                                                    
C     CALL SETERR (39H ODEQ - INTEGRATION CANNOT BE PERFORMED,39,3,1)   
C/7S                                                                    
      CALL SETERR (' ODEQ - INTEGRATION CANNOT BE PERFORMED',39,3,1)    
C/                                                                      
C                                                                       
 20   CALL RETSRC(IRSAVE)                                               
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DODEQ(N, F, A, B, EPS, ANS)                            
C                                                                       
C  USING THE DIFFERENTIAL-EQUATION ROUTINE, DODES1,                     
C  TO INTEGRATE A SET OF INTEGRALS.                                     
C                                                                       
C  INPUT                                                                
C                                                                       
C    N      - THE NUMBER OF INTEGRALS                                   
C    F      - CALL F(X,ANS,N,FVAL) SHOULD RETURN FVAL(I)=F(X)(I),       
C             FOR I=1,...,N.                                            
C             F SHOULD BE DECLARED EXTERNAL IN THE SUBPROGRAM           
C             CALLING DODEQ.                                            
C    A      - THE LOWER LIMIT FOR THE INTEGRAL                          
C    B      - THE UPPER LIMIT                                           
C    EPS    - ERROR TOLERATED IN THE ANSWERS                            
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    ANS    - THE VECTOR OF VALUES FOR THE INTEGRALS                    
C                                                                       
C                                                                       
C  SCRATCH SPACE (USED IN DODES1)                                       
C                                                                       
C     LET Z REPRESENT THE RATIO OF THE SPACE USED                       
C     BY DOUBLE-PRECISION LOCATIONS TO THAT USED FOR REALS.             
C                                                                       
C     THEN THE SPACE USED, IN TERMS OF REAL LOCATIONS, IS               
C                                                                       
C    (32 + 12N)*Z + 101 + MAX(2NZ + S(F), 11N + 10Z + 10)               
C                                                                       
C     WHERE S(F) IS THE STORAGE USED IN THE USERS F PROGRAM.            
C                                                                       
C                                                                       
C  ERROR STATES                                                         
C                                                                       
C    1 - N.LT.1.                                                        
C    2 - EPS IS ZERO OR NEGATIVE                                        
C    3 - DODES1 ERROR 7,INTEGRATION TROUBLE (RECOVERABLE)               
C                                                                       
      DOUBLE PRECISION ANS(N),A,B,DT,EPS                                
      REAL ERRPAR(2)                                                    
      EXTERNAL F, DODESQ, DODESH                                        
C                                                                       
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(16HDODEQ - N .LT. 1,16,1,2)             
C     IF (EPS .LE. 0.0D0)                                               
C    1    CALL SETERR(31HDODEQ - EPS IS ZERO OR NEGATIVE,31,2,2)        
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DODEQ - N .LT. 1',16,1,2)              
      IF (EPS .LE. 0.0D0)                                               
     1    CALL SETERR('DODEQ - EPS IS ZERO OR NEGATIVE',31,2,2)         
C/                                                                      
C                                                                       
C  SET THE INITIAL VALUES OF THE INTEGRALS TO ZERO                      
C                                                                       
      DO 10 K=1,N                                                       
 10     ANS(K) = 0.0D0                                                  
C                                                                       
C  IF A EQUALS B, RETURN                                                
C                                                                       
      IF(A .EQ. B) RETURN                                               
C                                                                       
C  PUT IN AN ESTIMATE FOR THE INITIAL DT                                
C                                                                       
      DT = SIGN( AMAX1( R1MACH(1),ABS(SNGL(EPS*(B-A)))), SNGL(B-A))     
C                                                                       
      ERRPAR(2) = EPS                                                   
      CALL ENTSRC(IRSAVE, 1)                                            
C                                                                       
      CALL DODES1(F,ANS,N,A,B,DT,DODESQ,ERRPAR,DODESH,.FALSE.,.FALSE.)  
C                                                                       
      IF (NERROR(NERR) .NE. 7) GO TO 20                                 
      CALL ERROFF                                                       
C/6S                                                                    
C     CALL SETERR (40H DODEQ - INTEGRATION CANNOT BE PERFORMED,40,3,1)  
C/7S                                                                    
      CALL SETERR (' DODEQ - INTEGRATION CANNOT BE PERFORMED',40,3,1)   
C/                                                                      
C                                                                       
 20   CALL RETSRC(IRSAVE)                                               
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE GQ1(N, X, W)                                           
      INTEGER N                                                         
      REAL X(N), W(N)                                                   
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR                      
      REAL R(500)                                                       
      EQUIVALENCE (D(1), R(1))                                          
C J. L. BLUE, 15 DEC 77                                                 
C CALCULATE GAUSS QUADRATURE RULES ON (-1, 1),                          
C    WITH WEIGHT FUNCTION 1                                             
C USE GAUSQ = SACK-DONOVAN PROGRAM, WITH LEGENDRE POLYNOMIALS.          
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(16H  GQ1 - N .LT. 1, 16, 1, 2)          
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('  GQ1 - N .LT. 1', 16, 1, 2)           
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(8*N, 3)                                               
      IB = IA+2*N                                                       
      IC = IB+2*N                                                       
      IN = IC+2*N                                                       
      CALL G8XAB(N, 0.E0, 0.E0, X, W, R(IA), R(IB), R(IC), R(IN))       
      IF (NERROR(NERR) .EQ. 0) GOTO 10                                  
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(30H GQ1   - CANNOT OBTAIN X AND W, 30, 2, 1)       
C/7S                                                                    
         CALL SETERR(' GQ1   - CANNOT OBTAIN X AND W', 30, 2, 1)        
C/                                                                      
   10 CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE GQEX(N, X, W)                                          
      INTEGER N                                                         
      REAL X(N), W(N)                                                   
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR                      
      REAL R(500)                                                       
      EQUIVALENCE (D(1), R(1))                                          
C J. L. BLUE, 15 DEC 77                                                 
C CALCULATE GAUSS QUADRATURE RULES ON (0, INFINITY),                    
C    WITH WEIGHT FUNCTION EXP(-X)                                       
C USE GAUSQ = SACK-DONOVAN PROGRAM, LAGUERRE POLYNOMIALS.               
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(17H  GQEX - N .LT. 1, 17, 1, 2)         
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('  GQEX - N .LT. 1', 17, 1, 2)          
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(8*N, 3)                                               
      IB = IA+2*N                                                       
      IC = IB+2*N                                                       
      IN = IC+2*N                                                       
      CALL G8EXA(N, 0.E0, X, W, R(IA), R(IB), R(IC), R(IN))             
      IF (NERROR(NERR) .EQ. 0) GOTO 10                                  
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(30H GQEX  - CANNOT OBTAIN X AND W, 30, 2, 1)       
C/7S                                                                    
         CALL SETERR(' GQEX  - CANNOT OBTAIN X AND W', 30, 2, 1)        
C/                                                                      
   10 CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE GQEX2(N, X, W)                                         
      INTEGER N                                                         
      REAL X(N), W(N)                                                   
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR                      
      REAL R(500)                                                       
      EQUIVALENCE (D(1), R(1))                                          
C J. L. BLUE, 15 DEC 77                                                 
C CALCULATE GAUSS QUADRATURE RULES ON (-INFINITY, INFINITY),            
C    WITH WEIGHT FUNCTION EXP(-X**2)                                    
C USE GAUSQ = SACK-DONOVAN PROGRAM,                                     
C   WITH HERMITE POLYNOMIALS.                                           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(18H  GQEX2 - N .LT. 1, 18, 1, 2)        
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('  GQEX2 - N .LT. 1', 18, 1, 2)         
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(8*N, 3)                                               
      IB = IA+2*N                                                       
      IC = IB+2*N                                                       
      IN = IC+2*N                                                       
      CALL G8EX2(N, X, W, R(IA), R(IB), R(IC), R(IN))                   
      IF (NERROR(NERR) .EQ. 0) GOTO 10                                  
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(30H GQEX2 - CANNOT OBTAIN X AND W, 30, 2, 1)       
C/7S                                                                    
         CALL SETERR(' GQEX2 - CANNOT OBTAIN X AND W', 30, 2, 1)        
C/                                                                      
   10 CALL LEAVE                                                        
      CALL ASYM(N,X)                                                    
      CALL SYM(N,W)                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE G8EX2(NPTS, X, W, DA, DB, DC, DN)                      
      INTEGER NPTS                                                      
      REAL X(NPTS), W(NPTS), DA(1), DB(1), DC(1), DN(1)                 
      INTEGER JJ, J                                                     
C J. L. BLUE, 15 DEC 77                                                 
      JJ = 2*NPTS                                                       
      DO  1 J = 1, JJ                                                   
         DA(J) = 0.5E0                                                  
         DB(J) = 0.E0                                                   
         DC(J) = J-1                                                    
         DN(J) = 0.E0                                                   
   1     CONTINUE                                                       
C SQRT(PI)                                                              
      DN(1) = SQRT(2.E0*ATAN2(1.E0, 0.E0))                              
      CALL GAUSQ(NPTS, DA, DB, DC, DN, X, W)                            
      CALL ASYM(NPTS, X)                                                
      CALL SYM(NPTS, W)                                                 
      RETURN                                                            
      END                                                               
      SUBROUTINE GQEXA(N, ALPHA, X, W)                                  
      INTEGER N                                                         
      REAL ALPHA, X(N), W(N)                                            
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR                      
      REAL R(500)                                                       
      EQUIVALENCE (D(1), R(1))                                          
C J. L. BLUE, 15 DEC 77                                                 
C CALCULATE GAUSS QUADRATURE RULES ON (0, INFINITY),                    
C    WITH WEIGHT FUNCTION X**ALPHA * EXP(-X)                            
C USE GAUSQ = SACK-DONOVAN PROGRAM, GENERALIZED LAGUERRE POLYNOMIALS.   
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(18H  GQEXA - N .LT. 1, 18, 1, 2)        
C     IF (ALPHA .LE. (-1.E0)) CALL SETERR(23H  GQEXA - ALPHA .LE. -1,   
C    1   23, 2, 2)                                                      
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('  GQEXA - N .LT. 1', 18, 1, 2)         
      IF (ALPHA .LE. (-1.E0)) CALL SETERR('  GQEXA - ALPHA .LE. -1',    
     1   23, 2, 2)                                                      
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(8*N, 3)                                               
      IB = IA+2*N                                                       
      IC = IB+2*N                                                       
      IN = IC+2*N                                                       
      CALL G8EXA(N, ALPHA, X, W, R(IA), R(IB), R(IC), R(IN))            
      IF (NERROR(NERR) .EQ. 0) GOTO 10                                  
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(30H GQEXA - CANNOT OBTAIN X AND W, 30, 3, 1)       
C/7S                                                                    
         CALL SETERR(' GQEXA - CANNOT OBTAIN X AND W', 30, 3, 1)        
C/                                                                      
   10 CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE G8EXA(NPTS, A, X, W, DA, DB, DC, DN)                   
      INTEGER NPTS                                                      
      REAL A, X(NPTS), W(NPTS), DA(1), DB(1), DC(1)                     
      REAL DN(1)                                                        
      INTEGER JJ, J                                                     
      REAL GAMMA, XN                                                    
C J. L. BLUE, 15 DEC 77                                                 
      JJ = 2*NPTS                                                       
      DO  1 J = 1, JJ                                                   
         XN = J-1                                                       
         DA(J) = -J                                                     
         DB(J) = 2.E0*XN+A+1.E0                                         
         DC(J) = -(XN+A)                                                
         DN(J) = 0.E0                                                   
   1     CONTINUE                                                       
      DN(1) = GAMMA(A+1.E0)                                             
      CALL GAUSQ(NPTS, DA, DB, DC, DN, X, W)                            
      RETURN                                                            
      END                                                               
      SUBROUTINE GQLOG(N, X, W)                                         
      INTEGER N                                                         
      REAL X(N), W(N)                                                   
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR                      
      REAL R(500)                                                       
      EQUIVALENCE (D(1), R(1))                                          
C J. L. BLUE, 15 DEC 77                                                 
C CALCULATE GAUSS QUADRATURE RULES ON (0, 1),                           
C    WITH WEIGHT FUNCTION LOG (1/X)                                     
C USE GAUSQ = SACK-DONOVAN PROGRAM,                                     
C   WITH SHIFTED LEGENDRE POLYNOMIALS.                                  
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(18H  GQLOG - N .LT. 1, 18, 1, 2)        
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('  GQLOG - N .LT. 1', 18, 1, 2)         
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(8*N, 3)                                               
      IB = IA+2*N                                                       
      IC = IB+2*N                                                       
      IN = IC+2*N                                                       
      CALL G8LOG(N, X, W, R(IA), R(IB), R(IC), R(IN))                   
      IF (NERROR(NERR) .EQ. 0) GOTO 10                                  
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(30H GQLOG - CANNOT OBTAIN X AND W, 30, 2, 1)       
C/7S                                                                    
         CALL SETERR(' GQLOG - CANNOT OBTAIN X AND W', 30, 2, 1)        
C/                                                                      
   10 CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE G8LOG(NPTS, X, W, DA, DB, DC, DN)                      
      INTEGER NPTS                                                      
      REAL X(NPTS), W(NPTS), DA(1), DB(1), DC(1), DN(1)                 
      INTEGER JJ, J                                                     
      REAL XJ, X2                                                       
C J. L. BLUE, 15 DEC 77                                                 
      DA(1) = 0.5E0                                                     
      DB(1) = 0.5E0                                                     
      DC(1) = 0.E0                                                      
      DN(1) = 1.E0                                                      
      JJ = 2*NPTS                                                       
      DO  1 J = 2, JJ                                                   
         XJ = J                                                         
         X2 = 4*J-2                                                     
         DA(J) = XJ/X2                                                  
         DB(J) = 0.5E0                                                  
         DC(J) = (XJ-1.E0)/X2                                           
         DN(J) = SIGN(1.E0/(XJ*(XJ-1.E0)), -DN(J-1))                    
   1     CONTINUE                                                       
      CALL GAUSQ(NPTS, DA, DB, DC, DN, X, W)                            
      RETURN                                                            
      END                                                               
      SUBROUTINE GQXA(N, ALPHA, X, W)                                   
      INTEGER N                                                         
      REAL ALPHA, X(N), W(N)                                            
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR                      
      REAL R(500)                                                       
      EQUIVALENCE (D(1), R(1))                                          
C J. L. BLUE, 15 DEC 77                                                 
C CALCULATE GAUSS QUADRATURE RULES ON (0, 1),                           
C    WITH WEIGHT FUNCTION X**ALPHA                                      
C USE GAUSQ = SACK-DONOVAN PROGRAM,                                     
C   WITH SHIFTED JACOBI POLYNOMIALS.                                    
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(17H  GQXA - N .LT. 1, 17, 1, 2)         
C     IF (ALPHA .LE. (-1.E0)) CALL SETERR(22H  GQXA - ALPHA .LE. -1, 22,
C    1   2, 2)                                                          
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('  GQXA - N .LT. 1', 17, 1, 2)          
      IF (ALPHA .LE. (-1.E0)) CALL SETERR('  GQXA - ALPHA .LE. -1', 22, 
     1   2, 2)                                                          
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(8*N, 3)                                               
      IB = IA+2*N                                                       
      IC = IB+2*N                                                       
      IN = IC+2*N                                                       
      CALL G8XA(N, ALPHA, X, W, R(IA), R(IB), R(IC), R(IN))             
      IF (NERROR(NERR) .EQ. 0) GOTO 10                                  
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(30H GQXA  - CANNOT OBTAIN X AND W, 30, 3, 1)       
C/7S                                                                    
         CALL SETERR(' GQXA  - CANNOT OBTAIN X AND W', 30, 3, 1)        
C/                                                                      
   10 CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE G8XA(NPTS, A, X, W, DA, DB, DC, DN)                    
      INTEGER NPTS                                                      
      REAL A, X(NPTS), W(NPTS), DA(1), DB(1), DC(1)                     
      REAL DN(1)                                                        
      INTEGER JJ, J                                                     
      REAL XN, P, X2                                                    
C J. L. BLUE, 15 DEC 77                                                 
      P = A+1.E0                                                        
      DA(1) = 1.E0                                                      
      DB(1) = P/(P+1.E0)                                                
      DC(1) = 0.E0                                                      
      DN(1) = 1.E0/P                                                    
      JJ = 2*NPTS                                                       
      DO  1 J = 2, JJ                                                   
         XN = J-1                                                       
         X2 = 2*(J-1)                                                   
         DA(J) = 1.E0                                                   
         DB(J) = (X2*(XN+P)+P*(P-1.E0))/((X2+P+1.E0)*(X2+P-1.E0))       
         DC(J) = (XN*(XN+P-1.E0))**2/((X2+P-2.E0)*(X2+P-1.E0)**2*(X2+P))
         DN(J) = 0.E0                                                   
   1     CONTINUE                                                       
      CALL GAUSQ(NPTS, DA, DB, DC, DN, X, W)                            
      RETURN                                                            
      END                                                               
      SUBROUTINE GQXAB(N, ALPHA, BETA, X, W)                            
      INTEGER N                                                         
      REAL ALPHA, BETA, X(N), W(N)                                      
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR                      
      REAL R(500)                                                       
      EQUIVALENCE (D(1), R(1))                                          
C J. L. BLUE, 15 DEC 77                                                 
C CALCULATE GAUSS QUADRATURE RULES ON (-1, 1),                          
C    WITH WEIGHT FUNCTION (1-X)**ALPHA * (1+X)**BETA                    
C USE GAUSQ = SACK-DONOVAN PROGRAM, WITH JACOBI POLYNOMIALS.            
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(18H  GQXAB - N .LT. 1, 18, 1, 2)        
C     IF (AMIN1(ALPHA, BETA) .LE. (-1.E0)) CALL SETERR(                 
C    1   31H  GQXAB - ALPHA OR BETA .LE. -1, 31, 2, 2)                  
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('  GQXAB - N .LT. 1', 18, 1, 2)         
      IF (AMIN1(ALPHA, BETA) .LE. (-1.E0)) CALL SETERR(                 
     1   '  GQXAB - ALPHA OR BETA .LE. -1', 31, 2, 2)                   
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(8*N, 3)                                               
      IB = IA+2*N                                                       
      IC = IB+2*N                                                       
      IN = IC+2*N                                                       
      CALL G8XAB(N, ALPHA, BETA, X, W, R(IA), R(IB), R(IC), R(IN))      
      IF (NERROR(NERR) .EQ. 0) GOTO 10                                  
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(30H GQXAB - CANNOT OBTAIN X AND W, 30, 3, 1)       
C/7S                                                                    
         CALL SETERR(' GQXAB - CANNOT OBTAIN X AND W', 30, 3, 1)        
C/                                                                      
   10 CALL LEAVE                                                        
      IF (ALPHA .NE. BETA) RETURN                                       
         CALL ASYM(N,X)                                                 
         CALL SYM(N,W)                                                  
         RETURN                                                         
      END                                                               
      SUBROUTINE G8XAB(NPTS, A, B, X, W, DA, DB, DC, DN)                
      INTEGER NPTS                                                      
      REAL A, B, X(NPTS), W(NPTS), DA(1), DB(1)                         
      REAL DC(1), DN(1)                                                 
      INTEGER JJ, J                                                     
      REAL GAMMA, XN, TERM, C, X2                                       
C J. L. BLUE, 15 DEC 77                                                 
      C = A+B+1.E0                                                      
      DA(1) = 2.E0/(C+1.E0)                                             
      DB(1) = (B-A)/(C+1.E0)                                            
      DC(1) = 0.                                                        
      DN(1) = 2.E0**C*GAMMA(A+1.E0)*GAMMA(B+1.E0)/GAMMA(C+1.E0)         
      TERM = (B+A)*(B-A)                                                
      JJ = 2*NPTS                                                       
      DO  1 J = 2, JJ                                                   
         XN = J-1                                                       
         X2 = 2*(J-1)                                                   
         DA(J) = 2.*(XN+1.)*(XN+C)/((X2+C)*(X2+C+1.))                   
         DB(J) = TERM/((X2+C-1.)*(X2+1.*C))                             
         DC(J) = 2.*(XN+A)*(XN+B)/((X2+C-1.)*(X2+C))                    
         DN(J) = 0.                                                     
   1     CONTINUE                                                       
      CALL GAUSQ(NPTS, DA, DB, DC, DN, X, W)                            
      IF (A .NE. B) RETURN                                              
         CALL ASYM(NPTS, X)                                             
         CALL SYM(NPTS,W)                                               
         RETURN                                                         
      END                                                               
      SUBROUTINE DGQ1(N, X, W)                                          
      INTEGER N                                                         
      DOUBLE PRECISION X(N), W(N)                                       
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR                      
      DOUBLE PRECISION R(500)                                           
      EQUIVALENCE (D(1), R(1))                                          
C J. L. BLUE, 15 DEC 77                                                 
C CALCULATE GAUSS QUADRATURE RULES ON (-1, 1),                          
C    WITH WEIGHT FUNCTION 1                                             
C USE DGAUSQ = SACK-DONOVAN PROGRAM, WITH LEGENDRE POLYNOMIALS.         
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(16H DGQ1 - N .LT. 1, 16, 1, 2)          
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' DGQ1 - N .LT. 1', 16, 1, 2)           
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(8*N, 4)                                               
      IB = IA+2*N                                                       
      IC = IB+2*N                                                       
      IN = IC+2*N                                                       
      CALL DG8XAB(N, 0.D0, 0.D0, X, W, R(IA), R(IB), R(IC), R(IN))      
      IF (NERROR(NERR) .EQ. 0) GOTO 10                                  
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(30HDGQ1   - CANNOT OBTAIN X AND W, 30, 2, 1)       
C/7S                                                                    
         CALL SETERR('DGQ1   - CANNOT OBTAIN X AND W', 30, 2, 1)        
C/                                                                      
   10 CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DGQEX(N, X, W)                                         
      INTEGER N                                                         
      DOUBLE PRECISION X(N), W(N)                                       
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR                      
      DOUBLE PRECISION R(500)                                           
      EQUIVALENCE (D(1), R(1))                                          
C J. L. BLUE, 15 DEC 77                                                 
C CALCULATE GAUSS QUADRATURE RULES ON (0, INFINITY),                    
C    WITH WEIGHT FUNCTION EXP(-X)                                       
C USE DGAUSQ = SACK-DONOVAN PROGRAM, LAGUERRE POLYNOMIALS.              
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(17H DGQEX - N .LT. 1, 17, 1, 2)         
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' DGQEX - N .LT. 1', 17, 1, 2)          
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(8*N, 4)                                               
      IB = IA+2*N                                                       
      IC = IB+2*N                                                       
      IN = IC+2*N                                                       
      CALL DG8EXA(N, 0.D0, X, W, R(IA), R(IB), R(IC), R(IN))            
      IF (NERROR(NERR) .EQ. 0) GOTO 10                                  
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(30HDGQEX  - CANNOT OBTAIN X AND W, 30, 2, 1)       
C/7S                                                                    
         CALL SETERR('DGQEX  - CANNOT OBTAIN X AND W', 30, 2, 1)        
C/                                                                      
   10 CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DGQEX2(N, X, W)                                        
      INTEGER N                                                         
      DOUBLE PRECISION X(N), W(N)                                       
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR                      
      DOUBLE PRECISION R(500)                                           
      EQUIVALENCE (D(1), R(1))                                          
C J. L. BLUE, 15 DEC 77                                                 
C CALCULATE GAUSS QUADRATURE RULES ON (-INFINITY, INFINITY),            
C    WITH WEIGHT FUNCTION EXP(-X**2)                                    
C USE DGAUSQ = SACK-DONOVAN PROGRAM,                                    
C   WITH HERMITE POLYNOMIALS.                                           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(18H DGQEX2 - N .LT. 1, 18, 1, 2)        
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' DGQEX2 - N .LT. 1', 18, 1, 2)         
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(8*N, 4)                                               
      IB = IA+2*N                                                       
      IC = IB+2*N                                                       
      IN = IC+2*N                                                       
      CALL DG8EX2(N, X, W, R(IA), R(IB), R(IC), R(IN))                  
      IF (NERROR(NERR) .EQ. 0) GOTO 10                                  
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(30HDGQEX2 - CANNOT OBTAIN X AND W, 30, 2, 1)       
C/7S                                                                    
         CALL SETERR('DGQEX2 - CANNOT OBTAIN X AND W', 30, 2, 1)        
C/                                                                      
   10 CALL LEAVE                                                        
      CALL DASYM(N,X)                                                   
      CALL DSYM(N,W)                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE DG8EX2(NPTS, X, W, DA, DB, DC, DN)                     
      INTEGER NPTS                                                      
      DOUBLE PRECISION X(NPTS), W(NPTS), DA(1), DB(1), DC(1), DN(1)     
      DOUBLE PRECISION DSQRT, DATAN2                                    
      INTEGER JJ, J                                                     
C J. L. BLUE, 15 DEC 77                                                 
      JJ = 2*NPTS                                                       
      DO  1 J = 1, JJ                                                   
         DA(J) = 0.5D0                                                  
         DB(J) = 0.D0                                                   
         DC(J) = J-1                                                    
         DN(J) = 0.D0                                                   
   1     CONTINUE                                                       
C SQRT(PI)                                                              
      DN(1) = DSQRT(2.D0*DATAN2(1.D0, 0.D0))                            
      CALL DGAUSQ(NPTS, DA, DB, DC, DN, X, W)                           
      CALL DASYM(NPTS, X)                                               
      CALL DSYM(NPTS, W)                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE DGQEXA(N, ALPHA, X, W)                                 
      INTEGER N                                                         
      DOUBLE PRECISION ALPHA, X(N), W(N)                                
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR                      
      DOUBLE PRECISION R(500)                                           
      EQUIVALENCE (D(1), R(1))                                          
C J. L. BLUE, 15 DEC 77                                                 
C CALCULATE GAUSS QUADRATURE RULES ON (0, INFINITY),                    
C    WITH WEIGHT FUNCTION X**ALPHA * EXP(-X)                            
C USE DGAUSQ = SACK-DONOVAN PROGRAM, GENERALIZED LAGUERRE POLYNOMIALS.  
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(18H DGQEXA - N .LT. 1, 18, 1, 2)        
C     IF (ALPHA .LE. (-1.D0)) CALL SETERR(23H DGQEXA - ALPHA .LE. -1,   
C    1   23, 2, 2)                                                      
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' DGQEXA - N .LT. 1', 18, 1, 2)         
      IF (ALPHA .LE. (-1.D0)) CALL SETERR(' DGQEXA - ALPHA .LE. -1',    
     1   23, 2, 2)                                                      
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(8*N, 4)                                               
      IB = IA+2*N                                                       
      IC = IB+2*N                                                       
      IN = IC+2*N                                                       
      CALL DG8EXA(N, ALPHA, X, W, R(IA), R(IB), R(IC), R(IN))           
      IF (NERROR(NERR) .EQ. 0) GOTO 10                                  
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(30HDGQEXA - CANNOT OBTAIN X AND W, 30, 3, 1)       
C/7S                                                                    
         CALL SETERR('DGQEXA - CANNOT OBTAIN X AND W', 30, 3, 1)        
C/                                                                      
   10 CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DG8EXA(NPTS, A, X, W, DA, DB, DC, DN)                  
      INTEGER NPTS                                                      
      DOUBLE PRECISION A, X(NPTS), W(NPTS), DA(1), DB(1), DC(1)         
      DOUBLE PRECISION DN(1)                                            
      INTEGER JJ, J                                                     
      DOUBLE PRECISION DGAMMA, XN                                       
C J. L. BLUE, 15 DEC 77                                                 
      JJ = 2*NPTS                                                       
      DO  1 J = 1, JJ                                                   
         XN = J-1                                                       
         DA(J) = -J                                                     
         DB(J) = 2.D0*XN+A+1.D0                                         
         DC(J) = -(XN+A)                                                
         DN(J) = 0.D0                                                   
   1     CONTINUE                                                       
      DN(1) = DGAMMA(A+1.D0)                                            
      CALL DGAUSQ(NPTS, DA, DB, DC, DN, X, W)                           
      RETURN                                                            
      END                                                               
      SUBROUTINE DGQLOG(N, X, W)                                        
      INTEGER N                                                         
      DOUBLE PRECISION X(N), W(N)                                       
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR                      
      DOUBLE PRECISION R(500)                                           
      EQUIVALENCE (D(1), R(1))                                          
C J. L. BLUE, 15 DEC 77                                                 
C CALCULATE GAUSS QUADRATURE RULES ON (0, 1),                           
C    WITH WEIGHT FUNCTION LOG (1/X)                                     
C USE DGAUSQ = SACK-DONOVAN PROGRAM,                                    
C   WITH SHIFTED LEGENDRE POLYNOMIALS.                                  
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(18H DGQLOG - N .LT. 1, 18, 1, 2)        
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' DGQLOG - N .LT. 1', 18, 1, 2)         
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(8*N, 4)                                               
      IB = IA+2*N                                                       
      IC = IB+2*N                                                       
      IN = IC+2*N                                                       
      CALL DG8LOG(N, X, W, R(IA), R(IB), R(IC), R(IN))                  
      IF (NERROR(NERR) .EQ. 0) GOTO 10                                  
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(30HDGQLOG - CANNOT OBTAIN X AND W, 30, 2, 1)       
C/7S                                                                    
         CALL SETERR('DGQLOG - CANNOT OBTAIN X AND W', 30, 2, 1)        
C/                                                                      
   10 CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DG8LOG(NPTS, X, W, DA, DB, DC, DN)                     
      INTEGER NPTS                                                      
      DOUBLE PRECISION X(NPTS), W(NPTS), DA(1), DB(1), DC(1), DN(1)     
      INTEGER JJ, J                                                     
      DOUBLE PRECISION XJ, X2                                           
C J. L. BLUE, 15 DEC 77                                                 
      DA(1) = 0.5D0                                                     
      DB(1) = 0.5D0                                                     
      DC(1) = 0.D0                                                      
      DN(1) = 1.D0                                                      
      JJ = 2*NPTS                                                       
      DO  1 J = 2, JJ                                                   
         XJ = J                                                         
         X2 = 4*J-2                                                     
         DA(J) = XJ/X2                                                  
         DB(J) = 0.5D0                                                  
         DC(J) = (XJ-1.D0)/X2                                           
         DN(J) = DSIGN(1.D0/(XJ*(XJ-1.D0)), -DN(J-1))                   
   1     CONTINUE                                                       
      CALL DGAUSQ(NPTS, DA, DB, DC, DN, X, W)                           
      RETURN                                                            
      END                                                               
      SUBROUTINE DGQXA(N, ALPHA, X, W)                                  
      INTEGER N                                                         
      DOUBLE PRECISION ALPHA, X(N), W(N)                                
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR                      
      DOUBLE PRECISION R(500)                                           
      EQUIVALENCE (D(1), R(1))                                          
C J. L. BLUE, 15 DEC 77                                                 
C CALCULATE GAUSS QUADRATURE RULES ON (0, 1),                           
C    WITH WEIGHT FUNCTION X**ALPHA                                      
C USE DGAUSQ = SACK-DONOVAN PROGRAM,                                    
C   WITH SHIFTED JACOBI POLYNOMIALS.                                    
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(17H DGQXA - N .LT. 1, 17, 1, 2)         
C     IF (ALPHA .LE. (-1.D0)) CALL SETERR(22H DGQXA - ALPHA .LE. -1, 22,
C    1   2, 2)                                                          
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' DGQXA - N .LT. 1', 17, 1, 2)          
      IF (ALPHA .LE. (-1.D0)) CALL SETERR(' DGQXA - ALPHA .LE. -1', 22, 
     1   2, 2)                                                          
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(8*N, 4)                                               
      IB = IA+2*N                                                       
      IC = IB+2*N                                                       
      IN = IC+2*N                                                       
      CALL DG8XA(N, ALPHA, X, W, R(IA), R(IB), R(IC), R(IN))            
      IF (NERROR(NERR) .EQ. 0) GOTO 10                                  
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(30HDGQXA  - CANNOT OBTAIN X AND W, 30, 3, 1)       
C/7S                                                                    
         CALL SETERR('DGQXA  - CANNOT OBTAIN X AND W', 30, 3, 1)        
C/                                                                      
   10 CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DG8XA(NPTS, A, X, W, DA, DB, DC, DN)                   
      INTEGER NPTS                                                      
      DOUBLE PRECISION A, X(NPTS), W(NPTS), DA(1), DB(1), DC(1)         
      DOUBLE PRECISION DN(1)                                            
      INTEGER JJ, J                                                     
      DOUBLE PRECISION XN, P, X2                                        
C J. L. BLUE, 15 DEC 77                                                 
      P = A+1.D0                                                        
      DA(1) = 1.D0                                                      
      DB(1) = P/(P+1.D0)                                                
      DC(1) = 0.D0                                                      
      DN(1) = 1.D0/P                                                    
      JJ = 2*NPTS                                                       
      DO  1 J = 2, JJ                                                   
         XN = J-1                                                       
         X2 = 2*(J-1)                                                   
         DA(J) = 1.D0                                                   
         DB(J) = (X2*(XN+P)+P*(P-1.D0))/((X2+P+1.D0)*(X2+P-1.D0))       
         DC(J) = (XN*(XN+P-1.D0))**2/((X2+P-2.D0)*(X2+P-1.D0)**2*(X2+P))
         DN(J) = 0.D0                                                   
   1     CONTINUE                                                       
      CALL DGAUSQ(NPTS, DA, DB, DC, DN, X, W)                           
      RETURN                                                            
      END                                                               
      SUBROUTINE DGQXAB(N, ALPHA, BETA, X, W)                           
      INTEGER N                                                         
      DOUBLE PRECISION ALPHA, BETA, X(N), W(N)                          
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR                      
      DOUBLE PRECISION R(500)                                           
      EQUIVALENCE (D(1), R(1))                                          
C J. L. BLUE, 15 DEC 77                                                 
C CALCULATE GAUSS QUADRATURE RULES ON (-1, 1),                          
C    WITH WEIGHT FUNCTION (1-X)**ALPHA * (1+X)**BETA                    
C USE DGAUSQ = SACK-DONOVAN PROGRAM, WITH JACOBI POLYNOMIALS.           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(18H DGQXAB - N .LT. 1, 18, 1, 2)        
C     IF (DMIN1(ALPHA, BETA) .LE. (-1.D0)) CALL SETERR(                 
C    1   31H DGQXAB - ALPHA OR BETA .LE. -1, 31, 2, 2)                  
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' DGQXAB - N .LT. 1', 18, 1, 2)         
      IF (DMIN1(ALPHA, BETA) .LE. (-1.D0)) CALL SETERR(                 
     1   ' DGQXAB - ALPHA OR BETA .LE. -1', 31, 2, 2)                   
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(8*N, 4)                                               
      IB = IA+2*N                                                       
      IC = IB+2*N                                                       
      IN = IC+2*N                                                       
      CALL DG8XAB(N, ALPHA, BETA, X, W, R(IA), R(IB), R(IC), R(IN))     
      IF (NERROR(NERR) .EQ. 0) GOTO 10                                  
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(30HDGQXAB - CANNOT OBTAIN X AND W, 30, 3, 1)       
C/7S                                                                    
         CALL SETERR('DGQXAB - CANNOT OBTAIN X AND W', 30, 3, 1)        
C/                                                                      
   10 CALL LEAVE                                                        
      IF (ALPHA .NE. BETA) RETURN                                       
         CALL DASYM(N,X)                                                
         CALL DSYM(N,W)                                                 
         RETURN                                                         
      END                                                               
      SUBROUTINE DG8XAB(NPTS, A, B, X, W, DA, DB, DC, DN)               
      INTEGER NPTS                                                      
      DOUBLE PRECISION A, B, X(NPTS), W(NPTS), DA(1), DB(1)             
      DOUBLE PRECISION DC(1), DN(1)                                     
      INTEGER JJ, J                                                     
      DOUBLE PRECISION DGAMMA, XN, TERM, C, X2                          
C J. L. BLUE, 15 DEC 77                                                 
      C = A+B+1.D0                                                      
      DA(1) = 2.D0/(C+1.D0)                                             
      DB(1) = (B-A)/(C+1.D0)                                            
      DC(1) = 0.                                                        
      DN(1) = 2.D0**C*DGAMMA(A+1.D0)*DGAMMA(B+1.D0)/DGAMMA(C+1.D0)      
      TERM = (B+A)*(B-A)                                                
      JJ = 2*NPTS                                                       
      DO  1 J = 2, JJ                                                   
         XN = J-1                                                       
         X2 = 2*(J-1)                                                   
         DA(J) = 2.*(XN+1.)*(XN+C)/((X2+C)*(X2+C+1.))                   
         DB(J) = TERM/((X2+C-1.)*(X2+1.*C))                             
         DC(J) = 2.*(XN+A)*(XN+B)/((X2+C-1.)*(X2+C))                    
         DN(J) = 0.                                                     
   1     CONTINUE                                                       
      CALL DGAUSQ(NPTS, DA, DB, DC, DN, X, W)                           
      IF (A .NE. B) RETURN                                              
         CALL DASYM(NPTS, X)                                            
         CALL DSYM(NPTS,W)                                              
         RETURN                                                         
      END                                                               
      SUBROUTINE GQ0IN(N,X,W)                                           
C                                                                       
C  TO COMPUTE THE ABCISSAE (X) AND WEIGHTS (W) FOR AN N POINT           
C  GAUSS-LAGUERRE QUADRATURE RULE ON (0,+INFINITY).                     
C                                                                       
C  SCRATCH SPACE ALLOCATED - 17*N REAL WORDS.                           
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - N.LT.1.                                                        
C                                                                       
      REAL X(N),W(N)                                                    
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL WS(1)                                                        
      EQUIVALENCE (DS(1),WS(1))                                         
C                                                                       
C/6S                                                                    
C     IF (N.LT.1)                                                       
C    1   CALL SETERR(15H GQ0IN - N.LT.1,15,1,2)                         
C/7S                                                                    
      IF (N.LT.1)                                                       
     1   CALL SETERR(' GQ0IN - N.LT.1',15,1,2)                          
C/                                                                      
C                                                                       
      N2=2*N                                                            
C                                                                       
C ... ALLOCATE SCRATCH SPACE FOR A,B,C AND NU.                          
C                                                                       
      IA=ISTKGT(8*N,3)-1                                                
      IB=IA+N2                                                          
      IC=IB+N2                                                          
      INU=IC+N2                                                         
C                                                                       
C ... COMPUTE A,B,C AND NU FOR CALL TO GAUSQ.                           
C                                                                       
      DO 10 I=1,N2                                                      
         IDXA=IA+I                                                      
         IDXB=IB+I                                                      
         IDXC=IC+I                                                      
         IDXNU=INU+I                                                    
         WS(IDXA)=(FLOAT(I))                                            
         WS(IDXB)=(FLOAT(2*I-1))                                        
         WS(IDXC)=(FLOAT(I-1))                                          
 10      WS(IDXNU)=0.0E0                                                
      WS(INU+1)=1.0E0                                                   
C                                                                       
C ... GET X AND W.                                                      
C                                                                       
      CALL GAUSQ(N,WS(IA+1),WS(IB+1),WS(IC+1),WS(INU+1),X,W)            
C                                                                       
      CALL ISTKRL(1)                                                    
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE GQM11(N,X,W)                                           
C                                                                       
C  TO COMPUTE THE ABCISSAE (X) AND WEIGHTS (W) FOR AN N POINT           
C  GAUSS-LEGENDRE QUADRATURE RULE ON (-1,+1).                           
C                                                                       
C  SCRATCH SPACE ALLOCATED - 17*N REAL WORDS.                           
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - N.LT.1.                                                        
C                                                                       
      REAL X(N),W(N)                                                    
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL WS(1)                                                        
      EQUIVALENCE (DS(1),WS(1))                                         
C                                                                       
C/6S                                                                    
C     IF (N.LT.1)                                                       
C    1   CALL SETERR(15H GQM11 - N.LT.1,15,1,2)                         
C/7S                                                                    
      IF (N.LT.1)                                                       
     1   CALL SETERR(' GQM11 - N.LT.1',15,1,2)                          
C/                                                                      
C                                                                       
      N2=2*N                                                            
C                                                                       
C ... ALLOCATE SCRATCH SPACE FOR A,B,C AND NU.                          
C                                                                       
      IA=ISTKGT(8*N,3)-1                                                
      IB=IA+N2                                                          
      IC=IB+N2                                                          
      INU=IC+N2                                                         
C                                                                       
C ... COMPUTE A,B,C AND NU FOR CALL TO GAUSQ.                           
C                                                                       
      DO 10 I=1,N2                                                      
         IDXA=IA+I                                                      
         IDXB=IB+I                                                      
         IDXC=IC+I                                                      
         IDXNU=INU+I                                                    
         WS(IDXA)=(FLOAT(I))/(FLOAT(2*I-1))                             
         WS(IDXB)=0.0E0                                                 
         WS(IDXC)=(FLOAT(I-1))/(FLOAT(2*I-1))                           
 10      WS(IDXNU)=0.0E0                                                
      WS(INU+1)=2.0E0                                                   
C                                                                       
C ... GET X AND W.                                                      
C                                                                       
      CALL GAUSQ(N,WS(IA+1),WS(IB+1),WS(IC+1),WS(INU+1),X,W)            
C                                                                       
      CALL ISTKRL(1)                                                    
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE GAUSQ(N,A,B,C,NU,X,W)                                  
C                                                                       
C  J. L. BLUE,  03 JAN 78                                               
C                                                                       
C  LET THE RECURRENCE RELATION                                          
C                                                                       
C  X*P(X,L)=A(L)*P(X,L+1)+B(L)*P(X,L)+C(L)*P(X,L-1), L=1,...,2*N        
C                                                                       
C  DEFINE POLYNOMIALS OF DEGREE L-1, WITH P(X,0)=0 AND P(X,1)=1.        
C                                                                       
C  LET NU(L)=INTEGRAL(A TO B)(W(X)*P(X,L))DX, L=1,...,2*N,              
C                                                                       
C  WHERE W(X) IS SOME NON-NEGATIVE WEIGHT FUNCTION ON (A,B).            
C                                                                       
C  THIS ROUTINE THEN RETURNS (X(I),W(I)), I=1,...,N, SUCH THAT          
C  THE QUADRATURE RULE GIVEN BY                                         
C                                                                       
C      INTEGRAL(A TO B)(W(X)*F(X))DX = SUM(I=1,...,N)(W(I)*F(X(I)))     
C                                                                       
C  IS EXACT FOR ALL POLYNOMIALS OF DEGREE LESS THAN 2*N.                
C                                                                       
C  SCRATCH SPACE ALLOCATED - 9*N REAL WORDS.                            
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - N.LT.1.                                                        
C    2 - A(I)=0.                                                        
C    3 - CANNOT OBTAIN X AND W.                                         
C                                                                       
C  SACK AND DONOVAN, NUM. MATH. 18, 465-478(1972).                      
C                                                                       
      REAL A(1),B(1),C(1),NU(1),X(N),W(N)                               
C     REAL (A,B,C,NU)(2*N)                                              
C                                                                       
      REAL ABCMAX,MEPS,R1MACH                                           
      LOGICAL OK,A6AUS0                                                 
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL WS(1)                                                        
      EQUIVALENCE (DS(1),WS(1))                                         
C                                                                       
C ... MEPS IS THE ROUNDING ERROR LEVEL OF THE MACHINE.                  
C                                                                       
      MEPS=R1MACH(4)                                                    
C                                                                       
C/6S                                                                    
C     IF (N.LT.1) CALL SETERR(15H GAUSQ - N.LT.1,15,1,2)                
C/7S                                                                    
      IF (N.LT.1) CALL SETERR(' GAUSQ - N.LT.1',15,1,2)                 
C/                                                                      
C                                                                       
      NM1=N-1                                                           
      N2=2*N                                                            
C                                                                       
C ... SCALE TO PREVENT OVER OR UNDER FLOW.                              
C                                                                       
      ABCMAX=0.0E0                                                      
      DO 10 I=1,N2                                                      
C/6S                                                                    
C        IF (A(I).EQ.0.0E0) CALL SETERR(15H GAUSQ - A(I)=0,15,2,2)      
C/7S                                                                    
         IF (A(I).EQ.0.0E0) CALL SETERR(' GAUSQ - A(I)=0',15,2,2)       
C/                                                                      
 10      ABCMAX=AMAX1(ABCMAX,ABS(A(I)),ABS(B(I)),ABS(C(I)))             
      DO 20 I=1,N2                                                      
         A(I)=A(I)/ABCMAX                                               
         B(I)=B(I)/ABCMAX                                               
         C(I)=C(I)/ABCMAX                                               
 20      NU(I)=NU(I)/ABCMAX                                             
C                                                                       
C ... ALLOCATE SCRATCH SPACE.                                           
C                                                                       
      IAC=ISTKGT(9*N,3)-1                                               
      IB=IAC+N2                                                         
      IA2=IB+N                                                          
      IE=IA2+N                                                          
      IR1=IE+N                                                          
      IR2=IR1+N2                                                        
C                                                                       
      DO 30 L=2,N2                                                      
         IDXAC=IAC+L                                                    
 30      WS(IDXAC)=A(L-1)*C(L)                                          
C                                                                       
C ... COMPUTE ORTHOGONAL POLYNOMIALS WITH GIVEN WEIGHT                  
C ... AND THEN COMPUTE TRIDIAGONAL MATRIX                               
C                                                                       
      CALL A6AUSQ(A,B,WS(IAC+1),NU,WS(IB+1),WS(IA2+1),N,WS(IR1+1),      
     1            WS(IR2+1))                                            
C                                                                       
      OK=.FALSE.                                                        
      IF (1.GT.NM1) GO TO 50                                            
      DO 40 L=1,NM1                                                     
         IDXE=IE+L+1                                                    
         IDXA2=IA2+L                                                    
         IDXB=IB+L                                                      
         IF (WS(IDXA2).LT.0.0E0) GO TO 70                               
         WS(IDXE)=SQRT(WS(IDXA2))                                       
 40      X(L)=WS(IDXB)                                                  
 50   X(N)=WS(IA2)                                                      
C                                                                       
C ... SOLVE THE EIGENSYSTEM.                                            
C                                                                       
      OK=A6AUS0(N,MEPS,X,WS(IE+1),W)                                    
C                                                                       
      DO 60 L=1,N                                                       
         X(L)=X(L)*ABCMAX                                               
 60      W(L)=NU(1)*(W(L)**2)*ABCMAX                                    
C                                                                       
   70 CALL ISTKRL(1)                                                    
C                                                                       
C/6S                                                                    
C     IF (.NOT.OK)                                                      
C    1   CALL SETERR(30H GAUSQ - CANNOT OBTAIN X AND W,30,3,1)          
C/7S                                                                    
      IF (.NOT.OK)                                                      
     1   CALL SETERR(' GAUSQ - CANNOT OBTAIN X AND W',30,3,1)           
C/                                                                      
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      LOGICAL FUNCTION A6AUS0 (N,MEPS,D,E,Z)                            
      INTEGER N                                                         
      REAL MEPS,D(1),E(1),Z(1)                                          
C..PETER BUSINGERS QL METHOD FOR FINDING THE EIGEN VALUES               
C..AND VECTORS OF A TRIDIAGONAL MATRIX.                                 
      INTEGER I,J,K,L,M                                                 
      REAL B,C,F,G,H,P,R,S                                              
      A6AUS0=.FALSE.                                                    
      IF(2.GT.N)GOTO 11                                                 
      DO 10 I=2,N                                                       
         Z(I)=0.0E0                                                     
   10    E(I-1)=E(I)                                                    
   11 E(N)=0.E0                                                         
      Z(1)=1.0E0                                                        
      B=0.E0                                                            
      F=0.E0                                                            
      DO 85 L=1,N                                                       
         J=0                                                            
         H=MEPS*(ABS(D(L))+ABS(E(L)))                                   
         IF(B.LT.H)B=H                                                  
         DO 20 M=L,N                                                    
            IF(ABS(E(M)).LE.B)GOTO 30                                   
   20       CONTINUE                                                    
   30    IF(M.EQ.L)GOTO 85                                              
   31    IF(J.EQ.30)RETURN                                              
         J=J+1                                                          
         G=D(L)                                                         
         P=(D(L+1)-G)/(2.E0*E(L))                                       
         IF(ABS(P).LT.1.E0)R=SQRT(1.E0+P*P)                             
         IF(ABS(P).GE.1.E0)R=ABS(P)*SQRT(1.E0+1.E0/P/P)                 
         IF(P.LT.0.E0)D(L)=E(L)/(P-R)                                   
         IF(P.GE.0.E0)D(L)=E(L)/(P+R)                                   
         H=G-D(L)                                                       
         L1=L+1                                                         
         IF(L1.GT.N)GOTO 41                                             
         DO 40 I=L1,N                                                   
   40       D(I)=D(I)-H                                                 
   41    F=F+H                                                          
         P=D(M)                                                         
         C=1.E0                                                         
         S=0.E0                                                         
         M1=M-1                                                         
         IF(L.GT.M1)GOTO 81                                             
         DO 80 II=L,M1                                                  
            I=M1+L-II                                                   
            G=C*E(I)                                                    
            H=C*P                                                       
            IF(ABS(P).LT.ABS(E(I)))GOTO 50                              
            C=E(I)/P                                                    
            R=SQRT(C*C+1.E0)                                            
            E(I+1)=S*P*R                                                
            S=C/R                                                       
            C=1.E0/R                                                    
            GOTO 60                                                     
   50       C=P/E(I)                                                    
            R=SQRT(C*C+1.E0)                                            
            E(I+1)=S*E(I)*R                                             
            S=1.E0/R                                                    
            C=C/R                                                       
   60       P=C*D(I)-S*G                                                
            D(I+1)=H+S*(C*G+S*D(I))                                     
            H=Z(I+1)                                                    
            Z(I+1)=S*Z(I)+C*H                                           
            Z(I)=C*Z(I)-S*H                                             
   80       CONTINUE                                                    
   81    E(L)=S*P                                                       
         D(L)=C*P                                                       
         IF(ABS(E(L)).GT.B)GOTO 31                                      
   85    D(L)=D(L)+F                                                    
      DO 110 I=1,N                                                      
         K=I                                                            
         P=D(I)                                                         
         I1=I+1                                                         
         IF(I1.GT.N)GOTO 91                                             
         DO 90 J=I1,N                                                   
            IF(D(J).GE.P)GOTO 90                                        
            K=J                                                         
            P=D(J)                                                      
   90       CONTINUE                                                    
   91    IF(K.EQ.I)GOTO 110                                             
         D(K)=D(I)                                                      
         D(I)=P                                                         
         P=Z(I)                                                         
         Z(I)=Z(K)                                                      
         Z(K)=P                                                         
  110    CONTINUE                                                       
      A6AUS0=.TRUE.                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE A6AUSQ(A,B,AC,NU,BETA,ALPHA2,N,R1,R2)                  
      REAL A(1),B(1),AC(1),NU(1),BETA(1),ALPHA2(1),R1(2)                
      REAL R2(1),TERM,SIGMA                                             
      LOGICAL EVEN                                                      
      IMAX=N                                                            
      JMAX=IMAX+N                                                       
      TERM=1.0E0/NU(1)                                                  
      DO 10 J=1,JMAX                                                    
         R1(J)=TERM*NU(J)                                               
         R2(J)=0.0E0                                                    
 10      TERM=TERM*A(J)                                                 
      SIGMA=R1(2)+B(1)                                                  
      BETA(1)=SIGMA                                                     
      EVEN=.TRUE.                                                       
      IF (2.GT.IMAX) GO TO 80                                           
      DO 70 I=2,IMAX                                                    
         IP1=I+1                                                        
         IF (EVEN) GO TO 40                                             
         JMAX=JMAX-1                                                    
         EVEN=.NOT.EVEN                                                 
         IF (I.GT.JMAX) GO TO 25                                        
         DO 20 J=I,JMAX                                                 
 20         R1(J)=(B(J)-SIGMA)*R2(J)+R2(J+1)+AC(J)*R2(J-1)-R1(J)        
 25      TERM=R1(I)                                                     
         ALPHA2(I-1)=TERM                                               
         TERM=1.0E0/TERM                                                
         R1(I)=1.0E0                                                    
         IF (IP1.GT.JMAX) GO  TO 35                                     
         DO 30 J=IP1,JMAX                                               
 30         R1(J)=R1(J)*TERM                                            
 35      SIGMA=R1(IP1)-R2(I)+B(I)                                       
         GO TO 70                                                       
 40      JMAX=JMAX-1                                                    
         EVEN=.NOT.EVEN                                                 
         IF (I.GT.JMAX) GO TO 55                                        
         DO 50 J=I,JMAX                                                 
 50         R2(J)=(B(J)-SIGMA)*R1(J)+R1(J+1)+AC(J)*R1(J-1)-R2(J)        
 55      TERM=R2(I)                                                     
         ALPHA2(I-1)=TERM                                               
         TERM=1.0E0/TERM                                                
         R2(I)=1.0E0                                                    
         IF (IP1.GT.JMAX) GO TO 65                                      
         DO 60 J=IP1,JMAX                                               
 60         R2(J)=R2(J)*TERM                                            
 65      SIGMA=R2(IP1)-R1(I)+B(I)                                       
 70      BETA(I)=SIGMA                                                  
 80   ALPHA2(IMAX)=0.0E0                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE DGQ0IN(N,X,W)                                          
C                                                                       
C  TO COMPUTE THE ABCISSAE (X) AND WEIGHTS (W) FOR AN N POINT           
C  GAUSS-LAGUERRE QUADRATURE RULE ON (0,+INFINITY).                     
C                                                                       
C  SCRATCH SPACE ALLOCATED - 17*N DOUBLE PRECISION WORDS.               
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - N.LT.1.                                                        
C                                                                       
      DOUBLE PRECISION X(N),W(N)                                        
C                                                                       
      COMMON /CSTAK/S                                                   
      DOUBLE PRECISION S(500)                                           
C                                                                       
C/6S                                                                    
C     IF (N.LT.1)                                                       
C    1   CALL SETERR(15HDGQ0IN - N.LT.1,15,1,2)                         
C/7S                                                                    
      IF (N.LT.1)                                                       
     1   CALL SETERR('DGQ0IN - N.LT.1',15,1,2)                          
C/                                                                      
C                                                                       
      N2=2*N                                                            
C                                                                       
C ... ALLOCATE SCRATCH SPACE FOR A,B,C AND NU.                          
C                                                                       
      IA=ISTKGT(8*N,4)-1                                                
      IB=IA+N2                                                          
      IC=IB+N2                                                          
      INU=IC+N2                                                         
C                                                                       
C ... COMPUTE A,B,C AND NU FOR CALL TO DGAUSQ.                          
C                                                                       
      DO 10 I=1,N2                                                      
         IDXA=IA+I                                                      
         IDXB=IB+I                                                      
         IDXC=IC+I                                                      
         IDXNU=INU+I                                                    
         S(IDXA)=DBLE(FLOAT(I))                                         
         S(IDXB)=DBLE(FLOAT(2*I-1))                                     
         S(IDXC)=DBLE(FLOAT(I-1))                                       
 10      S(IDXNU)=0.0D0                                                 
      S(INU+1)=1.0D0                                                    
C                                                                       
C ... GET X AND W.                                                      
C                                                                       
      CALL DGAUSQ(N,S(IA+1),S(IB+1),S(IC+1),S(INU+1),X,W)               
C                                                                       
      CALL ISTKRL(1)                                                    
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE DGQM11(N,X,W)                                          
C                                                                       
C  TO COMPUTE THE ABCISSAE (X) AND WEIGHTS (W) FOR AN N POINT           
C  GAUSS-LEGENDRE QUADRATURE RULE ON (-1,+1).                           
C                                                                       
C  SCRATCH SPACE ALLOCATED - 17*N DOUBLE PRECISION WORDS.               
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - N.LT.1.                                                        
C                                                                       
      DOUBLE PRECISION X(N),W(N)                                        
C                                                                       
      COMMON /CSTAK/S                                                   
      DOUBLE PRECISION S(500)                                           
C                                                                       
C/6S                                                                    
C     IF (N.LT.1)                                                       
C    1   CALL SETERR(15HDGQM11 - N.LT.1,15,1,2)                         
C/7S                                                                    
      IF (N.LT.1)                                                       
     1   CALL SETERR('DGQM11 - N.LT.1',15,1,2)                          
C/                                                                      
C                                                                       
      N2=2*N                                                            
C                                                                       
C ... ALLOCATE SCRATCH SPACE FOR A,B,C AND NU.                          
C                                                                       
      IA=ISTKGT(8*N,4)-1                                                
      IB=IA+N2                                                          
      IC=IB+N2                                                          
      INU=IC+N2                                                         
C                                                                       
C ... COMPUTE A,B,C AND NU FOR CALL TO DGAUSQ.                          
C                                                                       
      DO 10 I=1,N2                                                      
         IDXA=IA+I                                                      
         IDXB=IB+I                                                      
         IDXC=IC+I                                                      
         IDXNU=INU+I                                                    
         S(IDXA)=DBLE(FLOAT(I))/DBLE(FLOAT(2*I-1))                      
         S(IDXB)=0.0D0                                                  
         S(IDXC)=DBLE(FLOAT(I-1))/DBLE(FLOAT(2*I-1))                    
 10      S(IDXNU)=0.0D0                                                 
      S(INU+1)=2.0D0                                                    
C                                                                       
C ... GET X AND W.                                                      
C                                                                       
      CALL DGAUSQ(N,S(IA+1),S(IB+1),S(IC+1),S(INU+1),X,W)               
C                                                                       
      CALL ISTKRL(1)                                                    
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE DGAUSQ(N,A,B,C,NU,X,W)                                 
C                                                                       
C  J. L. BLUE,  03 JAN 78                                               
C                                                                       
C  LET THE RECURRENCE RELATION                                          
C                                                                       
C  X*P(X,L)=A(L)*P(X,L+1)+B(L)*P(X,L)+C(L)*P(X,L-1), L=1,...,2*N        
C                                                                       
C  DEFINE POLYNOMIALS OF DEGREE L-1, WITH P(X,0)=0 AND P(X,1)=1.        
C                                                                       
C  LET NU(L)=INTEGRAL(A TO B)(W(X)*P(X,L))DX, L=1,...,2*N,              
C                                                                       
C  WHERE W(X) IS SOME NON-NEGATIVE WEIGHT FUNCTION ON (A,B).            
C                                                                       
C  THIS ROUTINE THEN RETURNS (X(I),W(I)), I=1,...,N, SUCH THAT          
C  THE QUADRATURE RULE GIVEN BY                                         
C                                                                       
C      INTEGRAL(A TO B)(W(X)*F(X))DX = SUM(I=1,...,N)(W(I)*F(X(I)))     
C                                                                       
C  IS EXACT FOR ALL POLYNOMIALS OF DEGREE LESS THAN 2*N.                
C                                                                       
C  SCRATCH SPACE ALLOCATED - 9*N DOUBLE PRECISION WORDS.                
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - N.LT.1.                                                        
C    2 - A(I)=0.                                                        
C    3 - CANNOT OBTAIN X AND W.                                         
C                                                                       
C  SACK AND DONOVAN, NUM. MATH. 18, 465-478(1972).                      
C                                                                       
      DOUBLE PRECISION A(1),B(1),C(1),NU(1),X(N),W(N)                   
C     DOUBLE PRECISION (A,B,C,NU)(2*N)                                  
C                                                                       
      COMMON /CSTAK/S                                                   
      DOUBLE PRECISION S(500)                                           
      DOUBLE PRECISION ABCMAX,MEPS                                      
      DOUBLE PRECISION DSQRT                                            
      LOGICAL OK,D6AUS0                                                 
C                                                                       
C ... MEPS IS THE ROUNDING ERROR LEVEL OF THE MACHINE.                  
C                                                                       
      MEPS=DBLE(FLOAT(I1MACH(10)))**(1-I1MACH(14))                      
C                                                                       
C/6S                                                                    
C     IF (N.LT.1) CALL SETERR(15HDGAUSQ - N.LT.1,15,1,2)                
C/7S                                                                    
      IF (N.LT.1) CALL SETERR('DGAUSQ - N.LT.1',15,1,2)                 
C/                                                                      
C                                                                       
      NM1=N-1                                                           
      N2=2*N                                                            
C                                                                       
C ... SCALE TO PREVENT OVER OR UNDER FLOW.                              
C                                                                       
      ABCMAX=0.0D0                                                      
      DO 10 I=1,N2                                                      
C/6S                                                                    
C        IF (A(I).EQ.0.0D0) CALL SETERR(15HDGAUSQ - A(I)=0,15,2,2)      
C/7S                                                                    
         IF (A(I).EQ.0.0D0) CALL SETERR('DGAUSQ - A(I)=0',15,2,2)       
C/                                                                      
 10      ABCMAX=DMAX1(ABCMAX,DABS(A(I)),DABS(B(I)),DABS(C(I)))          
      DO 20 I=1,N2                                                      
         A(I)=A(I)/ABCMAX                                               
         B(I)=B(I)/ABCMAX                                               
         C(I)=C(I)/ABCMAX                                               
 20      NU(I)=NU(I)/ABCMAX                                             
C                                                                       
C ... ALLOCATE SCRATCH SPACE.                                           
C                                                                       
      IAC=ISTKGT(9*N,4)-1                                               
      IB=IAC+N2                                                         
      IA2=IB+N                                                          
      IE=IA2+N                                                          
      IR1=IE+N                                                          
      IR2=IR1+N2                                                        
C                                                                       
      DO 30 L=2,N2                                                      
         IDXAC=IAC+L                                                    
 30      S(IDXAC)=A(L-1)*C(L)                                           
C                                                                       
C ... COMPUTE ORTHOGONAL POLYNOMIALS WITH GIVEN WEIGHT                  
C ... AND THEN COMPUTE TRIDIAGONAL MATRIX                               
C                                                                       
      CALL D6AUSQ(A,B,S(IAC+1),NU,S(IB+1),S(IA2+1),N,S(IR1+1),S(IR2+1)) 
C                                                                       
      OK=.FALSE.                                                        
      IF (1.GT.NM1) GO TO 50                                            
      DO 40 L=1,NM1                                                     
         IDXE=IE+L+1                                                    
         IDXA2=IA2+L                                                    
         IDXB=IB+L                                                      
         IF (S(IDXA2).LT.0.0D0) GO TO 70                                
         S(IDXE)=DSQRT(S(IDXA2))                                        
 40      X(L)=S(IDXB)                                                   
 50   X(N)=S(IA2)                                                       
C                                                                       
C ... SOLVE THE EIGENSYSTEM.                                            
C                                                                       
      OK=D6AUS0(N,MEPS,X,S(IE+1),W)                                     
C                                                                       
      DO 60 L=1,N                                                       
         X(L)=X(L)*ABCMAX                                               
 60      W(L)=NU(1)*(W(L)**2)*ABCMAX                                    
C                                                                       
   70 CALL ISTKRL(1)                                                    
C                                                                       
C/6S                                                                    
C     IF (.NOT.OK)                                                      
C    1   CALL SETERR(30HDGAUSQ - CANNOT OBTAIN X AND W,30,3,1)          
C/7S                                                                    
      IF (.NOT.OK)                                                      
     1   CALL SETERR('DGAUSQ - CANNOT OBTAIN X AND W',30,3,1)           
C/                                                                      
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      LOGICAL FUNCTION D6AUS0 (N,MEPS,D,E,Z)                            
      INTEGER N                                                         
      DOUBLE PRECISION MEPS,D(1),E(1),Z(1)                              
C..PETER BUSINGERS QL METHOD FOR FINDING THE EIGEN VALUES               
C..AND VECTORS OF A TRIDIAGONAL MATRIX.                                 
      INTEGER I,J,K,L,M                                                 
      DOUBLE PRECISION B,C,F,G,H,P,R,S                                  
      DOUBLE PRECISION DSQRT                                            
      D6AUS0=.FALSE.                                                    
      IF(2.GT.N)GOTO 11                                                 
      DO 10 I=2,N                                                       
         Z(I)=0.0D0                                                     
   10    E(I-1)=E(I)                                                    
   11 E(N)=0.D0                                                         
      Z(1)=1.0D0                                                        
      B=0.D0                                                            
      F=0.D0                                                            
      DO 85 L=1,N                                                       
         J=0                                                            
         H=MEPS*(DABS(D(L))+DABS(E(L)))                                 
         IF(B.LT.H)B=H                                                  
         DO 20 M=L,N                                                    
            IF(DABS(E(M)).LE.B)GOTO 30                                  
   20       CONTINUE                                                    
   30    IF(M.EQ.L)GOTO 85                                              
   31    IF(J.EQ.30)RETURN                                              
         J=J+1                                                          
         G=D(L)                                                         
         P=(D(L+1)-G)/(2.D0*E(L))                                       
         IF(DABS(P).LT.1.D0)R=DSQRT(1.D0+P*P)                           
         IF(DABS(P).GE.1.D0)R=DABS(P)*DSQRT(1.D0+1.D0/P/P)              
         IF(P.LT.0.D0)D(L)=E(L)/(P-R)                                   
         IF(P.GE.0.D0)D(L)=E(L)/(P+R)                                   
         H=G-D(L)                                                       
         L1=L+1                                                         
         IF(L1.GT.N)GOTO 41                                             
         DO 40 I=L1,N                                                   
   40       D(I)=D(I)-H                                                 
   41    F=F+H                                                          
         P=D(M)                                                         
         C=1.D0                                                         
         S=0.D0                                                         
         M1=M-1                                                         
         IF(L.GT.M1)GOTO 81                                             
         DO 80 II=L,M1                                                  
            I=M1+L-II                                                   
            G=C*E(I)                                                    
            H=C*P                                                       
            IF(DABS(P).LT.DABS(E(I)))GOTO 50                            
            C=E(I)/P                                                    
            R=DSQRT(C*C+1.D0)                                           
            E(I+1)=S*P*R                                                
            S=C/R                                                       
            C=1.D0/R                                                    
            GOTO 60                                                     
   50       C=P/E(I)                                                    
            R=DSQRT(C*C+1.D0)                                           
            E(I+1)=S*E(I)*R                                             
            S=1.D0/R                                                    
            C=C/R                                                       
   60       P=C*D(I)-S*G                                                
            D(I+1)=H+S*(C*G+S*D(I))                                     
            H=Z(I+1)                                                    
            Z(I+1)=S*Z(I)+C*H                                           
            Z(I)=C*Z(I)-S*H                                             
   80       CONTINUE                                                    
   81    E(L)=S*P                                                       
         D(L)=C*P                                                       
         IF(DABS(E(L)).GT.B)GOTO 31                                     
   85    D(L)=D(L)+F                                                    
      DO 110 I=1,N                                                      
         K=I                                                            
         P=D(I)                                                         
         I1=I+1                                                         
         IF(I1.GT.N)GOTO 91                                             
         DO 90 J=I1,N                                                   
            IF(D(J).GE.P)GOTO 90                                        
            K=J                                                         
            P=D(J)                                                      
   90       CONTINUE                                                    
   91    IF(K.EQ.I)GOTO 110                                             
         D(K)=D(I)                                                      
         D(I)=P                                                         
         P=Z(I)                                                         
         Z(I)=Z(K)                                                      
         Z(K)=P                                                         
  110    CONTINUE                                                       
      D6AUS0=.TRUE.                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE D6AUSQ(A,B,AC,NU,BETA,ALPHA2,N,R1,R2)                  
C                                                                       
C THIS IS A TRANSLATION OF THE ALGOL PROGRAM IN                         
C SACK AND DONOVAN, NUM. MATH. 18, 465-478(1972).                       
C                                                                       
      DOUBLE PRECISION A(1),B(1),AC(1),NU(1),BETA(1),ALPHA2(1),R1(2)    
      DOUBLE PRECISION R2(1),TERM,SIGMA                                 
      LOGICAL EVEN                                                      
      IMAX=N                                                            
      JMAX=IMAX+N                                                       
      TERM=1.0D0/NU(1)                                                  
      DO 10 J=1,JMAX                                                    
         R1(J)=TERM*NU(J)                                               
         R2(J)=0.0D0                                                    
 10      TERM=TERM*A(J)                                                 
      SIGMA=R1(2)+B(1)                                                  
      BETA(1)=SIGMA                                                     
      EVEN=.TRUE.                                                       
      IF (2.GT.IMAX) GO TO 80                                           
      DO 70 I=2,IMAX                                                    
         IP1=I+1                                                        
         IF (EVEN) GO TO 40                                             
         JMAX=JMAX-1                                                    
         EVEN=.NOT.EVEN                                                 
         IF (I.GT.JMAX) GO TO 25                                        
         DO 20 J=I,JMAX                                                 
 20         R1(J)=(B(J)-SIGMA)*R2(J)+R2(J+1)+AC(J)*R2(J-1)-R1(J)        
 25      TERM=R1(I)                                                     
         ALPHA2(I-1)=TERM                                               
         TERM=1.0D0/TERM                                                
         R1(I)=1.0D0                                                    
         IF (IP1.GT.JMAX) GO  TO 35                                     
         DO 30 J=IP1,JMAX                                               
 30         R1(J)=R1(J)*TERM                                            
 35      SIGMA=R1(IP1)-R2(I)+B(I)                                       
         GO TO 70                                                       
 40      JMAX=JMAX-1                                                    
         EVEN=.NOT.EVEN                                                 
         IF (I.GT.JMAX) GO TO 55                                        
         DO 50 J=I,JMAX                                                 
 50         R2(J)=(B(J)-SIGMA)*R1(J)+R1(J+1)+AC(J)*R1(J-1)-R2(J)        
 55      TERM=R2(I)                                                     
         ALPHA2(I-1)=TERM                                               
         TERM=1.0D0/TERM                                                
         R2(I)=1.0D0                                                    
         IF (IP1.GT.JMAX) GO TO 65                                      
         DO 60 J=IP1,JMAX                                               
 60         R2(J)=R2(J)*TERM                                            
 65      SIGMA=R2(IP1)-R1(I)+B(I)                                       
 70      BETA(I)=SIGMA                                                  
 80   ALPHA2(IMAX)=0.0D0                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE RQUAD(F,A,B,EPSABS,EPSREL,ANS,ERREST)                  
C                                                                       
C TO INTEGRATE F(X) FROM A TO B WITH ACCURACY EPS,                      
C WHERE EPS = EPSABS + EPSREL * ABS(ANS),                               
C SET DEFAULT VALUES OF PARAMETERS AND CALL R1QUAD.                     
C IF EPSREL .GT. 0, NEED TO ESTIMATE ANS FIRST.                         
C                                                                       
      REAL F,A,B,EPSABS,EPSREL,EPS,ANS,ERREST,HSAMPL                    
      EXTERNAL F                                                        
C                                                                       
      HSAMPL = 0.125                                                    
      NFCALL = 2000                                                     
      LYSTAK = 250                                                      
      KMAXEX = 6                                                        
      KDIVID = 4                                                        
      JPRINT = 0                                                        
      NUMINT = 3                                                        
C                                                                       
      EPSA = AMAX1(EPSABS,0.)                                           
      EPSR = AMAX1(EPSREL,0.)                                           
      EPS = EPSABS                                                      
      JMAX = 1                                                          
      IF (EPSR .GT. 0.) EPS = 0.05*R1MACH(2)                            
      IF (EPSR .GT. 0.) JMAX = 3                                        
C                                                                       
      DO 100 J=1,JMAX                                                   
         CALL R1QUAD(F,A,B,EPS,HSAMPL,NFCALL,LYSTAK,KMAXEX,KDIVID,      
     X         JPRINT,NUMINT,ANS,ERREST,KWARN)                          
C                                                                       
C/6S                                                                    
C     IF (KWARN .LT. 0 ) CALL SETERR                                    
C    *   (58HRQUAD - THE DYNAMIC STORAGE STACK IS FULL, USE PORT ISTKIN 
C    *    ,58,2,2)                                                      
C/7S                                                                    
      IF (KWARN .LT. 0 ) CALL SETERR                                    
     *   ('RQUAD - THE DYNAMIC STORAGE STACK IS FULL, USE PORT ISTKIN'  
     *    ,58,2,2)                                                      
C/                                                                      
C                                                                       
         IF (KWARN .GT. 0) GO TO 200                                    
         EPS = EPSA + EPSR * ABS(ANS)                                   
         IF (ERREST .LE. EPS) RETURN                                    
         EPS = EPSA + EPSR * AMAX1(0.5E0*ABS(ANS), ABS(ANS)-ERREST)     
C                                                                       
  100 CONTINUE                                                          
C                                                                       
C/6S                                                                    
C 200 CALL SETERR                                                       
C    *   (53HRQUAD - THE ERROR ESTIMATE,ERREST, IS LARGER THAN EPS,     
C    *    53,1,1)                                                       
C/7S                                                                    
  200 CALL SETERR                                                       
     *   ('RQUAD - THE ERROR ESTIMATE,ERREST, IS LARGER THAN EPS',      
     *    53,1,1)                                                       
C/                                                                      
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE BQUAD(F,N,X,EPS,ANS,ERREST)                            
C                                                                       
C TO INTEGRATE F(X) FROM X(1) TO X(N) WITH ABSOLUTE ACCURACY EPS,       
C SET DEFAULT VALUES OF PARAMETERS AND CALL R2QUAD.  ALL THE DISTINCT   
C POINTS AMONG X(2), ..., X(N-1) ARE USED AS INITIAL BREAKPOINTS.       
C THIS PROGRAM IS USEFUL FOR INTEGRATING FUNCTIONS WHICH HAVE           
C DISCONTINUITIES IN DERIVATIVES AT THE BREAKPOINTS, BUT NOT            
C DISCONTINUITIES IN VALUE.                                             
C                                                                       
      REAL F,X(N),EPS,ANS,ERREST,HSAMPL                                 
      LOGICAL MONOR                                                     
      EXTERNAL F                                                        
C                                                                       
      HSAMPL = 0.125                                                    
      NFCALL = 2000                                                     
      LYSTAK = 250                                                      
      KMAXEX = 8                                                        
      KDIVID = 4                                                        
      JPRINT = 0                                                        
C                                                                       
C/6S                                                                    
C     IF (N .LT. 2) CALL SETERR                                         
C    *   (29H BQUAD - N MUST BE AT LEAST 2, 29, 1, 2)                   
C     IF (.NOT.MONOR(X,N,1)) CALL SETERR                                
C    *   (37H BQUAD - THE X ARRAY MUST BE MONOTONE, 37, 2, 2)           
C/7S                                                                    
      IF (N .LT. 2) CALL SETERR                                         
     *   (' BQUAD - N MUST BE AT LEAST 2', 29, 1, 2)                    
      IF (.NOT.MONOR(X,N,1)) CALL SETERR                                
     *   (' BQUAD - THE X ARRAY MUST BE MONOTONE', 37, 2, 2)            
C/                                                                      
C                                                                       
      CALL R2QUAD(F,N,X,EPS,HSAMPL,NFCALL,LYSTAK,KMAXEX,KDIVID,         
     *      JPRINT,ANS,ERREST,KWARN)                                    
C                                                                       
C                                                                       
C/6S                                                                    
C     IF (ERREST  .GT.  EPS) CALL SETERR                                
C    *   (54H BQUAD - THE ERROR ESTIMATE,ERREST, IS LARGER THAN EPS,    
C    *    54, 3, 1)                                                     
C/7S                                                                    
      IF (ERREST  .GT.  EPS) CALL SETERR                                
     *   (' BQUAD - THE ERROR ESTIMATE,ERREST, IS LARGER THAN EPS',     
     *    54, 3, 1)                                                     
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (KWARN .LT. 0 ) CALL SETERR                                    
C    *   (59H BQUAD - THE DYNAMIC STORAGE STACK IS FULL, USE PORT ISTKIN
C    *    ,59, 4, 2)                                                    
C/7S                                                                    
      IF (KWARN .LT. 0 ) CALL SETERR                                    
     *   (' BQUAD - THE DYNAMIC STORAGE STACK IS FULL, USE PORT ISTKIN' 
     *    ,59, 4, 2)                                                    
C/                                                                      
C                                                                       
       RETURN                                                           
      END                                                               
      SUBROUTINE R2QUAD(F,N,X,EPS,HS,NFCALL,LYSTAK,KMAXEX,KDIVID,       
     X          JPRINT,ANS,ERREST,KWARN)                                
C                                                                       
C INTEGRATE F(X) FROM X(1) TO X(N) WITH ABSOLUTE ACCURACY EPS.          
C MUST TAKE STEP SIZE AT LEAST AS SMALL AS HS*ABS(B-A).                 
C USE NO MORE THAN NFCALL SAMPLING POINTS, OR CALLS TO F.               
C FOR TEMPORARY STORAGE OF FUNCTION VALUES, ALLOCATE LYSTAK WORDS.      
C DO AT MOST KMAXEX EXTRAPOLATIONS.                                     
C DO NOT DIVIDE INTERVAL UNTIL AFTER KDIVID EXTRAPOLATIONS.             
C JPRINT = 0 FOR NO PRINTING, 1 FOR SOME, 2 FOR LOTS.                   
C TO START, DIVIDE (X(1),X(N)) INTO NUM INTERVALS.  INTERNAL            
C BREAKPOINTS ARE GIVEN IN ARRAY X.  NUM+1 = NUMBER OF DISTINCT X S.    
C RETURN VALUE AND ITS ESTIMATED ERROR IN ANS AND ERREST.               
C RETURN KWARN AS WARNING FLAG.                                         
C                                                                       
C FUNCTION ISTKQU RETURNS AMOUNT OF SPACE LEFT IN STACK.                
C FUNCTION ISTKGT ALLOCATES SPACE IN STACK AND GIVES POINTER TO IT.     
C SUBROUTINE LEAVE DE-ALLOCATES SPACE IN STACK.                         
C THE DEFAULT STACK LENGTH IS 500 DOUBLE PRECISION WORDS.               
C IF A LARGER STACK IS NECESSARY, THE PROGRAM CALLING R2QUAD            
C SHOULD INITIALIZE THE STACK  USING SUBROUTINE ISTKIN.                 
C                                                                       
      REAL F,EPS,HS,ANS,ERREST,X(1)                                     
      EXTERNAL F                                                        
      COMMON /R1QCM1/ DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL,        
     X   NPRINT,NUM,MAXY,MAXINT,MAXF,KMIN,KMAX,KDIV,NMAX                
      REAL DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL                    
C                                                                       
      COMMON/CSTAK/DS                                                   
      DOUBLE PRECISION DS(500)                                          
      REAL RS(1000),R1MACH                                              
      INTEGER IS(1000)                                                  
      EQUIVALENCE (DS(1),IS(1)),(DS(1),RS(1))                           
C                                                                       
C        MACHINE- AND PRECISION-DEPENDENT NUMBERS                       
C                                                                       
      HSMALL = 50.E0*R1MACH(4)                                          
      DROUND = 50.E0*R1MACH(4)                                          
      DSMALL = 10.E0*R1MACH(1)                                          
      DLARGE = 0.1E0*R1MACH(2)                                          
C                                                                       
C         PARAMETERS                                                    
C                                                                       
      NPRINT = JPRINT                                                   
      NUM=N-1                                                           
      DO 90 J=2,N                                                       
   90    IF (X(J-1).EQ.X(J)) NUM=NUM-1                                  
      NUM=MAX0(NUM,1)                                                   
      KMIN = 2                                                          
      KMAX = MAX0(4,MIN0(12,KMAXEX))                                    
      KMAX = 2*(KMAX/2)                                                 
      KDIV = MIN0(MAX0(KDIVID,4),KMAX)                                  
      KDIV = 2*(KDIV/2)                                                 
      HSAMPL = AMIN1(HS,1.E0)                                           
      ESMALL = EPS*1.E-3                                                
      NMAX = 1.-ALOG(DROUND)/ALOG(6.E0)                                 
      NMAX = MIN0(25,NMAX)                                              
C                                                                       
C        ALLOCATE SCRATCH SPACE                                         
C                                                                       
      MAXY = MAX0(NUM+7,LYSTAK)                                         
      MAXF = MAX0(4*NUM+1,NFCALL)                                       
      MAXINT=1.-ALOG(HSMALL)/ALOG(2.E0)                                 
      MAXINT = MAX0(MAXINT,NUM+2)                                       
      CALL ENTER(0)                                                     
      NSHORT = ISTKQU(2)-2*MAXINT                                       
      IF (NSHORT .LT. 0) GO TO 120                                      
      IIDIV = ISTKGT(2*MAXINT,2)                                        
      II = IIDIV+MAXINT                                                 
      NSHORT = ISTKQU(3)-(MAXINT+1+MAXY)                                
      IF (NSHORT .LT. 0) GO TO 120                                      
      IX = ISTKGT(MAXINT+1+MAXY,3)                                      
      IY = IX+MAXINT+1                                                  
C                                                                       
      JJ=IX                                                             
      RS(JJ)=X(1)                                                       
      DO 110 J=2,N                                                      
         IF (X(J).EQ.X(J-1)) GO TO 110                                  
            JJ=JJ+1                                                     
            RS(JJ)=X(J)                                                 
  110 CONTINUE                                                          
      JJ=IX+NUM                                                         
      RS(JJ)=X(N)                                                       
C                                                                       
      CALL R1QDAP(F,X(1),X(N),EPS,RS(IY),RS(IX),IS(II),IS(IIDIV),       
     X    ANS,ERREST,KWARN)                                             
C                                                                       
      CALL LEAVE                                                        
      RETURN                                                            
C                                                                       
  120 KWARN = NSHORT                                                    
      ANS = 0.                                                          
      ERREST = DLARGE                                                   
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE QUAD(F,A,B,EPS,ANS,ERREST)                             
C                                                                       
C TO INTEGRATE F(X) FROM A TO B WITH ABSOLUTE ACCURACY EPS,             
C SET DEFAULT VALUES OF PARAMETERS AND CALL R1QUAD                      
C                                                                       
      REAL F,A,B,EPS,ANS,ERREST,HSAMPL                                  
      EXTERNAL F                                                        
C                                                                       
      HSAMPL = 0.125                                                    
      NFCALL = 2000                                                     
      LYSTAK = 250                                                      
      KMAXEX = 6                                                        
      KDIVID = 4                                                        
      JPRINT = 0                                                        
      NUMINT = 3                                                        
C                                                                       
      CALL R1QUAD(F,A,B,EPS,HSAMPL,NFCALL,LYSTAK,KMAXEX,KDIVID,         
     X      JPRINT,NUMINT,ANS,ERREST,KWARN)                             
C                                                                       
C/6S                                                                    
C     IF (KWARN .LT. 0 ) CALL SETERR                                    
C    *   (57HQUAD - THE DYNAMIC STORAGE STACK IS FULL, USE PORT ISTKIN  
C    *    ,57,2,2)                                                      
C/7S                                                                    
      IF (KWARN .LT. 0 ) CALL SETERR                                    
     *   ('QUAD - THE DYNAMIC STORAGE STACK IS FULL, USE PORT ISTKIN'   
     *    ,57,2,2)                                                      
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (ERREST  .GT.  EPS) CALL SETERR                                
C    *   (52HQUAD - THE ERROR ESTIMATE,ERREST, IS LARGER THAN EPS,      
C    *    52,1,1)                                                       
C/7S                                                                    
      IF (ERREST  .GT.  EPS) CALL SETERR                                
     *   ('QUAD - THE ERROR ESTIMATE,ERREST, IS LARGER THAN EPS',       
     *    52,1,1)                                                       
C/                                                                      
C                                                                       
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE R1QUAD(F,A,B,EPS,HS,NFCALL,LYSTAK,KMAXEX,KDIVID,       
     X          JPRINT,NUMINT,ANS,ERREST,KWARN)                         
C                                                                       
C INTEGRATE F(X) FROM A TO B WITH ABSOLUTE ACCURACY EPS.                
C MUST TAKE STEP SIZE AT LEAST AS SMALL AS HS*ABS(B-A).                 
C USE NO MORE THAN NFCALL SAMPLING POINTS, OR CALLS TO F.               
C FOR TEMPORARY STORAGE OF FUNCTION VALUES, ALLOCATE LYSTAK WORDS.      
C DO AT MOST KMAXEX EXTRAPOLATIONS.                                     
C DO NOT DIVIDE INTERVAL UNTIL AFTER KDIVID EXTRAPOLATIONS.             
C JPRINT = 0 FOR NO PRINTING, 1 FOR SOME, 2 FOR LOTS.                   
C TO START, DIVIDE (A,B) INTO NUMINT EQUAL INTERVALS.                   
C RETURN VALUE AND ITS ESTIMATED ERROR IN ANS AND ERREST.               
C RETURN KWARN AS WARNING FLAG.                                         
C                                                                       
C FUNCTION ISTKQU RETURNS AMOUNT OF SPACE LEFT IN STACK.                
C FUNCTION ISTKGT ALLOCATES SPACE IN STACK AND GIVES POINTER TO IT.     
C SUBROUTINE LEAVE DE-ALLOCATES SPACE IN STACK.                         
C THE DEFAULT STACK LENGTH IS 500 DOUBLE PRECISION WORDS.               
C IF A LARGER STACK IS NECESSARY, THE PROGRAM CALLING R1QUAD OR QUAD    
C SHOULD INITIALIZE THE STACK  USING SUBROUTINE ISTKIN.                 
C                                                                       
      REAL F,A,B,EPS,HS,ANS,ERREST                                      
      EXTERNAL F                                                        
      COMMON /R1QCM1/ DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL,        
     X   NPRINT,NUM,MAXY,MAXINT,MAXF,KMIN,KMAX,KDIV,NMAX                
      REAL DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL                    
C                                                                       
      COMMON/CSTAK/DS                                                   
      DOUBLE PRECISION DS(500)                                          
      REAL RS(1000)                                                     
      INTEGER IS(1000)                                                  
      EQUIVALENCE (DS(1),IS(1)),(DS(1),RS(1))                           
C                                                                       
C        MACHINE- AND PRECISION-DEPENDENT NUMBERS                       
C                                                                       
      HSMALL = 50.*R1MACH(4)                                            
      DROUND = 50.*R1MACH(4)                                            
      DSMALL = 10.*R1MACH(1)                                            
      DLARGE = 0.1*R1MACH(2)                                            
C                                                                       
C         PARAMETERS                                                    
C                                                                       
      NPRINT = JPRINT                                                   
      NUM = MAX0(1,NUMINT)                                              
      KMIN = 2                                                          
      KMAX = MAX0(4,MIN0(12,KMAXEX))                                    
      KMAX = 2*(KMAX/2)                                                 
      KDIV = MIN0(MAX0(KDIVID,4),KMAX)                                  
      KDIV = 2*(KDIV/2)                                                 
      HSAMPL = AMIN1(HS,1.E0)                                           
      ESMALL = EPS*1.E-3                                                
      NMAX = 1.-ALOG(DROUND)/ALOG(6.E0)                                 
      NMAX = MIN0(25,NMAX)                                              
C                                                                       
C        ALLOCATE SCRATCH SPACE                                         
C                                                                       
      MAXY = MAX0(NUM+7,LYSTAK)                                         
      MAXF = MAX0(4*NUM+1,NFCALL)                                       
      MAXINT=1.-ALOG(HSMALL)/ALOG(2.E0)                                 
      MAXINT = MAX0(MAXINT,NUM+2)                                       
      CALL ENTER(0)                                                     
      NSHORT = ISTKQU(2)-2*MAXINT                                       
      IF (NSHORT .LT. 0) GO TO 120                                      
      IIDIV = ISTKGT(2*MAXINT,2)                                        
      II = IIDIV+MAXINT                                                 
      NSHORT = ISTKQU(3)-(MAXINT+1+MAXY)                                
      IF (NSHORT .LT. 0) GO TO 120                                      
      IX = ISTKGT(MAXINT+1+MAXY,3)                                      
      IY = IX+MAXINT+1                                                  
C                                                                       
      RS(IX)=A                                                          
      DO 110 J=1,NUM                                                    
         JJ=IX+J                                                        
  110    RS(JJ)=A+FLOAT(J)*(B-A)/FLOAT(NUM)                             
      RS(JJ)=B                                                          
C                                                                       
      CALL R1QDAP(F,A,B,EPS,RS(IY),RS(IX),IS(II),IS(IIDIV),             
     X    ANS,ERREST,KWARN)                                             
C                                                                       
      CALL LEAVE                                                        
      RETURN                                                            
C                                                                       
  120 KWARN = NSHORT                                                    
      ANS = 0.                                                          
      ERREST = DLARGE                                                   
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE R1QDAP(F,A,B,EPS,Y,X,I,IDIV,ANSWER,ERREST,KWARN)       
C R1QDAP ATTEMPTS TO INTEGRATE THE REAL FUNCTION F(X) FROM              
C X=A TO X=B, WITH ABSOLUTE ACCURACY EPS, USING NO MORE THAN            
C MAXF FUNCTION CALLS.                                                  
C Y, X, I, IDIV ARE SCRATCH ARRAYS WITH ASSUMED LENGTHS MAXY, MAXINT+1, 
C MAXINT, AND MAXINT.                                                   
C THE RESULT IS RETURNED IN ANSWER, AND THE ESTIMATED ERROR IN ERREST.  
C KWARN IS AN ERROR FLAG (SEE BELOW).                                   
C                                                                       
C IF THERE APPEARS TO BE A SINGULARITY AT X=A OR X=B, MAKE AN           
C APPROPRIATE CHANGE OF VARIABLE.  THE X ARRAY HAS DIVIDED (A,B) INTO   
C NUM INTERVALS.  IF NUM .GT. 1, REVERSE THE TOP INTERVAL TO SIMPLIFY   
C CHANGING VARIABLES.  IF NUM=1, ONLY LOOK FOR SINGULARITY AT X=A.      
C                                                                       
C IF INTEGRATION FAILS ON AN INTERVAL, IT IS DIVIDED IN  HALF,          
C THE LEFT HALF STACKED, AND THE RIGHT HALF ATTEMPTED.                  
C ALL FUNCTION VALUES ARE SAVED FOR USE ON THE HALF-INTERVAL.           
C                                                                       
C CALL R1QINT TO ATTEMPT THE INTEGRATION OF EACH INTERVAL.              
C PRO-RATE THE REMAINING ERROR ACCORDING TO THE INTERVAL SIZE,          
C BUT ALLOW AT LEAST ESMALL PER INTERVAL. IF NOISE IS SENSED,           
C AT LEAST ENOISE PER INTERVAL                                          
C                                                                       
C JF IS RETURNED BY R1QINT AS A FLAG.  JF=0 MEANS SUCCESS, JF=-1 MEANS  
C NEED TO DIVIDE THE INTERVAL, AND JF.GT.0 HAS THE CODE BELOW.          
C                                                                       
C      ERROR HANDLING AND ERROR FLAGS                                   
C KWARN IS RETURNED AS A BINARY  INTEGER, WITH UP TO 6 DIGITS.          
C EACH DIGIT IS 1 IF A PARTICULAR PROBLEM OCCURRED, AND 0 OTHERWISE.    
C THE MEANINGS OF THE SIX DIGITS FOLLOW.  1 IS THE RIGHT-MOST DIGIT,    
C AND 6 THE LEFT-MOST.                                                  
C 1.  ANSWER ACCEPTED BECAUSE ERROR ESTIMATE WAS BELOW EITHER           
C     ESTIMATED ROUNDOFF OR NOISE LEVEL IN FUNCTION.                    
C 2.  ANSWER ACCEPTED BECAUSE INTERVAL WAS TOO SHORT.                   
C 3.  ANSWER ACCEPTED BECAUSE INTERVAL STACKS (X, I, IDIV)              
C     WERE TOO FULL.                                                    
C 4.  ANSWER ACCEPTED BECAUSE FUNCTION VALUE STACK (Y) WAS TOO FULL.    
C 5.  ANSWER ACCEPTED BECAUSE NEXT BETTER APPROXIMATION WOULD           
C     HAVE REQUIRED EXCEEDING MAXF FUNCTION CALLS.                      
C 6.  ERREST IS GREATER THAN EPS.                                       
C                                                                       
C ERREST IS ALWAYS REALISTIC, UNLESS R1QDAP FAILS. IF NO RELIABLE       
C ANSWER AND ERROR ESTIMATE ARE AVAILABLE, ERREST IS SET TO DLARGE.     
C                                                                       
      DIMENSION Y(9),X(5),I(5),IDIV(5),MWARN(6)                         
      COMMON /R1QCM1/ DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL,        
     X   NPRINT,NUM,MAXY,MAXINT,MAXF,KMIN,KMAX,KDIV,NMAX                
      COMMON /R1QCM2/ TROUND,HMAX,HMIN,HNOISE,NUMF,INT,JY               
      COMMON /R1QCM3/ XBOT,DELX,R(3),RL(3),NEXP,KEXT,KLAST              
      REAL F,A,B,EPS,Y,X,ANSWER,ERREST,ERR,ERROR,ANS                    
      REAL DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL,ENOISE             
      REAL TROUND,HMAX,HMIN,HNOISE,SAVANS,SAVERR,TEMP,DX                
      REAL XBOT,DELX,DNOISE                                             
      EXTERNAL F                                                        
      LOGICAL LOWER                                                     
C                                                                       
      JOUT = I1MACH(2)                                                  
C                                                                       
      ANSWER = 0.                                                       
      ERREST = 0.                                                       
      KWARN = 0                                                         
      IF (B.EQ.A) RETURN                                                
C                                                                       
      DELX = B-A                                                        
      SAVANS = 0.                                                       
      JJ = NUM+1                                                        
      DO 10 J = 1,JJ                                                    
         Y(J) = F(X(J))                                                 
         SAVANS = SAVANS+Y(J)                                           
         I(J) = J                                                       
   10    IDIV(J) = 0                                                    
      INT = 1                                                           
      IF (NUM.EQ.1) GO TO 15                                            
         INT = NUM+1                                                    
         X(NUM+2) = X(NUM)                                              
         Y(NUM+2) = Y(NUM)                                              
   15 SAVANS = (SAVANS-0.5*(Y(NUM+1)+Y(1)))*DELX/FLOAT(NUM)             
      SAVERR = DLARGE                                                   
      NUMF = NUM+1                                                      
      HMAX = HSAMPL*ABS(DELX)                                           
      HMIN = HSMALL*ABS(DELX)                                           
      HNOISE = HMAX/32.                                                 
      ERROR = EPS/FLOAT(NUM)                                            
      ENOISE = 0.                                                       
      DO 20 J = 1,6                                                     
   20    MWARN(J) = 0                                                   
      XBOT = A                                                          
      NEXP = 1                                                          
      LOWER = NUM.EQ.1                                                  
      JPATT = 21                                                        
      IF (LOWER) JPATT = 0                                              
C                                                                       
C        TRY TOP INTERVAL IN STACK                                      
C                                                                       
  100 II = I(INT)                                                       
      JY = MAXY+1-II                                                    
      TROUND = DSMALL+DROUND*ABS(ANSWER)                                
      DX = ABS(X(INT+1)-X(INT))                                         
      ERROR = AMAX1(ERROR,ESMALL,ENOISE*DX)                             
      IF (NPRINT.GT.0) WRITE(JOUT,105) X(INT),X(INT+1),ERROR            
  105 FORMAT(16H0INTERVAL LIMITS,1P2E15.8,                              
     X   26H ATTEMPTED ERROR TOLERANCE, 1PE13.3)                        
C                                                                       
      CALL R1QINT(F,X(INT),X(INT+1),ERROR,Y(II),IDIV(INT),ANS,ERR,      
     X   DNOISE,JF)                                                     
C                                                                       
      JP = 2                                                            
      IF (JF.EQ.1) ENOISE = AMAX1(ENOISE,DNOISE)                        
      IF (JF.EQ.0 .OR. JF.EQ.1) JP = 1                                  
      JPATT = 10*MOD(JPATT,1000)+JP                                     
      IF (JF.GT.0) MWARN(JF) = 1                                        
      IF (JF.GT.0 .AND. NPRINT.GT.0) WRITE(JOUT,110) JF                 
  110 FORMAT(10X,7HWARNING,I2)                                          
      IF (JF.LT.0) GO TO 300                                            
      IF (ERR.GE.DLARGE) GO TO 400                                      
C                                                                       
C        ACCEPT ANSWER AND ERROR ESTIMATE FOR CURRENT INTERVAL.         
C                                                                       
      IF(LOWER) GO TO 200                                               
         ANS = -ANS                                                     
         IF (INT.GT.(NUM+1)) GO TO 200                                  
            LOWER = .TRUE.                                              
            INT = NUM                                                   
            XBOT = A                                                    
            DELX = B-A                                                  
            IF (NEXP.GT.1) HMIN = HSMALL*ABS(DELX)                      
            NEXP = 1                                                    
            JPATT = 2                                                   
C                                                                       
  200 ANSWER = ANSWER+ANS                                               
      ERREST = ERREST+ERR                                               
      IF (NPRINT.GT.0) WRITE(JOUT,205) ANS,ERR,NUMF                     
  205 FORMAT(17H SUCCESS.  ANSWER,1PE15.8,                              
     X       16H ESTIMATED ERROR,1PE13.3,                               
     X       28H SAMPLING POINTS USED SO FAR,I7)                        
      IF (INT.EQ.1) GO TO 500                                           
      INT = INT-1                                                       
      IF(     LOWER) TEMP = X(INT+1)-A                                  
      IF(.NOT.LOWER) TEMP = (B-X(INT+1))+(X(NUM)-A)                     
      ERROR = (EPS-ERREST)*ABS((X(INT+1)-X(INT))/TEMP)                  
C                                                                       
C           MAKE CHANGE OF VARIABLE, IF APPROPRIATE.                    
C                                                                       
      IF (JPATT.NE.2121 .OR. NEXP.GT.1) GO TO 100                       
         IF ((     LOWER .AND. INT.EQ.1) .OR.                           
     X       (.NOT.LOWER .AND. INT.EQ.(NUM+1)))                         
     X      CALL R1QCHG(IDIV(INT),X(INT),Y(INT),JPATT,NMAX,             
     X         DROUND,HMIN,NPRINT)                                      
         GO TO 100                                                      
C                                                                       
C        FAILED.  DIVIDE INTERVAL IN TWO.                               
C                                                                       
  300 X(INT+2) = X(INT+1)                                               
      X(INT+1) = .5*(X(INT)+X(INT+1))                                   
      IDIV(INT) = IDIV(INT)-1                                           
      IDIV(INT+1) = IDIV(INT)                                           
      I(INT+1) = I(INT)+3*2**IDIV(INT)                                  
      IF (NPRINT.GT.0) WRITE(JOUT,305) ANS,ERR,NUMF                     
  305 FORMAT(17H FAILURE.  ANSWER,1PE15.8,                              
     X       16H ESTIMATED ERROR,1PE13.3,                               
     X       28H SAMPLING POINTS USED SO FAR,I7)                        
      INT = INT+1                                                       
      ERROR = 0.5*ERROR                                                 
      IF (INT.EQ.2) SAVANS = ANSWER + ANS                               
      IF (INT.EQ.2) SAVERR = ERREST + ERR                               
      GO TO 100                                                         
C                                                                       
C        FAILED.  CANNOT CONTINUE.                                      
C        USE LAST GUESS FOR WHOLE INTERVAL                              
C                                                                       
  400 ERREST = SAVERR                                                   
      ANSWER = SAVANS                                                   
C                                                                       
C        PREPARE TO RETURN                                              
C                                                                       
  500 KWARN = 0                                                         
      IF (ERREST.GT.EPS) KWARN = 1                                      
      DO 510 J = 1,5                                                    
         JJ = 6-J                                                       
  510    KWARN =  2*KWARN+MWARN(JJ)                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE R1QCHG(IDIV,X,Y,JPATT,NMAX,DROUND,HMIN,NPRINT)         
C                                                                       
C USING RATIOS OF T-TABLE DIFFERENCES STORED IN RL, TRY TO RECOGNIZE    
C X**A BEHAVIOR.  FORMULAS ARE EMPIRICAL, NOT EXACT, FOR THE SEQUENCE   
C H, H/2, H/3, H/4, H/6, H/8, . . .                                     
C CHANGE OF VARIABLE PARAMETER NEXP IS CHOSEN SUCH THAT THE             
C TRANSFORMED INTEGRAL SHOULD BE ABLE TO CONVERGE IN THE THIRD          
C COLUMN OF THE T-TABLE.                                                
C                                                                       
      COMMON /R1QCM3/ XBOT,DELX,R(3),RL(3),NEXP,KEXT,KLAST              
      REAL X(5),Y(9),DROUND,XBOT,HMIN,DELX                              
      DIMENSION IDIV(5)                                                 
C                                                                       
      JOUT = I1MACH(2)                                                  
C                                                                       
      IF (AMIN1(RL(2),RL(3)) .LT. 0.1) RETURN                           
                      A2 = 2.74*ALOG(RL(2)/1.34)                        
                      A3 = -1.                                          
      IF (KLAST.EQ.4) A3 = 2.38*ALOG(RL(3)/1.57)                        
      IF (KLAST.GT.4) A3 = 2.89*ALOG(RL(3)/1.25)                        
      IF (ABS(A2-A3).GT.0.1 .OR. AMIN1(A2,A3).LT.(-0.99)) RETURN        
      NEXP=12./(2.+A2+A3)+0.5                                           
      NEXP = MAX0(1,MIN0(NEXP,NMAX))                                    
      IF (NEXP .LE. 1) RETURN                                           
         IF(NPRINT.GT.0) WRITE(JOUT,1) A2,A3,NEXP                       
    1    FORMAT(7H ALPHAS,2F6.3,26H CHANGE VARIABLES WITH N =,I4)       
         XBOT = X(1)                                                    
         DELX = X(2)-XBOT                                               
         Y(1) = 0.D0                                                    
         N = 1+3*2**IDIV(1)                                             
         Y(2) = FLOAT(NEXP)*Y(N)                                        
         IDIV(1) = 0                                                    
         HMIN = AMAX1(HMIN,ABS(DELX)*DROUND**(1./FLOAT(NEXP)))          
         JPATT = 0                                                      
         RETURN                                                         
      END                                                               
      SUBROUTINE R1QINT(F,A,B,ERR,Y,I,ANS,ERREST,DNOISE,JFAIL)          
C                                                                       
C ROMBERG QUADRATURE OF F(X) FROM A TO B, WITH ABSOLUTE ACCURACY ERR.   
C                                                                       
C ANSWER AND ESTIMATED ERROR RETURNED IN ANS AND ERREST.                
C ARRAY Y CONTAINS STACK OF F-VALUES FOR TRAPEZOIDAL RULES FROM H = B-A 
C THROUGH H = (B-A)/(3*2**(I-1)).  I AND Y ARE BOTH INPUT AND OUTPUT.   
C (IF I = 0 ON INPUT, ONLY HAVE VALUES FOR H = B-A.)                    
C JFAIL IS RETURNED AS A FLAG.  0 MEANS SUCCESS, -1 MEANS NEED          
C TO DIVIDE INTERVAL AND TRY AGAIN, AND JFAIL.GT.0 MEANS A PROBLEM.     
C IF JFAIL = 1, DNOISE MAY BE RETURNED AS AN ESTIMATE OF NOISE IN       
C THE FUNCTION F.                                                       
C                                                                       
C DIVIDE B-A INTO 1,2,3,4,6,8,12,16,...,96 INTERVALS                    
C DO TRAPEZOIDAL RULE AND CAUTIOUS RICHARDSON EXTRAPOLATION             
C                                                                       
C PARAMETERS GOVERNING EXTRAPOLATIONS                                   
C  KMIN = MINIMUM NUMBER BEFORE ANSWER IS BELIEVED  (KMIN.GE.2)         
C  KMAX = MAXIMUM NUMBER BEFORE GIVING UP,  (KMAX.LE.12)                
C  KDIV = MINIMUM NUMBER BEFORE GIVING UP IF NOT CONVERGING. (KDIV.GE.4)
C  HMAX = LARGEST TRAPEZOIDAL RULE H WHOSE ANSWER WILL BE BELIEVED.     
C  HMIN = SMALLEST H THAT WILL BE USED, APPROXIMATELY.                  
C                                                                       
C 0 .LE. I .LE. 6 ASSUMED ON INPUT.  I MAY BE INCREASED ON OUTPUT.      
C IF IT IS, NEW F-VALUES ARE CALCULATED AND STORED IN Y.                
C (THE FOLLOWING IS ASSUMED TRUE ON ENTERING, AND REMAINS TRUE ON UNSUC-
C CESSFUL EXIT, FOR OUTPUT VALUE OF I.  NOT TRUE ON SUCCESSFUL EXIT.)   
C Y(J) CONTAINS F(A+(J-1)*(B-A)/(3*2**I)), FOR THOSE JS USED IN TRAP-   
C EZOIDAL RULES THROUGH H = (B-A)/(3*2**(I-1)).  CONTENTS OF OTHER JS   
C ARE GARBAGE.  J RUNS FROM 1 THROUGH 3*2**I+1.                         
C                                                                       
C IF THE INTERVAL NEEDS TO BE SUBDIVIDED, RETURNS ARE MADE ONLY         
C AFTER 4, 6, 8, 10, OR 12 EXTRAPOLATIONS.                              
C SUCCESSFUL RETURNS CAN BE MADE AFTER KMIN OR MORE EXTRAPOLATIONS.     
C                                                                       
      COMMON /R1QCM1/ DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL,        
     X   NPRINT,NUM,MAXY,MAXINT,MAXF,KMIN,KMAX,KDIV,NMAX                
      COMMON /R1QCM2/ TROUND,HMAX,HMIN,HNOISE,NUMF,INT,JY               
      COMMON /R1QCM3/ XBOT,DELX,R(3),RL(3),NEXP,KEXT,KLAST              
      COMMON /R1QCM4/ TRAP,T(13),S(13),X(13)                            
      REAL F,A,B,ERR,Y(9),ANS,ERREST,DNOISE,DEL,H,DELTA                 
      REAL DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL                    
      REAL TROUND,HMAX,HMIN,HNOISE,XBOT,DELX,R1QFUN,S,X                 
      REAL TEMP1,TEMP2                                                  
      DOUBLE PRECISION TRAP,T,T0,TCUM(3)                                
      EXTERNAL F                                                        
      LOGICAL NCHECK,INIT,LROUND                                        
      DATA INIT/.FALSE./                                                
C                                                                       
      IF(INIT) GO TO 20                                                 
         X(1)=1.                                                        
         X(2)=4.                                                        
         X(3)=9.                                                        
         DO 10 J=4,13                                                   
   10       X(J)=4.*X(J-2)                                              
         INIT=.TRUE.                                                    
C                                                                       
   20 DO 30 J = 1,3                                                     
   30    RL(J) = R(J)                                                   
      KLAST = KEXT                                                      
      ANS = 0.                                                          
      ERREST = DLARGE                                                   
      DNOISE = 0.                                                       
      DEL = B-A                                                         
C                                                                       
C         FIRST TWO EXTRAPOLATIONS                                      
C                                                                       
      IF (I.GE.1) GO TO 40                                              
         IF(JY.LT.7) GO TO 9940                                         
         IF(NUMF+3.GT.MAXF) GO TO 9950                                  
            I = 1                                                       
            Y(3) = R1QFUN(F,(2.*A+B)/3.E0)                              
            Y(4) = R1QFUN(F,.5*(A+B))                                   
            Y(5) = R1QFUN(F,(A+2.*B)/3.E0)                              
            Y(7) = Y(2)                                                 
            NUMF = NUMF+3                                               
C                                                                       
   40 N = 3*2**I+1                                                      
      T0 = 0.5*(Y(1)+Y(N))                                              
      T(1) = DEL*T0                                                     
C                                                                       
      DO 100 KK = 1,2                                                   
         KEXT = KK                                                      
         H = DEL/FLOAT(KEXT+1)                                          
         L = (N+KEXT)/(KEXT+1)                                          
         TCUM(KEXT) = Y(L)                                              
         M = N+1-L                                                      
         IF (KEXT.EQ.2) TCUM(2) = TCUM(2)+Y(M)                          
         TRAP = H*(T0+TCUM(KEXT))                                       
         NCHECK = KEXT.LT.KMIN .OR. ABS(H).GT.HMAX                      
         CALL R1QEXT(KEXT,KDIV,NCHECK,TROUND,ANS,ERREST,R,KASYM,LROUND) 
         IF (ERREST.LE.ERR) GO TO 9900                                  
         IF (LROUND) GO TO 9910                                         
  100    CONTINUE                                                       
      TCUM(3) = TCUM(1)                                                 
C                                                                       
C         REST OF EXTRAPOLATIONS                                        
C                                                                       
      NEW = 1                                                           
      IMAX = MIN0(6,(KMAX+1)/2)                                         
      DO 200 INEW =2,IMAX                                               
C                                                                       
         II = MAX0(1,2**(I-INEW))                                       
         IF (INEW.LE.I) GO TO 120                                       
            IF ((2*N-1).GT.JY) GO TO 9940                               
            IF((NUMF+4*NEW).GT.MAXF) GO TO 9950                         
C                 HAVE USED ALL INPUT F-VALUES.                         
C                 SHIFT Y ARRAY TO MAKE ROOM FOR MORE.                  
               DO 110 J = 2,N                                           
                  L = N+2-J                                             
  110             Y(2*L-1) = Y(L)                                       
               N = 2*N-1                                                
C                                                                       
  120    DO 140 KK=1,2                                                  
            H = DEL/FLOAT(NEW*2*(KK+1))                                 
            DO 130 J = 1,NEW                                            
               L = II*(6*J*KK-(7*KK-4))+1                               
               M = N+1-L                                                
               IF (I.GE.INEW) GO TO 130                                 
                  DELTA = H*FLOAT((4*KK-2)*(J-1)+1)                     
                  Y(L) = R1QFUN(F,A+DELTA)                              
                  Y(M) = R1QFUN(F,B-DELTA)                              
                  NUMF = NUMF+2                                         
  130          TCUM(KK) = TCUM(KK)+Y(L)+Y(M)                            
            TRAP = T0+TCUM(KK)                                          
            IF(KK.EQ.2) TRAP = TRAP+TCUM(3)                             
            TRAP = H*TRAP                                               
            KEXT = KEXT+1                                               
            NCHECK = KEXT.LT.KMIN .OR. ABS(H).GT.HMAX                   
            CALL R1QEXT(KEXT,KDIV,NCHECK,TROUND,ANS,ERREST,R,KASYM,     
     X                  LROUND)                                         
            IF (ERREST.LE.ERR) GO TO 9900                               
            IF (LROUND) GO TO 9910                                      
  140       CONTINUE                                                    
         TCUM(3) = TCUM(1)                                              
         I = MAX0(I,INEW)                                               
C                                                                       
         IF (ABS(DEL).LE.HMIN) GO TO 9920                               
         IF (KEXT.GE.KDIV .AND. I.EQ.INEW                               
     X         .AND. (KASYM.LT.(KEXT-2))) GO TO 210                     
C                                                                       
  200    NEW = 2*NEW                                                    
C                                                                       
  210 IF (INT.EQ.MAXINT) GO TO 9930                                     
C                                                                       
      IF (ABS(H).GT.HNOISE) GO TO 9890                                  
         NS = 0                                                         
         KK = 1+3*2**I                                                  
         TEMP1 = Y(5)-2.*Y(3)+Y(1)                                      
         DNOISE = ABS(TEMP1)                                            
         DO 230 J = 7,KK,2                                              
            TEMP2 = Y(J)-2.*Y(J-2)+Y(J-4)                               
            DNOISE = DNOISE+ABS(TEMP2)                                  
            IF(SIGN(1.E0,TEMP1).NE.SIGN(1.E0,TEMP2)) NS = NS+1          
  230       TEMP1 = TEMP2                                               
         DNOISE = 2.*DNOISE/FLOAT(KK-3)                                 
         IF(NS.GT.2) GOTO 9910                                          
C                                                                       
 9890 JFAIL = -1                                                        
      RETURN                                                            
 9900 JFAIL = 0                                                         
      RETURN                                                            
 9910 JFAIL = 1                                                         
      RETURN                                                            
 9920 JFAIL = 2                                                         
      RETURN                                                            
 9930 JFAIL = 3                                                         
      RETURN                                                            
 9940 JFAIL = 4                                                         
      RETURN                                                            
 9950 JFAIL = 5                                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE R1QEXT(M,KDIV,NCHECK,TROUND,ANS,ERREST,R,K,LROUND)     
C                                                                       
C CAUTIOUS RICHARDSON POLYNOMIAL EXTRAPOLATION.                         
C DO MTH EXTRAPOLATION = CALCULATE (M+1)ST ROW IN T-TABLE.              
C (1 .LE. M .LE. 12 IS ASSUMED.)                                        
C NEWEST ESTIMATE IS IN TIN.  ERROR IN TIN IS ASSUMED TO BE OF          
C THE FORM SUM OF H**(GAMMA*N),N=1,2,3,...                              
C FOR THE SEQUENCE OF HS USED, VALUES OF 1/H**GAMMA ARE IN ARRAY X.     
C USUAL APPLICATION IS TO ROMBERG INTEGRATION BASED ON TRAPEZOIDAL      
C RULE--THEN GAMMA = 2.                                                 
C                                                                       
C ON ENTRY, ANS AND ERREST HAVE GUESS AT EXTRAPOLATED VALUE AND AN      
C ERROR ESTIMATE FOR IT.  IF THIS EXTRAPOLATION GENERATES A VALUE       
C WITH LOWER ESTIMATED ERROR, RETURN NEW VALUES FOR ANS AND ERREST.     
C                                                                       
C T CONTAINS ROW M OF T-TABLE ON ENTRY, AND S CONTAINS THE DIFFERENCE   
C BETWEEN ROWS M AND (M-1).  THESE ARE UPDATED BEFORE EXITING.          
C                                                                       
C DONT CHECK ERROR OF NEW VALUES IF NCHECK IS TRUE ON INPUT             
C K IS RETURNED TO INDICATE THE DEGREE OF ASYMPTOTICITY OF THE TRIANGLE.
C LROUND IS SET TRUE IF ERROR ESTIMATE IS LESS THAN GUESS AT ROUNDOFF   
C VALUES OF RATIOS OF DIFFERENCES OF T-TABLE ENTRIES ARE SAVED IN       
C ARRAY R FOR POSSIBLE USE IN CHANGING VARIABLES.                       
C                                                                       
      LOGICAL NCHECK,LROUND,NOGOOD,SKIP                                 
      COMMON /R1QCM1/ DUMMY(6),NPRINT,NDUMMY(8)                         
      COMMON /R1QCM4/ TIN,T(13),S(13),X(13)                             
      REAL TROUND,ANS,ERREST,ERR,DUMMY,S,X                              
      DOUBLE PRECISION TNEW,TNEXT,TIN,T,RDELTA,DELTA                    
      DIMENSION R(1)                                                    
      DATA TOL/0.05/                                                    
C                                                                       
      JOUT = I1MACH(2)                                                  
C                                                                       
      IF (M.EQ.1 .AND. NPRINT.GT.1) WRITE(JOUT,5) T(1)                  
    5 FORMAT(1X,1PE15.8,10X,7HT-TABLE)                                  
      SKIP = NCHECK                                                     
      TNEW = TIN                                                        
      K = 0                                                             
C                                                                       
      DO 100 J = 1,M                                                    
C                                                                       
C           TIN IS COLUMN 1 IN NEW ROW                                  
C           EXTRAPOLATE TO GET COLUMN J+1, AND ERROR IN COLUMN J.       
C                                                                       
         DELTA = TNEW-T(J)                                              
         ERR = DABS(DELTA)                                              
         JJ = M+1-J                                                     
         RDELTA = DELTA*X(JJ)/(X(M+1)-X(JJ))                            
         TNEXT = TNEW+RDELTA                                            
C                                                                       
         IF (SKIP .OR. (J.EQ.M .AND. M.EQ.2 .AND. K.LT.2)) GO TO 60     
            IF (ERR.LE.TROUND .OR.  J.EQ.M) GO TO 40                    
C                                                                       
C                 ESTIMATE ERROR IN TNEW.                               
C                 ALL PREVIOUS COLUMNS WERE ASYMPTOTIC, OR ALMOST SO.   
C                                                                       
               RTEMP = (X(JJ-1)*(X(M+1)-X(JJ))*S(J))/                   
     X                 (X(M+1)*(X(M)-X(JJ-1))*DELTA)                    
               IF (ABS(RTEMP-1.) .GT. TOL*FLOAT(J)) GO TO 20            
C                                                                       
C                   ASYMPTOTIC COLUMN. USE RUNGE ERROR ESTIMATE.        
C                                                                       
                  ERR = DABS(RDELTA)                                    
                  K = K+2                                               
                  GO TO 40                                              
C                                                                       
C                    NON-ASYMPTOTIC COLUMN. USE MORE CONSERVATIVE       
C                    ERROR ESTIMATE.                                    
C                                                                       
   20             SKIP = J.GT.1                                         
                  IF (J.EQ.1 .AND. M.LT.KDIV) GO TO 60                  
                    IF (J.EQ.1) ERR = 2.*(ERR+ABS(S(1)))                
                    NOGOOD = ABS(RTEMP-1.) .GT. TOL*FLOAT(J+4)          
                    IF(.NOT. NOGOOD) K = K+1                            
                    SKIP = SKIP .OR. NOGOOD                             
                    IF ((RTEMP.LT.0.25 .OR. RTEMP.GT.4.0)               
     X                  .AND. J.GT.1) GO TO 60                          
C                                                                       
C                 SAVE NEW VALUES IF LOWER ERROR ESTIMATE               
C                                                                       
   40          IF (ERR.GT.ERREST) GO TO 60                              
                  ANS = TNEW                                            
                  ERREST = ERR                                          
C                                                                       
C           SHIFT FOR NEXT EXTRAPOLATION. SAVE FIRST 3 RATIOS.          
C                                                                       
   60    IF (J.GT.MIN0(3,M-1)) GO TO 80                                 
            R(J) = 0.                                                   
            IF (ERR.GT.0.D0) R(J) = S(J)/DELTA                          
   80    S(J) = DELTA                                                   
         T(J) = TNEW                                                    
         TNEW = TNEXT                                                   
  100    CONTINUE                                                       
C                                                                       
      T(M+1) = TNEW                                                     
      LROUND = ERREST.LE.TROUND                                         
      MP =M+1                                                           
      IF (NPRINT.GT.1) WRITE(JOUT,15) (T(J),J = 1,MP)                   
   15 FORMAT(1X,1P8E15.8)                                               
      RETURN                                                            
      END                                                               
      REAL FUNCTION R1QFUN(F,X)                                         
C                                                                       
C FUNCTION TO BE INTEGRATED USING R1QDAP AND R1QINT.                    
C NEXP .GT. 1 IF CHANGE OF VARIABLE HAS BEEN MADE.                      
C                                                                       
      REAL F,X,XBOT,DELX,V                                              
      EXTERNAL F                                                        
      COMMON /R1QCM3/ XBOT,DELX,R(3),RL(3),NEXP,KEXT,KLAST              
C                                                                       
      IF (NEXP.GT.1) GO TO 10                                           
         R1QFUN = F(X)                                                  
         RETURN                                                         
C                                                                       
   10 V = ((X-XBOT)/DELX)**(NEXP-1)                                     
      R1QFUN = FLOAT(NEXP)*V*F(XBOT+V*(X-XBOT))                         
      RETURN                                                            
      END                                                               
      SUBROUTINE DRQUAD(F,A,B,EPSABS,EPSREL,ANS,ERREST)                 
C                                                                       
C TO INTEGRATE F(X) FROM A TO B WITH ACCURACY EPS,                      
C WHERE EPS = EPSABS + EPSREL * ABS(ANS),                               
C SET DEFAULT VALUES OF PARAMETERS AND CALL D1QUAD.                     
C IF EPSREL .GT. 0, NEED TO ESTIMATE ANS FIRST.                         
C                                                                       
      DOUBLE PRECISION F,A,B,EPSABS,EPSREL,EPS,ANS,ERREST,HSAMPL        
      DOUBLE PRECISION EPSA,EPSR,D1MACH                                 
      EXTERNAL F                                                        
C                                                                       
      HSAMPL = 0.125                                                    
      NFCALL = 2000                                                     
      LYSTAK = 250                                                      
      KMAXEX = 8                                                        
      KDIVID = 4                                                        
      JPRINT = 0                                                        
      NUMINT = 3                                                        
C                                                                       
      EPSA = DMAX1(EPSABS,0.D0)                                         
      EPSR = DMAX1(EPSREL,0.D0)                                         
      EPS = EPSABS                                                      
      JMAX = 1                                                          
      IF (EPSR .GT. 0.D0) EPS = 0.05D0*D1MACH(2)                        
      IF (EPSR .GT. 0.D0) JMAX = 3                                      
C                                                                       
      DO 100 J=1,JMAX                                                   
         CALL D1QUAD(F,A,B,EPS,HSAMPL,NFCALL,LYSTAK,KMAXEX,KDIVID,      
     X         JPRINT,NUMINT,ANS,ERREST,KWARN)                          
C                                                                       
C/6S                                                                    
C     IF (KWARN .LT. 0 ) CALL SETERR                                    
C    *   (59HDRQUAD - THE DYNAMIC STORAGE STACK IS FULL, USE PORT ISTKIN
C    *    ,59,2,2)                                                      
C/7S                                                                    
      IF (KWARN .LT. 0 ) CALL SETERR                                    
     *   ('DRQUAD - THE DYNAMIC STORAGE STACK IS FULL, USE PORT ISTKIN' 
     *    ,59,2,2)                                                      
C/                                                                      
C                                                                       
         IF (KWARN .GT. 0) GO TO 200                                    
         EPS = EPSA + EPSR * DABS(ANS)                                  
         IF (ERREST .LE. EPS) RETURN                                    
         EPS = EPSA + EPSR * DMAX1(0.5D0*DABS(ANS), DABS(ANS)-ERREST)   
C                                                                       
  100 CONTINUE                                                          
C                                                                       
C/6S                                                                    
C 200 CALL SETERR                                                       
C    *   (54HDRQUAD - THE ERROR ESTIMATE,ERREST, IS LARGER THAN EPS,    
C    *    54,1,1)                                                       
C/7S                                                                    
  200 CALL SETERR                                                       
     *   ('DRQUAD - THE ERROR ESTIMATE,ERREST, IS LARGER THAN EPS',     
     *    54,1,1)                                                       
C/                                                                      
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DBQUAD(F,N,X,EPS,ANS,ERREST)                           
C                                                                       
C TO INTEGRATE F(X) FROM X(1) TO X(N) WITH ABSOLUTE ACCURACY EPS,       
C SET DEFAULT VALUES OF PARAMETERS AND CALL D2QUAD.  ALL THE DISTINCT   
C POINTS AMONG X(2), ..., X(N-1) ARE USED AS INITIAL BREAKPOINTS.       
C THIS PROGRAM IS USEFUL FOR INTEGRATING FUNCTIONS WHICH HAVE           
C DISCONTINUITIES IN DERIVATIVES AT THE BREAKPOINTS, BUT NOT            
C DISCONTINUITIES IN VALUE.                                             
C                                                                       
      DOUBLE PRECISION F,X(N),EPS,ANS,ERREST,HSAMPL                     
      LOGICAL MONOD                                                     
      EXTERNAL F                                                        
C                                                                       
      HSAMPL = 0.125                                                    
      NFCALL = 2000                                                     
      LYSTAK = 250                                                      
      KMAXEX = 8                                                        
      KDIVID = 4                                                        
      JPRINT = 0                                                        
C                                                                       
C/6S                                                                    
C     IF (N .LT. 2) CALL SETERR                                         
C    *   (29HDBQUAD - N MUST BE AT LEAST 2, 29, 1, 2)                   
C     IF (.NOT.MONOD(X,N,1)) CALL SETERR                                
C    *   (37HDBQUAD - THE X ARRAY MUST BE MONOTONE, 37, 2, 2)           
C/7S                                                                    
      IF (N .LT. 2) CALL SETERR                                         
     *   ('DBQUAD - N MUST BE AT LEAST 2', 29, 1, 2)                    
      IF (.NOT.MONOD(X,N,1)) CALL SETERR                                
     *   ('DBQUAD - THE X ARRAY MUST BE MONOTONE', 37, 2, 2)            
C/                                                                      
C                                                                       
      CALL D2QUAD(F,N,X,EPS,HSAMPL,NFCALL,LYSTAK,KMAXEX,KDIVID,         
     *      JPRINT,ANS,ERREST,KWARN)                                    
C                                                                       
C                                                                       
C/6S                                                                    
C     IF (ERREST  .GT.  EPS) CALL SETERR                                
C    *   (54HDBQUAD - THE ERROR ESTIMATE,ERREST, IS LARGER THAN EPS,    
C    *    54, 3, 1)                                                     
C/7S                                                                    
      IF (ERREST  .GT.  EPS) CALL SETERR                                
     *   ('DBQUAD - THE ERROR ESTIMATE,ERREST, IS LARGER THAN EPS',     
     *    54, 3, 1)                                                     
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (KWARN .LT. 0 ) CALL SETERR                                    
C    *   (59HDBQUAD - THE DYNAMIC STORAGE STACK IS FULL, USE PORT ISTKIN
C    *    ,59, 4, 2)                                                    
C/7S                                                                    
      IF (KWARN .LT. 0 ) CALL SETERR                                    
     *   ('DBQUAD - THE DYNAMIC STORAGE STACK IS FULL, USE PORT ISTKIN' 
     *    ,59, 4, 2)                                                    
C/                                                                      
C                                                                       
       RETURN                                                           
      END                                                               
      SUBROUTINE D2QUAD(F,N,X,EPS,HS,NFCALL,LYSTAK,KMAXEX,KDIVID,       
     X          JPRINT,ANS,ERREST,KWARN)                                
C                                                                       
C INTEGRATE F(X) FROM X(1) TO X(N) WITH ABSOLUTE ACCURACY EPS.          
C MUST TAKE STEP SIZE AT LEAST AS SMALL AS HS*DABS(B-A).                
C USE NO MORE THAN NFCALL SAMPLING POINTS, OR CALLS TO F.               
C FOR TEMPORARY STORAGE OF FUNCTION VALUES, ALLOCATE LYSTAK WORDS.      
C DO AT MOST KMAXEX EXTRAPOLATIONS.                                     
C DO NOT DIVIDE INTERVAL UNTIL AFTER KDIVID EXTRAPOLATIONS.             
C JPRINT = 0 FOR NO PRINTING, 1 FOR SOME, 2 FOR LOTS.                   
C TO START, DIVIDE (X(1),X(N)) INTO NUM INTERVALS.  INTERNAL            
C BREAKPOINTS ARE GIVEN IN ARRAY X.  NUM+1 = NUMBER OF DISTINCT X S.    
C RETURN VALUE AND ITS ESTIMATED ERROR IN ANS AND ERREST.               
C RETURN KWARN AS WARNING FLAG.                                         
C                                                                       
C FUNCTION ISTKQU RETURNS AMOUNT OF SPACE LEFT IN STACK.                
C FUNCTION ISTKGT ALLOCATES SPACE IN STACK AND GIVES POINTER TO IT.     
C SUBROUTINE LEAVE DE-ALLOCATES SPACE IN STACK.                         
C THE DEFAULT STACK LENGTH IS 500 DOUBLE PRECISION WORDS.               
C IF A LARGER STACK IS NECESSARY, THE PROGRAM CALLING D2QUAD            
C SHOULD INITIALIZE THE STACK  USING SUBROUTINE ISTKIN.                 
C                                                                       
      DOUBLE PRECISION F,EPS,HS,ANS,ERREST,X(1)                         
      EXTERNAL F                                                        
      COMMON /D1QCM1/ DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL,        
     X   NPRINT,NUM,MAXY,MAXINT,MAXF,KMIN,KMAX,KDIV,NMAX                
      DOUBLE PRECISION DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL        
      DOUBLE PRECISION DLOG                                             
C                                                                       
      COMMON/CSTAK/DS                                                   
      DOUBLE PRECISION DS(500),D1MACH                                   
      INTEGER IS(1000)                                                  
      EQUIVALENCE (DS(1),IS(1))                                         
C                                                                       
C        MACHINE- AND PRECISION-DEPENDENT NUMBERS                       
C                                                                       
      HSMALL = 50.D0*D1MACH(4)                                          
      DROUND = 50.D0*D1MACH(4)                                          
      DSMALL = 10.D0*D1MACH(1)                                          
      DLARGE = 0.1D0*D1MACH(2)                                          
C                                                                       
C         PARAMETERS                                                    
C                                                                       
      NPRINT = JPRINT                                                   
      NUM=N-1                                                           
      DO 90 J=2,N                                                       
   90    IF (X(J-1).EQ.X(J)) NUM=NUM-1                                  
      NUM=MAX0(NUM,1)                                                   
      KMIN = 2                                                          
      KMAX = MAX0(4,MIN0(12,KMAXEX))                                    
      KMAX = 2*(KMAX/2)                                                 
      KDIV = MIN0(MAX0(KDIVID,4),KMAX)                                  
      KDIV = 2*(KDIV/2)                                                 
      HSAMPL = DMIN1(HS,1.D0)                                           
      ESMALL = EPS*1.D-3                                                
      NMAX = 1.-DLOG(DROUND)/DLOG(6.D0)                                 
      NMAX = MIN0(25,NMAX)                                              
C                                                                       
C        ALLOCATE SCRATCH SPACE                                         
C                                                                       
      MAXY = MAX0(NUM+7,LYSTAK)                                         
      MAXF = MAX0(4*NUM+1,NFCALL)                                       
      MAXINT=1.-DLOG(HSMALL)/DLOG(2.D0)                                 
      MAXINT = MAX0(MAXINT,NUM+2)                                       
      CALL ENTER(0)                                                     
      NSHORT = ISTKQU(2)-2*MAXINT                                       
      IF (NSHORT .LT. 0) GO TO 120                                      
      IIDIV = ISTKGT(2*MAXINT,2)                                        
      II = IIDIV+MAXINT                                                 
      NSHORT = ISTKQU(4)-(MAXINT+1+MAXY)                                
      IF (NSHORT .LT. 0) GO TO 120                                      
      IX = ISTKGT(MAXINT+1+MAXY,4)                                      
      IY = IX+MAXINT+1                                                  
C                                                                       
      JJ=IX                                                             
      DS(JJ)=X(1)                                                       
      DO 110 J=2,N                                                      
         IF (X(J).EQ.X(J-1)) GO TO 110                                  
            JJ=JJ+1                                                     
            DS(JJ)=X(J)                                                 
  110 CONTINUE                                                          
      JJ=IX+NUM                                                         
      DS(JJ)=X(N)                                                       
C                                                                       
      CALL D1QDAP(F,X(1),X(N),EPS,DS(IY),DS(IX),IS(II),IS(IIDIV),       
     X    ANS,ERREST,KWARN)                                             
C                                                                       
      CALL LEAVE                                                        
      RETURN                                                            
C                                                                       
  120 KWARN = NSHORT                                                    
      ANS = 0.                                                          
      ERREST = DLARGE                                                   
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DQUAD(F,A,B,EPS,ANS,ERREST)                            
C                                                                       
C TO INTEGRATE F(X) FROM A TO B WITH ABSOLUTE ACCURACY EPS,             
C SET DEFAULT VALUES OF PARAMETERS AND CALL D1QUAD                      
C                                                                       
      DOUBLE PRECISION F,A,B,EPS,ANS,ERREST,HSAMPL                      
      EXTERNAL F                                                        
C                                                                       
      HSAMPL = 0.125                                                    
      NFCALL = 2000                                                     
      LYSTAK = 250                                                      
      KMAXEX = 8                                                        
      KDIVID = 4                                                        
      JPRINT = 0                                                        
      NUMINT = 3                                                        
C                                                                       
      CALL D1QUAD(F,A,B,EPS,HSAMPL,NFCALL,LYSTAK,KMAXEX,KDIVID,         
     X      JPRINT,NUMINT,ANS,ERREST,KWARN)                             
C                                                                       
C/6S                                                                    
C     IF (KWARN .LT. 0 ) CALL SETERR                                    
C    *   (58HDQUAD - THE DYNAMIC STORAGE STACK IS FULL, USE PORT ISTKIN 
C    *    ,58,2,2)                                                      
C/7S                                                                    
      IF (KWARN .LT. 0 ) CALL SETERR                                    
     *   ('DQUAD - THE DYNAMIC STORAGE STACK IS FULL, USE PORT ISTKIN'  
     *    ,58,2,2)                                                      
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (ERREST  .GT.  EPS) CALL SETERR                                
C    *   (53HDQUAD - THE ERROR ESTIMATE,ERREST, IS LARGER THAN EPS,     
C    *    53,1,1)                                                       
C/7S                                                                    
      IF (ERREST  .GT.  EPS) CALL SETERR                                
     *   ('DQUAD - THE ERROR ESTIMATE,ERREST, IS LARGER THAN EPS',      
     *    53,1,1)                                                       
C/                                                                      
C                                                                       
C                                                                       
       RETURN                                                           
      END                                                               
      SUBROUTINE D1QUAD(F,A,B,EPS,HS,NFCALL,LYSTAK,KMAXEX,KDIVID,       
     X          JPRINT,NUMINT,ANS,ERREST,KWARN)                         
C                                                                       
C INTEGRATE F(X) FROM A TO B WITH ABSOLUTE ACCURACY EPS.                
C MUST TAKE STEP SIZE AT LEAST AS SMALL AS HS*DABS(B-A).                
C USE NO MORE THAN NFCALL SAMPLING POINTS, OR CALLS TO F.               
C FOR TEMPORARY STORAGE OF FUNCTION VALUES, ALLOCATE LYSTAK WORDS.      
C DO AT MOST KMAXEX EXTRAPOLATIONS.                                     
C DO NOT DIVIDE INTERVAL UNTIL AFTER KDIVID EXTRAPOLATIONS.             
C JPRINT = 0 FOR NO PRINTING, 1 FOR SOME, 2 FOR LOTS.                   
C TO START, DIVIDE (A,B) INTO NUMINT EQUAL INTERVALS.                   
C RETURN VALUE AND ITS ESTIMATED ERROR IN ANS AND ERREST.               
C RETURN KWARN AS WARNING FLAG.                                         
C                                                                       
C FUNCTION ISTKQU RETURNS AMOUNT OF SPACE LEFT IN STACK.                
C FUNCTION ISTKGT ALLOCATES SPACE IN STACK AND GIVES POINTER TO IT.     
C SUBROUTINE LEAVE DE-ALLOCATES SPACE IN STACK.                         
C THE DEFAULT STACK LENGTH IS 500 DOUBLE PRECISION WORDS.               
C IF A LARGER STACK IS NECESSARY, THE PROGRAM CALLING D1QUAD OR DQUAD   
C SHOULD INITIALIZE THE STACK  USING SUBROUTINE ISTKIN.                 
C                                                                       
      DOUBLE PRECISION F,A,B,EPS,HS,ANS,ERREST,D1MACH                   
      EXTERNAL F                                                        
      COMMON /D1QCM1/ DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL,        
     X   NPRINT,NUM,MAXY,MAXINT,MAXF,KMIN,KMAX,KDIV,NMAX                
      DOUBLE PRECISION DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL        
      DOUBLE PRECISION DLOG                                             
C                                                                       
      COMMON/CSTAK/DS                                                   
      DOUBLE PRECISION DS(500)                                          
      INTEGER IS(1000)                                                  
      EQUIVALENCE (DS(1),IS(1))                                         
C                                                                       
C        MACHINE- AND PRECISION-DEPENDENT NUMBERS                       
C                                                                       
      HSMALL = 50.D0*D1MACH(4)                                          
      DROUND = 50.D0*D1MACH(4)                                          
      DSMALL = 10.D0*D1MACH(1)                                          
      DLARGE = 0.1D0*D1MACH(2)                                          
C                                                                       
C         PARAMETERS                                                    
C                                                                       
      NPRINT = JPRINT                                                   
      NUM = MAX0(1,NUMINT)                                              
      KMIN = 2                                                          
      KMAX = MAX0(4,MIN0(12,KMAXEX))                                    
      KMAX = 2*(KMAX/2)                                                 
      KDIV = MIN0(MAX0(KDIVID,4),KMAX)                                  
      KDIV = 2*(KDIV/2)                                                 
      HSAMPL = DMIN1(HS,1.D0)                                           
      ESMALL = EPS*1.D-3                                                
      NMAX = 1.-DLOG(DROUND)/DLOG(6.D0)                                 
      NMAX = MIN0(25,NMAX)                                              
C                                                                       
C        ALLOCATE SCRATCH SPACE                                         
C                                                                       
      MAXY = MAX0(NUM+7,LYSTAK)                                         
      MAXF = MAX0(4*NUM+1,NFCALL)                                       
      MAXINT=1.-DLOG(HSMALL)/DLOG(2.D0)                                 
      MAXINT = MAX0(MAXINT,NUM+2)                                       
      CALL ENTER(0)                                                     
      NSHORT = ISTKQU(2)-2*MAXINT                                       
      IF (NSHORT .LT. 0) GO TO 120                                      
      IIDIV = ISTKGT(2*MAXINT,2)                                        
      II = IIDIV+MAXINT                                                 
      NSHORT = ISTKQU(4)-(MAXINT+1+MAXY)                                
      IF (NSHORT .LT. 0) GO TO 120                                      
      IX = ISTKGT(MAXINT+1+MAXY,4)                                      
      IY = IX+MAXINT+1                                                  
C                                                                       
      DS(IX)=A                                                          
      DO 110 J=1,NUM                                                    
         JJ=IX+J                                                        
  110    DS(JJ)=A+FLOAT(J)*(B-A)/FLOAT(NUM)                             
      DS(JJ)=B                                                          
C                                                                       
      CALL D1QDAP(F,A,B,EPS,DS(IY),DS(IX),IS(II),IS(IIDIV),             
     X    ANS,ERREST,KWARN)                                             
C                                                                       
      CALL LEAVE                                                        
      RETURN                                                            
C                                                                       
  120 KWARN = NSHORT                                                    
      ANS = 0.                                                          
      ERREST = DLARGE                                                   
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE D1QDAP(F,A,B,EPS,Y,X,I,IDIV,ANSWER,ERREST,KWARN)       
C D1QDAP ATTEMPTS TO INTEGRATE THE DOUBLE-PRECISION FUNCTION F(X) FROM  
C X=A TO X=B, WITH ABSOLUTE ACCURACY EPS, USING NO MORE THAN            
C MAXF FUNCTION CALLS.                                                  
C Y, X, I, IDIV ARE SCRATCH ARRAYS WITH ASSUMED LENGTHS MAXY,           
C MAXINT+1, MAXINT, AND MAXINT.                                         
C THE RESULT IS RETURNED IN ANSWER, AND THE ESTIMATED ERROR IN ERREST.  
C KWARN IS AN ERROR FLAG (SEE BELOW).                                   
C                                                                       
C IF THERE APPEARS TO BE A SINGULARITY AT X=A OR X=B, MAKE AN           
C APPROPRIATE CHANGE OF VARIABLE.  THE X ARRAY HAS DIVIDED (A,B) INTO   
C NUM INTERVALS.  IF NUM .GT. 1, REVERSE THE TOP INTERVAL TO SIMPLIFY   
C CHANGING VARIABLES.  IF NUM=1, ONLY LOOK FOR SINGULARITY AT X=A.      
C                                                                       
C IF INTEGRATION FAILS ON AN INTERVAL, IT IS DIVIDED IN  HALF,          
C THE LEFT HALF STACKED, AND THE RIGHT HALF ATTEMPTED.                  
C ALL FUNCTION VALUES ARE SAVED FOR USE ON THE HALF-INTERVAL.           
C                                                                       
C CALL D1QINT TO ATTEMPT THE INTEGRATION OF EACH INTERVAL.              
C PRO-RATE THE REMAINING ERROR ACCORDING TO THE INTERVAL SIZE,          
C BUT ALLOW AT LEAST ESMALL PER INTERVAL. IF NOISE IS SENSED, ALLOW     
C AT LEAST ENOISE PER INTERVAL.                                         
C                                                                       
C JF IS RETURNED BY D1QINT AS A FLAG.  JF=0 MEANS SUCCESS, JF=-1 MEANS  
C NEED TO DIVIDE THE INTERVAL, AND JF.GT.0 HAS THE CODE BELOW.          
C                                                                       
C KWARN IS RETURNED AS A BINARY  INTEGER, WITH UP TO 6 DIGITS.          
C EACH DIGIT IS 1 IF A PARTICULAR PROBLEM OCCURRED, AND 0 OTHERWISE.    
C THE MEANINGS OF THE SIX DIGITS FOLLOW.  1 IS THE RIGHT-MOST DIGIT,    
C AND 6 THE LEFT-MOST.                                                  
C 1.  ANSWER ACCEPTED BECAUSE ERROR ESTIMATE WAS BELOW EITHER           
C     ESTIMATED ROUNDOFF OR NOISE LEVEL IN FUNCTION.                    
C 2.  ANSWER ACCEPTED BECAUSE INTERVAL WAS TOO SHORT.                   
C 3.  ANSWER ACCEPTED BECAUSE INTERVAL STACKS (X, I, IDIV)              
C     WERE TOO FULL.                                                    
C 4.  ANSWER ACCEPTED BECAUSE FUNCTION VALUE STACK (Y) WAS TOO FULL.    
C 5.  ANSWER ACCEPTED BECAUSE NEXT BETTER APPROXIMATION WOULD           
C     HAVE REQUIRED EXCEEDING MAXF FUNCTION CALLS.                      
C 6.  ERREST IS GREATER THAN EPS.                                       
C                                                                       
C ERREST IS ALWAYS REALISTIC, UNLESS D1QDAP FAILS. IF NO RELIABLE       
C ANSWER AND ERROR ESTIMATE ARE AVAILABLE, ERREST IS SET TO DLARGE.     
C                                                                       
      DIMENSION Y(9),X(5),I(5),IDIV(5),MWARN(6)                         
      COMMON /D1QCM1/ DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL,        
     X   NPRINT,NUM,MAXY,MAXINT,MAXF,KMIN,KMAX,KDIV,NMAX                
      COMMON /D1QCM2/ TROUND,HMAX,HMIN,HNOISE,NUMF,INT,JY               
      COMMON /D1QCM3/ XBOT,DELX,R(3),RL(3),NEXP,KEXT,KLAST              
      DOUBLE PRECISION F,A,B,EPS,Y,X,ANSWER,ERREST,ERR,ERROR,ANS        
      DOUBLE PRECISION DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL,ENOISE 
      DOUBLE PRECISION TROUND,HMAX,HMIN,HNOISE,SAVANS,SAVERR,TEMP,DX    
      DOUBLE PRECISION XBOT,DELX,DNOISE                                 
      EXTERNAL F                                                        
      LOGICAL LOWER                                                     
C                                                                       
      JOUT = I1MACH(2)                                                  
C                                                                       
      ANSWER = 0.                                                       
      ERREST = 0.                                                       
      KWARN = 0                                                         
      IF (B.EQ.A) RETURN                                                
C                                                                       
      DELX = B-A                                                        
      SAVANS = 0.                                                       
      JJ = NUM+1                                                        
      DO 10 J = 1,JJ                                                    
         Y(J) = F(X(J))                                                 
         SAVANS = SAVANS+Y(J)                                           
         I(J) = J                                                       
   10    IDIV(J) = 0                                                    
      INT = 1                                                           
      IF (NUM.EQ.1) GO TO 15                                            
         INT = NUM+1                                                    
         X(NUM+2) = X(NUM)                                              
         Y(NUM+2) = Y(NUM)                                              
   15 SAVANS = (SAVANS-0.5*(Y(NUM+1)+Y(1)))*DELX/FLOAT(NUM)             
      SAVERR = DLARGE                                                   
      NUMF = NUM+1                                                      
      HMAX = HSAMPL*DABS(DELX)                                          
      HMIN = HSMALL*DABS(DELX)                                          
      HNOISE = HMAX/32.                                                 
      ERROR = EPS/FLOAT(NUM)                                            
      ENOISE = 0.                                                       
      DO 20 J = 1,6                                                     
   20    MWARN(J) = 0                                                   
      XBOT = A                                                          
      NEXP = 1                                                          
      LOWER = NUM.EQ.1                                                  
      JPATT = 21                                                        
      IF (LOWER) JPATT = 0                                              
C                                                                       
C        TRY TOP INTERVAL IN STACK                                      
C                                                                       
  100 II = I(INT)                                                       
      JY = MAXY+1-II                                                    
      TROUND = DSMALL+DROUND*DABS(ANSWER)                               
      DX = DABS(X(INT+1)-X(INT))                                        
      ERROR = DMAX1(ERROR,ESMALL,ENOISE*DX)                             
      IF (NPRINT.GT.0) WRITE(JOUT,105) X(INT),X(INT+1),ERROR            
  105 FORMAT(16H0INTERVAL LIMITS,1P2D15.8,                              
     X   26H ATTEMPTED ERROR TOLERANCE, 1PD13.3)                        
C                                                                       
      CALL D1QINT(F,X(INT),X(INT+1),ERROR,Y(II),IDIV(INT),ANS,ERR,      
     X   DNOISE,JF)                                                     
C                                                                       
      JP = 2                                                            
      IF (JF.EQ.1) ENOISE = DMAX1(ENOISE,DNOISE)                        
      IF (JF.EQ.0 .OR. JF.EQ.1) JP = 1                                  
      JPATT = 10*MOD(JPATT,1000)+JP                                     
      IF (JF.GT.0) MWARN(JF) = 1                                        
      IF (JF.GT.0 .AND. NPRINT.GT.0) WRITE(JOUT,110) JF                 
  110 FORMAT(10X,7HWARNING,I2)                                          
      IF (JF.LT.0) GO TO 300                                            
      IF (ERR.GE.DLARGE) GO TO 400                                      
C                                                                       
C        ACCEPT ANSWER AND ERROR ESTIMATE FOR CURRENT INTERVAL.         
C                                                                       
      IF (LOWER) GO TO 200                                              
         ANS = -ANS                                                     
         IF (INT.GT.(NUM+1)) GO TO 200                                  
            LOWER = .TRUE.                                              
            INT = NUM                                                   
            XBOT = A                                                    
            DELX = B-A                                                  
            IF (NEXP.GT.1) HMIN = HSMALL*DABS(DELX)                     
            NEXP = 1                                                    
            JPATT = 2                                                   
C                                                                       
  200 ANSWER = ANSWER+ANS                                               
      ERREST = ERREST+ERR                                               
      IF (NPRINT.GT.0) WRITE(JOUT,205) ANS,ERR,NUMF                     
  205 FORMAT(17H SUCCESS.  ANSWER,1PD15.8,                              
     X       16H ESTIMATED ERROR,1PD13.3,                               
     X       28H SAMPLING POINTS USED SO FAR,I7)                        
      IF (INT.EQ.1) GO TO 500                                           
      INT = INT-1                                                       
      IF(     LOWER) TEMP = X(INT+1)-A                                  
      IF(.NOT.LOWER) TEMP = (B-X(INT+1))+(X(NUM)-A)                     
      ERROR = (EPS-ERREST)*DABS((X(INT+1)-X(INT))/TEMP)                 
C                                                                       
C           MAKE CHANGE OF VARIABLE, IF APPROPRIATE.                    
C                                                                       
      IF (JPATT.NE.2121 .OR. NEXP.GT.1) GO TO 100                       
         IF ((     LOWER .AND. INT.EQ.1) .OR.                           
     X       (.NOT.LOWER .AND. INT.EQ.(NUM+1)))                         
     X      CALL D1QCHG(IDIV(INT),X(INT),Y(INT),JPATT,NMAX,             
     X         DROUND,HMIN,NPRINT)                                      
         GO TO 100                                                      
C                                                                       
C        FAILED.  DIVIDE INTERVAL IN TWO.                               
C                                                                       
  300 X(INT+2) = X(INT+1)                                               
      X(INT+1) = .5*(X(INT)+X(INT+1))                                   
      IDIV(INT  ) = IDIV(INT)-1                                         
      IDIV(INT+1) = IDIV(INT)                                           
      I(INT+1) = I(INT)+3*2**IDIV(INT)                                  
      IF (NPRINT.GT.0) WRITE(JOUT,305) ANS,ERR,NUMF                     
  305 FORMAT(17H FAILURE.  ANSWER,1PD15.8,                              
     X       16H ESTIMATED ERROR,1PD13.3,                               
     X       28H SAMPLING POINTS USED SO FAR,I7)                        
      INT = INT+1                                                       
      ERROR = 0.5*ERROR                                                 
      IF(ERREST.GE.DLARGE) GO TO 100                                    
         IF (INT.EQ.2) SAVANS = ANSWER + ANS                            
         IF (INT.EQ.2) SAVERR = ERREST + ERR                            
         GO TO 100                                                      
C                                                                       
C        FAILED.  CANNOT CONTINUE.                                      
C        USE LAST GUESS FOR WHOLE INTERVAL                              
C                                                                       
  400 ERREST = SAVERR                                                   
      ANSWER = SAVANS                                                   
C                                                                       
C        PREPARE TO RETURN                                              
C                                                                       
  500 KWARN = 0                                                         
      IF (ERREST.GT.EPS) KWARN = 1                                      
      DO 510 J = 1,5                                                    
         JJ = 6-J                                                       
  510    KWARN =  2*KWARN+MWARN(JJ)                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE D1QCHG(IDIV,X,Y,JPATT,NMAX,DROUND,HMIN,NPRINT)         
C                                                                       
C USING RATIOS OF T-TABLE DIFFERENCES STORED IN RL, TRY TO RECOGNIZE    
C X**A BEHAVIOR.  FORMULAS ARE EMPIRICAL, NOT EXACT, FOR THE SEQUENCE   
C H, H/2, H/3, H/4, H/6, H/8, . . .                                     
C CHANGE OF VARIABLE PARAMETER NEXP IS CHOSEN SUCH THAT THE             
C TRANSFORMED INTEGRAL SHOULD BE ABLE TO CONVERGE IN THE THIRD          
C COLUMN OF THE T-TABLE.                                                
C                                                                       
      COMMON /D1QCM3/ XBOT,DELX,R(3),RL(3),NEXP,KEXT,KLAST              
      DOUBLE PRECISION X(5),Y(9),DROUND,XBOT,HMIN,DELX                  
      DIMENSION IDIV(5)                                                 
C                                                                       
      JOUT = I1MACH(2)                                                  
C                                                                       
      IF (AMIN1(RL(2),RL(3)) .LT. 0.1) RETURN                           
                      A2 = 2.74*ALOG(RL(2)/1.34)                        
                      A3 = -1.                                          
      IF (KLAST.EQ.4) A3 = 2.38*ALOG(RL(3)/1.57)                        
      IF (KLAST.GT.4) A3 = 2.89*ALOG(RL(3)/1.25)                        
      IF (ABS(A2-A3).GT.0.1 .OR. AMIN1(A2,A3).LT.(-0.99)) RETURN        
      NEXP=12./(2.+A2+A3)+0.5                                           
      NEXP = MAX0(1,MIN0(NEXP,NMAX))                                    
      IF (NEXP .LE. 1) RETURN                                           
         IF(NPRINT.GT.0) WRITE(JOUT,1) A2,A3,NEXP                       
    1    FORMAT(7H ALPHAS,2F6.3,26H CHANGE VARIABLES WITH N =,I4)       
         XBOT = X(1)                                                    
         DELX = X(2)-XBOT                                               
         Y(1) = 0.D0                                                    
         N = 1+3*2**IDIV(1)                                             
         Y(2) = FLOAT(NEXP)*Y(N)                                        
         IDIV(1) = 0                                                    
         HMIN = DMAX1(HMIN,DABS(DELX)*DROUND**(1./FLOAT(NEXP)))         
         JPATT = 0                                                      
         RETURN                                                         
      END                                                               
      SUBROUTINE D1QINT(F,A,B,ERR,Y,I,ANS,ERREST,DNOISE,JFAIL)          
C                                                                       
C ROMBERG QUADRATURE OF F(X) FROM A TO B, WITH ABSOLUTE ACCURACY ERR.   
C                                                                       
C ANSWER AND ESTIMATED ERROR RETURNED IN ANS AND ERREST.                
C ARRAY Y CONTAINS STACK OF F-VALUES FOR TRAPEZOIDAL RULES FROM H = B-A 
C THROUGH H = (B-A)/(3*2**(I-1)).  I AND Y ARE BOTH INPUT AND OUTPUT.   
C (IF I = 0 ON INPUT, ONLY HAVE VALUES FOR H = B-A.)                    
C JFAIL IS RETURNED AS A FLAG.  0 MEANS SUCCESS, -1 MEANS NEED          
C TO DIVIDE INTERVAL AND TRY AGAIN, AND JFAIL.GT.0 MEANS A PROBLEM.     
C IF JFAIL = 1, DNOISE MAY BE RETURNED AS AN ESTIMATE OF NOISE IN       
C THE FUNCTION F.                                                       
C                                                                       
C DIVIDE B-A INTO 1,2,3,4,6,8,12,16,...,96 INTERVALS                    
C DO TRAPEZOIDAL RULE AND CAUTIOUS RICHARDSON EXTRAPOLATION             
C                                                                       
C PARAMETERS GOVERNING EXTRAPOLATIONS                                   
C  KMIN = MINIMUM NUMBER BEFORE ANSWER IS BELIEVED  (KMIN.GE.2)         
C  KMAX = MAXIMUM NUMBER BEFORE GIVING UP,  (KMAX.LE.12)                
C  KDIV = MINIMUM NUMBER BEFORE GIVING UP IF NOT CONVERGING. (KDIV.GE.4)
C  HMAX = LARGEST TRAPEZOIDAL RULE H WHOSE ANSWER WILL BE BELIEVED.     
C  HMIN = SMALLEST H THAT WILL BE USED, APPROXIMATELY.                  
C                                                                       
C 0 .LE. I .LE. 6 ASSUMED ON INPUT.  I MAY BE INCREASED ON OUTPUT.      
C IF IT IS, NEW F-VALUES ARE CALCULATED AND STORED IN Y.                
C (THE FOLLOWING IS ASSUMED TRUE ON ENTERING, AND REMAINS TRUE ON UNSUC-
C CESSFUL EXIT, FOR OUTPUT VALUE OF I.  NOT TRUE ON SUCCESSFUL EXIT.)   
C Y(J) CONTAINS F(A+(J-1)*(B-A)/(3*2**I)), FOR THOSE JS USED IN TRAP-   
C EZOIDAL RULES THROUGH H = (B-A)/(3*2**(I-1)).  CONTENTS OF OTHER JS   
C ARE GARBAGE.  J RUNS FROM 1 THROUGH 3*2**I+1.                         
C                                                                       
C IF THE INTERVAL NEEDS TO BE SUBDIVIDED, RETURNS ARE MADE ONLY         
C AFTER 4, 6, 8, 10, OR 12 EXTRAPOLATIONS.                              
C SUCCESSFUL RETURNS CAN BE MADE AFTER KMIN OR MORE EXTRAPOLATIONS.     
C                                                                       
      COMMON /D1QCM1/ DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL,        
     X   NPRINT,NUM,MAXY,MAXINT,MAXF,KMIN,KMAX,KDIV,NMAX                
      COMMON /D1QCM2/ TROUND,HMAX,HMIN,HNOISE,NUMF,INT,JY               
      COMMON /D1QCM3/ XBOT,DELX,R(3),RL(3),NEXP,KEXT,KLAST              
      COMMON /D1QCM4/ TRAP,T(13),S(13),X(13)                            
      DOUBLE PRECISION F,A,B,ERR,Y(9),ANS,ERREST,DNOISE,DEL,H,DELTA     
      DOUBLE PRECISION DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL        
      DOUBLE PRECISION TROUND,HMAX,HMIN,HNOISE,XBOT,DELX,TRAP,T,S,X     
      DOUBLE PRECISION T0,TCUM(3),D1QFUN,TEMP1,TEMP2                    
      EXTERNAL F                                                        
      LOGICAL NCHECK,INIT,LROUND                                        
      DATA INIT/.FALSE./                                                
C                                                                       
      IF(INIT) GO TO 20                                                 
         X(1)=1.                                                        
         X(2)=4.                                                        
         X(3)=9.                                                        
         DO 10 J=4,13                                                   
   10       X(J)=4.*X(J-2)                                              
         INIT=.TRUE.                                                    
C                                                                       
   20 DO 30 J = 1,3                                                     
   30    RL(J) = R(J)                                                   
      KLAST = KEXT                                                      
      ANS = 0.                                                          
      ERREST = DLARGE                                                   
      DNOISE = 0.                                                       
      DEL = B-A                                                         
C                                                                       
C         FIRST TWO EXTRAPOLATIONS                                      
C                                                                       
      IF (I.GE.1) GO TO 40                                              
         IF(JY.LT.7) GO TO 9940                                         
         IF(NUMF+3.GT.MAXF) GO TO 9950                                  
            I = 1                                                       
            Y(3) = D1QFUN(F,(2.*A+B)/3.D0)                              
            Y(4) = D1QFUN(F,.5*(A+B))                                   
            Y(5) = D1QFUN(F,(A+2.*B)/3.D0)                              
            Y(7) = Y(2)                                                 
            NUMF = NUMF+3                                               
C                                                                       
   40 N = 3*2**I+1                                                      
      T0 = 0.5*(Y(1)+Y(N))                                              
      T(1) = DEL*T0                                                     
C                                                                       
      DO 100 KK = 1,2                                                   
         KEXT = KK                                                      
         H = DEL/FLOAT(KEXT+1)                                          
         L = (N+KEXT)/(KEXT+1)                                          
         TCUM(KEXT) = Y(L)                                              
         M = N+1-L                                                      
         IF (KEXT.EQ.2) TCUM(2) = TCUM(2)+Y(M)                          
         TRAP = H*(T0+TCUM(KEXT))                                       
         NCHECK = KEXT.LT.KMIN .OR. DABS(H).GT.HMAX                     
         CALL D1QEXT(KEXT,KDIV,NCHECK,TROUND,ANS,ERREST,R,KASYM,LROUND) 
         IF (ERREST.LE.ERR) GO TO 9900                                  
         IF (LROUND) GO TO 9910                                         
  100    CONTINUE                                                       
      TCUM(3) = TCUM(1)                                                 
C                                                                       
C         REST OF EXTRAPOLATIONS                                        
C                                                                       
      NEW = 1                                                           
      IMAX = MIN0(6,(KMAX+1)/2)                                         
      DO 200 INEW =2,IMAX                                               
C                                                                       
         II = MAX0(1,2**(I-INEW))                                       
         IF (INEW.LE.I) GO TO 120                                       
            IF ((2*N-1).GT.JY) GO TO 9940                               
            IF((NUMF+4*NEW).GT.MAXF) GO TO 9950                         
C                 HAVE USED ALL INPUT F-VALUES.                         
C                 SHIFT Y ARRAY TO MAKE ROOM FOR MORE.                  
               DO 110 J = 2,N                                           
                  L = N+2-J                                             
  110             Y(2*L-1) = Y(L)                                       
               N = 2*N-1                                                
C                                                                       
  120    DO 140 KK=1,2                                                  
            H = DEL/FLOAT(NEW*2*(KK+1))                                 
            DO 130 J = 1,NEW                                            
               L = II*(6*J*KK-(7*KK-4))+1                               
               M = N+1-L                                                
               IF (I.GE.INEW) GO TO 130                                 
                  DELTA = H*FLOAT((4*KK-2)*(J-1)+1)                     
                  Y(L) = D1QFUN(F,A+DELTA)                              
                  Y(M) = D1QFUN(F,B-DELTA)                              
                  NUMF = NUMF+2                                         
  130          TCUM(KK) = TCUM(KK)+Y(L)+Y(M)                            
            TRAP = T0+TCUM(KK)                                          
            IF(KK.EQ.2) TRAP = TRAP+TCUM(3)                             
            TRAP = H*TRAP                                               
            KEXT = KEXT+1                                               
            NCHECK = KEXT.LT.KMIN .OR. DABS(H).GT.HMAX                  
            CALL D1QEXT(KEXT,KDIV,NCHECK,TROUND,ANS,ERREST,R,KASYM,     
     X                  LROUND)                                         
            IF (ERREST.LE.ERR) GO TO 9900                               
            IF (LROUND) GO TO 9910                                      
  140       CONTINUE                                                    
         TCUM(3) = TCUM(1)                                              
         I = MAX0(I,INEW)                                               
C                                                                       
         IF (DABS(DEL).LE.HMIN) GO TO 9920                              
         IF (KEXT.GE.KDIV .AND. I.EQ.INEW                               
     X         .AND. (KASYM.LT.(KEXT-2))) GO TO 210                     
C                                                                       
  200    NEW = 2*NEW                                                    
C                                                                       
  210 IF (INT.EQ.MAXINT) GO TO 9930                                     
C                                                                       
C        CHECK FOR NOISY FUNCTION                                       
C                                                                       
      IF (DABS(H).GT.HNOISE) GO TO 9890                                 
         NS = 0                                                         
         KK = 1+3*2**I                                                  
         TEMP1 = Y(5)-2.*Y(3)+Y(1)                                      
         DNOISE = DABS(TEMP1)                                           
         DO 230 J = 7,KK,2                                              
            TEMP2 = Y(J)-2.*Y(J-2)+Y(J-4)                               
            DNOISE = DNOISE+DABS(TEMP2)                                 
            IF(DSIGN(1.D0,TEMP1).NE.DSIGN(1.D0,TEMP2)) NS = NS+1        
  230       TEMP1 = TEMP2                                               
         DNOISE = 2.*DNOISE/FLOAT(KK-3)                                 
         IF(NS.GT.2) GOTO 9910                                          
C                                                                       
 9890 JFAIL = -1                                                        
      RETURN                                                            
 9900 JFAIL = 0                                                         
      RETURN                                                            
 9910 JFAIL = 1                                                         
      RETURN                                                            
 9920 JFAIL = 2                                                         
      RETURN                                                            
 9930 JFAIL = 3                                                         
      RETURN                                                            
 9940 JFAIL = 4                                                         
      RETURN                                                            
 9950 JFAIL = 5                                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE D1QEXT(M,KDIV,NCHECK,TROUND,ANS,ERREST,R,K,LROUND)     
C                                                                       
C CAUTIOUS RICHARDSON POLYNOMIAL EXTRAPOLATION.                         
C DO MTH EXTRAPOLATION = CALCULATE (M+1)ST ROW IN T-TABLE.              
C (1 .LE. M .LE. 12 IS ASSUMED.)                                        
C NEWEST ESTIMATE IS IN TIN.  ERROR IN TIN IS ASSUMED TO BE OF          
C THE FORM SUM OF H**(GAMMA*N),N=1,2,3,...                              
C FOR THE SEQUENCE OF HS USED, VALUES OF 1/H**GAMMA ARE IN ARRAY X.     
C USUAL APPLICATION IS TO ROMBERG INTEGRATION BASED ON TRAPEZOIDAL      
C RULE--THEN GAMMA = 2.                                                 
C                                                                       
C ON ENTRY, ANS AND ERREST HAVE GUESS AT EXTRAPOLATED VALUE AND AN      
C ERROR ESTIMATE FOR IT.  IF THIS EXTRAPOLATION GENERATES A VALUE       
C WITH LOWER ESTIMATED ERROR, RETURN NEW VALUES FOR ANS AND ERREST.     
C                                                                       
C T CONTAINS ROW M OF T-TABLE ON ENTRY, AND S CONTAINS THE DIFFERENCE   
C BETWEEN ROWS M AND (M-1).  THESE ARE UPDATED BEFORE EXITING.          
C                                                                       
C DONT CHECK ERROR OF NEW VALUES IF NCHECK IS TRUE ON INPUT             
C K IS RETURNED TO INDICATE THE DEGREE OF ASYMPTOTICITY OF THE TRIANGLE.
C LROUND IS SET TRUE IF ERROR ESTIMATE IS LESS THAN GUESS AT ROUNDOFF   
C VALUES OF RATIOS OF DIFFERENCES OF T-TABLE ENTRIES ARE SAVED IN       
C ARRAY R FOR POSSIBLE USE IN CHANGING VARIABLES.                       
C                                                                       
      LOGICAL NCHECK,LROUND,NOGOOD,SKIP                                 
      COMMON /D1QCM1/ DUMMY(6),NPRINT,NDUMMY(8)                         
      COMMON /D1QCM4/ TIN,T(13),S(13),X(13)                             
      DOUBLE PRECISION TROUND,ANS,ERREST,ERR,DELTA,RDELTA,TNEW,TNEXT    
      DOUBLE PRECISION DUMMY,TIN,T,S,X                                  
      DIMENSION R(1)                                                    
      DATA TOL/0.05/                                                    
C                                                                       
      JOUT = I1MACH(2)                                                  
C                                                                       
      IF (M.EQ.1 .AND. NPRINT.GT.1) WRITE(JOUT,5) T(1)                  
    5 FORMAT(1X,1PD15.8,10X,7HT-TABLE)                                  
      SKIP = NCHECK                                                     
      TNEW = TIN                                                        
      K = 0                                                             
C                                                                       
      DO 100 J = 1,M                                                    
C                                                                       
C           TIN IS COLUMN 1 IN NEW ROW                                  
C           EXTRAPOLATE TO GET COLUMN J+1, AND ERROR IN COLUMN J.       
C                                                                       
         DELTA = TNEW-T(J)                                              
         ERR = DABS(DELTA)                                              
         JJ = M+1-J                                                     
         RDELTA = DELTA*X(JJ)/(X(M+1)-X(JJ))                            
         TNEXT = TNEW+RDELTA                                            
C                                                                       
         IF (SKIP .OR. (J.EQ.M .AND. M.EQ.2 .AND. K.LT.2)) GO TO 60     
            IF (ERR.LE.TROUND .OR.  J.EQ.M) GO TO 40                    
C                                                                       
C                 ESTIMATE ERROR IN TNEW.                               
C                 ALL PREVIOUS COLUMNS WERE ASYMPTOTIC, OR ALMOST SO.   
C                                                                       
               RTEMP = (X(JJ-1)*(X(M+1)-X(JJ))*S(J))/                   
     X                 (X(M+1)*(X(M)-X(JJ-1))*DELTA)                    
               IF (ABS(RTEMP-1.) .GT. TOL*FLOAT(J)) GO TO 20            
C                                                                       
C                   ASYMPTOTIC COLUMN. USE RUNGE ERROR ESTIMATE.        
C                                                                       
                  ERR = DABS(RDELTA)                                    
                  K = K+2                                               
                  GO TO 40                                              
C                                                                       
C                    NON-ASYMPTOTIC COLUMN. USE MORE CONSERVATIVE       
C                    ERROR ESTIMATE.                                    
C                                                                       
   20             SKIP = J.GT.1                                         
                  IF (J.EQ.1 .AND. M.LT.KDIV) GO TO 60                  
                    IF (J.EQ.1) ERR = 2.*(ERR+DABS(S(1)))               
                    NOGOOD = ABS(RTEMP-1.) .GT. TOL*FLOAT(J+4)          
                    IF(.NOT. NOGOOD) K = K+1                            
                    SKIP = SKIP .OR. NOGOOD                             
                    IF ((RTEMP.LT.0.25 .OR. RTEMP.GT.4.0)               
     X                  .AND. J.GT.1) GO TO 60                          
C                                                                       
C                 SAVE NEW VALUES IF LOWER ERROR ESTIMATE               
C                                                                       
   40          IF (ERR.GT.ERREST) GO TO 60                              
                  ANS = TNEW                                            
                  ERREST = ERR                                          
C                                                                       
C           SHIFT FOR NEXT EXTRAPOLATION. SAVE FIRST 3 RATIOS.          
C                                                                       
   60    IF (J.GT.MIN0(3,M-1)) GO TO 80                                 
            R(J) = 0.                                                   
            IF (ERR.GT.0.D0) R(J) = S(J)/DELTA                          
   80    S(J) = DELTA                                                   
         T(J) = TNEW                                                    
         TNEW = TNEXT                                                   
  100    CONTINUE                                                       
C                                                                       
      T(M+1) = TNEW                                                     
      LROUND = ERREST.LE.TROUND                                         
      MP =M+1                                                           
      IF (NPRINT.GT.1) WRITE(JOUT,15) (T(J),J = 1,MP)                   
   15 FORMAT(1X,1P8D15.8)                                               
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION D1QFUN(F,X)                             
C                                                                       
C FUNCTION TO BE INTEGRATED USING D1QDAP AND D1QINT.                    
C NEXP .GT. 1 IF CHANGE OF VARIABLE HAS BEEN MADE.                      
C                                                                       
      DOUBLE PRECISION F,X,XBOT,DELX,V                                  
      EXTERNAL F                                                        
      COMMON /D1QCM3/ XBOT,DELX,R(3),RL(3),NEXP,KEXT,KLAST              
C                                                                       
      IF (NEXP.GT.1) GO TO 10                                           
         D1QFUN = F(X)                                                  
         RETURN                                                         
C                                                                       
   10 V = ((X-XBOT)/DELX)**(NEXP-1)                                     
      D1QFUN = FLOAT(NEXP)*V*F(XBOT+V*(X-XBOT))                         
      RETURN                                                            
      END                                                               
      SUBROUTINE CSPQU(X,Y,N,XLOW,XHIGH,ANS)                            
       REAL X(N),Y(N),B(6),XLOW,XHIGH,ANS,XL,XH                         
       REAL C3SPFT                                                      
C                                                                       
C INPUTS                                                                
C N DATA PAIRS (X,Y)                                                    
C XLOW AND XHIGH ARE LIMITS OF INTEGRATION                              
C                                                                       
C OUTPUT                                                                
C CUBIC SPLINE FIT TO Y(X).                                             
C ANS IS INTEGRAL OF CUBIC SPLINE FROM XLOW TO XHIGH.                   
C                                                                       
       COMMON/CSTAK/D                                                   
       DOUBLE PRECISION D(500)                                          
       REAL R(1000)                                                     
       EQUIVALENCE (D(1),R(1))                                          
C                                                                       
C CHECK FOR ERRORS IN INPUT PARAMETERS                                  
C                                                                       
C/6S                                                                    
C      IF (N.LT.4)   CALL SETERR(                                       
C    1   35HCSPQU - MUST HAVE AT LEAST FOUR X S,35,1,2)                 
C/7S                                                                    
       IF (N.LT.4)   CALL SETERR(                                       
     1   'CSPQU - MUST HAVE AT LEAST FOUR X S',35,1,2)                  
C/                                                                      
C                                                                       
       N1 = N-1                                                         
       DO 10 J=1,N1                                                     
C/6S                                                                    
C  10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
C    1    43HCSPQU - X ARRAY MUST BE IN INCREASING ORDER,43,2,2)        
C/7S                                                                    
   10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
     1    'CSPQU - X ARRAY MUST BE IN INCREASING ORDER',43,2,2)         
C/                                                                      
C                                                                       
       XL=AMIN1(XLOW,XHIGH)                                             
       XH=AMAX1(XLOW,XHIGH)                                             
C/6S                                                                    
C      IF (XL .LT.  X(1) .OR. XH .GT. X(N)) CALL SETERR(                
C    1    35HCSPQU -  LIMITS NOT INSIDE INTERVAL,35,3,2)                
C/7S                                                                    
       IF (XL .LT.  X(1) .OR. XH .GT. X(N)) CALL SETERR(                
     1    'CSPQU -  LIMITS NOT INSIDE INTERVAL',35,3,2)                 
C/                                                                      
C                                                                       
C BOUNDARY CONDITIONS - APPROXIMATE SECOND DERIVATIVES                  
C                                                                       
       B(1)=0.                                                          
       B(2)=1.                                                          
       B(3)=C3SPFT(X(1),Y(1),X(1))                                      
       B(4)=0.                                                          
       B(5)=1.                                                          
       B(6)=C3SPFT(X(N-3),Y(N-3),X(N))                                  
C                                                                       
C SPACE FOR PARAMETERS OF SPLINE                                        
C                                                                       
C/6S                                                                    
C      IF (ISTKQU(3) .LT. 2*N) CALL SETERR(                             
C    1   46HCSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,46,4,2)      
C/7S                                                                    
       IF (ISTKQU(3) .LT. 2*N) CALL SETERR(                             
     1   'CSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',46,4,2)       
C/                                                                      
       IYP=ISTKGT(2*N,3)                                                
       IYPP=IYP+N                                                       
C                                                                       
C CHECK IF HAVE ENOUGH SPACE FOR CSPFI                                  
C                                                                       
C/6S                                                                    
C      IF (ISTKQU(3) .LT. 4*N) CALL SETERR(                             
C    1   46HCSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,46,4,2)      
C/7S                                                                    
       IF (ISTKQU(3) .LT. 4*N) CALL SETERR(                             
     1   'CSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',46,4,2)       
C/                                                                      
C                                                                       
C FIT THE SPLINE                                                        
C                                                                       
       CALL CSPFI(X,Y,N,B,R(IYP),R(IYPP))                               
C                                                                       
C                                                                       
C DO THE INTEGRATION                                                    
C                                                                       
       CALL C2SPQU(X,Y,R(IYP),R(IYPP),N,XL,XH,ANS)                      
       IF (XLOW .GT. XHIGH) ANS = -ANS                                  
C                                                                       
       CALL ISTKRL(1)                                                   
       RETURN                                                           
       END                                                              
      SUBROUTINE DCSPQU(X,Y,N,XLOW,XHIGH,ANS)                           
       DOUBLE PRECISION X(N),Y(N),B(6),XLOW,XHIGH,ANS,XL,XH             
       DOUBLE PRECISION DC3SPF                                          
C                                                                       
C INPUTS                                                                
C N DATA PAIRS (X,Y)                                                    
C XLOW AND XHIGH ARE LIMITS OF INTEGRATION                              
C                                                                       
C OUTPUT                                                                
C CUBIC SPLINE FIT TO Y(X).                                             
C ANS IS INTEGRAL OF CUBIC SPLINE FROM XLOW TO XHIGH.                   
C                                                                       
       COMMON/CSTAK/D                                                   
       DOUBLE PRECISION D(500)                                          
C                                                                       
C CHECK FOR ERRORS IN INPUT PARAMETERS                                  
C                                                                       
C/6S                                                                    
C      IF (N.LT.4)   CALL SETERR(                                       
C    1   36HDCSPQU - MUST HAVE AT LEAST FOUR X S,36,1,2)                
C/7S                                                                    
       IF (N.LT.4)   CALL SETERR(                                       
     1   'DCSPQU - MUST HAVE AT LEAST FOUR X S',36,1,2)                 
C/                                                                      
C                                                                       
       N1 = N-1                                                         
       DO 10 J=1,N1                                                     
C/6S                                                                    
C  10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
C    1    44HDCSPQU - X ARRAY MUST BE IN INCREASING ORDER,44,2,2)       
C/7S                                                                    
   10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
     1    'DCSPQU - X ARRAY MUST BE IN INCREASING ORDER',44,2,2)        
C/                                                                      
C                                                                       
       XL=DMIN1(XLOW,XHIGH)                                             
       XH=DMAX1(XLOW,XHIGH)                                             
C/6S                                                                    
C      IF (XL .LT.  X(1) .OR. XH .GT. X(N)) CALL SETERR(                
C    1    36HDCSPQU -  LIMITS NOT INSIDE INTERVAL,36,3,2)               
C/7S                                                                    
       IF (XL .LT.  X(1) .OR. XH .GT. X(N)) CALL SETERR(                
     1    'DCSPQU -  LIMITS NOT INSIDE INTERVAL',36,3,2)                
C/                                                                      
C                                                                       
C BOUNDARY CONDITIONS - APPROXIMATE SECOND DERIVATIVES                  
C                                                                       
       B(1)=0.                                                          
       B(2)=1.                                                          
       B(3)=DC3SPF(X(1),Y(1),X(1))                                      
       B(4)=0.                                                          
       B(5)=1.                                                          
       B(6)=DC3SPF(X(N-3),Y(N-3),X(N))                                  
C                                                                       
C SPACE FOR PARAMETERS OF SPLINE                                        
C                                                                       
C/6S                                                                    
C      IF (ISTKQU(4) .LT. 2*N) CALL SETERR(                             
C    1   47HDCSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,47,4,2)     
C/7S                                                                    
       IF (ISTKQU(4) .LT. 2*N) CALL SETERR(                             
     1   'DCSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',47,4,2)      
C/                                                                      
       IYP=ISTKGT(2*N,4)                                                
       IYPP=IYP+N                                                       
C                                                                       
C CHECK IF HAVE ENOUGH SPACE FOR DCSPFI                                 
C                                                                       
C/6S                                                                    
C      IF (ISTKQU(4) .LT. 4*N) CALL SETERR(                             
C    1   47HDCSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,47,4,2)     
C/7S                                                                    
       IF (ISTKQU(4) .LT. 4*N) CALL SETERR(                             
     1   'DCSPQU - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',47,4,2)      
C/                                                                      
C                                                                       
C FIT THE SPLINE                                                        
C                                                                       
       CALL DCSPFI(X,Y,N,B,D(IYP),D(IYPP))                              
C                                                                       
C                                                                       
C DO THE INTEGRATION                                                    
C                                                                       
       CALL DC2SPQ(X,Y,D(IYP),D(IYPP),N,XL,XH,ANS)                      
       IF (XLOW .GT. XHIGH) ANS = -ANS                                  
C                                                                       
       CALL ISTKRL(1)                                                   
       RETURN                                                           
       END                                                              
C****END OF ROUTINES NEEDED FOR PORT 3 QUADRATURE CHAPTER***************
