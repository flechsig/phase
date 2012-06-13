      SUBROUTINE DV7PRM(N, IP, X)                                       
C                                                                       
C     PERMUTE X SO THAT X.OUTPUT(IP(I)) = X.INPUT(I).                   
C     IP IS UNCHANGED ON OUTPUT.                                        
C                                                                       
      INTEGER N                                                         
      INTEGER IP(N)                                                     
      DOUBLE PRECISION X(N)                                             
C                                                                       
      INTEGER I, J, K                                                   
      DOUBLE PRECISION S, T                                             
      DO 30 I = 1, N                                                    
         J = IP(I)                                                      
         IF (J .EQ. I) GO TO 30                                         
         IF (J .GT. 0) GO TO 10                                         
            IP(I) = -J                                                  
            GO TO 30                                                    
 10      T = X(I)                                                       
 20      S = X(J)                                                       
         X(J) = T                                                       
         T = S                                                          
         K = J                                                          
         J = IP(K)                                                      
         IP(K) = -J                                                     
         IF (J .GT. I) GO TO 20                                         
         X(J) = T                                                       
 30      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DV7PRM FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DS7CPR(C, IV, L, LIV)                                  
C                                                                       
C  ***  PRINT C FOR   DNSG (ETC.)  ***                                  
C                                                                       
      INTEGER L, LIV                                                    
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION C(L)                                             
C                                                                       
      INTEGER I, PU                                                     
C                                                                       
      INTEGER PRUNIT, SOLPRT                                            
C                                                                       
C/6                                                                     
C     DATA PRUNIT/21/, SOLPRT/22/                                       
C/7                                                                     
      PARAMETER (PRUNIT=21, SOLPRT=22)                                  
C/                                                                      
C  ***  BODY  ***                                                       
C                                                                       
      IF (IV(1) .GT. 11) GO TO 999                                      
      IF (IV(SOLPRT) .EQ. 0) GO TO 999                                  
      PU = IV(PRUNIT)                                                   
      IF (PU .EQ. 0) GO TO 999                                          
      IF (L .GT. 0) WRITE(PU,10) (I, C(I), I = 1, L)                    
 10   FORMAT(/21H LINEAR PARAMETERS...//(1X,I5,D16.6))                  
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DS7CPR FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DRN2GB(B, D, DR, IV, LIV, LV, N, ND, N1, N2, P, R,     
     1                  RD, V, X)                                       
C                                                                       
C  ***  REVISED ITERATION DRIVER FOR NL2SOL WITH SIMPLE BOUNDS  ***     
C                                                                       
      INTEGER LIV, LV, N, ND, N1, N2, P                                 
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION B(2,P), D(P), DR(ND,P), R(ND), RD(ND), V(LV),    
     1                 X(P)                                             
C                                                                       
C--------------------------  PARAMETER USAGE  --------------------------
C                                                                       
C B........ BOUNDS ON X.                                                
C D........ SCALE VECTOR.                                               
C DR....... DERIVATIVES OF R AT X.                                      
C IV....... INTEGER VALUES ARRAY.                                       
C LIV...... LENGTH OF IV... LIV MUST BE AT LEAST P + 80.                
C LV....... LENGTH OF V...  LV  MUST BE AT LEAST 105 + P*(2*P+16).      
C N........ TOTAL NUMBER OF RESIDUALS.                                  
C ND....... MAX. NO. OF RESIDUALS PASSED ON ONE CALL.                   
C N1....... LOWEST  ROW INDEX FOR RESIDUALS SUPPLIED THIS TIME.         
C N2....... HIGHEST ROW INDEX FOR RESIDUALS SUPPLIED THIS TIME.         
C P........ NUMBER OF PARAMETERS (COMPONENTS OF X) BEING ESTIMATED.     
C R........ RESIDUALS.                                                  
C V........ FLOATING-POINT VALUES ARRAY.                                
C X........ PARAMETER VECTOR BEING ESTIMATED (INPUT = INITIAL GUESS,    
C             OUTPUT = BEST VALUE FOUND).                               
C                                                                       
C  ***  DISCUSSION  ***                                                 
C                                                                       
C     THIS ROUTINE CARRIES OUT ITERATIONS FOR SOLVING NONLINEAR         
C  LEAST SQUARES PROBLEMS.  IT IS SIMILAR TO  DRN2G, EXCEPT THAT        
C  THIS ROUTINE ENFORCES THE BOUNDS  B(1,I) .LE. X(I) .LE. B(2,I),      
C  I = 1(1)P.                                                           
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY.                                            
C                                                                       
C+++++++++++++++++++++++++++++  DECLARATIONS  ++++++++++++++++++++++++++
C                                                                       
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      DOUBLE PRECISION DD7TPR, DV2NRM                                   
      EXTERNAL DIVSET, DD7TPR,DD7UPD, DG7ITB,DITSUM,DL7VML, DQ7APL,     
     1        DQ7RAD, DR7TVM,DV7CPY, DV7SCP, DV2NRM                     
C                                                                       
C DIVSET.... PROVIDES DEFAULT IV AND V INPUT COMPONENTS.                
C DD7TPR... COMPUTES INNER PRODUCT OF TWO VECTORS.                      
C DD7UPD...  UPDATES SCALE VECTOR D.                                    
C DG7ITB... PERFORMS BASIC MINIMIZATION ALGORITHM.                      
C DITSUM.... PRINTS ITERATION SUMMARY, INFO ABOUT INITIAL AND FINAL X.  
C DL7VML.... COMPUTES L * V, V = VECTOR, L = LOWER TRIANGULAR MATRIX.   
C DQ7APL... APPLIES QR TRANSFORMATIONS STORED BY DQ7RAD.                
C DQ7RAD.... ADDS A NEW BLOCK OF ROWS TO QR DECOMPOSITION.              
C DR7TVM... MULT. VECTOR BY TRANS. OF UPPER TRIANG. MATRIX FROM QR FACT.
C DV7CPY.... COPIES ONE VECTOR TO ANOTHER.                              
C DV7SCP... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.                  
C DV2NRM... RETURNS THE 2-NORM OF A VECTOR.                             
C                                                                       
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER G1, GI, I, IV1, IVMODE, JTOL1, L, LH, NN, QTR1,           
     1        RD1, RMAT1, YI, Y1                                        
      DOUBLE PRECISION T                                                
C                                                                       
      DOUBLE PRECISION HALF, ZERO                                       
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER DINIT, DTYPE, DTINIT, D0INIT, F, G, JCN, JTOL, MODE,      
     1        NEXTV, NF0, NF00, NF1, NFCALL, NFCOV, NFGCAL, QTR, RDREQ, 
     1        REGD, RESTOR, RLIMIT, RMAT, TOOBIG, VNEED                 
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C/6                                                                     
C     DATA DTYPE/16/, G/28/, JCN/66/, JTOL/59/, MODE/35/, NEXTV/47/,    
C    1     NF0/68/, NF00/81/, NF1/69/, NFCALL/6/, NFCOV/52/, NFGCAL/7/, 
C    2     QTR/77/, RDREQ/57/, RESTOR/9/, REGD/67/, RMAT/78/, TOOBIG/2/,
C    3     VNEED/4/                                                     
C/7                                                                     
      PARAMETER (DTYPE=16, G=28, JCN=66, JTOL=59, MODE=35, NEXTV=47,    
     1           NF0=68, NF00=81, NF1=69, NFCALL=6, NFCOV=52, NFGCAL=7, 
     2           QTR=77, RDREQ=57, RESTOR=9, REGD=67, RMAT=78, TOOBIG=2,
     3           VNEED=4)                                               
C/                                                                      
C                                                                       
C  ***  V SUBSCRIPT VALUES  ***                                         
C                                                                       
C/6                                                                     
C     DATA DINIT/38/, DTINIT/39/, D0INIT/40/, F/10/, RLIMIT/46/         
C/7                                                                     
      PARAMETER (DINIT=38, DTINIT=39, D0INIT=40, F=10, RLIMIT=46)       
C/                                                                      
C/6                                                                     
C     DATA HALF/0.5D+0/, ZERO/0.D+0/                                    
C/7                                                                     
      PARAMETER (HALF=0.5D+0, ZERO=0.D+0)                               
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      LH = P * (P+1) / 2                                                
      NN = N2 - N1 + 1                                                  
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)                  
      IV1 = IV(1)                                                       
      IF (IV1 .GT. 2) GO TO 10                                          
         IV(RESTOR) = 0                                                 
         I = IV1 + 4                                                    
         IF (IV(TOOBIG) .EQ. 0) GO TO (150, 130, 150, 120, 120, 150), I 
         IF (I .NE. 5) IV(1) = 2                                        
         GO TO 40                                                       
C                                                                       
C  ***  FRESH START OR RESTART -- CHECK INPUT INTEGERS  ***             
C                                                                       
 10   IF (ND .LE. 0) GO TO 220                                          
      IF (P .LE. 0) GO TO 220                                           
      IF (N .LE. 0) GO TO 220                                           
      IF (IV1 .EQ. 14) GO TO 30                                         
      IF (IV1 .GT. 16) GO TO 270                                        
      IF (IV1 .LT. 12) GO TO 40                                         
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      IF (IV(1) .NE. 13) GO TO 20                                       
      IV(VNEED) = IV(VNEED) + P*(P+15)/2                                
 20   CALL DG7ITB(B, D, X, IV, LIV, LV, P, P, V, X, X)                  
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(G) = IV(NEXTV)                                                 
      IV(JCN) = IV(G) + 2*P                                             
      IV(RMAT) = IV(JCN) + P                                            
      IV(QTR) = IV(RMAT) + LH                                           
      IV(JTOL) = IV(QTR) + 2*P                                          
      IV(NEXTV) = IV(JTOL) + 2*P                                        
C  ***  TURN OFF COVARIANCE COMPUTATION  ***                            
      IV(RDREQ) = 0                                                     
      IF (IV1 .EQ. 13) GO TO 999                                        
C                                                                       
 30   JTOL1 = IV(JTOL)                                                  
      IF (V(DINIT) .GE. ZERO) CALL DV7SCP(P, D, V(DINIT))               
      IF (V(DTINIT) .GT. ZERO) CALL DV7SCP(P, V(JTOL1), V(DTINIT))      
      I = JTOL1 + P                                                     
      IF (V(D0INIT) .GT. ZERO) CALL DV7SCP(P, V(I), V(D0INIT))          
      IV(NF0) = 0                                                       
      IV(NF1) = 0                                                       
      IF (ND .GE. N) GO TO 40                                           
C                                                                       
C  ***  SPECIAL CASE HANDLING OF FIRST FUNCTION AND GRADIENT EVALUATION 
C  ***  -- ASK FOR BOTH RESIDUAL AND JACOBIAN AT ONCE                   
C                                                                       
      G1 = IV(G)                                                        
      Y1 = G1 + P                                                       
      CALL DG7ITB(B, D, V(G1), IV, LIV, LV, P, P, V, X, V(Y1))          
      IF (IV(1) .NE. 1) GO TO 260                                       
      V(F) = ZERO                                                       
      CALL DV7SCP(P, V(G1), ZERO)                                       
      IV(1) = -1                                                        
      QTR1 = IV(QTR)                                                    
      CALL DV7SCP(P, V(QTR1), ZERO)                                     
      IV(REGD) = 0                                                      
      RMAT1 = IV(RMAT)                                                  
      GO TO 100                                                         
C                                                                       
 40   G1 = IV(G)                                                        
      Y1 = G1 + P                                                       
      CALL DG7ITB(B, D, V(G1), IV, LIV, LV, P, P, V, X, V(Y1))          
      IF (IV(1) - 2) 50, 60, 260                                        
C                                                                       
 50   V(F) = ZERO                                                       
      IF (IV(NF1) .EQ. 0) GO TO 240                                     
      IF (IV(RESTOR) .NE. 2) GO TO 240                                  
      IV(NF0) = IV(NF1)                                                 
      CALL DV7CPY(N, RD, R)                                             
      IV(REGD) = 0                                                      
      GO TO 240                                                         
C                                                                       
 60   CALL DV7SCP(P, V(G1), ZERO)                                       
      IF (IV(MODE) .GT. 0) GO TO 230                                    
      RMAT1 = IV(RMAT)                                                  
      QTR1 = IV(QTR)                                                    
      RD1 = QTR1 + P                                                    
      CALL DV7SCP(P, V(QTR1), ZERO)                                     
      IV(REGD) = 0                                                      
      IF (ND .LT. N) GO TO 90                                           
      IF (N1 .NE. 1) GO TO 90                                           
      IF (IV(MODE) .LT. 0) GO TO 100                                    
      IF (IV(NF1) .EQ. IV(NFGCAL)) GO TO 70                             
         IF (IV(NF0) .NE. IV(NFGCAL)) GO TO 90                          
            CALL DV7CPY(N, R, RD)                                       
            GO TO 80                                                    
 70   CALL DV7CPY(N, RD, R)                                             
 80   CALL DQ7APL(ND, N, P, DR, RD, 0)                                  
      CALL DR7TVM(ND, P, V(Y1), V(RD1), DR, RD)                         
      IV(REGD) = 0                                                      
      GO TO 110                                                         
C                                                                       
 90   IV(1) = -2                                                        
      IF (IV(MODE) .LT. 0) IV(1) = -3                                   
 100  CALL DV7SCP(P, V(Y1), ZERO)                                       
 110  CALL DV7SCP(LH, V(RMAT1), ZERO)                                   
      GO TO 240                                                         
C                                                                       
C  ***  COMPUTE F(X)  ***                                               
C                                                                       
 120  T = DV2NRM(NN, R)                                                 
      IF (T .GT. V(RLIMIT)) GO TO 210                                   
      V(F) = V(F)  +  HALF * T**2                                       
      IF (N2 .LT. N) GO TO 250                                          
      IF (N1 .EQ. 1) IV(NF1) = IV(NFCALL)                               
      GO TO 40                                                          
C                                                                       
C  ***  COMPUTE Y  ***                                                  
C                                                                       
 130  Y1 = IV(G) + P                                                    
      YI = Y1                                                           
      DO 140 L = 1, P                                                   
         V(YI) = V(YI) + DD7TPR(NN, DR(1,L), R)                         
         YI = YI + 1                                                    
 140     CONTINUE                                                       
      IF (N2 .LT. N) GO TO 250                                          
         IV(1) = 2                                                      
         IF (N1 .GT. 1) IV(1) = -3                                      
         GO TO 240                                                      
C                                                                       
C  ***  COMPUTE GRADIENT INFORMATION  ***                               
C                                                                       
 150  G1 = IV(G)                                                        
      IVMODE = IV(MODE)                                                 
      IF (IVMODE .LT. 0) GO TO 170                                      
      IF (IVMODE .EQ. 0) GO TO 180                                      
      IV(1) = 2                                                         
C                                                                       
C  ***  COMPUTE GRADIENT ONLY (FOR USE IN COVARIANCE COMPUTATION)  ***  
C                                                                       
      GI = G1                                                           
      DO 160 L = 1, P                                                   
         V(GI) = V(GI) + DD7TPR(NN, R, DR(1,L))                         
         GI = GI + 1                                                    
 160     CONTINUE                                                       
      GO TO 200                                                         
C                                                                       
C  *** COMPUTE INITIAL FUNCTION VALUE WHEN ND .LT. N ***                
C                                                                       
 170  IF (N .LE. ND) GO TO 180                                          
         T = DV2NRM(NN, R)                                              
         IF (T .GT. V(RLIMIT)) GO TO 210                                
         V(F) = V(F)  +  HALF * T**2                                    
C                                                                       
C  ***  UPDATE D IF DESIRED  ***                                        
C                                                                       
 180  IF (IV(DTYPE) .GT. 0)                                             
     1      CALL DD7UPD(D, DR, IV, LIV, LV, N, ND, NN, N2, P, V)        
C                                                                       
C  ***  COMPUTE RMAT AND QTR  ***                                       
C                                                                       
      QTR1 = IV(QTR)                                                    
      RMAT1 = IV(RMAT)                                                  
      CALL DQ7RAD(NN, ND, P, V(QTR1), .TRUE., V(RMAT1), DR, R)          
      IV(NF1) = 0                                                       
      IF (N1 .GT. 1) GO TO 200                                          
      IF (N2 .LT. N) GO TO 250                                          
C                                                                       
C  ***  SAVE DIAGONAL OF R FOR COMPUTING Y LATER  ***                   
C                                                                       
      RD1 = QTR1 + P                                                    
      L = RMAT1 - 1                                                     
      DO 190 I = 1, P                                                   
         L = L + I                                                      
         V(RD1) = V(L)                                                  
         RD1 = RD1 + 1                                                  
 190     CONTINUE                                                       
C                                                                       
 200  IF (N2 .LT. N) GO TO 250                                          
      IF (IVMODE .GT. 0) GO TO 40                                       
      IV(NF00) = IV(NFGCAL)                                             
C                                                                       
C  ***  COMPUTE G FROM RMAT AND QTR  ***                                
C                                                                       
      CALL DL7VML(P, V(G1), V(RMAT1), V(QTR1))                          
      IV(1) = 2                                                         
      IF (IVMODE .EQ. 0) GO TO 40                                       
      IF (N .LE. ND) GO TO 40                                           
C                                                                       
C  ***  FINISH SPECIAL CASE HANDLING OF FIRST FUNCTION AND GRADIENT     
C                                                                       
      Y1 = G1 + P                                                       
      IV(1) = 1                                                         
      CALL DG7ITB(B, D, V(G1), IV, LIV, LV, P, P, V, X, V(Y1))          
      IF (IV(1) .NE. 2) GO TO 260                                       
      GO TO 40                                                          
C                                                                       
C  ***  MISC. DETAILS  ***                                              
C                                                                       
C     ***  X IS OUT OF RANGE (OVERSIZE STEP)  ***                       
C                                                                       
 210  IV(TOOBIG) = 1                                                    
      GO TO 40                                                          
C                                                                       
C     ***  BAD N, ND, OR P  ***                                         
C                                                                       
 220  IV(1) = 66                                                        
      GO TO 270                                                         
C                                                                       
C  ***  RECORD EXTRA EVALUATIONS FOR FINITE-DIFFERENCE HESSIAN  ***     
C                                                                       
 230  IV(NFCOV) = IV(NFCOV) + 1                                         
      IV(NFCALL) = IV(NFCALL) + 1                                       
      IV(NFGCAL) = IV(NFCALL)                                           
      IV(1) = -1                                                        
C                                                                       
C  ***  RETURN FOR MORE FUNCTION OR GRADIENT INFORMATION  ***           
C                                                                       
 240  N2 = 0                                                            
 250  N1 = N2 + 1                                                       
      N2 = N2 + ND                                                      
      IF (N2 .GT. N) N2 = N                                             
      GO TO 999                                                         
C                                                                       
C  ***  PRINT SUMMARY OF FINAL ITERATION AND OTHER REQUESTED ITEMS  *** 
C                                                                       
 260  G1 = IV(G)                                                        
 270  CALL DITSUM(D, V(G1), IV, LIV, LV, P, V, X)                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DRN2GB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DC7VFN(IV, L, LH, LIV, LV, N, P, V)                    
C                                                                       
C  ***  FINISH COVARIANCE COMPUTATION FOR  DRN2G,  DRNSG  ***           
C                                                                       
      INTEGER LH, LIV, LV, N, P                                         
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION L(LH), V(LV)                                     
C                                                                       
      EXTERNAL DL7NVR, DL7TSQ, DV7SCL                                   
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER COV, I                                                    
      DOUBLE PRECISION HALF                                             
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER CNVCOD, COVMAT, F, FDH, H, MODE, RDREQ, REGD              
C                                                                       
C/6                                                                     
C     DATA CNVCOD/55/, COVMAT/26/, F/10/, FDH/74/, H/56/, MODE/35/,     
C    1     RDREQ/57/, REGD/67/                                          
C/7                                                                     
      PARAMETER (CNVCOD=55, COVMAT=26, F=10, FDH=74, H=56, MODE=35,     
     1           RDREQ=57, REGD=67)                                     
C/                                                                      
      DATA HALF/0.5D+0/                                                 
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
      IV(1) = IV(CNVCOD)                                                
      I = IV(MODE) - P                                                  
      IV(MODE) = 0                                                      
      IV(CNVCOD) = 0                                                    
      IF (IV(FDH) .LE. 0) GO TO 999                                     
      IF ((I-2)**2 .EQ. 1) IV(REGD) = 1                                 
      IF (MOD(IV(RDREQ),2) .NE. 1) GO TO 999                            
C                                                                       
C     ***  FINISH COMPUTING COVARIANCE MATRIX = INVERSE OF F.D. HESSIAN.
C                                                                       
      COV = IABS(IV(H))                                                 
      IV(FDH) = 0                                                       
C                                                                       
      IF (IV(COVMAT) .NE. 0) GO TO 999                                  
      IF (I .GE. 2) GO TO 10                                            
         CALL DL7NVR(P, V(COV), L)                                      
         CALL DL7TSQ(P, V(COV), V(COV))                                 
C                                                                       
 10   CALL DV7SCL(LH, V(COV), V(F)/(HALF * FLOAT(MAX0(1,N-P))), V(COV)) 
      IV(COVMAT) = COV                                                  
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DC7VFN FOLLOWS  ***                                
      END                                                               
        SUBROUTINE DD7DGB(B, D, DIG, DST, G, IPIV, KA, L, LV, P, PC,    
     1                    NWTST, STEP, TD, TG, V, W, X0)                
C                                                                       
C  ***  COMPUTE DOUBLE-DOGLEG STEP, SUBJECT TO SIMPLE BOUNDS ON X  ***  
C                                                                       
      INTEGER LV, KA, P, PC                                             
      INTEGER IPIV(P)                                                   
      DOUBLE PRECISION B(2,P), D(P), DIG(P), DST(P), G(P), L(1),        
     1                 NWTST(P), STEP(P), TD(P), TG(P), V(LV), W(P),    
     2                 X0(P)                                            
C                                                                       
C     DIMENSION L(P*(P+1)/2)                                            
C                                                                       
      DOUBLE PRECISION DD7TPR, DR7MDC, DV2NRM                           
      EXTERNAL DD7DOG, DD7TPR, I7SHFT, DL7ITV, DL7IVM, DL7TVM,DL7VML,   
     1         DQ7RSH, DR7MDC, DV2NRM,DV2AXY,DV7CPY, DV7IPR, DV7SCP,    
     2         DV7SHF, DV7VMP                                           
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, J, K, P1, P1M1                                         
      DOUBLE PRECISION DNWTST, GHINVG, GNORM, GNORM0, NRED, PRED, RAD,  
     1                 T, T1, T2, TI, X0I, XI                           
      DOUBLE PRECISION HALF, MEPS2, ONE, TWO, ZERO                      
C                                                                       
C  ***  V SUBSCRIPTS  ***                                               
C                                                                       
      INTEGER DGNORM, DST0, DSTNRM, GRDFAC, GTHG, GTSTEP, NREDUC,       
     1        NWTFAC, PREDUC, RADIUS, STPPAR                            
C                                                                       
C/6                                                                     
C     DATA DGNORM/1/, DST0/3/, DSTNRM/2/, GRDFAC/45/, GTHG/44/,         
C    1     GTSTEP/4/, NREDUC/6/, NWTFAC/46/, PREDUC/7/, RADIUS/8/,      
C    2     STPPAR/5/                                                    
C/7                                                                     
      PARAMETER (DGNORM=1, DST0=3, DSTNRM=2, GRDFAC=45, GTHG=44,        
     1           GTSTEP=4, NREDUC=6, NWTFAC=46, PREDUC=7, RADIUS=8,     
     2           STPPAR=5)                                              
C/                                                                      
C/6                                                                     
C     DATA HALF/0.5D+0/, ONE/1.D+0/, TWO/2.D+0/, ZERO/0.D+0/            
C/7                                                                     
      PARAMETER (HALF=0.5D+0, ONE=1.D+0, TWO=2.D+0, ZERO=0.D+0)         
      SAVE MEPS2                                                        
C/                                                                      
      DATA MEPS2/0.D+0/                                                 
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      IF (MEPS2 .LE. ZERO) MEPS2 = TWO * DR7MDC(3)                      
      GNORM0 = V(DGNORM)                                                
      V(DSTNRM) = ZERO                                                  
      IF (KA .LT. 0) GO TO 10                                           
         DNWTST = V(DST0)                                               
         NRED = V(NREDUC)                                               
 10   PRED = ZERO                                                       
      V(STPPAR) = ZERO                                                  
      RAD = V(RADIUS)                                                   
      IF (PC .GT. 0) GO TO 20                                           
         DNWTST = ZERO                                                  
         CALL DV7SCP(P, STEP, ZERO)                                     
         GO TO 140                                                      
C                                                                       
 20   P1 = PC                                                           
      CALL DV7CPY(P, TD, D)                                             
      CALL DV7IPR(P, IPIV, TD)                                          
      CALL DV7SCP(PC, DST, ZERO)                                        
      CALL DV7CPY(P, TG, G)                                             
      CALL DV7IPR(P, IPIV, TG)                                          
C                                                                       
 30   CALL DL7IVM(P1, NWTST, L, TG)                                     
      GHINVG = DD7TPR(P1, NWTST, NWTST)                                 
      V(NREDUC) = HALF * GHINVG                                         
      CALL DL7ITV(P1, NWTST, L, NWTST)                                  
      CALL DV7VMP(P1, STEP, NWTST, TD, 1)                               
      V(DST0) = DV2NRM(PC, STEP)                                        
      IF (KA .GE. 0) GO TO 40                                           
         KA = 0                                                         
         DNWTST = V(DST0)                                               
         NRED = V(NREDUC)                                               
 40   V(RADIUS) = RAD - V(DSTNRM)                                       
      IF (V(RADIUS) .LE. ZERO) GO TO 100                                
      CALL DV7VMP(P1, DIG, TG, TD, -1)                                  
      GNORM = DV2NRM(P1, DIG)                                           
      IF (GNORM .LE. ZERO) GO TO 100                                    
      V(DGNORM) = GNORM                                                 
      CALL DV7VMP(P1, DIG, DIG, TD, -1)                                 
      CALL DL7TVM(P1, W, L, DIG)                                        
      V(GTHG) = DV2NRM(P1, W)                                           
      KA = KA + 1                                                       
      CALL DD7DOG(DIG, LV, P1, NWTST, STEP, V)                          
C                                                                       
C     ***  FIND T SUCH THAT X - T*STEP IS STILL FEASIBLE.               
C                                                                       
      T = ONE                                                           
      K = 0                                                             
      DO 70 I = 1, P1                                                   
         J = IPIV(I)                                                    
         X0I = X0(J) + DST(I)/TD(I)                                     
         XI = X0I + STEP(I)                                             
         IF (XI .LT. B(1,J)) GO TO 50                                   
         IF (XI .LE. B(2,J)) GO TO 70                                   
              TI = (B(2,J) - X0I) / STEP(I)                             
              J = I                                                     
              GO TO 60                                                  
 50      TI = (B(1,J) - X0I) / STEP(I)                                  
         J = -I                                                         
 60      IF (T .LE. TI) GO TO 70                                        
              K = J                                                     
              T = TI                                                    
 70      CONTINUE                                                       
C                                                                       
C  ***  UPDATE DST, TG, AND PRED  ***                                   
C                                                                       
      CALL DV7VMP(P1, STEP, STEP, TD, 1)                                
      CALL DV2AXY(P1, DST, T, STEP, DST)                                
      V(DSTNRM) = DV2NRM(PC, DST)                                       
      T1 = T * V(GRDFAC)                                                
      T2 = T * V(NWTFAC)                                                
      PRED = PRED - T1*GNORM * ((T2 + ONE)*GNORM)                       
     1                 - T2 * (ONE + HALF*T2)*GHINVG                    
     2                  - HALF * (V(GTHG)*T1)**2                        
      IF (K .EQ. 0) GO TO 100                                           
      CALL DL7VML(P1, W, L, W)                                          
      T2 = ONE - T2                                                     
      DO 80 I = 1, P1                                                   
 80      TG(I) = T2*TG(I) - T1*W(I)                                     
C                                                                       
C     ***  PERMUTE L, ETC. IF NECESSARY  ***                            
C                                                                       
      P1M1 = P1 - 1                                                     
      J = IABS(K)                                                       
      IF (J .EQ. P1) GO TO 90                                           
         CALL DQ7RSH(J, P1, .FALSE., TG, L, W)                          
         CALL I7SHFT(P1, J, IPIV)                                       
         CALL DV7SHF(P1, J, TG)                                         
         CALL DV7SHF(P1, J, TD)                                         
         CALL DV7SHF(P1, J, DST)                                        
 90   IF (K .LT. 0) IPIV(P1) = -IPIV(P1)                                
      P1 = P1M1                                                         
      IF (P1 .GT. 0) GO TO 30                                           
C                                                                       
C     ***  UNSCALE STEP, UPDATE X AND DIHDI  ***                        
C                                                                       
 100  CALL DV7SCP(P, STEP, ZERO)                                        
      DO 110 I = 1, PC                                                  
         J = IABS(IPIV(I))                                              
         STEP(J) = DST(I) / TD(I)                                       
 110     CONTINUE                                                       
C                                                                       
C  ***  FUDGE STEP TO ENSURE THAT IT FORCES APPROPRIATE COMPONENTS      
C  ***  TO THEIR BOUNDS  ***                                            
C                                                                       
      IF (P1 .GE. PC) GO TO 140                                         
      CALL DV2AXY(P, TD, ONE, STEP, X0)                                 
      K = P1 + 1                                                        
      DO 130 I = K, PC                                                  
         J = IPIV(I)                                                    
         T = MEPS2                                                      
         IF (J .GT. 0) GO TO 120                                        
            T = -T                                                      
            J = -J                                                      
            IPIV(I) = J                                                 
 120     T = T * DMAX1(DABS(TD(J)), DABS(X0(J)))                        
         STEP(J) = STEP(J) + T                                          
 130     CONTINUE                                                       
C                                                                       
 140  V(DGNORM) = GNORM0                                                
      V(NREDUC) = NRED                                                  
      V(PREDUC) = PRED                                                  
      V(RADIUS) = RAD                                                   
      V(DST0) = DNWTST                                                  
      V(GTSTEP) = DD7TPR(P, STEP, G)                                    
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DD7DGB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DD7DOG(DIG, LV, N, NWTSTP, STEP, V)                    
C                                                                       
C  ***  COMPUTE DOUBLE DOGLEG STEP  ***                                 
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER LV, N                                                     
      DOUBLE PRECISION DIG(N), NWTSTP(N), STEP(N), V(LV)                
C                                                                       
C  ***  PURPOSE  ***                                                    
C                                                                       
C        THIS SUBROUTINE COMPUTES A CANDIDATE STEP (FOR USE IN AN UNCON-
C     STRAINED MINIMIZATION CODE) BY THE DOUBLE DOGLEG ALGORITHM OF     
C     DENNIS AND MEI (REF. 1), WHICH IS A VARIATION ON POWELL*S DOGLEG  
C     SCHEME (REF. 2, P. 95).                                           
C                                                                       
C--------------------------  PARAMETER USAGE  --------------------------
C                                                                       
C    DIG (INPUT) DIAG(D)**-2 * G -- SEE ALGORITHM NOTES.                
C      G (INPUT) THE CURRENT GRADIENT VECTOR.                           
C     LV (INPUT) LENGTH OF V.                                           
C      N (INPUT) NUMBER OF COMPONENTS IN  DIG, G, NWTSTP,  AND  STEP.   
C NWTSTP (INPUT) NEGATIVE NEWTON STEP -- SEE ALGORITHM NOTES.           
C   STEP (OUTPUT) THE COMPUTED STEP.                                    
C      V (I/O) VALUES ARRAY, THE FOLLOWING COMPONENTS OF WHICH ARE      
C             USED HERE...                                              
C V(BIAS)   (INPUT) BIAS FOR RELAXED NEWTON STEP, WHICH IS V(BIAS) OF   
C             THE WAY FROM THE FULL NEWTON TO THE FULLY RELAXED NEWTON  
C             STEP.  RECOMMENDED VALUE = 0.8 .                          
C V(DGNORM) (INPUT) 2-NORM OF DIAG(D)**-1 * G -- SEE ALGORITHM NOTES.   
C V(DSTNRM) (OUTPUT) 2-NORM OF DIAG(D) * STEP, WHICH IS V(RADIUS)       
C             UNLESS V(STPPAR) = 0 -- SEE ALGORITHM NOTES.              
C V(DST0) (INPUT) 2-NORM OF DIAG(D) * NWTSTP -- SEE ALGORITHM NOTES.    
C V(GRDFAC) (OUTPUT) THE COEFFICIENT OF  DIG  IN THE STEP RETURNED --   
C             STEP(I) = V(GRDFAC)*DIG(I) + V(NWTFAC)*NWTSTP(I).         
C V(GTHG)   (INPUT) SQUARE-ROOT OF (DIG**T) * (HESSIAN) * DIG -- SEE    
C             ALGORITHM NOTES.                                          
C V(GTSTEP) (OUTPUT) INNER PRODUCT BETWEEN G AND STEP.                  
C V(NREDUC) (OUTPUT) FUNCTION REDUCTION PREDICTED FOR THE FULL NEWTON   
C             STEP.                                                     
C V(NWTFAC) (OUTPUT) THE COEFFICIENT OF  NWTSTP  IN THE STEP RETURNED --
C             SEE V(GRDFAC) ABOVE.                                      
C V(PREDUC) (OUTPUT) FUNCTION REDUCTION PREDICTED FOR THE STEP RETURNED.
C V(RADIUS) (INPUT) THE TRUST REGION RADIUS.  D TIMES THE STEP RETURNED 
C             HAS 2-NORM V(RADIUS) UNLESS V(STPPAR) = 0.                
C V(STPPAR) (OUTPUT) CODE TELLING HOW STEP WAS COMPUTED... 0 MEANS A    
C             FULL NEWTON STEP.  BETWEEN 0 AND 1 MEANS V(STPPAR) OF THE 
C             WAY FROM THE NEWTON TO THE RELAXED NEWTON STEP.  BETWEEN  
C             1 AND 2 MEANS A TRUE DOUBLE DOGLEG STEP, V(STPPAR) - 1 OF 
C             THE WAY FROM THE RELAXED NEWTON TO THE CAUCHY STEP.       
C             GREATER THAN 2 MEANS 1 / (V(STPPAR) - 1) TIMES THE CAUCHY 
C             STEP.                                                     
C                                                                       
C-------------------------------  NOTES  -------------------------------
C                                                                       
C  ***  ALGORITHM NOTES  ***                                            
C                                                                       
C        LET  G  AND  H  BE THE CURRENT GRADIENT AND HESSIAN APPROXIMA- 
C     TION RESPECTIVELY AND LET D BE THE CURRENT SCALE VECTOR.  THIS    
C     ROUTINE ASSUMES DIG = DIAG(D)**-2 * G  AND  NWTSTP = H**-1 * G.   
C     THE STEP COMPUTED IS THE SAME ONE WOULD GET BY REPLACING G AND H  
C     BY  DIAG(D)**-1 * G  AND  DIAG(D)**-1 * H * DIAG(D)**-1,          
C     COMPUTING STEP, AND TRANSLATING STEP BACK TO THE ORIGINAL         
C     VARIABLES, I.E., PREMULTIPLYING IT BY DIAG(D)**-1.                
C                                                                       
C  ***  REFERENCES  ***                                                 
C                                                                       
C 1.  DENNIS, J.E., AND MEI, H.H.W. (1979), TWO NEW UNCONSTRAINED OPTI- 
C             MIZATION ALGORITHMS WHICH USE FUNCTION AND GRADIENT       
C             VALUES, J. OPTIM. THEORY APPLIC. 28, PP. 453-482.         
C 2. POWELL, M.J.D. (1970), A HYBRID METHOD FOR NON-LINEAR EQUATIONS,   
C             IN NUMERICAL METHODS FOR NON-LINEAR EQUATIONS, EDITED BY  
C             P. RABINOWITZ, GORDON AND BREACH, LONDON.                 
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY.                                            
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH SUPPORTED 
C     BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS MCS-7600324 AND   
C     MCS-7906671.                                                      
C                                                                       
C------------------------  EXTERNAL QUANTITIES  ------------------------
C                                                                       
C  ***  INTRINSIC FUNCTIONS  ***                                        
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
C--------------------------  LOCAL VARIABLES  --------------------------
C                                                                       
      INTEGER I                                                         
      DOUBLE PRECISION CFACT, CNORM, CTRNWT, GHINVG, FEMNSQ, GNORM,     
     1                 NWTNRM, RELAX, RLAMBD, T, T1, T2                 
      DOUBLE PRECISION HALF, ONE, TWO, ZERO                             
C                                                                       
C  ***  V SUBSCRIPTS  ***                                               
C                                                                       
      INTEGER BIAS, DGNORM, DSTNRM, DST0, GRDFAC, GTHG, GTSTEP,         
     1        NREDUC, NWTFAC, PREDUC, RADIUS, STPPAR                    
C                                                                       
C  ***  DATA INITIALIZATIONS  ***                                       
C                                                                       
C/6                                                                     
C     DATA HALF/0.5D+0/, ONE/1.D+0/, TWO/2.D+0/, ZERO/0.D+0/            
C/7                                                                     
      PARAMETER (HALF=0.5D+0, ONE=1.D+0, TWO=2.D+0, ZERO=0.D+0)         
C/                                                                      
C                                                                       
C/6                                                                     
C     DATA BIAS/43/, DGNORM/1/, DSTNRM/2/, DST0/3/, GRDFAC/45/,         
C    1     GTHG/44/, GTSTEP/4/, NREDUC/6/, NWTFAC/46/, PREDUC/7/,       
C    2     RADIUS/8/, STPPAR/5/                                         
C/7                                                                     
      PARAMETER (BIAS=43, DGNORM=1, DSTNRM=2, DST0=3, GRDFAC=45,        
     1           GTHG=44, GTSTEP=4, NREDUC=6, NWTFAC=46, PREDUC=7,      
     2           RADIUS=8, STPPAR=5)                                    
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      NWTNRM = V(DST0)                                                  
      RLAMBD = ONE                                                      
      IF (NWTNRM .GT. ZERO) RLAMBD = V(RADIUS) / NWTNRM                 
      GNORM = V(DGNORM)                                                 
      GHINVG = TWO * V(NREDUC)                                          
      V(GRDFAC) = ZERO                                                  
      V(NWTFAC) = ZERO                                                  
      IF (RLAMBD .LT. ONE) GO TO 30                                     
C                                                                       
C        ***  THE NEWTON STEP IS INSIDE THE TRUST REGION  ***           
C                                                                       
         V(STPPAR) = ZERO                                               
         V(DSTNRM) = NWTNRM                                             
         V(GTSTEP) = -GHINVG                                            
         V(PREDUC) = V(NREDUC)                                          
         V(NWTFAC) = -ONE                                               
         DO 20 I = 1, N                                                 
 20           STEP(I) = -NWTSTP(I)                                      
         GO TO 999                                                      
C                                                                       
 30   V(DSTNRM) = V(RADIUS)                                             
      CFACT = (GNORM / V(GTHG))**2                                      
C     ***  CAUCHY STEP = -CFACT * G.                                    
      CNORM = GNORM * CFACT                                             
      RELAX = ONE - V(BIAS) * (ONE - GNORM*CNORM/GHINVG)                
      IF (RLAMBD .LT. RELAX) GO TO 50                                   
C                                                                       
C        ***  STEP IS BETWEEN RELAXED NEWTON AND FULL NEWTON STEPS  *** 
C                                                                       
         V(STPPAR)  =  ONE  -  (RLAMBD - RELAX) / (ONE - RELAX)         
         T = -RLAMBD                                                    
         V(GTSTEP) = T * GHINVG                                         
         V(PREDUC) = RLAMBD * (ONE - HALF*RLAMBD) * GHINVG              
         V(NWTFAC) = T                                                  
         DO 40 I = 1, N                                                 
 40           STEP(I) = T * NWTSTP(I)                                   
         GO TO 999                                                      
C                                                                       
 50   IF (CNORM .LT. V(RADIUS)) GO TO 70                                
C                                                                       
C        ***  THE CAUCHY STEP LIES OUTSIDE THE TRUST REGION --          
C        ***  STEP = SCALED CAUCHY STEP  ***                            
C                                                                       
         T = -V(RADIUS) / GNORM                                         
         V(GRDFAC) = T                                                  
         V(STPPAR) = ONE  +  CNORM / V(RADIUS)                          
         V(GTSTEP) = -V(RADIUS) * GNORM                                 
      V(PREDUC) = V(RADIUS)*(GNORM - HALF*V(RADIUS)*(V(GTHG)/GNORM)**2) 
         DO 60 I = 1, N                                                 
 60           STEP(I) = T * DIG(I)                                      
         GO TO 999                                                      
C                                                                       
C     ***  COMPUTE DOGLEG STEP BETWEEN CAUCHY AND RELAXED NEWTON  ***   
C     ***  FEMUR = RELAXED NEWTON STEP MINUS CAUCHY STEP  ***           
C                                                                       
 70   CTRNWT = CFACT * RELAX * GHINVG / GNORM                           
C     *** CTRNWT = INNER PROD. OF CAUCHY AND RELAXED NEWTON STEPS,      
C     *** SCALED BY GNORM**-1.                                          
      T1 = CTRNWT - GNORM*CFACT**2                                      
C     ***  T1 = INNER PROD. OF FEMUR AND CAUCHY STEP, SCALED BY         
C     ***  GNORM**-1.                                                   
      T2 = V(RADIUS)*(V(RADIUS)/GNORM) - GNORM*CFACT**2                 
      T = RELAX * NWTNRM                                                
      FEMNSQ = (T/GNORM)*T - CTRNWT - T1                                
C     ***  FEMNSQ = SQUARE OF 2-NORM OF FEMUR, SCALED BY GNORM**-1.     
      T = T2 / (T1 + DSQRT(T1**2 + FEMNSQ*T2))                          
C     ***  DOGLEG STEP  =  CAUCHY STEP  +  T * FEMUR.                   
      T1 = (T - ONE) * CFACT                                            
      V(GRDFAC) = T1                                                    
      T2 = -T * RELAX                                                   
      V(NWTFAC) = T2                                                    
      V(STPPAR) = TWO - T                                               
      V(GTSTEP) = T1*GNORM**2 + T2*GHINVG                               
      V(PREDUC) = -T1*GNORM * ((T2 + ONE)*GNORM)                        
     1                 - T2 * (ONE + HALF*T2)*GHINVG                    
     2                  - HALF * (V(GTHG)*T1)**2                        
      DO 80 I = 1, N                                                    
 80      STEP(I) = T1*DIG(I) + T2*NWTSTP(I)                             
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DD7DOG FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DD7DUP(D, HDIAG, IV, LIV, LV, N, V)                    
C                                                                       
C  ***  UPDATE SCALE VECTOR D FOR  DMNH  ***                            
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER LIV, LV, N                                                
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION D(N), HDIAG(N), V(LV)                            
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER DTOLI, D0I, I                                             
      DOUBLE PRECISION T, VDFAC                                         
C                                                                       
C  ***  INTRINSIC FUNCTIONS  ***                                        
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER DFAC, DTOL, DTYPE, NITER                                  
C/6                                                                     
C     DATA DFAC/41/, DTOL/59/, DTYPE/16/, NITER/31/                     
C/7                                                                     
      PARAMETER (DFAC=41, DTOL=59, DTYPE=16, NITER=31)                  
C/                                                                      
C                                                                       
C-------------------------------  BODY  --------------------------------
C                                                                       
      I = IV(DTYPE)                                                     
      IF (I .EQ. 1) GO TO 10                                            
         IF (IV(NITER) .GT. 0) GO TO 999                                
C                                                                       
 10   DTOLI = IV(DTOL)                                                  
      D0I = DTOLI + N                                                   
      VDFAC = V(DFAC)                                                   
      DO 20 I = 1, N                                                    
         T = DMAX1(DSQRT(DABS(HDIAG(I))), VDFAC*D(I))                   
         IF (T .LT. V(DTOLI)) T = DMAX1(V(DTOLI), V(D0I))               
         D(I) = T                                                       
         DTOLI = DTOLI + 1                                              
         D0I = D0I + 1                                                  
 20      CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DD7DUP FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DD7UPD(D, DR, IV, LIV, LV, N, ND, NN, N2, P, V)        
C                                                                       
C  ***  UPDATE SCALE VECTOR D FOR NL2IT  ***                            
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER LIV, LV, N, ND, NN, N2, P                                 
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION D(P), DR(ND,P), V(LV)                            
C     DIMENSION V(*)                                                    
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER D0, I, JCN0, JCN1, JCNI, JTOL0, JTOLI, K, SII             
      DOUBLE PRECISION T, VDFAC                                         
C                                                                       
C     ***  CONSTANTS  ***                                               
C                                                                       
      DOUBLE PRECISION ZERO                                             
C                                                                       
C  ***  INTRINSIC FUNCTIONS  ***                                        
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
C  ***  EXTERNAL SUBROUTINE  ***                                        
C                                                                       
      EXTERNAL DV7SCP                                                   
C                                                                       
C DV7SCP... SETS ALL COMPONENTS OF A VECTOR TO A SCALAR.                
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER DFAC, DTYPE, JCN, JTOL, NITER, S                          
C/6                                                                     
C     DATA DFAC/41/, DTYPE/16/, JCN/66/, JTOL/59/, NITER/31/, S/62/     
C/7                                                                     
      PARAMETER (DFAC=41, DTYPE=16, JCN=66, JTOL=59, NITER=31, S=62)    
C/                                                                      
C                                                                       
C/6                                                                     
C     DATA ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.D+0)                                            
C/                                                                      
C                                                                       
C-------------------------------  BODY  --------------------------------
C                                                                       
      IF (IV(DTYPE) .NE. 1 .AND. IV(NITER) .GT. 0) GO TO 999            
      JCN1 = IV(JCN)                                                    
      JCN0 = IABS(JCN1) - 1                                             
      IF (JCN1 .LT. 0) GO TO 10                                         
         IV(JCN) = -JCN1                                                
         CALL DV7SCP(P, V(JCN1), ZERO)                                  
 10   DO 30 I = 1, P                                                    
         JCNI = JCN0 + I                                                
         T  = V(JCNI)                                                   
         DO 20 K = 1, NN                                                
 20           T = DMAX1(T, DABS(DR(K,I)))                               
         V(JCNI) = T                                                    
 30      CONTINUE                                                       
      IF (N2 .LT. N) GO TO 999                                          
      VDFAC = V(DFAC)                                                   
      JTOL0 = IV(JTOL) - 1                                              
      D0 = JTOL0 + P                                                    
      SII = IV(S) - 1                                                   
      DO 50 I = 1, P                                                    
         SII = SII + I                                                  
         JCNI = JCN0 + I                                                
         T = V(JCNI)                                                    
         IF (V(SII) .GT. ZERO) T = DMAX1(DSQRT(V(SII)), T)              
         JTOLI = JTOL0 + I                                              
         D0 = D0 + 1                                                    
         IF (T .LT. V(JTOLI)) T = DMAX1(V(D0), V(JTOLI))                
         D(I) = DMAX1(VDFAC*D(I), T)                                    
 50      CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DD7UPD FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DG7ITB(B, D, G, IV, LIV, LV, P, PS, V, X, Y)           
C                                                                       
C  ***  CARRY OUT NL2SOL-LIKE ITERATIONS FOR GENERALIZED LINEAR   ***   
C  ***  REGRESSION PROBLEMS (AND OTHERS OF SIMILAR STRUCTURE)     ***   
C  ***  HAVING SIMPLE BOUNDS ON THE PARAMETERS BEING ESTIMATED.   ***   
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER LIV, LV, P, PS                                            
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION B(2,P), D(P), G(P), V(LV), X(P), Y(P)            
C                                                                       
C--------------------------  PARAMETER USAGE  --------------------------
C                                                                       
C B.... VECTOR OF LOWER AND UPPER BOUNDS ON X.                          
C D.... SCALE VECTOR.                                                   
C IV... INTEGER VALUE ARRAY.                                            
C LIV.. LENGTH OF IV.  MUST BE AT LEAST 80.                             
C LH... LENGTH OF H = P*(P+1)/2.                                        
C LV... LENGTH OF V.  MUST BE AT LEAST P*(3*P + 19)/2 + 7.              
C G.... GRADIENT AT X (WHEN IV(1) = 2).                                 
C HC... GAUSS-NEWTON HESSIAN AT X (WHEN IV(1) = 2).                     
C P.... NUMBER OF PARAMETERS (COMPONENTS IN X).                         
C PS... NUMBER OF NONZERO ROWS AND COLUMNS IN S.                        
C V.... FLOATING-POINT VALUE ARRAY.                                     
C X.... PARAMETER VECTOR.                                               
C Y.... PART OF YIELD VECTOR (WHEN IV(1)= 2, SCRATCH OTHERWISE).        
C                                                                       
C  ***  DISCUSSION  ***                                                 
C                                                                       
C        DG7ITB IS SIMILAR TO DG7LIT, EXCEPT FOR THE EXTRA PARAMETER B  
C     -- DG7ITB ENFORCES THE BOUNDS  B(1,I) .LE. X(I) .LE. B(2,I),      
C     I = 1(1)P.                                                        
C        DG7ITB PERFORMS NL2SOL-LIKE ITERATIONS FOR A VARIETY OF        
C     REGRESSION PROBLEMS THAT ARE SIMILAR TO NONLINEAR LEAST-SQUARES   
C     IN THAT THE HESSIAN IS THE SUM OF TWO TERMS, A READILY-COMPUTED   
C     FIRST-ORDER TERM AND A SECOND-ORDER TERM.  THE CALLER SUPPLIES    
C     THE FIRST-ORDER TERM OF THE HESSIAN IN HC (LOWER TRIANGLE, STORED 
C     COMPACTLY BY ROWS), AND DG7ITB BUILDS AN APPROXIMATION, S, TO THE 
C     SECOND-ORDER TERM.  THE CALLER ALSO PROVIDES THE FUNCTION VALUE,  
C     GRADIENT, AND PART OF THE YIELD VECTOR USED IN UPDATING S.        
C     DG7ITB DECIDES DYNAMICALLY WHETHER OR NOT TO USE S WHEN CHOOSING  
C     THE NEXT STEP TO TRY...  THE HESSIAN APPROXIMATION USED IS EITHER 
C     HC ALONE (GAUSS-NEWTON MODEL) OR HC + S (AUGMENTED MODEL).        
C     IF PS .LT. P, THEN ROWS AND COLUMNS PS+1...P OF S ARE KEPT        
C     CONSTANT.  THEY WILL BE ZERO UNLESS THE CALLER SETS IV(INITS) TO  
C     1 OR 2 AND SUPPLIES NONZERO VALUES FOR THEM, OR THE CALLER SETS   
C     IV(INITS) TO 3 OR 4 AND THE FINITE-DIFFERENCE INITIAL S THEN      
C     COMPUTED HAS NONZERO VALUES IN THESE ROWS.                        
C                                                                       
C        IF IV(INITS) IS 3 OR 4, THEN THE INITIAL S IS COMPUTED BY      
C     FINITE DIFFERENCES.  3 MEANS USE FUNCTION DIFFERENCES, 4 MEANS    
C     USE GRADIENT DIFFERENCES.  FINITE DIFFERENCING IS DONE THE SAME   
C     WAY AS IN COMPUTING A COVARIANCE MATRIX (WITH IV(COVREQ) = -1, -2,
C     1, OR 2).                                                         
C                                                                       
C        FOR UPDATING S, DG7ITB ASSUMES THAT THE GRADIENT HAS THE FORM  
C     OF A SUM OVER I OF RHO(I,X)*GRAD(R(I,X)), WHERE GRAD DENOTES THE  
C     GRADIENT WITH RESPECT TO X.  THE TRUE SECOND-ORDER TERM THEN IS   
C     THE SUM OVER I OF RHO(I,X)*HESSIAN(R(I,X)).  IF X = X0 + STEP,    
C     THEN WE WISH TO UPDATE S SO THAT S*STEP IS THE SUM OVER I OF      
C     RHO(I,X)*(GRAD(R(I,X)) - GRAD(R(I,X0))).  THE CALLER MUST SUPPLY  
C     PART OF THIS IN Y, NAMELY THE SUM OVER I OF                       
C     RHO(I,X)*GRAD(R(I,X0)), WHEN CALLING DG7ITB WITH IV(1) = 2 AND    
C     IV(MODE) = 0 (WHERE MODE = 38).  G THEN CONTANS THE OTHER PART,   
C     SO THAT THE DESIRED YIELD VECTOR IS G - Y.  IF PS .LT. P, THEN    
C     THE ABOVE DISCUSSION APPLIES ONLY TO THE FIRST PS COMPONENTS OF   
C     GRAD(R(I,X)), STEP, AND Y.                                        
C                                                                       
C        PARAMETERS IV, P, V, AND X ARE THE SAME AS THE CORRESPONDING   
C     ONES TO  DN2GB (AND NL2SOL), EXCEPT THAT V CAN BE SHORTER         
C     (SINCE THE PART OF V THAT  DN2GB USES FOR STORING D, J, AND R IS  
C     NOT NEEDED).  MOREOVER, COMPARED WITH  DN2GB (AND NL2SOL), IV(1)  
C     MAY HAVE THE TWO ADDITIONAL OUTPUT VALUES 1 AND 2, WHICH ARE      
C     EXPLAINED BELOW, AS IS THE USE OF IV(TOOBIG) AND IV(NFGCAL).      
C     THE VALUES IV(D), IV(J), AND IV(R), WHICH ARE OUTPUT VALUES FROM  
C      DN2GB (AND  DN2FB), ARE NOT REFERENCED BY DG7ITB OR THE          
C     SUBROUTINES IT CALLS.                                             
C                                                                       
C        WHEN DG7ITB IS FIRST CALLED, I.E., WHEN DG7ITB IS CALLED WITH  
C     IV(1) = 0 OR 12, V(F), G, AND HC NEED NOT BE INITIALIZED.  TO     
C     OBTAIN THESE STARTING VALUES, DG7ITB RETURNS FIRST WITH IV(1) = 1,
C     THEN WITH IV(1) = 2, WITH IV(MODE) = -1 IN BOTH CASES.  ON        
C     SUBSEQUENT RETURNS WITH IV(1) = 2, IV(MODE) = 0 IMPLIES THAT      
C     Y MUST ALSO BE SUPPLIED.  (NOTE THAT Y IS USED FOR SCRATCH -- ITS 
C     INPUT CONTENTS ARE LOST.  BY CONTRAST, HC IS NEVER CHANGED.)      
C     ONCE CONVERGENCE HAS BEEN OBTAINED, IV(RDREQ) AND IV(COVREQ) MAY  
C     IMPLY THAT A FINITE-DIFFERENCE HESSIAN SHOULD BE COMPUTED FOR USE 
C     IN COMPUTING A COVARIANCE MATRIX.  IN THIS CASE DG7ITB WILL MAKE  
C     A NUMBER OF RETURNS WITH IV(1) = 1 OR 2 AND IV(MODE) POSITIVE.    
C     WHEN IV(MODE) IS POSITIVE, Y SHOULD NOT BE CHANGED.               
C                                                                       
C IV(1) = 1 MEANS THE CALLER SHOULD SET V(F) (I.E., V(10)) TO F(X), THE 
C             FUNCTION VALUE AT X, AND CALL DG7ITB AGAIN, HAVING CHANGED
C             NONE OF THE OTHER PARAMETERS.  AN EXCEPTION OCCURS IF F(X)
C             CANNOT BE EVALUATED (E.G. IF OVERFLOW WOULD OCCUR), WHICH 
C             MAY HAPPEN BECAUSE OF AN OVERSIZED STEP.  IN THIS CASE    
C             THE CALLER SHOULD SET IV(TOOBIG) = IV(2) TO 1, WHICH WILL 
C             CAUSE DG7ITB TO IGNORE V(F) AND TRY A SMALLER STEP.  NOTE 
C             THAT THE CURRENT FUNCTION EVALUATION COUNT IS AVAILABLE   
C             IN IV(NFCALL) = IV(6).  THIS MAY BE USED TO IDENTIFY      
C             WHICH COPY OF SAVED INFORMATION SHOULD BE USED IN COM-    
C             PUTING G, HC, AND Y THE NEXT TIME DG7ITB RETURNS WITH     
C             IV(1) = 2.  SEE MLPIT FOR AN EXAMPLE OF THIS.             
C IV(1) = 2 MEANS THE CALLER SHOULD SET G TO G(X), THE GRADIENT OF F AT 
C             X.  THE CALLER SHOULD ALSO SET HC TO THE GAUSS-NEWTON     
C             HESSIAN AT X.  IF IV(MODE) = 0, THEN THE CALLER SHOULD    
C             ALSO COMPUTE THE PART OF THE YIELD VECTOR DESCRIBED ABOVE.
C             THE CALLER SHOULD THEN CALL DG7ITB AGAIN (WITH IV(1) = 2).
C             THE CALLER MAY ALSO CHANGE D AT THIS TIME, BUT SHOULD NOT 
C             CHANGE X.  NOTE THAT IV(NFGCAL) = IV(7) CONTAINS THE      
C             VALUE THAT IV(NFCALL) HAD DURING THE RETURN WITH          
C             IV(1) = 1 IN WHICH X HAD THE SAME VALUE AS IT NOW HAS.    
C             IV(NFGCAL) IS EITHER IV(NFCALL) OR IV(NFCALL) - 1.  MLPIT 
C             IS AN EXAMPLE WHERE THIS INFORMATION IS USED.  IF G OR HC 
C             CANNOT BE EVALUATED AT X, THEN THE CALLER MAY SET         
C             IV(NFGCAL) TO 0, IN WHICH CASE DG7ITB WILL RETURN WITH    
C             IV(1) = 15.                                               
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY.                                            
C                                                                       
C        (SEE NL2SOL FOR REFERENCES.)                                   
C                                                                       
C+++++++++++++++++++++++++++  DECLARATIONS  ++++++++++++++++++++++++++++
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      LOGICAL HAVQTR, HAVRM                                             
      INTEGER DUMMY, DIG1, G01, H1, HC1, I, I1, IPI, IPIV0, IPIV1,      
     1        IPIV2, IPN, J, K, L, LMAT1, LSTGST, P1, P1LEN, PP1, PP1O2,
     2        QTR1, RMAT1, RSTRST, STEP1, STPMOD, S1, TD1, TEMP1, TEMP2,
     3        TG1, W1, WLM1, X01                                        
      DOUBLE PRECISION E, GI, STTSST, T, T1, XI                         
C                                                                       
C     ***  CONSTANTS  ***                                               
C                                                                       
      DOUBLE PRECISION HALF, NEGONE, ONE, ONEP2, ZERO                   
C                                                                       
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      LOGICAL STOPX                                                     
      DOUBLE PRECISION DD7TPR, DRLDST, DV2NRM                           
      EXTERNAL DA7SST, DD7TPR, DF7DHB, DG7QSB,I7COPY, I7PNVR, I7SHFT,   
     1        DITSUM, DL7MSB, DL7SQR, DL7TVM,DL7VML,DPARCK, DQ7RSH,     
     2         DRLDST, DS7DMP, DS7IPR, DS7LUP, DS7LVM, STOPX, DV2NRM,   
     3        DV2AXY,DV7CPY, DV7IPR, DV7SCP, DV7VMP                     
C                                                                       
C DA7SST.... ASSESSES CANDIDATE STEP.                                   
C DD7TPR... RETURNS INNER PRODUCT OF TWO VECTORS.                       
C DF7DHB... COMPUTE FINITE-DIFFERENCE HESSIAN (FOR INIT. S MATRIX).     
C DG7QSB... COMPUTES GOLDFELD-QUANDT-TROTTER STEP (AUGMENTED MODEL).    
C I7COPY.... COPIES ONE INTEGER VECTOR TO ANOTHER.                      
C I7PNVR... INVERTS PERMUTATION ARRAY.                                  
C I7SHFT... SHIFTS AN INTEGER VECTOR.                                   
C DITSUM.... PRINTS ITERATION SUMMARY AND INFO ON INITIAL AND FINAL X.  
C DL7MSB... COMPUTES LEVENBERG-MARQUARDT STEP (GAUSS-NEWTON MODEL).     
C DL7SQR... COMPUTES L * L**T FROM LOWER TRIANGULAR MATRIX L.           
C DL7TVM... COMPUTES L**T * V, V = VECTOR, L = LOWER TRIANGULAR MATRIX. 
C DL7VML.... COMPUTES L * V, V = VECTOR, L = LOWER TRIANGULAR MATRIX.   
C DPARCK.... CHECK VALIDITY OF IV AND V INPUT COMPONENTS.               
C DQ7RSH... SHIFTS A QR FACTORIZATION.                                  
C DRLDST... COMPUTES V(RELDX) = RELATIVE STEP SIZE.                     
C DS7DMP... MULTIPLIES A SYM. MATRIX FORE AND AFT BY A DIAG. MATRIX.    
C DS7IPR... APPLIES PERMUTATION TO (LOWER TRIANG. OF) SYM. MATRIX.      
C DS7LUP... PERFORMS QUASI-NEWTON UPDATE ON COMPACTLY STORED LOWER TRI- 
C             ANGLE OF A SYMMETRIC MATRIX.                              
C DS7LVM... MULTIPLIES COMPACTLY STORED SYM. MATRIX TIMES VECTOR.       
C STOPX.... RETURNS .TRUE. IF THE BREAK KEY HAS BEEN PRESSED.           
C DV2NRM... RETURNS THE 2-NORM OF A VECTOR.                             
C DV2AXY.... COMPUTES SCALAR TIMES ONE VECTOR PLUS ANOTHER.             
C DV7CPY.... COPIES ONE VECTOR TO ANOTHER.                              
C DV7IPR... APPLIES A PERMUTATION TO A VECTOR.                          
C DV7SCP... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.                  
C DV7VMP... MULTIPLIES (DIVIDES) VECTORS COMPONENTWISE.                 
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER CNVCOD, COSMIN, COVMAT, COVREQ, DGNORM, DIG,              
     1        DSTNRM, F, FDH, FDIF, FUZZ, F0, GTSTEP, H, HC, IERR,      
     2        INCFAC, INITS, IPIVOT, IRC, IVNEED, KAGQT, KALM, LMAT,    
     3        LMAX0, LMAXS, MODE, MODEL, MXFCAL, MXITER, NEXTIV, NEXTV, 
     4        NFCALL, NFGCAL, NFCOV, NGCOV, NGCALL, NITER, NVSAVE, P0,  
     5        PC, PERM, PHMXFC, PREDUC, QTR, RADFAC, RADINC, RADIUS,    
     6        RAD0, RDREQ, REGD, RELDX, RESTOR, RMAT, S, SIZE, STEP,    
     7        STGLIM, STPPAR, SUSED, SWITCH, TOOBIG, TUNER4, TUNER5,    
     8        VNEED, VSAVE, W, WSCALE, XIRC, X0                         
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C  ***  (NOTE THAT P0 AND PC ARE STORED IN IV(G0) AND IV(STLSTG) RESP.) 
C                                                                       
C/6                                                                     
C     DATA CNVCOD/55/, COVMAT/26/, COVREQ/15/, DIG/37/, FDH/74/, H/56/, 
C    1     HC/71/, IERR/75/, INITS/25/, IPIVOT/76/, IRC/29/, IVNEED/3/, 
C    2     KAGQT/33/, KALM/34/, LMAT/42/, MODE/35/, MODEL/5/,           
C    3     MXFCAL/17/, MXITER/18/, NEXTIV/46/, NEXTV/47/, NFCALL/6/,    
C    4     NFGCAL/7/, NFCOV/52/, NGCOV/53/, NGCALL/30/, NITER/31/,      
C    5     P0/48/, PC/41/, PERM/58/, QTR/77/, RADINC/8/, RDREQ/57/,     
C    6     REGD/67/, RESTOR/9/, RMAT/78/, S/62/, STEP/40/, STGLIM/11/,  
C    7     SUSED/64/, SWITCH/12/, TOOBIG/2/, VNEED/4/, VSAVE/60/, W/65/,
C    8     XIRC/13/, X0/43/                                             
C/7                                                                     
      PARAMETER (CNVCOD=55, COVMAT=26, COVREQ=15, DIG=37, FDH=74, H=56, 
     1           HC=71, IERR=75, INITS=25, IPIVOT=76, IRC=29, IVNEED=3, 
     2           KAGQT=33, KALM=34, LMAT=42, MODE=35, MODEL=5,          
     3           MXFCAL=17, MXITER=18, NEXTIV=46, NEXTV=47, NFCALL=6,   
     4           NFGCAL=7, NFCOV=52, NGCOV=53, NGCALL=30, NITER=31,     
     5           P0=48, PC=41, PERM=58, QTR=77, RADINC=8, RDREQ=57,     
     6           REGD=67, RESTOR=9, RMAT=78, S=62, STEP=40, STGLIM=11,  
     7           SUSED=64, SWITCH=12, TOOBIG=2, VNEED=4, VSAVE=60, W=65,
     8           XIRC=13, X0=43)                                        
C/                                                                      
C                                                                       
C  ***  V SUBSCRIPT VALUES  ***                                         
C                                                                       
C/6                                                                     
C     DATA COSMIN/47/, DGNORM/1/, DSTNRM/2/, F/10/, FDIF/11/, FUZZ/45/, 
C    1     F0/13/, GTSTEP/4/, INCFAC/23/, LMAX0/35/, LMAXS/36/,         
C    2     NVSAVE/9/, PHMXFC/21/, PREDUC/7/, RADFAC/16/, RADIUS/8/,     
C    3     RAD0/9/, RELDX/17/, SIZE/55/, STPPAR/5/, TUNER4/29/,         
C    4     TUNER5/30/, WSCALE/56/                                       
C/7                                                                     
      PARAMETER (COSMIN=47, DGNORM=1, DSTNRM=2, F=10, FDIF=11, FUZZ=45, 
     1           F0=13, GTSTEP=4, INCFAC=23, LMAX0=35, LMAXS=36,        
     2           NVSAVE=9, PHMXFC=21, PREDUC=7, RADFAC=16, RADIUS=8,    
     3           RAD0=9, RELDX=17, SIZE=55, STPPAR=5, TUNER4=29,        
     4           TUNER5=30, WSCALE=56)                                  
C/                                                                      
C                                                                       
C                                                                       
C/6                                                                     
C     DATA HALF/0.5D+0/, NEGONE/-1.D+0/, ONE/1.D+0/, ONEP2/1.2D+0/,     
C    1     ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (HALF=0.5D+0, NEGONE=-1.D+0, ONE=1.D+0, ONEP2=1.2D+0,   
     1           ZERO=0.D+0)                                            
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      I = IV(1)                                                         
      IF (I .EQ. 1) GO TO 50                                            
      IF (I .EQ. 2) GO TO 60                                            
C                                                                       
      IF (I .LT. 12) GO TO 10                                           
      IF (I .GT. 13) GO TO 10                                           
         IV(VNEED) = IV(VNEED) + P*(3*P + 25)/2 + 7                     
         IV(IVNEED) = IV(IVNEED) + 4*P                                  
 10   CALL DPARCK(1, D, IV, LIV, LV, P, V)                              
      I = IV(1) - 2                                                     
      IF (I .GT. 12) GO TO 999                                          
      GO TO (360, 360, 360, 360, 360, 360, 240, 190, 240, 20, 20, 30), I
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
 20   PP1O2 = P * (P + 1) / 2                                           
      IV(S) = IV(LMAT) + PP1O2                                          
      IV(X0) = IV(S) + PP1O2                                            
      IV(STEP) = IV(X0) + 2*P                                           
      IV(DIG) = IV(STEP) + 3*P                                          
      IV(W) = IV(DIG) + 2*P                                             
      IV(H) = IV(W) + 4*P + 7                                           
      IV(NEXTV) = IV(H) + PP1O2                                         
      IV(IPIVOT) = IV(PERM) + 3*P                                       
      IV(NEXTIV) = IV(IPIVOT) + P                                       
      IF (IV(1) .NE. 13) GO TO 30                                       
         IV(1) = 14                                                     
         GO TO 999                                                      
C                                                                       
C  ***  INITIALIZATION  ***                                             
C                                                                       
 30   IV(NITER) = 0                                                     
      IV(NFCALL) = 1                                                    
      IV(NGCALL) = 1                                                    
      IV(NFGCAL) = 1                                                    
      IV(MODE) = -1                                                     
      IV(STGLIM) = 2                                                    
      IV(TOOBIG) = 0                                                    
      IV(CNVCOD) = 0                                                    
      IV(COVMAT) = 0                                                    
      IV(NFCOV) = 0                                                     
      IV(NGCOV) = 0                                                     
      IV(RADINC) = 0                                                    
      IV(PC) = P                                                        
      V(RAD0) = ZERO                                                    
      V(STPPAR) = ZERO                                                  
      V(RADIUS) = V(LMAX0) / (ONE + V(PHMXFC))                          
C                                                                       
C  ***  CHECK CONSISTENCY OF B AND INITIALIZE IP ARRAY  ***             
C                                                                       
      IPI = IV(IPIVOT)                                                  
      DO 40 I = 1, P                                                    
         IV(IPI) = I                                                    
         IPI = IPI + 1                                                  
         IF (B(1,I) .GT. B(2,I)) GO TO 680                              
 40      CONTINUE                                                       
C                                                                       
C  ***  SET INITIAL MODEL AND S MATRIX  ***                             
C                                                                       
      IV(MODEL) = 1                                                     
      IV(1) = 1                                                         
      IF (IV(S) .LT. 0) GO TO 710                                       
      IF (IV(INITS) .GT. 1) IV(MODEL) = 2                               
      S1 = IV(S)                                                        
      IF (IV(INITS) .EQ. 0 .OR. IV(INITS) .GT. 2)                       
     1   CALL DV7SCP(P*(P+1)/2, V(S1), ZERO)                            
      GO TO 710                                                         
C                                                                       
C  ***  NEW FUNCTION VALUE  ***                                         
C                                                                       
 50   IF (IV(MODE) .EQ. 0) GO TO 360                                    
      IF (IV(MODE) .GT. 0) GO TO 590                                    
C                                                                       
      IF (IV(TOOBIG) .EQ. 0) GO TO 690                                  
         IV(1) = 63                                                     
         GO TO 999                                                      
C                                                                       
C  ***  MAKE SURE GRADIENT COULD BE COMPUTED  ***                       
C                                                                       
 60   IF (IV(TOOBIG) .EQ. 0) GO TO 70                                   
         IV(1) = 65                                                     
         GO TO 999                                                      
C                                                                       
C  ***  NEW GRADIENT  ***                                               
C                                                                       
 70   IV(KALM) = -1                                                     
      IV(KAGQT) = -1                                                    
      IV(FDH) = 0                                                       
      IF (IV(MODE) .GT. 0) GO TO 590                                    
      IF (IV(HC) .LE. 0 .AND. IV(RMAT) .LE. 0) GO TO 670                
C                                                                       
C  ***  CHOOSE INITIAL PERMUTATION  ***                                 
C                                                                       
      IPI = IV(IPIVOT)                                                  
      IPN = IPI + P - 1                                                 
      IPIV2 = IV(PERM) - 1                                              
      K = IV(PC)                                                        
      P1 = P                                                            
      PP1 = P + 1                                                       
      RMAT1 = IV(RMAT)                                                  
      HAVRM = RMAT1 .GT. 0                                              
      QTR1 = IV(QTR)                                                    
      HAVQTR = QTR1 .GT. 0                                              
C     *** MAKE SURE V(QTR1) IS LEGAL (EVEN WHEN NOT REFERENCED) ***     
      W1 = IV(W)                                                        
      IF (.NOT. HAVQTR) QTR1 = W1 + P                                   
C                                                                       
      DO 100 I = 1, P                                                   
         I1 = IV(IPN)                                                   
         IPN = IPN - 1                                                  
         IF (B(1,I1) .GE. B(2,I1)) GO TO 80                             
         XI = X(I1)                                                     
         GI = G(I1)                                                     
         IF (XI .LE. B(1,I1) .AND. GI .GT. ZERO) GO TO 80               
         IF (XI .GE. B(2,I1) .AND. GI .LT. ZERO) GO TO 80               
C           *** DISALLOW CONVERGENCE IF X(I1) HAS JUST BEEN FREED ***   
            J = IPIV2 + I1                                              
            IF (IV(J) .GT. K) IV(CNVCOD) = 0                            
            GO TO 100                                                   
 80      IF (I1 .GE. P1) GO TO 90                                       
            I1 = PP1 - I                                                
            CALL I7SHFT(P1, I1, IV(IPI))                                
            IF (HAVRM)                                                  
     1          CALL DQ7RSH(I1, P1, HAVQTR, V(QTR1), V(RMAT1), V(W1))   
 90      P1 = P1 - 1                                                    
 100     CONTINUE                                                       
      IV(PC) = P1                                                       
C                                                                       
C  ***  COMPUTE V(DGNORM) (AN OUTPUT VALUE IF WE STOP NOW)  ***         
C                                                                       
      V(DGNORM) = ZERO                                                  
      IF (P1 .LE. 0) GO TO 110                                          
      DIG1 = IV(DIG)                                                    
      CALL DV7VMP(P, V(DIG1), G, D, -1)                                 
      CALL DV7IPR(P, IV(IPI), V(DIG1))                                  
      V(DGNORM) = DV2NRM(P1, V(DIG1))                                   
 110  IF (IV(CNVCOD) .NE. 0) GO TO 580                                  
      IF (IV(MODE) .EQ. 0) GO TO 510                                    
      IV(MODE) = 0                                                      
      V(F0) = V(F)                                                      
      IF (IV(INITS) .LE. 2) GO TO 170                                   
C                                                                       
C  ***  ARRANGE FOR FINITE-DIFFERENCE INITIAL S  ***                    
C                                                                       
      IV(XIRC) = IV(COVREQ)                                             
      IV(COVREQ) = -1                                                   
      IF (IV(INITS) .GT. 3) IV(COVREQ) = 1                              
      IV(CNVCOD) = 70                                                   
      GO TO 600                                                         
C                                                                       
C  ***  COME TO NEXT STMT AFTER COMPUTING F.D. HESSIAN FOR INIT. S  *** 
C                                                                       
 120  H1 = IV(FDH)                                                      
      IF (H1 .LE. 0) GO TO 660                                          
      IV(CNVCOD) = 0                                                    
      IV(MODE) = 0                                                      
      IV(NFCOV) = 0                                                     
      IV(NGCOV) = 0                                                     
      IV(COVREQ) = IV(XIRC)                                             
      S1 = IV(S)                                                        
      PP1O2 = PS * (PS + 1) / 2                                         
      HC1 = IV(HC)                                                      
      IF (HC1 .LE. 0) GO TO 130                                         
         CALL DV2AXY(PP1O2, V(S1), NEGONE, V(HC1), V(H1))               
         GO TO 140                                                      
 130  RMAT1 = IV(RMAT)                                                  
      LMAT1 = IV(LMAT)                                                  
      CALL DL7SQR(P, V(LMAT1), V(RMAT1))                                
      IPI = IV(IPIVOT)                                                  
      IPIV1 = IV(PERM) + P                                              
      CALL I7PNVR(P, IV(IPIV1), IV(IPI))                                
      CALL DS7IPR(P, IV(IPIV1), V(LMAT1))                               
      CALL DV2AXY(PP1O2, V(S1), NEGONE, V(LMAT1), V(H1))                
C                                                                       
C     *** ZERO PORTION OF S CORRESPONDING TO FIXED X COMPONENTS ***     
C                                                                       
 140  DO 160 I = 1, P                                                   
         IF (B(1,I) .LT. B(2,I)) GO TO 160                              
         K = S1 + I*(I-1)/2                                             
         CALL DV7SCP(I, V(K), ZERO)                                     
         IF (I .GE. P) GO TO 170                                        
         K = K + 2*I - 1                                                
         I1 = I + 1                                                     
         DO 150 J = I1, P                                               
            V(K) = ZERO                                                 
            K = K + J                                                   
 150        CONTINUE                                                    
 160     CONTINUE                                                       
C                                                                       
 170  IV(1) = 2                                                         
C                                                                       
C                                                                       
C-----------------------------  MAIN LOOP  -----------------------------
C                                                                       
C                                                                       
C  ***  PRINT ITERATION SUMMARY, CHECK ITERATION LIMIT  ***             
C                                                                       
 180  CALL DITSUM(D, G, IV, LIV, LV, P, V, X)                           
 190  K = IV(NITER)                                                     
      IF (K .LT. IV(MXITER)) GO TO 200                                  
         IV(1) = 10                                                     
         GO TO 999                                                      
 200  IV(NITER) = K + 1                                                 
C                                                                       
C  ***  UPDATE RADIUS  ***                                              
C                                                                       
      IF (K .EQ. 0) GO TO 220                                           
      STEP1 = IV(STEP)                                                  
      DO 210 I = 1, P                                                   
         V(STEP1) = D(I) * V(STEP1)                                     
         STEP1 = STEP1 + 1                                              
 210     CONTINUE                                                       
      STEP1 = IV(STEP)                                                  
      T = V(RADFAC) * DV2NRM(P, V(STEP1))                               
      IF (V(RADFAC) .LT. ONE .OR. T .GT. V(RADIUS)) V(RADIUS) = T       
C                                                                       
C  ***  INITIALIZE FOR START OF NEXT ITERATION  ***                     
C                                                                       
 220  X01 = IV(X0)                                                      
      V(F0) = V(F)                                                      
      IV(IRC) = 4                                                       
      IV(H) = -IABS(IV(H))                                              
      IV(SUSED) = IV(MODEL)                                             
C                                                                       
C     ***  COPY X TO X0  ***                                            
C                                                                       
      CALL DV7CPY(P, V(X01), X)                                         
C                                                                       
C  ***  CHECK STOPX AND FUNCTION EVALUATION LIMIT  ***                  
C                                                                       
 230  IF (.NOT. STOPX(DUMMY)) GO TO 250                                 
         IV(1) = 11                                                     
         GO TO 260                                                      
C                                                                       
C     ***  COME HERE WHEN RESTARTING AFTER FUNC. EVAL. LIMIT OR STOPX.  
C                                                                       
 240  IF (V(F) .GE. V(F0)) GO TO 250                                    
         V(RADFAC) = ONE                                                
         K = IV(NITER)                                                  
         GO TO 200                                                      
C                                                                       
 250  IF (IV(NFCALL) .LT. IV(MXFCAL) + IV(NFCOV)) GO TO 270             
         IV(1) = 9                                                      
 260     IF (V(F) .GE. V(F0)) GO TO 999                                 
C                                                                       
C        ***  IN CASE OF STOPX OR FUNCTION EVALUATION LIMIT WITH        
C        ***  IMPROVED V(F), EVALUATE THE GRADIENT AT X.                
C                                                                       
              IV(CNVCOD) = IV(1)                                        
              GO TO 500                                                 
C                                                                       
C. . . . . . . . . . . . .  COMPUTE CANDIDATE STEP  . . . . . . . . . . 
C                                                                       
 270  STEP1 = IV(STEP)                                                  
      TG1 = IV(DIG)                                                     
      TD1 = TG1 + P                                                     
      X01 = IV(X0)                                                      
      W1 = IV(W)                                                        
      H1 = IV(H)                                                        
      P1 = IV(PC)                                                       
      IPI = IV(PERM)                                                    
      IPIV1 = IPI + P                                                   
      IPIV2 = IPIV1 + P                                                 
      IPIV0 = IV(IPIVOT)                                                
      IF (IV(MODEL) .EQ. 2) GO TO 280                                   
C                                                                       
C        ***  COMPUTE LEVENBERG-MARQUARDT STEP IF POSSIBLE...           
C                                                                       
         RMAT1 = IV(RMAT)                                               
         IF (RMAT1 .LE. 0) GO TO 280                                    
         QTR1 = IV(QTR)                                                 
         IF (QTR1 .LE. 0) GO TO 280                                     
         LMAT1 = IV(LMAT)                                               
         WLM1 = W1 + P                                                  
         CALL DL7MSB(B, D, G, IV(IERR), IV(IPIV0), IV(IPIV1),           
     1               IV(IPIV2), IV(KALM), V(LMAT1), LV, P, IV(P0),      
     2               IV(PC), V(QTR1), V(RMAT1), V(STEP1), V(TD1),       
     3               V(TG1), V, V(W1), V(WLM1), X, V(X01))              
C        *** H IS STORED IN THE END OF W AND HAS JUST BEEN OVERWRITTEN, 
C        *** SO WE MARK IT INVALID...                                   
         IV(H) = -IABS(H1)                                              
C        *** EVEN IF H WERE STORED ELSEWHERE, IT WOULD BE NECESSARY TO  
C        *** MARK INVALID THE INFORMATION DG7QTS MAY HAVE STORED IN V...
         IV(KAGQT) = -1                                                 
         GO TO 330                                                      
C                                                                       
 280  IF (H1 .GT. 0) GO TO 320                                          
C                                                                       
C     ***  SET H TO  D**-1 * (HC + T1*S) * D**-1.  ***                  
C                                                                       
         P1LEN = P1*(P1+1)/2                                            
         H1 = -H1                                                       
         IV(H) = H1                                                     
         IV(FDH) = 0                                                    
         IF (P1 .LE. 0) GO TO 320                                       
C        *** MAKE TEMPORARY PERMUTATION ARRAY ***                       
         CALL I7COPY(P, IV(IPI), IV(IPIV0))                             
         J = IV(HC)                                                     
         IF (J .GT. 0) GO TO 290                                        
            J = H1                                                      
            RMAT1 = IV(RMAT)                                            
            CALL DL7SQR(P1, V(H1), V(RMAT1))                            
            GO TO 300                                                   
 290     CALL DV7CPY(P*(P+1)/2, V(H1), V(J))                            
         CALL DS7IPR(P, IV(IPI), V(H1))                                 
 300     IF (IV(MODEL) .EQ. 1) GO TO 310                                
            LMAT1 = IV(LMAT)                                            
            S1 = IV(S)                                                  
            CALL DV7CPY(P*(P+1)/2, V(LMAT1), V(S1))                     
            CALL DS7IPR(P, IV(IPI), V(LMAT1))                           
            CALL DV2AXY(P1LEN, V(H1), ONE, V(LMAT1), V(H1))             
 310     CALL DV7CPY(P, V(TD1), D)                                      
         CALL DV7IPR(P, IV(IPI), V(TD1))                                
         CALL DS7DMP(P1, V(H1), V(H1), V(TD1), -1)                      
         IV(KAGQT) = -1                                                 
C                                                                       
C  ***  COMPUTE ACTUAL GOLDFELD-QUANDT-TROTTER STEP  ***                
C                                                                       
 320  LMAT1 = IV(LMAT)                                                  
      CALL DG7QSB(B, D, V(H1), G, IV(IPI), IV(IPIV1), IV(IPIV2),        
     1            IV(KAGQT), V(LMAT1), LV, P, IV(P0), P1, V(STEP1),     
     2            V(TD1), V(TG1), V, V(W1), X, V(X01))                  
      IF (IV(KALM) .GT. 0) IV(KALM) = 0                                 
C                                                                       
 330  IF (IV(IRC) .NE. 6) GO TO 340                                     
         IF (IV(RESTOR) .NE. 2) GO TO 360                               
         RSTRST = 2                                                     
         GO TO 370                                                      
C                                                                       
C  ***  CHECK WHETHER EVALUATING F(X0 + STEP) LOOKS WORTHWHILE  ***     
C                                                                       
 340  IV(TOOBIG) = 0                                                    
      IF (V(DSTNRM) .LE. ZERO) GO TO 360                                
      IF (IV(IRC) .NE. 5) GO TO 350                                     
      IF (V(RADFAC) .LE. ONE) GO TO 350                                 
      IF (V(PREDUC) .GT. ONEP2 * V(FDIF)) GO TO 350                     
         IF (IV(RESTOR) .NE. 2) GO TO 360                               
         RSTRST = 0                                                     
         GO TO 370                                                      
C                                                                       
C  ***  COMPUTE F(X0 + STEP)  ***                                       
C                                                                       
 350  X01 = IV(X0)                                                      
      STEP1 = IV(STEP)                                                  
      CALL DV2AXY(P, X, ONE, V(STEP1), V(X01))                          
      IV(NFCALL) = IV(NFCALL) + 1                                       
      IV(1) = 1                                                         
      GO TO 710                                                         
C                                                                       
C. . . . . . . . . . . . .  ASSESS CANDIDATE STEP  . . . . . . . . . . .
C                                                                       
 360  RSTRST = 3                                                        
 370  X01 = IV(X0)                                                      
      V(RELDX) = DRLDST(P, D, X, V(X01))                                
      CALL DA7SST(IV, LIV, LV, V)                                       
      STEP1 = IV(STEP)                                                  
      LSTGST = X01 + P                                                  
      I = IV(RESTOR) + 1                                                
      GO TO (410, 380, 390, 400), I                                     
 380  CALL DV7CPY(P, X, V(X01))                                         
      GO TO 410                                                         
 390   CALL DV7CPY(P, V(LSTGST), V(STEP1))                              
       GO TO 410                                                        
 400     CALL DV7CPY(P, V(STEP1), V(LSTGST))                            
         CALL DV2AXY(P, X, ONE, V(STEP1), V(X01))                       
         V(RELDX) = DRLDST(P, D, X, V(X01))                             
C                                                                       
C  ***  IF NECESSARY, SWITCH MODELS  ***                                
C                                                                       
 410  IF (IV(SWITCH) .EQ. 0) GO TO 420                                  
         IV(H) = -IABS(IV(H))                                           
         IV(SUSED) = IV(SUSED) + 2                                      
         L = IV(VSAVE)                                                  
         CALL DV7CPY(NVSAVE, V, V(L))                                   
 420  CALL DV2AXY(P, V(STEP1), NEGONE, V(X01), X)                       
      L = IV(IRC) - 4                                                   
      STPMOD = IV(MODEL)                                                
      IF (L .GT. 0) GO TO (440,450,460,460,460,460,460,460,570,510), L  
C                                                                       
C  ***  DECIDE WHETHER TO CHANGE MODELS  ***                            
C                                                                       
      E = V(PREDUC) - V(FDIF)                                           
      S1 = IV(S)                                                        
      CALL DS7LVM(PS, Y, V(S1), V(STEP1))                               
      STTSST = HALF * DD7TPR(PS, V(STEP1), Y)                           
      IF (IV(MODEL) .EQ. 1) STTSST = -STTSST                            
      IF (DABS(E + STTSST) * V(FUZZ) .GE. DABS(E)) GO TO 430            
C                                                                       
C     ***  SWITCH MODELS  ***                                           
C                                                                       
         IV(MODEL) = 3 - IV(MODEL)                                      
         IF (-2 .LT. L) GO TO 470                                       
              IV(H) = -IABS(IV(H))                                      
              IV(SUSED) = IV(SUSED) + 2                                 
              L = IV(VSAVE)                                             
              CALL DV7CPY(NVSAVE, V(L), V)                              
              GO TO 230                                                 
C                                                                       
 430  IF (-3 .LT. L) GO TO 470                                          
C                                                                       
C     ***  RECOMPUTE STEP WITH DIFFERENT RADIUS  ***                    
C                                                                       
 440  V(RADIUS) = V(RADFAC) * V(DSTNRM)                                 
      GO TO 230                                                         
C                                                                       
C  ***  COMPUTE STEP OF LENGTH V(LMAXS) FOR SINGULAR CONVERGENCE TEST   
C                                                                       
 450  V(RADIUS) = V(LMAXS)                                              
      GO TO 270                                                         
C                                                                       
C  ***  CONVERGENCE OR FALSE CONVERGENCE  ***                           
C                                                                       
 460  IV(CNVCOD) = L                                                    
      IF (V(F) .GE. V(F0)) GO TO 580                                    
         IF (IV(XIRC) .EQ. 14) GO TO 580                                
              IV(XIRC) = 14                                             
C                                                                       
C. . . . . . . . . . . .  PROCESS ACCEPTABLE STEP  . . . . . . . . . . .
C                                                                       
 470  IV(COVMAT) = 0                                                    
      IV(REGD) = 0                                                      
C                                                                       
C  ***  SEE WHETHER TO SET V(RADFAC) BY GRADIENT TESTS  ***             
C                                                                       
      IF (IV(IRC) .NE. 3) GO TO 500                                     
         STEP1 = IV(STEP)                                               
         TEMP1 = STEP1 + P                                              
         TEMP2 = IV(X0)                                                 
C                                                                       
C     ***  SET  TEMP1 = HESSIAN * STEP  FOR USE IN GRADIENT TESTS  ***  
C                                                                       
         HC1 = IV(HC)                                                   
         IF (HC1 .LE. 0) GO TO 480                                      
              CALL DS7LVM(P, V(TEMP1), V(HC1), V(STEP1))                
              GO TO 490                                                 
 480     RMAT1 = IV(RMAT)                                               
         IPIV0 = IV(IPIVOT)                                             
         CALL DV7CPY(P, V(TEMP1), V(STEP1))                             
         CALL DV7IPR(P, IV(IPIV0), V(TEMP1))                            
         CALL DL7TVM(P, V(TEMP1), V(RMAT1), V(TEMP1))                   
         CALL DL7VML(P, V(TEMP1), V(RMAT1), V(TEMP1))                   
         IPIV1 = IV(PERM) + P                                           
         CALL I7PNVR(P, IV(IPIV1), IV(IPIV0))                           
         CALL DV7IPR(P, IV(IPIV1), V(TEMP1))                            
C                                                                       
 490     IF (STPMOD .EQ. 1) GO TO 500                                   
              S1 = IV(S)                                                
              CALL DS7LVM(PS, V(TEMP2), V(S1), V(STEP1))                
              CALL DV2AXY(PS, V(TEMP1), ONE, V(TEMP2), V(TEMP1))        
C                                                                       
C  ***  SAVE OLD GRADIENT AND COMPUTE NEW ONE  ***                      
C                                                                       
 500  IV(NGCALL) = IV(NGCALL) + 1                                       
      G01 = IV(W)                                                       
      CALL DV7CPY(P, V(G01), G)                                         
      GO TO 690                                                         
C                                                                       
C  ***  INITIALIZATIONS -- G0 = G - G0, ETC.  ***                       
C                                                                       
 510  G01 = IV(W)                                                       
      CALL DV2AXY(P, V(G01), NEGONE, V(G01), G)                         
      STEP1 = IV(STEP)                                                  
      TEMP1 = STEP1 + P                                                 
      TEMP2 = IV(X0)                                                    
      IF (IV(IRC) .NE. 3) GO TO 540                                     
C                                                                       
C  ***  SET V(RADFAC) BY GRADIENT TESTS  ***                            
C                                                                       
C     ***  SET  TEMP1 = D**-1 * (HESSIAN * STEP  +  (G(X0) - G(X)))  ***
C                                                                       
         K = TEMP1                                                      
         L = G01                                                        
         DO 520 I = 1, P                                                
              V(K) = (V(K) - V(L)) / D(I)                               
              K = K + 1                                                 
              L = L + 1                                                 
 520          CONTINUE                                                  
C                                                                       
C        ***  DO GRADIENT TESTS  ***                                    
C                                                                       
         IF (DV2NRM(P, V(TEMP1)) .LE. V(DGNORM) * V(TUNER4))  GO TO 530 
              IF (DD7TPR(P, G, V(STEP1))                                
     1                  .GE. V(GTSTEP) * V(TUNER5))  GO TO 540          
 530               V(RADFAC) = V(INCFAC)                                
C                                                                       
C  ***  COMPUTE Y VECTOR NEEDED FOR UPDATING S  ***                     
C                                                                       
 540  CALL DV2AXY(PS, Y, NEGONE, Y, G)                                  
C                                                                       
C  ***  DETERMINE SIZING FACTOR V(SIZE)  ***                            
C                                                                       
C     ***  SET TEMP1 = S * STEP  ***                                    
      S1 = IV(S)                                                        
      CALL DS7LVM(PS, V(TEMP1), V(S1), V(STEP1))                        
C                                                                       
      T1 = DABS(DD7TPR(PS, V(STEP1), V(TEMP1)))                         
      T = DABS(DD7TPR(PS, V(STEP1), Y))                                 
      V(SIZE) = ONE                                                     
      IF (T .LT. T1) V(SIZE) = T / T1                                   
C                                                                       
C  ***  SET G0 TO WCHMTD CHOICE OF FLETCHER AND AL-BAALI  ***           
C                                                                       
      HC1 = IV(HC)                                                      
      IF (HC1 .LE. 0) GO TO 550                                         
         CALL DS7LVM(PS, V(G01), V(HC1), V(STEP1))                      
         GO TO 560                                                      
C                                                                       
 550  RMAT1 = IV(RMAT)                                                  
      IPIV0 = IV(IPIVOT)                                                
      CALL DV7CPY(P, V(G01), V(STEP1))                                  
      I = G01 + PS                                                      
      IF (PS .LT. P) CALL DV7SCP(P-PS, V(I), ZERO)                      
      CALL DV7IPR(P, IV(IPIV0), V(G01))                                 
      CALL DL7TVM(P, V(G01), V(RMAT1), V(G01))                          
      CALL DL7VML(P, V(G01), V(RMAT1), V(G01))                          
      IPIV1 = IV(PERM) + P                                              
      CALL I7PNVR(P, IV(IPIV1), IV(IPIV0))                              
      CALL DV7IPR(P, IV(IPIV1), V(G01))                                 
C                                                                       
 560  CALL DV2AXY(PS, V(G01), ONE, Y, V(G01))                           
C                                                                       
C  ***  UPDATE S  ***                                                   
C                                                                       
      CALL DS7LUP(V(S1), V(COSMIN), PS, V(SIZE), V(STEP1), V(TEMP1),    
     1            V(TEMP2), V(G01), V(WSCALE), Y)                       
      IV(1) = 2                                                         
      GO TO 180                                                         
C                                                                       
C. . . . . . . . . . . . . .  MISC. DETAILS  . . . . . . . . . . . . . .
C                                                                       
C  ***  BAD PARAMETERS TO ASSESS  ***                                   
C                                                                       
 570  IV(1) = 64                                                        
      GO TO 999                                                         
C                                                                       
C                                                                       
C  ***  CONVERGENCE OBTAINED -- SEE WHETHER TO COMPUTE COVARIANCE  ***  
C                                                                       
 580  IF (IV(RDREQ) .EQ. 0) GO TO 660                                   
      IF (IV(FDH) .NE. 0) GO TO 660                                     
      IF (IV(CNVCOD) .GE. 7) GO TO 660                                  
      IF (IV(REGD) .GT. 0) GO TO 660                                    
      IF (IV(COVMAT) .GT. 0) GO TO 660                                  
      IF (IABS(IV(COVREQ)) .GE. 3) GO TO 640                            
      IF (IV(RESTOR) .EQ. 0) IV(RESTOR) = 2                             
      GO TO 600                                                         
C                                                                       
C  ***  COMPUTE FINITE-DIFFERENCE HESSIAN FOR COMPUTING COVARIANCE  *** 
C                                                                       
 590  IV(RESTOR) = 0                                                    
 600  CALL DF7DHB(B, D, G, I, IV, LIV, LV, P, V, X)                     
      GO TO (610, 620, 630), I                                          
 610  IV(NFCOV) = IV(NFCOV) + 1                                         
      IV(NFCALL) = IV(NFCALL) + 1                                       
      IV(1) = 1                                                         
      GO TO 710                                                         
C                                                                       
 620  IV(NGCOV) = IV(NGCOV) + 1                                         
      IV(NGCALL) = IV(NGCALL) + 1                                       
      IV(NFGCAL) = IV(NFCALL) + IV(NGCOV)                               
      GO TO 690                                                         
C                                                                       
 630  IF (IV(CNVCOD) .EQ. 70) GO TO 120                                 
      GO TO 660                                                         
C                                                                       
 640  H1 = IABS(IV(H))                                                  
      IV(FDH) = H1                                                      
      IV(H) = -H1                                                       
      HC1 = IV(HC)                                                      
      IF (HC1 .LE. 0) GO TO 650                                         
           CALL DV7CPY(P*(P+1)/2, V(H1), V(HC1))                        
           GO TO 660                                                    
 650  RMAT1 = IV(RMAT)                                                  
      CALL DL7SQR(P, V(H1), V(RMAT1))                                   
C                                                                       
 660  IV(MODE) = 0                                                      
      IV(1) = IV(CNVCOD)                                                
      IV(CNVCOD) = 0                                                    
      GO TO 999                                                         
C                                                                       
C  ***  SPECIAL RETURN FOR MISSING HESSIAN INFORMATION -- BOTH          
C  ***  IV(HC) .LE. 0 AND IV(RMAT) .LE. 0                               
C                                                                       
 670  IV(1) = 1400                                                      
      GO TO 999                                                         
C                                                                       
C  ***  INCONSISTENT B  ***                                             
C                                                                       
 680  IV(1) = 82                                                        
      GO TO 999                                                         
C                                                                       
C  *** SAVE, THEN INITIALIZE IPIVOT ARRAY BEFORE COMPUTING G ***        
C                                                                       
 690  IV(1) = 2                                                         
      J = IV(IPIVOT)                                                    
      IPI = IV(PERM)                                                    
      CALL I7PNVR(P, IV(IPI), IV(J))                                    
      DO 700 I = 1, P                                                   
         IV(J) = I                                                      
         J = J + 1                                                      
 700     CONTINUE                                                       
C                                                                       
C  ***  PROJECT X INTO FEASIBLE REGION (PRIOR TO COMPUTING F OR G)  *** 
C                                                                       
 710  DO 720 I = 1, P                                                   
         IF (X(I) .LT. B(1,I)) X(I) = B(1,I)                            
         IF (X(I) .GT. B(2,I)) X(I) = B(2,I)                            
 720     CONTINUE                                                       
      IV(TOOBIG) = 0                                                    
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST LINE OF DG7ITB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DG7LIT(D, G, IV, LIV, LV, P, PS, V, X, Y)              
C                                                                       
C  ***  CARRY OUT NL2SOL-LIKE ITERATIONS FOR GENERALIZED LINEAR   ***   
C  ***  REGRESSION PROBLEMS (AND OTHERS OF SIMILAR STRUCTURE)     ***   
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER LIV, LV, P, PS                                            
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION D(P), G(P), V(LV), X(P), Y(P)                    
C                                                                       
C--------------------------  PARAMETER USAGE  --------------------------
C                                                                       
C D.... SCALE VECTOR.                                                   
C IV... INTEGER VALUE ARRAY.                                            
C LIV.. LENGTH OF IV.  MUST BE AT LEAST 82.                             
C LH... LENGTH OF H = P*(P+1)/2.                                        
C LV... LENGTH OF V.  MUST BE AT LEAST P*(3*P + 19)/2 + 7.              
C G.... GRADIENT AT X (WHEN IV(1) = 2).                                 
C P.... NUMBER OF PARAMETERS (COMPONENTS IN X).                         
C PS... NUMBER OF NONZERO ROWS AND COLUMNS IN S.                        
C V.... FLOATING-POINT VALUE ARRAY.                                     
C X.... PARAMETER VECTOR.                                               
C Y.... PART OF YIELD VECTOR (WHEN IV(1)= 2, SCRATCH OTHERWISE).        
C                                                                       
C  ***  DISCUSSION  ***                                                 
C                                                                       
C       DG7LIT PERFORMS NL2SOL-LIKE ITERATIONS FOR A VARIETY OF         
C     REGRESSION PROBLEMS THAT ARE SIMILAR TO NONLINEAR LEAST-SQUARES   
C     IN THAT THE HESSIAN IS THE SUM OF TWO TERMS, A READILY-COMPUTED   
C     FIRST-ORDER TERM AND A SECOND-ORDER TERM.  THE CALLER SUPPLIES    
C     THE FIRST-ORDER TERM OF THE HESSIAN IN HC (LOWER TRIANGLE, STORED 
C     COMPACTLY BY ROWS IN V, STARTING AT IV(HC)), AND DG7LIT BUILDS AN 
C     APPROXIMATION, S, TO THE SECOND-ORDER TERM.  THE CALLER ALSO      
C     PROVIDES THE FUNCTION VALUE, GRADIENT, AND PART OF THE YIELD      
C     VECTOR USED IN UPDATING S. DG7LIT DECIDES DYNAMICALLY WHETHER OR  
C     NOT TO USE S WHEN CHOOSING THE NEXT STEP TO TRY...  THE HESSIAN   
C     APPROXIMATION USED IS EITHER HC ALONE (GAUSS-NEWTON MODEL) OR     
C     HC + S (AUGMENTED MODEL).                                         
C                                                                       
C        IF PS .LT. P, THEN ROWS AND COLUMNS PS+1...P OF S ARE KEPT     
C     CONSTANT.  THEY WILL BE ZERO UNLESS THE CALLER SETS IV(INITS) TO  
C     1 OR 2 AND SUPPLIES NONZERO VALUES FOR THEM, OR THE CALLER SETS   
C     IV(INITS) TO 3 OR 4 AND THE FINITE-DIFFERENCE INITIAL S THEN      
C     COMPUTED HAS NONZERO VALUES IN THESE ROWS.                        
C                                                                       
C        IF IV(INITS) IS 3 OR 4, THEN THE INITIAL S IS COMPUTED BY      
C     FINITE DIFFERENCES.  3 MEANS USE FUNCTION DIFFERENCES, 4 MEANS    
C     USE GRADIENT DIFFERENCES.  FINITE DIFFERENCING IS DONE THE SAME   
C     WAY AS IN COMPUTING A COVARIANCE MATRIX (WITH IV(COVREQ) = -1, -2,
C     1, OR 2).                                                         
C                                                                       
C        FOR UPDATING S,DG7LIT ASSUMES THAT THE GRADIENT HAS THE FORM   
C     OF A SUM OVER I OF RHO(I,X)*GRAD(R(I,X)), WHERE GRAD DENOTES THE  
C     GRADIENT WITH RESPECT TO X.  THE TRUE SECOND-ORDER TERM THEN IS   
C     THE SUM OVER I OF RHO(I,X)*HESSIAN(R(I,X)).  IF X = X0 + STEP,    
C     THEN WE WISH TO UPDATE S SO THAT S*STEP IS THE SUM OVER I OF      
C     RHO(I,X)*(GRAD(R(I,X)) - GRAD(R(I,X0))).  THE CALLER MUST SUPPLY  
C     PART OF THIS IN Y, NAMELY THE SUM OVER I OF                       
C     RHO(I,X)*GRAD(R(I,X0)), WHEN CALLING DG7LIT WITH IV(1) = 2 AND    
C     IV(MODE) = 0 (WHERE MODE = 38).  G THEN CONTANS THE OTHER PART,   
C     SO THAT THE DESIRED YIELD VECTOR IS G - Y.  IF PS .LT. P, THEN    
C     THE ABOVE DISCUSSION APPLIES ONLY TO THE FIRST PS COMPONENTS OF   
C     GRAD(R(I,X)), STEP, AND Y.                                        
C                                                                       
C        PARAMETERS IV, P, V, AND X ARE THE SAME AS THE CORRESPONDING   
C     ONES TO NL2SOL (WHICH SEE), EXCEPT THAT V CAN BE SHORTER          
C     (SINCE THE PART OF V THAT NL2SOL USES FOR STORING D, J, AND R IS  
C     NOT NEEDED).  MOREOVER, COMPARED WITH NL2SOL, IV(1) MAY HAVE THE  
C     TWO ADDITIONAL OUTPUT VALUES 1 AND 2, WHICH ARE EXPLAINED BELOW,  
C     AS IS THE USE OF IV(TOOBIG) AND IV(NFGCAL).  THE VALUES IV(D),    
C     IV(J), AND IV(R), WHICH ARE OUTPUT VALUES FROM NL2SOL (AND        
C     NL2SNO), ARE NOT REFERENCED BY DG7LIT OR THE SUBROUTINES IT CALLS.
C                                                                       
C        WHEN DG7LIT IS FIRST CALLED, I.E., WHEN DG7LIT IS CALLED WITH  
C     IV(1) = 0 OR 12, V(F), G, AND HC NEED NOT BE INITIALIZED.  TO     
C     OBTAIN THESE STARTING VALUES,DG7LIT RETURNS FIRST WITH IV(1) = 1, 
C     THEN WITH IV(1) = 2, WITH IV(MODE) = -1 IN BOTH CASES.  ON        
C     SUBSEQUENT RETURNS WITH IV(1) = 2, IV(MODE) = 0 IMPLIES THAT      
C     Y MUST ALSO BE SUPPLIED.  (NOTE THAT Y IS USED FOR SCRATCH -- ITS 
C     INPUT CONTENTS ARE LOST.  BY CONTRAST, HC IS NEVER CHANGED.)      
C     ONCE CONVERGENCE HAS BEEN OBTAINED, IV(RDREQ) AND IV(COVREQ) MAY  
C     IMPLY THAT A FINITE-DIFFERENCE HESSIAN SHOULD BE COMPUTED FOR USE 
C     IN COMPUTING A COVARIANCE MATRIX.  IN THIS CASE DG7LIT WILL MAKE A
C     NUMBER OF RETURNS WITH IV(1) = 1 OR 2 AND IV(MODE) POSITIVE.      
C     WHEN IV(MODE) IS POSITIVE, Y SHOULD NOT BE CHANGED.               
C                                                                       
C IV(1) = 1 MEANS THE CALLER SHOULD SET V(F) (I.E., V(10)) TO F(X), THE 
C             FUNCTION VALUE AT X, AND CALL DG7LIT AGAIN, HAVING CHANGED
C             NONE OF THE OTHER PARAMETERS.  AN EXCEPTION OCCURS IF F(X)
C             CANNOT BE EVALUATED (E.G. IF OVERFLOW WOULD OCCUR), WHICH 
C             MAY HAPPEN BECAUSE OF AN OVERSIZED STEP.  IN THIS CASE    
C             THE CALLER SHOULD SET IV(TOOBIG) = IV(2) TO 1, WHICH WILL 
C             CAUSE DG7LIT TO IGNORE V(F) AND TRY A SMALLER STEP.  NOTE 
C             THAT THE CURRENT FUNCTION EVALUATION COUNT IS AVAILABLE   
C             IN IV(NFCALL) = IV(6).  THIS MAY BE USED TO IDENTIFY      
C             WHICH COPY OF SAVED INFORMATION SHOULD BE USED IN COM-    
C             PUTING G, HC, AND Y THE NEXT TIME DG7LIT RETURNS WITH     
C             IV(1) = 2.  SEE MLPIT FOR AN EXAMPLE OF THIS.             
C IV(1) = 2 MEANS THE CALLER SHOULD SET G TO G(X), THE GRADIENT OF F AT 
C             X.  THE CALLER SHOULD ALSO SET HC TO THE GAUSS-NEWTON     
C             HESSIAN AT X.  IF IV(MODE) = 0, THEN THE CALLER SHOULD    
C             ALSO COMPUTE THE PART OF THE YIELD VECTOR DESCRIBED ABOVE.
C             THE CALLER SHOULD THEN CALL DG7LIT AGAIN (WITH IV(1) = 2).
C             THE CALLER MAY ALSO CHANGE D AT THIS TIME, BUT SHOULD NOT 
C             CHANGE X.  NOTE THAT IV(NFGCAL) = IV(7) CONTAINS THE      
C             VALUE THAT IV(NFCALL) HAD DURING THE RETURN WITH          
C             IV(1) = 1 IN WHICH X HAD THE SAME VALUE AS IT NOW HAS.    
C             IV(NFGCAL) IS EITHER IV(NFCALL) OR IV(NFCALL) - 1.  MLPIT 
C             IS AN EXAMPLE WHERE THIS INFORMATION IS USED.  IF G OR HC 
C             CANNOT BE EVALUATED AT X, THEN THE CALLER MAY SET         
C             IV(TOOBIG) TO 1, IN WHICH CASE DG7LIT WILL RETURN WITH    
C             IV(1) = 15.                                               
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY.                                            
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH           
C     SUPPORTED IN PART BY D.O.E. GRANT EX-76-A-01-2295 TO MIT/CCREMS.  
C                                                                       
C        (SEE NL2SOL FOR REFERENCES.)                                   
C                                                                       
C+++++++++++++++++++++++++++  DECLARATIONS  ++++++++++++++++++++++++++++
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER DUMMY, DIG1, G01, H1, HC1, I, IPIV1, J, K, L, LMAT1,      
     1        LSTGST, PP1O2, QTR1, RMAT1, RSTRST, STEP1, STPMOD, S1,    
     2        TEMP1, TEMP2, W1, X01                                     
      DOUBLE PRECISION E, STTSST, T, T1                                 
C                                                                       
C     ***  CONSTANTS  ***                                               
C                                                                       
      DOUBLE PRECISION HALF, NEGONE, ONE, ONEP2, ZERO                   
C                                                                       
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      LOGICAL STOPX                                                     
      DOUBLE PRECISION DD7TPR, DL7SVX, DL7SVN, DRLDST, DR7MDC, DV2NRM   
      EXTERNAL DA7SST, DD7TPR,DF7HES,DG7QTS,DITSUM, DL7MST,DL7SRT,      
     1         DL7SQR, DL7SVX, DL7SVN, DL7TVM,DL7VML,DPARCK, DRLDST,    
     2         DR7MDC, DS7LUP, DS7LVM, STOPX,DV2AXY,DV7CPY, DV7SCP,     
     3         DV2NRM                                                   
C                                                                       
C DA7SST.... ASSESSES CANDIDATE STEP.                                   
C DD7TPR... RETURNS INNER PRODUCT OF TWO VECTORS.                       
C DF7HES.... COMPUTE FINITE-DIFFERENCE HESSIAN (FOR COVARIANCE).        
C DG7QTS.... COMPUTES GOLDFELD-QUANDT-TROTTER STEP (AUGMENTED MODEL).   
C DITSUM.... PRINTS ITERATION SUMMARY AND INFO ON INITIAL AND FINAL X.  
C DL7MST... COMPUTES LEVENBERG-MARQUARDT STEP (GAUSS-NEWTON MODEL).     
C DL7SRT.... COMPUTES CHOLESKY FACTOR OF (LOWER TRIANG. OF) SYM. MATRIX.
C DL7SQR... COMPUTES L * L**T FROM LOWER TRIANGULAR MATRIX L.           
C DL7TVM... COMPUTES L**T * V, V = VECTOR, L = LOWER TRIANGULAR MATRIX. 
C DL7SVX... ESTIMATES LARGEST SING. VALUE OF LOWER TRIANG. MATRIX.      
C DL7SVN... ESTIMATES SMALLEST SING. VALUE OF LOWER TRIANG. MATRIX.     
C DL7VML.... COMPUTES L * V, V = VECTOR, L = LOWER TRIANGULAR MATRIX.   
C DPARCK.... CHECK VALIDITY OF IV AND V INPUT COMPONENTS.               
C DRLDST... COMPUTES V(RELDX) = RELATIVE STEP SIZE.                     
C DR7MDC... RETURNS MACHINE-DEPENDENT CONSTANTS.                        
C DS7LUP... PERFORMS QUASI-NEWTON UPDATE ON COMPACTLY STORED LOWER TRI- 
C             ANGLE OF A SYMMETRIC MATRIX.                              
C STOPX.... RETURNS .TRUE. IF THE BREAK KEY HAS BEEN PRESSED.           
C DV2AXY.... COMPUTES SCALAR TIMES ONE VECTOR PLUS ANOTHER.             
C DV7CPY.... COPIES ONE VECTOR TO ANOTHER.                              
C DV7SCP... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.                  
C DV2NRM... RETURNS THE 2-NORM OF A VECTOR.                             
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER CNVCOD, COSMIN, COVMAT, COVREQ, DGNORM, DIG, DSTNRM, F,   
     1        FDH, FDIF, FUZZ, F0, GTSTEP, H, HC, IERR, INCFAC, INITS,  
     2        IPIVOT, IRC, KAGQT, KALM, LMAT, LMAX0, LMAXS, MODE, MODEL,
     3        MXFCAL, MXITER, NEXTV, NFCALL, NFGCAL, NFCOV, NGCOV,      
     4        NGCALL, NITER, NVSAVE, PHMXFC, PREDUC, QTR, RADFAC,       
     5        RADINC, RADIUS, RAD0, RCOND, RDREQ, REGD, RELDX, RESTOR,  
     6        RMAT, S, SIZE, STEP, STGLIM, STLSTG, STPPAR, SUSED,       
     7        SWITCH, TOOBIG, TUNER4, TUNER5, VNEED, VSAVE, W, WSCALE,  
     8        XIRC, X0                                                  
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C/6                                                                     
C     DATA CNVCOD/55/, COVMAT/26/, COVREQ/15/, DIG/37/, FDH/74/, H/56/, 
C    1     HC/71/, IERR/75/, INITS/25/, IPIVOT/76/, IRC/29/, KAGQT/33/, 
C    2     KALM/34/, LMAT/42/, MODE/35/, MODEL/5/, MXFCAL/17/,          
C    3     MXITER/18/, NEXTV/47/, NFCALL/6/, NFGCAL/7/, NFCOV/52/,      
C    4     NGCOV/53/, NGCALL/30/, NITER/31/, QTR/77/, RADINC/8/,        
C    5     RDREQ/57/, REGD/67/, RESTOR/9/, RMAT/78/, S/62/, STEP/40/,   
C    6     STGLIM/11/, STLSTG/41/, SUSED/64/, SWITCH/12/, TOOBIG/2/,    
C    7     VNEED/4/, VSAVE/60/, W/65/, XIRC/13/, X0/43/                 
C/7                                                                     
      PARAMETER (CNVCOD=55, COVMAT=26, COVREQ=15, DIG=37, FDH=74, H=56, 
     1           HC=71, IERR=75, INITS=25, IPIVOT=76, IRC=29, KAGQT=33, 
     2           KALM=34, LMAT=42, MODE=35, MODEL=5, MXFCAL=17,         
     3           MXITER=18, NEXTV=47, NFCALL=6, NFGCAL=7, NFCOV=52,     
     4           NGCOV=53, NGCALL=30, NITER=31, QTR=77, RADINC=8,       
     5           RDREQ=57, REGD=67, RESTOR=9, RMAT=78, S=62, STEP=40,   
     6           STGLIM=11, STLSTG=41, SUSED=64, SWITCH=12, TOOBIG=2,   
     7           VNEED=4, VSAVE=60, W=65, XIRC=13, X0=43)               
C/                                                                      
C                                                                       
C  ***  V SUBSCRIPT VALUES  ***                                         
C                                                                       
C/6                                                                     
C     DATA COSMIN/47/, DGNORM/1/, DSTNRM/2/, F/10/, FDIF/11/, FUZZ/45/, 
C    1     F0/13/, GTSTEP/4/, INCFAC/23/, LMAX0/35/, LMAXS/36/,         
C    2     NVSAVE/9/, PHMXFC/21/, PREDUC/7/, RADFAC/16/, RADIUS/8/,     
C    3     RAD0/9/, RCOND/53/, RELDX/17/, SIZE/55/, STPPAR/5/,          
C    4     TUNER4/29/, TUNER5/30/, WSCALE/56/                           
C/7                                                                     
      PARAMETER (COSMIN=47, DGNORM=1, DSTNRM=2, F=10, FDIF=11, FUZZ=45, 
     1           F0=13, GTSTEP=4, INCFAC=23, LMAX0=35, LMAXS=36,        
     2           NVSAVE=9, PHMXFC=21, PREDUC=7, RADFAC=16, RADIUS=8,    
     3           RAD0=9, RCOND=53, RELDX=17, SIZE=55, STPPAR=5,         
     4           TUNER4=29, TUNER5=30, WSCALE=56)                       
C/                                                                      
C                                                                       
C                                                                       
C/6                                                                     
C     DATA HALF/0.5D+0/, NEGONE/-1.D+0/, ONE/1.D+0/, ONEP2/1.2D+0/,     
C    1     ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (HALF=0.5D+0, NEGONE=-1.D+0, ONE=1.D+0, ONEP2=1.2D+0,   
     1           ZERO=0.D+0)                                            
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      I = IV(1)                                                         
      IF (I .EQ. 1) GO TO 40                                            
      IF (I .EQ. 2) GO TO 50                                            
C                                                                       
      IF (I .EQ. 12 .OR. I .EQ. 13)                                     
     1     IV(VNEED) = IV(VNEED) + P*(3*P + 19)/2 + 7                   
      CALL DPARCK(1, D, IV, LIV, LV, P, V)                              
      I = IV(1) - 2                                                     
      IF (I .GT. 12) GO TO 999                                          
      GO TO (290, 290, 290, 290, 290, 290, 170, 120, 170, 10, 10, 20), I
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
 10   PP1O2 = P * (P + 1) / 2                                           
      IV(S) = IV(LMAT) + PP1O2                                          
      IV(X0) = IV(S) + PP1O2                                            
      IV(STEP) = IV(X0) + P                                             
      IV(STLSTG) = IV(STEP) + P                                         
      IV(DIG) = IV(STLSTG) + P                                          
      IV(W) = IV(DIG) + P                                               
      IV(H) = IV(W) + 4*P + 7                                           
      IV(NEXTV) = IV(H) + PP1O2                                         
      IF (IV(1) .NE. 13) GO TO 20                                       
         IV(1) = 14                                                     
         GO TO 999                                                      
C                                                                       
C  ***  INITIALIZATION  ***                                             
C                                                                       
 20   IV(NITER) = 0                                                     
      IV(NFCALL) = 1                                                    
      IV(NGCALL) = 1                                                    
      IV(NFGCAL) = 1                                                    
      IV(MODE) = -1                                                     
      IV(STGLIM) = 2                                                    
      IV(TOOBIG) = 0                                                    
      IV(CNVCOD) = 0                                                    
      IV(COVMAT) = 0                                                    
      IV(NFCOV) = 0                                                     
      IV(NGCOV) = 0                                                     
      IV(RADINC) = 0                                                    
      IV(RESTOR) = 0                                                    
      IV(FDH) = 0                                                       
      V(RAD0) = ZERO                                                    
      V(STPPAR) = ZERO                                                  
      V(RADIUS) = V(LMAX0) / (ONE + V(PHMXFC))                          
C                                                                       
C  ***  SET INITIAL MODEL AND S MATRIX  ***                             
C                                                                       
      IV(MODEL) = 1                                                     
      IF (IV(S) .LT. 0) GO TO 999                                       
      IF (IV(INITS) .GT. 1) IV(MODEL) = 2                               
      S1 = IV(S)                                                        
      IF (IV(INITS) .EQ. 0 .OR. IV(INITS) .GT. 2)                       
     1   CALL DV7SCP(P*(P+1)/2, V(S1), ZERO)                            
      IV(1) = 1                                                         
      J = IV(IPIVOT)                                                    
      IF (J .LE. 0) GO TO 999                                           
      DO 30 I = 1, P                                                    
         IV(J) = I                                                      
         J = J + 1                                                      
 30      CONTINUE                                                       
      GO TO 999                                                         
C                                                                       
C  ***  NEW FUNCTION VALUE  ***                                         
C                                                                       
 40   IF (IV(MODE) .EQ. 0) GO TO 290                                    
      IF (IV(MODE) .GT. 0) GO TO 520                                    
C                                                                       
      IV(1) = 2                                                         
      IF (IV(TOOBIG) .EQ. 0) GO TO 999                                  
         IV(1) = 63                                                     
         GO TO 999                                                      
C                                                                       
C  ***  NEW GRADIENT  ***                                               
C                                                                       
 50   IV(KALM) = -1                                                     
      IV(KAGQT) = -1                                                    
      IV(FDH) = 0                                                       
      IF (IV(MODE) .GT. 0) GO TO 520                                    
C                                                                       
C  ***  MAKE SURE GRADIENT COULD BE COMPUTED  ***                       
C                                                                       
      IF (IV(TOOBIG) .EQ. 0) GO TO 60                                   
         IV(1) = 65                                                     
         GO TO 999                                                      
 60   IF (IV(HC) .LE. 0 .AND. IV(RMAT) .LE. 0) GO TO 610                
C                                                                       
C  ***  COMPUTE  D**-1 * GRADIENT  ***                                  
C                                                                       
      DIG1 = IV(DIG)                                                    
      K = DIG1                                                          
      DO 70 I = 1, P                                                    
         V(K) = G(I) / D(I)                                             
         K = K + 1                                                      
 70      CONTINUE                                                       
      V(DGNORM) = DV2NRM(P, V(DIG1))                                    
C                                                                       
      IF (IV(CNVCOD) .NE. 0) GO TO 510                                  
      IF (IV(MODE) .EQ. 0) GO TO 440                                    
      IV(MODE) = 0                                                      
      V(F0) = V(F)                                                      
      IF (IV(INITS) .LE. 2) GO TO 100                                   
C                                                                       
C  ***  ARRANGE FOR FINITE-DIFFERENCE INITIAL S  ***                    
C                                                                       
      IV(XIRC) = IV(COVREQ)                                             
      IV(COVREQ) = -1                                                   
      IF (IV(INITS) .GT. 3) IV(COVREQ) = 1                              
      IV(CNVCOD) = 70                                                   
      GO TO 530                                                         
C                                                                       
C  ***  COME TO NEXT STMT AFTER COMPUTING F.D. HESSIAN FOR INIT. S  *** 
C                                                                       
 80   IV(CNVCOD) = 0                                                    
      IV(MODE) = 0                                                      
      IV(NFCOV) = 0                                                     
      IV(NGCOV) = 0                                                     
      IV(COVREQ) = IV(XIRC)                                             
      S1 = IV(S)                                                        
      PP1O2 = PS * (PS + 1) / 2                                         
      HC1 = IV(HC)                                                      
      IF (HC1 .LE. 0) GO TO 90                                          
         CALL DV2AXY(PP1O2, V(S1), NEGONE, V(HC1), V(H1))               
         GO TO 100                                                      
 90   RMAT1 = IV(RMAT)                                                  
      CALL DL7SQR(PS, V(S1), V(RMAT1))                                  
      CALL DV2AXY(PP1O2, V(S1), NEGONE, V(S1), V(H1))                   
 100  IV(1) = 2                                                         
C                                                                       
C                                                                       
C-----------------------------  MAIN LOOP  -----------------------------
C                                                                       
C                                                                       
C  ***  PRINT ITERATION SUMMARY, CHECK ITERATION LIMIT  ***             
C                                                                       
 110  CALL DITSUM(D, G, IV, LIV, LV, P, V, X)                           
 120  K = IV(NITER)                                                     
      IF (K .LT. IV(MXITER)) GO TO 130                                  
         IV(1) = 10                                                     
         GO TO 999                                                      
 130  IV(NITER) = K + 1                                                 
C                                                                       
C  ***  UPDATE RADIUS  ***                                              
C                                                                       
      IF (K .EQ. 0) GO TO 150                                           
      STEP1 = IV(STEP)                                                  
      DO 140 I = 1, P                                                   
         V(STEP1) = D(I) * V(STEP1)                                     
         STEP1 = STEP1 + 1                                              
 140     CONTINUE                                                       
      STEP1 = IV(STEP)                                                  
      T = V(RADFAC) * DV2NRM(P, V(STEP1))                               
      IF (V(RADFAC) .LT. ONE .OR. T .GT. V(RADIUS)) V(RADIUS) = T       
C                                                                       
C  ***  INITIALIZE FOR START OF NEXT ITERATION  ***                     
C                                                                       
 150  X01 = IV(X0)                                                      
      V(F0) = V(F)                                                      
      IV(IRC) = 4                                                       
      IV(H) = -IABS(IV(H))                                              
      IV(SUSED) = IV(MODEL)                                             
C                                                                       
C     ***  COPY X TO X0  ***                                            
C                                                                       
      CALL DV7CPY(P, V(X01), X)                                         
C                                                                       
C  ***  CHECK STOPX AND FUNCTION EVALUATION LIMIT  ***                  
C                                                                       
 160  IF (.NOT. STOPX(DUMMY)) GO TO 180                                 
         IV(1) = 11                                                     
         GO TO 190                                                      
C                                                                       
C     ***  COME HERE WHEN RESTARTING AFTER FUNC. EVAL. LIMIT OR STOPX.  
C                                                                       
 170  IF (V(F) .GE. V(F0)) GO TO 180                                    
         V(RADFAC) = ONE                                                
         K = IV(NITER)                                                  
         GO TO 130                                                      
C                                                                       
 180  IF (IV(NFCALL) .LT. IV(MXFCAL) + IV(NFCOV)) GO TO 200             
         IV(1) = 9                                                      
 190     IF (V(F) .GE. V(F0)) GO TO 999                                 
C                                                                       
C        ***  IN CASE OF STOPX OR FUNCTION EVALUATION LIMIT WITH        
C        ***  IMPROVED V(F), EVALUATE THE GRADIENT AT X.                
C                                                                       
              IV(CNVCOD) = IV(1)                                        
              GO TO 430                                                 
C                                                                       
C. . . . . . . . . . . . .  COMPUTE CANDIDATE STEP  . . . . . . . . . . 
C                                                                       
 200  STEP1 = IV(STEP)                                                  
      W1 = IV(W)                                                        
      H1 = IV(H)                                                        
      T1 = ONE                                                          
      IF (IV(MODEL) .EQ. 2) GO TO 210                                   
         T1 = ZERO                                                      
C                                                                       
C        ***  COMPUTE LEVENBERG-MARQUARDT STEP IF POSSIBLE...           
C                                                                       
         RMAT1 = IV(RMAT)                                               
         IF (RMAT1 .LE. 0) GO TO 210                                    
         QTR1 = IV(QTR)                                                 
         IF (QTR1 .LE. 0) GO TO 210                                     
         IPIV1 = IV(IPIVOT)                                             
         CALL DL7MST(D, G, IV(IERR), IV(IPIV1), IV(KALM), P, V(QTR1),   
     1               V(RMAT1), V(STEP1), V, V(W1))                      
C        *** H IS STORED IN THE END OF W AND HAS JUST BEEN OVERWRITTEN, 
C        *** SO WE MARK IT INVALID...                                   
         IV(H) = -IABS(H1)                                              
C        *** EVEN IF H WERE STORED ELSEWHERE, IT WOULD BE NECESSARY TO  
C        *** MARK INVALID THE INFORMATION DG7QTS MAY HAVE STORED IN V...
         IV(KAGQT) = -1                                                 
         GO TO 260                                                      
C                                                                       
 210  IF (H1 .GT. 0) GO TO 250                                          
C                                                                       
C     ***  SET H TO  D**-1 * (HC + T1*S) * D**-1.  ***                  
C                                                                       
         H1 = -H1                                                       
         IV(H) = H1                                                     
         IV(FDH) = 0                                                    
         J = IV(HC)                                                     
         IF (J .GT. 0) GO TO 220                                        
            J = H1                                                      
            RMAT1 = IV(RMAT)                                            
            CALL DL7SQR(P, V(H1), V(RMAT1))                             
 220     S1 = IV(S)                                                     
         DO 240 I = 1, P                                                
              T = ONE / D(I)                                            
              DO 230 K = 1, I                                           
                   V(H1) = T * (V(J) + T1*V(S1)) / D(K)                 
                   J = J + 1                                            
                   H1 = H1 + 1                                          
                   S1 = S1 + 1                                          
 230               CONTINUE                                             
 240          CONTINUE                                                  
         H1 = IV(H)                                                     
         IV(KAGQT) = -1                                                 
C                                                                       
C  ***  COMPUTE ACTUAL GOLDFELD-QUANDT-TROTTER STEP  ***                
C                                                                       
 250  DIG1 = IV(DIG)                                                    
      LMAT1 = IV(LMAT)                                                  
      CALL DG7QTS(D, V(DIG1), V(H1), IV(KAGQT), V(LMAT1), P, V(STEP1),  
     1            V, V(W1))                                             
      IF (IV(KALM) .GT. 0) IV(KALM) = 0                                 
C                                                                       
 260  IF (IV(IRC) .NE. 6) GO TO 270                                     
         IF (IV(RESTOR) .NE. 2) GO TO 290                               
         RSTRST = 2                                                     
         GO TO 300                                                      
C                                                                       
C  ***  CHECK WHETHER EVALUATING F(X0 + STEP) LOOKS WORTHWHILE  ***     
C                                                                       
 270  IV(TOOBIG) = 0                                                    
      IF (V(DSTNRM) .LE. ZERO) GO TO 290                                
      IF (IV(IRC) .NE. 5) GO TO 280                                     
      IF (V(RADFAC) .LE. ONE) GO TO 280                                 
      IF (V(PREDUC) .GT. ONEP2 * V(FDIF)) GO TO 280                     
         IF (IV(RESTOR) .NE. 2) GO TO 290                               
         RSTRST = 0                                                     
         GO TO 300                                                      
C                                                                       
C  ***  COMPUTE F(X0 + STEP)  ***                                       
C                                                                       
 280  X01 = IV(X0)                                                      
      STEP1 = IV(STEP)                                                  
      CALL DV2AXY(P, X, ONE, V(STEP1), V(X01))                          
      IV(NFCALL) = IV(NFCALL) + 1                                       
      IV(1) = 1                                                         
      GO TO 999                                                         
C                                                                       
C. . . . . . . . . . . . .  ASSESS CANDIDATE STEP  . . . . . . . . . . .
C                                                                       
 290  RSTRST = 3                                                        
 300  X01 = IV(X0)                                                      
      V(RELDX) = DRLDST(P, D, X, V(X01))                                
      CALL DA7SST(IV, LIV, LV, V)                                       
      STEP1 = IV(STEP)                                                  
      LSTGST = IV(STLSTG)                                               
      I = IV(RESTOR) + 1                                                
      GO TO (340, 310, 320, 330), I                                     
 310  CALL DV7CPY(P, X, V(X01))                                         
      GO TO 340                                                         
 320   CALL DV7CPY(P, V(LSTGST), V(STEP1))                              
       GO TO 340                                                        
 330     CALL DV7CPY(P, V(STEP1), V(LSTGST))                            
         CALL DV2AXY(P, X, ONE, V(STEP1), V(X01))                       
         V(RELDX) = DRLDST(P, D, X, V(X01))                             
         IV(RESTOR) = RSTRST                                            
C                                                                       
C  ***  IF NECESSARY, SWITCH MODELS  ***                                
C                                                                       
 340  IF (IV(SWITCH) .EQ. 0) GO TO 350                                  
         IV(H) = -IABS(IV(H))                                           
         IV(SUSED) = IV(SUSED) + 2                                      
         L = IV(VSAVE)                                                  
         CALL DV7CPY(NVSAVE, V, V(L))                                   
 350  L = IV(IRC) - 4                                                   
      STPMOD = IV(MODEL)                                                
      IF (L .GT. 0) GO TO (370,380,390,390,390,390,390,390,500,440), L  
C                                                                       
C  ***  DECIDE WHETHER TO CHANGE MODELS  ***                            
C                                                                       
      E = V(PREDUC) - V(FDIF)                                           
      S1 = IV(S)                                                        
      CALL DS7LVM(PS, Y, V(S1), V(STEP1))                               
      STTSST = HALF * DD7TPR(PS, V(STEP1), Y)                           
      IF (IV(MODEL) .EQ. 1) STTSST = -STTSST                            
      IF (DABS(E + STTSST) * V(FUZZ) .GE. DABS(E)) GO TO 360            
C                                                                       
C     ***  SWITCH MODELS  ***                                           
C                                                                       
         IV(MODEL) = 3 - IV(MODEL)                                      
         IF (-2 .LT. L) GO TO 400                                       
              IV(H) = -IABS(IV(H))                                      
              IV(SUSED) = IV(SUSED) + 2                                 
              L = IV(VSAVE)                                             
              CALL DV7CPY(NVSAVE, V(L), V)                              
              GO TO 160                                                 
C                                                                       
 360  IF (-3 .LT. L) GO TO 400                                          
C                                                                       
C  ***  RECOMPUTE STEP WITH NEW RADIUS  ***                             
C                                                                       
 370  V(RADIUS) = V(RADFAC) * V(DSTNRM)                                 
      GO TO 160                                                         
C                                                                       
C  ***  COMPUTE STEP OF LENGTH V(LMAXS) FOR SINGULAR CONVERGENCE TEST   
C                                                                       
 380  V(RADIUS) = V(LMAXS)                                              
      GO TO 200                                                         
C                                                                       
C  ***  CONVERGENCE OR FALSE CONVERGENCE  ***                           
C                                                                       
 390  IV(CNVCOD) = L                                                    
      IF (V(F) .GE. V(F0)) GO TO 510                                    
         IF (IV(XIRC) .EQ. 14) GO TO 510                                
              IV(XIRC) = 14                                             
C                                                                       
C. . . . . . . . . . . .  PROCESS ACCEPTABLE STEP  . . . . . . . . . . .
C                                                                       
 400  IV(COVMAT) = 0                                                    
      IV(REGD) = 0                                                      
C                                                                       
C  ***  SEE WHETHER TO SET V(RADFAC) BY GRADIENT TESTS  ***             
C                                                                       
      IF (IV(IRC) .NE. 3) GO TO 430                                     
         STEP1 = IV(STEP)                                               
         TEMP1 = IV(STLSTG)                                             
         TEMP2 = IV(W)                                                  
C                                                                       
C     ***  SET  TEMP1 = HESSIAN * STEP  FOR USE IN GRADIENT TESTS  ***  
C                                                                       
         HC1 = IV(HC)                                                   
         IF (HC1 .LE. 0) GO TO 410                                      
              CALL DS7LVM(P, V(TEMP1), V(HC1), V(STEP1))                
              GO TO 420                                                 
 410     RMAT1 = IV(RMAT)                                               
         CALL DL7TVM(P, V(TEMP1), V(RMAT1), V(STEP1))                   
         CALL DL7VML(P, V(TEMP1), V(RMAT1), V(TEMP1))                   
C                                                                       
 420     IF (STPMOD .EQ. 1) GO TO 430                                   
              S1 = IV(S)                                                
              CALL DS7LVM(PS, V(TEMP2), V(S1), V(STEP1))                
              CALL DV2AXY(PS, V(TEMP1), ONE, V(TEMP2), V(TEMP1))        
C                                                                       
C  ***  SAVE OLD GRADIENT AND COMPUTE NEW ONE  ***                      
C                                                                       
 430  IV(NGCALL) = IV(NGCALL) + 1                                       
      G01 = IV(W)                                                       
      CALL DV7CPY(P, V(G01), G)                                         
      IV(1) = 2                                                         
      IV(TOOBIG) = 0                                                    
      GO TO 999                                                         
C                                                                       
C  ***  INITIALIZATIONS -- G0 = G - G0, ETC.  ***                       
C                                                                       
 440  G01 = IV(W)                                                       
      CALL DV2AXY(P, V(G01), NEGONE, V(G01), G)                         
      STEP1 = IV(STEP)                                                  
      TEMP1 = IV(STLSTG)                                                
      TEMP2 = IV(W)                                                     
      IF (IV(IRC) .NE. 3) GO TO 470                                     
C                                                                       
C  ***  SET V(RADFAC) BY GRADIENT TESTS  ***                            
C                                                                       
C     ***  SET  TEMP1 = D**-1 * (HESSIAN * STEP  +  (G(X0) - G(X)))  ***
C                                                                       
         K = TEMP1                                                      
         L = G01                                                        
         DO 450 I = 1, P                                                
              V(K) = (V(K) - V(L)) / D(I)                               
              K = K + 1                                                 
              L = L + 1                                                 
 450          CONTINUE                                                  
C                                                                       
C        ***  DO GRADIENT TESTS  ***                                    
C                                                                       
         IF (DV2NRM(P, V(TEMP1)) .LE. V(DGNORM) * V(TUNER4))  GO TO 460 
              IF (DD7TPR(P, G, V(STEP1))                                
     1                  .GE. V(GTSTEP) * V(TUNER5))  GO TO 470          
 460               V(RADFAC) = V(INCFAC)                                
C                                                                       
C  ***  COMPUTE Y VECTOR NEEDED FOR UPDATING S  ***                     
C                                                                       
 470  CALL DV2AXY(PS, Y, NEGONE, Y, G)                                  
C                                                                       
C  ***  DETERMINE SIZING FACTOR V(SIZE)  ***                            
C                                                                       
C     ***  SET TEMP1 = S * STEP  ***                                    
      S1 = IV(S)                                                        
      CALL DS7LVM(PS, V(TEMP1), V(S1), V(STEP1))                        
C                                                                       
      T1 = DABS(DD7TPR(PS, V(STEP1), V(TEMP1)))                         
      T = DABS(DD7TPR(PS, V(STEP1), Y))                                 
      V(SIZE) = ONE                                                     
      IF (T .LT. T1) V(SIZE) = T / T1                                   
C                                                                       
C  ***  SET G0 TO WCHMTD CHOICE OF FLETCHER AND AL-BAALI  ***           
C                                                                       
      HC1 = IV(HC)                                                      
      IF (HC1 .LE. 0) GO TO 480                                         
         CALL DS7LVM(PS, V(G01), V(HC1), V(STEP1))                      
         GO TO 490                                                      
C                                                                       
 480  RMAT1 = IV(RMAT)                                                  
      CALL DL7TVM(PS, V(G01), V(RMAT1), V(STEP1))                       
      CALL DL7VML(PS, V(G01), V(RMAT1), V(G01))                         
C                                                                       
 490  CALL DV2AXY(PS, V(G01), ONE, Y, V(G01))                           
C                                                                       
C  ***  UPDATE S  ***                                                   
C                                                                       
      CALL DS7LUP(V(S1), V(COSMIN), PS, V(SIZE), V(STEP1), V(TEMP1),    
     1            V(TEMP2), V(G01), V(WSCALE), Y)                       
      IV(1) = 2                                                         
      GO TO 110                                                         
C                                                                       
C. . . . . . . . . . . . . .  MISC. DETAILS  . . . . . . . . . . . . . .
C                                                                       
C  ***  BAD PARAMETERS TO ASSESS  ***                                   
C                                                                       
 500  IV(1) = 64                                                        
      GO TO 999                                                         
C                                                                       
C                                                                       
C  ***  CONVERGENCE OBTAINED -- SEE WHETHER TO COMPUTE COVARIANCE  ***  
C                                                                       
 510  IF (IV(RDREQ) .EQ. 0) GO TO 600                                   
      IF (IV(FDH) .NE. 0) GO TO 600                                     
      IF (IV(CNVCOD) .GE. 7) GO TO 600                                  
      IF (IV(REGD) .GT. 0) GO TO 600                                    
      IF (IV(COVMAT) .GT. 0) GO TO 600                                  
      IF (IABS(IV(COVREQ)) .GE. 3) GO TO 560                            
      IF (IV(RESTOR) .EQ. 0) IV(RESTOR) = 2                             
      GO TO 530                                                         
C                                                                       
C  ***  COMPUTE FINITE-DIFFERENCE HESSIAN FOR COMPUTING COVARIANCE  *** 
C                                                                       
 520  IV(RESTOR) = 0                                                    
 530  CALL DF7HES(D, G, I, IV, LIV, LV, P, V, X)                        
      GO TO (540, 550, 580), I                                          
 540  IV(NFCOV) = IV(NFCOV) + 1                                         
      IV(NFCALL) = IV(NFCALL) + 1                                       
      IV(1) = 1                                                         
      GO TO 999                                                         
C                                                                       
 550  IV(NGCOV) = IV(NGCOV) + 1                                         
      IV(NGCALL) = IV(NGCALL) + 1                                       
      IV(NFGCAL) = IV(NFCALL) + IV(NGCOV)                               
      IV(1) = 2                                                         
      GO TO 999                                                         
C                                                                       
 560  H1 = IABS(IV(H))                                                  
      IV(H) = -H1                                                       
      PP1O2 = P * (P + 1) / 2                                           
      RMAT1 = IV(RMAT)                                                  
      IF (RMAT1 .LE. 0) GO TO 570                                       
           LMAT1 = IV(LMAT)                                             
           CALL DV7CPY(PP1O2, V(LMAT1), V(RMAT1))                       
           V(RCOND) = ZERO                                              
           GO TO 590                                                    
 570  HC1 = IV(HC)                                                      
      IV(FDH) = H1                                                      
      CALL DV7CPY(P*(P+1)/2, V(H1), V(HC1))                             
C                                                                       
C  ***  COMPUTE CHOLESKY FACTOR OF FINITE-DIFFERENCE HESSIAN            
C  ***  FOR USE IN CALLER*S COVARIANCE CALCULATION...                   
C                                                                       
 580  LMAT1 = IV(LMAT)                                                  
      H1 = IV(FDH)                                                      
      IF (H1 .LE. 0) GO TO 600                                          
      IF (IV(CNVCOD) .EQ. 70) GO TO 80                                  
      CALL DL7SRT(1, P, V(LMAT1), V(H1), I)                             
      IV(FDH) = -1                                                      
      V(RCOND) = ZERO                                                   
      IF (I .NE. 0) GO TO 600                                           
C                                                                       
 590  IV(FDH) = -1                                                      
      STEP1 = IV(STEP)                                                  
      T = DL7SVN(P, V(LMAT1), V(STEP1), V(STEP1))                       
      IF (T .LE. ZERO) GO TO 600                                        
      T = T / DL7SVX(P, V(LMAT1), V(STEP1), V(STEP1))                   
      IF (T .GT. DR7MDC(4)) IV(FDH) = H1                                
      V(RCOND) = T                                                      
C                                                                       
 600  IV(MODE) = 0                                                      
      IV(1) = IV(CNVCOD)                                                
      IV(CNVCOD) = 0                                                    
      GO TO 999                                                         
C                                                                       
C  ***  SPECIAL RETURN FOR MISSING HESSIAN INFORMATION -- BOTH          
C  ***  IV(HC) .LE. 0 AND IV(RMAT) .LE. 0                               
C                                                                       
 610  IV(1) = 1400                                                      
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST LINE OF DG7LIT FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DA7SST(IV, LIV, LV, V)                                 
C                                                                       
C  ***  ASSESS CANDIDATE STEP (***SOL VERSION 2.3)  ***                 
C                                                                       
      INTEGER LIV, LV                                                   
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION V(LV)                                            
C                                                                       
C  ***  PURPOSE  ***                                                    
C                                                                       
C        THIS SUBROUTINE IS CALLED BY AN UNCONSTRAINED MINIMIZATION     
C     ROUTINE TO ASSESS THE NEXT CANDIDATE STEP.  IT MAY RECOMMEND ONE  
C     OF SEVERAL COURSES OF ACTION, SUCH AS ACCEPTING THE STEP, RECOM-  
C     PUTING IT USING THE SAME OR A NEW QUADRATIC MODEL, OR HALTING DUE 
C     TO CONVERGENCE OR FALSE CONVERGENCE.  SEE THE RETURN CODE LISTING 
C     BELOW.                                                            
C                                                                       
C--------------------------  PARAMETER USAGE  --------------------------
C                                                                       
C  IV (I/O) INTEGER PARAMETER AND SCRATCH VECTOR -- SEE DESCRIPTION     
C             BELOW OF IV VALUES REFERENCED.                            
C LIV (IN)  LENGTH OF IV ARRAY.                                         
C  LV (IN)  LENGTH OF V ARRAY.                                          
C   V (I/O) REAL PARAMETER AND SCRATCH VECTOR -- SEE DESCRIPTION        
C             BELOW OF V VALUES REFERENCED.                             
C                                                                       
C  ***  IV VALUES REFERENCED  ***                                       
C                                                                       
C    IV(IRC) (I/O) ON INPUT FOR THE FIRST STEP TRIED IN A NEW ITERATION,
C             IV(IRC) SHOULD BE SET TO 3 OR 4 (THE VALUE TO WHICH IT IS 
C             SET WHEN STEP IS DEFINITELY TO BE ACCEPTED).  ON INPUT    
C             AFTER STEP HAS BEEN RECOMPUTED, IV(IRC) SHOULD BE         
C             UNCHANGED SINCE THE PREVIOUS RETURN OF DA7SST.            
C                ON OUTPUT, IV(IRC) IS A RETURN CODE HAVING ONE OF THE  
C             FOLLOWING VALUES...                                       
C                  1 = SWITCH MODELS OR TRY SMALLER STEP.               
C                  2 = SWITCH MODELS OR ACCEPT STEP.                    
C                  3 = ACCEPT STEP AND DETERMINE V(RADFAC) BY GRADIENT  
C                       TESTS.                                          
C                  4 = ACCEPT STEP, V(RADFAC) HAS BEEN DETERMINED.      
C                  5 = RECOMPUTE STEP (USING THE SAME MODEL).           
C                  6 = RECOMPUTE STEP WITH RADIUS = V(LMAXS) BUT DO NOT 
C                       EVAULATE THE OBJECTIVE FUNCTION.                
C                  7 = X-CONVERGENCE (SEE V(XCTOL)).                    
C                  8 = RELATIVE FUNCTION CONVERGENCE (SEE V(RFCTOL)).   
C                  9 = BOTH X- AND RELATIVE FUNCTION CONVERGENCE.       
C                 10 = ABSOLUTE FUNCTION CONVERGENCE (SEE V(AFCTOL)).   
C                 11 = SINGULAR CONVERGENCE (SEE V(LMAXS)).             
C                 12 = FALSE CONVERGENCE (SEE V(XFTOL)).                
C                 13 = IV(IRC) WAS OUT OF RANGE ON INPUT.               
C             RETURN CODE I HAS PRECDENCE OVER I+1 FOR I = 9, 10, 11.   
C IV(MLSTGD) (I/O) SAVED VALUE OF IV(MODEL).                            
C  IV(MODEL) (I/O) ON INPUT, IV(MODEL) SHOULD BE AN INTEGER IDENTIFYING 
C             THE CURRENT QUADRATIC MODEL OF THE OBJECTIVE FUNCTION.    
C             IF A PREVIOUS STEP YIELDED A BETTER FUNCTION REDUCTION,   
C             THEN IV(MODEL) WILL BE SET TO IV(MLSTGD) ON OUTPUT.       
C IV(NFCALL) (IN)  INVOCATION COUNT FOR THE OBJECTIVE FUNCTION.         
C IV(NFGCAL) (I/O) VALUE OF IV(NFCALL) AT STEP THAT GAVE THE BIGGEST    
C             FUNCTION REDUCTION THIS ITERATION.  IV(NFGCAL) REMAINS    
C             UNCHANGED UNTIL A FUNCTION REDUCTION IS OBTAINED.         
C IV(RADINC) (I/O) THE NUMBER OF RADIUS INCREASES (OR MINUS THE NUMBER  
C             OF DECREASES) SO FAR THIS ITERATION.                      
C IV(RESTOR) (OUT) SET TO 1 IF V(F) HAS BEEN RESTORED AND X SHOULD BE   
C             RESTORED TO ITS INITIAL VALUE, TO 2 IF X SHOULD BE SAVED, 
C             TO 3 IF X SHOULD BE RESTORED FROM THE SAVED VALUE, AND TO 
C             0 OTHERWISE.                                              
C  IV(STAGE) (I/O) COUNT OF THE NUMBER OF MODELS TRIED SO FAR IN THE    
C             CURRENT ITERATION.                                        
C IV(STGLIM) (IN)  MAXIMUM NUMBER OF MODELS TO CONSIDER.                
C IV(SWITCH) (OUT) SET TO 0 UNLESS A NEW MODEL IS BEING TRIED AND IT    
C             GIVES A SMALLER FUNCTION VALUE THAN THE PREVIOUS MODEL,   
C             IN WHICH CASE DA7SST SETS IV(SWITCH) = 1.                 
C IV(TOOBIG) (IN)  IS NONZERO IF STEP WAS TOO BIG (E.G. IF IT CAUSED    
C             OVERFLOW).                                                
C   IV(XIRC) (I/O) VALUE THAT IV(IRC) WOULD HAVE IN THE ABSENCE OF      
C             CONVERGENCE, FALSE CONVERGENCE, AND OVERSIZED STEPS.      
C                                                                       
C  ***  V VALUES REFERENCED  ***                                        
C                                                                       
C V(AFCTOL) (IN)  ABSOLUTE FUNCTION CONVERGENCE TOLERANCE.  IF THE      
C             ABSOLUTE VALUE OF THE CURRENT FUNCTION VALUE V(F) IS LESS 
C             THAN V(AFCTOL), THEN DA7SST RETURNS WITH IV(IRC) = 10.    
C V(DECFAC) (IN)  FACTOR BY WHICH TO DECREASE RADIUS WHEN IV(TOOBIG) IS 
C             NONZERO.                                                  
C V(DSTNRM) (IN)  THE 2-NORM OF D*STEP.                                 
C V(DSTSAV) (I/O) VALUE OF V(DSTNRM) ON SAVED STEP.                     
C   V(DST0) (IN)  THE 2-NORM OF D TIMES THE NEWTON STEP (WHEN DEFINED,  
C             I.E., FOR V(NREDUC) .GE. 0).                              
C      V(F) (I/O) ON BOTH INPUT AND OUTPUT, V(F) IS THE OBJECTIVE FUNC- 
C             TION VALUE AT X.  IF X IS RESTORED TO A PREVIOUS VALUE,   
C             THEN V(F) IS RESTORED TO THE CORRESPONDING VALUE.         
C   V(FDIF) (OUT) THE FUNCTION REDUCTION V(F0) - V(F) (FOR THE OUTPUT   
C             VALUE OF V(F) IF AN EARLIER STEP GAVE A BIGGER FUNCTION   
C             DECREASE, AND FOR THE INPUT VALUE OF V(F) OTHERWISE).     
C V(FLSTGD) (I/O) SAVED VALUE OF V(F).                                  
C     V(F0) (IN)  OBJECTIVE FUNCTION VALUE AT START OF ITERATION.       
C V(GTSLST) (I/O) VALUE OF V(GTSTEP) ON SAVED STEP.                     
C V(GTSTEP) (IN)  INNER PRODUCT BETWEEN STEP AND GRADIENT.              
C V(INCFAC) (IN)  MINIMUM FACTOR BY WHICH TO INCREASE RADIUS.           
C  V(LMAXS) (IN)  MAXIMUM REASONABLE STEP SIZE (AND INITIAL STEP BOUND).
C             IF THE ACTUAL FUNCTION DECREASE IS NO MORE THAN TWICE     
C             WHAT WAS PREDICTED, IF A RETURN WITH IV(IRC) = 7, 8, 9,   
C             OR 10 DOES NOT OCCUR, IF V(DSTNRM) .GT. V(LMAXS), AND IF  
C             V(PREDUC) .LE. V(SCTOL) * ABS(V(F0)), THEN DA7SST RE-     
C             TURNS WITH IV(IRC) = 11.  IF SO DOING APPEARS WORTHWHILE, 
C             THEN DA7SST REPEATS THIS TEST WITH V(PREDUC) COMPUTED FOR 
C             A STEP OF LENGTH V(LMAXS) (BY A RETURN WITH IV(IRC) = 6). 
C V(NREDUC) (I/O)  FUNCTION REDUCTION PREDICTED BY QUADRATIC MODEL FOR  
C             NEWTON STEP.  IF DA7SST IS CALLED WITH IV(IRC) = 6, I.E., 
C             IF V(PREDUC) HAS BEEN COMPUTED WITH RADIUS = V(LMAXS) FOR 
C             USE IN THE SINGULAR CONVERVENCE TEST, THEN V(NREDUC) IS   
C             SET TO -V(PREDUC) BEFORE THE LATTER IS RESTORED.          
C V(PLSTGD) (I/O) VALUE OF V(PREDUC) ON SAVED STEP.                     
C V(PREDUC) (I/O) FUNCTION REDUCTION PREDICTED BY QUADRATIC MODEL FOR   
C             CURRENT STEP.                                             
C V(RADFAC) (OUT) FACTOR TO BE USED IN DETERMINING THE NEW RADIUS,      
C             WHICH SHOULD BE V(RADFAC)*DST, WHERE  DST  IS EITHER THE  
C             OUTPUT VALUE OF V(DSTNRM) OR THE 2-NORM OF                
C             DIAG(NEWD)*STEP  FOR THE OUTPUT VALUE OF STEP AND THE     
C             UPDATED VERSION, NEWD, OF THE SCALE VECTOR D.  FOR        
C             IV(IRC) = 3, V(RADFAC) = 1.0 IS RETURNED.                 
C V(RDFCMN) (IN)  MINIMUM VALUE FOR V(RADFAC) IN TERMS OF THE INPUT     
C             VALUE OF V(DSTNRM) -- SUGGESTED VALUE = 0.1.              
C V(RDFCMX) (IN)  MAXIMUM VALUE FOR V(RADFAC) -- SUGGESTED VALUE = 4.0. 
C  V(RELDX) (IN) SCALED RELATIVE CHANGE IN X CAUSED BY STEP, COMPUTED   
C             (E.G.) BY FUNCTION  DRLDST  AS                            
C                 MAX (D(I)*ABS(X(I)-X0(I)), 1 .LE. I .LE. P) /         
C                    MAX (D(I)*(ABS(X(I))+ABS(X0(I))), 1 .LE. I .LE. P).
C V(RFCTOL) (IN)  RELATIVE FUNCTION CONVERGENCE TOLERANCE.  IF THE      
C             ACTUAL FUNCTION REDUCTION IS AT MOST TWICE WHAT WAS PRE-  
C             DICTED AND  V(NREDUC) .LE. V(RFCTOL)*ABS(V(F0)),  THEN    
C            DA7SST RETURNS WITH IV(IRC) = 8 OR 9.                      
C V(STPPAR) (IN)  MARQUARDT PARAMETER -- 0 MEANS FULL NEWTON STEP.      
C V(TUNER1) (IN)  TUNING CONSTANT USED TO DECIDE IF THE FUNCTION        
C             REDUCTION WAS MUCH LESS THAN EXPECTED.  SUGGESTED         
C             VALUE = 0.1.                                              
C V(TUNER2) (IN)  TUNING CONSTANT USED TO DECIDE IF THE FUNCTION        
C             REDUCTION WAS LARGE ENOUGH TO ACCEPT STEP.  SUGGESTED     
C             VALUE = 10**-4.                                           
C V(TUNER3) (IN)  TUNING CONSTANT USED TO DECIDE IF THE RADIUS          
C             SHOULD BE INCREASED.  SUGGESTED VALUE = 0.75.             
C  V(XCTOL) (IN)  X-CONVERGENCE CRITERION.  IF STEP IS A NEWTON STEP    
C             (V(STPPAR) = 0) HAVING V(RELDX) .LE. V(XCTOL) AND GIVING  
C             AT MOST TWICE THE PREDICTED FUNCTION DECREASE, THEN       
C            DA7SST RETURNS IV(IRC) = 7 OR 9.                           
C  V(XFTOL) (IN)  FALSE CONVERGENCE TOLERANCE.  IF STEP GAVE NO OR ONLY 
C             A SMALL FUNCTION DECREASE AND V(RELDX) .LE. V(XFTOL),     
C             THEN DA7SST RETURNS WITH IV(IRC) = 12.                    
C                                                                       
C-------------------------------  NOTES  -------------------------------
C                                                                       
C  ***  APPLICATION AND USAGE RESTRICTIONS  ***                         
C                                                                       
C        THIS ROUTINE IS CALLED AS PART OF THE NL2SOL (NONLINEAR        
C     LEAST-SQUARES) PACKAGE.  IT MAY BE USED IN ANY UNCONSTRAINED      
C     MINIMIZATION SOLVER THAT USES DOGLEG, GOLDFELD-QUANDT-TROTTER,    
C     OR LEVENBERG-MARQUARDT STEPS.                                     
C                                                                       
C  ***  ALGORITHM NOTES  ***                                            
C                                                                       
C        SEE (1) FOR FURTHER DISCUSSION OF THE ASSESSING AND MODEL      
C     SWITCHING STRATEGIES.  WHILE NL2SOL CONSIDERS ONLY TWO MODELS,    
C    DA7SST IS DESIGNED TO HANDLE ANY NUMBER OF MODELS.                 
C                                                                       
C  ***  USAGE NOTES  ***                                                
C                                                                       
C        ON THE FIRST CALL OF AN ITERATION, ONLY THE I/O VARIABLES      
C     STEP, X, IV(IRC), IV(MODEL), V(F), V(DSTNRM), V(GTSTEP), AND      
C     V(PREDUC) NEED HAVE BEEN INITIALIZED.  BETWEEN CALLS, NO I/O      
C     VALUES EXECPT STEP, X, IV(MODEL), V(F) AND THE STOPPING TOLER-    
C     ANCES SHOULD BE CHANGED.                                          
C        AFTER A RETURN FOR CONVERGENCE OR FALSE CONVERGENCE, ONE CAN   
C     CHANGE THE STOPPING TOLERANCES AND CALL DA7SST AGAIN, IN WHICH    
C     CASE THE STOPPING TESTS WILL BE REPEATED.                         
C                                                                       
C  ***  REFERENCES  ***                                                 
C                                                                       
C     (1) DENNIS, J.E., JR., GAY, D.M., AND WELSCH, R.E. (1981),        
C        AN ADAPTIVE NONLINEAR LEAST-SQUARES ALGORITHM,                 
C        ACM TRANS. MATH. SOFTWARE, VOL. 7, NO. 3.                      
C                                                                       
C     (2) POWELL, M.J.D. (1970)  A FORTRAN SUBROUTINE FOR SOLVING       
C        SYSTEMS OF NONLINEAR ALGEBRAIC EQUATIONS, IN NUMERICAL         
C        METHODS FOR NONLINEAR ALGEBRAIC EQUATIONS, EDITED BY           
C        P. RABINOWITZ, GORDON AND BREACH, LONDON.                      
C                                                                       
C  ***  HISTORY  ***                                                    
C                                                                       
C        JOHN DENNIS DESIGNED MUCH OF THIS ROUTINE, STARTING WITH       
C     IDEAS IN (2). ROY WELSCH SUGGESTED THE MODEL SWITCHING STRATEGY.  
C        DAVID GAY AND STEPHEN PETERS CAST THIS SUBROUTINE INTO A MORE  
C     PORTABLE FORM (WINTER 1977), AND DAVID GAY CAST IT INTO ITS       
C     PRESENT FORM (FALL 1978).                                         
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH           
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS         
C     MCS-7600324, DCR75-10143, 76-14311DSS, MCS76-11989, AND           
C     MCS-7906671.                                                      
C                                                                       
C------------------------  EXTERNAL QUANTITIES  ------------------------
C                                                                       
C  ***  NO EXTERNAL FUNCTIONS AND SUBROUTINES  ***                      
C                                                                       
C--------------------------  LOCAL VARIABLES  --------------------------
C                                                                       
      LOGICAL GOODX                                                     
      INTEGER I, NFC                                                    
      DOUBLE PRECISION EMAX, EMAXS, GTS, RFAC1, XMAX                    
      DOUBLE PRECISION HALF, ONE, ONEP2, TWO, ZERO                      
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER AFCTOL, DECFAC, DSTNRM, DSTSAV, DST0, F, FDIF, FLSTGD, F0,
     1        GTSLST, GTSTEP, INCFAC, IRC, LMAXS, MLSTGD, MODEL, NFCALL,
     2        NFGCAL, NREDUC, PLSTGD, PREDUC, RADFAC, RADINC, RDFCMN,   
     3        RDFCMX, RELDX, RESTOR, RFCTOL, SCTOL, STAGE, STGLIM,      
     4        STPPAR, SWITCH, TOOBIG, TUNER1, TUNER2, TUNER3, XCTOL,    
     5        XFTOL, XIRC                                               
C                                                                       
C  ***  DATA INITIALIZATIONS  ***                                       
C                                                                       
C/6                                                                     
C     DATA HALF/0.5D+0/, ONE/1.D+0/, ONEP2/1.2D+0/, TWO/2.D+0/,         
C    1     ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (HALF=0.5D+0, ONE=1.D+0, ONEP2=1.2D+0, TWO=2.D+0,       
     1           ZERO=0.D+0)                                            
C/                                                                      
C                                                                       
C/6                                                                     
C     DATA IRC/29/, MLSTGD/32/, MODEL/5/, NFCALL/6/, NFGCAL/7/,         
C    1     RADINC/8/, RESTOR/9/, STAGE/10/, STGLIM/11/, SWITCH/12/,     
C    2     TOOBIG/2/, XIRC/13/                                          
C/7                                                                     
      PARAMETER (IRC=29, MLSTGD=32, MODEL=5, NFCALL=6, NFGCAL=7,        
     1           RADINC=8, RESTOR=9, STAGE=10, STGLIM=11, SWITCH=12,    
     2           TOOBIG=2, XIRC=13)                                     
C/                                                                      
C/6                                                                     
C     DATA AFCTOL/31/, DECFAC/22/, DSTNRM/2/, DST0/3/, DSTSAV/18/,      
C    1     F/10/, FDIF/11/, FLSTGD/12/, F0/13/, GTSLST/14/, GTSTEP/4/,  
C    2     INCFAC/23/, LMAXS/36/, NREDUC/6/, PLSTGD/15/, PREDUC/7/,     
C    3     RADFAC/16/, RDFCMN/24/, RDFCMX/25/, RELDX/17/, RFCTOL/32/,   
C    4     SCTOL/37/, STPPAR/5/, TUNER1/26/, TUNER2/27/, TUNER3/28/,    
C    5     XCTOL/33/, XFTOL/34/                                         
C/7                                                                     
      PARAMETER (AFCTOL=31, DECFAC=22, DSTNRM=2, DST0=3, DSTSAV=18,     
     1           F=10, FDIF=11, FLSTGD=12, F0=13, GTSLST=14, GTSTEP=4,  
     2           INCFAC=23, LMAXS=36, NREDUC=6, PLSTGD=15, PREDUC=7,    
     3           RADFAC=16, RDFCMN=24, RDFCMX=25, RELDX=17, RFCTOL=32,  
     4           SCTOL=37, STPPAR=5, TUNER1=26, TUNER2=27, TUNER3=28,   
     5           XCTOL=33, XFTOL=34)                                    
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      NFC = IV(NFCALL)                                                  
      IV(SWITCH) = 0                                                    
      IV(RESTOR) = 0                                                    
      RFAC1 = ONE                                                       
      GOODX = .TRUE.                                                    
      I = IV(IRC)                                                       
      IF (I .GE. 1 .AND. I .LE. 12)                                     
     1             GO TO (20,30,10,10,40,280,220,220,220,220,220,170), I
         IV(IRC) = 13                                                   
         GO TO 999                                                      
C                                                                       
C  ***  INITIALIZE FOR NEW ITERATION  ***                               
C                                                                       
 10   IV(STAGE) = 1                                                     
      IV(RADINC) = 0                                                    
      V(FLSTGD) = V(F0)                                                 
      IF (IV(TOOBIG) .EQ. 0) GO TO 110                                  
         IV(STAGE) = -1                                                 
         IV(XIRC) = I                                                   
         GO TO 60                                                       
C                                                                       
C  ***  STEP WAS RECOMPUTED WITH NEW MODEL OR SMALLER RADIUS  ***       
C  ***  FIRST DECIDE WHICH  ***                                         
C                                                                       
 20   IF (IV(MODEL) .NE. IV(MLSTGD)) GO TO 30                           
C        ***  OLD MODEL RETAINED, SMALLER RADIUS TRIED  ***             
C        ***  DO NOT CONSIDER ANY MORE NEW MODELS THIS ITERATION  ***   
         IV(STAGE) = IV(STGLIM)                                         
         IV(RADINC) = -1                                                
         GO TO 110                                                      
C                                                                       
C  ***  A NEW MODEL IS BEING TRIED.  DECIDE WHETHER TO KEEP IT.  ***    
C                                                                       
 30   IV(STAGE) = IV(STAGE) + 1                                         
C                                                                       
C     ***  NOW WE ADD THE POSSIBILTIY THAT STEP WAS RECOMPUTED WITH  ***
C     ***  THE SAME MODEL, PERHAPS BECAUSE OF AN OVERSIZED STEP.     ***
C                                                                       
 40   IF (IV(STAGE) .GT. 0) GO TO 50                                    
C                                                                       
C        ***  STEP WAS RECOMPUTED BECAUSE IT WAS TOO BIG.  ***          
C                                                                       
         IF (IV(TOOBIG) .NE. 0) GO TO 60                                
C                                                                       
C        ***  RESTORE IV(STAGE) AND PICK UP WHERE WE LEFT OFF.  ***     
C                                                                       
         IV(STAGE) = -IV(STAGE)                                         
         I = IV(XIRC)                                                   
         GO TO (20, 30, 110, 110, 70), I                                
C                                                                       
 50   IF (IV(TOOBIG) .EQ. 0) GO TO 70                                   
C                                                                       
C  ***  HANDLE OVERSIZE STEP  ***                                       
C                                                                       
      IF (IV(RADINC) .GT. 0) GO TO 80                                   
         IV(STAGE) = -IV(STAGE)                                         
         IV(XIRC) = IV(IRC)                                             
C                                                                       
 60      V(RADFAC) = V(DECFAC)                                          
         IV(RADINC) = IV(RADINC) - 1                                    
         IV(IRC) = 5                                                    
         IV(RESTOR) = 1                                                 
         GO TO 999                                                      
C                                                                       
 70   IF (V(F) .LT. V(FLSTGD)) GO TO 110                                
C                                                                       
C     *** THE NEW STEP IS A LOSER.  RESTORE OLD MODEL.  ***             
C                                                                       
      IF (IV(MODEL) .EQ. IV(MLSTGD)) GO TO 80                           
         IV(MODEL) = IV(MLSTGD)                                         
         IV(SWITCH) = 1                                                 
C                                                                       
C     ***  RESTORE STEP, ETC. ONLY IF A PREVIOUS STEP DECREASED V(F).   
C                                                                       
 80   IF (V(FLSTGD) .GE. V(F0)) GO TO 110                               
         IV(RESTOR) = 1                                                 
         V(F) = V(FLSTGD)                                               
         V(PREDUC) = V(PLSTGD)                                          
         V(GTSTEP) = V(GTSLST)                                          
         IF (IV(SWITCH) .EQ. 0) RFAC1 = V(DSTNRM) / V(DSTSAV)           
         V(DSTNRM) = V(DSTSAV)                                          
         NFC = IV(NFGCAL)                                               
         GOODX = .FALSE.                                                
C                                                                       
 110  V(FDIF) = V(F0) - V(F)                                            
      IF (V(FDIF) .GT. V(TUNER2) * V(PREDUC)) GO TO 140                 
      IF (IV(RADINC) .GT. 0) GO TO 140                                  
C                                                                       
C        ***  NO (OR ONLY A TRIVIAL) FUNCTION DECREASE                  
C        ***  -- SO TRY NEW MODEL OR SMALLER RADIUS                     
C                                                                       
         IF (V(F) .LT. V(F0)) GO TO 120                                 
              IV(MLSTGD) = IV(MODEL)                                    
              V(FLSTGD) = V(F)                                          
              V(F) = V(F0)                                              
              IV(RESTOR) = 1                                            
              GO TO 130                                                 
 120     IV(NFGCAL) = NFC                                               
 130     IV(IRC) = 1                                                    
         IF (IV(STAGE) .LT. IV(STGLIM)) GO TO 160                       
              IV(IRC) = 5                                               
              IV(RADINC) = IV(RADINC) - 1                               
              GO TO 160                                                 
C                                                                       
C  ***  NONTRIVIAL FUNCTION DECREASE ACHIEVED  ***                      
C                                                                       
 140  IV(NFGCAL) = NFC                                                  
      RFAC1 = ONE                                                       
      V(DSTSAV) = V(DSTNRM)                                             
      IF (V(FDIF) .GT. V(PREDUC)*V(TUNER1)) GO TO 190                   
C                                                                       
C  ***  DECREASE WAS MUCH LESS THAN PREDICTED -- EITHER CHANGE MODELS   
C  ***  OR ACCEPT STEP WITH DECREASED RADIUS.                           
C                                                                       
      IF (IV(STAGE) .GE. IV(STGLIM)) GO TO 150                          
C        ***  CONSIDER SWITCHING MODELS  ***                            
         IV(IRC) = 2                                                    
         GO TO 160                                                      
C                                                                       
C     ***  ACCEPT STEP WITH DECREASED RADIUS  ***                       
C                                                                       
 150  IV(IRC) = 4                                                       
C                                                                       
C  ***  SET V(RADFAC) TO FLETCHER*S DECREASE FACTOR  ***                
C                                                                       
 160  IV(XIRC) = IV(IRC)                                                
      EMAX = V(GTSTEP) + V(FDIF)                                        
      V(RADFAC) = HALF * RFAC1                                          
      IF (EMAX .LT. V(GTSTEP)) V(RADFAC) = RFAC1 * DMAX1(V(RDFCMN),     
     1                                           HALF * V(GTSTEP)/EMAX) 
C                                                                       
C  ***  DO FALSE CONVERGENCE TEST  ***                                  
C                                                                       
 170  IF (V(RELDX) .LE. V(XFTOL)) GO TO 180                             
         IV(IRC) = IV(XIRC)                                             
         IF (V(F) .LT. V(F0)) GO TO 200                                 
              GO TO 230                                                 
C                                                                       
 180  IV(IRC) = 12                                                      
      GO TO 240                                                         
C                                                                       
C  ***  HANDLE GOOD FUNCTION DECREASE  ***                              
C                                                                       
 190  IF (V(FDIF) .LT. (-V(TUNER3) * V(GTSTEP))) GO TO 210              
C                                                                       
C     ***  INCREASING RADIUS LOOKS WORTHWHILE.  SEE IF WE JUST          
C     ***  RECOMPUTED STEP WITH A DECREASED RADIUS OR RESTORED STEP     
C     ***  AFTER RECOMPUTING IT WITH A LARGER RADIUS.                   
C                                                                       
      IF (IV(RADINC) .LT. 0) GO TO 210                                  
      IF (IV(RESTOR) .EQ. 1) GO TO 210                                  
C                                                                       
C        ***  WE DID NOT.  TRY A LONGER STEP UNLESS THIS WAS A NEWTON   
C        ***  STEP.                                                     
C                                                                       
         V(RADFAC) = V(RDFCMX)                                          
         GTS = V(GTSTEP)                                                
         IF (V(FDIF) .LT. (HALF/V(RADFAC) - ONE) * GTS)                 
     1            V(RADFAC) = DMAX1(V(INCFAC), HALF*GTS/(GTS + V(FDIF)))
         IV(IRC) = 4                                                    
         IF (V(STPPAR) .EQ. ZERO) GO TO 230                             
         IF (V(DST0) .GE. ZERO .AND. (V(DST0) .LT. TWO*V(DSTNRM)        
     1             .OR. V(NREDUC) .LT. ONEP2*V(FDIF)))  GO TO 230       
C             ***  STEP WAS NOT A NEWTON STEP.  RECOMPUTE IT WITH       
C             ***  A LARGER RADIUS.                                     
              IV(IRC) = 5                                               
              IV(RADINC) = IV(RADINC) + 1                               
C                                                                       
C  ***  SAVE VALUES CORRESPONDING TO GOOD STEP  ***                     
C                                                                       
 200  V(FLSTGD) = V(F)                                                  
      IV(MLSTGD) = IV(MODEL)                                            
      IF (IV(RESTOR) .NE. 1) IV(RESTOR) = 2                             
      V(DSTSAV) = V(DSTNRM)                                             
      IV(NFGCAL) = NFC                                                  
      V(PLSTGD) = V(PREDUC)                                             
      V(GTSLST) = V(GTSTEP)                                             
      GO TO 230                                                         
C                                                                       
C  ***  ACCEPT STEP WITH RADIUS UNCHANGED  ***                          
C                                                                       
 210  V(RADFAC) = ONE                                                   
      IV(IRC) = 3                                                       
      GO TO 230                                                         
C                                                                       
C  ***  COME HERE FOR A RESTART AFTER CONVERGENCE  ***                  
C                                                                       
 220  IV(IRC) = IV(XIRC)                                                
      IF (V(DSTSAV) .GE. ZERO) GO TO 240                                
         IV(IRC) = 12                                                   
         GO TO 240                                                      
C                                                                       
C  ***  PERFORM CONVERGENCE TESTS  ***                                  
C                                                                       
 230  IV(XIRC) = IV(IRC)                                                
 240  IF (IV(RESTOR) .EQ. 1 .AND. V(FLSTGD) .LT. V(F0)) IV(RESTOR) = 3  
      IF (DABS(V(F)) .LT. V(AFCTOL)) IV(IRC) = 10                       
      IF (HALF * V(FDIF) .GT. V(PREDUC)) GO TO 999                      
      EMAX = V(RFCTOL) * DABS(V(F0))                                    
      EMAXS = V(SCTOL) * DABS(V(F0))                                    
      IF (V(PREDUC) .LE. EMAXS .AND. (V(DSTNRM) .GT. V(LMAXS) .OR.      
     1     V(STPPAR) .EQ. ZERO)) IV(IRC) = 11                           
      IF (V(DST0) .LT. ZERO) GO TO 250                                  
      I = 0                                                             
      IF ((V(NREDUC) .GT. ZERO .AND. V(NREDUC) .LE. EMAX) .OR.          
     1    (V(NREDUC) .EQ. ZERO. AND. V(PREDUC) .EQ. ZERO))  I = 2       
      IF (V(STPPAR) .EQ. ZERO .AND. V(RELDX) .LE. V(XCTOL)              
     1                        .AND. GOODX)                  I = I + 1   
      IF (I .GT. 0) IV(IRC) = I + 6                                     
C                                                                       
C  ***  CONSIDER RECOMPUTING STEP OF LENGTH V(LMAXS) FOR SINGULAR       
C  ***  CONVERGENCE TEST.                                               
C                                                                       
 250  IF (IV(IRC) .GT. 5 .AND. IV(IRC) .NE. 12) GO TO 999               
      IF (V(STPPAR) .EQ. ZERO) GO TO 999                                
      IF (V(DSTNRM) .GT. V(LMAXS)) GO TO 260                            
         IF (V(PREDUC) .GE. EMAXS) GO TO 999                            
              IF (V(DST0) .LE. ZERO) GO TO 270                          
                   IF (HALF * V(DST0) .LE. V(LMAXS)) GO TO 999          
                        GO TO 270                                       
 260  IF (HALF * V(DSTNRM) .LE. V(LMAXS)) GO TO 999                     
      XMAX = V(LMAXS) / V(DSTNRM)                                       
      IF (XMAX * (TWO - XMAX) * V(PREDUC) .GE. EMAXS) GO TO 999         
 270  IF (V(NREDUC) .LT. ZERO) GO TO 290                                
C                                                                       
C  ***  RECOMPUTE V(PREDUC) FOR USE IN SINGULAR CONVERGENCE TEST  ***   
C                                                                       
      V(GTSLST) = V(GTSTEP)                                             
      V(DSTSAV) = V(DSTNRM)                                             
      IF (IV(IRC) .EQ. 12) V(DSTSAV) = -V(DSTSAV)                       
      V(PLSTGD) = V(PREDUC)                                             
      I = IV(RESTOR)                                                    
      IV(RESTOR) = 2                                                    
      IF (I .EQ. 3) IV(RESTOR) = 0                                      
      IV(IRC) = 6                                                       
      GO TO 999                                                         
C                                                                       
C  ***  PERFORM SINGULAR CONVERGENCE TEST WITH RECOMPUTED V(PREDUC)  ***
C                                                                       
 280  V(GTSTEP) = V(GTSLST)                                             
      V(DSTNRM) = DABS(V(DSTSAV))                                       
      IV(IRC) = IV(XIRC)                                                
      IF (V(DSTSAV) .LE. ZERO) IV(IRC) = 12                             
      V(NREDUC) = -V(PREDUC)                                            
      V(PREDUC) = V(PLSTGD)                                             
      IV(RESTOR) = 3                                                    
 290  IF (-V(NREDUC) .LE. V(RFCTOL) * DABS(V(F0))) IV(IRC) = 11         
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF DA7SST FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DG7QSB(B, D, DIHDI, G, IPIV, IPIV1, IPIV2, KA, L, LV,  
     1                  P, P0, PC, STEP, TD, TG, V, W, X, X0)           
C                                                                       
C  ***  COMPUTE HEURISTIC BOUNDED NEWTON STEP  ***                      
C                                                                       
      INTEGER KA, LV, P, P0, PC                                         
      INTEGER IPIV(P), IPIV1(P), IPIV2(P)                               
      DOUBLE PRECISION B(2,P), D(P), DIHDI(1), G(P), L(1),              
     1                 STEP(P,2), TD(P), TG(P), V(LV), W(P), X0(P), X(P)
C     DIMENSION DIHDI(P*(P+1)/2), L(P*(P+1)/2)                          
C                                                                       
      DOUBLE PRECISION DD7TPR                                           
      EXTERNAL DD7TPR,DG7QTS, DS7BQN, DS7IPR,DV7CPY, DV7IPR,            
     1         DV7SCP, DV7VMP                                           
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER K, KB, KINIT, NS, P1, P10                                 
      DOUBLE PRECISION DS0, NRED, PRED, RAD                             
      DOUBLE PRECISION ZERO                                             
C                                                                       
C  ***  V SUBSCRIPTS  ***                                               
C                                                                       
      INTEGER DST0, DSTNRM, GTSTEP, NREDUC, PREDUC, RADIUS              
C                                                                       
C/6                                                                     
C     DATA DST0/3/, DSTNRM/2/, GTSTEP/4/, NREDUC/6/, PREDUC/7/,         
C    1     RADIUS/8/                                                    
C/7                                                                     
      PARAMETER (DST0=3, DSTNRM=2, GTSTEP=4, NREDUC=6, PREDUC=7,        
     1           RADIUS=8)                                              
C/                                                                      
      DATA ZERO/0.D+0/                                                  
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      P1 = PC                                                           
      IF (KA .LT. 0) GO TO 10                                           
         NRED = V(NREDUC)                                               
         DS0 = V(DST0)                                                  
         GO TO 20                                                       
 10   P0 = 0                                                            
      KA = -1                                                           
C                                                                       
 20   KINIT = -1                                                        
      IF (P0 .EQ. P1) KINIT = KA                                        
      CALL DV7CPY(P, X, X0)                                             
      PRED = ZERO                                                       
      RAD = V(RADIUS)                                                   
      KB = -1                                                           
      V(DSTNRM) = ZERO                                                  
      IF (P1 .GT. 0) GO TO 30                                           
         NRED = ZERO                                                    
         DS0 = ZERO                                                     
         CALL DV7SCP(P, STEP, ZERO)                                     
         GO TO 60                                                       
C                                                                       
 30   CALL DV7CPY(P, TD, D)                                             
      CALL DV7IPR(P, IPIV, TD)                                          
      CALL DV7VMP(P, TG, G, D, -1)                                      
      CALL DV7IPR(P, IPIV, TG)                                          
 40   K = KINIT                                                         
      KINIT = -1                                                        
      V(RADIUS) = RAD - V(DSTNRM)                                       
      CALL DG7QTS(TD, TG, DIHDI, K, L, P1, STEP, V, W)                  
      P0 = P1                                                           
      IF (KA .GE. 0) GO TO 50                                           
         NRED = V(NREDUC)                                               
         DS0 = V(DST0)                                                  
C                                                                       
 50   KA = K                                                            
      V(RADIUS) = RAD                                                   
      P10 = P1                                                          
      CALL DS7BQN(B, D, STEP(1,2), IPIV, IPIV1, IPIV2, KB, L, LV,       
     1            NS, P, P1, STEP, TD, TG, V, W, X, X0)                 
      IF (NS .GT. 0) CALL DS7IPR(P10, IPIV1, DIHDI)                     
      PRED = PRED + V(PREDUC)                                           
      IF (NS .NE. 0) P0 = 0                                             
      IF (KB .LE. 0) GO TO 40                                           
C                                                                       
 60   V(DST0) = DS0                                                     
      V(NREDUC) = NRED                                                  
      V(PREDUC) = PRED                                                  
      V(GTSTEP) = DD7TPR(P, G, STEP)                                    
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DG7QSB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DL7MSB(B, D, G, IERR, IPIV, IPIV1, IPIV2, KA, LMAT,    
     1                  LV, P, P0, PC, QTR, RMAT, STEP, TD, TG, V,      
     2                  W, WLM, X, X0)                                  
C                                                                       
C  ***  COMPUTE HEURISTIC BOUNDED NEWTON STEP  ***                      
C                                                                       
      INTEGER IERR, KA, LV, P, P0, PC                                   
      INTEGER IPIV(P), IPIV1(P), IPIV2(P)                               
      DOUBLE PRECISION B(2,P), D(P), G(P), LMAT(1), QTR(P), RMAT(1),    
     1                 STEP(P,3), TD(P), TG(P), V(LV), W(P), WLM(1),    
     2                 X0(P), X(P)                                      
C     DIMENSION LMAT(P*(P+1)/2), RMAT(P*(P+1)/2), WLM(P*(P+5)/2 + 4)    
C                                                                       
      DOUBLE PRECISION DD7TPR                                           
      EXTERNAL DD7MLP, DD7TPR, DL7MST, DL7TVM, DQ7RSH, DS7BQN,          
     1        DV2AXY,DV7CPY, DV7IPR, DV7SCP, DV7VMP                     
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, J, K, K0, KB, KINIT, L, NS, P1, P10, P11               
      DOUBLE PRECISION DS0, NRED, PRED, RAD                             
      DOUBLE PRECISION ONE, ZERO                                        
C                                                                       
C  ***  V SUBSCRIPTS  ***                                               
C                                                                       
      INTEGER DST0, DSTNRM, GTSTEP, NREDUC, PREDUC, RADIUS              
C                                                                       
C/6                                                                     
C     DATA DST0/3/, DSTNRM/2/, GTSTEP/4/, NREDUC/6/, PREDUC/7/,         
C    1     RADIUS/8/                                                    
C/7                                                                     
      PARAMETER (DST0=3, DSTNRM=2, GTSTEP=4, NREDUC=6, PREDUC=7,        
     1           RADIUS=8)                                              
C/                                                                      
      DATA ONE/1.D+0/, ZERO/0.D+0/                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      P1 = PC                                                           
      IF (KA .LT. 0) GO TO 10                                           
         NRED = V(NREDUC)                                               
         DS0 = V(DST0)                                                  
         GO TO 20                                                       
 10   P0 = 0                                                            
      KA = -1                                                           
C                                                                       
 20   KINIT = -1                                                        
      IF (P0 .EQ. P1) KINIT = KA                                        
      CALL DV7CPY(P, X, X0)                                             
      CALL DV7CPY(P, TD, D)                                             
C     *** USE STEP(1,3) AS TEMP. COPY OF QTR ***                        
      CALL DV7CPY(P, STEP(1,3), QTR)                                    
      CALL DV7IPR(P, IPIV, TD)                                          
      PRED = ZERO                                                       
      RAD = V(RADIUS)                                                   
      KB = -1                                                           
      V(DSTNRM) = ZERO                                                  
      IF (P1 .GT. 0) GO TO 30                                           
         NRED = ZERO                                                    
         DS0 = ZERO                                                     
         CALL DV7SCP(P, STEP, ZERO)                                     
         GO TO 90                                                       
C                                                                       
 30   CALL DV7VMP(P, TG, G, D, -1)                                      
      CALL DV7IPR(P, IPIV, TG)                                          
      P10 = P1                                                          
 40   K = KINIT                                                         
      KINIT = -1                                                        
      V(RADIUS) = RAD - V(DSTNRM)                                       
      CALL DV7VMP(P1, TG, TG, TD, 1)                                    
      DO 50 I = 1, P1                                                   
 50      IPIV1(I) = I                                                   
      K0 = MAX0(0, K)                                                   
      CALL DL7MST(TD, TG, IERR, IPIV1, K, P1, STEP(1,3), RMAT, STEP,    
     1            V, WLM)                                               
      CALL DV7VMP(P1, TG, TG, TD, -1)                                   
      P0 = P1                                                           
      IF (KA .GE. 0) GO TO 60                                           
         NRED = V(NREDUC)                                               
         DS0 = V(DST0)                                                  
C                                                                       
 60   KA = K                                                            
      V(RADIUS) = RAD                                                   
      L = P1 + 5                                                        
      IF (K .LE. K0) CALL DD7MLP(P1, LMAT, TD, RMAT, -1)                
      IF (K .GT. K0) CALL DD7MLP(P1, LMAT, TD, WLM(L), -1)              
      CALL DS7BQN(B, D, STEP(1,2), IPIV, IPIV1, IPIV2, KB, LMAT,        
     1            LV, NS, P, P1, STEP, TD, TG, V, W, X, X0)             
      PRED = PRED + V(PREDUC)                                           
      IF (NS .EQ. 0) GO TO 80                                           
      P0 = 0                                                            
C                                                                       
C  ***  UPDATE RMAT AND QTR  ***                                        
C                                                                       
      P11 = P1 + 1                                                      
      L = P10 + P11                                                     
      DO 70 K = P11, P10                                                
         J = L - K                                                      
         I = IPIV2(J)                                                   
         IF (I .LT. J) CALL DQ7RSH(I, J, .TRUE., QTR, RMAT, W)          
 70      CONTINUE                                                       
C                                                                       
 80   IF (KB .GT. 0) GO TO 90                                           
C                                                                       
C  ***  UPDATE LOCAL COPY OF QTR  ***                                   
C                                                                       
      CALL DV7VMP(P10, W, STEP(1,2), TD, -1)                            
      CALL DL7TVM(P10, W, LMAT, W)                                      
      CALL DV2AXY(P10, STEP(1,3), ONE, W, QTR)                          
      GO TO 40                                                          
C                                                                       
 90   V(DST0) = DS0                                                     
      V(NREDUC) = NRED                                                  
      V(PREDUC) = PRED                                                  
      V(GTSTEP) = DD7TPR(P, G, STEP)                                    
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DL7MSB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DS7BQN(B, D, DST, IPIV, IPIV1, IPIV2, KB, L, LV, NS,   
     1                  P, P1, STEP, TD, TG, V, W, X, X0)               
C                                                                       
C  ***  COMPUTE BOUNDED MODIFIED NEWTON STEP  ***                       
C                                                                       
      INTEGER KB, LV, NS, P, P1                                         
      INTEGER IPIV(P), IPIV1(P), IPIV2(P)                               
      DOUBLE PRECISION B(2,P), D(P), DST(P), L(1),                      
     1                 STEP(P), TD(P), TG(P), V(LV), W(P), X(P),        
     2                 X0(P)                                            
C     DIMENSION L(P*(P+1)/2)                                            
C                                                                       
      DOUBLE PRECISION DD7TPR, DR7MDC, DV2NRM                           
      EXTERNAL DD7TPR, I7SHFT, DL7ITV, DL7IVM, DQ7RSH, DR7MDC, DV2NRM,  
     1        DV2AXY,DV7CPY, DV7IPR, DV7SCP, DV7SHF                     
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, J, K, P0, P1M1                                         
      DOUBLE PRECISION ALPHA, DST0, DST1, DSTMAX, DSTMIN, DX, GTS, T,   
     1                 TI, T1, XI                                       
      DOUBLE PRECISION FUDGE, HALF, MEPS2, ONE, TWO, ZERO               
C                                                                       
C  ***  V SUBSCRIPTS  ***                                               
C                                                                       
      INTEGER DSTNRM, GTSTEP, PHMNFC, PHMXFC, PREDUC, RADIUS, STPPAR    
C                                                                       
C/6                                                                     
C     DATA DSTNRM/2/, GTSTEP/4/, PHMNFC/20/, PHMXFC/21/, PREDUC/7/,     
C    1     RADIUS/8/, STPPAR/5/                                         
C/7                                                                     
      PARAMETER (DSTNRM=2, GTSTEP=4, PHMNFC=20, PHMXFC=21, PREDUC=7,    
     1           RADIUS=8, STPPAR=5)                                    
      SAVE MEPS2                                                        
C/                                                                      
C                                                                       
      DATA FUDGE/1.0001D+0/, HALF/0.5D+0/, MEPS2/0.D+0/,                
     1     ONE/1.0D+0/, TWO/2.D+0/, ZERO/0.D+0/                         
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      DSTMAX = FUDGE * (ONE + V(PHMXFC)) * V(RADIUS)                    
      DSTMIN = (ONE + V(PHMNFC)) * V(RADIUS)                            
      DST1 = ZERO                                                       
      IF (MEPS2 .LE. ZERO) MEPS2 = TWO * DR7MDC(3)                      
      P0 = P1                                                           
      NS = 0                                                            
      DO 10 I = 1, P                                                    
         IPIV1(I) = I                                                   
         IPIV2(I) = I                                                   
 10      CONTINUE                                                       
      DO 20 I = 1, P1                                                   
 20      W(I) = -STEP(I) * TD(I)                                        
      ALPHA = DABS(V(STPPAR))                                           
      V(PREDUC) = ZERO                                                  
      GTS = -V(GTSTEP)                                                  
      IF (KB .LT. 0) CALL DV7SCP(P, DST, ZERO)                          
      KB = 1                                                            
C                                                                       
C     ***  -W = D TIMES RESTRICTED NEWTON STEP FROM X + DST/D.          
C                                                                       
C     ***  FIND T SUCH THAT X - T*W IS STILL FEASIBLE.                  
C                                                                       
 30   T = ONE                                                           
      K = 0                                                             
      DO 60 I = 1, P1                                                   
         J = IPIV(I)                                                    
         DX = W(I) / D(J)                                               
         XI = X(J) - DX                                                 
         IF (XI .LT. B(1,J)) GO TO 40                                   
         IF (XI .LE. B(2,J)) GO TO 60                                   
              TI = ( X(J)  -  B(2,J) ) / DX                             
              K = I                                                     
              GO TO 50                                                  
 40      TI = ( X(J)  -  B(1,J) ) / DX                                  
              K = -I                                                    
 50      IF (T .LE. TI) GO TO 60                                        
              T = TI                                                    
 60      CONTINUE                                                       
C                                                                       
      IF (P .GT. P1) CALL DV7CPY(P-P1, STEP(P1+1), DST(P1+1))           
      CALL DV2AXY(P1, STEP, -T, W, DST)                                 
      DST0 = DST1                                                       
      DST1 = DV2NRM(P, STEP)                                            
C                                                                       
C  ***  CHECK FOR OVERSIZE STEP  ***                                    
C                                                                       
      IF (DST1 .LE. DSTMAX) GO TO 80                                    
      IF (P1 .GE. P0) GO TO 70                                          
         IF (DST0 .LT. DSTMIN) KB = 0                                   
         GO TO 110                                                      
C                                                                       
 70   K = 0                                                             
C                                                                       
C  ***  UPDATE DST, TG, AND V(PREDUC)  ***                              
C                                                                       
 80   V(DSTNRM) = DST1                                                  
      CALL DV7CPY(P1, DST, STEP)                                        
      T1 = ONE - T                                                      
      DO 90 I = 1, P1                                                   
 90      TG(I) = T1 * TG(I)                                             
      IF (ALPHA .GT. ZERO) CALL DV2AXY(P1, TG, T*ALPHA, W, TG)          
      V(PREDUC) = V(PREDUC) + T*((ONE - HALF*T)*GTS +                   
     1                        HALF*ALPHA*T*DD7TPR(P1,W,W))              
      IF (K .EQ. 0) GO TO 110                                           
C                                                                       
C     ***  PERMUTE L, ETC. IF NECESSARY  ***                            
C                                                                       
      P1M1 = P1 - 1                                                     
      J = IABS(K)                                                       
      IF (J .EQ. P1) GO TO 100                                          
         NS = NS + 1                                                    
         IPIV2(P1) = J                                                  
         CALL DQ7RSH(J, P1, .FALSE., TG, L, W)                          
         CALL I7SHFT(P1, J, IPIV)                                       
         CALL I7SHFT(P1, J, IPIV1)                                      
         CALL DV7SHF(P1, J, TG)                                         
         CALL DV7SHF(P1, J, DST)                                        
 100  IF (K .LT. 0) IPIV(P1) = -IPIV(P1)                                
      P1 = P1M1                                                         
      IF (P1 .LE. 0) GO TO 110                                          
      CALL DL7IVM(P1, W, L, TG)                                         
      GTS = DD7TPR(P1, W, W)                                            
      CALL DL7ITV(P1, W, L, W)                                          
      GO TO 30                                                          
C                                                                       
C     ***  UNSCALE STEP  ***                                            
C                                                                       
 110  DO 120 I = 1, P                                                   
         J = IABS(IPIV(I))                                              
         STEP(J) = DST(I) / D(J)                                        
 120     CONTINUE                                                       
C                                                                       
C  ***  FUDGE STEP TO ENSURE THAT IT FORCES APPROPRIATE COMPONENTS      
C  ***  TO THEIR BOUNDS  ***                                            
C                                                                       
      IF (P1 .GE. P0) GO TO 150                                         
      K = P1 + 1                                                        
      DO 140 I = K, P0                                                  
         J = IPIV(I)                                                    
         T = MEPS2                                                      
         IF (J .GT. 0) GO TO 130                                        
            T = -T                                                      
            J = -J                                                      
            IPIV(I) = J                                                 
 130     T = T * DMAX1(DABS(X(J)), DABS(X0(J)))                         
         STEP(J) = STEP(J) + T                                          
 140     CONTINUE                                                       
C                                                                       
 150  CALL DV2AXY(P, X, ONE, STEP, X0)                                  
      IF (NS .GT. 0) CALL DV7IPR(P0, IPIV1, TD)                         
 999  RETURN                                                            
C  ***  LAST LINE OF DS7BQN FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DS7IPR(P, IP, H)                                       
C                                                                       
C  APPLY THE PERMUTATION DEFINED BY IP TO THE ROWS AND COLUMNS OF THE   
C  P X P SYMMETRIC MATRIX WHOSE LOWER TRIANGLE IS STORED COMPACTLY IN H.
C  THUS H.OUTPUT(I,J) = H.INPUT(IP(I), IP(J)).                          
C                                                                       
      INTEGER P                                                         
      INTEGER IP(P)                                                     
      DOUBLE PRECISION H(1)                                             
C                                                                       
      INTEGER I, J, J1, JM, K, K1, KK, KM, KMJ, L, M                    
      DOUBLE PRECISION T                                                
C                                                                       
C ***  BODY  ***                                                        
C                                                                       
      DO 90 I = 1, P                                                    
         J = IP(I)                                                      
         IF (J .EQ. I) GO TO 90                                         
         IP(I) = IABS(J)                                                
         IF (J .LT. 0) GO TO 90                                         
         K = I                                                          
 10         J1 = J                                                      
            K1 = K                                                      
            IF (J .LE. K) GO TO 20                                      
               J1 = K                                                   
               K1 = J                                                   
 20         KMJ = K1-J1                                                 
            L = J1-1                                                    
            JM = J1*L/2                                                 
            KM = K1*(K1-1)/2                                            
            IF (L .LE. 0) GO TO 40                                      
               DO 30 M = 1, L                                           
                  JM = JM+1                                             
                  T = H(JM)                                             
                  KM = KM+1                                             
                  H(JM) = H(KM)                                         
                  H(KM) = T                                             
 30               CONTINUE                                              
 40         KM = KM+1                                                   
            KK = KM+KMJ                                                 
            JM = JM+1                                                   
            T = H(JM)                                                   
            H(JM) = H(KK)                                               
            H(KK) = T                                                   
            J1 = L                                                      
            L = KMJ-1                                                   
            IF (L .LE. 0) GO TO 60                                      
               DO 50 M = 1, L                                           
                  JM = JM+J1+M                                          
                  T = H(JM)                                             
                  KM = KM+1                                             
                  H(JM) = H(KM)                                         
                  H(KM) = T                                             
 50               CONTINUE                                              
 60         IF (K1 .GE. P) GO TO 80                                     
               L = P-K1                                                 
               K1 = K1-1                                                
               KM = KK                                                  
               DO 70 M = 1, L                                           
                  KM = KM+K1+M                                          
                  JM = KM-KMJ                                           
                  T = H(JM)                                             
                  H(JM) = H(KM)                                         
                  H(KM) = T                                             
 70               CONTINUE                                              
 80         K = J                                                       
            J = IP(K)                                                   
            IP(K) = -J                                                  
            IF (J .GT. I) GO TO 10                                      
 90      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DS7IPR FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DITSUM(D, G, IV, LIV, LV, P, V, X)                     
C                                                                       
C  ***  PRINT ITERATION SUMMARY FOR ***SOL (VERSION 2.3)  ***           
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER LIV, LV, P                                                
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION D(P), G(P), V(LV), X(P)                          
C                                                                       
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER ALG, I, IV1, M, NF, NG, OL, PU                            
C/6S                                                                    
C     REAL MODEL1(6), MODEL2(6)                                         
C/7S                                                                    
      CHARACTER*4 MODEL1(6), MODEL2(6)                                  
C/                                                                      
      DOUBLE PRECISION NRELDF, OLDF, PRELDF, RELDF, ZERO                
C                                                                       
C  ***  NO EXTERNAL FUNCTIONS OR SUBROUTINES  ***                       
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER ALGSAV, DSTNRM, F, FDIF, F0, NEEDHD, NFCALL, NFCOV, NGCOV,
     1        NGCALL, NITER, NREDUC, OUTLEV, PREDUC, PRNTIT, PRUNIT,    
     2        RELDX, SOLPRT, STATPR, STPPAR, SUSED, X0PRT               
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C/6                                                                     
C     DATA ALGSAV/51/, NEEDHD/36/, NFCALL/6/, NFCOV/52/, NGCALL/30/,    
C    1     NGCOV/53/, NITER/31/, OUTLEV/19/, PRNTIT/39/, PRUNIT/21/,    
C    2     SOLPRT/22/, STATPR/23/, SUSED/64/, X0PRT/24/                 
C/7                                                                     
      PARAMETER (ALGSAV=51, NEEDHD=36, NFCALL=6, NFCOV=52, NGCALL=30,   
     1           NGCOV=53, NITER=31, OUTLEV=19, PRNTIT=39, PRUNIT=21,   
     2           SOLPRT=22, STATPR=23, SUSED=64, X0PRT=24)              
C/                                                                      
C                                                                       
C  ***  V SUBSCRIPT VALUES  ***                                         
C                                                                       
C/6                                                                     
C     DATA DSTNRM/2/, F/10/, F0/13/, FDIF/11/, NREDUC/6/, PREDUC/7/,    
C    1     RELDX/17/, STPPAR/5/                                         
C/7                                                                     
      PARAMETER (DSTNRM=2, F=10, F0=13, FDIF=11, NREDUC=6, PREDUC=7,    
     1           RELDX=17, STPPAR=5)                                    
C/                                                                      
C                                                                       
C/6                                                                     
C     DATA ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.D+0)                                            
C/                                                                      
C/6S                                                                    
C     DATA MODEL1(1)/4H    /, MODEL1(2)/4H    /, MODEL1(3)/4H    /,     
C    1     MODEL1(4)/4H    /, MODEL1(5)/4H  G /, MODEL1(6)/4H  S /,     
C    2     MODEL2(1)/4H G  /, MODEL2(2)/4H S  /, MODEL2(3)/4HG-S /,     
C    3     MODEL2(4)/4HS-G /, MODEL2(5)/4H-S-G/, MODEL2(6)/4H-G-S/      
C/7S                                                                    
      DATA MODEL1/'    ','    ','    ','    ','  G ','  S '/,           
     1     MODEL2/' G  ',' S  ','G-S ','S-G ','-S-G','-G-S'/            
C/                                                                      
C                                                                       
C-------------------------------  BODY  --------------------------------
C                                                                       
      PU = IV(PRUNIT)                                                   
      IF (PU .EQ. 0) GO TO 999                                          
      IV1 = IV(1)                                                       
      IF (IV1 .GT. 62) IV1 = IV1 - 51                                   
      OL = IV(OUTLEV)                                                   
      ALG = MOD(IV(ALGSAV)-1,2) + 1                                     
      IF (IV1 .LT. 2 .OR. IV1 .GT. 15) GO TO 370                        
      IF (IV1 .GE. 12) GO TO 120                                        
      IF (IV1 .EQ. 2 .AND. IV(NITER) .EQ. 0) GO TO 390                  
      IF (OL .EQ. 0) GO TO 120                                          
      IF (IV1 .GE. 10 .AND. IV(PRNTIT) .EQ. 0) GO TO 120                
      IF (IV1 .GT. 2) GO TO 10                                          
         IV(PRNTIT) = IV(PRNTIT) + 1                                    
         IF (IV(PRNTIT) .LT. IABS(OL)) GO TO 999                        
 10   NF = IV(NFCALL) - IABS(IV(NFCOV))                                 
      IV(PRNTIT) = 0                                                    
      RELDF = ZERO                                                      
      PRELDF = ZERO                                                     
      OLDF = DMAX1(DABS(V(F0)), DABS(V(F)))                             
      IF (OLDF .LE. ZERO) GO TO 20                                      
         RELDF = V(FDIF) / OLDF                                         
         PRELDF = V(PREDUC) / OLDF                                      
 20   IF (OL .GT. 0) GO TO 60                                           
C                                                                       
C        ***  PRINT SHORT SUMMARY LINE  ***                             
C                                                                       
         IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 1) WRITE(PU,30)           
 30   FORMAT(/10H   IT   NF,6X,1HF,7X,5HRELDF,3X,6HPRELDF,3X,5HRELDX,   
     1       2X,13HMODEL  STPPAR)                                       
         IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 2) WRITE(PU,40)           
 40   FORMAT(/11H    IT   NF,7X,1HF,8X,5HRELDF,4X,6HPRELDF,4X,5HRELDX,  
     1       3X,6HSTPPAR)                                               
         IV(NEEDHD) = 0                                                 
         IF (ALG .EQ. 2) GO TO 50                                       
         M = IV(SUSED)                                                  
         WRITE(PU,100) IV(NITER), NF, V(F), RELDF, PRELDF, V(RELDX),    
     1                 MODEL1(M), MODEL2(M), V(STPPAR)                  
         GO TO 120                                                      
C                                                                       
 50      WRITE(PU,110) IV(NITER), NF, V(F), RELDF, PRELDF, V(RELDX),    
     1                 V(STPPAR)                                        
         GO TO 120                                                      
C                                                                       
C     ***  PRINT LONG SUMMARY LINE  ***                                 
C                                                                       
 60   IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 1) WRITE(PU,70)              
 70   FORMAT(/11H    IT   NF,6X,1HF,7X,5HRELDF,3X,6HPRELDF,3X,5HRELDX,  
     1       2X,13HMODEL  STPPAR,2X,6HD*STEP,2X,7HNPRELDF)              
      IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 2) WRITE(PU,80)              
 80   FORMAT(/11H    IT   NF,7X,1HF,8X,5HRELDF,4X,6HPRELDF,4X,5HRELDX,  
     1       3X,6HSTPPAR,3X,6HD*STEP,3X,7HNPRELDF)                      
      IV(NEEDHD) = 0                                                    
      NRELDF = ZERO                                                     
      IF (OLDF .GT. ZERO) NRELDF = V(NREDUC) / OLDF                     
      IF (ALG .EQ. 2) GO TO 90                                          
      M = IV(SUSED)                                                     
      WRITE(PU,100) IV(NITER), NF, V(F), RELDF, PRELDF, V(RELDX),       
     1             MODEL1(M), MODEL2(M), V(STPPAR), V(DSTNRM), NRELDF   
      GO TO 120                                                         
C                                                                       
 90   WRITE(PU,110) IV(NITER), NF, V(F), RELDF, PRELDF,                 
     1             V(RELDX), V(STPPAR), V(DSTNRM), NRELDF               
 100  FORMAT(I6,I5,D10.3,2D9.2,D8.1,A3,A4,2D8.1,D9.2)                   
 110  FORMAT(I6,I5,D11.3,2D10.2,3D9.1,D10.2)                            
C                                                                       
 120  IF (IV1 .LE. 2) GO TO 999                                         
      I = IV(STATPR)                                                    
      IF (I .EQ. (-1)) GO TO 460                                        
      IF (I + IV1 .LT. 0) GO TO 460                                     
      GO TO (999, 999, 130, 150, 170, 190, 210, 230, 250, 270, 290, 310,
     1       330, 350, 500),  IV1                                       
C                                                                       
 130  WRITE(PU,140)                                                     
 140  FORMAT(/26H ***** X-CONVERGENCE *****)                            
      GO TO 430                                                         
C                                                                       
 150  WRITE(PU,160)                                                     
 160  FORMAT(/42H ***** RELATIVE FUNCTION CONVERGENCE *****)            
      GO TO 430                                                         
C                                                                       
 170  WRITE(PU,180)                                                     
 180  FORMAT(/49H ***** X- AND RELATIVE FUNCTION CONVERGENCE *****)     
      GO TO 430                                                         
C                                                                       
 190  WRITE(PU,200)                                                     
 200  FORMAT(/42H ***** ABSOLUTE FUNCTION CONVERGENCE *****)            
      GO TO 430                                                         
C                                                                       
 210  WRITE(PU,220)                                                     
 220  FORMAT(/33H ***** SINGULAR CONVERGENCE *****)                     
      GO TO 430                                                         
C                                                                       
 230  WRITE(PU,240)                                                     
 240  FORMAT(/30H ***** FALSE CONVERGENCE *****)                        
      GO TO 430                                                         
C                                                                       
 250  WRITE(PU,260)                                                     
 260  FORMAT(/38H ***** FUNCTION EVALUATION LIMIT *****)                
      GO TO 430                                                         
C                                                                       
 270  WRITE(PU,280)                                                     
 280  FORMAT(/28H ***** ITERATION LIMIT *****)                          
      GO TO 430                                                         
C                                                                       
 290  WRITE(PU,300)                                                     
 300  FORMAT(/18H ***** STOPX *****)                                    
      GO TO 430                                                         
C                                                                       
 310  WRITE(PU,320)                                                     
 320  FORMAT(/44H ***** INITIAL F(X) CANNOT BE COMPUTED *****)          
C                                                                       
      GO TO 390                                                         
C                                                                       
 330  WRITE(PU,340)                                                     
 340  FORMAT(/37H ***** BAD PARAMETERS TO ASSESS *****)                 
      GO TO 999                                                         
C                                                                       
 350  WRITE(PU,360)                                                     
 360  FORMAT(/43H ***** GRADIENT COULD NOT BE COMPUTED *****)           
      IF (IV(NITER) .GT. 0) GO TO 460                                   
      GO TO 390                                                         
C                                                                       
 370  WRITE(PU,380) IV(1)                                               
 380  FORMAT(/14H ***** IV(1) =,I5,6H *****)                            
      GO TO 999                                                         
C                                                                       
C  ***  INITIAL CALL ON DITSUM  ***                                     
C                                                                       
 390  IF (IV(X0PRT) .NE. 0) WRITE(PU,400) (I, X(I), D(I), I = 1, P)     
 400  FORMAT(/23H     I     INITIAL X(I),8X,4HD(I)//(1X,I5,D17.6,D14.3))
C     *** THE FOLLOWING ARE TO AVOID UNDEFINED VARIABLES WHEN THE       
C     *** FUNCTION EVALUATION LIMIT IS 1...                             
      V(DSTNRM) = ZERO                                                  
      V(FDIF) = ZERO                                                    
      V(NREDUC) = ZERO                                                  
      V(PREDUC) = ZERO                                                  
      V(RELDX) = ZERO                                                   
      IF (IV1 .GE. 12) GO TO 999                                        
      IV(NEEDHD) = 0                                                    
      IV(PRNTIT) = 0                                                    
      IF (OL .EQ. 0) GO TO 999                                          
      IF (OL .LT. 0 .AND. ALG .EQ. 1) WRITE(PU,30)                      
      IF (OL .LT. 0 .AND. ALG .EQ. 2) WRITE(PU,40)                      
      IF (OL .GT. 0 .AND. ALG .EQ. 1) WRITE(PU,70)                      
      IF (OL .GT. 0 .AND. ALG .EQ. 2) WRITE(PU,80)                      
      IF (ALG .EQ. 1) WRITE(PU,410) IV(NFCALL), V(F)                    
      IF (ALG .EQ. 2) WRITE(PU,420) IV(NFCALL), V(F)                    
 410  FORMAT(/6H     0,I5,D10.3)                                        
 420  FORMAT(/6H     0,I5,D11.3)                                        
      GO TO 999                                                         
C                                                                       
C  ***  PRINT VARIOUS INFORMATION REQUESTED ON SOLUTION  ***            
C                                                                       
 430  IV(NEEDHD) = 1                                                    
      IF (IV(STATPR) .LE. 0) GO TO 460                                  
         OLDF = DMAX1(DABS(V(F0)), DABS(V(F)))                          
         PRELDF = ZERO                                                  
         NRELDF = ZERO                                                  
         IF (OLDF .LE. ZERO) GO TO 440                                  
              PRELDF = V(PREDUC) / OLDF                                 
              NRELDF = V(NREDUC) / OLDF                                 
 440     NF = IV(NFCALL) - IV(NFCOV)                                    
         NG = IV(NGCALL) - IV(NGCOV)                                    
         WRITE(PU,450) V(F), V(RELDX), NF, NG, PRELDF, NRELDF           
 450  FORMAT(/9H FUNCTION,D17.6,8H   RELDX,D17.3/12H FUNC. EVALS,       
     1   I8,9X,11HGRAD. EVALS,I8/7H PRELDF,D16.3,6X,7HNPRELDF,D15.3)    
C                                                                       
 460  IF (IV(SOLPRT) .EQ. 0) GO TO 999                                  
         IV(NEEDHD) = 1                                                 
         IF (IV(ALGSAV) .GT. 2) GO TO 999                               
         WRITE(PU,470)                                                  
 470  FORMAT(/22H     I      FINAL X(I),8X,4HD(I),10X,4HG(I)/)          
         DO 480 I = 1, P                                                
 480          WRITE(PU,490) I, X(I), D(I), G(I)                         
 490     FORMAT(1X,I5,D16.6,2D14.3)                                     
      GO TO 999                                                         
C                                                                       
 500  WRITE(PU,510)                                                     
 510  FORMAT(/24H INCONSISTENT DIMENSIONS)                              
 999  RETURN                                                            
C  ***  LAST CARD OF DITSUM FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DV7VMP(N, X, Y, Z, K)                                  
C                                                                       
C ***  SET X(I) = Y(I) * Z(I)**K, 1 .LE. I .LE. N (FOR K = 1 OR -1)  ***
C                                                                       
      INTEGER N, K                                                      
      DOUBLE PRECISION X(N), Y(N), Z(N)                                 
      INTEGER I                                                         
C                                                                       
      IF (K .GE. 0) GO TO 20                                            
      DO 10 I = 1, N                                                    
 10      X(I) = Y(I) / Z(I)                                             
      GO TO 999                                                         
C                                                                       
 20   DO 30 I = 1, N                                                    
 30      X(I) = Y(I) * Z(I)                                             
 999  RETURN                                                            
C  ***  LAST CARD OF DV7VMP FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DV7IPR(N, IP, X)                                       
C                                                                       
C     PERMUTE X SO THAT X.OUTPUT(I) = X.INPUT(IP(I)).                   
C     IP IS UNCHANGED ON OUTPUT.                                        
C                                                                       
      INTEGER N                                                         
      INTEGER IP(N)                                                     
      DOUBLE PRECISION X(N)                                             
C                                                                       
      INTEGER I, J, K                                                   
      DOUBLE PRECISION T                                                
      DO 30 I = 1, N                                                    
         J = IP(I)                                                      
         IF (J .EQ. I) GO TO 30                                         
         IF (J .GT. 0) GO TO 10                                         
            IP(I) = -J                                                  
            GO TO 30                                                    
 10      T = X(I)                                                       
         K = I                                                          
 20      X(K) = X(J)                                                    
         K = J                                                          
         J = IP(K)                                                      
         IP(K) = -J                                                     
         IF (J .GT. I) GO TO 20                                         
         X(K) = T                                                       
 30      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DV7IPR FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DG7QTS(D, DIG, DIHDI, KA, L, P, STEP, V, W)            
C                                                                       
C  *** COMPUTE GOLDFELD-QUANDT-TROTTER STEP BY MORE-HEBDEN TECHNIQUE ***
C  ***  (NL2SOL VERSION 2.2), MODIFIED A LA MORE AND SORENSEN  ***      
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER KA, P                                                     
      DOUBLE PRECISION D(P), DIG(P), DIHDI(1), L(1), V(21), STEP(P),    
     1                 W(1)                                             
C     DIMENSION DIHDI(P*(P+1)/2), L(P*(P+1)/2), W(4*P+7)                
C                                                                       
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                                                                       
C  ***  PURPOSE  ***                                                    
C                                                                       
C        GIVEN THE (COMPACTLY STORED) LOWER TRIANGLE OF A SCALED        
C     HESSIAN (APPROXIMATION) AND A NONZERO SCALED GRADIENT VECTOR,     
C     THIS SUBROUTINE COMPUTES A GOLDFELD-QUANDT-TROTTER STEP OF        
C     APPROXIMATE LENGTH V(RADIUS) BY THE MORE-HEBDEN TECHNIQUE.  IN    
C     OTHER WORDS, STEP IS COMPUTED TO (APPROXIMATELY) MINIMIZE         
C     PSI(STEP) = (G**T)*STEP + 0.5*(STEP**T)*H*STEP  SUCH THAT THE     
C     2-NORM OF D*STEP IS AT MOST (APPROXIMATELY) V(RADIUS), WHERE      
C     G  IS THE GRADIENT,  H  IS THE HESSIAN, AND  D  IS A DIAGONAL     
C     SCALE MATRIX WHOSE DIAGONAL IS STORED IN THE PARAMETER D.         
C     (DG7QTS ASSUMES  DIG = D**-1 * G  AND  DIHDI = D**-1 * H * D**-1.)
C                                                                       
C  ***  PARAMETER DESCRIPTION  ***                                      
C                                                                       
C     D (IN)  = THE SCALE VECTOR, I.E. THE DIAGONAL OF THE SCALE        
C              MATRIX  D  MENTIONED ABOVE UNDER PURPOSE.                
C   DIG (IN)  = THE SCALED GRADIENT VECTOR, D**-1 * G.  IF G = 0, THEN  
C              STEP = 0  AND  V(STPPAR) = 0  ARE RETURNED.              
C DIHDI (IN)  = LOWER TRIANGLE OF THE SCALED HESSIAN (APPROXIMATION),   
C              I.E., D**-1 * H * D**-1, STORED COMPACTLY BY ROWS., I.E.,
C              IN THE ORDER (1,1), (2,1), (2,2), (3,1), (3,2), ETC.     
C    KA (I/O) = THE NUMBER OF HEBDEN ITERATIONS (SO FAR) TAKEN TO DETER-
C              MINE STEP.  KA .LT. 0 ON INPUT MEANS THIS IS THE FIRST   
C              ATTEMPT TO DETERMINE STEP (FOR THE PRESENT DIG AND DIHDI)
C              -- KA IS INITIALIZED TO 0 IN THIS CASE.  OUTPUT WITH     
C              KA = 0  (OR V(STPPAR) = 0)  MEANS  STEP = -(H**-1)*G.    
C     L (I/O) = WORKSPACE OF LENGTH P*(P+1)/2 FOR CHOLESKY FACTORS.     
C     P (IN)  = NUMBER OF PARAMETERS -- THE HESSIAN IS A  P X P  MATRIX.
C  STEP (I/O) = THE STEP COMPUTED.                                      
C     V (I/O) CONTAINS VARIOUS CONSTANTS AND VARIABLES DESCRIBED BELOW. 
C     W (I/O) = WORKSPACE OF LENGTH 4*P + 6.                            
C                                                                       
C  ***  ENTRIES IN V  ***                                               
C                                                                       
C V(DGNORM) (I/O) = 2-NORM OF (D**-1)*G.                                
C V(DSTNRM) (OUTPUT) = 2-NORM OF D*STEP.                                
C V(DST0)   (I/O) = 2-NORM OF D*(H**-1)*G (FOR POS. DEF. H ONLY), OR    
C             OVERESTIMATE OF SMALLEST EIGENVALUE OF (D**-1)*H*(D**-1). 
C V(EPSLON) (IN)  = MAX. REL. ERROR ALLOWED FOR PSI(STEP).  FOR THE     
C             STEP RETURNED, PSI(STEP) WILL EXCEED ITS OPTIMAL VALUE    
C             BY LESS THAN -V(EPSLON)*PSI(STEP).  SUGGESTED VALUE = 0.1.
C V(GTSTEP) (OUT) = INNER PRODUCT BETWEEN G AND STEP.                   
C V(NREDUC) (OUT) = PSI(-(H**-1)*G) = PSI(NEWTON STEP)  (FOR POS. DEF.  
C             H ONLY -- V(NREDUC) IS SET TO ZERO OTHERWISE).            
C V(PHMNFC) (IN)  = TOL. (TOGETHER WITH V(PHMXFC)) FOR ACCEPTING STEP   
C             (MORE*S SIGMA).  THE ERROR V(DSTNRM) - V(RADIUS) MUST LIE 
C             BETWEEN V(PHMNFC)*V(RADIUS) AND V(PHMXFC)*V(RADIUS).      
C V(PHMXFC) (IN)  (SEE V(PHMNFC).)                                      
C             SUGGESTED VALUES -- V(PHMNFC) = -0.25, V(PHMXFC) = 0.5.   
C V(PREDUC) (OUT) = PSI(STEP) = PREDICTED OBJ. FUNC. REDUCTION FOR STEP.
C V(RADIUS) (IN)  = RADIUS OF CURRENT (SCALED) TRUST REGION.            
C V(RAD0)   (I/O) = VALUE OF V(RADIUS) FROM PREVIOUS CALL.              
C V(STPPAR) (I/O) IS NORMALLY THE MARQUARDT PARAMETER, I.E. THE ALPHA   
C             DESCRIBED BELOW UNDER ALGORITHM NOTES.  IF H + ALPHA*D**2 
C             (SEE ALGORITHM NOTES) IS (NEARLY) SINGULAR, HOWEVER,      
C             THEN V(STPPAR) = -ALPHA.                                  
C                                                                       
C  ***  USAGE NOTES  ***                                                
C                                                                       
C     IF IT IS DESIRED TO RECOMPUTE STEP USING A DIFFERENT VALUE OF     
C     V(RADIUS), THEN THIS ROUTINE MAY BE RESTARTED BY CALLING IT       
C     WITH ALL PARAMETERS UNCHANGED EXCEPT V(RADIUS).  (THIS EXPLAINS   
C     WHY STEP AND W ARE LISTED AS I/O).  ON AN INITIAL CALL (ONE WITH  
C     KA .LT. 0), STEP AND W NEED NOT BE INITIALIZED AND ONLY COMPO-    
C     NENTS V(EPSLON), V(STPPAR), V(PHMNFC), V(PHMXFC), V(RADIUS), AND  
C     V(RAD0) OF V MUST BE INITIALIZED.                                 
C                                                                       
C  ***  ALGORITHM NOTES  ***                                            
C                                                                       
C        THE DESIRED G-Q-T STEP (REF. 2, 3, 4, 6) SATISFIES             
C     (H + ALPHA*D**2)*STEP = -G  FOR SOME NONNEGATIVE ALPHA SUCH THAT  
C     H + ALPHA*D**2 IS POSITIVE SEMIDEFINITE.  ALPHA AND STEP ARE      
C     COMPUTED BY A SCHEME ANALOGOUS TO THE ONE DESCRIBED IN REF. 5.    
C     ESTIMATES OF THE SMALLEST AND LARGEST EIGENVALUES OF THE HESSIAN  
C     ARE OBTAINED FROM THE GERSCHGORIN CIRCLE THEOREM ENHANCED BY A    
C     SIMPLE FORM OF THE SCALING DESCRIBED IN REF. 7.  CASES IN WHICH   
C     H + ALPHA*D**2 IS NEARLY (OR EXACTLY) SINGULAR ARE HANDLED BY     
C     THE TECHNIQUE DISCUSSED IN REF. 2.  IN THESE CASES, A STEP OF     
C     (EXACT) LENGTH V(RADIUS) IS RETURNED FOR WHICH PSI(STEP) EXCEEDS  
C     ITS OPTIMAL VALUE BY LESS THAN -V(EPSLON)*PSI(STEP).  THE TEST    
C     SUGGESTED IN REF. 6 FOR DETECTING THE SPECIAL CASE IS PERFORMED   
C     ONCE TWO MATRIX FACTORIZATIONS HAVE BEEN DONE -- DOING SO SOONER  
C     SEEMS TO DEGRADE THE PERFORMANCE OF OPTIMIZATION ROUTINES THAT    
C     CALL THIS ROUTINE.                                                
C                                                                       
C  ***  FUNCTIONS AND SUBROUTINES CALLED  ***                           
C                                                                       
C DD7TPR - RETURNS INNER PRODUCT OF TWO VECTORS.                        
C DL7ITV - APPLIES INVERSE-TRANSPOSE OF COMPACT LOWER TRIANG. MATRIX.   
C DL7IVM - APPLIES INVERSE OF COMPACT LOWER TRIANG. MATRIX.             
C DL7SRT  - FINDS CHOLESKY FACTOR (OF COMPACTLY STORED LOWER TRIANG.).  
C DL7SVN - RETURNS APPROX. TO MIN. SING. VALUE OF LOWER TRIANG. MATRIX. 
C DR7MDC - RETURNS MACHINE-DEPENDENT CONSTANTS.                         
C DV2NRM - RETURNS 2-NORM OF A VECTOR.                                  
C                                                                       
C  ***  REFERENCES  ***                                                 
C                                                                       
C 1.  DENNIS, J.E., GAY, D.M., AND WELSCH, R.E. (1981), AN ADAPTIVE     
C             NONLINEAR LEAST-SQUARES ALGORITHM, ACM TRANS. MATH.       
C             SOFTWARE, VOL. 7, NO. 3.                                  
C 2.  GAY, D.M. (1981), COMPUTING OPTIMAL LOCALLY CONSTRAINED STEPS,    
C             SIAM J. SCI. STATIST. COMPUTING, VOL. 2, NO. 2, PP.       
C             186-197.                                                  
C 3.  GOLDFELD, S.M., QUANDT, R.E., AND TROTTER, H.F. (1966),           
C             MAXIMIZATION BY QUADRATIC HILL-CLIMBING, ECONOMETRICA 34, 
C             PP. 541-551.                                              
C 4.  HEBDEN, M.D. (1973), AN ALGORITHM FOR MINIMIZATION USING EXACT    
C             SECOND DERIVATIVES, REPORT T.P. 515, THEORETICAL PHYSICS  
C             DIV., A.E.R.E. HARWELL, OXON., ENGLAND.                   
C 5.  MORE, J.J. (1978), THE LEVENBERG-MARQUARDT ALGORITHM, IMPLEMEN-   
C             TATION AND THEORY, PP.105-116 OF SPRINGER LECTURE NOTES   
C             IN MATHEMATICS NO. 630, EDITED BY G.A. WATSON, SPRINGER-  
C             VERLAG, BERLIN AND NEW YORK.                              
C 6.  MORE, J.J., AND SORENSEN, D.C. (1981), COMPUTING A TRUST REGION   
C             STEP, TECHNICAL REPORT ANL-81-83, ARGONNE NATIONAL LAB.   
C 7.  VARGA, R.S. (1965), MINIMAL GERSCHGORIN SETS, PACIFIC J. MATH. 15,
C             PP. 719-729.                                              
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY.                                            
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH           
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS         
C     MCS-7600324, DCR75-10143, 76-14311DSS, MCS76-11989, AND           
C     MCS-7906671.                                                      
C                                                                       
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      LOGICAL RESTRT                                                    
      INTEGER DGGDMX, DIAG, DIAG0, DSTSAV, EMAX, EMIN, I, IM1, INC, IRC,
     1        J, K, KALIM, KAMIN, K1, LK0, PHIPIN, Q, Q0, UK0, X        
      DOUBLE PRECISION ALPHAK, AKI, AKK, DELTA, DST, EPS, GTSTA, LK,    
     1                 OLDPHI, PHI, PHIMAX, PHIMIN, PSIFAC, RAD, RADSQ, 
     2                 ROOT, SI, SK, SW, T, TWOPSI, T1, T2, UK, WI      
C                                                                       
C     ***  CONSTANTS  ***                                               
      DOUBLE PRECISION BIG, DGXFAC, EPSFAC, FOUR, HALF, KAPPA, NEGONE,  
     1                 ONE, P001, SIX, THREE, TWO, ZERO                 
C                                                                       
C  ***  INTRINSIC FUNCTIONS  ***                                        
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      DOUBLE PRECISION DD7TPR, DL7SVN, DR7MDC, DV2NRM                   
      EXTERNAL DD7TPR, DL7ITV, DL7IVM,DL7SRT, DL7SVN, DR7MDC, DV2NRM    
C                                                                       
C  ***  SUBSCRIPTS FOR V  ***                                           
C                                                                       
      INTEGER DGNORM, DSTNRM, DST0, EPSLON, GTSTEP, STPPAR, NREDUC,     
     1        PHMNFC, PHMXFC, PREDUC, RADIUS, RAD0                      
C/6                                                                     
C     DATA DGNORM/1/, DSTNRM/2/, DST0/3/, EPSLON/19/, GTSTEP/4/,        
C    1     NREDUC/6/, PHMNFC/20/, PHMXFC/21/, PREDUC/7/, RADIUS/8/,     
C    2     RAD0/9/, STPPAR/5/                                           
C/7                                                                     
      PARAMETER (DGNORM=1, DSTNRM=2, DST0=3, EPSLON=19, GTSTEP=4,       
     1           NREDUC=6, PHMNFC=20, PHMXFC=21, PREDUC=7, RADIUS=8,    
     2           RAD0=9, STPPAR=5)                                      
C/                                                                      
C                                                                       
C/6                                                                     
C     DATA EPSFAC/50.0D+0/, FOUR/4.0D+0/, HALF/0.5D+0/,                 
C    1     KAPPA/2.0D+0/, NEGONE/-1.0D+0/, ONE/1.0D+0/, P001/1.0D-3/,   
C    2     SIX/6.0D+0/, THREE/3.0D+0/, TWO/2.0D+0/, ZERO/0.0D+0/        
C/7                                                                     
      PARAMETER (EPSFAC=50.0D+0, FOUR=4.0D+0, HALF=0.5D+0,              
     1     KAPPA=2.0D+0, NEGONE=-1.0D+0, ONE=1.0D+0, P001=1.0D-3,       
     2     SIX=6.0D+0, THREE=3.0D+0, TWO=2.0D+0, ZERO=0.0D+0)           
      SAVE DGXFAC                                                       
C/                                                                      
      DATA BIG/0.D+0/, DGXFAC/0.D+0/                                    
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
      IF (BIG .LE. ZERO) BIG = DR7MDC(6)                                
C                                                                       
C     ***  STORE LARGEST ABS. ENTRY IN (D**-1)*H*(D**-1) AT W(DGGDMX).  
      DGGDMX = P + 1                                                    
C     ***  STORE GERSCHGORIN OVER- AND UNDERESTIMATES OF THE LARGEST    
C     ***  AND SMALLEST EIGENVALUES OF (D**-1)*H*(D**-1) AT W(EMAX)     
C     ***  AND W(EMIN) RESPECTIVELY.                                    
      EMAX = DGGDMX + 1                                                 
      EMIN = EMAX + 1                                                   
C     ***  FOR USE IN RECOMPUTING STEP, THE FINAL VALUES OF LK, UK, DST,
C     ***  AND THE INVERSE DERIVATIVE OF MORE*S PHI AT 0 (FOR POS. DEF. 
C     ***  H) ARE STORED IN W(LK0), W(UK0), W(DSTSAV), AND W(PHIPIN)    
C     ***  RESPECTIVELY.                                                
      LK0 = EMIN + 1                                                    
      PHIPIN = LK0 + 1                                                  
      UK0 = PHIPIN + 1                                                  
      DSTSAV = UK0 + 1                                                  
C     ***  STORE DIAG OF (D**-1)*H*(D**-1) IN W(DIAG),...,W(DIAG0+P).   
      DIAG0 = DSTSAV                                                    
      DIAG = DIAG0 + 1                                                  
C     ***  STORE -D*STEP IN W(Q),...,W(Q0+P).                           
      Q0 = DIAG0 + P                                                    
      Q = Q0 + 1                                                        
C     ***  ALLOCATE STORAGE FOR SCRATCH VECTOR X  ***                   
      X = Q + P                                                         
      RAD = V(RADIUS)                                                   
      RADSQ = RAD**2                                                    
C     ***  PHITOL = MAX. ERROR ALLOWED IN DST = V(DSTNRM) = 2-NORM OF   
C     ***  D*STEP.                                                      
      PHIMAX = V(PHMXFC) * RAD                                          
      PHIMIN = V(PHMNFC) * RAD                                          
      PSIFAC = BIG                                                      
      T1 = TWO * V(EPSLON) / (THREE * (FOUR * (V(PHMNFC) + ONE) *       
     1                       (KAPPA + ONE)  +  KAPPA  +  TWO) * RAD)    
      IF (T1 .LT. BIG*DMIN1(RAD,ONE)) PSIFAC = T1 / RAD                 
C     ***  OLDPHI IS USED TO DETECT LIMITS OF NUMERICAL ACCURACY.  IF   
C     ***  WE RECOMPUTE STEP AND IT DOES NOT CHANGE, THEN WE ACCEPT IT. 
      OLDPHI = ZERO                                                     
      EPS = V(EPSLON)                                                   
      IRC = 0                                                           
      RESTRT = .FALSE.                                                  
      KALIM = KA + 50                                                   
C                                                                       
C  ***  START OR RESTART, DEPENDING ON KA  ***                          
C                                                                       
      IF (KA .GE. 0) GO TO 290                                          
C                                                                       
C  ***  FRESH START  ***                                                
C                                                                       
      K = 0                                                             
      UK = NEGONE                                                       
      KA = 0                                                            
      KALIM = 50                                                        
      V(DGNORM) = DV2NRM(P, DIG)                                        
      V(NREDUC) = ZERO                                                  
      V(DST0) = ZERO                                                    
      KAMIN = 3                                                         
      IF (V(DGNORM) .EQ. ZERO) KAMIN = 0                                
C                                                                       
C     ***  STORE DIAG(DIHDI) IN W(DIAG0+1),...,W(DIAG0+P)  ***          
C                                                                       
      J = 0                                                             
      DO 10 I = 1, P                                                    
         J = J + I                                                      
         K1 = DIAG0 + I                                                 
         W(K1) = DIHDI(J)                                               
 10      CONTINUE                                                       
C                                                                       
C     ***  DETERMINE W(DGGDMX), THE LARGEST ELEMENT OF DIHDI  ***       
C                                                                       
      T1 = ZERO                                                         
      J = P * (P + 1) / 2                                               
      DO 20 I = 1, J                                                    
         T = DABS(DIHDI(I))                                             
         IF (T1 .LT. T) T1 = T                                          
 20      CONTINUE                                                       
      W(DGGDMX) = T1                                                    
C                                                                       
C  ***  TRY ALPHA = 0  ***                                              
C                                                                       
 30   CALL DL7SRT(1, P, L, DIHDI, IRC)                                  
      IF (IRC .EQ. 0) GO TO 50                                          
C        ***  INDEF. H -- UNDERESTIMATE SMALLEST EIGENVALUE, USE THIS   
C        ***  ESTIMATE TO INITIALIZE LOWER BOUND LK ON ALPHA.           
         J = IRC*(IRC+1)/2                                              
         T = L(J)                                                       
         L(J) = ONE                                                     
         DO 40 I = 1, IRC                                               
 40           W(I) = ZERO                                               
         W(IRC) = ONE                                                   
         CALL DL7ITV(IRC, W, L, W)                                      
         T1 = DV2NRM(IRC, W)                                            
         LK = -T / T1 / T1                                              
         V(DST0) = -LK                                                  
         IF (RESTRT) GO TO 210                                          
         GO TO 70                                                       
C                                                                       
C     ***  POSITIVE DEFINITE H -- COMPUTE UNMODIFIED NEWTON STEP.  ***  
 50   LK = ZERO                                                         
      T = DL7SVN(P, L, W(Q), W(Q))                                      
      IF (T .GE. ONE) GO TO 60                                          
         IF (V(DGNORM) .GE. T*T*BIG) GO TO 70                           
 60   CALL DL7IVM(P, W(Q), L, DIG)                                      
      GTSTA = DD7TPR(P, W(Q), W(Q))                                     
      V(NREDUC) = HALF * GTSTA                                          
      CALL DL7ITV(P, W(Q), L, W(Q))                                     
      DST = DV2NRM(P, W(Q))                                             
      V(DST0) = DST                                                     
      PHI = DST - RAD                                                   
      IF (PHI .LE. PHIMAX) GO TO 260                                    
      IF (RESTRT) GO TO 210                                             
C                                                                       
C  ***  PREPARE TO COMPUTE GERSCHGORIN ESTIMATES OF LARGEST (AND        
C  ***  SMALLEST) EIGENVALUES.  ***                                     
C                                                                       
 70   K = 0                                                             
      DO 100 I = 1, P                                                   
         WI = ZERO                                                      
         IF (I .EQ. 1) GO TO 90                                         
         IM1 = I - 1                                                    
         DO 80 J = 1, IM1                                               
              K = K + 1                                                 
              T = DABS(DIHDI(K))                                        
              WI = WI + T                                               
              W(J) = W(J) + T                                           
 80           CONTINUE                                                  
 90      W(I) = WI                                                      
         K = K + 1                                                      
 100     CONTINUE                                                       
C                                                                       
C  ***  (UNDER-)ESTIMATE SMALLEST EIGENVALUE OF (D**-1)*H*(D**-1)  ***  
C                                                                       
      K = 1                                                             
      T1 = W(DIAG) - W(1)                                               
      IF (P .LE. 1) GO TO 120                                           
      DO 110 I = 2, P                                                   
         J = DIAG0 + I                                                  
         T = W(J) - W(I)                                                
         IF (T .GE. T1) GO TO 110                                       
              T1 = T                                                    
              K = I                                                     
 110     CONTINUE                                                       
C                                                                       
 120  SK = W(K)                                                         
      J = DIAG0 + K                                                     
      AKK = W(J)                                                        
      K1 = K*(K-1)/2 + 1                                                
      INC = 1                                                           
      T = ZERO                                                          
      DO 150 I = 1, P                                                   
         IF (I .EQ. K) GO TO 130                                        
         AKI = DABS(DIHDI(K1))                                          
         SI = W(I)                                                      
         J = DIAG0 + I                                                  
         T1 = HALF * (AKK - W(J) + SI - AKI)                            
         T1 = T1 + DSQRT(T1*T1 + SK*AKI)                                
         IF (T .LT. T1) T = T1                                          
         IF (I .LT. K) GO TO 140                                        
 130     INC = I                                                        
 140     K1 = K1 + INC                                                  
 150     CONTINUE                                                       
C                                                                       
      W(EMIN) = AKK - T                                                 
      UK = V(DGNORM)/RAD - W(EMIN)                                      
      IF (V(DGNORM) .EQ. ZERO) UK = UK + P001 + P001*UK                 
      IF (UK .LE. ZERO) UK = P001                                       
C                                                                       
C  ***  COMPUTE GERSCHGORIN (OVER-)ESTIMATE OF LARGEST EIGENVALUE  ***  
C                                                                       
      K = 1                                                             
      T1 = W(DIAG) + W(1)                                               
      IF (P .LE. 1) GO TO 170                                           
      DO 160 I = 2, P                                                   
         J = DIAG0 + I                                                  
         T = W(J) + W(I)                                                
         IF (T .LE. T1) GO TO 160                                       
              T1 = T                                                    
              K = I                                                     
 160     CONTINUE                                                       
C                                                                       
 170  SK = W(K)                                                         
      J = DIAG0 + K                                                     
      AKK = W(J)                                                        
      K1 = K*(K-1)/2 + 1                                                
      INC = 1                                                           
      T = ZERO                                                          
      DO 200 I = 1, P                                                   
         IF (I .EQ. K) GO TO 180                                        
         AKI = DABS(DIHDI(K1))                                          
         SI = W(I)                                                      
         J = DIAG0 + I                                                  
         T1 = HALF * (W(J) + SI - AKI - AKK)                            
         T1 = T1 + DSQRT(T1*T1 + SK*AKI)                                
         IF (T .LT. T1) T = T1                                          
         IF (I .LT. K) GO TO 190                                        
 180     INC = I                                                        
 190     K1 = K1 + INC                                                  
 200     CONTINUE                                                       
C                                                                       
      W(EMAX) = AKK + T                                                 
      LK = DMAX1(LK, V(DGNORM)/RAD - W(EMAX))                           
C                                                                       
C     ***  ALPHAK = CURRENT VALUE OF ALPHA (SEE ALG. NOTES ABOVE).  WE  
C     ***  USE MORE*S SCHEME FOR INITIALIZING IT.                       
      ALPHAK = DABS(V(STPPAR)) * V(RAD0)/RAD                            
      ALPHAK = DMIN1(UK, DMAX1(ALPHAK, LK))                             
C                                                                       
      IF (IRC .NE. 0) GO TO 210                                         
C                                                                       
C  ***  COMPUTE L0 FOR POSITIVE DEFINITE H  ***                         
C                                                                       
      CALL DL7IVM(P, W, L, W(Q))                                        
      T = DV2NRM(P, W)                                                  
      W(PHIPIN) = RAD / T / T                                           
      LK = DMAX1(LK, PHI*W(PHIPIN))                                     
C                                                                       
C  ***  SAFEGUARD ALPHAK AND ADD ALPHAK*I TO (D**-1)*H*(D**-1)  ***     
C                                                                       
 210  KA = KA + 1                                                       
      IF (-V(DST0) .GE. ALPHAK .OR. ALPHAK .LT. LK .OR. ALPHAK .GE. UK) 
     1                      ALPHAK = UK * DMAX1(P001, DSQRT(LK/UK))     
      IF (ALPHAK .LE. ZERO) ALPHAK = HALF * UK                          
      IF (ALPHAK .LE. ZERO) ALPHAK = UK                                 
      K = 0                                                             
      DO 220 I = 1, P                                                   
         K = K + I                                                      
         J = DIAG0 + I                                                  
         DIHDI(K) = W(J) + ALPHAK                                       
 220     CONTINUE                                                       
C                                                                       
C  ***  TRY COMPUTING CHOLESKY DECOMPOSITION  ***                       
C                                                                       
      CALL DL7SRT(1, P, L, DIHDI, IRC)                                  
      IF (IRC .EQ. 0) GO TO 240                                         
C                                                                       
C  ***  (D**-1)*H*(D**-1) + ALPHAK*I  IS INDEFINITE -- OVERESTIMATE     
C  ***  SMALLEST EIGENVALUE FOR USE IN UPDATING LK  ***                 
C                                                                       
      J = (IRC*(IRC+1))/2                                               
      T = L(J)                                                          
      L(J) = ONE                                                        
      DO 230 I = 1, IRC                                                 
 230     W(I) = ZERO                                                    
      W(IRC) = ONE                                                      
      CALL DL7ITV(IRC, W, L, W)                                         
      T1 = DV2NRM(IRC, W)                                               
      LK = ALPHAK - T/T1/T1                                             
      V(DST0) = -LK                                                     
      IF (UK .LT. LK) UK = LK                                           
      IF (ALPHAK .LT. LK) GO TO 210                                     
C                                                                       
C  ***  NASTY CASE -- EXACT GERSCHGORIN BOUNDS.  FUDGE LK, UK...        
C                                                                       
      T = P001 * ALPHAK                                                 
      IF (T .LE. ZERO) T = P001                                         
      LK = ALPHAK + T                                                   
      IF (UK .LE. LK) UK = LK + T                                       
      GO TO 210                                                         
C                                                                       
C  ***  ALPHAK MAKES (D**-1)*H*(D**-1) POSITIVE DEFINITE.               
C  ***  COMPUTE Q = -D*STEP, CHECK FOR CONVERGENCE.  ***                
C                                                                       
 240  CALL DL7IVM(P, W(Q), L, DIG)                                      
      GTSTA = DD7TPR(P, W(Q), W(Q))                                     
      CALL DL7ITV(P, W(Q), L, W(Q))                                     
      DST = DV2NRM(P, W(Q))                                             
      PHI = DST - RAD                                                   
      IF (PHI .LE. PHIMAX .AND. PHI .GE. PHIMIN) GO TO 270              
      IF (PHI .EQ. OLDPHI) GO TO 270                                    
      OLDPHI = PHI                                                      
      IF (PHI .LT. ZERO) GO TO 330                                      
C                                                                       
C  ***  UNACCEPTABLE ALPHAK -- UPDATE LK, UK, ALPHAK  ***               
C                                                                       
 250  IF (KA .GE. KALIM) GO TO 270                                      
C     ***  THE FOLLOWING DMIN1 IS NECESSARY BECAUSE OF RESTARTS  ***    
      IF (PHI .LT. ZERO) UK = DMIN1(UK, ALPHAK)                         
C     *** KAMIN = 0 ONLY IFF THE GRADIENT VANISHES  ***                 
      IF (KAMIN .EQ. 0) GO TO 210                                       
      CALL DL7IVM(P, W, L, W(Q))                                        
C     *** THE FOLLOWING, COMMENTED CALCULATION OF ALPHAK IS SOMETIMES   
C     *** SAFER BUT WORSE IN PERFORMANCE...                             
C     T1 = DST / DV2NRM(P, W)                                           
C     ALPHAK = ALPHAK  +  T1 * (PHI/RAD) * T1                           
      T1 = DV2NRM(P, W)                                                 
      ALPHAK = ALPHAK  +  (PHI/T1) * (DST/T1) * (DST/RAD)               
      LK = DMAX1(LK, ALPHAK)                                            
      ALPHAK = LK                                                       
      GO TO 210                                                         
C                                                                       
C  ***  ACCEPTABLE STEP ON FIRST TRY  ***                               
C                                                                       
 260  ALPHAK = ZERO                                                     
C                                                                       
C  ***  SUCCESSFUL STEP IN GENERAL.  COMPUTE STEP = -(D**-1)*Q  ***     
C                                                                       
 270  DO 280 I = 1, P                                                   
         J = Q0 + I                                                     
         STEP(I) = -W(J)/D(I)                                           
 280     CONTINUE                                                       
      V(GTSTEP) = -GTSTA                                                
      V(PREDUC) = HALF * (DABS(ALPHAK)*DST*DST + GTSTA)                 
      GO TO 410                                                         
C                                                                       
C                                                                       
C  ***  RESTART WITH NEW RADIUS  ***                                    
C                                                                       
 290  IF (V(DST0) .LE. ZERO .OR. V(DST0) - RAD .GT. PHIMAX) GO TO 310   
C                                                                       
C     ***  PREPARE TO RETURN NEWTON STEP  ***                           
C                                                                       
         RESTRT = .TRUE.                                                
         KA = KA + 1                                                    
         K = 0                                                          
         DO 300 I = 1, P                                                
              K = K + I                                                 
              J = DIAG0 + I                                             
              DIHDI(K) = W(J)                                           
 300          CONTINUE                                                  
         UK = NEGONE                                                    
         GO TO 30                                                       
C                                                                       
 310  KAMIN = KA + 3                                                    
      IF (V(DGNORM) .EQ. ZERO) KAMIN = 0                                
      IF (KA .EQ. 0) GO TO 50                                           
C                                                                       
      DST = W(DSTSAV)                                                   
      ALPHAK = DABS(V(STPPAR))                                          
      PHI = DST - RAD                                                   
      T = V(DGNORM)/RAD                                                 
      UK = T - W(EMIN)                                                  
      IF (V(DGNORM) .EQ. ZERO) UK = UK + P001 + P001*UK                 
      IF (UK .LE. ZERO) UK = P001                                       
      IF (RAD .GT. V(RAD0)) GO TO 320                                   
C                                                                       
C        ***  SMALLER RADIUS  ***                                       
         LK = ZERO                                                      
         IF (ALPHAK .GT. ZERO) LK = W(LK0)                              
         LK = DMAX1(LK, T - W(EMAX))                                    
         IF (V(DST0) .GT. ZERO) LK = DMAX1(LK, (V(DST0)-RAD)*W(PHIPIN)) 
         GO TO 250                                                      
C                                                                       
C     ***  BIGGER RADIUS  ***                                           
 320  IF (ALPHAK .GT. ZERO) UK = DMIN1(UK, W(UK0))                      
      LK = DMAX1(ZERO, -V(DST0), T - W(EMAX))                           
      IF (V(DST0) .GT. ZERO) LK = DMAX1(LK, (V(DST0)-RAD)*W(PHIPIN))    
      GO TO 250                                                         
C                                                                       
C  ***  DECIDE WHETHER TO CHECK FOR SPECIAL CASE... IN PRACTICE (FROM   
C  ***  THE STANDPOINT OF THE CALLING OPTIMIZATION CODE) IT SEEMS BEST  
C  ***  NOT TO CHECK UNTIL A FEW ITERATIONS HAVE FAILED -- HENCE THE    
C  ***  TEST ON KAMIN BELOW.                                            
C                                                                       
 330  DELTA = ALPHAK + DMIN1(ZERO, V(DST0))                             
      TWOPSI = ALPHAK*DST*DST + GTSTA                                   
      IF (KA .GE. KAMIN) GO TO 340                                      
C     *** IF THE TEST IN REF. 2 IS SATISFIED, FALL THROUGH TO HANDLE    
C     *** THE SPECIAL CASE (AS SOON AS THE MORE-SORENSEN TEST DETECTS   
C     *** IT).                                                          
      IF (PSIFAC .GE. BIG) GO TO 340                                    
      IF (DELTA .GE. PSIFAC*TWOPSI) GO TO 370                           
C                                                                       
C  ***  CHECK FOR THE SPECIAL CASE OF  H + ALPHA*D**2  (NEARLY)         
C  ***  SINGULAR.  USE ONE STEP OF INVERSE POWER METHOD WITH START      
C  ***  FROM DL7SVN TO OBTAIN APPROXIMATE EIGENVECTOR CORRESPONDING     
C  ***  TO SMALLEST EIGENVALUE OF (D**-1)*H*(D**-1).  DL7SVN RETURNS    
C  ***  X AND W WITH  L*W = X.                                          
C                                                                       
 340  T = DL7SVN(P, L, W(X), W)                                         
C                                                                       
C     ***  NORMALIZE W  ***                                             
      DO 350 I = 1, P                                                   
 350     W(I) = T*W(I)                                                  
C     ***  COMPLETE CURRENT INV. POWER ITER. -- REPLACE W BY (L**-T)*W. 
      CALL DL7ITV(P, W, L, W)                                           
      T2 = ONE/DV2NRM(P, W)                                             
      DO 360 I = 1, P                                                   
 360     W(I) = T2*W(I)                                                 
      T = T2 * T                                                        
C                                                                       
C  ***  NOW W IS THE DESIRED APPROXIMATE (UNIT) EIGENVECTOR AND         
C  ***  T*X = ((D**-1)*H*(D**-1) + ALPHAK*I)*W.                         
C                                                                       
      SW = DD7TPR(P, W(Q), W)                                           
      T1 = (RAD + DST) * (RAD - DST)                                    
      ROOT = DSQRT(SW*SW + T1)                                          
      IF (SW .LT. ZERO) ROOT = -ROOT                                    
      SI = T1 / (SW + ROOT)                                             
C                                                                       
C  ***  THE ACTUAL TEST FOR THE SPECIAL CASE...                         
C                                                                       
      IF ((T2*SI)**2 .LE. EPS*(DST**2 + ALPHAK*RADSQ)) GO TO 380        
C                                                                       
C  ***  UPDATE UPPER BOUND ON SMALLEST EIGENVALUE (WHEN NOT POSITIVE)   
C  ***  (AS RECOMMENDED BY MORE AND SORENSEN) AND CONTINUE...           
C                                                                       
      IF (V(DST0) .LE. ZERO) V(DST0) = DMIN1(V(DST0), T2**2 - ALPHAK)   
      LK = DMAX1(LK, -V(DST0))                                          
C                                                                       
C  ***  CHECK WHETHER WE CAN HOPE TO DETECT THE SPECIAL CASE IN         
C  ***  THE AVAILABLE ARITHMETIC.  ACCEPT STEP AS IT IS IF NOT.         
C                                                                       
C     ***  IF NOT YET AVAILABLE, OBTAIN MACHINE DEPENDENT VALUE DGXFAC. 
 370  IF (DGXFAC .EQ. ZERO) DGXFAC = EPSFAC * DR7MDC(3)                 
C                                                                       
      IF (DELTA .GT. DGXFAC*W(DGGDMX)) GO TO 250                        
         GO TO 270                                                      
C                                                                       
C  ***  SPECIAL CASE DETECTED... NEGATE ALPHAK TO INDICATE SPECIAL CASE 
C                                                                       
 380  ALPHAK = -ALPHAK                                                  
      V(PREDUC) = HALF * TWOPSI                                         
C                                                                       
C  ***  ACCEPT CURRENT STEP IF ADDING SI*W WOULD LEAD TO A              
C  ***  FURTHER RELATIVE REDUCTION IN PSI OF LESS THAN V(EPSLON)/3.     
C                                                                       
      T1 = ZERO                                                         
      T = SI*(ALPHAK*SW - HALF*SI*(ALPHAK + T*DD7TPR(P,W(X),W)))        
      IF (T .LT. EPS*TWOPSI/SIX) GO TO 390                              
         V(PREDUC) = V(PREDUC) + T                                      
         DST = RAD                                                      
         T1 = -SI                                                       
 390  DO 400 I = 1, P                                                   
         J = Q0 + I                                                     
         W(J) = T1*W(I) - W(J)                                          
         STEP(I) = W(J) / D(I)                                          
 400     CONTINUE                                                       
      V(GTSTEP) = DD7TPR(P, DIG, W(Q))                                  
C                                                                       
C  ***  SAVE VALUES FOR USE IN A POSSIBLE RESTART  ***                  
C                                                                       
 410  V(DSTNRM) = DST                                                   
      V(STPPAR) = ALPHAK                                                
      W(LK0) = LK                                                       
      W(UK0) = UK                                                       
      V(RAD0) = RAD                                                     
      W(DSTSAV) = DST                                                   
C                                                                       
C     ***  RESTORE DIAGONAL OF DIHDI  ***                               
C                                                                       
      J = 0                                                             
      DO 420 I = 1, P                                                   
         J = J + I                                                      
         K = DIAG0 + I                                                  
         DIHDI(J) = W(K)                                                
 420     CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF DG7QTS FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DL7MST(D, G, IERR, IPIVOT, KA, P, QTR, R, STEP, V, W)  
C                                                                       
C  ***  COMPUTE LEVENBERG-MARQUARDT STEP USING MORE-HEBDEN TECHNIQUE  **
C  ***  NL2SOL VERSION 2.2.  ***                                        
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER IERR, KA, P                                               
      INTEGER IPIVOT(P)                                                 
      DOUBLE PRECISION D(P), G(P), QTR(P), R(1), STEP(P), V(21), W(1)   
C     DIMENSION W(P*(P+5)/2 + 4)                                        
C                                                                       
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                                                                       
C  ***  PURPOSE  ***                                                    
C                                                                       
C        GIVEN THE R MATRIX FROM THE QR DECOMPOSITION OF A JACOBIAN     
C     MATRIX, J, AS WELL AS Q-TRANSPOSE TIMES THE CORRESPONDING         
C     RESIDUAL VECTOR, RESID, THIS SUBROUTINE COMPUTES A LEVENBERG-     
C     MARQUARDT STEP OF APPROXIMATE LENGTH V(RADIUS) BY THE MORE-       
C     TECHNIQUE.                                                        
C                                                                       
C  ***  PARAMETER DESCRIPTION  ***                                      
C                                                                       
C      D (IN)  = THE SCALE VECTOR.                                      
C      G (IN)  = THE GRADIENT VECTOR (J**T)*R.                          
C   IERR (I/O) = RETURN CODE FROM QRFACT OR DQ7RGS -- 0 MEANS R HAS     
C             FULL RANK.                                                
C IPIVOT (I/O) = PERMUTATION ARRAY FROM QRFACT OR DQ7RGS, WHICH COMPUTE 
C             QR DECOMPOSITIONS WITH COLUMN PIVOTING.                   
C     KA (I/O).  KA .LT. 0 ON INPUT MEANS THIS IS THE FIRST CALL ON     
C             DL7MST FOR THE CURRENT R AND QTR.  ON OUTPUT KA CON-      
C             TAINS THE NUMBER OF HEBDEN ITERATIONS NEEDED TO DETERMINE 
C             STEP.  KA = 0 MEANS A GAUSS-NEWTON STEP.                  
C      P (IN)  = NUMBER OF PARAMETERS.                                  
C    QTR (IN)  = (Q**T)*RESID = Q-TRANSPOSE TIMES THE RESIDUAL VECTOR.  
C      R (IN)  = THE R MATRIX, STORED COMPACTLY BY COLUMNS.             
C   STEP (OUT) = THE LEVENBERG-MARQUARDT STEP COMPUTED.                 
C      V (I/O) CONTAINS VARIOUS CONSTANTS AND VARIABLES DESCRIBED BELOW.
C      W (I/O) = WORKSPACE OF LENGTH P*(P+5)/2 + 4.                     
C                                                                       
C  ***  ENTRIES IN V  ***                                               
C                                                                       
C V(DGNORM) (I/O) = 2-NORM OF (D**-1)*G.                                
C V(DSTNRM) (I/O) = 2-NORM OF D*STEP.                                   
C V(DST0)   (I/O) = 2-NORM OF GAUSS-NEWTON STEP (FOR NONSING. J).       
C V(EPSLON) (IN) = MAX. REL. ERROR ALLOWED IN TWONORM(R)**2 MINUS       
C             TWONORM(R - J*STEP)**2.  (SEE ALGORITHM NOTES BELOW.)     
C V(GTSTEP) (OUT) = INNER PRODUCT BETWEEN G AND STEP.                   
C V(NREDUC) (OUT) = HALF THE REDUCTION IN THE SUM OF SQUARES PREDICTED  
C             FOR A GAUSS-NEWTON STEP.                                  
C V(PHMNFC) (IN)  = TOL. (TOGETHER WITH V(PHMXFC)) FOR ACCEPTING STEP   
C             (MORE*S SIGMA).  THE ERROR V(DSTNRM) - V(RADIUS) MUST LIE 
C             BETWEEN V(PHMNFC)*V(RADIUS) AND V(PHMXFC)*V(RADIUS).      
C V(PHMXFC) (IN)  (SEE V(PHMNFC).)                                      
C V(PREDUC) (OUT) = HALF THE REDUCTION IN THE SUM OF SQUARES PREDICTED  
C             BY THE STEP RETURNED.                                     
C V(RADIUS) (IN)  = RADIUS OF CURRENT (SCALED) TRUST REGION.            
C V(RAD0)   (I/O) = VALUE OF V(RADIUS) FROM PREVIOUS CALL.              
C V(STPPAR) (I/O) = MARQUARDT PARAMETER (OR ITS NEGATIVE IF THE SPECIAL 
C             CASE MENTIONED BELOW IN THE ALGORITHM NOTES OCCURS).      
C                                                                       
C NOTE -- SEE DATA STATEMENT BELOW FOR VALUES OF ABOVE SUBSCRIPTS.      
C                                                                       
C  ***  USAGE NOTES  ***                                                
C                                                                       
C     IF IT IS DESIRED TO RECOMPUTE STEP USING A DIFFERENT VALUE OF     
C     V(RADIUS), THEN THIS ROUTINE MAY BE RESTARTED BY CALLING IT       
C     WITH ALL PARAMETERS UNCHANGED EXCEPT V(RADIUS).  (THIS EXPLAINS   
C     WHY MANY PARAMETERS ARE LISTED AS I/O).  ON AN INTIIAL CALL (ONE  
C     WITH KA = -1), THE CALLER NEED ONLY HAVE INITIALIZED D, G, KA, P, 
C     QTR, R, V(EPSLON), V(PHMNFC), V(PHMXFC), V(RADIUS), AND V(RAD0).  
C                                                                       
C  ***  APPLICATION AND USAGE RESTRICTIONS  ***                         
C                                                                       
C     THIS ROUTINE IS CALLED AS PART OF THE NL2SOL (NONLINEAR LEAST-    
C     SQUARES) PACKAGE (REF. 1).                                        
C                                                                       
C  ***  ALGORITHM NOTES  ***                                            
C                                                                       
C     THIS CODE IMPLEMENTS THE STEP COMPUTATION SCHEME DESCRIBED IN     
C     REFS. 2 AND 4.  FAST GIVENS TRANSFORMATIONS (SEE REF. 3, PP. 60-  
C     62) ARE USED TO COMPUTE STEP WITH A NONZERO MARQUARDT PARAMETER.  
C        A SPECIAL CASE OCCURS IF J IS (NEARLY) SINGULAR AND V(RADIUS)  
C     IS SUFFICIENTLY LARGE.  IN THIS CASE THE STEP RETURNED IS SUCH    
C     THAT  TWONORM(R)**2 - TWONORM(R - J*STEP)**2  DIFFERS FROM ITS    
C     OPTIMAL VALUE BY LESS THAN V(EPSLON) TIMES THIS OPTIMAL VALUE,    
C     WHERE J AND R DENOTE THE ORIGINAL JACOBIAN AND RESIDUAL.  (SEE    
C     REF. 2 FOR MORE DETAILS.)                                         
C                                                                       
C  ***  FUNCTIONS AND SUBROUTINES CALLED  ***                           
C                                                                       
C DD7TPR - RETURNS INNER PRODUCT OF TWO VECTORS.                        
C DL7ITV - APPLY INVERSE-TRANSPOSE OF COMPACT LOWER TRIANG. MATRIX.     
C DL7IVM - APPLY INVERSE OF COMPACT LOWER TRIANG. MATRIX.               
C DV7CPY  - COPIES ONE VECTOR TO ANOTHER.                               
C DV2NRM - RETURNS 2-NORM OF A VECTOR.                                  
C                                                                       
C  ***  REFERENCES  ***                                                 
C                                                                       
C 1.  DENNIS, J.E., GAY, D.M., AND WELSCH, R.E. (1981), AN ADAPTIVE     
C             NONLINEAR LEAST-SQUARES ALGORITHM, ACM TRANS. MATH.       
C             SOFTWARE, VOL. 7, NO. 3.                                  
C 2.  GAY, D.M. (1981), COMPUTING OPTIMAL LOCALLY CONSTRAINED STEPS,    
C             SIAM J. SCI. STATIST. COMPUTING, VOL. 2, NO. 2, PP.       
C             186-197.                                                  
C 3.  LAWSON, C.L., AND HANSON, R.J. (1974), SOLVING LEAST SQUARES      
C             PROBLEMS, PRENTICE-HALL, ENGLEWOOD CLIFFS, N.J.           
C 4.  MORE, J.J. (1978), THE LEVENBERG-MARQUARDT ALGORITHM, IMPLEMEN-   
C             TATION AND THEORY, PP.105-116 OF SPRINGER LECTURE NOTES   
C             IN MATHEMATICS NO. 630, EDITED BY G.A. WATSON, SPRINGER-  
C             VERLAG, BERLIN AND NEW YORK.                              
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY.                                            
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH           
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS         
C     MCS-7600324, DCR75-10143, 76-14311DSS, MCS76-11989, AND           
C     MCS-7906671.                                                      
C                                                                       
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER DSTSAV, I, IP1, I1, J1, K, KALIM, L, LK0, PHIPIN,         
     1        PP1O2, RES, RES0, RMAT, RMAT0, UK0                        
      DOUBLE PRECISION A, ADI, ALPHAK, B, DFACSQ, DST, DTOL, D1, D2,    
     1                 LK, OLDPHI, PHI, PHIMAX, PHIMIN, PSIFAC, RAD,    
     2                 SI, SJ, SQRTAK, T, TWOPSI, UK, WL                
C                                                                       
C     ***  CONSTANTS  ***                                               
      DOUBLE PRECISION DFAC, EIGHT, HALF, NEGONE, ONE, P001, THREE,     
     1                 TTOL, ZERO                                       
      DOUBLE PRECISION BIG                                              
C                                                                       
C  ***  INTRINSIC FUNCTIONS  ***                                        
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      DOUBLE PRECISION DD7TPR, DL7SVN, DR7MDC, DV2NRM                   
      EXTERNAL DD7TPR, DL7ITV, DL7IVM, DL7SVN, DR7MDC,DV7CPY, DV2NRM    
C                                                                       
C  ***  SUBSCRIPTS FOR V  ***                                           
C                                                                       
      INTEGER DGNORM, DSTNRM, DST0, EPSLON, GTSTEP, NREDUC, PHMNFC,     
     1        PHMXFC, PREDUC, RADIUS, RAD0, STPPAR                      
C/6                                                                     
C     DATA DGNORM/1/, DSTNRM/2/, DST0/3/, EPSLON/19/, GTSTEP/4/,        
C    1     NREDUC/6/, PHMNFC/20/, PHMXFC/21/, PREDUC/7/, RADIUS/8/,     
C    2     RAD0/9/, STPPAR/5/                                           
C/7                                                                     
      PARAMETER (DGNORM=1, DSTNRM=2, DST0=3, EPSLON=19, GTSTEP=4,       
     1           NREDUC=6, PHMNFC=20, PHMXFC=21, PREDUC=7, RADIUS=8,    
     2           RAD0=9, STPPAR=5)                                      
C/                                                                      
C                                                                       
C/6                                                                     
C     DATA DFAC/256.D+0/, EIGHT/8.D+0/, HALF/0.5D+0/, NEGONE/-1.D+0/,   
C    1     ONE/1.D+0/, P001/1.D-3/, THREE/3.D+0/, TTOL/2.5D+0/,         
C    2     ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (DFAC=256.D+0, EIGHT=8.D+0, HALF=0.5D+0, NEGONE=-1.D+0, 
     1     ONE=1.D+0, P001=1.D-3, THREE=3.D+0, TTOL=2.5D+0,             
     2     ZERO=0.D+0)                                                  
      SAVE BIG                                                          
C/                                                                      
      DATA BIG/0.D+0/                                                   
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
C     ***  FOR USE IN RECOMPUTING STEP, THE FINAL VALUES OF LK AND UK,  
C     ***  THE INVERSE DERIVATIVE OF MORE*S PHI AT 0 (FOR NONSING. J)   
C     ***  AND THE VALUE RETURNED AS V(DSTNRM) ARE STORED AT W(LK0),    
C     ***  W(UK0), W(PHIPIN), AND W(DSTSAV) RESPECTIVELY.               
      LK0 = P + 1                                                       
      PHIPIN = LK0 + 1                                                  
      UK0 = PHIPIN + 1                                                  
      DSTSAV = UK0 + 1                                                  
      RMAT0 = DSTSAV                                                    
C     ***  A COPY OF THE R-MATRIX FROM THE QR DECOMPOSITION OF J IS     
C     ***  STORED IN W STARTING AT W(RMAT), AND A COPY OF THE RESIDUAL  
C     ***  VECTOR IS STORED IN W STARTING AT W(RES).  THE LOOPS BELOW   
C     ***  THAT UPDATE THE QR DECOMP. FOR A NONZERO MARQUARDT PARAMETER 
C     ***  WORK ON THESE COPIES.                                        
      RMAT = RMAT0 + 1                                                  
      PP1O2 = P * (P + 1) / 2                                           
      RES0 = PP1O2 + RMAT0                                              
      RES = RES0 + 1                                                    
      RAD = V(RADIUS)                                                   
      IF (RAD .GT. ZERO)                                                
     1   PSIFAC = V(EPSLON)/((EIGHT*(V(PHMNFC) + ONE) + THREE) * RAD**2)
      IF (BIG .LE. ZERO) BIG = DR7MDC(6)                                
      PHIMAX = V(PHMXFC) * RAD                                          
      PHIMIN = V(PHMNFC) * RAD                                          
C     ***  DTOL, DFAC, AND DFACSQ ARE USED IN RESCALING THE FAST GIVENS 
C     ***  REPRESENTATION OF THE UPDATED QR DECOMPOSITION.              
      DTOL = ONE/DFAC                                                   
      DFACSQ = DFAC*DFAC                                                
C     ***  OLDPHI IS USED TO DETECT LIMITS OF NUMERICAL ACCURACY.  IF   
C     ***  WE RECOMPUTE STEP AND IT DOES NOT CHANGE, THEN WE ACCEPT IT. 
      OLDPHI = ZERO                                                     
      LK = ZERO                                                         
      UK = ZERO                                                         
      KALIM = KA + 12                                                   
C                                                                       
C  ***  START OR RESTART, DEPENDING ON KA  ***                          
C                                                                       
      IF (KA) 10, 20, 370                                               
C                                                                       
C  ***  FRESH START -- COMPUTE V(NREDUC)  ***                           
C                                                                       
 10   KA = 0                                                            
      KALIM = 12                                                        
      K = P                                                             
      IF (IERR .NE. 0) K = IABS(IERR) - 1                               
      V(NREDUC) = HALF*DD7TPR(K, QTR, QTR)                              
C                                                                       
C  ***  SET UP TO TRY INITIAL GAUSS-NEWTON STEP  ***                    
C                                                                       
 20   V(DST0) = NEGONE                                                  
      IF (IERR .NE. 0) GO TO 90                                         
      T = DL7SVN(P, R, STEP, W(RES))                                    
      IF (T .GE. ONE) GO TO 30                                          
         IF (DV2NRM(P, QTR) .GE. BIG*T) GO TO 90                        
C                                                                       
C  ***  COMPUTE GAUSS-NEWTON STEP  ***                                  
C                                                                       
C     ***  NOTE -- THE R-MATRIX IS STORED COMPACTLY BY COLUMNS IN       
C     ***  R(1), R(2), R(3), ...  IT IS THE TRANSPOSE OF A              
C     ***  LOWER TRIANGULAR MATRIX STORED COMPACTLY BY ROWS, AND WE     
C     ***  TREAT IT AS SUCH WHEN USING DL7ITV AND DL7IVM.               
 30   CALL DL7ITV(P, W, R, QTR)                                         
C     ***  TEMPORARILY STORE PERMUTED -D*STEP IN STEP.                  
      DO 60 I = 1, P                                                    
         J1 = IPIVOT(I)                                                 
         STEP(I) = D(J1)*W(I)                                           
 60      CONTINUE                                                       
      DST = DV2NRM(P, STEP)                                             
      V(DST0) = DST                                                     
      PHI = DST - RAD                                                   
      IF (PHI .LE. PHIMAX) GO TO 410                                    
C     ***  IF THIS IS A RESTART, GO TO 110  ***                         
      IF (KA .GT. 0) GO TO 110                                          
C                                                                       
C  ***  GAUSS-NEWTON STEP WAS UNACCEPTABLE.  COMPUTE L0  ***            
C                                                                       
      DO 70 I = 1, P                                                    
         J1 = IPIVOT(I)                                                 
         STEP(I) = D(J1)*(STEP(I)/DST)                                  
 70      CONTINUE                                                       
      CALL DL7IVM(P, STEP, R, STEP)                                     
      T = ONE / DV2NRM(P, STEP)                                         
      W(PHIPIN) = (T/RAD)*T                                             
      LK = PHI*W(PHIPIN)                                                
C                                                                       
C  ***  COMPUTE U0  ***                                                 
C                                                                       
 90   DO 100 I = 1, P                                                   
 100     W(I) = G(I)/D(I)                                               
      V(DGNORM) = DV2NRM(P, W)                                          
      UK = V(DGNORM)/RAD                                                
      IF (UK .LE. ZERO) GO TO 390                                       
C                                                                       
C     ***  ALPHAK WILL BE USED AS THE CURRENT MARQUARDT PARAMETER.  WE  
C     ***  USE MORE*S SCHEME FOR INITIALIZING IT.                       
C                                                                       
      ALPHAK = DABS(V(STPPAR)) * V(RAD0)/RAD                            
      ALPHAK = DMIN1(UK, DMAX1(ALPHAK, LK))                             
C                                                                       
C                                                                       
C  ***  TOP OF LOOP -- INCREMENT KA, COPY R TO RMAT, QTR TO RES  ***    
C                                                                       
 110  KA = KA + 1                                                       
      CALL DV7CPY(PP1O2, W(RMAT), R)                                    
      CALL DV7CPY(P, W(RES), QTR)                                       
C                                                                       
C  ***  SAFEGUARD ALPHAK AND INITIALIZE FAST GIVENS SCALE VECTOR.  ***  
C                                                                       
      IF (ALPHAK .LE. ZERO .OR. ALPHAK .LT. LK .OR. ALPHAK .GE. UK)     
     1             ALPHAK = UK * DMAX1(P001, DSQRT(LK/UK))              
      IF (ALPHAK .LE. ZERO) ALPHAK = HALF * UK                          
      SQRTAK = DSQRT(ALPHAK)                                            
      DO 120 I = 1, P                                                   
 120     W(I) = ONE                                                     
C                                                                       
C  ***  ADD ALPHAK*D AND UPDATE QR DECOMP. USING FAST GIVENS TRANS.  ***
C                                                                       
      DO 270 I = 1, P                                                   
C        ***  GENERATE, APPLY 1ST GIVENS TRANS. FOR ROW I OF ALPHAK*D.  
C        ***  (USE STEP TO STORE TEMPORARY ROW)  ***                    
         L = I*(I+1)/2 + RMAT0                                          
         WL = W(L)                                                      
         D2 = ONE                                                       
         D1 = W(I)                                                      
         J1 = IPIVOT(I)                                                 
         ADI = SQRTAK*D(J1)                                             
         IF (ADI .GE. DABS(WL)) GO TO 150                               
 130     A = ADI/WL                                                     
         B = D2*A/D1                                                    
         T = A*B + ONE                                                  
         IF (T .GT. TTOL) GO TO 150                                     
         W(I) = D1/T                                                    
         D2 = D2/T                                                      
         W(L) = T*WL                                                    
         A = -A                                                         
         DO 140 J1 = I, P                                               
              L = L + J1                                                
              STEP(J1) = A*W(L)                                         
 140          CONTINUE                                                  
         GO TO 170                                                      
C                                                                       
 150     B = WL/ADI                                                     
         A = D1*B/D2                                                    
         T = A*B + ONE                                                  
         IF (T .GT. TTOL) GO TO 130                                     
         W(I) = D2/T                                                    
         D2 = D1/T                                                      
         W(L) = T*ADI                                                   
         DO 160 J1 = I, P                                               
              L = L + J1                                                
              WL = W(L)                                                 
              STEP(J1) = -WL                                            
              W(L) = A*WL                                               
 160          CONTINUE                                                  
C                                                                       
 170     IF (I .EQ. P) GO TO 280                                        
C                                                                       
C        ***  NOW USE GIVENS TRANS. TO ZERO ELEMENTS OF TEMP. ROW  ***  
C                                                                       
         IP1 = I + 1                                                    
         DO 260 I1 = IP1, P                                             
              L = I1*(I1+1)/2 + RMAT0                                   
              WL = W(L)                                                 
              SI = STEP(I1-1)                                           
              D1 = W(I1)                                                
C                                                                       
C             ***  RESCALE ROW I1 IF NECESSARY  ***                     
C                                                                       
              IF (D1 .GE. DTOL) GO TO 190                               
                   D1 = D1*DFACSQ                                       
                   WL = WL/DFAC                                         
                   K = L                                                
                   DO 180 J1 = I1, P                                    
                        K = K + J1                                      
                        W(K) = W(K)/DFAC                                
 180                    CONTINUE                                        
C                                                                       
C             ***  USE GIVENS TRANS. TO ZERO NEXT ELEMENT OF TEMP. ROW  
C                                                                       
 190          IF (DABS(SI) .GT. DABS(WL)) GO TO 220                     
              IF (SI .EQ. ZERO) GO TO 260                               
 200          A = SI/WL                                                 
              B = D2*A/D1                                               
              T = A*B + ONE                                             
              IF (T .GT. TTOL) GO TO 220                                
              W(L) = T*WL                                               
              W(I1) = D1/T                                              
              D2 = D2/T                                                 
              DO 210 J1 = I1, P                                         
                   L = L + J1                                           
                   WL = W(L)                                            
                   SJ = STEP(J1)                                        
                   W(L) = WL + B*SJ                                     
                   STEP(J1) = SJ - A*WL                                 
 210               CONTINUE                                             
              GO TO 240                                                 
C                                                                       
 220          B = WL/SI                                                 
              A = D1*B/D2                                               
              T = A*B + ONE                                             
              IF (T .GT. TTOL) GO TO 200                                
              W(I1) = D2/T                                              
              D2 = D1/T                                                 
              W(L) = T*SI                                               
              DO 230 J1 = I1, P                                         
                   L = L + J1                                           
                   WL = W(L)                                            
                   SJ = STEP(J1)                                        
                   W(L) = A*WL + SJ                                     
                   STEP(J1) = B*SJ - WL                                 
 230               CONTINUE                                             
C                                                                       
C             ***  RESCALE TEMP. ROW IF NECESSARY  ***                  
C                                                                       
 240          IF (D2 .GE. DTOL) GO TO 260                               
                   D2 = D2*DFACSQ                                       
                   DO 250 K = I1, P                                     
 250                    STEP(K) = STEP(K)/DFAC                          
 260          CONTINUE                                                  
 270     CONTINUE                                                       
C                                                                       
C  ***  COMPUTE STEP  ***                                               
C                                                                       
 280  CALL DL7ITV(P, W(RES), W(RMAT), W(RES))                           
C     ***  RECOVER STEP AND STORE PERMUTED -D*STEP AT W(RES)  ***       
      DO 290 I = 1, P                                                   
         J1 = IPIVOT(I)                                                 
         K = RES0 + I                                                   
         T = W(K)                                                       
         STEP(J1) = -T                                                  
         W(K) = T*D(J1)                                                 
 290     CONTINUE                                                       
      DST = DV2NRM(P, W(RES))                                           
      PHI = DST - RAD                                                   
      IF (PHI .LE. PHIMAX .AND. PHI .GE. PHIMIN) GO TO 430              
      IF (OLDPHI .EQ. PHI) GO TO 430                                    
      OLDPHI = PHI                                                      
C                                                                       
C  ***  CHECK FOR (AND HANDLE) SPECIAL CASE  ***                        
C                                                                       
      IF (PHI .GT. ZERO) GO TO 310                                      
         IF (KA .GE. KALIM) GO TO 430                                   
              TWOPSI = ALPHAK*DST*DST - DD7TPR(P, STEP, G)              
              IF (ALPHAK .GE. TWOPSI*PSIFAC) GO TO 310                  
                   V(STPPAR) = -ALPHAK                                  
                   GO TO 440                                            
C                                                                       
C  ***  UNACCEPTABLE STEP -- UPDATE LK, UK, ALPHAK, AND TRY AGAIN  ***  
C                                                                       
 300  IF (PHI .LT. ZERO) UK = DMIN1(UK, ALPHAK)                         
      GO TO 320                                                         
 310  IF (PHI .LT. ZERO) UK = ALPHAK                                    
 320  DO 330 I = 1, P                                                   
         J1 = IPIVOT(I)                                                 
         K = RES0 + I                                                   
         STEP(I) = D(J1) * (W(K)/DST)                                   
 330     CONTINUE                                                       
      CALL DL7IVM(P, STEP, W(RMAT), STEP)                               
      DO 340 I = 1, P                                                   
 340     STEP(I) = STEP(I) / DSQRT(W(I))                                
      T = ONE / DV2NRM(P, STEP)                                         
      ALPHAK = ALPHAK + T*PHI*T/RAD                                     
      LK = DMAX1(LK, ALPHAK)                                            
      ALPHAK = LK                                                       
      GO TO 110                                                         
C                                                                       
C  ***  RESTART  ***                                                    
C                                                                       
 370  LK = W(LK0)                                                       
      UK = W(UK0)                                                       
      IF (V(DST0) .GT. ZERO .AND. V(DST0) - RAD .LE. PHIMAX) GO TO 20   
      ALPHAK = DABS(V(STPPAR))                                          
      DST = W(DSTSAV)                                                   
      PHI = DST - RAD                                                   
      T = V(DGNORM)/RAD                                                 
      IF (RAD .GT. V(RAD0)) GO TO 380                                   
C                                                                       
C        ***  SMALLER RADIUS  ***                                       
         UK = T                                                         
         IF (ALPHAK .LE. ZERO) LK = ZERO                                
         IF (V(DST0) .GT. ZERO) LK = DMAX1(LK, (V(DST0)-RAD)*W(PHIPIN)) 
         GO TO 300                                                      
C                                                                       
C     ***  BIGGER RADIUS  ***                                           
 380  IF (ALPHAK .LE. ZERO .OR. UK .GT. T) UK = T                       
      LK = ZERO                                                         
      IF (V(DST0) .GT. ZERO) LK = DMAX1(LK, (V(DST0)-RAD)*W(PHIPIN))    
      GO TO 300                                                         
C                                                                       
C  ***  SPECIAL CASE -- RAD .LE. 0 OR (G = 0 AND J IS SINGULAR)  ***    
C                                                                       
 390  V(STPPAR) = ZERO                                                  
      DST = ZERO                                                        
      LK = ZERO                                                         
      UK = ZERO                                                         
      V(GTSTEP) = ZERO                                                  
      V(PREDUC) = ZERO                                                  
      DO 400 I = 1, P                                                   
 400     STEP(I) = ZERO                                                 
      GO TO 450                                                         
C                                                                       
C  ***  ACCEPTABLE GAUSS-NEWTON STEP -- RECOVER STEP FROM W  ***        
C                                                                       
 410  ALPHAK = ZERO                                                     
      DO 420 I = 1, P                                                   
         J1 = IPIVOT(I)                                                 
         STEP(J1) = -W(I)                                               
 420     CONTINUE                                                       
C                                                                       
C  ***  SAVE VALUES FOR USE IN A POSSIBLE RESTART  ***                  
C                                                                       
 430  V(STPPAR) = ALPHAK                                                
 440  V(GTSTEP) = DMIN1(DD7TPR(P,STEP,G), ZERO)                         
      V(PREDUC) = HALF * (ALPHAK*DST*DST - V(GTSTEP))                   
 450  V(DSTNRM) = DST                                                   
      W(DSTSAV) = DST                                                   
      W(LK0) = LK                                                       
      W(UK0) = UK                                                       
      V(RAD0) = RAD                                                     
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF DL7MST FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DL7NVR(N, LIN, L)                                      
C                                                                       
C  ***  COMPUTE  LIN = L**-1,  BOTH  N X N  LOWER TRIANG. STORED   ***  
C  ***  COMPACTLY BY ROWS.  LIN AND L MAY SHARE THE SAME STORAGE.  ***  
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
      INTEGER N                                                         
      DOUBLE PRECISION L(1), LIN(1)                                     
C     DIMENSION L(N*(N+1)/2), LIN(N*(N+1)/2)                            
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, II, IM1, JJ, J0, J1, K, K0, NP1                        
      DOUBLE PRECISION ONE, T, ZERO                                     
C/6                                                                     
C     DATA ONE/1.D+0/, ZERO/0.D+0/                                      
C/7                                                                     
      PARAMETER (ONE=1.D+0, ZERO=0.D+0)                                 
C/                                                                      
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
      NP1 = N + 1                                                       
      J0 = N*(NP1)/2                                                    
      DO 30 II = 1, N                                                   
         I = NP1 - II                                                   
         LIN(J0) = ONE/L(J0)                                            
         IF (I .LE. 1) GO TO 999                                        
         J1 = J0                                                        
         IM1 = I - 1                                                    
         DO 20 JJ = 1, IM1                                              
              T = ZERO                                                  
              J0 = J1                                                   
              K0 = J1 - JJ                                              
              DO 10 K = 1, JJ                                           
                   T = T - L(K0)*LIN(J0)                                
                   J0 = J0 - 1                                          
                   K0 = K0 + K - I                                      
 10                CONTINUE                                             
              LIN(J0) = T/L(K0)                                         
 20           CONTINUE                                                  
         J0 = J0 - 1                                                    
 30      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DL7NVR FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DL7SQR(N, A, L)                                        
C                                                                       
C  ***  COMPUTE  A = LOWER TRIANGLE OF  L*(L**T),  WITH BOTH            
C  ***  L  AND  A  STORED COMPACTLY BY ROWS.  (BOTH MAY OCCUPY THE      
C  ***  SAME STORAGE.                                                   
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
      INTEGER N                                                         
      DOUBLE PRECISION A(1), L(1)                                       
C     DIMENSION A(N*(N+1)/2), L(N*(N+1)/2)                              
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, II, IJ, IK, IP1, I0, J, JJ, JK, J0, K, NP1             
      DOUBLE PRECISION T                                                
C                                                                       
      NP1 = N + 1                                                       
      I0 = N*(N+1)/2                                                    
      DO 30 II = 1, N                                                   
         I = NP1 - II                                                   
         IP1 = I + 1                                                    
         I0 = I0 - I                                                    
         J0 = I*(I+1)/2                                                 
         DO 20 JJ = 1, I                                                
              J = IP1 - JJ                                              
              J0 = J0 - J                                               
              T = 0.0D0                                                 
              DO 10 K = 1, J                                            
                   IK = I0 + K                                          
                   JK = J0 + K                                          
                   T = T + L(IK)*L(JK)                                  
 10                CONTINUE                                             
              IJ = I0 + J                                               
              A(IJ) = T                                                 
 20           CONTINUE                                                  
 30      CONTINUE                                                       
 999  RETURN                                                            
      END                                                               
      SUBROUTINE DL7SRT(N1, N, L, A, IRC)                               
C                                                                       
C  ***  COMPUTE ROWS N1 THROUGH N OF THE CHOLESKY FACTOR  L  OF         
C  ***  A = L*(L**T),  WHERE  L  AND THE LOWER TRIANGLE OF  A  ARE BOTH 
C  ***  STORED COMPACTLY BY ROWS (AND MAY OCCUPY THE SAME STORAGE).     
C  ***  IRC = 0 MEANS ALL WENT WELL.  IRC = J MEANS THE LEADING         
C  ***  PRINCIPAL  J X J  SUBMATRIX OF  A  IS NOT POSITIVE DEFINITE --  
C  ***  AND  L(J*(J+1)/2)  CONTAINS THE (NONPOS.) REDUCED J-TH DIAGONAL.
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
      INTEGER N1, N, IRC                                                
      DOUBLE PRECISION L(1), A(1)                                       
C     DIMENSION L(N*(N+1)/2), A(N*(N+1)/2)                              
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, IJ, IK, IM1, I0, J, JK, JM1, J0, K                     
      DOUBLE PRECISION T, TD, ZERO                                      
C                                                                       
C  ***  INTRINSIC FUNCTIONS  ***                                        
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
C/6                                                                     
C     DATA ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.D+0)                                            
C/                                                                      
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
      I0 = N1 * (N1 - 1) / 2                                            
      DO 50 I = N1, N                                                   
         TD = ZERO                                                      
         IF (I .EQ. 1) GO TO 40                                         
         J0 = 0                                                         
         IM1 = I - 1                                                    
         DO 30 J = 1, IM1                                               
              T = ZERO                                                  
              IF (J .EQ. 1) GO TO 20                                    
              JM1 = J - 1                                               
              DO 10 K = 1, JM1                                          
                   IK = I0 + K                                          
                   JK = J0 + K                                          
                   T = T + L(IK)*L(JK)                                  
 10                CONTINUE                                             
 20           IJ = I0 + J                                               
              J0 = J0 + J                                               
              T = (A(IJ) - T) / L(J0)                                   
              L(IJ) = T                                                 
              TD = TD + T*T                                             
 30           CONTINUE                                                  
 40      I0 = I0 + I                                                    
         T = A(I0) - TD                                                 
         IF (T .LE. ZERO) GO TO 60                                      
         L(I0) = DSQRT(T)                                               
 50      CONTINUE                                                       
C                                                                       
      IRC = 0                                                           
      GO TO 999                                                         
C                                                                       
 60   L(I0) = T                                                         
      IRC = I                                                           
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF DL7SRT  ***                                        
      END                                                               
      DOUBLE PRECISION FUNCTION DL7SVN(P, L, X, Y)                      
C                                                                       
C  ***  ESTIMATE SMALLEST SING. VALUE OF PACKED LOWER TRIANG. MATRIX L  
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER P                                                         
      DOUBLE PRECISION L(1), X(P), Y(P)                                 
C     DIMENSION L(P*(P+1)/2)                                            
C                                                                       
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                                                                       
C  ***  PURPOSE  ***                                                    
C                                                                       
C     THIS FUNCTION RETURNS A GOOD OVER-ESTIMATE OF THE SMALLEST        
C     SINGULAR VALUE OF THE PACKED LOWER TRIANGULAR MATRIX L.           
C                                                                       
C  ***  PARAMETER DESCRIPTION  ***                                      
C                                                                       
C  P (IN)  = THE ORDER OF L.  L IS A  P X P  LOWER TRIANGULAR MATRIX.   
C  L (IN)  = ARRAY HOLDING THE ELEMENTS OF  L  IN ROW ORDER, I.E.       
C             L(1,1), L(2,1), L(2,2), L(3,1), L(3,2), L(3,3), ETC.      
C  X (OUT) IF DL7SVN RETURNS A POSITIVE VALUE, THEN X IS A NORMALIZED   
C             APPROXIMATE LEFT SINGULAR VECTOR CORRESPONDING TO THE     
C             SMALLEST SINGULAR VALUE.  THIS APPROXIMATION MAY BE VERY  
C             CRUDE.  IF DL7SVN RETURNS ZERO, THEN SOME COMPONENTS OF X 
C             ARE ZERO AND THE REST RETAIN THEIR INPUT VALUES.          
C  Y (OUT) IF DL7SVN RETURNS A POSITIVE VALUE, THEN Y = (L**-1)*X IS AN 
C             UNNORMALIZED APPROXIMATE RIGHT SINGULAR VECTOR CORRESPOND-
C             ING TO THE SMALLEST SINGULAR VALUE.  THIS APPROXIMATION   
C             MAY BE CRUDE.  IF DL7SVN RETURNS ZERO, THEN Y RETAINS ITS 
C             INPUT VALUE.  THE CALLER MAY PASS THE SAME VECTOR FOR X   
C             AND Y (NONSTANDARD FORTRAN USAGE), IN WHICH CASE Y OVER-  
C             WRITES X (FOR NONZERO DL7SVN RETURNS).                    
C                                                                       
C  ***  ALGORITHM NOTES  ***                                            
C                                                                       
C     THE ALGORITHM IS BASED ON (1), WITH THE ADDITIONAL PROVISION THAT 
C     DL7SVN = 0 IS RETURNED IF THE SMALLEST DIAGONAL ELEMENT OF L      
C     (IN MAGNITUDE) IS NOT MORE THAN THE UNIT ROUNDOFF TIMES THE       
C     LARGEST.  THE ALGORITHM USES A RANDOM NUMBER GENERATOR PROPOSED   
C     IN (4), WHICH PASSES THE SPECTRAL TEST WITH FLYING COLORS -- SEE  
C     (2) AND (3).                                                      
C                                                                       
C  ***  SUBROUTINES AND FUNCTIONS CALLED  ***                           
C                                                                       
C        DV2NRM - FUNCTION, RETURNS THE 2-NORM OF A VECTOR.             
C                                                                       
C  ***  REFERENCES  ***                                                 
C                                                                       
C     (1) CLINE, A., MOLER, C., STEWART, G., AND WILKINSON, J.H.(1977), 
C         AN ESTIMATE FOR THE CONDITION NUMBER OF A MATRIX, REPORT      
C         TM-310, APPLIED MATH. DIV., ARGONNE NATIONAL LABORATORY.      
C                                                                       
C     (2) HOAGLIN, D.C. (1976), THEORETICAL PROPERTIES OF CONGRUENTIAL  
C         RANDOM-NUMBER GENERATORS --  AN EMPIRICAL VIEW,               
C         MEMORANDUM NS-340, DEPT. OF STATISTICS, HARVARD UNIV.         
C                                                                       
C     (3) KNUTH, D.E. (1969), THE ART OF COMPUTER PROGRAMMING, VOL. 2   
C         (SEMINUMERICAL ALGORITHMS), ADDISON-WESLEY, READING, MASS.    
C                                                                       
C     (4) SMITH, C.S. (1971), MULTIPLICATIVE PSEUDO-RANDOM NUMBER       
C         GENERATORS WITH PRIME MODULUS, J. ASSOC. COMPUT. MACH. 18,    
C         PP. 586-593.                                                  
C                                                                       
C  ***  HISTORY  ***                                                    
C                                                                       
C     DESIGNED AND CODED BY DAVID M. GAY (WINTER 1977/SUMMER 1978).     
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH           
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS         
C     MCS-7600324, DCR75-10143, 76-14311DSS, AND MCS76-11989.           
C                                                                       
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, II, IX, J, JI, JJ, JJJ, JM1, J0, PM1                   
      DOUBLE PRECISION B, SMINUS, SPLUS, T, XMINUS, XPLUS               
C                                                                       
C  ***  CONSTANTS  ***                                                  
C                                                                       
      DOUBLE PRECISION HALF, ONE, R9973, ZERO                           
C                                                                       
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      DOUBLE PRECISION DD7TPR, DV2NRM                                   
      EXTERNAL DD7TPR, DV2NRM,DV2AXY                                    
C                                                                       
C/6                                                                     
C     DATA HALF/0.5D+0/, ONE/1.D+0/, R9973/9973.D+0/, ZERO/0.D+0/       
C/7                                                                     
      PARAMETER (HALF=0.5D+0, ONE=1.D+0, R9973=9973.D+0, ZERO=0.D+0)    
C/                                                                      
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
      IX = 2                                                            
      PM1 = P - 1                                                       
C                                                                       
C  ***  FIRST CHECK WHETHER TO RETURN DL7SVN = 0 AND INITIALIZE X  ***  
C                                                                       
      II = 0                                                            
      J0 = P*PM1/2                                                      
      JJ = J0 + P                                                       
      IF (L(JJ) .EQ. ZERO) GO TO 110                                    
      IX = MOD(3432*IX, 9973)                                           
      B = HALF*(ONE + FLOAT(IX)/R9973)                                  
      XPLUS = B / L(JJ)                                                 
      X(P) = XPLUS                                                      
      IF (P .LE. 1) GO TO 60                                            
      DO 10 I = 1, PM1                                                  
         II = II + I                                                    
         IF (L(II) .EQ. ZERO) GO TO 110                                 
         JI = J0 + I                                                    
         X(I) = XPLUS * L(JI)                                           
 10      CONTINUE                                                       
C                                                                       
C  ***  SOLVE (L**T)*X = B, WHERE THE COMPONENTS OF B HAVE RANDOMLY     
C  ***  CHOSEN MAGNITUDES IN (.5,1) WITH SIGNS CHOSEN TO MAKE X LARGE.  
C                                                                       
C     DO J = P-1 TO 1 BY -1...                                          
      DO 50 JJJ = 1, PM1                                                
         J = P - JJJ                                                    
C       ***  DETERMINE X(J) IN THIS ITERATION. NOTE FOR I = 1,2,...,J   
C       ***  THAT X(I) HOLDS THE CURRENT PARTIAL SUM FOR ROW I.         
         IX = MOD(3432*IX, 9973)                                        
         B = HALF*(ONE + FLOAT(IX)/R9973)                               
         XPLUS = (B - X(J))                                             
         XMINUS = (-B - X(J))                                           
         SPLUS = DABS(XPLUS)                                            
         SMINUS = DABS(XMINUS)                                          
         JM1 = J - 1                                                    
         J0 = J*JM1/2                                                   
         JJ = J0 + J                                                    
         XPLUS = XPLUS/L(JJ)                                            
         XMINUS = XMINUS/L(JJ)                                          
         IF (JM1 .EQ. 0) GO TO 30                                       
         DO 20 I = 1, JM1                                               
              JI = J0 + I                                               
              SPLUS = SPLUS + DABS(X(I) + L(JI)*XPLUS)                  
              SMINUS = SMINUS + DABS(X(I) + L(JI)*XMINUS)               
 20           CONTINUE                                                  
 30      IF (SMINUS .GT. SPLUS) XPLUS = XMINUS                          
         X(J) = XPLUS                                                   
C       ***  UPDATE PARTIAL SUMS  ***                                   
         IF (JM1 .GT. 0) CALL DV2AXY(JM1, X, XPLUS, L(J0+1), X)         
 50      CONTINUE                                                       
C                                                                       
C  ***  NORMALIZE X  ***                                                
C                                                                       
 60   T = ONE/DV2NRM(P, X)                                              
      DO 70 I = 1, P                                                    
 70      X(I) = T*X(I)                                                  
C                                                                       
C  ***  SOLVE L*Y = X AND RETURN DL7SVN = 1/TWONORM(Y)  ***             
C                                                                       
      DO 100 J = 1, P                                                   
         JM1 = J - 1                                                    
         J0 = J*JM1/2                                                   
         JJ = J0 + J                                                    
         T = ZERO                                                       
         IF (JM1 .GT. 0) T = DD7TPR(JM1, L(J0+1), Y)                    
         Y(J) = (X(J) - T) / L(JJ)                                      
 100     CONTINUE                                                       
C                                                                       
      DL7SVN = ONE/DV2NRM(P, Y)                                         
      GO TO 999                                                         
C                                                                       
 110  DL7SVN = ZERO                                                     
 999  RETURN                                                            
C  ***  LAST CARD OF DL7SVN FOLLOWS  ***                                
      END                                                               
      DOUBLE PRECISION FUNCTION DL7SVX(P, L, X, Y)                      
C                                                                       
C  ***  ESTIMATE LARGEST SING. VALUE OF PACKED LOWER TRIANG. MATRIX L   
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER P                                                         
      DOUBLE PRECISION L(1), X(P), Y(P)                                 
C     DIMENSION L(P*(P+1)/2)                                            
C                                                                       
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                                                                       
C  ***  PURPOSE  ***                                                    
C                                                                       
C     THIS FUNCTION RETURNS A GOOD UNDER-ESTIMATE OF THE LARGEST        
C     SINGULAR VALUE OF THE PACKED LOWER TRIANGULAR MATRIX L.           
C                                                                       
C  ***  PARAMETER DESCRIPTION  ***                                      
C                                                                       
C  P (IN)  = THE ORDER OF L.  L IS A  P X P  LOWER TRIANGULAR MATRIX.   
C  L (IN)  = ARRAY HOLDING THE ELEMENTS OF  L  IN ROW ORDER, I.E.       
C             L(1,1), L(2,1), L(2,2), L(3,1), L(3,2), L(3,3), ETC.      
C  X (OUT) IF DL7SVX RETURNS A POSITIVE VALUE, THEN X = (L**T)*Y IS AN  
C             (UNNORMALIZED) APPROXIMATE RIGHT SINGULAR VECTOR          
C             CORRESPONDING TO THE LARGEST SINGULAR VALUE.  THIS        
C             APPROXIMATION MAY BE CRUDE.                               
C  Y (OUT) IF DL7SVX RETURNS A POSITIVE VALUE, THEN Y = L*X IS A        
C             NORMALIZED APPROXIMATE LEFT SINGULAR VECTOR CORRESPOND-   
C             ING TO THE LARGEST SINGULAR VALUE.  THIS APPROXIMATION    
C             MAY BE VERY CRUDE.  THE CALLER MAY PASS THE SAME VECTOR   
C             FOR X AND Y (NONSTANDARD FORTRAN USAGE), IN WHICH CASE X  
C             OVER-WRITES Y.                                            
C                                                                       
C  ***  ALGORITHM NOTES  ***                                            
C                                                                       
C     THE ALGORITHM IS BASED ON ANALOGY WITH (1).  IT USES A            
C     RANDOM NUMBER GENERATOR PROPOSED IN (4), WHICH PASSES THE         
C     SPECTRAL TEST WITH FLYING COLORS -- SEE (2) AND (3).              
C                                                                       
C  ***  SUBROUTINES AND FUNCTIONS CALLED  ***                           
C                                                                       
C        DV2NRM - FUNCTION, RETURNS THE 2-NORM OF A VECTOR.             
C                                                                       
C  ***  REFERENCES  ***                                                 
C                                                                       
C     (1) CLINE, A., MOLER, C., STEWART, G., AND WILKINSON, J.H.(1977), 
C         AN ESTIMATE FOR THE CONDITION NUMBER OF A MATRIX, REPORT      
C         TM-310, APPLIED MATH. DIV., ARGONNE NATIONAL LABORATORY.      
C                                                                       
C     (2) HOAGLIN, D.C. (1976), THEORETICAL PROPERTIES OF CONGRUENTIAL  
C         RANDOM-NUMBER GENERATORS --  AN EMPIRICAL VIEW,               
C         MEMORANDUM NS-340, DEPT. OF STATISTICS, HARVARD UNIV.         
C                                                                       
C     (3) KNUTH, D.E. (1969), THE ART OF COMPUTER PROGRAMMING, VOL. 2   
C         (SEMINUMERICAL ALGORITHMS), ADDISON-WESLEY, READING, MASS.    
C                                                                       
C     (4) SMITH, C.S. (1971), MULTIPLICATIVE PSEUDO-RANDOM NUMBER       
C         GENERATORS WITH PRIME MODULUS, J. ASSOC. COMPUT. MACH. 18,    
C         PP. 586-593.                                                  
C                                                                       
C  ***  HISTORY  ***                                                    
C                                                                       
C     DESIGNED AND CODED BY DAVID M. GAY (WINTER 1977/SUMMER 1978).     
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH           
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS         
C     MCS-7600324, DCR75-10143, 76-14311DSS, AND MCS76-11989.           
C                                                                       
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, IX, J, JI, JJ, JJJ, JM1, J0, PM1, PPLUS1               
      DOUBLE PRECISION B, BLJI, SMINUS, SPLUS, T, YI                    
C                                                                       
C  ***  CONSTANTS  ***                                                  
C                                                                       
      DOUBLE PRECISION HALF, ONE, R9973, ZERO                           
C                                                                       
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      DOUBLE PRECISION DD7TPR, DV2NRM                                   
      EXTERNAL DD7TPR, DV2NRM,DV2AXY                                    
C                                                                       
C/6                                                                     
C     DATA HALF/0.5D+0/, ONE/1.D+0/, R9973/9973.D+0/, ZERO/0.D+0/       
C/7                                                                     
      PARAMETER (HALF=0.5D+0, ONE=1.D+0, R9973=9973.D+0, ZERO=0.D+0)    
C/                                                                      
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
      IX = 2                                                            
      PPLUS1 = P + 1                                                    
      PM1 = P - 1                                                       
C                                                                       
C  ***  FIRST INITIALIZE X TO PARTIAL SUMS  ***                         
C                                                                       
      J0 = P*PM1/2                                                      
      JJ = J0 + P                                                       
      IX = MOD(3432*IX, 9973)                                           
      B = HALF*(ONE + FLOAT(IX)/R9973)                                  
      X(P) = B * L(JJ)                                                  
      IF (P .LE. 1) GO TO 40                                            
      DO 10 I = 1, PM1                                                  
         JI = J0 + I                                                    
         X(I) = B * L(JI)                                               
 10      CONTINUE                                                       
C                                                                       
C  ***  COMPUTE X = (L**T)*B, WHERE THE COMPONENTS OF B HAVE RANDOMLY   
C  ***  CHOSEN MAGNITUDES IN (.5,1) WITH SIGNS CHOSEN TO MAKE X LARGE.  
C                                                                       
C     DO J = P-1 TO 1 BY -1...                                          
      DO 30 JJJ = 1, PM1                                                
         J = P - JJJ                                                    
C       ***  DETERMINE X(J) IN THIS ITERATION. NOTE FOR I = 1,2,...,J   
C       ***  THAT X(I) HOLDS THE CURRENT PARTIAL SUM FOR ROW I.         
         IX = MOD(3432*IX, 9973)                                        
         B = HALF*(ONE + FLOAT(IX)/R9973)                               
         JM1 = J - 1                                                    
         J0 = J*JM1/2                                                   
         SPLUS = ZERO                                                   
         SMINUS = ZERO                                                  
         DO 20 I = 1, J                                                 
              JI = J0 + I                                               
              BLJI = B * L(JI)                                          
              SPLUS = SPLUS + DABS(BLJI + X(I))                         
              SMINUS = SMINUS + DABS(BLJI - X(I))                       
 20           CONTINUE                                                  
         IF (SMINUS .GT. SPLUS) B = -B                                  
         X(J) = ZERO                                                    
C        ***  UPDATE PARTIAL SUMS  ***                                  
         CALL DV2AXY(J, X, B, L(J0+1), X)                               
 30      CONTINUE                                                       
C                                                                       
C  ***  NORMALIZE X  ***                                                
C                                                                       
 40   T = DV2NRM(P, X)                                                  
      IF (T .LE. ZERO) GO TO 80                                         
      T = ONE / T                                                       
      DO 50 I = 1, P                                                    
 50      X(I) = T*X(I)                                                  
C                                                                       
C  ***  COMPUTE L*X = Y AND RETURN SVMAX = TWONORM(Y)  ***              
C                                                                       
      DO 60 JJJ = 1, P                                                  
         J = PPLUS1 - JJJ                                               
         JI = J*(J-1)/2 + 1                                             
         Y(J) = DD7TPR(J, L(JI), X)                                     
 60      CONTINUE                                                       
C                                                                       
C  ***  NORMALIZE Y AND SET X = (L**T)*Y  ***                           
C                                                                       
      T = ONE / DV2NRM(P, Y)                                            
      JI = 1                                                            
      DO 70 I = 1, P                                                    
         YI = T * Y(I)                                                  
         X(I) = ZERO                                                    
         CALL DV2AXY(I, X, YI, L(JI), X)                                
         JI = JI + I                                                    
 70      CONTINUE                                                       
      DL7SVX = DV2NRM(P, X)                                             
      GO TO 999                                                         
C                                                                       
 80   DL7SVX = ZERO                                                     
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DL7SVX FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DL7TSQ(N, A, L)                                        
C                                                                       
C  ***  SET A TO LOWER TRIANGLE OF (L**T) * L  ***                      
C                                                                       
C  ***  L = N X N LOWER TRIANG. MATRIX STORED ROWWISE.  ***             
C  ***  A IS ALSO STORED ROWWISE AND MAY SHARE STORAGE WITH L.  ***     
C                                                                       
      INTEGER N                                                         
      DOUBLE PRECISION A(1), L(1)                                       
C     DIMENSION A(N*(N+1)/2), L(N*(N+1)/2)                              
C                                                                       
      INTEGER I, II, IIM1, I1, J, K, M                                  
      DOUBLE PRECISION LII, LJ                                          
C                                                                       
      II = 0                                                            
      DO 50 I = 1, N                                                    
         I1 = II + 1                                                    
         II = II + I                                                    
         M = 1                                                          
         IF (I .EQ. 1) GO TO 30                                         
         IIM1 = II - 1                                                  
         DO 20 J = I1, IIM1                                             
              LJ = L(J)                                                 
              DO 10 K = I1, J                                           
                   A(M) = A(M) + LJ*L(K)                                
                   M = M + 1                                            
 10                CONTINUE                                             
 20           CONTINUE                                                  
 30      LII = L(II)                                                    
         DO 40 J = I1, II                                               
 40           A(J) = LII * L(J)                                         
 50      CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DL7TSQ FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DL7UPD(BETA, GAMMA, L, LAMBDA, LPLUS, N, W, Z)         
C                                                                       
C  ***  COMPUTE LPLUS = SECANT UPDATE OF L  ***                         
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER N                                                         
      DOUBLE PRECISION BETA(N), GAMMA(N), L(1), LAMBDA(N), LPLUS(1),    
     1                 W(N), Z(N)                                       
C     DIMENSION L(N*(N+1)/2), LPLUS(N*(N+1)/2)                          
C                                                                       
C--------------------------  PARAMETER USAGE  --------------------------
C                                                                       
C   BETA = SCRATCH VECTOR.                                              
C  GAMMA = SCRATCH VECTOR.                                              
C      L (INPUT) LOWER TRIANGULAR MATRIX, STORED ROWWISE.               
C LAMBDA = SCRATCH VECTOR.                                              
C  LPLUS (OUTPUT) LOWER TRIANGULAR MATRIX, STORED ROWWISE, WHICH MAY    
C             OCCUPY THE SAME STORAGE AS  L.                            
C      N (INPUT) LENGTH OF VECTOR PARAMETERS AND ORDER OF MATRICES.     
C      W (INPUT, DESTROYED ON OUTPUT) RIGHT SINGULAR VECTOR OF RANK 1   
C             CORRECTION TO  L.                                         
C      Z (INPUT, DESTROYED ON OUTPUT) LEFT SINGULAR VECTOR OF RANK 1    
C             CORRECTION TO  L.                                         
C                                                                       
C-------------------------------  NOTES  -------------------------------
C                                                                       
C  ***  APPLICATION AND USAGE RESTRICTIONS  ***                         
C                                                                       
C        THIS ROUTINE UPDATES THE CHOLESKY FACTOR  L  OF A SYMMETRIC    
C     POSITIVE DEFINITE MATRIX TO WHICH A SECANT UPDATE IS BEING        
C     APPLIED -- IT COMPUTES A CHOLESKY FACTOR  LPLUS  OF               
C     L * (I + Z*W**T) * (I + W*Z**T) * L**T.  IT IS ASSUMED THAT  W    
C     AND  Z  HAVE BEEN CHOSEN SO THAT THE UPDATED MATRIX IS STRICTLY   
C     POSITIVE DEFINITE.                                                
C                                                                       
C  ***  ALGORITHM NOTES  ***                                            
C                                                                       
C        THIS CODE USES RECURRENCE 3 OF REF. 1 (WITH D(J) = 1 FOR ALL J)
C     TO COMPUTE  LPLUS  OF THE FORM  L * (I + Z*W**T) * Q,  WHERE  Q   
C     IS AN ORTHOGONAL MATRIX THAT MAKES THE RESULT LOWER TRIANGULAR.   
C        LPLUS MAY HAVE SOME NEGATIVE DIAGONAL ELEMENTS.                
C                                                                       
C  ***  REFERENCES  ***                                                 
C                                                                       
C 1.  GOLDFARB, D. (1976), FACTORIZED VARIABLE METRIC METHODS FOR UNCON-
C             STRAINED OPTIMIZATION, MATH. COMPUT. 30, PP. 796-811.     
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY (FALL 1979).                                
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH SUPPORTED 
C     BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS MCS-7600324 AND   
C     MCS-7906671.                                                      
C                                                                       
C------------------------  EXTERNAL QUANTITIES  ------------------------
C                                                                       
C  ***  INTRINSIC FUNCTIONS  ***                                        
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
C--------------------------  LOCAL VARIABLES  --------------------------
C                                                                       
      INTEGER I, IJ, J, JJ, JP1, K, NM1, NP1                            
      DOUBLE PRECISION A, B, BJ, ETA, GJ, LJ, LIJ, LJJ, NU, S, THETA,   
     1                 WJ, ZJ                                           
      DOUBLE PRECISION ONE, ZERO                                        
C                                                                       
C  ***  DATA INITIALIZATIONS  ***                                       
C                                                                       
C/6                                                                     
C     DATA ONE/1.D+0/, ZERO/0.D+0/                                      
C/7                                                                     
      PARAMETER (ONE=1.D+0, ZERO=0.D+0)                                 
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      NU = ONE                                                          
      ETA = ZERO                                                        
      IF (N .LE. 1) GO TO 30                                            
      NM1 = N - 1                                                       
C                                                                       
C  ***  TEMPORARILY STORE S(J) = SUM OVER K = J+1 TO N OF W(K)**2 IN    
C  ***  LAMBDA(J).                                                      
C                                                                       
      S = ZERO                                                          
      DO 10 I = 1, NM1                                                  
         J = N - I                                                      
         S = S + W(J+1)**2                                              
         LAMBDA(J) = S                                                  
 10      CONTINUE                                                       
C                                                                       
C  ***  COMPUTE LAMBDA, GAMMA, AND BETA BY GOLDFARB*S RECURRENCE 3.     
C                                                                       
      DO 20 J = 1, NM1                                                  
         WJ = W(J)                                                      
         A = NU*Z(J) - ETA*WJ                                           
         THETA = ONE + A*WJ                                             
         S = A*LAMBDA(J)                                                
         LJ = DSQRT(THETA**2 + A*S)                                     
         IF (THETA .GT. ZERO) LJ = -LJ                                  
         LAMBDA(J) = LJ                                                 
         B = THETA*WJ + S                                               
         GAMMA(J) = B * NU / LJ                                         
         BETA(J) = (A - B*ETA) / LJ                                     
         NU = -NU / LJ                                                  
         ETA = -(ETA + (A**2)/(THETA - LJ)) / LJ                        
 20      CONTINUE                                                       
 30   LAMBDA(N) = ONE + (NU*Z(N) - ETA*W(N))*W(N)                       
C                                                                       
C  ***  UPDATE L, GRADUALLY OVERWRITING  W  AND  Z  WITH  L*W  AND  L*Z.
C                                                                       
      NP1 = N + 1                                                       
      JJ = N * (N + 1) / 2                                              
      DO 60 K = 1, N                                                    
         J = NP1 - K                                                    
         LJ = LAMBDA(J)                                                 
         LJJ = L(JJ)                                                    
         LPLUS(JJ) = LJ * LJJ                                           
         WJ = W(J)                                                      
         W(J) = LJJ * WJ                                                
         ZJ = Z(J)                                                      
         Z(J) = LJJ * ZJ                                                
         IF (K .EQ. 1) GO TO 50                                         
         BJ = BETA(J)                                                   
         GJ = GAMMA(J)                                                  
         IJ = JJ + J                                                    
         JP1 = J + 1                                                    
         DO 40 I = JP1, N                                               
              LIJ = L(IJ)                                               
              LPLUS(IJ) = LJ*LIJ + BJ*W(I) + GJ*Z(I)                    
              W(I) = W(I) + LIJ*WJ                                      
              Z(I) = Z(I) + LIJ*ZJ                                      
              IJ = IJ + I                                               
 40           CONTINUE                                                  
 50      JJ = JJ - J                                                    
 60      CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DL7UPD FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DN2CVP(IV, LIV, LV, P, V)                              
C                                                                       
C  ***  PRINT COVARIANCE MATRIX FOR  DRN2G  ***                         
C                                                                       
      INTEGER LIV, LV, P                                                
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION V(LV)                                            
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER COV1, I, II, I1, J, PU                                    
      DOUBLE PRECISION T                                                
C                                                                       
C     ***  IV SUBSCRIPTS  ***                                           
C                                                                       
      INTEGER COVMAT, COVPRT, COVREQ, NEEDHD, NFCOV, NGCOV, PRUNIT,     
     1        RCOND, REGD, STATPR                                       
C                                                                       
C/6                                                                     
C     DATA COVMAT/26/, COVPRT/14/, COVREQ/15/, NEEDHD/36/, NFCOV/52/,   
C    1     NGCOV/53/, PRUNIT/21/, RCOND/53/, REGD/67/, STATPR/23/       
C/7                                                                     
      PARAMETER (COVMAT=26, COVPRT=14, COVREQ=15, NEEDHD=36, NFCOV=52,  
     1           NGCOV=53, PRUNIT=21, RCOND=53, REGD=67, STATPR=23)     
C/                                                                      
C  ***  BODY  ***                                                       
C                                                                       
      IF (IV(1) .GT. 8) GO TO 999                                       
      PU = IV(PRUNIT)                                                   
      IF (PU .EQ. 0) GO TO 999                                          
      IF (IV(STATPR) .EQ. 0) GO TO 30                                   
         IF (IV(NFCOV) .GT. 0) WRITE(PU,10) IV(NFCOV)                   
 10      FORMAT(/1X,I4,50H EXTRA FUNC. EVALS FOR COVARIANCE AND DIAGNOST
     1ICS.)                                                             
         IF (IV(NGCOV) .GT. 0) WRITE(PU,20) IV(NGCOV)                   
 20      FORMAT(1X,I4,50H EXTRA GRAD. EVALS FOR COVARIANCE AND DIAGNOSTI
     1CS.)                                                              
C                                                                       
 30   IF (IV(COVPRT) .LE. 0) GO TO 999                                  
      COV1 = IV(COVMAT)                                                 
      IF (IV(REGD) .LE. 0 .AND. COV1 .LE. 0) GO TO 70                   
      IV(NEEDHD) = 1                                                    
      T = V(RCOND)**2                                                   
      IF (IABS(IV(COVREQ)) .GT. 2) GO TO 50                             
C                                                                       
      WRITE(PU,40) T                                                    
 40   FORMAT(/47H RECIPROCAL CONDITION OF F.D. HESSIAN = AT MOST,D10.2) 
      GO TO 70                                                          
C                                                                       
 50   WRITE(PU,60) T                                                    
 60   FORMAT(/44H RECIPROCAL CONDITION OF (J**T)*J = AT LEAST,D10.2)    
C                                                                       
 70   IF (MOD(IV(COVPRT),2) .EQ. 0) GO TO 999                           
      IV(NEEDHD) = 1                                                    
      IF (COV1) 80,110,130                                              
 80   IF (-1 .EQ. COV1) WRITE(PU,90)                                    
 90   FORMAT(/43H ++++++ INDEFINITE COVARIANCE MATRIX ++++++)           
      IF (-2 .EQ. COV1) WRITE(PU,100)                                   
 100  FORMAT(/52H ++++++ OVERSIZE STEPS IN COMPUTING COVARIANCE +++++)  
      GO TO 999                                                         
C                                                                       
 110  WRITE(PU,120)                                                     
 120  FORMAT(/45H ++++++ COVARIANCE MATRIX NOT COMPUTED ++++++)         
      GO TO 999                                                         
C                                                                       
 130  I = IABS(IV(COVREQ))                                              
      IF (I .LE. 1) WRITE(PU,140)                                       
 140  FORMAT(/48H COVARIANCE = SCALE * H**-1 * (J**T * J) * H**-1/      
     1       23H WHERE H = F.D. HESSIAN/)                               
      IF (I .EQ. 2) WRITE(PU,150)                                       
 150  FORMAT(/56H COVARIANCE = H**-1, WHERE H = FINITE-DIFFERENCE HESSIA
     1N/)                                                               
      IF (I .GT. 2) WRITE(PU,160)                                       
 160  FORMAT(/30H COVARIANCE = SCALE * J**T * J/)                       
      II = COV1 - 1                                                     
      DO 170 I = 1, P                                                   
         I1 = II + 1                                                    
         II = II + I                                                    
         WRITE(PU,180) I, (V(J), J = I1, II)                            
 170     CONTINUE                                                       
 180  FORMAT(4H ROW,I3,2X,5D12.3/(9X,5D12.3))                           
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DN2CVP FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DN2LRD(DR, IV, L, LH, LIV, LV, ND, NN, P, R, RD, V)    
C                                                                       
C  ***  COMPUTE REGRESSION DIAGNOSTIC AND DEFAULT COVARIANCE MATRIX FOR 
C        DRN2G  ***                                                     
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
      INTEGER LH, LIV, LV, ND, NN, P                                    
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION DR(ND,P), L(LH), R(NN), RD(NN), V(LV)            
C                                                                       
C  ***  CODED BY DAVID M. GAY (WINTER 1982, FALL 1983)  ***             
C                                                                       
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      DOUBLE PRECISION DD7TPR                                           
      EXTERNAL DD7TPR, DL7ITV, DL7IVM,DO7PRD, DV7SCP                    
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER COV, I, J, M, STEP1                                       
      DOUBLE PRECISION A, FF, S, T                                      
C                                                                       
C  ***  CONSTANTS  ***                                                  
C                                                                       
      DOUBLE PRECISION NEGONE, ONE, ONEV(1), ZERO                       
C                                                                       
C  ***  INTRINSIC FUNCTIONS  ***                                        
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
C                                                                       
C  ***  IV AND V SUBSCRIPTS  ***                                        
C                                                                       
      INTEGER F, H, MODE, RDREQ, STEP                                   
C/6                                                                     
C     DATA F/10/, H/56/, MODE/35/, RDREQ/57/, STEP/40/                  
C/7                                                                     
      PARAMETER (F=10, H=56, MODE=35, RDREQ=57, STEP=40)                
C/                                                                      
C/6                                                                     
C     DATA NEGONE/-1.D+0/, ONE/1.D+0/, ZERO/0.D+0/                      
C/7                                                                     
      PARAMETER (NEGONE=-1.D+0, ONE=1.D+0, ZERO=0.D+0)                  
C/                                                                      
      DATA ONEV(1)/1.D+0/                                               
C                                                                       
C++++++++++++++++++++++++++++++++  BODY  +++++++++++++++++++++++++++++++
C                                                                       
      STEP1 = IV(STEP)                                                  
      I = IV(RDREQ)                                                     
      IF (I .LE. 0) GO TO 999                                           
      IF (MOD(I,4) .LT. 2) GO TO 30                                     
      FF = ONE                                                          
      IF (V(F) .NE. ZERO) FF = ONE / DSQRT(DABS(V(F)))                  
      CALL DV7SCP(NN, RD, NEGONE)                                       
      DO 20 I = 1, NN                                                   
         A = R(I)**2                                                    
         M = STEP1                                                      
         DO 10 J = 1, P                                                 
            V(M) = DR(I,J)                                              
            M = M + 1                                                   
 10         CONTINUE                                                    
         CALL DL7IVM(P, V(STEP1), L, V(STEP1))                          
         S = DD7TPR(P, V(STEP1), V(STEP1))                              
         T = ONE - S                                                    
         IF (T .LE. ZERO) GO TO 20                                      
         A = A * S / T                                                  
         RD(I) = DSQRT(A) * FF                                          
 20      CONTINUE                                                       
C                                                                       
 30   IF (IV(MODE) - P .LT. 2) GO TO 999                                
C                                                                       
C  ***  COMPUTE DEFAULT COVARIANCE MATRIX  ***                          
C                                                                       
      COV = IABS(IV(H))                                                 
      DO 50 I = 1, NN                                                   
         M = STEP1                                                      
         DO 40 J = 1, P                                                 
            V(M) = DR(I,J)                                              
            M = M + 1                                                   
 40         CONTINUE                                                    
         CALL DL7IVM(P, V(STEP1), L, V(STEP1))                          
         CALL DL7ITV(P, V(STEP1), L, V(STEP1))                          
         CALL DO7PRD(1, LH, P, V(COV), ONEV, V(STEP1), V(STEP1))        
 50      CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DN2LRD FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DO7PRD(L, LS, P, S, W, Y, Z)                           
C                                                                       
C  ***  FOR I = 1..L, SET S = S + W(I)*Y(.,I)*(Z(.,I)**T), I.E.,        
C  ***        ADD W(I) TIMES THE OUTER PRODUCT OF Y(.,I) AND Z(.,I).    
C                                                                       
      INTEGER L, LS, P                                                  
      DOUBLE PRECISION S(LS), W(L), Y(P,L), Z(P,L)                      
C     DIMENSION S(P*(P+1)/2)                                            
C                                                                       
      INTEGER I, J, K, M                                                
      DOUBLE PRECISION WK, YI, ZERO                                     
      DATA ZERO/0.D+0/                                                  
C                                                                       
      DO 30 K = 1, L                                                    
         WK = W(K)                                                      
         IF (WK .EQ. ZERO) GO TO 30                                     
         M = 1                                                          
         DO 20 I = 1, P                                                 
              YI = WK * Y(I,K)                                          
              DO 10 J = 1, I                                            
                   S(M) = S(M) + YI*Z(J,K)                              
                   M = M + 1                                            
 10                CONTINUE                                             
 20           CONTINUE                                                  
 30      CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DO7PRD FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DPARCK(ALG, D, IV, LIV, LV, N, V)                      
C                                                                       
C  ***  CHECK ***SOL (VERSION 2.3) PARAMETERS, PRINT CHANGED VALUES  ***
C                                                                       
C  ***  ALG = 1 FOR REGRESSION, ALG = 2 FOR GENERAL UNCONSTRAINED OPT.  
C                                                                       
      INTEGER ALG, LIV, LV, N                                           
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION D(N), V(LV)                                      
C                                                                       
      DOUBLE PRECISION DR7MDC                                           
      EXTERNAL DIVSET, DR7MDC,DV7CPY,DV7DFL                             
C DIVSET  -- SUPPLIES DEFAULT VALUES TO BOTH IV AND V.                  
C DR7MDC -- RETURNS MACHINE-DEPENDENT CONSTANTS.                        
C DV7CPY  -- COPIES ONE VECTOR TO ANOTHER.                              
C DV7DFL  -- SUPPLIES DEFAULT PARAMETER VALUES TO V ALONE.              
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER ALG1, I, II, IV1, J, K, L, M, MIV1, MIV2, NDFALT, PARSV1, 
     1        PU                                                        
      INTEGER IJMP, JLIM(4), MINIV(4), NDFLT(4)                         
C/6S                                                                    
C     INTEGER VARNM(2), SH(2)                                           
C     REAL CNGD(3), DFLT(3), VN(2,34), WHICH(3)                         
C/7S                                                                    
      CHARACTER*1 VARNM(2), SH(2)                                       
      CHARACTER*4 CNGD(3), DFLT(3), VN(2,34), WHICH(3)                  
C/                                                                      
      DOUBLE PRECISION BIG, MACHEP, TINY, VK, VM(34), VX(34), ZERO      
C                                                                       
C  ***  IV AND V SUBSCRIPTS  ***                                        
C                                                                       
      INTEGER ALGSAV, DINIT, DTYPE, DTYPE0, EPSLON, INITS, IVNEED,      
     1        LASTIV, LASTV, LMAT, NEXTIV, NEXTV, NVDFLT, OLDN,         
     2        PARPRT, PARSAV, PERM, PRUNIT, VNEED                       
C                                                                       
C                                                                       
C/6                                                                     
C     DATA ALGSAV/51/, DINIT/38/, DTYPE/16/, DTYPE0/54/, EPSLON/19/,    
C    1     INITS/25/, IVNEED/3/, LASTIV/44/, LASTV/45/, LMAT/42/,       
C    2     NEXTIV/46/, NEXTV/47/, NVDFLT/50/, OLDN/38/, PARPRT/20/,     
C    3     PARSAV/49/, PERM/58/, PRUNIT/21/, VNEED/4/                   
C/7                                                                     
      PARAMETER (ALGSAV=51, DINIT=38, DTYPE=16, DTYPE0=54, EPSLON=19,   
     1           INITS=25, IVNEED=3, LASTIV=44, LASTV=45, LMAT=42,      
     2           NEXTIV=46, NEXTV=47, NVDFLT=50, OLDN=38, PARPRT=20,    
     3           PARSAV=49, PERM=58, PRUNIT=21, VNEED=4)                
      SAVE BIG, MACHEP, TINY                                            
C/                                                                      
C                                                                       
      DATA BIG/0.D+0/, MACHEP/-1.D+0/, TINY/1.D+0/, ZERO/0.D+0/         
C/6S                                                                    
C     DATA VN(1,1),VN(2,1)/4HEPSL,4HON../                               
C     DATA VN(1,2),VN(2,2)/4HPHMN,4HFC../                               
C     DATA VN(1,3),VN(2,3)/4HPHMX,4HFC../                               
C     DATA VN(1,4),VN(2,4)/4HDECF,4HAC../                               
C     DATA VN(1,5),VN(2,5)/4HINCF,4HAC../                               
C     DATA VN(1,6),VN(2,6)/4HRDFC,4HMN../                               
C     DATA VN(1,7),VN(2,7)/4HRDFC,4HMX../                               
C     DATA VN(1,8),VN(2,8)/4HTUNE,4HR1../                               
C     DATA VN(1,9),VN(2,9)/4HTUNE,4HR2../                               
C     DATA VN(1,10),VN(2,10)/4HTUNE,4HR3../                             
C     DATA VN(1,11),VN(2,11)/4HTUNE,4HR4../                             
C     DATA VN(1,12),VN(2,12)/4HTUNE,4HR5../                             
C     DATA VN(1,13),VN(2,13)/4HAFCT,4HOL../                             
C     DATA VN(1,14),VN(2,14)/4HRFCT,4HOL../                             
C     DATA VN(1,15),VN(2,15)/4HXCTO,4HL.../                             
C     DATA VN(1,16),VN(2,16)/4HXFTO,4HL.../                             
C     DATA VN(1,17),VN(2,17)/4HLMAX,4H0.../                             
C     DATA VN(1,18),VN(2,18)/4HLMAX,4HS.../                             
C     DATA VN(1,19),VN(2,19)/4HSCTO,4HL.../                             
C     DATA VN(1,20),VN(2,20)/4HDINI,4HT.../                             
C     DATA VN(1,21),VN(2,21)/4HDTIN,4HIT../                             
C     DATA VN(1,22),VN(2,22)/4HD0IN,4HIT../                             
C     DATA VN(1,23),VN(2,23)/4HDFAC,4H..../                             
C     DATA VN(1,24),VN(2,24)/4HDLTF,4HDC../                             
C     DATA VN(1,25),VN(2,25)/4HDLTF,4HDJ../                             
C     DATA VN(1,26),VN(2,26)/4HDELT,4HA0../                             
C     DATA VN(1,27),VN(2,27)/4HFUZZ,4H..../                             
C     DATA VN(1,28),VN(2,28)/4HRLIM,4HIT../                             
C     DATA VN(1,29),VN(2,29)/4HCOSM,4HIN../                             
C     DATA VN(1,30),VN(2,30)/4HHUBE,4HRC../                             
C     DATA VN(1,31),VN(2,31)/4HRSPT,4HOL../                             
C     DATA VN(1,32),VN(2,32)/4HSIGM,4HIN../                             
C     DATA VN(1,33),VN(2,33)/4HETA0,4H..../                             
C     DATA VN(1,34),VN(2,34)/4HBIAS,4H..../                             
C/7S                                                                    
      DATA VN(1,1),VN(2,1)/'EPSL','ON..'/                               
      DATA VN(1,2),VN(2,2)/'PHMN','FC..'/                               
      DATA VN(1,3),VN(2,3)/'PHMX','FC..'/                               
      DATA VN(1,4),VN(2,4)/'DECF','AC..'/                               
      DATA VN(1,5),VN(2,5)/'INCF','AC..'/                               
      DATA VN(1,6),VN(2,6)/'RDFC','MN..'/                               
      DATA VN(1,7),VN(2,7)/'RDFC','MX..'/                               
      DATA VN(1,8),VN(2,8)/'TUNE','R1..'/                               
      DATA VN(1,9),VN(2,9)/'TUNE','R2..'/                               
      DATA VN(1,10),VN(2,10)/'TUNE','R3..'/                             
      DATA VN(1,11),VN(2,11)/'TUNE','R4..'/                             
      DATA VN(1,12),VN(2,12)/'TUNE','R5..'/                             
      DATA VN(1,13),VN(2,13)/'AFCT','OL..'/                             
      DATA VN(1,14),VN(2,14)/'RFCT','OL..'/                             
      DATA VN(1,15),VN(2,15)/'XCTO','L...'/                             
      DATA VN(1,16),VN(2,16)/'XFTO','L...'/                             
      DATA VN(1,17),VN(2,17)/'LMAX','0...'/                             
      DATA VN(1,18),VN(2,18)/'LMAX','S...'/                             
      DATA VN(1,19),VN(2,19)/'SCTO','L...'/                             
      DATA VN(1,20),VN(2,20)/'DINI','T...'/                             
      DATA VN(1,21),VN(2,21)/'DTIN','IT..'/                             
      DATA VN(1,22),VN(2,22)/'D0IN','IT..'/                             
      DATA VN(1,23),VN(2,23)/'DFAC','....'/                             
      DATA VN(1,24),VN(2,24)/'DLTF','DC..'/                             
      DATA VN(1,25),VN(2,25)/'DLTF','DJ..'/                             
      DATA VN(1,26),VN(2,26)/'DELT','A0..'/                             
      DATA VN(1,27),VN(2,27)/'FUZZ','....'/                             
      DATA VN(1,28),VN(2,28)/'RLIM','IT..'/                             
      DATA VN(1,29),VN(2,29)/'COSM','IN..'/                             
      DATA VN(1,30),VN(2,30)/'HUBE','RC..'/                             
      DATA VN(1,31),VN(2,31)/'RSPT','OL..'/                             
      DATA VN(1,32),VN(2,32)/'SIGM','IN..'/                             
      DATA VN(1,33),VN(2,33)/'ETA0','....'/                             
      DATA VN(1,34),VN(2,34)/'BIAS','....'/                             
C/                                                                      
C                                                                       
      DATA VM(1)/1.0D-3/, VM(2)/-0.99D+0/, VM(3)/1.0D-3/, VM(4)/1.0D-2/,
     1     VM(5)/1.2D+0/, VM(6)/1.D-2/, VM(7)/1.2D+0/, VM(8)/0.D+0/,    
     2     VM(9)/0.D+0/, VM(10)/1.D-3/, VM(11)/-1.D+0/, VM(13)/0.D+0/,  
     3     VM(15)/0.D+0/, VM(16)/0.D+0/, VM(19)/0.D+0/, VM(20)/-10.D+0/,
     4     VM(21)/0.D+0/, VM(22)/0.D+0/, VM(23)/0.D+0/, VM(27)/1.01D+0/,
     5     VM(28)/1.D+10/, VM(30)/0.D+0/, VM(31)/0.D+0/, VM(32)/0.D+0/, 
     6     VM(34)/0.D+0/                                                
      DATA VX(1)/0.9D+0/, VX(2)/-1.D-3/, VX(3)/1.D+1/, VX(4)/0.8D+0/,   
     1     VX(5)/1.D+2/, VX(6)/0.8D+0/, VX(7)/1.D+2/, VX(8)/0.5D+0/,    
     2     VX(9)/0.5D+0/, VX(10)/1.D+0/, VX(11)/1.D+0/, VX(14)/0.1D+0/, 
     3     VX(15)/1.D+0/, VX(16)/1.D+0/, VX(19)/1.D+0/, VX(23)/1.D+0/,  
     4     VX(24)/1.D+0/, VX(25)/1.D+0/, VX(26)/1.D+0/, VX(27)/1.D+10/, 
     5     VX(29)/1.D+0/, VX(31)/1.D+0/, VX(32)/1.D+0/, VX(33)/1.D+0/,  
     6     VX(34)/1.D+0/                                                
C                                                                       
C/6S                                                                    
C     DATA VARNM(1)/1HP/, VARNM(2)/1HP/, SH(1)/1HS/, SH(2)/1HH/         
C     DATA CNGD(1),CNGD(2),CNGD(3)/4H---C,4HHANG,4HED V/,               
C    1     DFLT(1),DFLT(2),DFLT(3)/4HNOND,4HEFAU,4HLT V/                
C/7S                                                                    
      DATA VARNM(1)/'P'/, VARNM(2)/'P'/, SH(1)/'S'/, SH(2)/'H'/         
      DATA CNGD(1),CNGD(2),CNGD(3)/'---C','HANG','ED V'/,               
     1     DFLT(1),DFLT(2),DFLT(3)/'NOND','EFAU','LT V'/                
C/                                                                      
      DATA IJMP/33/, JLIM(1)/0/, JLIM(2)/24/, JLIM(3)/0/, JLIM(4)/24/,  
     1     NDFLT(1)/32/, NDFLT(2)/25/, NDFLT(3)/32/, NDFLT(4)/25/       
      DATA MINIV(1)/82/, MINIV(2)/59/, MINIV(3)/103/, MINIV(4)/103/     
C                                                                       
C...............................  BODY  ................................
C                                                                       
      PU = 0                                                            
      IF (PRUNIT .LE. LIV) PU = IV(PRUNIT)                              
      IF (ALGSAV .GT. LIV) GO TO 20                                     
      IF (ALG .EQ. IV(ALGSAV)) GO TO 20                                 
         IF (PU .NE. 0) WRITE(PU,10) ALG, IV(ALGSAV)                    
 10      FORMAT(/40H THE FIRST PARAMETER TO DIVSET SHOULD BE,I3,        
     1          12H RATHER THAN,I3)                                     
         IV(1) = 67                                                     
         GO TO 999                                                      
 20   IF (ALG .LT. 1 .OR. ALG .GT. 4) GO TO 340                         
      MIV1 = MINIV(ALG)                                                 
      IF (IV(1) .EQ. 15) GO TO 360                                      
      ALG1 = MOD(ALG-1,2) + 1                                           
      IF (IV(1) .EQ. 0) CALL DIVSET(ALG, IV, LIV, LV, V)                
      IV1 = IV(1)                                                       
      IF (IV1 .NE. 13 .AND. IV1 .NE. 12) GO TO 30                       
      IF (PERM .LE. LIV) MIV1 = MAX0(MIV1, IV(PERM) - 1)                
      IF (IVNEED .LE. LIV) MIV2 = MIV1 + MAX0(IV(IVNEED), 0)            
      IF (LASTIV .LE. LIV) IV(LASTIV) = MIV2                            
      IF (LIV .LT. MIV1) GO TO 300                                      
      IV(IVNEED) = 0                                                    
      IV(LASTV) = MAX0(IV(VNEED), 0) + IV(LMAT) - 1                     
      IV(VNEED) = 0                                                     
      IF (LIV .LT. MIV2) GO TO 300                                      
      IF (LV .LT. IV(LASTV)) GO TO 320                                  
 30   IF (IV1 .LT. 12 .OR. IV1 .GT. 14) GO TO 60                        
         IF (N .GE. 1) GO TO 50                                         
              IV(1) = 81                                                
              IF (PU .EQ. 0) GO TO 999                                  
              WRITE(PU,40) VARNM(ALG1), N                               
 40           FORMAT(/8H /// BAD,A1,2H =,I5)                            
              GO TO 999                                                 
 50      IF (IV1 .NE. 14) IV(NEXTIV) = IV(PERM)                         
         IF (IV1 .NE. 14) IV(NEXTV) = IV(LMAT)                          
         IF (IV1 .EQ. 13) GO TO 999                                     
         K = IV(PARSAV) - EPSLON                                        
         CALL DV7DFL(ALG1, LV-K, V(K+1))                                
         IV(DTYPE0) = 2 - ALG1                                          
         IV(OLDN) = N                                                   
         WHICH(1) = DFLT(1)                                             
         WHICH(2) = DFLT(2)                                             
         WHICH(3) = DFLT(3)                                             
         GO TO 110                                                      
 60   IF (N .EQ. IV(OLDN)) GO TO 80                                     
         IV(1) = 17                                                     
         IF (PU .EQ. 0) GO TO 999                                       
         WRITE(PU,70) VARNM(ALG1), IV(OLDN), N                          
 70      FORMAT(/5H /// ,1A1,14H CHANGED FROM ,I5,4H TO ,I5)            
         GO TO 999                                                      
C                                                                       
 80   IF (IV1 .LE. 11 .AND. IV1 .GE. 1) GO TO 100                       
         IV(1) = 80                                                     
         IF (PU .NE. 0) WRITE(PU,90) IV1                                
 90      FORMAT(/13H ///  IV(1) =,I5,28H SHOULD BE BETWEEN 0 AND 14.)   
         GO TO 999                                                      
C                                                                       
 100  WHICH(1) = CNGD(1)                                                
      WHICH(2) = CNGD(2)                                                
      WHICH(3) = CNGD(3)                                                
C                                                                       
 110  IF (IV1 .EQ. 14) IV1 = 12                                         
      IF (BIG .GT. TINY) GO TO 120                                      
         TINY = DR7MDC(1)                                               
         MACHEP = DR7MDC(3)                                             
         BIG = DR7MDC(6)                                                
         VM(12) = MACHEP                                                
         VX(12) = BIG                                                   
         VX(13) = BIG                                                   
         VM(14) = MACHEP                                                
         VM(17) = TINY                                                  
         VX(17) = BIG                                                   
         VM(18) = TINY                                                  
         VX(18) = BIG                                                   
         VX(20) = BIG                                                   
         VX(21) = BIG                                                   
         VX(22) = BIG                                                   
         VM(24) = MACHEP                                                
         VM(25) = MACHEP                                                
         VM(26) = MACHEP                                                
         VX(28) = DR7MDC(5)                                             
         VM(29) = MACHEP                                                
         VX(30) = BIG                                                   
         VM(33) = MACHEP                                                
 120  M = 0                                                             
      I = 1                                                             
      J = JLIM(ALG1)                                                    
      K = EPSLON                                                        
      NDFALT = NDFLT(ALG1)                                              
      DO 150 L = 1, NDFALT                                              
         VK = V(K)                                                      
         IF (VK .GE. VM(I) .AND. VK .LE. VX(I)) GO TO 140               
              M = K                                                     
              IF (PU .NE. 0) WRITE(PU,130) VN(1,I), VN(2,I), K, VK,     
     1                                    VM(I), VX(I)                  
 130          FORMAT(/6H ///  ,2A4,5H.. V(,I2,3H) =,D11.3,7H SHOULD,    
     1               11H BE BETWEEN,D11.3,4H AND,D11.3)                 
 140     K = K + 1                                                      
         I = I + 1                                                      
         IF (I .EQ. J) I = IJMP                                         
 150     CONTINUE                                                       
C                                                                       
      IF (IV(NVDFLT) .EQ. NDFALT) GO TO 170                             
         IV(1) = 51                                                     
         IF (PU .EQ. 0) GO TO 999                                       
         WRITE(PU,160) IV(NVDFLT), NDFALT                               
 160     FORMAT(/13H IV(NVDFLT) =,I5,13H RATHER THAN ,I5)               
         GO TO 999                                                      
 170  IF ((IV(DTYPE) .GT. 0 .OR. V(DINIT) .GT. ZERO) .AND. IV1 .EQ. 12) 
     1                  GO TO 200                                       
      DO 190 I = 1, N                                                   
         IF (D(I) .GT. ZERO) GO TO 190                                  
              M = 18                                                    
              IF (PU .NE. 0) WRITE(PU,180) I, D(I)                      
 180     FORMAT(/8H ///  D(,I3,3H) =,D11.3,19H SHOULD BE POSITIVE)      
 190     CONTINUE                                                       
 200  IF (M .EQ. 0) GO TO 210                                           
         IV(1) = M                                                      
         GO TO 999                                                      
C                                                                       
 210  IF (PU .EQ. 0 .OR. IV(PARPRT) .EQ. 0) GO TO 999                   
      IF (IV1 .NE. 12 .OR. IV(INITS) .EQ. ALG1-1) GO TO 230             
         M = 1                                                          
         WRITE(PU,220) SH(ALG1), IV(INITS)                              
 220     FORMAT(/22H NONDEFAULT VALUES..../5H INIT,A1,14H..... IV(25) =,
     1          I3)                                                     
 230  IF (IV(DTYPE) .EQ. IV(DTYPE0)) GO TO 250                          
         IF (M .EQ. 0) WRITE(PU,260) WHICH                              
         M = 1                                                          
         WRITE(PU,240) IV(DTYPE)                                        
 240     FORMAT(20H DTYPE..... IV(16) =,I3)                             
 250  I = 1                                                             
      J = JLIM(ALG1)                                                    
      K = EPSLON                                                        
      L = IV(PARSAV)                                                    
      NDFALT = NDFLT(ALG1)                                              
      DO 290 II = 1, NDFALT                                             
         IF (V(K) .EQ. V(L)) GO TO 280                                  
              IF (M .EQ. 0) WRITE(PU,260) WHICH                         
 260          FORMAT(/1H ,3A4,9HALUES..../)                             
              M = 1                                                     
              WRITE(PU,270) VN(1,I), VN(2,I), K, V(K)                   
 270          FORMAT(1X,2A4,5H.. V(,I2,3H) =,D15.7)                     
 280     K = K + 1                                                      
         L = L + 1                                                      
         I = I + 1                                                      
         IF (I .EQ. J) I = IJMP                                         
 290     CONTINUE                                                       
C                                                                       
      IV(DTYPE0) = IV(DTYPE)                                            
      PARSV1 = IV(PARSAV)                                               
      CALL DV7CPY(IV(NVDFLT), V(PARSV1), V(EPSLON))                     
      GO TO 999                                                         
C                                                                       
 300  IV(1) = 15                                                        
      IF (PU .EQ. 0) GO TO 999                                          
      WRITE(PU,310) LIV, MIV2                                           
 310  FORMAT(/10H /// LIV =,I5,17H MUST BE AT LEAST,I5)                 
      IF (LIV .LT. MIV1) GO TO 999                                      
      IF (LV .LT. IV(LASTV)) GO TO 320                                  
      GO TO 999                                                         
C                                                                       
 320  IV(1) = 16                                                        
      IF (PU .NE. 0) WRITE(PU,330) LV, IV(LASTV)                        
 330  FORMAT(/9H /// LV =,I5,17H MUST BE AT LEAST,I5)                   
      GO TO 999                                                         
C                                                                       
 340  IV(1) = 67                                                        
      IF (PU .NE. 0) WRITE(PU,350) ALG                                  
 350  FORMAT(/10H /// ALG =,I5,21H MUST BE 1 2, 3, OR 4)                
      GO TO 999                                                         
 360  IF (PU .NE. 0) WRITE(PU,370) LIV, MIV1                            
 370  FORMAT(/10H /// LIV =,I5,17H MUST BE AT LEAST,I5,                 
     1       37H TO COMPUTE TRUE MIN. LIV AND MIN. LV)                  
      IF (LASTIV .LE. LIV) IV(LASTIV) = MIV1                            
      IF (LASTV .LE. LIV) IV(LASTV) = 0                                 
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DPARCK FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DIVSET(ALG, IV, LIV, LV, V)                            
C                                                                       
C  ***  SUPPLY ***SOL (VERSION 2.3) DEFAULT VALUES TO IV AND V  ***     
C                                                                       
C  ***  ALG = 1 MEANS REGRESSION CONSTANTS.                             
C  ***  ALG = 2 MEANS GENERAL UNCONSTRAINED OPTIMIZATION CONSTANTS.     
C                                                                       
      INTEGER LIV, LV                                                   
      INTEGER ALG, IV(LIV)                                              
      DOUBLE PRECISION V(LV)                                            
C                                                                       
      INTEGER I7MDCN                                                    
      EXTERNAL I7MDCN,DV7DFL                                            
C I7MDCN... RETURNS MACHINE-DEPENDENT INTEGER CONSTANTS.                
C DV7DFL.... PROVIDES DEFAULT VALUES TO V.                              
C                                                                       
      INTEGER ALG1, MIV, MV                                             
      INTEGER MINIV(4), MINV(4)                                         
C                                                                       
C  ***  SUBSCRIPTS FOR IV  ***                                          
C                                                                       
      INTEGER ALGSAV, COVPRT, COVREQ, DRADPR, DTYPE, HC, IERR, INITH,   
     1        INITS, IPIVOT, IVNEED, LASTIV, LASTV, LMAT, MXFCAL,       
     2        MXITER, NFCOV, NGCOV, NVDFLT, NVSAVE, OUTLEV, PARPRT,     
     3        PARSAV, PERM, PRUNIT, QRTYP, RDREQ, RMAT, SOLPRT, STATPR, 
     4        VNEED, VSAVE, X0PRT                                       
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C/6                                                                     
C     DATA ALGSAV/51/, COVPRT/14/, COVREQ/15/, DRADPR/101/, DTYPE/16/,  
C    1     HC/71/, IERR/75/, INITH/25/, INITS/25/, IPIVOT/76/,          
C    2     IVNEED/3/, LASTIV/44/, LASTV/45/, LMAT/42/, MXFCAL/17/,      
C    3     MXITER/18/, NFCOV/52/, NGCOV/53/, NVDFLT/50/, NVSAVE/9/,     
C    4     OUTLEV/19/, PARPRT/20/, PARSAV/49/, PERM/58/, PRUNIT/21/,    
C    5     QRTYP/80/, RDREQ/57/, RMAT/78/, SOLPRT/22/, STATPR/23/,      
C    6     VNEED/4/, VSAVE/60/, X0PRT/24/                               
C/7                                                                     
      PARAMETER (ALGSAV=51, COVPRT=14, COVREQ=15, DRADPR=101, DTYPE=16, 
     1           HC=71, IERR=75, INITH=25, INITS=25, IPIVOT=76,         
     2           IVNEED=3, LASTIV=44, LASTV=45, LMAT=42, MXFCAL=17,     
     3           MXITER=18, NFCOV=52, NGCOV=53, NVDFLT=50, NVSAVE=9,    
     4           OUTLEV=19, PARPRT=20, PARSAV=49, PERM=58, PRUNIT=21,   
     5           QRTYP=80, RDREQ=57, RMAT=78, SOLPRT=22, STATPR=23,     
     6           VNEED=4, VSAVE=60, X0PRT=24)                           
C/                                                                      
      DATA MINIV(1)/82/, MINIV(2)/59/, MINIV(3)/103/, MINIV(4)/103/,    
     1     MINV(1)/98/, MINV(2)/71/, MINV(3)/101/, MINV(4)/85/          
C                                                                       
C-------------------------------  BODY  --------------------------------
C                                                                       
      IF (PRUNIT .LE. LIV) IV(PRUNIT) = I7MDCN(1)                       
      IF (ALGSAV .LE. LIV) IV(ALGSAV) = ALG                             
      IF (ALG .LT. 1 .OR. ALG .GT. 4) GO TO 40                          
      MIV = MINIV(ALG)                                                  
      IF (LIV .LT. MIV) GO TO 20                                        
      MV = MINV(ALG)                                                    
      IF (LV .LT. MV) GO TO 30                                          
      ALG1 = MOD(ALG-1,2) + 1                                           
      CALL DV7DFL(ALG1, LV, V)                                          
      IV(1) = 12                                                        
      IF (ALG .GT. 2) IV(DRADPR) = 1                                    
      IV(IVNEED) = 0                                                    
      IV(LASTIV) = MIV                                                  
      IV(LASTV) = MV                                                    
      IV(LMAT) = MV + 1                                                 
      IV(MXFCAL) = 200                                                  
      IV(MXITER) = 150                                                  
      IV(OUTLEV) = 1                                                    
      IV(PARPRT) = 1                                                    
      IV(PERM) = MIV + 1                                                
      IV(SOLPRT) = 1                                                    
      IV(STATPR) = 1                                                    
      IV(VNEED) = 0                                                     
      IV(X0PRT) = 1                                                     
C                                                                       
      IF (ALG1 .GE. 2) GO TO 10                                         
C                                                                       
C  ***  REGRESSION  VALUES                                              
C                                                                       
      IV(COVPRT) = 3                                                    
      IV(COVREQ) = 1                                                    
      IV(DTYPE) = 1                                                     
      IV(HC) = 0                                                        
      IV(IERR) = 0                                                      
      IV(INITS) = 0                                                     
      IV(IPIVOT) = 0                                                    
      IV(NVDFLT) = 32                                                   
      IV(VSAVE) = 58                                                    
      IF (ALG .GT. 2) IV(VSAVE) = IV(VSAVE) + 3                         
      IV(PARSAV) = IV(VSAVE) + NVSAVE                                   
      IV(QRTYP) = 1                                                     
      IV(RDREQ) = 3                                                     
      IV(RMAT) = 0                                                      
      GO TO 999                                                         
C                                                                       
C  ***  GENERAL OPTIMIZATION VALUES                                     
C                                                                       
 10   IV(DTYPE) = 0                                                     
      IV(INITH) = 1                                                     
      IV(NFCOV) = 0                                                     
      IV(NGCOV) = 0                                                     
      IV(NVDFLT) = 25                                                   
      IV(PARSAV) = 47                                                   
      IF (ALG .GT. 2) IV(PARSAV) = 61                                   
      GO TO 999                                                         
C                                                                       
 20   IV(1) = 15                                                        
      GO TO 999                                                         
C                                                                       
 30   IV(1) = 16                                                        
      GO TO 999                                                         
C                                                                       
 40   IV(1) = 67                                                        
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DIVSET FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DQ7APL(NN, N, P, J, R, IERR)                           
C     *****PARAMETERS.                                                  
      INTEGER NN, N, P, IERR                                            
      DOUBLE PRECISION J(NN,P), R(N)                                    
C                                                                       
C     ..................................................................
C     ..................................................................
C                                                                       
C     *****PURPOSE.                                                     
C     THIS SUBROUTINE APPLIES TO R THE ORTHOGONAL TRANSFORMATIONS       
C     STORED IN J BY QRFACT                                             
C                                                                       
C     *****PARAMETER DESCRIPTION.                                       
C     ON INPUT.                                                         
C                                                                       
C        NN IS THE ROW DIMENSION OF THE MATRIX J AS DECLARED IN         
C             THE CALLING PROGRAM DIMENSION STATEMENT                   
C                                                                       
C        N IS THE NUMBER OF ROWS OF J AND THE SIZE OF THE VECTOR R      
C                                                                       
C        P IS THE NUMBER OF COLUMNS OF J AND THE SIZE OF SIGMA          
C                                                                       
C        J CONTAINS ON AND BELOW ITS DIAGONAL THE COLUMN VECTORS        
C             U WHICH DETERMINE THE HOUSEHOLDER TRANSFORMATIONS         
C             IDENT - U*U.TRANSPOSE                                     
C                                                                       
C        R IS THE RIGHT HAND SIDE VECTOR TO WHICH THE ORTHOGONAL        
C             TRANSFORMATIONS WILL BE APPLIED                           
C                                                                       
C        IERR IF NON-ZERO INDICATES THAT NOT ALL THE TRANSFORMATIONS    
C             WERE SUCCESSFULLY DETERMINED AND ONLY THE FIRST           
C             ABS(IERR) - 1 TRANSFORMATIONS WILL BE USED                
C                                                                       
C     ON OUTPUT.                                                        
C                                                                       
C        R HAS BEEN OVERWRITTEN BY ITS TRANSFORMED IMAGE                
C                                                                       
C     *****APPLICATION AND USAGE RESTRICTIONS.                          
C     NONE                                                              
C                                                                       
C     *****ALGORITHM NOTES.                                             
C     THE VECTORS U WHICH DETERMINE THE HOUSEHOLDER TRANSFORMATIONS     
C     ARE NORMALIZED SO THAT THEIR 2-NORM SQUARED IS 2.  THE USE OF     
C     THESE TRANSFORMATIONS HERE IS IN THE SPIRIT OF (1).               
C                                                                       
C     *****SUBROUTINES AND FUNCTIONS CALLED.                            
C                                                                       
C     DD7TPR - FUNCTION, RETURNS THE INNER PRODUCT OF VECTORS           
C                                                                       
C     *****REFERENCES.                                                  
C     (1) BUSINGER, P. A., AND GOLUB, G. H. (1965), LINEAR LEAST SQUARES
C        SOLUTIONS BY HOUSEHOLDER TRANSFORMATIONS, NUMER. MATH. 7,      
C        PP. 269-276.                                                   
C                                                                       
C     *****HISTORY.                                                     
C     DESIGNED BY DAVID M. GAY, CODED BY STEPHEN C. PETERS (WINTER 1977)
C     CALL ON DV2AXY SUBSTITUTED FOR DO LOOP, FALL 1983.                
C                                                                       
C     *****GENERAL.                                                     
C                                                                       
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH           
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS         
C     MCS-7600324, DCR75-10143, 76-14311DSS, AND MCS76-11989.           
C                                                                       
C     ..................................................................
C     ..................................................................
C                                                                       
C     *****LOCAL VARIABLES.                                             
      INTEGER K, L, NL1                                                 
C     *****FUNCTIONS.                                                   
      DOUBLE PRECISION DD7TPR                                           
      EXTERNAL DD7TPR,DV2AXY                                            
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
      K = P                                                             
      IF (IERR .NE. 0) K = IABS(IERR) - 1                               
      IF ( K .EQ. 0) GO TO 999                                          
C                                                                       
      DO 20 L = 1, K                                                    
         NL1 = N - L + 1                                                
         CALL DV2AXY(NL1, R(L), -DD7TPR(NL1,J(L,L),R(L)), J(L,L), R(L)) 
 20   CONTINUE                                                          
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DQ7APL FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DQ7RAD(N, NN, P, QTR, QTRSET, RMAT, W, Y)              
C                                                                       
C  ***  ADD ROWS W TO QR FACTORIZATION WITH R MATRIX RMAT AND           
C  ***  Q**T * RESIDUAL = QTR.  Y = NEW COMPONENTS OF RESIDUAL          
C  ***  CORRESPONDING TO W.  QTR, Y REFERENCED ONLY IF QTRSET = .TRUE.  
C                                                                       
      LOGICAL QTRSET                                                    
      INTEGER N, NN, P                                                  
      DOUBLE PRECISION QTR(P), RMAT(1), W(NN,P), Y(N)                   
C     DIMENSION RMAT(P*(P+1)/2)                                         
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
      DOUBLE PRECISION DD7TPR, DR7MDC, DV2NRM                           
      EXTERNAL DD7TPR, DR7MDC,DV2AXY, DV7SCL, DV2NRM                    
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, II, IJ, IP1, J, K, NK                                  
      DOUBLE PRECISION ARI, QRI, RI, S, T, WI                           
      DOUBLE PRECISION BIG, BIGRT, ONE, TINY, TINYRT, ZERO              
C/7                                                                     
      SAVE BIGRT, TINY, TINYRT                                          
C/                                                                      
      DATA BIG/-1.D+0/, BIGRT/-1.D+0/, ONE/1.D+0/, TINY/0.D+0/,         
     1     TINYRT/0.D+0/, ZERO/0.D+0/                                   
C                                                                       
C------------------------------ BODY -----------------------------------
C                                                                       
      IF (TINY .GT. ZERO) GO TO 10                                      
         TINY = DR7MDC(1)                                               
         BIG = DR7MDC(6)                                                
         IF (TINY*BIG .LT. ONE) TINY = ONE / BIG                        
 10   K = 1                                                             
      NK = N                                                            
      II = 0                                                            
      DO 180 I = 1, P                                                   
         II = II + I                                                    
         IP1 = I + 1                                                    
         IJ = II + I                                                    
         IF (NK .LE. 1) T = DABS(W(K,I))                                
         IF (NK .GT. 1) T = DV2NRM(NK, W(K,I))                          
         IF (T .LT. TINY) GOTO  180                                     
         RI = RMAT(II)                                                  
         IF (RI .NE. ZERO) GO TO 100                                    
            IF (NK .GT. 1) GO TO 30                                     
               IJ = II                                                  
               DO 20 J = I, P                                           
                  RMAT(IJ) = W(K,J)                                     
                  IJ = IJ + J                                           
 20               CONTINUE                                              
               IF (QTRSET) QTR(I) = Y(K)                                
               W(K,I) = ZERO                                            
               GO TO 999                                                
 30         WI = W(K,I)                                                 
            IF (BIGRT .GT. ZERO) GO TO 40                               
               BIGRT = DR7MDC(5)                                        
               TINYRT = DR7MDC(2)                                       
 40         IF (T .LE. TINYRT) GO TO 50                                 
            IF (T .GE. BIGRT) GO TO 50                                  
               IF (WI .LT. ZERO) T = -T                                 
               WI = WI + T                                              
               S = DSQRT(T * WI)                                        
               GO TO 70                                                 
 50         S = DSQRT(T)                                                
            IF (WI .LT. ZERO) GO TO 60                                  
               WI = WI + T                                              
               S = S * DSQRT(WI)                                        
               GO TO 70                                                 
 60         T = -T                                                      
            WI = WI + T                                                 
            S = S * DSQRT(-WI)                                          
 70         W(K,I) = WI                                                 
            CALL DV7SCL(NK, W(K,I), ONE/S, W(K,I))                      
            RMAT(II) = -T                                               
            IF (.NOT. QTRSET) GO TO 80                                  
            CALL DV2AXY(NK, Y(K), -DD7TPR(NK,Y(K),W(K,I)), W(K,I), Y(K))
            QTR(I) = Y(K)                                               
 80         IF (IP1 .GT. P) GO TO 999                                   
            DO 90 J = IP1, P                                            
               CALL DV2AXY(NK, W(K,J), -DD7TPR(NK,W(K,J),W(K,I)),       
     1                    W(K,I), W(K,J))                               
               RMAT(IJ) = W(K,J)                                        
               IJ = IJ + J                                              
 90            CONTINUE                                                 
            IF (NK .LE. 1) GO TO 999                                    
            K = K + 1                                                   
            NK = NK - 1                                                 
            GO TO 180                                                   
C                                                                       
 100     ARI = DABS(RI)                                                 
         IF (ARI .GT. T) GO TO 110                                      
            T = T * DSQRT(ONE + (ARI/T)**2)                             
            GO TO 120                                                   
 110     T = ARI * DSQRT(ONE + (T/ARI)**2)                              
 120     IF (RI .LT. ZERO) T = -T                                       
         RI = RI + T                                                    
         RMAT(II) = -T                                                  
         S = -RI / T                                                    
         IF (NK .LE. 1) GO TO 150                                       
         CALL DV7SCL(NK, W(K,I), ONE/RI, W(K,I))                        
         IF (.NOT. QTRSET) GO TO 130                                    
            QRI = QTR(I)                                                
            T = S * ( QRI  +  DD7TPR(NK, Y(K), W(K,I)) )                
            QTR(I) = QRI + T                                            
 130     IF (IP1 .GT. P) GO TO 999                                      
         IF (QTRSET) CALL DV2AXY(NK, Y(K), T, W(K,I), Y(K))             
         DO 140 J = IP1, P                                              
            RI = RMAT(IJ)                                               
            T = S * ( RI  +  DD7TPR(NK, W(K,J), W(K,I)) )               
            CALL DV2AXY(NK, W(K,J), T, W(K,I), W(K,J))                  
            RMAT(IJ) = RI + T                                           
            IJ = IJ + J                                                 
 140        CONTINUE                                                    
         GO TO 180                                                      
C                                                                       
 150     WI = W(K,I) / RI                                               
         W(K,I) = WI                                                    
         IF (.NOT. QTRSET) GO TO 160                                    
            QRI = QTR(I)                                                
            T = S * ( QRI + Y(K)*WI )                                   
            QTR(I) = QRI + T                                            
 160     IF (IP1 .GT. P) GO TO 999                                      
         IF (QTRSET) Y(K) = T*WI + Y(K)                                 
         DO 170 J = IP1, P                                              
            RI = RMAT(IJ)                                               
            T = S * (RI + W(K,J)*WI)                                    
            W(K,J) = W(K,J) + T*WI                                      
            RMAT(IJ) = RI + T                                           
            IJ = IJ + J                                                 
 170        CONTINUE                                                    
 180     CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DQ7RAD FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DQ7RFH(IERR, IPIVOT, N, NN, NOPIVK, P, Q, R, RLEN, W)  
C                                                                       
C  ***  COMPUTE QR FACTORIZATION VIA HOUSEHOLDER TRANSFORMATIONS        
C  ***  WITH COLUMN PIVOTING  ***                                       
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER IERR, N, NN, NOPIVK, P, RLEN                              
      INTEGER IPIVOT(P)                                                 
      DOUBLE PRECISION Q(NN,P), R(RLEN), W(P)                           
C     DIMENSION R(P*(P+1)/2)                                            
C                                                                       
C----------------------------  DESCRIPTION  ----------------------------
C                                                                       
C    THIS ROUTINE COMPUTES A QR FACTORIZATION (VIA HOUSEHOLDER TRANS-   
C FORMATIONS) OF THE MATRIX  A  THAT ON INPUT IS STORED IN Q.           
C IF  NOPIVK  ALLOWS IT, THIS ROUTINE DOES COLUMN PIVOTING -- IF        
C K .GT. NOPIVK,  THEN ORIGINAL COLUMN  K  IS ELIGIBLE FOR PIVOTING.    
C THE  Q  AND  R  RETURNED ARE SUCH THAT COLUMN  I  OF  Q*R  EQUALS     
C COLUMN  IPIVOT(I)  OF THE ORIGINAL MATRIX  A.  THE UPPER TRIANGULAR   
C MATRIX  R  IS STORED COMPACTLY BY COLUMNS, I.E., THE OUTPUT VECTOR  R 
C CONTAINS  R(1,1), R(1,2), R(2,2), R(1,3), R(2,3), ..., R(P,P) (IN     
C THAT ORDER).  IF ALL GOES WELL, THEN THIS ROUTINE SETS  IERR = 0.     
C BUT IF (PERMUTED) COLUMN  K  OF  A  IS LINEARLY DEPENDENT ON          
C (PERMUTED) COLUMNS 1,2,...,K-1, THEN  IERR  IS SET TO  K AND THE R    
C MATRIX RETURNED HAS  R(I,J) = 0  FOR  I .GE. K  AND  J .GE. K.        
C    THE ORIGINAL MATRIX  A  IS AN N BY P MATRIX.  NN  IS THE LEAD      
C DIMENSION OF THE ARRAY  Q  AND MUST SATISFY  NN .GE. N.  NO           
C PARAMETER CHECKING IS DONE.                                           
C    PIVOTING IS DONE AS THOUGH ALL COLUMNS OF Q WERE FIRST             
C SCALED TO HAVE THE SAME NORM.  IF COLUMN K IS ELIGIBLE FOR            
C PIVOTING AND ITS (SCALED) NORM**2 LOSS IS MORE THAN THE               
C MINIMUM SUCH LOSS (OVER COLUMNS K THRU P), THEN COLUMN K IS           
C SWAPPED WITH THE COLUMN OF LEAST NORM**2 LOSS.                        
C                                                                       
C        CODED BY DAVID M. GAY (FALL 1979, SPRING 1984).                
C                                                                       
C--------------------------  LOCAL VARIABLES  --------------------------
C                                                                       
      INTEGER I, II, J, K, KK, KM1, KP1, NK1                            
      DOUBLE PRECISION AK, QKK, S, SINGTL, T, T1, WK                    
      DOUBLE PRECISION DD7TPR, DR7MDC, DV2NRM                           
      EXTERNAL DD7TPR, DR7MDC,DV2AXY, DV7SCL, DV7SCP,DV7SWP, DV2NRM     
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
      DOUBLE PRECISION BIG, BIGRT, MEPS10, ONE, TEN, TINY, TINYRT,      
     1                 WTOL, ZERO                                       
C/6                                                                     
C     DATA ONE/1.0D+0/, TEN/1.D+1/, WTOL/0.75D+0/, ZERO/0.0D+0/         
C/7                                                                     
      PARAMETER (ONE=1.0D+0, TEN=1.D+1, WTOL=0.75D+0, ZERO=0.0D+0)      
      SAVE BIGRT, MEPS10, TINY, TINYRT                                  
C/                                                                      
      DATA BIGRT/0.0D+0/, MEPS10/0.0D+0/, TINY/0.D+0/, TINYRT/0.D+0/    
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      IERR = 0                                                          
      IF (MEPS10 .GT. ZERO) GO TO 10                                    
          BIGRT = DR7MDC(5)                                             
          MEPS10 = TEN * DR7MDC(3)                                      
          TINYRT = DR7MDC(2)                                            
          TINY = DR7MDC(1)                                              
          BIG = DR7MDC(6)                                               
          IF (TINY*BIG .LT. ONE) TINY = ONE / BIG                       
 10   SINGTL = FLOAT(MAX0(N,P)) * MEPS10                                
C                                                                       
C  ***  INITIALIZE W, IPIVOT, AND DIAG(R)  ***                          
C                                                                       
      J = 0                                                             
      DO 40 I = 1, P                                                    
         IPIVOT(I) = I                                                  
         T = DV2NRM(N, Q(1,I))                                          
         IF (T .GT. ZERO) GO TO 20                                      
              W(I) = ONE                                                
              GO TO 30                                                  
 20      W(I) = ZERO                                                    
 30      J = J + I                                                      
         R(J) = T                                                       
 40      CONTINUE                                                       
C                                                                       
C  ***  MAIN LOOP  ***                                                  
C                                                                       
      KK = 0                                                            
      NK1 = N + 1                                                       
      DO 130 K = 1, P                                                   
         IF (NK1 .LE. 1) GO TO 999                                      
         NK1 = NK1 - 1                                                  
         KK = KK + K                                                    
         KP1 = K + 1                                                    
         IF (K .LE. NOPIVK) GO TO 60                                    
         IF (K .GE. P) GO TO 60                                         
C                                                                       
C        ***  FIND COLUMN WITH MINIMUM WEIGHT LOSS  ***                 
C                                                                       
              T = W(K)                                                  
              IF (T .LE. ZERO) GO TO 60                                 
              J = K                                                     
              DO 50 I = KP1, P                                          
                   IF (W(I) .GE. T) GO TO 50                            
                        T = W(I)                                        
                        J = I                                           
 50                CONTINUE                                             
              IF (J .EQ. K) GO TO 60                                    
C                                                                       
C             ***  INTERCHANGE COLUMNS K AND J  ***                     
C                                                                       
                   I = IPIVOT(K)                                        
                   IPIVOT(K) = IPIVOT(J)                                
                   IPIVOT(J) = I                                        
                   W(J) = W(K)                                          
                   W(K) = T                                             
                   I = J*(J+1)/2                                        
                   T1 = R(I)                                            
                   R(I) = R(KK)                                         
                   R(KK) = T1                                           
                   CALL DV7SWP(N, Q(1,K), Q(1,J))                       
                   IF (K .LE. 1) GO TO 60                               
                        I = I - J + 1                                   
                        J = KK - K + 1                                  
                        CALL DV7SWP(K-1, R(I), R(J))                    
C                                                                       
C        ***  COLUMN K OF Q SHOULD BE NEARLY ORTHOGONAL TO THE PREVIOUS 
C        ***  COLUMNS.  NORMALIZE IT, TEST FOR SINGULARITY, AND DECIDE  
C        ***  WHETHER TO REORTHOGONALIZE IT.                            
C                                                                       
 60      AK = R(KK)                                                     
         IF (AK .LE. ZERO) GO TO 140                                    
         WK = W(K)                                                      
C                                                                       
C        *** SET T TO THE NORM OF (Q(K,K),...,Q(N,K))                   
C        *** AND CHECK FOR SINGULARITY.                                 
C                                                                       
         IF (WK .LT. WTOL) GO TO 70                                     
            T = DV2NRM(NK1, Q(K,K))                                     
            IF (T / AK .LE. SINGTL) GO TO 140                           
            GO TO 80                                                    
 70      T = DSQRT(ONE - WK)                                            
         IF (T .LE. SINGTL) GO TO 140                                   
         T = T * AK                                                     
C                                                                       
C        *** DETERMINE HOUSEHOLDER TRANSFORMATION ***                   
C                                                                       
 80      QKK = Q(K,K)                                                   
         IF (T .LE. TINYRT) GO TO 90                                    
         IF (T .GE. BIGRT) GO TO 90                                     
            IF (QKK .LT. ZERO) T = -T                                   
            QKK = QKK + T                                               
            S = DSQRT(T * QKK)                                          
            GO TO 110                                                   
 90       S = DSQRT(T)                                                  
          IF (QKK .LT. ZERO) GO TO 100                                  
             QKK = QKK + T                                              
             S = S * DSQRT(QKK)                                         
             GO TO 110                                                  
 100      T = -T                                                        
          QKK = QKK + T                                                 
          S = S * DSQRT(-QKK)                                           
 110      Q(K,K) = QKK                                                  
C                                                                       
C         ***  SCALE (Q(K,K),...,Q(N,K)) TO HAVE NORM SQRT(2)  ***      
C                                                                       
          IF (S .LE. TINY) GO TO 140                                    
          CALL DV7SCL(NK1, Q(K,K), ONE/S, Q(K,K))                       
C                                                                       
          R(KK) = -T                                                    
C                                                                       
C        ***  COMPUTE R(K,I) FOR I = K+1,...,P AND UPDATE Q  ***        
C                                                                       
         IF (K .GE. P) GO TO 999                                        
         J = KK + K                                                     
         II = KK                                                        
         DO 120 I = KP1, P                                              
              II = II + I                                               
              CALL DV2AXY(NK1, Q(K,I), -DD7TPR(NK1,Q(K,K),Q(K,I)),      
     1                   Q(K,K), Q(K,I))                                
              T = Q(K,I)                                                
              R(J) = T                                                  
              J = J + I                                                 
              T1 = R(II)                                                
              IF (T1 .GT. ZERO)  W(I) = W(I) + (T/T1)**2                
 120          CONTINUE                                                  
 130     CONTINUE                                                       
C                                                                       
C  ***  SINGULAR Q  ***                                                 
C                                                                       
 140  IERR = K                                                          
      KM1 = K - 1                                                       
      J = KK                                                            
      DO 150 I = K, P                                                   
         CALL DV7SCP(I-KM1, R(J), ZERO)                                 
         J = J + I                                                      
 150     CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DQ7RFH FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DV2AXY(P, W, A, X, Y)                                  
C                                                                       
C  ***  SET W = A*X + Y  --  W, X, Y = P-VECTORS, A = SCALAR  ***       
C                                                                       
      INTEGER P                                                         
      DOUBLE PRECISION A, W(P), X(P), Y(P)                              
C                                                                       
      INTEGER I                                                         
C                                                                       
      DO 10 I = 1, P                                                    
 10      W(I) = A*X(I) + Y(I)                                           
      RETURN                                                            
      END                                                               
      SUBROUTINE DR7TVM(N, P, Y, D, U, X)                               
C                                                                       
C  ***  SET Y TO R*X, WHERE R IS THE UPPER TRIANGULAR MATRIX WHOSE      
C  ***  DIAGONAL IS IN D AND WHOSE STRICT UPPER TRIANGLE IS IN U.       
C                                                                       
C  ***  X AND Y MAY SHARE STORAGE.                                      
C                                                                       
      INTEGER N, P                                                      
      DOUBLE PRECISION Y(P), D(P), U(N,P), X(P)                         
C                                                                       
      DOUBLE PRECISION DD7TPR                                           
      EXTERNAL DD7TPR                                                   
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, II, PL, PP1                                            
      DOUBLE PRECISION T                                                
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
      PL = MIN0(N-1, P)                                                 
      PP1 = PL + 1                                                      
      DO 10 II = 1, PL                                                  
         I = PP1 - II                                                   
         T = X(I) * D(I)                                                
         IF (I .GT. 1) T = T + DD7TPR(I-1, U(1,I), X)                   
         Y(I) = T                                                       
 10      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DR7TVM FOLLOWS  ***                                
      END                                                               
      DOUBLE PRECISION FUNCTION DRLDST(P, D, X, X0)                     
C                                                                       
C  ***  COMPUTE AND RETURN RELATIVE DIFFERENCE BETWEEN X AND X0  ***    
C  ***  NL2SOL VERSION 2.2  ***                                         
C                                                                       
      INTEGER P                                                         
      DOUBLE PRECISION D(P), X(P), X0(P)                                
C                                                                       
      INTEGER I                                                         
      DOUBLE PRECISION EMAX, T, XMAX, ZERO                              
C/6                                                                     
C     DATA ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.D+0)                                            
C/                                                                      
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
      EMAX = ZERO                                                       
      XMAX = ZERO                                                       
      DO 10 I = 1, P                                                    
         T = DABS(D(I) * (X(I) - X0(I)))                                
         IF (EMAX .LT. T) EMAX = T                                      
         T = D(I) * (DABS(X(I)) + DABS(X0(I)))                          
         IF (XMAX .LT. T) XMAX = T                                      
 10      CONTINUE                                                       
      DRLDST = ZERO                                                     
      IF (XMAX .GT. ZERO) DRLDST = EMAX / XMAX                          
 999  RETURN                                                            
C  ***  LAST CARD OF DRLDST FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DQ7RSH(K, P, HAVQTR, QTR, R, W)                        
C                                                                       
C  ***  PERMUTE COLUMN K OF R TO COLUMN P, MODIFY QTR ACCORDINGLY  ***  
C                                                                       
      LOGICAL HAVQTR                                                    
      INTEGER K, P                                                      
      DOUBLE PRECISION QTR(P), R(1), W(P)                               
C     DIMSNSION R(P*(P+1)/2)                                            
C                                                                       
      DOUBLE PRECISION DH2RFG                                           
      EXTERNAL DH2RFA, DH2RFG,DV7CPY                                    
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, I1, J, JM1, JP1, J1, KM1, K1, PM1                      
      DOUBLE PRECISION A, B, T, WJ, X, Y, Z, ZERO                       
C                                                                       
      DATA ZERO/0.0D+0/                                                 
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      IF (K .GE. P) GO TO 999                                           
      KM1 = K - 1                                                       
      K1 = K * KM1 / 2                                                  
      CALL DV7CPY(K, W, R(K1+1))                                        
      WJ = W(K)                                                         
      PM1 = P - 1                                                       
      J1 = K1 + KM1                                                     
      DO 50 J = K, PM1                                                  
         JM1 = J - 1                                                    
         JP1 = J + 1                                                    
         IF (JM1 .GT. 0) CALL DV7CPY(JM1, R(K1+1), R(J1+2))             
         J1 = J1 + JP1                                                  
         K1 = K1 + J                                                    
         A = R(J1)                                                      
         B = R(J1+1)                                                    
         IF (B .NE. ZERO) GO TO 10                                      
              R(K1) = A                                                 
              X = ZERO                                                  
              Z = ZERO                                                  
              GO TO 40                                                  
 10      R(K1) = DH2RFG(A, B, X, Y, Z)                                  
         IF (J .EQ. PM1) GO TO 30                                       
         I1 = J1                                                        
         DO 20 I = JP1, PM1                                             
              I1 = I1 + I                                               
              CALL DH2RFA(1, R(I1), R(I1+1), X, Y, Z)                   
 20           CONTINUE                                                  
 30      IF (HAVQTR) CALL DH2RFA(1, QTR(J), QTR(JP1), X, Y, Z)          
 40      T = X * WJ                                                     
         W(J) = WJ + T                                                  
         WJ = T * Z                                                     
 50      CONTINUE                                                       
      W(P) = WJ                                                         
      CALL DV7CPY(P, R(K1+1), W)                                        
 999  RETURN                                                            
      END                                                               
      SUBROUTINE DL7VML(N, X, L, Y)                                     
C                                                                       
C  ***  COMPUTE  X = L*Y, WHERE  L  IS AN  N X N  LOWER TRIANGULAR      
C  ***  MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY OCCUPY THE SAME   
C  ***  STORAGE.  ***                                                   
C                                                                       
      INTEGER N                                                         
      DOUBLE PRECISION X(N), L(1), Y(N)                                 
C     DIMENSION L(N*(N+1)/2)                                            
      INTEGER I, II, IJ, I0, J, NP1                                     
      DOUBLE PRECISION T, ZERO                                          
C/6                                                                     
C     DATA ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.D+0)                                            
C/                                                                      
C                                                                       
      NP1 = N + 1                                                       
      I0 = N*(N+1)/2                                                    
      DO 20 II = 1, N                                                   
         I = NP1 - II                                                   
         I0 = I0 - I                                                    
         T = ZERO                                                       
         DO 10 J = 1, I                                                 
              IJ = I0 + J                                               
              T = T + L(IJ)*Y(J)                                        
 10           CONTINUE                                                  
         X(I) = T                                                       
 20      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DL7VML FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DL7TVM(N, X, L, Y)                                     
C                                                                       
C  ***  COMPUTE  X = (L**T)*Y, WHERE  L  IS AN  N X N  LOWER            
C  ***  TRIANGULAR MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY        
C  ***  OCCUPY THE SAME STORAGE.  ***                                   
C                                                                       
      INTEGER N                                                         
      DOUBLE PRECISION X(N), L(1), Y(N)                                 
C     DIMENSION L(N*(N+1)/2)                                            
      INTEGER I, IJ, I0, J                                              
      DOUBLE PRECISION YI, ZERO                                         
C/6                                                                     
C     DATA ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.D+0)                                            
C/                                                                      
C                                                                       
      I0 = 0                                                            
      DO 20 I = 1, N                                                    
         YI = Y(I)                                                      
         X(I) = ZERO                                                    
         DO 10 J = 1, I                                                 
              IJ = I0 + J                                               
              X(J) = X(J) + YI*L(IJ)                                    
 10           CONTINUE                                                  
         I0 = I0 + I                                                    
 20      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DL7TVM FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DL7ITV(N, X, L, Y)                                     
C                                                                       
C  ***  SOLVE  (L**T)*X = Y,  WHERE  L  IS AN  N X N  LOWER TRIANGULAR  
C  ***  MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY OCCUPY THE SAME   
C  ***  STORAGE.  ***                                                   
C                                                                       
      INTEGER N                                                         
      DOUBLE PRECISION X(N), L(1), Y(N)                                 
      INTEGER I, II, IJ, IM1, I0, J, NP1                                
      DOUBLE PRECISION XI, ZERO                                         
C/6                                                                     
C     DATA ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.D+0)                                            
C/                                                                      
C                                                                       
      DO 10 I = 1, N                                                    
 10      X(I) = Y(I)                                                    
      NP1 = N + 1                                                       
      I0 = N*(N+1)/2                                                    
      DO 30 II = 1, N                                                   
         I = NP1 - II                                                   
         XI = X(I)/L(I0)                                                
         X(I) = XI                                                      
         IF (I .LE. 1) GO TO 999                                        
         I0 = I0 - I                                                    
         IF (XI .EQ. ZERO) GO TO 30                                     
         IM1 = I - 1                                                    
         DO 20 J = 1, IM1                                               
              IJ = I0 + J                                               
              X(J) = X(J) - XI*L(IJ)                                    
 20           CONTINUE                                                  
 30      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DL7ITV FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DS7DMP(N, X, Y, Z, K)                                  
C                                                                       
C ***  SET X = DIAG(Z)**K * Y * DIAG(Z)**K                              
C ***  FOR X, Y = COMPACTLY STORED LOWER TRIANG. MATRICES               
C ***  K = 1 OR -1.                                                     
C                                                                       
      INTEGER N, K                                                      
C/6S                                                                    
C     DOUBLE PRECISION X(1), Y(N), Z(1)                                 
C/7S                                                                    
      DOUBLE PRECISION X(*), Y(N), Z(*)                                 
C/                                                                      
      INTEGER I, J, L                                                   
      DOUBLE PRECISION ONE, T                                           
      DATA ONE/1.D+0/                                                   
C                                                                       
      L = 1                                                             
      IF (K .GE. 0) GO TO 30                                            
      DO 20 I = 1, N                                                    
         T = ONE / Z(I)                                                 
         DO 10 J = 1, I                                                 
            X(L) = T * Y(L) / Z(J)                                      
            L = L + 1                                                   
 10         CONTINUE                                                    
 20      CONTINUE                                                       
      GO TO 999                                                         
C                                                                       
 30   DO 50 I = 1, N                                                    
         T = Z(I)                                                       
         DO 40 J = 1, I                                                 
            X(L) = T * Y(L) * Z(J)                                      
            L = L + 1                                                   
 40         CONTINUE                                                    
 50      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DS7DMP FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DS7LUP(A, COSMIN, P, SIZE, STEP, U, W, WCHMTD, WSCALE, 
     1                  Y)                                              
C                                                                       
C  ***  UPDATE SYMMETRIC  A  SO THAT  A * STEP = Y  ***                 
C  ***  (LOWER TRIANGLE OF  A  STORED ROWWISE       ***                 
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER P                                                         
      DOUBLE PRECISION A(1), COSMIN, SIZE, STEP(P), U(P), W(P),         
     1                 WCHMTD(P), WSCALE, Y(P)                          
C     DIMENSION A(P*(P+1)/2)                                            
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, J, K                                                   
      DOUBLE PRECISION DENMIN, SDOTWM, T, UI, WI                        
C                                                                       
C     ***  CONSTANTS  ***                                               
      DOUBLE PRECISION HALF, ONE, ZERO                                  
C                                                                       
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      DOUBLE PRECISION DD7TPR, DV2NRM                                   
      EXTERNAL DD7TPR, DS7LVM, DV2NRM                                   
C                                                                       
C/6                                                                     
C     DATA HALF/0.5D+0/, ONE/1.D+0/, ZERO/0.D+0/                        
C/7                                                                     
      PARAMETER (HALF=0.5D+0, ONE=1.D+0, ZERO=0.D+0)                    
C/                                                                      
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
      SDOTWM = DD7TPR(P, STEP, WCHMTD)                                  
      DENMIN = COSMIN * DV2NRM(P,STEP) * DV2NRM(P,WCHMTD)               
      WSCALE = ONE                                                      
      IF (DENMIN .NE. ZERO) WSCALE = DMIN1(ONE, DABS(SDOTWM/DENMIN))    
      T = ZERO                                                          
      IF (SDOTWM .NE. ZERO) T = WSCALE / SDOTWM                         
      DO 10 I = 1, P                                                    
 10      W(I) = T * WCHMTD(I)                                           
      CALL DS7LVM(P, U, A, STEP)                                        
      T = HALF * (SIZE * DD7TPR(P, STEP, U)  -  DD7TPR(P, STEP, Y))     
      DO 20 I = 1, P                                                    
 20      U(I) = T*W(I) + Y(I) - SIZE*U(I)                               
C                                                                       
C  ***  SET  A = A + U*(W**T) + W*(U**T)  ***                           
C                                                                       
      K = 1                                                             
      DO 40 I = 1, P                                                    
         UI = U(I)                                                      
         WI = W(I)                                                      
         DO 30 J = 1, I                                                 
              A(K) = SIZE*A(K) + UI*W(J) + WI*U(J)                      
              K = K + 1                                                 
 30           CONTINUE                                                  
 40      CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DS7LUP FOLLOWS  ***                                
      END                                                               
      DOUBLE PRECISION FUNCTION DV2NRM(P, X)                            
C                                                                       
C  ***  RETURN THE 2-NORM OF THE P-VECTOR X, TAKING  ***                
C  ***  CARE TO AVOID THE MOST LIKELY UNDERFLOWS.    ***                
C                                                                       
      INTEGER P                                                         
      DOUBLE PRECISION X(P)                                             
C                                                                       
      INTEGER I, J                                                      
      DOUBLE PRECISION ONE, R, SCALE, SQTETA, T, XI, ZERO               
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
      DOUBLE PRECISION DR7MDC                                           
      EXTERNAL DR7MDC                                                   
C                                                                       
C/6                                                                     
C     DATA ONE/1.D+0/, ZERO/0.D+0/                                      
C/7                                                                     
      PARAMETER (ONE=1.D+0, ZERO=0.D+0)                                 
      SAVE SQTETA                                                       
C/                                                                      
      DATA SQTETA/0.D+0/                                                
C                                                                       
      IF (P .GT. 0) GO TO 10                                            
         DV2NRM = ZERO                                                  
         GO TO 999                                                      
 10   DO 20 I = 1, P                                                    
         IF (X(I) .NE. ZERO) GO TO 30                                   
 20      CONTINUE                                                       
      DV2NRM = ZERO                                                     
      GO TO 999                                                         
C                                                                       
 30   SCALE = DABS(X(I))                                                
      IF (I .LT. P) GO TO 40                                            
         DV2NRM = SCALE                                                 
         GO TO 999                                                      
 40   T = ONE                                                           
      IF (SQTETA .EQ. ZERO) SQTETA = DR7MDC(2)                          
C                                                                       
C     ***  SQTETA IS (SLIGHTLY LARGER THAN) THE SQUARE ROOT OF THE      
C     ***  SMALLEST POSITIVE FLOATING POINT NUMBER ON THE MACHINE.      
C     ***  THE TESTS INVOLVING SQTETA ARE DONE TO PREVENT UNDERFLOWS.   
C                                                                       
      J = I + 1                                                         
      DO 60 I = J, P                                                    
         XI = DABS(X(I))                                                
         IF (XI .GT. SCALE) GO TO 50                                    
              R = XI / SCALE                                            
              IF (R .GT. SQTETA) T = T + R*R                            
              GO TO 60                                                  
 50           R = SCALE / XI                                            
              IF (R .LE. SQTETA) R = ZERO                               
              T = ONE  +  T * R*R                                       
              SCALE = XI                                                
 60      CONTINUE                                                       
C                                                                       
      DV2NRM = SCALE * DSQRT(T)                                         
 999  RETURN                                                            
C  ***  LAST LINE OF DV2NRM FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DL7IVM(N, X, L, Y)                                     
C                                                                       
C  ***  SOLVE  L*X = Y, WHERE  L  IS AN  N X N  LOWER TRIANGULAR        
C  ***  MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY OCCUPY THE SAME   
C  ***  STORAGE.  ***                                                   
C                                                                       
      INTEGER N                                                         
      DOUBLE PRECISION X(N), L(1), Y(N)                                 
      DOUBLE PRECISION DD7TPR                                           
      EXTERNAL DD7TPR                                                   
      INTEGER I, J, K                                                   
      DOUBLE PRECISION T, ZERO                                          
C/6                                                                     
C     DATA ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.D+0)                                            
C/                                                                      
C                                                                       
      DO 10 K = 1, N                                                    
         IF (Y(K) .NE. ZERO) GO TO 20                                   
         X(K) = ZERO                                                    
 10      CONTINUE                                                       
      GO TO 999                                                         
 20   J = K*(K+1)/2                                                     
      X(K) = Y(K) / L(J)                                                
      IF (K .GE. N) GO TO 999                                           
      K = K + 1                                                         
      DO 30 I = K, N                                                    
         T = DD7TPR(I-1, L(J+1), X)                                     
         J = J + I                                                      
         X(I) = (Y(I) - T)/L(J)                                         
 30      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DL7IVM FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DS7LVM(P, Y, S, X)                                     
C                                                                       
C  ***  SET  Y = S * X,  S = P X P SYMMETRIC MATRIX.  ***               
C  ***  LOWER TRIANGLE OF  S  STORED ROWWISE.         ***               
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER P                                                         
      DOUBLE PRECISION S(1), X(P), Y(P)                                 
C     DIMENSION S(P*(P+1)/2)                                            
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, IM1, J, K                                              
      DOUBLE PRECISION XI                                               
C                                                                       
C  ***  NO INTRINSIC FUNCTIONS  ***                                     
C                                                                       
C  ***  EXTERNAL FUNCTION  ***                                          
C                                                                       
      DOUBLE PRECISION DD7TPR                                           
      EXTERNAL DD7TPR                                                   
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
      J = 1                                                             
      DO 10 I = 1, P                                                    
         Y(I) = DD7TPR(I, S(J), X)                                      
         J = J + I                                                      
 10      CONTINUE                                                       
C                                                                       
      IF (P .LE. 1) GO TO 999                                           
      J = 1                                                             
      DO 40 I = 2, P                                                    
         XI = X(I)                                                      
         IM1 = I - 1                                                    
         J = J + 1                                                      
         DO 30 K = 1, IM1                                               
              Y(K) = Y(K) + S(J)*XI                                     
              J = J + 1                                                 
 30           CONTINUE                                                  
 40      CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DS7LVM FOLLOWS  ***                                
      END                                                               
      DOUBLE PRECISION FUNCTION DD7TPR(P, X, Y)                         
C                                                                       
C  ***  RETURN THE INNER PRODUCT OF THE P-VECTORS X AND Y.  ***         
C                                                                       
      INTEGER P                                                         
      DOUBLE PRECISION X(P), Y(P)                                       
C                                                                       
      INTEGER I                                                         
      DOUBLE PRECISION ONE, SQTETA, T, ZERO                             
      DOUBLE PRECISION DR7MDC                                           
      EXTERNAL DR7MDC                                                   
C                                                                       
C  ***  DR7MDC(2) RETURNS A MACHINE-DEPENDENT CONSTANT, SQTETA, WHICH   
C  ***  IS SLIGHTLY LARGER THAN THE SMALLEST POSITIVE NUMBER THAT       
C  ***  CAN BE SQUARED WITHOUT UNDERFLOWING.                            
C                                                                       
C/6                                                                     
C     DATA ONE/1.D+0/, SQTETA/0.D+0/, ZERO/0.D+0/                       
C/7                                                                     
      PARAMETER (ONE=1.D+0, ZERO=0.D+0)                                 
      DATA SQTETA/0.D+0/                                                
C/                                                                      
C                                                                       
      DD7TPR = ZERO                                                     
      IF (P .LE. 0) GO TO 999                                           
      IF (SQTETA .EQ. ZERO) SQTETA = DR7MDC(2)                          
      DO 20 I = 1, P                                                    
         T = DMAX1(DABS(X(I)), DABS(Y(I)))                              
         IF (T .GT. ONE) GO TO 10                                       
         IF (T .LT. SQTETA) GO TO 20                                    
         T = (X(I)/SQTETA)*Y(I)                                         
         IF (DABS(T) .LT. SQTETA) GO TO 20                              
 10      DD7TPR = DD7TPR + X(I)*Y(I)                                    
 20   CONTINUE                                                          
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DD7TPR FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DV7DFL(ALG, LV, V)                                     
C                                                                       
C  ***  SUPPLY ***SOL (VERSION 2.3) DEFAULT VALUES TO V  ***            
C                                                                       
C  ***  ALG = 1 MEANS REGRESSION CONSTANTS.                             
C  ***  ALG = 2 MEANS GENERAL UNCONSTRAINED OPTIMIZATION CONSTANTS.     
C                                                                       
      INTEGER ALG, LV                                                   
      DOUBLE PRECISION V(LV)                                            
C                                                                       
      DOUBLE PRECISION DR7MDC                                           
      EXTERNAL DR7MDC                                                   
C DR7MDC... RETURNS MACHINE-DEPENDENT CONSTANTS                         
C                                                                       
      DOUBLE PRECISION MACHEP, MEPCRT, ONE, SQTEPS, THREE               
C                                                                       
C  ***  SUBSCRIPTS FOR V  ***                                           
C                                                                       
      INTEGER AFCTOL, BIAS, COSMIN, DECFAC, DELTA0, DFAC, DINIT, DLTFDC,
     1        DLTFDJ, DTINIT, D0INIT, EPSLON, ETA0, FUZZ, HUBERC,       
     2        INCFAC, LMAX0, LMAXS, PHMNFC, PHMXFC, RDFCMN, RDFCMX,     
     3        RFCTOL, RLIMIT, RSPTOL, SCTOL, SIGMIN, TUNER1, TUNER2,    
     4        TUNER3, TUNER4, TUNER5, XCTOL, XFTOL                      
C                                                                       
C/6                                                                     
C     DATA ONE/1.D+0/, THREE/3.D+0/                                     
C/7                                                                     
      PARAMETER (ONE=1.D+0, THREE=3.D+0)                                
C/                                                                      
C                                                                       
C  ***  V SUBSCRIPT VALUES  ***                                         
C                                                                       
C/6                                                                     
C     DATA AFCTOL/31/, BIAS/43/, COSMIN/47/, DECFAC/22/, DELTA0/44/,    
C    1     DFAC/41/, DINIT/38/, DLTFDC/42/, DLTFDJ/43/, DTINIT/39/,     
C    2     D0INIT/40/, EPSLON/19/, ETA0/42/, FUZZ/45/, HUBERC/48/,      
C    3     INCFAC/23/, LMAX0/35/, LMAXS/36/, PHMNFC/20/, PHMXFC/21/,    
C    4     RDFCMN/24/, RDFCMX/25/, RFCTOL/32/, RLIMIT/46/, RSPTOL/49/,  
C    5     SCTOL/37/, SIGMIN/50/, TUNER1/26/, TUNER2/27/, TUNER3/28/,   
C    6     TUNER4/29/, TUNER5/30/, XCTOL/33/, XFTOL/34/                 
C/7                                                                     
      PARAMETER (AFCTOL=31, BIAS=43, COSMIN=47, DECFAC=22, DELTA0=44,   
     1           DFAC=41, DINIT=38, DLTFDC=42, DLTFDJ=43, DTINIT=39,    
     2           D0INIT=40, EPSLON=19, ETA0=42, FUZZ=45, HUBERC=48,     
     3           INCFAC=23, LMAX0=35, LMAXS=36, PHMNFC=20, PHMXFC=21,   
     4           RDFCMN=24, RDFCMX=25, RFCTOL=32, RLIMIT=46, RSPTOL=49, 
     5           SCTOL=37, SIGMIN=50, TUNER1=26, TUNER2=27, TUNER3=28,  
     6           TUNER4=29, TUNER5=30, XCTOL=33, XFTOL=34)              
C/                                                                      
C                                                                       
C-------------------------------  BODY  --------------------------------
C                                                                       
      MACHEP = DR7MDC(3)                                                
      V(AFCTOL) = 1.D-20                                                
      IF (MACHEP .GT. 1.D-10) V(AFCTOL) = MACHEP**2                     
      V(DECFAC) = 0.5D+0                                                
      SQTEPS = DR7MDC(4)                                                
      V(DFAC) = 0.6D+0                                                  
      V(DTINIT) = 1.D-6                                                 
      MEPCRT = MACHEP ** (ONE/THREE)                                    
      V(D0INIT) = 1.D+0                                                 
      V(EPSLON) = 0.1D+0                                                
      V(INCFAC) = 2.D+0                                                 
      V(LMAX0) = 1.D+0                                                  
      V(LMAXS) = 1.D+0                                                  
      V(PHMNFC) = -0.1D+0                                               
      V(PHMXFC) = 0.1D+0                                                
      V(RDFCMN) = 0.1D+0                                                
      V(RDFCMX) = 4.D+0                                                 
      V(RFCTOL) = DMAX1(1.D-10, MEPCRT**2)                              
      V(SCTOL) = V(RFCTOL)                                              
      V(TUNER1) = 0.1D+0                                                
      V(TUNER2) = 1.D-4                                                 
      V(TUNER3) = 0.75D+0                                               
      V(TUNER4) = 0.5D+0                                                
      V(TUNER5) = 0.75D+0                                               
      V(XCTOL) = SQTEPS                                                 
      V(XFTOL) = 1.D+2 * MACHEP                                         
C                                                                       
      IF (ALG .GE. 2) GO TO 10                                          
C                                                                       
C  ***  REGRESSION  VALUES                                              
C                                                                       
      V(COSMIN) = DMAX1(1.D-6, 1.D+2 * MACHEP)                          
      V(DINIT) = 0.D+0                                                  
      V(DELTA0) = SQTEPS                                                
      V(DLTFDC) = MEPCRT                                                
      V(DLTFDJ) = SQTEPS                                                
      V(FUZZ) = 1.5D+0                                                  
      V(HUBERC) = 0.7D+0                                                
      V(RLIMIT) = DR7MDC(5)                                             
      V(RSPTOL) = 1.D-3                                                 
      V(SIGMIN) = 1.D-4                                                 
      GO TO 999                                                         
C                                                                       
C  ***  GENERAL OPTIMIZATION VALUES                                     
C                                                                       
 10   V(BIAS) = 0.8D+0                                                  
      V(DINIT) = -1.0D+0                                                
      V(ETA0) = 1.0D+3 * MACHEP                                         
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DV7DFL FOLLOWS  ***                                
      END                                                               
      DOUBLE PRECISION FUNCTION DR7MDC(K)                               
C                                                                       
C  ***  RETURN MACHINE DEPENDENT CONSTANTS USED BY NL2SOL  ***          
C                                                                       
      INTEGER K                                                         
C                                                                       
C  ***  THE CONSTANT RETURNED DEPENDS ON K...                           
C                                                                       
C  ***        K = 1... SMALLEST POS. ETA SUCH THAT -ETA EXISTS.         
C  ***        K = 2... SQUARE ROOT OF ETA.                              
C  ***        K = 3... UNIT ROUNDOFF = SMALLEST POS. NO. MACHEP SUCH    
C  ***                 THAT 1 + MACHEP .GT. 1 .AND. 1 - MACHEP .LT. 1.  
C  ***        K = 4... SQUARE ROOT OF MACHEP.                           
C  ***        K = 5... SQUARE ROOT OF BIG (SEE K = 6).                  
C  ***        K = 6... LARGEST MACHINE NO. BIG SUCH THAT -BIG EXISTS.   
C                                                                       
      DOUBLE PRECISION BIG, ETA, MACHEP                                 
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
C                                                                       
      DOUBLE PRECISION D1MACH, ZERO                                     
      EXTERNAL D1MACH                                                   
      DATA BIG/0.D+0/, ETA/0.D+0/, MACHEP/0.D+0/, ZERO/0.D+0/           
      IF (BIG .GT. ZERO) GO TO 1                                        
         BIG = D1MACH(2)                                                
         ETA = D1MACH(1)                                                
         MACHEP = D1MACH(4)                                             
 1    CONTINUE                                                          
C                                                                       
C-------------------------------  BODY  --------------------------------
C                                                                       
      GO TO (10, 20, 30, 40, 50, 60), K                                 
C                                                                       
 10   DR7MDC = ETA                                                      
      GO TO 999                                                         
C                                                                       
 20   DR7MDC = DSQRT(256.D+0*ETA)/16.D+0                                
      GO TO 999                                                         
C                                                                       
 30   DR7MDC = MACHEP                                                   
      GO TO 999                                                         
C                                                                       
 40   DR7MDC = DSQRT(MACHEP)                                            
      GO TO 999                                                         
C                                                                       
 50   DR7MDC = DSQRT(BIG/256.D+0)*16.D+0                                
      GO TO 999                                                         
C                                                                       
 60   DR7MDC = BIG                                                      
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DR7MDC FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DV7SHF(N, K, X)                                        
C                                                                       
C  ***  SHIFT X(K),...,X(N) LEFT CIRCULARLY ONE POSITION  ***           
C                                                                       
      INTEGER N, K                                                      
      DOUBLE PRECISION X(N)                                             
C                                                                       
      INTEGER I, NM1                                                    
      DOUBLE PRECISION T                                                
C                                                                       
      IF (K .GE. N) GO TO 999                                           
      NM1 = N - 1                                                       
      T = X(K)                                                          
      DO 10 I = K, NM1                                                  
 10      X(I) = X(I+1)                                                  
      X(N) = T                                                          
 999  RETURN                                                            
      END                                                               
      SUBROUTINE DV7SWP(N, X, Y)                                        
C                                                                       
C  ***  INTERCHANGE N-VECTORS X AND Y.  ***                             
C                                                                       
      INTEGER N                                                         
      DOUBLE PRECISION X(N), Y(N)                                       
C                                                                       
      INTEGER I                                                         
      DOUBLE PRECISION T                                                
C                                                                       
      DO 10 I = 1, N                                                    
         T = X(I)                                                       
         X(I) = Y(I)                                                    
         Y(I) = T                                                       
 10      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DV7SWP FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DF7DHB(B, D, G, IRT, IV, LIV, LV, P, V, X)             
C                                                                       
C  ***  COMPUTE FINITE-DIFFERENCE HESSIAN, STORE IT IN V STARTING       
C  ***  AT V(IV(FDH)) = V(-IV(H)).  HONOR SIMPLE BOUNDS IN B.           
C                                                                       
C  ***  IF IV(COVREQ) .GE. 0 THEN DF7DHB USES GRADIENT DIFFERENCES,     
C  ***  OTHERWISE FUNCTION DIFFERENCES.  STORAGE IN V IS AS IN DG7LIT.  
C                                                                       
C IRT VALUES...                                                         
C     1 = COMPUTE FUNCTION VALUE, I.E., V(F).                           
C     2 = COMPUTE G.                                                    
C     3 = DONE.                                                         
C                                                                       
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER IRT, LIV, LV, P                                           
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION B(2,P), D(P), G(P), V(LV), X(P)                  
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      LOGICAL OFFSID                                                    
      INTEGER GSAVE1, HES, HMI, HPI, HPM, I, K, KIND, L, M, MM1, MM1O2, 
     1        NEWM1, PP1O2, STPI, STPM, STP0                            
      DOUBLE PRECISION DEL, DEL0, T, XM, XM1                            
      DOUBLE PRECISION HALF, HLIM, ONE, TWO, ZERO                       
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      EXTERNAL DV7CPY, DV7SCP                                           
C                                                                       
C DV7CPY.... COPY ONE VECTOR TO ANOTHER.                                
C DV7SCP... COPY SCALAR TO ALL COMPONENTS OF A VECTOR.                  
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER COVREQ, DELTA, DELTA0, DLTFDC, F, FDH, FX, H, KAGQT, MODE,
     1        NFGCAL, SAVEI, SWITCH, TOOBIG, W, XMSAVE                  
C                                                                       
C/6                                                                     
C     DATA HALF/0.5D+0/, HLIM/0.1D+0/, ONE/1.D+0/, TWO/2.D+0/,          
C    1     ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (HALF=0.5D+0, HLIM=0.1D+0, ONE=1.D+0, TWO=2.D+0,        
     1           ZERO=0.D+0)                                            
C/                                                                      
C                                                                       
C/6                                                                     
C     DATA COVREQ/15/, DELTA/52/, DELTA0/44/, DLTFDC/42/, F/10/,        
C    1     FDH/74/, FX/53/, H/56/, KAGQT/33/, MODE/35/, NFGCAL/7/,      
C    2     SAVEI/63/, SWITCH/12/, TOOBIG/2/, W/65/, XMSAVE/51/          
C/7                                                                     
      PARAMETER (COVREQ=15, DELTA=52, DELTA0=44, DLTFDC=42, F=10,       
     1           FDH=74, FX=53, H=56, KAGQT=33, MODE=35, NFGCAL=7,      
     2           SAVEI=63, SWITCH=12, TOOBIG=2, W=65, XMSAVE=51)        
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      IRT = 4                                                           
      KIND = IV(COVREQ)                                                 
      M = IV(MODE)                                                      
      IF (M .GT. 0) GO TO 10                                            
         HES = IABS(IV(H))                                              
         IV(H) = -HES                                                   
         IV(FDH) = 0                                                    
         IV(KAGQT) = -1                                                 
         V(FX) = V(F)                                                   
C        *** SUPPLY ZEROS IN CASE B(1,I) = B(2,I) FOR SOME I ***        
         CALL DV7SCP(P*(P+1)/2, V(HES), ZERO)                           
 10   IF (M .GT. P) GO TO 999                                           
      IF (KIND .LT. 0) GO TO 120                                        
C                                                                       
C  ***  COMPUTE FINITE-DIFFERENCE HESSIAN USING BOTH FUNCTION AND       
C  ***  GRADIENT VALUES.                                                
C                                                                       
      GSAVE1 = IV(W) + P                                                
      IF (M .GT. 0) GO TO 20                                            
C        ***  FIRST CALL ON DF7DHB.  SET GSAVE = G, TAKE FIRST STEP  ***
         CALL DV7CPY(P, V(GSAVE1), G)                                   
         IV(SWITCH) = IV(NFGCAL)                                        
         GO TO 80                                                       
C                                                                       
 20   DEL = V(DELTA)                                                    
      X(M) = V(XMSAVE)                                                  
      IF (IV(TOOBIG) .EQ. 0) GO TO 30                                   
C                                                                       
C     ***  HANDLE OVERSIZE V(DELTA)  ***                                
C                                                                       
         DEL0 = V(DELTA0) * DMAX1(ONE/D(M), DABS(X(M)))                 
         DEL = HALF * DEL                                               
         IF (DABS(DEL/DEL0) .LE. HLIM) GO TO 140                        
C                                                                       
 30   HES = -IV(H)                                                      
C                                                                       
C  ***  SET  G = (G - GSAVE)/DEL  ***                                   
C                                                                       
      DEL = ONE / DEL                                                   
      DO 40 I = 1, P                                                    
         G(I) = DEL * (G(I) - V(GSAVE1))                                
         GSAVE1 = GSAVE1 + 1                                            
 40      CONTINUE                                                       
C                                                                       
C  ***  ADD G AS NEW COL. TO FINITE-DIFF. HESSIAN MATRIX  ***           
C                                                                       
      K = HES + M*(M-1)/2                                               
      L = K + M - 2                                                     
      IF (M .EQ. 1) GO TO 60                                            
C                                                                       
C  ***  SET  H(I,M) = 0.5 * (H(I,M) + G(I))  FOR I = 1 TO M-1  ***      
C                                                                       
      MM1 = M - 1                                                       
      DO 50 I = 1, MM1                                                  
         IF (B(1,I) .LT. B(2,I)) V(K) = HALF * (V(K) + G(I))            
         K = K + 1                                                      
 50      CONTINUE                                                       
C                                                                       
C  ***  ADD  H(I,M) = G(I)  FOR I = M TO P  ***                         
C                                                                       
 60   L = L + 1                                                         
      DO 70 I = M, P                                                    
         IF (B(1,I) .LT. B(2,I)) V(L) = G(I)                            
         L = L + I                                                      
 70      CONTINUE                                                       
C                                                                       
 80   M = M + 1                                                         
      IV(MODE) = M                                                      
      IF (M .GT. P) GO TO 340                                           
      IF (B(1,M) .GE. B(2,M)) GO TO 80                                  
C                                                                       
C  ***  CHOOSE NEXT FINITE-DIFFERENCE STEP, RETURN TO GET G THERE  ***  
C                                                                       
      DEL = V(DELTA0) * DMAX1(ONE/D(M), DABS(X(M)))                     
      XM = X(M)                                                         
      IF (XM .LT. ZERO) GO TO 90                                        
         XM1 = XM + DEL                                                 
         IF (XM1 .LE. B(2,M)) GO TO 110                                 
           XM1 = XM - DEL                                               
           IF (XM1 .GE. B(1,M)) GO TO 100                               
           GO TO 280                                                    
 90    XM1 = XM - DEL                                                   
       IF (XM1 .GE. B(1,M)) GO TO 100                                   
       XM1 = XM + DEL                                                   
       IF (XM1 .LE. B(2,M)) GO TO 110                                   
       GO TO 280                                                        
C                                                                       
 100  DEL = -DEL                                                        
 110  V(XMSAVE) = XM                                                    
      X(M) = XM1                                                        
      V(DELTA) = DEL                                                    
      IRT = 2                                                           
      GO TO 999                                                         
C                                                                       
C  ***  COMPUTE FINITE-DIFFERENCE HESSIAN USING FUNCTION VALUES ONLY.   
C                                                                       
 120  STP0 = IV(W) + P - 1                                              
      MM1 = M - 1                                                       
      MM1O2 = M*MM1/2                                                   
      HES = -IV(H)                                                      
      IF (M .GT. 0) GO TO 130                                           
C        ***  FIRST CALL ON DF7DHB.  ***                                
         IV(SAVEI) = 0                                                  
         GO TO 240                                                      
C                                                                       
 130  IF (IV(TOOBIG) .EQ. 0) GO TO 150                                  
C        ***  PUNT IN THE EVENT OF AN OVERSIZE STEP  ***                
 140     IV(FDH) = -2                                                   
         GO TO 350                                                      
 150  I = IV(SAVEI)                                                     
      IF (I .GT. 0) GO TO 190                                           
C                                                                       
C  ***  SAVE F(X + STP(M)*E(M)) IN H(P,M)  ***                          
C                                                                       
      PP1O2 = P * (P-1) / 2                                             
      HPM = HES + PP1O2 + MM1                                           
      V(HPM) = V(F)                                                     
C                                                                       
C  ***  START COMPUTING ROW M OF THE FINITE-DIFFERENCE HESSIAN H.  ***  
C                                                                       
      NEWM1 = 1                                                         
      GO TO 260                                                         
 160  HMI = HES + MM1O2                                                 
      IF (MM1 .EQ. 0) GO TO 180                                         
      HPI = HES + PP1O2                                                 
      DO 170 I = 1, MM1                                                 
         T = ZERO                                                       
         IF (B(1,I) .LT. B(2,I)) T = V(FX) - (V(F) + V(HPI))            
         V(HMI) = T                                                     
         HMI = HMI + 1                                                  
         HPI = HPI + 1                                                  
 170     CONTINUE                                                       
 180  V(HMI) = V(F) - TWO*V(FX)                                         
      IF (OFFSID) V(HMI) = V(FX) - TWO*V(F)                             
C                                                                       
C  ***  COMPUTE FUNCTION VALUES NEEDED TO COMPLETE ROW M OF H.  ***     
C                                                                       
      I = 0                                                             
      GO TO 200                                                         
C                                                                       
 190  X(I) = V(DELTA)                                                   
C                                                                       
C  ***  FINISH COMPUTING H(M,I)  ***                                    
C                                                                       
      STPI = STP0 + I                                                   
      HMI = HES + MM1O2 + I - 1                                         
      STPM = STP0 + M                                                   
      V(HMI) = (V(HMI) + V(F)) / (V(STPI)*V(STPM))                      
 200  I = I + 1                                                         
      IF (I .GT. M) GO TO 230                                           
         IF (B(1,I) .LT. B(2,I)) GO TO 210                              
         GO TO 200                                                      
C                                                                       
 210  IV(SAVEI) = I                                                     
      STPI = STP0 + I                                                   
      V(DELTA) = X(I)                                                   
      X(I) = X(I) + V(STPI)                                             
      IRT = 1                                                           
      IF (I .LT. M) GO TO 999                                           
      NEWM1 = 2                                                         
      GO TO 260                                                         
 220  X(M) = V(XMSAVE) - DEL                                            
      IF (OFFSID) X(M) = V(XMSAVE) + TWO*DEL                            
      GO TO 999                                                         
C                                                                       
 230  IV(SAVEI) = 0                                                     
      X(M) = V(XMSAVE)                                                  
C                                                                       
 240  M = M + 1                                                         
      IV(MODE) = M                                                      
      IF (M .GT. P) GO TO 330                                           
      IF (B(1,M) .LT. B(2,M)) GO TO 250                                 
      GO TO 240                                                         
C                                                                       
C  ***  PREPARE TO COMPUTE ROW M OF THE FINITE-DIFFERENCE HESSIAN H.    
C  ***  COMPUTE M-TH STEP SIZE STP(M), THEN RETURN TO OBTAIN            
C  ***  F(X + STP(M)*E(M)), WHERE E(M) = M-TH STD. UNIT VECTOR.         
C                                                                       
 250  V(XMSAVE) = X(M)                                                  
      NEWM1 = 3                                                         
 260  XM = V(XMSAVE)                                                    
      DEL = V(DLTFDC) * DMAX1(ONE/D(M), DABS(XM))                       
      XM1 = XM + DEL                                                    
      OFFSID = .FALSE.                                                  
      IF (XM1 .LE. B(2,M)) GO TO 270                                    
         OFFSID = .TRUE.                                                
         XM1 = XM - DEL                                                 
         IF (XM - TWO*DEL .GE. B(1,M)) GO TO 300                        
         GO TO 280                                                      
 270   IF (XM-DEL .GE. B(1,M)) GO TO 290                                
       OFFSID = .TRUE.                                                  
       IF (XM + TWO*DEL .LE. B(2,M)) GO TO 310                          
C                                                                       
 280  IV(FDH) = -2                                                      
      GO TO 350                                                         
C                                                                       
 290  IF (XM .GE. ZERO) GO TO 310                                       
      XM1 = XM - DEL                                                    
 300  DEL = -DEL                                                        
 310  GO TO (160, 220, 320), NEWM1                                      
 320  X(M) = XM1                                                        
      STPM = STP0 + M                                                   
      V(STPM) = DEL                                                     
      IRT = 1                                                           
      GO TO 999                                                         
C                                                                       
C  ***  HANDLE SPECIAL CASE OF B(1,P) = B(2,P) -- CLEAR SCRATCH VALUES  
C  ***  FROM LAST ROW OF FDH...                                         
C                                                                       
 330  IF (B(1,P) .LT. B(2,P)) GO TO 340                                 
         I = HES + P*(P-1)/2                                            
         CALL DV7SCP(P, V(I), ZERO)                                     
C                                                                       
C  ***  RESTORE V(F), ETC.  ***                                         
C                                                                       
 340  IV(FDH) = HES                                                     
 350  V(F) = V(FX)                                                      
      IRT = 3                                                           
      IF (KIND .LT. 0) GO TO 999                                        
         IV(NFGCAL) = IV(SWITCH)                                        
         GSAVE1 = IV(W) + P                                             
         CALL DV7CPY(P, G, V(GSAVE1))                                   
         GO TO 999                                                      
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DF7DHB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DV7SCL(N, X, A, Y)                                     
C                                                                       
C  ***  SET X(I) = A*Y(I), I = 1(1)N  ***                               
C                                                                       
      INTEGER N                                                         
      DOUBLE PRECISION A, X(N), Y(N)                                    
C                                                                       
      INTEGER I                                                         
C                                                                       
      DO 10 I = 1, N                                                    
 10       X(I) = A * Y(I)                                               
 999    RETURN                                                          
C  ***  LAST LINE OF DV7SCL FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DV7SCP(P, Y, S)                                        
C                                                                       
C  ***  SET P-VECTOR Y TO SCALAR S  ***                                 
C                                                                       
      INTEGER P                                                         
      DOUBLE PRECISION S, Y(P)                                          
C                                                                       
      INTEGER I                                                         
C                                                                       
      DO 10 I = 1, P                                                    
 10      Y(I) = S                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DD7MLP(N, X, Y, Z, K)                                  
C                                                                       
C ***  SET X = DIAG(Y)**K * Z                                           
C ***  FOR X, Z = LOWER TRIANG. MATRICES STORED COMPACTLY BY ROW        
C ***  K = 1 OR -1.                                                     
C                                                                       
      INTEGER N, K                                                      
C/6S                                                                    
C     DOUBLE PRECISION X(1), Y(N), Z(1)                                 
C/7S                                                                    
      DOUBLE PRECISION X(*), Y(N), Z(*)                                 
C/                                                                      
      INTEGER I, J, L                                                   
      DOUBLE PRECISION ONE, T                                           
      DATA ONE/1.D+0/                                                   
C                                                                       
      L = 1                                                             
      IF (K .GE. 0) GO TO 30                                            
      DO 20 I = 1, N                                                    
         T = ONE / Y(I)                                                 
         DO 10 J = 1, I                                                 
            X(L) = T * Z(L)                                             
            L = L + 1                                                   
 10         CONTINUE                                                    
 20      CONTINUE                                                       
      GO TO 999                                                         
C                                                                       
 30   DO 50 I = 1, N                                                    
         T = Y(I)                                                       
         DO 40 J = 1, I                                                 
            X(L) = T * Z(L)                                             
            L = L + 1                                                   
 40         CONTINUE                                                    
 50      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DD7MLP FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DF7HES(D, G, IRT, IV, LIV, LV, P, V, X)                
C                                                                       
C  ***  COMPUTE FINITE-DIFFERENCE HESSIAN, STORE IT IN V STARTING       
C  ***  AT V(IV(FDH)) = V(-IV(H)).                                      
C                                                                       
C  ***  IF IV(COVREQ) .GE. 0 THEN DF7HES USES GRADIENT DIFFERENCES,     
C  ***  OTHERWISE FUNCTION DIFFERENCES.  STORAGE IN V IS AS IN DG7LIT.  
C                                                                       
C IRT VALUES...                                                         
C     1 = COMPUTE FUNCTION VALUE, I.E., V(F).                           
C     2 = COMPUTE G.                                                    
C     3 = DONE.                                                         
C                                                                       
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER IRT, LIV, LV, P                                           
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION D(P), G(P), V(LV), X(P)                          
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER GSAVE1, HES, HMI, HPI, HPM, I, K, KIND, L, M, MM1, MM1O2, 
     1        PP1O2, STPI, STPM, STP0                                   
      DOUBLE PRECISION DEL, HALF, NEGPT5, ONE, TWO, ZERO                
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      EXTERNAL DV7CPY                                                   
C                                                                       
C DV7CPY.... COPY ONE VECTOR TO ANOTHER.                                
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER COVREQ, DELTA, DELTA0, DLTFDC, F, FDH, FX, H, KAGQT, MODE,
     1        NFGCAL, SAVEI, SWITCH, TOOBIG, W, XMSAVE                  
C                                                                       
C/6                                                                     
C     DATA HALF/0.5D+0/, NEGPT5/-0.5D+0/, ONE/1.D+0/, TWO/2.D+0/,       
C    1     ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (HALF=0.5D+0, NEGPT5=-0.5D+0, ONE=1.D+0, TWO=2.D+0,     
     1     ZERO=0.D+0)                                                  
C/                                                                      
C                                                                       
C/6                                                                     
C     DATA COVREQ/15/, DELTA/52/, DELTA0/44/, DLTFDC/42/, F/10/,        
C    1     FDH/74/, FX/53/, H/56/, KAGQT/33/, MODE/35/, NFGCAL/7/,      
C    2     SAVEI/63/, SWITCH/12/, TOOBIG/2/, W/65/, XMSAVE/51/          
C/7                                                                     
      PARAMETER (COVREQ=15, DELTA=52, DELTA0=44, DLTFDC=42, F=10,       
     1           FDH=74, FX=53, H=56, KAGQT=33, MODE=35, NFGCAL=7,      
     2           SAVEI=63, SWITCH=12, TOOBIG=2, W=65, XMSAVE=51)        
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      IRT = 4                                                           
      KIND = IV(COVREQ)                                                 
      M = IV(MODE)                                                      
      IF (M .GT. 0) GO TO 10                                            
         IV(H) = -IABS(IV(H))                                           
         IV(FDH) = 0                                                    
         IV(KAGQT) = -1                                                 
         V(FX) = V(F)                                                   
 10   IF (M .GT. P) GO TO 999                                           
      IF (KIND .LT. 0) GO TO 110                                        
C                                                                       
C  ***  COMPUTE FINITE-DIFFERENCE HESSIAN USING BOTH FUNCTION AND       
C  ***  GRADIENT VALUES.                                                
C                                                                       
      GSAVE1 = IV(W) + P                                                
      IF (M .GT. 0) GO TO 20                                            
C        ***  FIRST CALL ON DF7HES.  SET GSAVE = G, TAKE FIRST STEP  ***
         CALL DV7CPY(P, V(GSAVE1), G)                                   
         IV(SWITCH) = IV(NFGCAL)                                        
         GO TO 90                                                       
C                                                                       
 20   DEL = V(DELTA)                                                    
      X(M) = V(XMSAVE)                                                  
      IF (IV(TOOBIG) .EQ. 0) GO TO 40                                   
C                                                                       
C     ***  HANDLE OVERSIZE V(DELTA)  ***                                
C                                                                       
         IF (DEL*X(M) .GT. ZERO) GO TO 30                               
C             ***  WE ALREADY TRIED SHRINKING V(DELTA), SO QUIT  ***    
              IV(FDH) = -2                                              
              GO TO 220                                                 
C                                                                       
C        ***  TRY SHRINKING V(DELTA)  ***                               
 30      DEL = NEGPT5 * DEL                                             
         GO TO 100                                                      
C                                                                       
 40   HES = -IV(H)                                                      
C                                                                       
C  ***  SET  G = (G - GSAVE)/DEL  ***                                   
C                                                                       
      DO 50 I = 1, P                                                    
         G(I) = (G(I) - V(GSAVE1)) / DEL                                
         GSAVE1 = GSAVE1 + 1                                            
 50      CONTINUE                                                       
C                                                                       
C  ***  ADD G AS NEW COL. TO FINITE-DIFF. HESSIAN MATRIX  ***           
C                                                                       
      K = HES + M*(M-1)/2                                               
      L = K + M - 2                                                     
      IF (M .EQ. 1) GO TO 70                                            
C                                                                       
C  ***  SET  H(I,M) = 0.5 * (H(I,M) + G(I))  FOR I = 1 TO M-1  ***      
C                                                                       
      MM1 = M - 1                                                       
      DO 60 I = 1, MM1                                                  
         V(K) = HALF * (V(K) + G(I))                                    
         K = K + 1                                                      
 60      CONTINUE                                                       
C                                                                       
C  ***  ADD  H(I,M) = G(I)  FOR I = M TO P  ***                         
C                                                                       
 70   L = L + 1                                                         
      DO 80 I = M, P                                                    
         V(L) = G(I)                                                    
         L = L + I                                                      
 80      CONTINUE                                                       
C                                                                       
 90   M = M + 1                                                         
      IV(MODE) = M                                                      
      IF (M .GT. P) GO TO 210                                           
C                                                                       
C  ***  CHOOSE NEXT FINITE-DIFFERENCE STEP, RETURN TO GET G THERE  ***  
C                                                                       
      DEL = V(DELTA0) * DMAX1(ONE/D(M), DABS(X(M)))                     
      IF (X(M) .LT. ZERO) DEL = -DEL                                    
      V(XMSAVE) = X(M)                                                  
 100  X(M) = X(M) + DEL                                                 
      V(DELTA) = DEL                                                    
      IRT = 2                                                           
      GO TO 999                                                         
C                                                                       
C  ***  COMPUTE FINITE-DIFFERENCE HESSIAN USING FUNCTION VALUES ONLY.   
C                                                                       
 110  STP0 = IV(W) + P - 1                                              
      MM1 = M - 1                                                       
      MM1O2 = M*MM1/2                                                   
      IF (M .GT. 0) GO TO 120                                           
C        ***  FIRST CALL ON DF7HES.  ***                                
         IV(SAVEI) = 0                                                  
         GO TO 200                                                      
C                                                                       
 120  I = IV(SAVEI)                                                     
      HES = -IV(H)                                                      
      IF (I .GT. 0) GO TO 180                                           
      IF (IV(TOOBIG) .EQ. 0) GO TO 140                                  
C                                                                       
C     ***  HANDLE OVERSIZE STEP  ***                                    
C                                                                       
         STPM = STP0 + M                                                
         DEL = V(STPM)                                                  
         IF (DEL*X(XMSAVE) .GT. ZERO) GO TO 130                         
C             ***  WE ALREADY TRIED SHRINKING THE STEP, SO QUIT  ***    
              IV(FDH) = -2                                              
              GO TO 220                                                 
C                                                                       
C        ***  TRY SHRINKING THE STEP  ***                               
 130     DEL = NEGPT5 * DEL                                             
         X(M) = X(XMSAVE) + DEL                                         
         V(STPM) = DEL                                                  
         IRT = 1                                                        
         GO TO 999                                                      
C                                                                       
C  ***  SAVE F(X + STP(M)*E(M)) IN H(P,M)  ***                          
C                                                                       
 140  PP1O2 = P * (P-1) / 2                                             
      HPM = HES + PP1O2 + MM1                                           
      V(HPM) = V(F)                                                     
C                                                                       
C  ***  START COMPUTING ROW M OF THE FINITE-DIFFERENCE HESSIAN H.  ***  
C                                                                       
      HMI = HES + MM1O2                                                 
      IF (MM1 .EQ. 0) GO TO 160                                         
      HPI = HES + PP1O2                                                 
      DO 150 I = 1, MM1                                                 
         V(HMI) = V(FX) - (V(F) + V(HPI))                               
         HMI = HMI + 1                                                  
         HPI = HPI + 1                                                  
 150     CONTINUE                                                       
 160  V(HMI) = V(F) - TWO*V(FX)                                         
C                                                                       
C  ***  COMPUTE FUNCTION VALUES NEEDED TO COMPLETE ROW M OF H.  ***     
C                                                                       
      I = 1                                                             
C                                                                       
 170  IV(SAVEI) = I                                                     
      STPI = STP0 + I                                                   
      V(DELTA) = X(I)                                                   
      X(I) = X(I) + V(STPI)                                             
      IF (I .EQ. M) X(I) = V(XMSAVE) - V(STPI)                          
      IRT = 1                                                           
      GO TO 999                                                         
C                                                                       
 180  X(I) = V(DELTA)                                                   
      IF (IV(TOOBIG) .EQ. 0) GO TO 190                                  
C        ***  PUNT IN THE EVENT OF AN OVERSIZE STEP  ***                
         IV(FDH) = -2                                                   
         GO TO 220                                                      
C                                                                       
C  ***  FINISH COMPUTING H(M,I)  ***                                    
C                                                                       
 190  STPI = STP0 + I                                                   
      HMI = HES + MM1O2 + I - 1                                         
      STPM = STP0 + M                                                   
      V(HMI) = (V(HMI) + V(F)) / (V(STPI)*V(STPM))                      
      I = I + 1                                                         
      IF (I .LE. M) GO TO 170                                           
      IV(SAVEI) = 0                                                     
      X(M) = V(XMSAVE)                                                  
C                                                                       
 200  M = M + 1                                                         
      IV(MODE) = M                                                      
      IF (M .GT. P) GO TO 210                                           
C                                                                       
C  ***  PREPARE TO COMPUTE ROW M OF THE FINITE-DIFFERENCE HESSIAN H.    
C  ***  COMPUTE M-TH STEP SIZE STP(M), THEN RETURN TO OBTAIN            
C  ***  F(X + STP(M)*E(M)), WHERE E(M) = M-TH STD. UNIT VECTOR.         
C                                                                       
      DEL = V(DLTFDC) * DMAX1(ONE/D(M), DABS(X(M)))                     
      IF (X(M) .LT. ZERO) DEL = -DEL                                    
      V(XMSAVE) = X(M)                                                  
      X(M) = X(M) + DEL                                                 
      STPM = STP0 + M                                                   
      V(STPM) = DEL                                                     
      IRT = 1                                                           
      GO TO 999                                                         
C                                                                       
C  ***  RESTORE V(F), ETC.  ***                                         
C                                                                       
 210  IV(FDH) = HES                                                     
 220  V(F) = V(FX)                                                      
      IRT = 3                                                           
      IF (KIND .LT. 0) GO TO 999                                        
         IV(NFGCAL) = IV(SWITCH)                                        
         GSAVE1 = IV(W) + P                                             
         CALL DV7CPY(P, G, V(GSAVE1))                                   
         GO TO 999                                                      
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DF7HES FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DV7CPY(P, Y, X)                                        
C                                                                       
C  ***  SET Y = X, WHERE X AND Y ARE P-VECTORS  ***                     
C                                                                       
      INTEGER P                                                         
      DOUBLE PRECISION X(P), Y(P)                                       
C                                                                       
      INTEGER I                                                         
C                                                                       
      DO 10 I = 1, P                                                    
 10      Y(I) = X(I)                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE DH2RFA(N, A, B, X, Y, Z)                               
C                                                                       
C  ***  APPLY 2X2 HOUSEHOLDER REFLECTION DETERMINED BY X, Y, Z TO       
C  ***  N-VECTORS A, B  ***                                             
C                                                                       
      INTEGER N                                                         
      DOUBLE PRECISION A(N), B(N), X, Y, Z                              
      INTEGER I                                                         
      DOUBLE PRECISION T                                                
      DO 10 I = 1, N                                                    
         T = A(I)*X + B(I)*Y                                            
         A(I) = A(I) + T                                                
         B(I) = B(I) + T*Z                                              
 10      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DH2RFA FOLLOWS  ***                                
      END                                                               
      DOUBLE PRECISION FUNCTION DH2RFG(A, B, X, Y, Z)                   
C                                                                       
C  ***  DETERMINE X, Y, Z SO  I + (1,Z)**T * (X,Y)  IS A 2X2            
C  ***  HOUSEHOLDER REFLECTION SENDING (A,B)**T INTO (C,0)**T,          
C  ***  WHERE  C = -SIGN(A)*SQRT(A**2 + B**2)  IS THE VALUE DH2RFG      
C  ***  RETURNS.                                                        
C                                                                       
      DOUBLE PRECISION A, B, X, Y, Z                                    
C                                                                       
      DOUBLE PRECISION A1, B1, C, T                                     
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
      DOUBLE PRECISION ZERO                                             
      DATA ZERO/0.D+0/                                                  
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
      IF (B .NE. ZERO) GO TO 10                                         
         X = ZERO                                                       
         Y = ZERO                                                       
         Z = ZERO                                                       
         DH2RFG = A                                                     
         GO TO 999                                                      
 10   T = DABS(A) + DABS(B)                                             
      A1 = A / T                                                        
      B1 = B / T                                                        
      C = DSQRT(A1**2 + B1**2)                                          
      IF (A1 .GT. ZERO) C = -C                                          
      A1 = A1 - C                                                       
      Z = B1 / A1                                                       
      X = A1 / C                                                        
      Y = B1 / C                                                        
      DH2RFG = T * C                                                    
 999  RETURN                                                            
C  ***  LAST LINE OF DH2RFG FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DSM(M,N,NPAIRS,INDROW,INDCOL,NGRP,MAXGRP,MINGRP,       
     *               INFO,IPNTR,JPNTR,IWA,LIWA,BWA)                     
      INTEGER M,N,NPAIRS,MAXGRP,MINGRP,INFO,LIWA                        
      INTEGER INDROW(NPAIRS),INDCOL(NPAIRS),NGRP(N),                    
     *        IPNTR(1),JPNTR(1),IWA(LIWA)                               
      LOGICAL BWA(N)                                                    
C     **********                                                        
C                                                                       
C     SUBROUTINE DSM                                                    
C                                                                       
C     THE PURPOSE OF DSM IS TO DETERMINE AN OPTIMAL OR NEAR-            
C     OPTIMAL CONSISTENT PARTITION OF THE COLUMNS OF A SPARSE           
C     M BY N MATRIX A.                                                  
C                                                                       
C     THE SPARSITY PATTERN OF THE MATRIX A IS SPECIFIED BY              
C     THE ARRAYS INDROW AND INDCOL. ON INPUT THE INDICES                
C     FOR THE NON-ZERO ELEMENTS OF A ARE                                
C                                                                       
C           INDROW(K),INDCOL(K), K = 1,2,...,NPAIRS.                    
C                                                                       
C     THE (INDROW,INDCOL) PAIRS MAY BE SPECIFIED IN ANY ORDER.          
C     DUPLICATE INPUT PAIRS ARE PERMITTED, BUT THE SUBROUTINE           
C     ELIMINATES THEM.                                                  
C                                                                       
C     THE SUBROUTINE PARTITIONS THE COLUMNS OF A INTO GROUPS            
C     SUCH THAT COLUMNS IN THE SAME GROUP DO NOT HAVE A                 
C     NON-ZERO IN THE SAME ROW POSITION. A PARTITION OF THE             
C     COLUMNS OF A WITH THIS PROPERTY IS CONSISTENT WITH THE            
C     DIRECT DETERMINATION OF A.                                        
C                                                                       
C     THE SUBROUTINE STATEMENT IS                                       
C                                                                       
C       SUBROUTINE DSM(M,N,NPAIRS,INDROW,INDCOL,NGRP,MAXGRP,MINGRP,     
C                      INFO,IPNTR,JPNTR,IWA,LIWA,BWA)                   
C                                                                       
C     WHERE                                                             
C                                                                       
C       M IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        
C         OF ROWS OF A.                                                 
C                                                                       
C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        
C         OF COLUMNS OF A.                                              
C                                                                       
C       NPAIRS IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE          
C         NUMBER OF (INDROW,INDCOL) PAIRS USED TO DESCRIBE THE          
C         SPARSITY PATTERN OF A.                                        
C                                                                       
C       INDROW IS AN INTEGER ARRAY OF LENGTH NPAIRS. ON INPUT INDROW    
C         MUST CONTAIN THE ROW INDICES OF THE NON-ZERO ELEMENTS OF A.   
C         ON OUTPUT INDROW IS PERMUTED SO THAT THE CORRESPONDING        
C         COLUMN INDICES ARE IN NON-DECREASING ORDER. THE COLUMN        
C         INDICES CAN BE RECOVERED FROM THE ARRAY JPNTR.                
C                                                                       
C       INDCOL IS AN INTEGER ARRAY OF LENGTH NPAIRS. ON INPUT INDCOL    
C         MUST CONTAIN THE COLUMN INDICES OF THE NON-ZERO ELEMENTS OF   
C         A. ON OUTPUT INDCOL IS PERMUTED SO THAT THE CORRESPONDING     
C         ROW INDICES ARE IN NON-DECREASING ORDER. THE ROW INDICES      
C         CAN BE RECOVERED FROM THE ARRAY IPNTR.                        
C                                                                       
C       NGRP IS AN INTEGER OUTPUT ARRAY OF LENGTH N WHICH SPECIFIES     
C         THE PARTITION OF THE COLUMNS OF A. COLUMN JCOL BELONGS        
C         TO GROUP NGRP(JCOL).                                          
C                                                                       
C       MAXGRP IS AN INTEGER OUTPUT VARIABLE WHICH SPECIFIES THE        
C         NUMBER OF GROUPS IN THE PARTITION OF THE COLUMNS OF A.        
C                                                                       
C       MINGRP IS AN INTEGER OUTPUT VARIABLE WHICH SPECIFIES A LOWER    
C         BOUND FOR THE NUMBER OF GROUPS IN ANY CONSISTENT PARTITION    
C         OF THE COLUMNS OF A.                                          
C                                                                       
C       INFO IS AN INTEGER OUTPUT VARIABLE SET AS FOLLOWS. FOR          
C         NORMAL TERMINATION INFO = 1. IF M, N, OR NPAIRS IS NOT        
C         POSITIVE OR LIWA IS LESS THAN MAX(M,6*N), THEN INFO = 0.      
C         IF THE K-TH ELEMENT OF INDROW IS NOT AN INTEGER BETWEEN       
C         1 AND M OR THE K-TH ELEMENT OF INDCOL IS NOT AN INTEGER       
C         BETWEEN 1 AND N, THEN INFO = -K.                              
C                                                                       
C       IPNTR IS AN INTEGER OUTPUT ARRAY OF LENGTH M + 1 WHICH          
C         SPECIFIES THE LOCATIONS OF THE COLUMN INDICES IN INDCOL.      
C         THE COLUMN INDICES FOR ROW I ARE                              
C                                                                       
C               INDCOL(K), K = IPNTR(I),...,IPNTR(I+1)-1.               
C                                                                       
C         NOTE THAT IPNTR(M+1)-1 IS THEN THE NUMBER OF NON-ZERO         
C         ELEMENTS OF THE MATRIX A.                                     
C                                                                       
C       JPNTR IS AN INTEGER OUTPUT ARRAY OF LENGTH N + 1 WHICH          
C         SPECIFIES THE LOCATIONS OF THE ROW INDICES IN INDROW.         
C         THE ROW INDICES FOR COLUMN J ARE                              
C                                                                       
C               INDROW(K), K = JPNTR(J),...,JPNTR(J+1)-1.               
C                                                                       
C         NOTE THAT JPNTR(N+1)-1 IS THEN THE NUMBER OF NON-ZERO         
C         ELEMENTS OF THE MATRIX A.                                     
C                                                                       
C       IWA IS AN INTEGER WORK ARRAY OF LENGTH LIWA.                    
C                                                                       
C       LIWA IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN         
C         MAX(M,6*N).                                                   
C                                                                       
C       BWA IS A LOGICAL WORK ARRAY OF LENGTH N.                        
C                                                                       
C     SUBPROGRAMS CALLED                                                
C                                                                       
C       MINPACK-SUPPLIED ...D7EGR,I7DO,N7MSRT,M7SEQ,S7ETR,M7SLO,S7RTDT  
C                                                                       
C       FORTRAN-SUPPLIED ... MAX0                                       
C                                                                       
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1982.          
C     THOMAS F. COLEMAN, BURTON S. GARBOW, JORGE J. MORE                
C                                                                       
C     **********                                                        
      INTEGER I,IR,J,JP,JPL,JPU,K,MAXCLQ,NNZ,NUMGRP                     
C                                                                       
C     CHECK THE INPUT DATA.                                             
C                                                                       
      INFO = 0                                                          
      IF (M .LT. 1 .OR. N .LT. 1 .OR. NPAIRS .LT. 1 .OR.                
     *    LIWA .LT. MAX0(M,6*N)) GO TO 130                              
      DO 10 K = 1, NPAIRS                                               
         INFO = -K                                                      
         IF (INDROW(K) .LT. 1 .OR. INDROW(K) .GT. M .OR.                
     *       INDCOL(K) .LT. 1 .OR. INDCOL(K) .GT. N) GO TO 130          
   10    CONTINUE                                                       
      INFO = 1                                                          
C                                                                       
C     SORT THE DATA STRUCTURE BY COLUMNS.                               
C                                                                       
      CALL S7RTDT(N,NPAIRS,INDROW,INDCOL,JPNTR,IWA(1))                  
C                                                                       
C     COMPRESS THE DATA AND DETERMINE THE NUMBER OF                     
C     NON-ZERO ELEMENTS OF A.                                           
C                                                                       
      DO 20 I = 1, M                                                    
         IWA(I) = 0                                                     
   20    CONTINUE                                                       
      NNZ = 0                                                           
      DO 70 J = 1, N                                                    
         JPL = JPNTR(J)                                                 
         JPU = JPNTR(J+1) - 1                                           
         JPNTR(J) = NNZ + 1                                             
         IF (JPU .LT. JPL) GO TO 60                                     
         DO 40 JP = JPL, JPU                                            
            IR = INDROW(JP)                                             
            IF (IWA(IR) .NE. 0) GO TO 30                                
            NNZ = NNZ + 1                                               
            INDROW(NNZ) = IR                                            
            IWA(IR) = 1                                                 
   30       CONTINUE                                                    
   40       CONTINUE                                                    
         JPL = JPNTR(J)                                                 
         DO 50 JP = JPL, NNZ                                            
            IR = INDROW(JP)                                             
            IWA(IR) = 0                                                 
   50       CONTINUE                                                    
   60    CONTINUE                                                       
   70    CONTINUE                                                       
      JPNTR(N+1) = NNZ + 1                                              
C                                                                       
C     EXTEND THE DATA STRUCTURE TO ROWS.                                
C                                                                       
      CALL S7ETR(M,N,INDROW,JPNTR,INDCOL,IPNTR,IWA(1))                  
C                                                                       
C     DETERMINE A LOWER BOUND FOR THE NUMBER OF GROUPS.                 
C                                                                       
      MINGRP = 0                                                        
      DO 80 I = 1, M                                                    
         MINGRP = MAX0(MINGRP,IPNTR(I+1)-IPNTR(I))                      
   80    CONTINUE                                                       
C                                                                       
C     DETERMINE THE DEGREE SEQUENCE FOR THE INTERSECTION                
C     GRAPH OF THE COLUMNS OF A.                                        
C                                                                       
      CALL D7EGR(N,INDROW,JPNTR,INDCOL,IPNTR,IWA(5*N+1),IWA(N+1),BWA)   
C                                                                       
C     COLOR THE INTERSECTION GRAPH OF THE COLUMNS OF A                  
C     WITH THE SMALLEST-LAST (SL) ORDERING.                             
C                                                                       
      CALL M7SLO(N,INDROW,JPNTR,INDCOL,IPNTR,IWA(5*N+1),IWA(4*N+1),     
     *         MAXCLQ,IWA(1),IWA(N+1),IWA(2*N+1),IWA(3*N+1),BWA)        
      CALL M7SEQ(N,INDROW,JPNTR,INDCOL,IPNTR,IWA(4*N+1),NGRP,MAXGRP,    
     *         IWA(N+1),BWA)                                            
      MINGRP = MAX0(MINGRP,MAXCLQ)                                      
      IF (MAXGRP .EQ. MINGRP) GO TO 130                                 
C                                                                       
C     COLOR THE INTERSECTION GRAPH OF THE COLUMNS OF A                  
C     WITH THE INCIDENCE-DEGREE (ID) ORDERING.                          
C                                                                       
      CALL I7DO(M,N,INDROW,JPNTR,INDCOL,IPNTR,IWA(5*N+1),IWA(4*N+1),    
     *         MAXCLQ,IWA(1),IWA(N+1),IWA(2*N+1),IWA(3*N+1),BWA)        
      CALL M7SEQ(N,INDROW,JPNTR,INDCOL,IPNTR,IWA(4*N+1),IWA(1),NUMGRP,  
     *         IWA(N+1),BWA)                                            
      MINGRP = MAX0(MINGRP,MAXCLQ)                                      
      IF (NUMGRP .GE. MAXGRP) GO TO 100                                 
      MAXGRP = NUMGRP                                                   
      DO 90 J = 1, N                                                    
         NGRP(J) = IWA(J)                                               
   90    CONTINUE                                                       
      IF (MAXGRP .EQ. MINGRP) GO TO 130                                 
  100 CONTINUE                                                          
C                                                                       
C     COLOR THE INTERSECTION GRAPH OF THE COLUMNS OF A                  
C     WITH THE LARGEST-FIRST (LF) ORDERING.                             
C                                                                       
      CALL N7MSRT(N,N-1,IWA(5*N+1),-1,IWA(4*N+1),IWA(2*N+1),IWA(N+1))   
      CALL M7SEQ(N,INDROW,JPNTR,INDCOL,IPNTR,IWA(4*N+1),IWA(1),NUMGRP,  
     *         IWA(N+1),BWA)                                            
      IF (NUMGRP .GE. MAXGRP) GO TO 120                                 
      MAXGRP = NUMGRP                                                   
      DO 110 J = 1, N                                                   
         NGRP(J) = IWA(J)                                               
  110    CONTINUE                                                       
  120 CONTINUE                                                          
C                                                                       
C     EXIT FROM PROGRAM.                                                
C                                                                       
  130 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C     LAST CARD OF SUBROUTINE DSM.                                      
C                                                                       
      END                                                               
      SUBROUTINE I7DO(M,N,INDROW,JPNTR,INDCOL,IPNTR,NDEG,LIST,          
     *               MAXCLQ,IWA1,IWA2,IWA3,IWA4,BWA)                    
      INTEGER M,N,MAXCLQ                                                
      INTEGER INDROW(1),JPNTR(1),INDCOL(1),IPNTR(1),NDEG(N),LIST(N),    
     *        IWA1(N),IWA2(N),IWA3(N),IWA4(N)                           
      LOGICAL BWA(N)                                                    
C     **********                                                        
C                                                                       
C     SUBROUTINE I7DO                                                   
C                                                                       
C     GIVEN THE SPARSITY PATTERN OF AN M BY N MATRIX A, THIS            
C     SUBROUTINE DETERMINES AN INCIDENCE-DEGREE ORDERING OF THE         
C     COLUMNS OF A.                                                     
C                                                                       
C     THE INCIDENCE-DEGREE ORDERING IS DEFINED FOR THE LOOPLESS         
C     GRAPH G WITH VERTICES A(J), J = 1,2,...,N WHERE A(J) IS THE       
C     J-TH COLUMN OF A AND WITH EDGE (A(I),A(J)) IF AND ONLY IF         
C     COLUMNS I AND J HAVE A NON-ZERO IN THE SAME ROW POSITION.         
C                                                                       
C     AT EACH STAGE OF I7DO, A COLUMN OF MAXIMAL INCIDENCE IS           
C     CHOSEN AND ORDERED. IF JCOL IS AN UN-ORDERED COLUMN, THEN         
C     THE INCIDENCE OF JCOL IS THE NUMBER OF ORDERED COLUMNS            
C     ADJACENT TO JCOL IN THE GRAPH G. AMONG ALL THE COLUMNS OF         
C     MAXIMAL INCIDENCE,I7DO CHOOSES A COLUMN OF MAXIMAL DEGREE.        
C                                                                       
C     THE SUBROUTINE STATEMENT IS                                       
C                                                                       
C       SUBROUTINE I7DO(M,N,INDROW,JPNTR,INDCOL,IPNTR,NDEG,LIST,        
C                      MAXCLQ,IWA1,IWA2,IWA3,IWA4,BWA)                  
C                                                                       
C     WHERE                                                             
C                                                                       
C       M IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        
C         OF ROWS OF A.                                                 
C                                                                       
C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        
C         OF COLUMNS OF A.                                              
C                                                                       
C       INDROW IS AN INTEGER INPUT ARRAY WHICH CONTAINS THE ROW         
C         INDICES FOR THE NON-ZEROES IN THE MATRIX A.                   
C                                                                       
C       JPNTR IS AN INTEGER INPUT ARRAY OF LENGTH N + 1 WHICH           
C         SPECIFIES THE LOCATIONS OF THE ROW INDICES IN INDROW.         
C         THE ROW INDICES FOR COLUMN J ARE                              
C                                                                       
C               INDROW(K), K = JPNTR(J),...,JPNTR(J+1)-1.               
C                                                                       
C         NOTE THAT JPNTR(N+1)-1 IS THEN THE NUMBER OF NON-ZERO         
C         ELEMENTS OF THE MATRIX A.                                     
C                                                                       
C       INDCOL IS AN INTEGER INPUT ARRAY WHICH CONTAINS THE             
C         COLUMN INDICES FOR THE NON-ZEROES IN THE MATRIX A.            
C                                                                       
C       IPNTR IS AN INTEGER INPUT ARRAY OF LENGTH M + 1 WHICH           
C         SPECIFIES THE LOCATIONS OF THE COLUMN INDICES IN INDCOL.      
C         THE COLUMN INDICES FOR ROW I ARE                              
C                                                                       
C               INDCOL(K), K = IPNTR(I),...,IPNTR(I+1)-1.               
C                                                                       
C         NOTE THAT IPNTR(M+1)-1 IS THEN THE NUMBER OF NON-ZERO         
C         ELEMENTS OF THE MATRIX A.                                     
C                                                                       
C       NDEG IS AN INTEGER INPUT ARRAY OF LENGTH N WHICH SPECIFIES      
C         THE DEGREE SEQUENCE. THE DEGREE OF THE J-TH COLUMN            
C         OF A IS NDEG(J).                                              
C                                                                       
C       LIST IS AN INTEGER OUTPUT ARRAY OF LENGTH N WHICH SPECIFIES     
C         THE INCIDENCE-DEGREE ORDERING OF THE COLUMNS OF A. THE J-TH   
C         COLUMN IN THIS ORDER IS LIST(J).                              
C                                                                       
C       MAXCLQ IS AN INTEGER OUTPUT VARIABLE SET TO THE SIZE            
C         OF THE LARGEST CLIQUE FOUND DURING THE ORDERING.              
C                                                                       
C       IWA1,IWA2,IWA3, AND IWA4 ARE INTEGER WORK ARRAYS OF LENGTH N.   
C                                                                       
C       BWA IS A LOGICAL WORK ARRAY OF LENGTH N.                        
C                                                                       
C     SUBPROGRAMS CALLED                                                
C                                                                       
C       MINPACK-SUPPLIED ... N7MSRT                                     
C                                                                       
C       FORTRAN-SUPPLIED ... MAX0                                       
C                                                                       
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1982.          
C     THOMAS F. COLEMAN, BURTON S. GARBOW, JORGE J. MORE                
C                                                                       
C     **********                                                        
      INTEGER DEG,HEAD,IC,IP,IPL,IPU,IR,JCOL,JP,JPL,JPU,L,MAXINC,       
     *        MAXLST,NCOMP,NUMINC,NUMLST,NUMORD,NUMWGT                  
C                                                                       
C     SORT THE DEGREE SEQUENCE.                                         
C                                                                       
      CALL N7MSRT(N,N-1,NDEG,-1,IWA4,IWA1,IWA3)                         
C                                                                       
C     INITIALIZATION BLOCK.                                             
C                                                                       
C     CREATE A DOUBLY-LINKED LIST TO ACCESS THE INCIDENCES OF THE       
C     COLUMNS. THE POINTERS FOR THE LINKED LIST ARE AS FOLLOWS.         
C                                                                       
C     EACH UN-ORDERED COLUMN JCOL IS IN A LIST (THE INCIDENCE LIST)     
C     OF COLUMNS WITH THE SAME INCIDENCE.                               
C                                                                       
C     IWA1(NUMINC+1) IS THE FIRST COLUMN IN THE NUMINC LIST             
C     UNLESS IWA1(NUMINC+1) = 0. IN THIS CASE THERE ARE                 
C     NO COLUMNS IN THE NUMINC LIST.                                    
C                                                                       
C     IWA2(JCOL) IS THE COLUMN BEFORE JCOL IN THE INCIDENCE LIST        
C     UNLESS IWA2(JCOL) = 0. IN THIS CASE JCOL IS THE FIRST             
C     COLUMN IN THIS INCIDENCE LIST.                                    
C                                                                       
C     IWA3(JCOL) IS THE COLUMN AFTER JCOL IN THE INCIDENCE LIST         
C     UNLESS IWA3(JCOL) = 0. IN THIS CASE JCOL IS THE LAST              
C     COLUMN IN THIS INCIDENCE LIST.                                    
C                                                                       
C     IF JCOL IS AN UN-ORDERED COLUMN, THEN LIST(JCOL) IS THE           
C     INCIDENCE OF JCOL IN THE GRAPH. IF JCOL IS AN ORDERED COLUMN,     
C     THEN LIST(JCOL) IS THE INCIDENCE-DEGREE ORDER OF COLUMN JCOL.     
C                                                                       
      MAXINC = 0                                                        
      DO 10 JP = 1, N                                                   
         LIST(JP) = 0                                                   
         BWA(JP) = .FALSE.                                              
         IWA1(JP) = 0                                                   
         L = IWA4(JP)                                                   
         IF (JP .NE. 1) IWA2(L) = IWA4(JP-1)                            
         IF (JP .NE. N) IWA3(L) = IWA4(JP+1)                            
   10    CONTINUE                                                       
      IWA1(1) = IWA4(1)                                                 
      L = IWA4(1)                                                       
      IWA2(L) = 0                                                       
      L = IWA4(N)                                                       
      IWA3(L) = 0                                                       
C                                                                       
C     DETERMINE THE MAXIMAL SEARCH LENGTH FOR THE LIST                  
C     OF COLUMNS OF MAXIMAL INCIDENCE.                                  
C                                                                       
      MAXLST = 0                                                        
      DO 20 IR = 1, M                                                   
         MAXLST = MAXLST + (IPNTR(IR+1) - IPNTR(IR))**2                 
   20    CONTINUE                                                       
      MAXLST = MAXLST/N                                                 
      MAXCLQ = 1                                                        
C                                                                       
C     BEGINNING OF ITERATION LOOP.                                      
C                                                                       
      DO 140 NUMORD = 1, N                                              
C                                                                       
C        CHOOSE A COLUMN JCOL OF MAXIMAL DEGREE AMONG THE               
C        COLUMNS OF MAXIMAL INCIDENCE.                                  
C                                                                       
         JP = IWA1(MAXINC+1)                                            
         NUMLST = 1                                                     
         NUMWGT = -1                                                    
   30    CONTINUE                                                       
            IF (NDEG(JP) .LE. NUMWGT) GO TO 40                          
            NUMWGT = NDEG(JP)                                           
            JCOL = JP                                                   
   40       CONTINUE                                                    
            JP = IWA3(JP)                                               
            NUMLST = NUMLST + 1                                         
            IF (JP .GT. 0 .AND. NUMLST .LE. MAXLST) GO TO 30            
         LIST(JCOL) = NUMORD                                            
C                                                                       
C        DELETE COLUMN JCOL FROM THE LIST OF COLUMNS OF                 
C        MAXIMAL INCIDENCE.                                             
C                                                                       
         L = IWA2(JCOL)                                                 
         IF (L .EQ. 0) IWA1(MAXINC+1) = IWA3(JCOL)                      
         IF (L .GT. 0) IWA3(L) = IWA3(JCOL)                             
         L = IWA3(JCOL)                                                 
         IF (L .GT. 0) IWA2(L) = IWA2(JCOL)                             
C                                                                       
C        UPDATE THE SIZE OF THE LARGEST CLIQUE                          
C        FOUND DURING THE ORDERING.                                     
C                                                                       
         IF (MAXINC .EQ. 0) NCOMP = 0                                   
         NCOMP = NCOMP + 1                                              
         IF (MAXINC + 1 .EQ. NCOMP) MAXCLQ = MAX0(MAXCLQ,NCOMP)         
C                                                                       
C        UPDATE THE MAXIMAL INCIDENCE COUNT.                            
C                                                                       
   50    CONTINUE                                                       
            IF (IWA1(MAXINC+1) .GT. 0) GO TO 60                         
            MAXINC = MAXINC - 1                                         
            IF (MAXINC .GE. 0) GO TO 50                                 
   60    CONTINUE                                                       
C                                                                       
C        FIND ALL COLUMNS ADJACENT TO COLUMN JCOL.                      
C                                                                       
         BWA(JCOL) = .TRUE.                                             
         DEG = 0                                                        
C                                                                       
C        DETERMINE ALL POSITIONS (IR,JCOL) WHICH CORRESPOND             
C        TO NON-ZEROES IN THE MATRIX.                                   
C                                                                       
         JPL = JPNTR(JCOL)                                              
         JPU = JPNTR(JCOL+1) - 1                                        
         IF (JPU .LT. JPL) GO TO 100                                    
         DO 90 JP = JPL, JPU                                            
            IR = INDROW(JP)                                             
C                                                                       
C           FOR EACH ROW IR, DETERMINE ALL POSITIONS (IR,IC)            
C           WHICH CORRESPOND TO NON-ZEROES IN THE MATRIX.               
C                                                                       
            IPL = IPNTR(IR)                                             
            IPU = IPNTR(IR+1) - 1                                       
            DO 80 IP = IPL, IPU                                         
               IC = INDCOL(IP)                                          
C                                                                       
C              ARRAY BWA MARKS COLUMNS WHICH ARE ADJACENT TO            
C              COLUMN JCOL. ARRAY IWA4 RECORDS THE MARKED COLUMNS.      
C                                                                       
               IF (BWA(IC)) GO TO 70                                    
               BWA(IC) = .TRUE.                                         
               DEG = DEG + 1                                            
               IWA4(DEG) = IC                                           
   70          CONTINUE                                                 
   80          CONTINUE                                                 
   90       CONTINUE                                                    
  100    CONTINUE                                                       
C                                                                       
C        UPDATE THE POINTERS TO THE INCIDENCE LISTS.                    
C                                                                       
         IF (DEG .LT. 1) GO TO 130                                      
         DO 120 JP = 1, DEG                                             
            IC = IWA4(JP)                                               
            IF (LIST(IC) .GT. 0) GO TO 110                              
            NUMINC = -LIST(IC) + 1                                      
            LIST(IC) = -NUMINC                                          
            MAXINC = MAX0(MAXINC,NUMINC)                                
C                                                                       
C           DELETE COLUMN IC FROM THE NUMINC-1 LIST.                    
C                                                                       
            L = IWA2(IC)                                                
            IF (L .EQ. 0) IWA1(NUMINC) = IWA3(IC)                       
            IF (L .GT. 0) IWA3(L) = IWA3(IC)                            
            L = IWA3(IC)                                                
            IF (L .GT. 0) IWA2(L) = IWA2(IC)                            
C                                                                       
C           ADD COLUMN IC TO THE NUMINC LIST.                           
C                                                                       
            HEAD = IWA1(NUMINC+1)                                       
            IWA1(NUMINC+1) = IC                                         
            IWA2(IC) = 0                                                
            IWA3(IC) = HEAD                                             
            IF (HEAD .GT. 0) IWA2(HEAD) = IC                            
  110       CONTINUE                                                    
C                                                                       
C           UN-MARK COLUMN IC IN THE ARRAY BWA.                         
C                                                                       
            BWA(IC) = .FALSE.                                           
  120       CONTINUE                                                    
  130    CONTINUE                                                       
         BWA(JCOL) = .FALSE.                                            
C                                                                       
C        END OF ITERATION LOOP.                                         
C                                                                       
  140    CONTINUE                                                       
C                                                                       
C     INVERT THE ARRAY LIST.                                            
C                                                                       
      DO 150 JCOL = 1, N                                                
         NUMORD = LIST(JCOL)                                            
         IWA1(NUMORD) = JCOL                                            
  150    CONTINUE                                                       
      DO 160 JP = 1, N                                                  
         LIST(JP) = IWA1(JP)                                            
  160    CONTINUE                                                       
      RETURN                                                            
C                                                                       
C     LAST CARD OF SUBROUTINE I7DO.                                     
C                                                                       
      END                                                               
      SUBROUTINE I7PNVR(N, X, Y)                                        
C                                                                       
C  ***  SET PERMUTATION VECTOR X TO INVERSE OF Y  ***                   
C                                                                       
      INTEGER N                                                         
      INTEGER X(N), Y(N)                                                
C                                                                       
      INTEGER I, J                                                      
      DO 10 I = 1, N                                                    
         J = Y(I)                                                       
         X(J) = I                                                       
 10      CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF I7PNVR FOLLOWS  ***                                
      END                                                               
      SUBROUTINE I7SHFT(N, K, X)                                        
C                                                                       
C  ***  SHIFT X(K),...,X(N) LEFT CIRCULARLY ONE POSITION  ***           
C                                                                       
      INTEGER N, K                                                      
      INTEGER X(N)                                                      
C                                                                       
      INTEGER I, NM1, T                                                 
C                                                                       
      IF (K .GE. N) GO TO 999                                           
      NM1 = N - 1                                                       
      T = X(K)                                                          
      DO 10 I = K, NM1                                                  
 10      X(I) = X(I+1)                                                  
      X(N) = T                                                          
 999  RETURN                                                            
      END                                                               
      SUBROUTINE M7SEQ(N,INDROW,JPNTR,INDCOL,IPNTR,LIST,NGRP,MAXGRP,    
     *               IWA,BWA)                                           
      INTEGER N,MAXGRP                                                  
      INTEGER INDROW(1),JPNTR(1),INDCOL(1),IPNTR(1),LIST(N),NGRP(N),    
     *        IWA(N)                                                    
      LOGICAL BWA(N)                                                    
C     **********                                                        
C                                                                       
C     SUBROUTINE M7SEQ                                                  
C                                                                       
C     GIVEN THE SPARSITY PATTERN OF AN M BY N MATRIX A, THIS            
C     SUBROUTINE DETERMINES A CONSISTENT PARTITION OF THE               
C     COLUMNS OF A BY A SEQUENTIAL ALGORITHM.                           
C                                                                       
C     A CONSISTENT PARTITION IS DEFINED IN TERMS OF THE LOOPLESS        
C     GRAPH G WITH VERTICES A(J), J = 1,2,...,N WHERE A(J) IS THE       
C     J-TH COLUMN OF A AND WITH EDGE (A(I),A(J)) IF AND ONLY IF         
C     COLUMNS I AND J HAVE A NON-ZERO IN THE SAME ROW POSITION.         
C                                                                       
C     A PARTITION OF THE COLUMNS OF A INTO GROUPS IS CONSISTENT         
C     IF THE COLUMNS IN ANY GROUP ARE NOT ADJACENT IN THE GRAPH G.      
C     IN GRAPH-THEORY TERMINOLOGY, A CONSISTENT PARTITION OF THE        
C     COLUMNS OF A CORRESPONDS TO A COLORING OF THE GRAPH G.            
C                                                                       
C     THE SUBROUTINE EXAMINES THE COLUMNS IN THE ORDER SPECIFIED        
C     BY THE ARRAY LIST, AND ASSIGNS THE CURRENT COLUMN TO THE          
C     GROUP WITH THE SMALLEST POSSIBLE NUMBER.                          
C                                                                       
C     NOTE THAT THE VALUE OF M IS NOT NEEDED BY M7SEQ AND IS            
C     THEREFORE NOT PRESENT IN THE SUBROUTINE STATEMENT.                
C                                                                       
C     THE SUBROUTINE STATEMENT IS                                       
C                                                                       
C       SUBROUTINE M7SEQ(N,INDROW,JPNTR,INDCOL,IPNTR,LIST,NGRP,MAXGRP,  
C                      IWA,BWA)                                         
C                                                                       
C     WHERE                                                             
C                                                                       
C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        
C         OF COLUMNS OF A.                                              
C                                                                       
C       INDROW IS AN INTEGER INPUT ARRAY WHICH CONTAINS THE ROW         
C         INDICES FOR THE NON-ZEROES IN THE MATRIX A.                   
C                                                                       
C       JPNTR IS AN INTEGER INPUT ARRAY OF LENGTH N + 1 WHICH           
C         SPECIFIES THE LOCATIONS OF THE ROW INDICES IN INDROW.         
C         THE ROW INDICES FOR COLUMN J ARE                              
C                                                                       
C               INDROW(K), K = JPNTR(J),...,JPNTR(J+1)-1.               
C                                                                       
C         NOTE THAT JPNTR(N+1)-1 IS THEN THE NUMBER OF NON-ZERO         
C         ELEMENTS OF THE MATRIX A.                                     
C                                                                       
C       INDCOL IS AN INTEGER INPUT ARRAY WHICH CONTAINS THE             
C         COLUMN INDICES FOR THE NON-ZEROES IN THE MATRIX A.            
C                                                                       
C       IPNTR IS AN INTEGER INPUT ARRAY OF LENGTH M + 1 WHICH           
C         SPECIFIES THE LOCATIONS OF THE COLUMN INDICES IN INDCOL.      
C         THE COLUMN INDICES FOR ROW I ARE                              
C                                                                       
C               INDCOL(K), K = IPNTR(I),...,IPNTR(I+1)-1.               
C                                                                       
C         NOTE THAT IPNTR(M+1)-1 IS THEN THE NUMBER OF NON-ZERO         
C         ELEMENTS OF THE MATRIX A.                                     
C                                                                       
C       LIST IS AN INTEGER INPUT ARRAY OF LENGTH N WHICH SPECIFIES      
C         THE ORDER TO BE USED BY THE SEQUENTIAL ALGORITHM.             
C         THE J-TH COLUMN IN THIS ORDER IS LIST(J).                     
C                                                                       
C       NGRP IS AN INTEGER OUTPUT ARRAY OF LENGTH N WHICH SPECIFIES     
C         THE PARTITION OF THE COLUMNS OF A. COLUMN JCOL BELONGS        
C         TO GROUP NGRP(JCOL).                                          
C                                                                       
C       MAXGRP IS AN INTEGER OUTPUT VARIABLE WHICH SPECIFIES THE        
C         NUMBER OF GROUPS IN THE PARTITION OF THE COLUMNS OF A.        
C                                                                       
C       IWA IS AN INTEGER WORK ARRAY OF LENGTH N.                       
C                                                                       
C       BWA IS A LOGICAL WORK ARRAY OF LENGTH N.                        
C                                                                       
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1982.          
C     THOMAS F. COLEMAN, BURTON S. GARBOW, JORGE J. MORE                
C                                                                       
C     **********                                                        
      INTEGER DEG,IC,IP,IPL,IPU,IR,J,JCOL,JP,JPL,JPU,L,NUMGRP           
C                                                                       
C     INITIALIZATION BLOCK.                                             
C                                                                       
      MAXGRP = 0                                                        
      DO 10 JP = 1, N                                                   
         NGRP(JP) = N                                                   
         BWA(JP) = .FALSE.                                              
   10    CONTINUE                                                       
      BWA(N) = .TRUE.                                                   
C                                                                       
C     BEGINNING OF ITERATION LOOP.                                      
C                                                                       
      DO 100 J = 1, N                                                   
         JCOL = LIST(J)                                                 
C                                                                       
C        FIND ALL COLUMNS ADJACENT TO COLUMN JCOL.                      
C                                                                       
         DEG = 0                                                        
C                                                                       
C        DETERMINE ALL POSITIONS (IR,JCOL) WHICH CORRESPOND             
C        TO NON-ZEROES IN THE MATRIX.                                   
C                                                                       
         JPL = JPNTR(JCOL)                                              
         JPU = JPNTR(JCOL+1) - 1                                        
         IF (JPU .LT. JPL) GO TO 50                                     
         DO 40 JP = JPL, JPU                                            
            IR = INDROW(JP)                                             
C                                                                       
C           FOR EACH ROW IR, DETERMINE ALL POSITIONS (IR,IC)            
C           WHICH CORRESPOND TO NON-ZEROES IN THE MATRIX.               
C                                                                       
            IPL = IPNTR(IR)                                             
            IPU = IPNTR(IR+1) - 1                                       
            DO 30 IP = IPL, IPU                                         
               IC = INDCOL(IP)                                          
               L = NGRP(IC)                                             
C                                                                       
C              ARRAY BWA MARKS THE GROUP NUMBERS OF THE                 
C              COLUMNS WHICH ARE ADJACENT TO COLUMN JCOL.               
C              ARRAY IWA RECORDS THE MARKED GROUP NUMBERS.              
C                                                                       
               IF (BWA(L)) GO TO 20                                     
               BWA(L) = .TRUE.                                          
               DEG = DEG + 1                                            
               IWA(DEG) = L                                             
   20          CONTINUE                                                 
   30          CONTINUE                                                 
   40       CONTINUE                                                    
   50    CONTINUE                                                       
C                                                                       
C        ASSIGN THE SMALLEST UN-MARKED GROUP NUMBER TO JCOL.            
C                                                                       
         DO 60 JP = 1, N                                                
            NUMGRP = JP                                                 
            IF (.NOT. BWA(JP)) GO TO 70                                 
   60       CONTINUE                                                    
   70    CONTINUE                                                       
         NGRP(JCOL) = NUMGRP                                            
         MAXGRP = MAX0(MAXGRP,NUMGRP)                                   
C                                                                       
C        UN-MARK THE GROUP NUMBERS.                                     
C                                                                       
         IF (DEG .LT. 1) GO TO 90                                       
         DO 80 JP = 1, DEG                                              
            L = IWA(JP)                                                 
            BWA(L) = .FALSE.                                            
   80       CONTINUE                                                    
   90    CONTINUE                                                       
  100    CONTINUE                                                       
C                                                                       
C        END OF ITERATION LOOP.                                         
C                                                                       
      RETURN                                                            
C                                                                       
C     LAST CARD OF SUBROUTINE M7SEQ.                                    
C                                                                       
      END                                                               
      SUBROUTINE M7SLO(N,INDROW,JPNTR,INDCOL,IPNTR,NDEG,LIST,           
     *               MAXCLQ,IWA1,IWA2,IWA3,IWA4,BWA)                    
      INTEGER N,MAXCLQ                                                  
      INTEGER INDROW(1),JPNTR(1),INDCOL(1),IPNTR(1),NDEG(N),            
     *        LIST(N),IWA1(N),IWA2(N),IWA3(N),IWA4(N)                   
      LOGICAL BWA(N)                                                    
C     **********                                                        
C                                                                       
C     SUBROUTINE M7SLO                                                  
C                                                                       
C     GIVEN THE SPARSITY PATTERN OF AN M BY N MATRIX A, THIS            
C     SUBROUTINE DETERMINES THE SMALLEST-LAST ORDERING OF THE           
C     COLUMNS OF A.                                                     
C                                                                       
C     THE SMALLEST-LAST ORDERING IS DEFINED FOR THE LOOPLESS            
C     GRAPH G WITH VERTICES A(J), J = 1,2,...,N WHERE A(J) IS THE       
C     J-TH COLUMN OF A AND WITH EDGE (A(I),A(J)) IF AND ONLY IF         
C     COLUMNS I AND J HAVE A NON-ZERO IN THE SAME ROW POSITION.         
C                                                                       
C     THE SMALLEST-LAST ORDERING IS DETERMINED RECURSIVELY BY           
C     LETTING LIST(K), K = N,...,1 BE A COLUMN WITH LEAST DEGREE        
C     IN THE SUBGRAPH SPANNED BY THE UN-ORDERED COLUMNS.                
C                                                                       
C     NOTE THAT THE VALUE OF M IS NOT NEEDED BY M7SLO AND IS            
C     THEREFORE NOT PRESENT IN THE SUBROUTINE STATEMENT.                
C                                                                       
C     THE SUBROUTINE STATEMENT IS                                       
C                                                                       
C       SUBROUTINE M7SLO(N,INDROW,JPNTR,INDCOL,IPNTR,NDEG,LIST,         
C                      MAXCLQ,IWA1,IWA2,IWA3,IWA4,BWA)                  
C                                                                       
C     WHERE                                                             
C                                                                       
C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        
C         OF COLUMNS OF A.                                              
C                                                                       
C       INDROW IS AN INTEGER INPUT ARRAY WHICH CONTAINS THE ROW         
C         INDICES FOR THE NON-ZEROES IN THE MATRIX A.                   
C                                                                       
C       JPNTR IS AN INTEGER INPUT ARRAY OF LENGTH N + 1 WHICH           
C         SPECIFIES THE LOCATIONS OF THE ROW INDICES IN INDROW.         
C         THE ROW INDICES FOR COLUMN J ARE                              
C                                                                       
C               INDROW(K), K = JPNTR(J),...,JPNTR(J+1)-1.               
C                                                                       
C         NOTE THAT JPNTR(N+1)-1 IS THEN THE NUMBER OF NON-ZERO         
C         ELEMENTS OF THE MATRIX A.                                     
C                                                                       
C       INDCOL IS AN INTEGER INPUT ARRAY WHICH CONTAINS THE             
C         COLUMN INDICES FOR THE NON-ZEROES IN THE MATRIX A.            
C                                                                       
C       IPNTR IS AN INTEGER INPUT ARRAY OF LENGTH M + 1 WHICH           
C         SPECIFIES THE LOCATIONS OF THE COLUMN INDICES IN INDCOL.      
C         THE COLUMN INDICES FOR ROW I ARE                              
C                                                                       
C               INDCOL(K), K = IPNTR(I),...,IPNTR(I+1)-1.               
C                                                                       
C         NOTE THAT IPNTR(M+1)-1 IS THEN THE NUMBER OF NON-ZERO         
C         ELEMENTS OF THE MATRIX A.                                     
C                                                                       
C       NDEG IS AN INTEGER INPUT ARRAY OF LENGTH N WHICH SPECIFIES      
C         THE DEGREE SEQUENCE. THE DEGREE OF THE J-TH COLUMN            
C         OF A IS NDEG(J).                                              
C                                                                       
C       LIST IS AN INTEGER OUTPUT ARRAY OF LENGTH N WHICH SPECIFIES     
C         THE SMALLEST-LAST ORDERING OF THE COLUMNS OF A. THE J-TH      
C         COLUMN IN THIS ORDER IS LIST(J).                              
C                                                                       
C       MAXCLQ IS AN INTEGER OUTPUT VARIABLE SET TO THE SIZE            
C         OF THE LARGEST CLIQUE FOUND DURING THE ORDERING.              
C                                                                       
C       IWA1,IWA2,IWA3, AND IWA4 ARE INTEGER WORK ARRAYS OF LENGTH N.   
C                                                                       
C       BWA IS A LOGICAL WORK ARRAY OF LENGTH N.                        
C                                                                       
C     SUBPROGRAMS CALLED                                                
C                                                                       
C       FORTRAN-SUPPLIED ... MIN0                                       
C                                                                       
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1982.          
C     THOMAS F. COLEMAN, BURTON S. GARBOW, JORGE J. MORE                
C                                                                       
C     **********                                                        
      INTEGER DEG,HEAD,IC,IP,IPL,IPU,IR,JCOL,JP,JPL,JPU,                
     *        L,MINDEG,NUMDEG,NUMORD                                    
C                                                                       
C     INITIALIZATION BLOCK.                                             
C                                                                       
      MINDEG = N                                                        
      DO 10 JP = 1, N                                                   
         IWA1(JP) = 0                                                   
         BWA(JP) = .FALSE.                                              
         LIST(JP) = NDEG(JP)                                            
         MINDEG = MIN0(MINDEG,NDEG(JP))                                 
   10    CONTINUE                                                       
C                                                                       
C     CREATE A DOUBLY-LINKED LIST TO ACCESS THE DEGREES OF THE          
C     COLUMNS. THE POINTERS FOR THE LINKED LIST ARE AS FOLLOWS.         
C                                                                       
C     EACH UN-ORDERED COLUMN JCOL IS IN A LIST (THE DEGREE              
C     LIST) OF COLUMNS WITH THE SAME DEGREE.                            
C                                                                       
C     IWA1(NUMDEG+1) IS THE FIRST COLUMN IN THE NUMDEG LIST             
C     UNLESS IWA1(NUMDEG+1) = 0. IN THIS CASE THERE ARE                 
C     NO COLUMNS IN THE NUMDEG LIST.                                    
C                                                                       
C     IWA2(JCOL) IS THE COLUMN BEFORE JCOL IN THE DEGREE LIST           
C     UNLESS IWA2(JCOL) = 0. IN THIS CASE JCOL IS THE FIRST             
C     COLUMN IN THIS DEGREE LIST.                                       
C                                                                       
C     IWA3(JCOL) IS THE COLUMN AFTER JCOL IN THE DEGREE LIST            
C     UNLESS IWA3(JCOL) = 0. IN THIS CASE JCOL IS THE LAST              
C     COLUMN IN THIS DEGREE LIST.                                       
C                                                                       
C     IF JCOL IS AN UN-ORDERED COLUMN, THEN LIST(JCOL) IS THE           
C     DEGREE OF JCOL IN THE GRAPH INDUCED BY THE UN-ORDERED             
C     COLUMNS. IF JCOL IS AN ORDERED COLUMN, THEN LIST(JCOL)            
C     IS THE SMALLEST-LAST ORDER OF COLUMN JCOL.                        
C                                                                       
      DO 20 JP = 1, N                                                   
         NUMDEG = NDEG(JP)                                              
         HEAD = IWA1(NUMDEG+1)                                          
         IWA1(NUMDEG+1) = JP                                            
         IWA2(JP) = 0                                                   
         IWA3(JP) = HEAD                                                
         IF (HEAD .GT. 0) IWA2(HEAD) = JP                               
   20    CONTINUE                                                       
      MAXCLQ = 0                                                        
      NUMORD = N                                                        
C                                                                       
C     BEGINNING OF ITERATION LOOP.                                      
C                                                                       
   30 CONTINUE                                                          
C                                                                       
C        MARK THE SIZE OF THE LARGEST CLIQUE                            
C        FOUND DURING THE ORDERING.                                     
C                                                                       
         IF (MINDEG + 1 .EQ. NUMORD .AND. MAXCLQ .EQ. 0)                
     *       MAXCLQ = NUMORD                                            
C                                                                       
C        CHOOSE A COLUMN JCOL OF MINIMAL DEGREE MINDEG.                 
C                                                                       
   40    CONTINUE                                                       
            JCOL = IWA1(MINDEG+1)                                       
            IF (JCOL .GT. 0) GO TO 50                                   
            MINDEG = MINDEG + 1                                         
            GO TO 40                                                    
   50    CONTINUE                                                       
         LIST(JCOL) = NUMORD                                            
         NUMORD = NUMORD - 1                                            
C                                                                       
C        TERMINATION TEST.                                              
C                                                                       
         IF (NUMORD .EQ. 0) GO TO 120                                   
C                                                                       
C        DELETE COLUMN JCOL FROM THE MINDEG LIST.                       
C                                                                       
         L = IWA3(JCOL)                                                 
         IWA1(MINDEG+1) = L                                             
         IF (L .GT. 0) IWA2(L) = 0                                      
C                                                                       
C        FIND ALL COLUMNS ADJACENT TO COLUMN JCOL.                      
C                                                                       
         BWA(JCOL) = .TRUE.                                             
         DEG = 0                                                        
C                                                                       
C        DETERMINE ALL POSITIONS (IR,JCOL) WHICH CORRESPOND             
C        TO NON-ZEROES IN THE MATRIX.                                   
C                                                                       
         JPL = JPNTR(JCOL)                                              
         JPU = JPNTR(JCOL+1) - 1                                        
         IF (JPU .LT. JPL) GO TO 90                                     
         DO 80 JP = JPL, JPU                                            
            IR = INDROW(JP)                                             
C                                                                       
C           FOR EACH ROW IR, DETERMINE ALL POSITIONS (IR,IC)            
C           WHICH CORRESPOND TO NON-ZEROES IN THE MATRIX.               
C                                                                       
            IPL = IPNTR(IR)                                             
            IPU = IPNTR(IR+1) - 1                                       
            DO 70 IP = IPL, IPU                                         
               IC = INDCOL(IP)                                          
C                                                                       
C              ARRAY BWA MARKS COLUMNS WHICH ARE ADJACENT TO            
C              COLUMN JCOL. ARRAY IWA4 RECORDS THE MARKED COLUMNS.      
C                                                                       
               IF (BWA(IC)) GO TO 60                                    
               BWA(IC) = .TRUE.                                         
               DEG = DEG + 1                                            
               IWA4(DEG) = IC                                           
   60          CONTINUE                                                 
   70          CONTINUE                                                 
   80       CONTINUE                                                    
   90    CONTINUE                                                       
C                                                                       
C        UPDATE THE POINTERS TO THE CURRENT DEGREE LISTS.               
C                                                                       
         IF (DEG .LT. 1) GO TO 110                                      
         DO 100 JP = 1, DEG                                             
            IC = IWA4(JP)                                               
            NUMDEG = LIST(IC)                                           
            LIST(IC) = LIST(IC) - 1                                     
            MINDEG = MIN0(MINDEG,LIST(IC))                              
C                                                                       
C           DELETE COLUMN IC FROM THE NUMDEG LIST.                      
C                                                                       
            L = IWA2(IC)                                                
            IF (L .EQ. 0) IWA1(NUMDEG+1) = IWA3(IC)                     
            IF (L .GT. 0) IWA3(L) = IWA3(IC)                            
            L = IWA3(IC)                                                
            IF (L .GT. 0) IWA2(L) = IWA2(IC)                            
C                                                                       
C           ADD COLUMN IC TO THE NUMDEG-1 LIST.                         
C                                                                       
            HEAD = IWA1(NUMDEG)                                         
            IWA1(NUMDEG) = IC                                           
            IWA2(IC) = 0                                                
            IWA3(IC) = HEAD                                             
            IF (HEAD .GT. 0) IWA2(HEAD) = IC                            
C                                                                       
C           UN-MARK COLUMN IC IN THE ARRAY BWA.                         
C                                                                       
            BWA(IC) = .FALSE.                                           
  100       CONTINUE                                                    
  110    CONTINUE                                                       
C                                                                       
C        END OF ITERATION LOOP.                                         
C                                                                       
         GO TO 30                                                       
  120 CONTINUE                                                          
C                                                                       
C     INVERT THE ARRAY LIST.                                            
C                                                                       
      DO 130 JCOL = 1, N                                                
         NUMORD = LIST(JCOL)                                            
         IWA1(NUMORD) = JCOL                                            
  130    CONTINUE                                                       
      DO 140 JP = 1, N                                                  
         LIST(JP) = IWA1(JP)                                            
  140    CONTINUE                                                       
      RETURN                                                            
C                                                                       
C     LAST CARD OF SUBROUTINE M7SLO.                                    
C                                                                       
      END                                                               
      SUBROUTINE N7MSRT(N,NMAX,NUM,MODE,INDEX,LAST,NEXT)                
      INTEGER N,NMAX,MODE                                               
      INTEGER NUM(N),INDEX(N),LAST(1),NEXT(N)                           
C     **********.                                                       
C                                                                       
C     SUBROUTINE N7MSRT                                                 
C                                                                       
C     GIVEN A SEQUENCE OF INTEGERS, THIS SUBROUTINE GROUPS              
C     TOGETHER THOSE INDICES WITH THE SAME SEQUENCE VALUE               
C     AND, OPTIONALLY, SORTS THE SEQUENCE INTO EITHER                   
C     ASCENDING OR DESCENDING ORDER.                                    
C                                                                       
C     THE SEQUENCE OF INTEGERS IS DEFINED BY THE ARRAY NUM,             
C     AND IT IS ASSUMED THAT THE INTEGERS ARE EACH FROM THE SET         
C     0,1,...,NMAX. ON OUTPUT THE INDICES K SUCH THAT NUM(K) = L        
C     FOR ANY L = 0,1,...,NMAX CAN BE OBTAINED FROM THE ARRAYS          
C     LAST AND NEXT AS FOLLOWS.                                         
C                                                                       
C           K = LAST(L+1)                                               
C           WHILE (K .NE. 0) K = NEXT(K)                                
C                                                                       
C     OPTIONALLY, THE SUBROUTINE PRODUCES AN ARRAY INDEX SO THAT        
C     THE SEQUENCE NUM(INDEX(I)), I = 1,2,...,N IS SORTED.              
C                                                                       
C     THE SUBROUTINE STATEMENT IS                                       
C                                                                       
C       SUBROUTINE N7MSRT(N,NMAX,NUM,MODE,INDEX,LAST,NEXT)              
C                                                                       
C     WHERE                                                             
C                                                                       
C       N IS A POSITIVE INTEGER INPUT VARIABLE.                         
C                                                                       
C       NMAX IS A POSITIVE INTEGER INPUT VARIABLE.                      
C                                                                       
C       NUM IS AN INPUT ARRAY OF LENGTH N WHICH CONTAINS THE            
C         SEQUENCE OF INTEGERS TO BE GROUPED AND SORTED. IT             
C         IS ASSUMED THAT THE INTEGERS ARE EACH FROM THE SET            
C         0,1,...,NMAX.                                                 
C                                                                       
C       MODE IS AN INTEGER INPUT VARIABLE. THE SEQUENCE NUM IS          
C         SORTED IN ASCENDING ORDER IF MODE IS POSITIVE AND IN          
C         DESCENDING ORDER IF MODE IS NEGATIVE. IF MODE IS 0,           
C         NO SORTING IS DONE.                                           
C                                                                       
C       INDEX IS AN INTEGER OUTPUT ARRAY OF LENGTH N SET SO             
C         THAT THE SEQUENCE                                             
C                                                                       
C               NUM(INDEX(I)), I = 1,2,...,N                            
C                                                                       
C         IS SORTED ACCORDING TO THE SETTING OF MODE. IF MODE           
C         IS 0, INDEX IS NOT REFERENCED.                                
C                                                                       
C       LAST IS AN INTEGER OUTPUT ARRAY OF LENGTH NMAX + 1. THE         
C         INDEX OF NUM FOR THE LAST OCCURRENCE OF L IS LAST(L+1)        
C         FOR ANY L = 0,1,...,NMAX UNLESS LAST(L+1) = 0. IN             
C         THIS CASE L DOES NOT APPEAR IN NUM.                           
C                                                                       
C       NEXT IS AN INTEGER OUTPUT ARRAY OF LENGTH N. IF                 
C         NUM(K) = L, THEN THE INDEX OF NUM FOR THE PREVIOUS            
C         OCCURRENCE OF L IS NEXT(K) FOR ANY L = 0,1,...,NMAX           
C         UNLESS NEXT(K) = 0. IN THIS CASE THERE IS NO PREVIOUS         
C         OCCURRENCE OF L IN NUM.                                       
C                                                                       
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1982.          
C     THOMAS F. COLEMAN, BURTON S. GARBOW, JORGE J. MORE                
C                                                                       
C     **********                                                        
      INTEGER I,J,JP,K,L,NMAXP1,NMAXP2                                  
C                                                                       
C     DETERMINE THE ARRAYS NEXT AND LAST.                               
C                                                                       
      NMAXP1 = NMAX + 1                                                 
      DO 10 I = 1, NMAXP1                                               
         LAST(I) = 0                                                    
   10    CONTINUE                                                       
      DO 20 K = 1, N                                                    
         L = NUM(K)                                                     
         NEXT(K) = LAST(L+1)                                            
         LAST(L+1) = K                                                  
   20    CONTINUE                                                       
      IF (MODE .EQ. 0) GO TO 60                                         
C                                                                       
C     STORE THE POINTERS TO THE SORTED ARRAY IN INDEX.                  
C                                                                       
      I = 1                                                             
      NMAXP2 = NMAXP1 + 1                                               
      DO 50 J = 1, NMAXP1                                               
         JP = J                                                         
         IF (MODE .LT. 0) JP = NMAXP2 - J                               
         K = LAST(JP)                                                   
   30    CONTINUE                                                       
            IF (K .EQ. 0) GO TO 40                                      
            INDEX(I) = K                                                
            I = I + 1                                                   
            K = NEXT(K)                                                 
            GO TO 30                                                    
   40    CONTINUE                                                       
   50    CONTINUE                                                       
   60 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C     LAST CARD OF SUBROUTINE N7MSRT.                                   
C                                                                       
      END                                                               
      SUBROUTINE S7ETR(M,N,INDROW,JPNTR,INDCOL,IPNTR,IWA)               
      INTEGER M,N                                                       
      INTEGER INDROW(1),JPNTR(1),INDCOL(1),IPNTR(1),IWA(M)              
C     **********                                                        
C                                                                       
C     SUBROUTINE S7ETR                                                  
C                                                                       
C     GIVEN A COLUMN-ORIENTED DEFINITION OF THE SPARSITY PATTERN        
C     OF AN M BY N MATRIX A, THIS SUBROUTINE DETERMINES A               
C     ROW-ORIENTED DEFINITION OF THE SPARSITY PATTERN OF A.             
C                                                                       
C     ON INPUT THE COLUMN-ORIENTED DEFINITION IS SPECIFIED BY           
C     THE ARRAYS INDROW AND JPNTR. ON OUTPUT THE ROW-ORIENTED           
C     DEFINITION IS SPECIFIED BY THE ARRAYS INDCOL AND IPNTR.           
C                                                                       
C     THE SUBROUTINE STATEMENT IS                                       
C                                                                       
C       SUBROUTINE S7ETR(M,N,INDROW,JPNTR,INDCOL,IPNTR,IWA)             
C                                                                       
C     WHERE                                                             
C                                                                       
C       M IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        
C         OF ROWS OF A.                                                 
C                                                                       
C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        
C         OF COLUMNS OF A.                                              
C                                                                       
C       INDROW IS AN INTEGER INPUT ARRAY WHICH CONTAINS THE ROW         
C         INDICES FOR THE NON-ZEROES IN THE MATRIX A.                   
C                                                                       
C       JPNTR IS AN INTEGER INPUT ARRAY OF LENGTH N + 1 WHICH           
C         SPECIFIES THE LOCATIONS OF THE ROW INDICES IN INDROW.         
C         THE ROW INDICES FOR COLUMN J ARE                              
C                                                                       
C               INDROW(K), K = JPNTR(J),...,JPNTR(J+1)-1.               
C                                                                       
C         NOTE THAT JPNTR(N+1)-1 IS THEN THE NUMBER OF NON-ZERO         
C         ELEMENTS OF THE MATRIX A.                                     
C                                                                       
C       INDCOL IS AN INTEGER OUTPUT ARRAY WHICH CONTAINS THE            
C         COLUMN INDICES FOR THE NON-ZEROES IN THE MATRIX A.            
C                                                                       
C       IPNTR IS AN INTEGER OUTPUT ARRAY OF LENGTH M + 1 WHICH          
C         SPECIFIES THE LOCATIONS OF THE COLUMN INDICES IN INDCOL.      
C         THE COLUMN INDICES FOR ROW I ARE                              
C                                                                       
C               INDCOL(K), K = IPNTR(I),...,IPNTR(I+1)-1.               
C                                                                       
C         NOTE THAT IPNTR(1) IS SET TO 1 AND THAT IPNTR(M+1)-1 IS       
C         THEN THE NUMBER OF NON-ZERO ELEMENTS OF THE MATRIX A.         
C                                                                       
C       IWA IS AN INTEGER WORK ARRAY OF LENGTH M.                       
C                                                                       
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1982.          
C     THOMAS F. COLEMAN, BURTON S. GARBOW, JORGE J. MORE                
C                                                                       
C     **********                                                        
      INTEGER IR,JCOL,JP,JPL,JPU,L,NNZ                                  
C                                                                       
C     DETERMINE THE NUMBER OF NON-ZEROES IN THE ROWS.                   
C                                                                       
      DO 10 IR = 1, M                                                   
         IWA(IR) = 0                                                    
   10    CONTINUE                                                       
      NNZ = JPNTR(N+1) - 1                                              
      DO 20 JP = 1, NNZ                                                 
         IR = INDROW(JP)                                                
         IWA(IR) = IWA(IR) + 1                                          
   20    CONTINUE                                                       
C                                                                       
C     SET POINTERS TO THE START OF THE ROWS IN INDCOL.                  
C                                                                       
      IPNTR(1) = 1                                                      
      DO 30 IR = 1, M                                                   
         IPNTR(IR+1) = IPNTR(IR) + IWA(IR)                              
         IWA(IR) = IPNTR(IR)                                            
   30    CONTINUE                                                       
C                                                                       
C     FILL INDCOL.                                                      
C                                                                       
      DO 60 JCOL = 1, N                                                 
         JPL = JPNTR(JCOL)                                              
         JPU = JPNTR(JCOL+1) - 1                                        
         IF (JPU .LT. JPL) GO TO 50                                     
         DO 40 JP = JPL, JPU                                            
            IR = INDROW(JP)                                             
            L = IWA(IR)                                                 
            INDCOL(L) = JCOL                                            
            IWA(IR) = IWA(IR) + 1                                       
   40       CONTINUE                                                    
   50    CONTINUE                                                       
   60    CONTINUE                                                       
      RETURN                                                            
C                                                                       
C     LAST CARD OF SUBROUTINE S7ETR.                                    
C                                                                       
      END                                                               
      SUBROUTINE S7RTDT(N,NNZ,INDROW,INDCOL,JPNTR,IWA)                  
      INTEGER N,NNZ                                                     
      INTEGER INDROW(NNZ),INDCOL(NNZ),JPNTR(1),IWA(N)                   
C     **********                                                        
C                                                                       
C     SUBROUTINE S7RTDT                                                 
C                                                                       
C     GIVEN THE NON-ZERO ELEMENTS OF AN M BY N MATRIX A IN              
C     ARBITRARY ORDER AS SPECIFIED BY THEIR ROW AND COLUMN              
C     INDICES, THIS SUBROUTINE PERMUTES THESE ELEMENTS SO               
C     THAT THEIR COLUMN INDICES ARE IN NON-DECREASING ORDER.            
C                                                                       
C     ON INPUT IT IS ASSUMED THAT THE ELEMENTS ARE SPECIFIED IN         
C                                                                       
C           INDROW(K),INDCOL(K), K = 1,...,NNZ.                         
C                                                                       
C     ON OUTPUT THE ELEMENTS ARE PERMUTED SO THAT INDCOL IS             
C     IN NON-DECREASING ORDER. IN ADDITION, THE ARRAY JPNTR             
C     IS SET SO THAT THE ROW INDICES FOR COLUMN J ARE                   
C                                                                       
C           INDROW(K), K = JPNTR(J),...,JPNTR(J+1)-1.                   
C                                                                       
C     NOTE THAT THE VALUE OF M IS NOT NEEDED BY S7RTDT AND IS           
C     THEREFORE NOT PRESENT IN THE SUBROUTINE STATEMENT.                
C                                                                       
C     THE SUBROUTINE STATEMENT IS                                       
C                                                                       
C       SUBROUTINE S7RTDT(N,NNZ,INDROW,INDCOL,JPNTR,IWA)                
C                                                                       
C     WHERE                                                             
C                                                                       
C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        
C         OF COLUMNS OF A.                                              
C                                                                       
C       NNZ IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER      
C         OF NON-ZERO ELEMENTS OF A.                                    
C                                                                       
C       INDROW IS AN INTEGER ARRAY OF LENGTH NNZ. ON INPUT INDROW       
C         MUST CONTAIN THE ROW INDICES OF THE NON-ZERO ELEMENTS OF A.   
C         ON OUTPUT INDROW IS PERMUTED SO THAT THE CORRESPONDING        
C         COLUMN INDICES OF INDCOL ARE IN NON-DECREASING ORDER.         
C                                                                       
C       INDCOL IS AN INTEGER ARRAY OF LENGTH NNZ. ON INPUT INDCOL       
C         MUST CONTAIN THE COLUMN INDICES OF THE NON-ZERO ELEMENTS      
C         OF A. ON OUTPUT INDCOL IS PERMUTED SO THAT THESE INDICES      
C         ARE IN NON-DECREASING ORDER.                                  
C                                                                       
C       JPNTR IS AN INTEGER OUTPUT ARRAY OF LENGTH N + 1 WHICH          
C         SPECIFIES THE LOCATIONS OF THE ROW INDICES IN THE OUTPUT      
C         INDROW. THE ROW INDICES FOR COLUMN J ARE                      
C                                                                       
C               INDROW(K), K = JPNTR(J),...,JPNTR(J+1)-1.               
C                                                                       
C         NOTE THAT JPNTR(1) IS SET TO 1 AND THAT JPNTR(N+1)-1          
C         IS THEN NNZ.                                                  
C                                                                       
C       IWA IS AN INTEGER WORK ARRAY OF LENGTH N.                       
C                                                                       
C     SUBPROGRAMS CALLED                                                
C                                                                       
C       FORTRAN-SUPPLIED ... MAX0                                       
C                                                                       
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1982.          
C     THOMAS F. COLEMAN, BURTON S. GARBOW, JORGE J. MORE                
C                                                                       
C     **********                                                        
      INTEGER I,J,K,L                                                   
C                                                                       
C     DETERMINE THE NUMBER OF NON-ZEROES IN THE COLUMNS.                
C                                                                       
      DO 10 J = 1, N                                                    
         IWA(J) = 0                                                     
   10    CONTINUE                                                       
      DO 20 K = 1, NNZ                                                  
         J = INDCOL(K)                                                  
         IWA(J) = IWA(J) + 1                                            
   20    CONTINUE                                                       
C                                                                       
C     SET POINTERS TO THE START OF THE COLUMNS IN INDROW.               
C                                                                       
      JPNTR(1) = 1                                                      
      DO 30 J = 1, N                                                    
         JPNTR(J+1) = JPNTR(J) + IWA(J)                                 
         IWA(J) = JPNTR(J)                                              
   30    CONTINUE                                                       
      K = 1                                                             
C                                                                       
C     BEGIN IN-PLACE SORT.                                              
C                                                                       
   40 CONTINUE                                                          
         J = INDCOL(K)                                                  
         IF (K .LT. JPNTR(J) .OR. K .GE. JPNTR(J+1)) GO TO 50           
C                                                                       
C           CURRENT ELEMENT IS IN POSITION. NOW EXAMINE THE             
C           NEXT ELEMENT OR THE FIRST UN-SORTED ELEMENT IN              
C           THE J-TH GROUP.                                             
C                                                                       
            K = MAX0(K+1,IWA(J))                                        
            GO TO 60                                                    
   50    CONTINUE                                                       
C                                                                       
C           CURRENT ELEMENT IS NOT IN POSITION. PLACE ELEMENT           
C           IN POSITION AND MAKE THE DISPLACED ELEMENT THE              
C           CURRENT ELEMENT.                                            
C                                                                       
            L = IWA(J)                                                  
            IWA(J) = IWA(J) + 1                                         
            I = INDROW(K)                                               
            INDROW(K) = INDROW(L)                                       
            INDCOL(K) = INDCOL(L)                                       
            INDROW(L) = I                                               
            INDCOL(L) = J                                               
   60    CONTINUE                                                       
         IF (K .LE. NNZ) GO TO 40                                       
      RETURN                                                            
C                                                                       
C     LAST CARD OF SUBROUTINE S7RTDT.                                   
C                                                                       
      END                                                               
      LOGICAL FUNCTION STOPX(IDUMMY)                                    
C     *****PARAMETERS...                                                
      INTEGER IDUMMY                                                    
C                                                                       
C     ..................................................................
C                                                                       
C     *****PURPOSE...                                                   
C     THIS FUNCTION MAY SERVE AS THE STOPX (ASYNCHRONOUS INTERRUPTION)  
C     FUNCTION FOR THE NL2SOL (NONLINEAR LEAST-SQUARES) PACKAGE AT      
C     THOSE INSTALLATIONS WHICH DO NOT WISH TO IMPLEMENT A              
C     DYNAMIC STOPX.                                                    
C                                                                       
C     *****ALGORITHM NOTES...                                           
C     AT INSTALLATIONS WHERE THE NL2SOL SYSTEM IS USED                  
C     INTERACTIVELY, THIS DUMMY STOPX SHOULD BE REPLACED BY A           
C     FUNCTION THAT RETURNS .TRUE. IF AND ONLY IF THE INTERRUPT         
C     (BREAK) KEY HAS BEEN PRESSED SINCE THE LAST CALL ON STOPX.        
C                                                                       
C     ..................................................................
C                                                                       
      STOPX = .FALSE.                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE D7EGR(N,INDROW,JPNTR,INDCOL,IPNTR,NDEG,IWA,BWA)        
      INTEGER N                                                         
      INTEGER INDROW(1),JPNTR(1),INDCOL(1),IPNTR(1),NDEG(N),IWA(N)      
      LOGICAL BWA(N)                                                    
C     **********                                                        
C                                                                       
C     SUBROUTINE D7EGR                                                  
C                                                                       
C     GIVEN THE SPARSITY PATTERN OF AN M BY N MATRIX A,                 
C     THIS SUBROUTINE DETERMINES THE DEGREE SEQUENCE FOR                
C     THE INTERSECTION GRAPH OF THE COLUMNS OF A.                       
C                                                                       
C     IN GRAPH-THEORY TERMINOLOGY, THE INTERSECTION GRAPH OF            
C     THE COLUMNS OF A IS THE LOOPLESS GRAPH G WITH VERTICES            
C     A(J), J = 1,2,...,N WHERE A(J) IS THE J-TH COLUMN OF A            
C     AND WITH EDGE (A(I),A(J)) IF AND ONLY IF COLUMNS I AND J          
C     HAVE A NON-ZERO IN THE SAME ROW POSITION.                         
C                                                                       
C     NOTE THAT THE VALUE OF M IS NOT NEEDED BY D7EGR AND IS            
C     THEREFORE NOT PRESENT IN THE SUBROUTINE STATEMENT.                
C                                                                       
C     THE SUBROUTINE STATEMENT IS                                       
C                                                                       
C       SUBROUTINE D7EGR(N,INDROW,JPNTR,INDCOL,IPNTR,NDEG,IWA,BWA)      
C                                                                       
C     WHERE                                                             
C                                                                       
C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        
C         OF COLUMNS OF A.                                              
C                                                                       
C       INDROW IS AN INTEGER INPUT ARRAY WHICH CONTAINS THE ROW         
C         INDICES FOR THE NON-ZEROES IN THE MATRIX A.                   
C                                                                       
C       JPNTR IS AN INTEGER INPUT ARRAY OF LENGTH N + 1 WHICH           
C         SPECIFIES THE LOCATIONS OF THE ROW INDICES IN INDROW.         
C         THE ROW INDICES FOR COLUMN J ARE                              
C                                                                       
C               INDROW(K), K = JPNTR(J),...,JPNTR(J+1)-1.               
C                                                                       
C         NOTE THAT JPNTR(N+1)-1 IS THEN THE NUMBER OF NON-ZERO         
C         ELEMENTS OF THE MATRIX A.                                     
C                                                                       
C       INDCOL IS AN INTEGER INPUT ARRAY WHICH CONTAINS THE             
C         COLUMN INDICES FOR THE NON-ZEROES IN THE MATRIX A.            
C                                                                       
C       IPNTR IS AN INTEGER INPUT ARRAY OF LENGTH M + 1 WHICH           
C         SPECIFIES THE LOCATIONS OF THE COLUMN INDICES IN INDCOL.      
C         THE COLUMN INDICES FOR ROW I ARE                              
C                                                                       
C               INDCOL(K), K = IPNTR(I),...,IPNTR(I+1)-1.               
C                                                                       
C         NOTE THAT IPNTR(M+1)-1 IS THEN THE NUMBER OF NON-ZERO         
C         ELEMENTS OF THE MATRIX A.                                     
C                                                                       
C       NDEG IS AN INTEGER OUTPUT ARRAY OF LENGTH N WHICH               
C         SPECIFIES THE DEGREE SEQUENCE. THE DEGREE OF THE              
C         J-TH COLUMN OF A IS NDEG(J).                                  
C                                                                       
C       IWA IS AN INTEGER WORK ARRAY OF LENGTH N.                       
C                                                                       
C       BWA IS A LOGICAL WORK ARRAY OF LENGTH N.                        
C                                                                       
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1982.          
C     THOMAS F. COLEMAN, BURTON S. GARBOW, JORGE J. MORE                
C                                                                       
C     **********                                                        
      INTEGER DEG,IC,IP,IPL,IPU,IR,JCOL,JP,JPL,JPU                      
C                                                                       
C     INITIALIZATION BLOCK.                                             
C                                                                       
      DO 10 JP = 1, N                                                   
         NDEG(JP) = 0                                                   
         BWA(JP) = .FALSE.                                              
   10    CONTINUE                                                       
C                                                                       
C     COMPUTE THE DEGREE SEQUENCE BY DETERMINING THE CONTRIBUTIONS      
C     TO THE DEGREES FROM THE CURRENT(JCOL) COLUMN AND FURTHER          
C     COLUMNS WHICH HAVE NOT YET BEEN CONSIDERED.                       
C                                                                       
      IF (N .LT. 2) GO TO 90                                            
      DO 80 JCOL = 2, N                                                 
         BWA(JCOL) = .TRUE.                                             
         DEG = 0                                                        
C                                                                       
C        DETERMINE ALL POSITIONS (IR,JCOL) WHICH CORRESPOND             
C        TO NON-ZEROES IN THE MATRIX.                                   
C                                                                       
         JPL = JPNTR(JCOL)                                              
         JPU = JPNTR(JCOL+1) - 1                                        
         IF (JPU .LT. JPL) GO TO 50                                     
         DO 40 JP = JPL, JPU                                            
            IR = INDROW(JP)                                             
C                                                                       
C           FOR EACH ROW IR, DETERMINE ALL POSITIONS (IR,IC)            
C           WHICH CORRESPOND TO NON-ZEROES IN THE MATRIX.               
C                                                                       
            IPL = IPNTR(IR)                                             
            IPU = IPNTR(IR+1) - 1                                       
            DO 30 IP = IPL, IPU                                         
               IC = INDCOL(IP)                                          
C                                                                       
C              ARRAY BWA MARKS COLUMNS WHICH HAVE CONTRIBUTED TO        
C              THE DEGREE COUNT OF COLUMN JCOL. UPDATE THE DEGREE       
C              COUNTS OF THESE COLUMNS. ARRAY IWA RECORDS THE           
C              MARKED COLUMNS.                                          
C                                                                       
               IF (BWA(IC)) GO TO 20                                    
               BWA(IC) = .TRUE.                                         
               NDEG(IC) = NDEG(IC) + 1                                  
               DEG = DEG + 1                                            
               IWA(DEG) = IC                                            
   20          CONTINUE                                                 
   30          CONTINUE                                                 
   40       CONTINUE                                                    
   50    CONTINUE                                                       
C                                                                       
C        UN-MARK THE COLUMNS RECORDED BY IWA AND FINALIZE THE           
C        DEGREE COUNT OF COLUMN JCOL.                                   
C                                                                       
         IF (DEG .LT. 1) GO TO 70                                       
         DO 60 JP = 1, DEG                                              
            IC = IWA(JP)                                                
            BWA(IC) = .FALSE.                                           
   60       CONTINUE                                                    
         NDEG(JCOL) = NDEG(JCOL) + DEG                                  
   70    CONTINUE                                                       
   80    CONTINUE                                                       
   90 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C     LAST CARD OF SUBROUTINE D7EGR.                                    
C                                                                       
      END                                                               
      SUBROUTINE I7COPY(P, Y, X)                                        
C                                                                       
C  ***  SET Y = X, WHERE X AND Y ARE INTEGER P-VECTORS  ***             
C                                                                       
      INTEGER P                                                         
      INTEGER X(P), Y(P)                                                
C                                                                       
      INTEGER I                                                         
C                                                                       
      DO 10 I = 1, P                                                    
 10      Y(I) = X(I)                                                    
 999  RETURN                                                            
      END                                                               
      INTEGER FUNCTION I7MDCN(K)                                        
C                                                                       
      INTEGER K                                                         
C                                                                       
C  ***  RETURN INTEGER MACHINE-DEPENDENT CONSTANTS  ***                 
C                                                                       
C     ***  K = 1 MEANS RETURN STANDARD OUTPUT UNIT NUMBER.   ***        
C     ***  K = 2 MEANS RETURN ALTERNATE OUTPUT UNIT NUMBER.  ***        
C     ***  K = 3 MEANS RETURN  INPUT UNIT NUMBER.            ***        
C          (NOTE -- K = 2, 3 ARE USED ONLY BY TEST PROGRAMS.)           
C                                                                       
C  +++  PORT VERSION FOLLOWS...                                         
      INTEGER I1MACH                                                    
      EXTERNAL I1MACH                                                   
      INTEGER MDPERM(3)                                                 
      DATA MDPERM(1)/2/, MDPERM(2)/4/, MDPERM(3)/1/                     
      I7MDCN = I1MACH(MDPERM(K))                                        
C  +++  END OF PORT VERSION  +++                                        
C                                                                       
C  +++  NON-PORT VERSION FOLLOWS...                                     
C     INTEGER MDCON(3)                                                  
C     DATA MDCON(1)/6/, MDCON(2)/8/, MDCON(3)/5/                        
C     I7MDCN = MDCON(K)                                                 
C  +++  END OF NON-PORT VERSION  +++                                    
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF I7MDCN FOLLOWS  ***                                
      END                                                               
C****END OF ROUTINES FOR THIRD PART OF PORT 3 OPTIMIZATION CHAPTER******