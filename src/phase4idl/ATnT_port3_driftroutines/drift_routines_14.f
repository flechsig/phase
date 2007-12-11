      SUBROUTINE  L7MST(D, G, IERR, IPIVOT, KA, P, QTR, R, STEP, V, W)  
C                                                                       
C  ***  COMPUTE LEVENBERG-MARQUARDT STEP USING MORE-HEBDEN TECHNIQUE  **
C  ***  NL2SOL VERSION 2.2.  ***                                        
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER IERR, KA, P                                               
      INTEGER IPIVOT(P)                                                 
      REAL D(P), G(P), QTR(P), R(1), STEP(P), V(21), W(1)               
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
C   IERR (I/O) = RETURN CODE FROM QRFACT OR Q7RGS -- 0 MEANS R HAS      
C             FULL RANK.                                                
C IPIVOT (I/O) = PERMUTATION ARRAY FROM QRFACT OR Q7RGS, WHICH COMPUTE  
C             QR DECOMPOSITIONS WITH COLUMN PIVOTING.                   
C     KA (I/O).  KA .LT. 0 ON INPUT MEANS THIS IS THE FIRST CALL ON     
C              L7MST FOR THE CURRENT R AND QTR.  ON OUTPUT KA CON-      
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
C  D7TPR - RETURNS INNER PRODUCT OF TWO VECTORS.                        
C  L7ITV - APPLY INVERSE-TRANSPOSE OF COMPACT LOWER TRIANG. MATRIX.     
C  L7IVM - APPLY INVERSE OF COMPACT LOWER TRIANG. MATRIX.               
C V7CPY  - COPIES ONE VECTOR TO ANOTHER.                                
C  V2NRM - RETURNS 2-NORM OF A VECTOR.                                  
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
      REAL A, ADI, ALPHAK, B, DFACSQ, DST, DTOL, D1, D2,                
     1                 LK, OLDPHI, PHI, PHIMAX, PHIMIN, PSIFAC, RAD,    
     2                 SI, SJ, SQRTAK, T, TWOPSI, UK, WL                
C                                                                       
C     ***  CONSTANTS  ***                                               
      REAL DFAC, EIGHT, HALF, NEGONE, ONE, P001, THREE,                 
     1                 TTOL, ZERO                                       
      REAL BIG                                                          
C                                                                       
C  ***  INTRINSIC FUNCTIONS  ***                                        
C/+                                                                     
      REAL  SQRT                                                        
C/                                                                      
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      REAL  D7TPR,  L7SVN,  R7MDC,  V2NRM                               
      EXTERNAL  D7TPR,  L7ITV,  L7IVM,  L7SVN,  R7MDC, V7CPY,  V2NRM    
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
C     DATA DFAC/256.E+0/, EIGHT/8.E+0/, HALF/0.5E+0/, NEGONE/-1.E+0/,   
C    1     ONE/1.E+0/, P001/1.E-3/, THREE/3.E+0/, TTOL/2.5E+0/,         
C    2     ZERO/0.E+0/                                                  
C/7                                                                     
      PARAMETER (DFAC=256.E+0, EIGHT=8.E+0, HALF=0.5E+0, NEGONE=-1.E+0, 
     1     ONE=1.E+0, P001=1.E-3, THREE=3.E+0, TTOL=2.5E+0,             
     2     ZERO=0.E+0)                                                  
      SAVE BIG                                                          
C/                                                                      
      DATA BIG/0.E+0/                                                   
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
      IF (BIG .LE. ZERO) BIG =  R7MDC(6)                                
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
      V(NREDUC) = HALF* D7TPR(K, QTR, QTR)                              
C                                                                       
C  ***  SET UP TO TRY INITIAL GAUSS-NEWTON STEP  ***                    
C                                                                       
 20   V(DST0) = NEGONE                                                  
      IF (IERR .NE. 0) GO TO 90                                         
      T =  L7SVN(P, R, STEP, W(RES))                                    
      IF (T .GE. ONE) GO TO 30                                          
         IF ( V2NRM(P, QTR) .GE. BIG*T) GO TO 90                        
C                                                                       
C  ***  COMPUTE GAUSS-NEWTON STEP  ***                                  
C                                                                       
C     ***  NOTE -- THE R-MATRIX IS STORED COMPACTLY BY COLUMNS IN       
C     ***  R(1), R(2), R(3), ...  IT IS THE TRANSPOSE OF A              
C     ***  LOWER TRIANGULAR MATRIX STORED COMPACTLY BY ROWS, AND WE     
C     ***  TREAT IT AS SUCH WHEN USING  L7ITV AND  L7IVM.               
 30   CALL  L7ITV(P, W, R, QTR)                                         
C     ***  TEMPORARILY STORE PERMUTED -D*STEP IN STEP.                  
      DO 60 I = 1, P                                                    
         J1 = IPIVOT(I)                                                 
         STEP(I) = D(J1)*W(I)                                           
 60      CONTINUE                                                       
      DST =  V2NRM(P, STEP)                                             
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
      CALL  L7IVM(P, STEP, R, STEP)                                     
      T = ONE /  V2NRM(P, STEP)                                         
      W(PHIPIN) = (T/RAD)*T                                             
      LK = PHI*W(PHIPIN)                                                
C                                                                       
C  ***  COMPUTE U0  ***                                                 
C                                                                       
 90   DO 100 I = 1, P                                                   
 100     W(I) = G(I)/D(I)                                               
      V(DGNORM) =  V2NRM(P, W)                                          
      UK = V(DGNORM)/RAD                                                
      IF (UK .LE. ZERO) GO TO 390                                       
C                                                                       
C     ***  ALPHAK WILL BE USED AS THE CURRENT MARQUARDT PARAMETER.  WE  
C     ***  USE MORE*S SCHEME FOR INITIALIZING IT.                       
C                                                                       
      ALPHAK =  ABS(V(STPPAR)) * V(RAD0)/RAD                            
      ALPHAK = AMIN1(UK, AMAX1(ALPHAK, LK))                             
C                                                                       
C                                                                       
C  ***  TOP OF LOOP -- INCREMENT KA, COPY R TO RMAT, QTR TO RES  ***    
C                                                                       
 110  KA = KA + 1                                                       
      CALL V7CPY(PP1O2, W(RMAT), R)                                     
      CALL V7CPY(P, W(RES), QTR)                                        
C                                                                       
C  ***  SAFEGUARD ALPHAK AND INITIALIZE FAST GIVENS SCALE VECTOR.  ***  
C                                                                       
      IF (ALPHAK .LE. ZERO .OR. ALPHAK .LT. LK .OR. ALPHAK .GE. UK)     
     1             ALPHAK = UK * AMAX1(P001,  SQRT(LK/UK))              
      IF (ALPHAK .LE. ZERO) ALPHAK = HALF * UK                          
      SQRTAK =  SQRT(ALPHAK)                                            
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
         IF (ADI .GE.  ABS(WL)) GO TO 150                               
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
 190          IF ( ABS(SI) .GT.  ABS(WL)) GO TO 220                     
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
 280  CALL  L7ITV(P, W(RES), W(RMAT), W(RES))                           
C     ***  RECOVER STEP AND STORE PERMUTED -D*STEP AT W(RES)  ***       
      DO 290 I = 1, P                                                   
         J1 = IPIVOT(I)                                                 
         K = RES0 + I                                                   
         T = W(K)                                                       
         STEP(J1) = -T                                                  
         W(K) = T*D(J1)                                                 
 290     CONTINUE                                                       
      DST =  V2NRM(P, W(RES))                                           
      PHI = DST - RAD                                                   
      IF (PHI .LE. PHIMAX .AND. PHI .GE. PHIMIN) GO TO 430              
      IF (OLDPHI .EQ. PHI) GO TO 430                                    
      OLDPHI = PHI                                                      
C                                                                       
C  ***  CHECK FOR (AND HANDLE) SPECIAL CASE  ***                        
C                                                                       
      IF (PHI .GT. ZERO) GO TO 310                                      
         IF (KA .GE. KALIM) GO TO 430                                   
              TWOPSI = ALPHAK*DST*DST -  D7TPR(P, STEP, G)              
              IF (ALPHAK .GE. TWOPSI*PSIFAC) GO TO 310                  
                   V(STPPAR) = -ALPHAK                                  
                   GO TO 440                                            
C                                                                       
C  ***  UNACCEPTABLE STEP -- UPDATE LK, UK, ALPHAK, AND TRY AGAIN  ***  
C                                                                       
 300  IF (PHI .LT. ZERO) UK = AMIN1(UK, ALPHAK)                         
      GO TO 320                                                         
 310  IF (PHI .LT. ZERO) UK = ALPHAK                                    
 320  DO 330 I = 1, P                                                   
         J1 = IPIVOT(I)                                                 
         K = RES0 + I                                                   
         STEP(I) = D(J1) * (W(K)/DST)                                   
 330     CONTINUE                                                       
      CALL  L7IVM(P, STEP, W(RMAT), STEP)                               
      DO 340 I = 1, P                                                   
 340     STEP(I) = STEP(I) /  SQRT(W(I))                                
      T = ONE /  V2NRM(P, STEP)                                         
      ALPHAK = ALPHAK + T*PHI*T/RAD                                     
      LK = AMAX1(LK, ALPHAK)                                            
      ALPHAK = LK                                                       
      GO TO 110                                                         
C                                                                       
C  ***  RESTART  ***                                                    
C                                                                       
 370  LK = W(LK0)                                                       
      UK = W(UK0)                                                       
      IF (V(DST0) .GT. ZERO .AND. V(DST0) - RAD .LE. PHIMAX) GO TO 20   
      ALPHAK =  ABS(V(STPPAR))                                          
      DST = W(DSTSAV)                                                   
      PHI = DST - RAD                                                   
      T = V(DGNORM)/RAD                                                 
      IF (RAD .GT. V(RAD0)) GO TO 380                                   
C                                                                       
C        ***  SMALLER RADIUS  ***                                       
         UK = T                                                         
         IF (ALPHAK .LE. ZERO) LK = ZERO                                
         IF (V(DST0) .GT. ZERO) LK = AMAX1(LK, (V(DST0)-RAD)*W(PHIPIN)) 
         GO TO 300                                                      
C                                                                       
C     ***  BIGGER RADIUS  ***                                           
 380  IF (ALPHAK .LE. ZERO .OR. UK .GT. T) UK = T                       
      LK = ZERO                                                         
      IF (V(DST0) .GT. ZERO) LK = AMAX1(LK, (V(DST0)-RAD)*W(PHIPIN))    
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
 440  V(GTSTEP) = AMIN1( D7TPR(P,STEP,G), ZERO)                         
      V(PREDUC) = HALF * (ALPHAK*DST*DST - V(GTSTEP))                   
 450  V(DSTNRM) = DST                                                   
      W(DSTSAV) = DST                                                   
      W(LK0) = LK                                                       
      W(UK0) = UK                                                       
      V(RAD0) = RAD                                                     
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF  L7MST FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  L7NVR(N, LIN, L)                                      
C                                                                       
C  ***  COMPUTE  LIN = L**-1,  BOTH  N X N  LOWER TRIANG. STORED   ***  
C  ***  COMPACTLY BY ROWS.  LIN AND L MAY SHARE THE SAME STORAGE.  ***  
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
      INTEGER N                                                         
      REAL L(1), LIN(1)                                                 
C     DIMENSION L(N*(N+1)/2), LIN(N*(N+1)/2)                            
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, II, IM1, JJ, J0, J1, K, K0, NP1                        
      REAL ONE, T, ZERO                                                 
C/6                                                                     
C     DATA ONE/1.E+0/, ZERO/0.E+0/                                      
C/7                                                                     
      PARAMETER (ONE=1.E+0, ZERO=0.E+0)                                 
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
C  ***  LAST CARD OF  L7NVR FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  L7SQR(N, A, L)                                        
C                                                                       
C  ***  COMPUTE  A = LOWER TRIANGLE OF  L*(L**T),  WITH BOTH            
C  ***  L  AND  A  STORED COMPACTLY BY ROWS.  (BOTH MAY OCCUPY THE      
C  ***  SAME STORAGE.                                                   
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
      INTEGER N                                                         
      REAL A(1), L(1)                                                   
C     DIMENSION A(N*(N+1)/2), L(N*(N+1)/2)                              
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, II, IJ, IK, IP1, I0, J, JJ, JK, J0, K, NP1             
      REAL T                                                            
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
              T = 0.0E0                                                 
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
      SUBROUTINE L7SRT(N1, N, L, A, IRC)                                
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
      REAL L(1), A(1)                                                   
C     DIMENSION L(N*(N+1)/2), A(N*(N+1)/2)                              
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, IJ, IK, IM1, I0, J, JK, JM1, J0, K                     
      REAL T, TD, ZERO                                                  
C                                                                       
C  ***  INTRINSIC FUNCTIONS  ***                                        
C/+                                                                     
      REAL  SQRT                                                        
C/                                                                      
C/6                                                                     
C     DATA ZERO/0.E+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.E+0)                                            
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
         L(I0) =  SQRT(T)                                               
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
C  ***  LAST CARD OF L7SRT  ***                                         
      END                                                               
      REAL FUNCTION  L7SVN(P, L, X, Y)                                  
C                                                                       
C  ***  ESTIMATE SMALLEST SING. VALUE OF PACKED LOWER TRIANG. MATRIX L  
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER P                                                         
      REAL L(1), X(P), Y(P)                                             
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
C  X (OUT) IF  L7SVN RETURNS A POSITIVE VALUE, THEN X IS A NORMALIZED   
C             APPROXIMATE LEFT SINGULAR VECTOR CORRESPONDING TO THE     
C             SMALLEST SINGULAR VALUE.  THIS APPROXIMATION MAY BE VERY  
C             CRUDE.  IF  L7SVN RETURNS ZERO, THEN SOME COMPONENTS OF X 
C             ARE ZERO AND THE REST RETAIN THEIR INPUT VALUES.          
C  Y (OUT) IF  L7SVN RETURNS A POSITIVE VALUE, THEN Y = (L**-1)*X IS AN 
C             UNNORMALIZED APPROXIMATE RIGHT SINGULAR VECTOR CORRESPOND-
C             ING TO THE SMALLEST SINGULAR VALUE.  THIS APPROXIMATION   
C             MAY BE CRUDE.  IF  L7SVN RETURNS ZERO, THEN Y RETAINS ITS 
C             INPUT VALUE.  THE CALLER MAY PASS THE SAME VECTOR FOR X   
C             AND Y (NONSTANDARD FORTRAN USAGE), IN WHICH CASE Y OVER-  
C             WRITES X (FOR NONZERO  L7SVN RETURNS).                    
C                                                                       
C  ***  ALGORITHM NOTES  ***                                            
C                                                                       
C     THE ALGORITHM IS BASED ON (1), WITH THE ADDITIONAL PROVISION THAT 
C      L7SVN = 0 IS RETURNED IF THE SMALLEST DIAGONAL ELEMENT OF L      
C     (IN MAGNITUDE) IS NOT MORE THAN THE UNIT ROUNDOFF TIMES THE       
C     LARGEST.  THE ALGORITHM USES A RANDOM NUMBER GENERATOR PROPOSED   
C     IN (4), WHICH PASSES THE SPECTRAL TEST WITH FLYING COLORS -- SEE  
C     (2) AND (3).                                                      
C                                                                       
C  ***  SUBROUTINES AND FUNCTIONS CALLED  ***                           
C                                                                       
C         V2NRM - FUNCTION, RETURNS THE 2-NORM OF A VECTOR.             
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
      REAL B, SMINUS, SPLUS, T, XMINUS, XPLUS                           
C                                                                       
C  ***  CONSTANTS  ***                                                  
C                                                                       
      REAL HALF, ONE, R9973, ZERO                                       
C                                                                       
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      REAL  D7TPR,  V2NRM                                               
      EXTERNAL  D7TPR,  V2NRM, V2AXY                                    
C                                                                       
C/6                                                                     
C     DATA HALF/0.5E+0/, ONE/1.E+0/, R9973/9973.E+0/, ZERO/0.E+0/       
C/7                                                                     
      PARAMETER (HALF=0.5E+0, ONE=1.E+0, R9973=9973.E+0, ZERO=0.E+0)    
C/                                                                      
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
      IX = 2                                                            
      PM1 = P - 1                                                       
C                                                                       
C  ***  FIRST CHECK WHETHER TO RETURN  L7SVN = 0 AND INITIALIZE X  ***  
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
         SPLUS =  ABS(XPLUS)                                            
         SMINUS =  ABS(XMINUS)                                          
         JM1 = J - 1                                                    
         J0 = J*JM1/2                                                   
         JJ = J0 + J                                                    
         XPLUS = XPLUS/L(JJ)                                            
         XMINUS = XMINUS/L(JJ)                                          
         IF (JM1 .EQ. 0) GO TO 30                                       
         DO 20 I = 1, JM1                                               
              JI = J0 + I                                               
              SPLUS = SPLUS +  ABS(X(I) + L(JI)*XPLUS)                  
              SMINUS = SMINUS +  ABS(X(I) + L(JI)*XMINUS)               
 20           CONTINUE                                                  
 30      IF (SMINUS .GT. SPLUS) XPLUS = XMINUS                          
         X(J) = XPLUS                                                   
C       ***  UPDATE PARTIAL SUMS  ***                                   
         IF (JM1 .GT. 0) CALL V2AXY(JM1, X, XPLUS, L(J0+1), X)          
 50      CONTINUE                                                       
C                                                                       
C  ***  NORMALIZE X  ***                                                
C                                                                       
 60   T = ONE/ V2NRM(P, X)                                              
      DO 70 I = 1, P                                                    
 70      X(I) = T*X(I)                                                  
C                                                                       
C  ***  SOLVE L*Y = X AND RETURN  L7SVN = 1/TWONORM(Y)  ***             
C                                                                       
      DO 100 J = 1, P                                                   
         JM1 = J - 1                                                    
         J0 = J*JM1/2                                                   
         JJ = J0 + J                                                    
         T = ZERO                                                       
         IF (JM1 .GT. 0) T =  D7TPR(JM1, L(J0+1), Y)                    
         Y(J) = (X(J) - T) / L(JJ)                                      
 100     CONTINUE                                                       
C                                                                       
       L7SVN = ONE/ V2NRM(P, Y)                                         
      GO TO 999                                                         
C                                                                       
 110   L7SVN = ZERO                                                     
 999  RETURN                                                            
C  ***  LAST CARD OF  L7SVN FOLLOWS  ***                                
      END                                                               
      REAL FUNCTION  L7SVX(P, L, X, Y)                                  
C                                                                       
C  ***  ESTIMATE LARGEST SING. VALUE OF PACKED LOWER TRIANG. MATRIX L   
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER P                                                         
      REAL L(1), X(P), Y(P)                                             
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
C  X (OUT) IF  L7SVX RETURNS A POSITIVE VALUE, THEN X = (L**T)*Y IS AN  
C             (UNNORMALIZED) APPROXIMATE RIGHT SINGULAR VECTOR          
C             CORRESPONDING TO THE LARGEST SINGULAR VALUE.  THIS        
C             APPROXIMATION MAY BE CRUDE.                               
C  Y (OUT) IF  L7SVX RETURNS A POSITIVE VALUE, THEN Y = L*X IS A        
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
C         V2NRM - FUNCTION, RETURNS THE 2-NORM OF A VECTOR.             
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
      REAL B, BLJI, SMINUS, SPLUS, T, YI                                
C                                                                       
C  ***  CONSTANTS  ***                                                  
C                                                                       
      REAL HALF, ONE, R9973, ZERO                                       
C                                                                       
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      REAL  D7TPR,  V2NRM                                               
      EXTERNAL  D7TPR,  V2NRM, V2AXY                                    
C                                                                       
C/6                                                                     
C     DATA HALF/0.5E+0/, ONE/1.E+0/, R9973/9973.E+0/, ZERO/0.E+0/       
C/7                                                                     
      PARAMETER (HALF=0.5E+0, ONE=1.E+0, R9973=9973.E+0, ZERO=0.E+0)    
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
              SPLUS = SPLUS +  ABS(BLJI + X(I))                         
              SMINUS = SMINUS +  ABS(BLJI - X(I))                       
 20           CONTINUE                                                  
         IF (SMINUS .GT. SPLUS) B = -B                                  
         X(J) = ZERO                                                    
C        ***  UPDATE PARTIAL SUMS  ***                                  
         CALL V2AXY(J, X, B, L(J0+1), X)                                
 30      CONTINUE                                                       
C                                                                       
C  ***  NORMALIZE X  ***                                                
C                                                                       
 40   T =  V2NRM(P, X)                                                  
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
         Y(J) =  D7TPR(J, L(JI), X)                                     
 60      CONTINUE                                                       
C                                                                       
C  ***  NORMALIZE Y AND SET X = (L**T)*Y  ***                           
C                                                                       
      T = ONE /  V2NRM(P, Y)                                            
      JI = 1                                                            
      DO 70 I = 1, P                                                    
         YI = T * Y(I)                                                  
         X(I) = ZERO                                                    
         CALL V2AXY(I, X, YI, L(JI), X)                                 
         JI = JI + I                                                    
 70      CONTINUE                                                       
       L7SVX =  V2NRM(P, X)                                             
      GO TO 999                                                         
C                                                                       
 80    L7SVX = ZERO                                                     
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF  L7SVX FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  L7TSQ(N, A, L)                                        
C                                                                       
C  ***  SET A TO LOWER TRIANGLE OF (L**T) * L  ***                      
C                                                                       
C  ***  L = N X N LOWER TRIANG. MATRIX STORED ROWWISE.  ***             
C  ***  A IS ALSO STORED ROWWISE AND MAY SHARE STORAGE WITH L.  ***     
C                                                                       
      INTEGER N                                                         
      REAL A(1), L(1)                                                   
C     DIMENSION A(N*(N+1)/2), L(N*(N+1)/2)                              
C                                                                       
      INTEGER I, II, IIM1, I1, J, K, M                                  
      REAL LII, LJ                                                      
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
C  ***  LAST CARD OF  L7TSQ FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  L7UPD(BETA, GAMMA, L, LAMBDA, LPLUS, N, W, Z)         
C                                                                       
C  ***  COMPUTE LPLUS = SECANT UPDATE OF L  ***                         
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER N                                                         
      REAL BETA(N), GAMMA(N), L(1), LAMBDA(N), LPLUS(1),                
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
      REAL  SQRT                                                        
C/                                                                      
C--------------------------  LOCAL VARIABLES  --------------------------
C                                                                       
      INTEGER I, IJ, J, JJ, JP1, K, NM1, NP1                            
      REAL A, B, BJ, ETA, GJ, LJ, LIJ, LJJ, NU, S, THETA,               
     1                 WJ, ZJ                                           
      REAL ONE, ZERO                                                    
C                                                                       
C  ***  DATA INITIALIZATIONS  ***                                       
C                                                                       
C/6                                                                     
C     DATA ONE/1.E+0/, ZERO/0.E+0/                                      
C/7                                                                     
      PARAMETER (ONE=1.E+0, ZERO=0.E+0)                                 
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
         LJ =  SQRT(THETA**2 + A*S)                                     
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
C  ***  LAST CARD OF  L7UPD FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  N2CVP(IV, LIV, LV, P, V)                              
C                                                                       
C  ***  PRINT COVARIANCE MATRIX FOR   RN2G  ***                         
C                                                                       
      INTEGER LIV, LV, P                                                
      INTEGER IV(LIV)                                                   
      REAL V(LV)                                                        
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER COV1, I, II, I1, J, PU                                    
      REAL T                                                            
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
 40   FORMAT(/47H RECIPROCAL CONDITION OF F.D. HESSIAN = AT MOST,E10.2) 
      GO TO 70                                                          
C                                                                       
 50   WRITE(PU,60) T                                                    
 60   FORMAT(/44H RECIPROCAL CONDITION OF (J**T)*J = AT LEAST,E10.2)    
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
 180  FORMAT(4H ROW,I3,2X,5E12.3/(9X,5E12.3))                           
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF  N2CVP FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  N2LRD(DR, IV, L, LH, LIV, LV, ND, NN, P, R, RD, V)    
C                                                                       
C  ***  COMPUTE REGRESSION DIAGNOSTIC AND DEFAULT COVARIANCE MATRIX FOR 
C         RN2G  ***                                                     
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
      INTEGER LH, LIV, LV, ND, NN, P                                    
      INTEGER IV(LIV)                                                   
      REAL DR(ND,P), L(LH), R(NN), RD(NN), V(LV)                        
C                                                                       
C  ***  CODED BY DAVID M. GAY (WINTER 1982, FALL 1983)  ***             
C                                                                       
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      REAL  D7TPR                                                       
      EXTERNAL  D7TPR,  L7ITV,  L7IVM, O7PRD,  V7SCP                    
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER COV, I, J, M, STEP1                                       
      REAL A, FF, S, T                                                  
C                                                                       
C  ***  CONSTANTS  ***                                                  
C                                                                       
      REAL NEGONE, ONE, ONEV(1), ZERO                                   
C                                                                       
C  ***  INTRINSIC FUNCTIONS  ***                                        
C/+                                                                     
      REAL  SQRT                                                        
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
C     DATA NEGONE/-1.E+0/, ONE/1.E+0/, ZERO/0.E+0/                      
C/7                                                                     
      PARAMETER (NEGONE=-1.E+0, ONE=1.E+0, ZERO=0.E+0)                  
C/                                                                      
      DATA ONEV(1)/1.E+0/                                               
C                                                                       
C++++++++++++++++++++++++++++++++  BODY  +++++++++++++++++++++++++++++++
C                                                                       
      STEP1 = IV(STEP)                                                  
      I = IV(RDREQ)                                                     
      IF (I .LE. 0) GO TO 999                                           
      IF (MOD(I,4) .LT. 2) GO TO 30                                     
      FF = ONE                                                          
      IF (V(F) .NE. ZERO) FF = ONE /  SQRT( ABS(V(F)))                  
      CALL  V7SCP(NN, RD, NEGONE)                                       
      DO 20 I = 1, NN                                                   
         A = R(I)**2                                                    
         M = STEP1                                                      
         DO 10 J = 1, P                                                 
            V(M) = DR(I,J)                                              
            M = M + 1                                                   
 10         CONTINUE                                                    
         CALL  L7IVM(P, V(STEP1), L, V(STEP1))                          
         S =  D7TPR(P, V(STEP1), V(STEP1))                              
         T = ONE - S                                                    
         IF (T .LE. ZERO) GO TO 20                                      
         A = A * S / T                                                  
         RD(I) =  SQRT(A) * FF                                          
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
         CALL  L7IVM(P, V(STEP1), L, V(STEP1))                          
         CALL  L7ITV(P, V(STEP1), L, V(STEP1))                          
         CALL O7PRD(1, LH, P, V(COV), ONEV, V(STEP1), V(STEP1))         
 50      CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF  N2LRD FOLLOWS  ***                                
      END                                                               
      SUBROUTINE O7PRD(L, LS, P, S, W, Y, Z)                            
C                                                                       
C  ***  FOR I = 1..L, SET S = S + W(I)*Y(.,I)*(Z(.,I)**T), I.E.,        
C  ***        ADD W(I) TIMES THE OUTER PRODUCT OF Y(.,I) AND Z(.,I).    
C                                                                       
      INTEGER L, LS, P                                                  
      REAL S(LS), W(L), Y(P,L), Z(P,L)                                  
C     DIMENSION S(P*(P+1)/2)                                            
C                                                                       
      INTEGER I, J, K, M                                                
      REAL WK, YI, ZERO                                                 
      DATA ZERO/0.E+0/                                                  
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
C  ***  LAST CARD OF O7PRD FOLLOWS  ***                                 
      END                                                               
      SUBROUTINE PARCK(ALG, D, IV, LIV, LV, N, V)                       
C                                                                       
C  ***  CHECK ***SOL (VERSION 2.3) PARAMETERS, PRINT CHANGED VALUES  ***
C                                                                       
C  ***  ALG = 1 FOR REGRESSION, ALG = 2 FOR GENERAL UNCONSTRAINED OPT.  
C                                                                       
      INTEGER ALG, LIV, LV, N                                           
      INTEGER IV(LIV)                                                   
      REAL D(N), V(LV)                                                  
C                                                                       
      REAL  R7MDC                                                       
      EXTERNAL IVSET,  R7MDC, V7CPY, V7DFL                              
C IVSET  -- SUPPLIES DEFAULT VALUES TO BOTH IV AND V.                   
C  R7MDC -- RETURNS MACHINE-DEPENDENT CONSTANTS.                        
C V7CPY  -- COPIES ONE VECTOR TO ANOTHER.                               
C V7DFL  -- SUPPLIES DEFAULT PARAMETER VALUES TO V ALONE.               
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
      REAL BIG, MACHEP, TINY, VK, VM(34), VX(34), ZERO                  
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
      DATA BIG/0.E+0/, MACHEP/-1.E+0/, TINY/1.E+0/, ZERO/0.E+0/         
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
      DATA VM(1)/1.0E-3/, VM(2)/-0.99E+0/, VM(3)/1.0E-3/, VM(4)/1.0E-2/,
     1     VM(5)/1.2E+0/, VM(6)/1.E-2/, VM(7)/1.2E+0/, VM(8)/0.E+0/,    
     2     VM(9)/0.E+0/, VM(10)/1.E-3/, VM(11)/-1.E+0/, VM(13)/0.E+0/,  
     3     VM(15)/0.E+0/, VM(16)/0.E+0/, VM(19)/0.E+0/, VM(20)/-10.E+0/,
     4     VM(21)/0.E+0/, VM(22)/0.E+0/, VM(23)/0.E+0/, VM(27)/1.01E+0/,
     5     VM(28)/1.E+10/, VM(30)/0.E+0/, VM(31)/0.E+0/, VM(32)/0.E+0/, 
     6     VM(34)/0.E+0/                                                
      DATA VX(1)/0.9E+0/, VX(2)/-1.E-3/, VX(3)/1.E+1/, VX(4)/0.8E+0/,   
     1     VX(5)/1.E+2/, VX(6)/0.8E+0/, VX(7)/1.E+2/, VX(8)/0.5E+0/,    
     2     VX(9)/0.5E+0/, VX(10)/1.E+0/, VX(11)/1.E+0/, VX(14)/0.1E+0/, 
     3     VX(15)/1.E+0/, VX(16)/1.E+0/, VX(19)/1.E+0/, VX(23)/1.E+0/,  
     4     VX(24)/1.E+0/, VX(25)/1.E+0/, VX(26)/1.E+0/, VX(27)/1.E+10/, 
     5     VX(29)/1.E+0/, VX(31)/1.E+0/, VX(32)/1.E+0/, VX(33)/1.E+0/,  
     6     VX(34)/1.E+0/                                                
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
 10      FORMAT(/40H THE FIRST PARAMETER TO  IVSET SHOULD BE,I3,        
     1          12H RATHER THAN,I3)                                     
         IV(1) = 67                                                     
         GO TO 999                                                      
 20   IF (ALG .LT. 1 .OR. ALG .GT. 4) GO TO 340                         
      MIV1 = MINIV(ALG)                                                 
      IF (IV(1) .EQ. 15) GO TO 360                                      
      ALG1 = MOD(ALG-1,2) + 1                                           
      IF (IV(1) .EQ. 0) CALL IVSET(ALG, IV, LIV, LV, V)                 
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
         CALL V7DFL(ALG1, LV-K, V(K+1))                                 
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
         TINY =  R7MDC(1)                                               
         MACHEP =  R7MDC(3)                                             
         BIG =  R7MDC(6)                                                
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
         VX(28) =  R7MDC(5)                                             
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
 130          FORMAT(/6H ///  ,2A4,5H.. V(,I2,3H) =,E11.3,7H SHOULD,    
     1               11H BE BETWEEN,E11.3,4H AND,D11.3)                 
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
 180     FORMAT(/8H ///  D(,I3,3H) =,E11.3,19H SHOULD BE POSITIVE)      
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
 270          FORMAT(1X,2A4,5H.. V(,I2,3H) =,E15.7)                     
 280     K = K + 1                                                      
         L = L + 1                                                      
         I = I + 1                                                      
         IF (I .EQ. J) I = IJMP                                         
 290     CONTINUE                                                       
C                                                                       
      IV(DTYPE0) = IV(DTYPE)                                            
      PARSV1 = IV(PARSAV)                                               
      CALL V7CPY(IV(NVDFLT), V(PARSV1), V(EPSLON))                      
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
C  ***  LAST LINE OF PARCK FOLLOWS  ***                                 
      END                                                               
      SUBROUTINE IVSET(ALG, IV, LIV, LV, V)                             
C                                                                       
C  ***  SUPPLY ***SOL (VERSION 2.3) DEFAULT VALUES TO IV AND V  ***     
C                                                                       
C  ***  ALG = 1 MEANS REGRESSION CONSTANTS.                             
C  ***  ALG = 2 MEANS GENERAL UNCONSTRAINED OPTIMIZATION CONSTANTS.     
C                                                                       
      INTEGER LIV, LV                                                   
      INTEGER ALG, IV(LIV)                                              
      REAL V(LV)                                                        
C                                                                       
      INTEGER I7MDCN                                                    
      EXTERNAL I7MDCN, V7DFL                                            
C I7MDCN... RETURNS MACHINE-DEPENDENT INTEGER CONSTANTS.                
C V7DFL.... PROVIDES DEFAULT VALUES TO V.                               
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
      CALL V7DFL(ALG1, LV, V)                                           
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
C  ***  LAST CARD OF IVSET FOLLOWS  ***                                 
      END                                                               
      SUBROUTINE  Q7APL(NN, N, P, J, R, IERR)                           
C     *****PARAMETERS.                                                  
      INTEGER NN, N, P, IERR                                            
      REAL J(NN,P), R(N)                                                
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
C      D7TPR - FUNCTION, RETURNS THE INNER PRODUCT OF VECTORS           
C                                                                       
C     *****REFERENCES.                                                  
C     (1) BUSINGER, P. A., AND GOLUB, G. H. (1965), LINEAR LEAST SQUARES
C        SOLUTIONS BY HOUSEHOLDER TRANSFORMATIONS, NUMER. MATH. 7,      
C        PP. 269-276.                                                   
C                                                                       
C     *****HISTORY.                                                     
C     DESIGNED BY DAVID M. GAY, CODED BY STEPHEN C. PETERS (WINTER 1977)
C     CALL ON V2AXY SUBSTITUTED FOR DO LOOP, FALL 1983.                 
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
      REAL  D7TPR                                                       
      EXTERNAL  D7TPR, V2AXY                                            
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
      K = P                                                             
      IF (IERR .NE. 0) K = IABS(IERR) - 1                               
      IF ( K .EQ. 0) GO TO 999                                          
C                                                                       
      DO 20 L = 1, K                                                    
         NL1 = N - L + 1                                                
         CALL V2AXY(NL1, R(L), - D7TPR(NL1,J(L,L),R(L)), J(L,L), R(L))  
 20   CONTINUE                                                          
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF  Q7APL FOLLOWS  ***                                
      END                                                               
      SUBROUTINE Q7RAD(N, NN, P, QTR, QTRSET, RMAT, W, Y)               
C                                                                       
C  ***  ADD ROWS W TO QR FACTORIZATION WITH R MATRIX RMAT AND           
C  ***  Q**T * RESIDUAL = QTR.  Y = NEW COMPONENTS OF RESIDUAL          
C  ***  CORRESPONDING TO W.  QTR, Y REFERENCED ONLY IF QTRSET = .TRUE.  
C                                                                       
      LOGICAL QTRSET                                                    
      INTEGER N, NN, P                                                  
      REAL QTR(P), RMAT(1), W(NN,P), Y(N)                               
C     DIMENSION RMAT(P*(P+1)/2)                                         
C/+                                                                     
      REAL  SQRT                                                        
C/                                                                      
      REAL  D7TPR,  R7MDC,  V2NRM                                       
      EXTERNAL  D7TPR,  R7MDC, V2AXY,  V7SCL,  V2NRM                    
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, II, IJ, IP1, J, K, NK                                  
      REAL ARI, QRI, RI, S, T, WI                                       
      REAL BIG, BIGRT, ONE, TINY, TINYRT, ZERO                          
C/7                                                                     
      SAVE BIGRT, TINY, TINYRT                                          
C/                                                                      
      DATA BIG/-1.E+0/, BIGRT/-1.E+0/, ONE/1.E+0/, TINY/0.E+0/,         
     1     TINYRT/0.E+0/, ZERO/0.E+0/                                   
C                                                                       
C------------------------------ BODY -----------------------------------
C                                                                       
      IF (TINY .GT. ZERO) GO TO 10                                      
         TINY =  R7MDC(1)                                               
         BIG =  R7MDC(6)                                                
         IF (TINY*BIG .LT. ONE) TINY = ONE / BIG                        
 10   K = 1                                                             
      NK = N                                                            
      II = 0                                                            
      DO 180 I = 1, P                                                   
         II = II + I                                                    
         IP1 = I + 1                                                    
         IJ = II + I                                                    
         IF (NK .LE. 1) T =  ABS(W(K,I))                                
         IF (NK .GT. 1) T =  V2NRM(NK, W(K,I))                          
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
               BIGRT =  R7MDC(5)                                        
               TINYRT =  R7MDC(2)                                       
 40         IF (T .LE. TINYRT) GO TO 50                                 
            IF (T .GE. BIGRT) GO TO 50                                  
               IF (WI .LT. ZERO) T = -T                                 
               WI = WI + T                                              
               S =  SQRT(T * WI)                                        
               GO TO 70                                                 
 50         S =  SQRT(T)                                                
            IF (WI .LT. ZERO) GO TO 60                                  
               WI = WI + T                                              
               S = S *  SQRT(WI)                                        
               GO TO 70                                                 
 60         T = -T                                                      
            WI = WI + T                                                 
            S = S *  SQRT(-WI)                                          
 70         W(K,I) = WI                                                 
            CALL  V7SCL(NK, W(K,I), ONE/S, W(K,I))                      
            RMAT(II) = -T                                               
            IF (.NOT. QTRSET) GO TO 80                                  
            CALL V2AXY(NK, Y(K), - D7TPR(NK,Y(K),W(K,I)), W(K,I), Y(K)) 
            QTR(I) = Y(K)                                               
 80         IF (IP1 .GT. P) GO TO 999                                   
            DO 90 J = IP1, P                                            
               CALL V2AXY(NK, W(K,J), - D7TPR(NK,W(K,J),W(K,I)),        
     1                    W(K,I), W(K,J))                               
               RMAT(IJ) = W(K,J)                                        
               IJ = IJ + J                                              
 90            CONTINUE                                                 
            IF (NK .LE. 1) GO TO 999                                    
            K = K + 1                                                   
            NK = NK - 1                                                 
            GO TO 180                                                   
C                                                                       
 100     ARI =  ABS(RI)                                                 
         IF (ARI .GT. T) GO TO 110                                      
            T = T *  SQRT(ONE + (ARI/T)**2)                             
            GO TO 120                                                   
 110     T = ARI *  SQRT(ONE + (T/ARI)**2)                              
 120     IF (RI .LT. ZERO) T = -T                                       
         RI = RI + T                                                    
         RMAT(II) = -T                                                  
         S = -RI / T                                                    
         IF (NK .LE. 1) GO TO 150                                       
         CALL  V7SCL(NK, W(K,I), ONE/RI, W(K,I))                        
         IF (.NOT. QTRSET) GO TO 130                                    
            QRI = QTR(I)                                                
            T = S * ( QRI  +   D7TPR(NK, Y(K), W(K,I)) )                
            QTR(I) = QRI + T                                            
 130     IF (IP1 .GT. P) GO TO 999                                      
         IF (QTRSET) CALL V2AXY(NK, Y(K), T, W(K,I), Y(K))              
         DO 140 J = IP1, P                                              
            RI = RMAT(IJ)                                               
            T = S * ( RI  +   D7TPR(NK, W(K,J), W(K,I)) )               
            CALL V2AXY(NK, W(K,J), T, W(K,I), W(K,J))                   
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
C  ***  LAST LINE OF Q7RAD FOLLOWS  ***                                 
      END                                                               
      SUBROUTINE Q7RFH(IERR, IPIVOT, N, NN, NOPIVK, P, Q, R, RLEN, W)   
C                                                                       
C  ***  COMPUTE QR FACTORIZATION VIA HOUSEHOLDER TRANSFORMATIONS        
C  ***  WITH COLUMN PIVOTING  ***                                       
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER IERR, N, NN, NOPIVK, P, RLEN                              
      INTEGER IPIVOT(P)                                                 
      REAL Q(NN,P), R(RLEN), W(P)                                       
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
      REAL AK, QKK, S, SINGTL, T, T1, WK                                
      REAL  D7TPR,  R7MDC,  V2NRM                                       
      EXTERNAL  D7TPR,  R7MDC, V2AXY,  V7SCL,  V7SCP, V7SWP,  V2NRM     
C/+                                                                     
      REAL  SQRT                                                        
C/                                                                      
      REAL BIG, BIGRT, MEPS10, ONE, TEN, TINY, TINYRT,                  
     1                 WTOL, ZERO                                       
C/6                                                                     
C     DATA ONE/1.0E+0/, TEN/1.E+1/, WTOL/0.75E+0/, ZERO/0.0E+0/         
C/7                                                                     
      PARAMETER (ONE=1.0E+0, TEN=1.E+1, WTOL=0.75E+0, ZERO=0.0E+0)      
      SAVE BIGRT, MEPS10, TINY, TINYRT                                  
C/                                                                      
      DATA BIGRT/0.0E+0/, MEPS10/0.0E+0/, TINY/0.E+0/, TINYRT/0.E+0/    
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      IERR = 0                                                          
      IF (MEPS10 .GT. ZERO) GO TO 10                                    
          BIGRT =  R7MDC(5)                                             
          MEPS10 = TEN *  R7MDC(3)                                      
          TINYRT =  R7MDC(2)                                            
          TINY =  R7MDC(1)                                              
          BIG =  R7MDC(6)                                               
          IF (TINY*BIG .LT. ONE) TINY = ONE / BIG                       
 10   SINGTL = FLOAT(MAX0(N,P)) * MEPS10                                
C                                                                       
C  ***  INITIALIZE W, IPIVOT, AND DIAG(R)  ***                          
C                                                                       
      J = 0                                                             
      DO 40 I = 1, P                                                    
         IPIVOT(I) = I                                                  
         T =  V2NRM(N, Q(1,I))                                          
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
                   CALL V7SWP(N, Q(1,K), Q(1,J))                        
                   IF (K .LE. 1) GO TO 60                               
                        I = I - J + 1                                   
                        J = KK - K + 1                                  
                        CALL V7SWP(K-1, R(I), R(J))                     
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
            T =  V2NRM(NK1, Q(K,K))                                     
            IF (T / AK .LE. SINGTL) GO TO 140                           
            GO TO 80                                                    
 70      T =  SQRT(ONE - WK)                                            
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
            S =  SQRT(T * QKK)                                          
            GO TO 110                                                   
 90       S =  SQRT(T)                                                  
          IF (QKK .LT. ZERO) GO TO 100                                  
             QKK = QKK + T                                              
             S = S *  SQRT(QKK)                                         
             GO TO 110                                                  
 100      T = -T                                                        
          QKK = QKK + T                                                 
          S = S *  SQRT(-QKK)                                           
 110      Q(K,K) = QKK                                                  
C                                                                       
C         ***  SCALE (Q(K,K),...,Q(N,K)) TO HAVE NORM SQRT(2)  ***      
C                                                                       
          IF (S .LE. TINY) GO TO 140                                    
          CALL  V7SCL(NK1, Q(K,K), ONE/S, Q(K,K))                       
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
              CALL V2AXY(NK1, Q(K,I), - D7TPR(NK1,Q(K,K),Q(K,I)),       
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
         CALL  V7SCP(I-KM1, R(J), ZERO)                                 
         J = J + I                                                      
 150     CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF Q7RFH FOLLOWS  ***                                 
      END                                                               
      SUBROUTINE V2AXY(P, W, A, X, Y)                                   
C                                                                       
C  ***  SET W = A*X + Y  --  W, X, Y = P-VECTORS, A = SCALAR  ***       
C                                                                       
      INTEGER P                                                         
      REAL A, W(P), X(P), Y(P)                                          
C                                                                       
      INTEGER I                                                         
C                                                                       
      DO 10 I = 1, P                                                    
 10      W(I) = A*X(I) + Y(I)                                           
      RETURN                                                            
      END                                                               
      SUBROUTINE  R7TVM(N, P, Y, D, U, X)                               
C                                                                       
C  ***  SET Y TO R*X, WHERE R IS THE UPPER TRIANGULAR MATRIX WHOSE      
C  ***  DIAGONAL IS IN D AND WHOSE STRICT UPPER TRIANGLE IS IN U.       
C                                                                       
C  ***  X AND Y MAY SHARE STORAGE.                                      
C                                                                       
      INTEGER N, P                                                      
      REAL Y(P), D(P), U(N,P), X(P)                                     
C                                                                       
      REAL  D7TPR                                                       
      EXTERNAL  D7TPR                                                   
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, II, PL, PP1                                            
      REAL T                                                            
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
      PL = MIN0(N-1, P)                                                 
      PP1 = PL + 1                                                      
      DO 10 II = 1, PL                                                  
         I = PP1 - II                                                   
         T = X(I) * D(I)                                                
         IF (I .GT. 1) T = T +  D7TPR(I-1, U(1,I), X)                   
         Y(I) = T                                                       
 10      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF  R7TVM FOLLOWS  ***                                
      END                                                               
      REAL FUNCTION  RLDST(P, D, X, X0)                                 
C                                                                       
C  ***  COMPUTE AND RETURN RELATIVE DIFFERENCE BETWEEN X AND X0  ***    
C  ***  NL2SOL VERSION 2.2  ***                                         
C                                                                       
      INTEGER P                                                         
      REAL D(P), X(P), X0(P)                                            
C                                                                       
      INTEGER I                                                         
      REAL EMAX, T, XMAX, ZERO                                          
C/6                                                                     
C     DATA ZERO/0.E+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.E+0)                                            
C/                                                                      
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
      EMAX = ZERO                                                       
      XMAX = ZERO                                                       
      DO 10 I = 1, P                                                    
         T =  ABS(D(I) * (X(I) - X0(I)))                                
         IF (EMAX .LT. T) EMAX = T                                      
         T = D(I) * ( ABS(X(I)) +  ABS(X0(I)))                          
         IF (XMAX .LT. T) XMAX = T                                      
 10      CONTINUE                                                       
       RLDST = ZERO                                                     
      IF (XMAX .GT. ZERO)  RLDST = EMAX / XMAX                          
 999  RETURN                                                            
C  ***  LAST CARD OF  RLDST FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  Q7RSH(K, P, HAVQTR, QTR, R, W)                        
C                                                                       
C  ***  PERMUTE COLUMN K OF R TO COLUMN P, MODIFY QTR ACCORDINGLY  ***  
C                                                                       
      LOGICAL HAVQTR                                                    
      INTEGER K, P                                                      
      REAL QTR(P), R(1), W(P)                                           
C     DIMSNSION R(P*(P+1)/2)                                            
C                                                                       
      REAL  H2RFG                                                       
      EXTERNAL  H2RFA,  H2RFG, V7CPY                                    
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, I1, J, JM1, JP1, J1, KM1, K1, PM1                      
      REAL A, B, T, WJ, X, Y, Z, ZERO                                   
C                                                                       
      DATA ZERO/0.0E+0/                                                 
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      IF (K .GE. P) GO TO 999                                           
      KM1 = K - 1                                                       
      K1 = K * KM1 / 2                                                  
      CALL V7CPY(K, W, R(K1+1))                                         
      WJ = W(K)                                                         
      PM1 = P - 1                                                       
      J1 = K1 + KM1                                                     
      DO 50 J = K, PM1                                                  
         JM1 = J - 1                                                    
         JP1 = J + 1                                                    
         IF (JM1 .GT. 0) CALL V7CPY(JM1, R(K1+1), R(J1+2))              
         J1 = J1 + JP1                                                  
         K1 = K1 + J                                                    
         A = R(J1)                                                      
         B = R(J1+1)                                                    
         IF (B .NE. ZERO) GO TO 10                                      
              R(K1) = A                                                 
              X = ZERO                                                  
              Z = ZERO                                                  
              GO TO 40                                                  
 10      R(K1) =  H2RFG(A, B, X, Y, Z)                                  
         IF (J .EQ. PM1) GO TO 30                                       
         I1 = J1                                                        
         DO 20 I = JP1, PM1                                             
              I1 = I1 + I                                               
              CALL  H2RFA(1, R(I1), R(I1+1), X, Y, Z)                   
 20           CONTINUE                                                  
 30      IF (HAVQTR) CALL  H2RFA(1, QTR(J), QTR(JP1), X, Y, Z)          
 40      T = X * WJ                                                     
         W(J) = WJ + T                                                  
         WJ = T * Z                                                     
 50      CONTINUE                                                       
      W(P) = WJ                                                         
      CALL V7CPY(P, R(K1+1), W)                                         
 999  RETURN                                                            
      END                                                               
      SUBROUTINE L7VML(N, X, L, Y)                                      
C                                                                       
C  ***  COMPUTE  X = L*Y, WHERE  L  IS AN  N X N  LOWER TRIANGULAR      
C  ***  MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY OCCUPY THE SAME   
C  ***  STORAGE.  ***                                                   
C                                                                       
      INTEGER N                                                         
      REAL X(N), L(1), Y(N)                                             
C     DIMENSION L(N*(N+1)/2)                                            
      INTEGER I, II, IJ, I0, J, NP1                                     
      REAL T, ZERO                                                      
C/6                                                                     
C     DATA ZERO/0.E+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.E+0)                                            
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
C  ***  LAST CARD OF L7VML FOLLOWS  ***                                 
      END                                                               
      SUBROUTINE  L7TVM(N, X, L, Y)                                     
C                                                                       
C  ***  COMPUTE  X = (L**T)*Y, WHERE  L  IS AN  N X N  LOWER            
C  ***  TRIANGULAR MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY        
C  ***  OCCUPY THE SAME STORAGE.  ***                                   
C                                                                       
      INTEGER N                                                         
      REAL X(N), L(1), Y(N)                                             
C     DIMENSION L(N*(N+1)/2)                                            
      INTEGER I, IJ, I0, J                                              
      REAL YI, ZERO                                                     
C/6                                                                     
C     DATA ZERO/0.E+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.E+0)                                            
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
C  ***  LAST CARD OF  L7TVM FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  L7ITV(N, X, L, Y)                                     
C                                                                       
C  ***  SOLVE  (L**T)*X = Y,  WHERE  L  IS AN  N X N  LOWER TRIANGULAR  
C  ***  MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY OCCUPY THE SAME   
C  ***  STORAGE.  ***                                                   
C                                                                       
      INTEGER N                                                         
      REAL X(N), L(1), Y(N)                                             
      INTEGER I, II, IJ, IM1, I0, J, NP1                                
      REAL XI, ZERO                                                     
C/6                                                                     
C     DATA ZERO/0.E+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.E+0)                                            
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
C  ***  LAST CARD OF  L7ITV FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  S7DMP(N, X, Y, Z, K)                                  
C                                                                       
C ***  SET X = DIAG(Z)**K * Y * DIAG(Z)**K                              
C ***  FOR X, Y = COMPACTLY STORED LOWER TRIANG. MATRICES               
C ***  K = 1 OR -1.                                                     
C                                                                       
      INTEGER N, K                                                      
C/6S                                                                    
C     REAL X(1), Y(N), Z(1)                                             
C/7S                                                                    
      REAL X(*), Y(N), Z(*)                                             
C/                                                                      
      INTEGER I, J, L                                                   
      REAL ONE, T                                                       
      DATA ONE/1.E+0/                                                   
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
C  ***  LAST CARD OF  S7DMP FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  S7LUP(A, COSMIN, P, SIZE, STEP, U, W, WCHMTD, WSCALE, 
     1                  Y)                                              
C                                                                       
C  ***  UPDATE SYMMETRIC  A  SO THAT  A * STEP = Y  ***                 
C  ***  (LOWER TRIANGLE OF  A  STORED ROWWISE       ***                 
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER P                                                         
      REAL A(1), COSMIN, SIZE, STEP(P), U(P), W(P),                     
     1                 WCHMTD(P), WSCALE, Y(P)                          
C     DIMENSION A(P*(P+1)/2)                                            
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, J, K                                                   
      REAL DENMIN, SDOTWM, T, UI, WI                                    
C                                                                       
C     ***  CONSTANTS  ***                                               
      REAL HALF, ONE, ZERO                                              
C                                                                       
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      REAL  D7TPR,  V2NRM                                               
      EXTERNAL  D7TPR,  S7LVM,  V2NRM                                   
C                                                                       
C/6                                                                     
C     DATA HALF/0.5E+0/, ONE/1.E+0/, ZERO/0.E+0/                        
C/7                                                                     
      PARAMETER (HALF=0.5E+0, ONE=1.E+0, ZERO=0.E+0)                    
C/                                                                      
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
      SDOTWM =  D7TPR(P, STEP, WCHMTD)                                  
      DENMIN = COSMIN *  V2NRM(P,STEP) *  V2NRM(P,WCHMTD)               
      WSCALE = ONE                                                      
      IF (DENMIN .NE. ZERO) WSCALE = AMIN1(ONE,  ABS(SDOTWM/DENMIN))    
      T = ZERO                                                          
      IF (SDOTWM .NE. ZERO) T = WSCALE / SDOTWM                         
      DO 10 I = 1, P                                                    
 10      W(I) = T * WCHMTD(I)                                           
      CALL  S7LVM(P, U, A, STEP)                                        
      T = HALF * (SIZE *  D7TPR(P, STEP, U)  -   D7TPR(P, STEP, Y))     
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
C  ***  LAST CARD OF  S7LUP FOLLOWS  ***                                
      END                                                               
      REAL FUNCTION  V2NRM(P, X)                                        
C                                                                       
C  ***  RETURN THE 2-NORM OF THE P-VECTOR X, TAKING  ***                
C  ***  CARE TO AVOID THE MOST LIKELY UNDERFLOWS.    ***                
C                                                                       
      INTEGER P                                                         
      REAL X(P)                                                         
C                                                                       
      INTEGER I, J                                                      
      REAL ONE, R, SCALE, SQTETA, T, XI, ZERO                           
C/+                                                                     
      REAL  SQRT                                                        
C/                                                                      
      REAL  R7MDC                                                       
      EXTERNAL  R7MDC                                                   
C                                                                       
C/6                                                                     
C     DATA ONE/1.E+0/, ZERO/0.E+0/                                      
C/7                                                                     
      PARAMETER (ONE=1.E+0, ZERO=0.E+0)                                 
      SAVE SQTETA                                                       
C/                                                                      
      DATA SQTETA/0.E+0/                                                
C                                                                       
      IF (P .GT. 0) GO TO 10                                            
          V2NRM = ZERO                                                  
         GO TO 999                                                      
 10   DO 20 I = 1, P                                                    
         IF (X(I) .NE. ZERO) GO TO 30                                   
 20      CONTINUE                                                       
       V2NRM = ZERO                                                     
      GO TO 999                                                         
C                                                                       
 30   SCALE =  ABS(X(I))                                                
      IF (I .LT. P) GO TO 40                                            
          V2NRM = SCALE                                                 
         GO TO 999                                                      
 40   T = ONE                                                           
      IF (SQTETA .EQ. ZERO) SQTETA =  R7MDC(2)                          
C                                                                       
C     ***  SQTETA IS (SLIGHTLY LARGER THAN) THE SQUARE ROOT OF THE      
C     ***  SMALLEST POSITIVE FLOATING POINT NUMBER ON THE MACHINE.      
C     ***  THE TESTS INVOLVING SQTETA ARE DONE TO PREVENT UNDERFLOWS.   
C                                                                       
      J = I + 1                                                         
      DO 60 I = J, P                                                    
         XI =  ABS(X(I))                                                
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
       V2NRM = SCALE *  SQRT(T)                                         
 999  RETURN                                                            
C  ***  LAST LINE OF  V2NRM FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  L7IVM(N, X, L, Y)                                     
C                                                                       
C  ***  SOLVE  L*X = Y, WHERE  L  IS AN  N X N  LOWER TRIANGULAR        
C  ***  MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY OCCUPY THE SAME   
C  ***  STORAGE.  ***                                                   
C                                                                       
      INTEGER N                                                         
      REAL X(N), L(1), Y(N)                                             
      REAL  D7TPR                                                       
      EXTERNAL  D7TPR                                                   
      INTEGER I, J, K                                                   
      REAL T, ZERO                                                      
C/6                                                                     
C     DATA ZERO/0.E+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.E+0)                                            
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
         T =  D7TPR(I-1, L(J+1), X)                                     
         J = J + I                                                      
         X(I) = (Y(I) - T)/L(J)                                         
 30      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF  L7IVM FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  S7LVM(P, Y, S, X)                                     
C                                                                       
C  ***  SET  Y = S * X,  S = P X P SYMMETRIC MATRIX.  ***               
C  ***  LOWER TRIANGLE OF  S  STORED ROWWISE.         ***               
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER P                                                         
      REAL S(1), X(P), Y(P)                                             
C     DIMENSION S(P*(P+1)/2)                                            
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER I, IM1, J, K                                              
      REAL XI                                                           
C                                                                       
C  ***  NO INTRINSIC FUNCTIONS  ***                                     
C                                                                       
C  ***  EXTERNAL FUNCTION  ***                                          
C                                                                       
      REAL  D7TPR                                                       
      EXTERNAL  D7TPR                                                   
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
      J = 1                                                             
      DO 10 I = 1, P                                                    
         Y(I) =  D7TPR(I, S(J), X)                                      
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
C  ***  LAST CARD OF  S7LVM FOLLOWS  ***                                
      END                                                               
      REAL FUNCTION  D7TPR(P, X, Y)                                     
C                                                                       
C  ***  RETURN THE INNER PRODUCT OF THE P-VECTORS X AND Y.  ***         
C                                                                       
      INTEGER P                                                         
      REAL X(P), Y(P)                                                   
C                                                                       
      INTEGER I                                                         
      REAL ONE, SQTETA, T, ZERO                                         
      REAL  R7MDC                                                       
      EXTERNAL  R7MDC                                                   
C                                                                       
C  ***   R7MDC(2) RETURNS A MACHINE-DEPENDENT CONSTANT, SQTETA, WHICH   
C  ***  IS SLIGHTLY LARGER THAN THE SMALLEST POSITIVE NUMBER THAT       
C  ***  CAN BE SQUARED WITHOUT UNDERFLOWING.                            
C                                                                       
C/6                                                                     
C     DATA ONE/1.E+0/, SQTETA/0.E+0/, ZERO/0.E+0/                       
C/7                                                                     
      PARAMETER (ONE=1.E+0, ZERO=0.E+0)                                 
      DATA SQTETA/0.E+0/                                                
C/                                                                      
C                                                                       
       D7TPR = ZERO                                                     
      IF (P .LE. 0) GO TO 999                                           
      IF (SQTETA .EQ. ZERO) SQTETA =  R7MDC(2)                          
      DO 20 I = 1, P                                                    
         T = AMAX1( ABS(X(I)),  ABS(Y(I)))                              
         IF (T .GT. ONE) GO TO 10                                       
         IF (T .LT. SQTETA) GO TO 20                                    
         T = (X(I)/SQTETA)*Y(I)                                         
         IF ( ABS(T) .LT. SQTETA) GO TO 20                              
 10       D7TPR =  D7TPR + X(I)*Y(I)                                    
 20   CONTINUE                                                          
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF  D7TPR FOLLOWS  ***                                
      END                                                               
      SUBROUTINE V7DFL(ALG, LV, V)                                      
C                                                                       
C  ***  SUPPLY ***SOL (VERSION 2.3) DEFAULT VALUES TO V  ***            
C                                                                       
C  ***  ALG = 1 MEANS REGRESSION CONSTANTS.                             
C  ***  ALG = 2 MEANS GENERAL UNCONSTRAINED OPTIMIZATION CONSTANTS.     
C                                                                       
      INTEGER ALG, LV                                                   
      REAL V(LV)                                                        
C                                                                       
      REAL  R7MDC                                                       
      EXTERNAL  R7MDC                                                   
C  R7MDC... RETURNS MACHINE-DEPENDENT CONSTANTS                         
C                                                                       
      REAL MACHEP, MEPCRT, ONE, SQTEPS, THREE                           
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
C     DATA ONE/1.E+0/, THREE/3.E+0/                                     
C/7                                                                     
      PARAMETER (ONE=1.E+0, THREE=3.E+0)                                
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
      MACHEP =  R7MDC(3)                                                
      V(AFCTOL) = 1.E-20                                                
      IF (MACHEP .GT. 1.E-10) V(AFCTOL) = MACHEP**2                     
      V(DECFAC) = 0.5E+0                                                
      SQTEPS =  R7MDC(4)                                                
      V(DFAC) = 0.6E+0                                                  
      V(DTINIT) = 1.E-6                                                 
      MEPCRT = MACHEP ** (ONE/THREE)                                    
      V(D0INIT) = 1.E+0                                                 
      V(EPSLON) = 0.1E+0                                                
      V(INCFAC) = 2.E+0                                                 
      V(LMAX0) = 1.E+0                                                  
      V(LMAXS) = 1.E+0                                                  
      V(PHMNFC) = -0.1E+0                                               
      V(PHMXFC) = 0.1E+0                                                
      V(RDFCMN) = 0.1E+0                                                
      V(RDFCMX) = 4.E+0                                                 
      V(RFCTOL) = AMAX1(1.E-10, MEPCRT**2)                              
      V(SCTOL) = V(RFCTOL)                                              
      V(TUNER1) = 0.1E+0                                                
      V(TUNER2) = 1.E-4                                                 
      V(TUNER3) = 0.75E+0                                               
      V(TUNER4) = 0.5E+0                                                
      V(TUNER5) = 0.75E+0                                               
      V(XCTOL) = SQTEPS                                                 
      V(XFTOL) = 1.E+2 * MACHEP                                         
C                                                                       
      IF (ALG .GE. 2) GO TO 10                                          
C                                                                       
C  ***  REGRESSION  VALUES                                              
C                                                                       
      V(COSMIN) = AMAX1(1.E-6, 1.E+2 * MACHEP)                          
      V(DINIT) = 0.E+0                                                  
      V(DELTA0) = SQTEPS                                                
      V(DLTFDC) = MEPCRT                                                
      V(DLTFDJ) = SQTEPS                                                
      V(FUZZ) = 1.5E+0                                                  
      V(HUBERC) = 0.7E+0                                                
      V(RLIMIT) =  R7MDC(5)                                             
      V(RSPTOL) = 1.E-3                                                 
      V(SIGMIN) = 1.E-4                                                 
      GO TO 999                                                         
C                                                                       
C  ***  GENERAL OPTIMIZATION VALUES                                     
C                                                                       
 10   V(BIAS) = 0.8E+0                                                  
      V(DINIT) = -1.0E+0                                                
      V(ETA0) = 1.0E+3 * MACHEP                                         
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF V7DFL FOLLOWS  ***                                 
      END                                                               
      REAL FUNCTION  R7MDC(K)                                           
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
      REAL BIG, ETA, MACHEP                                             
C/+                                                                     
      REAL SQRT                                                         
C/                                                                      
      REAL R1MACH, ZERO                                                 
      EXTERNAL R1MACH                                                   
      DATA BIG/0.E+0/, ETA/0.E+0/, MACHEP/0.E+0/, ZERO/0.E+0/           
      IF (BIG .GT. ZERO) GO TO 1                                        
         BIG = R1MACH(2)                                                
         ETA = R1MACH(1)                                                
         MACHEP = R1MACH(4)                                             
 1    CONTINUE                                                          
C                                                                       
C-------------------------------  BODY  --------------------------------
C                                                                       
      GO TO (10, 20, 30, 40, 50, 60), K                                 
C                                                                       
 10    R7MDC = ETA                                                      
      GO TO 999                                                         
C                                                                       
 20    R7MDC = SQRT(256.E+0*ETA)/16.E+0                                 
      GO TO 999                                                         
C                                                                       
 30    R7MDC = MACHEP                                                   
      GO TO 999                                                         
C                                                                       
 40    R7MDC = SQRT(MACHEP)                                             
      GO TO 999                                                         
C                                                                       
 50    R7MDC = SQRT(BIG/256.E+0)*16.E+0                                 
      GO TO 999                                                         
C                                                                       
 60    R7MDC = BIG                                                      
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF  R7MDC FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  V7SHF(N, K, X)                                        
C                                                                       
C  ***  SHIFT X(K),...,X(N) LEFT CIRCULARLY ONE POSITION  ***           
C                                                                       
      INTEGER N, K                                                      
      REAL X(N)                                                         
C                                                                       
      INTEGER I, NM1                                                    
      REAL T                                                            
C                                                                       
      IF (K .GE. N) GO TO 999                                           
      NM1 = N - 1                                                       
      T = X(K)                                                          
      DO 10 I = K, NM1                                                  
 10      X(I) = X(I+1)                                                  
      X(N) = T                                                          
 999  RETURN                                                            
      END                                                               
      SUBROUTINE V7SWP(N, X, Y)                                         
C                                                                       
C  ***  INTERCHANGE N-VECTORS X AND Y.  ***                             
C                                                                       
      INTEGER N                                                         
      REAL X(N), Y(N)                                                   
C                                                                       
      INTEGER I                                                         
      REAL T                                                            
C                                                                       
      DO 10 I = 1, N                                                    
         T = X(I)                                                       
         X(I) = Y(I)                                                    
         Y(I) = T                                                       
 10      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF V7SWP FOLLOWS  ***                                 
      END                                                               
      SUBROUTINE  F7DHB(B, D, G, IRT, IV, LIV, LV, P, V, X)             
C                                                                       
C  ***  COMPUTE FINITE-DIFFERENCE HESSIAN, STORE IT IN V STARTING       
C  ***  AT V(IV(FDH)) = V(-IV(H)).  HONOR SIMPLE BOUNDS IN B.           
C                                                                       
C  ***  IF IV(COVREQ) .GE. 0 THEN  F7DHB USES GRADIENT DIFFERENCES,     
C  ***  OTHERWISE FUNCTION DIFFERENCES.  STORAGE IN V IS AS IN G7LIT.   
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
      REAL B(2,P), D(P), G(P), V(LV), X(P)                              
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      LOGICAL OFFSID                                                    
      INTEGER GSAVE1, HES, HMI, HPI, HPM, I, K, KIND, L, M, MM1, MM1O2, 
     1        NEWM1, PP1O2, STPI, STPM, STP0                            
      REAL DEL, DEL0, T, XM, XM1                                        
      REAL HALF, HLIM, ONE, TWO, ZERO                                   
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      EXTERNAL V7CPY,  V7SCP                                            
C                                                                       
C V7CPY.... COPY ONE VECTOR TO ANOTHER.                                 
C  V7SCP... COPY SCALAR TO ALL COMPONENTS OF A VECTOR.                  
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER COVREQ, DELTA, DELTA0, DLTFDC, F, FDH, FX, H, KAGQT, MODE,
     1        NFGCAL, SAVEI, SWITCH, TOOBIG, W, XMSAVE                  
C                                                                       
C/6                                                                     
C     DATA HALF/0.5E+0/, HLIM/0.1E+0/, ONE/1.E+0/, TWO/2.E+0/,          
C    1     ZERO/0.E+0/                                                  
C/7                                                                     
      PARAMETER (HALF=0.5E+0, HLIM=0.1E+0, ONE=1.E+0, TWO=2.E+0,        
     1           ZERO=0.E+0)                                            
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
         CALL  V7SCP(P*(P+1)/2, V(HES), ZERO)                           
 10   IF (M .GT. P) GO TO 999                                           
      IF (KIND .LT. 0) GO TO 120                                        
C                                                                       
C  ***  COMPUTE FINITE-DIFFERENCE HESSIAN USING BOTH FUNCTION AND       
C  ***  GRADIENT VALUES.                                                
C                                                                       
      GSAVE1 = IV(W) + P                                                
      IF (M .GT. 0) GO TO 20                                            
C        ***  FIRST CALL ON  F7DHB.  SET GSAVE = G, TAKE FIRST STEP  ***
         CALL V7CPY(P, V(GSAVE1), G)                                    
         IV(SWITCH) = IV(NFGCAL)                                        
         GO TO 80                                                       
C                                                                       
 20   DEL = V(DELTA)                                                    
      X(M) = V(XMSAVE)                                                  
      IF (IV(TOOBIG) .EQ. 0) GO TO 30                                   
C                                                                       
C     ***  HANDLE OVERSIZE V(DELTA)  ***                                
C                                                                       
         DEL0 = V(DELTA0) * AMAX1(ONE/D(M),  ABS(X(M)))                 
         DEL = HALF * DEL                                               
         IF ( ABS(DEL/DEL0) .LE. HLIM) GO TO 140                        
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
      DEL = V(DELTA0) * AMAX1(ONE/D(M),  ABS(X(M)))                     
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
C        ***  FIRST CALL ON  F7DHB.  ***                                
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
      DEL = V(DLTFDC) * AMAX1(ONE/D(M),  ABS(XM))                       
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
         CALL  V7SCP(P, V(I), ZERO)                                     
C                                                                       
C  ***  RESTORE V(F), ETC.  ***                                         
C                                                                       
 340  IV(FDH) = HES                                                     
 350  V(F) = V(FX)                                                      
      IRT = 3                                                           
      IF (KIND .LT. 0) GO TO 999                                        
         IV(NFGCAL) = IV(SWITCH)                                        
         GSAVE1 = IV(W) + P                                             
         CALL V7CPY(P, G, V(GSAVE1))                                    
         GO TO 999                                                      
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF  F7DHB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  V7SCL(N, X, A, Y)                                     
C                                                                       
C  ***  SET X(I) = A*Y(I), I = 1(1)N  ***                               
C                                                                       
      INTEGER N                                                         
      REAL A, X(N), Y(N)                                                
C                                                                       
      INTEGER I                                                         
C                                                                       
      DO 10 I = 1, N                                                    
 10       X(I) = A * Y(I)                                               
 999    RETURN                                                          
C  ***  LAST LINE OF  V7SCL FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  V7SCP(P, Y, S)                                        
C                                                                       
C  ***  SET P-VECTOR Y TO SCALAR S  ***                                 
C                                                                       
      INTEGER P                                                         
      REAL S, Y(P)                                                      
C                                                                       
      INTEGER I                                                         
C                                                                       
      DO 10 I = 1, P                                                    
 10      Y(I) = S                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE  D7MLP(N, X, Y, Z, K)                                  
C                                                                       
C ***  SET X = DIAG(Y)**K * Z                                           
C ***  FOR X, Z = LOWER TRIANG. MATRICES STORED COMPACTLY BY ROW        
C ***  K = 1 OR -1.                                                     
C                                                                       
      INTEGER N, K                                                      
C/6S                                                                    
C     REAL X(1), Y(N), Z(1)                                             
C/7S                                                                    
      REAL X(*), Y(N), Z(*)                                             
C/                                                                      
      INTEGER I, J, L                                                   
      REAL ONE, T                                                       
      DATA ONE/1.E+0/                                                   
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
C  ***  LAST CARD OF  D7MLP FOLLOWS  ***                                
      END                                                               
      SUBROUTINE F7HES(D, G, IRT, IV, LIV, LV, P, V, X)                 
C                                                                       
C  ***  COMPUTE FINITE-DIFFERENCE HESSIAN, STORE IT IN V STARTING       
C  ***  AT V(IV(FDH)) = V(-IV(H)).                                      
C                                                                       
C  ***  IF IV(COVREQ) .GE. 0 THEN F7HES USES GRADIENT DIFFERENCES,      
C  ***  OTHERWISE FUNCTION DIFFERENCES.  STORAGE IN V IS AS IN G7LIT.   
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
      REAL D(P), G(P), V(LV), X(P)                                      
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER GSAVE1, HES, HMI, HPI, HPM, I, K, KIND, L, M, MM1, MM1O2, 
     1        PP1O2, STPI, STPM, STP0                                   
      REAL DEL, HALF, NEGPT5, ONE, TWO, ZERO                            
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      EXTERNAL V7CPY                                                    
C                                                                       
C V7CPY.... COPY ONE VECTOR TO ANOTHER.                                 
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER COVREQ, DELTA, DELTA0, DLTFDC, F, FDH, FX, H, KAGQT, MODE,
     1        NFGCAL, SAVEI, SWITCH, TOOBIG, W, XMSAVE                  
C                                                                       
C/6                                                                     
C     DATA HALF/0.5E+0/, NEGPT5/-0.5E+0/, ONE/1.E+0/, TWO/2.E+0/,       
C    1     ZERO/0.E+0/                                                  
C/7                                                                     
      PARAMETER (HALF=0.5E+0, NEGPT5=-0.5E+0, ONE=1.E+0, TWO=2.E+0,     
     1     ZERO=0.E+0)                                                  
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
C        ***  FIRST CALL ON F7HES.  SET GSAVE = G, TAKE FIRST STEP  *** 
         CALL V7CPY(P, V(GSAVE1), G)                                    
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
      DEL = V(DELTA0) * AMAX1(ONE/D(M),  ABS(X(M)))                     
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
C        ***  FIRST CALL ON F7HES.  ***                                 
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
      DEL = V(DLTFDC) * AMAX1(ONE/D(M),  ABS(X(M)))                     
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
         CALL V7CPY(P, G, V(GSAVE1))                                    
         GO TO 999                                                      
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF F7HES FOLLOWS  ***                                 
      END                                                               
      SUBROUTINE V7CPY(P, Y, X)                                         
C                                                                       
C  ***  SET Y = X, WHERE X AND Y ARE P-VECTORS  ***                     
C                                                                       
      INTEGER P                                                         
      REAL X(P), Y(P)                                                   
C                                                                       
      INTEGER I                                                         
C                                                                       
      DO 10 I = 1, P                                                    
 10      Y(I) = X(I)                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE  H2RFA(N, A, B, X, Y, Z)                               
C                                                                       
C  ***  APPLY 2X2 HOUSEHOLDER REFLECTION DETERMINED BY X, Y, Z TO       
C  ***  N-VECTORS A, B  ***                                             
C                                                                       
      INTEGER N                                                         
      REAL A(N), B(N), X, Y, Z                                          
      INTEGER I                                                         
      REAL T                                                            
      DO 10 I = 1, N                                                    
         T = A(I)*X + B(I)*Y                                            
         A(I) = A(I) + T                                                
         B(I) = B(I) + T*Z                                              
 10      CONTINUE                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF  H2RFA FOLLOWS  ***                                
      END                                                               
      REAL FUNCTION  H2RFG(A, B, X, Y, Z)                               
C                                                                       
C  ***  DETERMINE X, Y, Z SO  I + (1,Z)**T * (X,Y)  IS A 2X2            
C  ***  HOUSEHOLDER REFLECTION SENDING (A,B)**T INTO (C,0)**T,          
C  ***  WHERE  C = -SIGN(A)*SQRT(A**2 + B**2)  IS THE VALUE  H2RFG      
C  ***  RETURNS.                                                        
C                                                                       
      REAL A, B, X, Y, Z                                                
C                                                                       
      REAL A1, B1, C, T                                                 
C/+                                                                     
      REAL  SQRT                                                        
C/                                                                      
      REAL ZERO                                                         
      DATA ZERO/0.E+0/                                                  
C                                                                       
C  ***  BODY  ***                                                       
C                                                                       
      IF (B .NE. ZERO) GO TO 10                                         
         X = ZERO                                                       
         Y = ZERO                                                       
         Z = ZERO                                                       
          H2RFG = A                                                     
         GO TO 999                                                      
 10   T =  ABS(A) +  ABS(B)                                             
      A1 = A / T                                                        
      B1 = B / T                                                        
      C =  SQRT(A1**2 + B1**2)                                          
      IF (A1 .GT. ZERO) C = -C                                          
      A1 = A1 - C                                                       
      Z = B1 / A1                                                       
      X = A1 / C                                                        
      Y = B1 / C                                                        
       H2RFG = T * C                                                    
 999  RETURN                                                            
C  ***  LAST LINE OF  H2RFG FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  DMNF(N, D, X, CALCF, IV, LIV, LV, V,                  
     1                  UIPARM, URPARM, UFPARM)                         
C                                                                       
C  ***  MINIMIZE GENERAL UNCONSTRAINED OBJECTIVE FUNCTION USING         
C  ***  FINITE-DIFFERENCE GRADIENTS AND SECANT HESSIAN APPROXIMATIONS.  
C                                                                       
      INTEGER N, LIV, LV                                                
      INTEGER IV(LIV), UIPARM(1)                                        
      DOUBLE PRECISION D(N), X(N), V(LV), URPARM(1)                     
C     DIMENSION V(77 + N*(N+17)/2), UIPARM(*), URPARM(*)                
      EXTERNAL CALCF, UFPARM                                            
C                                                                       
C  ***  PURPOSE  ***                                                    
C                                                                       
C        THIS ROUTINE INTERACTS WITH SUBROUTINE  DRMNF  IN AN ATTEMPT   
C     TO FIND AN N-VECTOR  X*  THAT MINIMIZES THE (UNCONSTRAINED)       
C     OBJECTIVE FUNCTION COMPUTED BY  CALCF.  (OFTEN THE  X*  FOUND IS  
C     A LOCAL MINIMIZER RATHER THAN A GLOBAL ONE.)                      
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
C        THE PARAMETERS FOR  DMNF ARE THE SAME AS THOSE FOR  DMNG       
C     (WHICH SEE), EXCEPT THAT CALCG IS OMITTED.  INSTEAD OF CALLING    
C     CALCG TO OBTAIN THE GRADIENT OF THE OBJECTIVE FUNCTION AT X,      
C      DMNF CALLS DS7GRD, WHICH COMPUTES AN APPROXIMATION TO THE        
C     GRADIENT BY FINITE (FORWARD AND CENTRAL) DIFFERENCES USING THE    
C     METHOD OF REF. 1.  THE FOLLOWING INPUT COMPONENT IS OF INTEREST   
C     IN THIS REGARD (AND IS NOT DESCRIBED IN  DMNG).                   
C                                                                       
C V(ETA0)..... V(42) IS AN ESTIMATED BOUND ON THE RELATIVE ERROR IN THE 
C             OBJECTIVE FUNCTION VALUE COMPUTED BY CALCF...             
C                  (TRUE VALUE) = (COMPUTED VALUE) * (1 + E),           
C             WHERE ABS(E) .LE. V(ETA0).  DEFAULT = MACHEP * 10**3,     
C             WHERE MACHEP IS THE UNIT ROUNDOFF.                        
C                                                                       
C        THE OUTPUT VALUES IV(NFCALL) AND IV(NGCALL) HAVE DIFFERENT     
C     MEANINGS FOR  DMNF THAN FOR  DMNG...                              
C                                                                       
C IV(NFCALL)... IV(6) IS THE NUMBER OF CALLS SO FAR MADE ON CALCF (I.E.,
C             FUNCTION EVALUATIONS) EXCLUDING THOSE MADE ONLY FOR       
C             COMPUTING GRADIENTS.  THE INPUT VALUE IV(MXFCAL) IS A     
C             LIMIT ON IV(NFCALL).                                      
C IV(NGCALL)... IV(30) IS THE NUMBER OF FUNCTION EVALUATIONS MADE ONLY  
C             FOR COMPUTING GRADIENTS.  THE TOTAL NUMBER OF FUNCTION    
C             EVALUATIONS IS THUS  IV(NFCALL) + IV(NGCALL).             
C                                                                       
C  ***  REFERENCE  ***                                                  
C                                                                       
C 1. STEWART, G.W. (1967), A MODIFICATION OF DAVIDON*S MINIMIZATION     
C        METHOD TO ACCEPT DIFFERENCE APPROXIMATIONS OF DERIVATIVES,     
C        J. ASSOC. COMPUT. MACH. 14, PP. 72-83.                         
C.                                                                      
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY (WINTER 1980).  REVISED SEPT. 1982.         
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH           
C     SUPPORTED IN PART BY THE NATIONAL SCIENCE FOUNDATION UNDER        
C     GRANTS MCS-7600324, DCR75-10143, 76-14311DSS, MCS76-11989,        
C     AND MCS-7906671.                                                  
C                                                                       
C                                                                       
C----------------------------  DECLARATIONS  ---------------------------
C                                                                       
      EXTERNAL DRMNF                                                    
C                                                                       
C DRMNF.... OVERSEES COMPUTATION OF FINITE-DIFFERENCE GRADIENT AND      
C         CALLS DRMNG TO CARRY OUT  DMNG ALGORITHM.                     
C                                                                       
      INTEGER NF                                                        
      DOUBLE PRECISION FX                                               
C                                                                       
C  ***  SUBSCRIPTS FOR IV   ***                                         
C                                                                       
      INTEGER NFCALL, TOOBIG                                            
C                                                                       
C/6                                                                     
C     DATA NFCALL/6/, TOOBIG/2/                                         
C/7                                                                     
      PARAMETER (NFCALL=6, TOOBIG=2)                                    
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
 10   CALL DRMNF(D, FX, IV, LIV, LV, N, V, X)                           
      IF (IV(1) .GT. 2) GO TO 999                                       
C                                                                       
C     ***  COMPUTE FUNCTION  ***                                        
C                                                                       
      NF = IV(NFCALL)                                                   
      CALL CALCF(N, X, NF, FX, UIPARM, URPARM, UFPARM)                  
      IF (NF .LE. 0) IV(TOOBIG) = 1                                     
      GO TO 10                                                          
C                                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF  DMNF FOLLOWS  ***                                 
      END                                                               
      SUBROUTINE  DMNFB(P, D, X, B, CALCF, IV, LIV, LV, V,              
     1                  UIPARM, URPARM, UFPARM)                         
C                                                                       
C  ***  MINIMIZE GENERAL UNCONSTRAINED OBJECTIVE FUNCTION USING         
C  ***  FINITE-DIFFERENCE GRADIENTS AND SECANT HESSIAN APPROXIMATIONS.  
C                                                                       
      INTEGER P, LIV, LV                                                
      INTEGER IV(LIV), UIPARM(1)                                        
      DOUBLE PRECISION B(2,P), D(P), X(P), V(LV), URPARM(1)             
C     DIMENSION V(77 + P*(P+17)/2), UIPARM(*), URPARM(*)                
      EXTERNAL CALCF, UFPARM                                            
C                                                                       
C  ***  PURPOSE  ***                                                    
C                                                                       
C        THIS ROUTINE INTERACTS WITH SUBROUTINE  DRMNF  IN AN ATTEMPT   
C     TO FIND AN P-VECTOR  X*  THAT MINIMIZES THE (UNCONSTRAINED)       
C     OBJECTIVE FUNCTION COMPUTED BY  CALCF.  (OFTEN THE  X*  FOUND IS  
C     A LOCAL MINIMIZER RATHER THAN A GLOBAL ONE.)                      
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
C        THE PARAMETERS FOR  DMNFB ARE THE SAME AS THOSE FOR  DMNGB     
C     (WHICH SEE), EXCEPT THAT CALCG IS OMITTED.  INSTEAD OF CALLING    
C     CALCG TO OBTAIN THE GRADIENT OF THE OBJECTIVE FUNCTION AT X,      
C      DMNFB CALLS DS7GRD, WHICH COMPUTES AN APPROXIMATION TO THE       
C     GRADIENT BY FINITE (FORWARD AND CENTRAL) DIFFERENCES USING THE    
C     METHOD OF REF. 1.  THE FOLLOWING INPUT COMPONENT IS OF INTEREST   
C     IN THIS REGARD (AND IS NOT DESCRIBED IN  DMNG OR  DMNGB).         
C                                                                       
C V(ETA0)..... V(42) IS AN ESTIMATED BOUND ON THE RELATIVE ERROR IN THE 
C             OBJECTIVE FUNCTION VALUE COMPUTED BY CALCF...             
C                  (TRUE VALUE) = (COMPUTED VALUE) * (1 + E),           
C             WHERE ABS(E) .LE. V(ETA0).  DEFAULT = MACHEP * 10**3,     
C             WHERE MACHEP IS THE UNIT ROUNDOFF.                        
C                                                                       
C        THE OUTPUT VALUES IV(NFCALL) AND IV(NGCALL) HAVE DIFFERENT     
C     MEANINGS FOR  DMNFB THAN FOR  DMNG...                             
C                                                                       
C IV(NFCALL)... IV(6) IS THE NUMBER OF CALLS SO FAR MADE ON CALCF (I.E.,
C             FUNCTION EVALUATIONS) EXCLUDING THOSE MADE ONLY FOR       
C             COMPUTING GRADIENTS.  THE INPUT VALUE IV(MXFCAL) IS A     
C             LIMIT ON IV(NFCALL).                                      
C IV(NGCALL)... IV(30) IS THE NUMBER OF FUNCTION EVALUATIONS MADE ONLY  
C             FOR COMPUTING GRADIENTS.  THE TOTAL NUMBER OF FUNCTION    
C             EVALUATIONS IS THUS  IV(NFCALL) + IV(NGCALL).             
C                                                                       
C  ***  REFERENCE  ***                                                  
C                                                                       
C 1. STEWART, G.W. (1967), A MODIFICATION OF DAVIDON*S MINIMIZATION     
C        METHOD TO ACCEPT DIFFERENCE APPROXIMATIONS OF DERIVATIVES,     
C        J. ASSOC. COMPUT. MACH. 14, PP. 72-83.                         
C.                                                                      
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY (WINTER 1980).  REVISED SEPT. 1982.         
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH           
C     SUPPORTED IN PART BY THE NATIONAL SCIENCE FOUNDATION UNDER        
C     GRANTS MCS-7600324, DCR75-10143, 76-14311DSS, MCS76-11989,        
C     AND MCS-7906671.                                                  
C                                                                       
C                                                                       
C----------------------------  DECLARATIONS  ---------------------------
C                                                                       
      EXTERNAL DRMNFB                                                   
C                                                                       
C DRMNFB... OVERSEES COMPUTATION OF FINITE-DIFFERENCE GRADIENT AND      
C         CALLS DRMNG TO CARRY OUT  DMNG ALGORITHM.                     
C                                                                       
      INTEGER NF                                                        
      DOUBLE PRECISION FX                                               
C                                                                       
C  ***  SUBSCRIPTS FOR IV   ***                                         
C                                                                       
      INTEGER NFCALL, TOOBIG                                            
C                                                                       
C/6                                                                     
C     DATA NFCALL/6/, TOOBIG/2/                                         
C/7                                                                     
      PARAMETER (NFCALL=6, TOOBIG=2)                                    
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
 10   CALL DRMNFB(B, D, FX, IV, LIV, LV, P, V, X)                       
      IF (IV(1) .GT. 2) GO TO 999                                       
C                                                                       
C     ***  COMPUTE FUNCTION  ***                                        
C                                                                       
      NF = IV(NFCALL)                                                   
      CALL CALCF(P, X, NF, FX, UIPARM, URPARM, UFPARM)                  
      IF (NF .LE. 0) IV(TOOBIG) = 1                                     
      GO TO 10                                                          
C                                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF  DMNFB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  DMNG(N, D, X, CALCF, CALCG, IV, LIV, LV, V,           
     1                  UIPARM, URPARM, UFPARM)                         
C                                                                       
C  ***  MINIMIZE GENERAL UNCONSTRAINED OBJECTIVE FUNCTION USING   ***   
C  ***  ANALYTIC GRADIENT AND HESSIAN APPROX. FROM SECANT UPDATE  ***   
C                                                                       
      INTEGER N, LIV, LV                                                
      INTEGER IV(LIV), UIPARM(1)                                        
      DOUBLE PRECISION D(N), X(N), V(LV), URPARM(1)                     
C     DIMENSION V(71 + N*(N+15)/2), UIPARM(*), URPARM(*)                
      EXTERNAL CALCF, CALCG, UFPARM                                     
C                                                                       
C  ***  PURPOSE  ***                                                    
C                                                                       
C        THIS ROUTINE INTERACTS WITH SUBROUTINE  DRMNG  IN AN ATTEMPT   
C     TO FIND AN N-VECTOR  X*  THAT MINIMIZES THE (UNCONSTRAINED)       
C     OBJECTIVE FUNCTION COMPUTED BY  CALCF.  (OFTEN THE  X*  FOUND IS  
C     A LOCAL MINIMIZER RATHER THAN A GLOBAL ONE.)                      
C                                                                       
C--------------------------  PARAMETER USAGE  --------------------------
C                                                                       
C N........ (INPUT) THE NUMBER OF VARIABLES ON WHICH  F  DEPENDS, I.E., 
C                  THE NUMBER OF COMPONENTS IN  X.                      
C D........ (INPUT/OUTPUT) A SCALE VECTOR SUCH THAT  D(I)*X(I),         
C                  I = 1,2,...,N,  ARE ALL IN COMPARABLE UNITS.         
C                  D CAN STRONGLY AFFECT THE BEHAVIOR OF  DMNG.         
C                  FINDING THE BEST CHOICE OF D IS GENERALLY A TRIAL-   
C                  AND-ERROR PROCESS.  CHOOSING D SO THAT D(I)*X(I)     
C                  HAS ABOUT THE SAME VALUE FOR ALL I OFTEN WORKS WELL. 
C                  THE DEFAULTS PROVIDED BY SUBROUTINE DIVSET (SEE IV   
C                  BELOW) REQUIRE THE CALLER TO SUPPLY D.               
C X........ (INPUT/OUTPUT) BEFORE (INITIALLY) CALLING  DMNG, THE CALL-  
C                  ER SHOULD SET  X  TO AN INITIAL GUESS AT  X*.  WHEN  
C                   DMNG RETURNS,  X  CONTAINS THE BEST POINT SO FAR    
C                  FOUND, I.E., THE ONE THAT GIVES THE LEAST VALUE SO   
C                  FAR SEEN FOR  F(X).                                  
C CALCF.... (INPUT) A SUBROUTINE THAT, GIVEN X, COMPUTES F(X).  CALCF   
C                  MUST BE DECLARED EXTERNAL IN THE CALLING PROGRAM.    
C                  IT IS INVOKED BY                                     
C                       CALL CALCF(N, X, NF, F, UIPARM, URPARM, UFPARM) 
C                  WHEN CALCF IS CALLED, NF IS THE INVOCATION           
C                  COUNT FOR CALCF.  NF IS INCLUDED FOR POSSIBLE USE    
C                  WITH CALCG.  IF X IS OUT OF BOUNDS (E.G., IF IT      
C                  WOULD CAUSE OVERFLOW IN COMPUTING F(X)), THEN CALCF  
C                  SHOULD SET NF TO 0.  THIS WILL CAUSE A SHORTER STEP  
C                  TO BE ATTEMPTED.  (IF X IS IN BOUNDS, THEN CALCF     
C                  SHOULD NOT CHANGE NF.)  THE OTHER PARAMETERS ARE AS  
C                  DESCRIBED ABOVE AND BELOW.  CALCF SHOULD NOT CHANGE  
C                  N, P, OR X.                                          
C CALCG.... (INPUT) A SUBROUTINE THAT, GIVEN X, COMPUTES G(X), THE GRA- 
C                  DIENT OF F AT X.  CALCG MUST BE DECLARED EXTERNAL IN 
C                  THE CALLING PROGRAM.  IT IS INVOKED BY               
C                       CALL CALCG(N, X, NF, G, UIPARM, URPARM, UFAPRM) 
C                  WHEN CALCG IS CALLED, NF IS THE INVOCATION           
C                  COUNT FOR CALCF AT THE TIME F(X) WAS EVALUATED.  THE 
C                  X PASSED TO CALCG IS USUALLY THE ONE PASSED TO CALCF 
C                  ON EITHER ITS MOST RECENT INVOCATION OR THE ONE      
C                  PRIOR TO IT.  IF CALCF SAVES INTERMEDIATE RESULTS    
C                  FOR USE BY CALCG, THEN IT IS POSSIBLE TO TELL FROM   
C                  NF WHETHER THEY ARE VALID FOR THE CURRENT X (OR      
C                  WHICH COPY IS VALID IF TWO COPIES ARE KEPT).  IF G   
C                  CANNOT BE COMPUTED AT X, THEN CALCG SHOULD SET NF TO 
C                  0.  IN THIS CASE,  DMNG WILL RETURN WITH IV(1) = 65. 
C                  (IF G CAN BE COMPUTED AT X, THEN CALCG SHOULD NOT    
C                  CHANGED NF.)  THE OTHER PARAMETERS TO CALCG ARE AS   
C                  DESCRIBED ABOVE AND BELOW.  CALCG SHOULD NOT CHANGE  
C                  N OR X.                                              
C IV....... (INPUT/OUTPUT) AN INTEGER VALUE ARRAY OF LENGTH LIV (SEE    
C                  BELOW) THAT HELPS CONTROL THE  DMNG ALGORITHM AND    
C                  THAT IS USED TO STORE VARIOUS INTERMEDIATE QUANTI-   
C                  TIES.  OF PARTICULAR INTEREST ARE THE INITIALIZATION/
C                  RETURN CODE IV(1) AND THE ENTRIES IN IV THAT CONTROL 
C                  PRINTING AND LIMIT THE NUMBER OF ITERATIONS AND FUNC-
C                  TION EVALUATIONS.  SEE THE SECTION ON IV INPUT       
C                  VALUES BELOW.                                        
C LIV...... (INPUT) LENGTH OF IV ARRAY.  MUST BE AT LEAST 60.  IF LIV   
C                  IS TOO SMALL, THEN  DMNG RETURNS WITH IV(1) = 15.    
C                  WHEN  DMNG RETURNS, THE SMALLEST ALLOWED VALUE OF    
C                  LIV IS STORED IN IV(LASTIV) -- SEE THE SECTION ON    
C                  IV OUTPUT VALUES BELOW.  (THIS IS INTENDED FOR USE   
C                  WITH EXTENSIONS OF  DMNG THAT HANDLE CONSTRAINTS.)   
C LV....... (INPUT) LENGTH OF V ARRAY.  MUST BE AT LEAST 71+N*(N+15)/2. 
C                  (AT LEAST 77+N*(N+17)/2 FOR  DMNF, AT LEAST          
C                  78+N*(N+12) FOR  DMNH).  IF LV IS TOO SMALL, THEN    
C                   DMNG RETURNS WITH IV(1) = 16.  WHEN  DMNG RETURNS,  
C                  THE SMALLEST ALLOWED VALUE OF LV IS STORED IN        
C                  IV(LASTV) -- SEE THE SECTION ON IV OUTPUT VALUES     
C                  BELOW.                                               
C V........ (INPUT/OUTPUT) A FLOATING-POINT VALUE ARRAY OF LENGTH LV    
C                  (SEE BELOW) THAT HELPS CONTROL THE  DMNG ALGORITHM   
C                  AND THAT IS USED TO STORE VARIOUS INTERMEDIATE       
C                  QUANTITIES.  OF PARTICULAR INTEREST ARE THE ENTRIES  
C                  IN V THAT LIMIT THE LENGTH OF THE FIRST STEP         
C                  ATTEMPTED (LMAX0) AND SPECIFY CONVERGENCE TOLERANCES 
C                  (AFCTOL, LMAXS, RFCTOL, SCTOL, XCTOL, XFTOL).        
C UIPARM... (INPUT) USER INTEGER PARAMETER ARRAY PASSED WITHOUT CHANGE  
C                  TO CALCF AND CALCG.                                  
C URPARM... (INPUT) USER FLOATING-POINT PARAMETER ARRAY PASSED WITHOUT  
C                  CHANGE TO CALCF AND CALCG.                           
C UFPARM... (INPUT) USER EXTERNAL SUBROUTINE OR FUNCTION PASSED WITHOUT 
C                  CHANGE TO CALCF AND CALCG.                           
C                                                                       
C  ***  IV INPUT VALUES (FROM SUBROUTINE DIVSET)  ***                   
C                                                                       
C IV(1)...  ON INPUT, IV(1) SHOULD HAVE A VALUE BETWEEN 0 AND 14......  
C             0 AND 12 MEAN THIS IS A FRESH START.  0 MEANS THAT        
C                 DIVSET(2, IV, LIV, LV, V)                             
C             IS TO BE CALLED TO PROVIDE ALL DEFAULT VALUES TO IV AND   
C             V.  12 (THE VALUE THAT DIVSET ASSIGNS TO IV(1)) MEANS THE 
C             CALLER HAS ALREADY CALLED DIVSET AND HAS POSSIBLY CHANGED 
C             SOME IV AND/OR V ENTRIES TO NON-DEFAULT VALUES.           
C             13 MEANS DIVSET HAS BEEN CALLED AND THAT  DMNG (AND       
C             DRMNG) SHOULD ONLY DO THEIR STORAGE ALLOCATION.  THAT IS, 
C             THEY SHOULD SET THE OUTPUT COMPONENTS OF IV THAT TELL     
C             WHERE VARIOUS SUBARRAYS ARRAYS OF V BEGIN, SUCH AS IV(G)  
C             (AND, FOR  DMNH AND DRMNH ONLY, IV(DTOL)), AND RETURN.    
C             14 MEANS THAT A STORAGE HAS BEEN ALLOCATED (BY A CALL     
C             WITH IV(1) = 13) AND THAT THE ALGORITHM SHOULD BE         
C             STARTED.  WHEN CALLED WITH IV(1) = 13,  DMNG RETURNS      
C             IV(1) = 14 UNLESS LIV OR LV IS TOO SMALL (OR N IS NOT     
C             POSITIVE).  DEFAULT = 12.                                 
C IV(INITH).... IV(25) TELLS WHETHER THE HESSIAN APPROXIMATION H SHOULD 
C             BE INITIALIZED.  1 (THE DEFAULT) MEANS DRMNG SHOULD       
C             INITIALIZE H TO THE DIAGONAL MATRIX WHOSE I-TH DIAGONAL   
C             ELEMENT IS D(I)**2.  0 MEANS THE CALLER HAS SUPPLIED A    
C             CHOLESKY FACTOR  L  OF THE INITIAL HESSIAN APPROXIMATION  
C             H = L*(L**T)  IN V, STARTING AT V(IV(LMAT)) = V(IV(42))   
C             (AND STORED COMPACTLY BY ROWS).  NOTE THAT IV(LMAT) MAY   
C             BE INITIALIZED BY CALLING  DMNG WITH IV(1) = 13 (SEE      
C             THE IV(1) DISCUSSION ABOVE).  DEFAULT = 1.                
C IV(MXFCAL)... IV(17) GIVES THE MAXIMUM NUMBER OF FUNCTION EVALUATIONS 
C             (CALLS ON CALCF) ALLOWED.  IF THIS NUMBER DOES NOT SUF-   
C             FICE, THEN  DMNG RETURNS WITH IV(1) = 9.  DEFAULT = 200.  
C IV(MXITER)... IV(18) GIVES THE MAXIMUM NUMBER OF ITERATIONS ALLOWED.  
C             IT ALSO INDIRECTLY LIMITS THE NUMBER OF GRADIENT EVALUA-  
C             TIONS (CALLS ON CALCG) TO IV(MXITER) + 1.  IF IV(MXITER)  
C             ITERATIONS DO NOT SUFFICE, THEN  DMNG RETURNS WITH        
C             IV(1) = 10.  DEFAULT = 150.                               
C IV(OUTLEV)... IV(19) CONTROLS THE NUMBER AND LENGTH OF ITERATION SUM- 
C             MARY LINES PRINTED (BY DITSUM).  IV(OUTLEV) = 0 MEANS DO  
C             NOT PRINT ANY SUMMARY LINES.  OTHERWISE, PRINT A SUMMARY  
C             LINE AFTER EACH ABS(IV(OUTLEV)) ITERATIONS.  IF IV(OUTLEV)
C             IS POSITIVE, THEN SUMMARY LINES OF LENGTH 78 (PLUS CARRI- 
C             AGE CONTROL) ARE PRINTED, INCLUDING THE FOLLOWING...  THE 
C             ITERATION AND FUNCTION EVALUATION COUNTS, F = THE CURRENT 
C             FUNCTION VALUE, RELATIVE DIFFERENCE IN FUNCTION VALUES    
C             ACHIEVED BY THE LATEST STEP (I.E., RELDF = (F0-V(F))/F01, 
C             WHERE F01 IS THE MAXIMUM OF ABS(V(F)) AND ABS(V(F0)) AND  
C             V(F0) IS THE FUNCTION VALUE FROM THE PREVIOUS ITERA-      
C             TION), THE RELATIVE FUNCTION REDUCTION PREDICTED FOR THE  
C             STEP JUST TAKEN (I.E., PRELDF = V(PREDUC) / F01, WHERE    
C             V(PREDUC) IS DESCRIBED BELOW), THE SCALED RELATIVE CHANGE 
C             IN X (SEE V(RELDX) BELOW), THE STEP PARAMETER FOR THE     
C             STEP JUST TAKEN (STPPAR = 0 MEANS A FULL NEWTON STEP,     
C             BETWEEN 0 AND 1 MEANS A RELAXED NEWTON STEP, BETWEEN 1    
C             AND 2 MEANS A DOUBLE DOGLEG STEP, GREATER THAN 2 MEANS    
C             A SCALED DOWN CAUCHY STEP -- SEE SUBROUTINE DBLDOG), THE  
C             2-NORM OF THE SCALE VECTOR D TIMES THE STEP JUST TAKEN    
C             (SEE V(DSTNRM) BELOW), AND NPRELDF, I.E.,                 
C             V(NREDUC)/F01, WHERE V(NREDUC) IS DESCRIBED BELOW -- IF   
C             NPRELDF IS POSITIVE, THEN IT IS THE RELATIVE FUNCTION     
C             REDUCTION PREDICTED FOR A NEWTON STEP (ONE WITH           
C             STPPAR = 0).  IF NPRELDF IS NEGATIVE, THEN IT IS THE      
C             NEGATIVE OF THE RELATIVE FUNCTION REDUCTION PREDICTED     
C             FOR A STEP COMPUTED WITH STEP BOUND V(LMAXS) FOR USE IN   
C             TESTING FOR SINGULAR CONVERGENCE.                         
C                  IF IV(OUTLEV) IS NEGATIVE, THEN LINES OF LENGTH 50   
C             ARE PRINTED, INCLUDING ONLY THE FIRST 6 ITEMS LISTED      
C             ABOVE (THROUGH RELDX).                                    
C             DEFAULT = 1.                                              
C IV(PARPRT)... IV(20) = 1 MEANS PRINT ANY NONDEFAULT V VALUES ON A     
C             FRESH START OR ANY CHANGED V VALUES ON A RESTART.         
C             IV(PARPRT) = 0 MEANS SKIP THIS PRINTING.  DEFAULT = 1.    
C IV(PRUNIT)... IV(21) IS THE OUTPUT UNIT NUMBER ON WHICH ALL PRINTING  
C             IS DONE.  IV(PRUNIT) = 0 MEANS SUPPRESS ALL PRINTING.     
C             DEFAULT = STANDARD OUTPUT UNIT (UNIT 6 ON MOST SYSTEMS).  
C IV(SOLPRT)... IV(22) = 1 MEANS PRINT OUT THE VALUE OF X RETURNED (AS  
C             WELL AS THE GRADIENT AND THE SCALE VECTOR D).             
C             IV(SOLPRT) = 0 MEANS SKIP THIS PRINTING.  DEFAULT = 1.    
C IV(STATPR)... IV(23) = 1 MEANS PRINT SUMMARY STATISTICS UPON RETURN-  
C             ING.  THESE CONSIST OF THE FUNCTION VALUE, THE SCALED     
C             RELATIVE CHANGE IN X CAUSED BY THE MOST RECENT STEP (SEE  
C             V(RELDX) BELOW), THE NUMBER OF FUNCTION AND GRADIENT      
C             EVALUATIONS (CALLS ON CALCF AND CALCG), AND THE RELATIVE  
C             FUNCTION REDUCTIONS PREDICTED FOR THE LAST STEP TAKEN AND 
C             FOR A NEWTON STEP (OR PERHAPS A STEP BOUNDED BY V(LMAXS)  
C             -- SEE THE DESCRIPTIONS OF PRELDF AND NPRELDF UNDER       
C             IV(OUTLEV) ABOVE).                                        
C             IV(STATPR) = 0 MEANS SKIP THIS PRINTING.                  
C             IV(STATPR) = -1 MEANS SKIP THIS PRINTING AS WELL AS THAT  
C             OF THE ONE-LINE TERMINATION REASON MESSAGE.  DEFAULT = 1. 
C IV(X0PRT).... IV(24) = 1 MEANS PRINT THE INITIAL X AND SCALE VECTOR D 
C             (ON A FRESH START ONLY).  IV(X0PRT) = 0 MEANS SKIP THIS   
C             PRINTING.  DEFAULT = 1.                                   
C                                                                       
C  ***  (SELECTED) IV OUTPUT VALUES  ***                                
C                                                                       
C IV(1)........ ON OUTPUT, IV(1) IS A RETURN CODE....                   
C             3 = X-CONVERGENCE.  THE SCALED RELATIVE DIFFERENCE (SEE   
C                  V(RELDX)) BETWEEN THE CURRENT PARAMETER VECTOR X AND 
C                  A LOCALLY OPTIMAL PARAMETER VECTOR IS VERY LIKELY AT 
C                  MOST V(XCTOL).                                       
C             4 = RELATIVE FUNCTION CONVERGENCE.  THE RELATIVE DIFFER-  
C                  ENCE BETWEEN THE CURRENT FUNCTION VALUE AND ITS LO-  
C                  CALLY OPTIMAL VALUE IS VERY LIKELY AT MOST V(RFCTOL).
C             5 = BOTH X- AND RELATIVE FUNCTION CONVERGENCE (I.E., THE  
C                  CONDITIONS FOR IV(1) = 3 AND IV(1) = 4 BOTH HOLD).   
C             6 = ABSOLUTE FUNCTION CONVERGENCE.  THE CURRENT FUNCTION  
C                  VALUE IS AT MOST V(AFCTOL) IN ABSOLUTE VALUE.        
C             7 = SINGULAR CONVERGENCE.  THE HESSIAN NEAR THE CURRENT   
C                  ITERATE APPEARS TO BE SINGULAR OR NEARLY SO, AND A   
C                  STEP OF LENGTH AT MOST V(LMAXS) IS UNLIKELY TO YIELD 
C                  A RELATIVE FUNCTION DECREASE OF MORE THAN V(SCTOL).  
C             8 = FALSE CONVERGENCE.  THE ITERATES APPEAR TO BE CONVERG-
C                  ING TO A NONCRITICAL POINT.  THIS MAY MEAN THAT THE  
C                  CONVERGENCE TOLERANCES (V(AFCTOL), V(RFCTOL),        
C                  V(XCTOL)) ARE TOO SMALL FOR THE ACCURACY TO WHICH    
C                  THE FUNCTION AND GRADIENT ARE BEING COMPUTED, THAT   
C                  THERE IS AN ERROR IN COMPUTING THE GRADIENT, OR THAT 
C                  THE FUNCTION OR GRADIENT IS DISCONTINUOUS NEAR X.    
C             9 = FUNCTION EVALUATION LIMIT REACHED WITHOUT OTHER CON-  
C                  VERGENCE (SEE IV(MXFCAL)).                           
C            10 = ITERATION LIMIT REACHED WITHOUT OTHER CONVERGENCE     
C                  (SEE IV(MXITER)).                                    
C            11 = STOPX RETURNED .TRUE. (EXTERNAL INTERRUPT).  SEE THE  
C                  USAGE NOTES BELOW.                                   
C            14 = STORAGE HAS BEEN ALLOCATED (AFTER A CALL WITH         
C                  IV(1) = 13).                                         
C            17 = RESTART ATTEMPTED WITH N CHANGED.                     
C            18 = D HAS A NEGATIVE COMPONENT AND IV(DTYPE) .LE. 0.      
C            19...43 = V(IV(1)) IS OUT OF RANGE.                        
C            63 = F(X) CANNOT BE COMPUTED AT THE INITIAL X.             
C            64 = BAD PARAMETERS PASSED TO ASSESS (WHICH SHOULD NOT     
C                  OCCUR).                                              
C            65 = THE GRADIENT COULD NOT BE COMPUTED AT X (SEE CALCG    
C                  ABOVE).                                              
C            67 = BAD FIRST PARAMETER TO DIVSET.                        
C            80 = IV(1) WAS OUT OF RANGE.                               
C            81 = N IS NOT POSITIVE.                                    
C IV(G)........ IV(28) IS THE STARTING SUBSCRIPT IN V OF THE CURRENT    
C             GRADIENT VECTOR (THE ONE CORRESPONDING TO X).             
C IV(LASTIV)... IV(44) IS THE LEAST ACCEPTABLE VALUE OF LIV.  (IT IS    
C             ONLY SET IF LIV IS AT LEAST 44.)                          
C IV(LASTV).... IV(45) IS THE LEAST ACCEPTABLE VALUE OF LV.  (IT IS     
C             ONLY SET IF LIV IS LARGE ENOUGH, AT LEAST IV(LASTIV).)    
C IV(NFCALL)... IV(6) IS THE NUMBER OF CALLS SO FAR MADE ON CALCF (I.E.,
C             FUNCTION EVALUATIONS).                                    
C IV(NGCALL)... IV(30) IS THE NUMBER OF GRADIENT EVALUATIONS (CALLS ON  
C             CALCG).                                                   
C IV(NITER).... IV(31) IS THE NUMBER OF ITERATIONS PERFORMED.           
C                                                                       
C  ***  (SELECTED) V INPUT VALUES (FROM SUBROUTINE DIVSET)  ***         
C                                                                       
C V(BIAS)..... V(43) IS THE BIAS PARAMETER USED IN SUBROUTINE DBLDOG -- 
C             SEE THAT SUBROUTINE FOR DETAILS.  DEFAULT = 0.8.          
C V(AFCTOL)... V(31) IS THE ABSOLUTE FUNCTION CONVERGENCE TOLERANCE.    
C             IF  DMNG FINDS A POINT WHERE THE FUNCTION VALUE IS LESS   
C             THAN V(AFCTOL) IN ABSOLUTE VALUE, AND IF  DMNG DOES NOT   
C             RETURN WITH IV(1) = 3, 4, OR 5, THEN IT RETURNS WITH      
C             IV(1) = 6.  THIS TEST CAN BE TURNED OFF BY SETTING        
C             V(AFCTOL) TO ZERO.  DEFAULT = MAX(10**-20, MACHEP**2),    
C             WHERE MACHEP IS THE UNIT ROUNDOFF.                        
C V(DINIT).... V(38), IF NONNEGATIVE, IS THE VALUE TO WHICH THE SCALE   
C             VECTOR D IS INITIALIZED.  DEFAULT = -1.                   
C V(LMAX0).... V(35) GIVES THE MAXIMUM 2-NORM ALLOWED FOR D TIMES THE   
C             VERY FIRST STEP THAT  DMNG ATTEMPTS.  THIS PARAMETER CAN  
C             MARKEDLY AFFECT THE PERFORMANCE OF  DMNG.                 
C V(LMAXS).... V(36) IS USED IN TESTING FOR SINGULAR CONVERGENCE -- IF  
C             THE FUNCTION REDUCTION PREDICTED FOR A STEP OF LENGTH     
C             BOUNDED BY V(LMAXS) IS AT MOST V(SCTOL) * ABS(F0), WHERE  
C             F0  IS THE FUNCTION VALUE AT THE START OF THE CURRENT     
C             ITERATION, AND IF  DMNG DOES NOT RETURN WITH IV(1) = 3,   
C             4, 5, OR 6, THEN IT RETURNS WITH IV(1) = 7.  DEFAULT = 1. 
C V(RFCTOL)... V(32) IS THE RELATIVE FUNCTION CONVERGENCE TOLERANCE.    
C             IF THE CURRENT MODEL PREDICTS A MAXIMUM POSSIBLE FUNCTION 
C             REDUCTION (SEE V(NREDUC)) OF AT MOST V(RFCTOL)*ABS(F0)    
C             AT THE START OF THE CURRENT ITERATION, WHERE  F0  IS THE  
C             THEN CURRENT FUNCTION VALUE, AND IF THE LAST STEP ATTEMPT-
C             ED ACHIEVED NO MORE THAN TWICE THE PREDICTED FUNCTION     
C             DECREASE, THEN  DMNG RETURNS WITH IV(1) = 4 (OR 5).       
C             DEFAULT = MAX(10**-10, MACHEP**(2/3)), WHERE MACHEP IS    
C             THE UNIT ROUNDOFF.                                        
C V(SCTOL).... V(37) IS THE SINGULAR CONVERGENCE TOLERANCE -- SEE THE   
C             DESCRIPTION OF V(LMAXS) ABOVE.                            
C V(TUNER1)... V(26) HELPS DECIDE WHEN TO CHECK FOR FALSE CONVERGENCE.  
C             THIS IS DONE IF THE ACTUAL FUNCTION DECREASE FROM THE     
C             CURRENT STEP IS NO MORE THAN V(TUNER1) TIMES ITS PREDICT- 
C             ED VALUE.  DEFAULT = 0.1.                                 
C V(XCTOL).... V(33) IS THE X-CONVERGENCE TOLERANCE.  IF A NEWTON STEP  
C             (SEE V(NREDUC)) IS TRIED THAT HAS V(RELDX) .LE. V(XCTOL)  
C             AND IF THIS STEP YIELDS AT MOST TWICE THE PREDICTED FUNC- 
C             TION DECREASE, THEN  DMNG RETURNS WITH IV(1) = 3 (OR 5).  
C             (SEE THE DESCRIPTION OF V(RELDX) BELOW.)                  
C             DEFAULT = MACHEP**0.5, WHERE MACHEP IS THE UNIT ROUNDOFF. 
C V(XFTOL).... V(34) IS THE FALSE CONVERGENCE TOLERANCE.  IF A STEP IS  
C             TRIED THAT GIVES NO MORE THAN V(TUNER1) TIMES THE PREDICT-
C             ED FUNCTION DECREASE AND THAT HAS V(RELDX) .LE. V(XFTOL), 
C             AND IF  DMNG DOES NOT RETURN WITH IV(1) = 3, 4, 5, 6, OR  
C             7, THEN IT RETURNS WITH IV(1) = 8.  (SEE THE DESCRIPTION  
C             OF V(RELDX) BELOW.)  DEFAULT = 100*MACHEP, WHERE          
C             MACHEP IS THE UNIT ROUNDOFF.                              
C V(*)........DIVSET SUPPLIES TO V A NUMBER OF TUNING CONSTANTS, WITH   
C             WHICH IT SHOULD ORDINARILY BE UNNECESSARY TO TINKER.  SEE 
C             SECTION 17 OF VERSION 2.2 OF THE NL2SOL USAGE SUMMARY     
C             (I.E., THE APPENDIX TO REF. 1) FOR DETAILS ON V(I),       
C             I = DECFAC, INCFAC, PHMNFC, PHMXFC, RDFCMN, RDFCMX,       
C             TUNER2, TUNER3, TUNER4, TUNER5.                           
C                                                                       
C  ***  (SELECTED) V OUTPUT VALUES  ***                                 
C                                                                       
C V(DGNORM)... V(1) IS THE 2-NORM OF (DIAG(D)**-1)*G, WHERE G IS THE    
C             MOST RECENTLY COMPUTED GRADIENT.                          
C V(DSTNRM)... V(2) IS THE 2-NORM OF DIAG(D)*STEP, WHERE STEP IS THE    
C             CURRENT STEP.                                             
C V(F)........ V(10) IS THE CURRENT FUNCTION VALUE.                     
C V(F0)....... V(13) IS THE FUNCTION VALUE AT THE START OF THE CURRENT  
C             ITERATION.                                                
C V(NREDUC)... V(6), IF POSITIVE, IS THE MAXIMUM FUNCTION REDUCTION     
C             POSSIBLE ACCORDING TO THE CURRENT MODEL, I.E., THE FUNC-  
C             TION REDUCTION PREDICTED FOR A NEWTON STEP (I.E.,         
C             STEP = -H**-1 * G,  WHERE  G  IS THE CURRENT GRADIENT AND 
C             H IS THE CURRENT HESSIAN APPROXIMATION).                  
C                  IF V(NREDUC) IS NEGATIVE, THEN IT IS THE NEGATIVE OF 
C             THE FUNCTION REDUCTION PREDICTED FOR A STEP COMPUTED WITH 
C             A STEP BOUND OF V(LMAXS) FOR USE IN TESTING FOR SINGULAR  
C             CONVERGENCE.                                              
C V(PREDUC)... V(7) IS THE FUNCTION REDUCTION PREDICTED (BY THE CURRENT 
C             QUADRATIC MODEL) FOR THE CURRENT STEP.  THIS (DIVIDED BY  
C             V(F0)) IS USED IN TESTING FOR RELATIVE FUNCTION           
C             CONVERGENCE.                                              
C V(RELDX).... V(17) IS THE SCALED RELATIVE CHANGE IN X CAUSED BY THE   
C             CURRENT STEP, COMPUTED AS                                 
C                  MAX(ABS(D(I)*(X(I)-X0(I)), 1 .LE. I .LE. P) /        
C                     MAX(D(I)*(ABS(X(I))+ABS(X0(I))), 1 .LE. I .LE. P),
C             WHERE X = X0 + STEP.                                      
C                                                                       
C-------------------------------  NOTES  -------------------------------
C                                                                       
C  ***  ALGORITHM NOTES  ***                                            
C                                                                       
C        THIS ROUTINE USES A HESSIAN APPROXIMATION COMPUTED FROM THE    
C     BFGS UPDATE (SEE REF 3).  ONLY A CHOLESKY FACTOR OF THE HESSIAN   
C     APPROXIMATION IS STORED, AND THIS IS UPDATED USING IDEAS FROM     
C     REF. 4.  STEPS ARE COMPUTED BY THE DOUBLE DOGLEG SCHEME DESCRIBED 
C     IN REF. 2.  THE STEPS ARE ASSESSED AS IN REF. 1.                  
C                                                                       
C  ***  USAGE NOTES  ***                                                
C                                                                       
C        AFTER A RETURN WITH IV(1) .LE. 11, IT IS POSSIBLE TO RESTART,  
C     I.E., TO CHANGE SOME OF THE IV AND V INPUT VALUES DESCRIBED ABOVE 
C     AND CONTINUE THE ALGORITHM FROM THE POINT WHERE IT WAS INTERRUPT- 
C     ED.  IV(1) SHOULD NOT BE CHANGED, NOR SHOULD ANY ENTRIES OF IV    
C     AND V OTHER THAN THE INPUT VALUES (THOSE SUPPLIED BY DIVSET).     
C        THOSE WHO DO NOT WISH TO WRITE A CALCG WHICH COMPUTES THE      
C     GRADIENT ANALYTICALLY SHOULD CALL  DMNF RATHER THAN  DMNG.        
C      DMNF USES FINITE DIFFERENCES TO COMPUTE AN APPROXIMATE GRADIENT. 
C        THOSE WHO WOULD PREFER TO PROVIDE F AND G (THE FUNCTION AND    
C     GRADIENT) BY REVERSE COMMUNICATION RATHER THAN BY WRITING SUBROU- 
C     TINES CALCF AND CALCG MAY CALL ON DRMNG DIRECTLY.  SEE THE COM-   
C     MENTS AT THE BEGINNING OF DRMNG.                                  
C        THOSE WHO USE  DMNG INTERACTIVELY MAY WISH TO SUPPLY THEIR     
C     OWN STOPX FUNCTION, WHICH SHOULD RETURN .TRUE. IF THE BREAK KEY   
C     HAS BEEN PRESSED SINCE STOPX WAS LAST INVOKED.  THIS MAKES IT     
C     POSSIBLE TO EXTERNALLY INTERRUPT  DMNG (WHICH WILL RETURN WITH    
C     IV(1) = 11 IF STOPX RETURNS .TRUE.).                              
C        STORAGE FOR G IS ALLOCATED AT THE END OF V.  THUS THE CALLER   
C     MAY MAKE V LONGER THAN SPECIFIED ABOVE AND MAY ALLOW CALCG TO USE 
C     ELEMENTS OF G BEYOND THE FIRST N AS SCRATCH STORAGE.              
C                                                                       
C  ***  PORTABILITY NOTES  ***                                          
C                                                                       
C        THE  DMNG DISTRIBUTION TAPE CONTAINS BOTH SINGLE- AND DOUBLE-  
C     PRECISION VERSIONS OF THE  DMNG SOURCE CODE, SO IT SHOULD BE UN-  
C     NECESSARY TO CHANGE PRECISIONS.                                   
C        ONLY THE FUNCTIONS I7MDCN AND DR7MDC CONTAIN MACHINE-DEPENDENT 
C     CONSTANTS.  TO CHANGE FROM ONE MACHINE TO ANOTHER, IT SHOULD      
C     SUFFICE TO CHANGE THE (FEW) RELEVANT LINES IN THESE FUNCTIONS.    
C        INTRINSIC FUNCTIONS ARE EXPLICITLY DECLARED.  ON CERTAIN COM-  
C     PUTERS (E.G. UNIVAC), IT MAY BE NECESSARY TO COMMENT OUT THESE    
C     DECLARATIONS.  SO THAT THIS MAY BE DONE AUTOMATICALLY BY A SIMPLE 
C     PROGRAM, SUCH DECLARATIONS ARE PRECEDED BY A COMMENT HAVING C/+   
C     IN COLUMNS 1-3 AND BLANKS IN COLUMNS 4-72 AND ARE FOLLOWED BY     
C     A COMMENT HAVING C/ IN COLUMNS 1 AND 2 AND BLANKS IN COLUMNS 3-72.
C        THE  DMNG SOURCE CODE IS EXPRESSED IN 1966 ANSI STANDARD       
C     FORTRAN.  IT MAY BE CONVERTED TO FORTRAN 77 BY COMMENTING OUT ALL 
C     LINES THAT FALL BETWEEN A LINE HAVING C/6 IN COLUMNS 1-3 AND A    
C     LINE HAVING C/7 IN COLUMNS 1-3 AND BY REMOVING (I.E., REPLACING   
C     BY A BLANK) THE C IN COLUMN 1 OF THE LINES THAT FOLLOW THE C/7    
C     LINE AND PRECEDE A LINE HAVING C/ IN COLUMNS 1-2 AND BLANKS IN    
C     COLUMNS 3-72.  THESE CHANGES CONVERT SOME DATA STATEMENTS INTO    
C     PARAMETER STATEMENTS, CONVERT SOME VARIABLES FROM REAL TO         
C     CHARACTER*4, AND MAKE THE DATA STATEMENTS THAT INITIALIZE THESE   
C     VARIABLES USE CHARACTER STRINGS DELIMITED BY PRIMES INSTEAD       
C     OF HOLLERITH CONSTANTS.  (SUCH VARIABLES AND DATA STATEMENTS      
C     APPEAR ONLY IN MODULES DITSUM AND DPARCK.  PARAMETER STATEMENTS   
C     APPEAR NEARLY EVERYWHERE.)  THESE CHANGES ALSO ADD SAVE STATE-    
C     MENTS FOR VARIABLES GIVEN MACHINE-DEPENDENT CONSTANTS BY DR7MDC.  
C                                                                       
C  ***  REFERENCES  ***                                                 
C                                                                       
C 1.  DENNIS, J.E., GAY, D.M., AND WELSCH, R.E. (1981), ALGORITHM 573 --
C             AN ADAPTIVE NONLINEAR LEAST-SQUARES ALGORITHM, ACM TRANS. 
C             MATH. SOFTWARE 7, PP. 369-383.                            
C                                                                       
C 2.  DENNIS, J.E., AND MEI, H.H.W. (1979), TWO NEW UNCONSTRAINED OPTI- 
C             MIZATION ALGORITHMS WHICH USE FUNCTION AND GRADIENT       
C             VALUES, J. OPTIM. THEORY APPLIC. 28, PP. 453-482.         
C                                                                       
C 3.  DENNIS, J.E., AND MORE, J.J. (1977), QUASI-NEWTON METHODS, MOTIVA-
C             TION AND THEORY, SIAM REV. 19, PP. 46-89.                 
C                                                                       
C 4.  GOLDFARB, D. (1976), FACTORIZED VARIABLE METRIC METHODS FOR UNCON-
C             STRAINED OPTIMIZATION, MATH. COMPUT. 30, PP. 796-811.     
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY (WINTER 1980).  REVISED SUMMER 1982.        
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH           
C     SUPPORTED IN PART BY THE NATIONAL SCIENCE FOUNDATION UNDER        
C     GRANTS MCS-7600324, DCR75-10143, 76-14311DSS, MCS76-11989,        
C     AND MCS-7906671.                                                  
C.                                                                      
C                                                                       
C----------------------------  DECLARATIONS  ---------------------------
C                                                                       
      EXTERNAL DIVSET, DRMNG                                            
C                                                                       
C DIVSET... SUPPLIES DEFAULT IV AND V INPUT COMPONENTS.                 
C DRMNG... REVERSE-COMMUNICATION ROUTINE THAT CARRIES OUT  DMNG ALGO-   
C             RITHM.                                                    
C                                                                       
      INTEGER G1, IV1, NF                                               
      DOUBLE PRECISION F                                                
C                                                                       
C  ***  SUBSCRIPTS FOR IV   ***                                         
C                                                                       
      INTEGER NEXTV, NFCALL, NFGCAL, G, TOOBIG, VNEED                   
C                                                                       
C/6                                                                     
C     DATA NEXTV/47/, NFCALL/6/, NFGCAL/7/, G/28/, TOOBIG/2/, VNEED/4/  
C/7                                                                     
      PARAMETER (NEXTV=47, NFCALL=6, NFGCAL=7, G=28, TOOBIG=2, VNEED=4) 
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(2, IV, LIV, LV, V)                  
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 12 .OR. IV1 .EQ. 13) IV(VNEED) = IV(VNEED) + N       
      IF (IV1 .EQ. 14) GO TO 10                                         
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10                        
      G1 = 1                                                            
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      GO TO 20                                                          
C                                                                       
 10   G1 = IV(G)                                                        
C                                                                       
 20   CALL DRMNG(D, F, V(G1), IV, LIV, LV, N, V, X)                     
      IF (IV(1) - 2) 30, 40, 50                                         
C                                                                       
 30   NF = IV(NFCALL)                                                   
      CALL CALCF(N, X, NF, F, UIPARM, URPARM, UFPARM)                   
      IF (NF .LE. 0) IV(TOOBIG) = 1                                     
      GO TO 20                                                          
C                                                                       
 40   NF = IV(NFGCAL)                                                   
      CALL CALCG(N, X, NF, V(G1), UIPARM, URPARM, UFPARM)               
      IF (NF .LE. 0) IV(TOOBIG) = 1                                     
      GO TO 20                                                          
C                                                                       
 50   IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION                                              
C                                                                       
      IV(G) = IV(NEXTV)                                                 
      IV(NEXTV) = IV(G) + N                                             
      IF (IV1 .NE. 13) GO TO 10                                         
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF  DMNG FOLLOWS  ***                                 
      END                                                               
      SUBROUTINE  DMNGB(N, D, X, B, CALCF, CALCG, IV, LIV, LV, V,       
     1                  UIPARM, URPARM, UFPARM)                         
C                                                                       
C  ***  MINIMIZE GENERAL SIMPLY BOUNDED OBJECTIVE FUNCTION USING  ***   
C  ***  ANALYTIC GRADIENT AND HESSIAN APPROX. FROM SECANT UPDATE  ***   
C                                                                       
      INTEGER N, LIV, LV                                                
      INTEGER IV(LIV), UIPARM(1)                                        
      DOUBLE PRECISION D(N), X(N), B(2,N), V(LV), URPARM(1)             
C     DIMENSION V(71 + N*(N+15)/2), UIPARM(*), URPARM(*)                
      EXTERNAL CALCF, CALCG, UFPARM                                     
C                                                                       
C  ***  DISCUSSION  ***                                                 
C                                                                       
C        THIS ROUTINE IS LIKE  DMNG, EXCEPT FOR THE EXTRA PARAMETER B,  
C     AN ARRAY OF LOWER AND UPPER BOUNDS ON X...  DMNGB ENFORCES THE    
C     CONSTRAINTS THAT  B(1,I) .LE. X(I) .LE. B(2,I), I = 1(1)N.        
C     (INSTEAD OF CALLING DRMNG,  DMNGB CALLS DRMNGB.)                  
C.                                                                      
C                                                                       
C----------------------------  DECLARATIONS  ---------------------------
C                                                                       
      EXTERNAL DIVSET, DRMNGB                                           
C                                                                       
C DIVSET.... SUPPLIES DEFAULT IV AND V INPUT COMPONENTS.                
C DRMNGB... REVERSE-COMMUNICATION ROUTINE THAT CARRIES OUT  DMNG ALGO-  
C             RITHM.                                                    
C                                                                       
      INTEGER G1, IV1, NF                                               
      DOUBLE PRECISION F                                                
C                                                                       
C  ***  SUBSCRIPTS FOR IV   ***                                         
C                                                                       
      INTEGER NEXTV, NFCALL, NFGCAL, G, TOOBIG, VNEED                   
C                                                                       
C/6                                                                     
C     DATA NEXTV/47/, NFCALL/6/, NFGCAL/7/, G/28/, TOOBIG/2/, VNEED/4/  
C/7                                                                     
      PARAMETER (NEXTV=47, NFCALL=6, NFGCAL=7, G=28, TOOBIG=2, VNEED=4) 
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(2, IV, LIV, LV, V)                  
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 14) GO TO 10                                         
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10                        
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      IF (IV(1) .EQ. 13) IV(VNEED) = IV(VNEED) + N                      
      CALL DRMNGB(B, D, F, V, IV, LIV, LV, N, V, X)                     
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION                                              
C                                                                       
      IV(G) = IV(NEXTV)                                                 
      IV(NEXTV) = IV(G) + N                                             
      IF (IV1 .EQ. 13) GO TO 999                                        
C                                                                       
 10   G1 = IV(G)                                                        
C                                                                       
 20   CALL DRMNGB(B, D, F, V(G1), IV, LIV, LV, N, V, X)                 
      IF (IV(1) - 2) 30, 40, 999                                        
C                                                                       
 30   NF = IV(NFCALL)                                                   
      CALL CALCF(N, X, NF, F, UIPARM, URPARM, UFPARM)                   
      IF (NF .LE. 0) IV(TOOBIG) = 1                                     
      GO TO 20                                                          
C                                                                       
 40   NF = IV(NFGCAL)                                                   
      CALL CALCG(N, X, NF, V(G1), UIPARM, URPARM, UFPARM)               
      IF (NF .LE. 0) IV(TOOBIG) = 1                                     
      GO TO 20                                                          
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF  DMNGB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  DMNH(N, D, X, CALCF, CALCGH, IV, LIV, LV, V,          
     1                  UIPARM, URPARM, UFPARM)                         
C                                                                       
C  ***  MINIMIZE GENERAL UNCONSTRAINED OBJECTIVE FUNCTION USING   ***   
C  ***  (ANALYTIC) GRADIENT AND HESSIAN PROVIDED BY THE CALLER.   ***   
C                                                                       
      INTEGER LIV, LV, N                                                
      INTEGER IV(LIV), UIPARM(1)                                        
      DOUBLE PRECISION D(N), X(N), V(LV), URPARM(1)                     
C     DIMENSION V(78 + N*(N+12)), UIPARM(*), URPARM(*)                  
      EXTERNAL CALCF, CALCGH, UFPARM                                    
C                                                                       
C------------------------------  DISCUSSION  ---------------------------
C                                                                       
C        THIS ROUTINE IS LIKE  DMNG, EXCEPT THAT THE SUBROUTINE PARA-   
C     METER CALCG OF  DMNG (WHICH COMPUTES THE GRADIENT OF THE OBJEC-   
C     TIVE FUNCTION) IS REPLACED BY THE SUBROUTINE PARAMETER CALCGH,    
C     WHICH COMPUTES BOTH THE GRADIENT AND (LOWER TRIANGLE OF THE)      
C     HESSIAN OF THE OBJECTIVE FUNCTION.  THE CALLING SEQUENCE IS...    
C             CALL CALCGH(N, X, NF, G, H, UIPARM, URPARM, UFPARM)       
C     PARAMETERS N, X, NF, G, UIPARM, URPARM, AND UFPARM ARE THE SAME   
C     AS FOR  DMNG, WHILE H IS AN ARRAY OF LENGTH N*(N+1)/2 IN WHICH    
C     CALCGH MUST STORE THE LOWER TRIANGLE OF THE HESSIAN AT X.  START- 
C     ING AT H(1), CALCGH MUST STORE THE HESSIAN ENTRIES IN THE ORDER   
C     (1,1), (2,1), (2,2), (3,1), (3,2), (3,3), ...                     
C        THE VALUE PRINTED (BY DITSUM) IN THE COLUMN LABELLED STPPAR    
C     IS THE LEVENBERG-MARQUARDT USED IN COMPUTING THE CURRENT STEP.    
C     ZERO MEANS A FULL NEWTON STEP.  IF THE SPECIAL CASE DESCRIBED IN  
C     REF. 1 IS DETECTED, THEN STPPAR IS NEGATED.  THE VALUE PRINTED    
C     IN THE COLUMN LABELLED NPRELDF IS ZERO IF THE CURRENT HESSIAN     
C     IS NOT POSITIVE DEFINITE.                                         
C        IT SOMETIMES PROVES WORTHWHILE TO LET D BE DETERMINED FROM THE 
C     DIAGONAL OF THE HESSIAN MATRIX BY SETTING IV(DTYPE) = 1 AND       
C     V(DINIT) = 0.  THE FOLLOWING IV AND V COMPONENTS ARE RELEVANT...  
C                                                                       
C IV(DTOL)..... IV(59) GIVES THE STARTING SUBSCRIPT IN V OF THE DTOL    
C             ARRAY USED WHEN D IS UPDATED.  (IV(DTOL) CAN BE           
C             INITIALIZED BY CALLING  DMNH WITH IV(1) = 13.)            
C IV(DTYPE).... IV(16) TELLS HOW THE SCALE VECTOR D SHOULD BE CHOSEN.   
C             IV(DTYPE) .LE. 0 MEANS THAT D SHOULD NOT BE UPDATED, AND  
C             IV(DTYPE) .GE. 1 MEANS THAT D SHOULD BE UPDATED AS        
C             DESCRIBED BELOW WITH V(DFAC).  DEFAULT = 0.               
C V(DFAC)..... V(41) AND THE DTOL AND D0 ARRAYS (SEE V(DTINIT) AND      
C             V(D0INIT)) ARE USED IN UPDATING THE SCALE VECTOR D WHEN   
C             IV(DTYPE) .GT. 0.  (D IS INITIALIZED ACCORDING TO         
C             V(DINIT), DESCRIBED IN  DMNG.)  LET                       
C                  D1(I) = MAX(SQRT(ABS(H(I,I))), V(DFAC)*D(I)),        
C             WHERE H(I,I) IS THE I-TH DIAGONAL ELEMENT OF THE CURRENT  
C             HESSIAN.  IF IV(DTYPE) = 1, THEN D(I) IS SET TO D1(I)     
C             UNLESS D1(I) .LT. DTOL(I), IN WHICH CASE D(I) IS SET TO   
C                  MAX(D0(I), DTOL(I)).                                 
C             IF IV(DTYPE) .GE. 2, THEN D IS UPDATED DURING THE FIRST   
C             ITERATION AS FOR IV(DTYPE) = 1 (AFTER ANY INITIALIZATION  
C             DUE TO V(DINIT)) AND IS LEFT UNCHANGED THEREAFTER.        
C             DEFAULT = 0.6.                                            
C V(DTINIT)... V(39), IF POSITIVE, IS THE VALUE TO WHICH ALL COMPONENTS 
C             OF THE DTOL ARRAY (SEE V(DFAC)) ARE INITIALIZED.  IF      
C             V(DTINIT) = 0, THEN IT IS ASSUMED THAT THE CALLER HAS     
C             STORED DTOL IN V STARTING AT V(IV(DTOL)).                 
C             DEFAULT = 10**-6.                                         
C V(D0INIT)... V(40), IF POSITIVE, IS THE VALUE TO WHICH ALL COMPONENTS 
C             OF THE D0 VECTOR (SEE V(DFAC)) ARE INITIALIZED.  IF       
C             V(DFAC) = 0, THEN IT IS ASSUMED THAT THE CALLER HAS       
C             STORED D0 IN V STARTING AT V(IV(DTOL)+N).  DEFAULT = 1.0. 
C                                                                       
C  ***  REFERENCE  ***                                                  
C                                                                       
C 1. GAY, D.M. (1981), COMPUTING OPTIMAL LOCALLY CONSTRAINED STEPS,     
C         SIAM J. SCI. STATIST. COMPUT. 2, PP. 186-197.                 
C.                                                                      
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY (WINTER 1980).  REVISED SEPT. 1982.         
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH SUPPORTED 
C     IN PART BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS           
C     MCS-7600324 AND MCS-7906671.                                      
C                                                                       
C----------------------------  DECLARATIONS  ---------------------------
C                                                                       
      EXTERNAL DIVSET, DRMNH                                            
C                                                                       
C DIVSET... PROVIDES DEFAULT INPUT VALUES FOR IV AND V.                 
C DRMNH... REVERSE-COMMUNICATION ROUTINE THAT DOES  DMNH ALGORITHM.     
C                                                                       
      INTEGER G1, H1, IV1, LH, NF                                       
      DOUBLE PRECISION F                                                
C                                                                       
C  ***  SUBSCRIPTS FOR IV   ***                                         
C                                                                       
      INTEGER G, H, NEXTV, NFCALL, NFGCAL, TOOBIG, VNEED                
C                                                                       
C/6                                                                     
C     DATA NEXTV/47/, NFCALL/6/, NFGCAL/7/, G/28/, H/56/, TOOBIG/2/,    
C    1     VNEED/4/                                                     
C/7                                                                     
      PARAMETER (NEXTV=47, NFCALL=6, NFGCAL=7, G=28, H=56, TOOBIG=2,    
     1           VNEED=4)                                               
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      LH = N * (N + 1) / 2                                              
      IF (IV(1) .EQ. 0) CALL DIVSET(2, IV, LIV, LV, V)                  
      IF (IV(1) .EQ. 12 .OR. IV(1) .EQ. 13)                             
     1     IV(VNEED) = IV(VNEED) + N*(N+3)/2                            
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 14) GO TO 10                                         
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10                        
      G1 = 1                                                            
      H1 = 1                                                            
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      GO TO 20                                                          
C                                                                       
 10   G1 = IV(G)                                                        
      H1 = IV(H)                                                        
C                                                                       
 20   CALL DRMNH(D, F, V(G1), V(H1), IV, LH, LIV, LV, N, V, X)          
      IF (IV(1) - 2) 30, 40, 50                                         
C                                                                       
 30   NF = IV(NFCALL)                                                   
      CALL CALCF(N, X, NF, F, UIPARM, URPARM, UFPARM)                   
      IF (NF .LE. 0) IV(TOOBIG) = 1                                     
      GO TO 20                                                          
C                                                                       
 40   NF = IV(NFGCAL)                                                   
      CALL CALCGH(N, X, NF, V(G1), V(H1), UIPARM, URPARM, UFPARM)       
      IF (NF .LE. 0) IV(TOOBIG) = 1                                     
      GO TO 20                                                          
C                                                                       
 50   IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION                                              
C                                                                       
      IV(G) = IV(NEXTV)                                                 
      IV(H) = IV(G) + N                                                 
      IV(NEXTV) = IV(H) + N*(N+1)/2                                     
      IF (IV1 .NE. 13) GO TO 10                                         
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF  DMNH FOLLOWS  ***                                 
      END                                                               
      SUBROUTINE  DMNHB(N, D, X, B, CALCF, CALCGH, IV, LIV, LV, V,      
     1                  UIPARM, URPARM, UFPARM)                         
C                                                                       
C  ***  MINIMIZE GENERAL SIMPLY BOUNDED OBJECTIVE FUNCTION USING   ***  
C  ***  (ANALYTIC) GRADIENT AND HESSIAN PROVIDED BY THE CALLER.    ***  
C                                                                       
      INTEGER LIV, LV, N                                                
      INTEGER IV(LIV), UIPARM(1)                                        
      DOUBLE PRECISION B(2,N), D(N), X(N), V(LV), URPARM(1)             
C     DIMENSION V(78 + N*(N+12)), UIPARM(*), URPARM(*)                  
      EXTERNAL CALCF, CALCGH, UFPARM                                    
C                                                                       
C------------------------------  DISCUSSION  ---------------------------
C                                                                       
C        THIS ROUTINE IS LIKE  DMNGB, EXCEPT THAT THE SUBROUTINE PARA-  
C     METER CALCG OF  DMNGB (WHICH COMPUTES THE GRADIENT OF THE OBJEC-  
C     TIVE FUNCTION) IS REPLACED BY THE SUBROUTINE PARAMETER CALCGH,    
C     WHICH COMPUTES BOTH THE GRADIENT AND (LOWER TRIANGLE OF THE)      
C     HESSIAN OF THE OBJECTIVE FUNCTION.  THE CALLING SEQUENCE IS...    
C             CALL CALCGH(N, X, NF, G, H, UIPARM, URPARM, UFPARM)       
C     PARAMETERS N, X, NF, G, UIPARM, URPARM, AND UFPARM ARE THE SAME   
C     AS FOR  DMNGB, WHILE H IS AN ARRAY OF LENGTH N*(N+1)/2 IN WHICH   
C     CALCGH MUST STORE THE LOWER TRIANGLE OF THE HESSIAN AT X.  START- 
C     ING AT H(1), CALCGH MUST STORE THE HESSIAN ENTRIES IN THE ORDER   
C     (1,1), (2,1), (2,2), (3,1), (3,2), (3,3), ...                     
C        THE VALUE PRINTED (BY DITSUM) IN THE COLUMN LABELLED STPPAR    
C     IS THE LEVENBERG-MARQUARDT USED IN COMPUTING THE CURRENT STEP.    
C     ZERO MEANS A FULL NEWTON STEP.  IF THE SPECIAL CASE DESCRIBED IN  
C     REF. 1 IS DETECTED, THEN STPPAR IS NEGATED.  THE VALUE PRINTED    
C     IN THE COLUMN LABELLED NPRELDF IS ZERO IF THE CURRENT HESSIAN     
C     IS NOT POSITIVE DEFINITE.                                         
C        IT SOMETIMES PROVES WORTHWHILE TO LET D BE DETERMINED FROM THE 
C     DIAGONAL OF THE HESSIAN MATRIX BY SETTING IV(DTYPE) = 1 AND       
C     V(DINIT) = 0.  THE FOLLOWING IV AND V COMPONENTS ARE RELEVANT...  
C                                                                       
C IV(DTOL)..... IV(59) GIVES THE STARTING SUBSCRIPT IN V OF THE DTOL    
C             ARRAY USED WHEN D IS UPDATED.  (IV(DTOL) CAN BE           
C             INITIALIZED BY CALLING  DMNHB WITH IV(1) = 13.)           
C IV(DTYPE).... IV(16) TELLS HOW THE SCALE VECTOR D SHOULD BE CHOSEN.   
C             IV(DTYPE) .LE. 0 MEANS THAT D SHOULD NOT BE UPDATED, AND  
C             IV(DTYPE) .GE. 1 MEANS THAT D SHOULD BE UPDATED AS        
C             DESCRIBED BELOW WITH V(DFAC).  DEFAULT = 0.               
C V(DFAC)..... V(41) AND THE DTOL AND D0 ARRAYS (SEE V(DTINIT) AND      
C             V(D0INIT)) ARE USED IN UPDATING THE SCALE VECTOR D WHEN   
C             IV(DTYPE) .GT. 0.  (D IS INITIALIZED ACCORDING TO         
C             V(DINIT), DESCRIBED IN  DMNG.)  LET                       
C                  D1(I) = MAX(SQRT(ABS(H(I,I))), V(DFAC)*D(I)),        
C             WHERE H(I,I) IS THE I-TH DIAGONAL ELEMENT OF THE CURRENT  
C             HESSIAN.  IF IV(DTYPE) = 1, THEN D(I) IS SET TO D1(I)     
C             UNLESS D1(I) .LT. DTOL(I), IN WHICH CASE D(I) IS SET TO   
C                  MAX(D0(I), DTOL(I)).                                 
C             IF IV(DTYPE) .GE. 2, THEN D IS UPDATED DURING THE FIRST   
C             ITERATION AS FOR IV(DTYPE) = 1 (AFTER ANY INITIALIZATION  
C             DUE TO V(DINIT)) AND IS LEFT UNCHANGED THEREAFTER.        
C             DEFAULT = 0.6.                                            
C V(DTINIT)... V(39), IF POSITIVE, IS THE VALUE TO WHICH ALL COMPONENTS 
C             OF THE DTOL ARRAY (SEE V(DFAC)) ARE INITIALIZED.  IF      
C             V(DTINIT) = 0, THEN IT IS ASSUMED THAT THE CALLER HAS     
C             STORED DTOL IN V STARTING AT V(IV(DTOL)).                 
C             DEFAULT = 10**-6.                                         
C V(D0INIT)... V(40), IF POSITIVE, IS THE VALUE TO WHICH ALL COMPONENTS 
C             OF THE D0 VECTOR (SEE V(DFAC)) ARE INITIALIZED.  IF       
C             V(DFAC) = 0, THEN IT IS ASSUMED THAT THE CALLER HAS       
C             STORED D0 IN V STARTING AT V(IV(DTOL)+N).  DEFAULT = 1.0. 
C                                                                       
C  ***  REFERENCE  ***                                                  
C                                                                       
C 1. GAY, D.M. (1981), COMPUTING OPTIMAL LOCALLY CONSTRAINED STEPS,     
C         SIAM J. SCI. STATIST. COMPUT. 2, PP. 186-197.                 
C.                                                                      
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY (WINTER, SPRING 1983).                      
C                                                                       
C----------------------------  DECLARATIONS  ---------------------------
C                                                                       
      EXTERNAL DIVSET, DRMNHB                                           
C                                                                       
C DIVSET.... PROVIDES DEFAULT INPUT VALUES FOR IV AND V.                
C DRMNHB... REVERSE-COMMUNICATION ROUTINE THAT DOES  DMNHB ALGORITHM.   
C                                                                       
      INTEGER G1, H1, IV1, LH, NF                                       
      DOUBLE PRECISION F                                                
C                                                                       
C  ***  SUBSCRIPTS FOR IV   ***                                         
C                                                                       
      INTEGER G, H, NEXTV, NFCALL, NFGCAL, TOOBIG, VNEED                
C                                                                       
C/6                                                                     
C     DATA NEXTV/47/, NFCALL/6/, NFGCAL/7/, G/28/, H/56/, TOOBIG/2/,    
C    1     VNEED/4/                                                     
C/7                                                                     
      PARAMETER (NEXTV=47, NFCALL=6, NFGCAL=7, G=28, H=56, TOOBIG=2,    
     1           VNEED=4)                                               
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      LH = N * (N + 1) / 2                                              
      IF (IV(1) .EQ. 0) CALL DIVSET(2, IV, LIV, LV, V)                  
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 14) GO TO 10                                         
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10                        
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      IF (IV(1) .EQ. 13) IV(VNEED) = IV(VNEED) + N*(N+3)/2              
      CALL DRMNHB(B, D, F, V, V, IV, LH, LIV, LV, N, V, X)              
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION                                              
C                                                                       
      IV(G) = IV(NEXTV)                                                 
      IV(H) = IV(G) + N                                                 
      IV(NEXTV) = IV(H) + N*(N+1)/2                                     
      IF (IV1 .EQ. 13) GO TO 999                                        
C                                                                       
 10   G1 = IV(G)                                                        
      H1 = IV(H)                                                        
C                                                                       
 20   CALL DRMNHB(B, D, F, V(G1), V(H1), IV, LH, LIV, LV, N, V, X)      
      IF (IV(1) - 2) 30, 40, 999                                        
C                                                                       
 30   NF = IV(NFCALL)                                                   
      CALL CALCF(N, X, NF, F, UIPARM, URPARM, UFPARM)                   
      IF (NF .LE. 0) IV(TOOBIG) = 1                                     
      GO TO 20                                                          
C                                                                       
 40   NF = IV(NFGCAL)                                                   
      CALL CALCGH(N, X, NF, V(G1), V(H1), UIPARM, URPARM, UFPARM)       
      IF (NF .LE. 0) IV(TOOBIG) = 1                                     
      GO TO 20                                                          
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF  DMNHB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE   DN2F(N, P, X, CALCR, IV, LIV, LV, V,                 
     1                  UIPARM, URPARM, UFPARM)                         
C                                                                       
C  ***  MINIMIZE A NONLINEAR SUM OF SQUARES USING RESIDUAL VALUES ONLY..
C  ***  THIS AMOUNTS TO   DN2G WITHOUT THE SUBROUTINE PARAMETER CALCJ.  
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
      INTEGER N, P, LIV, LV                                             
C/6                                                                     
C     INTEGER IV(LIV), UIPARM(1)                                        
C     DOUBLE PRECISION X(P), V(LV), URPARM(1)                           
C/7                                                                     
      INTEGER IV(LIV), UIPARM(*)                                        
      DOUBLE PRECISION X(P), V(LV), URPARM(*)                           
C/                                                                      
      EXTERNAL CALCR, UFPARM                                            
C                                                                       
C-----------------------------  DISCUSSION  ----------------------------
C                                                                       
C        THIS AMOUNTS TO SUBROUTINE NL2SNO (REF. 1) MODIFIED TO CALL    
C      DRN2G.                                                           
C        THE PARAMETERS FOR   DN2F ARE THE SAME AS THOSE FOR   DN2G     
C     (WHICH SEE), EXCEPT THAT CALCJ IS OMITTED.  INSTEAD OF CALLING    
C     CALCJ TO OBTAIN THE JACOBIAN MATRIX OF R AT X,   DN2F COMPUTES    
C     AN APPROXIMATION TO IT BY FINITE (FORWARD) DIFFERENCES -- SEE     
C     V(DLTFDJ) BELOW.    DN2F USES FUNCTION VALUES ONLY WHEN COMPUT-   
C     THE COVARIANCE MATRIX (RATHER THAN THE FUNCTIONS AND GRADIENTS    
C     THAT   DN2G MAY USE).  TO DO SO,   DN2F SETS IV(COVREQ) TO MINUS  
C     ITS ABSOLUTE VALUE.  THUS V(DELTA0) IS NEVER REFERENCED AND ONLY  
C     V(DLTFDC) MATTERS -- SEE NL2SOL FOR A DESCRIPTION OF V(DLTFDC).   
C        THE NUMBER OF EXTRA CALLS ON CALCR USED IN COMPUTING THE JACO- 
C     BIAN APPROXIMATION ARE NOT INCLUDED IN THE FUNCTION EVALUATION    
C     COUNT IV(NFCALL), BUT ARE RECORDED IN IV(NGCALL) INSTEAD.         
C                                                                       
C V(DLTFDJ)... V(43) HELPS CHOOSE THE STEP SIZE USED WHEN COMPUTING THE 
C             FINITE-DIFFERENCE JACOBIAN MATRIX.  FOR DIFFERENCES IN-   
C             VOLVING X(I), THE STEP SIZE FIRST TRIED IS                
C                       V(DLTFDJ) * MAX(ABS(X(I)), 1/D(I)),             
C             WHERE D IS THE CURRENT SCALE VECTOR (SEE REF. 1).  (IF    
C             THIS STEP IS TOO BIG, I.E., IF CALCR SETS NF TO 0, THEN   
C             SMALLER STEPS ARE TRIED UNTIL THE STEP SIZE IS SHRUNK BE- 
C             LOW 1000 * MACHEP, WHERE MACHEP IS THE UNIT ROUNDOFF.     
C             DEFAULT = MACHEP**0.5.                                    
C                                                                       
C  ***  REFERENCE  ***                                                  
C                                                                       
C 1.  DENNIS, J.E., GAY, D.M., AND WELSCH, R.E. (1981), AN ADAPTIVE     
C             NONLINEAR LEAST-SQUARES ALGORITHM, ACM TRANS. MATH.       
C             SOFTWARE, VOL. 7, NO. 3.                                  
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY.                                            
C                                                                       
C+++++++++++++++++++++++++++  DECLARATIONS  +++++++++++++++++++++++++++ 
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      EXTERNAL DIVSET,  DRN2G, DN2RDP, DV7SCP                           
C                                                                       
C DIVSET.... PROVIDES DEFAULT IV AND V INPUT COMPONENTS.                
C  DRN2G... CARRIES OUT OPTIMIZATION ITERATIONS.                        
C DN2RDP... PRINTS REGRESSION DIAGNOSTICS.                              
C DV7SCP... SETS ALL COMPONENTS OF A VECTOR TO A SCALAR.                
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER D1, DK, DR1, I, IV1, J1K, K, N1, N2, NF, NG, RD1, R1, RN  
      DOUBLE PRECISION H, H0, HLIM, NEGPT5, ONE, XK, ZERO               
C                                                                       
C  ***  IV AND V COMPONENTS  ***                                        
C                                                                       
      INTEGER COVREQ, D, DINIT, DLTFDJ, J, MODE, NEXTV, NFCALL, NFGCAL, 
     1        NGCALL, NGCOV, R, REGD, REGD0, TOOBIG, VNEED              
C/6                                                                     
C     DATA COVREQ/15/, D/27/, DINIT/38/, DLTFDJ/43/, J/70/, MODE/35/,   
C    1     NEXTV/47/, NFCALL/6/, NFGCAL/7/, NGCALL/30/, NGCOV/53/,      
C    2     R/61/, REGD/67/, REGD0/82/, TOOBIG/2/, VNEED/4/              
C/7                                                                     
      PARAMETER (COVREQ=15, D=27, DINIT=38, DLTFDJ=43, J=70, MODE=35,   
     1           NEXTV=47, NFCALL=6, NFGCAL=7, NGCALL=30, NGCOV=53,     
     2           R=61, REGD=67, REGD0=82, TOOBIG=2, VNEED=4)            
C/                                                                      
      DATA HLIM/0.1D+0/, NEGPT5/-0.5D+0/, ONE/1.D+0/, ZERO/0.D+0/       
C                                                                       
C---------------------------------  BODY  ------------------------------
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)                  
      IV(COVREQ) = -IABS(IV(COVREQ))                                    
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 14) GO TO 10                                         
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10                        
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      IF (IV(1) .EQ. 13) IV(VNEED) = IV(VNEED) + P + N*(P+2)            
      CALL  DRN2G(X, V, IV, LIV, LV, N, N, N1, N2, P, V, V, V, X)       
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(D) = IV(NEXTV)                                                 
      IV(R) = IV(D) + P                                                 
      IV(REGD0) = IV(R) + N                                             
      IV(J) = IV(REGD0) + N                                             
      IV(NEXTV) = IV(J) + N*P                                           
      IF (IV1 .EQ. 13) GO TO 999                                        
C                                                                       
 10   D1 = IV(D)                                                        
      DR1 = IV(J)                                                       
      R1 = IV(R)                                                        
      RN = R1 + N - 1                                                   
      RD1 = IV(REGD0)                                                   
C                                                                       
 20   CALL  DRN2G(V(D1), V(DR1), IV, LIV, LV, N, N, N1, N2, P, V(R1),   
     1           V(RD1), V, X)                                          
      IF (IV(1)-2) 30, 50, 100                                          
C                                                                       
C  ***  NEW FUNCTION VALUE (R VALUE) NEEDED  ***                        
C                                                                       
 30   NF = IV(NFCALL)                                                   
      CALL CALCR(N, P, X, NF, V(R1), UIPARM, URPARM, UFPARM)            
      IF (NF .GT. 0) GO TO 40                                           
         IV(TOOBIG) = 1                                                 
         GO TO 20                                                       
 40   IF (IV(1) .GT. 0) GO TO 20                                        
C                                                                       
C  ***  COMPUTE FINITE-DIFFERENCE APPROXIMATION TO DR = GRAD. OF R  *** 
C                                                                       
C     *** INITIALIZE D IF NECESSARY ***                                 
C                                                                       
 50   IF (IV(MODE) .LT. 0 .AND. V(DINIT) .EQ. ZERO)                     
     1        CALL DV7SCP(P, V(D1), ONE)                                
C                                                                       
      J1K = DR1                                                         
      DK = D1                                                           
      NG = IV(NGCALL) - 1                                               
      IF (IV(1) .EQ. (-1)) IV(NGCOV) = IV(NGCOV) - 1                    
      DO 90 K = 1, P                                                    
         XK = X(K)                                                      
         H = V(DLTFDJ) * DMAX1(DABS(XK), ONE/V(DK))                     
         H0 = H                                                         
         DK = DK + 1                                                    
 60      X(K) = XK + H                                                  
         NF = IV(NFGCAL)                                                
         CALL CALCR (N, P, X, NF, V(J1K), UIPARM, URPARM, UFPARM)       
         NG = NG + 1                                                    
         IF (NF .GT. 0) GO TO 70                                        
              H = NEGPT5 * H                                            
              IF (DABS(H/H0) .GE. HLIM) GO TO 60                        
                   IV(TOOBIG) = 1                                       
                   IV(NGCALL) = NG                                      
                   GO TO 20                                             
 70      X(K) = XK                                                      
         IV(NGCALL) = NG                                                
         DO 80 I = R1, RN                                               
              V(J1K) = (V(J1K) - V(I)) / H                              
              J1K = J1K + 1                                             
 80           CONTINUE                                                  
 90      CONTINUE                                                       
      GO TO 20                                                          
C                                                                       
 100  IF (IV(REGD) .GT. 0) IV(REGD) = RD1                               
      CALL DN2RDP(IV, LIV, LV, N, V(RD1), V)                            
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST LINE OF   DN2F FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  DN2FB(N, P, X, B, CALCR, IV, LIV, LV, V, UI, UR, UF)  
C                                                                       
C  ***  MINIMIZE A NONLINEAR SUM OF SQUARES USING RESIDUAL VALUES ONLY..
C  ***  THIS AMOUNTS TO   DN2G WITHOUT THE SUBROUTINE PARAMETER CALCJ.  
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
      INTEGER N, P, LIV, LV                                             
C/6                                                                     
C     INTEGER IV(LIV), UI(1)                                            
C     DOUBLE PRECISION X(P), B(2,P), V(LV), UR(1)                       
C/7                                                                     
      INTEGER IV(LIV), UI(*)                                            
      DOUBLE PRECISION X(P), B(2,P), V(LV), UR(*)                       
C/                                                                      
      EXTERNAL CALCR, UF                                                
C                                                                       
C-----------------------------  DISCUSSION  ----------------------------
C                                                                       
C        THIS AMOUNTS TO SUBROUTINE NL2SNO (REF. 1) MODIFIED TO HANDLE  
C     SIMPLE BOUNDS ON THE VARIABLES...                                 
C           B(1,I) .LE. X(I) .LE. B(2,I), I = 1(1)P.                    
C        THE PARAMETERS FOR  DN2FB ARE THE SAME AS THOSE FOR  DN2GB     
C     (WHICH SEE), EXCEPT THAT CALCJ IS OMITTED.  INSTEAD OF CALLING    
C     CALCJ TO OBTAIN THE JACOBIAN MATRIX OF R AT X,  DN2FB COMPUTES    
C     AN APPROXIMATION TO IT BY FINITE (FORWARD) DIFFERENCES -- SEE     
C     V(DLTFDJ) BELOW.   DN2FB DOES NOT COMPUTE A COVARIANCE MATRIX.    
C        THE NUMBER OF EXTRA CALLS ON CALCR USED IN COMPUTING THE JACO- 
C     BIAN APPROXIMATION ARE NOT INCLUDED IN THE FUNCTION EVALUATION    
C     COUNT IV(NFCALL), BUT ARE RECORDED IN IV(NGCALL) INSTEAD.         
C                                                                       
C V(DLTFDJ)... V(43) HELPS CHOOSE THE STEP SIZE USED WHEN COMPUTING THE 
C             FINITE-DIFFERENCE JACOBIAN MATRIX.  FOR DIFFERENCES IN-   
C             VOLVING X(I), THE STEP SIZE FIRST TRIED IS                
C                       V(DLTFDJ) * MAX(ABS(X(I)), 1/D(I)),             
C             WHERE D IS THE CURRENT SCALE VECTOR (SEE REF. 1).  (IF    
C             THIS STEP IS TOO BIG, I.E., IF CALCR SETS NF TO 0, THEN   
C             SMALLER STEPS ARE TRIED UNTIL THE STEP SIZE IS SHRUNK BE- 
C             LOW 1000 * MACHEP, WHERE MACHEP IS THE UNIT ROUNDOFF.     
C             DEFAULT = MACHEP**0.5.                                    
C                                                                       
C  ***  REFERENCE  ***                                                  
C                                                                       
C 1.  DENNIS, J.E., GAY, D.M., AND WELSCH, R.E. (1981), AN ADAPTIVE     
C             NONLINEAR LEAST-SQUARES ALGORITHM, ACM TRANS. MATH.       
C             SOFTWARE, VOL. 7, NO. 3.                                  
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY.                                            
C                                                                       
C+++++++++++++++++++++++++++  DECLARATIONS  +++++++++++++++++++++++++++ 
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      EXTERNAL DIVSET, DRN2GB, DV7SCP                                   
C                                                                       
C DIVSET.... PROVIDES DEFAULT IV AND V INPUT COMPONENTS.                
C DRN2GB... CARRIES OUT OPTIMIZATION ITERATIONS.                        
C DN2RDP... PRINTS REGRESSION DIAGNOSTICS.                              
C DV7SCP... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.                  
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER D1, DK, DR1, I, IV1, J1K, K, N1, N2, NF, NG, RD1, R1, RN  
      DOUBLE PRECISION H, H0, HLIM, NEGPT5, ONE, T, XK, XK1, ZERO       
C                                                                       
C  ***  IV AND V COMPONENTS  ***                                        
C                                                                       
      INTEGER COVREQ, D, DINIT, DLTFDJ, J, MODE, NEXTV, NFCALL, NFGCAL, 
     1        NGCALL, NGCOV, R, REGD0, TOOBIG, VNEED                    
C/6                                                                     
C     DATA COVREQ/15/, D/27/, DINIT/38/, DLTFDJ/43/, J/70/, MODE/35/,   
C    1     NEXTV/47/, NFCALL/6/, NFGCAL/7/, NGCALL/30/, NGCOV/53/,      
C    2     R/61/, REGD0/82/, TOOBIG/2/, VNEED/4/                        
C/7                                                                     
      PARAMETER (COVREQ=15, D=27, DINIT=38, DLTFDJ=43, J=70, MODE=35,   
     1           NEXTV=47, NFCALL=6, NFGCAL=7, NGCALL=30, NGCOV=53,     
     2           R=61, REGD0=82, TOOBIG=2, VNEED=4)                     
C/                                                                      
      DATA HLIM/0.1D+0/, NEGPT5/-0.5D+0/, ONE/1.D+0/, ZERO/0.D+0/       
C                                                                       
C---------------------------------  BODY  ------------------------------
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)                  
      IV(COVREQ) = 0                                                    
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 14) GO TO 10                                         
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10                        
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      IF (IV(1) .EQ. 13) IV(VNEED) = IV(VNEED) + P + N*(P+2)            
      CALL DRN2GB(B, X, V, IV, LIV, LV, N, N, N1, N2, P, V, V, V, X)    
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(D) = IV(NEXTV)                                                 
      IV(R) = IV(D) + P                                                 
      IV(REGD0) = IV(R) + N                                             
      IV(J) = IV(REGD0) + N                                             
      IV(NEXTV) = IV(J) + N*P                                           
      IF (IV1 .EQ. 13) GO TO 999                                        
C                                                                       
 10   D1 = IV(D)                                                        
      DR1 = IV(J)                                                       
      R1 = IV(R)                                                        
      RN = R1 + N - 1                                                   
      RD1 = IV(REGD0)                                                   
C                                                                       
 20   CALL DRN2GB(B, V(D1), V(DR1), IV, LIV, LV, N, N, N1, N2, P, V(R1),
     1           V(RD1), V, X)                                          
      IF (IV(1)-2) 30, 50, 999                                          
C                                                                       
C  ***  NEW FUNCTION VALUE (R VALUE) NEEDED  ***                        
C                                                                       
 30   NF = IV(NFCALL)                                                   
      CALL CALCR(N, P, X, NF, V(R1), UI, UR, UF)                        
      IF (NF .GT. 0) GO TO 40                                           
         IV(TOOBIG) = 1                                                 
         GO TO 20                                                       
 40   IF (IV(1) .GT. 0) GO TO 20                                        
C                                                                       
C  ***  COMPUTE FINITE-DIFFERENCE APPROXIMATION TO DR = GRAD. OF R  *** 
C                                                                       
C     *** INITIALIZE D IF NECESSARY ***                                 
C                                                                       
 50   IF (IV(MODE) .LT. 0 .AND. V(DINIT) .EQ. ZERO)                     
     1        CALL DV7SCP(P, V(D1), ONE)                                
C                                                                       
      J1K = DR1                                                         
      DK = D1                                                           
      NG = IV(NGCALL) - 1                                               
      IF (IV(1) .EQ. (-1)) IV(NGCOV) = IV(NGCOV) - 1                    
      DO 120 K = 1, P                                                   
         IF (B(1,K) .GE. B(2,K)) GO TO 110                              
         XK = X(K)                                                      
         H = V(DLTFDJ) * DMAX1(DABS(XK), ONE/V(DK))                     
         H0 = H                                                         
         DK = DK + 1                                                    
         T = NEGPT5                                                     
         XK1 = XK + H                                                   
         IF (XK - H .GE. B(1,K)) GO TO 60                               
            T = -T                                                      
            IF (XK1 .GT. B(2,K)) GO TO 80                               
 60      IF (XK1 .LE. B(2,K)) GO TO 70                                  
            T = -T                                                      
            H = -H                                                      
            XK1 = XK + H                                                
            IF (XK1 .LT. B(1,K)) GO TO 80                               
 70      X(K) = XK1                                                     
         NF = IV(NFGCAL)                                                
         CALL CALCR (N, P, X, NF, V(J1K), UI, UR, UF)                   
         NG = NG + 1                                                    
         IF (NF .GT. 0) GO TO 90                                        
              H = T * H                                                 
              XK1 = XK + H                                              
              IF (DABS(H/H0) .GE. HLIM) GO TO 70                        
 80                IV(TOOBIG) = 1                                       
                   IV(NGCALL) = NG                                      
                   GO TO 20                                             
 90      X(K) = XK                                                      
         IV(NGCALL) = NG                                                
         DO 100 I = R1, RN                                              
              V(J1K) = (V(J1K) - V(I)) / H                              
              J1K = J1K + 1                                             
 100          CONTINUE                                                  
         GO TO 120                                                      
C        *** SUPPLY A ZERO DERIVATIVE FOR CONSTANT COMPONENTS...        
 110     CALL DV7SCP(N, V(J1K), ZERO)                                   
         J1K = J1K + N                                                  
 120     CONTINUE                                                       
      GO TO 20                                                          
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF  DN2FB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE   DN2G(N, P, X, CALCR, CALCJ, IV, LIV, LV, V,          
     1                  UI, UR, UF)                                     
C                                                                       
C  ***  VERSION OF NL2SOL THAT CALLS  DRN2G  ***                        
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
      INTEGER N, P, LIV, LV                                             
C/6                                                                     
C     INTEGER IV(LIV), UI(1)                                            
C     DOUBLE PRECISION X(P), V(LV), UR(1)                               
C/7                                                                     
      INTEGER IV(LIV), UI(*)                                            
      DOUBLE PRECISION X(P), V(LV), UR(*)                               
C/                                                                      
      EXTERNAL CALCR, CALCJ, UF                                         
C                                                                       
C  ***  PARAMETER USAGE  ***                                            
C                                                                       
C N....... TOTAL NUMBER OF RESIDUALS.                                   
C P....... NUMBER OF PARAMETERS (COMPONENTS OF X) BEING ESTIMATED.      
C X....... PARAMETER VECTOR BEING ESTIMATED (INPUT = INITIAL GUESS,     
C             OUTPUT = BEST VALUE FOUND).                               
C CALCR... SUBROUTINE FOR COMPUTING RESIDUAL VECTOR.                    
C CALCJ... SUBROUTINE FOR COMPUTING JACOBIAN MATRIX = MATRIX OF FIRST   
C             PARTIALS OF THE RESIDUAL VECTOR.                          
C IV...... INTEGER VALUES ARRAY.                                        
C LIV..... LENGTH OF IV (SEE DISCUSSION BELOW).                         
C LV...... LENGTH OF V (SEE DISCUSSION BELOW).                          
C V....... FLOATING-POINT VALUES ARRAY.                                 
C UI...... PASSED UNCHANGED TO CALCR AND CALCJ.                         
C UR...... PASSED UNCHANGED TO CALCR AND CALCJ.                         
C UF...... PASSED UNCHANGED TO CALCR AND CALCJ.                         
C                                                                       
C                                                                       
C  ***  DISCUSSION  ***                                                 
C                                                                       
C        NOTE... NL2SOL (MENTIONED BELOW) IS A CODE FOR SOLVING         
C     NONLINEAR LEAST-SQUARES PROBLEMS.  IT IS DESCRIBED IN             
C     ACM TRANS. MATH. SOFTWARE, VOL. 9, PP. 369-383 (AN ADAPTIVE       
C     NONLINEAR LEAST-SQUARES ALGORITHM, BY J.E. DENNIS, D.M. GAY,      
C     AND R.E. WELSCH).                                                 
C                                                                       
C        LIV GIVES THE LENGTH OF IV.  IT MUST BE AT LEAST 82+P.  IF NOT,
C     THEN   DN2G RETURNS WITH IV(1) = 15.  WHEN   DN2G RETURNS, THE    
C     MINIMUM ACCEPTABLE VALUE OF LIV IS STORED IN IV(LASTIV) = IV(44), 
C     (PROVIDED THAT LIV .GE. 44).                                      
C                                                                       
C        LV GIVES THE LENGTH OF V.  THE MINIMUM VALUE FOR LV IS         
C     LV0 = 105 + P*(N + 2*P + 17) + 2*N.  IF LV IS SMALLER THAN THIS,  
C     THEN   DN2G RETURNS WITH IV(1) = 16.  WHEN   DN2G RETURNS, THE    
C     MINIMUM ACCEPTABLE VALUE OF LV IS STORED IN IV(LASTV) = IV(45)    
C     (PROVIDED LIV .GE. 45).                                           
C                                                                       
C        RETURN CODES AND CONVERGENCE TOLERANCES ARE THE SAME AS FOR    
C     NL2SOL, WITH SOME SMALL EXTENSIONS... IV(1) = 15 MEANS LIV WAS    
C     TOO SMALL.   IV(1) = 16 MEANS LV WAS TOO SMALL.                   
C                                                                       
C        THERE ARE TWO NEW V INPUT COMPONENTS...  V(LMAXS) = V(36) AND  
C     V(SCTOL) = V(37) SERVE WHERE V(LMAX0) AND V(RFCTOL) FORMERLY DID  
C     IN THE SINGULAR CONVERGENCE TEST -- SEE THE NL2SOL DOCUMENTATION. 
C                                                                       
C  ***  DEFAULT VALUES  ***                                             
C                                                                       
C        DEFAULT VALUES ARE PROVIDED BY SUBROUTINE DIVSET, RATHER THAN  
C     DFAULT.  THE CALLING SEQUENCE IS...                               
C             CALL DIVSET(1, IV, LIV, LV, V)                            
C     THE FIRST PARAMETER IS AN INTEGER 1.  IF LIV AND LV ARE LARGE     
C     ENOUGH FOR DIVSET, THEN DIVSET SETS IV(1) TO 12.  OTHERWISE IT    
C     SETS IV(1) TO 15 OR 16.  CALLING   DN2G WITH IV(1) = 0 CAUSES ALL 
C     DEFAULT VALUES TO BE USED FOR THE INPUT COMPONENTS OF IV AND V.   
C        IF YOU FIRST CALL DIVSET, THEN SET IV(1) TO 13 AND CALL   DN2G,
C     THEN STORAGE ALLOCATION ONLY WILL BE PERFORMED.  IN PARTICULAR,   
C     IV(D) = IV(27), IV(J) = IV(70), AND IV(R) = IV(61) WILL BE SET    
C     TO THE FIRST SUBSCRIPT IN V OF THE SCALE VECTOR, THE JACOBIAN     
C     MATRIX, AND THE RESIDUAL VECTOR RESPECTIVELY, PROVIDED LIV AND LV 
C     ARE LARGE ENOUGH.  IF SO, THEN   DN2G RETURNS WITH IV(1) = 14.    
C     WHEN CALLED WITH IV(1) = 14,   DN2G ASSUMES THAT STORAGE HAS      
C     BEEN ALLOCATED, AND IT BEGINS THE MINIMIZATION ALGORITHM.         
C                                                                       
C  ***  SCALE VECTOR  ***                                               
C                                                                       
C        ONE DIFFERENCE WITH NL2SOL IS THAT THE SCALE VECTOR D IS       
C     STORED IN V, STARTING AT A DIFFERENT SUBSCRIPT.  THE STARTING     
C     SUBSCRIPT VALUE IS STILL STORED IN IV(D) = IV(27).  THE           
C     DISCUSSION OF DEFAULT VALUES ABOVE TELLS HOW TO HAVE IV(D) SET    
C     BEFORE THE ALGORITHM IS STARTED.                                  
C                                                                       
C  ***  REGRESSION DIAGNOSTICS  ***                                     
C                                                                       
C        IF IV(RDREQ) SO DICTATES, THEN ESTIMATES ARE COMPUTED OF THE   
C     INFLUENCE EACH RESIDUAL COMPONENT HAS ON THE FINAL PARAMETER      
C     ESTIMATE X.  THE GENERAL IDEA IS THAT ONE MAY WISH TO EXAMINE     
C     RESIDUAL COMPONENTS (AND THE DATA BEHIND THEM) FOR WHICH THE      
C     INFLUENCE ESTIMATE IS SIGNIFICANTLY LARGER THAN MOST OF THE OTHER 
C     INFLUENCE ESTIMATES.  THESE ESTIMATES, HEREAFTER CALLED           
C     REGRESSION DIAGNOSTICS, ARE ONLY COMPUTED IF IV(RDREQ) = 2 OR 3.  
C     IN THIS CASE, FOR I = 1(1)N,                                      
C                    SQRT( G(I)**T * H(I)**-1 * G(I) )                  
C     IS COMPUTED AND STORED IN V, STARTING AT V(IV(REGD)), WHERE       
C     RDREQ = 57 AND REGD = 67.  HERE G(I) STANDS FOR THE GRADIENT      
C     RESULTING WHEN THE I-TH OBSERVATION IS DELETED AND H(I) STANDS    
C     FOR AN APPROXIMATION TO THE CORRESPONDING HESSIAN AT X, THE SOLU- 
C     TION CORRESPONDING TO ALL OBSERVATIONS.  (THIS APPROXIMATION IS   
C     OBTAINED BY SUBTRACTING THE FIRST-ORDER CONTRIBUTION OF THE I-TH  
C     OBSERVATION TO THE HESSIAN FROM A FINITE-DIFFERENCE HESSIAN       
C     APPROXIMATION.  IF H IS INDEFINITE, THEN IV(REGD) IS SET TO -1.   
C     IF H(I) IS INDEFINITE, THEN -1 IS RETURNED AS THE DIAGNOSTIC FOR  
C     OBSERVATION I.  IF NO DIAGNOSTICS ARE COMPUTED, PERHAPS BECAUSE   
C     OF A FAILURE TO CONVERGE, THEN IV(REGD) = 0 IS RETURNED.)         
C        PRINTING OF THE REGRESSION DIAGNOSTICS IS CONTROLLED BY        
C     IV(COVPRT) = IV(14)...  IF IV(COVPRT) = 3, THEN BOTH THE          
C     COVARIANCE MATRIX AND THE REGRESSION DIAGNOSTICS ARE PRINTED.     
C     IV(COVPRT) = 2 CAUSES ONLY THE REGRESSION DIAGNOSTICS TO BE       
C     PRINTED, IV(COVPRT) = 1 CAUSES ONLY THE COVARIANCE MATRIX TO BE   
C     PRINTED, AND IV(COVPRT) = 0 CAUSES NEITHER TO BE PRINTED.         
C                                                                       
C        RDREQ = 57 AND REGD = 67.                                      
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY.                                            
C                                                                       
C+++++++++++++++++++++++++++  DECLARATIONS  +++++++++++++++++++++++++++ 
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      EXTERNAL DIVSET,  DRN2G, DN2RDP                                   
C DIVSET.... PROVIDES DEFAULT IV AND V INPUT COMPONENTS.                
C  DRN2G... CARRIES OUT OPTIMIZATION ITERATIONS.                        
C DN2RDP... PRINTS REGRESSION DIAGNOSTICS.                              
C                                                                       
C  ***  NO INTRINSIC FUNCTIONS  ***                                     
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER D1, DR1, IV1, N1, N2, NF, R1, RD1                         
C                                                                       
C  ***  IV COMPONENTS  ***                                              
C                                                                       
      INTEGER D, J, NEXTV, NFCALL, NFGCAL, R, REGD, REGD0, TOOBIG, VNEED
C/6                                                                     
C     DATA D/27/, J/70/, NEXTV/47/, NFCALL/6/, NFGCAL/7/, R/61/,        
C    1     REGD/67/, REGD0/82/, TOOBIG/2/, VNEED/4/                     
C/7                                                                     
      PARAMETER (D=27, J=70, NEXTV=47, NFCALL=6, NFGCAL=7, R=61,        
     1           REGD=67, REGD0=82, TOOBIG=2, VNEED=4)                  
C/                                                                      
C---------------------------------  BODY  ------------------------------
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)                  
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 14) GO TO 10                                         
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10                        
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      IF (IV(1) .EQ. 13) IV(VNEED) = IV(VNEED) + P + N*(P+2)            
      CALL  DRN2G(X, V, IV, LIV, LV, N, N, N1, N2, P, V, V, V, X)       
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(D) = IV(NEXTV)                                                 
      IV(R) = IV(D) + P                                                 
      IV(REGD0) = IV(R) + N                                             
      IV(J) = IV(REGD0) + N                                             
      IV(NEXTV) = IV(J) + N*P                                           
      IF (IV1 .EQ. 13) GO TO 999                                        
C                                                                       
 10   D1 = IV(D)                                                        
      DR1 = IV(J)                                                       
      R1 = IV(R)                                                        
      RD1 = IV(REGD0)                                                   
C                                                                       
 20   CALL  DRN2G(V(D1), V(DR1), IV, LIV, LV, N, N, N1, N2, P, V(R1),   
     1            V(RD1), V, X)                                         
      IF (IV(1)-2) 30, 50, 60                                           
C                                                                       
C  ***  NEW FUNCTION VALUE (R VALUE) NEEDED  ***                        
C                                                                       
 30   NF = IV(NFCALL)                                                   
      CALL CALCR(N, P, X, NF, V(R1), UI, UR, UF)                        
      IF (NF .GT. 0) GO TO 40                                           
         IV(TOOBIG) = 1                                                 
         GO TO 20                                                       
 40   IF (IV(1) .GT. 0) GO TO 20                                        
C                                                                       
C  ***  COMPUTE DR = GRADIENT OF R COMPONENTS  ***                      
C                                                                       
 50   CALL CALCJ(N, P, X, IV(NFGCAL), V(DR1), UI, UR, UF)               
      IF (IV(NFGCAL) .EQ. 0) IV(TOOBIG) = 1                             
      GO TO 20                                                          
C                                                                       
C  ***  INDICATE WHETHER THE REGRESSION DIAGNOSTIC ARRAY WAS COMPUTED   
C  ***  AND PRINT IT IF SO REQUESTED...                                 
C                                                                       
 60   IF (IV(REGD) .GT. 0) IV(REGD) = RD1                               
      CALL DN2RDP(IV, LIV, LV, N, V(RD1), V)                            
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST LINE OF   DN2G FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  DN2GB(N, P, X, B, CALCR, CALCJ, IV, LIV, LV, V,       
     1                  UIPARM, URPARM, UFPARM)                         
C                                                                       
C  ***  VERSION OF NL2SOL THAT HANDLES SIMPLE BOUNDS ON X  ***          
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
      INTEGER N, P, LIV, LV                                             
C/6                                                                     
C     INTEGER IV(LIV), UIPARM(1)                                        
C     DOUBLE PRECISION X(P), B(2,P), V(LV), URPARM(1)                   
C/7                                                                     
      INTEGER IV(LIV), UIPARM(*)                                        
      DOUBLE PRECISION X(P), B(2,P), V(LV), URPARM(*)                   
C/                                                                      
      EXTERNAL CALCR, CALCJ, UFPARM                                     
C                                                                       
C  ***  DISCUSSION  ***                                                 
C                                                                       
C        NOTE... NL2SOL (MENTIONED BELOW) IS A CODE FOR SOLVING         
C     NONLINEAR LEAST-SQUARES PROBLEMS.  IT IS DESCRIBED IN             
C     ACM TRANS. MATH. SOFTWARE, VOL. 9, PP. 369-383 (AN ADAPTIVE       
C     NONLINEAR LEAST-SQUARES ALGORITHM, BY J.E. DENNIS, D.M. GAY,      
C     AND R.E. WELSCH).                                                 
C                                                                       
C        LIV GIVES THE LENGTH OF IV.  IT MUST BE AT LEAST 82 + 4*P.     
C     IF NOT, THEN  DN2GB RETURNS WITH IV(1) = 15.  WHEN  DN2GB         
C     RETURNS, THE MINIMUM ACCEPTABLE VALUE OF LIV IS STORED IN         
C     IV(LASTIV) = IV(44), (PROVIDED THAT LIV .GE. 44).                 
C                                                                       
C        LV GIVES THE LENGTH OF V.  THE MINIMUM VALUE FOR LV IS         
C     LV0 = 105 + P*(N + 2*P + 21) + 2*N.  IF LV IS SMALLER THAN THIS,  
C     THEN  DN2GB RETURNS WITH IV(1) = 16.  WHEN  DN2GB RETURNS, THE    
C     MINIMUM ACCEPTABLE VALUE OF LV IS STORED IN IV(LASTV) = IV(45)    
C     (PROVIDED LIV .GE. 45).                                           
C                                                                       
C        RETURN CODES AND CONVERGENCE TOLERANCES ARE THE SAME AS FOR    
C     NL2SOL, WITH SOME SMALL EXTENSIONS... IV(1) = 15 MEANS LIV WAS    
C     TOO SMALL.   IV(1) = 16 MEANS LV WAS TOO SMALL.                   
C                                                                       
C        THERE ARE TWO NEW V INPUT COMPONENTS...  V(LMAXS) = V(36) AND  
C     V(SCTOL) = V(37) SERVE WHERE V(LMAX0) AND V(RFCTOL) FORMERLY DID  
C     IN THE SINGULAR CONVERGENCE TEST -- SEE THE NL2SOL DOCUMENTATION. 
C                                                                       
C  ***  BOUNDS  ***                                                     
C                                                                       
C     THE BOUNDS  B(1,I) .LE. X(I) .LE. B(2,I), I = 1(1)P, ARE ENFORCED.
C                                                                       
C  ***  DEFAULT VALUES  ***                                             
C                                                                       
C        DEFAULT VALUES ARE PROVIDED BY SUBROUTINE DIVSET, RATHER THAN  
C     DFAULT.  THE CALLING SEQUENCE IS...                               
C             CALL DIVSET(1, IV, LIV, LV, V)                            
C     THE FIRST PARAMETER IS AN INTEGER 1.  IF LIV AND LV ARE LARGE     
C     ENOUGH FOR DIVSET, THEN DIVSET SETS IV(1) TO 12.  OTHERWISE IT    
C     SETS IV(1) TO 15 OR 16.  CALLING  DN2GB WITH IV(1) = 0 CAUSES ALL 
C     DEFAULT VALUES TO BE USED FOR THE INPUT COMPONENTS OF IV AND V.   
C        IF YOU FIRST CALL DIVSET, THEN SET IV(1) TO 13 AND CALL  DN2GB,
C     THEN STORAGE ALLOCATION ONLY WILL BE PERFORMED.  IN PARTICULAR,   
C     IV(D) = IV(27), IV(J) = IV(70), AND IV(R) = IV(61) WILL BE SET    
C     TO THE FIRST SUBSCRIPT IN V OF THE SCALE VECTOR, THE JACOBIAN     
C     MATRIX, AND THE RESIDUAL VECTOR RESPECTIVELY, PROVIDED LIV AND LV 
C     ARE LARGE ENOUGH.  IF SO, THEN  DN2GB RETURNS WITH IV(1) = 14.    
C     WHEN CALLED WITH IV(1) = 14,  DN2GB ASSUMES THAT STORAGE HAS      
C     BEEN ALLOCATED, AND IT BEGINS THE MINIMIZATION ALGORITHM.         
C                                                                       
C  ***  SCALE VECTOR  ***                                               
C                                                                       
C        ONE DIFFERENCE WITH NL2SOL IS THAT THE SCALE VECTOR D IS       
C     STORED IN V, STARTING AT A DIFFERENT SUBSCRIPT.  THE STARTING     
C     SUBSCRIPT VALUE IS STILL STORED IN IV(D) = IV(27).  THE           
C     DISCUSSION OF DEFAULT VALUES ABOVE TELLS HOW TO HAVE IV(D) SET    
C     BEFORE THE ALGORITHM IS STARTED.                                  
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY.                                            
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      EXTERNAL DIVSET, DRN2GB                                           
C DIVSET.... PROVIDES DEFAULT IV AND V INPUT COMPONENTS.                
C DRN2GB... CARRIES OUT OPTIMIZATION ITERATIONS.                        
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER D1, DR1, IV1, N1, N2, NF, R1, RD1                         
C                                                                       
C  ***  IV COMPONENTS  ***                                              
C                                                                       
      INTEGER D, J, NEXTV, NFCALL, NFGCAL, R, REGD0, TOOBIG, VNEED      
C/6                                                                     
C     DATA D/27/, J/70/, NEXTV/47/, NFCALL/6/, NFGCAL/7/, R/61/,        
C    1     REGD0/82/, TOOBIG/2/, VNEED/4/                               
C/7                                                                     
      PARAMETER (D=27, J=70, NEXTV=47, NFCALL=6, NFGCAL=7, R=61,        
     1           REGD0=82, TOOBIG=2, VNEED=4)                           
C/                                                                      
C---------------------------------  BODY  ------------------------------
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)                  
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 14) GO TO 10                                         
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10                        
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      IF (IV(1) .EQ. 13) IV(VNEED) = IV(VNEED) + P + N*(P+2)            
      CALL DRN2GB(B, X, V, IV, LIV, LV, N, N, N1, N2, P, V, V, V, X)    
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(D) = IV(NEXTV)                                                 
      IV(R) = IV(D) + P                                                 
      IV(REGD0) = IV(R) + N                                             
      IV(J) = IV(REGD0) + N                                             
      IV(NEXTV) = IV(J) + N*P                                           
      IF (IV1 .EQ. 13) GO TO 999                                        
C                                                                       
 10   D1 = IV(D)                                                        
      DR1 = IV(J)                                                       
      R1 = IV(R)                                                        
      RD1 = IV(REGD0)                                                   
C                                                                       
 20   CALL DRN2GB(B, V(D1), V(DR1), IV, LIV, LV, N, N, N1, N2, P, V(R1),
     1            V(RD1), V, X)                                         
      IF (IV(1)-2) 30, 50, 999                                          
C                                                                       
C  ***  NEW FUNCTION VALUE (R VALUE) NEEDED  ***                        
C                                                                       
 30   NF = IV(NFCALL)                                                   
      CALL CALCR(N, P, X, NF, V(R1), UIPARM, URPARM, UFPARM)            
      IF (NF .GT. 0) GO TO 40                                           
         IV(TOOBIG) = 1                                                 
         GO TO 20                                                       
 40   IF (IV(1) .GT. 0) GO TO 20                                        
C                                                                       
C  ***  COMPUTE DR = GRADIENT OF R COMPONENTS  ***                      
C                                                                       
 50   CALL CALCJ(N, P, X, IV(NFGCAL), V(DR1), UIPARM, URPARM, UFPARM)   
      IF (IV(NFGCAL) .EQ. 0) IV(TOOBIG) = 1                             
      GO TO 20                                                          
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF  DN2GB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE   DN2P(N, ND, P, X, CALCR, CALCJ, IV, LIV, LV, V,      
     1                  UI, UR, UF)                                     
C                                                                       
C  ***  VERSION OF NL2SOL THAT CALLS  DRN2G AND HAS EXPANDED CALLING    
C  ***  SEQUENCES FOR CALCR, CALCJ, ALLOWING THEM TO PROVIDE R AND J    
C  ***  (RESIDUALS AND JACOBIAN) IN CHUNKS.                             
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
      INTEGER N, ND, P, LIV, LV                                         
C/6                                                                     
C     INTEGER IV(LIV), UI(1)                                            
C     DOUBLE PRECISION X(P), V(LV), UR(1)                               
C/7                                                                     
      INTEGER IV(LIV), UI(*)                                            
      DOUBLE PRECISION X(P), V(LV), UR(*)                               
C/                                                                      
      EXTERNAL CALCR, CALCJ, UF                                         
C                                                                       
C                                                                       
C  ***  PARAMETER USAGE  ***                                            
C                                                                       
C N....... TOTAL NUMBER OF RESIDUALS.                                   
C ND...... MAXIMUM NUMBER OF RESIDUAL COMPONENTS PROVIDED BY ANY CALL   
C             ON CALCR.                                                 
C P....... NUMBER OF PARAMETERS (COMPONENTS OF X) BEING ESTIMATED.      
C X....... PARAMETER VECTOR BEING ESTIMATED (INPUT = INITIAL GUESS,     
C             OUTPUT = BEST VALUE FOUND).                               
C CALCR... SUBROUTINE FOR COMPUTING RESIDUAL VECTOR.                    
C CALCJ... SUBROUTINE FOR COMPUTING JACOBIAN MATRIX = MATRIX OF FIRST   
C             PARTIALS OF THE RESIDUAL VECTOR.                          
C IV...... INTEGER VALUES ARRAY.                                        
C LIV..... LENGTH OF IV (SEE DISCUSSION BELOW).                         
C LV...... LENGTH OF V (SEE DISCUSSION BELOW).                          
C V....... FLOATING-POINT VALUES ARRAY.                                 
C UI...... PASSED UNCHANGED TO CALCR AND CALCJ.                         
C UR...... PASSED UNCHANGED TO CALCR AND CALCJ.                         
C UF...... PASSED UNCHANGED TO CALCR AND CALCJ.                         
C                                                                       
C                                                                       
C  ***  DISCUSSION  ***                                                 
C                                                                       
C    THIS ROUTINE IS SIMILAR TO   DN2G (WHICH SEE), EXCEPT THAT THE     
C CALLING SEQUENCE FOR CALCR AND CALCJ IS DIFFERENT -- IT ALLOWS        
C THE RESIDUAL VECTOR AND JACOBIAN MATRIX TO BE PASSED IN BLOCKS.       
C                                                                       
C   FOR CALCR, THE CALLING SEQUENCE IS...                               
C                                                                       
C     CALCR(N, ND1, N1, N2, P, X, NF, R, UI, UR, UF)                    
C                                                                       
C   PARAMETERS N, P, X, NF, R, UI, UR, UF ARE AS FOR THE CALCR USED     
C BY NL2SOL OR   DN2G.                                                  
C   PARAMETERS ND1, N1, AND N2 ARE INPUTS TO CALCR.  CALCR SHOULD NOT   
C CHANGE ND1 OR N1 (BUT MAY CHANGE N2).                                 
C   ND1 = MIN(N,ND) IS THE MAXIMUM NUMBER OF RESIDUAL COMPONENTS THAT   
C CALCR SHOULD SUPPLY ON ONE CALL.                                      
C   N1 IS THE INDEX OF THE FIRST RESIDUAL COMPONENT THAT CALCR SHOULD   
C SUPPLY ON THIS CALL.                                                  
C   N2 HAS THE VALUE MIN(N, N1+ND1-1) WHEN CALCR IS CALLED.  CALCR      
C MAY SET N2 TO A LOWER VALUE (AT LEAST 1) AND FOR N1 .LE. I .LE. N2    
C SHOULD RETURN RESIDUAL COMPONENT I IN R(I-N1+1), I.E., IN COMPONENTS  
C R(1), R(2), ..., R(N2-N1+1).                                          
C                                                                       
C   FOR CALCJ, THE CALLING SEQUENCE IS...                               
C                                                                       
C     CALCJ(N, ND1, N1, N2, P, X, NF, J, UI, UR, UF)                    
C                                                                       
C   ALL PARAMETERS EXCEPT N2 AND J ARE AS FOR CALCR.  N2 MAY NOT BE     
C CHANGED, BUT OTHERWISE IS AS FOR CALCR.  (WHENEVER CALCJ IS CALLED,   
C CALCR WILL JUST HAVE BEEN CALLED WITH THE SAME VALUE OF N1 BUT        
C POSSIBLY A DIFFERENT X -- NF IDENTIFIES THE X PASSED.  IF CALCR       
C CHANGES N2, THEN THIS CHANGED VALUE IS PASSED TO CALCJ.)              
C   J IS A FLOATING-POINT ARRAY DIMENSIONED J(ND1,P).  FOR I = N1(1)N2  
C AND K = 1(1)P, CALCJ MUST STORE THE PARTIAL DERIVATIVE AT X OF        
C RESIDUAL COMPONENT I WITH RESPECT TO X(K) IN J(I-N1+1,K).             
C                                                                       
C   LIV MUST BE AT LEAST 82 + P.  LV MUST BE AT LEAST                   
C         105 + P*(17 + 2*P) + (P+1)*MIN(ND,N) + N                      
C IF ND .LT. N AND NO REGRESSION DIAGNOSTIC ARRAY IS REQUESTED          
C (I.E., IV(RDREQ) = 0 OR 1), THEN LV CAN BE N LESS THAN THIS.          
C                                                                       
C+++++++++++++++++++++++++++  DECLARATIONS  +++++++++++++++++++++++++++ 
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      EXTERNAL DIVSET, DN2RDP,  DRN2G                                   
C DIVSET.... PROVIDES DEFAULT IV AND V INPUT COMPONENTS.                
C DN2RDP... PRINTS REGRESSION DIAGNOSTICS.                              
C  DRN2G... CARRIES OUT OPTIMIZATION ITERATIONS.                        
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      LOGICAL ONERD                                                     
      INTEGER D1, DR1, I, IV1, N1, N2, ND1, NF, R1, RD0, RD1, X01       
C                                                                       
C  ***  IV COMPONENTS  ***                                              
C                                                                       
      INTEGER D, J, NEXTV, NF00, NFCALL, NFGCAL, R, RDREQ, REGD,        
     1        REGD0, TOOBIG, VNEED, X0                                  
C/6                                                                     
C     DATA D/27/, J/70/, NEXTV/47/, NF00/81/, NFCALL/6/, NFGCAL/7/,     
C    1     R/61/, RDREQ/57/, REGD/67/, REGD0/82/, TOOBIG/2/, VNEED/4/,  
C    2     X0/43/                                                       
C/7                                                                     
      PARAMETER (D=27, J=70, NEXTV=47, NF00=81, NFCALL=6, NFGCAL=7,     
     1           R=61, RDREQ=57, REGD=67, REGD0=82, TOOBIG=2, VNEED=4,  
     2           X0=43)                                                 
C/                                                                      
C---------------------------------  BODY  ------------------------------
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)                  
      ND1 = MIN0(ND, N)                                                 
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 14) GO TO 10                                         
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10                        
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      I = IV(VNEED) + P + ND1*(P+1)                                     
      ONERD = IV(RDREQ) .GE. 2 .OR. ND .GE. N                           
      IF (ONERD) I = I + N                                              
      IF (IV(1) .EQ. 13) IV(VNEED) = I                                  
      CALL  DRN2G(V, V, IV, LIV, LV, N, ND1, N1, N2, P, V, V, V, X)     
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(D) = IV(NEXTV)                                                 
      IV(R) = IV(D) + P                                                 
      I = IV(R) + ND1                                                   
      IV(REGD0) = I                                                     
      IF (ONERD) I = I + N                                              
      IV(J) = I                                                         
      IV(NEXTV) = I + ND1*P                                             
      IF (IV1 .EQ. 13) GO TO 999                                        
C                                                                       
 10   D1 = IV(D)                                                        
      DR1 = IV(J)                                                       
      R1 = IV(R)                                                        
      RD1 = IV(REGD0)                                                   
      RD0 = RD1 - 1                                                     
C                                                                       
 20   CALL  DRN2G(V(D1), V(DR1), IV, LIV, LV, N, ND1, N1, N2, P, V(R1), 
     1            V(RD1), V, X)                                         
      IV1 = IV(1)                                                       
      IF (IV1-2) 40, 30, 80                                             
 30   IF (ND .GE. N) GO TO 70                                           
C                                                                       
C  ***  FIRST COMPUTE RELEVANT PORTION OF R  ***                        
C                                                                       
 40   NF = IV(NFCALL)                                                   
      IF (IABS(IV1) .GE. 2) NF = IV(NFGCAL)                             
      CALL CALCR(N, ND1, N1, N2, P, X, NF, V(R1), UI, UR, UF)           
      IF (NF .GT. 0) GO TO 50                                           
         IV(TOOBIG) = 1                                                 
         GO TO 20                                                       
 50   I = IV1 + 4                                                       
      GO TO (70, 60, 70, 20, 20, 70), I                                 
 60   X01 = IV(X0)                                                      
      CALL CALCJ(N, ND1, N1, N2, P, V(X01), IV(NF00), V(DR1), UI,       
     1           UR, UF)                                                
      IF (IV(NF00) .LE. 0) IV(TOOBIG) = 1                               
      GO TO 20                                                          
C                                                                       
C  ***  COMPUTE DR = GRADIENT OF R COMPONENTS  ***                      
C                                                                       
 70   CALL CALCJ(N, ND1, N1, N2, P, X, IV(NFGCAL), V(DR1), UI, UR, UF)  
      IF (IV(NFGCAL) .EQ. 0) IV(TOOBIG) = 1                             
      RD1 = RD0 + N1                                                    
      GO TO 20                                                          
C                                                                       
 80   RD1 = RD0 + 1                                                     
      IF (IV(REGD) .GT. 0) IV(REGD) = RD1                               
      IF (IV(1) .LE. 8) CALL DN2RDP(IV, LIV, LV, N, V(RD1), V)          
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST LINE OF   DN2P FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  DN2PB(N, ND, P, X, B, CALCR, CALCJ, IV, LIV, LV, V,   
     1                  UI, UR, UF)                                     
C                                                                       
C *** SIMPLY BOUNDED VERSION OF NL2SOL THAT HAS EXPANDED CALLING        
C *** SEQUENCES FOR CALCR, CALCJ, ALLOWING THEM TO PROVIDE R AND J      
C *** (RESIDUALS AND JACOBIAN) IN CHUNKS.                               
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
      INTEGER N, ND, P, LIV, LV                                         
C/6                                                                     
C     INTEGER IV(LIV), UI(1)                                            
C     DOUBLE PRECISION B(2,P), X(P), V(LV), UR(1)                       
C/7                                                                     
      INTEGER IV(LIV), UI(*)                                            
      DOUBLE PRECISION B(2,P), X(P), V(LV), UR(*)                       
C/                                                                      
      EXTERNAL CALCR, CALCJ, UF                                         
C                                                                       
C                                                                       
C  ***  PARAMETER USAGE  ***                                            
C                                                                       
C N....... TOTAL NUMBER OF RESIDUALS.                                   
C ND...... MAXIMUM NUMBER OF RESIDUAL COMPONENTS PROVIDED BY ANY CALL   
C             ON CALCR.                                                 
C P....... NUMBER OF PARAMETERS (COMPONENTS OF X) BEING ESTIMATED.      
C X....... PARAMETER VECTOR BEING ESTIMATED (INPUT = INITIAL GUESS,     
C             OUTPUT = BEST VALUE FOUND).                               
C CALCR... SUBROUTINE FOR COMPUTING RESIDUAL VECTOR.                    
C CALCJ... SUBROUTINE FOR COMPUTING JACOBIAN MATRIX = MATRIX OF FIRST   
C             PARTIALS OF THE RESIDUAL VECTOR.                          
C IV...... INTEGER VALUES ARRAY.                                        
C LIV..... LENGTH OF IV (SEE DISCUSSION BELOW).                         
C LV...... LENGTH OF V (SEE DISCUSSION BELOW).                          
C V....... FLOATING-POINT VALUES ARRAY.                                 
C UI...... PASSED UNCHANGED TO CALCR AND CALCJ.                         
C UR...... PASSED UNCHANGED TO CALCR AND CALCJ.                         
C UF...... PASSED UNCHANGED TO CALCR AND CALCJ.                         
C                                                                       
C                                                                       
C  ***  DISCUSSION  ***                                                 
C                                                                       
C    THIS ROUTINE IS SIMILAR TO   DN2G (WHICH SEE), EXCEPT THAT THE     
C CALLING SEQUENCE FOR CALCR AND CALCJ IS DIFFERENT -- IT ALLOWS        
C THE RESIDUAL VECTOR AND JACOBIAN MATRIX TO BE PASSED IN BLOCKS.       
C                                                                       
C   FOR CALCR, THE CALLING SEQUENCE IS...                               
C                                                                       
C     CALCR(N, ND1, N1, N2, P, X, NF, R, UI, UR, UF)                    
C                                                                       
C   PARAMETERS N, P, X, NF, R, UI, UR, UF ARE AS FOR THE CALCR USED     
C BY NL2SOL OR   DN2G.                                                  
C   PARAMETERS ND1, N1, AND N2 ARE INPUTS TO CALCR.  CALCR SHOULD NOT   
C CHANGE ND1 OR N1 (BUT MAY CHANGE N2).                                 
C   ND1 = MIN(N,ND) IS THE MAXIMUM NUMBER OF RESIDUAL COMPONENTS THAT   
C CALCR SHOULD SUPPLY ON ONE CALL.                                      
C   N1 IS THE INDEX OF THE FIRST RESIDUAL COMPONENT THAT CALCR SHOULD   
C SUPPLY ON THIS CALL.                                                  
C   N2 HAS THE VALUE MIN(N, N1+ND1-1) WHEN CALCR IS CALLED.  CALCR      
C MAY SET N2 TO A LOWER VALUE (AT LEAST 1) AND FOR N1 .LE. I .LE. N2    
C SHOULD RETURN RESIDUAL COMPONENT I IN R(I-N1+1), I.E., IN COMPONENTS  
C R(1), R(2), ..., R(N2-N1+1).                                          
C                                                                       
C   FOR CALCJ, THE CALLING SEQUENCE IS...                               
C                                                                       
C     CALCJ(N, ND1, N1, N2, P, X, NF, J, UI, UR, UF)                    
C                                                                       
C   ALL PARAMETERS EXCEPT N2 AND J ARE AS FOR CALCR.  N2 MAY NOT BE     
C CHANGED, BUT OTHERWISE IS AS FOR CALCR.  (WHENEVER CALCJ IS CALLED,   
C CALCR WILL JUST HAVE BEEN CALLED WITH THE SAME VALUE OF N1 BUT        
C POSSIBLY A DIFFERENT X -- NF IDENTIFIES THE X PASSED.  IF CALCR       
C CHANGES N2, THEN THIS CHANGED VALUE IS PASSED TO CALCJ.)              
C   J IS A FLOATING-POINT ARRAY DIMENSIONED J(ND1,P).  FOR I = N1(1)N2  
C AND K = 1(1)P, CALCJ MUST STORE THE PARTIAL DERIVATIVE AT X OF        
C RESIDUAL COMPONENT I WITH RESPECT TO X(K) IN J(I-N1+1,K).             
C                                                                       
C   LIV MUST BE AT LEAST 82 + P.  LV MUST BE AT LEAST                   
C         105 + P*(17 + 2*P) + (P+1)*MIN(ND,N) + N                      
C IF ND .LT. N AND NO REGRESSION DIAGNOSTIC ARRAY IS REQUESTED          
C (I.E., IV(RDREQ) = 0 OR 1), THEN LV CAN BE N LESS THAN THIS.          
C                                                                       
C+++++++++++++++++++++++++++  DECLARATIONS  +++++++++++++++++++++++++++ 
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      EXTERNAL DIVSET, DRN2GB                                           
C DIVSET.... PROVIDES DEFAULT IV AND V INPUT COMPONENTS.                
C DRN2GB... CARRIES OUT OPTIMIZATION ITERATIONS.                        
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      LOGICAL ONERD                                                     
      INTEGER D1, DR1, I, IV1, N1, N2, ND1, NF, R1, RD1, X01            
C                                                                       
C  ***  IV COMPONENTS  ***                                              
C                                                                       
      INTEGER D, J, NEXTV, NF00, NFCALL, NFGCAL, R,                     
     1        REGD0, TOOBIG, VNEED, X0                                  
C/6                                                                     
C     DATA D/27/, J/70/, NEXTV/47/, NF00/81/, NFCALL/6/, NFGCAL/7/,     
C    1     R/61/, REGD0/82/, TOOBIG/2/, VNEED/4/, X0/43/                
C/7                                                                     
      PARAMETER (D=27, J=70, NEXTV=47, NF00=81, NFCALL=6, NFGCAL=7,     
     1           R=61, REGD0=82, TOOBIG=2, VNEED=4, X0=43)              
C/                                                                      
C---------------------------------  BODY  ------------------------------
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)                  
      ND1 = MIN0(ND, N)                                                 
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 14) GO TO 10                                         
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10                        
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      I = IV(VNEED) + P + ND1*(P+1)                                     
      ONERD = ND .GE. N                                                 
      IF (ONERD) I = I + N                                              
      IF (IV(1) .EQ. 13) IV(VNEED) = I                                  
      CALL DRN2GB(B, V, V, IV, LIV, LV, N, ND1, N1, N2, P, V, V, V, X)  
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(D) = IV(NEXTV)                                                 
      IV(R) = IV(D) + P                                                 
      I = IV(R) + ND1                                                   
      IV(REGD0) = I                                                     
      IF (ONERD) I = I + N                                              
      IV(J) = I                                                         
      IV(NEXTV) = I + ND1*P                                             
      IF (IV1 .EQ. 13) GO TO 999                                        
C                                                                       
 10   D1 = IV(D)                                                        
      DR1 = IV(J)                                                       
      R1 = IV(R)                                                        
      RD1 = IV(REGD0)                                                   
C                                                                       
 20   CALL DRN2GB(B, V(D1), V(DR1), IV, LIV, LV, N, ND1, N1, N2, P,     
     1            V(R1), V(RD1), V, X)                                  
      IV1 = IV(1)                                                       
      IF (IV1-2) 40, 30, 999                                            
 30   IF (ND .GE. N) GO TO 70                                           
C                                                                       
C  ***  FIRST COMPUTE RELEVANT PORTION OF R  ***                        
C                                                                       
 40   NF = IV(NFCALL)                                                   
      IF (IABS(IV1) .GE. 2) NF = IV(NFGCAL)                             
      CALL CALCR(N, ND1, N1, N2, P, X, NF, V(R1), UI, UR, UF)           
      IF (NF .GT. 0) GO TO 50                                           
         IV(TOOBIG) = 1                                                 
         GO TO 20                                                       
 50   I = IV1 + 4                                                       
      GO TO (70, 60, 70, 20, 20, 70), I                                 
 60   X01 = IV(X0)                                                      
      CALL CALCJ(N, ND1, N1, N2, P, V(X01), IV(NF00), V(DR1), UI,       
     1           UR, UF)                                                
      IF (IV(NF00) .LE. 0) IV(TOOBIG) = 1                               
      GO TO 20                                                          
C                                                                       
C  ***  COMPUTE DR = GRADIENT OF R COMPONENTS  ***                      
C                                                                       
 70   CALL CALCJ(N, ND1, N1, N2, P, X, IV(NFGCAL), V(DR1), UI, UR, UF)  
      IF (IV(NFGCAL) .EQ. 0) IV(TOOBIG) = 1                             
      GO TO 20                                                          
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST LINE OF  DN2PB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE   DNSF(N, P, L, ALF, C, Y, CALCA, INC, IINC, IV,       
     1                  LIV, LV, V, UIPARM, URPARM, UFPARM)             
C                                                                       
C  ***  SOLVE SEPARABLE NONLINEAR LEAST SQUARES USING                   
C  ***  FINITE-DIFFERENCE DERIVATIVES.                                  
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER IINC, L, LIV, LV, N, P                                    
C/6                                                                     
C     INTEGER INC(IINC,P), IV(LIV), UIPARM(1)                           
C     DOUBLE PRECISION ALF(P), C(L), URPARM(1), V(LV), Y(N)             
C/7                                                                     
      INTEGER INC(IINC,P), IV(LIV), UIPARM(*)                           
      DOUBLE PRECISION ALF(P), C(L), URPARM(*), V(LV), Y(N)             
C/                                                                      
      EXTERNAL CALCA, UFPARM                                            
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
C      N (IN)  NUMBER OF OBSERVATIONS.                                  
C      P (IN)  NUMBER OF NONLINEAR PARAMETERS TO BE ESTIMATED.          
C      L (IN)  NUMBER OF LINEAR PARAMETERS TO BE ESTIMATED.             
C    ALF (I/O) NONLINEAR PARAMETERS.                                    
C                 INPUT = INITIAL GUESS,                                
C                 OUTPUT = BEST ESTIMATE FOUND.                         
C      C (OUT) LINEAR PARAMETERS (ESTIMATED).                           
C      Y (IN)  RIGHT-HAND SIDE VECTOR.                                  
C  CALCA (IN)  SUBROUTINE TO COMPUTE A MATRIX.                          
C    INC (IN)  INCIDENCE MATRIX OF DEPENDENCIES OF COLUMNS OF A ON      
C                 COMPONENTS OF ALF -- INC(I,J) = 1 MEANS COLUMN I      
C                 OF A DEPENDS ON ALF(J).                               
C   IINC (IN)  DECLARED LEAD DIMENSION OF INC.  MUST BE AT LEAST L+1.   
C     IV (I/O) INTEGER PARAMETER AND SCRATCH VECTOR.                    
C    LIV (IN)  LENGTH OF IV.  MUST BE AT LEAST                          
C                 122 + 2*M + 4*P + 2*L + MAX(L+1,6*P), WHERE  M  IS    
C                 THE NUMBER OF ONES IN INC.                            
C     LV (IN)  LENGTH OF V.  MUST BE AT LEAST                           
C                 105 + 2*N*(L+3) + JLEN + L*(L+3)/2 + P*(2*P + 18),    
C                 WHERE  JLEN = (L+P)*(N+L+P+1),  UNLESS NEITHER A      
C                 COVARIANCE MATRIX NOR REGRESSION DIAGNOSTICS ARE      
C                 REQUESTED, IN WHICH CASE  JLEN = N*P.  IF THE LAST    
C                 ROW OF INC CONTAINS ONLY ZEROS, THEN LV CAN BE 4*N    
C                 LESS THAN JUST DESCRIBED.                             
C      V (I/O) FLOATING-POINT PARAMETER AND SCRATCH VECTOR.             
C UIPARM (I/O) INTEGER VECTOR PASSED WITHOUT CHANGE TO CALCA.           
C URPARM (I/O) FLOATING-POINT VECTOR PASSED WITHOUT CHANGE TO CALCA.    
C UFPARM (I/O) SUBROUTINE PASSED (WITHOUT HAVING BEEN CALLED) TO CALCA. 
C                                                                       
C                                                                       
C--------------------------  DECLARATIONS  ---------------------------- 
C                                                                       
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      EXTERNAL DIVSET, DSM,  DRNSG,DV2AXY,DV7CPY, DV7SCL                
C                                                                       
C DIVSET.... PROVIDES DEFAULT IV AND V VALUES.                          
C DSM...... DETERMINES EFFICIENT ORDER FOR FINITE DIFFERENCES.          
C  DRNSG... CARRIES OUT NL2SOL ALGORITHM.                               
C DV2AXY.... ADDS A MULTIPLE OF ONE VECTOR TO ANOTHER.                  
C DV7CPY.... COPIES ONE VECTOR TO ANOTHER.                              
C DV7SCL... SCALES AND COPIES ONE VECTOR TO ANOTHER.                    
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      LOGICAL PARTJ                                                     
      INTEGER A0, A1, AJ, ALP1, BWA1, D0, DA0, DA1, DAJ, GPTR1, GRP1,   
     1        GRP2, I, I1, IN0, IN1, IN2, INI, INLEN, IPNTR1, IV1, IWA1,
     2        IWALEN, J1, JN1, JPNTR1, K, L1, LP1, M, M0, NF, NG, NGRP0,
     3        NGRP1, NGRP2, RSAVE0, RSAVE1, RSVLEN, X0I, XSAVE0, XSAVE1 
      DOUBLE PRECISION DELTA, DI, H, XI                                 
      DOUBLE PRECISION NEGONE, ONE, ZERO                                
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER AMAT, COVREQ, D, DAMAT, DLTFDJ, GPTR, GRP, IN, IVNEED,    
     1        L1SAV, MAXGRP, MODE, MSAVE, NEXTIV, NEXTV, NFCALL, NFGCAL,
     2        PERM, RESTOR, TOOBIG, VNEED, XSAVE                        
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C/6                                                                     
C     DATA AMAT/113/, COVREQ/15/, D/27/, DAMAT/114/, DLTFDJ/43/,        
C    1     GPTR/117/, GRP/118/, IN/112/, IVNEED/3/, L1SAV/111/,         
C    2     MAXGRP/116/, MODE/35/, MSAVE/115/, NEXTIV/46/, NEXTV/47/,    
C    3     NFCALL/6/, NFGCAL/7/, PERM/58/, RESTOR/9/, TOOBIG/2/,        
C    4     VNEED/4/, XSAVE/119/                                         
C/7                                                                     
      PARAMETER (AMAT=113, COVREQ=15, D=27, DAMAT=114, DLTFDJ=43,       
     1           GPTR=117, GRP=118, IN=112, IVNEED=3, L1SAV=111,        
     2           MAXGRP=116, MODE=35, MSAVE=115, NEXTIV=46, NEXTV=47,   
     3           NFCALL=6, NFGCAL=7, PERM=58, RESTOR=9, TOOBIG=2,       
     4           VNEED=4, XSAVE=119)                                    
C/                                                                      
      DATA NEGONE/-1.D+0/, ONE/1.D+0/, ZERO/0.D+0/                      
C                                                                       
C++++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++ 
C                                                                       
      LP1 = L + 1                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)                  
      IF (P .LE. 0 .OR. L .LT. 0 .OR. IINC .LE. L) GO TO 80             
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 14) GO TO 120                                        
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 120                       
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      IF (IV(1) .NE. 13) GO TO 50                                       
C                                                                       
C  ***  FRESH START ***                                                 
C                                                                       
      IF (IV(PERM) .LE. XSAVE) IV(PERM) = XSAVE + 1                     
C                                                                       
C  ***  CHECK INC, COUNT ITS NONZEROS                                   
C                                                                       
      L1 = 0                                                            
      M = 0                                                             
      DO 40 I = 1, P                                                    
         M0 = M                                                         
         IF (L .EQ. 0) GO TO 20                                         
         DO 10 K = 1, L                                                 
            IF (INC(K,I) .LT. 0 .OR. INC(K,I) .GT. 1) GO TO 80          
            IF (INC(K,I) .EQ. 1) M = M + 1                              
 10         CONTINUE                                                    
 20      IF (INC(LP1,I) .NE. 1) GO TO 30                                
            M = M + 1                                                   
            L1 = 1                                                      
 30      IF (M .EQ. M0 .OR. INC(LP1,I) .LT. 0                           
     1                 .OR. INC(LP1,I) .GT. 1) GO TO 80                 
 40      CONTINUE                                                       
C                                                                       
C     *** NOW L1 = 1 MEANS A HAS COLUMN L+1 ***                         
C                                                                       
C     *** COMPUTE STORAGE REQUIREMENTS ***                              
C                                                                       
      IWALEN = MAX0(LP1, 6*P)                                           
      INLEN = 2 * M                                                     
      IV(IVNEED) = IV(IVNEED) + INLEN + 3*P + L + IWALEN + 3            
      RSVLEN = 2 * L1 * N                                               
      L1 = L + L1                                                       
      IV(VNEED) = IV(VNEED) + 2*N*L1 + RSVLEN + P                       
C                                                                       
 50   CALL  DRNSG(V, ALF, C, V, IV, IV, L, 1, N, LIV, LV, N, M, P, V, Y)
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(IN) = IV(NEXTIV)                                               
      IV(AMAT) = IV(NEXTV)                                              
      IV(DAMAT) = IV(AMAT) + N*L1                                       
      IV(XSAVE) = IV(DAMAT) + N*L1                                      
      IV(NEXTV) = IV(XSAVE) + P + RSVLEN                                
      IV(L1SAV) = L1                                                    
      IV(MSAVE) = M                                                     
C                                                                       
C  ***  DETERMINE HOW MANY GROUPS FOR FINITE DIFFERENCES                
C  ***  (SET UP TO CALL DSM)                                            
C                                                                       
      IN1 = IV(IN)                                                      
      JN1 = IN1 + M                                                     
      DO 70 K = 1, P                                                    
         DO 60 I = 1, LP1                                               
            IF (INC(I,K) .EQ. 0) GO TO 60                               
               IV(IN1) = I                                              
               IN1 = IN1 + 1                                            
               IV(JN1) = K                                              
               JN1 = JN1 + 1                                            
 60         CONTINUE                                                    
 70      CONTINUE                                                       
      IN1 = IV(IN)                                                      
      JN1 = IN1 + M                                                     
      IWA1 = IN1 + INLEN                                                
      NGRP1 = IWA1 + IWALEN                                             
      BWA1 = NGRP1 + P                                                  
      IPNTR1 = BWA1 + P                                                 
      JPNTR1 = IPNTR1 + L + 2                                           
      CALL DSM(LP1, P, M, IV(IN1), IV(JN1), IV(NGRP1), NG, K, I,        
     1         IV(IPNTR1), IV(JPNTR1), IV(IWA1), IWALEN, IV(BWA1))      
      IF (I .EQ. 1) GO TO 90                                            
         IV(1) = 69                                                     
         GO TO 50                                                       
 80   IV(1) = 66                                                        
      GO TO 50                                                          
C                                                                       
C  ***  SET UP GRP AND GPTR ARRAYS FOR COMPUTING FINITE DIFFERENCES     
C                                                                       
C  ***  THERE ARE NG GROUPS.  GROUP I CONTAINS ALF(GRP(J)) FOR          
C  ***  GPTR(I) .LE. J .LE. GPTR(I+1)-1.                                
C                                                                       
 90   IV(MAXGRP) = NG                                                   
      IV(GPTR) = IN1 + 2*L1                                             
      GPTR1 = IV(GPTR)                                                  
      IV(GRP) = GPTR1 + NG + 1                                          
      IV(NEXTIV) = IV(GRP) + P                                          
      GRP1 = IV(GRP)                                                    
      NGRP0 = NGRP1 - 1                                                 
      NGRP2 = NGRP0 + P                                                 
      DO 110 I = 1, NG                                                  
         IV(GPTR1) = GRP1                                               
         GPTR1 = GPTR1 + 1                                              
         DO 100 I1 = NGRP1, NGRP2                                       
            IF (IV(I1) .NE. I) GO TO 100                                
            IV(GRP1) = I1 - NGRP0                                       
            GRP1 = GRP1 + 1                                             
 100        CONTINUE                                                    
 110     CONTINUE                                                       
      IV(GPTR1) = GRP1                                                  
      IF (IV1 .EQ. 13) GO TO 999                                        
C                                                                       
C  ***  INITIALIZE POINTERS  ***                                        
C                                                                       
 120  A1 = IV(AMAT)                                                     
      A0 = A1 - N                                                       
      DA1 = IV(DAMAT)                                                   
      DA0 = DA1 - N                                                     
      IN1 = IV(IN)                                                      
      IN0 = IN1 - 2                                                     
      L1 = IV(L1SAV)                                                    
      IN2 = IN1 + 2*L1 - 1                                              
      D0 = IV(D) - 1                                                    
      NG = IV(MAXGRP)                                                   
      XSAVE1 = IV(XSAVE)                                                
      XSAVE0 = XSAVE1 - 1                                               
      RSAVE1 = XSAVE1 + P                                               
      RSAVE0 = RSAVE1 + N                                               
      ALP1 = A1 + L*N                                                   
      DELTA = V(DLTFDJ)                                                 
      IV(COVREQ) = -IABS(IV(COVREQ))                                    
C                                                                       
 130  CALL  DRNSG(V(A1), ALF, C, V(DA1), IV(IN1), IV, L, L1, N, LIV, LV,
     1            N, L1, P, V, Y)                                       
      IF (IV(1)-2) 140, 150, 999                                        
C                                                                       
C  ***  NEW FUNCTION VALUE (R VALUE) NEEDED  ***                        
C                                                                       
 140  NF = IV(NFCALL)                                                   
      CALL CALCA(N, P, L, ALF, NF, V(A1), UIPARM, URPARM, UFPARM)       
      IF (NF .LE. 0) IV(TOOBIG) = 1                                     
      IF (L1 .LE. L) GO TO 130                                          
      IF (IV(RESTOR) .EQ. 2) CALL DV7CPY(N, V(RSAVE0), V(RSAVE1))       
      CALL DV7CPY(N, V(RSAVE1), V(ALP1))                                
      GO TO 130                                                         
C                                                                       
C  ***  COMPUTE DR = GRADIENT OF R COMPONENTS  ***                      
C                                                                       
 150  IF (L1 .GT. L .AND. IV(NFGCAL) .EQ. IV(NFCALL))                   
     1      CALL DV7CPY(N, V(RSAVE0), V(RSAVE1))                        
      GPTR1 = IV(GPTR)                                                  
      DO 230 K = 1, NG                                                  
         CALL DV7CPY(P, V(XSAVE1), ALF)                                 
         GRP1 = IV(GPTR1)                                               
         GRP2 = IV(GPTR1+1) - 1                                         
         GPTR1 = GPTR1 + 1                                              
         DO 160 I1 = GRP1, GRP2                                         
            I = IV(I1)                                                  
            XI = ALF(I)                                                 
            J1 = D0 + I                                                 
            DI = V(J1)                                                  
            IF (DI .LE. ZERO) DI = ONE                                  
            H = DELTA * DMAX1(DABS(XI), ONE/DI)                         
            IF (XI .LT. ZERO) H = -H                                    
            X0I = XSAVE0 + I                                            
            V(X0I) = XI + H                                             
 160        CONTINUE                                                    
         CALL CALCA(N, P, L, V(XSAVE1), IV(NFGCAL), V(DA1),             
     1              UIPARM, URPARM, UFPARM)                             
         IF (IV(NFGCAL) .GT. 0) GO TO 170                               
            IV(TOOBIG) = 1                                              
            GO TO 130                                                   
 170     JN1 = IN1                                                      
         DO 180 I = IN1, IN2                                            
 180        IV(I) = 0                                                   
         PARTJ = IV(MODE) .LE. P                                        
         DO 220 I1 = GRP1, GRP2                                         
            I = IV(I1)                                                  
            DO 210 J1 = 1, L1                                           
               IF (INC(J1,I) .EQ. 0) GO TO 210                          
               INI = IN0 + 2*J1                                         
               IV(INI) = I                                              
               IV(INI+1) = J1                                           
               X0I = XSAVE0 + I                                         
               H = ONE / (V(X0I) - ALF(I))                              
               DAJ = DA0 + J1*N                                         
               IF (PARTJ) GO TO 190                                     
C                 *** FULL FINITE DIFFERENCE FOR COV. AND REG. DIAG. ***
                  AJ = A0 + J1*N                                        
                  CALL DV2AXY(N, V(DAJ), NEGONE, V(AJ), V(DAJ))         
                  GO TO 200                                             
 190           IF (J1 .GT. L)                                           
     1            CALL DV2AXY(N, V(DAJ), NEGONE, V(RSAVE0), V(DAJ))     
 200           CALL DV7SCL(N, V(DAJ), H, V(DAJ))                        
 210           CONTINUE                                                 
 220        CONTINUE                                                    
         IF (K .GE. NG) GO TO 240                                       
         IV(1) = -2                                                     
         CALL  DRNSG(V(A1), ALF, C, V(DA1), IV(IN1), IV, L, L1, N, LIV, 
     1               LV, N, L1, P, V, Y)                                
         IF (-2 .NE. IV(1)) GO TO 999                                   
 230     CONTINUE                                                       
 240  IV(1) = 2                                                         
      GO TO 130                                                         
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF   DNSF FOLLOWS  ***                                
      END                                                               
      SUBROUTINE   DNSG(N, P, L, ALF, C, Y, CALCA, CALCB, INC, IINC, IV,
     1                  LIV, LV, V, UIPARM, URPARM, UFPARM)             
C                                                                       
C  ***  SOLVE SEPARABLE NONLINEAR LEAST SQUARES USING  ***              
C  ***  ANALYTICALLY COMPUTED DERIVATIVES.             ***              
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER IINC, L, LIV, LV, N, P                                    
C/6                                                                     
C     INTEGER INC(IINC,P), IV(LIV), UIPARM(1)                           
C     DOUBLE PRECISION ALF(P), C(L), URPARM(1), V(LV), Y(N)             
C/7                                                                     
      INTEGER INC(IINC,P), IV(LIV), UIPARM(*)                           
      DOUBLE PRECISION ALF(P), C(L), URPARM(*), V(LV), Y(N)             
C/                                                                      
      EXTERNAL CALCA, CALCB, UFPARM                                     
C                                                                       
C  ***  PURPOSE  ***                                                    
C                                                                       
C GIVEN A SET OF N OBSERVATIONS Y(1)....Y(N) OF A DEPENDENT VARIABLE    
C T(1)...T(N),   DNSG ATTEMPTS TO COMPUTE A LEAST SQUARES FIT           
C TO A FUNCTION  ETA  (THE MODEL) WHICH IS A LINEAR COMBINATION         
C                                                                       
C                  L                                                    
C ETA(C,ALF,T) =  SUM C * PHI(ALF,T) +PHI   (ALF,T)                     
C                 J=1  J     J           L+1                            
C                                                                       
C OF NONLINEAR FUNCTIONS PHI(J) DEPENDENT ON T AND ALF(1),...,ALF(P)    
C (.E.G. A SUM OF EXPONENTIALS OR GAUSSIANS).  THAT IS, IT DETERMINES   
C NONLINEAR PARAMETERS ALF WHICH MINIMIZE                               
C                                                                       
C                   2    N                      2                       
C     NORM(RESIDUAL)  = SUM  (Y - ETA(C,ALF,T )).                       
C                       I=1    I             I                          
C                                                                       
C THE (L+1)ST TERM IS OPTIONAL.                                         
C                                                                       
C--------------------------  PARAMETER USAGE  ------------------------- 
C                                                                       
C INPUT PARAMETERS                                                      
C                                                                       
C N     INTEGER        NUMBER OF OBSERVATIONS (MUST BE .GE. MAX(L,P)).  
C                                                                       
C P     INTEGER        NUMBER OF NONLINEAR PARAMETERS (MUST BE .GE. 1). 
C                                                                       
C L     INTEGER        NUMBER OF LINEAR PARAMETERS (MUST BE .GE. 0).    
C                                                                       
C ALF   D.P. ARRAY     P VECTOR = INITIAL ESTIMATE OF THE NONLINEAR     
C                      PARAMETERS.                                      
C                                                                       
C CALCA SUBROUTINE     USER PROVIDED FUNCTION TO CALCULATE THE MODEL    
C                      (I.E., TO CALCULATE PHI) -- SEE THE NOTE BELOW   
C                      ON THE CALLING SEQUENCE FOR CALCA.               
C                      CALCA MUST BE DECLARED EXTERNAL IN THE CALLING   
C                      PROGRAM.                                         
C                                                                       
C CALCB SUBROUTINE     USER PROVIDED FUNCTION TO CALCULATE THE DERIVA-  
C                      TIVE OF THE MODEL (I.E., OF PHI) WITH RESPECT TO 
C                      ALF -- SEE THE NOTE BELOW ON THE CALLING         
C                      SEQUENCE FOR CALCB.  CALCB MUST BE DECLARED      
C                      EXTERNAL IN THE CALLING PROGRAM.                 
C                                                                       
C Y     D.P. ARRAY     VECTOR OF OBSERVATIONS.                          
C                                                                       
C INC   INTEGER ARRAY  A 2 DIM. ARRAY OF DIMENSION AT LEAST (L+1,P)     
C                      INDICATING THE POSITION OF THE NONLINEAR PARA-   
C                      METERS IN THE MODEL.  SET INC(J,K) = 1 IF ALF(K) 
C                      APPEARS IN PHI(J).  OTHERWISE SET INC(J,K) = 0.  
C                      IF PHI((L+1)) IS NOT IN THE MODEL, SET THE L+1ST 
C                      ROW OF INC TO ALL ZEROS.  EVERY COLUMN OF INC    
C                      MUST CONTAIN AT LEAST ONE 1.                     
C                                                                       
C IINC   INTEGER       DECLARED ROW DIMENSION OF INC, WHICH MUST BE AT  
C                      LEAST L+1.                                       
C                                                                       
C IV     INTEGER       ARRAY OF LENGTH AT LEAST LIV THAT CONTAINS       
C                      VARIOUS PARAMETERS FOR THE SUBROUTINE, SUCH AS   
C                      THE ITERATION AND FUNCTION EVALUATION LIMITS AND 
C                      SWITCHES THAT CONTROL PRINTING.  THE INPUT COM-  
C                      PONENTS OF IV ARE DESCRIBED IN DETAIL IN THE     
C                      PORT OPTIMIZATION DOCUMENTATION.                 
C                         IF IV(1)=0 ON INPUT, THEN DEFAULT PARAMETERS  
C                      ARE SUPPLIED TO IV AND V.  THE CALLER MAY SUPPLY 
C                      NONDEFAULT PARAMETERS TO IV AND V BY EXECUTING A 
C                      CALL DIVSET(1,IV,LIV,LV,V) AND THEN ASSIGNING    
C                      NONDEFAULT VALUES TO THE APPROPRIATE COMPONENTS  
C                      OF IV AND V BEFORE CALLING   DNSG.               
C                                                                       
C LIV     INTEGER      LENGTH OF IV.  MUST BE AT LEAST  115+P+L + 2*M,  
C                      WHERE  M  IS THE NUMBER OF ONES IN INC.          
C                                                                       
C LV      INTEGER      LENGTH OF V.  MUST BE AT LEAST                   
C                      105 + N*(L+M+3) + JLEN + L*(L+3)/2 + P*(2*P+17), 
C                      WHERE  M  IS AS FOR LIV (SEE ABOVE) AND          
C                      JLEN = (L+P)*(N+L+P+1),  UNLESS NEITHER A        
C                      COVARIANCE MATRIX NOR REGRESSION DIAGNOSTICS ARE 
C                      REQUESTED, IN WHICH CASE  JLEN = N*P.  IF THE    
C                      LAST ROW OF INC CONTAINS ONLY ZEROS, THEN LV     
C                      CAN BE N LESS THAN JUST DESCRIBED.               
C                                                                       
C V       D.P. ARRAY   WORK AND PARAMETER ARRAY OF LENGTH AT LEAST LV   
C                      THAT CONTAINS SUCH INPUT COMPONENTS AS THE       
C                      CONVERGENCE TOLERANCES.  THE INPUT COMPONENTS OF 
C                      V MAY BE SUPPLIED AS FOR IV (SEE ABOVE).  NOTE   
C                      THAT V(35) CONTAINS THE INITIAL STEP BOUND,      
C                      WHICH, IF TOO LARGE, MAY LEAD TO OVERFLOW.       
C                                                                       
C UIPARM INTEGER ARRAY SCRATCH SPACE FOR USER TO SEND INFORMATION       
C                      TO CALCA AND CALCB.                              
C                                                                       
C URPARM D.P. ARRAY    SCRATCH SPACE FOR USER TO SEND INFORMATION       
C                      TO CALCA AND CALCB.                              
C                                                                       
C UFPARM EXTERNAL      SUBROUTINE SENT TO CALCA AND CALCB FOR THEIR     
C                      USE.  NOTE THAT THE SUBROUTINE PASSED FOR UFPARM 
C                      MUST BE DECLARED EXTERNAL IN THE CALLING PROGRAM.
C                                                                       
C                                                                       
C OUTPUT PARAMETERS                                                     
C                                                                       
C ALF    D.P. ARRAY    FINAL NONLINEAR PARAMETERS.                      
C                                                                       
C C      D.P. ARRAY    L VECTOR OF LINEAR PARAMETERS -- NOTE THAT NO    
C                      INITIAL GUESS FOR C IS REQUIRED.                 
C                                                                       
C IV                   IV(1) CONTAINS A RETURN CODE DESCRIBED IN THE    
C                      PORT OPTIMIZATION DOCUMENTATION.  IF IV(1) LIES  
C                      BETWEEN 3 AND 7, THEN THE ALGORITHM HAS          
C                      CONVERGED (BUT IV(1) = 7 INDICATES POSSIBLE      
C                      TROUBLE WITH THE MODEL).  IV(1) = 9 OR 10 MEANS  
C                      FUNCTION EVALUATION OR ITERATION LIMIT REACHED.  
C                      IV(1) = 66 MEANS BAD PARAMETERS (INCLUDING A     
C                      COLUMN OF ZEROS IN INC).  NOTE THAT THE          
C                      ALGORITHM CAN BE RESTARTED AFTER ANY RETURN WITH 
C                      IV(1) .LT. 12 -- SEE THE PORT DOCUMENTATION.     
C                                                                       
C V                    VARIOUS ITEMS OF INTEREST, INCLUDING THE NORM OF 
C                      THE GRADIENT(1) AND THE FUNCTION VALUE(10).  SEE 
C                      THE PORT DOCUMENTATION FOR A COMPLETE LIST.      
C                                                                       
C                                                                       
C                                                                       
C PARAMETERS FOR CALCA(N,P,L,ALF,NF,PHI, UIPARM,URPARM,UFPARM)          
C                                                                       
C N,L,P,ALF ARE INPUT PARAMETERS AS DESCRIBED ABOVE                     
C                                                                       
C PHI    D.P. ARRAY  N*(L+1) ARRAY WHOSE COLUMNS CONTAIN THE TERMS OF   
C                    THE MODEL.  CALCA MUST EVALUATE PHI(ALF) AND STORE 
C                    THE RESULT IN PHI.  IF THE (L+1)ST TERM IS NOT IN  
C                    THE MODEL, THEN NOTHING SHOULD BE STORED IN THE    
C                    (L+1)ST COLUMN OF PHI.                             
C                                                                       
C NF     INTEGER     CURRENT INVOCATION COUNT FOR CALCA.  IF PHI CANNOT 
C                    BE EVALUATED AT ALF (E.G. BECAUSE AN ARGUMENT TO   
C                    AN INTRINSIC FUNCTION IS OUT OF RANGE), THEN CALCA 
C                    SHOULD SIMPLY SET NF TO 0 AND RETURN.  THIS        
C                    TELLS THE ALGORITHM TO TRY A SMALLER STEP.         
C                                                                       
C UIPARM,URPARM,UFPARM ARE AS DESCRIBED ABOVE                           
C                                                                       
C N.B. THE DEPENDENT VARIABLE T IS NOT EXPLICITLY PASSED.  IF REQUIRED, 
C IT MAY BE PASSED IN UIPARM OR URPARM OR STORED IN NAMED COMMON.       
C                                                                       
C                                                                       
C PARAMETERS FOR CALCB(N,P,L,ALF,NF,DER, UIPARM,URPARM,UFPARM)          
C                                                                       
C N,P,L,ALF,NF,UIPARM,URPARM,UFPARM ARE AS FOR CALCA                    
C                                                                       
C DER   D.P. ARRAY   N*M ARRAY, WHERE M IS THE NUMBER OF ONES IN INC.   
C                    CALCB MUST SET DER TO THE DERIVATIVES OF THE MODEL 
C                    WITH RESPECT TO ALF.  IF THE MODEL HAS K TERMS THAT
C                    DEPEND ON ALF(I), THEN DER WILL HAVE K CONSECUTIVE 
C                    COLUMNS OF DERIVATIVES WITH RESPECT TO ALF(I).  THE
C                    COLUMNS OF DER CORRESPOND TO THE ONES IN INC WHEN  
C                    ONE TRAVELS THROUGH INC BY COLUMNS.  FOR EXAMPLE,  
C                    IF INC HAS THE FORM...                             
C                      1  1  0                                          
C                      0  1  0                                          
C                      1  0  0                                          
C                      0  0  1                                          
C                    THEN THE FIRST TWO COLUMNS OF DER ARE FOR THE      
C                    DERIVATIVES OF COLUMNS 1 AND 3 OF PHI WITH RESPECT 
C                    TO ALF(1), COLUMNS 3 AND 4 OF DER ARE FOR THE      
C                    DERIVATIVES OF COLUMNS 1 AND 2 OF PHI WITH RESPECT 
C                    TO ALF(2), AND COLUMN 5 OF DER IS FOR THE DERIVA-  
C                    TIVE OF COLUMN 4 OF PHI WITH RESPECT TO ALF(3).    
C                    MORE SPECIFICALLY, DER(I,2) IS FOR THE DERIVATIVE  
C                    OF PHI(I,3) WITH RESPECT TO ALF(1) AND DER(I,5) IS 
C                    FOR THE DERIVATIVE OF PHI(I,4) WITH RESPECT TO     
C                    ALF(3) (FOR I = 1,2,...,N).                        
C                       THE VALUE OF ALF PASSED TO CALCB IS THE SAME AS 
C                    THAT PASSED TO CALCA THE LAST TIME IT WAS CALLED.  
C                    (IF DER CANNOT BE EVALUATED, THEN CALCB SHOULD SET 
C                    NF TO 0.  THIS WILL CAUSE AN ERROR RETURN.)        
C                                                                       
C N.B. DER IS FOR DERIVATIVES WITH RESPECT TO ALF, NOT T.               
C                                                                       
C------------------------------  NOTES  ------------------------------- 
C                                                                       
C      THIS PROGRAM WAS WRITTEN BY LINDA KAUFMAN AT BELL LABS, MURRAY   
C HILL, N.J. IN 1977 AND EXTENSIVELY REVISED BY HER AND DAVID GAY IN    
C 1980, 1981, 1983, 1984.  THE WORK OF DAVID GAY WAS SUPPORTED IN PART  
C BY NATIONAL SCIENCE FOUNDATION GRANT MCS-7906671.                     
C                                                                       
C--------------------------  DECLARATIONS  ---------------------------- 
C                                                                       
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      EXTERNAL DIVSET,  DRNSG                                           
C                                                                       
C DIVSET.... PROVIDES DEFAULT IV AND V VALUES.                          
C  DRNSG... CARRIES OUT NL2SOL ALGORITHM.                               
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER A1, DA1, I, IN1, IV1, K, L1, LP1, M, M0, NF               
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER AMAT, DAMAT, IN, IVNEED, L1SAV, MSAVE, NEXTIV,            
     1        NEXTV, NFCALL, NFGCAL, PERM, TOOBIG, VNEED                
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C/6                                                                     
C     DATA AMAT/113/, DAMAT/114/, IN/112/, IVNEED/3/, L1SAV/111/,       
C    1     MSAVE/115/, NEXTIV/46/, NEXTV/47/, NFCALL/6/, NFGCAL/7/,     
C    2     PERM/58/, TOOBIG/2/, VNEED/4/                                
C/7                                                                     
      PARAMETER (AMAT=113, DAMAT=114, IN=112, IVNEED=3, L1SAV=111,      
     1           MSAVE=115, NEXTIV=46, NEXTV=47, NFCALL=6, NFGCAL=7,    
     2           PERM=58, TOOBIG=2, VNEED=4)                            
C/                                                                      
C                                                                       
C++++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++ 
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)                  
      IF (P .LE. 0 .OR. L .LT. 0 .OR. IINC .LE. L) GO TO 50             
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 14) GO TO 90                                         
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 90                        
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      IF (IV(1) .NE. 13) GO TO 60                                       
      IF (IV(PERM) .LE. MSAVE) IV(PERM) = MSAVE + 1                     
      LP1 = L + 1                                                       
      L1 = 0                                                            
      M = 0                                                             
      DO 40 I = 1, P                                                    
         M0 = M                                                         
         IF (L .EQ. 0) GO TO 20                                         
         DO 10 K = 1, L                                                 
            IF (INC(K,I) .LT. 0 .OR. INC(K,I) .GT. 1) GO TO 50          
            IF (INC(K,I) .EQ. 1) M = M + 1                              
 10         CONTINUE                                                    
 20      IF (INC(LP1,I) .NE. 1) GO TO 30                                
            M = M + 1                                                   
            L1 = 1                                                      
 30      IF (M .EQ. M0 .OR. INC(LP1,I) .LT. 0                           
     1                 .OR. INC(LP1,I) .GT. 1) GO TO 50                 
 40      CONTINUE                                                       
C                                                                       
      IV(IVNEED) = IV(IVNEED) + 2*M                                     
      L1 = L + L1                                                       
      IV(VNEED) = IV(VNEED) + N*(L1+M)                                  
      GO TO 60                                                          
C                                                                       
 50   IV(1) = 66                                                        
C                                                                       
 60   CALL  DRNSG(V, ALF, C, V, IV, IV, L, 1, N, LIV, LV, N, M, P, V, Y)
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(IN) = IV(NEXTIV)                                               
      IV(NEXTIV) = IV(IN) + 2*M                                         
      IV(AMAT) = IV(NEXTV)                                              
      IV(DAMAT) = IV(AMAT) + N*L1                                       
      IV(NEXTV) = IV(DAMAT) + N*M                                       
      IV(L1SAV) = L1                                                    
      IV(MSAVE) = M                                                     
C                                                                       
C  ***  SET UP IN ARRAY  ***                                            
C                                                                       
      IN1 = IV(IN)                                                      
      DO 80 I = 1, P                                                    
         DO 70 K = 1, LP1                                               
            IF (INC(K,I) .EQ. 0) GO TO 70                               
               IV(IN1) = I                                              
               IV(IN1+1) = K                                            
               IN1 = IN1 + 2                                            
 70         CONTINUE                                                    
 80      CONTINUE                                                       
      IF (IV1 .EQ. 13) GO TO 999                                        
C                                                                       
 90   A1 = IV(AMAT)                                                     
      DA1 = IV(DAMAT)                                                   
      IN1 = IV(IN)                                                      
      L1 = IV(L1SAV)                                                    
      M = IV(MSAVE)                                                     
C                                                                       
 100  CALL  DRNSG(V(A1), ALF, C, V(DA1), IV(IN1), IV, L, L1, N, LIV, LV,
     1            N, M, P, V, Y)                                        
      IF (IV(1)-2) 110, 120, 999                                        
C                                                                       
C  ***  NEW FUNCTION VALUE (R VALUE) NEEDED  ***                        
C                                                                       
 110  NF = IV(NFCALL)                                                   
      CALL CALCA(N, P, L, ALF, NF, V(A1), UIPARM, URPARM, UFPARM)       
      IF (NF .LE. 0) IV(TOOBIG) = 1                                     
      GO TO 100                                                         
C                                                                       
C  ***  COMPUTE DR = GRADIENT OF R COMPONENTS  ***                      
C                                                                       
 120  CALL CALCB(N, P, L, ALF, IV(NFGCAL), V(DA1), UIPARM, URPARM,      
     1           UFPARM)                                                
      IF (IV(NFGCAL) .EQ. 0) IV(TOOBIG) = 1                             
      GO TO 100                                                         
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF   DNSG FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DRMNF(D, FX, IV, LIV, LV, N, V, X)                     
C                                                                       
C  ***  ITERATION DRIVER FOR  DMNF...                                   
C  ***  MINIMIZE GENERAL UNCONSTRAINED OBJECTIVE FUNCTION USING         
C  ***  FINITE-DIFFERENCE GRADIENTS AND SECANT HESSIAN APPROXIMATIONS.  
C                                                                       
      INTEGER LIV, LV, N                                                
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION D(N), FX, X(N), V(LV)                            
C     DIMENSION V(77 + N*(N+17)/2)                                      
C                                                                       
C  ***  PURPOSE  ***                                                    
C                                                                       
C        THIS ROUTINE INTERACTS WITH SUBROUTINE  DRMNG  IN AN ATTEMPT   
C     TO FIND AN N-VECTOR  X*  THAT MINIMIZES THE (UNCONSTRAINED)       
C     OBJECTIVE FUNCTION  FX = F(X)  COMPUTED BY THE CALLER.  (OFTEN    
C     THE  X*  FOUND IS A LOCAL MINIMIZER RATHER THAN A GLOBAL ONE.)    
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
C        THE PARAMETERS FOR DRMNF ARE THE SAME AS THOSE FOR  DMNG       
C     (WHICH SEE), EXCEPT THAT CALCF, CALCG, UIPARM, URPARM, AND UFPARM 
C     ARE OMITTED, AND A PARAMETER  FX  FOR THE OBJECTIVE FUNCTION      
C     VALUE AT X IS ADDED.  INSTEAD OF CALLING CALCG TO OBTAIN THE      
C     GRADIENT OF THE OBJECTIVE FUNCTION AT X, DRMNF CALLS DS7GRD,      
C     WHICH COMPUTES AN APPROXIMATION TO THE GRADIENT BY FINITE         
C     (FORWARD AND CENTRAL) DIFFERENCES USING THE METHOD OF REF. 1.     
C     THE FOLLOWING INPUT COMPONENT IS OF INTEREST IN THIS REGARD       
C     (AND IS NOT DESCRIBED IN  DMNG).                                  
C                                                                       
C V(ETA0)..... V(42) IS AN ESTIMATED BOUND ON THE RELATIVE ERROR IN THE 
C             OBJECTIVE FUNCTION VALUE COMPUTED BY CALCF...             
C                  (TRUE VALUE) = (COMPUTED VALUE) * (1 + E),           
C             WHERE ABS(E) .LE. V(ETA0).  DEFAULT = MACHEP * 10**3,     
C             WHERE MACHEP IS THE UNIT ROUNDOFF.                        
C                                                                       
C        THE OUTPUT VALUES IV(NFCALL) AND IV(NGCALL) HAVE DIFFERENT     
C     MEANINGS FOR  DMNF THAN FOR  DMNG...                              
C                                                                       
C IV(NFCALL)... IV(6) IS THE NUMBER OF CALLS SO FAR MADE ON CALCF (I.E.,
C             FUNCTION EVALUATIONS) EXCLUDING THOSE MADE ONLY FOR       
C             COMPUTING GRADIENTS.  THE INPUT VALUE IV(MXFCAL) IS A     
C             LIMIT ON IV(NFCALL).                                      
C IV(NGCALL)... IV(30) IS THE NUMBER OF FUNCTION EVALUATIONS MADE ONLY  
C             FOR COMPUTING GRADIENTS.  THE TOTAL NUMBER OF FUNCTION    
C             EVALUATIONS IS THUS  IV(NFCALL) + IV(NGCALL).             
C                                                                       
C  ***  REFERENCES  ***                                                 
C                                                                       
C 1. STEWART, G.W. (1967), A MODIFICATION OF DAVIDON*S MINIMIZATION     
C        METHOD TO ACCEPT DIFFERENCE APPROXIMATIONS OF DERIVATIVES,     
C        J. ASSOC. COMPUT. MACH. 14, PP. 72-83.                         
C.                                                                      
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY (AUGUST 1982).                              
C                                                                       
C----------------------------  DECLARATIONS  ---------------------------
C                                                                       
      DOUBLE PRECISION DD7TPR                                           
      EXTERNAL DIVSET, DD7TPR, DS7GRD, DRMNG, DV7SCP                    
C                                                                       
C DIVSET.... SUPPLIES DEFAULT PARAMETER VALUES.                         
C DD7TPR... RETURNS INNER PRODUCT OF TWO VECTORS.                       
C DS7GRD... COMPUTES FINITE-DIFFERENCE GRADIENT APPROXIMATION.          
C DRMNG.... REVERSE-COMMUNICATION ROUTINE THAT DOES  DMNG ALGORITHM.    
C DV7SCP... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.                  
C                                                                       
      INTEGER ALPHA, G1, I, IV1, J, K, W                                
      DOUBLE PRECISION ZERO                                             
C                                                                       
C  ***  SUBSCRIPTS FOR IV   ***                                         
C                                                                       
      INTEGER ETA0, F, G, LMAT, NEXTV, NGCALL, NITER, SGIRC, TOOBIG,    
     1        VNEED                                                     
C                                                                       
C/6                                                                     
C     DATA ETA0/42/, F/10/, G/28/, LMAT/42/, NEXTV/47/, NGCALL/30/,     
C    1     NITER/31/, SGIRC/57/, TOOBIG/2/, VNEED/4/                    
C/7                                                                     
      PARAMETER (ETA0=42, F=10, G=28, LMAT=42, NEXTV=47, NGCALL=30,     
     1           NITER=31, SGIRC=57, TOOBIG=2, VNEED=4)                 
C/                                                                      
C/6                                                                     
C     DATA ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.D+0)                                            
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 1) GO TO 10                                          
      IF (IV1 .EQ. 2) GO TO 50                                          
      IF (IV(1) .EQ. 0) CALL DIVSET(2, IV, LIV, LV, V)                  
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 12 .OR. IV1 .EQ. 13) IV(VNEED) = IV(VNEED) + 2*N + 6 
      IF (IV1 .EQ. 14) GO TO 10                                         
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10                        
      G1 = 1                                                            
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      GO TO 20                                                          
C                                                                       
 10   G1 = IV(G)                                                        
C                                                                       
 20   CALL DRMNG(D, FX, V(G1), IV, LIV, LV, N, V, X)                    
      IF (IV(1) - 2) 999, 30, 70                                        
C                                                                       
C  ***  COMPUTE GRADIENT  ***                                           
C                                                                       
 30   IF (IV(NITER) .EQ. 0) CALL DV7SCP(N, V(G1), ZERO)                 
      J = IV(LMAT)                                                      
      K = G1 - N                                                        
      DO 40 I = 1, N                                                    
         V(K) = DD7TPR(I, V(J), V(J))                                   
         K = K + 1                                                      
         J = J + I                                                      
 40      CONTINUE                                                       
C     ***  UNDO INCREMENT OF IV(NGCALL) DONE BY DRMNG  ***              
      IV(NGCALL) = IV(NGCALL) - 1                                       
C     ***  STORE RETURN CODE FROM DS7GRD IN IV(SGIRC)  ***              
      IV(SGIRC) = 0                                                     
C     ***  X MAY HAVE BEEN RESTORED, SO COPY BACK FX... ***             
      FX = V(F)                                                         
      GO TO 60                                                          
C                                                                       
C     ***  GRADIENT LOOP  ***                                           
C                                                                       
 50   IF (IV(TOOBIG) .NE. 0) GO TO 10                                   
C                                                                       
 60   G1 = IV(G)                                                        
      ALPHA = G1 - N                                                    
      W = ALPHA - 6                                                     
      CALL DS7GRD(V(ALPHA), D, V(ETA0), FX, V(G1), IV(SGIRC), N, V(W),X)
      IF (IV(SGIRC) .EQ. 0) GO TO 10                                    
         IV(NGCALL) = IV(NGCALL) + 1                                    
         GO TO 999                                                      
C                                                                       
 70   IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(G) = IV(NEXTV) + N + 6                                         
      IV(NEXTV) = IV(G) + N                                             
      IF (IV1 .NE. 13) GO TO 10                                         
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DRMNF FOLLOWS  ***                                 
      END                                                               
      SUBROUTINE DRMNFB(B, D, FX, IV, LIV, LV, P, V, X)                 
C                                                                       
C  ***  ITERATION DRIVER FOR  DMNF...                                   
C  ***  MINIMIZE GENERAL UNCONSTRAINED OBJECTIVE FUNCTION USING         
C  ***  FINITE-DIFFERENCE GRADIENTS AND SECANT HESSIAN APPROXIMATIONS.  
C                                                                       
      INTEGER LIV, LV, P                                                
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION B(2,P), D(P), FX, X(P), V(LV)                    
C     DIMENSION V(77 + P*(P+17)/2)                                      
C                                                                       
C  ***  PURPOSE  ***                                                    
C                                                                       
C        THIS ROUTINE INTERACTS WITH SUBROUTINE  DRMNGB  IN AN ATTEMPT  
C     TO FIND AN P-VECTOR  X*  THAT MINIMIZES THE (UNCONSTRAINED)       
C     OBJECTIVE FUNCTION  FX = F(X)  COMPUTED BY THE CALLER.  (OFTEN    
C     THE  X*  FOUND IS A LOCAL MINIMIZER RATHER THAN A GLOBAL ONE.)    
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
C        THE PARAMETERS FOR DRMNFB ARE THE SAME AS THOSE FOR  DMNG      
C     (WHICH SEE), EXCEPT THAT CALCF, CALCG, UIPARM, URPARM, AND UFPARM 
C     ARE OMITTED, AND A PARAMETER  FX  FOR THE OBJECTIVE FUNCTION      
C     VALUE AT X IS ADDED.  INSTEAD OF CALLING CALCG TO OBTAIN THE      
C     GRADIENT OF THE OBJECTIVE FUNCTION AT X, DRMNFB CALLS DS3GRD,     
C     WHICH COMPUTES AN APPROXIMATION TO THE GRADIENT BY FINITE         
C     (FORWARD AND CENTRAL) DIFFERENCES USING THE METHOD OF REF. 1.     
C     THE FOLLOWING INPUT COMPONENT IS OF INTEREST IN THIS REGARD       
C     (AND IS NOT DESCRIBED IN  DMNG).                                  
C                                                                       
C V(ETA0)..... V(42) IS AN ESTIMATED BOUND ON THE RELATIVE ERROR IN THE 
C             OBJECTIVE FUNCTION VALUE COMPUTED BY CALCF...             
C                  (TRUE VALUE) = (COMPUTED VALUE) * (1 + E),           
C             WHERE ABS(E) .LE. V(ETA0).  DEFAULT = MACHEP * 10**3,     
C             WHERE MACHEP IS THE UNIT ROUNDOFF.                        
C                                                                       
C        THE OUTPUT VALUES IV(NFCALL) AND IV(NGCALL) HAVE DIFFERENT     
C     MEANINGS FOR  DMNF THAN FOR  DMNG...                              
C                                                                       
C IV(NFCALL)... IV(6) IS THE NUMBER OF CALLS SO FAR MADE ON CALCF (I.E.,
C             FUNCTION EVALUATIONS) EXCLUDING THOSE MADE ONLY FOR       
C             COMPUTING GRADIENTS.  THE INPUT VALUE IV(MXFCAL) IS A     
C             LIMIT ON IV(NFCALL).                                      
C IV(NGCALL)... IV(30) IS THE NUMBER OF FUNCTION EVALUATIONS MADE ONLY  
C             FOR COMPUTING GRADIENTS.  THE TOTAL NUMBER OF FUNCTION    
C             EVALUATIONS IS THUS  IV(NFCALL) + IV(NGCALL).             
C                                                                       
C  ***  REFERENCES  ***                                                 
C                                                                       
C 1. STEWART, G.W. (1967), A MODIFICATION OF DAVIDON*S MINIMIZATION     
C        METHOD TO ACCEPT DIFFERENCE APPROXIMATIONS OF DERIVATIVES,     
C        J. ASSOC. COMPUT. MACH. 14, PP. 72-83.                         
C.                                                                      
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY (AUGUST 1982).                              
C                                                                       
C----------------------------  DECLARATIONS  ---------------------------
C                                                                       
      DOUBLE PRECISION DD7TPR                                           
      EXTERNAL DIVSET, DD7TPR, DS3GRD, DRMNGB, DV7SCP                   
C                                                                       
C DIVSET.... SUPPLIES DEFAULT PARAMETER VALUES.                         
C DD7TPR... RETURNS INNER PRODUCT OF TWO VECTORS.                       
C DS3GRD... COMPUTES FINITE-DIFFERENCE GRADIENT APPROXIMATION.          
C DRMNGB... REVERSE-COMMUNICATION ROUTINE THAT DOES  DMNGB ALGORITHM.   
C DV7SCP... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.                  
C                                                                       
      INTEGER ALPHA, ALPHA0, G1, I, IPI, IV1, J, K, W                   
      DOUBLE PRECISION ZERO                                             
C                                                                       
C  ***  SUBSCRIPTS FOR IV   ***                                         
C                                                                       
      INTEGER ETA0, F, G, LMAT, NEXTV, NGCALL,                          
     1        NITER, PERM, SGIRC, TOOBIG, VNEED                         
C                                                                       
C/6                                                                     
C     DATA ETA0/42/, F/10/, G/28/, LMAT/42/, NEXTV/47/, NGCALL/30/,     
C    1     NITER/31/, PERM/58/, SGIRC/57/, TOOBIG/2/, VNEED/4/          
C/7                                                                     
      PARAMETER (ETA0=42, F=10, G=28, LMAT=42, NEXTV=47, NGCALL=30,     
     1           NITER=31, PERM=58, SGIRC=57, TOOBIG=2, VNEED=4)        
C/                                                                      
C/6                                                                     
C     DATA ZERO/0.D+0/                                                  
C/7                                                                     
      PARAMETER (ZERO=0.D+0)                                            
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 1) GO TO 10                                          
      IF (IV1 .EQ. 2) GO TO 50                                          
      IF (IV(1) .EQ. 0) CALL DIVSET(2, IV, LIV, LV, V)                  
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 12 .OR. IV1 .EQ. 13) IV(VNEED) = IV(VNEED) + 2*P + 6 
      IF (IV1 .EQ. 14) GO TO 10                                         
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10                        
      G1 = 1                                                            
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      GO TO 20                                                          
C                                                                       
 10   G1 = IV(G)                                                        
C                                                                       
 20   CALL DRMNGB(B, D, FX, V(G1), IV, LIV, LV, P, V, X)                
      IF (IV(1) - 2) 999, 30, 80                                        
C                                                                       
C  ***  COMPUTE GRADIENT  ***                                           
C                                                                       
 30   IF (IV(NITER) .EQ. 0) CALL DV7SCP(P, V(G1), ZERO)                 
      J = IV(LMAT)                                                      
      ALPHA0 = G1 - P - 1                                               
      IPI = IV(PERM)                                                    
      DO 40 I = 1, P                                                    
         K = ALPHA0 + IV(IPI)                                           
         V(K) = DD7TPR(I, V(J), V(J))                                   
         IPI = IPI + 1                                                  
         J = J + I                                                      
 40      CONTINUE                                                       
C     ***  UNDO INCREMENT OF IV(NGCALL) DONE BY DRMNGB  ***             
      IV(NGCALL) = IV(NGCALL) - 1                                       
C     ***  STORE RETURN CODE FROM DS3GRD IN IV(SGIRC)  ***              
      IV(SGIRC) = 0                                                     
C     ***  X MAY HAVE BEEN RESTORED, SO COPY BACK FX... ***             
      FX = V(F)                                                         
      GO TO 60                                                          
C                                                                       
C     ***  GRADIENT LOOP  ***                                           
C                                                                       
 50   IF (IV(TOOBIG) .NE. 0) GO TO 10                                   
C                                                                       
 60   G1 = IV(G)                                                        
      ALPHA = G1 - P                                                    
      W = ALPHA - 6                                                     
      CALL DS3GRD(V(ALPHA), B, D, V(ETA0), FX, V(G1), IV(SGIRC), P,     
     1            V(W), X)                                              
      I = IV(SGIRC)                                                     
      IF (I .EQ. 0) GO TO 10                                            
      IF (I .LE. P) GO TO 70                                            
         IV(TOOBIG) = 1                                                 
         GO TO 10                                                       
C                                                                       
 70   IV(NGCALL) = IV(NGCALL) + 1                                       
      GO TO 999                                                         
C                                                                       
 80   IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(G) = IV(NEXTV) + P + 6                                         
      IV(NEXTV) = IV(G) + P                                             
      IF (IV1 .NE. 13) GO TO 10                                         
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DRMNFB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DRMNG(D, FX, G, IV, LIV, LV, N, V, X)                  
C                                                                       
C  ***  CARRY OUT  DMNG (UNCONSTRAINED MINIMIZATION) ITERATIONS, USING  
C  ***  DOUBLE-DOGLEG/BFGS STEPS.                                       
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER LIV, LV, N                                                
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION D(N), FX, G(N), V(LV), X(N)                      
C                                                                       
C--------------------------  PARAMETER USAGE  --------------------------
C                                                                       
C D.... SCALE VECTOR.                                                   
C FX... FUNCTION VALUE.                                                 
C G.... GRADIENT VECTOR.                                                
C IV... INTEGER VALUE ARRAY.                                            
C LIV.. LENGTH OF IV (AT LEAST 60).                                     
C LV... LENGTH OF V (AT LEAST 71 + N*(N+13)/2).                         
C N.... NUMBER OF VARIABLES (COMPONENTS IN X AND G).                    
C V.... FLOATING-POINT VALUE ARRAY.                                     
C X.... VECTOR OF PARAMETERS TO BE OPTIMIZED.                           
C                                                                       
C  ***  DISCUSSION  ***                                                 
C                                                                       
C        PARAMETERS IV, N, V, AND X ARE THE SAME AS THE CORRESPONDING   
C     ONES TO  DMNG (WHICH SEE), EXCEPT THAT V CAN BE SHORTER (SINCE    
C     THE PART OF V THAT  DMNG USES FOR STORING G IS NOT NEEDED).       
C     MOREOVER, COMPARED WITH  DMNG, IV(1) MAY HAVE THE TWO ADDITIONAL  
C     OUTPUT VALUES 1 AND 2, WHICH ARE EXPLAINED BELOW, AS IS THE USE   
C     OF IV(TOOBIG) AND IV(NFGCAL).  THE VALUE IV(G), WHICH IS AN       
C     OUTPUT VALUE FROM  DMNG (AND  DMNF), IS NOT REFERENCED BY         
C     DRMNG OR THE SUBROUTINES IT CALLS.                                
C        FX AND G NEED NOT HAVE BEEN INITIALIZED WHEN DRMNG IS CALLED   
C     WITH IV(1) = 12, 13, OR 14.                                       
C                                                                       
C IV(1) = 1 MEANS THE CALLER SHOULD SET FX TO F(X), THE FUNCTION VALUE  
C             AT X, AND CALL DRMNG AGAIN, HAVING CHANGED NONE OF THE    
C             OTHER PARAMETERS.  AN EXCEPTION OCCURS IF F(X) CANNOT BE  
C             (E.G. IF OVERFLOW WOULD OCCUR), WHICH MAY HAPPEN BECAUSE  
C             OF AN OVERSIZED STEP.  IN THIS CASE THE CALLER SHOULD SET 
C             IV(TOOBIG) = IV(2) TO 1, WHICH WILL CAUSE DRMNG TO IG-    
C             NORE FX AND TRY A SMALLER STEP.  THE PARAMETER NF THAT    
C              DMNG PASSES TO CALCF (FOR POSSIBLE USE BY CALCG) IS A    
C             COPY OF IV(NFCALL) = IV(6).                               
C IV(1) = 2 MEANS THE CALLER SHOULD SET G TO G(X), THE GRADIENT VECTOR  
C             OF F AT X, AND CALL DRMNG AGAIN, HAVING CHANGED NONE OF   
C             THE OTHER PARAMETERS EXCEPT POSSIBLY THE SCALE VECTOR D   
C             WHEN IV(DTYPE) = 0.  THE PARAMETER NF THAT  DMNG PASSES   
C             TO CALCG IS IV(NFGCAL) = IV(7).  IF G(X) CANNOT BE        
C             EVALUATED, THEN THE CALLER MAY SET IV(TOOBIG) TO 0, IN    
C             WHICH CASE DRMNG WILL RETURN WITH IV(1) = 65.             
C.                                                                      
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY (DECEMBER 1979).  REVISED SEPT. 1982.       
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH SUPPORTED 
C     IN PART BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS           
C     MCS-7600324 AND MCS-7906671.                                      
C                                                                       
C        (SEE  DMNG FOR REFERENCES.)                                    
C                                                                       
C+++++++++++++++++++++++++++  DECLARATIONS  ++++++++++++++++++++++++++++
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER DG1, DUMMY, G01, I, K, L, LSTGST, NWTST1, RSTRST, STEP1,  
     1        TEMP1, W, X01, Z                                          
      DOUBLE PRECISION T                                                
C                                                                       
C     ***  CONSTANTS  ***                                               
C                                                                       
      DOUBLE PRECISION HALF, NEGONE, ONE, ONEP2, ZERO                   
C                                                                       
C  ***  NO INTRINSIC FUNCTIONS  ***                                     
C                                                                       
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      LOGICAL STOPX                                                     
      DOUBLE PRECISION DD7TPR, DRLDST, DV2NRM                           
      EXTERNAL DA7SST,DD7DOG,DIVSET, DD7TPR,DITSUM, DL7ITV, DL7IVM,     
     1         DL7TVM, DL7UPD,DL7VML,DPARCK, DRLDST, STOPX,DV2AXY,      
     2        DV7CPY, DV7SCP, DV7VMP, DV2NRM, DW7ZBF                    
C                                                                       
C DA7SST.... ASSESSES CANDIDATE STEP.                                   
C DD7DOG.... COMPUTES DOUBLE-DOGLEG (CANDIDATE) STEP.                   
C DIVSET.... SUPPLIES DEFAULT IV AND V INPUT COMPONENTS.                
C DD7TPR... RETURNS INNER PRODUCT OF TWO VECTORS.                       
C DITSUM.... PRINTS ITERATION SUMMARY AND INFO ON INITIAL AND FINAL X.  
C DL7ITV... MULTIPLIES INVERSE TRANSPOSE OF LOWER TRIANGLE TIMES VECTOR.
C DL7IVM... MULTIPLIES INVERSE OF LOWER TRIANGLE TIMES VECTOR.          
C DL7TVM... MULTIPLIES TRANSPOSE OF LOWER TRIANGLE TIMES VECTOR.        
C LUPDT.... UPDATES CHOLESKY FACTOR OF HESSIAN APPROXIMATION.           
C DL7VML.... MULTIPLIES LOWER TRIANGLE TIMES VECTOR.                    
C DPARCK.... CHECKS VALIDITY OF INPUT IV AND V VALUES.                  
C DRLDST... COMPUTES V(RELDX) = RELATIVE STEP SIZE.                     
C STOPX.... RETURNS .TRUE. IF THE BREAK KEY HAS BEEN PRESSED.           
C DV2AXY.... COMPUTES SCALAR TIMES ONE VECTOR PLUS ANOTHER.             
C DV7CPY.... COPIES ONE VECTOR TO ANOTHER.                              
C DV7SCP... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.                  
C DV7VMP... MULTIPLIES VECTOR BY VECTOR RAISED TO POWER (COMPONENTWISE).
C DV2NRM... RETURNS THE 2-NORM OF A VECTOR.                             
C DW7ZBF... COMPUTES W AND Z FOR DL7UPD CORRESPONDING TO BFGS UPDATE.   
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER CNVCOD, DG, DGNORM, DINIT, DSTNRM, DST0, F, F0, FDIF,     
     1        GTHG, GTSTEP, G0, INCFAC, INITH, IRC, KAGQT, LMAT, LMAX0, 
     2        LMAXS, MODE, MODEL, MXFCAL, MXITER, NEXTV, NFCALL, NFGCAL,
     3        NGCALL, NITER, NREDUC, NWTSTP, PREDUC, RADFAC, RADINC,    
     4        RADIUS, RAD0, RELDX, RESTOR, STEP, STGLIM, STLSTG, TOOBIG,
     5        TUNER4, TUNER5, VNEED, XIRC, X0                           
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C/6                                                                     
C     DATA CNVCOD/55/, DG/37/, G0/48/, INITH/25/, IRC/29/, KAGQT/33/,   
C    1     MODE/35/, MODEL/5/, MXFCAL/17/, MXITER/18/, NFCALL/6/,       
C    2     NFGCAL/7/, NGCALL/30/, NITER/31/, NWTSTP/34/, RADINC/8/,     
C    3     RESTOR/9/, STEP/40/, STGLIM/11/, STLSTG/41/, TOOBIG/2/,      
C    4     VNEED/4/, XIRC/13/, X0/43/                                   
C/7                                                                     
      PARAMETER (CNVCOD=55, DG=37, G0=48, INITH=25, IRC=29, KAGQT=33,   
     1           MODE=35, MODEL=5, MXFCAL=17, MXITER=18, NFCALL=6,      
     2           NFGCAL=7, NGCALL=30, NITER=31, NWTSTP=34, RADINC=8,    
     3           RESTOR=9, STEP=40, STGLIM=11, STLSTG=41, TOOBIG=2,     
     4           VNEED=4, XIRC=13, X0=43)                               
C/                                                                      
C                                                                       
C  ***  V SUBSCRIPT VALUES  ***                                         
C                                                                       
C/6                                                                     
C     DATA DGNORM/1/, DINIT/38/, DSTNRM/2/, DST0/3/, F/10/, F0/13/,     
C    1     FDIF/11/, GTHG/44/, GTSTEP/4/, INCFAC/23/, LMAT/42/,         
C    2     LMAX0/35/, LMAXS/36/, NEXTV/47/, NREDUC/6/, PREDUC/7/,       
C    3     RADFAC/16/, RADIUS/8/, RAD0/9/, RELDX/17/, TUNER4/29/,       
C    4     TUNER5/30/                                                   
C/7                                                                     
      PARAMETER (DGNORM=1, DINIT=38, DSTNRM=2, DST0=3, F=10, F0=13,     
     1           FDIF=11, GTHG=44, GTSTEP=4, INCFAC=23, LMAT=42,        
     2           LMAX0=35, LMAXS=36, NEXTV=47, NREDUC=6, PREDUC=7,      
     3           RADFAC=16, RADIUS=8, RAD0=9, RELDX=17, TUNER4=29,      
     4           TUNER5=30)                                             
C/                                                                      
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
C  ***  CHECK VALIDITY OF IV AND V INPUT VALUES  ***                    
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(2, IV, LIV, LV, V)                  
      IF (IV(1) .EQ. 12 .OR. IV(1) .EQ. 13)                             
     1     IV(VNEED) = IV(VNEED) + N*(N+13)/2                           
      CALL DPARCK(2, D, IV, LIV, LV, N, V)                              
      I = IV(1) - 2                                                     
      IF (I .GT. 12) GO TO 999                                          
      GO TO (190, 190, 190, 190, 190, 190, 120, 90, 120, 10, 10, 20), I 
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
 10   L = IV(LMAT)                                                      
      IV(X0) = L + N*(N+1)/2                                            
      IV(STEP) = IV(X0) + N                                             
      IV(STLSTG) = IV(STEP) + N                                         
      IV(G0) = IV(STLSTG) + N                                           
      IV(NWTSTP) = IV(G0) + N                                           
      IV(DG) = IV(NWTSTP) + N                                           
      IV(NEXTV) = IV(DG) + N                                            
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
      IV(MODEL) = 1                                                     
      IV(STGLIM) = 1                                                    
      IV(TOOBIG) = 0                                                    
      IV(CNVCOD) = 0                                                    
      IV(RADINC) = 0                                                    
      V(RAD0) = ZERO                                                    
      IF (V(DINIT) .GE. ZERO) CALL DV7SCP(N, D, V(DINIT))               
      IF (IV(INITH) .NE. 1) GO TO 40                                    
C                                                                       
C     ***  SET THE INITIAL HESSIAN APPROXIMATION TO DIAG(D)**-2  ***    
C                                                                       
         L = IV(LMAT)                                                   
         CALL DV7SCP(N*(N+1)/2, V(L), ZERO)                             
         K = L - 1                                                      
         DO 30 I = 1, N                                                 
              K = K + I                                                 
              T = D(I)                                                  
              IF (T .LE. ZERO) T = ONE                                  
              V(K) = T                                                  
 30           CONTINUE                                                  
C                                                                       
C  ***  COMPUTE INITIAL FUNCTION VALUE  ***                             
C                                                                       
 40   IV(1) = 1                                                         
      GO TO 999                                                         
C                                                                       
 50   V(F) = FX                                                         
      IF (IV(MODE) .GE. 0) GO TO 190                                    
      IV(1) = 2                                                         
      IF (IV(TOOBIG) .EQ. 0) GO TO 999                                  
         IV(1) = 63                                                     
         GO TO 350                                                      
C                                                                       
C  ***  MAKE SURE GRADIENT COULD BE COMPUTED  ***                       
C                                                                       
 60   IF (IV(TOOBIG) .EQ. 0) GO TO 70                                   
         IV(1) = 65                                                     
         GO TO 350                                                      
C                                                                       
 70   DG1 = IV(DG)                                                      
      CALL DV7VMP(N, V(DG1), G, D, -1)                                  
      V(DGNORM) = DV2NRM(N, V(DG1))                                     
C                                                                       
      IF (IV(CNVCOD) .NE. 0) GO TO 340                                  
      IF (IV(MODE) .EQ. 0) GO TO 300                                    
C                                                                       
C  ***  ALLOW FIRST STEP TO HAVE SCALED 2-NORM AT MOST V(LMAX0)  ***    
C                                                                       
      V(RADIUS) = V(LMAX0)                                              
C                                                                       
      IV(MODE) = 0                                                      
C                                                                       
C                                                                       
C-----------------------------  MAIN LOOP  -----------------------------
C                                                                       
C                                                                       
C  ***  PRINT ITERATION SUMMARY, CHECK ITERATION LIMIT  ***             
C                                                                       
 80   CALL DITSUM(D, G, IV, LIV, LV, N, V, X)                           
 90   K = IV(NITER)                                                     
      IF (K .LT. IV(MXITER)) GO TO 100                                  
         IV(1) = 10                                                     
         GO TO 350                                                      
C                                                                       
C  ***  UPDATE RADIUS  ***                                              
C                                                                       
 100  IV(NITER) = K + 1                                                 
      IF (K .GT. 0) V(RADIUS) = V(RADFAC) * V(DSTNRM)                   
C                                                                       
C  ***  INITIALIZE FOR START OF NEXT ITERATION  ***                     
C                                                                       
      G01 = IV(G0)                                                      
      X01 = IV(X0)                                                      
      V(F0) = V(F)                                                      
      IV(IRC) = 4                                                       
      IV(KAGQT) = -1                                                    
C                                                                       
C     ***  COPY X TO X0, G TO G0  ***                                   
C                                                                       
      CALL DV7CPY(N, V(X01), X)                                         
      CALL DV7CPY(N, V(G01), G)                                         
C                                                                       
C  ***  CHECK STOPX AND FUNCTION EVALUATION LIMIT  ***                  
C                                                                       
 110  IF (.NOT. STOPX(DUMMY)) GO TO 130                                 
         IV(1) = 11                                                     
         GO TO 140                                                      
C                                                                       
C     ***  COME HERE WHEN RESTARTING AFTER FUNC. EVAL. LIMIT OR STOPX.  
C                                                                       
 120  IF (V(F) .GE. V(F0)) GO TO 130                                    
         V(RADFAC) = ONE                                                
         K = IV(NITER)                                                  
         GO TO 100                                                      
C                                                                       
 130  IF (IV(NFCALL) .LT. IV(MXFCAL)) GO TO 150                         
         IV(1) = 9                                                      
 140     IF (V(F) .GE. V(F0)) GO TO 350                                 
C                                                                       
C        ***  IN CASE OF STOPX OR FUNCTION EVALUATION LIMIT WITH        
C        ***  IMPROVED V(F), EVALUATE THE GRADIENT AT X.                
C                                                                       
              IV(CNVCOD) = IV(1)                                        
              GO TO 290                                                 
C                                                                       
C. . . . . . . . . . . . .  COMPUTE CANDIDATE STEP  . . . . . . . . . . 
C                                                                       
 150  STEP1 = IV(STEP)                                                  
      DG1 = IV(DG)                                                      
      NWTST1 = IV(NWTSTP)                                               
      IF (IV(KAGQT) .GE. 0) GO TO 160                                   
         L = IV(LMAT)                                                   
         CALL DL7IVM(N, V(NWTST1), V(L), G)                             
         V(NREDUC) = HALF * DD7TPR(N, V(NWTST1), V(NWTST1))             
         CALL DL7ITV(N, V(NWTST1), V(L), V(NWTST1))                     
         CALL DV7VMP(N, V(STEP1), V(NWTST1), D, 1)                      
         V(DST0) = DV2NRM(N, V(STEP1))                                  
         CALL DV7VMP(N, V(DG1), V(DG1), D, -1)                          
         CALL DL7TVM(N, V(STEP1), V(L), V(DG1))                         
         V(GTHG) = DV2NRM(N, V(STEP1))                                  
         IV(KAGQT) = 0                                                  
 160  CALL DD7DOG(V(DG1), LV, N, V(NWTST1), V(STEP1), V)                
      IF (IV(IRC) .NE. 6) GO TO 170                                     
         IF (IV(RESTOR) .NE. 2) GO TO 160                               
         RSTRST = 2                                                     
         GO TO 200                                                      
C                                                                       
C  ***  CHECK WHETHER EVALUATING F(X0 + STEP) LOOKS WORTHWHILE  ***     
C                                                                       
 170  IV(TOOBIG) = 0                                                    
      IF (V(DSTNRM) .LE. ZERO) GO TO 190                                
      IF (IV(IRC) .NE. 5) GO TO 180                                     
      IF (V(RADFAC) .LE. ONE) GO TO 180                                 
      IF (V(PREDUC) .GT. ONEP2 * V(FDIF)) GO TO 180                     
         IF (IV(RESTOR) .NE. 2) GO TO 190                               
         RSTRST = 0                                                     
         GO TO 200                                                      
C                                                                       
C  ***  COMPUTE F(X0 + STEP)  ***                                       
C                                                                       
 180  X01 = IV(X0)                                                      
      STEP1 = IV(STEP)                                                  
      CALL DV2AXY(N, X, ONE, V(STEP1), V(X01))                          
      IV(NFCALL) = IV(NFCALL) + 1                                       
      IV(1) = 1                                                         
      GO TO 999                                                         
C                                                                       
C. . . . . . . . . . . . .  ASSESS CANDIDATE STEP  . . . . . . . . . . .
C                                                                       
 190  RSTRST = 3                                                        
 200  X01 = IV(X0)                                                      
      V(RELDX) = DRLDST(N, D, X, V(X01))                                
      CALL DA7SST(IV, LIV, LV, V)                                       
      STEP1 = IV(STEP)                                                  
      LSTGST = IV(STLSTG)                                               
      I = IV(RESTOR) + 1                                                
      GO TO (240, 210, 220, 230), I                                     
 210  CALL DV7CPY(N, X, V(X01))                                         
      GO TO 240                                                         
 220   CALL DV7CPY(N, V(LSTGST), V(STEP1))                              
       GO TO 240                                                        
 230     CALL DV7CPY(N, V(STEP1), V(LSTGST))                            
         CALL DV2AXY(N, X, ONE, V(STEP1), V(X01))                       
         V(RELDX) = DRLDST(N, D, X, V(X01))                             
         IV(RESTOR) = RSTRST                                            
C                                                                       
 240  K = IV(IRC)                                                       
      GO TO (250,280,280,280,250,260,270,270,270,270,270,270,330,300), K
C                                                                       
C     ***  RECOMPUTE STEP WITH CHANGED RADIUS  ***                      
C                                                                       
 250     V(RADIUS) = V(RADFAC) * V(DSTNRM)                              
         GO TO 110                                                      
C                                                                       
C  ***  COMPUTE STEP OF LENGTH V(LMAXS) FOR SINGULAR CONVERGENCE TEST.  
C                                                                       
 260  V(RADIUS) = V(LMAXS)                                              
      GO TO 150                                                         
C                                                                       
C  ***  CONVERGENCE OR FALSE CONVERGENCE  ***                           
C                                                                       
 270  IV(CNVCOD) = K - 4                                                
      IF (V(F) .GE. V(F0)) GO TO 340                                    
         IF (IV(XIRC) .EQ. 14) GO TO 340                                
              IV(XIRC) = 14                                             
C                                                                       
C. . . . . . . . . . . .  PROCESS ACCEPTABLE STEP  . . . . . . . . . . .
C                                                                       
 280  IF (IV(IRC) .NE. 3) GO TO 290                                     
         STEP1 = IV(STEP)                                               
         TEMP1 = IV(STLSTG)                                             
C                                                                       
C     ***  SET  TEMP1 = HESSIAN * STEP  FOR USE IN GRADIENT TESTS  ***  
C                                                                       
         L = IV(LMAT)                                                   
         CALL DL7TVM(N, V(TEMP1), V(L), V(STEP1))                       
         CALL DL7VML(N, V(TEMP1), V(L), V(TEMP1))                       
C                                                                       
C  ***  COMPUTE GRADIENT  ***                                           
C                                                                       
 290  IV(NGCALL) = IV(NGCALL) + 1                                       
      IV(1) = 2                                                         
      GO TO 999                                                         
C                                                                       
C  ***  INITIALIZATIONS -- G0 = G - G0, ETC.  ***                       
C                                                                       
 300  G01 = IV(G0)                                                      
      CALL DV2AXY(N, V(G01), NEGONE, V(G01), G)                         
      STEP1 = IV(STEP)                                                  
      TEMP1 = IV(STLSTG)                                                
      IF (IV(IRC) .NE. 3) GO TO 320                                     
C                                                                       
C  ***  SET V(RADFAC) BY GRADIENT TESTS  ***                            
C                                                                       
C     ***  SET  TEMP1 = DIAG(D)**-1 * (HESSIAN*STEP + (G(X0)-G(X)))  ***
C                                                                       
         CALL DV2AXY(N, V(TEMP1), NEGONE, V(G01), V(TEMP1))             
         CALL DV7VMP(N, V(TEMP1), V(TEMP1), D, -1)                      
C                                                                       
C        ***  DO GRADIENT TESTS  ***                                    
C                                                                       
         IF (DV2NRM(N, V(TEMP1)) .LE. V(DGNORM) * V(TUNER4))            
     1                  GO TO 310                                       
              IF (DD7TPR(N, G, V(STEP1))                                
     1                  .GE. V(GTSTEP) * V(TUNER5))  GO TO 320          
 310               V(RADFAC) = V(INCFAC)                                
C                                                                       
C  ***  UPDATE H, LOOP  ***                                             
C                                                                       
 320  W = IV(NWTSTP)                                                    
      Z = IV(X0)                                                        
      L = IV(LMAT)                                                      
      CALL DW7ZBF(V(L), N, V(STEP1), V(W), V(G01), V(Z))                
C                                                                       
C     ** USE THE N-VECTORS STARTING AT V(STEP1) AND V(G01) FOR SCRATCH..
      CALL DL7UPD(V(TEMP1), V(STEP1), V(L), V(G01), V(L), N, V(W), V(Z))
      IV(1) = 2                                                         
      GO TO 80                                                          
C                                                                       
C. . . . . . . . . . . . . .  MISC. DETAILS  . . . . . . . . . . . . . .
C                                                                       
C  ***  BAD PARAMETERS TO ASSESS  ***                                   
C                                                                       
 330  IV(1) = 64                                                        
      GO TO 350                                                         
C                                                                       
C  ***  PRINT SUMMARY OF FINAL ITERATION AND OTHER REQUESTED ITEMS  *** 
C                                                                       
 340  IV(1) = IV(CNVCOD)                                                
      IV(CNVCOD) = 0                                                    
 350  CALL DITSUM(D, G, IV, LIV, LV, N, V, X)                           
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST LINE OF DRMNG FOLLOWS  ***                                 
      END                                                               
      SUBROUTINE DRMNGB(B, D, FX, G, IV, LIV, LV, N, V, X)              
C                                                                       
C  ***  CARRY OUT  DMNGB (SIMPLY BOUNDED MINIMIZATION) ITERATIONS,      
C  ***  USING DOUBLE-DOGLEG/BFGS STEPS.                                 
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER LIV, LV, N                                                
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION B(2,N), D(N), FX, G(N), V(LV), X(N)              
C                                                                       
C--------------------------  PARAMETER USAGE  --------------------------
C                                                                       
C B.... VECTOR OF LOWER AND UPPER BOUNDS ON X.                          
C D.... SCALE VECTOR.                                                   
C FX... FUNCTION VALUE.                                                 
C G.... GRADIENT VECTOR.                                                
C IV... INTEGER VALUE ARRAY.                                            
C LIV.. LENGTH OF IV (AT LEAST 60).                                     
C LV... LENGTH OF V (AT LEAST 71 + N*(N+13)/2).                         
C N.... NUMBER OF VARIABLES (COMPONENTS IN X AND G).                    
C V.... FLOATING-POINT VALUE ARRAY.                                     
C X.... VECTOR OF PARAMETERS TO BE OPTIMIZED.                           
C                                                                       
C  ***  DISCUSSION  ***                                                 
C                                                                       
C        PARAMETERS IV, N, V, AND X ARE THE SAME AS THE CORRESPONDING   
C     ONES TO  DMNGB (WHICH SEE), EXCEPT THAT V CAN BE SHORTER (SINCE   
C     THE PART OF V THAT  DMNGB USES FOR STORING G IS NOT NEEDED).      
C     MOREOVER, COMPARED WITH  DMNGB, IV(1) MAY HAVE THE TWO ADDITIONAL 
C     OUTPUT VALUES 1 AND 2, WHICH ARE EXPLAINED BELOW, AS IS THE USE   
C     OF IV(TOOBIG) AND IV(NFGCAL).  THE VALUE IV(G), WHICH IS AN       
C     OUTPUT VALUE FROM  DMNGB (AND SMSNOB), IS NOT REFERENCED BY       
C     DRMNGB OR THE SUBROUTINES IT CALLS.                               
C        FX AND G NEED NOT HAVE BEEN INITIALIZED WHEN DRMNGB IS CALLED  
C     WITH IV(1) = 12, 13, OR 14.                                       
C                                                                       
C IV(1) = 1 MEANS THE CALLER SHOULD SET FX TO F(X), THE FUNCTION VALUE  
C             AT X, AND CALL DRMNGB AGAIN, HAVING CHANGED NONE OF THE   
C             OTHER PARAMETERS.  AN EXCEPTION OCCURS IF F(X) CANNOT BE  
C             (E.G. IF OVERFLOW WOULD OCCUR), WHICH MAY HAPPEN BECAUSE  
C             OF AN OVERSIZED STEP.  IN THIS CASE THE CALLER SHOULD SET 
C             IV(TOOBIG) = IV(2) TO 1, WHICH WILL CAUSE DRMNGB TO IG-   
C             NORE FX AND TRY A SMALLER STEP.  THE PARAMETER NF THAT    
C              DMNGB PASSES TO CALCF (FOR POSSIBLE USE BY CALCG) IS A   
C             COPY OF IV(NFCALL) = IV(6).                               
C IV(1) = 2 MEANS THE CALLER SHOULD SET G TO G(X), THE GRADIENT VECTOR  
C             OF F AT X, AND CALL DRMNGB AGAIN, HAVING CHANGED NONE OF  
C             THE OTHER PARAMETERS EXCEPT POSSIBLY THE SCALE VECTOR D   
C             WHEN IV(DTYPE) = 0.  THE PARAMETER NF THAT  DMNGB PASSES  
C             TO CALCG IS IV(NFGCAL) = IV(7).  IF G(X) CANNOT BE        
C             EVALUATED, THEN THE CALLER MAY SET IV(NFGCAL) TO 0, IN    
C             WHICH CASE DRMNGB WILL RETURN WITH IV(1) = 65.            
C.                                                                      
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY (DECEMBER 1979).  REVISED SEPT. 1982.       
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH SUPPORTED 
C     IN PART BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS           
C     MCS-7600324 AND MCS-7906671.                                      
C                                                                       
C        (SEE  DMNG FOR REFERENCES.)                                    
C                                                                       
C+++++++++++++++++++++++++++  DECLARATIONS  ++++++++++++++++++++++++++++
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER DG1, DSTEP1, DUMMY, G01, I, I1, IPI, IPN, J, K, L, LSTGST,
     1        N1, NP1, NWTST1, RSTRST, STEP1, TEMP0, TEMP1, TD1, TG1,   
     2        W1, X01, Z                                                
      DOUBLE PRECISION GI, T, XI                                        
C                                                                       
C     ***  CONSTANTS  ***                                               
C                                                                       
      DOUBLE PRECISION NEGONE, ONE, ONEP2, ZERO                         
C                                                                       
C  ***  NO INTRINSIC FUNCTIONS  ***                                     
C                                                                       
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      LOGICAL STOPX                                                     
      DOUBLE PRECISION DD7TPR, DRLDST, DV2NRM                           
      EXTERNAL DA7SST, DD7DGB,DIVSET, DD7TPR, I7SHFT,DITSUM, DL7TVM,    
     1         DL7UPD,DL7VML,DPARCK, DQ7RSH, DRLDST, STOPX, DV2NRM,     
     2        DV2AXY,DV7CPY, DV7IPR, DV7SCP, DV7VMP, DW7ZBF             
C                                                                       
C DA7SST.... ASSESSES CANDIDATE STEP.                                   
C DD7DGB... COMPUTES SIMPLY BOUNDED DOUBLE-DOGLEG (CANDIDATE) STEP.     
C DIVSET.... SUPPLIES DEFAULT IV AND V INPUT COMPONENTS.                
C DD7TPR... RETURNS INNER PRODUCT OF TWO VECTORS.                       
C I7SHFT... CYCLICALLLY SHIFTS AN ARRAY OF INTEGERS.                    
C DITSUM.... PRINTS ITERATION SUMMARY AND INFO ON INITIAL AND FINAL X.  
C DL7TVM... MULTIPLIES TRANSPOSE OF LOWER TRIANGLE TIMES VECTOR.        
C LUPDT.... UPDATES CHOLESKY FACTOR OF HESSIAN APPROXIMATION.           
C DL7VML.... MULTIPLIES LOWER TRIANGLE TIMES VECTOR.                    
C DPARCK.... CHECKS VALIDITY OF INPUT IV AND V VALUES.                  
C DQ7RSH... CYCLICALLY SHIFTS CHOLESKY FACTOR.                          
C DRLDST... COMPUTES V(RELDX) = RELATIVE STEP SIZE.                     
C STOPX.... RETURNS .TRUE. IF THE BREAK KEY HAS BEEN PRESSED.           
C DV2NRM... RETURNS THE 2-NORM OF A VECTOR.                             
C DV2AXY.... COMPUTES SCALAR TIMES ONE VECTOR PLUS ANOTHER.             
C DV7CPY.... COPIES ONE VECTOR TO ANOTHER.                              
C DV7IPR... CYCLICALLY SHIFTS A FLOATING-POINT ARRAY.                   
C DV7SCP... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.                  
C DV7VMP... MULTIPLIES VECTOR BY VECTOR RAISED TO POWER (COMPONENTWISE).
C DW7ZBF... COMPUTES W AND Z FOR DL7UPD CORRESPONDING TO BFGS UPDATE.   
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER CNVCOD, DG, DGNORM, DINIT, DSTNRM, F, F0, FDIF,           
     1        GTSTEP, INCFAC, INITH, IRC, IVNEED, KAGQT, LMAT,          
     2        LMAX0, LMAXS, MODE, MODEL, MXFCAL, MXITER, NC, NEXTIV,    
     3        NEXTV, NFCALL, NFGCAL, NGCALL, NITER, NWTSTP, PERM,       
     4        PREDUC, RADFAC, RADINC, RADIUS, RAD0, RELDX, RESTOR, STEP,
     4        STGLIM, STLSTG, TOOBIG, TUNER4, TUNER5, VNEED, XIRC, X0   
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C  ***  (NOTE THAT NC IS STORED IN IV(G0)) ***                          
C                                                                       
C/6                                                                     
C     DATA CNVCOD/55/, DG/37/, INITH/25/, IRC/29/, IVNEED/3/, KAGQT/33/,
C    1     MODE/35/, MODEL/5/, MXFCAL/17/, MXITER/18/, NC/48/,          
C    2     NEXTIV/46/, NEXTV/47/, NFCALL/6/, NFGCAL/7/, NGCALL/30/,     
C    3     NITER/31/, NWTSTP/34/, PERM/58/, RADINC/8/, RESTOR/9/,       
C    4     STEP/40/, STGLIM/11/, STLSTG/41/, TOOBIG/2/, XIRC/13/, X0/43/
C/7                                                                     
      PARAMETER (CNVCOD=55, DG=37, INITH=25, IRC=29, IVNEED=3, KAGQT=33,
     1           MODE=35, MODEL=5, MXFCAL=17, MXITER=18, NC=48,         
     2           NEXTIV=46, NEXTV=47, NFCALL=6, NFGCAL=7, NGCALL=30,    
     3           NITER=31, NWTSTP=34, PERM=58, RADINC=8, RESTOR=9,      
     4           STEP=40, STGLIM=11, STLSTG=41, TOOBIG=2, XIRC=13,      
     5           X0=43)                                                 
C/                                                                      
C                                                                       
C  ***  V SUBSCRIPT VALUES  ***                                         
C                                                                       
C/6                                                                     
C     DATA DGNORM/1/, DINIT/38/, DSTNRM/2/, F/10/, F0/13/, FDIF/11/,    
C    1     GTSTEP/4/, INCFAC/23/, LMAT/42/, LMAX0/35/, LMAXS/36/,       
C    2     PREDUC/7/, RADFAC/16/, RADIUS/8/, RAD0/9/, RELDX/17/,        
C    3     TUNER4/29/, TUNER5/30/, VNEED/4/                             
C/7                                                                     
      PARAMETER (DGNORM=1, DINIT=38, DSTNRM=2, F=10, F0=13, FDIF=11,    
     1           GTSTEP=4, INCFAC=23, LMAT=42, LMAX0=35, LMAXS=36,      
     2           PREDUC=7, RADFAC=16, RADIUS=8, RAD0=9, RELDX=17,       
     3           TUNER4=29, TUNER5=30, VNEED=4)                         
C/                                                                      
C                                                                       
C/6                                                                     
C     DATA NEGONE/-1.D+0/, ONE/1.D+0/, ONEP2/1.2D+0/, ZERO/0.D+0/       
C/7                                                                     
      PARAMETER (NEGONE=-1.D+0, ONE=1.D+0, ONEP2=1.2D+0, ZERO=0.D+0)    
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      I = IV(1)                                                         
      IF (I .EQ. 1) GO TO 70                                            
      IF (I .EQ. 2) GO TO 80                                            
C                                                                       
C  ***  CHECK VALIDITY OF IV AND V INPUT VALUES  ***                    
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(2, IV, LIV, LV, V)                  
      IF (IV(1) .LT. 12) GO TO 10                                       
      IF (IV(1) .GT. 13) GO TO 10                                       
         IV(VNEED) = IV(VNEED) + N*(N+19)/2                             
         IV(IVNEED) = IV(IVNEED) + N                                    
 10   CALL DPARCK(2, D, IV, LIV, LV, N, V)                              
      I = IV(1) - 2                                                     
      IF (I .GT. 12) GO TO 999                                          
      GO TO (250, 250, 250, 250, 250, 250, 190, 150, 190, 20, 20, 30), I
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
 20   L = IV(LMAT)                                                      
      IV(X0) = L + N*(N+1)/2                                            
      IV(STEP) = IV(X0) + 2*N                                           
      IV(STLSTG) = IV(STEP) + 2*N                                       
      IV(NWTSTP) = IV(STLSTG) + N                                       
      IV(DG) = IV(NWTSTP) + 2*N                                         
      IV(NEXTV) = IV(DG) + 2*N                                          
      IV(NEXTIV) = IV(PERM) + N                                         
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
      IV(MODEL) = 1                                                     
      IV(STGLIM) = 1                                                    
      IV(TOOBIG) = 0                                                    
      IV(CNVCOD) = 0                                                    
      IV(RADINC) = 0                                                    
      IV(NC) = N                                                        
      V(RAD0) = ZERO                                                    
C                                                                       
C  ***  CHECK CONSISTENCY OF B AND INITIALIZE IP ARRAY  ***             
C                                                                       
      IPI = IV(PERM)                                                    
      DO 40 I = 1, N                                                    
         IV(IPI) = I                                                    
         IPI = IPI + 1                                                  
         IF (B(1,I) .GT. B(2,I)) GO TO 410                              
 40      CONTINUE                                                       
C                                                                       
      IF (V(DINIT) .GE. ZERO) CALL DV7SCP(N, D, V(DINIT))               
      IF (IV(INITH) .NE. 1) GO TO 60                                    
C                                                                       
C     ***  SET THE INITIAL HESSIAN APPROXIMATION TO DIAG(D)**-2  ***    
C                                                                       
         L = IV(LMAT)                                                   
         CALL DV7SCP(N*(N+1)/2, V(L), ZERO)                             
         K = L - 1                                                      
         DO 50 I = 1, N                                                 
              K = K + I                                                 
              T = D(I)                                                  
              IF (T .LE. ZERO) T = ONE                                  
              V(K) = T                                                  
 50           CONTINUE                                                  
C                                                                       
C  ***  GET INITIAL FUNCTION VALUE  ***                                 
C                                                                       
 60   IV(1) = 1                                                         
      GO TO 440                                                         
C                                                                       
 70   V(F) = FX                                                         
      IF (IV(MODE) .GE. 0) GO TO 250                                    
      IV(1) = 2                                                         
      IF (IV(TOOBIG) .EQ. 0) GO TO 999                                  
         IV(1) = 63                                                     
         GO TO 430                                                      
C                                                                       
C  ***  MAKE SURE GRADIENT COULD BE COMPUTED  ***                       
C                                                                       
 80   IF (IV(TOOBIG) .EQ. 0) GO TO 90                                   
         IV(1) = 65                                                     
         GO TO 430                                                      
C                                                                       
C  ***  CHOOSE INITIAL PERMUTATION  ***                                 
C                                                                       
 90   IPI = IV(PERM)                                                    
      IPN = IPI + N                                                     
      N1 = N                                                            
      NP1 = N + 1                                                       
      L = IV(LMAT)                                                      
      W1 = IV(NWTSTP) + N                                               
      K = N - IV(NC)                                                    
      DO 120 I = 1, N                                                   
         IPN = IPN - 1                                                  
         J = IV(IPN)                                                    
         IF (B(1,J) .GE. B(2,J)) GO TO 100                              
         XI = X(J)                                                      
         GI = G(J)                                                      
         IF (XI .LE. B(1,J) .AND. GI .GT. ZERO) GO TO 100               
         IF (XI .GE. B(2,J) .AND. GI .LT. ZERO) GO TO 100               
C           *** DISALLOW CONVERGENCE IF X(J) HAS JUST BEEN FREED ***    
            IF (I .LE. K) IV(CNVCOD) = 0                                
            GO TO 120                                                   
 100     I1 = NP1 - I                                                   
         IF (I1 .GE. N1) GO TO 110                                      
            CALL I7SHFT(N1, I1, IV(IPI))                                
            CALL DQ7RSH(I1, N1, .FALSE., G, V(L), V(W1))                
 110        N1 = N1 - 1                                                 
 120     CONTINUE                                                       
C                                                                       
      IV(NC) = N1                                                       
      V(DGNORM) = ZERO                                                  
      IF (N1 .LE. 0) GO TO 130                                          
         DG1 = IV(DG)                                                   
         CALL DV7VMP(N, V(DG1), G, D, -1)                               
         CALL DV7IPR(N, IV(IPI), V(DG1))                                
         V(DGNORM) = DV2NRM(N1, V(DG1))                                 
 130  IF (IV(CNVCOD) .NE. 0) GO TO 420                                  
      IF (IV(MODE) .EQ. 0) GO TO 370                                    
C                                                                       
C  ***  ALLOW FIRST STEP TO HAVE SCALED 2-NORM AT MOST V(LMAX0)  ***    
C                                                                       
      V(RADIUS) = V(LMAX0)                                              
C                                                                       
      IV(MODE) = 0                                                      
C                                                                       
C                                                                       
C-----------------------------  MAIN LOOP  -----------------------------
C                                                                       
C                                                                       
C  ***  PRINT ITERATION SUMMARY, CHECK ITERATION LIMIT  ***             
C                                                                       
 140  CALL DITSUM(D, G, IV, LIV, LV, N, V, X)                           
 150  K = IV(NITER)                                                     
      IF (K .LT. IV(MXITER)) GO TO 160                                  
         IV(1) = 10                                                     
         GO TO 430                                                      
C                                                                       
C  ***  UPDATE RADIUS  ***                                              
C                                                                       
 160  IV(NITER) = K + 1                                                 
      IF (K .EQ. 0) GO TO 170                                           
      T = V(RADFAC) * V(DSTNRM)                                         
      IF (V(RADFAC) .LT. ONE .OR. T .GT. V(RADIUS)) V(RADIUS) = T       
C                                                                       
C  ***  INITIALIZE FOR START OF NEXT ITERATION  ***                     
C                                                                       
 170  X01 = IV(X0)                                                      
      V(F0) = V(F)                                                      
      IV(IRC) = 4                                                       
      IV(KAGQT) = -1                                                    
C                                                                       
C     ***  COPY X TO X0  ***                                            
C                                                                       
      CALL DV7CPY(N, V(X01), X)                                         
C                                                                       
C  ***  CHECK STOPX AND FUNCTION EVALUATION LIMIT  ***                  
C                                                                       
 180  IF (.NOT. STOPX(DUMMY)) GO TO 200                                 
         IV(1) = 11                                                     
         GO TO 210                                                      
C                                                                       
C     ***  COME HERE WHEN RESTARTING AFTER FUNC. EVAL. LIMIT OR STOPX.  
C                                                                       
 190  IF (V(F) .GE. V(F0)) GO TO 200                                    
         V(RADFAC) = ONE                                                
         K = IV(NITER)                                                  
         GO TO 160                                                      
C                                                                       
 200  IF (IV(NFCALL) .LT. IV(MXFCAL)) GO TO 220                         
         IV(1) = 9                                                      
 210     IF (V(F) .GE. V(F0)) GO TO 430                                 
C                                                                       
C        ***  IN CASE OF STOPX OR FUNCTION EVALUATION LIMIT WITH        
C        ***  IMPROVED V(F), EVALUATE THE GRADIENT AT X.                
C                                                                       
              IV(CNVCOD) = IV(1)                                        
              GO TO 360                                                 
C                                                                       
C. . . . . . . . . . . . .  COMPUTE CANDIDATE STEP  . . . . . . . . . . 
C                                                                       
 220  STEP1 = IV(STEP)                                                  
      DG1 = IV(DG)                                                      
      NWTST1 = IV(NWTSTP)                                               
      W1 = NWTST1 + N                                                   
      DSTEP1 = STEP1 + N                                                
      IPI = IV(PERM)                                                    
      L = IV(LMAT)                                                      
      TG1 = DG1 + N                                                     
      X01 = IV(X0)                                                      
      TD1 = X01 + N                                                     
      CALL DD7DGB(B, D, V(DG1), V(DSTEP1), G, IV(IPI), IV(KAGQT),       
     1            V(L), LV, N, IV(NC), V(NWTST1), V(STEP1), V(TD1),     
     2            V(TG1), V, V(W1), V(X01))                             
      IF (IV(IRC) .NE. 6) GO TO 230                                     
         IF (IV(RESTOR) .NE. 2) GO TO 250                               
         RSTRST = 2                                                     
         GO TO 260                                                      
C                                                                       
C  ***  CHECK WHETHER EVALUATING F(X0 + STEP) LOOKS WORTHWHILE  ***     
C                                                                       
 230  IV(TOOBIG) = 0                                                    
      IF (V(DSTNRM) .LE. ZERO) GO TO 250                                
      IF (IV(IRC) .NE. 5) GO TO 240                                     
      IF (V(RADFAC) .LE. ONE) GO TO 240                                 
      IF (V(PREDUC) .GT. ONEP2 * V(FDIF)) GO TO 240                     
         IF (IV(RESTOR) .NE. 2) GO TO 250                               
         RSTRST = 0                                                     
         GO TO 260                                                      
C                                                                       
C  ***  COMPUTE F(X0 + STEP)  ***                                       
C                                                                       
 240  CALL DV2AXY(N, X, ONE, V(STEP1), V(X01))                          
      IV(NFCALL) = IV(NFCALL) + 1                                       
      IV(1) = 1                                                         
      GO TO 440                                                         
C                                                                       
C. . . . . . . . . . . . .  ASSESS CANDIDATE STEP  . . . . . . . . . . .
C                                                                       
 250  RSTRST = 3                                                        
 260  X01 = IV(X0)                                                      
      V(RELDX) = DRLDST(N, D, X, V(X01))                                
      CALL DA7SST(IV, LIV, LV, V)                                       
      STEP1 = IV(STEP)                                                  
      LSTGST = IV(STLSTG)                                               
      I = IV(RESTOR) + 1                                                
      GO TO (300, 270, 280, 290), I                                     
 270  CALL DV7CPY(N, X, V(X01))                                         
      GO TO 300                                                         
 280   CALL DV7CPY(N, V(LSTGST), X)                                     
       GO TO 300                                                        
 290     CALL DV7CPY(N, X, V(LSTGST))                                   
         CALL DV2AXY(N, V(STEP1), NEGONE, V(X01), X)                    
         V(RELDX) = DRLDST(N, D, X, V(X01))                             
         IV(RESTOR) = RSTRST                                            
C                                                                       
 300  K = IV(IRC)                                                       
      GO TO (310,340,340,340,310,320,330,330,330,330,330,330,400,370), K
C                                                                       
C     ***  RECOMPUTE STEP WITH CHANGED RADIUS  ***                      
C                                                                       
 310     V(RADIUS) = V(RADFAC) * V(DSTNRM)                              
         GO TO 180                                                      
C                                                                       
C  ***  COMPUTE STEP OF LENGTH V(LMAXS) FOR SINGULAR CONVERGENCE TEST.  
C                                                                       
 320  V(RADIUS) = V(LMAXS)                                              
      GO TO 220                                                         
C                                                                       
C  ***  CONVERGENCE OR FALSE CONVERGENCE  ***                           
C                                                                       
 330  IV(CNVCOD) = K - 4                                                
      IF (V(F) .GE. V(F0)) GO TO 420                                    
         IF (IV(XIRC) .EQ. 14) GO TO 420                                
              IV(XIRC) = 14                                             
C                                                                       
C. . . . . . . . . . . .  PROCESS ACCEPTABLE STEP  . . . . . . . . . . .
C                                                                       
 340  X01 = IV(X0)                                                      
      STEP1 = IV(STEP)                                                  
      CALL DV2AXY(N, V(STEP1), NEGONE, V(X01), X)                       
      IF (IV(IRC) .NE. 3) GO TO 360                                     
C                                                                       
C     ***  SET  TEMP1 = HESSIAN * STEP  FOR USE IN GRADIENT TESTS  ***  
C                                                                       
C     ***  USE X0 AS TEMPORARY...                                       
C                                                                       
         IPI = IV(PERM)                                                 
         CALL DV7CPY(N, V(X01), V(STEP1))                               
         CALL DV7IPR(N, IV(IPI), V(X01))                                
         L = IV(LMAT)                                                   
         CALL DL7TVM(N, V(X01), V(L), V(X01))                           
         CALL DL7VML(N, V(X01), V(L), V(X01))                           
C                                                                       
C        *** UNPERMUTE X0 INTO TEMP1 ***                                
C                                                                       
         TEMP1 = IV(STLSTG)                                             
         TEMP0 = TEMP1 - 1                                              
         DO 350 I = 1, N                                                
            J = IV(IPI)                                                 
            IPI = IPI + 1                                               
            K = TEMP0 + J                                               
            V(K) = V(X01)                                               
            X01 = X01 + 1                                               
 350        CONTINUE                                                    
C                                                                       
C  ***  SAVE OLD GRADIENT, COMPUTE NEW ONE  ***                         
C                                                                       
 360  G01 = IV(NWTSTP) + N                                              
      CALL DV7CPY(N, V(G01), G)                                         
      IV(NGCALL) = IV(NGCALL) + 1                                       
      IV(TOOBIG) = 0                                                    
      IV(1) = 2                                                         
      GO TO 999                                                         
C                                                                       
C  ***  INITIALIZATIONS -- G0 = G - G0, ETC.  ***                       
C                                                                       
 370  G01 = IV(NWTSTP) + N                                              
      CALL DV2AXY(N, V(G01), NEGONE, V(G01), G)                         
      STEP1 = IV(STEP)                                                  
      TEMP1 = IV(STLSTG)                                                
      IF (IV(IRC) .NE. 3) GO TO 390                                     
C                                                                       
C  ***  SET V(RADFAC) BY GRADIENT TESTS  ***                            
C                                                                       
C     ***  SET  TEMP1 = DIAG(D)**-1 * (HESSIAN*STEP + (G(X0)-G(X)))  ***
C                                                                       
         CALL DV2AXY(N, V(TEMP1), NEGONE, V(G01), V(TEMP1))             
         CALL DV7VMP(N, V(TEMP1), V(TEMP1), D, -1)                      
C                                                                       
C        ***  DO GRADIENT TESTS  ***                                    
C                                                                       
         IF (DV2NRM(N, V(TEMP1)) .LE. V(DGNORM) * V(TUNER4))            
     1                  GO TO 380                                       
              IF (DD7TPR(N, G, V(STEP1))                                
     1                  .GE. V(GTSTEP) * V(TUNER5))  GO TO 390          
 380               V(RADFAC) = V(INCFAC)                                
C                                                                       
C  ***  UPDATE H, LOOP  ***                                             
C                                                                       
 390  W1 = IV(NWTSTP)                                                   
      Z = IV(X0)                                                        
      L = IV(LMAT)                                                      
      IPI = IV(PERM)                                                    
      CALL DV7IPR(N, IV(IPI), V(STEP1))                                 
      CALL DV7IPR(N, IV(IPI), V(G01))                                   
      CALL DW7ZBF(V(L), N, V(STEP1), V(W1), V(G01), V(Z))               
C                                                                       
C     ** USE THE N-VECTORS STARTING AT V(STEP1) AND V(G01) FOR SCRATCH..
      CALL DL7UPD(V(TEMP1), V(STEP1), V(L), V(G01), V(L), N, V(W1),     
     1            V(Z))                                                 
      IV(1) = 2                                                         
      GO TO 140                                                         
C                                                                       
C. . . . . . . . . . . . . .  MISC. DETAILS  . . . . . . . . . . . . . .
C                                                                       
C  ***  BAD PARAMETERS TO ASSESS  ***                                   
C                                                                       
 400  IV(1) = 64                                                        
      GO TO 430                                                         
C                                                                       
C  ***  INCONSISTENT B  ***                                             
C                                                                       
 410  IV(1) = 82                                                        
      GO TO 430                                                         
C                                                                       
C  ***  PRINT SUMMARY OF FINAL ITERATION AND OTHER REQUESTED ITEMS  *** 
C                                                                       
 420  IV(1) = IV(CNVCOD)                                                
      IV(CNVCOD) = 0                                                    
 430  CALL DITSUM(D, G, IV, LIV, LV, N, V, X)                           
      GO TO 999                                                         
C                                                                       
C  ***  PROJECT X INTO FEASIBLE REGION (PRIOR TO COMPUTING F OR G)  *** 
C                                                                       
 440  DO 450 I = 1, N                                                   
         IF (X(I) .LT. B(1,I)) X(I) = B(1,I)                            
         IF (X(I) .GT. B(2,I)) X(I) = B(2,I)                            
 450     CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF DRMNGB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DRMNH(D, FX, G, H, IV, LH, LIV, LV, N, V, X)           
C                                                                       
C  ***  CARRY OUT  DMNH (UNCONSTRAINED MINIMIZATION) ITERATIONS, USING  
C  ***  HESSIAN MATRIX PROVIDED BY THE CALLER.                          
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER LH, LIV, LV, N                                            
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION D(N), FX, G(N), H(LH), V(LV), X(N)               
C                                                                       
C--------------------------  PARAMETER USAGE  --------------------------
C                                                                       
C D.... SCALE VECTOR.                                                   
C FX... FUNCTION VALUE.                                                 
C G.... GRADIENT VECTOR.                                                
C H.... LOWER TRIANGLE OF THE HESSIAN, STORED ROWWISE.                  
C IV... INTEGER VALUE ARRAY.                                            
C LH... LENGTH OF H = P*(P+1)/2.                                        
C LIV.. LENGTH OF IV (AT LEAST 60).                                     
C LV... LENGTH OF V (AT LEAST 78 + N*(N+21)/2).                         
C N.... NUMBER OF VARIABLES (COMPONENTS IN X AND G).                    
C V.... FLOATING-POINT VALUE ARRAY.                                     
C X.... PARAMETER VECTOR.                                               
C                                                                       
C  ***  DISCUSSION  ***                                                 
C                                                                       
C        PARAMETERS IV, N, V, AND X ARE THE SAME AS THE CORRESPONDING   
C     ONES TO  DMNH (WHICH SEE), EXCEPT THAT V CAN BE SHORTER (SINCE    
C     THE PART OF V THAT  DMNH USES FOR STORING G AND H IS NOT NEEDED). 
C     MOREOVER, COMPARED WITH  DMNH, IV(1) MAY HAVE THE TWO ADDITIONAL  
C     OUTPUT VALUES 1 AND 2, WHICH ARE EXPLAINED BELOW, AS IS THE USE   
C     OF IV(TOOBIG) AND IV(NFGCAL).  THE VALUE IV(G), WHICH IS AN       
C     OUTPUT VALUE FROM  DMNH, IS NOT REFERENCED BY DRMNH OR THE        
C     SUBROUTINES IT CALLS.                                             
C                                                                       
C IV(1) = 1 MEANS THE CALLER SHOULD SET FX TO F(X), THE FUNCTION VALUE  
C             AT X, AND CALL DRMNH AGAIN, HAVING CHANGED NONE OF THE    
C             OTHER PARAMETERS.  AN EXCEPTION OCCURS IF F(X) CANNOT BE  
C             COMPUTED (E.G. IF OVERFLOW WOULD OCCUR), WHICH MAY HAPPEN 
C             BECAUSE OF AN OVERSIZED STEP.  IN THIS CASE THE CALLER    
C             SHOULD SET IV(TOOBIG) = IV(2) TO 1, WHICH WILL CAUSE      
C             DRMNH TO IGNORE FX AND TRY A SMALLER STEP.  THE PARA-     
C             METER NF THAT  DMNH PASSES TO CALCF (FOR POSSIBLE USE BY  
C             CALCGH) IS A COPY OF IV(NFCALL) = IV(6).                  
C IV(1) = 2 MEANS THE CALLER SHOULD SET G TO G(X), THE GRADIENT OF F AT 
C             X, AND H TO THE LOWER TRIANGLE OF H(X), THE HESSIAN OF F  
C             AT X, AND CALL DRMNH AGAIN, HAVING CHANGED NONE OF THE    
C             OTHER PARAMETERS EXCEPT PERHAPS THE SCALE VECTOR D.       
C                  THE PARAMETER NF THAT  DMNH PASSES TO CALCG IS       
C             IV(NFGCAL) = IV(7).  IF G(X) AND H(X) CANNOT BE EVALUATED,
C             THEN THE CALLER MAY SET IV(TOOBIG) TO 0, IN WHICH CASE    
C             DRMNH WILL RETURN WITH IV(1) = 65.                        
C                  NOTE -- DRMNH OVERWRITES H WITH THE LOWER TRIANGLE   
C             OF  DIAG(D)**-1 * H(X) * DIAG(D)**-1.                     
C.                                                                      
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY (WINTER 1980).  REVISED SEPT. 1982.         
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH SUPPORTED 
C     IN PART BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS           
C     MCS-7600324 AND MCS-7906671.                                      
C                                                                       
C        (SEE  DMNG AND  DMNH FOR REFERENCES.)                          
C                                                                       
C+++++++++++++++++++++++++++  DECLARATIONS  ++++++++++++++++++++++++++++
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER DG1, DUMMY, I, J, K, L, LSTGST, NN1O2, RSTRST, STEP1,     
     1        TEMP1, W1, X01                                            
      DOUBLE PRECISION T                                                
C                                                                       
C     ***  CONSTANTS  ***                                               
C                                                                       
      DOUBLE PRECISION ONE, ONEP2, ZERO                                 
C                                                                       
C  ***  NO INTRINSIC FUNCTIONS  ***                                     
C                                                                       
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      LOGICAL STOPX                                                     
      DOUBLE PRECISION DD7TPR, DRLDST, DV2NRM                           
      EXTERNAL DA7SST,DIVSET, DD7TPR,DD7DUP,DG7QTS,DITSUM,DPARCK,       
     1         DRLDST, DS7LVM, STOPX,DV2AXY,DV7CPY, DV7SCP, DV2NRM      
C                                                                       
C DA7SST.... ASSESSES CANDIDATE STEP.                                   
C DIVSET.... PROVIDES DEFAULT IV AND V INPUT VALUES.                    
C DD7TPR... RETURNS INNER PRODUCT OF TWO VECTORS.                       
C DD7DUP.... UPDATES SCALE VECTOR D.                                    
C DG7QTS.... COMPUTES OPTIMALLY LOCALLY CONSTRAINED STEP.               
C DITSUM.... PRINTS ITERATION SUMMARY AND INFO ON INITIAL AND FINAL X.  
C DPARCK.... CHECKS VALIDITY OF INPUT IV AND V VALUES.                  
C DRLDST... COMPUTES V(RELDX) = RELATIVE STEP SIZE.                     
C DS7LVM... MULTIPLIES SYMMETRIC MATRIX TIMES VECTOR, GIVEN THE LOWER   
C             TRIANGLE OF THE MATRIX.                                   
C STOPX.... RETURNS .TRUE. IF THE BREAK KEY HAS BEEN PRESSED.           
C DV2AXY.... COMPUTES SCALAR TIMES ONE VECTOR PLUS ANOTHER.             
C DV7CPY.... COPIES ONE VECTOR TO ANOTHER.                              
C DV7SCP... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.                  
C DV2NRM... RETURNS THE 2-NORM OF A VECTOR.                             
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER CNVCOD, DG, DGNORM, DINIT, DSTNRM, DTINIT, DTOL,          
     1        DTYPE, D0INIT, F, F0, FDIF, GTSTEP, INCFAC, IRC, KAGQT,   
     2        LMAT, LMAX0, LMAXS, MODE, MODEL, MXFCAL, MXITER, NEXTV,   
     3        NFCALL, NFGCAL, NGCALL, NITER, PHMXFC, PREDUC, RADFAC,    
     4        RADINC, RADIUS, RAD0, RELDX, RESTOR, STEP, STGLIM, STLSTG,
     5        STPPAR, TOOBIG, TUNER4, TUNER5, VNEED, W, XIRC, X0        
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C/6                                                                     
C     DATA CNVCOD/55/, DG/37/, DTOL/59/, DTYPE/16/, IRC/29/, KAGQT/33/, 
C    1     LMAT/42/, MODE/35/, MODEL/5/, MXFCAL/17/, MXITER/18/,        
C    2     NEXTV/47/, NFCALL/6/, NFGCAL/7/, NGCALL/30/, NITER/31/,      
C    3     RADINC/8/, RESTOR/9/, STEP/40/, STGLIM/11/, STLSTG/41/,      
C    4     TOOBIG/2/, VNEED/4/, W/34/, XIRC/13/, X0/43/                 
C/7                                                                     
      PARAMETER (CNVCOD=55, DG=37, DTOL=59, DTYPE=16, IRC=29, KAGQT=33, 
     1           LMAT=42, MODE=35, MODEL=5, MXFCAL=17, MXITER=18,       
     2           NEXTV=47, NFCALL=6, NFGCAL=7, NGCALL=30, NITER=31,     
     3           RADINC=8, RESTOR=9, STEP=40, STGLIM=11, STLSTG=41,     
     4           TOOBIG=2, VNEED=4, W=34, XIRC=13, X0=43)               
C/                                                                      
C                                                                       
C  ***  V SUBSCRIPT VALUES  ***                                         
C                                                                       
C/6                                                                     
C     DATA DGNORM/1/, DINIT/38/, DSTNRM/2/, DTINIT/39/, D0INIT/40/,     
C    1     F/10/, F0/13/, FDIF/11/, GTSTEP/4/, INCFAC/23/, LMAX0/35/,   
C    2     LMAXS/36/, PHMXFC/21/, PREDUC/7/, RADFAC/16/, RADIUS/8/,     
C    3     RAD0/9/, RELDX/17/, STPPAR/5/, TUNER4/29/, TUNER5/30/        
C/7                                                                     
      PARAMETER (DGNORM=1, DINIT=38, DSTNRM=2, DTINIT=39, D0INIT=40,    
     1           F=10, F0=13, FDIF=11, GTSTEP=4, INCFAC=23, LMAX0=35,   
     2           LMAXS=36, PHMXFC=21, PREDUC=7, RADFAC=16, RADIUS=8,    
     3           RAD0=9, RELDX=17, STPPAR=5, TUNER4=29, TUNER5=30)      
C/                                                                      
C                                                                       
C/6                                                                     
C     DATA ONE/1.D+0/, ONEP2/1.2D+0/, ZERO/0.D+0/                       
C/7                                                                     
      PARAMETER (ONE=1.D+0, ONEP2=1.2D+0, ZERO=0.D+0)                   
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      I = IV(1)                                                         
      IF (I .EQ. 1) GO TO 30                                            
      IF (I .EQ. 2) GO TO 40                                            
C                                                                       
C  ***  CHECK VALIDITY OF IV AND V INPUT VALUES  ***                    
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(2, IV, LIV, LV, V)                  
      IF (IV(1) .EQ. 12 .OR. IV(1) .EQ. 13)                             
     1     IV(VNEED) = IV(VNEED) + N*(N+21)/2 + 7                       
      CALL DPARCK(2, D, IV, LIV, LV, N, V)                              
      I = IV(1) - 2                                                     
      IF (I .GT. 12) GO TO 999                                          
      NN1O2 = N * (N + 1) / 2                                           
      IF (LH .GE. NN1O2) GO TO (220,220,220,220,220,220,160,120,160,    
     1                          10,10,20), I                            
         IV(1) = 66                                                     
         GO TO 400                                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
 10   IV(DTOL) = IV(LMAT) + NN1O2                                       
      IV(X0) = IV(DTOL) + 2*N                                           
      IV(STEP) = IV(X0) + N                                             
      IV(STLSTG) = IV(STEP) + N                                         
      IV(DG) = IV(STLSTG) + N                                           
      IV(W) = IV(DG) + N                                                
      IV(NEXTV) = IV(W) + 4*N + 7                                       
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
      IV(MODEL) = 1                                                     
      IV(STGLIM) = 1                                                    
      IV(TOOBIG) = 0                                                    
      IV(CNVCOD) = 0                                                    
      IV(RADINC) = 0                                                    
      V(RAD0) = ZERO                                                    
      V(STPPAR) = ZERO                                                  
      IF (V(DINIT) .GE. ZERO) CALL DV7SCP(N, D, V(DINIT))               
      K = IV(DTOL)                                                      
      IF (V(DTINIT) .GT. ZERO) CALL DV7SCP(N, V(K), V(DTINIT))          
      K = K + N                                                         
      IF (V(D0INIT) .GT. ZERO) CALL DV7SCP(N, V(K), V(D0INIT))          
      IV(1) = 1                                                         
      GO TO 999                                                         
C                                                                       
 30   V(F) = FX                                                         
      IF (IV(MODE) .GE. 0) GO TO 220                                    
      IV(1) = 2                                                         
      IF (IV(TOOBIG) .EQ. 0) GO TO 999                                  
         IV(1) = 63                                                     
         GO TO 400                                                      
C                                                                       
C  ***  MAKE SURE GRADIENT COULD BE COMPUTED  ***                       
C                                                                       
 40   IF (IV(TOOBIG) .EQ. 0) GO TO 50                                   
         IV(1) = 65                                                     
         GO TO 400                                                      
C                                                                       
C  ***  UPDATE THE SCALE VECTOR D  ***                                  
C                                                                       
 50   DG1 = IV(DG)                                                      
      IF (IV(DTYPE) .LE. 0) GO TO 70                                    
      K = DG1                                                           
      J = 0                                                             
      DO 60 I = 1, N                                                    
         J = J + I                                                      
         V(K) = H(J)                                                    
         K = K + 1                                                      
 60      CONTINUE                                                       
      CALL DD7DUP(D, V(DG1), IV, LIV, LV, N, V)                         
C                                                                       
C  ***  COMPUTE SCALED GRADIENT AND ITS NORM  ***                       
C                                                                       
 70   DG1 = IV(DG)                                                      
      K = DG1                                                           
      DO 80 I = 1, N                                                    
         V(K) = G(I) / D(I)                                             
         K = K + 1                                                      
 80      CONTINUE                                                       
      V(DGNORM) = DV2NRM(N, V(DG1))                                     
C                                                                       
C  ***  COMPUTE SCALED HESSIAN  ***                                     
C                                                                       
      K = 1                                                             
      DO 100 I = 1, N                                                   
         T = ONE / D(I)                                                 
         DO 90 J = 1, I                                                 
              H(K) = T * H(K) / D(J)                                    
              K = K + 1                                                 
 90           CONTINUE                                                  
 100     CONTINUE                                                       
C                                                                       
      IF (IV(CNVCOD) .NE. 0) GO TO 390                                  
      IF (IV(MODE) .EQ. 0) GO TO 350                                    
C                                                                       
C  ***  ALLOW FIRST STEP TO HAVE SCALED 2-NORM AT MOST V(LMAX0)  ***    
C                                                                       
      V(RADIUS) = V(LMAX0) / (ONE + V(PHMXFC))                          
C                                                                       
      IV(MODE) = 0                                                      
C                                                                       
C                                                                       
C-----------------------------  MAIN LOOP  -----------------------------
C                                                                       
C                                                                       
C  ***  PRINT ITERATION SUMMARY, CHECK ITERATION LIMIT  ***             
C                                                                       
 110  CALL DITSUM(D, G, IV, LIV, LV, N, V, X)                           
 120  K = IV(NITER)                                                     
      IF (K .LT. IV(MXITER)) GO TO 130                                  
         IV(1) = 10                                                     
         GO TO 400                                                      
C                                                                       
 130  IV(NITER) = K + 1                                                 
C                                                                       
C  ***  INITIALIZE FOR START OF NEXT ITERATION  ***                     
C                                                                       
      DG1 = IV(DG)                                                      
      X01 = IV(X0)                                                      
      V(F0) = V(F)                                                      
      IV(IRC) = 4                                                       
      IV(KAGQT) = -1                                                    
C                                                                       
C     ***  COPY X TO X0  ***                                            
C                                                                       
      CALL DV7CPY(N, V(X01), X)                                         
C                                                                       
C  ***  UPDATE RADIUS  ***                                              
C                                                                       
      IF (K .EQ. 0) GO TO 150                                           
      STEP1 = IV(STEP)                                                  
      K = STEP1                                                         
      DO 140 I = 1, N                                                   
         V(K) = D(I) * V(K)                                             
         K = K + 1                                                      
 140     CONTINUE                                                       
      V(RADIUS) = V(RADFAC) * DV2NRM(N, V(STEP1))                       
C                                                                       
C  ***  CHECK STOPX AND FUNCTION EVALUATION LIMIT  ***                  
C                                                                       
 150  IF (.NOT. STOPX(DUMMY)) GO TO 170                                 
         IV(1) = 11                                                     
         GO TO 180                                                      
C                                                                       
C     ***  COME HERE WHEN RESTARTING AFTER FUNC. EVAL. LIMIT OR STOPX.  
C                                                                       
 160  IF (V(F) .GE. V(F0)) GO TO 170                                    
         V(RADFAC) = ONE                                                
         K = IV(NITER)                                                  
         GO TO 130                                                      
C                                                                       
 170  IF (IV(NFCALL) .LT. IV(MXFCAL)) GO TO 190                         
         IV(1) = 9                                                      
 180     IF (V(F) .GE. V(F0)) GO TO 400                                 
C                                                                       
C        ***  IN CASE OF STOPX OR FUNCTION EVALUATION LIMIT WITH        
C        ***  IMPROVED V(F), EVALUATE THE GRADIENT AT X.                
C                                                                       
              IV(CNVCOD) = IV(1)                                        
              GO TO 340                                                 
C                                                                       
C. . . . . . . . . . . . .  COMPUTE CANDIDATE STEP  . . . . . . . . . . 
C                                                                       
 190  STEP1 = IV(STEP)                                                  
      DG1 = IV(DG)                                                      
      L = IV(LMAT)                                                      
      W1 = IV(W)                                                        
      CALL DG7QTS(D, V(DG1), H, IV(KAGQT), V(L), N, V(STEP1), V, V(W1)) 
      IF (IV(IRC) .NE. 6) GO TO 200                                     
         IF (IV(RESTOR) .NE. 2) GO TO 220                               
         RSTRST = 2                                                     
         GO TO 230                                                      
C                                                                       
C  ***  CHECK WHETHER EVALUATING F(X0 + STEP) LOOKS WORTHWHILE  ***     
C                                                                       
 200  IV(TOOBIG) = 0                                                    
      IF (V(DSTNRM) .LE. ZERO) GO TO 220                                
      IF (IV(IRC) .NE. 5) GO TO 210                                     
      IF (V(RADFAC) .LE. ONE) GO TO 210                                 
      IF (V(PREDUC) .GT. ONEP2 * V(FDIF)) GO TO 210                     
         IF (IV(RESTOR) .NE. 2) GO TO 220                               
         RSTRST = 0                                                     
         GO TO 230                                                      
C                                                                       
C  ***  COMPUTE F(X0 + STEP)  ***                                       
C                                                                       
 210  X01 = IV(X0)                                                      
      STEP1 = IV(STEP)                                                  
      CALL DV2AXY(N, X, ONE, V(STEP1), V(X01))                          
      IV(NFCALL) = IV(NFCALL) + 1                                       
      IV(1) = 1                                                         
      GO TO 999                                                         
C                                                                       
C. . . . . . . . . . . . .  ASSESS CANDIDATE STEP  . . . . . . . . . . .
C                                                                       
 220  RSTRST = 3                                                        
 230  X01 = IV(X0)                                                      
      V(RELDX) = DRLDST(N, D, X, V(X01))                                
      CALL DA7SST(IV, LIV, LV, V)                                       
      STEP1 = IV(STEP)                                                  
      LSTGST = IV(STLSTG)                                               
      I = IV(RESTOR) + 1                                                
      GO TO (270, 240, 250, 260), I                                     
 240  CALL DV7CPY(N, X, V(X01))                                         
      GO TO 270                                                         
 250   CALL DV7CPY(N, V(LSTGST), V(STEP1))                              
       GO TO 270                                                        
 260     CALL DV7CPY(N, V(STEP1), V(LSTGST))                            
         CALL DV2AXY(N, X, ONE, V(STEP1), V(X01))                       
         V(RELDX) = DRLDST(N, D, X, V(X01))                             
         IV(RESTOR) = RSTRST                                            
C                                                                       
 270  K = IV(IRC)                                                       
      GO TO (280,310,310,310,280,290,300,300,300,300,300,300,380,350), K
C                                                                       
C     ***  RECOMPUTE STEP WITH NEW RADIUS  ***                          
C                                                                       
 280     V(RADIUS) = V(RADFAC) * V(DSTNRM)                              
         GO TO 150                                                      
C                                                                       
C  ***  COMPUTE STEP OF LENGTH V(LMAXS) FOR SINGULAR CONVERGENCE TEST.  
C                                                                       
 290  V(RADIUS) = V(LMAXS)                                              
      GO TO 190                                                         
C                                                                       
C  ***  CONVERGENCE OR FALSE CONVERGENCE  ***                           
C                                                                       
 300  IV(CNVCOD) = K - 4                                                
      IF (V(F) .GE. V(F0)) GO TO 390                                    
         IF (IV(XIRC) .EQ. 14) GO TO 390                                
              IV(XIRC) = 14                                             
C                                                                       
C. . . . . . . . . . . .  PROCESS ACCEPTABLE STEP  . . . . . . . . . . .
C                                                                       
 310  IF (IV(IRC) .NE. 3) GO TO 340                                     
         TEMP1 = LSTGST                                                 
C                                                                       
C     ***  PREPARE FOR GRADIENT TESTS  ***                              
C     ***  SET  TEMP1 = HESSIAN * STEP + G(X0)                          
C     ***             = DIAG(D) * (H * STEP + G(X0))                    
C                                                                       
C        USE X0 VECTOR AS TEMPORARY.                                    
         K = X01                                                        
         DO 320 I = 1, N                                                
              V(K) = D(I) * V(STEP1)                                    
              K = K + 1                                                 
              STEP1 = STEP1 + 1                                         
 320          CONTINUE                                                  
         CALL DS7LVM(N, V(TEMP1), H, V(X01))                            
         DO 330 I = 1, N                                                
              V(TEMP1) = D(I) * V(TEMP1) + G(I)                         
              TEMP1 = TEMP1 + 1                                         
 330          CONTINUE                                                  
C                                                                       
C  ***  COMPUTE GRADIENT AND HESSIAN  ***                               
C                                                                       
 340  IV(NGCALL) = IV(NGCALL) + 1                                       
      IV(TOOBIG) = 0                                                    
      IV(1) = 2                                                         
      GO TO 999                                                         
C                                                                       
 350  IV(1) = 2                                                         
      IF (IV(IRC) .NE. 3) GO TO 110                                     
C                                                                       
C  ***  SET V(RADFAC) BY GRADIENT TESTS  ***                            
C                                                                       
      TEMP1 = IV(STLSTG)                                                
      STEP1 = IV(STEP)                                                  
C                                                                       
C     ***  SET  TEMP1 = DIAG(D)**-1 * (HESSIAN*STEP + (G(X0)-G(X)))  ***
C                                                                       
      K = TEMP1                                                         
      DO 360 I = 1, N                                                   
         V(K) = (V(K) - G(I)) / D(I)                                    
         K = K + 1                                                      
 360     CONTINUE                                                       
C                                                                       
C     ***  DO GRADIENT TESTS  ***                                       
C                                                                       
      IF (DV2NRM(N, V(TEMP1)) .LE. V(DGNORM) * V(TUNER4)) GO TO 370     
           IF (DD7TPR(N, G, V(STEP1))                                   
     1               .GE. V(GTSTEP) * V(TUNER5))  GO TO 110             
 370            V(RADFAC) = V(INCFAC)                                   
                GO TO 110                                               
C                                                                       
C. . . . . . . . . . . . . .  MISC. DETAILS  . . . . . . . . . . . . . .
C                                                                       
C  ***  BAD PARAMETERS TO ASSESS  ***                                   
C                                                                       
 380  IV(1) = 64                                                        
      GO TO 400                                                         
C                                                                       
C  ***  PRINT SUMMARY OF FINAL ITERATION AND OTHER REQUESTED ITEMS  *** 
C                                                                       
 390  IV(1) = IV(CNVCOD)                                                
      IV(CNVCOD) = 0                                                    
 400  CALL DITSUM(D, G, IV, LIV, LV, N, V, X)                           
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF DRMNH FOLLOWS  ***                                 
      END                                                               
      SUBROUTINE DRMNHB(B, D, FX, G, H, IV, LH, LIV, LV, N, V, X)       
C                                                                       
C  ***  CARRY OUT  DMNHB (SIMPLY BOUNDED MINIMIZATION) ITERATIONS,      
C  ***  USING HESSIAN MATRIX PROVIDED BY THE CALLER.                    
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER LH, LIV, LV, N                                            
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION B(2,N), D(N), FX, G(N), H(LH), V(LV), X(N)       
C                                                                       
C--------------------------  PARAMETER USAGE  --------------------------
C                                                                       
C D.... SCALE VECTOR.                                                   
C FX... FUNCTION VALUE.                                                 
C G.... GRADIENT VECTOR.                                                
C H.... LOWER TRIANGLE OF THE HESSIAN, STORED ROWWISE.                  
C IV... INTEGER VALUE ARRAY.                                            
C LH... LENGTH OF H = P*(P+1)/2.                                        
C LIV.. LENGTH OF IV (AT LEAST 60).                                     
C LV... LENGTH OF V (AT LEAST 78 + N*(N+21)/2).                         
C N.... NUMBER OF VARIABLES (COMPONENTS IN X AND G).                    
C V.... FLOATING-POINT VALUE ARRAY.                                     
C X.... PARAMETER VECTOR.                                               
C                                                                       
C  ***  DISCUSSION  ***                                                 
C                                                                       
C        PARAMETERS IV, N, V, AND X ARE THE SAME AS THE CORRESPONDING   
C     ONES TO  DMNHB (WHICH SEE), EXCEPT THAT V CAN BE SHORTER (SINCE   
C     THE PART OF V THAT  DMNHB USES FOR STORING G AND H IS NOT NEEDED).
C     MOREOVER, COMPARED WITH  DMNHB, IV(1) MAY HAVE THE TWO ADDITIONAL 
C     OUTPUT VALUES 1 AND 2, WHICH ARE EXPLAINED BELOW, AS IS THE USE   
C     OF IV(TOOBIG) AND IV(NFGCAL).  THE VALUE IV(G), WHICH IS AN       
C     OUTPUT VALUE FROM  DMNHB, IS NOT REFERENCED BY DRMNHB OR THE      
C     SUBROUTINES IT CALLS.                                             
C                                                                       
C IV(1) = 1 MEANS THE CALLER SHOULD SET FX TO F(X), THE FUNCTION VALUE  
C             AT X, AND CALL DRMNHB AGAIN, HAVING CHANGED NONE OF THE   
C             OTHER PARAMETERS.  AN EXCEPTION OCCURS IF F(X) CANNOT BE  
C             COMPUTED (E.G. IF OVERFLOW WOULD OCCUR), WHICH MAY HAPPEN 
C             BECAUSE OF AN OVERSIZED STEP.  IN THIS CASE THE CALLER    
C             SHOULD SET IV(TOOBIG) = IV(2) TO 1, WHICH WILL CAUSE      
C             DRMNHB TO IGNORE FX AND TRY A SMALLER STEP.  THE PARA-    
C             METER NF THAT  DMNH PASSES TO CALCF (FOR POSSIBLE USE BY  
C             CALCGH) IS A COPY OF IV(NFCALL) = IV(6).                  
C IV(1) = 2 MEANS THE CALLER SHOULD SET G TO G(X), THE GRADIENT OF F AT 
C             X, AND H TO THE LOWER TRIANGLE OF H(X), THE HESSIAN OF F  
C             AT X, AND CALL DRMNHB AGAIN, HAVING CHANGED NONE OF THE   
C             OTHER PARAMETERS EXCEPT PERHAPS THE SCALE VECTOR D.       
C                  THE PARAMETER NF THAT  DMNHB PASSES TO CALCG IS      
C             IV(NFGCAL) = IV(7).  IF G(X) AND H(X) CANNOT BE EVALUATED,
C             THEN THE CALLER MAY SET IV(NFGCAL) TO 0, IN WHICH CASE    
C             DRMNHB WILL RETURN WITH IV(1) = 65.                       
C                  NOTE -- DRMNHB OVERWRITES H WITH THE LOWER TRIANGLE  
C             OF  DIAG(D)**-1 * H(X) * DIAG(D)**-1.                     
C.                                                                      
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY (WINTER, SPRING 1983).                      
C                                                                       
C        (SEE  DMNG AND  DMNH FOR REFERENCES.)                          
C                                                                       
C+++++++++++++++++++++++++++  DECLARATIONS  ++++++++++++++++++++++++++++
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER DG1, DUMMY, I, IPI, IPIV2, IPN, J, K, L, LSTGST, NN1O2,   
     1        RSTRST, STEP0, STEP1, TD1, TEMP0, TEMP1, TG1, W1, X01, X11
      DOUBLE PRECISION GI, T, XI                                        
C                                                                       
C     ***  CONSTANTS  ***                                               
C                                                                       
      DOUBLE PRECISION NEGONE, ONE, ONEP2, ZERO                         
C                                                                       
C  ***  NO INTRINSIC FUNCTIONS  ***                                     
C                                                                       
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      LOGICAL STOPX                                                     
      DOUBLE PRECISION DD7TPR, DRLDST, DV2NRM                           
      EXTERNAL DA7SST,DIVSET, DD7TPR,DD7DUP, DG7QSB, I7PNVR,DITSUM,     
     1        DPARCK, DRLDST, DS7IPR, DS7LVM, STOPX, DV2NRM,DV2AXY,     
     2        DV7CPY, DV7IPR, DV7SCP, DV7VMP                            
C                                                                       
C DA7SST.... ASSESSES CANDIDATE STEP.                                   
C DIVSET.... PROVIDES DEFAULT IV AND V INPUT VALUES.                    
C DD7TPR... RETURNS INNER PRODUCT OF TWO VECTORS.                       
C DD7DUP.... UPDATES SCALE VECTOR D.                                    
C DG7QSB... COMPUTES APPROXIMATE OPTIMAL BOUNDED STEP.                  
C I7PNVR... INVERTS PERMUTATION ARRAY.                                  
C DITSUM.... PRINTS ITERATION SUMMARY AND INFO ON INITIAL AND FINAL X.  
C DPARCK.... CHECKS VALIDITY OF INPUT IV AND V VALUES.                  
C DRLDST... COMPUTES V(RELDX) = RELATIVE STEP SIZE.                     
C DS7IPR... APPLIES PERMUTATION TO LOWER TRIANG. OF SYM. MATRIX.        
C DS7LVM... MULTIPLIES SYMMETRIC MATRIX TIMES VECTOR, GIVEN THE LOWER   
C             TRIANGLE OF THE MATRIX.                                   
C STOPX.... RETURNS .TRUE. IF THE BREAK KEY HAS BEEN PRESSED.           
C DV2NRM... RETURNS THE 2-NORM OF A VECTOR.                             
C DV2AXY.... COMPUTES SCALAR TIMES ONE VECTOR PLUS ANOTHER.             
C DV7CPY.... COPIES ONE VECTOR TO ANOTHER.                              
C DV7IPR... APPLIES PERMUTATION TO VECTOR.                              
C DV7SCP... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.                  
C DV7VMP... MULTIPLIES (OR DIVIDES) TWO VECTORS COMPONENTWISE.          
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER CNVCOD, DG, DGNORM, DINIT, DSTNRM, DTINIT, DTOL, DTYPE,   
     1        D0INIT, F, F0, FDIF, GTSTEP, INCFAC, IVNEED, IRC, KAGQT,  
     2        LMAT, LMAX0, LMAXS, MODE, MODEL, MXFCAL, MXITER, N0, NC,  
     3        NEXTIV, NEXTV, NFCALL, NFGCAL, NGCALL, NITER, PERM,       
     4        PHMXFC, PREDUC, RADFAC, RADINC, RADIUS, RAD0, RELDX,      
     5        RESTOR, STEP, STGLIM, STPPAR, TOOBIG, TUNER4, TUNER5,     
     6        VNEED, W, XIRC, X0                                        
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C  ***  (NOTE THAT NC AND N0 ARE STORED IN IV(G0) AND IV(STLSTG) RESP.) 
C                                                                       
C/6                                                                     
C     DATA CNVCOD/55/, DG/37/, DTOL/59/, DTYPE/16/, IRC/29/, IVNEED/3/, 
C    1     KAGQT/33/, LMAT/42/, MODE/35/, MODEL/5/, MXFCAL/17/,         
C    2     MXITER/18/, N0/41/, NC/48/, NEXTIV/46/, NEXTV/47/, NFCALL/6/,
C    3     NFGCAL/7/, NGCALL/30/, NITER/31/, PERM/58/, RADINC/8/,       
C    4     RESTOR/9/, STEP/40/, STGLIM/11/, TOOBIG/2/, VNEED/4/, W/34/, 
C    5     XIRC/13/, X0/43/                                             
C/7                                                                     
      PARAMETER (CNVCOD=55, DG=37, DTOL=59, DTYPE=16, IRC=29, IVNEED=3, 
     1           KAGQT=33, LMAT=42, MODE=35, MODEL=5, MXFCAL=17,        
     2           MXITER=18, N0=41, NC=48, NEXTIV=46, NEXTV=47, NFCALL=6,
     3           NFGCAL=7, NGCALL=30, NITER=31, PERM=58, RADINC=8,      
     4           RESTOR=9, STEP=40, STGLIM=11, TOOBIG=2, VNEED=4, W=34, 
     5           XIRC=13, X0=43)                                        
C/                                                                      
C                                                                       
C  ***  V SUBSCRIPT VALUES  ***                                         
C                                                                       
C/6                                                                     
C     DATA DGNORM/1/, DINIT/38/, DSTNRM/2/, DTINIT/39/, D0INIT/40/,     
C    1     F/10/, F0/13/, FDIF/11/, GTSTEP/4/, INCFAC/23/, LMAX0/35/,   
C    2     LMAXS/36/, PHMXFC/21/, PREDUC/7/, RADFAC/16/, RADIUS/8/,     
C    3     RAD0/9/, RELDX/17/, STPPAR/5/, TUNER4/29/, TUNER5/30/        
C/7                                                                     
      PARAMETER (DGNORM=1, DINIT=38, DSTNRM=2, DTINIT=39, D0INIT=40,    
     1           F=10, F0=13, FDIF=11, GTSTEP=4, INCFAC=23, LMAX0=35,   
     2           LMAXS=36, PHMXFC=21, PREDUC=7, RADFAC=16, RADIUS=8,    
     3           RAD0=9, RELDX=17, STPPAR=5, TUNER4=29, TUNER5=30)      
C/                                                                      
C                                                                       
C/6                                                                     
C     DATA NEGONE/-1.D+0/, ONE/1.D+0/, ONEP2/1.2D+0/, ZERO/0.D+0/       
C/7                                                                     
      PARAMETER (NEGONE=-1.D+0, ONE=1.D+0, ONEP2=1.2D+0, ZERO=0.D+0)    
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      I = IV(1)                                                         
      IF (I .EQ. 1) GO TO 50                                            
      IF (I .EQ. 2) GO TO 60                                            
C                                                                       
C  ***  CHECK VALIDITY OF IV AND V INPUT VALUES  ***                    
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(2, IV, LIV, LV, V)                  
      IF (IV(1) .LT. 12) GO TO 10                                       
      IF (IV(1) .GT. 13) GO TO 10                                       
         IV(VNEED) = IV(VNEED) + N*(N+27)/2 + 7                         
         IV(IVNEED) = IV(IVNEED) + 3*N                                  
 10   CALL DPARCK(2, D, IV, LIV, LV, N, V)                              
      I = IV(1) - 2                                                     
      IF (I .GT. 12) GO TO 999                                          
      NN1O2 = N * (N + 1) / 2                                           
      IF (LH .GE. NN1O2) GO TO (250,250,250,250,250,250,190,150,190,    
     1                          20,20,30), I                            
         IV(1) = 81                                                     
         GO TO 440                                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
 20   IV(DTOL) = IV(LMAT) + NN1O2                                       
      IV(X0) = IV(DTOL) + 2*N                                           
      IV(STEP) = IV(X0) + 2*N                                           
      IV(DG) = IV(STEP) + 3*N                                           
      IV(W) = IV(DG) + 2*N                                              
      IV(NEXTV) = IV(W) + 4*N + 7                                       
      IV(NEXTIV) = IV(PERM) + 3*N                                       
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
      IV(MODEL) = 1                                                     
      IV(STGLIM) = 1                                                    
      IV(TOOBIG) = 0                                                    
      IV(CNVCOD) = 0                                                    
      IV(RADINC) = 0                                                    
      IV(NC) = N                                                        
      V(RAD0) = ZERO                                                    
      V(STPPAR) = ZERO                                                  
      IF (V(DINIT) .GE. ZERO) CALL DV7SCP(N, D, V(DINIT))               
      K = IV(DTOL)                                                      
      IF (V(DTINIT) .GT. ZERO) CALL DV7SCP(N, V(K), V(DTINIT))          
      K = K + N                                                         
      IF (V(D0INIT) .GT. ZERO) CALL DV7SCP(N, V(K), V(D0INIT))          
C                                                                       
C  ***  CHECK CONSISTENCY OF B AND INITIALIZE IP ARRAY  ***             
C                                                                       
      IPI = IV(PERM)                                                    
      DO 40 I = 1, N                                                    
         IV(IPI) = I                                                    
         IPI = IPI + 1                                                  
         IF (B(1,I) .GT. B(2,I)) GO TO 420                              
 40      CONTINUE                                                       
C                                                                       
C  ***  GET INITIAL FUNCTION VALUE  ***                                 
C                                                                       
      IV(1) = 1                                                         
      GO TO 450                                                         
C                                                                       
 50   V(F) = FX                                                         
      IF (IV(MODE) .GE. 0) GO TO 250                                    
      IV(1) = 2                                                         
      IF (IV(TOOBIG) .EQ. 0) GO TO 999                                  
         IV(1) = 63                                                     
         GO TO 440                                                      
C                                                                       
C  ***  MAKE SURE GRADIENT COULD BE COMPUTED  ***                       
C                                                                       
 60   IF (IV(TOOBIG) .EQ. 0) GO TO 70                                   
         IV(1) = 65                                                     
         GO TO 440                                                      
C                                                                       
C  ***  UPDATE THE SCALE VECTOR D  ***                                  
C                                                                       
 70   DG1 = IV(DG)                                                      
      IF (IV(DTYPE) .LE. 0) GO TO 90                                    
      K = DG1                                                           
      J = 0                                                             
      DO 80 I = 1, N                                                    
         J = J + I                                                      
         V(K) = H(J)                                                    
         K = K + 1                                                      
 80      CONTINUE                                                       
      CALL DD7DUP(D, V(DG1), IV, LIV, LV, N, V)                         
C                                                                       
C  ***  COMPUTE SCALED GRADIENT AND ITS NORM  ***                       
C                                                                       
 90   DG1 = IV(DG)                                                      
      CALL DV7VMP(N, V(DG1), G, D, -1)                                  
C                                                                       
C  ***  COMPUTE SCALED HESSIAN  ***                                     
C                                                                       
      K = 1                                                             
      DO 110 I = 1, N                                                   
         T = ONE / D(I)                                                 
         DO 100 J = 1, I                                                
              H(K) = T * H(K) / D(J)                                    
              K = K + 1                                                 
 100          CONTINUE                                                  
 110     CONTINUE                                                       
C                                                                       
C  ***  CHOOSE INITIAL PERMUTATION  ***                                 
C                                                                       
      IPI = IV(PERM)                                                    
      IPN = IPI + N                                                     
      IPIV2 = IPN - 1                                                   
C     *** INVERT OLD PERMUTATION ARRAY ***                              
      CALL I7PNVR(N, IV(IPN), IV(IPI))                                  
      K = IV(NC)                                                        
      DO 130 I = 1, N                                                   
         IF (B(1,I) .GE. B(2,I)) GO TO 120                              
         XI = X(I)                                                      
         GI = G(I)                                                      
         IF (XI .LE. B(1,I) .AND. GI .GT. ZERO) GO TO 120               
         IF (XI .GE. B(2,I) .AND. GI .LT. ZERO) GO TO 120               
            IV(IPI) = I                                                 
            IPI = IPI + 1                                               
            J = IPIV2 + I                                               
C           *** DISALLOW CONVERGENCE IF X(I) HAS JUST BEEN FREED ***    
            IF (IV(J) .GT. K) IV(CNVCOD) = 0                            
            GO TO 130                                                   
 120     IPN = IPN - 1                                                  
         IV(IPN) = I                                                    
 130     CONTINUE                                                       
      IV(NC) = IPN - IV(PERM)                                           
C                                                                       
C  ***  PERMUTE SCALED GRADIENT AND HESSIAN ACCORDINGLY  ***            
C                                                                       
      IPI = IV(PERM)                                                    
      CALL DS7IPR(N, IV(IPI), H)                                        
      CALL DV7IPR(N, IV(IPI), V(DG1))                                   
      V(DGNORM) = ZERO                                                  
      IF (IV(NC) .GT. 0) V(DGNORM) = DV2NRM(IV(NC), V(DG1))             
C                                                                       
      IF (IV(CNVCOD) .NE. 0) GO TO 430                                  
      IF (IV(MODE) .EQ. 0) GO TO 380                                    
C                                                                       
C  ***  ALLOW FIRST STEP TO HAVE SCALED 2-NORM AT MOST V(LMAX0)  ***    
C                                                                       
      V(RADIUS) = V(LMAX0) / (ONE + V(PHMXFC))                          
C                                                                       
      IV(MODE) = 0                                                      
C                                                                       
C                                                                       
C-----------------------------  MAIN LOOP  -----------------------------
C                                                                       
C                                                                       
C  ***  PRINT ITERATION SUMMARY, CHECK ITERATION LIMIT  ***             
C                                                                       
 140  CALL DITSUM(D, G, IV, LIV, LV, N, V, X)                           
 150  K = IV(NITER)                                                     
      IF (K .LT. IV(MXITER)) GO TO 160                                  
         IV(1) = 10                                                     
         GO TO 440                                                      
C                                                                       
 160  IV(NITER) = K + 1                                                 
C                                                                       
C  ***  INITIALIZE FOR START OF NEXT ITERATION  ***                     
C                                                                       
      X01 = IV(X0)                                                      
      V(F0) = V(F)                                                      
      IV(IRC) = 4                                                       
      IV(KAGQT) = -1                                                    
C                                                                       
C     ***  COPY X TO X0  ***                                            
C                                                                       
      CALL DV7CPY(N, V(X01), X)                                         
C                                                                       
C  ***  UPDATE RADIUS  ***                                              
C                                                                       
      IF (K .EQ. 0) GO TO 180                                           
      STEP1 = IV(STEP)                                                  
      K = STEP1                                                         
      DO 170 I = 1, N                                                   
         V(K) = D(I) * V(K)                                             
         K = K + 1                                                      
 170     CONTINUE                                                       
      T = V(RADFAC) * DV2NRM(N, V(STEP1))                               
      IF (V(RADFAC) .LT. ONE .OR. T .GT. V(RADIUS)) V(RADIUS) = T       
C                                                                       
C  ***  CHECK STOPX AND FUNCTION EVALUATION LIMIT  ***                  
C                                                                       
 180  IF (.NOT. STOPX(DUMMY)) GO TO 200                                 
         IV(1) = 11                                                     
         GO TO 210                                                      
C                                                                       
C     ***  COME HERE WHEN RESTARTING AFTER FUNC. EVAL. LIMIT OR STOPX.  
C                                                                       
 190  IF (V(F) .GE. V(F0)) GO TO 200                                    
         V(RADFAC) = ONE                                                
         K = IV(NITER)                                                  
         GO TO 160                                                      
C                                                                       
 200  IF (IV(NFCALL) .LT. IV(MXFCAL)) GO TO 220                         
         IV(1) = 9                                                      
 210     IF (V(F) .GE. V(F0)) GO TO 440                                 
C                                                                       
C        ***  IN CASE OF STOPX OR FUNCTION EVALUATION LIMIT WITH        
C        ***  IMPROVED V(F), EVALUATE THE GRADIENT AT X.                
C                                                                       
              IV(CNVCOD) = IV(1)                                        
              GO TO 370                                                 
C                                                                       
C. . . . . . . . . . . . .  COMPUTE CANDIDATE STEP  . . . . . . . . . . 
C                                                                       
 220  STEP1 = IV(STEP)                                                  
      L = IV(LMAT)                                                      
      W1 = IV(W)                                                        
      IPI = IV(PERM)                                                    
      IPN = IPI + N                                                     
      IPIV2 = IPN + N                                                   
      TG1 = IV(DG)                                                      
      TD1 = TG1 + N                                                     
      X01 = IV(X0)                                                      
      X11 = X01 + N                                                     
      CALL DG7QSB(B, D, H, G, IV(IPI), IV(IPN), IV(IPIV2), IV(KAGQT),   
     1            V(L), LV, N, IV(N0), IV(NC), V(STEP1), V(TD1), V(TG1),
     2            V, V(W1), V(X11), V(X01))                             
      IF (IV(IRC) .NE. 6) GO TO 230                                     
         IF (IV(RESTOR) .NE. 2) GO TO 250                               
         RSTRST = 2                                                     
         GO TO 260                                                      
C                                                                       
C  ***  CHECK WHETHER EVALUATING F(X0 + STEP) LOOKS WORTHWHILE  ***     
C                                                                       
 230  IV(TOOBIG) = 0                                                    
      IF (V(DSTNRM) .LE. ZERO) GO TO 250                                
      IF (IV(IRC) .NE. 5) GO TO 240                                     
      IF (V(RADFAC) .LE. ONE) GO TO 240                                 
      IF (V(PREDUC) .GT. ONEP2 * V(FDIF)) GO TO 240                     
         IF (IV(RESTOR) .NE. 2) GO TO 250                               
         RSTRST = 0                                                     
         GO TO 260                                                      
C                                                                       
C  ***  COMPUTE F(X0 + STEP)  ***                                       
C                                                                       
 240  CALL DV2AXY(N, X, ONE, V(STEP1), V(X01))                          
      IV(NFCALL) = IV(NFCALL) + 1                                       
      IV(1) = 1                                                         
      GO TO 450                                                         
C                                                                       
C. . . . . . . . . . . . .  ASSESS CANDIDATE STEP  . . . . . . . . . . .
C                                                                       
 250  RSTRST = 3                                                        
 260  X01 = IV(X0)                                                      
      V(RELDX) = DRLDST(N, D, X, V(X01))                                
      CALL DA7SST(IV, LIV, LV, V)                                       
      STEP1 = IV(STEP)                                                  
      LSTGST = STEP1 + 2*N                                              
      I = IV(RESTOR) + 1                                                
      GO TO (300, 270, 280, 290), I                                     
 270  CALL DV7CPY(N, X, V(X01))                                         
      GO TO 300                                                         
 280   CALL DV7CPY(N, V(LSTGST), X)                                     
       GO TO 300                                                        
 290     CALL DV7CPY(N, X, V(LSTGST))                                   
         CALL DV2AXY(N, V(STEP1), NEGONE, V(X01), X)                    
         V(RELDX) = DRLDST(N, D, X, V(X01))                             
         IV(RESTOR) = RSTRST                                            
C                                                                       
 300  K = IV(IRC)                                                       
      GO TO (310,340,340,340,310,320,330,330,330,330,330,330,410,380), K
C                                                                       
C     ***  RECOMPUTE STEP WITH NEW RADIUS  ***                          
C                                                                       
 310     V(RADIUS) = V(RADFAC) * V(DSTNRM)                              
         GO TO 180                                                      
C                                                                       
C  ***  COMPUTE STEP OF LENGTH V(LMAXS) FOR SINGULAR CONVERGENCE TEST.  
C                                                                       
 320  V(RADIUS) = V(LMAXS)                                              
      GO TO 220                                                         
C                                                                       
C  ***  CONVERGENCE OR FALSE CONVERGENCE  ***                           
C                                                                       
 330  IV(CNVCOD) = K - 4                                                
      IF (V(F) .GE. V(F0)) GO TO 430                                    
         IF (IV(XIRC) .EQ. 14) GO TO 430                                
              IV(XIRC) = 14                                             
C                                                                       
C. . . . . . . . . . . .  PROCESS ACCEPTABLE STEP  . . . . . . . . . . .
C                                                                       
 340  IF (IV(IRC) .NE. 3) GO TO 370                                     
         TEMP1 = LSTGST                                                 
C                                                                       
C     ***  PREPARE FOR GRADIENT TESTS  ***                              
C     ***  SET  TEMP1 = HESSIAN * STEP + G(X0)                          
C     ***             = DIAG(D) * (H * STEP + G(X0))                    
C                                                                       
         K = TEMP1                                                      
         STEP0 = STEP1 - 1                                              
         IPI = IV(PERM)                                                 
         DO 350 I = 1, N                                                
              J = IV(IPI)                                               
              IPI = IPI + 1                                             
              STEP1 = STEP0 + J                                         
              V(K) = D(J) * V(STEP1)                                    
              K = K + 1                                                 
 350          CONTINUE                                                  
C        USE X0 VECTOR AS TEMPORARY.                                    
         CALL DS7LVM(N, V(X01), H, V(TEMP1))                            
         TEMP0 = TEMP1 - 1                                              
         IPI = IV(PERM)                                                 
         DO 360 I = 1, N                                                
              J = IV(IPI)                                               
              IPI = IPI + 1                                             
              TEMP1 = TEMP0 + J                                         
              V(TEMP1) = D(J) * V(X01) + G(J)                           
              X01 = X01 + 1                                             
 360          CONTINUE                                                  
C                                                                       
C  ***  COMPUTE GRADIENT AND HESSIAN  ***                               
C                                                                       
 370  IV(NGCALL) = IV(NGCALL) + 1                                       
      IV(TOOBIG) = 0                                                    
      IV(1) = 2                                                         
      GO TO 450                                                         
C                                                                       
 380  IV(1) = 2                                                         
      IF (IV(IRC) .NE. 3) GO TO 140                                     
C                                                                       
C  ***  SET V(RADFAC) BY GRADIENT TESTS  ***                            
C                                                                       
      STEP1 = IV(STEP)                                                  
C     *** TEMP1 = STLSTG ***                                            
      TEMP1 = STEP1 + 2*N                                               
C                                                                       
C     ***  SET  TEMP1 = DIAG(D)**-1 * (HESSIAN*STEP + (G(X0)-G(X)))  ***
C                                                                       
      K = TEMP1                                                         
      DO 390 I = 1, N                                                   
         V(K) = (V(K) - G(I)) / D(I)                                    
         K = K + 1                                                      
 390     CONTINUE                                                       
C                                                                       
C     ***  DO GRADIENT TESTS  ***                                       
C                                                                       
      IF (DV2NRM(N, V(TEMP1)) .LE. V(DGNORM) * V(TUNER4)) GO TO 400     
           IF (DD7TPR(N, G, V(STEP1))                                   
     1               .GE. V(GTSTEP) * V(TUNER5))  GO TO 140             
 400            V(RADFAC) = V(INCFAC)                                   
                GO TO 140                                               
C                                                                       
C. . . . . . . . . . . . . .  MISC. DETAILS  . . . . . . . . . . . . . .
C                                                                       
C  ***  BAD PARAMETERS TO ASSESS  ***                                   
C                                                                       
 410  IV(1) = 64                                                        
      GO TO 440                                                         
C                                                                       
C  ***  INCONSISTENT B  ***                                             
C                                                                       
 420  IV(1) = 82                                                        
      GO TO 440                                                         
C                                                                       
C  ***  PRINT SUMMARY OF FINAL ITERATION AND OTHER REQUESTED ITEMS  *** 
C                                                                       
 430  IV(1) = IV(CNVCOD)                                                
      IV(CNVCOD) = 0                                                    
 440  CALL DITSUM(D, G, IV, LIV, LV, N, V, X)                           
      GO TO 999                                                         
C                                                                       
C  ***  PROJECT X INTO FEASIBLE REGION (PRIOR TO COMPUTING F OR G)  *** 
C                                                                       
 450  DO 460 I = 1, N                                                   
         IF (X(I) .LT. B(1,I)) X(I) = B(1,I)                            
         IF (X(I) .GT. B(2,I)) X(I) = B(2,I)                            
 460     CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF DRMNHB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  DRNSG(A, ALF, C, DA, IN, IV, L, L1, LA, LIV, LV,      
     1                  N, NDA, P, V, Y)                                
C                                                                       
C  ***  ITERATION DRIVER FOR SEPARABLE NONLINEAR LEAST SQUARES.         
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER L, L1, LA, LIV, LV, N, NDA, P                             
      INTEGER IN(2,NDA), IV(LIV)                                        
C     DIMENSION UIPARM(*)                                               
      DOUBLE PRECISION A(LA,L1), ALF(P), C(L), DA(LA,NDA), V(LV), Y(N)  
C                                                                       
C  ***  PURPOSE  ***                                                    
C                                                                       
C GIVEN A SET OF N OBSERVATIONS Y(1)....Y(N) OF A DEPENDENT VARIABLE    
C T(1)...T(N),  DRNSG ATTEMPTS TO COMPUTE A LEAST SQUARES FIT           
C TO A FUNCTION  ETA  (THE MODEL) WHICH IS A LINEAR COMBINATION         
C                                                                       
C                  L                                                    
C ETA(C,ALF,T) =  SUM C * PHI(ALF,T) +PHI   (ALF,T)                     
C                 J=1  J     J           L+1                            
C                                                                       
C OF NONLINEAR FUNCTIONS PHI(J) DEPENDENT ON T AND ALF(1),...,ALF(P)    
C (.E.G. A SUM OF EXPONENTIALS OR GAUSSIANS).  THAT IS, IT DETERMINES   
C NONLINEAR PARAMETERS ALF WHICH MINIMIZE                               
C                                                                       
C                   2    N                      2                       
C     NORM(RESIDUAL)  = SUM  (Y - ETA(C,ALF,T )).                       
C                       I=1    I             I                          
C                                                                       
C THE (L+1)ST TERM IS OPTIONAL.                                         
C                                                                       
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
C      A (IN)  MATRIX PHI(ALF,T) OF THE MODEL.                          
C    ALF (I/O) NONLINEAR PARAMETERS.                                    
C                 INPUT = INITIAL GUESS,                                
C                 OUTPUT = BEST ESTIMATE FOUND.                         
C      C (OUT) LINEAR PARAMETERS (ESTIMATED).                           
C     DA (IN)  DERIVATIVES OF COLUMNS OF A WITH RESPECT TO COMPONENTS   
C                 OF ALF, AS SPECIFIED BY THE IN ARRAY...               
C     IN (IN)  WHEN  DRNSG IS CALLED WITH IV(1) = 2 OR -2, THEN FOR     
C                 I = 1(1)NDA, COLUMN I OF DA IS THE PARTIAL            
C                 DERIVATIVE WITH RESPECT TO ALF(IN(1,I)) OF COLUMN     
C                 IN(2,I) OF A, UNLESS IV(1,I) IS NOT POSITIVE (IN      
C                 WHICH CASE COLUMN I OF DA IS IGNORED.  IV(1) = -2     
C                 MEANS THERE ARE MORE COLUMNS OF DA TO COME AND        
C                  DRNSG SHOULD RETURN FOR THEM.                        
C     IV (I/O) INTEGER PARAMETER AND SCRATCH VECTOR.   DRNSG RETURNS    
C                 WITH IV(1) = 1 WHEN IT WANTS A TO BE EVALUATED AT     
C                 ALF AND WITH IV(1) = 2 WHEN IT WANTS DA TO BE         
C                 EVALUATED AT ALF.  WHEN CALLED WITH IV(1) = -2        
C                 (AFTER A RETURN WITH IV(1) = 2),  DRNSG RETURNS       
C                 WITH IV(1) = -2 TO GET MORE COLUMNS OF DA.            
C      L (IN)  NUMBER OF LINEAR PARAMETERS TO BE ESTIMATED.             
C     L1 (IN)  L+1 IF PHI(L+1) IS IN THE MODEL, L IF NOT.               
C     LA (IN)  LEAD DIMENSION OF A.  MUST BE AT LEAST N.                
C    LIV (IN)  LENGTH OF IV.  MUST BE AT LEAST 110 + L + P.             
C     LV (IN)  LENGTH OF V.  MUST BE AT LEAST                           
C                 105 + 2*N + JLEN + L*(L+3)/2 + P*(2*P + 17),          
C                 WHERE  JLEN = (L+P)*(N+L+P+1),  UNLESS NEITHER A      
C                 COVARIANCE MATRIX NOR REGRESSION DIAGNOSTICS ARE      
C                 REQUESTED, IN WHICH CASE  JLEN = N*P.                 
C      N (IN)  NUMBER OF OBSERVATIONS.                                  
C    NDA (IN)  NUMBER OF COLUMNS IN DA AND IN.                          
C      P (IN)  NUMBER OF NONLINEAR PARAMETERS TO BE ESTIMATED.          
C      V (I/O) FLOATING-POINT PARAMETER AND SCRATCH VECTOR.             
C      Y (IN)  RIGHT-HAND SIDE VECTOR.                                  
C                                                                       
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      DOUBLE PRECISION DD7TPR, DL7SVX, DL7SVN, DR7MDC                   
      EXTERNAL DC7VFN,DIVSET, DD7TPR,DITSUM, DL7ITV,DL7SRT, DL7SVX,     
     1         DL7SVN, DN2CVP, DN2LRD, DN2RDP,  DRN2G, DQ7APL,DQ7RAD,   
     2        DQ7RFH, DR7MDC, DS7CPR,DV2AXY,DV7CPY,DV7PRM, DV7SCL,      
     3         DV7SCP                                                   
C                                                                       
C DC7VFN... FINISHES COVARIANCE COMPUTATION.                            
C DIVSET.... SUPPLIES DEFAULT PARAMETER VALUES.                         
C DD7TPR... RETURNS INNER PRODUCT OF TWO VECTORS.                       
C DITSUM.... PRINTS ITERATION SUMMARY, INITIAL AND FINAL ALF.           
C DL7ITV... APPLIES INVERSE-TRANSPOSE OF COMPACT LOWER TRIANG. MATRIX.  
C DL7SRT.... COMPUTES (PARTIAL) CHOLESKY FACTORIZATION.                 
C DL7SVX... ESTIMATES LARGEST SING. VALUE OF LOWER TRIANG. MATRIX.      
C DL7SVN... ESTIMATES SMALLEST SING. VALUE OF LOWER TRIANG. MATRIX.     
C DN2CVP... PRINTS COVARIANCE MATRIX.                                   
C DN2LRD... COMPUTES COVARIANCE AND REGRESSION DIAGNOSTICS.             
C DN2RDP... PRINTS REGRESSION DIAGNOSTICS.                              
C  DRN2G... UNDERLYING NONLINEAR LEAST-SQUARES SOLVER.                  
C DQ7APL... APPLIES HOUSEHOLDER TRANSFORMS STORED BY DQ7RFH.            
C DQ7RFH.... COMPUTES QR FACT. VIA HOUSEHOLDER TRANSFORMS WITH PIVOTING.
C DQ7RAD.... QR FACT., NO PIVOTING.                                     
C DR7MDC... RETURNS MACHINE-DEP. CONSTANTS.                             
C DS7CPR... PRINTS LINEAR PARAMETERS AT SOLUTION.                       
C DV2AXY.... ADDS MULTIPLE OF ONE VECTOR TO ANOTHER.                    
C DV7CPY.... COPIES ONE VECTOR TO ANOTHER.                              
C DV7PRM.... PERMUTES A VECTOR.                                         
C DV7SCL... SCALES AND COPIES ONE VECTOR TO ANOTHER.                    
C DV7SCP... SETS ALL COMPONENTS OF A VECTOR TO A SCALAR.                
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      LOGICAL NOCOV                                                     
      INTEGER AR1, CSAVE1, D1, DR1, DR1L, DRI, DRI1, FDH0, HSAVE, I, I1,
     1        IPIV1, IER, IV1, J1, JLEN, K, LH, LI, LL1O2, MD, N1,      
     2        NML, NRAN, PP, PP1, R1, R1L, RD1, TEMP1                   
      DOUBLE PRECISION SINGTL, T                                        
      DOUBLE PRECISION MACHEP, NEGONE, SNGFAC, ZERO                     
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER AR, CNVCOD, COVMAT, COVREQ, CSAVE, CVRQSV, D, FDH, H,     
     1        IERS, IPIVS, IV1SAV, IVNEED, J, LMAT, MODE, NEXTIV, NEXTV,
     2        NFCALL, NFCOV, NFGCAL, NGCALL, NGCOV, PERM, R, RCOND,     
     3        RDREQ, RDRQSV, REGD, REGD0, RESTOR, TOOBIG, VNEED         
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C/6                                                                     
C     DATA AR/110/, CNVCOD/55/, COVMAT/26/, COVREQ/15/, CSAVE/105/,     
C    1     CVRQSV/106/, D/27/, FDH/74/, H/56/, IERS/108/, IPIVS/109/,   
C    2     IV1SAV/104/, IVNEED/3/, J/70/, LMAT/42/, MODE/35/,           
C    3     NEXTIV/46/, NEXTV/47/, NFCALL/6/, NFCOV/52/, NFGCAL/7/,      
C    4     NGCALL/30/, NGCOV/53/, PERM/58/, R/61/, RCOND/53/, RDREQ/57/,
C    5     RDRQSV/107/, REGD/67/, REGD0/82/, RESTOR/9/, TOOBIG/2/,      
C    6     VNEED/4/                                                     
C/7                                                                     
      PARAMETER (AR=110, CNVCOD=55, COVMAT=26, COVREQ=15, CSAVE=105,    
     1           CVRQSV=106, D=27, FDH=74, H=56, IERS=108, IPIVS=109,   
     2           IV1SAV=104, IVNEED=3, J=70, LMAT=42, MODE=35,          
     3           NEXTIV=46, NEXTV=47, NFCALL=6, NFCOV=52, NFGCAL=7,     
     4           NGCALL=30, NGCOV=53, PERM=58, R=61, RCOND=53, RDREQ=57,
     5           RDRQSV=107, REGD=67, REGD0=82, RESTOR=9, TOOBIG=2,     
     6           VNEED=4)                                               
C/                                                                      
      DATA MACHEP/-1.D+0/, NEGONE/-1.D+0/, SNGFAC/1.D+2/, ZERO/0.D+0/   
C                                                                       
C++++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++ 
C                                                                       
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)                  
      N1 = 1                                                            
      NML = N                                                           
      IV1 = IV(1)                                                       
      IF (IV1 .LE. 2) GO TO 20                                          
C                                                                       
C  ***  CHECK INPUT INTEGERS  ***                                       
C                                                                       
      IF (P .LE. 0) GO TO 370                                           
      IF (L .LT. 0) GO TO 370                                           
      IF (N .LE. L) GO TO 370                                           
      IF (LA .LT. N) GO TO 370                                          
      IF (IV1 .LT. 12) GO TO 20                                         
      IF (IV1 .EQ. 14) GO TO 20                                         
      IF (IV1 .EQ. 12) IV(1) = 13                                       
C                                                                       
C  ***  FRESH START -- COMPUTE STORAGE REQUIREMENTS  ***                
C                                                                       
      IF (IV(1) .GT. 16) GO TO 370                                      
      LL1O2 = L*(L+1)/2                                                 
      JLEN = N*P                                                        
      I = L + P                                                         
      IF (IV(RDREQ) .GT. 0 .AND. IV(COVREQ) .NE. 0) JLEN = I*(N + I + 1)
      IF (IV(1) .NE. 13) GO TO 10                                       
         IV(IVNEED) = IV(IVNEED) + L                                    
         IV(VNEED) = IV(VNEED) + P + 2*N + JLEN + LL1O2 + L             
 10   IF (IV(PERM) .LE. AR) IV(PERM) = AR + 1                           
      CALL  DRN2G(V, V, IV, LIV, LV, N, N, N1, NML, P, V, V, V, ALF)    
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(IPIVS) = IV(NEXTIV)                                            
      IV(NEXTIV) = IV(NEXTIV) + L                                       
      IV(D) = IV(NEXTV)                                                 
      IV(REGD0) = IV(D) + P                                             
      IV(AR) = IV(REGD0) + N                                            
      IV(CSAVE) = IV(AR) + LL1O2                                        
      IV(J) = IV(CSAVE) + L                                             
      IV(R) = IV(J) + JLEN                                              
      IV(NEXTV) = IV(R) + N                                             
      IV(IERS) = 0                                                      
      IF (IV1 .EQ. 13) GO TO 999                                        
C                                                                       
C  ***  SET POINTERS INTO IV AND V  ***                                 
C                                                                       
 20   AR1 = IV(AR)                                                      
      D1 = IV(D)                                                        
      DR1 = IV(J)                                                       
      DR1L = DR1 + L                                                    
      R1 = IV(R)                                                        
      R1L = R1 + L                                                      
      RD1 = IV(REGD0)                                                   
      CSAVE1 = IV(CSAVE)                                                
      NML = N - L                                                       
      IF (IV1 .LE. 2) GO TO 50                                          
C                                                                       
C  ***  IF F.D. HESSIAN WILL BE NEEDED (FOR COVARIANCE OR REG.          
C  ***  DIAGNOSTICS), HAVE  DRN2G COMPUTE ONLY THE PART CORRESP.        
C  ***  TO ALF WITH C FIXED...                                          
C                                                                       
      IF (L .LE. 0) GO TO 30                                            
      IV(CVRQSV) = IV(COVREQ)                                           
      IF (IABS(IV(COVREQ)) .GE. 3) IV(COVREQ) = 0                       
      IV(RDRQSV) = IV(RDREQ)                                            
      IF (IV(RDREQ) .GT. 0) IV(RDREQ) = -1                              
C                                                                       
 30   CALL  DRN2G(V(D1), V(DR1L), IV, LIV, LV, NML, N, N1, NML, P,      
     1            V(R1L), V(RD1), V, ALF)                               
      IF (IABS(IV(RESTOR)-2) .EQ. 1 .AND. L .GT. 0)                     
     1        CALL DV7CPY(L, C, V(CSAVE1))                              
      IV1 = IV(1)                                                       
      IF (IV1-2) 40, 150, 230                                           
C                                                                       
C  ***  NEW FUNCTION VALUE (RESIDUAL) NEEDED  ***                       
C                                                                       
 40   IV(IV1SAV) = IV(1)                                                
      IV(1) = IABS(IV1)                                                 
      IF (IV(RESTOR) .EQ. 2 .AND. L .GT. 0) CALL DV7CPY(L, V(CSAVE1), C)
      GO TO 999                                                         
C                                                                       
C  ***  COMPUTE NEW RESIDUAL OR GRADIENT  ***                           
C                                                                       
 50   IV(1) = IV(IV1SAV)                                                
      MD = IV(MODE)                                                     
      IF (MD .LE. 0) GO TO 60                                           
         NML = N                                                        
         DR1L = DR1                                                     
         R1L = R1                                                       
 60   IF (IV(TOOBIG) .NE. 0) GO TO 30                                   
      IF (IABS(IV1) .EQ. 2) GO TO 170                                   
C                                                                       
C  ***  COMPUTE NEW RESIDUAL  ***                                       
C                                                                       
      IF (L1 .LE. L) CALL DV7CPY(N, V(R1), Y)                           
      IF (L1 .GT. L) CALL DV2AXY(N, V(R1), NEGONE, A(1,L1), Y)          
      IF (MD .GT. 0) GO TO 120                                          
      IER = 0                                                           
      IF (L .LE. 0) GO TO 110                                           
      LL1O2 = L * (L + 1) / 2                                           
      IPIV1 = IV(IPIVS)                                                 
      CALL DQ7RFH(IER, IV(IPIV1), N, LA, 0, L, A, V(AR1), LL1O2, C)     
C                                                                       
C *** DETERMINE NUMERICAL RANK OF A ***                                 
C                                                                       
      IF (MACHEP .LE. ZERO) MACHEP = DR7MDC(3)                          
      SINGTL = SNGFAC * FLOAT(MAX0(L,N)) * MACHEP                       
      K = L                                                             
      IF (IER .NE. 0) K = IER - 1                                       
 70   IF (K .LE. 0) GO TO 90                                            
         T = DL7SVX(K, V(AR1), C, C)                                    
         IF (T .GT. ZERO) T = DL7SVN(K, V(AR1), C, C) / T               
         IF (T .GT. SINGTL) GO TO 80                                    
         K = K - 1                                                      
         GO TO 70                                                       
C                                                                       
C *** RECORD RANK IN IV(IERS)... IV(IERS) = 0 MEANS FULL RANK,          
C *** IV(IERS) .GT. 0 MEANS RANK IV(IERS) - 1.                          
C                                                                       
 80   IF (K .GE. L) GO TO 100                                           
 90      IER = K + 1                                                    
         CALL DV7SCP(L-K, C(K+1), ZERO)                                 
 100  IV(IERS) = IER                                                    
      IF (K .LE. 0) GO TO 110                                           
C                                                                       
C *** APPLY HOUSEHOLDER TRANSFORMATONS TO RESIDUALS...                  
C                                                                       
      CALL DQ7APL(LA, N, K, A, V(R1), IER)                              
C                                                                       
C *** COMPUTING C NOW MAY SAVE A FUNCTION EVALUATION AT                 
C *** THE LAST ITERATION.                                               
C                                                                       
      CALL DL7ITV(K, C, V(AR1), V(R1))                                  
      CALL DV7PRM(L, IV(IPIV1), C)                                      
C                                                                       
 110  IF(IV(1) .LT. 2) GO TO 220                                        
      GO TO 999                                                         
C                                                                       
C                                                                       
C  ***  RESIDUAL COMPUTATION FOR F.D. HESSIAN  ***                      
C                                                                       
 120  IF (L .LE. 0) GO TO 140                                           
      DO 130 I = 1, L                                                   
 130     CALL DV2AXY(N, V(R1), -C(I), A(1,I), V(R1))                    
 140  IF (IV(1) .GT. 0) GO TO 30                                        
         IV(1) = 2                                                      
         GO TO 160                                                      
C                                                                       
C  ***  NEW GRADIENT (JACOBIAN) NEEDED  ***                             
C                                                                       
 150  IV(IV1SAV) = IV1                                                  
      IF (IV(NFGCAL) .NE. IV(NFCALL)) IV(1) = 1                         
 160  CALL DV7SCP(N*P, V(DR1), ZERO)                                    
      GO TO 999                                                         
C                                                                       
C  ***  COMPUTE NEW JACOBIAN  ***                                       
C                                                                       
 170  NOCOV = MD .LE. P .OR. IABS(IV(COVREQ)) .GE. 3                    
      FDH0 = DR1 + N*(P+L)                                              
      IF (NDA .LE. 0) GO TO 370                                         
      DO 180 I = 1, NDA                                                 
         I1 = IN(1,I) - 1                                               
         IF (I1 .LT. 0) GO TO 180                                       
         J1 = IN(2,I)                                                   
         K = DR1 + I1*N                                                 
         T = NEGONE                                                     
         IF (J1 .LE. L) T = -C(J1)                                      
         CALL DV2AXY(N, V(K), T, DA(1,I), V(K))                         
         IF (NOCOV) GO TO 180                                           
         IF (J1 .GT. L) GO TO 180                                       
C        ***  ADD IN (L,P) PORTION OF SECOND-ORDER PART OF HESSIAN      
C        ***  FOR COVARIANCE OR REG. DIAG. COMPUTATIONS...              
         J1 = J1 + P                                                    
         K = FDH0 + J1*(J1-1)/2 + I1                                    
         V(K) = V(K) - DD7TPR(N, V(R1), DA(1,I))                        
 180     CONTINUE                                                       
      IF (IV1 .EQ. 2) GO TO 190                                         
         IV(1) = IV1                                                    
         GO TO 999                                                      
 190  IF (L .LE. 0) GO TO 30                                            
      IF (MD .GT. P) GO TO 240                                          
      IF (MD .GT. 0) GO TO 30                                           
      K = DR1                                                           
      IER = IV(IERS)                                                    
      NRAN = L                                                          
      IF (IER .GT. 0) NRAN = IER - 1                                    
      IF (NRAN .LE. 0) GO TO 210                                        
      DO 200 I = 1, P                                                   
         CALL DQ7APL(LA, N, NRAN, A, V(K), IER)                         
         K = K + N                                                      
 200     CONTINUE                                                       
 210  CALL DV7CPY(L, V(CSAVE1), C)                                      
 220  IF (IER .EQ. 0) GO TO 30                                          
C                                                                       
C     *** ADJUST SUBSCRIPTS DESCRIBING R AND DR...                      
C                                                                       
         NRAN = IER - 1                                                 
         DR1L = DR1 + NRAN                                              
         NML = N - NRAN                                                 
         R1L = R1 + NRAN                                                
         GO TO 30                                                       
C                                                                       
C  ***  CONVERGENCE OR LIMIT REACHED  ***                               
C                                                                       
 230  IF (L .LE. 0) GO TO 350                                           
      IV(COVREQ) = IV(CVRQSV)                                           
      IV(RDREQ) = IV(RDRQSV)                                            
      IF (IV(1) .GT. 6) GO TO 360                                       
      IF (MOD(IV(RDREQ),4) .EQ. 0) GO TO 360                            
      IF (IV(FDH) .LE. 0 .AND. IABS(IV(COVREQ)) .LT. 3) GO TO 360       
      IF (IV(REGD) .GT. 0) GO TO 360                                    
      IF (IV(COVMAT) .GT. 0) GO TO 360                                  
C                                                                       
C  *** PREPARE TO FINISH COMPUTING COVARIANCE MATRIX AND REG. DIAG. *** 
C                                                                       
      PP = L + P                                                        
      I = 0                                                             
      IF (MOD(IV(RDREQ),4) .GE. 2) I = 1                                
      IF (MOD(IV(RDREQ),2) .EQ. 1 .AND. IABS(IV(COVREQ)) .EQ. 1) I = I+2
      IV(MODE) = PP + I                                                 
      I = DR1 + N*PP                                                    
      K = P * (P + 1) / 2                                               
      I1 = IV(LMAT)                                                     
      CALL DV7CPY(K, V(I), V(I1))                                       
      I = I + K                                                         
      CALL DV7SCP(PP*(PP+1)/2 - K, V(I), ZERO)                          
      IV(NFCOV) = IV(NFCOV) + 1                                         
      IV(NFCALL) = IV(NFCALL) + 1                                       
      IV(NFGCAL) = IV(NFCALL)                                           
      IV(CNVCOD) = IV(1)                                                
      IV(IV1SAV) = -1                                                   
      IV(1) = 1                                                         
      IV(NGCALL) = IV(NGCALL) + 1                                       
      IV(NGCOV) = IV(NGCOV) + 1                                         
      GO TO 999                                                         
C                                                                       
C  ***  FINISH COVARIANCE COMPUTATION  ***                              
C                                                                       
 240  I = DR1 + N*P                                                     
      DO 250 I1 = 1, L                                                  
         CALL DV7SCL(N, V(I), NEGONE, A(1,I1))                          
         I = I + N                                                      
 250     CONTINUE                                                       
      PP = L + P                                                        
      HSAVE = IV(H)                                                     
      K = DR1 + N*PP                                                    
      LH = PP * (PP + 1) / 2                                            
      IF (IABS(IV(COVREQ)) .LT. 3) GO TO 270                            
      I = IV(MODE) - 4                                                  
      IF (I .GE. PP) GO TO 260                                          
      CALL DV7SCP(LH, V(K), ZERO)                                       
      CALL DQ7RAD(N, N, PP, V, .FALSE., V(K), V(DR1), V)                
      IV(MODE) = I + 8                                                  
      IV(1) = 2                                                         
      IV(NGCALL) = IV(NGCALL) + 1                                       
      IV(NGCOV) = IV(NGCOV) + 1                                         
      GO TO 160                                                         
C                                                                       
 260  IV(MODE) = I                                                      
      GO TO 300                                                         
C                                                                       
 270  PP1 = P + 1                                                       
      DRI = DR1 + N*P                                                   
      LI = K + P*PP1/2                                                  
      DO 290 I = PP1, PP                                                
         DRI1 = DR1                                                     
         DO 280 I1 = 1, I                                               
            V(LI) = V(LI) + DD7TPR(N, V(DRI), V(DRI1))                  
            LI = LI + 1                                                 
            DRI1 = DRI1 + N                                             
 280        CONTINUE                                                    
         DRI = DRI + N                                                  
 290     CONTINUE                                                       
      CALL DL7SRT(PP1, PP, V(K), V(K), I)                               
      IF (I .NE. 0) GO TO 310                                           
 300  TEMP1 = K + LH                                                    
      T = DL7SVN(PP, V(K), V(TEMP1), V(TEMP1))                          
      IF (T .LE. ZERO) GO TO 310                                        
      T = T / DL7SVX(PP, V(K), V(TEMP1), V(TEMP1))                      
      V(RCOND) = T                                                      
      IF (T .GT. DR7MDC(4)) GO TO 320                                   
 310     IV(REGD) = -1                                                  
         IV(COVMAT) = -1                                                
         IV(FDH) = -1                                                   
         GO TO 340                                                      
 320  IV(H) = TEMP1                                                     
      IV(FDH) = IABS(HSAVE)                                             
      IF (IV(MODE) - PP .LT. 2) GO TO 330                               
         I = IV(H)                                                      
         CALL DV7SCP(LH, V(I), ZERO)                                    
 330  CALL DN2LRD(V(DR1), IV, V(K), LH, LIV, LV, N, N, PP, V(R1),       
     1            V(RD1), V)                                            
 340  CALL DC7VFN(IV, V(K), LH, LIV, LV, N, PP, V)                      
      IV(H) = HSAVE                                                     
C                                                                       
 350  IF (IV(REGD) .EQ. 1) IV(REGD) = RD1                               
 360  IF (IV(1) .LE. 11) CALL DS7CPR(C, IV, L, LIV)                     
      IF (IV(1) .GT. 6) GO TO 999                                       
         CALL DN2CVP(IV, LIV, LV, P+L, V)                               
         CALL DN2RDP(IV, LIV, LV, N, V(RD1), V)                         
         GO TO 999                                                      
C                                                                       
 370  IV(1) = 66                                                        
      CALL DITSUM(V, V, IV, LIV, LV, P, V, ALF)                         
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF  DRNSG FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  DRN2G(D, DR, IV, LIV, LV, N, ND, N1, N2, P, R,        
     1                  RD, V, X)                                       
C                                                                       
C *** REVISED ITERATION DRIVER FOR NL2SOL (VERSION 2.3) ***             
C                                                                       
      INTEGER LIV, LV, N, ND, N1, N2, P                                 
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION D(P), DR(ND,P), R(ND), RD(ND), V(LV), X(P)       
C                                                                       
C--------------------------  PARAMETER USAGE  --------------------------
C                                                                       
C D........ SCALE VECTOR.                                               
C DR....... DERIVATIVES OF R AT X.                                      
C IV....... INTEGER VALUES ARRAY.                                       
C LIV...... LENGTH OF IV... LIV MUST BE AT LEAST P + 82.                
C LV....... LENGTH OF V...  LV  MUST BE AT LEAST 105 + P*(2*P+16).      
C N........ TOTAL NUMBER OF RESIDUALS.                                  
C ND....... MAX. NO. OF RESIDUALS PASSED ON ONE CALL.                   
C N1....... LOWEST  ROW INDEX FOR RESIDUALS SUPPLIED THIS TIME.         
C N2....... HIGHEST ROW INDEX FOR RESIDUALS SUPPLIED THIS TIME.         
C P........ NUMBER OF PARAMETERS (COMPONENTS OF X) BEING ESTIMATED.     
C R........ RESIDUALS.                                                  
C RD....... RD(I) = SQRT(G(I)**T * H(I)**-1 * G(I)) ON OUTPUT WHEN      
C        IV(RDREQ) IS NONZERO.   DRN2G SETS IV(REGD) = 1 IF RD          
C        IS SUCCESSFULLY COMPUTED, TO 0 IF NO ATTEMPT WAS MADE          
C        TO COMPUTE IT, AND TO -1 IF H (THE FINITE-DIFFERENCE HESSIAN)  
C        WAS INDEFINITE.  IF ND .GE. N, THEN RD IS ALSO USED AS         
C        TEMPORARY STORAGE.                                             
C V........ FLOATING-POINT VALUES ARRAY.                                
C X........ PARAMETER VECTOR BEING ESTIMATED (INPUT = INITIAL GUESS,    
C             OUTPUT = BEST VALUE FOUND).                               
C                                                                       
C  ***  DISCUSSION  ***                                                 
C                                                                       
C  NOTE... NL2SOL AND NL2ITR (MENTIONED BELOW) ARE DESCRIBED IN         
C  ACM TRANS. MATH. SOFTWARE, VOL. 7, PP. 369-383 (AN ADAPTIVE          
C  NONLINEAR LEAST-SQUARES ALGORITHM, BY J.E. DENNIS, D.M. GAY,         
C  AND R.E. WELSCH).                                                    
C                                                                       
C     THIS ROUTINE CARRIES OUT ITERATIONS FOR SOLVING NONLINEAR         
C  LEAST SQUARES PROBLEMS.  WHEN ND = N, IT IS SIMILAR TO NL2ITR        
C  (WITH J = DR), EXCEPT THAT R(X) AND DR(X) NEED NOT BE INITIALIZED    
C  WHEN  DRN2G IS CALLED WITH IV(1) = 0 OR 12.   DRN2G ALSO ALLOWS      
C  R AND DR TO BE SUPPLIED ROW-WISE -- JUST SET ND = 1 AND CALL         
C   DRN2G ONCE FOR EACH ROW WHEN PROVIDING RESIDUALS AND JACOBIANS.     
C     ANOTHER NEW FEATURE IS THAT CALLING  DRN2G WITH IV(1) = 13        
C  CAUSES STORAGE ALLOCATION ONLY TO BE PERFORMED -- ON RETURN, SUCH    
C  COMPONENTS AS IV(G) (THE FIRST SUBSCRIPT IN G OF THE GRADIENT)       
C  AND IV(S) (THE FIRST SUBSCRIPT IN V OF THE S LOWER TRIANGLE OF       
C  THE S MATRIX) WILL HAVE BEEN SET (UNLESS LIV OR LV IS TOO SMALL),    
C  AND IV(1) WILL HAVE BEEN SET TO 14. CALLING  DRN2G WITH IV(1) = 14   
C  CAUSES EXECUTION OF THE ALGORITHM TO BEGIN UNDER THE ASSUMPTION      
C  THAT STORAGE HAS BEEN ALLOCATED.                                     
C                                                                       
C ***  SUPPLYING R AND DR  ***                                          
C                                                                       
C      DRN2G USES IV AND V IN THE SAME WAY AS NL2SOL, WITH A SMALL      
C  NUMBER OF OBVIOUS CHANGES.  ONE DIFFERENCE BETWEEN  DRN2G AND        
C  NL2ITR IS THAT INITIAL FUNCTION AND GRADIENT INFORMATION NEED NOT    
C  BE SUPPLIED IN THE VERY FIRST CALL ON  DRN2G, THE ONE WITH           
C  IV(1) = 0 OR 12.  ANOTHER DIFFERENCE IS THAT  DRN2G RETURNS WITH     
C  IV(1) = -2 WHEN IT WANTS ANOTHER LOOK AT THE OLD JACOBIAN MATRIX     
C  AND THE CURRENT RESIDUAL -- THE ONE CORRESPONDING TO X AND           
C  IV(NFGCAL).  IT THEN RETURNS WITH IV(1) = -3 WHEN IT WANTS TO SEE    
C  BOTH THE NEW RESIDUAL AND THE NEW JACOBIAN MATRIX AT ONCE.  NOTE     
C  THAT IV(NFGCAL) = IV(7) CONTAINS THE VALUE THAT IV(NFCALL) = IV(6)   
C  HAD WHEN THE CURRENT RESIDUAL WAS EVALUATED.  ALSO NOTE THAT THE     
C  VALUE OF X CORRESPONDING TO THE OLD JACOBIAN MATRIX IS STORED IN     
C  V, STARTING AT V(IV(X0)) = V(IV(43)).                                
C     ANOTHER NEW RETURN...  DRN2G IV(1) = -1 WHEN IT WANTS BOTH THE    
C  RESIDUAL AND THE JACOBIAN TO BE EVALUATED AT X.                      
C     A NEW RESIDUAL VECTOR MUST BE SUPPLIED WHEN  DRN2G RETURNS WITH   
C  IV(1) = 1 OR -1.  THIS TAKES THE FORM OF VALUES OF R(I,X) PASSED     
C  IN R(I-N1+1), I = N1(1)N2.  YOU MAY PASS ALL THESE VALUES AT ONCE    
C  (I.E., N1 = 1 AND N2 = N) OR IN PIECES BY MAKING SEVERAL CALLS ON    
C   DRN2G.  EACH TIME  DRN2G RETURNS WITH IV(1) = 1, N1 WILL HAVE       
C  BEEN SET TO THE INDEX OF THE NEXT RESIDUAL THAT  DRN2G EXPECTS TO    
C  SEE, AND N2 WILL BE SET TO THE INDEX OF THE HIGHEST RESIDUAL THAT    
C  COULD BE GIVEN ON THE NEXT CALL, I.E., N2 = N1 + ND - 1.  (THUS      
C  WHEN  DRN2G FIRST RETURNS WITH IV(1) = 1 FOR A NEW X, IT WILL        
C  HAVE SET N1 TO 1 AND N2 TO MIN(ND,N).)  THE CALLER MAY PROVIDE       
C  FEWER THAN N2-N1+1 RESIDUALS ON THE NEXT CALL BY SETTING N2 TO       
C  A SMALLER VALUE.   DRN2G ASSUMES IT HAS SEEN ALL THE RESIDUALS       
C  FOR THE CURRENT X WHEN IT IS CALLED WITH N2 .GE. N.                  
C    EXAMPLE... SUPPOSE N = 80 AND THAT R IS TO BE PASSED IN 8          
C  BLOCKS OF SIZE 10.  THE FOLLOWING CODE WOULD DO THE JOB.             
C                                                                       
C      N = 80                                                           
C      ND = 10                                                          
C      ...                                                              
C      DO 10 K = 1, 8                                                   
C           ***  COMPUTE R(I,X) FOR I = 10*K-9 TO 10*K  ***             
C           ***  AND STORE THEM IN R(1),...,R(10)  ***                  
C           CALL  DRN2G(..., R, ...)                                    
C   10      CONTINUE                                                    
C                                                                       
C     THE SITUATION IS SIMILAR WHEN GRADIENT INFORMATION IS             
C  REQUIRED, I.E., WHEN  DRN2G RETURNS WITH IV(1) = 2, -1, OR -2.       
C  NOTE THAT  DRN2G OVERWRITES R, BUT THAT IN THE SPECIAL CASE OF       
C  N1 = 1 AND N2 = N ON PREVIOUS CALLS,  DRN2G NEVER RETURNS WITH       
C  IV(1) = -2.  IT SHOULD BE CLEAR THAT THE PARTIAL DERIVATIVE OF       
C  R(I,X) WITH RESPECT TO X(L) IS TO BE STORED IN DR(I-N1+1,L),         
C  L = 1(1)P, I = N1(1)N2.  IT IS ESSENTIAL THAT R(I) AND DR(I,L)       
C  ALL CORRESPOND TO THE SAME RESIDUALS WHEN IV(1) = -1 OR -2.          
C                                                                       
C  ***  COVARIANCE MATRIX  ***                                          
C                                                                       
C     IV(RDREQ) = IV(57) TELLS WHETHER TO COMPUTE A COVARIANCE          
C  MATRIX AND/OR REGRESSION DIAGNOSTICS... 0 MEANS NEITHER,             
C  1 MEANS COVARIANCE MATRIX ONLY, 2 MEANS REG. DIAGNOSTICS ONLY,       
C  3 MEANS BOTH.  AS WITH NL2SOL, IV(COVREQ) = IV(15) TELLS WHAT        
C  HESSIAN APPROXIMATION TO USE IN THIS COMPUTING.                      
C                                                                       
C  ***  REGRESSION DIAGNOSTICS  ***                                     
C                                                                       
C     SEE THE COMMENTS IN SUBROUTINE   DN2G.                            
C                                                                       
C  ***  GENERAL  ***                                                    
C                                                                       
C     CODED BY DAVID M. GAY.                                            
C                                                                       
C+++++++++++++++++++++++++++++  DECLARATIONS  ++++++++++++++++++++++++++
C                                                                       
C  ***  INTRINSIC FUNCTIONS  ***                                        
C/+                                                                     
      INTEGER IABS, MOD                                                 
C/                                                                      
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***                         
C                                                                       
      DOUBLE PRECISION DD7TPR, DV2NRM                                   
      EXTERNAL DC7VFN,DIVSET, DD7TPR,DD7UPD,DG7LIT,DITSUM,DL7VML,       
     1         DN2CVP, DN2LRD, DQ7APL,DQ7RAD,DV7CPY, DV7SCP, DV2NRM     
C                                                                       
C DC7VFN... FINISHES COVARIANCE COMPUTATION.                            
C DIVSET.... PROVIDES DEFAULT IV AND V INPUT COMPONENTS.                
C DD7TPR... COMPUTES INNER PRODUCT OF TWO VECTORS.                      
C DD7UPD...  UPDATES SCALE VECTOR D.                                    
C DG7LIT.... PERFORMS BASIC MINIMIZATION ALGORITHM.                     
C DITSUM.... PRINTS ITERATION SUMMARY, INFO ABOUT INITIAL AND FINAL X.  
C DL7VML.... COMPUTES L * V, V = VECTOR, L = LOWER TRIANGULAR MATRIX.   
C DN2CVP... PRINTS COVARIANCE MATRIX.                                   
C DN2LRD... COMPUTES REGRESSION DIAGNOSTICS.                            
C DQ7APL... APPLIES QR TRANSFORMATIONS STORED BY DQ7RAD.                
C DQ7RAD.... ADDS A NEW BLOCK OF ROWS TO QR DECOMPOSITION.              
C DV7CPY.... COPIES ONE VECTOR TO ANOTHER.                              
C DV7SCP... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.                  
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER G1, GI, I, IV1, IVMODE, JTOL1, K, L, LH, NN, QTR1,        
     1        RMAT1, YI, Y1                                             
      DOUBLE PRECISION T                                                
C                                                                       
      DOUBLE PRECISION HALF, ZERO                                       
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER CNVCOD, COVMAT, COVREQ, DINIT, DTYPE, DTINIT, D0INIT, F,  
     1        FDH, G, H, IPIVOT, IVNEED, JCN, JTOL, LMAT, MODE,         
     2        NEXTIV, NEXTV, NF0, NF00, NF1, NFCALL, NFCOV, NFGCAL,     
     3        NGCALL, NGCOV, QTR, RDREQ, REGD, RESTOR, RLIMIT, RMAT,    
     4        TOOBIG, VNEED, Y                                          
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C/6                                                                     
C     DATA CNVCOD/55/, COVMAT/26/, COVREQ/15/, DTYPE/16/, FDH/74/,      
C    1     G/28/, H/56/, IPIVOT/76/, IVNEED/3/, JCN/66/, JTOL/59/,      
C    2     LMAT/42/, MODE/35/, NEXTIV/46/, NEXTV/47/, NFCALL/6/,        
C    3     NFCOV/52/, NF0/68/, NF00/81/, NF1/69/, NFGCAL/7/, NGCALL/30/,
C    4     NGCOV/53/, QTR/77/, RESTOR/9/, RMAT/78/, RDREQ/57/, REGD/67/,
C    5     TOOBIG/2/, VNEED/4/, Y/48/                                   
C/7                                                                     
      PARAMETER (CNVCOD=55, COVMAT=26, COVREQ=15, DTYPE=16, FDH=74,     
     1           G=28, H=56, IPIVOT=76, IVNEED=3, JCN=66, JTOL=59,      
     2           LMAT=42, MODE=35, NEXTIV=46, NEXTV=47, NFCALL=6,       
     3           NFCOV=52, NF0=68, NF00=81, NF1=69, NFGCAL=7, NGCALL=30,
     4           NGCOV=53, QTR=77, RESTOR=9, RMAT=78, RDREQ=57, REGD=67,
     5           TOOBIG=2, VNEED=4, Y=48)                               
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
 10   IF (ND .LE. 0) GO TO 210                                          
      IF (P .LE. 0) GO TO 210                                           
      IF (N .LE. 0) GO TO 210                                           
      IF (IV1 .EQ. 14) GO TO 30                                         
      IF (IV1 .GT. 16) GO TO 300                                        
      IF (IV1 .LT. 12) GO TO 40                                         
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      IF (IV(1) .NE. 13) GO TO 20                                       
      IV(IVNEED) = IV(IVNEED) + P                                       
      IV(VNEED) = IV(VNEED) + P*(P+13)/2                                
 20   CALL DG7LIT(D, X, IV, LIV, LV, P, P, V, X, X)                     
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(IPIVOT) = IV(NEXTIV)                                           
      IV(NEXTIV) = IV(IPIVOT) + P                                       
      IV(Y) = IV(NEXTV)                                                 
      IV(G) = IV(Y) + P                                                 
      IV(JCN) = IV(G) + P                                               
      IV(RMAT) = IV(JCN) + P                                            
      IV(QTR) = IV(RMAT) + LH                                           
      IV(JTOL) = IV(QTR) + P                                            
      IV(NEXTV) = IV(JTOL) + 2*P                                        
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
      Y1 = IV(Y)                                                        
      CALL DG7LIT(D, V(G1), IV, LIV, LV, P, P, V, X, V(Y1))             
      IF (IV(1) .NE. 1) GO TO 220                                       
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
      Y1 = IV(Y)                                                        
      CALL DG7LIT(D, V(G1), IV, LIV, LV, P, P, V, X, V(Y1))             
      IF (IV(1) - 2) 50, 60, 220                                        
C                                                                       
 50   V(F) = ZERO                                                       
      IF (IV(NF1) .EQ. 0) GO TO 260                                     
      IF (IV(RESTOR) .NE. 2) GO TO 260                                  
      IV(NF0) = IV(NF1)                                                 
      CALL DV7CPY(N, RD, R)                                             
      IV(REGD) = 0                                                      
      GO TO 260                                                         
C                                                                       
 60   CALL DV7SCP(P, V(G1), ZERO)                                       
      IF (IV(MODE) .GT. 0) GO TO 230                                    
      RMAT1 = IV(RMAT)                                                  
      QTR1 = IV(QTR)                                                    
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
      CALL DL7VML(P, V(Y1), V(RMAT1), RD)                               
      GO TO 110                                                         
C                                                                       
 90   IV(1) = -2                                                        
      IF (IV(MODE) .LT. 0) IV(1) = -1                                   
 100  CALL DV7SCP(P, V(Y1), ZERO)                                       
 110  CALL DV7SCP(LH, V(RMAT1), ZERO)                                   
      GO TO 260                                                         
C                                                                       
C  ***  COMPUTE F(X)  ***                                               
C                                                                       
 120  T = DV2NRM(NN, R)                                                 
      IF (T .GT. V(RLIMIT)) GO TO 200                                   
      V(F) = V(F)  +  HALF * T**2                                       
      IF (N2 .LT. N) GO TO 270                                          
      IF (N1 .EQ. 1) IV(NF1) = IV(NFCALL)                               
      GO TO 40                                                          
C                                                                       
C  ***  COMPUTE Y  ***                                                  
C                                                                       
 130  Y1 = IV(Y)                                                        
      YI = Y1                                                           
      DO 140 L = 1, P                                                   
         V(YI) = V(YI) + DD7TPR(NN, DR(1,L), R)                         
         YI = YI + 1                                                    
 140     CONTINUE                                                       
      IF (N2 .LT. N) GO TO 270                                          
         IV(1) = 2                                                      
         IF (N1 .GT. 1) IV(1) = -3                                      
         GO TO 260                                                      
C                                                                       
C  ***  COMPUTE GRADIENT INFORMATION  ***                               
C                                                                       
 150  IF (IV(MODE) .GT. P) GO TO 240                                    
      G1 = IV(G)                                                        
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
      GO TO 190                                                         
C                                                                       
C  *** COMPUTE INITIAL FUNCTION VALUE WHEN ND .LT. N ***                
C                                                                       
 170  IF (N .LE. ND) GO TO 180                                          
         T = DV2NRM(NN, R)                                              
         IF (T .GT. V(RLIMIT)) GO TO 200                                
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
C                                                                       
 190  IF (N2 .LT. N) GO TO 270                                          
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
      Y1 = IV(Y)                                                        
      IV(1) = 1                                                         
      CALL DG7LIT(D, V(G1), IV, LIV, LV, P, P, V, X, V(Y1))             
      IF (IV(1) .NE. 2) GO TO 220                                       
      GO TO 40                                                          
C                                                                       
C  ***  MISC. DETAILS  ***                                              
C                                                                       
C     ***  X IS OUT OF RANGE (OVERSIZE STEP)  ***                       
C                                                                       
 200  IV(TOOBIG) = 1                                                    
      GO TO 40                                                          
C                                                                       
C     ***  BAD N, ND, OR P  ***                                         
C                                                                       
 210  IV(1) = 66                                                        
      GO TO 300                                                         
C                                                                       
C  ***  CONVERGENCE OBTAINED -- SEE WHETHER TO COMPUTE COVARIANCE  ***  
C                                                                       
 220  IF (IV(COVMAT) .NE. 0) GO TO 290                                  
      IF (IV(REGD) .NE. 0) GO TO 290                                    
C                                                                       
C     ***  SEE IF CHOLESKY FACTOR OF HESSIAN IS AVAILABLE  ***          
C                                                                       
      K = IV(FDH)                                                       
      IF (K .LE. 0) GO TO 280                                           
      IF (IV(RDREQ) .LE. 0) GO TO 290                                   
C                                                                       
C     ***  COMPUTE REGRESSION DIAGNOSTICS AND DEFAULT COVARIANCE IF     
C          DESIRED  ***                                                 
C                                                                       
      I = 0                                                             
      IF (MOD(IV(RDREQ),4) .GE. 2) I = 1                                
      IF (MOD(IV(RDREQ),2) .EQ. 1 .AND. IABS(IV(COVREQ)) .LE. 1) I = I+2
      IF (I .EQ. 0) GO TO 250                                           
      IV(MODE) = P + I                                                  
      IV(NGCALL) = IV(NGCALL) + 1                                       
      IV(NGCOV) = IV(NGCOV) + 1                                         
      IV(CNVCOD) = IV(1)                                                
      IF (I .LT. 2) GO TO 230                                           
         L = IABS(IV(H))                                                
         CALL DV7SCP(LH, V(L), ZERO)                                    
 230  IV(NFCOV) = IV(NFCOV) + 1                                         
      IV(NFCALL) = IV(NFCALL) + 1                                       
      IV(NFGCAL) = IV(NFCALL)                                           
      IV(1) = -1                                                        
      GO TO 260                                                         
C                                                                       
 240  L = IV(LMAT)                                                      
      CALL DN2LRD(DR, IV, V(L), LH, LIV, LV, ND, NN, P, R, RD, V)       
      IF (N2 .LT. N) GO TO 270                                          
      IF (N1 .GT. 1) GO TO 250                                          
C                                                                       
C     ***  ENSURE WE CAN RESTART -- AND MAKE RETURN STATE OF DR         
C     ***  INDEPENDENT OF WHETHER REGRESSION DIAGNOSTICS ARE COMPUTED.  
C     ***  USE STEP VECTOR (ALLOCATED BY DG7LIT) FOR SCRATCH.           
C                                                                       
      RMAT1 = IV(RMAT)                                                  
      CALL DV7SCP(LH, V(RMAT1), ZERO)                                   
      CALL DQ7RAD(NN, ND, P, R, .FALSE., V(RMAT1), DR, R)               
      IV(NF1) = 0                                                       
C                                                                       
C  ***  FINISH COMPUTING COVARIANCE  ***                                
C                                                                       
 250  L = IV(LMAT)                                                      
      CALL DC7VFN(IV, V(L), LH, LIV, LV, N, P, V)                       
      GO TO 290                                                         
C                                                                       
C  ***  RETURN FOR MORE FUNCTION OR GRADIENT INFORMATION  ***           
C                                                                       
 260  N2 = 0                                                            
 270  N1 = N2 + 1                                                       
      N2 = N2 + ND                                                      
      IF (N2 .GT. N) N2 = N                                             
      GO TO 999                                                         
C                                                                       
C  ***  COME HERE FOR INDEFINITE FINITE-DIFFERENCE HESSIAN  ***         
C                                                                       
 280  IV(COVMAT) = K                                                    
      IV(REGD) = K                                                      
C                                                                       
C  ***  PRINT SUMMARY OF FINAL ITERATION AND OTHER REQUESTED ITEMS  *** 
C                                                                       
 290  G1 = IV(G)                                                        
 300  CALL DITSUM(D, V(G1), IV, LIV, LV, P, V, X)                       
      IF (IV(1) .LE. 6 .AND. IV(RDREQ) .GT. 0)                          
     1     CALL DN2CVP(IV, LIV, LV, P, V)                               
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF  DRN2G FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DN2RDP(IV, LIV, LV, N, RD, V)                          
C                                                                       
C  ***  PRINT REGRESSION DIAGNOSTICS FOR MLPSL AND NL2S1 ***            
C                                                                       
      INTEGER LIV, LV, N                                                
      INTEGER IV(LIV)                                                   
      DOUBLE PRECISION RD(N), V(LV)                                     
C                                                                       
C     ***  NOTE -- V IS PASSED FOR POSSIBLE USE BY REVISED VERSIONS OF  
C     ***  THIS ROUTINE.                                                
C                                                                       
      INTEGER PU                                                        
C                                                                       
C  ***  IV AND V SUBSCRIPTS  ***                                        
C                                                                       
      INTEGER COVPRT, F, NEEDHD, PRUNIT, REGD                           
C                                                                       
C/6                                                                     
C     DATA COVPRT/14/, F/10/, NEEDHD/36/, PRUNIT/21/, REGD/67/          
C/7                                                                     
      PARAMETER (COVPRT=14, F=10, NEEDHD=36, PRUNIT=21, REGD=67)        
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      PU = IV(PRUNIT)                                                   
      IF (PU .EQ. 0) GO TO 999                                          
      IF (IV(COVPRT) .LT. 2) GO TO 999                                  
      IF (IV(REGD) .LE. 0) GO TO 999                                    
      IV(NEEDHD) = 1                                                    
      IF (V(F)) 10, 30, 10                                              
 10   WRITE(PU,20) RD                                                   
 20   FORMAT(/70H REGRESSION DIAGNOSTIC = SQRT( G(I)**T * H(I)**-1 * G(I
     1) / ABS(F) ).../(6D12.3))                                         
      GO TO 999                                                         
 30   WRITE(PU,40) RD                                                   
 40   FORMAT(/61H REGRESSION DIAGNOSTIC = SQRT( G(I)**T * H(I)**-1 * G(I
     1) ).../(6D12.3))                                                  
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DN2RDP FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DS3GRD(ALPHA, B, D, ETA0, FX, G, IRC, P, W, X)         
C                                                                       
C  ***  COMPUTE FINITE DIFFERENCE GRADIENT BY STWEART*S SCHEME  ***     
C                                                                       
C     ***  PARAMETERS  ***                                              
C                                                                       
      INTEGER IRC, P                                                    
      DOUBLE PRECISION ALPHA(P), B(2,P), D(P), ETA0, FX, G(P), W(6),    
     1                 X(P)                                             
C                                                                       
C.......................................................................
C                                                                       
C     ***  PURPOSE  ***                                                 
C                                                                       
C        THIS SUBROUTINE USES AN EMBELLISHED FORM OF THE FINITE-DIFFER- 
C     ENCE SCHEME PROPOSED BY STEWART (REF. 1) TO APPROXIMATE THE       
C     GRADIENT OF THE FUNCTION F(X), WHOSE VALUES ARE SUPPLIED BY       
C     REVERSE COMMUNICATION.                                            
C                                                                       
C     ***  PARAMETER DESCRIPTION  ***                                   
C                                                                       
C  ALPHA IN  (APPROXIMATE) DIAGONAL ELEMENTS OF THE HESSIAN OF F(X).    
C      B IN  ARRAY OF SIMPLE LOWER AND UPPER BOUNDS ON X.  X MUST       
C             SATISFY B(1,I) .LE. X(I) .LE. B(2,I), I = 1(1)P.          
C             FOR ALL I WITH B(1,I) .GE. B(2,I), DS3GRD SIMPLY          
C             SETS G(I) TO 0.                                           
C      D IN  SCALE VECTOR SUCH THAT D(I)*X(I), I = 1,...,P, ARE IN      
C             COMPARABLE UNITS.                                         
C   ETA0 IN  ESTIMATED BOUND ON RELATIVE ERROR IN THE FUNCTION VALUE... 
C             (TRUE VALUE) = (COMPUTED VALUE)*(1+E),   WHERE            
C             ABS(E) .LE. ETA0.                                         
C     FX I/O ON INPUT,  FX  MUST BE THE COMPUTED VALUE OF F(X).  ON     
C             OUTPUT WITH IRC = 0, FX HAS BEEN RESTORED TO ITS ORIGINAL 
C             VALUE, THE ONE IT HAD WHEN DS3GRD WAS LAST CALLED WITH    
C             IRC = 0.                                                  
C      G I/O ON INPUT WITH IRC = 0, G SHOULD CONTAIN AN APPROXIMATION   
C             TO THE GRADIENT OF F NEAR X, E.G., THE GRADIENT AT THE    
C             PREVIOUS ITERATE.  WHEN DS3GRD RETURNS WITH IRC = 0, G IS 
C             THE DESIRED FINITE-DIFFERENCE APPROXIMATION TO THE        
C             GRADIENT AT X.                                            
C    IRC I/O INPUT/RETURN CODE... BEFORE THE VERY FIRST CALL ON DS3GRD, 
C             THE CALLER MUST SET IRC TO 0.  WHENEVER DS3GRD RETURNS A  
C             NONZERO VALUE (OF AT MOST P) FOR IRC, IT HAS PERTURBED    
C             SOME COMPONENT OF X... THE CALLER SHOULD EVALUATE F(X)    
C             AND CALL DS3GRD AGAIN WITH FX = F(X).  IF B PREVENTS      
C             ESTIMATING G(I) I.E., IF THERE IS AN I WITH               
C             B(1,I) .LT. B(2,I) BUT WITH B(1,I) SO CLOSE TO B(2,I)     
C             THAT THE FINITE-DIFFERENCING STEPS CANNOT BE CHOSEN,      
C             THEN DS3GRD RETURNS WITH IRC .GT. P.                      
C      P IN  THE NUMBER OF VARIABLES (COMPONENTS OF X) ON WHICH F       
C             DEPENDS.                                                  
C      X I/O ON INPUT WITH IRC = 0, X IS THE POINT AT WHICH THE         
C             GRADIENT OF F IS DESIRED.  ON OUTPUT WITH IRC NONZERO, X  
C             IS THE POINT AT WHICH F SHOULD BE EVALUATED.  ON OUTPUT   
C             WITH IRC = 0, X HAS BEEN RESTORED TO ITS ORIGINAL VALUE   
C             (THE ONE IT HAD WHEN DS3GRD WAS LAST CALLED WITH IRC = 0) 
C             AND G CONTAINS THE DESIRED GRADIENT APPROXIMATION.        
C      W I/O WORK VECTOR OF LENGTH 6 IN WHICH DS3GRD SAVES CERTAIN      
C             QUANTITIES WHILE THE CALLER IS EVALUATING F(X) AT A       
C             PERTURBED X.                                              
C                                                                       
C     ***  APPLICATION AND USAGE RESTRICTIONS  ***                      
C                                                                       
C        THIS ROUTINE IS INTENDED FOR USE WITH QUASI-NEWTON ROUTINES    
C     FOR UNCONSTRAINED MINIMIZATION (IN WHICH CASE  ALPHA  COMES FROM  
C     THE DIAGONAL OF THE QUASI-NEWTON HESSIAN APPROXIMATION).          
C                                                                       
C     ***  ALGORITHM NOTES  ***                                         
C                                                                       
C        THIS CODE DEPARTS FROM THE SCHEME PROPOSED BY STEWART (REF. 1) 
C     IN ITS GUARDING AGAINST OVERLY LARGE OR SMALL STEP SIZES AND ITS  
C     HANDLING OF SPECIAL CASES (SUCH AS ZERO COMPONENTS OF ALPHA OR G).
C                                                                       
C     ***  REFERENCES  ***                                              
C                                                                       
C 1. STEWART, G.W. (1967), A MODIFICATION OF DAVIDON*S MINIMIZATION     
C        METHOD TO ACCEPT DIFFERENCE APPROXIMATIONS OF DERIVATIVES,     
C        J. ASSOC. COMPUT. MACH. 14, PP. 72-83.                         
C                                                                       
C     ***  HISTORY  ***                                                 
C                                                                       
C     DESIGNED AND CODED BY DAVID M. GAY (SUMMER 1977/SUMMER 1980).     
C                                                                       
C     ***  GENERAL  ***                                                 
C                                                                       
C        THIS ROUTINE WAS PREPARED IN CONNECTION WITH WORK SUPPORTED BY 
C     THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS MCS76-00324 AND      
C     MCS-7906671.                                                      
C                                                                       
C.......................................................................
C                                                                       
C     *****  EXTERNAL FUNCTION  *****                                   
C                                                                       
      DOUBLE PRECISION DR7MDC                                           
      EXTERNAL DR7MDC                                                   
C DR7MDC... RETURNS MACHINE-DEPENDENT CONSTANTS.                        
C                                                                       
C     ***** INTRINSIC FUNCTIONS *****                                   
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
C     ***** LOCAL VARIABLES *****                                       
C                                                                       
      LOGICAL HIT                                                       
      INTEGER FH, FX0, HSAVE, I, XISAVE                                 
      DOUBLE PRECISION AAI, AFX, AFXETA, AGI, ALPHAI, AXI, AXIBAR,      
     1                 DISCON, ETA, GI, H, HMIN, XI, XIH                
      DOUBLE PRECISION C2000, FOUR, HMAX0, HMIN0, H0, MACHEP, ONE, P002,
     1                 THREE, TWO, ZERO                                 
C                                                                       
C/6                                                                     
C     DATA C2000/2.0D+3/, FOUR/4.0D+0/, HMAX0/0.02D+0/, HMIN0/5.0D+1/,  
C    1     ONE/1.0D+0/, P002/0.002D+0/, THREE/3.0D+0/,                  
C    2     TWO/2.0D+0/, ZERO/0.0D+0/                                    
C/7                                                                     
      PARAMETER (C2000=2.0D+3, FOUR=4.0D+0, HMAX0=0.02D+0, HMIN0=5.0D+1,
     1     ONE=1.0D+0, P002=0.002D+0, THREE=3.0D+0,                     
     2     TWO=2.0D+0, ZERO=0.0D+0)                                     
C/                                                                      
C/6                                                                     
C     DATA FH/3/, FX0/4/, HSAVE/5/, XISAVE/6/                           
C/7                                                                     
      PARAMETER (FH=3, FX0=4, HSAVE=5, XISAVE=6)                        
C/                                                                      
C                                                                       
C---------------------------------  BODY  ------------------------------
C                                                                       
      IF (IRC) 80, 10, 210                                              
C                                                                       
C     ***  FRESH START -- GET MACHINE-DEPENDENT CONSTANTS  ***          
C                                                                       
C     STORE MACHEP IN W(1) AND H0 IN W(2), WHERE MACHEP IS THE UNIT     
C     ROUNDOFF (THE SMALLEST POSITIVE NUMBER SUCH THAT                  
C     1 + MACHEP .GT. 1  AND  1 - MACHEP .LT. 1),  AND  H0 IS THE       
C     SQUARE-ROOT OF MACHEP.                                            
C                                                                       
 10   W(1) = DR7MDC(3)                                                  
      W(2) = DSQRT(W(1))                                                
C                                                                       
      W(FX0) = FX                                                       
C                                                                       
C     ***  INCREMENT  I  AND START COMPUTING  G(I)  ***                 
C                                                                       
 20   I = IABS(IRC) + 1                                                 
      IF (I .GT. P) GO TO 220                                           
         IRC = I                                                        
         IF (B(1,I) .LT. B(2,I)) GO TO 30                               
            G(I) = ZERO                                                 
            GO TO 20                                                    
 30      AFX = DABS(W(FX0))                                             
         MACHEP = W(1)                                                  
         H0 = W(2)                                                      
         HMIN = HMIN0 * MACHEP                                          
         XI = X(I)                                                      
         W(XISAVE) = XI                                                 
         AXI = DABS(XI)                                                 
         AXIBAR = DMAX1(AXI, ONE/D(I))                                  
         GI = G(I)                                                      
         AGI = DABS(GI)                                                 
         ETA = DABS(ETA0)                                               
         IF (AFX .GT. ZERO) ETA = DMAX1(ETA, AGI*AXI*MACHEP/AFX)        
         ALPHAI = ALPHA(I)                                              
         IF (ALPHAI .EQ. ZERO) GO TO 130                                
         IF (GI .EQ. ZERO .OR. FX .EQ. ZERO) GO TO 140                  
         AFXETA = AFX*ETA                                               
         AAI = DABS(ALPHAI)                                             
C                                                                       
C        *** COMPUTE H = STEWART*S FORWARD-DIFFERENCE STEP SIZE.        
C                                                                       
         IF (GI**2 .LE. AFXETA*AAI) GO TO 40                            
              H = TWO*DSQRT(AFXETA/AAI)                                 
              H = H*(ONE - AAI*H/(THREE*AAI*H + FOUR*AGI))              
              GO TO 50                                                  
C40      H = TWO*(AFXETA*AGI/(AAI**2))**(ONE/THREE)                     
 40      H = TWO * (AFXETA*AGI)**(ONE/THREE) * AAI**(-TWO/THREE)        
         H = H*(ONE - TWO*AGI/(THREE*AAI*H + FOUR*AGI))                 
C                                                                       
C        ***  ENSURE THAT  H  IS NOT INSIGNIFICANTLY SMALL  ***         
C                                                                       
 50      H = DMAX1(H, HMIN*AXIBAR)                                      
C                                                                       
C        *** USE FORWARD DIFFERENCE IF BOUND ON TRUNCATION ERROR IS AT  
C        *** MOST 10**-3.                                               
C                                                                       
         IF (AAI*H .LE. P002*AGI) GO TO 120                             
C                                                                       
C        *** COMPUTE H = STEWART*S STEP FOR CENTRAL DIFFERENCE.         
C                                                                       
         DISCON = C2000*AFXETA                                          
         H = DISCON/(AGI + DSQRT(GI**2 + AAI*DISCON))                   
C                                                                       
C        ***  ENSURE THAT  H  IS NEITHER TOO SMALL NOR TOO BIG  ***     
C                                                                       
         H = DMAX1(H, HMIN*AXIBAR)                                      
         IF (H .GE. HMAX0*AXIBAR) H = AXIBAR * H0**(TWO/THREE)          
C                                                                       
C        ***  COMPUTE CENTRAL DIFFERENCE  ***                           
C                                                                       
         XIH = XI + H                                                   
         IF (XI - H .LT. B(1,I)) GO TO 60                               
         IRC = -I                                                       
         IF (XIH .LE. B(2,I)) GO TO 200                                 
            H = -H                                                      
            XIH = XI + H                                                
            IF (XI + TWO*H .LT. B(1,I)) GO TO 190                       
            GO TO 70                                                    
 60      IF (XI + TWO*H .GT. B(2,I)) GO TO 190                          
C        *** MUST DO OFF-SIDE CENTRAL DIFFERENCE ***                    
 70      IRC = -(I + P)                                                 
         GO TO 200                                                      
C                                                                       
 80      I = -IRC                                                       
         IF (I .LE. P) GO TO 100                                        
         I = I - P                                                      
         IF (I .GT. P) GO TO 90                                         
         W(FH) = FX                                                     
         H = TWO * W(HSAVE)                                             
         XIH = W(XISAVE) + H                                            
         IRC = IRC - P                                                  
         GO TO 200                                                      
C                                                                       
C    *** FINISH OFF-SIDE CENTRAL DIFFERENCE ***                         
C                                                                       
 90      I = I - P                                                      
         G(I) = (FOUR*W(FH) - FX - THREE*W(FX0)) / W(HSAVE)             
         IRC = I                                                        
         X(I) = W(XISAVE)                                               
         GO TO 20                                                       
C                                                                       
 100     H = -W(HSAVE)                                                  
         IF (H .GT. ZERO) GO TO 110                                     
         W(FH) = FX                                                     
         XIH = W(XISAVE) + H                                            
         GO TO 200                                                      
C                                                                       
 110     G(I) = (W(FH) - FX) / (TWO * H)                                
         X(I) = W(XISAVE)                                               
         GO TO 20                                                       
C                                                                       
C     ***  COMPUTE FORWARD DIFFERENCES IN VARIOUS CASES  ***            
C                                                                       
 120     IF (H .GE. HMAX0*AXIBAR) H = H0 * AXIBAR                       
         IF (ALPHAI*GI .LT. ZERO) H = -H                                
         GO TO 150                                                      
 130     H = AXIBAR                                                     
         GO TO 150                                                      
 140     H = H0 * AXIBAR                                                
C                                                                       
 150     HIT = .FALSE.                                                  
 160     XIH = XI + H                                                   
         IF (H .GT. ZERO) GO TO 170                                     
            IF (XIH .GE. B(1,I)) GO TO 200                              
            GO TO 180                                                   
 170     IF (XIH .LE. B(2,I)) GO TO 200                                 
 180        IF (HIT) GO TO 190                                          
            HIT = .TRUE.                                                
            H = -H                                                      
            GO TO 160                                                   
C                                                                       
C        *** ERROR RETURN...                                            
 190     IRC = I + P                                                    
         GO TO 230                                                      
C                                                                       
C        *** RETURN FOR NEW FUNCTION VALUE...                           
 200     X(I) = XIH                                                     
         W(HSAVE) = H                                                   
         GO TO 999                                                      
C                                                                       
C     ***  COMPUTE ACTUAL FORWARD DIFFERENCE  ***                       
C                                                                       
 210     G(IRC) = (FX - W(FX0)) / W(HSAVE)                              
         X(IRC) = W(XISAVE)                                             
         GO TO 20                                                       
C                                                                       
C  ***  RESTORE FX AND INDICATE THAT G HAS BEEN COMPUTED  ***           
C                                                                       
 220  IRC = 0                                                           
 230  FX = W(FX0)                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST LINE OF DS3GRD FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DS7GRD (ALPHA, D, ETA0, FX, G, IRC, N, W, X)           
C                                                                       
C  ***  COMPUTE FINITE DIFFERENCE GRADIENT BY STWEART*S SCHEME  ***     
C                                                                       
C     ***  PARAMETERS  ***                                              
C                                                                       
      INTEGER IRC, N                                                    
      DOUBLE PRECISION ALPHA(N), D(N), ETA0, FX, G(N), W(6), X(N)       
C                                                                       
C.......................................................................
C                                                                       
C     ***  PURPOSE  ***                                                 
C                                                                       
C        THIS SUBROUTINE USES AN EMBELLISHED FORM OF THE FINITE-DIFFER- 
C     ENCE SCHEME PROPOSED BY STEWART (REF. 1) TO APPROXIMATE THE       
C     GRADIENT OF THE FUNCTION F(X), WHOSE VALUES ARE SUPPLIED BY       
C     REVERSE COMMUNICATION.                                            
C                                                                       
C     ***  PARAMETER DESCRIPTION  ***                                   
C                                                                       
C  ALPHA IN  (APPROXIMATE) DIAGONAL ELEMENTS OF THE HESSIAN OF F(X).    
C      D IN  SCALE VECTOR SUCH THAT D(I)*X(I), I = 1,...,N, ARE IN      
C             COMPARABLE UNITS.                                         
C   ETA0 IN  ESTIMATED BOUND ON RELATIVE ERROR IN THE FUNCTION VALUE... 
C             (TRUE VALUE) = (COMPUTED VALUE)*(1+E),   WHERE            
C             ABS(E) .LE. ETA0.                                         
C     FX I/O ON INPUT,  FX  MUST BE THE COMPUTED VALUE OF F(X).  ON     
C             OUTPUT WITH IRC = 0, FX HAS BEEN RESTORED TO ITS ORIGINAL 
C             VALUE, THE ONE IT HAD WHEN DS7GRD WAS LAST CALLED WITH    
C             IRC = 0.                                                  
C      G I/O ON INPUT WITH IRC = 0, G SHOULD CONTAIN AN APPROXIMATION   
C             TO THE GRADIENT OF F NEAR X, E.G., THE GRADIENT AT THE    
C             PREVIOUS ITERATE.  WHEN DS7GRD RETURNS WITH IRC = 0, G IS 
C             THE DESIRED FINITE-DIFFERENCE APPROXIMATION TO THE        
C             GRADIENT AT X.                                            
C    IRC I/O INPUT/RETURN CODE... BEFORE THE VERY FIRST CALL ON DS7GRD, 
C             THE CALLER MUST SET IRC TO 0.  WHENEVER DS7GRD RETURNS A  
C             NONZERO VALUE FOR IRC, IT HAS PERTURBED SOME COMPONENT OF 
C             X... THE CALLER SHOULD EVALUATE F(X) AND CALL DS7GRD      
C             AGAIN WITH FX = F(X).                                     
C      N IN  THE NUMBER OF VARIABLES (COMPONENTS OF X) ON WHICH F       
C             DEPENDS.                                                  
C      X I/O ON INPUT WITH IRC = 0, X IS THE POINT AT WHICH THE         
C             GRADIENT OF F IS DESIRED.  ON OUTPUT WITH IRC NONZERO, X  
C             IS THE POINT AT WHICH F SHOULD BE EVALUATED.  ON OUTPUT   
C             WITH IRC = 0, X HAS BEEN RESTORED TO ITS ORIGINAL VALUE   
C             (THE ONE IT HAD WHEN DS7GRD WAS LAST CALLED WITH IRC = 0) 
C             AND G CONTAINS THE DESIRED GRADIENT APPROXIMATION.        
C      W I/O WORK VECTOR OF LENGTH 6 IN WHICH DS7GRD SAVES CERTAIN      
C             QUANTITIES WHILE THE CALLER IS EVALUATING F(X) AT A       
C             PERTURBED X.                                              
C                                                                       
C     ***  APPLICATION AND USAGE RESTRICTIONS  ***                      
C                                                                       
C        THIS ROUTINE IS INTENDED FOR USE WITH QUASI-NEWTON ROUTINES    
C     FOR UNCONSTRAINED MINIMIZATION (IN WHICH CASE  ALPHA  COMES FROM  
C     THE DIAGONAL OF THE QUASI-NEWTON HESSIAN APPROXIMATION).          
C                                                                       
C     ***  ALGORITHM NOTES  ***                                         
C                                                                       
C        THIS CODE DEPARTS FROM THE SCHEME PROPOSED BY STEWART (REF. 1) 
C     IN ITS GUARDING AGAINST OVERLY LARGE OR SMALL STEP SIZES AND ITS  
C     HANDLING OF SPECIAL CASES (SUCH AS ZERO COMPONENTS OF ALPHA OR G).
C                                                                       
C     ***  REFERENCES  ***                                              
C                                                                       
C 1. STEWART, G.W. (1967), A MODIFICATION OF DAVIDON*S MINIMIZATION     
C        METHOD TO ACCEPT DIFFERENCE APPROXIMATIONS OF DERIVATIVES,     
C        J. ASSOC. COMPUT. MACH. 14, PP. 72-83.                         
C                                                                       
C     ***  HISTORY  ***                                                 
C                                                                       
C     DESIGNED AND CODED BY DAVID M. GAY (SUMMER 1977/SUMMER 1980).     
C                                                                       
C     ***  GENERAL  ***                                                 
C                                                                       
C        THIS ROUTINE WAS PREPARED IN CONNECTION WITH WORK SUPPORTED BY 
C     THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS MCS76-00324 AND      
C     MCS-7906671.                                                      
C                                                                       
C.......................................................................
C                                                                       
C     *****  EXTERNAL FUNCTION  *****                                   
C                                                                       
      DOUBLE PRECISION DR7MDC                                           
      EXTERNAL DR7MDC                                                   
C DR7MDC... RETURNS MACHINE-DEPENDENT CONSTANTS.                        
C                                                                       
C     ***** INTRINSIC FUNCTIONS *****                                   
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
C     ***** LOCAL VARIABLES *****                                       
C                                                                       
      INTEGER FH, FX0, HSAVE, I, XISAVE                                 
      DOUBLE PRECISION AAI, AFX, AFXETA, AGI, ALPHAI, AXI, AXIBAR,      
     1                 DISCON, ETA, GI, H, HMIN                         
      DOUBLE PRECISION C2000, FOUR, HMAX0, HMIN0, H0, MACHEP, ONE, P002,
     1                 THREE, TWO, ZERO                                 
C                                                                       
C/6                                                                     
C     DATA C2000/2.0D+3/, FOUR/4.0D+0/, HMAX0/0.02D+0/, HMIN0/5.0D+1/,  
C    1     ONE/1.0D+0/, P002/0.002D+0/, THREE/3.0D+0/,                  
C    2     TWO/2.0D+0/, ZERO/0.0D+0/                                    
C/7                                                                     
      PARAMETER (C2000=2.0D+3, FOUR=4.0D+0, HMAX0=0.02D+0, HMIN0=5.0D+1,
     1     ONE=1.0D+0, P002=0.002D+0, THREE=3.0D+0,                     
     2     TWO=2.0D+0, ZERO=0.0D+0)                                     
C/                                                                      
C/6                                                                     
C     DATA FH/3/, FX0/4/, HSAVE/5/, XISAVE/6/                           
C/7                                                                     
      PARAMETER (FH=3, FX0=4, HSAVE=5, XISAVE=6)                        
C/                                                                      
C                                                                       
C---------------------------------  BODY  ------------------------------
C                                                                       
      IF (IRC) 140, 100, 210                                            
C                                                                       
C     ***  FRESH START -- GET MACHINE-DEPENDENT CONSTANTS  ***          
C                                                                       
C     STORE MACHEP IN W(1) AND H0 IN W(2), WHERE MACHEP IS THE UNIT     
C     ROUNDOFF (THE SMALLEST POSITIVE NUMBER SUCH THAT                  
C     1 + MACHEP .GT. 1  AND  1 - MACHEP .LT. 1),  AND  H0 IS THE       
C     SQUARE-ROOT OF MACHEP.                                            
C                                                                       
 100  W(1) = DR7MDC(3)                                                  
      W(2) = DSQRT(W(1))                                                
C                                                                       
      W(FX0) = FX                                                       
C                                                                       
C     ***  INCREMENT  I  AND START COMPUTING  G(I)  ***                 
C                                                                       
 110  I = IABS(IRC) + 1                                                 
      IF (I .GT. N) GO TO 300                                           
         IRC = I                                                        
         AFX = DABS(W(FX0))                                             
         MACHEP = W(1)                                                  
         H0 = W(2)                                                      
         HMIN = HMIN0 * MACHEP                                          
         W(XISAVE) = X(I)                                               
         AXI = DABS(X(I))                                               
         AXIBAR = DMAX1(AXI, ONE/D(I))                                  
         GI = G(I)                                                      
         AGI = DABS(GI)                                                 
         ETA = DABS(ETA0)                                               
         IF (AFX .GT. ZERO) ETA = DMAX1(ETA, AGI*AXI*MACHEP/AFX)        
         ALPHAI = ALPHA(I)                                              
         IF (ALPHAI .EQ. ZERO) GO TO 170                                
         IF (GI .EQ. ZERO .OR. FX .EQ. ZERO) GO TO 180                  
         AFXETA = AFX*ETA                                               
         AAI = DABS(ALPHAI)                                             
C                                                                       
C        *** COMPUTE H = STEWART*S FORWARD-DIFFERENCE STEP SIZE.        
C                                                                       
         IF (GI**2 .LE. AFXETA*AAI) GO TO 120                           
              H = TWO*DSQRT(AFXETA/AAI)                                 
              H = H*(ONE - AAI*H/(THREE*AAI*H + FOUR*AGI))              
              GO TO 130                                                 
C120     H = TWO*(AFXETA*AGI/(AAI**2))**(ONE/THREE)                     
 120     H = TWO * (AFXETA*AGI)**(ONE/THREE) * AAI**(-TWO/THREE)        
         H = H*(ONE - TWO*AGI/(THREE*AAI*H + FOUR*AGI))                 
C                                                                       
C        ***  ENSURE THAT  H  IS NOT INSIGNIFICANTLY SMALL  ***         
C                                                                       
 130     H = DMAX1(H, HMIN*AXIBAR)                                      
C                                                                       
C        *** USE FORWARD DIFFERENCE IF BOUND ON TRUNCATION ERROR IS AT  
C        *** MOST 10**-3.                                               
C                                                                       
         IF (AAI*H .LE. P002*AGI) GO TO 160                             
C                                                                       
C        *** COMPUTE H = STEWART*S STEP FOR CENTRAL DIFFERENCE.         
C                                                                       
         DISCON = C2000*AFXETA                                          
         H = DISCON/(AGI + DSQRT(GI**2 + AAI*DISCON))                   
C                                                                       
C        ***  ENSURE THAT  H  IS NEITHER TOO SMALL NOR TOO BIG  ***     
C                                                                       
         H = DMAX1(H, HMIN*AXIBAR)                                      
         IF (H .GE. HMAX0*AXIBAR) H = AXIBAR * H0**(TWO/THREE)          
C                                                                       
C        ***  COMPUTE CENTRAL DIFFERENCE  ***                           
C                                                                       
         IRC = -I                                                       
         GO TO 200                                                      
C                                                                       
 140     H = -W(HSAVE)                                                  
         I = IABS(IRC)                                                  
         IF (H .GT. ZERO) GO TO 150                                     
         W(FH) = FX                                                     
         GO TO 200                                                      
C                                                                       
 150     G(I) = (W(FH) - FX) / (TWO * H)                                
         X(I) = W(XISAVE)                                               
         GO TO 110                                                      
C                                                                       
C     ***  COMPUTE FORWARD DIFFERENCES IN VARIOUS CASES  ***            
C                                                                       
 160     IF (H .GE. HMAX0*AXIBAR) H = H0 * AXIBAR                       
         IF (ALPHAI*GI .LT. ZERO) H = -H                                
         GO TO 200                                                      
 170     H = AXIBAR                                                     
         GO TO 200                                                      
 180     H = H0 * AXIBAR                                                
C                                                                       
 200     X(I) = W(XISAVE) + H                                           
         W(HSAVE) = H                                                   
         GO TO 999                                                      
C                                                                       
C     ***  COMPUTE ACTUAL FORWARD DIFFERENCE  ***                       
C                                                                       
 210     G(IRC) = (FX - W(FX0)) / W(HSAVE)                              
         X(IRC) = W(XISAVE)                                             
         GO TO 110                                                      
C                                                                       
C  ***  RESTORE FX AND INDICATE THAT G HAS BEEN COMPUTED  ***           
C                                                                       
 300  FX = W(FX0)                                                       
      IRC = 0                                                           
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DS7GRD FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DW7ZBF (L, N, S, W, Y, Z)                              
C                                                                       
C  ***  COMPUTE  Y  AND  Z  FOR  DL7UPD  CORRESPONDING TO BFGS UPDATE.  
C                                                                       
      INTEGER N                                                         
      DOUBLE PRECISION L(1), S(N), W(N), Y(N), Z(N)                     
C     DIMENSION L(N*(N+1)/2)                                            
C                                                                       
C--------------------------  PARAMETER USAGE  --------------------------
C                                                                       
C L (I/O) CHOLESKY FACTOR OF HESSIAN, A LOWER TRIANG. MATRIX STORED     
C             COMPACTLY BY ROWS.                                        
C N (INPUT) ORDER OF  L  AND LENGTH OF  S,  W,  Y,  Z.                  
C S (INPUT) THE STEP JUST TAKEN.                                        
C W (OUTPUT) RIGHT SINGULAR VECTOR OF RANK 1 CORRECTION TO L.           
C Y (INPUT) CHANGE IN GRADIENTS CORRESPONDING TO S.                     
C Z (OUTPUT) LEFT SINGULAR VECTOR OF RANK 1 CORRECTION TO L.            
C                                                                       
C-------------------------------  NOTES  -------------------------------
C                                                                       
C  ***  ALGORITHM NOTES  ***                                            
C                                                                       
C        WHEN  S  IS COMPUTED IN CERTAIN WAYS, E.G. BY  GQTSTP  OR      
C     DBLDOG,  IT IS POSSIBLE TO SAVE N**2/2 OPERATIONS SINCE  (L**T)*S 
C     OR  L*(L**T)*S IS THEN KNOWN.                                     
C        IF THE BFGS UPDATE TO L*(L**T) WOULD REDUCE ITS DETERMINANT TO 
C     LESS THAN EPS TIMES ITS OLD VALUE, THEN THIS ROUTINE IN EFFECT    
C     REPLACES  Y  BY  THETA*Y + (1 - THETA)*L*(L**T)*S,  WHERE  THETA  
C     (BETWEEN 0 AND 1) IS CHOSEN TO MAKE THE REDUCTION FACTOR = EPS.   
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
C  ***  FUNCTIONS AND SUBROUTINES CALLED  ***                           
C                                                                       
      DOUBLE PRECISION DD7TPR                                           
      EXTERNAL DD7TPR, DL7IVM, DL7TVM                                   
C DD7TPR RETURNS INNER PRODUCT OF TWO VECTORS.                          
C DL7IVM MULTIPLIES L**-1 TIMES A VECTOR.                               
C DL7TVM MULTIPLIES L**T TIMES A VECTOR.                                
C                                                                       
C  ***  INTRINSIC FUNCTIONS  ***                                        
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
C--------------------------  LOCAL VARIABLES  --------------------------
C                                                                       
      INTEGER I                                                         
      DOUBLE PRECISION CS, CY, EPS, EPSRT, ONE, SHS, YS, THETA          
C                                                                       
C  ***  DATA INITIALIZATIONS  ***                                       
C                                                                       
C/6                                                                     
C     DATA EPS/0.1D+0/, ONE/1.D+0/                                      
C/7                                                                     
      PARAMETER (EPS=0.1D+0, ONE=1.D+0)                                 
C/                                                                      
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      CALL DL7TVM(N, W, L, S)                                           
      SHS = DD7TPR(N, W, W)                                             
      YS = DD7TPR(N, Y, S)                                              
      IF (YS .GE. EPS*SHS) GO TO 10                                     
         THETA = (ONE - EPS) * SHS / (SHS - YS)                         
         EPSRT = DSQRT(EPS)                                             
         CY = THETA / (SHS * EPSRT)                                     
         CS = (ONE + (THETA-ONE)/EPSRT) / SHS                           
         GO TO 20                                                       
 10   CY = ONE / (DSQRT(YS) * DSQRT(SHS))                               
      CS = ONE / SHS                                                    
 20   CALL DL7IVM(N, Z, L, Y)                                           
      DO 30 I = 1, N                                                    
 30      Z(I) = CY * Z(I)  -  CS * W(I)                                 
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DW7ZBF FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  DNSFB(N, P, L, ALF, B, C, Y, CALCA, INC, IINC, IV,    
     1                  LIV, LV, V, UIPARM, URPARM, UFPARM)             
C                                                                       
C  ***  SOLVE SEPARABLE NONLINEAR LEAST SQUARES USING                   
C  ***  FINITE-DIFFERENCE DERIVATIVES.                                  
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER IINC, L, LIV, LV, N, P                                    
C/6                                                                     
C     INTEGER INC(IINC,P), IV(LIV), UIPARM(1)                           
C     DOUBLE PRECISION ALF(P), C(L), B(2,P), URPARM(1), V(LV), Y(N)     
C/7                                                                     
      INTEGER INC(IINC,P), IV(LIV), UIPARM(*)                           
      DOUBLE PRECISION ALF(P), C(L), B(2,P), URPARM(*), V(LV), Y(N)     
C/                                                                      
      EXTERNAL CALCA, UFPARM                                            
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
C      N (IN)  NUMBER OF OBSERVATIONS.                                  
C      P (IN)  NUMBER OF NONLINEAR PARAMETERS TO BE ESTIMATED.          
C      L (IN)  NUMBER OF LINEAR PARAMETERS TO BE ESTIMATED.             
C    ALF (I/O) NONLINEAR PARAMETERS.                                    
C                 INPUT = INITIAL GUESS,                                
C                 OUTPUT = BEST ESTIMATE FOUND.                         
C      B (IN)  SIMBLE BOUNDS ON ALF.. B(1,I) .LE. ALF(I) .LE. B(2,I).   
C      C (OUT) LINEAR PARAMETERS (ESTIMATED).                           
C      Y (IN)  RIGHT-HAND SIDE VECTOR.                                  
C  CALCA (IN)  SUBROUTINE TO COMPUTE A MATRIX.                          
C    INC (IN)  INCIDENCE MATRIX OF DEPENDENCIES OF COLUMNS OF A ON      
C                 COMPONENTS OF ALF -- INC(I,J) = 1 MEANS COLUMN I      
C                 OF A DEPENDS ON ALF(J).                               
C   IINC (IN)  DECLARED LEAD DIMENSION OF INC.  MUST BE AT LEAST L+1.   
C     IV (I/O) INTEGER PARAMETER AND SCRATCH VECTOR.                    
C    LIV (IN)  LENGTH OF IV.  MUST BE AT LEAST                          
C                 122 + 2*M + 7*P + 2*L + MAX(L+1,6*P), WHERE  M  IS    
C                 THE NUMBER OF ONES IN INC.                            
C     LV (IN)  LENGTH OF V.  MUST BE AT LEAST                           
C                 105 + N*(2*L+6+P) + L*(L+3)/2 + P*(2*P + 22).         
C                 IF THE LAST ROW OF INC CONTAINS ONLY ZEROS, THEN LV   
C                 CAN BE 4*N LESS THAN JUST DESCRIBED.                  
C      V (I/O) FLOATING-POINT PARAMETER AND SCRATCH VECTOR.             
C UIPARM (I/O) INTEGER VECTOR PASSED WITHOUT CHANGE TO CALCA.           
C URPARM (I/O) FLOATING-POINT VECTOR PASSED WITHOUT CHANGE TO CALCA.    
C UFPARM (I/O) SUBROUTINE PASSED (WITHOUT HAVING BEEN CALLED) TO CALCA. 
C                                                                       
C                                                                       
C--------------------------  DECLARATIONS  ---------------------------- 
C                                                                       
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      EXTERNAL DIVSET, DSM, DRNSGB,DV2AXY,DV7CPY, DV7SCL                
C                                                                       
C DIVSET.... PROVIDES DEFAULT IV AND V VALUES.                          
C DSM...... DETERMINES EFFICIENT ORDER FOR FINITE DIFFERENCES.          
C DRNSGB... CARRIES OUT NL2SOL ALGORITHM.                               
C DV2AXY.... ADDS A MULTIPLE OF ONE VECTOR TO ANOTHER.                  
C DV7CPY.... COPIES ONE VECTOR TO ANOTHER.                              
C DV7SCL... SCALES AND COPIES ONE VECTOR TO ANOTHER.                    
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      LOGICAL PARTJ                                                     
      INTEGER A0, A1, AJ, ALP1, BWA1, D0, DA0, DA1, DAJ, GPTR1, GRP1,   
     1        GRP2, I, I1, IN0, IN1, IN2, INI, INLEN, IPNTR1, IV1, IWA1,
     2        IWALEN, J1, JN1, JPNTR1, K, L1, LP1, M, M0, NF, NG, NGRP0,
     3        NGRP1, NGRP2, RSAVE0, RSAVE1, RSVLEN, X0I, XSAVE0, XSAVE1 
      DOUBLE PRECISION DELTA, DI, H, XI, XI1                            
      DOUBLE PRECISION NEGONE, ONE, ZERO                                
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER AMAT, COVREQ, D, DAMAT, DLTFDJ, GPTR, GRP, IN, IVNEED,    
     1        L1SAV, MAXGRP, MODE, MSAVE, NEXTIV, NEXTV, NFCALL, NFGCAL,
     2        PERM, RESTOR, TOOBIG, VNEED, XSAVE                        
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C/6                                                                     
C     DATA AMAT/113/, COVREQ/15/, D/27/, DAMAT/114/, DLTFDJ/43/,        
C    1     GPTR/117/, GRP/118/, IN/112/, IVNEED/3/, L1SAV/111/,         
C    2     MAXGRP/116/, MODE/35/, MSAVE/115/, NEXTIV/46/, NEXTV/47/,    
C    3     NFCALL/6/, NFGCAL/7/, PERM/58/, RESTOR/9/, TOOBIG/2/,        
C    4     VNEED/4/, XSAVE/119/                                         
C/7                                                                     
      PARAMETER (AMAT=113, COVREQ=15, D=27, DAMAT=114, DLTFDJ=43,       
     1           GPTR=117, GRP=118, IN=112, IVNEED=3, L1SAV=111,        
     2           MAXGRP=116, MODE=35, MSAVE=115, NEXTIV=46, NEXTV=47,   
     3           NFCALL=6, NFGCAL=7, PERM=58, RESTOR=9, TOOBIG=2,       
     4           VNEED=4, XSAVE=119)                                    
C/                                                                      
      DATA NEGONE/-1.D+0/, ONE/1.D+0/, ZERO/0.D+0/                      
C                                                                       
C++++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++ 
C                                                                       
      LP1 = L + 1                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)                  
      IF (P .LE. 0 .OR. L .LT. 0 .OR. IINC .LE. L) GO TO 80             
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 14) GO TO 120                                        
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 120                       
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      IF (IV(1) .NE. 13) GO TO 50                                       
C                                                                       
C  ***  FRESH START ***                                                 
C                                                                       
      IF (IV(PERM) .LE. XSAVE) IV(PERM) = XSAVE + 1                     
C                                                                       
C  ***  CHECK INC, COUNT ITS NONZEROS                                   
C                                                                       
      L1 = 0                                                            
      M = 0                                                             
      DO 40 I = 1, P                                                    
         IF (B(1,I) .GE. B(2,I)) GO TO 40                               
         M0 = M                                                         
         IF (L .EQ. 0) GO TO 20                                         
         DO 10 K = 1, L                                                 
            IF (INC(K,I) .LT. 0 .OR. INC(K,I) .GT. 1) GO TO 80          
            IF (INC(K,I) .EQ. 1) M = M + 1                              
 10         CONTINUE                                                    
 20      IF (INC(LP1,I) .NE. 1) GO TO 30                                
            M = M + 1                                                   
            L1 = 1                                                      
            GO TO 40                                                    
 30      IF (M .EQ. M0 .OR. INC(LP1,I) .LT. 0                           
     1                 .OR. INC(LP1,I) .GT. 1) GO TO 80                 
 40      CONTINUE                                                       
C                                                                       
C     *** NOW L1 = 1 MEANS A HAS COLUMN L+1 ***                         
C                                                                       
C     *** COMPUTE STORAGE REQUIREMENTS ***                              
C                                                                       
      IWALEN = MAX0(LP1, 6*P)                                           
      INLEN = 2 * M                                                     
      IV(IVNEED) = IV(IVNEED) + INLEN + 3*P + L + IWALEN + 3            
      RSVLEN = 2 * L1 * N                                               
      L1 = L + L1                                                       
      IV(VNEED) = IV(VNEED) + 2*N*L1 + RSVLEN + P                       
C                                                                       
 50   CALL DRNSGB(V, ALF, B, C, V, IV, IV, L, 1, N, LIV, LV, N, M, P, V,
     1            Y)                                                    
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(IN) = IV(NEXTIV)                                               
      IV(AMAT) = IV(NEXTV)                                              
      IV(DAMAT) = IV(AMAT) + N*L1                                       
      IV(XSAVE) = IV(DAMAT) + N*L1                                      
      IV(NEXTV) = IV(XSAVE) + P + RSVLEN                                
      IV(L1SAV) = L1                                                    
      IV(MSAVE) = M                                                     
C                                                                       
C  ***  DETERMINE HOW MANY GROUPS FOR FINITE DIFFERENCES                
C  ***  (SET UP TO CALL DSM)                                            
C                                                                       
      IN1 = IV(IN)                                                      
      JN1 = IN1 + M                                                     
      DO 70 K = 1, P                                                    
         IF (B(1,K) .GE. B(2,K)) GO TO 70                               
         DO 60 I = 1, LP1                                               
            IF (INC(I,K) .EQ. 0) GO TO 60                               
               IV(IN1) = I                                              
               IN1 = IN1 + 1                                            
               IV(JN1) = K                                              
               JN1 = JN1 + 1                                            
 60         CONTINUE                                                    
 70      CONTINUE                                                       
      IN1 = IV(IN)                                                      
      JN1 = IN1 + M                                                     
      IWA1 = IN1 + INLEN                                                
      NGRP1 = IWA1 + IWALEN                                             
      BWA1 = NGRP1 + P                                                  
      IPNTR1 = BWA1 + P                                                 
      JPNTR1 = IPNTR1 + L + 2                                           
      CALL DSM(LP1, P, M, IV(IN1), IV(JN1), IV(NGRP1), NG, K, I,        
     1         IV(IPNTR1), IV(JPNTR1), IV(IWA1), IWALEN, IV(BWA1))      
      IF (I .EQ. 1) GO TO 90                                            
         IV(1) = 69                                                     
         GO TO 50                                                       
 80   IV(1) = 66                                                        
      GO TO 50                                                          
C                                                                       
C  ***  SET UP GRP AND GPTR ARRAYS FOR COMPUTING FINITE DIFFERENCES     
C                                                                       
C  ***  THERE ARE NG GROUPS.  GROUP I CONTAINS ALF(GRP(J)) FOR          
C  ***  GPTR(I) .LE. J .LE. GPTR(I+1)-1.                                
C                                                                       
 90   IV(MAXGRP) = NG                                                   
      IV(GPTR) = IN1 + 2*L1                                             
      GPTR1 = IV(GPTR)                                                  
      IV(GRP) = GPTR1 + NG + 1                                          
      IV(NEXTIV) = IV(GRP) + P                                          
      GRP1 = IV(GRP)                                                    
      NGRP0 = NGRP1 - 1                                                 
      NGRP2 = NGRP0 + P                                                 
      DO 110 I = 1, NG                                                  
         IV(GPTR1) = GRP1                                               
         GPTR1 = GPTR1 + 1                                              
         DO 100 I1 = NGRP1, NGRP2                                       
            IF (IV(I1) .NE. I) GO TO 100                                
            K = I1 - NGRP0                                              
            IF (B(1,K) .GE. B(2,K)) GO TO 100                           
            IV(GRP1) = K                                                
            GRP1 = GRP1 + 1                                             
 100        CONTINUE                                                    
 110     CONTINUE                                                       
      IV(GPTR1) = GRP1                                                  
      IF (IV1 .EQ. 13) GO TO 999                                        
C                                                                       
C  ***  INITIALIZE POINTERS  ***                                        
C                                                                       
 120  A1 = IV(AMAT)                                                     
      A0 = A1 - N                                                       
      DA1 = IV(DAMAT)                                                   
      DA0 = DA1 - N                                                     
      IN1 = IV(IN)                                                      
      IN0 = IN1 - 2                                                     
      L1 = IV(L1SAV)                                                    
      IN2 = IN1 + 2*L1 - 1                                              
      D0 = IV(D) - 1                                                    
      NG = IV(MAXGRP)                                                   
      XSAVE1 = IV(XSAVE)                                                
      XSAVE0 = XSAVE1 - 1                                               
      RSAVE1 = XSAVE1 + P                                               
      RSAVE0 = RSAVE1 + N                                               
      ALP1 = A1 + L*N                                                   
      DELTA = V(DLTFDJ)                                                 
      IV(COVREQ) = -IABS(IV(COVREQ))                                    
C                                                                       
 130  CALL DRNSGB(V(A1), ALF, B, C, V(DA1), IV(IN1), IV, L, L1, N, LIV, 
     1            LV, N, L1, P, V, Y)                                   
      IF (IV(1)-2) 140, 150, 999                                        
C                                                                       
C  ***  NEW FUNCTION VALUE (R VALUE) NEEDED  ***                        
C                                                                       
 140  NF = IV(NFCALL)                                                   
      CALL CALCA(N, P, L, ALF, NF, V(A1), UIPARM, URPARM, UFPARM)       
      IF (NF .LE. 0) IV(TOOBIG) = 1                                     
      IF (L1 .LE. L) GO TO 130                                          
      IF (IV(RESTOR) .EQ. 2) CALL DV7CPY(N, V(RSAVE0), V(RSAVE1))       
      CALL DV7CPY(N, V(RSAVE1), V(ALP1))                                
      GO TO 130                                                         
C                                                                       
C  ***  COMPUTE DR = GRADIENT OF R COMPONENTS  ***                      
C                                                                       
 150  IF (L1 .GT. L .AND. IV(NFGCAL) .EQ. IV(NFCALL))                   
     1      CALL DV7CPY(N, V(RSAVE0), V(RSAVE1))                        
      GPTR1 = IV(GPTR)                                                  
      DO 260 K = 1, NG                                                  
         CALL DV7CPY(P, V(XSAVE1), ALF)                                 
         GRP1 = IV(GPTR1)                                               
         GRP2 = IV(GPTR1+1) - 1                                         
         GPTR1 = GPTR1 + 1                                              
         DO 180 I1 = GRP1, GRP2                                         
            I = IV(I1)                                                  
            XI = ALF(I)                                                 
            J1 = D0 + I                                                 
            DI = V(J1)                                                  
            IF (DI .LE. ZERO) DI = ONE                                  
            H = DELTA * DMAX1(DABS(XI), ONE/DI)                         
            IF (XI .LT. ZERO) GO TO 160                                 
               XI1 = XI + H                                             
               IF (XI1 .LE. B(2,I)) GO TO 170                           
               XI1 = XI - H                                             
               IF (XI1 .GE. B(1,I)) GO TO 170                           
               GO TO 190                                                
 160         XI1 = XI - H                                               
             IF (XI1 .GE. B(1,I)) GO TO 170                             
             XI1 = XI + H                                               
             IF (XI1 .LE. B(2,I)) GO TO 170                             
             GO TO 190                                                  
 170        X0I = XSAVE0 + I                                            
            V(X0I) = XI1                                                
 180        CONTINUE                                                    
         CALL CALCA(N, P, L, V(XSAVE1), NF, V(DA1), UIPARM, URPARM,     
     1              UFPARM)                                             
         IF (IV(NFGCAL) .GT. 0) GO TO 200                               
 190        IV(TOOBIG) = 1                                              
            GO TO 130                                                   
 200     JN1 = IN1                                                      
         DO 210 I = IN1, IN2                                            
 210        IV(I) = 0                                                   
         PARTJ = IV(MODE) .LE. P                                        
         DO 250 I1 = GRP1, GRP2                                         
            I = IV(I1)                                                  
            DO 240 J1 = 1, L1                                           
               IF (INC(J1,I) .EQ. 0) GO TO 240                          
               INI = IN0 + 2*J1                                         
               IV(INI) = I                                              
               IV(INI+1) = J1                                           
               X0I = XSAVE0 + I                                         
               H = ONE / (V(X0I) - ALF(I))                              
               DAJ = DA0 + J1*N                                         
               IF (PARTJ) GO TO 220                                     
C                 *** FULL FINITE DIFFERENCE FOR COV. AND REG. DIAG. ***
                  AJ = A0 + J1*N                                        
                  CALL DV2AXY(N, V(DAJ), NEGONE, V(AJ), V(DAJ))         
                  GO TO 230                                             
 220           IF (J1 .GT. L)                                           
     1            CALL DV2AXY(N, V(DAJ), NEGONE, V(RSAVE0), V(DAJ))     
 230           CALL DV7SCL(N, V(DAJ), H, V(DAJ))                        
 240           CONTINUE                                                 
 250        CONTINUE                                                    
         IF (K .GE. NG) GO TO 270                                       
         IV(1) = -2                                                     
         CALL DRNSGB(V(A1), ALF, B, C, V(DA1), IV(IN1), IV, L, L1, N,   
     1               LIV, LV, N, L1, P, V, Y)                           
         IF (-2 .NE. IV(1)) GO TO 999                                   
 260     CONTINUE                                                       
 270  IV(1) = 2                                                         
      GO TO 130                                                         
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF  DNSFB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  DNSGB(N, P, L, ALF, B, C, Y, CALCA, CALCB, INC, IINC, 
     1                  IV, LIV, LV, V, UIPARM, URPARM, UFPARM)         
C                                                                       
C  ***  SOLVE SEPARABLE NONLINEAR LEAST SQUARES USING  ***              
C  ***  ANALYTICALLY COMPUTED DERIVATIVES.             ***              
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER IINC, L, LIV, LV, N, P                                    
C/6                                                                     
C     INTEGER INC(IINC,P), IV(LIV), UIPARM(1)                           
C     DOUBLE PRECISION ALF(P), B(2,P), C(L), URPARM(1), V(LV), Y(N)     
C/7                                                                     
      INTEGER INC(IINC,P), IV(LIV), UIPARM(*)                           
      DOUBLE PRECISION ALF(P), B(2,P), C(L), URPARM(*), V(LV), Y(N)     
C/                                                                      
      EXTERNAL CALCA, CALCB, UFPARM                                     
C                                                                       
C  ***  PURPOSE  ***                                                    
C                                                                       
C GIVEN A SET OF N OBSERVATIONS Y(1)....Y(N) OF A DEPENDENT VARIABLE    
C T(1)...T(N),  DNSGB ATTEMPTS TO COMPUTE A LEAST SQUARES FIT           
C TO A FUNCTION  ETA  (THE MODEL) WHICH IS A LINEAR COMBINATION         
C                                                                       
C                  L                                                    
C ETA(C,ALF,T) =  SUM C * PHI(ALF,T) +PHI   (ALF,T)                     
C                 J=1  J     J           L+1                            
C                                                                       
C OF NONLINEAR FUNCTIONS PHI(J) DEPENDENT ON T AND ALF(1),...,ALF(P)    
C (.E.G. A SUM OF EXPONENTIALS OR GAUSSIANS).  THAT IS, IT DETERMINES   
C NONLINEAR PARAMETERS ALF WHICH MINIMIZE                               
C                                                                       
C                   2    N                      2                       
C     NORM(RESIDUAL)  = SUM  (Y - ETA(C,ALF,T )) ,                      
C                       I=1    I             I                          
C                                                                       
C SUBJECT TO THE SIMPLE BOUND CONSTRAINTS                               
C B(1,I) .LE. ALF(I) .LE. B(2,I), C I = 1(1)P.                          
C                                                                       
C THE (L+1)ST TERM IS OPTIONAL.                                         
C                                                                       
C--------------------------  PARAMETER USAGE  ------------------------- 
C                                                                       
C INPUT PARAMETERS                                                      
C                                                                       
C N     INTEGER        NUMBER OF OBSERVATIONS (MUST BE .GE. MAX(L,P)).  
C                                                                       
C P     INTEGER        NUMBER OF NONLINEAR PARAMETERS (MUST BE .GE. 1). 
C                                                                       
C L     INTEGER        NUMBER OF LINEAR PARAMETERS (MUST BE .GE. 0).    
C                                                                       
C ALF   D.P. ARRAY     P VECTOR = INITIAL ESTIMATE OF THE NONLINEAR     
C                      PARAMETERS.                                      
C                                                                       
C CALCA SUBROUTINE     USER PROVIDED FUNCTION TO CALCULATE THE MODEL    
C                      (I.E., TO CALCULATE PHI) -- SEE THE NOTE BELOW   
C                      ON THE CALLING SEQUENCE FOR CALCA.               
C                      CALCA MUST BE DECLARED EXTERNAL IN THE CALLING   
C                      PROGRAM.                                         
C                                                                       
C CALCB SUBROUTINE     USER PROVIDED FUNCTION TO CALCULATE THE DERIVA-  
C                      TIVE OF THE MODEL (I.E., OF PHI) WITH RESPECT TO 
C                      ALF -- SEE THE NOTE BELOW ON THE CALLING         
C                      SEQUENCE FOR CALCB.  CALCB MUST BE DECLARED      
C                      EXTERNAL IN THE CALLING PROGRAM.                 
C                                                                       
C Y     D.P. ARRAY     VECTOR OF OBSERVATIONS.                          
C                                                                       
C INC   INTEGER ARRAY  A 2 DIM. ARRAY OF DIMENSION AT LEAST (L+1,P)     
C                      INDICATING THE POSITION OF THE NONLINEAR PARA-   
C                      METERS IN THE MODEL.  SET INC(J,K) = 1 IF ALF(K) 
C                      APPEARS IN PHI(J).  OTHERWISE SET INC(J,K) = 0.  
C                      IF PHI((L+1)) IS NOT IN THE MODEL, SET THE L+1ST 
C                      ROW OF INC TO ALL ZEROS.  EVERY COLUMN OF INC    
C                      MUST CONTAIN AT LEAST ONE 1.                     
C                                                                       
C IINC   INTEGER       DECLARED ROW DIMENSION OF INC, WHICH MUST BE AT  
C                      LEAST L+1.                                       
C                                                                       
C IV     INTEGER       ARRAY OF LENGTH AT LEAST LIV THAT CONTAINS       
C                      VARIOUS PARAMETERS FOR THE SUBROUTINE, SUCH AS   
C                      THE ITERATION AND FUNCTION EVALUATION LIMITS AND 
C                      SWITCHES THAT CONTROL PRINTING.  THE INPUT COM-  
C                      PONENTS OF IV ARE DESCRIBED IN DETAIL IN THE     
C                      PORT OPTIMIZATION DOCUMENTATION.                 
C                         IF IV(1)=0 ON INPUT, THEN DEFAULT PARAMETERS  
C                      ARE SUPPLIED TO IV AND V.  THE CALLER MAY SUPPLY 
C                      NONDEFAULT PARAMETERS TO IV AND V BY EXECUTING A 
C                      CALL DIVSET(1,IV,LIV,LV,V) AND THEN ASSIGNING    
C                      NONDEFAULT VALUES TO THE APPROPRIATE COMPONENTS  
C                      OF IV AND V BEFORE CALLING  DNSGB.               
C                                                                       
C LIV     INTEGER      LENGTH OF IV.  MUST BE AT LEAST                  
C                      115 + 4*P + L + 2*M,                             
C                      WHERE  M  IS THE NUMBER OF ONES IN INC.          
C                                                                       
C LV      INTEGER      LENGTH OF V.  MUST BE AT LEAST                   
C                      105 + N*(L+M+P+3) + L*(L+3)/2 + P*(2*P+21),      
C                      WHERE  M  IS AS FOR LIV (SEE ABOVE).  IF THE     
C                      LAST ROW OF INC CONTAINS ONLY ZEROS, THEN LV     
C                      CAN BE N LESS THAN JUST DESCRIBED.               
C                                                                       
C V       D.P. ARRAY   WORK AND PARAMETER ARRAY OF LENGTH AT LEAST LV   
C                      THAT CONTAINS SUCH INPUT COMPONENTS AS THE       
C                      CONVERGENCE TOLERANCES.  THE INPUT COMPONENTS OF 
C                      V MAY BE SUPPLIED AS FOR IV (SEE ABOVE).  NOTE   
C                      THAT V(35) CONTAINS THE INITIAL STEP BOUND,      
C                      WHICH, IF TOO LARGE, MAY LEAD TO OVERFLOW.       
C                                                                       
C UIPARM INTEGER ARRAY SCRATCH SPACE FOR USER TO SEND INFORMATION       
C                      TO CALCA AND CALCB.                              
C                                                                       
C URPARM D.P. ARRAY    SCRATCH SPACE FOR USER TO SEND INFORMATION       
C                      TO CALCA AND CALCB.                              
C                                                                       
C UFPARM EXTERNAL      SUBROUTINE SENT TO CALCA AND CALCB FOR THEIR     
C                      USE.  NOTE THAT THE SUBROUTINE PASSED FOR UFPARM 
C                      MUST BE DECLARED EXTERNAL IN THE CALLING PROGRAM.
C                                                                       
C                                                                       
C OUTPUT PARAMETERS                                                     
C                                                                       
C ALF    D.P. ARRAY    FINAL NONLINEAR PARAMETERS.                      
C                                                                       
C C      D.P. ARRAY    L VECTOR OF LINEAR PARAMETERS -- NOTE THAT NO    
C                      INITIAL GUESS FOR C IS REQUIRED.                 
C                                                                       
C IV                   IV(1) CONTAINS A RETURN CODE DESCRIBED IN THE    
C                      PORT OPTIMIZATION DOCUMENTATION.  IF IV(1) LIES  
C                      BETWEEN 3 AND 7, THEN THE ALGORITHM HAS          
C                      CONVERGED (BUT IV(1) = 7 INDICATES POSSIBLE      
C                      TROUBLE WITH THE MODEL).  IV(1) = 9 OR 10 MEANS  
C                      FUNCTION EVALUATION OR ITERATION LIMIT REACHED.  
C                      IV(1) = 66 MEANS BAD PARAMETERS (INCLUDING A     
C                      COLUMN OF ZEROS IN INC).  NOTE THAT THE          
C                      ALGORITHM CAN BE RESTARTED AFTER ANY RETURN WITH 
C                      IV(1) .LT. 12 -- SEE THE PORT DOCUMENTATION.     
C                                                                       
C V                    VARIOUS ITEMS OF INTEREST, INCLUDING THE NORM OF 
C                      THE GRADIENT(1) AND THE FUNCTION VALUE(10).  SEE 
C                      THE PORT DOCUMENTATION FOR A COMPLETE LIST.      
C                                                                       
C                                                                       
C                                                                       
C PARAMETERS FOR CALCA(N,P,L,ALF,NF,PHI, UIPARM,URPARM,UFPARM)          
C                                                                       
C N,L,P,ALF ARE INPUT PARAMETERS AS DESCRIBED ABOVE                     
C                                                                       
C PHI    D.P. ARRAY  N*(L+1) ARRAY WHOSE COLUMNS CONTAIN THE TERMS OF   
C                    THE MODEL.  CALCA MUST EVALUATE PHI(ALF) AND STORE 
C                    THE RESULT IN PHI.  IF THE (L+1)ST TERM IS NOT IN  
C                    THE MODEL, THEN NOTHING SHOULD BE STORED IN THE    
C                    (L+1)ST COLUMN OF PHI.                             
C                                                                       
C NF     INTEGER     CURRENT INVOCATION COUNT FOR CALCA.  IF PHI CANNOT 
C                    BE EVALUATED AT ALF (E.G. BECAUSE AN ARGUMENT TO   
C                    AN INTRINSIC FUNCTION IS OUT OF RANGE), THEN CALCA 
C                    SHOULD SIMPLY SET NF TO 0 AND RETURN.  THIS        
C                    TELLS THE ALGORITHM TO TRY A SMALLER STEP.         
C                                                                       
C UIPARM,URPARM,UFPARM ARE AS DESCRIBED ABOVE                           
C                                                                       
C N.B. THE DEPENDENT VARIABLE T IS NOT EXPLICITLY PASSED.  IF REQUIRED, 
C IT MAY BE PASSED IN UIPARM OR URPARM OR STORED IN NAMED COMMON.       
C                                                                       
C                                                                       
C PARAMETERS FOR CALCB(N,P,L,ALF,NF,DER, UIPARM,URPARM,UFPARM)          
C                                                                       
C N,P,L,ALF,NF,UIPARM,URPARM,UFPARM ARE AS FOR CALCA                    
C                                                                       
C DER   D.P. ARRAY   N*M ARRAY, WHERE M IS THE NUMBER OF ONES IN INC.   
C                    CALCB MUST SET DER TO THE DERIVATIVES OF THE MODEL 
C                    WITH RESPECT TO ALF.  IF THE MODEL HAS K TERMS THAT
C                    DEPEND ON ALF(I), THEN DER WILL HAVE K CONSECUTIVE 
C                    COLUMNS OF DERIVATIVES WITH RESPECT TO ALF(I).  THE
C                    COLUMNS OF DER CORRESPOND TO THE ONES IN INC WHEN  
C                    ONE TRAVELS THROUGH INC BY COLUMNS.  FOR EXAMPLE,  
C                    IF INC HAS THE FORM...                             
C                      1  1  0                                          
C                      0  1  0                                          
C                      1  0  0                                          
C                      0  0  1                                          
C                    THEN THE FIRST TWO COLUMNS OF DER ARE FOR THE      
C                    DERIVATIVES OF COLUMNS 1 AND 3 OF PHI WITH RESPECT 
C                    TO ALF(1), COLUMNS 3 AND 4 OF DER ARE FOR THE      
C                    DERIVATIVES OF COLUMNS 1 AND 2 OF PHI WITH RESPECT 
C                    TO ALF(2), AND COLUMN 5 OF DER IS FOR THE DERIVA-  
C                    TIVE OF COLUMN 4 OF PHI WITH RESPECT TO ALF(3).    
C                    MORE SPECIFICALLY, DER(I,2) IS FOR THE DERIVATIVE  
C                    OF PHI(I,3) WITH RESPECT TO ALF(1) AND DER(I,5) IS 
C                    FOR THE DERIVATIVE OF PHI(I,4) WITH RESPECT TO     
C                    ALF(3) (FOR I = 1,2,...,N).                        
C                       THE VALUE OF ALF PASSED TO CALCB IS THE SAME AS 
C                    THAT PASSED TO CALCA THE LAST TIME IT WAS CALLED.  
C                    (IF DER CANNOT BE EVALUATED, THEN CALCB SHOULD SET 
C                    NF TO 0.  THIS WILL CAUSE AN ERROR RETURN.)        
C                                                                       
C N.B. DER IS FOR DERIVATIVES WITH RESPECT TO ALF, NOT T.               
C                                                                       
C------------------------------  NOTES  ------------------------------- 
C                                                                       
C      THIS PROGRAM WAS WRITTEN BY LINDA KAUFMAN AT BELL LABS, MURRAY   
C HILL, N.J. IN 1977 AND EXTENSIVELY REVISED BY HER AND DAVID GAY IN    
C 1980, 1981, 1983, 1984.  THE WORK OF DAVID GAY WAS SUPPORTED IN PART  
C BY NATIONAL SCIENCE FOUNDATION GRANT MCS-7906671.                     
C                                                                       
C--------------------------  DECLARATIONS  ---------------------------- 
C                                                                       
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      EXTERNAL DIVSET, DRNSGB                                           
C                                                                       
C DIVSET.... PROVIDES DEFAULT IV AND V VALUES.                          
C DRNSGB... CARRIES OUT NL2SOL ALGORITHM.                               
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER A1, DA1, I, IN1, IV1, K, L1, LP1, M, M0, NF               
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER AMAT, DAMAT, IN, IVNEED, L1SAV, MSAVE, NEXTIV,            
     1        NEXTV, NFCALL, NFGCAL, PERM, TOOBIG, VNEED                
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C/6                                                                     
C     DATA AMAT/113/, DAMAT/114/, IN/112/, IVNEED/3/, L1SAV/111/,       
C    1     MSAVE/115/, NEXTIV/46/, NEXTV/47/, NFCALL/6/, NFGCAL/7/,     
C    2     PERM/58/, TOOBIG/2/, VNEED/4/                                
C/7                                                                     
      PARAMETER (AMAT=113, DAMAT=114, IN=112, IVNEED=3, L1SAV=111,      
     1           MSAVE=115, NEXTIV=46, NEXTV=47, NFCALL=6, NFGCAL=7,    
     2           PERM=58, TOOBIG=2, VNEED=4)                            
C/                                                                      
C                                                                       
C++++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++ 
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)                  
      IF (P .LE. 0 .OR. L .LT. 0 .OR. IINC .LE. L) GO TO 50             
      IV1 = IV(1)                                                       
      IF (IV1 .EQ. 14) GO TO 90                                         
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 90                        
      IF (IV1 .EQ. 12) IV(1) = 13                                       
      IF (IV(1) .NE. 13) GO TO 60                                       
      IF (IV(PERM) .LE. MSAVE) IV(PERM) = MSAVE + 1                     
      LP1 = L + 1                                                       
      L1 = 0                                                            
      M = 0                                                             
      DO 40 I = 1, P                                                    
         M0 = M                                                         
         IF (L .EQ. 0) GO TO 20                                         
         DO 10 K = 1, L                                                 
            IF (INC(K,I) .LT. 0 .OR. INC(K,I) .GT. 1) GO TO 50          
            IF (INC(K,I) .EQ. 1) M = M + 1                              
 10         CONTINUE                                                    
 20      IF (INC(LP1,I) .NE. 1) GO TO 30                                
            M = M + 1                                                   
            L1 = 1                                                      
 30      IF (M .EQ. M0 .OR. INC(LP1,I) .LT. 0                           
     1                 .OR. INC(LP1,I) .GT. 1) GO TO 50                 
 40      CONTINUE                                                       
C                                                                       
      IV(IVNEED) = IV(IVNEED) + 2*M                                     
      L1 = L + L1                                                       
      IV(VNEED) = IV(VNEED) + N*(L1+M)                                  
      GO TO 60                                                          
C                                                                       
 50   IV(1) = 66                                                        
C                                                                       
 60   CALL DRNSGB(V, ALF, B, C, V, IV, IV, L, 1, N, LIV, LV, N, M, P, V,
     1            Y)                                                    
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(IN) = IV(NEXTIV)                                               
      IV(NEXTIV) = IV(IN) + 2*M                                         
      IV(AMAT) = IV(NEXTV)                                              
      IV(DAMAT) = IV(AMAT) + N*L1                                       
      IV(NEXTV) = IV(DAMAT) + N*M                                       
      IV(L1SAV) = L1                                                    
      IV(MSAVE) = M                                                     
C                                                                       
C  ***  SET UP IN ARRAY  ***                                            
C                                                                       
      IN1 = IV(IN)                                                      
      DO 80 I = 1, P                                                    
         DO 70 K = 1, LP1                                               
            IF (INC(K,I) .EQ. 0) GO TO 70                               
               IV(IN1) = I                                              
               IV(IN1+1) = K                                            
               IN1 = IN1 + 2                                            
 70         CONTINUE                                                    
 80      CONTINUE                                                       
      IF (IV1 .EQ. 13) GO TO 999                                        
C                                                                       
 90   A1 = IV(AMAT)                                                     
      DA1 = IV(DAMAT)                                                   
      IN1 = IV(IN)                                                      
      L1 = IV(L1SAV)                                                    
      M = IV(MSAVE)                                                     
C                                                                       
 100  CALL DRNSGB(V(A1), ALF, B, C, V(DA1), IV(IN1), IV, L, L1, N, LIV, 
     1            LV, N, M, P, V, Y)                                    
      IF (IV(1)-2) 110, 120, 999                                        
C                                                                       
C  ***  NEW FUNCTION VALUE (R VALUE) NEEDED  ***                        
C                                                                       
 110  NF = IV(NFCALL)                                                   
      CALL CALCA(N, P, L, ALF, NF, V(A1), UIPARM, URPARM, UFPARM)       
      IF (NF .LE. 0) IV(TOOBIG) = 1                                     
      GO TO 100                                                         
C                                                                       
C  ***  COMPUTE DR = GRADIENT OF R COMPONENTS  ***                      
C                                                                       
 120  CALL CALCB(N, P, L, ALF, IV(NFGCAL), V(DA1), UIPARM, URPARM,      
     1           UFPARM)                                                
      IF (IV(NFGCAL) .EQ. 0) IV(TOOBIG) = 1                             
      GO TO 100                                                         
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF  DNSGB FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DRNSGB(A, ALF, B, C, DA, IN, IV, L, L1, LA, LIV, LV,   
     1                  N, NDA, P, V, Y)                                
C                                                                       
C  ***  ITERATION DRIVER FOR SEPARABLE NONLINEAR LEAST SQUARES,         
C  ***  WITH SIMPLE BOUNDS ON THE NONLINEAR VARIABLES.                  
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER L, L1, LA, LIV, LV, N, NDA, P                             
      INTEGER IN(2,NDA), IV(LIV)                                        
C     DIMENSION UIPARM(*)                                               
      DOUBLE PRECISION A(LA,L1), ALF(P), B(2,P), C(L), DA(LA,NDA),      
     1                 V(LV), Y(N)                                      
C                                                                       
C  ***  PURPOSE  ***                                                    
C                                                                       
C GIVEN A SET OF N OBSERVATIONS Y(1)....Y(N) OF A DEPENDENT VARIABLE    
C T(1)...T(N), DRNSGB ATTEMPTS TO COMPUTE A LEAST SQUARES FIT           
C TO A FUNCTION  ETA  (THE MODEL) WHICH IS A LINEAR COMBINATION         
C                                                                       
C                  L                                                    
C ETA(C,ALF,T) =  SUM C * PHI(ALF,T) +PHI   (ALF,T)                     
C                 J=1  J     J           L+1                            
C                                                                       
C OF NONLINEAR FUNCTIONS PHI(J) DEPENDENT ON T AND ALF(1),...,ALF(P)    
C (.E.G. A SUM OF EXPONENTIALS OR GAUSSIANS).  THAT IS, IT DETERMINES   
C NONLINEAR PARAMETERS ALF WHICH MINIMIZE                               
C                                                                       
C                   2    N                      2                       
C     NORM(RESIDUAL)  = SUM  (Y - ETA(C,ALF,T )) ,                      
C                       I=1    I             I                          
C                                                                       
C SUBJECT TO THE SIMPLE BOUND CONSTRAINTS                               
C B(1,I) .LE. ALF(I) .LE. B(2,I), I = 1(1)P.                            
C                                                                       
C THE (L+1)ST TERM IS OPTIONAL.                                         
C                                                                       
C                                                                       
C  ***  PARAMETERS  ***                                                 
C                                                                       
C      A (IN)  MATRIX PHI(ALF,T) OF THE MODEL.                          
C    ALF (I/O) NONLINEAR PARAMETERS.                                    
C                 INPUT = INITIAL GUESS,                                
C                 OUTPUT = BEST ESTIMATE FOUND.                         
C      C (OUT) LINEAR PARAMETERS (ESTIMATED).                           
C     DA (IN)  DERIVATIVES OF COLUMNS OF A WITH RESPECT TO COMPONENTS   
C                 OF ALF, AS SPECIFIED BY THE IN ARRAY...               
C     IN (IN)  WHEN DRNSGB IS CALLED WITH IV(1) = 2 OR -2, THEN FOR     
C                 I = 1(1)NDA, COLUMN I OF DA IS THE PARTIAL            
C                 DERIVATIVE WITH RESPECT TO ALF(IN(1,I)) OF COLUMN     
C                 IN(2,I) OF A, UNLESS IV(1,I) IS NOT POSITIVE (IN      
C                 WHICH CASE COLUMN I OF DA IS IGNORED.  IV(1) = -2     
C                 MEANS THERE ARE MORE COLUMNS OF DA TO COME AND        
C                 DRNSGB SHOULD RETURN FOR THEM.                        
C     IV (I/O) INTEGER PARAMETER AND SCRATCH VECTOR.  DRNSGB RETURNS    
C                 WITH IV(1) = 1 WHEN IT WANTS A TO BE EVALUATED AT     
C                 ALF AND WITH IV(1) = 2 WHEN IT WANTS DA TO BE         
C                 EVALUATED AT ALF.  WHEN CALLED WITH IV(1) = -2        
C                 (AFTER A RETURN WITH IV(1) = 2), DRNSGB RETURNS       
C                 WITH IV(1) = -2 TO GET MORE COLUMNS OF DA.            
C      L (IN)  NUMBER OF LINEAR PARAMETERS TO BE ESTIMATED.             
C     L1 (IN)  L+1 IF PHI(L+1) IS IN THE MODEL, L IF NOT.               
C     LA (IN)  LEAD DIMENSION OF A.  MUST BE AT LEAST N.                
C    LIV (IN)  LENGTH OF IV.  MUST BE AT LEAST 110 + L + 4*P.           
C     LV (IN)  LENGTH OF V.  MUST BE AT LEAST                           
C                 105 + 2*N + L*(L+3)/2 + P*(2*P + 21 + N).             
C      N (IN)  NUMBER OF OBSERVATIONS.                                  
C    NDA (IN)  NUMBER OF COLUMNS IN DA AND IN.                          
C      P (IN)  NUMBER OF NONLINEAR PARAMETERS TO BE ESTIMATED.          
C      V (I/O) FLOATING-POINT PARAMETER AND SCRATCH VECTOR.             
C      Y (IN)  RIGHT-HAND SIDE VECTOR.                                  
C                                                                       
C                                                                       
C  ***  EXTERNAL SUBROUTINES  ***                                       
C                                                                       
      DOUBLE PRECISION DL7SVX, DL7SVN, DR7MDC                           
      EXTERNAL DIVSET,DITSUM, DL7ITV, DL7SVX, DL7SVN, DRN2GB, DQ7APL,   
     1        DQ7RFH, DR7MDC, DS7CPR,DV2AXY,DV7CPY,DV7PRM, DV7SCP       
C                                                                       
C DIVSET.... SUPPLIES DEFAULT PARAMETER VALUES.                         
C DITSUM.... PRINTS ITERATION SUMMARY, INITIAL AND FINAL ALF.           
C DL7ITV... APPLIES INVERSE-TRANSPOSE OF COMPACT LOWER TRIANG. MATRIX.  
C DL7SVX... ESTIMATES LARGEST SING. VALUE OF LOWER TRIANG. MATRIX.      
C DL7SVN... ESTIMATES SMALLEST SING. VALUE OF LOWER TRIANG. MATRIX.     
C DRN2GB... UNDERLYING NONLINEAR LEAST-SQUARES SOLVER.                  
C DQ7APL... APPLIES HOUSEHOLDER TRANSFORMS STORED BY DQ7RFH.            
C DQ7RFH.... COMPUTES QR FACT. VIA HOUSEHOLDER TRANSFORMS WITH PIVOTING.
C DR7MDC... RETURNS MACHINE-DEP. CONSTANTS.                             
C DS7CPR... PRINTS LINEAR PARAMETERS AT SOLUTION.                       
C DV2AXY.... ADDS MULTIPLE OF ONE VECTOR TO ANOTHER.                    
C DV7CPY.... COPIES ONE VECTOR TO ANOTHER.                              
C DV7PRM.... PERMUTES VECTOR.                                           
C DV7SCL... SCALES AND COPIES ONE VECTOR TO ANOTHER.                    
C                                                                       
C  ***  LOCAL VARIABLES  ***                                            
C                                                                       
      INTEGER AR1, CSAVE1, D1, DR1, DR1L, I, I1,                        
     1        IPIV1, IER, IV1, J1, JLEN, K, LL1O2, MD, N1,              
     2        NML, NRAN, R1, R1L, RD1                                   
      DOUBLE PRECISION SINGTL, T                                        
      DOUBLE PRECISION MACHEP, NEGONE, SNGFAC, ZERO                     
C                                                                       
C  ***  SUBSCRIPTS FOR IV AND V  ***                                    
C                                                                       
      INTEGER AR, CSAVE, D, IERS, IPIVS, IV1SAV,                        
     2        IVNEED, J, MODE, NEXTIV, NEXTV,                           
     2        NFCALL, NFGCAL, PERM, R,                                  
     3        REGD, REGD0, RESTOR, TOOBIG, VNEED                        
C                                                                       
C  ***  IV SUBSCRIPT VALUES  ***                                        
C                                                                       
C/6                                                                     
C     DATA AR/110/, CSAVE/105/, D/27/, IERS/108/, IPIVS/109/,           
C    1     IV1SAV/104/, IVNEED/3/, J/70/, MODE/35/, NEXTIV/46/,         
C    2     NEXTV/47/, NFCALL/6/, NFGCAL/7/, PERM/58/, R/61/, REGD/67/,  
C    3     REGD0/82/, RESTOR/9/, TOOBIG/2/, VNEED/4/                    
C/7                                                                     
      PARAMETER (AR=110, CSAVE=105, D=27, IERS=108, IPIVS=109,          
     1           IV1SAV=104, IVNEED=3, J=70, MODE=35, NEXTIV=46,        
     2           NEXTV=47, NFCALL=6, NFGCAL=7, PERM=58, R=61, REGD=67,  
     3           REGD0=82, RESTOR=9, TOOBIG=2, VNEED=4)                 
C/                                                                      
      DATA MACHEP/-1.D+0/, NEGONE/-1.D+0/, SNGFAC/1.D+2/, ZERO/0.D+0/   
C                                                                       
C++++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++ 
C                                                                       
C                                                                       
      IF (IV(1) .EQ. 0) CALL DIVSET(1, IV, LIV, LV, V)                  
      N1 = 1                                                            
      NML = N                                                           
      IV1 = IV(1)                                                       
      IF (IV1 .LE. 2) GO TO 20                                          
C                                                                       
C  ***  CHECK INPUT INTEGERS  ***                                       
C                                                                       
      IF (P .LE. 0) GO TO 240                                           
      IF (L .LT. 0) GO TO 240                                           
      IF (N .LE. L) GO TO 240                                           
      IF (LA .LT. N) GO TO 240                                          
      IF (IV1 .LT. 12) GO TO 20                                         
      IF (IV1 .EQ. 14) GO TO 20                                         
      IF (IV1 .EQ. 12) IV(1) = 13                                       
C                                                                       
C  ***  FRESH START -- COMPUTE STORAGE REQUIREMENTS  ***                
C                                                                       
      IF (IV(1) .GT. 16) GO TO 240                                      
      LL1O2 = L*(L+1)/2                                                 
      JLEN = N*P                                                        
      I = L + P                                                         
      IF (IV(1) .NE. 13) GO TO 10                                       
         IV(IVNEED) = IV(IVNEED) + L                                    
         IV(VNEED) = IV(VNEED) + P + 2*N + JLEN + LL1O2 + L             
 10   IF (IV(PERM) .LE. AR) IV(PERM) = AR + 1                           
      CALL DRN2GB(B, V, V, IV, LIV, LV, N, N, N1, NML, P, V, V, V, ALF) 
      IF (IV(1) .NE. 14) GO TO 999                                      
C                                                                       
C  ***  STORAGE ALLOCATION  ***                                         
C                                                                       
      IV(IPIVS) = IV(NEXTIV)                                            
      IV(NEXTIV) = IV(NEXTIV) + L                                       
      IV(D) = IV(NEXTV)                                                 
      IV(REGD0) = IV(D) + P                                             
      IV(AR) = IV(REGD0) + N                                            
      IV(CSAVE) = IV(AR) + LL1O2                                        
      IV(J) = IV(CSAVE) + L                                             
      IV(R) = IV(J) + JLEN                                              
      IV(NEXTV) = IV(R) + N                                             
      IV(IERS) = 0                                                      
      IF (IV1 .EQ. 13) GO TO 999                                        
C                                                                       
C  ***  SET POINTERS INTO IV AND V  ***                                 
C                                                                       
 20   AR1 = IV(AR)                                                      
      D1 = IV(D)                                                        
      DR1 = IV(J)                                                       
      DR1L = DR1 + L                                                    
      R1 = IV(R)                                                        
      R1L = R1 + L                                                      
      RD1 = IV(REGD0)                                                   
      CSAVE1 = IV(CSAVE)                                                
      NML = N - L                                                       
      IF (IV1 .LE. 2) GO TO 50                                          
C                                                                       
 30   CALL DRN2GB(B, V(D1), V(DR1L), IV, LIV, LV, NML, N, N1, NML, P,   
     1            V(R1L), V(RD1), V, ALF)                               
      IF (IABS(IV(RESTOR)-2) .EQ. 1 .AND. L .GT. 0)                     
     1        CALL DV7CPY(L, C, V(CSAVE1))                              
      IV1 = IV(1)                                                       
      IF (IV1-2) 40, 150, 230                                           
C                                                                       
C  ***  NEW FUNCTION VALUE (RESIDUAL) NEEDED  ***                       
C                                                                       
 40   IV(IV1SAV) = IV(1)                                                
      IV(1) = IABS(IV1)                                                 
      IF (IV(RESTOR) .EQ. 2 .AND. L .GT. 0) CALL DV7CPY(L, V(CSAVE1), C)
      GO TO 999                                                         
C                                                                       
C  ***  COMPUTE NEW RESIDUAL OR GRADIENT  ***                           
C                                                                       
 50   IV(1) = IV(IV1SAV)                                                
      MD = IV(MODE)                                                     
      IF (MD .LE. 0) GO TO 60                                           
         NML = N                                                        
         DR1L = DR1                                                     
         R1L = R1                                                       
 60   IF (IV(TOOBIG) .NE. 0) GO TO 30                                   
      IF (IABS(IV1) .EQ. 2) GO TO 170                                   
C                                                                       
C  ***  COMPUTE NEW RESIDUAL  ***                                       
C                                                                       
      IF (L1 .LE. L) CALL DV7CPY(N, V(R1), Y)                           
      IF (L1 .GT. L) CALL DV2AXY(N, V(R1), NEGONE, A(1,L1), Y)          
      IF (MD .GT. 0) GO TO 120                                          
      IER = 0                                                           
      IF (L .LE. 0) GO TO 110                                           
      LL1O2 = L * (L + 1) / 2                                           
      IPIV1 = IV(IPIVS)                                                 
      CALL DQ7RFH(IER, IV(IPIV1), N, LA, 0, L, A, V(AR1), LL1O2, C)     
C                                                                       
C *** DETERMINE NUMERICAL RANK OF A ***                                 
C                                                                       
      IF (MACHEP .LE. ZERO) MACHEP = DR7MDC(3)                          
      SINGTL = SNGFAC * FLOAT(MAX0(L,N)) * MACHEP                       
      K = L                                                             
      IF (IER .NE. 0) K = IER - 1                                       
 70   IF (K .LE. 0) GO TO 90                                            
         T = DL7SVX(K, V(AR1), C, C)                                    
         IF (T .GT. ZERO) T = DL7SVN(K, V(AR1), C, C) / T               
         IF (T .GT. SINGTL) GO TO 80                                    
         K = K - 1                                                      
         GO TO 70                                                       
C                                                                       
C *** RECORD RANK IN IV(IERS)... IV(IERS) = 0 MEANS FULL RANK,          
C *** IV(IERS) .GT. 0 MEANS RANK IV(IERS) - 1.                          
C                                                                       
 80   IF (K .GE. L) GO TO 100                                           
 90      IER = K + 1                                                    
         CALL DV7SCP(L-K, C(K+1), ZERO)                                 
 100  IV(IERS) = IER                                                    
      IF (K .LE. 0) GO TO 110                                           
C                                                                       
C *** APPLY HOUSEHOLDER TRANSFORMATONS TO RESIDUALS...                  
C                                                                       
      CALL DQ7APL(LA, N, K, A, V(R1), IER)                              
C                                                                       
C *** COMPUTING C NOW MAY SAVE A FUNCTION EVALUATION AT                 
C *** THE LAST ITERATION.                                               
C                                                                       
      CALL DL7ITV(K, C, V(AR1), V(R1))                                  
      CALL DV7PRM(L, IV(IPIV1), C)                                      
C                                                                       
 110  IF(IV(1) .LT. 2) GO TO 220                                        
      GO TO 999                                                         
C                                                                       
C                                                                       
C  ***  RESIDUAL COMPUTATION FOR F.D. HESSIAN  ***                      
C                                                                       
 120  IF (L .LE. 0) GO TO 140                                           
      DO 130 I = 1, L                                                   
 130     CALL DV2AXY(N, V(R1), -C(I), A(1,I), V(R1))                    
 140  IF (IV(1) .GT. 0) GO TO 30                                        
         IV(1) = 2                                                      
         GO TO 160                                                      
C                                                                       
C  ***  NEW GRADIENT (JACOBIAN) NEEDED  ***                             
C                                                                       
 150  IV(IV1SAV) = IV1                                                  
      IF (IV(NFGCAL) .NE. IV(NFCALL)) IV(1) = 1                         
 160  CALL DV7SCP(N*P, V(DR1), ZERO)                                    
      GO TO 999                                                         
C                                                                       
C  ***  COMPUTE NEW JACOBIAN  ***                                       
C                                                                       
 170  IF (NDA .LE. 0) GO TO 240                                         
      DO 180 I = 1, NDA                                                 
         I1 = IN(1,I) - 1                                               
         IF (I1 .LT. 0) GO TO 180                                       
         J1 = IN(2,I)                                                   
         K = DR1 + I1*N                                                 
         T = NEGONE                                                     
         IF (J1 .LE. L) T = -C(J1)                                      
         CALL DV2AXY(N, V(K), T, DA(1,I), V(K))                         
 180     CONTINUE                                                       
      IF (IV1 .EQ. 2) GO TO 190                                         
         IV(1) = IV1                                                    
         GO TO 999                                                      
 190  IF (L .LE. 0) GO TO 30                                            
      IF (MD .GT. 0) GO TO 30                                           
      K = DR1                                                           
      IER = IV(IERS)                                                    
      NRAN = L                                                          
      IF (IER .GT. 0) NRAN = IER - 1                                    
      IF (NRAN .LE. 0) GO TO 210                                        
      DO 200 I = 1, P                                                   
         CALL DQ7APL(LA, N, NRAN, A, V(K), IER)                         
         K = K + N                                                      
 200     CONTINUE                                                       
 210  CALL DV7CPY(L, V(CSAVE1), C)                                      
 220  IF (IER .EQ. 0) GO TO 30                                          
C                                                                       
C     *** ADJUST SUBSCRIPTS DESCRIBING R AND DR...                      
C                                                                       
         NRAN = IER - 1                                                 
         DR1L = DR1 + NRAN                                              
         NML = N - NRAN                                                 
         R1L = R1 + NRAN                                                
         GO TO 30                                                       
C                                                                       
C  ***  CONVERGENCE OR LIMIT REACHED  ***                               
C                                                                       
 230  IF (IV(REGD) .EQ. 1) IV(REGD) = RD1                               
      IF (IV(1) .LE. 11) CALL DS7CPR(C, IV, L, LIV)                     
      GO TO 999                                                         
C                                                                       
 240  IV(1) = 66                                                        
      CALL DITSUM(V, V, IV, LIV, LV, P, V, ALF)                         
C                                                                       
 999  RETURN                                                            
C                                                                       
C  ***  LAST CARD OF DRNSGB FOLLOWS  ***                                
      END                                                               
C****END OF ROUTINES FOR SECOND PART OF PORT 3 OPTIMIZATION CHAPTER*****
