      SUBROUTINE SPMLE(N, ORDER, IA, JA, A, ISIZE, B, IB, NB)           
C                                                                       
C  THIS IS LINDA KAUFMANS SPARSE MATRIX PACKAGE                         
C  SINGLE PRECISION                                                     
C                                                                       
C  DECEMBER 9, 1982 (REVISION)                                          
C                                                                       
      INTEGER N, IB, NB                                                 
      INTEGER IA(N), JA(N), ISIZE                                       
      REAL A(N), B(IB, NB)                                              
      EXTERNAL I4YX                                                     
      LOGICAL ORDER                                                     
      DOUBLE PRECISION DSTAK(500)                                       
      COMMON /CSTAK/ DSTAK                                              
      INTEGER MCP, MAX, JLU, MRP, ISTKGT, ISTKQU                        
      INTEGER I, L, IEND, ILIN, IERR, ISUB                              
      INTEGER LAST, TEMP, IC, IHEAD, MC, II(1000)                       
      INTEGER IV, IU, LU, MR, IZ, ILEFT                                 
      INTEGER IC1                                                       
      REAL BEF, ABS, EPS, D(1000), AMAX1, R1MACH                        
      LOGICAL TEMP1                                                     
      EQUIVALENCE (D(1), II(1), DSTAK(1))                               
C THIS SUBROUTINE SOLVES AX = B WHERE A IS A SPARSE MATRIX              
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C ORDER   LOGICAL VARIABLE. IF .TRUE. REORDERING FOR                    
C         STABILITY WILL BE PERFORMED                                   
C IA      INTEGER VECTOR, LENGTH N+1, POINTING TO BEGINNINGS            
C         OF ROWS IN JA AND A VECTORS                                   
C JA      COLUMN INDICES OF NONZERO ELEMENTS OF MATRIX                  
C A       NONZERO ELEMENTS OF THE MATRIX                                
C B      RIGHT HAND SIDE MATRIX,DESTROYED ON OUTPUT                     
C IB     ROW DIMENSION OF B                                             
C NB     NUMBER OF COLUMNSOF B                                          
C OUTPUT PARAMETERS                                                     
C B      THE SOLUTION TO AX=B                                           
C ISIZE ACTUAL  UMBER OF ELEMENTS IN THE PORT STACK NEEDED TO           
C       SAVE THE DECOMPOSITION                                          
C SPACE ALLOCATED AND DEALLOCATED-3N+2 INTEGER AND N                    
C REAL LOCATIONS PLUS ISIZE INTEGER AND                                 
C REAL LOCATIONS NEED TO HOLD THE U FROM THE                            
C LU DECOMPOSITION OF A                                                 
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C 2 IB .LT. N                                                           
C 3 NB .LT. 1                                                           
C 10+K     NULL ROW           FATAL                                     
C N+K+10   INCORRECT COLUMN INDEX AT ROW K     FATAL                    
C 3N+K+10   SINGULAR MATRIX OF RANK K      RECOVERABLE                  
C 2N+K+10  RAN OUT OF SPACE WHEN PROCEESING ROW K                       
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13H SPMLE-N.LT.1, 13, 1, 2)             
C     IF (IB .LT. N) CALL SETERR(14H SPMLE-IB.LT.N, 14, 2, 2)           
C     IF (NB .LT. 1) CALL SETERR(14H SPMLE-NB.LT.1, 14, 3, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPMLE-N.LT.1', 13, 1, 2)              
      IF (IB .LT. N) CALL SETERR(' SPMLE-IB.LT.N', 14, 2, 2)            
      IF (NB .LT. 1) CALL SETERR(' SPMLE-NB.LT.1', 14, 3, 2)            
C/                                                                      
      CALL ENTER(1)                                                     
C GET SPACE FROM THE STACK FOR COLUMN ORDERING AND THE INVERSE          
C COLUMN ORDERING AND A REAL VECTOR OF LENGTH N USED IN S4MLE           
      IZ = ISTKGT(N, 3)                                                 
      IC = ISTKGT(2*N, 2)                                               
      MCP = IC+N                                                        
      IC1 = IC-1                                                        
      MC = MCP-1                                                        
      ILEFT = ISTKQU(2)-10                                              
C ISUB CONSTAINS THE NUMBER OF LOCATIONS NEEDED TO STORE THE ROW        
C PERMUTATION VECTOR, THE POINTER TO U AND AND AN INTEGER TEMPORARY     
C VECTOR                                                                
      ISUB = 3*N+3                                                      
C FIND THE NORM OF THE MATRIX                                           
      IEND = IA(N+1)-1                                                  
      BEF = 0.0                                                         
      DO  1 I = 1, IEND                                                 
         BEF = AMAX1(BEF, ABS(A(I)))                                    
   1     CONTINUE                                                       
      EPS = R1MACH(4)*BEF                                               
      IF (ORDER) GOTO 3                                                 
         ISUB = ISUB-N                                                  
C WHEN THERE IS NO PERMUTATION MATRIX NO STORAGE IS NECESSARY           
C FOR THE ROW PERMUTATION VECTOR                                        
C SET UP COLUMN PERMUTATION ARRAYS TO INDICATE NO PERMUTATION           
         DO  2 I = 1, N                                                 
            IC1 = IC1+1                                                 
            II(IC1) = I                                                 
            MC = MC+1                                                   
            II(MC) = I                                                  
   2        CONTINUE                                                    
         GOTO  5                                                        
   3     JLU = ISTKGT(ILEFT, 2)                                         
C ALLOCATE ALL THE REMAINING SPACE                                      
         MAX = (ILEFT-N)/2                                              
         IV = JLU                                                       
         L = IV+MAX                                                     
         IHEAD = L+MAX                                                  
         IERR = 2*N+11                                                  
         IF (MAX .LT. N) GOTO  12                                       
         IERR = 0                                                       
         CALL S4MDM(N, I4YX, MAX, II(IV), II(L), II(IHEAD), II(MCP),    
     1      II(IC), II(IV), IERR,IA,JA,1)                               
         IF (IERR .EQ. 0) GOTO 4                                        
            IF (IERR .GT. 2*N+10) GOTO  12                              
            IF (IERR .LE. N+10) GOTO  11                                
            TEMP1 = IERR .GT. N+10                                      
            IF (TEMP1) TEMP1 = IERR .LE. 2*N+10                         
C/6S                                                                    
C           IF (TEMP1) CALL SETERR(29H SPMLE-INCORRECT COLUMN INDEX, 29,
C    1         IERR,2)                                                  
C/7S                                                                    
            IF (TEMP1) CALL SETERR(' SPMLE-INCORRECT COLUMN INDEX', 29, 
     1         IERR,2)                                                  
C/                                                                      
   4     CALL ISTKRL(1)                                                 
C ILEFT IS THE NUMBER OF LOCATIONS LEFT IN THE STACK TO STORE U         
   5  ILEFT = ILEFT-ISUB                                                
C ALLOCATE THE SPACE BETWEEN INTEGER AND REAL LOCATIONS DEPENDENT       
C ON THE SPACE REQUIRED FOR EACH                                        
      ILIN = (ILEFT*II(7))/(II(7)+II(8))                                
      JLU = ISTKGT(ILIN, 2)                                             
      LU = ISTKGT(ILIN, 3)                                              
      MRP = 1                                                           
      IU = ISTKGT(ISUB, 2)                                              
      TEMP = IU+N+2                                                     
      IF (.NOT. ORDER) GOTO 7                                           
         MC = MCP-1                                                     
C PUT THE ROW PERMUTATION  VECTOR EQUAL TO THE COLUMN                   
C PERMUTATION VECTOR                                                    
         MR = TEMP+N                                                    
         MRP = MR+1                                                     
         DO  6 I = 1, N                                                 
            MC = MC+1                                                   
            MR = MR+1                                                   
            II(MR) = II(MC)                                             
   6        CONTINUE                                                    
   7  CALL S4MLE(N, B, IB, NB, II(JLU), D(LU), ILIN, II(IU), II(TEMP), D
     1   (IZ), II(IC), II(MRP), II(MCP), IERR, 0.1E0, LAST, IA, JA, A,  
     1   ORDER, EPS)                                                    
      IF (IERR .NE. 0) GOTO 8                                           
         ISIZE = LAST-1                                                 
         CALL S4MBS(N, II(IU), II(JLU), D(LU), II(MCP), B, IB, NB, D(IZ)
     1      )                                                           
         GOTO  13                                                       
   8     IF (IERR .LE. N+10) GOTO 11                                    
            IF (IERR .LE. 3*N+10) GOTO  12                              
C/6S                                                                    
C           CALL SETERR(22H SPMLE-SINGULAR MATRIX, 22, IERR, 1)         
C/7S                                                                    
            CALL SETERR(' SPMLE-SINGULAR MATRIX', 22, IERR, 1)          
C/                                                                      
            GOTO  13                                                    
C/6S                                                                    
C 11  CALL SETERR(15H SPMLE-NULL ROW, 15, IERR, 1)                      
C/7S                                                                    
  11  CALL SETERR(' SPMLE-NULL ROW', 15, IERR, 1)                       
C/                                                                      
      GOTO  13                                                          
C/6S                                                                    
C 12  CALL SETERR(25H SPMLE-INSUFFICIENT SPACE, 25, IERR, 1)            
C/7S                                                                    
  12  CALL SETERR(' SPMLE-INSUFFICIENT SPACE', 25, IERR, 1)             
C/                                                                      
  13  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE S4MLE(N, B, IB, NB, JA, A, IAMAX, IU, IROW, Z,         
     1   IC, R, C, IERR, THRESH, JLU, IA, JJA, AA, ORDER, EPS)          
      INTEGER N, IB, NB                                                 
      INTEGER JA(1), IAMAX, IU(N), IROW(N), IC(N), R(N)                 
      INTEGER C(N), IERR, JLU, IA(N), JJA(N)                            
      REAL B(IB, NB), A(1), Z(1), THRESH, AA(N), EPS                    
      LOGICAL ORDER                                                     
      INTEGER ICC, JUJ, NUM, I, J, K                                    
      INTEGER JAMA, M, JMIN, IMAX, JMAX, CK                             
      INTEGER II, JJ, IJ, IR, MM, RK                                    
      INTEGER JAMIN, NU, IUEND, NP1, NP2                                
      REAL ABS, AKI, PVT, PMAX, DK                                      
      LOGICAL TEMP                                                      
C THIS SUBROUTINE IS LOWER LEVEL SUBROUTINE FOR SPMLE                   
C PARAMETERS NOT DEFINED IN SPMLE ARE AS FOLLOWS                        
C JA -SPACE FOR STORING COLUMN INDICES OF U IN LU DECOMPOSITION         
C A - SPACE FOR STORING ELEMENTS OF U IN LU DECOMPOSITION               
C IAMAX - SIZE OF JA AND A                                              
C IU - POINTER INTO JA AND A GIVING BEGINNING OF EACH ROW               
C Z  TEMP VECTOR OF LENGTH N                                            
C IC- INTEGER VECTOR GIVING INVERTED COLUMN PERMUTATION                 
C R - IF ORDERING INTEGER VECTOR OF LENGTH N GIVING ROW PERMUTATIONS    
C C - COLUMN PERMUTATIONS, INTEGER VECTOR OF LENGTH N                   
C IERR - ERROR FLAG. OK IF SET OT 0 ON OUTPUT                           
C THRESH - THRESHHOLD PARAMETER FOR PIVOTING                            
C JLU - NUMBER OF ELEMENTS OF JA AND A ACTUALLY USED                    
C ORDER - LOGICAL VARIABLE. IF .TRUE. THEN PIVOTING FOR SPARSITY        
C WAS DONE AND R SHOULD BE LOOKED AT.                                   
C EPS - CRITERIA FOR SINGULARITY                                        
C IAMAX - SIZE OF SPACE AVAILABLE FOR SORING LU DECOMPOSITION           
C INITIALIZATION                                                        
      IU(1) = 1                                                         
C REARRANGE RIGHT HAND SIDES                                            
      IF (.NOT. ORDER) GOTO 4                                           
         DO  3 J = 1, NB                                                
            DO  1 I = 1, N                                              
               IR = R(I)                                                
               Z(I) = B(IR, J)                                          
   1           CONTINUE                                                 
            DO  2 I = 1, N                                              
               B(I, J) = Z(I)                                           
   2           CONTINUE                                                 
   3        CONTINUE                                                    
   4  NP1 = N+1                                                         
      IERR = 0                                                          
      NP2 = N+2                                                         
      JLU = 1                                                           
      DO  5 I = 1, N                                                    
         IROW(I) = 0                                                    
   5     CONTINUE                                                       
C DETERMINE NEXT ROW OF L AND U                                         
      DO  29 K = 1, N                                                   
         M = NP1                                                        
         IROW(NP1) = NP1                                                
         IUEND = NP2                                                    
         RK = K                                                         
         IF (ORDER) RK = R(K)                                           
         JAMIN = IA(RK)                                                 
         JAMA = IA(RK+1)-1                                              
         NUM = JAMA-JAMIN                                               
         Z(K) = 0.0                                                     
         NUM = NUM+1                                                    
C CHECK FOR NULL ROW                                                    
         IF (JAMA .LT. JAMIN) GOTO  30                                  
         DO  10 J = JAMIN, JAMA                                         
            JJ = JJA(J)                                                 
C CHECK FOR VALID COLUMN INDEX                                          
            TEMP = JJ .GT. N                                            
            IF (.NOT. TEMP) TEMP = JJ .LT. 1                            
C/6S                                                                    
C           IF (TEMP) CALL SETERR(29H SPMLE-INCORRECT COLUMN INDEX, 29  
C    1         , K+10+N, 2)                                             
C/7S                                                                    
            IF (TEMP) CALL SETERR(' SPMLE-INCORRECT COLUMN INDEX', 29   
     1         , K+10+N, 2)                                             
C/                                                                      
            ICC = IC(JJ)                                                
            Z(ICC) = AA(J)                                              
            IF (ICC .LT. K) GOTO 6                                      
               IROW(ICC) = IUEND                                        
C NEW ELEMENT IS IN U PART OF ROW                                       
               IUEND = ICC                                              
               GOTO  9                                                  
   6           IF (M .GT. ICC) M = NP1                                  
C NEW ELEMENT IS IN L PART OF THE ROW                                   
   7              II = IROW(M)                                          
                  IF (II .GE. ICC) GOTO  8                              
                  M = II                                                
                  GOTO  7                                               
   8           IROW(M) = ICC                                            
               IROW(ICC) = II                                           
   9        CONTINUE                                                    
  10        CONTINUE                                                    
C                                                                       
C ELIMINATE ROW                                                         
         I = NP1                                                        
  11        J = I                                                       
            I = IROW(I)                                                 
            IROW(J) = 0                                                 
            IF (I .EQ. NP1) GOTO  20                                    
            JMIN = IU(I)                                                
            JMAX = IU(I+1)-1                                            
            AKI = -Z(I)                                                 
C FORWARD SOLVE                                                         
            DO  12 MM = 1, NB                                           
               B(K, MM) = B(K, MM)+AKI*B(I, MM)                         
  12           CONTINUE                                                 
            IF (JMAX .LT. JMIN) GOTO 19                                 
               DO  18 J = JMIN, JMAX                                    
C ELIMINATE ITH ELEMENT                                                 
                  JUJ = JA(J)                                           
                  ICC = IC(JUJ)                                         
                  IF (IROW(ICC) .NE. 0) GOTO 17                         
                     IF (ICC .LT. K) GOTO 13                            
                        IROW(ICC) = IUEND                               
C FILL IN-SEE IF IT IS IN U OR L                                        
C FILL IN IS IN U                                                       
                        IUEND = ICC                                     
                        GOTO  16                                        
  13                    TEMP = M .GT. ICC                               
C FILL IS IN L PORTION                                                  
                        IF (.NOT. TEMP) TEMP = M .LT. I                 
                        IF (TEMP) M = I                                 
  14                       IJ = IROW(M)                                 
                           IF (IJ .GE. ICC) GOTO  15                    
                           M = IJ                                       
                           GOTO  14                                     
  15                    IROW(M) = ICC                                   
                        IROW(ICC) = IJ                                  
C SINCE THIS IS A FILL IN, INITIALIZE SPACE                             
  16                 Z(ICC) = 0.E0                                      
  17              Z(ICC) = Z(ICC)+AKI*A(J)                              
  18              CONTINUE                                              
  19        CONTINUE                                                    
            GOTO  11                                                    
  20     I = IUEND                                                      
         PMAX = 0.E0                                                    
         NU = -1                                                        
  21     IF (I .EQ. NP2) GOTO  23                                       
            PVT = ABS(Z(I))                                             
            NU = NU+1                                                   
            IF (PVT .LE. PMAX) GOTO 22                                  
               IMAX = I                                                 
               PMAX = PVT                                               
  22        I = IROW(I)                                                 
            GOTO  21                                                    
C DO THRESHHOLD PIVOTING                                                
  23     IF (ABS(Z(K)) .GE. THRESH*PMAX) IMAX = K                       
C CHECK FOR SINGULARITY                                                 
         IF (ABS(Z(IMAX)) .LE. EPS) GOTO  31                            
C CHECK IF SUFFICIENT SPACE                                             
         IF (NU+JLU .GT. IAMAX) GOTO  32                                
         NU = JLU+NU                                                    
         JLU = NU                                                       
         DK = 1.E0/Z(IMAX)                                              
C FIX UP RIGHT HAND SIDE                                                
         DO  24 MM = 1, NB                                              
            B(K, MM) = B(K, MM)*DK                                      
  24        CONTINUE                                                    
         I = IUEND                                                      
  25     IF (I .EQ. NP2) GOTO  27                                       
            IF (I .EQ. IMAX) GOTO 26                                    
               NU = NU-1                                                
               A(NU) = Z(I)*DK                                          
               JA(NU) = C(I)                                            
  26        II = I                                                      
            I = IROW(I)                                                 
C ZERO OUT SPCE SO NEXT TIME WILL NOT MISINTERPRET FILL-IN              
            IROW(II) = 0                                                
            GOTO  25                                                    
  27     IU(K+1) = JLU                                                  
C INTERCHANGE THE COLUMN INDICES                                        
         IF (K .EQ. IMAX) GOTO 28                                       
            JJ = C(K)                                                   
            C(K) = C(IMAX)                                              
            C(IMAX) = JJ                                                
            CK = C(K)                                                   
            IC(CK) = K                                                  
            IC(JJ) = IMAX                                               
  28     CONTINUE                                                       
  29     CONTINUE                                                       
      IU(N+1) = JLU                                                     
      JA(JLU) = JLU                                                     
      RETURN                                                            
  30  IERR = K+10                                                       
      RETURN                                                            
  31  IERR = 3*N+10+K                                                   
      RETURN                                                            
  32  IERR = 2*N+K+10                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE S4MBS(N, IA, JA, A, C, B, IB, NB, TMP)                 
      INTEGER N, IB, NB                                                 
      INTEGER IA(1), JA(1), C(N)                                        
      REAL A(1), B(IB, NB), TMP(N)                                      
      INTEGER IIB, IIC, JUJ, I, J, K                                    
      INTEGER JMIN, JMAX, NP1                                           
      REAL SUM                                                          
C SPARSE BACK SOLVE                                                     
      NP1 = N+1                                                         
      DO  5 K = 1, NB                                                   
         DO  3 IIB = 1, N                                               
            I = NP1-IIB                                                 
            JMIN = IA(I)                                                
            JMAX = IA(I+1)-1                                            
            SUM = B(I, K)                                               
            IF (JMIN .GT. JMAX) GOTO 2                                  
               DO  1 J = JMIN, JMAX                                     
                  JUJ = JA(J)                                           
                  SUM = SUM-A(J)*TMP(JUJ)                               
   1              CONTINUE                                              
   2        IIC = C(I)                                                  
            TMP(IIC) = SUM                                              
   3        CONTINUE                                                    
         DO  4 I = 1, N                                                 
            B(I, K) = TMP(I)                                            
   4        CONTINUE                                                    
   5     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE SPFLE(N, ORDER, AROW, ISIZE, B, IB, NB)                
      INTEGER IB, NB                                                    
      EXTERNAL AROW, I4YX                                               
      INTEGER N, ISIZE                                                  
      REAL B(IB, NB)                                                    
      LOGICAL ORDER                                                     
      DOUBLE PRECISION DSTAK(500)                                       
      COMMON /CSTAK/ DSTAK                                              
      INTEGER IIA, IMC, MCP, MAX, IEX, IMR                              
      INTEGER JLU, MRP, NUM, NUMBER, ISTKGT, ISTKQU                     
      INTEGER I, K, L, ILIN, IERR, ISUB                                 
      INTEGER TEMP, LAST, TEMP1, IA, IC, IHEAD                          
      INTEGER II(1000), JJ, LL, IT, IV, IU                              
      INTEGER LU, IZ, NU, ILEFT, IC1                                    
      REAL BEF, ABS, EPS, D(1000), AMAX1, BE                            
      REAL R1MACH                                                       
      LOGICAL MOVE                                                      
      EQUIVALENCE (D(1), II(1), DSTAK(1))                               
C THIS SUBROUTINE SOLVES AX = B WHERE A IS A SPARSE MATRIX              
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C ORDER   LOGICAL VARIABLE. IF .TRUE. REORDERING FOR                    
C         SPARSITY WILL BE PERFORMED.                                   
C AROW    SUBROUTINE OF THE FORM GETA(I,ROW,JCOL,NUM) WHICH             
C         FOR A GIVEN INPUT I RETURNS THE NONZERO ELEMENTS OF           
C         THE ITH ROW OF THE MATRIX A IN THE                            
C         REAL VECTOR ROW AND THE CORRESPONDING INDICES IN              
C         JCOL. THE VARIABLE NUM RETURNS THE NUMBER OF NONZERO          
C         ELEMENTS IN THE ITH ROW. AROW SHOULD BE DECLARED              
C         EXTERNAL IN THE CALLING PROGRAM.                              
C B      RIGHT HAND SIDE MATRIX,DESTROYED ON OUTPUT                     
C IB     ROW DIMENSION OF B                                             
C NB     NUMBER OF COLUMNSOF B                                          
C OUTPUT PARAMETERS                                                     
C B      THE SOLUTION TO AX=B                                           
C ISIZE  ACTUAL NUMBER OF ELEMENTS IN THE A ARRAY THAT WAS NEEDED       
C        FOR THE DECOMPOSITION                                          
C SPACE ALLOCATED AND DEALLOCATED-3N+2 INTEGER AND N LE                 
C REAL LOCATIONS PLUS ISIZE INTEGER AND                                 
C REAL LOCATIONS NEED TO HOLD THE U FROM THE                            
C LU DECOMPOSITION OF A                                                 
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C 2 IB .LT. N                                                           
C 3 NB .LT. 1                                                           
C 10+K     NULL ROW           FATAL                                     
C 3N+K+10   SINGULAR MATRIX OF RANK K      RECOVERABLE                  
C 2N+K+10  RAN OUT OF SPACE WHEN PROCEESING ROW K                       
C N+K+10 INCORRECT COLUMN INDEX                                         
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13H SPFLE-N.LT.1, 13, 1, 2)             
C     IF (IB .LT. N) CALL SETERR(14H SPFLE-IB.LT.N, 14, 3, 2)           
C     IF (NB .LT. 1) CALL SETERR(14H SPFLE-NB.LT.1, 14, 4, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPFLE-N.LT.1', 13, 1, 2)              
      IF (IB .LT. N) CALL SETERR(' SPFLE-IB.LT.N', 14, 3, 2)            
      IF (NB .LT. 1) CALL SETERR(' SPFLE-NB.LT.1', 14, 4, 2)            
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(N+1, 2)                                               
      IIA = IA                                                          
      II(IA) = 1                                                        
C GET SPACE FOR THE COLUMN PERMUTATION VECTOR AND ITS INVERSE           
      IC = ISTKGT(2*N, 2)                                               
      IC1 = IC-1                                                        
      ILEFT = ISTKQU(2)-10                                              
      ISUB = 2*N+2                                                      
      IF(ORDER)ISUB=3*N+2                                               
      ILIN = ((ILEFT-ISUB)*II(7))/(II(7)+II(8))                         
C GET SPACE FOR A AND LU AND DIVIDE IT BETWEEN REAL AND INTEGER         
      JLU = ISTKGT(ILIN, 2)                                             
      LU = ISTKGT(ILIN, 3)                                              
      LL = LU                                                           
      JJ = JLU                                                          
      NU = ILIN                                                         
      MCP = IC+N                                                        
      IMC = MCP-1                                                       
C READ IN THE MATRIX AND COMPUTE ITS NORM                               
      BEF = 0.0                                                         
      DO  2 I = 1, N                                                    
         CALL AROW(I, D(LL), II(JJ), NUM)                               
         BE = 0.0                                                       
         L = LL                                                         
         IERR = I+10                                                    
         IF (NUM .LT. 1) GOTO  12                                       
         DO  1 K = 1, NUM                                               
            BE = BE+ABS(D(L))                                           
            L = L+1                                                     
   1        CONTINUE                                                    
         BEF = AMAX1(BE, BEF)                                           
         IIA = IIA+1                                                    
         II(IIA) = II(IIA-1)+NUM                                        
         JJ = JJ+NUM                                                    
         LL = LL+NUM                                                    
         NU = NU-NUM                                                    
         IF (NU .LT. N) GOTO  11                                        
         TEMP1 = I+IC1                                                  
         IMC = IMC+1                                                    
         II(IMC) = I                                                    
         II(TEMP1) = I                                                  
   2     CONTINUE                                                       
      IF (.NOT. ORDER) GOTO 6                                           
         IERR=2*N+11                                                    
         IF (NU .LE. 4*II(IIA)-N) GOTO 3                                
            MOVE = .FALSE.                                              
C GENERATE COLUMN ORDERING WHEN PIVOTING FOR SPARSITY                   
C TEST IF HAVE ENOUGH SPACE INBETWEEN PRESENT COLUMN INDICES            
C AND NONZERO ELEMENTS                                                  
            GOTO  4                                                     
   3        MOVE = .TRUE.                                               
C DO NOT HAVE ENOUGH SPACE AND REPACKING IS NECESSARY                   
            CALL ISTKRL(1)                                              
            NUMBER = II(IIA)                                            
            JLU= ISTKMD(NUMBER)                                         
            IT = ISTKGT(NUMBER, 3)                                      
C MOVE REAL ELEMENTS FROM HALFWAY IN STACK UNTIL THE END                
            CALL MOVEFR(NUMBER, D(LU), D(IT))                           
            NU = ISTKQU(2)-10                                           
            JJ = ISTKGT(NU, 2)                                          
   4     MAX = (NU-N)/2                                                 
         IV = JJ                                                        
         L = IV+MAX                                                     
         IHEAD = L+MAX                                                  
         IF (MAX .LT. N) GOTO  11                                       
         IERR = 0                                                       
         CALL S4MDM(N, I4YX, MAX, II(IV), II(L), II(IHEAD),             
     1      II(MCP), II(IC), II(IV), IERR,II(IA),II(JLU),1)             
         IF (IERR .GT. 2*N+10) GOTO  11                                 
C/6S                                                                    
C        IF (IERR .GT. N+10) CALL SETERR(                               
C    1      29H SPFLE-INCORRECT COLUMN INDEX, 29, IERR, 2)              
C/7S                                                                    
         IF (IERR .GT. N+10) CALL SETERR(                               
     1      ' SPFLE-INCORRECT COLUMN INDEX', 29, IERR, 2)               
C/                                                                      
         IF (.NOT. MOVE) GOTO 5                                         
            CALL ISTKRL(2)                                              
C PUT THINGS BACK WITH NUMERICAL ELEMENTS IN THE MIDDLE OF THE          
C AVAILABLE SPACE                                                       
            JLU= ISTKMD(ILIN)                                           
            LU = ISTKGT(ILIN, 3)                                        
            CALL MOVEBR(NUMBER, D(IT), D(LU))                           
   5     CONTINUE                                                       
   6  IEX = JLU+ILIN-N                                                  
      IZ = LU+ILIN-N                                                    
      ILIN = ILIN-N                                                     
      IU = ISTKGT(ISUB, 2)                                              
      TEMP = IU+N+1                                                     
      MRP = 1                                                           
C PUT ROW ORDERING TO COLUMN ORDERING                                   
      IF(.NOT.ORDER) GO TO 77                                           
      MRP = TEMP+N+1                                                    
      IMR = MRP                                                         
      IMC = MCP                                                         
      DO  7 I = 1, N                                                    
         II(IMR) = II(IMC)                                              
         IMC = IMC+1                                                    
         IMR = IMR+1                                                    
   7     CONTINUE                                                       
 77   EPS = BEF*R1MACH(4)                                               
      CALL S4FLE(N, II(IA), II(JLU), D(LU), ILIN, II(IU), II(TEMP), D(  
     1  IZ), II(IEX), B, IB, NB, II(IC), II(MRP), II(MCP), ORDER, IERR, 
     1   0.1, LAST, EPS)                                                
      IF (IERR .NE. 0) GOTO 8                                           
         ISIZE = LAST-1                                                 
         CALL S4FBS(N, B, IB, NB, II(JLU), D(LU), II(IA), II(IU), II(   
     1      IEX), II(MCP), II(MRP), ORDER, D(IZ))                       
         GOTO  13                                                       
   8     IF (IERR .LE. N+10) GOTO  12                                   
         IF (IERR .GT. 2*N+10) GOTO 9                                   
C/6S                                                                    
C           CALL SETERR(22H SPFLE-SINGULAR MATRIX, 22, IERR, 1)         
C/7S                                                                    
            CALL SETERR(' SPFLE-SINGULAR MATRIX', 22, IERR, 1)          
C/                                                                      
            GOTO  13                                                    
   9     CONTINUE                                                       
C/6S                                                                    
C 11  CALL SETERR(25H SPFLE-INSUFFICIENT SPACE, 25, IERR, 1)            
C/7S                                                                    
  11  CALL SETERR(' SPFLE-INSUFFICIENT SPACE', 25, IERR, 1)             
C/                                                                      
      GOTO  13                                                          
C/6S                                                                    
C 12  CALL SETERR(15H SPFLE-NULL ROW, 15, IERR, 1)                      
C/7S                                                                    
  12  CALL SETERR(' SPFLE-NULL ROW', 15, IERR, 1)                       
C/                                                                      
  13  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE S4FLE(N, IA, JA, A, IAMAX, IU, IROW, Z, IEX, B,        
     1   IB, NB, IC, R, C, ORDER, IERR, THRESH, LAST, EPS)              
      INTEGER N, IB, NB                                                 
      INTEGER IA(1), JA(1), IAMAX, IU(N), IROW(N), IEX(N)               
      INTEGER IC(N), R(N), C(N), IERR, LAST                             
      REAL A(1), Z(1), B(IB, NB), THRESH, EPS                           
      LOGICAL ORDER                                                     
      INTEGER ICC, JUJ, JLU, NUU, MIN0, I                               
      INTEGER J, K, M, CMAX, JMIN, IMAX                                 
      INTEGER JMAX, CI, CK, NC, II, JJ                                  
      INTEGER IJ, RI, RK, IR, MM, JAMIN                                 
      INTEGER JAMAX, NU, ISPAC, IUEND, LASTA, NP1                       
      INTEGER JAMAX1                                                    
      REAL AKI, ABS, PVT, PMAX, DK                                      
      LOGICAL TEMP                                                      
C THIS IS LOWER LEVEL OF SFLE                                           
C PARAMETERS NOT DEFINED IN SFLE                                        
C IAMAX- SPACE LEFT IN STACK TO WORK IN                                 
C IA - POINTS TO BEGINNING OF EACH ROW IN A AND JA                      
C JA COLUMN INDICES NONZERO ELEMENTS                                    
C A NUMERICAL NONZERO ELEMENTS                                          
C IU END OF U PORTION OF THE ROW- INTEGER VECTOR OF LENGTH N            
C IROW  INTEGER SCRATCH VECTOR LENGTH N                                 
C Z REAL SCRATCH VECTOR LENGTH N                                        
C IEX - INTEGER SCRATCH VECTOR LENGTH N POINTING TO EXTRA SPACE         
C IC INTGER VECTOR LENGTH N OF INVERSE COLUMN ORDER                     
C C INTEGER VECTOR LENGTH N OF COLUMN ORDER                             
C R IF ORDERING USED AN INTEGER VECTOR OF LENGTH N GIVING ROW ORDER     
C IERR -ERROR PARAMETER                                                 
C THRESH-THRESHHOLD PARAMETER                                           
C LAST- HOW MUCH SPACE ACTUALLY USED                                    
C EPS CRITERIA FOR SINGULARITY                                          
C INITIALIZATION                                                        
C REARRANGE RIGHT HAND SIDES                                            
      IF (.NOT. ORDER) GOTO 4                                           
         DO  3 J = 1, NB                                                
            DO  1 I = 1, N                                              
               IR = R(I)                                                
               Z(I) = B(IR, J)                                          
   1           CONTINUE                                                 
            DO  2 I = 1, N                                              
               B(I, J) = Z(I)                                           
   2           CONTINUE                                                 
   3        CONTINUE                                                    
   4  LAST = IA(N+1)                                                    
      LASTA = LAST                                                      
      NP1 = N+1                                                         
      IERR = 0                                                          
      DO  5 I = 1, N                                                    
         IROW(I) = 0                                                    
   5     CONTINUE                                                       
C DETERMINE NEXT ROW OF L AND U                                         
      DO  32 K = 1, N                                                   
         NC = C(K)                                                      
         Z(NC) = 0.0E0                                                  
         IEX(K) = LAST                                                  
         RK = K                                                         
         IF (ORDER) RK = R(K)                                           
         M = NP1                                                        
         JAMIN = IA(RK)                                                 
         IROW(NP1) = NP1                                                
         IUEND = NP1                                                    
         JAMAX1 = IA(RK+1)                                              
         JAMAX = JAMAX1-1                                               
         DO  10 J = JAMIN, JAMAX                                        
            JJ = JA(J)                                                  
            Z(JJ) = A(J)                                                
            TEMP = JJ .LT. 1                                            
            IF (.NOT. TEMP) TEMP = JJ .GT. N                            
C/6S                                                                    
C           IF (TEMP) CALL SETERR(29H SPFLE-INCORRECT COLUMN INDEX, 29  
C    1         , K+10+N, 2)                                             
C/7S                                                                    
            IF (TEMP) CALL SETERR(' SPFLE-INCORRECT COLUMN INDEX', 29   
     1         , K+10+N, 2)                                             
C/                                                                      
            ICC = IC(JJ)                                                
            IF (ICC .LT. K) GOTO 6                                      
               IROW(ICC) = IUEND                                        
               IUEND = ICC                                              
               GOTO  9                                                  
   6           IF (M .GT. ICC) M = NP1                                  
C NEW ELEMENT IS IN L PART OF THE ROW                                   
   7              II = IROW(M)                                          
                  IF (II .GE. ICC) GOTO  8                              
                  M = II                                                
                  GOTO  7                                               
   8           IROW(M) = ICC                                            
               IROW(ICC) = II                                           
   9        CONTINUE                                                    
  10        CONTINUE                                                    
C                                                                       
C ELIMINATE ROW                                                         
         I = NP1                                                        
  11        J = I                                                       
            I = IROW(I)                                                 
            IROW(J) = 0                                                 
            IF (I .EQ. NP1) GOTO  23                                    
            RI = I                                                      
            IF (ORDER) RI = R(I)                                        
            JMIN = IA(RI)                                               
            JMAX = MIN0(IA(RI+1)-1, IU(I))                              
            CI = C(I)                                                   
            AKI = -Z(CI)                                                
C FORWARD SOLVE                                                         
            DO  12 MM = 1, NB                                           
               B(K, MM) = B(K, MM)+AKI*B(I, MM)                         
  12           CONTINUE                                                 
            IF (JMAX .LT. JMIN) GOTO 22                                 
  13              DO  19 J = JMIN, JMAX                                 
C ELIMINATE ITH ELEMENT                                                 
                     JUJ = JA(J)                                        
                     ICC = IC(JUJ)                                      
                     IF (IROW(ICC) .NE. 0) GOTO 18                      
                        IF (ICC .LT. K) GOTO 14                         
                           IROW(ICC) = IUEND                            
C FILL IN-SEE IF IT IS IN U OR L                                        
                           IUEND = ICC                                  
                           GOTO  17                                     
  14                       TEMP = M .GT. ICC                            
                           IF (.NOT. TEMP) TEMP = M .LT. I              
                           IF (TEMP) M = I                              
  15                          IJ = IROW(M)                              
                              IF (IJ .GE. ICC) GOTO  16                 
                              M = IJ                                    
                              GOTO  15                                  
  16                       IROW(M) = ICC                                
                           IROW(ICC) = IJ                               
  17                    Z(JUJ) = 0.E0                                   
  18                 Z(JUJ) = Z(JUJ)+AKI*A(J)                           
  19                 CONTINUE                                           
                  IF (JMAX .EQ. IU(I)) GOTO  21                         
                  JMIN = IEX(I)                                         
                  JMAX = IU(I)                                          
  20              IF (JMIN .LE. JMAX) GOTO  13                          
  21        CONTINUE                                                    
  22        CONTINUE                                                    
            GOTO  11                                                    
  23     I = IUEND                                                      
         PMAX = 0.E0                                                    
         NU = -1                                                        
  24     IF (I .EQ. NP1) GOTO  26                                       
            CI = C(I)                                                   
            PVT = ABS(Z(CI))                                            
            NU = NU+1                                                   
            IF (PVT .LE. PMAX) GOTO 25                                  
               IMAX = I                                                 
               PMAX = PVT                                               
  25        I = IROW(I)                                                 
            GOTO  24                                                    
C DO THRESHHOLD PIVOTING                                                
  26     IF (ABS(Z(NC)) .GE. THRESH*PMAX) IMAX = K                      
         CMAX = C(IMAX)                                                 
C CHECK FOR SINGULARITY                                                 
         IF (ABS(Z(CMAX)) .LE. EPS) GOTO  33                            
         ISPAC = JAMAX-JAMIN+1                                          
C SEE IF SUFFICIENT SPACE AVAILABLE                                     
         IF (NU-ISPAC+LAST .GE. IAMAX) GOTO  34                         
         NUU = JAMIN+NU-1                                               
         IF (NUU .GT. JAMAX) NUU = LAST+NU-ISPAC-1                      
         IU(K) = NUU                                                    
         JLU = NUU+1                                                    
         DK = 1.E0/Z(CMAX)                                              
C FIX UP RIGHT HAND SIDE                                                
         DO  27 MM = 1, NB                                              
            B(K, MM) = B(K, MM)*DK                                      
  27        CONTINUE                                                    
         I = IUEND                                                      
  28     IF (I .EQ. NP1) GOTO  30                                       
            IF (I .EQ. IMAX) GOTO 29                                    
               CI = C(I)                                                
C STORE ELEMENT OF U                                                    
               A(NUU) = Z(CI)*DK                                        
               JA(NUU) = CI                                             
               IF (NUU .EQ. LAST) NUU = JAMAX1                          
               NUU = NUU-1                                              
  29        II = I                                                      
            I = IROW(I)                                                 
            IROW(II) = 0                                                
            GOTO  28                                                    
  30     IF (JLU .GT. LAST) LAST = JLU                                  
C INTERCHANGE THE COLUMN INDICES                                        
         IF (K .EQ. IMAX) GOTO 31                                       
            JJ = C(K)                                                   
            C(K) = CMAX                                                 
            C(IMAX) = JJ                                                
            CK = C(K)                                                   
            IC(CK) = K                                                  
            IC(JJ) = IMAX                                               
  31     CONTINUE                                                       
  32     CONTINUE                                                       
      JA(LAST) = LAST                                                   
      RETURN                                                            
  33  IERR = 3*N+10+K                                                   
      RETURN                                                            
  34  IERR = 2*N+K+10                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE S4FBS(N, B, IB, NB, JA, A, IA, IU, IEX, C, R,          
     1   ORDER, TMP)                                                    
      INTEGER N, IB, NB                                                 
      INTEGER JA(1), IA(1), IU(N), IEX(N), C(N), R(N)                   
      REAL B(IB, NB), A(1), TMP(N)                                      
      LOGICAL ORDER                                                     
      INTEGER IIB, IIC, JUJ, MIN0, I, J                                 
      INTEGER K, JMIN, JMAX, RI, NP1                                    
      REAL SUM                                                          
      NP1 = N+1                                                         
      DO  8 K = 1, NB                                                   
C SPARSE BACK SOLVE                                                     
         DO  6 IIB = 1, N                                               
            I = NP1-IIB                                                 
            RI = I                                                      
            IF (ORDER) RI = R(I)                                        
            JMIN = IA(RI)                                               
            JMAX = MIN0(IA(RI+1)-1, IU(I))                              
            SUM = B(I, K)                                               
            IF (JMIN .GT. JMAX) GOTO 5                                  
   1              DO  2 J = JMIN, JMAX                                  
                     JUJ = JA(J)                                        
                     SUM = SUM-A(J)*TMP(JUJ)                            
   2                 CONTINUE                                           
                  IF (JMAX .EQ. IU(I)) GOTO  4                          
                  JMIN = IEX(I)                                         
                  JMAX = IU(I)                                          
   3              IF (JMIN .LE. JMAX) GOTO  1                           
   4        CONTINUE                                                    
   5        IIC = C(I)                                                  
            TMP(IIC) = SUM                                              
   6        CONTINUE                                                    
         DO  7 I = 1, N                                                 
            B(I, K) = TMP(I)                                            
   7        CONTINUE                                                    
   8     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE SPMCE(N, R, C, A, IA, JA, IAMAX, IL, ISIZE, COND,      
     1   Z)                                                             
      INTEGER N                                                         
      INTEGER R(N), C(N), IA(1), JA(1), IAMAX, IL(N)                    
      INTEGER ISIZE                                                     
      REAL A(1), COND, Z(N)                                             
      DOUBLE PRECISION D(500)                                           
      COMMON /CSTAK/ D                                                  
      INTEGER ICI, ISTKGT, I, J, JMIN, JMAX                             
      INTEGER IERR, ITMP, IC, II(1000), IC1                             
      REAL ABS, SUM, GROWTH, SUM1, RSTK(1000)                           
      LOGICAL TEMP                                                      
      EQUIVALENCE (D(1), II(1))                                         
      EQUIVALENCE (D(1), RSTK(1))                                       
C                                                                       
C SPARSE CONDITION ESTIMATOR                                            
C                                                                       
C INPUT PARAMETERS                                                      
C                                                                       
C N         NUMBER OF EQUATIONS                                         
C R         INTEGER VECTOR GIVING ROW PERMUTATIONS                      
C IA        INTEGER VECTOR OF LENGTH N+1 POINTING TO BEGINING OF        
C           EACH ROW IN THE A ARRAY                                     
C JA        INTEGER VECTOR GIVING COLUMN INDEX OF EACH ELEMENT          
C           IN THE A ARRAY                                              
C A         REAL ARRAY INTO WHICH THE NONZERO ELEMENTS                  
C           OF A ARE PACKED BY ROWS                                     
C IAMAX     TOTAL SIZE OF THE A ARRAY,MUST BE LARGE ENOUGH TO           
C           HOLD LU DECOMPOSITION OF A                                  
C OUTPUT PARAMETERS                                                     
C C         REORDERED COLUMNS                                           
C JA        COLUMN INDICES OF LU DECOMPOSITION OF A                     
C A         LU DECOMPOSITION OF A                                       
C IU        INTEGER VECTOR OF LENGTH N+1 STATING WHERE EACH ROW         
C           OF U BEGINS IN THE A AND JA ARRAYS                          
C COND      ESTIMATE OF THE CONDITION NUMBER OF A                       
C Z         REAL VECTOR LENGTH N, GIVING APPROXIMATE NULL               
C           VECTOR                                                      
C SPACE ALLOCATED 2N+1 INTEGER LOCATIONS  AND N REAL LOCATIONS          
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(12H SPCE-N.LT.1, 12, 1, 2)              
C     IF (IAMAX .LT. IA(N+1)-1) CALL SETERR(24H SPCE-INSUFFICIENT SPACE,
C    1   24, 3, 2)                                                      
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPCE-N.LT.1', 12, 1, 2)               
      IF (IAMAX .LT. IA(N+1)-1) CALL SETERR(' SPCE-INSUFFICIENT SPACE', 
     1   24, 3, 2)                                                      
C/                                                                      
      CALL ENTER(1)                                                     
      IC = ISTKGT(2*N+1, 2)                                             
      SUM1 = 0.E0                                                       
      ITMP = IC+N                                                       
      IC1 = IC-1                                                        
C FIND NORM OF MATRIX AND CHECK ROW PERMUTATIONS                        
C SET COLUMN PERMUTATION TO INITIALLY NO COLUMN INTERCHANGES.           
      DO  2 I = 1, N                                                    
         SUM = 0.E0                                                     
         TEMP = R(I) .LT. 1                                             
         IF (.NOT. TEMP) TEMP = R(I) .GT. N                             
C/6S                                                                    
C        IF (TEMP) CALL SETERR(21H SPCE-R  OUT OF RANGE, 21, 2, 2)      
C/7S                                                                    
         IF (TEMP) CALL SETERR(' SPCE-R  OUT OF RANGE', 21, 2, 2)       
C/                                                                      
         ICI = IC1+I                                                    
         C(I) = I                                                       
         II(ICI) = I                                                    
         JMIN = IA(I)                                                   
         JMAX = IA(I+1)-1                                               
         DO  1 J = JMIN, JMAX                                           
            SUM = SUM+ABS(A(J))                                         
   1        CONTINUE                                                    
         IF (SUM .GT. SUM1) SUM1 = SUM                                  
   2     CONTINUE                                                       
      CALL S4MLU(N, IA, JA, A, IAMAX, IL, II(ITMP), Z, II(IC), R, C,    
     1   IERR, 1.E0, 0.E0, ISIZE, GROWTH)                               
      IF (IERR .EQ. 0) GOTO 6                                           
         IF (IERR .GT. N+10) GOTO 3                                     
C/6S                                                                    
C           CALL SETERR(15H SPMCE-NULL ROW, 15, IERR, 2)                
C/7S                                                                    
            CALL SETERR(' SPMCE-NULL ROW', 15, IERR, 2)                 
C/                                                                      
            GOTO  7                                                     
C/6S                                                                    
C  3        IF (IERR .LE. 2*N+10) CALL SETERR(                          
C    1         29H SPMCE-INCORRECT COLUMN INDEX, 29, IERR, 2)           
C/7S                                                                    
   3        IF (IERR .LE. 2*N+10) CALL SETERR(                          
     1         ' SPMCE-INCORRECT COLUMN INDEX', 29, IERR, 2)            
C/                                                                      
            IF (IERR .LE. 3*N+10) GOTO 4                                
C/6S                                                                    
C              CALL SETERR(25H SPMCE-INSUFFICIENT SPACE, 25, IERR, 1)   
C/7S                                                                    
               CALL SETERR(' SPMCE-INSUFFICIENT SPACE', 25, IERR, 1)    
C/                                                                      
               GOTO  7                                                  
   4        CONTINUE                                                    
   6  CALL ISTKRL(1)                                                    
      ITMP = ISTKGT(N, 3)                                               
      CALL S4MCE(N, R, C, A, IA, JA, IL, SUM1, COND, Z, RSTK(ITMP))     
C/6S                                                                    
C     IF (IERR .NE. 0) CALL SETERR(22H SPMCE-SINGULAR MATRIX, 22, IERR  
C    1   , 1)                                                           
C/7S                                                                    
      IF (IERR .NE. 0) CALL SETERR(' SPMCE-SINGULAR MATRIX', 22, IERR   
     1   , 1)                                                           
C/                                                                      
   7  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE S4MCE(N, R, C, A, IA, JA, IL, ANORM, COND, Z,          
     1   TMP)                                                           
      INTEGER N                                                         
      INTEGER R(N), C(N), IA(1), JA(1), IL(1)                           
      REAL A(N), ANORM, COND, Z(N), TMP(N)                              
      INTEGER ICK, IDI, IEX, JUJ, IDI1, MIN0                            
      INTEGER I, J, K, JMIN, JMAX, JMIN1                                
      INTEGER JMAX1, IB, JJ, IR, KR, LASTA                              
      INTEGER ISIZE, NP1                                                
      REAL BIG, ABS, WKM, SUM, S, T                                     
      REAL SIGN, AMAX1, EK, SM, WK, GREAT                               
      REAL FLOAT, SASUM, YNORM, R1MACH                                  
      LOGICAL TEMP                                                      
C THIS IS LOWER LEVEL CONDITION ESTIMATOR FOR SPMCE                     
C PARAMETERS ARE THE SAME AS IN SPMCE EXCEPT THAT                       
C ANORM -NORM OF MATRIX                                                 
C TMP - N VECTOR REAL TEMPORARY                                         
C A,JA - ALREADY CONTAIN LU DECOMPOSITION COMPUTED BY S4MLU.            
      IF (N .NE. 1) GOTO 1                                              
         COND = 1.0                                                     
         Z(1) = 1.0                                                     
         GOTO  24                                                       
   1     EK = 1.0                                                       
         ISIZE = IA(N+1)-1                                              
         DO  2 J = 1, N                                                 
            Z(J) = 0.0                                                  
   2        CONTINUE                                                    
         BIG = R1MACH(2)/FLOAT(N)                                       
C                                                                       
C SOLVE TRANS(U) W =E AND PUT ANSWER IN Z                               
         LASTA = IA(N+1)                                                
         DO  14 K = 1, N                                                
            ICK = C(K)                                                  
            IF (ABS(Z(ICK)) .NE. 0.E0) EK = SIGN(EK, -Z(ICK))           
            IF (ABS(EK-Z(ICK)) .LE. 1.E0) GOTO 3                        
               S = 1.E0/ABS(EK-Z(ICK))                                  
               CALL SSCAL(N, S, Z, 1)                                   
               EK = S*EK                                                
   3        WK = EK-Z(ICK)                                              
            WKM = (-EK)-Z(ICK)                                          
            S = ABS(WK)                                                 
            SM = ABS(WKM)                                               
            KR = R(K)                                                   
C IN LU DECOMPOSITION, THE U PORTION OF THE ROW                         
C COMES BEFOR THE L  PORTION BUT IT COULD                               
C BE SPLIT IN 2.                                                        
            JMIN = IA(KR)                                               
            IDI = IL(K)                                                 
            JMAX = MIN0(IA(KR+1), IDI)-1                                
            IF (JMIN .GT. JMAX) GOTO 13                                 
               JMIN1 = JMIN                                             
               JMAX1 = JMAX                                             
   4              DO  5 J = JMIN, JMAX                                  
                     JUJ = JA(J)                                        
                     SM = SM+ABS(Z(JUJ)+WKM*A(J))                       
                     Z(JUJ) = Z(JUJ)+WK*A(J)                            
                     S = S+ABS(Z(JUJ))                                  
   5                 CONTINUE                                           
                  TEMP = IDI .LT. LASTA                                 
                  IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA                
                  IF (TEMP) GOTO  7                                     
                  JMIN = JA(IDI)                                        
                  JMAX = IDI-1                                          
   6              IF (JMIN .LE. JMAX) GOTO  4                           
   7           IF (S .GE. SM) GOTO 12                                   
                  T = WKM-WK                                            
   8                 DO  9 J = JMIN1, JMAX1                             
                        JUJ = JA(J)                                     
                        Z(JUJ) = Z(JUJ)+T*A(J)                          
   9                    CONTINUE                                        
                     TEMP = IDI .LT. LASTA                              
                     IF (.NOT. TEMP) TEMP = JMIN1 .GE. LASTA            
                     IF (TEMP) GOTO  11                                 
                     JMIN1 = JA(IDI)                                    
                     JMAX1 = IDI-1                                      
  10                 IF (JMIN1 .LE. JMAX1) GOTO  8                      
  11              CONTINUE                                              
  12           CONTINUE                                                 
  13        IF (S .LT. SM) WK = WKM                                     
            Z(ICK) = WK                                                 
  14        CONTINUE                                                    
         S = 1.0/SASUM(N, Z, 1)                                         
         CALL SSCAL(N, S, Z, 1)                                         
C                                                                       
C FORM Y=L(TRANSPOSE)*W                                                 
C AND PUT RESULT BACK INTO Z                                            
C                                                                       
         NP1 = N+1                                                      
         DO  21 IB = 1, N                                               
            I = NP1-IB                                                  
            IR = R(I)                                                   
            IDI = IL(I)                                                 
C A(IDI) HAS RECIPROCAL OF DIAGONAL                                     
            IDI1 = IL(I+1)                                              
            JMIN = IDI+1                                                
            IEX = JA(IDI1)-1                                            
            IF (JMIN .EQ. IA(IR+1)) JMIN = JA(IDI)                      
            JMAX = IA(IR+1)-1                                           
            IF (JMAX .LT. JMIN) JMAX = IEX                              
C TO AVOID OVERFLOW SCALE                                               
            SUM = AMAX1(ABS(A(IDI)), ABS(Z(I)))                         
            IF (ABS(Z(I)) .LE. 1.0E0) SUM = ABS(A(IDI)*Z(I))            
            S = 1.0E0                                                   
            IF (SUM .LE. 1.E0) GOTO 15                                  
               S = 1.0/SUM                                              
               CALL SSCAL(N, S, Z, 1)                                   
               IF (S .EQ. 0.0E0) Z(I) = 1.0E0                           
  15        IF (S .NE. 0.0E0) Z(I) = Z(I)*A(IDI)                        
            IF (JMIN .GT. JMAX) GOTO 20                                 
               SUM = Z(I)                                               
  16              DO  17 J = JMIN, JMAX                                 
                     JJ = JA(J)                                         
                     Z(JJ) = Z(JJ)+SUM*A(J)                             
  17                 CONTINUE                                           
                  IF (JMAX .EQ. IEX) GOTO  19                           
                  JMIN = JA(IDI)                                        
                  JMAX = IEX                                            
  18              IF (JMIN .LE. JMAX) GOTO  16                          
  19           CONTINUE                                                 
  20        CONTINUE                                                    
  21        CONTINUE                                                    
C                                                                       
C PUT NORM OF Z TO 1.0                                                  
C                                                                       
         S = 1.E0/SASUM(N, Z, 1)                                        
         YNORM = 1.0                                                    
         CALL SSCAL(N, S, Z, 1)                                         
C DO FORWARD AND BACK SOLVE                                             
         CALL S4MFB(N, R, C, IA, JA, A, IL, Z, TMP, YNORM)              
         S = 1.0/SASUM(N, Z, 1)                                         
         YNORM = YNORM*S                                                
         GREAT = R1MACH(2)                                              
         IF (YNORM .GE. 1.0) GOTO 23                                    
            TEMP = ANORM .EQ. 0.0E0                                     
            IF (.NOT. TEMP) TEMP = ANORM .GT. YNORM*GREAT               
            IF (.NOT. TEMP) GOTO 22                                     
               COND = GREAT                                             
               RETURN                                                   
  22     CONTINUE                                                       
  23     COND = ANORM/YNORM                                             
  24  RETURN                                                            
      END                                                               
      SUBROUTINE S4MFB(N, R, C, IA, JA, A, IL, B, Z, YNORM)             
      INTEGER N                                                         
      INTEGER R(N), C(N), IA(1), JA(1), IL(N)                           
      REAL A(1), B(N), Z(N), YNORM                                      
      INTEGER IIB, IIC, IDI, IEX, JUJ, IDI1                             
      INTEGER MIN0, I, J, JMIN, JMAX, JJ                                
      INTEGER IR, RI, LASTA, NP1                                        
      REAL ABS, SUM, S, SIGN, AMAX1, DK                                 
      REAL SC, SASUM                                                    
      LOGICAL TEMP                                                      
C SPARSE MATRIX SOLUTION                                                
C INPUT                                                                 
C N ORDER OF PROBLEM                                                    
C R ROW PERMUTATION                                                     
C C COLUMN PERMUTATION                                                  
C IA INTEGER VECTOR, LENGTH N+1 POINTING TO BEGINNING OF ROW IN JA AND A
C JA COLUMN INDICES CORRESPONDING TO NONZERO ELEMENTS IN A              
C A  REAL VECTOR OF NONZERO ELEMENTS IN LU DECOMPOSTION                 
C IL INTEGER VECTOR LENGTH N+1 POINTING TO BEGINNING OF EACH L ROW      
C    IN A AND JA. COMPUTED BY SPMLU                                     
C B RIGHT-HAND SIDE                                                     
C OUTPUT                                                                
C Z   SOLUTION TO PROBLEM                                               
      LASTA = IA(N+1)                                                   
      NP1 = N+1                                                         
C SPARSE FORWARD SOLVE                                                  
      DO  5 I = 1, N                                                    
         IR = R(I)                                                      
         IDI = IL(I)                                                    
         IDI1 = IL(I+1)                                                 
         JMIN = IDI+1                                                   
         IEX = JA(IDI1)-1                                               
         IF (JMIN .EQ. IA(IR+1)) JMIN = JA(IDI)                         
         JMAX = IA(IR+1)-1                                              
         IF (JMAX .LT. JMIN) JMAX = IEX                                 
C DK HAS RECIPROCAL OF THE DIAGONAL                                     
         DK = A(IDI)                                                    
         SUM = B(I)                                                     
   1     IF (JMIN .GT. JMAX) GOTO  3                                    
            DO  2 J = JMIN, JMAX                                        
               JJ = JA(J)                                               
               SUM = SUM+A(J)*B(JJ)                                     
   2           CONTINUE                                                 
            IF (JMAX .EQ. IEX) GOTO  3                                  
            JMIN = JA(IDI)                                              
            JMAX = IEX                                                  
            GOTO  1                                                     
C                                                                       
C SCALE THINGS TO AVOID OVERFLOW                                        
C                                                                       
   3     SC = AMAX1(ABS(SUM), ABS(DK))                                  
         IF (ABS(SUM) .LE. 1.0) SC = ABS(SUM*DK)                        
         S = 1.0E0                                                      
         IF (SC .LE. 1.0E0) GOTO 4                                      
            S = 1.0E0/SC                                                
            CALL SSCAL(N, S, B, 1)                                      
            SUM = SUM*S                                                 
            IF (S .EQ. 0.0E0) SUM = 1.0                                 
            YNORM = YNORM*S                                             
   4     IF (S .NE. 0.0E0) SUM = SUM*DK                                 
         B(I) = SUM                                                     
   5     CONTINUE                                                       
      S = 1.0/SASUM(N, B, 1)                                            
      IF (S .GT. 1.0) GOTO 6                                            
         CALL SSCAL(N, S, B, 1)                                         
         YNORM = S*YNORM                                                
C SPARSE BACK SOLVE                                                     
   6  DO  13 IIB = 1, N                                                 
         I = NP1-IIB                                                    
         RI = R(I)                                                      
         IDI = IL(I)                                                    
         JMIN = IA(RI)                                                  
         JMAX = MIN0(IA(RI+1), IDI)-1                                   
         SUM = B(I)                                                     
         IF (JMIN .GT. JMAX) GOTO 11                                    
   7           DO  8 J = JMIN, JMAX                                     
                  JUJ = JA(J)                                           
                  SUM = SUM-A(J)*Z(JUJ)                                 
   8              CONTINUE                                              
               TEMP = IDI .LT. LASTA                                    
               IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA                   
               IF (TEMP) GOTO  10                                       
               JMIN = JA(IDI)                                           
               JMAX = IDI-1                                             
   9           IF (JMIN .LE. JMAX) GOTO  7                              
  10     CONTINUE                                                       
  11     IIC = C(I)                                                     
         IF (ABS(SUM) .LE. 1.0E0) GOTO 12                               
            S = 1.0/ABS(SUM)                                            
            CALL SSCAL(N, S, Z, 1)                                      
            CALL SSCAL(I, S, B, 1)                                      
            YNORM = YNORM*S                                             
            Z(IIC) = SIGN(1.0, SUM)                                     
            SUM = SUM*S                                                 
  12     Z(IIC) = SUM                                                   
  13     CONTINUE                                                       
      DO  14 I = 1, N                                                   
         B(I) = Z(I)                                                    
  14     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE SPFCE(N, MRP, MCP, AROW, IWORK, UL, IAMAX, ISIZE,      
     1   COND, Z)                                                       
      INTEGER N, IAMAX                                                  
      EXTERNAL AROW                                                     
      INTEGER MRP(101), MCP(101), IWORK(IAMAX), ISIZE                   
      REAL UL(IAMAX), COND, Z(N)                                        
      DOUBLE PRECISION D(500)                                           
      COMMON /CSTAK/ D                                                  
      INTEGER JLU, ISTKGT, I, IERR, LAST, TEMP                          
      INTEGER TEMP1, IC, II(1000), IC1                                  
      REAL GROWTH, R(1000), ANORM                                       
      LOGICAL TEMP2                                                     
      EQUIVALENCE (R(1), II(1))                                         
      EQUIVALENCE (R(1), D(1))                                          
C THIS SUBROUTINE CALLS THE DECOMPOSITION ROUTINE AND DETERMINES        
C AN ESTIMATE OF THE CONDITION NUMBER OF A REAL SPARSE MATRIX           
C INPUT PARAMETERS                                                      
C    N        ORDER OF THE PROBLEM                                      
C    MRP      N VECTOR GIVING MATRIX ROW PERMUTATIONS                   
C    AROW      USER WRITTEN SUBROUTINE WHICH GIVES THE NONZERO ELEMENTS 
C              OF A SPECIFIED ROW AND THEIR COLUMN INDICES. THE CALLING 
C              SEQUENCE IS AROW(I,ROW,JROW,NUM)                         
C              WHERE I IS THE SPECIFIED ROW, NUM IS THE NUMBER OF       
C              NONZERO ELEMENTS IN THAT RWO, ROW IS A VECTOR OF THESE   
C              ELEMENTS AND JROW IS THE CORRESPONDING COLUMN INDICES.   
C    IAMAX     DECLARED LENGTH OF UL, IWORK SHOULD BE IAMAX+2N+1 LONG.  
C OUTPUT PARAMETERS                                                     
C    MCP      INTEGER N VECTOR CONTAINING COLUMN PERMUTATIONS FOR       
C             STABILITY                                                 
C    IWORK     INTGER VECTOR CONTAINING POINTER INFORMATION FOR LU      
C             DECOMPOSITION                                             
C    UL        NONZERO ELEMENTS OF LU DECOMPOSITION                     
C    ISIZE     ACTUAL NUMBER OF NONZERO ELEMENTS IN DECOMPOSITION       
C    COND     AN ESTIMATE OF THE CONDITION NUMBER OF THE MATRIX A       
C    Z        N VECTOR CONTAINING APPROXIMATE NULL VECTOR               
C EXTRA STORAGE ALLOCATED  -  N REAL  AND 2N+1 INTEGER LOCATIONS        
C THE SUBROUTINES SASUM, SPFLU, AND S4FCE ARE CALLED                    
C ERROR CONDITIONS-                                                     
C    1     N.LT.1       FATAL                                           
C    2    IA.LT.N       FATAL                                           
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HSPFCE-N.LT.1 , 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('SPFCE-N.LT.1 ', 13, 1, 2)              
C/                                                                      
C      COMPUTE NORM OF MATRIX STORED IN COMPACT FORM                    
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13H SPFCE-N.LT.1, 13, 1, 2)             
C     IF (IAMAX .LT. N) CALL SETERR(25H SPFCE-INSUFFICIENT SPACE, 25, 3,
C    1   2)                                                             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPFCE-N.LT.1', 13, 1, 2)              
      IF (IAMAX .LT. N) CALL SETERR(' SPFCE-INSUFFICIENT SPACE', 25, 3, 
     1   2)                                                             
C/                                                                      
      IC = ISTKGT(2*N+1, 2)                                             
      IC1 = IC-1                                                        
      DO  1 I = 1, N                                                    
         TEMP2 = MRP(I) .LT. 1                                          
         IF (.NOT. TEMP2) TEMP2 = MRP(I) .GT. N                         
C/6S                                                                    
C        IF (TEMP2) CALL SETERR(23H SPFCE-MRP OUT OF RANGE, 23, 2, 2)   
C/7S                                                                    
         IF (TEMP2) CALL SETERR(' SPFCE-MRP OUT OF RANGE', 23, 2, 2)    
C/                                                                      
         MCP(I) = I                                                     
         TEMP1 = I+IC1                                                  
         II(TEMP1) = I                                                  
   1     CONTINUE                                                       
      TEMP = IC+N                                                       
      JLU = 2*N+2                                                       
      CALL S4FLU(N, IWORK, IWORK(JLU), UL, IAMAX, IWORK(N+2), II(TEMP)  
     1   , Z, II(IC), MRP, MCP, IERR, 1.0, 0.0, LAST, AROW, GROWTH,     
     1   ANORM)                                                         
      IF (IERR .EQ. 0) GOTO 5                                           
         IF (IERR .GT. N+10) GOTO 2                                     
C/6S                                                                    
C           CALL SETERR(15H SPFCE-NULL ROW, 15, IERR, 2)                
C/7S                                                                    
            CALL SETERR(' SPFCE-NULL ROW', 15, IERR, 2)                 
C/                                                                      
            GOTO  6                                                     
C/6S                                                                    
C  2        IF (IERR .LE. 2*N+10) CALL SETERR(                          
C    1         29H SPFCE-INCORRECT COLUMN INDEX, 29, IERR, 2)           
C/7S                                                                    
   2        IF (IERR .LE. 2*N+10) CALL SETERR(                          
     1         ' SPFCE-INCORRECT COLUMN INDEX', 29, IERR, 2)            
C/                                                                      
            IF (IERR .LE. 3*N+10) GOTO 3                                
C/6S                                                                    
C              CALL SETERR(25H SPFCE-INSUFFICIENT SPACE, 25, IERR, 1)   
C/7S                                                                    
               CALL SETERR(' SPFCE-INSUFFICIENT SPACE', 25, IERR, 1)    
C/                                                                      
               GOTO  6                                                  
   3        CONTINUE                                                    
   5  ISIZE = LAST-1                                                    
      CALL ISTKRL(1)                                                    
      TEMP = ISTKGT(N, 3)                                               
      CALL S4FCE(N, IWORK, IWORK(JLU), UL, IAMAX, IWORK(N+2), R(        
     1   TEMP), ANORM, COND, Z, MCP)                                    
C/6S                                                                    
C     IF (IERR .NE. 0) CALL SETERR(22H SPFCE-SINGULAR MATRIX, 22, IERR  
C    1   , 1)                                                           
C/7S                                                                    
      IF (IERR .NE. 0) CALL SETERR(' SPFCE-SINGULAR MATRIX', 22, IERR   
     1   , 1)                                                           
C/                                                                      
   6  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE S4FCE(N, IA, JA, UL, IAMAX, IU, Z, ANORM               
     1   , COND, TMP, MCP)                                              
      INTEGER N, IAMAX                                                  
      INTEGER IA(1), JA(1), IU(N), MCP(N)                               
      REAL UL(IAMAX), Z(N), ANORM, COND, TMP(N)                         
      INTEGER IIC, ICT, IUI, JUJ, I, J                                  
      INTEGER K, JMIN, JMAX, KB, KC, ID                                 
      INTEGER JJ, KP1, NP1                                              
      REAL BIG, ABS, WKM, SUM, S, T                                     
      REAL SIGN, SQRT, AMAX1, EK, DK, SC                                
      REAL SM, WK, GREAT, FLOAT, SASUM, YNORM                           
      REAL R1MACH                                                       
      LOGICAL TEMP                                                      
C THIS SUBROUTINE DETERMINES A LOWER BOUND ON THE CONDITION NUMBER      
C OF THE DECOMPOSED MATRIX A VIA THE ALGORITHM USED IN LINPACK          
C                                                                       
C                                                                       
C SOLVE A(TRANSPOSE)W = E                                               
C WHERE E IS CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH                       
C IN THE COMPONENTS OF W                                                
      NP1 = N+1                                                         
      ICT = IA(N+1)-1                                                   
      IF (N .NE. 1) GOTO 1                                              
         COND = 1.0                                                     
         Z(1) = 1.0                                                     
         GOTO  26                                                       
   1     EK = 1.0                                                       
C                                                                       
C SOLVE U(TRANS) Y =E                                                   
C                                                                       
         BIG = SQRT(R1MACH(2))/FLOAT(N)                                 
         DO  2 J = 1, N                                                 
            Z(J) = 0.0                                                  
   2        CONTINUE                                                    
         DO  9 K = 1, N                                                 
            KC = MCP(K)                                                 
C        DIAG=UL(IU(K)-1)                                               
            IF (ABS(Z(KC)) .NE. 0.0) EK = SIGN(EK, -Z(KC))              
            IF (ABS(EK-Z(KC)) .LE. 1.0) GOTO 3                          
               S = 1.0/ABS(EK-Z(KC))                                    
               CALL SSCAL(N, S, Z, 1)                                   
               EK = S*EK                                                
   3        WK = EK-Z(KC)                                               
            WKM = (-EK)-Z(KC)                                           
            S = ABS(WK)                                                 
            SM = ABS(WKM)                                               
            KP1 = K+1                                                   
            IF (KP1 .GT. N) GOTO 8                                      
               JMIN = IU(K)                                             
               JMAX = IA(K+1)-1                                         
               IF (JMAX .LT. JMIN) GOTO 7                               
                  DO  4 J = JMIN, JMAX                                  
                     JUJ = JA(J)                                        
                     SM = SM+ABS(Z(JUJ)+WKM*UL(J))                      
                     Z(JUJ) = Z(JUJ)+UL(J)*WK                           
                     S = S+ABS(Z(JUJ))                                  
   4                 CONTINUE                                           
                  IF (S .GE. SM) GOTO 6                                 
                     T = WKM-WK                                         
                     WK = WKM                                           
                     DO  5 J = JMIN, JMAX                               
                        JUJ = JA(J)                                     
                        Z(JUJ) = Z(JUJ)+T*UL(J)                         
   5                    CONTINUE                                        
   6              CONTINUE                                              
   7           CONTINUE                                                 
   8        Z(KC) = WK                                                  
   9        CONTINUE                                                    
         S = 1.0/SASUM(N, Z, 1)                                         
         CALL SSCAL(N, S, Z, 1)                                         
C                                                                       
C SOLVE Y = L(TRANSPOSE) * W                                            
C                                                                       
         DO  13 KB = 1, N                                               
            K = N+1-KB                                                  
            ID = IU(K)-1                                                
C UL(ID) CONTAINS THE RECIPROCAL OF THE DIAGONAL OF U                   
C TRY TO AVOID OVERFLOW                                                 
            SUM = AMAX1(ABS(Z(K)), ABS(UL(ID)))                         
            IF (ABS(Z(K)) .LE. 1.0) SUM = ABS(Z(K)*UL(ID))              
            S = 1.0E0                                                   
            JMIN = IA(K)                                                
            JMAX = ID-1                                                 
            IF (SUM .LT. 1.0) GOTO 10                                   
               S = 1./SUM                                               
               CALL SSCAL(N, S, Z, 1)                                   
  10        SUM = Z(K)*UL(ID)                                           
            IF (S .EQ. 0.0E0) Z(K) = 1.0                                
            IF (JMAX .LT. JMIN) GOTO 12                                 
               DO  11 J = JMIN, JMAX                                    
                  JUJ = JA(J)                                           
                  Z(JUJ) = Z(JUJ)+UL(J)*SUM                             
  11              CONTINUE                                              
  12        Z(K) = SUM                                                  
  13        CONTINUE                                                    
         S = 1.0/SASUM(N, Z, 1)                                         
         CALL SSCAL(N, S, Z, 1)                                         
         YNORM = 1.0                                                    
C                                                                       
C SOLVE L X =W                                                          
C                                                                       
         DO  17 I = 1, N                                                
            JMIN = IA(I)                                                
            IUI = IU(I)-1                                               
            JMAX = IUI-1                                                
            DK = UL(IUI)                                                
            SUM = Z(I)                                                  
            IF (JMIN .GT. JMAX) GOTO 15                                 
               DO  14 J = JMIN, JMAX                                    
                  JJ = JA(J)                                            
                  SUM = SUM+UL(J)*Z(JJ)                                 
  14              CONTINUE                                              
  15        SC = AMAX1(ABS(SUM), ABS(DK))                               
            IF (ABS(SUM) .LE. 1.0) SC = ABS(SUM*DK)                     
            S = 1.0E0                                                   
            IF (SC .LE. 1.0) GOTO 16                                    
               S = 1./SC                                                
               CALL SSCAL(N, S, Z, 1)                                   
               YNORM = YNORM*S                                          
               SUM = SUM*S                                              
  16        SUM = SUM*DK                                                
            IF (S .EQ. 0.0E0) SUM = 1.0                                 
            Z(I) = SUM                                                  
  17        CONTINUE                                                    
         S = 1.0/SASUM(N, Z, 1)                                         
         IF (S .GT. 1.0) GOTO 18                                        
            CALL SSCAL(N, S, Z, 1)                                      
            YNORM = YNORM*S                                             
C                                                                       
C   SOLVE U * Z = X                                                     
  18     DO  22 KB = 1, N                                               
            K = N+1-KB                                                  
            JMIN = IU(K)                                                
            JMAX = IA(K+1)-1                                            
            SUM = Z(K)                                                  
            IF (JMIN .GT. JMAX) GOTO 20                                 
               DO  19 J = JMIN, JMAX                                    
                  JUJ = JA(J)                                           
                  SUM = SUM-UL(J)*TMP(JUJ)                              
  19              CONTINUE                                              
  20        IIC = MCP(K)                                                
            IF (ABS(SUM) .LE. 1.) GOTO 21                               
               S = 1.0/ABS(SUM)                                         
               CALL SSCAL(N, S, TMP, 1)                                 
               CALL SSCAL(K, S, Z, 1)                                   
               YNORM = YNORM*S                                          
               SUM = SUM*S                                              
  21        TMP(IIC) = SUM                                              
  22        CONTINUE                                                    
C    MAKE ZNORM = 1.0                                                   
         S = 1.0/SASUM(N, TMP, 1)                                       
         CALL SSCAL(N, S, TMP, 1)                                       
         DO  23 I = 1, N                                                
            Z(I) = TMP(I)                                               
  23        CONTINUE                                                    
         YNORM = YNORM*S                                                
C                                                                       
C   SET COND = ESTIMATE OF THE CONDITION NUMBER OF A                    
C                                                                       
         GREAT = R1MACH(2)                                              
         IF (YNORM .GT. 1.0) GOTO 25                                    
            TEMP = ANORM .GT. YNORM*GREAT                               
            IF (.NOT. TEMP) TEMP = ANORM .EQ. 0.0E0                     
            IF (.NOT. TEMP) GOTO 24                                     
               COND = GREAT                                             
               RETURN                                                   
  24     CONTINUE                                                       
  25     COND = ANORM/YNORM                                             
  26  RETURN                                                            
      END                                                               
      SUBROUTINE SPMLU(N, MRP, MCP, IA, JA, A, IAMAX, IL, THRESH        
     1   , EPS, ISIZE, GROWTH)                                          
      INTEGER N                                                         
      INTEGER MRP(N), MCP(N), IA(1), JA(1), IAMAX, IL(N)                
      INTEGER ISIZE                                                     
      REAL A(1), THRESH, EPS, GROWTH                                    
      DOUBLE PRECISION DSTAK(500)                                       
      COMMON /CSTAK/ DSTAK                                              
      INTEGER ISTKGT, I, K, IERR, LAST, TEMP                            
      INTEGER TEMP1, IC, II(1000), IZ, IC1                              
      REAL D(1000)                                                      
      LOGICAL TEMP2                                                     
      EQUIVALENCE (D(1), II(1), DSTAK(1))                               
C SPARSE DECOMPOSITION                                                  
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C MRP     INTEGER VECTOR GIVING ROW PERMUTATION                         
C MCP     INTEGER VECTOR GIVING COLUMN PERMUTATIONS                     
C IA     INTEGER VECTOR OF LENGTH N+1 GIVING BEGINNING                  
C        OF EACH ROW IN THE A AND JA ARRAYS                             
C JA     INTEGER VECTOR OF LENGTH IAMAX,GIVING COLUMN INDICES           
C        OF EACH ELEMENT IN THE A ARRAY                                 
C A      REAL ARRAY OF THE NONZERO ELEMENTS IN                          
C        THE COEFFICIENT MATRIX STORED BY ROWS                          
C        DESTROYED ON OUTPUT                                            
C IAMAX  DIMENSION OF THE JA AND A ARRAYS,SHOULD AT                     
C        LEAST BE THE SIZE OF THE NUMBER OF NONZERO                     
C        ELMENTS IN A,PREFERABLY,TWICE AS LARGE                         
C THRESH REAL VARIABLE BETWEEN 0 AND 1 GIVING                           
C        A THRESHHOLD FOR PIVOTING. IF THRESH IS 0, NO PIVOTING         
C        WILL BE DONE.  IF THRESH=1.E0,THEN GAUSSIAN                    
C        ELIMINATION WITH PARTIAL PIVOTING WILL BE DONE                 
C EPS    TEST FOR SINGULARITY                                           
C OUTPUT PARAMETERS                                                     
C MCP    THE REORDERING DONE TO INSURE STABILITY,AS DICTATED            
C        BY THRESH                                                      
C A       THE LU DECOMPOSITION OF A                                     
C JA      THE COLUMN INDICES OF THE LU DECOMPOSITION                    
C IL     INTEGER VECTOR OF LENGTH N+1 WHICH POINTS TO THE               
C        BEGINNING OF EACH ROW OF L IN JA ARRAY                         
C ISIZE  ACTUAL NUMBER OF ELEMENTS IN THE A ARRAY THAT WAS NEEDED       
C        FOR THE DECOMPOSITION                                          
C GROWTH NUMERICAL ELEMENT GROWTH. IF THIS MUCH .GT. 1, THEN THE        
C        COMPUTED DECOMPOSITION MAY BE THE DECOMPOSITION OF A MATRIX    
C        THAT IS NOT VERY CLOSE TO THE ORIGINAL MATRIX. RAISING         
C        THRESH MIGHT ALLEVIATE THE SITUATION.                          
C SPACE ALLOCATED AND DEALLOCATED-2N+1 INTEGER AND N DOUBLE             
C PRECISION LOCATIONS                                                   
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C 2 R  NOT IN 1 THROUGH N                                               
C 3 C NOT IN 1 THROUGH N                                                
C 4 IAMAX LESS THAN IA(N+1)-1                                           
C 10+K     NULL ROW           FATAL                                     
C 10+N+K   INVALID INDEX IN ROW K FATAL                                 
C 10+3N+K     SINGULAR MATRIX OF RANK K      RECOVERABLE                
C 10+2N+K    RAN OUT OF SPACE WHEN PROCEESING ROW K                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13H SPMLU-N.LT.1, 13, 1, 2)             
C     IF (IAMAX .LT. IA(N+1)-1) CALL SETERR(24H SPLU-INSUFFICIENT SPACE,
C    1   24, 4, 2)                                                      
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPMLU-N.LT.1', 13, 1, 2)              
      IF (IAMAX .LT. IA(N+1)-1) CALL SETERR(' SPLU-INSUFFICIENT SPACE', 
     1   24, 4, 2)                                                      
C/                                                                      
      CALL ENTER(1)                                                     
      IZ = ISTKGT(N, 3)                                                 
      IC = ISTKGT(2*N+1, 2)                                             
      IC1 = IC-1                                                        
      DO  1 I = 1, N                                                    
         TEMP2 = MRP(I) .LT. 1                                          
         IF (.NOT. TEMP2) TEMP2 = MRP(I) .GT. N                         
C/6S                                                                    
C        IF (TEMP2) CALL SETERR(23H SPMLU-MRP OUT OF RANGE, 23, 2, 2)   
C/7S                                                                    
         IF (TEMP2) CALL SETERR(' SPMLU-MRP OUT OF RANGE', 23, 2, 2)    
C/                                                                      
         K = MCP(I)                                                     
         TEMP2 = K .LT. 1                                               
         IF (.NOT. TEMP2) TEMP2 = K .GT. N                              
C/6S                                                                    
C        IF (TEMP2) CALL SETERR(23H SPMLU-MCP OUT OF RANGE, 23, 3, 2)   
C/7S                                                                    
         IF (TEMP2) CALL SETERR(' SPMLU-MCP OUT OF RANGE', 23, 3, 2)    
C/                                                                      
         TEMP1 = K+IC1                                                  
         II(TEMP1) = I                                                  
   1     CONTINUE                                                       
      TEMP = IC+N                                                       
      CALL S4MLU(N, IA, JA, A, IAMAX, IL, II(TEMP), D(IZ), II(IC), MRP  
     1   , MCP, IERR, THRESH, EPS, LAST, GROWTH)                        
      IF (IERR .EQ. 0) GOTO 6                                           
         IF (IERR .GT. N+10) GOTO 2                                     
C/6S                                                                    
C           CALL SETERR(15H SPMLU-NULL ROW, 15, IERR, 2)                
C/7S                                                                    
            CALL SETERR(' SPMLU-NULL ROW', 15, IERR, 2)                 
C/                                                                      
            GOTO  5                                                     
C/6S                                                                    
C  2        IF (IERR .LE. 2*N+10) CALL SETERR(                          
C    1         29H SPMLU-INCORRECT COLUMN INDEX, 29, IERR, 2)           
C/7S                                                                    
   2        IF (IERR .LE. 2*N+10) CALL SETERR(                          
     1         ' SPMLU-INCORRECT COLUMN INDEX', 29, IERR, 2)            
C/                                                                      
            IF (IERR .LE. 3*N+10) GOTO 3                                
C/6S                                                                    
C              CALL SETERR(25H SPMLU-INSUFFICIENT SPACE, 25, IERR, 1)   
C/7S                                                                    
               CALL SETERR(' SPMLU-INSUFFICIENT SPACE', 25, IERR, 1)    
C/                                                                      
               CALL LEAVE                                               
               GOTO  4                                                  
C/6S                                                                    
C  3           CALL SETERR(22H SPMLU-SINGULAR MATRIX, 22, IERR, 1)      
C/7S                                                                    
   3           CALL SETERR(' SPMLU-SINGULAR MATRIX', 22, IERR, 1)       
C/                                                                      
               CALL LEAVE                                               
   4        RETURN                                                      
   5  CONTINUE                                                          
   6  ISIZE = LAST-1                                                    
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE SPMSL(N, MRP, MCP, IA, JA, A, IL, B, IB, NB)           
      INTEGER N, IB, NB                                                 
      INTEGER MRP(N), MCP(N), IA(1), JA(1), IL(N)                       
      REAL A(1), B(IB, NB)                                              
      REAL D(1000)                                                      
      COMMON /CSTAK/ D                                                  
      INTEGER ISTKGT, ITMP, II(1)                                       
      EQUIVALENCE (D(1), II(1))                                         
C FORWARD SOLVE FOR SPARSE MATRICES                                     
C INPUT PARAMETERS                                                      
C N     NUMBER OF EQUATIONS                                             
C MRP   INTEGER VECTOR OF ROW PERMUTATIONS                              
C MCP   INTEGER VECTOR OF COLUMNS PERMUTATIONS                          
C IA    INTEGER VECTOR OF LENGTH N+1 GIVING BEGINNING OF                
C       EACH ROW OF DECOMPOSITION IN A NAD JA ARRAYS                    
C JA    INTEGER VECTOR COMPUTED BY  SPLU                                
C A     REAL ARRAY COMPUTED BY  SPLU                                    
C IL    INTEGER VECTOR OF LENGTH N+1 POINTING TO L IN LU                
C       DECOMPOSITION,COMPUTED BY SPLU,SPDC,OR SPCE                     
C B     MATRIX OF RIGHT HAND SIDES                                      
C IB    ROW DIMENSION OF B MATRIX                                       
C NB    NUMBER OF RIGHT HAND SIDES                                      
C OUTPUT PARAMETERS                                                     
C B     THE SOLUTION MATRIX                                             
C STORAGE SPACE ALLOCATED AND DEALLOCATED -N REAL LOCATIONS             
C ERROR CONDITIONS                                                      
C 1 N.LT.1    FATAL                                                     
C 2 IB.LT.N   FATAL                                                     
C 3 NB.LT.1   FATAL                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13H SPMSL-N.LT.1, 13, 1, 2)             
C     IF (IB .LT. N) CALL SETERR(14H SPMSL-IB.LT.N, 14, 2, 2)           
C     IF (NB .LT. 1) CALL SETERR(14H SPMSL-NB.LT.1, 14, 3, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPMSL-N.LT.1', 13, 1, 2)              
      IF (IB .LT. N) CALL SETERR(' SPMSL-IB.LT.N', 14, 2, 2)            
      IF (NB .LT. 1) CALL SETERR(' SPMSL-NB.LT.1', 14, 3, 2)            
C/                                                                      
      CALL ENTER(1)                                                     
      ITMP = ISTKGT(N, 3)                                               
      CALL S4MSL(N, MRP, MCP, IA, JA, A, IL, B, IB, NB, D(ITMP))        
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE S4MLU(N, IA, JA, A, IAMAX, IL, IROW, Z, IC, R, C,      
     1   IERR, THRESH, EPS, LAST, GROWTH)                               
      INTEGER N                                                         
      INTEGER IA(N), JA(N), IAMAX, IL(N), IROW(N), IC(N)                
      INTEGER R(N), C(N), IERR, LAST                                    
      REAL A(N), Z(N), THRESH, EPS, GROWTH                              
      INTEGER ICC, IDI, JUJ, JLU, NUR, NUU                              
      INTEGER MIN0, I, J, K, M, CMAX                                    
      INTEGER JMIN, IMAX, JMAX, CI, CK, NC                              
      INTEGER II, JJ, IJ, RI, RK, JAMIN                                 
      INTEGER JAMAX, NU, ISPAC, LASTA, IUEND, IT1                       
      INTEGER NP1, JAMAX1                                               
      REAL AKI, ABS, AFT, PVT, BFOR, PMAX                               
      REAL AMAX1, DK                                                    
      LOGICAL TEMP                                                      
C INPUT PARAMETERS                                                      
C N ORDER OF MATRIX                                                     
C IA POINTER TO JA AND A OF BEGINNING OF EACH NEW ROW                   
C    LENGTH N+1                                                         
C JA COLUMN INDICES OF NONZERONELEMENTS OF A                            
C A NONZERO ELEMENTS OF A                                               
C IAMAX - DIMENSION OF A AND JA ARRAYS                                  
C R- INTEGER VECTOR LENGTH N OF ROW PERMUTAIONS                         
C C - INTEGER VECTOR LENGTH N OF COLUMN PERMUTATIONS                    
C IC - INTEGER VECTOR OF LENGTH N OF INCERSE COLUMN PERMUTAIONS         
C     IC(C(I))=I                                                        
C THRESH - THRESHHOLD PIVOTING PARAMETER BETWEEN 0.0 AND 1.0.           
C          IF 1.0, PARTIAL PIVOTING WILL BE PEERFORMED                  
C EPS - SINGULARITY CRITERIA.                                           
C SCRATCH VECTORS                                                       
C Z ,REAL, LENGTH N                                                     
C IROW, INTEGER LENGTH N+1                                              
C OUTPUT                                                                
C JA,A NONZERO ELEMENTS AND CORRESPONDING COLUMN INDICES OF LU          
C       DECOMPOSITION                                                   
C C,IC - COLUMN PERMUTAIONS AFTER THRESHHOLD PIVOTING                   
C IERR - ERROR CRITERIA- IF NONZERO WORRY                               
C IF .LE.N+10 NULL ROW AT IERR-10                                       
C IF N+11.LE.IERR.LE.2N+10 INVALID INDEX AT ROW IERR-N-10               
C IF 2N+11.LE.IERR.LE.3N+10 .LT.  SINGULAR MATRIX OF RANK IERR-2N-10    
C IF 3N+11.LE.IERR.LE. RAN OUT OF STORAGE AT ROW IERR-3N-10             
C LAST- NUMBER OF ELEMENTS IN A AND JA USED                             
C GROWTH- ELEMENT GROWTH                                                
C IL- INTEGER VECTOR LENGTH N+1, POINTING TO BEGINNINGS OF EACH ROW OF  
C     L IN LU DECOMPOSITION                                             
C INITIALIZATION                                                        
      LAST = IA(N+1)                                                    
      BFOR = 0.0E0                                                      
      IT1 = 0                                                           
      AFT = 0.0E0                                                       
      LASTA = LAST                                                      
      NP1 = N+1                                                         
      IERR = 0                                                          
      DO  1 I = 1, N                                                    
         IROW(I) = 0                                                    
   1     CONTINUE                                                       
C DETERMINE NEXT ROW OF L AND U                                         
      DO  28 K = 1, N                                                   
         RK = R(K)                                                      
         M = NP1                                                        
         JAMIN = IA(RK)                                                 
         IROW(NP1) = NP1                                                
         IUEND = NP1                                                    
         JAMAX1 = IA(RK+1)                                              
         NC = C(K)                                                      
         Z(NC) = 0.0                                                    
         JAMAX = JAMAX1-1                                               
C CHECK FOR NULL ROW                                                    
         IF (JAMIN .GT. JAMAX) GOTO  29                                 
         DO  6 J = JAMIN, JAMAX                                         
            JJ = JA(J)                                                  
C CHECK FOR VALID COLUMN INDEX                                          
            TEMP = JJ .LE. 0                                            
            IF (.NOT. TEMP) TEMP = JJ .GT. N                            
            IF (TEMP) GOTO  32                                          
            Z(JJ) = A(J)                                                
            BFOR = AMAX1(BFOR, ABS(Z(JJ)))                              
            ICC = IC(JJ)                                                
C NEW ELEMENT IS IN U PART SO JUST ADD TO LINKED LIST                   
            IF (ICC .LT. K) GOTO 2                                      
               IROW(ICC) = IUEND                                        
               IUEND = ICC                                              
               GOTO  5                                                  
   2           IF (M .GT. ICC) M = NP1                                  
C NEW ELEMENT IS IN L PART OF THE ROW                                   
   3              II = IROW(M)                                          
                  IF (II .GE. ICC) GOTO  4                              
                  M = II                                                
                  GOTO  3                                               
   4           IROW(M) = ICC                                            
               IROW(ICC) = II                                           
   5        CONTINUE                                                    
   6        CONTINUE                                                    
C                                                                       
C ELIMINATE ROW                                                         
         NUR = 0                                                        
         I = NP1                                                        
   7        I = IROW(I)                                                 
            IF (I .EQ. NP1) GOTO  18                                    
C DETERMINE WHICH ROW IN PERMUTATION                                    
            RI = R(I)                                                   
            JMIN = IA(RI)                                               
            IDI = IL(I)                                                 
C RELEVANT ROW MAY BE IN TWO SECTIONS, THUS EITHER                      
C ELIMINATE UNTIL U IS FINISHED OR SECTION IS FINISHED                  
            JMAX = MIN0(IA(RI+1), IDI)-1                                
            CI = C(I)                                                   
            AKI = -Z(CI)                                                
            AFT = AMAX1(AFT, ABS(AKI))                                  
            NUR = NUR+1                                                 
            IF (JMAX .LT. JMIN) GOTO 17                                 
   8              DO  14 J = JMIN, JMAX                                 
C ELIMINATE ITH ELEMENT                                                 
                     JUJ = JA(J)                                        
                     ICC = IC(JUJ)                                      
                     IF (IROW(ICC) .NE. 0) GOTO 13                      
                        IF (ICC .LT. K) GOTO 9                          
                           IROW(ICC) = IUEND                            
C FILL IN-SEE IF IT IS IN U OR L                                        
                           IUEND = ICC                                  
                           GOTO  12                                     
   9                       TEMP = M .GT. ICC                            
                           IF (.NOT. TEMP) TEMP = M .LT. I              
                           IF (TEMP) M = I                              
C FILL-IN IS IN L SO FIND ITS ORDERING PLACE                            
  10                          IJ = IROW(M)                              
                              IF (IJ .GE. ICC) GOTO  11                 
                              M = IJ                                    
                              GOTO  10                                  
  11                       IROW(M) = ICC                                
                           IROW(ICC) = IJ                               
  12                    Z(JUJ) = 0.E0                                   
  13                 Z(JUJ) = Z(JUJ)+AKI*A(J)                           
  14                 CONTINUE                                           
C TEST IF THERE IS A SECOND SEGMENT                                     
                  TEMP = IDI .LT. LASTA                                 
                  IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA                
                  IF (TEMP) GOTO  16                                    
                  JMIN = JA(IDI)                                        
                  JMAX = IDI-1                                          
  15              IF (JMIN .LE. JMAX) GOTO  8                           
  16        CONTINUE                                                    
  17        CONTINUE                                                    
            GOTO  7                                                     
  18     I = IUEND                                                      
         PMAX = 0.E0                                                    
         NU = 0                                                         
  19     IF (I .EQ. NP1) GOTO  21                                       
            CI = C(I)                                                   
            PVT = ABS(Z(CI))                                            
            NU = NU+1                                                   
            IF (PVT .LE. PMAX) GOTO 20                                  
               IMAX = I                                                 
               PMAX = PVT                                               
  20        I = IROW(I)                                                 
            GOTO  19                                                    
C DO THRESHHOLD PIVOTING                                                
  21     IF (ABS(Z(NC)) .GE. THRESH*PMAX) IMAX = K                      
         CMAX = C(IMAX)                                                 
C TEST FOR SINGULARITY                                                  
         IF (ABS(Z(CMAX)) .GT. EPS) GOTO  30                            
            IF (IERR.EQ.0)IERR=3*N+K+10                                 
            DK=R1MACH(2)                                                
            GO TO 330                                                   
 30      CONTINUE                                                       
         DK=1.0E0/Z(CMAX)                                               
 330      CONTINUE                                                      
         NUR = NUR+NU                                                   
         ISPAC = JAMAX-JAMIN+1                                          
C TEST IS THERE IS ENOUGH SPACE                                         
         IF (NUR-ISPAC+LAST .GE. IAMAX) GOTO  31                        
         NUU = JAMIN+NU-1                                               
C DETERMINE IS FILLIN REQUIRES SECOND SEGMENT FOR U                     
         IF (NUU .GT. JAMAX) NUU = LAST+NU-ISPAC-1                      
         IL(K) = NUU                                                    
         JLU = NUU+1                                                    
         IF (NUU .EQ. JAMAX) JLU = LAST                                 
         JA(NUU) = LAST                                                 
         AFT = AMAX1(AFT, PMAX)                                         
         I = IUEND                                                      
C STORE DIAGONAL ELEMENT                                                
         A(NUU) = DK                                                    
  22     IF (I .EQ. NP1) GOTO  24                                       
            IF (I .EQ. IMAX) GOTO 23                                    
               CI = C(I)                                                
C STORE ELEMENT OF U                                                    
               IF (NUU .EQ. LAST) NUU = JAMAX1                          
               NUU = NUU-1                                              
               A(NUU) = Z(CI)*DK                                        
               JA(NUU) = CI                                             
  23        II = I                                                      
            I = IROW(I)                                                 
            IROW(II) = 0                                                
            GOTO  22                                                    
C STORE ELEMENTS OF L                                                   
  24     I = NP1                                                        
  25        J = I                                                       
            I = IROW(I)                                                 
            IROW(J) = 0                                                 
            IF (I .EQ. NP1) GOTO  26                                    
            JA(JLU) = I                                                 
            CI = C(I)                                                   
            A(JLU) = -Z(CI)                                             
            JLU = JLU+1                                                 
            IF (JLU .EQ. JAMAX1) JLU = LAST                             
            GOTO  25                                                    
  26     IF (JLU .GT. LAST) LAST = JLU                                  
C INTERCHANGE THE COLUMN INDICES                                        
         IF (K .EQ. IMAX) GOTO 27                                       
            JJ = C(K)                                                   
            C(K) = CMAX                                                 
            C(IMAX) = JJ                                                
            CK = C(K)                                                   
            IC(CK) = K                                                  
            IC(JJ) = IMAX                                               
  27     CONTINUE                                                       
  28     CONTINUE                                                       
      GROWTH =1.0E0                                                     
      IF (BFOR.NE.0.0E0) GROWTH = AMAX1(1.0E0, AFT/BFOR)                
      IL(N+1) = LAST                                                    
      JA(LAST) = LAST                                                   
      RETURN                                                            
  29  IERR = K+10                                                       
      RETURN                                                            
  31  IERR = 2*N+K+10                                                   
      RETURN                                                            
  32  IERR = N+K+10                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE S4MSL(N, R, C, IA, JA, A, IL, B, IB, NB, TMP)          
      INTEGER N, IB, NB                                                 
      INTEGER R(N), C(N), IA(1), JA(1), IL(N)                           
      REAL A(1), B(IB, NB), TMP(N)                                      
      INTEGER IIB, IIC, IDI, IEX, JUJ, IDI1                             
      INTEGER MIN0, I, J, K, JMIN, JMAX                                 
      INTEGER JJ, IR, RI, LASTA, NP1                                    
      REAL SUM, DK                                                      
      LOGICAL TEMP                                                      
C SPARSE MATRIX SOLUTION                                                
C INPUT                                                                 
C N ORDER OF PROBLEM                                                    
C R ROW PERMUTATION                                                     
C C COLUMN PERMUTATION                                                  
C IA INTEGER VECTOR, LENGTH N+1 POINTING TO BEGINNING OF ROW IN JA AND A
C JA COLUMN INDICES CORRESPONDING TO NONZERO ELEMENTS IN A              
C A  REAL VECTOR OF NONZERO ELEMENTS IN LU DECOMPOSTION                 
C IL INTEGER VECTOR LENGTH N+1 POINTING TO BEGINNING OF EACH L ROW      
C    IN A AND JA. COMPUTED BY SPMLU                                     
C B RIGHT-HAND SIDE                                                     
C IB ROW DIMENSION OF B                                                 
C NB NUMBER OF RIGHT HAND SIDES                                         
C SCRATCH VECTOR -TMP REAL VECTOR LENGTH N                              
C OUTPUT                                                                
C B   SOLUTION TO PROBLEM                                               
C THIS SUBROUTINE ASSUME EACH ROW CAN BE AT MOST TWO SEGMENTS           
C IL(I) POINTS TO DIAGONAL REALLY AND JA(IL(I)) INDICATES               
C WHERE EXTRA SPACE IS NEEDED FOR THAT ROW. EACH ROW                    
C LOOKS LIKE U,DIAGONAL,L                                               
      LASTA = IA(N+1)                                                   
      NP1 = N+1                                                         
      DO  11 K = 1, NB                                                  
C SPARSE FORWARD SOLVE                                                  
         DO  4 I = 1, N                                                 
            IR = R(I)                                                   
            IDI = IL(I)                                                 
            IDI1 = IL(I+1)                                              
            JMIN = IDI+1                                                
C DETERMINE WHERE FIRST PART OF L IS - IN FIRST OR SECOND               
C SEGMENT                                                               
            IEX = JA(IDI1)-1                                            
            IF (JMIN .EQ. IA(IR+1)) JMIN = JA(IDI)                      
            JMAX = IA(IR+1)-1                                           
            IF (JMAX .LT. JMIN) JMAX = IEX                              
            DK = A(IDI)                                                 
            SUM = B(IR, K)                                              
   1        IF (JMIN .GT. JMAX) GOTO  3                                 
C IS THERE ANY PART OF L IN EXTRA SEGMENT                               
               DO  2 J = JMIN, JMAX                                     
                  JJ = JA(J)                                            
                  SUM = SUM+A(J)*TMP(JJ)                                
   2              CONTINUE                                              
               IF (JMAX .EQ. IEX) GOTO  3                               
               JMIN = JA(IDI)                                           
               JMAX = IEX                                               
               GOTO  1                                                  
   3        TMP(I) = SUM*DK                                             
   4        CONTINUE                                                    
C SPARSE BACK SOLVE                                                     
         DO  10 IIB = 1, N                                              
            I = NP1-IIB                                                 
            RI = R(I)                                                   
            IDI = IL(I)                                                 
            JMIN = IA(RI)                                               
C GO UNTIL EITHER END OF U IS FOUND OR REACH END                        
C OF FIRST SEGMENT                                                      
            JMAX = MIN0(IA(RI+1), IDI)-1                                
            SUM = TMP(I)                                                
            IF (JMIN .GT. JMAX) GOTO 9                                  
   5              DO  6 J = JMIN, JMAX                                  
                     JUJ = JA(J)                                        
                     SUM = SUM-A(J)*B(JUJ, K)                           
   6                 CONTINUE                                           
C CHECK IF THERE IS ANY OF U IN SECOND SEGMENT                          
                  TEMP = IDI .LT. LASTA                                 
                  IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA                
                  IF (TEMP) GOTO  8                                     
                  JMIN = JA(IDI)                                        
                  JMAX = IDI-1                                          
   7              IF (JMIN .LE. JMAX) GOTO  5                           
   8        CONTINUE                                                    
   9        IIC = C(I)                                                  
            B(IIC, K) = SUM                                             
  10        CONTINUE                                                    
  11     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE SPFLU(N, MRP, MCP, AROW, IWORK, UL, IAMAX,             
     1   THRESH, EPS, ISIZE, GROWTH)                                    
      INTEGER N, IAMAX                                                  
      EXTERNAL AROW                                                     
      INTEGER MRP(N), MCP(N), IWORK(IAMAX), ISIZE                       
      REAL UL(N), THRESH, EPS, GROWTH                                   
      DOUBLE PRECISION DSTAK(500)                                       
      COMMON /CSTAK/ DSTAK                                              
      INTEGER JLU, ISTKGT, I, K, IERR, LAST                             
      INTEGER TEMP, TEMP1, IC, II(1), IZ, IC1                           
      REAL D(1000), ANORM                                               
      LOGICAL TEMP2                                                     
      EQUIVALENCE (D(1), II(1), DSTAK(1))                               
C SPARSE DECOMPOSITION                                                  
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C MRP     INTEGER VECTOR GIVING ROW PERMUTATION                         
C MCP     INTEGER VECTOR GIVING COLUMN PERMUTATIONS                     
C AROW    SUBROUTINE OF THE FORM AROW(I,ROW,JCOL,NUM) WHICH             
C         FOR A GIVEN INPUT I RETURNS THE NONZERO ELEMENTS OF           
C         THE ITH ROW OF THE MATRIX A IN THE                            
C         REAL VECTOR ROW AND THE CORRESPONDING INDICES IN              
C         JCOL. THE VARIABLE NUM RETURNS THE NUMBER OF NONZERO          
C         ELEMENTS IN THE ITH ROW. AROW SHOULD BE DECLARED              
C         EXTERNAL IN THE CALLING PROGRAM.                              
C IAMAX  DIMENSION OF THE UL ARRAY,SHOULD AT                            
C        LEAST BE THE SIZE OF  THE NUMBER OF NONZERO                    
C        ELMENTS IN A,PREFERABLY,TWICE AS LARGE                         
C THRESH REAL VARIABLE BETWEEN 0 AND 1 GIVING                           
C        A THRESHHOLD FOR PIVOTING. IF THRESH IS 0, NO PIVOTING         
C        WILL BE DONE.  IF THRESH=1.E0,THEN GAUSSIAN                    
C        ELIMINATION WITH PARTIAL PIVOTING WILL BE DONE                 
C EPS    TEST FOR SINGULARITY                                           
C OUTPUT PARAMETERS                                                     
C C      THE REORDERING DONE TO INSURE STABILITY,AS DICTATED            
C        BY THRESH                                                      
C IWORK   INTEGER VECTOR ,LENGTH 2N+1+IAMAX WHOSE FIRST N+1 COMOPONENTS 
C         POINT TO THE BEGINNING OF TH ROWS OF L IN THE UL              
C         AND WHOSE NEXT N COMPONENTS POINT TO THE BEGINNING            
C         OF THE ROWS OF U IN THE UL. COMPONENTS 2N+2 UNTIL             
C         2N+1+ISIZE GIVE THE COLUMN INDICES OF THE COMPONENTS          
C         OF UL                                                         
C UL      THE LU DECOMPOSITION OF THE SPARSE MATRIX A                   
C ISIZE  ACTUAL NUMBER OF ELEMENTS IN THE A ARRAY THAT WAS NEEDED       
C        FOR THE DECOMPOSITION                                          
C GROWTH NUMERICAL ELEMENT GROWTH. IF GROWTH MUCH .GT. 1,WORRY          
C SPACE ALLOCATED AND DEALLOCATED-2N+1 INTEGER AND N DOUBLE             
C PRECISION LOCATIONS                                                   
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C 2 R AND C NOT IN 1 THROUGH N                                          
C 3 IAMAX LESS THAN N                                                   
C 10+K     NULL ROW           FATAL                                     
C 3N+K+10     SINGULAR MATRIX OF RANK K      RECOVERABLE                
C 2N+K+10    RAN OUT OF SPACE WHEN PROCEESING ROW K                     
C N+K+10   INVALID COLUMN INDEX WHEN PROCESSING ROW K                   
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13H SPFLU-N.LT.1, 13, 1, 2)             
C     IF (IAMAX .LT. N) CALL SETERR(25H SPFLU-INSUFFICIENT SPACE, 25, 3,
C    1   2)                                                             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPFLU-N.LT.1', 13, 1, 2)              
      IF (IAMAX .LT. N) CALL SETERR(' SPFLU-INSUFFICIENT SPACE', 25, 3, 
     1   2)                                                             
C/                                                                      
      CALL ENTER(1)                                                     
      IZ = ISTKGT(N, 3)                                                 
      IC = ISTKGT(2*N+1, 2)                                             
      IC1 = IC-1                                                        
      DO  1 I = 1, N                                                    
         TEMP2 = MRP(I) .LT. 1                                          
         IF (.NOT. TEMP2) TEMP2 = MRP(I) .GT. N                         
C/6S                                                                    
C        IF (TEMP2) CALL SETERR(23H SPFLU-MRP OUT OF RANGE, 23, 2, 2)   
C/7S                                                                    
         IF (TEMP2) CALL SETERR(' SPFLU-MRP OUT OF RANGE', 23, 2, 2)    
C/                                                                      
         K = MCP(I)                                                     
         TEMP2 = K .LT. 1                                               
         IF (.NOT. TEMP2) TEMP2 = K .GT. N                              
C/6S                                                                    
C        IF (TEMP2) CALL SETERR(23H SPFLU-MCP OUT OF RANGE, 23, 2, 2)   
C/7S                                                                    
         IF (TEMP2) CALL SETERR(' SPFLU-MCP OUT OF RANGE', 23, 2, 2)    
C/                                                                      
         TEMP1 = K+IC1                                                  
         II(TEMP1) = I                                                  
   1     CONTINUE                                                       
      TEMP = IC+N                                                       
      JLU = 2*N+2                                                       
      CALL S4FLU(N, IWORK, IWORK(JLU), UL, IAMAX, IWORK(N+2), II(TEMP)  
     1   , D(IZ), II(IC), MRP, MCP, IERR, THRESH, EPS, LAST, AROW,      
     1   GROWTH, ANORM)                                                 
      IF (IERR .EQ. 0) GOTO 8                                           
         IF (IERR .GT. N+10) GOTO 2                                     
C/6S                                                                    
C           CALL SETERR(15H SPFLU-NULL ROW, 15, IERR, 2)                
C/7S                                                                    
            CALL SETERR(' SPFLU-NULL ROW', 15, IERR, 2)                 
C/                                                                      
            GOTO  7                                                     
   2        IF (IERR .LE. 3*N+10) GOTO 3                                
C/6S                                                                    
C              CALL SETERR(22H SPFLU-SINGULAR MATRIX, 22, IERR, 1)      
C/7S                                                                    
               CALL SETERR(' SPFLU-SINGULAR MATRIX', 22, IERR, 1)       
C/                                                                      
               GOTO  6                                                  
   3           IF (IERR .GT. 2*N+10) GOTO 4                             
C/6S                                                                    
C                 CALL SETERR(29H SPFLU-INCORRECT COLUMN INDEX, 29,     
C    1               IERR, 2)                                           
C/7S                                                                    
                  CALL SETERR(' SPFLU-INCORRECT COLUMN INDEX', 29,      
     1               IERR, 2)                                           
C/                                                                      
                  GOTO  5                                               
C/6S                                                                    
C  4              CALL SETERR(25H SPFLU-INSUFFICIENT SPACE, 25, IERR+N  
C    1               , 1)                                               
C/7S                                                                    
   4              CALL SETERR(' SPFLU-INSUFFICIENT SPACE', 25, IERR+N   
     1               , 1)                                               
C/                                                                      
   5        CONTINUE                                                    
   6     CONTINUE                                                       
   7  CONTINUE                                                          
   8  ISIZE = LAST-1                                                    
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE SPFSL(N, MRP, MCP, IWORK, UL, B, IB, NB)               
      INTEGER N, IB, NB                                                 
      INTEGER MRP(N), MCP(N), IWORK(1)                                  
      REAL UL(1), B(IB, NB)                                             
      REAL D(1000)                                                      
      COMMON /CSTAK/ D                                                  
      INTEGER JLU, ISTKGT, ITMP                                         
C FORWARD SOLVE FOR SPARSE MATRICES                                     
C INPUT PARAMETERS                                                      
C N     NUMBER OF EQUATIONS                                             
C MRP   INTEGER VECTOR OF ROW PERMUTATIONS                              
C MCP   INTEGER VECTOR,LENGTHN OR COLUMN PERMUTATIONS                   
C IWORK INTEGER VECTOR COMPUTED BY  SPFLU                               
C UL    REAL ARRAY COMPUTED BY  SPFLU                                   
C IU    INTEGER VECTOR OF LENGTH N POINTING TO U IN LU                  
C       DECOMPOSITION,COMPUTED BY SPFLU OR SPFCE                        
C B     MATRIX OF RIGHT HAND SIDES                                      
C IB    ROW DIMENSION OF B MATRIX                                       
C NB    NUMBER OF RIGHT HAND SIDES                                      
C OUTPUT PARAMETERS                                                     
C B     THE SOLUTION MATRIX                                             
C STORAGE SPACE ALLOCATED AND DEALLOCATED -N REAL LOCATIONS             
C ERROR CONDITIONS                                                      
C 1 N.LT.1    FATAL                                                     
C 2 IB.LT.N   FATAL                                                     
C 3 NB.LT.1   FATAL                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13H SPFSL-N.LT.1, 13, 1, 2)             
C     IF (IB .LT. N) CALL SETERR(14H SPFSL-IB.LT.N, 14, 2, 2)           
C     IF (NB .LT. 1) CALL SETERR(14H SPFSL-NB.LT.1, 14, 3, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPFSL-N.LT.1', 13, 1, 2)              
      IF (IB .LT. N) CALL SETERR(' SPFSL-IB.LT.N', 14, 2, 2)            
      IF (NB .LT. 1) CALL SETERR(' SPFSL-NB.LT.1', 14, 3, 2)            
C/                                                                      
      CALL ENTER(1)                                                     
      ITMP = ISTKGT(N, 3)                                               
      JLU = 2*N+2                                                       
      CALL S4FSL(N, MRP, MCP, IWORK, IWORK(JLU), UL, IWORK(N+2), B, IB  
     1   , NB, D(ITMP))                                                 
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE S4FLU(N, IA, JA, A, IAMAX, IU, IROW, Z, IC, R, C,      
     1   IERR, THRESH, EPS, JLU, GETA, GROWTH, BEFOR)                   
      INTEGER N                                                         
      EXTERNAL GETA                                                     
      INTEGER IA(1), JA(1), IAMAX, IU(N), IROW(N), IC(N)                
      INTEGER R(N), C(N), IERR, JLU                                     
      REAL A(1), Z(1), THRESH, EPS, GROWTH, BEFOR                       
      INTEGER ICC, JUJ, NUM, I, J, K                                    
      INTEGER M, JMIN, IMAX, JMAX, CK, II                               
      INTEGER JJ, IJ, RI, RK, NU, JAMAX                                 
      INTEGER IUEND, NP1, NP2                                           
      REAL AKI, ABS, AFT, PVT, BEFO, PMAX                               
      REAL AMAX1, DK                                                    
      LOGICAL TEMP, SING                                                
C THIS IS LOWER LEVEL LU DECOMPOSITION WITH THRESHHOLD PIVOTING         
C WITH FUNCTION INPUT                                                   
C INPUT PARAMETERS                                                      
C N INTEGER NUMBER OF ROWS                                              
C IAMAX DECLARED SIZE OF THE A ARRAY.                                   
C IC INTEGER N-VECTOR GIVING INVERSE COLUMN PERMUTATION                 
C R INTEGER N-VECTOR GIVING ROW PERMUTATION                             
C C INTEGER N-VECTOR GIVING COLUMN PERMUTATION                          
C THRESH REAL SCALAR GIVING THRESHHOLD FOR PIVOTING                     
C EPS   REAL SCALAR GIVING THREHHOLD FOR SINGULARITY                    
C GETA  SUBROUTINE WHICH RETURNS THE NONZERO ELEMENTS AND               
C  THEIR CORRESPONDING COLUMN INDICES WHEN ASKED FOR A                  
C  GIVEN ROW                                                            
C OUTPUT PARAMETERS                                                     
C IA INTEGER N+1 VECTOR GIVING BEGINNING OF ITH ROW                     
C    OF L IN SPARSE LU DECOMPOSITION IN JA AND A                        
C JA COLUMN INDICES OF LU DECOMPOSITION                                 
C A LU DECOMPOSITION                                                    
C IU INTEGER N VECTOR GIVING BEGINNING OF U ROWS IN IA AND JA           
C IC INVERSE COLUMN PERMUTATION AFTER PIUVOTING FOR STABILITY           
C C  COLUMN PERMUTATION AFTER PIVOTING FOR STABILITY                    
C IERR- ERROR FLAG                                                      
C     10 .LT. IERR .LT. 11+N   NULL ROW                                 
C     10+N .LT. IERR .LT. 11+2N   INVALID COLUMN INDEX                  
C     10+2N .LT. IERR .LT. 11+3N  NOT ENOUGH SPACE                      
C     10+3N .LT. IERR .LT. 11+4N   SINGULAR MATRIX OF RANK IERR-3N-10   
C JLU INTEGER SCALAR GIVING SIZE OF DECOMPOSITION                       
C GROWTH SCALAR GIVING ELEMENT GROWTH                                   
C BEFOR SCALR GIVING NORM OF ORIGINAL MATRIX                            
C SCRATCH SPACE                                                         
C IROW -INTEGER LENGTH N+1                                              
C Z-REAL LENGTH N                                                       
C INITIALIZATION                                                        
      IA(1) = 1                                                         
      BEFOR = 0.0E0                                                     
      AFT = 0.0E0                                                       
      NP1 = N+1                                                         
      IERR = 0                                                          
      NP2 = N+2                                                         
      JLU = 1                                                           
      DO  1 I = 1, N                                                    
         IROW(I) = 0                                                    
   1     CONTINUE                                                       
C DETERMINE NEXT ROW OF L AND U                                         
      DO  23 K = 1, N                                                   
         RK = R(K)                                                      
         SING=.FALSE.                                                   
         M = NP1                                                        
C CALL GETA TO GET ROW RK                                               
         CALL GETA(RK, A(JLU), JA(JLU), NUM)                            
         Z(K) = 0.0                                                     
         JAMAX = JLU+NUM-1                                              
C CHECK IF ENOUGH SPACE                                                 
         IF (JAMAX .GT. IAMAX) GOTO  24                                 
         IROW(NP1) = NP1                                                
         IUEND = NP2                                                    
         BEFO = 0.0                                                     
C CHECK FOR NULL ROW                                                    
         IF (NUM .LE. 0) GOTO  25                                       
         DO  6 J = JLU, JAMAX                                           
            JJ = JA(J)                                                  
C CHECK FOR INVLAID INDEX                                               
            TEMP = JJ .LE. 0                                            
            IF (.NOT. TEMP) TEMP = JJ .GT. N                            
            IF (TEMP) GOTO  29                                          
C GET INVERSE COLUMN INDEX                                              
            ICC = IC(JJ)                                                
            Z(ICC) = A(J)                                               
            BEFO = BEFO+ABS(A(J))                                       
C CHECK IF IN L OR U PORTION OF ROW                                     
            IF (ICC .LT. K) GOTO 2                                      
               IROW(ICC) = IUEND                                        
C IF IT IS U PORTION-SO JUST ADD TO LINKED LIST                         
               IUEND = ICC                                              
               GOTO  5                                                  
   2           IF (M .GT. ICC) M = NP1                                  
C NEW ELEMENT IS IN L PART OF THE ROW                                   
C THUS ONE MUST INSERT IN ORDERED LIST                                  
   3              II = IROW(M)                                          
                  IF (II .GE. ICC) GOTO  4                              
                  M = II                                                
                  GOTO  3                                               
   4           IROW(M) = ICC                                            
               IROW(ICC) = II                                           
   5        CONTINUE                                                    
   6        CONTINUE                                                    
         BEFOR = AMAX1(BEFO, BEFOR)                                     
C                                                                       
C ELIMINATE ROW                                                         
         I = NP1                                                        
   7        J = I                                                       
            I = IROW(I)                                                 
            IROW(J) = 0                                                 
            IF (I .EQ. NP1) GOTO  15                                    
            JMIN = IU(I)                                                
            RI = R(I)                                                   
            JMAX = IA(I+1)-1                                            
C AKI WILL HAVE MULTIPLIER                                              
            AKI = -Z(I)                                                 
            AFT = AMAX1(AFT, ABS(AKI))                                  
C CHECK IF SUFFICIENT SPACE                                             
C TO STORE ELEMENT OF L                                                 
            IF (JLU .GT. IAMAX) GOTO  26                                
C STORE ELEMENT OF L                                                    
            A(JLU) = AKI                                                
            JA(JLU) = I                                                 
            JLU = JLU+1                                                 
            IF (JMAX .LT. JMIN) GOTO 14                                 
               DO  13 J = JMIN, JMAX                                    
C ELIMINATE ITH ELEMENT                                                 
                  JUJ = JA(J)                                           
                  ICC = IC(JUJ)                                         
C IF IROW(ICC) IS 0 THEN FILL IN WILL OCCUR                             
                  IF (IROW(ICC) .NE. 0) GOTO 12                         
                     IF (ICC .LT. K) GOTO 8                             
                        IROW(ICC) = IUEND                               
C SINCE FILLIN IS IN U PORITION JSUT ADD TO END OF LINKED LIST          
                        IUEND = ICC                                     
                        GOTO  11                                        
   8                    TEMP = M .GT. ICC                               
                        IF (.NOT. TEMP) TEMP = M .LT. I                 
                        IF (TEMP) M = I                                 
C FILL IN IS IN L PORTION SO ONE MUST INSERT IN ORDERED LIST            
   9                       IJ = IROW(M)                                 
                           IF (IJ .GE. ICC) GOTO  10                    
                           M = IJ                                       
                           GOTO  9                                      
  10                    IROW(M) = ICC                                   
                        IROW(ICC) = IJ                                  
  11                 Z(ICC) = 0.E0                                      
  12              Z(ICC) = Z(ICC)+AKI*A(J)                              
  13              CONTINUE                                              
  14        CONTINUE                                                    
C FIND PIVOT                                                            
            GOTO  7                                                     
  15     I = IUEND                                                      
         PMAX = 0.E0                                                    
         NU = 0                                                         
  16     IF (I .EQ. NP2) GOTO  18                                       
            PVT = ABS(Z(I))                                             
            NU = NU+1                                                   
            IF (PVT .LE. PMAX) GOTO 17                                  
               IMAX = I                                                 
               PMAX = PVT                                               
  17        I = IROW(I)                                                 
            GOTO  16                                                    
C DO THRESHHOLD PIVOTING                                                
  18     IF (ABS(Z(K)) .GE. THRESH*PMAX) IMAX = K                       
C CHECK FOR SINGULARITY                                                 
         IF (ABS(Z(IMAX)) .GT. EPS) GOTO  27                            
            IF(IERR.EQ.0)IERR=K+10+3*N                                  
            SING=.TRUE.                                                 
   27       CONTINUE                                                    
         IF (.NOT.SING)DK = 1.E0/Z(IMAX)                                
         IF (SING)DK = R1MACH(2)                                        
         AFT = AMAX1(AFT, PMAX)                                         
         I = IUEND                                                      
         NU = JLU+NU                                                    
C CHECK IF SUFFICIENT SPACE TO STORE U PORTION                          
         IF (NU .GT. IAMAX) GOTO  28                                    
C STORE DIAGONAL ELEMENT                                                
         A(JLU) = DK                                                    
         JA(JLU) = C(IMAX)                                              
         IU(K) = JLU+1                                                  
         JLU = NU                                                       
  19     IF (I .EQ. NP2) GOTO  21                                       
C CHECK IF DIAGONAL ELEMENT WHICH HAS ALREADY BEEN STORED               
            IF (I .EQ. IMAX) GOTO 20                                    
               NU = NU-1                                                
C STORE ELEMENT OF U                                                    
               A(NU) = Z(I)*DK                                          
               JA(NU) = C(I)                                            
  20        II = I                                                      
            I = IROW(I)                                                 
            IROW(II) = 0                                                
            GOTO  19                                                    
  21     IA(K+1) = JLU                                                  
C INTERCHANGE THE COLUMN INDICES                                        
         IF (K .EQ. IMAX) GOTO 22                                       
            JJ = C(K)                                                   
            C(K) = C(IMAX)                                              
            C(IMAX) = JJ                                                
            CK = C(K)                                                   
            IC(CK) = K                                                  
            IC(JJ) = IMAX                                               
  22     CONTINUE                                                       
  23     CONTINUE                                                       
      GROWTH = 1.0E0                                                    
      IF (BEFOR.NE.0.0E0) GROWTH = AMAX1(1.0E0, AFT/BEFOR)              
      JA(JLU) = JLU                                                     
      RETURN                                                            
  24  IERR = 2*N+K+10                                                   
      RETURN                                                            
  25  IERR = K+10                                                       
      RETURN                                                            
  26  IERR = 2*N+K+10                                                   
      RETURN                                                            
  28  IERR = 2*N+K+10                                                   
      RETURN                                                            
  29  IERR = K+10+N                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE S4FSL(N, R, C, IA, JA, A, IU, B, IB, NB, TMP)          
      INTEGER N, IB, NB                                                 
      INTEGER R(N), C(N), IA(1), JA(1), IU(N)                           
      REAL A(1), B(IB, NB), TMP(N)                                      
      INTEGER IIB, IIC, IUI, JUJ, I, J                                  
      INTEGER K, JMIN, JMAX, JJ, IR, NP1                                
      REAL SUM, DK                                                      
C THIS IS LOWER LEVEL SUBROUTINE FOR SPFSL                              
C SPARSE FORWARD SOLVE                                                  
      NP1 = N+1                                                         
      DO  7 K = 1, NB                                                   
         DO  3 I = 1, N                                                 
            IR = R(I)                                                   
            JMIN = IA(I)                                                
            IUI = IU(I)-1                                               
            JMAX = IUI-1                                                
            DK = A(IUI)                                                 
            SUM = B(IR, K)                                              
            IF (JMIN .GT. JMAX) GOTO 2                                  
               DO  1 J = JMIN, JMAX                                     
                  JJ = JA(J)                                            
                  SUM = SUM+A(J)*TMP(JJ)                                
   1              CONTINUE                                              
   2        TMP(I) = SUM*DK                                             
   3        CONTINUE                                                    
C SPARSE BACKWARD SOLVE                                                 
         DO  6 IIB = 1, N                                               
            I = NP1-IIB                                                 
            JMIN = IU(I)                                                
            JMAX = IA(I+1)-1                                            
            SUM = TMP(I)                                                
            IF (JMIN .GT. JMAX) GOTO 5                                  
               DO  4 J = JMIN, JMAX                                     
                  JUJ = JA(J)                                           
                  SUM = SUM-A(J)*B(JUJ, K)                              
   4              CONTINUE                                              
   5        IIC = C(I)                                                  
            B(IIC, K) = SUM                                             
   6        CONTINUE                                                    
   7     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE SPFNF(N, MRP, MCP, AROW, IWORK, A, GROWTH, EPS)        
      INTEGER N                                                         
      EXTERNAL AROW                                                     
      INTEGER MRP(N), MCP(N), IWORK(1)                                  
      REAL A(1), GROWTH, EPS                                            
      REAL DD(1000)                                                     
      COMMON /CSTAK/ DD                                                 
      INTEGER JLU, IST(1000), ROW, ISTKGT, NERROR, I                    
      INTEGER K, IDEX, NERR, IROW, ROWM1, ID                            
      INTEGER INDEX                                                     
      EQUIVALENCE (DD(1), IST(1))                                       
C                                                                       
C                                                                       
C NUMERICAL FACTORIZATION                                               
C INPUT PARAMETERS                                                      
C N    ORDER OF MATRIX                                                  
C MRP   INTEGER VECTOR OF LENGTH N GIVING ROW PERMUTATIONS              
C MCP   INTEGER VECTOR OF LENGTH N GIVING COLUMN PERMUTATIONS           
C IWORK INTEGER VECTOR OF SYMBOLIC FACTORIZATION COMPUTED BY            
C        SPFSF                                                          
C EPS     LARGEST NONACCEPTABLE PIVOT FOR SINGULARITY TEST              
C AROW    SUBROUTINE,DECLARED EXTERNAL IN THE USERS MAIN PROGRAM        
C         WHICH HAS THE CALLING SEQUENCE AROW(I,ROW,ROW,NUM)            
C         WITH INPUT PARAMETER I AND OUTPUT PARAMETERS ROW, ROW         
C         AND NUM WHICH RETURNS IN THE D.P. VECTOR ROW THE              
C         NONZERO ENTRIES IN THE ITH ROW OF A. THE VECTOR               
C         ROW SHOULD BE FILLED WITH THE CORRESPONDING COLUMN            
C         INDICES OF THE NONZERO ELEMENTS. NUM GIVES THE NUMBER         
C         OF THE ELEMENTS                                               
C OUTPUT PARAMETERS                                                     
C A     NUMERICAL FACTORIZATION OF MATRIX                               
C GROWTH    GROWTH FACTOR                                               
C STORAGE ALCATED -2N REAL AND N INTEGER LOCATIONS                      
C       ARE RETURNED                                                    
C ERROR STATES                                                          
C 1   N.LT.1     FATAL                                                  
C 10+K SINGULAR MATRIX OF RANK K   RECOVERABLE                          
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13H SPFNF-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPFNF-N.LT.1', 13, 1, 2)              
C/                                                                      
      CALL ENTER(1)                                                     
      ID = ISTKGT(2*N, 3)                                               
      ROW = ISTKGT(N, 2)                                                
      ROWM1 = ROW-1                                                     
      IROW = ID+N                                                       
      DO  1 I = 1, N                                                    
         K = MCP(I)                                                     
         INDEX = ROWM1+K                                                
         IST(INDEX) = I                                                 
   1     CONTINUE                                                       
      JLU = 2*N+2                                                       
      CALL S4FNF(N, MRP, IST(ROW), IWORK, IWORK(N+2), IWORK(JLU), A,    
     1   AROW, DD(IROW), GROWTH, EPS, MCP)                              
C/6S                                                                    
C     IF (NERROR(NERR) .NE. 0) CALL N5ERR(22H SPFNF-SINGULAR MATRIX, 22,
C    1   NERR, 1)                                                       
C/7S                                                                    
      IF (NERROR(NERR) .NE. 0) CALL N5ERR(' SPFNF-SINGULAR MATRIX', 22, 
     1   NERR, 1)                                                       
C/                                                                      
      DO  2 I = 1, N                                                    
         IDEX = ROWM1+I                                                 
         INDEX = IST(IDEX)                                              
         MCP(INDEX) = I                                                 
   2     CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE S4FNF(N, R, IC, IL, IU, JU, U, GETA, ROW, G, EPS,      
     1   JROW)                                                          
      INTEGER N                                                         
      EXTERNAL GETA                                                     
      INTEGER R(N), IC(N), IL(1), IU(1), JU(1), JROW(N)                 
      REAL U(1), ROW(1), G, EPS                                         
      INTEGER NUM, I, J, K, IMIN, JMIN                                  
      INTEGER IMAX, JMAX, TEMP1, TEMP2, JJ, IR                          
      INTEGER IT1, IT2, IMINM1                                          
      REAL BEF, ABS, AFT, TEMP, AMAX1, DK                               
      REAL LI                                                           
C                                                                       
C                                                                       
C       INPUT VARIABLES N,IL,IU,JU                                      
C       OUTPUT VARIABLES--   G,  U,                                     
C       PARAMETERS USED INTERNALLY--                                    
C FIA     ROW - HOLDS INTERMEDIATE VALUES IN CALCULATION OF  U AND L.   
C                 SIZE = N.                                             
C RF       G      ELEMENT GROWTH                                        
C RN      EPS      LARGEST NONACCEPTABLE PIVOT                          
C  INTERNAL VARIABLES--                                                 
C    JMIN, JMAX - INDICES OF THE FIRST AND LAST POSITIONS IN A ROW TO   
C      BE EXAMINED.                                                     
C    AFT - NORM OF MATRIX AFTER DECOMPOSITION                           
C    BEF   NORM OF MATRIX ORIGINALLY                                    
      BEF = 0.0                                                         
      AFT = 0.0                                                         
      DO  1 I = 1, N                                                    
         ROW(I) = 0.E0                                                  
   1     CONTINUE                                                       
      DO  9 K = 1, N                                                    
C  ******  SET THE INITIAL STRUCTURE OF ROW  ************************** 
         IR = R(K)                                                      
         IMIN = IL(K)                                                   
         IMINM1 = IMIN-1                                                
         CALL GETA(IR, U(IMIN), JROW, NUM)                              
         DO  2 J = 1, NUM                                               
            JJ = J+IMINM1                                               
            IF (ABS(U(JJ)) .GT. BEF) BEF = ABS(U(JJ))                   
            IT1 = JROW(J)                                               
            IT2 = IC(IT1)                                               
            ROW(IT2) = U(JJ)                                            
   2        CONTINUE                                                    
C  ******  ASSIGN THE KTH ROW OF L AND ADJUST ROW,   ***************    
         IMAX = IU(K)-2                                                 
         IF (IMIN .GT. IMAX) GOTO 6                                     
            DO  5 I = IMIN, IMAX                                        
               TEMP2 = JU(I)                                            
               LI = -ROW(TEMP2)                                         
               ROW(TEMP2) = 0.E0                                        
               U(I) = LI                                                
               JMIN = IU(TEMP2)                                         
               JMAX = IL(TEMP2+1)-1                                     
               IF (JMIN .GT. JMAX) GOTO 4                               
                  DO  3 J = JMIN, JMAX                                  
                     TEMP1 = JU(J)                                      
                     ROW(TEMP1) = ROW(TEMP1)+LI*U(J)                    
   3                 CONTINUE                                           
   4           CONTINUE                                                 
   5           CONTINUE                                                 
C  ******  ASSIGN KTH ROW OF U AND DIAGONAL D, SET TMP(K)  ************ 
   6     IF (ABS(ROW(K)) .LE. EPS) GOTO  10                             
         DK = 1E0/ROW(K)                                                
         ROW(K) = 0.E0                                                  
         JMIN = IU(K)                                                   
         U(JMIN-1) = DK                                                 
         AFT = AMAX1(AFT, ABS(ROW(K)))                                  
         JMAX = IL(K+1)-1                                               
         IF (JMIN .GT. JMAX) GOTO 8                                     
            DO  7 J = JMIN, JMAX                                        
               TEMP1 = JU(J)                                            
               TEMP = ROW(TEMP1)                                        
               ROW(TEMP1) = 0.E0                                        
               IF (ABS(TEMP) .GT. AFT) AFT = ABS(TEMP)                  
               U(J) = TEMP*DK                                           
   7           CONTINUE                                                 
   8     CONTINUE                                                       
   9     CONTINUE                                                       
C  ******  NORMAL RETURN AND ERROR RETURNS  *************************** 
      G = AMAX1(AFT/BEF, 1.0)                                           
      RETURN                                                            
C  ZERO DIAGONAL ELEMENT                                                
C/6S                                                                    
C 10  CALL SETERR(22H SPFNF-SINGULAR MATRIX, 22, K+9, 1)                
C/7S                                                                    
  10  CALL SETERR(' SPFNF-SINGULAR MATRIX', 22, K+9, 1)                 
C/                                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE SPFML(N, AROW, X, B)                                   
      INTEGER N                                                         
      REAL  X(N), B(N)                                                  
      EXTERNAL AROW                                                     
      REAL SUM                                                          
      INTEGER JMAX, I, J, JJ, IIA, IIAM1, JP, JR, IROW, IROWM1          
      REAL R(1000)                                                      
      INTEGER IA(1000)                                                  
      COMMON /CSTAK/  R                                                 
      EQUIVALENCE (R(1),IA(1))                                          
C THIS SUBROUTINE MULTIPLIES A BY X AND PUTS THE RESULT IN B WHERE      
C A COMPUTED BY A FUNCTION                                              
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C AROW    FUNCTION WHICH DELIVERS TO SPFML ONE ROW OF THE               
C         MATRIX AT A TIME                                              
C X       N-VECTOR TO BE MULTIPLIED                                     
C OUTPUT PARAMETERS                                                     
C B      A*X                                                            
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C STORAGE TAKEN FROM STACK- N REAL AND N INTEGER LOCATIONS              
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13H SPFML-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPFML-N.LT.1', 13, 1, 2)              
C/                                                                      
      CALL ENTER(1)                                                     
      IIA=ISTKGT(N, 2)                                                  
      IIAM1=IIA-1                                                       
      IROW = ISTKGT(N, 3)                                               
      IROWM1=IROW-1                                                     
      DO 30 I=1,N                                                       
         CALL AROW(I, R(IROW), IA(IIA), JMAX)                           
         SUM=0.0                                                        
         IF (JMAX.LT.1) GO TO 20                                        
         DO 10 JJ=1,JMAX                                                
            JP=JJ+IIAM1                                                 
            J=IA(JP)                                                    
            JR=JJ+IROWM1                                                
            SUM=SUM+R(JR)*X(J)                                          
  10     CONTINUE                                                       
  20     B(I)=SUM                                                       
  30  CONTINUE                                                          
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE SPMML(N, IA, JA, A,  X, B)                             
      INTEGER N                                                         
      INTEGER IA(N), JA(N)                                              
      REAL A(N), X(N), B(N)                                             
      REAL SUM                                                          
      INTEGER JMIN, JMAX, I, J, JJ                                      
C THIS SUBROUTINE MULTIPLIES A BY X AND PUTS THE RESULT IN B WHERE      
C A IS A SPARSE MATRIX                                                  
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C IA      INTEGER VECTOR, LENGTH N+1, POINTING TO BEGINNINGS            
C         OF ROWS IN JA AND A VECTORS                                   
C JA      COLUMN INDICES OF NONZERO ELEMENTS OF MATRIX                  
C A       NONZERO ELEMENTS OF THE MATRIX                                
C X       N-VECTOR TO BE MULTIPLIED                                     
C OUTPUT PARAMETERS                                                     
C B      A*X                                                            
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13H SPMML-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPMML-N.LT.1', 13, 1, 2)              
C/                                                                      
      SUM=0.0                                                           
      DO 30 I=1,N                                                       
         JMIN=IA(I)                                                     
         JMAX=IA(I+1)-1                                                 
         SUM=0.0                                                        
         IF (JMAX.LT.JMIN) GO TO 20                                     
         DO 10 JJ=JMIN,JMAX                                             
            J=JA(JJ)                                                    
            SUM=SUM+A(JJ)*X(J)                                          
  10     CONTINUE                                                       
  20     B(I)=SUM                                                       
  30  CONTINUE                                                          
      RETURN                                                            
      END                                                               
        SUBROUTINE SPMIN(N,IC,IWORK,I,AROW,JROW,NUM,INDEX,A)            
C                                                                       
C THIS SUBROUTINE INSERTS IN THE VECTOR A THE ELEMENTS OF               
C AROW ACCORDING TO THE SCHEME RECORDED IN IWORK AND                    
C DETERMINED BY SPMSF                                                   
C                                                                       
C INPUT PARAMETERS                                                      
C N       NUMBER OF ROWS IN ORIGINAL MATRIX                             
C IC      INTEGER VECTOR OF LENGTH N CONTAINING INVERSE OF              
C         COLUMN PERMUTATIONS                                           
C IWORK   INTEGER VECTOR OUTPUT FROM SPMSF                              
C I       WHICH ROW NOW INTSERTING                                      
C AROW    REAL VECTOR LENGTH NUM OF NONZERO ELEMENTS IN ROW I           
C         OF ORIGINAL MATRIX                                            
C JROW    INTEGER VECTOR OF LENGTH NUM OF COLUMN INDICES IN ROW I       
C         OF ORIGINAL VECTOR                                            
C NUM     NUMBER OF NONZERO ELEMENTS IN ROW I NOW INSERTING INTO A      
C INDEX   IF INDEX IS 1 THE WHOLE ARRAY A IS ZEROED OUT BEFOREHAND      
C         THIS ALLOWS ONE TO CALL THE SUBROUTINE SEVERAL TIMES          
C         FOR 1 ROW AND TO ADD THE NEW INFORMATION                      
C OUTPUT PARAMETER                                                      
C A       VECTOR FOR INSERT INTO DSPMNF WITH THE INFORMATION            
C         FROM AROW INSERTED                                            
C ERROR STATES                                                          
C 1 N.LT.1                                                              
C 2 I NOT IN 1 THOUGH N                                                 
C 3 NUM.LT.1                                                            
C 4 INDEX IN JROW VECTOR FOR ROW I WAS NOT KNOWN TO SPMSF               
C                                                                       
        INTEGER N,I,NUM                                                 
        INTEGER IC(N),JROW(N),INDEX,IWORK(1)                            
        REAL A(N),AROW(NUM)                                             
        INTEGER NTOT,K,IR,JS,JE,KK,MM,M,MIN,IDISP                       
C/6S                                                                    
C       IF (N.LT.1) CALL SETERR(13H SPMIN-N.LT.1,13,1,2)                
C       IF (NUM.LT.1) CALL SETERR(15H SPMIN-NUM.LT.1,15,3,2)            
C       IF(I.LT.1.OR.I.GT.N) CALL SETERR(21H SPMIN-I OUT OF RANGE,      
C    1      21,2,2)                                                     
C/7S                                                                    
        IF (N.LT.1) CALL SETERR(' SPMIN-N.LT.1',13,1,2)                 
        IF (NUM.LT.1) CALL SETERR(' SPMIN-NUM.LT.1',15,3,2)             
        IF(I.LT.1.OR.I.GT.N) CALL SETERR(' SPMIN-I OUT OF RANGE',       
     1      21,2,2)                                                     
C/                                                                      
         IF (INDEX.NE.1) GO TO 10                                       
            NTOT=IWORK(N+1)-1                                           
            DO 1 K=1,NTOT                                               
                 A(K)=0.0E0                                             
 1          CONTINUE                                                    
 10      IR=I                                                           
         IDISP=2*N+1                                                    
         JS=IWORK(IR)+IDISP                                             
         JE= IWORK(IR+1)+IDISP-1                                        
         KK=JS                                                          
         DO 50 K=1,NUM                                                  
            MM=JROW(K)                                                  
            M=IC(MM)                                                    
            IF (IWORK(KK)-M)30,40,20                                    
 20         IF (KK.EQ.JS) GO TO 80                                      
            KK=KK-1                                                     
            IF (IWORK(KK)-M)80,40,20                                    
 30         IF (KK.EQ.JE) GO TO 80                                      
            KK=KK+1                                                     
            IF (IWORK(KK)-M)30,40,80                                    
 40         MIN=KK-IDISP                                                
            A(MIN)=A(MIN)+AROW(K)                                       
 50     CONTINUE                                                        
        RETURN                                                          
C/6S                                                                    
C80     CALL SETERR(20H SPMIN-UNKNOWN INDEX,20,4,2)                     
C/7S                                                                    
 80     CALL SETERR(' SPMIN-UNKNOWN INDEX',20,4,2)                      
C/                                                                      
        RETURN                                                          
        END                                                             
      SUBROUTINE  SPMNF(N, IWORK, A,  EPS, GROWTH)                      
      INTEGER N, IWORK(1), JLU                                          
      REAL A(1),  EPS, GROWTH                                           
      REAL DD(1000)                                                     
      COMMON /CSTAK/ DD                                                 
      INTEGER ID, NERR, ISTKGT, NERROR, ISP(1)                          
      EQUIVALENCE (DD(1), ISP(1))                                       
C NUMERICAL FACTORIZATION                                               
C INPUT PARAMETERS                                                      
C N    ORDER OF MATRIX                                                  
C IWORK INTEGER VECTOR OF SYMBOLIC FACTORIZATION COMPUTED BY            
C        SPMSF                                                          
C EPS     LARGEST NONACCEPTABLE PIVOT FOR SINGULARITY TEST              
C A       VECTOR COMPUTED BY  SPMIN ACCORDING TO INSTRUCTIONS           
C         GIVEN IN  SPMSF                                               
C OUTPUT PARAMETERS                                                     
C A     NUMERICAL FACTORIZATION OF MATRIX                               
C GROWTH NUMERICAL ELEMENT GROWTH. IF THIS MUCH .GT. 1, THEN THE        
C        COMPUTED DECOMPOSITION MAY BE THE DECOMPOSITION OF A MATRIX    
C        THAT IS NOT VERY CLOSE TO THE ORIGINAL MATRIX.                 
C STORAGE ALLOCATED -N REAL LOCATION WHICH                              
C       ARE RETURNED                                                    
C ERROR STATES                                                          
C 1   N.LT.1     FATAL                                                  
C 10+K SINGULAR MATRIX OF RANK K   RECOVERABLE                          
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13H SPMNF-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPMNF-N.LT.1', 13, 1, 2)              
C/                                                                      
      CALL ENTER(1)                                                     
      ID = ISTKGT(N, 3)                                                 
       JLU=2*N+2                                                        
       CALL  S4MNF(N,IWORK,IWORK(N+2),IWORK(JLU),A,DD(ID),GROWTH,EPS)   
C/6S                                                                    
C     IF (NERROR(NERR) .NE. 0) CALL N5ERR(22H SPMNF-SINGULAR MATRIX, 22,
C    1   NERR, 1)                                                       
C/7S                                                                    
      IF (NERROR(NERR) .NE. 0) CALL N5ERR(' SPMNF-SINGULAR MATRIX', 22, 
     1   NERR, 1)                                                       
C/                                                                      
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE  S4MNF(N, IL, IU, JU, U, ROW, G, EPS)                  
      INTEGER N, IL(N), IU(N), JU(N)                                    
      REAL U(N), ROW(N), G, EPS                                         
      INTEGER IMIN, JMIN, IMAX, JMAX, I, J                              
      INTEGER K                                                         
      REAL DK, ABS, LI, TEMP, BEF, AFT                                  
      INTEGER TEMP1, TEMP2                                              
C       INPUT VARIABLES N,IL,IU,JU                                      
C       OUTPUT VARIABLES--   G,  U,                                     
C       PARAMETERS USED INTERNALLY--                                    
C FIA     ROW - HOLDS INTERMEDIATE VALUES IN CALCULATION OF  U AND L.   
C                 SIZE = N.                                             
C RF       G      ELEMENT GROWTH                                        
C RN      EPS      LARGEST NONACCEPTABLE PIVOT                          
C  INTERNAL VARIABLES--                                                 
C    JMIN, JMAX - INDICES OF THE FIRST AND LAST POSITIONS IN A ROW TO   
C      BE EXAMINED.                                                     
C    SUM - USED IN CALCULATING  TMP.                                    
C  ******  FOR EACH ROW  ********************************************** 
      BEF = 0.0                                                         
      AFT = 0.0                                                         
      DO  8 K = 1, N                                                    
C  ******  SET THE INITIAL STRUCTURE OF ROW  ************************** 
         JMIN = IL(K)                                                   
         JMAX = IL(K+1)-1                                               
         DO  1 J = JMIN, JMAX                                           
            IF (ABS(U(J)) .GT. BEF) BEF = ABS(U(J))                     
            TEMP2 = JU(J)                                               
            ROW(TEMP2) = U(J)                                           
   1        CONTINUE                                                    
C  ******  ASSIGN THE KTH ROW OF L AND ADJUST ROW,   ***************    
         IMIN = IL(K)                                                   
         IMAX = IU(K)-2                                                 
         IF (IMIN .GT. IMAX) GOTO 5                                     
            DO  4 I = IMIN, IMAX                                        
               TEMP2 = JU(I)                                            
               LI = -ROW(TEMP2)                                         
               U(I) = LI                                                
               JMIN = IU(TEMP2)                                         
               JMAX = IL(TEMP2+1)-1                                     
               IF (JMIN .GT. JMAX) GOTO 3                               
                  DO  2 J = JMIN, JMAX                                  
                     TEMP1 = JU(J)                                      
                     ROW(TEMP1) = ROW(TEMP1)+LI*U(J)                    
   2                 CONTINUE                                           
   3           CONTINUE                                                 
   4           CONTINUE                                                 
C  ******  ASSIGN KTH ROW OF U AND DIAGONAL D, SET TMP(K)  ************ 
   5     IF (ABS(ROW(K)) .LE. EPS) GOTO  9                              
         DK = 1.E0/ROW(K)                                               
         JMIN = IU(K)                                                   
         U(JMIN-1) = DK                                                 
         AFT=AMAX1(AFT,ABS(ROW(K)))                                     
         JMAX = IL(K+1)-1                                               
         IF (JMIN .GT. JMAX) GOTO 7                                     
            DO  6 J = JMIN, JMAX                                        
               TEMP1 = JU(J)                                            
               TEMP = ROW(TEMP1)                                        
               IF (ABS(TEMP) .GT. AFT) AFT = ABS(TEMP)                  
               U(J) = TEMP*DK                                           
   6           CONTINUE                                                 
   7     CONTINUE                                                       
   8     CONTINUE                                                       
C  ******  NORMAL RETURN AND ERROR RETURNS  *************************** 
      G = AFT/BEF                                                       
      RETURN                                                            
C  ZERO DIAGONAL ELEMENT                                                
C/6S                                                                    
C  9  CALL SETERR(22H S4MNF-SINGULAR MATRIX, 22, K+9, 1)                
C/7S                                                                    
   9  CALL SETERR(' S4MNF-SINGULAR MATRIX', 22, K+9, 1)                 
C/                                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE  SPSOL(N, MRP,MCP, IWORK,UL,  B, IB, NB)               
      INTEGER IB, NB, N                                                 
       INTEGER MRP(N), MCP(N), IWORK(1)                                 
      REAL UL(1), B(IB, NB)                                             
      REAL D(1000)                                                      
      COMMON /CSTAK/ D                                                  
      INTEGER  ITMP, ISTKGT                                             
C FORWARD SOLVE FOR SPARSE MATRICES                                     
C INPUT PARAMETERS                                                      
C N     NUMBER OF EQUATIONS                                             
C MRP   INTEGER VECTOR OF ROW PERMUTATIONS                              
C MCP   INTEGER VECTOR,LENGTHN OR COLUMN PERMUTATIONS                   
C IWORK INTEGER VECTOR COMPUTED BY  SPF(ORM)NF                          
C UL    REAL ARRAY COMPUTED BY  SPF(ORM)NF                              
C IU    INTEGER VECTOR OF LENGTH N POINTING TO U IN LU                  
C       DECOMPOSITION,COMPUTED BY SPLU,SF(ORM)NF,OR SPCE                
C B     MATRIX OF RIGHT HAND SIDES                                      
C IB    ROW DIMENSION OF B MATRIX                                       
C NB    NUMBER OF RIGHT HAND SIDES                                      
C OUTPUT PARAMETERS                                                     
C B     THE SOLUTION MATRIX                                             
C STORAGE SPACE ALLOCATED AND DEALLOCATED -N REAL LOCATIONS             
C ERROR CONDITIONS                                                      
C 1 N.LT.1    FATAL                                                     
C 2 IB.LT.N   FATAL                                                     
C 3 NB.LT.1   FATAL                                                     
C/6S                                                                    
C     IF (N.LT.1) CALL SETERR(13H SPSOL-N.LT.1,13,1,2)                  
C     IF (IB.LT.N) CALL SETERR(14H SPSOL-IB.LT.N,14,2,2)                
C     IF(NB.LT.1) CALL SETERR(14H SPSOL-NB.LT.1,14,3,2)                 
C/7S                                                                    
      IF (N.LT.1) CALL SETERR(' SPSOL-N.LT.1',13,1,2)                   
      IF (IB.LT.N) CALL SETERR(' SPSOL-IB.LT.N',14,2,2)                 
      IF(NB.LT.1) CALL SETERR(' SPSOL-NB.LT.1',14,3,2)                  
C/                                                                      
      CALL ENTER(1)                                                     
      ITMP = ISTKGT(N, 3)                                               
      JLU=2*N+2                                                         
      CALL  S4SOL(N,MRP,MCP,IWORK,IWORK(JLU),UL, IWORK(N+2),            
     1  B,IB,NB,D(ITMP))                                                
       CALL LEAVE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE  S4SOL(N, R,C, IA, JA, A, IU,  B, IB, NB, TMP)         
      INTEGER IB, NB, N                                                 
      INTEGER R(N), IA(1), JA(1), IU(N), C(N)                           
      REAL A(1), B(IB, NB), TMP(N)                                      
      INTEGER JJ, IR, JMIN, JMAX, NP1, I                                
      INTEGER J, K, IUI                                                 
      REAL DK, SUM                                                      
C SPARSE FORWARD SOLVE                                                  
      NP1 = N+1                                                         
      DO  10 K = 1, NB                                                  
         DO  5 I = 1, N                                                 
            IR = R(I)                                                   
            JMIN = IA(I)                                                
            IUI = IU(I)-1                                               
            JMAX=IUI-1                                                  
            DK = A(IUI)                                                 
            SUM = B(IR, K)                                              
               IF (JMIN .GT. JMAX) GOTO 4                               
                  DO  2 J = JMIN, JMAX                                  
                     JJ = JA(J)                                         
                     SUM = SUM+A(J)*TMP(JJ)                             
   2                 CONTINUE                                           
   4        TMP(I) = SUM*DK                                             
   5        CONTINUE                                                    
         DO  9 IIB = 1, N                                               
            I = NP1-IIB                                                 
            JMIN = IU(I)                                                
            JMAX = IA(I+1)-1                                            
            SUM = TMP(I)                                                
            IF (JMIN .GT. JMAX) GOTO 8                                  
                  DO  7 J = JMIN, JMAX                                  
                     JUJ = JA(J)                                        
                     SUM = SUM-A(J)*TMP(JUJ)                            
   7                 CONTINUE                                           
   8        IIC = C(I)                                                  
            TMP(I) = SUM                                                
            B(IIC,K) = SUM                                              
   9        CONTINUE                                                    
   10     CONTINUE                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE DSPMLE(N, ORDER, IA, JA, A, ISIZE, B, IB, NB)          
      INTEGER N, IB, NB                                                 
      INTEGER IA(N), JA(N), ISIZE                                       
      EXTERNAL I4YX                                                     
      DOUBLE PRECISION A(N), B(IB, NB)                                  
      LOGICAL ORDER                                                     
      DOUBLE PRECISION DSTAK(500)                                       
      COMMON /CSTAK/ DSTAK                                              
      INTEGER MCP, MAX, JLU, MRP, ISTKGT, ISTKQU                        
      INTEGER I, L, IEND, ILIN, IERR, ISUB                              
      INTEGER LAST, TEMP, IC, IHEAD, MC, II(1000)                       
      INTEGER IV, IU, LU, MR, IZ, ILEFT                                 
      INTEGER IC1                                                       
      DOUBLE PRECISION BEF, EPS, D(500), D1MACH                         
      LOGICAL TEMP1                                                     
      EQUIVALENCE (D(1), II(1), DSTAK(1))                               
C THIS SUBROUTINE SOLVES AX = B WHERE A IS A SPARSE MATRIX              
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C ORDER   LOGICAL VARIABLE. IF .TRUE. REORDERING FOR                    
C         STABILITY WILL BE PERFORMED                                   
C IA      INTEGER VECTOR, LENGTH N+1, POINTING TO BEGINNINGS            
C         OF ROWS IN JA AND A VECTORS                                   
C JA      COLUMN INDICES OF NONZERO ELEMENTS OF MATRIX                  
C A       NONZERO ELEMENTS OF THE MATRIX                                
C B      RIGHT HAND SIDE MATRIX,DESTROYED ON OUTPUT                     
C IB     ROW DIMENSION OF B                                             
C NB     NUMBER OF COLUMNSOF B                                          
C OUTPUT PARAMETERS                                                     
C B      THE SOLUTION TO AX=B                                           
C ISIZE ACTUAL NUMBER OF ELEMENTS IN THE PORT STACK NEEDED TO           
C       SAVE THE DECOMPOSITION                                          
C SPACE ALLOCATED AND DEALLOCATED-3N+2 INTEGER AND N                    
C DOUBLE PRECISION LOCATIONS PLUS ISIZE INTEGER AND                     
C DOUBLE PRECISION LOCATIONS NEED TO HOLD THE U FROM THE                
C LU DECOMPOSITION OF A                                                 
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C 2 IB .LT. N                                                           
C 3 NB .LT. 1                                                           
C 10+K     NULL ROW           FATAL                                     
C N+K+10   INCORRECT COLUMN INDEX AT ROW K     FATAL                    
C 3N+K+10   SINGULAR MATRIX OF RANK K      RECOVERABLE                  
C 2N+K+10  RAN OUT OF SPACE WHEN PROCEESING ROW K                       
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HDSPMLE-N.LT.1, 13, 1, 2)             
C     IF (IB .LT. N) CALL SETERR(14HDSPMLE-IB.LT.N, 14, 2, 2)           
C     IF (NB .LT. 1) CALL SETERR(14HDSPMLE-NB.LT.1, 14, 3, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DSPMLE-N.LT.1', 13, 1, 2)              
      IF (IB .LT. N) CALL SETERR('DSPMLE-IB.LT.N', 14, 2, 2)            
      IF (NB .LT. 1) CALL SETERR('DSPMLE-NB.LT.1', 14, 3, 2)            
C/                                                                      
      CALL ENTER(1)                                                     
C GET SPACE FROM THE STACK FOR COLUMN ORDERING AND THE INVERSE          
C COLUMN ORDERING AND A  D.PRECISION VECTOR OF LENGTH N USED IN DS4MLE  
      IZ = ISTKGT(N, 4)                                                 
      IC = ISTKGT(2*N, 2)                                               
      MCP = IC+N                                                        
      IC1 = IC-1                                                        
      MC = MCP-1                                                        
      ILEFT = ISTKQU(2)-10                                              
C ISUB CONSTAINS THE NUMBER OF LOCATIONS NEEDED TO STORE THE ROW        
C PERMUTATION VECTOR, THE POINTER TO U AND AND AN INTEGER TEMPORARY     
C VECTOR                                                                
      ISUB = 3*N+3                                                      
C FIND THE NORM OF THE MATRIX                                           
      IEND = IA(N+1)-1                                                  
      BEF = 0.D0                                                        
      DO  1 I = 1, IEND                                                 
         BEF = DMAX1(BEF, DABS(A(I)))                                   
   1     CONTINUE                                                       
      EPS = D1MACH(4)*BEF                                               
      IF (ORDER) GOTO 3                                                 
         ISUB = ISUB-N                                                  
C WHEN THERE IS NO PERMUTATION MATRIX NO STORAGE IS NECESSARY           
C FOR THE ROW PERMUTATION VECTOR                                        
C SET UP COLUMN PERMUTATION ARRAYS TO INDICATE NO PERMUTATION           
         DO  2 I = 1, N                                                 
            IC1 = IC1+1                                                 
            II(IC1) = I                                                 
            MC = MC+1                                                   
            II(MC) = I                                                  
   2        CONTINUE                                                    
         GOTO  5                                                        
   3     JLU = ISTKGT(ILEFT, 2)                                         
C ALLOCATE ALL THE REMAINING SPACE                                      
         MAX = (ILEFT-N)/2                                              
         IV = JLU                                                       
         L = IV+MAX                                                     
         IHEAD = L+MAX                                                  
         IERR = 2*N+11                                                  
         IF (MAX .LT. N) GOTO  12                                       
         IERR = 0                                                       
         CALL S4MDM(N, I4YX, MAX, II(IV), II(L), II(IHEAD), II(MCP),    
     1      II(IC), II(IV), IERR,IA,JA,1)                               
         IF (IERR .EQ. 0) GOTO 4                                        
            IF (IERR .GT. 2*N+10) GOTO  12                              
            IF (IERR .LE. N+10) GOTO  11                                
            TEMP1 = IERR .GT. N+10                                      
            IF (TEMP1) TEMP1 = IERR .LE. 2*N+10                         
C/6S                                                                    
C           IF (TEMP1) CALL SETERR(29HDSPMLE-INCORRECT COLUMN INDEX, 29,
C    1         IERR, 2)                                                 
C/7S                                                                    
            IF (TEMP1) CALL SETERR('DSPMLE-INCORRECT COLUMN INDEX', 29, 
     1         IERR, 2)                                                 
C/                                                                      
   4     CALL ISTKRL(1)                                                 
C ILEFT IS THE NUMBER OF LOCATIONS LEFT IN THE STACK TO STORE U         
   5  ILEFT = ILEFT-ISUB                                                
C ALLOCATE THE SPACE BETWEEN INTEGER AND D.P. LOCATIONS DEPENDENT       
C ON THE SPACE REQUIRED FOR EACH                                        
      ILIN = (ILEFT*II(7))/(II(7)+II(9))                                
      JLU = ISTKGT(ILIN, 2)                                             
      LU = ISTKGT(ILIN, 4)                                              
      MRP = 1                                                           
      IU = ISTKGT(ISUB, 2)                                              
      TEMP = IU+N+2                                                     
      IF (.NOT. ORDER) GOTO 7                                           
         MC = MCP-1                                                     
C PUT THE ROW PERMUTATION  VECTOR EQUAL TO THE COLUMN                   
C PERMUTATION VECTOR                                                    
         MR = TEMP+N                                                    
         MRP = MR+1                                                     
         DO  6 I = 1, N                                                 
            MC = MC+1                                                   
            MR = MR+1                                                   
            II(MR) = II(MC)                                             
   6        CONTINUE                                                    
   7  CALL DS4MLE(N,B, IB, NB, II(JLU), D(LU), ILIN, II(IU), II(TEMP), D
     1   (IZ), II(IC), II(MRP), II(MCP), IERR, 0.1D0, LAST, IA, JA, A,  
     1   ORDER, EPS)                                                    
      IF (IERR .NE. 0) GOTO 8                                           
         ISIZE = LAST-1                                                 
         CALL DS4MBS(N,II(IU), II(JLU), D(LU), II(MCP), B, IB, NB, D(IZ)
     1      )                                                           
         GOTO  13                                                       
   8     IF (IERR .LE. N+10) GOTO 11                                    
            IF (IERR .LE. 3*N+10) GOTO  12                              
C/6S                                                                    
C           CALL SETERR(22HDSPMLE-SINGULAR MATRIX, 22, IERR, 1)         
C/7S                                                                    
            CALL SETERR('DSPMLE-SINGULAR MATRIX', 22, IERR, 1)          
C/                                                                      
            GOTO  13                                                    
C/6S                                                                    
C 11  CALL SETERR(15HDSPMLE-NULL ROW, 15, IERR, 1)                      
C/7S                                                                    
  11  CALL SETERR('DSPMLE-NULL ROW', 15, IERR, 1)                       
C/                                                                      
      GOTO  13                                                          
C/6S                                                                    
C 12  CALL SETERR(25HDSPMLE-INSUFFICIENT SPACE, 25, IERR, 1)            
C/7S                                                                    
  12  CALL SETERR('DSPMLE-INSUFFICIENT SPACE', 25, IERR, 1)             
C/                                                                      
  13  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DS4MLE(N, B, IB, NB, JA, A, IAMAX, IU, IROW, Z,        
     1   IC, R, C, IERR, THRESH, JLU, IA, JJA, AA, ORDER, EPS)          
      INTEGER N, IB, NB                                                 
      INTEGER JA(1), IAMAX, IU(N), IROW(N), IC(N), R(N)                 
      INTEGER C(N), IERR, JLU, IA(N), JJA(N)                            
      DOUBLE PRECISION B(IB, NB), A(1), Z(1), THRESH, AA(N), EPS        
      LOGICAL ORDER                                                     
      INTEGER ICC, JUJ, NUM, I, J, K                                    
      INTEGER JAMA, M, JMIN, IMAX, JMAX, CK                             
      INTEGER II, JJ, IJ, IR, MM, RK                                    
      INTEGER JAMIN, NU, IUEND, NP1, NP2                                
      DOUBLE PRECISION AKI, PVT, PMAX, DK                               
      LOGICAL TEMP                                                      
C THIS SUBROUTINE IS LOWER LEVEL SUBROUTINE FOR DSPMLE                  
C PARAMETERS NOT DEFINED IN DSPMLE ARE AS FOLLOWS                       
C JA -SPACE FOR STORING COLUMN INDICES OF U IN LU DECOMPOSITION         
C A - SPACE FOR STORING ELEMENTS OF U IN LU DECOMPOSITION               
C IAMAX - SIZE OF JA AND A                                              
C IU - POINTER INTO JA AND A GIVING BEGINNING OF EACH ROW               
C Z  TEMP VECTOR OF LENGTH N                                            
C IC- INTEGER VECTOR GIVING INVERTED COLUMN PERMUTATION                 
C R - IF ORDERING INTEGER VECTOR OF LENGTH N GIVING ROW PERMUTATIONS    
C C - COLUMN PERMUTATIONS, INTEGER VECTOR OF LENGTH N                   
C IERR - ERROR FLAG. OK IF SET OT 0 ON OUTPUT                           
C THRESH - THRESHHOLD PARAMETER FOR PIVOTING                            
C JLU - NUMBER OF ELEMENTS OF JA AND A ACTUALLY USED                    
C ORDER - LOGICAL VARIABLE. IF .TRUE. THEN PIVOTING FOR SPARSITY        
C WAS DONE AND R SHOULD BE LOOKED AT.                                   
C EPS - CRITERIA FOR SINGULARITY                                        
C IAMAX - SIZE OF SPACE AVAILABLE FOR SORING LU DECOMPOSITION           
C INITIALIZATION                                                        
      IU(1) = 1                                                         
C REARRANGE RIGHT HAND SIDES                                            
      IF (.NOT. ORDER) GOTO 4                                           
         DO  3 J = 1, NB                                                
            DO  1 I = 1, N                                              
               IR = R(I)                                                
               Z(I) = B(IR, J)                                          
   1           CONTINUE                                                 
            DO  2 I = 1, N                                              
               B(I, J) = Z(I)                                           
   2           CONTINUE                                                 
   3        CONTINUE                                                    
   4  NP1 = N+1                                                         
      IERR = 0                                                          
      NP2 = N+2                                                         
      JLU = 1                                                           
      DO  5 I = 1, N                                                    
         IROW(I) = 0                                                    
   5     CONTINUE                                                       
C DETERMINE NEXT ROW OF L AND U                                         
      DO  29 K = 1, N                                                   
         M = NP1                                                        
         IROW(NP1) = NP1                                                
         IUEND = NP2                                                    
         RK = K                                                         
         IF (ORDER) RK = R(K)                                           
         JAMIN = IA(RK)                                                 
         JAMA = IA(RK+1)-1                                              
         NUM = JAMA-JAMIN                                               
         Z(K) = 0.D0                                                    
         NUM = NUM+1                                                    
C CHECK FOR NULL ROW                                                    
         IF (JAMA .LT. JAMIN) GOTO  30                                  
         DO  10 J = JAMIN, JAMA                                         
            JJ = JJA(J)                                                 
C CHECK FOR VALID COLUMN INDEX                                          
            TEMP = JJ .GT. N                                            
            IF (.NOT. TEMP) TEMP = JJ .LT. 1                            
C/6S                                                                    
C           IF (TEMP) CALL SETERR(29HDSPMLE-INCORRECT COLUMN INDEX, 29  
C    1         , K+10+N, 2)                                             
C/7S                                                                    
            IF (TEMP) CALL SETERR('DSPMLE-INCORRECT COLUMN INDEX', 29   
     1         , K+10+N, 2)                                             
C/                                                                      
            ICC = IC(JJ)                                                
            Z(ICC) = AA(J)                                              
            IF (ICC .LT. K) GOTO 6                                      
               IROW(ICC) = IUEND                                        
C NEW ELEMENT IS IN U PART OF ROW                                       
               IUEND = ICC                                              
               GOTO  9                                                  
   6           IF (M .GT. ICC) M = NP1                                  
C NEW ELEMENT IS IN L PART OF THE ROW                                   
   7              II = IROW(M)                                          
                  IF (II .GE. ICC) GOTO  8                              
                  M = II                                                
                  GOTO  7                                               
   8           IROW(M) = ICC                                            
               IROW(ICC) = II                                           
   9        CONTINUE                                                    
  10        CONTINUE                                                    
C                                                                       
C ELIMINATE ROW                                                         
         I = NP1                                                        
  11        J = I                                                       
            I = IROW(I)                                                 
            IROW(J) = 0                                                 
            IF (I .EQ. NP1) GOTO  20                                    
            JMIN = IU(I)                                                
            JMAX = IU(I+1)-1                                            
            AKI = -Z(I)                                                 
C FORWARD SOLVE                                                         
            DO  12 MM = 1, NB                                           
               B(K, MM) = B(K, MM)+AKI*B(I, MM)                         
  12           CONTINUE                                                 
            IF (JMAX .LT. JMIN) GOTO 19                                 
               DO  18 J = JMIN, JMAX                                    
C ELIMINATE ITH ELEMENT                                                 
                  JUJ = JA(J)                                           
                  ICC = IC(JUJ)                                         
                  IF (IROW(ICC) .NE. 0) GOTO 17                         
                     IF (ICC .LT. K) GOTO 13                            
                        IROW(ICC) = IUEND                               
C FILL IN-SEE IF IT IS IN U OR L                                        
C FILL IN IS IN U                                                       
                        IUEND = ICC                                     
                        GOTO  16                                        
  13                    TEMP = M .GT. ICC                               
C FILL IS IN L PORTION                                                  
                        IF (.NOT. TEMP) TEMP = M .LT. I                 
                        IF (TEMP) M = I                                 
  14                       IJ = IROW(M)                                 
                           IF (IJ .GE. ICC) GOTO  15                    
                           M = IJ                                       
                           GOTO  14                                     
  15                    IROW(M) = ICC                                   
                        IROW(ICC) = IJ                                  
C SINCE THIS IS A FILL IN, INITIALIZE SPACE                             
  16                 Z(ICC) = 0.D0                                      
  17              Z(ICC) = Z(ICC)+AKI*A(J)                              
  18              CONTINUE                                              
  19        CONTINUE                                                    
            GOTO  11                                                    
  20     I = IUEND                                                      
         PMAX = 0.D0                                                    
         NU = -1                                                        
  21     IF (I .EQ. NP2) GOTO  23                                       
            PVT = DABS(Z(I))                                            
            NU = NU+1                                                   
            IF (PVT .LE. PMAX) GOTO 22                                  
               IMAX = I                                                 
               PMAX = PVT                                               
  22        I = IROW(I)                                                 
            GOTO  21                                                    
C DO THRESHHOLD PIVOTING                                                
  23     IF (DABS(Z(K)) .GE. THRESH*PMAX) IMAX = K                      
C CHECK FOR SINGULARITY                                                 
         IF (DABS(Z(IMAX)) .LE. EPS) GOTO  31                           
C CHECK IF SUFFICIENT SPACE                                             
         IF (NU+JLU .GT. IAMAX) GOTO  32                                
         NU = JLU+NU                                                    
         JLU = NU                                                       
         DK = 1.D0/Z(IMAX)                                              
C FIX UP RIGHT HAND SIDE                                                
         DO  24 MM = 1, NB                                              
            B(K, MM) = B(K, MM)*DK                                      
  24        CONTINUE                                                    
         I = IUEND                                                      
  25     IF (I .EQ. NP2) GOTO  27                                       
            IF (I .EQ. IMAX) GOTO 26                                    
               NU = NU-1                                                
               A(NU) = Z(I)*DK                                          
               JA(NU) = C(I)                                            
  26        II = I                                                      
            I = IROW(I)                                                 
C ZERO OUT SPCE SO NEXT TIME WILL NOT MISINTERPRET FILL-IN              
            IROW(II) = 0                                                
            GOTO  25                                                    
  27     IU(K+1) = JLU                                                  
C INTERCHANGE THE COLUMN INDICES                                        
         IF (K .EQ. IMAX) GOTO 28                                       
            JJ = C(K)                                                   
            C(K) = C(IMAX)                                              
            C(IMAX) = JJ                                                
            CK = C(K)                                                   
            IC(CK) = K                                                  
            IC(JJ) = IMAX                                               
  28     CONTINUE                                                       
  29     CONTINUE                                                       
      IU(N+1) = JLU                                                     
      JA(JLU) = JLU                                                     
      RETURN                                                            
  30  IERR = K+10                                                       
      RETURN                                                            
  31  IERR = 3*N+10+K                                                   
      RETURN                                                            
  32  IERR = 2*N+K+10                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE DS4MBS(N, IA, JA, A, C, B, IB, NB, TMP)                
      INTEGER N, IB, NB                                                 
      INTEGER IA(1), JA(1), C(N)                                        
      DOUBLE PRECISION A(1), B(IB, NB), TMP(N)                          
      INTEGER IIB, IIC, JUJ, I, J, K                                    
      INTEGER JMIN, JMAX, NP1                                           
      DOUBLE PRECISION SUM                                              
C SPARSE BACK SOLVE                                                     
      NP1 = N+1                                                         
      DO  5 K = 1, NB                                                   
         DO  3 IIB = 1, N                                               
            I = NP1-IIB                                                 
            JMIN = IA(I)                                                
            JMAX = IA(I+1)-1                                            
            SUM = B(I, K)                                               
            IF (JMIN .GT. JMAX) GOTO 2                                  
               DO  1 J = JMIN, JMAX                                     
                  JUJ = JA(J)                                           
                  SUM = SUM-A(J)*TMP(JUJ)                               
   1              CONTINUE                                              
   2        IIC = C(I)                                                  
            TMP(IIC) = SUM                                              
   3        CONTINUE                                                    
         DO  4 I = 1, N                                                 
            B(I, K) = TMP(I)                                            
   4        CONTINUE                                                    
   5     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DSPFLE(N, ORDER, AROW, ISIZE, B, IB, NB)               
C                                                                       
C  THIS IS LINDA KAUFMANS SPARSE MATRIX PACKAGE                         
C  DOUBLE PRECISION                                                     
C                                                                       
C  DECEMBER 9, 1982 (REVISION)                                          
C                                                                       
      INTEGER IB, NB                                                    
      EXTERNAL AROW, I4YX                                               
      INTEGER N, ISIZE                                                  
      DOUBLE PRECISION B(IB, NB)                                        
      LOGICAL ORDER                                                     
      DOUBLE PRECISION DSTAK(500)                                       
      COMMON /CSTAK/ DSTAK                                              
      INTEGER IIA, IMC, MCP, MAX, IEX, IMR                              
      INTEGER JLU, MRP, NUM, NUMBER, ISTKGT, ISTKQU                     
      INTEGER I, K, L, ILIN, IERR, ISUB                                 
      INTEGER TEMP, LAST, TEMP1, IA, IC, IHEAD                          
      INTEGER II(1000), JJ, LL, IT, IV, IU                              
      INTEGER LU, IZ, NU, ILEFT, IC1                                    
      DOUBLE PRECISION BEF, EPS, D(500), BE                             
      DOUBLE PRECISION D1MACH                                           
      LOGICAL MOVE                                                      
      EQUIVALENCE (D(1), II(1), DSTAK(1))                               
C THIS SUBROUTINE SOLVES AX = B WHERE A IS A SPARSE MATRIX              
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C ORDER   LOGICAL VARIABLE. IF .TRUE. REORDERING FOR                    
C         SPARSITY WILL BE PERFORMED.                                   
C AROW    SUBROUTINE OF THE FORM GETA(I,ROW,JCOL,NUM) WHICH             
C         FOR A GIVEN INPUT I RETURNS THE NONZERO ELEMENTS OF           
C         THE ITH ROW OF THE MATRIX A IN THE                            
C         DOUBLE PRECISION VECTOR ROW AND THE CORRESPONDING INDICES IN  
C         JCOL. THE VARIABLE NUM RETURNS THE NUMBER OF NONZERO          
C         ELEMENTS IN THE ITH ROW. AROW SHOULD BE DECLARED              
C         EXTERNAL IN THE CALLING PROGRAM.                              
C B      RIGHT HAND SIDE MATRIX,DESTROYED ON OUTPUT                     
C IB     ROW DIMENSION OF B                                             
C NB     NUMBER OF COLUMNSOF B                                          
C OUTPUT PARAMETERS                                                     
C B      THE SOLUTION TO AX=B                                           
C ISIZE  ACTUAL NUMBER OF ELEMENTS IN THE A ARRAY THAT WAS NEEDED       
C        FOR THE DECOMPOSITION                                          
C SPACE ALLOCATED AND DEALLOCATED-3N+2 INTEGER AND N LE                 
C DOUBLE PRECISION LOCATIONS PLUS ISIZE INTEGER AND                     
C DOUBLE PRECISION LOCATIONS NEED TO HOLD THE U FROM THE                
C LU DECOMPOSITION OF A                                                 
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C 2 IB .LT. N                                                           
C 3 NB .LT. 1                                                           
C 10+K     NULL ROW           FATAL                                     
C 3N+K+10   SINGULAR MATRIX OF RANK K      RECOVERABLE                  
C 2N+K+10  RAN OUT OF SPACE WHEN PROCEESING ROW K                       
C N+K+10 INCORRECT COLUMN INDEX                                         
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HDSPFLE-N.LT.1, 13, 1, 2)             
C     IF (IB .LT. N) CALL SETERR(14HDSPFLE-IB.LT.N, 14, 3, 2)           
C     IF (NB .LT. 1) CALL SETERR(14HDSPFLE-NB.LT.1, 14, 4, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DSPFLE-N.LT.1', 13, 1, 2)              
      IF (IB .LT. N) CALL SETERR('DSPFLE-IB.LT.N', 14, 3, 2)            
      IF (NB .LT. 1) CALL SETERR('DSPFLE-NB.LT.1', 14, 4, 2)            
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(N+1, 2)                                               
      IIA = IA                                                          
      II(IA) = 1                                                        
C GET SPACE FOR THE COLUMN PERMUTATION VECTOR AND ITS INVERSE           
      IC = ISTKGT(2*N, 2)                                               
      IC1 = IC-1                                                        
      ILEFT = ISTKQU(2)-10                                              
      ISUB = 2*N+2                                                      
      IF (ORDER) ISUB = 3*N+2                                           
      ILIN = ((ILEFT-ISUB)*II(7))/(II(7)+II(9))                         
C GET SPACE FOR A AND LU AND DIVIDE IT BETWEEN DOUBLE PRECISION AND INTE
      JLU = ISTKGT(ILIN, 2)                                             
      LU = ISTKGT(ILIN, 4)                                              
      LL = LU                                                           
      JJ = JLU                                                          
      NU = ILIN                                                         
      MCP = IC+N                                                        
      IMC = MCP-1                                                       
C READ IN THE MATRIX AND COMPUTE ITS NORM                               
      BEF = 0.D0                                                        
      DO  2 I = 1, N                                                    
         CALL AROW(I, D(LL), II(JJ), NUM)                               
         BE = 0.D0                                                      
         L = LL                                                         
         IERR = I+10                                                    
         IF (NUM .LT. 1) GOTO  12                                       
         DO  1 K = 1, NUM                                               
            BE = BE+DABS(D(L))                                          
            L = L+1                                                     
   1        CONTINUE                                                    
         BEF = DMAX1(BE, BEF)                                           
         IIA = IIA+1                                                    
         II(IIA) = II(IIA-1)+NUM                                        
         JJ = JJ+NUM                                                    
         LL = LL+NUM                                                    
         NU = NU-NUM                                                    
         IF (NU .LT. N) GOTO  11                                        
         TEMP1 = I+IC1                                                  
         IMC = IMC+1                                                    
         II(IMC) = I                                                    
         II(TEMP1) = I                                                  
   2     CONTINUE                                                       
      IF (.NOT. ORDER) GOTO 6                                           
         IERR=2*N+11                                                    
         IF (NU .LE. 4*II(IIA)-N) GOTO 3                                
            MOVE = .FALSE.                                              
C GENERATE COLUMN ORDERING WHEN PIVOTING FOR SPARSITY                   
C TEST IF HAVE ENOUGH SPACE INBETWEEN PRESENT COLUMN INDICES            
C AND NONZERO ELEMENTS                                                  
            GOTO  4                                                     
   3        MOVE = .TRUE.                                               
C DO NOT HAVE ENOUGH SPACE AND REPACKING IS NECESSARY                   
            CALL ISTKRL(1)                                              
            NUMBER = II(IIA)                                            
            JLU= ISTKMD(NUMBER)                                         
            IT = ISTKGT(NUMBER, 4)                                      
C MOVE D. PRECISION ELEMENTS FROM HALFWAY IN STACK UNTIL THE END        
            CALL MOVEFD(NUMBER, D(LU), D(IT))                           
            NU = ISTKQU(2)-10                                           
            JJ = ISTKGT(NU, 2)                                          
   4     MAX = (NU-N)/2                                                 
         IV = JJ                                                        
         L = IV+MAX                                                     
         IHEAD = L+MAX                                                  
         IF (MAX .LT. N) GOTO  11                                       
         IERR = 0                                                       
         CALL S4MDM(N, I4YX, MAX, II(IV), II(L), II(IHEAD),             
     1      II(MCP), II(IC), II(IV), IERR,II(IA),II(JLU),1)             
         IF (IERR .GT. 2*N+10) GOTO  11                                 
C/6S                                                                    
C        IF (IERR .GT. N+10) CALL SETERR(                               
C    1      29HDSPFLE-INCORRECT COLUMN INDEX, 29, IERR, 2)              
C/7S                                                                    
         IF (IERR .GT. N+10) CALL SETERR(                               
     1      'DSPFLE-INCORRECT COLUMN INDEX', 29, IERR, 2)               
C/                                                                      
         IF (.NOT. MOVE) GOTO 5                                         
            CALL ISTKRL(2)                                              
C PUT THINGS BACK WITH NUMERICAL ELEMENTS IN THE MIDDLE OF THE          
C AVAILABLE SPACE                                                       
            JLU= ISTKMD(ILIN)                                           
            LU = ISTKGT(ILIN, 4)                                        
            CALL MOVEBD(NUMBER, D(IT), D(LU))                           
   5     CONTINUE                                                       
   6  IEX = JLU+ILIN-N                                                  
      IZ = LU+ILIN-N                                                    
      ILIN = ILIN-N                                                     
      IU = ISTKGT(ISUB, 2)                                              
      TEMP = IU+N+1                                                     
      MRP = 1                                                           
      IF(.NOT.ORDER) GO TO 77                                           
C PUT ROW ORDERING TO COLUMN ORDERING                                   
      MRP = TEMP+N+1                                                    
      IMR = MRP                                                         
      IMC = MCP                                                         
      DO  7 I = 1, N                                                    
         II(IMR) = II(IMC)                                              
         IMC = IMC+1                                                    
         IMR = IMR+1                                                    
   7     CONTINUE                                                       
   77    CONTINUE                                                       
      EPS = BEF*D1MACH(4)                                               
      CALL DS4FLE(N,II(IA), II(JLU), D(LU), ILIN, II(IU), II(TEMP), D(  
     1   IZ), II(IEX), B, IB, NB, II(IC), II(MRP), II(MCP), ORDER, IERR,
     1   0.1D0, LAST, EPS)                                              
      IF (IERR .NE. 0) GOTO 8                                           
         ISIZE = LAST-1                                                 
         CALL DS4FBS(N, B, IB, NB, II(JLU), D(LU), II(IA), II(IU), II(  
     1      IEX), II(MCP), II(MRP), ORDER, D(IZ))                       
         GOTO  13                                                       
   8     IF (IERR .LE. N+10) GOTO  12                                   
         IF (IERR .GT. 2*N+10) GOTO 9                                   
C/6S                                                                    
C           CALL SETERR(22HDSPFLE-SINGULAR MATRIX, 22, IERR, 1)         
C/7S                                                                    
            CALL SETERR('DSPFLE-SINGULAR MATRIX', 22, IERR, 1)          
C/                                                                      
            GOTO  13                                                    
   9     CONTINUE                                                       
C/6S                                                                    
C 11  CALL SETERR(25HDSPFLE-INSUFFICIENT SPACE, 25, IERR, 1)            
C/7S                                                                    
  11  CALL SETERR('DSPFLE-INSUFFICIENT SPACE', 25, IERR, 1)             
C/                                                                      
      GOTO  13                                                          
C/6S                                                                    
C 12  CALL SETERR(15HDSPFLE-NULL ROW, 15, IERR, 1)                      
C/7S                                                                    
  12  CALL SETERR('DSPFLE-NULL ROW', 15, IERR, 1)                       
C/                                                                      
  13  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DS4FLE(N, IA, JA, A, IAMAX, IU, IROW, Z, IEX, B,       
     1   IB, NB, IC, R, C, ORDER, IERR, THRESH, LAST, EPS)              
      INTEGER N, IB, NB                                                 
      INTEGER IA(1), JA(1), IAMAX, IU(N), IROW(N), IEX(N)               
      INTEGER IC(N), R(N), C(N), IERR, LAST                             
      DOUBLE PRECISION A(1), Z(1), B(IB, NB), THRESH, EPS               
      LOGICAL ORDER                                                     
      INTEGER ICC, JUJ, JLU, NUU, MIN0, I                               
      INTEGER J, K, M, CMAX, JMIN, IMAX                                 
      INTEGER JMAX, CI, CK, NC, II, JJ                                  
      INTEGER IJ, RI, RK, IR, MM, JAMIN                                 
      INTEGER JAMAX, NU, ISPAC, IUEND, LASTA, NP1                       
      INTEGER JDMAX1                                                    
      DOUBLE PRECISION AKI, PVT, PMAX, DK                               
      LOGICAL TEMP                                                      
C THIS IS LOWER LEVEL OF SFLE                                           
C PARAMETERS NOT DEFINED IN SFLE                                        
C IAMAX- SPACE LEFT IN STACK TO WORK IN                                 
C IA - POINTS TO BEGINNING OF EACH ROW IN A AND JA                      
C JA COLUMN INDICES NONZERO ELEMENTS                                    
C A NUMERICAL NONZERO ELEMENTS                                          
C IU END OF U PORTION OF THE ROW- INTEGER VECTOR OF LENGTH N            
C IROW  INTEGER SCRATCH VECTOR LENGTH N                                 
C Z DOUBLE PRECISION SCRATCH VECTOR LENGTH N                            
C IEX - INTEGER SCRATCH VECTOR LENGTH N POINTING TO EXTRA SPACE         
C IC INTGER VECTOR LENGTH N OF INVERSE COLUMN ORDER                     
C C INTEGER VECTOR LENGTH N OF COLUMN ORDER                             
C R IF ORDERING USED AN INTEGER VECTOR OF LENGTH N GIVING ROW ORDER     
C IERR -ERROR PARAMETER                                                 
C THRESH-THRESHHOLD PARAMETER                                           
C LAST- HOW MUCH SPACE ACTUALLY USED                                    
C EPS CRITERIA FOR SINGULARITY                                          
C INITIALIZATION                                                        
C REARRANGE RIGHT HAND SIDES                                            
      IF (.NOT. ORDER) GOTO 4                                           
         DO  3 J = 1, NB                                                
            DO  1 I = 1, N                                              
               IR = R(I)                                                
               Z(I) = B(IR, J)                                          
   1           CONTINUE                                                 
            DO  2 I = 1, N                                              
               B(I, J) = Z(I)                                           
   2           CONTINUE                                                 
   3        CONTINUE                                                    
   4  LAST = IA(N+1)                                                    
      LASTA = LAST                                                      
      NP1 = N+1                                                         
      IERR = 0                                                          
      DO  5 I = 1, N                                                    
         IROW(I) = 0                                                    
   5     CONTINUE                                                       
C DETERMINE NEXT ROW OF L AND U                                         
      DO  32 K = 1, N                                                   
         NC = C(K)                                                      
         Z(NC) = 0.0D0                                                  
         IEX(K) = LAST                                                  
         RK = K                                                         
         IF (ORDER) RK = R(K)                                           
         M = NP1                                                        
         JAMIN = IA(RK)                                                 
         IROW(NP1) = NP1                                                
         IUEND = NP1                                                    
         JDMAX1 = IA(RK+1)                                              
         JAMAX = JDMAX1-1                                               
         DO  10 J = JAMIN, JAMAX                                        
            JJ = JA(J)                                                  
            Z(JJ) = A(J)                                                
            TEMP = JJ .LT. 1                                            
            IF (.NOT. TEMP) TEMP = JJ .GT. N                            
C/6S                                                                    
C           IF (TEMP) CALL SETERR(29HDSPFLE-INCORRECT COLUMN INDEX, 29  
C    1         , K+10+N, 2)                                             
C/7S                                                                    
            IF (TEMP) CALL SETERR('DSPFLE-INCORRECT COLUMN INDEX', 29   
     1         , K+10+N, 2)                                             
C/                                                                      
            ICC = IC(JJ)                                                
            IF (ICC .LT. K) GOTO 6                                      
               IROW(ICC) = IUEND                                        
               IUEND = ICC                                              
               GOTO  9                                                  
   6           IF (M .GT. ICC) M = NP1                                  
C NEW ELEMENT IS IN L PART OF THE ROW                                   
   7              II = IROW(M)                                          
                  IF (II .GE. ICC) GOTO  8                              
                  M = II                                                
                  GOTO  7                                               
   8           IROW(M) = ICC                                            
               IROW(ICC) = II                                           
   9        CONTINUE                                                    
  10        CONTINUE                                                    
C                                                                       
C ELIMINATE ROW                                                         
         I = NP1                                                        
  11        J = I                                                       
            I = IROW(I)                                                 
            IROW(J) = 0                                                 
            IF (I .EQ. NP1) GOTO  23                                    
            RI = I                                                      
            IF (ORDER) RI = R(I)                                        
            JMIN = IA(RI)                                               
            JMAX = MIN0(IA(RI+1)-1, IU(I))                              
            CI = C(I)                                                   
            AKI = -Z(CI)                                                
C FORWARD SOLVE                                                         
            DO  12 MM = 1, NB                                           
               B(K, MM) = B(K, MM)+AKI*B(I, MM)                         
  12           CONTINUE                                                 
            IF (JMAX .LT. JMIN) GOTO 22                                 
  13              DO  19 J = JMIN, JMAX                                 
C ELIMINATE ITH ELEMENT                                                 
                     JUJ = JA(J)                                        
                     ICC = IC(JUJ)                                      
                     IF (IROW(ICC) .NE. 0) GOTO 18                      
                        IF (ICC .LT. K) GOTO 14                         
                           IROW(ICC) = IUEND                            
C FILL IN-SEE IF IT IS IN U OR L                                        
                           IUEND = ICC                                  
                           GOTO  17                                     
  14                       TEMP = M .GT. ICC                            
                           IF (.NOT. TEMP) TEMP = M .LT. I              
                           IF (TEMP) M = I                              
  15                          IJ = IROW(M)                              
                              IF (IJ .GE. ICC) GOTO  16                 
                              M = IJ                                    
                              GOTO  15                                  
  16                       IROW(M) = ICC                                
                           IROW(ICC) = IJ                               
  17                    Z(JUJ) = 0.D0                                   
  18                 Z(JUJ) = Z(JUJ)+AKI*A(J)                           
  19                 CONTINUE                                           
                  IF (JMAX .EQ. IU(I)) GOTO  21                         
                  JMIN = IEX(I)                                         
                  JMAX = IU(I)                                          
  20              IF (JMIN .LE. JMAX) GOTO  13                          
  21        CONTINUE                                                    
  22        CONTINUE                                                    
            GOTO  11                                                    
  23     I = IUEND                                                      
         PMAX = 0.D0                                                    
         NU = -1                                                        
  24     IF (I .EQ. NP1) GOTO  26                                       
            CI = C(I)                                                   
            PVT = DABS(Z(CI))                                           
            NU = NU+1                                                   
            IF (PVT .LE. PMAX) GOTO 25                                  
               IMAX = I                                                 
               PMAX = PVT                                               
  25        I = IROW(I)                                                 
            GOTO  24                                                    
C DO THRESHHOLD PIVOTING                                                
  26     IF (DABS(Z(NC)) .GE. THRESH*PMAX) IMAX = K                     
         CMAX = C(IMAX)                                                 
C CHECK FOR SINGULARITY                                                 
         IF (DABS(Z(CMAX)) .LE. EPS) GOTO  33                           
         ISPAC = JAMAX-JAMIN+1                                          
C SEE IF SUFFICIENT SPACE AVAILABLE                                     
         IF (NU-ISPAC+LAST .GE. IAMAX) GOTO  34                         
         NUU = JAMIN+NU-1                                               
         IF (NUU .GT. JAMAX) NUU = LAST+NU-ISPAC-1                      
         IU(K) = NUU                                                    
         JLU = NUU+1                                                    
         DK = 1.D0/Z(CMAX)                                              
C FIX UP RIGHT HAND SIDE                                                
         DO  27 MM = 1, NB                                              
            B(K, MM) = B(K, MM)*DK                                      
  27        CONTINUE                                                    
         I = IUEND                                                      
  28     IF (I .EQ. NP1) GOTO  30                                       
            IF (I .EQ. IMAX) GOTO 29                                    
               CI = C(I)                                                
C STORE ELEMENT OF U                                                    
               A(NUU) = Z(CI)*DK                                        
               JA(NUU) = CI                                             
               IF (NUU .EQ. LAST) NUU = JDMAX1                          
               NUU = NUU-1                                              
  29        II = I                                                      
            I = IROW(I)                                                 
            IROW(II) = 0                                                
            GOTO  28                                                    
  30     IF (JLU .GT. LAST) LAST = JLU                                  
C INTERCHANGE THE COLUMN INDICES                                        
         IF (K .EQ. IMAX) GOTO 31                                       
            JJ = C(K)                                                   
            C(K) = CMAX                                                 
            C(IMAX) = JJ                                                
            CK = C(K)                                                   
            IC(CK) = K                                                  
            IC(JJ) = IMAX                                               
  31     CONTINUE                                                       
  32     CONTINUE                                                       
      JA(LAST) = LAST                                                   
      RETURN                                                            
  33  IERR = 3*N+9+K                                                    
      RETURN                                                            
  34  IERR = 2*N+K+10                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE DS4FBS(N, B, IB, NB, JA, A, IA, IU, IEX, C, R,         
     1   ORDER, TMP)                                                    
      INTEGER N, IB, NB                                                 
      INTEGER JA(1), IA(1), IU(N), IEX(N), C(N), R(N)                   
      DOUBLE PRECISION B(IB, NB), A(1), TMP(N)                          
      LOGICAL ORDER                                                     
      INTEGER IIB, IIC, JUJ, MIN0, I, J                                 
      INTEGER K, JMIN, JMAX, RI, NP1                                    
      DOUBLE PRECISION SUM                                              
      NP1 = N+1                                                         
      DO  8 K = 1, NB                                                   
C SPARSE BACK SOLVE                                                     
         DO  6 IIB = 1, N                                               
            I = NP1-IIB                                                 
            RI = I                                                      
            IF (ORDER) RI = R(I)                                        
            JMIN = IA(RI)                                               
            JMAX = MIN0(IA(RI+1)-1, IU(I))                              
            SUM = B(I, K)                                               
            IF (JMIN .GT. JMAX) GOTO 5                                  
   1              DO  2 J = JMIN, JMAX                                  
                     JUJ = JA(J)                                        
                     SUM = SUM-A(J)*TMP(JUJ)                            
   2                 CONTINUE                                           
                  IF (JMAX .EQ. IU(I)) GOTO  4                          
                  JMIN = IEX(I)                                         
                  JMAX = IU(I)                                          
   3              IF (JMIN .LE. JMAX) GOTO  1                           
   4        CONTINUE                                                    
   5        IIC = C(I)                                                  
            TMP(IIC) = SUM                                              
   6        CONTINUE                                                    
         DO  7 I = 1, N                                                 
            B(I, K) = TMP(I)                                            
   7        CONTINUE                                                    
   8     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DSPMCE(N, R, C, A, IA, JA, IAMAX, IL, ISIZE, COND,     
     1   Z)                                                             
      INTEGER N                                                         
      INTEGER R(N), C(N), IA(1), JA(1), IAMAX, IL(N)                    
      INTEGER ISIZE                                                     
      DOUBLE PRECISION A(1), COND, Z(N)                                 
      DOUBLE PRECISION D(500)                                           
      COMMON /CSTAK/ D                                                  
      INTEGER ICI, ISTKGT, I, J, JMIN, JMAX                             
      INTEGER IERR, ITMP, IC, II(1000), IC1                             
      DOUBLE PRECISION SUM, GROWTH, SUM1, RSTK(500)                     
      LOGICAL TEMP                                                      
      EQUIVALENCE (D(1), II(1))                                         
      EQUIVALENCE (D(1), RSTK(1))                                       
C                                                                       
C SPARSE CONDITION ESTIMATOR                                            
C                                                                       
C INPUT PARAMETERS                                                      
C                                                                       
C N         NUMBER OF EQUATIONS                                         
C R         INTEGER VECTOR GIVING ROW PERMUTATIONS                      
C IA        INTEGER VECTOR OF LENGTH N+1 POINTING TO BEGINING OF        
C           EACH ROW IN THE A ARRAY                                     
C JA        INTEGER VECTOR GIVING COLUMN INDEX OF EACH ELEMENT          
C           IN THE A ARRAY                                              
C A         DOUBLE PRECISION ARRAY INTO WHICH THE NONZERO ELEMENTS      
C           OF A ARE PACKED BY ROWS                                     
C IAMAX     TOTAL SIZE OF THE A ARRAY,MUST BE LARGE ENOUGH TO           
C           HOLD LU DECOMPOSITION OF A                                  
C OUTPUT PARAMETERS                                                     
C C         REORDERED COLUMNS                                           
C JA        COLUMN INDICES OF LU DECOMPOSITION OF A                     
C A         LU DECOMPOSITION OF A                                       
C IU        INTEGER VECTOR OF LENGTH N+1 STATING WHERE EACH ROW         
C           OF U BEGINS IN THE A AND JA ARRAYS                          
C COND      ESTIMATE OF THE CONDITION NUMBER OF A                       
C Z         DOUBLE PRECISION VECTOR LENGTH N, GIVING APPROXIMATE NULL   
C           VECTOR                                                      
C SPACE ALLOCATED 2N+1 INTEGER LOCATIONS  AND N DOUBLE PRECISION LOCATIO
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(12HDSPCE-N.LT.1, 12, 1, 2)              
C     IF (IAMAX .LT. IA(N+1)-1) CALL SETERR(24HDSPCE-INSUFFICIENT SPACE,
C    1   24, 3, 2)                                                      
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DSPCE-N.LT.1', 12, 1, 2)               
      IF (IAMAX .LT. IA(N+1)-1) CALL SETERR('DSPCE-INSUFFICIENT SPACE', 
     1   24, 3, 2)                                                      
C/                                                                      
      CALL ENTER(1)                                                     
      IC = ISTKGT(2*N+1, 2)                                             
      SUM1 = 0.D0                                                       
      ITMP = IC+N                                                       
      IC1 = IC-1                                                        
C FIND NORM OF MATRIX AND CHECK ROW PERMUTATIONS                        
C SET COLUMN PERMUTATION TO INITIALLY NO COLUMN INTERCHANGES.           
      DO  2 I = 1, N                                                    
         SUM = 0.D0                                                     
         TEMP = R(I) .LT. 1                                             
         IF (.NOT. TEMP) TEMP = R(I) .GT. N                             
C/6S                                                                    
C        IF (TEMP) CALL SETERR(21HDSPCE-R  OUT OF RANGE, 21, 2, 2)      
C/7S                                                                    
         IF (TEMP) CALL SETERR('DSPCE-R  OUT OF RANGE', 21, 2, 2)       
C/                                                                      
         ICI = IC1+I                                                    
         C(I) = I                                                       
         II(ICI) = I                                                    
         JMIN = IA(I)                                                   
         JMAX = IA(I+1)-1                                               
         DO  1 J = JMIN, JMAX                                           
            SUM = SUM+DABS(A(J))                                        
   1        CONTINUE                                                    
         IF (SUM .GT. SUM1) SUM1 = SUM                                  
   2     CONTINUE                                                       
      CALL DS4MLU(N,IA, JA, A, IAMAX, IL, II(ITMP), Z, II(IC), R, C,    
     1   IERR, 1.D0, 0.D0, ISIZE, GROWTH)                               
      IF (IERR .EQ. 0) GOTO 6                                           
         IF (IERR .GT. N+10) GOTO 3                                     
C/6S                                                                    
C           CALL SETERR(15HDSPMCE-NULL ROW, 15, IERR, 2)                
C/7S                                                                    
            CALL SETERR('DSPMCE-NULL ROW', 15, IERR, 2)                 
C/                                                                      
            GOTO  7                                                     
C/6S                                                                    
C  3        IF (IERR .LE. 2*N+10) CALL SETERR(                          
C    1         29HDSPMCE-INCORRECT COLUMN INDEX, 29, IERR, 2)           
C/7S                                                                    
   3        IF (IERR .LE. 2*N+10) CALL SETERR(                          
     1         'DSPMCE-INCORRECT COLUMN INDEX', 29, IERR, 2)            
C/                                                                      
            IF (IERR .LE. 3*N+10) GOTO 4                                
C/6S                                                                    
C              CALL SETERR(25HDSPMCE-INSUFFICIENT SPACE, 25, IERR, 1)   
C/7S                                                                    
               CALL SETERR('DSPMCE-INSUFFICIENT SPACE', 25, IERR, 1)    
C/                                                                      
               GOTO  7                                                  
   4        CONTINUE                                                    
   6  CALL ISTKRL(1)                                                    
      ITMP = ISTKGT(N, 4)                                               
      CALL DS4MCE(N, R, C, A, IA, JA, IL, SUM1, COND, Z, RSTK(ITMP))    
C/6S                                                                    
C     IF (IERR .NE. 0) CALL SETERR(22HDSPMCE-SINGULAR MATRIX, 22, IERR  
C    1   , 1)                                                           
C/7S                                                                    
      IF (IERR .NE. 0) CALL SETERR('DSPMCE-SINGULAR MATRIX', 22, IERR   
     1   , 1)                                                           
C/                                                                      
   7  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DS4MCE(N, R, C, A, IA, JA, IL, ANORM, COND, Z,         
     1   TMP)                                                           
      INTEGER N                                                         
      INTEGER R(N), C(N), IA(1), JA(1), IL(1)                           
      DOUBLE PRECISION A(N), ANORM, COND, Z(N), TMP(N)                  
      INTEGER ICK, IDI, IEX, JUJ, IDI1, MIN0                            
      INTEGER I, J, K, JMIN, JMAX, JMIN1                                
      INTEGER JMAX1, IB, JJ, IR, KR, LASTA                              
      INTEGER ISIZE, NP1                                                
      DOUBLE PRECISION BIG, WKM, SUM, S, T                              
      DOUBLE PRECISION  EK, SM, WK, GREAT                               
      DOUBLE PRECISION DFLOAT, DASUM, YNORM, D1MACH                     
      LOGICAL TEMP                                                      
C THIS IS LOWER LEVEL CONDITION ESTIMATOR FOR SPMCE                     
C PARAMETERS ARE THE SAME AS IN SPMCE EXCEPT THAT                       
C ANORM -NORM OF MATRIX                                                 
C TMP - N VECTOR DOUBLE PRECISION TEMPORARY                             
C A,JA - ALREADY CONTAIN LU DECOMPOSITION COMPUTED BY S4MLU.            
      IF (N .NE. 1) GOTO 1                                              
         COND = 1.0D0                                                   
         Z(1) = 1.0D0                                                   
         GOTO  24                                                       
   1     EK = 1.0D0                                                     
         ISIZE = IA(N+1)-1                                              
         DO  2 J = 1, N                                                 
            Z(J) = 0.0D0                                                
   2        CONTINUE                                                    
         BIG = D1MACH(2)/DFLOAT(N)                                      
C                                                                       
C SOLVE TRANS(U) W =E AND PUT ANSWER IN Z                               
         LASTA = IA(N+1)                                                
         DO  14 K = 1, N                                                
            ICK = C(K)                                                  
            IF (DABS(Z(ICK)) .NE. 0.D0) EK = DSIGN(EK, -Z(ICK))         
            IF (DABS(EK-Z(ICK)) .LE. 1.D0) GOTO 3                       
               S = 1.D0/DABS(EK-Z(ICK))                                 
               CALL DSCAL(N, S, Z, 1)                                   
               EK = S*EK                                                
   3        WK = EK-Z(ICK)                                              
            WKM = (-EK)-Z(ICK)                                          
            S = DABS(WK)                                                
            SM = DABS(WKM)                                              
            KR = R(K)                                                   
C IN LU DECOMPOSITION, THE U PORTION OF THE ROW                         
C COMES BEFOR THE L  PORTION BUT IT COULD                               
C BE SPLIT IN 2.                                                        
            JMIN = IA(KR)                                               
            IDI = IL(K)                                                 
            JMAX = MIN0(IA(KR+1), IDI)-1                                
            IF (JMIN .GT. JMAX) GOTO 13                                 
               JMIN1 = JMIN                                             
               JMAX1 = JMAX                                             
   4              DO  5 J = JMIN, JMAX                                  
                     JUJ = JA(J)                                        
                     SM = SM+DABS(Z(JUJ)+WKM*A(J))                      
                     Z(JUJ) = Z(JUJ)+WK*A(J)                            
                     S = S+DABS(Z(JUJ))                                 
   5                 CONTINUE                                           
                  TEMP = IDI .LT. LASTA                                 
                  IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA                
                  IF (TEMP) GOTO  7                                     
                  JMIN = JA(IDI)                                        
                  JMAX = IDI-1                                          
   6              IF (JMIN .LE. JMAX) GOTO  4                           
   7           IF (S .GE. SM) GOTO 12                                   
                  T = WKM-WK                                            
   8                 DO  9 J = JMIN1, JMAX1                             
                        JUJ = JA(J)                                     
                        Z(JUJ) = Z(JUJ)+T*A(J)                          
   9                    CONTINUE                                        
                     TEMP = IDI .LT. LASTA                              
                     IF (.NOT. TEMP) TEMP = JMIN1 .GE. LASTA            
                     IF (TEMP) GOTO  11                                 
                     JMIN1 = JA(IDI)                                    
                     JMAX1 = IDI-1                                      
  10                 IF (JMIN1 .LE. JMAX1) GOTO  8                      
  11              CONTINUE                                              
  12           CONTINUE                                                 
  13        IF (S .LT. SM) WK = WKM                                     
            Z(ICK) = WK                                                 
  14        CONTINUE                                                    
         S = 1.0D0/DASUM(N, Z, 1)                                       
         CALL DSCAL(N, S, Z, 1)                                         
C                                                                       
C FORM Y=L(TRANSPOSE)*W                                                 
C AND PUT RESULT BACK INTO Z                                            
C                                                                       
         NP1 = N+1                                                      
         DO  21 IB = 1, N                                               
            I = NP1-IB                                                  
            IR = R(I)                                                   
            IDI = IL(I)                                                 
C A(IDI) HAS RECIPROCAL OF DIAGONAL                                     
            IDI1 = IL(I+1)                                              
            JMIN = IDI+1                                                
            IEX = JA(IDI1)-1                                            
            IF (JMIN .EQ. IA(IR+1)) JMIN = JA(IDI)                      
            JMAX = IA(IR+1)-1                                           
            IF (JMAX .LT. JMIN) JMAX = IEX                              
C TO AVOID OVERFLOW SCALE                                               
            SUM = DMAX1(DABS(A(IDI)), DABS(Z(I)))                       
            IF (DABS(Z(I)) .LE. 1.0D0) SUM = DABS(A(IDI)*Z(I))          
            S = 1.0D0                                                   
            IF (SUM .LE. 1.D0) GOTO 15                                  
               S = 1.0D0/SUM                                            
               CALL DSCAL(N, S, Z, 1)                                   
               IF (S .EQ. 0.0D0) Z(I) = 1.0D0                           
  15        IF (S .NE. 0.0D0) Z(I) = Z(I)*A(IDI)                        
            IF (JMIN .GT. JMAX) GOTO 20                                 
               SUM = Z(I)                                               
  16              DO  17 J = JMIN, JMAX                                 
                     JJ = JA(J)                                         
                     Z(JJ) = Z(JJ)+SUM*A(J)                             
  17                 CONTINUE                                           
                  IF (JMAX .EQ. IEX) GOTO  19                           
                  JMIN = JA(IDI)                                        
                  JMAX = IEX                                            
  18              IF (JMIN .LE. JMAX) GOTO  16                          
  19           CONTINUE                                                 
  20        CONTINUE                                                    
  21        CONTINUE                                                    
C                                                                       
C PUT NORM OF Z TO 1.0D0                                                
C                                                                       
         S = 1.D0/DASUM(N, Z, 1)                                        
         YNORM = 1.0D0                                                  
         CALL DSCAL(N, S, Z, 1)                                         
C DO FORWARD AND BACK SOLVE                                             
         CALL DS4MFB(N, R, C, IA, JA, A, IL, Z, TMP, YNORM)             
         S = 1.0D0/DASUM(N, Z, 1)                                       
         YNORM = YNORM*S                                                
         GREAT = D1MACH(2)                                              
         IF (YNORM .GE. 1.0D0) GOTO 23                                  
            TEMP = ANORM .EQ. 0.0D0                                     
            IF (.NOT. TEMP) TEMP = ANORM .GT. YNORM*GREAT               
            IF (.NOT. TEMP) GOTO 22                                     
               COND = GREAT                                             
               RETURN                                                   
  22     CONTINUE                                                       
  23     COND = ANORM/YNORM                                             
  24  RETURN                                                            
      END                                                               
      SUBROUTINE DS4MFB(N, R, C, IA, JA, A, IL, B, Z, YNORM)            
      INTEGER N                                                         
      INTEGER R(N), C(N), IA(1), JA(1), IL(N)                           
      DOUBLE PRECISION A(1), B(N), Z(N), YNORM                          
      INTEGER IIB, IIC, IDI, IEX, JUJ, IDI1                             
      INTEGER MIN0, I, J, JMIN, JMAX, JJ                                
      INTEGER IR, RI, LASTA, NP1                                        
      DOUBLE PRECISION SUM, S, DK                                       
      DOUBLE PRECISION SC, DASUM                                        
      LOGICAL TEMP                                                      
C SPARSE MATRIX SOLUTION                                                
C INPUT                                                                 
C N ORDER OF PROBLEM                                                    
C R ROW PERMUTATION                                                     
C C COLUMN PERMUTATION                                                  
C IA INTEGER VECTOR, LENGTH N+1 POINTING TO BEGINNING OF ROW IN JA AND A
C JA COLUMN INDICES CORRESPONDING TO NONZERO ELEMENTS IN A              
C A  DOUBLE PRECISION VECTOR OF NONZERO ELEMENTS IN LU DECOMPOSTION     
C IL INTEGER VECTOR LENGTH N+1 POINTING TO BEGINNING OF EACH L ROW      
C    IN A AND JA. COMPUTED BY SPMLU                                     
C B RIGHT-HAND SIDE                                                     
C OUTPUT                                                                
C Z   SOLUTION TO PROBLEM                                               
      LASTA = IA(N+1)                                                   
      NP1 = N+1                                                         
C SPARSE FORWARD SOLVE                                                  
      DO  5 I = 1, N                                                    
         IR = R(I)                                                      
         IDI = IL(I)                                                    
         IDI1 = IL(I+1)                                                 
         JMIN = IDI+1                                                   
         IEX = JA(IDI1)-1                                               
         IF (JMIN .EQ. IA(IR+1)) JMIN = JA(IDI)                         
         JMAX = IA(IR+1)-1                                              
         IF (JMAX .LT. JMIN) JMAX = IEX                                 
C DK HAS RECIPROCAL OF THE DIAGONAL                                     
         DK = A(IDI)                                                    
         SUM = B(I)                                                     
   1     IF (JMIN .GT. JMAX) GOTO  3                                    
            DO  2 J = JMIN, JMAX                                        
               JJ = JA(J)                                               
               SUM = SUM+A(J)*B(JJ)                                     
   2           CONTINUE                                                 
            IF (JMAX .EQ. IEX) GOTO  3                                  
            JMIN = JA(IDI)                                              
            JMAX = IEX                                                  
            GOTO  1                                                     
C                                                                       
C SCALE THINGS TO AVOID OVERFLOW                                        
C                                                                       
   3     SC = DMAX1(DABS(SUM), DABS(DK))                                
         IF (DABS(SUM) .LE. 1.0D0) SC = DABS(SUM*DK)                    
         S = 1.0D0                                                      
         IF (SC .LE. 1.0D0) GOTO 4                                      
            S = 1.0D0/SC                                                
            CALL DSCAL(N, S, B, 1)                                      
            SUM = SUM*S                                                 
            IF (S .EQ. 0.0D0) SUM = 1.0D0                               
            YNORM = YNORM*S                                             
   4     IF (S .NE. 0.0D0) SUM = SUM*DK                                 
         B(I) = SUM                                                     
   5     CONTINUE                                                       
      S = 1.0D0/DASUM(N, B, 1)                                          
      IF (S .GT. 1.0D0) GOTO 6                                          
         CALL DSCAL(N, S, B, 1)                                         
         YNORM = S*YNORM                                                
C SPARSE BACK SOLVE                                                     
   6  DO  13 IIB = 1, N                                                 
         I = NP1-IIB                                                    
         RI = R(I)                                                      
         IDI = IL(I)                                                    
         JMIN = IA(RI)                                                  
         JMAX = MIN0(IA(RI+1), IDI)-1                                   
         SUM = B(I)                                                     
         IF (JMIN .GT. JMAX) GOTO 11                                    
   7           DO  8 J = JMIN, JMAX                                     
                  JUJ = JA(J)                                           
                  SUM = SUM-A(J)*Z(JUJ)                                 
   8              CONTINUE                                              
               TEMP = IDI .LT. LASTA                                    
               IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA                   
               IF (TEMP) GOTO  10                                       
               JMIN = JA(IDI)                                           
               JMAX = IDI-1                                             
   9           IF (JMIN .LE. JMAX) GOTO  7                              
  10     CONTINUE                                                       
  11     IIC = C(I)                                                     
         IF (DABS(SUM) .LE. 1.0D0) GOTO 12                              
            S = 1.0D0/DABS(SUM)                                         
            CALL DSCAL(N, S, Z, 1)                                      
            CALL DSCAL(I, S, B, 1)                                      
            YNORM = YNORM*S                                             
            Z(IIC) = DSIGN(1.0D0, SUM)                                  
            SUM = SUM*S                                                 
  12     Z(IIC) = SUM                                                   
  13     CONTINUE                                                       
      DO  14 I = 1, N                                                   
         B(I) = Z(I)                                                    
  14     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DSPFCE(N, MRP, MCP, AROW, IWORK, UL, IAMAX, ISIZE,     
     1   COND, Z)                                                       
      INTEGER N, IAMAX                                                  
      EXTERNAL AROW                                                     
      INTEGER MRP(101), MCP(101), IWORK(IAMAX), ISIZE                   
      DOUBLE PRECISION UL(IAMAX), COND, Z(N)                            
      DOUBLE PRECISION D(500)                                           
      COMMON /CSTAK/ D                                                  
      INTEGER JLU, ISTKGT, I, IERR, LAST, TEMP                          
      INTEGER TEMP1, IC, II(1000), IC1                                  
      DOUBLE PRECISION GROWTH, R(500), ANORM                            
      LOGICAL TEMP2                                                     
      EQUIVALENCE (R(1), II(1))                                         
      EQUIVALENCE (R(1), D(1))                                          
C THIS SUBROUTINE CALLS THE DECOMPOSITION ROUTINE AND DETERMINES        
C AN ESTIMATE OF THE CONDITION NUMBER OF A D. PRECISION SPARSE MATRIX   
C INPUT PARAMETERS                                                      
C    N        ORDER OF THE PROBLEM                                      
C    MRP      N VECTOR GIVING MATRIX ROW PERMUTATIONS                   
C    AROW      USER WRITTEN SUBROUTINE WHICH GIVES THE NONZERO ELEMENTS 
C              OF A SPECIFIED ROW AND THEIR COLUMN INDICES. THE CALLING 
C              SEQUENCE IS AROW(I,ROW,JROW,NUM)                         
C              WHERE I IS THE SPECIFIED ROW, NUM IS THE NUMBER OF       
C              NONZERO ELEMENTS IN THAT RWO, ROW IS A VECTOR OF THESE   
C              ELEMENTS AND JROW IS THE CORRESPONDING COLUMN INDICES.   
C    IAMAX     DECLARED LENGTH OF UL, IWORK SHOULD BE IAMAX+2N+1 LONG.  
C OUTPUT PARAMETERS                                                     
C    MCP      INTEGER N VECTOR CONTAINING COLUMN PERMUTATIONS FOR       
C             STABILITY                                                 
C    IWORK     INTGER VECTOR CONTAINING POINTER INFORMATION FOR LU      
C             DECOMPOSITION                                             
C    UL        NONZERO ELEMENTS OF LU DECOMPOSITION                     
C    ISIZE     ACTUAL NUMBER OF NONZERO ELEMENTS IN DECOMPOSITION       
C    COND     AN ESTIMATE OF THE CONDITION NUMBER OF THE MATRIX A       
C    Z        N VECTOR CONTAINING APPROXIMATE NULL VECTOR               
C EXTRA STORAGE ALLOCATED - N D. PRECISION  AND 2N+1 INTEGER LOCATIONS  
C THE SUBROUTINES DASUM, SPFLU, AND DS4FCE ARE CALLED                   
C ERROR CONDITIONS-                                                     
C    1     N.LT.1       FATAL                                           
C    2    IA.LT.N       FATAL                                           
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HSPFCE-N.LT.1 , 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('SPFCE-N.LT.1 ', 13, 1, 2)              
C/                                                                      
C      COMPUTE NORM OF MATRIX STORED IN COMPACT FORM                    
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HDSPFCE-N.LT.1, 13, 1, 2)             
C     IF (IAMAX .LT. N) CALL SETERR(25HDSPFCE-INSUFFICIENT SPACE, 25, 3,
C    1   2)                                                             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DSPFCE-N.LT.1', 13, 1, 2)              
      IF (IAMAX .LT. N) CALL SETERR('DSPFCE-INSUFFICIENT SPACE', 25, 3, 
     1   2)                                                             
C/                                                                      
      IC = ISTKGT(2*N+1, 2)                                             
      IC1 = IC-1                                                        
      DO  1 I = 1, N                                                    
         TEMP2 = MRP(I) .LT. 1                                          
         IF (.NOT. TEMP2) TEMP2 = MRP(I) .GT. N                         
C/6S                                                                    
C        IF (TEMP2) CALL SETERR(23HDSPFCE-MRP OUT OF RANGE, 23, 2, 2)   
C/7S                                                                    
         IF (TEMP2) CALL SETERR('DSPFCE-MRP OUT OF RANGE', 23, 2, 2)    
C/                                                                      
         MCP(I) = I                                                     
         TEMP1 = I+IC1                                                  
         II(TEMP1) = I                                                  
   1     CONTINUE                                                       
      TEMP = IC+N                                                       
      JLU = 2*N+2                                                       
      CALL DS4FLU(N,IWORK, IWORK(JLU), UL, IAMAX, IWORK(N+2), II(TEMP)  
     1   , Z, II(IC), MRP, MCP, IERR, 1.0D0, 0.0D0, LAST, AROW, GROWTH, 
     1   ANORM)                                                         
      IF (IERR .EQ. 0) GOTO 5                                           
         IF (IERR .GT. N+10) GOTO 2                                     
C/6S                                                                    
C           CALL SETERR(15HDSPFCE-NULL ROW, 15, IERR, 2)                
C/7S                                                                    
            CALL SETERR('DSPFCE-NULL ROW', 15, IERR, 2)                 
C/                                                                      
            GOTO  6                                                     
C/6S                                                                    
C  2        IF (IERR .LE. 2*N+10) CALL SETERR(                          
C    1         29HDSPFCE-INCORRECT COLUMN INDEX, 29, IERR, 2)           
C/7S                                                                    
   2        IF (IERR .LE. 2*N+10) CALL SETERR(                          
     1         'DSPFCE-INCORRECT COLUMN INDEX', 29, IERR, 2)            
C/                                                                      
            IF (IERR .LE. 3*N+10) GOTO 3                                
C/6S                                                                    
C              CALL SETERR(25HDSPFCE-INSUFFICIENT SPACE, 25, IERR, 1)   
C/7S                                                                    
               CALL SETERR('DSPFCE-INSUFFICIENT SPACE', 25, IERR, 1)    
C/                                                                      
               GOTO  6                                                  
   3        CONTINUE                                                    
   5  ISIZE = LAST-1                                                    
      CALL ISTKRL(1)                                                    
      TEMP = ISTKGT(N, 4)                                               
      CALL DS4FCE(N, IWORK, IWORK(JLU), UL, IAMAX, IWORK(N+2), R(       
     1   TEMP), ANORM, COND, Z, MCP)                                    
C/6S                                                                    
C     IF (IERR .NE. 0) CALL SETERR(22HDSPFCE-SINGULAR MATRIX, 22, IERR  
C    1   , 1)                                                           
C/7S                                                                    
      IF (IERR .NE. 0) CALL SETERR('DSPFCE-SINGULAR MATRIX', 22, IERR   
     1   , 1)                                                           
C/                                                                      
   6  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DS4FCE(N, IA, JA, UL, IAMAX, IU, Z, ANORM              
     1   , COND, TMP, MCP)                                              
      INTEGER N, IAMAX                                                  
      INTEGER IA(1), JA(1), IU(N), MCP(N)                               
      DOUBLE PRECISION UL(IAMAX), Z(N), ANORM, COND, TMP(N)             
      INTEGER IIC, ICT, IUI, JUJ, I, J                                  
      INTEGER K, JMIN, JMAX, KB, KC, ID                                 
      INTEGER JJ, KP1, NP1                                              
      DOUBLE PRECISION BIG, WKM, SUM, S, T                              
      DOUBLE PRECISION EK, DK, SC                                       
      DOUBLE PRECISION SM, WK, GREAT, DFLOAT, DASUM, YNORM              
      DOUBLE PRECISION D1MACH                                           
      LOGICAL TEMP                                                      
C THIS SUBROUTINE DETERMINES A LOWER BOUND ON THE CONDITION NUMBER      
C OF THE DECOMPOSED MATRIX A VIA THE ALGORITHM USED IN LINPACK          
C                                                                       
C                                                                       
C SOLVE A(TRANSPOSE)W = E                                               
C WHERE E IS CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH                       
C IN THE COMPONENTS OF W                                                
      NP1 = N+1                                                         
      ICT = IA(N+1)-1                                                   
      IF (N .NE. 1) GOTO 1                                              
         COND = 1.0D0                                                   
         Z(1) = 1.0D0                                                   
         GOTO  26                                                       
   1     EK = 1.0D0                                                     
C                                                                       
C SOLVE U(TRANS) Y =E                                                   
C                                                                       
         BIG = DSQRT(D1MACH(2))/DFLOAT(N)                               
         DO  2 J = 1, N                                                 
            Z(J) = 0.0D0                                                
   2        CONTINUE                                                    
         DO  9 K = 1, N                                                 
            KC = MCP(K)                                                 
C        DIAG=UL(IU(K)-1)                                               
            IF (DABS(Z(KC)) .NE. 0.0D0) EK = DSIGN(EK, -Z(KC))          
            IF (DABS(EK-Z(KC)) .LE. 1.0D0) GOTO 3                       
               S = 1.0D0/DABS(EK-Z(KC))                                 
               CALL DSCAL(N, S, Z, 1)                                   
               EK = S*EK                                                
   3        WK = EK-Z(KC)                                               
            WKM = (-EK)-Z(KC)                                           
            S = DABS(WK)                                                
            SM = DABS(WKM)                                              
            KP1 = K+1                                                   
            IF (KP1 .GT. N) GOTO 8                                      
               JMIN = IU(K)                                             
               JMAX = IA(K+1)-1                                         
               IF (JMAX .LT. JMIN) GOTO 7                               
                  DO  4 J = JMIN, JMAX                                  
                     JUJ = JA(J)                                        
                     SM = SM+DABS(Z(JUJ)+WKM*UL(J))                     
                     Z(JUJ) = Z(JUJ)+UL(J)*WK                           
                     S = S+DABS(Z(JUJ))                                 
   4                 CONTINUE                                           
                  IF (S .GE. SM) GOTO 6                                 
                     T = WKM-WK                                         
                     WK = WKM                                           
                     DO  5 J = JMIN, JMAX                               
                        JUJ = JA(J)                                     
                        Z(JUJ) = Z(JUJ)+T*UL(J)                         
   5                    CONTINUE                                        
   6              CONTINUE                                              
   7           CONTINUE                                                 
   8        Z(KC) = WK                                                  
   9        CONTINUE                                                    
         S = 1.0D0/DASUM(N, Z, 1)                                       
         CALL DSCAL(N, S, Z, 1)                                         
C                                                                       
C SOLVE Y = L(TRANSPOSE) * W                                            
C                                                                       
         DO  13 KB = 1, N                                               
            K = N+1-KB                                                  
            ID = IU(K)-1                                                
C UL(ID) CONTAINS THE RECIPROCAL OF THE DIAGONAL OF U                   
C TRY TO AVOID OVERFLOW                                                 
            SUM = DMAX1(DABS(Z(K)), DABS(UL(ID)))                       
            IF (DABS(Z(K)) .LE. 1.0D0) SUM = DABS(Z(K)*UL(ID))          
            S = 1.0D0                                                   
            JMIN = IA(K)                                                
            JMAX = ID-1                                                 
            IF (SUM .LT. 1.0D0) GOTO 10                                 
               S = 1./SUM                                               
               CALL DSCAL(N, S, Z, 1)                                   
  10        SUM = Z(K)*UL(ID)                                           
            IF (S .EQ. 0.0D0) Z(K) = 1.0D0                              
            IF (JMAX .LT. JMIN) GOTO 12                                 
               DO  11 J = JMIN, JMAX                                    
                  JUJ = JA(J)                                           
                  Z(JUJ) = Z(JUJ)+UL(J)*SUM                             
  11              CONTINUE                                              
  12        Z(K) = SUM                                                  
  13        CONTINUE                                                    
         S = 1.0D0/DASUM(N, Z, 1)                                       
         CALL DSCAL(N, S, Z, 1)                                         
         YNORM = 1.0D0                                                  
C                                                                       
C SOLVE L X =W                                                          
C                                                                       
         DO  17 I = 1, N                                                
            JMIN = IA(I)                                                
            IUI = IU(I)-1                                               
            JMAX = IUI-1                                                
            DK = UL(IUI)                                                
            SUM = Z(I)                                                  
            IF (JMIN .GT. JMAX) GOTO 15                                 
               DO  14 J = JMIN, JMAX                                    
                  JJ = JA(J)                                            
                  SUM = SUM+UL(J)*Z(JJ)                                 
  14              CONTINUE                                              
  15        SC = DMAX1(DABS(SUM), DABS(DK))                             
            IF (DABS(SUM) .LE. 1.0D0) SC = DABS(SUM*DK)                 
            S = 1.0D0                                                   
            IF (SC .LE. 1.0D0) GOTO 16                                  
               S = 1./SC                                                
               CALL DSCAL(N, S, Z, 1)                                   
               YNORM = YNORM*S                                          
               SUM = SUM*S                                              
  16        SUM = SUM*DK                                                
            IF (S .EQ. 0.0D0) SUM = 1.0D0                               
            Z(I) = SUM                                                  
  17        CONTINUE                                                    
         S = 1.0D0/DASUM(N, Z, 1)                                       
         IF (S .GT. 1.0D0) GOTO 18                                      
            CALL DSCAL(N, S, Z, 1)                                      
            YNORM = YNORM*S                                             
C                                                                       
C   SOLVE U * Z = X                                                     
  18     DO  22 KB = 1, N                                               
            K = N+1-KB                                                  
            JMIN = IU(K)                                                
            JMAX = IA(K+1)-1                                            
            SUM = Z(K)                                                  
            IF (JMIN .GT. JMAX) GOTO 20                                 
               DO  19 J = JMIN, JMAX                                    
                  JUJ = JA(J)                                           
                  SUM = SUM-UL(J)*TMP(JUJ)                              
  19              CONTINUE                                              
  20        IIC = MCP(K)                                                
            IF (DABS(SUM) .LE. 1.) GOTO 21                              
               S = 1.0D0/DABS(SUM)                                      
               CALL DSCAL(N, S, TMP, 1)                                 
               CALL DSCAL(K, S, Z, 1)                                   
               YNORM = YNORM*S                                          
               SUM = SUM*S                                              
  21        TMP(IIC) = SUM                                              
  22        CONTINUE                                                    
C    MAKE ZNORM = 1.0D0                                                 
         S = 1.0D0/DASUM(N, TMP, 1)                                     
         CALL DSCAL(N, S, TMP, 1)                                       
         DO  23 I = 1, N                                                
            Z(I) = TMP(I)                                               
  23        CONTINUE                                                    
         YNORM = YNORM*S                                                
C                                                                       
C   SET COND = ESTIMATE OF THE CONDITION NUMBER OF A                    
C                                                                       
         GREAT = D1MACH(2)                                              
         IF (YNORM .GT. 1.0D0) GOTO 25                                  
            TEMP = ANORM .GT. YNORM*GREAT                               
            IF (.NOT. TEMP) TEMP = ANORM .EQ. 0.0D0                     
            IF (.NOT. TEMP) GOTO 24                                     
               COND = GREAT                                             
               RETURN                                                   
  24     CONTINUE                                                       
  25     COND = ANORM/YNORM                                             
  26  RETURN                                                            
      END                                                               
      SUBROUTINE DSPMLU(N, MRP, MCP, IA, JA, A, IAMAX, IL, THRESH       
     1   , EPS, ISIZE, GROWTH)                                          
      INTEGER N                                                         
      INTEGER MRP(N), MCP(N), IA(1), JA(1), IAMAX, IL(N)                
      INTEGER ISIZE                                                     
      DOUBLE PRECISION A(1), THRESH, EPS, GROWTH                        
      DOUBLE PRECISION DSTAK(500)                                       
      COMMON /CSTAK/ DSTAK                                              
      INTEGER ISTKGT, I, K, IERR, LAST, TEMP                            
      INTEGER TEMP1, IC, II(1000), IZ, IC1                              
      DOUBLE PRECISION D(500)                                           
      LOGICAL TEMP2                                                     
      EQUIVALENCE (D(1), II(1), DSTAK(1))                               
C SPARSE DECOMPOSITION                                                  
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C MRP     INTEGER VECTOR GIVING ROW PERMUTATION                         
C MCP     INTEGER VECTOR GIVING COLUMN PERMUTATIONS                     
C IA     INTEGER VECTOR OF LENGTH N+1 GIVING BEGINNING                  
C        OF EACH ROW IN THE A AND JA ARRAYS                             
C JA     INTEGER VECTOR OF LENGTH IAMAX,GIVING COLUMN INDICES           
C        OF EACH ELEMENT IN THE A ARRAY                                 
C A      DOUBLE PRECISION ARRAY OF THE NONZERO ELEMENTS IN              
C        THE COEFFICIENT MATRIX STORED BY ROWS                          
C        DESTROYED ON OUTPUT                                            
C IAMAX  DIMENSION OF THE JA AND A ARRAYS,SHOULD AT                     
C        LEAST BE THE SIZE OF THE NUMBER OF NONZERO                     
C        ELMENTS IN A,PREFERABLY,TWICE AS LARGE                         
C THRESH DOUBLE PRECISION VARIABLE BETWEEN 0 AND 1 GIVING               
C        A THRESHHOLD FOR PIVOTING. IF THRESH IS 0, NO PIVOTING         
C        WILL BE DONE.  IF THRESH=1.D0,THEN GAUSSIAN                    
C        ELIMINATION WITH PARTIAL PIVOTING WILL BE DONE                 
C EPS    TEST FOR SINGULARITY                                           
C OUTPUT PARAMETERS                                                     
C MCP    THE REORDERING DONE TO INSURE STABILITY,AS DICTATED            
C        BY THRESH                                                      
C A       THE LU DECOMPOSITION OF A                                     
C JA      THE COLUMN INDICES OF THE LU DECOMPOSITION                    
C IL     INTEGER VECTOR OF LENGTH N+1 WHICH POINTS TO THE               
C        BEGINNING OF EACH ROW OF L IN JA ARRAY                         
C ISIZE  ACTUAL NUMBER OF ELEMENTS IN THE A ARRAY THAT WAS NEEDED       
C        FOR THE DECOMPOSITION                                          
C GROWTH NUMERICAL ELEMENT GROWTH. IF THIS MUCH .GT. 1, THEN THE        
C        COMPUTED DECOMPOSITION MAY BE THE DECOMPOSITION OF A MATRIX    
C        THAT IS NOT VERY CLOSE TO THE ORIGINAL MATRIX. RAISING         
C        THRESH MIGHT ALLEVIATE THE SITUATION.                          
C SPACE ALLOCATED AND DEALLOCATED-2N+1 INTEGER AND N DOUBLE             
C PRECISION LOCATIONS                                                   
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C 2 R  NOT IN 1 THROUGH N                                               
C 3 C NOT IN 1 THROUGH N                                                
C 4 IAMAX LESS THAN IA(N+1)-1                                           
C 10+K     NULL ROW           FATAL                                     
C 10+N+K   INVALID INDEX IN ROW K FATAL                                 
C 10+3N+K     SINGULAR MATRIX OF RANK K      RECOVERABLE                
C 10+2N+K    RAN OUT OF SPACE WHEN PROCEESING ROW K                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HDSPMLU-N.LT.1, 13, 1, 2)             
C     IF (IAMAX .LT. IA(N+1)-1) CALL SETERR(24H SPLU-INSUFFICIENT SPACE,
C    1   24, 4, 2)                                                      
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DSPMLU-N.LT.1', 13, 1, 2)              
      IF (IAMAX .LT. IA(N+1)-1) CALL SETERR(' SPLU-INSUFFICIENT SPACE', 
     1   24, 4, 2)                                                      
C/                                                                      
      CALL ENTER(1)                                                     
      IZ = ISTKGT(N, 4)                                                 
      IC = ISTKGT(2*N+1, 2)                                             
      IC1 = IC-1                                                        
      DO  1 I = 1, N                                                    
         TEMP2 = MRP(I) .LT. 1                                          
         IF (.NOT. TEMP2) TEMP2 = MRP(I) .GT. N                         
C/6S                                                                    
C        IF (TEMP2) CALL SETERR(23HDSPMLU-MRP OUT OF RANGE, 23, 2, 2)   
C/7S                                                                    
         IF (TEMP2) CALL SETERR('DSPMLU-MRP OUT OF RANGE', 23, 2, 2)    
C/                                                                      
         K = MCP(I)                                                     
         TEMP2 = K .LT. 1                                               
         IF (.NOT. TEMP2) TEMP2 = K .GT. N                              
C/6S                                                                    
C        IF (TEMP2) CALL SETERR(23HDSPMLU-MCP OUT OF RANGE, 23, 3, 2)   
C/7S                                                                    
         IF (TEMP2) CALL SETERR('DSPMLU-MCP OUT OF RANGE', 23, 3, 2)    
C/                                                                      
         TEMP1 = K+IC1                                                  
         II(TEMP1) = I                                                  
   1     CONTINUE                                                       
      TEMP = IC+N                                                       
      CALL DS4MLU(N, IA, JA, A, IAMAX, IL, II(TEMP), D(IZ), II(IC), MRP 
     1   , MCP, IERR, THRESH, EPS, LAST, GROWTH)                        
      IF (IERR .EQ. 0) GOTO 6                                           
         IF (IERR .GT. N+10) GOTO 2                                     
C/6S                                                                    
C           CALL SETERR(15HDSPMLU-NULL ROW, 15, IERR, 2)                
C/7S                                                                    
            CALL SETERR('DSPMLU-NULL ROW', 15, IERR, 2)                 
C/                                                                      
            GOTO  5                                                     
C/6S                                                                    
C  2        IF (IERR .LE. 2*N+10) CALL SETERR(                          
C    1         29HDSPMLU-INCORRECT COLUMN INDEX, 29, IERR, 2)           
C/7S                                                                    
   2        IF (IERR .LE. 2*N+10) CALL SETERR(                          
     1         'DSPMLU-INCORRECT COLUMN INDEX', 29, IERR, 2)            
C/                                                                      
            IF (IERR .LE. 3*N+10) GOTO 3                                
C/6S                                                                    
C              CALL SETERR(25HDSPMLU-INSUFFICIENT SPACE, 25, IERR, 1)   
C/7S                                                                    
               CALL SETERR('DSPMLU-INSUFFICIENT SPACE', 25, IERR, 1)    
C/                                                                      
               CALL LEAVE                                               
               GOTO  4                                                  
C/6S                                                                    
C  3           CALL SETERR(22HDSPMLU-SINGULAR MATRIX, 22, IERR, 1)      
C/7S                                                                    
   3           CALL SETERR('DSPMLU-SINGULAR MATRIX', 22, IERR, 1)       
C/                                                                      
               CALL LEAVE                                               
   4        RETURN                                                      
   5  CONTINUE                                                          
   6  ISIZE = LAST-1                                                    
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DSPMSL(N, MRP, MCP, IA, JA, A, IL, B, IB, NB)          
      INTEGER N, IB, NB                                                 
      INTEGER MRP(N), MCP(N), IA(1), JA(1), IL(N)                       
      DOUBLE PRECISION A(1), B(IB, NB)                                  
      DOUBLE PRECISION D(500)                                           
      COMMON /CSTAK/ D                                                  
      INTEGER ISTKGT, ITMP, II(1)                                       
      EQUIVALENCE (D(1), II(1))                                         
C FORWARD SOLVE FOR SPARSE MATRICES                                     
C INPUT PARAMETERS                                                      
C N     NUMBER OF EQUATIONS                                             
C MRP   INTEGER VECTOR OF ROW PERMUTATIONS                              
C MCP   INTEGER VECTOR OF COLUMNS PERMUTATIONS                          
C IA    INTEGER VECTOR OF LENGTH N+1 GIVING BEGINNING OF                
C       EACH ROW OF DECOMPOSITION IN A NAD JA ARRAYS                    
C JA    INTEGER VECTOR COMPUTED BY  SPLU                                
C A     DOUBLE PRECISION ARRAY COMPUTED BY  SPLU                        
C IL    INTEGER VECTOR OF LENGTH N+1 POINTING TO L IN LU                
C       DECOMPOSITION,COMPUTED BY SPLU,SPDC,OR SPCE                     
C B     MATRIX OF RIGHT HAND SIDES                                      
C IB    ROW DIMENSION OF B MATRIX                                       
C NB    NUMBER OF RIGHT HAND SIDES                                      
C OUTPUT PARAMETERS                                                     
C B     THE SOLUTION MATRIX                                             
C STORAGE SPACE ALLOCATED AND DEALLOCATED -N DOUBLE PRECISION LOCATIONS 
C ERROR CONDITIONS                                                      
C 1 N.LT.1    FATAL                                                     
C 2 IB.LT.N   FATAL                                                     
C 3 NB.LT.1   FATAL                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HDSPMSL-N.LT.1, 13, 1, 2)             
C     IF (IB .LT. N) CALL SETERR(14HDSPMSL-IB.LT.N, 14, 2, 2)           
C     IF (NB .LT. 1) CALL SETERR(14HDSPMSL-NB.LT.1, 14, 3, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DSPMSL-N.LT.1', 13, 1, 2)              
      IF (IB .LT. N) CALL SETERR('DSPMSL-IB.LT.N', 14, 2, 2)            
      IF (NB .LT. 1) CALL SETERR('DSPMSL-NB.LT.1', 14, 3, 2)            
C/                                                                      
      CALL ENTER(1)                                                     
      ITMP = ISTKGT(N, 4)                                               
      CALL DS4MSL(N, MRP, MCP, IA, JA, A, IL, B, IB, NB, D(ITMP))       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DS4MLU(N, IA, JA, A, IAMAX, IL, IROW, Z, IC, R, C,     
     1   IERR, THRESH, EPS, LAST, GROWTH)                               
      INTEGER N                                                         
      INTEGER IA(N), JA(N), IAMAX, IL(N), IROW(N), IC(N)                
      INTEGER R(N), C(N), IERR, LAST                                    
      DOUBLE PRECISION A(N), Z(N), THRESH, EPS, GROWTH                  
      INTEGER ICC, IDI, JUJ, JLU, NUR, NUU                              
      INTEGER MIN0, I, J, K, M, CMAX                                    
      INTEGER JMIN, IMAX, JMAX, CI, CK, NC                              
      INTEGER II, JJ, IJ, RI, RK, JAMIN                                 
      INTEGER JAMAX, NU, ISPAC, LASTA, IUEND, IT1                       
      INTEGER NP1, JDMAX1                                               
      DOUBLE PRECISION AKI, AFT, PVT, BFOR, PMAX                        
      DOUBLE PRECISION DK, D1MACH                                       
      LOGICAL TEMP, SING                                                
C INPUT PARAMETERS                                                      
C N ORDER OF MATRIX                                                     
C IA POINTER TO JA AND A OF BEGINNING OF EACH NEW ROW                   
C    LENGTH N+1                                                         
C JA COLUMN INDICES OF NONZERONELEMENTS OF A                            
C A NONZERO ELEMENTS OF A                                               
C IAMAX - DIMENSION OF A AND JA ARRAYS                                  
C R- INTEGER VECTOR LENGTH N OF ROW PERMUTAIONS                         
C C - INTEGER VECTOR LENGTH N OF COLUMN PERMUTATIONS                    
C IC - INTEGER VECTOR OF LENGTH N OF INCERSE COLUMN PERMUTAIONS         
C     IC(C(I))=I                                                        
C THRESH - THRESHHOLD PIVOTING PARAMETER BETWEEN 0.0 AND 1.0.           
C          IF 1.0, PARTIAL PIVOTING WILL BE PEERFORMED                  
C EPS - SINGULARITY CRITERIA.                                           
C SCRATCH VECTORS                                                       
C Z ,DOUBLE PRECISION, LENGTH N                                         
C IROW, INTEGER LENGTH N+1                                              
C OUTPUT                                                                
C JA,A NONZERO ELEMENTS AND CORRESPONDING COLUMN INDICES OF LU          
C       DECOMPOSITION                                                   
C C,IC - COLUMN PERMUTAIONS AFTER THRESHHOLD PIVOTING                   
C IERR - ERROR CRITERIA- IF NONZERO WORRY                               
C IF  .LE. N+10 NULL ROW AT IERR-10                                     
C IF N+11 .LE. IERR .LE. 2N+10 INVALID INDEX AT ROW IERR-N-10           
C IF 2N+11 .LE. IERR .LE. 3N+10  SINGULAR MATRIX OF RANK IERR-2N-10     
C IF 3N+11 .LE. IERR .LE.  RAN OUT OF STORAGE AT ROW IERR-3N-10         
C LAST- NUMBER OF ELEMENTS IN A AND JA USED                             
C GROWTH- ELEMENT GROWTH                                                
C IL- INTEGER VECTOR LENGTH N+1, POINTING TO BEGINNINGS OF EACH ROW OF  
C     L IN LU DECOMPOSITION                                             
C INITIALIZATION                                                        
      LAST = IA(N+1)                                                    
      BFOR = 0.0D0                                                      
      IT1 = 0                                                           
      AFT = 0.0D0                                                       
      LASTA = LAST                                                      
      NP1 = N+1                                                         
      IERR = 0                                                          
      DO  1 I = 1, N                                                    
         IROW(I) = 0                                                    
   1     CONTINUE                                                       
C DETERMINE NEXT ROW OF L AND U                                         
      DO  28 K = 1, N                                                   
         SING=.FALSE.                                                   
         RK = R(K)                                                      
         M = NP1                                                        
         JAMIN = IA(RK)                                                 
         IROW(NP1) = NP1                                                
         IUEND = NP1                                                    
         JDMAX1 = IA(RK+1)                                              
         NC = C(K)                                                      
         Z(NC) = 0.D0                                                   
         JAMAX = JDMAX1-1                                               
C CHECK FOR NULL ROW                                                    
         IF (JAMIN .GT. JAMAX) GOTO  29                                 
         DO  6 J = JAMIN, JAMAX                                         
            JJ = JA(J)                                                  
C CHECK FOR VALID COLUMN INDEX                                          
            TEMP = JJ .LE. 0                                            
            IF (.NOT. TEMP) TEMP = JJ .GT. N                            
            IF (TEMP) GOTO  32                                          
            Z(JJ) = A(J)                                                
            BFOR = DMAX1(BFOR, DABS(Z(JJ)))                             
            ICC = IC(JJ)                                                
C NEW ELEMENT IS IN U PART SO JUST ADD TO LINKED LIST                   
            IF (ICC .LT. K) GOTO 2                                      
               IROW(ICC) = IUEND                                        
               IUEND = ICC                                              
               GOTO  5                                                  
   2           IF (M .GT. ICC) M = NP1                                  
C NEW ELEMENT IS IN L PART OF THE ROW                                   
   3              II = IROW(M)                                          
                  IF (II .GE. ICC) GOTO  4                              
                  M = II                                                
                  GOTO  3                                               
   4           IROW(M) = ICC                                            
               IROW(ICC) = II                                           
   5        CONTINUE                                                    
   6        CONTINUE                                                    
C                                                                       
C ELIMINATE ROW                                                         
         NUR = 0                                                        
         I = NP1                                                        
   7        I = IROW(I)                                                 
            IF (I .EQ. NP1) GOTO  18                                    
C DETERMINE WHICH ROW IN PERMUTATION                                    
            RI = R(I)                                                   
            JMIN = IA(RI)                                               
            IDI = IL(I)                                                 
C RELEVANT ROW MAY BE IN TWO SECTIONS, THUS EITHER                      
C ELIMINATE UNTIL U IS FINISHED OR SECTION IS FINISHED                  
            JMAX = MIN0(IA(RI+1), IDI)-1                                
            CI = C(I)                                                   
            AKI = -Z(CI)                                                
            AFT = DMAX1(AFT, DABS(AKI))                                 
            NUR = NUR+1                                                 
            IF (JMAX .LT. JMIN) GOTO 17                                 
   8              DO  14 J = JMIN, JMAX                                 
C ELIMINATE ITH ELEMENT                                                 
                     JUJ = JA(J)                                        
                     ICC = IC(JUJ)                                      
                     IF (IROW(ICC) .NE. 0) GOTO 13                      
                        IF (ICC .LT. K) GOTO 9                          
                           IROW(ICC) = IUEND                            
C FILL IN-SEE IF IT IS IN U OR L                                        
                           IUEND = ICC                                  
                           GOTO  12                                     
   9                       TEMP = M .GT. ICC                            
                           IF (.NOT. TEMP) TEMP = M .LT. I              
                           IF (TEMP) M = I                              
C FILL-IN IS IN L SO FIND ITS ORDERING PLACE                            
  10                          IJ = IROW(M)                              
                              IF (IJ .GE. ICC) GOTO  11                 
                              M = IJ                                    
                              GOTO  10                                  
  11                       IROW(M) = ICC                                
                           IROW(ICC) = IJ                               
  12                    Z(JUJ) = 0.D0                                   
  13                 Z(JUJ) = Z(JUJ)+AKI*A(J)                           
  14                 CONTINUE                                           
C TEST IF THERE IS A SECOND SEGMENT                                     
                  TEMP = IDI .LT. LASTA                                 
                  IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA                
                  IF (TEMP) GOTO  16                                    
                  JMIN = JA(IDI)                                        
                  JMAX = IDI-1                                          
  15              IF (JMIN .LE. JMAX) GOTO  8                           
  16        CONTINUE                                                    
  17        CONTINUE                                                    
            GOTO  7                                                     
  18     I = IUEND                                                      
         PMAX = 0.D0                                                    
         NU = 0                                                         
  19     IF (I .EQ. NP1) GOTO  21                                       
            CI = C(I)                                                   
            PVT = DABS(Z(CI))                                           
            NU = NU+1                                                   
            IF (PVT .LE. PMAX) GOTO 20                                  
               IMAX = I                                                 
               PMAX = PVT                                               
  20        I = IROW(I)                                                 
            GOTO  19                                                    
C DO THRESHHOLD PIVOTING                                                
  21     IF (DABS(Z(NC)) .GE. THRESH*PMAX) IMAX = K                     
         CMAX = C(IMAX)                                                 
C TEST FOR SINGULARITY                                                  
         IF (DABS(Z(CMAX)) .GT. EPS) GOTO  30                           
             IF (IERR.EQ.0)IERR=3*N+K+10                                
             SING=.TRUE.                                                
 30      CONTINUE                                                       
         NUR = NUR+NU                                                   
         ISPAC = JAMAX-JAMIN+1                                          
C TEST IS THERE IS ENOUGH SPACE                                         
         IF (NUR-ISPAC+LAST .GE. IAMAX) GOTO  31                        
         NUU = JAMIN+NU-1                                               
C DETERMINE IS FILLIN REQUIRES SECOND SEGMENT FOR U                     
         IF (NUU .GT. JAMAX) NUU = LAST+NU-ISPAC-1                      
         IL(K) = NUU                                                    
         JLU = NUU+1                                                    
         IF (NUU .EQ. JAMAX) JLU = LAST                                 
         JA(NUU) = LAST                                                 
         AFT = DMAX1(AFT, PMAX)                                         
         IF(.NOT.SING)DK = 1.D0/Z(CMAX)                                 
         IF (SING)DK=D1MACH(2)                                          
         I = IUEND                                                      
C STORE DIAGONAL ELEMENT                                                
         A(NUU) = DK                                                    
  22     IF (I .EQ. NP1) GOTO  24                                       
            IF (I .EQ. IMAX) GOTO 23                                    
               CI = C(I)                                                
C STORE ELEMENT OF U                                                    
               IF (NUU .EQ. LAST) NUU = JDMAX1                          
               NUU = NUU-1                                              
               A(NUU) = Z(CI)*DK                                        
               JA(NUU) = CI                                             
  23        II = I                                                      
            I = IROW(I)                                                 
            IROW(II) = 0                                                
            GOTO  22                                                    
C STORE ELEMENTS OF L                                                   
  24     I = NP1                                                        
  25        J = I                                                       
            I = IROW(I)                                                 
            IROW(J) = 0                                                 
            IF (I .EQ. NP1) GOTO  26                                    
            JA(JLU) = I                                                 
            CI = C(I)                                                   
            A(JLU) = -Z(CI)                                             
            JLU = JLU+1                                                 
            IF (JLU .EQ. JDMAX1) JLU = LAST                             
            GOTO  25                                                    
  26     IF (JLU .GT. LAST) LAST = JLU                                  
C INTERCHANGE THE COLUMN INDICES                                        
         IF (K .EQ. IMAX) GOTO 27                                       
            JJ = C(K)                                                   
            C(K) = CMAX                                                 
            C(IMAX) = JJ                                                
            CK = C(K)                                                   
            IC(CK) = K                                                  
            IC(JJ) = IMAX                                               
  27     CONTINUE                                                       
  28     CONTINUE                                                       
      GROWTH = 1.0D0                                                    
      IF (BFOR.NE.0.0D0)GROWTH = DMAX1(1.0D0, AFT/BFOR)                 
      IL(N+1) = LAST                                                    
      JA(LAST) = LAST                                                   
      RETURN                                                            
  29  IERR = K+10                                                       
      RETURN                                                            
  31  IERR = 2*N+K+10                                                   
      RETURN                                                            
  32  IERR = N+K+10                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE DS4MSL(N, R, C, IA, JA, A, IL, B, IB, NB, TMP)         
      INTEGER N, IB, NB                                                 
      INTEGER R(N), C(N), IA(1), JA(1), IL(N)                           
      DOUBLE PRECISION A(1), B(IB, NB), TMP(N)                          
      INTEGER IIB, IIC, IDI, IEX, JUJ, IDI1                             
      INTEGER MIN0, I, J, K, JMIN, JMAX                                 
      INTEGER JJ, IR, RI, LASTA, NP1                                    
      DOUBLE PRECISION SUM, DK                                          
      LOGICAL TEMP                                                      
C SPARSE MATRIX SOLUTION                                                
C INPUT                                                                 
C N ORDER OF PROBLEM                                                    
C R ROW PERMUTATION                                                     
C C COLUMN PERMUTATION                                                  
C IA INTEGER VECTOR, LENGTH N+1 POINTING TO BEGINNING OF ROW IN JA AND A
C JA COLUMN INDICES CORRESPONDING TO NONZERO ELEMENTS IN A              
C A  DOUBLE PRECISION VECTOR OF NONZERO ELEMENTS IN LU DECOMPOSTION     
C IL INTEGER VECTOR LENGTH N+1 POINTING TO BEGINNING OF EACH L ROW      
C    IN A AND JA. COMPUTED BY DSPMLU                                    
C B RIGHT-HAND SIDE                                                     
C IB ROW DIMENSION OF B                                                 
C NB NUMBER OF RIGHT HAND SIDES                                         
C SCRATCH VECTOR -TMP DOUBLE PRECISION VECTOR LENGTH N                  
C OUTPUT                                                                
C B   SOLUTION TO PROBLEM                                               
C THIS SUBROUTINE ASSUME EACH ROW CAN BE AT MOST TWO SEGMENTS           
C IL(I) POINTS TO DIAGONAL DOUBLE PRECISIONLY AND JA(IL(I)) INDICATES   
C WHERE EXTRA SPACE IS NEEDED FOR THAT ROW. EACH ROW                    
C LOOKS LIKE U,DIAGONAL,L                                               
      LASTA = IA(N+1)                                                   
      NP1 = N+1                                                         
      DO  11 K = 1, NB                                                  
C SPARSE FORWARD SOLVE                                                  
         DO  4 I = 1, N                                                 
            IR = R(I)                                                   
            IDI = IL(I)                                                 
            IDI1 = IL(I+1)                                              
            JMIN = IDI+1                                                
C DETERMINE WHERE FIRST PART OF L IS - IN FIRST OR SECOND               
C SEGMENT                                                               
            IEX = JA(IDI1)-1                                            
            IF (JMIN .EQ. IA(IR+1)) JMIN = JA(IDI)                      
            JMAX = IA(IR+1)-1                                           
            IF (JMAX .LT. JMIN) JMAX = IEX                              
            DK = A(IDI)                                                 
            SUM = B(IR, K)                                              
   1        IF (JMIN .GT. JMAX) GOTO  3                                 
C IS THERE ANY PART OF L IN EXTRA SEGMENT                               
               DO  2 J = JMIN, JMAX                                     
                  JJ = JA(J)                                            
                  SUM = SUM+A(J)*TMP(JJ)                                
   2              CONTINUE                                              
               IF (JMAX .EQ. IEX) GOTO  3                               
               JMIN = JA(IDI)                                           
               JMAX = IEX                                               
               GOTO  1                                                  
   3        TMP(I) = SUM*DK                                             
   4        CONTINUE                                                    
C SPARSE BACK SOLVE                                                     
         DO  10 IIB = 1, N                                              
            I = NP1-IIB                                                 
            RI = R(I)                                                   
            IDI = IL(I)                                                 
            JMIN = IA(RI)                                               
C GO UNTIL EITHER END OF U IS FOUND OR REACH END                        
C OF FIRST SEGMENT                                                      
            JMAX = MIN0(IA(RI+1), IDI)-1                                
            SUM = TMP(I)                                                
            IF (JMIN .GT. JMAX) GOTO 9                                  
   5              DO  6 J = JMIN, JMAX                                  
                     JUJ = JA(J)                                        
                     SUM = SUM-A(J)*B(JUJ, K)                           
   6                 CONTINUE                                           
C CHECK IF THERE IS ANY OF U IN SECOND SEGMENT                          
                  TEMP = IDI .LT. LASTA                                 
                  IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA                
                  IF (TEMP) GOTO  8                                     
                  JMIN = JA(IDI)                                        
                  JMAX = IDI-1                                          
   7              IF (JMIN .LE. JMAX) GOTO  5                           
   8        CONTINUE                                                    
   9        IIC = C(I)                                                  
            B(IIC, K) = SUM                                             
  10        CONTINUE                                                    
  11     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DSPFLU(N, MRP, MCP, AROW, IWORK, UL, IAMAX,            
     1   THRESH, EPS, ISIZE, GROWTH)                                    
      INTEGER N, IAMAX                                                  
      EXTERNAL AROW                                                     
      INTEGER MRP(N), MCP(N), IWORK(IAMAX), ISIZE                       
      DOUBLE PRECISION UL(N), THRESH, EPS, GROWTH                       
      DOUBLE PRECISION DSTAK(500)                                       
      COMMON /CSTAK/ DSTAK                                              
      INTEGER JLU, ISTKGT, I, K, IERR, LAST                             
      INTEGER TEMP, TEMP1, IC, II(1), IZ, IC1                           
      DOUBLE PRECISION D(500), ANORM                                    
      LOGICAL TEMP2                                                     
      EQUIVALENCE (D(1), II(1), DSTAK(1))                               
C SPARSE DECOMPOSITION                                                  
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C MRP     INTEGER VECTOR GIVING ROW PERMUTATION                         
C MCP     INTEGER VECTOR GIVING COLUMN PERMUTATIONS                     
C AROW    SUBROUTINE OF THE FORM AROW(I,ROW,JCOL,NUM) WHICH             
C         FOR A GIVEN INPUT I RETURNS THE NONZERO ELEMENTS OF           
C         THE ITH ROW OF THE MATRIX A IN THE                            
C         DOUBLE PRECISION VECTOR ROW AND THE CORRESPONDING INDICES IN  
C         JCOL. THE VARIABLE NUM RETURNS THE NUMBER OF NONZERO          
C         ELEMENTS IN THE ITH ROW. AROW SHOULD BE DECLARED              
C         EXTERNAL IN THE CALLING PROGRAM.                              
C IAMAX  DIMENSION OF THE UL ARRAY,SHOULD AT                            
C        LEAST BE THE SIZE OF  THE NUMBER OF NONZERO                    
C        ELMENTS IN A,PREFERABLY,TWICE AS LARGE                         
C THRESH DOUBLE PRECISION VARIABLE BETWEEN 0 AND 1 GIVING               
C        A THRESHHOLD FOR PIVOTING. IF THRESH IS 0, NO PIVOTING         
C        WILL BE DONE.  IF THRESH=1.D0,THEN GAUSSIAN                    
C        ELIMINATION WITH PARTIAL PIVOTING WILL BE DONE                 
C EPS    TEST FOR SINGULARITY                                           
C OUTPUT PARAMETERS                                                     
C C      THE REORDERING DONE TO INSURE STABILITY,AS DICTATED            
C        BY THRESH                                                      
C IWORK   INTEGER VECTOR ,LENGTH 2N+1+IAMAX WHOSE FIRST N+1 COMOPONENTS 
C         POINT TO THE BEGINNING OF TH ROWS OF L IN THE UL              
C         AND WHOSE NEXT N COMPONENTS POINT TO THE BEGINNING            
C         OF THE ROWS OF U IN THE UL. COMPONENTS 2N+2 UNTIL             
C         2N+1+ISIZE GIVE THE COLUMN INDICES OF THE COMPONENTS          
C         OF UL                                                         
C UL      THE LU DECOMPOSITION OF THE SPARSE MATRIX A                   
C ISIZE  ACTUAL NUMBER OF ELEMENTS IN THE A ARRAY THAT WAS NEEDED       
C        FOR THE DECOMPOSITION                                          
C GROWTH NUMERICAL ELEMENT GROWTH. IF GROWTH MUCH .GT. 1,WORRY          
C SPACE ALLOCATED AND DEALLOCATED-2N+1 INTEGER AND N DOUBLE             
C PRECISION LOCATIONS                                                   
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C 2 R AND C NOT IN 1 THROUGH N                                          
C 3 IAMAX LESS THAN N                                                   
C 10+K     NULL ROW           FATAL                                     
C 3N+K+10     SINGULAR MATRIX OF RANK K      RECOVERABLE                
C 2N+K+10    RAN OUT OF SPACE WHEN PROCEESING ROW K                     
C N+K+10   INVALID COLUMN INDEX WHEN PROCESSING ROW K                   
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HDSPFLU-N.LT.1, 13, 1, 2)             
C     IF (IAMAX .LT. N) CALL SETERR(25HDSPFLU-INSUFFICIENT SPACE, 25, 3,
C    1   2)                                                             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DSPFLU-N.LT.1', 13, 1, 2)              
      IF (IAMAX .LT. N) CALL SETERR('DSPFLU-INSUFFICIENT SPACE', 25, 3, 
     1   2)                                                             
C/                                                                      
      CALL ENTER(1)                                                     
      IZ = ISTKGT(N, 4)                                                 
      IC = ISTKGT(2*N+1, 2)                                             
      IC1 = IC-1                                                        
      DO  1 I = 1, N                                                    
         TEMP2 = MRP(I) .LT. 1                                          
         IF (.NOT. TEMP2) TEMP2 = MRP(I) .GT. N                         
C/6S                                                                    
C        IF (TEMP2) CALL SETERR(23HDSPFLU-MRP OUT OF RANGE, 23, 2, 2)   
C/7S                                                                    
         IF (TEMP2) CALL SETERR('DSPFLU-MRP OUT OF RANGE', 23, 2, 2)    
C/                                                                      
         K = MCP(I)                                                     
         TEMP2 = K .LT. 1                                               
         IF (.NOT. TEMP2) TEMP2 = K .GT. N                              
C/6S                                                                    
C        IF (TEMP2) CALL SETERR(23HDSPFLU-MCP OUT OF RANGE, 23, 2, 2)   
C/7S                                                                    
         IF (TEMP2) CALL SETERR('DSPFLU-MCP OUT OF RANGE', 23, 2, 2)    
C/                                                                      
         TEMP1 = K+IC1                                                  
         II(TEMP1) = I                                                  
   1     CONTINUE                                                       
      TEMP = IC+N                                                       
      JLU = 2*N+2                                                       
      CALL DS4FLU(N,IWORK, IWORK(JLU), UL, IAMAX, IWORK(N+2), II(TEMP)  
     1   , D(IZ), II(IC), MRP, MCP, IERR, THRESH, EPS, LAST, AROW,      
     1   GROWTH, ANORM)                                                 
      IF (IERR .EQ. 0) GOTO 8                                           
         IF (IERR .GT. N+10) GOTO 2                                     
C/6S                                                                    
C           CALL SETERR(15HDSPFLU-NULL ROW, 15, IERR, 2)                
C/7S                                                                    
            CALL SETERR('DSPFLU-NULL ROW', 15, IERR, 2)                 
C/                                                                      
            GOTO  7                                                     
   2        IF (IERR .LE. 3*N+10) GOTO 3                                
C/6S                                                                    
C              CALL SETERR(22HDSPFLU-SINGULAR MATRIX, 22, IERR, 1)      
C/7S                                                                    
               CALL SETERR('DSPFLU-SINGULAR MATRIX', 22, IERR, 1)       
C/                                                                      
               GOTO  6                                                  
   3           IF (IERR .GT. 2*N+10) GOTO 4                             
C/6S                                                                    
C                 CALL SETERR(29HDSPFLU-INCORRECT COLUMN INDEX, 29,     
C    1               IERR, 2)                                           
C/7S                                                                    
                  CALL SETERR('DSPFLU-INCORRECT COLUMN INDEX', 29,      
     1               IERR, 2)                                           
C/                                                                      
                  GOTO  5                                               
C/6S                                                                    
C  4              CALL SETERR(25HDSPFLU-INSUFFICIENT SPACE, 25, IERR+N  
C    1               , 1)                                               
C/7S                                                                    
   4              CALL SETERR('DSPFLU-INSUFFICIENT SPACE', 25, IERR+N   
     1               , 1)                                               
C/                                                                      
   5        CONTINUE                                                    
   6     CONTINUE                                                       
   7  CONTINUE                                                          
   8  ISIZE = LAST-1                                                    
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DSPFSL(N, MRP, MCP, IWORK, UL, B, IB, NB)              
      INTEGER N, IB, NB                                                 
      INTEGER MRP(N), MCP(N), IWORK(1)                                  
      DOUBLE PRECISION UL(1), B(IB, NB)                                 
      DOUBLE PRECISION D(500)                                           
      COMMON /CSTAK/ D                                                  
      INTEGER JLU, ISTKGT, ITMP                                         
C FORWARD SOLVE FOR SPARSE MATRICES                                     
C INPUT PARAMETERS                                                      
C N     NUMBER OF EQUATIONS                                             
C MRP   INTEGER VECTOR OF ROW PERMUTATIONS                              
C MCP   INTEGER VECTOR,LENGTHN OR COLUMN PERMUTATIONS                   
C IWORK INTEGER VECTOR COMPUTED BY  DSPFLU                              
C UL    DOUBLE PRECISION ARRAY COMPUTED BY  DSPFLU                      
C IU    INTEGER VECTOR OF LENGTH N POINTING TO U IN LU                  
C       DECOMPOSITION,COMPUTED BY DSPFLU OR DSPFCE                      
C B     MATRIX OF RIGHT HAND SIDES                                      
C IB    ROW DIMENSION OF B MATRIX                                       
C NB    NUMBER OF RIGHT HAND SIDES                                      
C OUTPUT PARAMETERS                                                     
C B     THE SOLUTION MATRIX                                             
C STORAGE SPACE ALLOCATED AND DEALLOCATED -N DOUBLE PRECISION LOCATIONS 
C ERROR CONDITIONS                                                      
C 1 N.LT.1    FATAL                                                     
C 2 IB.LT.N   FATAL                                                     
C 3 NB.LT.1   FATAL                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HDSPFSL-N.LT.1, 13, 1, 2)             
C     IF (IB .LT. N) CALL SETERR(14HDSPFSL-IB.LT.N, 14, 2, 2)           
C     IF (NB .LT. 1) CALL SETERR(14HDSPFSL-NB.LT.1, 14, 3, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DSPFSL-N.LT.1', 13, 1, 2)              
      IF (IB .LT. N) CALL SETERR('DSPFSL-IB.LT.N', 14, 2, 2)            
      IF (NB .LT. 1) CALL SETERR('DSPFSL-NB.LT.1', 14, 3, 2)            
C/                                                                      
      CALL ENTER(1)                                                     
      ITMP = ISTKGT(N, 4)                                               
      JLU = 2*N+2                                                       
      CALL DS4FSL(N,MRP, MCP, IWORK, IWORK(JLU), UL, IWORK(N+2), B, IB  
     1   , NB, D(ITMP))                                                 
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DS4FLU(N,IA, JA, A, IAMAX, IU, IROW, Z, IC, R, C,      
     1   IERR, THRESH, EPS, JLU, GETA, GROWTH, BEFOR)                   
      INTEGER N                                                         
      EXTERNAL GETA                                                     
      INTEGER IA(1), JA(1), IAMAX, IU(N), IROW(N), IC(N)                
      INTEGER R(N), C(N), IERR, JLU                                     
      DOUBLE PRECISION A(1), Z(1), THRESH, EPS, GROWTH, BEFOR           
      INTEGER ICC, JUJ, NUM, I, J, K                                    
      INTEGER M, JMIN, IMAX, JMAX, CK, II                               
      INTEGER JJ, IJ, RI, RK, NU, JAMAX                                 
      INTEGER IUEND, NP1, NP2                                           
      DOUBLE PRECISION AKI, AFT, PVT, BEFO, PMAX                        
      DOUBLE PRECISION DK, D1MACH                                       
      LOGICAL TEMP                                                      
C THIS IS LOWER LEVEL LU DECOMPOSITION WITH THRESHHOLD PIVOTING         
C WITH FUNCTION INPUT                                                   
C INPUT PARAMETERS                                                      
C N INTEGER NUMBER OF ROWS                                              
C IAMAX DECLARED SIZE OF THE A ARRAY.                                   
C IC INTEGER N-VECTOR GIVING INVERSE COLUMN PERMUTATION                 
C R INTEGER N-VECTOR GIVING ROW PERMUTATION                             
C C INTEGER N-VECTOR GIVING COLUMN PERMUTATION                          
C THRESH DOUBLE PRECISION SCALAR GIVING THRESHHOLD FOR PIVOTING         
C EPS   DOUBLE PRECISION SCALAR GIVING THREHHOLD FOR SINGULARITY        
C GETA  SUBROUTINE WHICH RETURNS THE NONZERO ELEMENTS AND               
C  THEIR CORRESPONDING COLUMN INDICES WHEN ASKED FOR A                  
C  GIVEN ROW                                                            
C OUTPUT PARAMETERS                                                     
C IA INTEGER N+1 VECTOR GIVING BEGINNING OF ITH ROW                     
C    OF L IN SPARSE LU DECOMPOSITION IN JA AND A                        
C JA COLUMN INDICES OF LU DECOMPOSITION                                 
C A LU DECOMPOSITION                                                    
C IU INTEGER N VECTOR GIVING BEGINNING OF U ROWS IN IA AND JA           
C IC INVERSE COLUMN PERMUTATION AFTER PIUVOTING FOR STABILITY           
C C  COLUMN PERMUTATION AFTER PIVOTING FOR STABILITY                    
C IERR- ERROR FLAG                                                      
C     10 .LT. IERR .LT. 11+N   NULL ROW                                 
C     10+N .LT. IERR .LT. 11+2N   INVALID COLUMN INDEX                  
C     10+2N .LT. IERR .LT. 11+3N  NOT ENOUGH SPACE                      
C     10+3N .LT. IERR .LT. 11+4N   SINGULAR MATRIX OF RANK IERR-3N-10   
C JLU INTEGER SCALAR GIVING SIZE OF DECOMPOSITION                       
C GROWTH SCALAR GIVING ELEMENT GROWTH                                   
C BEFOR SCALR GIVING NORM OF ORIGINAL MATRIX                            
C SCRATCH SPACE                                                         
C IROW -INTEGER LENGTH N+1                                              
C Z-DOUBLE PRECISION LENGTH N                                           
C INITIALIZATION                                                        
      IA(1) = 1                                                         
      BEFOR = 0.0D0                                                     
      AFT = 0.0D0                                                       
      NP1 = N+1                                                         
      IERR = 0                                                          
      NP2 = N+2                                                         
      JLU = 1                                                           
      DO  1 I = 1, N                                                    
         IROW(I) = 0                                                    
   1     CONTINUE                                                       
C DETERMINE NEXT ROW OF L AND U                                         
      DO  23 K = 1, N                                                   
         RK = R(K)                                                      
         M = NP1                                                        
C CALL GETA TO GET ROW RK                                               
         CALL GETA(RK, A(JLU), JA(JLU), NUM)                            
         Z(K) = 0.D0                                                    
         JAMAX = JLU+NUM-1                                              
C CHECK IF ENOUGH SPACE                                                 
         IF (JAMAX .GT. IAMAX) GOTO  24                                 
         IROW(NP1) = NP1                                                
         IUEND = NP2                                                    
         BEFO = 0.D0                                                    
C CHECK FOR NULL ROW                                                    
         IF (NUM .LE. 0) GOTO  25                                       
         DO  6 J = JLU, JAMAX                                           
            JJ = JA(J)                                                  
C CHECK FOR INVLAID INDEX                                               
            TEMP = JJ .LE. 0                                            
            IF (.NOT. TEMP) TEMP = JJ .GT. N                            
            IF (TEMP) GOTO  29                                          
C GET INVERSE COLUMN INDEX                                              
            ICC = IC(JJ)                                                
            Z(ICC) = A(J)                                               
            BEFO = BEFO+DABS(A(J))                                      
C CHECK IF IN L OR U PORTION OF ROW                                     
            IF (ICC .LT. K) GOTO 2                                      
               IROW(ICC) = IUEND                                        
C IF IT IS U PORTION-SO JUST ADD TO LINKED LIST                         
               IUEND = ICC                                              
               GOTO  5                                                  
   2           IF (M .GT. ICC) M = NP1                                  
C NEW ELEMENT IS IN L PART OF THE ROW                                   
C THUS ONE MUST INSERT IN ORDERED LIST                                  
   3              II = IROW(M)                                          
                  IF (II .GE. ICC) GOTO  4                              
                  M = II                                                
                  GOTO  3                                               
   4           IROW(M) = ICC                                            
               IROW(ICC) = II                                           
   5        CONTINUE                                                    
   6        CONTINUE                                                    
         BEFOR = DMAX1(BEFO, BEFOR)                                     
C                                                                       
C ELIMINATE ROW                                                         
         I = NP1                                                        
   7        J = I                                                       
            I = IROW(I)                                                 
            IROW(J) = 0                                                 
            IF (I .EQ. NP1) GOTO  15                                    
            JMIN = IU(I)                                                
            RI = R(I)                                                   
            JMAX = IA(I+1)-1                                            
C AKI WILL HAVE MULTIPLIER                                              
            AKI = -Z(I)                                                 
            AFT = DMAX1(AFT, DABS(AKI))                                 
C CHECK IF SUFFICIENT SPACE                                             
C TO STORE ELEMENT OF L                                                 
            IF (JLU .GT. IAMAX) GOTO  26                                
C STORE ELEMENT OF L                                                    
            A(JLU) = AKI                                                
            JA(JLU) = I                                                 
            JLU = JLU+1                                                 
            IF (JMAX .LT. JMIN) GOTO 14                                 
               DO  13 J = JMIN, JMAX                                    
C ELIMINATE ITH ELEMENT                                                 
                  JUJ = JA(J)                                           
                  ICC = IC(JUJ)                                         
C IF IROW(ICC) IS 0 THEN FILL IN WILL OCCUR                             
                  IF (IROW(ICC) .NE. 0) GOTO 12                         
                     IF (ICC .LT. K) GOTO 8                             
                        IROW(ICC) = IUEND                               
C SINCE FILLIN IS IN U PORITION JSUT ADD TO END OF LINKED LIST          
                        IUEND = ICC                                     
                        GOTO  11                                        
   8                    TEMP = M .GT. ICC                               
                        IF (.NOT. TEMP) TEMP = M .LT. I                 
                        IF (TEMP) M = I                                 
C FILL IN IS IN L PORTION SO ONE MUST INSERT IN ORDERED LIST            
   9                       IJ = IROW(M)                                 
                           IF (IJ .GE. ICC) GOTO  10                    
                           M = IJ                                       
                           GOTO  9                                      
  10                    IROW(M) = ICC                                   
                        IROW(ICC) = IJ                                  
  11                 Z(ICC) = 0.D0                                      
  12              Z(ICC) = Z(ICC)+AKI*A(J)                              
  13              CONTINUE                                              
  14        CONTINUE                                                    
C FIND PIVOT                                                            
            GOTO  7                                                     
  15     I = IUEND                                                      
         PMAX = 0.D0                                                    
         NU = 0                                                         
  16     IF (I .EQ. NP2) GOTO  18                                       
            PVT = DABS(Z(I))                                            
            NU = NU+1                                                   
            IF (PVT .LE. PMAX) GOTO 17                                  
               IMAX = I                                                 
               PMAX = PVT                                               
  17        I = IROW(I)                                                 
            GOTO  16                                                    
C DO THRESHHOLD PIVOTING                                                
  18     IF (DABS(Z(K)) .GE. THRESH*PMAX) IMAX = K                      
C CHECK FOR SINGULARITY                                                 
         IF (DABS(Z(IMAX)) .GT. EPS) GO TO 27                           
            DK=D1MACH(2)                                                
            IF (IERR.EQ.0)IERR=3*N+K+10                                 
            GO TO 277                                                   
   27    DK=1.D0/Z(IMAX)                                                
  277    CONTINUE                                                       
         AFT = DMAX1(AFT, PMAX)                                         
         I = IUEND                                                      
         NU = JLU+NU                                                    
C CHECK IF SUFFICIENT SPACE TO STORE U PORTION                          
         IF (NU .GT. IAMAX) GOTO  28                                    
C STORE DIAGONAL ELEMENT                                                
         A(JLU) = DK                                                    
         JA(JLU) = C(IMAX)                                              
         IU(K) = JLU+1                                                  
         JLU = NU                                                       
  19     IF (I .EQ. NP2) GOTO  21                                       
C CHECK IF DIAGONAL ELEMENT WHICH HAS ALREADY BEEN STORED               
            IF (I .EQ. IMAX) GOTO 20                                    
               NU = NU-1                                                
C STORE ELEMENT OF U                                                    
               A(NU) = Z(I)*DK                                          
               JA(NU) = C(I)                                            
  20        II = I                                                      
            I = IROW(I)                                                 
            IROW(II) = 0                                                
            GOTO  19                                                    
  21     IA(K+1) = JLU                                                  
C INTERCHANGE THE COLUMN INDICES                                        
         IF (K .EQ. IMAX) GOTO 22                                       
            JJ = C(K)                                                   
            C(K) = C(IMAX)                                              
            C(IMAX) = JJ                                                
            CK = C(K)                                                   
            IC(CK) = K                                                  
            IC(JJ) = IMAX                                               
  22     CONTINUE                                                       
  23     CONTINUE                                                       
      GROWTH = 1.0D0                                                    
      IF (BEFOR.NE.0.0D0)GROWTH = DMAX1(1.0D0, AFT/BEFOR)               
      JA(JLU) = JLU                                                     
      RETURN                                                            
  24  IERR = 2*N+K+10                                                   
      RETURN                                                            
  25  IERR = K+10                                                       
      RETURN                                                            
  26  IERR = 2*N+K+10                                                   
      RETURN                                                            
  28  IERR = 2*N+K+10                                                   
      RETURN                                                            
  29  IERR = K+10+N                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE DS4FSL(N,R, C, IA, JA, A, IU, B, IB, NB, TMP)          
      INTEGER N, IB, NB                                                 
      INTEGER R(N), C(N), IA(1), JA(1), IU(N)                           
      DOUBLE PRECISION A(1), B(IB, NB), TMP(N)                          
      INTEGER IIB, IIC, IUI, JUJ, I, J                                  
      INTEGER K, JMIN, JMAX, JJ, IR, NP1                                
      DOUBLE PRECISION SUM, DK                                          
C THIS IS LOWER LEVEL SUBROUTINE FOR DSPFSL                             
C SPARSE FORWARD SOLVE                                                  
      NP1 = N+1                                                         
      DO  7 K = 1, NB                                                   
         DO  3 I = 1, N                                                 
            IR = R(I)                                                   
            JMIN = IA(I)                                                
            IUI = IU(I)-1                                               
            JMAX = IUI-1                                                
            DK = A(IUI)                                                 
            SUM = B(IR, K)                                              
            IF (JMIN .GT. JMAX) GOTO 2                                  
               DO  1 J = JMIN, JMAX                                     
                  JJ = JA(J)                                            
                  SUM = SUM+A(J)*TMP(JJ)                                
   1              CONTINUE                                              
   2        TMP(I) = SUM*DK                                             
   3        CONTINUE                                                    
C SPARSE BACKWARD SOLVE                                                 
         DO  6 IIB = 1, N                                               
            I = NP1-IIB                                                 
            JMIN = IU(I)                                                
            JMAX = IA(I+1)-1                                            
            SUM = TMP(I)                                                
            IF (JMIN .GT. JMAX) GOTO 5                                  
               DO  4 J = JMIN, JMAX                                     
                  JUJ = JA(J)                                           
                  SUM = SUM-A(J)*B(JUJ, K)                              
   4              CONTINUE                                              
   5        IIC = C(I)                                                  
            B(IIC, K) = SUM                                             
   6        CONTINUE                                                    
   7     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DSPFNF(N,MRP,MCP, AROW, IWORK,A , GROWTH, EPS)         
      INTEGER N, IWORK(1), JLU                                          
       INTEGER MRP(N), MCP(N)                                           
      INTEGER ROW, ROWM1                                                
      DOUBLE PRECISION  GROWTH, EPS, A(1)                               
      EXTERNAL AROW                                                     
      INTEGER IST(1000)                                                 
      DOUBLE PRECISION DD(500)                                          
      COMMON /CSTAK/ DD                                                 
       EQUIVALENCE (DD(1),IST(1))                                       
C NUMERICAL FACTORIZATION                                               
C INPUT PARAMETERS                                                      
C N    ORDER OF MATRIX                                                  
C MRP   INTEGER VECTOR OF LENGTH N GIVING ROW PERMUTATIONS              
C MCP   INTEGER VECTOR OF LENGTH N GIVING COLUMN PERMUTATIONS           
C IWORK INTEGER VECTOR OF SYMBOLIC FACTORIZATION COMPUTED BY            
C        SPFSF                                                          
C EPS     LARGEST NONACCEPTABLE PIVOT FOR SINGULARITY TEST              
C AROW    SUBROUTINE,DECLARED EXTERNAL IN THE USERS MAIN PROGRAM        
C         WHICH HAS THE CALLING SEQUENCE AROW(I,ROW,ROW,NUM)            
C         WITH INPUT PARAMETER I AND OUTPUT PARAMETERS ROW, ROW         
C         AND NUM WHICH RETURNS IN THE D.P. VECTOR ROW THE              
C         NONZERO ENTRIES IN THE ITH ROW OF A. THE VECTOR               
C         ROW SHOULD BE FILLED WITH THE CORRESPONDING COLUMN            
C         INDICES OF THE NONZERO ELEMENTS. NUM GIVES THE NUMBER         
C         OF THE ELEMENTS                                               
C OUTPUT PARAMETERS                                                     
C A     NUMERICAL FACTORIZATION OF MATRIX                               
C G    GROWTH FACTOR                                                    
C STORAGE ALCATED -2N DOUBLE PRECISION AND N INTEGER LOCATIONS          
C       ARE RETURNED                                                    
C ERROR STATES                                                          
C 1   N.LT.1     FATAL                                                  
C 10+K SINGULAR MATRIX OF RANK K   RECOVERABLE                          
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HDSPFNF-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DSPFNF-N.LT.1', 13, 1, 2)              
C/                                                                      
      CALL ENTER(1)                                                     
      ID = ISTKGT(2*N, 4)                                               
      ROW= ISTKGT(N,2)                                                  
      ROWM1=ROW-1                                                       
      IROW=ID+N                                                         
        DO 10 I=1,N                                                     
           K=MCP(I)                                                     
           INDEX=ROWM1+K                                                
           IST(INDEX)=I                                                 
 10     CONTINUE                                                        
       JLU=2*N+2                                                        
       CALL DS4FNF(N,MRP,IST(ROW),IWORK,IWORK(N+2),IWORK(JLU),A,        
     1   AROW,DD(IROW),GROWTH,EPS,MCP)                                  
C/6S                                                                    
C     IF (NERROR(NERR) .NE. 0) CALL N5ERR(22HDSPFNF-SINGULAR MATRIX, 22,
C    1   NERR, 1)                                                       
C/7S                                                                    
      IF (NERROR(NERR) .NE. 0) CALL N5ERR('DSPFNF-SINGULAR MATRIX', 22, 
     1   NERR, 1)                                                       
C/                                                                      
         DO 20 I=1,N                                                    
             IDEX=ROWM1+I                                               
             INDEX=IST(IDEX)                                            
             MCP(INDEX)=I                                               
 20      CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DS4FNF(N,R,IC,IL,IU,JU,U,GETA,ROW,G,EPS,JROW)          
      INTEGER N, IL(1), IU(1), JU(1),JROW(N)                            
      INTEGER R(N),IC(N)                                                
      EXTERNAL GETA                                                     
      DOUBLE PRECISION  ROW(1), G, EPS,U(1)                             
      INTEGER IMIN, JMIN, IMAX, JMAX, I, J                              
      INTEGER K                                                         
      DOUBLE PRECISION DK, LI, TEMP, BEF, AFT                           
      INTEGER TEMP1, TEMP2                                              
C       INPUT VARIABLES N,IL,IU,JU                                      
C       OUTPUT VARIABLES--   G,  U,                                     
C       PARAMETERS USED INTERNALLY--                                    
C FIA   * ROW - HOLDS INTERMEDIATE VALUES IN CALCULATION OF  U AND L.   
C       *         SIZE = N.                                             
C RF       G      ELEMENT GROWTH                                        
C RN      EPS      LARGEST NONACCEPTABLE PIVOT                          
C  INTERNAL VARIABLES--                                                 
C    JMIN, JMAX - INDICES OF THE FIRST AND LAST POSITIONS IN A ROW TO   
C      BE EXAMINED.                                                     
C    SUM - USED IN CALCULATING  TMP.                                    
C  ******  FOR EACH ROW  ********************************************** 
      BEF = 0.0                                                         
      AFT = 0.0                                                         
      DO 100 I=1,N                                                      
          ROW(I)=0.D0                                                   
 100  CONTINUE                                                          
      DO  8 K = 1, N                                                    
C  ******  SET THE INITIAL STRUCTURE OF ROW  ************************** 
         IR=R(K)                                                        
         IMIN = IL(K)                                                   
         IMINM1=IMIN-1                                                  
         CALL GETA(IR,U(IMIN),JROW,NUM)                                 
         DO  1 J = 1, NUM                                               
            JJ=J+IMINM1                                                 
            IF (DABS(U(JJ)) .GT. BEF) BEF = DABS(U(JJ))                 
            IT1= JROW(J)                                                
            IT2=IC(IT1)                                                 
            ROW(IT2) = U(JJ)                                            
   1        CONTINUE                                                    
C  ******  ASSIGN THE KTH ROW OF L AND ADJUST ROW,   ***************    
         IMAX = IU(K)-2                                                 
         IF (IMIN .GT. IMAX) GOTO 5                                     
            DO  4 I = IMIN, IMAX                                        
               TEMP2 = JU(I)                                            
               LI = -ROW(TEMP2)                                         
               ROW(TEMP2)=0.D0                                          
               U(I) = LI                                                
               JMIN = IU(TEMP2)                                         
               JMAX = IL(TEMP2+1)-1                                     
               IF (JMIN .GT. JMAX) GOTO 3                               
                  DO  2 J = JMIN, JMAX                                  
                     TEMP1 = JU(J)                                      
                     ROW(TEMP1) = ROW(TEMP1)+LI*U(J)                    
   2                 CONTINUE                                           
   3           CONTINUE                                                 
   4           CONTINUE                                                 
C  ******  ASSIGN KTH ROW OF U AND DIAGONAL D, SET TMP(K)  ************ 
   5     IF (DABS(ROW(K)) .LE. EPS) GOTO  9                             
         DK = 1D0/ROW(K)                                                
         AFT=DMAX1(AFT,DABS(ROW(K)))                                    
         ROW(K)=0.D0                                                    
         JMIN = IU(K)                                                   
         U(JMIN-1) = DK                                                 
         JMAX = IL(K+1)-1                                               
         IF (JMIN .GT. JMAX) GOTO 7                                     
            DO  6 J = JMIN, JMAX                                        
               TEMP1 = JU(J)                                            
               TEMP = ROW(TEMP1)                                        
               ROW(TEMP1)=0.D0                                          
               IF (DABS(TEMP) .GT. AFT) AFT = DABS(TEMP)                
               U(J) = TEMP*DK                                           
   6           CONTINUE                                                 
   7     CONTINUE                                                       
   8     CONTINUE                                                       
C  ******  NORMAL RETURN AND ERROR RETURNS  *************************** 
      G = AFT/BEF                                                       
      RETURN                                                            
C  ZERO DIAGONAL ELEMENT                                                
C/6S                                                                    
C  9  CALL SETERR(20HDNNF-SINGULAR MATRIX, 20, K+9, 1)                  
C/7S                                                                    
   9  CALL SETERR('DNNF-SINGULAR MATRIX', 20, K+9, 1)                   
C/                                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE DSPFML(N, AROW, X, B)                                  
      INTEGER N                                                         
      DOUBLE PRECISION X(N), B(N)                                       
      EXTERNAL AROW                                                     
      DOUBLE PRECISION SUM                                              
      INTEGER JMAX, I, J, JJ, IIA, IIAM1, JP, JR, IROW, IROWM1          
      DOUBLE PRECISION R(500)                                           
      INTEGER IA(1000)                                                  
      COMMON /CSTAK/  R                                                 
      EQUIVALENCE (R(1),IA(1))                                          
C THIS SUBROUTINE MULTIPLIES A BY X AND PUTS THE RESULT IN B WHERE      
C A COMPUTED BY A FUNCTION                                              
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C AROW    FUNCTION WHICH DELIVERS TO DSPFML ONE ROW OF THE              
C         MATRIX AT A TIME                                              
C X       N-VECTOR TO BE MULTIPLIED                                     
C OUTPUT PARAMETERS                                                     
C B      A*X                                                            
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C STORAGE TAKEN FROM STACK- N D.P. AND N INTEGER LOCATIONS              
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HDSPFML-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DSPFML-N.LT.1', 13, 1, 2)              
C/                                                                      
      CALL ENTER(1)                                                     
      IIA=ISTKGT(N, 2)                                                  
      IIAM1=IIA-1                                                       
      IROW = ISTKGT(N, 4)                                               
      IROWM1=IROW-1                                                     
      DO 30 I=1,N                                                       
         CALL AROW(I, R(IROW), IA(IIA), JMAX)                           
         SUM=0.0D0                                                      
         IF (JMAX.LT.1) GO TO 20                                        
         DO 10 JJ=1,JMAX                                                
            JP=JJ+IIAM1                                                 
            J=IA(JP)                                                    
            JR=JJ+IROWM1                                                
            SUM=SUM+R(JR)*X(J)                                          
  10     CONTINUE                                                       
  20     B(I)=SUM                                                       
  30  CONTINUE                                                          
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DSPMML(N, IA, JA, A,  X, B)                            
      INTEGER N                                                         
      INTEGER IA(N), JA(N)                                              
      DOUBLE PRECISION A(N), X(N), B(N)                                 
      DOUBLE PRECISION SUM                                              
      INTEGER JMIN, JMAX, I, J, JJ                                      
C THIS SUBROUTINE MULTIPLIES A BY X AND PUTS THE RESULT IN B WHERE      
C A IS A SPARSE MATRIX                                                  
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C IA      INTEGER VECTOR, LENGTH N+1, POINTING TO BEGINNINGS            
C         OF ROWS IN JA AND A VECTORS                                   
C JA      COLUMN INDICES OF NONZERO ELEMENTS OF MATRIX                  
C A       NONZERO ELEMENTS OF THE MATRIX                                
C X       N-VECTOR TO BE MULTIPLIED                                     
C OUTPUT PARAMETERS                                                     
C B      A*X                                                            
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HDSPMML-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DSPMML-N.LT.1', 13, 1, 2)              
C/                                                                      
      SUM=0.0D0                                                         
      DO 30 I=1,N                                                       
         JMIN=IA(I)                                                     
         JMAX=IA(I+1)-1                                                 
         SUM=0.0D0                                                      
         IF (JMAX.LT.JMIN) GO TO 20                                     
         DO 10 JJ=JMIN,JMAX                                             
            J=JA(JJ)                                                    
            SUM=SUM+A(JJ)*X(J)                                          
  10     CONTINUE                                                       
  20     B(I)=SUM                                                       
  30  CONTINUE                                                          
      RETURN                                                            
      END                                                               
        SUBROUTINE DSPMIN(N,IC,IWORK,I,AROW,JROW,NUM,INDEX,A)           
C                                                                       
C THIS SUBROUTINE INSERTS IN THE VECTOR A THE ELEMENTS OF               
C AROW ACCORDING TO THE SCHEME RECORDED IN IWORK AND                    
C DETERMINED BY SPMSF                                                   
C                                                                       
C INPUT PARAMETERS                                                      
C N       NUMBER OF ROWS IN ORIGINAL MATRIX                             
C R       INTEGER VECTOR OF LENGTH N CONTAINING ROW PERMUTAIONS         
C IC      INTEGER VECTOR OF LENGTH N CONTAINING INVERSE OF              
C         COLUMN PERMUTATIONS                                           
C IWORK   INTEGER VECTOR OUTPUT FROM SPMSF                              
C I       WHICH ROW NOW INTSERTING                                      
C AROW    COMPLEX VECTOR LENGTH NUM OF NONZERO ELEMENTS IN ROW I        
C         OF ORIGINAL MATRIX                                            
C JROW    INTEGER VECTOR OF LENGTH NUM OF COLUMN INDICES IN ROW I       
C         OF ORIGINAL VECTOR                                            
C NUM     NUMBER OF NONZERO ELEMENTS IN ROW I NOW INSERTING INTO A      
C INDEX   IF INDEX IS 1 THE WHOLE ARRAY A IS ZEROED OUT BEFOREHAND      
C         THIS ALLOWS ONE TO CALL THE SUBROUTINE SEVERAL TIMES          
C         FOR 1 ROW AND TO ADD THE NEW INFORMATION                      
C OUTPUT PARAMETER                                                      
C A       VECTOR FOR INSERT INTO DSPMNF WITH THE INFORMATION            
C         FROM AROW INSERTED                                            
C ERROR STATES                                                          
C 1 N.LT.1                                                              
C 2 I NOT IN 1 THOUGH N                                                 
C 3 NUM.LT.1                                                            
C 4 INDEX IN JROW VECTOR FOR ROW I WAS NOT KNOWN TO SPMSF               
C                                                                       
        INTEGER N,I,NUM                                                 
        INTEGER IC(N),JROW(N),INDEX,IWORK(1)                            
        DOUBLE PRECISION A(N),AROW(NUM)                                 
        INTEGER NTOT,K,IR,JS,JE,KK,MM,M,MIN,IDISP                       
C/6S                                                                    
C       IF (N.LT.1) CALL SETERR(13HDSPMIN-N.LT.1,13,1,2)                
C       IF (NUM.LT.1) CALL SETERR(15HDSPMIN-NUM.LT.1,15,3,2)            
C       IF(I.LT.1.OR.I.GT.N) CALL SETERR(21HDSPMIN-I OUT OF RANGE,      
C    1      21,2,2)                                                     
C/7S                                                                    
        IF (N.LT.1) CALL SETERR('DSPMIN-N.LT.1',13,1,2)                 
        IF (NUM.LT.1) CALL SETERR('DSPMIN-NUM.LT.1',15,3,2)             
        IF(I.LT.1.OR.I.GT.N) CALL SETERR('DSPMIN-I OUT OF RANGE',       
     1      21,2,2)                                                     
C/                                                                      
         IF (INDEX.NE.1) GO TO 10                                       
            NTOT=IWORK(N+1)-1                                           
            DO 1 K=1,NTOT                                               
                 A(K)=0.0D0                                             
 1          CONTINUE                                                    
 10      IR=I                                                           
         IDISP=2*N+1                                                    
         JS=IWORK(IR)+IDISP                                             
         JE= IWORK(IR+1)+IDISP-1                                        
         KK=JS                                                          
         DO 50 K=1,NUM                                                  
            MM=JROW(K)                                                  
            M=IC(MM)                                                    
            IF (IWORK(KK)-M)30,40,20                                    
 20         IF (KK.EQ.JS) GO TO 80                                      
            KK=KK-1                                                     
            IF (IWORK(KK)-M)80,40,20                                    
 30         IF (KK.EQ.JE) GO TO 80                                      
            KK=KK+1                                                     
            IF (IWORK(KK)-M)30,40,80                                    
 40         MIN=KK-IDISP                                                
            A(MIN)=A(MIN)+AROW(K)                                       
 50     CONTINUE                                                        
        RETURN                                                          
C/6S                                                                    
C80     CALL SETERR(20HDSPMIN-UNKNOWN INDEX,20,4,2)                     
C/7S                                                                    
 80     CALL SETERR('DSPMIN-UNKNOWN INDEX',20,4,2)                      
C/                                                                      
        RETURN                                                          
        END                                                             
      SUBROUTINE DSPMNF(N, IWORK, A,  EPS, GROWTH)                      
      INTEGER N, IWORK(1), JLU                                          
      DOUBLE PRECISION A(1),  EPS, GROWTH                               
      DOUBLE PRECISION DD(500)                                          
      COMMON /CSTAK/ DD                                                 
      INTEGER ID, NERR, ISTKGT, NERROR, ISP(1)                          
      EQUIVALENCE (DD(1), ISP(1))                                       
C NUMERICAL FACTORIZATION                                               
C INPUT PARAMETERS                                                      
C N    ORDER OF MATRIX                                                  
C IWORK INTEGER VECTOR OF SYMBOLIC FACTORIZATION COMPUTED BY            
C       DSPMSF                                                          
C EPS     LARGEST NONACCEPTABLE PIVOT FOR SINGULARITY TEST              
C A       VECTOR COMPUTED BY DSPMIN ACCORDING TO INSTRUCTIONS           
C         GIVEN IN DSPMSF                                               
C OUTPUT PARAMETERS                                                     
C A     NUMERICAL FACTORIZATION OF MATRIX                               
C GROWTH NUMERICAL ELEMENT GROWTH. IF THIS MUCH .GT. 1, THEN THE        
C        COMPUTED DECOMPOSITION MAY BE THE DECOMPOSITION OF A MATRIX    
C        THAT IS NOT VERY CLOSE TO THE ORIGINAL MATRIX.                 
C STORAGE ALLOCATED -N DOUBLE PRECISION LOCATION WHICH                  
C       ARE RETURNED                                                    
C ERROR STATES                                                          
C 1   N.LT.1     FATAL                                                  
C 10+K SINGULAR MATRIX OF RANK K   RECOVERABLE                          
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HDSPMNF-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DSPMNF-N.LT.1', 13, 1, 2)              
C/                                                                      
      CALL ENTER(1)                                                     
      ID = ISTKGT(N, 4)                                                 
       JLU=2*N+2                                                        
       CALL DS4MNF(N,IWORK,IWORK(N+2),IWORK(JLU),A,DD(ID),GROWTH,EPS)   
C/6S                                                                    
C     IF (NERROR(NERR) .NE. 0) CALL N5ERR(22HDSPMNF-SINGULAR MATRIX, 22,
C    1   NERR, 1)                                                       
C/7S                                                                    
      IF (NERROR(NERR) .NE. 0) CALL N5ERR('DSPMNF-SINGULAR MATRIX', 22, 
     1   NERR, 1)                                                       
C/                                                                      
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DS4MNF(N, IL, IU, JU, U, ROW, G, EPS)                  
      INTEGER N, IL(1), IU(1), JU(1)                                    
      DOUBLE PRECISION U(1), ROW(1), G, EPS                             
      INTEGER IMIN, JMIN, IMAX, JMAX, I, J                              
      INTEGER K                                                         
      DOUBLE PRECISION DK, LI, TEMP, BEF, AFT                           
      INTEGER TEMP1, TEMP2                                              
C       INPUT VARIABLES N,IL,IU,JU                                      
C       OUTPUT VARIABLES--   G,  U,                                     
C       PARAMETERS USED INTERNALLY--                                    
C FIA   * ROW - HOLDS INTERMEDIATE VALUES IN CALCULATION OF  U AND L.   
C       *         SIZE = N.                                             
C RF       G      ELEMENT GROWTH                                        
C RN      EPS      LARGEST NONACCEPTABLE PIVOT                          
C  INTERNAL VARIABLES--                                                 
C    JMIN, JMAX - INDICES OF THE FIRST AND LAST POSITIONS IN A ROW TO   
C      BE EXAMINED.                                                     
C    SUM - USED IN CALCULATING  TMP.                                    
C  ******  FOR EACH ROW  ********************************************** 
      BEF = 0.0                                                         
      AFT = 0.0                                                         
      DO  8 K = 1, N                                                    
C  ******  SET THE INITIAL STRUCTURE OF ROW  ************************** 
         JMIN = IL(K)                                                   
         JMAX = IL(K+1)-1                                               
         DO  1 J = JMIN, JMAX                                           
            IF (DABS(U(J)) .GT. BEF) BEF = DABS(U(J))                   
            TEMP2 = JU(J)                                               
            ROW(TEMP2) = U(J)                                           
   1        CONTINUE                                                    
C  ******  ASSIGN THE KTH ROW OF L AND ADJUST ROW,   ***************    
         IMIN = IL(K)                                                   
         IMAX = IU(K)-2                                                 
         IF (IMIN .GT. IMAX) GOTO 5                                     
            DO  4 I = IMIN, IMAX                                        
               TEMP2 = JU(I)                                            
               LI = -ROW(TEMP2)                                         
               U(I) = LI                                                
               JMIN = IU(TEMP2)                                         
               JMAX = IL(TEMP2+1)-1                                     
               IF (JMIN .GT. JMAX) GOTO 3                               
                  DO  2 J = JMIN, JMAX                                  
                     TEMP1 = JU(J)                                      
                     ROW(TEMP1) = ROW(TEMP1)+LI*U(J)                    
   2                 CONTINUE                                           
   3           CONTINUE                                                 
   4           CONTINUE                                                 
C  ******  ASSIGN KTH ROW OF U AND DIAGONAL D, SET TMP(K)  ************ 
   5     IF (DABS(ROW(K)) .LE. EPS) GOTO  9                             
         DK = 1D0/ROW(K)                                                
         JMIN = IU(K)                                                   
         U(JMIN-1) = DK                                                 
         AFT=DMAX1(AFT,DABS(ROW(K)))                                    
         JMAX = IL(K+1)-1                                               
         IF (JMIN .GT. JMAX) GOTO 7                                     
            DO  6 J = JMIN, JMAX                                        
               TEMP1 = JU(J)                                            
               TEMP = ROW(TEMP1)                                        
               IF (DABS(TEMP) .GT. AFT) AFT = DABS(TEMP)                
               U(J) = TEMP*DK                                           
   6           CONTINUE                                                 
   7     CONTINUE                                                       
   8     CONTINUE                                                       
C  ******  NORMAL RETURN AND ERROR RETURNS  *************************** 
      G = AFT/BEF                                                       
      RETURN                                                            
C  ZERO DIAGONAL ELEMENT                                                
C/6S                                                                    
C  9  CALL SETERR(22HDS4MNF-SINGULAR MATRIX, 22, K+9, 1)                
C/7S                                                                    
   9  CALL SETERR('DS4MNF-SINGULAR MATRIX', 22, K+9, 1)                 
C/                                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE DSPSOL(N, MRP,MCP, IWORK,UL,  B, IB, NB)               
      INTEGER IB, NB, N                                                 
       INTEGER MRP(N), MCP(N), IWORK(1)                                 
      DOUBLE PRECISION UL(1), B(IB, NB)                                 
      DOUBLE PRECISION D(500)                                           
      COMMON /CSTAK/ D                                                  
      INTEGER  ITMP, ISTKGT                                             
C FORWARD SOLVE FOR SPARSE MATRICES                                     
C INPUT PARAMETERS                                                      
C N     NUMBER OF EQUATIONS                                             
C MRP   INTEGER VECTOR OF ROW PERMUTATIONS                              
C MCP   INTEGER VECTOR,LENGTHN OR COLUMN PERMUTATIONS                   
C IWORK INTEGER VECTOR COMPUTED BY DSPF(ORM)NF                          
C UL    REAL ARRAY COMPUTED BY DSPF(ORM)NF                              
C IU    INTEGER VECTOR OF LENGTH N POINTING TO U IN LU                  
C       DECOMPOSITION,COMPUTED BY SPLU,SF(ORM)NF,OR SPCE                
C B     MATRIX OF RIGHT HAND SIDES                                      
C IB    ROW DIMENSION OF B MATRIX                                       
C NB    NUMBER OF RIGHT HAND SIDES                                      
C OUTPUT PARAMETERS                                                     
C B     THE SOLUTION MATRIX                                             
C STORAGE SPACE ALLOCATED AND DEALLOCATED -N DOUBLE PRECISION LOCATIONS 
C ERROR CONDITIONS                                                      
C 1 N.LT.1    FATAL                                                     
C 2 IB.LT.N   FATAL                                                     
C 3 NB.LT.1   FATAL                                                     
C/6S                                                                    
C     IF (N.LT.1) CALL SETERR(13HDSPSOL-N.LT.1,13,1,2)                  
C     IF (IB.LT.N) CALL SETERR(14HDSPSOL-IB.LT.N,14,2,2)                
C     IF(NB.LT.1) CALL SETERR(14HDSPSOL-NB.LT.1,14,3,2)                 
C/7S                                                                    
      IF (N.LT.1) CALL SETERR('DSPSOL-N.LT.1',13,1,2)                   
      IF (IB.LT.N) CALL SETERR('DSPSOL-IB.LT.N',14,2,2)                 
      IF(NB.LT.1) CALL SETERR('DSPSOL-NB.LT.1',14,3,2)                  
C/                                                                      
      CALL ENTER(1)                                                     
      ITMP = ISTKGT(N, 4)                                               
      JLU=2*N+2                                                         
      CALL DS4SOL(N,MRP,MCP,IWORK,IWORK(JLU),UL, IWORK(N+2),            
     1  B,IB,NB,D(ITMP))                                                
       CALL LEAVE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DS4SOL(N, R,C, IA, JA, A, IU,  B, IB, NB, TMP)         
      INTEGER IB, NB, N                                                 
      INTEGER R(N), IA(1), JA(1), IU(N), C(N)                           
      DOUBLE PRECISION A(1), B(IB, NB), TMP(N)                          
      INTEGER JJ, IR, JMIN, JMAX, NP1, I                                
      INTEGER J, K, IUI                                                 
      DOUBLE PRECISION DK, SUM                                          
C SPARSE FORWARD SOLVE                                                  
      NP1 = N+1                                                         
      DO  10 K = 1, NB                                                  
         DO  5 I = 1, N                                                 
            IR = R(I)                                                   
            JMIN = IA(I)                                                
            IUI = IU(I)-1                                               
            JMAX=IUI-1                                                  
            DK = A(IUI)                                                 
            SUM = B(IR, K)                                              
               IF (JMIN .GT. JMAX) GOTO 4                               
                  DO  2 J = JMIN, JMAX                                  
                     JJ = JA(J)                                         
                     SUM = SUM+A(J)*TMP(JJ)                             
   2                 CONTINUE                                           
   4        TMP(I) = SUM*DK                                             
   5        CONTINUE                                                    
         DO  9 IIB = 1, N                                               
            I = NP1-IIB                                                 
            JMIN = IU(I)                                                
            JMAX = IA(I+1)-1                                            
            SUM = TMP(I)                                                
            IF (JMIN .GT. JMAX) GOTO 8                                  
                  DO  7 J = JMIN, JMAX                                  
                     JUJ = JA(J)                                        
                     SUM = SUM-A(J)*TMP(JUJ)                            
   7                 CONTINUE                                           
   8        IIC = C(I)                                                  
            TMP(I) = SUM                                                
            B(IIC,K) = SUM                                              
   9        CONTINUE                                                    
   10     CONTINUE                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE CSPMLE(N, ORDER, IA, JA, A, ISIZE, B, IB, NB)          
C  THIS IS LINDA KAUFMANS SPARSE MATRIX PACKAGE                         
C  COMPLEX VERSION                                                      
C                                                                       
C  DECEMBER 9, 1982 (REVISION)                                          
C                                                                       
      INTEGER N, IB, NB                                                 
      INTEGER IA(N), JA(N), ISIZE                                       
      EXTERNAL I4YX                                                     
      COMPLEX A(N), B(IB, NB)                                           
      LOGICAL ORDER                                                     
      DOUBLE PRECISION DSTAK(500)                                       
      COMMON /CSTAK/ DSTAK                                              
      INTEGER MCP, MAX, JLU, MRP, ISTKGT, ISTKQU                        
      INTEGER I, L, IEND, ILIN, IERR, ISUB                              
      INTEGER LAST, TEMP, IC, IHEAD, MC, II(1000)                       
      INTEGER IV, IU, LU, MR, IZ, ILEFT                                 
      INTEGER IC1                                                       
      REAL BEF, CABS1, EPS,  AMAX1, R1MACH                              
      COMPLEX D(500)                                                    
      LOGICAL TEMP1                                                     
      EQUIVALENCE (D(1), II(1), DSTAK(1))                               
C THIS SUBROUTINE SOLVES AX = B WHERE A IS A SPARSE MATRIX              
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C ORDER   LOGICAL VARIABLE. IF .TRUE. REORDERING FOR                    
C         STABILITY WILL BE PERFORMED                                   
C IA      INTEGER VECTOR, LENGTH N+1, POINTING TO BEGINNINGS            
C         OF ROWS IN JA AND A VECTORS                                   
C JA      COLUMN INDICES OF NONZERO ELEMENTS OF MATRIX                  
C A       NONZERO ELEMENTS OF THE MATRIX                                
C B      RIGHT HAND SIDE MATRIX,DESTROYED ON OUTPUT                     
C IB     ROW DIMENSION OF B                                             
C NB     NUMBER OF COLUMNSOF B                                          
C OUTPUT PARAMETERS                                                     
C B      THE SOLUTION TO AX=B                                           
C ISIZE ACTUAL  UMBER OF ELEMENTS IN THE PORT STACK NEEDED TO           
C       SAVE THE DECOMPOSITION                                          
C SPACE ALLOCATED AND DEALLOCATED-3N+2 INTEGER AND N                    
C COMPLEX LOCATIONS PLUS ISIZE INTEGER AND                              
C COMPLEX LOCATIONS NEED TO HOLD THE U FROM THE                         
C LU DECOMPOSITION OF A                                                 
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C 2 IB .LT. N                                                           
C 3 NB .LT. 1                                                           
C 10+K     NULL ROW           FATAL                                     
C N+K+10   INCORRECT COLUMN INDEX AT ROW K     FATAL                    
C 3N+K+10   SINGULAR MATRIX OF RANK K      RECOVERABLE                  
C 2N+K+10  RAN OUT OF SPACE WHEN PROCEESING ROW K                       
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HCSPMLE-N.LT.1, 13, 1, 2)             
C     IF (IB .LT. N) CALL SETERR(14HCSPMLE-IB.LT.N, 14, 2, 2)           
C     IF (NB .LT. 1) CALL SETERR(14HCSPMLE-NB.LT.1, 14, 3, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('CSPMLE-N.LT.1', 13, 1, 2)              
      IF (IB .LT. N) CALL SETERR('CSPMLE-IB.LT.N', 14, 2, 2)            
      IF (NB .LT. 1) CALL SETERR('CSPMLE-NB.LT.1', 14, 3, 2)            
C/                                                                      
      CALL ENTER(1)                                                     
C GET SPACE FROM THE STACK FOR COLUMN ORDERING AND THE INVERSE          
C COLUMN ORDERING AND A COMPLEX VECTOR OF LENGTH N USED IN CS4MLE       
      IZ = ISTKGT(N, 5)                                                 
      IC = ISTKGT(2*N, 2)                                               
      MCP = IC+N                                                        
      IC1 = IC-1                                                        
      MC = MCP-1                                                        
      ILEFT = ISTKQU(2)-10                                              
C ISUB CONSTAINS THE NUMBER OF LOCATIONS NEEDED TO STORE THE ROW        
C PERMUTATION VECTOR, THE POINTER TO U AND AND AN INTEGER TEMPORARY     
C VECTOR                                                                
      ISUB = 3*N+3                                                      
C FIND THE NORM OF THE MATRIX                                           
      IEND = IA(N+1)-1                                                  
      BEF = 0.0                                                         
      DO  1 I = 1, IEND                                                 
         BEF = AMAX1(BEF, CABS1(A(I)))                                  
   1     CONTINUE                                                       
      EPS = R1MACH(4)*BEF                                               
      IF (ORDER) GOTO 3                                                 
         ISUB = ISUB-N                                                  
C WHEN THERE IS NO PERMUTATION MATRIX NO STORAGE IS NECESSARY           
C FOR THE ROW PERMUTATION VECTOR                                        
C SET UP COLUMN PERMUTATION ARRAYS TO INDICATE NO PERMUTATION           
         DO  2 I = 1, N                                                 
            IC1 = IC1+1                                                 
            II(IC1) = I                                                 
            MC = MC+1                                                   
            II(MC) = I                                                  
   2        CONTINUE                                                    
         GOTO  5                                                        
   3     JLU = ISTKGT(ILEFT, 2)                                         
C ALLOCATE ALL THE REMAINING SPACE                                      
         MAX = (ILEFT-N)/2                                              
         IV = JLU                                                       
         L = IV+MAX                                                     
         IHEAD = L+MAX                                                  
         IERR = 2*N+11                                                  
         IF (MAX .LT. N) GOTO  12                                       
         IERR = 0                                                       
         CALL S4MDM(N, I4YX, MAX, II(IV), II(L), II(IHEAD), II(MCP),    
     1      II(IC), II(IV), IERR,IA,JA,1)                               
         IF (IERR .EQ. 0) GOTO 4                                        
            IF (IERR .GT. 2*N+10) GOTO  12                              
            IF (IERR .LE. N+10) GOTO  11                                
            TEMP1 = IERR .GT. N+10                                      
            IF (TEMP1) TEMP1 = IERR .LE. 2*N+10                         
C/6S                                                                    
C           IF (TEMP1) CALL SETERR(29HCSPMLE-INCORRECT COLUMN INDEX, 29,
C    1         IERR,2)                                                  
C/7S                                                                    
            IF (TEMP1) CALL SETERR('CSPMLE-INCORRECT COLUMN INDEX', 29, 
     1         IERR,2)                                                  
C/                                                                      
   4     CALL ISTKRL(1)                                                 
C ILEFT IS THE NUMBER OF LOCATIONS LEFT IN THE STACK TO STORE U         
   5  ILEFT = ILEFT-ISUB                                                
C ALLOCATE THE SPACE BETWEEN INTEGER AND COMPLEX LOCATIONS DEPENDENT    
C ON THE SPACE REQUIRED FOR EACH                                        
      ILIN = (ILEFT*II(7))/(II(7)+II(10))                               
      JLU = ISTKGT(ILIN, 2)                                             
      LU = ISTKGT(ILIN, 5)                                              
      MRP = 1                                                           
      IU = ISTKGT(ISUB, 2)                                              
      TEMP = IU+N+2                                                     
      IF (.NOT. ORDER) GOTO 7                                           
         MC = MCP-1                                                     
C PUT THE ROW PERMUTATION  VECTOR EQUAL TO THE COLUMN                   
C PERMUTATION VECTOR                                                    
         MR = TEMP+N                                                    
         MRP = MR+1                                                     
         DO  6 I = 1, N                                                 
            MC = MC+1                                                   
            MR = MR+1                                                   
            II(MR) = II(MC)                                             
   6        CONTINUE                                                    
   7  CALL CS4MLE(N,B, IB, NB, II(JLU), D(LU), ILIN, II(IU), II(TEMP), D
     1   (IZ), II(IC), II(MRP), II(MCP), IERR, 0.1E0, LAST, IA, JA, A,  
     1   ORDER, EPS)                                                    
      IF (IERR .NE. 0) GOTO 8                                           
         ISIZE = LAST-1                                                 
         CALL CS4MBS(N,II(IU), II(JLU), D(LU), II(MCP), B, IB, NB, D(IZ)
     1      )                                                           
         GOTO  13                                                       
   8     IF (IERR .LE. N+10) GOTO 11                                    
            IF (IERR .LE. 3*N+10) GOTO  12                              
C/6S                                                                    
C           CALL SETERR(22HCSPMLE-SINGULAR MATRIX, 22, IERR, 1)         
C/7S                                                                    
            CALL SETERR('CSPMLE-SINGULAR MATRIX', 22, IERR, 1)          
C/                                                                      
            GOTO  13                                                    
C/6S                                                                    
C 11  CALL SETERR(15HCSPMLE-NULL ROW, 15, IERR, 1)                      
C/7S                                                                    
  11  CALL SETERR('CSPMLE-NULL ROW', 15, IERR, 1)                       
C/                                                                      
      GOTO  13                                                          
C/6S                                                                    
C 12  CALL SETERR(25HCSPMLE-INSUFFICIENT SPACE, 25, IERR, 1)            
C/7S                                                                    
  12  CALL SETERR('CSPMLE-INSUFFICIENT SPACE', 25, IERR, 1)             
C/                                                                      
  13  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE CS4MLE(N, B, IB, NB, JA, A, IAMAX, IU, IROW, Z,        
     1   IC, R, C, IERR, THRESH, JLU, IA, JJA, AA, ORDER, EPS)          
      INTEGER N, IB, NB                                                 
      INTEGER JA(1), IAMAX, IU(N), IROW(N), IC(N), R(N)                 
      INTEGER C(N), IERR, JLU, IA(N), JJA(N)                            
      COMPLEX B(IB, NB), A(1), Z(1),  AA(N)                             
      REAL THRESH, EPS                                                  
      LOGICAL ORDER                                                     
      INTEGER ICC, JUJ, NUM, I, J, K                                    
      INTEGER JAMA, M, JMIN, IMAX, JMAX, CK                             
      INTEGER II, JJ, IJ, IR, MM, RK                                    
      INTEGER JAMIN, NU, IUEND, NP1, NP2                                
      REAL CABS1,  PVT, PMAX                                            
      COMPLEX DK,AKI                                                    
      LOGICAL TEMP                                                      
C THIS SUBROUTINE IS LOWER LEVEL SUBROUTINE FOR SPMLE                   
C PARAMETERS NOT DEFINED IN SPMLE ARE AS FOLLOWS                        
C JA -SPACE FOR STORING COLUMN INDICES OF U IN LU DECOMPOSITION         
C A - SPACE FOR STORING ELEMENTS OF U IN LU DECOMPOSITION               
C IAMAX - SIZE OF JA AND A                                              
C IU - POINTER INTO JA AND A GIVING BEGINNING OF EACH ROW               
C Z  TEMP VECTOR OF LENGTH N                                            
C IC- INTEGER VECTOR GIVING INVERTED COLUMN PERMUTATION                 
C R - IF ORDERING INTEGER VECTOR OF LENGTH N GIVING ROW PERMUTATIONS    
C C - COLUMN PERMUTATIONS, INTEGER VECTOR OF LENGTH N                   
C IERR - ERROR FLAG. OK IF SET OT 0 ON OUTPUT                           
C THRESH - THRESHHOLD PARAMETER FOR PIVOTING                            
C JLU - NUMBER OF ELEMENTS OF JA AND A ACTUALLY USED                    
C ORDER - LOGICAL VARIABLE. IF .TRUE. THEN PIVOTING FOR SPARSITY        
C WAS DONE AND R SHOULD BE LOOKED AT.                                   
C EPS - CRITERIA FOR SINGULARITY                                        
C IAMAX - SIZE OF SPACE AVAILABLE FOR SORING LU DECOMPOSITION           
C INITIALIZATION                                                        
      IU(1) = 1                                                         
C REARRANGE RIGHT HAND SIDES                                            
      IF (.NOT. ORDER) GOTO 4                                           
         DO  3 J = 1, NB                                                
            DO  1 I = 1, N                                              
               IR = R(I)                                                
               Z(I) = B(IR, J)                                          
   1           CONTINUE                                                 
            DO  2 I = 1, N                                              
               B(I, J) = Z(I)                                           
   2           CONTINUE                                                 
   3        CONTINUE                                                    
   4  NP1 = N+1                                                         
      IERR = 0                                                          
      NP2 = N+2                                                         
      JLU = 1                                                           
      DO  5 I = 1, N                                                    
         IROW(I) = 0                                                    
   5     CONTINUE                                                       
C DETERMINE NEXT ROW OF L AND U                                         
      DO  29 K = 1, N                                                   
         M = NP1                                                        
         IROW(NP1) = NP1                                                
         IUEND = NP2                                                    
         RK = K                                                         
         IF (ORDER) RK = R(K)                                           
         JAMIN = IA(RK)                                                 
         JAMA = IA(RK+1)-1                                              
         NUM = JAMA-JAMIN                                               
         Z(K) = (0.0E0,0.0E0)                                           
         NUM = NUM+1                                                    
C CHECK FOR NULL ROW                                                    
         IF (JAMA .LT. JAMIN) GOTO  30                                  
         DO  10 J = JAMIN, JAMA                                         
            JJ = JJA(J)                                                 
C CHECK FOR VALID COLUMN INDEX                                          
            TEMP = JJ .GT. N                                            
            IF (.NOT. TEMP) TEMP = JJ .LT. 1                            
C/6S                                                                    
C           IF (TEMP) CALL SETERR(29HCSPMLE-INCORRECT COLUMN INDEX, 29  
C    1         , K+10+N, 2)                                             
C/7S                                                                    
            IF (TEMP) CALL SETERR('CSPMLE-INCORRECT COLUMN INDEX', 29   
     1         , K+10+N, 2)                                             
C/                                                                      
            ICC = IC(JJ)                                                
            Z(ICC) = AA(J)                                              
            IF (ICC .LT. K) GOTO 6                                      
               IROW(ICC) = IUEND                                        
C NEW ELEMENT IS IN U PART OF ROW                                       
               IUEND = ICC                                              
               GOTO  9                                                  
   6           IF (M .GT. ICC) M = NP1                                  
C NEW ELEMENT IS IN L PART OF THE ROW                                   
   7              II = IROW(M)                                          
                  IF (II .GE. ICC) GOTO  8                              
                  M = II                                                
                  GOTO  7                                               
   8           IROW(M) = ICC                                            
               IROW(ICC) = II                                           
   9        CONTINUE                                                    
  10        CONTINUE                                                    
C                                                                       
C ELIMINATE ROW                                                         
         I = NP1                                                        
  11        J = I                                                       
            I = IROW(I)                                                 
            IROW(J) = 0                                                 
            IF (I .EQ. NP1) GOTO  20                                    
            JMIN = IU(I)                                                
            JMAX = IU(I+1)-1                                            
            AKI = -Z(I)                                                 
C FORWARD SOLVE                                                         
            DO  12 MM = 1, NB                                           
               B(K, MM) = B(K, MM)+AKI*B(I, MM)                         
  12           CONTINUE                                                 
            IF (JMAX .LT. JMIN) GOTO 19                                 
               DO  18 J = JMIN, JMAX                                    
C ELIMINATE ITH ELEMENT                                                 
                  JUJ = JA(J)                                           
                  ICC = IC(JUJ)                                         
                  IF (IROW(ICC) .NE. 0) GOTO 17                         
                     IF (ICC .LT. K) GOTO 13                            
                        IROW(ICC) = IUEND                               
C FILL IN-SEE IF IT IS IN U OR L                                        
C FILL IN IS IN U                                                       
                        IUEND = ICC                                     
                        GOTO  16                                        
  13                    TEMP = M .GT. ICC                               
C FILL IS IN L PORTION                                                  
                        IF (.NOT. TEMP) TEMP = M .LT. I                 
                        IF (TEMP) M = I                                 
  14                       IJ = IROW(M)                                 
                           IF (IJ .GE. ICC) GOTO  15                    
                           M = IJ                                       
                           GOTO  14                                     
  15                    IROW(M) = ICC                                   
                        IROW(ICC) = IJ                                  
C SINCE THIS IS A FILL IN, INITIALIZE SPACE                             
  16                 Z(ICC) = (0.0E0,0.0E0)                             
  17              Z(ICC) = Z(ICC)+AKI*A(J)                              
  18              CONTINUE                                              
  19        CONTINUE                                                    
            GOTO  11                                                    
  20     I = IUEND                                                      
         PMAX = 0.E0                                                    
         NU = -1                                                        
  21     IF (I .EQ. NP2) GOTO  23                                       
            PVT = CABS1(Z(I))                                           
            NU = NU+1                                                   
            IF (PVT .LE. PMAX) GOTO 22                                  
               IMAX = I                                                 
               PMAX = PVT                                               
  22        I = IROW(I)                                                 
            GOTO  21                                                    
C DO THRESHHOLD PIVOTING                                                
  23     IF (CABS1(Z(K)) .GE. THRESH*PMAX) IMAX = K                     
C CHECK FOR SINGULARITY                                                 
         IF (CABS1(Z(IMAX)) .LE. EPS) GOTO  31                          
C CHECK IF SUFFICIENT SPACE                                             
         IF (NU+JLU .GT. IAMAX) GOTO  32                                
         NU = JLU+NU                                                    
         JLU = NU                                                       
         DK = (1.E0,0.0E0)/Z(IMAX)                                      
C FIX UP RIGHT HAND SIDE                                                
         DO  24 MM = 1, NB                                              
            B(K, MM) = B(K, MM)*DK                                      
  24        CONTINUE                                                    
         I = IUEND                                                      
  25     IF (I .EQ. NP2) GOTO  27                                       
            IF (I .EQ. IMAX) GOTO 26                                    
               NU = NU-1                                                
               A(NU) = Z(I)*DK                                          
               JA(NU) = C(I)                                            
  26        II = I                                                      
            I = IROW(I)                                                 
C ZERO OUT SPCE SO NEXT TIME WILL NOT MISINTERPRET FILL-IN              
            IROW(II) = 0                                                
            GOTO  25                                                    
  27     IU(K+1) = JLU                                                  
C INTERCHANGE THE COLUMN INDICES                                        
         IF (K .EQ. IMAX) GOTO 28                                       
            JJ = C(K)                                                   
            C(K) = C(IMAX)                                              
            C(IMAX) = JJ                                                
            CK = C(K)                                                   
            IC(CK) = K                                                  
            IC(JJ) = IMAX                                               
  28     CONTINUE                                                       
  29     CONTINUE                                                       
      IU(N+1) = JLU                                                     
      JA(JLU) = JLU                                                     
      RETURN                                                            
  30  IERR = K+10                                                       
      RETURN                                                            
  31  IERR = 3*N+9+K                                                    
      RETURN                                                            
  32  IERR = 2*N+K+10                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE CS4MBS(N, IA, JA, A, C, B, IB, NB, TMP)                
      INTEGER N, IB, NB                                                 
      INTEGER IA(1), JA(1), C(N)                                        
      COMPLEX A(1), B(IB, NB), TMP(N)                                   
      INTEGER IIB, IIC, JUJ, I, J, K                                    
      INTEGER JMIN, JMAX, NP1                                           
      COMPLEX SUM                                                       
C SPARSE BACK SOLVE                                                     
      NP1 = N+1                                                         
      DO  5 K = 1, NB                                                   
         DO  3 IIB = 1, N                                               
            I = NP1-IIB                                                 
            JMIN = IA(I)                                                
            JMAX = IA(I+1)-1                                            
            SUM = B(I, K)                                               
            IF (JMIN .GT. JMAX) GOTO 2                                  
               DO  1 J = JMIN, JMAX                                     
                  JUJ = JA(J)                                           
                  SUM = SUM-A(J)*TMP(JUJ)                               
   1              CONTINUE                                              
   2        IIC = C(I)                                                  
            TMP(IIC) = SUM                                              
   3        CONTINUE                                                    
         DO  4 I = 1, N                                                 
            B(I, K) = TMP(I)                                            
   4        CONTINUE                                                    
   5     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE CSPFLE(N, ORDER, AROW, ISIZE, B, IB, NB)               
      INTEGER IB, NB                                                    
      EXTERNAL AROW, I4YX                                               
      INTEGER N, ISIZE                                                  
      COMPLEX B(IB, NB)                                                 
      LOGICAL ORDER                                                     
      DOUBLE PRECISION DSTAK(500)                                       
      COMMON /CSTAK/ DSTAK                                              
      INTEGER IIA, IMC, MCP, MAX, IEX, IMR                              
      INTEGER JLU, MRP, NUM, NUMBER, ISTKGT, ISTKQU                     
      INTEGER I, K, L, ILIN, IERR, ISUB                                 
      INTEGER TEMP, LAST, TEMP1, IA, IC, IHEAD                          
      INTEGER II(1000), JJ, LL, IT, IV, IU                              
      INTEGER LU, IZ, NU, ILEFT, IC1                                    
      REAL BEF, EPS,  AMAX1, BE                                         
      COMPLEX D(500)                                                    
      REAL CABS1                                                        
      REAL R1MACH                                                       
      LOGICAL MOVE                                                      
      EQUIVALENCE (D(1), II(1), DSTAK(1))                               
C THIS SUBROUTINE SOLVES AX = B WHERE A IS A SPARSE MATRIX              
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C ORDER   LOGICAL VARIABLE. IF .TRUE. REORDERING FOR                    
C         SPARSITY WILL BE PERFORMED.                                   
C AROW    SUBROUTINE OF THE FORM GETA(I,ROW,JCOL,NUM) WHICH             
C         FOR A GIVEN INPUT I RETURNS THE NONZERO ELEMENTS OF           
C         THE ITH ROW OF THE MATRIX A IN THE                            
C         COMPLEX VECTOR ROW AND THE CORRESPONDING INDICES IN           
C         JCOL. THE VARIABLE NUM RETURNS THE NUMBER OF NONZERO          
C         ELEMENTS IN THE ITH ROW. AROW SHOULD BE DECLARED              
C         EXTERNAL IN THE CALLING PROGRAM.                              
C B      RIGHT HAND SIDE MATRIX,DESTROYED ON OUTPUT                     
C IB     ROW DIMENSION OF B                                             
C NB     NUMBER OF COLUMNSOF B                                          
C OUTPUT PARAMETERS                                                     
C B      THE SOLUTION TO AX=B                                           
C ISIZE  ACTUAL NUMBER OF ELEMENTS IN THE A ARRAY THAT WAS NEEDED       
C        FOR THE DECOMPOSITION                                          
C SPACE ALLOCATED AND DEALLOCATED-3N+2 INTEGER AND N LE                 
C COMPLEX LOCATIONS PLUS ISIZE INTEGER AND                              
C COMPLEX LOCATIONS NEED TO HOLD THE U FROM THE                         
C LU DECOMPOSITION OF A                                                 
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C 2 IB .LT. N                                                           
C 3 NB .LT. 1                                                           
C 10+K     NULL ROW           FATAL                                     
C 3N+K+10   SINGULAR MATRIX OF RANK K      RECOVERABLE                  
C 2N+K+10  RAN OUT OF SPACE WHEN PROCEESING ROW K                       
C N+K+10 INCORRECT COLUMN INDEX                                         
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HCSPFLE-N.LT.1, 13, 1, 2)             
C     IF (IB .LT. N) CALL SETERR(14HCSPFLE-IB.LT.N, 14, 3, 2)           
C     IF (NB .LT. 1) CALL SETERR(14HCSPFLE-NB.LT.1, 14, 4, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('CSPFLE-N.LT.1', 13, 1, 2)              
      IF (IB .LT. N) CALL SETERR('CSPFLE-IB.LT.N', 14, 3, 2)            
      IF (NB .LT. 1) CALL SETERR('CSPFLE-NB.LT.1', 14, 4, 2)            
C/                                                                      
      CALL ENTER(1)                                                     
      IA = ISTKGT(N+1, 2)                                               
      IIA = IA                                                          
      II(IA) = 1                                                        
C GET SPACE FOR THE COLUMN PERMUTATION VECTOR AND ITS INVERSE           
      IC = ISTKGT(2*N, 2)                                               
      IC1 = IC-1                                                        
      ILEFT = ISTKQU(2)-10                                              
      ISUB = 2*N+2                                                      
      IF (ORDER) ISUB = 3*N+2                                           
      ILIN = ((ILEFT-ISUB)*II(7))/(II(7)+II(10))                        
C GET SPACE FOR A AND LU AND DIVIDE IT BETWEEN COMPLEX AND INTEGER      
      JLU = ISTKGT(ILIN, 2)                                             
      LU = ISTKGT(ILIN, 5)                                              
      LL = LU                                                           
      JJ = JLU                                                          
      NU = ILIN                                                         
      MCP = IC+N                                                        
      IMC = MCP-1                                                       
C READ IN THE MATRIX AND COMPUTE ITS NORM                               
      BEF = 0.0                                                         
      DO  2 I = 1, N                                                    
         CALL AROW(I, D(LL), II(JJ), NUM)                               
         BE = 0.0                                                       
         L = LL                                                         
         IERR = I+10                                                    
         IF (NUM .LT. 1) GOTO  12                                       
         DO  1 K = 1, NUM                                               
            BE = BE+CABS1(D(L))                                         
            L = L+1                                                     
   1        CONTINUE                                                    
         BEF = AMAX1(BE, BEF)                                           
         IIA = IIA+1                                                    
         II(IIA) = II(IIA-1)+NUM                                        
         JJ = JJ+NUM                                                    
         LL = LL+NUM                                                    
         NU = NU-NUM                                                    
         IF (NU .LT. N) GOTO  11                                        
         TEMP1 = I+IC1                                                  
         IMC = IMC+1                                                    
         II(IMC) = I                                                    
         II(TEMP1) = I                                                  
   2     CONTINUE                                                       
      IF (.NOT. ORDER) GOTO 6                                           
         IERR=2*N+11                                                    
         IF (NU .LE. 4*II(IIA)-N) GOTO 3                                
            MOVE = .FALSE.                                              
C GENERATE COLUMN ORDERING WHEN PIVOTING FOR SPARSITY                   
C TEST IF HAVE ENOUGHCSPACE INBETWEEN PRESENT COLUMN INDICES            
C AND NONZERO ELEMENTS                                                  
            GOTO  4                                                     
   3        MOVE = .TRUE.                                               
C DO NOT HAVE ENOUGHCSPACE AND REPACKING IS NECESSARY                   
            CALL ISTKRL(1)                                              
            NUMBER = II(IIA)                                            
            JLU= ISTKMD(NUMBER)                                         
            IT = ISTKGT(NUMBER, 5)                                      
C MOVE COMPLEX ELEMENTS FROM HALFWAY IN STACK UNTIL THE END             
            CALL MOVEFC(NUMBER, D(LU), D(IT))                           
            NU = ISTKQU(2)-10                                           
            JJ = ISTKGT(NU, 2)                                          
   4     MAX = (NU-N)/2                                                 
         IV = JJ                                                        
         L = IV+MAX                                                     
         IHEAD = L+MAX                                                  
         IF (MAX .LT. N) GOTO  11                                       
         IERR = 0                                                       
         CALL S4MDM(N, I4YX, MAX, II(IV), II(L), II(IHEAD),             
     1      II(MCP), II(IC), II(IV), IERR,II(IA),II(JLU),1)             
         IF (IERR .GT. 2*N+10) GOTO  11                                 
C/6S                                                                    
C        IF (IERR .GT. N+10) CALL SETERR(                               
C    1      29HCSPFLE-INCORRECT COLUMN INDEX, 29, IERR, 2)              
C/7S                                                                    
         IF (IERR .GT. N+10) CALL SETERR(                               
     1      'CSPFLE-INCORRECT COLUMN INDEX', 29, IERR, 2)               
C/                                                                      
         IF (.NOT. MOVE) GOTO 5                                         
            CALL ISTKRL(2)                                              
C PUT THINGS BACK WITH NUMERICAL ELEMENTS IN THE MIDDLE OF THE          
C AVAILABLE SPACE                                                       
            JLU= ISTKMD(ILIN)                                           
            LU = ISTKGT(ILIN, 5)                                        
            CALL MOVEBC(NUMBER, D(IT), D(LU))                           
   5     CONTINUE                                                       
   6  IEX = JLU+ILIN-N                                                  
      IZ = LU+ILIN-N                                                    
      ILIN = ILIN-N                                                     
      IU = ISTKGT(ISUB, 2)                                              
      TEMP = IU+N+1                                                     
      MRP = 1                                                           
      IF(.NOT.ORDER)GO TO 77                                            
C PUT ROW ORDERING TO COLUMN ORDERING                                   
      MRP = TEMP+N+1                                                    
      IMR = MRP                                                         
      IMC = MCP                                                         
      DO  7 I = 1, N                                                    
         II(IMR) = II(IMC)                                              
         IMC = IMC+1                                                    
         IMR = IMR+1                                                    
   7     CONTINUE                                                       
   77    CONTINUE                                                       
      EPS = BEF*R1MACH(4)                                               
      CALL CS4FLE(N,II(IA), II(JLU), D(LU), ILIN, II(IU), II(TEMP), D(  
     1   IZ), II(IEX), B, IB, NB, II(IC), II(MRP), II(MCP), ORDER, IERR,
     1   0.1, LAST, EPS)                                                
      IF (IERR .NE. 0) GOTO 8                                           
         ISIZE = LAST-1                                                 
         CALL CS4FBS(N,B, IB, NB, II(JLU), D(LU), II(IA), II(IU), II(   
     1      IEX), II(MCP), II(MRP), ORDER, D(IZ))                       
         GOTO  13                                                       
   8     IF (IERR .LE. N+10) GOTO  12                                   
         IF (IERR .GT. 2*N+10) GOTO 9                                   
C/6S                                                                    
C           CALL SETERR(22HCSPFLE-SINGULAR MATRIX, 22, IERR, 1)         
C/7S                                                                    
            CALL SETERR('CSPFLE-SINGULAR MATRIX', 22, IERR, 1)          
C/                                                                      
            GOTO  13                                                    
   9     CONTINUE                                                       
C/6S                                                                    
C 11  CALL SETERR(25HCSPFLE-INSUFFICIENT SPACE, 25, IERR, 1)            
C/7S                                                                    
  11  CALL SETERR('CSPFLE-INSUFFICIENT SPACE', 25, IERR, 1)             
C/                                                                      
      GOTO  13                                                          
C/6S                                                                    
C 12  CALL SETERR(15HCSPFLE-NULL ROW, 15, IERR, 1)                      
C/7S                                                                    
  12  CALL SETERR('CSPFLE-NULL ROW', 15, IERR, 1)                       
C/                                                                      
  13  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE CS4FLE(N, IA, JA, A, IAMAX, IU, IROW, Z, IEX, B,       
     1   IB, NB, IC, R, C, ORDER, IERR, THRESH, LAST, EPS)              
      INTEGER N, IB, NB                                                 
      INTEGER IA(1), JA(1), IAMAX, IU(N), IROW(N), IEX(N)               
      INTEGER IC(N), R(N), C(N), IERR, LAST                             
      REAL THRESH, EPS                                                  
      COMPLEX A(1), Z(N), B(IB, NB)                                     
      LOGICAL ORDER                                                     
      INTEGER ICC, JUJ, JLU, NUU, MIN0, I                               
      INTEGER J, K, M, CMAX, JMIN, IMAX                                 
      INTEGER JMAX, CI, CK, NC, II, JJ                                  
      INTEGER IJ, RI, RK, IR, MM, JAMIN                                 
      INTEGER JAMAX, NU, ISPAC, IUEND, LASTA, NP1                       
      INTEGER JAMAX1                                                    
      REAL CABS1,PVT, PMAX                                              
      COMPLEX AKI,DK                                                    
      LOGICAL TEMP                                                      
C THIS IS LOWER LEVEL OF SFLE                                           
C PARAMETERS NOT DEFINED IN SFLE                                        
C IAMAX- SPACE LEFT IN STACK TO WORK IN                                 
C IA - POINTS TO BEGINNING OF EACH ROW IN A AND JA                      
C JA COLUMN INDICES NONZERO ELEMENTS                                    
C A NUMERICAL NONZERO ELEMENTS                                          
C IU END OF U PORTION OF THE ROW- INTEGER VECTOR OF LENGTH N            
C IROW  INTEGER SCRATCH VECTOR LENGTH N                                 
C Z COMPLEX SCRATCH VECTOR LENGTH N                                     
C IEX - INTEGER SCRATCH VECTOR LENGTH N POINTING TO EXTRA SPACE         
C IC INTGER VECTOR LENGTH N OF INVERSE COLUMN ORDER                     
C C INTEGER VECTOR LENGTH N OF COLUMN ORDER                             
C R IF ORDERING USED AN INTEGER VECTOR OF LENGTH N GIVING ROW ORDER     
C IERR -ERROR PARAMETER                                                 
C THRESH-THRESHHOLD PARAMETER                                           
C LAST- HOW MUCHCSPACE ACTUALLY USED                                    
C EPS CRITERIA FOR SINGULARITY                                          
C INITIALIZATION                                                        
C REARRANGE RIGHT HAND SIDES                                            
      IF (.NOT. ORDER) GOTO 4                                           
         DO  3 J = 1, NB                                                
            DO  1 I = 1, N                                              
               IR = R(I)                                                
               Z(I) = B(IR, J)                                          
   1           CONTINUE                                                 
            DO  2 I = 1, N                                              
               B(I, J) = Z(I)                                           
   2           CONTINUE                                                 
   3        CONTINUE                                                    
   4  LAST = IA(N+1)                                                    
      LASTA = LAST                                                      
      NP1 = N+1                                                         
      IERR = 0                                                          
      DO  5 I = 1, N                                                    
         IROW(I) = 0                                                    
   5     CONTINUE                                                       
C DETERMINE NEXT ROW OF L AND U                                         
      DO  32 K = 1, N                                                   
         NC = C(K)                                                      
         Z(NC)= CMPLX(0.0E0,0.0E0)                                      
         IEX(K) = LAST                                                  
         RK = K                                                         
         IF (ORDER) RK = R(K)                                           
         M = NP1                                                        
         JAMIN = IA(RK)                                                 
         IROW(NP1) = NP1                                                
         IUEND = NP1                                                    
         JAMAX1 = IA(RK+1)                                              
         JAMAX = JAMAX1-1                                               
         DO  10 J = JAMIN, JAMAX                                        
            JJ = JA(J)                                                  
            Z(JJ) = A(J)                                                
            TEMP = JJ .LT. 1                                            
            IF (.NOT. TEMP) TEMP = JJ .GT. N                            
C/6S                                                                    
C           IF (TEMP) CALL SETERR(29HCSPFLE-INCORRECT COLUMN INDEX, 29  
C    1         , K+10+N, 2)                                             
C/7S                                                                    
            IF (TEMP) CALL SETERR('CSPFLE-INCORRECT COLUMN INDEX', 29   
     1         , K+10+N, 2)                                             
C/                                                                      
            ICC = IC(JJ)                                                
            IF (ICC .LT. K) GOTO 6                                      
               IROW(ICC) = IUEND                                        
               IUEND = ICC                                              
               GOTO  9                                                  
   6           IF (M .GT. ICC) M = NP1                                  
C NEW ELEMENT IS IN L PART OF THE ROW                                   
   7              II = IROW(M)                                          
                  IF (II .GE. ICC) GOTO  8                              
                  M = II                                                
                  GOTO  7                                               
   8           IROW(M) = ICC                                            
               IROW(ICC) = II                                           
   9        CONTINUE                                                    
  10        CONTINUE                                                    
C                                                                       
C ELIMINATE ROW                                                         
         I = NP1                                                        
  11        J = I                                                       
            I = IROW(I)                                                 
            IROW(J) = 0                                                 
            IF (I .EQ. NP1) GOTO  23                                    
            RI = I                                                      
            IF (ORDER) RI = R(I)                                        
            JMIN = IA(RI)                                               
            JMAX = MIN0(IA(RI+1)-1, IU(I))                              
            CI = C(I)                                                   
            AKI = -Z(CI)                                                
C FORWARD SOLVE                                                         
            DO  12 MM = 1, NB                                           
               B(K, MM) = B(K, MM)+AKI*B(I, MM)                         
  12           CONTINUE                                                 
            IF (JMAX .LT. JMIN) GOTO 22                                 
  13              DO  19 J = JMIN, JMAX                                 
C ELIMINATE ITH ELEMENT                                                 
                     JUJ = JA(J)                                        
                     ICC = IC(JUJ)                                      
                     IF (IROW(ICC) .NE. 0) GOTO 18                      
                        IF (ICC .LT. K) GOTO 14                         
                           IROW(ICC) = IUEND                            
C FILL IN-SEE IF IT IS IN U OR L                                        
                           IUEND = ICC                                  
                           GOTO  17                                     
  14                       TEMP = M .GT. ICC                            
                           IF (.NOT. TEMP) TEMP = M .LT. I              
                           IF (TEMP) M = I                              
  15                          IJ = IROW(M)                              
                              IF (IJ .GE. ICC) GOTO  16                 
                              M = IJ                                    
                              GOTO  15                                  
  16                       IROW(M) = ICC                                
                           IROW(ICC) = IJ                               
  17                    Z(JUJ) = (0.E0,0.E0)                            
  18                 Z(JUJ) = Z(JUJ)+AKI*A(J)                           
  19                 CONTINUE                                           
                  IF (JMAX .EQ. IU(I)) GOTO  21                         
                  JMIN = IEX(I)                                         
                  JMAX = IU(I)                                          
  20              IF (JMIN .LE. JMAX) GOTO  13                          
  21        CONTINUE                                                    
  22        CONTINUE                                                    
            GOTO  11                                                    
  23     I = IUEND                                                      
         PMAX = 0.E0                                                    
         NU = -1                                                        
  24     IF (I .EQ. NP1) GOTO  26                                       
            CI = C(I)                                                   
            PVT = CABS1(Z(CI))                                          
            NU = NU+1                                                   
            IF (PVT .LE. PMAX) GOTO 25                                  
               IMAX = I                                                 
               PMAX = PVT                                               
  25        I = IROW(I)                                                 
            GOTO  24                                                    
C DO THRESHHOLD PIVOTING                                                
  26     IF (CABS1(Z(NC)) .GE. THRESH*PMAX) IMAX = K                    
         CMAX = C(IMAX)                                                 
C CHECK FOR SINGULARITY                                                 
         IF (CABS1(Z(CMAX)) .LE. EPS) GOTO  33                          
         ISPAC = JAMAX-JAMIN+1                                          
C SEE IF SUFFICIENT SPACE AVAILABLE                                     
         IF (NU-ISPAC+LAST .GE. IAMAX) GOTO  34                         
         NUU = JAMIN+NU-1                                               
         IF (NUU .GT. JAMAX) NUU = LAST+NU-ISPAC-1                      
         IU(K) = NUU                                                    
         JLU = NUU+1                                                    
         DK = (1.E0,0.0)/Z(CMAX)                                        
C FIX UP RIGHT HAND SIDE                                                
         DO  27 MM = 1, NB                                              
            B(K, MM) = B(K, MM)*DK                                      
  27        CONTINUE                                                    
         I = IUEND                                                      
  28     IF (I .EQ. NP1) GOTO  30                                       
            IF (I .EQ. IMAX) GOTO 29                                    
               CI = C(I)                                                
C STORE ELEMENT OF U                                                    
               A(NUU) = Z(CI)*DK                                        
               JA(NUU) = CI                                             
               IF (NUU .EQ. LAST) NUU = JAMAX1                          
               NUU = NUU-1                                              
  29        II = I                                                      
            I = IROW(I)                                                 
            IROW(II) = 0                                                
            GOTO  28                                                    
  30     IF (JLU .GT. LAST) LAST = JLU                                  
C INTERCHANGE THE COLUMN INDICES                                        
         IF (K .EQ. IMAX) GOTO 31                                       
            JJ = C(K)                                                   
            C(K) = CMAX                                                 
            C(IMAX) = JJ                                                
            CK = C(K)                                                   
            IC(CK) = K                                                  
            IC(JJ) = IMAX                                               
  31     CONTINUE                                                       
  32     CONTINUE                                                       
      JA(LAST) = LAST                                                   
      RETURN                                                            
  33  IERR = 3*N+9+K                                                    
      RETURN                                                            
  34  IERR = 2*N+K+10                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE CS4FBS(N, B, IB, NB, JA, A, IA, IU, IEX, C, R,         
     1   ORDER, TMP)                                                    
      INTEGER N, IB, NB                                                 
      INTEGER JA(1), IA(1), IU(N), IEX(N), C(N), R(N)                   
      COMPLEX B(IB, NB), A(1), TMP(N)                                   
      LOGICAL ORDER                                                     
      INTEGER IIB, IIC, JUJ, MIN0, I, J                                 
      INTEGER K, JMIN, JMAX, RI, NP1                                    
      COMPLEX SUM                                                       
      NP1 = N+1                                                         
      DO  8 K = 1, NB                                                   
C SPARSE BACK SOLVE                                                     
         DO  6 IIB = 1, N                                               
            I = NP1-IIB                                                 
            RI = I                                                      
            IF (ORDER) RI = R(I)                                        
            JMIN = IA(RI)                                               
            JMAX = MIN0(IA(RI+1)-1, IU(I))                              
            SUM = B(I, K)                                               
            IF (JMIN .GT. JMAX) GOTO 5                                  
   1              DO  2 J = JMIN, JMAX                                  
                     JUJ = JA(J)                                        
                     SUM = SUM-A(J)*TMP(JUJ)                            
   2                 CONTINUE                                           
                  IF (JMAX .EQ. IU(I)) GOTO  4                          
                  JMIN = IEX(I)                                         
                  JMAX = IU(I)                                          
   3              IF (JMIN .LE. JMAX) GOTO  1                           
   4        CONTINUE                                                    
   5        IIC = C(I)                                                  
            TMP(IIC) = SUM                                              
   6        CONTINUE                                                    
         DO  7 I = 1, N                                                 
            B(I, K) = TMP(I)                                            
   7        CONTINUE                                                    
   8     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE CSPMCE(N, R, C, A, IA, JA, IAMAX, IL, ISIZE, COND,     
     1   Z)                                                             
      INTEGER N                                                         
      INTEGER R(N), C(N), IA(1), JA(1), IAMAX, IL(N)                    
      INTEGER ISIZE                                                     
      COMPLEX A(1),  Z(N)                                               
      REAL COND                                                         
      DOUBLE PRECISION D(500)                                           
      COMMON /CSTAK/ D                                                  
      INTEGER ICI, ISTKGT, I, J, JMIN, JMAX                             
      INTEGER IERR, ITMP, IC, II(1000), IC1                             
      REAL SUM, GROWTH, SUM1                                            
      COMPLEX RSTK(500)                                                 
      LOGICAL TEMP                                                      
      EQUIVALENCE (D(1), II(1))                                         
      EQUIVALENCE (D(1), RSTK(1))                                       
C                                                                       
C SPARSE CONDITION ESTIMATOR                                            
C                                                                       
C INPUT PARAMETERS                                                      
C                                                                       
C N         NUMBER OF EQUATIONS                                         
C R         INTEGER VECTOR GIVING ROW PERMUTATIONS                      
C IA        INTEGER VECTOR OF LENGTH N+1 POINTING TO BEGINING OF        
C           EACH ROW IN THE A ARRAY                                     
C JA        INTEGER VECTOR GIVING COLUMN INDEX OF EACH ELEMENT          
C           IN THE A ARRAY                                              
C A         COMPLEX ARRAY INTO WHICH THE NONZERO ELEMENTS               
C           OF A ARE PACKED BY ROWS                                     
C IAMAX     TOTAL SIZE OF THE A ARRAY,MUST BE LARGE ENOUGH TO           
C           HOLD LU DECOMPOSITION OF A                                  
C OUTPUT PARAMETERS                                                     
C C         REORDERED COLUMNS                                           
C JA        COLUMN INDICES OF LU DECOMPOSITION OF A                     
C A         LU DECOMPOSITION OF A                                       
C IU        INTEGER VECTOR OF LENGTH N+1 STATING WHERE EACH ROW         
C           OF U BEGINS IN THE A AND JA ARRAYS                          
C COND      ESTIMATE OF THE CONDITION NUMBER OF A                       
C Z         COMPLEX VECTOR LENGTH N, GIVING APPROXIMATE NULL            
C           VECTOR                                                      
C SPACE ALLOCATED 2N+1 INTEGER LOCATIONS  AND N COMPLEX LOCATIONS       
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(12HCSPCE-N.LT.1, 12, 1, 2)              
C     IF (IAMAX .LT. IA(N+1)-1) CALL SETERR(24HCSPCE-INSUFFICIENT SPACE,
C    1   24, 3, 2)                                                      
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('CSPCE-N.LT.1', 12, 1, 2)               
      IF (IAMAX .LT. IA(N+1)-1) CALL SETERR('CSPCE-INSUFFICIENT SPACE', 
     1   24, 3, 2)                                                      
C/                                                                      
      CALL ENTER(1)                                                     
      IC = ISTKGT(2*N+1, 2)                                             
      SUM1 = 0.E0                                                       
      ITMP = IC+N                                                       
      IC1 = IC-1                                                        
C FIND NORM OF MATRIX AND CHECK ROW PERMUTATIONS                        
C SET COLUMN PERMUTATION TO INITIALLY NO COLUMN INTERCHANGES.           
      DO  2 I = 1, N                                                    
         SUM = 0.E0                                                     
         TEMP = R(I) .LT. 1                                             
         IF (.NOT. TEMP) TEMP = R(I) .GT. N                             
C/6S                                                                    
C        IF (TEMP) CALL SETERR(21HCSPCE-R  OUT OF RANGE, 21, 2, 2)      
C/7S                                                                    
         IF (TEMP) CALL SETERR('CSPCE-R  OUT OF RANGE', 21, 2, 2)       
C/                                                                      
         ICI = IC1+I                                                    
         C(I) = I                                                       
         II(ICI) = I                                                    
         JMIN = IA(I)                                                   
         JMAX = IA(I+1)-1                                               
         DO  1 J = JMIN, JMAX                                           
            SUM = SUM+CABS1(A(J))                                       
   1        CONTINUE                                                    
         IF (SUM .GT. SUM1) SUM1 = SUM                                  
   2     CONTINUE                                                       
      CALL CS4MLU(N, IA, JA, A, IAMAX, IL, II(ITMP), Z, II(IC), R, C,   
     1   IERR, 1.E0, 0.E0, ISIZE, GROWTH)                               
      IF (IERR .EQ. 0) GOTO 6                                           
         IF (IERR .GT. N+10) GOTO 3                                     
C/6S                                                                    
C           CALL SETERR(15HCSPMCE-NULL ROW, 15, IERR, 2)                
C/7S                                                                    
            CALL SETERR('CSPMCE-NULL ROW', 15, IERR, 2)                 
C/                                                                      
            GOTO  7                                                     
C/6S                                                                    
C  3        IF (IERR .LE. 2*N+10) CALL SETERR(                          
C    1         29HCSPMCE-INCORRECT COLUMN INDEX, 29, IERR, 2)           
C/7S                                                                    
   3        IF (IERR .LE. 2*N+10) CALL SETERR(                          
     1         'CSPMCE-INCORRECT COLUMN INDEX', 29, IERR, 2)            
C/                                                                      
            IF (IERR .LE. 3*N+10) GOTO 4                                
C/6S                                                                    
C              CALL SETERR(25HCSPMCE-INSUFFICIENT SPACE, 25, IERR, 1)   
C/7S                                                                    
               CALL SETERR('CSPMCE-INSUFFICIENT SPACE', 25, IERR, 1)    
C/                                                                      
               GOTO  7                                                  
   4        CONTINUE                                                    
   6  CALL ISTKRL(1)                                                    
      ITMP = ISTKGT(N, 5)                                               
      CALL SC4MCE(N, R, C, A, IA, JA, IL, SUM1, COND, Z, RSTK(ITMP))    
C/6S                                                                    
C     IF (IERR .NE. 0) CALL SETERR(22HCSPMCE-SINGULAR MATRIX, 22, IERR  
C    1   , 1)                                                           
C/7S                                                                    
      IF (IERR .NE. 0) CALL SETERR('CSPMCE-SINGULAR MATRIX', 22, IERR   
     1   , 1)                                                           
C/                                                                      
   7  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE SC4MCE(N, R, C, A, IA, JA, IL, ANORM, COND, Z,         
     1   TMP)                                                           
      INTEGER N                                                         
      INTEGER R(N), C(N), IA(1), JA(1), IL(1)                           
      COMPLEX A(N),  Z(N), TMP(N)                                       
      REAL ANORM, COND                                                  
      INTEGER ICK, IDI, IEX, JUJ, IDI1, MIN0                            
      INTEGER I, J, K, JMIN, JMAX, JMIN1                                
      INTEGER JMAX1, IB, JJ, IR, KR, LASTA                              
      INTEGER ISIZE, NP1                                                
      REAL BIG, SUM, S                                                  
      COMPLEX WKM, WK, EK,CSIGN1, T, SUMC, Z1, Z2                       
      REAL  AMAX1,  SM,  GREAT                                          
      REAL FLOAT, SCASUM, YNORM, R1MACH                                 
      LOGICAL TEMP                                                      
      CSIGN1(Z1,Z2)=CABS1(Z1)*(Z1/CABS1(Z2))                            
C THIS IS LOWER LEVEL CONDITION ESTIMATOR FOR CSPMCE                    
C PARAMETERS ARE THE SAME AS IN CSPMCE EXCEPT THAT                      
C ANORM -NORM OF MATRIX                                                 
C TMP - N VECTOR REAL TEMPORARY                                         
C A,JA - ALREADY CONTAIN LU DECOMPOSITION COMPUTED BY SC4MLU.           
      IF (N .NE. 1) GOTO 1                                              
         COND = 1.0                                                     
         Z(1) = (1.0,0.0)                                               
         GOTO  24                                                       
   1     EK = (1.0E0,0.0E0)                                             
         ISIZE = IA(N+1)-1                                              
         DO  2 J = 1, N                                                 
            Z(J) = (0.0E0,0.0E0)                                        
   2        CONTINUE                                                    
         BIG = R1MACH(2)/FLOAT(N)                                       
C                                                                       
C SOLVE TRANS(U) W =E AND PUT ANSWER IN Z                               
         LASTA = IA(N+1)                                                
         DO  14 K = 1, N                                                
            ICK = C(K)                                                  
            IF (CABS1(Z(ICK)) .NE. 0.E0) EK = CSIGN1(EK, -Z(ICK))       
            IF (CABS1(EK-Z(ICK)) .LE. 1.E0) GOTO 3                      
               S = 1.E0/CABS1(EK-Z(ICK))                                
               CALL CSSCAL(N, S, Z, 1)                                  
               EK = CMPLX(S, 0.0E0)*EK                                  
   3        WK = EK-Z(ICK)                                              
            WKM = (-EK)-Z(ICK)                                          
            S = CABS1(WK)                                               
            SM = CABS1(WKM)                                             
            KR = R(K)                                                   
C IN LU DECOMPOSITION, THE U PORTION OF THE ROW                         
C COMES BEFOR THE L  PORTION BUT IT COULD                               
C BE SPLIT IN 2.                                                        
            JMIN = IA(KR)                                               
            IDI = IL(K)                                                 
            JMAX = MIN0(IA(KR+1), IDI)-1                                
            IF (JMIN .GT. JMAX) GOTO 13                                 
               JMIN1 = JMIN                                             
               JMAX1 = JMAX                                             
   4              DO  5 J = JMIN, JMAX                                  
                     JUJ = JA(J)                                        
                     SM = SM+CABS1(Z(JUJ)+WKM*CONJG(A(J)))              
                     Z(JUJ) = Z(JUJ)+WK*CONJG(A(J))                     
                     S = S+CABS1(Z(JUJ))                                
   5                 CONTINUE                                           
                  TEMP = IDI .LT. LASTA                                 
                  IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA                
                  IF (TEMP) GOTO  7                                     
                  JMIN = JA(IDI)                                        
                  JMAX = IDI-1                                          
   6              IF (JMIN .LE. JMAX) GOTO  4                           
   7           IF (S .GE. SM) GOTO 12                                   
                  T = WKM-WK                                            
   8                 DO  9 J = JMIN1, JMAX1                             
                        JUJ = JA(J)                                     
                        Z(JUJ) = Z(JUJ)+T*CONJG(A(J))                   
   9                    CONTINUE                                        
                     TEMP = IDI .LT. LASTA                              
                     IF (.NOT. TEMP) TEMP = JMIN1 .GE. LASTA            
                     IF (TEMP) GOTO  11                                 
                     JMIN1 = JA(IDI)                                    
                     JMAX1 = IDI-1                                      
  10                 IF (JMIN1 .LE. JMAX1) GOTO  8                      
  11              CONTINUE                                              
  12           CONTINUE                                                 
  13        IF (S .LT. SM) WK = WKM                                     
            Z(ICK) = WK                                                 
  14        CONTINUE                                                    
         S = 1.0/SCASUM(N, Z, 1)                                        
         CALL CSSCAL(N, S, Z, 1)                                        
C                                                                       
C FORM Y=L(TRANSPOSE)*W                                                 
C AND PUT RESULT BACK INTO Z                                            
C                                                                       
         NP1 = N+1                                                      
         DO  21 IB = 1, N                                               
            I = NP1-IB                                                  
            IR = R(I)                                                   
            IDI = IL(I)                                                 
C A(IDI) HAS RECIPROCAL OF DIAGONAL                                     
            IDI1 = IL(I+1)                                              
            JMIN = IDI+1                                                
            IEX = JA(IDI1)-1                                            
            IF (JMIN .EQ. IA(IR+1)) JMIN = JA(IDI)                      
            JMAX = IA(IR+1)-1                                           
            IF (JMAX .LT. JMIN) JMAX = IEX                              
C TO AVOID OVERFLOW SCALE                                               
            SUM = AMAX1(CABS1(A(IDI)), CABS1(Z(I)))                     
            IF (CABS1(Z(I)) .LE. 1.0E0) SUM = CABS1(CONJG(A(IDI))*Z(I)) 
            S = 1.0E0                                                   
            IF (SUM .LE. 1.E0) GOTO 15                                  
               S = 1.0/SUM                                              
               CALL CSSCAL(N, S, Z, 1)                                  
               IF (S .EQ. 0.0E0) Z(I) = (1.0E0,0.0E0)                   
  15        IF (S .NE. 0.0E0) Z(I) = Z(I)*CONJG(A(IDI))                 
            IF (JMIN .GT. JMAX) GOTO 20                                 
               SUMC = Z(I)                                              
  16              DO  17 J = JMIN, JMAX                                 
                     JJ = JA(J)                                         
                     Z(JJ) = Z(JJ)+SUMC*CONJG(A(J))                     
  17                 CONTINUE                                           
                  IF (JMAX .EQ. IEX) GOTO  19                           
                  JMIN = JA(IDI)                                        
                  JMAX = IEX                                            
  18              IF (JMIN .LE. JMAX) GOTO  16                          
  19           CONTINUE                                                 
  20        CONTINUE                                                    
  21        CONTINUE                                                    
C                                                                       
C PUT NORM OF Z TO 1.0                                                  
C                                                                       
         S = 1.E0/SCASUM(N, Z, 1)                                       
         YNORM = 1.0                                                    
         CALL CSSCAL(N, S, Z, 1)                                        
C DO FORWARD AND BACK SOLVE                                             
         CALL CS4MFB(N, R, C, IA, JA, A, IL, Z, TMP, YNORM)             
         S = 1.0/SCASUM(N, Z, 1)                                        
         YNORM = YNORM*S                                                
         GREAT = R1MACH(2)                                              
         IF (YNORM .GE. 1.0) GOTO 23                                    
            TEMP = ANORM .EQ. 0.0E0                                     
            IF (.NOT. TEMP) TEMP = ANORM .GT. YNORM*GREAT               
            IF (.NOT. TEMP) GOTO 22                                     
               COND = GREAT                                             
               RETURN                                                   
  22     CONTINUE                                                       
  23     COND = ANORM/YNORM                                             
  24  RETURN                                                            
      END                                                               
      SUBROUTINE CS4MFB(N, R, C, IA, JA, A, IL, B, Z, YNORM)            
      INTEGER N                                                         
      INTEGER R(N), C(N), IA(1), JA(1), IL(N)                           
      COMPLEX A(1), B(N), Z(N)                                          
      REAL YNORM                                                        
      INTEGER IIB, IIC, IDI, IEX, JUJ, IDI1                             
      INTEGER MIN0, I, J, JMIN, JMAX, JJ                                
      INTEGER IR, RI, LASTA, NP1                                        
      REAL   S,  AMAX1                                                  
      COMPLEX DK,SUM,CSIGN1,Z1, Z2                                      
      REAL SC, SCASUM                                                   
      LOGICAL TEMP                                                      
      CSIGN1(Z1,Z2)=CABS1(Z1)*(Z1/CABS1(Z2))                            
C SPARSE MATRIX SOLUTION                                                
C INPUT                                                                 
C N ORDER OF PROBLEM                                                    
C R ROW PERMUTATION                                                     
C C COLUMN PERMUTATION                                                  
C IA INTEGER VECTOR, LENGTH N+1 POINTING TO BEGINNING OF ROW IN JA AND A
C JA COLUMN INDICES CORRESPONDING TO NONZERO ELEMENTS IN A              
C A  COMPLEX VECTOR OF NONZERO ELEMENTS IN LU DECOMPOSTION              
C IL INTEGER VECTOR LENGTH N+1 POINTING TO BEGINNING OF EACH L ROW      
C    IN A AND JA. COMPUTED BY CSPMLU                                    
C B RIGHT-HAND SIDE                                                     
C OUTPUT                                                                
C Z   SOLUTION TO PROBLEM                                               
      LASTA = IA(N+1)                                                   
      NP1 = N+1                                                         
C SPARSE FORWARD SOLVE                                                  
      DO  5 I = 1, N                                                    
         IR = R(I)                                                      
         IDI = IL(I)                                                    
         IDI1 = IL(I+1)                                                 
         JMIN = IDI+1                                                   
         IEX = JA(IDI1)-1                                               
         IF (JMIN .EQ. IA(IR+1)) JMIN = JA(IDI)                         
         JMAX = IA(IR+1)-1                                              
         IF (JMAX .LT. JMIN) JMAX = IEX                                 
C DK HAS RECIPROCAL OF THE DIAGONAL                                     
         DK = A(IDI)                                                    
         SUM = B(I)                                                     
   1     IF (JMIN .GT. JMAX) GOTO  3                                    
            DO  2 J = JMIN, JMAX                                        
               JJ = JA(J)                                               
               SUM = SUM+A(J)*B(JJ)                                     
   2           CONTINUE                                                 
            IF (JMAX .EQ. IEX) GOTO  3                                  
            JMIN = JA(IDI)                                              
            JMAX = IEX                                                  
            GOTO  1                                                     
C                                                                       
C SCALE THINGS TO AVOID OVERFLOW                                        
C                                                                       
   3     SC = AMAX1(CABS1(SUM), CABS1(DK))                              
         IF (CABS1(SUM) .LE. 1.0) SC = CABS1(SUM*DK)                    
         S = 1.0E0                                                      
         IF (SC .LE. 1.0E0) GOTO 4                                      
            S = 1.0E0/SC                                                
            CALL CSSCAL(N, S, B, 1)                                     
            SUM = SUM*CMPLX(S,0.0E0)                                    
            IF (S .EQ. 0.0E0) SUM = (1.0,0.0)                           
            YNORM = YNORM*S                                             
   4     IF (S .NE. 0.0E0) SUM = SUM*DK                                 
         B(I) = SUM                                                     
   5     CONTINUE                                                       
      S = 1.0/SCASUM(N, B, 1)                                           
      IF (S .GT. 1.0) GOTO 6                                            
         CALL CSSCAL(N, S, B, 1)                                        
         YNORM = S*YNORM                                                
C SPARSE BACK SOLVE                                                     
   6  DO  13 IIB = 1, N                                                 
         I = NP1-IIB                                                    
         RI = R(I)                                                      
         IDI = IL(I)                                                    
         JMIN = IA(RI)                                                  
         JMAX = MIN0(IA(RI+1), IDI)-1                                   
         SUM = B(I)                                                     
         IF (JMIN .GT. JMAX) GOTO 11                                    
   7           DO  8 J = JMIN, JMAX                                     
                  JUJ = JA(J)                                           
                  SUM = SUM-A(J)*Z(JUJ)                                 
   8              CONTINUE                                              
               TEMP = IDI .LT. LASTA                                    
               IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA                   
               IF (TEMP) GOTO  10                                       
               JMIN = JA(IDI)                                           
               JMAX = IDI-1                                             
   9           IF (JMIN .LE. JMAX) GOTO  7                              
  10     CONTINUE                                                       
  11     IIC = C(I)                                                     
         IF (CABS1(SUM) .LE. 1.0E0) GOTO 12                             
            S = 1.0/CABS1(SUM)                                          
            CALL CSSCAL(N, S, Z, 1)                                     
            CALL CSSCAL(I, S, B, 1)                                     
            YNORM = YNORM*S                                             
            Z(IIC) = CSIGN1(CMPLX(1.0,0.0), SUM)                        
            SUM = SUM*S                                                 
  12     Z(IIC) = SUM                                                   
  13     CONTINUE                                                       
      DO  14 I = 1, N                                                   
         B(I) = Z(I)                                                    
  14     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE CSPFCE(N, MRP, MCP, AROW, IWORK, UL, IAMAX, ISIZE,     
     1   COND, Z)                                                       
      INTEGER N, IAMAX                                                  
      EXTERNAL AROW                                                     
      INTEGER MRP(101), MCP(101), IWORK(IAMAX), ISIZE                   
      COMPLEX UL(IAMAX),  Z(N)                                          
      REAL COND                                                         
      DOUBLE PRECISION D(500)                                           
      COMMON /CSTAK/ D                                                  
      INTEGER JLU, ISTKGT, I, IERR, LAST, TEMP                          
      INTEGER TEMP1, IC, II(1000), IC1                                  
      REAL GROWTH,  ANORM                                               
      COMPLEX R(500)                                                    
      LOGICAL TEMP2                                                     
      EQUIVALENCE (R(1), II(1))                                         
      EQUIVALENCE (R(1), D(1))                                          
C THIS SUBROUTINE CALLS THE DECOMPOSITION ROUTINE AND DETERMINES        
C AN ESTIMATE OF THE CONDITION NUMBER OF A COMPLEX SPARSE MATRIX        
C INPUT PARAMETERS                                                      
C    N        ORDER OF THE PROBLEM                                      
C    MRP      N VECTOR GIVING MATRIX ROW PERMUTATIONS                   
C    AROW      USER WRITTEN SUBROUTINE WHICH GIVES THE NONZERO ELEMENTS 
C              OF A SPECIFIED ROW AND THEIR COLUMN INDICES. THE CALLING 
C              SEQUENCE IS AROW(I,ROW,JROW,NUM)                         
C              WHERE I IS THE SPECIFIED ROW, NUM IS THE NUMBER OF       
C              NONZERO ELEMENTS IN THAT RWO, ROW IS A VECTOR OF THESE   
C              ELEMENTS AND JROW IS THE CORRESPONDING COLUMN INDICES.   
C    IAMAX     DECLARED LENGTH OF UL, IWORK SHOULD BE IAMAX+2N+1 LONG.  
C OUTPUT PARAMETERS                                                     
C    MCP      INTEGER N VECTOR CONTAINING COLUMN PERMUTATIONS FOR       
C             STABILITY                                                 
C    IWORK     INTGER VECTOR CONTAINING POINTER INFORMATION FOR LU      
C             DECOMPOSITION                                             
C    UL        NONZERO ELEMENTS OF LU DECOMPOSITION                     
C    ISIZE     ACTUAL NUMBER OF NONZERO ELEMENTS IN DECOMPOSITION       
C    COND     AN ESTIMATE OF THE CONDITION NUMBER OF THE MATRIX A       
C    Z        N VECTOR CONTAINING APPROXIMATE NULL VECTOR               
C EXTRA STORAGE ALLOCATED - N COMPLEX  AND 2N+1 INTEGER LOCATIONS       
C THE SUBROUTINES SCASUM, SPFLU, AND CS4FCE ARE CALLED                  
C ERROR CONDITIONS-                                                     
C    1     N.LT.1       FATAL                                           
C    2    IA.LT.N       FATAL                                           
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HSPFCE-N.LT.1 , 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('SPFCE-N.LT.1 ', 13, 1, 2)              
C/                                                                      
C      COMPUTE NORM OF MATRIX STORED IN COMPACT FORM                    
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HCSPFCE-N.LT.1, 13, 1, 2)             
C     IF (IAMAX .LT. N) CALL SETERR(25HCSPFCE-INSUFFICIENT SPACE, 25, 3,
C    1   2)                                                             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('CSPFCE-N.LT.1', 13, 1, 2)              
      IF (IAMAX .LT. N) CALL SETERR('CSPFCE-INSUFFICIENT SPACE', 25, 3, 
     1   2)                                                             
C/                                                                      
      IC = ISTKGT(2*N+1, 2)                                             
      IC1 = IC-1                                                        
      DO  1 I = 1, N                                                    
         TEMP2 = MRP(I) .LT. 1                                          
         IF (.NOT. TEMP2) TEMP2 = MRP(I) .GT. N                         
C/6S                                                                    
C        IF (TEMP2) CALL SETERR(23HCSPFCE-MRP OUT OF RANGE, 23, 2, 2)   
C/7S                                                                    
         IF (TEMP2) CALL SETERR('CSPFCE-MRP OUT OF RANGE', 23, 2, 2)    
C/                                                                      
         MCP(I) = I                                                     
         TEMP1 = I+IC1                                                  
         II(TEMP1) = I                                                  
   1     CONTINUE                                                       
      TEMP = IC+N                                                       
      JLU = 2*N+2                                                       
      CALL CS4FLU(N,IWORK, IWORK(JLU), UL, IAMAX, IWORK(N+2), II(TEMP)  
     1   , Z, II(IC), MRP, MCP, IERR, 1.0, 0.0, LAST, AROW, GROWTH,     
     1   ANORM)                                                         
      IF (IERR .EQ. 0) GOTO 5                                           
         IF (IERR .GT. N+10) GOTO 2                                     
C/6S                                                                    
C           CALL SETERR(15HCSPFCE-NULL ROW, 15, IERR, 2)                
C/7S                                                                    
            CALL SETERR('CSPFCE-NULL ROW', 15, IERR, 2)                 
C/                                                                      
            GOTO  6                                                     
C/6S                                                                    
C  2        IF (IERR .LE. 2*N+10) CALL SETERR(                          
C    1         29HCSPFCE-INCORRECT COLUMN INDEX, 29, IERR, 2)           
C/7S                                                                    
   2        IF (IERR .LE. 2*N+10) CALL SETERR(                          
     1         'CSPFCE-INCORRECT COLUMN INDEX', 29, IERR, 2)            
C/                                                                      
            IF (IERR .LE. 3*N+10) GOTO 3                                
C/6S                                                                    
C              CALL SETERR(25HCSPFCE-INSUFFICIENT SPACE, 25, IERR, 1)   
C/7S                                                                    
               CALL SETERR('CSPFCE-INSUFFICIENT SPACE', 25, IERR, 1)    
C/                                                                      
               GOTO  6                                                  
   3        CONTINUE                                                    
   5  ISIZE = LAST-1                                                    
      CALL ISTKRL(1)                                                    
      TEMP = ISTKGT(N, 5)                                               
      CALL CS4FCE(N, IWORK, IWORK(JLU), UL, IAMAX, IWORK(N+2), R(       
     1   TEMP), ANORM, COND, Z, MCP)                                    
C/6S                                                                    
C     IF (IERR .NE. 0) CALL SETERR(22HCSPFCE-SINGULAR MATRIX, 22, IERR  
C    1   , 1)                                                           
C/7S                                                                    
      IF (IERR .NE. 0) CALL SETERR('CSPFCE-SINGULAR MATRIX', 22, IERR   
     1   , 1)                                                           
C/                                                                      
   6  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE CS4FCE(N, IA, JA, UL, IAMAX, IU, Z, ANORM              
     1   , COND, TMP, MCP)                                              
      INTEGER N, IAMAX                                                  
      INTEGER IA(1), JA(1), IU(N), MCP(N)                               
      COMPLEX UL(IAMAX), Z(N), TMP(N)                                   
      REAL ANORM, COND                                                  
      INTEGER IIC, ICT, IUI, JUJ, I, J                                  
      INTEGER K, JMIN, JMAX, KB, KC, ID                                 
      INTEGER JJ, KP1, NP1                                              
      REAL BIG, CABS1,  SUM, S                                          
      REAL  SQRT, AMAX1,   SC                                           
      REAL SM,  GREAT, FLOAT, SCASUM, YNORM                             
      COMPLEX WK,WKM, T,CSIGN1, EK, SCUM, DK, Z1, Z2                    
      REAL R1MACH                                                       
      LOGICAL TEMP                                                      
      CSIGN1(Z1,Z2)=CABS1(Z1)*(Z1/CABS1(Z2))                            
C THIS SUBROUTINE DETERMINES A LOWER BOUND ON THE CONDITION NUMBER      
C OF THE DECOMPOSED MATRIX A VIA THE ALGORITHM USED IN LINPACK          
C                                                                       
C                                                                       
C SOLVE A(TRANSPOSE)W = E                                               
C WHERE E IS CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH                       
C IN THE COMPONENTS OF W                                                
      NP1 = N+1                                                         
      ICT = IA(N+1)-1                                                   
      IF (N .NE. 1) GOTO 1                                              
         COND = 1.0                                                     
         Z(1) = (1.0,0.0)                                               
         GOTO  26                                                       
   1     EK = (1.0E0,0.0E0)                                             
C                                                                       
C SOLVE U(TRANS) Y =E                                                   
C                                                                       
         BIG = SQRT(R1MACH(2))/FLOAT(N)                                 
         DO  2 J = 1, N                                                 
            Z(J) = (0.0,0.0)                                            
   2        CONTINUE                                                    
         DO  9 K = 1, N                                                 
            KC = MCP(K)                                                 
C        DIAG=UL(IU(K)-1)                                               
            IF (CABS1(Z(KC)) .NE. 0.0) EK = CSIGN1(EK, -Z(KC))          
            IF (CABS1(EK-Z(KC)) .LE. 1.0) GOTO 3                        
               S = 1.0/CABS1(EK-Z(KC))                                  
               CALL CSSCAL(N, S, Z, 1)                                  
               EK = CMPLX(S,0.0E0)*EK                                   
   3        WK = EK-Z(KC)                                               
            WKM = (-EK)-Z(KC)                                           
            S = CABS1(WK)                                               
            SM = CABS1(WKM)                                             
            KP1 = K+1                                                   
            IF (KP1 .GT. N) GOTO 8                                      
               JMIN = IU(K)                                             
               JMAX = IA(K+1)-1                                         
               IF (JMAX .LT. JMIN) GOTO 7                               
                  DO  4 J = JMIN, JMAX                                  
                     JUJ = JA(J)                                        
                     SM = SM+CABS1(Z(JUJ)+WKM*CONJG(UL(J)))             
                     Z(JUJ) = Z(JUJ)+CONJG(UL(J))*WK                    
                     S = S+CABS1(Z(JUJ))                                
   4                 CONTINUE                                           
                  IF (S .GE. SM) GOTO 6                                 
                     T = WKM-WK                                         
                     WK = WKM                                           
                     DO  5 J = JMIN, JMAX                               
                        JUJ = JA(J)                                     
                        Z(JUJ) = Z(JUJ)+T*CONJG(UL(J))                  
   5                    CONTINUE                                        
   6              CONTINUE                                              
   7           CONTINUE                                                 
   8        Z(KC) = WK                                                  
   9        CONTINUE                                                    
         S = 1.0/SCASUM(N, Z, 1)                                        
         CALL CSSCAL(N, S, Z, 1)                                        
C                                                                       
C SOLVE Y = L(TRANSPOSE) * W                                            
C                                                                       
         DO  13 KB = 1, N                                               
            K = N+1-KB                                                  
            ID = IU(K)-1                                                
C UL(ID) CONTAINS THE RECIPROCAL OF THE DIAGONAL OF U                   
C TRY TO AVOID OVERFLOW                                                 
            SUM = AMAX1(CABS1(Z(K)), CABS1(UL(ID)))                     
            IF (CABS1(Z(K)) .LE. 1.0) SUM = CABS1(Z(K)*CONJG(UL(ID)))   
            S = 1.0E0                                                   
            JMIN = IA(K)                                                
            JMAX = ID-1                                                 
            IF (SUM .LT. 1.0) GOTO 10                                   
               S = 1./SUM                                               
               CALL CSSCAL(N, S, Z, 1)                                  
  10        SCUM = Z(K)*CONJG(UL(ID))                                   
            IF (S .EQ. 0.0E0) Z(K) = (1.0,0.0)                          
            IF (JMAX .LT. JMIN) GOTO 12                                 
               DO  11 J = JMIN, JMAX                                    
                  JUJ = JA(J)                                           
                  Z(JUJ) = Z(JUJ)+CONJG(UL(J))*SCUM                     
  11              CONTINUE                                              
  12        Z(K) = SCUM                                                 
  13        CONTINUE                                                    
         S = 1.0/SCASUM(N, Z, 1)                                        
         CALL CSSCAL(N, S, Z, 1)                                        
         YNORM = 1.0                                                    
C                                                                       
C SOLVE L X =W                                                          
C                                                                       
         DO  17 I = 1, N                                                
            JMIN = IA(I)                                                
            IUI = IU(I)-1                                               
            JMAX = IUI-1                                                
            DK = UL(IUI)                                                
            SCUM = Z(I)                                                 
            IF (JMIN .GT. JMAX) GOTO 15                                 
               DO  14 J = JMIN, JMAX                                    
                  JJ = JA(J)                                            
                  SCUM = SCUM+UL(J)*Z(JJ)                               
  14              CONTINUE                                              
  15        SC = AMAX1(CABS1(SCUM), CABS1(DK))                          
            IF (CABS1(SCUM) .LE. 1.0) SC = CABS1(SCUM*DK)               
            S = 1.0E0                                                   
            IF (SC .LE. 1.0) GOTO 16                                    
               S = 1./SC                                                
               CALL CSSCAL(N, S, Z, 1)                                  
               YNORM = YNORM*S                                          
               SCUM = SCUM*CMPLX(S,0.0E0)                               
  16        SCUM = SCUM*DK                                              
            IF (S .EQ. 0.0E0) SCUM = (1.0,0.0)                          
            Z(I) = SCUM                                                 
  17        CONTINUE                                                    
         S = 1.0/SCASUM(N, Z, 1)                                        
         IF (S .GT. 1.0) GOTO 18                                        
            CALL CSSCAL(N, S, Z, 1)                                     
            YNORM = YNORM*S                                             
C                                                                       
C   SOLVE U * Z = X                                                     
  18     DO  22 KB = 1, N                                               
            K = N+1-KB                                                  
            JMIN = IU(K)                                                
            JMAX = IA(K+1)-1                                            
            SCUM = Z(K)                                                 
            IF (JMIN .GT. JMAX) GOTO 20                                 
               DO  19 J = JMIN, JMAX                                    
                  JUJ = JA(J)                                           
                  SCUM = SCUM-UL(J)*TMP(JUJ)                            
  19              CONTINUE                                              
  20        IIC = MCP(K)                                                
            IF (CABS1(SCUM) .LE. 1.) GOTO 21                            
               S = 1.0/CABS1(SCUM)                                      
               CALL CSSCAL(N, S, TMP, 1)                                
               CALL CSSCAL(K, S, Z, 1)                                  
               YNORM = YNORM*S                                          
               SCUM = SCUM*CMPLX(S,0.0)                                 
  21        TMP(IIC) = SCUM                                             
  22        CONTINUE                                                    
C    MAKE ZNORM = 1.0                                                   
         S = 1.0/SCASUM(N, TMP, 1)                                      
         CALL CSSCAL(N, S, TMP, 1)                                      
         DO  23 I = 1, N                                                
            Z(I) = TMP(I)                                               
  23        CONTINUE                                                    
         YNORM = YNORM*S                                                
C                                                                       
C   SET COND = ESTIMATE OF THE CONDITION NUMBER OF A                    
C                                                                       
         GREAT = R1MACH(2)                                              
         IF (YNORM .GT. 1.0) GOTO 25                                    
            TEMP = ANORM .GT. YNORM*GREAT                               
            IF (.NOT. TEMP) TEMP = ANORM .EQ. 0.0E0                     
            IF (.NOT. TEMP) GOTO 24                                     
               COND = GREAT                                             
               RETURN                                                   
  24     CONTINUE                                                       
  25     COND = ANORM/YNORM                                             
  26  RETURN                                                            
      END                                                               
      SUBROUTINE CSPMLU(N,MRP, MCP, IA, JA, A, IAMAX, IL, THRESH        
     1   , EPS, ISIZE, GROWTH)                                          
      INTEGER N                                                         
      INTEGER MRP(N), MCP(N), IA(1), JA(1), IAMAX, IL(N)                
      INTEGER ISIZE                                                     
      REAL THRESH, EPS, GROWTH                                          
      COMPLEX A(1)                                                      
      DOUBLE PRECISION DSTAK(500)                                       
      COMMON /CSTAK/ DSTAK                                              
      INTEGER ISTKGT, I, K, IERR, LAST, TEMP                            
      INTEGER TEMP1, IC, II(1000), IZ, IC1                              
      COMPLEX D(500)                                                    
      LOGICAL TEMP2                                                     
      EQUIVALENCE (D(1), II(1), DSTAK(1))                               
C SPARSE DECOMPOSITION                                                  
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C MRP     INTEGER VECTOR GIVING ROW PERMUTATION                         
C MCP     INTEGER VECTOR GIVING COLUMN PERMUTATIONS                     
C IA     INTEGER VECTOR OF LENGTH N+1 GIVING BEGINNING                  
C        OF EACH ROW IN THE A AND JA ARRAYS                             
C JA     INTEGER VECTOR OF LENGTH IAMAX,GIVING COLUMN INDICES           
C        OF EACH ELEMENT IN THE A ARRAY                                 
C A      COMPLEX ARRAY OF THE NONZERO ELEMENTS IN                       
C        THE COEFFICIENT MATRIX STORED BY ROWS                          
C        DESTROYED ON OUTPUT                                            
C IAMAX  DIMENSION OF THE JA AND A ARRAYS,SHOULD AT                     
C        LEAST BE THE SIZE OF THE NUMBER OF NONZERO                     
C        ELMENTS IN A,PREFERABLY,TWICE AS LARGE                         
C THRESH REAL VARIABLE BETWEEN 0 AND 1 GIVING                           
C        A THRESHHOLD FOR PIVOTING. IF THRESH IS 0, NO PIVOTING         
C        WILL BE DONE.  IF THRESH=1.E0,THEN GAUSSIAN                    
C        ELIMINATION WITH PARTIAL PIVOTING WILL BE DONE                 
C EPS    TEST FOR SINGULARITY                                           
C OUTPUT PARAMETERS                                                     
C MCP    THE REORDERING DONE TO INSURE STABILITY,AS DICTATED            
C        BY THRESH                                                      
C A       THE LU DECOMPOSITION OF A                                     
C JA      THE COLUMN INDICES OF THE LU DECOMPOSITION                    
C IL     INTEGER VECTOR OF LENGTH N+1 WHICH POINTS TO THE               
C        BEGINNING OF EACH ROW OF L IN JA ARRAY                         
C ISIZE  ACTUAL NUMBER OF ELEMENTS IN THE A ARRAY THAT WAS NEEDED       
C        FOR THE DECOMPOSITION                                          
C GROWTH NUMERICAL ELEMENT GROWTH. IF THIS MUCH .GT. 1, THEN THE        
C        COMPUTED DECOMPOSITION MAY BE THE DECOMPOSITION OF A MATRIX    
C        THAT IS NOT VERY CLOSE TO THE ORIGINAL MATRIX. RAISING         
C        THRESH MIGHT ALLEVIATE THE SITUATION.                          
C SPACE ALLOCATED AND DEALLOCATED-2N+1 INTEGER AND N DOUBLE             
C PRECISION LOCATIONS                                                   
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C 2 R  NOT IN 1 THROUGH N                                               
C 3 C NOT IN 1 THROUGH N                                                
C 4 IAMAX LESS THAN IA(N+1)-1                                           
C 10+K     NULL ROW           FATAL                                     
C 10+N+K   INVALID INDEX IN ROW K FATAL                                 
C 10+3N+K     SINGULAR MATRIX OF RANK K      RECOVERABLE                
C 10+2N+K    RAN OUT OF SPACE WHEN PROCEESING ROW K                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HCSPMLU-N.LT.1, 13, 1, 2)             
C     IF (IAMAX .LT. IA(N+1)-1) CALL SETERR(24HCSPLU-INSUFFICIENT SPACE,
C    1   24, 4, 2)                                                      
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('CSPMLU-N.LT.1', 13, 1, 2)              
      IF (IAMAX .LT. IA(N+1)-1) CALL SETERR('CSPLU-INSUFFICIENT SPACE', 
     1   24, 4, 2)                                                      
C/                                                                      
      CALL ENTER(1)                                                     
      IZ = ISTKGT(N, 5)                                                 
      IC = ISTKGT(2*N+1, 2)                                             
      IC1 = IC-1                                                        
      DO  1 I = 1, N                                                    
         TEMP2 = MRP(I) .LT. 1                                          
         IF (.NOT. TEMP2) TEMP2 = MRP(I) .GT. N                         
C/6S                                                                    
C        IF (TEMP2) CALL SETERR(23HCSPMLU-MRP OUT OF RANGE, 23, 2, 2)   
C/7S                                                                    
         IF (TEMP2) CALL SETERR('CSPMLU-MRP OUT OF RANGE', 23, 2, 2)    
C/                                                                      
         K = MCP(I)                                                     
         TEMP2 = K .LT. 1                                               
         IF (.NOT. TEMP2) TEMP2 = K .GT. N                              
C/6S                                                                    
C        IF (TEMP2) CALL SETERR(23HCSPMLU-MCP OUT OF RANGE, 23, 3, 2)   
C/7S                                                                    
         IF (TEMP2) CALL SETERR('CSPMLU-MCP OUT OF RANGE', 23, 3, 2)    
C/                                                                      
         TEMP1 = K+IC1                                                  
         II(TEMP1) = I                                                  
   1     CONTINUE                                                       
      TEMP = IC+N                                                       
      CALL CS4MLU(N,IA, JA, A, IAMAX, IL, II(TEMP), D(IZ), II(IC), MRP  
     1   , MCP, IERR, THRESH, EPS, LAST, GROWTH)                        
      IF (IERR .EQ. 0) GOTO 6                                           
         IF (IERR .GT. N+10) GOTO 2                                     
C/6S                                                                    
C           CALL SETERR(15HCSPMLU-NULL ROW, 15, IERR, 2)                
C/7S                                                                    
            CALL SETERR('CSPMLU-NULL ROW', 15, IERR, 2)                 
C/                                                                      
            GOTO  5                                                     
C/6S                                                                    
C  2        IF (IERR .LE. 2*N+10) CALL SETERR(                          
C    1         29HCSPMLU-INCORRECT COLUMN INDEX, 29, IERR, 2)           
C/7S                                                                    
   2        IF (IERR .LE. 2*N+10) CALL SETERR(                          
     1         'CSPMLU-INCORRECT COLUMN INDEX', 29, IERR, 2)            
C/                                                                      
            IF (IERR .LE. 3*N+10) GOTO 3                                
C/6S                                                                    
C              CALL SETERR(25HCSPMLU-INSUFFICIENT SPACE, 25, IERR, 1)   
C/7S                                                                    
               CALL SETERR('CSPMLU-INSUFFICIENT SPACE', 25, IERR, 1)    
C/                                                                      
               CALL LEAVE                                               
               GOTO  4                                                  
C/6S                                                                    
C  3           CALL SETERR(22HCSPMLU-SINGULAR MATRIX, 22, IERR, 1)      
C/7S                                                                    
   3           CALL SETERR('CSPMLU-SINGULAR MATRIX', 22, IERR, 1)       
C/                                                                      
               CALL LEAVE                                               
   4        RETURN                                                      
   5  CONTINUE                                                          
   6  ISIZE = LAST-1                                                    
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE CSPMSL(N, MRP, MCP, IA, JA, A, IL, B, IB, NB)          
      INTEGER N, IB, NB                                                 
      INTEGER MRP(N), MCP(N), IA(1), JA(1), IL(N)                       
      COMPLEX A(1), B(IB, NB)                                           
      COMPLEX D(500)                                                    
      COMMON /CSTAK/ D                                                  
      INTEGER ISTKGT, ITMP, II(1)                                       
      EQUIVALENCE (D(1), II(1))                                         
C FORWARD SOLVE FOR SPARSE MATRICES                                     
C INPUT PARAMETERS                                                      
C N     NUMBER OF EQUATIONS                                             
C MRP   INTEGER VECTOR OF ROW PERMUTATIONS                              
C MCP   INTEGER VECTOR OF COLUMNS PERMUTATIONS                          
C IA    INTEGER VECTOR OF LENGTH N+1 GIVING BEGINNING OF                
C       EACH ROW OF DECOMPOSITION IN A NAD JA ARRAYS                    
C JA    INTEGER VECTOR COMPUTED BY  SPLU                                
C A     COMPLEX ARRAY COMPUTED BY  CSPMLU                               
C IL    INTEGER VECTOR OF LENGTH N+1 POINTING TO L IN LU                
C       DECOMPOSITION,COMPUTED BY CSPLU OR CSPCE                        
C B     MATRIX OF RIGHT HAND SIDES                                      
C IB    ROW DIMENSION OF B MATRIX                                       
C NB    NUMBER OF RIGHT HAND SIDES                                      
C OUTPUT PARAMETERS                                                     
C B     THE SOLUTION MATRIX                                             
C STORAGE SPACE ALLOCATED AND DEALLOCATED -N COMPLEX LOCATIONS          
C ERROR CONDITIONS                                                      
C 1 N.LT.1    FATAL                                                     
C 2 IB.LT.N   FATAL                                                     
C 3 NB.LT.1   FATAL                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HCSPMSL-N.LT.1, 13, 1, 2)             
C     IF (IB .LT. N) CALL SETERR(14HCSPMSL-IB.LT.N, 14, 2, 2)           
C     IF (NB .LT. 1) CALL SETERR(14HCSPMSL-NB.LT.1, 14, 3, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('CSPMSL-N.LT.1', 13, 1, 2)              
      IF (IB .LT. N) CALL SETERR('CSPMSL-IB.LT.N', 14, 2, 2)            
      IF (NB .LT. 1) CALL SETERR('CSPMSL-NB.LT.1', 14, 3, 2)            
C/                                                                      
      CALL ENTER(1)                                                     
      ITMP = ISTKGT(N, 5)                                               
      CALL CS4MSL(N, MRP, MCP, IA, JA, A, IL, B, IB, NB, D(ITMP))       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE CS4MLU(N, IA, JA, A, IAMAX, IL, IROW, Z, IC, R, C,     
     1   IERR, THRESH, EPS, LAST, GROWTH)                               
      INTEGER N                                                         
      INTEGER IA(N), JA(N), IAMAX, IL(N), IROW(N), IC(N)                
      INTEGER R(N), C(N), IERR, LAST                                    
      REAL THRESH, EPS, GROWTH                                          
      COMPLEX A(N), Z(N)                                                
      INTEGER ICC, IDI, JUJ, JLU, NUR, NUU                              
      INTEGER MIN0, I, J, K, M, CMAX                                    
      INTEGER JMIN, IMAX, JMAX, CI, CK, NC                              
      INTEGER II, JJ, IJ, RI, RK, JAMIN                                 
      INTEGER JAMAX, NU, ISPAC, LASTA, IUEND, IT1                       
      INTEGER NP1, JAMAX1                                               
      REAL  CABS1, AFT, PVT, BFOR, PMAX                                 
      REAL AMAX1                                                        
      COMPLEX AKI, DK                                                   
      LOGICAL TEMP                                                      
C INPUT PARAMETERS                                                      
C N ORDER OF MATRIX                                                     
C IA POINTER TO JA AND A OF BEGINNING OF EACH NEW ROW                   
C    LENGTH N+1                                                         
C JA COLUMN INDICES OF NONZERONELEMENTS OF A                            
C A NONZERO ELEMENTS OF A                                               
C IAMAX - DIMENSION OF A AND JA ARRAYS                                  
C R- INTEGER VECTOR LENGTH N OF ROW PERMUTAIONS                         
C C - INTEGER VECTOR LENGTH N OF COLUMN PERMUTATIONS                    
C IC - INTEGER VECTOR OF LENGTH N OF INCERSE COLUMN PERMUTAIONS         
C     IC(C(I))=I                                                        
C THRESH - THRESHHOLD PIVOTING PARAMETER BETWEEN 0.0 AND 1.0.           
C          IF 1.0, PARTIAL PIVOTING WILL BE PEERFORMED                  
C EPS - SINGULARITY CRITERIA.                                           
C SCRATCH VECTORS                                                       
C Z ,COMPLEX, LENGTH N                                                  
C IROW, INTEGER LENGTH N+1                                              
C OUTPUT                                                                
C JA,A NONZERO ELEMENTS AND CORRESPONDING COLUMN INDICES OF LU          
C       DECOMPOSITION                                                   
C C,IC - COLUMN PERMUTAIONS AFTER THRESHHOLD PIVOTING                   
C IERR - ERROR CRITERIA- IF NONZERO WORRY                               
C IF  .LE.N+10 NULL ROW AT IERR-10                                      
C IF N+11 .LE.IERR .LE.2N+10 INVALID INDEX AT ROW IERR-N-10             
C IF 2N+11 .LE.IERR .LE.3N+10  SINGULAR MATRIX OF RANK IERR-2N-10       
C IF 3N+11 .LE.IERR .LE. RAN OUT OF STORAGE AT ROW IERR-3N-10           
C LAST- NUMBER OF ELEMENTS IN A AND JA USED                             
C GROWTH- ELEMENT GROWTH                                                
C IL- INTEGER VECTOR LENGTH N+1, POINTING TO BEGINNINGS OF EACH ROW OF  
C     L IN LU DECOMPOSITION                                             
C INITIALIZATION                                                        
      LAST = IA(N+1)                                                    
      BFOR = 0.0E0                                                      
      IT1 = 0                                                           
      AFT = 0.0E0                                                       
      LASTA = LAST                                                      
      NP1 = N+1                                                         
      IERR = 0                                                          
      DO  1 I = 1, N                                                    
         IROW(I) = 0                                                    
   1     CONTINUE                                                       
C DETERMINE NEXT ROW OF L AND U                                         
      DO  28 K = 1, N                                                   
         RK = R(K)                                                      
         M = NP1                                                        
         JAMIN = IA(RK)                                                 
         IROW(NP1) = NP1                                                
         IUEND = NP1                                                    
         JAMAX1 = IA(RK+1)                                              
         NC = C(K)                                                      
         Z(NC) = (0.0E0,0.0E0)                                          
         JAMAX = JAMAX1-1                                               
C CHECK FOR NULL ROW                                                    
         IF (JAMIN .GT. JAMAX) GOTO  29                                 
         DO  6 J = JAMIN, JAMAX                                         
            JJ = JA(J)                                                  
C CHECK FOR VALID COLUMN INDEX                                          
            TEMP = JJ .LE. 0                                            
            IF (.NOT. TEMP) TEMP = JJ .GT. N                            
            IF (TEMP) GOTO  32                                          
            Z(JJ) = A(J)                                                
            BFOR = AMAX1(BFOR, CABS1(Z(JJ)))                            
            ICC = IC(JJ)                                                
C NEW ELEMENT IS IN U PART SO JUST ADD TO LINKED LIST                   
            IF (ICC .LT. K) GOTO 2                                      
               IROW(ICC) = IUEND                                        
               IUEND = ICC                                              
               GOTO  5                                                  
   2           IF (M .GT. ICC) M = NP1                                  
C NEW ELEMENT IS IN L PART OF THE ROW                                   
   3              II = IROW(M)                                          
                  IF (II .GE. ICC) GOTO  4                              
                  M = II                                                
                  GOTO  3                                               
   4           IROW(M) = ICC                                            
               IROW(ICC) = II                                           
   5        CONTINUE                                                    
   6        CONTINUE                                                    
C                                                                       
C ELIMINATE ROW                                                         
         NUR = 0                                                        
         I = NP1                                                        
   7        I = IROW(I)                                                 
            IF (I .EQ. NP1) GOTO  18                                    
C DETERMINE WHICH ROW IN PERMUTATION                                    
            RI = R(I)                                                   
            JMIN = IA(RI)                                               
            IDI = IL(I)                                                 
C RELEVANT ROW MAY BE IN TWO SECTIONS, THUS EITHER                      
C ELIMINATE UNTIL U IS FINISHED OR SECTION IS FINISHED                  
            JMAX = MIN0(IA(RI+1), IDI)-1                                
            CI = C(I)                                                   
            AKI = -Z(CI)                                                
            AFT = AMAX1(AFT, CABS1(AKI))                                
            NUR = NUR+1                                                 
            IF (JMAX .LT. JMIN) GOTO 17                                 
   8              DO  14 J = JMIN, JMAX                                 
C ELIMINATE ITH ELEMENT                                                 
                     JUJ = JA(J)                                        
                     ICC = IC(JUJ)                                      
                     IF (IROW(ICC) .NE. 0) GOTO 13                      
                        IF (ICC .LT. K) GOTO 9                          
                           IROW(ICC) = IUEND                            
C FILL IN-SEE IF IT IS IN U OR L                                        
                           IUEND = ICC                                  
                           GOTO  12                                     
   9                       TEMP = M .GT. ICC                            
                           IF (.NOT. TEMP) TEMP = M .LT. I              
                           IF (TEMP) M = I                              
C FILL-IN IS IN L SO FIND ITS ORDERING PLACE                            
  10                          IJ = IROW(M)                              
                              IF (IJ .GE. ICC) GOTO  11                 
                              M = IJ                                    
                              GOTO  10                                  
  11                       IROW(M) = ICC                                
                           IROW(ICC) = IJ                               
  12                    Z(JUJ) = (0.E0,0.E0)                            
  13                 Z(JUJ) = Z(JUJ)+AKI*A(J)                           
  14                 CONTINUE                                           
C TEST IF THERE IS A SECOND SEGMENT                                     
                  TEMP = IDI .LT. LASTA                                 
                  IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA                
                  IF (TEMP) GOTO  16                                    
                  JMIN = JA(IDI)                                        
                  JMAX = IDI-1                                          
  15              IF (JMIN .LE. JMAX) GOTO  8                           
  16        CONTINUE                                                    
  17        CONTINUE                                                    
            GOTO  7                                                     
  18     I = IUEND                                                      
         PMAX = 0.E0                                                    
         NU = 0                                                         
  19     IF (I .EQ. NP1) GOTO  21                                       
            CI = C(I)                                                   
            PVT = CABS1(Z(CI))                                          
            NU = NU+1                                                   
            IF (PVT .LE. PMAX) GOTO 20                                  
               IMAX = I                                                 
               PMAX = PVT                                               
  20        I = IROW(I)                                                 
            GOTO  19                                                    
C DO THRESHHOLD PIVOTING                                                
  21     IF (CABS1(Z(NC)) .GE. THRESH*PMAX) IMAX = K                    
         CMAX = C(IMAX)                                                 
C TEST FOR SINGULARITY                                                  
         IF (CABS1(Z(CMAX)) .GT. EPS) GOTO  30                          
            IF (IERR.EQ.0)IERR=3*N+K+9                                  
            DK =CMPLX(R1MACH(2),0.0E0)                                  
            GO TO 330                                                   
 30      CONTINUE                                                       
         DK=CMPLX(1.0E0,0.0E0)/Z(CMAX)                                  
 330     CONTINUE                                                       
         NUR = NUR+NU                                                   
         ISPAC = JAMAX-JAMIN+1                                          
C TEST IS THERE IS ENOUGH SPACE                                         
         IF (NUR-ISPAC+LAST .GE. IAMAX) GOTO  31                        
         NUU = JAMIN+NU-1                                               
C DETERMINE IS FILLIN REQUIRES SECOND SEGMENT FOR U                     
         IF (NUU .GT. JAMAX) NUU = LAST+NU-ISPAC-1                      
         IL(K) = NUU                                                    
         JLU = NUU+1                                                    
         IF (NUU .EQ. JAMAX) JLU = LAST                                 
         JA(NUU) = LAST                                                 
         AFT = AMAX1(AFT, PMAX)                                         
         I = IUEND                                                      
C STORE DIAGONAL ELEMENT                                                
         A(NUU) = DK                                                    
  22     IF (I .EQ. NP1) GOTO  24                                       
            IF (I .EQ. IMAX) GOTO 23                                    
               CI = C(I)                                                
C STORE ELEMENT OF U                                                    
               IF (NUU .EQ. LAST) NUU = JAMAX1                          
               NUU = NUU-1                                              
               A(NUU) = Z(CI)*DK                                        
               JA(NUU) = CI                                             
  23        II = I                                                      
            I = IROW(I)                                                 
            IROW(II) = 0                                                
            GOTO  22                                                    
C STORE ELEMENTS OF L                                                   
  24     I = NP1                                                        
  25        J = I                                                       
            I = IROW(I)                                                 
            IROW(J) = 0                                                 
            IF (I .EQ. NP1) GOTO  26                                    
            JA(JLU) = I                                                 
            CI = C(I)                                                   
            A(JLU) = -Z(CI)                                             
            JLU = JLU+1                                                 
            IF (JLU .EQ. JAMAX1) JLU = LAST                             
            GOTO  25                                                    
  26     IF (JLU .GT. LAST) LAST = JLU                                  
C INTERCHANGE THE COLUMN INDICES                                        
         IF (K .EQ. IMAX) GOTO 27                                       
            JJ = C(K)                                                   
            C(K) = CMAX                                                 
            C(IMAX) = JJ                                                
            CK = C(K)                                                   
            IC(CK) = K                                                  
            IC(JJ) = IMAX                                               
  27     CONTINUE                                                       
  28     CONTINUE                                                       
      GROWTH =1.0E0                                                     
      IF (BFOR.NE.0.0E0) GROWTH = AMAX1(1.0E0, AFT/BFOR)                
      IL(N+1) = LAST                                                    
      JA(LAST) = LAST                                                   
      RETURN                                                            
  29  IERR = K+10                                                       
      RETURN                                                            
  31  IERR = 2*N+K+10                                                   
      RETURN                                                            
  32  IERR = N+K+10                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE CS4MSL(N, R, C, IA, JA, A, IL, B, IB, NB, TMP)         
      INTEGER N, IB, NB                                                 
      INTEGER R(N), C(N), IA(1), JA(1), IL(N)                           
      COMPLEX A(1), B(IB, NB), TMP(N)                                   
      INTEGER IIB, IIC, IDI, IEX, JUJ, IDI1                             
      INTEGER MIN0, I, J, K, JMIN, JMAX                                 
      INTEGER JJ, IR, RI, LASTA, NP1                                    
      COMPLEX SUM, DK                                                   
      LOGICAL TEMP                                                      
C SPARSE MATRIX SOLUTION                                                
C INPUT                                                                 
C N ORDER OF PROBLEM                                                    
C R ROW PERMUTATION                                                     
C C COLUMN PERMUTATION                                                  
C IA INTEGER VECTOR, LENGTH N+1 POINTING TO BEGINNING OF ROW IN JA AND A
C JA COLUMN INDICES CORRESPONDING TO NONZERO ELEMENTS IN A              
C A  COMPLEX VECTOR OF NONZERO ELEMENTS IN LU DECOMPOSTION              
C IL INTEGER VECTOR LENGTH N+1 POINTING TO BEGINNING OF EACH L ROW      
C    IN A AND JA. COMPUTED BY SPMLU                                     
C B RIGHT-HAND SIDE                                                     
C IB ROW DIMENSION OF B                                                 
C NB NUMBER OF RIGHT HAND SIDES                                         
C SCRATCH VECTOR -TMP COMPLEX VECTOR LENGTH N                           
C OUTPUT                                                                
C B   SOLUTION TO PROBLEM                                               
C THIS SUBROUTINE ASSUME EACH ROW CAN BE AT MOST TWO SEGMENTS           
C IL(I) POINTS TO DIAGONAL REALLY AND JA(IL(I)) INDICATES               
C WHERE EXTRA SPACE IS NEEDED FOR THAT ROW. EACH ROW                    
C LOOKS LIKE U,DIAGONAL,L                                               
      LASTA = IA(N+1)                                                   
      NP1 = N+1                                                         
      DO  11 K = 1, NB                                                  
C SPARSE FORWARD SOLVE                                                  
         DO  4 I = 1, N                                                 
            IR = R(I)                                                   
            IDI = IL(I)                                                 
            IDI1 = IL(I+1)                                              
            JMIN = IDI+1                                                
C DETERMINE WHERE FIRST PART OF L IS - IN FIRST OR SECOND               
C SEGMENT                                                               
            IEX = JA(IDI1)-1                                            
            IF (JMIN .EQ. IA(IR+1)) JMIN = JA(IDI)                      
            JMAX = IA(IR+1)-1                                           
            IF (JMAX .LT. JMIN) JMAX = IEX                              
            DK = A(IDI)                                                 
            SUM = B(IR, K)                                              
   1        IF (JMIN .GT. JMAX) GOTO  3                                 
C IS THERE ANY PART OF L IN EXTRA SEGMENT                               
               DO  2 J = JMIN, JMAX                                     
                  JJ = JA(J)                                            
                  SUM = SUM+A(J)*TMP(JJ)                                
   2              CONTINUE                                              
               IF (JMAX .EQ. IEX) GOTO  3                               
               JMIN = JA(IDI)                                           
               JMAX = IEX                                               
               GOTO  1                                                  
   3        TMP(I) = SUM*DK                                             
   4        CONTINUE                                                    
C SPARSE BACK SOLVE                                                     
         DO  10 IIB = 1, N                                              
            I = NP1-IIB                                                 
            RI = R(I)                                                   
            IDI = IL(I)                                                 
            JMIN = IA(RI)                                               
C GO UNTIL EITHER END OF U IS FOUND OR REACH END                        
C OF FIRST SEGMENT                                                      
            JMAX = MIN0(IA(RI+1), IDI)-1                                
            SUM = TMP(I)                                                
            IF (JMIN .GT. JMAX) GOTO 9                                  
   5              DO  6 J = JMIN, JMAX                                  
                     JUJ = JA(J)                                        
                     SUM = SUM-A(J)*B(JUJ, K)                           
   6                 CONTINUE                                           
C CHECK IF THERE IS ANY OF U IN SECOND SEGMENT                          
                  TEMP = IDI .LT. LASTA                                 
                  IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA                
                  IF (TEMP) GOTO  8                                     
                  JMIN = JA(IDI)                                        
                  JMAX = IDI-1                                          
   7              IF (JMIN .LE. JMAX) GOTO  5                           
   8        CONTINUE                                                    
   9        IIC = C(I)                                                  
            B(IIC, K) = SUM                                             
  10        CONTINUE                                                    
  11     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE CSPFLU(N, MRP, MCP, AROW, IWORK, UL, IAMAX,            
     1   THRESH, EPS, ISIZE, GROWTH)                                    
      INTEGER N, IAMAX                                                  
      EXTERNAL AROW                                                     
      INTEGER MRP(N), MCP(N), IWORK(IAMAX), ISIZE                       
      REAL  THRESH, EPS, GROWTH                                         
      COMPLEX UL(N)                                                     
      DOUBLE PRECISION DSTAK(500)                                       
      COMMON /CSTAK/ DSTAK                                              
      INTEGER JLU, ISTKGT, I, K, IERR, LAST                             
      INTEGER TEMP, TEMP1, IC, II(1), IZ, IC1                           
      REAL  ANORM                                                       
      COMPLEX D(500)                                                    
      LOGICAL TEMP2                                                     
      EQUIVALENCE (D(1), II(1), DSTAK(1))                               
C SPARSE DECOMPOSITION                                                  
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C MRP     INTEGER VECTOR GIVING ROW PERMUTATION                         
C MCP     INTEGER VECTOR GIVING COLUMN PERMUTATIONS                     
C AROW    SUBROUTINE OF THE FORM AROW(I,ROW,JCOL,NUM) WHICH             
C         FOR A GIVEN INPUT I RETURNS THE NONZERO ELEMENTS OF           
C         THE ITH ROW OF THE MATRIX A IN THE                            
C         COMPLEX VECTOR ROW AND THE CORRESPONDING INDICES IN           
C         JCOL. THE VARIABLE NUM RETURNS THE NUMBER OF NONZERO          
C         ELEMENTS IN THE ITH ROW. AROW SHOULD BE DECLARED              
C         EXTERNAL IN THE CALLING PROGRAM.                              
C IAMAX  DIMENSION OF THE UL ARRAY,SHOULD AT                            
C        LEAST BE THE SIZE OF  THE NUMBER OF NONZERO                    
C        ELMENTS IN A,PREFERABLY,TWICE AS LARGE                         
C THRESH REAL VARIABLE BETWEEN 0 AND 1 GIVING                           
C        A THRESHHOLD FOR PIVOTING. IF THRESH IS 0, NO PIVOTING         
C        WILL BE DONE.  IF THRESH=1.E0,THEN GAUSSIAN                    
C        ELIMINATION WITH PARTIAL PIVOTING WILL BE DONE                 
C EPS    TEST FOR SINGULARITY                                           
C OUTPUT PARAMETERS                                                     
C C      THE REORDERING DONE TO INSURE STABILITY,AS DICTATED            
C        BY THRESH                                                      
C IWORK   INTEGER VECTOR ,LENGTH 2N+1+IAMAX WHOSE FIRST N+1 COMOPONENTS 
C         POINT TO THE BEGINNING OF TH ROWS OF L IN THE UL              
C         AND WHOSE NEXT N COMPONENTS POINT TO THE BEGINNING            
C         OF THE ROWS OF U IN THE UL. COMPONENTS 2N+2 UNTIL             
C         2N+1+ISIZE GIVE THE COLUMN INDICES OF THE COMPONENTS          
C         OF UL                                                         
C UL      THE LU DECOMPOSITION OF THE SPARSE MATRIX A                   
C ISIZE  ACTUAL NUMBER OF ELEMENTS IN THE A ARRAY THAT WAS NEEDED       
C        FOR THE DECOMPOSITION                                          
C GROWTH NUMERICAL ELEMENT GROWTH. IF GROWTH MUCH .GT. 1,WORRY          
C SPACE ALLOCATED AND DEALLOCATED-2N+1 INTEGER AND N DOUBLE             
C PRECISION LOCATIONS                                                   
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C 2 R AND C NOT IN 1 THROUGH N                                          
C 3 IAMAX LESS THAN N                                                   
C 10+K     NULL ROW           FATAL                                     
C 3N+K+10     SINGULAR MATRIX OF RANK K      RECOVERABLE                
C 2N+K+10    RAN OUT OF SPACE WHEN PROCEESING ROW K                     
C N+K+10   INVALID COLUMN INDEX WHEN PROCESSING ROW K                   
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HCSPFLU-N.LT.1, 13, 1, 2)             
C     IF (IAMAX .LT. N) CALL SETERR(25HCSPFLU-INSUFFICIENT SPACE, 25, 3,
C    1   2)                                                             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('CSPFLU-N.LT.1', 13, 1, 2)              
      IF (IAMAX .LT. N) CALL SETERR('CSPFLU-INSUFFICIENT SPACE', 25, 3, 
     1   2)                                                             
C/                                                                      
      CALL ENTER(1)                                                     
      IZ = ISTKGT(N, 5)                                                 
      IC = ISTKGT(2*N+1, 2)                                             
      IC1 = IC-1                                                        
      DO  1 I = 1, N                                                    
         TEMP2 = MRP(I) .LT. 1                                          
         IF (.NOT. TEMP2) TEMP2 = MRP(I) .GT. N                         
C/6S                                                                    
C        IF (TEMP2) CALL SETERR(23HCSPFLU-MRP OUT OF RANGE, 23, 2, 2)   
C/7S                                                                    
         IF (TEMP2) CALL SETERR('CSPFLU-MRP OUT OF RANGE', 23, 2, 2)    
C/                                                                      
         K = MCP(I)                                                     
         TEMP2 = K .LT. 1                                               
         IF (.NOT. TEMP2) TEMP2 = K .GT. N                              
C/6S                                                                    
C        IF (TEMP2) CALL SETERR(23HCSPFLU-MCP OUT OF RANGE, 23, 2, 2)   
C/7S                                                                    
         IF (TEMP2) CALL SETERR('CSPFLU-MCP OUT OF RANGE', 23, 2, 2)    
C/                                                                      
         TEMP1 = K+IC1                                                  
         II(TEMP1) = I                                                  
   1     CONTINUE                                                       
      TEMP = IC+N                                                       
      JLU = 2*N+2                                                       
      CALL CS4FLU(N,IWORK, IWORK(JLU), UL, IAMAX, IWORK(N+2), II(TEMP)  
     1   , D(IZ), II(IC), MRP, MCP, IERR, THRESH, EPS, LAST, AROW,      
     1   GROWTH, ANORM)                                                 
      IF (IERR .EQ. 0) GOTO 8                                           
         IF (IERR .GT. N+10) GOTO 2                                     
C/6S                                                                    
C           CALL SETERR(15HCSPFLU-NULL ROW, 15, IERR, 2)                
C/7S                                                                    
            CALL SETERR('CSPFLU-NULL ROW', 15, IERR, 2)                 
C/                                                                      
            GOTO  7                                                     
   2        IF (IERR .LE. 3*N+10) GOTO 3                                
C/6S                                                                    
C              CALL SETERR(22HCSPFLU-SINGULAR MATRIX, 22, IERR, 1)      
C/7S                                                                    
               CALL SETERR('CSPFLU-SINGULAR MATRIX', 22, IERR, 1)       
C/                                                                      
               GOTO  6                                                  
   3           IF (IERR .GT. 2*N+10) GOTO 4                             
C/6S                                                                    
C                 CALL SETERR(29HCSPFLU-INCORRECT COLUMN INDEX, 29,     
C    1               IERR, 2)                                           
C/7S                                                                    
                  CALL SETERR('CSPFLU-INCORRECT COLUMN INDEX', 29,      
     1               IERR, 2)                                           
C/                                                                      
                  GOTO  5                                               
C/6S                                                                    
C  4              CALL SETERR(25HCSPFLU-INSUFFICIENT SPACE, 25, IERR+N  
C    1               , 1)                                               
C/7S                                                                    
   4              CALL SETERR('CSPFLU-INSUFFICIENT SPACE', 25, IERR+N   
     1               , 1)                                               
C/                                                                      
   5        CONTINUE                                                    
   6     CONTINUE                                                       
   7  CONTINUE                                                          
   8  ISIZE = LAST-1                                                    
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE CSPFSL(N, MRP, MCP, IWORK, UL, B, IB, NB)              
      INTEGER N, IB, NB                                                 
      INTEGER MRP(N), MCP(N), IWORK(1)                                  
      COMPLEX UL(1), B(IB, NB)                                          
      COMPLEX D(500)                                                    
      COMMON /CSTAK/ D                                                  
      INTEGER JLU, ISTKGT, ITMP                                         
C FORWARD SOLVE FOR SPARSE MATRICES                                     
C INPUT PARAMETERS                                                      
C N     NUMBER OF EQUATIONS                                             
C MRP   INTEGER VECTOR OF ROW PERMUTATIONS                              
C MCP   INTEGER VECTOR,LENGTHN OR COLUMN PERMUTATIONS                   
C IWORK INTEGER VECTOR COMPUTED BY  CSPFLU                              
C UL    COMPLEX ARRAY COMPUTED BY  CSPFLU                               
C IU    INTEGER VECTOR OF LENGTH N POINTING TO U IN LU                  
C       DECOMPOSITION,COMPUTED BY CSPFLU OR CSPFCE                      
C B     MATRIX OF RIGHT HAND SIDES                                      
C IB    ROW DIMENSION OF B MATRIX                                       
C NB    NUMBER OF RIGHT HAND SIDES                                      
C OUTPUT PARAMETERS                                                     
C B     THE SOLUTION MATRIX                                             
C STORAGE SPACE ALLOCATED AND DEALLOCATED -N COMPLEX LOCATIONS          
C ERROR CONDITIONS                                                      
C 1 N.LT.1    FATAL                                                     
C 2 IB.LT.N   FATAL                                                     
C 3 NB.LT.1   FATAL                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HCSPFSL-N.LT.1, 13, 1, 2)             
C     IF (IB .LT. N) CALL SETERR(14HCSPFSL-IB.LT.N, 14, 2, 2)           
C     IF (NB .LT. 1) CALL SETERR(14HCSPFSL-NB.LT.1, 14, 3, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('CSPFSL-N.LT.1', 13, 1, 2)              
      IF (IB .LT. N) CALL SETERR('CSPFSL-IB.LT.N', 14, 2, 2)            
      IF (NB .LT. 1) CALL SETERR('CSPFSL-NB.LT.1', 14, 3, 2)            
C/                                                                      
      CALL ENTER(1)                                                     
      ITMP = ISTKGT(N, 5)                                               
      JLU = 2*N+2                                                       
      CALL CS4FSL(N,MRP, MCP, IWORK, IWORK(JLU), UL, IWORK(N+2), B, IB  
     1   , NB, D(ITMP))                                                 
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE CS4FLU(N, IA, JA, A, IAMAX, IU, IROW, Z, IC, R, C,     
     1   IERR, THRESH, EPS, JLU, GETA, GROWTH, BEFOR)                   
      INTEGER N                                                         
      EXTERNAL GETA                                                     
      INTEGER IA(1), JA(1), IAMAX, IU(N), IROW(N), IC(N)                
      INTEGER R(N), C(N), IERR, JLU                                     
      REAL THRESH, EPS, GROWTH, BEFOR                                   
      COMPLEX A(1), Z(N)                                                
      INTEGER ICC, JUJ, NUM, I, J, K                                    
      INTEGER M, JMIN, IMAX, JMAX, CK, II                               
      INTEGER JJ, IJ, RI, RK, NU, JAMAX                                 
      INTEGER IUEND, NP1, NP2                                           
      REAL  CABS1, AFT, PVT, BEFO, PMAX                                 
      COMPLEX AKI,DK                                                    
      REAL AMAX1                                                        
      LOGICAL TEMP, SING                                                
C THIS IS LOWER LEVEL LU DECOMPOSITION WITH THRESHHOLD PIVOTING         
C WITH FUNCTION INPUT                                                   
C INPUT PARAMETERS                                                      
C N INTEGER NUMBER OF ROWS                                              
C IAMAX DECLARED SIZE OF THE A ARRAY.                                   
C IC INTEGER N-VECTOR GIVING INVERSE COLUMN PERMUTATION                 
C R INTEGER N-VECTOR GIVING ROW PERMUTATION                             
C C INTEGER N-VECTOR GIVING COLUMN PERMUTATION                          
C THRESH REAL SCALAR GIVING THRESHHOLD FOR PIVOTING                     
C EPS   REAL SCALAR GIVING THREHHOLD FOR SINGULARITY                    
C GETA  SUBROUTINE WHICH RETURNS THE NONZERO ELEMENTS AND               
C  THEIR CORRESPONDING COLUMN INDICES WHEN ASKED FOR A                  
C  GIVEN ROW                                                            
C OUTPUT PARAMETERS                                                     
C IA INTEGER N+1 VECTOR GIVING BEGINNING OF ITH ROW                     
C    OF L IN SPARSE LU DECOMPOSITION IN JA AND A                        
C JA COLUMN INDICES OF LU DECOMPOSITION                                 
C A LU DECOMPOSITION                                                    
C IU INTEGER N VECTOR GIVING BEGINNING OF U ROWS IN IA AND JA           
C IC INVERSE COLUMN PERMUTATION AFTER PIUVOTING FOR STABILITY           
C C  COLUMN PERMUTATION AFTER PIVOTING FOR STABILITY                    
C IERR- ERROR FLAG                                                      
C     10 .LT. IERR .LT. 11+N   NULL ROW                                 
C     10+N .LT. IERR .LT. 11+2N   INVALID COLUMN INDEX                  
C     10+2N .LT. IERR .LT. 11+3N  NOT ENOUGH SPACE                      
C     10+3N .LT. IERR .LT. 11+4N   SINGULAR MATRIX OF RANK IERR-3N-10   
C JLU INTEGER SCALAR GIVING SIZE OF DECOMPOSITION                       
C GROWTH SCALAR GIVING ELEMENT GROWTH                                   
C BEFOR SCALR GIVING NORM OF ORIGINAL MATRIX                            
C SCRATCH SPACE                                                         
C IROW -INTEGER LENGTH N+1                                              
C Z-COMPLEX LENGTH N                                                    
C INITIALIZATION                                                        
      IA(1) = 1                                                         
      BEFOR = 0.0E0                                                     
      AFT = 0.0E0                                                       
      NP1 = N+1                                                         
      IERR = 0                                                          
      NP2 = N+2                                                         
      JLU = 1                                                           
      DO  1 I = 1, N                                                    
         IROW(I) = 0                                                    
   1     CONTINUE                                                       
C DETERMINE NEXT ROW OF L AND U                                         
      DO  23 K = 1, N                                                   
         RK = R(K)                                                      
         SING=.FALSE.                                                   
         M = NP1                                                        
C CALL GETA TO GET ROW RK                                               
         CALL GETA(RK, A(JLU), JA(JLU), NUM)                            
         Z(K) = (0.E0,0.E0)                                             
         JAMAX = JLU+NUM-1                                              
C CHECK IF ENOUGH SPACE                                                 
         IF (JAMAX .GT. IAMAX) GOTO  24                                 
         IROW(NP1) = NP1                                                
         IUEND = NP2                                                    
         BEFO = 0.0                                                     
C CHECK FOR NULL ROW                                                    
         IF (NUM .LE. 0) GOTO  25                                       
         DO  6 J = JLU, JAMAX                                           
            JJ = JA(J)                                                  
C CHECK FOR INVLAID INDEX                                               
            TEMP = JJ .LE. 0                                            
            IF (.NOT. TEMP) TEMP = JJ .GT. N                            
            IF (TEMP) GOTO  29                                          
C GET INVERSE COLUMN INDEX                                              
            ICC = IC(JJ)                                                
            Z(ICC) = A(J)                                               
            BEFO = BEFO+CABS1(A(J))                                     
C CHECK IF IN L OR U PORTION OF ROW                                     
            IF (ICC .LT. K) GOTO 2                                      
               IROW(ICC) = IUEND                                        
C IF IT IS U PORTION-SO JUST ADD TO LINKED LIST                         
               IUEND = ICC                                              
               GOTO  5                                                  
   2           IF (M .GT. ICC) M = NP1                                  
C NEW ELEMENT IS IN L PART OF THE ROW                                   
C THUS ONE MUST INSERT IN ORDERED LIST                                  
   3              II = IROW(M)                                          
                  IF (II .GE. ICC) GOTO  4                              
                  M = II                                                
                  GOTO  3                                               
   4           IROW(M) = ICC                                            
               IROW(ICC) = II                                           
   5        CONTINUE                                                    
   6        CONTINUE                                                    
         BEFOR = AMAX1(BEFO, BEFOR)                                     
C                                                                       
C ELIMINATE ROW                                                         
         I = NP1                                                        
   7        J = I                                                       
            I = IROW(I)                                                 
            IROW(J) = 0                                                 
            IF (I .EQ. NP1) GOTO  15                                    
            JMIN = IU(I)                                                
            RI = R(I)                                                   
            JMAX = IA(I+1)-1                                            
C AKI WILL HAVE MULTIPLIER                                              
            AKI = -Z(I)                                                 
            AFT = AMAX1(AFT, CABS1(AKI))                                
C CHECK IF SUFFICIENT SPACE                                             
C TO STORE ELEMENT OF L                                                 
            IF (JLU .GT. IAMAX) GOTO  26                                
C STORE ELEMENT OF L                                                    
            A(JLU) = AKI                                                
            JA(JLU) = I                                                 
            JLU = JLU+1                                                 
            IF (JMAX .LT. JMIN) GOTO 14                                 
               DO  13 J = JMIN, JMAX                                    
C ELIMINATE ITH ELEMENT                                                 
                  JUJ = JA(J)                                           
                  ICC = IC(JUJ)                                         
C IF IROW(ICC) IS 0 THEN FILL IN WILL OCCUR                             
                  IF (IROW(ICC) .NE. 0) GOTO 12                         
                     IF (ICC .LT. K) GOTO 8                             
                        IROW(ICC) = IUEND                               
C SINCE FILLIN IS IN U PORITION JSUT ADD TO END OF LINKED LIST          
                        IUEND = ICC                                     
                        GOTO  11                                        
   8                    TEMP = M .GT. ICC                               
                        IF (.NOT. TEMP) TEMP = M .LT. I                 
                        IF (TEMP) M = I                                 
C FILL IN IS IN L PORTION SO ONE MUST INSERT IN ORDERED LIST            
   9                       IJ = IROW(M)                                 
                           IF (IJ .GE. ICC) GOTO  10                    
                           M = IJ                                       
                           GOTO  9                                      
  10                    IROW(M) = ICC                                   
                        IROW(ICC) = IJ                                  
  11                 Z(ICC) = (0.E0,0.E0)                               
  12              Z(ICC) = Z(ICC)+AKI*A(J)                              
  13              CONTINUE                                              
  14        CONTINUE                                                    
C FIND PIVOT                                                            
            GOTO  7                                                     
  15     I = IUEND                                                      
         PMAX = 0.E0                                                    
         NU = 0                                                         
  16     IF (I .EQ. NP2) GOTO  18                                       
            PVT = CABS1(Z(I))                                           
            NU = NU+1                                                   
            IF (PVT .LE. PMAX) GOTO 17                                  
               IMAX = I                                                 
               PMAX = PVT                                               
  17        I = IROW(I)                                                 
            GOTO  16                                                    
C DO THRESHHOLD PIVOTING                                                
  18     IF (CABS1(Z(K)) .GE. THRESH*PMAX) IMAX = K                     
C CHECK FOR SINGULARITY                                                 
         IF (CABS1(Z(IMAX)) .GT. EPS) GOTO  27                          
            IF(IERR.EQ.0)IERR=K+9+3*N                                   
            SING=.TRUE.                                                 
   27       CONTINUE                                                    
         IF (.NOT.SING)DK = (1.E0,0.E0)/Z(IMAX)                         
         IF (SING)DK = CMPLX(R1MACH(2),0.0)                             
         AFT = AMAX1(AFT, PMAX)                                         
         I = IUEND                                                      
         NU = JLU+NU                                                    
C CHECK IF SUFFICIENT SPACE TO STORE U PORTION                          
         IF (NU .GT. IAMAX) GOTO  28                                    
C STORE DIAGONAL ELEMENT                                                
         A(JLU) = DK                                                    
         JA(JLU) = C(IMAX)                                              
         IU(K) = JLU+1                                                  
         JLU = NU                                                       
  19     IF (I .EQ. NP2) GOTO  21                                       
C CHECK IF DIAGONAL ELEMENT WHICH HAS ALREADY BEEN STORED               
            IF (I .EQ. IMAX) GOTO 20                                    
               NU = NU-1                                                
C STORE ELEMENT OF U                                                    
               A(NU) = Z(I)*DK                                          
               JA(NU) = C(I)                                            
  20        II = I                                                      
            I = IROW(I)                                                 
            IROW(II) = 0                                                
            GOTO  19                                                    
  21     IA(K+1) = JLU                                                  
C INTERCHANGE THE COLUMN INDICES                                        
         IF (K .EQ. IMAX) GOTO 22                                       
            JJ = C(K)                                                   
            C(K) = C(IMAX)                                              
            C(IMAX) = JJ                                                
            CK = C(K)                                                   
            IC(CK) = K                                                  
            IC(JJ) = IMAX                                               
  22     CONTINUE                                                       
  23     CONTINUE                                                       
      GROWTH = 1.0E0                                                    
      IF (BEFOR.NE.0.0E0) GROWTH = AMAX1(1.0E0, AFT/BEFOR)              
      JA(JLU) = JLU                                                     
      RETURN                                                            
  24  IERR = 2*N+K+10                                                   
      RETURN                                                            
  25  IERR = K+10                                                       
      RETURN                                                            
  26  IERR = 2*N+K+10                                                   
      RETURN                                                            
  28  IERR = 2*N+K+10                                                   
      RETURN                                                            
  29  IERR = K+10+N                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE CS4FSL(N, R, C, IA, JA, A, IU, B, IB, NB, TMP)         
      INTEGER N, IB, NB                                                 
      INTEGER R(N), C(N), IA(1), JA(1), IU(N)                           
      COMPLEX A(1), B(IB, NB), TMP(N)                                   
      INTEGER IIB, IIC, IUI, JUJ, I, J                                  
      INTEGER K, JMIN, JMAX, JJ, IR, NP1                                
      COMPLEX SUM, DK                                                   
C THIS IS LOWER LEVEL SUBROUTINE FORCSPFSL                              
C SPARSE FORWARD SOLVE                                                  
      NP1 = N+1                                                         
      DO  7 K = 1, NB                                                   
         DO  3 I = 1, N                                                 
            IR = R(I)                                                   
            JMIN = IA(I)                                                
            IUI = IU(I)-1                                               
            JMAX = IUI-1                                                
            DK = A(IUI)                                                 
            SUM = B(IR, K)                                              
            IF (JMIN .GT. JMAX) GOTO 2                                  
               DO  1 J = JMIN, JMAX                                     
                  JJ = JA(J)                                            
                  SUM = SUM+A(J)*TMP(JJ)                                
   1              CONTINUE                                              
   2        TMP(I) = SUM*DK                                             
   3        CONTINUE                                                    
C SPARSE BACKWARD SOLVE                                                 
         DO  6 IIB = 1, N                                               
            I = NP1-IIB                                                 
            JMIN = IU(I)                                                
            JMAX = IA(I+1)-1                                            
            SUM = TMP(I)                                                
            IF (JMIN .GT. JMAX) GOTO 5                                  
               DO  4 J = JMIN, JMAX                                     
                  JUJ = JA(J)                                           
                  SUM = SUM-A(J)*B(JUJ, K)                              
   4              CONTINUE                                              
   5        IIC = C(I)                                                  
            B(IIC, K) = SUM                                             
   6        CONTINUE                                                    
   7     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE CSPFNF(N, MRP, MCP, AROW, IWORK, A, GROWTH, EPS)       
      INTEGER N                                                         
      EXTERNAL AROW                                                     
      INTEGER MRP(N), MCP(N), IWORK(1)                                  
      REAL  GROWTH, EPS                                                 
      COMPLEX A(1)                                                      
      COMPLEX DD(500)                                                   
      COMMON /CSTAK/ DD                                                 
      INTEGER JLU, IST(1000), ROW, ISTKGT, NERROR, I                    
      INTEGER K, IDEX, NERR, IROW, ROWM1, ID                            
      INTEGER INDEX                                                     
      EQUIVALENCE (DD(1), IST(1))                                       
C                                                                       
C                                                                       
C NUMERICAL FACTORIZATION                                               
C INPUT PARAMETERS                                                      
C N    ORDER OF MATRIX                                                  
C MRP   INTEGER VECTOR OF LENGTH N GIVING ROW PERMUTATIONS              
C MCP   INTEGER VECTOR OF LENGTH N GIVING COLUMN PERMUTATIONS           
C IWORK INTEGER VECTOR OF SYMBOLIC FACTORIZATION COMPUTED BY            
C        SPFSF                                                          
C EPS     LARGEST NONACCEPTABLE PIVOT FOR SINGULARITY TEST              
C AROW    SUBROUTINE,DECLARED EXTERNAL IN THE USERS MAIN PROGRAM        
C         WHICH HAS THE CALLING SEQUENCE AROW(I,ROW,ROW,NUM)            
C         WITH INPUT PARAMETER I AND OUTPUT PARAMETERS ROW, ROW         
C         AND NUM WHICH RETURNS IN THE D.P. VECTOR ROW THE              
C         NONZERO ENTRIES IN THE ITH ROW OF A. THE VECTOR               
C         ROW SHOULD BE FILLED WITH THE CORRESPONDING COLUMN            
C         INDICES OF THE NONZERO ELEMENTS. NUM GIVES THE NUMBER         
C         OF THE ELEMENTS                                               
C OUTPUT PARAMETERS                                                     
C A     NUMERICAL FACTORIZATION OF MATRIX                               
C GROWTH    GROWTH FACTOR                                               
C STORAGE ALCATED -2N COMPLEX AND N INTEGER LOCATIONS                   
C       ARE RETURNED                                                    
C ERROR STATES                                                          
C 1   N.LT.1     FATAL                                                  
C 10+K SINGULAR MATRIX OF RANK K   RECOVERABLE                          
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HCSPFNF-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('CSPFNF-N.LT.1', 13, 1, 2)              
C/                                                                      
      CALL ENTER(1)                                                     
      ID = ISTKGT(2*N, 5)                                               
      ROW = ISTKGT(N, 2)                                                
      ROWM1 = ROW-1                                                     
      IROW = ID+N                                                       
      DO  1 I = 1, N                                                    
         K = MCP(I)                                                     
         INDEX = ROWM1+K                                                
         IST(INDEX) = I                                                 
   1     CONTINUE                                                       
      JLU = 2*N+2                                                       
      CALL CS4FNF(N, MRP, IST(ROW), IWORK, IWORK(N+2), IWORK(JLU), A,   
     1   AROW, DD(IROW), GROWTH, EPS, MCP)                              
C/6S                                                                    
C     IF (NERROR(NERR) .NE. 0) CALL N5ERR(22HCSPFNF-SINGULAR MATRIX, 22,
C    1   NERR, 1)                                                       
C/7S                                                                    
      IF (NERROR(NERR) .NE. 0) CALL N5ERR('CSPFNF-SINGULAR MATRIX', 22, 
     1   NERR, 1)                                                       
C/                                                                      
      DO  2 I = 1, N                                                    
         IDEX = ROWM1+I                                                 
         INDEX = IST(IDEX)                                              
         MCP(INDEX) = I                                                 
   2     CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE CS4FNF(N, R, IC, IL, IU, JU, U, GETA, ROW, G, EPS,     
     1   JROW)                                                          
      INTEGER N                                                         
      EXTERNAL GETA                                                     
      INTEGER R(N), IC(N), IL(1), IU(1), JU(1), JROW(N)                 
      COMPLEX U(1), ROW(1)                                              
      REAL G,EPS                                                        
      INTEGER NUM, I, J, K, IMIN, JMIN                                  
      INTEGER IMAX, JMAX, TEMP1, TEMP2, JJ, IR                          
      INTEGER IT1, IT2, IMINM1                                          
      REAL BEF, CABS1, AFT,  AMAX1                                      
      COMPLEX DK, TEMP                                                  
      COMPLEX LI                                                        
C                                                                       
C                                                                       
C       INPUT VARIABLES N,IL,IU,JU                                      
C       OUTPUT VARIABLES--   G,  U,                                     
C       PARAMETERS USED INTERNALLY--                                    
C FIA   * ROW - HOLDS INTERMEDIATE VALUES IN CALCULATION OF  U AND L.   
C       *         SIZE = N.                                             
C RF       G      ELEMENT GROWTH                                        
C RN      EPS      LARGEST NONACCEPTABLE PIVOT                          
C  INTERNAL VARIABLES--                                                 
C    JMIN, JMAX - INDICES OF THE FIRST AND LAST POSITIONS IN A ROW TO   
C      BE EXAMINED.                                                     
C    AFT - NORM OF MATRIX AFTER DECOMPOSITION                           
C    BEF   NORM OF MATRIX ORIGINALLY                                    
      BEF = 0.0                                                         
      AFT = 0.0                                                         
      DO  1 I = 1, N                                                    
         ROW(I) = (0.E0,0.E0)                                           
   1     CONTINUE                                                       
      DO  9 K = 1, N                                                    
C  ******  SET THE INITIAL STRUCTURE OF ROW  ************************** 
         IR = R(K)                                                      
         IMIN = IL(K)                                                   
         IMINM1 = IMIN-1                                                
         CALL GETA(IR, U(IMIN), JROW, NUM)                              
         DO  2 J = 1, NUM                                               
            JJ = J+IMINM1                                               
            IF (CABS1(U(JJ)) .GT. BEF) BEF = CABS1(U(JJ))               
            IT1 = JROW(J)                                               
            IT2 = IC(IT1)                                               
            ROW(IT2) = U(JJ)                                            
   2        CONTINUE                                                    
C  ******  ASSIGN THE KTH ROW OF L AND ADJUST ROW,   ***************    
         IMAX = IU(K)-2                                                 
         IF (IMIN .GT. IMAX) GOTO 6                                     
            DO  5 I = IMIN, IMAX                                        
               TEMP2 = JU(I)                                            
               LI = -ROW(TEMP2)                                         
               ROW(TEMP2) = (0.E0,0.E0)                                 
               U(I) = LI                                                
               JMIN = IU(TEMP2)                                         
               JMAX = IL(TEMP2+1)-1                                     
               IF (JMIN .GT. JMAX) GOTO 4                               
                  DO  3 J = JMIN, JMAX                                  
                     TEMP1 = JU(J)                                      
                     ROW(TEMP1) = ROW(TEMP1)+LI*U(J)                    
   3                 CONTINUE                                           
   4           CONTINUE                                                 
   5           CONTINUE                                                 
C  ******  ASSIGN KTH ROW OF U AND DIAGONAL D, SET TMP(K)  ************ 
   6     IF (CABS1(ROW(K)) .LE. EPS) GOTO  10                           
         DK = (1.E0,0.E0)/ROW(K)                                        
         ROW(K) = (0.E0,0.E0)                                           
         JMIN = IU(K)                                                   
         U(JMIN-1) = DK                                                 
         AFT = AMAX1(AFT, CABS1(ROW(K)))                                
         JMAX = IL(K+1)-1                                               
         IF (JMIN .GT. JMAX) GOTO 8                                     
            DO  7 J = JMIN, JMAX                                        
               TEMP1 = JU(J)                                            
               TEMP = ROW(TEMP1)                                        
               ROW(TEMP1) = (0.E0,0.E0)                                 
               IF (CABS1(TEMP) .GT. AFT) AFT = CABS1(TEMP)              
               U(J) = TEMP*DK                                           
   7           CONTINUE                                                 
   8     CONTINUE                                                       
   9     CONTINUE                                                       
C  ******  NORMAL RETURN AND ERROR RETURNS  *************************** 
      G = AMAX1(AFT/BEF, 1.0)                                           
      RETURN                                                            
C  ZERO DIAGONAL ELEMENT                                                
C/6S                                                                    
C 10  CALL SETERR(22HCSPFNF-SINGULAR MATRIX, 22, K+9, 1)                
C/7S                                                                    
  10  CALL SETERR('CSPFNF-SINGULAR MATRIX', 22, K+9, 1)                 
C/                                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE CSPFML(N, AROW, X, B)                                  
      INTEGER N                                                         
      COMPLEX  X(N), B(N)                                               
      EXTERNAL AROW                                                     
      COMPLEX SUM                                                       
      INTEGER JMAX, I, J, JJ, IIA, IIAM1, JP, JR, IROW, IROWM1          
      COMPLEX R(500)                                                    
      INTEGER IA(1000)                                                  
      COMMON /CSTAK/  R                                                 
      EQUIVALENCE (R(1),IA(1))                                          
C THIS SUBROUTINE MULTIPLIES A BY X AND PUTS THE RESULT IN B WHERE      
C A COMPUTED BY A FUNCTION                                              
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C AROW    FUNCTION WHICH DELIVERS TO CSPFML ONE ROW OF THE              
C         MATRIX AT A TIME                                              
C X       N-VECTOR TO BE MULTIPLIED                                     
C OUTPUT PARAMETERS                                                     
C B      A*X                                                            
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C STORAGE TAKEN FROM STACK- N COMPLEX AND N INTEGER LOCATIONS           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HCSPFML-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('CSPFML-N.LT.1', 13, 1, 2)              
C/                                                                      
      CALL ENTER(1)                                                     
      IIA=ISTKGT(N, 2)                                                  
      IIAM1=IIA-1                                                       
      IROW = ISTKGT(N, 5)                                               
      IROWM1=IROW-1                                                     
      DO 30 I=1,N                                                       
         CALL AROW(I, R(IROW), IA(IIA), JMAX)                           
         SUM=(0.0,0.0)                                                  
         IF (JMAX.LT.1) GO TO 20                                        
         DO 10 JJ=1,JMAX                                                
            JP=JJ+IIAM1                                                 
            J=IA(JP)                                                    
            JR=JJ+IROWM1                                                
            SUM=SUM+R(JR)*X(J)                                          
  10     CONTINUE                                                       
  20     B(I)=SUM                                                       
  30  CONTINUE                                                          
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE CSPMML(N, IA, JA, A,  X, B)                            
      INTEGER N                                                         
      INTEGER IA(N), JA(N)                                              
      COMPLEX A(N), X(N), B(N)                                          
      COMPLEX SUM                                                       
      INTEGER JMIN, JMAX, I, J, JJ                                      
C THIS SUBROUTINE MULTIPLIES A BY X AND PUTS THE RESULT IN B WHERE      
C A IS A SPARSE MATRIX                                                  
C INPUT PARAMETERS                                                      
C N      NUMBER OF EQUATIONS                                            
C IA      INTEGER VECTOR, LENGTH N+1, POINTING TO BEGINNINGS            
C         OF ROWS IN JA AND A VECTORS                                   
C JA      COLUMN INDICES OF NONZERO ELEMENTS OF MATRIX                  
C A       NONZERO ELEMENTS OF THE MATRIX                                
C X       N-VECTOR TO BE MULTIPLIED                                     
C OUTPUT PARAMETERS                                                     
C B      A*X                                                            
C ERROR STATES                                                          
C 1 N.LT.1        FATAL                                                 
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HCSPMML-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('CSPMML-N.LT.1', 13, 1, 2)              
C/                                                                      
      SUM=(0.0,0.0)                                                     
      DO 30 I=1,N                                                       
         JMIN=IA(I)                                                     
         JMAX=IA(I+1)-1                                                 
         SUM=(0.0,0.0)                                                  
         IF (JMAX.LT.JMIN) GO TO 20                                     
         DO 10 JJ=JMIN,JMAX                                             
            J=JA(JJ)                                                    
            SUM=SUM+A(JJ)*X(J)                                          
  10     CONTINUE                                                       
  20     B(I)=SUM                                                       
  30  CONTINUE                                                          
      RETURN                                                            
      END                                                               
        SUBROUTINE CSPMIN(N,IC,IWORK,I,AROW,JROW,NUM,INDEX,A)           
C                                                                       
C THIS SUBROUTINE INSERTS IN THE VECTOR A THE ELEMENTS OF               
C AROW ACCORDING TO THE SCHEME RECORDED IN IWORK AND                    
C DETERMINED BY SPMSF                                                   
C                                                                       
C INPUT PARAMETERS                                                      
C N       NUMBER OF ROWS IN ORIGINAL MATRIX                             
C IC      INTEGER VECTOR OF LENGTH N CONTAINING INVERSE OF              
C         COLUMN PERMUTATIONS                                           
C IWORK   INTEGER VECTOR OUTPUT FROM SPMSF                              
C I       WHICH ROW NOW INTSERTING                                      
C AROW    COMPLEX VECTOR LENGTH NUM OF NONZERO ELEMENTS IN ROW I        
C         OF ORIGINAL MATRIX                                            
C JROW    INTEGER VECTOR OF LENGTH NUM OF COLUMN INDICES IN ROW I       
C         OF ORIGINAL VECTOR                                            
C NUM     NUMBER OF NONZERO ELEMENTS IN ROW I NOW INSERTING INTO A      
C INDEX   IF INDEX IS 1 THE WHOLE ARRAY A IS ZEROED OUT BEFOREHAND      
C         THIS ALLOWS ONE TO CALL THE SUBROUTINE SEVERAL TIMES          
C         FOR 1 ROW AND TO ADD THE NEW INFORMATION                      
C OUTPUT PARAMETER                                                      
C A       VECTOR FOR INSERT INTO DSPMNF WITH THE INFORMATION            
C         FROM AROW INSERTED                                            
C ERROR STATES                                                          
C 1 N.LT.1                                                              
C 2 I NOT IN 1 THOUGH N                                                 
C 3 NUM.LT.1                                                            
C 4 INDEX IN JROW VECTOR FOR ROW I WAS NOT KNOWN TO SPMSF               
C                                                                       
        INTEGER N,I,NUM                                                 
        INTEGER IC(N),JROW(N),INDEX,IWORK(1)                            
        COMPLEX A(N),AROW(NUM)                                          
        INTEGER NTOT,K,IR,JS,JE,KK,MM,M,MIN,IDISP                       
C/6S                                                                    
C       IF (N.LT.1) CALL SETERR(13HCSPMIN-N.LT.1,13,1,2)                
C       IF (NUM.LT.1) CALL SETERR(15HCSPMIN-NUM.LT.1,15,3,2)            
C       IF(I.LT.1.OR.I.GT.N) CALL SETERR(21HCSPMIN-I OUT OF RANGE,      
C    1      21,2,2)                                                     
C/7S                                                                    
        IF (N.LT.1) CALL SETERR('CSPMIN-N.LT.1',13,1,2)                 
        IF (NUM.LT.1) CALL SETERR('CSPMIN-NUM.LT.1',15,3,2)             
        IF(I.LT.1.OR.I.GT.N) CALL SETERR('CSPMIN-I OUT OF RANGE',       
     1      21,2,2)                                                     
C/                                                                      
         IF (INDEX.NE.1) GO TO 10                                       
            NTOT=IWORK(N+1)-1                                           
            DO 1 K=1,NTOT                                               
                 A(K)=(0.0E0,0.0E0)                                     
 1          CONTINUE                                                    
 10      IR=I                                                           
         IDISP=2*N+1                                                    
         JS=IWORK(IR)+IDISP                                             
         JE= IWORK(IR+1)+IDISP-1                                        
         KK=JS                                                          
         DO 50 K=1,NUM                                                  
            MM=JROW(K)                                                  
            M=IC(MM)                                                    
            IF (IWORK(KK)-M)30,40,20                                    
 20         IF (KK.EQ.JS) GO TO 80                                      
            KK=KK-1                                                     
            IF (IWORK(KK)-M)80,40,20                                    
 30         IF (KK.EQ.JE) GO TO 80                                      
            KK=KK+1                                                     
            IF (IWORK(KK)-M)30,40,80                                    
 40         MIN=KK-IDISP                                                
            A(MIN)=A(MIN)+AROW(K)                                       
 50     CONTINUE                                                        
        RETURN                                                          
C/6S                                                                    
C80     CALL SETERR(20HCSPMIN-UNKNOWN INDEX,20,4,2)                     
C/7S                                                                    
 80     CALL SETERR('CSPMIN-UNKNOWN INDEX',20,4,2)                      
C/                                                                      
        RETURN                                                          
        END                                                             
      SUBROUTINE CSPMNF(N, IWORK, A,  EPS, GROWTH)                      
      INTEGER N, IWORK(1), JLU                                          
      REAL   EPS, GROWTH                                                
      COMPLEX A(1)                                                      
      COMPLEX DD(500)                                                   
      COMMON /CSTAK/ DD                                                 
      INTEGER ID, NERR, ISTKGT, NERROR, ISP(1)                          
      EQUIVALENCE (DD(1), ISP(1))                                       
C NUMERICAL FACTORIZATION                                               
C INPUT PARAMETERS                                                      
C N    ORDER OF MATRIX                                                  
C IWORK INTEGER VECTOR OF SYMBOLIC FACTORIZATION COMPUTED BY            
C        SPMSF                                                          
C EPS     LARGEST NONACCEPTABLE PIVOT FOR SINGULARITY TEST              
C A       VECTOR COMPUTED BY  SPMIN ACCORDING TO INSTRUCTIONS           
C         GIVEN IN  SPMSF                                               
C OUTPUT PARAMETERS                                                     
C A     NUMERICAL FACTORIZATION OF MATRIX                               
C GROWTH NUMERICAL ELEMENT GROWTH. IF THIS MUCH .GT. 1, THEN THE        
C        COMPUTED DECOMPOSITION MAY BE THE DECOMPOSITION OF A MATRIX    
C        THAT IS NOT VERY CLOSE TO THE ORIGINAL MATRIX.                 
C STORAGE ALLOCATED -N COMPLEX LOCATION WHICH                           
C       ARE RETURNED                                                    
C ERROR STATES                                                          
C 1   N.LT.1     FATAL                                                  
C 10+K SINGULAR MATRIX OF RANK K   RECOVERABLE                          
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13HCSPMNF-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('CSPMNF-N.LT.1', 13, 1, 2)              
C/                                                                      
      CALL ENTER(1)                                                     
      ID = ISTKGT(N, 5)                                                 
       JLU=2*N+2                                                        
       CALL CS4MNF(N,IWORK,IWORK(N+2),IWORK(JLU),A,DD(ID),GROWTH,EPS)   
C/6S                                                                    
C     IF (NERROR(NERR) .NE. 0) CALL N5ERR(22HCSPMNF-SINGULAR MATRIX, 22,
C    1   NERR, 1)                                                       
C/7S                                                                    
      IF (NERROR(NERR) .NE. 0) CALL N5ERR('CSPMNF-SINGULAR MATRIX', 22, 
     1   NERR, 1)                                                       
C/                                                                      
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE CS4MNF(N, IL, IU, JU, U, ROW, G, EPS)                  
      INTEGER N, IL(N), IU(N), JU(N)                                    
      REAL G, EPS                                                       
      COMPLEX U(N), ROW(N)                                              
      INTEGER IMIN, JMIN, IMAX, JMAX, I, J                              
      INTEGER K                                                         
      REAL  CABS1,   BEF, AFT                                           
      COMPLEX DK, LI, TEMP                                              
      INTEGER TEMP1, TEMP2                                              
C       INPUT VARIABLES N,IL,IU,JU                                      
C       OUTPUT VARIABLES--   G,  U,                                     
C       PARAMETERS USED INTERNALLY--                                    
C FIA   * ROW - HOL S INTERMEDIATE VALUES IN CALCULATION OF  U AND L.   
C       *         SIZE = N.                                             
C RF       G      ELEMENT GROWTH                                        
C RN      EPS      LARGEST NONACCEPTABLE PIVOT                          
C  INTERNAL VARIABLES--                                                 
C    JMIN, JMAX - INDICES OF THE FIRST AND LAST POSITIONS IN A ROW TO   
C      BE EXAMINED.                                                     
C    SUM - USED IN CALCULATING  TMP.                                    
C  ******  FOR EACH ROW  ********************************************** 
      BEF = 0.0                                                         
      AFT = 0.0                                                         
      DO  8 K = 1, N                                                    
C  ******  SET THE INITIAL STRUCTURE OF ROW  ************************** 
         JMIN = IL(K)                                                   
         JMAX = IL(K+1)-1                                               
         DO  1 J = JMIN, JMAX                                           
            IF (CABS1(U(J)) .GT. BEF) BEF = CABS1(U(J))                 
            TEMP2 = JU(J)                                               
            ROW(TEMP2) = U(J)                                           
   1        CONTINUE                                                    
C  ******  ASSIGN THE KTH ROW OF L AND ADJUST ROW,   ***************    
         IMIN = IL(K)                                                   
         IMAX = IU(K)-2                                                 
         IF (IMIN .GT. IMAX) GOTO 5                                     
            DO  4 I = IMIN, IMAX                                        
               TEMP2 = JU(I)                                            
               LI = -ROW(TEMP2)                                         
               U(I) = LI                                                
               JMIN = IU(TEMP2)                                         
               JMAX = IL(TEMP2+1)-1                                     
               IF (JMIN .GT. JMAX) GOTO 3                               
                  DO  2 J = JMIN, JMAX                                  
                     TEMP1 = JU(J)                                      
                     ROW(TEMP1) = ROW(TEMP1)+LI*U(J)                    
   2                 CONTINUE                                           
   3           CONTINUE                                                 
   4           CONTINUE                                                 
C  ******  ASSIGN KTH ROW OF U AND DIAGONAL D, SET TMP(K)  ************ 
   5     IF (CABS1(ROW(K)) .LE. EPS) GOTO  9                            
         DK = (1.E0,0.0E0)/ROW(K)                                       
         JMIN = IU(K)                                                   
         U(JMIN-1) = DK                                                 
         AFT=AMAX1(AFT,CABS1(ROW(K)))                                   
         JMAX = IL(K+1)-1                                               
         IF (JMIN .GT. JMAX) GOTO 7                                     
            DO  6 J = JMIN, JMAX                                        
               TEMP1 = JU(J)                                            
               TEMP = ROW(TEMP1)                                        
               IF (CABS1(TEMP) .GT. AFT) AFT = CABS1(TEMP)              
               U(J) = TEMP*DK                                           
   6           CONTINUE                                                 
   7     CONTINUE                                                       
   8     CONTINUE                                                       
C  ******  NORMAL RETURN AND ERROR RETURNS  *************************** 
      G = AFT/BEF                                                       
      RETURN                                                            
C  ZERO DIAGONAL ELEMENT                                                
C/6S                                                                    
C  9  CALL SETERR(22HCS4MNF-SINGULAR MATRIX, 22, K+9, 1)                
C/7S                                                                    
   9  CALL SETERR('CS4MNF-SINGULAR MATRIX', 22, K+9, 1)                 
C/                                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE CSPSOL(N, MRP,MCP, IWORK,UL,  B, IB, NB)               
      INTEGER IB, NB, N                                                 
       INTEGER MRP(N), MCP(N), IWORK(1)                                 
      COMPLEX UL(1), B(IB, NB)                                          
      COMPLEX D(500)                                                    
      COMMON /CSTAK/ D                                                  
      INTEGER  ITMP, ISTKGT                                             
C FORWARD SOLVE FOR SPARSE MATRICES                                     
C INPUT PARAMETERS                                                      
C N     NUMBER OF EQUATIONS                                             
C MRP   INTEGER VECTOR OF ROW PERMUTATIONS                              
C MCP   INTEGER VECTOR,LENGTHN OR COLUMN PERMUTATIONS                   
C IWORK INTEGER VECTOR COMPUTED BY CSPF(ORM)NF                          
C UL    REAL ARRAY COMPUTED BY CSPF(ORM)NF                              
C IU    INTEGER VECTOR OF LENGTH N POINTING TO U IN LU                  
C       DECOMPOSITION,COMPUTED BY SPLU,SF(ORM)NF,OR SPCE                
C B     MATRIX OF RIGHT HAND SIDES                                      
C IB    ROW DIMENSION OF B MATRIX                                       
C NB    NUMBER OF RIGHT HAND SIDES                                      
C OUTPUT PARAMETERS                                                     
C B     THE SOLUTION MATRIX                                             
C STORAGE SPACE ALLOCATED AND DEALLOCATED -N COMPLEX LOCATIONS          
C ERROR CONDITIONS                                                      
C 1 N.LT.1    FATAL                                                     
C 2 IB.LT.N   FATAL                                                     
C 3 NB.LT.1   FATAL                                                     
C/6S                                                                    
C     IF (N.LT.1) CALL SETERR(13HCSPSOL-N.LT.1,13,1,2)                  
C     IF (IB.LT.N) CALL SETERR(14HCSPSOL-IB.LT.N,14,2,2)                
C     IF(NB.LT.1) CALL SETERR(14HCSPSOL-NB.LT.1,14,3,2)                 
C/7S                                                                    
      IF (N.LT.1) CALL SETERR('CSPSOL-N.LT.1',13,1,2)                   
      IF (IB.LT.N) CALL SETERR('CSPSOL-IB.LT.N',14,2,2)                 
      IF(NB.LT.1) CALL SETERR('CSPSOL-NB.LT.1',14,3,2)                  
C/                                                                      
      CALL ENTER(1)                                                     
      ITMP = ISTKGT(N, 5)                                               
      JLU=2*N+2                                                         
      CALL CS4SOL(N,MRP,MCP,IWORK,IWORK(JLU),UL, IWORK(N+2),            
     1  B,IB,NB,D(ITMP))                                                
       CALL LEAVE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE CS4SOL(N, R,C, IA, JA, A, IU,  B, IB, NB, TMP)         
      INTEGER IB, NB, N                                                 
      INTEGER R(N), IA(1), JA(1), IU(N), C(N)                           
      COMPLEX A(1), B(IB, NB), TMP(N)                                   
      INTEGER JJ, IR, JMIN, JMAX, NP1, I                                
      INTEGER J, K, IUI                                                 
      COMPLEX DK, SUM                                                   
C SPARSE FORWARD SOLVE                                                  
      NP1 = N+1                                                         
      DO  10 K = 1, NB                                                  
         DO  5 I = 1, N                                                 
            IR = R(I)                                                   
            JMIN = IA(I)                                                
            IUI = IU(I)-1                                               
            JMAX=IUI-1                                                  
            DK = A(IUI)                                                 
            SUM = B(IR, K)                                              
               IF (JMIN .GT. JMAX) GOTO 4                               
                  DO  2 J = JMIN, JMAX                                  
                     JJ = JA(J)                                         
                     SUM = SUM+A(J)*TMP(JJ)                             
   2                 CONTINUE                                           
   4        TMP(I) = SUM*DK                                             
   5        CONTINUE                                                    
         DO  9 IIB = 1, N                                               
            I = NP1-IIB                                                 
            JMIN = IU(I)                                                
            JMAX = IA(I+1)-1                                            
            SUM = TMP(I)                                                
            IF (JMIN .GT. JMAX) GOTO 8                                  
                  DO  7 J = JMIN, JMAX                                  
                     JUJ = JA(J)                                        
                     SUM = SUM-A(J)*TMP(JUJ)                            
   7                 CONTINUE                                           
   8        IIC = C(I)                                                  
            TMP(I) = SUM                                                
            B(IIC,K) = SUM                                              
   9        CONTINUE                                                    
   10     CONTINUE                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE SPMOR(N, IA, JA, C, IC)                                
C  THIS IS LINDA KAUFMANS SPARSE MATRIX PACKAGE                         
C  ROUTINES USED BY ALL THE SPARSE.S SPARSE.D AND SPARSE.C              
C  PACKAGES                                                             
C                                                                       
C  DECEMBER 9, 1982 (SLIGHT REVISION)                                   
C                                                                       
      INTEGER N                                                         
      INTEGER IA(1), JA(1), C(N), IC(N)                                 
      EXTERNAL I4YX                                                     
      DOUBLE PRECISION DD(500)                                          
      COMMON /CSTAK/ DD                                                 
      INTEGER IV, LV, FLAG, ISTKGT, ISTKQU, MAX                         
      INTEGER ISP(1000), NSP                                            
      EQUIVALENCE (DD(1), ISP(1))                                       
C MNEUMONIC -UNSYMMETRIC ORDER SCHEME                                   
C THIS SUBROUTINE USES THE MINIMUM DEGREE ORDERING                      
C SCHEME TO REORDER A MATRIX                                            
C INPUT PARAMETERS                                                      
C N      ORDER OF THE MATRIX                                            
C IA     INTEGER VECTOR,LENGTH N+1,WHICH POINTS TO                      
C        TO BEGININNING OF ITH ROW IN JA ARRAY                          
C JA     THE COLUMN INDICES OF THE NONZERO ELEMENTS IN THE              
C        SPARSE MATRIX                                                  
C OUTPUT PARAMETERS                                                     
C C      INTEGER VECTOR OF LENGTH N OF REORDERED COLUMNS(AND            
C        ROWS)                                                          
C IC     INTEGER VECTOR OF LENGTH NOF INVERTED ORDER                    
C        I.E. C(IC(I))=I                                                
C ERROR STATES                                                          
C 1      N.LT.1     FATAL                                               
C 2      STORAGE EXCEEDED IMMEDIATELY   FATAL                           
C N+K    KTH ROW OF MATRIX IS NULL                                      
C 2N+K   STORAGE EXCEED AT KTH ROW                                      
C MINIMUM STORAGE TAKEN FROM STACK-2N+M,WHERE                           
C M IS THE NUMBER OF NONZEROS IN SPARSE MATRIX                          
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13H SPMOR-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPMOR-N.LT.1', 13, 1, 2)              
C/                                                                      
      NSP = ISTKQU(2)                                                   
      IV = ISTKGT(NSP, 2)                                               
      MAX=(NSP-N)/2-1                                                   
      LV=IV+MAX                                                         
      IHEAD=LV+MAX                                                      
      FLAG=0                                                            
      CALL S4MDM(N,I4YX,MAX,ISP(IV),ISP(LV),ISP(IHEAD),C,IC,            
     1 ISP(IV),FLAG,IA,JA,1)                                            
      IF (FLAG .EQ. 0) GOTO 1                                           
C/6S                                                                    
C     IF (FLAG.GT.2*N+10)                                               
C    2   CALL SETERR(27H SPMOR-INSUFFICIENT STORAGE,27,FLAG,1)          
C     IF (FLAG.LE.N+10)                                                 
C    1   CALL SETERR(15H SPMOR-NULL ROW,15,FLAG,1)                      
C     IF (FLAG.GT.N+10.AND.FLAG.LE.10+2*N)                              
C    1   CALL SETERR(29H SPMOR-INCORRECT COLUMN INDEX,29,FLAG,1)        
C/7S                                                                    
      IF (FLAG.GT.2*N+10)                                               
     2   CALL SETERR(' SPMOR-INSUFFICIENT STORAGE',27,FLAG,1)           
      IF (FLAG.LE.N+10)                                                 
     1   CALL SETERR(' SPMOR-NULL ROW',15,FLAG,1)                       
      IF (FLAG.GT.N+10.AND.FLAG.LE.10+2*N)                              
     1   CALL SETERR(' SPMOR-INCORRECT COLUMN INDEX',29,FLAG,1)         
C/                                                                      
   1  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
       SUBROUTINE I4YX(I,J,K)                                           
C                                                                       
C THIS IS A DUMMY SUBROUTINE                                            
C                                                                       
       RETURN                                                           
       END                                                              
      SUBROUTINE SPFOR(N, IROW, C)                                      
      INTEGER N                                                         
      INTEGER C(N)                                                      
       EXTERNAL IROW                                                    
      DOUBLE PRECISION DD(500)                                          
      COMMON /CSTAK/ DD                                                 
      INTEGER IV, LV, FLAG, ISTKGT, ISTKQU, MAX                         
      INTEGER ISP(1), NSP                                               
      EQUIVALENCE (DD(1), ISP(1))                                       
C MNEUMONIC -UNSYMMETRIC ORDER SCHEME                                   
C THIS SUBROUTINE USES THE MINIMUM DEGREE ORDERING                      
C SCHEME TO REORDER A MATRIX                                            
C INPUT PARAMETERS                                                      
C N      ORDER OF THE MATRIX                                            
C IROW    FUNCTION DECLARED EXTERNAL IN MAIN PROGRAM WHICH              
C          RETURNS TO SPFOR THE COLUMN INDICES IN JCOL OF THE           
C          NUM NONZERO ELEMENTS IN THE ITH ROW. ONLY I                  
C          IS AN INPUT PARAMTER.                                        
C         CALL IROW(I, JCOL, NUM)                                       
C OUTPUT PARAMETERS                                                     
C C      INTEGER VECTOR OF LENGTH N OF REORDERED COLUMNS(AND            
C        ROWS)                                                          
C ERROR STATES                                                          
C 1      N.LT.1     FATAL                                               
C 2      STORAGE EXCEEDED IMMEDIATELY   FATAL                           
C N+K    KTH ROW OF MATRIX IS NULL                                      
C 2N+K   STORAGE EXCEED AT KTH ROW                                      
C MINIMUM STORAGE TAKEN FROM STACK-2N+M,WHERE                           
C M IS THE NUMBER OF NONZEROS IN SPARSE MATRIX                          
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13H SPFOR-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPFOR-N.LT.1', 13, 1, 2)              
C/                                                                      
      IC =ISTKGT(N,2)                                                   
      NSP = ISTKQU(2)                                                   
      IV = ISTKGT(NSP, 2)                                               
      MAX=NSP/2-N                                                       
      LV=IV+MAX                                                         
      IHEAD=LV+MAX                                                      
      NEXT=IHEAD+N                                                      
      FLAG=0                                                            
C/6S                                                                    
C     IF (MAX .LT. N) CALL SETERR(25H SPFOR-INSUFFICIENT SPACE, 25, 2, 2
C    1   )                                                              
C/7S                                                                    
      IF (MAX .LT. N) CALL SETERR(' SPFOR-INSUFFICIENT SPACE', 25, 2, 2 
     1   )                                                              
C/                                                                      
      CALL S4MDM(N,IROW,MAX,ISP(IV),ISP(LV),ISP(IHEAD),C,ISP(IC),       
     1 ISP(IV),FLAG,ISP,ISP,0)                                          
      IF (FLAG .EQ. 0) GOTO 1                                           
C/6S                                                                    
C     IF (FLAG.GT.2*N+10)                                               
C    2   CALL SETERR(27H SPFOR-INSUFFICIENT STORAGE,27,FLAG,1)          
C     IF (FLAG.LE.N+10)                                                 
C    1   CALL SETERR(15H SPFOR-NULL ROW,15,FLAG,1)                      
C     IF (FLAG.GT.N+10.AND.FLAG.LE.10+2*N)                              
C    1   CALL SETERR(29H SPFOR-INCORRECT COLUMN INDEX,29,FLAG,1)        
C/7S                                                                    
      IF (FLAG.GT.2*N+10)                                               
     2   CALL SETERR(' SPFOR-INSUFFICIENT STORAGE',27,FLAG,1)           
      IF (FLAG.LE.N+10)                                                 
     1   CALL SETERR(' SPFOR-NULL ROW',15,FLAG,1)                       
      IF (FLAG.GT.N+10.AND.FLAG.LE.10+2*N)                              
     1   CALL SETERR(' SPFOR-INCORRECT COLUMN INDEX',29,FLAG,1)         
C/                                                                      
   1  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
        SUBROUTINE  S4MDM                                               
     *     (N, JCOL, MAX, V,L, HEAD,LAST,NEXT, MARK, FLAG,IA,JA,MA)     
        INTEGER     V(1), L(1),  HEAD(1), LAST(1), NEXT(1),             
     *     MARK(1),  FLAG,  TAG, DMIN, VK,EK, IA(1),JA(1)               
         EXTERNAL JCOL                                                  
        EQUIVALENCE  (VK,EK)                                            
C                                                                       
C                                                                       
C    VARIABLES-                                                         
C                                                                       
C    ---------+-----------------------------+---------------------------
C    HEAD(D)  * VJ GE VJ HEAD OF D-LIST D                               
C             *  0 GE NO VERTEX IN D-LIST D                             
C                                                                       
C                                                                       
C             *                  VI UNELIMINATED VERTEX                 
C             *          VI IN EK           *       VI NOT IN EK        
C    ---------+-----------------------------+---------------------------
C    NEXT(VI) * UNDEFINED BUT NONNEGATIVE   * VJ GE VJ NEXT IN D-LIST   
C             *                             *  0 GE VI TAIL OF D-LIST   
C    ---------+-----------------------------+---------------------------
C    LAST(VI) * (NOT SET UNTIL M4DP)        * -D GE VI HEAD OF D-LIST D 
C             *-VK GE COMPUTE DEGREE        * VJ GE VJ LAST IN D-LIST   
C             * EJ GE VI PROTOTYPE OF EJ    *  0 GE VI NOT IN ANY D-LIST
C             *  0 GE DO NOT COMPUTE DEGREE *                           
C    ---------+-----------------------------+---------------------------
C    MARK(VI) * MARK(VK)                    * NONNEG TAG .LT. MARK(VK)  
C                                                                       
C                                                                       
C             *                   VI ELIMINATED VERTEX                  
C             *      EI ACTIVE ELEMENT      *           OTHERWISE       
C    ---------+-----------------------------+---------------------------
C    NEXT(VI) * -J GE VI WAS J-TH VERTEX    * -J GE VI WAS J-TH VERTEX  
C             *       TO BE ELIMINATED      *       TO BE ELIMINATED    
C    ---------+-----------------------------+---------------------------
C    LAST(VI) *  M GE SIZE OF EI = M        * UNDEFINED                 
C    ---------+-----------------------------+---------------------------
C    MARK(VI) * -M GE OVERLAP COUNT OF EI   * UNDEFINED                 
C             *       WITH EK = M           *                           
C             * OTHERWISE NONNEGATIVE TAG   *                           
C             *    .LT. MARK(VK)            *                           
C    ---------+-----------------------------+---------------------------
C                                                                       
C                                                                       
C----INITIALIZATION                                                     
        TAG = 0                                                         
        IF (MA.EQ.0)CALL  M4DIF                                         
     *     (N,JCOL,IA,LAST,MAX,V,L,HEAD,LAST,NEXT,MARK,TAG,FLAG,IBIG,MA)
        IF (MA.EQ.1)CALL  M4DIF                                         
     *     (N,JCOL,IA,JA,MAX,V,L,HEAD,LAST,NEXT,MARK,TAG,FLAG,IBIG,MA)  
        IF (FLAG.NE.0)  RETURN                                          
C                                                                       
        K = 0                                                           
        DMIN = 1                                                        
C                                                                       
C----WHILE  K .LT. IBIG  DO                                             
   1    IF (K.GE.IBIG)  GO TO 4                                         
C                                                                       
C------SEARCH FOR VERTEX OF MINIMUM DEGREE                              
   2      IF (HEAD(DMIN).GT.0)  GO TO 3                                 
            DMIN = DMIN + 1                                             
            GO TO 2                                                     
C                                                                       
C------REMOVE VERTEX VK OF MINIMUM DEGREE FROM DEGREE LIST              
   3      VK = HEAD(DMIN)                                               
          HEAD(DMIN) = NEXT(VK)                                         
          NVK=HEAD(DMIN)                                                
          IF (NVK.GT.0)  LAST(NVK) = -DMIN                              
C                                                                       
C------NUMBER VERTEX VK AND ADJUST TAG                                  
          K = K+1                                                       
          NEXT(VK) = -K                                                 
          LAST(EK) = DMIN - 1                                           
          TAG = TAG + LAST(EK)                                          
C                                                                       
C------FORM ELEMENT EK FROM UNELIMINATED NEIGHBORS OF VK                
          CALL  M4DM                                                    
     *       (VK,TAG, V,L, HEAD,LAST,NEXT, MARK)                        
C                                                                       
C------PURGE INACTIVE ELEMENTS AND DO MASS ELIMINATION                  
          CALL  M4DP                                                    
     *       (K,EK, V,L, HEAD,LAST,NEXT, MARK)                          
C                                                                       
C------UPDATE DEGREES OF UNELIMINATED VERTICES IN EK                    
          CALL  M4DU                                                    
     *       (EK,DMIN, V,L, HEAD,LAST,NEXT, MARK)                       
C                                                                       
          GO TO 1                                                       
C                                                                       
C----GENERATE INVERSE PERMUTATION FROM PERMUTATION                      
   4    DO 5 K=1,N                                                      
          NEXT(K) = -NEXT(K)                                            
          NK=NEXT(K)                                                    
   5      LAST(NK) = K                                                  
C                                                                       
        RETURN                                                          
        END                                                             
        SUBROUTINE  M4DIF                                               
     *     (N,JCOL,IA,JA,MAX,V,L,HEAD,LAST,NEXT,MARK,TAG,FLAG,IBIG,MA)  
        INTEGER   IA(1),JA(N),  V(N), L(N),  HEAD(N), LAST(N), NEXT(N), 
     *     MARK(N), TAG,  FLAG,  SFS, VI,DVI, VJ                        
         EXTERNAL JCOL                                                  
C                                                                       
C----INITIALIZE DEGREES, ELEMENT LISTS, AND DEGREE LISTS                
        DO 1 VI=1,N                                                     
          MARK(VI) = 1                                                  
          L(VI) = 0                                                     
   1      HEAD(VI) = 0                                                  
        SFS = N+1                                                       
C                                                                       
C----CREATE NONZERO STRUCTURE-                                          
C----FOR EACH NONZERO ENTRY A(VI,VJ) IN STRICT UPPER TRIANGLE           
         IBIG=N                                                         
        LIM=N/2+1                                                       
        DO 3 VI=1,N                                                     
           IF(MARK(VI).LT.0) GO TO 3                                    
           IF (MA.EQ.0) GO TO 103                                       
C                                                                       
C MATRIX INPUT                                                          
C                                                                       
              JMIN=IA(VI)                                               
              JMAX=IA(VI+1)-1                                           
              NUM=JMAX-JMIN+1                                           
              GO TO 133                                                 
 103     CONTINUE                                                       
           CALL JCOL(VI,JA,NUM)                                         
           JMIN=1                                                       
           JMAX=NUM                                                     
 133       CONTINUE                                                     
           IF (NUM.LT.1) GO TO 102                                      
          IF (NUM.LE.LIM) GO TO 10                                      
             CALL V4ELIM(N,L,V,VI,MARK,IBIG)                            
             GO TO 3                                                    
 10       CONTINUE                                                      
          M=MARK(VI)-1                                                  
          LKK=L(VI)                                                     
          DO 2 J=JMIN,JMAX                                              
            VJ = JA(J)                                                  
            IF (VJ.LT.1.OR.VJ.GT.N) GO TO 105                           
            IF (VI.EQ.VJ.OR.MARK(VJ).LT.0)  GO TO 2                     
            IF (VI.LT.VJ)GO TO 30                                       
                 LK=LKK                                                 
                  IF (M.EQ.0) GO TO 12                                  
                 DO 11 I=1,M                                            
                    IF (V(LK).EQ.VJ) GO TO 2                            
                    LK=L(LK)                                            
  11              CONTINUE                                              
  12             CONTINUE                                               
  30          IF (SFS.GE.MAX)  GO TO 101                                
C                                                                       
C------ENTER VJ IN ELEMENT LIST FOR VI                                  
              MARK(VI) = MARK(VI) + 1                                   
              V(SFS) = VJ                                               
              L(SFS) = L(VI)                                            
              L(VI) = SFS                                               
              SFS = SFS+1                                               
C                                                                       
C------ENTER VI IN ELEMENT LIST FOR VJ                                  
              MARK(VJ) = MARK(VJ) + 1                                   
              V(SFS) = VI                                               
              L(SFS) = L(VJ)                                            
              L(VJ) = SFS                                               
              SFS = SFS+1                                               
              IF (MARK(VJ).GE.LIM)CALL V4ELIM(N,L,V,VJ,MARK,IBIG)       
              IF (MARK(VI).LT.LIM) GO TO 2                              
                  CALL V4ELIM(N,L,V,VI,MARK,IBIG)                       
                  GO TO 3                                               
   2        CONTINUE                                                    
   3      CONTINUE                                                      
C                                                                       
C----CREATE DEGREE LISTS AND INITIALIZE MARK VECTOR                     
        IB=IBIG+1                                                       
        DO 4 VI=1,N                                                     
          IF (MARK(VI).GT.0) GO TO 104                                  
              NEXT(VI)=-IB                                              
              IB=IB+1                                                   
              GO TO 4                                                   
 104      CONTINUE                                                      
          DVI = MARK(VI)                                                
          NEXT(VI) = HEAD(DVI)                                          
          HEAD(DVI) = VI                                                
          LAST(VI) = -DVI                                               
          NVI=NEXT(VI)                                                  
          IF (NVI.GT.0)  LAST(NVI) = VI                                 
   4      MARK(VI) = TAG                                                
C                                                                       
        RETURN                                                          
C                                                                       
C ** ERROR-  INSUFFICIENT STORAGE                                       
 101    FLAG = 2*N + VI+10                                              
        RETURN                                                          
 102    FLAG = 10+VI                                                    
        RETURN                                                          
C INCORRECT COLUMN INDEX                                                
 105    FLAG=10+N+VI                                                    
        RETURN                                                          
        END                                                             
           SUBROUTINE V4ELIM(N,L,V,IV,MARK,IBIG)                        
           INTEGER L(N),V(N),MARK(N)                                    
C THIS SUBROUTINE ELIMINATES VERTEX IV FROM THE LINK LIST               
C STRUCTURE                                                             
C                                                                       
      M=MARK(IV)-1                                                      
      IF (M.LT.1) GO TO 30                                              
      LK=L(IV)                                                          
      DO 20 I=1,M                                                       
          LN=V(LK)                                                      
          MARK(LN)=MARK(LN)-1                                           
  10      LN1=LN                                                        
          LN=L(LN)                                                      
          IF (V(LN).NE.IV) GO TO 10                                     
          L(LN1)=L(LN)                                                  
          LK=L(LK)                                                      
  20   CONTINUE                                                         
  30   IBIG=IBIG-1                                                      
       MARK(IV)=-1                                                      
       RETURN                                                           
       END                                                              
        SUBROUTINE  M4DM                                                
     *     (VK,TAG, V,L, HEAD,LAST,NEXT, MARK)                          
        INTEGER  VK, TAG,  V(1), L(1),  HEAD(1), LAST(1), NEXT(1),      
     *     MARK(1),  S,LS,VS,ES, B,LB,VB, BLP,BLPMAX                    
        EQUIVALENCE  (VS, ES)                                           
C                                                                       
C  M4DM -- FORM ELEMENT FROM UNELIMINATED NEIGHBORS OF VK               
C                                                                       
C----TAG VK AND INITIALIZE LIST OF UNELIMINATED NEIGHBORS               
        MARK(VK) = TAG                                                  
        LIST = 0                                                        
C                                                                       
C----FOR EACH VERTEX/ELEMENT VS/ES IN ELEMENT LIST OF VK                
        LS = VK                                                         
   1    S = LS                                                          
        LS = L(S)                                                       
        IF (LS.EQ.0)  GO TO 5                                           
          VS = V(LS)                                                    
          IF (NEXT(VS).LT.0)  GO TO 2                                   
C                                                                       
C------IF VS IS UNELIMINATED VERTEX, THEN TAG AND ADD TO LIST OF        
C------UNELIMINATED NEIGHBORS                                           
            MARK(VS) = TAG                                              
            L(S) = L(LS)                                                
            L(LS) = LIST                                                
            LIST = LS                                                   
            LS = S                                                      
            GO TO 4                                                     
C                                                                       
C------IF ES IS ACTIVE ELEMENT, THEN ...                                
C--------FOR EACH VERTEX VB IN BOUNDARY LIST OF ELEMENT ES              
   2        LB = ES                                                     
            BLPMAX = LAST(ES)                                           
            DO 3 BLP=1,BLPMAX                                           
              B = LB                                                    
              LB = L(B)                                                 
              VB = V(LB)                                                
C                                                                       
C----------IF VB IS UNTAGGED VERTEX, THEN TAG AND ADD TO LIST OF        
C----------UNELIMINATED NEIGHBORS                                       
              IF (MARK(VB).GE.TAG)  GO TO 3                             
                MARK(VB) = TAG                                          
                L(B) = L(LB)                                            
                L(LB) = LIST                                            
                LIST = LB                                               
                LB = B                                                  
   3          CONTINUE                                                  
C                                                                       
C--------MARK ES INACTIVE                                               
            MARK(ES) = TAG                                              
C                                                                       
   4      GO TO 1                                                       
C                                                                       
C----ATTACH LIST OF UNELIMINATED NEIGHBORS TO EK                        
   5    L(VK) = LIST                                                    
C                                                                       
        RETURN                                                          
        END                                                             
        SUBROUTINE  M4DP                                                
     *     (K,EK, V,L, HEAD,LAST,NEXT, MARK)                            
        INTEGER  EK,  V(1), L(1),  HEAD(1), LAST(1), NEXT(1),  MARK(1), 
     *     TAG, FREE, LI,VI,LVI,EVI, S,LS,ES, ILP,ILPMAX                
C                                                                       
C  M4DP -- PURGE INACTIVE ELEMENTS AND DO MASS ELIMINATION              
C                                                                       
C----INITIALIZE TAG AND LIST OF PROTOTYPE VERTICES                      
        TAG = MARK(EK)                                                  
        LIST = 0                                                        
C                                                                       
C----FOR EACH VERTEX VI IN EK                                           
        LI = EK                                                         
        ILPMAX = LAST(EK)                                               
        IF (ILPMAX.LE.0)  GO TO 12                                      
        DO 11 ILP=1,ILPMAX                                              
          I = LI                                                        
          LI = L(I)                                                     
          VI = V(LI)                                                    
C                                                                       
C------REMOVE VI FROM DEGREE LIST                                       
          LIVI=LAST(VI)                                                 
          IF (LIVI.EQ.0)  GO TO 3                                       
            IF (LIVI.GT.0)  GO TO 1                                     
              MLIVI= -LIVI                                              
              HEAD(MLIVI) = NEXT(VI)                                    
              GO TO 2                                                   
   1          NEXT(LIVI) = NEXT(VI)                                     
   2        NIVI=NEXT(VI)                                               
            IF (NIVI.GT.0)  LAST(NIVI) = LAST(VI)                       
C                                                                       
C------REMOVE INACTIVE ITEMS FROM ELEMENT LIST OF VI                    
   3      LS = VI                                                       
   4      S = LS                                                        
          LS = L(S)                                                     
          IF (LS.EQ.0)  GO TO 6                                         
            ES = V(LS)                                                  
            IF (MARK(ES).LT.TAG)  GO TO 5                               
              FREE = LS                                                 
              L(S) = L(LS)                                              
              LS = S                                                    
   5        GO TO 4                                                     
C                                                                       
C------IF VI IS INTERIOR VERTEX, THEN REMOVE FROM LIST AND ELIMINATE    
   6      LVI = L(VI)                                                   
          IF (LVI.NE.0)  GO TO 7                                        
            L(I) = L(LI)                                                
            LI = I                                                      
C                                                                       
            K = K+1                                                     
            NEXT(VI) = -K                                               
            LAST(EK) = LAST(EK) - 1                                     
            GO TO 11                                                    
C                                                                       
C------ELSE ...                                                         
C--------CLASSIFY VERTEX VI-                                            
   7        IF (L(LVI).NE.0)  GO TO 9                                   
              EVI = V(LVI)                                              
              IF (NEXT(EVI).GE.0)  GO TO 9                              
                IF (MARK(EVI).LT.0)  GO TO 8                            
C                                                                       
C----------IF VI IS PROTOTYPE VERTEX, THEN MARK AS SUCH, INITIALIZE     
C----------OVERLAP COUNT FOR CORRESPONDING ELEMENT, AND MOVE VI TO LIST 
C----------OF PROTOTYPE VERTICES                                        
                  LAST(VI) = EVI                                        
                  MARK(EVI) = -1                                        
                  L(I) = L(LI)                                          
                  L(LI) = LIST                                          
                  LIST = LI                                             
                  LI = I                                                
                  GO TO 10                                              
C                                                                       
C----------ELSE IF VI IS DUPLICATE VERTEX, THEN MARK AS SUCH AND ADJUST 
C----------OVERLAP COUNT FOR CORRESPONDING ELEMENT                      
   8              LAST(VI) = 0                                          
                  MARK(EVI) = MARK(EVI) - 1                             
                  GO TO 10                                              
C                                                                       
C----------ELSE MARK VI TO COMPUTE DEGREE                               
   9              LAST(VI) = -EK                                        
C                                                                       
C--------INSERT EK IN ELEMENT LIST OF VI                                
  10        V(FREE) = EK                                                
            L(FREE) = L(VI)                                             
            L(VI) = FREE                                                
  11      CONTINUE                                                      
C                                                                       
C----APPEND LIST OF PROTOTYPE VERTICES TO END OF BOUNDARY LIST          
  12    L(LI) = LIST                                                    
C                                                                       
        RETURN                                                          
        END                                                             
        SUBROUTINE  M4DU                                                
     *     (EK,DMIN, V,L, HEAD,LAST,NEXT, MARK)                         
        INTEGER  EK, DMIN,  V(1), L(1),  HEAD(1), LAST(1), NEXT(1),     
     *     MARK(1),  TAG, VI,EVI,DVI, S,VS,ES, B,VB, ILP,ILPMAX,        
     *     BLP,BLPMAX                                                   
        EQUIVALENCE  (VS, ES)                                           
C                                                                       
C  M4DU -- UPDATE DEGREES OF UNELIMINATED VERTICES IN EK                
C                                                                       
C----INITIALIZE TAG                                                     
        TAG = MARK(EK) - LAST(EK)                                       
C                                                                       
C----FOR EACH VERTEX VI IN EK                                           
        I = EK                                                          
        ILPMAX = LAST(EK)                                               
        IF (ILPMAX.LE.0)  GO TO 11                                      
        DO 10 ILP=1,ILPMAX                                              
          I = L(I)                                                      
          VI = V(I)                                                     
          IF (LAST(VI))  1, 10, 8                                       
C                                                                       
C------IF VI NEITHER PROTOTYPE NOR DUPLICATE VERTEX, THEN MERGE ELEMENTS
C------TO COMPUTE DEGREE-                                               
   1        TAG = TAG + 1                                               
            DVI = LAST(EK)                                              
C                                                                       
C--------FOR EACH VERTEX/ELEMENT VS/ES IN ELEMENT LIST OF VI            
            S = L(VI)                                                   
   2        S = L(S)                                                    
            IF (S.EQ.0)  GO TO 9                                        
              VS = V(S)                                                 
              IF (NEXT(VS).LT.0)  GO TO 3                               
C                                                                       
C----------IF VS IS UNELIMINATED VERTEX, THEN TAG AND ADJUST DEGREE     
                MARK(VS) = TAG                                          
                DVI = DVI + 1                                           
                GO TO 5                                                 
C                                                                       
C----------IF ES IS ACTIVE ELEMENT, THEN EXPAND-                        
C------------CHECK FOR OUTMATCHED VERTEX                                
   3            IF (MARK(ES).LT.0)  GO TO 6                             
C                                                                       
C------------FOR EACH VERTEX VB IN ES                                   
                B = ES                                                  
                BLPMAX = LAST(ES)                                       
                DO 4 BLP=1,BLPMAX                                       
                  B = L(B)                                              
                  VB = V(B)                                             
C                                                                       
C--------------IF VB IS UNTAGGED, THEN TAG AND ADJUST DEGREE            
                  IF (MARK(VB).GE.TAG)  GO TO 4                         
                    MARK(VB) = TAG                                      
                    DVI = DVI + 1                                       
   4              CONTINUE                                              
C                                                                       
   5          GO TO 2                                                   
C                                                                       
C------ELSE IF VI IS OUTMATCHED VERTEX, THEN ADJUST OVERLAPS BUT DO NOT 
C------COMPUTE DEGREE                                                   
   6        LAST(VI) = 0                                                
            MARK(ES) = MARK(ES) - 1                                     
   7        S = L(S)                                                    
            IF (S.EQ.0)  GO TO 10                                       
              VS = V(S)                                                 
              IF (MARK(ES).LT.0)  MARK(ES) = MARK(ES) - 1               
              GO TO 7                                                   
C                                                                       
C------ELSE IF VI IS PROTOTYPE VERTEX, THEN CALCULATE DEGREE BY         
C------INCLUSION/EXCLUSION AND RESET OVERLAP COUNT                      
   8        EVI = LAST(VI)                                              
            DVI = LAST(EK) + LAST(EVI) + MARK(EVI)                      
            MARK(EVI) = 0                                               
C                                                                       
C------INSERT VI IN APPROPRIATE DEGREE LIST                             
   9      NEXT(VI) = HEAD(DVI)                                          
          HEAD(DVI) = VI                                                
          LAST(VI) = -DVI                                               
          NIVI=NEXT(VI)                                                 
          IF (NIVI.GT.0)  LAST(NIVI) = VI                               
          IF (DVI.LT.DMIN)  DMIN = DVI                                  
C                                                                       
  10      CONTINUE                                                      
C                                                                       
  11    RETURN                                                          
        END                                                             
      SUBROUTINE SPMSF(N,MRP, INMCP, IA, JA, IWORK, IWMAX, IFILL)       
      INTEGER N                                                         
      INTEGER MRP(N), INMCP(N), IA(1), JA(1), IWORK(IWMAX)              
      INTEGER IFILL                                                     
      DOUBLE PRECISION DD(500)                                          
      COMMON /CSTAK/ DD                                                 
      INTEGER IM, ISTKGT                                                
      INTEGER I, K, Q                                                   
      INTEGER ISP(1000)                                                 
      LOGICAL TEMP1                                                     
      EQUIVALENCE (DD(1), ISP(1))                                       
C       SYMBOLIC FACTORIZATION OF A MATRIX                              
C INPUT PARAMTERS                                                       
C N     ORDER OF SYSTEM                                                 
C MRP   INTEGER VECTOR LENGTH N, ROW ORDER                              
C INMCP  INTEGER VECTOR LENGTH N,INVERSE COLUMN ORDER                   
C IA    INTEGER VECTOR, LENGTH N+1, POINTS TO BEGINNING                 
C        OF EACH ROW IN JA                                              
C JA    INTEGER VECTOR-COLUMN INDICES OF ARRAY                          
C IWMAX LENGTH OF WORK STACK, SHOULD BE AT LEAST 2N+2+IA(N+1)           
C       PREFERABLY TWICE THAT SIZE                                      
C OUTPUT PARAMETERS                                                     
C IWORK     INTEGER VECTOR  WHICH HAS THE INFORMATION OF THE            
C           SYMBOLIC FACTORIZATION. FIRST N+1 QUANTITIES INDICATE       
C           THE BEGINNING OF EACH L ROW IN SYMBOLIC FACT. NEXT          
C           N+1 GIVE BEGINNING OF EACH L ROW, AFTER THAT COMES          
C           THE COLUMN INDICES OF THE FACTORIZATION                     
C IFILL     AMOUNT OF STORAGE NEEDED TO STORE FULL LU FACTOR-           
C           IZATION OF A MATRIX                                         
C ERROR STATES                                                          
C 1    N.LT.1     FATAL                                                 
C 2    MRP NOT IN 1 THROUGH N     FATAL                                 
C 3     MCP NOT IN 1 THROUGH N  FATAL                                   
C 4 IWMAX LESS TAN 2N+1+IA(N+1)                                         
C 10+K  KTH ROW IN A IS NULL     FATAL                                  
C 10+N+K DUPLICATE ENTRY IN ROW K   FATAL                               
C 10+2N+K STORAGE EXCEEDED WHILE PROCESSING ROW K    RECOVERABLE        
C 10+3N+K NULL PIVOT AT ROW K FATAL                                     
C STORAGE TAKEN FROM PORT STACK-2N+1 INTEGER LOCATIONS                  
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13H SPMSF-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPMSF-N.LT.1', 13, 1, 2)              
C/                                                                      
      CALL ENTER(1)                                                     
      Q=ISTKGT(2*N+1,2)                                                 
      IM=Q+N+1                                                          
      DO  1 I = 1, N                                                    
         TEMP1 = MRP(I) .LT. 1                                          
         IF (.NOT. TEMP1) TEMP1 = MRP(I) .GT. N                         
C/6S                                                                    
C        IF (TEMP1) CALL SETERR(23H SPMSF-MRP OUT OF RANGE,23,2,2)      
C/7S                                                                    
         IF (TEMP1) CALL SETERR(' SPMSF-MRP OUT OF RANGE',23,2,2)       
C/                                                                      
         K = INMCP(I)                                                   
         TEMP1 = K .LT. 1                                               
         IF (.NOT. TEMP1) TEMP1 = K .GT. N                              
C/6S                                                                    
C        IF (TEMP1) CALL SETERR(23H SPMSF-MCP OUT OF RANGE,23,3,2)      
C/7S                                                                    
         IF (TEMP1) CALL SETERR(' SPMSF-MCP OUT OF RANGE',23,3,2)       
C/                                                                      
   1     CONTINUE                                                       
C/6S                                                                    
C     IF (IWMAX.LT.2*N+1+IA(N+1)) CALL SETERR(                          
C    122H SPMSF-IWMAX TOO SMALL,22,4,2)                                 
C/7S                                                                    
      IF (IWMAX.LT.2*N+1+IA(N+1)) CALL SETERR(                          
     1' SPMSF-IWMAX TOO SMALL',22,4,2)                                  
C/                                                                      
      JLU=2*N+2                                                         
       IAMAX=IWMAX-2*N-2                                                
      CALL S4MSF(N,MRP,INMCP,IA,JA,IWORK,IWORK(N+2),IWORK(JLU),IAMAX,   
     1 ISP(Q),ISP(IM),IERR)                                             
C/6S                                                                    
C     IF (IERR.GT.10+3*N) CALL SETERR(17H SPMSF-NULL PIVOT,             
C    1   17,IERR,2)                                                     
C/7S                                                                    
      IF (IERR.GT.10+3*N) CALL SETERR(' SPMSF-NULL PIVOT',              
     1   17,IERR,2)                                                     
C/                                                                      
      IF (IERR.LT.10+2*N) GO TO 3                                       
C/6S                                                                    
C       CALL SETERR(23H SPMSF-STORAGE EXCEEDED,23,IERR,1)               
C/7S                                                                    
        CALL SETERR(' SPMSF-STORAGE EXCEEDED',23,IERR,1)                
C/                                                                      
        CALL LEAVE                                                      
        RETURN                                                          
C/6S                                                                    
C3    IF(IERR.GT.10+N)CALL SETERR(                                      
C    1  34H SPMSF-INVALID COLUMN INDEX IN ROW,34,IERR,2)                
C      IF (IERR.GT.0) CALL SETERR(20H SPMSF-NULL ROW IN A,              
C    1   20,IERR,2)                                                     
C/7S                                                                    
 3    IF(IERR.GT.10+N)CALL SETERR(                                      
     1  ' SPMSF-INVALID COLUMN INDEX IN ROW',34,IERR,2)                 
       IF (IERR.GT.0) CALL SETERR(' SPMSF-NULL ROW IN A',               
     1   20,IERR,2)                                                     
C/                                                                      
           IFILL=IWORK(N+1)-1                                           
          CALL LEAVE                                                    
   4  RETURN                                                            
      END                                                               
      SUBROUTINE S4MSF(N,R,IC,IA,JA,IL,IU,JU,JUMAX,Q,IM,IERR)           
      INTEGER N, R(1), IC(1), IA(1), JA(1), IL(1)                       
      INTEGER IU(1), JU(1), JUMAX, Q(1), IM(1)                          
      INTEGER QM, VJ, JMIN, JMAX, I, J                                  
      INTEGER K, M, JUPTR                                               
      INTEGER TEMP                                                      
C       --------------------------------------------------------------- 
C    V.   PARAMETERS                                                    
C         FOLLOWING IS A LIST OF PARAMETERS TO THE PROGRAMS.  NAMES ARE 
C    UNIFORM AMONG THE VARIOUS SUBROUTINES.  CLASS ABBREVIATIONS ARE--  
C       V - SUPPLIES A VALUE TO A SUBROUTINE                            
C       R - CONTAINS A RESULT RETURNED BY A SUBROUTINE                  
C       I - IS USED INTERNALLY BY A SUBROUTINE                          
C       A - IS AN ARRAY                                                 
C       N - IS AN INTEGER VARIABLE                                      
C       F - IS A DOUBLE PRECISION VARIABLE.                             
C CLASS * PARAMETER                                                     
C ----- * ---------                                                     
C NVA   * IA    - POINTERS TO FIRST ELEMENTS OF EACH ROW IN  A.         
C       *           SIZE = N+1.                                         
C NVA   * IC    - INVERSE OF THE ORDERING OF THE COLUMNS OF  M.  FOR    
C       *           EXAMPLE, IF COLUMN 1 IS THE 5TH COLUMN AFTER        
C       *           REORDERING, THEN  IC(1)=5.                          
C       *           SIZE = N.                                           
C NVRA  * IL    - POINTERS TO THE FIRST ELEMENTS OF EACH ROW IN  L.     
C       *           SIZE = N+1.                                         
C NVRA  * IU    - POINTERS TO THE FIRST ELEMENTS OF EACH ROW IN  U.     
C       *           SIZE = N.                                           
C NVA   * JA    - COLUMN NUMBER CORRESPONDING TO EACH ELEMENT OF  A.    
C       *           SIZE = SIZE OF  A.                                  
C NVRA  * JU    - COLUMN NUMBER CORRESPONDING TO EACH ELEMENT OF  U.    
C                 AND L                                                 
C NV    * JUMAX - DECLARED DIMENSION OF  JU.                            
C NV    * N     - NUMBER OF ROWS/COLUMNS IN MATRIX  M.                  
C NVA   * R     - ORDERING OF THE ROWS OF  M.                           
C       *           SIZE = N.                                           
C       --------------------------------------------------------------- 
C*** SYMBOLIC LU-FACTORIZATION OF A NONSYMMETRIC SPARSE MATRIX          
C       INPUT VARIABLES-- N, R, IC, IA, JA,  JUMAX.                     
C       OUTPUT VARIABLES-- IL,  IU, JU, .                               
C       PARAMETERS USED INTERNALLY--                                    
C NIA   * Q     - SUPPOSE  MO  IS THE RESULT OF REORDERING  M.  IF      
C       *           PROCESSING OF THE ITH ROW OF  MO  (HENCE THE ITH    
C       *           ROWS OF  L AND U) IS BEING DONE,  Q(J)  IS          
C       *           INITIALLY NONZERO IF  MO(I,J) IS NONZERO.  SINCE    
C       *           VALUES NEED NOT BE STORED, EACH ENTRY POINTS TO THE 
C       *           NEXT NONZERO.  FOR EXAMPLE, IF N=9 AND THE 5TH ROW  
C       *           OF  MO  IS                                          
C       *              0 X X 0 X 0 0 X 0                                
C       *           THEN  Q  WILL INITIALLY BE                          
C       *              A 3 5 A 8 A A 10 A 2         (A - ARBITRARY).    
C       *           Q(N+1)  POINTS TO THE FIRST NONZERO IN THE ROW AND  
C       *           THE LAST NONZERO POINTS TO N+1.  AS THE ALGORITHM   
C       *           PROCEEDS, OTHER ELEMENTS OF  Q  ARE INSERTED IN THE 
C       *           LIST BECAUSE OF FILLIN.                             
C       *           SIZE = N+1.                                         
C NIA   * IM    - AT EACH STEP IN THE FACTORING,  IM(I)  IS THE LAST    
C       *           ELEMENT OF THE ITH ROW OF  U  WHICH NEEDS TO BE     
C       *           CONSIDERED IN COMPUTING FILLIN.                     
C       *           SIZE = N.                                           
C  INTERNAL VARIABLES--                                                 
C    JLPTR - POINTS TO THE LAST POSITION USED IN  JU FOR L.             
C    JUPTR - POINTS TO THE LAST POSITION USED IN  JU.                   
C    JMIN,JMAX - ARE THE INDICES IN  A OR U  OF THE FIRST AND LAST      
C                ELEMENTS TO BE EXAMINED IN A GIVEN ROW.                
C                FOR EXAMPLE,  JMIN=IA(K), JMAX=IA(K+1)-1.              
C  ******  INITIALIZE POINTERS  *************************************** 
      IL(1) = 1                                                         
      JUPTR = 0                                                         
      IERR= 0                                                           
      IU(1) = 1                                                         
       NP1=N+1                                                          
C  ******  FOR EACH ROW OF L AND U  *********************************** 
      DO  13 K = 1, N                                                   
C  ******  SET Q TO THE REORDERED ROW OF A  *************************** 
         Q(NP1) = NP1                                                   
         M=NP1                                                          
         TEMP = R(K)                                                    
         JMIN = IA(TEMP)                                                
         JMAX = IA(TEMP+1)-1                                            
         IF (JMIN .GT. JMAX) GOTO  14                                   
         DO  3 J = JMIN, JMAX                                           
            TEMP = JA(J)                                                
            VJ = IC(TEMP)                                               
C CHECK IF COLUMN ENTRY IS BETWEEN 1 AND N                              
            IF (VJ.LT.1.OR.VJ.GT.N) GO TO 15                            
            IF (M.GT.VJ)M=NP1                                           
 1            QM=Q(M)                                                   
              IF(QM.GE.VJ) GO TO 2                                      
              M=QM                                                      
              GO TO 1                                                   
   2        IF (QM .EQ. VJ) GOTO  15                                    
            Q(M) = VJ                                                   
            Q(VJ) = QM                                                  
   3        CONTINUE                                                    
C  ******  FOR EACH ENTRY IN THE LOWER TRIANGLE  ********************** 
         I = N+1                                                        
   4        I = Q(I)                                                    
            IF (I .GE. K) GOTO  10                                      
C  ******  L(K,I) WILL BE NONZERO, SO ADD IT TO JL  ******************* 
            JUPTR = JUPTR+1                                             
            IF (JUPTR .GT. JUMAX) GOTO  17                              
            JU(JUPTR) = I                                               
            QM = I                                                      
C  ******  INSPECT ITH ROW FOR FILLIN, ADJUST IM IF POSSIBLE  ********* 
            JMIN = IU(I)                                                
            JMAX = IM(I)                                                
            IF (JMIN .GT. JMAX) GOTO 9                                  
               DO  8 J = JMIN, JMAX                                     
                  VJ = JU(J)                                            
                  IF (VJ .EQ. K) IM(I) = J                              
   5                 M = QM                                             
                     QM = Q(M)                                          
                     IF (QM .GE. VJ) GOTO  6                            
                     GOTO  5                                            
   6              IF (QM .EQ. VJ) GOTO 7                                
                     Q(M) = VJ                                          
                     Q(VJ) = QM                                         
                     QM = VJ                                            
   7              CONTINUE                                              
   8              CONTINUE                                              
   9        CONTINUE                                                    
C  ******  CHECK FOR NULL PIVOT  ************************************** 
            GOTO  4                                                     
  10     IF (I .NE. K) GOTO  16                                         
         JUPTR = JUPTR+1                                                
         IF (JUPTR .GT. JUMAX) GOTO  17                                 
         JU(JUPTR) = I                                                  
         IU(K) = JUPTR+1                                                
C  ******  REMAINING ELEMENTS OF Q DEFINE STRUCTURE OF U(K, )  ******** 
  11        I = Q(I)                                                    
            IF (I .GT. N) GOTO  12                                      
            JUPTR = JUPTR+1                                             
            IF (JUPTR .GT. JUMAX) GOTO  17                              
            JU(JUPTR) = I                                               
C  ******  GET READY FOR NEXT ROW  ************************************ 
            GOTO  11                                                    
  12     IM(K) = JUPTR                                                  
         IL(K+1) = JUPTR+1                                              
  13     CONTINUE                                                       
      RETURN                                                            
C  NULL ROW IN A                                                        
  14   IERR=R(K)+10                                                     
      RETURN                                                            
C  INVALID ENTRY IN ROW OF A                                            
 15   IERR= 10+N+K                                                      
      RETURN                                                            
C  NULL PIVOT                                                           
  16  IERR=10+K+3*N                                                     
      RETURN                                                            
C  JL STORAGE EXCEEDED                                                  
  17  IERR=2*N+K+10                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE  SPFSF(N, MRP, MCP, JROW, IWORK, IWMAX, IFILL)         
      INTEGER N                                                         
      INTEGER MRP(N), MCP(N), IWORK(IWMAX)                              
      EXTERNAL JROW                                                     
      INTEGER IFILL, INDEX                                              
      DOUBLE PRECISION DD(500)                                          
      COMMON /CSTAK/ DD                                                 
      INTEGER IM, ISTKGT                                                
      INTEGER ISTKQU,  I, K, Q                                          
      INTEGER ISP(1000)                                                 
      LOGICAL TEMP1                                                     
      EQUIVALENCE (DD(1), ISP(1))                                       
C       SYMBOLIC FACTORIZATION OF A MATRIX                              
C INPUT PARAMTERS                                                       
C N     ORDER OF SYSTEM                                                 
C MRP   INTEGER VECTOR LENGTH N, ROW ORDER                              
C MCP    INTEGER VECTOR LENGTH N,COLUMN ORDER                           
C JROW   SUBROUTINE,DECLARED EXTERNAL IN THE MAIN PROGRAM               
C        WHCIH HAS THE CALLING SEQUENCE JROW(I,JCOL,NUM)                
C         WHICH HAS INPUT PARAMETER I AND OUTPUT PARAMETERS             
C         JCOL AND NUM WHICH RETURNS IN JCOL THE NUM COLUMN             
C         INDICES CORRESPONDING TO THE NUM NONZERO ENTRIES              
C         OF THE ITH ROW OF THE MATRIX                                  
C IWMAX LENGTH OF WORK STACK, SHOULD BE AT LEAST 3N+2                   
C       PREFERABLY TWICE THAT SIZE                                      
C OUTPUT PARAMTERS                                                      
C IWORK     INTEGER VECTOR  WHICH HAS THE INFORMATION OF THE            
C           SYMBOLIC FACTORIZATION. FIRST N+1 QUANTITIES INDICATE       
C           THE BEGINNING OF EACH L ROW IN SYMBOLIC FACT. NEXY          
C           N+1 GIVE BEGINNING OF EACH L ROW, AFTER THAT COMES          
C           THE COLUMN INDICES OF THE FACTORIZATION                     
C IFILL     AMOUNT OF STORAGE NEEDED TO STORE FULL LU FACTOR-           
C           IZATION OF A MATRIX                                         
C ERROR STATES                                                          
C 1    N.LT.1     FATAL                                                 
C 2    R NOT IN 1 THROUGH N     FATAL                                   
C 3    ELEMENTS OF C NOT IN 1 THROUGH N                                 
C 4 IWMAX LESS THAN 3N+1                                                
C 10+K  KTH ROW IN A IS NULL     FATAL                                  
C 10+N+K DUPLICATE ENTRY IN ROW K   FATAL                               
C 10+2N+K STORAGE EXCEEDED WHILE PROCESSING ROW K    RECOVERABLE        
C 10+3N+K NULL PIVOT AT ROW K FATAL                                     
C STORAGE TAKEN FROM PORT STACK-3N INTEGER LOCATIONS                    
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(13H SPFSF-N.LT.1, 13, 1, 2)             
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' SPFSF-N.LT.1', 13, 1, 2)              
C/                                                                      
      CALL ENTER(1)                                                     
      Q=ISTKGT(3*N,2)                                                   
      IM=Q+N                                                            
      DO  1 I = 1, N                                                    
         TEMP1 = MRP(I) .LT. 1                                          
         IF (.NOT. TEMP1) TEMP1 = MRP(I) .GT. N                         
C/6S                                                                    
C        IF (TEMP1) CALL SETERR(23H SPFSF-MRP OUT OF RANGE, 23, 2, 2)   
C/7S                                                                    
         IF (TEMP1) CALL SETERR(' SPFSF-MRP OUT OF RANGE', 23, 2, 2)    
C/                                                                      
         K = MCP(I)                                                     
         TEMP1 = K .LT. 1                                               
         IF (.NOT. TEMP1) TEMP1 = K .GT. N                              
C/6S                                                                    
C        IF (TEMP1) CALL SETERR(23H SPFSF-MCP OUT OF RANGE,23,3,2)      
C/7S                                                                    
         IF (TEMP1) CALL SETERR(' SPFSF-MCP OUT OF RANGE',23,3,2)       
C/                                                                      
         INDEX=IM+K                                                     
         ISP(INDEX)=I                                                   
   1     CONTINUE                                                       
C/6S                                                                    
C     IF (IWMAX.LT.3*N+1) CALL SETERR(                                  
C    122H SPFSF-IWMAX TOO SMALL,22,4,2)                                 
C/7S                                                                    
      IF (IWMAX.LT.3*N+1) CALL SETERR(                                  
     1' SPFSF-IWMAX TOO SMALL',22,4,2)                                  
C/                                                                      
      IM=IM+1                                                           
      JLU=2*N+2                                                         
       JCOL=Q+2*N+1                                                     
       IAMAX=IWMAX-2*N-2                                                
      CALL DS4FSF(N,MRP,ISP(IM),JROW,IWORK,IWORK(N+2),IWORK(JLU),IAMAX, 
     1 ISP(Q),MCP,IERR,ISP(JCOL))                                       
C/6S                                                                    
C     IF (IERR.GT.10+3*N) CALL SETERR(17H SPFSF-NULL PIVOT,             
C    1   17,IERR,2)                                                     
C/7S                                                                    
      IF (IERR.GT.10+3*N) CALL SETERR(' SPFSF-NULL PIVOT',              
     1   17,IERR,2)                                                     
C/                                                                      
      IF (IERR.LT.10+2*N) GO TO 3                                       
C/6S                                                                    
C       CALL SETERR(23H SPFSF-STORAGE EXCEEDED,23,IERR,1)               
C/7S                                                                    
        CALL SETERR(' SPFSF-STORAGE EXCEEDED',23,IERR,1)                
C/                                                                      
        CALL LEAVE                                                      
        RETURN                                                          
C/6S                                                                    
C3    IF(IERR.GT.10+N)CALL SETERR(                                      
C    1  29H SPFSF-DUPLICATE ENTRY IN ROW,29,IERR,2)                     
C      IF (IERR.GT.0) CALL SETERR(20H SPFSF-NULL ROW IN A,              
C    1   20,IERR,2)                                                     
C/7S                                                                    
 3    IF(IERR.GT.10+N)CALL SETERR(                                      
     1  ' SPFSF-DUPLICATE ENTRY IN ROW',29,IERR,2)                      
       IF (IERR.GT.0) CALL SETERR(' SPFSF-NULL ROW IN A',               
     1   20,IERR,2)                                                     
C/                                                                      
           IM=IM-1                                                      
           DO 20 I=1,N                                                  
              INDEX=IM+I                                                
              INDEX1=ISP(INDEX)                                         
              MCP(INDEX1)=I                                             
  20        CONTINUE                                                    
           IFILL=IWORK(N+1)-1                                           
          CALL LEAVE                                                    
   4  RETURN                                                            
      END                                                               
      SUBROUTINE DS4FSF(N,R,IC,GETROW,IL,IU,JU,JUMAX,Q,IM,IERR,         
     1JROW)                                                             
      INTEGER N, R(1), IC(1),  IL(1), JROW(N)                           
      EXTERNAL GETROW                                                   
      INTEGER IU(1), JU(1), JUMAX, Q(1), IM(1)                          
      INTEGER QM, VJ, JMIN, JMAX, I, J                                  
      INTEGER K, M, JUPTR                                               
      INTEGER TEMP                                                      
C       --------------------------------------------------------------- 
C    V.   PARAMETERS                                                    
C         FOLLOWING IS A LIST OF PARAMETERS TO THE PROGRAMS.  NAMES ARE 
C    UNIFORM AMONG THE VARIOUS SUBROUTINES.  CLASS ABBREVIATIONS ARE--  
C       V - SUPPLIES A VALUE TO A SUBROUTINE                            
C       R - CONTAINS A RESULT RETURNED BY A SUBROUTINE                  
C       I - IS USED INTERNALLY BY A SUBROUTINE                          
C       A - IS AN ARRAY                                                 
C       N - IS AN INTEGER VARIABLE                                      
C       F - IS A DOUBLE PRECISION VARIABLE.                             
C CLASS * PARAMETER                                                     
C ----- * ---------                                                     
C NVA   * IC    - INVERSE OF THE ORDERING OF THE COLUMNS OF  M.  FOR    
C       *           EXAMPLE, IF COLUMN 1 IS THE 5TH COLUMN AFTER        
C       *           REORDERING, THEN  IC(1)=5.                          
C       *           SIZE = N.                                           
C NVRA  * IL    - POINTERS TO THE FIRST ELEMENTS OF EACH ROW IN  L.     
C       *           SIZE = N+1.                                         
C NVRA  * IU    - POINTERS TO THE FIRST ELEMENTS OF EACH ROW IN  U.     
C       *           SIZE = N+1.                                         
C NVRA  * JU    - COLUMN NUMBER CORRESPONDING TO EACH ELEMENT OF  U.    
C                 AND L                                                 
C NV    * JUMAX - DECLARED DIMENSION OF  JU.                            
C NV    * N     - NUMBER OF ROWS/COLUMNS IN MATRIX  M.                  
C NVA   * R     - ORDERING OF THE ROWS OF  M.                           
C       *           SIZE = N.                                           
C       --------------------------------------------------------------- 
C*** SYMBOLIC LU-FACTORIZATION OF A NONSYMMETRIC SPARSE MATRIX          
C       INPUT VARIABLES-- N, R, IC,   JUMAX.                            
C       OUTPUT VARIABLES-- IL,  IU, JU, .                               
C       PARAMETERS USED INTERNALLY--                                    
C NIA   * Q     - SUPPOSE  MO  IS THE RESULT OF REORDERING  M.  IF      
C       *           PROCESSING OF THE ITH ROW OF  MO  (HENCE THE ITH    
C       *           ROWS OF  L AND U) IS BEING DONE,  Q(J)  IS          
C       *           INITIALLY NONZERO IF  MO(I,J) IS NONZERO.  SINCE    
C       *           VALUES NEED NOT BE STORED, EACH ENTRY POINTS TO THE 
C       *           NEXT NONZERO.  FOR EXAMPLE, IF N=9 AND THE 5TH ROW  
C       *           OF  MO  IS                                          
C       *              0 X X 0 X 0 0 X 0                                
C       *           THEN  Q  WILL INITIALLY BE                          
C       *              A 3 5 A 8 A A 10 A 2         (A - ARBITRARY).    
C       *           Q(N+1)  POINTS TO THE FIRST NONZERO IN THE ROW AND  
C       *           THE LAST NONZERO POINTS TO N+1.  AS THE ALGORITHM   
C       *           PROCEEDS, OTHER ELEMENTS OF  Q  ARE INSERTED IN THE 
C       *           LIST BECAUSE OF FILLIN.                             
C       *           SIZE = N+1.                                         
C NIA   * IM    - AT EACH STEP IN THE FACTORING,  IM(I)  IS THE LAST    
C       *           ELEMENT OF THE ITH ROW OF  U  WHICH NEEDS TO BE     
C       *           CONSIDERED IN COMPUTING FILLIN.                     
C       *           SIZE = N.                                           
C  INTERNAL VARIABLES--                                                 
C    JLPTR - POINTS TO THE LAST POSITION USED IN  JU FOR L.             
C    JUPTR - POINTS TO THE LAST POSITION USED IN  JU.                   
C    JMIN,JMAX - ARE THE INDICES IN  A OR U  OF THE FIRST AND LAST      
C                ELEMENTS TO BE EXAMINED IN A GIVEN ROW.                
C                FOR EXAMPLE,  JMIN=IA(K), JMAX=IA(K+1)-1.              
C  ******  INITIALIZE POINTERS  *************************************** 
      IL(1) = 1                                                         
      IERR=0                                                            
      JUPTR = 0                                                         
      IU(1) = 1                                                         
       NP1=N+1                                                          
C  ******  FOR EACH ROW OF L AND U  *********************************** 
      DO  13 K = 1, N                                                   
C  ******  SET Q TO THE REORDERED ROW OF A  *************************** 
         Q(NP1) = NP1                                                   
         M=NP1                                                          
         TEMP = R(K)                                                    
         CALL GETROW(TEMP,JROW,NUM)                                     
         IF (NUM.LT.1) GO TO 14                                         
         DO  3 J = 1,NUM                                                
            TEMP = JROW(J)                                              
            VJ = IC(TEMP)                                               
            IF (M.GT.VJ)M=NP1                                           
 1            QM=Q(M)                                                   
              IF(QM.GE.VJ) GO TO 2                                      
              M=QM                                                      
              GO TO 1                                                   
   2        IF (QM .EQ. VJ) GOTO  15                                    
            Q(M) = VJ                                                   
            Q(VJ) = QM                                                  
   3        CONTINUE                                                    
C  ******  FOR EACH ENTRY IN THE LOWER TRIANGLE  ********************** 
         I = N+1                                                        
   4        I = Q(I)                                                    
            IF (I .GE. K) GOTO  10                                      
C  ******  L(K,I) WILL BE NONZERO, SO ADD IT TO JL  ******************* 
            JUPTR = JUPTR+1                                             
            IF (JUPTR .GT. JUMAX) GOTO  17                              
            JU(JUPTR) = I                                               
            QM = I                                                      
C  ******  INSPECT ITH ROW FOR FILLIN, ADJUST IM IF POSSIBLE  ********* 
            JMIN = IU(I)                                                
            JMAX = IM(I)                                                
            IF (JMIN .GT. JMAX) GOTO 9                                  
               DO  8 J = JMIN, JMAX                                     
                  VJ = JU(J)                                            
                  IF (VJ .EQ. K) IM(I) = J                              
   5                 M = QM                                             
                     QM = Q(M)                                          
                     IF (QM .GE. VJ) GOTO  6                            
                     GOTO  5                                            
   6              IF (QM .EQ. VJ) GOTO 7                                
                     Q(M) = VJ                                          
                     Q(VJ) = QM                                         
                     QM = VJ                                            
   7              CONTINUE                                              
   8              CONTINUE                                              
   9        CONTINUE                                                    
C  ******  CHECK FOR NULL PIVOT  ************************************** 
            GOTO  4                                                     
  10     IF (I .NE. K) GOTO  16                                         
         JUPTR = JUPTR+1                                                
         IF (JUPTR .GT. JUMAX) GOTO  17                                 
         JU(JUPTR) = I                                                  
         IU(K) = JUPTR+1                                                
C  ******  REMAINING ELEMENTS OF Q DEFINE STRUCTURE OF U(K, )  ******** 
  11        I = Q(I)                                                    
            IF (I .GT. N) GOTO  12                                      
            JUPTR = JUPTR+1                                             
            IF (JUPTR .GT. JUMAX) GOTO  17                              
            JU(JUPTR) = I                                               
C  ******  GET READY FOR NEXT ROW  ************************************ 
            GOTO  11                                                    
  12     IM(K) = JUPTR                                                  
         IL(K+1) = JUPTR+1                                              
  13     CONTINUE                                                       
      RETURN                                                            
C  NULL ROW IN A                                                        
  14   IERR=R(K)+10                                                     
      RETURN                                                            
C  DUPLICATE ENTRY IN ROW OF A                                          
 15   IERR= 10+N+K                                                      
      RETURN                                                            
C  NULL PIVOT                                                           
  16  IERR=10+K+3*N                                                     
      RETURN                                                            
C  JL STORAGE EXCEEDED                                                  
  17  IERR=2*N+K+10                                                     
      RETURN                                                            
      END                                                               
C****END OF ROUTINES FOR PORT 3 SPARSE ROUTINES (IN LINEAR ALGEBRA)*****
