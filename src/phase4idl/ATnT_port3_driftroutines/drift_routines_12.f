      SUBROUTINE IQP(N, X, Q, IQ, C, M, A, IA, B, BL, BU,               
     1   IPRINT, MAXITR, IEQ)                                           
      INTEGER IA, IQ, N                                                 
      INTEGER M, IPRINT, MAXITR                                         
      REAL X(N), Q(IQ, N), C(N), A(IA, N), B(M), BL(N)                  
      REAL BU(N)                                                        
      REAL EPSI                                                         
      INTEGER ID, IE, IG, IT, JT                                        
      INTEGER IY, IRES, IAC, I, INJ, IZL                                
      INTEGER IJSIM, IIRES                                              
      INTEGER IEQ, IWUNIT                                               
      REAL SDOT, R, R1MACH                                              
      INTEGER IISIMP, IEXTRA, ILIM, IND, I1MACH                         
      INTEGER ICT, IRCT, INDI, INDD,IIND                                
C                                                                       
      DOUBLE PRECISION DSTAK(500)                                       
      COMMON /CSTAK/DSTAK                                               
      INTEGER ISTAK(1000)                                               
      REAL RSTAK(1000)                                                  
      EQUIVALENCE (DSTAK(1), ISTAK(1))                                  
      EQUIVALENCE (DSTAK(1), RSTAK(1))                                  
C                                                                       
C THIS SUBROUTINE SOLVES THE INDEFINITE QUADRATIC                       
C PROGRAMMING PROBLEM OF MINIMIZING                                     
C           T      T                                                    
C          X QX/2+C X                                                   
C SUCH THAT                                                             
C        AX.GE.B                                                        
C   AND                                                                 
C         BL(I).LE.X(I).LE.BU(I).I=1,...,N                              
C                                                                       
C PARAMETERS ON INPUT                                                   
C   N       ORDER OF PROBLEM                                            
C   X       INITIAL GUESS, NEED NOT SATISFY CONSTRAINTS                 
C   Q       N X N SYMMETRIC MATRIX,MAY BE INDEFINITE.                   
C           MUST BE FILLED IN COMPLETELY, STRICTLY                      
C           LOWER TRIANGLE DESTROYED                                    
C   IQ      LEADING DIMENSION OF Q. MUST BE AT LEAST N                  
C   C       VECTOR IN LINEAR TERM OF FUNCTION TO BE MINIMIZED           
C   IEQ     NUMBER OF LINEAR EQUALITY CONSTRAINTS                       
C   M       NUMBER OF LINEAR INEQUALITY CONSTRAINTS                     
C   A       M X N MATRIX OF LINEAR CONSTRAINTS                          
C   IA      LEADING DIMENSION OF A, MUST BE AT LEAST M                  
C   B       RIGHT HAND SIDE VECTOR OF CONSTRAINTS                       
C   BL      LOWER BOUND ON VARIABLES                                    
C   BU      UPPER BOUND ON VARIABLES                                    
C   IPRINT  IF IPRINT IS GREATER THAN 0,THEN THE FUNCTION,GRADIENT,     
C           APPROXIMATE SOLUTION WILL BE PRINTED EACH ITERATION.        
C           FEASABILITY CONDITIONS FROM FEAS WILL ALSO BE PRINTED.      
C  MAXITR   MAXIMUM NUMBER OF ITERATIONS PERMITTED                      
C                                                                       
C OUTPUT PARAMETERS                                                     
C   X       SOLUTION OF THE PROBLEM                                     
C                                                                       
C SCRATCH STORAGE (ALLOCATED FROM THE PORT STACK)                       
C RSTAK     REAL ARRAY OF LENGTH AT LEAST M+(4+N)N LOCATIONS            
C           (UNLESS (N.GT.M) WHEN (5+N)N LOCATIONS WILL BE ALLOCATED)   
C ISTAK     INTEGER ARRAY OF LENGTH AT LEAST 4N + M LOCATIONS           
C       ISTAK(1...N) = INEQUALITY, EQUALITY FOR UPPER AND LOWER BOUNDS  
C       ISTAK(N+1...N+M) = EQUALITY, INEQUALITY FOR MATRIX CONSTRAINTS  
C       ISTAK(N+M+1...3N+M) = ISIMP FOR FEASA                           
C       ISTAK(3N+M+1...4N+M) = STORAGE SPACE FOR READING OFF ISIMP      
C CHECK FOR ERRORS                                                      
C                                                                       
        CALL ENTER(1)                                                   
        IWUNIT = I1MACH(2)                                              
        EPSI = R1MACH(4)                                                
C/6S                                                                    
C     IF ((N .LT. 1).OR.(M .LT. 0))                                     
C    1  CALL SETERR(20HIQP-N.LT.1 OR M.LT.0,20,1,2)                     
C     IF (MAXITR .LT. 1) CALL SETERR(15HIQP-MAXITR.LT.1,15,2,2)         
C     IF (IQ .LT. N) CALL SETERR(11HIQP-IQ.LT.N,11,3,2)                 
C     IF (IA .LT. M) CALL SETERR(11HIQP-IA.LT.M,11,4,2)                 
C     IF ((IEQ .GT. N).OR.(IEQ .GT. M).OR.(IEQ .LT. 0))                 
C    1  CALL SETERR(36HIQP-IEQ.GT.N OR IEQ.GT.M OR IEQ.LT.0,36,5,2)     
C/7S                                                                    
      IF ((N .LT. 1).OR.(M .LT. 0))                                     
     1  CALL SETERR('IQP-N.LT.1 OR M.LT.0',20,1,2)                      
      IF (MAXITR .LT. 1) CALL SETERR('IQP-MAXITR.LT.1',15,2,2)          
      IF (IQ .LT. N) CALL SETERR('IQP-IQ.LT.N',11,3,2)                  
      IF (IA .LT. M) CALL SETERR('IQP-IA.LT.M',11,4,2)                  
      IF ((IEQ .GT. N).OR.(IEQ .GT. M).OR.(IEQ .LT. 0))                 
     1  CALL SETERR('IQP-IEQ.GT.N OR IEQ.GT.M OR IEQ.LT.0',36,5,2)      
C/                                                                      
C USE FEASA TO GET A FEASIBLE SOLUTION (OR CHECK FEASIBILITY OF CURRENT 
C SOLUTION.)                                                            
C                                                                       
C GET SPACE FOR FEASA AND TRANSLATION                                   
      IAC = ISTKGT(M,2)                                                 
      IISIMP = ISTKGT(2*N,2)                                            
      INDD = ISTKGT(2*N,3)                                              
      CALL Q6INT(A, M, N, IA, B, X, BL, BU, RSTAK(INDD),                
     1   ISTAK(IISIMP),                                                 
     2   ISTAK(IAC), IAS, IAG, MAXITR, IPRINT, IEQ, IWUNIT)             
C                                                                       
      CALL ISTKRL(1)                                                    
      IF ((NERROR(NERR).NE.6).AND.(NERROR(NERR).NE.8)                   
     1    .AND.(NERROR(NERR).NE.9))  GO TO 301                          
      CALL ERROFF                                                       
C/6S                                                                    
C     IF (NERROR(NERR).EQ.6) CALL SETERR (18HIQP-ITER.GT.MAXITR,18,8,1) 
C     IF (NERROR(NERR).EQ.8) CALL SETERR (16HIQP-NO FEAS. SOL,16,9,1)   
C     IF (NERROR(NERR).EQ.9) CALL SETERR (17HIQP-COND. PROBLEM,17,10,1) 
C/7S                                                                    
      IF (NERROR(NERR).EQ.6) CALL SETERR ('IQP-ITER.GT.MAXITR',18,8,1)  
      IF (NERROR(NERR).EQ.8) CALL SETERR ('IQP-NO FEAS. SOL',16,9,1)    
      IF (NERROR(NERR).EQ.9) CALL SETERR ('IQP-COND. PROBLEM',17,10,1)  
C/                                                                      
      CALL LEAVE                                                        
      RETURN                                                            
 301  CONTINUE                                                          
      IRCT = 2*M + (3+N) * N                                            
      IF (N.GT.M) IRCT = (4+N) * N + M                                  
      ICT = 2*N                                                         
      INDD = ISTKGT(IRCT, 3)                                            
      INDI = ISTKGT(ICT, 2)                                             
      IZL = INDD                                                        
      IY = IZL+N*N                                                      
      IE = IY+N                                                         
      ID = IE+N                                                         
      IG = ID+N                                                         
      MN=M                                                              
      IF(N.GT.M) MN=N                                                   
      IRES = IG+MN                                                      
      IJSIM = INDI                                                      
      INJ = IJSIM                                                       
      IEXTRA = IJSIM+N                                                  
      JT = IEXTRA-1                                                     
      ILIM = IEXTRA+N-1                                                 
      DO  1 I = IEXTRA, ILIM                                            
         ISTAK(I) = 0                                                   
 1    CONTINUE                                                          
C                                                                       
      IF (IAS.EQ.0) GO TO 19                                            
      DO  2 I = 1, IAS                                                  
         IIND = IISIMP + I - 1                                          
         IND = IEXTRA + IABS(ISTAK(IIND)) - 1                           
         ISTAK(IND) = 1                                                 
 2    CONTINUE                                                          
 19   CONTINUE                                                          
      DO  3 I = 1,N                                                     
         IND = IEXTRA + I - 1                                           
         IF(ISTAK(IND).NE.1) GO TO 4                                    
         ISTAK(JT) = I                                                  
         JT = JT - 1                                                    
         GO TO 3                                                        
 4       ISTAK(INJ) = I                                                 
         INJ = INJ + 1                                                  
 3    CONTINUE                                                          
      IT = IAG + IEQ                                                    
      JT = N - IAS                                                      
      ILIM = 2*N + INDD                                                 
      DO 8 I=INDD,ILIM                                                  
        RSTAK(I) = 0.                                                   
 8    CONTINUE                                                          
      IIRES = IRES - 1                                                  
      IF (M.LT.1) GO TO 9                                               
      DO 10 I=1,M                                                       
        R = SDOT(N,A(I,1),IA,X,1) - B(I)                                
        IIRES = IIRES + 1                                               
        IF (R.LT.0.) R = 0.                                             
        RSTAK(IIRES) = R                                                
 10   CONTINUE                                                          
 9    CONTINUE                                                          
C                                                                       
      CALL Q6IQP(X, N, A, IA, RSTAK(IRES), M, Q, IQ, C, BL, BU, IT      
     1   , JT, ISTAK(IJSIM), ISTAK(IAC), RSTAK(ID), RSTAK(IE)           
     2   , RSTAK(IY), RSTAK(IZL), RSTAK(IG), IPRINT, MAXITR             
     3   , IEQ, IERR, IWUNIT, EPSI, B)                                  
C/6S                                                                    
C     IF (IERR.EQ.1) CALL SETERR(18HIQP-ITER.GT.MAXITR,17,6,1)          
C     IF (IERR.EQ.2) CALL SETERR(22HIQP-UNBOUNDED SOLUTION,21,7,1)      
C/7S                                                                    
      IF (IERR.EQ.1) CALL SETERR('IQP-ITER.GT.MAXITR',17,6,1)           
      IF (IERR.EQ.2) CALL SETERR('IQP-UNBOUNDED SOLUTION',21,7,1)       
C/                                                                      
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
        SUBROUTINE Q6INT(A, M, N, IA, B, X, BL, BU, SIMP, ISIMP, IPTG,  
     1          IAS, IAG, MAXITR, IPRINT, IE, IWUNIT)                   
C                                                                       
C THIS SUBROUTINE PROVIDES AN INTERFACE BETWEEN QP (IN QPPAK.F)         
C AND FEASA (IN LPH11T.F)                                               
C                                                                       
C PARAMETERS ON INPUT                                                   
C   N       ORDER OF PROBLEM                                            
C   X       INITIAL GUESS, NEED NOT SATISFY CONSTRAINTS                 
C   IE      NUMBER OF LINEAR EQUALITY CONSTRAINTS                       
C   M       NUMBER OF LINEAR EQUALITY AND INEQUALITY CONSTRAINTS        
C   A       M X N MATRIX OF LINEAR CONSTRAINTS                          
C   IA      LEADING DIMENSION OF A, MUST BE AT LEAST M                  
C   B       RIGHT HAND SIDE VECTOR OF CONSTRAINTS                       
C   BL      LOWER BOUND ON VARIABLES                                    
C   BU      UPPER BOUND ON VARIABLES                                    
C   SIMP    VECTOR CONTAINING SIMPLE CONSTRAINTS                        
C   S       NUMBER OF SIMPLE CONSTRAINTS                                
C   ISIMP   VECTOR TELLING THE ELEMENT OF X THAT THE SIMPLE             
C           CONSTRAINT PERTAINS TO.  IF NEGATIVE IT IS AN UPPER         
C           BOUND OTHERWISE IT IS A LOWER BOUND.                        
C   IPRINT  IF IPRINT IS GREATER THAN 0,THEN THE FUNCTION,GRADIENT,     
C           APPROXIMATE SOLUTION WILL BE PRINTED EACH ITERATION.        
C           FEASABILITY CONDITIONS FROM FEAS WILL ALSO BE PRINTED.      
C   MAXITR  MAXIMUM NUMBER OF ITERATIONS PERMITTED                      
C                                                                       
C   PARAMETERS ON OUTPUT                                                
C                                                                       
C   X       FEASIBLE SOLUTION COMPUTED BY FEASA                         
C                                                                       
        DOUBLE PRECISION DSTAK(500)                                     
        COMMON/CSTAK/DSTAK                                              
        INTEGER M, N, IA, IFLAG                                         
        REAL A(IA,N), B(M), X(N), BL(N), BU(N)                          
        INTEGER ICT, ISIMP(1), MAXITR, IE, IPTG(M), SMAX, IAG, IAS      
        INTEGER IPRINT, IWUNIT, IND                                     
        REAL SIMP(1)                                                    
        EXTERNAL LPRNT, QP1NT                                           
        EXTERNAL LPMAN                                                  
C                                                                       
        IFLAG = 0                                                       
C TRANSLATE BL AND BU TO SIMP AND ISIMP                                 
        DO 1 I=1,N                                                      
            SIMP(I) = BL(I)                                             
            ISIMP(I) = I                                                
            IND = N + I                                                 
            SIMP(IND) = BU(I)                                           
            ISIMP(IND) = -I                                             
 1      CONTINUE                                                        
        ICT = 2*N                                                       
        SMAX = MAXITR                                                   
C                                                                       
 100    CONTINUE                                                        
        IF (IPRINT.LE.0) GOTO 2                                         
            CALL FEASA(A, M, N, LPMAN, IA, B, X, SMAX, ICT, SIMP,       
     1          ISIMP, IE, QP1NT, IAG, IAS, IPTG)                       
            GO TO 31                                                    
 2      CALL FEASA(A, M, N, LPMAN, IA, B, X, SMAX, ICT, SIMP, ISIMP,    
     1      IE, LPRNT, IAG, IAS, IPTG)                                  
C NO. OF ITERATIONS .GT. MAXITR                                         
 31        IF (NERROR(NERR).NE.6) GO TO 106                             
           IF (IFLAG.NE.0) GO TO 3                                      
           IFLAG = 1                                                    
           SMAX = 3*N                                                   
           CALL ERROFF                                                  
           GOTO 100                                                     
C NO FEASIBLE SOLUTION                                                  
 106    IF (NERROR(NERR).EQ.8) GO TO 3                                  
C CONDITIONING PROBLEM                                                  
        IF ((NERROR(NERR).NE.9).OR.(IFLAG.NE.0)) GO TO 3                
           IFLAG = 1                                                    
           CALL ERROFF                                                  
           GO TO 100                                                    
C                                                                       
 3      RETURN                                                          
        END                                                             
      SUBROUTINE Q6IQP(X, N, A, IA, RES, IP, Q, IQQ, C, BL, BU, IT      
     1   , JT, JSIM, IAC, D, E, Y, ZL, G, IPRINT, MAXITR, IE            
     2   , IERR, IWUNIT, EPSI, B)                                       
      INTEGER IA, N, IQQ                                                
      INTEGER IP, IT, JT, JSIM(N), IAC(IA), IPRINT                      
      INTEGER MAXITR                                                    
      REAL X(IQQ), A(IA, N), RES(IA), Q(IQQ, N), C(N), BL(N)            
      REAL BU(N), D(N), E(N), Y(N), ZL(N, N), G(N), B(IP)               
      INTEGER IB, IC, JDEC, JC, IQ, IS                                  
      INTEGER JDEX, IZ, IDEX, ITER, I, J                                
      INTEGER K, ITM1, ITP1, JTP1                                       
      LOGICAL POSDEF, CONVER                                            
      REAL LAMBDA, ACUM, SDOT, F, S, SUM, EPS                           
      REAL SNRM2, SS, EPSI                                              
      INTEGER TEMP1                                                     
      INTEGER IE                                                        
      LOGICAL TEMP, TEMP2                                               
      INTEGER IWUNIT, ITMP                                              
C                                                                       
C THIS SUBROUTINE SOLVES THE INDEFINITE QUADRATIC PROGRAMMING           
C PROBLEM OF MINIMIZING X(TRANSPOSE)QX/2+C(TRANSPOSE)X                  
C SUCH THAT AX-B=RES IS GREATER OR EQUAL TO ZERO.                       
C ON INPUT IT IS ASSUMED THAT THE FIRST IT CONSTRAINTS ARE              
C ACTIVE AT THE INITIAL GUESS.I.E. THE ROWS OF A HAVE BEEN              
C PERMUTED SO THAT RES(I)=0,I=1,...IT.                                  
C PARAMETERS ON INPUT*                                                  
C    X    AN N VECTOR OF INITIAL GUESSES                                
C    N    THE LENGTH OF THE X VECTOR                                    
C    A    AN IP X N MATRIX DEFINING THE LINEAR INEQUALITY               
C          CONSTRAINTS OF ROW DIMENSION IA                              
C    IA   ROW DIMENSION OF A MATRIX, MUST BE AT LEAST IP                
C   RES   THE RESIDUAL AT THE INITIAL GUESS                             
C         WHERE RES(I)=SUM OVER J OF (A(IAC(I),J)*X(J))-B(IAC(I))       
C    IP   THE TOTAL NUMBER OF GENERAL EQUALITY AND INEQUALITY           
C         CONSTRAINTS                                                   
C    Q    A SYMMETRIC INDEFINITE MATRIX,COMPLETELY FILLED IN            
C         OF DIMENSION N X N DEFINING THE HESSIAN OF THE                
C         FUNCTION TO BE MINIMIZED                                      
C    IQQ   ROW DIMENSION OF Q MATRIX AT LEAST N.                        
C    C    AN N VECTOR INVOLVED IN THE SECOND TERM IN THE FUNCTION       
C         TO BE MINIMIZED                                               
C    BL   REAL ARRAY, LENGTH AT LEAST N, WHOSE ITH COMPONENT GIVES      
C         LOWER BOUND ON X(I)                                           
C    BU   REAL ARRAY, LENGTH AT LEAST N, WHOSE ITH COMPONENT GIVES      
C         AN UPPER BOUND ON X(I)                                        
C    IT   THE NUMBER OF EQUALITY AND INEQUALITY CONSTRAINTS ACTIVE      
C         AT THE INITIAL GUESS                                          
C    JT   A POINTER INTO THE JSIM ARRAY, X(JSIM(JT+1)) THROUGH          
C         X(JSIM(N)) ARE CONSIDERED AT BOUNDS. USUALLY JSIM IS          
C         SET INITIALLY TO N AND EVERY VARIABLE MAY MOVE.               
C  JSIM   AN ARRAY OF LENGTH N CONTAINING THE NUMBERS 1 THROUGH N       
C         IN ANY ORDER. THE FIRST JT NUMBERS OF JSIM INDICATE           
C         THE ELEMENTS OF X WHICH ARE NOT AT THEIR BOUNDS.              
C  IAC    AN ARRAY OF LENGTH IP CONTAINING THE NUMBERS 1 THOUGH IP      
C         IN ANY ORDER EXCEPT THAT THE FIRST IT NUMBERS POINT TO        
C         CONSTRAINTS THAT ARE INITIALLY ACTIVE                         
C  IPRINT IF IPRINT .LE. 0, NOTHING IS PRINTED. OTHERWISE               
C         FUNCTION IS PRINTED EACH ITERATION                            
C MAXITR  MAXIMUM NUMBER OF PERMITTED ITERATIONS                        
C    IE   THE TOTAL NUMBER OF EQUALITY CONSTRAINTS                      
C         THE FIRST IE ROWS OF A SHOULD BE THE EQUALITY CONSTRAINTS     
C         AND THE FIRST IE ELEMENTS OF IAC SHOULD POINT TO THEM.        
C  IWUNIT OUTPUT UNIT NUMBER FOR PRINTING                               
C  EPSI   MACHINE PRECISION                                             
C                                                                       
C SCRATCH SPACE                                                         
C    D    SCRATCH VECTOR OF LENGTH N                                    
C    E    SCRATCH VECTOR OF LENGTH N                                    
C    Y    SCRATCH VECTOR OF LENGTH N                                    
C    ZL   SCRATCH ARRAY DIMENSIONED (N,N) WHICH WILL HAVE               
C         ORTHOGONAL DECOMPOSITION FOR ACTIVE CONSTRAINT MATRIX.        
C    G    A SCRATCH VECTOR OF LENGTH NIP .GT. MAX(N,IP)                 
C                                                                       
C PARAMETERS ON OUTPUT                                                  
C   X    THE SOLUTION                                                   
C   IT   THE NUMBER OF ACTIVE CONSTRAINTS AT THE SOLUTION               
C   Q    THE LOWER HALF OF THIS MATRIX HAS BEEN DESTROYED               
C  IERR  ERROR CONDITION,IF 0 -NO PROBLEM                               
C                        IF 4 - MAXIMUM NUMBER OF ITERATONS USED        
C                                                                       
C DETERINE E,D,L,Z                                                      
C                                                                       
      ITER = 0                                                          
      RELDX = 0.0E0                                                     
      IQ1=0                                                             
      IF (JT .EQ. 0) GOTO 1                                             
         CALL G6TZD(A, Q, N, IT, IP, POSDEF, IA, IQQ, JT, JSIM, IAC, D  
     1      , E, Y, ZL, IE, EPSI)                                       
         GOTO  2                                                        
   1     IT = 0                                                         
   2  IZ = N                                                            
      CONVER = .FALSE.                                                  
C                                                                       
C COMPUTE THE GRADIENT= QX+C                                            
C                                                                       
      ITP1 = IT+1                                                       
   3     ITER = ITER+1                                                  
         IF (ITER .LE. MAXITR) GOTO 4                                   
            IERR = 1                                                    
            RETURN                                                      
   4     DO  5 I = 1, N                                                 
            G(I)=C(I)                                                   
            IF(I.GT.1)G(I) = C(I)+SDOT(I-1, Q(1, I), 1, X(1), 1)        
            G(I) = G(I)+SDOT(N-I+1, Q(I, I), IQQ, X(I), 1)              
   5        CONTINUE                                                    
         IF (IPRINT .LE. 0) GOTO 12                                     
C           F = SDOT(N, G(1), 1, X(1), 1)                               
            F = .5 * (SDOT(N, G(1), 1, X(1), 1) +                       
     1          SDOT(N, C(1), 1, X(1), 1))                              
              RELDF=0.0                                                 
             IF (ITER.NE.1.AND.F.EQ.0.0)RELDF=F-OLDF                    
             IF(ITER.NE.1.AND.F.NE.0.0)RELDF=(F-OLDF)/F                 
              OLDF=F                                                    
  12     TEMP = IT .GE. JT                                              
         S=-1.0E0                                                       
         IS=0                                                           
         IS1=0                                                          
         IF (.NOT. TEMP) TEMP = CONVER                                  
         IF (.NOT. TEMP) GOTO 36                                        
            CONVER = .FALSE.                                            
C                                                                       
C HAVE FOUND MINIMUM IN THE SUBSPACE,DETERMINE IF IT IS                 
C ALSO THE MINIMUM OF THE PROBLEM BY FINDING THE LAGRANGE               
C MULTIPLIERS                                                           
 745         CONTINUE                                                   
            S = 1.E0                                                    
C                                                                       
            IF (IT .LE. 0) GOTO 21                                      
               IF (JT .NE. 0) GOTO 14                                   
                  DO  13 I = 1, IT                                      
                     Y(I) = 0.E0                                        
  13                 CONTINUE                                           
                  GOTO  20                                              
  14              DO  16 I = 1, IT                                      
                     ACUM = 0.E0                                        
                     DO  15 J = 1, JT                                   
                        JDEC = JSIM(J)                                  
                        ACUM = ACUM+ZL(J, I)*G(JDEC)                    
  15                    CONTINUE                                        
                     Y(I) = ACUM                                        
  16                 CONTINUE                                           
                  IS = IT                                               
                  Y(IT) = Y(IT)/D(IT)                                   
                  IF (IT.GT.IE)S = Y(IT)                                
                  IF (IT .EQ. 1) GOTO 19                                
                     ITM1 = IT-1                                        
                     DO  18 IB = 1, ITM1                                
                        I = IT-IB                                       
                        Y(I) = (Y(I)-SDOT(IB, Q(I+1, I), 1, Y(I+1), 1))/
     1                     D(I)                                         
                        IF (Y(I).GT.S.OR.I.LE.IE) GO TO 17              
                           IS = I                                       
                           S = Y(I)                                     
  17                    CONTINUE                                        
  18                    CONTINUE                                        
  19              CONTINUE                                              
  20        CONTINUE                                                    
  21        IF (JT .GE. N) GOTO 26                                      
               JTP1 = JT+1                                              
               DO  25 I = JTP1, N                                       
                  IDEX = JSIM(I)                                        
                  ACUM = G(IDEX)                                        
                  IF (IT .LT. 1) GOTO 23                                
                     DO  22 J = 1, IT                                   
                        JC = IAC(J)                                     
                        ACUM = ACUM-A(JC, IDEX)*Y(J)                    
  22                    CONTINUE                                        
  23              IF (X(IDEX) .GT. BL(IDEX)) ACUM = -ACUM               
                  IF (ACUM .GE. S) GOTO 24                              
                     IS = -I                                            
                     S = ACUM                                           
  24              CONTINUE                                              
  25              CONTINUE                                              
C                                                                       
C TEST FOR CONVERGENCE BY SEEING IF S IS NEGATIVE                       
C                                                                       
  26        CONTINUE                                                    
             IF (IPRINT.LE.0) GO TO 27                                  
             IS1=0                                                      
             IF(IS.GT.0)IS1=IAC(IS)                                     
             ISM=-IS                                                    
             IF(IS.LT.0)IS1=-JSIM(ISM)                                  
             IF (S.GT.0.0E0)IS1=0                                       
  27        IF (S .LE. 0.E0) GOTO 29                                    
               IF(JT-ITP1.GT.0)GO TO35                                  
             IF (IPRINT.GT.0)                                           
     1        CALL QP2NT(ITER,N,IP,X,A,IA,B,F,RELDF,RELDX,JT,JSIM,      
     1       IT,IAC,IS1,IQ1,POSDEF,BU)                                  
               RETURN                                                   
C                                                                       
C DROP CONSTRAINT IS, NO CONVERGENCE                                    
C                                                                       
 29         CONTINUE                                                    
            IF (IS .LE. 0) GOTO 32                                      
               CALL Q6DRP(Q, IQQ, IS, IT, IP, N, POSDEF, JT, IAC,       
     1            JSIM, D, E, Y, ZL, IWUNIT)                            
               GOTO  35                                                 
 32            CALL S6IMD(A, IA, N, Q, IQQ, IS, IT, JT, POSDEF, JSIM,   
     1            IAC, D, E, Y, ZL, IWUNIT)                             
  35        CONTINUE                                                    
C                                                                       
C                                                                       
C COMPUTE THE PROJECTED GRADIENT,UHY=Z(TRANSPOSE)G                      
C                                                                       
  36     ITP1 = IT+1                                                    
         DO  38 I = ITP1, JT                                            
            SUM = 0.E0                                                  
            DO  37 J = 1, JT                                            
               JDEX = JSIM(J)                                           
               SUM = SUM+ZL(J, I)*G(JDEX)                               
  37           CONTINUE                                                 
            Y(I) = SUM                                                  
  38        CONTINUE                                                    
              IF (IPRINT.GT.0)                                          
     1        CALL QP2NT(ITER,N,IP,X,A,IA,B,F,RELDF,RELDX,JT,JSIM,      
     1       IT,IAC,IS1,IQ1,POSDEF,BU)                                  
 389         IF(S.GT.0.E0) RETURN                                       
            EPS=EPSI*10.0                                               
             SS=0.0E0                                                   
             DO 367 I=ITP1,JT                                           
                SS=SS+ABS(Y(I))                                         
 367         CONTINUE                                                   
            IF (SS.LT.EPS*10.0.AND.S.LT.0.0E0)GO TO 745                 
             IF (SS.LT.EPS)RETURN                                       
C                                                                       
C COMPUTE DBAR AND G=-DBAR(INVERSE)Y                                    
C                                                                       
         CALL B6ARD(G, JT, ITP1, POSDEF, D, E, Y, EPSI)                 
C                                                                       
C COMPUTE THE SEARCH DIRECTION AND PUT IT IN Y                          
C                                                                       
         DO  39 I = 1, JT                                               
            Y(I) = SDOT(JT-IT, ZL(I, ITP1), IZ, G(ITP1), 1)             
  39        CONTINUE                                                    
C                                                                       
C FIND TH  DISTANCE TO THE NEAREST CONSTRAINT ALONG Y                   
C                                                                       
         IQ = 0                                                         
         EPS=EPSI*SNRM2(JT,Y,1)                                         
         LAMBDA = -1.E0                                                 
         IF (POSDEF) LAMBDA = 1.E0                                      
         IF (IT .EQ. IP) GOTO 44                                        
            DO  43 I = ITP1, IP                                         
               SUM = 0.E0                                               
               IC = IAC(I)                                              
               DO  40 K = 1, JT                                         
                  JDEX = JSIM(K)                                        
                  SUM = SUM+A(IC, JDEX)*Y(K)                            
  40              CONTINUE                                              
               G(I) = SUM                                               
               ACUM = G(I)                                              
               IF (-ACUM.LT.EPS) GO TO 43                               
                  ACUM = (-RES(IC))/ACUM                                
                  TEMP = LAMBDA .LT. 0.E0                               
                  IF (.NOT. TEMP) TEMP = ACUM .LT. LAMBDA               
                  IF (.NOT. TEMP) GOTO 41                               
                     IQ = I                                             
                     LAMBDA = ACUM                                      
  41              CONTINUE                                              
  42           IF (LAMBDA .EQ. 0.E0) GOTO  51                           
  43           CONTINUE                                                 
  44     DO  48 I = 1, JT                                               
            J = JSIM(I)                                                 
            IF (ABS(Y(I)).LT.EPS) GO TO 48                              
            IF (Y(I) .GE. 0.E0) GOTO 45                                 
             IH=-1                                                      
               ACUM = (-(X(J)-BL(J)))/Y(I)                              
               GOTO  46                                                 
  45           ACUM = ((-X(J))+BU(J))/Y(I)                              
                IH=1                                                    
  46        TEMP = LAMBDA .LT. 0.E0                                     
            IF (.NOT. TEMP) TEMP = ACUM .LT. LAMBDA                     
            IF (.NOT. TEMP) GOTO 47                                     
              IHIT=IH                                                   
               LAMBDA = ACUM                                            
               IQ = -I                                                  
  47        IF (LAMBDA .EQ. 0.E0) GOTO  51                              
  48        CONTINUE                                                    
C                                                                       
C UPDAT RESIDUALS                                                       
C                                                                       
         IF (IP.LT.ITP1) GO TO 501                                      
         DO  49 I = ITP1, IP                                            
            IC = IAC(I)                                                 
            RES(IC) = RES(IC)+LAMBDA*G(I)                               
  49        CONTINUE                                                    
          IF (IQ.LE.0) GO TO 501                                        
             IC=IAC(IQ)                                                 
             RES(IC)=0.0E0                                              
 501       CONTINUE                                                     
C                                                                       
C                                                                       
C FIND NEW MINIMUM                                                      
C                                                                       
          RELDX=0.0                                                     
         DO  50 I = 1, JT                                               
            JDEX = JSIM(I)                                              
            X(JDEX) = X(JDEX)+LAMBDA*Y(I)                               
            RELDX=RELDX+Y(I)*Y(I)                                       
  50        CONTINUE                                                    
            RELDX=SQRT(RELDX)*LAMBDA                                    
            XNORM=SNRM2(N,X,1)                                          
             IF (XNORM.NE.0.0)RELDX=RELDX/XNORM                         
  51     CONTINUE                                                       
C                                                                       
C HAS AA CONSTRAINT BEEN HIT(                                           
C                                                                       
         TEMP = POSDEF                                                  
         IF (TEMP) TEMP = IQ .EQ. 0                                     
         IF (.NOT. TEMP) GOTO 53                                        
            CONVER = .TRUE.                                             
            TEMP2 = IT .EQ. 0                                           
            IF (TEMP2) TEMP2 = JT .EQ. N                                
            IF (.NOT. TEMP2) GOTO 52                                    
               RETURN                                                   
  52        CONTINUE                                                    
            IQ1=IQ                                                      
            GOTO  3                                                     
  53          IF (IQ)56,61,55                                           
  55         CONTINUE                                                   
             IQ1=IAC(IQ)                                                
               CALL Q6ADD(A, IQ, N, IT, POSDEF, IA, Q, IQQ, JT, JSIM,   
     1            IAC, D, E, Y, ZL)                                     
               GOTO  3                                                  
  56           ITMP = -IQ                                               
               J=JSIM(ITMP)                                             
               X(J)=BU(J)                                               
               IF(IHIT.LT.0) X(J) =BL(J)                                
               IQ1=-J                                                   
  58           CALL A6SIM(Q, IQQ, IT, JT, IQ, POSDEF, JSIM, D, E, Y,    
     1            ZL, N)                                                
         GOTO  3                                                        
  61  IERR = 2                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE A6SIM(Q, IQQ, IT, JT, IQ, POSDEF, JSIM, D, E, Y,       
     1   ZL, N)                                                         
      INTEGER N, IQQ                                                    
      INTEGER IT, JT, IQ, JSIM(N)                                       
      LOGICAL POSDEF                                                    
      REAL Q(IQQ, N), D(N), E(N), Y(N), ZL(N, N)                        
      INTEGER IB, I, J, K, ITE, JTM1                                    
      INTEGER ITP1                                                      
      REAL DUM, V1, V2, U2                                              
C THIS SUBROUTINES FIXES UP THE D,Z,Q, AND L MATRICES                   
C WHEN A SIMPLE CONSTRAINT IS ACTIVATED.                                
C                                                                       
      IQ = -IQ                                                          
      ITP1 = IT+1                                                       
C ZERO OUT THE BOTTOM OF THE VECTOR                                     
      IF (ITP1 .GE. JT) GOTO 2                                          
         DO  1 I = ITP1, JT                                             
            Y(I) = ZL(IQ, I)                                            
   1        CONTINUE                                                    
         CALL U6DAT(IT+2, POSDEF, JT, IT+2, D, E, Y, ZL, N)             
C UPDATE R                                                              
   2  IF (IT .LE. 0) GOTO 4                                             
         DUM = ZL(IQ, ITP1)                                             
         DO  3 IB = 1, IT                                               
            I = ITP1-IB                                                 
            Y(I) = 0.E0                                                 
            CALL G6TH2(DUM, ZL(IQ, I), V1, V2, U2, DUM)                 
            CALL A6PH2(JT, ZL(1, ITP1), ZL(1, I), 1, V1, V2, U2)        
            CALL A6PH2(1, Y(I), D(I), 1, V1, V2, U2)                    
            IF (IB .GT. 1) CALL A6PH2(IB-1, Y(I+1), Q(I+1, I), 1, V1,   
     1         V2, U2)                                                  
   3        CONTINUE                                                    
         ZL(IQ, ITP1) = DUM                                             
   4  JTM1 = JT-1                                                       
C SHOVE THINGS OVER                                                     
      IF (ITP1 .GE. JT) GOTO 10                                         
   6        DO  8 K = ITP1, JTM1                                        
               DO  7 I = 1, JT                                          
                  ZL(I, K) = ZL(I, K+1)                                 
   7              CONTINUE                                              
               D(K) = D(K+1)                                            
               E(K) = E(K+1)                                            
   8           CONTINUE                                                 
   9     CONTINUE                                                       
  10  IF (IQ .EQ. JT) GOTO 12                                           
         DO  11 J = 1, JT                                               
            ZL(IQ, J) = ZL(JT, J)                                       
  11        CONTINUE                                                    
         ITE = JSIM(IQ)                                                 
         JSIM(IQ) = JSIM(JT)                                            
         JSIM(JT) = ITE                                                 
  12  JT = JT-1                                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE B6ARD(W, N, IBEGIN, POSDEF, D, E, Y, EPSI)             
      INTEGER N                                                         
      INTEGER IBEGIN                                                    
      LOGICAL POSDEF                                                    
      REAL W(N), D(N), E(N), Y(N)                                       
      INTEGER J, K                                                      
      REAL SIGN, EPS, EPSI                                              
      REAL ALFA, DJ, EJ, BETA, YJ, TEMP                                 
      REAL YTDY, GAMMA, DELTA, SGN, DENOM, SQRT                         
      REAL YTDIY, DJP1, YJP1                                            
C                                                                       
C THIS SUBROUTINE DETERMINES W=DBAR(INVERSE)*Y                          
C                                                                       
       EPS=EPSI                                                         
      J = IBEGIN                                                        
      K = 0                                                             
      GAMMA = 0.E0                                                      
      ALFA = 0.E0                                                       
   1     J = J+K                                                        
         IF (J .GT. N) GOTO  11                                         
         DJ = D(J)                                                      
         EJ = E(J)                                                      
         YJ = Y(J)                                                      
         IF (EJ .NE. 0.E0) GOTO 5                                       
            K = 1                                                       
C                                                                       
C THE JTH BLOCK IN D IS 1-BY-1                                          
C                                                                       
            W(J) = -YJ                                                  
            IF (DJ .GE. 0E0) GOTO 2                                     
               W(J) = YJ/DJ                                             
               ALFA = ALFA+W(J)*YJ                                      
               GOTO  4                                                  
   2           IF (DJ .EQ. 0E0) GOTO 3                                  
                  W(J) = (-YJ)/DJ                                       
                  GAMMA = GAMMA+(-YJ)*W(J)                              
   3        CONTINUE                                                    
   4        CONTINUE                                                    
            GOTO  10                                                    
   5        K = 2                                                       
C                                                                       
C THE JTH BLOCK IN D IS 2-BY-2                                          
C                                                                       
            DJP1 = D(J+1)                                               
            YJP1 = Y(J+1)                                               
            DENOM = DJ*DJP1-EJ**2                                       
            W(J) = (DJP1*YJ-EJ*YJP1)/DENOM                              
            W(J+1) = (DJ*YJP1-EJ*YJ)/DENOM                              
            YTDIY = W(J)*YJ+W(J+1)*YJP1                                 
            YTDY = DJ*YJ**2+2.*EJ*YJ*YJP1+DJP1*YJP1**2                  
            IF (YTDIY .GE. 0E0) GOTO 6                                  
               ALFA = ALFA+YTDIY                                        
               GOTO  9                                                  
   6           IF (YTDY .GE. 0E0) GOTO 7                                
                  W(J) = -YJ                                            
                  W(J+1) = -YJP1                                        
                  ALFA = ALFA+YTDY                                      
                  GOTO  8                                               
   7              W(J) = YJP1                                           
                  W(J+1) = -YJ                                          
                  ALFA = YTDIY*DENOM+ALFA                               
   8        CONTINUE                                                    
   9        CONTINUE                                                    
  10     CONTINUE                                                       
         GOTO  1                                                        
  11  IF (POSDEF) GOTO 22                                               
         IF (ALFA .EQ. 0.E0) GOTO 16                                    
            BETA = 1.                                                   
            IF (GAMMA.GT.EPS) BETA=SQRT(2.E0*EPS-ALFA)/                 
     1          SQRT(GAMMA)                                             
            J = IBEGIN                                                  
            K = 0                                                       
  12           J = J+K                                                  
               IF (J .GT. N) GOTO  15                                   
               IF (E(J) .NE. 0.E0) GOTO 13                              
                  K = 1                                                 
                  IF (D(J) .LE. 0.E0) W(J) = W(J)/BETA                  
                  GOTO  14                                              
  13              K = 2                                                 
                  W(J) = W(J)/BETA                                      
                  W(J+1) = W(J+1)/BETA                                  
  14           CONTINUE                                                 
               GOTO  12                                                 
  15        CONTINUE                                                    
            GOTO  21                                                    
  16        J = IBEGIN                                                  
C                                                                       
C ALPHA IS EQUAL TO 0 SO COMPUTE W DIRECTLY                             
C                                                                       
            K = 0                                                       
  17           J = J+K                                                  
               IF (J .GT. N) GOTO  20                                   
               IF (E(J) .NE. 0.E0) GOTO 18                              
                  K = 1                                                 
                  W(J) = 0.E0                                           
                  IF (D(J) .LE. 0.E0) W(J) = SIGN(1.E0, -Y(J))          
                  GOTO  19                                              
  18              K = 2                                                 
                  TEMP = (D(J)-D(J+1))/2.                               
                  DELTA = TEMP-SQRT(TEMP**2+E(J)**2)                    
                  SGN = SIGN(1.E0, E(J)*(-Y(J))+DELTA*(-Y(J+1)))        
                  W(J) = E(J)*SGN                                       
                  W(J+1) = DELTA*SGN                                    
  19           CONTINUE                                                 
               GOTO  17                                                 
  20        CONTINUE                                                    
  21  CONTINUE                                                          
  22  RETURN                                                            
      END                                                               
      SUBROUTINE G6TZD(A, Q, N, IT, IP, POSDEF, IA, IQQ, JT, JSIM,      
     1   IAC, D, E, Y, ZL, IE, EPSI)                                    
      INTEGER IA, N, IQQ                                                
      INTEGER IT, IP, JT, JSIM(N), IAC(IA)                              
      LOGICAL POSDEF                                                    
      REAL A(IA, N), Q(IQQ, N), D(N), E(N), Y(N), ZL(N, N)              
      INTEGER IB, IC, JC, KE, II, JDEX                                  
      INTEGER IZ, IFIX, IM1, IP1, NM1, I                                
      INTEGER J, K, ITE, ITT, I1, K1                                    
      INTEGER ITP1, NMITM1                                              
      INTEGER IE                                                        
      REAL TEMP, SDOT, DEL, SIGMA, TAU, EPS                             
      REAL SUM, SQRT, EPSI                                              
      EPS=EPSI                                                          
C                                                                       
C THIS SUBROUTINE DETERMINES L, THE LOWER TRIANGULAR FACTOR             
C OF THE LQ FACTORIZATION OF THE FIRST IT ROWS OF A AND Z,              
C A MATRIX WHICH SPANS THE NULL SPACE OF A SUCH THAT Z(TRANSPOSE)QZ     
C IS BLOCK DIAGONAL. THE MATRICES LA AND Z ON OUTPUT WILL BE IN THE     
C ZL ARRAY.  THE VECTOR D WILL CONTAIN THE DIAGONAL OF THE              
C BLOCK DIAGONAL MATRIX AND E THE OFFDIAGONAL ELEMENTS. THE             
C VECTOR Y IS JUST A SCRATCH VECTOR.                                    
C                                                                       
      IZ = N                                                            
C                                                                       
C                                                                       
C SAVE THE DIAGONAL OF Q SO IT WONT BE DESTROYED                        
C                                                                       
      DO  1 I = 1, JT                                                   
         D(I) = Q(I, I)                                                 
   1     CONTINUE                                                       
      POSDEF = .TRUE.                                                   
C                                                                       
      DO  5 I = 1, JT                                                   
         IC = JSIM(I)                                                   
         DO  4 J = 1, I                                                 
            JC = JSIM(J)                                                
            IF (IC .LT. JC) GOTO 2                                      
               Q(I, J) = Q(IC, JC)                                      
               GOTO  3                                                  
   2           Q(I, J) = Q(JC, IC)                                      
   3        CONTINUE                                                    
   4        CONTINUE                                                    
   5     CONTINUE                                                       
C                                                                       
C REDUCE ZL TO TRIANGULAR FORM                                          
C AND APPLY THE TRANSFORMATIONS ON THE RIGHT AND LEFT TO THE            
C Q MATRIX                                                              
C                                                                       
      I = 0                                                             
      IF (IT .EQ. 0) GOTO 18                                            
         ITT = IT                                                       
         DO  17 II = 1, IT                                              
            I = I+1                                                     
            IC = IAC(I)                                                 
            DO  6 J = 1, JT                                             
               JDEX = JSIM(J)                                           
               ZL(I, J) = A(IC, JDEX)                                   
   6           CONTINUE                                                 
C APPLY PREVIOUS TRANSFORMATIONS TO THE NEW ROW                         
            IF (I .LE. 1) GOTO 9                                        
               IM1 = I-1                                                
               DO  8 J = 1, IM1                                         
                  TAU = SDOT(JT-J+1, ZL(J, J), IZ, ZL(I, J), IZ)/Y(J)   
                  DO  7 K = J, JT                                       
                     ZL(I, K) = ZL(I, K)-TAU*ZL(J, K)                   
   7                 CONTINUE                                           
   8              CONTINUE                                              
   9        SIGMA = SDOT(JT-I+1, ZL(I, I), IZ, ZL(I, I), IZ)            
C TEST FOR LINEAR DEPENDENCE                                            
C                                                                       
            IF (SIGMA .GE. EPS) GOTO 10                                 
               ITE = IAC(II)                                            
               IF (II .GT. IE) GOTO 110                                 
                  IE = IE - 1                                           
                  IP = IP - 1                                           
                  ITE = IAC(IP)                                         
 110           IAC(II) = IAC(ITT)                                       
               IAC(ITT) = ITE                                           
               ITT = ITT-1                                              
               I = I-1                                                  
               GOTO  16                                                 
  10        SIGMA = SQRT(SIGMA)                                         
            IF (ZL(I, I) .LT. 0.E0) SIGMA = -SIGMA                      
            ZL(I, I) = ZL(I, I)+SIGMA                                   
            Y(I) = SIGMA*ZL(I, I)                                       
            IP1 = I+1                                                   
C                                                                       
C                                                                       
C                                                                       
C APPLY THE SAME TRANSFORMATION AS A CONGRUENT TRANSFORMATIONS          
C TO  THE LOWER TRIANGULAR PART OF THE Q MATRIX                         
C                                                                       
            DEL = 0.E0                                                  
            DO  11 J = 1, JT                                            
               TAU = 0.E0                                               
               IF (J .GE. I) TAU = SDOT(J-I+1, ZL(I, I), IZ, Q(J, I),   
     1            IQQ)                                                  
               IF (J .NE. JT) TAU = TAU+SDOT(JT-J, ZL(I, J+1), IZ, Q(J+1
     1            , J), 1)                                              
               E(J) = TAU/Y(I)                                          
               IF (J .GE. I) DEL = DEL+E(J)*ZL(I, J)                    
  11           CONTINUE                                                 
            DEL = DEL/(2.E0*Y(I))                                       
            IM1 = I-1                                                   
            DO  15 J = I, JT                                            
               E(J) = E(J)-DEL*ZL(I, J)                                 
               IF (I .EQ. 1) GOTO 13                                    
                  DO  12 K = 1, IM1                                     
                     Q(J, K) = Q(J, K)-ZL(I, J)*E(K)                    
  12                 CONTINUE                                           
  13           DO  14 K = I, J                                          
                  Q(J, K) = Q(J, K)-ZL(I, J)*E(K)-ZL(I, K)*E(J)         
  14              CONTINUE                                              
  15           CONTINUE                                                 
            IF (I .EQ. N) GOTO  19                                      
  16        CONTINUE                                                    
  17        CONTINUE                                                    
  18  CONTINUE                                                          
  19  IT = I                                                            
C                                                                       
C NOW REDUCE RELEVANT PART OF Q TO BLOCK DIAGONAL FORM                  
C                                                                       
      ITP1 = IT+1                                                       
      IF (IT .EQ. JT) GOTO 30                                           
         CALL P6MDC(Q, IQQ, JT, E, ITP1)                                
C                                                                       
C APPLY THE TRANSFORMATIONS USED TO REDUCE Q TO THE LAST JT-IT COLUMNS  
C OF THE IDENTITY MATRIX                                                
C                                                                       
         ZL(JT, JT) = 1.E0                                              
         IF (IT .GE. JT-1) GOTO 29                                      
            NM1 = JT-1                                                  
            Y(JT) = JT                                                  
            DO  20 J = ITP1, NM1                                        
               ZL(JT, J) = 0.E0                                         
  20           CONTINUE                                                 
            NMITM1 = JT-IT-1                                            
            DO  28 IB = 1, NMITM1                                       
               I = JT-IB                                                
               ZL(I, I) = 1.E0                                          
               K1 = I+1                                                 
               IF (E(I+1) .LT. 0.E0) K1 = I+2                           
               IP1 = I+1                                                
               Y(I) = I                                                 
               DO  23 J = IP1, JT                                       
                  SUM = 0.E0                                            
                  KE = Y(J)                                             
                  IF (KE .LT. K1) GOTO 22                               
                     DO  21 K = K1, KE                                  
                        SUM = SUM+ZL(K, J)*Q(K, I)                      
  21                    CONTINUE                                        
  22              ZL(I, J) = SUM                                        
  23              CONTINUE                                              
               IM1 = I-1                                                
               IF (I .EQ. ITP1) GOTO 25                                 
                  DO  24 J = ITP1, IM1                                  
                     ZL(I, J) = 0.E0                                    
  24                 CONTINUE                                           
  25           IF (E(I) .LT. 0.E0) GOTO 27                              
                  I1 = I                                                
C                                                                       
C APPLY THE PERMUTATIONS WHICH GO WITH THE ITH TRANSFORMATION           
C                                                                       
                  IF (E(I+1) .LT. 0.E0) I1 = I+1                        
                  K = E(I)                                              
                  DO  26 J = I1, JT                                     
                     IF (IFIX((Y(J))) .LT. K) Y(J) = K                  
                     TEMP = ZL(I1, J)                                   
                     ZL(I1, J) = ZL(K, J)                               
                     ZL(K, J) = TEMP                                    
  26                 CONTINUE                                           
  27           CONTINUE                                                 
  28           CONTINUE                                                 
  29     CONTINUE                                                       
C                                                                       
C RESTORE THE DIAGONAL OF Q AND PICK OUT D AND E FROM Q                 
C                                                                       
  30  IF (IT .EQ. 0) GOTO 32                                            
         DO  31 I = 1, IT                                               
            Q(I, I) = D(I)                                              
  31        CONTINUE                                                    
  32  IF (IT .EQ. JT) GOTO 36                                           
         DO  35 I = ITP1, JT                                            
            TEMP = D(I)                                                 
            D(I) = Q(I, I)                                              
            Q(I, I) = TEMP                                              
            IF (D(I) .LE. 0.E0) POSDEF = .FALSE.                        
            E(I) = 0.E0                                                 
            IF (I .EQ. JT) GOTO 34                                      
               IF (E(I+1) .GT. 0.E0) GOTO 33                            
                  E(I) = Q(I+1, I)                                      
                  POSDEF = .FALSE.                                      
  33           CONTINUE                                                 
  34        CONTINUE                                                    
  35        CONTINUE                                                    
C                                                                       
C APPLY THE TRANSFORMATION FROM THE REDUCTION OF A TO THE LAST          
C N-IT COLUMNS OF THE ZL MATRIX TO FORM THE FINAL Z MATRIX              
C                                                                       
  36  IF (IT .EQ. 0) GOTO 45                                            
         DO  44 IB = 1, IT                                              
            I = IT+1-IB                                                 
            IP1 = I+1                                                   
            D(I) = (-Y(I))/ZL(I, I)                                     
            ZL(I, I) = 1E0-Y(I)/(D(I)*D(I))                             
            IF (I .EQ. 1) GOTO 38                                       
               IM1 = I-1                                                
               DO  37 J = 1, IM1                                        
                  Q(I, J) = ZL(I, J)                                    
  37              CONTINUE                                              
C                                                                       
C FORM THE ITH COLUMN                                                   
C                                                                       
  38        TAU = 1.E0/D(I)                                             
            DO  39 K = IP1, JT                                          
               ZL(K, I) = TAU*ZL(I, K)                                  
  39           CONTINUE                                                 
            IF (I .EQ. JT) GOTO 43                                      
               DO  41 J = IP1, JT                                       
                  TAU = SDOT(JT-I, ZL(I, IP1), IZ, ZL(IP1, J), 1)/Y(I)  
                  DO  40 K = IP1, JT                                    
                     ZL(K, J) = ZL(K, J)-TAU*ZL(I, K)                   
  40                 CONTINUE                                           
                  Y(J) = TAU                                            
  41              CONTINUE                                              
C                                                                       
C FORM THE ITH ROW                                                      
C                                                                       
               SIGMA = Y(I)/D(I)                                        
               DO  42 J = IP1, JT                                       
                  ZL(I, J) = SIGMA*Y(J)                                 
  42              CONTINUE                                              
  43        CONTINUE                                                    
  44        CONTINUE                                                    
  45  RETURN                                                            
      END                                                               
      SUBROUTINE Q6ADD(A, IQ, N, IT, POSDEF, IA, Q, IQQ, JT, JSIM,      
     1   IAC, D, E, Y, ZL)                                              
      INTEGER IA, N, IQQ                                                
      INTEGER IQ, IT, JT, JSIM(N), IAC(IA)                              
      LOGICAL POSDEF                                                    
      REAL A(IA, N), Q(IQQ, N), D(N), E(N), Y(N), ZL(N, N)              
      INTEGER JDEX, I, J, IAQ, ITP1                                     
      REAL ACUM                                                         
C                                                                       
C                                                                       
C THIS SUBROUTINE UPDATES L AND Z CONTAINED IN THE ZL MATRIX            
C WHEN ROW IQ OF THE A MATRIX BECOMES THE IT+1ST ROW OF                 
C THAT MATRIX SO THAT NOW THERE ARE IT+1 ACTIVE CONSTRAINTS.            
C NOTE THAT L THE LOWER TRIANGULAR FACTOR OF THE ACTIVE                 
C CONSTRAINT MATRIX WILL GAIN A ROW AND Z,WHICH SPANS THE               
C NULL SPACE OF THE ACTIVE CONSTRAINTS WILL LOSE A COLUMN.              
C                                                                       
      IAQ = IAC(IQ)                                                     
C                                                                       
C APPLY Q TO NEW ROW TO BE ADDED                                        
C                                                                       
      ITP1 = IT+1                                                       
      DO  4 I = 1, JT                                                   
         ACUM = 0.E0                                                    
         DO  1 J = 1, JT                                                
            JDEX = JSIM(J)                                              
            ACUM = ACUM+ZL(J, I)*A(IAQ, JDEX)                           
   1        CONTINUE                                                    
         IF (I .LE. IT) GOTO 2                                          
            Y(I) = ACUM                                                 
            GOTO  3                                                     
   2        Q(ITP1, I) = ACUM                                           
   3     CONTINUE                                                       
   4     CONTINUE                                                       
      IF (IT .LT. JT-1) CALL U6DAT(IT+2, POSDEF, JT, IT+2, D, E, Y, ZL, 
     1   N)                                                             
      D(ITP1) = Y(ITP1)                                                 
      IF (IQ .EQ. ITP1) GOTO 5                                          
         IAC(IQ) = IAC(ITP1)                                            
         IAC(ITP1) = IAQ                                                
   5  IT = ITP1                                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE Q6DRP(Q, IQQ, IS, IT, IP, N, POSDEF, JT, IAC,          
     1   JSIM, D, E, Y, ZL, IWUNIT)                                     
      INTEGER N, IQQ                                                    
      INTEGER IS, IT, IP, JT, IAC(N), JSIM(N)                           
      LOGICAL POSDEF                                                    
      REAL Q(IQQ, N), D(N), E(N), Y(N), ZL(N, N)                        
      INTEGER IP1, IP2, I, J, ITE, ISM1                                 
      INTEGER ITM1, ISP1                                                
      INTEGER IWUNIT                                                    
      REAL T, V1, V2, U2                                                
C                                                                       
C THIS SUBROUTINE CHANGES THE D,E, AND ZL MATRIX WHEN CONSTRAINT        
C IS IS ROPPED FROM THE SET OF ACTIVE CONSTRAINTS TO MAKE               
C IT-1 ACTIVE CONSTRAINTS                                               
C                                                                       
      ISP1 = IS+1                                                       
      ITM1 = IT-1                                                       
      ITE = IAC(IS)                                                     
C                                                                       
C UPDATE L BY SHOVING UP ROWS                                           
C                                                                       
      IF (IS .EQ. IT) GOTO 7                                            
         ISM1 = IS-1                                                    
         IF (IS .EQ. 1) GOTO 3                                          
            DO  2 J = 1, ISM1                                           
               DO  1 I = ISP1, IT                                       
                  Q(I-1, J) = Q(I, J)                                   
   1              CONTINUE                                              
   2           CONTINUE                                                 
   3     DO  6 I = IS, ITM1                                             
            IP1 = I+1                                                   
            IAC(I) = IAC(IP1)                                           
            CALL G6TH2(Q(IP1, I), D(IP1), V1, V2, U2, D(I))             
            CALL A6PH2(JT, ZL(1, I), ZL(1, IP1), 1, V1, V2, U2)         
            IF (I .EQ. ITM1) GOTO 5                                     
               IP2 = I+2                                                
               DO  4 J = IP2, IT                                        
                  T = Q(J, I)+U2*Q(J, IP1)                              
                  Q(J-1, I) = Q(J, I)+T*V1                              
                  Q(J, IP1) = Q(J, IP1)+T*V2                            
   4              CONTINUE                                              
   5        CONTINUE                                                    
   6        CONTINUE                                                    
   7  CALL M6CON(Q, IQQ, IT, JT, POSDEF, JSIM, D, E, Y, ZL, N, IWUNIT)  
      IAC(IT) = ITE                                                     
      IT = IT-1                                                         
      RETURN                                                            
      END                                                               
        SUBROUTINE QP2NT(ITER,N,M,X,A,IA,B,F,RELDF,RELDX,JT,JSIM,IT,    
     1  IAC,IDROP,IADD,POSDEF,BU)                                       
        INTEGER ITER,N,IA,JT,IT,IDROP,IADD                              
        INTEGER PU,M                                                    
        INTEGER JSIM(N),IAC(M)                                          
        REAL X(N),A(IA,N),B(M),F,RELDF,RELDX,BU(N)                      
        LOGICAL POSDEF                                                  
        REAL RNEG,R,SNRM2                                               
        PU=I1MACH(2)                                                    
        IF (.NOT.POSDEF)WRITE(PU,99)                                    
 99      FORMAT(29H INDEFINITE PROJECTED HESSIAN)                       
        IF (ITER.GT.1) GO TO 9                                          
         WRITE(PU,599)                                                  
 599     FORMAT(22H FEASIBLE POINT FOUND )                              
         WRITE(PU,699)(X(I),I=1,N)                                      
 699     FORMAT(1H ,5E15.5)                                             
      IF (IT .LE. 0) GOTO 4                                             
         WRITE (PU,  1)                                                 
   1     FORMAT (21HEQUALITY CONSTRAINTS )                              
         DO  3 I = 1, IT                                                
            WRITE (PU,  2) IAC(I)                                       
   2        FORMAT (I10)                                                
   3        CONTINUE                                                    
   4  JTP1 = JT+1                                                       
      IF (JT .GE. N) GOTO 8                                             
         WRITE (PU,  5)                                                 
   5     FORMAT (25H COORDINATES AT BOUNDARY )                          
         DO  7 I = JTP1, N                                              
            WRITE (PU,  6) JSIM(I)                                      
   6        FORMAT (I10)                                                
   7        CONTINUE                                                    
   8     CONTINUE                                                       
       WRITE(PU,199)                                                    
 199    FORMAT(49H ITERATION  F   RELDF     RELDX  DROPPED    ADDED)    
       WRITE(PU,299)                                                    
 299   FORMAT(52H                               CONSTRAINT CONSTRAINT)  
 9     CONTINUE                                                         
        IDR=IABS(IDROP)                                                 
        IAD=IABS(IADD)                                                  
        IF (IDROP)81,499,41                                             
 499    IF (IAD.EQ.0)WRITE(PU,10)ITER,F,RELDF,RELDX                     
 10       FORMAT(I3,1X,3E10.3)                                          
        IF (IADD.GT.0)                                                  
     1   WRITE(PU,20)ITER,F,RELDF,RELDX,IAD                             
 20      FORMAT(I3,1X,3E10.3,6X,I3,1HG)                                 
        IF (IADD.LT.0.AND.X(IAD).EQ.BU(IAD))                            
     1     WRITE(PU,30)ITER,F,RELDF,RELDX,IAD                           
 30      FORMAT(I3,1X,3E10.3,6X,I3,1HU)                                 
        IF (IADD.LT.0.AND.X(IAD).NE.BU(IAD))                            
     1     WRITE(PU,40)ITER,F,RELDF,RELDX,IAD                           
 40      FORMAT(I3,1X,3E10.3,6X,I3,1HL)                                 
         GO TO 161                                                      
 41      CONTINUE                                                       
        IF (IAD.EQ.0)                                                   
     1WRITE(PU,50)ITER,F,RELDF,RELDX,IDR                                
 50      FORMAT(I3,1X,3E10.3,1X,I3,1HG)                                 
        IF (IADD.GT.0)                                                  
     1     WRITE(PU,60)ITER,F,RELDF,RELDX,IDR,IAD                       
 60      FORMAT(I3,1X,3E10.3,1X,I3,1HG,1X,I3,1HG)                       
        IF (IADD.LT.0.AND.X(IAD).EQ.BU(IAD))                            
     1     WRITE(PU,70)ITER,F,RELDF,RELDX,IDR,IAD                       
 70      FORMAT(I3,1X,3E10.3,1X,I3,1HG,1X,I3,1HU)                       
        IF (IADD.LT.0.AND.X(IAD).NE.BU(IAD))                            
     1     WRITE(PU,80)ITER,F,RELDF,RELDX,IDR,IAD                       
 80      FORMAT(I3,1X,3E10.3,1X,I3,1HG,1X,I3,1HL)                       
         GO TO 161                                                      
 81      CONTINUE                                                       
         IF (X(IDR).NE.BU(IDR))GO TO 121                                
         IF (IAD.EQ.0)                                                  
     1     WRITE(PU,90)ITER,F,RELDF,RELDX,IDR                           
 90      FORMAT(I3,1X,3E10.3,1X,I3,1HU)                                 
         IF (IADD.GT.0)                                                 
     1     WRITE(PU,100)ITER,F,RELDF,RELDX,IDR,IAD                      
 100       FORMAT(I3,1X,3E10.3,1X,I3,1HU,1X,I3,1HG)                     
         IF (IADD.LT.0.AND.X(IAD).EQ.BU(IAD))                           
     1     WRITE(PU,110)ITER,F,RELDF,RELDX,IDR,IAD                      
 110       FORMAT(I3,1X,3E10.3,1X,I3,1HU,1X,I3,1HU)                     
         IF (IADD.LT.0.AND.X(IAD).LT.BU(IAD))                           
     1     WRITE(PU,120)ITER,F,RELDF,RELDX,IDR,IAD                      
 120       FORMAT(I3,1X,3E10.3,1X,I3,1HU,1X,I3,1HL)                     
         GO TO 161                                                      
 121     CONTINUE                                                       
         IF (IAD.EQ.0)                                                  
     1     WRITE(PU,130)ITER,F,RELDF,RELDX,IDR                          
 130       FORMAT(I3,1X,3E10.3,1X,I3,1HL)                               
         IF (IADD.GT.0)                                                 
     1     WRITE(PU,140)ITER,F,RELDF,RELDX,IDR,IAD                      
 140       FORMAT(I3,1X,3E10.3,1X,I3,1HL,1X,I3,1HG)                     
         IF (IADD.LT.0.AND.X(IAD).EQ.BU(IAD))                           
     1     WRITE(PU,150)ITER,F,RELDF,RELDX,IDR,IAD                      
 150       FORMAT(I3,1X,3E10.3,1X,I3,1HL,1X,I3,1HU)                     
         IF (IADD.LT.0.AND.X(IAD).NE.BU(IAD))                           
     1     WRITE(PU,160)ITER,F,RELDF,RELDX,IDR,IAD                      
 160       FORMAT(I3,1X,3E10.3,1X,I3,1HL,1X,I3,1HL)                     
 161      CONTINUE                                                      
       IF (M.EQ.0)RETURN                                                
       RNEG=0.0E0                                                       
       DO 200 I=1,M                                                     
        R=-DBLE(B(I))                                                   
        DO 180 J=1,N                                                    
         R=R+DBLE(A(I,J))*DBLE(X(J))                                    
 180    CONTINUE                                                        
        IF (R.GE.0.0E0) GO TO 200                                       
        R=R/SNRM2(N,A(I,1),IA)                                          
        IF (R.GT.RNEG) GO TO 200                                        
         RNEG=R                                                         
         IR=I                                                           
 200   CONTINUE                                                         
       IF (RNEG.GT.(-1000.0)*R1MACH(4)) RETURN                          
       WRITE(PU,210)IR,RNEG                                             
 210  FORMAT(19H WARNING CONSTRAINT,I3,29H IS MOST VIOLATED CONSTRAINT ,
     122HWITH RELATIVE RESIDUAL,E10.3)                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE S6IMD(A, IA, N, Q, IQQ, IS, IT, JT, POSDEF,            
     1   JSIM, IAC, D, E, Y, ZL, IWUNIT)                                
      INTEGER IA, N, IQQ                                                
      INTEGER IS, IT, JT, JSIM(N), IAC(N)                               
      LOGICAL POSDEF                                                    
      REAL A(IA, N), Q(IQQ, N), D(N), E(N), Y(N), ZL(N, N)              
      INTEGER IDEX, JDEX, I, JTM1, ITP1                                 
      INTEGER IWUNIT                                                    
      REAL V1, V2, U2                                                   
C                                                                       
C THIS SUBROUTINE FIXES UP THE D,Z,Q,AND L MATRICES                     
C WHEN A SIMPLE CONSTRAINT IS DELETED FROM THE SET OF                   
C ACTIVE CONSTRAINTS                                                    
C                                                                       
      IS = -IS                                                          
      ITP1 = IT+1                                                       
      IDEX = JSIM(IS)                                                   
      JT = JT+1                                                         
      ZL(JT, JT) = 0.E0                                                 
C SINCE THE NEW COLUMN WILL BE THE ITTH POSITION SOME SWAPPING          
C MUST BE DONE                                                          
      D(JT) = D(ITP1)                                                   
      IF (JT .LE. 1) GOTO 2                                             
         JTM1 = JT-1                                                    
         DO  1 I = 1, JTM1                                              
            ZL(I, JT) = ZL(I, ITP1)                                     
            ZL(I, ITP1) = 0.E0                                          
            ZL(JT, I) = 0.E0                                            
   1        CONTINUE                                                    
   2  ZL(JT, ITP1) = 1.E0                                               
      IF (IT .LE. 0) GOTO 5                                             
         DO  3 I = 1, IT                                                
            JDEX = IAC(I)                                               
            Y(I) = A(JDEX, IDEX)                                        
   3        CONTINUE                                                    
         DO  4 I = 1, IT                                                
            CALL G6TH2(D(I), Y(I), V1, V2, U2, D(I))                    
            CALL A6PH2(JT, ZL(1, I), ZL(1, ITP1), 1, V1, V2, U2)        
            IF (I .NE. IT) CALL A6PH2(IT-I, Q(I+1, I), Y(I+1), 1, V1,   
     1         V2, U2)                                                  
   4        CONTINUE                                                    
   5  JSIM(IS) = JSIM(JT)                                               
      JSIM(JT) = IDEX                                                   
      CALL M6CON(Q, IQQ, ITP1, JT, POSDEF, JSIM, D, E, Y, ZL            
     1   , N, IWUNIT)                                                   
      RETURN                                                            
      END                                                               
       SUBROUTINE QP1NT(A,M,N,AMAN,IA,B,C,X,CTX,IS,SIMP,ISIMP,IE,       
     1 ITER,IPTG,IAG,IAS,U,IEND,IPH)                                    
C                                                                       
C THIS IS A PRINT ROUTINE                                               
C                                                                       
       INTEGER IA, N                                                    
       INTEGER IPTG(N),ISIMP(1)                                         
       REAL CTX,A(IA,1),X(N),B(1)                                       
       LOGICAL IEND                                                     
       EXTERNAL AMAN                                                    
       REAL SIMP(1),C(1),U(1)                                           
       REAL TOUTD                                                       
       IEND = .FALSE.                                                   
       IWRITE=I1MACH(2)                                                 
       IAGPE=IAG+IE                                                     
       EPSM=-R1MACH(4)*100.0                                            
       IF (ITER.EQ.1)WRITE(IWRITE,20)                                   
 20    FORMAT(1H ,30H TRYING TO FIND FEASIBLE POINT)                    
       WRITE(IWRITE,1)ITER,IAGPE,IAS                                    
 1     FORMAT(/14H AT ITERATION ,I5                                     
     1  /18H NO.OF ACT. GEN.= ,I5,15H NO.OF ACT.SIM=,I5)                
C      WRITE(IWRITE,2)(X(I),I=1,N)                                      
 2     FORMAT(3H X ,5E15.5)                                             
       DO 10 I=1,M                                                      
          TOUTD=-B(I)                                                   
          DO 8 J=1,N                                                    
             TOUTD=TOUTD+A(I,J)*X(J)                                    
 8       CONTINUE                                                       
          IF (TOUTD.LT.EPSM)WRITE(IWRITE,9)I,TOUTD                      
 9        FORMAT(15H AT CONSTRAINT ,I5,11H RESIDUAL= ,E15.5)            
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
      SUBROUTINE A6PH2(N, A, B, IDIM, V1, V2, U2)                       
      INTEGER IDIM                                                      
      INTEGER N                                                         
      REAL A(IDIM, 1), B(IDIM, 1), V1, V2, U2                           
      INTEGER I                                                         
      REAL T                                                            
C                                                                       
C                                                                       
C THIS SUBROUTINE APPLIES A 2 X 2 HOUSEHOLDER TO THE                    
C TWO COLUMNS A AND B                                                   
C                                                                       
      IF (N .LT. 1) RETURN                                              
      DO  1 I = 1, N                                                    
         T = A(1, I)+U2*B(1, I)                                         
         A(1, I) = A(1, I)+T*V1                                         
         B(1, I) = B(1, I)+T*V2                                         
   1     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE G6TH2(A, B, V1, V2, U2, C)                             
      REAL A, B, V1, V2, U2, C                                          
      REAL ABS, RN, SS, SQRT, U1                                        
C                                                                       
C THIS SUBROUTINE GENERATES A HOUSEHOLDER TRANSFORMATIONS               
C WHICH ZEROES THE ELEMENT B                                            
C                                                                       
      SS = ABS(A)+ABS(B)                                                
      U1 = A/SS                                                         
      U2 = B/SS                                                         
      RN = SQRT(U1*U1+U2*U2)                                            
      IF (U1 .LT. 0.E0) RN = -RN                                        
      V1 = (-(U1+RN))/RN                                                
      V2 = (-U2)/RN                                                     
      U2 = V2/V1                                                        
      C = (-RN)*SS                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE M6CON(Q, IQQ, IT, JT, POSDEF, JSIM, D, E, Y, ZL,       
     1   N, IWUNIT)                                                     
      INTEGER JT, N, IQQ                                                
      INTEGER IT, JSIM(N)                                               
      LOGICAL POSDEF                                                    
      REAL Q(IQQ, JT), D(N), E(N), Y(N), ZL(N, N)                       
      INTEGER IDEX, JDEX, I, J                                          
      REAL SDOT, A, SUM                                                 
      INTEGER IWUNIT                                                    
C                                                                       
C THIS SUBROUTINE MAKES THE ITTH COLUMN OF ZL                           
C CONJUGATE TO THE OTHER COLUMNS                                        
      DO  2 I = 1, JT                                                   
         SUM = 0.E0                                                     
         IDEX = JSIM(I)                                                 
         DO  1 J = 1, JT                                                
            JDEX = JSIM(J)                                              
            A = Q(JDEX, IDEX)                                           
            IF (JDEX .GT. IDEX) A = Q(IDEX, JDEX)                       
            SUM = SUM+A*ZL(J, IT)                                       
   1        CONTINUE                                                    
         E(I) = SUM                                                     
   2     CONTINUE                                                       
C FORM Z(TRANSPOSE)QZ                                                   
      DO  3 I = IT, JT                                                  
         Y(I) = SDOT(JT, ZL(1, I), 1, E(1), 1)                          
   3     CONTINUE                                                       
      D(IT) = Y(IT)                                                     
      POSDEF = .TRUE.                                                   
      DO  4 I = IT, JT                                                  
         E(I) = 0.E0                                                    
   4     CONTINUE                                                       
      IF (IT .GE. JT) GOTO 5                                            
         IF (IT .LT. JT-1) CALL U6DAT(IT+2, POSDEF, JT, IT+1, D, E, Y   
     1      , ZL, N)                                                    
         E(IT) = Y(IT+1)                                                
         CALL T6DIA(IT, IT+1, JT, D, E, ZL, N)                          
         IF (E(IT) .NE. 0.E0) POSDEF = .FALSE.                          
   5  IF (D(IT) .LE. 0.E0) POSDEF = .FALSE.                             
      RETURN                                                            
      END                                                               
      SUBROUTINE U6DAT(IT, POSDEF, N, IT1, D, E, Y, ZL, NN)             
      INTEGER NN, N                                                     
      INTEGER IT, IT1                                                   
      LOGICAL POSDEF                                                    
      REAL D(N), E(N), Y(N), ZL(NN, N)                                  
      INTEGER I, J, ITM1                                                
      LOGICAL PIV                                                       
      REAL TEMP, C, F                                                   
      LOGICAL TEMP1                                                     
C                                                                       
C THIS SUBROUTINE IS CALLED BY BOTH Q6DRP AND Q6ADD WHEN TRYING         
C TO MAKE THE REPRESENTATION OF THE NULL SPACE CONJUGATE TO             
C THE HESSIAN OF THE FUNCTION TO BE MINIMIZED                           
C                                                                       
      E(N) = 0.E0                                                       
      ITM1 = IT-1                                                       
      I = N                                                             
   1  IF (I .EQ. IT-1) GOTO  15                                         
         IF (I .EQ. IT) GOTO 11                                         
            IF (E(I-2) .EQ. 0.E0) GOTO 10                               
               CALL E6LIM(N, Y(I-1), Y(I-2), C, I-1, I-2, E(I-2),       
     1            PIV, D, E, ZL, NN)                                    
               CALL E6LIM(N, Y(I), Y(I-2), C, I, I-2, F, PIV, D, E, ZL, 
     1            NN)                                                   
               IF (E(I) .EQ. 0.E0) GOTO 5                               
                  IF (.NOT. PIV) GOTO 3                                 
                     TEMP = D(I+1)                                      
C                                                                       
C**********************                                                 
C                                                                       
                     D(I+1) = D(I-1)                                    
                     D(I-1) = TEMP                                      
                     TEMP = E(I)                                        
                     E(I) = E(I-2)                                      
                     E(I-2) = TEMP                                      
                     DO  2 J = 1, N                                     
                        TEMP = ZL(J, I-1)                               
                        ZL(J, I-1) = ZL(J, I+1)                         
                        ZL(J, I+1) = TEMP                               
   2                    CONTINUE                                        
   3              E(I-1) = (-C)*E(I-2)                                  
                  CALL E6LIM(N, F, E(I-2), C, I, I-1, E(I-1), PIV, D, E 
     1               , ZL, NN)                                          
                  IF (.NOT. PIV) GOTO 4                                 
                     F = E(I)                                           
                     E(I) = (-C)*F                                      
                     CALL E6LIM(N, F, E(I-1), C, I+1, I, E(I), PIV,     
     1                  D, E, ZL, NN)                                   
   4              CALL T6DIA(I-2, I+1, N, D, E, ZL, NN)                 
                  I = I-2                                               
                  GOTO  9                                               
   5              IF (PIV) GOTO 6                                       
                     E(I-1) = (-C)*E(I-2)                               
                     CALL E6LIM(N, F, E(I-2), C, I, I-1, E(I-1), PIV, D 
     1                  , E, ZL, NN)                                    
                     GOTO  8                                            
   6                 E(I-1) = E(I-2)                                    
                     E(I-2) = F                                         
                     DO  7 J = 1, N                                     
                        TEMP = ZL(J, I)                                 
                        ZL(J, I) = ZL(J, I-1)                           
                        ZL(J, I-1) = TEMP                               
   7                    CONTINUE                                        
                     TEMP = D(I)                                        
                     D(I) = D(I-1)                                      
                     D(I-1) = TEMP                                      
   8              CALL T6DIA(I-2, I, N, D, E, ZL, NN)                   
                  I = I-2                                               
   9           GOTO  1                                                  
  10        CONTINUE                                                    
  11     IF (E(I) .EQ. 0.E0) GOTO 13                                    
            CALL E6LIM(N, Y(I), Y(I-1), C, I, I-1, E(I-1), PIV, D,      
     1         E, ZL, NN)                                               
C                                                                       
C**********************                                                 
C                                                                       
            IF (.NOT. PIV) GOTO 12                                      
               F = E(I)                                                 
               E(I) = (-C)*F                                            
               CALL E6LIM(N, F, E(I-1), C, I+1, I, E(I), PIV, D, E, ZL, 
     1            NN)                                                   
  12        CALL T6DIA(I-1, I+1, N, D, E, ZL, NN)                       
            I = I-1                                                     
            GOTO  14                                                    
  13        CALL E6LIM(N, Y(I), Y(I-1), C, I, I-1, E(I-1), PIV, D,      
     1         E, ZL, NN)                                               
            IF (I .GT. IT1) CALL T6DIA(I-1, I, N, D, E, ZL, NN)         
            I = I-1                                                     
  14     CONTINUE                                                       
         GOTO  1                                                        
  15  POSDEF = .TRUE.                                                   
      DO  16 I = IT1, N                                                 
         TEMP1 = D(I) .LE. 0.E0                                         
         IF (.NOT. TEMP1) TEMP1 = E(I) .NE. 0.E0                        
         IF (TEMP1) POSDEF = .FALSE.                                    
  16     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE P6MDC(A, IDIM, N, CHANGE, IT)                          
      INTEGER IDIM, N                                                   
      INTEGER IT                                                        
      REAL A(IDIM, N), CHANGE(N)                                        
      INTEGER IP1, IP2, JM1, JP1, I, J                                  
      INTEGER K                                                         
      REAL LAMBDA, SAVE, TEMP, AIP1I, AII                               
      REAL ALPHA, DET, SIGMA, SQRT, AIP1                                
C                                                                       
C GIVEN A SYMMETRIC INDEFINITE MATRIX OF ORDER N,THIS SUBROUTINE        
C DETERMINES ITS DECOMPOSITION INTO PMDM(TRANSPOSE)P(TRANSPOSE)         
C WHERE P IS A PERMUTATION MATRIX,M IS A UNIT LOWER TRIANGULAR          
C MATRIX, AND D IS A BLOCK DIAGONAL MATRIX WITH BLOCKS OF ORDER         
C 1 OR 2 WHERE D(I+1,I) IS NONZERO WHENEVER M(I+1,I) IS ZERO.           
C ONLY THE LOWER TRIANGULAR PORTION OF A IS USED. THE DECOMPOSITION     
C IS PLACED IN THE LOWER TRIANGULAR PORTION. THUS IF ALL THE ELEMENTS   
C OF A ARE SPECIFIED,THE STRICT UPPER TRIANGLE IS NOT DESTROYED BUT     
C THE DIAGONAL IS DESTROYED.  ON OUTPUT THE VECTOR CHANGE OF            
C LENGTH N WILL CONTAIN A RECORD OF THE PERMUTATIONS GENERATED.  THE    
C INTEGER VARIABLE IDIM GIVES THE ROW DIMENSION O THE A MATRIX.         
C                                                                       
      CHANGE(N) = N                                                     
      ALPHA = (SQRT(17.E0)+1.E0)/8.E0                                   
      I = IT                                                            
   1  IF (I .GE. N) GOTO  19                                            
         AII = ABS(A(I, I))                                             
         CHANGE(I) = I                                                  
C                                                                       
C FIND THE LARGEST OFF DIAGONAL ELEMENT IN THE ITH COLUMN               
C                                                                       
         J = I+1                                                        
         IP1 = I+1                                                      
         LAMBDA = ABS(A(IP1, I))                                        
         IP2 = I+2                                                      
         IF (IP2 .GT. N) GOTO 4                                         
            DO  3 K = IP2, N                                            
               IF (ABS(A(K, I)) .LE. LAMBDA) GOTO 2                     
                  LAMBDA = ABS(A(K, I))                                 
                  J = K                                                 
   2           CONTINUE                                                 
   3           CONTINUE                                                 
   4     TEMP = ALPHA*LAMBDA                                            
         IF (AII .GE. TEMP) GOTO 15                                     
            SIGMA = LAMBDA                                              
C                                                                       
C FIND THE   LARGEST OFFDIAGONAL ELEMENT IN THE JTH COLUMN              
C                                                                       
            JM1 = J-1                                                   
            IF (IP1 .GT. JM1) GOTO 6                                    
               DO  5 K = IP1, JM1                                       
                  IF (ABS(A(J, K)) .GT. SIGMA) SIGMA = ABS(A(J, K))     
   5              CONTINUE                                              
   6        JP1 = J+1                                                   
            IF (JP1 .GT. N) GOTO 8                                      
               DO  7 K = JP1, N                                         
                  IF (ABS(A(K, J)) .GT. SIGMA) SIGMA = ABS(A(K, J))     
   7              CONTINUE                                              
   8        IF (AII*SIGMA .GE. TEMP*LAMBDA) GOTO 14                     
               IF (ABS(A(J, J)) .GE. ALPHA*SIGMA) GOTO 13               
                  CHANGE(I) = J                                         
C                                                                       
C PERFORM A 2 BY 2 PIVOT STEP                                           
C                                                                       
                  IF (J .EQ. IP1) GOTO 9                                
                     CALL E6CHG(A, IDIM, N, J, IP1)                     
                     TEMP = A(J, I)                                     
                     A(J, I) = A(IP1, I)                                
                     A(IP1, I) = TEMP                                   
   9              DET = A(I, I)*A(IP1, IP1)/A(IP1, I)-A(IP1, I)         
                  AIP1I = A(IP1, I)                                     
                  AII = A(I, I)/AIP1I                                   
                  AIP1 = A(IP1, IP1)                                    
                  IF (IP2 .GT. N) GOTO 12                               
                     DO  11 J = IP2, N                                  
                        TEMP = (A(J, I)-AII*A(J, IP1))/DET              
                        SAVE = (-(AIP1*TEMP+A(J, IP1)))/AIP1I           
                        DO  10 K = J, N                                 
                           A(K, J) = A(K, J)+A(K, I)*SAVE+A(K, IP1)*    
     1                        TEMP                                      
  10                       CONTINUE                                     
                        A(J, I) = SAVE                                  
                        A(J, IP1) = TEMP                                
  11                    CONTINUE                                        
  12              CHANGE(IP1) = -1                                      
                  I = IP2                                               
                  GOTO  1                                               
C                                                                       
C INTERCHANGE THE ITH AND JTH ROWS AND COLUMNS                          
C                                                                       
  13           CHANGE(I) = J                                            
               CALL E6CHG(A, IDIM, N, J, I)                             
  14        CONTINUE                                                    
C                                                                       
C PERFORM A 1 X 1 PIVOT                                                 
C                                                                       
  15     IF (A(I, I) .EQ. 0.E0) GOTO 18                                 
            AII = A(I, I)                                               
            DO  17 J = IP1, N                                           
               SAVE = (-A(J, I))/AII                                    
               DO  16 K = J, N                                          
                  A(K, J) = A(K, J)+A(K, I)*SAVE                        
  16              CONTINUE                                              
               A(J, I) = SAVE                                           
  17           CONTINUE                                                 
  18     I = IP1                                                        
         GOTO  1                                                        
  19  RETURN                                                            
      END                                                               
      SUBROUTINE E6LIM(N, A, B, C, I, J, F, PIV, D, E, ZL, NN)          
      INTEGER NN, N                                                     
      INTEGER I, J                                                      
      LOGICAL PIV                                                       
      REAL A, B, C, F, D(N), E(N)                                       
      REAL ZL(NN, N)                                                    
      INTEGER K                                                         
      REAL ABS, H, S                                                    
C                                                                       
C                                                                       
C                                                                       
C THIS SUBROUTINE APPLIES ONE STEP OF GAUSSIAN ELIMINATION WITH         
C PARTIAL PIVOTING IN THEI AND JTH PLANES TO ELIMINATE THE              
C VALUE CONTAINED IN A WHICH IS IN THE I TH PLANE USING                 
C B IN THE JTH PLANE. THE TRANSFORMATIONS CONSTRUCTED ARE APPLIED       
C AS CONGRUENCE TRANSFORMATIONS TO A TRIDIAGONAL MATRIX                 
C IN VECTORS D AND E AND AS A COLUMN TRANSFORMATION TO THE ZL MATRIX.   
C                                                                       
      H = E(J)                                                          
      IF (I .GT. J+1) H = 0                                             
      F = H                                                             
      IF (ABS(A) .LE. ABS(B)) GOTO 2                                    
         C = B/A                                                        
         F = H-C*D(I)                                                   
         S = D(I)                                                       
         D(I) = D(J)-C*(H+F)                                            
         D(J) = S                                                       
         DO  1 K = 1, N                                                 
            S = ZL(K, I)                                                
            ZL(K, I) = ZL(K, J)-C*S                                     
            ZL(K, J) = S                                                
   1        CONTINUE                                                    
         B = A                                                          
         PIV = .TRUE.                                                   
         GOTO  5                                                        
   2     C = 0.E0                                                       
         IF (A .EQ. 0.E0) GOTO 4                                        
            C = A/B                                                     
            F = H-C*D(J)                                                
            D(I) = D(I)-C*(H+F)                                         
            DO  3 K = 1, N                                              
               ZL(K, I) = ZL(K, I)-C*ZL(K, J)                           
   3           CONTINUE                                                 
   4     PIV = .FALSE.                                                  
   5  RETURN                                                            
      END                                                               
      SUBROUTINE T6DIA(J, K, N, D, E, ZL, NN)                           
      INTEGER NN, N                                                     
      INTEGER J, K                                                      
      REAL D(N), E(N), ZL(NN, N)                                        
      INTEGER I, L                                                      
      REAL ABS, AMAX1, C, DEN, ALPHA, SIGMA                             
      REAL C1, C2                                                       
C                                                                       
C THIS SUBROUTINE TAKES A SYMMETRIC TRIDIAGONAL MATRIX WHOSE            
C DIAGONAL ELEMENTS ARE STORED IN ELEMENTS J THROUGH K OF THE           
C VECTOR D AND WHOSE OFF DIAGONAL ELEMENTS ARE STORED IN                
C ELEMENTS J THROUGH K-1 AND REDUCES IT BY CONGRUENCE                   
C TRANSFORMATIONS WITHOUT PIVOTING TO BLOCK DIAGONAL FORM               
C WHERE THE BLOCKS ARE OF ORDER 1 AND 2.  THE RIGHT                     
C TRANSFORMATIONS ARE ALSO APPLIED TO THE CORRESPONDING COLUMNS         
C OF THE Z MATRIX.;                                                     
C                                                                       
      I = K                                                             
   1     SIGMA = AMAX1(ABS(E(I-1)), ABS(D(I-1)))                        
         ALPHA = .5                                                     
         IF (I-1 .GT. J) SIGMA = AMAX1(SIGMA, ABS(E(I-2)))              
         IF (SIGMA*ABS(D(I)) .GE. ALPHA*E(I-1)*E(I-1)) GOTO 3           
            IF (I .EQ. J+1) GOTO  7                                     
            DEN = D(I)*D(I-1)/E(I-1)-E(I-1)                             
            C1 = (-D(I))/E(I-1)*E(I-2)/DEN                              
            C2 = (-(C1*D(I-1)+E(I-2)))/E(I-1)                           
            DO  2 L = 1, N                                              
               ZL(L, I-2) = ZL(L, I-2)+C2*ZL(L, I)+C1*ZL(L, I-1)        
   2           CONTINUE                                                 
            D(I-2) = D(I-2)+C1*E(I-2)                                   
            E(I-2) = 0.E0                                               
            I = I-2                                                     
            GOTO  6                                                     
   3        IF (E(I-1) .EQ. 0.E0) GOTO 5                                
               C = E(I-1)/D(I)                                          
               DO  4 L = 1, N                                           
                  ZL(L, I-1) = ZL(L, I-1)-C*ZL(L, I)                    
   4              CONTINUE                                              
               D(I-1) = D(I-1)-C*E(I-1)                                 
               E(I-1) = 0.E0                                            
C                                                                       
   5        I = I-1                                                     
   6     IF (I .EQ. J) GOTO  7                                          
         GOTO  1                                                        
   7  RETURN                                                            
      END                                                               
      SUBROUTINE E6CHG(A, IDIM, N, J, I)                                
      INTEGER IDIM, N                                                   
      INTEGER J, I                                                      
      REAL A(IDIM, N)                                                   
      INTEGER JP1, IP1, JM1, K                                          
      REAL TEMP                                                         
C                                                                       
C THIS SUBROUTINE IS CALLED BY P6MDC TO INTERCHANGE ROWS AND            
C COLUMNS I AND J OF A SYMMETRIC MATRIX WHEN ONE IS WORKING ONLY        
C WITH THE LOWER TRIANGULAR PORTION OF THAT MATRIX.                     
C                                                                       
C                                                                       
C INTERCHANGE THE ELEMENTS BELOW BOTH DIAGONALS                         
C                                                                       
      JP1 = J+1                                                         
      IF (JP1 .GT. N) GOTO 2                                            
         DO  1 K = JP1, N                                               
            TEMP = A(K, J)                                              
            A(K, J) = A(K, I)                                           
            A(K, I) = TEMP                                              
   1        CONTINUE                                                    
   2  IF (I+1 .GT. J-1) GOTO 4                                          
         IP1 = I+1                                                      
         JM1 = J-1                                                      
         DO  3 K = IP1, JM1                                             
            TEMP = A(K, I)                                              
            A(K, I) = A(J, K)                                           
            A(J, K) = TEMP                                              
   3        CONTINUE                                                    
C                                                                       
C INTERCHANGE THE DIAGONAL ELEMENTS                                     
C                                                                       
   4  TEMP = A(I, I)                                                    
      A(I, I) = A(J, J)                                                 
      A(J, J) = TEMP                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE DIQP(N, X, Q, IQ, C, M, A, IA, B, BL, BU,              
     1   IPRINT, MAXITR, IEQ)                                           
      INTEGER IA, IQ, N                                                 
      INTEGER M, IPRINT, MAXITR                                         
      DOUBLE PRECISION X(N), Q(IQ, N), C(N), A(IA, N), B(M), BL(N)      
      DOUBLE PRECISION BU(N)                                            
      DOUBLE PRECISION EPSI                                             
      INTEGER ID, IE, IG, IT, JT                                        
      INTEGER IY, IRES, IAC, I, INJ, IZL                                
      INTEGER IJSIM, IIRES                                              
      INTEGER IEQ, IWUNIT                                               
      DOUBLE PRECISION DDOT, R, D1MACH                                  
      INTEGER IISIMP, IEXTRA, ILIM, IND, I1MACH                         
      INTEGER ICT, IRCT, INDI, INDD,IIND                                
C                                                                       
      DOUBLE PRECISION DSTAK(500)                                       
      COMMON /CSTAK/DSTAK                                               
      INTEGER ISTAK(1000)                                               
      EQUIVALENCE (DSTAK(1), ISTAK(1))                                  
C                                                                       
C THIS SUBROUTINE SOLVES THE INDEFINITE QUADRATIC                       
C PROGRAMMING PROBLEM OF MINIMIZING                                     
C           T      T                                                    
C          X QX/2+C X                                                   
C SUCH THAT                                                             
C        AX.GE.B                                                        
C   AND                                                                 
C         BL(I).LE.X(I).LE.BU(I).I=1,...,N                              
C                                                                       
C PARAMETERS ON INPUT                                                   
C   N       ORDER OF PROBLEM                                            
C   X       INITIAL GUESS, NEED NOT SATISFY CONSTRAINTS                 
C   Q       N X N SYMMETRIC MATRIX,MAY BE INDEFINITE.                   
C           MUST BE FILLED IN COMPLETELY, STRICTLY                      
C           LOWER TRIANGLE DESTROYED                                    
C   IQ      LEADING DIMENSION OF Q. MUST BE AT LEAST N                  
C   C       VECTOR IN LINEAR TERM OF FUNCTION TO BE MINIMIZED           
C   IEQ     NUMBER OF LINEAR EQUALITY CONSTRAINTS                       
C   M       NUMBER OF LINEAR INEQUALITY CONSTRAINTS                     
C   A       M X N MATRIX OF LINEAR CONSTRAINTS                          
C   IA      LEADING DIMENSION OF A, MUST BE AT LEAST M                  
C   B       RIGHT HAND SIDE VECTOR OF CONSTRAINTS                       
C   BL      LOWER BOUND ON VARIABLES                                    
C   BU      UPPER BOUND ON VARIABLES                                    
C   IPRINT  IF IPRINT IS GREATER THAN 0,THEN THE FUNCTION,GRADIENT,     
C           APPROXIMATE SOLUTION WILL BE PRINTED EACH ITERATION.        
C           FEASABILITY CONDITIONS FROM DFEAS WILL ALSO BE PRINTED.     
C  MAXITR   MAXIMUM NUMBER OF ITERATIONS PERMITTED                      
C                                                                       
C OUTPUT PARAMETERS                                                     
C   X       SOLUTION OF THE PROBLEM                                     
C                                                                       
C SCRATCH STORAGE (ALLOCATED FROM THE PORT STACK)                       
C DSTAK     DOUBLE PRECISION ARRAY OF LENGTH AT LEAST M+(4+N)N LOCATIONS
C           (UNLESS (N.GT.M) WHEN (5+N)N LOCATIONS WILL BE ALLOCATED)   
C ISTAK     INTEGER ARRAY OF LENGTH AT LEAST 4N + M LOCATIONS           
C       ISTAK(1...N) = INEQUALITY, EQUALITY FOR UPPER AND LOWER BOUNDS  
C       ISTAK(N+1...N+M) = EQUALITY, INEQUALITY FOR MATRIX CONSTRAINTS  
C       ISTAK(N+M+1...3N+M) = ISIMP FOR FEASA                           
C       ISTAK(3N+M+1...4N+M) = STORAGE SPACE FOR READING OFF ISIMP      
C CHECK FOR ERRORS                                                      
C                                                                       
        CALL ENTER(1)                                                   
        IWUNIT = I1MACH(2)                                              
        EPSI = D1MACH(4)                                                
C/6S                                                                    
C     IF ((N .LT. 1).OR.(M .LT. 0))                                     
C    1  CALL SETERR(21HDIQP-N.LT.1 OR M.LT.0,21,1,2)                    
C     IF (MAXITR .LT. 1) CALL SETERR(16HDIQP-MAXITR.LT.1,16,2,2)        
C     IF (IQ .LT. N) CALL SETERR(12HDIQP-IQ.LT.N,12,3,2)                
C     IF (IA .LT. M) CALL SETERR(12HDIQP-IA.LT.M,12,4,2)                
C     IF ((IEQ .GT. N).OR.(IEQ .GT. M).OR.(IEQ .LT. 0))                 
C    1  CALL SETERR(37HDIQP-IEQ.GT.N OR IEQ.GT.M OR IEQ.LT.0,37,5,2)    
C/7S                                                                    
      IF ((N .LT. 1).OR.(M .LT. 0))                                     
     1  CALL SETERR('DIQP-N.LT.1 OR M.LT.0',21,1,2)                     
      IF (MAXITR .LT. 1) CALL SETERR('DIQP-MAXITR.LT.1',16,2,2)         
      IF (IQ .LT. N) CALL SETERR('DIQP-IQ.LT.N',12,3,2)                 
      IF (IA .LT. M) CALL SETERR('DIQP-IA.LT.M',12,4,2)                 
      IF ((IEQ .GT. N).OR.(IEQ .GT. M).OR.(IEQ .LT. 0))                 
     1  CALL SETERR('DIQP-IEQ.GT.N OR IEQ.GT.M OR IEQ.LT.0',37,5,2)     
C/                                                                      
C USE FEASA TO GET A FEASIBLE SOLUTION (OR CHECK FEASIBILITY OF CURRENT 
C SOLUTION.)                                                            
C                                                                       
C GET SPACE FOR DFEASA AND TRANSLATION                                  
      IAC = ISTKGT(M,2)                                                 
      IISIMP = ISTKGT(2*N,2)                                            
      INDD = ISTKGT(2*N,4)                                              
      CALL DQ6INT(A, M, N, IA, B, X, BL, BU, DSTAK(INDD),               
     1   ISTAK(IISIMP),                                                 
     2   ISTAK(IAC), IAS, IAG, MAXITR, IPRINT, IEQ, IWUNIT)             
C                                                                       
      CALL ISTKRL(1)                                                    
      IF ((NERROR(NERR).NE.6).AND.(NERROR(NERR).NE.8)                   
     1    .AND.(NERROR(NERR).NE.9))  GO TO 301                          
      CALL ERROFF                                                       
C/6S                                                                    
C     IF (NERROR(NERR).EQ.6) CALL SETERR (                              
C    1    49HDIQP-NO FEASIBLE SOLUTION AFTER MAXITR ITERATIONS,49,8,1)  
C     IF (NERROR(NERR).EQ.8) CALL SETERR (17HDIQP-NO FEAS. SOL,17,9,1)  
C     IF (NERROR(NERR).EQ.9) CALL SETERR (18HDIQP-COND. PROBLEM,18,10,1)
C/7S                                                                    
      IF (NERROR(NERR).EQ.6) CALL SETERR (                              
     1    'DIQP-NO FEASIBLE SOLUTION AFTER MAXITR ITERATIONS',49,8,1)   
      IF (NERROR(NERR).EQ.8) CALL SETERR ('DIQP-NO FEAS. SOL',17,9,1)   
      IF (NERROR(NERR).EQ.9) CALL SETERR ('DIQP-COND. PROBLEM',18,10,1) 
C/                                                                      
      CALL LEAVE                                                        
      RETURN                                                            
 301  CONTINUE                                                          
      IRCT = 2*M + (3+N) * N                                            
      IF (N.GT.M) IRCT = (4+N) * N + M                                  
      ICT = 2*N                                                         
      INDD = ISTKGT(IRCT, 4)                                            
      INDI = ISTKGT(ICT, 2)                                             
      IZL = INDD                                                        
      IY = IZL+N*N                                                      
      IE = IY+N                                                         
      ID = IE+N                                                         
      IG = ID+N                                                         
      MN=M                                                              
      IF(N.GT.M) MN=N                                                   
      IRES = IG+MN                                                      
      IJSIM = INDI                                                      
      INJ = IJSIM                                                       
      IEXTRA = IJSIM+N                                                  
      JT = IEXTRA-1                                                     
C                                                                       
      ILIM = IEXTRA+N-1                                                 
      DO  1 I = IEXTRA, ILIM                                            
         ISTAK(I) = 0                                                   
 1    CONTINUE                                                          
C                                                                       
      IF (IAS.EQ.0) GO TO 19                                            
      DO  2 I = 1, IAS                                                  
         IIND = IISIMP + I - 1                                          
         IND = IEXTRA + IABS(ISTAK(IIND)) - 1                           
         ISTAK(IND) = 1                                                 
 2    CONTINUE                                                          
 19   CONTINUE                                                          
      DO  3 I = 1,N                                                     
         IND = IEXTRA + I - 1                                           
         IF(ISTAK(IND).NE.1) GO TO 4                                    
         ISTAK(JT) = I                                                  
         JT = JT - 1                                                    
         GO TO 3                                                        
 4       ISTAK(INJ) = I                                                 
         INJ = INJ + 1                                                  
 3    CONTINUE                                                          
      IT = IAG + IEQ                                                    
      JT = N - IAS                                                      
      ILIM = 2*N + INDD                                                 
      DO 8 I=INDD,ILIM                                                  
        DSTAK(I) = 0.                                                   
 8    CONTINUE                                                          
      IIRES = IRES - 1                                                  
      IF (M.LT.1) GO TO 9                                               
      DO 10 I=1,M                                                       
        R = DDOT(N,A(I,1),IA,X,1) - B(I)                                
        IIRES = IIRES + 1                                               
        IF (R.LT.0.) R = 0.                                             
        DSTAK(IIRES) = R                                                
 10   CONTINUE                                                          
 9    CONTINUE                                                          
C                                                                       
      CALL DQ6IQP(X, N, A, IA, DSTAK(IRES), M, Q, IQ, C, BL, BU, IT     
     1   , JT, ISTAK(IJSIM), ISTAK(IAC), DSTAK(ID), DSTAK(IE)           
     2   , DSTAK(IY), DSTAK(IZL), DSTAK(IG), IPRINT, MAXITR             
     3   , IEQ, IERR, IWUNIT, EPSI,B)                                   
C/6S                                                                    
C     IF (IERR.EQ.1) CALL SETERR(                                       
C    1   40HDIQP-NO SOLUTION AFTER MAXITR ITERATIONS,40,6,1)            
C     IF (IERR.EQ.2) CALL SETERR(23HDIQP-UNBOUNDED SOLUTION,23,7,1)     
C/7S                                                                    
      IF (IERR.EQ.1) CALL SETERR(                                       
     1   'DIQP-NO SOLUTION AFTER MAXITR ITERATIONS',40,6,1)             
      IF (IERR.EQ.2) CALL SETERR('DIQP-UNBOUNDED SOLUTION',23,7,1)      
C/                                                                      
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
        SUBROUTINE DQ6INT(A, M, N, IA, B, X, BL, BU, SIMP, ISIMP, IPTG, 
     1          IAS, IAG, MAXITR, IPRINT, IE, IWUNIT)                   
C                                                                       
C THIS SUBROUTINE PROVIDES AN INTERFACE BETWEEN QP (IN DQPPAK.F)        
C AND DFEASA (IN DLPH11T.F)                                             
C                                                                       
C PARAMETERS ON INPUT                                                   
C   N       ORDER OF PROBLEM                                            
C   X       INITIAL GUESS, NEED NOT SATISFY CONSTRAINTS                 
C   IE      NUMBER OF LINEAR EQUALITY CONSTRAINTS                       
C   M       NUMBER OF LINEAR EQUALITY AND INEQUALITY CONSTRAINTS        
C   A       M X N MATRIX OF LINEAR CONSTRAINTS                          
C   IA      LEADING DIMENSION OF A, MUST BE AT LEAST M                  
C   B       RIGHT HAND SIDE VECTOR OF CONSTRAINTS                       
C   BL      LOWER BOUND ON VARIABLES                                    
C   BU      UPPER BOUND ON VARIABLES                                    
C   SIMP    VECTOR CONTAINING SIMPLE CONSTRAINTS                        
C   S       NUMBER OF SIMPLE CONSTRAINTS                                
C   ISIMP   VECTOR TELLING THE ELEMENT OF X THAT THE SIMPLE             
C           CONSTRAINT PERTAINS TO.  IF NEGATIVE IT IS AN UPPER         
C           BOUND OTHERWISE IT IS A LOWER BOUND.                        
C   IPRINT  IF IPRINT IS GREATER THAN 0,THEN THE FUNCTION,GRADIENT,     
C           APPROXIMATE SOLUTION WILL BE PRINTED EACH ITERATION.        
C           FEASABILITY CONDITIONS FROM DFEAS WILL ALSO BE PRINTED.     
C   MAXITR  MAXIMUM NUMBER OF ITERATIONS PERMITTED                      
C                                                                       
C   PARAMETERS ON OUTPUT                                                
C                                                                       
C   X       FEASIBLE SOLUTION COMPUTED BY DFEASA                        
C                                                                       
        DOUBLE PRECISION DSTAK(500)                                     
        COMMON/CSTAK/DSTAK                                              
        INTEGER M, N, IA, IFLAG                                         
        DOUBLE PRECISION A(IA,N), B(M), X(N), BL(N), BU(N)              
        INTEGER ICT, ISIMP(1), MAXITR, IE, IPTG(M), SMAX, IAG, IAS      
        INTEGER IPRINT, IWUNIT, IND                                     
        DOUBLE PRECISION SIMP(1)                                        
        EXTERNAL DLPRNT,DQP1NT                                          
        EXTERNAL DLPMAN                                                 
C                                                                       
        IFLAG = 0                                                       
C TRANSLATE BL AND BU TO SIMP AND ISIMP                                 
        DO 1 I=1,N                                                      
            SIMP(I) = BL(I)                                             
            ISIMP(I) = I                                                
            IND = N + I                                                 
            SIMP(IND) = BU(I)                                           
            ISIMP(IND) = -I                                             
 1      CONTINUE                                                        
        ICT = 2*N                                                       
        SMAX = MAXITR                                                   
C                                                                       
 100    IF (IPRINT.LE.0) GOTO 2                                         
            CALL DFEASA(A, M, N, DLPMAN, IA, B, X, SMAX, ICT, SIMP,     
     1          ISIMP, IE, DQP1NT, IAG, IAS, IPTG)                      
            GO TO 31                                                    
 2      CALL DFEASA(A, M, N, DLPMAN, IA, B, X, SMAX, ICT, SIMP, ISIMP,  
     1      IE, DLPRNT, IAG, IAS, IPTG)                                 
C NO. OF ITERATIONS .GT. MAXITR                                         
 31        IF (NERROR(NERR).NE.6) GO TO 106                             
           IF (IFLAG.NE.0) GO TO 3                                      
           IFLAG = 1                                                    
           SMAX = 3*N                                                   
           CALL ERROFF                                                  
           GOTO 100                                                     
C NO FEASIBLE SOLUTION                                                  
 106    IF (NERROR(NERR).EQ.8) GO TO 3                                  
C CONDITIONING PROBLEM                                                  
        IF ((NERROR(NERR).NE.9).OR.(IFLAG.NE.0)) GO TO 3                
           IFLAG = 1                                                    
           CALL ERROFF                                                  
           GO TO 100                                                    
C                                                                       
 3      RETURN                                                          
        END                                                             
      SUBROUTINE DQ6IQP(X, N, A, IA, RES, IP, Q, IQQ, C, BL, BU, IT     
     1   , JT, JSIM, IAC, D, E, Y, ZL, G, IPRINT, MAXITR, IE            
     2   , IERR, IWUNIT, EPSI,B)                                        
      INTEGER IA, N, IQQ                                                
      INTEGER IP, IT, JT, JSIM(N), IAC(IA), IPRINT                      
      INTEGER MAXITR                                                    
      DOUBLE PRECISION X(IQQ), A(IA, N), RES(IA), Q(IQQ, N), C(N), BL(N)
      DOUBLE PRECISION B(IP)                                            
      DOUBLE PRECISION RELDF,RELDX,OLDX,OLDF,XNORM,DSQRT                
      DOUBLE PRECISION BU(N), D(N), E(N), Y(N), ZL(N, N), G(N)          
      INTEGER IB, IC, JDEC, JC, IQ, IS                                  
      INTEGER JDEX, IZ, IDEX, ITER, I, J                                
      INTEGER K, ITM1, ITP1, JTP1                                       
      LOGICAL POSDEF, CONVER                                            
      DOUBLE PRECISION LAMBDA, ACUM, DDOT, F, S, SUM, EPS               
      DOUBLE PRECISION DNRM2, SS, EPSI                                  
      INTEGER IE                                                        
      LOGICAL TEMP, TEMP2                                               
      INTEGER IWUNIT, ITMP                                              
C                                                                       
C                                                                       
C THIS SUBROUTINE SOLVES THE INDEFINITE QUADRATIC PROGRAMMING           
C PROBLEM OF MINIMIZING X(TRANSPOSE)QX/2+C(TRANSPOSE)X                  
C SUCH THAT AX-B=RES IS GREATER OR EQUAL TO ZERO.                       
C ON INPUT IT IS ASSUMED THAT THE FIRST IT CONSTRAINTS ARE              
C ACTIVE AT THE INITIAL GUESS.I.E. THE ROWS OF A HAVE BEEN              
C PERMUTED SO THAT RES(I)=0,I=1,...IT.                                  
C PARAMETERS ON INPUT*                                                  
C    X    AN N VECTOR OF INITIAL GUESSES                                
C    N    THE LENGTH OF THE X VECTOR                                    
C    A    AN IP X N MATRIX DEFINING THE LINEAR INEQUALITY               
C          CONSTRAINTS OF ROW DIMENSION IA                              
C    IA   ROW DIMENSION OF A MATRIX, MUST BE AT LEAST IP                
C   RES   THE RESIDUAL AT THE INITIAL GUESS                             
C         WHERE RES(I)=SUM OVER J OF (A(IAC(I),J)*X(J))-B(IAC(I))       
C    IP   THE TOTAL NUMBER OF GENERAL EQUALITY AND INEQUALITY           
C         CONSTRAINTS                                                   
C    Q    A SYMMETRIC INDEFINITE MATRIX,COMPLETELY FILLED IN            
C         OF DIMENSION N X N DEFINING THE HESSIAN OF THE                
C         FUNCTION TO BE MINIMIZED                                      
C    IQQ   ROW DIMENSION OF Q MATRIX AT LEAST N.                        
C    C    AN N VECTOR INVOLVED IN THE SECOND TERM IN THE FUNCTION       
C         TO BE MINIMIZED                                               
C    BL   REAL ARRAY, LENGTH AT LEAST N, WHOSE ITH COMPONENT GIVES      
C         LOWER BOUND ON X(I)                                           
C    BU   REAL ARRAY, LENGTH AT LEAST N, WHOSE ITH COMPONENT GIVES      
C         AN UPPER BOUND ON X(I)                                        
C    IT   THE NUMBER OF EQUALITY AND INEQUALITY CONSTRAINTS ACTIVE      
C         AT THE INITIAL GUESS                                          
C    JT   A POINTER INTO THE JSIM ARRAY, X(JSIM(JT+1)) THROUGH          
C         X(JSIM(N)) ARE CONSIDERED AT BOUNDS. USUALLY JSIM IS          
C         SET INITIALLY TO N AND EVERY VARIABLE MAY MOVE.               
C  JSIM   AN ARRAY OF LENGTH N CONTAINING THE NUMBERS 1 THROUGH N       
C         IN ANY ORDER. THE FIRST JT NUMBERS OF JSIM INDICATE           
C         THE ELEMENTS OF X WHICH ARE NOT AT THEIR BOUNDS.              
C  IAC    AN ARRAY OF LENGTH IP CONTAINING THE NUMBERS 1 THOUGH IP      
C         IN ANY ORDER EXCEPT THAT THE FIRST IT NUMBERS POINT TO        
C         CONSTRAINTS THAT ARE INITIALLY ACTIVE                         
C  IPRINT IF IPRINT .LE. 0, NOTHING IS PRINTED. OTHERWISE               
C         FUNCTION,GRADIENT, AND SOLUTION ARE PRINTED                   
C         EACH ITERATION.                                               
C MAXITR  MAXIMUM NUMBER OF PERMITTED ITERATIONS                        
C    IE   THE TOTAL NUMBER OF EQUALITY CONSTRAINTS                      
C         THE FIRST IE ROWS OF A SHOULD BE THE EQUALITY CONSTRAINTS     
C         AND THE FIRST IE ELEMENTS OF IAC SHOULD POINT TO THEM.        
C  IWUNIT OUTPUT UNIT NUMBER FOR PRINTING                               
C  EPSI   MACHINE PRECISION                                             
C                                                                       
C SCRATCH SPACE                                                         
C    D    SCRATCH VECTOR OF LENGTH N                                    
C    E    SCRATCH VECTOR OF LENGTH N                                    
C    Y    SCRATCH VECTOR OF LENGTH N                                    
C    ZL   SCRATCH ARRAY DIMENSIONED (N,N) WHICH WILL HAVE               
C         ORTHOGONAL DECOMPOSITION FOR ACTIVE CONSTRAINT MATRIX.        
C    G    A SCRATCH VECTOR OF LENGTH NIP .GT. MAX(N,IP)                 
C                                                                       
C PARAMETERS ON OUTPUT                                                  
C   X    THE SOLUTION                                                   
C   IT   THE NUMBER OF ACTIVE CONSTRAINTS AT THE SOLUTION               
C   Q    THE LOWER HALF OF THIS MATRIX HAS BEEN DESTROYED               
C  IERR  ERROR CONDITION,IF 0 -NO PROBLEM                               
C                        IF 4 - MAXIMUM NUMBER OF ITERATONS USED        
C                                                                       
C DETERINE E,D,L,Z                                                      
C                                                                       
      ITER = 0                                                          
      RELDX = 0.0D0                                                     
      IQ1=0                                                             
      IF (JT .EQ. 0) GOTO 1                                             
         CALL DG6TZD(A, Q, N, IT, IP, POSDEF, IA, IQQ, JT, JSIM, IAC, D 
     1      , E, Y, ZL, IE, EPSI)                                       
         GOTO  2                                                        
   1     IT = 0                                                         
   2  IZ = N                                                            
      CONVER = .FALSE.                                                  
C                                                                       
C COMPUTE THE GRADIENT= QX+C                                            
C                                                                       
      ITP1 = IT+1                                                       
   3     ITER = ITER+1                                                  
         IF (ITER .LE. MAXITR) GOTO 4                                   
            IERR = 1                                                    
            RETURN                                                      
   4     DO  5 I = 1, N                                                 
            G(I)=C(I)                                                   
            IF(I.GT.1)G(I) = C(I)+DDOT(I-1, Q(1, I), 1, X(1), 1)        
            G(I) = G(I)+DDOT(N-I+1, Q(I, I), IQQ, X(I), 1)              
   5        CONTINUE                                                    
         IF (IPRINT .LE. 0) GOTO 12                                     
C           F = DDOT(N, G(1), 1, X(1), 1)                               
            F = .5 * (DDOT(N, G(1), 1, X(1), 1) +                       
     1          DDOT(N, C(1), 1, X(1), 1))                              
              RELDF=0.0D0                                               
             IF (ITER.NE.1.AND.F.EQ.0.0D0)RELDF=F-OLDF                  
             IF(ITER.NE.1.AND.F.NE.0.0D0)RELDF=(F-OLDF)/F               
              OLDF=F                                                    
  12     TEMP = IT .GE. JT                                              
         S=-1.0D0                                                       
         IS=0                                                           
         IS1=0                                                          
         IF (.NOT. TEMP) TEMP = CONVER                                  
         IF (.NOT. TEMP) GOTO 36                                        
            CONVER = .FALSE.                                            
C                                                                       
C HAVE FOUND MINIMUM IN THE SUBSPACE,DETERMINE IF IT IS                 
C ALSO THE MINIMUM OF THE PROBLEM BY FINDING THE LAGRANGE               
C MULTIPLIERS                                                           
745          CONTINUE                                                   
            S = 1.D0                                                    
C                                                                       
            IF (IT .LE. 0) GOTO 21                                      
               IF (JT .NE. 0) GOTO 14                                   
                  DO  13 I = 1, IT                                      
                     Y(I) = 0.D0                                        
  13                 CONTINUE                                           
                  GOTO  20                                              
  14              DO  16 I = 1, IT                                      
                     ACUM = 0.D0                                        
                     DO  15 J = 1, JT                                   
                        JDEC = JSIM(J)                                  
                        ACUM = ACUM+ZL(J, I)*G(JDEC)                    
  15                    CONTINUE                                        
                     Y(I) = ACUM                                        
  16                 CONTINUE                                           
                  IS = IT                                               
                  Y(IT) = Y(IT)/D(IT)                                   
                  IF (IT.GT.IE)S = Y(IT)                                
                  IF (IT .EQ. 1) GOTO 19                                
                     ITM1 = IT-1                                        
                     DO  18 IB = 1, ITM1                                
                        I = IT-IB                                       
                        Y(I) = (Y(I)-DDOT(IB, Q(I+1, I), 1, Y(I+1), 1))/
     1                     D(I)                                         
                        IF (Y(I).GT.S.OR.I.LE.IE) GO TO 17              
                           IS = I                                       
                           S = Y(I)                                     
  17                    CONTINUE                                        
  18                    CONTINUE                                        
  19              CONTINUE                                              
  20        CONTINUE                                                    
  21        IF (JT .GE. N) GOTO 26                                      
               JTP1 = JT+1                                              
               DO  25 I = JTP1, N                                       
                  IDEX = JSIM(I)                                        
                  ACUM = G(IDEX)                                        
                  IF (IT .LT. 1) GOTO 23                                
                     DO  22 J = 1, IT                                   
                        JC = IAC(J)                                     
                        ACUM = ACUM-A(JC, IDEX)*Y(J)                    
  22                    CONTINUE                                        
  23              IF (X(IDEX) .GT. BL(IDEX)) ACUM = -ACUM               
                  IF (ACUM .GE. S) GOTO 24                              
                     IS = -I                                            
                     S = ACUM                                           
  24              CONTINUE                                              
  25              CONTINUE                                              
C                                                                       
C TEST FOR CONVERGENCE BY SEEING IF S IS NEGATIVE                       
C                                                                       
  26        CONTINUE                                                    
            IF (IPRINT.LE.0) GO TO 27                                   
             IS1=0                                                      
             IF (IS.GT.0)IS1=IAC(IS)                                    
             ISM=-IS                                                    
             IF (IS.LT.0)IS1=-JSIM(ISM)                                 
  27        IF (S .LE. 0.D0) GOTO 29                                    
               IF(JT-ITP1.GT.0)GO TO35                                  
              IF (IPRINT.GT.0)                                          
     1        CALL DQP2NT(ITER,N,IP,X,A,IA,B,F,RELDF,RELDX,JT,JSIM,     
     1       IT,IAC,IS1,IQ1,POSDEF,BU)                                  
               RETURN                                                   
C                                                                       
C DROP CONSTRAINT IS, NO CONVERGENCE                                    
C                                                                       
 29         IF (IS .LE. 0) GOTO 32                                      
               CALL DQ6DRP(Q, IQQ, IS, IT, IP, N, POSDEF, JT, IAC,      
     1            JSIM, D, E, Y, ZL, IWUNIT)                            
               GOTO  35                                                 
 32            CALL DD6SIM(A, IA, N, Q, IQQ, IS, IT, JT, POSDEF, JSIM,  
     1            IAC, D, E, Y, ZL, IWUNIT)                             
  35        CONTINUE                                                    
C                                                                       
C                                                                       
C COMPUTE THE PROJECTED GRADIENT,UHY=Z(TRANSPOSE)G                      
C                                                                       
  36     ITP1 = IT+1                                                    
         DO  38 I = ITP1, JT                                            
            SUM = 0.D0                                                  
            DO  37 J = 1, JT                                            
               JDEX = JSIM(J)                                           
               SUM = SUM+ZL(J, I)*G(JDEX)                               
  37           CONTINUE                                                 
            Y(I) = SUM                                                  
  38        CONTINUE                                                    
            IF(IPRINT.LE.0) GO TO 389                                   
              CALL DQP2NT(ITER,N,IP,X,A,IA,B,F,RELDF,RELDX,JT,JSIM,     
     1       IT,IAC,IS1,IQ1,POSDEF,BU)                                  
 389         IF(S.GT.0.D0) RETURN                                       
            EPS=EPSI*10.0D0                                             
             SS=0.0D0                                                   
             DO 367 I=ITP1,JT                                           
                SS=SS+DABS(Y(I))                                        
 367         CONTINUE                                                   
            IF (SS.LT.EPS*10.0D0.AND.S.LT.0.0D0)GO TO 745               
             IF (SS.LT.EPS)RETURN                                       
C                                                                       
C COMPUTE DBAR AND G=-DBAR(INVERSE)Y                                    
C                                                                       
         CALL DD6BAR(G, JT, ITP1, POSDEF, D, E, Y, EPSI)                
C                                                                       
C COMPUTE THE SEARCH DIRECTION AND PUT IT IN Y                          
C                                                                       
         DO  39 I = 1, JT                                               
            Y(I) = DDOT(JT-IT, ZL(I, ITP1), IZ, G(ITP1), 1)             
  39        CONTINUE                                                    
C                                                                       
C FIND TH  DISTANCE TO THE NEAREST CONSTRAINT ALONG Y                   
C                                                                       
         IQ = 0                                                         
         EPS=EPSI*DNRM2(JT,Y,1)                                         
         LAMBDA = -1.D0                                                 
         IF (POSDEF) LAMBDA = 1.D0                                      
         IF (IT .EQ. IP) GOTO 44                                        
            DO  43 I = ITP1, IP                                         
               SUM = 0.D0                                               
               IC = IAC(I)                                              
               DO  40 K = 1, JT                                         
                  JDEX = JSIM(K)                                        
                  SUM = SUM+A(IC, JDEX)*Y(K)                            
  40              CONTINUE                                              
               G(I) = SUM                                               
               ACUM = G(I)                                              
               IF (-ACUM.LT.EPS) GO TO 43                               
                  ACUM = (-RES(IC))/ACUM                                
                  TEMP = LAMBDA .LT. 0.D0                               
                  IF (.NOT. TEMP) TEMP = ACUM .LT. LAMBDA               
                  IF (.NOT. TEMP) GOTO 41                               
                     IQ = I                                             
                     LAMBDA = ACUM                                      
  41              CONTINUE                                              
  42           IF (LAMBDA .EQ. 0.D0) GOTO  51                           
  43           CONTINUE                                                 
  44     DO  48 I = 1, JT                                               
            J = JSIM(I)                                                 
            IF (DABS(Y(I)).LT.EPS) GO TO 48                             
            IF (Y(I) .GE. 0.D0) GOTO 45                                 
             IH=-1                                                      
               ACUM = (-(X(J)-BL(J)))/Y(I)                              
               GOTO  46                                                 
  45           ACUM = ((-X(J))+BU(J))/Y(I)                              
                IH=1                                                    
  46        TEMP = LAMBDA .LT. 0.D0                                     
            IF (.NOT. TEMP) TEMP = ACUM .LT. LAMBDA                     
            IF (.NOT. TEMP) GOTO 47                                     
              IHIT=IH                                                   
               LAMBDA = ACUM                                            
               IQ = -I                                                  
  47        IF (LAMBDA .EQ. 0.D0) GOTO  51                              
  48        CONTINUE                                                    
C                                                                       
C UPDAT RESIDUALS                                                       
C                                                                       
         IF (IP.LT.ITP1) GO TO 501                                      
         DO  49 I = ITP1, IP                                            
            IC = IAC(I)                                                 
            RES(IC) = RES(IC)+LAMBDA*G(I)                               
  49        CONTINUE                                                    
          IF (IQ.LE.0) GO TO 501                                        
             IC=IAC(IQ)                                                 
             RES(IC)=0.0D0                                              
 501       CONTINUE                                                     
C                                                                       
C                                                                       
C FIND NEW MINIMUM                                                      
C                                                                       
          RELDX=0.0D0                                                   
         DO  50 I = 1, JT                                               
            JDEX = JSIM(I)                                              
            X(JDEX) = X(JDEX)+LAMBDA*Y(I)                               
            RELDX=RELDX+Y(I)*Y(I)                                       
  50        CONTINUE                                                    
            RELDX=DSQRT(RELDX)*LAMBDA                                   
            XNORM=DNRM2(N,X,1)                                          
             IF (XNORM.NE.0.0D0)RELDX=RELDX/XNORM                       
  51     CONTINUE                                                       
C                                                                       
C HAS AA CONSTRAINT BEEN HIT(                                           
C                                                                       
         TEMP = POSDEF                                                  
         IF (TEMP) TEMP = IQ .EQ. 0                                     
         IF (.NOT. TEMP) GOTO 53                                        
            CONVER = .TRUE.                                             
            TEMP2 = IT .EQ. 0                                           
            IF (TEMP2) TEMP2 = JT .EQ. N                                
            IF (.NOT. TEMP2) GOTO 52                                    
               RETURN                                                   
  52        CONTINUE                                                    
            IQ1=IQ                                                      
            GOTO  3                                                     
  53          IF (IQ)56,61,55                                           
  55         CONTINUE                                                   
             IQ1=IAC(IQ)                                                
               CALL DQ6ADD(A, IQ, N, IT, POSDEF, IA, Q, IQQ, JT, JSIM,  
     1            IAC, D, E, Y, ZL)                                     
               GOTO  3                                                  
  56           ITMP = -IQ                                               
               J=JSIM(ITMP)                                             
               X(J)=BU(J)                                               
               IF(IHIT.LT.0) X(J) =BL(J)                                
               IQ1=-J                                                   
  58           CALL DA6SIM(Q, IQQ, IT, JT, IQ, POSDEF, JSIM, D, E, Y,   
     1            ZL, N)                                                
         GOTO  3                                                        
  61  IERR = 2                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE DA6SIM(Q, IQQ, IT, JT, IQ, POSDEF, JSIM, D, E, Y,      
     1   ZL, N)                                                         
      INTEGER N, IQQ                                                    
      INTEGER IT, JT, IQ, JSIM(N)                                       
      LOGICAL POSDEF                                                    
      DOUBLE PRECISION Q(IQQ, N), D(N), E(N), Y(N), ZL(N, N)            
      INTEGER IB, I, J, K, ITE, JTM1                                    
      INTEGER ITP1                                                      
      DOUBLE PRECISION DUM, V1, V2, U2                                  
C THIS SUBROUTINES FIXES UP THE D,Z,Q, AND L MATRICES                   
C WHEN A SIMPLE CONSTRAINT IS ACTIVATED.                                
C                                                                       
      IQ = -IQ                                                          
      ITP1 = IT+1                                                       
C ZERO OUT THE BOTTOM OF THE VECTOR                                     
      IF (ITP1 .GE. JT) GOTO 2                                          
         DO  1 I = ITP1, JT                                             
            Y(I) = ZL(IQ, I)                                            
   1        CONTINUE                                                    
         CALL DU6DAT(IT+2, POSDEF, JT, IT+2, D, E, Y, ZL, N)            
C UPDATE R                                                              
   2  IF (IT .LE. 0) GOTO 4                                             
         DUM = ZL(IQ, ITP1)                                             
         DO  3 IB = 1, IT                                               
            I = ITP1-IB                                                 
            Y(I) = 0.D0                                                 
            CALL DG6TH2(DUM, ZL(IQ, I), V1, V2, U2, DUM)                
            CALL DA6PH2(JT, ZL(1, ITP1), ZL(1, I), 1, V1, V2, U2)       
            CALL DA6PH2(1, Y(I), D(I), 1, V1, V2, U2)                   
            IF (IB .GT. 1) CALL DA6PH2(IB-1, Y(I+1), Q(I+1, I), 1, V1,  
     1         V2, U2)                                                  
   3        CONTINUE                                                    
         ZL(IQ, ITP1) = DUM                                             
   4  JTM1 = JT-1                                                       
C SHOVE THINGS OVER                                                     
      IF (ITP1 .GE. JT) GOTO 10                                         
   6        DO  8 K = ITP1, JTM1                                        
               DO  7 I = 1, JT                                          
                  ZL(I, K) = ZL(I, K+1)                                 
   7              CONTINUE                                              
               D(K) = D(K+1)                                            
               E(K) = E(K+1)                                            
   8           CONTINUE                                                 
   9     CONTINUE                                                       
  10  IF (IQ .EQ. JT) GOTO 12                                           
         DO  11 J = 1, JT                                               
            ZL(IQ, J) = ZL(JT, J)                                       
  11        CONTINUE                                                    
         ITE = JSIM(IQ)                                                 
         JSIM(IQ) = JSIM(JT)                                            
         JSIM(JT) = ITE                                                 
  12  JT = JT-1                                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE DD6BAR(W, N, IBEGIN, POSDEF, D, E, Y, EPSI)            
      INTEGER N                                                         
      INTEGER IBEGIN                                                    
      LOGICAL POSDEF                                                    
      DOUBLE PRECISION W(N), D(N), E(N), Y(N)                           
      INTEGER J, K                                                      
      DOUBLE PRECISION EPS, EPSI                                        
      DOUBLE PRECISION ALFA, DJ, EJ, BETA, YJ, TEMP                     
      DOUBLE PRECISION YTDY, GAMMA, DELTA, SGN, DENOM, DSQRT            
      DOUBLE PRECISION YTDIY, DJP1, YJP1                                
C                                                                       
C THIS SUBROUTINE DETERMINES W=DBAR(INVERSE)*Y                          
C                                                                       
       EPS=EPSI                                                         
      J = IBEGIN                                                        
      K = 0                                                             
      GAMMA = 0.D0                                                      
      ALFA = 0.D0                                                       
   1     J = J+K                                                        
         IF (J .GT. N) GOTO  11                                         
         DJ = D(J)                                                      
         EJ = E(J)                                                      
         YJ = Y(J)                                                      
         IF (EJ .NE. 0.D0) GOTO 5                                       
            K = 1                                                       
C                                                                       
C THE JTH BLOCK IN D IS 1-BY-1                                          
C                                                                       
            W(J) = -YJ                                                  
            IF (DJ .GE. 0D0) GOTO 2                                     
               W(J) = YJ/DJ                                             
               ALFA = ALFA+W(J)*YJ                                      
               GOTO  4                                                  
   2           IF (DJ .EQ. 0D0) GOTO 3                                  
                  W(J) = (-YJ)/DJ                                       
                  GAMMA = GAMMA+(-YJ)*W(J)                              
   3        CONTINUE                                                    
   4        CONTINUE                                                    
            GOTO  10                                                    
   5        K = 2                                                       
C                                                                       
C THE JTH BLOCK IN D IS 2-BY-2                                          
C                                                                       
            DJP1 = D(J+1)                                               
            YJP1 = Y(J+1)                                               
            DENOM = DJ*DJP1-EJ**2                                       
            W(J) = (DJP1*YJ-EJ*YJP1)/DENOM                              
            W(J+1) = (DJ*YJP1-EJ*YJ)/DENOM                              
            YTDIY = W(J)*YJ+W(J+1)*YJP1                                 
            YTDY = DJ*YJ**2+2.*EJ*YJ*YJP1+DJP1*YJP1**2                  
            IF (YTDIY .GE. 0D0) GOTO 6                                  
               ALFA = ALFA+YTDIY                                        
               GOTO  9                                                  
   6           IF (YTDY .GE. 0D0) GOTO 7                                
                  W(J) = -YJ                                            
                  W(J+1) = -YJP1                                        
                  ALFA = ALFA+YTDY                                      
                  GOTO  8                                               
   7              W(J) = YJP1                                           
                  W(J+1) = -YJ                                          
                  ALFA = YTDIY*DENOM+ALFA                               
   8        CONTINUE                                                    
   9        CONTINUE                                                    
  10     CONTINUE                                                       
         GOTO  1                                                        
  11  IF (POSDEF) GOTO 22                                               
         IF (ALFA .EQ. 0.D0) GOTO 16                                    
            BETA = 1.                                                   
            IF (GAMMA.GT.EPS) BETA=DSQRT(2.D0*EPS-ALFA)/                
     1          DSQRT(GAMMA)                                            
            J = IBEGIN                                                  
            K = 0                                                       
  12           J = J+K                                                  
               IF (J .GT. N) GOTO  15                                   
               IF (E(J) .NE. 0.D0) GOTO 13                              
                  K = 1                                                 
                  IF (D(J) .LE. 0.D0) W(J) = W(J)/BETA                  
                  GOTO  14                                              
  13              K = 2                                                 
                  W(J) = W(J)/BETA                                      
                  W(J+1) = W(J+1)/BETA                                  
  14           CONTINUE                                                 
               GOTO  12                                                 
  15        CONTINUE                                                    
            GOTO  21                                                    
  16        J = IBEGIN                                                  
C                                                                       
C ALPHA IS EQUAL TO 0 SO COMPUTE W DIRECTLY                             
C                                                                       
            K = 0                                                       
  17           J = J+K                                                  
               IF (J .GT. N) GOTO  20                                   
               IF (E(J) .NE. 0.D0) GOTO 18                              
                  K = 1                                                 
                  W(J) = 0.D0                                           
                  IF (D(J) .LE. 0.D0) W(J) = DSIGN(1.D0, -Y(J))         
                  GOTO  19                                              
  18              K = 2                                                 
                  TEMP = (D(J)-D(J+1))/2.                               
                  DELTA = TEMP-DSQRT(TEMP**2+E(J)**2)                   
                  SGN = DSIGN(1.D0, E(J)*(-Y(J))+DELTA*(-Y(J+1)))       
                  W(J) = E(J)*SGN                                       
                  W(J+1) = DELTA*SGN                                    
  19           CONTINUE                                                 
               GOTO  17                                                 
  20        CONTINUE                                                    
  21  CONTINUE                                                          
  22  RETURN                                                            
      END                                                               
      SUBROUTINE DG6TZD(A, Q, N, IT, IP, POSDEF, IA, IQQ, JT, JSIM,     
     1   IAC, D, E, Y, ZL, IE, EPSI)                                    
      INTEGER IA, N, IQQ                                                
      INTEGER IT, IP, JT, JSIM(N), IAC(IA)                              
      LOGICAL POSDEF                                                    
      DOUBLE PRECISION A(IA, N), Q(IQQ, N), D(N), E(N), Y(N), ZL(N, N)  
      INTEGER IB, IC, JC, KE, II, JDEX                                  
      INTEGER IZ, IFIX, IM1, IP1, NM1, I                                
      INTEGER J, K, ITE, ITT, I1, K1                                    
      INTEGER ITP1, NMITM1                                              
      INTEGER IE                                                        
      REAL SNGL                                                         
      DOUBLE PRECISION TEMP, DDOT, DEL, SIGMA, TAU, EPS                 
      DOUBLE PRECISION SUM, DSQRT, EPSI                                 
      EPS=EPSI                                                          
C                                                                       
C THIS SUBROUTINE DETERMINES L, THE LOWER TRIANGULAR FACTOR             
C OF THE LQ FACTORIZATION OF THE FIRST IT ROWS OF A AND Z,              
C A MATRIX WHICH SPANS THE NULL SPACE OF A SUCH THAT Z(TRANSPOSE)QZ     
C IS BLOCK DIAGONAL. THE MATRICES LA AND Z ON OUTPUT WILL BE IN THE     
C ZL ARRAY.  THE VECTOR D WILL CONTAIN THE DIAGONAL OF THE              
C BLOCK DIAGONAL MATRIX AND E THE OFFDIAGONAL ELEMENTS. THE             
C VECTOR Y IS JUST A SCRATCH VECTOR.                                    
C                                                                       
      IZ = N                                                            
C                                                                       
C                                                                       
C SAVE THE DIAGONAL OF Q SO IT WONT BE DESTROYED                        
C                                                                       
      DO  1 I = 1, JT                                                   
         D(I) = Q(I, I)                                                 
   1     CONTINUE                                                       
      POSDEF = .TRUE.                                                   
C                                                                       
      DO  5 I = 1, JT                                                   
         IC = JSIM(I)                                                   
         DO  4 J = 1, I                                                 
            JC = JSIM(J)                                                
            IF (IC .LT. JC) GOTO 2                                      
               Q(I, J) = Q(IC, JC)                                      
               GOTO  3                                                  
   2           Q(I, J) = Q(JC, IC)                                      
   3        CONTINUE                                                    
   4        CONTINUE                                                    
   5     CONTINUE                                                       
C                                                                       
C REDUCE ZL TO TRIANGULAR FORM                                          
C AND APPLY THE TRANSFORMATIONS ON THE RIGHT AND LEFT TO THE            
C Q MATRIX                                                              
C                                                                       
      I = 0                                                             
      IF (IT .EQ. 0) GOTO 18                                            
         ITT = IT                                                       
         DO  17 II = 1, IT                                              
            I = I+1                                                     
            IC = IAC(I)                                                 
            DO  6 J = 1, JT                                             
               JDEX = JSIM(J)                                           
               ZL(I, J) = A(IC, JDEX)                                   
   6           CONTINUE                                                 
C APPLY PREVIOUS TRANSFORMATIONS TO THE NEW ROW                         
            IF (I .LE. 1) GOTO 9                                        
               IM1 = I-1                                                
               DO  8 J = 1, IM1                                         
                  TAU = DDOT(JT-J+1, ZL(J, J), IZ, ZL(I, J), IZ)/Y(J)   
                  DO  7 K = J, JT                                       
                     ZL(I, K) = ZL(I, K)-TAU*ZL(J, K)                   
   7                 CONTINUE                                           
   8              CONTINUE                                              
   9        SIGMA = DDOT(JT-I+1, ZL(I, I), IZ, ZL(I, I), IZ)            
C TEST FOR LINEAR DEPENDENCE                                            
C                                                                       
            IF (SIGMA .GE. EPS) GOTO 10                                 
               ITE = IAC(II)                                            
               IF (II .GT. IE) GOTO 110                                 
                  IE = IE - 1                                           
                  IP = IP - 1                                           
                  ITE = IAC(IP)                                         
 110           IAC(II) = IAC(ITT)                                       
               IAC(ITT) = ITE                                           
               ITT = ITT-1                                              
               I = I-1                                                  
               GOTO  16                                                 
  10        SIGMA = DSQRT(SIGMA)                                        
            IF (ZL(I, I) .LT. 0.D0) SIGMA = -SIGMA                      
            ZL(I, I) = ZL(I, I)+SIGMA                                   
            Y(I) = SIGMA*ZL(I, I)                                       
            IP1 = I+1                                                   
C                                                                       
C                                                                       
C                                                                       
C APPLY THE SAME TRANSFORMATION AS A CONGRUENT TRANSFORMATIONS          
C TO  THE LOWER TRIANGULAR PART OF THE Q MATRIX                         
C                                                                       
            DEL = 0.D0                                                  
            DO  11 J = 1, JT                                            
               TAU = 0.D0                                               
               IF (J .GE. I) TAU = DDOT(J-I+1, ZL(I, I), IZ, Q(J, I),   
     1            IQQ)                                                  
               IF (J .NE. JT) TAU = TAU+DDOT(JT-J, ZL(I, J+1), IZ, Q(J+1
     1            , J), 1)                                              
               E(J) = TAU/Y(I)                                          
               IF (J .GE. I) DEL = DEL+E(J)*ZL(I, J)                    
  11           CONTINUE                                                 
            DEL = DEL/(2.D0*Y(I))                                       
            IM1 = I-1                                                   
            DO  15 J = I, JT                                            
               E(J) = E(J)-DEL*ZL(I, J)                                 
               IF (I .EQ. 1) GOTO 13                                    
                  DO  12 K = 1, IM1                                     
                     Q(J, K) = Q(J, K)-ZL(I, J)*E(K)                    
  12                 CONTINUE                                           
  13           DO  14 K = I, J                                          
                  Q(J, K) = Q(J, K)-ZL(I, J)*E(K)-ZL(I, K)*E(J)         
  14              CONTINUE                                              
  15           CONTINUE                                                 
            IF (I .EQ. N) GOTO  19                                      
  16        CONTINUE                                                    
  17        CONTINUE                                                    
  18  CONTINUE                                                          
  19  IT = I                                                            
C                                                                       
C NOW REDUCE RELEVANT PART OF Q TO BLOCK DIAGONAL FORM                  
C                                                                       
      ITP1 = IT+1                                                       
      IF (IT .EQ. JT) GOTO 30                                           
         CALL DP6MDC(Q, IQQ, JT, E, ITP1)                               
C                                                                       
C APPLY THE TRANSFORMATIONS USED TO REDUCE Q TO THE LAST JT-IT COLUMNS  
C OF THE IDENTITY MATRIX                                                
C                                                                       
         ZL(JT, JT) = 1.D0                                              
         IF (IT .GE. JT-1) GOTO 29                                      
            NM1 = JT-1                                                  
            Y(JT) = JT                                                  
            DO  20 J = ITP1, NM1                                        
               ZL(JT, J) = 0.D0                                         
  20           CONTINUE                                                 
            NMITM1 = JT-IT-1                                            
            DO  28 IB = 1, NMITM1                                       
               I = JT-IB                                                
               ZL(I, I) = 1.D0                                          
               K1 = I+1                                                 
               IF (E(I+1) .LT. 0.D0) K1 = I+2                           
               IP1 = I+1                                                
               Y(I) = I                                                 
               DO  23 J = IP1, JT                                       
                  SUM = 0.D0                                            
                  KE = Y(J)                                             
                  IF (KE .LT. K1) GOTO 22                               
                     DO  21 K = K1, KE                                  
                        SUM = SUM+ZL(K, J)*Q(K, I)                      
  21                    CONTINUE                                        
  22              ZL(I, J) = SUM                                        
  23              CONTINUE                                              
               IM1 = I-1                                                
               IF (I .EQ. ITP1) GOTO 25                                 
                  DO  24 J = ITP1, IM1                                  
                     ZL(I, J) = 0.D0                                    
  24                 CONTINUE                                           
  25           IF (E(I) .LT. 0.D0) GOTO 27                              
                  I1 = I                                                
C                                                                       
C APPLY THE PERMUTATIONS WHICH GO WITH THE ITH TRANSFORMATION           
C                                                                       
                  IF (E(I+1) .LT. 0.D0) I1 = I+1                        
                  K = E(I)                                              
                  DO  26 J = I1, JT                                     
                     IF (IFIX(SNGL(Y(J))) .LT. K) Y(J) = K              
                     TEMP = ZL(I1, J)                                   
                     ZL(I1, J) = ZL(K, J)                               
                     ZL(K, J) = TEMP                                    
  26                 CONTINUE                                           
  27           CONTINUE                                                 
  28           CONTINUE                                                 
  29     CONTINUE                                                       
C                                                                       
C RESTORE THE DIAGONAL OF Q AND PICK OUT D AND E FROM Q                 
C                                                                       
  30  IF (IT .EQ. 0) GOTO 32                                            
         DO  31 I = 1, IT                                               
            Q(I, I) = D(I)                                              
  31        CONTINUE                                                    
  32  IF (IT .EQ. JT) GOTO 36                                           
         DO  35 I = ITP1, JT                                            
            TEMP = D(I)                                                 
            D(I) = Q(I, I)                                              
            Q(I, I) = TEMP                                              
            IF (D(I) .LE. 0.D0) POSDEF = .FALSE.                        
            E(I) = 0.D0                                                 
            IF (I .EQ. JT) GOTO 34                                      
               IF (E(I+1) .GT. 0.D0) GOTO 33                            
                  E(I) = Q(I+1, I)                                      
                  POSDEF = .FALSE.                                      
  33           CONTINUE                                                 
  34        CONTINUE                                                    
  35        CONTINUE                                                    
C                                                                       
C APPLY THE TRANSFORMATION FROM THE REDUCTION OF A TO THE LAST          
C N-IT COLUMNS OF THE ZL MATRIX TO FORM THE FINAL Z MATRIX              
C                                                                       
  36  IF (IT .EQ. 0) GOTO 45                                            
         DO  44 IB = 1, IT                                              
            I = IT+1-IB                                                 
            IP1 = I+1                                                   
            D(I) = (-Y(I))/ZL(I, I)                                     
            ZL(I, I) = 1D0-Y(I)/(D(I)*D(I))                             
            IF (I .EQ. 1) GOTO 38                                       
               IM1 = I-1                                                
               DO  37 J = 1, IM1                                        
                  Q(I, J) = ZL(I, J)                                    
  37              CONTINUE                                              
C                                                                       
C FORM THE ITH COLUMN                                                   
C                                                                       
  38        TAU = 1.D0/D(I)                                             
            DO  39 K = IP1, JT                                          
               ZL(K, I) = TAU*ZL(I, K)                                  
  39           CONTINUE                                                 
            IF (I .EQ. JT) GOTO 43                                      
               DO  41 J = IP1, JT                                       
                  TAU = DDOT(JT-I, ZL(I, IP1), IZ, ZL(IP1, J), 1)/Y(I)  
                  DO  40 K = IP1, JT                                    
                     ZL(K, J) = ZL(K, J)-TAU*ZL(I, K)                   
  40                 CONTINUE                                           
                  Y(J) = TAU                                            
  41              CONTINUE                                              
C                                                                       
C FORM THE ITH ROW                                                      
C                                                                       
               SIGMA = Y(I)/D(I)                                        
               DO  42 J = IP1, JT                                       
                  ZL(I, J) = SIGMA*Y(J)                                 
  42              CONTINUE                                              
  43        CONTINUE                                                    
  44        CONTINUE                                                    
  45  RETURN                                                            
      END                                                               
      SUBROUTINE DQ6ADD(A, IQ, N, IT, POSDEF, IA, Q, IQQ, JT, JSIM,     
     1   IAC, D, E, Y, ZL)                                              
      INTEGER IA, N, IQQ                                                
      INTEGER IQ, IT, JT, JSIM(N), IAC(IA)                              
      LOGICAL POSDEF                                                    
      DOUBLE PRECISION A(IA, N), Q(IQQ, N), D(N), E(N), Y(N), ZL(N, N)  
      INTEGER JDEX, I, J, IAQ, ITP1                                     
      DOUBLE PRECISION ACUM                                             
C                                                                       
C                                                                       
C THIS SUBROUTINE UPDATES L AND Z CONTAINED IN THE ZL MATRIX            
C WHEN ROW IQ OF THE A MATRIX BECOMES THE IT+1ST ROW OF                 
C THAT MATRIX SO THAT NOW THERE ARE IT+1 ACTIVE CONSTRAINTS.            
C NOTE THAT L THE LOWER TRIANGULAR FACTOR OF THE ACTIVE                 
C CONSTRAINT MATRIX WILL GAIN A ROW AND Z,WHICH SPANS THE               
C NULL SPACE OF THE ACTIVE CONSTRAINTS WILL LOSE A COLUMN.              
C                                                                       
      IAQ = IAC(IQ)                                                     
C                                                                       
C APPLY Q TO NEW ROW TO BE ADDED                                        
C                                                                       
      ITP1 = IT+1                                                       
      DO  4 I = 1, JT                                                   
         ACUM = 0.D0                                                    
         DO  1 J = 1, JT                                                
            JDEX = JSIM(J)                                              
            ACUM = ACUM+ZL(J, I)*A(IAQ, JDEX)                           
   1        CONTINUE                                                    
         IF (I .LE. IT) GOTO 2                                          
            Y(I) = ACUM                                                 
            GOTO  3                                                     
   2        Q(ITP1, I) = ACUM                                           
   3     CONTINUE                                                       
   4     CONTINUE                                                       
      IF (IT .LT. JT-1) CALL DU6DAT(IT+2, POSDEF, JT, IT+2, D, E, Y, ZL,
     1   N)                                                             
      D(ITP1) = Y(ITP1)                                                 
      IF (IQ .EQ. ITP1) GOTO 5                                          
         IAC(IQ) = IAC(ITP1)                                            
         IAC(ITP1) = IAQ                                                
   5  IT = ITP1                                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE DQ6DRP(Q, IQQ, IS, IT, IP, N, POSDEF, JT, IAC,         
     1   JSIM, D, E, Y, ZL, IWUNIT)                                     
      INTEGER N, IQQ                                                    
      INTEGER IS, IT, IP, JT, IAC(N), JSIM(N)                           
      LOGICAL POSDEF                                                    
      DOUBLE PRECISION Q(IQQ, N), D(N), E(N), Y(N), ZL(N, N)            
      INTEGER IP1, IP2, I, J, ITE, ISM1                                 
      INTEGER ITM1, ISP1                                                
      INTEGER IWUNIT                                                    
      DOUBLE PRECISION T, V1, V2, U2                                    
C                                                                       
C THIS SUBROUTINE CHANGES THE D,E, AND ZL MATRIX WHEN CONSTRAINT        
C IS IS ROPPED FROM THE SET OF ACTIVE CONSTRAINTS TO MAKE               
C IT-1 ACTIVE CONSTRAINTS                                               
C                                                                       
      ISP1 = IS+1                                                       
      ITM1 = IT-1                                                       
      ITE = IAC(IS)                                                     
C                                                                       
C UPDATES L BY SHOVING UP ROWS                                          
C                                                                       
      IF (IS .EQ. IT) GOTO 7                                            
         ISM1 = IS-1                                                    
         IF (IS .EQ. 1) GOTO 3                                          
            DO  2 J = 1, ISM1                                           
               DO  1 I = ISP1, IT                                       
                  Q(I-1, J) = Q(I, J)                                   
   1              CONTINUE                                              
   2           CONTINUE                                                 
   3     DO  6 I = IS, ITM1                                             
            IP1 = I+1                                                   
            IAC(I) = IAC(IP1)                                           
            CALL DG6TH2(Q(IP1, I), D(IP1), V1, V2, U2, D(I))            
            CALL DA6PH2(JT, ZL(1, I), ZL(1, IP1), 1, V1, V2, U2)        
            IF (I .EQ. ITM1) GOTO 5                                     
               IP2 = I+2                                                
               DO  4 J = IP2, IT                                        
                  T = Q(J, I)+U2*Q(J, IP1)                              
                  Q(J-1, I) = Q(J, I)+T*V1                              
                  Q(J, IP1) = Q(J, IP1)+T*V2                            
   4              CONTINUE                                              
   5        CONTINUE                                                    
   6        CONTINUE                                                    
   7  CALL DM6CON(Q, IQQ, IT, JT, POSDEF, JSIM, D, E, Y, ZL, N, IWUNIT) 
      IAC(IT) = ITE                                                     
      IT = IT-1                                                         
      RETURN                                                            
      END                                                               
        SUBROUTINE DQP2NT(ITER,N,M,X,A,IA,B,F,RELDF,RELDX,JT,JSIM,IT,   
     1  IAC,IDROP,IADD,POSDEF,BU)                                       
        INTEGER ITER,N,IA,JT,IT,IDROP,IADD                              
        INTEGER PU,M                                                    
        INTEGER JSIM(N),IAC(M)                                          
        DOUBLE PRECISION X(N),A(IA,N),B(M),F,RELDF,RELDX,BU(N)          
        LOGICAL POSDEF                                                  
        DOUBLE PRECISION RNEG,R,DNRM2,D1MACH                            
        PU=I1MACH(2)                                                    
        IF (.NOT.POSDEF)WRITE(PU,99)                                    
 99      FORMAT(29H INDEFINITE PROJECTED HESSIAN)                       
        IF (ITER.GT.1) GO TO 9                                          
         WRITE(PU,699)                                                  
 699      FORMAT(24H INITIAL FEASIBLE POINT )                           
         WRITE(PU,599)(X(I),I=1,N)                                      
 599      FORMAT(1H ,5D15.5)                                            
      IF (IT .LE. 0) GOTO 4                                             
         WRITE (PU,  1)                                                 
   1     FORMAT (21HEQUALITY CONSTRAINTS )                              
         DO  3 I = 1, IT                                                
            WRITE (PU,  2) IAC(I)                                       
   2        FORMAT (I10)                                                
   3        CONTINUE                                                    
   4  JTP1 = JT+1                                                       
      IF (JT .GE. N) GOTO 8                                             
         WRITE (PU,  5)                                                 
   5     FORMAT (25H COORDINATES AT BOUNDARY )                          
         DO  7 I = JTP1, N                                              
            WRITE (PU,  6) JSIM(I)                                      
   6        FORMAT (I10)                                                
   7        CONTINUE                                                    
   8     CONTINUE                                                       
       WRITE(PU,199)                                                    
 199    FORMAT(49H ITERATION  F   RELDF     RELDX  DROPPED    ADDED)    
       WRITE(PU,299)                                                    
 299   FORMAT(52H                               CONSTRAINT CONSTRAINT)  
 9     CONTINUE                                                         
        IDR=IABS(IDROP)                                                 
        IAD=IABS(IADD)                                                  
        IF (IDROP)81,499,41                                             
 499    IF (IAD.EQ.0)WRITE(PU,10)ITER,F,RELDF,RELDX                     
 10       FORMAT(I3,1X,3D10.3)                                          
        IF (IADD.GT.0)                                                  
     1   WRITE(PU,20)ITER,F,RELDF,RELDX,IAD                             
 20      FORMAT(I3,1X,3D10.3,6X,I3,1HG)                                 
        IF (IADD.LT.0.AND.X(IAD).EQ.BU(IAD))                            
     1     WRITE(PU,30)ITER,F,RELDF,RELDX,IAD                           
 30      FORMAT(I3,1X,3D10.3,6X,I3,1HU)                                 
        IF (IADD.LT.0.AND.X(IAD).NE.BU(IAD))                            
     1     WRITE(PU,40)ITER,F,RELDF,RELDX,IAD                           
 40      FORMAT(I3,1X,3D10.3,6X,I3,1HL)                                 
         GO TO 161                                                      
 41      CONTINUE                                                       
        IF (IAD.EQ.0)                                                   
     1WRITE(PU,50)ITER,F,RELDF,RELDX,IDR                                
 50       FORMAT(I3,1X,3D10.3,1X,I3,1HG)                                
        IF (IADD.GT.0)                                                  
     1     WRITE(PU,60)ITER,F,RELDF,RELDX,IDR,IAD                       
 60      FORMAT(I3,1X,3D10.3,1X,I3,1HG,1X,I3,1HG)                       
        IF (IADD.LT.0.AND.X(IAD).EQ.BU(IAD))                            
     1     WRITE(PU,70)ITER,F,RELDF,RELDX,IDR,IAD                       
 70      FORMAT(I3,1X,3D10.3,1X,I3,1HG,1X,I3,1HU)                       
        IF (IADD.LT.0.AND.X(IAD).NE.BU(IAD))                            
     1     WRITE(PU,80)ITER,F,RELDF,RELDX,IDR,IAD                       
 80      FORMAT(I3,1X,3D10.3,1X,I3,1HG,1X,I3,1HL)                       
         GO TO 161                                                      
 81      CONTINUE                                                       
         IF (X(IDR).NE.BU(IDR))GO TO 121                                
         IF (IAD.EQ.0)                                                  
     1     WRITE(PU,90)ITER,F,RELDF,RELDX,IDR                           
 90      FORMAT(I3,1X,3D10.3,1X,I3,1HU)                                 
         IF (IADD.GT.0)                                                 
     1     WRITE(PU,100)ITER,F,RELDF,RELDX,IDR,IAD                      
 100       FORMAT(I3,1X,3D10.3,1X,I3,1HU,1X,I3,1HG)                     
         IF (IADD.LT.0.AND.X(IAD).EQ.BU(IAD))                           
     1     WRITE(PU,110)ITER,F,RELDF,RELDX,IDR,IAD                      
 110       FORMAT(I3,1X,3D10.3,1X,I3,1HU,1X,I3,1HU)                     
         IF (IADD.LT.0.AND.X(IAD).LT.BU(IAD))                           
     1     WRITE(PU,120)ITER,F,RELDF,RELDX,IDR,IAD                      
 120       FORMAT(I3,1X,3D10.3,1X,I3,1HU,1X,I3,1HL)                     
         GO TO 161                                                      
 121     CONTINUE                                                       
         IF (IAD.EQ.0)                                                  
     1     WRITE(PU,130)ITER,F,RELDF,RELDX,IDR                          
 130       FORMAT(I3,1X,3D10.3,1X,I3,1HL)                               
         IF (IADD.GT.0)                                                 
     1     WRITE(PU,140)ITER,F,RELDF,RELDX,IDR,IAD                      
 140       FORMAT(I3,1X,3D10.3,1X,I3,1HL,1X,I3,1HG)                     
         IF (IADD.LT.0.AND.X(IAD).EQ.BU(IAD))                           
     1     WRITE(PU,150)ITER,F,RELDF,RELDX,IDR,IAD                      
 150       FORMAT(I3,1X,3D10.3,1X,I3,1HL,1X,I3,1HU)                     
         IF (IADD.LT.0.AND.X(IAD).NE.BU(IAD))                           
     1     WRITE(PU,160)ITER,F,RELDF,RELDX,IDR,IAD                      
 160       FORMAT(I3,1X,3D10.3,1X,I3,1HL,1X,I3,1HL)                     
 161      CONTINUE                                                      
       IF (M.EQ.0)RETURN                                                
       RNEG=0.0D0                                                       
       DO 200 I=1,M                                                     
        R=-(B(I))                                                       
        DO 180 J=1,N                                                    
         R=R+(A(I,J))*(X(J))                                            
 180    CONTINUE                                                        
        IF (R.GE.0.0D0) GO TO 200                                       
        R=R/DNRM2(N,A(I,1),IA)                                          
        IF (R.GT.RNEG) GO TO 200                                        
         RNEG=R                                                         
         IR=I                                                           
 200   CONTINUE                                                         
       IF (RNEG.GT.(-1000.0D0)*D1MACH(4)) RETURN                        
       WRITE(PU,210)IR,RNEG                                             
 210  FORMAT(19H WARNING CONSTRAINT,I3,29H IS MOST VIOLATED CONSTRAINT ,
     122HWITH RELATIVE RESIDUAL,D10.3)                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE DD6SIM(A, IA, N, Q, IQQ, IS, IT, JT, POSDEF,           
     1   JSIM, IAC, D, E, Y, ZL, IWUNIT)                                
      INTEGER IA, N, IQQ                                                
      INTEGER IS, IT, JT, JSIM(N), IAC(N)                               
      LOGICAL POSDEF                                                    
      DOUBLE PRECISION A(IA, N), Q(IQQ, N), D(N), E(N), Y(N), ZL(N, N)  
      INTEGER IDEX, JDEX, I, JTM1, ITP1                                 
      INTEGER IWUNIT                                                    
      DOUBLE PRECISION V1, V2, U2                                       
C                                                                       
C THIS SUBROUTINE FIXES UP THE D,Z,Q,AND L MATRICES                     
C WHEN A SIMPLE CONSTRAINT IS DELETED FROM THE SET OF                   
C ACTIVE CONSTRAINTS                                                    
C                                                                       
      IS = -IS                                                          
      ITP1 = IT+1                                                       
      IDEX = JSIM(IS)                                                   
      JT = JT+1                                                         
      ZL(JT, JT) = 0.D0                                                 
C SINCE THE NEW COLUMN WILL BE THE ITTH POSITION SOME SWAPPING          
C MUST BE DONE                                                          
      D(JT) = D(ITP1)                                                   
      IF (JT .LE. 1) GOTO 2                                             
         JTM1 = JT-1                                                    
         DO  1 I = 1, JTM1                                              
            ZL(I, JT) = ZL(I, ITP1)                                     
            ZL(I, ITP1) = 0.D0                                          
            ZL(JT, I) = 0.D0                                            
   1        CONTINUE                                                    
   2  ZL(JT, ITP1) = 1.D0                                               
      IF (IT .LE. 0) GOTO 5                                             
         DO  3 I = 1, IT                                                
            JDEX = IAC(I)                                               
            Y(I) = A(JDEX, IDEX)                                        
   3        CONTINUE                                                    
         DO  4 I = 1, IT                                                
            CALL DG6TH2(D(I), Y(I), V1, V2, U2, D(I))                   
            CALL DA6PH2(JT, ZL(1, I), ZL(1, ITP1), 1, V1, V2, U2)       
            IF (I .NE. IT) CALL DA6PH2(IT-I, Q(I+1, I), Y(I+1), 1, V1,  
     1         V2, U2)                                                  
   4        CONTINUE                                                    
   5  JSIM(IS) = JSIM(JT)                                               
      JSIM(JT) = IDEX                                                   
      CALL DM6CON(Q, IQQ, ITP1, JT, POSDEF, JSIM, D, E, Y, ZL           
     1   , N, IWUNIT)                                                   
      RETURN                                                            
      END                                                               
       SUBROUTINE DQP1NT(A,M,N,AMAN,IA,B,C,X,CTX,IS,SIMP,ISIMP,IE,      
     1 ITER,IPTG,IAG,IAS,U,IEND,IPH)                                    
C                                                                       
C THIS IS A PRINT ROUTINE                                               
C                                                                       
       INTEGER IA,N                                                     
       DOUBLE PRECISION CTX,A(IA,1),X(N),B(1)                           
       LOGICAL IEND                                                     
       EXTERNAL AMAN                                                    
       INTEGER IPTG(N),ISIMP(1)                                         
       DOUBLE PRECISION SIMP(1),C(1),U(1)                               
       DOUBLE PRECISION TOUTD                                           
       DOUBLE PRECISION EPSI,D1MACH                                     
       IWRITE=I1MACH(2)                                                 
       EPSI=-1000.D0*D1MACH(4)                                          
       IF(ITER.EQ.1)WRITE(IWRITE,20)                                    
 20    FORMAT(1H ,30H TRYING TO FIND FEASIBLE POINT)                    
       IEND = .FALSE.                                                   
       IAGPE=IAG+IE                                                     
       WRITE(IWRITE,1)ITER,IAGPE,IAS                                    
 1     FORMAT(/14H AT ITERATION ,I5                                     
     1  /18H NO.OF ACT. GEN.= ,I5,15H NO.OF ACT.SIM=,I5)                
C      WRITE(IWRITE,2)(X(I),I=1,N)                                      
 2     FORMAT(3H X ,5D15.5)                                             
       DO 10 I=1,M                                                      
          TOUTD=-B(I)                                                   
          DO 8 J=1,N                                                    
             TOUTD=TOUTD+A(I,J)*X(J)                                    
 8       CONTINUE                                                       
          IF (TOUTD.LT.EPSI)WRITE(IWRITE,9)I,TOUTD                      
 9        FORMAT(15H AT CONSTRAINT ,I5,11H RESIDUAL= ,D15.5)            
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
      SUBROUTINE DA6PH2(N, A, B, IDIM, V1, V2, U2)                      
      INTEGER IDIM                                                      
      INTEGER N                                                         
      DOUBLE PRECISION A(IDIM, 1), B(IDIM, 1), V1, V2, U2               
      INTEGER I                                                         
      DOUBLE PRECISION T                                                
C                                                                       
C                                                                       
C THIS SUBROUTINE APPLIES A 2 X 2 HOUSEHOLDER TO THE                    
C TWO COLUMNS A AND B                                                   
C                                                                       
      IF (N .LT. 1) RETURN                                              
      DO  1 I = 1, N                                                    
         T = A(1, I)+U2*B(1, I)                                         
         A(1, I) = A(1, I)+T*V1                                         
         B(1, I) = B(1, I)+T*V2                                         
   1     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DG6TH2(A, B, V1, V2, U2, C)                            
      DOUBLE PRECISION A, B, V1, V2, U2, C                              
      DOUBLE PRECISION RN, SS, DSQRT, U1                                
C                                                                       
C THIS SUBROUTINE GENERATES A HOUSEHOLDER TRANSFORMATIONS               
C WHICH ZEROES THE ELEMENT B                                            
C                                                                       
      SS = DABS(A)+DABS(B)                                              
      U1 = A/SS                                                         
      U2 = B/SS                                                         
      RN = DSQRT(U1*U1+U2*U2)                                           
      IF (U1 .LT. 0.D0) RN = -RN                                        
      V1 = (-(U1+RN))/RN                                                
      V2 = (-U2)/RN                                                     
      U2 = V2/V1                                                        
      C = (-RN)*SS                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE DM6CON(Q, IQQ, IT, JT, POSDEF, JSIM, D, E, Y, ZL,      
     1   N, IWUNIT)                                                     
      INTEGER JT, N, IQQ                                                
      INTEGER IT, JSIM(N)                                               
      LOGICAL POSDEF                                                    
      DOUBLE PRECISION Q(IQQ, JT), D(N), E(N), Y(N), ZL(N, N)           
      INTEGER IDEX, JDEX, I, J                                          
      DOUBLE PRECISION DDOT, A, SUM                                     
      INTEGER IWUNIT                                                    
C                                                                       
C THIS SUBROUTINE MAKES THE ITTH COLUMN OF ZL                           
C CONJUGATE TO THE OTHER COLUMNS                                        
      DO  2 I = 1, JT                                                   
         SUM = 0.D0                                                     
         IDEX = JSIM(I)                                                 
         DO  1 J = 1, JT                                                
            JDEX = JSIM(J)                                              
            A = Q(JDEX, IDEX)                                           
            IF (JDEX .GT. IDEX) A = Q(IDEX, JDEX)                       
            SUM = SUM+A*ZL(J, IT)                                       
   1        CONTINUE                                                    
         E(I) = SUM                                                     
   2     CONTINUE                                                       
C FORM Z(TRANSPOSE)QZ                                                   
      DO  3 I = IT, JT                                                  
         Y(I) = DDOT(JT, ZL(1, I), 1, E(1), 1)                          
   3     CONTINUE                                                       
      D(IT) = Y(IT)                                                     
      POSDEF = .TRUE.                                                   
      DO  4 I = IT, JT                                                  
         E(I) = 0.D0                                                    
   4     CONTINUE                                                       
      IF (IT .GE. JT) GOTO 5                                            
         IF (IT .LT. JT-1) CALL DU6DAT(IT+2, POSDEF, JT, IT+1, D, E, Y  
     1      , ZL, N)                                                    
         E(IT) = Y(IT+1)                                                
         CALL DT6DIA(IT, IT+1, JT, D, E, ZL, N)                         
         IF (E(IT) .NE. 0.D0) POSDEF = .FALSE.                          
   5  IF (D(IT) .LE. 0.D0) POSDEF = .FALSE.                             
      RETURN                                                            
      END                                                               
      SUBROUTINE DU6DAT(IT, POSDEF, N, IT1, D, E, Y, ZL, NN)            
      INTEGER NN, N                                                     
      INTEGER IT, IT1                                                   
      LOGICAL POSDEF                                                    
      DOUBLE PRECISION D(N), E(N), Y(N), ZL(NN, N)                      
      INTEGER I, J, ITM1                                                
      LOGICAL PIV                                                       
      DOUBLE PRECISION TEMP, C, F                                       
      LOGICAL TEMP1                                                     
C                                                                       
C THIS SUBROUTINE IS CALLED BY BOTH DQ6DRP AND DQ6ADD WHEN TRYING       
C TO MAKE THE REPRESENTATION OF THE NULL SPACE CONJUGATE TO             
C THE HESSIAN OF THE FUNCTION TO BE MINIMIZED                           
C                                                                       
      E(N) = 0.D0                                                       
      ITM1 = IT-1                                                       
      I = N                                                             
   1  IF (I .EQ. IT-1) GOTO  15                                         
         IF (I .EQ. IT) GOTO 11                                         
            IF (E(I-2) .EQ. 0.D0) GOTO 10                               
               CALL DE6LIM(N, Y(I-1), Y(I-2), C, I-1, I-2, E(I-2),      
     1            PIV, D, E, ZL, NN)                                    
               CALL DE6LIM(N, Y(I), Y(I-2), C, I, I-2, F, PIV, D, E, ZL,
     1            NN)                                                   
               IF (E(I) .EQ. 0.D0) GOTO 5                               
                  IF (.NOT. PIV) GOTO 3                                 
                     TEMP = D(I+1)                                      
C                                                                       
C**********************                                                 
C                                                                       
                     D(I+1) = D(I-1)                                    
                     D(I-1) = TEMP                                      
                     TEMP = E(I)                                        
                     E(I) = E(I-2)                                      
                     E(I-2) = TEMP                                      
                     DO  2 J = 1, N                                     
                        TEMP = ZL(J, I-1)                               
                        ZL(J, I-1) = ZL(J, I+1)                         
                        ZL(J, I+1) = TEMP                               
   2                    CONTINUE                                        
   3              E(I-1) = (-C)*E(I-2)                                  
                  CALL DE6LIM(N, F, E(I-2), C, I, I-1, E(I-1), PIV, D, E
     1               , ZL, NN)                                          
                  IF (.NOT. PIV) GOTO 4                                 
                     F = E(I)                                           
                     E(I) = (-C)*F                                      
                     CALL DE6LIM(N, F, E(I-1), C, I+1, I, E(I), PIV,    
     1                  D, E, ZL, NN)                                   
   4              CALL DT6DIA(I-2, I+1, N, D, E, ZL, NN)                
                  I = I-2                                               
                  GOTO  9                                               
   5              IF (PIV) GOTO 6                                       
                     E(I-1) = (-C)*E(I-2)                               
                     CALL DE6LIM(N, F, E(I-2), C, I, I-1, E(I-1), PIV, D
     1                  , E, ZL, NN)                                    
                     GOTO  8                                            
   6                 E(I-1) = E(I-2)                                    
                     E(I-2) = F                                         
                     DO  7 J = 1, N                                     
                        TEMP = ZL(J, I)                                 
                        ZL(J, I) = ZL(J, I-1)                           
                        ZL(J, I-1) = TEMP                               
   7                    CONTINUE                                        
                     TEMP = D(I)                                        
                     D(I) = D(I-1)                                      
                     D(I-1) = TEMP                                      
   8              CALL DT6DIA(I-2, I, N, D, E, ZL, NN)                  
                  I = I-2                                               
   9           GOTO  1                                                  
  10        CONTINUE                                                    
  11     IF (E(I) .EQ. 0.D0) GOTO 13                                    
            CALL DE6LIM(N, Y(I), Y(I-1), C, I, I-1, E(I-1), PIV,        
     1         D, E, ZL, NN)                                            
C                                                                       
C**********************                                                 
C                                                                       
            IF (.NOT. PIV) GOTO 12                                      
               F = E(I)                                                 
               E(I) = (-C)*F                                            
               CALL DE6LIM(N, F, E(I-1), C, I+1, I, E(I), PIV, D, E, ZL,
     1            NN)                                                   
  12        CALL DT6DIA(I-1, I+1, N, D, E, ZL, NN)                      
            I = I-1                                                     
            GOTO  14                                                    
  13        CALL DE6LIM(N, Y(I), Y(I-1), C, I, I-1, E(I-1), PIV,        
     1         D, E, ZL, NN)                                            
            IF (I .GT. IT1) CALL DT6DIA(I-1, I, N, D, E, ZL, NN)        
            I = I-1                                                     
  14     CONTINUE                                                       
         GOTO  1                                                        
  15  POSDEF = .TRUE.                                                   
      DO  16 I = IT1, N                                                 
         TEMP1 = D(I) .LE. 0.D0                                         
         IF (.NOT. TEMP1) TEMP1 = E(I) .NE. 0.D0                        
         IF (TEMP1) POSDEF = .FALSE.                                    
  16     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DP6MDC(A, IDIM, N, CHANGE, IT)                         
      INTEGER IDIM, N                                                   
      INTEGER IT                                                        
      DOUBLE PRECISION A(IDIM, N), CHANGE(N)                            
      INTEGER IP1, IP2, JM1, JP1, I, J                                  
      INTEGER K                                                         
      DOUBLE PRECISION LAMBDA, SAVE, TEMP, AIP1I, AII                   
      DOUBLE PRECISION ALPHA, DET, SIGMA, DSQRT, AIP1                   
C                                                                       
C GIVEN A SYMMETRIC INDEFINITE MATRIX OF ORDER N,THIS SUBROUTINE        
C DETERMINES ITS DECOMPOSITION INTO PMDM(TRANSPOSE)P(TRANSPOSE)         
C WHERE P IS A PERMUTATION MATRIX,M IS A UNIT LOWER TRIANGULAR          
C MATRIX, AND D IS A BLOCK DIAGONAL MATRIX WITH BLOCKS OF ORDER         
C 1 OR 2 WHERE D(I+1,I) IS NONZERO WHENEVER M(I+1,I) IS ZERO.           
C ONLY THE LOWER TRIANGULAR PORTION OF A IS USED. THE DECOMPOSITION     
C IS PLACED IN THE LOWER TRIANGULAR PORTION. THUS IF ALL THE ELEMENTS   
C OF A ARE SPECIFIED,THE STRICT UPPER TRIANGLE IS NOT DESTROYED BUT     
C THE DIAGONAL IS DESTROYED.  ON OUTPUT THE VECTOR CHANGE OF            
C LENGTH N WILL CONTAIN A RECORD OF THE PERMUTATIONS GENERATED.  THE    
C INTEGER VARIABLE IDIM GIVES THE ROW DIMENSION O THE A MATRIX.         
C                                                                       
      CHANGE(N) = N                                                     
      ALPHA = (DSQRT(17.D0)+1.D0)/8.D0                                  
      I = IT                                                            
   1  IF (I .GE. N) GOTO  19                                            
         AII = DABS(A(I, I))                                            
         CHANGE(I) = I                                                  
C                                                                       
C FIND THE LARGEST OFF DIAGONAL ELEMENT IN THE ITH COLUMN               
C                                                                       
         J = I+1                                                        
         IP1 = I+1                                                      
         LAMBDA = DABS(A(IP1, I))                                       
         IP2 = I+2                                                      
         IF (IP2 .GT. N) GOTO 4                                         
            DO  3 K = IP2, N                                            
               IF (DABS(A(K, I)) .LE. LAMBDA) GOTO 2                    
                  LAMBDA = DABS(A(K, I))                                
                  J = K                                                 
   2           CONTINUE                                                 
   3           CONTINUE                                                 
   4     TEMP = ALPHA*LAMBDA                                            
         IF (AII .GE. TEMP) GOTO 15                                     
            SIGMA = LAMBDA                                              
C                                                                       
C FIND THE   LARGEST OFFDIAGONAL ELEMENT IN THE JTH COLUMN              
C                                                                       
            JM1 = J-1                                                   
            IF (IP1 .GT. JM1) GOTO 6                                    
               DO  5 K = IP1, JM1                                       
                  IF (DABS(A(J, K)) .GT. SIGMA) SIGMA = DABS(A(J, K))   
   5              CONTINUE                                              
   6        JP1 = J+1                                                   
            IF (JP1 .GT. N) GOTO 8                                      
               DO  7 K = JP1, N                                         
                  IF (DABS(A(K, J)) .GT. SIGMA) SIGMA = DABS(A(K, J))   
   7              CONTINUE                                              
   8        IF (AII*SIGMA .GE. TEMP*LAMBDA) GOTO 14                     
               IF (DABS(A(J, J)) .GE. ALPHA*SIGMA) GOTO 13              
                  CHANGE(I) = J                                         
C                                                                       
C PERFORM A 2 BY 2 PIVOT STEP                                           
C                                                                       
                  IF (J .EQ. IP1) GOTO 9                                
                     CALL DE6CHG(A, IDIM, N, J, IP1)                    
                     TEMP = A(J, I)                                     
                     A(J, I) = A(IP1, I)                                
                     A(IP1, I) = TEMP                                   
   9              DET = A(I, I)*A(IP1, IP1)/A(IP1, I)-A(IP1, I)         
                  AIP1I = A(IP1, I)                                     
                  AII = A(I, I)/AIP1I                                   
                  AIP1 = A(IP1, IP1)                                    
                  IF (IP2 .GT. N) GOTO 12                               
                     DO  11 J = IP2, N                                  
                        TEMP = (A(J, I)-AII*A(J, IP1))/DET              
                        SAVE = (-(AIP1*TEMP+A(J, IP1)))/AIP1I           
                        DO  10 K = J, N                                 
                           A(K, J) = A(K, J)+A(K, I)*SAVE+A(K, IP1)*    
     1                        TEMP                                      
  10                       CONTINUE                                     
                        A(J, I) = SAVE                                  
                        A(J, IP1) = TEMP                                
  11                    CONTINUE                                        
  12              CHANGE(IP1) = -1                                      
                  I = IP2                                               
                  GOTO  1                                               
C                                                                       
C INTERCHANGE THE ITH AND JTH ROWS AND COLUMNS                          
C                                                                       
  13           CHANGE(I) = J                                            
               CALL DE6CHG(A, IDIM, N, J, I)                            
  14        CONTINUE                                                    
C                                                                       
C PERFORM A 1 X 1 PIVOT                                                 
C                                                                       
  15     IF (A(I, I) .EQ. 0.D0) GOTO 18                                 
            AII = A(I, I)                                               
            DO  17 J = IP1, N                                           
               SAVE = (-A(J, I))/AII                                    
               DO  16 K = J, N                                          
                  A(K, J) = A(K, J)+A(K, I)*SAVE                        
  16              CONTINUE                                              
               A(J, I) = SAVE                                           
  17           CONTINUE                                                 
  18     I = IP1                                                        
         GOTO  1                                                        
  19  RETURN                                                            
      END                                                               
      SUBROUTINE DE6LIM(N, A, B, C, I, J, F, PIV, D, E, ZL, NN)         
      INTEGER NN, N                                                     
      INTEGER I, J                                                      
      LOGICAL PIV                                                       
      DOUBLE PRECISION A, B, C, F, D(N), E(N)                           
      DOUBLE PRECISION ZL(NN, N)                                        
      INTEGER K                                                         
      DOUBLE PRECISION H, S                                             
C                                                                       
C                                                                       
C                                                                       
C THIS SUBROUTINE APPLIES ONE STEP OF GAUSSIAN ELIMINATION WITH         
C PARTIAL PIVOTING IN THEI AND JTH PLANES TO ELIMINATE THE              
C VALUE CONTAINED IN A WHICH IS IN THE I TH PLANE USING                 
C B IN THE JTH PLANE. THE TRANSFORMATIONS CONSTRUCTED ARE APPLIED       
C AS CONGRUENCE TRANSFORMATIONS TO A TRIDIAGONAL MATRIX                 
C IN VECTORS D AND E AND AS A COLUMN TRANSFORMATION TO THE ZL MATRIX.   
C                                                                       
      H = E(J)                                                          
      IF (I .GT. J+1) H = 0                                             
      F = H                                                             
      IF (DABS(A) .LE. DABS(B)) GOTO 2                                  
         C = B/A                                                        
         F = H-C*D(I)                                                   
         S = D(I)                                                       
         D(I) = D(J)-C*(H+F)                                            
         D(J) = S                                                       
         DO  1 K = 1, N                                                 
            S = ZL(K, I)                                                
            ZL(K, I) = ZL(K, J)-C*S                                     
            ZL(K, J) = S                                                
   1        CONTINUE                                                    
         B = A                                                          
         PIV = .TRUE.                                                   
         GOTO  5                                                        
   2     C = 0.D0                                                       
         IF (A .EQ. 0.D0) GOTO 4                                        
            C = A/B                                                     
            F = H-C*D(J)                                                
            D(I) = D(I)-C*(H+F)                                         
            DO  3 K = 1, N                                              
               ZL(K, I) = ZL(K, I)-C*ZL(K, J)                           
   3           CONTINUE                                                 
   4     PIV = .FALSE.                                                  
   5  RETURN                                                            
      END                                                               
      SUBROUTINE DT6DIA(J, K, N, D, E, ZL, NN)                          
      INTEGER NN, N                                                     
      INTEGER J, K                                                      
      DOUBLE PRECISION D(N), E(N), ZL(NN, N)                            
      INTEGER I, L                                                      
      DOUBLE PRECISION C, DEN, ALPHA, SIGMA                             
      DOUBLE PRECISION C1, C2                                           
C                                                                       
C THIS SUBROUTINE TAKES A SYMMETRIC TRIDIAGONAL MATRIX WHOSE            
C DIAGONAL ELEMENTS ARE STORED IN ELEMENTS J THROUGH K OF THE           
C VECTOR D AND WHOSE OFF DIAGONAL ELEMENTS ARE STORED IN                
C ELEMENTS J THROUGH K-1 AND REDUCES IT BY CONGRUENCE                   
C TRANSFORMATIONS WITHOUT PIVOTING TO BLOCK DIAGONAL FORM               
C WHERE THE BLOCKS ARE OF ORDER 1 AND 2.  THE RIGHT                     
C TRANSFORMATIONS ARE ALSO APPLIED TO THE CORRESPONDING COLUMNS         
C OF THE Z MATRIX.                                                      
C                                                                       
      I = K                                                             
   1     SIGMA = DMAX1(DABS(E(I-1)), DABS(D(I-1)))                      
         ALPHA = .5                                                     
         IF (I-1 .GT. J) SIGMA = DMAX1(SIGMA, DABS(E(I-2)))             
         IF (SIGMA*DABS(D(I)) .GE. ALPHA*E(I-1)*E(I-1)) GOTO 3          
            IF (I .EQ. J+1) GOTO  7                                     
            DEN = D(I)*D(I-1)/E(I-1)-E(I-1)                             
            C1 = (-D(I))/E(I-1)*E(I-2)/DEN                              
            C2 = (-(C1*D(I-1)+E(I-2)))/E(I-1)                           
            DO  2 L = 1, N                                              
               ZL(L, I-2) = ZL(L, I-2)+C2*ZL(L, I)+C1*ZL(L, I-1)        
   2           CONTINUE                                                 
            D(I-2) = D(I-2)+C1*E(I-2)                                   
            E(I-2) = 0.D0                                               
            I = I-2                                                     
            GOTO  6                                                     
   3        IF (E(I-1) .EQ. 0.D0) GOTO 5                                
               C = E(I-1)/D(I)                                          
               DO  4 L = 1, N                                           
                  ZL(L, I-1) = ZL(L, I-1)-C*ZL(L, I)                    
   4              CONTINUE                                              
               D(I-1) = D(I-1)-C*E(I-1)                                 
               E(I-1) = 0.D0                                            
C                                                                       
   5        I = I-1                                                     
   6     IF (I .EQ. J) GOTO  7                                          
         GOTO  1                                                        
   7  RETURN                                                            
      END                                                               
      SUBROUTINE DE6CHG(A, IDIM, N, J, I)                               
      INTEGER IDIM, N                                                   
      INTEGER J, I                                                      
      DOUBLE PRECISION A(IDIM, N)                                       
      INTEGER JP1, IP1, JM1, K                                          
      DOUBLE PRECISION TEMP                                             
C                                                                       
C THIS SUBROUTINE IS CALLED BY DP6MDC TO INTERCHANGE ROWS AND           
C COLUMNS I AND J OF A SYMMETRIC MATRIX WHEN ONE IS WORKING ONLY        
C WITH THE LOWER TRIANGULAR PORTION OF THAT MATRIX.                     
C                                                                       
C                                                                       
C INTERCHANGE THE ELEMENTS BELOW BOTH DIAGONALS                         
C                                                                       
      JP1 = J+1                                                         
      IF (JP1 .GT. N) GOTO 2                                            
         DO  1 K = JP1, N                                               
            TEMP = A(K, J)                                              
            A(K, J) = A(K, I)                                           
            A(K, I) = TEMP                                              
   1        CONTINUE                                                    
   2  IF (I+1 .GT. J-1) GOTO 4                                          
         IP1 = I+1                                                      
         JM1 = J-1                                                      
         DO  3 K = IP1, JM1                                             
            TEMP = A(K, I)                                              
            A(K, I) = A(J, K)                                           
            A(J, K) = TEMP                                              
   3        CONTINUE                                                    
C                                                                       
C INTERCHANGE THE DIAGONAL ELEMENTS                                     
C                                                                       
   4  TEMP = A(I, I)                                                    
      A(I, I) = A(J, J)                                                 
      A(J, J) = TEMP                                                    
      RETURN                                                            
      END                                                               
       SUBROUTINE LINPR(A,M,N,IA,B,C,X,MAXITR,CTX,S,SIMP,               
     1   ISIMP,E)                                                       
C THIS IS A LINEAR PROGRAMMING PACKAGE FOR MAXIMIZING                   
C THE FUNCTION                                                          
C                    T                                                  
C                   C X                                                 
C SUBJECT TO LINEAR INEQUALITY CONSTRAINTS ON THE VARIABLES             
C AND OPTIONAL UPPPER AND LOWER BOUND CONSTRAINTS ON THE                
C VARIABLES. THE                                                        
C ALGORITHM IS SIMPLEX-LIKE BUT INITIALLY PROJECTED GRADIENT            
C STEPS MAY BE TAKEN-I.E. ONE CAN CUT ACROSS THE FEASIBLE               
C REGION RATHER THAN VISITING VERTICES. THE QR DECOMPOSITION            
C OF THE BASIS MATRIX IS SAVED. NO ATTEMPT IS MADE TO UTILIZE           
C THE SPARSE STRUCTURE OF THE CONSTRAINTS EXCEPT FOR THE LOWER AND      
C UPPER BOUND CONSTRAINTS.                                              
C THE PARAMETERS HAVE THE FOLLOWING INTERPRETATION                      
C                                                                       
C A     THE MATRIX OF CONSTRAINTS WITH LEADING DIMENSION IA             
C M     NUMBER OF GENERAL EQUALITY AND INEQUALITY CONSTRAINTS           
C N     NUMBER OF UNKNOWNS                                              
C IA    ROW DIMENSION OF THE A MATRIX                                   
C B     INPUT VECTOR LENGTH M OF RIGHT HAND SIDE OF GENERAL CONSTRAINTS 
C C     INPUT VECTOR LENGTH N, COST VECTOR                              
C X     REAL VECTOR LENGTH N, ON INPUT APPROXIMATION TO THE SOLUTION,   
C       WHICH NEED NOT BE FEASIBLE, ON OUTPUT-THE SOLUTION              
C MAXITR MAXIMUM NUMBER OF ITERATIONS TOLERATED                         
C CTX   SCALAR ON OUPUT OF THE COST FUNCTION                            
C S     INTEGER INPUT SCALAR OF NUMBER OF SIMPLE CONSTRAINTS            
C SIMP  REAL INPUT VECTOR OF LENGTH S HAVING SIMPLE CONSTRAINTS         
C ISIMP INTEGER INPUT VECTOR TELLING THE ELEMENT OF X THE SIMPLE        
C       CONSTRAINT PERTAINS, IF NEGATIVE IT IS AN UPPER BOUND           
C       IF SIMP(1)=-5, SIMP(2)=10.0, ISIMP(1)=3, ISIMP(2)=-3            
C       THE SOLUTION MUST SATISFY X(3) .GE. -5.0,X(3) .LE. 10.0         
C E     INTEGER INPUT SCALAR GIVING THE NUMBER OF EQUALITY CONSTRAINTS  
C       IT IS ASSUMED THAT THE FIRST E CONSTRAINTS ARE EQUALITY CONS.   
C                                                                       
C STORAGE TAKEN FROM PORT STACK -3N*N/2+4M+11N/2 REAL LOCATIONS         
C      AND 2M+N+S INTEGER LOCATIONS                                     
       INTEGER ISIMP(1), M, E, S, N                                     
       EXTERNAL LPMAN,LPRNT                                             
       INTEGER MAXITR                                                   
       REAL A(IA,N),B(1),C(1),X(N),CTX,SIMP(1)                          
       COMMON /CSTAK/ DSTAK                                             
       DOUBLE PRECISION DSTAK(500)                                      
       INTEGER ISTAK(1000),ISTKGT                                       
       REAL WS(500)                                                     
       EQUIVALENCE (DSTAK(1),ISTAK(1))                                  
       EQUIVALENCE(DSTAK(1),WS(1))                                      
C/6S                                                                    
C      IF (N.LT.1)CALL SETERR(13H LINPR-N.LT.1,13,1,2)                  
C      IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
C    1 20H LINPR-M,S,OR E.LT.0,20,2,2)                                  
C      IF(E.GT.M.OR.E.GT.N.OR.S.GT.2*N)CALL SETERR(                     
C    1 34H LINPR-E.GT.M.OR.E.GT.N.OR.S.GT.2N,34,3,2)                    
C      IF(M+S.LT.N.OR.IA.LT.M)CALL SETERR(                              
C    1 26H LINPR-M+S.LT.N.OR IA.LT.M,26,4,2)                            
C      IF (MAXITR.LT.1)CALL SETERR(                                     
C    1 18H LINPR-MAXITR.LT.1,18,5,2)                                    
C/7S                                                                    
       IF (N.LT.1)CALL SETERR(' LINPR-N.LT.1',13,1,2)                   
       IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
     1 ' LINPR-M,S,OR E.LT.0',20,2,2)                                   
       IF(E.GT.M.OR.S.GT.2*N)CALL SETERR(                               
     1 ' LINPR-E.GT.M.OR.S.GT.2N',24,3,2)                               
       IF(M+S.LT.N.OR.IA.LT.M)CALL SETERR(                              
     1 ' LINPR-M+S.LT.N.OR IA.LT.M',26,4,2)                             
       IF (MAXITR.LT.1)CALL SETERR(                                     
     1 ' LINPR-MAXITR.LT.1',18,5,2)                                     
C/                                                                      
       CALL ENTER(1)                                                    
       IU=ISTKGT(N,3)                                                   
       IPGPTR=ISTKGT(M,2)                                               
       CALL LINPA(A,M,N,LPMAN,IA,B,C,X,MAXITR,CTX,S,SIMP,ISIMP,E,       
     1 LPRNT,IAG,IAS,ISTAK(IPGPTR),WS(IU))                              
      IF (NERROR(IERR).EQ.0)GO TO 10                                    
        CALL ERROFF                                                     
C/6S                                                                    
C     IF (IERR.EQ.6) CALL SETERR(                                       
C    133H LINPR-NO.OF ITER. EXCEEDS MAXITR,33,6,1)                      
C     IF (IERR.EQ.7) CALL SETERR(                                       
C    125H LINPR-UNBOUNDED SOLUTION,25,7,1)                              
C       IF (IERR.EQ.8)CALL SETERR(                                      
C    127H LINPR-NO FEASIBLE SOLUTION,27,8,1)                            
C      IF (IERR.EQ.9)CALL SETERR(                                       
C    127H LINPR-CONDITIONING PROBLEM,27,9,1)                            
C/7S                                                                    
      IF (IERR.EQ.6) CALL SETERR(                                       
     1' LINPR-NO.OF ITER. EXCEEDS MAXITR',33,6,1)                       
      IF (IERR.EQ.7) CALL SETERR(                                       
     1' LINPR-UNBOUNDED SOLUTION',25,7,1)                               
        IF (IERR.EQ.8)CALL SETERR(                                      
     1' LINPR-NO FEASIBLE SOLUTION',27,8,1)                             
       IF (IERR.EQ.9)CALL SETERR(                                       
     1' LINPR-CONDITIONING PROBLEM',27,9,1)                             
C/                                                                      
 10   CALL LEAVE                                                        
       RETURN                                                           
       END                                                              
       SUBROUTINE LINPA(A,M,N,AMAN,IA,B,C,X,MAXITR,CTX,S,SIMP,          
     1   ISIMP,E, PRINT,IAG,IAS,IPTG,U)                                 
C THIS IS A LINEAR PROGRAMMING PACKAGE FOR MAXIMIZING                   
C THE FUNCTION                                                          
C                    T                                                  
C                   C X                                                 
C SUBJECT TO LINEAR INEQUALITY CONSTRAINTS ON THE VARIABLES             
C AND OPTIONAL UPPPER AND LOWER BOUND CONSTRAINTS ON THE                
C VARIABLES. THE USER PROVIDES A FUNCTION WHICH FOR A GIVEN             
C ROW RETURNS EITHER THAT ROW OR THE INNER PRODUCT OF THAT              
C ROW AND A SPECIFIED VECTOR. THUS IF THE CONSTRAINT MATRIX             
C IS SPARSE, THE USER MAY TAKE ADVANTAGE OF THIS. THE                   
C ALGORITHM IS SIMPLEX-LIKE BUT INITIALLY PROJECTED GRADIENT            
C STEPS MAY BE TAKEN-I.E. ONE CAN CUT ACROSS THE FEASIBLE               
C REGION RATHER THAN VISITING VERTICES. THE QR DECOMPOSITION            
C OF THE BASIS MATRIX IS SAVED. NO ATTEMPT IS MADE TO UTILIZE           
C THE SPARSE STRUCTURE OF THE CONSTRAINTS EXCEPT FOR THE LOWER AND      
C UPPER BOUND CONSTRAINTS.                                              
C THE PARAMETERS HAVE THE FOLLOWING INTERPRETATION                      
C                                                                       
C A     A SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION AMAN    
C M     NUMBER OF GENERAL EQUALITY AND INEQUALITY CONSTRAINTS           
C N     NUMBER OF UNKNOWNS                                              
C AMAN  USER PROVIDED FUNCTION WHICH EITHER RETURNS A ROW OF THE MATRIX 
C       OR DOES A VECTOR-VECTOR INNER PRODUCT WITH A PARTICULAR ROW.    
C       AMAN IS INVOKED AS FOLLOWS                                      
C       CALL AMAN(INNER,A,IA,N,IROW,P,T)                                
C       WHERE                                                           
C       INNER    INPUT LOGICAL VARIABLE, IF .TRUE., AMAN                
C                SHOULD RETURN IN T THE INNER PRODUCT OF P AND          
C                THE IROWTH ROW OF THE CONSTRAINT MATRIX                
C       A        REAL VECTOR PASSED THROUGH TO THE SUBROUTINE.          
C                IF THE USER DECIDES TO USE THE DEFAULT SUBROUTINE      
C                LPMAN, A SHOULD CONTAIN THE CONSTRAINT MATRIX          
C                DIMENSIONED A(IA,N) IN THE MAIN PROGRAM.               
C       IA       INTEGER VECTOR PASSED THROUGH TO THE SUBROUTINE.       
C                IF THE USER DECIDES TO USE THE DEFAULT SUBROUTINE      
C                LPMAN, IA IS JUST A SCALAR GIVING THE ROW DIMENSION    
C                OF THE CONSTRAINT MATRIX A                             
C       N        INPUT VARIABLE GIVING LENGTH OF P VECTOR               
C       IROW     THE IROWTH ROW IS EITHER TO BE PUT IN P OR INVOLVED    
C                IN THE INNER PRODUCT DEPENDING ON INNER                
C       P        IF INNER IS .TRUE., THEN P IS AN INPUT VECTOR OF       
C                LENGTH N INVOLVED IN THE INNER PRODUCT. IF INNER       
C                IS .FALSE., ON OUTPUT IT SHOULD CONTAIN THE IROWTH     
C                ROW OF A                                               
C       T        IF INNER IS .TRUE., ON OUTPUT, T=THE INNER             
C                PRODUCT OF P AND THE IROWTH ROW OF A                   
C                IF INNER .FALSE.,T NEED NOT BE TOUCHED                 
C IA    INTEGER SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION   
C       AMAN                                                            
C B     INPUT VECTOR LENGTH M OF RIGHT HAND SIDE OF GENERAL CONSTRAINTS 
C C     INPUT VECTOR LENGTH N, COST VECTOR                              
C X     REAL VECTOR LENGTH N, ON INPUT APPROXIMATION TO THE SOLUTION,   
C       WHICH NEED NOT BE FEASIBLE, ON OUTPUT-THE SOLUTION              
C MAXITR MAXIMUM NUMBER OF ITERATIONS TOLERATED                         
C CTX   SCALAR ON OUPUT OF THE COST FUNCTION                            
C S     INTEGER INPUT SCALAR OF NUMBER OF SIMPLE CONSTRAINTS            
C SIMP  REAL INPUT VECTOR OF LENGTH S HAVING SIMPLE CONSTRAINTS         
C ISIMP INTEGER INPUT VECTOR TELLING THE ELEMENT OF X THE SIMPLE        
C       CONSTRAINT PERTAINS, IF NEGATIVE IT IS AN UPPER BOUND           
C       IF SIMP(1)=-5, SIMP(2)=10.0, ISIMP(1)=3, ISIMP(2)=-3            
C       THE SOLUTION MUST SATISFY X(3) .GE. -5.0,X(3) .LE. 10.0         
C E     INTEGER INPUT SCALAR GIVING THE NUMBER OF EQUALITY CONSTRAINTS  
C       IT IS ASSUMED THAT THE FIRST E CONSTRAINTS ARE EQUALITY CONS.   
C PRINT USER WRITTEN SUBOURINTE WHICH PRINTS INFORMATION ABOUT LP       
C      EACH ITERATION. DEFAULT IS LPRNT WHICH PRINTS NOTHING            
C                                                                       
C STORAGE TAKEN FROM PORT STACK -3N*N/2+4M+11N/2 REAL LOCATIONS         
C      AND 2M+N+S INTEGER LOCATIONS                                     
       INTEGER ISIMP(1), M, E, S, N                                     
       INTEGER TPTR                                                     
       INTEGER IPSPTR,DVSPTR,DVGPTR                                     
       EXTERNAL AMAN,PRINT                                              
       INTEGER MAXITR                                                   
       REAL A(1),B(1),C(1),X(N),CTX,SIMP(1),U(N)                        
       INTEGER IA(1),IPTG(1)                                            
       COMMON /CSTAK/ DSTAK                                             
       DOUBLE PRECISION DSTAK(500)                                      
       INTEGER WPTR,QPTR,LTPTR,PPTR,VPTR,SCLPTR                         
       INTEGER ISTAK(1000),ISTKGT                                       
       REAL WS(500)                                                     
       EQUIVALENCE (DSTAK(1),ISTAK(1))                                  
       EQUIVALENCE(DSTAK(1),WS(1))                                      
C/6S                                                                    
C      IF (N.LT.1)CALL SETERR(13H LINPA-N.LT.1,13,1,2)                  
C      IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
C    1 20H LINPA-M,S,OR E.LT.0,20,2,2)                                  
C      IF(E.GT.M.OR.E.GT.N.OR.S.GT.2*N)CALL SETERR(                     
C    1 34H LINPA-E.GT.M.OR.E.GT.N.OR.S.GT.2N,34,3,2)                    
C      IF(M+S.LT.N)CALL SETERR(                                         
C    1 15H LINPA-M+S.LT.N,15,4,2)                                       
C      IF (MAXITR.LT.1)CALL SETERR(                                     
C    1 18H LINPA-MAXITR.LT.1,18,5,2)                                    
C/7S                                                                    
       IF (N.LT.1)CALL SETERR(' LINPA-N.LT.1',13,1,2)                   
       IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
     1 ' LINPA-M,S,OR E.LT.0',20,2,2)                                   
       IF(E.GT.M.OR.S.GT.2*N)CALL SETERR(                               
     1 ' LINPA-E.GT.M.OR.S.GT.2N',24,3,2)                               
       IF(M+S.LT.N)CALL SETERR(                                         
     1 ' LINPA-M+S.LT.N',15,4,2)                                        
       IF (MAXITR.LT.1)CALL SETERR(                                     
     1 ' LINPA-MAXITR.LT.1',18,5,2)                                     
C/                                                                      
       CALL ENTER(1)                                                    
       WPTR = ISTKGT(M,3)                                               
       QPTR = ISTKGT(N**2,3)                                            
       LTI=(N*(N+1))/2                                                  
       LTPTR = ISTKGT(LTI, 3)                                           
       PPTR =ISTKGT(N, 3)                                               
       VPTR = ISTKGT(M, 3)                                              
       SCLPTR = ISTKGT(M, 3)                                            
       IPSPTR = ISTKGT(N, 2)                                            
       DVSPTR=1                                                         
       IF (S.GT.0)DVSPTR =ISTKGT(S, 2)                                  
       DVGPTR = ISTKGT(M, 2)                                            
       TPTR = ISTKGT(N, 3)                                              
       IRHS = ISTKGT(N,3)                                               
       ICCPTR = ISTKGT(N,3)                                             
       IQ=N                                                             
       CALL I4NTL(N,WS(QPTR),IQ,IPTG,M,ISTAK(IPSPTR),                   
     1 A,IA,AMAN,WS(SCLPTR),WS(TPTR))                                   
       CALL L4PH1(A,M,N,AMAN,IA,B,X,MAXITR,CTX,S,SIMP,ISIMP,E,          
     1   WS(WPTR),WS(QPTR),IQ,WS(LTPTR),WS(PPTR),WS(VPTR),WS(SCLPTR),   
     1   ISTAK(IPSPTR),IPTG,ISTAK(DVSPTR),ISTAK(DVGPTR),                
     1   WS(TPTR),U,PRINT,WS(IRHS),IAS,IAG,KK,WS(ICCPTR),IERR)          
        IF (IERR.EQ.0)GO TO 10                                          
        CALL ERROFF                                                     
C/6S                                                                    
C     IF (IERR.EQ.6) CALL SETERR(                                       
C    133H LINPA-NO.OF ITER. EXCEEDS MAXITR,33,6,1)                      
C     IF (IERR.EQ.7) CALL SETERR(                                       
C    125H LINPA-UNBOUNDED SOLUTION,25,7,1)                              
C       IF (IERR.EQ.8)CALL SETERR(                                      
C    127H LINPA-NO FEASIBLE SOLUTION,27,8,1)                            
C      IF (IERR.EQ.9)CALL SETERR(                                       
C    127H LINPA-CONDITIONING PROBLEM,27,9,1)                            
C/7S                                                                    
      IF (IERR.EQ.6) CALL SETERR(                                       
     1' LINPA-NO.OF ITER. EXCEEDS MAXITR',33,6,1)                       
      IF (IERR.EQ.7) CALL SETERR(                                       
     1' LINPA-UNBOUNDED SOLUTION',25,7,1)                               
        IF (IERR.EQ.8)CALL SETERR(                                      
     1' LINPA-NO FEASIBLE SOLUTION',27,8,1)                             
       IF (IERR.EQ.9)CALL SETERR(                                       
     1' LINPA-CONDITIONING PROBLEM',27,9,1)                             
C/                                                                      
        GO TO 20                                                        
 10     CONTINUE                                                        
       CALL L4P2(A,M,N,AMAN,IA,B,C,X,MAXITR,CTX,S,SIMP,ISIMP,E,         
     1   WS(WPTR),WS(QPTR),IQ,WS(LTPTR),WS(PPTR),WS(VPTR),WS(SCLPTR),   
     1   ISTAK(IPSPTR),IPTG,ISTAK(DVSPTR),ISTAK(DVGPTR),                
     1   WS(TPTR),U,PRINT,WS(IRHS),IAS,IAG,KK,IER2)                     
        IF (IER2.EQ.0)GO TO 20                                          
        CALL ERROFF                                                     
C/6S                                                                    
C     IF (IER2.EQ.6) CALL SETERR(                                       
C    133H LINPA-NO.OF ITER. EXCEEDS MAXITR,33,6,1)                      
C     IF (IER2.EQ.7) CALL SETERR(                                       
C    125H LINPA-UNBOUNDED SOLUTION,25,7,1)                              
C       IF (IER2.EQ.8)CALL SETERR(                                      
C    127H LINPA-NO FEASIBLE SOLUTION,27,8,1)                            
C      IF (IER2.EQ.9)CALL SETERR(                                       
C    127H LINPA-CONDITIONING PROBLEM,27,9,1)                            
C/7S                                                                    
      IF (IER2.EQ.6) CALL SETERR(                                       
     1' LINPA-NO.OF ITER. EXCEEDS MAXITR',33,6,1)                       
      IF (IER2.EQ.7) CALL SETERR(                                       
     1' LINPA-UNBOUNDED SOLUTION',25,7,1)                               
        IF (IER2.EQ.8)CALL SETERR(                                      
     1' LINPA-NO FEASIBLE SOLUTION',27,8,1)                             
       IF (IER2.EQ.9)CALL SETERR(                                       
     1' LINPA-CONDITIONING PROBLEM',27,9,1)                             
C/                                                                      
 20    CALL LEAVE                                                       
       RETURN                                                           
       END                                                              
       SUBROUTINE  FEAS(A,M,N,IA,B,X,MAXITR,S,SIMP,                     
     1   ISIMP,E)                                                       
C THIS SUBROUTINE DETERMINES IF POSSIBLE WHETHER A POINT                
C SATISFIES A SYSTEM OF LINEAR INEQUALITY AND EQUALITY                  
C CONSTRAINTS AND OPTIONAL LOWER AND UPPER BOUND CONSTRAINTS            
C ON THE VARIABLES. THE                                                 
C ALGORITHM IS SIMPLEX-LIKE BUT INITIALLY PROJECTED GRADIENT            
C STEPS MAY BE TAKEN-I.E. ONE CAN CUT ACROSS THE  FEASIBLE              
C REGION RATHER THAN VISITING VERTICES. THE QR DECOMPOSITION            
C OF THE BASIS MATRIX IS SAVED. NO ATTEMPT IS MADE TO UTILIZE           
C THE SPARSE STRUCTURE OF THE CONSTRAINTS EXCEPT FOR THE LOWER AND      
C UPPER BOUND CONSTRAINTS.                                              
C THE PARAMETERS HAVE THE FOLLOWING INTERPRETATION                      
C                                                                       
C A     THE MATRIX OF CONSTRAINTS WITH LEADING DIMENSION IA             
C M     NUMBER OF GENERAL EQUALITY AND INEQUALITY CONSTRAINTS           
C N     NUMBER OF UNKNOWNS                                              
C IA    ROW DIMENSION OF THE A MATRIX                                   
C B     INPUT VECTOR LENGTH M OF RIGHT HAND SIDE OF GENERAL CONSTRAINTS 
C X     REAL VECTOR LENGTH N, ON INPUT APPROXIMATION TO THE SOLUTION,   
C       WHICH NEED NOT BE  FEASIBLE, ON OUTPUT-THE SOLUTION             
C MAXITR MAXIMUM NUMBER OF ITERATIONS TOLERATED                         
C S     INTEGER INPUT SCALAR OF NUMBER OF SIMPLE CONSTRAINTS            
C SIMP  REAL INPUT VECTOR OF LENGTH S HAVING SIMPLE CONSTRAINTS         
C ISIMP INTEGER INPUT VECTOR TELLING THE ELEMENT OF X THE SIMPLE        
C       CONSTRAINT PERTAINS, IF NEGATIVE IT IS AN UPPER BOUND           
C       IF SIMP(1)=-5, SIMP(2)=10.0, ISIMP(1)=3, ISIMP(2)=-3            
C       THE SOLUTION MUST SATISFY X(3) .GE. -5.0,X(3) .LE. 10.0         
C E     INTEGER INPUT SCALAR GIVING THE NUMBER OF EQUALITY CONSTRAINTS  
C       IT IS ASSUMED THAT THE FIRST E CONSTRAINTS ARE EQUALITY CONS.   
C                                                                       
C STORAGE TAKEN FROM PORT STACK -3N*N/2+4M+11N/2 REAL LOCATIONS         
C      AND 2M+N+S INTEGER LOCATIONS                                     
       INTEGER ISIMP(1), M, E, S, N                                     
       EXTERNAL LPMAN,LPRNT                                             
       INTEGER MAXITR                                                   
       REAL A(IA,N),B(1),X(N),SIMP(1)                                   
       COMMON /CSTAK/ DSTAK                                             
       DOUBLE PRECISION DSTAK(500)                                      
       INTEGER ISTAK(1000),ISTKGT                                       
       REAL WS(500)                                                     
       EQUIVALENCE (DSTAK(1),ISTAK(1))                                  
       EQUIVALENCE(DSTAK(1),WS(1))                                      
C/6S                                                                    
C      IF (N.LT.1)CALL SETERR(13H  FEAS-N.LT.1,13,1,2)                  
C      IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
C    1 20H  FEAS-M,S,OR E.LT.0,20,2,2)                                  
C      IF(E.GT.M.OR.S.GT.2*N)CALL SETERR(                               
C    1 24H  FEAS-E.GT.M.OR.S.GT.2N,24,3,2)                              
C      IF (IA.LT.M)CALL SETERR(14H FEAS -IA.LT.M,14,4,2)                
C      IF (MAXITR.LT.1)CALL SETERR(                                     
C    1 18H  FEAS-MAXITR.LT.1,18,5,2)                                    
C/7S                                                                    
       IF (N.LT.1)CALL SETERR('  FEAS-N.LT.1',13,1,2)                   
       IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
     1 '  FEAS-M,S,OR E.LT.0',20,2,2)                                   
       IF(E.GT.M.OR.S.GT.2*N)CALL SETERR(                               
     1 '  FEAS-E.GT.M.OR.S.GT.2N',24,3,2)                               
       IF (IA.LT.M)CALL SETERR(' FEAS -IA.LT.M',14,4,2)                 
       IF (MAXITR.LT.1)CALL SETERR(                                     
     1 '  FEAS-MAXITR.LT.1',18,5,2)                                     
C/                                                                      
       CALL ENTER(1)                                                    
       IPGPTR=ISTKGT(M,2)                                               
       CALL FEASA(A,M,N,LPMAN,IA,B,X,MAXITR,S,SIMP,ISIMP,E,             
     1 LPRNT,IAG,IAS,ISTAK(IPGPTR))                                     
      IF (NERROR(IERR).EQ.0)GO TO 10                                    
        CALL ERROFF                                                     
C/6S                                                                    
C     IF (IERR.EQ.6) CALL SETERR(                                       
C    133H  FEAS-NO.OF ITER. EXCEEDS MAXITR,33,6,1)                      
C     IF (IERR.EQ.7) CALL SETERR(                                       
C    125H  FEAS-UNBOUNDED SOLUTION,25,7,1)                              
C       IF (IERR.EQ.8)CALL SETERR(                                      
C    127H FEAS-NO  FEASIBLE SOLUTION,27,8,1)                            
C      IF (IERR.EQ.9)CALL SETERR(                                       
C    127H  FEAS-CONDITIONING PROBLEM,27,9,1)                            
C/7S                                                                    
      IF (IERR.EQ.6) CALL SETERR(                                       
     1'  FEAS-NO.OF ITER. EXCEEDS MAXITR',33,6,1)                       
      IF (IERR.EQ.7) CALL SETERR(                                       
     1'  FEAS-UNBOUNDED SOLUTION',25,7,1)                               
        IF (IERR.EQ.8)CALL SETERR(                                      
     1' FEAS-NO  FEASIBLE SOLUTION',27,8,1)                             
       IF (IERR.EQ.9)CALL SETERR(                                       
     1'  FEAS-CONDITIONING PROBLEM',27,9,1)                             
C/                                                                      
 10   CALL LEAVE                                                        
       RETURN                                                           
       END                                                              
       SUBROUTINE FEASA(A,M,N,AMAN,IA,B,X,MAXITR,S,SIMP,                
     1   ISIMP,E, PRINT,IAG,IAS,IPTG)                                   
C THIS SUBROUTINE DETERMINES IF POSSIBLE A POINT WHICH SATISFIES        
C A SYSTEM OF LINEAR INEQUALITY AND EQUALITY CONSTRAINTS                
C AND OPTIONAL UPPPER AND LOWER BOUND CONSTRAINTS ON THE                
C VARIABLES. THE USER PROVIDES A FUNCTION WHICH FOR A GIVEN             
C ROW RETURNS EITHER THAT ROW OR THE INNER PRODUCT OF THAT              
C ROW AND A SPECIFIED VECTOR. THUS IF THE CONSTRAINT MATRIX             
C IS SPARSE, THE USER MAY TAKE ADVANTAGE OF THIS. THE                   
C ALGORITHM IS SIMPLEX-LIKE BUT INITIALLY PROJECTED GRADIENT            
C STEPS MAY BE TAKEN-I.E. ONE CAN CUT ACROSS THE FEASIBLE               
C REGION RATHER THAN VISITING VERTICES. THE QR DECOMPOSITION            
C OF THE BASIS MATRIX IS SAVED. NO ATTEMPT IS MADE TO UTILIZE           
C THE SPARSE STRUCTURE OF THE CONSTRAINTS EXCEPT FOR THE LOWER AND      
C UPPER BOUND CONSTRAINTS.                                              
C THE PARAMETERS HAVE THE FOLLOWING INTERPRETATION                      
C                                                                       
C A     A SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION AMAN    
C M     NUMBER OF GENERAL EQUALITY AND INEQUALITY CONSTRAINTS           
C N     NUMBER OF UNKNOWNS                                              
C AMAN  USER PROVIDED FUNCTION WHICH EITHER RETURNS A ROW OF THE MATRIX 
C       OR DOES A VECTOR-VECTOR INNER PRODUCT WITH A PARTICULAR ROW.    
C       AMAN IS INVOKED AS FOLLOWS                                      
C       CALL AMAN(INNER,A,IA,N,IROW,P,T)                                
C       WHERE                                                           
C       INNER    INPUT LOGICAL VARIABLE, IF .TRUE., AMAN                
C                SHOULD RETURN IN T THE INNER PRODUCT OF P AND          
C                THE IROWTH ROW OF THE CONSTRAINT MATRIX                
C       A        REAL VECTOR PASSED THROUGH TO THE SUBROUTINE.          
C                IF THE USER DECIDES TO USE THE DEFAULT SUBROUTINE      
C                LPMAN, A SHOULD CONTAIN THE CONSTRAINT MATRIX          
C                DIMENSIONED A(IA,N) IN THE MAIN PROGRAM.               
C       IA       INTEGER VECTOR PASSED THROUGH TO THE SUBROUTINE.       
C                IF THE USER DECIDES TO USE THE DEFAULT SUBROUTINE      
C                LPMAN, IA IS JUST A SCALAR GIVING THE ROW DIMENSION    
C                OF THE CONSTRAINT MATRIX A                             
C       N        INPUT VARIABLE GIVING LENGTH OF P VECTOR               
C       IROW     THE IROWTH ROW IS EITHER TO BE PUT IN P OR INVOLVED    
C                IN THE INNER PRODUCT DEPENDING ON INNER                
C       P        IF INNER IS .TRUE., THEN P IS AN INPUT VECTOR OF       
C                LENGTH N INVOLVED IN THE INNER PRODUCT. IF INNER       
C                IS .FALSE., ON OUTPUT IT SHOULD CONTAIN THE IROWTH     
C                ROW OF A                                               
C       T        IF INNER IS .TRUE., ON OUTPUT, T=THE INNER             
C                PRODUCT OF P AND THE IROWTH ROW OF A                   
C                IF INNER .FALSE.,T NEED NOT BE TOUCHED                 
C IA    INTEGER SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION   
C       AMAN                                                            
C B     INPUT VECTOR LENGTH M OF RIGHT HAND SIDE OF GENERAL CONSTRAINTS 
C X     REAL VECTOR LENGTH N, ON INPUT APPROXIMATION TO THE SOLUTION,   
C       WHICH NEED NOT BE FEASIBLE, ON OUTPUT-THE SOLUTION              
C MAXITR MAXIMUM NUMBER OF ITERATIONS TOLERATED                         
C S     INTEGER INPUT SCALAR OF NUMBER OF SIMPLE CONSTRAINTS            
C SIMP  REAL INPUT VECTOR OF LENGTH S HAVING SIMPLE CONSTRAINTS         
C ISIMP INTEGER INPUT VECTOR TELLING THE ELEMENT OF X THE SIMPLE        
C       CONSTRAINT PERTAINS, IF NEGATIVE IT IS AN UPPER BOUND           
C       IF SIMP(1)=-5, SIMP(2)=10.0, ISIMP(1)=3, ISIMP(2)=-3            
C       THE SOLUTION MUST SATISFY X(3) .GE. -5.0,X(3) .LE. 10.0         
C E     INTEGER INPUT SCALAR GIVING THE NUMBER OF EQUALITY CONSTRAINTS  
C       IT IS ASSUMED THAT THE FIRST E CONSTRAINTS ARE EQUALITY CONS.   
C PRINT USER WRITTEN SUBOURINTE WHICH PRINTS INFORMATION ABOUT LP       
C      EACH ITERATION. DEFAULT IS LPRNT WHICH PRINTS NOTHING            
C                                                                       
C STORAGE TAKEN FROM PORT STACK -3N*N/2+4M+11N/2 REAL LOCATIONS         
C      AND 2M+N+S INTEGER LOCATIONS                                     
       INTEGER ISIMP(1), M, E, S, N                                     
       INTEGER TPTR                                                     
       INTEGER IPSPTR,DVSPTR,DVGPTR                                     
       EXTERNAL AMAN,PRINT                                              
       INTEGER MAXITR                                                   
       REAL A(1),B(1),X(N),CTX,SIMP(1)                                  
       INTEGER IA(1),IPTG(1)                                            
       COMMON /CSTAK/ DSTAK                                             
       DOUBLE PRECISION DSTAK(500)                                      
       INTEGER WPTR,QPTR,LTPTR,PPTR,VPTR,SCLPTR                         
       INTEGER ISTAK(1000),ISTKGT                                       
       REAL WS(500)                                                     
       EQUIVALENCE (DSTAK(1),ISTAK(1))                                  
       EQUIVALENCE(DSTAK(1),WS(1))                                      
C/6S                                                                    
C      IF (N.LT.1)CALL SETERR(13H FEASA-N.LT.1,13,1,2)                  
C      IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
C    1 20H FEASA-M,S,OR E.LT.0,20,2,2)                                  
C      IF(E.GT.M.OR.S.GT.2*N)CALL SETERR(                               
C    1 24H FEASA-E.GT.M.OR.S.GT.2N,24,3,2)                              
C      IF (MAXITR.LT.1)CALL SETERR(                                     
C    1 18H FEASA-MAXITR.LT.1,18,4,2)                                    
C/7S                                                                    
       IF (N.LT.1)CALL SETERR(' FEASA-N.LT.1',13,1,2)                   
       IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
     1 ' FEASA-M,S,OR E.LT.0',20,2,2)                                   
       IF(E.GT.M.OR.S.GT.2*N)CALL SETERR(                               
     1 ' FEASA-E.GT.M.OR.S.GT.2N',24,3,2)                               
       IF (MAXITR.LT.1)CALL SETERR(                                     
     1 ' FEASA-MAXITR.LT.1',18,4,2)                                     
C/                                                                      
       CALL ENTER(1)                                                    
       WPTR = ISTKGT(M,3)                                               
       IUPTR = ISTKGT(N,3)                                              
       QPTR = ISTKGT(N**2,3)                                            
       LTI=(N*(N+1))/2                                                  
       LTPTR = ISTKGT(LTI, 3)                                           
       PPTR =ISTKGT(N, 3)                                               
       VPTR = ISTKGT(M, 3)                                              
       SCLPTR = ISTKGT(M, 3)                                            
       IPSPTR = ISTKGT(N, 2)                                            
       DVSPTR=1                                                         
       IF (S.GT.0)DVSPTR =ISTKGT(S, 2)                                  
       DVGPTR = ISTKGT(M, 2)                                            
       TPTR = ISTKGT(N, 3)                                              
       IRHS = ISTKGT(N,3)                                               
       ICCPTR = ISTKGT(N,3)                                             
       IQ=N                                                             
       CALL I4NTL(N,WS(QPTR),IQ,IPTG,M,ISTAK(IPSPTR),                   
     1 A,IA,AMAN,WS(SCLPTR),WS(TPTR))                                   
       CALL L4PH1(A,M,N,AMAN,IA,B,X,MAXITR,CTX,S,SIMP,ISIMP,E,          
     1   WS(WPTR),WS(QPTR),IQ,WS(LTPTR),WS(PPTR),WS(VPTR),WS(SCLPTR),   
     1   ISTAK(IPSPTR),IPTG,ISTAK(DVSPTR),ISTAK(DVGPTR),                
     1   WS(TPTR),WS(IUPTR),PRINT,WS(IRHS),IAS,IAG,KK,WS(ICCPTR),IERR)  
        IF (IERR.EQ.0)GO TO 10                                          
        CALL ERROFF                                                     
C/6S                                                                    
C     IF (IERR.EQ.6) CALL SETERR(                                       
C    133H FEASA-NO.OF ITER. EXCEEDS MAXITR,33,6,1)                      
C       IF (IERR.EQ.8)CALL SETERR(                                      
C    127H FEASA-NO FEASIBLE SOLUTION,27,8,1)                            
C      IF (IERR.EQ.9)CALL SETERR(                                       
C    127H FEASA-CONDITIONING PROBLEM,27,9,1)                            
C/7S                                                                    
      IF (IERR.EQ.6) CALL SETERR(                                       
     1' FEASA-NO.OF ITER. EXCEEDS MAXITR',33,6,1)                       
        IF (IERR.EQ.8)CALL SETERR(                                      
     1' FEASA-NO FEASIBLE SOLUTION',27,8,1)                             
       IF (IERR.EQ.9)CALL SETERR(                                       
     1' FEASA-CONDITIONING PROBLEM',27,9,1)                             
C/                                                                      
 10     CONTINUE                                                        
 20    CALL LEAVE                                                       
       RETURN                                                           
       END                                                              
      SUBROUTINE L4P2(A, M, N, AMAN, IA, B, C, X, ITRMAX, CTX, S, SIMP, 
     1  ISIMP, E, W, Q,IQ, LT, P, V, SCALE, IPTS, IPTG, DVECS, DVECG    
     1   ,TT,U1,PRINT,RHS,AS,AG,KK,IER2)                                
C                                                                       
C THIS IS PHASE2 OF THE LINEAR PROGRAMMING PACKAGE                      
C                                                                       
C THE PARAMETERS HAVE THE FOLLOWING INTERPRETATION                      
C                                                                       
C A     A SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION AMAN    
C M     NUMBER OF GNERAL EQUALITY AND INEQUALITY CONSTRAINTS            
C N     NUMBER OF UNKNOWNS                                              
C AMAN  USER PROVIDED FUNCTION WHICH EITHER RETURNS A ROW OF THE MATRIX 
C       OR DOES A VECTOR-VECTOR INNER PRODUCT WITH A PARTICULAR ROW.    
C B     INPUT VECTOR LENGTH M OF RIGHT HAND SIDE OF GNERAL CONSTRAINTS  
C C     INPUT VECTOR LENGTH N, COST VECTOR                              
C X     VECTOR LENGTH N, FEASIBLE VECTOR ON INPUT, SOLUTION ON OUTPUT   
C ITRMAX MAXIMUM NUMBER OF ITERATIONS TOLERATED                         
C CTX   SCALAR ON OUPUT OF THE COST FUNCTION                            
C S     INTEGER INPUT SCALAR OF NUMBER OF SIMPLE CONSTRAINTS            
C SIMP  REAL INPUT VECTOR OF LENGTH S HAVING SIMPLE CONSTRAINTS         
C ISIMP INTEGER INPUT VECTOR TELLING THE ELEMENT OF X THE SIMPLE        
C       CONSTRAINT PERTAINS, IF NEGATIVE IT IS AN UPPER BOUND           
C E     INTEGER INPUT SCALAR GIVING THE NUMBER OF EQUALITY CONSTRAINTS  
C W     M VECTOR OF RESIDUALS                                           
C Q     AN N X N ARRAY STORING THE Q FACTOR OF THE LQ FACTORIZATION     
C       OF THE ACTIVE CONSTRAINT MATRIX                                 
C LT    AN N X N REAL ARRAY STORING THE TRANSPOSE OF L IN THE QL FAC-   
C       TORIZATION                                                      
C P     SCRATCH VECTOR, LENGTH N, WHICH WILL CONTAIN THE SEARCH         
C       DIRECTION                                                       
C V     SCRATCH VECTOR, LENGTH M, WHICH WILL ONTAIN INACTIVE CONSTRAINT 
C       MATRIX TIMES P                                                  
C SCALE INPUT VECTOR, LENGTH M, CONTAINING NORMS OF CONSTRAINT ROWS     
C IPTG SCRATCH VECTOR POINTING TO GENERAL CONSTRAINTS                   
C DVECS SCRATCH VECTOR STATING WHETHER SIMPLE CONSTAINT HAS BEEN        
C       DROPPED-TO PREVENT CYCLING                                      
C DVECG SCRATCH VECTOR STATING WHETHER GENERAL CONSTRAINT HAS           
C       BEEN DROPPED-TO PREVENT CYCLING                                 
C TT    PURE SCRATCH VECTOR                                             
C U1    SCRATCH VECTOR TO STORE LAGRANGE MULTIPLIERS                    
      EXTERNAL AMAN,PRINT                                               
      LOGICAL IEND                                                      
      INTEGER ITRMAX,ISIMP(1)                                           
      REAL A(1), B(1), C(1), X(N), CTX, W(1)                            
       REAL TT(N),RHS(N)                                                
      REAL Q(IQ, N), LT( N), P(N), V(1), SCALE(1)                       
      INTEGER NMS, AGP1,  E, ASP1                                       
      INTEGER I, K, AGPE, FLAG,  IPTG(1)                                
      INTEGER IPTS(N), INDX2, AG, II, AS, DVECG(1)                      
      INTEGER KK, DVECS(1),  ITEMP, ITRPH2, IA(1),S                     
      REAL EPS,  CNRM, TOLL, UMAX,SDOT                                  
      REAL PNRM, SIMP(1), TOLU, XNRM, U1(1)                             
      REAL  SNRM2,  THETA, FLOAT                                        
      REAL CTEMP,  R1MACH                                               
      INTEGER ISAR(1000)                                                
      COMMON /CSTAK/ ISAR                                               
      REAL BOUND                                                        
      BOUND=100*R1MACH(4)*SNRM2(N,C,1)                                  
       CALL ENTER(0)                                                    
       MAXMN=MAX0(M,N)                                                  
       MAXMS=MAX0(M,S)                                                  
       IIHI=ISTKGT(MAXMN, 2)                                            
       INVHIT=1                                                         
       IF(MAXMS.GT.0)INVHIT=ISTKGT(MAXMS, 2)                            
      IER2=0                                                            
      IEND= .FALSE.                                                     
      INDX2=0                                                           
      EPS = R1MACH(4)*AMAX1(FLOAT(N),10.0E0)                            
      TOLL = 1. + R1MACH(4)*10.0E0                                      
      TOLU = 1. - R1MACH(4)*10.0E0                                      
      XNRM = SNRM2(N, X, 1)                                             
      CNRM = SNRM2(N, C, 1)                                             
      IF (M .EQ. 0) GOTO 20                                             
      DO  10 II = 1, M                                                  
         V(II)=0.0E0                                                    
         DVECG(II) = 0                                                  
 10      CONTINUE                                                       
 20      CONTINUE                                                       
      DO  25 I=1,N                                                      
         P(I)=0.0E0                                                     
 25   CONTINUE                                                          
       IF (S .EQ. 0) GOTO 40                                            
       DO 30 II=1,S                                                     
          DVECS(II) = 0                                                 
 30       CONTINUE                                                      
 40    CONTINUE                                                         
            ASP1 = AS + 1                                               
            JDEPS=AS                                                    
            DO 50 II = ASP1,N                                           
               IK = IPTS(II)                                            
               IIAS = II - AS                                           
               RHS(IIAS) = C(IK)                                        
 50         CONTINUE                                                    
            NMS=N-AS                                                    
            CALL M5TOP(IQ,N,Q,1,NMS,1,NMS,RHS,1,RHS)                    
      CTX=SDOT(N,C,1,X,1)                                               
      ITRPH2 = 1                                                        
      JDEPG=AG+E                                                        
         GOTO  70                                                       
 60      ITRPH2 = ITRPH2+1                                              
 70      IF (ITRPH2 .GT. ITRMAX) GOTO  200                              
       MMAG = M - AG - E                                                
           AGPE=AG+E                                                    
         KKK=KK                                                         
C                                                                       
C CALL SUBROUTINE TO TEST IF SIMPLE CONSTRAINTS SHOULD BE               
C ADDED AND THEN ADD THEM AND UPDATE LQ DECOMPOSTION                    
C                                                                       
         NMS=N-AS                                                       
         IF (ITRPH2. EQ .1) GO TO 75                                    
         IF (NMS .NE. 0) CALL A4PPS(A, M, N, IA, KK,S, Q,IQ, LT, AS,    
     1      AG, E, IPTS, DVECS, X, SIMP, ISIMP, TOLL, TOLU, EPS, TT,    
     2      IPRINT,RHS,INDX2,P,2,JDEPS,ISAR(IIHI),ISAR(INVHIT),0)       
C                                                                       
C CALL SUBROUTINE TO TEST IF GENERAL CONSTRAINTS SHOULD BE              
C ADDED AND THEN ADD THEM AND UPDATE LQ DECOMPOSITION                   
C                                                                       
         IF (MMAG .NE. 0) CALL A4PPG(A, M, N, IA, KK, Q,IQ, LT, AG,     
     1      AS,E,IPTG,IPTS,W,SCALE,XNRM,AMAN,EPS,TT,P,IPRINT,RHS,       
     2      INDX2,V, JDEPG,ISAR(IIHI),ISAR(INVHIT),0,DVECG)             
 75         AGPE = AG + E                                               
C COMPUTE LAGRANGE MULTIPLIERS                                          
C                                                                       
            IF (AGPE .EQ. 0) GOTO 80                                    
C                                                                       
C COMPUTE LAGRANGE MULTIPLIERS FOR GENERAL CONSTRAINTS                  
C                                                                       
            ASP1 = AS + 1                                               
          CALL M4TOP(AGPE,LT,RHS,U1)                                    
 80         CONTINUE                                                    
            AGP1=AG+E+1                                                 
C                                                                       
C COMPUTE LAGRANGE MULTIPLIERS FOR SIMPLE CONSTRAINTS                   
C                                                                       
            IF (AS .NE. 0) CALL L4AGS(A,M,N,IA,AMAN,AS,U1,U1(AGP1),C,   
     1           ISIMP,AGPE,IPTS,IPTG,TT)                               
C                                                                       
C DETERMINE WHICH CONSTRAINT TO DROP GIVEN THE LAGRANGE MULTIPLIERS     
C                                                                       
       CALL PRINT(A,M,N,AMAN,IA,B,C,X,CTX,S,SIMP,ISIMP,E,ITRPH2,        
     1 IPTG,AG,AS,U1,IEND,2)                                            
       IF (IEND)GO TO 210                                               
         FLAG=1                                                         
         IF (E .GE. KK) GOTO 130                                        
C                                                                       
 90         CALL D4CLM(U1, AG, AS, E, UMAX, INDX2, FLAG, DVECS, DVECG,  
     1         IPTG, IPTS,BOUND,2,KK,N)                                 
              AGPE=AG+E                                                 
            IF (FLAG .NE. 1) GOTO 100                                   
            IF (KK .EQ. N) GO TO 210                                    
            INDX2=0                                                     
            GOTO 130                                                    
 100   CONTINUE                                                         
       IF (INDX2 .LE. AGPE) GOTO 110                                    
C                                                                       
C SINCE A SIMPLE CONSTRAINT IS TO BE DROPPED, UPDATE APPROPRIATE        
C ARRAYS AND THE LQ DECOMPOSITION ACCORDINGLY                           
C                                                                       
          IND2=INDX2-AGPE                                               
          ITEMP=IPTS(IND2)                                              
           NMS=N-AS                                                     
          RHS(NMS+1)=C(ITEMP)                                           
          IF (AS .NE. 0) CALL D4RPS(A,M,N,IA,AMAN,KK,Q,IQ,LT,AG,AS,E,   
     1   IPTG,IPTS,DVECS,SIMP,ISIMP,INDX2,TT,P, RHS)                    
          INDX2=1                                                       
          GOTO 120                                                      
 110     CONTINUE                                                       
C                                                                       
C SINCE A GENERAL CONSTRAINT IS TO BE DROPPED, UPDATE APPROPRIATE       
C ARRAYS AND LQ DECOMPOSITION ACCORDINGLY                               
C                                                                       
       IF (AG .NE. 0) CALL D4RPG (M,N,KK,Q,IQ,LT,AG,AS,E,IPTG,INDX2     
     1                ,DVECG,RHS,C,IPTS)                                
       INDX2=-1                                                         
 120   CONTINUE                                                         
       JDEPG=AG+E                                                       
       JDEPS=AS                                                         
 130   CONTINUE                                                         
C                                                                       
C COMPUTE NEW SEARCH DIRECTION                                          
C                                                                       
         CALL P4RJD(N, AS, AG, E, Q,IQ, IPTS, C, P, TT,RHS)             
         IF (JDEPS.EQ.AS) GO TO 132                                     
         ASP1=AS+1                                                      
         DO 131 I=ASP1,JDEPS                                            
            I2=ISIMP(I)                                                 
 131      P(I2)=0.0E0                                                   
 132      CONTINUE                                                      
         PNRM = SNRM2(N, P, 1)                                          
C                                                                       
C CHECK IF FINISHED                                                     
C                                                                       
         IF (KK.EQ.N)GO TO 210                                          
         IF (PNRM.LT.EPS*CNRM.AND.FLAG.NE.1)GO TO 80                    
         IF (PNRM.LT.EPS*CNRM) GO TO 210                                
C                                                                       
C DETERMINE HOW FAR ONE SHOULD PROCEED IN THE SEARCH DIRECTION          
C                                                                       
           AGPE=AG+E                                                    
           CALL C4NST(A,M,N,IA,S,P,V,W,JDEPG,JDEPS,AS,PNRM,IPTS         
     1  ,IPTG,SIMP,ISIMP, AMAN,EPS,SCALE,X,THETA,TT,IHIT,IERC,          
     1   FLAG)                                                          
         IF (IERC.NE.0) GO TO 190                                       
         CTEMP=SDOT(N,C,1,P,1)                                          
         IF (0.0E0 .LE. CTEMP) GOTO 140                                 
            IER2=9                                                      
            GO TO 210                                                   
 140        CTX = CTX+THETA*CTEMP                                       
            DO  150 K = 1, N                                            
               X(K) = X(K)+THETA*P(K)                                   
 150           CONTINUE                                                 
            IF (IHIT.LE.M) GO TO 160                                    
                I1=IHIT-M                                               
                I2=IABS(ISIMP(I1))                                      
                X(I2)=SIMP(I1)                                          
 160      CONTINUE                                                      
            AGP1 = AG+E+1                                               
            IF(M.LT.AGP1) GO TO 180                                     
C                                                                       
C UPDATE RESIDUALS                                                      
C                                                                       
            DO  170 I = AGP1, M                                         
               K = IPTG(I)                                              
               W(K) = W(K)-THETA*V(K)                                   
               IF (W(K).GT.0.0E0)W(K)=0.0E0                             
 170           CONTINUE                                                 
 180        CONTINUE                                                    
            IF (IHIT.LE.M)W(IHIT)=0.0E0                                 
         GOTO  60                                                       
 190  IER2=7                                                            
      GO TO 210                                                         
 200  IER2=6                                                            
C                                                                       
C RECOMPUTE DUAL VARIABLS                                               
C                                                                       
 210    CONTINUE                                                        
        AGPE=AG+E                                                       
        IF (AGPE.GT.0)CALL M4TOP(AGPE,LT,RHS,U1)                        
        IF (AS.NE.0)CALL L4AGS(A,M,N,IA,AMAN,AS,U1,U1(AGPE+1),          
     1  C,ISIMP,AGPE,IPTS,IPTG,TT)                                      
        CALL LEAVE                                                      
        RETURN                                                          
      END                                                               
      SUBROUTINE L4PH1(A,M, N, AMAN, IA, B,  X, ITRMAX, CTX, S, SIMP,   
     1  ISIMP, E, W, Q,IQ, LT, P, V, SCALE, IPTS, IPTG, DVECS, DVECG    
     1   ,TT,U1,PRINT, RHS,AS,AG,KK,CC,IERR)                            
C THIS IS PHASE2 OF THE LINEAR PROGRAMMING PACKAGE                      
C                                                                       
C THE PARAMTERS HAVE THE FOLLOWING INTERPRETATIONS                      
C A  SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION AMAN.        
C M  NUMBER OF GENERAL EQUALITY AND INEQUALITY CONSTRAINTS              
C N  NUMBER OF UNKNOWNS                                                 
C AMAN - USER PROVIDED FUNCTION WHICH EITHER RETURNS A ROW              
C    OF THE CONSTRAINT MATRIX OR DOES A MATRIX-VECTOR                   
C    INNER PRODUCT                                                      
C IA  INTEGER SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION     
C     AMAN                                                              
C B   RIGHT HAND SIDE CONSTRAINT VECTOR                                 
C X   FEASIBLE VECTOR ON INPUT, SOLUTION ON OUTPUT                      
C ITRMAX   MAXIMUM NUMBER OF ITERATIONS TOLERATED.                      
C CTX ON OUTPUT THE COST FUNCTION TO BE MAXIMIZED                       
C S   NUMBER OF SIMPLE CONSTRAINTS                                      
C SIMP VECTOR GIVING LOWER OR UPPERBOUND                                
C ISIMP VECTOR TELLING ON WHICH ELEMENT THE SIMPLE CONSTRAINT           
C     PERTAINS. IF NEGATIVE IT IS AN UPPER BOUND                        
C E   NUMBER OF EQUALITY CONSTRAINTS                                    
C W   M VECTOR OF RESIDUALS                                             
C Q   AN N XN ARRAY STORING Q FACTOR OF LQ FACTORIZATION OF ACTIVE      
C     CONSTRAINT MATRIX                                                 
C LT  AN N X N ARRAY STORYING THE TRANPOSE OF L IN QL FACTORIZATION     
C P   SCRATCH VECTOR WHICH WILL CONTAIN SEARCH DIRECTION                
C V   SCRATCH VECTOR LENGTH M WHICH WILL CONTAIN INACTIVE CONSTRAINT    
C     MATRIX TIMES P                                                    
C SCALE SCRATCH VECTOR WHICH WILL CONTAIN NORM OF CONSTRAINT ROWS       
C IPTS SCRATCH VECTOR GIVING PERMUTATION OF SIMPLE CONSTRAINTS          
C IPTG SCRATCH VECTOR POINTING TO GENERAL CONSTRAINTS                   
C DVECS SCRATCH VECTOR STATING WHETHER SIMPLE CONSTAINT HAS BEEN        
C       DROPPED-TO PREVENT CYCLING                                      
C DVECG SCRATCH VECTOR STATING WHETHER GENERAL CONSTRAINT HAS           
C       BEEN DROPPED-TO PREVENT CYCLING                                 
C TT    PURE SCRATCH VECTOR                                             
C U1    SCRATCH VECTOR TO STORE LAGRANGE MULTIPLIERS                    
C PRINT   - USER WRITTEN SUBROUTINE WHICH  PRINTS STUFF EACH ITER.      
      INTEGER M, N, S, ISIMP(1),IA(1)                                   
      EXTERNAL AMAN,PRINT                                               
      LOGICAL ISTOP,FIRST                                               
      INTEGER ITRMAX                                                    
      REAL A(1), B(1),  X(N), CTX, W(1), ET                             
       REAL TT(N),RHS(N)                                                
      REAL Q(IQ, N), LT( N), P(N), V(1), SCALE(1)                       
      INTEGER  NMS,  AGP1,  E                                           
      INTEGER I, K, AGPE, FLAG, IPTG(1)                                 
      INTEGER IPTS(N), INDX2, AG, II, AS, DVECG(1)                      
      INTEGER KK, DVECS(1),   ITRPH1                                    
      LOGICAL DONE                                                      
      REAL EPS,  CNRM, TEMP, TOLL, SDOT,C                               
      REAL PNRM, SIMP(1), TOLU, XNRM, U1(1),  CC(1)                     
      REAL  SNRM2,  THETA, FLOAT                                        
      REAL CTEMP,  R1MACH, UMAX,BOUND, BIGBND                           
      INTEGER ISMA(1000)                                                
      COMMON /CSTAK/ISMA                                                
      CALL ENTER(0)                                                     
      MAXMN=MAX0(M,N)                                                   
      MAXSM=MAX0(S,M)                                                   
      IIHIT=ISTKGT(MAXMN, 2)                                            
      INVHIT=1                                                          
      IF(MAXSM.NE.0) INVHIT=ISTKGT(MAXSM, 2)                            
      EPS = FLOAT(N)*R1MACH(4)*10.0E0                                   
      BIGBND=R1MACH(2)*(1.0E0-EPS)                                      
      BOUND=EPS*100.0                                                   
      IHIT=0                                                            
      TOLL = 1. + R1MACH(4)                                             
       ISTOP=.FALSE.                                                    
      TOLU = 1. - R1MACH(4)                                             
      XNRM = SNRM2(N, X, 1)                                             
      JDEPS=0                                                           
      JDEPG=E                                                           
      IF (M .EQ. 0) GOTO 20                                             
      DO  10 II = 1, M                                                  
         V(II)=0.0E0                                                    
         DVECG(II) = 0                                                  
 10      CONTINUE                                                       
 20      CONTINUE                                                       
      DO 25 I=1,N                                                       
 25      P(I)=0.0E0                                                     
       IF (S .EQ. 0) GOTO 40                                            
       DO 30 II=1,S                                                     
           IF (ISIMP(I).LT.0.AND.SIMP(I).GT.BIGBND)SIMP(I)=BIGBND       
           IF (ISIMP(I).GT.0.AND.SIMP(I).LT.-BIGBND)SIMP(I)=-BIGBND     
          DVECS(II) = 0                                                 
 30       CONTINUE                                                      
 40    CONTINUE                                                         
      AS = 0                                                            
      AG = 0                                                            
      KK=E                                                              
      IERR=0                                                            
      INDX2=0                                                           
C                                                                       
C HANDLE EQUALITY CONSTRAINTS                                           
C                                                                       
       IF (E.GT.0)                                                      
     1CALL E4QL(A,N,AMAN,IA,E,Q,IQ,TT,B,X,RHS,V,LT)                     
C                                                                       
C CREATE RESIDUAL VECTOR                                                
C                                                                       
       IF (M.EQ.0) GO TO 60                                             
       DO 50 I=1,M                                                      
          CALL AMAN(.TRUE.,A,IA,N,I,X,TEMP)                             
          W(I)=B(I)-TEMP                                                
 50    CONTINUE                                                         
 60    CONTINUE                                                         
      ITRPH1 = 1                                                        
         GOTO  80                                                       
 70      ITRPH1 = ITRPH1+1                                              
 80      IF (ITRPH1 .GT. ITRMAX) GOTO  210                              
       FIRST=.TRUE.                                                     
       MMAG = M - AG - E                                                
C                                                                       
C GET NEW GRADIENT                                                      
C                                                                       
           AGPE=AG+E                                                    
       IST=AGPE                                                         
       IF (JDEPS.EQ.AS) GO TO 92                                        
       JDEPS1=JDEPS+1                                                   
       DO 91 I=JDEPS1,AS                                                
          II=ISIMP(I)                                                   
          X(II)=SIMP(I)                                                 
91     CONTINUE                                                         
92     IST=JDEPG                                                        
 90    CONTINUE                                                         
       CALL G4ETC(N,M,S,AS,AGPE,A,IA,AMAN,X,CC,RHS,                     
     1     W,SCALE,Q,IQ,IPTS,ISIMP,SIMP,IPTG,DONE,CTX,                  
     2     CNRM,IST,FIRST)                                              
       IF (DONE.AND.FIRST)GOTO 233                                      
       IF (DONE.AND..NOT.FIRST)GO TO 230                                
       IF (ITRPH1.EQ.1) GO TO 95                                        
C                                                                       
C CALL SUBROUTINE TO TEST IF SIMPLE CONSTRAINTS SHOULD BE               
C ADDED AND THEN ADD THEM AND UPDATE LQ DECOMPOSTION                    
C                                                                       
         NMS=N-AS                                                       
          ISTRIK=IHIT-M                                                 
         IF (NMS .NE. 0) CALL A4PPS(A, M, N, IA, KK,S, Q,IQ, LT, AS,    
     1      AG, E, IPTS, DVECS, X, SIMP, ISIMP, TOLL, TOLU, EPS, TT,    
     2    IPRINT,RHS,INDX2,P,1, JDEPS,ISMA(IIHIT),ISMA(INVHIT),ISTRIK)  
C                                                                       
C CALL SUBROUTINE TO TEST IF GENERAL CONSTRAINTS SHOULD BE              
C ADDED AND THEN ADD THEM AND UPDATE LQ DECOMPOSITION                   
C                                                                       
         IF (MMAG .NE. 0) CALL A4PPG(A, M, N, IA, KK, Q,IQ, LT, AG,     
     1      AS,E,IPTG,IPTS,W,SCALE,XNRM,AMAN,EPS,TT,P,IPRINT,RHS,       
     2      INDX2,V,JDEPG,ISMA(IIHIT),ISMA(INVHIT),IHIT,DVECG)          
 95         AGPE=AG+E                                                   
         FLAG=1                                                         
       IF (ISTOP)GOTO 233                                               
C                                                                       
C DETERMINE WHICH CONSTRAINT TO DROP                                    
C AND DROP THEM                                                         
C                                                                       
C COMPUTE LAGRANGE MULTIPLIERS                                          
C                                                                       
            IF (AGPE .EQ. 0) GOTO 100                                   
C                                                                       
C COMPUTE LAGRANGE MULTIPLIERS FOR GENERAL CONSTRAINTS                  
C                                                                       
            ASP1 = AS + 1                                               
          CALL M4TOP(AGPE,LT,RHS,U1)                                    
 100        CONTINUE                                                    
            AGP1=AG+E+1                                                 
C                                                                       
C COMPUTE LAGRANGE MULTIPLIERS FOR SIMPLE CONSTRAINTS                   
C                                                                       
            IF (AS .NE. 0) CALL L4AGS(A,M,N,IA,AMAN,AS,U1,U1(AGP1),CC,  
     1           ISIMP,AGPE,IPTS,IPTG,TT)                               
C                                                                       
C DETERMINE WHICH CONSTRAINT TO DROP GIVEN THE LAGRANGE MULTIPLIERS     
C                                                                       
       CALL PRINT(A,M,N,AMAN,IA,B,C,X,CTX,S,SIMP,ISIMP,E,ITRPH1,        
     1 IPTG,AG,AS,U1,ISTOP,1)                                           
       IF (ISTOP)GOTO 231                                               
         IF (E .GE. KK) GOTO 150                                        
C                                                                       
 110        CALL D4CLM(U1, AG, AS, E, UMAX, INDX2, FLAG, DVECS, DVECG,  
     1         IPTG, IPTS, BOUND,1,KK,N)                                
            IF (FLAG .NE. 1) GOTO 120                                   
            INDX2=0                                                     
            GOTO 150                                                    
 120   IF(INDX2.LE.AGPE) GO TO 130                                      
C                                                                       
C SINCE A SIMPLE CONSTRAINT IS TO BE DROPPED, UPDATE APPROPRIATE        
C ARRAYS AND THE LQ DECOMPOSITION ACCORDINGLY                           
C                                                                       
          IND2=INDX2-AGPE                                               
          ITEMP=IPTS(IND2)                                              
          NMS=N-AS                                                      
          RHS(NMS+1)=CC(ITEMP)                                          
          IF (AS .NE. 0) CALL D4RPS(A,M,N,IA,AMAN,KK,Q,IQ,LT,AG,AS,E,   
     1   IPTG,IPTS,DVECS,SIMP,ISIMP,INDX2,TT,P, RHS)                    
          INDX2=1                                                       
          GOTO 140                                                      
 130      CONTINUE                                                      
C                                                                       
C SINCE A GENERAL CONSTRAINT IS TO BE DROPPED, UPDATE APPROPRIATE       
C ARRAYS AND LQ DECOMPOSITION ACCORDINGLY                               
C                                                                       
       IF (AG .NE. 0) CALL D4RPG (M,N,KK,Q,IQ,LT,AG,AS,E,IPTG,INDX2     
     1                ,DVECG,RHS,CC,IPTS)                               
       INDX2=-1                                                         
 140   CONTINUE                                                         
       JDEPS=AS                                                         
       JDEPG=AG+E                                                       
 150   CONTINUE                                                         
C COMPUTE NEW SEARCH DIRECTION                                          
C                                                                       
         CALL P4RJD(N, AS, AG, E, Q,IQ, IPTS, CC, P, TT,RHS)            
         IF(JDEPS.EQ.AS) GO TO 132                                      
         ASP1=AS+1                                                      
         DO 131 I=ASP1,JDEPS                                            
            I2=ISIMP(I)                                                 
 131     P(I2)=0.0E0                                                    
 132      CONTINUE                                                      
         PNRM = SNRM2(N, P, 1)                                          
C                                                                       
C CHECK IF FINISHED                                                     
C                                                                       
         IF (KK .EQ. N ) GO TO 230                                      
         IF (ITRPH1.EQ.1)                                               
     1 CALL PRINT(A,M,N,AMAN,IA,B,C,X,CTX,S,SIMP,ISIMP,E,ITRPH1,        
     1 IPTG,AG,AS,U1,ISTOP,1)                                           
          IF (PNRM.LT.EPS.AND.FLAG.NE.1) GO TO 95                       
         IF (PNRM.LT.EPS.AND..NOT.FIRST)GO TO 230                       
          FIRST=.FALSE.                                                 
         IF (PNRM.LT.EPS) GO TO 90                                      
C                                                                       
C DETERMINE HOW FAR ONE SHOULD PROCEED IN THE SEARCH DIRECTION          
C                                                                       
           AGPE=AG+E                                                    
           CALL C4ONS(A,M,N,IA,S,P,V,W,JDEPG,JDEPS,AS,PNRM,IPTS         
     1  ,IPTG,SIMP,ISIMP, AMAN,EPS,SCALE,X,THETA,TT,IHIT,IERR)          
         CTEMP=SDOT(N,CC,1,P,1)                                         
         IF (0.0E0 .LE. CTEMP) GOTO 160                                 
            IERR=9                                                      
            GOTO 231                                                    
 160        CTX = CTX+THETA*CTEMP                                       
            DO  170 K = 1, N                                            
               X(K) = X(K)+THETA*P(K)                                   
 170           CONTINUE                                                 
            IF (IHIT.LE.M) GO TO 180                                    
                I1=IHIT-M                                               
                I2=IABS(ISIMP(I1))                                      
                X(I2)=SIMP(I1)                                          
 180      CONTINUE                                                      
            AGP1 = AG+E+1                                               
            IF(M.LT.AGP1) GO TO 200                                     
C                                                                       
C UPDATE RESIDUALS                                                      
C                                                                       
            DO  190 I = AGP1, M                                         
               K = IPTG(I)                                              
               W(K) = W(K)-THETA*V(K)                                   
 190           CONTINUE                                                 
 200        CONTINUE                                                    
            IF (IHIT.LE.M.AND.IHIT.GT.0)W(IHIT)=0.0E0                   
         GOTO  70                                                       
 210   IERR=6                                                           
 220  GOTO231                                                           
 230  IERR=8                                                            
 231   CALL LEAVE                                                       
      RETURN                                                            
 233   CONTINUE                                                         
       CALL LEAVE                                                       
      IF(S.EQ.0)RETURN                                                  
      DO 234 I=1,S                                                      
         ET=1.0                                                         
         IF(ISIMP(I).LT.0)ET=-1.0E0                                     
         I1=IABS(ISIMP(I))                                              
         IF (ET*X(I1).LT.ET*SIMP(I))X(I1)=SIMP(I)                       
234   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE LPMAN(INNERP, A, M, N, K, X, SK)                       
      INTEGER M, N                                                      
      INTEGER K                                                         
      REAL A(M, N), X(N), SK                                            
      LOGICAL INNERP                                                    
      INTEGER I                                                         
      REAL H                                                            
C   THIS PROCEDURE HANDLES THE MATRIX A, WHICH IS STORED IN             
C   THE CONVENTIONAL FASHION.  IF THE MATRIX IS LARGE AND SPARSE        
C   THE USER SHOULD REPLACE THIS ROUTINE WITH A MORE APPROPRIATE        
C   ROUTINE.                                                            
C   INPUT                                                               
C   INNERP - IF TRUE THEN COMPUTE THE INNER PRODUCT OF ROW K WITH X.    
C            OTHERWISE RETURN ROW K OF A IN THE VECTOR X.               
C   A      - THE MATRIX A.                                              
C   M,N    - THE ROW AND COLUMN DIMENSIONS OF A.                        
C   K      - THE INDEX OF THE ROW OF A WHICH IS TO BE PROCESSED.        
C   X      - IF INNERP IS TRUE, THEN X IS A VECTOR WHOSE INNER          
C            PRODUCT WITH ROW K OF A IS TO BE COMPUTED.                 
C   OUTPUT                                                              
C   X      - IF INNERP IS FALSE, THEN X CONTAINS ROW K OF A.            
C   SK     - IF INNERP IS TRUE, THEN SK CONTAINS THE INNER              
C            PRODUCT OF X AND ROW K OF A.                               
C/6S                                                                    
C     IF (M .LE. 0 .OR. N .LE. 0) CALL SETERR(                          
C    1   24HDLPA - INVALID DIMENSION, 25, 1, 2)                         
C     IF (K .LT. 1 .OR. M .LT. K) CALL SETERR(20HDLPA - INVALID INDEX,  
C    1   21, 2, 2)                                                      
C/7S                                                                    
      IF (M .LE. 0 .OR. N .LE. 0) CALL SETERR(                          
     1   'DLPA - INVALID DIMENSION', 25, 1, 2)                          
      IF (K .LT. 1 .OR. M .LT. K) CALL SETERR('DLPA - INVALID INDEX',   
     1   21, 2, 2)                                                      
C/                                                                      
      IF (.NOT. INNERP) GOTO 20                                         
         H = 0.0E0                                                      
         DO  10 I = 1, N                                                
            H = H+X(I)*A(K, I)                                          
 10         CONTINUE                                                    
         SK = H                                                         
         GOTO  40                                                       
 20      DO  30 I = 1, N                                                
            X(I) = A(K, I)                                              
 30         CONTINUE                                                    
 40   RETURN                                                            
      END                                                               
       SUBROUTINE LPRNT(A,M,N,AMAN,IA,B,C,X,CTX,IS,SIMP,ISIMP,IE,       
     1 ITER,IPTG,IAG,IAS,U,IEND,IPH)                                    
C                                                                       
C THIS IS A PRINT ROUTINE                                               
C                                                                       
       REAL CTX,A(1),X(N),B(1)                                          
       EXTERNAL AMAN                                                    
       LOGICAL IEND                                                     
       INTEGER IA(1),IPTG(N),ISIMP(1)                                   
       REAL SIMP(1),C(1),U(1)                                           
       IEND= .FALSE.                                                    
       RETURN                                                           
       END                                                              
       SUBROUTINE I4NTL(N,Q,IQ,IPTG,M,IPTS,A,IA,AMAN,SCALE,T)           
C                                                                       
C THIS SUBROUTINE INITIALIZES Q,IPTG,AND IPTS AND SCALE                 
C                                                                       
       INTEGER N,M                                                      
       INTEGER IPTS(1),IPTG(1),IA(1)                                    
       REAL Q(IQ,N),A(1),SCALE(1),T(N)                                  
       REAL SNRM2, TEMP1                                                
       EXTERNAL AMAN                                                    
       CALL SETR(N*N,0.0E0,Q)                                           
       DO 10 I=1,N                                                      
          Q(I,I)=1.0                                                    
          IPTS(I)=I                                                     
 10    CONTINUE                                                         
       IF (M.EQ.0) RETURN                                               
       DO 20 I=1,M                                                      
          IPTG(I)=I                                                     
          CALL AMAN(.FALSE.,A,IA,N,I,T,TEMP1)                           
          SCALE(I)=SNRM2(N,T,1)                                         
 20    CONTINUE                                                         
       RETURN                                                           
       END                                                              
      SUBROUTINE A4PPG(A,M,N,IA,KK,Q,IQ,LT,AG,AS,E,IPTG,IPTS,W,         
     1   SCALE, XNRM,  AMAN, EPS, TMP, P,IPRINT, RHS,INDX2, V, JDEPG,   
     2   IHIT,INVHIT,ISTRIK,DVECG)                                      
C                                                                       
C THIS SUBROUTINE ETERMINES WHETHER A GENERAL CONSTRAINT                
C SHOULD BE ADDED TO THE ACTIVE CONSTRAINT LIST AND                     
C CALLS A4GQR TO UPDATE THE LQ DECOMPOSTION IF ONE SHOULD               
C                                                                       
      INTEGER M, N, AGPE                                                
      EXTERNAL AMAN                                                     
      INTEGER KK, AG, AS, E, IPTG(1), IPTS(1),DVECG(1)                  
      REAL A(N),Q(IQ,1),LT( 1),W(N),SCALE(N),EPS,RHS(N)                 
      INTEGER NCT, NMS, AGP1, ASP1, I, IA(1)                            
      INTEGER II, IHIT(M),INVHIT(M)                                     
      REAL TMP(1), P(1),  PNRM, XNRM, SNRM2, T1, V(M), VV               
      ASP1 = AS+1                                                       
      NMS = N-AS                                                        
      IF(NMS.LE.0)RETURN                                                
      AGP1 = AG+E+1                                                     
      ISGE=AS+AGP1                                                      
      II = JDEPG+1                                                      
      NH=0                                                              
      DO 5 I=1,M                                                        
 5      INVHIT(I)=0                                                     
      IF (INDX2.LT.0)II=II+1                                            
         GOTO  20                                                       
 10      II = II+1                                                      
 20      IF (II .GT. M) GOTO  25                                        
          IF(IPTG(II).LT.0) GO TO 10                                    
         I = IPTG(II)                                                   
         IF (ABS(W(I)) .GT. (EPS)*SCALE(I)*XNRM) GOTO  10               
          IF (V(I).GE.0.0E0.AND.ISTRIK.NE.I) GO TO 10                   
         JH=0                                                           
         V(I)=ABS(V(I))/SCALE(I)                                        
         VV=V(I)                                                        
 21      JH=JH+1                                                        
         IF (JH.GT.NH) GO TO 23                                         
           J2=IHIT(JH)                                                  
           IF(VV.LT.V(J2)) GO TO 21                                     
           MNH=NH+1                                                     
           DO 22 MH=JH,NH                                               
              IHIT(MNH)=IHIT(MNH-1)                                     
              MNH=MNH-1                                                 
 22        CONTINUE                                                     
           NH=NH+1                                                      
           IHIT(JH)=II                                                  
           GO TO 10                                                     
 23      NH=NH+1                                                        
         IHIT(NH)=II                                                    
         GO TO 10                                                       
 25   IF (NH.EQ.0) GO TO 40                                             
       DO 26 I2=1,NH                                                    
          I3=IHIT(I2)                                                   
 26    INVHIT(I3)=I2                                                    
       DO 37 I2=1,NH                                                    
          II=IHIT(I2)                                                   
          I=IPTG(II)                                                    
      AGP1 = AG+E+1                                                     
C                                                                       
C A CONSTRAINT HAS BEEN HIT.                                            
C TEST IF IT LINEAR INDEPENDENT OF OTHERS                               
C                                                                       
         CALL AMAN(.FALSE., A, IA, N, I, TMP, T1)                       
       DO 30 JJ=ASP1,N                                                  
          J=IPTS(JJ)                                                    
          JAS = JJ - AS                                                 
          P(JAS) = TMP(J)                                               
 30       CONTINUE                                                      
       CALL M5TOP (IQ,N,Q,1,NMS,1,NMS,P,1,P)                            
         NCT=N-ISGE+1                                                   
         PNRM = SNRM2(NCT, P(AGP1), 1)                                  
C                                                                       
C CONSTRAINT IS INDEPENDEDNT OF OTHER CONSTRAINTS , SO                  
C ADD IT BY INTERCHANGING ELEMENTS IN IPTG AND                          
C UPDATING DECOMPOSTION                                                 
C                                                                       
           JDEPG1=JDEPG+1                                               
         IPTG(II) = IPTG(JDEPG1)                                        
         IPTG(JDEPG1) = IPTG(AGP1)                                      
         IPTG(AGP1)=I                                                   
         IDV =DVECG(II)                                                 
         DVECG(II)=DVECG(JDEPG1)                                        
         DVECG(JDEPG1)=DVECG(AGP1)                                      
         DVECG(AGP1)=IDV                                                
         IN1=INVHIT(JDEPG1)                                             
         IN2=INVHIT(AGP1)                                               
         IF (IN2.NE.0) IHIT(IN2)=JDEPG1                                 
         IF(IN1.NE.0)IHIT(IN1)=II                                       
         INVHIT(JDEPG1)=IN2                                             
         INVHIT(II)=IN1                                                 
         INVHIT(AGP1)=0                                                 
         AGPE = AG + E                                                  
         JDEPG=JDEPG+1                                                  
         IF (PNRM .LE. EPS*SCALE(I)) GOTO 37                            
         CALL A4GQR(IQ, NMS, AGPE, Q, LT, P, RHS)                       
         ISGE=ISGE+1                                                    
         AG = AG + 1                                                    
         KK = KK + 1                                                    
 37      CONTINUE                                                       
 40   RETURN                                                            
      END                                                               
      SUBROUTINE A4PPS(A, M, N,IA, KK, S, Q,IQ, R, AS, AG, E, IPTS,     
     1   DVECS,X,SIMP,ISIMP,TOLL,TOLU,EPS,TEMP,IPRINT,RHS,INDX2,P,      
     2   IPHAS, JDEPS,IHIT,INVHIT,ISTRIK)                               
C                                                                       
C THIS SUBROUTINE TESTS IF ANY OF THE SIMPLE CONSTRAINTS                
C SHOULD BE ADDED TO THE ACTIVE SET AND THEN UPDATES                    
C THE VECTORS AND LQ DECOMPOSTION                                       
C                                                                       
      INTEGER M, S, DVECS(1), IA(1),AGPE,AGEP1,IPHAS                    
      INTEGER N, KK, AS, AG, E, IPTS(1), JDEPS                          
      INTEGER ISIMP(1)                                                  
      REAL A(1),Q(IQ, 1), R( 1), X(1), SIMP(1), TOLL                    
      REAL RHS(N), P(N)                                                 
      REAL TOLU, EPS,PP                                                 
      INTEGER NMS, ASP1, I, J, K                                        
      INTEGER IABS, II,  INDEX, NMSGE, ITEMP                            
      REAL L, PNRM, SNRM2, TEMP(1),R1MACH,SL,SU,EPA                     
      INTEGER IHIT(N),INVHIT(S)                                         
      COMMON /CSTAK/ISTAK(1000)                                         
      AGPE=AG+E                                                         
      ASP1 = AS+1                                                       
      NH=0                                                              
      NMS = N - AS                                                      
      AGEP1=AG+E+1                                                      
      II = JDEPS+1                                                      
      EPA=SNRM2(N,X,1)*R1MACH(4)+R1MACH(1)                              
      IF (INDX2.GT.0)II=ASP1+1                                          
      DO  5 I=1,S                                                       
 5     INVHIT(I)=0                                                      
C                                                                       
C FIND OUT IF ANY CONSTRAINT HAS BEEN HIT                               
C                                                                       
         GOTO  20                                                       
 10      II = II+1                                                      
 20      IF (II .GT. S) GOTO  46                                        
         K = ISIMP(II)                                                  
         L = SIMP(II)                                                   
         I = IABS(K)                                                    
          SU=L*TOLU                                                     
          SL=L*TOLL                                                     
          IF (L.LT.0.0E0)SU=L*TOLL                                      
          IF(L.LT.0.E0)SL=L*TOLU                                        
          IF(SU.EQ.0.E0)SU=-EPA                                         
          IF(SL.EQ.0.E0)SL=EPA                                          
          IF (K .GT. 0) GOTO 30                                         
             IF (X(I) .LT. SU.OR.(IPHAS.EQ.1.AND.                       
     1   X(I).GT.SL)) GOTO 10                                           
             X(I)=L                                                     
             IF (P(I).LE.0.0E0.AND.ISTRIK.NE.II) GO TO 10               
             GOTO 40                                                    
 30      CONTINUE                                                       
         IF  (X(I) .GT. SL.OR.(IPHAS.EQ.1.AND.                          
     1 X(I).LT.SU)) GOTO 10                                             
             X(I)=L                                                     
             IF (P(I).GE.0.0E0.AND.ISTRIK.NE.II) GO TO 10               
 40      CONTINUE                                                       
         JH=0                                                           
         PP=ABS(P(I))                                                   
 41      JH=JH+1                                                        
         IF(JH.GT.NH) GO TO 45                                          
           J2=IHIT(JH)                                                  
           I2=IABS(ISIMP(J2))                                           
           IF (PP.LT.ABS(P(I2)))GO TO 41                                
           MNH=NH+1                                                     
           DO 43 MH=JH,NH                                               
             IHIT(MNH)=IHIT(MNH-1)                                      
             MNH=MNH-1                                                  
  43       CONTINUE                                                     
           NH=NH+1                                                      
           IHIT(JH)=II                                                  
           GO TO 10                                                     
  45       NH=NH+1                                                      
           IHIT(NH)=II                                                  
            GO TO 10                                                    
C                                                                       
C DETERMINE WHO IT BELONGS TO                                           
C                                                                       
  46       IF(NH.EQ.0) GO TO 110                                        
         DO 47 I2=1,NH                                                  
             I3=IHIT(I2)                                                
 47       INVHIT(I3)=I2                                                 
           DO 105 I2=1,NH                                               
              II=IHIT(I2)                                               
              K=ISIMP(II)                                               
              I=IABS(K)                                                 
               L=SIMP(II)                                               
         DO  60 J = ASP1, N                                             
            IF (IPTS(J) .NE. I) GOTO 50                                 
            IP = IPTS(J)                                                
            NMSGE = NMS - AG - E                                        
       JMAS = J-AS                                                      
C                                                                       
C TEST FOR LINEAR INDEPENDENCE                                          
C                                                                       
            PNRM = SNRM2 (NMSGE, Q(AGEP1,JMAS),1)                       
               ITEMP2 = DVECS(II)                                       
               JDEPS1=JDEPS+1                                           
               IN1=INVHIT(JDEPS1)                                       
               IN2=INVHIT(ASP1)                                         
               IF (IN2.NE.0)IHIT(IN2)=JDEPS1                            
               IF(IN1.NE.0)IHIT(IN1)=II                                 
               INVHIT(JDEPS1)=IN2                                       
               INVHIT(II)=IN1                                           
               ISIMP(II)=ISIMP(JDEPS1)                                  
               SIMP(II)=SIMP(JDEPS1)                                    
               DVECS(II)=DVECS(JDEPS1)                                  
C                                                                       
               ISIMP(JDEPS1)=ISIMP(ASP1)                                
               SIMP(JDEPS1)=SIMP(ASP1)                                  
                DVECS(JDEPS1)=DVECS(ASP1)                               
               ISIMP(ASP1) = K                                          
               SIMP(ASP1) = L                                           
               DVECS(ASP1) = ITEMP2                                     
               JDEPS=JDEPS+1                                            
               IF(PNRM.LE.EPS) GO TO 104                                
C IT IS INDEPENDENT SO CHANGE VECTORS                                   
C                                                                       
               GOTO  70                                                 
 50         CONTINUE                                                    
 60         CONTINUE                                                    
 70      CONTINUE                                                       
C                                                                       
C UPDATE LQ DECOMPOSTION                                                
            INDEX = J - AS                                              
         IF (AGPE.EQ.0) GO TO 80                                        
            ITEMP=IPTS(N)                                               
            CALL MOVEBI(NMS-1,IPTS(ASP1),IPTS(AS+2))                    
            IF(J.NE.N)IPTS(J+1)=ITEMP                                   
         CALL A4SQR(IQ,NMS,AGPE,Q,R,INDEX,TEMP,RHS)                     
         GO TO 100                                                      
 80       CONTINUE                                                      
         CALL MOVEBI(INDEX-1,IPTS(ASP1),IPTS(AS+2))                     
         NMS=NMS-1                                                      
         IF (INDEX.EQ.NMS+1) GO TO 100                                  
          DO 90 IJ=INDEX,NMS                                            
             RHS(IJ)=RHS(IJ+1)                                          
 90       CONTINUE                                                      
 100     CONTINUE                                                       
         IPTS(ASP1)=I                                                   
         AS = AS+1                                                      
         ASP1 = ASP1 + 1                                                
         KK = KK+1                                                      
  104       CONTINUE                                                    
 105     CONTINUE                                                       
 110  RETURN                                                            
      END                                                               
      SUBROUTINE A4GQR(K, M, N, Q, R, B, RHS)                           
C                                                                       
C THIS SUBROUTINE UPDATES THE QR DECOMPOSITION OF A MATRIX              
C WHEN THE VECTOR B IS ADDED AS THE LAST COLUMN OF THE MATRIX           
C                                                                       
      INTEGER K, I, M, N                                                
      REAL Q(K, 1), R( 1), B(1), RHS(K)                                 
      REAL BETA, ALPHA, F, SDOT                                         
       MMN=M-N                                                          
      N = N+1                                                           
      CALL H4HG(MMN,B(N),ALPHA,BETA,1)                                  
      DO 10 I=1,M                                                       
         F=SDOT(MMN,Q(N,I),1,B(N),1)/ALPHA                              
         CALL SAXPY(MMN,F,B(N),1,Q(N,I),1)                              
 10   CONTINUE                                                          
      F=SDOT(MMN,B(N),1,RHS(N),1)/ALPHA                                 
      CALL SAXPY(MMN,F,B(N),1,RHS(N),1)                                 
      IS=(N*(N-1))/2+1                                                  
      IF (N.GT.1)CALL SCOPY(N-1,B,1,R(IS),1)                            
      IS=IS+N-1                                                         
      R(IS)=BETA                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE A4SQR(K, M, N, Q, R, INDEX, TEMP, RHS)                 
C                                                                       
C THIS SUBROUTINE UPDATES THE QR DECOMPOSITION WHEN                     
C THE INDEXTH ROW IS DELETED FROM THE MATRIX                            
      INTEGER M, N, INDEX, K                                            
      REAL TEMP(1), Q(K, 1), R( 1), RHS(K)                              
      INTEGER I, J                                                      
      REAL BETA, ALPHA, F,FF, SDOT, X,Y                                 
C/6S                                                                    
C     IF (INDEX .LE. 0 .OR. M+1 .LT. INDEX) CALL SETERR(                
C    1   21HA4SQR - INVALID INDEX, 21, 2, 2)                            
C/7S                                                                    
      IF (INDEX .LE. 0 .OR. M+1 .LT. INDEX) CALL SETERR(                
     1   'A4SQR - INVALID INDEX', 21, 2, 2)                             
C/                                                                      
       CALL SCOPY (M,Q(1,INDEX),1,TEMP,1)                               
C                                                                       
C INTERCHANGE LAST COLUMN AND COLUMN INDEX                              
C                                                                       
       IF (INDEX.NE.M)CALL SCOPY(M,Q(1,M),1,Q(1,INDEX),1)               
         MMN=M-N                                                        
C                                                                       
C                                                                       
         KK=M                                                           
         IF (INDEX.GT.N)KK=INDEX                                        
         X=RHS(KK)                                                      
         MM1=M-1                                                        
         NP1=N+1                                                        
      IF (M.LE.N+1) GO TO 20                                            
         KKMN=KK-N                                                      
         CALL H4HG(MMN,TEMP(N+1),ALPHA,BETA,KKMN)                       
         DO 10 I=1,MM1                                                  
            F=SDOT(MMN,TEMP(NP1),1,Q(NP1,I),1)/ALPHA                    
            CALL SAXPY(MMN,F,TEMP(NP1),1,Q(NP1,I),1)                    
 10      CONTINUE                                                       
         FF=SDOT(MMN,TEMP(NP1),1,RHS(NP1),1)/ALPHA                      
         CALL SAXPY(MMN,FF,TEMP(NP1),1,RHS(NP1),1)                      
         TEMP(KK)=BETA                                                  
         X=RHS(KK)                                                      
         IF (INDEX.LE.N)GO TO 20                                        
            RHS(INDEX)=RHS(M)                                           
            CALL SSWAP(M-1,Q(INDEX,1),K,Q(M,1),K)                       
 20      CONTINUE                                                       
         I=N                                                            
C                                                                       
C ELIMINATE UNWANTED ELEMENTS                                           
C                                                                       
             IS=(N*(N+1))/2                                             
         GOTO  40                                                       
 30      I = I-1                                                        
 40      IF (I .LT. 1) GOTO  70                                         
         IIJ=I                                                          
         IF (I.LE.N)Q(I,M)=0.0                                          
         IF (I.NE.INDEX)GO TO 60                                        
          CALL SSWAP(M-1,Q(I,1),K,Q(M,1),K)                             
C SWAP RHS                                                              
         Y=RHS(I)                                                       
         RHS(I)=X                                                       
         X=Y                                                            
         IIJ=M                                                          
         KK=INDEX                                                       
C SWAP R                                                                
         IF (I.GT.N) GO TO 60                                           
           IS2=IS                                                       
           DO 50 J=I,N                                                  
              Y=Q(J,M)                                                  
              Q(J,M)=R(IS2)                                             
              R(IS2)=Y                                                  
              IS2=IS2+J                                                 
 50        CONTINUE                                                     
 60    CONTINUE                                                         
            CALL SROTG(TEMP(KK), TEMP(IIJ),ALPHA,BETA)                  
            CALL SROT (M-1,Q(M, 1),K,Q(I,1),K,ALPHA,BETA)               
            Y=RHS(I)                                                    
            RHS(I)=-BETA*X+ALPHA*Y                                      
            X=ALPHA*X+BETA*Y                                            
            IF (I.GT.N)GO TO 30                                         
            NMI=N-I                                                     
            CALL SROT2(NMI+1,Q(I,M),-1,R(IS),I,ALPHA,BETA)              
            IS=IS-I                                                     
            GOTO  30                                                    
 70       M=M-1                                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE C4NST(A, M, N, IA, S, P, V, W, AG, JDEPS,AS, PNRM,     
     1   IPTS,IPTG,SIMP,ISIMP,AMAN,EPS,SCALE,X,THETA,T,IHIT,IERC,       
     2   IFLAG)                                                         
C                                                                       
C THIS SUBROUTINE LEAVES IN THETA THE DISTANCE ONE                      
C CAN TRAVEL ALONG THE DIRECTION P BEFORE HITTING                       
C THE INACTIVE CONSTRAINTS                                              
C                                                                       
C IF IHIT IS LESS THAN M, THE IHITTH GENERAL CONSTRAINT HAS BEEN        
C HIT. IF GREATER THAN M, THEN THE IHITTH-M SIMPLE CONSTRAINT HAS BEEN  
C HIT.                                                                  
C                                                                       
      INTEGER M, N, S, IA(1), ISIMP(1)                                  
      EXTERNAL AMAN                                                     
      INTEGER AG, AS, IPTS(1), IPTG(1)                                  
      REAL A(1), P(1), V(1), W(1), PNRM, EPS                            
      REAL SCALE(1), X(1)                                               
      INTEGER NMS, AGP1, ASP1, I, K, II                                 
      REAL T(1), TEMP1, THETA, SIMP(1),VV,WW,ET,FLOAT                   
      LOGICAL UNBNDD                                                    
      INTEGER IT1                                                       
      UNBNDD = .TRUE.                                                   
      IERC=0                                                            
      NMS = N-AS                                                        
      AGP1 = AG+1                                                       
      ASP1 = AS+1                                                       
      IF (M .LT. AGP1) GOTO  60                                         
C                                                                       
C DETERMINE THE DISTANCE TO THE NEAREST GENERAL CONSTRAINT              
      IHN=N/2                                                           
      DO  50 IJ = AGP1, M                                               
         I = IPTG(IJ)                                                   
         IF (AS.GT.IHN)GO TO 10                                         
          CALL AMAN(.TRUE.,A,IA,N,I,P,V(I))                             
          GO TO 30                                                      
 10       CALL AMAN(.FALSE., A, IA, N, I, T, TEMP1)                     
          V(I) = 0.                                                     
          DO  20 II = 1, NMS                                            
             IT1 = AS+II                                                
             K = IPTS(IT1)                                              
             V(I) = V(I)+T(K)*P(K)                                      
 20          CONTINUE                                                   
 30       CONTINUE                                                      
         IF (V(I) .GE. 0.0E0) GOTO 50                                   
C                                                                       
C WE WILL HIT THE CONSTRAINT BE GOING IN THE DIRECTION P                
C                                                                       
C  JULY 1983, DAVID GAY DETERMINED BUG TROUBLE, AND                     
C  PHYL FOX COMMENTED OUT THE FOLLOWING LINE (S. P. VERSION ALSO)       
C       IF (W(I).EQ.0.0E0.AND.IFLAG.EQ.1) GO TO 5                       
            IF (UNBNDD) GOTO 40                                         
               IF (V(I)*THETA .GE. W(I)) GO TO 50                       
                   THETA=W(I)/V(I)                                      
               IHIT=I                                                   
               GOTO  50                                                 
 40            UNBNDD = .FALSE.                                         
C                                                                       
C THIS IS THE FIRST CONSTRAINT ENCOUNTERED THAT EVEN                    
C IS IN THE RIGHT DIRECTION                                             
C                                                                       
               THETA = W(I)/V(I)                                        
               IHIT=I                                                   
 50      CONTINUE                                                       
 60      CONTINUE                                                       
      JDEPS1=JDEPS+1                                                    
      IF (S .LT. JDEPS1) GOTO  90                                       
C                                                                       
C DETERMINE THE DISTANCE TO THE NEAREST SIMPLE CONSTRAINT               
C                                                                       
      DO  80 II = JDEPS1, S                                             
       ET = SIGN(1.0E0, FLOAT(ISIMP(II)))                               
       I1 = IABS(ISIMP(II))                                             
       VV = ET * P(I1)                                                  
       IF (VV .GE. 0.0E0) GOTO 80                                       
       WW = ET*(SIMP(II)-X(I1))                                         
       IF (UNBNDD) GOTO 70                                              
       IF (THETA*VV .GE. WW) GO TO 80                                   
           THETA=WW/VV                                                  
         IHIT=II+M                                                      
         GOTO 80                                                        
 70     UNBNDD = .FALSE.                                                
        THETA = WW/VV                                                   
        IHIT=II+M                                                       
 80      CONTINUE                                                       
 90      CONTINUE                                                       
      IF (UNBNDD) GOTO 100                                              
         RETURN                                                         
 100     IERC=6                                                         
 110  RETURN                                                            
      END                                                               
      SUBROUTINE C4ONS(A, M, N, IA, S, P, V, W, AG, JDEPS,AS, PNRM,     
     1   IPTS,IPTG,SIMP,ISIMP,AMAN,EPS,SCALE,X,THETA,T,IHIT,IERC)       
C                                                                       
C THIS SUBROUTINE LEAVES IN THETA THE DISTANCE ONE                      
C CAN TRAVEL ALONG THE DIRECTION P BEFORE HITTING                       
C THE INACTIVE CONSTRAINTS                                              
C                                                                       
C IF IHIT IS LESS THAN M, THE IHITTH GENERAL CONSTRAINT HAS BEEN        
C HIT. IF GREATER THAN M, THEN THE IHITTH-M SIMPLE CONSTRAINT HAS BEEN  
C HIT.                                                                  
C                                                                       
      INTEGER M, N, S, IA(1), ISIMP(1)                                  
      EXTERNAL AMAN                                                     
      INTEGER AG, AS, IPTS(1), IPTG(1)                                  
      REAL A(1), P(1), V(1), W(1), PNRM, EPS,THETA2                     
      REAL SCALE(1), X(1), FLOAT                                        
      INTEGER NMS, AGP1, ASP1, I, K, II                                 
      REAL T(1), TEMP1, THETA, SIMP(1), EPP, ET, VV, WW                 
      LOGICAL UNBNDD                                                    
      INTEGER IT1                                                       
      IERC=0                                                            
      EPP=PNRM*EPS                                                      
      UNBNDD = .TRUE.                                                   
      AGP1 = AG+1                                                       
      ASP1 = AS+1                                                       
      THETA2=0.0E0                                                      
      IHIT2=0                                                           
      IF (M .LT. AGP1) GOTO  50                                         
C                                                                       
C DETERMINE THE DISTANCE TO THE NEAREST GENERAL CONSTRAINT              
      DO  40 IJ = AGP1, M                                               
         I = IPTG(IJ)                                                   
         CALL AMAN(.FALSE., A, IA, N, I, T, TEMP1)                      
         V(I) = 0.                                                      
      NMS=N-AS                                                          
         DO  10 II = 1, NMS                                             
            IT1 = AS+II                                                 
            K = IPTS(IT1)                                               
            V(I) = V(I)+T(K)*P(K)                                       
 10         CONTINUE                                                    
        IF (W(I).LT.0.0E0) GO TO 20                                     
           IF (V(I).LE.EPP*SCALE(I))GO TO 40                            
           IF (V(I)*THETA2.GE.W(I)) GO TO 40                            
                IHIT2=I                                                 
                THETA2=W(I)/V(I)                                        
                GO TO 40                                                
 20     CONTINUE                                                        
         IF (V(I) .GE. (-EPP)*SCALE(I)) GOTO 40                         
C                                                                       
C WE WILL HIT THE CONSTRAINT BE GOING IN THE DIRECTION P                
C                                                                       
            IF (UNBNDD) GOTO 30                                         
               IF (V(I)*THETA .GE. W(I)) GO TO 40                       
                   THETA=W(I)/V(I)                                      
               IHIT=I                                                   
               GOTO  40                                                 
 30            UNBNDD = .FALSE.                                         
C                                                                       
C THIS IS THE FIRST CONSTRAINT ENCOUNTERED THAT EVEN                    
C IS IN THE RIGHT DIRECTION                                             
C                                                                       
               THETA = W(I)/V(I)                                        
               IHIT=I                                                   
 40      CONTINUE                                                       
 50      CONTINUE                                                       
      JDEPS1=JDEPS+1                                                    
      IF (S .LT. JDEPS1) GOTO  90                                       
C                                                                       
C DETERMINE THE DISTANCE TO THE NEAREST SIMPLE CONSTRAINT               
C                                                                       
      DO  80 II = JDEPS1, S                                             
       ET = SIGN(1.0E0, FLOAT(ISIMP(II)))                               
       I1 = IABS(ISIMP(II))                                             
       WW = ET*(SIMP(II)-X(I1))                                         
       VV = ET * P(I1)                                                  
       IF (WW.LE.0.0E0) GO TO 60                                        
          IF (VV.LE.EPS.OR.VV*THETA2.GE.WW) GO TO 80                    
              THETA2=WW/VV                                              
              IHIT2=II+M                                                
 60    CONTINUE                                                         
       IF (VV .GE. (-EPS)) GOTO 80                                      
       IF (UNBNDD) GOTO 70                                              
       IF (THETA*VV .GE. WW) GO TO 80                                   
           THETA=WW/VV                                                  
         IHIT=II+M                                                      
         GOTO 80                                                        
 70     UNBNDD = .FALSE.                                                
        THETA = WW/VV                                                   
        IHIT=II+M                                                       
 80      CONTINUE                                                       
 90      CONTINUE                                                       
      IF (.NOT.UNBNDD)RETURN                                            
         THETA=THETA2                                                   
         IHIT=IHIT2                                                     
          IF (IHIT2.EQ.0)IERC=7                                         
 100  RETURN                                                            
      END                                                               
      SUBROUTINE D4CLM(U, AG, AS, E, UMAX, INDX2, FLAG, DVECS,          
     1   DVECG, IPTG, IPTS, BOUND,IPHAS,KK,N)                           
C                                                                       
C GIVEN THE LAGRANGE MULTIPLIERS, THIS SUBROUTINE ECIDES                
C WHICH CONSTRAINT TO DROP.                                             
C CURRENTLY THE DECISION IS MADE TO DROP THE ONE IWTH THE               
C LARGEST LAGRANGE MULTIPLIER THAT HAS NOT BEEN                         
C DROPPED BEFORE A MINIMUM ON THE CURRENT SUBSPACE HAS                  
C BEEN FOUND                                                            
C                                                                       
      INTEGER AG, AS, E, INDX2, FLAG, DVECG(1)                          
      INTEGER DVECS(1), IPTG(1), IPTS(1)                                
      REAL U(1), UMAX, BOUND                                            
      INTEGER I, K, INDEX, EP1                                          
      REAL  V5MAX, R1MACH                                               
      INTEGER AGPAS                                                     
       EP1=E+1                                                          
      FLAG=1                                                            
      UMAX = V5MAX(AG+AS, U(E+1), INDEX)                                
      INDX2 = INDEX+E                                                   
C     IF (UMAX.LT.R1MACH(4)*100.0E0) RETURN                             
      IF (UMAX.LT.0.0D0) RETURN                                         
C                                                                       
C SOMETHING MUST BE DROPPED                                             
C                                                                       
      AGPAS=AG+AS                                                       
      DO 30 I=1,AGPAS                                                   
      FLAG=0                                                            
          IF (KK.EQ.N) RETURN                                           
         IF(INDEX.LE.AG)GO TO 10                                        
C SIMPLE CONSTRAINT                                                     
         K=INDEX-AG                                                     
          IF (DVECS(K).LT.1)RETURN                                      
         GO TO 20                                                       
 10      K=IPTG(INDEX)                                                  
C LOOKING AT A GENERAL CONSTRAINT                                       
         IF (DVECG(K).LT.1)RETURN                                       
 20      U(INDX2)=-1.0E0                                                
         FLAG=1                                                         
         IF(V5MAX(AGPAS,U(E+1),INDEX).LT.0.0E0) RETURN                  
         INDX2=INDEX+E                                                  
 30   CONTINUE                                                          
      RETURN                                                            
      END                                                               
       SUBROUTINE D4RPG(M,N,KK,Q,IQ,LT,AG,AS,E,IPTG,INDX2,DVECG,RHS,    
     1  C,IPTS)                                                         
        INTEGER DVECG(1)                                                
       INTEGER M,N,KK,AG,AS,E,IPTG(1),AGM1,AGPE,ITEMP,INDX2,NMS         
       REAL Q(IQ, 1), LT( 1),RHS(N), C(N)                               
       INTEGER IPTS(1)                                                  
       AGPE = AG +E                                                     
       AGM1=AGPE-1                                                      
C                                                                       
C THIS SUBROUTINE UPDATES THE VECTORS AND CALLS A                       
C SUBROUTINE TO UPDATE THE LQ DECOMPOSITION WHEN                        
C A GENERAL CONSTRAINT IS DROPPED                                       
C                                                                       
       NMS = N - AS                                                     
       ITEMP = IPTG(INDX2)                                              
       IF (AGPE .EQ. INDX2) GOTO 20                                     
       DO 10 I = INDX2,AGM1                                             
            IPTG(I) = IPTG(I+1)                                         
 10         CONTINUE                                                    
       IPTG(AGPE) = ITEMP                                               
 20    CONTINUE                                                         
       DVECG(ITEMP) = 1                                                 
       IF (AGM1.GT.0) GO TO 50                                          
          DO 40 I=1,NMS                                                 
             DO 30 J=1,NMS                                              
                Q(I,J)=0.0                                              
 30         CONTINUE                                                    
            Q(I,I)=1.0                                                  
            IPAS=I+AS                                                   
            IK=IPTS(IPAS)                                               
            RHS(I)=C(IK)                                                
 40       CONTINUE                                                      
          GO TO 60                                                      
 50       CONTINUE                                                      
       CALL D4GQR(IQ,NMS,AGPE,Q,LT,INDX2,RHS)                           
 60    AG = AG - 1                                                      
       KK = KK - 1                                                      
       RETURN                                                           
       END                                                              
      SUBROUTINE D4GQR(K, M, N, Q, R, INDEX,RHS)                        
C                                                                       
C THIS SUBROUTINE UPDATES THE QR DECOMPOSITION OF A MATRIX WHEN         
C COLUMN INDEX IS DELETED FROM THE MATRIX                               
C                                                                       
      INTEGER M                                                         
      INTEGER N, INDEX                                                  
      REAL Q(K, 1), R( 1), RHS(K)                                       
      INTEGER I                                                         
      REAL BETA, ALPHA                                                  
C/6S                                                                    
C     IF (M .LE. 0 .OR. N .LE. 0 .OR. M .LT. N) CALL SETERR(            
C    1   25HD4GQR - INVALID DIMENSION, 25, 1, 2)                        
C     IF (INDEX .LE. 0 .OR. N .LT. INDEX) CALL SETERR(                  
C    1   21HD4GQR - INVALID INDEX, 21, 2, 2)                            
C/7S                                                                    
      IF (M .LE. 0 .OR. N .LE. 0 .OR. M .LT. N) CALL SETERR(            
     1   'D4GQR - INVALID DIMENSION', 25, 1, 2)                         
      IF (INDEX .LE. 0 .OR. N .LT. INDEX) CALL SETERR(                  
     1   'D4GQR - INVALID INDEX', 21, 2, 2)                             
C/                                                                      
       N=N-1                                                            
C                                                                       
C ELIMINATE UNWANTED SUBDIAGONAL ELEMENTS                               
C                                                                       
      IF (INDEX.GT.N) RETURN                                            
      IS =(INDEX*(INDEX+1))/2+INDEX                                     
      DO 10 I=INDEX,N                                                   
         CALL SROTG(R(IS), R(IS+1), ALPHA, BETA)                        
         CALL SROT(1,RHS(I),1,RHS(I+1),1,ALPHA,BETA)                    
         CALL SROT(M, Q(I, 1), K, Q(I+1, 1), K, ALPHA, BETA)            
         NMI=N-I                                                        
         IS=IS+I+2                                                      
         CALL SROT2(NMI,R(IS-1),I+2,R(IS),I+2,ALPHA,BETA)               
 10    CONTINUE                                                         
C                                                                       
C REARRANGE R                                                           
C                                                                       
       IS2=(INDEX*(INDEX+1))/2+1                                        
       IS1=IS2-INDEX                                                    
       DO 20 I=INDEX,N                                                  
          CALL SCOPY(I,R(IS2),1,R(IS1),1)                               
          IS1=IS1+I                                                     
          IS2=IS2+I+1                                                   
 20    CONTINUE                                                         
      RETURN                                                            
      END                                                               
       SUBROUTINE D4RPS(A,M,N,IA,AMAN,KK,Q,IQ,LT,AG,AS,E,IPTG,IPTS,     
     1   DVECS,SIMP,ISIMP,INDD2,TEMP,B,RHS)                             
C                                                                       
C THIS SUBROUTINE UPDATES THE VECTORS AND CALLS A SUBROUTINE            
C TO UPDATE THE LQ DECOMPOSTION WHEN A SIMPLE CONSTRAINT IS             
C DROPPED                                                               
C                                                                       
       INTEGER ISIMP(1),IPTG(1),IA(1)                                   
       INTEGER M,N,KK,AG,AS,E,IPTS(1),ITEMP,INDX2,AGPE,DVECS(1)         
       REAL A(N),SIMP(N),Q(IQ,1),LT(1),B(N),TEMP(N),RHS(N)              
       REAL TEMP1,TOUT, T1                                              
       EXTERNAL AMAN                                                    
       INDX2 = INDD2 - AG-E                                             
        CALL AMAN(.FALSE.,A,IA,N,1,TEMP,TOUT)                           
       ITEMP = IPTS(INDX2)                                              
       ITEMP1 = ISIMP(INDX2)                                            
       TEMP1 = SIMP(INDX2)                                              
       NMS = N - AS                                                     
       AS = AS - 1                                                      
       IF (AS .LT. INDX2) GOTO  20                                      
       DO 10 I = INDX2,AS                                               
            DVECS(I) = DVECS(I+1)                                       
            SIMP(I) = SIMP(I+1)                                         
            ISIMP(I) = ISIMP(I+1)                                       
 10         CONTINUE                                                    
       ISIMP(AS + 1) = ITEMP1                                           
       SIMP(AS+1) = TEMP1                                               
 20    CONTINUE                                                         
       NM1=N-1                                                          
       DO 30 I=INDX2,NM1                                                
          IPTS(I)=IPTS(I+1)                                             
 30    CONTINUE                                                         
       IPTS(N) = ITEMP                                                  
       DVECS(AS+1) = 1                                                  
C                                                                       
C A NEW ROW IS TO BE ADDED TO THE DECOMPXOSTITION                       
C DETERMINE IT                                                          
C                                                                       
       AGPE = AG + E                                                    
       IF (AGPE .EQ. 0) GOTO 50                                         
       DO 40 II = 1, AGPE                                               
            I = IPTG(II)                                                
            CALL AMAN(.FALSE.,A,IA,N,I,TEMP,T1)                         
            B(II) = TEMP(ITEMP)                                         
 40         CONTINUE                                                    
       CALL D4SQR (IQ, NMS, AGPE, Q, LT, B, RHS)                        
 50    CONTINUE                                                         
       KK = KK - 1                                                      
       RETURN                                                           
       END                                                              
      SUBROUTINE D4SQR(K, M, N, Q, R, B, RHS)                           
      INTEGER M                                                         
      INTEGER N                                                         
      REAL Q(K, 1), R( 1), B(1), RHS(K)                                 
      INTEGER I                                                         
      REAL BETA, ALPHA,U, X                                             
C                                                                       
C THIS SUBROUTINE UPDATES THE QR DECOMPOSTION WHENE A NEW               
C ROW CONTAINED IN B IS ADDED TO THE MATRIX                             
C                                                                       
       M=M+1                                                            
       MM1=M-1                                                          
C                                                                       
C ZERO OUT ROW AND COLUMN OF Q MATRIX                                   
C                                                                       
       Q(M,M)=1.                                                        
       IF(M.EQ.1)RETURN                                                 
       DO 10 II=1,MM1                                                   
          Q(M,II)=0.0                                                   
          Q(II,M)=0.0                                                   
 10       CONTINUE                                                      
       X=RHS(M)                                                         
         IF (N.EQ.0) RETURN                                             
         IS=1                                                           
         DO 20 I=1,N                                                    
         CALL SROTG(R(IS), B(I), ALPHA, BETA)                           
         CALL SROT(M, Q(I, 1), K, Q(M, 1), K, ALPHA, BETA)              
         U=RHS(I)                                                       
         RHS(I)=ALPHA*U+BETA*X                                          
         X=-BETA*U+ALPHA*X                                              
         IS=IS+I+1                                                      
         IF (N-I.GE.1)                                                  
     1    CALL SROT2(N-I,R(IS-1),I+1,B(I+1),-1,ALPHA,BETA)              
 20     CONTINUE                                                        
      RHS(M)=X                                                          
      RETURN                                                            
      END                                                               
       SUBROUTINE E4QL(A,N,AMAN,IA,KK,G,IG,DU,B,X,RHS,V,R)              
       INTEGER N,IA(1),KK                                               
       REAL A(1),G(IG,1),DU(1),X(N),RHS(N),B(N),V(1),R(1)               
       REAL SDOT,SUM2,SUM,TAU,TEMP1                                     
       EXTERNAL AMAN                                                    
C                                                                       
C GET THE QR DECOMPOSITION AND GENERATE THE RIGHT HAND SIDE             
C                                                                       
       DO 10   I = 1,KK                                                 
          CALL AMAN (.FALSE.,A,IA,N,I,G(1,I),TEMP1)                     
          RHS(I)=B(I)-SDOT(N,X,1,G(1,I),1)                              
 10       CONTINUE                                                      
          CALL LST2D(N,N,KK,G,DU)                                       
C                                                                       
C SOLVE THE LEAST SQUARES PROBLEM                                       
C                                                                       
         DO 40 I=1,KK                                                   
            SUM=RHS(I)                                                  
            IM1=I-1                                                     
            IF (I.EQ.1) GO TO 30                                        
            DO 20 J=1,IM1                                               
               SUM=SUM-RHS(J)*G(J,I)                                    
 20         CONTINUE                                                    
 30         RHS(I)=SUM/DU(I)                                            
 40      CONTINUE                                                       
C                                                                       
C PUT THE R FROM THE UPPER TRIANGULAR FORM IN R                         
C                                                                       
        L=1                                                             
        DO 70 I=1,KK                                                    
           IF (I.EQ.1)GO TO 60                                          
           IM1=I-1                                                      
           DO 50 J=1,IM1                                                
              R(L)=G(J,I)                                               
              L=L+1                                                     
 50        CONTINUE                                                     
 60        R(L)=DU(I)                                                   
           L=L+1                                                        
 70      CONTINUE                                                       
C                                                                       
C FORM THE Q INTO G                                                     
C                                                                       
        KKP1=KK+1                                                       
        DO 100 IB=1,KK                                                  
           I=KKP1-IB                                                    
           TAU=DU(I)*G(I,I)                                             
           NMI=N-I                                                      
           SUM2=1.0/DU(I)                                               
           IP1=I+1                                                      
           IF (I.EQ.N) GO TO 90                                         
           DO 80 K=IP1,N                                                
              G(I,K)=0.0                                                
              SUM=SDOT(NMI,G(IP1,I),1,G(IP1,K),1)/TAU                   
              CALL SAXPY(NMI+1,SUM,G(I,I),1,G(I,K),1)                   
 80        CONTINUE                                                     
           CALL SSCAL(NMI,SUM2,G(IP1,I),1)                              
 90     CONTINUE                                                        
        G(I,I)=1.0+SUM2*G(I,I)                                          
 100    CONTINUE                                                        
       DO 110 I=1,N                                                     
          NMI=N-I                                                       
          IF (I.NE.N)CALL SSWAP(NMI,G(I+1,I),1,G(I,I+1),IG)             
 110   CONTINUE                                                         
         CALL M5TOP(IG,N,G,1,KK,1,N,RHS,2,RHS)                          
         DO 120 I=1,N                                                   
            X(I)=X(I)+RHS(I)                                            
 120     CONTINUE                                                       
          RETURN                                                        
        END                                                             
      SUBROUTINE G4ETC(N, M, S, AS, AGPE, A, IA, AMAN, X, CC, RHS       
     1   , W, SCALE, Q,IQ,IPTS, ISIMP, SIMP, IPTG, DONE, CTX,           
     2    CNRM,IST,FIRST)                                               
      INTEGER N                                                         
      EXTERNAL AMAN                                                     
       REAL DSTACK(1000)                                                
       COMMON /CSTAK/DSTACK                                             
      INTEGER M, S, AS, AGPE, IA(1), IPTS(1)                            
      INTEGER ISIMP(1), IPTG(1), IST,ISP1                               
      REAL A(1), X(N), CC(N), RHS(N), W(1), SCALE(1),CTX                
      REAL CNRM,SDOT                                                    
      REAL Q(IQ, N), SIMP(1)                                            
      LOGICAL DONE, FIRST                                               
      REAL SNRM2,EPS                                                    
      INTEGER II, IIAS, IK, IABS, I, K                                  
      INTEGER NMS, I1, ASP1                                             
      REAL ET, SC, TEMP, FLOAT, R1MACH, EP1, SI, EP2                    
C THIS PROCEDURE OBTAINS THE GRADIENT AND THE PROJECT GRADIENT          
C TO PUSH INFEASIBLE POINTS TO BE MORE FEASIBLE                         
      EP1=R1MACH(4)*FLOAT(N)*10.0E0                                     
      EPS=SNRM2(N,X,1)*EP1                                              
      IS=IST                                                            
      ASP1=AS+1                                                         
      DO  1 I = 1, N                                                    
         CC(I) = 0.0E0                                                  
   1     CONTINUE                                                       
C FIND VIOLATED GENERAL CONSTRAINTS AND ADJUST GRADIENT                 
 11   DONE=.TRUE.                                                       
      IF (IST.LT.0)GO TO 4                                              
      ISP1=IST+1                                                        
      IF (M .LT. ISP1) GOTO 34                                          
         DO  3 I = ISP1, M                                              
            K = IPTG(I)                                                 
            IF (W(K) .LE. EPS*SCALE(K)) GOTO 2                          
            IF (DONE.AND..NOT.FIRST)IST=I                               
               DONE = .FALSE.                                           
               SC = 1.0/SCALE(K)                                        
               CALL AMAN(.FALSE., A, IA, N, K, RHS, TEMP)               
               CALL SAXPY(N, SC, RHS, 1, CC, 1)                         
               IF (.NOT.FIRST)GO TO 77                                  
   2        CONTINUE                                                    
   3        CONTINUE                                                    
C LOOK NOW AT SIMPLE CONSTRAINTS TO FIND THOSE THAT                     
C ARE VIOLATED                                                          
 34   IS=-AS                                                            
   4  ISP1=-IS+1                                                        
      IF (S .LT. ISP1) GOTO 77                                          
         DO  6 I = ISP1, S                                              
            ET = 1.0                                                    
            IF (ISIMP(I) .LT. 0) ET = -1.0                              
            I1 = IABS(ISIMP(I))                                         
            EP2=EP1                                                     
            IF (SIMP(I).LT.0.0E0)EP2=-EP1                               
            SI=ET*SIMP(I)-EP2*SIMP(I)                                   
            IF (SI.EQ.0.0)SI=-EPS                                       
            IF (ET*X(I1) .GE. SI) GOTO 5                                
                 IF (DONE.AND..NOT.FIRST)IST=-I                         
               DONE = .FALSE.                                           
               CC(I1) = CC(I1)+ET                                       
              IF (.NOT.FIRST)GO TO 77                                   
   5        CONTINUE                                                    
   6        CONTINUE                                                    
           IF (DONE)RETURN                                              
           CNRM=SNRM2(N,CC,1)                                           
           IF (CNRM.GT.EP1)GO TO 77                                     
               FIRST=.FALSE.                                            
               GO TO 11                                                 
 77       CONTINUE                                                      
            CTX=SDOT(N,X,1,CC,1)                                        
C PROJECT GRADIENT                                                      
   7  DO  8 II = ASP1, N                                                
         IK = IPTS(II)                                                  
         IIAS = II-AS                                                   
         RHS(IIAS) = CC(IK)                                             
   8     CONTINUE                                                       
      NMS = N-AS                                                        
      CALL M5TOP(IQ,N,Q,1,NMS,1,NMS,RHS,1,RHS)                          
      RETURN                                                            
      END                                                               
      SUBROUTINE H4HG(N,U,H,G,K)                                        
C                                                                       
C THIS SUBROUTINE GENERATES A HOUSEHOLD TRANSFORMATION                  
C                                                                       
       REAL G,U(N)                                                      
       REAL H,S,SASUM,SCALE                                             
        IF (N.LT.1)RETURN                                               
       SCALE=SASUM(N,U,1)                                               
       S=0.0                                                            
       H=1.0                                                            
       IF (SCALE.EQ.0.0)RETURN                                          
        DO 10 I=1,N                                                     
          U(I)=U(I)/SCALE                                               
          S=S+U(I)*U(I)                                                 
 10    CONTINUE                                                         
       G=SQRT(S)                                                        
       IF(U(K).GT.0.0)G=-G                                              
       H=-(S-G*U(K))                                                    
       U(K)=U(K)-G                                                      
       G=G*SCALE                                                        
       RETURN                                                           
       END                                                              
       SUBROUTINE L4AGS(A,M,N,IA,AMAN,AS,U1,U2,C,ISIMP,AGPE,IPTS,       
     1   IPTG,TEMP)                                                     
C                                                                       
C THIS SUBROUTINE COMPUTES THE LAGRANGE MULTIPLIERS                     
C FOR SIMPLE ACTIVE CONSTRAINTS                                         
C                                                                       
       INTEGER N,IA(1),AS,ISIMP(1),AGPE,IPTS(1),IPTG(1),J,M             
       REAL U1(1),U2(1),C(1),A(1),TEMP(N),T                             
       EXTERNAL AMAN                                                    
       DO 10 II=1,AS                                                    
          I=IPTS(II)                                                    
          U2(II)=C(I)                                                   
 10       CONTINUE                                                      
       IF (AGPE .EQ.0) GOTO 40                                          
       DO 30 II = 1, AGPE                                               
            I = IPTG(II)                                                
            CALL AMAN (.FALSE.,A,IA,N,I,TEMP,T)                         
            DO 20 JJ=1,AS                                               
               J=IPTS(JJ)                                               
               U2(JJ) =  U2(JJ) -U1(II) * TEMP(J)                       
 20            CONTINUE                                                 
 30         CONTINUE                                                    
 40    CONTINUE                                                         
C                                                                       
C ADJUST SIGNS FOR UPPER BOUNDS                                         
C                                                                       
       DO 50 II=1,AS                                                    
          IF (ISIMP(II).LT.0)U2(II)=-U2(II)                             
 50    CONTINUE                                                         
       RETURN                                                           
       END                                                              
        SUBROUTINE M4TOP(N,R,RHS,X)                                     
C                                                                       
C THIS SUBROUTINE SOLVES AN UPPER TRIANGULAR SYSTEM                     
C STORED IN 1 VECTOR                                                    
        REAL RHS(N),X(N),R(N), T                                        
        L=(N*(N+1))/2                                                   
        DO 10 I=1,N                                                     
           X(I)=-RHS(I)                                                 
 10     CONTINUE                                                        
        L=(N*(N+1))/2                                                   
        NP1=N+1                                                         
        DO 30 IB=1,N                                                    
           I=NP1-IB                                                     
           X(I)=-X(I)/R(L)                                              
           T=X(I)                                                       
           L=L-1                                                        
           J=I-1                                                        
 20        IF (J.LE.0)GO TO 30                                          
           X(J)=X(J)+T*R(L)                                             
           L=L-1                                                        
           J=J-1                                                        
           GO TO 20                                                     
 30     CONTINUE                                                        
        RETURN                                                          
        END                                                             
      SUBROUTINE P4RJD(N, AS, AG, E, Q,IQ, IPTS, C, P, TEMP, RHS)       
       INTEGER N                                                        
      INTEGER AS, AG, IPTS(1)                                           
      REAL Q(IQ, 1), C(N),RHS(N)                                        
      INTEGER NMS, ASP1, E, I, K, AGPE                                  
      REAL P(N), TEMP(1)                                                
      NMS = N-AS                                                        
      ASP1 = AS + 1                                                     
      AGPE = AG+E                                                       
       IAGP1=AGPE+1                                                     
       IF (AS.EQ.0 ) GO TO 20                                           
       DO 10 I=1,AS                                                     
          K=IPTS(I)                                                     
          P(K)=0.0                                                      
 10    CONTINUE                                                         
 20    CONTINUE                                                         
          DO 30 I=ASP1,N                                                
             TEMP(I)=0.0                                                
 30       CONTINUE                                                      
          IF (AGPE.GT.0)CALL M5TOP(IQ,N,Q,1,AGPE,1,NMS,RHS,2,TEMP(ASP1))
          DO 40 I=ASP1,N                                                
             K=IPTS(I)                                                  
             P(K)=C(K)-TEMP(I)                                          
 40       CONTINUE                                                      
          RETURN                                                        
          END                                                           
       SUBROUTINE DLINPR(A,M,N,IA,B,C,X,MAXITR,CTX,S,SIMP,              
     1   ISIMP,E)                                                       
C THIS IS A LINEAR PROGRAMMING PACKAGE FOR MAXIMIZING                   
C THE FUNCTION                                                          
C                    T                                                  
C                   C X                                                 
C SUBJECT TO LINEAR INEQUALITY CONSTRAINTS ON THE VARIABLES             
C AND OPTIONAL UPPPER AND LOWER BOUND CONSTRAINTS ON THE                
C VARIABLES. THE                                                        
C ALGORITHM IS SIMPLEX-LIKE BUT INITIALLY PROJECTED GRADIENT            
C STEPS MAY BE TAKEN-I.E. ONE CAN CUT ACROSS THE FEASIBLE               
C REGION RATHER THAN VISITING VERTICES. THE QR DECOMPOSITION            
C OF THE BASIS MATRIX IS SAVED. NO ATTEMPT IS MADE TO UTILIZE           
C THE SPARSE STRUCTURE OF THE CONSTRAINTS EXCEPT FOR THE LOWER AND      
C UPPER BOUND CONSTRAINTS.                                              
C THE PARAMETERS HAVE THE FOLLOWING INTERPRETATION                      
C                                                                       
C A     THE MATRIX OF CONSTRAINTS WITH LEADING DIMENSION IA             
C M     NUMBER OF GENERAL EQUALITY AND INEQUALITY CONSTRAINTS           
C N     NUMBER OF UNKNOWNS                                              
C IA    ROW DIMENSION OF THE A MATRIX                                   
C B     INPUT VECTOR LENGTH M OF RIGHT HAND SIDE OF GENERAL CONSTRAINTS 
C C     INPUT VECTOR LENGTH N, COST VECTOR                              
C X     REAL VECTOR LENGTH N, ON INPUT APPROXIMATION TO THE SOLUTION,   
C       WHICH NEED NOT BE FEASIBLE, ON OUTPUT-THE SOLUTION              
C MAXITR MAXIMUM NUMBER OF ITERATIONS TOLERATED                         
C CTX   SCALAR ON OUPUT OF THE COST FUNCTION                            
C S     INTEGER INPUT SCALAR OF NUMBER OF SIMPLE CONSTRAINTS            
C SIMP  REAL INPUT VECTOR OF LENGTH S HAVING SIMPLE CONSTRAINTS         
C ISIMP INTEGER INPUT VECTOR TELLING THE ELEMENT OF X THE SIMPLE        
C       CONSTRAINT PERTAINS, IF NEGATIVE IT IS AN UPPER BOUND           
C       IF SIMP(1)=-5, SIMP(2)=10.0, ISIMP(1)=3, ISIMP(2)=-3            
C       THE SOLUTION MUST SATISFY X(3) .GE. -5.0,X(3) .LE. 10.0         
C E     INTEGER INPUT SCALAR GIVING THE NUMBER OF EQUALITY CONSTRAINTS  
C       IT IS ASSUMED THAT THE FIRST E CONSTRAINTS ARE EQUALITY CONS.   
C                                                                       
C STORAGE TAKEN FROM PORT STACK -3N*N/2+4M+11N/2 REAL LOCATIONS         
C      AND 2M+N+S INTEGER LOCATIONS                                     
       INTEGER ISIMP(1), M, E, S, N                                     
       EXTERNAL DLPMAN,DLPRNT                                           
       INTEGER MAXITR                                                   
       DOUBLE PRECISION A(IA,N),B(1),C(1),X(N),CTX,SIMP(1)              
       COMMON /CSTAK/ DSTAK                                             
       DOUBLE PRECISION DSTAK(500)                                      
       INTEGER ISTAK(1000),ISTKGT                                       
       DOUBLE PRECISION WS(500)                                         
       EQUIVALENCE (DSTAK(1),ISTAK(1))                                  
       EQUIVALENCE(DSTAK(1),WS(1))                                      
C/6S                                                                    
C      IF (N.LT.1)CALL SETERR(13HDLINPR-N.LT.1,13,1,2)                  
C      IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
C    1 20HDLINPR-M,S,OR E.LT.0,20,2,2)                                  
C      IF(E.GT.M.OR.E.GT.N.OR.S.GT.2*N)CALL SETERR(                     
C    1 34HDLINPR-E.GT.M.OR.E.GT.N.OR.S.GT.2N,34,3,2)                    
C      IF(M+S.LT.N.OR.IA.LT.M)CALL SETERR(                              
C    1 26HDLINPR-M+S.LT.N.OR IA.LT.M,26,4,2)                            
C      IF (MAXITR.LT.1)CALL SETERR(                                     
C    1 18HDLINPR-MAXITR.LT.1,18,5,2)                                    
C/7S                                                                    
       IF (N.LT.1)CALL SETERR('DLINPR-N.LT.1',13,1,2)                   
       IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
     1 'DLINPR-M,S,OR E.LT.0',20,2,2)                                   
       IF(E.GT.M.OR.S.GT.2*N)CALL SETERR(                               
     1 'DLINPR-E.GT.M.OR.S.GT.2N',24,3,2)                               
       IF(M+S.LT.N.OR.IA.LT.M)CALL SETERR(                              
     1 'DLINPR-M+S.LT.N.OR IA.LT.M',26,4,2)                             
       IF (MAXITR.LT.1)CALL SETERR(                                     
     1 'DLINPR-MAXITR.LT.1',18,5,2)                                     
C/                                                                      
       CALL ENTER(1)                                                    
       IU=ISTKGT(N,4)                                                   
       IPGPTR=ISTKGT(M,2)                                               
       CALL DLINPA(A,M,N,DLPMAN,IA,B,C,X,MAXITR,CTX,S,SIMP,ISIMP,E,     
     1 DLPRNT,IAG,IAS,ISTAK(IPGPTR),WS(IU))                             
      IF (NERROR(IERR).EQ.0)GO TO 10                                    
        CALL ERROFF                                                     
C/6S                                                                    
C     IF (IERR.EQ.6) CALL SETERR(                                       
C    133HDLINPR-NO.OF ITER. EXCEEDS MAXITR,33,6,1)                      
C     IF (IERR.EQ.7) CALL SETERR(                                       
C    125HDLINPR-UNBOUNDED SOLUTION,25,7,1)                              
C       IF (IERR.EQ.8)CALL SETERR(                                      
C    127HDLINPR-NO FEASIBLE SOLUTION,27,8,1)                            
C      IF (IERR.EQ.9)CALL SETERR(                                       
C    127HDLINPR-CONDITIONING PROBLEM,27,9,1)                            
C/7S                                                                    
      IF (IERR.EQ.6) CALL SETERR(                                       
     1'DLINPR-NO.OF ITER. EXCEEDS MAXITR',33,6,1)                       
      IF (IERR.EQ.7) CALL SETERR(                                       
     1'DLINPR-UNBOUNDED SOLUTION',25,7,1)                               
        IF (IERR.EQ.8)CALL SETERR(                                      
     1'DLINPR-NO FEASIBLE SOLUTION',27,8,1)                             
       IF (IERR.EQ.9)CALL SETERR(                                       
     1'DLINPR-CONDITIONING PROBLEM',27,9,1)                             
C/                                                                      
 10   CALL LEAVE                                                        
       RETURN                                                           
       END                                                              
       SUBROUTINE DLINPA(A,M,N,AMAN,IA,B,C,X,MAXITR,CTX,S,SIMP,         
     1   ISIMP,E, PRINT,IAG,IAS,IPTG,U)                                 
C THIS IS A LINEAR PROGRAMMING PACKAGE FOR MAXIMIZING                   
C THE FUNCTION                                                          
C                    T                                                  
C                   C X                                                 
C SUBJECT TO LINEAR INEQUALITY CONSTRAINTS ON THE VARIABLES             
C AND OPTIONAL UPPPER AND LOWER BOUND CONSTRAINTS ON THE                
C VARIABLES. THE USER PROVIDES A FUNCTION WHICH FOR A GIVEN             
C ROW RETURNS EITHER THAT ROW OR THE INNER PRODUCT OF THAT              
C ROW AND A SPECIFIED VECTOR. THUS IF THE CONSTRAINT MATRIX             
C IS SPARSE, THE USER MAY TAKE ADVANTAGE OF THIS. THE                   
C ALGORITHM IS SIMPLEX-LIKE BUT INITIALLY PROJECTED GRADIENT            
C STEPS MAY BE TAKEN-I.E. ONE CAN CUT ACROSS THE FEASIBLE               
C REGION RATHER THAN VISITING VERTICES. THE QR DECOMPOSITION            
C OF THE BASIS MATRIX IS SAVED. NO ATTEMPT IS MADE TO UTILIZE           
C THE SPARSE STRUCTURE OF THE CONSTRAINTS EXCEPT FOR THE LOWER AND      
C UPPER BOUND CONSTRAINTS.                                              
C THE PARAMETERS HAVE THE FOLLOWING INTERPRETATION                      
C                                                                       
C A     A SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION AMAN    
C M     NUMBER OF GENERAL EQUALITY AND INEQUALITY CONSTRAINTS           
C N     NUMBER OF UNKNOWNS                                              
C AMAN  USER PROVIDED FUNCTION WHICH EITHER RETURNS A ROW OF THE MATRIX 
C       OR DOES A VECTOR-VECTOR INNER PRODUCT WITH A PARTICULAR ROW.    
C       AMAN IS INVOKED AS FOLLOWS                                      
C       CALL AMAN(INNER,A,IA,N,IROW,P,T)                                
C       WHERE                                                           
C       INNER    INPUT LOGICAL VARIABLE, IF .TRUE., AMAN                
C                SHOULD RETURN IN T THE INNER PRODUCT OF P AND          
C                THE IROWTH ROW OF THE CONSTRAINT MATRIX                
C       A        REAL VECTOR PASSED THROUGH TO THE SUBROUTINE.          
C                IF THE USER DECIDES TO USE THE DEFAULT SUBROUTINE      
C                DLPMAN, A SHOULD CONTAIN THE CONSTRAINT MATRIX         
C                DIMENSIONED A(IA,N) IN THE MAIN PROGRAM.               
C       IA       INTEGER VECTOR PASSED THROUGH TO THE SUBROUTINE.       
C                IF THE USER DECIDES TO USE THE DEFAULT SUBROUTINE      
C                DLPMAN, IA IS JUST A SCALAR GIVING THE ROW DIMENSION   
C                OF THE CONSTRAINT MATRIX A                             
C       N        INPUT VARIABLE GIVING LENGTH OF P VECTOR               
C       IROW     THE IROWTH ROW IS EITHER TO BE PUT IN P OR INVOLVED    
C                IN THE INNER PRODUCT DEPENDING ON INNER                
C       P        IF INNER IS .TRUE., THEN P IS AN INPUT VECTOR OF       
C                LENGTH N INVOLVED IN THE INNER PRODUCT. IF INNER       
C                IS .FALSE., ON OUTPUT IT SHOULD CONTAIN THE IROWTH     
C                ROW OF A                                               
C       T        IF INNER IS .TRUE., ON OUTPUT, T=THE INNER             
C                PRODUCT OF P AND THE IROWTH ROW OF A                   
C                IF INNER .FALSE.,T NEED NOT BE TOUCHED                 
C IA    INTEGER SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION   
C       AMAN                                                            
C B     INPUT VECTOR LENGTH M OF RIGHT HAND SIDE OF GENERAL CONSTRAINTS 
C C     INPUT VECTOR LENGTH N, COST VECTOR                              
C X     REAL VECTOR LENGTH N, ON INPUT APPROXIMATION TO THE SOLUTION,   
C       WHICH NEED NOT BE FEASIBLE, ON OUTPUT-THE SOLUTION              
C MAXITR MAXIMUM NUMBER OF ITERATIONS TOLERATED                         
C CTX   SCALAR ON OUPUT OF THE COST FUNCTION                            
C S     INTEGER INPUT SCALAR OF NUMBER OF SIMPLE CONSTRAINTS            
C SIMP  REAL INPUT VECTOR OF LENGTH S HAVING SIMPLE CONSTRAINTS         
C ISIMP INTEGER INPUT VECTOR TELLING THE ELEMENT OF X THE SIMPLE        
C       CONSTRAINT PERTAINS, IF NEGATIVE IT IS AN UPPER BOUND           
C       IF SIMP(1)=-5, SIMP(2)=10.0, ISIMP(1)=3, ISIMP(2)=-3            
C       THE SOLUTION MUST SATISFY X(3) .GE. -5.0,X(3) .LE. 10.0         
C E     INTEGER INPUT SCALAR GIVING THE NUMBER OF EQUALITY CONSTRAINTS  
C       IT IS ASSUMED THAT THE FIRST E CONSTRAINTS ARE EQUALITY CONS.   
C PRINT USER WRITTEN SUBOURINTE WHICH PRINTS INFORMATION ABOUT LP       
C      EACH ITERATION. DEFAULT IS DLPRNT WHICH PRINTS NOTHING           
C                                                                       
C STORAGE TAKEN FROM PORT STACK -3N*N/2+4M+11N/2 REAL LOCATIONS         
C      AND 2M+N+S INTEGER LOCATIONS                                     
       INTEGER ISIMP(1), M, E, S, N                                     
       INTEGER TPTR                                                     
       INTEGER IPSPTR,DVSPTR,DVGPTR                                     
       EXTERNAL AMAN,PRINT                                              
       INTEGER MAXITR                                                   
       DOUBLE PRECISION A(1),B(1),C(1),X(N),CTX,SIMP(1),U(N)            
       INTEGER IA(1),IPTG(1)                                            
       COMMON /CSTAK/ DSTAK                                             
       DOUBLE PRECISION DSTAK(500)                                      
       INTEGER WPTR,QPTR,LTPTR,PPTR,VPTR,SCLPTR                         
       INTEGER ISTAK(1000),ISTKGT                                       
       DOUBLE PRECISION WS(500)                                         
       EQUIVALENCE (DSTAK(1),ISTAK(1))                                  
       EQUIVALENCE(DSTAK(1),WS(1))                                      
C/6S                                                                    
C      IF (N.LT.1)CALL SETERR(13HDLINPA-N.LT.1,13,1,2)                  
C      IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
C    1 20HDLINPA-M,S,OR E.LT.0,20,2,2)                                  
C      IF(E.GT.M.OR.E.GT.N.OR.S.GT.2*N)CALL SETERR(                     
C    1 34HDLINPA-E.GT.M.OR.E.GT.N.OR.S.GT.2N,34,3,2)                    
C      IF(M+S.LT.N)CALL SETERR(                                         
C    1 15HDLINPA-M+S.LT.N,15,4,2)                                       
C      IF (MAXITR.LT.1)CALL SETERR(                                     
C    1 18HDLINPA-MAXITR.LT.1,18,5,2)                                    
C/7S                                                                    
       IF (N.LT.1)CALL SETERR('DLINPA-N.LT.1',13,1,2)                   
       IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
     1 'DLINPA-M,S,OR E.LT.0',20,2,2)                                   
       IF(E.GT.M.OR.S.GT.2*N)CALL SETERR(                               
     1 'DLINPA-E.GT.M.OR.S.GT.2N',24,3,2)                               
       IF(M+S.LT.N)CALL SETERR(                                         
     1 'DLINPA-M+S.LT.N',15,4,2)                                        
       IF (MAXITR.LT.1)CALL SETERR(                                     
     1 'DLINPA-MAXITR.LT.1',18,5,2)                                     
C/                                                                      
       CALL ENTER(1)                                                    
       WPTR = ISTKGT(M,4)                                               
       QPTR = ISTKGT(N**2,4)                                            
       LTI=(N*(N+1))/2                                                  
       LTPTR = ISTKGT(LTI, 4)                                           
       PPTR =ISTKGT(N, 4)                                               
       VPTR = ISTKGT(M, 4)                                              
       SCLPTR = ISTKGT(M, 4)                                            
       IPSPTR = ISTKGT(N, 2)                                            
       DVSPTR=1                                                         
       IF (S.GT.0)DVSPTR =ISTKGT(S, 2)                                  
       DVGPTR = ISTKGT(M, 2)                                            
       TPTR = ISTKGT(N, 4)                                              
       IRHS = ISTKGT(N,4)                                               
       ICCPTR = ISTKGT(N,4)                                             
       IQ=N                                                             
       CALL DI4NTL(N,WS(QPTR),IQ,IPTG,M,ISTAK(IPSPTR),                  
     1 A,IA,AMAN,WS(SCLPTR),WS(TPTR))                                   
       CALL DL4PH1(A,M,N,AMAN,IA,B,X,MAXITR,CTX,S,SIMP,ISIMP,E,         
     1   WS(WPTR),WS(QPTR),IQ,WS(LTPTR),WS(PPTR),WS(VPTR),WS(SCLPTR),   
     1   ISTAK(IPSPTR),IPTG,ISTAK(DVSPTR),ISTAK(DVGPTR),                
     1   WS(TPTR),U,PRINT,WS(IRHS),IAS,IAG,KK,WS(ICCPTR),IERR)          
        IF (IERR.EQ.0)GO TO 10                                          
        CALL ERROFF                                                     
C/6S                                                                    
C     IF (IERR.EQ.6) CALL SETERR(                                       
C    133HDLINPA-NO.OF IERR. EXCEEDS MAXITR,33,6,1)                      
C     IF (IERR.EQ.7) CALL SETERR(                                       
C    125HDLINPA-UNBOUNDED SOLUTION,25,7,1)                              
C       IF (IERR.EQ.8)CALL SETERR(                                      
C    127HDLINPA-NO FEASIBLE SOLUTION,27,8,1)                            
C      IF (IERR.EQ.9)CALL SETERR(                                       
C    127HDLINPA-CONDITIONING PROBLEM,27,9,1)                            
C/7S                                                                    
      IF (IERR.EQ.6) CALL SETERR(                                       
     1'DLINPA-NO.OF IERR. EXCEEDS MAXITR',33,6,1)                       
      IF (IERR.EQ.7) CALL SETERR(                                       
     1'DLINPA-UNBOUNDED SOLUTION',25,7,1)                               
        IF (IERR.EQ.8)CALL SETERR(                                      
     1'DLINPA-NO FEASIBLE SOLUTION',27,8,1)                             
       IF (IERR.EQ.9)CALL SETERR(                                       
     1'DLINPA-CONDITIONING PROBLEM',27,9,1)                             
C/                                                                      
         GO TO 20                                                       
 10     CONTINUE                                                        
       CALL DL4P2(A,M,N,AMAN,IA,B,C,X,MAXITR,CTX,S,SIMP,ISIMP,E,        
     1   WS(WPTR),WS(QPTR),IQ,WS(LTPTR),WS(PPTR),WS(VPTR),WS(SCLPTR),   
     1   ISTAK(IPSPTR),IPTG,ISTAK(DVSPTR),ISTAK(DVGPTR),                
     1   WS(TPTR),U,PRINT,WS(IRHS),IAS,IAG,KK,IER2)                     
        IF (IER2.EQ.0)GO TO 20                                          
        CALL ERROFF                                                     
C/6S                                                                    
C     IF (IER2.EQ.6) CALL SETERR(                                       
C    133HDLINPA-NO.OF ITER. EXCEEDS MAXITR,33,6,1)                      
C     IF (IER2.EQ.7) CALL SETERR(                                       
C    125HDLINPA-UNBOUNDED SOLUTION,25,7,1)                              
C       IF (IER2.EQ.8)CALL SETERR(                                      
C    127HDLINPA-NO FEASIBLE SOLUTION,27,8,1)                            
C      IF (IER2.EQ.9)CALL SETERR(                                       
C    127HDLINPA-CONDITIONING PROBLEM,27,9,1)                            
C/7S                                                                    
      IF (IER2.EQ.6) CALL SETERR(                                       
     1'DLINPA-NO.OF ITER. EXCEEDS MAXITR',33,6,1)                       
      IF (IER2.EQ.7) CALL SETERR(                                       
     1'DLINPA-UNBOUNDED SOLUTION',25,7,1)                               
        IF (IER2.EQ.8)CALL SETERR(                                      
     1'DLINPA-NO FEASIBLE SOLUTION',27,8,1)                             
       IF (IER2.EQ.9)CALL SETERR(                                       
     1'DLINPA-CONDITIONING PROBLEM',27,9,1)                             
C/                                                                      
 20    CALL LEAVE                                                       
       RETURN                                                           
       END                                                              
       SUBROUTINE  DFEAS(A,M,N,IA,B,X,MAXITR,S,SIMP,                    
     1   ISIMP,E)                                                       
C THIS SUBROUTINE DETERMINES IF POSSIBLE WHETHER A POINT                
C SATISFIES A SYSTEM OF LINEAR INEQUALITY AND EQUALITY                  
C CONSTRAINTS AND OPTIONAL LOWER AND UPPER BOUND CONSTRAINTS            
C ON THE VARIABLES. THE                                                 
C ALGORITHM IS SIMPLEX-LIKE BUT INITIALLY PROJECTED GRADIENT            
C STEPS MAY BE TAKEN-I.E. ONE CAN CUT ACROSS THE  FEASIBLE              
C REGION RATHER THAN VISITING VERTICES. THE QR DECOMPOSITION            
C OF THE BASIS MATRIX IS SAVED. NO ATTEMPT IS MADE TO UTILIZE           
C THE SPARSE STRUCTURE OF THE CONSTRAINTS EXCEPT FOR THE LOWER AND      
C UPPER BOUND CONSTRAINTS.                                              
C THE PARAMETERS HAVE THE FOLLOWING INTERPRETATION                      
C                                                                       
C A     THE MATRIX OF CONSTRAINTS WITH LEADING DIMENSION IA             
C M     NUMBER OF GENERAL EQUALITY AND INEQUALITY CONSTRAINTS           
C N     NUMBER OF UNKNOWNS                                              
C IA    ROW DIMENSION OF THE A MATRIX                                   
C B     INPUT VECTOR LENGTH M OF RIGHT HAND SIDE OF GENERAL CONSTRAINTS 
C X     REAL VECTOR LENGTH N, ON INPUT APPROXIMATION TO THE SOLUTION,   
C       WHICH NEED NOT BE  FEASIBLE, ON OUTPUT-THE SOLUTION             
C MAXITR MAXIMUM NUMBER OF ITERATIONS TOLERATED                         
C S     INTEGER INPUT SCALAR OF NUMBER OF SIMPLE CONSTRAINTS            
C SIMP  REAL INPUT VECTOR OF LENGTH S HAVING SIMPLE CONSTRAINTS         
C ISIMP INTEGER INPUT VECTOR TELLING THE ELEMENT OF X THE SIMPLE        
C       CONSTRAINT PERTAINS, IF NEGATIVE IT IS AN UPPER BOUND           
C       IF SIMP(1)=-5, SIMP(2)=10.0D0, ISIMP(1)=3, ISIMP(2)=-3          
C       THE SOLUTION MUST SATISFY X(3) .GE. -5.0,X(3) .LE. 10.0D0       
C E     INTEGER INPUT SCALAR GIVING THE NUMBER OF EQUALITY CONSTRAINTS  
C       IT IS ASSUMED THAT THE FIRST E CONSTRAINTS ARE EQUALITY CONS.   
C                                                                       
C STORAGE TAKEN FROM PORT STACK -3N*N/2+4M+11N/2 REAL LOCATIONS         
C      AND 2M+N+S INTEGER LOCATIONS                                     
       INTEGER ISIMP(1), M, E, S, N                                     
       EXTERNAL DLPMAN,DLPRNT                                           
       INTEGER MAXITR                                                   
       DOUBLE PRECISION A(IA,N),B(1),X(N),SIMP(1)                       
       COMMON /CSTAK/ DSTAK                                             
       DOUBLE PRECISION DSTAK(500)                                      
       INTEGER ISTAK(1000),ISTKGT                                       
       DOUBLE PRECISION WS(500)                                         
       EQUIVALENCE (DSTAK(1),ISTAK(1))                                  
       EQUIVALENCE(DSTAK(1),WS(1))                                      
C/6S                                                                    
C      IF (N.LT.1)CALL SETERR(13H DFEAS-N.LT.1,13,1,2)                  
C      IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
C    1 20H DFEAS-M,S,OR E.LT.0,20,2,2)                                  
C      IF(E.GT.M.OR.S.GT.2*N)CALL SETERR(                               
C    1 24H DFEAS-E.GT.M.OR.S.GT.2N,24,3,2)                              
C      IF (IA.LT.M)CALL SETERR(14HDFEAS -IA.LT.M,14,4,2)                
C      IF (MAXITR.LT.1)CALL SETERR(                                     
C    1 18H DFEAS-MAXITR.LT.1,18,5,2)                                    
C/7S                                                                    
       IF (N.LT.1)CALL SETERR(' DFEAS-N.LT.1',13,1,2)                   
       IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
     1 ' DFEAS-M,S,OR E.LT.0',20,2,2)                                   
       IF(E.GT.M.OR.S.GT.2*N)CALL SETERR(                               
     1 ' DFEAS-E.GT.M.OR.S.GT.2N',24,3,2)                               
       IF (IA.LT.M)CALL SETERR('DFEAS -IA.LT.M',14,4,2)                 
       IF (MAXITR.LT.1)CALL SETERR(                                     
     1 ' DFEAS-MAXITR.LT.1',18,5,2)                                     
C/                                                                      
       CALL ENTER(1)                                                    
       IPGPTR=ISTKGT(M,2)                                               
       CALL DFEASA(A,M,N,DLPMAN,IA,B,X,MAXITR,S,SIMP,ISIMP,E,           
     1 DLPRNT,IAG,IAS,ISTAK(IPGPTR))                                    
      IF (NERROR(IERR).EQ.0)GO TO 10                                    
        CALL ERROFF                                                     
C/6S                                                                    
C     IF (IERR.EQ.6) CALL SETERR(                                       
C    133H DFEAS-NO.OF ITER. EXCEEDS MAXITR,33,6,1)                      
C     IF (IERR.EQ.7) CALL SETERR(                                       
C    125H DFEAS-UNBOUNDED SOLUTION,25,7,1)                              
C       IF (IERR.EQ.8)CALL SETERR(                                      
C    127HDFEAS-NO  FEASIBLE SOLUTION,27,8,1)                            
C      IF (IERR.EQ.9)CALL SETERR(                                       
C    127H DFEAS-CONDITIONING PROBLEM,27,9,1)                            
C/7S                                                                    
      IF (IERR.EQ.6) CALL SETERR(                                       
     1' DFEAS-NO.OF ITER. EXCEEDS MAXITR',33,6,1)                       
      IF (IERR.EQ.7) CALL SETERR(                                       
     1' DFEAS-UNBOUNDED SOLUTION',25,7,1)                               
        IF (IERR.EQ.8)CALL SETERR(                                      
     1'DFEAS-NO  FEASIBLE SOLUTION',27,8,1)                             
       IF (IERR.EQ.9)CALL SETERR(                                       
     1' DFEAS-CONDITIONING PROBLEM',27,9,1)                             
C/                                                                      
 10   CALL LEAVE                                                        
       RETURN                                                           
       END                                                              
       SUBROUTINE DFEASA(A,M,N,AMAN,IA,B,X,MAXITR,S,SIMP,               
     1   ISIMP,E, PRINT,IAG,IAS,IPTG)                                   
C THIS SUBROUTINE DETERMINES IF POSSIBLE A POINT WHICH SATISFIES        
C A SYSTEM OF LINEAR INEQUALITY AND EQUALITY CONSTRAINTS                
C AND OPTIONAL UPPPER AND LOWER BOUND CONSTRAINTS ON THE                
C VARIABLES. THE USER PROVIDES A FUNCTION WHICH FOR A GIVEN             
C ROW RETURNS EITHER THAT ROW OR THE INNER PRODUCT OF THAT              
C ROW AND A SPECIFIED VECTOR. THUS IF THE CONSTRAINT MATRIX             
C IS SPARSE, THE USER MAY TAKE ADVANTAGE OF THIS. THE                   
C ALGORITHM IS SIMPLEX-LIKE BUT INITIALLY PROJECTED GRADIENT            
C STEPS MAY BE TAKEN-I.E. ONE CAN CUT ACROSS THE FEASIBLE               
C REGION RATHER THAN VISITING VERTICES. THE QR DECOMPOSITION            
C OF THE BASIS MATRIX IS SAVED. NO ATTEMPT IS MADE TO UTILIZE           
C THE SPARSE STRUCTURE OF THE CONSTRAINTS EXCEPT FOR THE LOWER AND      
C UPPER BOUND CONSTRAINTS.                                              
C THE PARAMETERS HAVE THE FOLLOWING INTERPRETATION                      
C                                                                       
C A     A SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION AMAN    
C M     NUMBER OF GENERAL EQUALITY AND INEQUALITY CONSTRAINTS           
C N     NUMBER OF UNKNOWNS                                              
C AMAN  USER PROVIDED FUNCTION WHICH EITHER RETURNS A ROW OF THE MATRIX 
C       OR DOES A VECTOR-VECTOR INNER PRODUCT WITH A PARTICULAR ROW.    
C       AMAN IS INVOKED AS FOLLOWS                                      
C       CALL AMAN(INNER,A,IA,N,IROW,P,T)                                
C       WHERE                                                           
C       INNER    INPUT LOGICAL VARIABLE, IF .TRUE., AMAN                
C                SHOULD RETURN IN T THE INNER PRODUCT OF P AND          
C                THE IROWTH ROW OF THE CONSTRAINT MATRIX                
C       A        REAL VECTOR PASSED THROUGH TO THE SUBROUTINE.          
C                IF THE USER DECIDES TO USE THE DEFAULT SUBROUTINE      
C                DLPMAN, A SHOULD CONTAIN THE CONSTRAINT MATRIX         
C                DIMENSIONED A(IA,N) IN THE MAIN PROGRAM.               
C       IA       INTEGER VECTOR PASSED THROUGH TO THE SUBROUTINE.       
C                IF THE USER DECIDES TO USE THE DEFAULT SUBROUTINE      
C                DLPMAN, IA IS JUST A SCALAR GIVING THE ROW DIMENSION   
C                OF THE CONSTRAINT MATRIX A                             
C       N        INPUT VARIABLE GIVING LENGTH OF P VECTOR               
C       IROW     THE IROWTH ROW IS EITHER TO BE PUT IN P OR INVOLVED    
C                IN THE INNER PRODUCT DEPENDING ON INNER                
C       P        IF INNER IS .TRUE., THEN P IS AN INPUT VECTOR OF       
C                LENGTH N INVOLVED IN THE INNER PRODUCT. IF INNER       
C                IS .FALSE., ON OUTPUT IT SHOULD CONTAIN THE IROWTH     
C                ROW OF A                                               
C       T        IF INNER IS .TRUE., ON OUTPUT, T=THE INNER             
C                PRODUCT OF P AND THE IROWTH ROW OF A                   
C                IF INNER .FALSE.,T NEED NOT BE TOUCHED                 
C IA    INTEGER SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION   
C       AMAN                                                            
C B     INPUT VECTOR LENGTH M OF RIGHT HAND SIDE OF GENERAL CONSTRAINTS 
C X     REAL VECTOR LENGTH N, ON INPUT APPROXIMATION TO THE SOLUTION,   
C       WHICH NEED NOT BE FEASIBLE, ON OUTPUT-THE SOLUTION              
C MAXITR MAXIMUM NUMBER OF ITERATIONS TOLERATED                         
C S     INTEGER INPUT SCALAR OF NUMBER OF SIMPLE CONSTRAINTS            
C SIMP  REAL INPUT VECTOR OF LENGTH S HAVING SIMPLE CONSTRAINTS         
C ISIMP INTEGER INPUT VECTOR TELLING THE ELEMENT OF X THE SIMPLE        
C       CONSTRAINT PERTAINS, IF NEGATIVE IT IS AN UPPER BOUND           
C       IF SIMP(1)=-5, SIMP(2)=10.0D0, ISIMP(1)=3, ISIMP(2)=-3          
C       THE SOLUTION MUST SATISFY X(3) .GE. -5.0,X(3) .LE. 10.0D0       
C E     INTEGER INPUT SCALAR GIVING THE NUMBER OF EQUALITY CONSTRAINTS  
C       IT IS ASSUMED THAT THE FIRST E CONSTRAINTS ARE EQUALITY CONS.   
C PRINT USER WRITTEN SUBOURINTE WHICH PRINTS INFORMATION ABOUT LP       
C      EACH ITERATION. DEFAULT IS DLPRNT WHICH PRINTS NOTHING           
C                                                                       
C STORAGE TAKEN FROM PORT STACK -3N*N/2+4M+11N/2 REAL LOCATIONS         
C      AND 2M+N+S INTEGER LOCATIONS                                     
       INTEGER ISIMP(1), M, E, S, N                                     
       INTEGER TPTR                                                     
       INTEGER IPSPTR,DVSPTR,DVGPTR                                     
       EXTERNAL AMAN,PRINT                                              
       INTEGER MAXITR                                                   
       DOUBLE PRECISION A(1),B(1),X(N),CTX,SIMP(1)                      
       INTEGER IA(1),IPTG(1)                                            
       COMMON /CSTAK/ DSTAK                                             
       DOUBLE PRECISION DSTAK(500)                                      
       INTEGER WPTR,QPTR,LTPTR,PPTR,VPTR,SCLPTR                         
       INTEGER ISTAK(1000),ISTKGT                                       
       DOUBLE PRECISION WS(500)                                         
       EQUIVALENCE (DSTAK(1),ISTAK(1))                                  
       EQUIVALENCE(DSTAK(1),WS(1))                                      
C/6S                                                                    
C      IF (N.LT.1)CALL SETERR(13HDFEASA-N.LT.1,13,1,2)                  
C      IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
C    1 20HDFEASA-M,S,OR E.LT.0,20,2,2)                                  
C      IF(E.GT.M.OR.S.GT.2*N)CALL SETERR(                               
C    1 24HDFEASA-E.GT.M.OR.S.GT.2N,24,3,2)                              
C      IF (MAXITR.LT.1)CALL SETERR(                                     
C    1 18HDFEASA-MAXITR.LT.1,18,4,2)                                    
C/7S                                                                    
       IF (N.LT.1)CALL SETERR('DFEASA-N.LT.1',13,1,2)                   
       IF (M.LT.0.OR.S.LT.0.OR.E.LT.0)CALL SETERR(                      
     1 'DFEASA-M,S,OR E.LT.0',20,2,2)                                   
       IF(E.GT.M.OR.S.GT.2*N)CALL SETERR(                               
     1 'DFEASA-E.GT.M.OR.S.GT.2N',24,3,2)                               
       IF (MAXITR.LT.1)CALL SETERR(                                     
     1 'DFEASA-MAXITR.LT.1',18,4,2)                                     
C/                                                                      
       CALL ENTER(1)                                                    
       WPTR = ISTKGT(M,4)                                               
       IUPTR = ISTKGT(N,4)                                              
       QPTR = ISTKGT(N**2,4)                                            
       LTI=(N*(N+1))/2                                                  
       LTPTR = ISTKGT(LTI, 4)                                           
       PPTR =ISTKGT(N, 4)                                               
       VPTR = ISTKGT(M, 4)                                              
       SCLPTR = ISTKGT(M, 4)                                            
       IPSPTR = ISTKGT(N, 2)                                            
       DVSPTR=1                                                         
       IF (S.GT.0)DVSPTR =ISTKGT(S, 2)                                  
       DVGPTR = ISTKGT(M, 2)                                            
       TPTR = ISTKGT(N, 4)                                              
       IRHS = ISTKGT(N,4)                                               
       ICCPTR = ISTKGT(N,4)                                             
       IQ=N                                                             
       CALL DI4NTL(N,WS(QPTR),IQ,IPTG,M,ISTAK(IPSPTR),                  
     1 A,IA,AMAN,WS(SCLPTR),WS(TPTR))                                   
       CALL DL4PH1(A,M,N,AMAN,IA,B,X,MAXITR,CTX,S,SIMP,ISIMP,E,         
     1   WS(WPTR),WS(QPTR),IQ,WS(LTPTR),WS(PPTR),WS(VPTR),WS(SCLPTR),   
     1   ISTAK(IPSPTR),IPTG,ISTAK(DVSPTR),ISTAK(DVGPTR),                
     1   WS(TPTR),WS(IUPTR),PRINT,WS(IRHS),IAS,IAG,KK,WS(ICCPTR),IERR)  
        IF (IERR.EQ.0)GO TO 10                                          
        CALL ERROFF                                                     
C/6S                                                                    
C     IF (IERR.EQ.6) CALL SETERR(                                       
C    133HDFEASA-NO.OF ITER. EXCEEDS MAXITR,33,6,1)                      
C       IF (IERR.EQ.8)CALL SETERR(                                      
C    127HDFEASA-NO FEASIBLE SOLUTION,27,8,1)                            
C      IF (IERR.EQ.9)CALL SETERR(                                       
C    127HDFEASA-CONDITIONING PROBLEM,27,9,1)                            
C/7S                                                                    
      IF (IERR.EQ.6) CALL SETERR(                                       
     1'DFEASA-NO.OF ITER. EXCEEDS MAXITR',33,6,1)                       
        IF (IERR.EQ.8)CALL SETERR(                                      
     1'DFEASA-NO FEASIBLE SOLUTION',27,8,1)                             
       IF (IERR.EQ.9)CALL SETERR(                                       
     1'DFEASA-CONDITIONING PROBLEM',27,9,1)                             
C/                                                                      
 10     CONTINUE                                                        
 20    CALL LEAVE                                                       
       RETURN                                                           
       END                                                              
      SUBROUTINE DL4P2(A, M, N, AMAN, IA, B, C, X, ITRMAX, CTX, S, SIMP,
     1  ISIMP, E, W, Q,IQ, LT, P, V, SCALE, IPTS, IPTG, DVECS, DVECG    
     1   ,TT,U1,PRINT,RHS,AS,AG,KK,IER2)                                
C                                                                       
C THIS IS PHASE2 OF THE LINEAR PROGRAMMING PACKAGE                      
C                                                                       
C THE PARAMETERS HAVE THE FOLLOWING INTERPRETATION                      
C                                                                       
C A     A SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION AMAN    
C M     NUMBER OF GNERAL EQUALITY AND INEQUALITY CONSTRAINTS            
C N     NUMBER OF UNKNOWNS                                              
C AMAN  USER PROVIDED FUNCTION WHICH EITHER RETURNS A ROW OF THE MATRIX 
C       OR DOES A VECTOR-VECTOR INNER PRODUCT WITH A PARTICULAR ROW.    
C B     INPUT VECTOR LENGTH M OF RIGHT HAND SIDE OF GNERAL CONSTRAINTS  
C C     INPUT VECTOR LENGTH N, COST VECTOR                              
C X     VECTOR LENGTH N, FEASIBLE VECTOR ON INPUT, SOLUTION ON OUTPUT   
C ITRMAX MAXIMUM NUMBER OF ITERATIONS TOLERATED                         
C CTX   SCALAR ON OUPUT OF THE COST FUNCTION                            
C S     INTEGER INPUT SCALAR OF NUMBER OF SIMPLE CONSTRAINTS            
C SIMP  REAL INPUT VECTOR OF LENGTH S HAVING SIMPLE CONSTRAINTS         
C ISIMP INTEGER INPUT VECTOR TELLING THE ELEMENT OF X THE SIMPLE        
C       CONSTRAINT PERTAINS, IF NEGATIVE IT IS AN UPPER BOUND           
C E     INTEGER INPUT SCALAR GIVING THE NUMBER OF EQUALITY CONSTRAINTS  
C W     M VECTOR OF RESIDUALS                                           
C Q     AN N X N ARRAY STORING THE Q FACTOR OF THE LQ FACTORIZATION     
C       OF THE ACTIVE CONSTRAINT MATRIX                                 
C LT    AN N X N REAL ARRAY STORING THE TRANSPOSE OF L IN THE QL FAC-   
C       TORIZATION                                                      
C P     SCRATCH VECTOR, LENGTH N, WHICH WILL CONTAIN THE SEARCH         
C       DIRECTION                                                       
C V     SCRATCH VECTOR, LENGTH M, WHICH WILL ONTAIN INACTIVE CONSTRAINT 
C       MATRIX TIMES P                                                  
C SCALE INPUT VECTOR, LENGTH M, CONTAINING NORMS OF CONSTRAINT ROWS     
C IPTG SCRATCH VECTOR POINTING TO GENERAL CONSTRAINTS                   
C DVECS SCRATCH VECTOR STATING WHETHER SIMPLE CONSTAINT HAS BEEN        
C       DROPPED-TO PREVENT CYCLING                                      
C DVECG SCRATCH VECTOR STATING WHETHER GENERAL CONSTRAINT HAS           
C       BEEN DROPPED-TO PREVENT CYCLING                                 
C TT    PURE SCRATCH VECTOR                                             
C U1    SCRATCH VECTOR TO STORE LAGRANGE MULTIPLIERS                    
      EXTERNAL AMAN,PRINT                                               
      LOGICAL IEND                                                      
      INTEGER ITRMAX,ISIMP(1)                                           
      DOUBLE PRECISION A(1), B(1), C(1), X(N), CTX, W(1)                
       DOUBLE PRECISION TT(N),RHS(N)                                    
      DOUBLE PRECISION Q(IQ, N), LT( N), P(N), V(1), SCALE(1)           
      INTEGER NMS, AGP1,  E, ASP1                                       
      INTEGER I, K, AGPE, FLAG,  IPTG(1)                                
      INTEGER IPTS(N), INDX2, AG, II, AS, DVECG(1)                      
      INTEGER KK, DVECS(1),  ITEMP, ITRPH2, IA(1),S                     
      DOUBLE PRECISION EPS,  CNRM, TOLL, UMAX,DDOT                      
      DOUBLE PRECISION PNRM, SIMP(1), TOLU, XNRM, U1(1)                 
      DOUBLE PRECISION  DNRM2,  THETA, DFLOAT                           
      DOUBLE PRECISION CTEMP,  D1MACH                                   
      INTEGER ISAR(1000)                                                
      COMMON /CSTAK/ ISAR                                               
      DOUBLE PRECISION BOUND                                            
      BOUND=100*D1MACH(4)*DNRM2(N,C,1)                                  
       CALL ENTER(0)                                                    
       MAXMN=MAX0(M,N)                                                  
       MAXMS=MAX0(M,S)                                                  
       IIHI=ISTKGT(MAXMN, 2)                                            
       INVHIT=1                                                         
       IF(MAXMS.GT.0)INVHIT=ISTKGT(MAXMS, 2)                            
      IER2=0                                                            
      IEND= .FALSE.                                                     
      INDX2=0                                                           
      EPS = D1MACH(4)*DMAX1(DFLOAT(N),10.0D0)                           
      TOLL = 1. + D1MACH(4)*10.0D0                                      
      TOLU = 1. - D1MACH(4)*10.0D0                                      
      XNRM = DNRM2(N, X, 1)                                             
      CNRM = DNRM2(N, C, 1)                                             
      IF (M .EQ. 0) GOTO 20                                             
      DO  10 II = 1, M                                                  
         V(II)=0.0D0                                                    
         DVECG(II) = 0                                                  
 10      CONTINUE                                                       
 20      CONTINUE                                                       
      DO  25 I=1,N                                                      
         P(I)=0.0D0                                                     
 25   CONTINUE                                                          
       IF (S .EQ. 0) GOTO 40                                            
       DO 30 II=1,S                                                     
          DVECS(II) = 0                                                 
 30       CONTINUE                                                      
 40    CONTINUE                                                         
            ASP1 = AS + 1                                               
            JDEPS=AS                                                    
            DO 50 II = ASP1,N                                           
               IK = IPTS(II)                                            
               IIAS = II - AS                                           
               RHS(IIAS) = C(IK)                                        
 50         CONTINUE                                                    
            NMS=N-AS                                                    
C           IF(AG+E.GT.0)CALL DM5TOP(IQ,N,Q,1,NMS,1,NMS,RHS,1,RHS)      
            CALL DM5TOP(IQ,N,Q,1,NMS,1,NMS,RHS,1,RHS)                   
      CTX=DDOT(N,C,1,X,1)                                               
      ITRPH2 = 1                                                        
      JDEPG=AG+E                                                        
         GOTO  70                                                       
 60      ITRPH2 = ITRPH2+1                                              
 70      IF (ITRPH2 .GT. ITRMAX) GOTO  200                              
       MMAG = M - AG - E                                                
           AGPE=AG+E                                                    
         KKK=KK                                                         
C                                                                       
C CALL SUBROUTINE TO TEST IF SIMPLE CONSTRAINTS SHOULD BE               
C ADDED AND THEN ADD THEM AND UPDATE LQ DECOMPOSTION                    
C                                                                       
         NMS=N-AS                                                       
         IF (ITRPH2. EQ .1) GO TO 75                                    
         IF (NMS .NE. 0) CALL DA4PPS(A, M, N, IA, KK,S, Q,IQ, LT, AS,   
     1      AG, E, IPTS, DVECS, X, SIMP, ISIMP, TOLL, TOLU, EPS, TT,    
     2      IPRINT,RHS,INDX2,P,2,JDEPS,ISAR(IIHI),ISAR(INVHIT),0)       
C                                                                       
C CALL SUBROUTINE TO TEST IF GENERAL CONSTRAINTS SHOULD BE              
C ADDED AND THEN ADD THEM AND UPDATE LQ DECOMPOSITION                   
C                                                                       
         IF (MMAG .NE. 0) CALL DA4PPG(A, M, N, IA, KK, Q,IQ, LT, AG,    
     1      AS,E,IPTG,IPTS,W,SCALE,XNRM,AMAN,EPS,TT,P,IPRINT,RHS,       
     2      INDX2,V, JDEPG,ISAR(IIHI),ISAR(INVHIT),0,DVECG)             
 75         AGPE = AG + E                                               
C COMPUTE LAGRANGE MULTIPLIERS                                          
C                                                                       
            IF (AGPE .EQ. 0) GOTO 80                                    
C                                                                       
C COMPUTE LAGRANGE MULTIPLIERS FOR GENERAL CONSTRAINTS                  
C                                                                       
            ASP1 = AS + 1                                               
          CALL DM4TOP(AGPE,LT,RHS,U1)                                   
 80         CONTINUE                                                    
            AGP1=AG+E+1                                                 
C                                                                       
C COMPUTE LAGRANGE MULTIPLIERS FOR SIMPLE CONSTRAINTS                   
C                                                                       
            IF (AS .NE. 0) CALL DL4AGS(A,M,N,IA,AMAN,AS,U1,U1(AGP1),C,  
     1           ISIMP,AGPE,IPTS,IPTG,TT)                               
C                                                                       
C DETERMINE WHICH CONSTRAINT TO DROP GIVEN THE LAGRANGE MULTIPLIERS     
C                                                                       
       CALL PRINT(A,M,N,AMAN,IA,B,C,X,CTX,S,SIMP,ISIMP,E,ITRPH2,        
     1 IPTG,AG,AS,U1,IEND,2)                                            
       IF (IEND)GO TO 210                                               
         FLAG=1                                                         
         IF (E .GE. KK) GOTO 130                                        
C                                                                       
 90         CALL DD4CLM(U1, AG, AS, E, UMAX, INDX2, FLAG, DVECS, DVECG, 
     1         IPTG, IPTS,BOUND,2,KK,N)                                 
              AGPE=AG+E                                                 
            IF (FLAG .NE. 1) GOTO 100                                   
            IF (KK .EQ. N) GO TO 210                                    
            INDX2=0                                                     
            GOTO 130                                                    
 100   CONTINUE                                                         
       IF (INDX2 .LE. AGPE) GOTO 110                                    
C                                                                       
C SINCE A SIMPLE CONSTRAINT IS TO BE DROPPED, UPDATE APPROPRIATE        
C ARRAYS AND THE LQ DECOMPOSITION ACCORDINGLY                           
C                                                                       
          IND2=INDX2-AGPE                                               
          ITEMP=IPTS(IND2)                                              
           NMS=N-AS                                                     
          RHS(NMS+1)=C(ITEMP)                                           
          IF (AS .NE. 0) CALL DD4RPS(A,M,N,IA,AMAN,KK,Q,IQ,LT,AG,AS,E,  
     1   IPTG,IPTS,DVECS,SIMP,ISIMP,INDX2,TT,P, RHS)                    
          INDX2=1                                                       
          GOTO 120                                                      
 110     CONTINUE                                                       
C                                                                       
C SINCE A GENERAL CONSTRAINT IS TO BE DROPPED, UPDATE APPROPRIATE       
C ARRAYS AND LQ DECOMPOSITION ACCORDINGLY                               
C                                                                       
       IF (AG .NE. 0) CALL DD4RPG (M,N,KK,Q,IQ,LT,AG,AS,E,IPTG,INDX2    
     1                ,DVECG,RHS,C,IPTS)                                
       INDX2=-1                                                         
 120   CONTINUE                                                         
       JDEPG=AG+E                                                       
       JDEPS=AS                                                         
 130   CONTINUE                                                         
C                                                                       
C COMPUTE NEW SEARCH DIRECTION                                          
C                                                                       
         CALL DP4RJD(N, AS, AG, E, Q,IQ, IPTS, C, P, TT,RHS)            
         IF (JDEPS.EQ.AS) GO TO 132                                     
         ASP1=AS+1                                                      
         DO 131 I=ASP1,JDEPS                                            
            I2=ISIMP(I)                                                 
 131      P(I2)=0.0D0                                                   
 132      CONTINUE                                                      
         PNRM = DNRM2(N, P, 1)                                          
C                                                                       
C CHECK IF FINISHED                                                     
C                                                                       
         IF (KK.EQ.N)GO TO 210                                          
         IF (PNRM.LT.EPS*CNRM.AND.FLAG.NE.1)GO TO 80                    
         IF (PNRM.LT.EPS*CNRM) GO TO 210                                
C                                                                       
C DETERMINE HOW FAR ONE SHOULD PROCEED IN THE SEARCH DIRECTION          
C                                                                       
           AGPE=AG+E                                                    
           CALL DC4NST(A,M,N,IA,S,P,V,W,JDEPG,JDEPS,AS,PNRM,IPTS        
     1  ,IPTG,SIMP,ISIMP, AMAN,EPS,SCALE,X,THETA,TT,IHIT,IERC,          
     1   FLAG)                                                          
         IF (IERC.NE.0) GO TO 190                                       
         CTEMP=DDOT(N,C,1,P,1)                                          
         IF (0.0D0 .LE. CTEMP) GOTO 140                                 
            IER2=9                                                      
            GO TO 210                                                   
 140        CTX = CTX+THETA*CTEMP                                       
            DO  150 K = 1, N                                            
               X(K) = X(K)+THETA*P(K)                                   
 150           CONTINUE                                                 
            IF (IHIT.LE.M) GO TO 160                                    
                I1=IHIT-M                                               
                I2=IABS(ISIMP(I1))                                      
                X(I2)=SIMP(I1)                                          
 160      CONTINUE                                                      
            AGP1 = AG+E+1                                               
            IF(M.LT.AGP1) GO TO 180                                     
C                                                                       
C UPDATE RESIDUALS                                                      
C                                                                       
            DO  170 I = AGP1, M                                         
               K = IPTG(I)                                              
               W(K) = W(K)-THETA*V(K)                                   
               IF (W(K).GT.0.0D0)W(K)=0.0D0                             
 170           CONTINUE                                                 
 180        CONTINUE                                                    
            IF (IHIT.LE.M)W(IHIT)=0.0D0                                 
         GOTO  60                                                       
 190  IER2=7                                                            
      GO TO 210                                                         
 200  IER2=6                                                            
C                                                                       
C RECOMPUTE DUAL VARIABLS                                               
C                                                                       
 210    CONTINUE                                                        
        AGPE=AG+E                                                       
        IF (AGPE.GT.0)CALL DM4TOP(AGPE,LT,RHS,U1)                       
        IF (AS.NE.0)CALL DL4AGS(A,M,N,IA,AMAN,AS,U1,U1(AGPE+1),         
     1  C,ISIMP,AGPE,IPTS,IPTG,TT)                                      
        CALL LEAVE                                                      
        RETURN                                                          
      END                                                               
      SUBROUTINE DL4PH1(A,M, N, AMAN, IA, B,  X, ITRMAX, CTX, S, SIMP,  
     1  ISIMP, E, W, Q,IQ, LT, P, V, SCALE, IPTS, IPTG, DVECS, DVECG    
     1   ,TT,U1,PRINT, RHS,AS,AG,KK,CC,IERR)                            
C THIS IS PHASE2 OF THE LINEAR PROGRAMMING PACKAGE                      
C                                                                       
C THE PARAMTERS HAVE THE FOLLOWING INTERPRETATIONS                      
C A  SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION AMAN.        
C M  NUMBER OF GENERAL EQUALITY AND INEQUALITY CONSTRAINTS              
C N  NUMBER OF UNKNOWNS                                                 
C AMAN - USER PROVIDED FUNCTION WHICH EITHER RETURNS A ROW              
C    OF THE CONSTRAINT MATRIX OR DOES A MATRIX-VECTOR                   
C    INNER PRODUCT                                                      
C IA  INTEGER SCRATCH VECTOR FOR USER TO BE PASSED TO USER FUNCTION     
C     AMAN                                                              
C B   RIGHT HAND SIDE CONSTRAINT VECTOR                                 
C X   FEASIBLE VECTOR ON INPUT, SOLUTION ON OUTPUT                      
C ITRMAX   MAXIMUM NUMBER OF ITERATIONS TOLERATED.                      
C CTX ON OUTPUT THE COST FUNCTION TO BE MAXIMIZED                       
C S   NUMBER OF SIMPLE CONSTRAINTS                                      
C SIMP VECTOR GIVING LOWER OR UPPERBOUND                                
C ISIMP VECTOR TELLING ON WHICH ELEMENT THE SIMPLE CONSTRAINT           
C     PERTAINS. IF NEGATIVE IT IS AN UPPER BOUND                        
C E   NUMBER OF EQUALITY CONSTRAINTS                                    
C W   M VECTOR OF RESIDUALS                                             
C Q   AN N XN ARRAY STORING Q FACTOR OF LQ FACTORIZATION OF ACTIVE      
C     CONSTRAINT MATRIX                                                 
C LT  AN N X N ARRAY STORYING THE TRANPOSE OF L IN QL FACTORIZATION     
C P   SCRATCH VECTOR WHICH WILL CONTAIN SEARCH DIRECTION                
C V   SCRATCH VECTOR LENGTH M WHICH WILL CONTAIN INACTIVE CONSTRAINT    
C     MATRIX TIMES P                                                    
C SCALE SCRATCH VECTOR WHICH WILL CONTAIN NORM OF CONSTRAINT ROWS       
C IPTS SCRATCH VECTOR GIVING PERMUTATION OF SIMPLE CONSTRAINTS          
C IPTG SCRATCH VECTOR POINTING TO GENERAL CONSTRAINTS                   
C DVECS SCRATCH VECTOR STATING WHETHER SIMPLE CONSTAINT HAS BEEN        
C       DROPPED-TO PREVENT CYCLING                                      
C DVECG SCRATCH VECTOR STATING WHETHER GENERAL CONSTRAINT HAS           
C       BEEN DROPPED-TO PREVENT CYCLING                                 
C TT    PURE SCRATCH VECTOR                                             
C U1    SCRATCH VECTOR TO STORE LAGRANGE MULTIPLIERS                    
C PRINT   - USER WRITTEN SUBROUTINE WHICH  PRINTS STUFF EACH ITER.      
      INTEGER M, N, S, ISIMP(1),IA(1)                                   
      EXTERNAL AMAN,PRINT                                               
      LOGICAL ISTOP,FIRST                                               
      INTEGER ITRMAX                                                    
      DOUBLE PRECISION A(1), B(1),  X(N), CTX, W(1), ET                 
       DOUBLE PRECISION TT(N),RHS(N)                                    
      DOUBLE PRECISION Q(IQ, N), LT( N), P(N), V(1), SCALE(1)           
      INTEGER  NMS,  AGP1,  E                                           
      INTEGER I, K, AGPE, FLAG, IPTG(1)                                 
      INTEGER IPTS(N), INDX2, AG, II, AS, DVECG(1)                      
      INTEGER KK, DVECS(1),   ITRPH1                                    
      LOGICAL DONE                                                      
      DOUBLE PRECISION EPS,  CNRM, TEMP, TOLL, DDOT,C                   
      DOUBLE PRECISION PNRM, SIMP(1), TOLU, XNRM, U1(1),  CC(1)         
      DOUBLE PRECISION  DNRM2,  THETA,DFLOAT                            
      DOUBLE PRECISION CTEMP,  D1MACH, UMAX,BOUND, BIGBND               
      INTEGER ISMA(1000)                                                
      COMMON /CSTAK/ISMA                                                
      CALL ENTER(0)                                                     
      MAXMN=MAX0(M,N)                                                   
      MAXSM=MAX0(S,M)                                                   
      IIHIT=ISTKGT(MAXMN, 2)                                            
      INVHIT=1                                                          
      IF(MAXSM.NE.0) INVHIT=ISTKGT(MAXSM, 2)                            
      EPS =DFLOAT(N)*D1MACH(4)*10.0D0                                   
      BIGBND=D1MACH(2)*(1.0D0-EPS)                                      
      BOUND=EPS*100.0D0                                                 
      IHIT=0                                                            
      TOLL = 1.D0 + D1MACH(4)                                           
       ISTOP=.FALSE.                                                    
      TOLU = 1.D0 - D1MACH(4)                                           
      XNRM = DNRM2(N, X, 1)                                             
      JDEPS=0                                                           
      JDEPG=E                                                           
      IF (M .EQ. 0) GOTO 20                                             
      DO  10 II = 1, M                                                  
         V(II)=0.0D0                                                    
         DVECG(II) = 0                                                  
 10      CONTINUE                                                       
 20      CONTINUE                                                       
      DO 25 I=1,N                                                       
 25      P(I)=0.0D0                                                     
       IF (S .EQ. 0) GOTO 40                                            
       DO 30 II=1,S                                                     
           IF (ISIMP(I).LT.0.AND.SIMP(I).GT.BIGBND)SIMP(I)=BIGBND       
           IF (ISIMP(I).GT.0.AND.SIMP(I).LT.-BIGBND)SIMP(I)=-BIGBND     
          DVECS(II) = 0                                                 
 30       CONTINUE                                                      
 40    CONTINUE                                                         
      AS = 0                                                            
      AG = 0                                                            
      KK=E                                                              
      IERR=0                                                            
      INDX2=0                                                           
C                                                                       
C HANDLE EQUALITY CONSTRAINTS                                           
C                                                                       
       IF (E.GT.0)                                                      
     1CALL DE4QL(A,N,AMAN,IA,E,Q,IQ,TT,B,X,RHS,V,LT)                    
C                                                                       
C CREATE RESIDUAL VECTOR                                                
C                                                                       
       IF (M.EQ.0) GO TO 60                                             
       DO 50 I=1,M                                                      
          CALL AMAN(.TRUE.,A,IA,N,I,X,TEMP)                             
          W(I)=B(I)-TEMP                                                
 50    CONTINUE                                                         
 60    CONTINUE                                                         
      ITRPH1 = 1                                                        
         GOTO  80                                                       
 70      ITRPH1 = ITRPH1+1                                              
 80      IF (ITRPH1 .GT. ITRMAX) GOTO  210                              
       FIRST=.TRUE.                                                     
       MMAG = M - AG - E                                                
C                                                                       
C GET NEW GRADIENT                                                      
C                                                                       
           AGPE=AG+E                                                    
       IST=AGPE                                                         
       IF (JDEPS.EQ.AS) GO TO 92                                        
       JDEPS1=JDEPS+1                                                   
       DO 91 I=JDEPS1,AS                                                
          II=ISIMP(I)                                                   
          X(II)=SIMP(I)                                                 
91     CONTINUE                                                         
92     IST=JDEPG                                                        
 90    CONTINUE                                                         
       CALL DG4ETC(N,M,S,AS,AGPE,A,IA,AMAN,X,CC,RHS,                    
     1     W,SCALE,Q,IQ,IPTS,ISIMP,SIMP,IPTG,DONE,CTX,                  
     2     CNRM,IST,FIRST)                                              
       IF (DONE.AND.FIRST)GOTO 233                                      
       IF (DONE.AND..NOT.FIRST)GO TO 230                                
       IF (ITRPH1.EQ.1) GO TO 95                                        
C                                                                       
C CALL SUBROUTINE TO TEST IF SIMPLE CONSTRAINTS SHOULD BE               
C ADDED AND THEN ADD THEM AND UPDATE LQ DECOMPOSTION                    
C                                                                       
         NMS=N-AS                                                       
          ISTRIK=IHIT-M                                                 
         IF (NMS .NE. 0) CALL DA4PPS(A, M, N, IA, KK,S, Q,IQ, LT, AS,   
     1      AG, E, IPTS, DVECS, X, SIMP, ISIMP, TOLL, TOLU, EPS, TT,    
     2    IPRINT,RHS,INDX2,P,1, JDEPS,ISMA(IIHIT),ISMA(INVHIT),ISTRIK)  
C                                                                       
C CALL SUBROUTINE TO TEST IF GENERAL CONSTRAINTS SHOULD BE              
C ADDED AND THEN ADD THEM AND UPDATE LQ DECOMPOSITION                   
C                                                                       
         IF (MMAG .NE. 0) CALL DA4PPG(A, M, N, IA, KK, Q,IQ, LT, AG,    
     1      AS,E,IPTG,IPTS,W,SCALE,XNRM,AMAN,EPS,TT,P,IPRINT,RHS,       
     2      INDX2,V,JDEPG,ISMA(IIHIT),ISMA(INVHIT),IHIT,DVECG)          
 95         AGPE=AG+E                                                   
         FLAG=1                                                         
       IF (ISTOP)GOTO 233                                               
C                                                                       
C DETERMINE WHICH CONSTRAINT TO DROP                                    
C AND DROP THEM                                                         
C                                                                       
C COMPUTE LAGRANGE MULTIPLIERS                                          
C                                                                       
            IF (AGPE .EQ. 0) GOTO 100                                   
C                                                                       
C COMPUTE LAGRANGE MULTIPLIERS FOR GENERAL CONSTRAINTS                  
C                                                                       
            ASP1 = AS + 1                                               
          CALL DM4TOP(AGPE,LT,RHS,U1)                                   
 100        CONTINUE                                                    
            AGP1=AG+E+1                                                 
C                                                                       
C COMPUTE LAGRANGE MULTIPLIERS FOR SIMPLE CONSTRAINTS                   
C                                                                       
            IF (AS .NE. 0) CALL DL4AGS(A,M,N,IA,AMAN,AS,U1,U1(AGP1),CC, 
     1           ISIMP,AGPE,IPTS,IPTG,TT)                               
C                                                                       
C DETERMINE WHICH CONSTRAINT TO DROP GIVEN THE LAGRANGE MULTIPLIERS     
C                                                                       
       CALL PRINT(A,M,N,AMAN,IA,B,C,X,CTX,S,SIMP,ISIMP,E,ITRPH1,        
     1 IPTG,AG,AS,U1,ISTOP,1)                                           
       IF (ISTOP)GOTO 231                                               
         IF (E .GE. KK) GOTO 150                                        
C                                                                       
 110        CALL DD4CLM(U1, AG, AS, E, UMAX, INDX2, FLAG, DVECS, DVECG, 
     1         IPTG, IPTS, BOUND,1,KK,N)                                
            IF (FLAG .NE. 1) GOTO 120                                   
            INDX2=0                                                     
            GOTO 150                                                    
 120   IF(INDX2.LE.AGPE) GO TO 130                                      
C                                                                       
C SINCE A SIMPLE CONSTRAINT IS TO BE DROPPED, UPDATE APPROPRIATE        
C ARRAYS AND THE LQ DECOMPOSITION ACCORDINGLY                           
C                                                                       
          IND2=INDX2-AGPE                                               
          ITEMP=IPTS(IND2)                                              
          NMS=N-AS                                                      
          RHS(NMS+1)=CC(ITEMP)                                          
          IF (AS .NE. 0) CALL DD4RPS(A,M,N,IA,AMAN,KK,Q,IQ,LT,AG,AS,E,  
     1   IPTG,IPTS,DVECS,SIMP,ISIMP,INDX2,TT,P, RHS)                    
          INDX2=1                                                       
          GOTO 140                                                      
 130      CONTINUE                                                      
C                                                                       
C SINCE A GENERAL CONSTRAINT IS TO BE DROPPED, UPDATE APPROPRIATE       
C ARRAYS AND LQ DECOMPOSITION ACCORDINGLY                               
C                                                                       
       IF (AG .NE. 0) CALL DD4RPG (M,N,KK,Q,IQ,LT,AG,AS,E,IPTG,INDX2    
     1                ,DVECG,RHS,CC,IPTS)                               
       INDX2=-1                                                         
 140   CONTINUE                                                         
       JDEPS=AS                                                         
       JDEPG=AG+E                                                       
 150   CONTINUE                                                         
C COMPUTE NEW SEARCH DIRECTION                                          
C                                                                       
         CALL DP4RJD(N, AS, AG, E, Q,IQ, IPTS, CC, P, TT,RHS)           
         IF(JDEPS.EQ.AS) GO TO 132                                      
         ASP1=AS+1                                                      
         DO 131 I=ASP1,JDEPS                                            
            I2=ISIMP(I)                                                 
 131     P(I2)=0.0D0                                                    
 132      CONTINUE                                                      
         PNRM = DNRM2(N, P, 1)                                          
C                                                                       
C CHECK IF FINISHED                                                     
C                                                                       
         IF (KK .EQ. N ) GO TO 230                                      
         IF (ITRPH1.EQ.1)                                               
     1 CALL PRINT(A,M,N,AMAN,IA,B,C,X,CTX,S,SIMP,ISIMP,E,ITRPH1,        
     1 IPTG,AG,AS,U1,ISTOP,1)                                           
          IF (PNRM.LT.EPS.AND.FLAG.NE.1) GO TO 95                       
         IF (PNRM.LT.EPS.AND..NOT.FIRST)GO TO 230                       
          FIRST=.FALSE.                                                 
         IF (PNRM.LT.EPS) GO TO 90                                      
C                                                                       
C DETERMINE HOW FAR ONE SHOULD PROCEED IN THE SEARCH DIRECTION          
C                                                                       
           AGPE=AG+E                                                    
           CALL DC4ONS(A,M,N,IA,S,P,V,W,JDEPG,JDEPS,AS,PNRM,IPTS        
     1  ,IPTG,SIMP,ISIMP, AMAN,EPS,SCALE,X,THETA,TT,IHIT,IERR)          
         CTEMP=DDOT(N,CC,1,P,1)                                         
         IF (0.0D0 .LE. CTEMP) GOTO 160                                 
            IERR=9                                                      
            GOTO 231                                                    
 160        CTX = CTX+THETA*CTEMP                                       
            DO  170 K = 1, N                                            
               X(K) = X(K)+THETA*P(K)                                   
 170           CONTINUE                                                 
            IF (IHIT.LE.M) GO TO 180                                    
                I1=IHIT-M                                               
                I2=IABS(ISIMP(I1))                                      
                X(I2)=SIMP(I1)                                          
 180      CONTINUE                                                      
            AGP1 = AG+E+1                                               
            IF(M.LT.AGP1) GO TO 200                                     
C                                                                       
C UPDATE RESIDUALS                                                      
C                                                                       
            DO  190 I = AGP1, M                                         
               K = IPTG(I)                                              
               W(K) = W(K)-THETA*V(K)                                   
 190           CONTINUE                                                 
 200        CONTINUE                                                    
            IF (IHIT.LE.M.AND.IHIT.GT.0)W(IHIT)=0.0D0                   
         GOTO  70                                                       
 210   IERR=6                                                           
 220  GOTO231                                                           
 230  IERR=8                                                            
 231   CALL LEAVE                                                       
      RETURN                                                            
 233   CONTINUE                                                         
       CALL LEAVE                                                       
      IF(S.EQ.0)RETURN                                                  
      DO 234 I=1,S                                                      
         ET=1.0D0                                                       
         IF(ISIMP(I).LT.0)ET=-1.0D0                                     
         I1=IABS(ISIMP(I))                                              
         IF (ET*X(I1).LT.ET*SIMP(I))X(I1)=SIMP(I)                       
234   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE DLPMAN(INNERP, A, M, N, K, X, SK)                      
      INTEGER M, N                                                      
      INTEGER K                                                         
      DOUBLE PRECISION A(M, N), X(N), SK                                
      LOGICAL INNERP                                                    
      INTEGER I                                                         
      DOUBLE PRECISION H                                                
C   THIS PROCEDURE HANDLES THE MATRIX A, WHICH IS STORED IN             
C   THE CONVENTIONAL FASHION.  IF THE MATRIX IS LARGE AND SPARSE        
C   THE USER SHOULD REPLACE THIS ROUTINE WITH A MORE APPROPRIATE        
C   ROUTINE.                                                            
C   INPUT                                                               
C   INNERP - IF TRUE THEN COMPUTE THE INNER PRODUCT OF ROW K WITH X.    
C            OTHERWISE RETURN ROW K OF A IN THE VECTOR X.               
C   A      - THE MATRIX A.                                              
C   M,N    - THE ROW AND COLUMN DIMENSIONS OF A.                        
C   K      - THE INDEX OF THE ROW OF A WHICH IS TO BE PROCESSED.        
C   X      - IF INNERP IS TRUE, THEN X IS A VECTOR WHOSE INNER          
C            PRODUCT WITH ROW K OF A IS TO BE COMPUTED.                 
C   OUTPUT                                                              
C   X      - IF INNERP IS FALSE, THEN X CONTAINS ROW K OF A.            
C   SK     - IF INNERP IS TRUE, THEN SK CONTAINS THE INNER              
C            PRODUCT OF X AND ROW K OF A.                               
C/6S                                                                    
C     IF (M .LE. 0 .OR. N .LE. 0) CALL SETERR(                          
C    1   24HDLPA - INVALID DIMENSION, 25, 1, 2)                         
C     IF (K .LT. 1 .OR. M .LT. K) CALL SETERR(20HDLPA - INVALID INDEX,  
C    1   21, 2, 2)                                                      
C/7S                                                                    
      IF (M .LE. 0 .OR. N .LE. 0) CALL SETERR(                          
     1   'DLPA - INVALID DIMENSION', 25, 1, 2)                          
      IF (K .LT. 1 .OR. M .LT. K) CALL SETERR('DLPA - INVALID INDEX',   
     1   21, 2, 2)                                                      
C/                                                                      
      IF (.NOT. INNERP) GOTO 20                                         
         H = 0.D0                                                       
         DO  10 I = 1, N                                                
            H = H+X(I)*A(K, I)                                          
 10         CONTINUE                                                    
         SK = H                                                         
         GOTO  40                                                       
 20      DO  30 I = 1, N                                                
            X(I) = A(K, I)                                              
 30         CONTINUE                                                    
 40   RETURN                                                            
      END                                                               
       SUBROUTINE DLPRNT(A,M,N,AMAN,IA,B,C,X,CTX,IS,SIMP,ISIMP,IE,      
     1 ITER,IPTG,IAG,IAS,U,IEND,IPH)                                    
C                                                                       
C THIS IS A PRINT ROUTINE                                               
C                                                                       
       DOUBLE PRECISION CTX,A(1),X(N),B(1)                              
       EXTERNAL AMAN                                                    
       LOGICAL IEND                                                     
       INTEGER IA(1),IPTG(N),ISIMP(1)                                   
       DOUBLE PRECISION SIMP(1),C(1),U(1)                               
       IEND= .FALSE.                                                    
       RETURN                                                           
       END                                                              
       SUBROUTINE DI4NTL(N,Q,IQ,IPTG,M,IPTS,A,IA,AMAN,SCALE,T)          
C                                                                       
C THIS SUBROUTINE INITIALIZES Q,IPTG,AND IPTS AND SCALE                 
C                                                                       
       INTEGER N,M                                                      
       INTEGER IPTS(1),IPTG(1),IA(1)                                    
       DOUBLE PRECISION Q(IQ,N),A(1),SCALE(1),T(N)                      
       DOUBLE PRECISION DNRM2, TEMP1                                    
       EXTERNAL AMAN                                                    
       CALL SETD(N*N,0.0D0,Q)                                           
       DO 10 I=1,N                                                      
          Q(I,I)=1.D0                                                   
          IPTS(I)=I                                                     
 10    CONTINUE                                                         
       IF (M.EQ.0) RETURN                                               
       DO 20 I=1,M                                                      
          IPTG(I)=I                                                     
          CALL AMAN(.FALSE.,A,IA,N,I,T,TEMP1)                           
          SCALE(I)=DNRM2(N,T,1)                                         
 20    CONTINUE                                                         
       RETURN                                                           
       END                                                              
      SUBROUTINE DA4PPG(A,M,N,IA,KK,Q,IQ,LT,AG,AS,E,IPTG,IPTS,W,        
     1   SCALE, XNRM,  AMAN, EPS, TMP, P,IPRINT, RHS,INDX2, V, JDEPG,   
     2   IHIT,INVHIT,ISTRIK,DVECG)                                      
C                                                                       
C THIS SUBROUTINE DETERMINES WHETHER A GENERAL CONSTRAINT               
C SHOULD BE ADDED TO THE ACTIVE CONSTRAINT LIST AND                     
C CALLS DA4GQR TO UPDATE THE LQ DECOMPOSTION IF ONE SHOULD              
C                                                                       
      INTEGER M, N, AGPE                                                
      EXTERNAL AMAN                                                     
      INTEGER KK, AG, AS, E, IPTG(1), IPTS(1),DVECG(1)                  
      DOUBLE PRECISION A(N),Q(IQ,1),LT( 1),W(N),SCALE(N),EPS,RHS(N)     
      INTEGER NCT, NMS, AGP1, ASP1, I, IA(1)                            
      INTEGER II, IHIT(M),INVHIT(M)                                     
      DOUBLE PRECISION TMP(1), P(1),  PNRM, XNRM, DNRM2, T1, V(M), VV   
      ASP1 = AS+1                                                       
      NMS = N-AS                                                        
      IF(NMS.LE.0)RETURN                                                
      AGP1 = AG+E+1                                                     
      ISGE=AS+AGP1                                                      
      II = JDEPG+1                                                      
      NH=0                                                              
      DO 5 I=1,M                                                        
 5      INVHIT(I)=0                                                     
      IF (INDX2.LT.0)II=II+1                                            
         GOTO  20                                                       
 10      II = II+1                                                      
 20      IF (II .GT. M) GOTO  25                                        
          IF(IPTG(II).LT.0) GO TO 10                                    
         I = IPTG(II)                                                   
         IF (DABS(W(I)) .GT. (EPS)*SCALE(I)*XNRM) GOTO  10              
          IF (V(I).GE.0.0D0.AND.ISTRIK.NE.I) GO TO 10                   
         JH=0                                                           
         V(I)=DABS(V(I))/SCALE(I)                                       
         VV=V(I)                                                        
 21      JH=JH+1                                                        
         IF (JH.GT.NH) GO TO 23                                         
           J2=IHIT(JH)                                                  
           IF(VV.LT.V(J2)) GO TO 21                                     
           MNH=NH+1                                                     
           DO 22 MH=JH,NH                                               
              IHIT(MNH)=IHIT(MNH-1)                                     
              MNH=MNH-1                                                 
 22        CONTINUE                                                     
           NH=NH+1                                                      
           IHIT(JH)=II                                                  
           GO TO 10                                                     
 23      NH=NH+1                                                        
         IHIT(NH)=II                                                    
         GO TO 10                                                       
 25   IF (NH.EQ.0) GO TO 40                                             
       DO 26 I2=1,NH                                                    
          I3=IHIT(I2)                                                   
 26    INVHIT(I3)=I2                                                    
       DO 37 I2=1,NH                                                    
          II=IHIT(I2)                                                   
          I=IPTG(II)                                                    
      AGP1 = AG+E+1                                                     
C                                                                       
C A CONSTRAINT HAS BEEN HIT.                                            
C TEST IF IT LINEAR INDEPENDENT OF OTHERS                               
C                                                                       
         CALL AMAN(.FALSE., A, IA, N, I, TMP, T1)                       
       DO 30 JJ=ASP1,N                                                  
          J=IPTS(JJ)                                                    
          JAS = JJ - AS                                                 
          P(JAS) = TMP(J)                                               
 30       CONTINUE                                                      
       CALL DM5TOP (IQ,N,Q,1,NMS,1,NMS,P,1,P)                           
         NCT=N-ISGE+1                                                   
         PNRM = DNRM2(NCT, P(AGP1), 1)                                  
C                                                                       
C CONSTRAINT IS INDEPENDEDNT OF OTHER CONSTRAINTS , SO                  
C ADD IT BY INTERCHANGING ELEMENTS IN IPTG AND                          
C UPDATING DECOMPOSTION                                                 
C                                                                       
         JDEPG1=JDEPG+1                                                 
         IPTG(II) = IPTG(JDEPG1)                                        
         IPTG(JDEPG1) = IPTG(AGP1)                                      
         IPTG(AGP1)=I                                                   
         IDV =DVECG(II)                                                 
         DVECG(II)=DVECG(JDEPG1)                                        
         DVECG(JDEPG1)=DVECG(AGP1)                                      
         DVECG(AGP1)=IDV                                                
         IN1=INVHIT(JDEPG1)                                             
         IN2=INVHIT(AGP1)                                               
         IF (IN2.NE.0) IHIT(IN2)=JDEPG1                                 
         IF(IN1.NE.0)IHIT(IN1)=II                                       
         INVHIT(JDEPG1)=IN2                                             
         INVHIT(II)=IN1                                                 
         AGPE = AG + E                                                  
         JDEPG=JDEPG+1                                                  
         IF (PNRM .LE. EPS*SCALE(I)) GOTO 37                            
         CALL DA4GQR(IQ, NMS, AGPE, Q, LT, P, RHS)                      
         ISGE=ISGE+1                                                    
         AG = AG + 1                                                    
         KK = KK + 1                                                    
 37      CONTINUE                                                       
 40   RETURN                                                            
      END                                                               
      SUBROUTINE DA4PPS(A, M, N,IA, KK, S, Q,IQ, R, AS, AG, E, IPTS,    
     1   DVECS,X,SIMP,ISIMP,TOLL,TOLU,EPS,TEMP,IPRINT,RHS,INDX2,P,      
     2   IPHAS, JDEPS,IHIT,INVHIT,ISTRIK)                               
C                                                                       
C THIS SUBROUTINE TESTS IF ANY OF THE SIMPLE CONSTRAINTS                
C SHOULD BE ADDED TO THE ACTIVE SET AND THEN UPDATES                    
C THE VECTORS AND LQ DECOMPOSTION                                       
C                                                                       
      INTEGER M, S, DVECS(1), IA(1),AGPE,AGEP1,IPHAS                    
      INTEGER N, KK, AS, AG, E, IPTS(1), JDEPS                          
      INTEGER ISIMP(1)                                                  
      DOUBLE PRECISION A(1),Q(IQ, 1), R( 1), X(1), SIMP(1), TOLL        
      DOUBLE PRECISION RHS(N), P(N)                                     
      DOUBLE PRECISION TOLU, EPS,PP                                     
      INTEGER NMS, ASP1, I, J, K                                        
      INTEGER IABS, II,  INDEX, NMSGE, ITEMP                            
      DOUBLE PRECISION L, PNRM, DNRM2, TEMP(1),D1MACH,SL,SU,EPA         
      INTEGER IHIT(N),INVHIT(S)                                         
      COMMON /CSTAK/ISTAK(1000)                                         
      AGPE=AG+E                                                         
      ASP1 = AS+1                                                       
      NH=0                                                              
      NMS = N - AS                                                      
      AGEP1=AG+E+1                                                      
      II = JDEPS+1                                                      
      EPA=DNRM2(N,X,1)*D1MACH(4)+D1MACH(1)                              
      IF (INDX2.GT.0)II=ASP1+1                                          
      DO  5 I=1,S                                                       
 5     INVHIT(I)=0                                                      
C                                                                       
C FIND OUT IF ANY CONSTRAINT HAS BEEN HIT                               
C                                                                       
         GOTO  20                                                       
 10      II = II+1                                                      
 20      IF (II .GT. S) GOTO  46                                        
         K = ISIMP(II)                                                  
         L = SIMP(II)                                                   
         I = IABS(K)                                                    
          SU=L*TOLU                                                     
          SL=L*TOLL                                                     
          IF (L.LT.0.0D0)SU=L*TOLL                                      
          IF(L.LT.0.D0)SL=L*TOLU                                        
          IF(SU.EQ.0.D0)SU=-EPA                                         
          IF(SL.EQ.0.D0)SL=EPA                                          
          IF (K .GT. 0) GOTO 30                                         
             IF (X(I) .LT. SU.OR.(IPHAS.EQ.1.AND.                       
     1   X(I).GT.SL)) GOTO 10                                           
             X(I)=L                                                     
             IF (P(I).LE.0.0D0.AND.ISTRIK.NE.II) GO TO 10               
             GOTO 40                                                    
 30      CONTINUE                                                       
         IF  (X(I) .GT. SL.OR.(IPHAS.EQ.1.AND.                          
     1 X(I).LT.SU)) GOTO 10                                             
             X(I)=L                                                     
             IF (P(I).GE.0.0D0.AND.ISTRIK.NE.II) GO TO 10               
 40      CONTINUE                                                       
         JH=0                                                           
         PP=ABS(P(I))                                                   
 41      JH=JH+1                                                        
         IF(JH.GT.NH) GO TO 45                                          
           J2=IHIT(JH)                                                  
           I2=IABS(ISIMP(J2))                                           
           IF (PP.LT.ABS(P(I2)))GO TO 41                                
           MNH=NH+1                                                     
           DO 43 MH=JH,NH                                               
             IHIT(MNH)=IHIT(MNH-1)                                      
             MNH=MNH-1                                                  
  43       CONTINUE                                                     
           NH=NH+1                                                      
           IHIT(JH)=II                                                  
           GO TO 10                                                     
  45       NH=NH+1                                                      
           IHIT(NH)=II                                                  
            GO TO 10                                                    
C                                                                       
C DETERMINE WHO IT BELONGS TO                                           
C                                                                       
  46       IF(NH.EQ.0) GO TO 110                                        
         DO 47 I2=1,NH                                                  
             I3=IHIT(I2)                                                
 47       INVHIT(I3)=I2                                                 
           DO 105 I2=1,NH                                               
              II=IHIT(I2)                                               
              K=ISIMP(II)                                               
              I=IABS(K)                                                 
               L=SIMP(II)                                               
         DO  60 J = ASP1, N                                             
            IF (IPTS(J) .NE. I) GOTO 50                                 
            IP = IPTS(J)                                                
            NMSGE = NMS - AG - E                                        
       JMAS = J-AS                                                      
C                                                                       
C TEST FOR LINEAR INDEPENDENCE                                          
C                                                                       
            PNRM = DNRM2 (NMSGE, Q(AGEP1,JMAS),1)                       
               ITEMP2 = DVECS(II)                                       
               JDEPS1=JDEPS+1                                           
               IN1=INVHIT(JDEPS1)                                       
               IN2=INVHIT(ASP1)                                         
               IF (IN2.NE.0)IHIT(IN2)=JDEPS1                            
               IF(IN1.NE.0)IHIT(IN1)=II                                 
               INVHIT(JDEPS1)=IN2                                       
               INVHIT(II)=IN1                                           
               ISIMP(II)=ISIMP(JDEPS1)                                  
               SIMP(II)=SIMP(JDEPS1)                                    
               DVECS(II)=DVECS(JDEPS1)                                  
C                                                                       
               ISIMP(JDEPS1)=ISIMP(ASP1)                                
               SIMP(JDEPS1)=SIMP(ASP1)                                  
                DVECS(JDEPS1)=DVECS(ASP1)                               
               ISIMP(ASP1) = K                                          
               SIMP(ASP1) = L                                           
               DVECS(ASP1) = ITEMP2                                     
               JDEPS=JDEPS+1                                            
               IF(PNRM.LE.EPS) GO TO 104                                
C IT IS INDEPENDENT SO CHANGE VECTORS                                   
C                                                                       
               GOTO  70                                                 
 50         CONTINUE                                                    
 60         CONTINUE                                                    
 70      CONTINUE                                                       
C                                                                       
C UPDATE LQ DECOMPOSTION                                                
            INDEX = J - AS                                              
         IF (AGPE.EQ.0) GO TO 80                                        
            ITEMP=IPTS(N)                                               
            CALL MOVEBI(NMS-1,IPTS(ASP1),IPTS(AS+2))                    
            IF(J.NE.N)IPTS(J+1)=ITEMP                                   
         CALL DA4SQR(IQ,NMS,AGPE,Q,R,INDEX,TEMP,RHS)                    
         GO TO 100                                                      
 80       CONTINUE                                                      
         CALL MOVEBI(INDEX-1,IPTS(ASP1),IPTS(AS+2))                     
         NMS=NMS-1                                                      
         IF (INDEX.EQ.NMS+1) GO TO 100                                  
          DO 90 IJ=INDEX,NMS                                            
             RHS(IJ)=RHS(IJ+1)                                          
 90       CONTINUE                                                      
 100     CONTINUE                                                       
         IPTS(ASP1)=I                                                   
         AS = AS+1                                                      
         ASP1 = ASP1 + 1                                                
         KK = KK+1                                                      
  104       CONTINUE                                                    
 105     CONTINUE                                                       
 110  RETURN                                                            
      END                                                               
      SUBROUTINE DA4GQR(K, M, N, Q, R, B, RHS)                          
C                                                                       
C THIS SUBROUTINE UPDATES THE QR DECOMPOSITION OF A MATRIX              
C WHEN THE VECTOR B IS ADDED AS THE LAST COLUMN OF THE MATRIX           
C                                                                       
      INTEGER K, I, M, N                                                
      DOUBLE PRECISION Q(K, 1), R( 1), B(1), RHS(K)                     
      DOUBLE PRECISION BETA, ALPHA, F, DDOT                             
       MMN=M-N                                                          
      N = N+1                                                           
      CALL DH4HG(MMN,B(N),ALPHA,BETA,1)                                 
      DO 10 I=1,M                                                       
         F=DDOT(MMN,Q(N,I),1,B(N),1)/ALPHA                              
         CALL DAXPY(MMN,F,B(N),1,Q(N,I),1)                              
 10   CONTINUE                                                          
      F=DDOT(MMN,B(N),1,RHS(N),1)/ALPHA                                 
      CALL DAXPY(MMN,F,B(N),1,RHS(N),1)                                 
      IS=(N*(N-1))/2+1                                                  
      IF (N.GT.1)CALL DCOPY(N-1,B,1,R(IS),1)                            
      IS=IS+N-1                                                         
      R(IS)=BETA                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DA4SQR(K, M, N, Q, R, INDEX, TEMP, RHS)                
C                                                                       
C THIS SUBROUTINE UPDATES THE QR DECOMPOSITION WHEN                     
C THE INDEXTH ROW IS DELETED FROM THE MATRIX                            
      INTEGER M, N, INDEX, K                                            
      DOUBLE PRECISION TEMP(1), Q(K, 1), R( 1), RHS(K)                  
      INTEGER I, J                                                      
      DOUBLE PRECISION BETA, ALPHA, F,FF, DDOT, X,Y                     
C/6S                                                                    
C     IF (INDEX .LE. 0 .OR. M+1 .LT. INDEX) CALL SETERR(                
C    1   22HDA4SQR - INVALID INDEX, 21, 2, 2)                           
C/7S                                                                    
      IF (INDEX .LE. 0 .OR. M+1 .LT. INDEX) CALL SETERR(                
     1   'DA4SQR - INVALID INDEX', 21, 2, 2)                            
C/                                                                      
       CALL DCOPY (M,Q(1,INDEX),1,TEMP,1)                               
C                                                                       
C INTERCHANGE LAST COLUMN AND COLUMN INDEX                              
C                                                                       
       IF (INDEX.NE.M)CALL DCOPY(M,Q(1,M),1,Q(1,INDEX),1)               
         MMN=M-N                                                        
C                                                                       
C                                                                       
         KK=M                                                           
         IF (INDEX.GT.N)KK=INDEX                                        
         X=RHS(KK)                                                      
         MM1=M-1                                                        
         NP1=N+1                                                        
      IF (M.LE.N+1) GO TO 20                                            
         KKMN=KK-N                                                      
         CALL DH4HG(MMN,TEMP(N+1),ALPHA,BETA,KKMN)                      
         DO 10 I=1,MM1                                                  
            F=DDOT(MMN,TEMP(NP1),1,Q(NP1,I),1)/ALPHA                    
            CALL DAXPY(MMN,F,TEMP(NP1),1,Q(NP1,I),1)                    
 10      CONTINUE                                                       
         FF=DDOT(MMN,TEMP(NP1),1,RHS(NP1),1)/ALPHA                      
         CALL DAXPY(MMN,FF,TEMP(NP1),1,RHS(NP1),1)                      
         TEMP(KK)=BETA                                                  
         X=RHS(KK)                                                      
         IF (INDEX.LE.N)GO TO 20                                        
            RHS(INDEX)=RHS(M)                                           
            CALL DSWAP(M-1,Q(INDEX,1),K,Q(M,1),K)                       
 20      CONTINUE                                                       
         I=N                                                            
C                                                                       
C ELIMINATE UNWANTED ELEMENTS                                           
C                                                                       
             IS=(N*(N+1))/2                                             
         GOTO  40                                                       
 30      I = I-1                                                        
 40      IF (I .LT. 1) GOTO  70                                         
         IIJ=I                                                          
         IF (I.LE.N)Q(I,M)=0.0D0                                        
         IF (I.NE.INDEX)GO TO 60                                        
          CALL DSWAP(M-1,Q(I,1),K,Q(M,1),K)                             
C SWAP RHS                                                              
         Y=RHS(I)                                                       
         RHS(I)=X                                                       
         X=Y                                                            
         IIJ=M                                                          
         KK=INDEX                                                       
C SWAP R                                                                
         IF (I.GT.N) GO TO 60                                           
           IS2=IS                                                       
           DO 50 J=I,N                                                  
              Y=Q(J,M)                                                  
              Q(J,M)=R(IS2)                                             
              R(IS2)=Y                                                  
              IS2=IS2+J                                                 
 50        CONTINUE                                                     
 60    CONTINUE                                                         
            CALL DROTG(TEMP(KK), TEMP(IIJ),ALPHA,BETA)                  
            CALL DROT (M-1,Q(M, 1),K,Q(I,1),K,ALPHA,BETA)               
            Y=RHS(I)                                                    
            RHS(I)=-BETA*X+ALPHA*Y                                      
            X=ALPHA*X+BETA*Y                                            
            IF (I.GT.N)GO TO 30                                         
            NMI=N-I                                                     
            CALL DS4ROT(NMI+1,Q(I,M),-1,R(IS),I,ALPHA,BETA)             
            IS=IS-I                                                     
            GOTO  30                                                    
 70       M=M-1                                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE  DC4NST(A, M, N, IA, S, P, V, W, AG, JDEPS,AS, PNRM,   
     1   IPTS,IPTG,SIMP,ISIMP,AMAN,EPS,SCALE,X,THETA,T,IHIT,IERC,       
     2   IFLAG)                                                         
C                                                                       
C THIS SUBROUTINE LEAVES IN THETA THE DISTANCE ONE                      
C CAN TRAVEL ALONG THE DIRECTION P BEFORE HITTING                       
C THE INACTIVE CONSTRAINTS                                              
C                                                                       
C IF IHIT IS LESS THAN M, THE IHITTH GENERAL CONSTRAINT HAS BEEN        
C HIT. IF GREATER THAN M, THEN THE IHITTH-M SIMPLE CONSTRAINT HAS BEEN  
C HIT.                                                                  
C                                                                       
      INTEGER M, N, S, IA(1), ISIMP(1)                                  
      EXTERNAL AMAN                                                     
      INTEGER AG, AS, IPTS(1), IPTG(1)                                  
      DOUBLE PRECISION A(1), P(1), V(1), W(1), PNRM, EPS                
      DOUBLE PRECISION SCALE(1), X(1)                                   
      INTEGER NMS, AGP1, ASP1, I, K, II                                 
      DOUBLE PRECISION T(1), TEMP1, THETA, SIMP(1),VV,WW,ET,DFLOAT      
      LOGICAL UNBNDD                                                    
      INTEGER IT1                                                       
      UNBNDD = .TRUE.                                                   
      IERC=0                                                            
      NMS = N-AS                                                        
      AGP1 = AG+1                                                       
      ASP1 = AS+1                                                       
      IF (M .LT. AGP1) GOTO  60                                         
C                                                                       
C DETERMINE THE DISTANCE TO THE NEAREST GENERAL CONSTRAINT              
      IHN=N/2                                                           
      DO  50 IJ = AGP1, M                                               
         I = IPTG(IJ)                                                   
         IF (AS.GT.IHN)GO TO 10                                         
          CALL AMAN(.TRUE.,A,IA,N,I,P,V(I))                             
          GO TO 30                                                      
 10       CALL AMAN(.FALSE., A, IA, N, I, T, TEMP1)                     
          V(I) = 0.                                                     
          DO  20 II = 1, NMS                                            
             IT1 = AS+II                                                
             K = IPTS(IT1)                                              
             V(I) = V(I)+T(K)*P(K)                                      
 20          CONTINUE                                                   
 30       CONTINUE                                                      
         IF (V(I) .GE. 0.0D0) GOTO 50                                   
C                                                                       
C WE WILL HIT THE CONSTRAINT BE GOING IN THE DIRECTION P                
C                                                                       
C  JULY 1983, DAVID GAY DETERMINED BUG TROUBLE, AND                     
C  PHYL FOX COMMENTED OUT THE FOLLOWING LINE (S. P. VERSION ALSO)       
C       IF (W(I).EQ.0.0D0.AND.IFLAG.EQ.1) GO TO 5                       
            IF (UNBNDD) GOTO 40                                         
               IF (V(I)*THETA .GE. W(I)) GO TO 50                       
                   THETA=W(I)/V(I)                                      
               IHIT=I                                                   
               GOTO  50                                                 
 40            UNBNDD = .FALSE.                                         
C                                                                       
C THIS IS THE FIRST CONSTRAINT ENCOUNTERED THAT EVEN                    
C IS IN THE RIGHT DIRECTION                                             
C                                                                       
               THETA = W(I)/V(I)                                        
               IHIT=I                                                   
 50      CONTINUE                                                       
 60      CONTINUE                                                       
      JDEPS1=JDEPS+1                                                    
      IF (S .LT. JDEPS1) GOTO  90                                       
C                                                                       
C DETERMINE THE DISTANCE TO THE NEAREST SIMPLE CONSTRAINT               
C                                                                       
      DO  80 II = JDEPS1, S                                             
       ET = DSIGN(1.0D0, DFLOAT(ISIMP(II)))                             
       I1 = IABS(ISIMP(II))                                             
       VV = ET * P(I1)                                                  
       IF (VV .GE. 0.0D0) GOTO 80                                       
       WW = ET*(SIMP(II)-X(I1))                                         
       IF (UNBNDD) GOTO 70                                              
       IF (THETA*VV .GE. WW) GO TO 80                                   
           THETA=WW/VV                                                  
         IHIT=II+M                                                      
         GOTO 80                                                        
 70     UNBNDD = .FALSE.                                                
        THETA = WW/VV                                                   
        IHIT=II+M                                                       
 80      CONTINUE                                                       
 90      CONTINUE                                                       
      IF (UNBNDD) GOTO 100                                              
         RETURN                                                         
 100     IERC=6                                                         
 110  RETURN                                                            
      END                                                               
      SUBROUTINE DC4ONS(A, M, N, IA, S, P, V, W, AG, JDEPS,AS, PNRM,    
     1   IPTS,IPTG,SIMP,ISIMP,AMAN,EPS,SCALE,X,THETA,T,IHIT,IERC)       
C                                                                       
C THIS SUBROUTINE LEAVES IN THETA THE DISTANCE ONE                      
C CAN TRAVEL ALONG THE DIRECTION P BEFORE HITTING                       
C THE INACTIVE CONSTRAINTS                                              
C                                                                       
C IF IHIT IS LESS THAN M, THE IHITTH GENERAL CONSTRAINT HAS BEEN        
C HIT. IF GREATER THAN M, THEN THE IHITTH-M SIMPLE CONSTRAINT HAS BEEN  
C HIT.                                                                  
C                                                                       
      INTEGER M, N, S, IA(1), ISIMP(1)                                  
      EXTERNAL AMAN                                                     
      INTEGER AG, AS, IPTS(1), IPTG(1)                                  
      DOUBLE PRECISION A(1), P(1), V(1), W(1), PNRM, EPS,THETA2         
      DOUBLE PRECISION SCALE(1), X(1), DFLOAT                           
      INTEGER NMS, AGP1, ASP1, I, K, II                                 
      DOUBLE PRECISION T(1), TEMP1, THETA, SIMP(1), EPP, ET, VV, WW     
      LOGICAL UNBNDD                                                    
      INTEGER IT1                                                       
      IERC=0                                                            
      EPP=PNRM*EPS                                                      
      UNBNDD = .TRUE.                                                   
      AGP1 = AG+1                                                       
      ASP1 = AS+1                                                       
      THETA2=0.0D0                                                      
      IHIT2=0                                                           
      IF (M .LT. AGP1) GOTO  50                                         
C                                                                       
C DETERMINE THE DISTANCE TO THE NEAREST GENERAL CONSTRAINT              
      DO  40 IJ = AGP1, M                                               
         I = IPTG(IJ)                                                   
         CALL AMAN(.FALSE., A, IA, N, I, T, TEMP1)                      
         V(I) = 0.                                                      
      NMS=N-AS                                                          
         DO  10 II = 1, NMS                                             
            IT1 = AS+II                                                 
            K = IPTS(IT1)                                               
            V(I) = V(I)+T(K)*P(K)                                       
 10         CONTINUE                                                    
        IF (W(I).LT.0.0D0) GO TO 20                                     
           IF (V(I).LE.EPP*SCALE(I))GO TO 40                            
           IF (V(I)*THETA2.GE.W(I)) GO TO 40                            
                IHIT2=I                                                 
                THETA2=W(I)/V(I)                                        
                GO TO 40                                                
 20     CONTINUE                                                        
         IF (V(I) .GE. (-EPP)*SCALE(I)) GOTO 40                         
C                                                                       
C WE WILL HIT THE CONSTRAINT BE GOING IN THE DIRECTION P                
C                                                                       
            IF (UNBNDD) GOTO 30                                         
               IF (V(I)*THETA .GE. W(I)) GO TO 40                       
                   THETA=W(I)/V(I)                                      
               IHIT=I                                                   
               GOTO  40                                                 
 30            UNBNDD = .FALSE.                                         
C                                                                       
C THIS IS THE FIRST CONSTRAINT ENCOUNTERED THAT EVEN                    
C IS IN THE RIGHT DIRECTION                                             
C                                                                       
               THETA = W(I)/V(I)                                        
               IHIT=I                                                   
 40      CONTINUE                                                       
 50      CONTINUE                                                       
      JDEPS1=JDEPS+1                                                    
      IF (S .LT. JDEPS1) GOTO  90                                       
C                                                                       
C DETERMINE THE DISTANCE TO THE NEAREST SIMPLE CONSTRAINT               
C                                                                       
      DO  80 II = JDEPS1, S                                             
       ET = DSIGN(1.0D0, DFLOAT(ISIMP(II)))                             
       I1 = IABS(ISIMP(II))                                             
       WW = ET*(SIMP(II)-X(I1))                                         
       VV = ET * P(I1)                                                  
       IF (WW.LE.0.0D0) GO TO 60                                        
          IF (VV.LE.EPS.OR.VV*THETA2.GE.WW) GO TO 80                    
              THETA2=WW/VV                                              
              IHIT2=II+M                                                
 60    CONTINUE                                                         
       IF (VV .GE. (-EPS)) GOTO 80                                      
       IF (UNBNDD) GOTO 70                                              
       IF (THETA*VV .GE. WW) GO TO 80                                   
           THETA=WW/VV                                                  
         IHIT=II+M                                                      
         GOTO 80                                                        
 70     UNBNDD = .FALSE.                                                
        THETA = WW/VV                                                   
        IHIT=II+M                                                       
 80      CONTINUE                                                       
 90      CONTINUE                                                       
      IF (.NOT.UNBNDD)RETURN                                            
         THETA=THETA2                                                   
         IHIT=IHIT2                                                     
          IF (IHIT2.EQ.0)IERC=7                                         
 100  RETURN                                                            
      END                                                               
      SUBROUTINE DD4CLM(U, AG, AS, E, UMAX, INDX2, FLAG, DVECS,         
     1   DVECG, IPTG, IPTS, BOUND,IPHAS,KK,N)                           
C                                                                       
C GIVEN THE LAGRANGE MULTIPLIERS, THIS SUBROUTINE DECIDES               
C WHICH CONSTRAINT TO DROP.                                             
C CURRENTLY THE DECISION IS MADE TO DROP THE ONE IWTH THE               
C LARGEST LAGRANGE MULTIPLIER THAT HAS NOT BEEN                         
C DROPPED BEFORE A MINIMUM ON THE CURRENT SUBSPACE HAS                  
C BEEN FOUND                                                            
C                                                                       
      INTEGER AG, AS, E, INDX2, FLAG, DVECG(1)                          
      INTEGER DVECS(1), IPTG(1), IPTS(1)                                
      DOUBLE PRECISION U(1), UMAX, BOUND                                
      INTEGER I, K, INDEX, EP1                                          
      DOUBLE PRECISION  DV5MAX, D1MACH                                  
      INTEGER AGPAS                                                     
       EP1=E+1                                                          
      FLAG=1                                                            
      UMAX = DV5MAX(AG+AS, U(E+1), INDEX)                               
      INDX2 = INDEX+E                                                   
C     IF (UMAX.LT.D1MACH(4)*100.0D0) RETURN                             
      IF (UMAX.LT.0.0D0) RETURN                                         
C                                                                       
C SOMETHING MUST BE DROPPED                                             
C                                                                       
      AGPAS=AG+AS                                                       
      DO 30 I=1,AGPAS                                                   
      FLAG=0                                                            
      IF (KK.EQ.N) RETURN                                               
         IF(INDEX.LE.AG)GO TO 10                                        
C SIMPLE CONSTRAINT                                                     
         K=INDEX-AG                                                     
          IF (DVECS(K).LT.1)RETURN                                      
         GO TO 20                                                       
 10      K=IPTG(INDEX)                                                  
C LOOKING AT A GENERAL CONSTRAINT                                       
         IF (DVECG(K).LT.1)RETURN                                       
 20      U(INDX2)=-1.0D0                                                
         FLAG=1                                                         
         IF(DV5MAX(AGPAS,U(E+1),INDEX).LT.0.0D0) RETURN                 
         INDX2=INDEX+E                                                  
 30   CONTINUE                                                          
      RETURN                                                            
      END                                                               
       SUBROUTINE DD4RPG(M,N,KK,Q,IQ,LT,AG,AS,E,IPTG,INDX2,DVECG,RHS,   
     1  C,IPTS)                                                         
        INTEGER DVECG(1)                                                
       INTEGER M,N,KK,AG,AS,E,IPTG(1),AGM1,AGPE,ITEMP,INDX2,NMS         
       DOUBLE PRECISION Q(IQ, 1), LT( 1),RHS(N), C(N)                   
       INTEGER IPTS(1)                                                  
       AGPE = AG +E                                                     
       AGM1=AGPE-1                                                      
C                                                                       
C THIS SUBROUTINE UPDATES THE VECTORS AND CALLS A                       
C SUBROUTINE TO UPDATE THE LQ DECOMPOSITION WHEN                        
C A GENERAL CONSTRAINT IS DROPPED                                       
C                                                                       
       NMS = N - AS                                                     
       ITEMP = IPTG(INDX2)                                              
       IF (AGPE .EQ. INDX2) GOTO 20                                     
       DO 10 I = INDX2,AGM1                                             
            IPTG(I) = IPTG(I+1)                                         
 10         CONTINUE                                                    
       IPTG(AGPE) = ITEMP                                               
 20    CONTINUE                                                         
       DVECG(ITEMP) = 1                                                 
       IF (AGM1.GT.0) GO TO 50                                          
          DO 40 I=1,NMS                                                 
             DO 30 J=1,NMS                                              
                Q(I,J)=0.0D0                                            
 30         CONTINUE                                                    
            Q(I,I)=1.0D0                                                
            IPAS=I+AS                                                   
            IK=IPTS(IPAS)                                               
            RHS(I)=C(IK)                                                
 40       CONTINUE                                                      
          GO TO 60                                                      
 50       CONTINUE                                                      
       CALL DD4GQR(IQ,NMS,AGPE,Q,LT,INDX2,RHS)                          
 60    AG = AG - 1                                                      
       KK = KK - 1                                                      
       RETURN                                                           
       END                                                              
      SUBROUTINE DD4GQR(K, M, N, Q, R, INDEX,RHS)                       
C                                                                       
C THIS SUBROUTINE UPDATES THE QR DECOMPOSITION OF A MATRIX WHEN         
C COLUMN INDEX IS DELETED FROM THE MATRIX                               
C                                                                       
      INTEGER M                                                         
      INTEGER N, INDEX                                                  
      DOUBLE PRECISION Q(K, 1), R( 1), RHS(K)                           
      INTEGER I                                                         
      DOUBLE PRECISION BETA, ALPHA                                      
C/6S                                                                    
C     IF (M .LE. 0 .OR. N .LE. 0 .OR. M .LT. N) CALL SETERR(            
C    1   25HD4GQR - INVALID DIMENSION, 25, 1, 2)                        
C     IF (INDEX .LE. 0 .OR. N .LT. INDEX) CALL SETERR(                  
C    1   21HD4GQR - INVALID INDEX, 21, 2, 2)                            
C/7S                                                                    
      IF (M .LE. 0 .OR. N .LE. 0 .OR. M .LT. N) CALL SETERR(            
     1   'D4GQR - INVALID DIMENSION', 25, 1, 2)                         
      IF (INDEX .LE. 0 .OR. N .LT. INDEX) CALL SETERR(                  
     1   'D4GQR - INVALID INDEX', 21, 2, 2)                             
C/                                                                      
       N=N-1                                                            
C                                                                       
C ELIMINATE UNWANTED SUBDIAGONAL ELEMENTS                               
C                                                                       
      IF (INDEX.GT.N) RETURN                                            
      IS =(INDEX*(INDEX+1))/2+INDEX                                     
      DO 10 I=INDEX,N                                                   
         CALL DROTG(R(IS), R(IS+1), ALPHA, BETA)                        
         CALL DROT(1,RHS(I),1,RHS(I+1),1,ALPHA,BETA)                    
         CALL DROT(M, Q(I, 1), K, Q(I+1, 1), K, ALPHA, BETA)            
         NMI=N-I                                                        
         IS=IS+I+2                                                      
         CALL DS4ROT(NMI,R(IS-1),I+2,R(IS),I+2,ALPHA,BETA)              
 10    CONTINUE                                                         
C                                                                       
C REARRANGE R                                                           
C                                                                       
       IS2=(INDEX*(INDEX+1))/2+1                                        
       IS1=IS2-INDEX                                                    
       DO 20 I=INDEX,N                                                  
          CALL DCOPY(I,R(IS2),1,R(IS1),1)                               
          IS1=IS1+I                                                     
          IS2=IS2+I+1                                                   
 20    CONTINUE                                                         
      RETURN                                                            
      END                                                               
       SUBROUTINE DD4RPS(A,M,N,IA,AMAN,KK,Q,IQ,LT,AG,AS,E,IPTG,IPTS,    
     1   DVECS,SIMP,ISIMP,INDD2,TEMP,B,RHS)                             
C                                                                       
C THIS SUBROUTINE UPDATES THE VECTORS AND CALLS A SUBROUTINE            
C TO UPDATE THE LQ DECOMPOSTION WHEN A SIMPLE CONSTRAINT IS             
C DROPPED                                                               
C                                                                       
       INTEGER ISIMP(1),IPTG(1),IA(1)                                   
       INTEGER M,N,KK,AG,AS,E,IPTS(1),ITEMP,INDX2,AGPE,DVECS(1)         
       DOUBLE PRECISION A(N),SIMP(N),Q(IQ,1),LT(1),B(N),TEMP(N),RHS(N)  
       DOUBLE PRECISION TEMP1,TOUT, T1                                  
       EXTERNAL AMAN                                                    
       INDX2 = INDD2 - AG-E                                             
        CALL AMAN(.FALSE.,A,IA,N,1,TEMP,TOUT)                           
       ITEMP = IPTS(INDX2)                                              
       ITEMP1 = ISIMP(INDX2)                                            
       TEMP1 = SIMP(INDX2)                                              
       NMS = N - AS                                                     
       AS = AS - 1                                                      
       IF (AS .LT. INDX2) GOTO  20                                      
       DO 10 I = INDX2,AS                                               
            DVECS(I) = DVECS(I+1)                                       
            SIMP(I) = SIMP(I+1)                                         
            ISIMP(I) = ISIMP(I+1)                                       
 10         CONTINUE                                                    
       ISIMP(AS + 1) = ITEMP1                                           
       SIMP(AS+1) = TEMP1                                               
 20    CONTINUE                                                         
       NM1=N-1                                                          
       DO 30 I=INDX2,NM1                                                
          IPTS(I)=IPTS(I+1)                                             
 30    CONTINUE                                                         
       IPTS(N) = ITEMP                                                  
       DVECS(AS+1) = 1                                                  
C                                                                       
C A NEW ROW IS TO BE ADDED TO THE DECOMPXOSTITION                       
C DETERMINE IT                                                          
C                                                                       
       AGPE = AG + E                                                    
       IF (AGPE .EQ. 0) GOTO 50                                         
       DO 40 II = 1, AGPE                                               
            I = IPTG(II)                                                
            CALL AMAN(.FALSE.,A,IA,N,I,TEMP,T1)                         
            B(II) = TEMP(ITEMP)                                         
 40         CONTINUE                                                    
       CALL DD4SQR (IQ, NMS, AGPE, Q, LT, B, RHS)                       
 50    CONTINUE                                                         
       KK = KK - 1                                                      
       RETURN                                                           
       END                                                              
      SUBROUTINE DD4SQR(K, M, N, Q, R, B, RHS)                          
      INTEGER M                                                         
      INTEGER N                                                         
      DOUBLE PRECISION Q(K, 1), R( 1), B(1), RHS(K)                     
      INTEGER I                                                         
      DOUBLE PRECISION BETA, ALPHA,U, X                                 
C                                                                       
C THIS SUBROUTINE UPDATES THE QR DECOMPOSTION WHENE A NEW               
C ROW CONTAINED IN B IS ADDED TO THE MATRIX                             
C                                                                       
       M=M+1                                                            
       MM1=M-1                                                          
C                                                                       
C ZERO OUT ROW AND COLUMN OF Q MATRIX                                   
C                                                                       
       Q(M,M)=1.                                                        
       IF(M.EQ.1)RETURN                                                 
       DO 10 II=1,MM1                                                   
          Q(M,II)=0.0D0                                                 
          Q(II,M)=0.0D0                                                 
 10       CONTINUE                                                      
       X=RHS(M)                                                         
         IF (N.EQ.0) RETURN                                             
         IS=1                                                           
         DO 20 I=1,N                                                    
         CALL DROTG(R(IS), B(I), ALPHA, BETA)                           
         CALL DROT(M, Q(I, 1), K, Q(M, 1), K, ALPHA, BETA)              
         U=RHS(I)                                                       
         RHS(I)=ALPHA*U+BETA*X                                          
         X=-BETA*U+ALPHA*X                                              
         IS=IS+I+1                                                      
         IF (N-I.GE.1)                                                  
     1    CALL DS4ROT(N-I,R(IS-1),I+1,B(I+1),-1,ALPHA,BETA)             
 20     CONTINUE                                                        
      RHS(M)=X                                                          
      RETURN                                                            
      END                                                               
       SUBROUTINE DE4QL(A,N,AMAN,IA,KK,G,IG,DU,B,X,RHS,V,R)             
       INTEGER N,IA(1),KK                                               
       DOUBLE PRECISION A(1),G(IG,1),DU(1),X(N),RHS(N),B(N),V(1),R(1)   
       DOUBLE PRECISION DDOT,SUM2,SUM,TAU,TEMP1                         
       EXTERNAL AMAN                                                    
C                                                                       
C GET THE QR DECOMPOSITION AND GENERATE THE RIGHT HAND SIDE             
C                                                                       
       DO 10   I = 1,KK                                                 
          CALL AMAN (.FALSE.,A,IA,N,I,G(1,I),TEMP1)                     
          RHS(I)=B(I)-DDOT(N,X,1,G(1,I),1)                              
 10       CONTINUE                                                      
          CALL DLST2D(N,N,KK,G,DU)                                      
C                                                                       
C SOLVE THE LEAST SQUARES PROBLEM                                       
C                                                                       
         DO 40 I=1,KK                                                   
            SUM=RHS(I)                                                  
            IM1=I-1                                                     
            IF (I.EQ.1) GO TO 30                                        
            DO 20 J=1,IM1                                               
               SUM=SUM-RHS(J)*G(J,I)                                    
 20         CONTINUE                                                    
 30         RHS(I)=SUM/DU(I)                                            
 40      CONTINUE                                                       
C                                                                       
C PUT THE R FROM THE UPPER TRIANGULAR FORM IN R                         
C                                                                       
        L=1                                                             
        DO 70 I=1,KK                                                    
           IF (I.EQ.1)GO TO 60                                          
           IM1=I-1                                                      
           DO 50 J=1,IM1                                                
              R(L)=G(J,I)                                               
              L=L+1                                                     
 50        CONTINUE                                                     
 60        R(L)=DU(I)                                                   
           L=L+1                                                        
 70      CONTINUE                                                       
C                                                                       
C FORM THE Q INTO G                                                     
C                                                                       
        KKP1=KK+1                                                       
        DO 100 IB=1,KK                                                  
           I=KKP1-IB                                                    
           TAU=DU(I)*G(I,I)                                             
           NMI=N-I                                                      
           SUM2=1.0D0/DU(I)                                             
           IP1=I+1                                                      
           IF (I.EQ.N) GO TO 90                                         
           DO 80 K=IP1,N                                                
              G(I,K)=0.0D0                                              
              SUM=DDOT(NMI,G(IP1,I),1,G(IP1,K),1)/TAU                   
              CALL DAXPY(NMI+1,SUM,G(I,I),1,G(I,K),1)                   
 80        CONTINUE                                                     
           CALL DSCAL(NMI,SUM2,G(IP1,I),1)                              
 90     CONTINUE                                                        
        G(I,I)=1.0D0+SUM2*G(I,I)                                        
 100    CONTINUE                                                        
       DO 110 I=1,N                                                     
          NMI=N-I                                                       
          IF (I.NE.N)CALL DSWAP(NMI,G(I+1,I),1,G(I,I+1),IG)             
 110   CONTINUE                                                         
         CALL DM5TOP(IG,N,G,1,KK,1,N,RHS,2,RHS)                         
         DO 120 I=1,N                                                   
            X(I)=X(I)+RHS(I)                                            
 120     CONTINUE                                                       
          RETURN                                                        
        END                                                             
      SUBROUTINE DG4ETC(N, M, S, AS, AGPE, A, IA, AMAN, X, CC, RHS      
     1   , W, SCALE, Q,IQ,IPTS, ISIMP, SIMP, IPTG, DONE, CTX,           
     2    CNRM,IST,FIRST)                                               
      INTEGER N                                                         
      EXTERNAL AMAN                                                     
       DOUBLE PRECISION DSTACK(500)                                     
       COMMON /CSTAK/DSTACK                                             
      INTEGER M, S, AS, AGPE, IA(1), IPTS(1)                            
      INTEGER ISIMP(1), IPTG(1), IST,ISP1                               
      DOUBLE PRECISION A(1), X(N), CC(N), RHS(N), W(1), SCALE(1),CTX    
      DOUBLE PRECISION CNRM,DDOT                                        
      DOUBLE PRECISION Q(IQ, N), SIMP(1)                                
      LOGICAL DONE, FIRST                                               
      DOUBLE PRECISION DNRM2,EPS                                        
      INTEGER II, IIAS, IK, IABS, I, K                                  
      INTEGER NMS, I1, ASP1                                             
      DOUBLE PRECISION ET, SC, TEMP,DFLOAT, D1MACH, EP1, SI, EP2        
C THIS PROCEDURE OBTAINS THE GRADIENT AND THE PROJECT GRADIENT          
C TO PUSH INFEASIBLE POINTS TO BE MORE FEASIBLE                         
      EP1=D1MACH(4)*DFLOAT(N)*10.0D0                                    
      EPS=DNRM2(N,X,1)*EP1                                              
      IS=IST                                                            
      ASP1=AS+1                                                         
      DO  1 I = 1, N                                                    
         CC(I) = 0.0D0                                                  
   1     CONTINUE                                                       
C FIND VIOLATED GENERAL CONSTRAINTS AND ADJUST GRADIENT                 
 11   DONE=.TRUE.                                                       
      IF (IST.LT.0)GO TO 4                                              
      ISP1=IST+1                                                        
      IF (M .LT. ISP1) GOTO 34                                          
         DO  3 I = ISP1, M                                              
            K = IPTG(I)                                                 
            IF (W(K) .LE. EPS*SCALE(K)) GOTO 2                          
            IF (DONE.AND..NOT.FIRST)IST=I                               
               DONE = .FALSE.                                           
               SC = 1.0D0/SCALE(K)                                      
               CALL AMAN(.FALSE., A, IA, N, K, RHS, TEMP)               
               CALL DAXPY(N, SC, RHS, 1, CC, 1)                         
               IF (.NOT.FIRST)GO TO 77                                  
   2        CONTINUE                                                    
   3        CONTINUE                                                    
C LOOK NOW AT SIMPLE CONSTRAINTS TO FIND THOSE THAT                     
C ARE VIOLATED                                                          
 34   IS=-AS                                                            
   4  ISP1=-IS+1                                                        
      IF (S .LT. ISP1) GOTO 77                                          
         DO  6 I = ISP1, S                                              
            ET = 1.0D0                                                  
            IF (ISIMP(I) .LT. 0) ET = -1.0D0                            
            I1 = IABS(ISIMP(I))                                         
            EP2=EP1                                                     
            IF (SIMP(I).LT.0.0D0)EP2=-EP1                               
            SI=ET*SIMP(I)-EP2*SIMP(I)                                   
            IF (SI.EQ.0.0D0)SI=-EPS                                     
            IF (ET*X(I1) .GE. SI) GOTO 5                                
                 IF (DONE.AND..NOT.FIRST)IST=-I                         
               DONE = .FALSE.                                           
               CC(I1) = CC(I1)+ET                                       
              IF (.NOT.FIRST)GO TO 77                                   
   5        CONTINUE                                                    
   6        CONTINUE                                                    
           IF (DONE)RETURN                                              
           CNRM=DNRM2(N,CC,1)                                           
           IF (CNRM.GT.EP1)GO TO 77                                     
               FIRST=.FALSE.                                            
               GO TO 11                                                 
 77       CONTINUE                                                      
            CTX=DDOT(N,X,1,CC,1)                                        
C PROJECT GRADIENT                                                      
   7  DO  8 II = ASP1, N                                                
         IK = IPTS(II)                                                  
         IIAS = II-AS                                                   
         RHS(IIAS) = CC(IK)                                             
   8     CONTINUE                                                       
      NMS = N-AS                                                        
      CALL DM5TOP(IQ,N,Q,1,NMS,1,NMS,RHS,1,RHS)                         
      RETURN                                                            
      END                                                               
      SUBROUTINE DH4HG(N,U,H,G,K)                                       
C                                                                       
C THIS SUBROUTINE GENERATES A HOUSEHOLD TRANSFORMATION                  
C                                                                       
       DOUBLE PRECISION G,U(N)                                          
       DOUBLE PRECISION H,S,DASUM,SCALE,DSQRT                           
        IF (N.LT.1)RETURN                                               
       SCALE=DASUM(N,U,1)                                               
       S=0.0D0                                                          
       H=1.0D0                                                          
       IF (SCALE.EQ.0.0D0)RETURN                                        
        DO 10 I=1,N                                                     
          U(I)=U(I)/SCALE                                               
          S=S+U(I)*U(I)                                                 
 10    CONTINUE                                                         
       G=DSQRT(S)                                                       
       IF(U(K).GT.0.0D0)G=-G                                            
       H=-(S-G*U(K))                                                    
       U(K)=U(K)-G                                                      
       G=G*SCALE                                                        
       RETURN                                                           
       END                                                              
       SUBROUTINE DL4AGS(A,M,N,IA,AMAN,AS,U1,U2,C,ISIMP,AGPE,IPTS,      
     1   IPTG,TEMP)                                                     
C                                                                       
C THIS SUBROUTINE COMPUTES THE LAGRANGE MULTIPLIERS                     
C FOR SIMPLE ACTIVE CONSTRAINTS                                         
C                                                                       
       INTEGER N,IA(1),AS,ISIMP(1),AGPE,IPTS(1),IPTG(1),J,M             
       DOUBLE PRECISION U1(1),U2(1),C(1),A(1),TEMP(N),T                 
       EXTERNAL AMAN                                                    
       DO 10 II=1,AS                                                    
          I=IPTS(II)                                                    
          U2(II)=C(I)                                                   
 10       CONTINUE                                                      
       IF (AGPE .EQ.0) GOTO 40                                          
       DO 30 II = 1, AGPE                                               
            I = IPTG(II)                                                
            CALL AMAN (.FALSE.,A,IA,N,I,TEMP,T)                         
            DO 20 JJ=1,AS                                               
               J=IPTS(JJ)                                               
               U2(JJ) =  U2(JJ) -U1(II) * TEMP(J)                       
 20            CONTINUE                                                 
 30         CONTINUE                                                    
 40    CONTINUE                                                         
C                                                                       
C ADJUST SIGNS FOR UPPER BOUNDS                                         
C                                                                       
       DO 50 II=1,AS                                                    
          IF (ISIMP(II).LT.0)U2(II)=-U2(II)                             
 50    CONTINUE                                                         
       RETURN                                                           
       END                                                              
        SUBROUTINE DM4TOP(N,R,RHS,X)                                    
C                                                                       
C THIS SUBROUTINE SOLVES AN UPPER TRIANGULAR SYSTEM                     
C STORED IN 1 VECTOR                                                    
        DOUBLE PRECISION RHS(N),X(N),R(N), T                            
        L=(N*(N+1))/2                                                   
        DO 10 I=1,N                                                     
           X(I)=-RHS(I)                                                 
 10     CONTINUE                                                        
        L=(N*(N+1))/2                                                   
        NP1=N+1                                                         
        DO 30 IB=1,N                                                    
           I=NP1-IB                                                     
           X(I)=-X(I)/R(L)                                              
           T=X(I)                                                       
           L=L-1                                                        
           J=I-1                                                        
 20        IF (J.LE.0)GO TO 30                                          
           X(J)=X(J)+T*R(L)                                             
           L=L-1                                                        
           J=J-1                                                        
           GO TO 20                                                     
 30     CONTINUE                                                        
        RETURN                                                          
        END                                                             
      SUBROUTINE DP4RJD(N, AS, AG, E, Q,IQ, IPTS, C, P, TEMP, RHS)      
       INTEGER N                                                        
      INTEGER AS, AG, IPTS(1)                                           
      DOUBLE PRECISION Q(IQ, 1), C(N),RHS(N)                            
      INTEGER NMS, ASP1, E, I, K, AGPE                                  
      DOUBLE PRECISION P(N), TEMP(1)                                    
      NMS = N-AS                                                        
      ASP1 = AS + 1                                                     
      AGPE = AG+E                                                       
       IAGP1=AGPE+1                                                     
       IF (AS.EQ.0 ) GO TO 20                                           
       DO 10 I=1,AS                                                     
          K=IPTS(I)                                                     
          P(K)=0.0D0                                                    
 10    CONTINUE                                                         
 20    CONTINUE                                                         
          DO 30 I=ASP1,N                                                
             TEMP(I)=0.0D0                                              
 30       CONTINUE                                                      
         IF (AGPE.GT.0)CALL DM5TOP(IQ,N,Q,1,AGPE,1,NMS,RHS,2,TEMP(ASP1))
          DO 40 I=ASP1,N                                                
             K=IPTS(I)                                                  
             P(K)=C(K)-TEMP(I)                                          
 40       CONTINUE                                                      
          RETURN                                                        
          END                                                           
      SUBROUTINE DS4ROT(N,X,INCX,Y,INCY,A,B)                            
C                                                                       
C THIS SUBROUTINE IS DROT WITH THE OPTION OF GOING                      
C A VARIED AMOUNT IN INCREMENTS                                         
C                                                                       
        DOUBLE PRECISION X(1),Y(1)                                      
        DOUBLE PRECISION A,B,W,V                                        
        IF (N.LT.1)RETURN                                               
        IX=IABS(INCX)                                                   
        IY=IABS(INCY)                                                   
        L=1                                                             
        K=1                                                             
        DO 10 I=1,N                                                     
           W=X(L)                                                       
           V=Y(K)                                                       
           X(L)=A*W+B*V                                                 
           Y(K)=-B*W+A*V                                                
           L=L+IX                                                       
           K=K+IY                                                       
           IF (INCX.GT.0)IX=IX+1                                        
           IF (INCY.GT.0)IY=IY+1                                        
 10    CONTINUE                                                         
       RETURN                                                           
       END                                                              
      REAL FUNCTION FMIN(F,X,A,B,T)                                     
C                                                                       
C  FMIN FINDS AN APPROXIMATION X TO THE POINT IN                        
C  THE INTERVAL (A,B) AT WHICH F ATTAINS ITS MINIMUM,                   
C  AND RETURNS IN FMIN THE VALUE OF F AT X.                             
C                                                                       
C  T DETERMINES A TOLERANCE OF                                          
C                                                                       
C              TOL  =  R1MACH(4) * ABS(X) + T                           
C                                                                       
C  AND F IS NEVER EVALUATED AT TWO POINTS CLOSER                        
C  TOGETHER THAN TOL.                                                   
C                                                                       
C  IF T IS INPUT .LE. ZERO, IT IS SET TO 10.*R1MACH(1)                  
C                                                                       
C  THE METHOD USED IS A COMBINATION OF GOLDEN SEARCH                    
C  AND SUCCESSIVE PARABOLIC INTERPOLATION.                              
C  CONVERGENCE IS NEVER MUCH SLOWER THAN FOR A                          
C  FIBONACCI SEARCH.                                                    
C  IF F HAS A CONTINUOUS SECOND DERIVATIVE WHICH IS POSITIVE            
C  AT THE MINIMUM ( NOT AT A OR B) THEN, IGNORING                       
C  ROUNDING ERRORS, CONVERGENCE IS SUPERLINEAR,                         
C  AND USUALLY THE ORDER IS AT LEAST 1.3247....                         
C                                                                       
C  THIS IS BRENT'S ALGORITHM - SEE PAGE 188 OF HIS BOOK.                
C                                                                       
C  A, STORED IN SA, AND B, STORED IN SB ARE                             
C  AT ANY STEP THE CURRENT BOUNDARIES FOR                               
C  THE INTERVAL CONTAINING THE MINIMUM.                                 
C                                                                       
C  X IS THE POINT AT WHICH F HAS THE LEAST VALUE                        
C  SO FAR, (OR THE POINT OF MOST RECENT EVALUATION                      
C  IF THERE IS A TIE).                                                  
C                                                                       
C  W IS THE POINT WITH THE NEXT LOWEST VALUE OF F                       
C                                                                       
C  V IS THE PREVIOUS VALUE OF W                                         
C                                                                       
C  U IS THE LAST POINT AT WHICH F HAS BEEN EVALUATED                    
C   (U IS UNDEFINED THE FIRST TIME.)                                    
C                                                                       
      REAL A,B,R1MACH,T,F,X,SA,SB,D,E,M,P,Q,R                           
      REAL TOL,T2,TT,U,V,W,FU,FV,FW,FX,CONS                             
      EXTERNAL F                                                        
      TT = T                                                            
      IF (T .LE. 0.0) TT = 10.*R1MACH(1)                                
      IF (A .LT. B) GO TO 5                                             
      SA = B                                                            
      SB = A                                                            
      GO TO 8                                                           
    5 SA = A                                                            
      SB = B                                                            
    8 CONS = .5*(3.0-SQRT(5.0))                                         
C                                                                       
C  ARBITRARILY FOR THE FIRST STEP CHOOSE                                
C                                                                       
C     X = A + .5(3-SQRT(5))* (B-A)                                      
C                                                                       
      X = SA + CONS*(SB - SA)                                           
      W = X                                                             
      V = W                                                             
      E = 0.0                                                           
      FX = F(X)                                                         
      FW = FX                                                           
      FV = FW                                                           
C                                                                       
C  THE MAIN LOOP STARTS HERE.                                           
C                                                                       
   10 M = 0.5*(SA + SB)                                                 
      TOL = R1MACH(4) * ABS(X) + TT                                     
      T2 = 2.0 * TOL                                                    
C                                                                       
C  CHECK THE STOPPING CRITERION:                                        
C        (M = MIDPOINT)                                                 
C  IF ABS(X-M) .LE. (2*TOL - .5(B-A)),                                  
C  I.E. IF MAX(X-A, B-X) .LE. 2*TOL, THEN                               
C  THE PROCEDURE TERMINATES WITH X AS THE                               
C  APPROXIMATE POSITION OF THE MINIMUM.                                 
C                                                                       
      IF (ABS(X-M) .LE. T2-0.5*(SB-SA)) GO TO 190                       
      R = 0.0                                                           
      Q = R                                                             
      P = Q                                                             
      IF (ABS(E) .LE. TOL) GO TO 40                                     
C                                                                       
C    FIT THE PARABOLA                                                   
C                                                                       
C    Q = 2((X-V)(FX-FW) - (X-W)(FX-FV))                                 
C    P = ((X-V)**2)(FX-FW) - ((X-W))**2)(FX-FV)                         
C                                                                       
      R = (X-W)*(FX-FV)                                                 
      Q = (X-V)*(FX-FW)                                                 
      P = (X-V)*Q - (X-W) * R                                           
      Q = 2.0*(Q-R)                                                     
      IF (Q .LE. 0.0) GO TO 20                                          
      P = -P                                                            
      GO TO 30                                                          
C                                                                       
   20 Q = -Q                                                            
   30 R = E                                                             
      E = D                                                             
C                                                                       
C  HERE E IS THE VALUE OF P/Q AT THE SECOND LAST                        
C  CYCLE; IF ABS(E) .LE. TOL, OR IF Q = 0.0.                            
C  OR IF X+P/Q LIES OUTSIDE OF (A,B), OR                                
C  ABS(P/Q) .GE. .5E, THEN A "GOLDEN                                    
C  SECTION" STEP IS PERFORMED (AT 60 BELOW).                            
C                                                                       
C  OTHERWISE A PARABOLIC INTERPOLATION                                  
C  STEP IS TAKEN                                                        
C                                                                       
   40 IF (ABS(P).GE.ABS(0.5*Q*R)) GO TO 60                              
      IF ((P.LE.Q*(SA-X)).OR.(P.GE.Q*(SB-X))) GO TO 60                  
      D = P/Q                                                           
      U = X + D                                                         
C                                                                       
C  EXCEPT F MUST NOT BE EVALUATED TOO CLOSE TO A OR B.                  
C                                                                       
C  IF THE NEW POINT IS TOO CLOSE JUST PUT                               
C       D = +TOL   IF X.LT.M                                            
C       D = -TOL   IF X.GE.M                                            
C                                                                       
      IF((U-SA.GE.T2).AND.(SB-U.GE.T2)) GO TO 90                        
      IF (X.GE.M) GO TO 50                                              
      D = TOL                                                           
      GO TO 90                                                          
   50 D = -TOL                                                          
      GO TO 90                                                          
C                                                                       
C  THIS IS THE "GOLDEN SECTION" STEP:                                   
C                                                                       
C       U = .5(SQRT(5)-1)X + .5(3-SQRT(5)A   IF X.GE.M                  
C       U = .5(SQRT(5)-1)X + .5(3-SQRT(5)B   IF X.LT.M                  
C                                                                       
   60 IF (X.GE.M) GO TO 70                                              
      E = SB - X                                                        
      GO TO 80                                                          
   70 E = SA - X                                                        
   80 D = CONS*E                                                        
C                                                                       
C     U = X+(IF ABS(D).GE.TOL THEN D,                                   
C          ELSE IF D.GT.0 THEN TOL     ELSE  -TOL)                      
C                                                                       
   90 IF (ABS(D).LT.TOL) GO TO 100                                      
      U = X + D                                                         
      GO TO 120                                                         
  100 IF (D.LE.0.0) GO TO 110                                           
      U = X + TOL                                                       
      GO TO 120                                                         
  110 U = X - TOL                                                       
C                                                                       
C  UPDATE EVERYTHING                                                    
C  IF FU.LE.FX THEN                                                     
C     BEGIN IF U.LT.X THEN B = X ELSE A = X                             
C     V = W;FV = FW;W = X;FW = FX;X = U;FX = FU                         
C     END                                                               
C  ELSE                                                                 
C     BEGIN IF U.LT.X THEN A = U ELSE B = U                             
C     IF FU.LE.FW OR W = X THEN                                         
C          BEGIN V = W;FV = FW;W = X;FW = FU END                        
C     ELSE IF FU.LE.FV OR V = X OR V = W THEN                           
C            BEGIN V = U; FV = FU                                       
C            END                                                        
C     END                                                               
C                                                                       
  120 FU = F(U)                                                         
      IF (FU.GT.FX) GO TO 150                                           
      IF (U.GE.X) GO TO 130                                             
      SB = X                                                            
      GO TO 140                                                         
  130 SA = X                                                            
  140 V = W                                                             
      FV = FW                                                           
      W = X                                                             
      FW = FX                                                           
      X = U                                                             
      FX = FU                                                           
      GO TO 10                                                          
C                                                                       
  150 IF (U.GE.X) GO TO 160                                             
      SA = U                                                            
      GO TO 170                                                         
  160 SB = U                                                            
  170 IF ((FU.GT.FW) .AND.(W.NE.X)) GO TO 180                           
      V = W                                                             
      FV = FW                                                           
      W = U                                                             
      FW = FU                                                           
      GO TO 10                                                          
C                                                                       
  180 IF ((FU.GT.FV).AND.(V.NE.X).AND.(V.NE.W)) GO TO 10                
      V = U                                                             
      FV = FU                                                           
      GO TO 10                                                          
C                                                                       
  190 FMIN = FX                                                         
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DFMIN(F,X,A,B,T)                        
C                                                                       
C  DFMIN FINDS AN APPROXIMATION X TO THE POINT IN                       
C  THE INTERVAL (A,B) AT WHICH F ATTAINS ITS MINIMUM,                   
C  AND RETURNS IN DFMIN THE VALUE OF F AT X.                            
C                                                                       
C  T DETERMINES A TOLERANCE OF                                          
C                                                                       
C              TOL  =  D1MACH(4) * DABS(X) + T                          
C                                                                       
C  AND F IS NEVER EVALUATED AT TWO POINTS CLOSER                        
C  TOGETHER THAN TOL.                                                   
C                                                                       
C  IF T IS INPUT .LE. ZERO, IT IS SET TO 10.*D1MACH(1)                  
C                                                                       
C  THE METHOD USED IS A COMBINATION OF GOLDEN SEARCH                    
C  AND SUCCESSIVE PARABOLIC INTERPOLATION.                              
C  CONVERGENCE IS NEVER MUCH SLOWER THAN FOR A                          
C  FIBONACCI SEARCH.                                                    
C  IF F HAS A CONTINUOUS SECOND DERIVATIVE WHICH IS POSITIVE            
C  AT THE MINIMUM ( NOT AT A OR B) THEN, IGNORING                       
C  ROUNDING ERRORS, CONVERGENCE IS SUPERLINEAR,                         
C  AND USUALLY THE ORDER IS AT LEAST 1.3247....                         
C                                                                       
C  THIS IS BRENT'S ALGORITHM - SEE PAGE 188 OF HIS BOOK.                
C                                                                       
C  A, STORED IN SA, AND B, STORED IN SB ARE                             
C  AT ANY STEP THE CURRENT BOUNDARIES FOR                               
C  THE INTERVAL CONTAINING THE MINIMUM.                                 
C                                                                       
C  X IS THE POINT AT WHICH F HAS THE LEAST VALUE                        
C  SO FAR, (OR THE POINT OF MOST RECENT EVALUATION                      
C  IF THERE IS A TIE).                                                  
C                                                                       
C  W IS THE POINT WITH THE NEXT LOWEST VALUE OF F                       
C                                                                       
C  V IS THE PREVIOUS VALUE OF W                                         
C                                                                       
C  U IS THE LAST POINT AT WHICH F HAS BEEN EVALUATED                    
C   (U IS UNDEFINED THE FIRST TIME.)                                    
C                                                                       
      DOUBLE PRECISION A,B,D1MACH,T,F,X,SA,SB,D,E,M,P,Q,R               
      DOUBLE PRECISION TOL,T2,TT,U,V,W,FU,FV,FW,FX,CONS                 
      EXTERNAL F                                                        
      TT = T                                                            
      IF (T .LE. 0.D0 ) TT = 10.D0*D1MACH(1)                            
      IF (A .LT. B) GO TO 5                                             
      SA = B                                                            
      SB = A                                                            
      GO TO 8                                                           
    5 SA = A                                                            
      SB = B                                                            
    8 CONS = .5D0*(3.0D0-DSQRT(5.0D0))                                  
C                                                                       
C  ARBITRARILY FOR THE FIRST STEP CHOOSE                                
C                                                                       
C     X = A + .5(3-DSQRT(5))* (B-A)                                     
C                                                                       
      X = SA + CONS*(SB - SA)                                           
      W = X                                                             
      V = W                                                             
      E = 0.0D0                                                         
      FX = F(X)                                                         
      FW = FX                                                           
      FV = FW                                                           
C                                                                       
C  THE MAIN LOOP STARTS HERE.                                           
C                                                                       
   10 M = 0.5D0*(SA + SB)                                               
      TOL = D1MACH(4) * DABS(X) + TT                                    
      T2 = 2.0D0 * TOL                                                  
C                                                                       
C  CHECK THE STOPPING CRITERION:                                        
C        (M = MIDPOINT)                                                 
C  IF DABS(X-M) .LE. (2*TOL - .5(B-A)),                                 
C  I.E. IF MAX(X-A, B-X) .LE. 2*TOL, THEN                               
C  THE PROCEDURE TERMINATES WITH X AS THE                               
C  APPROXIMATE POSITION OF THE MINIMUM.                                 
C                                                                       
      IF (DABS(X-M) .LE. T2-0.5D0*(SB-SA)) GO TO 190                    
      R = 0.0D0                                                         
      Q = R                                                             
      P = Q                                                             
      IF (DABS(E) .LE. TOL) GO TO 40                                    
C                                                                       
C    FIT THE PARABOLA                                                   
C                                                                       
C    Q = 2((X-V)(FX-FW) - (X-W)(FX-FV))                                 
C    P = ((X-V)**2)(FX-FW) - ((X-W))**2)(FX-FV)                         
C                                                                       
      R = (X-W)*(FX-FV)                                                 
      Q = (X-V)*(FX-FW)                                                 
      P = (X-V)*Q - (X-W) * R                                           
      Q = 2.0D0*(Q-R)                                                   
      IF (Q .LE. 0.0D0) GO TO 20                                        
      P = -P                                                            
      GO TO 30                                                          
C                                                                       
   20 Q = -Q                                                            
   30 R = E                                                             
      E = D                                                             
C                                                                       
C  HERE E IS THE VALUE OF P/Q AT THE SECOND LAST                        
C  CYCLE; IF DABS(E) .LE. TOL, OR IF Q = 0.0.                           
C  OR IF X+P/Q LIES OUTSIDE OF (A,B), OR                                
C  DABS(P/Q) .GE. .5E, THEN A "GOLDEN                                   
C  SECTION" STEP IS PERFORMED (AT 60 BELOW).                            
C                                                                       
C  OTHERWISE A PARABOLIC INTERPOLATION                                  
C  STEP IS TAKEN                                                        
C                                                                       
   40 IF (DABS(P).GE.DABS(0.5D0*Q*R)) GO TO 60                          
      IF ((P.LE.Q*(SA-X)).OR.(P.GE.Q*(SB-X))) GO TO 60                  
      D = P/Q                                                           
      U = X + D                                                         
C                                                                       
C  EXCEPT F MUST NOT BE EVALUATED TOO CLOSE TO A OR B.                  
C                                                                       
C  IF THE NEW POINT IS TOO CLOSE JUST PUT                               
C       D = +TOL   IF X.LT.M                                            
C       D = -TOL   IF X.GE.M                                            
C                                                                       
      IF((U-SA.GE.T2).AND.(SB-U.GE.T2)) GO TO 90                        
      IF (X.GE.M) GO TO 50                                              
      D = TOL                                                           
      GO TO 90                                                          
   50 D = -TOL                                                          
      GO TO 90                                                          
C                                                                       
C  THIS IS THE "GOLDEN SECTION" STEP:                                   
C                                                                       
C       U = .5(SQRT(5)-1)X + .5(3-SQRT(5)A   IF X.GE.M                  
C       U = .5(SQRT(5)-1)X + .5(3-SQRT(5)B   IF X.LT.M                  
C                                                                       
   60 IF (X.GE.M) GO TO 70                                              
      E = SB - X                                                        
      GO TO 80                                                          
   70 E = SA - X                                                        
   80 D = CONS*E                                                        
C                                                                       
C     U = X+(IF DABS(D).GE.TOL THEN D,                                  
C          ELSE IF D.GT.0 THEN TOL     ELSE  -TOL)                      
C                                                                       
   90 IF (DABS(D).LT.TOL) GO TO 100                                     
      U = X + D                                                         
      GO TO 120                                                         
  100 IF (D.LE.0.0D0) GO TO 110                                         
      U = X + TOL                                                       
      GO TO 120                                                         
  110 U = X - TOL                                                       
C                                                                       
C  UPDATE EVERYTHING                                                    
C  IF FU.LE.FX THEN                                                     
C     BEGIN IF U.LT.X THEN B = X ELSE A = X                             
C     V = W;FV = FW;W = X;FW = FX;X = U;FX = FU                         
C     END                                                               
C  ELSE                                                                 
C     BEGIN IF U.LT.X THEN A = U ELSE B = U                             
C     IF FU.LE.FW OR W = X THEN                                         
C          BEGIN V = W;FV = FW;W = X;FW = FU END                        
C     ELSE IF FU.LE.FV OR V = X OR V = W THEN                           
C            BEGIN V = U; FV = FU                                       
C            END                                                        
C     END                                                               
C                                                                       
  120 FU = F(U)                                                         
      IF (FU.GT.FX) GO TO 150                                           
      IF (U.GE.X) GO TO 130                                             
      SB = X                                                            
      GO TO 140                                                         
  130 SA = X                                                            
  140 V = W                                                             
      FV = FW                                                           
      W = X                                                             
      FW = FX                                                           
      X = U                                                             
      FX = FU                                                           
      GO TO 10                                                          
C                                                                       
  150 IF (U.GE.X) GO TO 160                                             
      SA = U                                                            
      GO TO 170                                                         
  160 SB = U                                                            
  170 IF ((FU.GT.FW) .AND.(W.NE.X)) GO TO 180                           
      V = W                                                             
      FV = FW                                                           
      W = U                                                             
      FW = FU                                                           
      GO TO 10                                                          
C                                                                       
  180 IF ((FU.GT.FV).AND.(V.NE.X).AND.(V.NE.W)) GO TO 10                
      V = U                                                             
      FV = FU                                                           
      GO TO 10                                                          
C                                                                       
  190 DFMIN = FX                                                        
      RETURN                                                            
      END                                                               
C****END OF ROUTINES NEEDED FOR PORT 3 OPTIMIZATION CHAPTER*************
