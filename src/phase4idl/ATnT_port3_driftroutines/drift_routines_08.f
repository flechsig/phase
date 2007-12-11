      SUBROUTINE RPOLY(DEGREE,COEFF,ZEROR,ZEROI)                        
C                                                                       
C FINDS THE ZEROS OF A POLYNOMIAL WITH REAL COEFFICIENTS.               
C                                                                       
C  INPUTS -                                                             
C                                                                       
C     COEFF         -  REAL VECTOR OF THE COEFFICIENTS                  
C                      IN ORDER OF DECREASING POWERS.                   
C                                                                       
C     DEGREE        -  INTEGER DEGREE OF POLYNOMIAL.                    
C                                                                       
C  OUTPUTS -                                                            
C                                                                       
C     ZEROR, ZEROI  -  REAL VECTORS OF THE REAL                         
C                      AND IMAGINARY PARTS OF THE ZEROS.                
C                                                                       
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1    - DEGREE IS LESS THAN 1                                       
C    2    - LEADING COEFFICIENT IS ZERO                                 
C    3    - THE DYNAMIC STORAGE STACK IS NOT BIG ENOUGH                 
C    NNN  - ONLY NNN (.LT. DEGREE) ZEROS HAVE BEEN                      
C           FOUND (RECOVERABLE)                                         
C                                                                       
C                                                                       
C PORT NOTE -                                                           
C                                                                       
C THE ORIGINAL PROGRAM HAS BEEN ADAPTED TO PORT BY -                    
C                                                                       
C   (1) PUTTING IN AUTOMATIC ERROR HANDLING.                            
C   (2) SUBSTITUTING DYNAMIC STACK ALLOCATION FOR THE DIMENSIONED       
C       ARRAYS IN NAMED COMMON.                                         
C   (3) CHANGING THE NAMES OF THE INTERNAL ROUTINES TO AVOID USER       
C       NAME CONFLICT.                                                  
C                                                                       
C  THE FOLLOWING NAME EQUIVALENCES (ORIGINAL - NEW(S.P.)) APPLY -       
C                                                                       
C            RPOLY    -   R1RPLY                                        
C            CALCSC   -   R2RPLY                                        
C            FXSHFR   -   R3RPLY                                        
C            NEWEST   -   R4RPLY                                        
C            NEXTK    -   R5RPLY                                        
C            QUAD     -   R6RPLY                                        
C            QUADIT   -   R7RPLY                                        
C            QUADSD   -   R8RPLY                                        
C            REALIT   -   R9RPLY                                        
C                                                                       
C                                                                       
C DYNAMIC STORAGE SPACE USED -                                          
C                                                                       
C    THE RPOLY PROGRAMS USE 7*(DEGREE+1)                                
C    REAL LOCATIONS IN THE DYNAMIC STORAGE STACK.                       
C                                                                       
C COMMON AREA                                                           
      COMMON/CSTAK/DSTAK(500)                                           
C                                                                       
      INTEGER DEGREE                                                    
      REAL RSTAK(1000)                                                  
      DOUBLE PRECISION DSTAK                                            
      REAL COEFF(1),ZEROR(1),ZEROI(1)                                   
      LOGICAL FAIL                                                      
C                                                                       
      EQUIVALENCE (DSTAK(1),RSTAK(1))                                   
C                                                                       
C THE DEGREE IS LESS THAN 1                                             
C                                                                       
C/6S                                                                    
C     IF (DEGREE .LT. 1) CALL SETERR(                                   
C    1   34H RPOLY - THE DEGREE IS LESS THAN 1,34,1,2)                  
C/7S                                                                    
      IF (DEGREE .LT. 1) CALL SETERR(                                   
     1   ' RPOLY - THE DEGREE IS LESS THAN 1',34,1,2)                   
C/                                                                      
C                                                                       
C ALGORITHM FAILS IF THE LEADING COEFFICIENT IS ZERO.                   
C                                                                       
C/6S                                                                    
C     IF (COEFF(1) .EQ. 0.0E0) CALL SETERR(                             
C    1   36H RPOLY - LEADING COEFFICIENT IS ZERO,36,2,2)                
C/7S                                                                    
      IF (COEFF(1) .EQ. 0.0E0) CALL SETERR(                             
     1   ' RPOLY - LEADING COEFFICIENT IS ZERO',36,2,2)                 
C/                                                                      
C                                                                       
C SET UP THE STORAGE IN THE DYNAMIC STORAGE STACK -                     
C IF THERE IS ROOM                                                      
C                                                                       
      NN = DEGREE + 1                                                   
C                                                                       
      NLEFT = ISTKQU(3) - 7*NN                                          
C/6S                                                                    
C     IF (NLEFT .LE. 0) CALL SETERR(                                    
C    1   47H RPOLY - THE DYNAMIC STORAGE LEFT IS NOT ENOUGH,47,3,2)     
C/7S                                                                    
      IF (NLEFT .LE. 0) CALL SETERR(                                    
     1   ' RPOLY - THE DYNAMIC STORAGE LEFT IS NOT ENOUGH',47,3,2)      
C/                                                                      
C                                                                       
      NNN = 7*NN                                                        
      IP    = ISTKGT(NNN,3)                                             
      IQP   = IP    + NN                                                
      IK    = IQP   + NN                                                
      IQK   = IK    + NN                                                
      ISVK  = IQK   + NN                                                
      ITEMP = ISVK  + NN                                                
      IPT   = ITEMP + NN                                                
C                                                                       
C                                                                       
C  CALL THE JENKINS-TRAUB ROUTINE (OLD RPOLY IS NOW R1RPLY) WITH        
C  THE STACK LOCATIONS INCLUDED IN THE CALL.                            
C                                                                       
      CALL R1RPLY(COEFF,DEGREE,ZEROR,ZEROI,FAIL,                        
     1  RSTAK(IP),RSTAK(IQP),RSTAK(IK),RSTAK(IQK),RSTAK(ISVK),          
     2  RSTAK(ITEMP),RSTAK(IPT))                                        
C                                                                       
C  SEE IF THE THING FAILED                                              
C                                                                       
      IF (.NOT. FAIL)  GO TO 10                                         
C                                                                       
C  OTHERWISE FIGURE HOW MANY ROOTS WERE FOUND                           
C                                                                       
      KP10 = DEGREE+10                                                  
C                                                                       
C/6S                                                                    
C     CALL SETERR(                                                      
C    1   37H RPOLY - ONLY K ZEROS HAVE BEEN FOUND,37,KP10,1)            
C/7S                                                                    
      CALL SETERR(                                                      
     1   ' RPOLY - ONLY K ZEROS HAVE BEEN FOUND',37,KP10,1)             
C/                                                                      
C                                                                       
  10  CALL ISTKRL(1)                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE R1RPLY(OP, DEGREE, ZEROR, ZEROI,                       
     1              FAIL, P, QP, K, QK, SVK, TEMP, PT)                  
C FINDS THE ZEROS OF A REAL POLYNOMIAL                                  
C OP  - REAL VECTOR OF COEFFICIENTS IN                                  
C       ORDER OF DECREASING POWERS.                                     
C DEGREE   - INTEGER DEGREE OF POLYNOMIAL.                              
C ZEROR, ZEROI - OUTPUT REAL VECTORS OF                                 
C                ZEROS.                                                 
C FAIL  - TRUE IF D1RPLY HAS FOUND FEWER THAN                           
C         DEGREE NUMBER OF ZEROS.                                       
C         IN THIS CASE, DEGREE IS RESET TO                              
C         THE NUMBER OF ZEROS FOUND.                                    
C                                                                       
C THE SUBROUTINE USES SINGLE PRECISION CALCULATIONS                     
C FOR SCALING, BOUNDS AND ERROR CALCULATIONS. ALL                       
C CALCULATIONS FOR THE ITERATIONS ARE DONE IN DOUBLE                    
C PRECISION.                                                            
C                                                                       
      COMMON /P66PLY/ SR, SI, U,                                        
     1 V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                      
     2 H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                      
C                                                                       
      INTEGER N, NN                                                     
      INTEGER DEGREE, CNT, NZ, I, J, JJ, NM1                            
      REAL ETA, ARE, MRE                                                
      REAL PT(1), LO, MAX, MIN, XX, YY, COSR,                           
     1 SINR, XXX, X, SC, BND, XM, FF, DF, DX, INFIN,                    
     2 SMALNO, BASE                                                     
C                                                                       
      REAL P(3), QP(1), K(1),                                           
     1 QK(1), SVK(1), SR, SI, U, V, A, B, C, D,                         
     2 A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                        
     3 LZR, LZI                                                         
C                                                                       
      REAL OP(1), TEMP(1),                                              
     1 ZEROR(1), ZEROI(1), T, AA, BB, CC, ABS,                          
     2 FACTOR                                                           
      LOGICAL FAIL, ZEROK                                               
C THE FOLLOWING STATEMENTS SET MACHINE CONSTANTS USED                   
C IN VARIOUS PARTS OF THE PROGRAM. THE MEANING OF THE                   
C FOUR CONSTANTS ARE...                                                 
C ETA     THE MAXIMUM RELATIVE REPRESENTATION ERROR                     
C         WHICH CAN BE DESCRIBED AS THE SMALLEST                        
C         POSITIVE FLOATING POINT NUMBER SUCH THAT                      
C         1.E0+ETA IS GREATER THAN 1.                                   
C INFINY  THE LARGEST FLOATING-POINT NUMBER.                            
C SMALNO  THE SMALLEST POSITIVE FLOATING-POINT NUMBER                   
C         IF THE EXPONENT RANGE DIFFERS IN SINGLE AND                   
C         REAL THEN SMALNO AND INFIN                                    
C         SHOULD INDICATE THE SMALLER RANGE.                            
C                                                                       
C PORT NOTE -                                                           
C     FOR THE ABOVE REASON, THE MACHINE-CONSTANT                        
C     ROUTINE R1MACH HAS BEEN USED FOR THESE TWO VALUES.                
C                                                                       
C BASE    THE BASE OF THE FLOATING-POINT NUMBER                         
C         SYSTEM USED.                                                  
C                                                                       
      BASE = I1MACH(10)                                                 
      ETA = R1MACH(4)                                                   
      INFIN = R1MACH(2)                                                 
      SMALNO = R1MACH(1)                                                
C ARE AND MRE REFER TO THE UNIT ERROR IN + AND *                        
C RESPECTIVELY. THEY ARE ASSUMED TO BE THE SAME AS                      
C ETA.                                                                  
      ARE = ETA                                                         
      MRE = ETA                                                         
      LO = SMALNO/ETA                                                   
C INITIALIZATION OF CONSTANTS FOR SHIFT ROTATION                        
      XX = .5*SQRT(2.)                                                  
      YY = -XX                                                          
C                                                                       
      ANG = 94./180.*(4.*ATAN(1.))                                      
      COSR = COS(ANG)                                                   
      SINR = SIN(ANG)                                                   
      FAIL = .FALSE.                                                    
      N = DEGREE                                                        
      NN = N + 1                                                        
C                                                                       
C REMOVE THE ZEROS AT THE ORIGIN IF ANY                                 
   10 IF (OP(NN).NE.0.0E0) GO TO 20                                     
      J = DEGREE - N + 1                                                
      ZEROR(J) = 0.E0                                                   
      ZEROI(J) = 0.E0                                                   
      NN = NN - 1                                                       
      N = N - 1                                                         
      GO TO 10                                                          
C MAKE A COPY OF THE COEFFICIENTS                                       
   20 DO 30 I=1,NN                                                      
        P(I) = OP(I)                                                    
   30 CONTINUE                                                          
C START THE ALGORITHM FOR ONE ZERO                                      
   40 IF (N.GT.2) GO TO 60                                              
      IF (N.LT.1) RETURN                                                
C CALCULATE THE FINAL ZERO OR PAIR OF ZEROS                             
      IF (N.EQ.2) GO TO 50                                              
      ZEROR(DEGREE) = -P(2)/P(1)                                        
      ZEROI(DEGREE) = 0.0E0                                             
      RETURN                                                            
   50 CALL R6RPLY(P(1), P(2), P(3), ZEROR(DEGREE-1),                    
     1 ZEROI(DEGREE-1), ZEROR(DEGREE), ZEROI(DEGREE))                   
      RETURN                                                            
C FIND LARGEST AND SMALLEST MODULI OF COEFFICIENTS.                     
   60 MAX = 0.                                                          
      MIN = INFIN                                                       
      DO 70 I=1,NN                                                      
        X = ABS((P(I)))                                                 
        IF (X.GT.MAX) MAX = X                                           
        IF (X.NE.0. .AND. X.LT.MIN) MIN = X                             
   70 CONTINUE                                                          
C SCALE IF THERE ARE LARGE OR VERY SMALL COEFFICIENTS                   
C COMPUTES A SCALE FACTOR TO MULTIPLY THE                               
C COEFFICIENTS OF THE POLYNOMIAL. THE SCALING IS DONE                   
C TO AVOID OVERFLOW AND TO AVOID UNDETECTED UNDERFLOW                   
C INTERFERING WITH THE CONVERGENCE CRITERION.                           
C THE FACTOR IS A POWER OF THE BASE                                     
      SC = LO/MIN                                                       
      IF (SC.GT.1.0) GO TO 80                                           
      IF (MAX.LT.10.) GO TO 110                                         
      IF (SC.EQ.0.) SC = SMALNO                                         
      GO TO 90                                                          
   80 IF (INFIN/SC.LT.MAX) GO TO 110                                    
   90 L = ALOG(SC)/ALOG(BASE) + .5                                      
      FACTOR = (BASE*1.0E0)**L                                          
      IF (FACTOR.EQ.1.E0) GO TO 110                                     
      DO 100 I=1,NN                                                     
        P(I) = FACTOR*P(I)                                              
  100 CONTINUE                                                          
C COMPUTE LOWER BOUND ON MODULI OF ZEROS.                               
  110 DO 120 I=1,NN                                                     
        PT(I) = ABS((P(I)))                                             
  120 CONTINUE                                                          
      PT(NN) = -PT(NN)                                                  
C COMPUTE UPPER ESTIMATE OF BOUND                                       
      X = EXP((ALOG(-PT(NN))-ALOG(PT(1)))/FLOAT(N))                     
      IF (PT(N).EQ.0.) GO TO 130                                        
C IF NEWTON STEP AT THE ORIGIN IS BETTER, USE IT.                       
      XM = -PT(NN)/PT(N)                                                
      IF (XM.LT.X) X = XM                                               
C CHOP THE INTERVAL (0,X) UNTIL FF .LE. 0                               
  130 XM = X*.1                                                         
      FF = PT(1)                                                        
      DO 140 I=2,NN                                                     
        FF = FF*XM + PT(I)                                              
  140 CONTINUE                                                          
      IF (FF.LE.0.) GO TO 150                                           
      X = XM                                                            
      GO TO 130                                                         
  150 DX = X                                                            
C DO NEWTON ITERATION UNTIL X CONVERGES TO TWO                          
C DECIMAL PLACES                                                        
  160 IF (ABS(DX/X).LE..005) GO TO 180                                  
      FF = PT(1)                                                        
      DF = FF                                                           
      DO 170 I=2,N                                                      
        FF = FF*X + PT(I)                                               
        DF = DF*X + FF                                                  
  170 CONTINUE                                                          
      FF = FF*X + PT(NN)                                                
      DX = FF/DF                                                        
      X = X - DX                                                        
      GO TO 160                                                         
  180 BND = X                                                           
C COMPUTE THE DERIVATIVE AS THE INTIAL K POLYNOMIAL                     
C AND DO 5 STEPS WITH NO SHIFT                                          
      NM1 = N - 1                                                       
      DO 190 I=2,N                                                      
        K(I) = FLOAT(NN-I)*P(I)/FLOAT(N)                                
  190 CONTINUE                                                          
      K(1) = P(1)                                                       
      AA = P(NN)                                                        
      BB = P(N)                                                         
      ZEROK = K(N).EQ.0.E0                                              
      DO 230 JJ=1,5                                                     
        CC = K(N)                                                       
        IF (ZEROK) GO TO 210                                            
C USE SCALED FORM OF RECURRENCE IF VALUE OF K AT 0 IS                   
C NONZERO                                                               
        T = -AA/CC                                                      
        DO 200 I=1,NM1                                                  
          J = NN - I                                                    
          K(J) = T*K(J-1) + P(J)                                        
  200   CONTINUE                                                        
        K(1) = P(1)                                                     
        ZEROK = ABS(K(N)).LE.ABS(BB)*ETA*10.                            
        GO TO 230                                                       
C USE UNSCALED FORM OF RECURRENCE                                       
  210   DO 220 I=1,NM1                                                  
          J = NN - I                                                    
          K(J) = K(J-1)                                                 
  220   CONTINUE                                                        
        K(1) = 0.E0                                                     
        ZEROK = K(N).EQ.0.E0                                            
  230 CONTINUE                                                          
C SAVE K FOR RESTARTS WITH NEW SHIFTS                                   
      DO 240 I=1,N                                                      
        TEMP(I) = K(I)                                                  
  240 CONTINUE                                                          
C LOOP TO SELECT THE QUADRATIC  CORRESPONDING TO EACH                   
C NEW SHIFT                                                             
      DO 280 CNT=1,20                                                   
C QUADRATIC CORRESPONDS TO A DOUBLE SHIFT TO A                          
C NON-REAL POINT AND ITS COMPLEX CONJUGATE. THE POINT                   
C HAS MODULUS BND AND AMPLITUDE ROTATED BY 94 DEGREES                   
C FROM THE PREVIOUS SHIFT                                               
        XXX = COSR*XX - SINR*YY                                         
        YY = SINR*XX + COSR*YY                                          
        XX = XXX                                                        
        SR = BND*XX                                                     
        SI = BND*YY                                                     
        U = -2.0E0*SR                                                   
        V = BND                                                         
C SECOND STAGE CALCULATION, FIXED QUADRATIC                             
        CALL R3RPLY(20*CNT, NZ, P, QP, K, QK, SVK)                      
        IF (NZ.EQ.0) GO TO 260                                          
C THE SECOND STAGE JUMPS DIRECTLY TO ONE OF THE THIRD                   
C STAGE ITERATIONS AND RETURNS HERE IF SUCCESSFUL.                      
C DEFLATE THE POLYNOMIAL, STORE THE ZERO OR ZEROS AND                   
C RETURN TO THE MAIN ALGORITHM.                                         
        J = DEGREE - N + 1                                              
        ZEROR(J) = SZR                                                  
        ZEROI(J) = SZI                                                  
        NN = NN - NZ                                                    
        N = NN - 1                                                      
        DO 250 I=1,NN                                                   
          P(I) = QP(I)                                                  
  250   CONTINUE                                                        
        IF (NZ.EQ.1) GO TO 40                                           
        ZEROR(J+1) = LZR                                                
        ZEROI(J+1) = LZI                                                
        GO TO 40                                                        
C IF THE ITERATION IS UNSUCCESSFUL ANOTHER QUADRATIC                    
C IS CHOSEN AFTER RESTORING K                                           
  260   DO 270 I=1,N                                                    
          K(I) = TEMP(I)                                                
  270   CONTINUE                                                        
  280 CONTINUE                                                          
C RETURN WITH FAILURE IF NO CONVERGENCE WITH 20                         
C SHIFTS                                                                
      FAIL = .TRUE.                                                     
      DEGREE = DEGREE - N                                               
      RETURN                                                            
      END                                                               
      SUBROUTINE R3RPLY(L2, NZ, P, QP, K, QK, SVK)                      
C COMPUTES UP TO  L2  FIXED SHIFT K-POLYNOMIALS,                        
C TESTING FOR CONVERGENCE IN THE LINEAR OR QUADRATIC                    
C CASE. INITIATES ONE OF THE VARIABLE SHIFT                             
C ITERATIONS AND RETURNS WITH THE NUMBER OF ZEROS                       
C FOUND.                                                                
C L2 - LIMIT OF FIXED SHIFT STEPS                                       
C NZ - NUMBER OF ZEROS FOUND                                            
      COMMON /P66PLY/ SR, SI, U,                                        
     1 V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                      
     2 H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                      
C                                                                       
      INTEGER N, NN                                                     
      INTEGER L2, NZ, TYPE, I, J, IFLAG                                 
C                                                                       
      REAL ETA, ARE, MRE                                                
      REAL BETAS, BETAV, OSS, OVV, SS, VV, TS, TV,                      
     1 OTS, OTV, TVV, TSS                                               
      REAL P(1), QP(1), K(1),                                           
     1 QK(1), SVK(1), SR, SI, U, V, A, B, C, D,                         
     2 A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                        
     3 LZR, LZI                                                         
      REAL SVU, SVV, UI, VI, S                                          
      LOGICAL VPASS, SPASS, VTRY, STRY                                  
      NZ = 0                                                            
      BETAV = .25                                                       
      BETAS = .25                                                       
      OSS = SR                                                          
      OVV = V                                                           
C EVALUATE POLYNOMIAL BY SYNTHETIC DIVISION                             
      CALL R8RPLY(NN, U, V, P, QP, A, B)                                
      CALL R2RPLY(TYPE, K, QK)                                          
      DO 80 J=1,L2                                                      
C CALCULATE NEXT K POLYNOMIAL AND ESTIMATE V                            
        CALL R5RPLY(TYPE, P, QP, K, QK)                                 
        CALL R2RPLY(TYPE, K, QK)                                        
        CALL R4RPLY(TYPE, UI, VI, P, K)                                 
        VV = VI                                                         
C ESTIMATE S                                                            
        SS = 0.                                                         
        IF (K(N).NE.0.E0) SS = -P(NN)/K(N)                              
        TV = 1.                                                         
        TS = 1.                                                         
        IF (J.EQ.1 .OR. TYPE.EQ.3) GO TO 70                             
C COMPUTE RELATIVE MEASURES OF CONVERGENCE OF S AND V                   
C SEQUENCES                                                             
        IF (VV.NE.0.) TV = ABS((VV-OVV)/VV)                             
        IF (SS.NE.0.) TS = ABS((SS-OSS)/SS)                             
C IF DECREASING, MULTIPLY TWO MOST RECENT                               
C CONVERGENCE MEASURES                                                  
        TVV = 1.                                                        
        IF (TV.LT.OTV) TVV = TV*OTV                                     
        TSS = 1.                                                        
        IF (TS.LT.OTS) TSS = TS*OTS                                     
C COMPARE WITH CONVERGENCE CRITERIA                                     
        VPASS = TVV.LT.BETAV                                            
        SPASS = TSS.LT.BETAS                                            
        IF (.NOT.(SPASS .OR. VPASS)) GO TO 70                           
C AT LEAST ONE SEQUENCE HAS PASSED THE CONVERGENCE                      
C TEST. STORE VARIABLES BEFORE ITERATING                                
        SVU = U                                                         
        SVV = V                                                         
        DO 10 I=1,N                                                     
          SVK(I) = K(I)                                                 
   10   CONTINUE                                                        
        S = SS                                                          
C CHOOSE ITERATION ACCORDING TO THE FASTEST                             
C CONVERGING SEQUENCE                                                   
        VTRY = .FALSE.                                                  
        STRY = .FALSE.                                                  
        IF (SPASS .AND. ((.NOT.VPASS) .OR.                              
     1   TSS.LT.TVV)) GO TO 40                                          
   20   CALL R7RPLY(UI, VI, NZ, P, QP, K, QK, SVK)                      
        IF (NZ.GT.0) RETURN                                             
C QUADRATIC ITERATION HAS FAILED. FLAG THAT IT HAS                      
C BEEN TRIED AND DECREASE THE CONVERGENCE CRITERION.                    
        VTRY = .TRUE.                                                   
        BETAV = BETAV*.25                                               
C TRY LINEAR ITERATION IF IT HAS NOT BEEN TRIED AND                     
C THE S SEQUENCE IS CONVERGING                                          
        IF (STRY .OR. (.NOT.SPASS)) GO TO 50                            
        DO 30 I=1,N                                                     
          K(I) = SVK(I)                                                 
   30   CONTINUE                                                        
   40   CALL R9RPLY(S, NZ, IFLAG, P, QP, K, QK)                         
        IF (NZ.GT.0) RETURN                                             
C LINEAR ITERATION HAS FAILED. FLAG THAT IT HAS BEEN                    
C TRIED AND DECREASE THE CONVERGENCE CRITERION                          
        STRY = .TRUE.                                                   
        BETAS = BETAS*.25                                               
        IF (IFLAG.EQ.0) GO TO 50                                        
C IF LINEAR ITERATION SIGNALS AN ALMOST DOUBLE REAL                     
C ZERO ATTEMPT QUADRATIC INTERATION                                     
        UI = -(S+S)                                                     
        VI = S*S                                                        
        GO TO 20                                                        
C RESTORE VARIABLES                                                     
   50   U = SVU                                                         
        V = SVV                                                         
        DO 60 I=1,N                                                     
          K(I) = SVK(I)                                                 
   60   CONTINUE                                                        
C TRY QUADRATIC ITERATION IF IT HAS NOT BEEN TRIED                      
C AND THE V SEQUENCE IS CONVERGING                                      
        IF (VPASS .AND. (.NOT.VTRY)) GO TO 20                           
C RECOMPUTE QP AND SCALAR VALUES TO CONTINUE THE                        
C SECOND STAGE                                                          
        CALL R8RPLY(NN, U, V, P, QP, A, B)                              
        CALL R2RPLY(TYPE, K, QK)                                        
   70   OVV = VV                                                        
        OSS = SS                                                        
        OTV = TV                                                        
        OTS = TS                                                        
   80 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE R7RPLY(UU, VV, NZ, P, QP, K, QK, SVK)                  
C VARIABLE-SHIFT K-POLYNOMIAL ITERATION FOR A                           
C QUADRATIC FACTOR CONVERGES ONLY IF THE ZEROS ARE                      
C EQUIMODULAR OR NEARLY SO.                                             
C UU,VV - COEFFICIENTS OF STARTING QUADRATIC                            
C NZ - NUMBER OF ZERO FOUND                                             
C                                                                       
      COMMON /P66PLY/ SR, SI, U,                                        
     1 V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                      
     2 H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                      
C                                                                       
      INTEGER N, NN                                                     
      INTEGER NZ, TYPE, I, J                                            
C                                                                       
      REAL ETA, ARE, MRE                                                
      REAL MP, OMP, EE, RELSTP, T, ZM                                   
C                                                                       
      REAL P(1), QP(1), K(1),                                           
     1   QK(1), SVK(1), SR, SI, U, V, A, B, C, D,                       
     2   A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                      
     3   LZR, LZI                                                       
C                                                                       
      REAL UI, VI, UU, VV, ABS                                          
      LOGICAL TRIED                                                     
C                                                                       
      NZ = 0                                                            
      TRIED = .FALSE.                                                   
      U = UU                                                            
      V = VV                                                            
      J = 0                                                             
C MAIN LOOP                                                             
   10 CALL R6RPLY(1.E0, U, V, SZR, SZI, LZR, LZI)                       
C RETURN IF ROOTS OF THE QUADRATIC ARE REAL AND NOT                     
C CLOSE TO MULTIPLE OR NEARLY EQUAL AND  OF OPPOSITE                    
C SIGN                                                                  
      IF (ABS(ABS(SZR)-ABS(LZR)).GT..01E0*                              
     1   ABS(LZR)) RETURN                                               
C EVALUATE POLYNOMIAL BY QUADRATIC SYNTHETIC DIVISION                   
      CALL R8RPLY(NN, U, V, P, QP, A, B)                                
      MP = ABS(A-SZR*B) + ABS(SZI*B)                                    
C COMPUTE A RIGOROUS  BOUND ON THE ROUNDING ERROR IN                    
C EVALUTING P                                                           
      ZM = SQRT(ABS((V)))                                               
      EE = 2.*ABS((QP(1)))                                              
      T = -SZR*B                                                        
      DO 20 I=2,N                                                       
        EE = EE*ZM + ABS((QP(I)))                                       
   20 CONTINUE                                                          
      EE = EE*ZM + ABS((A)+T)                                           
      EE = (5.*MRE+4.*ARE)*EE - (5.*MRE+2.*ARE)*                        
     2   (ABS((A)+T)+ABS((B))*ZM) +                                     
     3   2.*ARE*ABS(T)                                                  
C ITERATION HAS CONVERGED SUFFICIENTLY IF THE                           
C POLYNOMIAL VALUE IS LESS THAN 20 TIMES THIS BOUND                     
      IF (MP.GT.20.*EE) GO TO 30                                        
      NZ = 2                                                            
      RETURN                                                            
   30 J = J + 1                                                         
C STOP ITERATION AFTER 20 STEPS                                         
      IF (J.GT.20) RETURN                                               
      IF (J.LT.2) GO TO 50                                              
      IF (RELSTP.GT..01 .OR. MP.LT.OMP .OR. TRIED)                      
     1   GO TO 50                                                       
C A CLUSTER APPEARS TO BE STALLING THE CONVERGENCE.                     
C FIVE FIXED SHIFT STEPS ARE TAKEN WITH A U,V CLOSE                     
C TO THE CLUSTER                                                        
      IF (RELSTP.LT.ETA) RELSTP = ETA                                   
      RELSTP = SQRT(RELSTP)                                             
      U = U - U*RELSTP                                                  
      V = V + V*RELSTP                                                  
      CALL R8RPLY(NN, U, V, P, QP, A, B)                                
      DO 40 I=1,5                                                       
        CALL R2RPLY(TYPE, K, QK)                                        
        CALL R5RPLY(TYPE, P, QP, K, QK)                                 
   40 CONTINUE                                                          
      TRIED = .TRUE.                                                    
      J = 0                                                             
   50 OMP = MP                                                          
C CALCULATE NEXT K POLYNOMIAL AND NEW U AND V                           
      CALL R2RPLY(TYPE, K, QK)                                          
      CALL R5RPLY(TYPE, P, QP, K, QK)                                   
      CALL R2RPLY(TYPE, K, QK)                                          
      CALL R4RPLY(TYPE, UI, VI, P, K)                                   
C IF VI IS ZERO THE ITERATION IS NOT CONVERGING                         
      IF (VI.EQ.0.E0) RETURN                                            
      RELSTP = ABS((VI-V)/VI)                                           
      U = UI                                                            
      V = VI                                                            
      GO TO 10                                                          
      END                                                               
      SUBROUTINE R2RPLY(TYPE, K, QK)                                    
C THIS ROUTINE CALCULATES SCALAR QUANTITIES USED TO                     
C COMPUTE THE NEXT K POLYNOMIAL AND NEW ESTIMATES OF                    
C THE QUADRATIC COEFFICIENTS.                                           
C TYPE - INTEGER VARIABLE SET HERE INDICATING HOW THE                   
C CALCULATIONS ARE NORMALIZED TO AVOID OVERFLOW                         
C                                                                       
      COMMON /P66PLY/ SR, SI, U,                                        
     1 V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                      
     2 H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                      
C                                                                       
      INTEGER N, NN                                                     
      INTEGER TYPE                                                      
      REAL ETA, ARE, MRE                                                
C                                                                       
      REAL K(1), QK(1), SR, SI, U, V, A, B, C, D,                       
     1 A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                        
     2 LZR, LZI                                                         
      REAL ABS                                                          
C SYNTHETIC DIVISION OF K BY THE QUADRATIC 1,U,V                        
      CALL R8RPLY(N, U, V, K, QK, C, D)                                 
      IF (ABS(C).GT.ABS(K(N))*100.*ETA) GO TO 10                        
      IF (ABS(D).GT.ABS(K(N-1))*100.*ETA) GO TO 10                      
      TYPE = 3                                                          
C TYPE=3 INDICATES THE QUADRATIC IS ALMOST A FACTOR                     
C OF K                                                                  
      RETURN                                                            
   10 IF (ABS(D).LT.ABS(C)) GO TO 20                                    
      TYPE = 2                                                          
C TYPE=2 INDICATES THAT ALL FORMULAS ARE DIVIDED BY D                   
      E = A/D                                                           
      F = C/D                                                           
      G = U*B                                                           
      H = V*B                                                           
      A3 = (A+G)*E + H*(B/D)                                            
      A1 = B*F - A                                                      
      A7 = (F+U)*A + H                                                  
      RETURN                                                            
   20 TYPE = 1                                                          
C TYPE=1 INDICATES THAT ALL FORMULAS ARE DIVIDED BY C                   
      E = A/C                                                           
      F = D/C                                                           
      G = U*E                                                           
      H = V*B                                                           
      A3 = A*E + (H/C+G)*B                                              
      A1 = B - A*(D/C)                                                  
      A7 = A + G*D + H*F                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE R4RPLY(TYPE, UU, VV, P, K)                             
C COMPUTE NEW ESTIMATES OF THE QUADRATIC COEFFICIENTS                   
C USING THE SCALARS COMPUTED IN R2RPLY.                                 
      COMMON /P66PLY/ SR, SI, U,                                        
     1 V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                      
     2 H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                      
C                                                                       
      INTEGER N, NN                                                     
      INTEGER TYPE                                                      
      REAL ETA, ARE, MRE                                                
C                                                                       
      REAL P(1), K(1),                                                  
     1  SR, SI, U, V, A, B, C, D,                                       
     2 A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                        
     3 LZR, LZI                                                         
      REAL A4, A5, B1, B2, C1, C2, C3,                                  
     1 C4, TEMP, UU, VV                                                 
C USE FORMULAS APPROPRIATE TO SETTING OF TYPE.                          
      IF (TYPE.EQ.3) GO TO 30                                           
      IF (TYPE.EQ.2) GO TO 10                                           
      A4 = A + U*B + H*F                                                
      A5 = C + (U+V*F)*D                                                
      GO TO 20                                                          
   10 A4 = (A+G)*F + H                                                  
      A5 = (F+U)*C + V*D                                                
C EVALUATE NEW QUADRATIC COEFFICIENTS.                                  
   20 B1 = -K(N)/P(NN)                                                  
      B2 = -(K(N-1)+B1*P(N))/P(NN)                                      
      C1 = V*B2*A1                                                      
      C2 = B1*A7                                                        
      C3 = B1*B1*A3                                                     
      C4 = C1 - C2 - C3                                                 
      TEMP = A5 + B1*A4 - C4                                            
      IF (TEMP.EQ.0.E0) GO TO 30                                        
      UU = U - (U*(C3+C2)+V*(B1*A1+B2*A7))/TEMP                         
      VV = V*(1.+C4/TEMP)                                               
      RETURN                                                            
C IF TYPE=3 THE QUADRATIC IS ZEROED                                     
   30 UU = 0.E0                                                         
      VV = 0.E0                                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE R5RPLY(TYPE, P, QP, K, QK)                             
C COMPUTES THE NEXT K POLYNOMIALS USING SCALARS                         
C COMPUTED IN R2RPLY                                                    
C                                                                       
      COMMON /P66PLY/ SR, SI, U,                                        
     1 V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                      
     2 H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                      
C                                                                       
      INTEGER N, NN                                                     
      INTEGER TYPE                                                      
C                                                                       
      REAL ETA, ARE, MRE                                                
      REAL P(1), QP(2), K(2),                                           
     1 QK(1), SR, SI, U, V, A, B, C, D,                                 
     2 A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                        
     3 LZR, LZI                                                         
      REAL TEMP, ABS                                                    
      IF (TYPE.EQ.3) GO TO 40                                           
      TEMP = A                                                          
      IF (TYPE.EQ.1) TEMP = B                                           
      IF (ABS(A1).GT.ABS(TEMP)*ETA*10.) GO TO 20                        
C IF A1 IS NEARLY ZERO THEN USE A SPECIAL FORM OF THE                   
C RECURRENCE                                                            
      K(1) = 0.E0                                                       
      K(2) = -A7*QP(1)                                                  
      DO 10 I=3,N                                                       
        K(I) = A3*QK(I-2) - A7*QP(I-1)                                  
   10 CONTINUE                                                          
      RETURN                                                            
C USE SCALED FORM OF THE RECURRENCE                                     
   20 A7 = A7/A1                                                        
      A3 = A3/A1                                                        
      K(1) = QP(1)                                                      
      K(2) = QP(2) - A7*QP(1)                                           
      DO 30 I=3,N                                                       
        K(I) = A3*QK(I-2) - A7*QP(I-1) + QP(I)                          
   30 CONTINUE                                                          
      RETURN                                                            
C USE UNSCALED FORM OF THE RECURRENCE IF TYPE IS 3                      
   40 K(1) = 0.E0                                                       
      K(2) = 0.E0                                                       
      DO 50 I=3,N                                                       
        K(I) = QK(I-2)                                                  
   50 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE R6RPLY(A, B1, C, SR, SI, LR, LI)                       
C CALCULATE THE ZEROS OF THE QUADRATIC A*Z**2+B1*Z+C.                   
C THE QUADRATIC FORMULA, MODIFIED TO AVOID                              
C OVERFLOW, IS USED TO FIND THE LARGER ZERO IF THE                      
C ZEROS ARE REAL AND BOTH ZEROS ARE COMPLEX.                            
C THE SMALLER REAL ZERO IS FOUND DIRECTLY FROM THE                      
C PRODUCT OF THE ZEROS C/A.                                             
      REAL A, B1, C, SR, SI, LR, LI, B,                                 
     1 D, E, ABS, SQRT                                                  
      IF (A.NE.0.E0) GO TO 20                                           
      SR = 0.E0                                                         
      IF (B1.NE.0.E0) SR = -C/B1                                        
      LR = 0.E0                                                         
   10 SI = 0.E0                                                         
      LI = 0.E0                                                         
      RETURN                                                            
   20 IF (C.NE.0.E0) GO TO 30                                           
      SR = 0.E0                                                         
      LR = -B1/A                                                        
      GO TO 10                                                          
C COMPUTE DISCRIMINANT AVOIDING OVERFLOW                                
   30 B = B1/2.E0                                                       
      IF (ABS(B).LT.ABS(C)) GO TO 40                                    
      E = 1.E0 - (A/B)*(C/B)                                            
      D = SQRT(ABS(E))*ABS(B)                                           
      GO TO 50                                                          
   40 E = A                                                             
      IF (C.LT.0.E0) E = -A                                             
      E = B*(B/ABS(C)) - E                                              
      D = SQRT(ABS(E))*SQRT(ABS(C))                                     
   50 IF (E.LT.0.E0) GO TO 60                                           
C REAL ZEROS                                                            
      IF (B.GE.0.E0) D = -D                                             
      LR = (-B+D)/A                                                     
      SR = 0.E0                                                         
      IF (LR.NE.0.E0) SR = (C/LR)/A                                     
      GO TO 10                                                          
C COMPLEX CONJUGATE ZEROS                                               
   60 SR = -B/A                                                         
      LR = SR                                                           
      SI = ABS(D/A)                                                     
      LI = -SI                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE R8RPLY(NN, U, V, P, Q, A, B)                           
C DIVIDES P BY THE QUADRATIC  1,U,V  PLACING THE                        
C QUOTIENT IN Q AND THE REMAINDER IN A,B                                
      REAL P(NN), Q(NN), U, V, A, B, C                                  
      INTEGER I                                                         
      B = P(1)                                                          
      Q(1) = B                                                          
      A = P(2) - U*B                                                    
      Q(2) = A                                                          
      DO 10 I=3,NN                                                      
        C = P(I) - U*A - V*B                                            
        Q(I) = C                                                        
        B = A                                                           
        A = C                                                           
   10 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE R9RPLY(SSS, NZ, IFLAG, P, QP, K, QK)                   
C VARIABLE-SHIFT H POLYNOMIAL ITERATION FOR A REAL                      
C ZERO.                                                                 
C SSS   - STARTING ITERATE                                              
C NZ    - NUMBER OF ZERO FOUND                                          
C IFLAG - FLAG TO INDICATE A PAIR OF ZEROS NEAR REAL                    
C         AXIS.                                                         
C                                                                       
      COMMON /P66PLY/ SR, SI, U,                                        
     1 V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                      
     2 H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                      
C                                                                       
      INTEGER N, NN                                                     
      INTEGER NZ, IFLAG, I, J, NM1                                      
C                                                                       
      REAL ETA, ARE, MRE                                                
      REAL MS, MP, OMP, EE                                              
C                                                                       
      REAL P(1), QP(1), K(1),                                           
     1   QK(1), SR, SI, U, V, A, B, C, D,                               
     2   A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                      
     3   LZR, LZI                                                       
      REAL PV, KV, T, S, SSS, ABS                                       
      NM1 = N - 1                                                       
      NZ = 0                                                            
      S = SSS                                                           
      IFLAG = 0                                                         
      J = 0                                                             
C MAIN LOOP                                                             
   10 PV = P(1)                                                         
C EVALUATE P AT S                                                       
      QP(1) = PV                                                        
      DO 20 I=2,NN                                                      
        PV = PV*S + P(I)                                                
        QP(I) = PV                                                      
   20 CONTINUE                                                          
      MP = ABS(PV)                                                      
C COMPUTE A RIGOROUS BOUND ON THE ERROR IN EVALUATING                   
C P                                                                     
      MS = ABS(S)                                                       
      EE = (MRE/(ARE+MRE))*ABS((QP(1)))                                 
      DO 30 I=2,NN                                                      
        EE = EE*MS + ABS((QP(I)))                                       
   30 CONTINUE                                                          
C ITERATION HAS CONVERGED SUFFICIENTLY IF THE                           
C POLYNOMIAL VALUE IS LESS THAN 20 TIMES THIS BOUND                     
      IF (MP.GT.20.*((ARE+MRE)*EE-MRE*MP)) GO TO 40                     
      NZ = 1                                                            
      SZR = S                                                           
      SZI = 0.E0                                                        
      RETURN                                                            
   40 J = J + 1                                                         
C STOP ITERATION AFTER 10 STEPS                                         
      IF (J.GT.10) RETURN                                               
      IF (J.LT.2) GO TO 50                                              
      IF (ABS(T).GT..001*ABS(S-T) .OR. MP.LE.OMP)                       
     1   GO TO 50                                                       
C A CLUSTER OF ZEROS NEAR THE REAL AXIS HAS BEEN                        
C ENCOUNTERED RETURN WITH IFLAG SET TO INITIATE A                       
C QUADRATIC ITERATION                                                   
      IFLAG = 1                                                         
      SSS = S                                                           
      RETURN                                                            
C RETURN IF THE POLYNOMIAL VALUE HAS INCREASED                          
C SIGNIFICANTLY                                                         
   50 OMP = MP                                                          
C COMPUTE T, THE NEXT POLYNOMIAL, AND THE NEW ITERATE                   
      KV = K(1)                                                         
      QK(1) = KV                                                        
      DO 60 I=2,N                                                       
        KV = KV*S + K(I)                                                
        QK(I) = KV                                                      
   60 CONTINUE                                                          
      IF (ABS(KV).LE.ABS(K(N))*10.*ETA) GO TO 80                        
C USE THE SCALED FORM OF THE RECURRENCE IF THE VALUE                    
C OF K AT S IS NONZERO                                                  
      T = -PV/KV                                                        
      K(1) = QP(1)                                                      
      DO 70 I=2,N                                                       
        K(I) = T*QK(I-1) + QP(I)                                        
   70 CONTINUE                                                          
      GO TO 100                                                         
C USE UNSCALED FORM                                                     
   80 K(1) = 0.0E0                                                      
      DO 90 I=2,N                                                       
        K(I) = QK(I-1)                                                  
   90 CONTINUE                                                          
  100 KV = K(1)                                                         
      DO 110 I=2,N                                                      
        KV = KV*S + K(I)                                                
  110 CONTINUE                                                          
      T = 0.E0                                                          
      IF (ABS(KV).GT.ABS(K(N))*10.*ETA) T = -PV/KV                      
      S = S + T                                                         
      GO TO 10                                                          
      END                                                               
      SUBROUTINE DRPOLY(DEGREE,COEFF,ZEROR,ZEROI)                       
C                                                                       
C FINDS THE ZEROS OF A POLYNOMIAL WITH REAL COEFFICIENTS.               
C                                                                       
C  INPUTS -                                                             
C                                                                       
C     COEFF         -  DOUBLE PRECISION VECTOR OF THE COEFF-            
C                      ICIENTS IN ORDER OF DECREASING POWERS.           
C                                                                       
C     DEGREE        -  INTEGER DEGREE OF POLYNOMIAL.                    
C                                                                       
C  OUTPUTS -                                                            
C                                                                       
C     ZEROR, ZEROI  -  DOUBLE PRECISION VECTORS OF REAL                 
C                      AND IMAGINARY PARTS OF THE ZEROS.                
C                                                                       
C THE SUBROUTINE USES SINGLE PRECISION CALCULATIONS                     
C FOR SCALING, BOUNDS AND ERROR CALCULATIONS. ALL                       
C CALCULATIONS FOR THE ITERATIONS ARE DONE IN DOUBLE                    
C PRECISION.                                                            
C                                                                       
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1    - DEGREE IS LESS THAN 1                                       
C    2    - LEADING COEFFICIENT IS ZERO                                 
C    3    - THE DYNAMIC STORAGE STACK IS NOT BIG ENOUGH                 
C    NNN  - ONLY NNN (.LT. DEGREE) ZEROS HAVE BEEN                      
C          FOUND (RECOVERABLE)                                          
C                                                                       
C                                                                       
C PORT NOTE -                                                           
C                                                                       
C THE ORIGINAL PROGRAM HAS BEEN ADAPTED TO PORT BY -                    
C                                                                       
C   (1) PUTTING IN AUTOMATIC ERROR HANDLING.                            
C   (2) SUBSTITUTING DYNAMIC STACK ALLOCATION FOR THE DIMENSIONED       
C       ARRAYS IN NAMED COMMON.                                         
C   (3) CHANGING THE NAMES OF THE INTERNAL ROUTINES TO AVOID USER       
C       NAME CONFLICT.                                                  
C                                                                       
C  THE FOLLOWING NAME EQUIVALENCES (ORIGINAL - NEW) APPLY -             
C                                                                       
C            RPOLY    -   D1RPLY                                        
C            CALCSC   -   D2RPLY                                        
C            FXSHFR   -   D3RPLY                                        
C            NEWEST   -   D4RPLY                                        
C            NEXTK    -   D5RPLY                                        
C            QUAD     -   D6RPLY                                        
C            QUADIT   -   D7RPLY                                        
C            QUADSD   -   D8RPLY                                        
C            REALIT   -   D9RPLY                                        
C                                                                       
C                                                                       
C DYNAMIC STORAGE SPACE USED -                                          
C                                                                       
C    THE DRPOLY PROGRAMS USE 6*(DEGREE+1)                               
C    DOUBLE-PRECISION LOCATIONS PLUS 1*(DEGREE+1) REAL                  
C    LOCATIONS IN THE DYNAMIC STORAGE STACK.                            
C                                                                       
C COMMON AREA                                                           
      COMMON/CSTAK/DSTAK(500)                                           
C                                                                       
      INTEGER DEGREE                                                    
      REAL RSTAK(1000)                                                  
      DOUBLE PRECISION DSTAK                                            
      DOUBLE PRECISION COEFF(1),ZEROR(1),ZEROI(1)                       
      LOGICAL FAIL                                                      
C                                                                       
      EQUIVALENCE (DSTAK(1),RSTAK(1))                                   
C                                                                       
C THE DEGREE IS LESS THAN 1                                             
C                                                                       
C/6S                                                                    
C     IF (DEGREE .LT. 1) CALL SETERR(                                   
C    1   34HDRPOLY - THE DEGREE IS LESS THAN 1,34,1,2)                  
C/7S                                                                    
      IF (DEGREE .LT. 1) CALL SETERR(                                   
     1   'DRPOLY - THE DEGREE IS LESS THAN 1',34,1,2)                   
C/                                                                      
C                                                                       
C ALGORITHM FAILS IF THE LEADING COEFFICIENT IS ZERO.                   
C                                                                       
C/6S                                                                    
C     IF (COEFF(1) .EQ. 0.0D0) CALL SETERR(                             
C    1   36HDRPOLY - LEADING COEFFICIENT IS ZERO,36,2,2)                
C/7S                                                                    
      IF (COEFF(1) .EQ. 0.0D0) CALL SETERR(                             
     1   'DRPOLY - LEADING COEFFICIENT IS ZERO',36,2,2)                 
C/                                                                      
C                                                                       
C SET UP THE STORAGE IN THE DYNAMIC STORAGE STACK -                     
C IF THERE IS ROOM                                                      
C                                                                       
      NN = DEGREE + 1                                                   
C                                                                       
      NLEFT = ISTKQU(4) - 6*NN                                          
C/6S                                                                    
C     IF (NLEFT .LE. 0) CALL SETERR(                                    
C    1   47HDRPOLY - THE DYNAMIC STORAGE LEFT IS NOT ENOUGH,47,3,2)     
C/7S                                                                    
      IF (NLEFT .LE. 0) CALL SETERR(                                    
     1   'DRPOLY - THE DYNAMIC STORAGE LEFT IS NOT ENOUGH',47,3,2)      
C/                                                                      
C                                                                       
      NNN = 6*NN                                                        
      IP    = ISTKGT(NNN,4)                                             
      IQP   = IP   + NN                                                 
      IK    = IQP  + NN                                                 
      IQK   = IK   + NN                                                 
      ISVK  = IQK  + NN                                                 
      ITEMP = ISVK + NN                                                 
C                                                                       
      NLEFT = ISTKQU(3) - 1*NN                                          
C/6S                                                                    
C     IF (NLEFT .LE. 0) CALL SETERR(                                    
C    1   47HDRPOLY - THE DYNAMIC STORAGE LEFT IS NOT ENOUGH,47,3,2)     
C/7S                                                                    
      IF (NLEFT .LE. 0) CALL SETERR(                                    
     1   'DRPOLY - THE DYNAMIC STORAGE LEFT IS NOT ENOUGH',47,3,2)      
C/                                                                      
C                                                                       
      IPT   = ISTKGT(NN,3)                                              
C                                                                       
C                                                                       
C  CALL THE JENKINS-TRAUB ROUTINE (OLD RPOLY IS NOW D1RPLY) WITH        
C  THE STACK LOCATIONS INCLUDED IN THE CALL.                            
C                                                                       
      CALL D1RPLY(COEFF,DEGREE,ZEROR,ZEROI,FAIL,                        
     1  DSTAK(IP),DSTAK(IQP),DSTAK(IK),DSTAK(IQK),DSTAK(ISVK),          
     2  DSTAK(ITEMP),RSTAK(IPT))                                        
C                                                                       
C  SEE IF THE THING FAILED                                              
C                                                                       
      IF (.NOT. FAIL) GO TO 10                                          
C                                                                       
C  OTHERWISE FIGURE HOW MANY ROOTS WERE FOUND                           
C                                                                       
      KP10 = DEGREE+10                                                  
C                                                                       
C/6S                                                                    
C     CALL SETERR(                                                      
C    1   37HDRPOLY - ONLY K ZEROS HAVE BEEN FOUND,37,KP10,1)            
C/7S                                                                    
      CALL SETERR(                                                      
     1   'DRPOLY - ONLY K ZEROS HAVE BEEN FOUND',37,KP10,1)             
C/                                                                      
C                                                                       
  10  CALL ISTKRL(2)                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE D1RPLY(OP, DEGREE, ZEROR, ZEROI,                       
     1              FAIL, P, QP, K, QK, SVK, TEMP, PT)                  
C FINDS THE ZEROS OF A REAL POLYNOMIAL                                  
C OP  - DOUBLE PRECISION VECTOR OF COEFFICIENTS IN                      
C       ORDER OF DECREASING POWERS.                                     
C DEGREE   - INTEGER DEGREE OF POLYNOMIAL.                              
C ZEROR, ZEROI - OUTPUT DOUBLE PRECISION VECTORS OF                     
C                ZEROS.                                                 
C FAIL  - TRUE IF D1RPLY HAS FOUND FEWER THAN                           
C         DEGREE NUMBER OF ZEROS.                                       
C         IN THIS CASE, DEGREE IS RESET TO                              
C         THE NUMBER OF ZEROS FOUND.                                    
C                                                                       
C THE SUBROUTINE USES SINGLE PRECISION CALCULATIONS                     
C FOR SCALING, BOUNDS AND ERROR CALCULATIONS. ALL                       
C CALCULATIONS FOR THE ITERATIONS ARE DONE IN DOUBLE                    
C PRECISION.                                                            
C                                                                       
      COMMON /P77PLY/ SR, SI, U,                                        
     1 V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                      
     2 H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                      
C                                                                       
      INTEGER N, NN                                                     
      INTEGER DEGREE, CNT, NZ, I, J, JJ, NM1                            
      REAL ETA, ARE, MRE                                                
      REAL PT(1), LO, MAX, MIN, XX, YY, COSR,                           
     1 SINR, XXX, X, SC, BND, XM, FF, DF, DX, INFIN,                    
     2 SMALNO, BASE                                                     
C                                                                       
      DOUBLE PRECISION P(3), QP(1), K(1),                               
     1 QK(1), SVK(1), SR, SI, U, V, A, B, C, D,                         
     2 A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                        
     3 LZR, LZI                                                         
C                                                                       
      DOUBLE PRECISION OP(1), TEMP(1),                                  
     1 ZEROR(1), ZEROI(1), T, AA, BB, CC, FACTOR, D1MACH                
      LOGICAL FAIL, ZEROK                                               
C THE FOLLOWING STATEMENTS SET MACHINE CONSTANTS USED                   
C IN VARIOUS PARTS OF THE PROGRAM. THE MEANING OF THE                   
C FOUR CONSTANTS ARE...                                                 
C ETA     THE MAXIMUM RELATIVE REPRESENTATION ERROR                     
C         WHICH CAN BE DESCRIBED AS THE SMALLEST                        
C         POSITIVE FLOATING POINT NUMBER SUCH THAT                      
C         1.D0+ETA IS GREATER THAN 1.                                   
C INFINY  THE LARGEST FLOATING-POINT NUMBER.                            
C SMALNO  THE SMALLEST POSITIVE FLOATING-POINT NUMBER                   
C         IF THE EXPONENT RANGE DIFFERS IN SINGLE AND                   
C         DOUBLE PRECISION THEN SMALNO AND INFIN                        
C         SHOULD INDICATE THE SMALLER RANGE.                            
C                                                                       
C PORT NOTE -                                                           
C     FOR THE ABOVE REASON, THE MACHINE-CONSTANT                        
C     ROUTINE R1MACH HAS BEEN USED FOR THESE TWO VALUES.                
C                                                                       
C BASE    THE BASE OF THE FLOATING-POINT NUMBER                         
C         SYSTEM USED.                                                  
C                                                                       
      BASE = I1MACH(10)                                                 
      ETA = D1MACH(4)                                                   
      INFIN = R1MACH(2)                                                 
      SMALNO = R1MACH(1)                                                
C ARE AND MRE REFER TO THE UNIT ERROR IN + AND *                        
C RESPECTIVELY. THEY ARE ASSUMED TO BE THE SAME AS                      
C ETA.                                                                  
      ARE = ETA                                                         
      MRE = ETA                                                         
      LO = SMALNO/ETA                                                   
C INITIALIZATION OF CONSTANTS FOR SHIFT ROTATION                        
      XX = .5*SQRT(2.)                                                  
      YY = -XX                                                          
C                                                                       
      ANG = 94./180.*(4.*ATAN(1.))                                      
      COSR = COS(ANG)                                                   
      SINR = SIN(ANG)                                                   
      FAIL = .FALSE.                                                    
      N = DEGREE                                                        
      NN = N + 1                                                        
C                                                                       
C REMOVE THE ZEROS AT THE ORIGIN IF ANY                                 
   10 IF (OP(NN).NE.0.0D0) GO TO 20                                     
      J = DEGREE - N + 1                                                
      ZEROR(J) = 0.D0                                                   
      ZEROI(J) = 0.D0                                                   
      NN = NN - 1                                                       
      N = N - 1                                                         
      GO TO 10                                                          
C MAKE A COPY OF THE COEFFICIENTS                                       
   20 DO 30 I=1,NN                                                      
        P(I) = OP(I)                                                    
   30 CONTINUE                                                          
C START THE ALGORITHM FOR ONE ZERO                                      
   40 IF (N.GT.2) GO TO 60                                              
      IF (N.LT.1) RETURN                                                
C CALCULATE THE FINAL ZERO OR PAIR OF ZEROS                             
      IF (N.EQ.2) GO TO 50                                              
      ZEROR(DEGREE) = -P(2)/P(1)                                        
      ZEROI(DEGREE) = 0.0D0                                             
      RETURN                                                            
   50 CALL D6RPLY(P(1), P(2), P(3), ZEROR(DEGREE-1),                    
     1 ZEROI(DEGREE-1), ZEROR(DEGREE), ZEROI(DEGREE))                   
      RETURN                                                            
C FIND LARGEST AND SMALLEST MODULI OF COEFFICIENTS.                     
   60 MAX = 0.                                                          
      MIN = INFIN                                                       
      DO 70 I=1,NN                                                      
        X = ABS(SNGL(P(I)))                                             
        IF (X.GT.MAX) MAX = X                                           
        IF (X.NE.0. .AND. X.LT.MIN) MIN = X                             
   70 CONTINUE                                                          
C SCALE IF THERE ARE LARGE OR VERY SMALL COEFFICIENTS                   
C COMPUTES A SCALE FACTOR TO MULTIPLY THE                               
C COEFFICIENTS OF THE POLYNOMIAL. THE SCALING IS DONE                   
C TO AVOID OVERFLOW AND TO AVOID UNDETECTED UNDERFLOW                   
C INTERFERING WITH THE CONVERGENCE CRITERION.                           
C THE FACTOR IS A POWER OF THE BASE                                     
      SC = LO/MIN                                                       
      IF (SC.GT.1.0) GO TO 80                                           
      IF (MAX.LT.10.) GO TO 110                                         
      IF (SC.EQ.0.) SC = SMALNO                                         
      GO TO 90                                                          
   80 IF (INFIN/SC.LT.MAX) GO TO 110                                    
   90 L = ALOG(SC)/ALOG(BASE) + .5                                      
      FACTOR = (BASE*1.0D0)**L                                          
      IF (FACTOR.EQ.1.D0) GO TO 110                                     
      DO 100 I=1,NN                                                     
        P(I) = FACTOR*P(I)                                              
  100 CONTINUE                                                          
C COMPUTE LOWER BOUND ON MODULI OF ZEROS.                               
  110 DO 120 I=1,NN                                                     
        PT(I) = ABS(SNGL(P(I)))                                         
  120 CONTINUE                                                          
      PT(NN) = -PT(NN)                                                  
C COMPUTE UPPER ESTIMATE OF BOUND                                       
      X = EXP((ALOG(-PT(NN))-ALOG(PT(1)))/FLOAT(N))                     
      IF (PT(N).EQ.0.) GO TO 130                                        
C IF NEWTON STEP AT THE ORIGIN IS BETTER, USE IT.                       
      XM = -PT(NN)/PT(N)                                                
      IF (XM.LT.X) X = XM                                               
C CHOP THE INTERVAL (0,X) UNTIL FF .LE. 0                               
  130 XM = X*.1                                                         
      FF = PT(1)                                                        
      DO 140 I=2,NN                                                     
        FF = FF*XM + PT(I)                                              
  140 CONTINUE                                                          
      IF (FF.LE.0.) GO TO 150                                           
      X = XM                                                            
      GO TO 130                                                         
  150 DX = X                                                            
C DO NEWTON ITERATION UNTIL X CONVERGES TO TWO                          
C DECIMAL PLACES                                                        
  160 IF (ABS(DX/X).LE..005) GO TO 180                                  
      FF = PT(1)                                                        
      DF = FF                                                           
      DO 170 I=2,N                                                      
        FF = FF*X + PT(I)                                               
        DF = DF*X + FF                                                  
  170 CONTINUE                                                          
      FF = FF*X + PT(NN)                                                
      DX = FF/DF                                                        
      X = X - DX                                                        
      GO TO 160                                                         
  180 BND = X                                                           
C COMPUTE THE DERIVATIVE AS THE INTIAL K POLYNOMIAL                     
C AND DO 5 STEPS WITH NO SHIFT                                          
      NM1 = N - 1                                                       
      DO 190 I=2,N                                                      
        K(I) = FLOAT(NN-I)*P(I)/FLOAT(N)                                
  190 CONTINUE                                                          
      K(1) = P(1)                                                       
      AA = P(NN)                                                        
      BB = P(N)                                                         
      ZEROK = K(N).EQ.0.D0                                              
      DO 230 JJ=1,5                                                     
        CC = K(N)                                                       
        IF (ZEROK) GO TO 210                                            
C USE SCALED FORM OF RECURRENCE IF VALUE OF K AT 0 IS                   
C NONZERO                                                               
        T = -AA/CC                                                      
        DO 200 I=1,NM1                                                  
          J = NN - I                                                    
          K(J) = T*K(J-1) + P(J)                                        
  200   CONTINUE                                                        
        K(1) = P(1)                                                     
        ZEROK = DABS(K(N)).LE.DABS(BB)*ETA*10.                          
        GO TO 230                                                       
C USE UNSCALED FORM OF RECURRENCE                                       
  210   DO 220 I=1,NM1                                                  
          J = NN - I                                                    
          K(J) = K(J-1)                                                 
  220   CONTINUE                                                        
        K(1) = 0.D0                                                     
        ZEROK = K(N).EQ.0.D0                                            
  230 CONTINUE                                                          
C SAVE K FOR RESTARTS WITH NEW SHIFTS                                   
      DO 240 I=1,N                                                      
        TEMP(I) = K(I)                                                  
  240 CONTINUE                                                          
C LOOP TO SELECT THE QUADRATIC  CORRESPONDING TO EACH                   
C NEW SHIFT                                                             
      DO 280 CNT=1,20                                                   
C QUADRATIC CORRESPONDS TO A DOUBLE SHIFT TO A                          
C NON-REAL POINT AND ITS COMPLEX CONJUGATE. THE POINT                   
C HAS MODULUS BND AND AMPLITUDE ROTATED BY 94 DEGREES                   
C FROM THE PREVIOUS SHIFT                                               
        XXX = COSR*XX - SINR*YY                                         
        YY = SINR*XX + COSR*YY                                          
        XX = XXX                                                        
        SR = BND*XX                                                     
        SI = BND*YY                                                     
        U = -2.0D0*SR                                                   
        V = BND                                                         
C SECOND STAGE CALCULATION, FIXED QUADRATIC                             
        CALL D3RPLY(20*CNT, NZ, P, QP, K, QK, SVK)                      
        IF (NZ.EQ.0) GO TO 260                                          
C THE SECOND STAGE JUMPS DIRECTLY TO ONE OF THE THIRD                   
C STAGE ITERATIONS AND RETURNS HERE IF SUCCESSFUL.                      
C DEFLATE THE POLYNOMIAL, STORE THE ZERO OR ZEROS AND                   
C RETURN TO THE MAIN ALGORITHM.                                         
        J = DEGREE - N + 1                                              
        ZEROR(J) = SZR                                                  
        ZEROI(J) = SZI                                                  
        NN = NN - NZ                                                    
        N = NN - 1                                                      
        DO 250 I=1,NN                                                   
          P(I) = QP(I)                                                  
  250   CONTINUE                                                        
        IF (NZ.EQ.1) GO TO 40                                           
        ZEROR(J+1) = LZR                                                
        ZEROI(J+1) = LZI                                                
        GO TO 40                                                        
C IF THE ITERATION IS UNSUCCESSFUL ANOTHER QUADRATIC                    
C IS CHOSEN AFTER RESTORING K                                           
  260   DO 270 I=1,N                                                    
          K(I) = TEMP(I)                                                
  270   CONTINUE                                                        
  280 CONTINUE                                                          
C RETURN WITH FAILURE IF NO CONVERGENCE WITH 20                         
C SHIFTS                                                                
      FAIL = .TRUE.                                                     
      DEGREE = DEGREE - N                                               
      RETURN                                                            
      END                                                               
      SUBROUTINE D3RPLY(L2, NZ, P, QP, K, QK, SVK)                      
C COMPUTES UP TO  L2  FIXED SHIFT K-POLYNOMIALS,                        
C TESTING FOR CONVERGENCE IN THE LINEAR OR QUADRATIC                    
C CASE. INITIATES ONE OF THE VARIABLE SHIFT                             
C ITERATIONS AND RETURNS WITH THE NUMBER OF ZEROS                       
C FOUND.                                                                
C L2 - LIMIT OF FIXED SHIFT STEPS                                       
C NZ - NUMBER OF ZEROS FOUND                                            
      COMMON /P77PLY/ SR, SI, U,                                        
     1 V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                      
     2 H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                      
C                                                                       
      INTEGER N, NN                                                     
      INTEGER L2, NZ, TYPE, I, J, IFLAG                                 
C                                                                       
      REAL ETA, ARE, MRE                                                
      REAL BETAS, BETAV, OSS, OVV, SS, VV, TS, TV,                      
     1 OTS, OTV, TVV, TSS                                               
      DOUBLE PRECISION P(1), QP(1), K(1),                               
     1 QK(1), SVK(1), SR, SI, U, V, A, B, C, D,                         
     2 A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                        
     3 LZR, LZI                                                         
      DOUBLE PRECISION SVU, SVV, UI, VI, S                              
      LOGICAL VPASS, SPASS, VTRY, STRY                                  
      NZ = 0                                                            
      BETAV = .25                                                       
      BETAS = .25                                                       
      OSS = SR                                                          
      OVV = V                                                           
C EVALUATE POLYNOMIAL BY SYNTHETIC DIVISION                             
      CALL D8RPLY(NN, U, V, P, QP, A, B)                                
      CALL D2RPLY(TYPE, K, QK)                                          
      DO 80 J=1,L2                                                      
C CALCULATE NEXT K POLYNOMIAL AND ESTIMATE V                            
        CALL D5RPLY(TYPE, P, QP, K, QK)                                 
        CALL D2RPLY(TYPE, K, QK)                                        
        CALL D4RPLY(TYPE, UI, VI, P, K)                                 
        VV = VI                                                         
C ESTIMATE S                                                            
        SS = 0.                                                         
        IF (K(N).NE.0.D0) SS = -P(NN)/K(N)                              
        TV = 1.                                                         
        TS = 1.                                                         
        IF (J.EQ.1 .OR. TYPE.EQ.3) GO TO 70                             
C COMPUTE RELATIVE MEASURES OF CONVERGENCE OF S AND V                   
C SEQUENCES                                                             
        IF (VV.NE.0.) TV = ABS((VV-OVV)/VV)                             
        IF (SS.NE.0.) TS = ABS((SS-OSS)/SS)                             
C IF DECREASING, MULTIPLY TWO MOST RECENT                               
C CONVERGENCE MEASURES                                                  
        TVV = 1.                                                        
        IF (TV.LT.OTV) TVV = TV*OTV                                     
        TSS = 1.                                                        
        IF (TS.LT.OTS) TSS = TS*OTS                                     
C COMPARE WITH CONVERGENCE CRITERIA                                     
        VPASS = TVV.LT.BETAV                                            
        SPASS = TSS.LT.BETAS                                            
        IF (.NOT.(SPASS .OR. VPASS)) GO TO 70                           
C AT LEAST ONE SEQUENCE HAS PASSED THE CONVERGENCE                      
C TEST. STORE VARIABLES BEFORE ITERATING                                
        SVU = U                                                         
        SVV = V                                                         
        DO 10 I=1,N                                                     
          SVK(I) = K(I)                                                 
   10   CONTINUE                                                        
        S = SS                                                          
C CHOOSE ITERATION ACCORDING TO THE FASTEST                             
C CONVERGING SEQUENCE                                                   
        VTRY = .FALSE.                                                  
        STRY = .FALSE.                                                  
        IF (SPASS .AND. ((.NOT.VPASS) .OR.                              
     1   TSS.LT.TVV)) GO TO 40                                          
   20   CALL D7RPLY(UI, VI, NZ, P, QP, K, QK, SVK)                      
        IF (NZ.GT.0) RETURN                                             
C QUADRATIC ITERATION HAS FAILED. FLAG THAT IT HAS                      
C BEEN TRIED AND DECREASE THE CONVERGENCE CRITERION.                    
        VTRY = .TRUE.                                                   
        BETAV = BETAV*.25                                               
C TRY LINEAR ITERATION IF IT HAS NOT BEEN TRIED AND                     
C THE S SEQUENCE IS CONVERGING                                          
        IF (STRY .OR. (.NOT.SPASS)) GO TO 50                            
        DO 30 I=1,N                                                     
          K(I) = SVK(I)                                                 
   30   CONTINUE                                                        
   40   CALL D9RPLY(S, NZ, IFLAG, P, QP, K, QK)                         
        IF (NZ.GT.0) RETURN                                             
C LINEAR ITERATION HAS FAILED. FLAG THAT IT HAS BEEN                    
C TRIED AND DECREASE THE CONVERGENCE CRITERION                          
        STRY = .TRUE.                                                   
        BETAS = BETAS*.25                                               
        IF (IFLAG.EQ.0) GO TO 50                                        
C IF LINEAR ITERATION SIGNALS AN ALMOST DOUBLE REAL                     
C ZERO ATTEMPT QUADRATIC INTERATION                                     
        UI = -(S+S)                                                     
        VI = S*S                                                        
        GO TO 20                                                        
C RESTORE VARIABLES                                                     
   50   U = SVU                                                         
        V = SVV                                                         
        DO 60 I=1,N                                                     
          K(I) = SVK(I)                                                 
   60   CONTINUE                                                        
C TRY QUADRATIC ITERATION IF IT HAS NOT BEEN TRIED                      
C AND THE V SEQUENCE IS CONVERGING                                      
        IF (VPASS .AND. (.NOT.VTRY)) GO TO 20                           
C RECOMPUTE QP AND SCALAR VALUES TO CONTINUE THE                        
C SECOND STAGE                                                          
        CALL D8RPLY(NN, U, V, P, QP, A, B)                              
        CALL D2RPLY(TYPE, K, QK)                                        
   70   OVV = VV                                                        
        OSS = SS                                                        
        OTV = TV                                                        
        OTS = TS                                                        
   80 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE D7RPLY(UU, VV, NZ, P, QP, K, QK, SVK)                  
C VARIABLE-SHIFT K-POLYNOMIAL ITERATION FOR A                           
C QUADRATIC FACTOR CONVERGES ONLY IF THE ZEROS ARE                      
C EQUIMODULAR OR NEARLY SO.                                             
C UU,VV - COEFFICIENTS OF STARTING QUADRATIC                            
C NZ - NUMBER OF ZERO FOUND                                             
C                                                                       
      COMMON /P77PLY/ SR, SI, U,                                        
     1 V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                      
     2 H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                      
C                                                                       
      INTEGER N, NN                                                     
      INTEGER NZ, TYPE, I, J                                            
C                                                                       
      REAL ETA, ARE, MRE                                                
      REAL MP, OMP, EE, RELSTP, T, ZM                                   
C                                                                       
      DOUBLE PRECISION P(1), QP(1), K(1),                               
     1   QK(1), SVK(1), SR, SI, U, V, A, B, C, D,                       
     2   A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                      
     3   LZR, LZI                                                       
C                                                                       
      DOUBLE PRECISION UI, VI, UU, VV                                   
      LOGICAL TRIED                                                     
C                                                                       
      NZ = 0                                                            
      TRIED = .FALSE.                                                   
      U = UU                                                            
      V = VV                                                            
      J = 0                                                             
C MAIN LOOP                                                             
   10 CALL D6RPLY(1.D0, U, V, SZR, SZI, LZR, LZI)                       
C RETURN IF ROOTS OF THE QUADRATIC ARE REAL AND NOT                     
C CLOSE TO MULTIPLE OR NEARLY EQUAL AND  OF OPPOSITE                    
C SIGN                                                                  
      IF (DABS(DABS(SZR)-DABS(LZR)).GT..01D0*                           
     1   DABS(LZR)) RETURN                                              
C EVALUATE POLYNOMIAL BY QUADRATIC SYNTHETIC DIVISION                   
      CALL D8RPLY(NN, U, V, P, QP, A, B)                                
      MP = DABS(A-SZR*B) + DABS(SZI*B)                                  
C COMPUTE A RIGOROUS  BOUND ON THE ROUNDING ERROR IN                    
C EVALUTING P                                                           
      ZM = SQRT(ABS(SNGL(V)))                                           
      EE = 2.*ABS(SNGL(QP(1)))                                          
      T = -SZR*B                                                        
      DO 20 I=2,N                                                       
        EE = EE*ZM + ABS(SNGL(QP(I)))                                   
   20 CONTINUE                                                          
      EE = EE*ZM + ABS(SNGL(A)+T)                                       
      EE = (5.*MRE+4.*ARE)*EE - (5.*MRE+2.*ARE)*                        
     2   (ABS(SNGL(A)+T)+ABS(SNGL(B))*ZM) +                             
     3   2.*ARE*ABS(T)                                                  
C ITERATION HAS CONVERGED SUFFICIENTLY IF THE                           
C POLYNOMIAL VALUE IS LESS THAN 20 TIMES THIS BOUND                     
      IF (MP.GT.20.*EE) GO TO 30                                        
      NZ = 2                                                            
      RETURN                                                            
   30 J = J + 1                                                         
C STOP ITERATION AFTER 20 STEPS                                         
      IF (J.GT.20) RETURN                                               
      IF (J.LT.2) GO TO 50                                              
      IF (RELSTP.GT..01 .OR. MP.LT.OMP .OR. TRIED)                      
     1   GO TO 50                                                       
C A CLUSTER APPEARS TO BE STALLING THE CONVERGENCE.                     
C FIVE FIXED SHIFT STEPS ARE TAKEN WITH A U,V CLOSE                     
C TO THE CLUSTER                                                        
      IF (RELSTP.LT.ETA) RELSTP = ETA                                   
      RELSTP = SQRT(RELSTP)                                             
      U = U - U*RELSTP                                                  
      V = V + V*RELSTP                                                  
      CALL D8RPLY(NN, U, V, P, QP, A, B)                                
      DO 40 I=1,5                                                       
        CALL D2RPLY(TYPE, K, QK)                                        
        CALL D5RPLY(TYPE, P, QP, K, QK)                                 
   40 CONTINUE                                                          
      TRIED = .TRUE.                                                    
      J = 0                                                             
   50 OMP = MP                                                          
C CALCULATE NEXT K POLYNOMIAL AND NEW U AND V                           
      CALL D2RPLY(TYPE, K, QK)                                          
      CALL D5RPLY(TYPE, P, QP, K, QK)                                   
      CALL D2RPLY(TYPE, K, QK)                                          
      CALL D4RPLY(TYPE, UI, VI, P, K)                                   
C IF VI IS ZERO THE ITERATION IS NOT CONVERGING                         
      IF (VI.EQ.0.D0) RETURN                                            
      RELSTP = DABS((VI-V)/VI)                                          
      U = UI                                                            
      V = VI                                                            
      GO TO 10                                                          
      END                                                               
      SUBROUTINE D2RPLY(TYPE, K, QK)                                    
C THIS ROUTINE CALCULATES SCALAR QUANTITIES USED TO                     
C COMPUTE THE NEXT K POLYNOMIAL AND NEW ESTIMATES OF                    
C THE QUADRATIC COEFFICIENTS.                                           
C TYPE - INTEGER VARIABLE SET HERE INDICATING HOW THE                   
C CALCULATIONS ARE NORMALIZED TO AVOID OVERFLOW                         
C                                                                       
      COMMON /P77PLY/ SR, SI, U,                                        
     1 V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                      
     2 H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                      
C                                                                       
      INTEGER N, NN                                                     
      INTEGER TYPE                                                      
      REAL ETA, ARE, MRE                                                
C                                                                       
      DOUBLE PRECISION K(1), QK(1), SR, SI, U, V, A, B, C, D,           
     1 A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                        
     2 LZR, LZI                                                         
C SYNTHETIC DIVISION OF K BY THE QUADRATIC 1,U,V                        
      CALL D8RPLY(N, U, V, K, QK, C, D)                                 
      IF (DABS(C).GT.DABS(K(N))*100.*ETA) GO TO 10                      
      IF (DABS(D).GT.DABS(K(N-1))*100.*ETA) GO TO 10                    
      TYPE = 3                                                          
C TYPE=3 INDICATES THE QUADRATIC IS ALMOST A FACTOR                     
C OF K                                                                  
      RETURN                                                            
   10 IF (DABS(D).LT.DABS(C)) GO TO 20                                  
      TYPE = 2                                                          
C TYPE=2 INDICATES THAT ALL FORMULAS ARE DIVIDED BY D                   
      E = A/D                                                           
      F = C/D                                                           
      G = U*B                                                           
      H = V*B                                                           
      A3 = (A+G)*E + H*(B/D)                                            
      A1 = B*F - A                                                      
      A7 = (F+U)*A + H                                                  
      RETURN                                                            
   20 TYPE = 1                                                          
C TYPE=1 INDICATES THAT ALL FORMULAS ARE DIVIDED BY C                   
      E = A/C                                                           
      F = D/C                                                           
      G = U*E                                                           
      H = V*B                                                           
      A3 = A*E + (H/C+G)*B                                              
      A1 = B - A*(D/C)                                                  
      A7 = A + G*D + H*F                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE D4RPLY(TYPE, UU, VV, P, K)                             
C COMPUTE NEW ESTIMATES OF THE QUADRATIC COEFFICIENTS                   
C USING THE SCALARS COMPUTED IN D2RPLY.                                 
      COMMON /P77PLY/ SR, SI, U,                                        
     1 V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                      
     2 H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                      
C                                                                       
      INTEGER N, NN                                                     
      INTEGER TYPE                                                      
      REAL ETA, ARE, MRE                                                
C                                                                       
      DOUBLE PRECISION P(1), K(1),                                      
     1  SR, SI, U, V, A, B, C, D,                                       
     2 A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                        
     3 LZR, LZI                                                         
      DOUBLE PRECISION A4, A5, B1, B2, C1, C2, C3,                      
     1 C4, TEMP, UU, VV                                                 
C USE FORMULAS APPROPRIATE TO SETTING OF TYPE.                          
      IF (TYPE.EQ.3) GO TO 30                                           
      IF (TYPE.EQ.2) GO TO 10                                           
      A4 = A + U*B + H*F                                                
      A5 = C + (U+V*F)*D                                                
      GO TO 20                                                          
   10 A4 = (A+G)*F + H                                                  
      A5 = (F+U)*C + V*D                                                
C EVALUATE NEW QUADRATIC COEFFICIENTS.                                  
   20 B1 = -K(N)/P(NN)                                                  
      B2 = -(K(N-1)+B1*P(N))/P(NN)                                      
      C1 = V*B2*A1                                                      
      C2 = B1*A7                                                        
      C3 = B1*B1*A3                                                     
      C4 = C1 - C2 - C3                                                 
      TEMP = A5 + B1*A4 - C4                                            
      IF (TEMP.EQ.0.D0) GO TO 30                                        
      UU = U - (U*(C3+C2)+V*(B1*A1+B2*A7))/TEMP                         
      VV = V*(1.+C4/TEMP)                                               
      RETURN                                                            
C IF TYPE=3 THE QUADRATIC IS ZEROED                                     
   30 UU = 0.D0                                                         
      VV = 0.D0                                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE D5RPLY(TYPE, P, QP, K, QK)                             
C COMPUTES THE NEXT K POLYNOMIALS USING SCALARS                         
C COMPUTED IN D2RPLY                                                    
C                                                                       
      COMMON /P77PLY/ SR, SI, U,                                        
     1 V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                      
     2 H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                      
C                                                                       
      INTEGER N, NN                                                     
      INTEGER TYPE                                                      
C                                                                       
      REAL ETA, ARE, MRE                                                
      DOUBLE PRECISION P(1), QP(2), K(2),                               
     1 QK(1), SR, SI, U, V, A, B, C, D,                                 
     2 A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                        
     3 LZR, LZI                                                         
      DOUBLE PRECISION TEMP                                             
      IF (TYPE.EQ.3) GO TO 40                                           
      TEMP = A                                                          
      IF (TYPE.EQ.1) TEMP = B                                           
      IF (DABS(A1).GT.DABS(TEMP)*ETA*10.) GO TO 20                      
C IF A1 IS NEARLY ZERO THEN USE A SPECIAL FORM OF THE                   
C RECURRENCE                                                            
      K(1) = 0.D0                                                       
      K(2) = -A7*QP(1)                                                  
      DO 10 I=3,N                                                       
        K(I) = A3*QK(I-2) - A7*QP(I-1)                                  
   10 CONTINUE                                                          
      RETURN                                                            
C USE SCALED FORM OF THE RECURRENCE                                     
   20 A7 = A7/A1                                                        
      A3 = A3/A1                                                        
      K(1) = QP(1)                                                      
      K(2) = QP(2) - A7*QP(1)                                           
      DO 30 I=3,N                                                       
        K(I) = A3*QK(I-2) - A7*QP(I-1) + QP(I)                          
   30 CONTINUE                                                          
      RETURN                                                            
C USE UNSCALED FORM OF THE RECURRENCE IF TYPE IS 3                      
   40 K(1) = 0.D0                                                       
      K(2) = 0.D0                                                       
      DO 50 I=3,N                                                       
        K(I) = QK(I-2)                                                  
   50 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE D6RPLY(A, B1, C, SR, SI, LR, LI)                       
C CALCULATE THE ZEROS OF THE QUADRATIC A*Z**2+B1*Z+C.                   
C THE QUADRATIC FORMULA, MODIFIED TO AVOID                              
C OVERFLOW, IS USED TO FIND THE LARGER ZERO IF THE                      
C ZEROS ARE REAL AND BOTH ZEROS ARE COMPLEX.                            
C THE SMALLER REAL ZERO IS FOUND DIRECTLY FROM THE                      
C PRODUCT OF THE ZEROS C/A.                                             
      DOUBLE PRECISION A, B1, C, SR, SI, LR, LI, B,                     
     1 D, E, DSQRT                                                      
      IF (A.NE.0.D0) GO TO 20                                           
      SR = 0.D0                                                         
      IF (B1.NE.0.D0) SR = -C/B1                                        
      LR = 0.D0                                                         
   10 SI = 0.D0                                                         
      LI = 0.D0                                                         
      RETURN                                                            
   20 IF (C.NE.0.D0) GO TO 30                                           
      SR = 0.D0                                                         
      LR = -B1/A                                                        
      GO TO 10                                                          
C COMPUTE DISCRIMINANT AVOIDING OVERFLOW                                
   30 B = B1/2.D0                                                       
      IF (DABS(B).LT.DABS(C)) GO TO 40                                  
      E = 1.D0 - (A/B)*(C/B)                                            
      D = DSQRT(DABS(E))*DABS(B)                                        
      GO TO 50                                                          
   40 E = A                                                             
      IF (C.LT.0.D0) E = -A                                             
      E = B*(B/DABS(C)) - E                                             
      D = DSQRT(DABS(E))*DSQRT(DABS(C))                                 
   50 IF (E.LT.0.D0) GO TO 60                                           
C REAL ZEROS                                                            
      IF (B.GE.0.D0) D = -D                                             
      LR = (-B+D)/A                                                     
      SR = 0.D0                                                         
      IF (LR.NE.0.D0) SR = (C/LR)/A                                     
      GO TO 10                                                          
C COMPLEX CONJUGATE ZEROS                                               
   60 SR = -B/A                                                         
      LR = SR                                                           
      SI = DABS(D/A)                                                    
      LI = -SI                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE D8RPLY(NN, U, V, P, Q, A, B)                           
C DIVIDES P BY THE QUADRATIC  1,U,V  PLACING THE                        
C QUOTIENT IN Q AND THE REMAINDER IN A,B                                
      DOUBLE PRECISION P(NN), Q(NN), U, V, A, B, C                      
      INTEGER I                                                         
      B = P(1)                                                          
      Q(1) = B                                                          
      A = P(2) - U*B                                                    
      Q(2) = A                                                          
      DO 10 I=3,NN                                                      
        C = P(I) - U*A - V*B                                            
        Q(I) = C                                                        
        B = A                                                           
        A = C                                                           
   10 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE D9RPLY(SSS, NZ, IFLAG, P, QP, K, QK)                   
C VARIABLE-SHIFT H POLYNOMIAL ITERATION FOR A REAL                      
C ZERO.                                                                 
C SSS   - STARTING ITERATE                                              
C NZ    - NUMBER OF ZERO FOUND                                          
C IFLAG - FLAG TO INDICATE A PAIR OF ZEROS NEAR REAL                    
C         AXIS.                                                         
C                                                                       
      COMMON /P77PLY/ SR, SI, U,                                        
     1 V, A, B, C, D, A1, A2, A3, A6, A7, E, F, G,                      
     2 H, SZR, SZI, LZR, LZI, ETA, ARE, MRE, N, NN                      
C                                                                       
      INTEGER N, NN                                                     
      INTEGER NZ, IFLAG, I, J, NM1                                      
C                                                                       
      REAL ETA, ARE, MRE                                                
      REAL MS, MP, OMP, EE                                              
C                                                                       
      DOUBLE PRECISION P(1), QP(1), K(1),                               
     1   QK(1), SR, SI, U, V, A, B, C, D,                               
     2   A1, A2, A3, A6, A7, E, F, G, H, SZR, SZI,                      
     3   LZR, LZI                                                       
      DOUBLE PRECISION PV, KV, T, S, SSS                                
      NM1 = N - 1                                                       
      NZ = 0                                                            
      S = SSS                                                           
      IFLAG = 0                                                         
      J = 0                                                             
C MAIN LOOP                                                             
   10 PV = P(1)                                                         
C EVALUATE P AT S                                                       
      QP(1) = PV                                                        
      DO 20 I=2,NN                                                      
        PV = PV*S + P(I)                                                
        QP(I) = PV                                                      
   20 CONTINUE                                                          
      MP = DABS(PV)                                                     
C COMPUTE A RIGOROUS BOUND ON THE ERROR IN EVALUATING                   
C P                                                                     
      MS = DABS(S)                                                      
      EE = (MRE/(ARE+MRE))*ABS(SNGL(QP(1)))                             
      DO 30 I=2,NN                                                      
        EE = EE*MS + ABS(SNGL(QP(I)))                                   
   30 CONTINUE                                                          
C ITERATION HAS CONVERGED SUFFICIENTLY IF THE                           
C POLYNOMIAL VALUE IS LESS THAN 20 TIMES THIS BOUND                     
      IF (MP.GT.20.*((ARE+MRE)*EE-MRE*MP)) GO TO 40                     
      NZ = 1                                                            
      SZR = S                                                           
      SZI = 0.D0                                                        
      RETURN                                                            
   40 J = J + 1                                                         
C STOP ITERATION AFTER 10 STEPS                                         
      IF (J.GT.10) RETURN                                               
      IF (J.LT.2) GO TO 50                                              
      IF (DABS(T).GT..001*DABS(S-T) .OR. MP.LE.OMP)                     
     1   GO TO 50                                                       
C A CLUSTER OF ZEROS NEAR THE REAL AXIS HAS BEEN                        
C ENCOUNTERED RETURN WITH IFLAG SET TO INITIATE A                       
C QUADRATIC ITERATION                                                   
      IFLAG = 1                                                         
      SSS = S                                                           
      RETURN                                                            
C RETURN IF THE POLYNOMIAL VALUE HAS INCREASED                          
C SIGNIFICANTLY                                                         
   50 OMP = MP                                                          
C COMPUTE T, THE NEXT POLYNOMIAL, AND THE NEW ITERATE                   
      KV = K(1)                                                         
      QK(1) = KV                                                        
      DO 60 I=2,N                                                       
        KV = KV*S + K(I)                                                
        QK(I) = KV                                                      
   60 CONTINUE                                                          
      IF (DABS(KV).LE.DABS(K(N))*10.*ETA) GO TO 80                      
C USE THE SCALED FORM OF THE RECURRENCE IF THE VALUE                    
C OF K AT S IS NONZERO                                                  
      T = -PV/KV                                                        
      K(1) = QP(1)                                                      
      DO 70 I=2,N                                                       
        K(I) = T*QK(I-1) + QP(I)                                        
   70 CONTINUE                                                          
      GO TO 100                                                         
C USE UNSCALED FORM                                                     
   80 K(1) = 0.0D0                                                      
      DO 90 I=2,N                                                       
        K(I) = QK(I-1)                                                  
   90 CONTINUE                                                          
  100 KV = K(1)                                                         
      DO 110 I=2,N                                                      
        KV = KV*S + K(I)                                                
  110 CONTINUE                                                          
      T = 0.D0                                                          
      IF (DABS(KV).GT.DABS(K(N))*10.*ETA) T = -PV/KV                    
      S = S + T                                                         
      GO TO 10                                                          
      END                                                               
      SUBROUTINE CPOLY(DEGREE,OPR,OPI,ZEROR,ZEROI)                      
C                                                                       
C FINDS THE ZEROS OF A COMPLEX POLYNOMIAL.                              
C                                                                       
C  INPUTS -                                                             
C                                                                       
C     OPR, OPI      -  VECTORS OF REAL AND IMAGINARY                    
C                      PARTS OF THE COEFFICIENTS IN                     
C                      ORDER OF DECREASING POWERS.                      
C                                                                       
C     DEGREE        -  INTEGER DEGREE OF POLYNOMIAL.                    
C                                                                       
C  OUTPUTS -                                                            
C                                                                       
C     ZEROR, ZEROI  -  VECTORS OF REAL AND IMAGINARY                    
C                      PARTS OF THE ZEROS.                              
C                                                                       
C THE PROGRAM HAS BEEN WRITTEN TO REDUCE THE CHANCE OF OVERFLOW         
C OCCURRING. IF IT DOES OCCUR, THERE IS STILL A POSSIBILITY THAT        
C THE ZEROFINDER WILL WORK PROVIDED THE OVERFLOWED QUANTITY IS          
C REPLACED BY A LARGE NUMBER.                                           
C                                                                       
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - DEGREE IS LESS THAN 1                                          
C    2 - LEADING COEFFICIENT IS ZERO                                    
C    3 - NOT ALL ZEROS HAVE BEEN FOUND (RECOVERABLE)                    
C    4 - THE DYNAMIC STORAGE STACK IS NOT BIG ENOUGH                    
C                                                                       
C                                                                       
C PORT NOTE -                                                           
C                                                                       
C THE ORIGINAL PROGRAM HAS BEEN ADAPTED TO PORT BY -                    
C                                                                       
C   (1) PUTTING IN AUTOMATIC ERROR HANDLING.                            
C   (2) SUBSTITUTING DYNAMIC STACK ALLOCATION FOR THE DIMENSIONED       
C       ARRAYS IN NAMED COMMON.                                         
C   (3) CHANGING THE NAMES OF THE INTERNAL ROUTINES TO AVOID USER       
C       NAME CONFLICT.                                                  
C                                                                       
C  THE FOLLOWING NAME EQUIVALENCES (ORIGINAL - NEW) APPLY -             
C                                                                       
C            CMOD    -  CR1MOD                                          
C            CDIVID  -  CR1DIV                                          
C            CALCT   -  R1POLY                                          
C            CAUCHY  -  R2POLY                                          
C            ERREV   -  R3POLY                                          
C            FXSHFT  -  R4POLY                                          
C            NEXTH   -  R5POLY                                          
C            NOSHFT  -  R6POLY                                          
C            POLYEV  -  R7POLY                                          
C            SCALE   -  R8POLY                                          
C            VRSHFT  -  R9POLY                                          
C                                                                       
C                                                                       
C DYNAMIC STORAGE SPACE USED -                                          
C                                                                       
C    THE CPOLY PROGRAMS USE 10*(DEGREE+1)                               
C    REAL LOCATIONS IN THE DYNAMIC STORAGE STACK.                       
C                                                                       
C COMMON AREA                                                           
      COMMON/CSTAK/D(500)                                               
      COMMON/P88PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN            
C                                                                       
      INTEGER DEGREE,CNT1,CNT2                                          
      REAL R(1000)                                                      
      REAL SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN                        
      REAL XX,YY,COSR,SINR,SMALNO,BASE,XXX,ZR,ZI,BND,                   
     1    ANG,OPR(1),OPI(1),ZEROR(1),ZEROI(1),                          
     2    CR1MOD,R8POLY,R2POLY,SQRT,R1MACH                              
      DOUBLE PRECISION D                                                
      LOGICAL CONV                                                      
C                                                                       
      EQUIVALENCE (D(1),R(1))                                           
C                                                                       
C INITIALIZATION OF CONSTANTS                                           
C                                                                       
      ETA = R1MACH(4)                                                   
      ARE = ETA                                                         
      INFIN = R1MACH(2)                                                 
      SMALNO = R1MACH(1)                                                
      BASE = I1MACH(10)                                                 
C                                                                       
      ANG = 94.E0/180.E0*(4.E0*ATAN(1.E0))                              
      COSR = COS(ANG)                                                   
      SINR = SIN(ANG)                                                   
C                                                                       
      XX = .5E0*SQRT(2.E0)                                              
      YY = -XX                                                          
      MRE = 4.E0*XX*ETA                                                 
C                                                                       
      NN = DEGREE+1                                                     
C                                                                       
C THE DEGREE IS LESS THAN 1                                             
C                                                                       
C/6S                                                                    
C     IF (DEGREE .LT. 1) CALL SETERR(                                   
C    1   33HCPOLY - THE DEGREE IS LESS THAN 1,33,1,2)                   
C/7S                                                                    
      IF (DEGREE .LT. 1) CALL SETERR(                                   
     1   'CPOLY - THE DEGREE IS LESS THAN 1',33,1,2)                    
C/                                                                      
C                                                                       
C ALGORITHM FAILS IF THE LEADING COEFFICIENT IS ZERO.                   
C                                                                       
C/6S                                                                    
C     IF (OPR(1) .EQ. 0.0E0 .AND. OPI(1) .EQ. 0.0E0) CALL SETERR(       
C    1   42H CPOLY - LEADING COEFFICIENT INPUT AS ZERO,42,2,2)          
C/7S                                                                    
      IF (OPR(1) .EQ. 0.0E0 .AND. OPI(1) .EQ. 0.0E0) CALL SETERR(       
     1   ' CPOLY - LEADING COEFFICIENT INPUT AS ZERO',42,2,2)           
C/                                                                      
C                                                                       
C REMOVE THE ZEROS AT THE ORIGIN IF ANY.                                
   10 IF (OPR(NN) .NE. 0.0E0 .OR. OPI(NN) .NE. 0.0E0) GO TO 20          
          IDNN2 = DEGREE-NN+2                                           
          ZEROR(IDNN2) = 0.0E0                                          
          ZEROI(IDNN2) = 0.0E0                                          
          NN = NN-1                                                     
          GO TO 10                                                      
C                                                                       
C                                                                       
C SET UP THE STORAGE IN THE DYNAMIC STORAGE STACK -                     
C IF THERE IS ROOM                                                      
C                                                                       
   20 NSHORT = ISTKQU(3) - 10*NN                                        
C/6S                                                                    
C     IF (NSHORT .LE. 0) CALL SETERR(                                   
C    1   47H CPOLY - THE DYNAMIC STORAGE LEFT IS NOT ENOUGH,47,3,2)     
C/7S                                                                    
      IF (NSHORT .LE. 0) CALL SETERR(                                   
     1   ' CPOLY - THE DYNAMIC STORAGE LEFT IS NOT ENOUGH',47,3,2)      
C/                                                                      
C                                                                       
      NNN = 10*NN                                                       
      IPR  = ISTKGT(NNN,3)                                              
      IPI  = IPR  + NN                                                  
      IHR  = IPI  + NN                                                  
      IHI  = IHR  + NN                                                  
      IQPR = IHI  + NN                                                  
      IQPI = IQPR + NN                                                  
      IQHR = IQPI + NN                                                  
      IQHI = IQHR + NN                                                  
      ISHR = IQHI + NN                                                  
      ISHI = ISHR + NN                                                  
C                                                                       
C MAKE A COPY OF THE COEFFICIENTS.                                      
      DO 30 I = 1,NN                                                    
        IIPR  = IPR + I -1                                              
        IIPI  = IPI + I -1                                              
        IISHR = ISHR + I -1                                             
          R(IIPR)  = OPR(I)                                             
          R(IIPI)  = OPI(I)                                             
          R(IISHR) = CR1MOD(R(IIPR),R(IIPI))                            
   30 CONTINUE                                                          
C                                                                       
C SCALE THE POLYNOMIAL.                                                 
      BND = R8POLY (NN,R(ISHR),ETA,INFIN,SMALNO,BASE)                   
      IF (BND .EQ. 1.0E0) GO TO 40                                      
      DO 35 I = 1,NN                                                    
        IIPR = IPR + I - 1                                              
        IIPI = IPI + I - 1                                              
          R(IIPR) = BND*R(IIPR)                                         
          R(IIPI) = BND*R(IIPI)                                         
   35 CONTINUE                                                          
C                                                                       
C START THE ALGORITHM FOR ONE ZERO .                                    
C                                                                       
   40 IF (NN .GT. 2) GO TO 50                                           
      IF (NN .EQ. 1) GO TO 110                                          
C                                                                       
C CALCULATE THE FINAL ZERO AND RETURN.                                  
          CALL CR1DIV(-R(IPR+1),-R(IPI+1),R(IPR),R(IPI),                
     1    ZEROR(DEGREE),ZEROI(DEGREE))                                  
          CALL ISTKRL(1)                                                
          RETURN                                                        
C                                                                       
C CALCULATE BND, A LOWER BOUND ON THE MODULUS OF THE ZEROS.             
   50 DO 60 I = 1,NN                                                    
        IISHR = ISHR + I - 1                                            
        IIPR = IPR + I - 1                                              
        IIPI = IPI + I - 1                                              
          R(IISHR) = CR1MOD(R(IIPR),R(IIPI))                            
   60 CONTINUE                                                          
      BND = R2POLY(NN,R(ISHR),R(ISHI))                                  
C                                                                       
C OUTER LOOP TO CONTROL 2 MAJOR PASSES WITH DIFFERENT SEQUENCES         
C OF SHIFTS.                                                            
      DO 100 CNT1 = 1,2                                                 
C FIRST STAGE CALCULATION, NO SHIFT.                                    
          CALL R6POLY(5,R(IPR),R(IPI),R(IHR),R(IHI))                    
C INNER LOOP TO SELECT A SHIFT.                                         
          DO 90 CNT2 = 1,9                                              
C SHIFT IS CHOSEN WITH MODULUS BND AND AMPLITUDE ROTATED BY             
C 94 DEGREES FROM THE PREVIOUS SHIFT                                    
               XXX = COSR*XX-SINR*YY                                    
               YY = SINR*XX+COSR*YY                                     
               XX = XXX                                                 
               SR = BND*XX                                              
               SI = BND*YY                                              
C SECOND STAGE CALCULATION, FIXED SHIFT.                                
          CALL R4POLY(10*CNT2,ZR,ZI,CONV,R(IPR),R(IPI),R(IHR),R(IHI),   
     1         R(IQPR),R(IQPI),R(IQHR),R(IQHI),R(ISHR),R(ISHI))         
               IF (.NOT. CONV) GO TO 80                                 
C THE SECOND STAGE JUMPS DIRECTLY TO THE THIRD STAGE ITERATION.         
C IF SUCCESSFUL THE ZERO IS STORED AND THE POLYNOMIAL DEFLATED.         
                    IDNN2 = DEGREE-NN+2                                 
                    ZEROR(IDNN2) = ZR                                   
                    ZEROI(IDNN2) = ZI                                   
                    NN = NN-1                                           
                    DO 70 I = 1,NN                                      
                       IIPR  = IPR + I -1                               
                       IIPI  = IPI + I - 1                              
                       IIQPR = IQPR + I - 1                             
                       IIQPI = IQPI + I - 1                             
                         R(IIPR) = R(IIQPR)                             
                         R(IIPI) = R(IIQPI)                             
   70               CONTINUE                                            
                    GO TO 40                                            
   80          CONTINUE                                                 
C IF THE ITERATION IS UNSUCCESSFUL ANOTHER SHIFT IS CHOSEN.             
   90     CONTINUE                                                      
C IF 9 SHIFTS FAIL, THE OUTER LOOP IS REPEATED WITH ANOTHER             
C SEQUENCE OF SHIFTS.                                                   
  100 CONTINUE                                                          
C                                                                       
C THE ZEROFINDER HAS FAILED ON TWO MAJOR PASSES.                        
C RETURN EMPTY HANDED.                                                  
C                                                                       
      KP10 = IDNN2 + 10                                                 
C                                                                       
C/6S                                                                    
C     CALL SETERR(37H CPOLY - ONLY K ZEROS HAVE BEEN FOUND,37,KP10,1)   
C/7S                                                                    
      CALL SETERR(' CPOLY - ONLY K ZEROS HAVE BEEN FOUND',37,KP10,1)    
C/                                                                      
  110 CALL ISTKRL(1)                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE R4POLY(L2,ZR,ZI,CONV,PR,PI,HR,HI,QPR,QPI,              
     1    QHR,QHI,SHR,SHI)                                              
C COMPUTES L2 FIXED-SHIFT H POLYNOMIALS AND TESTS FOR                   
C                                                                       
C CONVERGENCE.                                                          
C INITIATES A VARIABLE-SHIFT ITERATION AND RETURNS WITH THE             
C APPROXIMATE ZERO IF SUCCESSFUL.                                       
C L2 - LIMIT OF FIXED SHIFT STEPS                                       
C ZR,ZI - APPROXIMATE ZERO IF CONV IS .TRUE.                            
C CONV  - LOGICAL INDICATING CONVERGENCE OF STAGE 3 ITERATION           
C COMMON AREA                                                           
      COMMON/P88PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN            
C                                                                       
      REAL SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,                       
     1    PR(1),PI(1),HR(1),HI(1),QPR(1),QPI(1),QHR(1),                 
     2    QHI(1),SHR(1),SHI(1)                                          
      REAL ZR,ZI,OTR,OTI,SVSR,SVSI,CR1MOD                               
          LOGICAL CONV,TEST,PASD,BOOL                                   
      N = NN-1                                                          
C EVALUATE P AT S.                                                      
      CALL R7POLY(NN,SR,SI,PR,PI,QPR,QPI,PVR,PVI)                       
      TEST = .TRUE.                                                     
      PASD = .FALSE.                                                    
C CALCULATE FIRST T = -P(S)/H(S).                                       
      CALL R1POLY(BOOL,HR,HI,QHR,QHI)                                   
C MAIN LOOP FOR ONE SECOND STAGE STEP.                                  
      DO 50 J = 1,L2                                                    
          OTR = TR                                                      
          OTI = TI                                                      
C COMPUTE NEXT H POLYNOMIAL AND NEW T.                                  
          CALL R5POLY(BOOL,HR,HI,QPR,QPI,QHR,QHI)                       
          CALL R1POLY(BOOL,HR,HI,QHR,QHI)                               
          ZR = SR+TR                                                    
          ZI = SI+TI                                                    
C TEST FOR CONVERGENCE UNLESS STAGE 3 HAS FAILED ONCE OR THIS           
C IS THE LAST H POLYNOMIAL .                                            
          IF ( BOOL .OR. .NOT. TEST .OR. J .EQ. L2) GO TO 50            
          IF (CR1MOD(TR-OTR,TI-OTI) .GE. .5E0*CR1MOD(ZR,ZI)) GO TO 40   
               IF (.NOT. PASD) GO TO 30                                 
C THE WEAK CONVERGENCE TEST HAS BEEN PASSED TWICE, START THE            
C THIRD STAGE ITERATION, AFTER SAVING THE CURRENT H POLYNOMIAL          
C AND SHIFT.                                                            
                    DO 10 I = 1,N                                       
                         SHR(I) = HR(I)                                 
                         SHI(I) = HI(I)                                 
   10               CONTINUE                                            
                    SVSR = SR                                           
                    SVSI = SI                                           
               CALL R9POLY(10,ZR,ZI,CONV,PR,PI,HR,HI,QPR,QPI,QHR,QHI)   
                    IF (CONV) RETURN                                    
C THE ITERATION FAILED TO CONVERGE. TURN OFF TESTING AND RESTORE        
C H,S,PV AND T.                                                         
                    TEST = .FALSE.                                      
                    DO 20 I = 1,N                                       
                         HR(I) = SHR(I)                                 
                         HI(I) = SHI(I)                                 
   20               CONTINUE                                            
                    SR = SVSR                                           
                    SI = SVSI                                           
                    CALL R7POLY(NN,SR,SI,PR,PI,QPR,QPI,PVR,PVI)         
                    CALL R1POLY(BOOL,HR,HI,QHR,QHI)                     
                    GO TO 50                                            
   30          PASD = .TRUE.                                            
               GO TO 50                                                 
   40     PASD = .FALSE.                                                
   50 CONTINUE                                                          
C ATTEMPT AN ITERATION WITH FINAL H POLYNOMIAL FROM SECOND STAGE.       
      CALL R9POLY(10,ZR,ZI,CONV,PR,PI,HR,HI,QPR,QPI,QHR,QHI)            
      RETURN                                                            
      END                                                               
      SUBROUTINE R9POLY(L3,ZR,ZI,CONV,PR,PI,HR,HI,QPR,QPI,QHR,QHI)      
C CARRIES OUT THE THIRD STAGE ITERATION.                                
C L3 - LIMIT OF STEPS IN STAGE 3.                                       
C ZR,ZI   - ON ENTRY CONTAINS THE INITIAL ITERATE, IF THE               
C ITERATION CONVERGES IT CONTAINS THE FINAL ITERATE                     
C ON EXIT.                                                              
C CONV    -  .TRUE. IF ITERATION CONVERGES                              
C COMMON AREA                                                           
      COMMON/P88PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN            
C                                                                       
      REAL SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,                       
     1    PR(1),PI(1),HR(1),HI(1),QPR(1),QPI(1),QHR(1),QHI(1)           
      REAL ZR,ZI,MP,MS,OMP,RELSTP,R1,R2,CR1MOD,SQRT                     
      REAL R3POLY,TP                                                    
      LOGICAL CONV,B,BOOL                                               
      CONV = .FALSE.                                                    
      B = .FALSE.                                                       
      SR = ZR                                                           
      SI = ZI                                                           
C MAIN LOOP FOR STAGE THREE                                             
      DO 60 I = 1,L3                                                    
C EVALUATE P AT S AND TEST FOR CONVERGENCE.                             
          CALL R7POLY(NN,SR,SI,PR,PI,QPR,QPI,PVR,PVI)                   
          MP = CR1MOD(PVR,PVI)                                          
          MS = CR1MOD(SR,SI)                                            
          IF (MP .GT. 20.0E0*R3POLY(NN,QPR,QPI,MS,MP,ARE,MRE))          
     1       GO TO 10                                                   
C POLYNOMIAL VALUE IS SMALLER IN VALUE THAN A BOUND ON THE ERROR        
C IN EVALUATING P, TERMINATE THE ITERATION.                             
               CONV = .TRUE.                                            
               ZR = SR                                                  
               ZI = SI                                                  
               RETURN                                                   
   10     IF (I .EQ. 1) GO TO 40                                        
               IF (B .OR. MP .LT.OMP .OR. RELSTP .GE. .05E0)            
     1            GO TO 30                                              
C ITERATION HAS STALLED. PROBABLY A CLUSTER OF ZEROS. DO 5 FIXED        
C SHIFT STEPS INTO THE CLUSTER TO FORCE ONE ZERO TO DOMINATE.           
                    TP = RELSTP                                         
                    B = .TRUE.                                          
                    IF (RELSTP .LT. ETA) TP = ETA                       
                    R1 = SQRT(TP)                                       
                    R2 = SR*(1.0E0+R1)-SI*R1                            
                    SI = SR*R1+SI*(1.0E0+R1)                            
                    SR = R2                                             
                    CALL R7POLY(NN,SR,SI,PR,PI,QPR,QPI,PVR,PVI)         
                    DO 20 J = 1,5                                       
                         CALL R1POLY(BOOL,HR,HI,QHR,QHI)                
                         CALL R5POLY(BOOL,HR,HI,QPR,QPI,QHR,QHI)        
   20               CONTINUE                                            
      OMP = INFIN                                                       
                    GO TO 50                                            
C EXIT IF POLYNOMIAL VALUE INCREASES SIGNIFICANTLY.                     
   30          IF (MP*.1E0 .GT. OMP) RETURN                             
   40     OMP = MP                                                      
C CALCULATE NEXT ITERATE.                                               
   50     CALL R1POLY(BOOL,HR,HI,QHR,QHI)                               
          CALL R5POLY(BOOL,HR,HI,QPR,QPI,QHR,QHI)                       
          CALL R1POLY(BOOL,HR,HI,QHR,QHI)                               
          IF (BOOL) GO TO 60                                            
          RELSTP = CR1MOD(TR,TI)/CR1MOD(SR,SI)                          
          SR = SR+TR                                                    
          SI = SI+TI                                                    
   60 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      REAL FUNCTION R3POLY(NN,QR,QI,MS,MP,ARE,MRE)                      
C BOUNDS THE ERROR IN EVALUATING THE POLYNOMIAL BY THE HORNER           
C RECURRENCE.                                                           
C QR,QI - THE PARTIAL SUMS                                              
C MS    -MODULUS OF THE POINT                                           
C MP    -MODULUS OF POLYNOMIAL VALUE                                    
C ARE, MRE -ERROR BOUNDS ON COMPLEX ADDITION AND MULTIPLICATION         
      REAL QR(1),QI(1),MS,MP,ARE,MRE,E,CR1MOD                           
      E = CR1MOD(QR(1),QI(1))*MRE/(ARE+MRE)                             
      DO 10 I = 1,NN                                                    
          E = E*MS+CR1MOD(QR(I),QI(I))                                  
   10 CONTINUE                                                          
      R3POLY = E*(ARE+MRE)-MP*MRE                                       
      RETURN                                                            
      END                                                               
      REAL FUNCTION R2POLY(NN,PT,Q)                                     
C R2POLY COMPUTES A LOWER BOUND ON THE MODULI OF THE ZEROS OF A         
C POLYNOMIAL - PT IS THE MODULUS OF THE COEFFICIENTS.                   
      REAL Q(1),PT(1),X,XM,F,DX,DF,                                     
     1   ABS,EXP,ALOG                                                   
      PT(NN) = -PT(NN)                                                  
C COMPUTE UPPER ESTIMATE OF BOUND.                                      
      N = NN-1                                                          
      X = EXP( (ALOG(-PT(NN)) - ALOG(PT(1)))/FLOAT(N) )                 
      IF (PT(N).EQ.0.0E0) GO TO 20                                      
C IF NEWTON STEP AT THE ORIGIN IS BETTER, USE IT.                       
          XM = -PT(NN)/PT(N)                                            
          IF (XM.LT.X) X=XM                                             
C CHOP THE INTERVAL (0,X) UNTIL F LE 0.                                 
   20 XM = X*.1E0                                                       
      F = PT(1)                                                         
      DO 30 I = 2,NN                                                    
          F = F*XM+PT(I)                                                
   30 CONTINUE                                                          
      IF (F.LE. 0.0E0) GO TO 40                                         
          X = XM                                                        
          GO TO 20                                                      
   40 DX = X                                                            
C DO NEWTON ITERATION UNTIL X CONVERGES TO TWO DECIMAL PLACES.          
   50 IF (ABS(DX/X) .LE. .005E0) GO TO 70                               
          Q(1) = PT(1)                                                  
          DO 60 I = 2,NN                                                
               Q(I) = Q(I-1)*X+PT(I)                                    
   60     CONTINUE                                                      
          F = Q(NN)                                                     
          DF = Q(1)                                                     
          DO 65 I = 2,N                                                 
               DF = DF*X+Q(I)                                           
   65     CONTINUE                                                      
          DX = F/DF                                                     
          X = X-DX                                                      
          GO TO 50                                                      
   70 R2POLY = X                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE R1POLY(BOOL,HR,HI,QHR,QHI)                             
C COMPUTES  T = -P(S)/H(S).                                             
C BOOL   - LOGICAL, SET TRUE IF H(S) IS ESSENTIALLY ZERO.               
C COMMON AREA                                                           
      COMMON/P88PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN            
C                                                                       
      REAL SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN                        
      REAL HR(1),HI(1),QHR(1),QHI(1)                                    
      REAL HVR,HVI,CR1MOD                                               
      LOGICAL BOOL                                                      
      N = NN-1                                                          
C EVALUATE H(S).                                                        
      CALL R7POLY(N,SR,SI,HR,HI,QHR,QHI,HVR,HVI)                        
      BOOL = CR1MOD(HVR,HVI) .LE. ARE*10.0E0*CR1MOD(HR(N),HI(N))        
      IF (BOOL) GO TO 10                                                
          CALL CR1DIV(-PVR,-PVI,HVR,HVI,TR,TI)                          
          RETURN                                                        
   10 TR = 0.0E0                                                        
      TI = 0.0E0                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE R5POLY(BOOL,HR,HI,QPR,QPI,QHR,QHI)                     
C CALCULATES THE NEXT SHIFTED H POLYNOMIAL.                             
C BOOL   -  LOGICAL, IF .TRUE. H(S) IS ESSENTIALLY ZERO                 
C COMMON AREA                                                           
      COMMON/P88PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN            
C                                                                       
      REAL SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,                       
     1    HR(1),HI(1),QPR(1),QPI(1),QHR(1),QHI(1)                       
      REAL T1,T2                                                        
      LOGICAL BOOL                                                      
      N = NN-1                                                          
      NM1 = N-1                                                         
      IF (BOOL) GO TO 20                                                
          DO 10 J = 2,N                                                 
               T1 = QHR(J-1)                                            
               T2 = QHI(J-1)                                            
               HR(J) = TR*T1-TI*T2+QPR(J)                               
               HI(J) = TR*T2+TI*T1+QPI(J)                               
   10     CONTINUE                                                      
          HR(1) = QPR(1)                                                
          HI(1) = QPI(1)                                                
          RETURN                                                        
C IF H(S) IS ZERO REPLACE H WITH QH.                                    
   20 DO 30 J = 2,N                                                     
          HR(J) = QHR(J-1)                                              
          HI(J) = QHI(J-1)                                              
   30 CONTINUE                                                          
      HR(1) = 0.0E0                                                     
      HI(1) = 0.0E0                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE R6POLY(L1,PR,PI,HR,HI)                                 
C COMPUTES  THE DERIVATIVE  POLYNOMIAL AS THE INITIAL H                 
C POLYNOMIAL AND COMPUTES L1 NO-SHIFT H POLYNOMIALS.                    
C COMMON AREA                                                           
      COMMON/P88PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN            
C                                                                       
      REAL SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,                       
     1    PR(1),PI(1),HR(1),HI(1)                                       
      REAL XNI,T1,T2,CR1MOD                                             
      N = NN-1                                                          
      NM1 = N-1                                                         
      DO 10 I = 1,N                                                     
          XNI = NN-I                                                    
          HR(I) = XNI*PR(I)/FLOAT(N)                                    
          HI(I) = XNI*PI(I)/FLOAT(N)                                    
   10 CONTINUE                                                          
      DO 50 JJ = 1,L1                                                   
          IF (CR1MOD(HR(N),HI(N)) .LE. ETA*10.0E0*CR1MOD(PR(N),PI(N)))  
     *    GO TO 30                                                      
          CALL CR1DIV(-PR(NN),-PI(NN),HR(N),HI(N),TR,TI)                
          DO 20 I = 1,NM1                                               
               J = NN-I                                                 
               T1 = HR(J-1)                                             
               T2 = HI(J-1)                                             
               HR(J) = TR*T1-TI*T2+PR(J)                                
               HI(J) = TR*T2+TI*T1+PI(J)                                
   20     CONTINUE                                                      
          HR(1) = PR(1)                                                 
          HI(1) = PI(1)                                                 
          GO TO 50                                                      
C IF THE CONSTANT TERM IS ESSENTIALLY ZERO, SHIFT H COEFFICIENTS.       
   30     DO 40 I = 1,NM1                                               
               J = NN-I                                                 
               HR(J) = HR(J-1)                                          
               HI(J) = HI(J-1)                                          
   40     CONTINUE                                                      
          HR(1) = 0.0E0                                                 
          HI(1) = 0.0E0                                                 
   50 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE R7POLY(NN,SR,SI,PR,PI,QR,QI,PVR,PVI)                   
C EVALUATES A POLYNOMIAL  P  AT  S  BY THE HORNER RECURRENCE            
C PLACING THE PARTIAL SUMS IN Q AND THE COMPUTED VALUE IN PV.           
      REAL PR(1),PI(1),QR(1),QI(1),                                     
     1    SR,SI,PVR,PVI,T                                               
      QR(1) = PR(1)                                                     
      QI(1) = PI(1)                                                     
      PVR = QR(1)                                                       
      PVI = QI(1)                                                       
      DO 10 I = 2,NN                                                    
          T = PVR*SR-PVI*SI+PR(I)                                       
          PVI = PVR*SI+PVI*SR+PI(I)                                     
          PVR = T                                                       
          QR(I) = PVR                                                   
          QI(I) = PVI                                                   
   10 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      REAL FUNCTION R8POLY(NN,PT,ETA,INFIN,SMALNO,BASE)                 
C RETURNS A SCALE FACTOR TO MULTIPLY THE COEFFICIENTS OF THE            
C POLYNOMIAL. THE SCALING IS DONE TO AVOID OVERFLOW AND TO AVOID        
C UNDETECTED UNDERFLOW INTERFERING WITH THE CONVERGENCE                 
C CRITERION.  THE FACTOR IS A POWER OF THE BASE.                        
C PT - MODULUS OF COEFFICIENTS OF P                                     
C ETA,INFIN,SMALNO,BASE - CONSTANTS DESCRIBING THE                      
C FLOATING POINT ARITHMETIC.                                            
      REAL PT(1),ETA,INFIN,SMALNO,BASE,HI,LO,                           
     1    MAX,MIN,X,SC,SQRT,ALOG                                        
C FIND LARGEST AND SMALLEST MODULI OF COEFFICIENTS.                     
      HI = SQRT(INFIN)                                                  
      LO = SMALNO/ETA                                                   
      MAX = 0.0E0                                                       
      MIN = INFIN                                                       
      DO 10 I = 1,NN                                                    
          X = PT(I)                                                     
          IF (X .GT. MAX) MAX = X                                       
          IF (X .NE. 0.0E0 .AND. X.LT.MIN) MIN = X                      
   10 CONTINUE                                                          
C SCALE ONLY IF THERE ARE VERY LARGE OR VERY SMALL COMPONENTS.          
      R8POLY = 1.0E0                                                    
      IF (MIN .GE. LO .AND. MAX .LE. HI) RETURN                         
      X = LO/MIN                                                        
      IF (X .GT. 1.0E0) GO TO 20                                        
          SC = 1.0E0/(SQRT(MAX)*SQRT(MIN))                              
          GO TO 30                                                      
   20 SC = X                                                            
      IF (INFIN/SC .GT. MAX) SC = 1.0E0                                 
   30 L = ALOG(SC)/ALOG(BASE) + .500                                    
      R8POLY = BASE**L                                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE DCPOLY(DEGREE,OPR,OPI,ZEROR,ZEROI)                     
C                                                                       
C FINDS THE ZEROS OF A COMPLEX POLYNOMIAL.                              
C                                                                       
C  INPUTS -                                                             
C                                                                       
C     OPR, OPI      -  DOUBLE PRECISION VECTORS OF REAL AND             
C                      IMAGINARY PARTS OF THE COEFFICIENTS IN           
C                      ORDER OF DECREASING POWERS.                      
C                                                                       
C     DEGREE        -  INTEGER DEGREE OF POLYNOMIAL.                    
C                                                                       
C  OUTPUTS -                                                            
C                                                                       
C     ZEROR, ZEROI  -  DOUBLE PRECISION VECTORS OF REAL                 
C                      AND IMAGINARY PARTS OF THE ZEROS.                
C                                                                       
C THE PROGRAM HAS BEEN WRITTEN TO REDUCE THE CHANCE OF OVERFLOW         
C OCCURRING. IF IT DOES OCCUR, THERE IS STILL A POSSIBILITY THAT        
C THE ZEROFINDER WILL WORK PROVIDED THE OVERFLOWED QUANTITY IS          
C REPLACED BY A LARGE NUMBER.                                           
C                                                                       
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - DEGREE IS LESS THAN 1                                          
C    2 - LEADING COEFFICIENT IS ZERO                                    
C    3 - NOT ALL ZEROS HAVE BEEN FOUND (RECOVERABLE)                    
C    4 - THE DYNAMIC STORAGE STACK IS NOT BIG ENOUGH                    
C                                                                       
C                                                                       
C PORT NOTE -                                                           
C                                                                       
C THE ORIGINAL PROGRAM HAS BEEN ADAPTED TO PORT BY -                    
C                                                                       
C   (1) PUTTING IN AUTOMATIC ERROR HANDLING.                            
C   (2) SUBSTITUTING DYNAMIC STACK ALLOCATION FOR THE DIMENSIONED       
C       ARRAYS IN NAMED COMMON.                                         
C   (3) CHANGING THE NAMES OF THE INTERNAL ROUTINES TO AVOID USER       
C       NAME CONFLICT.                                                  
C                                                                       
C  THE FOLLOWING NAME EQUIVALENCES (ORIGINAL - NEW) APPLY -             
C                                                                       
C            CMOD    -  CD1MOD                                          
C            CDIVID  -  CD1DIV                                          
C            CALCT   -  D1POLY                                          
C            CAUCHY  -  D2POLY                                          
C            ERREV   -  D3POLY                                          
C            FXSHFT  -  D4POLY                                          
C            NEXTH   -  D5POLY                                          
C            NOSHFT  -  D6POLY                                          
C            POLYEV  -  D7POLY                                          
C            SCALE   -  D8POLY                                          
C            VRSHFT  -  D9POLY                                          
C                                                                       
C                                                                       
C DYNAMIC STORAGE SPACE USED -                                          
C                                                                       
C    THE DCPOLY PROGRAMS USE 10*(DEGREE+1)                              
C    DOUBLE-PRECISION LOCATIONS IN THE DYNAMIC                          
C    STORAGE STACK.                                                     
C                                                                       
C COMMON AREA                                                           
      COMMON/CSTAK/D(500)                                               
      COMMON/P99PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN            
C                                                                       
      DOUBLE PRECISION SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,D          
      DOUBLE PRECISION XX,YY,COSR,SINR,SMALNO,BASE,XXX,ZR,ZI,BND,       
     1    ANG,OPR(1),OPI(1),ZEROR(1),ZEROI(1),                          
     2    CD1MOD,D8POLY,D2POLY,DSQRT,D1MACH                             
      DOUBLE PRECISION DATAN, DCOS, DSIN                                
      LOGICAL CONV                                                      
      INTEGER DEGREE,CNT1,CNT2                                          
C                                                                       
C                                                                       
C INITIALIZATION OF CONSTANTS                                           
C                                                                       
      ETA = D1MACH(4)                                                   
      ARE = ETA                                                         
      INFIN = D1MACH(2)                                                 
      SMALNO = D1MACH(1)                                                
      BASE = I1MACH(10)                                                 
C                                                                       
      ANG = 94.D0/180.D0*(4.D0*DATAN(1.D0))                             
      COSR = DCOS(ANG)                                                  
      SINR = DSIN(ANG)                                                  
C                                                                       
      XX = .5D0*DSQRT(2.D0)                                             
      YY = -XX                                                          
      MRE = 4.D0*XX*ETA                                                 
C                                                                       
      NN = DEGREE+1                                                     
C                                                                       
C THE DEGREE MUST BE AT LEAST 1.                                        
C                                                                       
C/6S                                                                    
C     IF (DEGREE .LT. 1) CALL SETERR(                                   
C    1   34HDCPOLY - THE DEGREE IS LESS THAN 1,34,1,2)                  
C/7S                                                                    
      IF (DEGREE .LT. 1) CALL SETERR(                                   
     1   'DCPOLY - THE DEGREE IS LESS THAN 1',34,1,2)                   
C/                                                                      
C                                                                       
C ALGORITHM FAILS IF THE LEADING COEFFICIENT IS ZERO.                   
C                                                                       
C/6S                                                                    
C     IF (OPR(1) .EQ. 0.0D0 .AND. OPI(1) .EQ. 0.0D0) CALL SETERR(       
C    1   42HDCPOLY - LEADING COEFFICIENT INPUT AS ZERO,42,2,2)          
C/7S                                                                    
      IF (OPR(1) .EQ. 0.0D0 .AND. OPI(1) .EQ. 0.0D0) CALL SETERR(       
     1   'DCPOLY - LEADING COEFFICIENT INPUT AS ZERO',42,2,2)           
C/                                                                      
C                                                                       
C REMOVE THE ZEROS AT THE ORIGIN IF ANY.                                
   10 IF (OPR(NN) .NE. 0.0D0 .OR. OPI(NN) .NE. 0.0D0) GO TO 20          
          IDNN2 = DEGREE-NN+2                                           
          ZEROR(IDNN2) = 0.0D0                                          
          ZEROI(IDNN2) = 0.0D0                                          
          NN = NN-1                                                     
          GO TO 10                                                      
C                                                                       
C                                                                       
C SET UP THE STORAGE IN THE DYNAMIC STORAGE STACK -                     
C IF THERE IS ROOM                                                      
C                                                                       
   20 NSHORT = ISTKQU(4) - 10*NN                                        
C/6S                                                                    
C     IF (NSHORT .LE. 0) CALL SETERR(                                   
C    1   47HDCPOLY - THE DYNAMIC STORAGE LEFT IS NOT ENOUGH,47,3,2)     
C/7S                                                                    
      IF (NSHORT .LE. 0) CALL SETERR(                                   
     1   'DCPOLY - THE DYNAMIC STORAGE LEFT IS NOT ENOUGH',47,3,2)      
C/                                                                      
C                                                                       
      NNN = 10*NN                                                       
      IPR  = ISTKGT(NNN,4)                                              
      IPI  = IPR  + NN                                                  
      IHR  = IPI  + NN                                                  
      IHI  = IHR  + NN                                                  
      IQPR = IHI  + NN                                                  
      IQPI = IQPR + NN                                                  
      IQHR = IQPI + NN                                                  
      IQHI = IQHR + NN                                                  
      ISHR = IQHI + NN                                                  
      ISHI = ISHR + NN                                                  
C                                                                       
C MAKE A COPY OF THE COEFFICIENTS.                                      
      DO 30 I = 1,NN                                                    
        IIPR  = IPR + I -1                                              
        IIPI  = IPI + I -1                                              
        IISHR = ISHR + I -1                                             
          D(IIPR)  = OPR(I)                                             
          D(IIPI)  = OPI(I)                                             
          D(IISHR) = CD1MOD(D(IIPR),D(IIPI))                            
   30 CONTINUE                                                          
C                                                                       
C SCALE THE POLYNOMIAL.                                                 
      BND = D8POLY (NN,D(ISHR),ETA,INFIN,SMALNO,BASE)                   
      IF (BND .EQ. 1.0D0) GO TO 40                                      
      DO 35 I = 1,NN                                                    
        IIPR = IPR + I - 1                                              
        IIPI = IPI + I - 1                                              
          D(IIPR) = BND*D(IIPR)                                         
          D(IIPI) = BND*D(IIPI)                                         
   35 CONTINUE                                                          
C                                                                       
C START THE ALGORITHM FOR ONE ZERO .                                    
C                                                                       
   40 IF (NN .GT. 2) GO TO 50                                           
      IF (NN .EQ. 1) GO TO 110                                          
C                                                                       
C CALCULATE THE FINAL ZERO AND RETURN.                                  
          CALL CD1DIV(-D(IPR+1),-D(IPI+1),D(IPR),D(IPI),                
     1    ZEROR(DEGREE),ZEROI(DEGREE))                                  
          CALL ISTKRL(1)                                                
          RETURN                                                        
C                                                                       
C CALCULATE BND, A LOWER BOUND ON THE MODULUS OF THE ZEROS.             
   50 DO 60 I = 1,NN                                                    
        IISHR = ISHR + I - 1                                            
        IIPR = IPR + I - 1                                              
        IIPI = IPI + I - 1                                              
          D(IISHR) = CD1MOD(D(IIPR),D(IIPI))                            
   60 CONTINUE                                                          
      BND = D2POLY(NN,D(ISHR),D(ISHI))                                  
C                                                                       
C OUTER LOOP TO CONTROL 2 MAJOR PASSES WITH DIFFERENT SEQUENCES         
C OF SHIFTS.                                                            
      DO 100 CNT1 = 1,2                                                 
C FIRST STAGE CALCULATION, NO SHIFT.                                    
          CALL D6POLY(5,D(IPR),D(IPI),D(IHR),D(IHI))                    
C INNER LOOP TO SELECT A SHIFT.                                         
          DO 90 CNT2 = 1,9                                              
C SHIFT IS CHOSEN WITH MODULUS BND AND AMPLITUDE ROTATED BY             
C 94 DEGREES FROM THE PREVIOUS SHIFT                                    
               XXX = COSR*XX-SINR*YY                                    
               YY = SINR*XX+COSR*YY                                     
               XX = XXX                                                 
               SR = BND*XX                                              
               SI = BND*YY                                              
C SECOND STAGE CALCULATION, FIXED SHIFT.                                
          CALL D4POLY(10*CNT2,ZR,ZI,CONV,D(IPR),D(IPI),D(IHR),D(IHI),   
     1         D(IQPR),D(IQPI),D(IQHR),D(IQHI),D(ISHR),D(ISHI))         
               IF (.NOT. CONV) GO TO 80                                 
C THE SECOND STAGE JUMPS DIRECTLY TO THE THIRD STAGE ITERATION.         
C IF SUCCESSFUL THE ZERO IS STORED AND THE POLYNOMIAL DEFLATED.         
                    IDNN2 = DEGREE-NN+2                                 
                    ZEROR(IDNN2) = ZR                                   
                    ZEROI(IDNN2) = ZI                                   
                    NN = NN-1                                           
                    DO 70 I = 1,NN                                      
                       IIPR  = IPR + I -1                               
                       IIPI  = IPI + I - 1                              
                       IIQPR = IQPR + I - 1                             
                       IIQPI = IQPI + I - 1                             
                         D(IIPR) = D(IIQPR)                             
                         D(IIPI) = D(IIQPI)                             
   70               CONTINUE                                            
                    GO TO 40                                            
   80          CONTINUE                                                 
C IF THE ITERATION IS UNSUCCESSFUL ANOTHER SHIFT IS CHOSEN.             
   90     CONTINUE                                                      
C IF 9 SHIFTS FAIL, THE OUTER LOOP IS REPEATED WITH ANOTHER             
C SEQUENCE OF SHIFTS.                                                   
  100 CONTINUE                                                          
C                                                                       
C THE ZEROFINDER HAS FAILED ON TWO MAJOR PASSES.                        
C RETURN EMPTY HANDED.                                                  
C                                                                       
      KP10 = IDNN2 + 10                                                 
C                                                                       
C/6S                                                                    
C     CALL SETERR(37HDCPOLY - ONLY K ZEROS HAVE BEEN FOUND,37,KP10,1)   
C/7S                                                                    
      CALL SETERR('DCPOLY - ONLY K ZEROS HAVE BEEN FOUND',37,KP10,1)    
C/                                                                      
  110 CALL ISTKRL(1)                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE D4POLY(L2,ZR,ZI,CONV,PR,PI,HR,HI,QPR,QPI,              
     1    QHR,QHI,SHR,SHI)                                              
C COMPUTES L2 FIXED-SHIFT H POLYNOMIALS AND TESTS FOR                   
C                                                                       
C CONVERGENCE.                                                          
C INITIATES A VARIABLE-SHIFT ITERATION AND RETURNS WITH THE             
C APPROXIMATE ZERO IF SUCCESSFUL.                                       
C L2 - LIMIT OF FIXED SHIFT STEPS                                       
C ZR,ZI - APPROXIMATE ZERO IF CONV IS .TRUE.                            
C CONV  - LOGICAL INDICATING CONVERGENCE OF STAGE 3 ITERATION           
C COMMON AREA                                                           
      COMMON/P99PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN            
C                                                                       
      DOUBLE PRECISION SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,           
     1    PR(1),PI(1),HR(1),HI(1),QPR(1),QPI(1),QHR(1),                 
     2    QHI(1),SHR(1),SHI(1)                                          
      DOUBLE PRECISION ZR,ZI,OTR,OTI,SVSR,SVSI,CD1MOD                   
          LOGICAL CONV,TEST,PASD,BOOL                                   
      N = NN-1                                                          
C EVALUATE P AT S.                                                      
      CALL D7POLY(NN,SR,SI,PR,PI,QPR,QPI,PVR,PVI)                       
      TEST = .TRUE.                                                     
      PASD = .FALSE.                                                    
C CALCULATE FIRST T = -P(S)/H(S).                                       
      CALL D1POLY(BOOL,HR,HI,QHR,QHI)                                   
C MAIN LOOP FOR ONE SECOND STAGE STEP.                                  
      DO 50 J = 1,L2                                                    
          OTR = TR                                                      
          OTI = TI                                                      
C COMPUTE NEXT H POLYNOMIAL AND NEW T.                                  
          CALL D5POLY(BOOL,HR,HI,QPR,QPI,QHR,QHI)                       
          CALL D1POLY(BOOL,HR,HI,QHR,QHI)                               
          ZR = SR+TR                                                    
          ZI = SI+TI                                                    
C TEST FOR CONVERGENCE UNLESS STAGE 3 HAS FAILED ONCE OR THIS           
C IS THE LAST H POLYNOMIAL .                                            
          IF ( BOOL .OR. .NOT. TEST .OR. J .EQ. L2) GO TO 50            
          IF (CD1MOD(TR-OTR,TI-OTI) .GE. .5D0*CD1MOD(ZR,ZI)) GO TO 40   
               IF (.NOT. PASD) GO TO 30                                 
C THE WEAK CONVERGENCE TEST HAS BEEN PASSED TWICE, START THE            
C THIRD STAGE ITERATION, AFTER SAVING THE CURRENT H POLYNOMIAL          
C AND SHIFT.                                                            
                    DO 10 I = 1,N                                       
                         SHR(I) = HR(I)                                 
                         SHI(I) = HI(I)                                 
   10               CONTINUE                                            
                    SVSR = SR                                           
                    SVSI = SI                                           
               CALL D9POLY(10,ZR,ZI,CONV,PR,PI,HR,HI,QPR,QPI,QHR,QHI)   
                    IF (CONV) RETURN                                    
C THE ITERATION FAILED TO CONVERGE. TURN OFF TESTING AND RESTORE        
C H,S,PV AND T.                                                         
                    TEST = .FALSE.                                      
                    DO 20 I = 1,N                                       
                         HR(I) = SHR(I)                                 
                         HI(I) = SHI(I)                                 
   20               CONTINUE                                            
                    SR = SVSR                                           
                    SI = SVSI                                           
                    CALL D7POLY(NN,SR,SI,PR,PI,QPR,QPI,PVR,PVI)         
                    CALL D1POLY(BOOL,HR,HI,QHR,QHI)                     
                    GO TO 50                                            
   30          PASD = .TRUE.                                            
               GO TO 50                                                 
   40     PASD = .FALSE.                                                
   50 CONTINUE                                                          
C ATTEMPT AN ITERATION WITH FINAL H POLYNOMIAL FROM SECOND STAGE.       
      CALL D9POLY(10,ZR,ZI,CONV,PR,PI,HR,HI,QPR,QPI,QHR,QHI)            
      RETURN                                                            
      END                                                               
      SUBROUTINE D9POLY(L3,ZR,ZI,CONV,PR,PI,HR,HI,QPR,QPI,QHR,QHI)      
C CARRIES OUT THE THIRD STAGE ITERATION.                                
C L3 - LIMIT OF STEPS IN STAGE 3.                                       
C ZR,ZI   - ON ENTRY CONTAINS THE INITIAL ITERATE, IF THE               
C ITERATION CONVERGES IT CONTAINS THE FINAL ITERATE                     
C ON EXIT.                                                              
C CONV    -  .TRUE. IF ITERATION CONVERGES                              
C COMMON AREA                                                           
      COMMON/P99PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN            
C                                                                       
      DOUBLE PRECISION SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,           
     1    PR(1),PI(1),HR(1),HI(1),QPR(1),QPI(1),QHR(1),QHI(1)           
      DOUBLE PRECISION ZR,ZI,MP,MS,OMP,RELSTP,R1,R2,CD1MOD,DSQRT        
      DOUBLE PRECISION D3POLY,TP                                        
      LOGICAL CONV,B,BOOL                                               
      CONV = .FALSE.                                                    
      B = .FALSE.                                                       
      SR = ZR                                                           
      SI = ZI                                                           
C MAIN LOOP FOR STAGE THREE                                             
      DO 60 I = 1,L3                                                    
C EVALUATE P AT S AND TEST FOR CONVERGENCE.                             
          CALL D7POLY(NN,SR,SI,PR,PI,QPR,QPI,PVR,PVI)                   
          MP = CD1MOD(PVR,PVI)                                          
          MS = CD1MOD(SR,SI)                                            
          IF (MP .GT. 20.0D0*D3POLY(NN,QPR,QPI,MS,MP,ARE,MRE))          
     1       GO TO 10                                                   
C POLYNOMIAL VALUE IS SMALLER IN VALUE THAN A BOUND ON THE ERROR        
C IN EVALUATING P, TERMINATE THE ITERATION.                             
               CONV = .TRUE.                                            
               ZR = SR                                                  
               ZI = SI                                                  
               RETURN                                                   
   10     IF (I .EQ. 1) GO TO 40                                        
               IF (B .OR. MP .LT.OMP .OR. RELSTP .GE. .05D0)            
     1            GO TO 30                                              
C ITERATION HAS STALLED. PROBABLY A CLUSTER OF ZEROS. DO 5 FIXED        
C SHIFT STEPS INTO THE CLUSTER TO FORCE ONE ZERO TO DOMINATE.           
                    TP = RELSTP                                         
                    B = .TRUE.                                          
                    IF (RELSTP .LT. ETA) TP = ETA                       
                    R1 = DSQRT(TP)                                      
                    R2 = SR*(1.0D0+R1)-SI*R1                            
                    SI = SR*R1+SI*(1.0D0+R1)                            
                    SR = R2                                             
                    CALL D7POLY(NN,SR,SI,PR,PI,QPR,QPI,PVR,PVI)         
                    DO 20 J = 1,5                                       
                         CALL D1POLY(BOOL,HR,HI,QHR,QHI)                
                         CALL D5POLY(BOOL,HR,HI,QPR,QPI,QHR,QHI)        
   20               CONTINUE                                            
      OMP = INFIN                                                       
                    GO TO 50                                            
C EXIT IF POLYNOMIAL VALUE INCREASES SIGNIFICANTLY.                     
   30          IF (MP*.1D0 .GT. OMP) RETURN                             
   40     OMP = MP                                                      
C CALCULATE NEXT ITERATE.                                               
   50     CALL D1POLY(BOOL,HR,HI,QHR,QHI)                               
          CALL D5POLY(BOOL,HR,HI,QPR,QPI,QHR,QHI)                       
          CALL D1POLY(BOOL,HR,HI,QHR,QHI)                               
          IF (BOOL) GO TO 60                                            
          RELSTP = CD1MOD(TR,TI)/CD1MOD(SR,SI)                          
          SR = SR+TR                                                    
          SI = SI+TI                                                    
   60 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION D3POLY(NN,QR,QI,MS,MP,ARE,MRE)          
C BOUNDS THE ERROR IN EVALUATING THE POLYNOMIAL BY THE HORNER           
C RECURRENCE.                                                           
C QR,QI - THE PARTIAL SUMS                                              
C MS    -MODULUS OF THE POINT                                           
C MP    -MODULUS OF POLYNOMIAL VALUE                                    
C ARE, MRE -ERROR BOUNDS ON COMPLEX ADDITION AND MULTIPLICATION         
      DOUBLE PRECISION QR(1),QI(1),MS,MP,ARE,MRE,E,CD1MOD               
      E = CD1MOD(QR(1),QI(1))*MRE/(ARE+MRE)                             
      DO 10 I = 1,NN                                                    
          E = E*MS+CD1MOD(QR(I),QI(I))                                  
   10 CONTINUE                                                          
      D3POLY = E*(ARE+MRE)-MP*MRE                                       
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION D2POLY(NN,PT,Q)                         
C D2POLY COMPUTES A LOWER BOUND ON THE MODULI OF THE ZEROS OF A         
C POLYNOMIAL - PT IS THE MODULUS OF THE COEFFICIENTS.                   
      DOUBLE PRECISION Q(1),PT(1),X,XM,F,DX,DF,                         
     1   DEXP,DLOG                                                      
      PT(NN) = -PT(NN)                                                  
C COMPUTE UPPER ESTIMATE OF BOUND.                                      
      N = NN-1                                                          
      X = DEXP( (DLOG(-PT(NN)) - DLOG(PT(1)))/FLOAT(N) )                
      IF (PT(N).EQ.0.0D0) GO TO 20                                      
C IF NEWTON STEP AT THE ORIGIN IS BETTER, USE IT.                       
          XM = -PT(NN)/PT(N)                                            
          IF (XM.LT.X) X=XM                                             
C CHOP THE INTERVAL (0,X) UNTIL F LE 0.                                 
   20 XM = X*.1D0                                                       
      F = PT(1)                                                         
      DO 30 I = 2,NN                                                    
          F = F*XM+PT(I)                                                
   30 CONTINUE                                                          
      IF (F.LE. 0.0D0) GO TO 40                                         
          X = XM                                                        
          GO TO 20                                                      
   40 DX = X                                                            
C DO NEWTON ITERATION UNTIL X CONVERGES TO TWO DECIMAL PLACES.          
   50 IF (DABS(DX/X) .LE. .005D0) GO TO 70                              
          Q(1) = PT(1)                                                  
          DO 60 I = 2,NN                                                
               Q(I) = Q(I-1)*X+PT(I)                                    
   60     CONTINUE                                                      
          F = Q(NN)                                                     
          DF = Q(1)                                                     
          DO 65 I = 2,N                                                 
               DF = DF*X+Q(I)                                           
   65     CONTINUE                                                      
          DX = F/DF                                                     
          X = X-DX                                                      
          GO TO 50                                                      
   70 D2POLY = X                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE D1POLY(BOOL,HR,HI,QHR,QHI)                             
C COMPUTES  T = -P(S)/H(S).                                             
C BOOL   - LOGICAL, SET TRUE IF H(S) IS ESSENTIALLY ZERO.               
C COMMON AREA                                                           
      COMMON/P99PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN            
C                                                                       
      DOUBLE PRECISION SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN            
      DOUBLE PRECISION HR(1),HI(1),QHR(1),QHI(1)                        
      DOUBLE PRECISION HVR,HVI,CD1MOD                                   
      LOGICAL BOOL                                                      
      N = NN-1                                                          
C EVALUATE H(S).                                                        
      CALL D7POLY(N,SR,SI,HR,HI,QHR,QHI,HVR,HVI)                        
      BOOL = CD1MOD(HVR,HVI) .LE. ARE*10.0D0*CD1MOD(HR(N),HI(N))        
      IF (BOOL) GO TO 10                                                
          CALL CD1DIV(-PVR,-PVI,HVR,HVI,TR,TI)                          
          RETURN                                                        
   10 TR = 0.0D0                                                        
      TI = 0.0D0                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE D5POLY(BOOL,HR,HI,QPR,QPI,QHR,QHI)                     
C CALCULATES THE NEXT SHIFTED H POLYNOMIAL.                             
C BOOL   -  LOGICAL, IF .TRUE. H(S) IS ESSENTIALLY ZERO                 
C COMMON AREA                                                           
      COMMON/P99PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN            
C                                                                       
      DOUBLE PRECISION SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,           
     1    HR(1),HI(1),QPR(1),QPI(1),QHR(1),QHI(1)                       
      DOUBLE PRECISION T1,T2                                            
      LOGICAL BOOL                                                      
      N = NN-1                                                          
      NM1 = N-1                                                         
      IF (BOOL) GO TO 20                                                
          DO 10 J = 2,N                                                 
               T1 = QHR(J-1)                                            
               T2 = QHI(J-1)                                            
               HR(J) = TR*T1-TI*T2+QPR(J)                               
               HI(J) = TR*T2+TI*T1+QPI(J)                               
   10     CONTINUE                                                      
          HR(1) = QPR(1)                                                
          HI(1) = QPI(1)                                                
          RETURN                                                        
C IF H(S) IS ZERO REPLACE H WITH QH.                                    
   20 DO 30 J = 2,N                                                     
          HR(J) = QHR(J-1)                                              
          HI(J) = QHI(J-1)                                              
   30 CONTINUE                                                          
      HR(1) = 0.0D0                                                     
      HI(1) = 0.0D0                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE D6POLY(L1,PR,PI,HR,HI)                                 
C COMPUTES  THE DERIVATIVE  POLYNOMIAL AS THE INITIAL H                 
C POLYNOMIAL AND COMPUTES L1 NO-SHIFT H POLYNOMIALS.                    
C COMMON AREA                                                           
      COMMON/P99PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN            
C                                                                       
      DOUBLE PRECISION SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,           
     1    PR(1),PI(1),HR(1),HI(1)                                       
      DOUBLE PRECISION XNI,T1,T2,CD1MOD                                 
      N = NN-1                                                          
      NM1 = N-1                                                         
      DO 10 I = 1,N                                                     
          XNI = NN-I                                                    
          HR(I) = XNI*PR(I)/FLOAT(N)                                    
          HI(I) = XNI*PI(I)/FLOAT(N)                                    
   10 CONTINUE                                                          
      DO 50 JJ = 1,L1                                                   
          IF (CD1MOD(HR(N),HI(N)) .LE. ETA*10.0D0*CD1MOD(PR(N),PI(N)))  
     *    GO TO 30                                                      
          CALL CD1DIV(-PR(NN),-PI(NN),HR(N),HI(N),TR,TI)                
          DO 20 I = 1,NM1                                               
               J = NN-I                                                 
               T1 = HR(J-1)                                             
               T2 = HI(J-1)                                             
               HR(J) = TR*T1-TI*T2+PR(J)                                
               HI(J) = TR*T2+TI*T1+PI(J)                                
   20     CONTINUE                                                      
          HR(1) = PR(1)                                                 
          HI(1) = PI(1)                                                 
          GO TO 50                                                      
C IF THE CONSTANT TERM IS ESSENTIALLY ZERO, SHIFT H COEFFICIENTS.       
   30     DO 40 I = 1,NM1                                               
               J = NN-I                                                 
               HR(J) = HR(J-1)                                          
               HI(J) = HI(J-1)                                          
   40     CONTINUE                                                      
          HR(1) = 0.0D0                                                 
          HI(1) = 0.0D0                                                 
   50 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE D7POLY(NN,SR,SI,PR,PI,QR,QI,PVR,PVI)                   
C EVALUATES A POLYNOMIAL  P  AT  S  BY THE HORNER RECURRENCE            
C PLACING THE PARTIAL SUMS IN Q AND THE COMPUTED VALUE IN PV.           
      DOUBLE PRECISION PR(1),PI(1),QR(1),QI(1),                         
     1    SR,SI,PVR,PVI,T                                               
      QR(1) = PR(1)                                                     
      QI(1) = PI(1)                                                     
      PVR = QR(1)                                                       
      PVI = QI(1)                                                       
      DO 10 I = 2,NN                                                    
          T = PVR*SR-PVI*SI+PR(I)                                       
          PVI = PVR*SI+PVI*SR+PI(I)                                     
          PVR = T                                                       
          QR(I) = PVR                                                   
          QI(I) = PVI                                                   
   10 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION D8POLY(NN,PT,ETA,INFIN,SMALNO,BASE)     
C RETURNS A SCALE FACTOR TO MULTIPLY THE COEFFICIENTS OF THE            
C POLYNOMIAL. THE SCALING IS DONE TO AVOID OVERFLOW AND TO AVOID        
C UNDETECTED UNDERFLOW INTERFERING WITH THE CONVERGENCE                 
C CRITERION.  THE FACTOR IS A POWER OF THE BASE.                        
C PT - MODULUS OF COEFFICIENTS OF P                                     
C ETA,INFIN,SMALNO,BASE - CONSTANTS DESCRIBING THE                      
C FLOATING POINT ARITHMETIC.                                            
      DOUBLE PRECISION PT(1),ETA,INFIN,SMALNO,BASE,HI,LO,               
     1    MAX,MIN,X,SC,DSQRT,DLOG                                       
C FIND LARGEST AND SMALLEST MODULI OF COEFFICIENTS.                     
      HI = DSQRT(INFIN)                                                 
      LO = SMALNO/ETA                                                   
      MAX = 0.0D0                                                       
      MIN = INFIN                                                       
      DO 10 I = 1,NN                                                    
          X = PT(I)                                                     
          IF (X .GT. MAX) MAX = X                                       
          IF (X .NE. 0.0D0 .AND. X.LT.MIN) MIN = X                      
   10 CONTINUE                                                          
C SCALE ONLY IF THERE ARE VERY LARGE OR VERY SMALL COMPONENTS.          
      D8POLY = 1.0D0                                                    
      IF (MIN .GE. LO .AND. MAX .LE. HI) RETURN                         
      X = LO/MIN                                                        
      IF (X .GT. 1.0D0) GO TO 20                                        
          SC = 1.0D0/(DSQRT(MAX)*DSQRT(MIN))                            
          GO TO 30                                                      
   20 SC = X                                                            
      IF (INFIN/SC .GT. MAX) SC = 1.0D0                                 
   30 L = DLOG(SC)/DLOG(BASE) + .500                                    
      D8POLY = BASE**L                                                  
      RETURN                                                            
      END                                                               
      COMPLEX FUNCTION MULLR (F, Z1, Z2, Z3, EPSZ, EPSF, MAXITR, ITER)  
C                                                                       
C  THIS SUBROUTINE IS A MODULE WHICH USES MULLERS METHOD                
C  TO ITERATE TOWARD A ZERO OF THE REAL FUNCTION COMPUTED               
C  BY THE USER-WRITTEN FUNCTION, F.                                     
C                                                                       
C  WRITTEN AUGUST 1979 BY PHYLLIS FOX.                                  
C                                                                       
C  THE THREE STARTING POINTS ARE Z1 AND Z2 AND Z3.                      
C  THESE MUST BE THREE DIFFERENT VALUES.                                
C                                                                       
C  IF THE USER ALREADY KNOWS SOME ZEROS OF F, SAY ZA AND ZB,            
C  IT MIGHT BE BEST FOR THE FUNCTION ROUTINE TO COMPUTE                 
C  THE DEFLATED VALUE,                                                  
C                                                                       
C        G(Z) = F(Z)/((Z-ZA)(Z-ZB))                                     
C                                                                       
C  (CARE MUST BE TAKEN HERE TO BE SURE THAT Z IS NOT NEAR               
C  TO ZA OR TO ZB.)                                                     
C                                                                       
C  INPUT PARAMETERS -                                                   
C                                                                       
C  F        - A COMPLEX FUNCTION SUBPROGRAM WRITTEN BY THE USER,        
C             WHICH, GIVEN A COMPLEX VALUE, ZNOW, RETURNS               
C             THE VALUE OF THE FUNCTION, F(ZNOW).                       
C             NOTE THAT COMPLEX ARITHMETIC MUST BE USED EVEN IF         
C             THE FUNCTION IS REAL FOR REAL VALUES OF ITS ARGUMENT.     
C             SEE ALSO THE REMARK ABOVE ON EVALUATING THE FUNCTION.     
C                                                                       
C             THE FUNCTION NAME SHOULD BE DECLARED EXTERNAL AND         
C             COMPLEX IN THE CALLING PROGRAM.                           
C                                                                       
C  Z1,Z2,Z3 - THREE GUESSES (COMPLEX-VALUED) AT THE LOCATION IN THE     
C             COMPLEX PLANE OF THE DESIRED ZERO.                        
C                                                                       
C  EPSZ    -  THE PROCESS IS ASSUMED TO CONVERGE WHEN                   
C             THE TWO MOST RECENT ITERATES, ZNOW AND ZLAST,             
C             ARE SUCH THAT ABS(ZNOW-ZLAST) IS LESS THAN EPSZ           
C             AND IT IS ALSO TRUE THAT THE VALUE OF THE                 
C             FUNCTION, F(ZNOW) IS LESS (IN ABSOLUTE VALUE)             
C             THAN EPSF.                                                
C             BOTH CRITERIA MUST HOLD.                                  
C                                                                       
C  EPSF    -  CONVERGENCE CRITERION ON F - SEE EPSZ ABOVE.              
C                                                                       
C  MAXITR  -  THE MAXIMUM NUMBER OF ITERATIONS -                        
C             THE PROCESS FAILS (RECOVERABLE ERROR)                     
C             IF THE ABOVE ERROR CRITERIA ARE NOT MET WITHIN            
C             MAXITR NUMBER OF ITERATIONS.                              
C                                                                       
C                                                                       
C  OUTPUT PARAMETERS -                                                  
C                                                                       
C  ITER    -  ACTUAL NUMBER OF ITERATIONS USED                          
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C  1 - MAXITR .LT. 1                                                    
C                                                                       
C  2 - EPSZ OR EPSF .LT. 0.                                             
C                                                                       
C  3 - THE NUMBER OF ITERATIONS EXCEEDED MAXITR                         
C      WITHOUT CONVERGENCE TO A ZERO.  (RECOVERABLE ERROR)              
C                                                                       
C  4 - DIVERGENCE -                                                     
C        EITHER THE COMPUTED CORRECTION TO Z EXCEEDS                    
C        100 TIMES THE PREVIOUS CORRECTION, OR                          
C        ONE OF THE DIVIDED DIFFERENCES WOULD OVERFLOW.                 
C        THIS ERROR WILL ALSO OCCUR IF TWO INPUT ZS ARE                 
C        THE SAME.                                                      
C        (RECOVERABLE ERROR)                                            
C                                                                       
C  5 - THE PROCESS IS STUCK - THE COMPUTED CORRECTION TO Z IS LESS      
C      THAN 10 ROUNDING ERRORS TIMES CABS(ZNEW), BUT CABS(F(ZNEW))      
C      IS NOT LESS THAN EPSF.  (RECOVERABLE ERROR)                      
C                                                                       
      INTEGER ITER, MAXITR                                              
      REAL EPSF, EPSZ, SQRTMX, R1MACH, RNDERR                           
C                                                                       
      COMPLEX B, CSTEP, DENOM, F, F1, F2, F3, F21, F32, F321,           
     1  FNEW, RADICL, USEZ21, USEZ32, Z1, Z2, Z3, Z32, ZNEW             
      EXTERNAL F                                                        
      COMPLEX CSQRT                                                     
C                                                                       
C  CHECK THE INPUT PARAMETERS.                                          
C                                                                       
C/6S                                                                    
C     IF (MAXITR .LT. 1) CALL                                           
C    1  SETERR(23H  MULLR - MAXITR .LT. 1, 23, 1, 2)                    
C/7S                                                                    
      IF (MAXITR .LT. 1) CALL                                           
     1  SETERR('  MULLR - MAXITR .LT. 1', 23, 1, 2)                     
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (EPSZ .LT. 0. .OR. EPSF .LT. 0.) CALL                          
C    1  SETERR(32H  MULLR - EPSZ OR EPSF .LT. ZERO, 32, 2, 2)           
C/7S                                                                    
      IF (EPSZ .LT. 0. .OR. EPSF .LT. 0.) CALL                          
     1  SETERR('  MULLR - EPSZ OR EPSF .LT. ZERO', 32, 2, 2)            
C/                                                                      
C                                                                       
C                                                                       
C  SET UP THE INITIAL VALUES.                                           
C                                                                       
      ITER = 0                                                          
C                                                                       
      RNDERR = R1MACH(4)                                                
      SQRTMX = SQRT(R1MACH(2))                                          
C                                                                       
      MULLR = Z3                                                        
      F1 = F(Z1)                                                        
      F2 = F(Z2)                                                        
      F3 = F(Z3)                                                        
C                                                                       
C  CHECK FOR POSSIBLE OVERFLOW.                                         
C                                                                       
       USEZ21 = Z2 - Z1                                                 
      IF (CABS(F2-F1) .LT. CABS(USEZ21)*SQRTMX) GO TO 10                
       USEZ21 = 10.0E0 * USEZ21                                         
      IF (CABS(F2-F1) .LT. CABS(USEZ21)*SQRTMX) GO TO 10                
      GO TO 40                                                          
C                                                                       
 10   F21 = (F2 - F1)/USEZ21                                            
C                                                                       
C                                                                       
C  SET UP THE QUADRATIC AND FIND ITS ROOTS.                             
C                                                                       
C  CHECK FOR POSSIBLE OVERFLOW.                                         
C                                                                       
 20    Z32 = Z3 - Z2                                                    
        USEZ32 = Z32                                                    
      IF (CABS(F3-F2) .LT. CABS(USEZ32)*SQRTMX) GO TO 30                
       USEZ32 = 10.0E0 * USEZ32                                         
      IF (CABS(F3-F2) .LT. CABS(USEZ32)*SQRTMX) GO TO 30                
      GO TO 40                                                          
C                                                                       
 30   F32 = (F3 - F2)/USEZ32                                            
C                                                                       
C  IF F321 IS GOING TO OVERFLOW, JUST BLATANTLY SET                     
C  IT TO ZERO.                                                          
C  (IT IS IN THE NATURE OF A CORRECTION TERM ANYHOW.)                   
C                                                                       
      F321 = CMPLX(0.0E0,0.E0)                                          
C                                                                       
      IF (CABS(F32-F21) .LT. CABS(Z3-Z1)*SQRTMX)                        
     1    F321 = (F32 - F21)/(Z3-Z1)                                    
C                                                                       
      B = F32 + Z32*F321                                                
      RADICL = B*B - 4.E0*F321*F3                                       
      RADICL = CSQRT(RADICL)                                            
C                                                                       
C  TAKE THE DENOMINATOR OF LARGER MAGNITUDE.                            
C                                                                       
      IF (REAL(B)*REAL(RADICL) + AIMAG(B)*AIMAG(RADICL)                 
     1   .LT. 0.0) RADICL = -RADICL                                     
      DENOM = B + RADICL                                                
C                                                                       
C  CHECK THAT THE NEW CORRECTION, CSTEP=2F3/DENOM,                      
C  IS LESS THAN 100 TIMES THE PREVIOUS, Z32.                            
C  OTHERWISE SIGNAL ERROR                                               
C  (EXCEPT SKIP THIS TEST AT THE FIRST ITERATION.)                      
      IF (ITER .EQ. 0) GO TO 50                                         
C                                                                       
      IF (CABS(DENOM) .GT. CABS(F3)/(50.E0*CABS(Z32))) GO TO 50         
C                                                                       
C  GETS HERE IF PROCESS SEEMS TO BE DIVERGING -                         
C  SEE ERROR 4 IN ERROR STATES ABOVE.                                   
C                                                                       
C/6S                                                                    
C40   CALL SETERR (32H  MULLR - PROCESS NOT CONVERGING, 32, 4,1)        
C/7S                                                                    
 40   CALL SETERR ('  MULLR - PROCESS NOT CONVERGING', 32, 4,1)         
C/                                                                      
      RETURN                                                            
C                                                                       
C  FIND THE CORRECTION, CSTEP TO Z3.                                    
C                                                                       
 50   CSTEP = 2.E0 * F3/DENOM                                           
      ZNEW = Z3 - CSTEP                                                 
 60   MULLR = ZNEW                                                      
      FNEW = F(ZNEW)                                                    
C                                                                       
C  COUNT ITERATIONS                                                     
C                                                                       
      ITER = ITER + 1                                                   
C                                                                       
C  TEST FOR NUMBER OF ITERATIONS, CONVERGENCE, OR DIVERGENCE.           
C                                                                       
      IF (ITER .LT. MAXITR) GO TO 70                                    
C/6S                                                                    
C     CALL SETERR (46H  MULLR - NUMBER OF ITERATIONS EXCEEDED MAXITR,   
C    1  46, 3, 1)                                                       
C/7S                                                                    
      CALL SETERR ('  MULLR - NUMBER OF ITERATIONS EXCEEDED MAXITR',    
     1  46, 3, 1)                                                       
C/                                                                      
      RETURN                                                            
 70   IF (CABS(FNEW) .LE. 100.E0*CABS(F3)) GO TO 80                     
C                                                                       
C  SEEMS TO BE DIVERGING HERE - HALVE THE STEP                          
C                                                                       
      CSTEP = .5E0*CSTEP                                                
      ZNEW = ZNEW + CSTEP                                               
      GO TO 60                                                          
C                                                                       
C  HERE ALL LOOKS HOPEFUL.                                              
C                                                                       
 80   IF (CABS(FNEW) .LT. EPSF .AND. CABS(ZNEW-Z3) .LT. EPSZ) RETURN    
      IF (CABS(CSTEP) .GT. 10.0E0*RNDERR*CABS(Z3)) GO TO 90             
C/6S                                                                    
C     CALL SETERR(41H  MULLR - THE TRIAL FOR THE ZERO IS STUCK,41,5,1)  
C/7S                                                                    
      CALL SETERR('  MULLR - THE TRIAL FOR THE ZERO IS STUCK',41,5,1)   
C/                                                                      
      RETURN                                                            
 90   Z1 = Z2                                                           
      Z2 = Z3                                                           
      Z3 = ZNEW                                                         
      F1 = F2                                                           
      F2 = F3                                                           
      F3 = FNEW                                                         
      F21 = F32                                                         
      GO TO 20                                                          
C                                                                       
      END                                                               
      REAL FUNCTION ZERO(F,A,B,T)                                       
C                                                                       
C  FINDS THE REAL ROOT OF THE FUNCTION F LYING BETWEEN A AND B          
C  TO WITHIN A TOLERANCE OF                                             
C                                                                       
C         6*R1MACH(3) * ABS(ZERO) + 2 * T                               
C                                                                       
C  F(A) AND F(B) MUST HAVE OPPOSITE SIGNS                               
C                                                                       
C  THIS IS BRENTS ALGORITHM                                             
C                                                                       
C  A, STORED IN SA, IS THE PREVIOUS BEST APPROXIMATION (I.E. THE OLD B) 
C  B, STORED IN SB, IS THE CURRENT BEST APPROXIMATION                   
C  C IS THE MOST RECENTLY COMPUTED POINT SATISFYING F(B)*F(C) .LT. 0    
C  D CONTAINS THE CORRECTION TO THE APPROXIMATION                       
C  E CONTAINS THE PREVIOUS VALUE OF D                                   
C  M CONTAINS THE BISECTION QUANTITY (C-B)/2                            
C                                                                       
      REAL A,B,T,TT,SA,SB,C,D,E,FA,FB,FC,TOL,M,P,Q,R,S                  
      EXTERNAL F                                                        
C                                                                       
      TT = T                                                            
      IF (T .LE. 0.0) TT = 10.*R1MACH(1)                                
C                                                                       
      SA = A                                                            
      SB = B                                                            
      FA = F(SA)                                                        
      FB = F(SB)                                                        
      IF (FA .NE. 0.0) GO TO 5                                          
      ZERO = SA                                                         
      RETURN                                                            
  5   IF (FB .EQ. 0.0) GO TO 140                                        
C/6S                                                                    
C     IF (SIGN(FA,FB) .EQ. FA) CALL SETERR(                             
C    1   46H ZERO - F(A) AND F(B) ARE NOT OF OPPOSITE SIGN, 46, 1, 1)   
C/7S                                                                    
      IF (SIGN(FA,FB) .EQ. FA) CALL SETERR(                             
     1   ' ZERO - F(A) AND F(B) ARE NOT OF OPPOSITE SIGN', 46, 1, 1)    
C/                                                                      
C                                                                       
 10   C  = SA                                                           
      FC = FA                                                           
      E  = SB-SA                                                        
      D  = E                                                            
C                                                                       
C  INTERCHANGE B AND C IF ABS F(C) .LT. ABS F(B)                        
C                                                                       
 20   IF (ABS(FC).GE.ABS(FB)) GO TO 30                                  
      SA = SB                                                           
      SB = C                                                            
      C  = SA                                                           
      FA = FB                                                           
      FB = FC                                                           
      FC = FA                                                           
C                                                                       
 30   TOL = 2.0*R1MACH(4)*ABS(SB)+TT                                    
      M = 0.5*(C-SB)                                                    
C                                                                       
C  SUCCESS INDICATED BY M REDUCES TO UNDER TOLERANCE OR                 
C  BY F(B) = 0                                                          
C                                                                       
      IF ((ABS(M).LE.TOL).OR.(FB.EQ.0.0)) GO TO 140                     
C                                                                       
C  A BISECTION IS FORCED IF E, THE NEXT-TO-LAST CORRECTION              
C  WAS LESS THAN THE TOLERANCE OR IF THE PREVIOUS B GAVE                
C  A SMALLER F(B).  OTHERWISE GO TO 40.                                 
C                                                                       
      IF ((ABS(E).GE.TOL).AND.(ABS(FA).GE.ABS(FB))) GO TO 40            
      E = M                                                             
      D = E                                                             
      GO TO 100                                                         
 40   S = FB/FA                                                         
C                                                                       
C  QUADRATIC INTERPOLATION CAN ONLY BE DONE IF A (IN SA)                
C  AND C ARE DIFFERENT POINTS.                                          
C  OTHERWISE DO THE FOLLOWING LINEAR INTERPOLATION                      
C                                                                       
      IF (SA.NE.C) GO TO 50                                             
      P = 2.0*M*S                                                       
      Q = 1.0-S                                                         
      GO TO 60                                                          
C                                                                       
C  INVERSE QUADRATIC INTERPOLATION                                      
C                                                                       
 50   Q = FA/FC                                                         
      R = FB/FC                                                         
      P = S*(2.0*M*Q*(Q-R)-(SB-SA)*(R-1.0))                             
      Q = (Q-1.0)*(R-1.0)*(S-1.0)                                       
 60   IF (P.LE.0.0) GO TO 70                                            
      Q = -Q                                                            
      GO TO 80                                                          
 70   P = -P                                                            
C                                                                       
C  UPDATE THE QUANTITIES USING THE NEWLY COMPUTED                       
C  INTERPOLATE UNLESS IT WOULD EITHER FORCE THE                         
C  NEW POINT TOO FAR TO ONE SIDE OF THE INTERVAL                        
C  OR WOULD REPRESENT A CORRECTION GREATER THAN                         
C  HALF THE PREVIOUS CORRECTION.                                        
C                                                                       
C  IN THESE LAST TWO CASES - DO THE BISECTION                           
C  BELOW (FROM STATEMENT 90 TO 100)                                     
C                                                                       
 80   S = E                                                             
      E = D                                                             
      IF ((2.0*P.GE.3.0*M*Q-ABS(TOL*Q)).OR.                             
     1    (P.GE.ABS(0.5*S*Q))) GO TO 90                                 
      D = P/Q                                                           
      GO TO 100                                                         
 90   E = M                                                             
      D = E                                                             
C                                                                       
C  SET A TO THE PREVIOUS B                                              
C                                                                       
 100  SA = SB                                                           
      FA = FB                                                           
C                                                                       
C  IF THE CORRECTION TO BE MADE IS SMALLER THAN                         
C  THE TOLERANCE, JUST TAKE A  DELTA STEP  (DELTA=TOLERANCE)            
C         B = B + DELTA * SIGN(M)                                       
C                                                                       
      IF (ABS(D).LE.TOL) GO TO 110                                      
      SB = SB+D                                                         
      GO TO 130                                                         
C                                                                       
 110  IF (M.LE.0.0) GO TO 120                                           
      SB = SB+TOL                                                       
      GO TO 130                                                         
C                                                                       
 120  SB = SB-TOL                                                       
 130  FB = F(SB)                                                        
C                                                                       
C  IF F(B) AND F(C) HAVE THE SAME SIGN ONLY                             
C  LINEAR INTERPOLATION (NOT INVERSE QUADRATIC)                         
C  CAN BE DONE                                                          
C                                                                       
      IF ((FB.GT.0.0).AND.(FC.GT.0.0)) GO TO 10                         
      IF ((FB.LE.0.0).AND.(FC.LE.0.0)) GO TO 10                         
      GO TO 20                                                          
C                                                                       
C***SUCCESS***                                                          
 140  ZERO = SB                                                         
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DZERO(F,A,B,T)                          
C                                                                       
C  FINDS THE REAL ROOT OF THE FUNCTION F LYING BETWEEN A AND B          
C  TO WITHIN A TOLERANCE OF                                             
C                                                                       
C         6*D1MACH(3) * DABS(DZERO) + 2 * T                             
C                                                                       
C  F(A) AND F(B) MUST HAVE OPPOSITE SIGNS                               
C                                                                       
C  THIS IS BRENTS ALGORITHM                                             
C                                                                       
C  A, STORED IN SA, IS THE PREVIOUS BEST APPROXIMATION (I.E. THE OLD B) 
C  B, STORED IN SB, IS THE CURRENT BEST APPROXIMATION                   
C  C IS THE MOST RECENTLY COMPUTED POINT SATISFYING F(B)*F(C) .LT. 0    
C  D CONTAINS THE CORRECTION TO THE APPROXIMATION                       
C  E CONTAINS THE PREVIOUS VALUE OF D                                   
C  M CONTAINS THE BISECTION QUANTITY (C-B)/2                            
C                                                                       
      DOUBLE PRECISION F,A,B,T,TT,SA,SB,C,D,E,FA,FB,FC,TOL,M,P,Q,R,S    
      EXTERNAL F                                                        
      DOUBLE PRECISION D1MACH                                           
C                                                                       
      TT = T                                                            
      IF (T .LE. 0.0D0) TT = 10.D0*D1MACH(1)                            
C                                                                       
      SA = A                                                            
      SB = B                                                            
      FA = F(SA)                                                        
      FB = F(SB)                                                        
      IF (FA .NE. 0.0D0) GO TO 5                                        
      DZERO = SA                                                        
      RETURN                                                            
  5   IF (FB .EQ. 0.0D0) GO TO 140                                      
C/6S                                                                    
C     IF (DSIGN(FA,FB) .EQ. FA) CALL SETERR(                            
C    1   47H DZERO - F(A) AND F(B) ARE NOT OF OPPOSITE SIGN, 47, 1, 1)  
C/7S                                                                    
      IF (DSIGN(FA,FB) .EQ. FA) CALL SETERR(                            
     1   ' DZERO - F(A) AND F(B) ARE NOT OF OPPOSITE SIGN', 47, 1, 1)   
C/                                                                      
C                                                                       
 10   C  = SA                                                           
      FC = FA                                                           
      E  = SB-SA                                                        
      D  = E                                                            
C                                                                       
C  INTERCHANGE B AND C IF DABS F(C) .LT. DABS F(B)                      
C                                                                       
 20   IF (DABS(FC).GE.DABS(FB)) GO TO 30                                
      SA = SB                                                           
      SB = C                                                            
      C  = SA                                                           
      FA = FB                                                           
      FB = FC                                                           
      FC = FA                                                           
C                                                                       
 30   TOL = 2.0D0*D1MACH(4)*DABS(SB)+TT                                 
      M = 0.5D0*(C-SB)                                                  
C                                                                       
C  SUCCESS INDICATED BY M REDUCES TO UNDER TOLERANCE OR                 
C  BY F(B) = 0                                                          
C                                                                       
      IF ((DABS(M).LE.TOL).OR.(FB.EQ.0.0D0)) GO TO 140                  
C                                                                       
C  A BISECTION IS FORCED IF E, THE NEXT-TO-LAST CORRECTION              
C  WAS LESS THAN THE TOLERANCE OR IF THE PREVIOUS B GAVE                
C  A SMALLER F(B).  OTHERWISE GO TO 40.                                 
C                                                                       
      IF ((DABS(E).GE.TOL).AND.(DABS(FA).GE.DABS(FB))) GO TO 40         
      E = M                                                             
      D = E                                                             
      GO TO 100                                                         
 40   S = FB/FA                                                         
C                                                                       
C  QUADRATIC INTERPOLATION CAN ONLY BE DONE IF A (IN SA)                
C  AND C ARE DIFFERENT POINTS.                                          
C  OTHERWISE DO THE FOLLOWING LINEAR INTERPOLATION                      
C                                                                       
      IF (SA.NE.C) GO TO 50                                             
      P = 2.0D0*M*S                                                     
      Q = 1.0D0-S                                                       
      GO TO 60                                                          
C                                                                       
C  INVERSE QUADRATIC INTERPOLATION                                      
C                                                                       
 50   Q = FA/FC                                                         
      R = FB/FC                                                         
      P = S*(2.0D0*M*Q*(Q-R)-(SB-SA)*(R-1.0D0))                         
      Q = (Q-1.0D0)*(R-1.0D0)*(S-1.0D0)                                 
 60   IF (P.LE.0.0D0) GO TO 70                                          
      Q = -Q                                                            
      GO TO 80                                                          
 70   P = -P                                                            
C                                                                       
C  UPDATE THE QUANTITIES USING THE NEWLY COMPUTED                       
C  INTERPOLATE UNLESS IT WOULD EITHER FORCE THE                         
C  NEW POINT TOO FAR TO ONE SIDE OF THE INTERVAL                        
C  OR WOULD REPRESENT A CORRECTION GREATER THAN                         
C  HALF THE PREVIOUS CORRECTION.                                        
C                                                                       
C  IN THESE LAST TWO CASES - DO THE BISECTION                           
C  BELOW (FROM STATEMENT 90 TO 100)                                     
C                                                                       
 80   S = E                                                             
      E = D                                                             
      IF ((2.0D0*P.GE.3.0D0*M*Q-DABS(TOL*Q)).OR.                        
     1    (P.GE.DABS(0.5D0*S*Q))) GO TO 90                              
      D = P/Q                                                           
      GO TO 100                                                         
 90   E = M                                                             
      D = E                                                             
C                                                                       
C  SET A TO THE PREVIOUS B                                              
C                                                                       
 100  SA = SB                                                           
      FA = FB                                                           
C                                                                       
C  IF THE CORRECTION TO BE MADE IS SMALLER THAN                         
C  THE TOLERANCE, JUST TAKE A  DELTA STEP  (DELTA=TOLERANCE)            
C         B = B + DELTA * SIGN(M)                                       
C                                                                       
      IF (DABS(D).LE.TOL) GO TO 110                                     
      SB = SB+D                                                         
      GO TO 130                                                         
C                                                                       
 110  IF (M.LE.0.0D0) GO TO 120                                         
      SB = SB+TOL                                                       
      GO TO 130                                                         
C                                                                       
 120  SB = SB-TOL                                                       
 130  FB = F(SB)                                                        
C                                                                       
C  IF F(B) AND F(C) HAVE THE SAME SIGN ONLY                             
C  LINEAR INTERPOLATION (NOT INVERSE QUADRATIC)                         
C  CAN BE DONE                                                          
C                                                                       
      IF ((FB.GT.0.0D0).AND.(FC.GT.0.0D0)) GO TO 10                     
      IF ((FB.LE.0.0D0).AND.(FC.LE.0.0D0)) GO TO 10                     
      GO TO 20                                                          
C                                                                       
C***SUCCESS***                                                          
 140  DZERO = SB                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE ZONE(FUNC, N, X, EPS, JMAX, F2NORM)                    
      INTEGER N                                                         
      EXTERNAL FUNC                                                     
      INTEGER JMAX                                                      
      REAL X(N), EPS, F2NORM                                            
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      EXTERNAL Z1JAC                                                    
      INTEGER IFF, ISTKGT, IDX, IDF, IPP, IGG                           
      INTEGER IAUX, IRMAT, IQMAT, IPRINT, KFLAG                         
      REAL R(1000)                                                      
      EQUIVALENCE (D(1), R(1))                                          
C SOLVE N NONLINEAR EQUATIONS F(X)=0                                    
C INPUTS                                                                
C FUNC IS SUBROUTINE, CALL FUNC(N,X,F).                                 
C IT MUST RETURN F-VECTOR, GIVEN X-VECTOR.                              
C Z1JAC IS SUBROUTINE TO CALCULATE JACOBIAN                             
C X IS N-VECTOR, GUESS AT ANSWER.                                       
C EPS IS TOLERANCE.  SUCCESS MEANS 2-NORM OF FUNCTION .LE. EPS.         
C JMAX IS UPPER LIMIT ON NUMBER OF CALLS TO FUNC                        
C OUTPUTS                                                               
C X IS REPLACED BY NEW GUESS                                            
C F2NORM IS 2-NORM OF FUNC AT X.                                        
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(28H ZONE - N MUST BE AT LEAST 1, 28, 1  
C    1   , 2)                                                           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' ZONE - N MUST BE AT LEAST 1', 28, 1   
     1   , 2)                                                           
C/                                                                      
      IFF = ISTKGT(N*(2*N+6), 3)                                        
      IDX = IFF+N                                                       
      IDF = IDX+N                                                       
      IPP = IDF+N                                                       
      IGG = IPP+N                                                       
      IAUX = IGG+N                                                      
      IRMAT = IAUX+N                                                    
      IQMAT = IRMAT+N*N                                                 
      IPRINT = 0                                                        
      CALL Z1ONE(FUNC, Z1JAC, N, X, EPS, JMAX, F2NORM, IPRINT, R(IFF), R
     1   (IDX), R(IDF), R(IPP), R(IGG), R(IAUX), R(IRMAT), R(IQMAT),    
     2   KFLAG)                                                         
      CALL ISTKRL(1)                                                    
      IF (KFLAG .NE. 2) GOTO 1                                          
C/6S                                                                    
C        CALL SETERR(32H ZONE - INITIAL X-VECTOR NO GOOD, 32, 2, 1)     
C/7S                                                                    
         CALL SETERR(' ZONE - INITIAL X-VECTOR NO GOOD', 32, 2, 1)      
C/                                                                      
         GOTO  8                                                        
   1     IF (KFLAG .NE. 3) GOTO 2                                       
C/6S                                                                    
C           CALL SETERR(30H ZONE - TOO MANY CALLS TO FUNC, 30, 3, 1)    
C/7S                                                                    
            CALL SETERR(' ZONE - TOO MANY CALLS TO FUNC', 30, 3, 1)     
C/                                                                      
            GOTO  7                                                     
   2        IF (KFLAG .NE. 4) GOTO 3                                    
C/6S                                                                    
C              CALL SETERR(28H ZONE - CONVERGENCE TOO SLOW, 28, 4, 1)   
C/7S                                                                    
               CALL SETERR(' ZONE - CONVERGENCE TOO SLOW', 28, 4, 1)    
C/                                                                      
               GOTO  6                                                  
   3           IF (KFLAG .NE. 5) GOTO 4                                 
C/6S                                                                    
C                 CALL SETERR(34H ZONE - COULD NOT GET NEW JACOBIAN, 34,
C    1               5, 1)                                              
C/7S                                                                    
                  CALL SETERR(' ZONE - COULD NOT GET NEW JACOBIAN', 34, 
     1               5, 1)                                              
C/                                                                      
                  GOTO  5                                               
C/6S                                                                    
C  4              IF (KFLAG .EQ. 6) CALL SETERR(                        
C    1               41H ZONE - DID NOT IMPROVE WITH NEW JACOBIAN, 41, 6
C    2               , 1)                                               
C/7S                                                                    
   4              IF (KFLAG .EQ. 6) CALL SETERR(                        
     1               ' ZONE - DID NOT IMPROVE WITH NEW JACOBIAN', 41, 6 
     2               , 1)                                               
C/                                                                      
   5        CONTINUE                                                    
   6     CONTINUE                                                       
   7  CONTINUE                                                          
   8  RETURN                                                            
      END                                                               
      SUBROUTINE ZONEJ(FUNC, Z1JAC, N, X, EPS, JMAX, F2NORM)            
      INTEGER N                                                         
      EXTERNAL FUNC, Z1JAC                                              
      INTEGER JMAX                                                      
      REAL X(N), EPS, F2NORM                                            
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IFF, ISTKGT, IDX, IDF, IPP, IGG                           
      INTEGER IAUX, IRMAT, IQMAT, IPRINT, KFLAG                         
      REAL R(1000)                                                      
      EQUIVALENCE (D(1), R(1))                                          
C SOLVE N NONLINEAR EQUATIONS F(X)=0                                    
C INPUTS                                                                
C FUNC IS SUBROUTINE, CALL FUNC(N,X,F).                                 
C IT MUST RETURN F-VECTOR, GIVEN X-VECTOR.                              
C Z1JAC IS SUBROUTINE TO CALCULATE JACOBIAN                             
C X IS N-VECTOR, GUESS AT ANSWER.                                       
C EPS IS TOLERANCE.  SUCCESS MEANS 2-NORM OF FUNCTION .LE. EPS.         
C JMAX IS UPPER LIMIT ON NUMBER OF CALLS TO FUNC                        
C OUTPUTS                                                               
C X IS REPLACED BY NEW GUESS                                            
C F2NORM IS 2-NORM OF FUNC AT X.                                        
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(28H ZONEJ- N MUST BE AT LEAST 1, 28, 1  
C    1   , 2)                                                           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' ZONEJ- N MUST BE AT LEAST 1', 28, 1   
     1   , 2)                                                           
C/                                                                      
      IFF = ISTKGT(N*(2*N+6), 3)                                        
      IDX = IFF+N                                                       
      IDF = IDX+N                                                       
      IPP = IDF+N                                                       
      IGG = IPP+N                                                       
      IAUX = IGG+N                                                      
      IRMAT = IAUX+N                                                    
      IQMAT = IRMAT+N*N                                                 
      IPRINT = 0                                                        
      CALL Z1ONE(FUNC, Z1JAC, N, X, EPS, JMAX, F2NORM, IPRINT, R(IFF), R
     1   (IDX), R(IDF), R(IPP), R(IGG), R(IAUX), R(IRMAT), R(IQMAT),    
     2   KFLAG)                                                         
      CALL ISTKRL(1)                                                    
      IF (KFLAG .NE. 2) GOTO 1                                          
C/6S                                                                    
C        CALL SETERR(32H ZONEJ- INITIAL X-VECTOR NO GOOD, 32, 2, 1)     
C/7S                                                                    
         CALL SETERR(' ZONEJ- INITIAL X-VECTOR NO GOOD', 32, 2, 1)      
C/                                                                      
         GOTO  8                                                        
   1     IF (KFLAG .NE. 3) GOTO 2                                       
C/6S                                                                    
C           CALL SETERR(30H ZONEJ- TOO MANY CALLS TO FUNC, 30, 3, 1)    
C/7S                                                                    
            CALL SETERR(' ZONEJ- TOO MANY CALLS TO FUNC', 30, 3, 1)     
C/                                                                      
            GOTO  7                                                     
   2        IF (KFLAG .NE. 4) GOTO 3                                    
C/6S                                                                    
C              CALL SETERR(28H ZONEJ- CONVERGENCE TOO SLOW, 28, 4, 1)   
C/7S                                                                    
               CALL SETERR(' ZONEJ- CONVERGENCE TOO SLOW', 28, 4, 1)    
C/                                                                      
               GOTO  6                                                  
   3           IF (KFLAG .NE. 5) GOTO 4                                 
C/6S                                                                    
C                 CALL SETERR(34H ZONEJ- COULD NOT GET NEW JACOBIAN, 34,
C    1               5, 1)                                              
C/7S                                                                    
                  CALL SETERR(' ZONEJ- COULD NOT GET NEW JACOBIAN', 34, 
     1               5, 1)                                              
C/                                                                      
                  GOTO  5                                               
C/6S                                                                    
C  4              IF (KFLAG .EQ. 6) CALL SETERR(                        
C    1               41H ZONEJ- DID NOT IMPROVE WITH NEW JACOBIAN, 41, 6
C    2               , 1)                                               
C/7S                                                                    
   4              IF (KFLAG .EQ. 6) CALL SETERR(                        
     1               ' ZONEJ- DID NOT IMPROVE WITH NEW JACOBIAN', 41, 6 
     2               , 1)                                               
C/                                                                      
   5        CONTINUE                                                    
   6     CONTINUE                                                       
   7  CONTINUE                                                          
   8  RETURN                                                            
      END                                                               
      SUBROUTINE Z1ONE(FUNC, Z1JAC, N, X, EPS, JJMAX, F2OUT,            
     1   IPRINT, F, DX, DF, P, G, AUX, R, QT, KFLAG)                    
      INTEGER N                                                         
      EXTERNAL FUNC, Z1JAC                                              
      INTEGER JJMAX, IPRINT, KFLAG                                      
      REAL X(N), EPS, F2OUT, F(N), DX(N), DF(N)                         
      REAL P(N), G(N), AUX(N), R(N, N), QT(N, N)                        
      COMMON /Z1COM/ P2, G2, XP, XG, RATSAV, XGMAX, X2, DX2OLD, DXTRY,  
     1   RELERR, F2PREV, F2BEST, F2LAST, F2NORM, F2DMAX, JMAX, JCALL,   
     2   ISLOW, JPRINT, NUMUPD, N2BIG, SMALL, SING, GFLAG, VFLAG        
      INTEGER JMAX, JCALL, ISLOW, JPRINT, NUMUPD, N2BIG                 
      REAL P2, G2, XP, XG, RATSAV, XGMAX                                
      REAL X2, DX2OLD, DXTRY, RELERR, F2PREV, F2BEST                    
      REAL F2LAST, F2NORM, F2DMAX                                       
      LOGICAL SMALL, SING, GFLAG, VFLAG                                 
      INTEGER ISMAX, IROLD, MAXSD, MIN0, NERROR, NERR                   
      INTEGER MODE, JUSED, MAX0, J1IMPR, J2IMPR, J                      
      REAL SNRM2, R1MACH, Z1CON, Z1DOT, COND, CONMAX                    
      REAL DX2, G2OLD, F2IMPR, PMIN, RATE1, RATE2                       
      REAL TEMP, COS, XPBIG, FLOAT, AMIN1, SQRT                         
      REAL AMAX1                                                        
      LOGICAL NEWJAC, LAST, FIRST, NOGOOD                               
      DATA ISMAX/3/                                                     
      DATA RATE1/0.98E0/                                                
      DATA RATE2/0.95E0/                                                
      DATA PMIN/1.E-4/                                                  
C SOLVE N NONLINEAR EQUATIONS F(X)=0                                    
C INPUTS                                                                
C FUNC IS SUBROUTINE, CALL FUNC(N,X,F).                                 
C IT MUST RETURN F-VECTOR, GIVEN X-VECTOR.                              
C Z1JAC IS SUBROUTINE, CALL Z1JAC(FUNC,N,X,F,DFDX,J).                   
C X IS N-VECTOR, GUESS AT ANSWER.                                       
C EPS IS TOLERANCE.  SUCCESS MEANS 2-NORM OF FUNCTION .LE. EPS.         
C JJMAX IS UPPER LIMIT ON NUMBER OF CALLS TO FUNC                       
C IPRINT = PRINTING LEVEL                                               
C F,DX,DF,P,G,AUX,R,QT ARE SCRATCH ARRAYS                               
C OUTPUTS                                                               
C NEW X                                                                 
C F2OUT = 2-NORM OF FUNC AT X                                           
C F = FUNC-VECTOR AT X                                                  
C KFLAG=1   SUCCESSFUL                                                  
C       2   INITIAL X-VECTOR NO GOOD                                    
C       3   TOO MANY CALLS TO FUNC                                      
C       4   CONVERGENCE TOO SLOW                                        
C       5   COULD NOT GET NEW JACOBIAN                                  
C       6   DID NOT IMPROVE WITH NEW JACOBIAN                           
      CALL ENTSRC(IROLD, 1)                                             
      JMAX = JJMAX                                                      
      MAXSD = MIN0(3*N, 15)                                             
      JPRINT = IPRINT                                                   
      KFLAG = 0                                                         
      CALL FUNC(N, X, F)                                                
      JCALL = 1                                                         
      IF (NERROR(NERR) .EQ. 0) F2NORM = SNRM2(N, F, 1)                  
      IF (NERROR(NERR) .EQ. 0) GOTO 1                                   
         CALL ERROFF                                                    
         KFLAG = 2                                                      
         F2OUT = R1MACH(2)                                              
         RETURN                                                         
   1  IF (F2NORM .GT. EPS) GOTO 2                                       
         F2OUT = F2NORM                                                 
         RETURN                                                         
   2  RELERR = FLOAT(2*N)*R1MACH(4)                                     
      CONMAX = AMIN1(1.E4, 0.01/R1MACH(4))                              
      F2BEST = F2NORM                                                   
      F2PREV = 2.0E0*F2BEST                                             
      DX2OLD = 0.0E0                                                    
      XG = 1.0E0                                                        
      MODE = 2                                                          
      LAST = .FALSE.                                                    
      SMALL = .FALSE.                                                   
C GET NEW JACOBIAN MATRIX R AND CONTINUE                                
   3     IF (F2BEST .LE. RATE2*F2PREV) GOTO 5                           
            ISLOW = ISLOW+1                                             
C SLOW CONVERGENCE                                                      
            IF (ISLOW .LE. ISMAX) GOTO 4                                
               KFLAG = 4                                                
               GOTO  26                                                 
   4        LAST = LAST .OR. ISLOW .EQ. ISMAX .OR. F2DMAX .LT. 0.3E0    
            GFLAG = XPBIG .LE. 0.0E0                                    
            GOTO  6                                                     
   5        ISLOW = 0                                                   
C ACCEPTABLE CONVERGENCE                                                
            GFLAG = .FALSE.                                             
   6     X2 = SNRM2(N, X, 1)                                            
         DXTRY = SQRT(R1MACH(4))*AMAX1(1.0E0, X2)                       
C        *** G2 IS NOT DEFINED WHEN JCALL .LE. 1 ***                    
         IF (JCALL .LE. 1) GO TO 61                                     
            IF (G2 .GT. 0.0E0) DXTRY = AMIN1(DXTRY, G2)                 
   61    JUSED = 0                                                      
         CALL Z1JAC(FUNC, N, X, F, R, JUSED)                            
         JCALL = JCALL+MAX0(0, JUSED)                                   
         IF (NERROR(NERR) .EQ. 0) GOTO 7                                
            CALL ERROFF                                                 
            KFLAG = 5                                                   
            GOTO  26                                                    
   7     NUMUPD = 0                                                     
         J1IMPR = 0                                                     
         J2IMPR = 0                                                     
         F2IMPR = F2BEST                                                
         F2PREV = F2BEST                                                
         F2DMAX = 0.0E0                                                 
         FIRST = .TRUE.                                                 
         VFLAG = .FALSE.                                                
         XPBIG = 0.0E0                                                  
         RATSAV = 1.0E0                                                 
         N2BIG = 0                                                      
C QR FACTORIZATION OF JACOBIAN                                          
         CALL Z1FAC(N, R, QT)                                           
C LOWER BOUND ON CONDITION NUMBER                                       
         COND = Z1CON(N, R)                                             
         IF (.NOT. LAST) GOTO 8                                         
            SING = COND .GT. 0.5E0*R1MACH(2)                            
            GOTO  9                                                     
   8        SING = COND .GT. CONMAX                                     
   9     CONTINUE                                                       
C USE CURRENT JACOBIAN APPROXIMATION AS LONG AS IT WORKS.               
C GET P=NEWTON STEP AND G=STEEPEST-DESCENT STEP                         
            CALL Z1PG(N, F, F2NORM, QT, R, P, G, P2, G2, SING, AUX)     
            IF ((.NOT. FIRST) .OR. G2 .GT. RELERR*X2) GOTO 11           
               IF (G2 .GT. 0.0E0 .AND. ((.NOT. SMALL) .OR. (.NOT. SING))
     1            ) GOTO 10                                             
                  KFLAG = 6                                             
                  GOTO  24                                              
  10        CONTINUE                                                    
  11        IF (FIRST) GOTO 14                                          
               COS = Z1DOT(N, G, G2, DX, G2OLD)                         
               IF (MODE .EQ. 2 .AND. COS .GT. SQRT(1.0E0-1.0E0/FLOAT(N))
     1            ) GOTO  24                                            
C AND XG.GE.1.0E0                                                       
               VFLAG = VFLAG .AND. COS .LT. 0.3E0 .AND. COS .GT. (-     
     1            0.9E0) .AND. NUMUPD .LT. 3 .AND. G2 .GT. 0.25E0*G2OLD 
     2             .AND. F2DMAX .LT. 0.3E0 .AND. F2NORM .GT. RATE1*     
     3            F2BEST                                                
               IF (.NOT. VFLAG) GOTO 13                                 
                  DO  12 J = 1, N                                       
C AVERAGE OF TWO GS MAY BE DOWN THE VALLEY                              
                     G2 = 0.5E0*XG                                      
                     TEMP = G2*(G(J)+DX(J))                             
                     G(J) = G2*(G(J)-DX(J))                             
                     P(J) = TEMP                                        
  12                 CONTINUE                                           
                  G2 = SNRM2(N, G, 1)                                   
                  P2 = SNRM2(N, P, 1)                                   
  13           CONTINUE                                                 
C GET ACCEPTABLE NEW VALUES OF X, F                                     
  14        CALL Z1NEW(FUNC, N, X, EPS, F, DX, DF, AUX, P, G, DX2, PMIN,
     1         ISMAX, MODE, NEWJAC, NOGOOD)                             
            IF (F2NORM .GT. EPS) GOTO 15                                
               KFLAG = 1                                                
               GOTO  24                                                 
  15        XPBIG = AMAX1(XP, XPBIG)                                    
            IF (JCALL .LT. JMAX) GOTO 16                                
               KFLAG = 3                                                
               GOTO  24                                                 
  16        IF ((.NOT. FIRST) .OR. (.NOT. NEWJAC) .OR. (.NOT. NOGOOD))  
     1          GOTO 17                                                 
               KFLAG = 6                                                
               GOTO  24                                                 
  17        J1IMPR = J1IMPR+1                                           
            IF (F2NORM .GE. F2BEST) GOTO 18                             
               IF (F2NORM .LT. RATE1*F2BEST) J1IMPR = 0                 
               F2BEST = F2NORM                                          
  18        J2IMPR = J2IMPR+1                                           
            IF (F2NORM .GE. RATE2*F2IMPR) GOTO 19                       
               F2IMPR = F2NORM                                          
               J2IMPR = 0                                               
  19        IF (J2IMPR .GT. 4 .OR. NEWJAC .OR. J1IMPR .GT. 4 .OR. SMALL 
     1          .AND. (.NOT. FIRST) .OR. XPBIG .LE. 0.0E0 .AND. NUMUPD  
     2          .GT. MAXSD) GOTO  24                                    
            IF (.NOT. SMALL) GOTO 20                                    
               SING = COND .GT. 0.5E0*R1MACH(2)                         
               IF (SING) GOTO  24                                       
               GOTO  22                                                 
  20           CALL Z1MV(N, QT, R, DX, AUX, P)                          
C RANK-ONE UPDATE TO APPROXIMATE JACOBIAN                               
C P=Q*R*DX                                                              
               DO  21 J = 1, N                                          
                  DF(J) = (DF(J)-P(J))/DX2                              
                  DX(J) = DX(J)/DX2                                     
  21              CONTINUE                                              
               CALL Z1UPD(N, QT, R, DF, DX, AUX)                        
               COND = Z1CON(N, R)                                       
               SING = COND .GT. CONMAX                                  
               FIRST = .FALSE.                                          
               NUMUPD = NUMUPD+1                                        
  22        VFLAG = (.NOT. VFLAG) .AND. (XP .LE. 0.01E0 .AND. F2NORM    
     1          .GT. RATE2*F2LAST)                                      
C SAVE G FOR NEXT TIME                                                  
            DO  23 J = 1, N                                             
               DX(J) = G(J)                                             
  23           CONTINUE                                                 
            G2OLD = G2                                                  
            GOTO  9                                                     
  24     IF (KFLAG .EQ. 0) GOTO 25                                      
            IF (LAST .OR. KFLAG .NE. 4) GOTO  26                        
            LAST = .TRUE.                                               
            KFLAG = 0                                                   
  25     CONTINUE                                                       
         GOTO  3                                                        
  26  F2OUT = F2NORM                                                    
      CALL RETSRC(IROLD)                                                
      RETURN                                                            
      END                                                               
      REAL FUNCTION Z1CON(N, R)                                         
      INTEGER N                                                         
      REAL R(N, N)                                                      
      INTEGER I, NP, J                                                  
      REAL R1MACH, RMIN, RMAX, ABS, AMAX1, AMIN1                        
C LOWER BOUND ON CONDITION NUMBER OF UPPER TRIANGULAR MATRIX R.         
      RMAX = ABS(R(N, N))                                               
      RMIN = RMAX                                                       
      I = N-1                                                           
         GOTO  2                                                        
   1     I = I-1                                                        
   2     IF (I .LT. 1)GOTO  4                                           
         NP = N+1                                                       
         DO  3 J = NP, N                                                
            RMAX = AMAX1(RMAX, ABS(R(I, J)))                            
   3        CONTINUE                                                    
         RMIN = AMIN1(RMIN, ABS(R(I, I)))                               
         GOTO  1                                                        
   4  IF (RMIN .GE. 1.0E0 .AND. RMAX .GT. 0.0E0) GOTO 6                 
         IF (RMAX .LT. 0.5E0*R1MACH(2)*RMIN) GOTO 5                     
            Z1CON = R1MACH(2)                                           
            RETURN                                                      
   5  CONTINUE                                                          
   6  Z1CON = RMAX/RMIN                                                 
      RETURN                                                            
      END                                                               
      SUBROUTINE Z1NEW(FUNC, N, X, EPS, F, XNEW, FNEW, FSAV, P, G,      
     1   DX2, PMIN, ISMAX, MODE, NEWJAC, NOGOOD)                        
      INTEGER N                                                         
      EXTERNAL FUNC                                                     
      INTEGER ISMAX, MODE                                               
      REAL X(N), EPS, F(N), XNEW(N), FNEW(N), FSAV(N)                   
      REAL P(N), G(N), DX2, PMIN                                        
      LOGICAL NEWJAC, NOGOOD                                            
      COMMON /Z1COM/ P2, G2, XP, XG, RATSAV, XGMAX, X2, DX2OLD, DXTRY,  
     1   RELERR, F2PREV, F2BEST, F2LAST, F2NORM, F2DMAX, JMAX, JCALL,   
     2   ISLOW, JPRINT, NUMUPD, N2BIG, SMALL, SING, GFLAG, VFLAG        
      INTEGER JMAX, JCALL, ISLOW, JPRINT, NUMUPD, N2BIG                 
      REAL P2, G2, XP, XG, RATSAV, XGMAX                                
      REAL X2, DX2OLD, DXTRY, RELERR, F2PREV, F2BEST                    
      REAL F2LAST, F2NORM, F2DMAX                                       
      LOGICAL SMALL, SING, GFLAG, VFLAG                                 
      INTEGER LASTM, NTMAX, NT, J, NERROR, NERR                         
      INTEGER I1SGN, I2SGN, MIN0                                        
      REAL SNRM2, Z1DEL, SDOT, Z1DOT, DELTA, DX2SAV                     
      REAL F2GOAL, F2NEW, F2SAV, RATOLD, RATIO, RHO                     
      REAL TEMP, XGSAV, XPSAV, FW, FWORSE, PG                           
      REAL F2IN, F2DEL, CONVR, COS, AMIN1, AMAX1                        
      REAL SQRT, FLOAT                                                  
      LOGICAL STEEP, TOOFAR, FIRST, OKAY3, LTEMP                        
      DATA FW/1.5E0/                                                    
      DATA RHO/0.7E0/                                                   
C OBTAIN NEW X-VECTOR AND F-VECTOR                                      
C INPUTS                                                                
C FUNC IS SUBROUTINE, CALL FUNC(N,X,F).                                 
C IT MUST RETURN F-VECTOR, GIVEN X-VECTOR.                              
C X IS CURRENT X-VECTOR                                                 
C EPS IS TOLERANCE.  SUCCESS MEANS 2-NORM OF FUNCTION .LE. EPS.         
C F IS N-VECTOR OF VALUE OF FUNC AT X                                   
C XNEW,FNEW,FSAV ARE SCRATCH ARRAYS                                     
C IF (VFLAG) DO VALLEY SEARCH                                           
C    P IS N-VECTOR, STEP ALONG VALLEY                                   
C    G IS N-VECTOR, STEP INTO VALLEY                                    
C IF ((NOT)VFLAG) DOGLEG STRATEGY                                       
C    P IS N-VECTOR, NEWTON STEP                                         
C    G IS N-VECTOR, STEEPEST-DESCENT STEP                               
C PMIN IS MINIMUM ALLOWABLE FRACTION OF P IN DX STEP                    
C ISMAX IS MAXIMUM VALUE OF ISLOW                                       
C OUTPUTS                                                               
C IF SUCCESSFUL                                                         
C    NEW X AND F-VECTORS                                                
C    XNEW, FNEW HAVE DX, DF-VECTORS                                     
C    DX2 IS LENGTH OF DX-VECTOR                                         
C IF UNSUCCESSFUL                                                       
C    X AND F-VECTORS ARE UNCHANGED                                      
C MODE = STRATEGY MODE USED                                             
C      = 1, STANDARD HYBRID                                             
C      = 2, GRADIENT SEARCH                                             
C      = 3, HYBRID SEARCH                                               
C      = 4, VALLEY SEARCH                                               
C NEWJAC = (DO NOT WANT TO USE OLD JACOBIAN ANY MORE)                   
C NOGOOD = (DID NOT GET NEW F THAT WAS ACCEPTABLE)                      
      NEWJAC = .FALSE.                                                  
      NOGOOD = .FALSE.                                                  
      LTEMP = .FALSE.                                                   
      FIRST = NUMUPD .EQ. 0                                             
      IF (.NOT. FIRST) GOTO 1                                           
         XG = AMIN1(XG, 1.0E0)                                          
         DELTA = AMAX1(XG*G2, DX2OLD)                                   
         GOTO  2                                                        
   1     DELTA = Z1DEL(DX2OLD, XP, P2, G2, PMIN, F2NORM, F2LAST, F2BEST)
COKAY3=NUMUPD.LT.3 AND DELTA.GE.PMIN*P2      AND XG.GE.1.0D0            
C  AND XG.GE.1.0E0                                                      
   2  OKAY3 = FIRST .AND. DELTA .GE. PMIN*P2                            
      LASTM = MODE                                                      
      MODE = 1                                                          
      NTMAX = 2                                                         
      IF (FIRST) NTMAX = 15                                             
C  IF (DELTA.LT.0.1D0*P2)      SEARCH ALONG STEEPEST-DESCENT            
C     MODE=2, NTMAX=2*N, DELTA=G2                                       
      IF ((.NOT. FIRST) .OR. ISLOW .NE. ISMAX .AND. (.NOT. GFLAG) .OR. (
     1   .NOT. OKAY3)) GOTO 3                                           
         MODE = 3                                                       
C DOGLEG SEARCH                                                         
         NTMAX = 15                                                     
   3  IF (.NOT. VFLAG) GOTO 6                                           
         MODE = 4                                                       
         PG = SDOT(N, P, 1, G, 1)                                       
         IF (P2 .LE. 0.1E0*G2) GOTO 4                                   
            NTMAX = 15                                                  
            GOTO  5                                                     
   4        NTMAX = 2                                                   
   5     CONTINUE                                                       
   6  F2IN = F2NORM                                                     
      NT = 1                                                            
         GOTO  8                                                        
   7     NT = NT+1                                                      
   8     IF (NT .GT. NTMAX)GOTO  58                                     
C USING SAME P, AND G,                                                  
C LOOK FOR ACCEPTABLE XNEW.                                             
         IF (.NOT. FIRST) GOTO 9                                        
            STEEP = DELTA .LT. 0.05E0*P2 .AND. MODE .NE. 3 .OR. LASTM   
     1          .EQ. 2 .AND. ISLOW .EQ. 0                               
            GOTO  10                                                    
   9        STEEP = DELTA .LE. G2 .AND. DELTA .LT. 0.05E0*P2 .OR. XP    
     1          .LE. 0.0E0 .AND. DELTA .LT. 0.05E0*P2 .AND. F2NORM      
     2          .LT. RHO*F2LAST .AND. G2 .GT. 0.25E0*DX2OLD             
  10     IF (STEEP .OR. DELTA .LT. PMIN*P2) DELTA = AMIN1(DELTA, G2)    
C GET XP AND XG, DX=XP*P+XG*G                                           
         IF (MODE .NE. 2) GOTO 11                                       
            XP = 0.0E0                                                  
            XG = NT                                                     
            STEEP = .TRUE.                                              
            DX2 = XG*G2                                                 
            GOTO  16                                                    
  11        IF (MODE .NE. 4) GOTO 14                                    
               XG = 1.0E0                                               
               IF (NT .NE. 1) GOTO 12                                   
                  XP = 2.0E0                                            
                  GOTO  13                                              
  12              XP = 2.0E0*XP                                         
                  IF (F2DEL .LT. 0.5E0) XP = 0.5E0*XP/AMAX1(0.1E0,      
     1               F2DEL)                                             
  13           DX2 = SQRT(G2**2+2.0E0*XP*PG+(XP*P2)**2)                 
               GOTO  15                                                 
  14           CALL Z1XPG(N, STEEP, DELTA, P, P2, PMIN, G, G2, XP, XG,  
     1            DX2)                                                  
  15     CONTINUE                                                       
  16     IF (DX2 .GE. RELERR*X2) GOTO 23                                
            IF (.NOT. SMALL) GOTO 21                                    
               IF ((.NOT. SING) .AND. FIRST) GOTO 17                    
                  NEWJAC = .TRUE.                                       
                  GOTO  20                                              
  17              MODE = 3                                              
                  NTMAX = 15                                            
                  XG = 1.0E0                                            
                  PG = SDOT(N, P, 1, G, 1)                              
  18              IF ((.NOT. SMALL) .OR. NT .GE. NTMAX)GOTO  19         
                     XP = PMIN*2.0E0**(NT-1)                            
                     NT = NT+1                                          
                     DX2 = SQRT(G2**2+2.0E0*XP*PMIN*PG+(XP*PMIN*P2)**2) 
                     SMALL = DX2 .LT. RELERR*X2                         
                     GOTO  18                                           
  19              CONTINUE                                              
  20           CONTINUE                                                 
               GOTO  22                                                 
  21           SMALL = .TRUE.                                           
  22        CONTINUE                                                    
            GOTO  24                                                    
  23        SMALL = .FALSE.                                             
  24     DO  25 J = 1, N                                                
            XNEW(J) = X(J)+XP*P(J)+XG*G(J)                              
  25        CONTINUE                                                    
         JCALL = JCALL+1                                                
CGET NEW F                                                              
         CALL FUNC(N, XNEW, FNEW)                                       
         IF (NERROR(NERR) .NE. 0) GOTO 26                               
            F2NEW = SNRM2(N, FNEW, 1)                                   
            F2LAST = F2NORM                                             
  26     IF (NERROR(NERR) .EQ. 0) GOTO 27                               
            CALL ERROFF                                                 
            F2NEW = 4.0*F2BEST                                          
  27     IF (NT .GT. 1) RATOLD = RATIO                                  
         RATIO = F2NEW/F2NORM                                           
         IF (RATIO .LE. 1.0E0) GOTO 30                                  
            IF (.NOT. FIRST) GOTO 28                                    
               N2BIG = 1                                                
               GOTO  29                                                 
  28           N2BIG = N2BIG+1                                          
  29     CONTINUE                                                       
  30     IF (N2BIG .LT. 4) GOTO 31                                      
            FWORSE = 1.0E0                                              
            GOTO  32                                                    
  31        FWORSE = FW*F2BEST/F2IN                                     
  32     F2DEL = AMIN1(10.0E0*F2IN, F2NEW)/F2IN                         
         IF (F2NEW .LE. 0.) GOTO 33                                     
            COS = Z1DOT(N, F, F2IN, FNEW, F2NEW)                        
            F2DEL = F2DEL*(F2DEL-2.0E0*COS)+1.0E0                       
            GOTO  34                                                    
  33        COS = 0.0E0                                                 
            F2DEL = 1.0E0                                               
  34     F2DEL = SQRT(AMAX1(0.0E0, F2DEL))                              
C SEE IF TENTATIVE MODE IS REASONABLE                                   
         TOOFAR = .FALSE.                                               
         IF (F2NEW .GT. EPS) GOTO 35                                    
            MODE = 1                                                    
            GOTO  58                                                    
  35     IF (NT .NE. 1) GOTO 39                                         
            IF (XP .GT. 0.0E0 .OR. RATIO .GT. 0.4E0) GOTO 36            
               MODE = 2                                                 
               NTMAX = 2*N                                              
               CONVR = (F2IN/F2NEW)**(1.0E0/FLOAT(N+1))-1.0E0           
  36        IF ((MODE .NE. 4 .OR. RATIO .LE. FWORSE) .AND. (MODE .NE. 3 
     1          .OR. RATIO .LE. 1.0E0)) GOTO 37                         
               MODE = 1                                                 
               NTMAX = 15                                               
  37        IF (RATIO .GE. 1.0E0 .OR. (.NOT. OKAY3) .OR. RATIO .LE.     
     1         0.98E0 .OR. F2DEL .GE. 0.1E0 .OR. MODE .EQ. 4) GOTO 38   
               MODE = 3                                                 
               NTMAX = 15                                               
C     IF (FIRST AND XP.LE.0.0E0 AND RATIO.GT.1.0E0) NEWJAC=.TRUE.       
  38        CONTINUE                                                    
C DECIDE ON ACCEPTABILITY OF XNEW AND FNEW                              
  39     IF (MODE .NE. 2) GOTO 45                                       
            IF (NT .NE. 1) GOTO 40                                      
               I1SGN = 0                                                
               GOTO  43                                                 
  40           IF (RATIO .LE. RATOLD) GOTO 41                           
                  I2SGN = 1                                             
                  GOTO  42                                              
  41              I2SGN = -1                                            
  42        CONTINUE                                                    
C  COS .LE. 0.7                                                         
  43        TOOFAR = F2DEL .GT. 1.0E0 .OR. RATIO .GT. 1.0E0             
            IF (I1SGN*I2SGN .LT. 0) NTMAX = NT                          
            IF (NT .LE. 1 .OR. TOOFAR) GOTO 44                          
               TEMP = (F2IN/F2NEW)**(1.0E0/FLOAT(N+NT))-1.0E0           
               TOOFAR = TEMP .LT. 1.05E0*CONVR                          
               CONVR = TEMP                                             
               I1SGN = I2SGN                                            
  44        CONTINUE                                                    
  45     IF (MODE .EQ. 3) TOOFAR = F2NEW .GT. FWORSE*F2IN               
         IF (MODE .NE. 4) GOTO 48                                       
            IF (F2NORM .GT. 0.9E0*F2PREV) GOTO 46                       
               TOOFAR = F2NEW .GT. F2NORM                               
               GOTO  47                                                 
  46           TOOFAR = F2NEW .GT. FWORSE*F2IN                          
  47        TOOFAR = TOOFAR .OR. NT .EQ. 1 .AND. RATIO .GT. 0.5E0*      
     1         RATSAV+0.5E0                                             
            IF (F2DEL .GT. 0.7) NTMAX = NT                              
  48     IF (MODE .NE. 1) TOOFAR = TOOFAR .OR. F2DEL .GT. 1.0E0         
         IF (NT .EQ. 1 .AND. TOOFAR) MODE = 1                           
         IF (MODE .NE. 1) GOTO 51                                       
            F2GOAL = FWORSE*F2IN                                        
            IF (F2BEST .LT. 0.1E0*F2PREV .OR. NT .EQ. 1 .AND. XP .LE.   
     1         0.0E0) F2GOAL = F2IN                                     
            TOOFAR = F2NEW .GT. AMAX1(EPS, F2GOAL)                      
            IF (.NOT. TOOFAR) GOTO 49                                   
               DELTA = AMAX1(0.25E0, AMIN1(0.5E0, F2IN/F2NEW))*DX2      
               GOTO  50                                                 
  49           F2DMAX = AMAX1(F2DMAX, F2DEL)                            
               NTMAX = NT                                               
  50        CONTINUE                                                    
            GOTO  57                                                    
  51        IF (.NOT. TOOFAR) GOTO 54                                   
               IF (MODE .NE. 3 .OR. (.NOT. OKAY3) .OR. DX2 .LE. 3.0*    
     1            DX2OLD .OR. NT .LE. 1) GOTO 52                        
                  DELTA = SQRT(DX2*DX2SAV)                              
                  OKAY3 = .FALSE.                                       
                  LTEMP = .TRUE.                                        
                  TOOFAR = .FALSE.                                      
                  NTMAX = MIN0(NT+1, NTMAX)                             
                  GOTO  53                                              
  52              GOTO  58                                              
  53           CONTINUE                                                 
  54        IF (LTEMP) GOTO  7                                          
C 2-LEVEL NEXT                                                          
            XPSAV = XP                                                  
            XGSAV = XG                                                  
            F2SAV = F2NEW                                               
            F2NORM = F2NEW                                              
            DX2SAV = DX2                                                
            F2DMAX = AMAX1(F2DMAX, F2DEL)                               
            DO  55 J = 1, N                                             
               FSAV(J) = FNEW(J)                                        
  55           CONTINUE                                                 
            IF (MODE .NE. 3) GOTO 56                                    
               IF (XP .GE. 0.99E0) GOTO  58                             
               DELTA = AMAX1(2.0E0*DX2, RELERR*X2)                      
               IF (F2DEL .LT. 0.5E0) DELTA = 0.5E0*DELTA/AMAX1(0.1E0,   
     1            F2DEL)                                                
               IF (.NOT. SING) DELTA = AMAX1(DELTA, PMIN*P2)            
  56        CONTINUE                                                    
  57     IF (JCALL .GE. JMAX .OR. N2BIG .GE. 5 .OR. SMALL .AND. RATIO   
     1       .GT. 0.98E0) GOTO  58                                      
         GOTO  7                                                        
C END OF  FOR -LOOP ON NT                                               
  58  IF (MODE .EQ. 1 .OR. (.NOT. TOOFAR)) GOTO 60                      
         XP = XPSAV                                                     
C RETURN SUCCESSFUL VALUES SAVED EARLIER                                
         XG = XGSAV                                                     
         DX2 = DX2SAV                                                   
         F2NORM = F2SAV                                                 
         DO  59 J = 1, N                                                
            XNEW(J) = XP*P(J)+XG*G(J)                                   
            X(J) = X(J)+XNEW(J)                                         
            FNEW(J) = FSAV(J)-F(J)                                      
            F(J) = FSAV(J)                                              
  59        CONTINUE                                                    
         IF (NT .EQ. 2) MODE = 1                                        
         GOTO  64                                                       
  60     IF (TOOFAR) GOTO 62                                            
            F2NORM = F2NEW                                              
C SUCCESSFUL MODE 1, OR MODE 2 OR 3, AND FELL                           
C OUT BOTTOM OF FOR-LOOP                                                
            DO  61 J = 1, N                                             
               TEMP = XNEW(J)                                           
               XNEW(J) = XNEW(J)-X(J)                                   
               X(J) = TEMP                                              
               TEMP = FNEW(J)                                           
               FNEW(J) = FNEW(J)-F(J)                                   
               F(J) = TEMP                                              
  61           CONTINUE                                                 
            GOTO  63                                                    
  62        NEWJAC = .TRUE.                                             
C FAILED                                                                
            NOGOOD = .TRUE.                                             
  63  CONTINUE                                                          
  64  IF (MODE .NE. 2 .AND. MODE .NE. 4 .OR. NT .LE. 3) GOTO 65         
         DX2OLD = 0.0E0                                                 
         NEWJAC = .TRUE.                                                
         GOTO  66                                                       
  65     DX2OLD = DX2                                                   
  66  NEWJAC = NEWJAC .OR. N2BIG .GE. 5                                 
      IF (F2NORM .LT. F2IN) RATSAV = F2NORM/F2IN                        
      RETURN                                                            
      END                                                               
      REAL FUNCTION Z1DEL(DX2, XP, P2, G2, PMIN, F2NORM, F2LAST,        
     1   F2BEST)                                                        
      REAL DX2, XP, P2, G2, PMIN, F2NORM                                
      REAL F2LAST, F2BEST                                               
      REAL XINCR, RATE, AMIN1, AMAX1                                    
      DATA XINCR/1.5E0/                                                 
      DATA RATE/0.7E0/                                                  
C GET NEW GUESS AT REASONABLE STEP LENGTH                               
C INPUTS                                                                
C DX2 = PREVIOUS STEP LENGTH                                            
C XP = FRACTION OF NEWTON STEP USED IN PREVIOUS STEP                    
C P2 = NEW NEWTON STEP LENGTH                                           
C G2 = NEW STEEPEST-DESCENT STEP LENGTH                                 
C PMIN = MINIMUM ALLOWABLE XP                                           
C F2NORM = CURRENT 2-NORM OF FUNCTION                                   
C F2LAST = PREVIOUS 2-NORM                                              
C F2BEST = BEST-EVER 2-NORM                                             
C OUTPUT                                                                
C NEW STEP LENGTH Z1DEL                                                 
      IF (XP .LE. 0.0E0) GOTO 7                                         
         IF (XP .LT. 1.0E0 .OR. F2NORM .GE. F2LAST) GOTO 1              
            Z1DEL = DX2*AMIN1(XINCR, (XINCR-1.0E0)*(F2NORM/F2LAST-1.0E0)
     1         /(RATE-1.0E0)+1.0E0)                                     
C LAST STEP HAD SOME NEWTON                                             
C SUCCESSFUL FULL NEWTON STEP                                           
            GOTO  6                                                     
   1        IF (F2NORM .LT. F2LAST) GOTO 2                              
               Z1DEL = AMAX1(0.25E0, F2BEST/F2NORM)*DX2                 
               IF (F2NORM .GT. F2LAST*(RATE*XP+1.0E0)) Z1DEL = AMIN1(   
     1            Z1DEL, RATE*DX2)                                      
               GOTO  5                                                  
   2           Z1DEL = DX2*AMIN1(XINCR, (XINCR-1.0E0)*(F2NORM/F2LAST-   
     1            1.0E0)/(RATE-1.0E0)+1.0E0)                            
               IF (F2NORM .LE. F2BEST) GOTO 3                           
                  Z1DEL = AMIN1(Z1DEL, DX2)                             
                  GOTO  4                                               
   3              Z1DEL = AMAX1(Z1DEL, DX2)                             
   4           CONTINUE                                                 
   5     CONTINUE                                                       
   6     CONTINUE                                                       
         GOTO  8                                                        
   7     Z1DEL = DX2*AMIN1(XINCR, (XINCR-1.0E0)*(F2NORM/F2LAST-1.0E0)/( 
     1      RATE-1.0E0)+1.0E0)                                          
C LAST WAS STEEPEST-DESCENT                                             
         IF (F2NORM .LT. RATE*F2LAST .AND. PMIN*P2 .LT. Z1DEL .AND.     
     1      Z1DEL .LT. 0.1E0*P2 .AND. G2 .GE. 0.25E0*DX2) Z1DEL = AMIN1(
     2      G2, Z1DEL)                                                  
   8  IF (Z1DEL .LE. 0.0E0) Z1DEL = G2                                  
      RETURN                                                            
      END                                                               
      REAL FUNCTION Z1DOT(N, X, X2, Y, Y2)                              
      INTEGER N                                                         
      REAL X(N), X2, Y(N), Y2                                           
      INTEGER J                                                         
C FIND DOT PRODUCT OF UNIT VECTORS IN X AND Y DIRECTIONS                
      Z1DOT = 0.0E0                                                     
      DO  1 J = 1, N                                                    
         Z1DOT = Z1DOT+(X(J)/X2)*(Y(J)/Y2)                              
   1     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE Z1PG(N, F, F2, QT, R, P, G, P2, G2, SING, AUX)         
      INTEGER N                                                         
      REAL F(N), F2, QT(N, N), R(N, N), P(N), G(N)                      
      REAL P2, G2, AUX(N)                                               
      LOGICAL SING                                                      
      INTEGER J, IROLD, NERROR, NERR                                    
      REAL SNRM2, R1MACH                                                
C GET P=NEWTON STEP, P2= ITS 2-NORM,                                    
C     G=STEEPEST-DESCENT STEP, G2=ITS 2-NORM                            
C INPUTS                                                                
C N = NUMBER OF COMPONENTS                                              
C F = FUNCTION VECTOR                                                   
C F2 = ITS NORM                                                         
C Q*R = APPROXIMATE JACOBIAN AT F                                       
C Q IS ORTHOGONAL, TRANSPOSE STORED IN QT                               
C R IS UPPER TRIANGULAR                                                 
C SCRATCH ARRAY AUX                                                     
C OUTPUTS                                                               
C G AND G2                                                              
C IF R IS NON-SINGULAR, P AND P2 ARE CALCULATED, SING LEFT ALONE.       
C IF R IS SINGULAR, SET P2=LARGE NUMBER, P=0-VECTOR, SING=TRUE.         
      DO  1 J = 1, N                                                    
C USE P FOR SCRATCH                                                     
         P(J) = F(J)/F2                                                 
   1     CONTINUE                                                       
C NEGATIVE OF STEEPEST DESCENT DIRECTION                                
      CALL Z1VM(N, P, QT, R, AUX, G)                                    
      G2 = SNRM2(N, G, 1)                                               
      IF (G2 .LE. 0.0E0) RETURN                                         
      DO  2 J = 1, N                                                    
         G(J) = (-G(J))/G2                                              
   2     CONTINUE                                                       
C USE P FOR SCRATCH                                                     
      CALL Z1MV(N, QT, R, G, AUX, P)                                    
      P2 = SNRM2(N, P, 1)                                               
      G2 = (F2*(G2/P2))/P2                                              
      IF (G2 .LE. 0.0E0) RETURN                                         
      DO  3 J = 1, N                                                    
C NOW G IS OF PROPER LENGTH                                             
         G(J) = G2*G(J)                                                 
   3     CONTINUE                                                       
      IF (SING) GOTO 7                                                  
         CALL ENTSRC(IROLD, 1)                                          
C ENTER RECOVERY MODE                                                   
C NEGATIVE OF NEWTON DIRECTION                                          
         CALL Z1SOL(N, QT, R, F, AUX, P)                                
         IF (NERROR(NERR) .EQ. 0) GOTO 4                                
            SING = .TRUE.                                               
C MATRIX R IS SINGULAR                                                  
            CALL ERROFF                                                 
            GOTO  6                                                     
   4        P2 = SNRM2(N, P, 1)                                         
            DO  5 J = 1, N                                              
               P(J) = -P(J)                                             
   5           CONTINUE                                                 
C RESTORE OLD RECOVERY MODE                                             
   6     CALL RETSRC(IROLD)                                             
   7  IF (.NOT. SING) GOTO 9                                            
         P2 = R1MACH(2)                                                 
         DO  8 J = 1, N                                                 
            P(J) = 0.0E0                                                
   8        CONTINUE                                                    
   9  RETURN                                                            
      END                                                               
      SUBROUTINE Z1XPG(N, STEEP, DELTA, P, P2, PMIN, G, G2, XP,         
     1   XG, DX2)                                                       
      INTEGER N                                                         
      REAL DELTA, P(N), P2, PMIN, G(N), G2                              
      REAL XP, XG, DX2                                                  
      LOGICAL STEEP                                                     
      REAL SDOT, B, C, PG, SQRT                                         
C GET XP AND XG SO THAT DX(J)=XP*P(J)+XG*G(J) HAS LENGTH .LE. DELTA.    
C (LENGTH ALSO .LE.P2)                                                  
C INPUTS                                                                
C N = LNUMBER OF COMPONENTS                                             
C DELTA = MAXIMUM DX LENGTH                                             
C P = NEWTON VECTOR                                                     
C P2 = ITS LENGTH                                                       
C PMIN = SMALLEST ALLOWABLE XP                                          
C G = STEEPEST-DESCENT VECTOR                                           
C G2 = ITS LENGTH                                                       
C OUTPUTS                                                               
C XP AND XG                                                             
C DX2 = LENGTH OF DX-VECTOR                                             
      IF (P2 .GT. DELTA) GOTO 1                                         
         DX2 = P2                                                       
C USE NEWTON STEP                                                       
         XP = 1.0                                                       
         XG = 0.0E0                                                     
         GOTO  4                                                        
   1     IF ((.NOT. STEEP) .AND. DELTA .GT. G2 .AND. DELTA .GE. PMIN*P2)
     1       GOTO 2                                                     
            DX2 = DELTA                                                 
C DX ALONG STEEPEST-DESCENT                                             
            XG = DX2/G2                                                 
            XP = 0.0E0                                                  
            GOTO  3                                                     
   2        PG = SDOT(N, P, 1, G, 1)                                    
C DX IS SOME STEEPEST-DESCENT PLUS SOME NEWTON                          
            DX2 = DELTA                                                 
            C = DX2**2-G2**2                                            
            B = G2**2-PG                                                
            XP = C/(B+SQRT(B**2+C*(P2**2-2.0E0*PG+G2**2)))              
            XG = 1.0E0-XP                                               
   3  CONTINUE                                                          
   4  RETURN                                                            
      END                                                               
      SUBROUTINE Z1FAC(N, A, QT)                                        
      INTEGER N                                                         
      REAL A(N, N), QT(N, N)                                            
      INTEGER J, K                                                      
      REAL SC, SS                                                       
C DECOMPOSE N BY N MATRIX A INTO Q*R                                    
C Q IS N BY N ORTHOGONAL MATRIX                                         
C R IS N BY N UPPER-TRIANGULAR MATRIX                                   
C A IS OVERWRITTEN BY R.                                                
C TRANSPOSE OF Q IS RETURNED IN QT.                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15H Z1FAC - N.LT.1, 15, 1, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' Z1FAC - N.LT.1', 15, 1, 2)            
C/                                                                      
      DO  2 J = 1, N                                                    
         DO  1 K = 1, N                                                 
            QT(J, K) = 0.0E0                                            
   1        CONTINUE                                                    
         QT(J, J) = 1.                                                  
   2     CONTINUE                                                       
      K = 1                                                             
         GOTO  4                                                        
   3     K = K+1                                                        
   4     IF (K .GE. N)GOTO  8                                           
         J = N                                                          
            GOTO  6                                                     
   5        J = J-1                                                     
   6        IF (J .LE. K)GOTO  7                                        
            CALL SROTG(A(J-1, K), A(J, K), SC, SS)                      
            CALL SROT(N-K, A(J-1, K+1), N, A(J, K+1), N, SC, SS)        
            CALL SROT(N, QT(J-1, 1), N, QT(J, 1), N, SC, SS)            
            GOTO  5                                                     
   7     GOTO  3                                                        
   8  RETURN                                                            
      END                                                               
      SUBROUTINE Z1JAC(FUNC, N, X, F, DFDX, JUSED)                      
      INTEGER N                                                         
      EXTERNAL FUNC                                                     
      INTEGER JUSED                                                     
      REAL X(N), F(N), DFDX(N, N)                                       
      COMMON /Z1COM/ P2, G2, XP, XG, RATSAV, XGMAX, X2, DX2OLD, DXTRY,  
     1   RELERR, F2PREV, F2BEST, F2LAST, F2NORM, F2DMAX, JMAX, JCALL,   
     2   ISLOW, JPRINT, NUMUPD, N2BIG, SMALL, SING, GFLAG, VFLAG        
      INTEGER JMAX, JCALL, ISLOW, JPRINT, NUMUPD, N2BIG                 
      REAL P2, G2, XP, XG, RATSAV, XGMAX                                
      REAL X2, DX2OLD, DXTRY, RELERR, F2PREV, F2BEST                    
      REAL F2LAST, F2NORM, F2DMAX                                       
      LOGICAL SMALL, SING, GFLAG, VFLAG                                 
      INTEGER K, NERROR, NERR, J                                        
      REAL R1MACH, RSMALL, RELACC, RLARGE, XSAVE, DXSM                  
      REAL DXBIG, DX, DF2NRM, SNRM2, AMAX1, ABS                         
      REAL AMIN1                                                        
C NUMERICAL APPROXIMATION TO JACOBIAN OF FUNC AT X                      
C INPUTS                                                                
C N = NUMBER OF COMPONENTS                                              
C N-VECTORS X, AND F = VALUE OF FUNC AT X                               
C OUTPUTS                                                               
C JACOBIAN MATRIX DFDX                                                  
C JUSED = NUMBER OF CALLS TO FUNC USED                                  
      RSMALL = R1MACH(1)                                                
      RLARGE = R1MACH(2)                                                
      RELACC = 100.0E0*R1MACH(4)                                        
      JUSED = 0                                                         
      DO  11 K = 1, N                                                   
         DXSM = AMAX1(RSMALL, RELACC*ABS(X(K)))                         
         DXBIG = RLARGE                                                 
         XSAVE = X(K)                                                   
         DX = AMAX1(DXTRY, DXSM)                                        
   1        IF (JCALL+JUSED+N-K .LT. JMAX) GOTO 2                       
C/6S                                                                    
C              CALL SETERR(15H Z1JAC - FAILED, 15, 1, 1)                
C/7S                                                                    
               CALL SETERR(' Z1JAC - FAILED', 15, 1, 1)                 
C/                                                                      
               RETURN                                                   
   2        X(K) = XSAVE+DX                                             
            JUSED = JUSED+1                                             
            CALL FUNC(N, X, DFDX(1, K))                                 
            IF (NERROR(NERR) .EQ. 0) GOTO 3                             
               CALL ERROFF                                              
               DF2NRM = 2.0E0*F2NORM                                    
               GOTO  5                                                  
   3           DO  4 J = 1, N                                           
                  DFDX(J, K) = DFDX(J, K)-F(J)                          
   4              CONTINUE                                              
               DF2NRM = SNRM2(N, DFDX(1, K), 1)                         
C     IF (DF2NRM.LT.RELACC*F2NORM)      DX TOO SMALL                    
C        DXSM=AMAX1(DX,DXSM), DX=10.0E0*DX                              
C     ELSE                                                              
   5        IF (DF2NRM .LE. F2NORM) GOTO 6                              
               DXBIG = AMIN1(DX, DXBIG)                                 
C DX TOO LARGE                                                          
               DX = DX*AMIN1(0.5E0, AMAX1(0.01E0, 0.5E0*F2NORM/DF2NRM)) 
               GOTO  8                                                  
   6           DO  7 J = 1, N                                           
                  DFDX(J, K) = DFDX(J, K)/DX                            
   7              CONTINUE                                              
               X(K) = XSAVE                                             
               GOTO  10                                                 
   8        IF (DXBIG .GT. DXSM) GOTO 9                                 
C/6S                                                                    
C              CALL SETERR(15H Z1JAC - FAILED, 15, 2, 1)                
C/7S                                                                    
               CALL SETERR(' Z1JAC - FAILED', 15, 2, 1)                 
C/                                                                      
               RETURN                                                   
   9        DX = AMIN1(DXBIG, AMAX1(DX, DXSM))                          
            GOTO  1                                                     
  10     CONTINUE                                                       
  11     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE Z1MV(N, QT, R, X, AUX, Y)                              
      INTEGER N                                                         
      REAL QT(N, N), R(N, N), X(N), AUX(N), Y(N)                        
      INTEGER J                                                         
      REAL SDOT                                                         
C MULTIPLY MATRIX A TIMES VECTOR X = VECTOR Y                           
C N BY N MATRIX A = Q*R                                                 
C Q IS ORTHOGONAL, TRANSPOSE STORED IN QT                               
C R IS UPPER TRIANGULAR                                                 
C SCRATCH SPACE AUX                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(14H Z1MV - N.LT.1, 15, 1, 2)            
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' Z1MV - N.LT.1', 15, 1, 2)             
C/                                                                      
      DO  1 J = 1, N                                                    
         AUX(J) = SDOT(N+1-J, R(J, J), N, X(J), 1)                      
   1     CONTINUE                                                       
      DO  2 J = 1, N                                                    
         Y(J) = SDOT(N, AUX(1), 1, QT(1, J), 1)                         
   2     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE Z1SOL(N, QT, R, B, AUX, X)                             
      INTEGER N                                                         
      REAL QT(N, N), R(N, N), B(N), AUX(N), X(N)                        
      INTEGER J                                                         
      REAL BIG, R1MACH, TEMP, SDOT, ABS                                 
C SOLVE A*X=B FOR X                                                     
C N BY N MATRIX A = Q*R                                                 
C Q IS ORTHOGONAL, TRANSPOSE STORED IN QT                               
C R IS UPPER TRIANGULAR                                                 
C B IS GIVEN VECTOR                                                     
C SCRATCH SPACE AUX                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15H Z1SOL - N.LT.1, 15, 1, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' Z1SOL - N.LT.1', 15, 1, 2)            
C/                                                                      
      DO  1 J = 1, N                                                    
         AUX(J) = SDOT(N, QT(J, 1), N, B(1), 1)                         
   1     CONTINUE                                                       
      BIG = 0.5E0*R1MACH(2)                                             
      J = N                                                             
         GOTO  3                                                        
   2     J = J-1                                                        
   3     IF (J .LE. 0)GOTO  6                                           
         TEMP = AUX(J)                                                  
         IF (J .LT. N) TEMP = TEMP - SDOT(N-J, X(J+1), 1, R(J, J+1), N) 
         IF (ABS(R(J, J)) .GE. 1.0E0) GOTO 5                            
            IF (ABS(TEMP) .LE. BIG*ABS(R(J, J))) GOTO 4                 
C/6S                                                                    
C              CALL SETERR(24H Z1SOL - SINGULAR MATRIX, 24, 2, 1)       
C/7S                                                                    
               CALL SETERR(' Z1SOL - SINGULAR MATRIX', 24, 2, 1)        
C/                                                                      
               RETURN                                                   
   4     CONTINUE                                                       
   5     X(J) = TEMP/R(J, J)                                            
         GOTO  2                                                        
   6  RETURN                                                            
      END                                                               
      SUBROUTINE Z1UPD(N, QT, R, U, V, AUX)                             
      INTEGER N                                                         
      REAL QT(N, N), R(N, N), U(N), V(N), AUX(N)                        
      INTEGER J, K                                                      
      REAL SC, SS, SDOT                                                 
C RANK-ONE UPDATE OF QR FACTORIZATION                                   
C INPUT MATRIX A=Q*R                                                    
C TO BE UPDATED BY ADDING U*V-TRANSPOSE                                 
C Q IS ORTHOGONAL, TRANSPOSE STORED IN QT                               
C R IS UPPER TRIANGULAR                                                 
C SCRATCH SPACE AUX                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15H Z1UPD - N.LT.1, 15, 1, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' Z1UPD - N.LT.1', 15, 1, 2)            
C/                                                                      
      DO  1 J = 1, N                                                    
         AUX(J) = SDOT(N, QT(J, 1), N, U, 1)                            
   1     CONTINUE                                                       
      J = N                                                             
         GOTO  3                                                        
   2     J = J-1                                                        
   3     IF (J .LE. 2)GOTO  4                                           
         CALL SROTG(AUX(J-1), AUX(J), SC, SS)                           
         CALL SROT(N+2-J, R(J-1, J-1), N, R(J, J-1), N, SC, SS)         
         CALL SROT(N, QT(J-1, 1), N, QT(J, 1), N, SC, SS)               
         GOTO  2                                                        
   4  DO  5 K = 1, N                                                    
         R(1, K) = R(1, K)+AUX(1)*V(K)                                  
   5     CONTINUE                                                       
      IF (N .LE. 1) GOTO 10                                             
         DO  6 K = 1, N                                                 
            R(2, K) = R(2, K)+AUX(2)*V(K)                               
   6        CONTINUE                                                    
         J = 1                                                          
            GOTO  8                                                     
   7        J = J+1                                                     
   8        IF (J .GE. N)GOTO  9                                        
            CALL SROTG(R(J, J), R(J+1, J), SC, SS)                      
            CALL SROT(N-J, R(J, J+1), N, R(J+1, J+1), N, SC, SS)        
            CALL SROT(N, QT(J, 1), N, QT(J+1, 1), N, SC, SS)            
            GOTO  7                                                     
   9     CONTINUE                                                       
  10  RETURN                                                            
      END                                                               
      SUBROUTINE Z1VM(N, X, QT, R, AUX, Y)                              
      INTEGER N                                                         
      REAL X(N), QT(N, N), R(N, N), AUX(N), Y(N)                        
      INTEGER J                                                         
      REAL SDOT                                                         
C MULTIPLY VECTOR X-TRANSPOSE TIMES MATRIX A                            
C A = Q*R                                                               
C Q IS ORTHOGONAL, TRANSPOSE STORED IN QT                               
C R IS UPPER TRIANGULAR                                                 
C SCRATCH SPACE AUX                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(14H Z1VM - N.LT.1, 15, 1, 2)            
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' Z1VM - N.LT.1', 15, 1, 2)             
C/                                                                      
      DO  1 J = 1, N                                                    
         AUX(J) = SDOT(N, QT(J, 1), N, X(1), 1)                         
   1     CONTINUE                                                       
      DO  2 J = 1, N                                                    
         Y(J) = SDOT(J, AUX(1), 1, R(1, J), 1)                          
   2     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DZONE(FUNC, N, X, EPS, JMAX, F2NORM)                   
      INTEGER N                                                         
      EXTERNAL FUNC                                                     
      INTEGER JMAX                                                      
      DOUBLE PRECISION X(N), EPS, F2NORM                                
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      EXTERNAL DZ1JAC                                                   
      INTEGER IFF, ISTKGT, IDX, IDF, IPP, IGG                           
      INTEGER IAUX, IRMAT, IQMAT, IPRINT, KFLAG                         
      DOUBLE PRECISION R(1)                                             
      EQUIVALENCE (D(1), R(1))                                          
C SOLVE N NONLINEAR EQUATIONS F(X)=0                                    
C INPUTS                                                                
C FUNC IS SUBROUTINE, CALL FUNC(N,X,F).                                 
C IT MUST RETURN F-VECTOR, GIVEN X-VECTOR.                              
C DZ1JAC IS SUBROUTINE TO CALCULATE JACOBIAN                            
C X IS N-VECTOR, GUESS AT ANSWER.                                       
C EPS IS TOLERANCE.  SUCCESS MEANS 2-NORM OF FUNCTION .LE. EPS.         
C JMAX IS UPPER LIMIT ON NUMBER OF CALLS TO FUNC                        
C OUTPUTS                                                               
C X IS REPLACED BY NEW GUESS                                            
C F2NORM IS 2-NORM OF FUNC AT X.                                        
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(28HDZONE - N MUST BE AT LEAST 1, 28, 1  
C    1   , 2)                                                           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DZONE - N MUST BE AT LEAST 1', 28, 1   
     1   , 2)                                                           
C/                                                                      
      IFF = ISTKGT(N*(2*N+6), 4)                                        
      IDX = IFF+N                                                       
      IDF = IDX+N                                                       
      IPP = IDF+N                                                       
      IGG = IPP+N                                                       
      IAUX = IGG+N                                                      
      IRMAT = IAUX+N                                                    
      IQMAT = IRMAT+N*N                                                 
      IPRINT = 0                                                        
      CALL DZ1ONE(FUNC, DZ1JAC, N, X, EPS, JMAX, F2NORM, IPRINT, R(IFF),
     1   R(IDX), R(IDF), R(IPP), R(IGG), R(IAUX), R(IRMAT), R(IQMAT),   
     2   KFLAG)                                                         
      CALL ISTKRL(1)                                                    
      IF (KFLAG .NE. 2) GOTO 1                                          
C/6S                                                                    
C        CALL SETERR(32HDZONE - INITIAL X-VECTOR NO GOOD, 32, 2, 1)     
C/7S                                                                    
         CALL SETERR('DZONE - INITIAL X-VECTOR NO GOOD', 32, 2, 1)      
C/                                                                      
         GOTO  8                                                        
   1     IF (KFLAG .NE. 3) GOTO 2                                       
C/6S                                                                    
C           CALL SETERR(30HDZONE - TOO MANY CALLS TO FUNC, 30, 3, 1)    
C/7S                                                                    
            CALL SETERR('DZONE - TOO MANY CALLS TO FUNC', 30, 3, 1)     
C/                                                                      
            GOTO  7                                                     
   2        IF (KFLAG .NE. 4) GOTO 3                                    
C/6S                                                                    
C              CALL SETERR(28HDZONE - CONVERGENCE TOO SLOW, 28, 4, 1)   
C/7S                                                                    
               CALL SETERR('DZONE - CONVERGENCE TOO SLOW', 28, 4, 1)    
C/                                                                      
               GOTO  6                                                  
   3           IF (KFLAG .NE. 5) GOTO 4                                 
C/6S                                                                    
C                 CALL SETERR(34HDZONE - COULD NOT GET NEW JACOBIAN, 34,
C    1               5, 1)                                              
C/7S                                                                    
                  CALL SETERR('DZONE - COULD NOT GET NEW JACOBIAN', 34, 
     1               5, 1)                                              
C/                                                                      
                  GOTO  5                                               
C/6S                                                                    
C  4              IF (KFLAG .EQ. 6) CALL SETERR(                        
C    1               41HDZONE - DID NOT IMPROVE WITH NEW JACOBIAN, 41, 6
C    2               , 1)                                               
C/7S                                                                    
   4              IF (KFLAG .EQ. 6) CALL SETERR(                        
     1               'DZONE - DID NOT IMPROVE WITH NEW JACOBIAN', 41, 6 
     2               , 1)                                               
C/                                                                      
   5        CONTINUE                                                    
   6     CONTINUE                                                       
   7  CONTINUE                                                          
   8  RETURN                                                            
      END                                                               
      SUBROUTINE DZONEJ(FUNC, DZ1JAC, N, X, EPS, JMAX, F2NORM)          
      INTEGER N                                                         
      EXTERNAL FUNC, DZ1JAC                                             
      INTEGER JMAX                                                      
      DOUBLE PRECISION X(N), EPS, F2NORM                                
      COMMON /CSTAK/ D                                                  
      DOUBLE PRECISION D(500)                                           
      INTEGER IFF, ISTKGT, IDX, IDF, IPP, IGG                           
      INTEGER IAUX, IRMAT, IQMAT, IPRINT, KFLAG                         
      DOUBLE PRECISION R(1)                                             
      EQUIVALENCE (D(1), R(1))                                          
C SOLVE N NONLINEAR EQUATIONS F(X)=0                                    
C INPUTS                                                                
C FUNC IS SUBROUTINE, CALL FUNC(N,X,F).                                 
C IT MUST RETURN F-VECTOR, GIVEN X-VECTOR.                              
C DZ1JAC IS SUBROUTINE TO CALCULATE JACOBIAN                            
C X IS N-VECTOR, GUESS AT ANSWER.                                       
C EPS IS TOLERANCE.  SUCCESS MEANS 2-NORM OF FUNCTION .LE. EPS.         
C JMAX IS UPPER LIMIT ON NUMBER OF CALLS TO FUNC                        
C OUTPUTS                                                               
C X IS REPLACED BY NEW GUESS                                            
C F2NORM IS 2-NORM OF FUNC AT X.                                        
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(28HDZONEJ- N MUST BE AT LEAST 1, 28, 1  
C    1   , 2)                                                           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DZONEJ- N MUST BE AT LEAST 1', 28, 1   
     1   , 2)                                                           
C/                                                                      
      IFF = ISTKGT(N*(2*N+6), 4)                                        
      IDX = IFF+N                                                       
      IDF = IDX+N                                                       
      IPP = IDF+N                                                       
      IGG = IPP+N                                                       
      IAUX = IGG+N                                                      
      IRMAT = IAUX+N                                                    
      IQMAT = IRMAT+N*N                                                 
      IPRINT = 0                                                        
      CALL DZ1ONE(FUNC, DZ1JAC, N, X, EPS, JMAX, F2NORM, IPRINT, R(IFF),
     1   R(IDX), R(IDF), R(IPP), R(IGG), R(IAUX), R(IRMAT), R(IQMAT),   
     2   KFLAG)                                                         
      CALL ISTKRL(1)                                                    
      IF (KFLAG .NE. 2) GOTO 1                                          
C/6S                                                                    
C        CALL SETERR(32HDZONEJ- INITIAL X-VECTOR NO GOOD, 32, 2, 1)     
C/7S                                                                    
         CALL SETERR('DZONEJ- INITIAL X-VECTOR NO GOOD', 32, 2, 1)      
C/                                                                      
         GOTO  8                                                        
   1     IF (KFLAG .NE. 3) GOTO 2                                       
C/6S                                                                    
C           CALL SETERR(30HDZONEJ- TOO MANY CALLS TO FUNC, 30, 3, 1)    
C/7S                                                                    
            CALL SETERR('DZONEJ- TOO MANY CALLS TO FUNC', 30, 3, 1)     
C/                                                                      
            GOTO  7                                                     
   2        IF (KFLAG .NE. 4) GOTO 3                                    
C/6S                                                                    
C              CALL SETERR(28HDZONEJ- CONVERGENCE TOO SLOW, 28, 4, 1)   
C/7S                                                                    
               CALL SETERR('DZONEJ- CONVERGENCE TOO SLOW', 28, 4, 1)    
C/                                                                      
               GOTO  6                                                  
   3           IF (KFLAG .NE. 5) GOTO 4                                 
C/6S                                                                    
C                 CALL SETERR(34HDZONEJ- COULD NOT GET NEW JACOBIAN, 34,
C    1               5, 1)                                              
C/7S                                                                    
                  CALL SETERR('DZONEJ- COULD NOT GET NEW JACOBIAN', 34, 
     1               5, 1)                                              
C/                                                                      
                  GOTO  5                                               
C/6S                                                                    
C  4              IF (KFLAG .EQ. 6) CALL SETERR(                        
C    1               41HDZONEJ- DID NOT IMPROVE WITH NEW JACOBIAN, 41, 6
C    2               , 1)                                               
C/7S                                                                    
   4              IF (KFLAG .EQ. 6) CALL SETERR(                        
     1               'DZONEJ- DID NOT IMPROVE WITH NEW JACOBIAN', 41, 6 
     2               , 1)                                               
C/                                                                      
   5        CONTINUE                                                    
   6     CONTINUE                                                       
   7  CONTINUE                                                          
   8  RETURN                                                            
      END                                                               
      SUBROUTINE DZ1ONE(FUNC, DZ1JAC, N, X, EPS, JJMAX, F2OUT,          
     1   IPRINT, F, DX, DF, P, G, AUX, R, QT, KFLAG)                    
      INTEGER N                                                         
      EXTERNAL FUNC, DZ1JAC                                             
      INTEGER JJMAX, IPRINT, KFLAG                                      
      DOUBLE PRECISION X(N), EPS, F2OUT, F(N), DX(N), DF(N)             
      DOUBLE PRECISION P(N), G(N), AUX(N), R(N, N), QT(N, N)            
      COMMON /DZ1COM/ P2, G2, XP, XG, RATSAV, XGMAX, X2, DX2OLD, DXTRY  
     1   , RELERR, F2PREV, F2BEST, F2LAST, F2NORM, F2DMAX, SMALL, SING  
     2   , GFLAG, VFLAG, JMAX, JCALL, ISLOW, JPRINT, NUMUPD, N2BIG      
      INTEGER JMAX, JCALL, ISLOW, JPRINT, NUMUPD, N2BIG                 
      LOGICAL SMALL, SING, GFLAG, VFLAG                                 
      DOUBLE PRECISION P2, G2, XP, XG, RATSAV, XGMAX                    
      DOUBLE PRECISION X2, DX2OLD, DXTRY, RELERR, F2PREV, F2BEST        
      DOUBLE PRECISION F2LAST, F2NORM, F2DMAX                           
      INTEGER ISMAX, IROLD, MAXSD, MIN0, NERROR, NERR                   
      INTEGER MODE, JUSED, MAX0, J1IMPR, J2IMPR, J                      
      REAL FLOAT                                                        
      LOGICAL NEWJAC, LAST, FIRST, NOGOOD                               
      DOUBLE PRECISION DNRM2, D1MACH, DZ1CON, DZ1DOT, COND, CONMAX      
      DOUBLE PRECISION DX2, G2OLD, F2IMPR, PMIN, RATE1, RATE2           
      DOUBLE PRECISION TEMP, COS, XPBIG, DSQRT                          
      DATA ISMAX/3/                                                     
      DATA RATE1/0.98D0/                                                
      DATA RATE2/0.95D0/                                                
      DATA PMIN/1.D-4/                                                  
C SOLVE N NONLINEAR EQUATIONS F(X)=0                                    
C INPUTS                                                                
C FUNC IS SUBROUTINE, CALL FUNC(N,X,F).                                 
C IT MUST RETURN F-VECTOR, GIVEN X-VECTOR.                              
C DZ1JAC IS SUBROUTINE, CALL DZ1JAC(FUNC,N,X,F,DFDX,J).                 
C X IS N-VECTOR, GUESS AT ANSWER.                                       
C EPS IS TOLERANCE.  SUCCESS MEANS 2-NORM OF FUNCTION .LE. EPS.         
C JJMAX IS UPPER LIMIT ON NUMBER OF CALLS TO FUNC                       
C IPRINT = PRINTING LEVEL                                               
C F,DX,DF,P,G,AUX,R,QT ARE SCRATCH ARRAYS                               
C OUTPUTS                                                               
C NEW X                                                                 
C F2OUT = 2-NORM OF FUNC AT X                                           
C F = FUNC-VECTOR AT X                                                  
C KFLAG=1   SUCCESSFUL                                                  
C       2   INITIAL X-VECTOR NO GOOD                                    
C       3   TOO MANY CALLS TO FUNC                                      
C       4   CONVERGENCE TOO SLOW                                        
C       5   COULD NOT GET NEW JACOBIAN                                  
C       6   DID NOT IMPROVE WITH NEW JACOBIAN                           
      CALL ENTSRC(IROLD, 1)                                             
      JMAX = JJMAX                                                      
      MAXSD = MIN0(3*N, 15)                                             
      JPRINT = IPRINT                                                   
      KFLAG = 0                                                         
      CALL FUNC(N, X, F)                                                
      JCALL = 1                                                         
      IF (NERROR(NERR) .EQ. 0) F2NORM = DNRM2(N, F, 1)                  
      IF (NERROR(NERR) .EQ. 0) GOTO 1                                   
         CALL ERROFF                                                    
         KFLAG = 2                                                      
         F2OUT = D1MACH(2)                                              
         RETURN                                                         
   1  IF (F2NORM .GT. EPS) GOTO 2                                       
         F2OUT = F2NORM                                                 
         RETURN                                                         
   2  RELERR = FLOAT(2*N)*D1MACH(4)                                     
      CONMAX = DMIN1(1.D4, 0.01/D1MACH(4))                              
      F2BEST = F2NORM                                                   
      F2PREV = 2.0D0*F2BEST                                             
      DX2OLD = 0.0D0                                                    
      XG = 1.0D0                                                        
      MODE = 2                                                          
      LAST = .FALSE.                                                    
      SMALL = .FALSE.                                                   
C GET NEW JACOBIAN MATRIX R AND CONTINUE                                
   3     IF (F2BEST .LE. RATE2*F2PREV) GOTO 5                           
            ISLOW = ISLOW+1                                             
C SLOW CONVERGENCE                                                      
            IF (ISLOW .LE. ISMAX) GOTO 4                                
               KFLAG = 4                                                
               GOTO  26                                                 
   4        LAST = LAST .OR. ISLOW .EQ. ISMAX .OR. F2DMAX .LT. 0.3D0    
            GFLAG = XPBIG .LE. 0.0D0                                    
            GOTO  6                                                     
   5        ISLOW = 0                                                   
C ACCEPTABLE CONVERGENCE                                                
            GFLAG = .FALSE.                                             
   6     X2 = DNRM2(N, X, 1)                                            
         DXTRY = DSQRT(D1MACH(4))*DMAX1(1.0D0, X2)                      
C        *** G2 IS NOT DEFINED WHEN JCALL .LE. 1 ***                    
         IF (JCALL .LE. 1) GO TO 61                                     
            IF (G2 .GT. 0.0D0) DXTRY = DMIN1(DXTRY, G2)                 
   61    JUSED = 0                                                      
         CALL DZ1JAC(FUNC, N, X, F, R, JUSED)                           
         JCALL = JCALL+MAX0(0, JUSED)                                   
         IF (NERROR(NERR) .EQ. 0) GOTO 7                                
            CALL ERROFF                                                 
            KFLAG = 5                                                   
            GOTO  26                                                    
   7     NUMUPD = 0                                                     
         J1IMPR = 0                                                     
         J2IMPR = 0                                                     
         F2IMPR = F2BEST                                                
         F2PREV = F2BEST                                                
         F2DMAX = 0.0D0                                                 
         FIRST = .TRUE.                                                 
         VFLAG = .FALSE.                                                
         XPBIG = 0.0D0                                                  
         RATSAV = 1.0D0                                                 
         N2BIG = 0                                                      
C QR FACTORIZATION OF JACOBIAN                                          
         CALL DZ1FAC(N, R, QT)                                          
C LOWER BOUND ON CONDITION NUMBER                                       
         COND = DZ1CON(N, R)                                            
         IF (.NOT. LAST) GOTO 8                                         
            SING = COND .GT. 0.5D0*D1MACH(2)                            
            GOTO  9                                                     
   8        SING = COND .GT. CONMAX                                     
   9     CONTINUE                                                       
C USE CURRENT JACOBIAN APPROXIMATION AS LONG AS IT WORKS.               
C GET P=NEWTON STEP AND G=STEEPEST-DESCENT STEP                         
            CALL DZ1PG(N, F, F2NORM, QT, R, P, G, P2, G2, SING, AUX)    
            IF ((.NOT. FIRST) .OR. G2 .GT. RELERR*X2) GOTO 11           
               IF (G2 .GT. 0.0D0 .AND. ((.NOT. SMALL) .OR. (.NOT. SING))
     1            ) GOTO 10                                             
                  KFLAG = 6                                             
                  GOTO  24                                              
  10        CONTINUE                                                    
  11        IF (FIRST) GOTO 14                                          
               COS = DZ1DOT(N, G, G2, DX, G2OLD)                        
               IF (MODE .EQ. 2 .AND. COS .GT. DSQRT(1.0D0-1.0D0/FLOAT(N)
     1            )) GOTO  24                                           
C AND XG.GE.1.0D0                                                       
               VFLAG = VFLAG .AND. COS .LT. 0.3D0 .AND. COS .GT. (-     
     1            0.9D0) .AND. NUMUPD .LT. 3 .AND. G2 .GT. 0.25D0*G2OLD 
     2             .AND. F2DMAX .LT. 0.3D0 .AND. F2NORM .GT. RATE1*     
     3            F2BEST                                                
               IF (.NOT. VFLAG) GOTO 13                                 
                  DO  12 J = 1, N                                       
C AVERAGE OF TWO GS MAY BE DOWN THE VALLEY                              
                     G2 = 0.5D0*XG                                      
                     TEMP = G2*(G(J)+DX(J))                             
                     G(J) = G2*(G(J)-DX(J))                             
                     P(J) = TEMP                                        
  12                 CONTINUE                                           
                  G2 = DNRM2(N, G, 1)                                   
                  P2 = DNRM2(N, P, 1)                                   
  13           CONTINUE                                                 
C GET ACCEPTABLE NEW VALUES OF X, F                                     
  14        CALL DZ1NEW(FUNC, N, X, EPS, F, DX, DF, AUX, P, G, DX2,     
     1         PMIN, ISMAX, MODE, NEWJAC, NOGOOD)                       
            IF (F2NORM .GT. EPS) GOTO 15                                
               KFLAG = 1                                                
               GOTO  24                                                 
  15        XPBIG = DMAX1(XP, XPBIG)                                    
            IF (JCALL .LT. JMAX) GOTO 16                                
               KFLAG = 3                                                
               GOTO  24                                                 
  16        IF ((.NOT. FIRST) .OR. (.NOT. NEWJAC) .OR. (.NOT. NOGOOD))  
     1          GOTO 17                                                 
               KFLAG = 6                                                
               GOTO  24                                                 
  17        J1IMPR = J1IMPR+1                                           
            IF (F2NORM .GE. F2BEST) GOTO 18                             
               IF (F2NORM .LT. RATE1*F2BEST) J1IMPR = 0                 
               F2BEST = F2NORM                                          
  18        J2IMPR = J2IMPR+1                                           
            IF (F2NORM .GE. RATE2*F2IMPR) GOTO 19                       
               F2IMPR = F2NORM                                          
               J2IMPR = 0                                               
  19        IF (J2IMPR .GT. 4 .OR. NEWJAC .OR. J1IMPR .GT. 4 .OR. SMALL 
     1          .AND. (.NOT. FIRST) .OR. XPBIG .LE. 0.0D0 .AND. NUMUPD  
     2          .GT. MAXSD) GOTO  24                                    
            IF (.NOT. SMALL) GOTO 20                                    
               SING = COND .GT. 0.5D0*D1MACH(2)                         
               IF (SING) GOTO  24                                       
               GOTO  22                                                 
  20           CALL DZ1MV(N, QT, R, DX, AUX, P)                         
C RANK-ONE UPDATE TO APPROXIMATE JACOBIAN                               
C P=Q*R*DX                                                              
               DO  21 J = 1, N                                          
                  DF(J) = (DF(J)-P(J))/DX2                              
                  DX(J) = DX(J)/DX2                                     
  21              CONTINUE                                              
               CALL DZ1UPD(N, QT, R, DF, DX, AUX)                       
               COND = DZ1CON(N, R)                                      
               SING = COND .GT. CONMAX                                  
               FIRST = .FALSE.                                          
               NUMUPD = NUMUPD+1                                        
  22        VFLAG = (.NOT. VFLAG) .AND. (XP .LE. 0.01D0 .AND. F2NORM    
     1          .GT. RATE2*F2LAST)                                      
C SAVE G FOR NEXT TIME                                                  
            DO  23 J = 1, N                                             
               DX(J) = G(J)                                             
  23           CONTINUE                                                 
            G2OLD = G2                                                  
            GOTO  9                                                     
  24     IF (KFLAG .EQ. 0) GOTO 25                                      
            IF (LAST .OR. KFLAG .NE. 4) GOTO  26                        
            LAST = .TRUE.                                               
            KFLAG = 0                                                   
  25     CONTINUE                                                       
         GOTO  3                                                        
  26  F2OUT = F2NORM                                                    
      CALL RETSRC(IROLD)                                                
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DZ1CON(N, R)                            
      INTEGER N                                                         
      DOUBLE PRECISION R(N, N)                                          
      INTEGER I, NP, J                                                  
      DOUBLE PRECISION D1MACH, RMIN, RMAX                               
C LOWER BOUND ON CONDITION NUMBER OF UPPER TRIANGULAR MATRIX R.         
      RMAX = DABS(R(N, N))                                              
      RMIN = RMAX                                                       
      I = N-1                                                           
         GOTO  2                                                        
   1     I = I-1                                                        
   2     IF (I .LT. 1)GOTO  4                                           
         NP = N+1                                                       
         DO  3 J = NP, N                                                
            RMAX = DMAX1(RMAX, DABS(R(I, J)))                           
   3        CONTINUE                                                    
         RMIN = DMIN1(RMIN, DABS(R(I, I)))                              
         GOTO  1                                                        
   4  IF (RMIN .GE. 1.0D0 .AND. RMAX .GT. 0.0D0) GOTO 6                 
         IF (RMAX .LT. 0.5D0*D1MACH(2)*RMIN) GOTO 5                     
            DZ1CON = D1MACH(2)                                          
            RETURN                                                      
   5  CONTINUE                                                          
   6  DZ1CON = RMAX/RMIN                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE DZ1NEW(FUNC, N, X, EPS, F, XNEW, FNEW, FSAV, P, G      
     1   , DX2, PMIN, ISMAX, MODE, NEWJAC, NOGOOD)                      
      INTEGER N                                                         
      EXTERNAL FUNC                                                     
      INTEGER ISMAX, MODE                                               
      LOGICAL NEWJAC, NOGOOD                                            
      DOUBLE PRECISION X(N), EPS, F(N), XNEW(N), FNEW(N), FSAV(N)       
      DOUBLE PRECISION P(N), G(N), DX2, PMIN                            
      COMMON /DZ1COM/ P2, G2, XP, XG, RATSAV, XGMAX, X2, DX2OLD, DXTRY  
     1   , RELERR, F2PREV, F2BEST, F2LAST, F2NORM, F2DMAX, SMALL, SING  
     2   , GFLAG, VFLAG, JMAX, JCALL, ISLOW, JPRINT, NUMUPD, N2BIG      
      INTEGER JMAX, JCALL, ISLOW, JPRINT, NUMUPD, N2BIG                 
      LOGICAL SMALL, SING, GFLAG, VFLAG                                 
      DOUBLE PRECISION P2, G2, XP, XG, RATSAV, XGMAX                    
      DOUBLE PRECISION X2, DX2OLD, DXTRY, RELERR, F2PREV, F2BEST        
      DOUBLE PRECISION F2LAST, F2NORM, F2DMAX                           
      INTEGER LASTM, NTMAX, NT, J, NERROR, NERR                         
      INTEGER I1SGN, I2SGN, MIN0                                        
      REAL FLOAT                                                        
      LOGICAL STEEP, TOOFAR, FIRST, OKAY3, LTEMP                        
      DOUBLE PRECISION DNRM2, DZ1DEL, DDOT, DZ1DOT, DELTA, DX2SAV       
      DOUBLE PRECISION F2GOAL, F2NEW, F2SAV, RATOLD, RATIO, RHO         
      DOUBLE PRECISION TEMP, XGSAV, XPSAV, FW, FWORSE, PG               
      DOUBLE PRECISION F2IN, F2DEL, CONVR, COS                          
      DOUBLE PRECISION DSQRT                                            
      DATA FW/1.5D0/                                                    
      DATA RHO/0.7D0/                                                   
C OBTAIN NEW X-VECTOR AND F-VECTOR                                      
C INPUTS                                                                
C FUNC IS SUBROUTINE, CALL FUNC(N,X,F).                                 
C IT MUST RETURN F-VECTOR, GIVEN X-VECTOR.                              
C X IS CURRENT X-VECTOR                                                 
C EPS IS TOLERANCE.  SUCCESS MEANS 2-NORM OF FUNCTION .LE. EPS.         
C F IS N-VECTOR OF VALUE OF FUNC AT X                                   
C XNEW,FNEW,FSAV ARE SCRATCH ARRAYS                                     
C IF (VFLAG) DO VALLEY SEARCH                                           
C    P IS N-VECTOR, STEP ALONG VALLEY                                   
C    G IS N-VECTOR, STEP INTO VALLEY                                    
C IF ((NOT)VFLAG) DOGLEG STRATEGY                                       
C    P IS N-VECTOR, NEWTON STEP                                         
C    G IS N-VECTOR, STEEPEST-DESCENT STEP                               
C PMIN IS MINIMUM ALLOWABLE FRACTION OF P IN DX STEP                    
C ISMAX IS MAXIMUM VALUE OF ISLOW                                       
C OUTPUTS                                                               
C IF SUCCESSFUL                                                         
C    NEW X AND F-VECTORS                                                
C    XNEW, FNEW HAVE DX, DF-VECTORS                                     
C    DX2 IS LENGTH OF DX-VECTOR                                         
C IF UNSUCCESSFUL                                                       
C    X AND F-VECTORS ARE UNCHANGED                                      
C MODE = STRATEGY MODE USED                                             
C      = 1, STANDARD HYBRID                                             
C      = 2, GRADIENT SEARCH                                             
C      = 3, HYBRID SEARCH                                               
C      = 4, VALLEY SEARCH                                               
C NEWJAC = (DO NOT WANT TO USE OLD JACOBIAN ANY MORE)                   
C NOGOOD = (DID NOT GET NEW F THAT WAS ACCEPTABLE)                      
      NEWJAC = .FALSE.                                                  
      NOGOOD = .FALSE.                                                  
      LTEMP = .FALSE.                                                   
      FIRST = NUMUPD .EQ. 0                                             
      IF (.NOT. FIRST) GOTO 1                                           
         XG = DMIN1(XG, 1.0D0)                                          
         DELTA = DMAX1(XG*G2, DX2OLD)                                   
         GOTO  2                                                        
   1     DELTA = DZ1DEL(DX2OLD, XP, P2, G2, PMIN, F2NORM, F2LAST,       
     1      F2BEST)                                                     
COKAY3=NUMUPD.LT.3 AND DELTA.GE.PMIN*P2      AND XG.GE.1.0D0            
C  AND XG.GE.1.0D0                                                      
   2  OKAY3 = FIRST .AND. DELTA .GE. PMIN*P2                            
      LASTM = MODE                                                      
      MODE = 1                                                          
      NTMAX = 2                                                         
      IF (FIRST) NTMAX = 15                                             
C  IF (DELTA.LT.0.1D0*P2)      SEARCH ALONG STEEPEST-DESCENT            
C     MODE=2, NTMAX=2*N, DELTA=G2                                       
      IF ((.NOT. FIRST) .OR. ISLOW .NE. ISMAX .AND. (.NOT. GFLAG) .OR. (
     1   .NOT. OKAY3)) GOTO 3                                           
         MODE = 3                                                       
C DOGLEG SEARCH                                                         
         NTMAX = 15                                                     
   3  IF (.NOT. VFLAG) GOTO 6                                           
         MODE = 4                                                       
         PG = DDOT(N, P, 1, G, 1)                                       
         IF (P2 .LE. 0.1D0*G2) GOTO 4                                   
            NTMAX = 15                                                  
            GOTO  5                                                     
   4        NTMAX = 2                                                   
   5     CONTINUE                                                       
   6  F2IN = F2NORM                                                     
      NT = 1                                                            
         GOTO  8                                                        
   7     NT = NT+1                                                      
   8     IF (NT .GT. NTMAX)GOTO  58                                     
C USING SAME P, AND G,                                                  
C LOOK FOR ACCEPTABLE XNEW.                                             
         IF (.NOT. FIRST) GOTO 9                                        
            STEEP = DELTA .LT. 0.05D0*P2 .AND. MODE .NE. 3 .OR. LASTM   
     1          .EQ. 2 .AND. ISLOW .EQ. 0                               
            GOTO  10                                                    
   9        STEEP = DELTA .LE. G2 .AND. DELTA .LT. 0.05D0*P2 .OR. XP    
     1          .LE. 0.0D0 .AND. DELTA .LT. 0.05D0*P2 .AND. F2NORM      
     2          .LT. RHO*F2LAST .AND. G2 .GT. 0.25D0*DX2OLD             
  10     IF (STEEP .OR. DELTA .LT. PMIN*P2) DELTA = DMIN1(DELTA, G2)    
C GET XP AND XG, DX=XP*P+XG*G                                           
         IF (MODE .NE. 2) GOTO 11                                       
            XP = 0.0D0                                                  
            XG = NT                                                     
            STEEP = .TRUE.                                              
            DX2 = XG*G2                                                 
            GOTO  16                                                    
  11        IF (MODE .NE. 4) GOTO 14                                    
               XG = 1.0D0                                               
               IF (NT .NE. 1) GOTO 12                                   
                  XP = 2.0D0                                            
                  GOTO  13                                              
  12              XP = 2.0D0*XP                                         
                  IF (F2DEL .LT. 0.5D0) XP = 0.5D0*XP/DMAX1(0.1D0,      
     1               F2DEL)                                             
  13           DX2 = DSQRT(G2**2+2.0D0*XP*PG+(XP*P2)**2)                
               GOTO  15                                                 
  14           CALL DZ1XPG(N, STEEP, DELTA, P, P2, PMIN, G, G2, XP, XG  
     1            , DX2)                                                
  15     CONTINUE                                                       
  16     IF (DX2 .GE. RELERR*X2) GOTO 23                                
            IF (.NOT. SMALL) GOTO 21                                    
               IF ((.NOT. SING) .AND. FIRST) GOTO 17                    
                  NEWJAC = .TRUE.                                       
                  GOTO  20                                              
  17              MODE = 3                                              
                  NTMAX = 15                                            
                  XG = 1.0D0                                            
                  PG = DDOT(N, P, 1, G, 1)                              
  18              IF ((.NOT. SMALL) .OR. NT .GE. NTMAX)GOTO  19         
                     XP = PMIN*2.0D0**(NT-1)                            
                     NT = NT+1                                          
                     DX2 = DSQRT(G2**2+2.0D0*XP*PMIN*PG+(XP*PMIN*P2)**2)
                     SMALL = DX2 .LT. RELERR*X2                         
                     GOTO  18                                           
  19              CONTINUE                                              
  20           CONTINUE                                                 
               GOTO  22                                                 
  21           SMALL = .TRUE.                                           
  22        CONTINUE                                                    
            GOTO  24                                                    
  23        SMALL = .FALSE.                                             
  24     DO  25 J = 1, N                                                
            XNEW(J) = X(J)+XP*P(J)+XG*G(J)                              
  25        CONTINUE                                                    
         JCALL = JCALL+1                                                
CGET NEW F                                                              
         CALL FUNC(N, XNEW, FNEW)                                       
         IF (NERROR(NERR) .NE. 0) GOTO 26                               
            F2NEW = DNRM2(N, FNEW, 1)                                   
            F2LAST = F2NORM                                             
  26     IF (NERROR(NERR) .EQ. 0) GOTO 27                               
            CALL ERROFF                                                 
            F2NEW = 4.0*F2BEST                                          
  27     IF (NT .GT. 1) RATOLD = RATIO                                  
         RATIO = F2NEW/F2NORM                                           
         IF (RATIO .LE. 1.0D0) GOTO 30                                  
            IF (.NOT. FIRST) GOTO 28                                    
               N2BIG = 1                                                
               GOTO  29                                                 
  28           N2BIG = N2BIG+1                                          
  29     CONTINUE                                                       
  30     IF (N2BIG .LT. 4) GOTO 31                                      
            FWORSE = 1.0D0                                              
            GOTO  32                                                    
  31        FWORSE = FW*F2BEST/F2IN                                     
  32     F2DEL = DMIN1(10.0D0*F2IN, F2NEW)/F2IN                         
         IF (F2NEW .LE. 0.) GOTO 33                                     
            COS = DZ1DOT(N, F, F2IN, FNEW, F2NEW)                       
            F2DEL = F2DEL*(F2DEL-2.0D0*COS)+1.0D0                       
            GOTO  34                                                    
  33        COS = 0.0D0                                                 
            F2DEL = 1.0D0                                               
  34     F2DEL = DSQRT(DMAX1(0.0D0, F2DEL))                             
C SEE IF TENTATIVE MODE IS REASONABLE                                   
         TOOFAR = .FALSE.                                               
         IF (F2NEW .GT. EPS) GOTO 35                                    
            MODE = 1                                                    
            GOTO  58                                                    
  35     IF (NT .NE. 1) GOTO 39                                         
            IF (XP .GT. 0.0D0 .OR. RATIO .GT. 0.4D0) GOTO 36            
               MODE = 2                                                 
               NTMAX = 2*N                                              
               CONVR = (F2IN/F2NEW)**(1.0D0/FLOAT(N+1))-1.0D0           
  36        IF ((MODE .NE. 4 .OR. RATIO .LE. FWORSE) .AND. (MODE .NE. 3 
     1          .OR. RATIO .LE. 1.0D0)) GOTO 37                         
               MODE = 1                                                 
               NTMAX = 15                                               
  37        IF (RATIO .GE. 1.0D0 .OR. (.NOT. OKAY3) .OR. RATIO .LE.     
     1         0.98D0 .OR. F2DEL .GE. 0.1D0 .OR. MODE .EQ. 4) GOTO 38   
               MODE = 3                                                 
               NTMAX = 15                                               
C     IF (FIRST AND XP.LE.0.0D0 AND RATIO.GT.1.0D0) NEWJAC=.TRUE.       
  38        CONTINUE                                                    
C DECIDE ON ACCEPTABILITY OF XNEW AND FNEW                              
  39     IF (MODE .NE. 2) GOTO 45                                       
            IF (NT .NE. 1) GOTO 40                                      
               I1SGN = 0                                                
               GOTO  43                                                 
  40           IF (RATIO .LE. RATOLD) GOTO 41                           
                  I2SGN = 1                                             
                  GOTO  42                                              
  41              I2SGN = -1                                            
  42        CONTINUE                                                    
C  COS .LE. 0.7                                                         
  43        TOOFAR = F2DEL .GT. 1.0D0 .OR. RATIO .GT. 1.0D0             
            IF (I1SGN*I2SGN .LT. 0) NTMAX = NT                          
            IF (NT .LE. 1 .OR. TOOFAR) GOTO 44                          
               TEMP = (F2IN/F2NEW)**(1.0D0/FLOAT(N+NT))-1.0D0           
               TOOFAR = TEMP .LT. 1.05D0*CONVR                          
               CONVR = TEMP                                             
               I1SGN = I2SGN                                            
  44        CONTINUE                                                    
  45     IF (MODE .EQ. 3) TOOFAR = F2NEW .GT. FWORSE*F2IN               
         IF (MODE .NE. 4) GOTO 48                                       
            IF (F2NORM .GT. 0.9D0*F2PREV) GOTO 46                       
               TOOFAR = F2NEW .GT. F2NORM                               
               GOTO  47                                                 
  46           TOOFAR = F2NEW .GT. FWORSE*F2IN                          
  47        TOOFAR = TOOFAR .OR. NT .EQ. 1 .AND. RATIO .GT. 0.5D0*      
     1         RATSAV+0.5D0                                             
            IF (F2DEL .GT. 0.7) NTMAX = NT                              
  48     IF (MODE .NE. 1) TOOFAR = TOOFAR .OR. F2DEL .GT. 1.0D0         
         IF (NT .EQ. 1 .AND. TOOFAR) MODE = 1                           
         IF (MODE .NE. 1) GOTO 51                                       
            F2GOAL = FWORSE*F2IN                                        
            IF (F2BEST .LT. 0.1D0*F2PREV .OR. NT .EQ. 1 .AND. XP .LE.   
     1         0.0D0) F2GOAL = F2IN                                     
            TOOFAR = F2NEW .GT. DMAX1(EPS, F2GOAL)                      
            IF (.NOT. TOOFAR) GOTO 49                                   
               DELTA = DMAX1(0.25D0, DMIN1(0.5D0, F2IN/F2NEW))*DX2      
               GOTO  50                                                 
  49           F2DMAX = DMAX1(F2DMAX, F2DEL)                            
               NTMAX = NT                                               
  50        CONTINUE                                                    
            GOTO  57                                                    
  51        IF (.NOT. TOOFAR) GOTO 54                                   
               IF (MODE .NE. 3 .OR. (.NOT. OKAY3) .OR. DX2 .LE. 3.0*    
     1            DX2OLD .OR. NT .LE. 1) GOTO 52                        
                  DELTA = DSQRT(DX2*DX2SAV)                             
                  OKAY3 = .FALSE.                                       
                  LTEMP = .TRUE.                                        
                  TOOFAR = .FALSE.                                      
                  NTMAX = MIN0(NT+1, NTMAX)                             
                  GOTO  53                                              
  52              GOTO  58                                              
  53           CONTINUE                                                 
  54        IF (LTEMP) GOTO  7                                          
C 2-LEVEL NEXT                                                          
            XPSAV = XP                                                  
            XGSAV = XG                                                  
            F2SAV = F2NEW                                               
            F2NORM = F2NEW                                              
            DX2SAV = DX2                                                
            F2DMAX = DMAX1(F2DMAX, F2DEL)                               
            DO  55 J = 1, N                                             
               FSAV(J) = FNEW(J)                                        
  55           CONTINUE                                                 
            IF (MODE .NE. 3) GOTO 56                                    
               IF (XP .GE. 0.99D0) GOTO  58                             
               DELTA = DMAX1(2.0D0*DX2, RELERR*X2)                      
               IF (F2DEL .LT. 0.5D0) DELTA = 0.5D0*DELTA/DMAX1(0.1D0,   
     1            F2DEL)                                                
               IF (.NOT. SING) DELTA = DMAX1(DELTA, PMIN*P2)            
  56        CONTINUE                                                    
  57     IF (JCALL .GE. JMAX .OR. N2BIG .GE. 5 .OR. SMALL .AND. RATIO   
     1       .GT. 0.98D0) GOTO  58                                      
         GOTO  7                                                        
C END OF  FOR -LOOP ON NT                                               
  58  IF (MODE .EQ. 1 .OR. (.NOT. TOOFAR)) GOTO 60                      
         XP = XPSAV                                                     
C RETURN SUCCESSFUL VALUES SAVED EARLIER                                
         XG = XGSAV                                                     
         DX2 = DX2SAV                                                   
         F2NORM = F2SAV                                                 
         DO  59 J = 1, N                                                
            XNEW(J) = XP*P(J)+XG*G(J)                                   
            X(J) = X(J)+XNEW(J)                                         
            FNEW(J) = FSAV(J)-F(J)                                      
            F(J) = FSAV(J)                                              
  59        CONTINUE                                                    
         IF (NT .EQ. 2) MODE = 1                                        
         GOTO  64                                                       
  60     IF (TOOFAR) GOTO 62                                            
            F2NORM = F2NEW                                              
C SUCCESSFUL MODE 1, OR MODE 2 OR 3, AND FELL                           
C OUT BOTTOM OF FOR-LOOP                                                
            DO  61 J = 1, N                                             
               TEMP = XNEW(J)                                           
               XNEW(J) = XNEW(J)-X(J)                                   
               X(J) = TEMP                                              
               TEMP = FNEW(J)                                           
               FNEW(J) = FNEW(J)-F(J)                                   
               F(J) = TEMP                                              
  61           CONTINUE                                                 
            GOTO  63                                                    
  62        NEWJAC = .TRUE.                                             
C FAILED                                                                
            NOGOOD = .TRUE.                                             
  63  CONTINUE                                                          
  64  IF (MODE .NE. 2 .AND. MODE .NE. 4 .OR. NT .LE. 3) GOTO 65         
         DX2OLD = 0.0D0                                                 
         NEWJAC = .TRUE.                                                
         GOTO  66                                                       
  65     DX2OLD = DX2                                                   
  66  NEWJAC = NEWJAC .OR. N2BIG .GE. 5                                 
      IF (F2NORM .LT. F2IN) RATSAV = F2NORM/F2IN                        
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DZ1DEL(DX2, XP, P2, G2, PMIN,           
     1   F2NORM, F2LAST, F2BEST)                                        
      DOUBLE PRECISION DX2, XP, P2, G2, PMIN, F2NORM                    
      DOUBLE PRECISION F2LAST, F2BEST                                   
      DOUBLE PRECISION XINCR, RATE                                      
      DATA XINCR/1.5D0/                                                 
      DATA RATE/0.7D0/                                                  
C GET NEW GUESS AT REASONABLE STEP LENGTH                               
C INPUTS                                                                
C DX2 = PREVIOUS STEP LENGTH                                            
C XP = FRACTION OF NEWTON STEP USED IN PREVIOUS STEP                    
C P2 = NEW NEWTON STEP LENGTH                                           
C G2 = NEW STEEPEST-DESCENT STEP LENGTH                                 
C PMIN = MINIMUM ALLOWABLE XP                                           
C F2NORM = CURRENT 2-NORM OF FUNCTION                                   
C F2LAST = PREVIOUS 2-NORM                                              
C F2BEST = BEST-EVER 2-NORM                                             
C OUTPUT                                                                
C NEW STEP LENGTH DZ1DEL                                                
      IF (XP .LE. 0.0D0) GOTO 7                                         
         IF (XP .LT. 1.0D0 .OR. F2NORM .GE. F2LAST) GOTO 1              
            DZ1DEL = DX2*DMIN1(XINCR, (XINCR-1.0D0)*(F2NORM/F2LAST-     
     1         1.0D0)/(RATE-1.0D0)+1.0D0)                               
C LAST STEP HAD SOME NEWTON                                             
C SUCCESSFUL FULL NEWTON STEP                                           
            GOTO  6                                                     
   1        IF (F2NORM .LT. F2LAST) GOTO 2                              
               DZ1DEL = DMAX1(0.25D0, F2BEST/F2NORM)*DX2                
               IF (F2NORM .GT. F2LAST*(RATE*XP+1.0D0)) DZ1DEL = DMIN1(  
     1            DZ1DEL, RATE*DX2)                                     
               GOTO  5                                                  
   2           DZ1DEL = DX2*DMIN1(XINCR, (XINCR-1.0D0)*(F2NORM/F2LAST-  
     1            1.0D0)/(RATE-1.0D0)+1.0D0)                            
               IF (F2NORM .LE. F2BEST) GOTO 3                           
                  DZ1DEL = DMIN1(DZ1DEL, DX2)                           
                  GOTO  4                                               
   3              DZ1DEL = DMAX1(DZ1DEL, DX2)                           
   4           CONTINUE                                                 
   5     CONTINUE                                                       
   6     CONTINUE                                                       
         GOTO  8                                                        
   7     DZ1DEL = DX2*DMIN1(XINCR, (XINCR-1.0D0)*(F2NORM/F2LAST-1.0D0)/(
     1      RATE-1.0D0)+1.0D0)                                          
C LAST WAS STEEPEST-DESCENT                                             
         IF (F2NORM .LT. RATE*F2LAST .AND. PMIN*P2 .LT. DZ1DEL .AND.    
     1      DZ1DEL .LT. 0.1D0*P2 .AND. G2 .GE. 0.25D0*DX2) DZ1DEL =     
     2      DMIN1(G2, DZ1DEL)                                           
   8  IF (DZ1DEL .LE. 0.0D0) DZ1DEL = G2                                
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DZ1DOT(N, X, X2, Y, Y2)                 
      INTEGER N                                                         
      DOUBLE PRECISION X(N), X2, Y(N), Y2                               
      INTEGER J                                                         
C FIND DOT PRODUCT OF UNIT VECTORS IN X AND Y DIRECTIONS                
      DZ1DOT = 0.0D0                                                    
      DO  1 J = 1, N                                                    
         DZ1DOT = DZ1DOT+(X(J)/X2)*(Y(J)/Y2)                            
   1     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DZ1PG(N, F, F2, QT, R, P, G, P2, G2, SING, AUX)        
      INTEGER N                                                         
      LOGICAL SING                                                      
      DOUBLE PRECISION F(N), F2, QT(N, N), R(N, N), P(N), G(N)          
      DOUBLE PRECISION P2, G2, AUX(N)                                   
      INTEGER J, IROLD, NERROR, NERR                                    
      DOUBLE PRECISION DNRM2, D1MACH                                    
C GET P=NEWTON STEP, P2= ITS 2-NORM,                                    
C     G=STEEPEST-DESCENT STEP, G2=ITS 2-NORM                            
C INPUTS                                                                
C N = NUMBER OF COMPONENTS                                              
C F = FUNCTION VECTOR                                                   
C F2 = ITS NORM                                                         
C Q*R = APPROXIMATE JACOBIAN AT F                                       
C Q IS ORTHOGONAL, TRANSPOSE STORED IN QT                               
C R IS UPPER TRIANGULAR                                                 
C SCRATCH ARRAY AUX                                                     
C OUTPUTS                                                               
C G AND G2                                                              
C IF R IS NON-SINGULAR, P AND P2 ARE CALCULATED, SING LEFT ALONE.       
C IF R IS SINGULAR, SET P2=LARGE NUMBER, P=0-VECTOR, SING=TRUE.         
      DO  1 J = 1, N                                                    
C USE P FOR SCRATCH                                                     
         P(J) = F(J)/F2                                                 
   1     CONTINUE                                                       
C NEGATIVE OF STEEPEST DESCENT DIRECTION                                
      CALL DZ1VM(N, P, QT, R, AUX, G)                                   
      G2 = DNRM2(N, G, 1)                                               
      IF (G2 .LE. 0.0D0) RETURN                                         
      DO  2 J = 1, N                                                    
         G(J) = (-G(J))/G2                                              
   2     CONTINUE                                                       
C USE P FOR SCRATCH                                                     
      CALL DZ1MV(N, QT, R, G, AUX, P)                                   
      P2 = DNRM2(N, P, 1)                                               
      G2 = (F2*(G2/P2))/P2                                              
      IF (G2 .LE. 0.0D0) RETURN                                         
      DO  3 J = 1, N                                                    
C NOW G IS OF PROPER LENGTH                                             
         G(J) = G2*G(J)                                                 
   3     CONTINUE                                                       
      IF (SING) GOTO 7                                                  
         CALL ENTSRC(IROLD, 1)                                          
C ENTER RECOVERY MODE                                                   
C NEGATIVE OF NEWTON DIRECTION                                          
         CALL DZ1SOL(N, QT, R, F, AUX, P)                               
         IF (NERROR(NERR) .EQ. 0) GOTO 4                                
            SING = .TRUE.                                               
C MATRIX R IS SINGULAR                                                  
            CALL ERROFF                                                 
            GOTO  6                                                     
   4        P2 = DNRM2(N, P, 1)                                         
            DO  5 J = 1, N                                              
               P(J) = -P(J)                                             
   5           CONTINUE                                                 
C RESTORE OLD RECOVERY MODE                                             
   6     CALL RETSRC(IROLD)                                             
   7  IF (.NOT. SING) GOTO 9                                            
         P2 = D1MACH(2)                                                 
         DO  8 J = 1, N                                                 
            P(J) = 0.0D0                                                
   8        CONTINUE                                                    
   9  RETURN                                                            
      END                                                               
      SUBROUTINE DZ1XPG(N, STEEP, DELTA, P, P2, PMIN, G, G2, XP,        
     1   XG, DX2)                                                       
      INTEGER N                                                         
      LOGICAL STEEP                                                     
      DOUBLE PRECISION DELTA, P(N), P2, PMIN, G(N), G2                  
      DOUBLE PRECISION XP, XG, DX2                                      
      DOUBLE PRECISION DDOT, B, C, PG, DSQRT                            
C GET XP AND XG SO THAT DX(J)=XP*P(J)+XG*G(J) HAS LENGTH .LE. DELTA.    
C (LENGTH ALSO .LE.P2)                                                  
C INPUTS                                                                
C N = LNUMBER OF COMPONENTS                                             
C DELTA = MAXIMUM DX LENGTH                                             
C P = NEWTON VECTOR                                                     
C P2 = ITS LENGTH                                                       
C PMIN = SMALLEST ALLOWABLE XP                                          
C G = STEEPEST-DESCENT VECTOR                                           
C G2 = ITS LENGTH                                                       
C OUTPUTS                                                               
C XP AND XG                                                             
C DX2 = LENGTH OF DX-VECTOR                                             
      IF (P2 .GT. DELTA) GOTO 1                                         
         DX2 = P2                                                       
C USE NEWTON STEP                                                       
         XP = 1.0                                                       
         XG = 0.0D0                                                     
         GOTO  4                                                        
   1     IF ((.NOT. STEEP) .AND. DELTA .GT. G2 .AND. DELTA .GE. PMIN*P2)
     1       GOTO 2                                                     
            DX2 = DELTA                                                 
C DX ALONG STEEPEST-DESCENT                                             
            XG = DX2/G2                                                 
            XP = 0.0D0                                                  
            GOTO  3                                                     
   2        PG = DDOT(N, P, 1, G, 1)                                    
C DX IS SOME STEEPEST-DESCENT PLUS SOME NEWTON                          
            DX2 = DELTA                                                 
            C = DX2**2-G2**2                                            
            B = G2**2-PG                                                
            XP = C/(B+DSQRT(B**2+C*(P2**2-2.0D0*PG+G2**2)))             
            XG = 1.0D0-XP                                               
   3  CONTINUE                                                          
   4  RETURN                                                            
      END                                                               
      SUBROUTINE DZ1FAC(N, A, QT)                                       
      INTEGER N                                                         
      DOUBLE PRECISION A(N, N), QT(N, N)                                
      INTEGER J, K                                                      
      DOUBLE PRECISION SC, SS                                           
C DECOMPOSE N BY N MATRIX A INTO Q*R                                    
C Q IS N BY N ORTHOGONAL MATRIX                                         
C R IS N BY N UPPER-TRIANGULAR MATRIX                                   
C A IS OVERWRITTEN BY R.                                                
C TRANSPOSE OF Q IS RETURNED IN QT.                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15HDZ1FAC - N.LT.1, 15, 1, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DZ1FAC - N.LT.1', 15, 1, 2)            
C/                                                                      
      DO  2 J = 1, N                                                    
         DO  1 K = 1, N                                                 
            QT(J, K) = 0.0D0                                            
   1        CONTINUE                                                    
         QT(J, J) = 1.                                                  
   2     CONTINUE                                                       
      K = 1                                                             
         GOTO  4                                                        
   3     K = K+1                                                        
   4     IF (K .GE. N)GOTO  8                                           
         J = N                                                          
            GOTO  6                                                     
   5        J = J-1                                                     
   6        IF (J .LE. K)GOTO  7                                        
            CALL DROTG(A(J-1, K), A(J, K), SC, SS)                      
            CALL DROT(N-K, A(J-1, K+1), N, A(J, K+1), N, SC, SS)        
            CALL DROT(N, QT(J-1, 1), N, QT(J, 1), N, SC, SS)            
            GOTO  5                                                     
   7     GOTO  3                                                        
   8  RETURN                                                            
      END                                                               
      SUBROUTINE DZ1JAC(FUNC, N, X, F, DFDX, JUSED)                     
      INTEGER N                                                         
      EXTERNAL FUNC                                                     
      INTEGER JUSED                                                     
      DOUBLE PRECISION X(N), F(N), DFDX(N, N)                           
      COMMON /DZ1COM/ P2, G2, XP, XG, RATSAV, XGMAX, X2, DX2OLD, DXTRY  
     1   , RELERR, F2PREV, F2BEST, F2LAST, F2NORM, F2DMAX, SMALL, SING  
     2   , GFLAG, VFLAG, JMAX, JCALL, ISLOW, JPRINT, NUMUPD, N2BIG      
      INTEGER JMAX, JCALL, ISLOW, JPRINT, NUMUPD, N2BIG                 
      LOGICAL SMALL, SING, GFLAG, VFLAG                                 
      DOUBLE PRECISION P2, G2, XP, XG, RATSAV, XGMAX                    
      DOUBLE PRECISION X2, DX2OLD, DXTRY, RELERR, F2PREV, F2BEST        
      DOUBLE PRECISION F2LAST, F2NORM, F2DMAX                           
      INTEGER K, NERROR, NERR, J                                        
      DOUBLE PRECISION D1MACH, RSMALL, RELACC, RLARGE, XSAVE, DXSM      
      DOUBLE PRECISION DXBIG, DX, DF2NRM, DNRM2                         
C NUMERICAL APPROXIMATION TO JACOBIAN OF FUNC AT X                      
C INPUTS                                                                
C N = NUMBER OF COMPONENTS                                              
C N-VECTORS X, AND F = VALUE OF FUNC AT X                               
C OUTPUTS                                                               
C JACOBIAN MATRIX DFDX                                                  
C JUSED = NUMBER OF CALLS TO FUNC USED                                  
      RSMALL = D1MACH(1)                                                
      RLARGE = D1MACH(2)                                                
      RELACC = 100.0D0*D1MACH(4)                                        
      JUSED = 0                                                         
      DO  11 K = 1, N                                                   
         DXSM = DMAX1(RSMALL, RELACC*DABS(X(K)))                        
         DXBIG = RLARGE                                                 
         XSAVE = X(K)                                                   
         DX = DMAX1(DXTRY, DXSM)                                        
   1        IF (JCALL+JUSED+N-K .LT. JMAX) GOTO 2                       
C/6S                                                                    
C              CALL SETERR(15HDZ1JAC - FAILED, 15, 1, 1)                
C/7S                                                                    
               CALL SETERR('DZ1JAC - FAILED', 15, 1, 1)                 
C/                                                                      
               RETURN                                                   
   2        X(K) = XSAVE+DX                                             
            JUSED = JUSED+1                                             
            CALL FUNC(N, X, DFDX(1, K))                                 
            IF (NERROR(NERR) .EQ. 0) GOTO 3                             
               CALL ERROFF                                              
               DF2NRM = 2.0D0*F2NORM                                    
               GOTO  5                                                  
   3           DO  4 J = 1, N                                           
                  DFDX(J, K) = DFDX(J, K)-F(J)                          
   4              CONTINUE                                              
               DF2NRM = DNRM2(N, DFDX(1, K), 1)                         
C     IF (DF2NRM.LT.RELACC*F2NORM)      DX TOO SMALL                    
C        DXSM=DMAX1(DX,DXSM), DX=10.0D0*DX                              
C     ELSE                                                              
   5        IF (DF2NRM .LE. F2NORM) GOTO 6                              
               DXBIG = DMIN1(DX, DXBIG)                                 
C DX TOO LARGE                                                          
               DX = DX*DMIN1(0.5D0, DMAX1(0.01D0, 0.5D0*F2NORM/DF2NRM)) 
               GOTO  8                                                  
   6           DO  7 J = 1, N                                           
                  DFDX(J, K) = DFDX(J, K)/DX                            
   7              CONTINUE                                              
               X(K) = XSAVE                                             
               GOTO  10                                                 
   8        IF (DXBIG .GT. DXSM) GOTO 9                                 
C/6S                                                                    
C              CALL SETERR(15HDZ1JAC - FAILED, 15, 2, 1)                
C/7S                                                                    
               CALL SETERR('DZ1JAC - FAILED', 15, 2, 1)                 
C/                                                                      
               RETURN                                                   
   9        DX = DMIN1(DXBIG, DMAX1(DX, DXSM))                          
            GOTO  1                                                     
  10     CONTINUE                                                       
  11     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DZ1MV(N, QT, R, X, AUX, Y)                             
      INTEGER N                                                         
      DOUBLE PRECISION QT(N, N), R(N, N), X(N), AUX(N), Y(N)            
      INTEGER J                                                         
      DOUBLE PRECISION DDOT                                             
C MULTIPLY MATRIX A TIMES VECTOR X = VECTOR Y                           
C N BY N MATRIX A = Q*R                                                 
C Q IS ORTHOGONAL, TRANSPOSE STORED IN QT                               
C R IS UPPER TRIANGULAR                                                 
C SCRATCH SPACE AUX                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(14HDZ1MV - N.LT.1, 15, 1, 2)            
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DZ1MV - N.LT.1', 15, 1, 2)             
C/                                                                      
      DO  1 J = 1, N                                                    
         AUX(J) = DDOT(N+1-J, R(J, J), N, X(J), 1)                      
   1     CONTINUE                                                       
      DO  2 J = 1, N                                                    
         Y(J) = DDOT(N, AUX(1), 1, QT(1, J), 1)                         
   2     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DZ1SOL(N, QT, R, B, AUX, X)                            
      INTEGER N                                                         
      DOUBLE PRECISION QT(N, N), R(N, N), B(N), AUX(N), X(N)            
      INTEGER J                                                         
      DOUBLE PRECISION BIG, D1MACH, TEMP, DDOT, DABS                    
C SOLVE A*X=B FOR X                                                     
C N BY N MATRIX A = Q*R                                                 
C Q IS ORTHOGONAL, TRANSPOSE STORED IN QT                               
C R IS UPPER TRIANGULAR                                                 
C B IS GIVEN VECTOR                                                     
C SCRATCH SPACE AUX                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15HDZ1SOL - N.LT.1, 15, 1, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DZ1SOL - N.LT.1', 15, 1, 2)            
C/                                                                      
      DO  1 J = 1, N                                                    
         AUX(J) = DDOT(N, QT(J, 1), N, B(1), 1)                         
   1     CONTINUE                                                       
      BIG = 0.5D0*D1MACH(2)                                             
      J = N                                                             
         GOTO  3                                                        
   2     J = J-1                                                        
   3     IF (J .LE. 0)GOTO  6                                           
         TEMP = AUX(J)                                                  
         IF (J .LT. N) TEMP = TEMP - DDOT(N-J, X(J+1), 1, R(J, J+1), N) 
         IF (DABS(R(J, J)) .GE. 1.0D0) GOTO 5                           
            IF (DABS(TEMP) .LE. BIG*DABS(R(J, J))) GOTO 4               
C/6S                                                                    
C              CALL SETERR(24HDZ1SOL - SINGULAR MATRIX, 24, 2, 1)       
C/7S                                                                    
               CALL SETERR('DZ1SOL - SINGULAR MATRIX', 24, 2, 1)        
C/                                                                      
               RETURN                                                   
   4     CONTINUE                                                       
   5     X(J) = TEMP/R(J, J)                                            
         GOTO  2                                                        
   6  RETURN                                                            
      END                                                               
      SUBROUTINE DZ1UPD(N, QT, R, U, V, AUX)                            
      INTEGER N                                                         
      DOUBLE PRECISION QT(N, N), R(N, N), U(N), V(N), AUX(N)            
      INTEGER J, K                                                      
      DOUBLE PRECISION SC, SS, DDOT                                     
C RANK-ONE UPDATE OF QR FACTORIZATION                                   
C INPUT MATRIX A=Q*R                                                    
C TO BE UPDATED BY ADDING U*V-TRANSPOSE                                 
C Q IS ORTHOGONAL, TRANSPOSE STORED IN QT                               
C R IS UPPER TRIANGULAR                                                 
C SCRATCH SPACE AUX                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15HDZ1UPD - N.LT.1, 15, 1, 2)           
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DZ1UPD - N.LT.1', 15, 1, 2)            
C/                                                                      
      DO  1 J = 1, N                                                    
         AUX(J) = DDOT(N, QT(J, 1), N, U, 1)                            
   1     CONTINUE                                                       
      J = N                                                             
         GOTO  3                                                        
   2     J = J-1                                                        
   3     IF (J .LE. 2)GOTO  4                                           
         CALL DROTG(AUX(J-1), AUX(J), SC, SS)                           
         CALL DROT(N+2-J, R(J-1, J-1), N, R(J, J-1), N, SC, SS)         
         CALL DROT(N, QT(J-1, 1), N, QT(J, 1), N, SC, SS)               
         GOTO  2                                                        
   4  DO  5 K = 1, N                                                    
         R(1, K) = R(1, K)+AUX(1)*V(K)                                  
   5     CONTINUE                                                       
      IF (N .LE. 1) GOTO 10                                             
         DO  6 K = 1, N                                                 
            R(2, K) = R(2, K)+AUX(2)*V(K)                               
   6        CONTINUE                                                    
         J = 1                                                          
            GOTO  8                                                     
   7        J = J+1                                                     
   8        IF (J .GE. N)GOTO  9                                        
            CALL DROTG(R(J, J), R(J+1, J), SC, SS)                      
            CALL DROT(N-J, R(J, J+1), N, R(J+1, J+1), N, SC, SS)        
            CALL DROT(N, QT(J, 1), N, QT(J+1, 1), N, SC, SS)            
            GOTO  7                                                     
   9     CONTINUE                                                       
  10  RETURN                                                            
      END                                                               
      SUBROUTINE DZ1VM(N, X, QT, R, AUX, Y)                             
      INTEGER N                                                         
      DOUBLE PRECISION X(N), QT(N, N), R(N, N), AUX(N), Y(N)            
      INTEGER J                                                         
      DOUBLE PRECISION DDOT                                             
C MULTIPLY VECTOR X-TRANSPOSE TIMES MATRIX A                            
C A = Q*R                                                               
C Q IS ORTHOGONAL, TRANSPOSE STORED IN QT                               
C R IS UPPER TRIANGULAR                                                 
C SCRATCH SPACE AUX                                                     
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(14HDZ1VM - N.LT.1, 15, 1, 2)            
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DZ1VM - N.LT.1', 15, 1, 2)             
C/                                                                      
      DO  1 J = 1, N                                                    
         AUX(J) = DDOT(N, QT(J, 1), N, X(1), 1)                         
   1     CONTINUE                                                       
      DO  2 J = 1, N                                                    
         Y(J) = DDOT(J, AUX(1), 1, R(1, J), 1)                          
   2     CONTINUE                                                       
      RETURN                                                            
      END                                                               
C****END OF ROUTINES FOR PORT 3 ROOTS CHAPTER***************************
