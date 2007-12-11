C$TEST THYP                                                             
C **********************************************************************
C                                                                       
C  TEST OF PORT LINGUISTIC HYPOTHESES                                   
C                                                                       
C **********************************************************************
C                                                                       
      COMMON /OK/ CREM, DREM, RREM, IREM, LREM, CIDX, DIDX, RIDX, IIDX  
     1   , LIDX                                                         
      LOGICAL CREM, DREM, RREM, IREM, LREM, CIDX                        
      LOGICAL DIDX, RIDX, IIDX, LIDX                                    
      COMMON /LNAME/ L                                                  
      LOGICAL L(1000)                                                   
      COMMON /INAME/ I                                                  
      INTEGER I(1000)                                                   
      COMMON /RNAME/ R                                                  
      REAL R(1000)                                                      
      COMMON /DNAME/ D                                                  
      DOUBLE PRECISION D(1000)                                          
      COMMON /CNAME/ C                                                  
C/R                                                                     
C     REAL C(2,1000)                                                    
C/C                                                                     
      COMPLEX C(1000)                                                   
C/                                                                      
      INTEGER P, I1MACH                                                 
      REAL FLOAT                                                        
      LOGICAL LOGVAR                                                    
C/R                                                                     
C/C                                                                     
      COMPLEX CMPLX                                                     
C/                                                                      
      INTEGER TEMP                                                      
C TO TEST THE PORT LINGUISTIC HYPOTHESES.                               
C .REM MEANS REMEMBERED AND .IDX MEANS INDEXING OK IN THE FOLLOWING.    
      CREM = .TRUE.                                                     
      DREM = .TRUE.                                                     
      RREM = .TRUE.                                                     
      IREM = .TRUE.                                                     
      LREM = .TRUE.                                                     
      CIDX = .TRUE.                                                     
      DIDX = .TRUE.                                                     
      RIDX = .TRUE.                                                     
      IIDX = .TRUE.                                                     
      LIDX = .TRUE.                                                     
      LOGVAR = .TRUE.                                                   
C MAKE SOME DATA.                                                       
      DO  1 P = 1, 1000                                                 
C/R                                                                     
C        C(1,P) = -FLOAT(P)                                             
C        C(2,P) = FLOAT(P)                                              
C/C                                                                     
         C(P) = CMPLX(-FLOAT(P), FLOAT(P))                              
C/                                                                      
         D(P) = P                                                       
         R(P) = -P                                                      
         I(P) = P                                                       
         L(P) = LOGVAR                                                  
         LOGVAR = .NOT. LOGVAR                                          
   1     CONTINUE                                                       
C CHECK IT IN ARGUMENT AND COMMON LISTS.                                
      DO  2 P = 1, 999                                                  
         CALL CHECK(C, D, R, I, L, P)                                   
   2     CONTINUE                                                       
      IF (CREM) GOTO 4                                                  
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  3)                                               
   3     FORMAT (49H COMPLEX VALUES NOT REMEMBERED BY DATA STATEMENT.)  
   4  IF (DREM) GOTO 6                                                  
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  5)                                               
   5     FORMAT (                                                       
     1     58H DOUBLE PRECISION VALUES NOT REMEMBERED BY DATA STATEMENT.
     2      )                                                           
   6  IF (RREM) GOTO 8                                                  
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  7)                                               
   7     FORMAT (46H REAL VALUES NOT REMEMBERED BY DATA STATEMENT.)     
   8  IF (IREM) GOTO 10                                                 
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  9)                                               
   9     FORMAT (49H INTEGER VALUES NOT REMEMBERED BY DATA STATEMENT.)  
  10  IF (LREM) GOTO 12                                                 
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  11)                                              
  11     FORMAT (49H LOGICAL VALUES NOT REMEMBERED BY DATA STATEMENT.)  
  12  IF (CIDX) GOTO 14                                                 
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  13)                                              
  13     FORMAT (45H COMPLEX INDICES LARGER THAN ONE NOT ALLOWED.)      
  14  IF (DIDX) GOTO 16                                                 
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  15)                                              
  15     FORMAT (                                                       
     1      54H DOUBLE PRECISION INDICES LARGER THAN ONE NOT ALLOWED.)  
  16  IF (RIDX) GOTO 18                                                 
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  17)                                              
  17     FORMAT (42H REAL INDICES LARGER THAN ONE NOT ALLOWED.)         
  18  IF (IIDX) GOTO 20                                                 
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  19)                                              
  19     FORMAT (45H INTEGER INDICES LARGER THAN ONE NOT ALLOWED.)      
  20  IF (LIDX) GOTO 22                                                 
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  21)                                              
  21     FORMAT (45H LOGICAL INDICES LARGER THAN ONE NOT ALLOWED.)      
  22  TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  23)                                                 
  23  FORMAT (45H TEST OF PORT LINGUISTIC HYPOTHESES COMPLETE.)         
      STOP                                                              
      END                                                               
      SUBROUTINE CHECK(C, D, R, I, L, P)                                
      INTEGER I(1), P                                                   
      REAL R(1)                                                         
      LOGICAL L(1)                                                      
C/R                                                                     
C     REAL C(2,1)                                                       
C/C                                                                     
      COMPLEX C(1)                                                      
C/                                                                      
      DOUBLE PRECISION D(1)                                             
      COMMON /OK/ CREM, DREM, RREM, IREM, LREM, CIDX, DIDX, RIDX, IIDX  
     1   , LIDX                                                         
      LOGICAL CREM, DREM, RREM, IREM, LREM, CIDX                        
      LOGICAL DIDX, RIDX, IIDX, LIDX                                    
      COMMON /LNAME/ CL                                                 
      LOGICAL CL(1000)                                                  
      COMMON /INAME/ CI                                                 
      INTEGER CI(1000)                                                  
      COMMON /RNAME/ CR                                                 
      REAL CR(1000)                                                     
      COMMON /DNAME/ CD                                                 
      DOUBLE PRECISION CD(1000)                                         
      COMMON /CNAME/ CC                                                 
C/R                                                                     
C     REAL CC(2,1000), LC(2)                                            
C/C                                                                     
      COMPLEX CC(1000), LC                                              
C/                                                                      
      INTEGER LI, Q, I1MACH                                             
      REAL LR, AIMAG, REAL                                              
      LOGICAL LL                                                        
      DOUBLE PRECISION LD                                               
      INTEGER TEMP                                                      
C/R                                                                     
C     DATA LC(1),LC(2)/-1., 1./                                         
C/C                                                                     
      DATA LC/(-1.,  1.)/                                               
C/                                                                      
      DATA LD/1.0D0/                                                    
      DATA LR/-1.0E0/                                                   
      DATA LI/1/                                                        
      DATA LL/.TRUE./                                                   
C LOCAL VARIABLES.                                                      
C START LOCAL VARIABLES AS COMMON                                       
C VARIABLES BEGIN.                                                      
      IF (P .NE. 1) GOTO 11                                             
C/R                                                                     
C        IF (LC(1) .EQ. C(1,P) .AND. LC(2) .EQ. C(2,P)                  
C    1      ) GOTO 2                                                    
C/C                                                                     
         IF (REAL(LC) .EQ. REAL(C(P)) .AND. AIMAG(LC) .EQ. AIMAG(C(P))  
     1      ) GOTO 2                                                    
C/                                                                      
            TEMP = I1MACH(2)                                            
C CHECK DATA VALUES OF LOCAL VARIABLES.                                 
            WRITE (TEMP,  1)                                            
   1        FORMAT (34H DATA STATEMENT FAILS FOR COMPLEX.)              
   2     IF (LD .EQ. D(P)) GOTO 4                                       
            TEMP = I1MACH(2)                                            
            WRITE (TEMP,  3)                                            
   3        FORMAT (43H DATA STATEMENT FAILS FOR DOUBLE PRECISION.)     
   4     IF (LR .EQ. R(P)) GOTO 6                                       
            TEMP = I1MACH(2)                                            
            WRITE (TEMP,  5)                                            
   5        FORMAT (31H DATA STATEMENT FAILS FOR REAL.)                 
   6     IF (LI .EQ. I(P)) GOTO 8                                       
            TEMP = I1MACH(2)                                            
            WRITE (TEMP,  7)                                            
   7        FORMAT (34H DATA STATEMENT FAILS FOR INTEGER.)              
   8     IF ((LL .OR. (.NOT. L(P))) .AND. ((.NOT. LL) .OR. L(P))) GOTO  
     1      10                                                          
            TEMP = I1MACH(2)                                            
            WRITE (TEMP,  9)                                            
   9        FORMAT (34H DATA STATEMENT FAILS FOR LOGICAL.)              
  10     CONTINUE                                                       
         GOTO  12                                                       
C/R                                                                     
C 11     IF (LC(1) .NE. C(1,P) .OR. LC(2) .NE. C(2,P))                  
C    1       CREM = .FALSE.                                             
C/C                                                                     
  11     IF (REAL(LC) .NE. REAL(C(P)) .OR. AIMAG(LC) .NE. AIMAG(C(P)))  
     1       CREM = .FALSE.                                             
C/                                                                      
C CHECK THE REMEMBERED LOCAL VALUES.                                    
         IF (LD .NE. D(P)) DREM = .FALSE.                               
         IF (LR .NE. R(P)) RREM = .FALSE.                               
         IF (LI .NE. I(P)) IREM = .FALSE.                               
         IF ((.NOT. LL) .AND. L(P) .OR. LL .AND. (.NOT. L(P))) LREM =   
     1      .FALSE.                                                     
C MAKE A LOCAL COPY OF THE NEXT VALUES TO BE SEEN BY CHECK.             
C/R                                                                     
C 12  LC(1) = C(1,P+1)                                                  
C     LC(2) = C(2,P+1)                                                  
C/C                                                                     
  12  LC = C(P+1)                                                       
C/                                                                      
      LD = D(P+1)                                                       
      LR = R(P+1)                                                       
      LI = I(P+1)                                                       
      LL = L(P+1)                                                       
      IF (P .NE. 1) GOTO 13                                             
C/R                                                                     
C        IF (LC(1) .EQ. (-1.) .AND. LC(2) .EQ. 1.) CIDX =               
C    1      .FALSE.                                                     
C/C                                                                     
         IF (REAL(LC) .EQ. (-1.) .AND. AIMAG(LC) .EQ. 1.) CIDX =        
     1      .FALSE.                                                     
C/                                                                      
C CHECK THAT LOCAL VARIABLES WERE REALLY UPDATED.                       
         IF (LD .EQ. 1D0) DIDX = .FALSE.                                
         IF (LR .EQ. (-1.)) RIDX = .FALSE.                              
         IF (LI .EQ. 1) IIDX = .FALSE.                                  
         IF (LL) LIDX = .FALSE.                                         
C CHECK THAT THE LOCAL VARIABLES WERE UPDATED.                          
C/R                                                                     
C 13  IF (LC(1) .EQ. C(1,P) .AND. LC(2) .EQ. C(2,P))                    
C    1   CIDX = .FALSE.                                                 
C/C                                                                     
  13  IF (REAL(LC) .EQ. REAL(C(P)) .AND. AIMAG(LC) .EQ. AIMAG(C(P)))    
     1   CIDX = .FALSE.                                                 
C/                                                                      
      IF (LD .EQ. D(P)) DIDX = .FALSE.                                  
      IF (LR .EQ. R(P)) RIDX = .FALSE.                                  
      IF (LI .EQ. I(P)) IIDX = .FALSE.                                  
      IF (LL .AND. L(P) .OR. (.NOT. LL) .AND. (.NOT. L(P))) LIDX =      
     1   .FALSE.                                                        
      DO  14 Q = P, 999                                                 
C/R                                                                     
C        IF (C(1,Q+1) .NE. CC(1,Q+1) .OR. C(2,Q+1) .NE.                 
C    1      CC(2,Q+1)) CIDX = .FALSE.                                   
C/C                                                                     
         IF (REAL(C(Q+1)) .NE. REAL(CC(Q+1)) .OR. AIMAG(C(Q+1)) .NE.    
     1      AIMAG(CC(Q+1))) CIDX = .FALSE.                              
C/                                                                      
         IF (D(Q+1) .NE. CD(Q+1)) DIDX = .FALSE.                        
         IF (R(Q+1) .NE. CR(Q+1)) RIDX = .FALSE.                        
         IF (I(Q+1) .NE. CI(Q+1)) IIDX = .FALSE.                        
         IF ((.NOT. L(Q+1)) .AND. CL(Q+1) .OR. L(Q+1) .AND. (.NOT. CL(Q+
     1      1))) LIDX = .FALSE.                                         
  14     CONTINUE                                                       
      RETURN                                                            
      END                                                               
C$TEST MACH                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT MACHINE CONSTANTS  (FOR CONSISTENCY)                
C                                                                       
C***********************************************************************
      CALL S1MACH                                                       
      STOP                                                              
      END                                                               
C$TEST ERR1                                                             
C **********************************************************************
C                                                                       
C  TO TEST THE ERROR HANDLING PACKAGE                                   
C TEST NUMBER 1: TURNING RECOVERY ON AND OFF.                           
C                                                                       
C **********************************************************************
      INTEGER OLDREC, NERR, NERROR, I1MACH                              
      INTEGER TEMP                                                      
C ENTER RECOVERY MODE.                                                  
      CALL ENTSRC(OLDREC, 1)                                            
C SET AN ERROR.                                                         
C/6S                                                                    
C     CALL SETERR(39HFIRST ERROR TEST - RECOVERABLE ERROR OK, 39, 1, 1) 
C/7S                                                                    
      CALL SETERR('FIRST ERROR TEST - RECOVERABLE ERROR OK', 39, 1, 1)  
C/                                                                      
C PRINT THE ERROR.                                                      
      CALL EPRINT                                                       
      IF (NERROR(NERR) .EQ. 1) GOTO 2                                   
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  1)                                               
   1     FORMAT (14H NERROR FAILS.)                                     
C TURN THE ERROR STATE OFF.                                             
   2  CALL ERROFF                                                       
      IF (NERROR(NERR) .EQ. 0) GOTO 4                                   
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  3)                                               
   3     FORMAT (42H ERROFF FAILS TO TURN THE ERROR STATE OFF.)         
C SEE IF THE ERROR PRINTS.                                              
   4  CALL EPRINT                                                       
C/6S                                                                    
C     CALL SETERR(                                                      
C    1   34HSAME TEST -  LEAVING RECOVERY MODE,                         
C    2   34, 2, 1)                                                      
C/7S                                                                    
      CALL SETERR(                                                      
     1   'SAME TEST -  LEAVING RECOVERY MODE',                          
     2   34, 2, 1)                                                      
C/                                                                      
C RESTORE OLD RECOVERY LEVEL, THE DEFAULT.                              
      CALL RETSRC(OLDREC)                                               
      TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  5)                                                  
   5  FORMAT (42H RECOVERY MODE REMAINS ON WHEN TURNED OFF.)            
      STOP                                                              
      END                                                               
C$TEST ERR2                                                             
C **********************************************************************
C                                                                       
C  TO TEST THE ERROR HANDLING PACKAGE                                   
C TEST NUMBER 2: FATAL ERRORS.                                          
C                                                                       
C **********************************************************************
      INTEGER I1MACH                                                    
      INTEGER TEMP                                                      
C SET AN ERROR.                                                         
C/6S                                                                    
C     CALL SETERR(                                                      
C    1   39HSECOND ERROR TEST - FATAL ERROR CHECKED, 39, 1, 2)          
C/7S                                                                    
      CALL SETERR(                                                      
     1   'SECOND ERROR TEST - FATAL ERROR CHECKED', 39, 1, 2)           
C/                                                                      
      TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  1)                                                  
   1  FORMAT (39H A FATAL ERROR FAILS TO HALT EXECUTION.)               
      STOP                                                              
      END                                                               
C$TEST ERR3                                                             
C **********************************************************************
C                                                                       
C  TO TEST THE ERROR HANDLING PACKAGE                                   
C TEST NUMBER 3: TWO RECOVERABLE ERRORS IN A ROW.                       
C                                                                       
C **********************************************************************
      INTEGER OLDREC, I1MACH                                            
      INTEGER TEMP                                                      
C ENTER RECOVERY MODE.                                                  
      CALL ENTSRC(OLDREC, 1)                                            
C/6S                                                                    
C     CALL SETERR(30HMAIN - FIRST RECOVERABLE ERROR, 30, 1, 1)          
C     CALL SETERR(31HMAIN - SECOND RECOVERABLE ERROR, 31, 2, 1)         
C/7S                                                                    
      CALL SETERR('MAIN - FIRST RECOVERABLE ERROR', 30, 1, 1)           
      CALL SETERR('MAIN - SECOND RECOVERABLE ERROR', 31, 2, 1)          
C/                                                                      
      TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  1)                                                  
   1  FORMAT (                                                          
     1   56H TWO RECOVERABLE ERRORS IN A ROW FAIL TO HALT EXECUTION.)   
      STOP                                                              
      END                                                               
C$TEST STK1                                                             
C **********************************************************************
C                                                                       
C  FIRST STORAGE ALLOCATOR TEST                                         
C TESTS THE STORAGE ALLOCATOR WITH DEFAULT INITIALIZATION LENGTH.       
C                                                                       
C **********************************************************************
C NUMBER OF OUTSTANDING ALLOCATIONS.                                    
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IS(1000), ISTKMD, ISTKGT, ISTKQU, ISTKST, I               
      INTEGER J, K, NALOCS, I1MACH                                      
      REAL RS(1000), R1MACH                                             
      LOGICAL LS(1000)                                                  
C/R                                                                     
C     REAL CS(2,500), R2(2)                                             
C/C                                                                     
      COMPLEX CS(500), CMPLX                                            
C/                                                                      
      DOUBLE PRECISION D1MACH                                           
      INTEGER TEMP                                                      
C/R                                                                     
C     EQUIVALENCE (DS(1), CS(1,1), RS(1), IS(1), LS(1))                 
C/C                                                                     
      EQUIVALENCE (DS(1), CS(1), RS(1), IS(1), LS(1))                   
C/                                                                      
      NALOCS = 0                                                        
      TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  1)                                                  
   1  FORMAT (                                                          
     1  61H AN ERROR BELOW INDICATES TROUBLE WITH THE STORAGE ALLOCATOR,
     2   )                                                              
      TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  2)                                                  
   2  FORMAT (42H WHEN USING THE STACK WITH DEFAULT LENGTH.//)          
      I = 1                                                             
         GOTO  4                                                        
   3     I = I+1                                                        
   4     K = 5                                                          
C GET THE ENTIRE STACK, I ITEMS AT A TIME.                              
C   DO THE ALLOCATIONS IN ORDER OF                                      
C   COMPLEX, LONG REAL, REAL, INTEGER AND LOGICAL.                      
            GOTO  6                                                     
   5        K = K-1                                                     
   6        IF (K .LT. 1) GOTO  16                                      
            IF (ISTKQU(K) .LT. I) GOTO  17                              
C GET ALL THE REMAINING STACK.                                          
            J = ISTKGT(ISTKQU(K), K)                                    
C TRUNCATE TO I ITEMS.                                                  
            J = ISTKMD(I)                                               
            GOTO  12                                                    
C FILL THE SPACE UP ACCORDINGLY.                                        
C/R                                                                     
C  7           R2(1) = R1MACH(2)                                        
C              R2(2) = R2(1)                                            
C              CALL SETC(I, R2, CS(1,J))                                
C/C                                                                     
   7           CALL SETC(I, CMPLX(R1MACH(2), R1MACH(2)), CS(J))         
C/                                                                      
               GOTO  13                                                 
   8           CALL SETD(I, D1MACH(2), DS(J))                           
               GOTO  13                                                 
   9           CALL SETR(I, R1MACH(2), RS(J))                           
               GOTO  13                                                 
  10           CALL SETI(I, I1MACH(9), IS(J))                           
               GOTO  13                                                 
  11           CALL SETL(I, .TRUE., LS(J))                              
               GOTO  13                                                 
  12           IF (K .EQ. 1) GOTO  11                                   
               IF (K .EQ. 2) GOTO  10                                   
               IF (K .EQ. 3) GOTO  9                                    
               IF (K .EQ. 4) GOTO  8                                    
               IF (K .EQ. 5) GOTO  7                                    
  13        NALOCS = NALOCS+1                                           
            IF (ISTKST(1) .EQ. NALOCS) GOTO 15                          
               TEMP = I1MACH(2)                                         
               WRITE (TEMP,  14)                                        
  14           FORMAT (24H ISTKST(1) IS INCORRECT.)                     
  15        CONTINUE                                                    
            GOTO  5                                                     
  16     CONTINUE                                                       
         GOTO  3                                                        
  17  IF (NALOCS .GE. 6) GOTO 19                                        
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  18) NALOCS                                       
  18     FORMAT (30H THE DEFAULT STACK ONLY HOLDS , I1, 7H ITEMS,,      
     1      32H IT SHOULD HOLD SEVERAL HUNDRED.)                        
C RELEASE THE ALLOCATIONS, ONE BY ONE.                                  
  19  I = 1                                                             
         GOTO  21                                                       
  20     I = I+1                                                        
  21     IF (I .GT. NALOCS) GOTO  25                                    
         IF (ISTKST(1) .LE. 0) GOTO 22                                  
            CALL ISTKRL(1)                                              
            GOTO  24                                                    
  22        TEMP = I1MACH(2)                                            
            WRITE (TEMP,  23)                                           
  23        FORMAT (                                                    
     1         50H ALLOCATOR OUT OF ALLOCATIONS BEFORE IT SHOULD BE.)   
  24     CONTINUE                                                       
         GOTO  20                                                       
  25  IF (ISTKST(1) .EQ. 0) GOTO 27                                     
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  26)                                              
  26     FORMAT (49H AFTER DE-ALLOCATING ALL THE ITEMS, ITEMS REMAIN.)  
  27  TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  28)                                                 
  28  FORMAT (                                                          
     1   39H FIRST STORAGE ALLOCATOR TEST COMPLETE.//)                  
      TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  29)                                                 
  29  FORMAT (49H NOW FORCE AN ERROR BY REQUESTING TOO MUCH SPACE.)     
      I = ISTKGT(2*ISTKQU(5)+10, 5)                                     
      STOP                                                              
      END                                                               
      SUBROUTINE SETC(N,V,B)                                            
C                                                                       
C     SETC SETS THE N COMPLEX ITEMS IN B TO V                           
C                                                                       
C/R                                                                     
C     REAL B(2,N), V(2)                                                 
C/C                                                                     
      COMPLEX B(N),V                                                    
C/                                                                      
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
C/R                                                                     
C       B(1,I) = V(1)                                                   
C10     B(2,I) = V(2)                                                   
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
      DOUBLE PRECISION B(N),V                                           
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
      INTEGER B(N),V                                                    
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
      LOGICAL B(N),V                                                    
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
      REAL B(N),V                                                       
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = V                                                        
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
C$TEST STK2                                                             
C **********************************************************************
C                                                                       
C  SECOND ALLOCATOR TEST                                                
C TESTS THE STORAGE ALLOCATOR WHEN INITIALIZED TO A NON-DEFAULT LENGTH. 
C                                                                       
C **********************************************************************
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(5000)                                         
      INTEGER IS(1000), ISTKMD, ISTKGT, ISTKQU, ISTKST, I               
      INTEGER J, K, NALOCS, I1MACH                                      
      REAL RS(1000), R1MACH                                             
      LOGICAL LS(1000)                                                  
C/R                                                                     
C     REAL CS(2,500)                                                    
C/C                                                                     
      COMPLEX CS(500), CMPLX                                            
C/                                                                      
      DOUBLE PRECISION D1MACH                                           
      INTEGER TEMP                                                      
C/R                                                                     
C     EQUIVALENCE (DS(1), CS(1,1), RS(1), IS(1), LS(1))                 
C/C                                                                     
      EQUIVALENCE (DS(1), CS(1), RS(1), IS(1), LS(1))                   
C/                                                                      
C INITIALIZE THE STACK.                                                 
      CALL ISTKIN(5000, 4)                                              
C NUMBER OF OUTSTANDING ALLOCATIONS.                                    
      NALOCS = 0                                                        
      TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  1)                                                  
   1  FORMAT (                                                          
     1  61H AN ERROR BELOW INDICATES TROUBLE WITH THE STORAGE ALLOCATOR,
     2   )                                                              
      TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  2)                                                  
   2  FORMAT (54H WHEN USING A STACK INITIALIZED TO NON-DEFAULT LENGTH. 
     1   //)                                                            
      I = 1                                                             
         GOTO  4                                                        
   3     I = I+1                                                        
   4     K = 5                                                          
C GET THE ENTIRE STACK, I ITEMS AT A TIME.                              
C   DO THE ALLOCATIONS IN ORDER OF                                      
C   COMPLEX, LONG REAL, REAL, INTEGER AND LOGICAL.                      
            GOTO  6                                                     
   5        K = K-1                                                     
   6        IF (K .LT. 1) GOTO  16                                      
            IF (ISTKQU(K) .LT. I) GOTO  17                              
C GET ALL THE REMAINING STACK.                                          
            J = ISTKGT(ISTKQU(K), K)                                    
C TRUNCATE TO I ITEMS.                                                  
            J = ISTKMD(I)                                               
            GOTO  12                                                    
C FILL THE SPACE UP ACCORDINGLY.                                        
C/R                                                                     
C  7           CALL SETC(I, R1MACH(2), R1MACH(2), CS(1,J))              
C/C                                                                     
   7           CALL SETC(I, CMPLX(R1MACH(2), R1MACH(2)), CS(J))         
C/                                                                      
               GOTO  13                                                 
   8           CALL SETD(I, D1MACH(2), DS(J))                           
               GOTO  13                                                 
   9           CALL SETR(I, R1MACH(2), RS(J))                           
               GOTO  13                                                 
  10           CALL SETI(I, I1MACH(9), IS(J))                           
               GOTO  13                                                 
  11           CALL SETL(I, .TRUE., LS(J))                              
               GOTO  13                                                 
  12           IF (K .EQ. 1) GOTO  11                                   
               IF (K .EQ. 2) GOTO  10                                   
               IF (K .EQ. 3) GOTO  9                                    
               IF (K .EQ. 4) GOTO  8                                    
               IF (K .EQ. 5) GOTO  7                                    
  13        NALOCS = NALOCS+1                                           
            IF (ISTKST(1) .EQ. NALOCS) GOTO 15                          
               TEMP = I1MACH(2)                                         
               WRITE (TEMP,  14)                                        
  14           FORMAT (24H ISTKST(1) IS INCORRECT.)                     
  15        CONTINUE                                                    
            GOTO  5                                                     
  16     CONTINUE                                                       
         GOTO  3                                                        
  17  IF (NALOCS .GE. 6) GOTO 19                                        
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  18) NALOCS                                       
  18     FORMAT (30H THE DEFAULT STACK ONLY HOLDS , I1, 7H ITEMS,,      
     1      32H IT SHOULD HOLD SEVERAL HUNDRED.)                        
C RELEASE THE ALLOCATIONS, ONE BY ONE.                                  
  19  I = 1                                                             
         GOTO  21                                                       
  20     I = I+1                                                        
  21     IF (I .GT. NALOCS) GOTO  25                                    
         IF (ISTKST(1) .LE. 0) GOTO 22                                  
            CALL ISTKRL(1)                                              
            GOTO  24                                                    
  22        TEMP = I1MACH(2)                                            
            WRITE (TEMP,  23)                                           
  23        FORMAT (                                                    
     1         50H ALLOCATOR OUT OF ALLOCATIONS BEFORE IT SHOULD BE.)   
  24     CONTINUE                                                       
         GOTO  20                                                       
  25  IF (ISTKST(1) .EQ. 0) GOTO 27                                     
         TEMP = I1MACH(2)                                               
         WRITE (TEMP,  26)                                              
  26     FORMAT (49H AFTER DE-ALLOCATING ALL THE ITEMS, ITEMS REMAIN.)  
  27  TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  28)                                                 
  28  FORMAT (40H SECOND STORAGE ALLOCATOR TEST COMPLETE.//)            
      TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  29)                                                 
  29  FORMAT (49H NOW FORCE AN ERROR BY REQUESTING TOO MUCH SPACE.)     
      I = ISTKGT(2*ISTKQU(5)+10, 5)                                     
      STOP                                                              
      END                                                               
C/R                                                                     
C     SUBROUTINE SETC(N, V1, V2, B)                                     
C     REAL V1, V2, B(2,N)                                               
C/C                                                                     
      SUBROUTINE SETC(N,V,B)                                            
      COMPLEX B(N),V                                                    
C/                                                                      
C                                                                       
C     SETC SETS THE N COMPLEX ITEMS IN B TO V                           
C                                                                       
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
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE SETD(N,V,B)                                            
C                                                                       
C     SETD SETS THE N DOUBLE PRECISION ITEMS IN B TO V                  
C                                                                       
      DOUBLE PRECISION B(N),V                                           
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
      INTEGER B(N),V                                                    
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
      LOGICAL B(N),V                                                    
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
      REAL B(N),V                                                       
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = V                                                        
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
C$TEST ERRK                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM STKDMP                                      
C                                                                       
C***********************************************************************
C                                                                       
C  TEST OF THE STACK DUMP,  STKDMP                                      
C                                                                       
C     WRITTEN JANUARY 15, 1983 BY P. FOX                                
C                                                                       
C     ALLOCATES AND DEALLOCATES SPACE AND DUMPS THE STACK.              
C     THEN WRITES OVER PART OF THE STACK AND DUMPS IT.                  
C                                                                       
C     FINALLY WRITES OVER THE HEADING INFORMATION, WHICH                
C     CAUSES THE ENTIRE STACK TO BE DUMPED.                             
C                                                                       
      INTEGER  IPNTR, RPNTR, ISTKGT, J, K, KK                           
C                                                                       
      COMMON  /CSTAK/   DSTAK                                           
      DOUBLE PRECISION  DSTAK(500)                                      
      INTEGER           ISTAK(1000)                                     
      REAL              RSTAK(1000)                                     
      LOGICAL           LSTAK(1000)                                     
C/R                                                                     
C     REAL              CMSTAK(2,500)                                   
C/C                                                                     
      COMPLEX           CMSTAK(500)                                     
C/                                                                      
C                                                                       
      EQUIVALENCE (DSTAK(1), ISTAK(1))                                  
      EQUIVALENCE (DSTAK(1), LSTAK(1))                                  
      EQUIVALENCE (DSTAK(1), RSTAK(1))                                  
C/R                                                                     
C     EQUIVALENCE (DSTAK(1), CMSTAK(1,1))                               
C     REAL ONES(2)                                                      
C     DATA ONES(1),ONES(2)/1., -1./                                     
C/C                                                                     
      EQUIVALENCE (DSTAK(1), CMSTAK(1))                                 
      COMPLEX  ONES                                                     
      DATA ONES/(1., -1.)/                                              
C/                                                                      
C                                                                       
C                                                                       
C  SET UP STACK LOCATIONS FOR EACH TYPE OF VARIABLE -                   
C  LOGICAL, INTEGER, REAL, DOUBLE-PRECISION, AND                        
C  COMPLEX, AND STORE A -1 OF THE APPROPRIATE TYPE                      
C  OR, FOR LOGICALS STORE AN F (.FALSE.).                               
C                                                                       
      IPNTR = ISTKGT(25, 1)                                             
      CALL SETL(25, .FALSE., LSTAK(IPNTR))                              
      IPNTR = ISTKGT(25, 2)                                             
      CALL SETI(25, -1, ISTAK(IPNTR))                                   
      IPNTR = ISTKGT(25, 3)                                             
      CALL SETR(25, -1.0, RSTAK(IPNTR))                                 
      IPNTR = ISTKGT(25, 4)                                             
      CALL SETD(25, -1.0D0, DSTAK(IPNTR))                               
      IPNTR = ISTKGT(25, 5)                                             
C/R                                                                     
C     CALL SETC(25, ONES, CMSTAK(1,IPNTR))                              
C/C                                                                     
      CALL SETC(25, ONES, CMSTAK(IPNTR))                                
C/                                                                      
C                                                                       
C  NOW CALL STACK DUMP                                                  
C                                                                       
      CALL STKDMP                                                       
C                                                                       
C  NOW RELEASE ALL BUT THE FIRST ALLOCATION.                            
C                                                                       
      CALL ISTKRL(4)                                                    
C                                                                       
C  AND CALL THE STACK DUMP AGAIN                                        
C                                                                       
      CALL STKDMP                                                       
C                                                                       
C  NOW (SO IT WILL LOOK LIKE A FRESH START)                             
C  RELEASE THE ONE OUTSTANDING ALLOCATION AND                           
C  ZERO-OUT THE ENTIRE STACK (BELOW THE BOOKKEEPING PART)               
C                                                                       
      CALL ISTKRL(1)                                                    
      DO 1 J = 11,1000                                                  
  1    ISTAK(J) = 0                                                     
C                                                                       
C  THEN ALLOCATE FIRST 10 INTEGERS AND THEN 10 REALS.                   
C                                                                       
       IPNTR = ISTKGT(10,2)                                             
       RPNTR = ISTKGT(10,3)                                             
C                                                                       
      DO 2 J=1,10                                                       
        K = IPNTR+J-1                                                   
       KK = RPNTR+J-1                                                   
        ISTAK(K) = J                                                    
  2     RSTAK(KK) = FLOAT(J)                                            
C                                                                       
C  CALL STACK DUMP                                                      
C                                                                       
      CALL STKDMP                                                       
C                                                                       
C  NOW OVERWRITE PART OF THE STACK, SAY WITH 77                         
C                                                                       
      DO 3 J=25,35                                                      
  3     ISTAK(J) = 77                                                   
C                                                                       
C  DUMP THIS                                                            
C                                                                       
      CALL STKDMP                                                       
C                                                                       
C  NOW OVERWRITE EVEN THE BOOKKEEPING PART OF THE STACK                 
C                                                                       
C  AND DUMP THINGS.                                                     
C                                                                       
      DO 4 J=1,10                                                       
  4     ISTAK(J) = 99                                                   
C                                                                       
      CALL STKDMP                                                       
C                                                                       
      STOP                                                              
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
      COMPLEX B(N),V                                                    
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
      DOUBLE PRECISION B(N),V                                           
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
      INTEGER B(N),V                                                    
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
      LOGICAL B(N),V                                                    
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
      REAL B(N),V                                                       
C                                                                       
      IF(N .LE. 0) RETURN                                               
C                                                                       
      DO 10 I = 1, N                                                    
 10     B(I) = V                                                        
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
C$TEST RNRM                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM RNORM                                       
C                                                                       
C***********************************************************************
      INTEGER IWRITE,J, K                                               
      INTEGER COUNT(80)                                                 
      REAL X, RNORM                                                     
C                                                                       
C  ZERO THE COUNTERS                                                    
C                                                                       
      DO 5 K=1,80                                                       
  5    COUNT(K) = 0                                                     
C                                                                       
C  SET UP THE OUTPUT WRITE UNIT                                         
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      DO 10 K=1,20000                                                   
      X = RNORM(0)                                                      
      IF (X .LT. (-3.E0)) COUNT(79)=COUNT(79)+1                         
      IF (X .GT. 3.E0) COUNT(80)=COUNT(80)+1                            
      IF (ABS(X) .GT. 3.E0) GO TO 10                                    
C                                                                       
      J = (X + 4.E0)*10.E0 + 1.E0                                       
      COUNT(J) = COUNT(J)+1                                             
 10     CONTINUE                                                        
C                                                                       
      WRITE(IWRITE,99) COUNT                                            
 99      FORMAT(1H0,10I5)                                               
      STOP                                                              
      END                                                               
C$TEST PLYA                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM TRIGONOMETRIC POLYNOMIAL ROUTINES           
C                                                                       
C***********************************************************************
      INTEGER IWRITE                                                    
      REAL COEF1(1001),COEF2(1001),X,STEP                               
      DOUBLE PRECISION X0,X1,Y0,Y1,Z0,Z1,T0(2),T1(2),T2(2)              
      IWRITE = I1MACH(2)                                                
      WRITE(IWRITE,10)                                                  
 10   FORMAT(3H  X,14X,3HSUM,25X,10HTRIGP A.E.,5X,10HTRIGP R.E.,5X,     
     111HHORNER A.E.,4X,11HHORNER R.E.)                                 
      I = 1000                                                          
      STEP = 1.0/1024.0                                                 
      PI2 = 3.141592653/2.0                                             
      X=STEP                                                            
23000 IF(.NOT.(X.LE.PI2))GOTO 23002                                     
      X0 = X                                                            
      T0(1) = 1.0D0 - DCOS(FLOAT(I)*X0)                                 
      T0(2) = -DSIN(FLOAT(I)*X0)                                        
      T1(1) = 1.0D0 - DCOS(X0)                                          
      T1(2) = -DSIN(X0)                                                 
      IF(.NOT.(T1(1) .EQ. 0.0D0 .AND. T1(2) .EQ. 0.0D0))GOTO 23003      
      T2(1) =FLOAT(I)                                                   
      T2(2) = 0.0D0                                                     
      GOTO 23004                                                        
23003 CONTINUE                                                          
      CALL CDDIV(T0,T1,T2)                                              
23004 CONTINUE                                                          
      DO 23005 J=1,I                                                    
      COEF1(J) = 1.0                                                    
      COEF2(J) = 0.0                                                    
23005 CONTINUE                                                          
      X0 = TRIGP(I-1,COEF1,COEF2,X)                                     
      X1 = TRIG01(COEF1,COEF2,I,X)                                      
      Y0 = DABS(T2(1) - X0)                                             
      Y1 = DABS(T2(1) - X1)                                             
      Z0 = Y0/DABS(T2(1))                                               
      Z1 = Y1/DABS(T2(1))                                               
      WRITE(IWRITE,20) X,T2(1),Y0,Z0,Y1,Z1                              
20    FORMAT(1PE15.6,D25.17,4D15.4)                                     
      X0 = TRIGP(I-1,COEF2,COEF1,X)                                     
      X1 = TRIG01(COEF2,COEF1,I,X)                                      
      Y0 = DABS(T2(2) - X0)                                             
      Y1 = DABS(T2(2) - X1)                                             
      Z0 = Y0/DABS(T2(2))                                               
      Z1 = Y1/DABS(T2(2))                                               
      WRITE(IWRITE,30) T2(2),Y0,Z0,Y1,Z1                                
30    FORMAT(15X,1PD25.17,4D15.4)                                       
       X=X+STEP                                                         
      GOTO 23000                                                        
23002 CONTINUE                                                          
      STOP                                                              
      END                                                               
      FUNCTION TRIG01(ALFA,BETA,NP1,THETA)                              
      INTEGERNP1,K                                                      
      REAL ALFA(NP1),BETA(NP1),THETA,C,S,MU,GK,GK1,DK,DK1               
      IF(.NOT.(NP1 .LE. 0))GOTO 23007                                   
C/6S                                                                    
C     CALL SETERR(26HTRIG01 - INVALID DIMENSION,26,1,2)                 
C/7S                                                                    
      CALL SETERR('TRIG01 - INVALID DIMENSION',26,1,2)                  
C/                                                                      
      GOTO 23008                                                        
23007 CONTINUE                                                          
      IF(.NOT.(NP1 .EQ. 1))GOTO 23009                                   
      TRIG01 = ALFA(1)                                                  
      GOTO 23010                                                        
23009 CONTINUE                                                          
      C = COS(THETA)                                                    
      S = SIN(THETA)                                                    
      GK1 = ALFA(NP1)                                                   
      DK1 = BETA(NP1)                                                   
      K=NP1                                                             
23011 IF(.NOT.(K.GE.2))GOTO 23013                                       
      GK = C*GK1 + S*DK1                                                
      DK = -S*GK1 + C*DK1                                               
      GK1 = GK + ALFA(K-1)                                              
      DK1 = DK + BETA(K-1)                                              
       K=K-1                                                            
      GOTO 23011                                                        
23013 CONTINUE                                                          
      TRIG01 = ALFA(1) + GK                                             
23010 CONTINUE                                                          
23008 CONTINUE                                                          
      RETURN                                                            
      END                                                               
C$TEST PLYC                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT ORTHOGONAL POLYNOMIAL ROUTINES                      
C                                                                       
C***********************************************************************
      INTEGER IWRITE                                                    
      REAL COEF1(1001),COEF2(1001),A(1001),B(1001),C(1001),X,X0,X1,Y0,  
     1Y1,Y2,RHO                                                         
      DOUBLE PRECISION T0(2),T1(2),T2(2)                                
      REAL TCHBP,ORTHP,TRIGP                                            
      IWRITE = I1MACH(2)                                                
      GO TO 1                                                           
 1    WRITE(IWRITE,5)                                                   
 5    FORMAT(10X,11HTCHEBYCHEFF,14X,5HTCHBP,19X,5HORTHP)                
      DO 23000 I=1,20                                                   
      A(I) = 2.0                                                        
      B(I) = 0.0                                                        
      C(I) = -1.0                                                       
23000 CONTINUE                                                          
      A(1) = 1.0                                                        
      DO 23002 I=1,20                                                   
      DO 23004 K=1,I                                                    
      COEF1(K) = 0.0                                                    
23004 CONTINUE                                                          
      COEF1(I) = 1.0                                                    
      X=-1.0                                                            
23006 IF(.NOT.(X.LE.1.0))GOTO 23008                                     
      IF(.NOT.(I.EQ.1))GOTO 23009                                       
      Y0 = 1.0                                                          
      GOTO 23010                                                        
23009 CONTINUE                                                          
      IF(.NOT.(I.EQ.2))GOTO 23011                                       
      Y0 = X                                                            
      GOTO 23012                                                        
23011 CONTINUE                                                          
      Y2 = 1.0                                                          
      Y1 = X                                                            
      DO 23013 K=3,I                                                    
      Y0 = 2.0*X*Y1 - Y2                                                
      Y2 = Y1                                                           
      Y1 = Y0                                                           
23013 CONTINUE                                                          
23012 CONTINUE                                                          
23010 CONTINUE                                                          
      IM1 = I-1                                                         
      Y1 = TCHBP(IM1,COEF1,X,-1.0,1.0)                                  
      Y2 = ORTHP(IM1,COEF1,X,A,B,C)                                     
      WRITE(IWRITE,10) IM1,Y0,Y1,Y2                                     
 10   FORMAT(I5,1P4E25.17)                                              
       X=X+0.25                                                         
      GOTO 23006                                                        
23008 CONTINUE                                                          
23002 CONTINUE                                                          
 15   WRITE(IWRITE,20)                                                  
20    FORMAT(10X,8HLEGENDRE,17X,5HORTHP)                                
      A(1) = 1.0                                                        
      B(1) = 0.0                                                        
      C(1) = 0.0                                                        
      DO 23015 I=1,19                                                   
      A(I+1) = FLOAT(2*I+1)/FLOAT(I+1)                                  
      B(I+1) = 0.0                                                      
      C(I+1) = -FLOAT(I)/FLOAT(I+1)                                     
23015 CONTINUE                                                          
      DO 23017 I=1,20                                                   
      DO 23019 K=1,20                                                   
      COEF1(K) = 0.0                                                    
23019 CONTINUE                                                          
      COEF1(I) = 1.0                                                    
      X=-1.0                                                            
23021 IF(.NOT.(X.LE.1.0))GOTO 23023                                     
      IF(.NOT.(I.EQ.1))GOTO 23024                                       
      Y0 = 1.0                                                          
      GOTO 23025                                                        
23024 CONTINUE                                                          
      IF(.NOT.(I.EQ.2))GOTO 23026                                       
      Y0 = X                                                            
      GOTO 23027                                                        
23026 CONTINUE                                                          
      Y2 = 1.0                                                          
      Y1 = X                                                            
      DO 23028 K=3,I                                                    
      Y0 = A(K-1)*X*Y1 + C(K-1)*Y2                                      
      Y2 = Y1                                                           
      Y1 = Y0                                                           
23028 CONTINUE                                                          
23027 CONTINUE                                                          
23025 CONTINUE                                                          
      IM1 = I-1                                                         
      Y1 = ORTHP(IM1,COEF1,X,A,B,C)                                     
      WRITE(IWRITE,10) IM1,Y0,Y1                                        
       X=X+0.25                                                         
      GOTO 23021                                                        
23023 CONTINUE                                                          
23017 CONTINUE                                                          
25    WRITE(IWRITE,30)                                                  
30    FORMAT(10X,8HLAGUERRE,17X,5HORTHP)                                
      A(1) = -1.0                                                       
      B(1) = 1.0                                                        
      C(1) = 0.0                                                        
      DO 23030 I=1,19                                                   
      A(I+1) = -1.0                                                     
      B(I+1) = FLOAT(2*I+1)                                             
      C(I+1) = -FLOAT(I*I)                                              
23030 CONTINUE                                                          
      DO 23032 I=1,20                                                   
      DO 23034 K=1,20                                                   
      COEF1(K) = 0.0                                                    
23034 CONTINUE                                                          
      COEF1(I) = 1.0                                                    
      X=0.0                                                             
23036 IF(.NOT.(X.LE.1.0D1))GOTO 23038                                   
      IF(.NOT.(I.EQ.1))GOTO 23039                                       
      Y0 = 1.0                                                          
      GOTO 23040                                                        
23039 CONTINUE                                                          
      IF(.NOT.(I.EQ.2))GOTO 23041                                       
      Y0 = -X + 1.0                                                     
      GOTO 23042                                                        
23041 CONTINUE                                                          
      Y2 = 1.0                                                          
      Y1 = -X + 1.0                                                     
      DO 23043 K=3,I                                                    
      Y0 = (A(K-1)*X+B(K-1))*Y1 + C(K-1)*Y2                             
      Y2 = Y1                                                           
      Y1 = Y0                                                           
23043 CONTINUE                                                          
23042 CONTINUE                                                          
23040 CONTINUE                                                          
      IM1 = I-1                                                         
      Y1 = ORTHP(IM1,COEF1,X,A,B,C)                                     
      WRITE(IWRITE,10) IM1,Y0,Y1                                        
       X=X+0.5                                                          
      GOTO 23036                                                        
23038 CONTINUE                                                          
23032 CONTINUE                                                          
35    WRITE(IWRITE,40)                                                  
40    FORMAT(10X,3HCOS,22X,3HSIN,22X,5HTRIGP,19X,5HTRIGP)               
      DO 23045 I=1,20                                                   
      COEF1(I) = 0.0                                                    
      COEF2(I) = 0.0                                                    
23045 CONTINUE                                                          
      DO 23047 I=1,20                                                   
      IM1 = I-1                                                         
      X=-1.0                                                            
23049 IF(.NOT.(X.LE.1.0))GOTO 23051                                     
      X0 = COS(FLOAT(IM1)*X)                                            
      X1 = SIN(FLOAT(IM1)*X)                                            
      COEF1(I) = 1.0                                                    
      Y1 = TRIGP(IM1,COEF1,COEF2,X)                                     
      COEF1(I) = 0.0                                                    
      COEF2(I) = 1.0                                                    
      Y2 = TRIGP(IM1,COEF1,COEF2,X)                                     
      COEF2(I) = 0.0                                                    
      WRITE(IWRITE,10) IM1,X0,X1,Y1,Y2                                  
       X=X+0.25                                                         
      GOTO 23049                                                        
23051 CONTINUE                                                          
23047 CONTINUE                                                          
50    CONTINUE                                                          
      RHO=1.0                                                           
23052 IF(.NOT.(RHO.GE.0.5E0))GOTO 23054                                 
      WRITE(IWRITE,55) RHO                                              
55    FORMAT(F5.2,5X,7HCOS SUM,18X,7HSIN SUM,18X,5HTRIGP,19X,5HTRIGP)   
      I=10                                                              
23055 IF(.NOT.(I.LE.1000))GOTO 23057                                    
      IM1 = I-1                                                         
      X=-1.0                                                            
23058 IF(.NOT.(X.LE.1.0))GOTO 23060                                     
      T0(1) = 1.0 - RHO**(I)*COS(FLOAT(I)*X)                            
      T0(2) = -RHO**(I)*SIN(FLOAT(I)*X)                                 
      T1(1) = 1.0 - RHO*COS(X)                                          
      T1(2) = -RHO*SIN(X)                                               
      IF(.NOT.(T1(1) .EQ. 0.0 .AND. T1(2) .EQ. 0.0))GOTO 23061          
      T2(1) =FLOAT(I)                                                   
      T2(2) = 0.0                                                       
      GOTO 23062                                                        
23061 CONTINUE                                                          
      CALL CDDIV(T0,T1,T2)                                              
23062 CONTINUE                                                          
      COEF1(1) = 1.0                                                    
      COEF2(1) = 0.0                                                    
      DO 23063 J=2,I                                                    
      COEF1(J) = RHO*COEF1(J-1)                                         
      COEF2(J) = 0.0                                                    
23063 CONTINUE                                                          
      Y0 = TRIGP(IM1,COEF1,COEF2,X)                                     
      COEF1(1) = 0.0                                                    
      COEF2(1) = 1.0                                                    
      DO 23065 J=2,I                                                    
      COEF1(J) = 0.0                                                    
      COEF2(J) = RHO*COEF2(J-1)                                         
23065 CONTINUE                                                          
      COEF2(1) = 0.0                                                    
      Y1 = TRIGP(IM1,COEF1,COEF2,X)                                     
      WRITE(IWRITE,10) IM1,T2(1),T2(2),Y0,Y1                            
       X=X+0.25E0                                                       
      GOTO 23058                                                        
23060 CONTINUE                                                          
       I=I*10                                                           
      GOTO 23055                                                        
23057 CONTINUE                                                          
       RHO=RHO-0.25E0                                                   
      GOTO 23052                                                        
23054 CONTINUE                                                          
      STOP                                                              
      END                                                               
C$TEST EXTR                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM EXTRMR                                      
C                                                                       
C***********************************************************************
C     TEST OF THE EXTREMAL FINDER.                                      
      REAL     PI,STEP,X,F(100)                                         
      INTEGER  IWRITE,IEXT(100),NEX,IMAX,IMIN,IMAG                      
C                                                                       
      IWRITE = I1MACH(2)                                                
      PI = 4.0*ATAN(1.0)                                                
      STEP = 2.0*PI/99.0                                                
      DO 10 I=1,100                                                     
          X = STEP*FLOAT(I-1)                                           
 10       F(I) = EXP(-X)*COS(X)                                         
C                                                                       
      CALL EXTRMR(100,F,NEX,IEXT,IMAX,IMIN,IMAG)                        
C                                                                       
      WRITE(IWRITE,20)                                                  
20    FORMAT(6X,9HEXTREMALS/5X,1HX,10X,4HF(X))                          
      DO 30 J=1,NEX                                                     
          I = IEXT(J)                                                   
          X = STEP*FLOAT(I-1)                                           
30        WRITE(IWRITE,40) X,F(I)                                       
40    FORMAT(2F10.5)                                                    
      STOP                                                              
      END                                                               
C$TEST MNTB                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT MONOTONICITY ROUTINES                               
C                                                                       
C***********************************************************************
      INTEGER IX(12), I, INC, IWRITE, I1MACH                            
      REAL RX(12)                                                       
      LOGICAL SMONOD, SMONOI, SMONOR, MONOD, MONOI, MONOR               
      DOUBLE PRECISION DX(12)                                           
      DATA INC/1/                                                       
      DATA IX(1)/-4/                                                    
      DATA IX(2)/-4/                                                    
      DATA IX(3)/-2/                                                    
      DATA IX(4)/0/                                                     
      DATA IX(5)/2/                                                     
      DATA IX(6)/4/                                                     
      DATA IX(7)/4/                                                     
      DATA IX(8)/2/                                                     
      DATA IX(9)/0/                                                     
      DATA IX(10)/-2/                                                   
      DATA IX(11)/-4/                                                   
      DATA IX(12)/-4/                                                   
C     THIS PROCEDURE TESTS THE MONOTONICITY ROUTINES                    
      IWRITE = I1MACH(2)                                                
      WRITE (IWRITE,  1)                                                
   1  FORMAT (47H0THIS PROCEDURE TESTS THE MONOTONICITY ROUTINES)       
      IF ((.NOT. MONOI(IX, 0, INC)) .OR. (.NOT. MONOI(IX, 1, INC))      
     1    .OR. (.NOT. MONOI(IX, 2, INC)) .OR. (.NOT. MONOI(IX, 3, INC)) 
     2    .OR. (.NOT. MONOI(IX, 4, INC)) .OR. (.NOT. MONOI(IX, 5, INC)) 
     3    .OR. (.NOT. MONOI(IX(2), 5, INC)) .OR. (.NOT. MONOI(IX(3), 5  
     4   , INC)) .OR. MONOI(IX(4), 5, INC) .OR. MONOI(IX(5), 5, INC)    
     5    .OR. (.NOT. MONOI(IX(6), 5, INC)) .OR. (.NOT. MONOI(IX(7), 5  
     6   , INC)) .OR. (.NOT. MONOI(IX(8), 5, INC)) .OR. (.NOT. MONOI(IX(
     7   9), 4, INC)) .OR. (.NOT. MONOI(IX(10), 3, INC))) GOTO 3        
         WRITE (IWRITE,  2)                                             
   2     FORMAT (18H MONOI PASSED TEST)                                 
         GOTO  5                                                        
   3     WRITE (IWRITE,  4)                                             
   4     FORMAT (18H MONOI FAILED TEST)                                 
   5  DO  6 I = 1, 12                                                   
         RX(I) = IX(I)                                                  
   6     CONTINUE                                                       
      IF ((.NOT. MONOR(RX, 0, INC)) .OR. (.NOT. MONOR(RX, 1, INC))      
     1    .OR. (.NOT. MONOR(RX, 2, INC)) .OR. (.NOT. MONOR(RX, 3, INC)) 
     2    .OR. (.NOT. MONOR(RX, 4, INC)) .OR. (.NOT. MONOR(RX, 5, INC)) 
     3    .OR. (.NOT. MONOR(RX(2), 5, INC)) .OR. (.NOT. MONOR(RX(3), 5  
     4   , INC)) .OR. MONOR(RX(4), 5, INC) .OR. MONOR(RX(5), 5, INC)    
     5    .OR. (.NOT. MONOR(RX(6), 5, INC)) .OR. (.NOT. MONOR(RX(7), 5  
     6   , INC)) .OR. (.NOT. MONOR(RX(8), 5, INC)) .OR. (.NOT. MONOR(RX(
     7   9), 4, INC)) .OR. (.NOT. MONOR(RX(10), 3, INC))) GOTO 8        
         WRITE (IWRITE,  7)                                             
   7     FORMAT (18H MONOR PASSED TEST)                                 
         GOTO  10                                                       
   8     WRITE (IWRITE,  9)                                             
   9     FORMAT (18H MONOR FAILED TEST)                                 
  10  DO  11 I = 1, 12                                                  
         DX(I) = IX(I)                                                  
  11     CONTINUE                                                       
      IF ((.NOT. MONOD(DX, 0, INC)) .OR. (.NOT. MONOD(DX, 1, INC))      
     1    .OR. (.NOT. MONOD(DX, 2, INC)) .OR. (.NOT. MONOD(DX, 3, INC)) 
     2    .OR. (.NOT. MONOD(DX, 4, INC)) .OR. (.NOT. MONOD(DX, 5, INC)) 
     3    .OR. (.NOT. MONOD(DX(2), 5, INC)) .OR. (.NOT. MONOD(DX(3), 5  
     4   , INC)) .OR. MONOD(DX(4), 5, INC) .OR. MONOD(DX(5), 5, INC)    
     5    .OR. (.NOT. MONOD(DX(6), 5, INC)) .OR. (.NOT. MONOD(DX(7), 5  
     6   , INC)) .OR. (.NOT. MONOD(DX(8), 5, INC)) .OR. (.NOT. MONOD(DX(
     7   9), 4, INC)) .OR. (.NOT. MONOD(DX(10), 3, INC))) GOTO 13       
         WRITE (IWRITE,  12)                                            
  12     FORMAT (18H MONOD PASSED TEST)                                 
         GOTO  15                                                       
  13     WRITE (IWRITE,  14)                                            
  14     FORMAT (18H MONOD FAILED TEST)                                 
  15  IF ((.NOT. SMONOI(IX, 0, INC)) .OR. (.NOT. SMONOI(IX, 1, INC))    
     1    .OR. SMONOI(IX, 2, INC) .OR. SMONOI(IX, 3, INC) .OR. SMONOI(  
     2   IX, 4, INC) .OR. SMONOI(IX, 5, INC) .OR. (.NOT. SMONOI(IX(2), 5
     3   , INC)) .OR. SMONOI(IX(3), 5, INC) .OR. SMONOI(IX(4), 5, INC)  
     4    .OR. SMONOI(IX(5), 5, INC) .OR. SMONOI(IX(6), 5, INC) .OR. (  
     5   .NOT. SMONOI(IX(7), 5, INC)) .OR. SMONOI(IX(8), 5, INC) .OR.   
     6   SMONOI(IX(9), 4, INC) .OR. SMONOI(IX(10), 3, INC)) GOTO 17     
         WRITE (IWRITE,  16)                                            
  16     FORMAT (20H  SMONOI PASSED TEST)                               
         GOTO  19                                                       
  17     WRITE (IWRITE,  18)                                            
  18     FORMAT (20H  SMONOI FAILED TEST)                               
  19  IF ((.NOT. SMONOR(RX, 0, INC)) .OR. (.NOT. SMONOR(RX, 1, INC))    
     1    .OR. SMONOR(RX, 2, INC) .OR. SMONOR(RX, 3, INC) .OR. SMONOR(  
     2   RX, 4, INC) .OR. SMONOR(RX, 5, INC) .OR. (.NOT. SMONOR(RX(2), 5
     3   , INC)) .OR. SMONOR(RX(3), 5, INC) .OR. SMONOR(RX(4), 5, INC)  
     4    .OR. SMONOR(RX(5), 5, INC) .OR. SMONOR(RX(6), 5, INC) .OR. (  
     5   .NOT. SMONOR(RX(7), 5, INC)) .OR. SMONOR(RX(8), 5, INC) .OR.   
     6   SMONOR(RX(9), 4, INC) .OR. SMONOR(RX(10), 3, INC)) GOTO 21     
         WRITE (IWRITE,  20)                                            
  20     FORMAT (20H  SMONOR PASSED TEST)                               
         GOTO  23                                                       
  21     WRITE (IWRITE,  22)                                            
  22     FORMAT (20H  SMONOR FAILED TEST)                               
  23  IF ((.NOT. SMONOD(DX, 0, INC)) .OR. (.NOT. SMONOD(DX, 1, INC))    
     1    .OR. SMONOD(DX, 2, INC) .OR. SMONOD(DX, 3, INC) .OR. SMONOD(  
     2   DX, 4, INC) .OR. SMONOD(DX, 5, INC) .OR. (.NOT. SMONOD(DX(2), 5
     3   , INC)) .OR. SMONOD(DX(3), 5, INC) .OR. SMONOD(DX(4), 5, INC)  
     4    .OR. SMONOD(DX(5), 5, INC) .OR. SMONOD(DX(6), 5, INC) .OR. (  
     5   .NOT. SMONOD(DX(7), 5, INC)) .OR. SMONOD(DX(8), 5, INC) .OR.   
     6   SMONOD(DX(9), 4, INC) .OR. SMONOD(DX(10), 3, INC)) GOTO 25     
         WRITE (IWRITE,  24)                                            
  24     FORMAT (20H  SMONOD PASSED TEST)                               
         GOTO  27                                                       
  25     WRITE (IWRITE,  26)                                            
  26     FORMAT (20H  SMONOD FAILED TEST)                               
  27  STOP                                                              
      END                                                               
C$TEST GAMA                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM GAMMA                                       
C                                                                       
C***********************************************************************
             REAL A(10), GAMMA, PI                                      
             INTEGER WWIDTH, EWIDTH, IWRITE                             
C                                                                       
C   TEST THE PORT 3 GAMMA FUNCTION.                                     
C                                                                       
C   TEST A FEW SELECT VALUES, ESPECIALLY THOSE SPANNING REGIONS WHERE   
C   THE METHODS OF EVALUATION ARE DIFFERENT.                            
C   FOR EXAMPLE, SINCE GAMMA(-12.5) AND GAMMA(-11.5) ARE EVALUATED WITH 
C   DIFFERENT FORMULAS, CHECK THAT THE RELATIONSHIP                     
C   X*GAMMA(X)=GAMMA(1+X) HOLDS.                                        
C                                                                       
C   FIND THE FORMAT AND OUTPUT UNIT FOR PRINTING ON THE TARGET MACHINE. 
C                                                                       
       CALL FRMATR(WWIDTH, EWIDTH)                                      
       IWRITE = I1MACH(2)                                               
C                                                                       
C   THE LARGEST FITTING ERROR FOR X BETWEEN 1.0 AND 2.0 SEEMS TO BE     
C   AROUND X=1.640625 (NEARBY MULTIPLE OF INVERSE POWER OF TWO).        
C   THE ANSWER (USING RICHARD BRENT'S MULTIPLE PRECCISION PACKAGE)      
C   SHOULD BE ABOUT .8987319511828340762537627180621745626143           
C                                                                       
C                                                                       
       A(1) = 1.640625E0                                                
       A(2) = GAMMA(A(1))                                               
       WRITE(IWRITE,91)                                                 
  91    FORMAT(37H FOR X = 1.640625, GAMMA(X) SHOULD BE)                
       WRITE(IWRITE,92)                                                 
  92    FORMAT(43H  .8987319511828340762537627180621745626143)          
       CALL APRNTR(A, 2, IWRITE, 80, WWIDTH, EWIDTH)                    
C                                                                       
C   CHECK FORMULA X*GAMMA(X) - GAMMA(X+1) FOR X = -12.5                 
C   SINCE GAMMA(-12.5) IS COMPUTED BY AN ASYMPTOTIC FORMULA             
C   (AND A DIVISION), WHEREAS GAMMA(-11.5.) IS COMPUTED BY              
C   A CHEBYSHEV APPROXIMATION (AND RANGE REDUCTION)                     
C                                                                       
       A(1) = -12.5E0                                                   
       A(2) = GAMMA(A(1))                                               
       A(3) = A(1) + 1.E0                                               
       A(3) = GAMMA(A(3))                                               
       A(4) = A(1)*A(2) - A(3)                                          
       WRITE(IWRITE,93)                                                 
  93    FORMAT(34H0X AND GAMMA(X) AND GAMMA(X+1) ARE)                   
       CALL APRNTR(A, 3, IWRITE, 80, WWIDTH, EWIDTH)                    
       WRITE(IWRITE,94)                                                 
  94    FORMAT(11H WITH ERROR)                                          
       CALL APRNTR(A(4), 1, IWRITE, 80, WWIDTH, EWIDTH)                 
C                                                                       
C   CHECK THAT GAMMA(-2.5)*GAMMA(3.5) = PI/SIN(-2.5PI)                  
C   HERE THE GAMMAS ARE COMPUTED BY CHEBYSHEV APPROXIMATION             
C   (AFTER RANGE REDUCTION TO 1 .LE. X .LE. 2.)                         
C                                                                       
       A(1) = -2.5E0                                                    
       A(2) =  3.5E0                                                    
       A(3) = GAMMA(A(1))                                               
       A(4) = GAMMA(A(2))                                               
       PI = 4.E0*ATAN(1.E0)                                             
       WRITE(IWRITE,95)                                                 
  95    FORMAT(52H0DIFFERENCE GAMMA(-2.5)*GAMMA(3.5)-PI/SIN(-2.5PI) IS) 
       A(5) = A(3)*A(4) - PI/SIN(-2.5E0*PI)                             
       CALL APRNTR(A(5), 1, IWRITE, 80, WWIDTH, EWIDTH)                 
C                                                                       
C   CHECK THE BOUNDARY BETWEEN THE CHEBYSHEV APPROXIMATION AND THE      
C   ASYMPTOTIC REGION.                                                  
C   THIS IS ALL BASED ON A DIVIDING LINE OF X=12 BETWEEN THE REGIONS.   
C                                                                       
       A(1) = 11.5E0                                                    
       A(2) = 12.5E0                                                    
       A(3) = GAMMA(A(1))                                               
       A(4) = GAMMA(A(2))                                               
       A(5) = A(1)*A(3) - A(4)                                          
       WRITE(IWRITE,96)                                                 
  96    FORMAT(15H0GAMMA(12.5) IS)                                      
       CALL APRNTR(A(4), 1, IWRITE, 80, WWIDTH, EWIDTH)                 
       WRITE(IWRITE,97)                                                 
  97    FORMAT(34H 11.5*GAMMA(11.5) - GAMMA(12.5) IS)                   
       CALL APRNTR(A(5), 1, IWRITE, 80, WWIDTH, EWIDTH)                 
C                                                                       
C    CHECK THE ASYMPTOTIC REGION FOR TWO CONSECUTIVE LARGISH INTEGERS.  
C                                                                       
       A(1) = 25.E0                                                     
       A(2) = 26.E0                                                     
       A(3) = GAMMA(A(2))                                               
       A(4) = A(1)*GAMMA(A(1)) - A(3)                                   
       WRITE(IWRITE,98)                                                 
  98    FORMAT(13H0GAMMA(26) IS)                                        
       CALL APRNTR(A(3), 1, IWRITE, 80, WWIDTH, EWIDTH)                 
       WRITE(IWRITE,99)                                                 
  99    FORMAT(27H 25GAMMA(25) - GAMMA(26) IS)                          
       CALL APRNTR(A(4), 1, IWRITE, 80, WWIDTH, EWIDTH)                 
C                                                                       
       STOP                                                             
       END                                                              
C$TEST GMAD                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM DGAMMA                                      
C                                                                       
C***********************************************************************
             DOUBLE PRECISION A(10), DGAMMA, PI, DSIN, DATAN            
             INTEGER WWIDTH, EWIDTH, IWRITE                             
C                                                                       
C   TEST THE PORT3 DGAMMA FUNCTION.                                     
C                                                                       
C   TEST A FEW SELECT VALUES, ESPECIALLY THOSE SPANNING REGIONS WHERE   
C   THE METHODS OF EVALUATION ARE DIFFERENT.                            
C   FOR EXAMPLE, SINCE GAMMA(-12.5) AND GAMMA(-11.5) ARE EVALUATED WITH 
C   DIFFERENT FORMULAS, CHECK THAT THE RELATIONSHIP                     
C   X*GAMMA(X)=GAMMA(1+X) HOLDS.                                        
C                                                                       
C   FIND THE FORMAT AND OUTPUT UNIT FOR PRINTING ON THE TARGET MACHINE. 
C                                                                       
       CALL FRMATD(WWIDTH, EWIDTH)                                      
       IWRITE = I1MACH(2)                                               
C                                                                       
C   THE LARGEST FITTING ERROR FOR X BETWEEN 1.0 AND 2.0 SEEMS TO BE     
C   AROUND X=1.640625 (NEARBY MULTIPLE OF INVERSE POWER OF TWO).        
C   THE ANSWER (USING RICHARD BRENT'S MULTIPLE PRECCISION PACKAGE)      
C   SHOULD BE ABOUT .8987319511828340762537627180621745626143           
C                                                                       
C                                                                       
       A(1) = 1.640625D0                                                
       A(2) = DGAMMA(A(1))                                              
       WRITE(IWRITE,91)                                                 
  91    FORMAT(37H FOR X = 1.640625, GAMMA(X) SHOULD BE)                
       WRITE(IWRITE,92)                                                 
  92    FORMAT(43H  .8987319511828340762537627180621745626143)          
       CALL APRNTD(A, 2, IWRITE, 80, WWIDTH, EWIDTH)                    
C                                                                       
C   CHECK FORMULA X*GAMMA(X) - GAMMA(X+1) FOR X = -12.5                 
C   SINCE GAMMA(-12.5) IS COMPUTED BY AN ASYMPTOTIC FORMULA             
C   (AND A DIVISION), WHEREAS GAMMA(-11.5.) IS COMPUTED BY              
C   A CHEBYSHEV APPROXIMATION (AND RANGE REDUCTION)                     
C                                                                       
       A(1) = -12.5D0                                                   
       A(2) = DGAMMA(A(1))                                              
       A(3) = A(1) + 1.D0                                               
       A(3) = DGAMMA(A(3))                                              
       A(4) = A(1)*A(2) - A(3)                                          
       WRITE(IWRITE,93)                                                 
  93    FORMAT(34H0X AND GAMMA(X) AND GAMMA(X+1) ARE)                   
       CALL APRNTD(A, 3, IWRITE, 80, WWIDTH, EWIDTH)                    
       WRITE(IWRITE,94)                                                 
  94    FORMAT(11H WITH ERROR)                                          
       CALL APRNTD(A(4), 1, IWRITE, 80, WWIDTH, EWIDTH)                 
C                                                                       
C   CHECK THAT GAMMA(-2.5)*GAMMA(3.5) = PI/SIN(-2.5PI)                  
C   HERE THE GAMMAS ARE COMPUTED BY CHEBYSHEV APPROXIMATION             
C   (AFTER RANGE REDUCTION TO 1 .LE. X .LE. 2.)                         
C                                                                       
       A(1) = -2.5D0                                                    
       A(2) =  3.5D0                                                    
       A(3) = DGAMMA(A(1))                                              
       A(4) = DGAMMA(A(2))                                              
       PI = 4.D0*DATAN(1.D0)                                            
       WRITE(IWRITE,95)                                                 
  95    FORMAT(52H0DIFFERENCE GAMMA(-2.5)*GAMMA(3.5)-PI/SIN(-2.5PI) IS) 
       A(5) = A(3)*A(4) - PI/DSIN(-2.5D0*PI)                            
       CALL APRNTD(A(5), 1, IWRITE, 80, WWIDTH, EWIDTH)                 
C                                                                       
C   CHECK THE BOUNDARY BETWEEN THE CHEBYSHEV APPROXIMATION AND THE      
C   ASYMPTOTIC REGION.                                                  
C   THIS IS ALL BASED ON A DIVIDING LINE OF X=12 BETWEEN THE REGIONS.   
C                                                                       
       A(1) = 11.5D0                                                    
       A(2) = 12.5D0                                                    
       A(3) = DGAMMA(A(1))                                              
       A(4) = DGAMMA(A(2))                                              
       A(5) = A(1)*A(3) - A(4)                                          
       WRITE(IWRITE,96)                                                 
  96    FORMAT(15H0GAMMA(12.5) IS)                                      
       CALL APRNTD(A(4), 1, IWRITE, 80, WWIDTH, EWIDTH)                 
       WRITE(IWRITE,97)                                                 
  97    FORMAT(34H 11.5*GAMMA(11.5) - GAMMA(12.5) IS)                   
       CALL APRNTD(A(5), 1, IWRITE, 80, WWIDTH, EWIDTH)                 
C                                                                       
C    CHECK THE ASYMPTOTIC REGION FOR TWO CONSECUTIVE LARGISH INTEGERS.  
C                                                                       
       A(1) = 25.D0                                                     
       A(2) = 26.D0                                                     
       A(3) = DGAMMA(A(2))                                              
       A(4) = A(1)*DGAMMA(A(1)) - A(3)                                  
       WRITE(IWRITE,98)                                                 
  98    FORMAT(13H0GAMMA(26) IS)                                        
       CALL APRNTD(A(3), 1, IWRITE, 80, WWIDTH, EWIDTH)                 
       WRITE(IWRITE,99)                                                 
  99    FORMAT(27H 25GAMMA(25) - GAMMA(26) IS)                          
       CALL APRNTD(A(4), 1, IWRITE, 80, WWIDTH, EWIDTH)                 
C                                                                       
       STOP                                                             
       END                                                              
C$TEST QBLG                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM QUAD                                        
C                                                                       
C***********************************************************************
C   BLUE'S QUADRATURE TESTER - SINGLE-PRECISION- OBTAINED 6/16/77.      
C                                                                       
C THIS PACKAGE CONSISTS OF THREE TEST SUBROUTINES AND A MAIN PROGRAM    
C                                                                       
C    A. TEST ON KAHANER'S 21 TEST INTEGRALS PUBLISHED IN MATHEMATICAL   
C       SOFTWARE, J. R. RICE, ED., ACADEMIC PRESS 1971.  MAIN PROGRAM   
C       AND FUNCTION SUBPROGRAM.                                        
C    B. VARY PARAMETER ALPHA AND ACCURACY EPSILON.  MAIN PROGRAM, TWO   
C       FUNCTION SUBPROGRAMS, AND ONE SUBROUTINE.                       
C    C. VARY NOISE IN FUNCTION.  MAIN PROGRAM AND FUNCTION SUBPROGRAM.  
C                                                                       
C                                                                       
      CALL ENTER(1)                                                     
      CALL TEST1                                                        
      CALL TEST2                                                        
      CALL TEST3                                                        
      STOP                                                              
      END                                                               
      SUBROUTINE TEST1                                                  
C                                                                       
C        FIRST OF THREE TEST PACKAGES                                   
C        TEST QUAD ON KAHANER'S 21 FUNCTIONS                            
C                                                                       
      DIMENSION A(22),B(22),ANS(22)                                     
      REAL A,B,ANS,EPS,F,ANSWER,ERREST                                  
      EXTERNAL F                                                        
      COMMON/WHICH/N,JCALL                                              
C                                                                       
      DATA A/0.E0,0.E0,0.E0,-1.E0,-1.E0,0.E0,0.E0,0.E0,0.E0,0.E0,0.E0,  
     *     0.E0,0.1E0,0.E0,0.E0,0.E0,.01E0,0.E0,0.E0,-1.E0,0.E0,0.E0/   
      DATA B/13*1.E0,3*10.E0,1.E0,3.1415927E0,4*1.E0/                   
      DATA ANS/1.7182818284E0,0.7E0,.6666666667E0,.4794282267E0,        
     *  1.5822329637E0,                                                 
     *  0.4E0,2.E0,.8669729873E0,1.154700669E0,.6931471806E0,           
     *  .3798854930E0,.7775046341E0,.009098645256E0,.5000002112E0,1.E0, 
     *  .4993638029E0,.1121395696E0,.8386763234E0,-1.E0,1.564396443E0,  
     *  2*0.2108027354E0/                                               
C                                                                       
C   SET THE OUTPUT UNIT TO IWRITE                                       
C                                                                       
       IWRITE = I1MACH(2)                                               
C                                                                       
      DO 1000 J=3,6,3                                                   
         EPS=10.**(-J)                                                  
         WRITE(IWRITE,1) EPS                                            
    1    FORMAT(1H1,10X,26HTEST OF QUAD ON KAHANER 21//                 
     *    10X,17HATTEMPED ACCURACY,1PE12.3)                             
    2    FORMAT(///12X,1HN,6X,4HTRUE,11X,4HCALC,                        
     *    9X,5HERROR,7X,7HEST ERR,4X,15HJCALL     KWARN//)              
         WRITE(IWRITE,2)                                                
          DO 900 N=1,22                                                 
             JCALL=0                                                    
             CALL QUAD(F,A(N),B(N),EPS,ANSWER,ERREST)                   
            CALL ERROFF                                                 
            KWARN=0                                                     
            IF (ERREST .GT. EPS) KWARN=1                                
             ERR=ANS(N)-ANSWER                                          
  900       WRITE(IWRITE,901) N,ANS(N),ANSWER,ERR,ERREST,JCALL,KWARN    
  901       FORMAT(10X,I3, 2F15.10,1P2E12.3,I6,I10)                     
 1000 CONTINUE                                                          
      RETURN                                                            
       END                                                              
      REAL FUNCTION F(X)                                                
      COMMON/WHICH/N,JCALL                                              
      REAL X,SECH,PI,Y                                                  
      DATA PI/3.14159E0/                                                
C                                                                       
      SECH(Y)=2.*EXP(-ABS(Y))/(1.+EXP(-2.*ABS(Y)))                      
C                                                                       
      JCALL=JCALL+1                                                     
       GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21     
     *  ,22),N                                                          
C                                                                       
    1 F=EXP(X)                                                          
      RETURN                                                            
    2 F=0.                                                              
      IF(X.GE.0.3E0) F=1.                                               
      RETURN                                                            
    3 F=SQRT(X)                                                         
      RETURN                                                            
    4 F=0.46E0*(EXP(X)+EXP(-X))-COS(X)                                  
      RETURN                                                            
    5 F=1./(X**4+X**2+0.9E0)                                            
      RETURN                                                            
    6 F=X*SQRT(X)                                                       
      RETURN                                                            
    7 F=0.                                                              
      IF(X.GT.0.E0) F=1./SQRT(X)                                        
      RETURN                                                            
    8 F=1./(1.+X**4)                                                    
      RETURN                                                            
    9 F=2./(2.+SIN( 10.*PI*X))                                          
      RETURN                                                            
   10 F=1./(1.+X)                                                       
      RETURN                                                            
   11 F=1./(1.+EXP(X))                                                  
      RETURN                                                            
   12 F=1.                                                              
      IF(X.GT.0.E0) F=X/(EXP(X)-1.E0)                                   
      RETURN                                                            
   13 F=100.                                                            
      IF(X.GT.0.E0) F=SIN(100.*PI*X)/(PI*X)                             
      RETURN                                                            
   14 F=SQRT(50.)*EXP(-50.*PI*X**2)                                     
      RETURN                                                            
   15 F=25.*EXP(-25.*X)                                                 
      RETURN                                                            
   16 F=50./(PI*(2500.*X**2+1.))                                        
      RETURN                                                            
   17 F=(SIN(50.*  PI   *X))**2/(50.*(  PI   *X)**2)                    
      RETURN                                                            
   18 F=COS( COS(X)+3.*SIN(X)+2.*COS(2.*X)+SIN(2.*X)+3.*COS(3.*X)       
     *   +2.*SIN(2.*X))                                                 
      RETURN                                                            
   19 F=0.                                                              
      IF(X.GT.0.E0) F=ALOG(X)                                           
      RETURN                                                            
   20 F=1./(X**2+1.005E0)                                               
      RETURN                                                            
   21 F=(SECH(  10.*(X-0.2E0)))**2                                      
     * +(SECH( 100.*(X-0.4E0)))**4                                      
     * +(SECH(1000.*(X-0.6E0)))**6                                      
      RETURN                                                            
   22 F=(SECH(  10.*(X-0.2E0)))**2                                      
     * +(SECH( 100.*(X-0.4E0)))**4                                      
     * +(SECH(1000.*(X-.61E0)))**6                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE TEST2                                                  
C                                                                       
C        SECOND TEST PACKAGE FOR QUAD                                   
C                                                                       
C        PARAMETER TESTING - ALPHA AND EPSILON                          
C                                                                       
      REAL EPS,A,B,ALF,ALPHA,ALIM,BLIM,BET,ALFNUM,V                     
      REAL FALF,ANS,ERREST                                              
      DIMENSION MAX(7),ALIM(7),BLIM(7),NFUN(7)                          
      DIMENSION JNUM(9),ALFNUM(9)                                       
      COMMON/FALFCM/ALF,V,BET,M,JCALL,PARAM                             
      LOGICAL PARAM,JPRINT                                              
      DATA MAX/30,50,69,101,25,1,25/                                    
      DATA NFUN/1,1,2,3,4,5,6/                                          
      DATA ALIM/-1.E0,6*0.E0/                                           
      DATA BLIM/7*1.E0/                                                 
      DATA JNUM/5,3,4,4,5*1/                                            
      DATA ALFNUM/3.E0,0.5E0,1.95E0,17.95E0,5*1.E0/                     
C                                                                       
C        ALPHA AS A PARAMETER                                           
C                                                                       
C                                                                       
C   SET THE OUTPUT UNIT TO IWRITE                                       
C                                                                       
       IWRITE = I1MACH(2)                                               
      EPS =1.E-6                                                        
      PARAM=.TRUE.                                                      
      DO 1000 JFUN=1,7                                                  
         N=NFUN(JFUN)                                                   
         MAXA=MAX(JFUN)                                                 
         MAXBET=1                                                       
         IF(JFUN.EQ.7) MAXBET=8                                         
         DO 900 JB=1,MAXBET                                             
            BET=2**JB                                                   
            IF(N.EQ.1) WRITE(IWRITE,1)                                  
            IF(N.EQ.2) WRITE(IWRITE,2)                                  
            IF(N.EQ.3) WRITE(IWRITE,3)                                  
            IF(N.EQ.4) WRITE(IWRITE,4)                                  
            IF(N.EQ.5) WRITE(IWRITE,5)                                  
            IF(N.EQ.6) WRITE(IWRITE,6)                                  
            A=ALIM(JFUN)                                                
            B=BLIM(JFUN)                                                
            IF(N.EQ.6) WRITE(IWRITE,11) BET                             
            WRITE(IWRITE,10) A,B,EPS                                    
            DO 50 J=1,MAXA                                              
               JJ=J                                                     
               ALF=ALPHA(JJ,N)                                          
               NN=JFUN                                                  
   50          CALL TEST(A,B,EPS,ALF,NN)                                
  900       CONTINUE                                                    
 1000    CONTINUE                                                       
C                                                                       
C ACCURACY AS A PARAMETER                                               
C                                                                       
      PARAM=.FALSE.                                                     
      BET=32.                                                           
      DO 2000 NUM=1,4                                                   
         JFUN=JNUM(NUM)                                                 
         N=NFUN(JFUN)                                                   
         ALF=ALFNUM(NUM)                                                
         IF(N.EQ.2) WRITE(IWRITE,2)                                     
         IF(N.EQ.3) WRITE(IWRITE,3)                                     
         IF(N.EQ.4) WRITE(IWRITE,4)                                     
         IF(N.EQ.6) WRITE(IWRITE,6)                                     
         IF(N.EQ.6) WRITE(IWRITE,11) BET                                
         WRITE(IWRITE,12) A,B,ALF                                       
         DO 1500 JEPS=2,12                                              
            EPS=10.**(2-JEPS)                                           
            NN=JFUN                                                     
 1500       CALL TEST(A,B,EPS,ALF,NN)                                   
 2000 CONTINUE                                                          
    1 FORMAT(51H0    TEST OF QUAD ON F(X)=(2**A)/(1.+(X*(2**A))**2))    
    2 FORMAT(30H0    TEST OF QUAD ON F(X)=X**A)                         
    3 FORMAT(41H0    TEST OF QUAD ON F(X))=1.+COS(A*PI*X))              
    4 FORMAT(47H0    TEST OF QUAD ON F(X)=(2**A)*EXP(-(2**A)*X))        
    5 FORMAT(52H0    TEST OF QUAD ON F(X)=(2**A)**2*X*EXP(-(2**A)*X))   
    6 FORMAT(27H0    TEST OF QUAD ON F(X)=B,                            
     X      20H*EXP(-B**2*(X-A)**2))                                    
   10 FORMAT(1H0,10X,6HLIMITS,2F6.2,21H   ATTEMPTED ACCURACY,1PE12.2/   
     X      /14X,1HA,8X,8HTRUE INT,2X,                                  
     X      8HTRUE ERR,4X,8HCALC ERR,4X,5HJCALL,6X,5HKWARN//)           
   12 FORMAT(1H0,12X,6HLIMITS,2F6.2,4H   A,1PE14.4/                     
     X      /12X,3HEPS,8X,8HTRUE INT,2X,                                
     X      8HTRUE ERR,4X,8HCALC ERR,4X,5HJCALL,6X,5HKWARN//)           
   11 FORMAT(17X,1HB,F7.2)                                              
      RETURN                                                            
      END                                                               
      REAL FUNCTION ALPHA(N,J)                                          
C                                                                       
      GO TO (1,2,3,4,5,6),J                                             
    1 ALPHA=N-1                                                         
      RETURN                                                            
    2 ALPHA=FLOAT(N-101)/100.E0                                         
      IF(N.GE.21) ALPHA=FLOAT(N-21)/20.E0-0.8E0                         
      IF(N.GE.37) ALPHA=FLOAT(N-37)/10.E0                               
      IF(N.GE.57) ALPHA=FLOAT(N-57)/4.E0+2.E0                           
      RETURN                                                            
    3 ALPHA=0.25E0*FLOAT(N)                                             
      IF(N.GE.81) ALPHA=35.5E0+FLOAT(N-81)/20.E0                        
      RETURN                                                            
    4 ALPHA=N-1                                                         
      RETURN                                                            
    5 ALPHA=0.5E0*FLOAT(N-1)                                            
      RETURN                                                            
    6 ALPHA=0.02E0*FLOAT(N)                                             
      RETURN                                                            
      END                                                               
      SUBROUTINE TEST(A,B,EPS,ALP,N)                                    
      COMMON/FALFCM/ALF,V,BET,M,JCALL,PARAM                             
      REAL A,B,ALF,EPS,ANS,ERREST,TRUE,F,ALP,BET,V,FALF                 
      LOGICAL PARAM                                                     
      EXTERNAL FALF                                                     
C                                                                       
C   SET THE OUTPUT UNIT TO IWRITE                                       
C                                                                       
       IWRITE = I1MACH(2)                                               
C                                                                       
C                                                                       
      JCALL=0                                                           
      M=N                                                               
      ALF=ALP                                                           
      V=2.**ALF                                                         
      CALL QUAD(FALF,A,B,EPS,ANS,ERREST)                                
      CALL ERROFF                                                       
      KWARN=0                                                           
      IF (ERREST .GT. EPS) KWARN=1                                      
      M=-N                                                              
      TRUE=FALF(0.E0)                                                   
      ERR=TRUE-ANS                                                      
      IF(     PARAM) WRITE(IWRITE,1) ALF,TRUE,ERR,ERREST,JCALL,KWARN    
      IF(.NOT.PARAM) WRITE(IWRITE,2) EPS,TRUE,ERR,ERREST,JCALL,KWARN    
      RETURN                                                            
    1 FORMAT(F17.4,F12.4,1P2E12.2,2I8)                                  
    2 FORMAT(5X,1PE12.2,0PF12.4,1P2E12.2,2I8)                           
      END                                                               
      REAL FUNCTION FALF(X)                                             
      REAL X,ALPHA,PI,ALF,B,V,ERFC,Y                                    
      COMMON/FALFCM/ALF,V,B,N,JCALL,PARAM                               
      LOGICAL PARAM                                                     
C                                                                       
      DATA PI/3.14159265E0/                                             
C                                                                       
C      7-DECIMAL ERFC, HART NUMBER 5662                                 
C                                                                       
      ERFC(Y)=EXP(-Y**2)*(3.5322166+Y*(2.1539977+Y*0.57404837))/        
     *   (3.5322162+Y*(6.1397195+Y*(3.9690912+Y)))                      
C                                                                       
      M=IABS(N)                                                         
      IF(N.LE.0) GO TO (100,101,102,103,104,105,106),M                  
      JCALL=JCALL+1                                                     
      GO TO (1,1,2,3,4,5,6),M                                           
C                                                                       
    1 FALF=V/(1.E0+(X*V)**2)                                            
      RETURN                                                            
    2 FALF=0.                                                           
      IF(X.GT.0.E0) FALF=X**ALF                                         
      RETURN                                                            
    3 FALF=1.E0+COS(ALF*X*PI)                                           
      RETURN                                                            
    4 FALF=V*EXP(-V*X)                                                  
      RETURN                                                            
    5 FALF=V**2*X*EXP(-V*X)                                             
      RETURN                                                            
    6 FALF=B*EXP(-(B*(X-ALF))**2)                                       
      RETURN                                                            
C                                                                       
  100 FALF=2.E0*ATAN(V)                                                 
      RETURN                                                            
  101 FALF=ATAN(V)                                                      
      RETURN                                                            
  102 FALF=R1MACH(2)                                                    
      IF (ALF .GT. (-1.E0)) FALF=1.E0/(1.E0+ALF)                        
      RETURN                                                            
  103 IF (ALF .NE. 0.E0) FALF=1.E0+SIN(ALF*PI)/(ALF*PI)                 
      IF (ALF .EQ. 0.E0) FALF=2.E0                                      
      RETURN                                                            
  104 FALF=1.E0-EXP(-V)                                                 
      RETURN                                                            
  105 FALF=1.E0-(1.E0+V)*EXP(-V)                                        
      RETURN                                                            
  106 FALF=SQRT(PI)*(1.E0-0.5*(ERFC(B*ALF)+ERFC(B*(1.E0-ALF))))         
      RETURN                                                            
      END                                                               
      SUBROUTINE TEST3                                                  
C                                                                       
C        THIRD PACKAGE TO TEST QUAD                                     
C                                                                       
C TEST QUAD FOR EFFECTS OF ROUNDOFF IN FUNCTION RATHER THAN MACHINE.    
C                                                                       
      EXTERNAL FRAND                                                    
      REAL FRAND,ANS,ERREST,RAND,TRUE,ERR                               
      COMMON/NOISE/RAND,JCALL,N,NR                                      
C                                                                       
C   SET THE OUTPUT UNIT TO IWRITE                                       
C                                                                       
       IWRITE = I1MACH(2)                                               
C                                                                       
C                                                                       
      DO 200 N=1,4                                                      
         RAND=0.E0                                                      
         JCALL = 0                                                      
         CALL QUAD(FRAND,0.E0,1.E0,1.E-7,TRUE,ERREST)                   
         CALL ERROFF                                                    
         KWARN=0                                                        
         IF (ERREST .GT. 1.E-10) KWARN=1                                
         WRITE(IWRITE,5) N,TRUE,ERREST,KWARN,JCALL                      
    5    FORMAT(1H0,I4,1PD15.8,1PD12.3,2I10)                            
         DO 150 NR=1,2                                                  
            WRITE(IWRITE,4) N,NR                                        
    4       FORMAT(2I5)                                                 
            DO 100 JR=1,9                                               
               JEXP=2-JR                                                
               RAND=10.E0**JEXP                                         
               WRITE(IWRITE,1) JEXP                                     
               WRITE(IWRITE,3)                                          
    1          FORMAT(18H0NOISE LEVEL 10**(,I3,1H))                     
    3          FORMAT(47H    "TRUE" ERR     EST ERR      KWARN     JCALL
     *)                                                                 
               DO 50 NN=1,5                                             
                  JCALL=0                                               
                  CALL QUAD(FRAND,0.E0,1.E0,1.E-5,ANS,ERREST)           
                  CALL ERROFF                                           
                  KWARN=0                                               
                  IF (ERREST .GT. 1.E-6) KWARN=1                        
                  ERR=TRUE-ANS                                          
                  WRITE(IWRITE,2) ERR,ERREST,KWARN,JCALL                
    2             FORMAT(1X,1P2D13.3,2I10)                              
   50          CONTINUE                                                 
  100       CONTINUE                                                    
  150    CONTINUE                                                       
  200 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      REAL FUNCTION FRAND(X)                                            
      REAL X,RAND,PI,RAN                                                
      COMMON/NOISE/RAND,JCALL,N,NR                                      
      DATA PI/3.14159265E0/                                             
C                                                                       
      JCALL=JCALL+1                                                     
C                                                                       
      RAN=RAND*(2.*UNI(0)-1.)                                           
         GO TO (1,2,3,4),N                                              
    1    FRAND=8.*EXP(-8.*X)                                            
         GO TO 10                                                       
    2    FRAND=SQRT(X)                                                  
         GO TO 10                                                       
    3    FRAND=1.+COS(1.95E0*PI*X)                                      
         GO TO 10                                                       
    4    FRAND=1.+COS(17.95E0*PI*X)                                     
         GO TO 10                                                       
   10 IF(NR.EQ.1) FRAND=FRAND+RAN                                       
      IF(NR.EQ.2) FRAND=FRAND*(1.E0+RAN)                                
      RETURN                                                            
      END                                                               
C$TEST QBGD                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM DQUAD                                       
C                                                                       
C***********************************************************************
C  BLUE'S QUADRATURE TESTER - DOUBLE-PRECISION _ OBTAINED 6/16/77.      
C                                                                       
C THIS PACKAGE CONSISTS OF THREE TEST SUBROUTINES AND A MAIN PROGRAM    
C                                                                       
C    A. TEST ON KAHANER'S 21 TEST INTEGRALS PUBLISHED IN MATHEMATICAL   
C       SOFTWARE, J. R. RICE, ED., ACADEMIC PRESS 1971.  MAIN PROGRAM   
C       AND FUNCTION SUBPROGRAM.                                        
C    B. VARY PARAMETER ALPHA AND ACCURACY EPSILON.  MAIN PROGRAM, TWO   
C       FUNCTION SUBPROGRAMS, AND ONE SUBROUTINE.                       
C    C. VARY NOISE IN FUNCTION.  MAIN PROGRAM AND FUNCTION SUBPROGRAM.  
C                                                                       
C                                                                       
      CALL ENTER(1)                                                     
      CALL TEST1                                                        
      CALL TEST2                                                        
      CALL TEST3                                                        
      STOP                                                              
      END                                                               
      SUBROUTINE TEST1                                                  
C                                                                       
C        FIRST OF THREE TEST PACKAGES                                   
C        TEST DQUAD ON KAHANER'S 21 FUNCTIONS                           
C                                                                       
      DIMENSION A(22),B(22),ANS(22)                                     
      DOUBLE PRECISION A,B,ANS,EPS,F,ANSWER,ERREST,ERR                  
      EXTERNAL F                                                        
      COMMON/WHICH/N,JCALL                                              
C                                                                       
      DATA A/0.D0,0.D0,0.D0,-1.D0,-1.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,  
     *     0.D0,0.1D0,0.D0,0.D0,0.D0,.01D0,0.D0,0.D0,-1.D0,0.D0,0.D0/   
      DATA B/13*1.D0,3*10.D0,1.D0,3.1415927D0,4*1.D0/                   
      DATA ANS/1.7182818284D0,0.7D0,.6666666667D0,.4794282267D0,        
     *  1.5822329637D0,                                                 
     *  0.4D0,2.D0,.8669729873D0,1.154700669D0,.6931471806D0,           
     *  .3798854930D0,.7775046341D0,.009098645256D0,.5000002112D0,1.D0, 
     *  .4993638029D0,.1121395696D0,.8386763234D0,-1.D0,1.564396443D0,  
     *  2*0.2108027354D0/                                               
C                                                                       
C   SET THE OUTPUT UNIT TO IWRITE                                       
C                                                                       
       IWRITE = I1MACH(2)                                               
C                                                                       
      DO 1000 J=3,9,3                                                   
         EPS=10.D0**(-J)                                                
         WRITE(IWRITE,1) EPS                                            
    1    FORMAT(1H1,10X,27HTEST OF DQUAD ON KAHANER 21//                
     *    10X,17HATTEMPED ACCURACY,1PD12.3)                             
    2    FORMAT(///12X,1HN,6X,4HTRUE,11X,4HCALC,                        
     *    9X,5HERROR,7X,7HEST ERR,4X,15HJCALL     KWARN//)              
         WRITE(IWRITE,2)                                                
          DO 900 N=1,22                                                 
             JCALL=0                                                    
             CALL DQUAD(F,A(N),B(N),EPS,ANSWER,ERREST)                  
            CALL ERROFF                                                 
            KWARN=0                                                     
            IF (ERREST .GT. EPS) KWARN=1                                
             ERR=ANS(N)-ANSWER                                          
  900       WRITE(IWRITE,901) N,ANS(N),ANSWER,ERR,ERREST,JCALL,KWARN    
  901       FORMAT(10X,I3, 2F15.10,1P2D12.3,I6,I10)                     
 1000 CONTINUE                                                          
      RETURN                                                            
       END                                                              
      DOUBLE PRECISION FUNCTION F(X)                                    
      COMMON/WHICH/N,JCALL                                              
      DOUBLE PRECISION X,SECH,PI,Y                                      
      DOUBLE PRECISION DEXP, DSQRT, DCOS, DSIN, DLOG                    
      DATA PI/3.14159D0/                                                
C                                                                       
      SECH(Y)=2.*DEXP(-DABS(Y))/(1.+DEXP(-2.*DABS(Y)))                  
C                                                                       
      JCALL=JCALL+1                                                     
       GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21     
     *  ,22),N                                                          
C                                                                       
    1 F=DEXP(X)                                                         
      RETURN                                                            
    2 F=0.                                                              
      IF(X.GE.0.3D0) F=1.                                               
      RETURN                                                            
    3 F=DSQRT(X)                                                        
      RETURN                                                            
    4 F=0.46D0*(DEXP(X)+DEXP(-X))-DCOS(X)                               
      RETURN                                                            
    5 F=1./(X**4+X**2+0.9D0)                                            
      RETURN                                                            
    6 F=X*DSQRT(X)                                                      
      RETURN                                                            
    7 F=0.                                                              
      IF(X.GT.0.D0) F=1./DSQRT(X)                                       
      RETURN                                                            
    8 F=1./(1.+X**4)                                                    
      RETURN                                                            
    9 F=2./(2.+DSIN( 10.*PI*X))                                         
      RETURN                                                            
   10 F=1./(1.+X)                                                       
      RETURN                                                            
   11 F=1./(1.+DEXP(X))                                                 
      RETURN                                                            
   12 F=1.                                                              
      IF(X.GT.0.D0) F=X/(DEXP(X)-1.D0)                                  
      RETURN                                                            
   13 F=100.                                                            
      IF(X.GT.0.D0) F=DSIN(100.*PI*X)/(PI*X)                            
      RETURN                                                            
   14 F=DSQRT(50.D0)*DEXP(-50.D0*PI*X**2)                               
      RETURN                                                            
   15 F=25.*DEXP(-25.*X)                                                
      RETURN                                                            
   16 F=50./(PI*(2500.*X**2+1.))                                        
      RETURN                                                            
   17 F=(DSIN(50.*  PI   *X))**2/(50.*(  PI   *X)**2)                   
      RETURN                                                            
   18 F=DCOS( DCOS(X)+3.*DSIN(X)+2.*DCOS(2.*X)+DSIN(2.*X)+3.*DCOS(3.*X) 
     *   +2.*DSIN(2.*X))                                                
      RETURN                                                            
   19 F=0.                                                              
      IF(X.GT.0.D0) F=DLOG(X)                                           
      RETURN                                                            
   20 F=1./(X**2+1.005D0)                                               
      RETURN                                                            
   21 F=(SECH(  10.*(X-0.2D0)))**2                                      
     * +(SECH( 100.*(X-0.4D0)))**4                                      
     * +(SECH(1000.*(X-0.6D0)))**6                                      
      RETURN                                                            
   22 F=(SECH(  10.*(X-0.2D0)))**2                                      
     * +(SECH( 100.*(X-0.4D0)))**4                                      
     * +(SECH(1000.*(X-.61D0)))**6                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE TEST2                                                  
C                                                                       
C        SECOND TEST PACKAGE FOR DQUAD                                  
C                                                                       
C        PARAMETER TESTING - ALPHA AND EPSILON                          
C                                                                       
      DOUBLE PRECISION EPS,A,B,ALF,ALPHA,ALIM,BLIM,BET,ALFNUM,V         
      DOUBLE PRECISION FALF,ANS,ERREST                                  
      DIMENSION MAX(7),ALIM(7),BLIM(7),NFUN(7)                          
      DIMENSION JNUM(9),ALFNUM(9)                                       
      COMMON/FALFCM/ALF,V,BET,M,JCALL,PARAM                             
      LOGICAL PARAM,JPRINT                                              
      DATA MAX/30,50,69,101,25,1,25/                                    
      DATA NFUN/1,1,2,3,4,5,6/                                          
      DATA ALIM/-1.D0,6*0.D0/                                           
      DATA BLIM/7*1.D0/                                                 
      DATA JNUM/5,3,4,4,5*1/                                            
      DATA ALFNUM/3.D0,0.5D0,1.95D0,17.95D0,5*1.D0/                     
C                                                                       
C        ALPHA AS A PARAMETER                                           
C                                                                       
      IWRITE = I1MACH(2)                                                
      EPS =1.D-6                                                        
      PARAM=.TRUE.                                                      
      DO 1000 JFUN=1,7                                                  
         N=NFUN(JFUN)                                                   
         MAXA=MAX(JFUN)                                                 
         MAXBET=1                                                       
         IF(JFUN.EQ.7) MAXBET=8                                         
         DO 900 JB=1,MAXBET                                             
            BET=2**JB                                                   
            IF(N.EQ.1) WRITE(IWRITE,1)                                  
            IF(N.EQ.2) WRITE(IWRITE,2)                                  
            IF(N.EQ.3) WRITE(IWRITE,3)                                  
            IF(N.EQ.4) WRITE(IWRITE,4)                                  
            IF(N.EQ.5) WRITE(IWRITE,5)                                  
            IF(N.EQ.6) WRITE(IWRITE,6)                                  
            A=ALIM(JFUN)                                                
            B=BLIM(JFUN)                                                
            IF(N.EQ.6) WRITE(IWRITE,11) BET                             
            WRITE(IWRITE,10) A,B,EPS                                    
            DO 50 J=1,MAXA                                              
               JJ=J                                                     
               ALF=ALPHA(JJ,N)                                          
               NN=JFUN                                                  
   50          CALL TEST(A,B,EPS,ALF,NN)                                
  900       CONTINUE                                                    
 1000    CONTINUE                                                       
C                                                                       
C ACCURACY AS A PARAMETER                                               
C                                                                       
      PARAM=.FALSE.                                                     
      BET=32.                                                           
      DO 2000 NUM=1,4                                                   
         JFUN=JNUM(NUM)                                                 
         N=NFUN(JFUN)                                                   
         ALF=ALFNUM(NUM)                                                
         IF(N.EQ.2) WRITE(IWRITE,2)                                     
         IF(N.EQ.3) WRITE(IWRITE,3)                                     
         IF(N.EQ.4) WRITE(IWRITE,4)                                     
         IF(N.EQ.6) WRITE(IWRITE,6)                                     
         IF(N.EQ.6) WRITE(IWRITE,11) BET                                
         WRITE(IWRITE,12) A,B,ALF                                       
         DO 1500 JEPS=2,22                                              
            EPS=10.D0**(2-JEPS)                                         
            NN=JFUN                                                     
 1500       CALL TEST(A,B,EPS,ALF,NN)                                   
 2000 CONTINUE                                                          
    1 FORMAT(52H0    TEST OF DQUAD ON F(X)=(2**A)/(1.+(X*(2**A))**2))   
    2 FORMAT(31H0    TEST OF DQUAD ON F(X)=X**A)                        
    3 FORMAT(42H0    TEST OF DQUAD ON F(X))=1.+COS(A*PI*X))             
    4 FORMAT(48H0    TEST OF DQUAD ON F(X)=(2**A)*EXP(-(2**A)*X))       
    5 FORMAT(53H0    TEST OF DQUAD ON F(X)=(2**A)**2*X*EXP(-(2**A)*X))  
    6 FORMAT(28H0    TEST OF DQUAD ON F(X)=B,                           
     X      20H*EXP(-B**2*(X-A)**2))                                    
   10 FORMAT(1H0,10X,6HLIMITS,2F6.2,21H   ATTEMPTED ACCURACY,1PD12.2/   
     X      /14X,1HA,8X,8HTRUE INT,2X,                                  
     X      8HTRUE ERR,4X,8HCALC ERR,4X,5HJCALL,6X,5HKWARN//)           
   12 FORMAT(1H0,12X,6HLIMITS,2F6.2,4H   A,1PD14.4/                     
     X      /12X,3HEPS,8X,8HTRUE INT,2X,                                
     X      8HTRUE ERR,4X,8HCALC ERR,4X,5HJCALL,6X,5HKWARN//)           
   11 FORMAT(17X,1HB,F7.2)                                              
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION ALPHA(N,J)                              
      DOUBLE PRECISION DFLOAT                                           
C                                                                       
      GO TO (1,2,3,4,5,6),J                                             
    1 ALPHA=N-1                                                         
      RETURN                                                            
    2 ALPHA=DFLOAT(N-101)/100.D0                                        
      IF(N.GE.21) ALPHA=DFLOAT(N-21)/20.D0-0.8D0                        
      IF(N.GE.37) ALPHA=DFLOAT(N-37)/10.D0                              
      IF(N.GE.57) ALPHA=DFLOAT(N-57)/4.D0+2.D0                          
      RETURN                                                            
    3 ALPHA=0.25D0*DFLOAT(N)                                            
      IF(N.GE.81) ALPHA=35.5D0+DFLOAT(N-81)/20.D0                       
      RETURN                                                            
    4 ALPHA=N-1                                                         
      RETURN                                                            
    5 ALPHA=0.5D0*DFLOAT(N-1)                                           
      RETURN                                                            
    6 ALPHA=0.02D0*DFLOAT(N)                                            
      RETURN                                                            
      END                                                               
      SUBROUTINE TEST(A,B,EPS,ALP,N)                                    
      COMMON/FALFCM/ALF,V,BET,M,JCALL,PARAM                             
      DOUBLE PRECISION A,B,ALF,EPS,ANS,ERREST,TRUE,F,ALP,BET,V,FALF,ERR 
      LOGICAL PARAM                                                     
      EXTERNAL FALF                                                     
C                                                                       
      IWRITE = I1MACH(2)                                                
      JCALL=0                                                           
      M=N                                                               
      ALF=ALP                                                           
      V=2.**ALF                                                         
      CALL DQUAD(FALF,A,B,EPS,ANS,ERREST)                               
      CALL ERROFF                                                       
      KWARN=0                                                           
      IF (ERREST .GT. EPS) KWARN=1                                      
      M=-N                                                              
      TRUE=FALF(0.D0)                                                   
      ERR=TRUE-ANS                                                      
      IF(     PARAM) WRITE(IWRITE,1) ALF,TRUE,ERR,ERREST,JCALL,KWARN    
      IF(.NOT.PARAM) WRITE(IWRITE,2) EPS,TRUE,ERR,ERREST,JCALL,KWARN    
      RETURN                                                            
    1 FORMAT(F17.4,F12.4,1P2D12.2,2I8)                                  
    2 FORMAT(5X,1PD12.2,0PF12.4,1P2D12.2,2I8)                           
      END                                                               
      DOUBLE PRECISION FUNCTION FALF(X)                                 
      DOUBLE PRECISION X,ALPHA,PI,ALF,B,V,ERFC,Y                        
      DOUBLE PRECISION DEXP, DCOS, DATAN, DSIN, DSQRT                   
      COMMON/FALFCM/ALF,V,B,N,JCALL,PARAM                               
      LOGICAL PARAM                                                     
C                                                                       
      DATA PI/3.141592653589793238D0/                                   
C                                                                       
C      7-DECIMAL ERFC, HART NUMBER 5662                                 
C                                                                       
      ERFC(Y)=DEXP(-Y**2)*(3.5322166+Y*(2.1539977+Y*0.57404837))/       
     *   (3.5322162+Y*(6.1397195+Y*(3.9690912+Y)))                      
C                                                                       
      M=IABS(N)                                                         
      IF(N.LE.0) GO TO (100,101,102,103,104,105,106),M                  
      JCALL=JCALL+1                                                     
      GO TO (1,1,2,3,4,5,6),M                                           
C                                                                       
    1 FALF=V/(1.D0+(X*V)**2)                                            
      RETURN                                                            
    2 FALF=0.                                                           
      IF(X.GT.0.D0) FALF=X**ALF                                         
      RETURN                                                            
    3 FALF=1.D0+DCOS(ALF*X*PI)                                          
      RETURN                                                            
    4 FALF=V*DEXP(-V*X)                                                 
      RETURN                                                            
    5 FALF=V**2*X*DEXP(-V*X)                                            
      RETURN                                                            
    6 FALF=B*DEXP(-(B*(X-ALF))**2)                                      
      RETURN                                                            
C                                                                       
  100 FALF=2.D0*DATAN(V)                                                
      RETURN                                                            
  101 FALF=DATAN(V)                                                     
      RETURN                                                            
  102 FALF=R1MACH(2)                                                    
      IF (ALF .GT. (-1.D0)) FALF=1.D0/(1.D0+ALF)                        
      RETURN                                                            
  103 IF (ALF .NE. 0.D0) FALF=1.D0+DSIN(ALF*PI)/(ALF*PI)                
      IF (ALF .EQ. 0.D0) FALF=2.D0                                      
      RETURN                                                            
  104 FALF=1.D0-DEXP(-V)                                                
      RETURN                                                            
  105 FALF=1.D0-(1.D0+V)*DEXP(-V)                                       
      RETURN                                                            
  106 FALF=DSQRT(PI)*(1.D0-0.5*(ERFC(B*ALF)+ERFC(B*(1.D0-ALF))))        
      RETURN                                                            
      END                                                               
      SUBROUTINE TEST3                                                  
C                                                                       
C        THIRD PACKAGE TO TEST DQUAD                                    
C                                                                       
C TEST DQUAD FOR EFFECTS OF ROUNDOFF IN FUNCTION RATHER THAN MACHINE.   
C                                                                       
      EXTERNAL FRAND                                                    
      DOUBLE PRECISION FRAND,ANS,ERREST,RAND,TRUE,ERR                   
      COMMON/NOISE/RAND,JCALL,N,NR                                      
C                                                                       
      IWRITE = I1MACH(2)                                                
      DO 200 N=1,4                                                      
         RAND=0.D0                                                      
         JCALL = 0                                                      
         CALL DQUAD(FRAND,0.D0,1.D0,1.D-10,TRUE,ERREST)                 
         CALL ERROFF                                                    
         KWARN=0                                                        
         IF (ERREST .GT. 1.D-10) KWARN=1                                
         WRITE(IWRITE,5) N,TRUE,ERREST,KWARN,JCALL                      
    5    FORMAT(1H0,I4,1PD15.8,1PD12.3,2I10)                            
         DO 150 NR=1,2                                                  
            WRITE(IWRITE,4) N,NR                                        
    4       FORMAT(2I5)                                                 
            DO 100 JR=1,10                                              
               JEXP=2-JR                                                
               RAND=10.D0**JEXP                                         
               WRITE(IWRITE,1) JEXP                                     
               WRITE(IWRITE,3)                                          
    1          FORMAT(18H0NOISE LEVEL 10**(,I3,1H))                     
    3          FORMAT(47H    "TRUE" ERR     EST ERR      KWARN     JCALL
     *)                                                                 
               DO 50 NN=1,5                                             
                  JCALL=0                                               
                  CALL DQUAD(FRAND,0.D0,1.D0,1.D-6,ANS,ERREST)          
                  CALL ERROFF                                           
                  KWARN=0                                               
                  IF (ERREST .GT. 1.D-6) KWARN=1                        
                  ERR=TRUE-ANS                                          
                  WRITE(IWRITE,2) ERR,ERREST,KWARN,JCALL                
    2             FORMAT(1X,1P2D13.3,2I10)                              
   50          CONTINUE                                                 
  100       CONTINUE                                                    
  150    CONTINUE                                                       
  200 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION FRAND(X)                                
      DOUBLE PRECISION X,RAND,PI,RAN                                    
      DOUBLE PRECISION DEXP, DSQRT, DCOS                                
      COMMON/NOISE/RAND,JCALL,N,NR                                      
      DATA PI/3.141592653589793238D0/                                   
C                                                                       
      JCALL=JCALL+1                                                     
C                                                                       
      RAN=RAND*(2.*UNI(0)-1.)                                           
         GO TO (1,2,3,4),N                                              
    1    FRAND=8.*DEXP(-8.*X)                                           
         GO TO 10                                                       
    2    FRAND=DSQRT(X)                                                 
         GO TO 10                                                       
    3    FRAND=1.+DCOS(1.95D0*PI*X)                                     
         GO TO 10                                                       
    4    FRAND=1.+DCOS(17.95D0*PI*X)                                    
         GO TO 10                                                       
   10 IF(NR.EQ.1) FRAND=FRAND+RAN                                       
      IF(NR.EQ.2) FRAND=FRAND*(1.D0+RAN)                                
      RETURN                                                            
      END                                                               
C$TEST QGSG                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAMS FOR GAUSS QUADRATURE                       
C                                                                       
C***********************************************************************
      COMMON/CSTAK/D(500)                                               
      DOUBLE PRECISION D                                                
C                                                                       
      CALL ISTKIN(500, 4)                                               
C                                                                       
      CALL TG1                                                          
      CALL TGEX                                                         
      CALL TGEX2                                                        
      CALL TGEXA                                                        
      CALL TGLOG                                                        
      CALL TGXA                                                         
      CALL TGXAB                                                        
C                                                                       
      STOP                                                              
      END                                                               
      SUBROUTINE TGEX2                                                  
      REAL X(5),W(5),FEX2,SUM,TRUE,PI,ERR                               
C                                                                       
      CALL  GQEX2(5,X,W)                                                
      IOUT=I1MACH(2)                                                    
      WRITE(IOUT,1)                                                     
      DO 10 J=1,5                                                       
   10    WRITE(IOUT,2) J, X(J),W(J)                                     
      SUM=0.E0                                                          
      DO 20 J=1,5                                                       
   20    SUM=SUM+W(J)*FEX2(X(J))                                        
      PI=2.E0*ATAN2(1.E0,0.E0)                                          
      TRUE=SQRT(PI)*EXP(-0.25E0)                                        
      ERR=TRUE-SUM                                                      
      WRITE(IOUT,3) TRUE,SUM,ERR                                        
      RETURN                                                            
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
      SUBROUTINE TGEXA                                                  
      REAL X(5),W(5),FEXA,SUM,TRUE,PI,ERR                               
C                                                                       
      CALL  GQEXA(5,-0.5E0,X,W)                                         
      IOUT=I1MACH(2)                                                    
      WRITE(IOUT,1)                                                     
      DO 10 J=1,5                                                       
   10    WRITE(IOUT,2) J, X(J),W(J)                                     
      SUM=0.E0                                                          
      DO 20 J=1,5                                                       
   20    SUM=SUM+W(J)*FEXA(X(J))                                        
      PI=2.E0*ATAN2(1.E0,0.E0)                                          
      TRUE=0.5E0*SQRT(PI)*(1.E0-1.E0/SQRT(3.E0))                        
      ERR=TRUE-SUM                                                      
      WRITE(IOUT,3) TRUE,SUM,ERR                                        
      RETURN                                                            
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
      SUBROUTINE TGLOG                                                  
      REAL X(5),W(5),FLOG,SUM,TRUE,PI2,ERR                              
C                                                                       
      CALL  GQLOG(5,X,W)                                                
      IOUT=I1MACH(2)                                                    
      WRITE(IOUT,1)                                                     
      DO 10 J=1,5                                                       
   10    WRITE(IOUT,2) J, X(J),W(J)                                     
      SUM=0.E0                                                          
      DO 20 J=1,5                                                       
   20    SUM=SUM+W(J)*FLOG(X(J))                                        
      PI2=ATAN2(1.E0,0.E0)                                              
      TRUE=-(PI2**2/3.E0)                                               
      ERR=TRUE-SUM                                                      
      WRITE(IOUT,3) TRUE,SUM,ERR                                        
      RETURN                                                            
    1 FORMAT(///15H TEST OF  GQLOG//30H0ABSCISSAS AND WEIGHTS FOR N=5)  
    2 FORMAT(I4,0P2E16.7)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/                       
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)                             
      END                                                               
      REAL FUNCTION FLOG(X)                                             
      REAL X                                                            
      FLOG=-1.E0/(1.E0+X)                                               
      RETURN                                                            
      END                                                               
      SUBROUTINE TGXA                                                   
      REAL X(5),W(5),FXA,SUM,TRUE,B(1),PI2,ERR                          
C                                                                       
      CALL  GQXA(5,-0.5E0,X,W)                                          
      IOUT=I1MACH(2)                                                    
      WRITE(IOUT,1)                                                     
      DO 10 J=1,5                                                       
   10    WRITE(IOUT,2) J, X(J),W(J)                                     
      SUM=0.E0                                                          
      DO 20 J=1,5                                                       
   20    SUM=SUM+W(J)*FXA(X(J))                                         
      CALL BESRJ(1.E0,1,B)                                              
      PI2=ATAN2(1.E0,0.E0)                                              
      TRUE=PI2*B(1)                                                     
      ERR=TRUE-SUM                                                      
      WRITE(IOUT,3) TRUE,SUM,ERR                                        
      RETURN                                                            
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
      SUBROUTINE TGXAB                                                  
      REAL X(5),W(5),FXAB,SUM,TRUE,PI,ERR                               
C                                                                       
      CALL  GQXAB(5,-0.5E0,0.5E0,X,W)                                   
      IOUT=I1MACH(2)                                                    
      WRITE(IOUT,1)                                                     
      DO 10 J=1,5                                                       
   10    WRITE(IOUT,2) J, X(J),W(J)                                     
      SUM=0.E0                                                          
      DO 20 J=1,5                                                       
   20    SUM=SUM+W(J)*FXAB(X(J))                                        
      PI=2.E0*ATAN2(1.E0,0.E0)                                          
      TRUE=PI*(1.E0-1.E0/SQRT(3.E0))                                    
      ERR=TRUE-SUM                                                      
      WRITE(IOUT,3) TRUE,SUM,ERR                                        
      RETURN                                                            
    1 FORMAT(///15H TEST OF  GQXAB//30H0ABSCISSAS AND WEIGHTS FOR N=5)  
    2 FORMAT(I4,0P2E16.7)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/                       
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)                             
      END                                                               
      REAL FUNCTION FXAB(X)                                             
      REAL X                                                            
      FXAB=1.E0/(2.E0+X)                                                
      RETURN                                                            
      END                                                               
      REAL FUNCTION  GAMMA(X)                                           
      REAL X                                                            
      REAL Y, SQRT, ATAN2                                               
C J. L. BLUE, 16 DEC 77                                                 
C DUMMY VERSION - ONLY GOOD FOR INTEGERS OR HALF-INTEGERS               
      Y = X                                                             
       GAMMA = 1.E0                                                     
   1  IF (Y .LE. 1.E0) GOTO  2                                          
         Y = Y-1.E0                                                     
          GAMMA = Y* GAMMA                                              
         GOTO  1                                                        
   2  IF (Y .NE. 1.E0) GOTO 3                                           
         GOTO  6                                                        
   3     IF (Y .NE. 0.5E0) GOTO 4                                       
             GAMMA =  GAMMA*SQRT(2.E0*ATAN2(1.E0, 0.E0))                
            GOTO  5                                                     
C/6S                                                                    
C  4        CALL SETERR(16H GAMMA - NOT YET, 16, 1, 2)                  
C/7S                                                                    
   4        CALL SETERR(' GAMMA - NOT YET', 16, 1, 2)                   
C/                                                                      
   5  CONTINUE                                                          
   6  RETURN                                                            
      END                                                               
      SUBROUTINE TGEX                                                   
      REAL X(5),W(5),FEX,SUM,TRUE,PI,ERR                                
C                                                                       
      CALL  GQEX(5,X,W)                                                 
      IOUT=I1MACH(2)                                                    
      WRITE(IOUT,1)                                                     
      DO 10 J=1,5                                                       
   10    WRITE(IOUT,2) J, X(J),W(J)                                     
      SUM=0.E0                                                          
      DO 20 J=1,5                                                       
   20    SUM=SUM+W(J)*FEX(X(J))                                         
      PI=2.E0*ATAN2(1.E0,0.E0)                                          
      TRUE=PI**2/6.E0                                                   
      ERR=TRUE-SUM                                                      
      WRITE(IOUT,3) TRUE,SUM,ERR                                        
      RETURN                                                            
    1 FORMAT(///14H TEST OF  GQEX//30H0ABSCISSAS AND WEIGHTS FOR N=5)   
    2 FORMAT(I4,0P2E16.7)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/                       
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)                             
      END                                                               
      REAL FUNCTION FEX(X)                                              
      REAL X                                                            
      FEX=X/(1.E0-EXP(-X))                                              
      RETURN                                                            
      END                                                               
      SUBROUTINE TG1                                                    
      REAL X(5),W(5),F1,SUM,TRUE,ERR                                    
C                                                                       
      CALL  GQ1(5,X,W)                                                  
      IOUT=I1MACH(2)                                                    
      WRITE(IOUT,1)                                                     
      DO 10 J=1,5                                                       
   10    WRITE(IOUT,2) J, X(J),W(J)                                     
      SUM=0.E0                                                          
      DO 20 J=1,5                                                       
   20    SUM=SUM+W(J)*F1(X(J))                                          
      TRUE=ALOG(3.E0)                                                   
      ERR=TRUE-SUM                                                      
      WRITE(IOUT,3) TRUE,SUM,ERR                                        
      RETURN                                                            
    1 FORMAT(///13H TEST OF  GQ1//30H0ABSCISSAS AND WEIGHTS FOR N=5)    
    2 FORMAT(I4,0P2E16.7)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PE16.7/                       
     X   6H CALC=,1PE16.7/6H ERR =,1PE11.2)                             
      END                                                               
      REAL FUNCTION F1(X)                                               
      REAL X                                                            
      F1=1.E0/(2.E0+X)                                                  
      RETURN                                                            
      END                                                               
C OUTPUT FROM TEST....                                                  
C                                                                       
C TEST OF  GQ1                                                          
C ABSCISSAS AND WEIGHTS FOR N=5                                         
C   1  -0.9061799E 00   0.2369269E 00                                   
C   2  -0.5384693E 00   0.4786287E 00                                   
C   3   0.              0.5688889E 00                                   
C   4   0.5384693E 00   0.4786287E 00                                   
C   5   0.9061799E 00   0.2369269E 00                                   
C SAMPLE PROBLEM                                                        
C TRUE=   1.0986123E 00                                                 
C CALC=   1.0986093E 00                                                 
C ERR =   2.99E-06                                                      
C TEST OF  GQEX                                                         
C ABSCISSAS AND WEIGHTS FOR N=5                                         
C   1   0.2635603E 00   0.5217556E 00                                   
C   2   0.1413403E 01   0.3986668E 00                                   
C   3   0.3596426E 01   0.7594245E-01                                   
C   4   0.7085810E 01   0.3611759E-02                                   
C   5   0.1264080E 02   0.2336997E-04                                   
C SAMPLE PROBLEM                                                        
C TRUE=   1.6449341E 00                                                 
C CALC=   1.6449244E 00                                                 
C ERR =   9.68E-06                                                      
C TEST OF  GQEX2                                                        
C ABSCISSAS AND WEIGHTS FOR N=5                                         
C   1  -0.2020183E 01   0.1995325E-01                                   
C   2  -0.9585725E 00   0.3936193E 00                                   
C   3   0.              0.9453087E 00                                   
C   4   0.9585725E 00   0.3936193E 00                                   
C   5   0.2020183E 01   0.1995325E-01                                   
C SAMPLE PROBLEM                                                        
C TRUE=   1.3803884E 00                                                 
C CALC=   1.3803901E 00                                                 
C ERR =  -1.65E-06                                                      
C TEST OF  GQEXA                                                        
C ABSCISSAS AND WEIGHTS FOR N=5                                         
C   1   0.1175813E 00   0.1221725E 01                                   
C   2   0.1074562E 01   0.4802772E 00                                   
C   3   0.3085937E 01   0.6774879E-01                                   
C   4   0.6414729E 01   0.2687291E-02                                   
C   5   0.1180719E 02   0.1528086E-04                                   
C SAMPLE PROBLEM                                                        
C TRUE=   3.7456357E-01                                                 
C CALC=   3.7530771E-01                                                 
C ERR =  -7.44E-04                                                      
C TEST OF  GQLOG                                                        
C ABSCISSAS AND WEIGHTS FOR N=5                                         
C   1   0.2913447E-01   0.2978935E 00                                   
C   2   0.1739772E 00   0.3497762E 00                                   
C   3   0.4117025E 00   0.2344883E 00                                   
C   4   0.6773142E 00   0.9893046E-01                                   
C   5   0.8947714E 00   0.1891155E-01                                   
C SAMPLE PROBLEM                                                        
C TRUE=  -8.2246703E-01                                                 
C CALC=  -8.2246699E-01                                                 
C ERR =  -4.05E-08                                                      
C TEST OF  GQXA                                                         
C ABSCISSAS AND WEIGHTS FOR N=5                                         
C   1   0.2216357E-01   0.5910485E 00                                   
C   2   0.1878316E 00   0.5385334E 00                                   
C   3   0.4615974E 00   0.4381727E 00                                   
C   4   0.7483346E 00   0.2989027E 00                                   
C   5   0.9484939E 00   0.1333427E 00                                   
C SAMPLE PROBLEM                                                        
C TRUE=   1.2019697E 00                                                 
C CALC=   1.2019697E 00                                                 
C ERR =  -9.66E-09                                                      
C TEST OF  GQXAB                                                        
C ABSCISSAS AND WEIGHTS FOR N=5                                         
C   1  -0.8412535E 00   0.9067575E-01                                   
C   2  -0.4154150E 00   0.3339142E 00                                   
C   3   0.1423148E 00   0.6524887E 00                                   
C   4   0.6548607E 00   0.9452542E 00                                   
C   5   0.9594930E 00   0.1119260E 01                                   
C SAMPLE PROBLEM                                                        
C TRUE=   1.3277933E 00                                                 
C CALC=   1.3277914E 00                                                 
C ERR =   1.88E-06                                                      
C$TEST QGGD                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAMS FOR GAUSS QUADRATURE (DOUBLE-PRECISION)    
C                                                                       
C***********************************************************************
      COMMON/CSTAK/D(500)                                               
      DOUBLE PRECISION D                                                
C                                                                       
      CALL ISTKIN(500,4)                                                
C                                                                       
      CALL TDG1                                                         
      CALL TDGEX                                                        
      CALL TDGEX2                                                       
      CALL TDGEXA                                                       
      CALL TDGLOG                                                       
      CALL TDGXA                                                        
      CALL TDGXAB                                                       
C                                                                       
      STOP                                                              
      END                                                               
      SUBROUTINE TDGEX2                                                 
      DOUBLE PRECISION X(5),W(5),FEX2,SUM,TRUE,PI,ERR                   
      DOUBLE PRECISION DATAN2, DSQRT, DEXP                              
C                                                                       
      CALL DGQEX2(5,X,W)                                                
      IOUT=I1MACH(2)                                                    
      WRITE(IOUT,1)                                                     
      DO 10 J=1,5                                                       
   10    WRITE(IOUT,2) J, X(J),W(J)                                     
      SUM=0.D0                                                          
      DO 20 J=1,5                                                       
   20    SUM=SUM+W(J)*FEX2(X(J))                                        
      PI=2.D0*DATAN2(1.D0,0.D0)                                         
      TRUE=DSQRT(PI)*DEXP(-0.25D0)                                      
      ERR=TRUE-SUM                                                      
      WRITE(IOUT,3) TRUE,SUM,ERR                                        
      RETURN                                                            
    1 FORMAT(///15H TEST OF DGQEX2//30H0ABSCISSAS AND WEIGHTS FOR N=5)  
    2 FORMAT(I4,0P2D18.9)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PD18.9/                       
     X   6H CALC=,1PD18.9/6H ERR =,1PD11.2)                             
      END                                                               
      DOUBLE PRECISION FUNCTION FEX2(X)                                 
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION DCOS                                             
      FEX2=DCOS(X)                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE TDGEXA                                                 
      DOUBLE PRECISION X(5),W(5),FEXA,SUM,TRUE,PI,ERR                   
      DOUBLE PRECISION DATAN2, DSQRT                                    
C                                                                       
      CALL DGQEXA(5,-0.5D0,X,W)                                         
      IOUT=I1MACH(2)                                                    
      WRITE(IOUT,1)                                                     
      DO 10 J=1,5                                                       
   10    WRITE(IOUT,2) J, X(J),W(J)                                     
      SUM=0.D0                                                          
      DO 20 J=1,5                                                       
   20    SUM=SUM+W(J)*FEXA(X(J))                                        
      PI=2.D0*DATAN2(1.D0,0.D0)                                         
      TRUE=0.5D0*DSQRT(PI)*(1.D0-1.D0/DSQRT(3.D0))                      
      ERR=TRUE-SUM                                                      
      WRITE(IOUT,3) TRUE,SUM,ERR                                        
      RETURN                                                            
    1 FORMAT(///15H TEST OF DGQEXA//30H0ABSCISSAS AND WEIGHTS FOR N=5)  
    2 FORMAT(I4,0P2D18.9)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PD18.9/                       
     X   6H CALC=,1PD18.9/6H ERR =,1PD11.2)                             
      END                                                               
      DOUBLE PRECISION FUNCTION FEXA(X)                                 
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION DEXP                                             
      FEXA=0.5D0*(1.D0-DEXP(-2.D0*X))                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE TDGLOG                                                 
      DOUBLE PRECISION X(5),W(5),FLOG,SUM,TRUE,PI2,ERR                  
      DOUBLE PRECISION DATAN2                                           
C                                                                       
      CALL DGQLOG(5,X,W)                                                
      IOUT=I1MACH(2)                                                    
      WRITE(IOUT,1)                                                     
      DO 10 J=1,5                                                       
   10    WRITE(IOUT,2) J, X(J),W(J)                                     
      SUM=0.D0                                                          
      DO 20 J=1,5                                                       
   20    SUM=SUM+W(J)*FLOG(X(J))                                        
      PI2=DATAN2(1.D0,0.D0)                                             
      TRUE=-(PI2**2/3.D0)                                               
      ERR=TRUE-SUM                                                      
      WRITE(IOUT,3) TRUE,SUM,ERR                                        
      RETURN                                                            
    1 FORMAT(///15H TEST OF DGQLOG//30H0ABSCISSAS AND WEIGHTS FOR N=5)  
    2 FORMAT(I4,0P2D18.9)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PD18.9/                       
     X   6H CALC=,1PD18.9/6H ERR =,1PD11.2)                             
      END                                                               
      DOUBLE PRECISION FUNCTION FLOG(X)                                 
      DOUBLE PRECISION X                                                
      FLOG=-1.D0/(1.D0+X)                                               
      RETURN                                                            
      END                                                               
      SUBROUTINE TDGXA                                                  
      DOUBLE PRECISION X(5),W(5),FXA,SUM,TRUE,B(1),PI2,ERR              
      DOUBLE PRECISION DATAN2                                           
C                                                                       
      CALL DGQXA(5,-0.5D0,X,W)                                          
      IOUT=I1MACH(2)                                                    
      WRITE(IOUT,1)                                                     
      DO 10 J=1,5                                                       
   10    WRITE(IOUT,2) J, X(J),W(J)                                     
      SUM=0.D0                                                          
      DO 20 J=1,5                                                       
   20    SUM=SUM+W(J)*FXA(X(J))                                         
      CALL DBESRJ(1.D0,1,B)                                             
      PI2=DATAN2(1.D0,0.D0)                                             
      TRUE=PI2*B(1)                                                     
      ERR=TRUE-SUM                                                      
      WRITE(IOUT,3) TRUE,SUM,ERR                                        
      RETURN                                                            
    1 FORMAT(///14H TEST OF DGQXA//30H0ABSCISSAS AND WEIGHTS FOR N=5)   
    2 FORMAT(I4,0P2D18.9)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PD18.9/                       
     X   6H CALC=,1PD18.9/6H ERR =,1PD11.2)                             
      END                                                               
      DOUBLE PRECISION FUNCTION FXA(X)                                  
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION DCOS, DSQRT                                      
      FXA=DCOS(1.D0-X)/DSQRT(2.D0-X)                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE TDGXAB                                                 
      DOUBLE PRECISION X(5),W(5),FXAB,SUM,TRUE,PI,ERR                   
      DOUBLE PRECISION DATAN2, DSQRT                                    
C                                                                       
      CALL DGQXAB(5,-0.5D0,0.5D0,X,W)                                   
      IOUT=I1MACH(2)                                                    
      WRITE(IOUT,1)                                                     
      DO 10 J=1,5                                                       
   10    WRITE(IOUT,2) J, X(J),W(J)                                     
      SUM=0.D0                                                          
      DO 20 J=1,5                                                       
   20    SUM=SUM+W(J)*FXAB(X(J))                                        
      PI=2.D0*DATAN2(1.D0,0.D0)                                         
      TRUE=PI*(1.D0-1.D0/DSQRT(3.D0))                                   
      ERR=TRUE-SUM                                                      
      WRITE(IOUT,3) TRUE,SUM,ERR                                        
      RETURN                                                            
    1 FORMAT(///15H TEST OF DGQXAB//30H0ABSCISSAS AND WEIGHTS FOR N=5)  
    2 FORMAT(I4,0P2D18.9)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PD18.9/                       
     X   6H CALC=,1PD18.9/6H ERR =,1PD11.2)                             
      END                                                               
      DOUBLE PRECISION FUNCTION FXAB(X)                                 
      DOUBLE PRECISION X                                                
      FXAB=1.D0/(2.D0+X)                                                
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DGAMMA(X)                               
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION Y, DSQRT, DATAN2                                 
C J. L. BLUE, 16 DEC 77                                                 
C DUMMY VERSION - ONLY GOOD FOR INTEGERS OR HALF-INTEGERS               
      Y = X                                                             
      DGAMMA = 1.D0                                                     
   1  IF (Y .LE. 1.D0) GOTO  2                                          
         Y = Y-1.D0                                                     
         DGAMMA = Y*DGAMMA                                              
         GOTO  1                                                        
   2  IF (Y .NE. 1.D0) GOTO 3                                           
         GOTO  6                                                        
   3     IF (Y .NE. 0.5D0) GOTO 4                                       
            DGAMMA = DGAMMA*DSQRT(2.D0*DATAN2(1.D0, 0.D0))              
            GOTO  5                                                     
C/6S                                                                    
C  4        CALL SETERR(16HDGAMMA - NOT YET, 16, 1, 2)                  
C/7S                                                                    
   4        CALL SETERR('DGAMMA - NOT YET', 16, 1, 2)                   
C/                                                                      
   5  CONTINUE                                                          
   6  RETURN                                                            
      END                                                               
      SUBROUTINE TDGEX                                                  
      DOUBLE PRECISION X(5),W(5),FEX,SUM,TRUE,PI,ERR                    
      DOUBLE PRECISION DATAN2                                           
C                                                                       
      CALL DGQEX(5,X,W)                                                 
      IOUT=I1MACH(2)                                                    
      WRITE(IOUT,1)                                                     
      DO 10 J=1,5                                                       
   10    WRITE(IOUT,2) J, X(J),W(J)                                     
      SUM=0.D0                                                          
      DO 20 J=1,5                                                       
   20    SUM=SUM+W(J)*FEX(X(J))                                         
      PI=2.D0*DATAN2(1.D0,0.D0)                                         
      TRUE=PI**2/6.D0                                                   
      ERR=TRUE-SUM                                                      
      WRITE(IOUT,3) TRUE,SUM,ERR                                        
      RETURN                                                            
    1 FORMAT(///14H TEST OF DGQEX//30H0ABSCISSAS AND WEIGHTS FOR N=5)   
    2 FORMAT(I4,0P2D18.9)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PD18.9/                       
     X   6H CALC=,1PD18.9/6H ERR =,1PD11.2)                             
      END                                                               
      DOUBLE PRECISION FUNCTION FEX(X)                                  
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION DEXP                                             
      FEX=X/(1.D0-DEXP(-X))                                             
      RETURN                                                            
      END                                                               
      SUBROUTINE TDG1                                                   
      DOUBLE PRECISION X(5),W(5),F1,SUM,TRUE,ERR                        
      DOUBLE PRECISION DLOG                                             
C                                                                       
      CALL DGQ1(5,X,W)                                                  
      IOUT=I1MACH(2)                                                    
      WRITE(IOUT,1)                                                     
      DO 10 J=1,5                                                       
   10    WRITE(IOUT,2) J, X(J),W(J)                                     
      SUM=0.D0                                                          
      DO 20 J=1,5                                                       
   20    SUM=SUM+W(J)*F1(X(J))                                          
      TRUE=DLOG(3.D0)                                                   
      ERR=TRUE-SUM                                                      
      WRITE(IOUT,3) TRUE,SUM,ERR                                        
      RETURN                                                            
    1 FORMAT(///13H TEST OF DGQ1//30H0ABSCISSAS AND WEIGHTS FOR N=5)    
    2 FORMAT(I4,0P2D18.9)                                               
    3 FORMAT(15H0SAMPLE PROBLEM/6H TRUE=,1PD18.9/                       
     X   6H CALC=,1PD18.9/6H ERR =,1PD11.2)                             
      END                                                               
      DOUBLE PRECISION FUNCTION F1(X)                                   
      DOUBLE PRECISION X                                                
      F1=1.D0/(2.D0+X)                                                  
      RETURN                                                            
       END                                                              
C OUTPUT FROM TEST....                                                  
C                                                                       
C TEST OF DGQ1                                                          
C ABSCISSAS AND WEIGHTS FOR N=5                                         
C   1  -0.906179852D 00   0.236926886D 00                               
C   2  -0.538469312D 00   0.478628672D 00                               
C   3   0.                0.568888889D 00                               
C   4   0.538469312D 00   0.478628672D 00                               
C   5   0.906179852D 00   0.236926886D 00                               
C SAMPLE PROBLEM                                                        
C TRUE=   1.098612288D 00                                               
C CALC=   1.098609242D 00                                               
C ERR =   3.05D-06                                                      
C TEST OF DGQEX                                                         
C ABSCISSAS AND WEIGHTS FOR N=5                                         
C   1   0.263560320D 00   0.521755609D 00                               
C   2   0.141340307D 01   0.398666812D 00                               
C   3   0.359642579D 01   0.759424499D-01                               
C   4   0.708580998D 01   0.361175868D-02                               
C   5   0.126408009D 02   0.233699724D-04                               
C SAMPLE PROBLEM                                                        
C TRUE=   1.644934067D 00                                               
C CALC=   1.644924428D 00                                               
C ERR =   9.64D-06                                                      
C TEST OF DGQEX2                                                        
C ABSCISSAS AND WEIGHTS FOR N=5                                         
C   1  -0.202018288D 01   0.199532420D-01                               
C   2  -0.958572467D 00   0.393619324D 00                               
C   3   0.                0.945308723D 00                               
C   4   0.958572467D 00   0.393619324D 00                               
C   5   0.202018288D 01   0.199532420D-01                               
C SAMPLE PROBLEM                                                        
C TRUE=   1.380388448D 00                                               
C CALC=   1.380390076D 00                                               
C ERR =  -1.63D-06                                                      
C TEST OF DGQEXA                                                        
C ABSCISSAS AND WEIGHTS FOR N=5                                         
C   1   0.117581320D 00   0.122172528D 01                               
C   2   0.107456201D 01   0.480277222D 00                               
C   3   0.308593744D 01   0.677487891D-01                               
C   4   0.641472972D 01   0.268729148D-02                               
C   5   0.118071896D 02   0.152808657D-04                               
C SAMPLE PROBLEM                                                        
C TRUE=   3.745635718D-01                                               
C CALC=   3.753077036D-01                                               
C ERR =  -7.44D-04                                                      
C TEST OF DGQLOG                                                        
C ABSCISSAS AND WEIGHTS FOR N=5                                         
C   1   0.291344723D-01   0.297893472D 00                               
C   2   0.173977214D 00   0.349776227D 00                               
C   3   0.411702521D 00   0.234488291D 00                               
C   4   0.677314176D 00   0.989304601D-01                               
C   5   0.894771366D 00   0.189115521D-01                               
C SAMPLE PROBLEM                                                        
C TRUE=  -8.224670336D-01                                               
C CALC=  -8.224670176D-01                                               
C ERR =  -1.59D-08                                                      
C TEST OF DGQXA                                                         
C ABSCISSAS AND WEIGHTS FOR N=5                                         
C   1   0.221635689D-01   0.591048448D 00                               
C   2   0.187831568D 00   0.538533440D 00                               
C   3   0.461597363D 00   0.438172723D 00                               
C   4   0.748334630D 00   0.298902697D 00                               
C   5   0.948493926D 00   0.133342689D 00                               
C SAMPLE PROBLEM                                                        
C TRUE=   1.201969715D 00                                               
C CALC=   1.201969712D 00                                               
C ERR =   3.93D-09                                                      
C TEST OF DGQXAB                                                        
C ABSCISSAS AND WEIGHTS FOR N=5                                         
C   1  -0.841253536D 00   0.906757696D-01                               
C   2  -0.415415014D 00   0.333914163D 00                               
C   3   0.142314838D 00   0.652488710D 00                               
C   4   0.654860736D 00   0.945254246D 00                               
C   5   0.959492979D 00   0.111925977D 01                               
C SAMPLE PROBLEM                                                        
C TRUE=   1.327793289D 00                                               
C CALC=   1.327791436D 00                                               
C ERR =   1.85D-06                                                      
C$TEST MFTE                                                             
C***********************************************************************
C                                                                       
C  TEST OF PORT PROGRAMS FOR MULTIPLE FFT -- MFTCC, MFTRC, MFTCR        
C                                                                       
C***********************************************************************
C                                                                       
      REAL DSTAK(16000)                                                 
      COMMON /CSTAK/DSTAK                                               
C                                                                       
      CALL ISTKIN(16000,3)                                              
      CALL TESTER(70)                                                   
      CALL TESTER(210)                                                  
      STOP                                                              
      END                                                               
      SUBROUTINE TESTER(N)                                              
      INTEGER N                                                         
C                                                                       
C   TEST COMPUTES FORWARD AND BACKWARD TRANSFORMS ON RANDOM             
C   INPUT DATA                                                          
C                                                                       
      EXTERNAL I1MACH, ISTKST, UNI                                      
      INTEGER  I1MACH, ISTKST                                           
      REAL UNI                                                          
      REAL T(1000),A(630),B(630)                                        
      REAL C(630),D(630),E(630),F(630)                                  
      REAL ERR,FM1,FN1,FN2,Q1,Q2,SGN                                    
      INTEGER IFX(25)                                                   
      INTEGER I,ICUR,IJA,IMAX,IOUT,IWRITE,J,K,M                         
      REAL DSTAK(16000)                                                 
C                                                                       
      COMMON /CSTAK/DSTAK                                               
C                                                                       
C TEST MFTCC, EACH RADIX, FOR RANDOM INPUT SEQUENCES                    
C                                                                       
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      WRITE(IWRITE,130)                                                 
C                                                                       
      M = 3                                                             
C                                                                       
      CALL MFTCI(N,IFX,T)                                               
      I = IFX(2) + 2                                                    
      WRITE(IWRITE,140)(IFX(K),K=1,I)                                   
C                                                                       
      DO 20 J = 1,M                                                     
         IJA = (J-1)*N+1                                                
         DO 10 I = 1,N                                                  
            Q1     = (UNI(0))                                           
            Q2     = (UNI(0))                                           
            A(IJA) = Q1                                                 
            E(IJA) = Q1                                                 
            B(IJA) = Q2                                                 
            F(IJA) = Q2                                                 
            IJA    = IJA+1                                              
 10      CONTINUE                                                       
 20   CONTINUE                                                          
C                                                                       
C COMPUTE SIGN = +1.0 TRANSFORM                                         
      SGN = +1.0E0                                                      
      CALL MFTCC(N,M,A(1),B(1),1,N,C(1),D(1),1,N,IFX,T,SGN)             
C NOW COMPUTE UN-NORMALIZED INVERSE                                     
      SGN = -1.0E0                                                      
      CALL MFTCC(N,M,C(1),D(1),1,N,A(1),B(1),1,N,IFX,T,SGN)             
C NOW COMPARE INPUT SEQUENCE TO RESULT OF FORWARD/BACKWARD X-FORM       
      FN1 = 1.E0/FLOAT(N)                                               
      FN2 = FN1*0.5E0                                                   
C E AND F ARE THE REAL/IMAGINARY PARTS OF RESIDUAL                      
      DO 40 J = 1,M                                                     
         IJA = (J-1)*N+1                                                
         DO 30 I = 1,N                                                  
            E(IJA) = E(IJA) - FN1*A(IJA)                                
            F(IJA) = F(IJA) - FN1*B(IJA)                                
            IJA    = IJA + 1                                            
 30      CONTINUE                                                       
 40   CONTINUE                                                          
      FM1 = 1.E0/FLOAT(M)                                               
      ERR = 0.E0                                                        
      DO 60 J = 1,M                                                     
         IJA = (J-1)*N+1                                                
         DO 50 I = 1,N                                                  
            ERR = ERR +  ABS(E(IJA)) +  ABS(F(IJA))                     
            IJA = IJA+1                                                 
 50      CONTINUE                                                       
 60   CONTINUE                                                          
      ERR = ERR*FN2*FM1                                                 
      WRITE(IWRITE,150)ERR                                              
C                                                                       
      IOUT = ISTKST(1)                                                  
      ICUR = ISTKST(2)                                                  
      IMAX = ISTKST(3)                                                  
      WRITE(IWRITE,160)IOUT,ICUR,IMAX                                   
C                                                                       
C NOW TEST THE REAL TO COMPLEX ROUTINES                                 
C                                                                       
      WRITE(IWRITE,170)                                                 
C                                                                       
      CALL MFTRI(N,IFX,T)                                               
      WRITE(IWRITE,140)(IFX(K),K=1,5)                                   
C                                                                       
      DO 80 J = 1,M                                                     
         IJA = (J-1)*N+1                                                
         DO 70 I = 1,N                                                  
            Q1     = (UNI(0))                                           
            A(IJA) = Q1                                                 
            E(IJA) = A(IJA)                                             
            IJA    = IJA + 1                                            
 70      CONTINUE                                                       
 80   CONTINUE                                                          
C                                                                       
C COMPUTE FORWARD (SIGN=1.) REAL TO COMPLEX TRANSFORM                   
C                                                                       
      SGN = 1.0E0                                                       
      CALL MFTRC(N,M,A,1,N,C(1),D(1),1,N,IFX,T,SGN)                     
C NOW COMPUTE INVERSE                                                   
      SGN = -1.0E0                                                      
      CALL MFTCR(N,M,C(1),D(1),1,N,A,1,N,IFX,T,SGN)                     
C                                                                       
C NOW FIND THE ERROR                                                    
C                                                                       
      FN1 = 1.E0/FLOAT(N)                                               
      FM1 = 1.E0/FLOAT(M)                                               
      DO 100 J = 1,M                                                    
         IJA = (J-1)*N+1                                                
         DO 90 I = 1,N                                                  
            E(IJA) = E(IJA) - FN1*A(IJA)                                
            IJA    = IJA +1                                             
 90      CONTINUE                                                       
 100  CONTINUE                                                          
C                                                                       
      ERR = 0.0E0                                                       
      DO 120 J = 1,M                                                    
         IJA = (J-1)*N+1                                                
         DO 110 I = 1,N                                                 
            ERR = ERR +  ABS(E(IJA))                                    
            IJA = IJA+1                                                 
 110     CONTINUE                                                       
 120  CONTINUE                                                          
C                                                                       
      ERR = ERR*FN1*FM1                                                 
      WRITE(IWRITE,150)ERR                                              
      IOUT = ISTKST(1)                                                  
      ICUR = ISTKST(2)                                                  
      IMAX = ISTKST(3)                                                  
      WRITE(IWRITE,160)IOUT,ICUR,IMAX                                   
C                                                                       
C OUTPUT FORMATS                                                        
C                                                                       
 130  FORMAT(/1X,46H ******* MFTCC RESULTS AND DIAGNOSTICS *******)     
 140  FORMAT(1X,7H IFX = /10(1X,I5))                                    
 150  FORMAT(1X,26H AVERAGE ABSOLUTE ERROR = ,E20.8)                    
 160  FORMAT(43H NUMBER OF OUTSTANDING STACK ALLOCATIONS = ,I5          
     1      /43H NUMBER OF CURRENT ACTIVE ALLOCATIONS    = ,I5          
     2      /43H MAXIMUM ACTIVE LENGTH ACHIEVED          = ,I5)         
 170  FORMAT(/1X,47H ****** MFTRC-MFTCR RESULTS, DIAGNOSTICS ******)    
      RETURN                                                            
      END                                                               
C$TEST MFED                                                             
C***********************************************************************
C                                                                       
C  TEST OF PORT PROGRAMS FOR MULTIPLE FFT (D.P.) - DMFTCC,DMFTRC,DMFTCR 
C                                                                       
C***********************************************************************
C                                                                       
      REAL DSTAK(16000)                                                 
      COMMON /CSTAK/DSTAK                                               
C                                                                       
      CALL ISTKIN(16000,3)                                              
      CALL TESTER(70)                                                   
      CALL TESTER(210)                                                  
      STOP                                                              
      END                                                               
      SUBROUTINE TESTER(N)                                              
      INTEGER N                                                         
C                                                                       
C   TEST COMPUTES FORWARD AND BACKWARD TRANSFORMS ON RANDOM             
C   INPUT DATA                                                          
C                                                                       
      EXTERNAL I1MACH, ISTKST, UNI                                      
      INTEGER  I1MACH, ISTKST                                           
      REAL UNI                                                          
      DOUBLE PRECISION T(1000),A(630),B(630)                            
      DOUBLE PRECISION C(630),D(630),E(630),F(630)                      
      DOUBLE PRECISION ERR,FM1,FN1,FN2,Q1,Q2,SGN                        
      INTEGER IFX(25)                                                   
      INTEGER I,ICUR,IJA,IMAX,IOUT,IWRITE,J,K,M                         
      REAL DSTAK(16000)                                                 
C                                                                       
      COMMON /CSTAK/DSTAK                                               
C                                                                       
C TEST MFTCC, EACH RADIX, FOR RANDOM INPUT SEQUENCES                    
C                                                                       
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      WRITE(IWRITE,130)                                                 
C                                                                       
      M = 3                                                             
C                                                                       
      CALL DMFTCI(N,IFX,T)                                              
      I = IFX(2) + 2                                                    
      WRITE(IWRITE,140)(IFX(K),K=1,I)                                   
C                                                                       
      DO 20 J = 1,M                                                     
         IJA = (J-1)*N+1                                                
         DO 10 I = 1,N                                                  
            Q1     = DBLE(UNI(0))                                       
            Q2     = DBLE(UNI(0))                                       
            A(IJA) = Q1                                                 
            E(IJA) = Q1                                                 
            B(IJA) = Q2                                                 
            F(IJA) = Q2                                                 
            IJA    = IJA+1                                              
 10      CONTINUE                                                       
 20   CONTINUE                                                          
C                                                                       
C COMPUTE SIGN = +1.0 TRANSFORM                                         
      SGN = +1.0D0                                                      
      CALL DMFTCC(N,M,A(1),B(1),1,N,C(1),D(1),1,N,IFX,T,SGN)            
C NOW COMPUTE UN-NORMALIZED INVERSE                                     
      SGN = -1.0D0                                                      
      CALL DMFTCC(N,M,C(1),D(1),1,N,A(1),B(1),1,N,IFX,T,SGN)            
C NOW COMPARE INPUT SEQUENCE TO RESULT OF FORWARD/BACKWARD X-FORM       
      FN1 = 1.D0/DBLE(FLOAT(N))                                         
      FN2 = FN1*0.5D0                                                   
C E AND F ARE THE REAL/IMAGINARY PARTS OF RESIDUAL                      
      DO 40 J = 1,M                                                     
         IJA = (J-1)*N+1                                                
         DO 30 I = 1,N                                                  
            E(IJA) = E(IJA) - FN1*A(IJA)                                
            F(IJA) = F(IJA) - FN1*B(IJA)                                
            IJA    = IJA + 1                                            
 30      CONTINUE                                                       
 40   CONTINUE                                                          
      FM1 = 1.D0/DBLE(FLOAT(M))                                         
      ERR = 0.D0                                                        
      DO 60 J = 1,M                                                     
         IJA = (J-1)*N+1                                                
         DO 50 I = 1,N                                                  
            ERR = ERR + DABS(E(IJA)) + DABS(F(IJA))                     
            IJA = IJA+1                                                 
 50      CONTINUE                                                       
 60   CONTINUE                                                          
      ERR = ERR*FN2*FM1                                                 
      WRITE(IWRITE,150)ERR                                              
C                                                                       
      IOUT = ISTKST(1)                                                  
      ICUR = ISTKST(2)                                                  
      IMAX = ISTKST(3)                                                  
      WRITE(IWRITE,160)IOUT,ICUR,IMAX                                   
C                                                                       
C NOW TEST THE REAL TO COMPLEX ROUTINES                                 
C                                                                       
      WRITE(IWRITE,170)                                                 
C                                                                       
      CALL DMFTRI(N,IFX,T)                                              
      WRITE(IWRITE,140)(IFX(K),K=1,5)                                   
C                                                                       
      DO 80 J = 1,M                                                     
         IJA = (J-1)*N+1                                                
         DO 70 I = 1,N                                                  
            Q1     = DBLE(UNI(0))                                       
            A(IJA) = Q1                                                 
            E(IJA) = A(IJA)                                             
            IJA    = IJA + 1                                            
 70      CONTINUE                                                       
 80   CONTINUE                                                          
C                                                                       
C COMPUTE FORWARD (SIGN=1.) REAL TO COMPLEX TRANSFORM                   
C                                                                       
      SGN = 1.0D0                                                       
      CALL DMFTRC(N,M,A,1,N,C(1),D(1),1,N,IFX,T,SGN)                    
C NOW COMPUTE INVERSE                                                   
      SGN = -1.0D0                                                      
      CALL DMFTCR(N,M,C(1),D(1),1,N,A,1,N,IFX,T,SGN)                    
C                                                                       
C NOW FIND THE ERROR                                                    
C                                                                       
      FN1 = 1.D0/DBLE(FLOAT(N))                                         
      FM1 = 1.D0/DBLE(FLOAT(M))                                         
      DO 100 J = 1,M                                                    
         IJA = (J-1)*N+1                                                
         DO 90 I = 1,N                                                  
            E(IJA) = E(IJA) - FN1*A(IJA)                                
            IJA    = IJA +1                                             
 90      CONTINUE                                                       
 100  CONTINUE                                                          
C                                                                       
      ERR = 0.0D0                                                       
      DO 120 J = 1,M                                                    
         IJA = (J-1)*N+1                                                
         DO 110 I = 1,N                                                 
            ERR = ERR + DABS(E(IJA))                                    
            IJA = IJA+1                                                 
 110     CONTINUE                                                       
 120  CONTINUE                                                          
C                                                                       
      ERR = ERR*FN1*FM1                                                 
      WRITE(IWRITE,150)ERR                                              
      IOUT = ISTKST(1)                                                  
      ICUR = ISTKST(2)                                                  
      IMAX = ISTKST(3)                                                  
      WRITE(IWRITE,160)IOUT,ICUR,IMAX                                   
C                                                                       
C OUTPUT FORMATS                                                        
C                                                                       
 130  FORMAT(/1X,47H ******* DMFTCC RESULTS AND DIAGNOSTICS *******)    
 140  FORMAT(1X,7H IFX = /10(1X,I5))                                    
 150  FORMAT(1X,26H AVERAGE ABSOLUTE ERROR = ,D20.8)                    
 160  FORMAT(43H NUMBER OF OUTSTANDING STACK ALLOCATIONS = ,I5          
     1      /43H NUMBER OF CURRENT ACTIVE ALLOCATIONS    = ,I5          
     2      /43H MAXIMUM ACTIVE LENGTH ACHIEVED          = ,I5)         
 170  FORMAT(/1X,49H ****** DMFTRC-DMFTCR RESULTS, DIAGNOSTICS ******)  
      RETURN                                                            
      END                                                               
C$TEST SDBA                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM DL2SF                                       
C                                                                       
C***********************************************************************
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /PASS/ KCOM, ITCOM, NTCOM, IACOM                           
      INTEGER KCOM, ITCOM, NTCOM, IACOM                                 
      INTEGER IA, NA(2), IS(1000), IT, NT, IX                           
      INTEGER IUMB, IY, NX, IW, ISTKGT, IFB                             
      INTEGER I, K, M, NDT, IPUMB, I1MACH                               
      REAL XB(3), WS(1000), RS(1000)                                    
      LOGICAL LS(1000)                                                  
      INTEGER TEMP, TEMP1, TEMP2                                        
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      CALL ISTKIN(1000, 3)                                              
C                                                                       
C SET OUTPUT UNIT                                                       
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      XB(1) = -1                                                        
      XB(2) = 0                                                         
      XB(3) = 2                                                         
      DO  9 K = 2, 4                                                    
         KCOM = K                                                       
         TEMP = 4*K+3                                                   
         DO  8 NDT = 2, TEMP                                            
            CALL ENTER(1)                                               
            NA(1) = (NDT+1)/2                                           
            NA(2) = NDT+1-NA(1)                                         
            IF (NDT .NE. 2) GOTO 1                                      
               IT = IUMB(XB(1), XB(3), NDT, K, NT)                      
               GOTO  2                                                  
   1           IT = IPUMB(XB, 3, NA, K, NT)                             
   2        DO  7 IFB = 1, 2                                            
               CALL ENTER(1)                                            
               IACOM = ISTKGT(NT-K, 3)                                  
               TEMP1 = NT-K                                             
               DO  3 I = 1, TEMP1                                       
                  TEMP2 = IACOM+I                                       
                  WS(TEMP2-1) = (-1)**(IFB+1)*I+(IFB-1)*(NT-K+1)        
   3              CONTINUE                                              
               IA = ISTKGT(NT-K, 3)                                     
               NA(1) = (NT-K+1)/2                                       
               NA(2) = NT-K+1-NA(1)                                     
               IF (NT-K .NE. 2) GOTO 4                                  
                  IX = IUMB(XB(1), XB(3), NT-K, K, NX)                  
                  GOTO  5                                               
   4              IX = IPUMB(XB, 3, NA, K, NX)                          
   5           IY = ISTKGT(2*NX, 3)                                     
               IW = ISTKGT(2*NX, 3)                                     
               TEMP1 = K-1                                              
               DO  6 M = 1, K, TEMP1                                    
                  ITCOM = IT+K-M                                        
                  NTCOM = NT-2*(K-M)                                    
                  CALL CHECK(K, WS(ITCOM), NTCOM, WS(IA), WS(IACOM), WS(
     1               IX), NX, WS(IY), WS(IW))                           
   6              CONTINUE                                              
               CALL LEAVE                                               
   7           CONTINUE                                                 
            CALL LEAVE                                                  
   8        CONTINUE                                                    
   9     CONTINUE                                                       
      CALL WRAPUP                                                       
      WRITE (IWRITE,  10)                                               
  10  FORMAT (24H  DL2SF TEST IS COMPLETE)                              
      STOP                                                              
      END                                                               
      SUBROUTINE CHECK(K, T, NT, A, ACOM, X, NX, Y, W)                  
      INTEGER NT, NX                                                    
      INTEGER K                                                         
      REAL T(NT), A(1), ACOM(1), X(NX), Y(NX, 2), W(NX, 2)              
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IS(1000), I, I1MACH                                       
      REAL RS(1000), WS(500), ERRBND, AMAX1, ABS, EXP                   
      REAL FLOAT, ERROR, R1MACH                                         
      LOGICAL LS(1000)                                                  
      INTEGER TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      IF (NT .LE. K) RETURN                                             
      ERROR = 0                                                         
      ERRBND = 1.0E+1*EXP(2.0E0)*FLOAT(NT*K**2)*R1MACH(4)               
      CALL F(X, NX, Y, W)                                               
      CALL DL2SF(X, Y, NX, K, T, NT, A)                                 
      TEMP = NT-K                                                       
      DO  1 I = 1, TEMP                                                 
         ERROR = AMAX1(ERROR, ABS(A(I)-ACOM(I)))                        
   1     CONTINUE                                                       
      IF (ERROR .LE. ERRBND) GOTO 3                                     
         WRITE (IWRITE,  2) ERROR                                       
   2     FORMAT (23H DDL2SF FAILS FOR F BY , 1PE10.2)                   
         ERROR = ERRBND                                                 
   3  CALL FD(X, NX, 2, Y, W)                                           
      CALL DL2SH(X, Y, NX, 2, W, K, T, NT, A)                           
      TEMP = NT-K                                                       
      DO  4 I = 1, TEMP                                                 
         ERROR = AMAX1(ERROR, ABS(A(I)-ACOM(I)))                        
   4     CONTINUE                                                       
      IF (ERROR .LE. ERRBND) GOTO 6                                     
         WRITE (IWRITE,  5) ERROR                                       
   5     FORMAT (24H DDL2SH FAILS FOR FD BY , 1PE10.2)                  
         ERROR = ERRBND                                                 
   6  CALL FW(X, NX, 1, Y, W)                                           
      CALL DL2SW(X, Y, NX, W, K, T, NT, A)                              
      TEMP = NT-K                                                       
      DO  7 I = 1, TEMP                                                 
         ERROR = AMAX1(ERROR, ABS(A(I)-ACOM(I)))                        
   7     CONTINUE                                                       
      IF (ERROR .LE. ERRBND) GOTO 9                                     
         WRITE (IWRITE,  8) ERROR                                       
   8     FORMAT (24H DDL2SW FAILS FOR FW BY , 1PE10.2)                  
         ERROR = ERRBND                                                 
   9  CALL FWD(X, NX, 2, Y, W)                                          
      CALL DL2SH(X, Y, NX, 2, W, K, T, NT, A)                           
      TEMP = NT-K                                                       
      DO  10 I = 1, TEMP                                                
         ERROR = AMAX1(ERROR, ABS(A(I)-ACOM(I)))                        
  10     CONTINUE                                                       
      IF (ERROR .LE. ERRBND) GOTO 12                                    
         WRITE (IWRITE,  11) ERROR                                      
  11     FORMAT (25H DDL2SH FAILS FOR FWD BY , 1PE10.2)                 
         ERROR = ERRBND                                                 
  12  RETURN                                                            
      END                                                               
      SUBROUTINE F(X, NX, FX, WX)                                       
      INTEGER NX                                                        
      REAL X(NX), FX(NX), WX(NX)                                        
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /PASS/ K, IT, NT, IA                                       
      INTEGER K, IT, NT, IA                                             
      INTEGER IS(1000)                                                  
      REAL RS(1000), WS(500)                                            
      LOGICAL LS(1000)                                                  
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      CALL SPLNE(K, WS(IT), NT, WS(IA), X, NX, FX)                      
      CALL SETR(NX, 1.0E0, WX)                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE FW(X, NX, MD, FX, WX)                                  
      INTEGER MD, NX                                                    
      REAL X(NX), FX(NX, MD), WX(NX, MD)                                
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /PASS/ K, IT, NT, IA                                       
      INTEGER K, IT, NT, IA                                             
      INTEGER IS(1000), I, J                                            
      REAL RS(1000), WS(500)                                            
      LOGICAL LS(1000)                                                  
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      CALL SPLNE(K, WS(IT), NT, WS(IA), X, NX, FX)                      
      DO  2 I = 1, NX                                                   
         DO  1 J = 1, MD                                                
            WX(I, J) = X(I)**2+1.0E0                                    
   1        CONTINUE                                                    
   2     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE FD(X, NX, MD, FX, WX)                                  
      INTEGER MD, NX                                                    
      REAL X(NX), FX(NX, MD), WX(NX, MD)                                
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /PASS/ K, IT, NT, IA                                       
      INTEGER K, IT, NT, IA                                             
      INTEGER IS(1000)                                                  
      REAL RS(1000), WS(500)                                            
      LOGICAL LS(1000)                                                  
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      CALL SPLND(K, WS(IT), NT, WS(IA), X, NX, MD, FX)                  
      CALL SETR(NX*MD, 1.0E0, WX)                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE FWD(X, NX, MD, FX, WX)                                 
      INTEGER MD, NX                                                    
      REAL X(NX), FX(NX, MD), WX(NX, MD)                                
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /PASS/ K, IT, NT, IA                                       
      INTEGER K, IT, NT, IA                                             
      INTEGER IS(1000), I, J                                            
      REAL RS(1000), WS(500)                                            
      LOGICAL LS(1000)                                                  
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      CALL SPLND(K, WS(IT), NT, WS(IA), X, NX, MD, FX)                  
      DO  2 I = 1, NX                                                   
         DO  1 J = 1, MD                                                
            WX(I, J) = X(I)**2+1.0E0                                    
   1        CONTINUE                                                    
   2     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE WRAPUP                                                 
      INTEGER LUSED, I1MACH, NERROR, ISTKST, LMAX, NERR                 
      INTEGER IWRITE                                                    
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
      IWRITE = I1MACH(2)                                                
      WRITE (IWRITE,  99) LUSED, LMAX                                   
  99  FORMAT (6H USED , I8, 3H / , I8, 22H OF THE STACK ALLOWED.)       
      RETURN                                                            
      END                                                               
C$TEST SPLE                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM SPLNE                                       
C                                                                       
C***********************************************************************
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER NA(2), IS(1000), IT, NT, IUMB, K                          
      INTEGER NDT, IPUMB                                                
      INTEGER TEMP, TEMP1, IWRITE, I1MACH                               
      REAL XB(3), RS(1000), WS(1000)                                    
      LOGICAL LS(1000)                                                  
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      CALL ISTKIN(1000, 3)                                              
C                                                                       
C  SET OUTPUT UNIT                                                      
C                                                                       
      IWRITE = I1MACH(2)                                                
      XB(1) = -1                                                        
      XB(2) = 0                                                         
      XB(3) = 2                                                         
      DO  4 K = 2, 4                                                    
         TEMP = 4*K+3                                                   
         DO  3 NDT = 2, TEMP                                            
            CALL ENTER(1)                                               
            IT = IUMB(XB(1), XB(3), NDT, K, NT)                         
            CALL USPLCK(K, WS(IT), NT)                                  
            CALL LEAVE                                                  
            CALL ENTER(1)                                               
            IF (NDT .NE. 2) GOTO 1                                      
               IT = IUMB(XB(1), XB(3), NDT, K, NT)                      
               GOTO  2                                                  
   1           NA(1) = (NDT+1)/2                                        
               NA(2) = NDT-NA(1)+1                                      
               IT = IPUMB(XB, 3, NA, K, NT)                             
   2        CALL OSPLCK(K, WS(IT), NT)                                  
            TEMP1 = IT+K                                                
            CALL MSPLCK(K, WS(IT), NT, WS(TEMP1-1), NT-2*(K-1))         
            CALL LEAVE                                                  
   3        CONTINUE                                                    
   4     CONTINUE                                                       
      CALL WRAPUP                                                       
      TEMP = I1MACH(2)                                                  
      WRITE (IWRITE,  5)                                                
   5  FORMAT (21H DSPLNE TEST COMPLETE)                                 
      STOP                                                              
      END                                                               
      SUBROUTINE USPLCK(K, T, NT)                                       
      INTEGER NT                                                        
      INTEGER K                                                         
      REAL T(NT)                                                        
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IA, IS(1000), IBASIS, ISTKGT, IBAISC, IUBSIS              
      REAL RS(1000), WS(500)                                            
      LOGICAL LS(1000)                                                  
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      CALL ENTER(1)                                                     
      IA = ISTKGT(NT-K, 3)                                              
      IBASIS = ISTKGT(K**2*(K+1), 3)                                    
      IUBSIS = ISTKGT(K**2*(K+1), 3)                                    
      IBAISC = ISTKGT(K**2*(K+1), 3)                                    
      CALL U8PLCK(K, T, NT, WS(IA), WS(IBASIS), WS(IUBSIS), WS(IBAISC)) 
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE U8PLCK(K, T, NT, A, BASIS, UBASIS, BASISC)             
      INTEGER NT                                                        
      INTEGER K                                                         
      REAL T(NT), A(1), BASIS(1), UBASIS(1), BASISC(1)                  
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ID(1), JD, JJ, IS(1000), IUMD, IXCHEK                     
      INTEGER IYCHEK, NXCHEK, ISTKGT, INTRVR, I, J                      
      INTEGER M, IID, INA, ILEFT, IAINT, IXCECK                         
      INTEGER ILUMB, NXCECK, IPUMD, ILUMD, MTEMP, IXFIT                 
      INTEGER IYFIT, ITINT, NXFIT, NTINT, IVALU                         
      INTEGER IUM1                                                      
      REAL AMAXDF, RS(1000), ERRBND, TEMP, WS(500), AMAX1               
      REAL ABS, FLOAT, ERROR, R1MACH                                    
      LOGICAL LS(1000)                                                  
      INTEGER TEMP1, TEMP2, TEMP3, IWRITE, I1MACH                       
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      ERROR = 0                                                         
      ERRBND = 1.0E+1*10.E0**(FLOAT(K)/5.0E0)*FLOAT(K**2)*R1MACH(4)*    
     1   FLOAT(NT-2*K+1)**(K-1)                                         
      CALL ENTER(1)                                                     
      M = K+1                                                           
      INA = ISTKGT(K, 2)                                                
      DO  1 I = 1, K                                                    
         TEMP1 = INA+I                                                  
         IS(TEMP1-1) = M                                                
   1     CONTINUE                                                       
      IID = ISTKGT(K-1, 2)                                              
      TEMP1 = K-1                                                       
      DO  2 I = 1, TEMP1                                                
         TEMP3 = IID+I                                                  
         IS(TEMP3-1) = I                                                
   2     CONTINUE                                                       
      TEMP1 = NT-K                                                      
      DO  24 I = 1, TEMP1                                               
         CALL SETR(NT-K, 0.0E0, A)                                      
         A(I) = 1                                                       
         CALL ENTER(1)                                                  
         ITINT = ILUMB(T, NT, 2, K+1, NTINT)                            
         IAINT = ISTKGT(NTINT-(K+1), 3)                                 
         IXFIT = ILUMD(T, NT, K+1, NXFIT)                               
         IYFIT = ISTKGT(NXFIT, 3)                                       
         CALL SPLNI(K, T, NT, A, WS(IXFIT), NXFIT, WS(IYFIT))           
         ERROR = AMAX1(ERROR, ABS(WS(IYFIT)))                           
         IF (ERROR .LE. ERRBND) GOTO 4                                  
            IWRITE = I1MACH(2)                                          
            WRITE (IWRITE,  3) ERROR                                    
   3        FORMAT (32H DSPLNI(T(1)) DIFFERS FROM 0 BY , 1PE10.2)       
            ERROR = ERRBND                                              
   4     CALL DL2SF(WS(IXFIT), WS(IYFIT), NXFIT, K+1, WS(ITINT), NTINT  
     1      , WS(IAINT))                                                
         CALL SPLNE(K, T, NT, A, WS(IXFIT), NXFIT, WS(IYFIT))           
         IVALU = ISTKGT(NXFIT, 3)                                       
         ID(1) = 1                                                      
         CALL SPLN1(K+1, WS(ITINT), NTINT, WS(IAINT), WS(IXFIT), NXFIT  
     1      , ID, 1, WS(IVALU))                                         
         ERROR = AMAX1(ERROR, AMAXDF(WS(IYFIT), 1, WS(IVALU), 1, NXFIT))
         IF (ERROR .LE. ERRBND) GOTO 6                                  
            WRITE (IWRITE,  5) ERROR                                    
   5        FORMAT (                                                    
     1         49H THE DERIVATIVE OF DSPLNI DIFFERS FROM DSPLNE BY , 1P 
     2         E10.2)                                                   
            ERROR = ERRBND                                              
   6     CALL LEAVE                                                     
         CALL ENTER(1)                                                  
         TEMP3 = I+K                                                    
         ILEFT = INTRVR(NT, T, T(TEMP3-1))                              
         IXCHEK = ILUMD(T(ILEFT), 2, M, NXCHEK)                         
         IYCHEK = ISTKGT(NXCHEK, 3)                                     
         CALL SPLNI(K, T, NT, A, WS(IXCHEK), NXCHEK, WS(IYCHEK))        
         IVALU = ISTKGT(K*NXCHEK, 3)                                    
         CALL BSPLI(K, T, NT, WS(IXCHEK), NXCHEK, ILEFT, WS(IVALU))     
         TEMP3 = IVALU+(I+K-ILEFT-1)*NXCHEK                             
         ERROR = AMAX1(ERROR, AMAXDF(WS(IYCHEK), 1, WS(TEMP3), 1,       
     1      NXCHEK))                                                    
         IF (ERROR .LE. ERRBND) GOTO 8                                  
            WRITE (IWRITE,  7) ERROR                                    
   7        FORMAT (31H DSPLNI AND DBSPLI DISAGREE BY , 1PE10.2)        
            ERROR = ERRBND                                              
   8     CALL LEAVE                                                     
         CALL ENTER(1)                                                  
         IXCECK = IPUMD(T(I), K+1, IS(INA), NXCECK)                     
         CALL SPLNE(K, T, NT, A, WS(IXCECK), NXCECK, BASISC)            
         CALL SPLND(K, T, NT, A, WS(IXCECK), NXCECK, K, BASIS)          
         ERROR = AMAX1(ERROR, AMAXDF(BASISC, 1, BASIS, 1, NXCECK))      
         IF (ERROR .LE. ERRBND) GOTO 10                                 
            WRITE (IWRITE,  9) ERROR                                    
   9        FORMAT (31H DSPLNE AND DSPLND DISAGREE BY , 1PE10.2)        
            ERROR = ERRBND                                              
  10     IF (I .EQ. K) CALL MOVEFR(K*NXCECK, BASIS, UBASIS)             
         CALL MOVEFR(K*NXCECK, BASIS, BASISC)                           
         CALL SPLN1(K, T, NT, A, WS(IXCECK), NXCECK, IS(IID), K-1,      
     1      BASIS)                                                      
         ERROR = AMAX1(ERROR, AMAXDF(BASISC(NXCECK+1), 1, BASIS, 1, (K-1
     1      )*NXCECK))                                                  
         IF (ERROR .LE. ERRBND) GOTO 12                                 
            WRITE (IWRITE,  11) ERROR                                   
  11        FORMAT (31H DSPLND AND DSPLN1 DISAGREE BY , 1PE10.2)        
            ERROR = ERRBND                                              
  12     IF (K .LE. I .AND. I .LT. NT-2*K+1) ERROR = AMAX1(ERROR,       
     1      AMAXDF(BASISC, 1, UBASIS, 1, K*NXCECK))                     
         IF (ERROR .LE. ERRBND) GOTO 14                                 
            WRITE (IWRITE,  13) ERROR, K, NT, I                         
  13        FORMAT (48H INTERIOR B SUB I DIFFER FROM CANONICAL FORM BY ,
     1         1PE10.2, 12H   K,NT,I = , I3, I3, I3)                    
            ERROR = ERRBND                                              
  14     CALL LEAVE                                                     
         TEMP3 = I+K-1                                                  
         DO  23 J = I, TEMP3                                            
            IF (T(J) .EQ. T(J+1)) GOTO  23                              
            CALL ENTER(1)                                               
            IF (T(J+1) .EQ. T(NT)) IXCECK = IUMD(T(J), T(J+1), M)       
            IF (T(J+1) .LT. T(NT)) IXCECK = IUM1(T(J), T(J+1), M+1, 1, 1
     1         , 0, MTEMP)                                              
            CALL BSPLN(K, T, NT, WS(IXCECK), M, J, BASIS)               
            CALL BSPLD(K, T, NT, WS(IXCECK), M, J, K, BASISC)           
            ERROR = AMAX1(ERROR, AMAXDF(BASIS, 1, BASISC, 1, K*M))      
            IF (ERROR .LE. ERRBND) GOTO 16                              
               WRITE (IWRITE,  15) ERROR                                
  15           FORMAT (33H DBSPLN DISAGREES WITH DBSPLD BY , 1PE10.2)   
               ERROR = ERRBND                                           
  16        CALL MOVEFR(K**2*M, BASISC, BASIS)                          
            CALL BSPL1(K, T, NT, WS(IXCECK), M, J, IS(IID), K-1, BASISC)
            TEMP2 = M*K                                                 
            ERROR = AMAX1(ERROR, AMAXDF(BASIS(TEMP2+1), 1, BASISC, 1, (K
     1         -1)*M*K))                                                
            IF (ERROR .LE. ERRBND) GOTO 18                              
               WRITE (IWRITE,  17) ERROR                                
  17           FORMAT (33H DBSPLD DISAGREES WITH DBSPL1 BY , 1PE10.2)   
               ERROR = ERRBND                                           
  18        DO  20 JJ = 1, K                                            
               CALL SETR(NT-K, 0.0E0, A)                                
               TEMP2 = J-K+JJ                                           
               A(TEMP2) = 1                                             
               DO  19 JD = 1, K                                         
                  ID(1) = JD-1                                          
                  CALL SPLN1(K, T, NT, A, WS(IXCECK), M, ID, 1, BASISC) 
                  TEMP2 = (JJ-1)*M+1+(JD-1)*M*K                         
                  ERROR = AMAX1(ERROR, AMAXDF(BASIS(TEMP2), 1, BASISC, 1
     1               , M))                                              
  19              CONTINUE                                              
  20           CONTINUE                                                 
            IF (ERROR .LE. ERRBND) GOTO 22                              
               WRITE (IWRITE,  21) ERROR, K, NT, I, J                   
  21           FORMAT (33H DSPLN1 DISAGREES WITH DBSPLD BY , 1PE10.2,   
     1            14H   K,NT,I,J = , I3, I3, I3, I3)                    
               ERROR = ERRBND                                           
  22        CALL LEAVE                                                  
  23        CONTINUE                                                    
  24     CONTINUE                                                       
      DO  31 I = 1, K                                                   
         CALL ENTER(1)                                                  
         CALL SETR(NT-K, 0.0E0, A)                                      
         A(I) = 1                                                       
         IXCECK = ILUMD(T(I), K+1, M, NXCECK)                           
         CALL SPLND(K, T, NT, A, WS(IXCECK), NXCECK, K, BASIS)          
         J = NT-K+1-I                                                   
         CALL SETR(NT-K, 0.0E0, A)                                      
         A(J) = 1                                                       
         IXCECK = ILUMD(T(J), K+1, M, MTEMP)                            
         TEMP1 = NXCECK/2                                               
         DO  25 J = 1, TEMP1                                            
            TEMP3 = IXCECK+J                                            
            TEMP = WS(TEMP3-1)                                          
            TEMP3 = IXCECK+J                                            
            TEMP2 = IXCECK+NXCECK-J                                     
            WS(TEMP3-1) = WS(TEMP2)                                     
            TEMP2 = IXCECK+NXCECK-J                                     
            WS(TEMP2) = TEMP                                            
  25        CONTINUE                                                    
         CALL SPLND(K, T, NT, A, WS(IXCECK), NXCECK, K, BASISC)         
         DO  27 J = 1, NXCECK                                           
            DO  26 JJ = 2, K, 2                                         
               TEMP1 = J+(JJ-1)*NXCECK                                  
               BASISC(TEMP1) = BASISC(TEMP1)*(-1.0E0)                   
  26           CONTINUE                                                 
  27        CONTINUE                                                    
         TEMP1 = NXCECK+(K-1)*NXCECK                                    
         TEMP2 = NXCECK-1+(K-1)*NXCECK                                  
         BASIS(TEMP1) = BASIS(TEMP2)                                    
         J = M                                                          
            GOTO  29                                                    
  28        J = J+M-1                                                   
  29        IF (J .GE. NXCECK) GOTO  30                                 
            TEMP2 = J+(K-1)*NXCECK                                      
            TEMP1 = J+1+(K-1)*NXCECK                                    
            BASISC(TEMP2) = BASISC(TEMP1)                               
            GOTO  28                                                    
  30     ERROR = AMAX1(ERROR, AMAXDF(BASIS, 1, BASISC, 1, K*NXCECK))    
         CALL LEAVE                                                     
  31     CONTINUE                                                       
      IF (ERROR .LE. ERRBND) GOTO 33                                    
         WRITE (IWRITE,  32) ERROR                                      
  32     FORMAT (44H THE END BASIS SPLINES ARE NOT SYMMETRIC BY , 1P    
     1      E10.2)                                                      
         ERROR = ERRBND                                                 
  33  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE OSPLCK(K, T, NT)                                       
      INTEGER NT                                                        
      INTEGER K                                                         
      REAL T(NT)                                                        
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IA, IB, ID(1), IS(1000), IUMD, IXCHEK                     
      INTEGER IYCHEK, NXCHEK, ISTKGT, I, J, L                           
      INTEGER INA, IXCECK, NXCECK, IVALU, IPUMD, ILUMD                  
      INTEGER MTEMP, I1MACH, IUM1, IWRITE                               
      REAL RS(1000), WS(500), ERRBND, VALU, AMAX1, ABS                  
      REAL FLOAT, EXP, ERROR, R1MACH                                    
      LOGICAL LS(1000)                                                  
      INTEGER TEMP, TEMP1, TEMP2, TEMP3                                 
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      ERRBND = 1.0E+1*10.0E0**(FLOAT(K)/5.0E0)*FLOAT(K**2)*R1MACH(4)*   
     1   EXP(2.0E0)*FLOAT(NT-2*K+1)**(K-1)                              
      ERROR = 0                                                         
      CALL ENTER(1)                                                     
      IA = ISTKGT(NT-K, 3)                                              
      CALL SETR(NT-K, 1.0E0, WS(IA))                                    
      CALL ENTER(1)                                                     
      IXCHEK = ILUMD(T, NT, K+1, NXCHEK)                                
      IYCHEK = ISTKGT(NXCHEK, 3)                                        
      CALL SPLNI(K, T, NT, WS(IA), WS(IXCHEK), NXCHEK, WS(IYCHEK))      
      DO  1 I = 1, NXCHEK                                               
         TEMP1 = IYCHEK+I                                               
         TEMP = IXCHEK+I                                                
         WS(TEMP1-1) = WS(TEMP1-1)-(WS(TEMP-1)-T(1))                    
         TEMP = IYCHEK+I                                                
         ERROR = AMAX1(ERROR, ABS(WS(TEMP-1)))                          
   1     CONTINUE                                                       
      IF (ERROR .LE. ERRBND) GOTO 3                                     
         IWRITE = I1MACH(2)                                             
         WRITE (IWRITE,  2) ERROR                                       
   2     FORMAT (34H DSPLNI(1) DIFFERS FROM X-T(1) BY , 1PE10.2)        
         ERROR = ERRBND                                                 
   3  CALL LEAVE                                                        
      INA = ISTKGT(NT-1, 2)                                             
      TEMP = NT-1                                                       
      DO  4 I = 1, TEMP                                                 
         TEMP1 = INA+I                                                  
         IS(TEMP1-1) = K+1                                              
   4     CONTINUE                                                       
      IXCECK = IPUMD(T, NT, IS(INA), NXCECK)                            
      IVALU = ISTKGT(NXCECK, 3)                                         
      DO  10 J = 1, K                                                   
         ID(1) = J-1                                                    
         CALL SPLN1(K, T, NT, WS(IA), WS(IXCECK), NXCECK, ID, 1, WS(    
     1      IVALU))                                                     
         IF (J .NE. 1) GOTO 5                                           
            VALU = 1                                                    
            GOTO  6                                                     
   5        VALU = 0                                                    
   6     DO  7 I = 1, NXCECK                                            
            TEMP = IVALU+I                                              
            ERROR = AMAX1(ERROR, ABS(WS(TEMP-1)-VALU))                  
   7        CONTINUE                                                    
         IF (ERROR .LE. ERRBND) GOTO 9                                  
            WRITE (IWRITE,  8) ERROR                                    
   8        FORMAT (35H B(1)+...+B(N-K) DIFFERS FROM 1 BY , 1PE10.2)    
            ERROR = ERRBND                                              
   9     CONTINUE                                                       
  10     CONTINUE                                                       
      CALL LEAVE                                                        
      CALL ENTER(1)                                                     
      IVALU = ISTKGT(K*(K+1), 3)                                        
      TEMP = NT-K                                                       
      DO  20 I = K, TEMP                                                
         CALL ENTER(1)                                                  
         IF (I .NE. NT-K) GOTO 11                                       
            IXCECK = IUMD(T(I), T(I+1), K+1)                            
            GOTO  12                                                    
  11        IXCECK = IUM1(T(I), T(I+1), K+2, 1, 1, 0, MTEMP)            
  12     DO  19 J = 1, K                                                
            ID(1) = J-1                                                 
            CALL BSPL1(K, T, NT, WS(IXCECK), K+1, I, ID, 1, WS(IVALU))  
            ERROR = 0                                                   
            IF (J .NE. 1) GOTO 13                                       
               VALU = 1                                                 
               GOTO  14                                                 
  13           VALU = 0                                                 
  14        TEMP1 = K+1                                                 
            DO  16 L = 1, TEMP1                                         
               DO  15 IB = 2, K                                         
                  TEMP3 = IVALU+L                                       
                  TEMP2 = IVALU+L-1+(IB-1)*(K+1)                        
                  WS(TEMP3-1) = WS(TEMP3-1)+WS(TEMP2)                   
  15              CONTINUE                                              
               TEMP2 = IVALU+L                                          
               ERROR = AMAX1(ERROR, ABS(WS(TEMP2-1)-VALU))              
  16           CONTINUE                                                 
            IF (ERROR .LE. ERRBND) GOTO 18                              
               WRITE (IWRITE,  17) ERROR                                
  17           FORMAT (43H BASIS(1)+...+BASIS(N-K) DIFFERS FROM 1 BY ,  
     1            1PE10.2)                                              
               ERROR = ERRBND                                           
  18        CONTINUE                                                    
  19        CONTINUE                                                    
         CALL LEAVE                                                     
  20     CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE MSPLCK(K, T1, NT1, T2, NT2)                            
      INTEGER NT1, NT2                                                  
      INTEGER K                                                         
      REAL T1(NT1), T2(NT2)                                             
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IA, ID(1), IS(1000), IUMD, IBASIS, IXCHEK                 
      INTEGER NXCHEK, ISTKGT, IYCEK1, IYCEK2, I, J                      
      INTEGER IID, INA, IBAISC, IXCECK, NXCECK, IPUMD                   
      INTEGER ILUMD, I1MACH, MAX0, IVALU1, IVALU2                       
      REAL AMAXDF, RS(1000), WS(500), ERRBND, AMAX1, FLOAT              
      REAL EXP, ERROR, R1MACH                                           
      LOGICAL LS(1000)                                                  
      INTEGER TEMP, TEMP1, IWRITE                                       
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      ERRBND = 1.0E+1*10.0E0**(FLOAT(K)/5.0E0)*FLOAT(K**2)*R1MACH(4)*   
     1   EXP(2.0E0)*FLOAT(NT1-2*K+1)**(K-1)                             
      ERROR = 0                                                         
      IF (NT1 .LT. 3*K-1) GOTO 8                                        
         CALL ENTER(1)                                                  
         IA = ISTKGT(NT1-K, 3)                                          
         CALL SETR(NT1-K, 0.0E0, WS(IA))                                
         TEMP = NT2-K                                                   
         DO  1 I = 1, TEMP                                              
            TEMP1 = IA+I+K                                              
            WS(TEMP1-2) = I                                             
   1        CONTINUE                                                    
         CALL ENTER(1)                                                  
         IXCHEK = ILUMD(T1, NT1, K+1, NXCHEK)                           
         IYCEK1 = ISTKGT(NXCHEK, 3)                                     
         IYCEK2 = ISTKGT(NXCHEK, 3)                                     
         CALL SPLNI(K, T1, NT1, WS(IA), WS(IXCHEK), NXCHEK, WS(IYCEK1)) 
         TEMP = IA+K                                                    
         CALL SPLNI(K, T2, NT2, WS(TEMP-1), WS(IXCHEK), NXCHEK, WS(     
     1      IYCEK2))                                                    
         ERROR = AMAX1(ERROR, AMAXDF(WS(IYCEK1), 1, WS(IYCEK2), 1,      
     1      NXCHEK))                                                    
         IF (ERROR .LE. ERRBND) GOTO 3                                  
            WRITE (IWRITE,  2) ERROR                                    
   2        FORMAT (46H DSPLNI HANDLES MULTIPLICITIES INCORRECTLY BY ,  
     1         1PE10.2)                                                 
            ERROR = ERRBND                                              
   3     CALL LEAVE                                                     
         INA = ISTKGT(NT1-1, 2)                                         
         TEMP = NT1-1                                                   
         DO  4 I = 1, TEMP                                              
            TEMP1 = INA+I                                               
            IS(TEMP1-1) = K+1                                           
   4        CONTINUE                                                    
         IXCECK = IPUMD(T1, NT1, IS(INA), NXCECK)                       
         IVALU1 = ISTKGT(NXCECK, 3)                                     
         IVALU2 = ISTKGT(NXCECK, 3)                                     
         DO  7 J = 1, K                                                 
            ID(1) = J-1                                                 
            CALL SPLN1(K, T1, NT1, WS(IA), WS(IXCECK), NXCECK, ID, 1,   
     1         WS(IVALU1))                                              
            TEMP = IA+K                                                 
            CALL SPLN1(K, T2, NT2, WS(TEMP-1), WS(IXCECK), NXCECK, ID, 1
     1         , WS(IVALU2))                                            
            ERROR = AMAXDF(WS(IVALU1), 1, WS(IVALU2), 1, NXCECK)        
            IF (ERROR .LE. ERRBND) GOTO 6                               
               WRITE (IWRITE,  5) ERROR                                 
   5           FORMAT (23H F1 DIFFERS FROM F2 BY , 1PE10.2)             
               ERROR = ERRBND                                           
   6        CONTINUE                                                    
   7        CONTINUE                                                    
         CALL LEAVE                                                     
   8  IF (NT1 .GT. 2*K) RETURN                                          
      CALL ENTER(1)                                                     
      IBASIS = ISTKGT(K**2*(K+1), 3)                                    
      IBAISC = ISTKGT(K**2*(K+1), 3)                                    
      IXCECK = IUMD(T1(K), T1(K+1), K+2)                                
      NXCECK = K+1                                                      
      IID = ISTKGT(K, 2)                                                
      DO  9 I = 1, K                                                    
         TEMP = IID+I                                                   
         IS(TEMP-1) = I-1                                               
   9     CONTINUE                                                       
      DO  13 I = 1, K                                                   
         IF (I .GT. 1) T1(I-1) = R1MACH(2)                              
         IF (I+MAX0(2*(K+1-I), K+1) .GT. NT1) GOTO 10                   
            TEMP = I+MAX0(2*(K+1-I), K+1)                               
            T1(TEMP) = -R1MACH(2)                                       
  10     CALL BSPL1(K, T1(I), MAX0(2*(K+1-I), K+1), WS(IXCECK), NXCECK  
     1      , K+1-I, IS(IID), K, WS(IBASIS))                            
         IF (I .EQ. 1) CALL MOVEFR(K**2*NXCECK, WS(IBASIS), WS(IBAISC)) 
         ERROR = AMAX1(ERROR, AMAXDF(WS(IBASIS), 1, WS(IBAISC), 1, K**2*
     1      NXCECK))                                                    
         IF (ERROR .LE. ERRBND) GOTO 12                                 
            WRITE (IWRITE,  11) ERROR                                   
  11        FORMAT (                                                    
     1         50H DBSPL1 HANDLES END MULTIPLICITIES INCORRECTLY BY ,   
     2         1PE10.2)                                                 
            ERROR = ERRBND                                              
  12     CONTINUE                                                       
  13     CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      REAL FUNCTION AMAXDF(A, INCA, B, INCB, NAB)                       
      INTEGER INCA, INCB, NAB                                           
      REAL A(INCA, NAB), B(INCB, NAB)                                   
      INTEGER I                                                         
      REAL AMAX1, ABS, MAXIFF                                           
      MAXIFF = 0                                                        
      I = 1                                                             
         GOTO  2                                                        
   1     I = I+1                                                        
   2     IF (I .GT. NAB) GOTO  3                                        
         MAXIFF = AMAX1(MAXIFF, ABS(A(1, I)-B(1, I)))                   
         GOTO  1                                                        
   3  AMAXDF = MAXIFF                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE WRAPUP                                                 
      INTEGER LUSED, I1MACH, NERROR, ISTKST, LMAX, NERR                 
      INTEGER IWRITE                                                    
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
      IWRITE = I1MACH(2)                                                
      WRITE (IWRITE,  99) LUSED, LMAX                                   
  99  FORMAT (6H USED , I8, 3H / , I8, 22H OF THE STACK ALLOWED.)       
      RETURN                                                            
      END                                                               
C$TEST VDAD                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM DVDSS1                                      
C                                                                       
C***********************************************************************
C  DSIN(X) + X**2 + 3                                                   
C                                                                       
       DOUBLE PRECISION X, A(10), F, FPRIME, PT, H, SOL                 
       INTEGER I1MACH, IWRITE, N, I                                     
       DOUBLE PRECISION U, L, DIST, DSOL                                
       DOUBLE PRECISION DSIN, DCOS                                      
C                                                                       
       IWRITE = I1MACH(2)                                               
       N = 10                                                           
       X = 1.5D0                                                        
       H = 1.D0/ DBLE(FLOAT(N-1))                                       
       L = 1.0D0                                                        
       U = 5.0D0                                                        
       DIST = U - L                                                     
C SET UP THE SPLINE COEFFICIENTS                                        
       DO 100 I=1,N                                                     
              PT = L + DIST*DBLE(FLOAT(I-1))*H                          
              A(I) = DSIN(PT) + PT**2 + 3.0D0                           
 100   CONTINUE                                                         
       CALL DVDSS1 (X,N,U,L,A,F,FPRIME)                                 
C CHECK SOLUTION                                                        
       SOL = DSIN(X) + X**2 + 3.0D0                                     
       DSOL = DCOS(X) + 2.0D0*X                                         
       WRITE (IWRITE,101)                                               
 101    FORMAT (34H                ACTUAL    COMPUTED)                  
       WRITE (IWRITE,102) SOL,F                                         
 102    FORMAT (15H          F(X)=,2D15.8)                              
       WRITE (IWRITE,103) DSOL,FPRIME                                   
 103    FORMAT (15H    DERIVATIVE=,2D15.8)                              
       STOP                                                             
       END                                                              
C$TEST VDBD                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM DVDSS2                                      
C                                                                       
C***********************************************************************
C  DSIN(X(1)) + X(2)**2 + 3                                             
C                                                                       
       DOUBLE PRECISION X(2), A(10,10), F, FPRIME(2), PT1, PT2, H1, H2  
       DOUBLE PRECISION SOL, DSOL1, DSOL2                               
       INTEGER I1MACH, IWRITE, N1, N2, NA1, I, J                        
       DOUBLE PRECISION U(2),L(2),DIST(2)                               
C                                                                       
       DOUBLE PRECISION  DSIN, DCOS                                     
C                                                                       
C                                                                       
       IWRITE = I1MACH(2)                                               
       N1 = 10                                                          
       N2 = 10                                                          
       X(1) = 1.5D0                                                     
       X(2) = 1.5D0                                                     
       H1 = 1.D0/ DBLE(FLOAT(N1-1))                                     
       H2 = 1.D0/ DBLE(FLOAT(N2-1))                                     
       L(1) = 1.0D0                                                     
       U(1) = 5.0D0                                                     
       L(2) = 1.0D0                                                     
       U(2) = 5.0D0                                                     
       DIST(1) = U(1) - L(1)                                            
       DIST(2) = U(2) - L(2)                                            
C SET UP THE SPLINE COEFFICIENTS                                        
       DO 100 I=1,N1                                                    
              PT1 = L(1) + DIST(1)*DBLE(FLOAT(I-1))*H1                  
              DO 100 J=1,N2                                             
                     PT2 = L(2) + DIST(2)*DBLE(FLOAT(J-1))*H2           
                     A(I,J) = DSIN(PT1) + PT2**2 + 3.0D0                
 100   CONTINUE                                                         
       NA1 = 10                                                         
       CALL DVDSS2 (X,N1,N2,U,L,A,NA1,F,FPRIME)                         
C CHECK SOLUTION                                                        
       SOL = DSIN(X(1)) + X(2)**2 + 3.0D0                               
       DSOL1 = DCOS(X(1))                                               
       DSOL2 = 2.0D0*X(2)                                               
       WRITE (IWRITE,101)                                               
 101    FORMAT (34H                ACTUAL    COMPUTED)                  
       WRITE (IWRITE,102) SOL,F                                         
 102    FORMAT (15H          F(X)=,2D15.8)                              
       WRITE (IWRITE,103) DSOL1,FPRIME(1)                               
 103    FORMAT (15H     PARTIAL X=,2D15.8)                              
       WRITE (IWRITE,104) DSOL2,FPRIME(2)                               
 104    FORMAT (15H     PARTIAL Y=,2D15.8)                              
       STOP                                                             
       END                                                              
C$TEST VDED                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM DVDSS3                                      
C                                                                       
C***********************************************************************
C  DSIN(X(1)) + X(2)**2 + 2*X(3) + 3                                    
C                                                                       
       DOUBLE PRECISION X(3), A(10,10,10), F, FPRIME(3), PT1, PT2, PT3  
       DOUBLE PRECISION H1, H2, H3, SOL                                 
       DOUBLE PRECISION DSOL1, DSOL2, DSOL3                             
       INTEGER I1MACH, IWRITE, N1, N2, N3, NA1, NA2                     
       INTEGER I,J,K                                                    
       DOUBLE PRECISION U(3),L(3),DIST(3)                               
C                                                                       
       IWRITE = I1MACH(2)                                               
       N1 = 10                                                          
       N2 = 10                                                          
       N3 = 10                                                          
       X(1) = 2.5D0                                                     
       X(2) = 2.5D0                                                     
       X(3) = 2.5D0                                                     
       H1 = 1.D0/ DBLE(FLOAT(N1-1))                                     
       H2 = 1.D0/ DBLE(FLOAT(N2-1))                                     
       H3 = 1.D0/ DBLE(FLOAT(N3-1))                                     
       L(1) = 1.0D0                                                     
       U(1) = 3.0D0                                                     
       L(2) = 2.0D0                                                     
       U(2) = 4.0D0                                                     
       L(3) = 1.5D0                                                     
       U(3) = 3.5D0                                                     
       DIST(1) = U(1) - L(1)                                            
       DIST(2) = U(2) - L(2)                                            
       DIST(3) = U(3) - L(3)                                            
C SET UP THE SPLINE COEFFICIENTS                                        
       DO 100 I=1,N1                                                    
              PT1 = L(1) + DIST(1)*DBLE(FLOAT(I-1))*H1                  
              DO 100 J=1,N2                                             
                     PT2 = L(2) + DIST(2)*DBLE(FLOAT(J-1))*H2           
                     DO 100 K=1,N3                                      
                            PT3 = L(3) + DIST(3)*DBLE(FLOAT(K-1))*H3    
                            A(I,J,K) = PT1*PT2 + PT3**2                 
 100   CONTINUE                                                         
       NA1 = 10                                                         
       NA2 = 10                                                         
       CALL DVDSS3 (X,N1,N2,N3,U,L,A,NA1,NA2,F,FPRIME)                  
C CHECK SOLUTION                                                        
       SOL = X(1)*X(2) + X(3)**2                                        
       DSOL1 = X(2)                                                     
       DSOL2 = X(1)                                                     
       DSOL3 = 2.0D0*X(3)                                               
       WRITE (IWRITE,101)                                               
 101    FORMAT (34H                ACTUAL    COMPUTED)                  
       WRITE (IWRITE,102) SOL,F                                         
 102    FORMAT (15H          F(X)=,2D15.8)                              
       WRITE (IWRITE,103) DSOL1,FPRIME(1)                               
 103    FORMAT (15H     PARTIAL X=,2D15.8)                              
       WRITE (IWRITE,104) DSOL2,FPRIME(2)                               
 104    FORMAT (15H     PARTIAL Y=,2D15.8)                              
       WRITE (IWRITE,105) DSOL3,FPRIME(3)                               
 105    FORMAT (15H     PARTIAL Z=,2D15.8)                              
       STOP                                                             
       END                                                              
C$TEST LTSQ                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAMS LTSQ AND LSTSQ                             
C                                                                       
C***********************************************************************
C TEST PROGRAM TO COMPARE LTSQ AND LSTSQ                                
       INTEGER I                                                        
       INTEGER M, N, IWRITE, I1MACH                                     
       REAL A(10,2)                                                     
       REAL B(10), W(10)                                                
       REAL X(10,2), Y(10), C(2)                                        
C                                                                       
       IWRITE = I1MACH(2)                                               
       M = 6                                                            
       N = 2                                                            
C SET THE FIRST COLUMN OF THE X AND A ARRAYS TO THE ACTUAL X,           
C AND THE SECOND COLUMN TO 1.0                                          
       DO 151 I=1,M                                                     
          A(I,1) = FLOAT(I)                                             
          X(I,1) = A(I,1)                                               
          A(I,2) = 1.0                                                  
          X(I,2) = A(I,2)                                               
 151   CONTINUE                                                         
C SET THE VALUES OF THE RIGHT HAND SIDES, Y AND B                       
       Y(1) = .3                                                        
       Y(2) = .95                                                       
       Y(3) = 2.6                                                       
       Y(4) = 2.5                                                       
       Y(5) = 2.3                                                       
       Y(6) = 3.95                                                      
       DO 145 I=1,M                                                     
          B(I) = Y(I)                                                   
 145   CONTINUE                                                         
C CALL THE LEAST SQUARES PACKAGE                                        
       CALL LSTSQ(10, 2, M, N, X, Y, 1, C)                              
       WRITE (IWRITE,103)                                               
 103    FORMAT (6H LSTSQ)                                               
       WRITE (IWRITE,100) C(1), C(2)                                    
 100    FORMAT (7H C(1)- ,E15.8, 7H C(2)- , E15.8)                      
C CALL THE SINGULAR VALUE DECOMPOSITION PACKAGE                         
       CALL LTSQ(M,N,A,10,B,1,W)                                        
       WRITE (IWRITE,104)                                               
 104    FORMAT (4H SVD)                                                 
       WRITE (IWRITE,100) B(1), B(2)                                    
       STOP                                                             
       END                                                              
C$TEST LTQD                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAMS DLTSQ AND DLSTSQ                           
C                                                                       
C***********************************************************************
C TDLTSQ2.F--TEST PROGRAM TO COMPARE DLTSQ AND DLSTSQ                   
       INTEGER I                                                        
       INTEGER M, N, IWUNIT, I1MACH                                     
       DOUBLE PRECISION A(10,2)                                         
       DOUBLE PRECISION B(10), W(10)                                    
       DOUBLE PRECISION X(10,2), Y(10), C(2)                            
       DOUBLE PRECISION DFLOAT                                          
C                                                                       
       IWUNIT = I1MACH(2)                                               
       M = 6                                                            
       N = 2                                                            
C SET THE FIRST COLUMN OF THE X AND A ARRAYS TO THE ACTUAL X,           
C AND THE SECOND COLUMN TO 1.0                                          
       DO 10 I=1,M                                                      
          A(I,1) = DFLOAT(I)                                            
          X(I,1) = A(I,1)                                               
          A(I,2) = 1.0                                                  
          X(I,2) = A(I,2)                                               
 10    CONTINUE                                                         
C SET THE VALUES OF THE RIGHT HAND SIDES, Y AND B                       
       Y(1) = .3                                                        
       Y(2) = .95                                                       
       Y(3) = 2.6                                                       
       Y(4) = 2.5                                                       
       Y(5) = 2.3                                                       
       Y(6) = 3.95                                                      
       DO 20 I=1,M                                                      
          B(I) = Y(I)                                                   
 20    CONTINUE                                                         
C CALL THE LEAST SQUARES PACKAGE                                        
       CALL DLSTSQ(10, 2, M, N, X, Y, 1, C)                             
       WRITE (IWUNIT,30)                                                
 30     FORMAT (7H DLSTSQ)                                              
       WRITE (IWUNIT,40) C(1), C(2)                                     
 40     FORMAT (7H C(1)- ,E15.8, 7H C(2)- , E15.8)                      
C CALL THE SINGULAR VALUE DECOMPOSITION PACKAGE                         
       CALL DLTSQ(M,N,A,10,B,1,W)                                       
       WRITE (IWUNIT,50)                                                
 50     FORMAT (5H DSVD)                                                
       WRITE (IWUNIT,40) B(1), B(2)                                     
       STOP                                                             
       END                                                              
C$TEST LGEA                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAMS GECE AND FRIENDS                           
C                                                                       
C***********************************************************************
C     MAIN PROGRAM                                                      
      INTEGER IWRITE, I1MACH                                            
C     ALLOW 5000 UNDERFLOWS.                                            
C                                                                       
C     OUTPUT UNIT NUMBER                                                
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      CALL SGETS(IWRITE)                                                
      STOP                                                              
      END                                                               
      SUBROUTINE SGETS(IWRITE)                                          
C     IWRITE IS THE OUTPUT UNIT NUMBER                                  
C                                                                       
C     TESTS                                                             
C        GECE,GEFS,GEBS,GEML,BACE,BABS,BAFS,BALE,BAML                   
C                                                                       
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     PORT GECE,GEFS,GEBS,GEML,BACE,BAFS,BABS,BAML,BALE                 
C     PORT UTILITIES ERROFF,ENTER,LEAVE,R1MACH                          
C     EXTERNAL SGEXX                                                    
C     BLAS SAXPY,SDOT,SSCAL,SASUM                                       
C     FORTRAN ABS,AMAX1,FLOAT,MAX0,MIN0                                 
C                                                                       
C     INTERNAL VARIABLES                                                
C                                                                       
      INTEGER I,IPVT(15),IPVTB(15),IQ(8),I1,I2,J                        
      INTEGER K,KASE,KB,KBFAIL,KOUNT,KP1,KSING,KSUSP(8)                 
      INTEGER L,LDA,LDAB,IWRITE,M,ML,MU,N,NM1,NPRINT                    
      REAL A(15,15),AB(43,15),AINV(15,15),ASAVE(15,15),AL(43,15)        
      REAL B(15)                                                        
      REAL X(15),XB(15),XEXACT(15),T                                    
      REAL AINORM,ANORM,COND1,EN,ENORM,EPS                              
      REAL FNI,FNORM,RCOND,RCONDB,RNORM                                 
      REAL Q(10),QS(10),SASUM,XNORM                                     
      REAL ABSAVE(43,15),AINVB(15,15)                                   
      LOGICAL KBF                                                       
C                                                                       
      LDA = 15                                                          
      CALL ENTER(1)                                                     
      LDAB = 43                                                         
C                                                                       
C     WRITE MATRIX AND SOLUTIONS IF  N .LE. NPRINT                      
C                                                                       
      NPRINT =3                                                         
C                                                                       
      WRITE (IWRITE,460)                                                
      WRITE (IWRITE,880)                                                
C                                                                       
      DO 10 I = 1, 8                                                    
         KSUSP(I) = 0                                                   
   10 CONTINUE                                                          
      KSING = 0                                                         
      KBFAIL = 0                                                        
C                                                                       
C     SET EPS TO ROUNDING UNIT                                          
C                                                                       
      EPS =R1MACH(4)                                                    
      WRITE (IWRITE,470) EPS                                            
      WRITE (IWRITE,450)                                                
C                                                                       
C     START MAIN LOOP                                                   
C                                                                       
      KASE = 1                                                          
   20 CONTINUE                                                          
C                                                                       
C        GENERATE TEST MATRIX                                           
C                                                                       
         CALL SGEXX(A,LDA,N,KASE,IWRITE)                                
C                                                                       
C        N = 0 SIGNALS NO MORE TEST MATRICES                            
C                                                                       
C     ...EXIT                                                           
         IF (N .LE. 0) GO TO 440                                        
         ANORM = 0.0E0                                                  
         DO 30 J = 1, N                                                 
            ANORM = AMAX1(ANORM,SASUM(N,A(1,J),1))                      
   30    CONTINUE                                                       
         WRITE (IWRITE,650) ANORM                                       
C                                                                       
         IF (N .GT. NPRINT) GO TO 50                                    
            WRITE (IWRITE,450)                                          
            DO 40 I = 1, N                                              
               WRITE (IWRITE,700) (A(I,J), J = 1, N)                    
   40       CONTINUE                                                    
            WRITE (IWRITE,450)                                          
   50    CONTINUE                                                       
C                                                                       
C        GENERATE EXACT SOLUTION                                        
C                                                                       
         XEXACT(1) = 1.0E0                                              
         IF (N .GE. 2) XEXACT(2) = 0.0E0                                
         IF (N .LE. 2) GO TO 70                                         
            DO 60 I = 3, N                                              
               XEXACT(I) = -XEXACT(I-2)                                 
   60       CONTINUE                                                    
   70    CONTINUE                                                       
C                                                                       
C        SAVE MATRIX AND GENERATE R.H.S.                                
C                                                                       
         DO 90 I = 1, N                                                 
            DO 80 J = 1, N                                              
               ASAVE(I,J) = A(I,J)                                      
   80       CONTINUE                                                    
   90    CONTINUE                                                       
         CALL GEML(N,A,LDA,XEXACT,B)                                    
         CALL MOVEFR(N,B,X)                                             
C                                                                       
C        FACTOR AND ESTIMATE CONDITION                                  
C                                                                       
         CALL GECE(N,A,LDA,IPVT,RCOND)                                  
         IF (NERROR(INE).NE.0) CALL  ERROFF                             
C                                                                       
C                                                                       
C        FACTOR BAND FORM AND COMPARE                                   
C                                                                       
         KBF = .FALSE.                                                  
         ML = 0                                                         
         MU = 0                                                         
         DO 140 J = 1, N                                                
            DO 130 I = 1, N                                             
               IF (ASAVE(I,J) .EQ. 0.0E0) GO TO 120                     
                  IF (I .LT. J) MU = MAX0(MU,J-I)                       
                  IF (I .GT. J) ML = MAX0(ML,I-J)                       
  120          CONTINUE                                                 
  130       CONTINUE                                                    
  140    CONTINUE                                                       
         WRITE (IWRITE,790) ML,MU                                       
            M = ML + MU + 1                                             
            DO 170 J = 1, N                                             
               I1 = MAX0(1,J-MU)                                        
               I2 = MIN0(N,J+ML)                                        
               DO 160 I = I1, I2                                        
                  K=ML+1+J-I                                            
                  AB(K,I) = ASAVE(I,J)                                  
                  ABSAVE(K,I)=ASAVE(I,J)                                
  160          CONTINUE                                                 
  170       CONTINUE                                                    
C                                                                       
            CALL BACE(N,ML+1,M,AB,LDAB,AL,LDAB,IPVTB,MU,RCONDB)         
          WRITE(IWRITE,820)RCOND,RCONDB                                 
C                                                                       
C                                                                       
C           TEST FOR SINGULARITY                                        
C                                                                       
           IF (INE+NERROR(IRE).EQ.0) GO TO 210                          
             IF (IRE.NE.0) CALL ERROFF                                  
             IF (IRE.NE.INE) WRITE(IWRITE,118)                          
 118         FORMAT(35H BAND AND GENERAL ROUTINES DISAGREE)             
               WRITE (IWRITE,480)                                       
               KSING = KSING + 1                                        
            GO TO 420                                                   
  210       CONTINUE                                                    
C                                                                       
C              COMPUTE INVERSE AND COND1 = TRUE CONDITION               
C                                                                       
               DO 230 J = 1, N                                          
                  DO 220 I = 1, N                                       
                     AINV(I,J) = 0.E0                                   
                     AINVB(I,J)=0.E0                                    
  220             CONTINUE                                              
               AINV(J,J)=1.E0                                           
               AINVB(J,J)=1.E0                                          
  230          CONTINUE                                                 
               CALL GEFS(N,A,LDA,AINV,LDA,N,IPVT)                       
               CALL GEBS(N,A,LDA,AINV,LDA,N)                            
               CALL BAFS(N,ML+1,AL,LDAB,IPVTB,AINVB,LDA,N)              
               CALL BABS(N,AB,LDAB,AINVB,LDA,N,MU)                      
               AINORM = 0.0E0                                           
               DO 240 J = 1, N                                          
                  AINORM = AMAX1(AINORM,SASUM(N,AINV(1,J),1))           
  240          CONTINUE                                                 
               COND1 = ANORM*AINORM                                     
               WRITE (IWRITE,510) COND1                                 
C                                                                       
C              SOLVE  A*X = B                                           
C                                                                       
               CALL GEFS(N,A,LDA,X,N,1,IPVT)                            
               CALL GEBS(N,A,LDA,X,N,1)                                 
C                                                                       
C              MORE BAND COMPARE                                        
C                                                                       
C              TEST CONSISTENCY OF BAML AND BALE                        
C                                                                       
               CALL BAML(N,ML+1,M,ABSAVE,LDAB,XEXACT,XB)                
               CALL BALE(N,ML+1,M,ABSAVE,LDAB,XB,N,1)                   
               IF (NERROR(IRE).EQ.0) GO TO 245                          
                 WRITE(IWRITE,490)                                      
                 CALL ERROFF                                            
                 GO TO 420                                              
 245           CONTINUE                                                 
               IF (N .GT. NPRINT) GO TO 270                             
                  WRITE (IWRITE,520)                                    
                  DO 250 I = 1, N                                       
                     WRITE (IWRITE,740) X(I),XB(I)                      
  250             CONTINUE                                              
                  WRITE (IWRITE,450)                                    
  270          CONTINUE                                                 
C                                                                       
C              RECONSTRUCT  A  FROM TRIANGULAR FACTORS , L AND U        
C                                                                       
               NM1 = N - 1                                              
               IF (NM1 .LT. 1) GO TO 330                                
               DO 320 KB = 1, NM1                                       
                  K = N - KB                                            
                  KP1 = K + 1                                           
                  L = IPVT(K)                                           
                  DO 310 J = KP1, N                                     
                     T = -A(K,J)                                        
                     CALL SAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)            
                     T = A(L,J)                                         
                     A(L,J) = A(K,J)                                    
                     A(K,J) = T                                         
  310             CONTINUE                                              
                  T = -A(K,K)                                           
                  CALL SSCAL(N-K,T,A(K+1,K),1)                          
                  T = A(L,K)                                            
                  A(L,K) = A(K,K)                                       
                  A(K,K) = T                                            
  320          CONTINUE                                                 
  330          CONTINUE                                                 
C                                                                       
C              COMPUTE ERRORS AND RESIDUALS                             
C                 E  =  X - XEXACT                                      
C                 EB =  XB - XEXACT                                     
C                 R  =  B - A*X                                         
C                 F  =  A - L*U                                         
C                 AI =  A*INV(A) - I                                    
C                 AIB = A(BAND)*INV(A(BAND)) - I                        
C                                                                       
               XNORM = SASUM(N,X,1)                                     
               ENORM = 0.0E0                                            
               EBNORM=0.E0                                              
               FNORM = 0.0E0                                            
               DO 350 J = 1, N                                          
                  ENORM = ENORM + ABS(X(J)-XEXACT(J))                   
                  EBNORM = EBNORM + ABS(XB(J) - XEXACT(J))              
                  T = -X(J)                                             
                  CALL SAXPY(N,T,ASAVE(1,J),1,B,1)                      
                  FNI = 0.0E0                                           
                  DO 340 I = 1, N                                       
                     FNI = FNI + ABS(ASAVE(I,J)-A(I,J))                 
  340             CONTINUE                                              
                  IF (FNI .GT. FNORM) FNORM = FNI                       
  350          CONTINUE                                                 
               RNORM = SASUM(N,B,1)                                     
C                                                                       
C              A*INV(A) - I                                             
C                                                                       
               AINORM = 0.0E0                                           
               AIBNO=0.0E0                                              
               DO 380 J = 1, N                                          
                  DO 360 I = 1, N                                       
                     B(I) = 0.0E0                                       
                     XB(I) = 0.E0                                       
  360             CONTINUE                                              
                  DO 370 K = 1, N                                       
                     T = AINV(K,J)                                      
                     CALL SAXPY(N,T,ASAVE(1,K),1,B,1)                   
                     T=AINVB(K,J)                                       
                     CALL SAXPY(N,T,ASAVE(1,K),1,XB,1)                  
  370             CONTINUE                                              
                  B(J) = B(J) - 1.0E0                                   
                  XB(J) = XB(J) -1.0E0                                  
                  AIBNO=AMAX1(AIBNO,SASUM(N,XB,1))                      
                  AINORM = AMAX1(AINORM,SASUM(N,B,1))                   
  380          CONTINUE                                                 
C                                                                       
               WRITE (IWRITE,540) ENORM,EBNORM                          
               WRITE (IWRITE,550) RNORM                                 
               WRITE (IWRITE,660) FNORM                                 
               WRITE (IWRITE,670) AINORM,AIBNO                          
C                                                                       
C              COMPUTE TEST RATIOS                                      
C                                                                       
               Q(1) = RCOND/COND1                                       
               Q(2) = RCONDB/COND1                                      
               Q(3) = COND1/RCOND                                       
               Q(4)=COND1/RCONDB                                        
               Q(5) = ENORM/(EPS*RCOND*XNORM)                           
               Q(6) = EBNORM/(EPS*RCOND*XNORM)                          
               DENOM=AMAX1(100.0*R1MACH(1),EPS*ANORM*XNORM)             
               Q(7) = RNORM/DENOM                                       
               DENOM=AMAX1(100.0*R1MACH(1),EPS*ANORM)                   
               Q(8)=FNORM/DENOM                                         
               Q(9) = AINORM/(EPS*RCOND)                                
               Q(10)=AIBNO/(EPS*RCOND)                                  
               WRITE (IWRITE,450)                                       
               WRITE (IWRITE,560)                                       
               WRITE (IWRITE,450)                                       
               WRITE (IWRITE,620)                                       
               WRITE (IWRITE,630)                                       
               WRITE (IWRITE,640)                                       
               WRITE (IWRITE,450)                                       
               WRITE (IWRITE,690) (Q(I), I = 1, 10)                     
               WRITE (IWRITE,450)                                       
C                                                                       
C              LOOK FOR SUSPICIOUS RATIOS                               
C                                                                       
               QS(1) = 1.0E0 + 4.0E0*EPS                                
               QS(2)=QS(1)                                              
               QS(4) = 10.0E0                                           
               QS(3)=10.0E0                                             
               EN = FLOAT(N)                                            
               IF (N .EQ. 1) EN = 2.0E0                                 
               DO 390 I = 3, 10                                         
                  QS(I) = EN                                            
  390          CONTINUE                                                 
               KOUNT = 0                                                
               DO 410 I = 1, 10                                         
                  IQ(I) = 0                                             
                  IF (Q(I) .LE. QS(I)) GO TO 400                        
                     IQ(I) = 1                                          
                     KSUSP(I) = KSUSP(I) + 1                            
                     KOUNT = KOUNT + 1                                  
  400             CONTINUE                                              
  410          CONTINUE                                                 
               IF (KOUNT .EQ. 0) WRITE (IWRITE,860)                     
               IF (KOUNT .NE. 0) WRITE (IWRITE,870) (IQ(I), I = 1, 10)  
               WRITE (IWRITE,450)                                       
  420       CONTINUE                                                    
  430    CONTINUE                                                       
C                                                                       
         WRITE (IWRITE,570)                                             
         KASE = KASE + 1                                                
      GO TO 20                                                          
  440 CONTINUE                                                          
C                                                                       
C     FINISH MAIN LOOP                                                  
C                                                                       
C     SUMMARY                                                           
C                                                                       
      WRITE (IWRITE,580)                                                
      KASE = KASE - 1                                                   
      WRITE (IWRITE,590) KASE                                           
      WRITE (IWRITE,600) KSING                                          
      WRITE (IWRITE,610) KSUSP                                          
      WRITE (IWRITE,810)                                                
      CALL LEAVE                                                        
      RETURN                                                            
C                                                                       
C     MOST FORMATS, ALSO SOME IN SGEXX                                  
C                                                                       
  450 FORMAT (1H )                                                      
  460 FORMAT (29H1  PORT  TESTER,  GE**,  BA**)                         
  470 FORMAT ( / 14H EPSILON     =, 1PE15.5)                            
  480 FORMAT ( / 19H EXACT SINGULARITY. /)                              
  490 FORMAT ( / 16H MAYBE SINGULAR. /)                                 
  510 FORMAT (14H ACTUAL COND =, 1PE15.5)                               
  520 FORMAT(/14H X AND XBAND =)                                        
  540 FORMAT (14H ERROR NORMS =, 2(1PE15.5))                            
  550 FORMAT (14H RESID NORMS =, 2(1PE15.5))                            
  560 FORMAT (26H TEST RATIOS.. E = EPSILON)                            
  570 FORMAT ( / 14H ************* /)                                   
  580 FORMAT (8H1SUMMARY)                                               
  590 FORMAT (18H NUMBER OF TESTS =, I4)                                
  600 FORMAT (30H NUMBER OF SINGULAR MATRICES =, I4)                    
  610 FORMAT (30H NUMBER OF SUSPICIOUS RATIOS =, 10I4)                  
  620 FORMAT(42H    COND  COND(B)  ACTUAL  ACTUAL  ERROR  ,             
     1       40HERROR(B)  RESID  A-LU  A*AI-I  A*AI-I(B))               
  630 FORMAT (10(8H   -----))                                           
  640 FORMAT(42H    ACTUAL ACTUAL   COND  COND(B) E*COND*X,             
     1       40H E*COND*X E*A*X    E*A   E*COND  E*COND )               
  650 FORMAT (14H NORM(A)     =, 1PE15.5)                               
  660 FORMAT (14H NORM(A - LU)=, 1PE15.5)                               
  670 FORMAT (14H NORM(A*AI-I)=, 2(1PE15.5))                            
  690 FORMAT (10(1X, F7.2))                                             
  700 FORMAT (1H , 6G11.4)                                              
  710 FORMAT (14H 1/COND      =, 1PE15.5)                               
  740 FORMAT (2G14.6)                                                   
  790 FORMAT (5H ML =, I2, 6H  MU =, I2)                                
  810 FORMAT ( / 12H END OF TEST)                                       
  820 FORMAT(7H COND =,1PE15.5,13H COND(BAND) =,1PE15.5 /)              
  860 FORMAT (21H NO SUSPICIOUS RATIOS)                                 
  870 FORMAT (I8, 9I10 / 7X, 28H1 INDICATES SUSPICIOUS RATIO)           
  880 FORMAT (29H THIS VERSION DATED 03/11/78.)                         
      END                                                               
      SUBROUTINE SGEXX(A,LDA,N,KASE,IWRITE)                             
C                                                                       
C     GENERATES REAL GENERAL TEST MATRICES                              
C                                                                       
C     FORTRAN FLOAT,MAX0                                                
      INTEGER LDA,N,KASE,IWRITE                                         
      INTEGER I,J                                                       
      REAL A(LDA,1)                                                     
      REAL T1,T2                                                        
      REAL HUGE,TINY                                                    
C                                                                       
      GO TO (10, 10, 10, 60, 60, 80, 80, 80, 120, 160, 200, 240, 280,   
     *       320, 360, 410, 460), KASE                                  
C                                                                       
C     KASE 1, 2 AND 3                                                   
C                                                                       
   10 CONTINUE                                                          
         N = 3*KASE                                                     
         WRITE (IWRITE,20) KASE,N                                       
   20    FORMAT (5H KASE, I3, 3X, 16HHILBERT SLICE    / 4H N =, I4)     
         DO 50 J = 1, N                                                 
            DO 40 I = 1, N                                              
               A(I,J) = 0.0E0                                           
               IF (I .GT. J + 2) GO TO 30                               
               IF (I .LT. J - 3) GO TO 30                               
                  A(I,J) = 1.0E0/FLOAT(I+J-1)                           
   30          CONTINUE                                                 
   40       CONTINUE                                                    
   50    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 4 AND 5                                                      
C                                                                       
   60 CONTINUE                                                          
         N = 1                                                          
         WRITE (IWRITE,70) KASE,N                                       
   70    FORMAT (5H KASE, I3, 3X, 16HMONOELEMENTAL    / 4H N =, I4)     
         IF (KASE .EQ. 4) A(1,1) = 3.0E0                                
         IF (KASE .EQ. 5) A(1,1) = 0.0E0                                
      GO TO 470                                                         
C                                                                       
C     KASE 6, 7 AND 8                                                   
C                                                                       
   80 CONTINUE                                                          
         N = 15                                                         
         WRITE (IWRITE,90) KASE,N                                       
   90    FORMAT (5H KASE, I3, 3X, 16HTRIDIAGONAL      / 4H N =, I4)     
         T1 = 1.0E0                                                     
         T2 = 1.0E0                                                     
         IF (KASE .EQ. 7) T1 = 100.0E0                                  
         IF (KASE .EQ. 8) T2 = 100.0E0                                  
         DO 110 I = 1, N                                                
            DO 100 J = 1, N                                             
               A(I,J) = 0.0E0                                           
               IF (I .EQ. J) A(I,I) = 4.0E0                             
               IF (I .EQ. J - 1) A(I,J) = T1                            
               IF (I .EQ. J + 1) A(I,J) = T2                            
  100       CONTINUE                                                    
  110    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 9                                                            
C                                                                       
  120 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,130) KASE,N                                      
  130    FORMAT (5H KASE, I3, 3X, 16HRANK ONE         / 4H N =, I4)     
         DO 150 I = 1, N                                                
            DO 140 J = 1, N                                             
               A(I,J) = 10.0E0**(I - J)                                 
  140       CONTINUE                                                    
  150    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 10                                                           
C                                                                       
  160 CONTINUE                                                          
         N = 4                                                          
         WRITE (IWRITE,170) KASE,N                                      
  170    FORMAT (5H KASE, I3, 3X, 16HZERO COLUMN      / 4H N =, I4)     
         DO 190 I = 1, N                                                
            DO 180 J = 1, N                                             
               T1 = FLOAT(J-3)                                          
               T2 = FLOAT(I)                                            
               A(I,J) = T1/T2                                           
  180       CONTINUE                                                    
  190    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 11                                                           
C                                                                       
  200 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,210) KASE,N                                      
  210    FORMAT (5H KASE, I3, 3X, 16HTEST COND        / 4H N =, I4)     
         DO 230 I = 1, N                                                
            DO 220 J = 1, N                                             
               IF (I .EQ. J) A(I,J) = FLOAT(I)                          
               IF (I .GT. J) A(I,J) = FLOAT(J-2)                        
               IF (I .LT. J) A(I,J) = FLOAT(I-2)                        
  220       CONTINUE                                                    
  230    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 12                                                           
C                                                                       
  240 CONTINUE                                                          
         N = 3                                                          
         WRITE (IWRITE,250) KASE,N                                      
  250    FORMAT (5H KASE, I3, 3X, 16HIDENTITY         / 4H N =, I4)     
         DO 270 I = 1, N                                                
            DO 260 J = 1, N                                             
               IF (I .EQ. J) A(I,I) = 1.0E0                             
               IF (I .NE. J) A(I,J) = 0.0E0                             
  260       CONTINUE                                                    
  270    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 13                                                           
C                                                                       
  280 CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,290) KASE,N                                      
  290    FORMAT (5H KASE, I3, 3X, 16HUPPER TRIANGULAR / 4H N =, I4)     
         DO 310 I = 1, N                                                
            DO 300 J = 1, N                                             
               IF (I .GT. J) A(I,J) = 0.0E0                             
               IF (I .LE. J) A(I,J) = FLOAT(J-I+1)                      
  300       CONTINUE                                                    
  310    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 14                                                           
C                                                                       
  320 CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,330) KASE,N                                      
  330    FORMAT (5H KASE, I3, 3X, 16HLOWER TRIANGULAR / 4H N =, I4)     
         DO 350 I = 1, N                                                
            DO 340 J = 1, N                                             
               IF (I .LT. J) A(I,J) = 0.0E0                             
               IF (I .GE. J) A(I,J) = FLOAT(I-J+1)                      
  340       CONTINUE                                                    
  350    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 15                                                           
C                                                                       
  360 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,370) KASE,N                                      
  370    FORMAT (5H KASE, I3, 3X, 16HNEAR UNDERFLOW   / 4H N =, I4)     
         TINY =R1MACH(1)*FLOAT(N*N)*200.0                               
         WRITE (IWRITE,380) TINY                                        
  380    FORMAT (14H TINY        =, 1PE15.5)                            
         DO 400 I = 1, N                                                
            DO 390 J = 1, N                                             
               A(I,J) = TINY*FLOAT(J)/FLOAT(MAX0(I,J))                  
  390       CONTINUE                                                    
  400    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 16                                                           
C                                                                       
  410 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,420) KASE,N                                      
  420    FORMAT (5H KASE, I3, 3X, 16HNEAR OVERFLOW    / 4H N =, I4)     
         HUGE =R1MACH(2)/FLOAT(N*N)                                     
         WRITE (IWRITE,430) HUGE                                        
  430    FORMAT (14H HUGE        =, 1PE15.5)                            
         DO 450 I = 1, N                                                
            DO 440 J = 1, N                                             
               A(I,J) = HUGE*(FLOAT(J)/FLOAT(MAX0(I,J)))                
  440       CONTINUE                                                    
  450    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
  460 CONTINUE                                                          
         N = 0                                                          
  470 CONTINUE                                                          
      RETURN                                                            
C                                                                       
      END                                                               
C$TEST LGAD                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAMS DGECE AND FRIENDS                          
C                                                                       
C***********************************************************************
C     MAIN PROGRAM                                                      
      INTEGER IWRITE,I1MACH                                             
C     ALLOW 5000 UNDERFLOWS.                                            
C                                                                       
C     SET OUTPUT UNIT NUMBER                                            
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      CALL SGETS(IWRITE)                                                
      STOP                                                              
      END                                                               
      SUBROUTINE SGETS(IWRITE)                                          
C     IWRITE IS THE OUTPUT UNIT NUMBER                                  
C                                                                       
C     TESTS                                                             
C        DGECE,DGEFS,DGEBS,DGEML,,DBACE,,DBABS,,DBAFS,,DBALE,,DBAML     
C                                                                       
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     PORT DGECE,DGEFS,DGEBS,DGEML,DBACE,DBAFS,DBABS,DBAML,DBALE        
C     PORT UTILITIES ERROFF,ENTER,LEAVE,D1MACH                          
C     EXTERNAL SGEXX                                                    
C     BLAS DAXPY,DDOT,DSCAL,DASUM                                       
C     FORTRAN ABS,DMAX1,FLOAT,MAX0,MIN0                                 
C                                                                       
C     INTERNAL VARIABLES                                                
C                                                                       
      INTEGER I,IPVT(15),IPVTB(15),IQ(8),I1,I2,J                        
      INTEGER K,KASE,KB,KBFAIL,KOUNT,KP1,KSING,KSUSP(8)                 
      INTEGER L,LDA,LDAB,IWRITE,M,ML,MU,N,NM1,NPRINT                    
      REAL    Q(10),QS(10)                                              
      DOUBLE PRECISION A(15,15),AB(43,15),AINV(15,15),ASAVE(15,15)      
      DOUBLE PRECISION D1MACH,AIBNO,EBNORM                              
      DOUBLE PRECISION B(15),BT(15),DDOT,AL(43,15)                      
      DOUBLE PRECISION X(15),XB(15),XEXACT(15),XT(15),XTB(15),T         
      DOUBLE PRECISION AINORM,ANORM,SMACH,COND,COND1,EN,ENORM,EPS       
      DOUBLE PRECISION ETNORM,FNI,FNORM,ONEPX,RCOND,RCONDB,RNORM        
      DOUBLE PRECISION RTNORM,DASUM,XNORM                               
      DOUBLE PRECISION ABSAVE(43,15),AINVB(15,15),DENOM                 
      LOGICAL KBF                                                       
C                                                                       
      LDA = 15                                                          
      CALL ENTER(1)                                                     
      LDAB = 43                                                         
C                                                                       
C     WRITE MATRIX AND SOLUTIONS IF  N .LE. NPRINT                      
C                                                                       
      NPRINT =3                                                         
C                                                                       
      WRITE (IWRITE,460)                                                
      WRITE (IWRITE,880)                                                
C                                                                       
      DO 10 I = 1, 8                                                    
         KSUSP(I) = 0                                                   
   10 CONTINUE                                                          
      KSING = 0                                                         
      KBFAIL = 0                                                        
C                                                                       
C     SET EPS TO ROUNDING UNIT                                          
C                                                                       
      EPS =D1MACH(4)                                                    
      WRITE (IWRITE,470) EPS                                            
      WRITE (IWRITE,450)                                                
C                                                                       
C     START MAIN LOOP                                                   
C                                                                       
      KASE = 1                                                          
   20 CONTINUE                                                          
C                                                                       
C        GENERATE TEST MATRIX                                           
C                                                                       
         CALL SGEXX(A,LDA,N,KASE,IWRITE)                                
C                                                                       
C        N = 0 SIGNALS NO MORE TEST MATRICES                            
C                                                                       
C     ...EXIT                                                           
         IF (N .LE. 0) GO TO 440                                        
         ANORM = 0.0D0                                                  
         DO 30 J = 1, N                                                 
            ANORM = DMAX1(ANORM,DASUM(N,A(1,J),1))                      
   30    CONTINUE                                                       
         WRITE (IWRITE,650) ANORM                                       
C                                                                       
         IF (N .GT. NPRINT) GO TO 50                                    
            WRITE (IWRITE,450)                                          
            DO 40 I = 1, N                                              
               WRITE (IWRITE,700) (A(I,J), J = 1, N)                    
   40       CONTINUE                                                    
            WRITE (IWRITE,450)                                          
   50    CONTINUE                                                       
C                                                                       
C        GENERATE EXACT SOLUTION                                        
C                                                                       
         XEXACT(1) = 1.0D0                                              
         IF (N .GE. 2) XEXACT(2) = 0.0D0                                
         IF (N .LE. 2) GO TO 70                                         
            DO 60 I = 3, N                                              
               XEXACT(I) = -XEXACT(I-2)                                 
   60       CONTINUE                                                    
   70    CONTINUE                                                       
C                                                                       
C        SAVE MATRIX AND GENERATE R.H.S.                                
C                                                                       
         DO 90 I = 1, N                                                 
            DO 80 J = 1, N                                              
               ASAVE(I,J) = A(I,J)                                      
   80       CONTINUE                                                    
   90    CONTINUE                                                       
         CALL DGEML(N,A,LDA,XEXACT,B)                                   
         CALL MOVEFD(N,B,X)                                             
C                                                                       
C        FACTOR AND ESTIMATE CONDITION                                  
C                                                                       
         CALL DGECE(N,A,LDA,IPVT,RCOND)                                 
         IF (NERROR(INE).NE.0) CALL  ERROFF                             
C                                                                       
C                                                                       
C        FACTOR BAND FORM AND COMPARE                                   
C                                                                       
         KBF = .FALSE.                                                  
         ML = 0                                                         
         MU = 0                                                         
         DO 140 J = 1, N                                                
            DO 130 I = 1, N                                             
               IF (ASAVE(I,J) .EQ. 0.0D0) GO TO 120                     
                  IF (I .LT. J) MU = MAX0(MU,J-I)                       
                  IF (I .GT. J) ML = MAX0(ML,I-J)                       
  120          CONTINUE                                                 
  130       CONTINUE                                                    
  140    CONTINUE                                                       
         WRITE (IWRITE,790) ML,MU                                       
            M = ML + MU + 1                                             
            DO 170 J = 1, N                                             
               I1 = MAX0(1,J-MU)                                        
               I2 = MIN0(N,J+ML)                                        
               DO 160 I = I1, I2                                        
                  K=ML+1+J-I                                            
                  AB(K,I) = ASAVE(I,J)                                  
                  ABSAVE(K,I)=ASAVE(I,J)                                
  160          CONTINUE                                                 
  170       CONTINUE                                                    
C                                                                       
            CALL DBACE(N,ML+1,M,AB,LDAB,AL,LDAB,IPVTB,MU,RCONDB)        
          WRITE(IWRITE,820)RCOND,RCONDB                                 
C                                                                       
C                                                                       
C           TEST FOR SINGULARITY                                        
C                                                                       
           IF (INE+NERROR(IRE).EQ.0) GO TO 210                          
             IF (IRE.NE.0) CALL ERROFF                                  
             IF (IRE.NE.INE) WRITE(IWRITE,118)                          
 118         FORMAT(35H BAND AND GENERAL ROUTINES DISAGREE)             
               WRITE (IWRITE,480)                                       
               KSING = KSING + 1                                        
            GO TO 420                                                   
  210       CONTINUE                                                    
C                                                                       
C              COMPUTE INVERSE AND COND1 = TRUE CONDITION               
C                                                                       
               DO 230 J = 1, N                                          
                  DO 220 I = 1, N                                       
                     AINV(I,J) = 0.D0                                   
                     AINVB(I,J)=0.D0                                    
  220             CONTINUE                                              
               AINV(J,J)=1.D0                                           
               AINVB(J,J)=1.D0                                          
  230          CONTINUE                                                 
               CALL DGEFS(N,A,LDA,AINV,LDA,N,IPVT)                      
               CALL DGEBS(N,A,LDA,AINV,LDA,N)                           
               CALL DBAFS(N,ML+1,AL,LDAB,IPVTB,AINVB,LDA,N)             
               CALL DBABS(N,AB,LDAB,AINVB,LDA,N,MU)                     
               AINORM = 0.0D0                                           
               DO 240 J = 1, N                                          
                  AINORM = DMAX1(AINORM,DASUM(N,AINV(1,J),1))           
  240          CONTINUE                                                 
               COND1 = ANORM*AINORM                                     
               WRITE (IWRITE,510) COND1                                 
C                                                                       
C              SOLVE  A*X = B                                           
C                                                                       
               CALL DGEFS(N,A,LDA,X,N,1,IPVT)                           
               CALL DGEBS(N,A,LDA,X,N,1)                                
C                                                                       
C              MORE BAND COMPARE                                        
C                                                                       
C              TEST CONSISTENCY OF DBAML AND DBALE                      
C                                                                       
               CALL DBAML(N,ML+1,M,ABSAVE,LDAB,XEXACT,XB)               
               CALL DBALE(N,ML+1,M,ABSAVE,LDAB,XB,N,1)                  
               IF (NERROR(IRE).EQ.0) GO TO 245                          
                 WRITE(IWRITE,490)                                      
                 CALL ERROFF                                            
                 GO TO 420                                              
 245           CONTINUE                                                 
               IF (N .GT. NPRINT) GO TO 270                             
                  WRITE (IWRITE,520)                                    
                  DO 250 I = 1, N                                       
                     WRITE (IWRITE,740) X(I),XB(I)                      
  250             CONTINUE                                              
                  WRITE (IWRITE,450)                                    
  270          CONTINUE                                                 
C                                                                       
C              RECONSTRUCT  A  FROM TRIANGULAR FACTORS , L AND U        
C                                                                       
               NM1 = N - 1                                              
               IF (NM1 .LT. 1) GO TO 330                                
               DO 320 KB = 1, NM1                                       
                  K = N - KB                                            
                  KP1 = K + 1                                           
                  L = IPVT(K)                                           
                  DO 310 J = KP1, N                                     
                     T = -A(K,J)                                        
                     CALL DAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)            
                     T = A(L,J)                                         
                     A(L,J) = A(K,J)                                    
                     A(K,J) = T                                         
  310             CONTINUE                                              
                  T = -A(K,K)                                           
                  CALL DSCAL(N-K,T,A(K+1,K),1)                          
                  T = A(L,K)                                            
                  A(L,K) = A(K,K)                                       
                  A(K,K) = T                                            
  320          CONTINUE                                                 
  330          CONTINUE                                                 
C                                                                       
C              COMPUTE ERRORS AND RESIDUALS                             
C                 E  =  X - XEXACT                                      
C                 EB =  XB - XEXACT                                     
C                 R  =  B - A*X                                         
C                 F  =  A - L*U                                         
C                 AI =  A*INV(A) - I                                    
C                 AIB = A(BAND)*INV(A(BAND)) - I                        
C                                                                       
               XNORM = DASUM(N,X,1)                                     
               ENORM = 0.0D0                                            
               EBNORM=0.D0                                              
               FNORM = 0.0D0                                            
               DO 350 J = 1, N                                          
                  ENORM = ENORM + DABS(X(J)-XEXACT(J))                  
                  EBNORM = EBNORM + DABS(XB(J) - XEXACT(J))             
                  T = -X(J)                                             
                  CALL DAXPY(N,T,ASAVE(1,J),1,B,1)                      
                  FNI = 0.0D0                                           
                  DO 340 I = 1, N                                       
                     FNI = FNI + DABS(ASAVE(I,J)-A(I,J))                
  340             CONTINUE                                              
                  IF (FNI .GT. FNORM) FNORM = FNI                       
  350          CONTINUE                                                 
               RNORM = DASUM(N,B,1)                                     
C                                                                       
C              A*INV(A) - I                                             
C                                                                       
               AINORM = 0.0D0                                           
               AIBNO=0.0D0                                              
               DO 380 J = 1, N                                          
                  DO 360 I = 1, N                                       
                     B(I) = 0.0D0                                       
                     XB(I) = 0.D0                                       
  360             CONTINUE                                              
                  DO 370 K = 1, N                                       
                     T = AINV(K,J)                                      
                     CALL DAXPY(N,T,ASAVE(1,K),1,B,1)                   
                     T=AINVB(K,J)                                       
                     CALL DAXPY(N,T,ASAVE(1,K),1,XB,1)                  
  370             CONTINUE                                              
                  B(J) = B(J) - 1.0D0                                   
                  XB(J) = XB(J) -1.0D0                                  
                  AIBNO=DMAX1(AIBNO,DASUM(N,XB,1))                      
                  AINORM = DMAX1(AINORM,DASUM(N,B,1))                   
  380          CONTINUE                                                 
C                                                                       
               WRITE (IWRITE,540) ENORM,EBNORM                          
               WRITE (IWRITE,550) RNORM                                 
               WRITE (IWRITE,660) FNORM                                 
               WRITE (IWRITE,670) AINORM,AIBNO                          
C                                                                       
C              COMPUTE TEST RATIOS                                      
C                                                                       
               Q(1) = RCOND/COND1                                       
               Q(2) = RCONDB/COND1                                      
               Q(3) = COND1/RCOND                                       
               Q(4)=COND1/RCONDB                                        
               Q(5) = ENORM/(EPS*RCOND*XNORM)                           
               Q(6) = EBNORM/(EPS*RCOND*XNORM)                          
               DENOM=DMAX1(1.0D2*D1MACH(1),EPS*ANORM*XNORM)             
               Q(7) = RNORM/DENOM                                       
               DENOM=DMAX1(1.0D2*D1MACH(1),EPS*ANORM)                   
               Q(8) = FNORM/DENOM                                       
               Q(9) = AINORM/(EPS*RCOND)                                
               Q(10)=AIBNO/(EPS*RCOND)                                  
               WRITE (IWRITE,450)                                       
               WRITE (IWRITE,560)                                       
               WRITE (IWRITE,450)                                       
               WRITE (IWRITE,620)                                       
               WRITE (IWRITE,630)                                       
               WRITE (IWRITE,640)                                       
               WRITE (IWRITE,450)                                       
               WRITE (IWRITE,690) (Q(I), I = 1, 10)                     
               WRITE (IWRITE,450)                                       
C                                                                       
C              LOOK FOR SUSPICIOUS RATIOS                               
C                                                                       
               QS(1) = 1.0D0 + 4.0D0*EPS                                
               QS(2)=QS(1)                                              
               QS(4) = 10.0D0                                           
               QS(3)=10.0D0                                             
               EN = DBLE(FLOAT(N))                                      
               IF (N .EQ. 1) EN = 2.0D0                                 
               DO 390 I = 3, 10                                         
                  QS(I) = EN                                            
  390          CONTINUE                                                 
               KOUNT = 0                                                
               DO 410 I = 1, 10                                         
                  IQ(I) = 0                                             
                  IF (Q(I) .LE. QS(I)) GO TO 400                        
                     IQ(I) = 1                                          
                     KSUSP(I) = KSUSP(I) + 1                            
                     KOUNT = KOUNT + 1                                  
  400             CONTINUE                                              
  410          CONTINUE                                                 
               IF (KOUNT .EQ. 0) WRITE (IWRITE,860)                     
               IF (KOUNT .NE. 0) WRITE (IWRITE,870) (IQ(I), I = 1, 10)  
               WRITE (IWRITE,450)                                       
  420       CONTINUE                                                    
  430    CONTINUE                                                       
C                                                                       
         WRITE (IWRITE,570)                                             
         KASE = KASE + 1                                                
      GO TO 20                                                          
  440 CONTINUE                                                          
C                                                                       
C     FINISH MAIN LOOP                                                  
C                                                                       
C     SUMMARY                                                           
C                                                                       
      WRITE (IWRITE,580)                                                
      KASE = KASE - 1                                                   
      WRITE (IWRITE,590) KASE                                           
      WRITE (IWRITE,600) KSING                                          
      WRITE (IWRITE,610) KSUSP                                          
      WRITE (IWRITE,810)                                                
      CALL LEAVE                                                        
      RETURN                                                            
C                                                                       
C     MOST FORMATS, ALSO SOME IN SGEXX                                  
C                                                                       
  450 FORMAT (1H )                                                      
  460 FORMAT (29H1  PORT  TESTER, DGE**, DBA**)                         
  470 FORMAT ( / 14H EPSILON     =, 1PD15.5)                            
  480 FORMAT ( / 19H EXACT SINGULARITY. /)                              
  490 FORMAT ( / 16H MAYBE SINGULAR. /)                                 
  510 FORMAT (14H ACTUAL COND =, 1PD15.5)                               
  520 FORMAT(/14H X AND XBAND =)                                        
  540 FORMAT (14H ERROR NORMS =, 2(1PD15.5))                            
  550 FORMAT (14H RESID NORMS =, 2(1PD15.5))                            
  560 FORMAT (26H TEST RATIOS.. E = EPSILON)                            
  570 FORMAT ( / 14H ************* /)                                   
  580 FORMAT (8H1SUMMARY)                                               
  590 FORMAT (18H NUMBER OF TESTS =, I4)                                
  600 FORMAT (30H NUMBER OF SINGULAR MATRICES =, I4)                    
  610 FORMAT (30H NUMBER OF SUSPICIOUS RATIOS =, 10I4)                  
  620 FORMAT(42H    COND  COND(B)  ACTUAL  ACTUAL  ERROR  ,             
     1       40HERROR(B)  RESID  A-LU  A*AI-I  A*AI-I(B))               
  630 FORMAT (10(8H   -----))                                           
  640 FORMAT(42H    ACTUAL ACTUAL   COND  COND(B) E*COND*X,             
     1       40H E*COND*X E*A*X    E*A   E*COND  E*COND )               
  650 FORMAT (14H NORM(A)     =, 1PD15.5)                               
  660 FORMAT (14H NORM(A - LU)=, 1PD15.5)                               
  670 FORMAT (14H NORM(A*AI-I)=, 2(1PD15.5))                            
  690 FORMAT (10(1X, F7.2))                                             
  700 FORMAT (1H , 6D15.4)                                              
  710 FORMAT (14H 1/COND      =, 1PD15.5)                               
  740 FORMAT (2D18.6)                                                   
  790 FORMAT (5H ML =, I2, 6H  MU =, I2)                                
  810 FORMAT ( / 12H END OF TEST)                                       
  820 FORMAT(7H COND =,1PD15.5,13H COND(BAND) =,1PD15.5 /)              
  860 FORMAT (21H NO SUSPICIOUS RATIOS)                                 
  870 FORMAT (I8, 9I10 / 7X, 28H1 INDICATES SUSPICIOUS RATIO)           
  880 FORMAT (29H THIS VERSION DATED 03/11/78.)                         
      END                                                               
      SUBROUTINE SGEXX(A,LDA,N,KASE,IWRITE)                             
C                                                                       
C     GENERATES DOUBLE PRECISION GENERAL TEST MATRICES                  
C                                                                       
C     EXTERNAL SMACH                                                    
C     FORTRAN FLOAT,MAX0                                                
      INTEGER LDA,N,KASE,IWRITE                                         
      INTEGER I,J                                                       
      DOUBLE PRECISION A(LDA,1)                                         
      DOUBLE PRECISION T1,T2                                            
      DOUBLE PRECISION D1MACH,HUGE,TINY                                 
C                                                                       
      GO TO (10, 10, 10, 60, 60, 80, 80, 80, 120, 160, 200, 240, 280,   
     *       320, 360, 410, 460), KASE                                  
C                                                                       
C     KASE 1, 2 AND 3                                                   
C                                                                       
   10 CONTINUE                                                          
         N = 3*KASE                                                     
         WRITE (IWRITE,20) KASE,N                                       
   20    FORMAT (5H KASE, I3, 3X, 16HHILBERT SLICE    / 4H N =, I4)     
         DO 50 J = 1, N                                                 
            DO 40 I = 1, N                                              
               A(I,J) = 0.0D0                                           
               IF (I .GT. J + 2) GO TO 30                               
               IF (I .LT. J - 3) GO TO 30                               
                  A(I,J) = 1.0D0/DBLE(FLOAT(I+J-1))                     
   30          CONTINUE                                                 
   40       CONTINUE                                                    
   50    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 4 AND 5                                                      
C                                                                       
   60 CONTINUE                                                          
         N = 1                                                          
         WRITE (IWRITE,70) KASE,N                                       
   70    FORMAT (5H KASE, I3, 3X, 16HMONOELEMENTAL    / 4H N =, I4)     
         IF (KASE .EQ. 4) A(1,1) = 3.0D0                                
         IF (KASE .EQ. 5) A(1,1) = 0.0D0                                
      GO TO 470                                                         
C                                                                       
C     KASE 6, 7 AND 8                                                   
C                                                                       
   80 CONTINUE                                                          
         N = 15                                                         
         WRITE (IWRITE,90) KASE,N                                       
   90    FORMAT (5H KASE, I3, 3X, 16HTRIDIAGONAL      / 4H N =, I4)     
         T1 = 1.0D0                                                     
         T2 = 1.0D0                                                     
         IF (KASE .EQ. 7) T1 = 100.0D0                                  
         IF (KASE .EQ. 8) T2 = 100.0D0                                  
         DO 110 I = 1, N                                                
            DO 100 J = 1, N                                             
               A(I,J) = 0.0D0                                           
               IF (I .EQ. J) A(I,I) = 4.0D0                             
               IF (I .EQ. J - 1) A(I,J) = T1                            
               IF (I .EQ. J + 1) A(I,J) = T2                            
  100       CONTINUE                                                    
  110    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 9                                                            
C                                                                       
  120 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,130) KASE,N                                      
  130    FORMAT (5H KASE, I3, 3X, 16HRANK ONE         / 4H N =, I4)     
         DO 150 I = 1, N                                                
            DO 140 J = 1, N                                             
               A(I,J) = 10.0D0**(I - J)                                 
  140       CONTINUE                                                    
  150    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 10                                                           
C                                                                       
  160 CONTINUE                                                          
         N = 4                                                          
         WRITE (IWRITE,170) KASE,N                                      
  170    FORMAT (5H KASE, I3, 3X, 16HZERO COLUMN      / 4H N =, I4)     
         DO 190 I = 1, N                                                
            DO 180 J = 1, N                                             
               T1 = DBLE(FLOAT(J-3))                                    
               T2 = DBLE(FLOAT(I))                                      
               A(I,J) = T1/T2                                           
  180       CONTINUE                                                    
  190    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 11                                                           
C                                                                       
  200 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,210) KASE,N                                      
  210    FORMAT (5H KASE, I3, 3X, 16HTEST COND        / 4H N =, I4)     
         DO 230 I = 1, N                                                
            DO 220 J = 1, N                                             
               IF (I .EQ. J) A(I,J) = DBLE(FLOAT(I))                    
               IF (I .GT. J) A(I,J) = DBLE(FLOAT(J-2))                  
               IF (I .LT. J) A(I,J) = DBLE(FLOAT(I-2))                  
  220       CONTINUE                                                    
  230    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 12                                                           
C                                                                       
  240 CONTINUE                                                          
         N = 3                                                          
         WRITE (IWRITE,250) KASE,N                                      
  250    FORMAT (5H KASE, I3, 3X, 16HIDENTITY         / 4H N =, I4)     
         DO 270 I = 1, N                                                
            DO 260 J = 1, N                                             
               IF (I .EQ. J) A(I,I) = 1.0D0                             
               IF (I .NE. J) A(I,J) = 0.0D0                             
  260       CONTINUE                                                    
  270    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 13                                                           
C                                                                       
  280 CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,290) KASE,N                                      
  290    FORMAT (5H KASE, I3, 3X, 16HUPPER TRIANGULAR / 4H N =, I4)     
         DO 310 I = 1, N                                                
            DO 300 J = 1, N                                             
               IF (I .GT. J) A(I,J) = 0.0D0                             
               IF (I .LE. J) A(I,J) = DBLE(FLOAT(J-I+1))                
  300       CONTINUE                                                    
  310    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 14                                                           
C                                                                       
  320 CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,330) KASE,N                                      
  330    FORMAT (5H KASE, I3, 3X, 16HLOWER TRIANGULAR / 4H N =, I4)     
         DO 350 I = 1, N                                                
            DO 340 J = 1, N                                             
               IF (I .LT. J) A(I,J) = 0.0D0                             
               IF (I .GE. J) A(I,J) = DBLE(FLOAT(I-J+1))                
  340       CONTINUE                                                    
  350    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 15                                                           
C                                                                       
  360 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,370) KASE,N                                      
  370    FORMAT (5H KASE, I3, 3X, 16HNEAR UNDERFLOW   / 4H N =, I4)     
         TINY =D1MACH(1)*DBLE(FLOAT(N*N))*200.D0                        
         WRITE (IWRITE,380) TINY                                        
  380    FORMAT (14H TINY        =, 1PD15.5)                            
         DO 400 I = 1, N                                                
            DO 390 J = 1, N                                             
               A(I,J) = TINY*DBLE(FLOAT(J))/DBLE(FLOAT(MAX0(I,J)))      
  390       CONTINUE                                                    
  400    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 16                                                           
C                                                                       
  410 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,420) KASE,N                                      
  420    FORMAT (5H KASE, I3, 3X, 16HNEAR OVERFLOW    / 4H N =, I4)     
         HUGE =D1MACH(2)/DBLE(FLOAT(N*N))                               
         WRITE (IWRITE,430) HUGE                                        
  430    FORMAT (14H HUGE        =, 1PD15.5)                            
         DO 450 I = 1, N                                                
            DO 440 J = 1, N                                             
               A(I,J) = HUGE*(DBLE(FLOAT(J))/DBLE(FLOAT(MAX0(I,J))))    
  440       CONTINUE                                                    
  450    CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
  460 CONTINUE                                                          
         N = 0                                                          
  470 CONTINUE                                                          
      RETURN                                                            
C                                                                       
      END                                                               
C$TEST LYMA                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAMS SYCE AND FRIENDS                           
C                                                                       
C***********************************************************************
C     MAIN PROGRAM                                                      
      INTEGER IWRITE,I1MACH                                             
C     ALLOW 5000 UNDERFLOWS.                                            
C                                                                       
C     OUTPUT UNIT NUMBER                                                
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      CALL SPOTS(IWRITE)                                                
      STOP                                                              
      END                                                               
      SUBROUTINE SPOTS(IWRITE)                                          
C     IWRITE IS THE OUTPUT UNIT NUMBER                                  
C                                                                       
C     TESTS                                                             
C        SYCE;SYFBS;SYML;SYLE;BPCE;BPFS;BPBS;BPML;BPLE                  
C                                                                       
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     PORT SYCE,SYFBS,SYML,SYLE,BPCE,BPFS,BPBS,BPML,BPLE                
C     EXTERNAL SPOXX,R1MACH                                             
C     BLAS SAXPY,SDOT,SASUM                                             
C     FORTRAN ABS,AMAX1,FLOAT,MAX0                                      
C                                                                       
C     INTERNAL VARIABLES                                                
C                                                                       
      INTEGER IPVT(15),IPVTS(15)                                        
      INTEGER I,IQ(10),I1,J,JB,INDEX                                    
      INTEGER K,KASE,KB,KBFAIL,KNPD,KOUNT,KPFAIL                        
      INTEGER KSUSP(10),LDA,IWRITE,M,N,NPRINT                           
      REAL APSAVE(120),AB(15,15),AINV(15,15),ASAVE(15,15)               
      REAL AP(120),B(15),SDOT,X(15),XB(15),XEXACT(15)                   
      REAL ABSAVE(15,15)                                                
      REAL XP(15),T,Z(15)                                               
      REAL ANORM,AINORM,COND,COND1,AIBNO,EBNORM                         
      REAL EN,ENORM,EPS,FNORM,Q(10),QS(10),RCOND,RCONDB                 
      REAL RCONDP,RNORM,SASUM,R1MACH,XNORM                              
      REAL AINVB(15,15),AIS,SC                                          
      LOGICAL KBF,KPF                                                   
C                                                                       
      LDA = 15                                                          
C                                                                       
C     WRITE MATRIX AND SOLUTIONS IF  N .LE. NPRINT                      
C                                                                       
      NPRINT = 3                                                        
C                                                                       
      WRITE (IWRITE,560)                                                
      WRITE (IWRITE,1000)                                               
C                                                                       
      DO 10 I = 1,10                                                    
         KSUSP(I) = 0                                                   
   10 CONTINUE                                                          
      KNPD = 0                                                          
      KPFAIL = 0                                                        
      KBFAIL = 0                                                        
C                                                                       
C     SET EPS TO ROUNDING UNIT FOR REAL ARITHMETIC                      
C                                                                       
      EPS = R1MACH(4)                                                   
      WRITE (IWRITE,570) EPS                                            
      WRITE (IWRITE,550)                                                
C                                                                       
        CALL ENTER(1)                                                   
C     START MAIN LOOP                                                   
C                                                                       
      KASE = 1                                                          
   20 CONTINUE                                                          
C                                                                       
C        GENERATE TEST MATRIX                                           
C                                                                       
         CALL SPOXX(ASAVE,LDA,N,KASE,IWRITE)                            
C                                                                       
C        N = 0 SIGNALS NO MORE TEST MATRICES                            
C                                                                       
C     ...EXIT                                                           
         IF (N .LE. 0) GO TO 540                                        
         ANORM = 0.0E0                                                  
         DO 30 J = 1, N                                                 
            ANORM = AMAX1(ANORM,SASUM(N,ASAVE(1,J),1))                  
   30    CONTINUE                                                       
         WRITE (IWRITE,720) ANORM                                       
C                                                                       
         IF (N .GT. NPRINT) GO TO 50                                    
            WRITE (IWRITE,550)                                          
            DO 40 I = 1, N                                              
               WRITE (IWRITE,760) (ASAVE(I,J), J = 1, N)                
   40       CONTINUE                                                    
            WRITE (IWRITE,550)                                          
   50    CONTINUE                                                       
C                                                                       
C        GENERATE EXACT SOLUTION                                        
C                                                                       
         XEXACT(1) = 1.0E0                                              
         IF (N .GE. 2) XEXACT(2) = 0.0E0                                
         IF (N .LE. 2) GO TO 70                                         
            DO 60 I = 3, N                                              
               XEXACT(I) = -XEXACT(I-2)                                 
   60       CONTINUE                                                    
   70    CONTINUE                                                       
C                                                                       
C                                                                       
C      PUT INTO PACKED FORM                                             
         K = 0                                                          
         DO 130 J = 1, N                                                
            DO 120 I = J,N                                              
               K = K + 1                                                
               AP(K) = ASAVE(I,J)                                       
               APSAVE(K)=AP(K)                                          
  120       CONTINUE                                                    
  130    CONTINUE                                                       
         CALL SYCE(N,AP,IPVTS,RCONDP)                                   
         IF (NERROR(IERR).NE.0) CALL ERROFF                             
C        FACTOR BAND FORM AND COMPARE                                   
C                                                                       
         KBF = .FALSE.                                                  
         M = 0                                                          
         DO 200 J = 1, N                                                
            DO 190 I = 1, J                                             
               IF (ASAVE(I,J) .NE. 0.0E0) M = MAX0(M,J-I)               
  190       CONTINUE                                                    
  200    CONTINUE                                                       
C                                                                       
         DO 220 J = 1, N                                                
             I1=MIN0(N,J+M)                                             
            DO 210 I = J, I1                                            
               K =I-J+1                                                 
               AB(K,J) = ASAVE(I,J)                                     
               ABSAVE(K,J)=AB(K,J)                                      
  210       CONTINUE                                                    
  220    CONTINUE                                                       
         WRITE (IWRITE,840) M                                           
        CALL BPCE(N,M+1,AB,LDA,RCONDB)                                  
           IF (NERROR(IERR).EQ.0) GO TO 230                             
          CALL ERROFF                                                   
          IF ((IERR).LT.10+N)GO TO 226                                  
             WRITE(IWRITE,860)IERR                                      
             INDEX = IERR - N - 10                                      
             WRITE(IWRITE,229)AB(1,INDEX)                               
229           FORMAT( 19H OFFENDING DIAGONAL,E17.7)                     
             GO TO 530                                                  
226          WRITE(IWRITE,580)                                          
            WRITE (IWRITE,930) RCONDP,RCONDB                            
             GO TO 530                                                  
230        CONTINUE                                                     
            WRITE(IWRITE,930)RCONDP,RCONDB                              
C                                                                       
C           COMPUTE INVERSE AND COND1 = TRUE CONDITION                  
C                                                                       
            DO 290 J = 1, N                                             
               DO 280 I = 1, N                                          
                   AINV(I,J)=0.0                                        
                   AINVB(I,J)=0.0                                       
  280          CONTINUE                                                 
               AINV(J,J)=1.0                                            
               AINVB(J,J)=1.0                                           
  290       CONTINUE                                                    
           CALL BPFS(N,M+1,AB,LDA,AINVB,LDA,N)                          
            CALL BPBS(N,M+1,AB,LDA,AINVB,LDA,N)                         
           CALL SYFBS(N,AP,AINV,LDA,N,IPVTS)                            
           AINORM=0.0                                                   
           DO 310 J=1,N                                                 
              AIS=SASUM(N,AINV(1,J),1)                                  
               AINORM = AMAX1(AINORM,AIS)                               
  310       CONTINUE                                                    
            COND1 = ANORM*AINORM                                        
            WRITE (IWRITE,600) COND1                                    
C                                                                       
C           GENERATE RIGHT HAND SIDE FOR BOTH SYMMETRIC AND BAND        
C                                                                       
            CALL SYML(N,APSAVE,XEXACT,B)                                
            CALL MOVEFR(N,B,X)                                          
            CALL BPML(N,M+1,ABSAVE,LDA,XEXACT,XB)                       
C           SOLVE A*X = B                                               
C                                                                       
            CALL SYLE(N,APSAVE,X,N,1)                                   
            IF (NERROR(IRE).NE.0) CALL ERROFF                           
            CALL BPLE(N,M+1,ABSAVE,LDA,XB,N,1)                          
            IF (IRE+NERROR(IRB).EQ.0) GO TO 311                         
               IF (IRB.NE.0) CALL ERROFF                                
               WRITE(IWRITE,580)                                        
               GO TO 530                                                
  311       CONTINUE                                                    
C                                                                       
            IF (N .GT. NPRINT) GO TO 330                                
               WRITE (IWRITE,610)                                       
               DO 320 I = 1, N                                          
                  WRITE (IWRITE,790) X(I), XB(I)                        
  320          CONTINUE                                                 
               WRITE (IWRITE,550)                                       
  330       CONTINUE                                                    
C                                                                       
C                                                                       
C           COMPUTE ERRORS AND RESIDUALS                                
C              E  =  X - XEXACT                                         
C              EB =  XB - XEXACT                                        
C              R  =  B - A*X                                            
C              F  =  A - TRANS(R)*R                                     
C                                                                       
            XNORM = SASUM(N,X,1)                                        
            ENORM = 0.0E0                                               
            EBNORM = 0.E0                                               
            DO 460 J = 1, N                                             
               ENORM = ENORM + ABS(X(J)-XEXACT(J))                      
               EBNORM = EBNORM + ABS(XB(J)-XEXACT(J))                   
               T = -X(J)                                                
               CALL SAXPY(N,T,ASAVE(1,J),1,B,1)                         
  460       CONTINUE                                                    
            RNORM = SASUM(N,B,1)                                        
C                                                                       
C           A*INV(A) - I                                                
C                                                                       
            AINORM = 0.0E0                                              
            AIBNO = 0.E0                                                
            DO 490 J = 1, N                                             
               DO 470 I = 1, N                                          
                  B(I) = 0.0E0                                          
                 XB(I) = 0.0E0                                          
  470          CONTINUE                                                 
               DO 480 K = 1, N                                          
                  T = AINV(K,J)                                         
                  CALL SAXPY(N,T,ASAVE(1,K),1,B,1)                      
                  T=AINVB(K,J)                                          
                  CALL SAXPY(N,T,ASAVE(1,K),1,XB,1)                     
  480          CONTINUE                                                 
               B(J) = B(J) - 1.0E0                                      
               XB(J) = XB(J) - 1.0E0                                    
               AINORM = AMAX1(AINORM,SASUM(N,B,1))                      
               AIBNO = AMAX1(AIBNO,SASUM(N,XB,1))                       
  490       CONTINUE                                                    
            FNORM = 0.0E0                                               
            ML=M+1                                                      
            NP1=N+1                                                     
            MLP1=ML+1                                                   
            DO 495 J=1,N                                                
               NUMAX= MIN0(ML,NP1-J)                                    
               JM1=J-1                                                  
               IEND=JM1+NUMAX                                           
               DO 491 I=J,IEND                                          
                  B(I)=-ASAVE(I,J)                                      
491            CONTINUE                                                 
               KBEGIN=MAX0(1,J-M)                                       
               L=MIN0(J,ML)                                             
               NUM=MLP1-L                                               
               IF (JM1.LT.KBEGIN) GO TO 493                             
               DO 492 K=KBEGIN,JM1                                      
                  SC=AB(L,K)*AB(1,K)                                    
                  CALL SAXPY(NUM,SC,AB(L,K),1,B(J),1)                   
                  L=L-1                                                 
                  NUM=MIN0(NUM+1,NUMAX)                                 
492            CONTINUE                                                 
493            SC=AB(1,J)                                               
               CALL SAXPY(NUM-1,SC,AB(2,J),1,B(J+1),1)                  
               B(J)=B(J)+AB(1,J)                                        
               FNORM=AMAX1(FNORM,SASUM(NUMAX,B(J),1))                   
495        CONTINUE                                                     
C                                                                       
            WRITE (IWRITE,620) ENORM, EBNORM                            
            WRITE (IWRITE,630) RNORM                                    
            WRITE (IWRITE,730) FNORM                                    
            WRITE (IWRITE,740) AINORM,AIBNO                             
C                                                                       
C           COMPUTE TEST RATIOS                                         
C                                                                       
            Q(1) = RCONDP/COND1                                         
            Q(2) = RCONDB/COND1                                         
            Q(3) = COND1/RCONDP                                         
            Q(4) = COND1/RCONDB                                         
            Q(5) = ENORM/(EPS*RCONDP*XNORM)                             
            Q(6) = EBNORM/(EPS*RCONDP*XNORM)                            
            DENOM=AMAX1(1.0E2*R1MACH(1),EPS*ANORM*XNORM)                
            Q(7)=RNORM/DENOM                                            
            DENOM=AMAX1(1.0E2*R1MACH(1),EPS*ANORM)                      
            Q(8)=FNORM/DENOM                                            
            Q(9) = AINORM/(EPS*RCONDP)                                  
            Q(10) = AIBNO/(EPS*RCONDP)                                  
            WRITE (IWRITE,550)                                          
            WRITE (IWRITE,640)                                          
            WRITE (IWRITE,550)                                          
            WRITE (IWRITE,690)                                          
            WRITE (IWRITE,700)                                          
            WRITE (IWRITE,710)                                          
            WRITE (IWRITE,550)                                          
            WRITE (IWRITE,750) (Q(I), I = 1, 10)                        
            WRITE (IWRITE,550)                                          
C                                                                       
C           LOOK FOR SUSPICIOUS RATIOS                                  
C                                                                       
            QS(1) = 1.0E0 + 4.0E0*EPS                                   
            QS(2) = QS(1)                                               
            QS(3) = 10.0E0                                              
            QS(4) =QS(3)                                                
            EN = FLOAT(N)                                               
            IF (N .EQ. 1) EN = 2.0E0                                    
            DO 500 I=5,10                                               
               QS(I) = EN                                               
  500       CONTINUE                                                    
            KOUNT = 0                                                   
            DO 520 I = 1, 10                                            
               IQ(I) = 0                                                
               IF (Q(I) .LE. QS(I)) GO TO 510                           
                  IQ(I) = 1                                             
                  KSUSP(I) = KSUSP(I) + 1                               
                  KOUNT = KOUNT + 1                                     
  510          CONTINUE                                                 
  520       CONTINUE                                                    
            IF (KOUNT .EQ. 0) WRITE (IWRITE,980)                        
            IF (KOUNT .NE. 0) WRITE (IWRITE,990) (IQ(I), I = 1,10)      
            WRITE (IWRITE,550)                                          
  530    CONTINUE                                                       
C                                                                       
         WRITE (IWRITE,650)                                             
         KASE = KASE + 1                                                
      GO TO 20                                                          
  540 CONTINUE                                                          
C                                                                       
C     FINISH MAIN LOOP                                                  
C                                                                       
C     SUMMARY                                                           
C                                                                       
      WRITE (IWRITE,660)                                                
      KASE = KASE - 1                                                   
      WRITE (IWRITE,670) KASE                                           
      WRITE (IWRITE,680) KSUSP                                          
      WRITE (IWRITE,910)                                                
      RETURN                                                            
C                                                                       
C     MOST FORMATS, ALSO SOME IN SPOXX                                  
C                                                                       
  550 FORMAT (1H )                                                      
 560  FORMAT(23H1PORT TESTER, SY** BP**)                                
  570 FORMAT ( / 14H EPSILON     =, 1PE15.5)                            
  580 FORMAT ( / 16H MAYBE SINGULAR. /)                                 
  600 FORMAT (14H ACTUAL COND =, 1PE15.5)                               
  610 FORMAT ( / 4H X =)                                                
  620 FORMAT (14H ERROR NORM  =, 2(1PE15.5))                            
  630 FORMAT (14H RESID NORM  =, 1P1E15.5)                              
  640 FORMAT (26H TEST RATIOS.. E = EPSILON)                            
  650 FORMAT ( / 14H ************* /)                                   
  660 FORMAT (8H1SUMMARY)                                               
  670 FORMAT (18H NUMBER OF TESTS =, I4)                                
  680 FORMAT ( / 30H NUMBER OF SUSPICIOUS RATIOS =, 10I4)               
  690 FORMAT( 42H    COND  COND(B)  ACTUAL  ACTUAL  ERROR  ,            
     1        40HERROR(B)  RESID  A-RT*R A*AI-I A*AI-I(B))              
  700 FORMAT (10(8H   -----))                                           
 710  FORMAT(42H    ACTUAL ACTUAL   COND  COND(B) E*COND*X,             
     1       40H E*COND*X E*A*X    E*A   E*COND  E*COND )               
  720 FORMAT (14H NORM(A)     =, 1PE15.5)                               
  730 FORMAT (14H NORM(A-RT*R)=, 1PE15.5)                               
  740 FORMAT (14H NORM(A*AI-I)=, 2(1PE15.5))                            
  750 FORMAT (10(1X, F7.2))                                             
  760 FORMAT(1H ,3E15.5)                                                
  780 FORMAT (2G14.6)                                                   
  790 FORMAT (2G14.6)                                                   
  830 FORMAT ( / 28H BAND ROUTINES DO NOT AGREE,)                       
  840 FORMAT (5H M  =, I2)                                              
  860 FORMAT (30H NOT POSITIVE DEFINITE, INFO =, I2)                    
  910 FORMAT ( / 12H END OF TEST)                                       
  930 FORMAT (8H RCOND =, 1P3E15.5)                                     
  980 FORMAT (21H NO SUSPICIOUS RATIOS)                                 
  990 FORMAT (I8, 5I10 / 7X, 28H1 INDICATES SUSPICIOUS RATIO)           
 1000 FORMAT (29H THIS VERSION DATED 09/21/78.)                         
      END                                                               
      SUBROUTINE SPOXX(A,LDA,N,KASE,IWRITE)                             
      INTEGER LDA,N,KASE,IWRITE                                         
      REAL A(LDA,1)                                                     
C                                                                       
C     GENERATES REAL POSITIVE DEFINITE TEST MATRICES                    
C                                                                       
C     EXTERNAL R1MACH                                                   
C     FORTRAN ABS,FLOAT,IABS,MAX0,MIN0                                  
      REAL T                                                            
      REAL TINY,HUGE,R1MACH                                             
      INTEGER I,J                                                       
C                                                                       
      GO TO (10, 10, 10, 50, 50, 70, 70, 70, 120, 160, 200, 240, 290,   
     *       340), KASE                                                 
C                                                                       
C     KASE 1, 2 AND 3                                                   
C                                                                       
   10 CONTINUE                                                          
         N = 5*KASE                                                     
         WRITE (IWRITE,20) KASE,N                                       
   20    FORMAT (5H KASE, I3, 3X, 16HHILBERT          / 4H N =, I4)     
         T = 1.0E0                                                      
         T = SIGN(1.0E0,T)                                              
         DO 40 J = 1, N                                                 
            DO 30 I = 1, N                                              
               A(I,J) = T**(I - J)/FLOAT(I+J-1)                         
C              FOR REAL MATRICES, A(I,J) = 1.0/FLOAT(I+J-1)             
   30       CONTINUE                                                    
   40    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 4 AND 5                                                      
C                                                                       
   50 CONTINUE                                                          
         N = 1                                                          
         WRITE (IWRITE,60) KASE,N                                       
   60    FORMAT (5H KASE, I3, 3X, 16HMONOELEMENTAL    / 4H N =, I4)     
         IF (KASE .EQ. 4) A(1,1) = 3.0E0                                
         IF (KASE .EQ. 5) A(1,1) = 0.0E0                                
      GO TO 350                                                         
C                                                                       
C     KASE 6, 7 AND 8                                                   
C                                                                       
   70 CONTINUE                                                          
         N = 15                                                         
         IF (KASE .NE. 8) WRITE (IWRITE,80) KASE,N                      
   80    FORMAT (5H KASE, I3, 3X, 16HTRIDIAGONAL      / 4H N =, I4)     
         IF (KASE .EQ. 8) WRITE (IWRITE,90) KASE,N                      
   90    FORMAT (5H KASE, I3, 3X, 16HDIAGONAL         / 4H N =, I4)     
         T = 1.0E0                                                      
         IF (KASE .EQ. 7) T = 2.0E0                                     
         IF (KASE .EQ. 8) T = 0.0E0                                     
         DO 110 J = 1, N                                                
            DO 100 I = 1, J                                             
               A(I,J) = 0.0E0                                           
               IF (I .EQ. J) A(I,I) = 4.0E0                             
               IF (I .EQ. J - 1) A(I,J) = T                             
               A(J,I) = A(I,J)                                          
  100       CONTINUE                                                    
  110    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 9                                                            
C                                                                       
  120 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,130) KASE,N                                      
  130    FORMAT (5H KASE, I3, 3X, 16HPENTADIAGONAL    / 4H N =, I4)     
         DO 150 J = 1, N                                                
            DO 140 I = 1, N                                             
               A(I,J) = 0.0E0                                           
               IF (IABS(I-J) .LE. 2)                                    
     *            A(I,J) = (5.0E0 - FLOAT(IABS(I-J)))**(10 - I - J)     
  140       CONTINUE                                                    
  150    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 10                                                           
C                                                                       
  160 CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,170) KASE,N                                      
  170    FORMAT (5H KASE, I3, 3X, 16HTRIDIAG INVERSE  / 4H N =, I4)     
         DO 190 J = 1, N                                                
            DO 180 I = 1, J                                             
               A(I,J) = FLOAT(N+1-J)                                    
               A(J,I) = A(I,J)                                          
  180       CONTINUE                                                    
  190    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 11                                                           
C                                                                       
  200 CONTINUE                                                          
         N = 15                                                         
         WRITE (IWRITE,210) KASE,N                                      
  210    FORMAT (5H KASE, I3, 3X, 16HTEST COND        / 4H N =, I4)     
         DO 230 J = 1, N                                                
            DO 220 I = 1, N                                             
               IF (I .EQ. J) A(I,J) = FLOAT(I)                          
               IF (I .GT. J) A(I,J) = FLOAT(J-2)                        
               IF (I .LT. J) A(I,J) = FLOAT(I-2)                        
  220       CONTINUE                                                    
  230    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 12                                                           
C                                                                       
  240 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,250) KASE,N                                      
  250    FORMAT (5H KASE, I3, 3X, 16HNEAR UNDERFLOW   / 4H N =, I4)     
         TINY = R1MACH(1)*FLOAT(N*N*100)                                
         WRITE (IWRITE,260) TINY                                        
  260    FORMAT (14H TINY        =, 1PE15.5)                            
         DO 280 I = 1, N                                                
            DO 270 J = 1, N                                             
               A(I,J) = TINY*FLOAT(MIN0(I,J))/FLOAT(MAX0(I,J))          
  270       CONTINUE                                                    
  280    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 13                                                           
C                                                                       
  290 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,300) KASE,N                                      
  300    FORMAT (5H KASE, I3, 3X, 16HNEAR OVERFLOW    / 4H N =, I4)     
         HUGE = R1MACH(2)/FLOAT(N*N)                                    
         WRITE (IWRITE,310) HUGE                                        
  310    FORMAT (14H HUGE        =, 1PE15.5)                            
         DO 330 I = 1, N                                                
            DO 320 J = 1, N                                             
               A(I,J) = HUGE*FLOAT(MIN0(I,J))/FLOAT(MAX0(I,J))          
  320       CONTINUE                                                    
  330    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
  340 CONTINUE                                                          
         N = 0                                                          
  350 CONTINUE                                                          
      RETURN                                                            
C                                                                       
      END                                                               
C$TEST LYAD                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAMS DSYCE AND FRIENDS                          
C                                                                       
C***********************************************************************
C     MAIN PROGRAM                                                      
      INTEGER IWRITE,I1MACH                                             
C     ALLOW 5000 UNDERFLOWS.                                            
C                                                                       
C     SET OUTPUT UNIT NUMBER                                            
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      CALL SPOTS(IWRITE)                                                
      STOP                                                              
      END                                                               
      SUBROUTINE SPOTS(IWRITE)                                          
C     IWRITE IS THE OUTPUT UNIT NUMBER                                  
C                                                                       
C     TESTS                                                             
C        DSYCE;DSYFBS;DSYML;DSYLE;DBPCE;DBPFS;DBPBS;DBPML;DBPLE         
C                                                                       
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     PORT DSYCE,DSYFBS,DSYML,DSYLE,DBPCE,DBPFS,DBPBS,DBPML,DBPLE       
C     EXTERNAL SPOXX,D1MACH                                             
C     BLAS DAXPY,DDOT,DASUM                                             
C     FORTRAN DABS,DMAX1,FLOAT,MAX0                                     
C                                                                       
C     INTERNAL VARIABLES                                                
C                                                                       
      INTEGER IPVT(15),IPVTS(15)                                        
      INTEGER I,IQ(10),I1,J,JB,INDEX                                    
      INTEGER K,KASE,KB,KBFAIL,KNPD,KOUNT,KPFAIL                        
      INTEGER KSUSP(10),LDA,IWRITE,M,N,NPRINT                           
      REAL    Q(10),QS(10)                                              
      DOUBLE PRECISION APSAVE(120),AB(15,15),AINV(15,15),ASAVE(15,15)   
      DOUBLE PRECISION AP(120),B(15),DDOT,X(15),XB(15),XEXACT(15)       
      DOUBLE PRECISION ABSAVE(15,15),DENOM                              
      DOUBLE PRECISION XP(15),T,Z(15)                                   
      DOUBLE PRECISION ANORM,AINORM,COND,COND1,AIBNO,EBNORM             
      DOUBLE PRECISION EN,ENORM,EPS,FNORM,RCOND,RCONDB                  
      DOUBLE PRECISION RCONDP,RNORM,DASUM,D1MACH,XNORM                  
      DOUBLE PRECISION AINVB(15,15),AIS,SC                              
      LOGICAL KBF,KPF                                                   
C                                                                       
      LDA = 15                                                          
C                                                                       
C     WRITE MATRIX AND SOLUTIONS IF  N .LE. NPRINT                      
C                                                                       
      NPRINT = 3                                                        
C                                                                       
      WRITE (IWRITE,560)                                                
      WRITE (IWRITE,1000)                                               
C                                                                       
      DO 10 I = 1,10                                                    
         KSUSP(I) = 0                                                   
   10 CONTINUE                                                          
      KNPD = 0                                                          
      KPFAIL = 0                                                        
      KBFAIL = 0                                                        
C                                                                       
C     SET EPS TO ROUNDING UNIT FOR DOUBLE PRECISION ARITHMETIC          
C                                                                       
      EPS = D1MACH(4)                                                   
      WRITE (IWRITE,570) EPS                                            
      WRITE (IWRITE,550)                                                
C                                                                       
        CALL ENTER(1)                                                   
C     START MAIN LOOP                                                   
C                                                                       
      KASE = 1                                                          
   20 CONTINUE                                                          
C                                                                       
C        GENERATE TEST MATRIX                                           
C                                                                       
         CALL SPOXX(ASAVE,LDA,N,KASE,IWRITE)                            
C                                                                       
C        N = 0 SIGNALS NO MORE TEST MATRICES                            
C                                                                       
C     ...EXIT                                                           
         IF (N .LE. 0) GO TO 540                                        
         ANORM = 0.0D0                                                  
         DO 30 J = 1, N                                                 
            ANORM = DMAX1(ANORM,DASUM(N,ASAVE(1,J),1))                  
   30    CONTINUE                                                       
         WRITE (IWRITE,720) ANORM                                       
C                                                                       
         IF (N .GT. NPRINT) GO TO 50                                    
            WRITE (IWRITE,550)                                          
            DO 40 I = 1, N                                              
               WRITE (IWRITE,760) (ASAVE(I,J), J = 1, N)                
   40       CONTINUE                                                    
            WRITE (IWRITE,550)                                          
   50    CONTINUE                                                       
C                                                                       
C        GENERATE EXACT SOLUTION                                        
C                                                                       
         XEXACT(1) = 1.0D0                                              
         IF (N .GE. 2) XEXACT(2) = 0.0D0                                
         IF (N .LE. 2) GO TO 70                                         
            DO 60 I = 3, N                                              
               XEXACT(I) = -XEXACT(I-2)                                 
   60       CONTINUE                                                    
   70    CONTINUE                                                       
C                                                                       
C                                                                       
C      PUT INTO PACKED FORM                                             
         K = 0                                                          
         DO 130 J = 1, N                                                
            DO 120 I = J,N                                              
               K = K + 1                                                
               AP(K) = ASAVE(I,J)                                       
               APSAVE(K)=AP(K)                                          
  120       CONTINUE                                                    
  130    CONTINUE                                                       
         CALL DSYCE(N,AP,IPVTS,RCONDP)                                  
         IF (NERROR(IERR).NE.0) CALL ERROFF                             
C        FACTOR BAND FORM AND COMPARE                                   
C                                                                       
         KBF = .FALSE.                                                  
         M = 0                                                          
         DO 200 J = 1, N                                                
            DO 190 I = 1, J                                             
               IF (ASAVE(I,J) .NE. 0.0D0) M = MAX0(M,J-I)               
  190       CONTINUE                                                    
  200    CONTINUE                                                       
C                                                                       
         DO 220 J = 1, N                                                
             I1=MIN0(N,J+M)                                             
            DO 210 I = J, I1                                            
               K =I-J+1                                                 
               AB(K,J) = ASAVE(I,J)                                     
               ABSAVE(K,J)=AB(K,J)                                      
  210       CONTINUE                                                    
  220    CONTINUE                                                       
         WRITE (IWRITE,840) M                                           
        CALL DBPCE(N,M+1,AB,LDA,RCONDB)                                 
           IF (NERROR(IERR).EQ.0) GO TO 230                             
          CALL ERROFF                                                   
          IF ((IERR).LT.10+N)GO TO 226                                  
             WRITE(IWRITE,860)IERR                                      
             INDEX = IERR - N - 10                                      
             WRITE(IWRITE,229)AB(1,INDEX)                               
229           FORMAT( 19H OFFENDING DIAGONAL,D17.7)                     
             GO TO 530                                                  
226          WRITE(IWRITE,580)                                          
            WRITE (IWRITE,930) RCONDP,RCONDB                            
             GO TO 530                                                  
230        CONTINUE                                                     
            WRITE(IWRITE,930)RCONDP,RCONDB                              
C                                                                       
C           COMPUTE INVERSE AND COND1 = TRUE CONDITION                  
C                                                                       
            DO 290 J = 1, N                                             
               DO 280 I = 1, N                                          
                   AINV(I,J)=0.D0                                       
                   AINVB(I,J)=0.D0                                      
  280          CONTINUE                                                 
               AINV(J,J)=1.D0                                           
               AINVB(J,J)=1.D0                                          
  290       CONTINUE                                                    
           CALL DBPFS(N,M+1,AB,LDA,AINVB,LDA,N)                         
            CALL DBPBS(N,M+1,AB,LDA,AINVB,LDA,N)                        
           CALL DSYFBS(N,AP,AINV,LDA,N,IPVTS)                           
           AINORM=0.D0                                                  
           DO 310 J=1,N                                                 
              AIS=DASUM(N,AINV(1,J),1)                                  
               AINORM = DMAX1(AINORM,AIS)                               
  310       CONTINUE                                                    
            COND1 = ANORM*AINORM                                        
            WRITE (IWRITE,600) COND1                                    
C                                                                       
C           GENERATE RIGHT HAND SIDE FOR BOTH SYMMETRIC AND BAND        
C                                                                       
            CALL DSYML(N,APSAVE,XEXACT,B)                               
            CALL MOVEFD(N,B,X)                                          
            CALL DBPML(N,M+1,ABSAVE,LDA,XEXACT,XB)                      
C           SOLVE A*X = B                                               
C                                                                       
            CALL DSYLE(N,APSAVE,X,N,1)                                  
            IF (NERROR(IRE).NE.0) CALL ERROFF                           
            CALL DBPLE(N,M+1,ABSAVE,LDA,XB,N,1)                         
            IF (IRE+NERROR(IRB).EQ.0) GO TO 311                         
               IF (IRB.NE.0) CALL ERROFF                                
               WRITE(IWRITE,580)                                        
               GO TO 530                                                
  311       CONTINUE                                                    
C                                                                       
            IF (N .GT. NPRINT) GO TO 330                                
               WRITE (IWRITE,610)                                       
               DO 320 I = 1, N                                          
                  WRITE (IWRITE,790) X(I), XB(I)                        
  320          CONTINUE                                                 
               WRITE (IWRITE,550)                                       
  330       CONTINUE                                                    
C                                                                       
C                                                                       
C           COMPUTE ERRORS AND RESIDUALS                                
C              E  =  X - XEXACT                                         
C              EB =  XB - XEXACT                                        
C              R  =  B - A*X                                            
C              F  =  A - TRANS(R)*R                                     
C                                                                       
            XNORM = DASUM(N,X,1)                                        
            ENORM = 0.0D0                                               
            EBNORM = 0.E0                                               
            DO 460 J = 1, N                                             
               ENORM = ENORM + DABS(X(J)-XEXACT(J))                     
               EBNORM = EBNORM + DABS(XB(J)-XEXACT(J))                  
               T = -X(J)                                                
               CALL DAXPY(N,T,ASAVE(1,J),1,B,1)                         
  460       CONTINUE                                                    
            RNORM = DASUM(N,B,1)                                        
C                                                                       
C           A*INV(A) - I                                                
C                                                                       
            AINORM = 0.0D0                                              
            AIBNO = 0.E0                                                
            DO 490 J = 1, N                                             
               DO 470 I = 1, N                                          
                  B(I) = 0.0D0                                          
                 XB(I) = 0.0D0                                          
  470          CONTINUE                                                 
               DO 480 K = 1, N                                          
                  T = AINV(K,J)                                         
                  CALL DAXPY(N,T,ASAVE(1,K),1,B,1)                      
                  T=AINVB(K,J)                                          
                  CALL DAXPY(N,T,ASAVE(1,K),1,XB,1)                     
  480          CONTINUE                                                 
               B(J) = B(J) - 1.0D0                                      
               XB(J) = XB(J) - 1.0D0                                    
               AINORM = DMAX1(AINORM,DASUM(N,B,1))                      
               AIBNO = DMAX1(AIBNO,DASUM(N,XB,1))                       
  490       CONTINUE                                                    
            FNORM = 0.0D0                                               
            ML=M+1                                                      
            NP1=N+1                                                     
            MLP1=ML+1                                                   
            DO 495 J=1,N                                                
               NUMAX= MIN0(ML,NP1-J)                                    
               JM1=J-1                                                  
               IEND=JM1+NUMAX                                           
               DO 491 I=J,IEND                                          
                  B(I)=-ASAVE(I,J)                                      
491            CONTINUE                                                 
               KBEGIN=MAX0(1,J-M)                                       
               L=MIN0(J,ML)                                             
               NUM=MLP1-L                                               
               IF (JM1.LT.KBEGIN) GO TO 493                             
               DO 492 K=KBEGIN,JM1                                      
                  SC=AB(L,K)*AB(1,K)                                    
                  CALL DAXPY(NUM,SC,AB(L,K),1,B(J),1)                   
                  L=L-1                                                 
                  NUM=MIN0(NUM+1,NUMAX)                                 
492            CONTINUE                                                 
493            SC=AB(1,J)                                               
               CALL DAXPY(NUM-1,SC,AB(2,J),1,B(J+1),1)                  
               B(J)=B(J)+AB(1,J)                                        
               FNORM=DMAX1(FNORM,DASUM(NUMAX,B(J),1))                   
495        CONTINUE                                                     
C                                                                       
            WRITE (IWRITE,620) ENORM, EBNORM                            
            WRITE (IWRITE,630) RNORM                                    
            WRITE (IWRITE,730) FNORM                                    
            WRITE (IWRITE,740) AINORM,AIBNO                             
C                                                                       
C           COMPUTE TEST RATIOS                                         
C                                                                       
            Q(1) = RCONDP/COND1                                         
            Q(2) = RCONDB/COND1                                         
            Q(3) = COND1/RCONDP                                         
            Q(4) = COND1/RCONDB                                         
            Q(5) = ENORM/(EPS*RCONDP*XNORM)                             
            Q(6) = EBNORM/(EPS*RCONDP*XNORM)                            
            DENOM=DMAX1(1.0D2*D1MACH(1),EPS*ANORM*XNORM)                
            Q(7)= RNORM/DENOM                                           
            DENOM=DMAX1(1.0D2*D1MACH(1),EPS*ANORM)                      
            Q(8)=FNORM/DENOM                                            
            Q(9) = AINORM/(EPS*RCONDP)                                  
            Q(10) = AIBNO/(EPS*RCONDP)                                  
            WRITE (IWRITE,550)                                          
            WRITE (IWRITE,640)                                          
            WRITE (IWRITE,550)                                          
            WRITE (IWRITE,690)                                          
            WRITE (IWRITE,700)                                          
            WRITE (IWRITE,710)                                          
            WRITE (IWRITE,550)                                          
            WRITE (IWRITE,750) (Q(I), I = 1, 10)                        
            WRITE (IWRITE,550)                                          
C                                                                       
C           LOOK FOR SUSPICIOUS RATIOS                                  
C                                                                       
            QS(1) = 1.0D0 + 4.0D0*EPS                                   
            QS(2) = QS(1)                                               
            QS(3) = 10.0D0                                              
            QS(4) =QS(3)                                                
            EN = DBLE(FLOAT(N))                                         
            IF (N .EQ. 1) EN = 2.0D0                                    
            DO 500 I=5,10                                               
               QS(I) = EN                                               
  500       CONTINUE                                                    
            KOUNT = 0                                                   
            DO 520 I = 1, 10                                            
               IQ(I) = 0                                                
               IF (Q(I) .LE. QS(I)) GO TO 510                           
                  IQ(I) = 1                                             
                  KSUSP(I) = KSUSP(I) + 1                               
                  KOUNT = KOUNT + 1                                     
  510          CONTINUE                                                 
  520       CONTINUE                                                    
            IF (KOUNT .EQ. 0) WRITE (IWRITE,980)                        
            IF (KOUNT .NE. 0) WRITE (IWRITE,990) (IQ(I), I = 1,10)      
            WRITE (IWRITE,550)                                          
  530    CONTINUE                                                       
C                                                                       
         WRITE (IWRITE,650)                                             
         KASE = KASE + 1                                                
      GO TO 20                                                          
  540 CONTINUE                                                          
C                                                                       
C     FINISH MAIN LOOP                                                  
C                                                                       
C     SUMMARY                                                           
C                                                                       
      WRITE (IWRITE,660)                                                
      KASE = KASE - 1                                                   
      WRITE (IWRITE,670) KASE                                           
      WRITE (IWRITE,680) KSUSP                                          
      WRITE (IWRITE,910)                                                
      RETURN                                                            
C                                                                       
C     MOST FORMATS, ALSO SOME IN SPOXX                                  
C                                                                       
  550 FORMAT (1H )                                                      
 560  FORMAT(24H1PORT TESTER,DSY** DBP**)                               
  570 FORMAT ( / 14H EPSILON     =, 1PD15.5)                            
  580 FORMAT ( / 16H MAYBE SINGULAR. /)                                 
  600 FORMAT (14H ACTUAL COND =, 1PD15.5)                               
  610 FORMAT ( / 4H X =)                                                
  620 FORMAT (14H ERROR NORM  =, 2(1PD15.5))                            
  630 FORMAT (14H RESID NORM  =, 1P1D15.5)                              
  640 FORMAT (26H TEST RATIOS.. E = EPSILON)                            
  650 FORMAT ( / 14H ************* /)                                   
  660 FORMAT (8H1SUMMARY)                                               
  670 FORMAT (18H NUMBER OF TESTS =, I4)                                
  680 FORMAT ( / 30H NUMBER OF SUSPICIOUS RATIOS =, 10I4)               
  690 FORMAT( 42H    COND  COND(B)  ACTUAL  ACTUAL  ERROR  ,            
     1        40HERROR(B)  RESID  A-RT*R A*AI-I A*AI-I(B))              
  700 FORMAT (10(8H   -----))                                           
 710  FORMAT(42H    ACTUAL ACTUAL   COND  COND(B) E*COND*X,             
     1       40H E*COND*X E*A*X    E*A   E*COND  E*COND )               
  720 FORMAT (14H NORM(A)     =, 1PD15.5)                               
  730 FORMAT (14H NORM(A-RT*R)=, 1PD15.5)                               
  740 FORMAT (14H NORM(A*AI-I)=, 2(1PD15.5))                            
  750 FORMAT (10(1X, F7.2))                                             
 760  FORMAT(1H ,3D15.5)                                                
  780 FORMAT (2D18.6)                                                   
  790 FORMAT (2D18.6)                                                   
  830 FORMAT ( / 28H BAND ROUTINES DO NOT AGREE,)                       
  840 FORMAT (5H M  =, I2)                                              
  860 FORMAT (30H NOT POSITIVE DEFINITE, INFO =, I2)                    
  910 FORMAT ( / 12H END OF TEST)                                       
  930 FORMAT (8H RCOND =, 3(1PD15.5))                                   
  980 FORMAT (21H NO SUSPICIOUS RATIOS)                                 
  990 FORMAT (I8, 5I10 / 7X, 28H1 INDICATES SUSPICIOUS RATIO)           
 1000 FORMAT (29H THIS VERSION DATED 09/21/78.)                         
      END                                                               
      SUBROUTINE SPOXX(A,LDA,N,KASE,IWRITE)                             
C                                                                       
C     GENERATES DOUBLE PRECISION POSITIVE DEFINITE TEST MATRICES        
C                                                                       
C     EXTERNAL D1MACH                                                   
C     FORTRAN DABS,FLOAT,IABS,MAX0,MIN0                                 
      INTEGER LDA,N,KASE,IWRITE                                         
      INTEGER I,J                                                       
      DOUBLE PRECISION A(LDA,1)                                         
      DOUBLE PRECISION T                                                
      DOUBLE PRECISION TINY,HUGE,D1MACH                                 
C                                                                       
      GO TO (10, 10, 10, 50, 50, 70, 70, 70, 120, 160, 200, 240, 290,   
     *       340), KASE                                                 
C                                                                       
C     KASE 1, 2 AND 3                                                   
C                                                                       
   10 CONTINUE                                                          
         N = 5*KASE                                                     
         WRITE (IWRITE,20) KASE,N                                       
   20    FORMAT (5H KASE, I3, 3X, 16HHILBERT          / 4H N =, I4)     
         T = 1.0D0                                                      
         T = DSIGN(1.0D0,T)                                             
         DO 40 J = 1, N                                                 
            DO 30 I = 1, N                                              
               A(I,J) = T**(I - J)/DBLE(FLOAT(I+J-1))                   
C              FOR DOUBLE PRECISION MATRICES, A(I,J) = 1.0/FLOAT(I+J-1) 
   30       CONTINUE                                                    
   40    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 4 AND 5                                                      
C                                                                       
   50 CONTINUE                                                          
         N = 1                                                          
         WRITE (IWRITE,60) KASE,N                                       
   60    FORMAT (5H KASE, I3, 3X, 16HMONOELEMENTAL    / 4H N =, I4)     
         IF (KASE .EQ. 4) A(1,1) = 3.0D0                                
         IF (KASE .EQ. 5) A(1,1) = 0.0D0                                
      GO TO 350                                                         
C                                                                       
C     KASE 6, 7 AND 8                                                   
C                                                                       
   70 CONTINUE                                                          
         N = 15                                                         
         IF (KASE .NE. 8) WRITE (IWRITE,80) KASE,N                      
   80    FORMAT (5H KASE, I3, 3X, 16HTRIDIAGONAL      / 4H N =, I4)     
         IF (KASE .EQ. 8) WRITE (IWRITE,90) KASE,N                      
   90    FORMAT (5H KASE, I3, 3X, 16HDIAGONAL         / 4H N =, I4)     
         T = 1.0D0                                                      
         IF (KASE .EQ. 7) T = 2.0D0                                     
         IF (KASE .EQ. 8) T = 0.0D0                                     
         DO 110 J = 1, N                                                
            DO 100 I = 1, J                                             
               A(I,J) = 0.0D0                                           
               IF (I .EQ. J) A(I,I) = 4.0D0                             
               IF (I .EQ. J - 1) A(I,J) = T                             
               A(J,I) = A(I,J)                                          
  100       CONTINUE                                                    
  110    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 9                                                            
C                                                                       
  120 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,130) KASE,N                                      
  130    FORMAT (5H KASE, I3, 3X, 16HPENTADIAGONAL    / 4H N =, I4)     
         DO 150 J = 1, N                                                
            DO 140 I = 1, N                                             
               A(I,J) = 0.0D0                                           
               IF (IABS(I-J) .LE. 2)                                    
     *         A(I,J) = (5.0D0 - DBLE(FLOAT(IABS(I-J))))**(10 - I - J)  
  140       CONTINUE                                                    
  150    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 10                                                           
C                                                                       
  160 CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,170) KASE,N                                      
  170    FORMAT (5H KASE, I3, 3X, 16HTRIDIAG INVERSE  / 4H N =, I4)     
         DO 190 J = 1, N                                                
            DO 180 I = 1, J                                             
               A(I,J) = DBLE(FLOAT(N+1-J))                              
               A(J,I) = A(I,J)                                          
  180       CONTINUE                                                    
  190    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 11                                                           
C                                                                       
  200 CONTINUE                                                          
         N = 15                                                         
         WRITE (IWRITE,210) KASE,N                                      
  210    FORMAT (5H KASE, I3, 3X, 16HTEST COND        / 4H N =, I4)     
         DO 230 J = 1, N                                                
            DO 220 I = 1, N                                             
               IF (I .EQ. J) A(I,J) = DBLE(FLOAT(I))                    
               IF (I .GT. J) A(I,J) = DBLE(FLOAT(J-2))                  
               IF (I .LT. J) A(I,J) = DBLE(FLOAT(I-2))                  
  220       CONTINUE                                                    
  230    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 12                                                           
C                                                                       
  240 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,250) KASE,N                                      
  250    FORMAT (5H KASE, I3, 3X, 16HNEAR UNDERFLOW   / 4H N =, I4)     
         TINY = D1MACH(1)*DBLE(FLOAT(N*N*100))                          
         WRITE (IWRITE,260) TINY                                        
  260    FORMAT (14H TINY        =, 1PD15.5)                            
         DO 280 I = 1, N                                                
            DO 270 J = 1, N                                             
             A(I,J) = TINY*DBLE(FLOAT(MIN0(I,J)))/DBLE(FLOAT(MAX0(I,J)))
  270       CONTINUE                                                    
  280    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 13                                                           
C                                                                       
  290 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,300) KASE,N                                      
  300    FORMAT (5H KASE, I3, 3X, 16HNEAR OVERFLOW    / 4H N =, I4)     
         HUGE = D1MACH(2)/DBLE(FLOAT(N*N))                              
         WRITE (IWRITE,310) HUGE                                        
  310    FORMAT (14H HUGE        =, 1PD15.5)                            
         DO 330 I = 1, N                                                
            DO 320 J = 1, N                                             
             A(I,J) = HUGE*DBLE(FLOAT(MIN0(I,J)))/DBLE(FLOAT(MAX0(I,J)))
  320       CONTINUE                                                    
  330    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
  340 CONTINUE                                                          
         N = 0                                                          
  350 CONTINUE                                                          
      RETURN                                                            
C                                                                       
      END                                                               
C$TEST LGAC                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAMS CGECE AND FRIENDS                          
C                                                                       
C***********************************************************************
C     MAIN PROGRAM                                                      
      INTEGER IWRITE,I1MACH                                             
C     ALLOW 5000 UNDERFLOWS.                                            
C                                                                       
C     SET OUTPUT UNIT NUMBER                                            
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      CALL SGETS(IWRITE)                                                
      STOP                                                              
      END                                                               
      SUBROUTINE SGETS(IWRITE)                                          
C     IWRITE IS THE OUTPUT UNIT NUMBER                                  
C                                                                       
C     TESTS                                                             
C        CGECE,CGEFS,CGEBS,CGEML,CBACE,CBABS,CBAFS,CBALE,CBAML          
C                                                                       
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     PORT CGECE,CGEFS,CGEBS,CGEML,CBACE,CBAFS,CBABS,CBAML,CBALE        
C     PORT UTILITIES ERROFF,ENTER,LEAVE,R1MACH                          
C     EXTERNAL SGEXX                                                    
C     BLAS CAXPY,CSCAL,SCASUM,CABS1                                     
C     FORTRAN AMAX1,FLOAT,MAX0,MIN0,CABS                                
C                                                                       
C     INTERNAL VARIABLES                                                
C                                                                       
      INTEGER I,IPVT(15),IPVTB(15),IQ(8),I1,I2,J                        
      INTEGER K,KASE,KB,KBFAIL,KOUNT,KP1,KSING,KSUSP(8)                 
      INTEGER L,LDA,LDAB,IWRITE,M,ML,MU,N,NM1,NPRINT                    
      REAL CMACH,HUGE,TINY                                              
      REAL AINORM,ANORM,SMACH,COND,COND1,EN,ENORM,EPS                   
      REAL ETNORM,FNI,FNORM,ONEPX,RCOND,RCONDB,RNORM                    
      REAL RTNORM,Q(10),QS(10),SCASUM,XNORM,DENOM                       
      COMPLEX A(15,15),AB(43,15),AINV(15,15),ASAVE(15,15),AL(43,15)     
      COMPLEX B(15),BT(15),CDOTU,DET(2),DETB(2)                         
      COMPLEX X(15),XB(15),XEXACT(15),XT(15),XTB(15),T                  
      COMPLEX ABSAVE(43,15),AINVB(15,15)                                
      LOGICAL KBF                                                       
C                                                                       
      LDA = 15                                                          
      CALL ENTER(1)                                                     
      LDAB = 43                                                         
C                                                                       
C     WRITE MATRIX AND SOLUTIONS IF  N .LE. NPRINT                      
C                                                                       
      NPRINT =3                                                         
C                                                                       
      WRITE (IWRITE,380)                                                
      WRITE (IWRITE,670)                                                
C                                                                       
      DO 10 I = 1, 8                                                    
         KSUSP(I) = 0                                                   
 10   CONTINUE                                                          
      KSING = 0                                                         
      KBFAIL = 0                                                        
C                                                                       
C     SET EPS TO ROUNDING UNIT                                          
C                                                                       
      EPS =R1MACH(4)                                                    
      WRITE (IWRITE,390) EPS                                            
      WRITE (IWRITE,370)                                                
C                                                                       
C     START MAIN LOOP                                                   
C                                                                       
      KASE = 1                                                          
 20   CONTINUE                                                          
C                                                                       
C        GENERATE TEST MATRIX                                           
C                                                                       
         CALL CGEXX(A,LDA,N,KASE,IWRITE)                                
C                                                                       
C        N = 0 SIGNALS NO MORE TEST MATRICES                            
C                                                                       
C     ...EXIT                                                           
         IF(N. LE.0) GO TO 360                                          
         ANORM = 0.0E0                                                  
         DO 30 J = 1, N                                                 
            ANORM = AMAX1(ANORM,SCASUM(N,A(1,J),1))                     
 30      CONTINUE                                                       
         WRITE (IWRITE,550) ANORM                                       
C                                                                       
         IF (N .GT. NPRINT) GO TO 50                                    
            WRITE (IWRITE,370)                                          
            DO 40 I = 1, N                                              
               WRITE (IWRITE,590) (A(I,J), J = 1, N)                    
 40         CONTINUE                                                    
            WRITE (IWRITE,370)                                          
 50      CONTINUE                                                       
C                                                                       
C        GENERATE EXACT SOLUTION                                        
C                                                                       
         XEXACT(1) = (1.0E0,1.0E0)                                      
         IF (N .GE. 2) XEXACT(2) = (0.0E0,0.0E0)                        
         IF (N .LE. 2) GO TO 70                                         
            DO 60 I = 3, N                                              
               XEXACT(I) = -XEXACT(I-2)                                 
 60         CONTINUE                                                    
 70      CONTINUE                                                       
C                                                                       
C        SAVE MATRIX AND GENERATE R.H.S.                                
C                                                                       
         DO 90 I = 1, N                                                 
            DO 80 J = 1, N                                              
               ASAVE(I,J) = A(I,J)                                      
 80         CONTINUE                                                    
 90      CONTINUE                                                       
         CALL CGEML(N,A,LDA,XEXACT,B)                                   
         CALL MOVEFC(N,B,X)                                             
C                                                                       
C        FACTOR AND ESTIMATE CONDITION                                  
C                                                                       
         CALL CGECE(N,A,LDA,IPVT,RCOND)                                 
         IF (NERROR(INE).NE.0) CALL  ERROFF                             
C                                                                       
C                                                                       
C        FACTOR BAND FORM AND COMPARE                                   
C                                                                       
         KBF = .FALSE.                                                  
         ML = 0                                                         
         MU = 0                                                         
         DO 120 J = 1, N                                                
            DO 110 I = 1, N                                             
               IF (CABS1(ASAVE(I,J)) .EQ. 0.0E0) GO TO 100              
                  IF (I .LT. J) MU = MAX0(MU,J-I)                       
                  IF (I .GT. J) ML = MAX0(ML,I-J)                       
 100           CONTINUE                                                 
 110        CONTINUE                                                    
 120     CONTINUE                                                       
         MLP1=ML+1                                                      
         WRITE (IWRITE,620) ML,MU                                       
            M = ML + MU + 1                                             
            DO 140 J = 1, N                                             
               I1 = MAX0(1,J-MU)                                        
               I2 = MIN0(N,J+ML)                                        
               DO 130 I = I1, I2                                        
                  K=ML+1+J-I                                            
                  AB(K,I) = ASAVE(I,J)                                  
                  ABSAVE(K,I)=ASAVE(I,J)                                
 130           CONTINUE                                                 
 140        CONTINUE                                                    
C                                                                       
            CALL CBACE(N,MLP1,M,AB,LDAB,AL,LDAB,IPVTB,MU,RCONDB)        
          WRITE(IWRITE,640)RCOND,RCONDB                                 
C                                                                       
C                                                                       
C           TEST FOR SINGULARITY                                        
C                                                                       
           IF (INE+NERROR(IRE).EQ.0) GO TO 160                          
             IF (IRE.NE.0) CALL ERROFF                                  
             IF (IRE.NE.INE) WRITE(IWRITE,150)                          
 150         FORMAT(35H BAND AND GENERAL ROUTINES DISAGREE)             
               WRITE (IWRITE,400)                                       
               KSING = KSING + 1                                        
            GO TO 340                                                   
 160        CONTINUE                                                    
C                                                                       
C              COMPUTE INVERSE AND COND1 = TRUE CONDITION               
C                                                                       
               DO 180 J = 1, N                                          
                  DO 170 I = 1, N                                       
                     AINV(I,J) = (0.E0,0.E0)                            
                     AINVB(I,J)=(0.E0,0.E0)                             
 170              CONTINUE                                              
               AINV(J,J)=(1.E0,0.E0)                                    
               AINVB(J,J)=(1.E0,0.E0)                                   
 180           CONTINUE                                                 
               CALL CGEFS(N,A,LDA,AINV,LDA,N,IPVT)                      
               CALL CGEBS(N,A,LDA,AINV,LDA,N)                           
               CALL CBAFS(N,MLP1,AL,LDAB,IPVTB,AINVB,LDA,N)             
               CALL CBABS(N,AB,LDAB,AINVB,LDA,N,MU)                     
               AINORM = 0.0E0                                           
               DO 190 J = 1, N                                          
                  AINORM = AMAX1(AINORM,SCASUM(N,AINV(1,J),1))          
 190           CONTINUE                                                 
               COND1 = ANORM*AINORM                                     
               WRITE (IWRITE,420) COND1                                 
C                                                                       
C              SOLVE  A*X = B                                           
C                                                                       
               CALL CGEFS(N,A,LDA,X,N,1,IPVT)                           
               CALL CGEBS(N,A,LDA,X,N,1)                                
C                                                                       
C              MORE BAND COMPARE                                        
C                                                                       
C              TEST CONSISTENCY OF BAML AND BALE                        
C                                                                       
               CALL CBAML(N,MLP1,M,ABSAVE,LDAB,XEXACT,XB)               
               CALL CBALE(N,MLP1,M,ABSAVE,LDAB,XB,N,1)                  
               IF (NERROR(IRE).EQ.0) GO TO 200                          
                    CALL ERROFF                                         
                    WRITE(IWRITE,410)                                   
                    GO TO 340                                           
 200            CONTINUE                                                
               IF (N .GT. NPRINT) GO TO 220                             
                  WRITE (IWRITE,430)                                    
                  DO 210 I = 1, N                                       
                     WRITE (IWRITE,610) X(I),XB(I)                      
 210              CONTINUE                                              
                  WRITE (IWRITE,370)                                    
 220           CONTINUE                                                 
C                                                                       
C              RECONSTRUCT  A  FROM TRIANGULAR FACTORS , L AND U        
C                                                                       
               NM1 = N - 1                                              
               IF (NM1 .LT. 1) GO TO 250                                
               DO 240 KB = 1, NM1                                       
                  K = N - KB                                            
                  NMK=N-K                                               
                  KP1 = K + 1                                           
                  L = IPVT(K)                                           
                  DO 230 J = KP1, N                                     
                     T = -A(K,J)                                        
                     CALL CAXPY(NMK,T,A(K+1,K),1,A(K+1,J),1)            
                     T = A(L,J)                                         
                     A(L,J) = A(K,J)                                    
                     A(K,J) = T                                         
 230              CONTINUE                                              
                  T = -A(K,K)                                           
                  CALL CSCAL(NMK,T,A(K+1,K),1)                          
                  T = A(L,K)                                            
                  A(L,K) = A(K,K)                                       
                  A(K,K) = T                                            
 240           CONTINUE                                                 
 250           CONTINUE                                                 
C                                                                       
C              COMPUTE ERRORS AND RESIDUALS                             
C                 E  =  X - XEXACT                                      
C                 EB =  XB - XEXACT                                     
C                 R  =  B - A*X                                         
C                 F  =  A - L*U                                         
C                 AI =  A*INV(A) - I                                    
C                 AIB = A(BAND)*INV(A(BAND)) - I                        
C                                                                       
               XNORM = SCASUM(N,X,1)                                    
               ENORM = 0.0E0                                            
               EBNORM=0.E0                                              
               FNORM = 0.0E0                                            
               DO 270 J = 1, N                                          
                  ENORM = ENORM + CABS(X(J)-XEXACT(J))                  
                  EBNORM = EBNORM + CABS(XB(J) - XEXACT(J))             
                  T = -X(J)                                             
                  CALL CAXPY(N,T,ASAVE(1,J),1,B,1)                      
                  FNI = 0.0E0                                           
                  DO 260 I = 1, N                                       
                     FNI = FNI + CABS(ASAVE(I,J)-A(I,J))                
 260              CONTINUE                                              
                  IF (FNI .GT. FNORM) FNORM = FNI                       
 270           CONTINUE                                                 
               RNORM = SCASUM(N,B,1)                                    
C                                                                       
C              A*INV(A) - I                                             
C                                                                       
               AINORM = 0.0E0                                           
               AIBNO=0.0E0                                              
               DO 300 J = 1, N                                          
                  DO 280 I = 1, N                                       
                     B(I) = (0.E0,0.E0)                                 
                     XB(I) = (0.E0,0.E0)                                
 280              CONTINUE                                              
                  DO 290 K = 1, N                                       
                     T = AINV(K,J)                                      
                     CALL CAXPY(N,T,ASAVE(1,K),1,B,1)                   
                     T=AINVB(K,J)                                       
                     CALL CAXPY(N,T,ASAVE(1,K),1,XB,1)                  
 290              CONTINUE                                              
                  B(J) = B(J) - (1.0E0,0.0E0)                           
                  XB(J) = XB(J) -(1.0E0,0.0E0)                          
                  AIBNO=AMAX1(AIBNO,SCASUM(N,XB,1))                     
                  AINORM = AMAX1(AINORM,SCASUM(N,B,1))                  
 300           CONTINUE                                                 
C                                                                       
               WRITE (IWRITE,440) ENORM,EBNORM                          
               WRITE (IWRITE,450) RNORM                                 
               WRITE (IWRITE,560) FNORM                                 
               WRITE (IWRITE,570) AINORM,AIBNO                          
C                                                                       
C              COMPUTE TEST RATIOS                                      
C                                                                       
               Q(1) = RCOND/COND1                                       
               Q(2) = RCONDB/COND1                                      
               Q(3) = COND1/RCOND                                       
               Q(4)=COND1/RCONDB                                        
               Q(5) = ENORM/(EPS*RCOND*XNORM)                           
               Q(6) = EBNORM/(EPS*RCOND*XNORM)                          
              DENOM=AMAX1(100.*R1MACH(1),EPS*ANORM*XNORM)               
               Q(7)=RNORM/DENOM                                         
               DENOM=AMAX1(100.*R1MACH(1),EPS*ANORM)                    
               Q(8)=FNORM/DENOM                                         
               Q(9) = AINORM/(EPS*RCOND)                                
               Q(10)=AIBNO/(EPS*RCOND)                                  
               WRITE (IWRITE,370)                                       
               WRITE (IWRITE,460)                                       
               WRITE (IWRITE,370)                                       
               WRITE (IWRITE,520)                                       
               WRITE (IWRITE,530)                                       
               WRITE (IWRITE,540)                                       
               WRITE (IWRITE,370)                                       
               WRITE (IWRITE,580) (Q(I), I = 1, 10)                     
               WRITE (IWRITE,370)                                       
C                                                                       
C              LOOK FOR SUSPICIOUS RATIOS                               
C                                                                       
               QS(1) = 1.0E0 + 4.0E0*EPS                                
               QS(2)=QS(1)                                              
               QS(4) = 10.0E0                                           
               QS(3)=10.0E0                                             
               EN = FLOAT(N)                                            
               IF (N.LT.3)EN=3.0                                        
               DO 310 I = 3, 10                                         
                  QS(I) = EN                                            
 310           CONTINUE                                                 
               KOUNT = 0                                                
            IF (KASE.EQ.15)QS(7)=QS(7)*100.0                            
            IF (KASE.EQ.15)QS(8)=QS(8)*100.0                            
               DO 330 I = 1, 10                                         
                  IQ(I) = 0                                             
                  IF (Q(I) .LE. QS(I)) GO TO 320                        
                     IQ(I) = 1                                          
                     KSUSP(I) = KSUSP(I) + 1                            
                     KOUNT = KOUNT + 1                                  
 320              CONTINUE                                              
 330           CONTINUE                                                 
               IF (KOUNT .EQ. 0) WRITE (IWRITE,650)                     
               IF (KOUNT .NE. 0) WRITE (IWRITE,660) (IQ(I), I = 1, 10)  
               WRITE (IWRITE,370)                                       
 340        CONTINUE                                                    
 350     CONTINUE                                                       
C                                                                       
         WRITE (IWRITE,470)                                             
         KASE = KASE + 1                                                
      GO TO 20                                                          
 360  CONTINUE                                                          
C                                                                       
C     FINISH MAIN LOOP                                                  
C                                                                       
C     SUMMARY                                                           
C                                                                       
      WRITE (IWRITE,480)                                                
      KASE = KASE - 1                                                   
      WRITE (IWRITE,490) KASE                                           
      WRITE (IWRITE,500) KSING                                          
      WRITE (IWRITE,510) KSUSP                                          
      WRITE (IWRITE,630)                                                
      CALL LEAVE                                                        
      RETURN                                                            
C                                                                       
C     MOST FORMATS, ALSO SOME IN SGEXX                                  
C                                                                       
 370  FORMAT (1H )                                                      
 380  FORMAT (29H1  PORT  TESTER, CGE**, CBA**)                         
 390  FORMAT ( / 14H EPSILON     =, 1PE17.5)                            
 400  FORMAT ( / 19H EXACT SINGULARITY. /)                              
 410  FORMAT ( / 16H MAYBE SINGULAR. /)                                 
 420  FORMAT (14H ACTUAL COND =, 1PE17.5)                               
 430  FORMAT(/14H X AND XBAND =)                                        
 440  FORMAT (14H ERROR NORMS =, 2(1PE17.5))                            
 450  FORMAT (14H RESID NORMS =, 2(1PE17.5))                            
 460  FORMAT (26H TEST RATIOS.. E = EPSILON)                            
 470  FORMAT ( / 14H ************* /)                                   
 480  FORMAT (8H1SUMMARY)                                               
 490  FORMAT (18H NUMBER OF TESTS =, I4)                                
 500  FORMAT (30H NUMBER OF SINGULAR MATRICES =, I4)                    
 510  FORMAT (30H NUMBER OF SUSPICIOUS RATIOS =, 10I4)                  
 520  FORMAT(42H    COND  COND(B)  ACTUAL  ACTUAL  ERROR  ,             
     1       40HERROR(B)  RESID  A-LU  A*AI-I  A*AI-I(B))               
 530  FORMAT (10(8H   -----))                                           
 540  FORMAT(42H    ACTUAL ACTUAL   COND  COND(B) E*COND*X,             
     1       40H E*COND*X E*A*X    E*A   E*COND  E*COND )               
 550  FORMAT (14H NORM(A)     =, 1PE17.5)                               
 560  FORMAT (14H NORM(A - LU)=, 1PE17.5)                               
 570  FORMAT (14H NORM(A*AI-I)=, 2(1PE17.5))                            
 580  FORMAT (10(1X, F7.2))                                             
 590  FORMAT (1H , 6E15.5)                                              
 600  FORMAT (14H 1/COND      =, 1PE17.5)                               
 610  FORMAT (4G14.6)                                                   
 620  FORMAT (5H ML =, I2, 6H  MU =, I2)                                
 630  FORMAT ( / 12H END OF TEST)                                       
 640  FORMAT(7H COND =,1PE17.5,13H COND(BAND) =,1PE17.5 /)              
 650  FORMAT (21H NO SUSPICIOUS RATIOS)                                 
 660  FORMAT (I8, 9I10 / 7X, 28H1 INDICATES SUSPICIOUS RATIO)           
 670  FORMAT (29H THIS VERSION DATED 03/11/78.)                         
      END                                                               
      SUBROUTINE CGEXX(A,LDA,N,KASE,IWRITE)                             
C                                                                       
C     GENERATES COMPLEX GENERAL TEST MATRICES                           
C                                                                       
C     EXTERNAL CMACH                                                    
C     FORTRAN CMPLX,FLOAT,MAX0                                          
      INTEGER LDA,N,KASE,IWRITE                                         
      COMPLEX A(LDA,1)                                                  
      COMPLEX T1,T2                                                     
      GO TO (10, 10, 10, 60, 60, 80, 80, 80, 120, 160, 200, 240, 280,   
     *       320, 360, 410, 460), KASE                                  
C                                                                       
C     KASE 1, 2 AND 3                                                   
C                                                                       
 10   CONTINUE                                                          
         N = 3*KASE                                                     
         WRITE (IWRITE,20) KASE,N                                       
 20      FORMAT (5H KASE, I3, 3X, 16HHILBERT SLICE    / 4H N =, I4)     
         DO 50 J = 1, N                                                 
            DO 40 I = 1, N                                              
               A(I,J) = (0.0E0,0.0E0)                                   
               IF (I .GT. J + 2) GO TO 30                               
               IF (I .LT. J - 3) GO TO 30                               
                  A(I,J) = (1.0E0,0.0E0)/CMPLX(FLOAT(I+J-1),1.0E0)      
 30            CONTINUE                                                 
 40         CONTINUE                                                    
 50      CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 4 AND 5                                                      
C                                                                       
 60   CONTINUE                                                          
         N = 1                                                          
         WRITE (IWRITE,70) KASE,N                                       
 70      FORMAT (5H KASE, I3, 3X, 16HMONOELEMENTAL    / 4H N =, I4)     
         IF (KASE .EQ. 4) A(1,1) = (3.0E0,1.0E0)                        
         IF (KASE .EQ. 5) A(1,1) = (0.0E0,0.0E0)                        
      GO TO 470                                                         
C                                                                       
C     KASE 6, 7 AND 8                                                   
C                                                                       
 80   CONTINUE                                                          
         N = 15                                                         
         WRITE (IWRITE,90) KASE,N                                       
 90      FORMAT (5H KASE, I3, 3X, 16HTRIDIAGONAL      / 4H N =, I4)     
         T1 = (1.0E0,0.0E0)                                             
         T2 = (1.0E0,0.0E0)                                             
         IF (KASE .EQ. 7) T1 = (100.0E0,100.0E0)                        
         IF (KASE .EQ. 8) T2 = (100.0E0,100.0E0)                        
         DO 110 I = 1, N                                                
            DO 100 J = 1, N                                             
               A(I,J) = (0.0E0,0.0E0)                                   
               IF (I .EQ. J) A(I,I) = (4.0E0,0.0E0)                     
               IF (I .EQ. J - 1) A(I,J) = T1                            
               IF (I .EQ. J + 1) A(I,J) = T2                            
 100        CONTINUE                                                    
 110     CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 9                                                            
C                                                                       
 120  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,130) KASE,N                                      
 130     FORMAT (5H KASE, I3, 3X, 16HRANK ONE         / 4H N =, I4)     
         DO 150 I = 1, N                                                
            DO 140 J = 1, N                                             
               A(I,J) = CMPLX(10.0E0**(I-J),0.0E0)                      
 140        CONTINUE                                                    
 150     CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 10                                                           
C                                                                       
 160  CONTINUE                                                          
         N = 4                                                          
         WRITE (IWRITE,170) KASE,N                                      
 170     FORMAT (5H KASE, I3, 3X, 16HZERO COLUMN      / 4H N =, I4)     
         DO 190 I = 1, N                                                
            DO 180 J = 1, N                                             
               T1 = CMPLX(FLOAT(J-3),0.0E0)                             
               T2 = CMPLX(FLOAT(I),0.0E0)                               
               A(I,J) = T1/T2                                           
 180        CONTINUE                                                    
 190     CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 11                                                           
C                                                                       
 200  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,210) KASE,N                                      
 210     FORMAT (5H KASE, I3, 3X, 16HTEST COND        / 4H N =, I4)     
         DO 230 I = 1, N                                                
            DO 220 J = 1, N                                             
               IF (I .EQ. J) A(I,J) = CMPLX(FLOAT(I),0.0E0)             
               IF (I .GT. J) A(I,J) = CMPLX(FLOAT(J-2),0.0E0)           
               IF (I .LT. J) A(I,J) = CMPLX(FLOAT(I-2),0.0E0)           
 220        CONTINUE                                                    
 230     CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 12                                                           
C                                                                       
 240  CONTINUE                                                          
         N = 3                                                          
         WRITE (IWRITE,250) KASE,N                                      
 250     FORMAT (5H KASE, I3, 3X, 16HIDENTITY         / 4H N =, I4)     
         DO 270 I = 1, N                                                
            DO 260 J = 1, N                                             
               IF (I .EQ. J) A(I,I) = (1.0E0,0.0E0)                     
               IF (I .NE. J) A(I,J) = (0.0E0,0.0E0)                     
 260        CONTINUE                                                    
 270     CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 13                                                           
C                                                                       
 280  CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,290) KASE,N                                      
 290     FORMAT (5H KASE, I3, 3X, 16HUPPER TRIANGULAR / 4H N =, I4)     
         DO 310 I = 1, N                                                
            DO 300 J = 1, N                                             
               IF (I .GT. J) A(I,J) = (0.0E0,0.0E0)                     
               IF (I .LE. J) A(I,J) = CMPLX(FLOAT(J-I+1),FLOAT(J-I))    
 300        CONTINUE                                                    
 310     CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 14                                                           
C                                                                       
 320  CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,330) KASE,N                                      
 330     FORMAT (5H KASE, I3, 3X, 16HLOWER TRIANGULAR / 4H N =, I4)     
         DO 350 I = 1, N                                                
            DO 340 J = 1, N                                             
               IF (I .LT. J) A(I,J) = (0.0E0,0.0E0)                     
               IF (I .GE. J) A(I,J) = CMPLX(FLOAT(I-J+1),-1.0E0)        
 340        CONTINUE                                                    
 350     CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 15                                                           
C                                                                       
 360  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,370) KASE,N                                      
 370     FORMAT (5H KASE, I3, 3X, 16HNEAR UNDERFLOW   / 4H N =, I4)     
         TINY =R1MACH(1)*FLOAT(N*N)*200.0                               
         WRITE (IWRITE,380) TINY                                        
 380     FORMAT (14H TINY        =, 1PE13.5)                            
         DO 400 I = 1, N                                                
            DO 390 J = 1, N                                             
               A(I,J) = CMPLX(TINY*FLOAT(J)/FLOAT(MAX0(I,J)),0.0E0)     
 390        CONTINUE                                                    
 400     CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
C     KASE 16                                                           
C                                                                       
 410  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,420) KASE,N                                      
 420     FORMAT (5H KASE, I3, 3X, 16HNEAR OVERFLOW    / 4H N =, I4)     
         HUGE= SQRT(R1MACH(2))/FLOAT(200*N*N)                           
         WRITE (IWRITE,430) HUGE                                        
 430     FORMAT (14H HUGE        =, 1PE13.5)                            
         DO 450 I = 1, N                                                
            DO 440 J = 1, N                                             
               A(I,J) = CMPLX(HUGE*FLOAT(J)/FLOAT(MAX0(I,J)),0.0E0)     
 440        CONTINUE                                                    
 450     CONTINUE                                                       
      GO TO 470                                                         
C                                                                       
 460  CONTINUE                                                          
         N = 0                                                          
 470  CONTINUE                                                          
      RETURN                                                            
C                                                                       
      END                                                               
C$TEST LYRC                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAMS CHECE AND FRIENDS                          
C                                                                       
C***********************************************************************
C     MAIN PROGRAM                                                      
      INTEGER IWRITE                                                    
C     ALLOW 5000 UNDERFLOWS.                                            
C                                                                       
C     OUTPUT UNIT NUMBER                                                
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      CALL SPOTS(IWRITE)                                                
      STOP                                                              
      END                                                               
      SUBROUTINE SPOTS(IWRITE)                                          
C     IWRITE IS THE OUTPUT UNIT NUMBER                                  
C                                                                       
C     TESTS                                                             
C        CHECE;CHEFBS;CHEML;CHELE;CBACE;CBAFS;CBABS;CBAML;CBALE         
C                                                                       
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     PORT CHECE,CHEFBS,CHEML,CHELE,CBACE,CBAFS,CBABS,CBAML,CBALE       
C     EXTERNAL CSIXX,R1MACH                                             
C     BLAS CAXPY,SCASUM                                                 
C     FORTRAN CABS1,AMAX1,FLOAT,MAX0                                    
C                                                                       
C     INTERNAL VARIABLES                                                
C                                                                       
      COMPLEX APSAVE(120),AB(35,15),AINV(15,15),ASAVE(15,15)            
      COMPLEX AP(120),B(15),X(15),XB(15),XEXACT(15)                     
      COMPLEX ABSAVE(35,15),AL(15,15)                                   
      COMPLEX T                                                         
      REAL ANORM,AINORM,COND1,AIBNO,EBNORM                              
      REAL EN,ENORM,EPS,Q(10),QS(10),RCONDB                             
      REAL RCONDP,RNORM,SCASUM,R1MACH,XNORM                             
      COMPLEX AINVB(15,15)                                              
      INTEGER IPVT(15),IPVTS(15)                                        
      INTEGER I,IQ(10),I1,J                                             
      INTEGER K,KASE,KBFAIL,KNPD,KOUNT,KPFAIL                           
      INTEGER KSUSP(10),LDA,IWRITE,M,N,NPRINT                           
      LOGICAL KBF                                                       
C                                                                       
      LDA = 15                                                          
      LDAB= 35                                                          
C                                                                       
C     WRITE MATRIX AND SOLUTIONS IF  N .LE. NPRINT                      
C                                                                       
      NPRINT = 3                                                        
C                                                                       
      WRITE (IWRITE,560)                                                
      WRITE (IWRITE,1000)                                               
C                                                                       
      DO 10 I = 1,10                                                    
         KSUSP(I) = 0                                                   
   10 CONTINUE                                                          
      KNPD = 0                                                          
      KPFAIL = 0                                                        
      KBFAIL = 0                                                        
C                                                                       
C     SET EPS TO ROUNDING UNIT FOR REAL ARITHMETIC                      
C                                                                       
      EPS = R1MACH(4)                                                   
      WRITE (IWRITE,570) EPS                                            
      WRITE (IWRITE,550)                                                
C                                                                       
        CALL ENTER(1)                                                   
C     START MAIN LOOP                                                   
C                                                                       
      KASE = 1                                                          
   20 CONTINUE                                                          
C                                                                       
C        GENERATE TEST MATRIX                                           
C                                                                       
         CALL CSIXX(ASAVE,LDA,N,KASE,IWRITE)                            
C                                                                       
C        N = 0 SIGNALS NO MORE TEST MATRICES                            
C                                                                       
C     ...EXIT                                                           
         IF (N .LE. 0) GO TO 540                                        
         ANORM = 0.0E0                                                  
         DO 30 J = 1, N                                                 
            ANORM = AMAX1(ANORM,SCASUM(N,ASAVE(1,J),1))                 
   30    CONTINUE                                                       
         WRITE (IWRITE,720) ANORM                                       
C                                                                       
         IF (N .GT. NPRINT) GO TO 50                                    
            WRITE (IWRITE,550)                                          
            DO 40 I = 1, N                                              
               WRITE (IWRITE,760) (ASAVE(I,J), J = 1, N)                
   40       CONTINUE                                                    
            WRITE (IWRITE,550)                                          
   50    CONTINUE                                                       
C                                                                       
C        GENERATE EXACT SOLUTION                                        
C                                                                       
         XEXACT(1) = (1.0E0,0.0E0)                                      
         IF (N .GE. 2) XEXACT(2) = (0.0E0,1.0E0)                        
         IF (N .LE. 2) GO TO 70                                         
            DO 60 I = 3, N                                              
               XEXACT(I) = -XEXACT(I-2)                                 
   60       CONTINUE                                                    
   70    CONTINUE                                                       
C                                                                       
C                                                                       
C      PUT INTO PACKED FORM                                             
         K = 0                                                          
         DO 130 J = 1, N                                                
            DO 120 I = J,N                                              
               K = K + 1                                                
               AP(K) = ASAVE(I,J)                                       
               APSAVE(K)=AP(K)                                          
  120       CONTINUE                                                    
  130    CONTINUE                                                       
         CALL CHECE(N,AP,IPVTS,RCONDP)                                  
         IF (NERROR(IERS).NE.0) CALL ERROFF                             
C        FACTOR BAND FORM AND COMPARE                                   
C                                                                       
         KBF = .FALSE.                                                  
         M = 0                                                          
         DO 200 J = 1, N                                                
            DO 190 I = 1, J                                             
               IF (ASAVE(I,J) .NE. 0.0E0) M = MAX0(M,J-I)               
  190       CONTINUE                                                    
  200    CONTINUE                                                       
C                                                                       
         ML=M+1                                                         
         DO 220 J = 1, N                                                
             I1=MIN0(N,J+M)                                             
             I2=MAX0(1,J-M)                                             
            DO 210 I = I2, I1                                           
               K =ML+I-J                                                
               AB(K,J) = ASAVE(J,I)                                     
               ABSAVE(K,J)=AB(K,J)                                      
  210       CONTINUE                                                    
  220    CONTINUE                                                       
        M=2*ML-1                                                        
         WRITE (IWRITE,840) M                                           
        CALL CBACE(N,ML,M,AB,LDAB,AL,LDA,IPVT,MU,RCONDB)                
           IF (NERROR(IERR).NE.0) CALL ERROFF                           
           IF(IERR+IERS.EQ.0) GO TO 230                                 
             WRITE(IWRITE,580)                                          
            WRITE (IWRITE,930) RCONDP,RCONDB                            
             GO TO 530                                                  
 230       CONTINUE                                                     
            WRITE(IWRITE,930)RCONDP,RCONDB                              
C                                                                       
C           COMPUTE INVERSE AND COND1 = TRUE CONDITION                  
C                                                                       
            DO 290 J = 1, N                                             
               DO 280 I = 1, N                                          
                   AINV(I,J)=(0.0,0.0)                                  
                   AINVB(I,J)=(0.0,0.0)                                 
  280          CONTINUE                                                 
               AINV(J,J)=(1.0,0.0)                                      
               AINVB(J,J)=(1.0,0.0)                                     
  290       CONTINUE                                                    
           CALL CBAFS(N,ML,AL,LDA,IPVT,AINVB,LDA,N)                     
           CALL CBABS(N,AB,LDAB,AINVB,LDA,N,MU)                         
           CALL CHEFBS(N,AP,AINV,LDA,N,IPVTS)                           
           AINORM=0.0                                                   
           DO 310 J=1,N                                                 
              AIS=SCASUM(N,AINV(1,J),1)                                 
               AINORM = AMAX1(AINORM,AIS)                               
  310       CONTINUE                                                    
            COND1 = ANORM*AINORM                                        
            WRITE (IWRITE,600) COND1                                    
C                                                                       
C           GENERATE RIGHT HAND SIDE FOR BOTH SYMMETRIC AND BAND        
C                                                                       
            CALL CHEML(N,APSAVE,XEXACT,B)                               
            CALL MOVEFD(N,B,X)                                          
            CALL CBAML(N,ML,M,ABSAVE,LDAB,XEXACT,XB)                    
C           SOLVE A*X = B                                               
C                                                                       
            CALL CHELE(N,APSAVE,X,N,1)                                  
            IF (NERROR(IRE).NE.0) CALL ERROFF                           
            CALL CBALE(N,ML,M,ABSAVE,LDAB,XB,N,1)                       
            IF (IRE+NERROR(IRB).EQ.0) GO TO 311                         
               IF (IRB.NE.0) CALL ERROFF                                
               WRITE(IWRITE,580)                                        
               GO TO 530                                                
  311       CONTINUE                                                    
C                                                                       
            IF (N .GT. NPRINT) GO TO 330                                
               WRITE (IWRITE,610)                                       
               DO 320 I = 1, N                                          
                  WRITE (IWRITE,790) X(I), XB(I)                        
  320          CONTINUE                                                 
               WRITE (IWRITE,550)                                       
  330       CONTINUE                                                    
C                                                                       
C                                                                       
C           COMPUTE ERRORS AND RESIDUALS                                
C              E  =  X - XEXACT                                         
C              EB =  XB - XEXACT                                        
C              R  =  B - A*X                                            
C                                                                       
            XNORM = SCASUM(N,X,1)                                       
            ENORM = 0.0E0                                               
            EBNORM = 0.E0                                               
            DO 460 J = 1, N                                             
               ENORM = ENORM + CABS1(X(J)-XEXACT(J))                    
               EBNORM = EBNORM + CABS1(XB(J)-XEXACT(J))                 
               T = -X(J)                                                
               CALL CAXPY(N,T,ASAVE(1,J),1,B,1)                         
  460       CONTINUE                                                    
            RNORM = SCASUM(N,B,1)                                       
C                                                                       
C           A*INV(A) - I                                                
C                                                                       
            AINORM = 0.0E0                                              
            AIBNO = 0.E0                                                
            DO 490 J = 1, N                                             
               DO 470 I = 1, N                                          
                  B(I) =(0.0E0, 0.0E0)                                  
                 XB(I) =(0.0E0, 0.0E0)                                  
  470          CONTINUE                                                 
               DO 480 K = 1, N                                          
                  T = AINV(K,J)                                         
                  CALL CAXPY(N,T,ASAVE(1,K),1,B,1)                      
                  T=AINVB(K,J)                                          
                  CALL CAXPY(N,T,ASAVE(1,K),1,XB,1)                     
  480          CONTINUE                                                 
               B(J) = B(J) - (1.0E0,0.0E0)                              
               XB(J) = XB(J) - (1.0E0,0.0E0)                            
               AINORM = AMAX1(AINORM,SCASUM(N,B,1))                     
               AIBNO = AMAX1(AIBNO,SCASUM(N,XB,1))                      
  490       CONTINUE                                                    
C                                                                       
            WRITE (IWRITE,620) ENORM, EBNORM                            
            WRITE (IWRITE,630) RNORM                                    
            WRITE (IWRITE,740) AINORM,AIBNO                             
C                                                                       
C           COMPUTE TEST RATIOS                                         
C                                                                       
            Q(1) = RCONDP/COND1                                         
            Q(2) = RCONDB/COND1                                         
            Q(3) = COND1/RCONDP                                         
            Q(4) = COND1/RCONDB                                         
            Q(5) = ENORM/(EPS*RCONDP*XNORM)                             
            Q(6) = EBNORM/(EPS*RCONDP*XNORM)                            
            DENOM=AMAX1(100.E0*R1MACH(1),EPS*ANORM*XNORM)               
            Q(7) = RNORM/DENOM                                          
            Q(8) = AINORM/(EPS*RCONDP)                                  
            Q(9) = AIBNO/(EPS*RCONDP)                                   
            WRITE (IWRITE,550)                                          
            WRITE (IWRITE,640)                                          
            WRITE (IWRITE,550)                                          
            WRITE (IWRITE,690)                                          
            WRITE (IWRITE,700)                                          
            WRITE (IWRITE,710)                                          
            WRITE (IWRITE,550)                                          
            WRITE (IWRITE,750) (Q(I), I = 1, 9)                         
            WRITE (IWRITE,550)                                          
C                                                                       
C           LOOK FOR SUSPICIOUS RATIOS                                  
C                                                                       
            QS(1) = 1.0E0 + 4.0E0*EPS                                   
            QS(2) = QS(1)                                               
            QS(3) = 10.0E0                                              
            QS(4) =QS(3)                                                
            EN = FLOAT(N)                                               
            IF (N .EQ. 1) EN = 2.0E0                                    
            DO 500 I=5,9                                                
               QS(I) = EN                                               
  500       CONTINUE                                                    
            KOUNT = 0                                                   
            DO 520 I = 1, 9                                             
               IQ(I) = 0                                                
               IF (Q(I) .LE. QS(I)) GO TO 510                           
                  IQ(I) = 1                                             
                  KSUSP(I) = KSUSP(I) + 1                               
                  KOUNT = KOUNT + 1                                     
  510          CONTINUE                                                 
  520       CONTINUE                                                    
            IF (KOUNT .EQ. 0) WRITE (IWRITE,980)                        
            IF (KOUNT .NE. 0) WRITE (IWRITE,990) (IQ(I), I = 1,9)       
            WRITE (IWRITE,550)                                          
  530    CONTINUE                                                       
C                                                                       
         WRITE (IWRITE,650)                                             
         KASE = KASE + 1                                                
      GO TO 20                                                          
  540 CONTINUE                                                          
C                                                                       
C     FINISH MAIN LOOP                                                  
C                                                                       
C     SUMMARY                                                           
C                                                                       
      WRITE (IWRITE,660)                                                
      KASE = KASE - 1                                                   
      WRITE (IWRITE,670) KASE                                           
      WRITE (IWRITE,680) KSUSP                                          
      WRITE (IWRITE,910)                                                
      RETURN                                                            
C                                                                       
C     MOST FORMATS, ALSO SOME IN CSIXX                                  
C                                                                       
  550 FORMAT (1H )                                                      
 560  FORMAT(22HPORT TESTER,CHE**CBA**)                                 
  570 FORMAT ( / 14H EPSILON     =, 1PE17.5)                            
  580 FORMAT ( / 16H MAYBE SINGULAR. /)                                 
  600 FORMAT (14H ACTUAL COND =, 1PE17.5)                               
  610 FORMAT ( / 4H X =)                                                
  620 FORMAT (14H ERROR NORM  =, 1P2E17.5)                              
  630 FORMAT (14H RESID NORM  =, 1P1E17.5)                              
  640 FORMAT (26H TEST RATIOS.. E = EPSILON)                            
  650 FORMAT ( / 14H ************* /)                                   
  660 FORMAT (8H1SUMMARY)                                               
  670 FORMAT (18H NUMBER OF TESTS =, I4)                                
  680 FORMAT ( / 30H NUMBER OF SUSPICIOUS RATIOS =, 10I4)               
  690 FORMAT( 42H    COND  COND(B)  ACTUAL  ACTUAL  ERROR  ,            
     1        34HERROR(B)  RESID   A*AI-I A*AI-I(B))                    
  700 FORMAT (10(8H   -----))                                           
 710  FORMAT(42H    ACTUAL ACTUAL   COND  COND(B) E*COND*X,             
     1       34H E*COND*X E*A*X    E*COND  E*COND )                     
  720 FORMAT (14H NORM(A)     =, 1PE17.5)                               
  730 FORMAT (14H NORM(A-RT*R)=, 1PE17.5)                               
  740 FORMAT (14H NORM(A*AI-I)=, 1P2E17.5)                              
  750 FORMAT (10(1X, F7.2))                                             
 760  FORMAT(1H ,E17.4)                                                 
  780 FORMAT (2G14.6)                                                   
  790 FORMAT (2G14.6)                                                   
  830 FORMAT ( / 28H BAND ROUTINES DO NOT AGREE,)                       
  840 FORMAT (5H M  =, I2)                                              
  910 FORMAT ( / 12H END OF TEST)                                       
  930 FORMAT (8H RCOND =, 1P3E17.5)                                     
  980 FORMAT (21H NO SUSPICIOUS RATIOS)                                 
  990 FORMAT (I8, 5I10 / 7X, 28H1 INDICATES SUSPICIOUS RATIO)           
 1000 FORMAT (29H THIS VERSION DATED 09/21/78.)                         
      END                                                               
      SUBROUTINE CSIXX(A,LDA,N,KASE,IWRITE)                             
      INTEGER LDA,N,KASE,IWRITE                                         
C/6S                                                                    
C     COMPLEX A(LDA,1)                                                  
C/7S                                                                    
      COMPLEX A(LDA,*)                                                  
C/                                                                      
C                                                                       
C     GENERATES COMPLEX SYMMETRIC INDEFINITE TEST MATRICES              
C                                                                       
C     EXTERNAL R1MACH                                                   
C     FORTRAN CABS,CMPLX,FLOAT,IABS                                     
      COMPLEX T                                                         
      REAL TINY,HUGE,R1MACH                                             
      INTEGER I,J                                                       
C                                                                       
      GO TO (10, 10, 10, 50, 50, 70, 70, 70, 110, 150, 190, 230, 250,   
     *       290, 330, 330, 380, 430, 480), KASE                        
C                                                                       
C     KASE 1, 2 AND 3                                                   
C                                                                       
   10 CONTINUE                                                          
         N = 5*KASE                                                     
         WRITE (IWRITE,20) KASE,N                                       
   20    FORMAT (5H KASE, I3, 3X, 16HCOMPLEX HILBERT  / 4H N =, I4)     
         T = (1.0E0,1.0E0)                                              
         T = T/CABS(T)                                                  
         DO 40 J = 1, N                                                 
            DO 30 I = 1, J                                              
               A(I,J) = T**(J - I)/CMPLX(FLOAT(I+J-1),0.0E0)            
              A(J,I) = CONJG( A(I,J) )                                  
C              FOR NON-C0MPLEX MATRICES, A(I,J) = 1.0/FLOAT(I+J-1)      
   30       CONTINUE                                                    
   40    CONTINUE                                                       
      GO TO 490                                                         
C                                                                       
C     KASE 4 AND 5                                                      
C                                                                       
   50 CONTINUE                                                          
         N = 1                                                          
         WRITE (IWRITE,60) KASE,N                                       
   60    FORMAT (5H KASE, I3, 3X, 16HMONOELEMENTAL    / 4H N =, I4)     
         IF (KASE .EQ. 4) A(1,1) = (3.0E0,0.0E0)                        
         IF (KASE .EQ. 5) A(1,1) = (0.0E0,0.0E0)                        
      GO TO 490                                                         
C                                                                       
C     KASE 6, 7 AND 8                                                   
C                                                                       
   70 CONTINUE                                                          
         N = 15                                                         
         WRITE (IWRITE,80) KASE,N                                       
   80    FORMAT (5H KASE, I3, 3X, 16HTRIDIAGONAL      / 4H N =, I4)     
         T = (1.0E0,0.0E0)                                              
         IF (KASE .EQ. 7) T = (3.0E0,1.0E0)                             
         IF (KASE .EQ. 8) T = (100.0E0,100.0E0)                         
         DO 100 J = 1, N                                                
            DO 90 I = 1, J                                              
               A(I,J) = (0.0E0,0.0E0)                                   
               IF (I .EQ. J) A(I,I) = (4.0E0,0.0E0)                     
               IF (I .EQ. J - 1) A(I,J) = T                             
              A(J,I) = CONJG( A(I,J) )                                  
   90       CONTINUE                                                    
  100    CONTINUE                                                       
      GO TO 490                                                         
C                                                                       
C     KASE 9                                                            
C                                                                       
  110 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,120) KASE,N                                      
  120    FORMAT (5H KASE, I3, 3X, 16HPENTADIAGONAL    / 4H N =, I4)     
         DO 140 J = 1, N                                                
            DO 130 I = 1, J                                             
               A(I,J) = (0.0E0,0.0E0)                                   
               IF (I .GE. J - 2)                                        
     *            A(I,J) = CMPLX((5.0E0-FLOAT(IABS(I-J)))**(10-I-J),    
     *                           0.0E0)                                 
              A(J,I) = CONJG( A(I,J) )                                  
  130       CONTINUE                                                    
  140    CONTINUE                                                       
      GO TO 490                                                         
C                                                                       
C     KASE 10                                                           
C                                                                       
  150 CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,160) KASE,N                                      
  160    FORMAT (5H KASE, I3, 3X, 16HTRIDIAG INVERSE  / 4H N =, I4)     
         DO 180 J = 1, N                                                
            DO 170 I = 1, J                                             
               A(I,J) = CMPLX(FLOAT(N+1-J),0.0E0)                       
              A(J,I) = CONJG( A(I,J) )                                  
  170       CONTINUE                                                    
  180    CONTINUE                                                       
      GO TO 490                                                         
C                                                                       
C     KASE 11                                                           
C                                                                       
  190 CONTINUE                                                          
         N = 10                                                         
         WRITE (IWRITE,200) KASE,N                                      
  200    FORMAT (5H KASE, I3, 3X, 16HZERO DIAGONAL    / 4H N =, I4)     
         DO 220 J = 1, N                                                
            DO 210 I = 1, J                                             
               A(I,J) = (0.0E0,0.0E0)                                   
               IF (I .EQ. J - 1) A(I,J) = (1.0E0,1.0E0)                 
              A(J,I) = CONJG( A(I,J) )                                  
  210       CONTINUE                                                    
  220    CONTINUE                                                       
      GO TO 490                                                         
C                                                                       
C     KASE 12                                                           
C                                                                       
  230 CONTINUE                                                          
         N = 2                                                          
         WRITE (IWRITE,240) KASE,N                                      
  240    FORMAT (5H KASE, I3, 3X, 16HTWO BY TWO       / 4H N =, I4)     
         A(1,1) = (4.0E0,0.0E0)                                         
         A(1,2) = (1.0E0,2.0E0)                                         
         A(2,1) = CONJG(A(1,2))                                         
         A(2,2) = (0.0E0,0.0E0)                                         
      GO TO 490                                                         
C                                                                       
C     KASE 13                                                           
C                                                                       
  250 CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,260) KASE,N                                      
  260    FORMAT (5H KASE, I3, 3X, 16H ZERO MATRIX     / 4H N =, I4)     
         DO 280 J = 1, N                                                
            DO 270 I = 1, J                                             
               A(I,J) = (0.0E0,0.0E0)                                   
              A(J,I) = CONJG( A(I,J) )                                  
  270       CONTINUE                                                    
  280    CONTINUE                                                       
      GO TO 490                                                         
C                                                                       
C     KASE 14                                                           
C                                                                       
  290 CONTINUE                                                          
         N = 3                                                          
         WRITE (IWRITE,300) KASE,N                                      
  300    FORMAT (5H KASE, I3 / 4H N =, I4)                              
         DO 320 I = 1, N                                                
            DO 310 J = 1, N                                             
               A(I,J) = (0.0E0,0.0E0)                                   
  310       CONTINUE                                                    
  320    CONTINUE                                                       
         A(1,3) = (1.0E0,0.0E0)                                         
         A(3,1) = (1.0E0,0.0E0)                                         
      GO TO 490                                                         
C                                                                       
C     KASE 15 AND 16                                                    
C                                                                       
  330 CONTINUE                                                          
         N = 15                                                         
         WRITE (IWRITE,340) KASE,N                                      
  340    FORMAT (5H KASE, I3, 3X, 16H                 / 4H N =, I4)     
         DO 370 J = 1, N                                                
            DO 360 I = 1, J                                             
               A(I,J) = (-1.0E0,1.0E0)                                  
              A(J,I) = CONJG( A(I,J) )                                  
               IF (I .NE. J) GO TO 350                                  
                  IF (KASE .EQ. 15) A(I,I) = (26.0E0,0.0E0)             
                  IF (KASE .EQ. 16) A(I,I) = CMPLX(FLOAT(I),0.0E0)      
  350          CONTINUE                                                 
  360       CONTINUE                                                    
  370    CONTINUE                                                       
      GO TO 490                                                         
C                                                                       
C     KASE 17                                                           
C                                                                       
  380 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,390) KASE,N                                      
  390    FORMAT (5H KASE, I3, 3X, 16HNEAR UNDERFLOW   / 4H N =, I4)     
         TINY = R1MACH(1)*FLOAT(100*N*N)                                
         WRITE (IWRITE,400) TINY                                        
  400    FORMAT (14H TINY        =, 1PE17.5)                            
         DO 420 J = 1, N                                                
            DO 410 I = 1, J                                             
               A(I,J) = TINY                                            
     *                  *CMPLX(FLOAT(IABS(I-J))/FLOAT(I+J),FLOAT(I-J))  
              A(J,I) = CONJG( A(I,J) )                                  
  410       CONTINUE                                                    
  420    CONTINUE                                                       
      GO TO 490                                                         
C                                                                       
C     KASE 18                                                           
C                                                                       
  430 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,440) KASE,N                                      
  440    FORMAT (5H KASE, I3, 3X, 16HNEAR OVERFLOW    / 4H N =, I4)     
         HUGE = SQRT(R1MACH(2))/FLOAT(100*N*N)                          
         WRITE (IWRITE,450) HUGE                                        
  450    FORMAT (14H HUGE        =, 1PE17.5)                            
         DO 470 J = 1, N                                                
            DO 460 I = 1, J                                             
               A(I,J) = HUGE                                            
     *                  *CMPLX(FLOAT(IABS(I-J))/FLOAT(I+J),FLOAT(I-J))  
              A(J,I) = CONJG( A(I,J) )                                  
  460       CONTINUE                                                    
  470    CONTINUE                                                       
      GO TO 490                                                         
C                                                                       
  480 CONTINUE                                                          
         N = 0                                                          
  490 CONTINUE                                                          
      RETURN                                                            
C                                                                       
      END                                                               
C$TEST LYSC                                                             
C***********************************************************************
C                                                                       
C  SECOND TEST OF THE PORT PROGRAMS CHECE AND FRIENDS                   
C                                                                       
C***********************************************************************
C     MAIN PROGRAM                                                      
      INTEGER IWRITE                                                    
C     ALLOW 5000 UNDERFLOWS.                                            
C                                                                       
C     OUTPUT UNIT NUMBER                                                
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      CALL SPOTS(IWRITE)                                                
      STOP                                                              
      END                                                               
      SUBROUTINE SPOTS(IWRITE)                                          
C     IWRITE IS THE OUTPUT UNIT NUMBER                                  
C                                                                       
C     TESTS                                                             
C        CHECE;CHEFBS;CHEML;CHELE;CBPCE;CBPFS;CBPBS;CBPML;CBPLE         
C                                                                       
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     PORT CHECE,CHEFBS,CHEML,CHELE,CBPCE,CBPFS,CBPBS,CBPML,CBPLE       
C     EXTERNAL CPOXX,R1MACH                                             
C     BLAS CAXPY,SCASUM                                                 
C     FORTRAN CABS1,AMAX1,FLOAT,MAX0                                    
C                                                                       
C     INTERNAL VARIABLES                                                
C                                                                       
      COMPLEX APSAVE(120),AB(15,15),AINV(15,15),ASAVE(15,15)            
      COMPLEX AP(120),B(15),X(15),XB(15),XEXACT(15)                     
      COMPLEX ABSAVE(15,15)                                             
      COMPLEX XP(15),T,Z(15)                                            
      REAL ANORM,AINORM,COND,COND1,AIBNO,EBNORM                         
      REAL EN,ENORM,EPS,FNORM,Q(10),QS(10),RCOND,RCONDB                 
      REAL RCONDP,RNORM,SCASUM,R1MACH,XNORM                             
      COMPLEX AINVB(15,15),SC                                           
      REAL AIS                                                          
      INTEGER IPVT(15),IPVTS(15)                                        
      INTEGER I,IQ(10),I1,J,JB                                          
      INTEGER K,KASE,KB,KBFAIL,KNPD,KOUNT,KPFAIL                        
      INTEGER KSUSP(10),LDA,IWRITE,M,N,NPRINT                           
      LOGICAL KBF,KPF                                                   
C                                                                       
      LDA = 15                                                          
C                                                                       
C     WRITE MATRIX AND SOLUTIONS IF  N .LE. NPRINT                      
C                                                                       
      NPRINT = 3                                                        
C                                                                       
      WRITE (IWRITE,560)                                                
      WRITE (IWRITE,1000)                                               
C                                                                       
      DO 10 I = 1,10                                                    
         KSUSP(I) = 0                                                   
   10 CONTINUE                                                          
      KNPD = 0                                                          
      KPFAIL = 0                                                        
      KBFAIL = 0                                                        
C                                                                       
C     SET EPS TO ROUNDING UNIT FOR REAL ARITHMETIC                      
C                                                                       
      EPS = R1MACH(4)                                                   
      WRITE (IWRITE,570) EPS                                            
      WRITE (IWRITE,550)                                                
C                                                                       
        CALL ENTER(1)                                                   
C     START MAIN LOOP                                                   
C                                                                       
      KASE = 1                                                          
   20 CONTINUE                                                          
C                                                                       
C        GENERATE TEST MATRIX                                           
C                                                                       
         CALL CPOXX(ASAVE,LDA,N,KASE,IWRITE)                            
C                                                                       
C        N = 0 SIGNALS NO MORE TEST MATRICES                            
C                                                                       
C     ...EXIT                                                           
         IF (N .LE. 0) GO TO 540                                        
         ANORM = 0.0E0                                                  
         DO 30 J = 1, N                                                 
            ANORM = AMAX1(ANORM,SCASUM(N,ASAVE(1,J),1))                 
   30    CONTINUE                                                       
         WRITE (IWRITE,720) ANORM                                       
C                                                                       
         IF (N .GT. NPRINT) GO TO 50                                    
            WRITE (IWRITE,550)                                          
            DO 40 I = 1, N                                              
               WRITE (IWRITE,760) (ASAVE(I,J), J = 1, N)                
   40       CONTINUE                                                    
            WRITE (IWRITE,550)                                          
   50    CONTINUE                                                       
C                                                                       
C        GENERATE EXACT SOLUTION                                        
C                                                                       
         XEXACT(1) = (1.0,0.0)                                          
         IF (N .GE. 2) XEXACT(2) = (0.0,1.0)                            
         IF (N .LE. 2) GO TO 70                                         
            DO 60 I = 3, N                                              
               XEXACT(I) = -XEXACT(I-2)                                 
   60       CONTINUE                                                    
   70    CONTINUE                                                       
C                                                                       
C                                                                       
C      PUT INTO PACKED FORM                                             
         K = 0                                                          
         DO 130 J = 1, N                                                
            DO 120 I = J,N                                              
               K = K + 1                                                
               AP(K) = ASAVE(I,J)                                       
               APSAVE(K)=AP(K)                                          
  120       CONTINUE                                                    
  130    CONTINUE                                                       
         CALL CHECE(N,AP,IPVTS,RCONDP)                                  
         IF (NERROR(IERR).NE.0) CALL ERROFF                             
C        FACTOR BAND FORM AND COMPARE                                   
C                                                                       
         KBF = .FALSE.                                                  
         M = 0                                                          
         DO 200 J = 1, N                                                
            DO 190 I = 1, J                                             
               IF (CABS1(ASAVE(I,J)) .NE. 0.0E0) M = MAX0(M,J-I)        
  190       CONTINUE                                                    
  200    CONTINUE                                                       
         MP1 =M+1                                                       
C                                                                       
         DO 220 J = 1, N                                                
             I1=MIN0(N,J+M)                                             
            DO 210 I = J, I1                                            
               K =I-J+1                                                 
               AB(K,J) = ASAVE(I,J)                                     
               ABSAVE(K,J)=AB(K,J)                                      
  210       CONTINUE                                                    
  220    CONTINUE                                                       
         WRITE (IWRITE,840) M                                           
        CALL CBPCE(N,MP1,AB,LDA,RCONDB)                                 
           IF (NERROR(IERR).EQ.0) GO TO 230                             
          CALL ERROFF                                                   
          IF ((IERR).LT.10+N)GO TO 226                                  
             WRITE(IWRITE,860)IERR                                      
             J = IERR - N - 10                                          
             WRITE(IWRITE,229)AB(1,J)                                   
 229          FORMAT(19H OFFENDING DIAGONAL,E15.7)                      
             GO TO 530                                                  
 226         WRITE(IWRITE,580)                                          
            WRITE (IWRITE,930) RCONDP,RCONDB                            
             GO TO 530                                                  
 230       CONTINUE                                                     
            WRITE(IWRITE,930)RCONDP,RCONDB                              
C                                                                       
C           COMPUTE INVERSE AND COND1 = TRUE CONDITION                  
C                                                                       
            DO 290 J = 1, N                                             
               DO 280 I = 1, N                                          
                   AINV(I,J)=(0.0,0.0)                                  
                   AINVB(I,J)=(0.0,0.0)                                 
  280          CONTINUE                                                 
               AINV(J,J)=(1.0,0.0)                                      
               AINVB(J,J)=(1.0,0.0)                                     
  290       CONTINUE                                                    
           CALL CBPFS(N,MP1,AB,LDA,AINVB,LDA,N)                         
            CALL CBPBS(N,MP1,AB,LDA,AINVB,LDA,N)                        
           CALL CHEFBS(N,AP,AINV,LDA,N,IPVTS)                           
           AINORM=0.0                                                   
           DO 310 J=1,N                                                 
              AIS=SCASUM(N,AINV(1,J),1)                                 
               AINORM = AMAX1(AINORM,AIS)                               
  310       CONTINUE                                                    
            COND1 = ANORM*AINORM                                        
            WRITE (IWRITE,600) COND1                                    
C                                                                       
C           GENERATE RIGHT HAND SIDE FOR BOTH SYMMETRIC AND BAND        
C                                                                       
            CALL CHEML(N,APSAVE,XEXACT,B)                               
            CALL MOVEFD(N,B,X)                                          
            CALL CBPML(N,MP1,ABSAVE,LDA,XEXACT,XB)                      
C           SOLVE A*X = B                                               
C                                                                       
            CALL CHELE(N,APSAVE,X,N,1)                                  
            IF (NERROR(IRE).NE.0) CALL ERROFF                           
            CALL CBPLE(N,MP1,ABSAVE,LDA,XB,N,1)                         
            IF (IRE+NERROR(IRB).EQ.0) GO TO 311                         
               IF (IRB.NE.0) CALL ERROFF                                
               WRITE(IWRITE,580)                                        
               GO TO 530                                                
  311       CONTINUE                                                    
C                                                                       
            IF (N .GT. NPRINT) GO TO 330                                
               WRITE (IWRITE,610)                                       
               DO 320 I = 1, N                                          
                  WRITE (IWRITE,790) X(I), XB(I)                        
  320          CONTINUE                                                 
               WRITE (IWRITE,550)                                       
  330       CONTINUE                                                    
C                                                                       
C                                                                       
C           COMPUTE ERRORS AND RESIDUALS                                
C              E  =  X - XEXACT                                         
C              EB =  XB - XEXACT                                        
C              R  =  B - A*X                                            
C              F  =  A - TRANS(R)*R                                     
C                                                                       
            XNORM = SCASUM(N,X,1)                                       
            ENORM = 0.0E-0                                              
            EBNORM = 0.E0                                               
            DO 460 J = 1, N                                             
               ENORM = ENORM + CABS1(X(J)-XEXACT(J))                    
               EBNORM = EBNORM + CABS1(XB(J)-XEXACT(J))                 
               T = -X(J)                                                
               CALL CAXPY(N,T,ASAVE(1,J),1,B,1)                         
  460       CONTINUE                                                    
            RNORM = SCASUM(N,B,1)                                       
C                                                                       
C           A*INV(A) - I                                                
C                                                                       
            AINORM = 0.0E0                                              
            AIBNO = 0.E0                                                
            DO 490 J = 1, N                                             
               DO 470 I = 1, N                                          
                  B(I) = (0.0,0.0)                                      
                 XB(I) = (0.0,0.0)                                      
  470          CONTINUE                                                 
               DO 480 K = 1, N                                          
                  T = AINV(K,J)                                         
                  CALL CAXPY(N,T,ASAVE(1,K),1,B,1)                      
                  T=AINVB(K,J)                                          
                  CALL CAXPY(N,T,ASAVE(1,K),1,XB,1)                     
  480          CONTINUE                                                 
               B(J) = B(J) - (1.0,0.0)                                  
               XB(J) = XB(J) - (1.0,0.0)                                
               AINORM = AMAX1(AINORM,SCASUM(N,B,1))                     
               AIBNO = AMAX1(AIBNO,SCASUM(N,XB,1))                      
  490       CONTINUE                                                    
            FNORM = 0.0E0                                               
            ML=M+1                                                      
            NP1=N+1                                                     
            MLP1=ML+1                                                   
            DO 495 J=1,N                                                
               NUMAX= MIN0(ML,NP1-J)                                    
               JM1=J-1                                                  
               IEND=JM1+NUMAX                                           
               DO 491 I=J,IEND                                          
                  B(I)=-ASAVE(I,J)                                      
 491           CONTINUE                                                 
               KBEGIN=MAX0(1,J-M)                                       
               L=MIN0(J,ML)                                             
               NUM=MLP1-L                                               
               IF (JM1.LT.KBEGIN) GO TO 493                             
               DO 492 K=KBEGIN,JM1                                      
                  SC=(AB(L,K))*AB(1,K)                                  
                  IEND=JM1+NUM                                          
                  LL=L                                                  
                  IF(IEND.LT.J) GO TO 4912                              
                  DO 4911 I=J,IEND                                      
                      B(I)=B(I)+SC*CONJG(AB(LL,K))                      
                      LL=LL+1                                           
 4911             CONTINUE                                              
 4912             L=L-1                                                 
                  NUM=MIN0(NUM+1,NUMAX)                                 
 492           CONTINUE                                                 
 493           SC=AB(1,J)                                               
                  IEND=JM1+NUM                                          
                  JP1=J+1                                               
                  IF (IEND.LT.JP1) GO TO 4941                           
                  L=2                                                   
                  DO 494 I=JP1,IEND                                     
                     B(I)=B(I)+SC*CONJG(AB(L,J))                        
                     L=L+1                                              
 494              CONTINUE                                              
 4941          B(J)=B(J)+AB(1,J)                                        
               FNORM=AMAX1(FNORM,SCASUM(NUMAX,B(J),1))                  
 495       CONTINUE                                                     
C                                                                       
            WRITE (IWRITE,620) ENORM, EBNORM                            
            WRITE (IWRITE,630) RNORM                                    
            WRITE (IWRITE,730) FNORM                                    
            WRITE (IWRITE,740) AINORM,AIBNO                             
C                                                                       
C           COMPUTE TEST RATIOS                                         
C                                                                       
            Q(1) = RCONDP/COND1                                         
            Q(2) = RCONDB/COND1                                         
            Q(3) = COND1/RCONDP                                         
            Q(4) = COND1/RCONDB                                         
            Q(5) = ENORM/(EPS*RCONDP*XNORM)                             
            Q(6) = EBNORM/(EPS*RCONDP*XNORM)                            
            DENOM=AMAX1(EPS*ANORM*XNORM,100.0*R1MACH(1))                
            Q(7) = RNORM/DENOM                                          
            DENOM=AMAX1(EPS*ANORM,100.0*R1MACH(1))                      
            Q(8) = FNORM/DENOM                                          
            Q(9) = AINORM/(EPS*RCONDP)                                  
            Q(10) = AIBNO/(EPS*RCONDP)                                  
            WRITE (IWRITE,550)                                          
            WRITE (IWRITE,640)                                          
            WRITE (IWRITE,550)                                          
            WRITE (IWRITE,690)                                          
            WRITE (IWRITE,700)                                          
            WRITE (IWRITE,710)                                          
            WRITE (IWRITE,550)                                          
            WRITE (IWRITE,750) (Q(I), I = 1, 10)                        
            WRITE (IWRITE,550)                                          
C                                                                       
C           LOOK FOR SUSPICIOUS RATIOS                                  
C                                                                       
            QS(1) = 1.0E0 + 4.0E0*EPS                                   
            QS(2) = QS(1)                                               
            QS(3) = 10.0E0                                              
            QS(4) =QS(3)                                                
            EN = DBLE(FLOAT(N))                                         
            IF (N .EQ. 1) EN = 2.0E0                                    
            DO 500 I=5,10                                               
               QS(I) = EN                                               
  500       CONTINUE                                                    
            KOUNT = 0                                                   
            DO 520 I = 1, 10                                            
               IQ(I) = 0                                                
               IF (Q(I) .LE. QS(I)) GO TO 510                           
                  IQ(I) = 1                                             
                  KSUSP(I) = KSUSP(I) + 1                               
                  KOUNT = KOUNT + 1                                     
  510          CONTINUE                                                 
  520       CONTINUE                                                    
            IF (KOUNT .EQ. 0) WRITE (IWRITE,980)                        
            IF (KOUNT .NE. 0) WRITE (IWRITE,990) (IQ(I), I = 1,10)      
            WRITE (IWRITE,550)                                          
  530    CONTINUE                                                       
C                                                                       
         WRITE (IWRITE,650)                                             
         KASE = KASE + 1                                                
      GO TO 20                                                          
  540 CONTINUE                                                          
C                                                                       
C     FINISH MAIN LOOP                                                  
C                                                                       
C     SUMMARY                                                           
C                                                                       
      WRITE (IWRITE,660)                                                
      KASE = KASE - 1                                                   
      WRITE (IWRITE,670) KASE                                           
      WRITE (IWRITE,900) KNPD                                           
      WRITE (IWRITE,680) KSUSP                                          
      WRITE (IWRITE,910)                                                
      RETURN                                                            
C                                                                       
C     MOST FORMATS, ALSO SOME IN CPOXX                                  
C                                                                       
  550 FORMAT (1H )                                                      
 560  FORMAT(22HPORT TESTER,CHE**CBP**)                                 
  570 FORMAT ( / 14H EPSILON     =, 1PE17.5)                            
  580 FORMAT ( / 16H MAYBE SINGULAR. /)                                 
  600 FORMAT (14H ACTUAL COND =, 1PE17.5)                               
  610 FORMAT ( / 4H X =)                                                
  620 FORMAT (14H ERROR NORM  =, 1P2E17.5)                              
  630 FORMAT (14H RESID NORM  =, 1P1E17.5)                              
  640 FORMAT (26H TEST RATIOS.. E = EPSILON)                            
  650 FORMAT ( / 14H ************* /)                                   
  660 FORMAT (8H1SUMMARY)                                               
  670 FORMAT (18H NUMBER OF TESTS =, I4)                                
  680 FORMAT ( / 30H NUMBER OF SUSPICIOUS RATIOS =, 10I4)               
  690 FORMAT( 42H    COND  COND(B)  ACTUAL  ACTUAL  ERROR  ,            
     1        40HERROR(B)  RESID  A-RT*R A*AI-I A*AI-I(B))              
  700 FORMAT (10(8H   -----))                                           
 710  FORMAT(42H    ACTUAL ACTUAL   COND  COND(B) E*COND*X,             
     1       40H E*COND*X E*A*X    E*A   E*COND  E*COND )               
  720 FORMAT (14H NORM(A)     =, 1PE17.5)                               
  730 FORMAT (14H NORM(A-RT*R)=, 1PE17.5)                               
  740 FORMAT (14H NORM(A*AI-I)=, 1P2E17.5)                              
  750 FORMAT (10(1X, F7.2))                                             
  760 FORMAT(1H ,3E17.4)                                                
  780 FORMAT (2G14.6)                                                   
  790 FORMAT (2G14.6)                                                   
  830 FORMAT ( / 28H BAND ROUTINES DO NOT AGREE,)                       
  840 FORMAT (5H M  =, I2)                                              
  860 FORMAT (30H NOT POSITIVE DEFINITE, INFO =, I2)                    
  900 FORMAT (34H NUMBER OF NOT POSITIVE DEFINITE =, I4)                
  910 FORMAT ( / 12H END OF TEST)                                       
  930 FORMAT (8H RCOND =, 1P3E17.5)                                     
  980 FORMAT (21H NO SUSPICIOUS RATIOS)                                 
  990 FORMAT (I8, 5I10 / 7X, 28H1 INDICATES SUSPICIOUS RATIO)           
 1000 FORMAT (29H THIS VERSION DATED 09/21/78.)                         
      END                                                               
      SUBROUTINE CPOXX(A,LDA,N,KASE,IWRITE)                             
      INTEGER LDA,N,KASE,IWRITE                                         
      COMPLEX A(LDA,1)                                                  
C                                                                       
C     GENERATES COMPLEX POSITIVE DEFINITE TEST MATRICES                 
C                                                                       
C     EXTERNAL CMACH                                                    
C     FORTRAN CABS,CMPLX,CONJG,FLOAT,IABS,MAX0,MIN0                     
      COMPLEX T                                                         
      REAL TINY,HUGE,CMACH                                              
      INTEGER I,J                                                       
C                                                                       
      GO TO (10, 10, 10, 50, 50, 70, 70, 70, 120, 160, 200, 240, 290,   
     *       340), KASE                                                 
C                                                                       
C     KASE 1, 2 AND 3                                                   
C                                                                       
   10 CONTINUE                                                          
         N = 5*KASE                                                     
         WRITE (IWRITE,20) KASE,N                                       
   20    FORMAT (5H KASE, I3, 3X, 16HHILBERT          / 4H N =, I4)     
         T = (1.0E0,1.0E0)                                              
         T = T/CABS(T)                                                  
         DO 40 J = 1, N                                                 
            DO 30 I = 1, N                                              
               A(I,J) = T**(J - I)/CMPLX(FLOAT(I+J-1),0.0E0)            
C              FOR REAL MATRICES, A(I,J) = 1.0/FLOAT(I+J-1)             
   30       CONTINUE                                                    
   40    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 4 AND 5                                                      
C                                                                       
   50 CONTINUE                                                          
         N = 1                                                          
         WRITE (IWRITE,60) KASE,N                                       
   60    FORMAT (5H KASE, I3, 3X, 16HMONOELEMENTAL    / 4H N =, I4)     
         IF (KASE .EQ. 4) A(1,1) = (3.0E0,0.0E0)                        
         IF (KASE .EQ. 5) A(1,1) = (0.0E0,0.0E0)                        
      GO TO 350                                                         
C                                                                       
C     KASE 6, 7 AND 8                                                   
C                                                                       
   70 CONTINUE                                                          
         N = 15                                                         
         IF (KASE .NE. 8) WRITE (IWRITE,80) KASE,N                      
   80    FORMAT (5H KASE, I3, 3X, 16HTRIDIAGONAL      / 4H N =, I4)     
         IF (KASE .EQ. 8) WRITE (IWRITE,90) KASE,N                      
   90    FORMAT (5H KASE, I3, 3X, 16HDIAGONAL         / 4H N =, I4)     
         T = (1.0E0,0.0E0)                                              
         IF (KASE .EQ. 7) T = (2.0E0,1.0E0)                             
         IF (KASE .EQ. 8) T = (0.0E0,0.0E0)                             
         DO 110 J = 1, N                                                
            DO 100 I = 1, J                                             
               A(I,J) = (0.0E0,0.0E0)                                   
               IF (I .EQ. J) A(I,I) = (4.0E0,0.0E0)                     
               IF (I .EQ. J - 1) A(I,J) = T                             
               A(J,I) = CONJG(A(I,J))                                   
  100       CONTINUE                                                    
  110    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 9                                                            
C                                                                       
  120 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,130) KASE,N                                      
  130    FORMAT (5H KASE, I3, 3X, 16HPENTADIAGONAL    / 4H N =, I4)     
         DO 150 J = 1, N                                                
            DO 140 I = 1, N                                             
               A(I,J) = (0.0E0,0.0E0)                                   
               IF (IABS(I-J) .LE. 2)                                    
     *            A(I,J) = CMPLX((5.0E0-FLOAT(IABS(I-J)))**(10-I-J),    
     *                           0.0E0)                                 
  140       CONTINUE                                                    
  150    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 10                                                           
C                                                                       
  160 CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,170) KASE,N                                      
  170    FORMAT (5H KASE, I3, 3X, 16HTRIDIAG INVERSE  / 4H N =, I4)     
         DO 190 J = 1, N                                                
            DO 180 I = 1, J                                             
               A(I,J) = CMPLX(FLOAT(N+1-J),0.0E0)                       
               A(J,I) = A(I,J)                                          
  180       CONTINUE                                                    
  190    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 11                                                           
C                                                                       
  200 CONTINUE                                                          
         N = 15                                                         
         WRITE (IWRITE,210) KASE,N                                      
  210    FORMAT (5H KASE, I3, 3X, 16HTEST COND        / 4H N =, I4)     
         DO 230 J = 1, N                                                
            DO 220 I = 1, N                                             
               IF (I .EQ. J) A(I,J) = CMPLX(FLOAT(I),0.0E0)             
               IF (I .GT. J) A(I,J) = CMPLX(FLOAT(J-2),0.0E0)           
               IF (I .LT. J) A(I,J) = CMPLX(FLOAT(I-2),0.0E0)           
  220       CONTINUE                                                    
  230    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 12                                                           
C                                                                       
  240 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,250) KASE,N                                      
  250    FORMAT (5H KASE, I3, 3X, 16HNEAR UNDERFLOW   / 4H N =, I4)     
         TINY = 100. * R1MACH(1)                                        
         WRITE (IWRITE,260) TINY                                        
  260    FORMAT (14H TINY        =, 1PE17.5)                            
         DO 280 I = 1, N                                                
            DO 270 J = 1, N                                             
               A(I,J) = CMPLX(TINY*FLOAT(MIN0(I,J))/FLOAT(MAX0(I,J)),   
     *                        0.0E0)                                    
  270       CONTINUE                                                    
  280    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
C     KASE 13                                                           
C                                                                       
  290 CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,300) KASE,N                                      
  300    FORMAT (5H KASE, I3, 3X, 16HNEAR OVERFLOW    / 4H N =, I4)     
         HUGE= SQRT(R1MACH(2))/(FLOAT(100*N*N))                         
         WRITE (IWRITE,310) HUGE                                        
  310    FORMAT (14H HUGE        =, 1PE17.5)                            
         DO 330 I = 1, N                                                
            DO 320 J = 1, N                                             
               A(I,J) = CMPLX(HUGE*FLOAT(MIN0(I,J))/FLOAT(MAX0(I,J)),   
     *                        0.0E0)                                    
  320       CONTINUE                                                    
  330    CONTINUE                                                       
      GO TO 350                                                         
C                                                                       
  340 CONTINUE                                                          
         N = 0                                                          
  350 CONTINUE                                                          
      RETURN                                                            
C                                                                       
      END                                                               
C$TEST PRSA                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT SPARSE MATRIX PACKAGE                               
C                                                                       
C***********************************************************************
C  THIS IS THE TESTER FOR LINDA KAUFMAN'S SPARSE                        
C  MATRIX PACKAGE - SINGLE PRECISION                                    
C                                                                       
C  MARCH 20, 1981                                                       
C                                                                       
C     MAIN PROGRAM                                                      
      INTEGER IWRITE, I1MACH                                            
C     ALLOW 5000 UNDERFLOWS.                                            
C                                                                       
C     SET OUTPUT UNIT NUMBER                                            
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      CALL SGETS(IWRITE)                                                
      STOP                                                              
      END                                                               
      SUBROUTINE SGETS(IWRITE)                                          
C     IWRITE IS THE OUTPUT UNIT NUMBER                                  
C                                                                       
C     TESTS                                                             
C        GECE,GEFS,GEBS,GEML,SPMOR,SPFOR,SPMCE,SPFCE,SPMLE,SPFLE,SPMSL  
C        SPFSL,SPMNF,SPFNF,SPFSF,SPMSF,SPMIN                            
C                                                                       
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     PORT GECE,GEFS,GEBS,GEML,SPMOR,SPFOR,SPMCE,SPFCE,SPMLE,SPFLE      
C     PORT SPMSL,SPFSL,SPMNF,SPFNF,SPMS,SPFSF.SPMIN                     
C     PORT UTILITIES ERROFF,ENTER,LEAVE,R1MACH                          
C     EXTERNAL SGEXX                                                    
C     BLAS SAXPY,SDOT,SSCAL,SASUM                                       
C     FORTRAN ABS,AMAX1,FLOAT,MAX0,MIN0                                 
C                                                                       
C     INTERNAL VARIABLES                                                
C                                                                       
      INTEGER IIA(101), JJA(2500)                                       
      INTEGER JSPSAV(2500),IWORK(2800),IL(101),MR(100),MC(100),FR(100)  
      INTEGER FC(100),MIC(100),JA(2500),IA(101)                         
      INTEGER I,IPVT(100),IQ(8),J                                       
      INTEGER KASE,KBFAIL,KOUNT,KSING,KSUSP(8)                          
      INTEGER LDA,IWRITE,N,NPRINT                                       
      REAL A(100,100),AS(2500),AINV(100,100),ASAVE(100,100),UL(2500)    
      REAL XEXACT(100,2),B(100,2),BM(100,2),BM2(100,2),BF(100,2)        
      REAL Z(100)                                                       
      REAL BF2(100,2)                                                   
      REAL AFNORM,AMNORM,ANRM,CONDF1,CONDF,CONDM1,CONDM,GROWTH,AA(2500) 
      REAL SPSAV(2500)                                                  
      REAL AINORM,ANORM,COND1,EN,EPS, EPS1                              
      REAL RCOND                                                        
      REAL Q(10),QS(10),SASUM                                           
      REAL EB(2),XN(2),EBM(2),EBF(2),EBM2(2),EBF2(2)                    
      COMMON /SPA/ IIA,JJA,AA                                           
      COMMON /CSTAK/ D(6000)                                            
C                                                                       
      EXTERNAL IIROW,AROW                                               
C                                                                       
      CALL ISTKIN(6000,3)                                               
C                                                                       
      LDA = 100                                                         
      IAMAX = 2500                                                      
      MAXUL = 2500                                                      
      IWMAX = 2500                                                      
      CALL ENTER(1)                                                     
C                                                                       
C     WRITE MATRIX AND SOLUTIONS IF  N .LE. NPRINT                      
C                                                                       
      NPRINT =3                                                         
C                                                                       
      WRITE (IWRITE,480)                                                
      WRITE (IWRITE,770)                                                
C                                                                       
      DO 10 I = 1, 8                                                    
         KSUSP(I) = 0                                                   
 10   CONTINUE                                                          
      KSING = 0                                                         
      KBFAIL = 0                                                        
C                                                                       
C     SET EPS TO ROUNDING UNIT                                          
C                                                                       
      EPS =R1MACH(4)                                                    
      WRITE (IWRITE,490) EPS                                            
      WRITE (IWRITE,470)                                                
C                                                                       
C     START MAIN LOOP                                                   
C                                                                       
      KASE = 1                                                          
 20   CONTINUE                                                          
C                                                                       
C        GENERATE TEST MATRIX                                           
C                                                                       
         CALL SGEXX(A,LDA,N,KASE,IWRITE)                                
C                                                                       
C        N = 0 SIGNALS NO MORE TEST MATRICES                            
C                                                                       
C     ...EXIT                                                           
         IF (N .LE. 0) GO TO 460                                        
         ANORM = 0.0E0                                                  
         DO 30 J = 1, N                                                 
            ANORM = AMAX1(ANORM,SASUM(N,A(1,J),1))                      
 30      CONTINUE                                                       
         WRITE (IWRITE,650) ANORM                                       
C                                                                       
         IF (N .GT. NPRINT) GO TO 50                                    
            WRITE (IWRITE,470)                                          
            DO 40 I = 1, N                                              
               WRITE (IWRITE,690) (A(I,J), J = 1, N)                    
 40         CONTINUE                                                    
            WRITE (IWRITE,470)                                          
 50      CONTINUE                                                       
C                                                                       
C        GENERATE EXACT SOLUTION                                        
C                                                                       
         XEXACT(1,1) = 1.0E0                                            
         XEXACT(1,2) = 1.0E0                                            
         XEXACT(2,2) = 1.0E0                                            
         IF (N .GE. 2) XEXACT(2,1) = 0.0E0                              
         IF (N .LE. 2) GO TO 70                                         
            DO 60 I = 3, N                                              
               XEXACT(I,1) = -XEXACT(I-2,1)                             
                XEXACT(I,2) = 1.0E0                                     
 60         CONTINUE                                                    
 70      CONTINUE                                                       
C                                                                       
C        SAVE MATRIX AND GENERATE R.H.S.                                
C                                                                       
         DO 90 I = 1, N                                                 
            DO 80 J = 1, N                                              
               ASAVE(I,J) = A(I,J)                                      
 80         CONTINUE                                                    
 90      CONTINUE                                                       
         CALL GEML(N,A,LDA,XEXACT,B)                                    
         CALL GEML(N,A,LDA,XEXACT(1,2),B(1,2))                          
         NN=LDA*2                                                       
         CALL MOVEFR(NN,B,BM)                                           
         CALL MOVEFR(NN,B,BF)                                           
         CALL MOVEFR(NN,B,BM2)                                          
         CALL MOVEFR(NN,B,BF2)                                          
      CALL GETSP(N,A, LDA, IA,JA,AS,MAXA)                               
      CALL GETSP(N,A,LDA,IIA,JJA,AA,MAXA)                               
         DO 100 I=1,MAXA                                                
            SPSAV(I)=AS(I)                                              
            JSPSAV(I)=JA(I)                                             
 100     CONTINUE                                                       
         NP1=N+1                                                        
C                                                                       
C CHECK ORDERING SUBROUTINES                                            
C                                                                       
         CALL SPMOR(N,IA,JA,MC,MIC)                                     
         CALL SPFOR(N,IIROW,FC)                                         
         IBAD=0                                                         
         DO 120 I=1,N                                                   
            MR(I)=MC(I)                                                 
            FR(I)=FC(I)                                                 
            IF (MC(I).EQ.FC(I)) GO TO 120                               
            IBAD=IBAD+1                                                 
            WRITE(IWRITE,110)I,MC(I),FC(I)                              
 110         FORMAT(19H ORDER DISAGREEMENT,3I5)                         
 120    CONTINUE                                                        
        IF (IBAD.EQ.0)WRITE(IWRITE,130)                                 
 130     FORMAT(28H NO DISAGREEMENT IN ORDERING)                        
C                                                                       
C        FACTOR AND ESTIMATE CONDITION                                  
C                                                                       
         CALL GECE(N,A,LDA,IPVT,RCOND)                                  
         IF (NERROR(INE).NE.0) CALL  ERROFF                             
C                                                                       
C                                                                       
C        FACTOR SPARSE FORM AND COMPARE                                 
C                                                                       
        CALL SPMCE(N,MR,MC,AS,IA,JA,IAMAX,IL,ISIZE,CONDM,Z)             
        IF(NERROR(INM).NE.0) CALL ERROFF                                
         CALL SPFCE(N,FR,FC,AROW,IWORK,UL,MAXUL,ISIZE,CONDF,Z)          
         IF (NERROR(INF).NE.0) CALL ERROFF                              
        WRITE(IWRITE,140)                                               
 140     FORMAT(21H CONDITION ESTIMATION)                               
        WRITE(IWRITE,150)                                               
 150     FORMAT(42H    GENERAL        SPARSE M       SPARSE F)          
        WRITE(IWRITE,160)RCOND,CONDM,CONDF                              
 160    FORMAT(1H ,3E15.5)                                              
C                                                                       
C                                                                       
C           TEST FOR SINGULARITY                                        
C                                                                       
          IF (INE+INM+INF.EQ.0)GO TO 190                                
            WRITE(IWRITE,170)INE,INM,INF                                
 170         FORMAT(13H INE, INM,INF,3I5)                               
 180          FORMAT(25H SPARSE ROUTINES DISAGREE,2I4)                  
               WRITE (IWRITE,500)                                       
               KSING = KSING + 1                                        
            GO TO 440                                                   
 190        CONTINUE                                                    
C                                                                       
C              COMPUTE INVERSE AND COND1 = TRUE CONDITION               
C                                                                       
               DO 210 J = 1, N                                          
                  DO 200 I = 1, N                                       
                     AINV(I,J) = 0.E0                                   
 200              CONTINUE                                              
               AINV(J,J)=1.E0                                           
 210           CONTINUE                                                 
               CALL GEFS(N,A,LDA,AINV,LDA,N,IPVT)                       
               CALL GEBS(N,A,LDA,AINV,LDA,N)                            
               AINORM=ANRM(N,ASAVE,LDA,AINV,LDA, ANORM, COND1)          
               WRITE (IWRITE,520) COND1                                 
               DO 230 I=1,N                                             
                  DO 220 J=1,N                                          
                     AINV(I,J)=0.0E0                                    
 220               CONTINUE                                             
                  AINV(I,I)=1.0E0                                       
 230           CONTINUE                                                 
               CALL SPMSL(N,MR,MC,IA,JA,AS,IL,AINV,LDA,N)               
               AMNORM=ANRM(N,ASAVE,LDA,AINV,LDA,ANORM,CONDM1)           
               WRITE(IWRITE,260)CONDM1                                  
               DO 250 I=1,N                                             
                   DO 240 J=1,N                                         
                      AINV(I,J)=0.0E0                                   
 240               CONTINUE                                             
                   AINV(I,I)=1.0E0                                      
 250           CONTINUE                                                 
               CALL SPFSL(N,FR,FC,IWORK,UL,AINV,LDA,N)                  
               AFNORM=ANRM(N,ASAVE,LDA,AINV,LDA,ANORM,CONDF1)           
               WRITE(IWRITE,270)CONDF1                                  
 260           FORMAT(19H COND FROM AINV*A M,E15.5)                     
 270           FORMAT(19H COND FROM AINV*A F,E15.5)                     
C                                                                       
C              SOLVE  A*X = B                                           
C                                                                       
               CALL GEFS(N,A,LDA,B,LDA,2,IPVT)                          
               CALL GEBS(N,A,LDA,B,LDA,2)                               
C                                                                       
C              MORE SPAR COMPARE                                        
C                                                                       
               DO 280 I=1,N                                             
                  MC(I)=MR(I)                                           
                  FC(I)=FR(I)                                           
 280           CONTINUE                                                 
               CALL SPMSF(N,MR,MIC,IA,JSPSAV,IWORK,IWMAX,IFILL)         
               DO 290 J=1,N                                             
                  I=MR(J)                                               
                  NUM=IA(I+1)-IA(I)                                     
                 IB=IA(I)                                               
                   CALL SPMIN(N,MIC,IWORK,J,SPSAV(IB),JSPSAV(IB),NUM,   
     1          J,UL)                                                   
 290           CONTINUE                                                 
              EPS1=EPS*ANORM                                            
               CALL SPMNF(N,IWORK,UL,EPS1,GROWTH)                       
               IF (NERROR(IMNF).EQ.0) GO TO 300                         
                 CALL ERROFF                                            
                 GO TO 310                                              
 300           CONTINUE                                                 
               CALL SPSOL(N,MR,MC,IWORK,UL,BM,LDA,2)                    
 310           CONTINUE                                                 
               CALL SPFSF(N,FR,FC,IIROW,IWORK,IWMAX,IFILL)              
               CALL SPFNF(N,FR,FC,AROW,IWORK,UL,GROWTH,EPS1)            
               IF (NERROR(IFNF).EQ.0) GO TO 320                         
                  CALL ERROFF                                           
                  GO TO 330                                             
 320           CONTINUE                                                 
               CALL SPSOL(N,FR,FC,IWORK,UL,BF,LDA,2)                    
 330           CONTINUE                                                 
               CALL SPMLE(N,.TRUE.,IA,JSPSAV,SPSAV,ISIZE,BM2,LDA,2)     
               IF(NERROR(IMLE).NE.0)CALL ERROFF                         
               CALL SPFLE(N,.TRUE.,AROW,ISIZE,BF2,LDA,2)                
                IF (NERROR(IFLE).NE.0) CALL ERROFF                      
C COMPUTE ERRORS IN SOLUTION WITH 2 RIGHT HAND SIDES                    
               IF(IFLE+IMLE+IMNF+IFNF.EQ.0)GO TO 350                    
                 WRITE(IWRITE,340)IMNF,IFNF,IMLE,IFLE                   
 340    FORMAT(42H NUMERICALLY SINGULAR- IMNF,IFNF,IMLE,IFLE,4I5)       
                GO TO 440                                               
 350           CONTINUE                                                 
               DO 360 I=1,2                                             
                  XN(I)=SASUM(N,B(1,I),1)                               
                  CALL SAXPY(N,-1.0,XEXACT(1,I),1,B(1,I),1)             
                  CALL SAXPY(N,-1.0,XEXACT(1,I),1,BM(1,I),1)            
                  CALL SAXPY(N,-1.0,XEXACT(1,I),1,BF(1,I),1)            
                  CALL SAXPY(N,-1.0,XEXACT(1,I),1,BM2(1,I),1)           
                  CALL SAXPY(N,-1.0,XEXACT(1,I),1,BF2(1,I),1)           
                  EB(I)=SASUM(N,B(1,I),1)                               
                  EBM(I)=SASUM(N,BM(1,I),1)                             
                  EBF(I)=SASUM(N,BF(1,I),1)                             
                  EBM2(I)=SASUM(N,BM2(1,I),1)                           
                  EBF2(I)=SASUM(N,BF2(1,I),1)                           
 360              CONTINUE                                              
                  WRITE(IWRITE,370)                                     
 370              FORMAT(29H ERRORS IN SOLUTION -ABSOLUTE)              
                  WRITE(IWRITE,380)                                     
 380           FORMAT(15X,43H GENERAL SPMN      SPFN       SPMLE   SPFLE
     *)                                                                 
                  DO 400 I=1,2                                          
                  WRITE(IWRITE,390)I,EB(I),EBM(I),EBF(I),EBM2(I),EBF2(I)
 390      FORMAT(8H PROBLEM,I3,5E15.5)                                  
 400              CONTINUE                                              
C                                                                       
C                                                                       
               Q(1) = RCOND/COND1                                       
               Q(2) = CONDM/COND1                                       
               Q(3) = COND1/RCOND                                       
               Q(4)=COND1/CONDM                                         
               Q(5) = EB(1)/(EPS*RCOND*XN(1))                           
               Q(6) = EBM(1)/(EPS*RCOND*XN(1))                          
               Q(7) = EBM2(1)/(EPS*RCOND*XN(1))                         
               Q(8) = AINORM/(EPS*RCOND)                                
               Q(9)=AMNORM/(EPS*RCOND)                                  
               WRITE (IWRITE,470)                                       
               WRITE (IWRITE,560)                                       
               WRITE (IWRITE,470)                                       
               WRITE (IWRITE,620)                                       
               WRITE (IWRITE,630)                                       
               WRITE (IWRITE,640)                                       
               WRITE (IWRITE,470)                                       
               WRITE (IWRITE,680) (Q(I), I = 1, 9)                      
               WRITE (IWRITE,470)                                       
C                                                                       
C              LOOK FOR SUSPICIOUS RATIOS                               
C                                                                       
               QS(1) = 1.0E0 + 4.0E0*EPS                                
               QS(2)=QS(1)                                              
               QS(4) = 10.0E0                                           
               QS(3)=10.0E0                                             
               EN = FLOAT(N)                                            
               IF (N .EQ. 1) EN = 2.0E0                                 
               DO 410 I = 3, 10                                         
                  QS(I) = EN                                            
 410           CONTINUE                                                 
               KOUNT = 0                                                
               DO 430 I = 1, 9                                          
                  IQ(I) = 0                                             
                  IF (Q(I) .LE. QS(I)) GO TO 420                        
                     IQ(I) = 1                                          
                     KSUSP(I) = KSUSP(I) + 1                            
                     KOUNT = KOUNT + 1                                  
 420              CONTINUE                                              
 430           CONTINUE                                                 
               IF (KOUNT .EQ. 0) WRITE (IWRITE,750)                     
               IF (KOUNT .NE. 0) WRITE (IWRITE,760) (IQ(I), I = 1, 9)   
               WRITE (IWRITE,470)                                       
 440        CONTINUE                                                    
 450     CONTINUE                                                       
C                                                                       
         WRITE (IWRITE,570)                                             
         KASE = KASE + 1                                                
      GO TO 20                                                          
 460  CONTINUE                                                          
C                                                                       
C     FINISH MAIN LOOP                                                  
C                                                                       
C     SUMMARY                                                           
C                                                                       
      WRITE (IWRITE,580)                                                
      KASE = KASE - 1                                                   
      WRITE (IWRITE,590) KASE                                           
      WRITE (IWRITE,600) KSING                                          
      WRITE (IWRITE,610) KSUSP                                          
      WRITE (IWRITE,730)                                                
      CALL LEAVE                                                        
      RETURN                                                            
C                                                                       
C     MOST FORMATS, ALSO SOME IN SGEXX                                  
C                                                                       
 470  FORMAT (1H )                                                      
 480  FORMAT (29H1  PORT  TESTER,  GE**,  SP**)                         
 490  FORMAT ( / 14H EPSILON     =, 1PE13.5)                            
 500  FORMAT ( / 19H EXACT SINGULARITY. /)                              
 510  FORMAT ( / 16H MAYBE SINGULAR. /)                                 
 520  FORMAT (14H ACTUAL COND =, 1PE13.5)                               
 530  FORMAT(/14H X AND XBAND =)                                        
 540  FORMAT (14H ERROR NORMS =, 2(1PE13.5))                            
 550  FORMAT (14H RESID NORMS =, 2(1PE13.5))                            
 560  FORMAT (26H TEST RATIOS.. E = EPSILON)                            
 570  FORMAT ( / 14H ************* /)                                   
 580  FORMAT (8H1SUMMARY)                                               
 590  FORMAT (18H NUMBER OF TESTS =, I4)                                
 600  FORMAT (30H NUMBER OF SINGULAR MATRICES =, I4)                    
 610  FORMAT (30H NUMBER OF SUSPICIOUS RATIOS =, 10I4)                  
 620  FORMAT(40H    COND  COND(M)  ACTUAL  ACTUAL  ERROR,               
     1       36H ERROR(M) ERROR(M2) A*AI-I A*AI-I(M))                   
 630  FORMAT (9(8H   -----))                                            
 640  FORMAT(42H    ACTUAL ACTUAL   COND  COND(M) E*COND*X,             
     1       31H E*CONDX E*COND*X E*COND E*COND)                        
 650  FORMAT (14H NORM(A)     =, 1PE13.5)                               
 660  FORMAT (14H NORM(A - LU)=, 1PE13.5)                               
 670  FORMAT (14H NORM(A*AI-I)=, 2(1PE13.5))                            
 680  FORMAT (10(1X, F7.2))                                             
 690  FORMAT (1H , 6G11.4)                                              
 700  FORMAT (14H 1/COND      =, 1PE13.5)                               
 710  FORMAT (2G14.6)                                                   
 720  FORMAT (5H ML =, I2, 6H  MU =, I2)                                
 730  FORMAT ( / 12H END OF TEST)                                       
 740  FORMAT(7H COND =,1PE13.5,13H COND(BAND) =,1PE13.5 /)              
 750  FORMAT (21H NO SUSPICIOUS RATIOS)                                 
 760  FORMAT (I8, 9I10 / 7X, 28H1 INDICATES SUSPICIOUS RATIO)           
 770  FORMAT (29H THIS VERSION DATED 03/11/78.)                         
      END                                                               
      SUBROUTINE SGEXX(A,LDA,N,KASE,IWRITE)                             
C                                                                       
C     GENERATES REAL GENERAL TEST MATRICES                              
C                                                                       
C     FORTRAN FLOAT,MAX0                                                
      INTEGER LDA,N,KASE,IWRITE,IRAND                                   
      INTEGER I,J,LMINM,LPLUSM                                          
      REAL T1,T2                                                        
      REAL A(LDA,1)                                                     
      REAL HUGE,TINY                                                    
C                                                                       
      GO TO (10, 10, 10, 60, 60, 80, 80, 80, 120, 160, 200, 250, 290,   
     *       330, 370, 420, 470, 530, 600), KASE                        
C                                                                       
C     KASE 1, 2 AND 3                                                   
C                                                                       
 10   CONTINUE                                                          
         N = 3*KASE                                                     
         WRITE (IWRITE,20) KASE,N                                       
 20      FORMAT (5H KASE, I3, 3X, 16HHILBERT SLICE    / 4H N =, I4)     
         DO 50 J = 1, N                                                 
            DO 40 I = 1, N                                              
               A(I,J) = 0.0E0                                           
               IF (I .GT. J + 2) GO TO 30                               
               IF (I .LT. J - 3) GO TO 30                               
                  A(I,J) = 1.0E0/FLOAT(I+J-1)                           
 30            CONTINUE                                                 
 40         CONTINUE                                                    
 50      CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 4 AND 5                                                      
C                                                                       
 60   CONTINUE                                                          
         N = 1                                                          
         WRITE (IWRITE,70) KASE,N                                       
 70      FORMAT (5H KASE, I3, 3X, 16HMONOELEMENTAL    / 4H N =, I4)     
         IF (KASE .EQ. 4) A(1,1) = 3.0E0                                
         IF (KASE .EQ. 5) A(1,1) = 0.0E0                                
      GO TO 610                                                         
C                                                                       
C     KASE 6, 7 AND 8                                                   
C                                                                       
 80   CONTINUE                                                          
         N = 15                                                         
         WRITE (IWRITE,90) KASE,N                                       
 90      FORMAT (5H KASE, I3, 3X, 16HTRIDIAGONAL      / 4H N =, I4)     
         T1 = 1.0E0                                                     
         T2 = 1.0E0                                                     
         IF (KASE .EQ. 7) T1 = 100.0E0                                  
         IF (KASE .EQ. 8) T2 = 100.0E0                                  
         DO 110 I = 1, N                                                
            DO 100 J = 1, N                                             
               A(I,J) = 0.0E0                                           
               IF (I .EQ. J) A(I,I) = 4.0E0                             
               IF (I .EQ. J - 1) A(I,J) = T1                            
               IF (I .EQ. J + 1) A(I,J) = T2                            
 100        CONTINUE                                                    
 110     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 9                                                            
C                                                                       
 120  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,130) KASE,N                                      
 130     FORMAT (5H KASE, I3, 3X, 16HRANK ONE         / 4H N =, I4)     
         DO 150 I = 1, N                                                
            DO 140 J = 1, N                                             
               A(I,J) = 10.0E0**(I - J)                                 
 140        CONTINUE                                                    
 150     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 10                                                           
C                                                                       
 160  CONTINUE                                                          
         N = 4                                                          
         WRITE (IWRITE,170) KASE,N                                      
 170     FORMAT (5H KASE, I3, 3X, 16HZERO COLUMN      / 4H N =, I4)     
         DO 190 I = 1, N                                                
            DO 180 J = 1, N                                             
               T1 = FLOAT(J-3)                                          
               T2 = FLOAT(I)                                            
               A(I,J) = T1/T2                                           
 180        CONTINUE                                                    
 190     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 11                                                           
C                                                                       
 200  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,210) KASE,N                                      
 210     FORMAT (5H KASE, I3, 3X, 16HTEST COND        / 4H N =, I4)     
         WRITE(IWRITE,220)(A(I,I),I=1,N)                                
 220       FORMAT(15H DIGAG FROM GEN,5E15.5)                            
         DO 240 I = 1, N                                                
            DO 230 J = 1, N                                             
               IF (I .EQ. J) A(I,J) = FLOAT(I)                          
               IF (I .GT. J) A(I,J) = FLOAT(J-2)                        
               IF (I .LT. J) A(I,J) = FLOAT(I-2)                        
 230        CONTINUE                                                    
 240     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 12                                                           
C                                                                       
 250  CONTINUE                                                          
         N = 3                                                          
         WRITE (IWRITE,260) KASE,N                                      
 260     FORMAT (5H KASE, I3, 3X, 16HIDENTITY         / 4H N =, I4)     
         DO 280 I = 1, N                                                
            DO 270 J = 1, N                                             
               IF (I .EQ. J) A(I,I) = 1.0E0                             
               IF (I .NE. J) A(I,J) = 0.0E0                             
 270        CONTINUE                                                    
 280     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 13                                                           
C                                                                       
 290  CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,300) KASE,N                                      
 300     FORMAT (5H KASE, I3, 3X, 16HUPPER TRIANGULAR / 4H N =, I4)     
         DO 320 I = 1, N                                                
            DO 310 J = 1, N                                             
               IF (I .GT. J) A(I,J) = 0.0E0                             
               IF (I .LE. J) A(I,J) = FLOAT(J-I+1)                      
 310        CONTINUE                                                    
 320     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 14                                                           
C                                                                       
 330  CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,340) KASE,N                                      
 340     FORMAT (5H KASE, I3, 3X, 16HLOWER TRIANGULAR / 4H N =, I4)     
         DO 360 I = 1, N                                                
            DO 350 J = 1, N                                             
               IF (I .LT. J) A(I,J) = 0.0E0                             
               IF (I .GE. J) A(I,J) = FLOAT(I-J+1)                      
 350        CONTINUE                                                    
 360     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 15                                                           
C                                                                       
 370  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,380) KASE,N                                      
 380     FORMAT (5H KASE, I3, 3X, 16HNEAR UNDERFLOW   / 4H N =, I4)     
         TINY =R1MACH(1)*FLOAT(N*N)*1000.0                              
         WRITE (IWRITE,390) TINY                                        
 390     FORMAT (14H TINY        =, 1PE13.5)                            
         DO 410 I = 1, N                                                
            DO 400 J = 1, N                                             
               A(I,J) = TINY*FLOAT(J)/FLOAT(MAX0(I,J))                  
 400        CONTINUE                                                    
 410     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 16                                                           
C                                                                       
 420  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,430) KASE,N                                      
 430     FORMAT (5H KASE, I3, 3X, 16HNEAR OVERFLOW    / 4H N =, I4)     
         HUGE =R1MACH(2)/FLOAT(6000*N*N)                                
         WRITE (IWRITE,440) HUGE                                        
 440     FORMAT (14H HUGE        =, 1PE13.5)                            
         DO 460 I = 1, N                                                
            DO 450 J = 1, N                                             
               A(I,J) = HUGE*(FLOAT(J)/FLOAT(MAX0(I,J)))                
 450        CONTINUE                                                    
 460     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
 470  CONTINUE                                                          
C                                                                       
C THIS IS RANDOM SPARSE ROUTINE                                         
C                                                                       
        WRITE(IWRITE,480)KASE,N                                         
 480    FORMAT(5H KASE, I3,14H RANDOM SPARSE,/4H N =, I4)               
        DO 500 I=1,N                                                    
           DO 490 J=1,N                                                 
              A(I,J)=0.0E0                                              
 490       CONTINUE                                                     
 500     CONTINUE                                                       
        K=N/2                                                           
         DO 520 I=1,N                                                   
            J=0                                                         
            IRAND = UNI(0)                                              
 510        JJ=J+K*IRAND+1                                              
             IF (J.LT.I.AND.JJ.GT.I) JJ=I                               
             J=JJ                                                       
             IF (J.GT.N) GO TO 520                                      
               A(I,J)=UNI(0)                                            
               GO TO 510                                                
 520     CONTINUE                                                       
         GO TO 610                                                      
C                                                                       
C QUEING PROBLEM                                                        
C                                                                       
 530    CONTINUE                                                        
        N=100                                                           
        WRITE(IWRITE,540)KASE,N                                         
 540    FORMAT(5H KASE,I3, 9H QUEUEING,/4H N =, I4)                     
        DO 560 I=1,N                                                    
           DO 550 J=1,N                                                 
              A(I,J)=0.0E0                                              
 550       CONTINUE                                                     
 560    CONTINUE                                                        
        M=SQRT(FLOAT(N))                                                
        L=0                                                             
       A1=M                                                             
        DO 580 I=1,M                                                    
           DO 570 J=1,M                                                 
              L=L+1                                                     
               A(L,L)=-2.0*A1-FLOAT(I+J-2)                              
              IF (J.GT.1)A(L,L-1)=A1                                    
              IF (J.LT.M)A(L,L+1)=J                                     
              LMINM = L-M                                               
              IF (I.GT.1)A(L,LMINM)=A1                                  
              LPLUSM = L+M                                              
              IF (I.LT.M)A(L,LPLUSM)=I                                  
 570        CONTINUE                                                    
 580    CONTINUE                                                        
        DO 590 I=1,N                                                    
 590         A(N,I)=1.0                                                 
          GO TO 610                                                     
 600       CONTINUE                                                     
         N = 0                                                          
 610  CONTINUE                                                          
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE AROW(I, ROW, JCOL, NUM)                                
      INTEGER JCOL(200), JPIB, IA(101), JA(2500)                        
      REAL ROW(200),A(2500)                                             
      COMMON /SPA/IA,JA,A                                               
      NUM=IA(I+1)-IA(I)                                                 
      IB=IA(I)-1                                                        
      DO 10 J=1,NUM                                                     
          JPIB = J+IB                                                   
          ROW(J)=A(JPIB)                                                
          JCOL(J)=JA(JPIB)                                              
 10   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE IIROW(I,  JCOL, NUM)                                   
      INTEGER JCOL(200), JPIB                                           
      COMMON /SPA/IA(101),JA(2500),A(2500)                              
      NUM=IA(I+1)-IA(I)                                                 
      IB=IA(I)-1                                                        
      DO 10 J=1,NUM                                                     
          JPIB = J+IB                                                   
          JCOL(J)=JA(JPIB)                                              
 10   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      REAL FUNCTION ANRM(N,AS,LDA1,AINV,LDA,ANORM,COND1)                
      REAL AS(LDA,N),AINV(LDA1,N)                                       
      REAL AIN,ANORM,COND1,SASUM,T                                      
C                                                                       
C     THIS SUBROUTINE COMPUTES THE CONDITION NUMBER FROM                
C     A AND AINVERSE                                                    
C                                                                       
C     IT ALSO COMPUTES THE ERROR A*AINV-I AND STORES IT IN ANRM         
      REAL B(200)                                                       
      ANRM=0.0E0                                                        
      DO 30 J =1,N                                                      
         DO 10 I=1,N                                                    
            B(I)=0.0E0                                                  
 10      CONTINUE                                                       
         DO 20 K=1,N                                                    
            T=AINV(K,J)                                                 
            CALL SAXPY(N,T,AS(1,K),1,B,1)                               
 20       CONTINUE                                                      
         B(J)=B(J)-1.0E0                                                
         ANRM=AMAX1(ANRM,SASUM(N,B,1))                                  
 30   CONTINUE                                                          
      AIN=0.0E0                                                         
      DO 40 I=1,N                                                       
         AIN=AMAX1(AIN,SASUM(N,AINV(1,I),1))                            
 40   CONTINUE                                                          
      COND1=ANORM*AIN                                                   
      RETURN                                                            
      END                                                               
         SUBROUTINE GETSP(N,A,LDA,IA,JA,AS,L)                           
         INTEGER JA(1),IA(1)                                            
         REAL A(LDA,N),AS(1)                                            
         L=0                                                            
         IA(1)=1                                                        
         DO 30 I=1,N                                                    
            DO 10 J=1,N                                                 
               IF (A(I,J).EQ.0.0E0) GO TO 10                            
               L=L+1                                                    
               AS(L)=A(I,J)                                             
               JA(L)=J                                                  
 10         CONTINUE                                                    
           IF (L.GE.IA(I))GO TO 20                                      
               L=L+1                                                    
               AS(L)=0.0                                                
               JA(L)=1                                                  
 20        CONTINUE                                                     
            IA(I+1)=L+1                                                 
 30     CONTINUE                                                        
        RETURN                                                          
        END                                                             
C$TEST PRAD                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT DOUBLE PRECISION SPARSE MATRIX PACKAGE              
C                                                                       
C***********************************************************************
C  THIS IS THE TESTER FOR LINDA KAUFMAN'S SPARSE                        
C  MATRIX PACKAGE - DOUBLE PRECISION                                    
C                                                                       
C  MARCH 20, 1981                                                       
C                                                                       
C     MAIN PROGRAM                                                      
      INTEGER IWRITE, I1MACH                                            
C     ALLOW 5000 UNDERFLOWS.                                            
C                                                                       
C     SET OUTPUT UNIT NUMBER                                            
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      CALL SGETS(IWRITE)                                                
      STOP                                                              
      END                                                               
      SUBROUTINE SGETS(IWRITE)                                          
C     IWRITE IS THE OUTPUT UNIT NUMBER                                  
C                                                                       
C     TESTS                                                             
C        GECE,GEFS,GEBS,GEML,SPMOR,SPFOR,SPMCE,SPFCE,SPMLE,SPFLE,SPMSL  
C        DSPFSL,DSPMNF,DSPFNF,SPFSF,DSPMSF,DSPMIN                       
C                                                                       
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     PORT GECE,GEFS,GEBS,GEML,SPMOR,SPFOR,SPMCE,SPFCE,SPMLE,SPFLE      
C     PORT DSPMSL,DSPFSL,DSPMNF,DSPFNF,DSPMS,SPFSF.DSPMIN               
C     PORT UTILITIES ERROFF,ENTER,LEAVE,R1MACH                          
C     EXTERNAL SGEXX                                                    
C     BLAS DAXPY,DDOT,SSCAL,DASUM                                       
C     FORTRAN ABS,DMAX1,DFLOAT,MAX0,MIN0                                
C                                                                       
C     INTERNAL VARIABLES                                                
C                                                                       
      INTEGER IIA(101), JJA(2500)                                       
      INTEGER JSPSAV(2500),IWORK(2800),IL(101)                          
      INTEGER MR(100),MC(100),FR(100)                                   
      INTEGER FC(100),MIC(100),JA(2500),IA(101)                         
      INTEGER I,IPVT(100),IPVTB(100),IQ(8),I1,I2,J                      
      INTEGER K,KASE,KB,KBFAIL,KOUNT,KP1,KSING,KSUSP(8)                 
      INTEGER L,LDA,LDAB,IWRITE,M,ML,MU,N,NM1,NPRINT                    
      DOUBLE PRECISION A(100,100),AS(2500),AINV(100,100),Z(100)         
      DOUBLE PRECISION ASAVE(100,100),UL(2500)                          
      DOUBLE PRECISION XEXACT(100,2),B(100,2),BM(100,2)                 
      DOUBLE PRECISION BM2(100,2),BF(100,2), D1MACH, DFLOAT             
      DOUBLE PRECISION BF2(100,2)                                       
      DOUBLE PRECISION AFNORM,AMNORM,ANRM,CONDF1,CONDF,CONDM1           
      DOUBLE PRECISION CONDM,GROWTH,AA(2500)                            
      DOUBLE PRECISION SPSAV(2500)                                      
      DOUBLE PRECISION AINORM,ANORM,SMACH,COND,COND1,EN,ENORM,EPS, EPS1 
      DOUBLE PRECISION ETNORM,FNI,FNORM,ONEPX,RCOND,RCONDB,RNORM        
      DOUBLE PRECISION RTNORM,Q(10),QS(10),DASUM,XNORM                  
      DOUBLE PRECISION EB(2),XN(2),EBM(2),EBF(2),EBM2(2),EBF2(2)        
      LOGICAL KBF                                                       
C                                                                       
      COMMON /SPA/ AA,JJA,IIA                                           
      COMMON /CSTAK/ D(12000)                                           
C                                                                       
      EXTERNAL IIROW,AROW                                               
C                                                                       
      CALL ISTKIN(12000,3)                                              
C                                                                       
      LDA = 100                                                         
      IAMAX = 2500                                                      
      MAXUL = 2500                                                      
      IWMAX = 2500                                                      
      CALL ENTER(1)                                                     
C                                                                       
C     WRITE MATRIX AND SOLUTIONS IF  N .LE. NPRINT                      
C                                                                       
      NPRINT =3                                                         
C                                                                       
      WRITE (IWRITE,480)                                                
      WRITE (IWRITE,770)                                                
C                                                                       
      DO 10 I = 1, 8                                                    
         KSUSP(I) = 0                                                   
 10   CONTINUE                                                          
      KSING = 0                                                         
      KBFAIL = 0                                                        
C                                                                       
C     SET EPS TO ROUNDING UNIT                                          
C                                                                       
      EPS =D1MACH(4)                                                    
      WRITE (IWRITE,490) EPS                                            
      WRITE (IWRITE,470)                                                
C                                                                       
C     START MAIN LOOP                                                   
C                                                                       
      KASE = 1                                                          
 20   CONTINUE                                                          
C                                                                       
C        GENERATE TEST MATRIX                                           
C                                                                       
         CALL SGEXX(A,LDA,N,KASE,IWRITE)                                
C                                                                       
C        N = 0 SIGNALS NO MORE TEST MATRICES                            
C                                                                       
C     ...EXIT                                                           
         IF (N .LE. 0) GO TO 460                                        
         ANORM = 0.0D0                                                  
         DO 30 J = 1, N                                                 
            ANORM = DMAX1(ANORM,DASUM(N,A(1,J),1))                      
 30      CONTINUE                                                       
         WRITE (IWRITE,650) ANORM                                       
C                                                                       
         IF (N .GT. NPRINT) GO TO 50                                    
            WRITE (IWRITE,470)                                          
            DO 40 I = 1, N                                              
               WRITE (IWRITE,690) (A(I,J), J = 1, N)                    
 40         CONTINUE                                                    
            WRITE (IWRITE,470)                                          
 50      CONTINUE                                                       
C                                                                       
C        GENERATE EXACT SOLUTION                                        
C                                                                       
         XEXACT(1,1) = 1.0D0                                            
         XEXACT(1,2) = 1.0D0                                            
         XEXACT(2,2) = 1.0D0                                            
         IF (N .GE. 2) XEXACT(2,1) = 0.0D0                              
         IF (N .LE. 2) GO TO 70                                         
            DO 60 I = 3, N                                              
               XEXACT(I,1) = -XEXACT(I-2,1)                             
                XEXACT(I,2) = 1.0D0                                     
 60         CONTINUE                                                    
 70      CONTINUE                                                       
C                                                                       
C        SAVE MATRIX AND GENERATE R.H.S.                                
C                                                                       
         DO 90 I = 1, N                                                 
            DO 80 J = 1, N                                              
               ASAVE(I,J) = A(I,J)                                      
 80         CONTINUE                                                    
 90      CONTINUE                                                       
         CALL DGEML(N,A,LDA,XEXACT,B)                                   
         CALL DGEML(N,A,LDA,XEXACT(1,2),B(1,2))                         
         NN=LDA*2                                                       
         CALL MOVEFD(NN,B,BM)                                           
         CALL MOVEFD(NN,B,BF)                                           
         CALL MOVEFD(NN,B,BM2)                                          
         CALL MOVEFD(NN,B,BF2)                                          
      CALL DGETSP(N,A, LDA, IA,JA,AS,MAXA)                              
      CALL DGETSP(N,A,LDA,IIA,JJA,AA,MAXA)                              
         DO 100 I=1,MAXA                                                
            SPSAV(I)=AS(I)                                              
            JSPSAV(I)=JA(I)                                             
 100     CONTINUE                                                       
         NP1=N+1                                                        
C                                                                       
C CHECK ORDERING SUBROUTINES                                            
C                                                                       
         CALL SPMOR(N,IA,JA,MC,MIC)                                     
         CALL SPFOR(N,IIROW,FC)                                         
         IBAD=0                                                         
         DO 120 I=1,N                                                   
            MR(I)=MC(I)                                                 
            FR(I)=FC(I)                                                 
            IF (MC(I).EQ.FC(I)) GO TO 120                               
            IBAD=IBAD+1                                                 
            WRITE(IWRITE,110)I,MC(I),FC(I)                              
 110         FORMAT(19H ORDER DISAGREEMENT,3I5)                         
 120    CONTINUE                                                        
        IF (IBAD.EQ.0)WRITE(IWRITE,130)                                 
 130     FORMAT(28H NO DISAGREEMENT IN ORDERING)                        
C                                                                       
C        FACTOR AND ESTIMATE CONDITION                                  
C                                                                       
         CALL DGECE(N,A,LDA,IPVT,RCOND)                                 
         IF (NERROR(INE).NE.0) CALL  ERROFF                             
C                                                                       
C                                                                       
C        FACTOR SPARSE FORM AND COMPARE                                 
C                                                                       
        CALL DSPMCE(N,MR,MC,AS,IA,JA,IAMAX,IL,ISIZE,CONDM,Z)            
        IF(NERROR(INM).NE.0) CALL ERROFF                                
         CALL DSPFCE(N,FR,FC,AROW,IWORK,UL,MAXUL,ISIZE,CONDF,Z)         
         IF (NERROR(INF).NE.0) CALL ERROFF                              
        WRITE(IWRITE,140)                                               
 140     FORMAT(21H CONDITION ESTIMATION)                               
        WRITE(IWRITE,150)                                               
 150     FORMAT(42H    GENERAL        SPARSE M       SPARSE F)          
        WRITE(IWRITE,160)RCOND,CONDM,CONDF                              
 160    FORMAT(1H ,3D15.5)                                              
C                                                                       
C                                                                       
C           TEST FOR SINGULARITY                                        
C                                                                       
          IF (INE+INM+INF.EQ.0)GO TO 190                                
            WRITE(IWRITE,170)INE,INM,INF                                
 170         FORMAT(13H INE, INM,INF,3I5)                               
 180          FORMAT(25H SPARSE ROUTINES DISAGREE,2I4)                  
               WRITE (IWRITE,500)                                       
               KSING = KSING + 1                                        
            GO TO 440                                                   
 190        CONTINUE                                                    
C                                                                       
C              COMPUTE INVERSE AND COND1 = TRUE CONDITION               
C                                                                       
               DO 210 J = 1, N                                          
                  DO 200 I = 1, N                                       
                     AINV(I,J) = 0.D0                                   
 200              CONTINUE                                              
               AINV(J,J)=1.D0                                           
 210           CONTINUE                                                 
               CALL DGEFS(N,A,LDA,AINV,LDA,N,IPVT)                      
               CALL DGEBS(N,A,LDA,AINV,LDA,N)                           
               AINORM=ANRM(N,ASAVE,LDA,AINV,LDA, ANORM, COND1)          
               WRITE (IWRITE,520) COND1                                 
               DO 230 I=1,N                                             
                  DO 220 J=1,N                                          
                     AINV(I,J)=0.0D0                                    
 220               CONTINUE                                             
                  AINV(I,I)=1.0D0                                       
 230           CONTINUE                                                 
               CALL DSPMSL(N,MR,MC,IA,JA,AS,IL,AINV,LDA,N)              
               AMNORM=ANRM(N,ASAVE,LDA,AINV,LDA,ANORM,CONDM1)           
               WRITE(IWRITE,260)CONDM1                                  
               DO 250 I=1,N                                             
                   DO 240 J=1,N                                         
                      AINV(I,J)=0.0D0                                   
 240               CONTINUE                                             
                   AINV(I,I)=1.0D0                                      
 250           CONTINUE                                                 
               CALL DSPFSL(N,FR,FC,IWORK,UL,AINV,LDA,N)                 
               AFNORM=ANRM(N,ASAVE,LDA,AINV,LDA,ANORM,CONDF1)           
               WRITE(IWRITE,270)CONDF1                                  
 260           FORMAT(19H COND FROM AINV*A M,D15.5)                     
 270           FORMAT(19H COND FROM AINV*A F,D15.5)                     
C                                                                       
C              SOLVE  A*X = B                                           
C                                                                       
               CALL DGEFS(N,A,LDA,B,LDA,2,IPVT)                         
               CALL DGEBS(N,A,LDA,B,LDA,2)                              
C                                                                       
C              MORE SPAR COMPARE                                        
C                                                                       
               DO 280 I=1,N                                             
                  MC(I)=MR(I)                                           
                  FC(I)=FR(I)                                           
 280           CONTINUE                                                 
               CALL SPMSF(N,MR,MIC,IA,JSPSAV,IWORK,IWMAX,IFILL)         
               DO 290 J=1,N                                             
                  I=MR(J)                                               
                  NUM=IA(I+1)-IA(I)                                     
                 IB=IA(I)                                               
                   CALL DSPMIN(N,MIC,IWORK,J,SPSAV(IB),JSPSAV(IB),NUM,  
     1          J,UL)                                                   
 290           CONTINUE                                                 
              EPS1=EPS*ANORM                                            
               CALL DSPMNF(N,IWORK,UL,EPS1,GROWTH)                      
               IF (NERROR(IMNF).EQ.0) GO TO 300                         
                 CALL ERROFF                                            
                 GO TO 310                                              
 300           CONTINUE                                                 
               CALL DSPSOL(N,MR,MC,IWORK,UL,BM,LDA,2)                   
 310           CONTINUE                                                 
               CALL SPFSF(N,FR,FC,IIROW,IWORK,IWMAX,IFILL)              
               CALL DSPFNF(N,FR,FC,AROW,IWORK,UL,GROWTH,EPS1)           
               IF (NERROR(IFNF).EQ.0) GO TO 320                         
                  CALL ERROFF                                           
                  GO TO 330                                             
 320           CONTINUE                                                 
               CALL DSPSOL(N,FR,FC,IWORK,UL,BF,LDA,2)                   
 330           CONTINUE                                                 
               CALL DSPMLE(N,.TRUE.,IA,JSPSAV,SPSAV,ISIZE,BM2,LDA,2)    
               IF(NERROR(IMLE).NE.0)CALL ERROFF                         
               CALL DSPFLE(N,.TRUE.,AROW,ISIZE,BF2,LDA,2)               
                IF (NERROR(IFLE).NE.0) CALL ERROFF                      
C COMPUTE ERRORS IN SOLUTION WITH 2 RIGHT HAND SIDES                    
               IF(IFLE+IMLE+IMNF+IFNF.EQ.0)GO TO 350                    
                 WRITE(IWRITE,340)IMNF,IFNF,IMLE,IFLE                   
 340    FORMAT(42H NUMERICALLY SINGULAR- IMNF,IFNF,IMLE,IFLE,4I5)       
                GO TO 440                                               
 350           CONTINUE                                                 
               DO 360 I=1,2                                             
                  XN(I)=DASUM(N,B(1,I),1)                               
                  CALL DAXPY(N,-1.0D0,XEXACT(1,I),1,B(1,I),1)           
                  CALL DAXPY(N,-1.0D0,XEXACT(1,I),1,BM(1,I),1)          
                  CALL DAXPY(N,-1.0D0,XEXACT(1,I),1,BF(1,I),1)          
                  CALL DAXPY(N,-1.0D0,XEXACT(1,I),1,BM2(1,I),1)         
                  CALL DAXPY(N,-1.0D0,XEXACT(1,I),1,BF2(1,I),1)         
                  EB(I)=DASUM(N,B(1,I),1)                               
                  EBM(I)=DASUM(N,BM(1,I),1)                             
                  EBF(I)=DASUM(N,BF(1,I),1)                             
                  EBM2(I)=DASUM(N,BM2(1,I),1)                           
                  EBF2(I)=DASUM(N,BF2(1,I),1)                           
 360              CONTINUE                                              
                  WRITE(IWRITE,370)                                     
 370              FORMAT(29H ERRORS IN SOLUTION -ABSOLUTE)              
                  WRITE(IWRITE,380)                                     
 380           FORMAT(15X,43H GENERAL SPMN      SPFN       SPMLE   SPFLE
     *)                                                                 
                  DO 400 I=1,2                                          
                 WRITE(IWRITE,390)I,EB(I),EBM(I),EBF(I),EBM2(I),EBF2(I) 
 390      FORMAT(8H PROBLEM,I3,5D15.5)                                  
 400              CONTINUE                                              
C                                                                       
C                                                                       
               Q(1) = RCOND/COND1                                       
               Q(2) = CONDM/COND1                                       
               Q(3) = COND1/RCOND                                       
               Q(4)=COND1/CONDM                                         
               Q(5) = EB(1)/(EPS*RCOND*XN(1))                           
               Q(6) = EBM(1)/(EPS*RCOND*XN(1))                          
               Q(7) = EBM2(1)/(EPS*RCOND*XN(1))                         
               Q(8) = AINORM/(EPS*RCOND)                                
               Q(9)=AMNORM/(EPS*RCOND)                                  
               WRITE (IWRITE,470)                                       
               WRITE (IWRITE,560)                                       
               WRITE (IWRITE,470)                                       
               WRITE (IWRITE,620)                                       
               WRITE (IWRITE,630)                                       
               WRITE (IWRITE,640)                                       
               WRITE (IWRITE,470)                                       
               WRITE (IWRITE,680) (Q(I), I = 1, 9)                      
               WRITE (IWRITE,470)                                       
C                                                                       
C              LOOK FOR SUSPICIOUS RATIOS                               
C                                                                       
               QS(1) = 1.0D0 + 4.0D0*EPS                                
               QS(2)=QS(1)                                              
               QS(4) = 10.0D0                                           
               QS(3)=10.0D0                                             
               EN = DFLOAT(N)                                           
               IF (N .EQ. 1) EN = 2.0D0                                 
               DO 410 I = 3, 10                                         
                  QS(I) = EN                                            
 410           CONTINUE                                                 
               KOUNT = 0                                                
               DO 430 I = 1, 9                                          
                  IQ(I) = 0                                             
                  IF (Q(I) .LE. QS(I)) GO TO 420                        
                     IQ(I) = 1                                          
                     KSUSP(I) = KSUSP(I) + 1                            
                     KOUNT = KOUNT + 1                                  
 420              CONTINUE                                              
 430           CONTINUE                                                 
               IF (KOUNT .EQ. 0) WRITE (IWRITE,750)                     
               IF (KOUNT .NE. 0) WRITE (IWRITE,760) (IQ(I), I = 1, 9)   
               WRITE (IWRITE,470)                                       
 440        CONTINUE                                                    
 450     CONTINUE                                                       
C                                                                       
         WRITE (IWRITE,570)                                             
         KASE = KASE + 1                                                
      GO TO 20                                                          
 460  CONTINUE                                                          
C                                                                       
C     FINISH MAIN LOOP                                                  
C                                                                       
C     SUMMARY                                                           
C                                                                       
      WRITE (IWRITE,580)                                                
      KASE = KASE - 1                                                   
      WRITE (IWRITE,590) KASE                                           
      WRITE (IWRITE,600) KSING                                          
      WRITE (IWRITE,610) KSUSP                                          
      WRITE (IWRITE,730)                                                
      CALL LEAVE                                                        
      RETURN                                                            
C                                                                       
C     MOST FORMATS, ALSO SOME IN SGEXX                                  
C                                                                       
 470  FORMAT (1H )                                                      
 480  FORMAT (29H1  PORT  TESTER, DGE**, DSP**)                         
 490  FORMAT ( / 14H EPSILON     =, 1PD13.5)                            
 500  FORMAT ( / 19H EXACT SINGULARITY. /)                              
 510  FORMAT ( / 16H MAYBE SINGULAR. /)                                 
 520  FORMAT (14H ACTUAL COND =, 1PD13.5)                               
 530  FORMAT(/14H X AND XBAND =)                                        
 540  FORMAT (14H ERROR NORMS =, 2(1PD13.5))                            
 550  FORMAT (14H RESID NORMS =, 2(1PD13.5))                            
 560  FORMAT (26H TEST RATIOS.. E = EPSILON)                            
 570  FORMAT ( / 14H ************* /)                                   
 580  FORMAT (8H1SUMMARY)                                               
 590  FORMAT (18H NUMBER OF TESTS =, I4)                                
 600  FORMAT (30H NUMBER OF SINGULAR MATRICES =, I4)                    
 610  FORMAT (30H NUMBER OF SUSPICIOUS RATIOS =, 10I4)                  
 620  FORMAT(40H    COND  COND(M)  ACTUAL  ACTUAL  ERROR,               
     1       36H ERROR(M) ERROR(M2) A*AI-I A*AI-I(M))                   
 630  FORMAT (9(8H   -----))                                            
 640  FORMAT(42H    ACTUAL ACTUAL   COND  COND(M) E*COND*X,             
     1       31H E*CONDX E*COND*X E*COND E*COND)                        
 650  FORMAT (14H NORM(A)     =, 1PD13.5)                               
 660  FORMAT (14H NORM(A - LU)=, 1PD13.5)                               
 670  FORMAT (14H NORM(A*AI-I)=, 2(1PD13.5))                            
 680  FORMAT (10(1X, F7.2))                                             
 690  FORMAT (1H , 6G11.4)                                              
 700  FORMAT (14H 1/COND      =, 1PD13.5)                               
 710  FORMAT (2G14.6)                                                   
 720  FORMAT (5H ML =, I2, 6H  MU =, I2)                                
 730  FORMAT ( / 12H END OF TEST)                                       
 740  FORMAT(7H COND =,1PD13.5,13H COND(BAND) =,1PD13.5 /)              
 750  FORMAT (21H NO SUSPICIOUS RATIOS)                                 
 760  FORMAT (I8, 9I10 / 7X, 28H1 INDICATES SUSPICIOUS RATIO)           
 770  FORMAT (29H THIS VERSION DATED 03/11/78.)                         
      END                                                               
      SUBROUTINE SGEXX(A,LDA,N,KASE,IWRITE)                             
C                                                                       
C     GENERATES DOUBLE PRECISION GENERAL TEST MATRICES                  
C                                                                       
C     EXTERNAL SMACH                                                    
C     FORTRAN DFLOAT,MAX0                                               
      INTEGER I,J,IRAND                                                 
      INTEGER LDA,N,KASE,IWRITE                                         
      DOUBLE PRECISION A(LDA,1), D1MACH, DFLOAT                         
      DOUBLE PRECISION T1,T2                                            
      DOUBLE PRECISION SMACH,HUGE,TINY                                  
C                                                                       
      GO TO (10, 10, 10, 60, 60, 80, 80, 80, 120, 160, 200, 250, 290,   
     *       330, 370, 420, 470, 530, 600), KASE                        
C                                                                       
C     KASE 1, 2 AND 3                                                   
C                                                                       
 10   CONTINUE                                                          
         N = 3*KASE                                                     
         WRITE (IWRITE,20) KASE,N                                       
 20      FORMAT (5H KASE, I3, 3X, 16HHILBERT SLICE    / 4H N =, I4)     
         DO 50 J = 1, N                                                 
            DO 40 I = 1, N                                              
               A(I,J) = 0.0D0                                           
               IF (I .GT. J + 2) GO TO 30                               
               IF (I .LT. J - 3) GO TO 30                               
                  A(I,J) = 1.0D0/DFLOAT(I+J-1)                          
 30            CONTINUE                                                 
 40         CONTINUE                                                    
 50      CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 4 AND 5                                                      
C                                                                       
 60   CONTINUE                                                          
         N = 1                                                          
         WRITE (IWRITE,70) KASE,N                                       
 70      FORMAT (5H KASE, I3, 3X, 16HMONOELEMENTAL    / 4H N =, I4)     
         IF (KASE .EQ. 4) A(1,1) = 3.0D0                                
         IF (KASE .EQ. 5) A(1,1) = 0.0D0                                
      GO TO 610                                                         
C                                                                       
C     KASE 6, 7 AND 8                                                   
C                                                                       
 80   CONTINUE                                                          
         N = 15                                                         
         WRITE (IWRITE,90) KASE,N                                       
 90      FORMAT (5H KASE, I3, 3X, 16HTRIDIAGONAL      / 4H N =, I4)     
         T1 = 1.0D0                                                     
         T2 = 1.0D0                                                     
         IF (KASE .EQ. 7) T1 = 100.0D0                                  
         IF (KASE .EQ. 8) T2 = 100.0D0                                  
         DO 110 I = 1, N                                                
            DO 100 J = 1, N                                             
               A(I,J) = 0.0D0                                           
               IF (I .EQ. J) A(I,I) = 4.0D0                             
               IF (I .EQ. J - 1) A(I,J) = T1                            
               IF (I .EQ. J + 1) A(I,J) = T2                            
 100        CONTINUE                                                    
 110     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 9                                                            
C                                                                       
 120  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,130) KASE,N                                      
 130     FORMAT (5H KASE, I3, 3X, 16HRANK ONE         / 4H N =, I4)     
         DO 150 I = 1, N                                                
            DO 140 J = 1, N                                             
               A(I,J) = 10.0D0**(I - J)                                 
 140        CONTINUE                                                    
 150     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 10                                                           
C                                                                       
 160  CONTINUE                                                          
         N = 4                                                          
         WRITE (IWRITE,170) KASE,N                                      
 170     FORMAT (5H KASE, I3, 3X, 16HZERO COLUMN      / 4H N =, I4)     
         DO 190 I = 1, N                                                
            DO 180 J = 1, N                                             
               T1 = DFLOAT(J-3)                                         
               T2 = DFLOAT(I)                                           
               A(I,J) = T1/T2                                           
 180        CONTINUE                                                    
 190     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 11                                                           
C                                                                       
 200  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,210) KASE,N                                      
 210     FORMAT (5H KASE, I3, 3X, 16HTEST COND        / 4H N =, I4)     
         WRITE(IWRITE,220)(A(I,I),I=1,N)                                
 220       FORMAT(15H DIGAG FROM GEN,5E15.5)                            
         DO 240 I = 1, N                                                
            DO 230 J = 1, N                                             
               IF (I .EQ. J) A(I,J) = DFLOAT(I)                         
               IF (I .GT. J) A(I,J) = DFLOAT(J-2)                       
               IF (I .LT. J) A(I,J) = DFLOAT(I-2)                       
 230        CONTINUE                                                    
 240     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 12                                                           
C                                                                       
 250  CONTINUE                                                          
         N = 3                                                          
         WRITE (IWRITE,260) KASE,N                                      
 260     FORMAT (5H KASE, I3, 3X, 16HIDENTITY         / 4H N =, I4)     
         DO 280 I = 1, N                                                
            DO 270 J = 1, N                                             
               IF (I .EQ. J) A(I,I) = 1.0D0                             
               IF (I .NE. J) A(I,J) = 0.0D0                             
 270        CONTINUE                                                    
 280     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 13                                                           
C                                                                       
 290  CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,300) KASE,N                                      
 300     FORMAT (5H KASE, I3, 3X, 16HUPPER TRIANGULAR / 4H N =, I4)     
         DO 320 I = 1, N                                                
            DO 310 J = 1, N                                             
               IF (I .GT. J) A(I,J) = 0.0D0                             
               IF (I .LE. J) A(I,J) = DFLOAT(J-I+1)                     
 310        CONTINUE                                                    
 320     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 14                                                           
C                                                                       
 330  CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,340) KASE,N                                      
 340     FORMAT (5H KASE, I3, 3X, 16HLOWER TRIANGULAR / 4H N =, I4)     
         DO 360 I = 1, N                                                
            DO 350 J = 1, N                                             
               IF (I .LT. J) A(I,J) = 0.0D0                             
               IF (I .GE. J) A(I,J) = DFLOAT(I-J+1)                     
 350        CONTINUE                                                    
 360     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 15                                                           
C                                                                       
 370  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,380) KASE,N                                      
 380     FORMAT (5H KASE, I3, 3X, 16HNEAR UNDERFLOW   / 4H N =, I4)     
         TINY =D1MACH(1)*DFLOAT(N*N)*1000.0                             
         WRITE (IWRITE,390) TINY                                        
 390     FORMAT (14H TINY        =, 1PD13.5)                            
         DO 410 I = 1, N                                                
            DO 400 J = 1, N                                             
               A(I,J) = TINY*DFLOAT(J)/DFLOAT(MAX0(I,J))                
 400        CONTINUE                                                    
 410     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
C     KASE 16                                                           
C                                                                       
 420  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,430) KASE,N                                      
 430     FORMAT (5H KASE, I3, 3X, 16HNEAR OVERFLOW    / 4H N =, I4)     
         HUGE =D1MACH(2)/DFLOAT(12000*N*N)                              
         WRITE (IWRITE,440) HUGE                                        
 440     FORMAT (14H HUGE        =, 1PD13.5)                            
         DO 460 I = 1, N                                                
            DO 450 J = 1, N                                             
               A(I,J) = HUGE*(DFLOAT(J)/DFLOAT(MAX0(I,J)))              
 450        CONTINUE                                                    
 460     CONTINUE                                                       
      GO TO 610                                                         
C                                                                       
 470  CONTINUE                                                          
C                                                                       
C THIS IS RANDOM SPARSE ROUTINE                                         
C                                                                       
        WRITE(IWRITE,480)KASE,N                                         
 480    FORMAT(5H KASE, I3,14H RANDOM SPARSE,/4H N =, I4)               
        DO 500 I=1,N                                                    
           DO 490 J=1,N                                                 
              A(I,J)=0.0D0                                              
 490       CONTINUE                                                     
 500     CONTINUE                                                       
        K=N/2                                                           
         DO 520 I=1,N                                                   
            J=0                                                         
            IRAND = UNI(0)                                              
 510        JJ=J+K*IRAND+1                                              
             IF (J.LT.I.AND.JJ.GT.I) JJ=I                               
             J=JJ                                                       
             IF (J.GT.N) GO TO 520                                      
               A(I,J)=UNI(0)                                            
               GO TO 510                                                
 520     CONTINUE                                                       
         GO TO 610                                                      
C                                                                       
C QUEING PROBLEM                                                        
C                                                                       
 530    CONTINUE                                                        
        N=100                                                           
        WRITE(IWRITE,540)KASE,N                                         
 540    FORMAT(5H KASE,I3, 9H QUEUEING,/4H N =, I4)                     
        DO 560 I=1,N                                                    
           DO 550 J=1,N                                                 
              A(I,J)=0.0D0                                              
 550       CONTINUE                                                     
 560    CONTINUE                                                        
        M=SQRT(FLOAT(N))                                                
        L=0                                                             
       A1=M                                                             
        DO 580 I=1,M                                                    
           DO 570 J=1,M                                                 
              L=L+1                                                     
               A(L,L)=-2.0*A1-DFLOAT(I+J-2)                             
              IF (J.GT.1)A(L,L-1)=A1                                    
              IF (J.LT.M)A(L,L+1)=J                                     
              IF (I.GT.1)A(L,L-M)=A1                                    
              IF (I.LT.M)A(L,L+M)=I                                     
 570        CONTINUE                                                    
 580    CONTINUE                                                        
        DO 590 I=1,N                                                    
 590         A(N,I)=1.0                                                 
          GO TO 610                                                     
 600       CONTINUE                                                     
         N = 0                                                          
 610  CONTINUE                                                          
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE AROW(I, ROW, JCOL, NUM)                                
      INTEGER JCOL(200), IA(101), JA(2500), JPIB                        
      DOUBLE PRECISION ROW(200),A(2500)                                 
      COMMON /SPA/A,JA,IA                                               
       NUM=IA(I+1)-IA(I)                                                
      IB=IA(I)-1                                                        
      DO 10 J=1,NUM                                                     
          JPIB = J+IB                                                   
          ROW(J)=A(JPIB)                                                
          JCOL(J)=JA(JPIB)                                              
 10   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE IIROW(I,  JCOL, NUM)                                   
      INTEGER JCOL (200), IA(101), JA(2500), JPIB                       
      DOUBLE PRECISION A(2500)                                          
      COMMON /SPA/A,JA,IA                                               
      NUM=IA(I+1)-IA(I)                                                 
      IB=IA(I)-1                                                        
      DO 10 J=1,NUM                                                     
          JPIB = J+IB                                                   
          JCOL(J)=JA(JPIB)                                              
 10   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION ANRM(N,AS,LDA1,AINV,LDA,ANORM,COND1)    
      DOUBLE PRECISION AS(LDA,N),AINV(LDA1,N)                           
      DOUBLE PRECISION AIN,ANORM,COND1,DASUM,T                          
C                                                                       
C     THIS SUBROUTINE COMPUTES THE CONDITION NUMBER FROM                
C     A AND AINVERSE                                                    
C                                                                       
C     IT ALSO COMPUTES THE ERROR A*AINV-I AND STORES IT IN ANRM         
      DOUBLE PRECISION B(200)                                           
      ANRM=0.0D0                                                        
      DO 30 J =1,N                                                      
         DO 10 I=1,N                                                    
            B(I)=0.0D0                                                  
 10      CONTINUE                                                       
         DO 20 K=1,N                                                    
            T=AINV(K,J)                                                 
            CALL DAXPY(N,T,AS(1,K),1,B,1)                               
 20       CONTINUE                                                      
         B(J)=B(J)-1.0D0                                                
         ANRM=DMAX1(ANRM,DASUM(N,B,1))                                  
 30   CONTINUE                                                          
      AIN=0.0D0                                                         
      DO 40 I=1,N                                                       
         AIN=DMAX1(AIN,DASUM(N,AINV(1,I),1))                            
 40   CONTINUE                                                          
      COND1=ANORM*AIN                                                   
      RETURN                                                            
      END                                                               
         SUBROUTINE DGETSP(N,A,LDA,IA,JA,AS,L)                          
         DOUBLE PRECISION A(LDA,N),AS(1)                                
         INTEGER JA(1),IA(1)                                            
         L=0                                                            
         IA(1)=1                                                        
         DO 30 I=1,N                                                    
            DO 10 J=1,N                                                 
               IF (A(I,J).EQ.0.0D0) GO TO 10                            
               L=L+1                                                    
               AS(L)=A(I,J)                                             
               JA(L)=J                                                  
 10         CONTINUE                                                    
           IF (L.GE.IA(I))GO TO 20                                      
               L=L+1                                                    
               AS(L)=0.0                                                
               JA(L)=1                                                  
 20        CONTINUE                                                     
            IA(I+1)=L+1                                                 
 30     CONTINUE                                                        
        RETURN                                                          
        END                                                             
C$TEST PRAC                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT COMPLEX SPARSE MATRIX PACKAGE                       
C                                                                       
C***********************************************************************
C  THIS IS THE TESTER FOR LINDA KAUFMAN'S SPARSE                        
C  MATRIX PACKAGE - COMPLEX                                             
C                                                                       
C  MARCH 20, 1981                                                       
C                                                                       
C     MAIN PROGRAM                                                      
      INTEGER IWRITE,I1MACH                                             
C     ALLOW 5000 UNDERFLOWS.                                            
C                                                                       
C     OUTPUT UNIT NUMBER                                                
C                                                                       
      IWRITE = I1MACH(2)                                                
C                                                                       
      CALL SGETS(IWRITE)                                                
      STOP                                                              
      END                                                               
      SUBROUTINE SGETS(IWRITE)                                          
C     IWRITE IS THE OUTPUT UNIT NUMBER                                  
C                                                                       
C     TESTS                                                             
C        GECE,GEFS,GEBS,GEML,SPMOR,SPFOR,SPMCE,SPFCE,SPMLE,SPFLE,SPMSL  
C        SPFSL,SPMNF,SPFNF,SPFSF,SPMSF,SPMIN                            
C         COMPLEX VERSIONS                                              
C                                                                       
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     PORT GECE,GEFS,GEBS,GEML,SPMOR,SPFOR,SPMCE,SPFCE,SPMLE,SPFLE      
C     PORT SPMSL,SPFSL,SPMNF,SPFNF,SPMS,SPFSF.SPMIN                     
C     PORT UTILITIES ERROFF,ENTER,LEAVE,R1MACH                          
C     EXTERNAL SGEXX                                                    
C     BLAS CAXPY,SDOT,SSCAL,SCASUM                                      
C     FORTRAN ABS,AMAX1,FLOAT,MAX0,MIN0                                 
C                                                                       
C     INTERNAL VARIABLES                                                
C                                                                       
      INTEGER IIA(101), JJA(2500)                                       
      INTEGER JSPSAV(2500),IWORK(2800),IL(101),MR(100),MC(100),FR(100)  
      INTEGER FC(100),MIC(100),JA(2500),IA(101)                         
      INTEGER I,IPVT(100),IPVTB(100),IQ(8),I1,I2,J                      
      INTEGER K,KASE,KB,KBFAIL,KOUNT,KP1,KSING,KSUSP(8)                 
      INTEGER L,LDA,LDAB,IWRITE,M,ML,MU,N,NM1,NPRINT                    
      REAL AINORM,ANORM,SMACH,COND,COND1,EN,ENORM,EPS, EPS1             
      REAL ETNORM,FNI,FNORM,ONEPX,RCOND,RCONDB,RNORM                    
      REAL RTNORM,Q(10),QS(10),SCASUM,XNORM                             
      REAL AIN                                                          
      REAL EB(2),XN(2),EBM(2),EBF(2),EBM2(2),EBF2(2)                    
      REAL AFNORM,AMNORM,ANRM,CONDF1,CONDF,CONDM1,CONDM,GROWTH          
      COMPLEX A(100,100),AS(2500),AINV(100,100),ASAVE(100,100),UL(2500) 
      COMPLEX XEXACT(100,2),B(100,2),BM(100,2),BM2(100,2),BF(100,2)     
      COMPLEX BF2(100,2)                                                
      COMPLEX SPSAV(2500),Z(200),AA(2500)                               
      LOGICAL KBF                                                       
      EXTERNAL IIROW,AROW                                               
      COMMON /SPA/ IIA,JJA,AA                                           
      COMMON /CSTAK/ D(6000)                                            
      CALL ISTKIN(6000,3)                                               
C                                                                       
      LDA = 100                                                         
      IAMAX = 2500                                                      
      MAXUL = 2500                                                      
      IWMAX = 2500                                                      
      CALL ENTER(1)                                                     
C                                                                       
C     WRITE MATRIX AND SOLUTIONS IF  N .LE. NPRINT                      
C                                                                       
      NPRINT =3                                                         
C                                                                       
      WRITE (IWRITE,480)                                                
      WRITE (IWRITE,770)                                                
C                                                                       
      DO 10 I = 1, 8                                                    
         KSUSP(I) = 0                                                   
 10   CONTINUE                                                          
      KSING = 0                                                         
      KBFAIL = 0                                                        
C                                                                       
C     SET EPS TO ROUNDING UNIT                                          
C                                                                       
      EPS =R1MACH(4)                                                    
      WRITE (IWRITE,490) EPS                                            
      WRITE (IWRITE,470)                                                
C                                                                       
C     START MAIN LOOP                                                   
C                                                                       
      KASE = 1                                                          
 20   CONTINUE                                                          
C                                                                       
C        GENERATE TEST MATRIX                                           
C                                                                       
         CALL CGEXX(A,LDA,N,KASE,IWRITE)                                
C                                                                       
C        N = 0 SIGNALS NO MORE TEST MATRICES                            
C                                                                       
C     ...EXIT                                                           
         IF (N .LE. 0) GO TO 460                                        
         ANORM = 0.0E0                                                  
         DO 30 J = 1, N                                                 
            ANORM = AMAX1(ANORM,SCASUM(N,A(1,J),1))                     
 30      CONTINUE                                                       
         WRITE (IWRITE,650) ANORM                                       
C                                                                       
         IF (N .GT. NPRINT) GO TO 50                                    
            WRITE (IWRITE,470)                                          
            DO 40 I = 1, N                                              
               WRITE (IWRITE,690) (A(I,J), J = 1, N)                    
 40         CONTINUE                                                    
            WRITE (IWRITE,470)                                          
 50      CONTINUE                                                       
C                                                                       
C        GENERATE EXACT SOLUTION                                        
C                                                                       
         XEXACT(1,1) = (1.0,1.0)                                        
         XEXACT(1,2) = (1.0,1.0)                                        
         XEXACT(2,2) = (1.0,1.0)                                        
         IF (N .GE. 2) XEXACT(2,1) = (0.0,0.0)                          
         IF (N .LE. 2) GO TO 70                                         
            DO 60 I = 3, N                                              
               XEXACT(I,1) = -XEXACT(I-2,1)                             
                XEXACT(I,2) = (1.0,1.0)                                 
 60         CONTINUE                                                    
 70      CONTINUE                                                       
C                                                                       
C        SAVE MATRIX AND GENERATE R.H.S.                                
C                                                                       
         DO 90 I = 1, N                                                 
            DO 80 J = 1, N                                              
               ASAVE(I,J) = A(I,J)                                      
 80         CONTINUE                                                    
 90      CONTINUE                                                       
         CALL CGEML(N,A,LDA,XEXACT,B)                                   
         CALL CGEML(N,A,LDA,XEXACT(1,2),B(1,2))                         
         NN=LDA*2                                                       
         CALL MOVEFC(NN,B,BM)                                           
         CALL MOVEFC(NN,B,BF)                                           
         CALL MOVEFC(NN,B,BM2)                                          
         CALL MOVEFC(NN,B,BF2)                                          
      CALL CGETSP(N,A, LDA, IA,JA,AS,MAXA)                              
      CALL CGETSP(N,A,LDA,IIA,JJA,AA,MAXA)                              
         DO 100 I=1,MAXA                                                
            SPSAV(I)=AS(I)                                              
            JSPSAV(I)=JA(I)                                             
 100     CONTINUE                                                       
         NP1=N+1                                                        
C                                                                       
C CHECK ORDERING SUBROUTINES                                            
C                                                                       
         CALL SPMOR(N,IA,JA,MC,MIC)                                     
         CALL SPFOR(N,IIROW,FC)                                         
         IBAD=0                                                         
         DO 120 I=1,N                                                   
            MR(I)=MC(I)                                                 
            FR(I)=FC(I)                                                 
            IF (MC(I).EQ.FC(I)) GO TO 120                               
            IBAD=IBAD+1                                                 
            WRITE(IWRITE,110)I,MC(I),FC(I)                              
 110         FORMAT(19H ORDER DISAGREEMENT,3I5)                         
 120    CONTINUE                                                        
        IF (IBAD.EQ.0)WRITE(IWRITE,130)                                 
 130     FORMAT(28H NO DISAGREEMENT IN ORDERING)                        
C                                                                       
C        FACTOR AND ESTIMATE CONDITION                                  
C                                                                       
         CALL CGECE(N,A,LDA,IPVT,RCOND)                                 
         IF (NERROR(INE).NE.0) CALL  ERROFF                             
C                                                                       
C                                                                       
C        FACTOR SPARSE FORM AND COMPARE                                 
C                                                                       
        CALL CSPMCE(N,MR,MC,AS,IA,JA,IAMAX,IL,ISIZE,CONDM,Z)            
        IF(NERROR(INM).NE.0) CALL ERROFF                                
         CALL CSPFCE(N,FR,FC,AROW,IWORK,UL,MAXUL,ISIZE,CONDF,Z)         
         IF (NERROR(INF).NE.0) CALL ERROFF                              
        WRITE(IWRITE,140)                                               
 140     FORMAT(21H CONDITION ESTIMATION)                               
        WRITE(IWRITE,150)                                               
 150     FORMAT(42H    GENERAL        SPARSE M       SPARSE F)          
        WRITE(IWRITE,160)RCOND,CONDM,CONDF                              
 160    FORMAT(1H ,3E15.5)                                              
C                                                                       
C                                                                       
C           TEST FOR SINGULARITY                                        
C                                                                       
          IF (INE+INM+INF.EQ.0)GO TO 190                                
            WRITE(IWRITE,170)INE,INM,INF                                
 170         FORMAT(13H INE, INM,INF,3I5)                               
 180          FORMAT(25H SPARSE ROUTINES DISAGREE,2I4)                  
               WRITE (IWRITE,500)                                       
               KSING = KSING + 1                                        
            GO TO 440                                                   
 190        CONTINUE                                                    
C                                                                       
C              COMPUTE INVERSE AND COND1 = TRUE CONDITION               
C                                                                       
               DO 210 J = 1, N                                          
                  DO 200 I = 1, N                                       
                     AINV(I,J) = (0.0,0.0)                              
 200              CONTINUE                                              
               AINV(J,J)=(1.0,0.0)                                      
 210           CONTINUE                                                 
               CALL CGEFS(N,A,LDA,AINV,LDA,N,IPVT)                      
               CALL CGEBS(N,A,LDA,AINV,LDA,N)                           
               AINORM=ANRM(N,ASAVE,LDA,AINV,LDA, ANORM, COND1)          
               WRITE (IWRITE,520) COND1                                 
               DO 230 I=1,N                                             
                  DO 220 J=1,N                                          
                     AINV(I,J)=(0.0,0.0)                                
 220               CONTINUE                                             
                  AINV(I,I)=(1.0,0.0)                                   
 230           CONTINUE                                                 
               CALL CSPMSL(N,MR,MC,IA,JA,AS,IL,AINV,LDA,N)              
               AMNORM=ANRM(N,ASAVE,LDA,AINV,LDA,ANORM,CONDM1)           
               WRITE(IWRITE,260)CONDM1                                  
               DO 250 I=1,N                                             
                   DO 240 J=1,N                                         
                      AINV(I,J)=(0.0,0.0)                               
 240               CONTINUE                                             
                   AINV(I,I)=(1.0,0.0)                                  
 250           CONTINUE                                                 
               CALL CSPFSL(N,FR,FC,IWORK,UL,AINV,LDA,N)                 
               AFNORM=ANRM(N,ASAVE,LDA,AINV,LDA,ANORM,CONDF1)           
               WRITE(IWRITE,270)CONDF1                                  
 260           FORMAT(19H COND FROM AINV*A M,E15.5)                     
 270           FORMAT(19H COND FROM AINV*A F,E15.5)                     
C                                                                       
C              SOLVE  A*X = B                                           
C                                                                       
               CALL CGEFS(N,A,LDA,B,LDA,2,IPVT)                         
               CALL CGEBS(N,A,LDA,B,LDA,2)                              
C                                                                       
C              MORE SPAR COMPARE                                        
C                                                                       
               DO 280 I=1,N                                             
                  MC(I)=MR(I)                                           
                  FC(I)=FR(I)                                           
 280           CONTINUE                                                 
               CALL SPMSF(N,MR,MIC,IA,JSPSAV,IWORK,IWMAX,IFILL)         
               DO 290 J=1,N                                             
                  I=MR(J)                                               
                  NUM=IA(I+1)-IA(I)                                     
                 IB=IA(I)                                               
                   CALL CSPMIN(N,MIC,IWORK,J,SPSAV(IB),JSPSAV(IB),NUM,  
     1          J,UL)                                                   
 290           CONTINUE                                                 
              EPS1=EPS*ANORM                                            
               CALL CSPMNF(N,IWORK,UL,EPS1,GROWTH)                      
               IF (NERROR(IMNF).EQ.0) GO TO 300                         
                 CALL ERROFF                                            
                 GO TO 310                                              
 300           CONTINUE                                                 
               CALL CSPSOL(N,MR,MC,IWORK,UL,BM,LDA,2)                   
 310           CONTINUE                                                 
               CALL SPFSF(N,MR,MC,IIROW,IWORK,IWMAX,IFILL)              
               CALL CSPFNF(N,MR,MC,AROW,IWORK,UL,GROWTH,EPS1)           
               IF (NERROR(IFNF).EQ.0) GO TO 320                         
                  CALL ERROFF                                           
                  GO TO 330                                             
 320           CONTINUE                                                 
               CALL CSPSOL(N,MR,MC,IWORK,UL,BF,LDA,2)                   
 330           CONTINUE                                                 
               CALL CSPMLE(N,.TRUE.,IA,JSPSAV,SPSAV,ISIZE,BM2,LDA,2)    
               IF(NERROR(IMLE).NE.0)CALL ERROFF                         
               CALL CSPFLE(N,.TRUE.,AROW,ISIZE,BF2,LDA,2)               
                IF (NERROR(IFLE).NE.0) CALL ERROFF                      
C COMPUTE ERRORS IN SOLUTION WITH 2 RIGHT HAND SIDES                    
               IF(IFLE+IMLE+IMNF+IFNF.EQ.0)GO TO 350                    
                 WRITE(IWRITE,340)IMNF,IFNF,IMLE,IFLE                   
 340    FORMAT(42H NUMERICALLY SINGULAR- IMNF,IFNF,IMLE,IFLE,4I5)       
                GO TO 440                                               
 350           CONTINUE                                                 
               DO 360 I=1,2                                             
                  XN(I)=SCASUM(N,B(1,I),1)                              
                  CALL CAXPY(N,(-1.0,0.0),XEXACT(1,I),1,B(1,I),1)       
                  CALL CAXPY(N,(-1.0,0.0),XEXACT(1,I),1,BM(1,I),1)      
                  CALL CAXPY(N,(-1.0,0.0),XEXACT(1,I),1,BF(1,I),1)      
                  CALL CAXPY(N,(-1.0,0.0),XEXACT(1,I),1,BM2(1,I),1)     
                  CALL CAXPY(N,(-1.0,0.0),XEXACT(1,I),1,BF2(1,I),1)     
                  EB(I)=SCASUM(N,B(1,I),1)                              
                  EBM(I)=SCASUM(N,BM(1,I),1)                            
                  EBF(I)=SCASUM(N,BF(1,I),1)                            
                  EBM2(I)=SCASUM(N,BM2(1,I),1)                          
                  EBF2(I)=SCASUM(N,BF2(1,I),1)                          
 360              CONTINUE                                              
                  WRITE(IWRITE,370)                                     
 370              FORMAT(29H ERRORS IN SOLUTION -ABSOLUTE)              
                  WRITE(IWRITE,380)                                     
 380           FORMAT(15X,42H GENERAL SPMN     SPFN       SPMLE   SPFLE)
                  DO 400 I=1,2                                          
                  WRITE(IWRITE,390)I,EB(I),EBM(I),EBF(I),EBM2(I),EBF2(I)
 390      FORMAT(8H PROBLEM,I3,5E15.5)                                  
 400              CONTINUE                                              
C                                                                       
C                                                                       
               Q(1) = RCOND/COND1                                       
               Q(2) = CONDM/COND1                                       
               Q(3) = COND1/RCOND                                       
               Q(4)=COND1/CONDM                                         
               Q(5) = EB(1)/(EPS*COND1*XN(1))                           
               Q(6) = EBM(1)/(EPS*COND1*XN(1))                          
               Q(7) = EBM2(1)/(EPS*COND1*XN(1))                         
               Q(8) = AINORM/(EPS*COND1)                                
               Q(9)=AMNORM/(EPS*COND1)                                  
               WRITE (IWRITE,470)                                       
               WRITE (IWRITE,560)                                       
               WRITE (IWRITE,470)                                       
               WRITE (IWRITE,620)                                       
               WRITE (IWRITE,630)                                       
               WRITE (IWRITE,640)                                       
               WRITE (IWRITE,470)                                       
               WRITE (IWRITE,680) (Q(I), I = 1, 9)                      
               WRITE (IWRITE,470)                                       
C                                                                       
C              LOOK FOR SUSPICIOUS RATIOS                               
C                                                                       
               QS(1) = 1.0E0 + 4.0E0*EPS                                
               QS(2)=QS(1)                                              
               QS(4) = 10.0E0                                           
               QS(3)=10.0E0                                             
               EN = FLOAT(N)                                            
               IF (N .EQ. 1) EN = 2.0E0                                 
               DO 410 I = 3, 10                                         
                  QS(I) = EN                                            
 410           CONTINUE                                                 
               KOUNT = 0                                                
               DO 430 I = 1, 9                                          
                  IQ(I) = 0                                             
                  IF (Q(I) .LE. QS(I)) GO TO 420                        
                     IQ(I) = 1                                          
                     KSUSP(I) = KSUSP(I) + 1                            
                     KOUNT = KOUNT + 1                                  
 420              CONTINUE                                              
 430           CONTINUE                                                 
               IF (KOUNT .EQ. 0) WRITE (IWRITE,750)                     
               IF (KOUNT .NE. 0) WRITE (IWRITE,760) (IQ(I), I = 1, 9)   
               WRITE (IWRITE,470)                                       
 440        CONTINUE                                                    
 450     CONTINUE                                                       
C                                                                       
         WRITE (IWRITE,570)                                             
         KASE = KASE + 1                                                
      GO TO 20                                                          
 460  CONTINUE                                                          
C                                                                       
C     FINISH MAIN LOOP                                                  
C                                                                       
C     SUMMARY                                                           
C                                                                       
      WRITE (IWRITE,580)                                                
      KASE = KASE - 1                                                   
      WRITE (IWRITE,590) KASE                                           
      WRITE (IWRITE,600) KSING                                          
      WRITE (IWRITE,610) KSUSP                                          
      WRITE (IWRITE,730)                                                
      CALL LEAVE                                                        
      RETURN                                                            
C                                                                       
C     MOST FORMATS, ALSO SOME IN SGEXX                                  
C                                                                       
 470  FORMAT (1H )                                                      
 480  FORMAT (29H1  PORT  TESTER, CGE**, CSP**)                         
 490  FORMAT ( / 14H EPSILON     =, 1PE13.5)                            
 500  FORMAT ( / 19H EXACT SINGULARITY. /)                              
 510  FORMAT ( / 16H MAYBE SINGULAR. /)                                 
 520  FORMAT (14H ACTUAL COND =, 1PE13.5)                               
 530  FORMAT(/14H X AND XBAND =)                                        
 540  FORMAT (14H ERROR NORMS =, 2(1PE13.5))                            
 550  FORMAT (14H RESID NORMS =, 2(1PE13.5))                            
 560  FORMAT (26H TEST RATIOS.. E = EPSILON)                            
 570  FORMAT ( / 14H ************* /)                                   
 580  FORMAT (8H1SUMMARY)                                               
 590  FORMAT (18H NUMBER OF TESTS =, I4)                                
 600  FORMAT (30H NUMBER OF SINGULAR MATRICES =, I4)                    
 610  FORMAT (30H NUMBER OF SUSPICIOUS RATIOS =, 10I4)                  
 620  FORMAT(40H    COND  COND(M)  ACTUAL  ACTUAL  ERROR,               
     1       36H ERROR(M) ERROR(M2) A*AI-I A*AI-I(M))                   
 630  FORMAT (9(8H   -----))                                            
 640  FORMAT(42H    ACTUAL ACTUAL   COND  COND(M) E*COND*X,             
     1       31H E*CONDX E*COND*X E*COND E*COND)                        
 650  FORMAT (14H NORM(A)     =, 1PE13.5)                               
 660  FORMAT (14H NORM(A - LU)=, 1PE13.5)                               
 670  FORMAT (14H NORM(A*AI-I)=, 2(1PE13.5))                            
 680  FORMAT (10(1X, F7.2))                                             
 690  FORMAT (1H , 6G11.4)                                              
 700  FORMAT (14H 1/COND      =, 1PE13.5)                               
 710  FORMAT (2G14.6)                                                   
 720  FORMAT (5H ML =, I2, 6H  MU =, I2)                                
 730  FORMAT ( / 12H END OF TEST)                                       
 740  FORMAT(7H COND =,1PE13.5,13H COND(BAND) =,1PE13.5 /)              
 750  FORMAT (21H NO SUSPICIOUS RATIOS)                                 
 760  FORMAT (I8, 9I10 / 7X, 28H1 INDICATES SUSPICIOUS RATIO)           
 770  FORMAT (29H THIS VERSION DATED 03/11/78.)                         
      END                                                               
      SUBROUTINE CGEXX(A,LDA,N,KASE,IWRITE)                             
      INTEGER LDA,N,KASE,IWRITE                                         
      COMPLEX A(LDA,1)                                                  
C                                                                       
C     GENERATES COMPLEX GENERAL TEST MATRICES                           
C                                                                       
C     EXTERNAL CMACH                                                    
C     FORTRAN CMPLX,FLOAT,MAX0                                          
      COMPLEX T1,T2                                                     
      REAL CMACH,HUGE,TINY                                              
      INTEGER I,J,IRAND                                                 
C                                                                       
      GO TO (10, 10, 10, 60, 60, 80, 80, 80, 120, 160, 200, 240, 280,   
     *       320, 360, 410, 460, 520), KASE                             
C                                                                       
C     KASE 1, 2 AND 3                                                   
C                                                                       
 10   CONTINUE                                                          
         N = 3*KASE                                                     
         WRITE (IWRITE,20) KASE,N                                       
 20      FORMAT (5H KASE, I3, 3X, 16HHILBERT SLICE    / 4H N =, I4)     
         DO 50 J = 1, N                                                 
            DO 40 I = 1, N                                              
               A(I,J) = (0.0E0,0.0E0)                                   
               IF (I .GT. J + 2) GO TO 30                               
               IF (I .LT. J - 3) GO TO 30                               
                  A(I,J) = (1.0E0,0.0E0)/CMPLX(FLOAT(I+J-1),1.0E0)      
 30            CONTINUE                                                 
 40         CONTINUE                                                    
 50      CONTINUE                                                       
      GO TO 530                                                         
C                                                                       
C     KASE 4 AND 5                                                      
C                                                                       
 60   CONTINUE                                                          
         N = 1                                                          
         WRITE (IWRITE,70) KASE,N                                       
 70      FORMAT (5H KASE, I3, 3X, 16HMONOELEMENTAL    / 4H N =, I4)     
         IF (KASE .EQ. 4) A(1,1) = (3.0E0,1.0E0)                        
         IF (KASE .EQ. 5) A(1,1) = (0.0E0,0.0E0)                        
      GO TO 530                                                         
C                                                                       
C     KASE 6, 7 AND 8                                                   
C                                                                       
 80   CONTINUE                                                          
         N = 15                                                         
         WRITE (IWRITE,90) KASE,N                                       
 90      FORMAT (5H KASE, I3, 3X, 16HTRIDIAGONAL      / 4H N =, I4)     
         T1 = (1.0E0,0.0E0)                                             
         T2 = (1.0E0,0.0E0)                                             
         IF (KASE .EQ. 7) T1 = (100.0E0,100.0E0)                        
         IF (KASE .EQ. 8) T2 = (100.0E0,100.0E0)                        
         DO 110 I = 1, N                                                
            DO 100 J = 1, N                                             
               A(I,J) = (0.0E0,0.0E0)                                   
               IF (I .EQ. J) A(I,I) = (4.0E0,0.0E0)                     
               IF (I .EQ. J - 1) A(I,J) = T1                            
               IF (I .EQ. J + 1) A(I,J) = T2                            
 100        CONTINUE                                                    
 110     CONTINUE                                                       
      GO TO 530                                                         
C                                                                       
C     KASE 9                                                            
C                                                                       
 120  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,130) KASE,N                                      
 130     FORMAT (5H KASE, I3, 3X, 16HRANK ONE         / 4H N =, I4)     
         DO 150 I = 1, N                                                
            DO 140 J = 1, N                                             
               A(I,J) = CMPLX(10.0E0**(I-J),0.0E0)                      
 140        CONTINUE                                                    
 150     CONTINUE                                                       
      GO TO 530                                                         
C                                                                       
C     KASE 10                                                           
C                                                                       
 160  CONTINUE                                                          
         N = 4                                                          
         WRITE (IWRITE,170) KASE,N                                      
 170     FORMAT (5H KASE, I3, 3X, 16HZERO COLUMN      / 4H N =, I4)     
         DO 190 I = 1, N                                                
            DO 180 J = 1, N                                             
               T1 = CMPLX(FLOAT(J-3),0.0E0)                             
               T2 = CMPLX(FLOAT(I),0.0E0)                               
               A(I,J) = T1/T2                                           
 180        CONTINUE                                                    
 190     CONTINUE                                                       
      GO TO 530                                                         
C                                                                       
C     KASE 11                                                           
C                                                                       
 200  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,210) KASE,N                                      
 210     FORMAT (5H KASE, I3, 3X, 16HTEST COND        / 4H N =, I4)     
         DO 230 I = 1, N                                                
            DO 220 J = 1, N                                             
               IF (I .EQ. J) A(I,J) = CMPLX(FLOAT(I),0.0E0)             
               IF (I .GT. J) A(I,J) = CMPLX(FLOAT(J-2),0.0E0)           
               IF (I .LT. J) A(I,J) = CMPLX(FLOAT(I-2),0.0E0)           
 220        CONTINUE                                                    
 230     CONTINUE                                                       
      GO TO 530                                                         
C                                                                       
C     KASE 12                                                           
C                                                                       
 240  CONTINUE                                                          
         N = 3                                                          
         WRITE (IWRITE,250) KASE,N                                      
 250     FORMAT (5H KASE, I3, 3X, 16HIDENTITY         / 4H N =, I4)     
         DO 270 I = 1, N                                                
            DO 260 J = 1, N                                             
               IF (I .EQ. J) A(I,I) = (1.0E0,0.0E0)                     
               IF (I .NE. J) A(I,J) = (0.0E0,0.0E0)                     
 260        CONTINUE                                                    
 270     CONTINUE                                                       
      GO TO 530                                                         
C                                                                       
C     KASE 13                                                           
C                                                                       
 280  CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,290) KASE,N                                      
 290     FORMAT (5H KASE, I3, 3X, 16HUPPER TRIANGULAR / 4H N =, I4)     
         DO 310 I = 1, N                                                
            DO 300 J = 1, N                                             
               IF (I .GT. J) A(I,J) = (0.0E0,0.0E0)                     
               IF (I .LE. J) A(I,J) = CMPLX(FLOAT(J-I+1),FLOAT(J-I))    
 300        CONTINUE                                                    
 310     CONTINUE                                                       
      GO TO 530                                                         
C                                                                       
C     KASE 14                                                           
C                                                                       
 320  CONTINUE                                                          
         N = 6                                                          
         WRITE (IWRITE,330) KASE,N                                      
 330     FORMAT (5H KASE, I3, 3X, 16HLOWER TRIANGULAR / 4H N =, I4)     
         DO 350 I = 1, N                                                
            DO 340 J = 1, N                                             
               IF (I .LT. J) A(I,J) = (0.0E0,0.0E0)                     
               IF (I .GE. J) A(I,J) = CMPLX(FLOAT(I-J+1),-1.0E0)        
 340        CONTINUE                                                    
 350     CONTINUE                                                       
      GO TO 530                                                         
C                                                                       
C     KASE 15                                                           
C                                                                       
 360  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,370) KASE,N                                      
 370     FORMAT (5H KASE, I3, 3X, 16HNEAR UNDERFLOW   / 4H N =, I4)     
         TINY =R1MACH(1)*FLOAT(N*N)*100.0                               
         TINY=SQRT(TINY)                                                
         WRITE (IWRITE,380) TINY                                        
 380     FORMAT (14H TINY        =, 1PE13.5)                            
         DO 400 I = 1, N                                                
            DO 390 J = 1, N                                             
               A(I,J) = CMPLX(TINY*FLOAT(J)/FLOAT(MAX0(I,J)),0.0E0)     
 390        CONTINUE                                                    
 400     CONTINUE                                                       
      GO TO 530                                                         
C                                                                       
C     KASE 16                                                           
C                                                                       
 410  CONTINUE                                                          
         N = 5                                                          
         WRITE (IWRITE,420) KASE,N                                      
 420     FORMAT (5H KASE, I3, 3X, 16HNEAR OVERFLOW    / 4H N =, I4)     
         HUGE= SQRT(R1MACH(2))/FLOAT(200*N*N)                           
         WRITE (IWRITE,430) HUGE                                        
 430     FORMAT (14H HUGE        =, 1PE13.5)                            
         DO 450 I = 1, N                                                
            DO 440 J = 1, N                                             
               A(I,J) = CMPLX(HUGE*FLOAT(J)/FLOAT(MAX0(I,J)),0.0E0)     
 440        CONTINUE                                                    
 450     CONTINUE                                                       
      GO TO 530                                                         
C                                                                       
 460  CONTINUE                                                          
C                                                                       
C THIS IS RANDOM SPARSE ROUTINE                                         
C                                                                       
        WRITE(IWRITE,470)KASE,N                                         
 470    FORMAT(5H KASE, I3,14H RANDOM SPARSE,/4H N =, I4)               
        DO 490 I=1,N                                                    
           DO 480 J=1,N                                                 
              A(I,J)=(0.0E0,0.0E0)                                      
 480       CONTINUE                                                     
 490     CONTINUE                                                       
        K=N/2                                                           
         DO 510 I=1,N                                                   
            J=0                                                         
            IRAND = UNI(0)                                              
 500        JJ=J+K*IRAND+1                                              
             IF (J.LT.I.AND.JJ.GT.I) JJ=I                               
             J=JJ                                                       
             IF (J.GT.N) GO TO 510                                      
               A(I,J)=CMPLX(UNI(0),UNI(0))                              
               GO TO 500                                                
 510     CONTINUE                                                       
         GO TO 530                                                      
C                                                                       
 520       CONTINUE                                                     
         N = 0                                                          
 530  CONTINUE                                                          
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE AROW(I, ROW, JCOL, NUM)                                
      INTEGER JCOL(200), JPIB, IA(101), JA(2500)                        
      COMPLEX ROW(200),A(2500)                                          
      COMMON /SPA/IA,JA,A                                               
       NUM=IA(I+1)-IA(I)                                                
      IB=IA(I)-1                                                        
      DO 10 J=1,NUM                                                     
          JPIB = J+IB                                                   
          ROW(J)=A(JPIB)                                                
          JCOL(J)=JA(JPIB)                                              
 10   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE IIROW(I,  JCOL, NUM)                                   
      INTEGER JCOL(200),JPIB,IA(101),JA(2500)                           
      COMPLEX A(2500)                                                   
      COMMON /SPA/IA,JA,A                                               
      NUM=IA(I+1)-IA(I)                                                 
      IB=IA(I)-1                                                        
      DO 10 J=1,NUM                                                     
          JPIB = J+IB                                                   
          JCOL(J)=JA(JPIB)                                              
 10   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      REAL FUNCTION ANRM(N,AS,LDA1,AINV,LDA,ANORM,COND1)                
      COMPLEX AS(LDA,N),AINV(LDA1,N)                                    
C                                                                       
C     THIS SUBROUTINE COMPUTES THE CONDITION NUMBER FROM                
C     A AND AINVERSE                                                    
C                                                                       
C     IT ALSO COMPUTES THE ERROR A*AINV-I AND STORES IT IN ANRM         
      COMPLEX B(200),T                                                  
      ANRM=0.0E0                                                        
      DO 30 J =1,N                                                      
         DO 10 I=1,N                                                    
            B(I)=(0.0,0.0)                                              
 10      CONTINUE                                                       
         DO 20 K=1,N                                                    
            T=AINV(K,J)                                                 
            CALL CAXPY(N,T,AS(1,K),1,B,1)                               
 20       CONTINUE                                                      
         B(J)=B(J)-(1.0,0.0)                                            
         ANRM=AMAX1(ANRM,SCASUM(N,B,1))                                 
 30   CONTINUE                                                          
      AIN=0.0E0                                                         
      DO 40 I=1,N                                                       
         AIN=AMAX1(AIN,SCASUM(N,AINV(1,I),1))                           
 40   CONTINUE                                                          
      COND1=ANORM*AIN                                                   
      RETURN                                                            
      END                                                               
         SUBROUTINE CGETSP(N,A,LDA,IA,JA,AS,L)                          
         COMPLEX A(LDA,N),AS(1)                                         
         INTEGER JA(1),IA(1)                                            
         L=0                                                            
         IA(1)=1                                                        
         DO 30 I=1,N                                                    
            DO 10 J=1,N                                                 
               IF (CABS1(A(I,J)).EQ.0.0E0) GO TO 10                     
               L=L+1                                                    
               AS(L)=A(I,J)                                             
               JA(L)=J                                                  
 10         CONTINUE                                                    
           IF (L.GE.IA(I))GO TO 20                                      
               L=L+1                                                    
               AS(L)=(0.0,0.0)                                          
               JA(L)=1                                                  
 20        CONTINUE                                                     
            IA(I+1)=L+1                                                 
 30     CONTINUE                                                        
        RETURN                                                          
        END                                                             
C$TEST SVDA                                                             
C***********************************************************************
C                                                                       
C  FIRST TEST OF THE PORT PROGRAM SVD                                   
C                                                                       
C***********************************************************************
C TSVD.F--FIRST TEST PROGRAM FOR SVD                                    
       INTEGER NAU, NV, I, J, K                                         
       INTEGER M, N, IWRITE, I1MACH                                     
       REAL A(15,5), V(5,5), W(5)                                       
       REAL U(15,5)                                                     
       REAL TMP, MAX, UNI                                               
       LOGICAL MATU, MATV                                               
       DOUBLE PRECISION DSTAK(500)                                      
       REAL RSTAK(1000)                                                 
       COMMON /CSTAK/DSTAK                                              
       EQUIVALENCE (DSTAK(1),RSTAK(1))                                  
C                                                                       
       CALL ISTKIN(500,4)                                               
       IWRITE = I1MACH(2)                                               
       M = 15                                                           
       N = 5                                                            
       NAU = 15                                                         
       NV = 5                                                           
       MATU = .TRUE.                                                    
       MATV = .TRUE.                                                    
C SET UP THE RANDOM MATRIX TO BE DECOMPOSED                             
       DO 10 I=1,M                                                      
          DO 10 J=1,N                                                   
             A(I,J) = UNI(0) * 10.0                                     
 10    CONTINUE                                                         
C CALL THE SINGULAR VALUE DECOMPOSITION PACKAGE                         
       CALL SVD(M,N,A,NAU,U,MATU,W,V,NV,MATV)                           
C CHECK THAT THE DECOMPOSITION RETURNED IS CORRECT                      
C COMPUTE U*W*TRANSPOSE(V)                                              
       MAX = 0.                                                         
       DO 30 I=1,N                                                      
          DO 30 J=1,M                                                   
              TMP = 0                                                   
              DO 20 K=1,N                                               
                  TMP = TMP + U(J,K)*V(I,K)*W(K)                        
 20           CONTINUE                                                  
C CALCULATE INFINITY NORM                                               
              TMP = TMP - A(J,I)                                        
              IF (ABS(TMP).GT.MAX) MAX = ABS(TMP)                       
 30    CONTINUE                                                         
C PRINT OUT RESULTS                                                     
       WRITE (IWRITE,40) MAX                                            
 40       FORMAT (33H NORM OF (A - U*W*TRANSPOSE(V)) -,E15.8)           
       WRITE (IWRITE,50) (W(I), I=1,N)                                  
 50       FORMAT (3H W-,5E15.8)                                         
       STOP                                                             
       END                                                              
C$TEST SVDB                                                             
C***********************************************************************
C                                                                       
C  SECOND TEST OF THE PORT PROGRAM SVD                                  
C                                                                       
C***********************************************************************
C TSVD1.F--SECOND TEST PROGRAM FOR SVD                                  
       INTEGER NAU, NV, I, J, K                                         
       INTEGER M, N, IWRITE, I1MACH                                     
       REAL A(15,5), V(5,5), W(5)                                       
       REAL U(15,5)                                                     
       REAL TMP(15,5), EPS                                              
       REAL TEMP, R1MACH, MAX                                           
       LOGICAL MATU, MATV                                               
       DOUBLE PRECISION DSTAK(500)                                      
       REAL RSTAK(1000)                                                 
       COMMON /CSTAK/DSTAK                                              
       EQUIVALENCE (DSTAK(1),RSTAK(1))                                  
C                                                                       
       IWRITE = I1MACH(2)                                               
       CALL ISTKIN(500,4)                                               
       M = 4                                                            
       N = 3                                                            
       NAU = 15                                                         
       NV = 5                                                           
       MATU = .TRUE.                                                    
       MATV = .TRUE.                                                    
C SET UP THE RANDOM MATRIX TO BE DECOMPOSED                             
       A(1,1) = 1.                                                      
       A(1,2) = 2.                                                      
       A(1,3) = 3.                                                      
       A(2,1) = 3.                                                      
       A(2,2) = 4.                                                      
       A(2,3) = 7.                                                      
       A(3,1) = 5.                                                      
       A(3,2) = 6.                                                      
       A(3,3) = 11.                                                     
       A(4,1) = 7.                                                      
       A(4,2) = 8.                                                      
       A(4,3) = 15.                                                     
C CALL THE SINGULAR VALUE DECOMPOSITION PACKAGE                         
       CALL SVD(M,N,A,NAU,U,MATU,W,V,NV,MATV)                           
       EPS = R1MACH(4) * 1.E03                                          
C CHECK THAT THE DECOMPOSITION RETURNED IS CORRECT                      
C COMPUTE U*W                                                           
       MAX = 0.                                                         
       DO 10 J=1,N                                                      
          DO 10 I=1,M                                                   
             TMP(I,J) = U(I,J) * W(J)                                   
 10    CONTINUE                                                         
C COMPUTE U*W*TRANSPOSE(V)                                              
       DO 40 I=1,N                                                      
          DO 40 J=1,M                                                   
              DO 20 K=1,N                                               
                  TEMP = TEMP + TMP(J,K)*V(I,K)                         
 20           CONTINUE                                                  
C CALCULATE INFINITY NORM                                               
              TEMP = TEMP - A(J,I)                                      
              IF (ABS(TEMP).GT.MAX) MAX = ABS(TEMP)                     
 30           TEMP = 0                                                  
 40    CONTINUE                                                         
C PRINT OUT RESULTS                                                     
       WRITE (IWRITE,50) MAX                                            
 50    FORMAT (33H NORM OF (A - U*W*TRANSPOSE(V))- ,E15.8)              
       WRITE (IWRITE,60) (W(I), I=1,N)                                  
 60       FORMAT (3H W-,5E15.8)                                         
       STOP                                                             
       END                                                              
C$TEST SVDC                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM SVDLS                                       
C                                                                       
C***********************************************************************
C TSVDLS.F--TEST PROGRAM FOR SVDLS                                      
       INTEGER I,J,K                                                    
       INTEGER M, N, IWRITE, I1MACH                                     
       REAL A(10,2), V(2,2), W(2)                                       
       REAL B(10), U(10,2)                                              
       REAL TMP, MAX                                                    
C                                                                       
       IWRITE = I1MACH(2)                                               
       M = 6                                                            
       N = 2                                                            
C SET THE FIRST COLUMN OF THE A ARRAYS TO THE ACTUAL X,                 
C AND THE SECOND COLUMN TO 1.0                                          
       DO 10 I=1,M                                                      
          A(I,1) = FLOAT(I)                                             
          A(I,2) = 1.0                                                  
 10    CONTINUE                                                         
C SET THE VALUES OF THE RIGHT HAND SIDE, B                              
       B(1) = .3                                                        
       B(2) = .95                                                       
       B(3) = 2.6                                                       
       B(4) = 2.5                                                       
       B(5) = 2.3                                                       
       B(6) = 3.95                                                      
C CALL THE SINGULAR VALUE DECOMPOSITION PACKAGE                         
       CALL SVDLS(M,N,A,10,U,.TRUE.,W,V,2,.TRUE.,B,1)                   
       WRITE (IWRITE,20)                                                
 20    FORMAT (4H SVD)                                                  
       WRITE (IWRITE,30) B(1), B(2)                                     
 30    FORMAT (7H C(1)- , E15.8, 7H C(2): , E15.8)                      
       WRITE (IWRITE,40) W(1), W(2)                                     
 40    FORMAT (7H W(1)- , E15.8, 7H W(2): , E15.8)                      
C CHECK THAT THE DECOMPOSITION RETURNED IS CORRECT                      
C COMPUTE U*W*TRANSPOSE(V)                                              
       MAX = 0.                                                         
       DO 60 I=1,N                                                      
          DO 60 J=1,M                                                   
              TMP = 0                                                   
              DO 50 K=1,N                                               
                  TMP = TMP + U(J,K)*V(I,K)*W(K)                        
 50           CONTINUE                                                  
C CALCULATE INFINITY NORM                                               
              TMP = TMP - A(J,I)                                        
              IF (ABS(TMP).GT.MAX) MAX = ABS(TMP)                       
 60    CONTINUE                                                         
C PRINT OUT RESULTS                                                     
       WRITE (IWRITE,70) MAX                                            
 70       FORMAT (33H NORM OF (A - U*W*TRANSPOSE(V)) - ,E15.8)          
       STOP                                                             
       END                                                              
C$TEST SVAD                                                             
C***********************************************************************
C                                                                       
C  FIRST TEST OF THE PORT PROGRAM DSVD                                  
C                                                                       
C***********************************************************************
C TDSVD.F-- FIRST TEST PROGRAM FOR DSVD                                 
       INTEGER NAU, NV, I, J, K                                         
       INTEGER M, N, IWRITE, I1MACH                                     
       DOUBLE PRECISION A(15,5), V(5,5), W(5)                           
       DOUBLE PRECISION U(15,5)                                         
       DOUBLE PRECISION TMP, MAX                                        
       REAL UNI                                                         
       LOGICAL MATU, MATV                                               
       DOUBLE PRECISION DSTAK(500)                                      
       COMMON /CSTAK/DSTAK                                              
C                                                                       
       CALL ISTKIN(500,4)                                               
       IWRITE = I1MACH(2)                                               
       M = 15                                                           
       N = 5                                                            
       NAU = 15                                                         
       NV = 5                                                           
       MATU = .TRUE.                                                    
       MATV = .TRUE.                                                    
C SET UP THE RANDOM MATRIX TO BE DECOMPOSED                             
       DO 10 I=1,M                                                      
          DO 10 J=1,N                                                   
             A(I,J) = DBLE(UNI(0)) * 10.0                               
 10    CONTINUE                                                         
C CALL THE SINGULAR VALUE DECOMPOSITION PACKAGE                         
       CALL DSVD(M,N,A,NAU,U,MATU,W,V,NV,MATV)                          
C CHECK THAT THE DECOMPOSITION RETURNED IS CORRECT                      
C COMPUTE U*W*TRANSPOSE(V)                                              
       MAX = 0.                                                         
       DO 30 I=1,N                                                      
          DO 30 J=1,M                                                   
              TMP = 0                                                   
              DO 20 K=1,N                                               
                  TMP = TMP + U(J,K)*V(I,K)*W(K)                        
 20           CONTINUE                                                  
C CALCULATE INFINITY NORM                                               
              TMP = TMP - A(J,I)                                        
              IF (DABS(TMP).GT.MAX) MAX = DABS(TMP)                     
 30    CONTINUE                                                         
C PRINT OUT RESULTS                                                     
       WRITE (IWRITE,40) MAX                                            
 40       FORMAT (33H NORM OF (A - U*W*TRANSPOSE(V)) -,E15.8)           
       WRITE (IWRITE,50) (W(I), I=1,N)                                  
 50       FORMAT (3H W-,5E15.8)                                         
       STOP                                                             
       END                                                              
C$TEST SVBD                                                             
C***********************************************************************
C                                                                       
C  SECOND TEST OF THE PORT PROGRAM DSVD                                 
C                                                                       
C***********************************************************************
C TDSVD1.F--SECOND TEST PROGRAM FOR DSVD                                
       INTEGER NAU, NV, I, J, K                                         
       INTEGER M, N, IWRITE, I1MACH                                     
       DOUBLE PRECISION A(15,5), V(5,5), W(5)                           
       DOUBLE PRECISION U(15,5)                                         
       DOUBLE PRECISION TMP(15,5), EPS                                  
       DOUBLE PRECISION TEMP, D1MACH, MAX                               
       LOGICAL MATU, MATV                                               
       DOUBLE PRECISION DSTAK(500)                                      
       COMMON /CSTAK/DSTAK                                              
C                                                                       
       IWRITE = I1MACH(2)                                               
       CALL ISTKIN(500,4)                                               
       M = 4                                                            
       N = 3                                                            
       NAU = 15                                                         
       NV = 5                                                           
       MATU = .TRUE.                                                    
       MATV = .TRUE.                                                    
C SET UP THE RANDOM MATRIX TO BE DECOMPOSED                             
       A(1,1) = 1.                                                      
       A(1,2) = 2.                                                      
       A(1,3) = 3.                                                      
       A(2,1) = 3.                                                      
       A(2,2) = 4.                                                      
       A(2,3) = 7.                                                      
       A(3,1) = 5.                                                      
       A(3,2) = 6.                                                      
       A(3,3) = 11.                                                     
       A(4,1) = 7.                                                      
       A(4,2) = 8.                                                      
       A(4,3) = 15.                                                     
C CALL THE SINGULAR VALUE DECOMPOSITION PACKAGE                         
       CALL DSVD(M,N,A,NAU,U,MATU,W,V,NV,MATV)                          
       EPS = D1MACH(4) * 1.D3                                           
C CHECK THAT THE DECOMPOSITION RETURNED IS CORRECT                      
C COMPUTE U*W                                                           
       MAX = 0.                                                         
       DO 10 J=1,N                                                      
          DO 10 I=1,M                                                   
             TMP(I,J) = U(I,J) * W(J)                                   
 10    CONTINUE                                                         
C COMPUTE U*W*TRANSPOSE(V)                                              
       TEMP = 0.D0                                                      
       DO 40 I=1,N                                                      
          DO 40 J=1,M                                                   
              DO 20 K=1,N                                               
                  TEMP = TEMP + TMP(J,K)*V(I,K)                         
 20           CONTINUE                                                  
C CALCULATE INFINITY NORM                                               
              TEMP = TEMP - A(J,I)                                      
              IF (DABS(TEMP).GT.MAX) MAX = DABS(TEMP)                   
 30           TEMP = 0                                                  
 40    CONTINUE                                                         
C PRINT OUT RESULTS                                                     
       WRITE (IWRITE,50) MAX                                            
 50    FORMAT (33H NORM OF (A - U*W*TRANSPOSE(V))- ,E15.8)              
       WRITE (IWRITE,60) (W(I), I=1,N)                                  
 60       FORMAT (3H W-,5E15.8)                                         
       STOP                                                             
       END                                                              
C$TEST SVCD                                                             
C***********************************************************************
C                                                                       
C  THIRD TEST OF THE PORT PROGRAM DSVDLS                                
C                                                                       
C***********************************************************************
C TDSVDLS4.F-- THIRD TEST PROGRAM FOR DSVDLS                            
       INTEGER I, J, K                                                  
       INTEGER M, N, IWRITE, I1MACH                                     
       DOUBLE PRECISION A(10,2), V(2,2), W(2)                           
       DOUBLE PRECISION B(10), U(10,2)                                  
       DOUBLE PRECISION TMP, MAX, DFLOAT                                
C                                                                       
       IWRITE = I1MACH(2)                                               
       M = 6                                                            
       N = 2                                                            
C SET THE FIRST COLUMN OF THE A ARRAYS TO THE ACTUAL X,                 
C AND THE SECOND COLUMN TO 1.D0                                         
       DO 10 I=1,M                                                      
          A(I,1) = DFLOAT(I)                                            
          A(I,2) = 1.D0                                                 
 10    CONTINUE                                                         
C SET THE VALUES OF THE RIGHT HAND SIDE, B                              
       B(1) = 0.3                                                       
       B(2) = 0.95                                                      
       B(3) = 2.6                                                       
       B(4) = 2.5                                                       
       B(5) = 2.3                                                       
       B(6) = 3.95                                                      
C CALL THE SINGULAR VALUE DECOMPOSITION PACKAGE                         
       CALL DSVDLS(M,N,A,10,U,.TRUE.,W,V,2,.TRUE.,B,1)                  
       WRITE (IWRITE,20)                                                
 20    FORMAT (5H DSVD)                                                 
       WRITE (IWRITE,30) B(1), B(2)                                     
 30    FORMAT (7H C(1)- , E15.8, 7H C(2): , E15.8)                      
       WRITE (IWRITE,40) W(1), W(2)                                     
 40    FORMAT (7H W(1)- , E15.8, 7H W(2): , E15.8)                      
C CHECK THAT THE DECOMPOSITION RETURNED IS CORRECT                      
C COMPUTE U*W*TRANSPOSE(V)                                              
       MAX = 0.D0                                                       
       DO 60 I=1,N                                                      
          DO 60 J=1,M                                                   
              TMP = 0                                                   
              DO 50 K=1,N                                               
                  TMP = TMP + U(J,K)*V(I,K)*W(K)                        
 50           CONTINUE                                                  
C CALCULATE INFINITY NORM                                               
              TMP = TMP - A(J,I)                                        
              IF (DABS(TMP).GT.MAX) MAX = DABS(TMP)                     
 60    CONTINUE                                                         
C PRINT OUT RESULTS                                                     
       WRITE (IWRITE,70) MAX                                            
 70       FORMAT (33H NORM OF (A - U*W*TRANSPOSE(V)) - ,E15.8)          
       STOP                                                             
       END                                                              
C$TEST LLZA                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM LZ                                          
C                                                                       
C***********************************************************************
          COMPLEX A(20,20),B(20,20),X(20,20),EIGA(20),EIGB(20)          
          COMPLEX ASAVE(20,20),BSAVE(20,20),EIG(20),EI                  
          INTEGER IP(20), I1MACH, IWRITE                                
          REAL EIGBN(20),EIGN(20),ABNORM,RESERR,RESUM                   
          COMPLEX SUMA,SUMB                                             
          REAL AN,BN,ANORM,BNORM                                        
          INTEGER MR(20,20),MI(20,20),LR(20),LI(20)                     
C                                                                       
C THIS IS TEST PROGRAM FOR LZ                                           
C                                                                       
C                                                                       
C SET UP OUTPUT WRITE UNIT                                              
C                                                                       
      IWRITE = I1MACH(2)                                                
          NM=20                                                         
          WRITE(IWRITE,1)                                               
 1        FORMAT(12H TEST FOR LZ)                                       
          DO 100 ICASE=1,9                                              
C SETUP PROBLEM                                                         
              CALL SETUP(NM,A,B,MR,MI,LR,LI,N,NA,NB,ICASE)              
C                                                                       
C SAVE MATRICES AND FIND THEIR NORM                                     
C                                                                       
              ANORM=0.0                                                 
              BNORM=0.0                                                 
              DO 10 I=1,N                                               
                 AN=0.0                                                 
                 BN=0.0                                                 
                 DO 5 J=1,N                                             
                    ASAVE(I,J)=A(I,J)                                   
                    BSAVE(I,J)=B(I,J)                                   
                    AN=AN+CABS(A(I,J))                                  
                    BN=BN+CABS(B(I,J))                                  
 5               CONTINUE                                               
              ANORM=AMAX1(ANORM,AN)                                     
              BNORM=AMAX1(BNORM,BN)                                     
 10           CONTINUE                                                  
              ABNORM=ANORM+BNORM                                        
C SOLVE GENERALIZED EIGENVALUE PROBLEM                                  
              CALL LZ(N,A,NM,B,NM,X,NM,.TRUE.,EIGA,EIGB)                
C TRY TO FIND ABSOLUTE ERROR WITHOUT DIVISION BY ZERO                   
              DO 20 I=1,N                                               
                 EIGBN(I)=CABS(EIGB(I))                                 
 20           CONTINUE                                                  
              CALL SRTPDR(EIGBN,1,IP,1,N)                               
              DO 30 I=1,N                                               
                 IN=IP(I)                                               
                 EIG(I)=EIGA(IN)/EIGB(IN)                               
                 EIGN(I)=CABS(EIG(I))                                   
 30           CONTINUE                                                  
              CALL SRTPAR(EIGN,1,IP,1,NB)                               
              ERR=0.0                                                   
              DO 40 I=1,NB                                              
                 IN=IP(I)                                               
                 ERR=ERR+CABS(EIG(IN)-CMPLX(FLOAT(I),FLOAT(I)))         
 40           CONTINUE                                                  
C PRINT EIGENVALUES FOR SMALL PROBLEMS                                  
              IF (N.GT.5) GO TO 60                                      
              WRITE(IWRITE,35)                                          
 35           FORMAT(5H EIGA,17X,5H EIGB,17X,20H COMPLEX EIGENVALUES)   
              DO 50 I=1,N                                               
                 IF (EIGB(I).NE.0.0) GO TO 45                           
                 WRITE(IWRITE,43)EIGA(I),EIGB(I)                        
 43              FORMAT(4E11.3)                                         
                 GO TO 50                                               
 45              CONTINUE                                               
                 EI=EIGA(I)/EIGB(I)                                     
                 WRITE(IWRITE,44)EIGA(I),EIGB(I),EI                     
 44              FORMAT(4E10.3,2E15.6)                                  
 50           CONTINUE                                                  
 60         CONTINUE                                                    
C DETERMINE RELATIVE RESIDUAL                                           
            RESERR=0.0                                                  
            DO 90 K=1,N                                                 
               RESUM=0.0                                                
               DO 80 I=1,N                                              
                  SUMA=0.0                                              
                  SUMB=0.0                                              
                  DO 70 J=1,N                                           
                     SUMA=SUMA+ASAVE(I,J)*X(J,K)                        
                     SUMB=SUMB+BSAVE(I,J)*X(J,K)                        
 70               CONTINUE                                              
                  RESUM=RESUM+CABS(SUMA*EIGB(K)-SUMB*EIGA(K))           
 80           CONTINUE                                                  
              RESERR=AMAX1(RESUM,RESERR)                                
 90        CONTINUE                                                     
           RESERR=RESERR/(ABNORM)                                       
           WRITE(IWRITE,95)N,NA,NB,RESERR,ERR                           
 95         FORMAT(36H N,RANKA,RANKB,REL.RESID.,ABS. ERROR,3I3,2E15.5)  
 100       CONTINUE                                                     
           STOP                                                         
           END                                                          
            SUBROUTINE SETUP(NM,A,B,MR,MI,LR,LI,N,NA,NB,ICASE)          
            COMPLEX A(NM,NM),B(NM,NM)                                   
            INTEGER MR(NM,NM),MI(NM,NM),LR(NM),LI(NM)                   
            N=((ICASE+2)/3)*5                                           
            NA=N                                                        
            NB=N                                                        
            IF (ICASE.NE.1.AND.ICASE.NE.4.AND.ICASE.NE.7)NB=N-2         
            IF (ICASE.EQ.3.OR.ICASE.EQ.6.OR.ICASE.EQ.9)NA=N-1           
            DO 20 I=1,N                                                 
               DO 10 J=1,N                                              
                   A(I,J)=(0.0,0.0)                                     
                   B(I,J)=(0.0,0.0)                                     
                   MR(I,J)=N*UNI(0)                                     
                   MI(I,J)=2*N*UNI(0)-N                                 
 10            CONTINUE                                                 
 20         CONTINUE                                                    
            DO 80 I=1,N                                                 
               DO 30 K=1,N                                              
                  LR(K)=2*N*UNI(0)-N                                    
                  LI(K)=2*N*UNI(0)-N                                    
 30            CONTINUE                                                 
               DO 70 J=1,N                                              
                  DO 50 K=1,NA                                          
                   LLR=LR(K)-LI(K)                                      
                   LLI=LR(K)+LI(K)                                      
                   A(I,J)=A(I,J)+CMPLX((LLR*MR(K,J)-LLI*MI(K,J))*K,(LLR*
     1   MI(K,J)+LLI*MR(K,J))*K)                                        
 50              CONTINUE                                               
                DO 60 K=1,NB                                            
                   B(I,J)=B(I,J)+CMPLX(LR(K)*MR(K,J)-LI(K)*MI(K,J),     
     1    LI(K)*MR(K,J)+LR(K)*MI(K,J))                                  
 60               CONTINUE                                              
 70            CONTINUE                                                 
 80         CONTINUE                                                    
            RETURN                                                      
            END                                                         
C$TEST QPRA                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM IQP                                         
C                                                                       
C***********************************************************************
C TEST PROGRAM FOR QP/FEASA  2/12/81                                    
        REAL X(50), Q(50,50), A(200,50), BL(50), BU(50)                 
        REAL C(50), B(200), TEMPQ(50,50), INIT(50)                      
        REAL SAVEQ(50,50), AX(200), SUM(50)                             
        REAL SOL(50), EPSI, SUM1, R1MACH                                
        INTEGER N, I, J, IPRINT, MAXITR, IQ, M, IA, IE, TEST(5)         
        INTEGER IN, ICOUNT, NUM, IROLD                                  
        INTEGER ITMP1, ITMP, ICT, IFLAG, IPROB, IWRITE                  
C                                                                       
        DOUBLE PRECISION DSTAK(6000)                                    
        COMMON /CSTAK/DSTAK                                             
C                                                                       
        IWRITE = I1MACH(2)                                              
        CALL ISTKIN(6000,4)                                             
        CALL ENTSRC(IROLD,1)                                            
        EPSI = SQRT(R1MACH(4))                                          
C                                                                       
        DO 350 IN = 5,20,15                                             
           WRITE (IWRITE,10) IN                                         
 10        FORMAT (/3HN  ,I5)                                           
        DO 340 ICT=1,2                                                  
           WRITE (IWRITE,20) ICT                                        
 20        FORMAT(/19HRANDOM GENERATION -,I2)                           
           IE = ICT - 1                                                 
        DO 330 IPROB = 1,2                                              
           ICOUNT = 0                                                   
           DO 30 I=1,5                                                  
              TEST(I) = 0                                               
 30        CONTINUE                                                     
           N = IN                                                       
           M = N                                                        
C PROBLEM -1                                                            
           IQ = 50                                                      
           IA = 200                                                     
           IPRINT = 1                                                   
           IF (IN.GT.5.OR.ICT.GT.1)IPRINT=0                             
           MAXITR = 150                                                 
           IF (IPROB.NE.1) GO TO 50                                     
           WRITE (IWRITE,40)                                            
 40        FORMAT(48HTEST -1  Q IS POSITIVE DEFINITE  A,X ARE RANDOM.)  
           CALL SETUP1(X, Q, A, BL, BU, C, B, TEMPQ,                    
     *          SAVEQ, IQ, IA, N, M, INIT)                              
           IFLAG = 0                                                    
           GO TO 70                                                     
 50        WRITE (IWRITE,60)                                            
 60        FORMAT(41HTEST -1  Q IS INDEFINITE  A,X ARE RANDOM.)         
           CALL SETUP2(X, Q, A, BL, BU, C, B, TEMPQ,                    
     *          SAVEQ, IQ, IA, N, M)                                    
           IFLAG = 1                                                    
C                                                                       
 70        CONTINUE                                                     
           NUM = 1                                                      
           WRITE (IWRITE,80) M, IE                                      
 80        FORMAT (13H CONSTRAINTS ,I5,10H EQUALITY ,I5)                
           CALL IQP(N, X, Q, IQ, C, M, A, IA, B, BL, BU, IPRINT,        
     1          MAXITR, IE)                                             
C ERROR IN FEAS                                                         
           IF ((NERROR(NERR).EQ.10).OR.(NERROR(NERR).EQ.8)              
     1        .OR.(NERROR(NERR).EQ.9)) CALL ERROFF                      
C ERROR IN IQP                                                          
           IF ((NERROR(NERR).EQ.6).OR.(NERROR(NERR).EQ.7)) CALL ERROFF  
           DO 90 I=1,N                                                  
               SOL(I) = X(I)                                            
 90        CONTINUE                                                     
           CALL FUNCT(N, IQ, X, C, SUM, Q)                              
           CALL MULT(A, X, AX, B, N, M, IA, TEST(NUM), EPSI)            
           CALL CHBND(N, BL, BU, X, TEST(NUM), EPSI)                    
C                                                                       
C          IF (N.GE.10) GO TO 340                                       
           WRITE (IWRITE,100)                                           
 100       FORMAT (15H FINAL SOLUTION)                                  
           DO 120 I=1,N                                                 
              WRITE (IWRITE,110) X(I)                                   
 110          FORMAT (F12.3)                                            
 120       CONTINUE                                                     
C PROBLEM -2 --PUT IN THE SOLUTION FROM PROBLEM -1                      
 130       WRITE (IWRITE,140)                                           
 140       FORMAT(48HTEST -2  SOLVING WITH SOLUTION AS INITIAL GUESS.)  
           NUM = 2                                                      
           DO 150 I=1,N                                                 
               X(I) = SOL(I)                                            
 150       CONTINUE                                                     
           DO 160 I=1,N                                                 
              DO 160 J=1,N                                              
                 Q(I,J) = SAVEQ(I,J)                                    
 160       CONTINUE                                                     
C                                                                       
           CALL IQP(N, X, Q, IQ, C, M, A, IA, B, BL, BU, IPRINT,        
     1          MAXITR, IE)                                             
           IF ((NERROR(NERR).EQ.10).OR.(NERROR(NERR).EQ.8)              
     1        .OR.(NERROR(NERR).EQ.9)) CALL ERROFF                      
           IF ((NERROR(NERR).EQ.6).OR.(NERROR(NERR).EQ.7)) CALL ERROFF  
           CALL CHANGE(N, SOL, X, EPSI, TEST(NUM), IFLAG)               
           CALL FUNCT(N, IQ, X, C, SUM, Q)                              
           CALL MULT(A, X, AX, B, N, M, IA, TEST(NUM), EPSI)            
           CALL CHBND(N, BL, BU, X, TEST(NUM), EPSI)                    
C                                                                       
C PROBLEM -3 --VIOLATE A SIMPLE CONSTRAINT                              
           WRITE (IWRITE,170)                                           
 170       FORMAT(41HTEST -3  VIOLATE FIRST SIMPLE CONSTRAINT.)         
           NUM = 3                                                      
           DO 180 I=1,N                                                 
               X(I) = SOL(I)                                            
 180       CONTINUE                                                     
           X(1) = BL(1) - 10.E0                                         
           DO 190 I=1,N                                                 
              DO 190 J=1,N                                              
                 Q(I,J) = SAVEQ(I,J)                                    
 190       CONTINUE                                                     
           CALL IQP(N, X, Q, IQ, C, M, A, IA, B, BL, BU, IPRINT,        
     1          MAXITR, IE)                                             
           IF ((NERROR(NERR).EQ.10).OR.(NERROR(NERR).EQ.8)              
     1        .OR.(NERROR(NERR).EQ.9)) CALL ERROFF                      
           IF ((NERROR(NERR).EQ.6).OR.(NERROR(NERR).EQ.7)) CALL ERROFF  
           CALL CHANGE(N, SOL, X, EPSI, TEST(NUM), IFLAG)               
           CALL FUNCT(N, IQ, X, C, SUM, Q)                              
           CALL MULT(A, X, AX, B, N, M, IA, TEST(NUM), EPSI)            
           CALL CHBND(N, BL, BU, X, TEST(NUM), EPSI)                    
C                                                                       
C PROBLEM -4 --VIOLATE A GENERAL CONSTRAINT                             
           WRITE (IWRITE,200)                                           
 200       FORMAT(42HTEST -4  VIOLATE FIRST GENERAL CONSTRAINT.)        
           NUM = 4                                                      
           DO 210 I=1,N                                                 
               X(I) = SOL(I)                                            
 210       CONTINUE                                                     
           SUM1 = 0.                                                    
           DO 220 I=2,N                                                 
              SUM1 = SUM1 + A(1,I)*X(I)                                 
 220      CONTINUE                                                      
          X(1) = 10.E0 + (SUM1 + B(1))/A(1,1)                           
          DO 230 I=1,N                                                  
             DO 230 J=1,N                                               
                Q(I,J) = SAVEQ(I,J)                                     
 230      CONTINUE                                                      
          CALL IQP(N, X, Q, IQ, C, M, A, IA, B, BL, BU, IPRINT,         
     1          MAXITR, IE)                                             
           IF ((NERROR(NERR).EQ.10).OR.(NERROR(NERR).EQ.8)              
     1        .OR.(NERROR(NERR).EQ.9)) CALL ERROFF                      
           IF ((NERROR(NERR).EQ.6).OR.(NERROR(NERR).EQ.7)) CALL ERROFF  
          CALL CHANGE(N, SOL, X, EPSI, TEST(NUM), IFLAG)                
          CALL FUNCT(N, IQ, X, C, SUM, Q)                               
          CALL MULT(A, X, AX, B, N, M, IA, TEST(NUM), EPSI)             
          CALL CHBND(N, BL, BU, X, TEST(NUM), EPSI)                     
C                                                                       
C PROBLEM -5 --MAKE SIMPLE CONSTRAINTS INTO GENERAL                     
          WRITE (IWRITE,240)                                            
 240      FORMAT(46HTEST -5  MAKE SIMPLE CONSTRAINTS INTO GENERAL.)     
          NUM = 5                                                       
          DO 250 I=1,N                                                  
              X(I) = INIT(I)                                            
 250      CONTINUE                                                      
          M = M + 2*N                                                   
          DO 260 I=1,N                                                  
             ITMP = N+I                                                 
             ITMP1 = 2*N+I                                              
             A(ITMP,I) = 1.E0                                           
             A(ITMP1,I) = -1.E0                                         
             B(ITMP) = BL(I)                                            
             B(ITMP1) = -BU(I)                                          
             BL(I) = BL(I) - 10.E0                                      
             BU(I) = BU(I) + 10.E0                                      
 260      CONTINUE                                                      
          DO 270 I=1,N                                                  
             DO 270 J=1,N                                               
                Q(I,J) = SAVEQ(I,J)                                     
 270      CONTINUE                                                      
          CALL IQP(N, X, Q, IQ, C, M, A, IA, B, BL, BU, IPRINT,         
     1          MAXITR, IE)                                             
           IF ((NERROR(NERR).EQ.10).OR.(NERROR(NERR).EQ.8)              
     1        .OR.(NERROR(NERR).EQ.9)) CALL ERROFF                      
           IF ((NERROR(NERR).EQ.6).OR.(NERROR(NERR).EQ.7)) CALL ERROFF  
          CALL CHANGE(N, SOL, X, EPSI, TEST(NUM), IFLAG)                
          CALL FUNCT(N, IQ, X, C, SUM, Q)                               
          CALL MULT(A, X, AX, B, N, M, IA, TEST(NUM), EPSI)             
          CALL CHBND(N, BL, BU, X, TEST(NUM), EPSI)                     
C                                                                       
          DO 280 I=1,N                                                  
             BL(I) = BL(I) + 10.                                        
             BU(I) = BU(I) - 10.                                        
 280      CONTINUE                                                      
C                                                                       
          DO 300 I=1,5                                                  
              IF (TEST(I).EQ.0)  GOTO 300                               
              WRITE (IWRITE,290) I                                      
 290          FORMAT (25HTHE PROGRAM FAILED TEST -,I3)                  
              ICOUNT = ICOUNT + 1                                       
 300      CONTINUE                                                      
          IF (ICOUNT.EQ.0) WRITE (IWRITE,310)                           
 310      FORMAT (25HALL TESTS WERE SUCCESSFUL/)                        
          IF (ICOUNT.NE.0) WRITE (IWRITE,320) ICOUNT                    
 320      FORMAT (19HTHE PROGRAM FAILED ,I3,6H TESTS/)                  
 330   CONTINUE                                                         
 340   CONTINUE                                                         
 350   CONTINUE                                                         
       STOP                                                             
       END                                                              
C PROGRAM TO SET UP PROBLEMS FOR TESTCOMP.F 6/7/82                      
       SUBROUTINE SETUP1(X, Q, A, BL, BU, C, B, TEMPQ, SAVEQ,           
     *              IQ, IA, N, M, INIT)                                 
        INTEGER N, I, J, IQ, M, IA                                      
        REAL X(N), Q(IQ,N), A(IA,N), BL(N), BU(N)                       
        REAL C(N), B(M), TEMPQ(IQ,N), INIT(N)                           
        REAL SAVEQ(IQ,N), SUM                                           
C                                                                       
        DO 20 I=1,N                                                     
            X(I) = UNI(0) * 10.                                         
            INIT(I) = X(I)                                              
            C(I) = 0.E0                                                 
            DO 10 J=1,I                                                 
                TEMPQ(J,I) = 0.E0                                       
                TEMPQ(I,J) = UNI(0) * 10. + 1.                          
 10         CONTINUE                                                    
 20     CONTINUE                                                        
        DO 40 I=1,M                                                     
            B(I) = 0.E0                                                 
            DO 30 J=1,N                                                 
                A(I,J) = UNI(0) * 10. + 1.                              
                B(I) = B(I) + A(I,J)                                    
 30         CONTINUE                                                    
        B(I) = B(I)/2.E0                                                
 40     CONTINUE                                                        
C                                                                       
        DO 60 I=1,N                                                     
            ITMP = M+I                                                  
            B(ITMP) = 0.E0                                              
            ITMP1 = M+N+I                                               
            B(ITMP1) = 0.E0                                             
            DO 60 K=1,N                                                 
                A(ITMP,K) = 0.E0                                        
                A(ITMP1,K) = 0.E0                                       
                SUM = 0.E0                                              
                DO 50 J=1,N                                             
                    SUM = SUM + TEMPQ(K,J)*TEMPQ(I,J)                   
 50             CONTINUE                                                
                Q(K,I) = SUM                                            
                SAVEQ(K,I) = SUM                                        
 60     CONTINUE                                                        
C                                                                       
        DO 70 I=1,N                                                     
            BL(I) = 0.E0                                                
            BU(I) = 10.E0                                               
 70     CONTINUE                                                        
        RETURN                                                          
        END                                                             
C                                                                       
C                                                                       
C                                                                       
        SUBROUTINE SETUP2(X, Q, A, BL, BU, C, B, TEMPQ, SAVEQ,          
     *              IQ, IA, N, M)                                       
        INTEGER N, I, J, IQ, M, IA                                      
        REAL X(N), Q(IQ,N), A(IA,N), BL(N), BU(N)                       
        REAL C(N), B(M), TEMPQ(IQ,N)                                    
        REAL SAVEQ(IQ,N)                                                
C                                                                       
        DO 10 I=1,N                                                     
           DO 10 J=1,N                                                  
                Q(I,J) = UNI(0) * 10. + 1.                              
                SAVEQ(I,J) = Q(I,J)                                     
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
         SUBROUTINE CHANGE(N, SOL, X, EPSI, ICOUNT, IFLAG)              
         INTEGER N, ICOUNT, I, IFLAG, IWRITE                            
         REAL SOL(N), X(N), RNRM, RMAX, EPSI                            
C                                                                       
         IWRITE = I1MACH(2)                                             
         RMAX = 0.                                                      
         DO 10 I=1,N                                                    
            RNRM = ABS(SOL(I) - X(I))                                   
            IF (RNRM.GT.RMAX) RMAX = RNRM                               
 10      CONTINUE                                                       
         WRITE (IWRITE,20) RMAX                                         
 20      FORMAT (21H CHANGE IN SOLUTION  ,E15.5)                        
         IF (IFLAG.EQ.1) RETURN                                         
         IF (RMAX.GT.EPSI) ICOUNT = 1                                   
         RETURN                                                         
         END                                                            
        SUBROUTINE FUNCT(N, IQ, X, C, SUM, Q)                           
        INTEGER N, IQ, ITMP, IWRITE                                     
        REAL X(N), C(N), Q(IQ,N), SUM(N)                                
        REAL F, CTX                                                     
        IWRITE = I1MACH(2)                                              
        CTX = 0.                                                        
        DO 10 I=1,N                                                     
            CTX = X(I) * C(I) + CTX                                     
 10     CONTINUE                                                        
        DO 20 J=1,N                                                     
            SUM(J) = 0.                                                 
            SUM(1) = SUM(1) + X(J)*Q(1,J)                               
 20     CONTINUE                                                        
        DO 50 I=2,N                                                     
            DO 30 J=I,N                                                 
                SUM(I) = SUM(I) + X(J)*Q(I,J)                           
 30         CONTINUE                                                    
            ITMP = I-1                                                  
            DO 40 J=1,ITMP                                              
                SUM(I) = SUM(I) + X(J)*Q(J,I)                           
 40         CONTINUE                                                    
 50     CONTINUE                                                        
        F = 0.                                                          
        DO 60 I=1,N                                                     
            F = SUM(I) * X(I) + F                                       
 60     CONTINUE                                                        
        F = F/2. + CTX                                                  
        WRITE (IWRITE,70) F                                             
 70     FORMAT (22H FINAL FUNCTION VALUE , E14.5)                       
        RETURN                                                          
        END                                                             
C                                                                       
        SUBROUTINE MULT(A, X, AX, B, N, M, IA, TEST, EPSI)              
        INTEGER IA, IWRITE                                              
        INTEGER N, M, I, J, TEST, MTEST                                 
        REAL A(IA,N), X(N), AX(N), B(N)                                 
        REAL  RMAX, RES, EPSI                                           
C                                                                       
        IWRITE = I1MACH(2)                                              
        MTEST = 0                                                       
        RMAX = 0.                                                       
        DO 30 I=1,M                                                     
           AX(I) = 0.                                                   
           DO 10 J=1,N                                                  
                AX(I) = AX(I) + A(I,J) * X(J)                           
 10        CONTINUE                                                     
           RES = B(I) - AX(I)                                           
           IF (RES.LE.EPSI) GO TO 30                                    
           WRITE (IWRITE,20) I, RES                                     
 20        FORMAT (11H CONSTRAINT,I5,24H IS VIOLATED.  RESIDUAL ,E15.5) 
           IF (RMAX.LT.RES) RMAX = RES                                  
           TEST = 1                                                     
           MTEST = 1                                                    
 30     CONTINUE                                                        
        IF (MTEST.EQ.0) WRITE (IWRITE,40)                               
 40     FORMAT (30H NO CONSTRAINTS WERE VIOLATED.)                      
        IF (MTEST.GT.0) WRITE (IWRITE,50) RMAX                          
 50     FORMAT (38H RESIDUAL OF THE VIOLATED CONSTRAINTS ,E15.5)        
        RETURN                                                          
        END                                                             
       SUBROUTINE CHBND(N, BL, BU, X, TEST, EPSI)                       
       INTEGER N, ILOW, IUP, I, TEST, IWRITE                            
       REAL BL(N), BU(N), X(N), EPSI                                    
C                                                                       
       IWRITE = I1MACH(2)                                               
       ILOW = 0                                                         
       IUP = 0                                                          
       DO 60 I=1,N                                                      
           IF ((BL(I)-X(I)).GT.EPSI) GO TO 20                           
 10        IF ((X(I)-BU(I)).GT.EPSI) GO TO 40                           
           GO TO 60                                                     
C                                                                       
 20        WRITE (IWRITE,30) I                                          
 30        FORMAT (13H LOWER BOUND ,I5,9H VIOLATED)                     
           ILOW = ILOW + 1                                              
           GO TO 60                                                     
 40        WRITE (IWRITE,50) I                                          
 50        FORMAT (13H UPPER BOUND ,I5,9H VIOLATED)                     
           IUP = IUP + 1                                                
 60     CONTINUE                                                        
        IF ((ILOW.NE.0).OR.(IUP.NE.0)) GO TO 80                         
        WRITE (IWRITE,70)                                               
 70     FORMAT (40H NO UPPER OR LOWER BOUNDS WERE VIOLATED.)            
        RETURN                                                          
 80     WRITE (IWRITE,90) ILOW                                          
 90     FORMAT (1H ,I5,27H LOWER BOUNDS WERE VIOLATED)                  
        WRITE (IWRITE,100) IUP                                          
 100    FORMAT (1H ,I5,27H UPPER BOUNDS WERE VIOLATED)                  
        TEST = 1                                                        
        RETURN                                                          
       END                                                              
C$TEST QPAD                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT PROGRAM DIQP                                        
C                                                                       
C***********************************************************************
C TEST PROGRAM FOR QP/FEASA  2/12/81                                    
        DOUBLE PRECISION X(50), Q(50,50), A(200,50), BL(50), BU(50)     
        DOUBLE PRECISION C(50), B(200), TEMPQ(50,50), INIT(50)          
        DOUBLE PRECISION SAVEQ(50,50), AX(200), SUM(50)                 
        DOUBLE PRECISION SOL(50), EPSI, SUM1, D1MACH                    
        INTEGER N, I, J, IPRINT, MAXITR, IQ, M, IA, IE, TEST(5)         
        INTEGER IN, ICOUNT, NUM, IROLD                                  
        INTEGER ITMP1, ITMP, ICT, IFLAG, IPROB, IWRITE                  
C                                                                       
        DOUBLE PRECISION DSTAK                                          
        COMMON /CSTAK/DSTAK(6000)                                       
C                                                                       
        IWRITE = I1MACH(2)                                              
        CALL ISTKIN(6000,4)                                             
        CALL ENTSRC(IROLD,1)                                            
        EPSI = DSQRT(D1MACH(4))                                         
C                                                                       
        DO 100 IN = 5,20,15                                             
           WRITE (IWRITE,110) IN                                        
 110       FORMAT (/3HN  ,I5)                                           
        DO 200 ICT=1,2                                                  
           WRITE (IWRITE,210) ICT                                       
 210       FORMAT(/19HRANDOM GENERATION -,I2)                           
           IE = ICT - 1                                                 
        DO 300 IPROB = 1,2                                              
           ICOUNT = 0                                                   
           DO 301 I=1,5                                                 
              TEST(I) = 0                                               
 301       CONTINUE                                                     
           N = IN                                                       
           M = N                                                        
C PROBLEM -1                                                            
           IQ = 50                                                      
           IA = 200                                                     
           IPRINT = 0                                                   
           IF (N.LE.5.AND.ICT.EQ.1)IPRINT=1                             
           MAXITR = 150                                                 
           IF (IPROB.NE.1) GO TO 305                                    
           WRITE (IWRITE,310)                                           
 310       FORMAT(48HTEST -1  Q IS POSITIVE DEFINITE  A,X ARE RANDOM.)  
           CALL SETUP1(X, Q, A, BL, BU, C, B, TEMPQ,                    
     *          SAVEQ, IQ, IA, N, M, INIT)                              
           IFLAG = 0                                                    
           GO TO 315                                                    
 305       WRITE (IWRITE,320)                                           
 320       FORMAT(41HTEST -1  Q IS INDEFINITE  A,X ARE RANDOM.)         
           CALL SETUP2(X, Q, A, BL, BU, C, B, TEMPQ,                    
     *          SAVEQ, IQ, IA, N, M)                                    
           IFLAG = 1                                                    
C                                                                       
 315       CONTINUE                                                     
           NUM = 1                                                      
           WRITE (IWRITE,321) M, IE                                     
 321       FORMAT (13H CONSTRAINTS ,I5,10H EQUALITY ,I5)                
           CALL DIQP(N, X, Q, IQ, C, M, A, IA, B, BL, BU, IPRINT,       
     1          MAXITR, IE)                                             
C ERROR IN DFEAS                                                        
           IF ((NERROR(NERR).EQ.10).OR.(NERROR(NERR).EQ.8)              
     1        .OR.(NERROR(NERR).EQ.9)) CALL ERROFF                      
C ERROR IN DIQP                                                         
           IF ((NERROR(NERR).EQ.6).OR.(NERROR(NERR).EQ.7)) CALL ERROFF  
           DO 330 I=1,N                                                 
               SOL(I) = X(I)                                            
 330       CONTINUE                                                     
           CALL FUNCT(N, IQ, X, C, SUM, Q)                              
           CALL MULT(A, X, AX, B, N, M, IA, TEST(NUM))                  
           CALL CHBND(N, BL, BU, X, TEST(NUM))                          
C                                                                       
C          IF (N.GE.10) GO TO 340                                       
           WRITE (IWRITE,350)                                           
 350       FORMAT (15H FINAL SOLUTION)                                  
           DO 360 I=1,N                                                 
              WRITE (IWRITE,370) X(I)                                   
 370          FORMAT (F12.3)                                            
 360       CONTINUE                                                     
C PROBLEM -2 --PUT IN THE SOLUTION FROM PROBLEM -1                      
 340       WRITE (IWRITE,380)                                           
 380       FORMAT(48HTEST -2  SOLVING WITH SOLUTION AS INITIAL GUESS.)  
           NUM = 2                                                      
           DO 385 I=1,N                                                 
               X(I) = SOL(I)                                            
 385       CONTINUE                                                     
           DO 390 I=1,N                                                 
              DO 390 J=1,N                                              
                 Q(I,J) = SAVEQ(I,J)                                    
 390       CONTINUE                                                     
C                                                                       
           CALL DIQP(N, X, Q, IQ, C, M, A, IA, B, BL, BU, IPRINT,       
     1          MAXITR, IE)                                             
           IF ((NERROR(NERR).EQ.10).OR.(NERROR(NERR).EQ.8)              
     1        .OR.(NERROR(NERR).EQ.9)) CALL ERROFF                      
           IF ((NERROR(NERR).EQ.6).OR.(NERROR(NERR).EQ.7)) CALL ERROFF  
           CALL CHANGE(N, SOL, X, EPSI, TEST(NUM), IFLAG)               
           CALL FUNCT(N, IQ, X, C, SUM, Q)                              
           CALL MULT(A, X, AX, B, N, M, IA, TEST(NUM))                  
           CALL CHBND(N, BL, BU, X, TEST(NUM))                          
C                                                                       
C PROBLEM -3 --VIOLATE A SIMPLE CONSTRAINT                              
           WRITE (IWRITE,400)                                           
 400       FORMAT(41HTEST -3  VIOLATE FIRST SIMPLE CONSTRAINT.)         
           NUM = 3                                                      
           DO 405 I=1,N                                                 
               X(I) = SOL(I)                                            
 405       CONTINUE                                                     
           X(1) = BL(1) - 10.D0                                         
           DO 410 I=1,N                                                 
              DO 410 J=1,N                                              
                 Q(I,J) = SAVEQ(I,J)                                    
 410       CONTINUE                                                     
           CALL DIQP(N, X, Q, IQ, C, M, A, IA, B, BL, BU, IPRINT,       
     1          MAXITR, IE)                                             
           IF ((NERROR(NERR).EQ.10).OR.(NERROR(NERR).EQ.8)              
     1        .OR.(NERROR(NERR).EQ.9)) CALL ERROFF                      
           IF ((NERROR(NERR).EQ.6).OR.(NERROR(NERR).EQ.7)) CALL ERROFF  
           CALL CHANGE(N, SOL, X, EPSI, TEST(NUM), IFLAG)               
           CALL FUNCT(N, IQ, X, C, SUM, Q)                              
           CALL MULT(A, X, AX, B, N, M, IA, TEST(NUM))                  
           CALL CHBND(N, BL, BU, X, TEST(NUM))                          
C                                                                       
C PROBLEM -4 --VIOLATE A GENERAL CONSTRAINT                             
           WRITE (IWRITE,420)                                           
 420       FORMAT(42HTEST -4  VIOLATE FIRST GENERAL CONSTRAINT.)        
           NUM = 4                                                      
           DO 425 I=1,N                                                 
               X(I) = SOL(I)                                            
 425       CONTINUE                                                     
           SUM1 = 0.                                                    
           DO 430 I=2,N                                                 
              SUM1 = SUM1 + A(1,I)*X(I)                                 
 430      CONTINUE                                                      
          X(1) = 10.D0 + (SUM1 + B(1))/A(1,1)                           
          DO 440 I=1,N                                                  
             DO 440 J=1,N                                               
                Q(I,J) = SAVEQ(I,J)                                     
 440      CONTINUE                                                      
          CALL DIQP(N, X, Q, IQ, C, M, A, IA, B, BL, BU, IPRINT,        
     1          MAXITR, IE)                                             
           IF ((NERROR(NERR).EQ.10).OR.(NERROR(NERR).EQ.8)              
     1        .OR.(NERROR(NERR).EQ.9)) CALL ERROFF                      
           IF ((NERROR(NERR).EQ.6).OR.(NERROR(NERR).EQ.7)) CALL ERROFF  
          CALL CHANGE(N, SOL, X, EPSI, TEST(NUM), IFLAG)                
          CALL FUNCT(N, IQ, X, C, SUM, Q)                               
          CALL MULT(A, X, AX, B, N, M, IA, TEST(NUM))                   
          CALL CHBND(N, BL, BU, X, TEST(NUM))                           
C                                                                       
C PROBLEM -5 --MAKE SIMPLE CONSTRAINTS INTO GENERAL                     
          WRITE (IWRITE,450)                                            
 450      FORMAT(46HTEST -5  MAKE SIMPLE CONSTRAINTS INTO GENERAL.)     
          NUM = 5                                                       
          DO 455 I=1,N                                                  
              X(I) = INIT(I)                                            
 455      CONTINUE                                                      
          M = M + 2*N                                                   
          DO 460 I=1,N                                                  
             ITMP = N+I                                                 
             ITMP1 = 2*N+I                                              
             A(ITMP,I) = 1.D0                                           
             A(ITMP1,I) = -1.D0                                         
             B(ITMP) = BL(I)                                            
             B(ITMP1) = -BU(I)                                          
             BL(I) = BL(I) - 10.D0                                      
             BU(I) = BU(I) + 10.D0                                      
 460      CONTINUE                                                      
          DO 470 I=1,N                                                  
             DO 470 J=1,N                                               
                Q(I,J) = SAVEQ(I,J)                                     
 470      CONTINUE                                                      
          CALL DIQP(N, X, Q, IQ, C, M, A, IA, B, BL, BU, IPRINT,        
     1          MAXITR, IE)                                             
           IF ((NERROR(NERR).EQ.10).OR.(NERROR(NERR).EQ.8)              
     1        .OR.(NERROR(NERR).EQ.9)) CALL ERROFF                      
           IF ((NERROR(NERR).EQ.6).OR.(NERROR(NERR).EQ.7)) CALL ERROFF  
          CALL CHANGE(N, SOL, X, EPSI, TEST(NUM), IFLAG)                
          CALL FUNCT(N, IQ, X, C, SUM, Q)                               
          CALL MULT(A, X, AX, B, N, M, IA, TEST(NUM))                   
          CALL CHBND(N, BL, BU, X, TEST(NUM))                           
C                                                                       
          DO 480 I=1,N                                                  
             BL(I) = BL(I) + 10.                                        
             BU(I) = BU(I) - 10.                                        
 480      CONTINUE                                                      
C                                                                       
          DO 481 I=1,5                                                  
              IF (TEST(I).EQ.0)  GOTO 481                               
              WRITE (IWRITE,482) I                                      
 482          FORMAT (25HTHE PROGRAM FAILED TEST -,I3)                  
              ICOUNT = ICOUNT + 1                                       
 481      CONTINUE                                                      
          IF (ICOUNT.EQ.0) WRITE (IWRITE,490)                           
 490      FORMAT (25HALL TESTS WERE SUCCESSFUL/)                        
          IF (ICOUNT.NE.0) WRITE (IWRITE,500) ICOUNT                    
 500      FORMAT (19HTHE PROGRAM FAILED ,I3,6H TESTS/)                  
 300   CONTINUE                                                         
 200    CONTINUE                                                        
 100   CONTINUE                                                         
       STOP                                                             
       END                                                              
         SUBROUTINE CHANGE(N, SOL, X, EPSI, ICOUNT, IFLAG)              
         INTEGER N, ICOUNT, I, IFLAG, IWRITE                            
         DOUBLE PRECISION SOL(N), X(N), RNRM, RMAX, EPSI                
C                                                                       
         IWRITE = I1MACH(2)                                             
         RMAX = 0.                                                      
         DO 1 I=1,N                                                     
            RNRM = DABS(SOL(I) - X(I))                                  
            IF (RNRM.GT.RMAX) RMAX = RNRM                               
 1       CONTINUE                                                       
         WRITE (IWRITE,2) RMAX                                          
 2       FORMAT (21H CHANGE IN SOLUTION  ,E15.5)                        
         IF (IFLAG.EQ.1) RETURN                                         
         IF (RMAX.GT.EPSI) ICOUNT = 1                                   
         RETURN                                                         
         END                                                            
        SUBROUTINE FUNCT(N, IQ, X, C, SUM, Q)                           
        INTEGER N, IQ, ITMP, IWRITE                                     
        DOUBLE PRECISION X(N), C(N), Q(IQ,N), SUM(N)                    
        DOUBLE PRECISION F, CTX                                         
        IWRITE = I1MACH(2)                                              
        CTX = 0.                                                        
        DO 5 I=1,N                                                      
            CTX = X(I) * C(I) + CTX                                     
 5      CONTINUE                                                        
        DO 6 J=1,N                                                      
            SUM(J) = 0.                                                 
            SUM(1) = SUM(1) + X(J)*Q(1,J)                               
 6      CONTINUE                                                        
        DO 7 I=2,N                                                      
            DO 8 J=I,N                                                  
                SUM(I) = SUM(I) + X(J)*Q(I,J)                           
 8          CONTINUE                                                    
            ITMP = I-1                                                  
            DO 9 J=1,ITMP                                               
                SUM(I) = SUM(I) + X(J)*Q(J,I)                           
 9          CONTINUE                                                    
 7      CONTINUE                                                        
        F = 0.                                                          
        DO 10 I=1,N                                                     
            F = SUM(I) * X(I) + F                                       
 10     CONTINUE                                                        
        F = F/2. + CTX                                                  
        WRITE (IWRITE,1002) F                                           
 1002   FORMAT (22H FINAL FUNCTION VALUE , D14.5)                       
        RETURN                                                          
        END                                                             
C                                                                       
        SUBROUTINE MULT(A, X, AX, B, N, M, IA, TEST)                    
        INTEGER IA, IWRITE                                              
        INTEGER N, M, I, J, TEST, MTEST                                 
        DOUBLE PRECISION A(IA,N), X(N), AX(N), B(N)                     
        DOUBLE PRECISION  RMAX, RES, EPSI, D1MACH                       
C                                                                       
        IWRITE = I1MACH(2)                                              
        MTEST = 0                                                       
        RMAX = 0.                                                       
        EPSI = DSQRT(D1MACH(4))                                         
        DO 1 I=1,M                                                      
           AX(I) = 0.                                                   
           DO 3 J=1,N                                                   
                AX(I) = AX(I) + A(I,J) * X(J)                           
 3         CONTINUE                                                     
           RES = B(I) - AX(I)                                           
           IF (RES.LE.EPSI) GO TO 1                                     
           WRITE (IWRITE,10) I, RES                                     
 10        FORMAT (11H CONSTRAINT,I5,24H IS VIOLATED.  RESIDUAL ,E15.5) 
           IF (RMAX.LT.RES) RMAX = RES                                  
           TEST = 1                                                     
           MTEST = 1                                                    
 1      CONTINUE                                                        
        IF (MTEST.EQ.0) WRITE (IWRITE,20)                               
 20     FORMAT (30H NO CONSTRAINTS WERE VIOLATED.)                      
        IF (MTEST.GT.0) WRITE (IWRITE,30) RMAX                          
 30      FORMAT (38H RESIDUAL OF THE VIOLATED CONSTRAINTS ,E15.5)       
        RETURN                                                          
        END                                                             
       SUBROUTINE CHBND(N, BL, BU, X, TEST)                             
       INTEGER N, ILOW, IUP, I, TEST, IWRITE                            
       DOUBLE PRECISION BL(N), BU(N), X(N)                              
C                                                                       
       IWRITE = I1MACH(2)                                               
       ILOW = 0                                                         
       IUP = 0                                                          
       DO 1 I=1,N                                                       
           IF (BL(I).GT.X(I)) GO TO 100                                 
 120       IF (BU(I).LT.X(I)) GO TO 110                                 
           GO TO 1                                                      
C                                                                       
 100       WRITE (IWRITE,150) I                                         
 150       FORMAT (13H LOWER BOUND ,I5,9H VIOLATED)                     
           ILOW = ILOW + 1                                              
           GO TO 120                                                    
 110       WRITE (IWRITE,160) I                                         
 160       FORMAT (13H UPPER BOUND ,I5,9H VIOLATED)                     
           IUP = IUP + 1                                                
 1      CONTINUE                                                        
        IF ((ILOW.NE.0).OR.(IUP.NE.0)) GO TO 210                        
        WRITE (IWRITE,200)                                              
 200    FORMAT (40H NO UPPER OR LOWER BOUNDS WERE VIOLATED.)            
        RETURN                                                          
 210    WRITE (IWRITE,220) ILOW                                         
 220    FORMAT (1H ,I5,27H LOWER BOUNDS WERE VIOLATED)                  
        WRITE (IWRITE,230) IUP                                          
 230    FORMAT (1H ,I5,27H UPPER BOUNDS WERE VIOLATED)                  
        TEST = 1                                                        
        RETURN                                                          
       END                                                              
C PROGRAM TO SET UP PROBLEMS FOR TESTCOMP.F 6/7/82                      
       SUBROUTINE SETUP1(X, Q, A, BL, BU, C, B, TEMPQ, SAVEQ,           
     *              IQ, IA, N, M, INIT)                                 
        INTEGER N, I, J, IQ, M, IA                                      
        DOUBLE PRECISION X(N), Q(IQ,N), A(IA,N), BL(N), BU(N)           
        DOUBLE PRECISION C(N), B(M), TEMPQ(IQ,N), INIT(N)               
        DOUBLE PRECISION SAVEQ(IQ,N), SUM, DBLE                         
C                                                                       
        DO 1 I=1,N                                                      
            X(I) = DBLE(UNI(0) * 10.)                                   
            INIT(I) = X(I)                                              
            C(I) = 0.D0                                                 
            DO 7 J=1,I                                                  
                TEMPQ(J,I) = 0.D0                                       
                TEMPQ(I,J) = DBLE(UNI(0) * 10. + 1.)                    
 7          CONTINUE                                                    
 1      CONTINUE                                                        
        DO 9 I=1,M                                                      
            B(I) = 0.D0                                                 
            DO 11 J=1,N                                                 
                A(I,J) = DBLE(UNI(0) * 10. + 1.)                        
                B(I) = B(I) + A(I,J)                                    
 11         CONTINUE                                                    
        B(I) = B(I)/2.D0                                                
 9      CONTINUE                                                        
C                                                                       
        DO 12 I=1,N                                                     
            ITMP = M+I                                                  
            B(ITMP) = 0.D0                                              
            ITMP1 = M+N+I                                               
            B(ITMP1) = 0.D0                                             
            DO 12 K=1,N                                                 
                A(ITMP,K) = 0.D0                                        
                A(ITMP1,K) = 0.D0                                       
                SUM = 0.D0                                              
                DO 13 J=1,N                                             
                    SUM = SUM + TEMPQ(K,J)*TEMPQ(I,J)                   
 13             CONTINUE                                                
                Q(K,I) = SUM                                            
                SAVEQ(K,I) = SUM                                        
 12     CONTINUE                                                        
C                                                                       
        DO 3 I=1,N                                                      
            BL(I) = 0.D0                                                
            BU(I) = 10.D0                                               
 3      CONTINUE                                                        
        RETURN                                                          
        END                                                             
C                                                                       
C                                                                       
C                                                                       
        SUBROUTINE SETUP2(X, Q, A, BL, BU, C, B, TEMPQ, SAVEQ,          
     *              IQ, IA, N, M)                                       
        INTEGER N, I, J, IQ, M, IA                                      
        DOUBLE PRECISION X(N), Q(IQ,N), A(IA,N), BL(N), BU(N)           
        DOUBLE PRECISION C(N), B(M), TEMPQ(IQ,N)                        
        DOUBLE PRECISION SAVEQ(IQ,N), DBLE                              
C                                                                       
        DO 1973 I=1,N                                                   
           DO 1973 J=1,N                                                
                Q(I,J) = DBLE(UNI(0) * 10. + 1.)                        
                SAVEQ(I,J) = Q(I,J)                                     
 1973   CONTINUE                                                        
        RETURN                                                          
        END                                                             
C$TEST LRPA                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT LINEAR PROGRAMMING - LINPR, ETC.                    
C                                                                       
C***********************************************************************
C  MAIN PROGRAM                                                         
      REAL DSTAK(8000)                                                  
      REAL ANULL, FEASIN, SN                                            
      COMMON /CSTAK/ DSTAK                                              
      EXTERNAL LPMAN,  LPT,LPRNT                                        
      INTEGER IAG, IAS,  NERROR, I, J                                   
      INTEGER K, L, M, N, KASE                                          
      INTEGER IERR, IPTG(150), IA, IE, IL, IS                           
      INTEGER IROLD, ISIMP(200), ITMAX, ITYPE, IWRITE                   
      REAL CHANGE,  CTX, SUM, CHAGE2, CHAGE3                            
      REAL CHAGE4, CHAGE5, A(150, 50), B(150), C(150), X(100,5)         
      REAL Y(100), SDOT, CTXD, SIMP(200),  SASUM                        
      REAL Q(8),EPS                                                     
      REAL R1MACH, UNI                                                  
C                                                                       
C SET UP OUTPUT WRITE UNIT                                              
C                                                                       
      IWRITE = I1MACH(2)                                                
      CALL ISTKIN(8000, 3)                                              
      CALL ENTSRC(IROLD, 1)                                             
      KASE = 0                                                          
          EPS=SQRT(R1MACH(4))                                           
         NOBAD=0                                                        
      IA = 150                                                          
 10   CONTINUE                                                          
      KASE = KASE+1                                                     
      CALL SETUP(A, IA, M, N, IE, IS, KASE, ISIMP, SIMP, ITYPE, B, C)   
          DO 20 I=1,8                                                   
                 Q(I)=0.0                                               
 20         CONTINUE                                                    
       IF (N.GT.0) GO TO 40                                             
            WRITE(IWRITE,30)NOBAD                                       
 30          FORMAT(26H NUMBER OF BAD EXAMPLES IS, I5)                  
                STOP                                                    
 40               CONTINUE                                              
      WRITE(IWRITE,60)KASE                                              
      WRITE (IWRITE,  50) M, N, IE, IS, ITYPE                           
 50   FORMAT (3H M=, I10, 2HN=, I10, 13H NO. EQUALITY, I10,             
     1   11H NO. SIMPLE, I10, 6H ITYPE, I10)                            
 60   FORMAT(//6H KASE=,I5)                                             
      DO  70 I = 1, N                                                   
         X(I, 1) = UNI(0)                                               
         X(I, 2) = X(I, 1)                                              
 70      CONTINUE                                                       
      ITMAX = 5*N                                                       
         CALL LINPA(A,M,N,LPMAN,IA,B,C,X,ITMAX,CTX,IS,SIMP,             
     1      ISIMP, IE, LPRNT, IAG, IAS, IPTG, X(I, 5))                  
      IF (NERROR(IERR) .EQ. 0) GOTO 90                                  
         WRITE (IWRITE,  80) IERR                                       
 80      FORMAT (4H ERR, I10)                                           
         CALL ERROFF                                                    
         IF (IERR.EQ.8) GO TO 170                                       
         GOTO  10                                                       
 90       CONTINUE                                                      
           FEASIN=0.0E0                                                 
           IAGIE=IAG+IE                                                 
           ANULL=0.0E0                                                  
           DO 100 I=1,M                                                 
              JJ=IPTG(I)                                                
              SUM=SDOT(N,A(JJ,1),IA,X,1)-B(JJ)                          
              FEASIN=AMAX1(FEASIN,-SUM)                                 
              IF (I.LE.IAGIE)ANULL=AMAX1(ANULL,ABS(SUM))                
 100       CONTINUE                                                     
      SN= SASUM(N,X,1)                                                  
      SN=AMAX1(SN,1.E0)                                                 
           FEASIN=FEASIN/SN                                             
            ANULL=ANULL/SN                                              
           Q(1)=FEASIN                                                  
           Q(2)=ANULL                                                   
           WRITE(IWRITE,110)FEASIN,ANULL                                
 110       FORMAT(22H MAX. INFEASABILIBITY=,E15.5,18H NULLITY VIOLATION,
     1     E15.5)                                                       
C SOLVE THE PROBLEM AGAIN                                               
C                                                                       
      DO  120 I = 1, N                                                  
         X(I, 3) = X(I, 1)                                              
 120     CONTINUE                                                       
      CALL LINPR(A, M, N, IA, B, C, X(1, 3), ITMAX, CTX, IS,            
     1   SIMP, ISIMP, IE)                                               
      IF (NERROR(IERR) .EQ. 0) GOTO 140                                 
         WRITE (IWRITE,  130) IERR                                      
 130     FORMAT (4H ERR, I10)                                           
         CALL ERROFF                                                    
         GOTO  10                                                       
 140  DO  150 I = 1, N                                                  
         Y(I) = X(I, 3)-X(I, 1)                                         
 150     CONTINUE                                                       
      CHANGE = SASUM(N, Y, 1)/SN                                        
            Q(3)=CHANGE                                                 
      WRITE (IWRITE,  160) CHANGE                                       
 160  FORMAT(51H SOLVING SECOND TIME WITH INITIAL GUESS AS SOLUTION/    
     1  9H CHANGE= ,1PD23.15)                                           
C                                                                       
C DIFFERENT INITIAL POINT                                               
C                                                                       
 170      CONTINUE                                                      
      DO  180 I = 1, N                                                  
         X(I, 3) = -X(I, 2)                                             
 180     CONTINUE                                                       
      CALL LINPR(A, M, N, IA, B, C, X(1, 3), ITMAX, CTX, IS,            
     1   SIMP, ISIMP, IE)                                               
      IF (NERROR(IERR) .EQ. 0) GOTO 200                                 
         WRITE (IWRITE,  190) IERR                                      
 190     FORMAT (4H ERR, I10)                                           
         CALL ERROFF                                                    
         GOTO  10                                                       
 200  DO  210 I = 1, N                                                  
         Y(I) = X(I, 3)-X(I, 1)                                         
 210     CONTINUE                                                       
      CHAGE2=SASUM(N,Y,1)/SN                                            
            Q(4)=CHAGE2                                                 
      WRITE (IWRITE,  220) CHAGE2                                       
 220  FORMAT(44H INITIAL POINT IS NEGATED AND PROBLEM SOLVED/           
     *  9H CHANGE= ,1PD23.15)                                           
C                                                                       
C MAKE SURE HAVE INFEASIBLE STARTING POINT                              
C                                                                       
      IF (M .LE. 0) GOTO 330                                            
         SUM = B(1)-SDOT(N, A(1, 1), IA, X(1, 2), 1)                    
         IF (SUM .LE. 0E0) GOTO 240                                     
            WRITE (IWRITE,  230)                                        
 230        FORMAT (29H INITIAL GUESS WAS INFEASIBLE)                   
            GOTO  320                                                   
 240        I = 1                                                       
 250        IF (A(I, 1) .NE. 0.0E0) GOTO  260                           
               I = I+1                                                  
               GOTO  250                                                
 260        DO  270 J = 1, N                                            
               X(J, 3) = X(J, 2)                                        
 270           CONTINUE                                                 
            X(I, 3) = X(I, 3)+(SUM+1.E0)/A(I, 1)                        
            CALL LINPR(A, M, N, IA, B, C, X(1, 3), ITMAX, CTX,          
     1   IS, SIMP, ISIMP, IE)                                           
            IF (NERROR(IERR) .EQ. 0) GOTO 290                           
               WRITE (IWRITE,  280) IERR                                
 280           FORMAT (4H ERR, I10)                                     
               CALL ERROFF                                              
               GOTO  10                                                 
 290        DO  300 I = 1, N                                            
               Y(I) = X(I, 3)-X(I, 1)                                   
 300           CONTINUE                                                 
            CHAGE3 = SASUM(N, Y, 1)/SN                                  
             Q(5)=CHAGE3                                                
            WRITE (IWRITE,  310) CHAGE3                                 
 310     FORMAT(43H INITIAL POINT MADE INFEASIBLE AND RESOLVED/         
     *10H CHANGE = ,1PD23.15)                                           
 320     CONTINUE                                                       
C                                                                       
C SOLVE WITH SIMPLE CONVERTED INTO GENERAL                              
C                                                                       
 330  IF (IS .EQ. 0) GOTO 430                                           
         L = M                                                          
         DO  370 I = 1, IS                                              
            L = L+1                                                     
            DO  340 J = 1, N                                            
               A(L, J) = 0.0                                            
 340           CONTINUE                                                 
            K = IABS(ISIMP(I))                                          
            A(L, K) = -1.E0                                             
            B(L) = SIMP(I)                                              
            IF (ISIMP(I) .LE. 0) GOTO 350                               
               A(L, K) = 1.E0                                           
               GOTO  360                                                
 350           B(L) = -SIMP(I)                                          
 360        CONTINUE                                                    
 370        CONTINUE                                                    
         DO  380 I = 1, N                                               
            X(I, 3) = X(I, 2)                                           
 380        CONTINUE                                                    
         IL = 0                                                         
         MM=M+IS                                                        
         CALL LINPR(A,MM, N, IA, B, C, X(1, 3), ITMAX, CTX, IL          
     1, SIMP, ISIMP, IE)                                                
         IF (NERROR(IERR) .EQ. 0) GOTO 400                              
            WRITE (IWRITE,  390) IERR                                   
 390        FORMAT (4H ERR, I10)                                        
            CALL ERROFF                                                 
            GOTO  10                                                    
 400     DO  410 I = 1, N                                               
            Y(I) = X(I, 3)-X(I, 1)                                      
 410        CONTINUE                                                    
         CHAGE4 = SASUM(N, Y, 1)/SN                                     
             Q(6)=CHAGE4                                                
         WRITE (IWRITE,  420) CHAGE4                                    
 420    FORMAT(38H SIMPLE CONSTRAINTS WRITTEN AS GENERAL/               
     1 9H CHANGE= ,1PD23.15)                                            
C                                                                       
C TEST DUALITY IF APPLICABLE                                            
C                                                                       
 430  IF (ITYPE .NE. 1) GOTO 520                                        
       WRITE(IWRITE,440)                                                
 440   FORMAT(17H TEST FOR DUALITY)                                     
         DO  460 I = 1, M                                               
             DO 450 J=1,N                                               
                A(I,J)=-A(I,J)                                          
 450          CONTINUE                                                  
            X(I, 4) = UNI(0)                                            
 460        CONTINUE                                                    
         CALL LINPA(A,N,M,LPT,IA,C,B,X(1,4),ITMAX,CTXD,0,SIMP,          
     1      ISIMP, 0, LPRNT, IAG, IAS, IPTG, X(1, 3))                   
         IF (NERROR(IERR) .EQ. 0) GOTO 480                              
            WRITE (IWRITE,  470) IERR                                   
 470        FORMAT (4H ERR, I10)                                        
            CALL ERROFF                                                 
            GOTO  10                                                    
 480     DO  490 I = 1, M                                               
            II=IPTG(I)                                                  
            Y(I) = X(I, 3)+X(II, 1)                                     
 490        CONTINUE                                                    
         CHAGE5 = SASUM(M, Y, 1)/SN                                     
         CTXD=-CTXD                                                     
              Q(7)=ABS(CTX-CTXD)/ABS(CTX)                               
             Q(8)=CHAGE5                                                
         WRITE (IWRITE,  500) CTX, CTXD                                 
 500     FORMAT (4H CTX, 1PD23.15, 4HCTXX, 1PD23.15)                    
         WRITE (IWRITE,  510) CHAGE5                                    
 510     FORMAT (17H CHANGE FROM DUAL, 1PD23.15)                        
C                                                                       
C                                                                       
 520         CONTINUE                                                   
             IBAD=0                                                     
             DO 530 I=1,8                                               
                  IF (Q(I).GT.EPS)IBAD=1                                
 530          CONTINUE                                                  
             IF (IBAD.EQ.0)WRITE(IWRITE,540)                            
 540        FORMAT(12H NO PROBLEMS)                                     
              IF (IBAD.EQ.1)WRITE(IWRITE,550)KASE                       
 550         FORMAT(18H PROBLEM WITH CASE,I5)                           
             IF(IBAD.EQ.1)NOBAD=NOBAD+1                                 
      GOTO  10                                                          
      END                                                               
      SUBROUTINE SETUP(A, IA, M, N, IE, IS, KASE, ISIMP, SIMP,          
     1   ITYPE, B, C)                                                   
      INTEGER IA                                                        
      INTEGER M, N, IE, IS, KASE, ISIMP(1)                              
      INTEGER ITYPE                                                     
      REAL A(IA, 1), SIMP(1), B(1), C(1)                                
      INTEGER KAS, I, J, KA, JJ                                         
      REAL UNI                                                          
      INTEGER TEMP                                                      
C THIS SUBROUTINE SETS UP VARIOUS PROBLEMS                              
      KAS = (KASE+1)/2                                                  
      N = 0                                                             
      IF (KAS.EQ.6)RETURN                                               
      KA = 5                                                            
      IF (2*KAS .NE. KASE) KA = 1                                       
      N = 5*KA                                                          
      M = N/2                                                           
       IF (KAS.EQ.2)M=M/2                                               
C SET UP CONSTRAINT MATRIX                                              
C                                                                       
      DO  20 I = 1, M                                                   
          SUM=0.0                                                       
         DO  10 J = 1, N                                                
            A(I, J) = 2.0*UNI(0) - 1.0                                  
             SUM=SUM+A(I,J)                                             
 10         CONTINUE                                                    
           B(I)=0.5*SUM                                                 
 20      CONTINUE                                                       
C                                                                       
C SET UP COST FUNCTIONAL                                                
C                                                                       
      DO  30 I = 1, N                                                   
         C(I) = -UNI(0)                                                 
 30      CONTINUE                                                       
C                                                                       
C                                                                       
C SET UP SIMPLE CONSTRAINTS                                             
C                                                                       
      JJ = N                                                            
      DO  40 I = 1, JJ                                                  
         SIMP(I) = 0.0                                                  
         ISIMP(I) = I                                                   
         TEMP = I+JJ                                                    
         SIMP(TEMP) =0.6                                                
         TEMP = I+JJ                                                    
         ISIMP(TEMP) = -I                                               
 40      CONTINUE                                                       
      GO TO (50,60,70,80,90),KAS                                        
 50      ITYPE = 1                                                      
         IS = JJ                                                        
         IE = M                                                         
         GO TO 100                                                      
 60      ITYPE = 10                                                     
         IS = 2*JJ                                                      
         IE = M                                                         
         GO TO 100                                                      
 70      ITYPE = 2                                                      
         IS = JJ                                                        
         IE = 0                                                         
         GO TO 100                                                      
 80      ITYPE = 10                                                     
         IS = 2*JJ                                                      
         IE = 0                                                         
         GO TO 100                                                      
 90      ITYPE = 10                                                     
         IS = JJ                                                        
         IE = M/8                                                       
         GOTO  110                                                      
 100     CONTINUE                                                       
 110  RETURN                                                            
      END                                                               
       SUBROUTINE LPT(L,AA,IA,N,I,TVEC,T)                               
       LOGICAL L                                                        
       REAL AA(IA,N),TVEC(N),T,SDOT                                     
       IF (L)GO TO 10                                                   
       CALL SCOPY(N,AA(1,I),1,TVEC,1)                                   
       RETURN                                                           
 10    T=SDOT(N,AA(1,I),1,TVEC,1)                                       
        RETURN                                                          
       END                                                              
C$TEST LRAD                                                             
C***********************************************************************
C                                                                       
C  TEST OF THE PORT LINEAR PROGRAMMING - DLINPR, ETC.                   
C                                                                       
C***********************************************************************
C  MAIN PROGRAM                                                         
      DOUBLE PRECISION DSTAK(8000)                                      
      DOUBLE PRECISION ANULL, FEASIN, SN                                
      COMMON /CSTAK/ DSTAK                                              
      EXTERNAL DLPMAN,  LPT,DLPRNT                                      
      INTEGER IAG, IAS,  NERROR, I, J                                   
      INTEGER K, L, M, N, KASE                                          
      INTEGER IERR, IPTG(150), IA, IE, IL, IS                           
      INTEGER IROLD, ISIMP(200), ITMAX, ITYPE, IWRITE                   
      COMMON /KA/ KASE                                                  
      DOUBLE PRECISION CHANGE,  CTX, SUM, CHAGE2, CHAGE3                
      DOUBLE PRECISION CHAGE4, CHAGE5, A(150, 50), B(150), C(150), X(   
     1   100, 5)                                                        
      DOUBLE PRECISION Y(100), DDOT, CTXD, SIMP(200),  DASUM            
      REAL UNI                                                          
      DOUBLE PRECISION D1MACH,Q(8),EPS                                  
C  SET UP OUTPUT WRITE UNIT                                             
C                                                                       
       IWRITE = I1MACH(2)                                               
      CALL ISTKIN(8000, 4)                                              
      CALL ENTSRC(IROLD, 1)                                             
      KASE = 0                                                          
          EPS=DSQRT(D1MACH(4))                                          
         NOBAD=0                                                        
      IA = 150                                                          
 10   CONTINUE                                                          
      KASE = KASE+1                                                     
      CALL SETUP(A, IA, M, N, IE, IS, KASE, ISIMP, SIMP, ITYPE, B, C)   
          DO 20 I=1,8                                                   
                 Q(I)=0.0D0                                             
 20         CONTINUE                                                    
       IF (N.GT.0) GO TO 40                                             
            WRITE(IWRITE,30)NOBAD                                       
 30          FORMAT(26H NUMBER OF BAD EXAMPLES IS, I5)                  
                STOP                                                    
 40               CONTINUE                                              
      WRITE(IWRITE,60)KASE                                              
      WRITE (IWRITE,  50) M, N, IE, IS, ITYPE                           
 50   FORMAT (3H M=, I10, 2HN=, I10, 13H NO. EQUALITY, I10,             
     1   11H NO. SIMPLE, I10, 6H ITYPE, I10)                            
 60   FORMAT(//6H KASE=,I5)                                             
      DO  70 I = 1, N                                                   
         X(I, 1) = DBLE(UNI(0))                                         
         X(I, 2) = X(I, 1)                                              
 70      CONTINUE                                                       
      ITMAX = 5*N                                                       
         CALL DLINPA(A,M,N,DLPMAN,IA,B,C,X,ITMAX,CTX,IS,SIMP,           
     1      ISIMP, IE, DLPRNT, IAG, IAS, IPTG, X(I, 5))                 
      IF (NERROR(IERR) .EQ. 0) GOTO 90                                  
         WRITE (IWRITE,  80) IERR                                       
 80      FORMAT (4H ERR, I10)                                           
         CALL ERROFF                                                    
         IF (IERR.EQ.8) GO TO 170                                       
         GOTO  10                                                       
 90       CONTINUE                                                      
           FEASIN=0.0D0                                                 
           IAGIE=IAG+IE                                                 
           ANULL=0.0D0                                                  
           DO 100 I=1,M                                                 
              JJ=IPTG(I)                                                
              SUM=DDOT(N,A(JJ,1),IA,X,1)-B(JJ)                          
              FEASIN=DMAX1(FEASIN,-SUM)                                 
              IF (I.LE.IAGIE)ANULL=DMAX1(ANULL,DABS(SUM))               
 100       CONTINUE                                                     
      SN= DASUM(N,X,1)                                                  
      SN=DMAX1(SN,1.D0)                                                 
           FEASIN=FEASIN/SN                                             
            ANULL=ANULL/SN                                              
           Q(1)=FEASIN                                                  
           Q(2)=ANULL                                                   
           WRITE(IWRITE,110)FEASIN,ANULL                                
 110       FORMAT(22H MAX. INFEASABILIBITY=,E15.5,18H NULLITY VIOLATION,
     1     E15.5)                                                       
C SOLVE THE PROBLEM AGAIN                                               
C                                                                       
      DO  120 I = 1, N                                                  
         X(I, 3) = X(I, 1)                                              
 120     CONTINUE                                                       
      CALL DLINPR(A, M, N, IA, B, C, X(1, 3), ITMAX, CTX, IS,           
     1   SIMP, ISIMP, IE)                                               
      IF (NERROR(IERR) .EQ. 0) GOTO 140                                 
         WRITE (IWRITE,  130) IERR                                      
 130     FORMAT (4H ERR, I10)                                           
         CALL ERROFF                                                    
         GOTO  10                                                       
 140  DO  150 I = 1, N                                                  
         Y(I) = X(I, 3)-X(I, 1)                                         
 150     CONTINUE                                                       
      CHANGE = DASUM(N, Y, 1)/SN                                        
            Q(3)=CHANGE                                                 
      WRITE (IWRITE,  160) CHANGE                                       
 160  FORMAT(51H SOLVING SECOND TIME WITH INITIAL GUESS AS SOLUTION/    
     1  9H CHANGE= ,1PD23.15)                                           
C                                                                       
C DIFFERENT INITIAL POINT                                               
C                                                                       
 170      CONTINUE                                                      
      DO  180 I = 1, N                                                  
         X(I, 3) = -X(I, 2)                                             
 180     CONTINUE                                                       
      CALL DLINPR(A, M, N, IA, B, C, X(1, 3), ITMAX, CTX, IS,           
     1   SIMP, ISIMP, IE)                                               
      IF (NERROR(IERR) .EQ. 0) GOTO 200                                 
         WRITE (IWRITE,  190) IERR                                      
 190     FORMAT (4H ERR, I10)                                           
         CALL ERROFF                                                    
         GOTO  10                                                       
 200  DO  210 I = 1, N                                                  
         Y(I) = X(I, 3)-X(I, 1)                                         
 210     CONTINUE                                                       
      CHAGE2=DASUM(N,Y,1)/SN                                            
            Q(4)=CHAGE2                                                 
      WRITE (IWRITE,  220) CHAGE2                                       
 220  FORMAT(44H INITIAL POINT IS NEGATED AND PROBLEM SOLVED/           
     *9H CHANGE= ,1PD23.15)                                             
C                                                                       
C MAKE SURE HAVE INFEASIBLE STARTING POINT                              
C                                                                       
      IF (M .LE. 0) GOTO 330                                            
         SUM = B(1)-DDOT(N, A(1, 1), IA, X(1, 2), 1)                    
         IF (SUM .LE. 0D0) GOTO 240                                     
            WRITE (IWRITE,  230)                                        
 230        FORMAT (29H INITIAL GUESS WAS INFEASIBLE)                   
            GOTO  320                                                   
 240        I = 1                                                       
 250        IF (A(I, 1) .NE. 0.0D0) GOTO  260                           
               I = I+1                                                  
               GOTO  250                                                
 260        DO  270 J = 1, N                                            
               X(J, 3) = X(J, 2)                                        
 270           CONTINUE                                                 
            X(I, 3) = X(I, 3)+(SUM+1.D0)/A(I, 1)                        
            CALL DLINPR(A, M, N, IA, B, C, X(1, 3), ITMAX, CTX,         
     1    IS, SIMP, ISIMP, IE)                                          
            IF (NERROR(IERR) .EQ. 0) GOTO 290                           
               WRITE (IWRITE,  280) IERR                                
 280           FORMAT (4H ERR, I10)                                     
               CALL ERROFF                                              
               GOTO  10                                                 
 290        DO  300 I = 1, N                                            
               Y(I) = X(I, 3)-X(I, 1)                                   
 300           CONTINUE                                                 
            CHAGE3 = DASUM(N, Y, 1)/SN                                  
             Q(5)=CHAGE3                                                
            WRITE (IWRITE,  310) CHAGE3                                 
 310     FORMAT(43H INITIAL POINT MADE INFEASIBLE AND RESOLVED/         
     *10H CHANGE = ,1PD23.15)                                           
 320     CONTINUE                                                       
C                                                                       
C SOLVE WITH SIMPLE CONVERTED INTO GENERAL                              
C                                                                       
 330  IF (IS .EQ. 0) GOTO 430                                           
         L = M                                                          
         DO  370 I = 1, IS                                              
            L = L+1                                                     
            DO  340 J = 1, N                                            
               A(L, J) = 0.0                                            
 340           CONTINUE                                                 
            K = IABS(ISIMP(I))                                          
            A(L, K) = -1.D0                                             
            B(L) = SIMP(I)                                              
            IF (ISIMP(I) .LE. 0) GOTO 350                               
               A(L, K) = 1.D0                                           
               GOTO  360                                                
 350           B(L) = -SIMP(I)                                          
 360        CONTINUE                                                    
 370        CONTINUE                                                    
         DO  380 I = 1, N                                               
            X(I, 3) = X(I, 2)                                           
 380        CONTINUE                                                    
         IL = 0                                                         
         MM=M+IS                                                        
         CALL DLINPR(A,MM, N, IA, B, C, X(1, 3), ITMAX, CTX, IL         
     1 , SIMP, ISIMP, IE)                                               
         IF (NERROR(IERR) .EQ. 0) GOTO 400                              
            WRITE (IWRITE,  390) IERR                                   
 390        FORMAT (4H ERR, I10)                                        
            CALL ERROFF                                                 
            GOTO  10                                                    
 400     DO  410 I = 1, N                                               
            Y(I) = X(I, 3)-X(I, 1)                                      
 410        CONTINUE                                                    
         CHAGE4 = DASUM(N, Y, 1)/SN                                     
             Q(6)=CHAGE4                                                
         WRITE (IWRITE,  420) CHAGE4                                    
 420    FORMAT(38H SIMPLE CONSTRAINTS WRITTEN AS GENERAL/               
     1 9H CHANGE= ,1PD23.15)                                            
C                                                                       
C TEST DUALITY IF APPLICABLE                                            
C                                                                       
 430  IF (ITYPE .NE. 1) GOTO 520                                        
       WRITE(IWRITE,440)                                                
 440   FORMAT(17H TEST FOR DUALITY)                                     
         DO  460 I = 1, M                                               
             DO 450 J=1,N                                               
                A(I,J)=-A(I,J)                                          
 450          CONTINUE                                                  
            X(I, 4) = DBLE(UNI(0))                                      
 460        CONTINUE                                                    
         CALL DLINPA(A,N,M,LPT,IA,C,B,X(1,4),ITMAX,CTXD,0,SIMP,         
     1      ISIMP, 0, DLPRNT, IAG, IAS, IPTG, X(1, 3))                  
         IF (NERROR(IERR) .EQ. 0) GOTO 480                              
            WRITE (IWRITE,  470) IERR                                   
 470        FORMAT (4H ERR, I10)                                        
            CALL ERROFF                                                 
            GOTO  10                                                    
 480     DO  490 I = 1, M                                               
            II=IPTG(I)                                                  
            Y(I) = X(I, 3)+X(II, 1)                                     
 490        CONTINUE                                                    
         CHAGE5 = DASUM(M, Y, 1)/SN                                     
         CTXD=-CTXD                                                     
              Q(7)=DABS(CTX-CTXD)/DABS(CTX)                             
             Q(8)=CHAGE5                                                
         WRITE (IWRITE,  500) CTX, CTXD                                 
 500     FORMAT (4H CTX, 1PD23.15, 4HCTXX, 1PD23.15)                    
         WRITE (IWRITE,  510) CHAGE5                                    
 510     FORMAT (17H CHANGE FROM DUAL, 1PD23.15)                        
C                                                                       
C                                                                       
 520         CONTINUE                                                   
             IBAD=0                                                     
             DO 530 I=1,8                                               
                  IF (Q(I).GT.EPS)IBAD=1                                
 530          CONTINUE                                                  
             IF (IBAD.EQ.0)WRITE(IWRITE,540)                            
 540        FORMAT(12H NO PROBLEMS)                                     
              IF (IBAD.EQ.1)WRITE(IWRITE,550)KASE                       
 550         FORMAT(18H PROBLEM WITH CASE,I5)                           
             IF(IBAD.EQ.1)NOBAD=NOBAD+1                                 
      GOTO  10                                                          
      END                                                               
      SUBROUTINE SETUP(A, IA, M, N, IE, IS, KASE, ISIMP, SIMP,          
     1   ITYPE, B, C)                                                   
      INTEGER IA                                                        
      INTEGER M, N, IE, IS, KASE, ISIMP(1)                              
      INTEGER ITYPE                                                     
      DOUBLE PRECISION A(IA, 1), SIMP(1), B(1), C(1)                    
      INTEGER KAS, I, J, KA, JJ                                         
      REAL UNI                                                          
      INTEGER TEMP                                                      
C THIS SUBROUTINE SETS UP VARIOUS PROBLEMS                              
      KAS = (KASE+1)/2                                                  
      N = 0                                                             
      IF (KAS.EQ.6)RETURN                                               
      KA = 5                                                            
      IF (2*KAS .NE. KASE) KA = 1                                       
      N = 5*KA                                                          
      M = N/2                                                           
       IF (KAS.EQ.2)M=M/2                                               
C SET UP CONSTRAINT MATRIX                                              
C                                                                       
      DO  20 I = 1, M                                                   
          SUM=0.0                                                       
         DO  10 J = 1, N                                                
            A(I, J) = 2.D0*DBLE(UNI(0)) - 1.D0                          
             SUM=SUM+A(I,J)                                             
 10         CONTINUE                                                    
           B(I)=0.5D0*SUM                                               
 20      CONTINUE                                                       
C                                                                       
C SET UP COST FUNCTIONAL                                                
C                                                                       
      DO  30 I = 1, N                                                   
         C(I) = -DBLE(UNI(0))                                           
 30      CONTINUE                                                       
C                                                                       
C                                                                       
C SET UP SIMPLE CONSTRAINTS                                             
C                                                                       
      JJ = N                                                            
      DO  40 I = 1, JJ                                                  
         SIMP(I) = 0.0D0                                                
         ISIMP(I) = I                                                   
         TEMP = I+JJ                                                    
         SIMP(TEMP) =0.6D0                                              
         TEMP = I+JJ                                                    
         ISIMP(TEMP) = -I                                               
 40      CONTINUE                                                       
      GO TO (50,60,70,80,90),KAS                                        
 50      ITYPE = 1                                                      
         IS = JJ                                                        
         IE = M                                                         
         GO TO 100                                                      
 60      ITYPE = 10                                                     
         IS = 2*JJ                                                      
         IE = M                                                         
         GO TO 100                                                      
 70      ITYPE = 2                                                      
         IS = JJ                                                        
         IE = 0                                                         
         GO TO 100                                                      
 80      ITYPE = 10                                                     
         IS = 2*JJ                                                      
         IE = 0                                                         
         GO TO 100                                                      
 90      ITYPE = 10                                                     
         IS = JJ                                                        
         IE = M/8                                                       
         GOTO  110                                                      
 100     CONTINUE                                                       
 110  RETURN                                                            
      END                                                               
       SUBROUTINE LPT(L,AA,IA,N,I,TVEC,T)                               
       LOGICAL L                                                        
       DOUBLE PRECISION AA(IA,N),TVEC(N),T,DDOT                         
       IF (L)GO TO 10                                                   
       CALL DCOPY(N,AA(1,I),1,TVEC,1)                                   
       RETURN                                                           
 10    T=DDOT(N,AA(1,I),1,TVEC,1)                                       
        RETURN                                                          
       END                                                              
C************END OF PORT 3 TEST PROGRAMS********************************
